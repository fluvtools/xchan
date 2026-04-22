# This script is used only to prepare demo data for the package.
# Script to download the HRDEM 1 m DTM mosaic for the Squamish River demo area.

import os
import json
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path

import geopandas as gpd
import rasterio
from rasterio.io import MemoryFile
from rasterio.merge import merge
from rasterio.windows import Window, from_bounds
from rasterio.warp import transform_bounds
from shapely.geometry import box, shape


# Input AOI (polygon/line defining your POI)
AOI_PATH = "Squamish_river.gpkg"

# CanElevation 1 m mosaic index
S3_BUCKET_BASE = "https://canelevation-dem.s3.amazonaws.com"
S3_PREFIX = "hrdem-mosaic-1m/"

# Outputs
OUTPUT_BBOX = "canada_hrdem_1m_bbox.tif"


def list_s3_keys(prefix: str) -> list[str]:
    keys: list[str] = []
    token: str | None = None

    while True:
        params = {"list-type": "2", "prefix": prefix}
        if token:
            params["continuation-token"] = token
        url = f"{S3_BUCKET_BASE}?{urllib.parse.urlencode(params)}"

        with urllib.request.urlopen(url) as resp:
            root = ET.fromstring(resp.read())

        ns = {"s3": "http://s3.amazonaws.com/doc/2006-03-01/"}
        keys.extend(
            key_el.text
            for key_el in root.findall(".//s3:Contents/s3:Key", ns)
            if key_el.text
        )

        is_truncated = root.findtext(".//s3:IsTruncated", default="false", namespaces=ns)
        if is_truncated.lower() != "true":
            break
        token = root.findtext(".//s3:NextContinuationToken", default=None, namespaces=ns)
        if not token:
            break

    return keys


def choose_intersecting_dtm_urls(aoi_bbox_ll: tuple[float, float, float, float]) -> list[str]:
    keys = list_s3_keys(S3_PREFIX)
    dtm_keys = [k for k in keys if k.endswith("-dtm.tif")]
    extent_keys = [k for k in keys if k.endswith("-extent.geojson")]

    dtm_map = {
        k.replace("-dtm.tif", ""): f"{S3_BUCKET_BASE}/{k}"
        for k in dtm_keys
    }
    extent_map = {
        k.replace("-extent.geojson", ""): f"{S3_BUCKET_BASE}/{k}"
        for k in extent_keys
    }

    aoi_box = box(*aoi_bbox_ll)
    selected_urls: list[str] = []

    for mosaic_id, dtm_url in dtm_map.items():
        extent_url = extent_map.get(mosaic_id)
        if not extent_url:
            continue

        with urllib.request.urlopen(extent_url) as resp:
            extent_geojson = json.load(resp)
        geom = shape(extent_geojson["features"][0]["geometry"])

        if geom.intersects(aoi_box):
            selected_urls.append(dtm_url)

    if not selected_urls:
        raise ValueError(
            "No HRDEM 1 m DTM mosaic extent intersects AOI. "
            "Verify AOI coordinates/CRS and dataset coverage."
        )

    return sorted(selected_urls)


def main() -> None:
    gdf = gpd.read_file(AOI_PATH)
    if gdf.empty:
        raise ValueError(f"No features found in {AOI_PATH}.")

    # Keep only valid geometries and ensure lon/lat CRS before deriving bbox.
    gdf = gdf[gdf.geometry.notnull() & gdf.geometry.is_valid].copy()
    if gdf.empty:
        raise ValueError("All geometries are null/invalid.")
    if gdf.crs is None:
        raise ValueError("AOI layer has no CRS; cannot transform bounds.")

    gdf_ll = gdf.to_crs("EPSG:4326")
    minx, miny, maxx, maxy = gdf_ll.total_bounds
    print(f"AOI bbox (EPSG:4326): {(minx, miny, maxx, maxy)}")
    dem_urls = choose_intersecting_dtm_urls((minx, miny, maxx, maxy))
    print(f"Selected {len(dem_urls)} intersecting DTM mosaic(s)")
    for url in dem_urls:
        print(f" - {url}")

    # Make remote reads friendlier for very large COGs.
    os.environ.setdefault("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
    os.environ.setdefault("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif,.tiff")

    memfiles: list[MemoryFile] = []
    subsets: list[rasterio.io.DatasetReader] = []
    output_crs = None
    output_dtype = None
    output_nodata = None

    try:
        for dem_url in dem_urls:
            with rasterio.open(dem_url) as src:
                dem_bounds = transform_bounds(
                    "EPSG:4326",
                    src.crs,
                    minx,
                    miny,
                    maxx,
                    maxy,
                    densify_pts=21,
                )

                left = max(dem_bounds[0], src.bounds.left)
                bottom = max(dem_bounds[1], src.bounds.bottom)
                right = min(dem_bounds[2], src.bounds.right)
                top = min(dem_bounds[3], src.bounds.top)
                if left >= right or bottom >= top:
                    continue

                window = from_bounds(left, bottom, right, top, transform=src.transform)
                window = window.intersection(Window(0, 0, src.width, src.height))
                window = window.round_offsets().round_lengths()
                if window.width < 1 or window.height < 1:
                    continue

                subset = src.read(1, window=window)
                subset_transform = src.window_transform(window)
                subset_profile = src.profile.copy()
                subset_profile.update(
                    {
                        "driver": "GTiff",
                        "height": subset.shape[0],
                        "width": subset.shape[1],
                        "count": 1,
                        "transform": subset_transform,
                    }
                )

                memfile = MemoryFile()
                tmp = memfile.open(**subset_profile)
                tmp.write(subset, 1)
                tmp.close()

                memfiles.append(memfile)
                subsets.append(memfile.open())

                output_crs = src.crs
                output_dtype = src.dtypes[0]
                output_nodata = src.nodata

        if not subsets:
            raise ValueError("No readable AOI subset found in intersecting DTM mosaic(s).")

        mosaic, mosaic_transform = merge(subsets)
        if mosaic.shape[1] == 0 or mosaic.shape[2] == 0:
            raise ValueError("Merged AOI subset is empty.")

        bbox_profile = {
            "driver": "GTiff",
            "height": mosaic.shape[1],
            "width": mosaic.shape[2],
            "count": 1,
            "dtype": output_dtype,
            "crs": output_crs,
            "transform": mosaic_transform,
            "nodata": output_nodata,
        }

        with rasterio.open(OUTPUT_BBOX, "w", **bbox_profile) as dst:
            dst.write(mosaic[0], 1)
        print(f"Wrote bbox subset: {Path(OUTPUT_BBOX).resolve()}")

    finally:
        for ds in subsets:
            ds.close()
        for mem in memfiles:
            mem.close()


if __name__ == "__main__":
    main()