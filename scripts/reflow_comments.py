#!/usr/bin/env python3
"""Reflow roxygen prose and R comments to a target line width.

`@examples` blocks are left unchanged: they contain executable R code where
line breaks are semantic, and Air does not format roxygen anyway.
"""

from __future__ import annotations

import argparse
import re
import textwrap
from pathlib import Path

ROXYGEN_RE = re.compile(r"^(#'\s*)(.*)$")
R_CHUNK_START = re.compile(r"^```\{r")
R_CHUNK_END = re.compile(r"^```\s*$")
URL_RE = re.compile(r"https?://\S+")
HTML_TAG_RE = re.compile(r"<[^>]+>")


def wrap_text(text: str, width: int) -> list[str]:
    text = " ".join(text.split())
    if not text:
        return [""]
    return textwrap.wrap(
        text,
        width=width,
        break_long_words=False,
        break_on_hyphens=False,
    )


def content_width(prefix: str, max_width: int) -> int:
    return max(1, max_width - len(prefix))


def reflow_with_prefixes(first_prefix: str, cont_prefix: str, text: str, max_width: int) -> list[str]:
    wrapped = wrap_text(text, content_width(first_prefix, max_width))
    if not wrapped:
        return [first_prefix.rstrip()]
    lines = [first_prefix + wrapped[0]]
    lines.extend(cont_prefix + part for part in wrapped[1:])
    return lines


def parse_roxygen_block(lines: list[str]) -> list[dict]:
    items: list[dict] = []
    current: dict | None = None

    for line in lines:
        match = ROXYGEN_RE.match(line)
        if not match:
            continue
        prefix, content = match.group(1), match.group(2)
        if content.strip() == "":
            if current is not None:
                items.append(current)
                current = None
            items.append({"kind": "blank", "line": line})
            continue

        stripped = content.lstrip()
        indent = len(content) - len(stripped)

        if stripped.startswith("@"):
            if current is not None:
                items.append(current)
            current = {"kind": "tag", "prefix": prefix, "lines": [line]}
        elif current is not None and current["kind"] == "tag" and indent >= 2:
            current["lines"].append(line)
        elif current is not None and current["kind"] == "paragraph":
            current["lines"].append(line)
        else:
            if current is not None:
                items.append(current)
            current = {"kind": "paragraph", "prefix": prefix, "lines": [line]}

    if current is not None:
        items.append(current)
    return items


def tag_header_and_body(first_content: str) -> tuple[str, str]:
    stripped = first_content.lstrip()
    if stripped.startswith("@param "):
        match = re.match(r"^(@param\s+\S+\s+)(.*)$", stripped)
        if match:
            return match.group(1), match.group(2)
    if stripped.startswith("@return"):
        match = re.match(r"^(@returns?\s+)(.*)$", stripped)
        if match:
            return match.group(1), match.group(2)
    if stripped.startswith("@section "):
        match = re.match(r"^(@section\s+[^:]+\:\s*)(.*)$", stripped)
        if match:
            return match.group(1), match.group(2)
    if stripped.startswith("@describeIn "):
        match = re.match(r"^(@describeIn\s+\S+\s+)(.*)$", stripped)
        if match:
            return match.group(1), match.group(2)
    return "", stripped


def reflow_tag_item(item: dict, max_width: int) -> list[str]:
    first_line = item["lines"][0]
    first_prefix, first_content = ROXYGEN_RE.match(first_line).groups()
    if first_content.lstrip().startswith("@examples"):
        return item["lines"]

    cont_prefix = first_prefix + "  "
    header, body_start = tag_header_and_body(first_content)
    body_parts = [body_start.strip()] if body_start.strip() else []
    for line in item["lines"][1:]:
        body_parts.append(ROXYGEN_RE.match(line).group(2).strip())
    body = " ".join(part for part in body_parts if part)
    if header:
        header_prefix = first_prefix + first_content[: len(first_content) - len(first_content.lstrip())] + header
        # header_prefix should be first_prefix + header, but header is from stripped content
        header_prefix = first_prefix + header
        wrapped = wrap_text(body, content_width(header_prefix, max_width))
        if not wrapped:
            return [first_prefix + header.rstrip()]
        out = [header_prefix + wrapped[0]]
        out.extend(cont_prefix + part for part in wrapped[1:])
        return out
    return reflow_with_prefixes(first_prefix, first_prefix, body, max_width)


def reflow_paragraph_item(item: dict, max_width: int) -> list[str]:
    first_prefix = ROXYGEN_RE.match(item["lines"][0]).group(1)
    body = " ".join(ROXYGEN_RE.match(line).group(2).strip() for line in item["lines"])
    return reflow_with_prefixes(first_prefix, first_prefix, body, max_width)


def reflow_roxygen_block(lines: list[str], max_width: int) -> list[str]:
    items = parse_roxygen_block(lines)
    out: list[str] = []

    for item in items:
        if item["kind"] == "blank":
            out.append(item["line"])
            continue

        if item["kind"] == "tag":
            out.extend(reflow_tag_item(item, max_width))
        else:
            out.extend(reflow_paragraph_item(item, max_width))

    return out


def reflow_regular_comment(line: str, max_width: int) -> list[str]:
    stripped = line.lstrip()
    if not stripped.startswith("#") or stripped.startswith("#'"):
        return [line]
    indent = line[: len(line) - len(stripped)]
    body = stripped[1:]
    if body.startswith(" "):
        comment_prefix = "# "
        text = body[1:]
    else:
        comment_prefix = "#"
        text = body
    wrapped = wrap_text(text.strip(), max_width - len(indent) - len(comment_prefix))
    if not wrapped:
        return [line]
    result = [indent + comment_prefix + wrapped[0]]
    for part in wrapped[1:]:
        result.append(indent + comment_prefix + " " + part)
    return result


def reflow_r_file(path: Path, code_width: int) -> None:
    lines = path.read_text().splitlines()
    new_lines: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if ROXYGEN_RE.match(line):
            block = []
            while i < len(lines) and ROXYGEN_RE.match(lines[i]):
                block.append(lines[i])
                i += 1
            new_lines.extend(reflow_roxygen_block(block, code_width))
            continue
        if line.lstrip().startswith("#") and not line.lstrip().startswith("#'"):
            if len(line) > code_width:
                new_lines.extend(reflow_regular_comment(line, code_width))
            else:
                new_lines.append(line)
            i += 1
            continue
        new_lines.append(line)
        i += 1
    path.write_text("\n".join(new_lines) + "\n")


def should_skip_markdown_line(line: str) -> bool:
    stripped = line.strip()
    if not stripped:
        return True
    if stripped.startswith("---"):
        return True
    if stripped.startswith("<!--"):
        return True
    if stripped.startswith("```"):
        return True
    if stripped.startswith("#"):
        return True
    if stripped.startswith("|"):
        return True
    if stripped.startswith("[!["):
        return True
    if HTML_TAG_RE.search(line):
        return True
    if URL_RE.search(line) and len(line) <= 120:
        return True
    return False


def reflow_markdown_prose(line: str, max_width: int) -> list[str]:
    if should_skip_markdown_line(line):
        return [line]
    if len(line) <= max_width:
        return [line]
    indent = line[: len(line) - len(line.lstrip())]
    wrapped = wrap_text(line.strip(), max_width - len(indent))
    return [indent + part for part in wrapped]


def reflow_rmd_file(path: Path, prose_width: int) -> None:
    lines = path.read_text().splitlines()
    new_lines: list[str] = []
    in_r_chunk = False
    for line in lines:
        if R_CHUNK_START.match(line.strip()):
            in_r_chunk = True
            new_lines.append(line)
            continue
        if in_r_chunk and R_CHUNK_END.match(line.strip()):
            in_r_chunk = False
            new_lines.append(line)
            continue
        if in_r_chunk:
            if line.strip() == "":
                new_lines.append(line)
            elif len(line) > prose_width and line.lstrip().startswith("#"):
                new_lines.extend(reflow_regular_comment(line, prose_width))
            elif len(line) > prose_width:
                new_lines.extend(wrap_text(line, prose_width))
            else:
                new_lines.append(line)
        else:
            new_lines.extend(reflow_markdown_prose(line, prose_width))
    path.write_text("\n".join(new_lines) + "\n")


def iter_r_files(root: Path) -> list[Path]:
    skip = {".git", "renv", "revdep", "excluded"}
    files: list[Path] = []
    for path in root.rglob("*.R"):
        if any(part in skip for part in path.parts):
            continue
        files.append(path)
    return sorted(files)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path("."))
    args = parser.parse_args()
    root = args.root

    for path in iter_r_files(root):
        reflow_r_file(path, code_width=80)

    for rel in ["README.Rmd", *sorted((root / "vignettes").glob("*.Rmd"))]:
        path = root / rel if isinstance(rel, str) else rel
        if path.exists():
            reflow_rmd_file(path, prose_width=72)


if __name__ == "__main__":
    main()
