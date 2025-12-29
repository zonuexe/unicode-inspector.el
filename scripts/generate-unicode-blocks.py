#!/usr/bin/env python3
"""Generate unicode-inspector-blocks.el from Unicode Blocks.txt.

Usage:
  python3 scripts/generate-unicode-blocks.py [path/to/Blocks.txt]

If no path is provided, the script downloads Unicode 17.0 Blocks.txt.
"""

from __future__ import annotations

import re
import sys
import urllib.request
from pathlib import Path

BLOCKS_URL = "https://www.unicode.org/Public/17.0.0/ucd/Blocks.txt"
LICENSE_URL = "https://www.unicode.org/license.txt"
OUT_PATH = Path(__file__).resolve().parents[1] / "unicode-inspector-blocks.el"
TEMPLATE_PATH = Path(__file__).resolve().parent / "unicode-inspector-blocks.el.tpl"

PATTERN = re.compile(r"^([0-9A-F]+)\.\.([0-9A-F]+);\s*(.+)$")
DATE_PATTERN = re.compile(r"^# Date:\s*(\d{4})-")


def read_blocks_text(path: Path | None) -> str:
    if path is None:
        with urllib.request.urlopen(BLOCKS_URL) as response:
            return response.read().decode("utf-8")
    return path.read_text(encoding="utf-8")


def read_license_text() -> str:
    with urllib.request.urlopen(LICENSE_URL) as response:
        return response.read().decode("utf-8")


def validate_license(text: str) -> None:
    first_line = text.splitlines()[0].strip() if text else ""
    if first_line != "UNICODE LICENSE V3":
        raise ValueError(f"Unexpected license header: {first_line!r}")


def format_license(text: str) -> str:
    lines = [f";; {line}".rstrip() for line in text.splitlines()]
    return "\n".join(lines).strip()


def parse_year(text: str) -> str:
    for line in text.splitlines():
        match = DATE_PATTERN.match(line)
        if match:
            return match.group(1)
    return "2025"


def parse_blocks(text: str) -> list[tuple[int, int, str]]:
    rows: list[tuple[int, int, str]] = []
    for line in text.splitlines():
        match = PATTERN.match(line)
        if not match:
            continue
        start = int(match.group(1), 16)
        end = int(match.group(2), 16)
        name = match.group(3).strip()
        rows.append((start, end, name))
    return rows


def render_blocks(rows: list[tuple[int, int, str]]) -> str:
    lines = [f'    (#x{start:04X} #x{end:04X} "{name}")' for start, end, name in rows]
    return "\n".join(lines).strip()


def write_elisp(rows: list[tuple[int, int, str]], year: str, license_text: str) -> None:
    template = TEMPLATE_PATH.read_text(encoding="utf-8")
    rendered = template.replace("${year}", year)
    rendered = rendered.replace("${url}", BLOCKS_URL)
    rendered = rendered.replace("${blocks}", render_blocks(rows))
    rendered = rendered.replace("${license}", format_license(license_text))
    OUT_PATH.write_text(rendered, encoding="utf-8")


def main() -> int:
    source_path = Path(sys.argv[1]) if len(sys.argv) > 1 else None
    text = read_blocks_text(source_path)
    year = parse_year(text)
    license_text = read_license_text()
    try:
        validate_license(license_text)
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        return 1
    rows = parse_blocks(text)
    if not rows:
        print("No block ranges parsed.", file=sys.stderr)
        return 1
    write_elisp(rows, year, license_text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
