#!/usr/bin/env python3
"""Taipei historical “rent” crawler
Downloads every quarterly ZIP (101S1 → 114S1),
keeps only Taipei files  a_*_c.csv   (land‑rent detail table),
filters rows:
    1) 交易標的 ∉ {土地, 車位, 建物}
    2) 土地使用分區含「住」 or 兩欄皆空
    3) 有租賃年月日 or 租賃期間
dedupes on  (編號‑交易年月日‑門牌)
exports ONE aligned CSV:  dataset/taipei_rent.csv
Ported from crawl.ts  (v2025‑05‑18)
"""

from __future__ import annotations

import csv
import io
import os
import re
import sys
import tempfile
import zipfile
from pathlib import Path
from typing import Dict, List

import requests

CITY_CODE = "a"  # Taipei City
EXCLUDE_SET = {"土地", "車位", "建物"}
ZONING_RESIDENTIAL = re.compile("住")  # human‑living zoning

TMP_DIR = Path("tmp")
DATASET_DIR = Path("dataset")

TMP_DIR.mkdir(parents=True, exist_ok=True)
DATASET_DIR.mkdir(parents=True, exist_ok=True)


# ---------------------------------------------------------------------------#
def generate_seasons() -> List[str]:
    """Return list of season strings like '101S1', ..., '114S1'."""
    CURRENT_ROC_YEAR = 114  # 2025‑05‑18 = ROC 114
    CURRENT_SEASON = 1  # last finished quarter
    seasons: List[str] = []
    for y in range(101, CURRENT_ROC_YEAR + 1):
        last_q = CURRENT_SEASON if y == CURRENT_ROC_YEAR else 4
        for s in range(1, last_q + 1):
            seasons.append(f"{y}S{s}")
    return seasons


def download_zip(url: str, outfile: Path) -> None:
    print("⇣ downloading zip …", outfile.name)
    r = requests.get(url, timeout=30, stream=True)
    r.raise_for_status()
    outfile.write_bytes(r.content)


def extract_zip(zip_path: Path, out_dir: Path) -> None:
    with zipfile.ZipFile(zip_path) as z:
        z.extractall(out_dir)


# ---------------------------------------------------------------------------#
Row = Dict[str, str]


def _read_csv(path_: Path) -> List[Row]:
    """Return list of dict rows.  Keeps header names as in the CSV (trimmed)."""
    with path_.open("r", encoding="utf8", newline="") as fh:
        # read entire file and strip leading BOM
        raw = fh.read().lstrip("\ufeff")
    # sniff dialect roughly; they are simple comma‑separated
    rdr = csv.reader(io.StringIO(raw))
    rows = list(rdr)
    if not rows:
        return []

    # The first row is Chinese header, second row (index 1) is English header.
    header = [h.strip() for h in rows[0]]
    data_rows = rows[2:] if len(rows) >= 2 else []
    return [dict(zip(header, (c.strip() for c in row))) for row in data_rows]


def filter_rows(csv_path: Path, source: str) -> List[Row]:
    records = _read_csv(csv_path)
    if not records:
        return []

    out: List[Row] = []
    for r in records:
        # 1) 交易標的
        if r.get("交易標的", "").strip() in EXCLUDE_SET:
            continue

        # 2) 土地使用分區 (都市 / 非都市)
        zu = r.get("都市土地使用分區", "")
        zr = r.get("非都市土地使用分區", "")
        if not (ZONING_RESIDENTIAL.search(zu + zr) or (zu == zr == "")):
            continue

        # 3) 租賃欄位
        has_rent = bool(r.get("租賃年月日")) or bool(r.get("租賃期間"))
        if not has_rent:
            continue

        new_r = dict(r)
        new_r["source_file"] = source
        out.append(new_r)

    return out


# ---------------------------------------------------------------------------#
def main() -> None:
    all_rows: List[Row] = []

    for season in generate_seasons():
        url = f"https://plvr.land.moi.gov.tw/DownloadSeason?fileName=lvr_landcsv.zip&season={season}&type=zip"
        zip_path = TMP_DIR / f"lvr_{season}.zip"
        season_dir = TMP_DIR / season
        season_dir.mkdir(parents=True, exist_ok=True)

        download_zip(url, zip_path)
        extract_zip(zip_path, season_dir)

        # a_*_c.csv  (land‑rent detail)
        for csv_file in season_dir.glob(f"{CITY_CODE}_*_c.csv"):
            print(f"⇣ parsing {csv_file.name} ({season}) …")
            rows = filter_rows(csv_file, f"{season}/{csv_file.name}")
            all_rows.extend(rows)

    # Deduplicate on 編號‑交易年月日‑門牌
    uniq: Dict[str, Row] = {}
    for r in all_rows:
        key = f"{r.get('編號','')}-{r.get('交易年月日','')}-{r.get('土地位置建物門牌') or r.get('土地區段位置或建物區門牌')}"
        uniq[key] = r

    uniq_rows = list(uniq.values())
    print(f"✓ {len(uniq_rows)} unique Taipei rent rows")

    if not uniq_rows:
        print("⚠️  No rent rows after filtering – nothing written.")
        return

    # Align header (union of all keys)
    header = sorted({k for r in uniq_rows for k in r.keys()})

    # Write aligned CSV
    out_path = DATASET_DIR / "taipei_rent.csv"
    with out_path.open("w", encoding="utf8", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(header)
        for r in uniq_rows:
            w.writerow([r.get(k, "") for k in header])

    print("\nArtifacts ready:\n  •", out_path)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
