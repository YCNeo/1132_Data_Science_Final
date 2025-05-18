#!/usr/bin/env python3
"""analyze.py – profile taipei_rent_clean.csv (ported from analyze.ts, 2025‑05‑18)

Usage
-----
    python analyze.py [csvPath] [outputPath] [order]

    • csvPath   (opt)  default = ./dataset/taipei_rent_clean.csv
    • outputPath(opt)  default = ./analysis/anal_cln.txt
    • order     (opt)  "item" | "amount"   default = "amount"

The script counts value frequencies for every column (except a skip list),
aggregates month‑level stats for date columns, and writes a plain‑text
profile ordered either by frequency (descending) or by item name.
"""
from __future__ import annotations

import csv
import locale
import sys
from collections import defaultdict
from datetime import date
from pathlib import Path
from typing import Dict

# ── CLI ──────────────────────────────────────────────────────────────
args = [a for a in sys.argv[1:] if a.strip()]

BASE_DIR = Path(__file__).resolve().parent.parent

CSV_PATH = next((Path(a) for a in args if a.lower().endswith(".csv")), None)
if CSV_PATH is None:
    CSV_PATH = BASE_DIR / "dataset" / "taipei_rent_clean.csv"

OUT_PATH = next((Path(a) for a in args if a.lower().endswith(".txt")), None)
if OUT_PATH is None:
    OUT_PATH = BASE_DIR / "analysis" / "anal_cln.txt"

ORDER: str = "item" if any(a.lower() == "item" for a in args) else "amount"
assert ORDER in {"item", "amount"}

# ── 常數 ──────────────────────────────────────────────────────────────
SKIP = {"編號", "土地位置建物門牌", "備註"}
MONTH_COLS = {"租賃年月日", "建築完成年月"}

# Attempt to set zh locale for proper sorting; ignore if not present.
try:
    locale.setlocale(locale.LC_COLLATE, "zh_TW.UTF-8")
except locale.Error:
    pass

# ── 讀檔 ──────────────────────────────────────────────────────────────
if not CSV_PATH.exists():
    sys.stderr.write(f"❌  File not found: {CSV_PATH}\n")
    sys.exit(1)

OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
print("🔍  Analyzing …")

with CSV_PATH.open("r", encoding="utf8", newline="") as fh:
    reader = csv.DictReader(fh)
    data = list(reader)
    fieldnames = reader.fieldnames or []

row_count = len(data)
col_count = len(fieldnames)
analysis_date = date.today().isoformat()

# ── 統計 ─────────────────────────────────────────────────────────────
FreqMap = Dict[str, Dict[str, int]]
freq: FreqMap = defaultdict(lambda: defaultdict(int))

for row in data:
    for col, raw in row.items():
        if col in SKIP:
            continue
        val = (raw or "").strip()
        if col in MONTH_COLS and val:
            # keep only YYYY-MM
            if len(val) >= 7 and val[4] == "-":
                val = val[:7]
        freq[col][val] += 1

# ── 產生輸出 ─────────────────────────────────────────────────────────
lines: list[str] = []

# 前置摘要
lines.extend(
    [
        "台北市租屋資料集分析報告",
        "==================================",
        "資料來源  : https://plvr.land.moi.gov.tw/DownloadSeason/",
        f"來源資料集: {CSV_PATH.resolve()}",
        f"資料筆數  : {row_count}",
        f"欄位數    : {col_count}",
        f"分析日期  : {analysis_date}",
        f"排序方式  : Order by {ORDER}",
        "",  # blank line
    ]
)

# 各欄統計
for col, mapping in freq.items():
    lines.append(f"{col}: {len(mapping)}")
    items = list(mapping.items())

    if ORDER == "item":
        # sort by item (locale aware if possible)
        items.sort(key=lambda kv: locale.strxfrm(kv[0]))
    else:
        # sort by amount desc, tie‑break item
        items.sort(key=lambda kv: (-kv[1], locale.strxfrm(kv[0])))

    for v, c in items:
        display = v if v else "<empty>"
        lines.append(f"\t{display}: {c}")
    lines.append("")

# ── 寫檔 & 完成 ─────────────────────────────────────────────────────
OUT_PATH.write_text("\n".join(lines), encoding="utf8")
print(f"✅  Analysis saved to {OUT_PATH.resolve()}")
