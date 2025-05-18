#!/usr/bin/env python3
"""Taipei rent dataset cleaner (ported from clean.ts, 2025‑05‑18)

Usage:
    python clean.py [SRC] [DEST]
    Default SRC : dataset/taipei_rent.csv
    Default DEST: dataset/taipei_rent_clean.csv

Cleans rows with residential‑specific criteria, standardises dates/fields,
and expands attached‑equipment columns.

The logic mirrors the original TypeScript implementation one‑for‑one.
"""

from __future__ import annotations

import csv
import math
import re
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Tuple, Union

# --------------------------------------------------------------------------- #
# CLI paths
BASE_DIR = Path(__file__).resolve().parent.parent
SRC: Path = (
    Path(sys.argv[1]) if len(sys.argv) > 1 else BASE_DIR / "dataset" / "taipei_rent.csv"
)
DEST: Path = (
    Path(sys.argv[2])
    if len(sys.argv) > 2
    else BASE_DIR / "dataset" / "taipei_rent_clean.csv"
)

# --------------------------------------------------------------------------- #
# Constants
DROP_COLS = {
    "都市土地使用分區",
    "非都市土地使用分區",
    "非都市土地使用編定",
    "備註",
}
PURPOSE_RE = re.compile(
    r"住家用|住宅|集合住宅|多戶住宅|國民住宅|公寓|雙併住宅|農舍|住商用|住工用|宿舍|寄宿|住宿單元"
)
EQUIP_SEP = re.compile(r"[、,，]")
MAX_LAYOUT = 100
DAY_MS = 86_400_000  # kept for 1‑to‑1 parity with TS

HEADER_ORDER: List[str] = [
    "編號",
    "交易標的",
    "鄉鎮市區",
    "土地位置建物門牌",
    "租賃年月日",
    "總額元",
    "出租型態",
    "租賃天數",
    "主要用途",
    "租賃層次",
    "總樓層數",
    "建物型態",
    "交易筆棟數-土地",
    "交易筆棟數-建物",
    "交易筆棟數-車位",
    "租賃住宅服務",
    "現況格局",
    "有無管理組織",
    "有無管理員",
    "有無附傢俱",
    "有無電梯",
    "主要建材",
    "建物現況格局-房",
    "建物現況格局-廳",
    "建物現況格局-衛",
    "建物現況格局-隔間",
    "土地面積平方公尺",
    "建物總面積平方公尺",
    "單價元平方公尺",
    "車位類別",
    "車位面積平方公尺",
    "車位總額元",
    "建築完成年月",
    "附屬設備-冷氣",
    "附屬設備-熱水器",
    "附屬設備-洗衣機",
    "附屬設備-電視機",
    "附屬設備-冰箱",
    "附屬設備-瓦斯或天然氣",
    "附屬設備-有線電視",
    "附屬設備-網路",
    "source_file",
]

DIGIT_MAP = {
    "零": 0,
    "一": 1,
    "二": 2,
    "兩": 2,
    "三": 3,
    "四": 4,
    "五": 5,
    "六": 6,
    "七": 7,
    "八": 8,
    "九": 9,
}


# --------------------------------------------------------------------------- #
# Helpers
def parse_roc(raw: Union[str, int, float, None]) -> Union[datetime, None]:
    """Convert a ROC date string (民國年) to a datetime; return None if invalid."""
    if raw is None:
        return None
    s = str(raw).strip()
    if not s or s.lower() == "nan":
        return None
    if s.endswith(".0"):
        s = s[:-2]

    # pure digits 6/7 chars: yyyMMdd
    if re.fullmatch(r"\d{6,7}", s):
        if len(s) == 6:
            s = "0" + s  # pad
        yyy, mm, dd = int(s[:3]), int(s[3:5]), int(s[5:7])
        try:
            return datetime(yyy + 1911, mm, dd)
        except ValueError:
            return None

    # any delimiter
    m = re.search(r"(\d{1,3})[./年-](\d{1,2})[./月-](\d{1,2})", s)
    if m:
        yyy, mm, dd = int(m.group(1)), int(m.group(2)), int(m.group(3))
        try:
            return datetime(yyy + 1911, mm, dd)
        except ValueError:
            return None

    return None


def roc_to_iso(raw: Union[str, int, float, None]) -> Union[str, None]:
    dt = parse_roc(raw)
    return dt.strftime("%Y-%m-%d") if dt else None


def period_to_days(s: str) -> Union[int, None]:
    if "~" not in s:
        return None
    a, b = s.split("~", 1)
    da, db = parse_roc(a), parse_roc(b)
    return (db - da).days if da and db else None


def to_num(v: Union[str, int, float, None]) -> Union[float, None]:
    if isinstance(v, (int, float)):
        return float(v) if math.isfinite(v) else None
    if isinstance(v, str):
        v_clean = v.replace(",", "").strip()
        try:
            n = float(v_clean)
            return n if math.isfinite(n) else None
        except ValueError:
            return None
    return None


def parse_trans_ratio(raw: Union[str, None]) -> Tuple[float, float, float]:
    land = building = parking = 0.0
    if not raw:
        return land, building, parking

    pair_re = re.compile(
        r"土地\s*[:：]?\s*(\d+(?:\.\d+)?)|建物\s*[:：]?\s*(\d+(?:\.\d+)?)|車位\s*[:：]?\s*(\d+(?:\.\d+)?)"
    )
    for m in pair_re.finditer(raw):
        if m.group(1):
            land = float(m.group(1))
        if m.group(2):
            building = float(m.group(2))
        if m.group(3):
            parking = float(m.group(3))

    # pure numbers fallback e.g. "1 1 0"
    if land == building == parking == 0.0:
        nums = re.findall(r"\d+(?:\.\d+)?", raw)
        if len(nums) >= 3:
            land, building, parking = map(float, nums[:3])

    return land, building, parking


def floor_to_number(chinese: str) -> Union[int, None]:
    if not chinese:
        return None
    s = chinese.strip()
    if s.endswith("層"):
        s = s[:-1]
    negative = s.startswith("地下")
    if negative:
        s = s[2:]

    if not s:
        return 0

    result = 0
    if len(s) == 1:
        result = DIGIT_MAP.get(s, 0)
    else:
        if "十" in s:
            ten_idx = s.index("十")
            tens = 1 if ten_idx == 0 else DIGIT_MAP.get(s[ten_idx - 1], 1)
            result += tens * 10
            ones_char = s[ten_idx + 1 : ten_idx + 2]
            result += DIGIT_MAP.get(ones_char, 0)
        else:
            result = DIGIT_MAP.get(s, 0)

    return -result if negative else result


# --------------------------------------------------------------------------- #
# Load CSV
if not SRC.exists():
    print(f"❌  File not found: {SRC}")
    sys.exit(1)
print("🔍  Cleaning …")

rows: List[Dict[str, str]] = []
with SRC.open("r", encoding="utf8", newline="") as fh:
    reader = csv.DictReader(fh)
    for r in reader:
        rows.append({k.strip(): v.strip() for k, v in r.items()})

# --------------------------------------------------------------------------- #
# Collect equipment list
equip_set = set()
for r in rows:
    for eq in EQUIP_SEP.split(r.get("附屬設備", "")):
        eq = eq.strip()
        if eq:
            equip_set.add(eq)
EQUIP_COLS = sorted(equip_set)

# --------------------------------------------------------------------------- #
# Cleaning
removed: Dict[str, int] = {
    "用途不符": 0,
    "租賃筆棟數不符": 0,
    "建築完成年月缺失": 0,
    "租賃年月日缺失": 0,
    "租賃層次不明": 0,
    "房過大": 0,
    "廳過大": 0,
    "衛過大": 0,
    "總額缺失": 0,
}

cleaned: List[Dict[str, Union[str, int, float]]] = []

for row in rows:
    # delete rules
    if not PURPOSE_RE.search(row.get("主要用途", "")):
        removed["用途不符"] += 1
        continue

    land, building, parking = parse_trans_ratio(row.get("租賃筆棟數"))
    if land == 0 and building == 0:
        removed["租賃筆棟數不符"] += 1
        continue

    built_iso = roc_to_iso(row.get("建築完成年月"))
    if built_iso is None:
        removed["建築完成年月缺失"] += 1
        continue

    lease_iso = roc_to_iso(row.get("租賃年月日"))
    if lease_iso is None:
        removed["租賃年月日缺失"] += 1
        continue

    if row.get("租賃層次") == "見其他登記事項":
        removed["租賃層次不明"] += 1
        continue

    rooms = int(row.get("建物現況格局-房", "0") or 0)
    if rooms > MAX_LAYOUT:
        removed["房過大"] += 1
        continue

    halls = int(row.get("建物現況格局-廳", "0") or 0)
    if halls > MAX_LAYOUT:
        removed["廳過大"] += 1
        continue

    baths = int(row.get("建物現況格局-衛", "0") or 0)
    if baths > MAX_LAYOUT:
        removed["衛過大"] += 1
        continue

    total_amt = to_num(row.get("總額元"))
    if total_amt is None:
        removed["總額缺失"] += 1
        continue

    # --- transform ---
    out: Dict[str, Union[str, int, float]] = {}

    out["編號"] = row.get("編號", "")
    out["租賃年月日"] = lease_iso
    out["建築完成年月"] = built_iso

    rental_level = row.get("租賃層次", "")
    out["租賃層次"] = "NA" if rental_level == "全" else floor_to_number(rental_level)

    out["交易筆棟數-土地"], out["交易筆棟數-建物"], out["交易筆棟數-車位"] = (
        land,
        building,
        parking,
    )

    out["租賃天數"] = period_to_days(row.get("租賃期間", "")) or "NA"

    out["主要用途"] = row.get("主要用途", "")
    out["主要建材"] = row.get("主要建材", "").strip() or "NA"

    out["建物現況格局-房"] = rooms
    out["建物現況格局-廳"] = halls
    out["建物現況格局-衛"] = baths

    for k in [
        "建物現況格局-隔間",
        "有無管理組織",
        "有無附傢俱",
        "有無電梯",
        "有無管理員",
    ]:
        v = row.get(k, "").strip()
        out[k] = 1 if v == "有" else 0 if v == "無" else "NA"

    out["總額元"] = total_amt

    # copy other columns
    for col, v in row.items():
        if (
            col
            in {
                "附屬設備",
                "租賃期間",
                "租賃筆棟數",
                "建築完成年月",
                "編號",
                "租賃年月日",
                "備註",
            }
            or col in DROP_COLS
        ):
            continue
        if col not in out:
            out[col] = v

    # equipment split
    own_equip = {
        eq.strip() for eq in EQUIP_SEP.split(row.get("附屬設備", "")) if eq.strip()
    }
    for eq in EQUIP_COLS:
        out[f"附屬設備-{eq}"] = 1 if eq in own_equip else 0

    out["source_file"] = row.get("source_file", "")
    cleaned.append(out)

# --------------------------------------------------------------------------- #
if not cleaned:
    print("⚠️  No rows left after cleaning; nothing written.")
    sys.exit(0)

extra_cols = [c for c in cleaned[0].keys() if c not in HEADER_ORDER]
ordered_cols = HEADER_ORDER + extra_cols

DEST.parent.mkdir(parents=True, exist_ok=True)
with DEST.open("w", encoding="utf8", newline="") as fh:
    writer = csv.writer(fh)
    writer.writerow(ordered_cols)
    for r in cleaned:
        writer.writerow([r.get(c, "") for c in ordered_cols])

# --------------------------------------------------------------------------- #
total_removed = sum(removed.values())
print("🧹  Cleaning summary")
for k, v in removed.items():
    print(f"  • {k}: {v} rows")
print(f"Total removed: {total_removed}")
print(f"Remaining rows: {len(cleaned)}")
print(f"✅  Clean file saved to {DEST}")
