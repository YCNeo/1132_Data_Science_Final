import pandas as pd
import requests
import time

def geocode_address(address, max_locations=3):
    url = "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates"
    params = {
        "f": "json",
        "SingleLine": address,
        "outFields": "*",
        "maxLocations": max_locations
    }
    try:
        resp = requests.get(url, params=params, timeout=10)
        resp.raise_for_status()
        cands = resp.json().get("candidates", [])
        if not cands:
            return None, None
        top = max(cands, key=lambda c: c.get("score", 0))
        loc = top["location"]
        return loc.get("x"), loc.get("y")
    except Exception as e:
        print(f"Geocode 失敗 [{address}]: {e}")
        return None, None

def batch_geocode_csv(path_csv, address_col="土地位置建物門牌", pause_sec=0.2, batch_size=10):
    # 1. 讀取 CSV
    df = pd.read_csv(
        path_csv,
        header=0,
        encoding='utf-8-sig',
        engine='python',
        on_bad_lines='warn'    # pandas 1.3+ 改用 on_bad_lines
    )

    print("pandas 讀入資料筆數：", len(df))
    # 2. 新增 x, y 欄位
    df["x"] = None
    df["y"] = None

    total = len(df)
    processed = 0

    # 3. 逐列做地理編碼
    for i, (idx, addr) in enumerate(df[address_col].fillna("").items()):
        if not addr.strip():
            # 空地址也算一筆「已處理」
            processed += 1
            continue

        x, y = geocode_address(addr)
        df.at[idx, "x"] = x
        df.at[idx, "y"] = y
        processed += 1

        # 每 batch_size 筆印一次進度
        if processed % batch_size == 0 or processed == total:
            print(f"已處理 {processed}/{total} 筆地址")

        time.sleep(pause_sec)

    # 4. 覆寫原 CSV
    df.to_csv(path_csv, index=False, encoding="utf-8-sig")
    print(f"全部完成，共 {total} 筆，已更新並覆寫：{path_csv}")

if __name__ == "__main__":
    csv_path = "../dataset/trades.csv"
    batch_geocode_csv(csv_path) 