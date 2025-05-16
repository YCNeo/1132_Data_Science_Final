import pandas as pd
import numpy as np

def haversine_array(lon1, lat1, lon2, lat2):
    """
    計算兩組經緯度陣列/數值之間的 Haversine 距離（公尺）
    lon1, lat1: 單一點的 lon, lat (float)
    lon2, lat2: 多點陣列的 lon, lat (1D numpy array)
    """
    # 轉成弧度
    lon1_rad, lat1_rad = np.radians(lon1), np.radians(lat1)
    lon2_rad, lat2_rad = np.radians(lon2), np.radians(lat2)
    dlon = lon2_rad - lon1_rad
    dlat = lat2_rad - lat1_rad

    a = np.sin(dlat / 2) ** 2 + \
        np.cos(lat1_rad) * np.cos(lat2_rad) * np.sin(dlon / 2) ** 2
    c = 2 * np.arcsin(np.sqrt(a))
    R = 6_371_000  # 地球半徑 (公尺)
    return R * c

def attach_nearest_mrt(
    addr_csv_path,
    mrt_csv_path,
    addr_lon_col="x",
    addr_lat_col="y",
    station_name_col="站名",
    station_lon_col="經度",
    station_lat_col="緯度",
    line_cols=None,
    open_date_col="通車日期",
    output_csv_path=None
):
    # 1. 讀取資料
    df_addr = pd.read_csv(addr_csv_path, encoding="utf-8-sig")
    df_mrt = pd.read_csv(mrt_csv_path, encoding="utf-8-sig")

    if line_cols is None:
        line_cols = [
            "文湖線", "淡水信義線", "新北投支線", "松山新店線",
            "小碧潭支線", "中和新蘆線", "板南線", "環狀線"
        ]

    # 2. 取出站點層級資料（若原檔有多個出入口，這裡以每個站名第一筆為代表）
    cols_needed = [station_name_col, station_lon_col, station_lat_col] + line_cols + [open_date_col]
    df_stations = df_mrt[cols_needed] \
        .drop_duplicates(subset=[station_name_col], keep="first") \
        .rename(columns={
            station_name_col: "station_name",
            station_lon_col: "station_lon",
            station_lat_col: "station_lat"
        }) \
        .reset_index(drop=True)

    # 3. 對每筆地址找出最近站點
    nearest_col = "最近捷運站"
    distance_col = "捷運站距離(公尺)"

    def find_nearest(row):
        lon, lat = row.get(addr_lon_col), row.get(addr_lat_col)
        # 若缺經緯度，直接回傳空值
        if pd.isna(lon) or pd.isna(lat):
            return pd.Series(
                {nearest_col: None, distance_col: np.nan, **{c: None for c in line_cols + [open_date_col]}}
            )

        # 計算到所有站的距離
        dists = haversine_array(
            lon, lat,
            df_stations["station_lon"].values,
            df_stations["station_lat"].values
        )
        idx_min = dists.argmin()
        station = df_stations.iloc[idx_min]

        data = {
            nearest_col: station["station_name"],
            distance_col: dists[idx_min]
        }
        # 加上各條線別與通車日期
        for c in line_cols + [open_date_col]:
            data[c] = station[c]
        return pd.Series(data)

    df_extra = df_addr.apply(find_nearest, axis=1)

    # 4. 合併並輸出
    df_out = pd.concat([df_addr, df_extra], axis=1)
    if output_csv_path is None:
        output_csv_path = addr_csv_path.replace(".csv", "_with_mrt.csv")
    df_out.to_csv(output_csv_path, index=False, encoding="utf-8-sig")
    print(f"已輸出：{output_csv_path}")

if __name__ == "__main__":
    attach_nearest_mrt(
        addr_csv_path="../dataset/trades.csv",  # 你的地址檔
        mrt_csv_path="../dataset/mrt.csv",
        addr_lon_col="x",
        addr_lat_col="y",
        station_name_col="站名",         
        station_lon_col="經度",
        station_lat_col="緯度",
        line_cols=[
            "文湖線", "淡水信義線", "新北投支線", "松山新店線",
            "小碧潭支線", "中和新蘆線", "板南線", "環狀線"
        ],
        open_date_col="通車日期",
        output_csv_path="../dataset/wenshan_w_mrt.csv"
    ) 