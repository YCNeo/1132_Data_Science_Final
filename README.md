
# 台北市房價預測分析專案

## 專案概述
本專案旨在分析台北市房價數據，並建立預測模型。專案包含數據收集、地理編碼處理、捷運站特徵提取等功能。

## 專案結構
```
1132_Data_Science_Final/
├── dataset/            # 數據集目錄
│   ├── trades.csv     # 原始房價交易數據
│   ├── mrt.csv        # 捷運站資料
│   └── trades_w_mrt.csv  # 處理後的數據
├── src/               # 源代碼目錄
│   ├── main.py        # 主程式
│   ├── geocoding.py   # 地理編碼處理
│   └── mrt_features.py # 捷運站特徵處理
└── README.md          # 專案說明文檔
```

## 快速開始

### 使用主程式（推薦）
使用主程式可以一次執行完整個處理流程：

```bash
python src/main.py
```

主程式會：
1. 讀取 `../dataset/trades.csv` 作為輸入檔案
2. 使用 `../dataset/mrt.csv` 作為捷運站資料
3. 將處理後的結果儲存至 `../dataset/trades_w_mrt.csv`

如需修改輸入輸出檔案路徑，請直接編輯 `src/main.py` 中的以下變數：
```python
input_file = "../dataset/trades.csv"    # 輸入檔案
mrt_file = "../dataset/mrt.csv"         # 捷運站資料
output_file = "../dataset/trades_w_mrt.csv"  # 輸出檔案
```

### 分開執行各模組

#### 1. 地理編碼處理 (geocoding.py)
將地址轉換為經緯度座標，使用 ArcGIS 的地理編碼服務。

主要功能：
- `geocode_address(address)`: 將單一地址轉換為經緯度
- `batch_geocode_csv(path_csv)`: 批次處理 CSV 檔案中的地址

使用方式：
```python
python src/geocoding.py
```

#### 2. 捷運站特徵處理 (mrt_features.py)
計算每個地址到最近捷運站的距離，並添加相關特徵。

主要功能：
- `haversine_array()`: 計算兩點間的球面距離
- `attach_nearest_mrt()`: 添加最近捷運站資訊

特徵包括：
- 最近捷運站名稱
- 到捷運站距離（公尺）
- 捷運線路資訊
- 通車日期

使用方式：
```python
python src/mrt_features.py
```

## 數據處理流程
1. 讀取原始房價交易數據 (trades.csv)
2. 使用地理編碼服務將地址轉換為經緯度
3. 計算每個地址到最近捷運站的距離
4. 添加捷運站相關特徵
5. 輸出處理後的數據集

## 環境需求
- Python 3.6+
- pandas
- numpy
- requests

## 安裝依賴
```bash
pip install pandas numpy requests
```

## 注意事項
1. 地理編碼服務有請求限制，建議適當設置 `pause_sec` 參數
2. 確保數據集檔案使用 UTF-8 編碼
3. 處理大量數據時請注意記憶體使用
4. 如需修改處理參數，請直接編輯 `src/main.py` 中的相關變數

## 參考資料
- [內政部不動產交易實價查詢服務網](https://lvr.land.moi.gov.tw/)
- [ArcGIS 地理編碼服務](https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer)

