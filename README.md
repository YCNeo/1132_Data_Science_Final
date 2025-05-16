# 台北市房價與租金預測分析專案

## 專案概述
本專案旨在分析台北市房價與租金數據，並建立預測模型。專案包含數據收集、地理編碼處理、捷運站特徵提取等功能。主要目標是研究捷運站距離對房價和租金的影響，並建立有效的預測模型。

## 專案結構
```
1132_Data_Science_Final/
├── dataset/                # 數據集目錄
│   ├── mrt.csv            # 捷運站資料
│   ├── rent_trades.csv    # 原始租金交易數據
│   ├── rent_trades_w_mrt.csv  # 租金數據（含捷運站資訊）
│   ├── taipei_other.csv   # 其他房價交易數據
│   ├── other_trades_w_mrt.csv # 其他房價數據（含捷運站資訊）
│   └── taipei_rent_clean.csv  # 清理後的租金數據
├── src/                   # 源代碼目錄
│   ├── main.py           # 主程式
│   ├── geocoding.py      # 地理編碼處理
│   └── mrt_features.py   # 捷運站特徵處理
├── model/                # 模型相關目錄
│   ├── reg.R            # 迴歸模型分析
│   ├── eda/             # 探索性數據分析
│   └── preprocess/      # 數據預處理
└── README.md            # 專案說明文檔
```

## 數據說明
1. **租金數據**：
   - `rent_trades.csv`：原始租金交易數據
   - `rent_trades_w_mrt.csv`：加入捷運站資訊的租金數據
   - `taipei_rent_clean.csv`：清理後的租金數據

2. **其他房價數據**：
   - `taipei_other.csv`：其他類型的房價交易數據
   - `other_trades_w_mrt.csv`：加入捷運站資訊的其他房價數據

3. **捷運站數據**：
   - `mrt.csv`：包含捷運站位置、線路等資訊

## 功能模組

### 1. 數據處理 (src/)
- **地理編碼處理** (`geocoding.py`)
  - 將地址轉換為經緯度座標
  - 使用 ArcGIS 地理編碼服務
  - 支援批次處理

- **捷運站特徵處理** (`mrt_features.py`)
  - 計算到最近捷運站的距離
  - 添加捷運線路資訊
  - 生成捷運站相關特徵

- **主程式** (`main.py`)
  - 整合地理編碼和捷運站特徵處理
  - 提供完整的數據處理流程

### 2. 模型分析 (model/)
- **迴歸分析** (`reg.R`)
  - 建立租金預測模型
  - 分析捷運站距離對租金的影響
  - 模型評估與驗證

- **探索性數據分析** (`model/eda/`)
  - 數據分布分析
  - 特徵相關性分析
  - 視覺化分析結果

- **數據預處理** (`model/preprocess/`)
  - 數據清理與轉換
  - 特徵工程
  - 數據標準化

## 環境需求
- Python 3.6+
  - pandas
  - numpy
  - requests
- R 4.0+
  - ggplot2
  - dplyr
  - tidyr
  - lmtest
  - car

## 安裝依賴
```bash
# Python 依賴
pip install pandas numpy requests

# R 依賴
R -e "install.packages(c('ggplot2', 'dplyr', 'tidyr', 'lmtest', 'car'))"
```

## 使用方式

### 1. 數據處理
```bash
# 執行完整處理流程
python src/main.py

# 或分步執行
python src/geocoding.py
python src/mrt_features.py
```

### 2. 模型分析
```bash
# 執行迴歸分析
Rscript model/reg.R
```

## 模型結果
迴歸模型分析顯示：
1. 捷運站距離對租金有顯著影響
2. 土地面積、總樓層數等特徵對租金有重要影響
3. 不同捷運線路對租金影響程度不同
4. 模型預測誤差在可接受範圍內

## 注意事項
1. 地理編碼服務有請求限制，建議適當設置 `pause_sec` 參數
2. 確保數據集檔案使用 UTF-8 編碼
3. 處理大量數據時請注意記憶體使用
4. 模型分析結果會保存在 model/ 目錄下

## 參考資料
- [內政部不動產交易實價查詢服務網](https://lvr.land.moi.gov.tw/)
- [ArcGIS 地理編碼服務](https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer)
- [台北捷運公司](https://www.metro.taipei/)
