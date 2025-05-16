import os
from geocoding import batch_geocode_csv
from mrt_features import attach_nearest_mrt

def main():
    # 取得程式檔案所在目錄的絕對路徑
    current_dir = os.path.dirname(os.path.abspath(__file__))
    project_dir = os.path.dirname(current_dir)  # 上一層目錄
    
    # 設定輸入輸出檔案路徑
    input_file = os.path.join(project_dir, "dataset", "other_trades.csv")
    mrt_file = os.path.join(project_dir, "dataset", "mrt.csv")
    output_file = os.path.join(project_dir, "dataset", "other_trades_w_mrt.csv")
    
    # 設定處理參數
    address_col = "土地位置建物門牌"
    pause_sec = 0.2
    batch_size = 10

    # 確保輸出目錄存在
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    print("=== 開始處理房價數據 ===")
    print(f"輸入檔案: {input_file}")
    print(f"捷運站資料: {mrt_file}")
    print(f"輸出檔案: {output_file}")
    print(f"地址欄位: {address_col}")
    print(f"請求間隔: {pause_sec}秒")
    print(f"批次大小: {batch_size}")
    print("======================")

    # 步驟 1: 地理編碼處理
    print("\n1. 開始地理編碼處理...")
    batch_geocode_csv(
        path_csv=input_file,
        address_col=address_col,
        pause_sec=pause_sec,
        batch_size=batch_size
    )
    print("地理編碼處理完成！")

    # 步驟 2: 捷運站特徵處理
    print("\n2. 開始捷運站特徵處理...")
    attach_nearest_mrt(
        addr_csv_path=input_file,
        mrt_csv_path=mrt_file,
        output_csv_path=output_file
    )
    print("捷運站特徵處理完成！")

    print("\n=== 所有處理完成！===")
    print(f"處理後的數據已儲存至: {output_file}")

if __name__ == "__main__":
    main() 