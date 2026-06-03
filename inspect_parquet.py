from pathlib import Path
import pandas as pd

# Project folders
PROJECT_ROOT = Path(r"C:\Users\344792\Gokce\GIT PROJECTS\DisaggCPI\CPI-disaggregation-in-PT")
FAME_EXPORTS_DIR = PROJECT_ROOT / "data" / "fame_exports"
RAW_DIR = FAME_EXPORTS_DIR / "raw"


def convert_folder(folder: Path):
    if not folder.exists():
        print(f"Folder does not exist: {folder}")
        return

    parquet_files = sorted(folder.glob("*.parquet"))

    if not parquet_files:
        print(f"No parquet files found in: {folder}")
        return

    print(f"\nConverting files in: {folder}")

    for file_path in parquet_files:
        csv_path = file_path.with_suffix(".csv")

        df = pd.read_parquet(file_path)
        df.to_csv(csv_path, index=False)

        print(f"Converted: {file_path.name} -> {csv_path.name}")


convert_folder(FAME_EXPORTS_DIR)
convert_folder(RAW_DIR)

print("\nDone.")