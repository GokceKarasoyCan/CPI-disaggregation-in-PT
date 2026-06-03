from pathlib import Path
import argparse

import pandas as pd


def default_output_path(input_path: Path) -> Path:
    return input_path.with_suffix(".csv")


def convert_parquet_to_csv(input_path: Path, output_path: Path) -> None:
    if not input_path.exists():
        raise FileNotFoundError(f"Parquet file not found: {input_path}")

    output_path.parent.mkdir(parents=True, exist_ok=True)

    df = pd.read_parquet(input_path)
    df.to_csv(output_path, index=False)

    print(f"Input: {input_path}")
    print(f"Output: {output_path}")
    print(f"Rows: {len(df)}")
    print(f"Columns: {len(df.columns)}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Convert parquet files to CSV for quick viewing in Excel."
    )
    parser.add_argument(
        "--file",
        type=Path,
        default=Path("data/fame_exports/raw/cpi_vintages.parquet"),
        help="Input parquet file path.",
    )
    parser.add_argument(
        "--out",
        type=Path,
        default=None,
        help="Output CSV path. If omitted, uses same name as input with .csv extension.",
    )

    args = parser.parse_args()
    output_path = args.out if args.out is not None else default_output_path(args.file)

    convert_parquet_to_csv(args.file, output_path)


if __name__ == "__main__":
    main()
