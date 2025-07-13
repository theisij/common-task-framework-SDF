import pandas as pd
import polars as pl


def one_over_n(chars: pd.DataFrame) -> pl.DataFrame:
    df = (
        pl.from_pandas(chars)
        .lazy()
        .filter(pl.col('ctff_test') == 1)
        .select(['id', 'eom'])
        .with_columns(
            w = 1 / pl.count('id').over('eom')
        )
        .collect()
    )
    return df
    

def main(chars: pd.DataFrame, features: pd.DataFrame, daily_ret: pd.DataFrame) -> pd.DataFrame:
    """
    Main function to load packages, prepare data, train model, and calculate portfolio weights.
    Args:
        chars (pd.DataFrame): DataFrame containing characteristics data.
        features (pd.Series): Series containing feature names.
        daily_ret (str): DataFrame containing daily returns data.
    Returns:
        pd.DataFrame: DataFrame with columns 'id', eom, and 'w'.
    """
    # Prepare the data
    # chars = pd.read_parquet('data/raw/ctff_chars.parquet') 
    df = one_over_n(chars)

    # Output
    return df.to_pandas()

if __name__ == "__main__":
    features, chars, daily_ret = load_data()
    pf = main(chars, features, daily_ret)
    export_data(pf)
