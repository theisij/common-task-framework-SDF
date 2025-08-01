# %%
# Method: MVO portfolio of IPCA factors (Kelly, Pruitt, Su (JFE, 2019))
import pandas as pd
import numpy as np
from ipca import InstrumentedPCA
from datetime import timedelta

from tqdm import tqdm

def ecdf(data: pd.Series) -> pd.Series:
    """ Example helper function for ecdf. """
    if data.empty:
        return data
    sorted_data = data.sort_values()
    ranks = sorted_data.rank(method='max', pct=True)
    cdf_values = ranks
    return pd.Series(cdf_values, index=data.index)

def prepare_data(chars: pd.DataFrame, features: pd.Series, eom: str) -> pd.DataFrame:
    """Example helper function to apply an ECDF transformation grouped by 'eom'"""
    for feature in features:
        is_zero = chars[feature] == 0  # Preserve zeros
        chars[feature] = chars.groupby(eom)[feature].transform(lambda x: ecdf(x))
        chars.loc[is_zero, feature] = 0  # Restore zeros
        chars[feature].fillna(0.5, inplace=True)  # Impute missing values
    chars[features] -= 0.5
    return chars


def run_ipca(chars: pd.DataFrame, features: list[str], window_length: int) -> pd.DataFrame:
    pf_dates = chars.loc[chars.ctff_test, "eom_ret"].sort_values().unique()
    results = []

    for d in tqdm(pf_dates):
        chars_train = chars[(chars['eom_ret'] <= d) & (chars['eom_ret'] >= (d + timedelta(days=1) - pd.DateOffset(months=window_length) - timedelta(days=1)).date())]
        train = chars_train[chars_train['eom_ret'] < d][['id', 'eom', 'ret_exc_lead1m'] + features]
        train = train.set_index(['id', 'eom'])
        py_X = train.drop(columns=['ret_exc_lead1m'])
        py_y = train['ret_exc_lead1m']

        regr = InstrumentedPCA(n_factors=5, intercept=False)
        regr.fit(py_X, py_y, quiet=True)
        gamma = np.array(regr.get_factors(label_ind=True)[0])
        fct_ret = regr.get_factors(label_ind=True)[1].T

        mu = fct_ret.mean()
        inv = np.linalg.inv(fct_ret.cov())
        wf = inv @ np.array(mu)

        X_t = np.array(chars.loc[chars["eom_ret"] == d, features])
        w = X_t @ gamma @ np.linalg.inv(gamma.T @ X_t.T @ X_t @ gamma) @ wf

        result_df = chars.loc[chars["eom_ret"] == d, ["id", "eom"]].copy()
        result_df["w"] = w
        results.append(result_df)

    # Concatenate all results into a single DataFrame
    final_results = pd.concat(results, ignore_index=True)

    return final_results

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
    chars = prepare_data(chars, features, "eom")

    pf_ipca = run_ipca(chars, features, window_length=120)

    # Output
    return pf_ipca


# Test code and save output (SET TO FALSE WHEN SUBMITTING)
if False:
    chars = pd.read_parquet('data/raw/ctff_chars.parquet')
    features = pd.read_parquet('data/raw/ctff_features.parquet')
    daily_ret = pd.read_parquet('data/raw/ctff_daily_ret.parquet')
    output = main(chars=chars, features=features, daily_ret=daily_ret)
    output.to_csv('data/processed/one_over_n/one_over_n.csv', index=False)


if __name__ == "__main__":
    features, chars, daily_ret = load_data()
    pf = main(chars, features, daily_ret)
    export_data(pf)
