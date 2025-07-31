import time

import polars as pl

from utils.data_prep import impute_and_rank

# Prepare input
features = (
    pl
    .read_parquet("data/raw/ctff_features.parquet")
    .get_column("features")
    .to_list()
)
df = pl.scan_parquet("data/raw/ctff_chars.parquet") # Change name from df to chars
ids = ["id", "excntry", "eom", "eom_ret", "ret_exc_lead1m", "ctff_test"]
group_cols = ["id", "excntry", "feature"]

# Impute and rank
start_time = time.perf_counter()
data_processed = impute_and_rank(
    df=df,
    ids=ids,
    features=features,
    feat_prank=True,  
    impute_flag=True,  
    group_cols=group_cols
)
print(f"Elapsed time: {time.perf_counter()-start_time:.6f} seconds") # 1000 sec for lazy API without streaming

# Save 
data_processed.write_parquet("data/processed/ctff_chars_processed.parquet")

