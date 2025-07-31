import polars as pl


def impute_and_rank(
    df: pl.LazyFrame | pl.DataFrame,
    ids: list[str],
    features: list[str],
    feat_prank: bool,
    impute_flag: bool,
    group_cols: list[str]
) -> pl.DataFrame:
    """
    Master function to prepare prediction data.
    1. Apply percentile ranks if feat_prank is True
    2. Impute missing values based on feat_prank flag
    """
    # Pivot data to long format (and make sure that value is numeric)
    df = (
        df
        .select(ids + features)  
        .unpivot(
            index = ids,        
            on = features,  
            variable_name = "feature", 
            value_name = "value"
        )
        .cast({"value": pl.Float64})
    )


    # Percentile rank
    if feat_prank:
        df = (
                df
                .with_columns(
                    zeroes = pl.col('value') == 0,
                    prank_raw = (
                        pl.col('value')
                        .rank(method="max") # R uses "max" for ECDF, but maybe min is better?
                        .over(group_cols) 
                    ) / (
                        pl.col('value')
                        .count()
                        .over(group_cols)
                    )
                )
                .with_columns(
                    # Set exact zeros to zero (value always return positive ranks)
                    value = pl.when(pl.col('zeroes')).then(0).otherwise(pl.col('prank_raw'))
                )
                .drop("zeroes", "prank_raw")
            )
        # Center percentile ranks centered around 0
        df = (
            df
            .with_columns(
                value = pl.col("value") - 0.5
            )
        )
    
    # Imputation
    if impute_flag and feat_prank:
        df = df.fill_null(0)

    if impute_flag and not feat_prank:
        df = df.fill_null(
                value = pl.col('value').median().over(group_cols)
            )

    return df.collect()
