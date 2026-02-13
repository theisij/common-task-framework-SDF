# Factor covariance matrix
fct_cov <- factor_cov[[d]]
# Factor chars 
factor_char_sub <- char_data[eom == as.Date(d)]
# Add specific risk
factor_char_sub %>% setorder(id)  # Important to align ids across different datasets
X <- factor_char_sub[, str_remove_all(colnames(fct_cov), "`"), with=F] %>% as.matrix() # Using colnames(fct_cov) is important so char and factor is aligned across x and fct_cov
# Stock covariance matrix
D <- diag(factor_char_sub$res_vol^2)
cov_est <- X %*% fct_cov %*% t(X) + D

# Woodbury matrix identity
library(WoodburyMatrix)
# With solve 
act <- solve(cov_est)
# with woodbury manually

# with library (don't understad)
?WoodburyMatrix
W <- WoodburyMatrix(
  A = D,
  B = fct_cov,
  X = X,
  symmetric = T
)
n <- nrow(act)
ones <- matrix(1, n, 1)
(act %*% ones)[1:4]
(solve(W) %*% ones)[1:4]
