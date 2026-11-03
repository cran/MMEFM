## ----setup, include=FALSE-----------------------------------------------------
library(MMEFM)

## ----simulation---------------------------------------------------------------
rank <- list(r1 = 1L, l1 = 1L, r2 = c(a = 1L, b = 1L, c = 1L), l2 = c(a = 1L, b = 1L, c = 1L),
             kr = 1L, kc = 1L, kr_m = c(a = 1L, b = 1L, c = 1L), kc_m = c(a = 1L, b = 1L, c = 1L))
simulation <- gen_MMEFM(TT = 40L, p = c(a = 6L, b = 7L, c = 8L), q = c(a = 7L, b = 8L, c = 6L),
                        rank = rank, burn = 20L, filter_length = 40L, seed = 2026L)
Xt <- simulation$Xt
rank <- simulation$rank
lapply(Xt, dim)

## ----estimation---------------------------------------------------------------
fit <- est_MMEFM(Xt, rank = rank, K0 = 8L, max_iter = 50L, tol = 1e-4, seed = 2026L)
summary(fit)

## ----fitted-components--------------------------------------------------------
predicted <- fitted(fit)
global_main <- fitted(fit, component = "global_main")
local_main <- fitted(fit, component = "local_main")
global_common <- fitted(fit, component = "global_common")
local_common <- fitted(fit, component = "local_common")
errors <- residuals(fit)
lapply(predicted, dim)
vapply(errors, function(x) sqrt(mean(x^2)), numeric(1L))

## ----automatic-ranks, eval=FALSE----------------------------------------------
# selected_rank <- select_MMEFM_rank(Xt, K0 = 8L, max_iter = 50L, seed = 2026L)
# automatic_fit <- est_MMEFM(Xt, rank = NULL, K0 = 8L, max_iter = 50L, seed = 2026L)

## ----detection----------------------------------------------------------------
detection <- detect_MMEFM_global(fit, B = 3L, K0 = 8L, max_iter = 50L, tol = 1e-4,
                                 parallel = FALSE, seed = 2026L)
detection
detection$statistic
detection$selected

