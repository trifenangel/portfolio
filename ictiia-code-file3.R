# =========================================================
# FINAL MASTER PIPELINE
# RETURN PREDICTION -> PRICE RECONSTRUCTION
# BASELINE + CROSS-MARKET + REGIME + EV + CONFLICT
# =========================================================

# =========================================================
# 0. LOAD PACKAGES
# =========================================================
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(writexl)
library(zoo)
library(ggplot2)
library(xgboost)
library(lmtest)
library(ranger)

# OPTIONAL
library(lightgbm)

# =========================================================
# 0.1 GLOBAL SETTINGS
# =========================================================
start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2025-06-30")
split_date <- as.Date("2022-01-01")

# =========================================================
# 1. HELPER FUNCTIONS
# =========================================================

clean_market_data <- function(file_path) {
  df <- read_excel(file_path) %>%
    clean_names()
  
  if ("x1" %in% names(df)) {
    df <- df %>% rename(date = x1)
  }
  
  if (!"date" %in% names(df)) {
    stop(paste("Kolom 'date' tidak ditemukan di file:", file_path))
  }
  
  df <- df %>%
    mutate(date = as.Date(date)) %>%
    mutate(across(-date, as.numeric)) %>%
    filter(!is.na(date)) %>%
    filter(date >= start_date & date <= end_date) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date)
  
  cat("\n=============================\n")
  cat("FILE:", file_path, "\n")
  cat("Jumlah baris setelah cleaning:", nrow(df), "\n")
  cat("Range tanggal:\n")
  print(range(df$date, na.rm = TRUE))
  cat("\nMissing values per kolom:\n")
  print(colSums(is.na(df)))
  
  return(df)
}

make_features <- function(df, price_col = "adjusted") {
  df %>%
    arrange(date) %>%
    mutate(
      log_return = log(.data[[price_col]] / lag(.data[[price_col]])),
      rv1  = log_return^2,
      rv7  = rollapply(log_return^2, width = 7,  FUN = sum, fill = NA, align = "right"),
      rv15 = rollapply(log_return^2, width = 15, FUN = sum, fill = NA, align = "right"),
      rv30 = rollapply(log_return^2, width = 30, FUN = sum, fill = NA, align = "right")
    )
}
eval_metrics_return <- function(actual, pred) {
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - pred), na.rm = TRUE)
  mean_return <- mean(actual, na.rm = TRUE)
  
  data.frame(
    RMSE = rmse,
    MAE = mae,
    MEAN_RETURN = mean_return
  )
}


eval_price_metrics <- function(actual, pred) {
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - pred), na.rm = TRUE)
  mape <- mean(abs((actual - pred) / actual), na.rm = TRUE) * 100
  data.frame(RMSE = rmse, MAE = mae, MAPE = mape)
}

check_significance <- function(model, var) {
  coef_table <- summary(model)$coefficients
  if (!(var %in% rownames(coef_table))) return("NA")
  pval <- coef_table[var, "Pr(>|t|)"]
  if (is.na(pval)) return("NA")
  if (pval < 0.05) "Signifikan" else "Tidak Signifikan"
}

compare_direction <- function(df, var1, var2) {
  g1 <- df %>% filter(Feature == var1) %>% pull(Gain)
  g2 <- df %>% filter(Feature == var2) %>% pull(Gain)
  
  if (length(g1) == 0 | length(g2) == 0) return(NA)
  
  if (g1 > g2) {
    paste(var1, "lebih dominan")
  } else {
    paste(var2, "lebih dominan")
  }
}

calc_regime_metrics <- function(data, actual, predicted, regime_var, asset_name, model_name) {
  data %>%
    group_by(.data[[regime_var]]) %>%
    summarise(
      N = n(),
      RMSE = sqrt(mean((.data[[actual]] - .data[[predicted]])^2, na.rm = TRUE)),
      MAE  = mean(abs(.data[[actual]] - .data[[predicted]]), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Asset = asset_name,
      Model = model_name,
      Regime_Type = regime_var
    ) %>%
    rename(Regime = all_of(regime_var)) %>%
    select(Asset, Model, Regime_Type, Regime, N, RMSE, MAE)
}

# =========================================================
# 2. LOAD MARKET DATA
# =========================================================
btc  <- clean_market_data("data_bitcoin_daily_2018_2025.xlsx")
sp   <- clean_market_data("data_sp500_daily_2018_2025.xlsx")
oil  <- clean_market_data("data_oil_daily_2018_2025.xlsx")
vix  <- clean_market_data("data_vix_daily_2018_2025.xlsx")
gold <- clean_market_data("gold_daily_2018_2025.xlsx")
gpr  <- clean_market_data("data_gpr_daily_recent.xls")

# =========================================================
# 3. FEATURE ENGINEERING
# =========================================================
btc  <- make_features(btc)
sp   <- make_features(sp)
oil  <- make_features(oil)
gold <- make_features(gold)

vix <- vix %>%
  arrange(date) %>%
  mutate(vix = adjusted)

gpr <- gpr %>%
  arrange(date)

# =========================================================
# 4. MERGE MASTER DATASET
# =========================================================
btc_merge <- btc %>%
  select(date,
         btc_price = adjusted,
         btc_return = log_return,
         btc_rv30 = rv30)

sp_merge <- sp %>%
  select(date,
         sp_price = adjusted,
         sp_return = log_return,
         sp_rv30 = rv30)

oil_merge <- oil %>%
  select(date,
         oil_price = adjusted,
         oil_return = log_return,
         oil_rv30 = rv30)

gold_merge <- gold %>%
  select(date,
         gold_price = adjusted,
         gold_return = log_return,
         gold_rv30 = rv30)

vix_merge <- vix %>%
  select(date, vix)

gpr_merge <- gpr %>%
  select(date, gpr = gprd)

data_all <- btc_merge %>%
  left_join(sp_merge, by = "date") %>%
  left_join(oil_merge, by = "date") %>%
  left_join(gold_merge, by = "date") %>%
  left_join(vix_merge, by = "date") %>%
  left_join(gpr_merge, by = "date") %>%
  arrange(date)

# =========================================================
# 5. FILTER COMPLETE CASES
# =========================================================
data_all <- data_all %>%
  filter(
    !is.na(btc_price),
    !is.na(sp_price),
    !is.na(oil_price),
    !is.na(gold_price),
    !is.na(vix),
    !is.na(gpr),
    !is.na(btc_return),
    !is.na(sp_return),
    !is.na(oil_return),
    !is.na(gold_return),
    !is.na(btc_rv30),
    !is.na(sp_rv30),
    !is.na(oil_rv30),
    !is.na(gold_rv30)
  )

# =========================================================
# 6. CORRELATION ANALYSIS
# =========================================================
correlation_results <- data.frame(
  Pair = c(
    "sp_return vs oil_return",
    "sp_return vs gold_return",
    "btc_return vs oil_return",
    "btc_return vs gold_return",
    "oil_return vs gold_return"
  ),
  Correlation = c(
    cor(data_all$sp_return,   data_all$oil_return,  use = "complete.obs"),
    cor(data_all$sp_return,   data_all$gold_return, use = "complete.obs"),
    cor(data_all$btc_return,  data_all$oil_return,  use = "complete.obs"),
    cor(data_all$btc_return,  data_all$gold_return, use = "complete.obs"),
    cor(data_all$oil_return,  data_all$gold_return, use = "complete.obs")
  )
)

print(correlation_results)
write_xlsx(correlation_results, "correlation_results.xlsx")

# Optional plot
plot_returns <- data_all %>%
  select(date, sp_return, oil_return, gold_return) %>%
  pivot_longer(-date)

p_returns <- ggplot(plot_returns, aes(date, value, color = name)) +
  geom_line() +
  labs(title = "Return Dynamics: SP500 vs Oil vs Gold")

ggsave("return_dynamics.png", p_returns, width = 10, height = 5)

# =========================================================
# 7. TARGET CREATION
# =========================================================
data_all <- data_all %>%
  arrange(date) %>%
  mutate(
    target_oil_return  = lead(oil_return),
    target_gold_return = lead(gold_return),
    next_oil_price     = lead(oil_price),
    next_gold_price    = lead(gold_price)
  ) %>%
  filter(
    !is.na(target_oil_return),
    !is.na(target_gold_return),
    !is.na(next_oil_price),
    !is.na(next_gold_price)
  )

# =========================================================
# 8. BASE TRAIN TEST SPLIT
# =========================================================
train_data <- data_all %>%
  filter(date < split_date)

test_data <- data_all %>%
  filter(date >= split_date)

cat("Train dim:", dim(train_data), "\n")
cat("Test dim :", dim(test_data), "\n")
cat("Train range:", range(train_data$date), "\n")
cat("Test range :", range(test_data$date), "\n")

# =========================================================
# 9. REGIME CONSTRUCTION
# =========================================================
build_regime_cutoffs <- function(train_df) {
  list(
    p50_btc = quantile(train_df$btc_rv30, 0.50, na.rm = TRUE),
    p90_btc = quantile(train_df$btc_rv30, 0.90, na.rm = TRUE),
    p50_gpr = quantile(train_df$gpr, 0.50, na.rm = TRUE),
    p90_gpr = quantile(train_df$gpr, 0.90, na.rm = TRUE),
    p50_vix = quantile(train_df$vix, 0.50, na.rm = TRUE),
    p90_vix = quantile(train_df$vix, 0.90, na.rm = TRUE)
  )
}

apply_regimes <- function(df, cutoffs) {
  df %>%
    mutate(
      vol_regime = case_when(
        btc_rv30 < cutoffs$p50_btc ~ "S1",
        btc_rv30 >= cutoffs$p50_btc & btc_rv30 < cutoffs$p90_btc ~ "S2",
        btc_rv30 >= cutoffs$p90_btc ~ "S3"
      ),
      gpr_regime = case_when(
        gpr < cutoffs$p50_gpr ~ "G1",
        gpr >= cutoffs$p50_gpr & gpr < cutoffs$p90_gpr ~ "G2",
        gpr >= cutoffs$p90_gpr ~ "G3"
      ),
      vix_regime = case_when(
        vix < cutoffs$p50_vix ~ "V1",
        vix >= cutoffs$p50_vix & vix < cutoffs$p90_vix ~ "V2",
        vix >= cutoffs$p90_vix ~ "V3"
      ),
      vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
      gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
      vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
    )
}

cutoffs <- build_regime_cutoffs(train_data)
train_data <- apply_regimes(train_data, cutoffs)
test_data  <- apply_regimes(test_data, cutoffs)

# =========================================================
# 10. PREPARE DATASETS
# =========================================================

# ---------- BASELINE ----------
train_oil_base <- train_data %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

test_oil_base <- test_data %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

train_gold_base <- train_data %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

test_gold_base <- test_data %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

# ---------- CROSS MARKET ----------
train_oil_cross <- train_data %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

test_oil_cross <- test_data %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

train_gold_cross <- train_data %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

test_gold_cross <- test_data %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    vol_regime, gpr_regime, vix_regime
  )

# =========================================================
# 11. LINEAR MODELS - BASELINE
# =========================================================
model_oil_lm_base <- lm(
  target_oil_return ~ oil_return + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr,
  data = train_oil_base
)
pred_oil_lm_base <- predict(model_oil_lm_base, newdata = test_oil_base)
oil_lm_base_metrics <- eval_metrics_return(test_oil_base$target_oil_return, pred_oil_lm_base)

model_gold_lm_base <- lm(
  target_gold_return ~ gold_return + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr,
  data = train_gold_base
)
pred_gold_lm_base <- predict(model_gold_lm_base, newdata = test_gold_base)
gold_lm_base_metrics <- eval_metrics_return(test_gold_base$target_gold_return, pred_gold_lm_base)

# =========================================================
# 12. LINEAR MODELS - CROSS MARKET
# =========================================================
model_oil_lm_cross <- lm(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr,
  data = train_oil_cross
)
pred_oil_lm_cross <- predict(model_oil_lm_cross, newdata = test_oil_cross)
oil_lm_cross_metrics <- eval_metrics_return(test_oil_cross$target_oil_return, pred_oil_lm_cross)

model_gold_lm_cross <- lm(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr,
  data = train_gold_cross
)
pred_gold_lm_cross <- predict(model_gold_lm_cross, newdata = test_gold_cross)
gold_lm_cross_metrics <- eval_metrics_return(test_gold_cross$target_gold_return, pred_gold_lm_cross)

# =========================================================
# 13. LINEAR MODELS - CROSS + REGIME
# =========================================================
model_oil_lm_cross_reg <- lm(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    vol_regime + gpr_regime + vix_regime,
  data = train_oil_cross
)
pred_oil_lm_cross_reg <- predict(model_oil_lm_cross_reg, newdata = test_oil_cross)
oil_lm_cross_reg_metrics <- eval_metrics_return(test_oil_cross$target_oil_return, pred_oil_lm_cross_reg)

model_gold_lm_cross_reg <- lm(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    vol_regime + gpr_regime + vix_regime,
  data = train_gold_cross
)
pred_gold_lm_cross_reg <- predict(model_gold_lm_cross_reg, newdata = test_gold_cross)
gold_lm_cross_reg_metrics <- eval_metrics_return(test_gold_cross$target_gold_return, pred_gold_lm_cross_reg)

# =========================================================
# 14. XGBOOST HELPERS
# =========================================================
params_xgb <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

run_xgb <- function(train_x, test_x, y_train, y_test, params = params_xgb, nrounds = 200) {
  dtrain <- xgb.DMatrix(as.matrix(train_x), label = y_train)
  dtest  <- xgb.DMatrix(as.matrix(test_x),  label = y_test)
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    evals = list(train = dtrain, test = dtest),
    early_stopping_rounds = 20,
    print_every_n = 20
  )
  
  pred <- predict(model, dtest)
  
  list(
    model = model,
    pred = pred,
    metrics = eval_metrics_return(y_test, pred),
    dtest = dtest
  )
}

# =========================================================
# 15. XGBOOST - BASELINE
# =========================================================
x_train_oil_base <- train_oil_base %>%
  select(oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr)

x_test_oil_base <- test_oil_base %>%
  select(oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr)

res_oil_xgb_base <- run_xgb(
  x_train_oil_base, x_test_oil_base,
  train_oil_base$target_oil_return,
  test_oil_base$target_oil_return
)

model_oil_xgb_base <- res_oil_xgb_base$model
pred_oil_xgb_base <- res_oil_xgb_base$pred
oil_xgb_base_metrics <- res_oil_xgb_base$metrics

x_train_gold_base <- train_gold_base %>%
  select(gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr)

x_test_gold_base <- test_gold_base %>%
  select(gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr)

res_gold_xgb_base <- run_xgb(
  x_train_gold_base, x_test_gold_base,
  train_gold_base$target_gold_return,
  test_gold_base$target_gold_return
)

model_gold_xgb_base <- res_gold_xgb_base$model
pred_gold_xgb_base <- res_gold_xgb_base$pred
gold_xgb_base_metrics <- res_gold_xgb_base$metrics

# =========================================================
# 16. XGBOOST - CROSS MARKET
# =========================================================
x_train_oil_cross <- train_oil_cross %>%
  select(oil_return, gold_return, sp_return, oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr)

x_test_oil_cross <- test_oil_cross %>%
  select(oil_return, gold_return, sp_return, oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr)

res_oil_xgb_cross <- run_xgb(
  x_train_oil_cross, x_test_oil_cross,
  train_oil_cross$target_oil_return,
  test_oil_cross$target_oil_return
)

model_oil_xgb_cross <- res_oil_xgb_cross$model
pred_oil_xgb_cross <- res_oil_xgb_cross$pred
oil_xgb_cross_metrics <- res_oil_xgb_cross$metrics

x_train_gold_cross <- train_gold_cross %>%
  select(gold_return, oil_return, sp_return, gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr)

x_test_gold_cross <- test_gold_cross %>%
  select(gold_return, oil_return, sp_return, gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr)

res_gold_xgb_cross <- run_xgb(
  x_train_gold_cross, x_test_gold_cross,
  train_gold_cross$target_gold_return,
  test_gold_cross$target_gold_return
)

model_gold_xgb_cross <- res_gold_xgb_cross$model
pred_gold_xgb_cross <- res_gold_xgb_cross$pred
gold_xgb_cross_metrics <- res_gold_xgb_cross$metrics

# =========================================================
# 17. XGBOOST - CROSS + REGIME
# =========================================================
x_train_oil_cross_reg <- model.matrix(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    vol_regime + gpr_regime + vix_regime,
  data = train_oil_cross
)[, -1]

x_test_oil_cross_reg <- model.matrix(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    vol_regime + gpr_regime + vix_regime,
  data = test_oil_cross
)[, -1]

res_oil_xgb_cross_reg <- run_xgb(
  x_train_oil_cross_reg, x_test_oil_cross_reg,
  train_oil_cross$target_oil_return,
  test_oil_cross$target_oil_return
)

model_oil_xgb_cross_reg <- res_oil_xgb_cross_reg$model
pred_oil_xgb_cross_reg <- res_oil_xgb_cross_reg$pred
oil_xgb_cross_reg_metrics <- res_oil_xgb_cross_reg$metrics

x_train_gold_cross_reg <- model.matrix(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    vol_regime + gpr_regime + vix_regime,
  data = train_gold_cross
)[, -1]

x_test_gold_cross_reg <- model.matrix(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    vol_regime + gpr_regime + vix_regime,
  data = test_gold_cross
)[, -1]

res_gold_xgb_cross_reg <- run_xgb(
  x_train_gold_cross_reg, x_test_gold_cross_reg,
  train_gold_cross$target_gold_return,
  test_gold_cross$target_gold_return
)

model_gold_xgb_cross_reg <- res_gold_xgb_cross_reg$model
pred_gold_xgb_cross_reg <- res_gold_xgb_cross_reg$pred
gold_xgb_cross_reg_metrics <- res_gold_xgb_cross_reg$metrics

# =========================================================
# 18. RANDOM FOREST - CROSS MARKET ONLY
# =========================================================
rf_oil_cross <- ranger(
  target_oil_return ~ .,
  data = train_oil_cross %>% select(-date, -oil_price, -next_oil_price),
  num.trees = 500,
  importance = "impurity"
)
pred_rf_oil_cross <- predict(rf_oil_cross, data = test_oil_cross)$predictions
oil_rf_cross_metrics <- eval_metrics_return(test_oil_cross$target_oil_return, pred_rf_oil_cross)

rf_gold_cross <- ranger(
  target_gold_return ~ .,
  data = train_gold_cross %>% select(-date, -gold_price, -next_gold_price),
  num.trees = 500,
  importance = "impurity"
)
pred_rf_gold_cross <- predict(rf_gold_cross, data = test_gold_cross)$predictions
gold_rf_cross_metrics <- eval_metrics_return(test_gold_cross$target_gold_return, pred_rf_gold_cross)

# =========================================================
# 19. PRICE RECONSTRUCTION
# =========================================================

# ---------- OIL ----------
oil_price_eval <- test_oil_cross %>%
  transmute(
    date = date,
    current_price = oil_price,
    actual_next_price = next_oil_price,
    
    pred_price_lm_base       = oil_price * exp(pred_oil_lm_base),
    pred_price_lm_cross      = oil_price * exp(pred_oil_lm_cross),
    pred_price_lm_cross_reg  = oil_price * exp(pred_oil_lm_cross_reg),
    
    pred_price_xgb_base      = oil_price * exp(pred_oil_xgb_base),
    pred_price_xgb_cross     = oil_price * exp(pred_oil_xgb_cross),
    pred_price_xgb_cross_reg = oil_price * exp(pred_oil_xgb_cross_reg),
    
    pred_price_rf_cross      = oil_price * exp(pred_rf_oil_cross)
  )

oil_price_lm_base_metrics       <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_lm_base)
oil_price_lm_cross_metrics      <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_lm_cross)
oil_price_lm_cross_reg_metrics  <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_lm_cross_reg)
oil_price_xgb_base_metrics      <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_xgb_base)
oil_price_xgb_cross_metrics     <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_xgb_cross)
oil_price_xgb_cross_reg_metrics <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_xgb_cross_reg)
oil_price_rf_cross_metrics      <- eval_price_metrics(oil_price_eval$actual_next_price, oil_price_eval$pred_price_rf_cross)

# ---------- GOLD ----------
gold_price_eval <- test_gold_cross %>%
  transmute(
    date = date,
    current_price = gold_price,
    actual_next_price = next_gold_price,
    
    pred_price_lm_base       = gold_price * exp(pred_gold_lm_base),
    pred_price_lm_cross      = gold_price * exp(pred_gold_lm_cross),
    pred_price_lm_cross_reg  = gold_price * exp(pred_gold_lm_cross_reg),
    
    pred_price_xgb_base      = gold_price * exp(pred_gold_xgb_base),
    pred_price_xgb_cross     = gold_price * exp(pred_gold_xgb_cross),
    pred_price_xgb_cross_reg = gold_price * exp(pred_gold_xgb_cross_reg),
    
    pred_price_rf_cross      = gold_price * exp(pred_rf_gold_cross)
  )

gold_price_lm_base_metrics       <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_lm_base)
gold_price_lm_cross_metrics      <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_lm_cross)
gold_price_lm_cross_reg_metrics  <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_lm_cross_reg)
gold_price_xgb_base_metrics      <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_xgb_base)
gold_price_xgb_cross_metrics     <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_xgb_cross)
gold_price_xgb_cross_reg_metrics <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_xgb_cross_reg)
gold_price_rf_cross_metrics      <- eval_price_metrics(gold_price_eval$actual_next_price, gold_price_eval$pred_price_rf_cross)

# =========================================================
# 20. FEATURE IMPORTANCE
# =========================================================
importance_oil_xgb_cross <- xgb.importance(
  feature_names = colnames(x_train_oil_cross),
  model = model_oil_xgb_cross
)

importance_gold_xgb_cross <- xgb.importance(
  feature_names = colnames(x_train_gold_cross),
  model = model_gold_xgb_cross
)

importance_oil_xgb_cross_reg <- xgb.importance(
  feature_names = colnames(x_train_oil_cross_reg),
  model = model_oil_xgb_cross_reg
)

importance_gold_xgb_cross_reg <- xgb.importance(
  feature_names = colnames(x_train_gold_cross_reg),
  model = model_gold_xgb_cross_reg
)

importance_oil_rf_cross <- data.frame(
  Feature = names(rf_oil_cross$variable.importance),
  Importance = rf_oil_cross$variable.importance
) %>% arrange(desc(Importance))

importance_gold_rf_cross <- data.frame(
  Feature = names(rf_gold_cross$variable.importance),
  Importance = rf_gold_cross$variable.importance
) %>% arrange(desc(Importance))

feature_focus_summary <- bind_rows(
  importance_oil_xgb_cross %>%
    filter(Feature %in% c("sp_return", "gold_return", "oil_return")) %>%
    mutate(Target = "Oil", Model = "XGB Cross"),
  
  importance_oil_xgb_cross_reg %>%
    filter(Feature %in% c("sp_return", "gold_return", "oil_return")) %>%
    mutate(Target = "Oil", Model = "XGB Cross + Regime"),
  
  importance_gold_xgb_cross %>%
    filter(Feature %in% c("sp_return", "oil_return", "gold_return")) %>%
    mutate(Target = "Gold", Model = "XGB Cross"),
  
  importance_gold_xgb_cross_reg %>%
    filter(Feature %in% c("sp_return", "oil_return", "gold_return")) %>%
    mutate(Target = "Gold", Model = "XGB Cross + Regime")
) %>%
  arrange(Target, Model, desc(Gain))

# =========================================================
# 21. REGIME-SPECIFIC EVALUATION
# =========================================================
test_eval <- test_data %>%
  mutate(
    pred_oil_xgb_cross      = pred_oil_xgb_cross,
    pred_oil_xgb_cross_reg  = pred_oil_xgb_cross_reg,
    pred_gold_xgb_cross     = pred_gold_xgb_cross,
    pred_gold_xgb_cross_reg = pred_gold_xgb_cross_reg
  )

regime_eval_results <- bind_rows(
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross",     "vol_regime", "Oil",  "XGB Cross"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross_reg", "vol_regime", "Oil",  "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross",     "gpr_regime", "Oil",  "XGB Cross"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross_reg", "gpr_regime", "Oil",  "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross",     "vix_regime", "Oil",  "XGB Cross"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross_reg", "vix_regime", "Oil",  "XGB Cross + Regime"),
  
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross",     "vol_regime", "Gold", "XGB Cross"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross_reg", "vol_regime", "Gold", "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross",     "gpr_regime", "Gold", "XGB Cross"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross_reg", "gpr_regime", "Gold", "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross",     "vix_regime", "Gold", "XGB Cross"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross_reg", "vix_regime", "Gold", "XGB Cross + Regime")
) %>%
  mutate(
    Regime = as.character(Regime),
    Stress_Level = case_when(
      Regime %in% c("S1", "G1", "V1") ~ "Low",
      Regime %in% c("S2", "G2", "V2") ~ "Medium",
      Regime %in% c("S3", "G3", "V3") ~ "High",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(Asset, Regime_Var, Model, Regime)

]x# tampilkan full hasil regime di console
print(regime_eval_results, n = Inf)

# ringkasan low vs high stress
regime_summary <- regime_eval_results %>%
  filter(Stress_Level %in% c("Low", "High")) %>%
  select(Asset, Model, Regime_Var, Stress_Level, RMSE, MAE) %>%
  tidyr::pivot_wider(
    names_from  = Stress_Level,
    values_from = c(RMSE, MAE)
  ) %>%
  mutate(
    RMSE_Change_High_vs_Low = RMSE_High - RMSE_Low,
    MAE_Change_High_vs_Low  = MAE_High - MAE_Low
  ) %>%
  arrange(Asset, Regime_Var, Model)

print(regime_summary, n = Inf)

# model improvement from adding regime variables
regime_model_compare <- regime_eval_results %>%
  select(Asset, Regime_Var, Regime, Model, RMSE, MAE) %>%
  tidyr::pivot_wider(
    names_from  = Model,
    values_from = c(RMSE, MAE)
  ) %>%
  mutate(
    RMSE_Improvement = `RMSE_XGB Cross` - `RMSE_XGB Cross + Regime`,
    MAE_Improvement  = `MAE_XGB Cross`  - `MAE_XGB Cross + Regime`
  ) %>%
  arrange(Asset, Regime_Var, Regime)

print(regime_model_compare, n = Inf)
View(regime_eval_results)
View(regime_summary)
View(regime_model_compare)

write_xlsx(
  list(
    regime_eval_results  = regime_eval_results,
    regime_summary       = regime_summary,
    regime_model_compare = regime_model_compare
  ),
  "regime_eval_results.xlsx"
)
# =========================================================
# 22. GRANGER TESTS
# =========================================================
granger_sp_to_oil   <- grangertest(oil_return ~ sp_return, order = 2, data = data_all)
granger_oil_to_sp   <- grangertest(sp_return ~ oil_return, order = 2, data = data_all)
granger_oil_to_gold <- grangertest(gold_return ~ oil_return, order = 2, data = data_all)
granger_gold_to_oil <- grangertest(oil_return ~ gold_return, order = 2, data = data_all)

write_xlsx(
  list(
    sp_to_oil = as.data.frame(granger_sp_to_oil),
    oil_to_sp = as.data.frame(granger_oil_to_sp),
    oil_to_gold = as.data.frame(granger_oil_to_gold),
    gold_to_oil = as.data.frame(granger_gold_to_oil)
  ),
  "granger_results.xlsx"
)

# =========================================================
# 23. DIRECTIONAL SUMMARY TABLE
# =========================================================
lm_sp <- lm(
  sp_return ~ oil_return + sp_rv30 + oil_rv30 + gold_rv30 + btc_rv30 + vix + gpr,
  data = train_data
)

linear_oil_to_sp   <- check_significance(lm_sp, "oil_return")
linear_sp_to_oil   <- check_significance(model_oil_lm_cross, "sp_return")
linear_oil_to_gold <- check_significance(model_gold_lm_cross, "oil_return")
linear_gold_to_oil <- check_significance(model_oil_lm_cross, "gold_return")

cor_sp_oil   <- cor(data_all$sp_return, data_all$oil_return, use = "complete.obs")
cor_oil_gold <- cor(data_all$oil_return, data_all$gold_return, use = "complete.obs")

xgb_sp_oil <- importance_oil_xgb_cross %>%
  filter(Feature %in% c("sp_return", "oil_return")) %>%
  select(Feature, Gain)

xgb_oil_gold <- importance_gold_xgb_cross %>%
  filter(Feature %in% c("oil_return", "gold_return")) %>%
  select(Feature, Gain)

xgb_sp_to_oil   <- compare_direction(xgb_sp_oil, "sp_return", "oil_return")
xgb_oil_to_sp   <- xgb_sp_to_oil
xgb_oil_to_gold <- compare_direction(xgb_oil_gold, "oil_return", "gold_return")
xgb_gold_to_oil <- xgb_oil_to_gold

final_table <- data.frame(
  Method = c("Granger", "Linear", "Correlation", "XGBoost"),
  Oil_to_SP = c(
    ifelse(granger_oil_to_sp$`Pr(>F)`[2] < 0.05, "Signifikan", "Tidak signifikan"),
    linear_oil_to_sp,
    round(cor_sp_oil, 3),
    xgb_oil_to_sp
  ),
  SP_to_Oil = c(
    ifelse(granger_sp_to_oil$`Pr(>F)`[2] < 0.05, "Signifikan", "Tidak signifikan"),
    linear_sp_to_oil,
    round(cor_sp_oil, 3),
    xgb_sp_to_oil
  ),
  Oil_to_Gold = c(
    ifelse(granger_oil_to_gold$`Pr(>F)`[2] < 0.05, "Signifikan", "Tidak signifikan"),
    linear_oil_to_gold,
    round(cor_oil_gold, 3),
    xgb_oil_to_gold
  ),
  Gold_to_Oil = c(
    ifelse(granger_gold_to_oil$`Pr(>F)`[2] < 0.05, "Signifikan", "Tidak signifikan"),
    linear_gold_to_oil,
    round(cor_oil_gold, 3),
    xgb_gold_to_oil
  )
)

print(final_table)


# =========================================================
# 24. RETURN RESULT TABLES
# =========================================================
return_results <- bind_rows(
  data.frame(Asset = "Oil",  Model = "LM Baseline", oil_lm_base_metrics),
  data.frame(Asset = "Oil",  Model = "LM Cross", oil_lm_cross_metrics),
  data.frame(Asset = "Oil",  Model = "LM Cross + Regime", oil_lm_cross_reg_metrics),
  data.frame(Asset = "Oil",  Model = "XGB Baseline", oil_xgb_base_metrics),
  data.frame(Asset = "Oil",  Model = "XGB Cross", oil_xgb_cross_metrics),
  data.frame(Asset = "Oil",  Model = "XGB Cross + Regime", oil_xgb_cross_reg_metrics),
  data.frame(Asset = "Oil",  Model = "RF Cross", oil_rf_cross_metrics),
  
  data.frame(Asset = "Gold", Model = "LM Baseline", gold_lm_base_metrics),
  data.frame(Asset = "Gold", Model = "LM Cross", gold_lm_cross_metrics),
  data.frame(Asset = "Gold", Model = "LM Cross + Regime", gold_lm_cross_reg_metrics),
  data.frame(Asset = "Gold", Model = "XGB Baseline", gold_xgb_base_metrics),
  data.frame(Asset = "Gold", Model = "XGB Cross", gold_xgb_cross_metrics),
  data.frame(Asset = "Gold", Model = "XGB Cross + Regime", gold_xgb_cross_reg_metrics),
  data.frame(Asset = "Gold", Model = "RF Cross", gold_rf_cross_metrics)
)

print(return_results)

# =========================================================
# 25. PRICE RESULT TABLES
# =========================================================
price_results <- bind_rows(
  data.frame(Asset = "Oil",  Model = "LM Baseline -> Price", oil_price_lm_base_metrics),
  data.frame(Asset = "Oil",  Model = "LM Cross -> Price", oil_price_lm_cross_metrics),
  data.frame(Asset = "Oil",  Model = "LM Cross + Regime -> Price", oil_price_lm_cross_reg_metrics),
  data.frame(Asset = "Oil",  Model = "XGB Baseline -> Price", oil_price_xgb_base_metrics),
  data.frame(Asset = "Oil",  Model = "XGB Cross -> Price", oil_price_xgb_cross_metrics),
  data.frame(Asset = "Oil",  Model = "XGB Cross + Regime -> Price", oil_price_xgb_cross_reg_metrics),
  data.frame(Asset = "Oil",  Model = "RF Cross -> Price", oil_price_rf_cross_metrics),
  
  data.frame(Asset = "Gold", Model = "LM Baseline -> Price", gold_price_lm_base_metrics),
  data.frame(Asset = "Gold", Model = "LM Cross -> Price", gold_price_lm_cross_metrics),
  data.frame(Asset = "Gold", Model = "LM Cross + Regime -> Price", gold_price_lm_cross_reg_metrics),
  data.frame(Asset = "Gold", Model = "XGB Baseline -> Price", gold_price_xgb_base_metrics),
  data.frame(Asset = "Gold", Model = "XGB Cross -> Price", gold_price_xgb_cross_metrics),
  data.frame(Asset = "Gold", Model = "XGB Cross + Regime -> Price", gold_price_xgb_cross_reg_metrics),
  data.frame(Asset = "Gold", Model = "RF Cross -> Price", gold_price_rf_cross_metrics)
)

print(price_results)

# =========================================================
# 26. FEATURE RANK SUMMARY
# =========================================================
feature_rank_summary <- bind_rows(
  importance_oil_xgb_cross %>% mutate(Target = "Oil", Model = "XGB Cross"),
  importance_oil_xgb_cross_reg %>% mutate(Target = "Oil", Model = "XGB Cross + Regime"),
  importance_gold_xgb_cross %>% mutate(Target = "Gold", Model = "XGB Cross"),
  importance_gold_xgb_cross_reg %>% mutate(Target = "Gold", Model = "XGB Cross + Regime")
) %>%
  group_by(Target, Model) %>%
  arrange(desc(Gain), .by_group = TRUE) %>%
  mutate(Rank = row_number()) %>%
  ungroup()

# =========================================================
# 27. EXPORT MAIN RESULTS
# =========================================================
write_xlsx(return_results, "return_results_master.xlsx")
write_xlsx(price_results, "price_results_master.xlsx")
write_xlsx(oil_price_eval, "oil_price_reconstruction_master.xlsx")
write_xlsx(gold_price_eval, "gold_price_reconstruction_master.xlsx")
write_xlsx(as.data.frame(importance_oil_xgb_cross), "importance_oil_xgb_cross.xlsx")
write_xlsx(as.data.frame(importance_gold_xgb_cross), "importance_gold_xgb_cross.xlsx")
write_xlsx(as.data.frame(importance_oil_xgb_cross_reg), "importance_oil_xgb_cross_reg.xlsx")
write_xlsx(as.data.frame(importance_gold_xgb_cross_reg), "importance_gold_xgb_cross_reg.xlsx")
write_xlsx(importance_oil_rf_cross, "importance_oil_rf_cross.xlsx")
write_xlsx(importance_gold_rf_cross, "importance_gold_rf_cross.xlsx")
write_xlsx(feature_focus_summary, "feature_focus_summary.xlsx")
write_xlsx(feature_rank_summary, "feature_rank_summary.xlsx")
write_xlsx(regime_eval_results, "regime_eval_results.xlsx")
write_xlsx(final_table, "directional_summary_table.xlsx")

# =========================================================
# 28. EV MODULE
# OPTIONAL EXTENSION
# =========================================================
ev_raw <- read_excel(
  "EV Data Explorer 2025.xlsx",
  sheet = "EV sales countries",
  col_names = FALSE
)

ev_header <- ev_raw[8, ] %>%
  unlist(use.names = FALSE) %>%
  as.character()

ev_header[1] <- "region_country"

ev_data <- ev_raw[-c(1:8), ]
names(ev_data) <- ev_header

ev_data <- ev_data %>%
  filter(!is.na(region_country))

names(ev_data) <- make_clean_names(names(ev_data))

ev_data <- ev_data %>%
  mutate(
    across(
      -region_country,
      ~ as.numeric(gsub("[^0-9]", "", as.character(.)))
    )
  )

ev_long <- ev_data %>%
  pivot_longer(
    cols = -region_country,
    names_to = "year",
    values_to = "ev_sales"
  ) %>%
  mutate(
    year = gsub("^x", "", year),
    year = as.integer(year),
    ev_sales = as.numeric(ev_sales)
  ) %>%
  arrange(region_country, year)

ev_global <- ev_long %>%
  group_by(year) %>%
  summarise(
    ev_sales = sum(ev_sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

ev_yearly <- ev_global %>%
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  arrange(date)

ev_daily <- data.frame(
  date = seq(start_date, end_date, by = "day")
) %>%
  left_join(ev_yearly %>% select(date, ev_sales), by = "date") %>%
  arrange(date) %>%
  mutate(
    ev_sales_interp = zoo::na.approx(ev_sales, x = date, na.rm = FALSE),
    ev_sales_interp = zoo::na.locf(ev_sales_interp, na.rm = FALSE),
    log_ev_sales = log(ev_sales_interp + 1)
  )

data_all_ev <- data_all %>%
  left_join(ev_daily %>% select(date, ev_sales_interp, log_ev_sales), by = "date")

write_xlsx(ev_long, "ev_long.xlsx")
write_xlsx(ev_global, "ev_global.xlsx")
write_xlsx(ev_daily, "ev_daily.xlsx")
write_xlsx(data_all_ev, "data_all_with_ev.xlsx")

# =========================================================
# 29. CONFLICT MODULE
# OPTIONAL EXTENSION
# =========================================================
eu_conflict <- read_excel("Europe-Central-Asia_aggregated_data_up_to_week_of-2026-03-14.xlsx")
me_conflict <- read_excel("Middle-East_aggregated_data_up_to_week_of-2026-03-14.xlsx")

conflict_raw <- bind_rows(eu_conflict, me_conflict)

conflict_weekly <- conflict_raw %>%
  filter(DISORDER_TYPE == "Political violence") %>%
  mutate(WEEK = as.Date(WEEK)) %>%
  group_by(WEEK) %>%
  summarise(
    conflict_events_weekly = sum(EVENTS, na.rm = TRUE),
    conflict_fatalities_weekly = sum(FATALITIES, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(WEEK) %>%
  mutate(
    log_conflict_events = log1p(conflict_events_weekly),
    log_conflict_fatalities = log1p(conflict_fatalities_weekly)
  )

conflict_daily <- conflict_weekly %>%
  mutate(
    week_start = WEEK,
    week_end = WEEK + 6
  ) %>%
  rowwise() %>%
  mutate(date = list(seq.Date(week_start, week_end, by = "day"))) %>%
  unnest(date) %>%
  ungroup() %>%
  select(
    date,
    conflict_events_weekly,
    conflict_fatalities_weekly,
    log_conflict_events,
    log_conflict_fatalities
  ) %>%
  filter(date >= start_date, date <= end_date)

data_all_ev_conf <- data_all_ev %>%
  left_join(conflict_daily, by = "date") %>%
  mutate(
    conflict_events_weekly = ifelse(is.na(conflict_events_weekly), 0, conflict_events_weekly),
    conflict_fatalities_weekly = ifelse(is.na(conflict_fatalities_weekly), 0, conflict_fatalities_weekly),
    log_conflict_events = ifelse(is.na(log_conflict_events), 0, log_conflict_events),
    log_conflict_fatalities = ifelse(is.na(log_conflict_fatalities), 0, log_conflict_fatalities)
  )

write_xlsx(conflict_weekly, "conflict_weekly.xlsx")
write_xlsx(conflict_daily, "conflict_daily.xlsx")
write_xlsx(data_all_ev_conf, "data_all_with_ev_conflict.xlsx")

# =========================================================
# 30. OPTIONAL ADVANCED MODEL WITH EV + CONFLICT
# =========================================================
train_ev_conf <- data_all_ev_conf %>%
  filter(date < split_date)

test_ev_conf <- data_all_ev_conf %>%
  filter(date >= split_date)

cutoffs_ev_conf <- build_regime_cutoffs(train_ev_conf)
train_ev_conf <- apply_regimes(train_ev_conf, cutoffs_ev_conf)
test_ev_conf  <- apply_regimes(test_ev_conf, cutoffs_ev_conf)

train_oil_ev_conf <- train_ev_conf %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales, log_conflict_events,
    vol_regime, gpr_regime, vix_regime
  )

test_oil_ev_conf <- test_ev_conf %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales, log_conflict_events,
    vol_regime, gpr_regime, vix_regime
  )

train_gold_ev_conf <- train_ev_conf %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales, log_conflict_events,
    vol_regime, gpr_regime, vix_regime
  )

test_gold_ev_conf <- test_ev_conf %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales, log_conflict_events,
    vol_regime, gpr_regime, vix_regime
  )

model_oil_lm_ev_conf <- lm(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales + log_conflict_events +
    vol_regime + gpr_regime + vix_regime,
  data = train_oil_ev_conf
)

pred_oil_lm_ev_conf <- predict(model_oil_lm_ev_conf, newdata = test_oil_ev_conf)
oil_lm_ev_conf_metrics <- eval_metrics_return(test_oil_ev_conf$target_oil_return, pred_oil_lm_ev_conf)

model_gold_lm_ev_conf <- lm(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales + log_conflict_events +
    vol_regime + gpr_regime + vix_regime,
  data = train_gold_ev_conf
)

pred_gold_lm_ev_conf <- predict(model_gold_lm_ev_conf, newdata = test_gold_ev_conf)
gold_lm_ev_conf_metrics <- eval_metrics_return(test_gold_ev_conf$target_gold_return, pred_gold_lm_ev_conf)

x_train_oil_ev_conf <- model.matrix(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales + log_conflict_events +
    vol_regime + gpr_regime + vix_regime,
  data = train_oil_ev_conf
)[, -1]

x_test_oil_ev_conf <- model.matrix(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales + log_conflict_events +
    vol_regime + gpr_regime + vix_regime,
  data = test_oil_ev_conf
)[, -1]

res_oil_xgb_ev_conf <- run_xgb(
  x_train_oil_ev_conf, x_test_oil_ev_conf,
  train_oil_ev_conf$target_oil_return,
  test_oil_ev_conf$target_oil_return
)

x_train_gold_ev_conf <- model.matrix(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales + log_conflict_events +
    vol_regime + gpr_regime + vix_regime,
  data = train_gold_ev_conf
)[, -1]

x_test_gold_ev_conf <- model.matrix(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales + log_conflict_events +
    vol_regime + gpr_regime + vix_regime,
  data = test_gold_ev_conf
)[, -1]

res_gold_xgb_ev_conf <- run_xgb(
  x_train_gold_ev_conf, x_test_gold_ev_conf,
  train_gold_ev_conf$target_gold_return,
  test_gold_ev_conf$target_gold_return
)

advanced_return_results <- bind_rows(
  data.frame(Asset = "Oil", Model = "LM EV + Conflict + Regime", oil_lm_ev_conf_metrics),
  data.frame(Asset = "Oil", Model = "XGB EV + Conflict + Regime", res_oil_xgb_ev_conf$metrics),
  data.frame(Asset = "Gold", Model = "LM EV + Conflict + Regime", gold_lm_ev_conf_metrics),
  data.frame(Asset = "Gold", Model = "XGB EV + Conflict + Regime", res_gold_xgb_ev_conf$metrics)
)

print(advanced_return_results)
write_xlsx(advanced_return_results, "advanced_return_results_ev_conflict.xlsx")

# =========================================================
# 31. DONE
# =========================================================
cat("\n=================================================\n")
cat("SELESAI\n")
cat("File output utama:\n")
cat("- return_results_master.xlsx\n")
cat("- price_results_master.xlsx\n")
cat("- directional_summary_table.xlsx\n")
cat("- feature_focus_summary.xlsx\n")
cat("- regime_eval_results.xlsx\n")
cat("- advanced_return_results_ev_conflict.xlsx\n")
cat("=================================================\n")

# =========================================================
# RANDOM FOREST + PRICE METRICS + COMPARISON 3 MODELS
# =========================================================
library(ranger)
library(dplyr)
library(writexl)

# =========================================================
# 1. RANDOM FOREST
# =========================================================

# ---------- OIL RF ----------
rf_oil_model <- ranger(
  target_oil_return ~ .,
  data = train_oil_cross %>% select(-date, -oil_price, -next_oil_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_rf_oil <- predict(rf_oil_model, data = test_oil_cross)$predictions

oil_rf_metrics <- eval_metrics_return(
  actual = test_oil_cross$target_oil_return,
  pred   = pred_rf_oil
)

# ---------- GOLD RF ----------
rf_gold_model <- ranger(
  target_gold_return ~ .,
  data = train_gold_cross %>% select(-date, -gold_price, -next_gold_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_rf_gold <- predict(rf_gold_model, data = test_gold_cross)$predictions

gold_rf_metrics <- eval_metrics_return(
  actual = test_gold_cross$target_gold_return,
  pred   = pred_rf_gold
)

print(oil_rf_metrics)
print(gold_rf_metrics)

# =========================================================
# 2. PRICE METRICS FOR LINEAR, XGBOOST, RF
# =========================================================

# ---------- OIL ----------
oil_price_lm_metrics <- eval_price_metrics(
  actual = test_oil_cross$next_oil_price,
  pred   = test_oil_cross$oil_price * exp(pred_oil_lm_cross)
)

oil_price_xgb_metrics <- eval_price_metrics(
  actual = test_oil_cross$next_oil_price,
  pred   = test_oil_cross$oil_price * exp(pred_oil_xgb_cross)
)

oil_price_rf_metrics <- eval_price_metrics(
  actual = test_oil_cross$next_oil_price,
  pred   = test_oil_cross$oil_price * exp(pred_rf_oil_cross)
)

# ---------- GOLD ----------
gold_price_lm_metrics <- eval_price_metrics(
  actual = test_gold_cross$next_gold_price,
  pred   = test_gold_cross$gold_price * exp(pred_gold_lm_cross)
)

gold_price_xgb_metrics <- eval_price_metrics(
  actual = test_gold_cross$next_gold_price,
  pred   = test_gold_cross$gold_price * exp(pred_gold_xgb_cross)
)

gold_price_rf_metrics <- eval_price_metrics(
  actual = test_gold_cross$next_gold_price,
  pred   = test_gold_cross$gold_price * exp(pred_rf_gold_cross)
)

print(oil_price_lm_metrics)
print(oil_price_xgb_metrics)
print(oil_price_rf_metrics)

print(gold_price_lm_metrics)
print(gold_price_xgb_metrics)
print(gold_price_rf_metrics)

# =========================================================
# 3. COMPARISON TABLE - RETURN
# =========================================================
comparison_return <- bind_rows(
  data.frame(Asset = "Oil",  Model = "Linear",        oil_price_lm_metrics),
  data.frame(Asset = "Oil",  Model = "XGBoost",       oil_price_xgb_metrics),
  data.frame(Asset = "Oil",  Model = "Random Forest", oil_rf_metrics),
  
  data.frame(Asset = "Gold", Model = "Linear",        gold_price_lm_metrics),
  data.frame(Asset = "Gold", Model = "XGBoost",       gold_price_xgb_metrics),
  data.frame(Asset = "Gold", Model = "Random Forest", gold_rf_metrics)
)

print(comparison_return)
write_xlsx(comparison_return, "comparison_return_3models.xlsx")

# =========================================================
# 4. COMPARISON TABLE - PRICE
# =========================================================
comparison_price <- bind_rows(
  data.frame(Asset = "Oil",  Model = "Linear",        oil_lm_cross_metrics),
  data.frame(Asset = "Oil",  Model = "XGBoost",       oil_xgb_cross_metrics),
  data.frame(Asset = "Oil",  Model = "Random Forest", oil_rf_metrics),
  
  data.frame(Asset = "Gold", Model = "Linear",        gold_lm_cross_metrics),
  data.frame(Asset = "Gold", Model = "XGBoost",       gold_xgb_cross_metrics),
  data.frame(Asset = "Gold", Model = "Random Forest", gold_rf_metrics)
)

print(comparison_price)
write_xlsx(comparison_price, "comparison_price_3models.xlsx")

# =========================================================
# 5. BEST MODEL BASED ON RETURN RMSE
# =========================================================
best_model_return <- comparison_return %>%
  group_by(Asset) %>%
  arrange(RMSE, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

print(best_model_return)
write_xlsx(best_model_return, "best_model_return_3models.xlsx")

# =========================================================
# 6. BEST MODEL BASED ON PRICE RMSE
# =========================================================
best_model_price <- comparison_price %>%
  group_by(Asset) %>%
  arrange(RMSE, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

print(best_model_price)
write_xlsx(best_model_price, "best_model_price_3models.xlsx")

# =========================================================
# 7. RF FEATURE IMPORTANCE
# =========================================================
importance_oil_rf <- data.frame(
  Feature = names(rf_oil_model$variable.importance),
  Importance = rf_oil_model$variable.importance
) %>%
  arrange(desc(Importance))

importance_gold_rf <- data.frame(
  Feature = names(rf_gold_model$variable.importance),
  Importance = rf_gold_model$variable.importance
) %>%
  arrange(desc(Importance))

print(importance_oil_rf)
print(importance_gold_rf)

write_xlsx(importance_oil_rf, "importance_oil_rf.xlsx")
write_xlsx(importance_gold_rf, "importance_gold_rf.xlsx")

# =========================================================
# 30.1 RANDOM FOREST - EV + CONFLICT + REGIME
# OPTIONAL EXTENSION
# =========================================================

# ---------- OIL RF EV + CONFLICT + REGIME ----------
rf_oil_ev_conf <- ranger(
  target_oil_return ~ .,
  data = train_oil_ev_conf %>% select(-date, -oil_price, -next_oil_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_oil_rf_ev_conf <- predict(rf_oil_ev_conf, data = test_oil_ev_conf)$predictions

oil_rf_ev_conf_metrics <- eval_metrics_return(
  actual = test_oil_ev_conf$target_oil_return,
  pred   = pred_oil_rf_ev_conf
)

# ---------- GOLD RF EV + CONFLICT + REGIME ----------
rf_gold_ev_conf <- ranger(
  target_gold_return ~ .,
  data = train_gold_ev_conf %>% select(-date, -gold_price, -next_gold_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_gold_rf_ev_conf <- predict(rf_gold_ev_conf, data = test_gold_ev_conf)$predictions

gold_rf_ev_conf_metrics <- eval_metrics_return(
  actual = test_gold_ev_conf$target_gold_return,
  pred   = pred_gold_rf_ev_conf
)

print(oil_rf_ev_conf_metrics)
print(gold_rf_ev_conf_metrics)

# =========================================================
# 30.2 PRICE METRICS - RF EV + CONFLICT + REGIME
# =========================================================

# ---------- OIL PRICE ----------
oil_price_rf_ev_conf_metrics <- eval_price_metrics(
  actual = test_oil_ev_conf$next_oil_price,
  pred   = test_oil_ev_conf$oil_price * exp(pred_oil_rf_ev_conf)
)

# ---------- GOLD PRICE ----------
gold_price_rf_ev_conf_metrics <- eval_price_metrics(
  actual = test_gold_ev_conf$next_gold_price,
  pred   = test_gold_ev_conf$gold_price * exp(pred_gold_rf_ev_conf)
)

print(oil_price_rf_ev_conf_metrics)
print(gold_price_rf_ev_conf_metrics)

# =========================================================
# 30.3 FEATURE IMPORTANCE - RF EV + CONFLICT + REGIME
# =========================================================

importance_oil_rf_ev_conf <- data.frame(
  Feature = names(rf_oil_ev_conf$variable.importance),
  Importance = rf_oil_ev_conf$variable.importance
) %>%
  arrange(desc(Importance))

importance_gold_rf_ev_conf <- data.frame(
  Feature = names(rf_gold_ev_conf$variable.importance),
  Importance = rf_gold_ev_conf$variable.importance
) %>%
  arrange(desc(Importance))

print(importance_oil_rf_ev_conf)
print(importance_gold_rf_ev_conf)

write_xlsx(importance_oil_rf_ev_conf, "importance_oil_rf_ev_conf.xlsx")
write_xlsx(importance_gold_rf_ev_conf, "importance_gold_rf_ev_conf.xlsx")

# =========================================================
# 30.4 ADVANCED RETURN RESULTS - TAMBAH RF
# =========================================================

advanced_return_results <- bind_rows(
  data.frame(Asset = "Oil",  Model = "LM EV + Conflict + Regime",  oil_lm_ev_conf_metrics),
  data.frame(Asset = "Oil",  Model = "XGB EV + Conflict + Regime", res_oil_xgb_ev_conf$metrics),
  data.frame(Asset = "Oil",  Model = "RF EV + Conflict + Regime",  oil_rf_ev_conf_metrics),
  
  data.frame(Asset = "Gold", Model = "LM EV + Conflict + Regime",  gold_lm_ev_conf_metrics),
  data.frame(Asset = "Gold", Model = "XGB EV + Conflict + Regime", res_gold_xgb_ev_conf$metrics),
  data.frame(Asset = "Gold", Model = "RF EV + Conflict + Regime",  gold_rf_ev_conf_metrics)
)

print(advanced_return_results)
write_xlsx(advanced_return_results, "advanced_return_results_ev_conflict.xlsx")

# =========================================================
# 30.5 ADVANCED PRICE RESULTS - TAMBAH RF
# =========================================================

advanced_price_results <- bind_rows(
  data.frame(Asset = "Oil",  Model = "RF EV + Conflict + Regime -> Price",  oil_price_rf_ev_conf_metrics),
  data.frame(Asset = "Gold", Model = "RF EV + Conflict + Regime -> Price", gold_price_rf_ev_conf_metrics)
)

print(advanced_price_results)
write_xlsx(advanced_price_results, "advanced_price_results_ev_conflict.xlsx")


# =========================================================
# COMPARISON TABLE: RETURN
# WITHOUT EV / WITH EV / WITH EV + CONFLICT / RF
# =========================================================
comparison_ev_conf_return_all <- bind_rows(
  # ---------------- OIL ----------------
  data.frame(Asset = "Oil",  Model = "Linear Return", oil_lm_cross_metrics),
  data.frame(Asset = "Oil",  Model = "Linear Return + EV", oil_lm_ev_metrics),
  data.frame(Asset = "Oil",  Model = "Linear Return + EV + Conflict", oil_lm_ev_conf_metrics),
  data.frame(Asset = "Oil",  Model = "Linear Return + EV + Conflict + Regime", oil_lm_ev_conf_reg_metrics),
  
  data.frame(Asset = "Oil",  Model = "XGBoost Return", oil_xgb_ret_metrics),
  data.frame(Asset = "Oil",  Model = "XGBoost Return + EV", oil_xgb_ev_metrics),
  data.frame(Asset = "Oil",  Model = "XGBoost Return + EV + Conflict", oil_xgb_ev_conf_metrics),
  data.frame(Asset = "Oil",  Model = "XGBoost Return + EV + Conflict + Regime", oil_xgb_ev_conf_reg_metrics),
  
  data.frame(Asset = "Oil",  Model = "Random Forest Return", oil_rf_metrics),
  data.frame(Asset = "Oil",  Model = "Random Forest Return + EV + Conflict + Regime", oil_rf_ev_conf_metrics),
  
  # ---------------- GOLD ----------------
  data.frame(Asset = "Gold", Model = "Linear Return", gold_lm_cross_metrics),
  data.frame(Asset = "Gold", Model = "Linear Return + EV", gold_lm_ev_metrics),
  data.frame(Asset = "Gold", Model = "Linear Return + EV + Conflict", gold_lm_ev_conf_metrics),
  data.frame(Asset = "Gold", Model = "Linear Return + EV + Conflict + Regime", gold_lm_ev_conf_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "XGBoost Return", gold_xgb_ret_metrics),
  data.frame(Asset = "Gold", Model = "XGBoost Return + EV", gold_xgb_ev_metrics),
  data.frame(Asset = "Gold", Model = "XGBoost Return + EV + Conflict", gold_xgb_ev_conf_metrics),
  data.frame(Asset = "Gold", Model = "XGBoost Return + EV + Conflict + Regime", gold_xgb_ev_conf_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "Random Forest Return", gold_rf_metrics),
  data.frame(Asset = "Gold", Model = "Random Forest Return + EV + Conflict + Regime", gold_rf_ev_conf_metrics)
)

print(comparison_ev_conf_return_all)
write_xlsx(
  comparison_ev_conf_return_all,
  "comparison_without_vs_with_ev_conflict_return_all_models.xlsx"
)

# =========================================================
# EV-ONLY DATASET (tanpa conflict, tanpa regime)
# =========================================================

# pastikan kolom EV yang dipakai konsisten:
# kalau di data kamu namanya log_ev_sales, pakai itu
# kalau namanya log_ev, ganti di bawah

train_oil_ev <- train_oil_ev_conf %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  ) %>%
  na.omit()

test_oil_ev <- test_oil_ev_conf %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  ) %>%
  na.omit()

train_gold_ev <- train_gold_ev_conf %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  ) %>%
  na.omit()

test_gold_ev <- test_gold_ev_conf %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  ) %>%
  na.omit()

cat("Train oil EV dim:", dim(train_oil_ev), "\n")
cat("Test oil EV dim :", dim(test_oil_ev), "\n")
cat("Train gold EV dim:", dim(train_gold_ev), "\n")
cat("Test gold EV dim :", dim(test_gold_ev), "\n")

# =========================================================
# LINEAR + EV
# =========================================================

# ---------- OIL ----------
model_oil_lm_ev <- lm(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales,
  data = train_oil_ev
)

pred_oil_lm_ev <- predict(model_oil_lm_ev, newdata = test_oil_ev)

oil_lm_ev_metrics <- eval_metrics_return(
  actual = test_oil_ev$target_oil_return,
  pred   = pred_oil_lm_ev
)

# ---------- GOLD ----------
model_gold_lm_ev <- lm(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales,
  data = train_gold_ev
)

pred_gold_lm_ev <- predict(model_gold_lm_ev, newdata = test_gold_ev)

gold_lm_ev_metrics <- eval_metrics_return(
  actual = test_gold_ev$target_gold_return,
  pred   = pred_gold_lm_ev
)

print(oil_lm_ev_metrics)
print(gold_lm_ev_metrics)

# =========================================================
# XGBOOST + EV
# =========================================================

# ---------- OIL ----------
x_train_oil_ev <- train_oil_ev %>%
  select(
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  )

x_test_oil_ev <- test_oil_ev %>%
  select(
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  )

res_oil_xgb_ev <- run_xgb(
  x_train_oil_ev, x_test_oil_ev,
  train_oil_ev$target_oil_return,
  test_oil_ev$target_oil_return
)

model_oil_xgb_ev <- res_oil_xgb_ev$model
pred_oil_xgb_ev <- res_oil_xgb_ev$pred
oil_xgb_ev_metrics <- res_oil_xgb_ev$metrics

# ---------- GOLD ----------
x_train_gold_ev <- train_gold_ev %>%
  select(
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  )

x_test_gold_ev <- test_gold_ev %>%
  select(
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales
  )

res_gold_xgb_ev <- run_xgb(
  x_train_gold_ev, x_test_gold_ev,
  train_gold_ev$target_gold_return,
  test_gold_ev$target_gold_return
)

model_gold_xgb_ev <- res_gold_xgb_ev$model
pred_gold_xgb_ev <- res_gold_xgb_ev$pred
gold_xgb_ev_metrics <- res_gold_xgb_ev$metrics

print(oil_xgb_ev_metrics)
print(gold_xgb_ev_metrics)


# =========================================================
# RANDOM FOREST + EV
# =========================================================

# ---------- OIL ----------
rf_oil_ev <- ranger(
  target_oil_return ~ .,
  data = train_oil_ev %>% select(-date, -oil_price, -next_oil_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_oil_rf_ev <- predict(rf_oil_ev, data = test_oil_ev)$predictions

oil_rf_ev_metrics <- eval_metrics_return(
  actual = test_oil_ev$target_oil_return,
  pred   = pred_oil_rf_ev
)

# ---------- GOLD ----------
rf_gold_ev <- ranger(
  target_gold_return ~ .,
  data = train_gold_ev %>% select(-date, -gold_price, -next_gold_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_gold_rf_ev <- predict(rf_gold_ev, data = test_gold_ev)$predictions

gold_rf_ev_metrics <- eval_metrics_return(
  actual = test_gold_ev$target_gold_return,
  pred   = pred_gold_rf_ev
)

print(oil_rf_ev_metrics)
print(gold_rf_ev_metrics)

# =========================================================
# EV + REGIME DATASET
# =========================================================
train_oil_ev_reg <- train_oil_ev_conf %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales,
    vol_regime, gpr_regime, vix_regime
  ) %>%
  na.omit()

test_oil_ev_reg <- test_oil_ev_conf %>%
  select(
    date, oil_price, next_oil_price, target_oil_return,
    oil_return, gold_return, sp_return,
    oil_rv30, gold_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales,
    vol_regime, gpr_regime, vix_regime
  ) %>%
  na.omit()

train_gold_ev_reg <- train_gold_ev_conf %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales,
    vol_regime, gpr_regime, vix_regime
  ) %>%
  na.omit()

test_gold_ev_reg <- test_gold_ev_conf %>%
  select(
    date, gold_price, next_gold_price, target_gold_return,
    gold_return, oil_return, sp_return,
    gold_rv30, oil_rv30, sp_rv30, btc_rv30, vix, gpr,
    log_ev_sales,
    vol_regime, gpr_regime, vix_regime
  ) %>%
  na.omit()

# =========================================================
# LINEAR + EV + REGIME
# =========================================================
model_oil_lm_ev_reg <- lm(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales +
    vol_regime + gpr_regime + vix_regime,
  data = train_oil_ev_reg
)

pred_oil_lm_ev_reg <- predict(model_oil_lm_ev_reg, newdata = test_oil_ev_reg)
oil_lm_ev_reg_metrics <- eval_metrics_return(test_oil_ev_reg$target_oil_return, pred_oil_lm_ev_reg)

model_gold_lm_ev_reg <- lm(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales +
    vol_regime + gpr_regime + vix_regime,
  data = train_gold_ev_reg
)

pred_gold_lm_ev_reg <- predict(model_gold_lm_ev_reg, newdata = test_gold_ev_reg)
gold_lm_ev_reg_metrics <- eval_metrics_return(test_gold_ev_reg$target_gold_return, pred_gold_lm_ev_reg)

# =========================================================
# XGBOOST + EV + REGIME
# =========================================================
x_train_oil_ev_reg <- model.matrix(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales +
    vol_regime + gpr_regime + vix_regime,
  data = train_oil_ev_reg
)[, -1]

x_test_oil_ev_reg <- model.matrix(
  target_oil_return ~
    oil_return + gold_return + sp_return +
    oil_rv30 + gold_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales +
    vol_regime + gpr_regime + vix_regime,
  data = test_oil_ev_reg
)[, -1]

res_oil_xgb_ev_reg <- run_xgb(
  x_train_oil_ev_reg, x_test_oil_ev_reg,
  train_oil_ev_reg$target_oil_return,
  test_oil_ev_reg$target_oil_return
)

pred_oil_xgb_ev_reg <- res_oil_xgb_ev_reg$pred
oil_xgb_ev_reg_metrics <- res_oil_xgb_ev_reg$metrics

x_train_gold_ev_reg <- model.matrix(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales +
    vol_regime + gpr_regime + vix_regime,
  data = train_gold_ev_reg
)[, -1]

x_test_gold_ev_reg <- model.matrix(
  target_gold_return ~
    gold_return + oil_return + sp_return +
    gold_rv30 + oil_rv30 + sp_rv30 + btc_rv30 + vix + gpr +
    log_ev_sales +
    vol_regime + gpr_regime + vix_regime,
  data = test_gold_ev_reg
)[, -1]

res_gold_xgb_ev_reg <- run_xgb(
  x_train_gold_ev_reg, x_test_gold_ev_reg,
  train_gold_ev_reg$target_gold_return,
  test_gold_ev_reg$target_gold_return
)

pred_gold_xgb_ev_reg <- res_gold_xgb_ev_reg$pred
gold_xgb_ev_reg_metrics <- res_gold_xgb_ev_reg$metrics

# =========================================================
# RANDOM FOREST + EV + REGIME
# =========================================================
rf_oil_ev_reg <- ranger(
  target_oil_return ~ .,
  data = train_oil_ev_reg %>% select(-date, -oil_price, -next_oil_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_oil_rf_ev_reg <- predict(rf_oil_ev_reg, data = test_oil_ev_reg)$predictions
oil_rf_ev_reg_metrics <- eval_metrics_return(test_oil_ev_reg$target_oil_return, pred_oil_rf_ev_reg)

rf_gold_ev_reg <- ranger(
  target_gold_return ~ .,
  data = train_gold_ev_reg %>% select(-date, -gold_price, -next_gold_price),
  num.trees = 500,
  importance = "impurity",
  seed = 123
)

pred_gold_rf_ev_reg <- predict(rf_gold_ev_reg, data = test_gold_ev_reg)$predictions
gold_rf_ev_reg_metrics <- eval_metrics_return(test_gold_ev_reg$target_gold_return, pred_gold_rf_ev_reg)



# =========================================================
# PRICE METRICS - EV + REGIME
# =========================================================
oil_price_lm_ev_reg_metrics <- eval_price_metrics(
  actual = test_oil_ev_reg$next_oil_price,
  pred   = test_oil_ev_reg$oil_price * exp(pred_oil_lm_ev_reg)
)

oil_price_xgb_ev_reg_metrics <- eval_price_metrics(
  actual = test_oil_ev_reg$next_oil_price,
  pred   = test_oil_ev_reg$oil_price * exp(pred_oil_xgb_ev_reg)
)

oil_price_rf_ev_reg_metrics <- eval_price_metrics(
  actual = test_oil_ev_reg$next_oil_price,
  pred   = test_oil_ev_reg$oil_price * exp(pred_oil_rf_ev_reg)
)

gold_price_lm_ev_reg_metrics <- eval_price_metrics(
  actual = test_gold_ev_reg$next_gold_price,
  pred   = test_gold_ev_reg$gold_price * exp(pred_gold_lm_ev_reg)
)

gold_price_xgb_ev_reg_metrics <- eval_price_metrics(
  actual = test_gold_ev_reg$next_gold_price,
  pred   = test_gold_ev_reg$gold_price * exp(pred_gold_xgb_ev_reg)
)

gold_price_rf_ev_reg_metrics <- eval_price_metrics(
  actual = test_gold_ev_reg$next_gold_price,
  pred   = test_gold_ev_reg$gold_price * exp(pred_gold_rf_ev_reg)
)

comparison_final_return_ev_regime <- bind_rows(
  data.frame(Asset = "Oil",  Model = "LM", oil_lm_cross_metrics),
  data.frame(Asset = "Oil",  Model = "LM + EV", oil_lm_ev_metrics),
  data.frame(Asset = "Oil",  Model = "LM + EV + Regime", oil_lm_ev_reg_metrics),
  
  data.frame(Asset = "Oil",  Model = "XGB", oil_xgb_cross_metrics),
  data.frame(Asset = "Oil",  Model = "XGB + EV", oil_xgb_ev_metrics),
  data.frame(Asset = "Oil",  Model = "XGB + EV + Regime", oil_xgb_ev_reg_metrics),
  
  data.frame(Asset = "Oil",  Model = "RF", oil_rf_cross_metrics),
  data.frame(Asset = "Oil",  Model = "RF + EV", oil_rf_ev_metrics),
  data.frame(Asset = "Oil",  Model = "RF + EV + Regime", oil_rf_ev_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "LM", gold_lm_cross_metrics),
  data.frame(Asset = "Gold", Model = "LM + EV", gold_lm_ev_metrics),
  data.frame(Asset = "Gold", Model = "LM + EV + Regime", gold_lm_ev_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "XGB", gold_xgb_cross_metrics),
  data.frame(Asset = "Gold", Model = "XGB + EV", gold_xgb_ev_metrics),
  data.frame(Asset = "Gold", Model = "XGB + EV + Regime", gold_xgb_ev_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "RF", gold_rf_cross_metrics),
  data.frame(Asset = "Gold", Model = "RF + EV", gold_rf_ev_metrics),
  data.frame(Asset = "Gold", Model = "RF + EV + Regime", gold_rf_ev_reg_metrics)
)

print(comparison_final_return_ev_regime)
write_xlsx(comparison_final_return_ev_regime, "comparison_final_return_ev_regime.xlsx")

comparison_final_price_ev_regime <- bind_rows(
  data.frame(Asset = "Oil",  Model = "LM -> Price", oil_price_lm_cross_metrics),
  data.frame(Asset = "Oil",  Model = "LM + EV -> Price", oil_price_lm_ev_metrics),
  data.frame(Asset = "Oil",  Model = "LM + EV + Regime -> Price", oil_price_lm_ev_reg_metrics),
  
  data.frame(Asset = "Oil",  Model = "XGB -> Price", oil_price_xgb_cross_metrics),
  data.frame(Asset = "Oil",  Model = "XGB + EV -> Price", oil_price_xgb_ev_metrics),
  data.frame(Asset = "Oil",  Model = "XGB + EV + Regime -> Price", oil_price_xgb_ev_reg_metrics),
  
  data.frame(Asset = "Oil",  Model = "RF -> Price", oil_price_rf_cross_metrics),
  data.frame(Asset = "Oil",  Model = "RF + EV -> Price", oil_price_rf_ev_metrics),
  data.frame(Asset = "Oil",  Model = "RF + EV + Regime -> Price", oil_price_rf_ev_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "LM -> Price", gold_price_lm_cross_metrics),
  data.frame(Asset = "Gold", Model = "LM + EV -> Price", gold_price_lm_ev_metrics),
  data.frame(Asset = "Gold", Model = "LM + EV + Regime -> Price", gold_price_lm_ev_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "XGB -> Price", gold_price_xgb_cross_metrics),
  data.frame(Asset = "Gold", Model = "XGB + EV -> Price", gold_price_xgb_ev_metrics),
  data.frame(Asset = "Gold", Model = "XGB + EV + Regime -> Price", gold_price_xgb_ev_reg_metrics),
  
  data.frame(Asset = "Gold", Model = "RF -> Price", gold_price_rf_cross_metrics),
  data.frame(Asset = "Gold", Model = "RF + EV -> Price", gold_price_rf_ev_metrics),
  data.frame(Asset = "Gold", Model = "RF + EV + Regime -> Price", gold_price_rf_ev_reg_metrics)
)

print(comparison_final_price_ev_regime)
write_xlsx(comparison_final_price_ev_regime, "comparison_final_price_ev_regime.xlsx")


# =========================================================
# CORRELATION MATRIX - ALL VARIABLES
# =========================================================

# pilih variabel numerik yang relevan
corr_data <- data_all %>%
  select(
    btc_return, sp_return, oil_return, gold_return,
    btc_rv30, sp_rv30, oil_rv30, gold_rv30,
    vix, gpr
  )

# hitung correlation matrix
cor_matrix <- cor(corr_data, use = "complete.obs")

# ubah ke format tabel (long format)
cor_table <- as.data.frame(as.table(cor_matrix)) %>%
  rename(
    Variable_1 = Var1,
    Variable_2 = Var2,
    Correlation = Freq
  ) %>%
  arrange(desc(abs(Correlation)))

# export
write_xlsx(cor_table, "correlation_all_variables.xlsx")

print(cor_matrix)

# =========================================================
# CORRELATION WITH TARGET VARIABLES
# =========================================================

# Oil
corr_oil <- cor(
  data_all_ev %>%
    select(target_oil_return,
           oil_return, gold_return, sp_return,
           oil_rv30, gold_rv30, sp_rv30, btc_rv30,
           vix, gpr),
  use = "complete.obs"
)

corr_oil_target <- data.frame(
  Variable = colnames(corr_oil),
  Correlation_with_Oil = corr_oil[, "target_oil_return"]
) %>%
  arrange(desc(abs(Correlation_with_Oil)))

# Gold
corr_gold <- cor(
  data_all_ev %>%
    select(target_gold_return,
           gold_return, oil_return, sp_return,
           gold_rv30, oil_rv30, sp_rv30, btc_rv30,
           vix, gpr),
  use = "complete.obs"
)

corr_gold_target <- data.frame(
  Variable = colnames(corr_gold),
  Correlation_with_Gold = corr_gold[, "target_gold_return"]
) %>%
  arrange(desc(abs(Correlation_with_Gold)))

# export
write_xlsx(corr_oil_target, "correlation_oil_target.xlsx")
write_xlsx(corr_gold_target, "correlation_gold_target.xlsx")

print(corr_oil_target)
print(corr_gold_target)

# =========================================================
# CORRELATION WITH EV
# =========================================================

corr_ev <- cor(
  data_all_ev %>%
    select(
      target_oil_return, target_gold_return,
      log_ev_sales,
      oil_return, gold_return, sp_return,
      oil_rv30, gold_rv30, sp_rv30, btc_rv30,
      vix, gpr
    ),
  use = "complete.obs"
)

corr_ev_target <- data.frame(
  Variable = colnames(corr_ev),
  Oil = corr_ev[, "target_oil_return"],
  Gold = corr_ev[, "target_gold_return"]
) %>%
  arrange(desc(abs(Oil)))

write_xlsx(corr_ev_target, "correlation_with_ev.xlsx")

print(corr_ev_target)

# =========================================================
# NORMALITY CHECK - ALL RETURN MODELS
# =========================================================

library(tseries)
library(writexl)
library(dplyr)

# ---------------------------------------------------------
# 1. FUNCTION NORMALITY TEST
# ---------------------------------------------------------
check_normality <- function(actual, pred, model_name) {
  resid <- actual - pred
  
  shapiro_p <- tryCatch(
    shapiro.test(resid)$p.value,
    error = function(e) NA
  )
  
  jb_p <- tryCatch(
    jarque.bera.test(resid)$p.value,
    error = function(e) NA
  )
  
  data.frame(
    Model = model_name,
    N = length(resid),
    Mean_Residual = mean(resid, na.rm = TRUE),
    SD_Residual = sd(resid, na.rm = TRUE),
    Shapiro_p = shapiro_p,
    JB_p = jb_p,
    Shapiro_Result = ifelse(!is.na(shapiro_p) & shapiro_p > 0.05, "Normal", "Not Normal"),
    JB_Result = ifelse(!is.na(jb_p) & jb_p > 0.05, "Normal", "Not Normal")
  )
}

# ---------------------------------------------------------
# 2. RUN FOR ALL OIL MODELS
# ---------------------------------------------------------
normality_results <- bind_rows(
  # ================= OIL =================
  check_normality(
    actual = test_oil_cross$target_oil_return,
    pred   = pred_oil_lm_cross,
    model_name = "Oil LM"
  ),
  check_normality(
    actual = test_oil_cross$target_oil_return,
    pred   = pred_oil_xgb_cross,
    model_name = "Oil XGB"
  ),
  check_normality(
    actual = test_oil_cross$target_oil_return,
    pred   = pred_rf_oil_cross,
    model_name = "Oil RF"
  ),
  
  check_normality(
    actual = test_oil_ev$target_oil_return,
    pred   = pred_oil_lm_ev,
    model_name = "Oil LM + EV"
  ),
  check_normality(
    actual = test_oil_ev$target_oil_return,
    pred   = pred_oil_xgb_ev,
    model_name = "Oil XGB + EV"
  ),
  check_normality(
    actual = test_oil_ev$target_oil_return,
    pred   = pred_oil_rf_ev,
    model_name = "Oil RF + EV"
  ),
  
  check_normality(
    actual = test_oil_ev_reg$target_oil_return,
    pred   = pred_oil_lm_ev_reg,
    model_name = "Oil LM + EV + Regime"
  ),
  check_normality(
    actual = test_oil_ev_reg$target_oil_return,
    pred   = pred_oil_xgb_ev_reg,
    model_name = "Oil XGB + EV + Regime"
  ),
  check_normality(
    actual = test_oil_ev_reg$target_oil_return,
    pred   = pred_oil_rf_ev_reg,
    model_name = "Oil RF + EV + Regime"
  ),
  
  # ================= GOLD =================
  check_normality(
    actual = test_gold_cross$target_gold_return,
    pred   = pred_gold_lm_cross,
    model_name = "Gold LM"
  ),
  check_normality(
    actual = test_gold_cross$target_gold_return,
    pred   = pred_gold_xgb_cross,
    model_name = "Gold XGB"
  ),
  check_normality(
    actual = test_gold_cross$target_gold_return,
    pred   = pred_rf_gold_cross,
    model_name = "Gold RF"
  ),
  
  check_normality(
    actual = test_gold_ev$target_gold_return,
    pred   = pred_gold_lm_ev,
    model_name = "Gold LM + EV"
  ),
  check_normality(
    actual = test_gold_ev$target_gold_return,
    pred   = pred_gold_xgb_ev,
    model_name = "Gold XGB + EV"
  ),
  check_normality(
    actual = test_gold_ev$target_gold_return,
    pred   = pred_gold_rf_ev,
    model_name = "Gold RF + EV"
  ),
  
  check_normality(
    actual = test_gold_ev_reg$target_gold_return,
    pred   = pred_gold_lm_ev_reg,
    model_name = "Gold LM + EV + Regime"
  ),
  check_normality(
    actual = test_gold_ev_reg$target_gold_return,
    pred   = pred_gold_xgb_ev_reg,
    model_name = "Gold XGB + EV + Regime"
  ),
  check_normality(
    actual = test_gold_ev_reg$target_gold_return,
    pred   = pred_gold_rf_ev_reg,
    model_name = "Gold RF + EV + Regime"
  )
)

# ---------------------------------------------------------
# 3. PRINT + EXPORT
# ---------------------------------------------------------
print(normality_results)
write_xlsx(normality_results, "normality_results_all_models.xlsx")
  
# =========================================================
# SAVE RESIDUALS - ALL MODELS
# =========================================================

resid_oil_lm         <- test_oil_cross$target_oil_return - pred_oil_lm_cross
resid_oil_xgb        <- test_oil_cross$target_oil_return - pred_oil_xgb_cross
resid_oil_rf         <- test_oil_cross$target_oil_return - pred_rf_oil_cross

resid_oil_lm_ev      <- test_oil_ev$target_oil_return - pred_oil_lm_ev
resid_oil_xgb_ev     <- test_oil_ev$target_oil_return - pred_oil_xgb_ev
resid_oil_rf_ev      <- test_oil_ev$target_oil_return - pred_oil_rf_ev

resid_oil_lm_ev_reg  <- test_oil_ev_reg$target_oil_return - pred_oil_lm_ev_reg
resid_oil_xgb_ev_reg <- test_oil_ev_reg$target_oil_return - pred_oil_xgb_ev_reg
resid_oil_rf_ev_reg  <- test_oil_ev_reg$target_oil_return - pred_oil_rf_ev_reg

resid_gold_lm         <- test_gold_cross$target_gold_return - pred_gold_lm_cross
resid_gold_xgb        <- test_gold_cross$target_gold_return - pred_gold_xgb_cross
resid_gold_rf         <- test_gold_cross$target_gold_return - pred_rf_gold_cross

resid_gold_lm_ev      <- test_gold_ev$target_gold_return - pred_gold_lm_ev
resid_gold_xgb_ev     <- test_gold_ev$target_gold_return - pred_gold_xgb_ev
resid_gold_rf_ev      <- test_gold_ev$target_gold_return - pred_gold_rf_ev

resid_gold_lm_ev_reg  <- test_gold_ev_reg$target_gold_return - pred_gold_lm_ev_reg
resid_gold_xgb_ev_reg <- test_gold_ev_reg$target_gold_return - pred_gold_xgb_ev_reg
resid_gold_rf_ev_reg  <- test_gold_ev_reg$target_gold_return - pred_gold_rf_ev_reg


# =========================================================
# PREPARE RESIDUAL LIST
# =========================================================
# =========================================================
# PREPARE RESIDUAL LIST
# =========================================================
residual_list <- list(
  "Oil LM" = list(
    resid = test_oil_cross$target_oil_return - pred_oil_lm_cross,
    date  = test_oil_cross$date
  ),
  "Oil XGB" = list(
    resid = test_oil_cross$target_oil_return - pred_oil_xgb_cross,
    date  = test_oil_cross$date
  ),
  "Oil RF" = list(
    resid = test_oil_cross$target_oil_return - pred_rf_oil_cross,
    date  = test_oil_cross$date
  ),
  
  
  "Oil LM + EV" = list(
    resid = test_oil_ev$target_oil_return - pred_oil_lm_ev,
    date  = test_oil_cross$date
  ),
  "Oil XGB + EV" = list(
    resid = test_oil_ev$target_oil_return - pred_oil_xgb_ev,
    date  = test_oil_cross$date
  ),
  "Oil RF + EV" = list(
    resid = test_oil_ev$target_oil_return - pred_oil_rf_ev,
    date  = test_oil_cross$date
  ),
  
  
  "Oil LM + EV + Reg" = list(
    resid = test_oil_ev_reg$target_oil_return - pred_oil_lm_ev_reg,
    date  = test_oil_cross$date
  ),
  "Oil XGB + EV + Reg" = list(
    resid = test_oil_ev_reg$target_oil_return - pred_oil_xgb_ev_reg,
    date  = test_oil_cross$date
  ),
  "Oil RF + EV + Reg" = list(
    resid = test_oil_ev_reg$target_oil_return - pred_oil_rf_ev_reg,
    date  = test_oil_cross$date
  ),
  
  
  "Gold LM" = list(
    resid = test_gold_cross$target_gold_return - pred_gold_lm_cross,
    date  = test_gold_cross$date
  ),
  "Gold XGB" = list(
    resid = test_gold_cross$target_gold_return - pred_gold_xgb_cross,
    date  = test_gold_cross$date
  ),
  "Gold RF" = list(
    resid = test_gold_cross$target_gold_return - pred_rf_gold_cross,
    date  = test_gold_cross$date
  ),
  
  
  "Gold LM + EV" = list(
    resid = test_gold_ev$target_gold_return - pred_gold_lm_ev,
    date  = test_gold_cross$date
  ),
  "Gold XGB + EV" = list(
    resid = test_gold_ev$target_gold_return - pred_gold_xgb_ev,
    date  = test_gold_cross$date
  ),
  "Gold RF + EV" = list(
    resid = test_gold_ev$target_gold_return - pred_gold_rf_ev,
    date  = test_gold_cross$date
  ),
  
  
  "Gold LM + EV + Regime" = list(
    resid = test_gold_ev_reg$target_gold_return - pred_gold_lm_ev_reg,
    date  = test_gold_ev_reg$date
  ),
  "Gold XGB + EV + Regime" = list(
    resid = test_gold_ev_reg$target_gold_return - pred_gold_xgb_ev_reg,
    date  = test_gold_ev_reg$date
  ),
  "Gold RF + EV + Regime" = list(
    resid = test_gold_ev_reg$target_gold_return - pred_gold_rf_ev_reg,
    date  = test_gold_ev_reg$date
  )
)

# =========================================================
# LOOP LJUNG-BOX FOR MULTIPLE MODELS
# =========================================================
run_ljung_box_table <- function(residual_list, lag_box = 10) {
  result_list <- lapply(names(residual_list), function(model_name) {
    resid <- residual_list[[model_name]]$resid
    lb <- Box.test(resid, lag = lag_box, type = "Ljung-Box")
    
    data.frame(
      Model = model_name,
      Statistic = as.numeric(lb$statistic),
      Lag = as.numeric(lb$parameter),
      P_Value = as.numeric(lb$p.value),
      Result = ifelse(lb$p.value > 0.05, "No Autocorrelation", "Autocorrelation Detected")
    )
  })
  
  bind_rows(result_list)
}

ljung_box_all <- run_ljung_box_table(residual_list, lag_box = 10)

print(ljung_box_all)
write_xlsx(ljung_box_all, "ljung_box_multiple_models.xlsx")

# =========================================================
# HYBRID RESIDUAL TIME-SERIES FOR ALL OIL MODELS
# =========================================================

library(forecast)
library(writexl)
library(dplyr)

# ---------------------------------------------------------
# 1. HELPER FUNCTIONS
# ---------------------------------------------------------
eval_metrics_return <- function(actual, pred) {
  rmse <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - pred), na.rm = TRUE)
  mean_return <- mean(actual, na.rm = TRUE)
  
  data.frame(
    RMSE = rmse,
    MAE = mae,
    MEAN_RETURN = mean_return
  )
}

fit_residual_ts_hybrid <- function(train_actual, train_pred, test_actual, test_pred,
                                   model_name, lag_box = 10) {
  train_resid <- train_actual - train_pred
  test_resid  <- test_actual - test_pred
  
  # fit ARIMA ke residual train
  resid_ts_model <- forecast::auto.arima(train_resid)
  
  # forecast residual sepanjang horizon test
  resid_fc <- forecast::forecast(resid_ts_model, h = length(test_actual))$mean
  resid_fc <- as.numeric(resid_fc)
  
  # corrected prediction
  final_pred <- test_pred + resid_fc
  final_resid <- test_actual - final_pred
  
  # metrics before vs after
  metrics_before <- eval_metrics_return(test_actual, test_pred)
  metrics_after  <- eval_metrics_return(test_actual, final_pred)
  
  # Ljung-Box before vs after
  lb_before <- Box.test(test_resid, lag = lag_box, type = "Ljung-Box")
  lb_after  <- Box.test(final_resid, lag = lag_box, type = "Ljung-Box")
  
  summary_table <- data.frame(
    Model = model_name,
    RMSE_Before = metrics_before$RMSE,
    MAE_Before = metrics_before$MAE,
    RMSE_After = metrics_after$RMSE,
    MAE_After = metrics_after$MAE,
    LB_p_Before = as.numeric(lb_before$p.value),
    LB_p_After = as.numeric(lb_after$p.value),
    Before_Result = ifelse(lb_before$p.value > 0.05, "No Autocorrelation", "Autocorrelation Detected"),
    After_Result  = ifelse(lb_after$p.value > 0.05, "No Autocorrelation", "Autocorrelation Detected")
  )
  
  list(
    model_name = model_name,
    resid_ts_model = resid_ts_model,
    train_resid = train_resid,
    test_resid = test_resid,
    resid_forecast = resid_fc,
    final_pred = final_pred,
    final_resid = final_resid,
    summary = summary_table
  )
}

plot_hybrid_diagnostics <- function(dates, before_resid, after_resid, model_name) {
  par(mfrow = c(2, 2))
  
  plot(dates, before_resid, type = "l",
       main = paste("Before Hybrid -", model_name),
       xlab = "Date", ylab = "Residual")
  abline(h = 0, col = "red", lwd = 2)
  
  acf(before_resid, main = paste("ACF Before -", model_name))
  
  plot(dates, after_resid, type = "l",
       main = paste("After Hybrid -", model_name),
       xlab = "Date", ylab = "Residual")
  abline(h = 0, col = "blue", lwd = 2)
  
  acf(after_resid, main = paste("ACF After -", model_name))
}

# ---------------------------------------------------------
# 2. TRAIN PREDICTIONS FOR ALL OIL MODELS
# ---------------------------------------------------------
# Oil LM
pred_train_oil_lm <- predict(model_oil_lm_cross, newdata = train_oil_cross)

# Oil XGB
pred_train_oil_xgb <- predict(
  model_oil_xgb_cross,
  newdata = xgb.DMatrix(as.matrix(x_train_oil_cross))
)

# Oil RF
pred_train_oil_rf <- predict(rf_oil_cross, data = train_oil_cross)$predictions

# Oil LM + EV
pred_train_oil_lm_ev <- predict(model_oil_lm_ev, newdata = train_oil_ev)

# Oil XGB + EV
pred_train_oil_xgb_ev <- predict(
  model_oil_xgb_ev,
  newdata = xgb.DMatrix(as.matrix(x_train_oil_ev))
)

# Oil RF + EV
pred_train_oil_rf_ev <- predict(rf_oil_ev, data = train_oil_ev)$predictions

# Oil LM + EV + Reg
pred_train_oil_lm_ev_reg <- predict(model_oil_lm_ev_reg, newdata = train_oil_ev_reg)

# Oil XGB + EV + Reg
pred_train_oil_xgb_ev_reg <- predict(
  res_oil_xgb_ev_reg$model,
  newdata = xgb.DMatrix(as.matrix(x_train_oil_ev_reg))
)

# Oil RF + EV + Reg
pred_train_oil_rf_ev_reg <- predict(rf_oil_ev_reg, data = train_oil_ev_reg)$predictions

# ---------------------------------------------------------
# 3. FIT HYBRID FOR ALL OIL MODELS
# ---------------------------------------------------------
hybrid_oil_lm <- fit_residual_ts_hybrid(
  train_actual = train_oil_cross$target_oil_return,
  train_pred   = pred_train_oil_lm,
  test_actual  = test_oil_cross$target_oil_return,
  test_pred    = pred_oil_lm_cross,
  model_name   = "Oil LM"
)

hybrid_oil_xgb <- fit_residual_ts_hybrid(
  train_actual = train_oil_cross$target_oil_return,
  train_pred   = pred_train_oil_xgb,
  test_actual  = test_oil_cross$target_oil_return,
  test_pred    = pred_oil_xgb_cross,
  model_name   = "Oil XGB"
)

hybrid_oil_rf <- fit_residual_ts_hybrid(
  train_actual = train_oil_cross$target_oil_return,
  train_pred   = pred_train_oil_rf,
  test_actual  = test_oil_cross$target_oil_return,
  test_pred    = pred_rf_oil_cross,
  model_name   = "Oil RF"
)

hybrid_oil_lm_ev <- fit_residual_ts_hybrid(
  train_actual = train_oil_ev$target_oil_return,
  train_pred   = pred_train_oil_lm_ev,
  test_actual  = test_oil_ev$target_oil_return,
  test_pred    = pred_oil_lm_ev,
  model_name   = "Oil LM + EV"
)

hybrid_oil_xgb_ev <- fit_residual_ts_hybrid(
  train_actual = train_oil_ev$target_oil_return,
  train_pred   = pred_train_oil_xgb_ev,
  test_actual  = test_oil_ev$target_oil_return,
  test_pred    = pred_oil_xgb_ev,
  model_name   = "Oil XGB + EV"
)

hybrid_oil_rf_ev <- fit_residual_ts_hybrid(
  train_actual = train_oil_ev$target_oil_return,
  train_pred   = pred_train_oil_rf_ev,
  test_actual  = test_oil_ev$target_oil_return,
  test_pred    = pred_oil_rf_ev,
  model_name   = "Oil RF + EV"
)

hybrid_oil_lm_ev_reg <- fit_residual_ts_hybrid(
  train_actual = train_oil_ev_reg$target_oil_return,
  train_pred   = pred_train_oil_lm_ev_reg,
  test_actual  = test_oil_ev_reg$target_oil_return,
  test_pred    = pred_oil_lm_ev_reg,
  model_name   = "Oil LM + EV + Reg"
)

hybrid_oil_xgb_ev_reg <- fit_residual_ts_hybrid(
  train_actual = train_oil_ev_reg$target_oil_return,
  train_pred   = pred_train_oil_xgb_ev_reg,
  test_actual  = test_oil_ev_reg$target_oil_return,
  test_pred    = pred_oil_xgb_ev_reg,
  model_name   = "Oil XGB + EV + Reg"
)

hybrid_oil_rf_ev_reg <- fit_residual_ts_hybrid(
  train_actual = train_oil_ev_reg$target_oil_return,
  train_pred   = pred_train_oil_rf_ev_reg,
  test_actual  = test_oil_ev_reg$target_oil_return,
  test_pred    = pred_oil_rf_ev_reg,
  model_name   = "Oil RF + EV + Reg"
)

# ---------------------------------------------------------
# 4. SUMMARY TABLE
# ---------------------------------------------------------
oil_hybrid_summary <- bind_rows(
  hybrid_oil_lm$summary,
  hybrid_oil_xgb$summary,
  hybrid_oil_rf$summary,
  hybrid_oil_lm_ev$summary,
  hybrid_oil_xgb_ev$summary,
  hybrid_oil_rf_ev$summary,
  hybrid_oil_lm_ev_reg$summary,
  hybrid_oil_xgb_ev_reg$summary,
  hybrid_oil_rf_ev_reg$summary
)

print(oil_hybrid_summary)
write_xlsx(oil_hybrid_summary, "oil_hybrid_residual_summary.xlsx")

# ---------------------------------------------------------
# 5. OPTIONAL: SAVE CORRECTED PREDICTIONS
# ---------------------------------------------------------
oil_hybrid_predictions <- list(
  oil_lm = data.frame(
    date = test_oil_cross$date,
    actual = test_oil_cross$target_oil_return,
    pred_before = pred_oil_lm_cross,
    pred_after = hybrid_oil_lm$final_pred,
    resid_before = hybrid_oil_lm$test_resid,
    resid_after = hybrid_oil_lm$final_resid
  ),
  oil_xgb = data.frame(
    date = test_oil_cross$date,
    actual = test_oil_cross$target_oil_return,
    pred_before = pred_oil_xgb_cross,
    pred_after = hybrid_oil_xgb$final_pred,
    resid_before = hybrid_oil_xgb$test_resid,
    resid_after = hybrid_oil_xgb$final_resid
  ),
  oil_rf = data.frame(
    date = test_oil_cross$date,
    actual = test_oil_cross$target_oil_return,
    pred_before = pred_rf_oil_cross,
    pred_after = hybrid_oil_rf$final_pred,
    resid_before = hybrid_oil_rf$test_resid,
    resid_after = hybrid_oil_rf$final_resid
  ),
  oil_lm_ev = data.frame(
    date = test_oil_ev$date,
    actual = test_oil_ev$target_oil_return,
    pred_before = pred_oil_lm_ev,
    pred_after = hybrid_oil_lm_ev$final_pred,
    resid_before = hybrid_oil_lm_ev$test_resid,
    resid_after = hybrid_oil_lm_ev$final_resid
  ),
  oil_xgb_ev = data.frame(
    date = test_oil_ev$date,
    actual = test_oil_ev$target_oil_return,
    pred_before = pred_oil_xgb_ev,
    pred_after = hybrid_oil_xgb_ev$final_pred,
    resid_before = hybrid_oil_xgb_ev$test_resid,
    resid_after = hybrid_oil_xgb_ev$final_resid
  ),
  oil_rf_ev = data.frame(
    date = test_oil_ev$date,
    actual = test_oil_ev$target_oil_return,
    pred_before = pred_oil_rf_ev,
    pred_after = hybrid_oil_rf_ev$final_pred,
    resid_before = hybrid_oil_rf_ev$test_resid,
    resid_after = hybrid_oil_rf_ev$final_resid
  ),
  oil_lm_ev_reg = data.frame(
    date = test_oil_ev_reg$date,
    actual = test_oil_ev_reg$target_oil_return,
    pred_before = pred_oil_lm_ev_reg,
    pred_after = hybrid_oil_lm_ev_reg$final_pred,
    resid_before = hybrid_oil_lm_ev_reg$test_resid,
    resid_after = hybrid_oil_lm_ev_reg$final_resid
  ),
  oil_xgb_ev_reg = data.frame(
    date = test_oil_ev_reg$date,
    actual = test_oil_ev_reg$target_oil_return,
    pred_before = pred_oil_xgb_ev_reg,
    pred_after = hybrid_oil_xgb_ev_reg$final_pred,
    resid_before = hybrid_oil_xgb_ev_reg$test_resid,
    resid_after = hybrid_oil_xgb_ev_reg$final_resid
  ),
  oil_rf_ev_reg = data.frame(
    date = test_oil_ev_reg$date,
    actual = test_oil_ev_reg$target_oil_return,
    pred_before = pred_oil_rf_ev_reg,
    pred_after = hybrid_oil_rf_ev_reg$final_pred,
    resid_before = hybrid_oil_rf_ev_reg$test_resid,
    resid_after = hybrid_oil_rf_ev_reg$final_resid
  )
)

write_xlsx(oil_hybrid_predictions, "oil_hybrid_predictions.xlsx")

# ---------------------------------------------------------
# 6. OPTIONAL: PLOT DIAGNOSTICS FOR SELECTED MODELS
# ---------------------------------------------------------
plot_hybrid_diagnostics(
  dates = test_oil_cross$date,
  before_resid = hybrid_oil_xgb$test_resid,
  after_resid = hybrid_oil_xgb$final_resid,
  model_name = "Oil XGB"
)

plot_hybrid_diagnostics(
  dates = test_oil_ev_reg$date,
  before_resid = hybrid_oil_xgb_ev_reg$test_resid,
  after_resid = hybrid_oil_xgb_ev_reg$final_resid,
  model_name = "Oil XGB + EV + Reg"
)



# -----------------------------------------------------------
# COBA GARCH
# -------------------------------------------------------------

install.packages("rugarch")
library(rugarch)

resid_oil_xgb <- test_oil_cross$target_oil_return - pred_oil_xgb_cross

spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1)
  ),
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = FALSE
  ),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = spec, data = resid_oil_xgb)
show(garch_fit)

std_resid <- residuals(garch_fit, standardize = TRUE)
Box.test(std_resid, lag = 10, type = "Ljung-Box")
acf(std_resid, main = "ACF Standardized Residual - Oil XGB GARCH")

run_garch_test <- function(resid, model_name) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
    distribution.model = "norm"
  )
  
  fit <- ugarchfit(spec = spec, data = resid)
  std_resid <- residuals(fit, standardize = TRUE)
  
  lb <- Box.test(std_resid, lag = 10, type = "Ljung-Box")
  
  cat("\n========================\n")
  cat("Model:", model_name, "\n")
  print(lb)
  
  return(data.frame(
    Model = model_name,
    LB_p_value = lb$p.value,
    Result = ifelse(lb$p.value > 0.05, "No Autocorrelation", "Autocorrelation Detected")
  ))
}

garch_results <- bind_rows(
  run_garch_test(resid_oil_lm, "Oil LM"),
  run_garch_test(resid_oil_xgb, "Oil XGB"),
  run_garch_test(resid_oil_rf, "Oil RF"),
  run_garch_test(resid_oil_lm_ev, "Oil LM + EV"),
  run_garch_test(resid_oil_xgb_ev, "Oil XGB + EV"),
  run_garch_test(resid_oil_rf_ev, "Oil RF + EV"),
  run_garch_test(resid_oil_lm_ev_reg, "Oil LM + EV + Reg"),
  run_garch_test(resid_oil_xgb_ev_reg, "Oil XGB + EV + Reg"),
  run_garch_test(resid_oil_rf_ev_reg, "Oil RF + EV + Reg")
)

print(garch_results)

print(importance_oil_xgb_cross[1:10,])
print(importance_gold_xgb_cross[1:10,])

print(granger_sp_to_oil)
print(granger_oil_to_sp)
print(granger_oil_to_gold)
print(granger_gold_to_oil)

# =========================================================
# FIGURE 3. XGBOOST FEATURE IMPORTANCE
# =========================================================
library(dplyr)
library(ggplot2)
library(tidytext)
install.packages("tidytext")

top_oil_imp <- importance_oil_xgb_cross %>%
  select(Feature, Gain) %>%
  slice_max(order_by = Gain, n = 10) %>%
  mutate(Asset = "Oil")

top_gold_imp <- importance_gold_xgb_cross %>%
  select(Feature, Gain) %>%
  slice_max(order_by = Gain, n = 10) %>%
  mutate(Asset = "Gold")

fig_imp <- bind_rows(top_oil_imp, top_gold_imp) %>%
  mutate(Feature = reorder_within(Feature, Gain, Asset))

p_feature_imp <- ggplot(fig_imp, aes(x = Feature, y = Gain)) +
  geom_col() +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ Asset, scales = "free_y") +
  labs(
    title = "Top XGBoost Feature Importance by Information Gain",
    x = "Feature",
    y = "Gain"
  ) +
  theme_minimal(base_size = 11)

ggsave("figure_feature_importance_xgb_fixed.png", p_feature_imp,
       width = 9, height = 5.5, dpi = 300)

print(p_feature_imp)


# =========================================================
# 21. REGIME-SPECIFIC EVALUATION (XGBOOST ONLY)
# =========================================================

library(dplyr)
library(tidyr)
library(writexl)

# satukan actual, regime, dan prediksi model XGB
test_eval <- test_data %>%
  mutate(
    pred_oil_xgb_cross      = pred_oil_xgb_cross,
    pred_oil_xgb_cross_reg  = pred_oil_xgb_cross_reg,
    pred_gold_xgb_cross     = pred_gold_xgb_cross,
    pred_gold_xgb_cross_reg = pred_gold_xgb_cross_reg
  )

# hasil lengkap per regime
regime_eval_results <- bind_rows(
  # Oil
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross",     "vol_regime", "Oil",  "XGB Cross"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross_reg", "vol_regime", "Oil",  "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross",     "gpr_regime", "Oil",  "XGB Cross"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross_reg", "gpr_regime", "Oil",  "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross",     "vix_regime", "Oil",  "XGB Cross"),
  calc_regime_metrics(test_eval, "target_oil_return",  "pred_oil_xgb_cross_reg", "vix_regime", "Oil",  "XGB Cross + Regime"),
  
  # Gold
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross",     "vol_regime", "Gold", "XGB Cross"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross_reg", "vol_regime", "Gold", "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross",     "gpr_regime", "Gold", "XGB Cross"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross_reg", "gpr_regime", "Gold", "XGB Cross + Regime"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross",     "vix_regime", "Gold", "XGB Cross"),
  calc_regime_metrics(test_eval, "target_gold_return", "pred_gold_xgb_cross_reg", "vix_regime", "Gold", "XGB Cross + Regime")
) %>%
  mutate(
    Regime = as.character(Regime),
    Stress_Level = case_when(
      Regime %in% c("S1", "G1", "V1") ~ "Low",
      Regime %in% c("S2", "G2", "V2") ~ "Medium",
      Regime %in% c("S3", "G3", "V3") ~ "High",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(Asset, Regime_Type, Model, Regime)

print(regime_eval_results, n = Inf)
View(regime_eval_results)

# ringkasan low vs high stress
regime_summary <- regime_eval_results %>%
  filter(Stress_Level %in% c("Low", "High")) %>%
  select(Asset, Model, Regime_Type, Stress_Level, N, RMSE, MAE) %>%
  tidyr::pivot_wider(
    names_from = Stress_Level,
    values_from = c(N, RMSE, MAE)
  ) %>%
  mutate(
    RMSE_Change_High_vs_Low = RMSE_High - RMSE_Low,
    MAE_Change_High_vs_Low  = MAE_High - MAE_Low
  ) %>%
  arrange(Asset, Regime_Type, Model)

print(regime_summary, n = Inf)

# bandingkan XGB tanpa regime vs XGB + regime di tiap regime
regime_model_compare <- regime_eval_results %>%
  select(Asset, Regime_Type, Regime, Model, RMSE, MAE) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(RMSE, MAE)
  ) %>%
  mutate(
    RMSE_Improvement = `RMSE_XGB Cross` - `RMSE_XGB Cross + Regime`,
    MAE_Improvement  = `MAE_XGB Cross`  - `MAE_XGB Cross + Regime`
  ) %>%
  arrange(Asset, Regime_Type, Regime)

print(regime_model_compare, n = Inf)
View(regime_model_compare)

# export
write_xlsx(
  list(
    regime_eval_results  = regime_eval_results,
    regime_summary       = regime_summary,
    regime_model_compare = regime_model_compare
  ),
  "regime_eval_results.xlsx"
)

table(test_eval$vol_regime, useNA = "ifany")
table(test_eval$gpr_regime, useNA = "ifany")
table(test_eval$vix_regime, useNA = "ifany")

library(ggplot2)
library(dplyr)

plot_data <- regime_summary %>%
  filter(Regime_Type == "vix_regime") %>%
  select(Asset, Model, RMSE_Low, RMSE_High) %>%
  tidyr::pivot_longer(
    cols = c(RMSE_Low, RMSE_High),
    names_to = "Stress",
    values_to = "RMSE"
  ) %>%
  mutate(
    Stress = ifelse(Stress == "RMSE_Low", "Low Stress", "High Stress")
  )

ggplot(plot_data, aes(x = Stress, y = RMSE, fill = Stress)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Asset) +
  labs(
    title = "Prediction Error Across Market Stress Regimes (VIX)",
    y = "RMSE",
    x = ""
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

plot_line <- regime_eval_results %>%
  filter(Model == "XGB Cross",
         Regime_Type %in% c("vix_regime", "gpr_regime")) %>%
  mutate(
    Regime_Type = recode(Regime_Type,
                         "vix_regime" = "VIX Regime",
                         "gpr_regime" = "GPR Regime"),
    Stress = case_when(
      Regime %in% c("V1", "G1") ~ "Low",
      Regime %in% c("V2", "G2") ~ "Medium",
      Regime %in% c("V3", "G3") ~ "High"
    ),
    Stress = factor(Stress, levels = c("Low", "Medium", "High"))
  )

ggplot(plot_line, aes(x = Stress, y = RMSE, group = Asset, color = Asset)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~Regime_Type, scales = "free_y") +
  labs(
    title = "RMSE Across Stress Levels",
    x = "Stress Level",
    y = "RMSE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  )

par(mfrow = c(1,2))

acf(resid_oil_xgb, main = "ACF Residuals (Before GARCH)")
acf(std_resid, main = "ACF Standardized Residuals (After GARCH)")

png("residuals_squared.png", width = 900, height = 400)

plot(residuals_xgb_oil^2, type = "l",
     main = "Squared Residuals (Volatility Clustering)",
     ylab = "Squared Residual",
     col = "black")

dev.off()
