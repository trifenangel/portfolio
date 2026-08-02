# =========================================================
# 0. INSTALL & LOAD PACKAGES
# =========================================================
install.packages(c("readxl", "dplyr", "janitor", "writexl", "zoo", "ggplot2", "forecast"))

library(readxl)
library(dplyr)
library(janitor)
library(writexl)
library(zoo)
library(ggplot2)
library(forecast)

# =========================================================
# 0.5 SAMPLE PERIOD
# =========================================================
start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2025-06-30")

# =========================================================
# 1. FUNCTION UNTUK CLEANING SATU FILE
# =========================================================
clean_market_data <- function(file_path) {
  
  df <- read_excel(file_path) %>%
    clean_names()
  
  # rename x1 menjadi date jika ada
  if ("x1" %in% names(df)) {
    df <- df %>% rename(date = x1)
  }
  
  # cek kolom date
  if (!"date" %in% names(df)) {
    stop(paste("Kolom 'date' tidak ditemukan di file:", file_path))
  }
  
  # ubah date ke Date
  df <- df %>%
    mutate(date = as.Date(date))
  
  # ubah semua kolom selain date ke numeric
  df <- df %>%
    mutate(across(-date, as.numeric))
  
  # hapus baris date kosong
  df <- df %>%
    filter(!is.na(date))
  
  # batasi periode penelitian
  df <- df %>%
    filter(date >= start_date & date <= end_date)
  
  # hapus duplicate date
  df <- df %>%
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

# =========================================================
# 2. LOAD & CLEAN SEMUA DATA
# =========================================================
btc  <- clean_market_data("data_bitcoin_daily_2018_2025.xlsx")
sp   <- clean_market_data("data_sp500_daily_2018_2025.xlsx")
oil  <- clean_market_data("data_oil_daily_2018_2025.xlsx")
vix  <- clean_market_data("data_vix_daily_2018_2025.xlsx")
gold <- clean_market_data("gold_daily_2018_2025.xlsx")
gpr  <- clean_market_data("data_gpr_daily_recent.xls")

# =========================================================
# 3. CEK HASIL AWAL
# =========================================================
cat("\n===== HEAD BTC =====\n")
print(head(btc))

cat("\n===== HEAD SP500 =====\n")
print(head(sp))

cat("\n===== HEAD OIL =====\n")
print(head(oil))

cat("\n===== HEAD VIX =====\n")
print(head(vix))

cat("\n===== HEAD GOLD =====\n")
print(head(gold))

cat("\n===== HEAD GPR =====\n")
print(head(gpr))

# =========================================================
# 4. SIMPAN HASIL CLEANING
# =========================================================
write_xlsx(btc,  "btc_clean_1.xlsx")
write_xlsx(sp,   "sp500_clean_1.xlsx")
write_xlsx(oil,  "oil_clean_1.xlsx")
write_xlsx(vix,  "vix_clean_1.xlsx")
write_xlsx(gold, "gold_clean_1.xlsx")
write_xlsx(gpr,  "gpr_clean_1.xlsx")

# =========================================================
# 5. HITUNG LOG RETURN
# =========================================================
btc <- btc %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted / lag(adjusted))) %>%
  filter(!is.na(log_return), is.finite(log_return))

sp <- sp %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted / lag(adjusted))) %>%
  filter(!is.na(log_return), is.finite(log_return))

oil <- oil %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted / lag(adjusted))) %>%
  filter(!is.na(log_return), is.finite(log_return))

vix <- vix %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted / lag(adjusted))) %>%
  filter(!is.na(log_return), is.finite(log_return))

gold <- gold %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted / lag(adjusted))) %>%
  filter(!is.na(log_return), is.finite(log_return))

# =========================================================
# 6. SUMMARY RETURN
# =========================================================
summary(btc$log_return)
summary(sp$log_return)
summary(oil$log_return)
summary(vix$log_return)
summary(gold$log_return)

# =========================================================
# 7. HITUNG REALIZED VARIANCE
# =========================================================
btc <- btc %>%
  arrange(date) %>%
  mutate(
    rv1  = log_return^2,
    rv7  = rollapply(log_return^2, width = 7,  FUN = sum, fill = NA, align = "right"),
    rv15 = rollapply(log_return^2, width = 15, FUN = sum, fill = NA, align = "right"),
    rv30 = rollapply(log_return^2, width = 30, FUN = sum, fill = NA, align = "right")
  )

sp <- sp %>%
  arrange(date) %>%
  mutate(
    rv1  = log_return^2,
    rv7  = rollapply(log_return^2, width = 7,  FUN = sum, fill = NA, align = "right"),
    rv15 = rollapply(log_return^2, width = 15, FUN = sum, fill = NA, align = "right"),
    rv30 = rollapply(log_return^2, width = 30, FUN = sum, fill = NA, align = "right")
  )

oil <- oil %>%
  arrange(date) %>%
  mutate(
    rv1  = log_return^2,
    rv7  = rollapply(log_return^2, width = 7,  FUN = sum, fill = NA, align = "right"),
    rv15 = rollapply(log_return^2, width = 15, FUN = sum, fill = NA, align = "right"),
    rv30 = rollapply(log_return^2, width = 30, FUN = sum, fill = NA, align = "right")
  )

gold <- gold %>%
  arrange(date) %>%
  mutate(
    rv1  = log_return^2,
    rv7  = rollapply(log_return^2, width = 7,  FUN = sum, fill = NA, align = "right"),
    rv15 = rollapply(log_return^2, width = 15, FUN = sum, fill = NA, align = "right"),
    rv30 = rollapply(log_return^2, width = 30, FUN = sum, fill = NA, align = "right")
  )

summary(btc[, c("rv1","rv7","rv15","rv30")])
summary(sp[, c("rv1","rv7","rv15","rv30")])
summary(oil[, c("rv1","rv7","rv15","rv30")])
summary(gold[, c("rv1","rv7","rv15","rv30")])

# =========================================================
# 8. MERGE DATA
# =========================================================
btc_merge <- btc %>%
  select(date,
         btc_price = adjusted,
         btc_rv1 = rv1,
         btc_rv7 = rv7,
         btc_rv15 = rv15,
         btc_rv30 = rv30)

sp_merge <- sp %>%
  select(date,
         sp_price = adjusted,
         sp_rv1 = rv1,
         sp_rv7 = rv7,
         sp_rv15 = rv15,
         sp_rv30 = rv30)

oil_merge <- oil %>%
  select(date,
         oil_price = adjusted,
         oil_rv1 = rv1,
         oil_rv7 = rv7,
         oil_rv15 = rv15,
         oil_rv30 = rv30)

vix_merge <- vix %>%
  select(date,
         vix = adjusted)

gold_merge <- gold %>%
  select(date,
         gold_price = adjusted,
         gold_rv1 = rv1,
         gold_rv7 = rv7,
         gold_rv15 = rv15,
         gold_rv30 = rv30)

# GANTI "adjusted" JIKA KOLOM GPR UTAMAMU BUKAN adjusted
gpr_merge <- gpr %>%
  select(date,
         gpr = gprd)

data_all <- btc_merge %>%
  left_join(sp_merge, by = "date") %>%
  left_join(oil_merge, by = "date") %>%
  left_join(vix_merge, by = "date") %>%
  left_join(gold_merge, by = "date") %>%
  left_join(gpr_merge, by = "date")

head(data_all)
summary(data_all)

# =========================================================
# 8.1 FILTER DATA LENGKAP
# =========================================================
data_all <- data_all %>%
  filter(!is.na(btc_price),
         !is.na(sp_price),
         !is.na(oil_price),
         !is.na(gold_price),
         !is.na(vix),
         !is.na(gpr))

summary(data_all)
dim(data_all)

data_all <- data_all %>%
  filter(!is.na(btc_rv30),
         !is.na(sp_rv30),
         !is.na(oil_rv30),
         !is.na(gold_rv30))

dim(data_all)

cor(data_all[, c("btc_rv7","btc_rv15","btc_rv30")], use = "complete.obs")

write_xlsx(data_all, "data_all_fix_1.xlsx")

colSums(is.na(data_all))
summary(data_all$gold_price)
summary(data_all$gold_rv30)

data_all %>%
  arrange(desc(gold_price)) %>%
  select(date, gold_price) %>%
  head(10)

write_xlsx(data_all, "data_all_fix_2.xlsx")

cor(data_all[, c("btc_rv7","btc_rv15","btc_rv30")], use = "complete.obs")

# =========================================================
# 9. CEK ACF
# =========================================================
par(mfrow = c(2,2))

acf(data_all$btc_rv1,  main = "ACF BTC RV1")
acf(data_all$btc_rv7,  main = "ACF BTC RV7")
acf(data_all$btc_rv15, main = "ACF BTC RV15")
acf(data_all$btc_rv30, main = "ACF BTC RV30")

btc_return <- diff(log(data_all$btc_price))
acf(btc_return, main = "ACF BTC Return")

# =========================================================
# 10. BUAT VOLATILITY REGIME DARI BTC RV30
# =========================================================
sum(is.na(data_all$btc_rv30))

data_all <- data_all %>%
  filter(!is.na(btc_rv30)) %>%
  arrange(date)

p50_btc <- quantile(data_all$btc_rv30, 0.50, na.rm = TRUE)
p90_btc <- quantile(data_all$btc_rv30, 0.90, na.rm = TRUE)

cat("50th percentile of BTC RV30 =", p50_btc, "\n")
cat("90th percentile of BTC RV30 =", p90_btc, "\n")

data_all <- data_all %>%
  mutate(
    vol_regime = case_when(
      btc_rv30 < p50_btc ~ "S1",
      btc_rv30 >= p50_btc & btc_rv30 < p90_btc ~ "S2",
      btc_rv30 >= p90_btc ~ "S3"
    ),
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3"))
  )

head(data_all[, c("date", "btc_rv30", "vol_regime")])

table(data_all$vol_regime)
prop.table(table(data_all$vol_regime)) * 100

data_all %>%
  group_by(vol_regime) %>%
  summarise(
    count = n(),
    min_rv30 = min(btc_rv30, na.rm = TRUE),
    mean_rv30 = mean(btc_rv30, na.rm = TRUE),
    median_rv30 = median(btc_rv30, na.rm = TRUE),
    max_rv30 = max(btc_rv30, na.rm = TRUE)
  )

ggplot(data_all, aes(x = date, y = btc_rv30)) +
  geom_line() +
  geom_hline(yintercept = p50_btc, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = p90_btc, linetype = "dashed", color = "red") +
  labs(
    title = "BTC RV30 with Volatility Regime Thresholds",
    x = "Date",
    y = "BTC RV30"
  ) +
  theme_minimal()

ggplot(data_all, aes(x = date, y = btc_rv30, color = vol_regime)) +
  geom_line() +
  labs(
    title = "BTC RV30 by Volatility Regime",
    x = "Date",
    y = "BTC RV30",
    color = "Volatility Regime"
  ) +
  theme_minimal()

# =========================================================
# 11. SIMPAN HASIL
# =========================================================
write_xlsx(data_all, "data_all_with_vol_regime.xlsx")


# =========================================================
# 11. BUAT GEOPOLITICAL REGIME DARI GPR
# =========================================================

# =========================================
# 1. CEK MISSING VALUE GPR
# =========================================
sum(is.na(data_all$gpr))

# kalau mau ekstra aman
data_all <- data_all %>%
  filter(!is.na(gpr)) %>%
  arrange(date)

# =========================================
# 2. HITUNG THRESHOLD PERCENTILE GPR
# =========================================
p50_gpr <- quantile(data_all$gpr, 0.50, na.rm = TRUE)
p90_gpr <- quantile(data_all$gpr, 0.90, na.rm = TRUE)

cat("50th percentile of GPR =", p50_gpr, "\n")
cat("90th percentile of GPR =", p90_gpr, "\n")

# =========================================
# 3. BUAT GPR REGIME
# =========================================
# G1 = low geopolitical risk
# G2 = medium geopolitical risk
# G3 = extreme geopolitical risk

data_all <- data_all %>%
  mutate(
    gpr_regime = case_when(
      gpr < p50_gpr ~ "G1",
      gpr >= p50_gpr & gpr < p90_gpr ~ "G2",
      gpr >= p90_gpr ~ "G3"
    )
  )

# =========================================
# 4. UBAH JADI FACTOR
# =========================================
data_all <- data_all %>%
  mutate(
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3"))
  )

# =========================================
# 5. CEK HASIL
# =========================================
head(data_all[, c("date", "gpr", "gpr_regime")])

table(data_all$gpr_regime)
prop.table(table(data_all$gpr_regime)) * 100

# =========================================
# 6. RINGKASAN STATISTIK PER REGIME
# =========================================
data_all %>%
  group_by(gpr_regime) %>%
  summarise(
    count = n(),
    min_gpr = min(gpr, na.rm = TRUE),
    mean_gpr = mean(gpr, na.rm = TRUE),
    median_gpr = median(gpr, na.rm = TRUE),
    max_gpr = max(gpr, na.rm = TRUE)
  )

# =========================================
# 7. VISUALISASI GPR + THRESHOLD
# =========================================
ggplot(data_all, aes(x = date, y = gpr)) +
  geom_line() +
  geom_hline(yintercept = p50_gpr, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = p90_gpr, linetype = "dashed", color = "red") +
  labs(
    title = "GPR with Geopolitical Regime Thresholds",
    x = "Date",
    y = "GPR"
  ) +
  theme_minimal()

# =========================================
# 8. VISUALISASI DENGAN WARNA REGIME
# =========================================
ggplot(data_all, aes(x = date, y = gpr, color = gpr_regime)) +
  geom_line() +
  labs(
    title = "GPR by Geopolitical Regime",
    x = "Date",
    y = "GPR",
    color = "Geopolitical Regime"
  ) +
  theme_minimal()

# =========================================
# 9. SIMPAN HASIL
# =========================================
write_xlsx(data_all, "data_all_with_vol_gpr_regime.xlsx")


# =========================================================
# 12. BUAT MARKET FEAR REGIME DARI VIX
# =========================================================

# =========================================
# 1. CEK MISSING VALUE VIX
# =========================================
sum(is.na(data_all$vix))

data_all <- data_all %>%
  filter(!is.na(vix)) %>%
  arrange(date)

# =========================================
# 2. HITUNG THRESHOLD PERCENTILE VIX
# =========================================
p50_vix <- quantile(data_all$vix, 0.50, na.rm = TRUE)
p90_vix <- quantile(data_all$vix, 0.90, na.rm = TRUE)

cat("50th percentile of VIX =", p50_vix, "\n")
cat("90th percentile of VIX =", p90_vix, "\n")

# =========================================
# 3. BUAT VIX REGIME
# =========================================
# V1 = low market fear
# V2 = high uncertainty
# V3 = extreme fear

data_all <- data_all %>%
  mutate(
    vix_regime = case_when(
      vix < p50_vix ~ "V1",
      vix >= p50_vix & vix < p90_vix ~ "V2",
      vix >= p90_vix ~ "V3"
    )
  )

# =========================================
# 4. UBAH JADI FACTOR
# =========================================
data_all <- data_all %>%
  mutate(
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

# =========================================
# 5. CEK HASIL
# =========================================
head(data_all[, c("date", "vix", "vix_regime")])

table(data_all$vix_regime)
prop.table(table(data_all$vix_regime)) * 100

# =========================================
# 6. RINGKASAN STATISTIK PER REGIME
# =========================================
data_all %>%
  group_by(vix_regime) %>%
  summarise(
    count = n(),
    min_vix = min(vix, na.rm = TRUE),
    mean_vix = mean(vix, na.rm = TRUE),
    median_vix = median(vix, na.rm = TRUE),
    max_vix = max(vix, na.rm = TRUE)
  )

# =========================================
# 7. VISUALISASI VIX + THRESHOLD
# =========================================
ggplot(data_all, aes(x = date, y = vix)) +
  geom_line() +
  geom_hline(yintercept = p50_vix, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = p90_vix, linetype = "dashed", color = "red") +
  labs(
    title = "VIX with Market Fear Regime Thresholds",
    x = "Date",
    y = "VIX"
  ) +
  theme_minimal()

# =========================================
# 8. VISUALISASI REGIME
# =========================================
ggplot(data_all, aes(x = date, y = vix, color = vix_regime)) +
  geom_line() +
  labs(
    title = "VIX by Market Fear Regime",
    x = "Date",
    y = "VIX",
    color = "Market Fear Regime"
  ) +
  theme_minimal()

# =========================================
# 9. SIMPAN DATA FINAL
# =========================================
write_xlsx(data_all, "data_all_with_all_regimes.xlsx")

# =========================================================
# 13. KOMBINASI REGIME
# =========================================================

# =========================================
# 1. BUAT LABEL KOMBINASI REGIME
# =========================================
data_all <- data_all %>%
  mutate(
    market_regime = paste(vol_regime, gpr_regime, vix_regime, sep = "_")
  )

# contoh: S1_G1_V1, S3_G3_V3

# =========================================
# 2. CEK DISTRIBUSI KOMBINASI
# =========================================
table(data_all$market_regime)

prop.table(table(data_all$market_regime)) * 100

# =========================================
# 3. BUAT TABEL RAPI
# =========================================
library(dplyr)

regime_table <- data_all %>%
  group_by(vol_regime, gpr_regime, vix_regime) %>%
  summarise(
    count = n()
  ) %>%
  arrange(desc(count))

print(regime_table)

# =========================================
# 4. VISUALISASI KOMBINASI REGIME
# =========================================
library(ggplot2)

ggplot(regime_table,
       aes(x = interaction(vol_regime, gpr_regime, vix_regime),
           y = count)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Combined Market Regimes",
    x = "Volatility × Geopolitical × Fear Regime",
    y = "Number of Observations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================
# 5. SIMPAN TABEL
# =========================================
write_xlsx(regime_table, "combined_regime_table.xlsx")

# =========================================================
# 14. TARGET VARIABLES (NEXT-DAY FUTURES PRICE)
# =========================================================

data_all <- data_all %>%
  arrange(date) %>%
  mutate(
    
    # Oil futures prediction target
    target_oil_price  = lead(oil_price),
    
    # Gold futures prediction target
    target_gold_price = lead(gold_price)
    
  )

# hapus baris terakhir (tidak punya target)
data_all <- data_all %>%
  filter(!is.na(target_oil_price),
         !is.na(target_gold_price))

# cek hasil
head(data_all[, c("date",
                  "oil_price","target_oil_price",
                  "gold_price","target_gold_price")])

summary(data_all$target_oil_price)
summary(data_all$target_gold_price)

# simpan dataset
write_xlsx(data_all, "data_all_with_targets.xlsx")

# =========================================================
# TRAIN TEST SPLIT (TIME SERIES SPLIT)
# =========================================================

train_data <- data_all %>%
  filter(date < as.Date("2022-01-01"))

test_data <- data_all %>%
  filter(date >= as.Date("2022-01-01"))

# cek ukuran dataset
dim(train_data)
dim(test_data)

# cek range tanggal
range(train_data$date)
range(test_data$date)

table(train_data$vol_regime)
table(test_data$vol_regime)

table(train_data$gpr_regime)
table(test_data$gpr_regime)

table(train_data$vix_regime)
table(test_data$vix_regime)

# =========================================================
# EXPLORATORY DATA ANALYSIS (EDA)
# =========================================================

library(dplyr)
library(ggplot2)

# =========================================================
# 1. PILIH VARIABEL NUMERIK UTAMA
# =========================================================
eda_vars <- data_all %>%
  select(
    btc_price, sp_price, oil_price, gold_price,
    btc_rv30, sp_rv30, oil_rv30, gold_rv30,
    vix, gpr,
    target_oil_price, target_gold_price
  )

# =========================================================
# 2. SUMMARY STATISTICS
# =========================================================
summary(eda_vars)

# tambahan: mean, sd, min, max
eda_stats <- eda_vars %>%
  summarise(across(
    everything(),
    list(
      mean = ~mean(., na.rm = TRUE),
      sd   = ~sd(., na.rm = TRUE),
      min  = ~min(., na.rm = TRUE),
      max  = ~max(., na.rm = TRUE)
    )
  ))

print(eda_stats)
write_xlsx(eda_stats, "eda_stats_table.xlsx")

# =========================================================
# 3. HISTOGRAM / DISTRIBUTION
# =========================================================

# BTC RV30
ggplot(data_all, aes(x = btc_rv30)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of BTC RV30", x = "BTC RV30", y = "Frequency") +
  theme_minimal()

# Oil RV30
ggplot(data_all, aes(x = oil_rv30)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Oil RV30", x = "Oil RV30", y = "Frequency") +
  theme_minimal()

# Gold RV30
ggplot(data_all, aes(x = gold_rv30)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Gold RV30", x = "Gold RV30", y = "Frequency") +
  theme_minimal()

# VIX
ggplot(data_all, aes(x = vix)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of VIX", x = "VIX", y = "Frequency") +
  theme_minimal()

# GPR
ggplot(data_all, aes(x = gpr)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of GPR", x = "GPR", y = "Frequency") +
  theme_minimal()

# =========================================================
# 4. TIME SERIES PLOT
# =========================================================

# BTC price
ggplot(data_all, aes(x = date, y = btc_price)) +
  geom_line() +
  labs(title = "Bitcoin Price Over Time", x = "Date", y = "BTC Price") +
  theme_minimal()

# BTC RV30
ggplot(data_all, aes(x = date, y = btc_rv30)) +
  geom_line() +
  labs(title = "BTC RV30 Over Time", x = "Date", y = "BTC RV30") +
  theme_minimal()

# Oil price
ggplot(data_all, aes(x = date, y = oil_price)) +
  geom_line() +
  labs(title = "Oil Price Over Time", x = "Date", y = "Oil Price") +
  theme_minimal()

# Gold price
ggplot(data_all, aes(x = date, y = gold_price)) +
  geom_line() +
  labs(title = "Gold Price Over Time", x = "Date", y = "Gold Price") +
  theme_minimal()

# VIX
ggplot(data_all, aes(x = date, y = vix)) +
  geom_line() +
  labs(title = "VIX Over Time", x = "Date", y = "VIX") +
  theme_minimal()

# GPR
ggplot(data_all, aes(x = date, y = gpr)) +
  geom_line() +
  labs(title = "GPR Over Time", x = "Date", y = "GPR") +
  theme_minimal()

install.packages("patchwork")
library(patchwork)
p1 <- ggplot(data_all, aes(x = date, y = btc_price)) +
  geom_line() +
  labs(title = "Bitcoin Price Over Time", x = "Date", y = "BTC Price") +
  theme_minimal()

p2 <- ggplot(data_all, aes(x = date, y = btc_rv30)) +
  geom_line() +
  labs(title = "BTC RV30 Over Time", x = "Date", y = "BTC RV30") +
  theme_minimal()

p3 <- ggplot(data_all, aes(x = date, y = oil_price)) +
  geom_line() +
  labs(title = "Oil Price Over Time", x = "Date", y = "Oil Price") +
  theme_minimal()

p4 <- ggplot(data_all, aes(x = date, y = gold_price)) +
  geom_line() +
  labs(title = "Gold Price Over Time", x = "Date", y = "Gold Price") +
  theme_minimal()

p5 <- ggplot(data_all, aes(x = date, y = vix)) +
  geom_line() +
  labs(title = "VIX Over Time", x = "Date", y = "VIX") +
  theme_minimal()

p6 <- ggplot(data_all, aes(x = date, y = gpr)) +
  geom_line() +
  labs(title = "GPR Over Time", x = "Date", y = "GPR") +
  theme_minimal()

(p1 | p2 | p3) /
  (p4 | p5 | p6)
# =========================================================
# 5. CORRELATION MATRIX
# =========================================================
corr_vars <- data_all %>%
  select(
    btc_rv30, sp_rv30, oil_rv30, gold_rv30,
    vix, gpr,
    target_oil_price, target_gold_price
  )

corr_matrix <- cor(corr_vars, use = "complete.obs")
print(corr_matrix)

# =========================================================
# 6. BOXPLOT BERDASARKAN REGIME
# =========================================================

# BTC RV30 by volatility regime
ggplot(data_all, aes(x = vol_regime, y = btc_rv30, fill = vol_regime)) +
  geom_boxplot() +
  labs(title = "BTC RV30 by Volatility Regime", x = "Volatility Regime", y = "BTC RV30") +
  theme_minimal()

# GPR by GPR regime
ggplot(data_all, aes(x = gpr_regime, y = gpr, fill = gpr_regime)) +
  geom_boxplot() +
  labs(title = "GPR by Geopolitical Regime", x = "GPR Regime", y = "GPR") +
  theme_minimal()

# VIX by VIX regime
ggplot(data_all, aes(x = vix_regime, y = vix, fill = vix_regime)) +
  geom_boxplot() +
  labs(title = "VIX by Market Fear Regime", x = "VIX Regime", y = "VIX") +
  theme_minimal()

p1 <- ggplot(data_all, aes(x = vol_regime, y = btc_rv30, fill = vol_regime)) +
  geom_boxplot() +
  labs(title = "BTC RV30 by Volatility Regime",
       x = "Volatility Regime",
       y = "BTC RV30") +
  theme_minimal()

p2 <- ggplot(data_all, aes(x = gpr_regime, y = gpr, fill = gpr_regime)) +
  geom_boxplot() +
  labs(title = "GPR by Geopolitical Regime",
       x = "GPR Regime",
       y = "GPR") +
  theme_minimal()

p3 <- ggplot(data_all, aes(x = vix_regime, y = vix, fill = vix_regime)) +
  geom_boxplot() +
  labs(title = "VIX by Market Fear Regime",
       x = "VIX Regime",
       y = "VIX") +
  theme_minimal()

library(patchwork)

p1 | p2 | p3

combined_regime_plot <- p1 | p2 | p3

ggsave("regime_boxplots.png",
       combined_regime_plot,
       width = 12,
       height = 4,
       dpi = 300)

# =========================================================
# 7. REGIME COUNTS
# =========================================================
table(data_all$vol_regime)
table(data_all$gpr_regime)
table(data_all$vix_regime)
table(data_all$market_regime)

# =========================================================
# 8. SAVE CORRELATION MATRIX
# =========================================================
corr_df <- as.data.frame(corr_matrix)
write_xlsx(corr_df, "eda_correlation_matrix.xlsx")

# Correlation check
feature_vars <- data_all %>%
  select(
    btc_rv30,
    sp_rv30,
    oil_rv30,
    gold_rv30,
    vix,
    gpr,
    oil_price,
    gold_price
  )

cor(feature_vars, use = "complete.obs")

# var check
apply(feature_vars, 2, var)

model_check <- lm(
  target_oil_price ~
    oil_price +
    oil_rv30 +
    sp_rv30 +
    btc_rv30 +
    gold_rv30 +
    vix +
    gpr,
  data = train_data
)

summary(model_check)

train_oil <- train_data %>%
  select(
    target_oil_price,
    oil_price,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

test_oil <- test_data %>%
  select(
    target_oil_price,
    oil_price,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )
train_gold <- train_data %>%
  select(
    target_gold_price,
    gold_price,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

test_gold <- test_data %>%
  select(
    target_gold_price,
    gold_price,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

#Buat model
# =========================================================
# BASELINE MODEL: LINEAR REGRESSION FOR OIL
# =========================================================

model_oil_lm <- lm(
  target_oil_price ~
    oil_price +
    oil_rv30 +
    sp_rv30 +
    btc_rv30 +
    vix +
    gpr,
  data = train_oil
)

summary(model_oil_lm)

# prediksi pada test set
pred_oil_lm <- predict(model_oil_lm, newdata = test_oil)

# =========================================================
# EVALUATION METRICS FOR OIL
# =========================================================

rmse_oil_lm <- sqrt(mean((test_oil$target_oil_price - pred_oil_lm)^2))
mae_oil_lm  <- mean(abs(test_oil$target_oil_price - pred_oil_lm))
mape_oil_lm <- mean(abs((test_oil$target_oil_price - pred_oil_lm) / test_oil$target_oil_price)) * 100

cat("Oil Linear Regression Performance:\n")
cat("RMSE =", rmse_oil_lm, "\n")
cat("MAE  =", mae_oil_lm, "\n")
cat("MAPE =", mape_oil_lm, "%\n")

# =========================================================
# BASELINE MODEL: LINEAR REGRESSION FOR GOLD
# =========================================================

model_gold_lm <- lm(
  target_gold_price ~
    gold_price +
    gold_rv30 +
    sp_rv30 +
    btc_rv30 +
    vix +
    gpr,
  data = train_gold
)

summary(model_gold_lm)

# prediksi pada test set
pred_gold_lm <- predict(model_gold_lm, newdata = test_gold)

# =========================================================
# EVALUATION METRICS FOR GOLD
# =========================================================

rmse_gold_lm <- sqrt(mean((test_gold$target_gold_price - pred_gold_lm)^2))
mae_gold_lm  <- mean(abs(test_gold$target_gold_price - pred_gold_lm))
mape_gold_lm <- mean(abs((test_gold$target_gold_price - pred_gold_lm) / test_gold$target_gold_price)) * 100

cat("Gold Linear Regression Performance:\n")
cat("RMSE =", rmse_gold_lm, "\n")
cat("MAE  =", mae_gold_lm, "\n")
cat("MAPE =", mape_gold_lm, "%\n")

# =========================================================
# SUMMARY TABLE
# =========================================================

baseline_results <- data.frame(
  Model = c("Linear Regression Oil", "Linear Regression Gold"),
  RMSE  = c(rmse_oil_lm, rmse_gold_lm),
  MAE   = c(mae_oil_lm, mae_gold_lm),
  MAPE  = c(mape_oil_lm, mape_gold_lm)
)

print(baseline_results)

# =========================================================
# BASELINE WITH REGIME DUMMIES
# =========================================================

# pastikan regime bertipe factor
train_data <- train_data %>%
  mutate(
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

test_data <- test_data %>%
  mutate(
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

# =========================================================
# DATASET OIL WITH REGIME DUMMIES
# =========================================================
train_oil_reg <- train_data %>%
  select(
    target_oil_price,
    oil_price,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

test_oil_reg <- test_data %>%
  select(
    target_oil_price,
    oil_price,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

# =========================================================
# DATASET GOLD WITH REGIME DUMMIES
# =========================================================
train_gold_reg <- train_data %>%
  select(
    target_gold_price,
    gold_price,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

test_gold_reg <- test_data %>%
  select(
    target_gold_price,
    gold_price,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

# =========================================================
# LINEAR REGRESSION OIL + REGIME DUMMIES
# =========================================================
model_oil_lm_regime <- lm(
  target_oil_price ~
    oil_price +
    oil_rv30 +
    sp_rv30 +
    btc_rv30 +
    vix +
    gpr +
    vol_regime +
    gpr_regime +
    vix_regime,
  data = train_oil_reg
)

summary(model_oil_lm_regime)

# prediksi
pred_oil_lm_regime <- predict(model_oil_lm_regime, newdata = test_oil_reg)

# evaluasi
rmse_oil_lm_regime <- sqrt(mean((test_oil_reg$target_oil_price - pred_oil_lm_regime)^2))
mae_oil_lm_regime  <- mean(abs(test_oil_reg$target_oil_price - pred_oil_lm_regime))
mape_oil_lm_regime <- mean(abs((test_oil_reg$target_oil_price - pred_oil_lm_regime) / test_oil_reg$target_oil_price)) * 100

cat("Oil Linear Regression + Regime Dummies Performance:\n")
cat("RMSE =", rmse_oil_lm_regime, "\n")
cat("MAE  =", mae_oil_lm_regime, "\n")
cat("MAPE =", mape_oil_lm_regime, "%\n")

# =========================================================
# LINEAR REGRESSION GOLD + REGIME DUMMIES
# =========================================================
model_gold_lm_regime <- lm(
  target_gold_price ~
    gold_price +
    gold_rv30 +
    sp_rv30 +
    btc_rv30 +
    vix +
    gpr +
    vol_regime +
    gpr_regime +
    vix_regime,
  data = train_gold_reg
)

summary(model_gold_lm_regime)

# prediksi
pred_gold_lm_regime <- predict(model_gold_lm_regime, newdata = test_gold_reg)

# evaluasi
rmse_gold_lm_regime <- sqrt(mean((test_gold_reg$target_gold_price - pred_gold_lm_regime)^2))
mae_gold_lm_regime  <- mean(abs(test_gold_reg$target_gold_price - pred_gold_lm_regime))
mape_gold_lm_regime <- mean(abs((test_gold_reg$target_gold_price - pred_gold_lm_regime) / test_gold_reg$target_gold_price)) * 100

cat("Gold Linear Regression + Regime Dummies Performance:\n")
cat("RMSE =", rmse_gold_lm_regime, "\n")
cat("MAE  =", mae_gold_lm_regime, "\n")
cat("MAPE =", mape_gold_lm_regime, "%\n")

# =========================================================
# COMPARISON TABLE: BASELINE VS BASELINE + REGIME
# =========================================================
baseline_comparison <- data.frame(
  Model = c(
    "Linear Regression Oil",
    "Linear Regression Oil + Regime",
    "Linear Regression Gold",
    "Linear Regression Gold + Regime"
  ),
  RMSE = c(
    rmse_oil_lm,
    rmse_oil_lm_regime,
    rmse_gold_lm,
    rmse_gold_lm_regime
  ),
  MAE = c(
    mae_oil_lm,
    mae_oil_lm_regime,
    mae_gold_lm,
    mae_gold_lm_regime
  ),
  MAPE = c(
    mape_oil_lm,
    mape_oil_lm_regime,
    mape_gold_lm,
    mape_gold_lm_regime
  )
)

print(baseline_comparison)
write_xlsx(baseline_comparison, "baseline_vs_regime_comparison.xlsx")

table(train_data$vol_regime)
table(train_data$gpr_regime)
table(train_data$vix_regime)

# =========================================================
# XGBOOST BASELINE (NO REGIME) FOR OIL AND GOLD
# =========================================================

# 1. INSTALL & LOAD PACKAGE
install.packages("xgboost")
library(xgboost)

# =========================================================
# 2. PREPARE MATRIX DATA
# =========================================================

# -------- OIL --------
x_train_oil <- as.matrix(
  train_oil %>%
    select(oil_price, oil_rv30, sp_rv30, btc_rv30, vix, gpr)
)

y_train_oil <- train_oil$target_oil_price

x_test_oil <- as.matrix(
  test_oil %>%
    select(oil_price, oil_rv30, sp_rv30, btc_rv30, vix, gpr)
)

y_test_oil <- test_oil$target_oil_price

# -------- GOLD --------
x_train_gold <- as.matrix(
  train_gold %>%
    select(gold_price, gold_rv30, sp_rv30, btc_rv30, vix, gpr)
)

y_train_gold <- train_gold$target_gold_price

x_test_gold <- as.matrix(
  test_gold %>%
    select(gold_price, gold_rv30, sp_rv30, btc_rv30, vix, gpr)
)

y_test_gold <- test_gold$target_gold_price

# =========================================================
# 3. CONVERT TO DMATRIX
# =========================================================
dtrain_oil  <- xgb.DMatrix(data = x_train_oil, label = y_train_oil)
dtest_oil   <- xgb.DMatrix(data = x_test_oil,  label = y_test_oil)

dtrain_gold <- xgb.DMatrix(data = x_train_gold, label = y_train_gold)
dtest_gold  <- xgb.DMatrix(data = x_test_gold,  label = y_test_gold)

# =========================================================
# 4. SET XGBOOST PARAMETERS
# =========================================================
params_xgb <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# =========================================================
# 5. TRAIN XGBOOST MODEL FOR OIL
# =========================================================
model_oil_xgb <- xgb.train(
  params = params_xgb,
  data = dtrain_oil,
  nrounds = 200,
  watchlist = list(train = dtrain_oil, test = dtest_oil),
  early_stopping_rounds = 20,
  print_every_n = 20
)

# predict oil
pred_oil_xgb <- predict(model_oil_xgb, newdata = dtest_oil)

# evaluate oil
rmse_oil_xgb <- sqrt(mean((y_test_oil - pred_oil_xgb)^2))
mae_oil_xgb  <- mean(abs(y_test_oil - pred_oil_xgb))
mape_oil_xgb <- mean(abs((y_test_oil - pred_oil_xgb) / y_test_oil)) * 100

cat("Oil XGBoost Baseline Performance:\n")
cat("RMSE =", rmse_oil_xgb, "\n")
cat("MAE  =", mae_oil_xgb, "\n")
cat("MAPE =", mape_oil_xgb, "%\n\n")

# =========================================================
# 6. TRAIN XGBOOST MODEL FOR GOLD
# =========================================================
model_gold_xgb <- xgb.train(
  params = params_xgb,
  data = dtrain_gold,
  nrounds = 200,
  watchlist = list(train = dtrain_gold, test = dtest_gold),
  early_stopping_rounds = 20,
  print_every_n = 20
)

# predict gold
pred_gold_xgb <- predict(model_gold_xgb, newdata = dtest_gold)

# evaluate gold
rmse_gold_xgb <- sqrt(mean((y_test_gold - pred_gold_xgb)^2))
mae_gold_xgb  <- mean(abs(y_test_gold - pred_gold_xgb))
mape_gold_xgb <- mean(abs((y_test_gold - pred_gold_xgb) / y_test_gold)) * 100

cat("Gold XGBoost Baseline Performance:\n")
cat("RMSE =", rmse_gold_xgb, "\n")
cat("MAE  =", mae_gold_xgb, "\n")
cat("MAPE =", mape_gold_xgb, "%\n\n")

# =========================================================
# 7. COMPARISON TABLE: LM VS XGBOOST
# NOTE:
# rmse_oil_lm, mae_oil_lm, mape_oil_lm,
# rmse_gold_lm, mae_gold_lm, mape_gold_lm
# must already exist from your previous baseline code
# =========================================================
xgb_comparison <- data.frame(
  Model = c(
    "Linear Regression Oil",
    "XGBoost Oil",
    "Linear Regression Gold",
    "XGBoost Gold"
  ),
  RMSE = c(
    rmse_oil_lm,
    rmse_oil_xgb,
    rmse_gold_lm,
    rmse_gold_xgb
  ),
  MAE = c(
    mae_oil_lm,
    mae_oil_xgb,
    mae_gold_lm,
    mae_gold_xgb
  ),
  MAPE = c(
    mape_oil_lm,
    mape_oil_xgb,
    mape_gold_lm,
    mape_gold_xgb
  )
)

print(xgb_comparison)
write_xlsx(xgb_comparison, "lm_vs_xgboost_comparison.xlsx")

# =========================================================
# 8. FEATURE IMPORTANCE
# =========================================================

# Oil feature importance
importance_oil <- xgb.importance(
  feature_names = colnames(x_train_oil),
  model = model_oil_xgb
)

print(importance_oil)
xgb.plot.importance(
  importance_oil,
  top_n = 10,
  main = "XGBoost Feature Importance - Oil"
)

# Gold feature importance
importance_gold <- xgb.importance(
  feature_names = colnames(x_train_gold),
  model = model_gold_xgb
)

print(importance_gold)
xgb.plot.importance(
  importance_gold,
  top_n = 10,
  main = "XGBoost Feature Importance - Gold"
)

# save feature importance
write_xlsx(as.data.frame(importance_oil),  "xgb_importance_oil.xlsx")
write_xlsx(as.data.frame(importance_gold), "xgb_importance_gold.xlsx")

# =========================================================
# XGBOOST FOR NEXT-DAY RETURN PREDICTION
# OIL AND GOLD
# =========================================================

# =========================================================
# 0. LOAD PACKAGE
# =========================================================
install.packages("xgboost")
library(xgboost)
library(dplyr)

# =========================================================
# 1. CREATE RETURN TARGETS
# =========================================================
data_all <- data_all %>%
  arrange(date) %>%
  mutate(
    oil_return  = log(oil_price / lag(oil_price)),
    gold_return = log(gold_price / lag(gold_price)),
    target_oil_return  = lead(oil_return),
    target_gold_return = lead(gold_return)
  ) %>%
  filter(
    !is.na(oil_return),
    !is.na(gold_return),
    !is.na(target_oil_return),
    !is.na(target_gold_return)
  )

# =========================================================
# 2. TRAIN-TEST SPLIT
# =========================================================
train_data <- data_all %>%
  filter(date < as.Date("2022-01-01"))

test_data <- data_all %>%
  filter(date >= as.Date("2022-01-01"))

cat("Train dimension:", dim(train_data), "\n")
cat("Test dimension :", dim(test_data), "\n")
cat("Train date range:", range(train_data$date), "\n")
cat("Test date range :", range(test_data$date), "\n")

# =========================================================
# 3. PREPARE DATASET FOR RETURN MODEL
# =========================================================

# -------- OIL RETURN --------
train_oil_ret <- train_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

test_oil_ret <- test_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

# -------- GOLD RETURN --------
train_gold_ret <- train_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

test_gold_ret <- test_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

# =========================================================
# 4. CONVERT TO MATRIX
# =========================================================

# Oil
x_train_oil_ret <- as.matrix(
  train_oil_ret %>%
    select(oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr)
)
y_train_oil_ret <- train_oil_ret$target_oil_return

x_test_oil_ret <- as.matrix(
  test_oil_ret %>%
    select(oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr)
)
y_test_oil_ret <- test_oil_ret$target_oil_return

# Gold
x_train_gold_ret <- as.matrix(
  train_gold_ret %>%
    select(gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr)
)
y_train_gold_ret <- train_gold_ret$target_gold_return

x_test_gold_ret <- as.matrix(
  test_gold_ret %>%
    select(gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr)
)
y_test_gold_ret <- test_gold_ret$target_gold_return

# =========================================================
# 5. CONVERT TO DMATRIX
# =========================================================
dtrain_oil_ret  <- xgb.DMatrix(data = x_train_oil_ret,  label = y_train_oil_ret)
dtest_oil_ret   <- xgb.DMatrix(data = x_test_oil_ret,   label = y_test_oil_ret)

dtrain_gold_ret <- xgb.DMatrix(data = x_train_gold_ret, label = y_train_gold_ret)
dtest_gold_ret  <- xgb.DMatrix(data = x_test_gold_ret,  label = y_test_gold_ret)

# =========================================================
# 6. XGBOOST PARAMETERS
# =========================================================
params_ret <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# =========================================================
# 7. TRAIN XGBOOST MODEL FOR OIL RETURN
# =========================================================
model_oil_xgb_ret <- xgb.train(
  params = params_ret,
  data = dtrain_oil_ret,
  nrounds = 200,
  evals = list(train = dtrain_oil_ret, test = dtest_oil_ret),
  early_stopping_rounds = 20,
  print_every_n = 20
)

# prediction
pred_oil_xgb_ret <- predict(model_oil_xgb_ret, newdata = dtest_oil_ret)

# evaluation
rmse_oil_xgb_ret <- sqrt(mean((y_test_oil_ret - pred_oil_xgb_ret)^2))
mae_oil_xgb_ret  <- mean(abs(y_test_oil_ret - pred_oil_xgb_ret))

cat("\nOil Return XGBoost Performance:\n")
cat("RMSE =", rmse_oil_xgb_ret, "\n")
cat("MAE  =", mae_oil_xgb_ret, "\n")

# =========================================================
# 8. TRAIN XGBOOST MODEL FOR GOLD RETURN
# =========================================================
model_gold_xgb_ret <- xgb.train(
  params = params_ret,
  data = dtrain_gold_ret,
  nrounds = 200,
  evals = list(train = dtrain_gold_ret, test = dtest_gold_ret),
  early_stopping_rounds = 20,
  print_every_n = 20
)

# prediction
pred_gold_xgb_ret <- predict(model_gold_xgb_ret, newdata = dtest_gold_ret)

# evaluation
rmse_gold_xgb_ret <- sqrt(mean((y_test_gold_ret - pred_gold_xgb_ret)^2))
mae_gold_xgb_ret  <- mean(abs(y_test_gold_ret - pred_gold_xgb_ret))

cat("\nGold Return XGBoost Performance:\n")
cat("RMSE =", rmse_gold_xgb_ret, "\n")
cat("MAE  =", mae_gold_xgb_ret, "\n")

# =========================================================
# 9. FEATURE IMPORTANCE
# =========================================================

# Oil importance
importance_oil_ret <- xgb.importance(
  feature_names = colnames(x_train_oil_ret),
  model = model_oil_xgb_ret
)

cat("\nOil Return Feature Importance:\n")
print(importance_oil_ret)

# Gold importance
importance_gold_ret <- xgb.importance(
  feature_names = colnames(x_train_gold_ret),
  model = model_gold_xgb_ret
)

cat("\nGold Return Feature Importance:\n")
print(importance_gold_ret)

# =========================================================
# 10. SAVE RESULTS
# =========================================================
return_xgb_results <- data.frame(
  Model = c("XGBoost Oil Return", "XGBoost Gold Return"),
  RMSE  = c(rmse_oil_xgb_ret, rmse_gold_xgb_ret),
  MAE   = c(mae_oil_xgb_ret, mae_gold_xgb_ret)
)

print(return_xgb_results)

write_xlsx(return_xgb_results, "xgboost_return_results.xlsx")
write_xlsx(as.data.frame(importance_oil_ret), "xgb_importance_oil_return.xlsx")
write_xlsx(as.data.frame(importance_gold_ret), "xgb_importance_gold_return.xlsx")

# =========================================================
# 11. OPTIONAL: PLOT IMPORTANCE
# IF ERROR "figure margins too large", run dev.off() first
# =========================================================
# dev.off()
# xgb.plot.importance(importance_oil_ret, top_n = 10, main = "XGBoost Feature Importance - Oil Return")
# dev.off()
# xgb.plot.importance(importance_gold_ret, top_n = 10, main = "XGBoost Feature Importance - Gold Return")

# =========================================================
# XGBOOST RETURN + REGIME FEATURES
# OIL AND GOLD
# =========================================================

library(dplyr)
library(xgboost)
library(writexl)

# =========================================================
# 1. PASTIKAN REGIME = FACTOR
# =========================================================
train_data <- train_data %>%
  mutate(
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

test_data <- test_data %>%
  mutate(
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

# =========================================================
# 2. DATASET: RETURN + REGIME
# =========================================================

# -------- OIL --------
train_oil_ret_reg <- train_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

test_oil_ret_reg <- test_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

# -------- GOLD --------
train_gold_ret_reg <- train_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

test_gold_ret_reg <- test_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

# =========================================================
# 3. UBAH KE DESIGN MATRIX (DUMMY VARIABLES)
# =========================================================

# -------- OIL --------
x_train_oil_ret_reg <- model.matrix(
  target_oil_return ~ .,
  data = train_oil_ret_reg
)[, -1]

x_test_oil_ret_reg <- model.matrix(
  target_oil_return ~ .,
  data = test_oil_ret_reg
)[, -1]

y_train_oil_ret_reg <- train_oil_ret_reg$target_oil_return
y_test_oil_ret_reg  <- test_oil_ret_reg$target_oil_return

# -------- GOLD --------
x_train_gold_ret_reg <- model.matrix(
  target_gold_return ~ .,
  data = train_gold_ret_reg
)[, -1]

x_test_gold_ret_reg <- model.matrix(
  target_gold_return ~ .,
  data = test_gold_ret_reg
)[, -1]

y_train_gold_ret_reg <- train_gold_ret_reg$target_gold_return
y_test_gold_ret_reg  <- test_gold_ret_reg$target_gold_return

# cek nama kolom
cat("Oil regime feature columns:\n")
print(colnames(x_train_oil_ret_reg))

cat("\nGold regime feature columns:\n")
print(colnames(x_train_gold_ret_reg))

# =========================================================
# 4. CONVERT TO DMATRIX
# =========================================================
dtrain_oil_ret_reg  <- xgb.DMatrix(data = x_train_oil_ret_reg,  label = y_train_oil_ret_reg)
dtest_oil_ret_reg   <- xgb.DMatrix(data = x_test_oil_ret_reg,   label = y_test_oil_ret_reg)

dtrain_gold_ret_reg <- xgb.DMatrix(data = x_train_gold_ret_reg, label = y_train_gold_ret_reg)
dtest_gold_ret_reg  <- xgb.DMatrix(data = x_test_gold_ret_reg,  label = y_test_gold_ret_reg)

# =========================================================
# 5. XGBOOST PARAMETERS
# =========================================================
params_ret_reg <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# =========================================================
# 6. TRAIN MODEL: OIL RETURN + REGIME
# =========================================================
model_oil_xgb_ret_reg <- xgb.train(
  params = params_ret_reg,
  data = dtrain_oil_ret_reg,
  nrounds = 200,
  evals = list(train = dtrain_oil_ret_reg, test = dtest_oil_ret_reg),
  early_stopping_rounds = 20,
  print_every_n = 20
)

pred_oil_xgb_ret_reg <- predict(model_oil_xgb_ret_reg, newdata = dtest_oil_ret_reg)

rmse_oil_xgb_ret_reg <- sqrt(mean((y_test_oil_ret_reg - pred_oil_xgb_ret_reg)^2))
mae_oil_xgb_ret_reg  <- mean(abs(y_test_oil_ret_reg - pred_oil_xgb_ret_reg))

cat("\nOil Return + Regime XGBoost Performance:\n")
cat("RMSE =", rmse_oil_xgb_ret_reg, "\n")
cat("MAE  =", mae_oil_xgb_ret_reg, "\n")

# =========================================================
# 7. TRAIN MODEL: GOLD RETURN + REGIME
# =========================================================
model_gold_xgb_ret_reg <- xgb.train(
  params = params_ret_reg,
  data = dtrain_gold_ret_reg,
  nrounds = 200,
  evals = list(train = dtrain_gold_ret_reg, test = dtest_gold_ret_reg),
  early_stopping_rounds = 20,
  print_every_n = 20
)

pred_gold_xgb_ret_reg <- predict(model_gold_xgb_ret_reg, newdata = dtest_gold_ret_reg)

rmse_gold_xgb_ret_reg <- sqrt(mean((y_test_gold_ret_reg - pred_gold_xgb_ret_reg)^2))
mae_gold_xgb_ret_reg  <- mean(abs(y_test_gold_ret_reg - pred_gold_xgb_ret_reg))

cat("\nGold Return + Regime XGBoost Performance:\n")
cat("RMSE =", rmse_gold_xgb_ret_reg, "\n")
cat("MAE  =", mae_gold_xgb_ret_reg, "\n")

# =========================================================
# 8. COMPARISON TABLE
# NOTE:
# Needs existing objects from previous run:
# rmse_oil_xgb_ret, mae_oil_xgb_ret
# rmse_gold_xgb_ret, mae_gold_xgb_ret
# =========================================================
xgb_return_comparison <- data.frame(
  Model = c(
    "XGBoost Oil Return",
    "XGBoost Oil Return + Regime",
    "XGBoost Gold Return",
    "XGBoost Gold Return + Regime"
  ),
  RMSE = c(
    rmse_oil_xgb_ret,
    rmse_oil_xgb_ret_reg,
    rmse_gold_xgb_ret,
    rmse_gold_xgb_ret_reg
  ),
  MAE = c(
    mae_oil_xgb_ret,
    mae_oil_xgb_ret_reg,
    mae_gold_xgb_ret,
    mae_gold_xgb_ret_reg
  )
)

print(xgb_return_comparison)
write_xlsx(xgb_return_comparison, "xgb_return_vs_regime_comparison.xlsx")

# =========================================================
# 9. FEATURE IMPORTANCE
# =========================================================

# Oil
importance_oil_ret_reg <- xgb.importance(
  feature_names = colnames(x_train_oil_ret_reg),
  model = model_oil_xgb_ret_reg
)

cat("\nOil Return + Regime Feature Importance:\n")
print(importance_oil_ret_reg)

# Gold
importance_gold_ret_reg <- xgb.importance(
  feature_names = colnames(x_train_gold_ret_reg),
  model = model_gold_xgb_ret_reg
)

cat("\nGold Return + Regime Feature Importance:\n")
print(importance_gold_ret_reg)

write_xlsx(as.data.frame(importance_oil_ret_reg),  "xgb_importance_oil_return_regime.xlsx")
write_xlsx(as.data.frame(importance_gold_ret_reg), "xgb_importance_gold_return_regime.xlsx")

# =========================================================
# 10. OPTIONAL PLOTS
# =========================================================
dev.off()
xgb.plot.importance(importance_oil_ret_reg, top_n = 15, main = "XGBoost Importance - Oil Return + Regime")
dev.off()
xgb.plot.importance(importance_gold_ret_reg, top_n = 15, main = "XGBoost Importance - Gold Return + Regime")

png("xgb_importance_oil_return_regime.png", width = 1200, height = 800)
xgb.plot.importance(importance_oil_ret_reg, top_n = 15, main = "XGBoost Importance - Oil Return + Regime")
dev.off()

png("xgb_importance_gold_return_regime.png", width = 1200, height = 800)
xgb.plot.importance(importance_gold_ret_reg, top_n = 15, main = "XGBoost Importance - Gold Return + Regime")
dev.off()

# =========================================================
# REGIME-SPECIFIC EVALUATION
# XGBOOST RETURN vs XGBOOST RETURN + REGIME
# =========================================================

library(dplyr)
library(writexl)

# =========================================================
# 1. SIMPAN PREDICTION KE TEST DATA
# NOTE:
# object berikut harus sudah ada:
# pred_oil_xgb_ret
# pred_oil_xgb_ret_reg
# pred_gold_xgb_ret
# pred_gold_xgb_ret_reg
# =========================================================

test_eval <- test_data %>%
  mutate(
    pred_oil_xgb_ret      = pred_oil_xgb_ret,
    pred_oil_xgb_ret_reg  = pred_oil_xgb_ret_reg,
    pred_gold_xgb_ret     = pred_gold_xgb_ret,
    pred_gold_xgb_ret_reg = pred_gold_xgb_ret_reg
  )

# =========================================================
# 2. FUNCTION UNTUK HITUNG METRICS PER REGIME
# =========================================================
calc_regime_metrics <- function(data, actual, predicted, regime_var, asset_name, model_name) {
  data %>%
    group_by(.data[[regime_var]]) %>%
    summarise(
      N    = n(),
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
# 3. OIL RETURN - VOL REGIME
# =========================================================
oil_vol_base <- calc_regime_metrics(
  data = test_eval,
  actual = "target_oil_return",
  predicted = "pred_oil_xgb_ret",
  regime_var = "vol_regime",
  asset_name = "Oil Return",
  model_name = "XGBoost Return"
)

oil_vol_reg <- calc_regime_metrics(
  data = test_eval,
  actual = "target_oil_return",
  predicted = "pred_oil_xgb_ret_reg",
  regime_var = "vol_regime",
  asset_name = "Oil Return",
  model_name = "XGBoost Return + Regime"
)

# =========================================================
# 4. OIL RETURN - GPR REGIME
# =========================================================
oil_gpr_base <- calc_regime_metrics(
  data = test_eval,
  actual = "target_oil_return",
  predicted = "pred_oil_xgb_ret",
  regime_var = "gpr_regime",
  asset_name = "Oil Return",
  model_name = "XGBoost Return"
)

oil_gpr_reg <- calc_regime_metrics(
  data = test_eval,
  actual = "target_oil_return",
  predicted = "pred_oil_xgb_ret_reg",
  regime_var = "gpr_regime",
  asset_name = "Oil Return",
  model_name = "XGBoost Return + Regime"
)

# =========================================================
# 5. OIL RETURN - VIX REGIME
# =========================================================
oil_vix_base <- calc_regime_metrics(
  data = test_eval,
  actual = "target_oil_return",
  predicted = "pred_oil_xgb_ret",
  regime_var = "vix_regime",
  asset_name = "Oil Return",
  model_name = "XGBoost Return"
)

oil_vix_reg <- calc_regime_metrics(
  data = test_eval,
  actual = "target_oil_return",
  predicted = "pred_oil_xgb_ret_reg",
  regime_var = "vix_regime",
  asset_name = "Oil Return",
  model_name = "XGBoost Return + Regime"
)

# =========================================================
# 6. GOLD RETURN - VOL REGIME
# =========================================================
gold_vol_base <- calc_regime_metrics(
  data = test_eval,
  actual = "target_gold_return",
  predicted = "pred_gold_xgb_ret",
  regime_var = "vol_regime",
  asset_name = "Gold Return",
  model_name = "XGBoost Return"
)

gold_vol_reg <- calc_regime_metrics(
  data = test_eval,
  actual = "target_gold_return",
  predicted = "pred_gold_xgb_ret_reg",
  regime_var = "vol_regime",
  asset_name = "Gold Return",
  model_name = "XGBoost Return + Regime"
)

# =========================================================
# 7. GOLD RETURN - GPR REGIME
# =========================================================
gold_gpr_base <- calc_regime_metrics(
  data = test_eval,
  actual = "target_gold_return",
  predicted = "pred_gold_xgb_ret",
  regime_var = "gpr_regime",
  asset_name = "Gold Return",
  model_name = "XGBoost Return"
)

gold_gpr_reg <- calc_regime_metrics(
  data = test_eval,
  actual = "target_gold_return",
  predicted = "pred_gold_xgb_ret_reg",
  regime_var = "gpr_regime",
  asset_name = "Gold Return",
  model_name = "XGBoost Return + Regime"
)

# =========================================================
# 8. GOLD RETURN - VIX REGIME
# =========================================================
gold_vix_base <- calc_regime_metrics(
  data = test_eval,
  actual = "target_gold_return",
  predicted = "pred_gold_xgb_ret",
  regime_var = "vix_regime",
  asset_name = "Gold Return",
  model_name = "XGBoost Return"
)

gold_vix_reg <- calc_regime_metrics(
  data = test_eval,
  actual = "target_gold_return",
  predicted = "pred_gold_xgb_ret_reg",
  regime_var = "vix_regime",
  asset_name = "Gold Return",
  model_name = "XGBoost Return + Regime"
)

# =========================================================
# 9. GABUNGKAN SEMUA HASIL
# =========================================================
regime_eval_results <- bind_rows(
  oil_vol_base, oil_vol_reg,
  oil_gpr_base, oil_gpr_reg,
  oil_vix_base, oil_vix_reg,
  gold_vol_base, gold_vol_reg,
  gold_gpr_base, gold_gpr_reg,
  gold_vix_base, gold_vix_reg
)

print(regime_eval_results)

# simpan ke excel
write_xlsx(regime_eval_results, "regime_specific_evaluation.xlsx")

# =========================================================
# 10. TABEL PERBANDINGAN LEBIH RINGKAS
# =========================================================
regime_eval_wide <- regime_eval_results %>%
  arrange(Asset, Regime_Type, Regime, Model)

print(regime_eval_wide)

write_xlsx(regime_eval_wide, "regime_specific_evaluation_sorted.xlsx")

# =========================================================
# 11. OPTIONAL: FILTER KHUSUS PER REGIME TYPE
# =========================================================

cat("\n================ VOL REGIME =================\n")
print(regime_eval_results %>% filter(Regime_Type == "vol_regime"))

cat("\n================ GPR REGIME =================\n")
print(regime_eval_results %>% filter(Regime_Type == "gpr_regime"))

cat("\n================ VIX REGIME =================\n")
print(regime_eval_results %>% filter(Regime_Type == "vix_regime"))

# =========================================================
# 1. PASTIKAN REGIME = FACTOR
# =========================================================
train_data <- train_data %>%
  mutate(
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

test_data <- test_data %>%
  mutate(
    vol_regime = factor(vol_regime, levels = c("S1", "S2", "S3")),
    gpr_regime = factor(gpr_regime, levels = c("G1", "G2", "G3")),
    vix_regime = factor(vix_regime, levels = c("V1", "V2", "V3"))
  )

# =========================================================
# 2. BASELINE RETURN DATASETS
# =========================================================

# -------- OIL RETURN BASELINE --------
train_oil_ret <- train_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

test_oil_ret <- test_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

# -------- GOLD RETURN BASELINE --------
train_gold_ret <- train_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

test_gold_ret <- test_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr
  )

# =========================================================
# 3. REGIME RETURN DATASETS
# =========================================================

# -------- OIL RETURN + REGIME --------
train_oil_ret_reg <- train_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

test_oil_ret_reg <- test_data %>%
  select(
    target_oil_return,
    oil_return,
    oil_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

# -------- GOLD RETURN + REGIME --------
train_gold_ret_reg <- train_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

test_gold_ret_reg <- test_data %>%
  select(
    target_gold_return,
    gold_return,
    gold_rv30,
    sp_rv30,
    btc_rv30,
    vix,
    gpr,
    vol_regime,
    gpr_regime,
    vix_regime
  )

# =========================================================
# 4. MATRIX: BASELINE
# =========================================================

# Oil baseline
x_train_oil_lgb <- as.matrix(train_oil_ret %>%
                               select(oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr))
y_train_oil_lgb <- train_oil_ret$target_oil_return

x_test_oil_lgb <- as.matrix(test_oil_ret %>%
                              select(oil_return, oil_rv30, sp_rv30, btc_rv30, vix, gpr))
y_test_oil_lgb <- test_oil_ret$target_oil_return

# Gold baseline
x_train_gold_lgb <- as.matrix(train_gold_ret %>%
                                select(gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr))
y_train_gold_lgb <- train_gold_ret$target_gold_return

x_test_gold_lgb <- as.matrix(test_gold_ret %>%
                               select(gold_return, gold_rv30, sp_rv30, btc_rv30, vix, gpr))
y_test_gold_lgb <- test_gold_ret$target_gold_return

# =========================================================
# 5. MATRIX: RETURN + REGIME
# =========================================================

# Oil regime design matrix
x_train_oil_lgb_reg <- model.matrix(
  target_oil_return ~ .,
  data = train_oil_ret_reg
)[, -1]
y_train_oil_lgb_reg <- train_oil_ret_reg$target_oil_return

x_test_oil_lgb_reg <- model.matrix(
  target_oil_return ~ .,
  data = test_oil_ret_reg
)[, -1]
y_test_oil_lgb_reg <- test_oil_ret_reg$target_oil_return

# Gold regime design matrix
x_train_gold_lgb_reg <- model.matrix(
  target_gold_return ~ .,
  data = train_gold_ret_reg
)[, -1]
y_train_gold_lgb_reg <- train_gold_ret_reg$target_gold_return

x_test_gold_lgb_reg <- model.matrix(
  target_gold_return ~ .,
  data = test_gold_ret_reg
)[, -1]
y_test_gold_lgb_reg <- test_gold_ret_reg$target_gold_return

# =========================================================
# 6. CREATE LIGHTGBM DATASETS
# =========================================================
library(lightgbm)
install.packages("lightgbm")
# Baseline
dtrain_oil_lgb  <- lgb.Dataset(data = x_train_oil_lgb,  label = y_train_oil_lgb)
dtrain_gold_lgb <- lgb.Dataset(data = x_train_gold_lgb, label = y_train_gold_lgb)

# Regime
dtrain_oil_lgb_reg  <- lgb.Dataset(data = x_train_oil_lgb_reg,  label = y_train_oil_lgb_reg)
dtrain_gold_lgb_reg <- lgb.Dataset(data = x_train_gold_lgb_reg, label = y_train_gold_lgb_reg)

# =========================================================
# 7. LIGHTGBM PARAMETERS
# =========================================================
params_lgb <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 15,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 1,
  verbosity = -1
)

# =========================================================
# 8. TRAIN LIGHTGBM BASELINE - OIL
# =========================================================
model_oil_lgb <- lgb.train(
  params = params_lgb,
  data = dtrain_oil_lgb,
  nrounds = 200
)

pred_oil_lgb <- predict(model_oil_lgb, x_test_oil_lgb)

rmse_oil_lgb <- sqrt(mean((y_test_oil_lgb - pred_oil_lgb)^2))
mae_oil_lgb  <- mean(abs(y_test_oil_lgb - pred_oil_lgb))

cat("\nOil Return LightGBM Performance:\n")
cat("RMSE =", rmse_oil_lgb, "\n")
cat("MAE  =", mae_oil_lgb, "\n")

# =========================================================
# 9. TRAIN LIGHTGBM BASELINE - GOLD
# =========================================================
model_gold_lgb <- lgb.train(
  params = params_lgb,
  data = dtrain_gold_lgb,
  nrounds = 200
)

pred_gold_lgb <- predict(model_gold_lgb, x_test_gold_lgb)

rmse_gold_lgb <- sqrt(mean((y_test_gold_lgb - pred_gold_lgb)^2))
mae_gold_lgb  <- mean(abs(y_test_gold_lgb - pred_gold_lgb))

cat("\nGold Return LightGBM Performance:\n")
cat("RMSE =", rmse_gold_lgb, "\n")
cat("MAE  =", mae_gold_lgb, "\n")

# =========================================================
# 10. TRAIN LIGHTGBM RETURN + REGIME - OIL
# =========================================================
model_oil_lgb_reg <- lgb.train(
  params = params_lgb,
  data = dtrain_oil_lgb_reg,
  nrounds = 200
)

pred_oil_lgb_reg <- predict(model_oil_lgb_reg, x_test_oil_lgb_reg)

rmse_oil_lgb_reg <- sqrt(mean((y_test_oil_lgb_reg - pred_oil_lgb_reg)^2))
mae_oil_lgb_reg  <- mean(abs(y_test_oil_lgb_reg - pred_oil_lgb_reg))

cat("\nOil Return + Regime LightGBM Performance:\n")
cat("RMSE =", rmse_oil_lgb_reg, "\n")
cat("MAE  =", mae_oil_lgb_reg, "\n")

# =========================================================
# 11. TRAIN LIGHTGBM RETURN + REGIME - GOLD
# =========================================================
model_gold_lgb_reg <- lgb.train(
  params = params_lgb,
  data = dtrain_gold_lgb_reg,
  nrounds = 200
)

pred_gold_lgb_reg <- predict(model_gold_lgb_reg, x_test_gold_lgb_reg)

rmse_gold_lgb_reg <- sqrt(mean((y_test_gold_lgb_reg - pred_gold_lgb_reg)^2))
mae_gold_lgb_reg  <- mean(abs(y_test_gold_lgb_reg - pred_gold_lgb_reg))

cat("\nGold Return + Regime LightGBM Performance:\n")
cat("RMSE =", rmse_gold_lgb_reg, "\n")
cat("MAE  =", mae_gold_lgb_reg, "\n")

# =========================================================
# 12. COMPARISON TABLE: XGBOOST VS LIGHTGBM
# NOTE:
# assumes these already exist:
# rmse_oil_xgb_ret, mae_oil_xgb_ret
# rmse_oil_xgb_ret_reg, mae_oil_xgb_ret_reg
# rmse_gold_xgb_ret, mae_gold_xgb_ret
# rmse_gold_xgb_ret_reg, mae_gold_xgb_ret_reg
# =========================================================
model_comparison <- data.frame(
  Model = c(
    "XGBoost Oil Return",
    "XGBoost Oil Return + Regime",
    "LightGBM Oil Return",
    "LightGBM Oil Return + Regime",
    "XGBoost Gold Return",
    "XGBoost Gold Return + Regime",
    "LightGBM Gold Return",
    "LightGBM Gold Return + Regime"
  ),
  RMSE = c(
    rmse_oil_xgb_ret,
    rmse_oil_xgb_ret_reg,
    rmse_oil_lgb,
    rmse_oil_lgb_reg,
    rmse_gold_xgb_ret,
    rmse_gold_xgb_ret_reg,
    rmse_gold_lgb,
    rmse_gold_lgb_reg
  ),
  MAE = c(
    mae_oil_xgb_ret,
    mae_oil_xgb_ret_reg,
    mae_oil_lgb,
    mae_oil_lgb_reg,
    mae_gold_xgb_ret,
    mae_gold_xgb_ret_reg,
    mae_gold_lgb,
    mae_gold_lgb_reg
  )
)

print(model_comparison)
write_xlsx(model_comparison, "xgb_lgb_return_comparison.xlsx")

# =========================================================
# 13. FEATURE IMPORTANCE
# =========================================================

# Oil baseline
importance_oil_lgb <- lgb.importance(model_oil_lgb)
cat("\nLightGBM Oil Return Importance:\n")
print(importance_oil_lgb)

# Oil regime
importance_oil_lgb_reg <- lgb.importance(model_oil_lgb_reg)
cat("\nLightGBM Oil Return + Regime Importance:\n")
print(importance_oil_lgb_reg)

# Gold baseline
importance_gold_lgb <- lgb.importance(model_gold_lgb)
cat("\nLightGBM Gold Return Importance:\n")
print(importance_gold_lgb)

# Gold regime
importance_gold_lgb_reg <- lgb.importance(model_gold_lgb_reg)
cat("\nLightGBM Gold Return + Regime Importance:\n")
print(importance_gold_lgb_reg)

write_xlsx(as.data.frame(importance_oil_lgb),      "lgb_importance_oil_return.xlsx")
write_xlsx(as.data.frame(importance_oil_lgb_reg),  "lgb_importance_oil_return_regime.xlsx")
write_xlsx(as.data.frame(importance_gold_lgb),     "lgb_importance_gold_return.xlsx")
write_xlsx(as.data.frame(importance_gold_lgb_reg), "lgb_importance_gold_return_regime.xlsx")

# =========================================================
# 14. OPTIONAL: REGIME-SPECIFIC EVALUATION FOR LIGHTGBM
# =========================================================
test_eval_lgb <- test_data %>%
  mutate(
    pred_oil_lgb = pred_oil_lgb,
    pred_oil_lgb_reg = pred_oil_lgb_reg,
    pred_gold_lgb = pred_gold_lgb,
    pred_gold_lgb_reg = pred_gold_lgb_reg
  )

calc_regime_metrics <- function(data, actual, predicted, regime_var, asset_name, model_name) {
  data %>%
    group_by(.data[[regime_var]]) %>%
    summarise(
      N    = n(),
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

lgb_regime_eval <- bind_rows(
  calc_regime_metrics(test_eval_lgb, "target_oil_return",  "pred_oil_lgb",     "vol_regime", "Oil Return",  "LightGBM Return"),
  calc_regime_metrics(test_eval_lgb, "target_oil_return",  "pred_oil_lgb_reg", "vol_regime", "Oil Return",  "LightGBM Return + Regime"),
  calc_regime_metrics(test_eval_lgb, "target_oil_return",  "pred_oil_lgb",     "gpr_regime", "Oil Return",  "LightGBM Return"),
  calc_regime_metrics(test_eval_lgb, "target_oil_return",  "pred_oil_lgb_reg", "gpr_regime", "Oil Return",  "LightGBM Return + Regime"),
  calc_regime_metrics(test_eval_lgb, "target_oil_return",  "pred_oil_lgb",     "vix_regime", "Oil Return",  "LightGBM Return"),
  calc_regime_metrics(test_eval_lgb, "target_oil_return",  "pred_oil_lgb_reg", "vix_regime", "Oil Return",  "LightGBM Return + Regime"),
  calc_regime_metrics(test_eval_lgb, "target_gold_return", "pred_gold_lgb",     "vol_regime", "Gold Return", "LightGBM Return"),
  calc_regime_metrics(test_eval_lgb, "target_gold_return", "pred_gold_lgb_reg", "vol_regime", "Gold Return", "LightGBM Return + Regime"),
  calc_regime_metrics(test_eval_lgb, "target_gold_return", "pred_gold_lgb",     "gpr_regime", "Gold Return", "LightGBM Return"),
  calc_regime_metrics(test_eval_lgb, "target_gold_return", "pred_gold_lgb_reg", "gpr_regime", "Gold Return", "LightGBM Return + Regime"),
  calc_regime_metrics(test_eval_lgb, "target_gold_return", "pred_gold_lgb",     "vix_regime", "Gold Return", "LightGBM Return"),
  calc_regime_metrics(test_eval_lgb, "target_gold_return", "pred_gold_lgb_reg", "vix_regime", "Gold Return", "LightGBM Return + Regime")
)

print(lgb_regime_eval)

# =========================================================
# LINEAR REGRESSION FOR NEXT-DAY RETURN
# OIL AND GOLD
# =========================================================

# =========================================================
# 1. DATASET SUDAH ADA:
# train_oil_ret, test_oil_ret
# train_gold_ret, test_gold_ret
# =========================================================

# =========================================================
# 2. LINEAR REGRESSION - OIL RETURN
# =========================================================
model_oil_lm_ret <- lm(
  target_oil_return ~
    oil_return +
    oil_rv30 +
    sp_rv30 +
    btc_rv30 +
    vix +
    gpr,
  data = train_oil_ret
)

summary(model_oil_lm_ret)

pred_oil_lm_ret <- predict(model_oil_lm_ret, newdata = test_oil_ret)

rmse_oil_lm_ret <- sqrt(mean((test_oil_ret$target_oil_return - pred_oil_lm_ret)^2))
mae_oil_lm_ret  <- mean(abs(test_oil_ret$target_oil_return - pred_oil_lm_ret))

cat("Oil Return Linear Regression Performance:\n")
cat("RMSE =", rmse_oil_lm_ret, "\n")
cat("MAE  =", mae_oil_lm_ret, "\n\n")

# =========================================================
# 3. LINEAR REGRESSION - GOLD RETURN
# =========================================================
model_gold_lm_ret <- lm(
  target_gold_return ~
    gold_return +
    gold_rv30 +
    sp_rv30 +
    btc_rv30 +
    vix +
    gpr,
  data = train_gold_ret
)

summary(model_gold_lm_ret)

pred_gold_lm_ret <- predict(model_gold_lm_ret, newdata = test_gold_ret)

rmse_gold_lm_ret <- sqrt(mean((test_gold_ret$target_gold_return - pred_gold_lm_ret)^2))
mae_gold_lm_ret  <- mean(abs(test_gold_ret$target_gold_return - pred_gold_lm_ret))

cat("Gold Return Linear Regression Performance:\n")
cat("RMSE =", rmse_gold_lm_ret, "\n")
cat("MAE  =", mae_gold_lm_ret, "\n\n")

# =========================================================
# 4. SUMMARY TABLE
# =========================================================
lm_return_results <- data.frame(
  Model = c("Linear Regression Oil Return", "Linear Regression Gold Return"),
  RMSE  = c(rmse_oil_lm_ret, rmse_gold_lm_ret),
  MAE   = c(mae_oil_lm_ret, mae_gold_lm_ret)
)

print(lm_return_results)
write_xlsx(lm_return_results, "linear_return_results.xlsx")

# =========================================================
# COMPARISON: LINEAR RETURN VS XGBOOST RETURN
# =========================================================
lm_vs_xgb_return <- data.frame(
  Model = c(
    "Linear Regression Oil Return",
    "XGBoost Oil Return",
    "Linear Regression Gold Return",
    "XGBoost Gold Return"
  ),
  RMSE = c(
    rmse_oil_lm_ret,
    rmse_oil_xgb_ret,
    rmse_gold_lm_ret,
    rmse_gold_xgb_ret
  ),
  MAE = c(
    mae_oil_lm_ret,
    mae_oil_xgb_ret,
    mae_gold_lm_ret,
    mae_gold_xgb_ret
  )
)

print(lm_vs_xgb_return)
write_xlsx(lm_vs_xgb_return, "lm_vs_xgb_return_comparison.xlsx")