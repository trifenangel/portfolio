
install.packages("quantmod")
install.packages("dplyr")
install.packages("tibble")
install.packages("writexl")

library(quantmod)
library(dplyr)
library(tibble)
library(writexl)

start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2025-12-31")

###############################################################################
## BITCOIN ##

# Ambil data Bitcoin dari Yahoo Finance
btc_xts <- getSymbols(
  Symbols = "BTC-USD",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Ubah ke data frame
btc_data <- tibble(
  date     = as.Date(index(btc_xts)),
  open     = as.numeric(Op(btc_xts)),
  high     = as.numeric(Hi(btc_xts)),
  low      = as.numeric(Lo(btc_xts)),
  close    = as.numeric(Cl(btc_xts)),
  volume   = as.numeric(Vo(btc_xts)),
  adjusted = as.numeric(Ad(btc_xts))
) %>%
  filter(date >= start_date, date <= end_date) %>%
  arrange(date)

# Lihat hasil awal
head(btc_data)
tail(btc_data)
summary(btc_data)

# Simpan ke CSV
write.csv(btc_data, "bitcoin_daily_2018_2025.csv", row.names = FALSE)

# Simpan ke Excel
write_xlsx(btc_data, "bitcoin_daily_2018_2025.xlsx")


###############################################################################
##S&P500

# Ambil data S&P500
spx_xts <- getSymbols(
  Symbols = "^GSPC",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Convert ke dataframe
spx_data <- tibble(
  date     = as.Date(index(spx_xts)),
  open     = as.numeric(Op(spx_xts)),
  high     = as.numeric(Hi(spx_xts)),
  low      = as.numeric(Lo(spx_xts)),
  close    = as.numeric(Cl(spx_xts)),
  volume   = as.numeric(Vo(spx_xts)),
  adjusted = as.numeric(Ad(spx_xts))
) %>%
  filter(date >= start_date, date <= end_date) %>%
  arrange(date)

# Cek data
head(spx_data)
tail(spx_data)
summary(spx_data)

# Simpan ke file
write.csv(spx_data, "sp500_daily_2018_2025.csv", row.names = FALSE)
write_xlsx(spx_data, "sp500_daily_2018_2025.xlsx")


##############################################################################
## OIL

# Ambil data Crude Oil
oil_xts <- getSymbols(
  Symbols = "CL=F",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Convert ke dataframe
oil_data <- tibble(
  date     = as.Date(index(oil_xts)),
  open     = as.numeric(Op(oil_xts)),
  high     = as.numeric(Hi(oil_xts)),
  low      = as.numeric(Lo(oil_xts)),
  close    = as.numeric(Cl(oil_xts)),
  volume   = as.numeric(Vo(oil_xts)),
  adjusted = as.numeric(Ad(oil_xts))
) %>%
  filter(date >= start_date, date <= end_date) %>%
  arrange(date)

# Cek data
head(oil_data)
tail(oil_data)
summary(oil_data)

# Simpan ke file
write.csv(oil_data, "oil_daily_2018_2025.csv", row.names = FALSE)
write_xlsx(oil_data, "oil_daily_2018_2025.xlsx")


##############################################################################
## GOLD

# Ambil data Gold Futures
gold_xts <- getSymbols(
  Symbols = "GC=F",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Convert ke dataframe
gold_data <- tibble(
  date     = as.Date(index(gold_xts)),
  open     = as.numeric(Op(gold_xts)),
  high     = as.numeric(Hi(gold_xts)),
  low      = as.numeric(Lo(gold_xts)),
  close    = as.numeric(Cl(gold_xts)),
  volume   = as.numeric(Vo(gold_xts)),
  adjusted = as.numeric(Ad(gold_xts))
) %>%
  filter(date >= start_date, date <= end_date) %>%
  arrange(date)

# Cek data
head(gold_data)
tail(gold_data)
summary(gold_data)

# Simpan ke file
write.csv(gold_data, "gold_daily_2018_2025.csv", row.names = FALSE)
write_xlsx(gold_data, "gold_daily_2018_2025.xlsx")

##############################################################################
## VIX


# Ambil data VIX
vix_xts <- getSymbols(
  Symbols = "^VIX",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Convert ke dataframe
vix_data <- tibble(
  date     = as.Date(index(vix_xts)),
  open     = as.numeric(Op(vix_xts)),
  high     = as.numeric(Hi(vix_xts)),
  low      = as.numeric(Lo(vix_xts)),
  close    = as.numeric(Cl(vix_xts)),
  volume   = as.numeric(Vo(vix_xts)),
  adjusted = as.numeric(Ad(vix_xts))
) %>%
  filter(date >= start_date, date <= end_date) %>%
  arrange(date)

# Cek data
head(vix_data)
tail(vix_data)
summary(vix_data)

# Simpan ke file
write.csv(vix_data, "vix_daily_2018_2025.csv", row.names = FALSE)
write_xlsx(vix_data, "vix_daily_2018_2025.xlsx")





