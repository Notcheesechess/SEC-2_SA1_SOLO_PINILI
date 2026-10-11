# ============================================================
# TASK 2
# Bitcoin Distribution Fitting and Ethereum Normality Testing
# ============================================================

library(jsonlite)

# ============================================================
# PART A - BITCOIN
# ============================================================

cat("\n========================================\n")
cat("BITCOIN DISTRIBUTION FITTING ANALYSIS\n")
cat("(This will take a while, btc will be in 4 minutes less\n")
cat("eth meanwhile, because it's huge, will take long.)S\n")
cat("========================================\n")

btc_data <- read.csv("btcusd_1-min_data.csv")

btc_data <- btc_data[!is.na(btc_data$Close), ]

btc_returns <- diff(log(btc_data$Close))

btc_returns <- btc_returns[
  is.finite(btc_returns)
]

cat(
  "\nNumber of Bitcoin Returns:",
  length(btc_returns),
  "\n"
)

cat(
  "Mean Return:",
  round(mean(btc_returns), 8),
  "\n"
)

cat(
  "Variance:",
  round(var(btc_returns), 8),
  "\n"
)

ks_normal <- ks.test(
  btc_returns,
  "pnorm",
  mean = mean(btc_returns),
  sd = sd(btc_returns)
)

cat("\nKOLMOGOROV-SMIRNOV TEST\n")
print(ks_normal)

par(mfrow = c(1, 2))

hist(
  btc_returns,
  breaks = 100,
  col = "lightblue",
  main = "Bitcoin Returns Histogram",
  xlab = "Log Returns"
)

qqnorm(
  btc_returns,
  main = "Bitcoin QQ Plot"
)

qqline(
  btc_returns,
  col = "red",
  lwd = 2
)

par(mfrow = c(1, 1))

if (ks_normal$p.value > 0.05) {
  
  cat(
    "\nConclusion: Bitcoin returns are reasonably",
    "consistent with a Normal distribution.\n"
  )
  
} else {
  
  cat(
    "\nConclusion: Bitcoin returns are not",
    "well-described by a Normal distribution.\n"
  )
}

# ============================================================
# PART B - ETHEREUM
# ============================================================

cat("\n========================================\n")
cat("ETHEREUM NORMALITY TEST\n")
cat("========================================\n")

eth_data <- fromJSON(
  "lsethusd_1m_recent_30d.json"
)

eth_data <- as.data.frame(
  eth_data
)

colnames(eth_data) <- c(
  "Timestamp",
  "Open",
  "High",
  "Low",
  "Close",
  "Volume"
)

eth_5min <- eth_data[
  seq(
    1,
    nrow(eth_data),
    by = 5
  ),
]

eth_returns <- diff(
  log(
    as.numeric(eth_5min$Close)
  )
)

eth_returns <- eth_returns[
  is.finite(eth_returns)
]

cat(
  "\nNumber of Ethereum Returns:",
  length(eth_returns),
  "\n"
)

cat(
  "Mean Return:",
  round(mean(eth_returns), 8),
  "\n"
)

cat(
  "Variance:",
  round(var(eth_returns), 8),
  "\n"
)

set.seed(123)

shapiro_sample <- sample(
  eth_returns,
  min(
    5000,
    length(eth_returns)
  )
)

shapiro_result <- shapiro.test(
  shapiro_sample
)

cat("\nSHAPIRO-WILK NORMALITY TEST\n")
print(shapiro_result)

par(mfrow = c(1, 2))

hist(
  eth_returns,
  breaks = 100,
  col = "lightgreen",
  main = "Ethereum 5-Minute Returns",
  xlab = "Log Returns"
)

qqnorm(
  eth_returns,
  main = "Ethereum QQ Plot"
)

qqline(
  eth_returns,
  col = "blue",
  lwd = 2
)

par(mfrow = c(1, 1))

if (shapiro_result$p.value > 0.05) {
  
  cat(
    "\nConclusion: Ethereum returns appear",
    "normally distributed.\n"
  )
  
} else {
  
  cat(
    "\nConclusion: Ethereum returns do not appear",
    "normally distributed.\n"
  )
}