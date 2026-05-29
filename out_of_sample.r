# out_of_sample.r
# ── Dependencies ──────────────────────────────────────────────
library(dplyr)
library(rugarch)

# ══════════════════════════════════════════════════════════════
# CONFIG — only change these lines
CSV_FILE     <- "data/AMZN.csv"
MODEL        <- "eGARCH"
DISTRIBUTION <- "std"
TRAIN_END    <- as.Date("2020-07-16")
TEST_START   <- as.Date("2020-07-16")
TEST_END     <- as.Date("2020-09-30")
# ══════════════════════════════════════════════════════════════

# ── Load & prepare data ───────────────────────────────────────
df           <- read.csv(CSV_FILE)
colnames(df)[1] <- "Date"
df$Date      <- as.Date(df$Date)

# ── Auto-detect close price column ───────────────────────────
close_col <- grep("\\.Close$", colnames(df), value = TRUE)[1]
cat("Using close column:", close_col, "\n")

# ── Split ─────────────────────────────────────────────────────
train_df <- df %>% dplyr::filter(Date <= TRAIN_END)
test_df  <- df %>% dplyr::filter(Date >= TEST_START & Date <= TEST_END)

cat("Train rows:", nrow(train_df), "| Last train date:", as.character(max(train_df$Date)), "\n")
cat("Test  rows:", nrow(test_df),  "| First test date:", as.character(min(test_df$Date)),  "\n")

# ── Log returns ───────────────────────────────────────────────
train_ret <- diff(log(df[[close_col]][df$Date <= TRAIN_END]))
test_ret  <- diff(log(df[[close_col]][df$Date >= TEST_START & df$Date <= TEST_END]))

cat("Train return range:", round(range(train_ret), 4), "\n")
cat("Test  return range:", round(range(test_ret),  4), "\n")

# ── Fit model ─────────────────────────────────────────────────
cat(sprintf("\nFitting %s + %s ...\n", MODEL, DISTRIBUTION))

spec <- ugarchspec(
  variance.model     = list(model = MODEL, garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = DISTRIBUTION
)

fit <- ugarchfit(spec = spec, data = train_ret, solver = "hybrid")

cat(sprintf("\n── Coefficients (%s + %s) ──────────────────\n", MODEL, DISTRIBUTION))
print(round(coef(fit), 6))

cat(sprintf("\n── Information Criteria (%s + %s) ──────────\n", MODEL, DISTRIBUTION))
print(infocriteria(fit))

# ── Forecast over test window ─────────────────────────────────
horizons <- c(1, 15, 30)
results  <- list()

for (h in horizons) {
  fc       <- ugarchforecast(fit, n.ahead = h)
  fc_vol   <- as.numeric(sigma(fc))
  realised <- abs(test_ret[1:h])
  
  n        <- min(h, length(realised))
  fc_vol   <- fc_vol[1:n]
  realised <- realised[1:n]
  
  rmse <- sqrt(mean((fc_vol - realised)^2))
  mae  <- mean(abs(fc_vol - realised))
  
  results[[paste0("h", h)]] <- data.frame(
    Horizon = h,
    RMSE    = round(rmse, 6),
    MAE     = round(mae,  6)
  )
}

# ── Print summary ─────────────────────────────────────────────
metrics <- do.call(rbind, results)
rownames(metrics) <- NULL

ic <- infocriteria(fit)

# Information Criteria
ic_df <- data.frame(
  Metric = c("AIC", "BIC", "Shibata", "Hannan-Quinn"),
  Value  = round(c(ic["Akaike",], ic["Bayes",], ic["Shibata",], ic["Hannan-Quinn",]), 6)
)
cat(sprintf("\n── Information Criteria (%s + %s) ──────────\n", MODEL, DISTRIBUTION))
print(ic_df, row.names = FALSE)

# Forecast Error Metrics
cat(sprintf("\n── Forecast Error Metrics (%s + %s) ─────────\n", MODEL, DISTRIBUTION))
print(metrics, row.names = FALSE)