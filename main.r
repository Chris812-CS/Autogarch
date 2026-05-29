# ══════════════════════════════════════════════════════════════
# CONFIG — only change these lines
TICKER    <- "F"
CSV_FILE  <- "data/F.csv"
END_DATE  <- as.Date("2020-07-16")
SKIP_DIST <- c("")  # slow distributions to skip
# ══════════════════════════════════════════════════════════════

# Fix dplyr conflict
filter <- stats::filter
lag    <- stats::lag

# ── Load all Autogarch functions ──────────────────────────────
source("import_file.R")
source("returns.R")
source("beta.R")
source("init_spec.R")
source("init_fit.R")
source("agg_models.R")
source("rank_models.R")

# ── Load data from CSV ────────────────────────────────────────
library(dplyr)
df           <- read.csv(CSV_FILE)
colnames(df)[1] <- "Date"
df$Date      <- as.Date(df$Date)

# ── Trim to END_DATE ──────────────────────────────────────────
df <- df %>% dplyr::filter(Date <= END_DATE)
cat("Last date:", as.character(max(df$Date)), "\n")
cat("Rows:", nrow(df), "\n")

# ── Detect close price column automatically ───────────────────
close_col <- grep("\\.Close$", colnames(df), value = TRUE)[1]
cat("Using close column:", close_col, "\n")

# ── Build prices dataframe ────────────────────────────────────
prices_df        <- data.frame(Date = df$Date, df[[close_col]])
colnames(prices_df)[2] <- TICKER

# ── Step 1: Compute log returns ───────────────────────────────
returns(prices_df, simple = FALSE, view = FALSE)
log_ret <- log_returns

# ── Step 2: Specify all GARCH model types ────────────────────
init_spec(all = TRUE)

# ── Remove slow/problematic distributions ────────────────────
spec_names <- ls(pattern = "\\.spec$", envir = .GlobalEnv)
for (s in spec_names) {
  if (any(sapply(SKIP_DIST, function(d) grepl(d, s)))) {
    rm(list = s, envir = .GlobalEnv)
    message("Skipped slow spec: ", s)
  }
}
cat("Remaining specs:", length(ls(pattern = "\\.spec$", envir = .GlobalEnv)), "\n")

# ── Step 3: Fit remaining models ─────────────────────────────
init_fit(log_ret[[TICKER]], spec_rm = FALSE)

# ── Remove failed/non-converged fits ─────────────────────────
fit_names <- ls(pattern = ".fit", envir = .GlobalEnv)
for (fit in fit_names) {
  obj <- get(fit)
  if (!any(class(obj) == "uGARCHfit") || convergence(obj) != 0) {
    rm(list = fit, envir = .GlobalEnv)
    message("Removed failed fit: ", fit)
  }
}
cat("Converged fits:", length(ls(pattern = "\\.fit$", envir = .GlobalEnv)), "\n")

# ── Step 4: Aggregate information criteria ───────────────────
agg_models(all = TRUE, view = FALSE)
ic <- info_criteria

# ── Step 5: Rank and find best model ─────────────────────────
print(ic)
rank_models(ic)