###############################################################################
#         CLEANED & ARRANGED CODE FOR 2MA STRATEGIES + VOL BREAKOUT-LIKE
#         SECTION 1: 2MAs MOMENTUM STRATEGY (UPDATED)
###############################################################################

rm(list = ls())

## 1) Get present location and set as working directory
LOC_CODE <- dirname(rstudioapi::getSourceEditorContext()$path)
cat("Current script path:", LOC_CODE, "\n")
setwd(LOC_CODE)

## 2) Load necessary libraries (all here at the top)
library(xts)
library(zoo)
library(TTR)          # For EMA, etc.
library(lubridate)    # For date/time manipulations
library(tseries)      # For maxdrawdown()
library(caTools)      # For runsd(), etc.
library(RColorBrewer) # For heatmaps if needed
library(ggplot2)
library(dplyr)

## 3) Load external functions (all at the top too)
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

###############################################################################
# 4) Custom Calmar Ratio function & flatten_times
###############################################################################
myCalmarRatio <- function(x, scale) {
  avg_ <- mean(coredata(x), na.rm = TRUE)
  dd_obj <- maxdrawdown(cumsum(x))
  if (is.null(dd_obj$maxdrawdown) || dd_obj$maxdrawdown == 0) {
    return(NA)
  }
  return(scale * avg_ / dd_obj$maxdrawdown)
}

flatten_times <- function(pos, allowed_start = "09:55", allowed_end = "15:40") {
  # Force position to 0 if time < allowed_start or time >= allowed_end
  hhmm <- format(index(pos), "%H:%M")
  
  pos[hhmm < allowed_start]  <- 0
  pos[hhmm >= allowed_end]   <- 0
  
  return(pos)
}

# ---------------------------------------------------------------------------
# NOTE: We keep build_pos_flat defined if needed by the rest of the code,
# but we no longer rely on it to flatten intraday. We won't remove it.
# ---------------------------------------------------------------------------
build_pos_flat <- function(x) {
  posf <- xts(rep(0, NROW(x)), order.by = index(x))
  hhmm <- format(index(x), format = "%H:%M")
  posf[hhmm >= "15:40"] <- 1
  posf[hhmm >= "09:31" & hhmm < "09:55"] <- 1
  return(posf)
}

###############################################################################
# SECTION 1: 2MAs MOMENTUM STRATEGY - UPDATED FLATTENING
###############################################################################
quarters <- c("2022_Q1", "2022_Q3", "2022_Q4", 
              "2023_Q2", "2023_Q4", "2024_Q1", "2024_Q2")

all_results_mom <- data.frame()

contract_multiplier_NQ <- 20
contract_multiplier_SP <- 50
transaction_cost       <- 12.0
scale_annual           <- 252

fast_ema_list <- c(10, 20, 30)
slow_ema_list <- c(60, 75, 90)

cat("\n===== 2MAs MOMENTUM STRATEGY =====\n\n")

for (selected_quarter in quarters) {
  
  cat("Processing quarter (Momentum):", selected_quarter, "...\n")
  
  filename <- paste0("data1_", selected_quarter, ".RData")
  if (!file.exists(filename)) {
    cat("File", filename, "not found. Skipping quarter", selected_quarter, "\n")
    next
  }
  
  load(filename)
  object_name <- paste0("data1_", selected_quarter)
  if (!exists(object_name)) {
    cat("Object", object_name, "not found after loading", filename, "\n")
    next
  }
  
  data_group1 <- get(object_name)
  
  if (!all(c("NQ", "SP") %in% colnames(data_group1))) {
    cat("Columns NQ or SP not found in", object_name, "\n")
    next
  }
  
  # 4) Forward-fill missing
  data_group1$NQ <- na.locf(data_group1$NQ, na.rm = FALSE)
  data_group1$SP <- na.locf(data_group1$SP, na.rm = FALSE)
  
  # 5) Remove data from certain times
  hhmm <- format(index(data_group1), format = "%H:%M")
  data_group1[hhmm >= "09:31" & hhmm <= "09:40", ] <- NA
  data_group1[hhmm >= "15:51" & hhmm <= "16:00", ] <- NA
  
  for (fast_ema in fast_ema_list) {
    for (slow_ema in slow_ema_list) {
      
      # --- 6.1) (REMOVED the direct flatten from build_pos_flat usage) ---
      # Instead, we just compute the raw momentum positions, then flatten later.
      
      # NQ: compute raw momentum position
      NQ_ff <- na.locf(data_group1$NQ, na.rm = FALSE)
      ema_fast_NQ <- EMA(NQ_ff, n = fast_ema)
      ema_slow_NQ <- EMA(NQ_ff, n = slow_ema)
      
      lag_fast_NQ <- lag.xts(ema_fast_NQ, k = 1)
      lag_slow_NQ <- lag.xts(ema_slow_NQ, k = 1)
      
      posNQ_mom_raw <- ifelse(lag_fast_NQ > lag_slow_NQ, 1, -1)
      posNQ_mom_raw <- na.locf(posNQ_mom_raw, na.rm = FALSE)
      posNQ_mom_raw <- xts(posNQ_mom_raw, order.by = index(data_group1))
      
      # Flatten only AFTER computing the raw position
      posNQ_mom_final <- flatten_times(posNQ_mom_raw, allowed_start = "09:55", allowed_end = "15:40")
      
      # SP: raw momentum position
      SP_ff <- na.locf(data_group1$SP, na.rm = FALSE)
      ema_fast_SP <- EMA(SP_ff, n = fast_ema)
      ema_slow_SP <- EMA(SP_ff, n = slow_ema)
      
      lag_fast_SP <- lag.xts(ema_fast_SP, k = 1)
      lag_slow_SP <- lag.xts(ema_slow_SP, k = 1)
      
      posSP_mom_raw <- ifelse(lag_fast_SP > lag_slow_SP, 1, -1)
      posSP_mom_raw <- na.locf(posSP_mom_raw, na.rm = FALSE)
      posSP_mom_raw <- xts(posSP_mom_raw, order.by = index(data_group1))
      
      posSP_mom_final <- flatten_times(posSP_mom_raw, allowed_start = "09:55", allowed_end = "15:40")
      
      # 7) Compute PnL from final positions
      dNQ <- diff.xts(data_group1$NQ)
      dSP <- diff.xts(data_group1$SP)
      dNQ[is.na(dNQ)] <- 0
      dSP[is.na(dSP)] <- 0
      
      # Gross PnL
      pnl_grossNQ_mom <- posNQ_mom_final * dNQ * contract_multiplier_NQ
      pnl_grossSP_mom <- posSP_mom_final * dSP * contract_multiplier_SP
      
      # Transactions from the final positions
      ntransNQ_mom <- abs(diff.xts(posNQ_mom_final))
      ntransSP_mom <- abs(diff.xts(posSP_mom_final))
      ntransNQ_mom[is.na(ntransNQ_mom)] <- 0
      ntransSP_mom[is.na(ntransSP_mom)] <- 0
      
      pnl_netNQ_mom <- pnl_grossNQ_mom - (ntransNQ_mom * transaction_cost)
      pnl_netSP_mom <- pnl_grossSP_mom - (ntransSP_mom * transaction_cost)
      
      pnl_gross_mom <- pnl_grossNQ_mom + pnl_grossSP_mom
      pnl_net_mom   <- pnl_netNQ_mom   + pnl_netSP_mom
      
      # 8) Aggregate to daily
      day_ends <- endpoints(data_group1, on = "days")
      daily_gross_mom <- period.apply(pnl_gross_mom, INDEX = day_ends, FUN = sum, na.rm = TRUE)
      daily_net_mom   <- period.apply(pnl_net_mom,   INDEX = day_ends, FUN = sum, na.rm = TRUE)
      
      # Combine # of trades from both instruments
      daily_ntrans <- period.apply(ntransNQ_mom + ntransSP_mom, INDEX = day_ends, FUN = sum, na.rm = TRUE)
      
      # 9) Metrics
      grossSR <- mySR(daily_gross_mom, scale = scale_annual)
      netSR   <- mySR(daily_net_mom,   scale = scale_annual)
      
      grossCR <- myCalmarRatio(daily_gross_mom, scale = scale_annual)
      netCR   <- myCalmarRatio(daily_net_mom,   scale = scale_annual)
      
      avg_ntrades <- mean(daily_ntrans, na.rm = TRUE)
      grossPnL    <- sum(daily_gross_mom, na.rm = TRUE)
      netPnL      <- sum(daily_net_mom,   na.rm = TRUE)
      
      stat_ <- if (abs(netPnL) < 1e-10) {
        0
      } else {
        netCR * max(0, log(abs(netPnL / 1000)))
      }
      
      param_result <- data.frame(
        quarter           = selected_quarter,
        fastEMA           = fast_ema,
        slowEMA           = slow_ema,
        contractNQ        = contract_multiplier_NQ,
        contractSP        = contract_multiplier_SP,
        transaction_cost  = transaction_cost,
        grossSR           = grossSR,
        netSR             = netSR,
        grossCR           = grossCR,
        netCR             = netCR,
        av_daily_ntrades  = avg_ntrades,
        grossPnL          = grossPnL,
        netPnL            = netPnL,
        stat              = stat_,
        stringsAsFactors  = FALSE
      )
      
      all_results_mom <- rbind(all_results_mom, param_result)
    }
  }
  
  rm(data_group1)
}

# Sort final momentum results & show top lines
all_results_mom <- all_results_mom[order(-all_results_mom$stat), ]
cat("\nTop Momentum Results:\n")
print(head(all_results_mom, 20))

# Save results
save(all_results_mom, file = "mom_multi_param_results_for_2MA.RData")
write.csv(all_results_mom, file = "mom_multi_param_results_for_2MA.csv", row.names = FALSE)
cat("Saved multi-parameter (Momentum) strategy results to 'mom_multi_param_results_for_2MA'\n")


summary_mom <- all_results_mom[order(-all_results_mom$stat), ]
summary_mom$signalEMA_slowEMA <- paste0(summary_mom$fastEMA, "_", summary_mom$slowEMA)

plotHeatmap(
  data_plot    = summary_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "2MAs Momentum Strategy: Sensitivity Analysis Using 'stat'",
  label_size   = 3
)

df_agg_mom <- summary_mom %>%
  group_by(fastEMA, slowEMA) %>%
  summarise(stat = mean(stat, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

plotHeatmap(
  data_plot    = df_agg_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "Aggregated 2MAs Momentum Sensitivity (Using 'stat')",
  label_size   = 3
)


###############################################################################
# SECTION 2: 2MAs MEAN-REVERSION STRATEGY (UPDATED)
###############################################################################

rm(list = setdiff(ls(), c("LOC_CODE","mySR","myCalmarRatio","plotHeatmap","build_pos_flat")))

# (Re)load libraries if needed:
library(xts)
library(zoo)
library(TTR)
library(lubridate)
library(tseries)
library(caTools)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

## We'll re-define flatten_times here just like above
flatten_times <- function(pos, allowed_start = "09:55", allowed_end = "15:40") {
  hhmm <- format(index(pos), "%H:%M")
  pos[hhmm < allowed_start] <- 0
  pos[hhmm >= allowed_end]  <- 0
  return(pos)
}

quarters <- c("2022_Q1", "2022_Q3", "2022_Q4", 
              "2023_Q2", "2023_Q4", "2024_Q1", "2024_Q2")

contract_multiplier_NQ <- 20
contract_multiplier_SP <- 50
transaction_cost <- 12.0
scale_annual <- 252

fast_ema_list <- c(10, 20, 30)
slow_ema_list <- c(60, 75, 90)

all_results_mr <- data.frame()

cat("\n===== 2MAs MEAN-REVERSION STRATEGY (UPDATED FLATTENING) =====\n\n")
for (selected_quarter in quarters) {
  
  cat("Processing quarter (MR):", selected_quarter, "...\n")
  
  filename <- paste0("data1_", selected_quarter, ".RData")
  if (!file.exists(filename)) {
    cat("File", filename, "not found. Skipping quarter", selected_quarter, "\n")
    next
  }
  
  load(filename)
  object_name <- paste0("data1_", selected_quarter)
  if (!exists(object_name)) {
    cat("Object", object_name, "not found after loading", filename, "\n")
    next
  }
  
  data_group1 <- get(object_name)
  if (!all(c("NQ", "SP") %in% colnames(data_group1))) {
    cat("Columns NQ or SP not found in", object_name, "\n")
    next
  }
  
  data_group1$NQ <- na.locf(data_group1$NQ, na.rm = FALSE)
  data_group1$SP <- na.locf(data_group1$SP, na.rm = FALSE)
  
  hhmm <- format(index(data_group1), format = "%H:%M")
  data_group1[hhmm >= "09:31" & hhmm <= "09:40", ] <- NA
  data_group1[hhmm >= "15:51" & hhmm <= "16:00", ] <- NA
  
  for (fast_ema in fast_ema_list) {
    for (slow_ema in slow_ema_list) {
      
      # --- RAW MR Positions (no direct flatten in ifelse) ---
      
      # NQ
      NQ_ff <- na.locf(data_group1$NQ, na.rm = FALSE)
      ema_fast_NQ <- EMA(NQ_ff, n = fast_ema)
      ema_slow_NQ <- EMA(NQ_ff, n = slow_ema)
      
      lag_fast_NQ <- lag.xts(ema_fast_NQ, k = 1)
      lag_slow_NQ <- lag.xts(ema_slow_NQ, k = 1)
      
      posNQ_mr_raw <- ifelse(lag_fast_NQ > lag_slow_NQ, -1, 1)
      posNQ_mr_raw <- na.locf(posNQ_mr_raw, na.rm = FALSE)
      posNQ_mr_raw <- xts(posNQ_mr_raw, order.by = index(data_group1))
      
      # Flatten afterwards
      posNQ_mr_final <- flatten_times(posNQ_mr_raw, "09:55", "15:40")
      
      # SP
      SP_ff <- na.locf(data_group1$SP, na.rm = FALSE)
      ema_fast_SP <- EMA(SP_ff, n = fast_ema)
      ema_slow_SP <- EMA(SP_ff, n = slow_ema)
      
      lag_fast_SP <- lag.xts(ema_fast_SP, k = 1)
      lag_slow_SP <- lag.xts(ema_slow_SP, k = 1)
      
      posSP_mr_raw <- ifelse(lag_fast_SP > lag_slow_SP, -1, 1)
      posSP_mr_raw <- na.locf(posSP_mr_raw, na.rm = FALSE)
      posSP_mr_raw <- xts(posSP_mr_raw, order.by = index(data_group1))
      
      posSP_mr_final <- flatten_times(posSP_mr_raw, "09:55", "15:40")
      
      # PnL from final positions
      dNQ <- diff.xts(data_group1$NQ)
      dSP <- diff.xts(data_group1$SP)
      dNQ[is.na(dNQ)] <- 0
      dSP[is.na(dSP)] <- 0
      
      pnl_grossNQ_mr <- posNQ_mr_final * dNQ * contract_multiplier_NQ
      pnl_grossSP_mr <- posSP_mr_final * dSP * contract_multiplier_SP
      
      ntransNQ_mr <- abs(diff.xts(posNQ_mr_final))
      ntransSP_mr <- abs(diff.xts(posSP_mr_final))
      ntransNQ_mr[is.na(ntransNQ_mr)] <- 0
      ntransSP_mr[is.na(ntransSP_mr)] <- 0
      
      pnl_netNQ_mr <- pnl_grossNQ_mr - (ntransNQ_mr * transaction_cost)
      pnl_netSP_mr <- pnl_grossSP_mr - (ntransSP_mr * transaction_cost)
      
      pnl_gross_mr <- pnl_grossNQ_mr + pnl_grossSP_mr
      pnl_net_mr   <- pnl_netNQ_mr   + pnl_netSP_mr
      
      day_ends <- endpoints(data_group1, on = "days")
      daily_gross_mr <- period.apply(pnl_gross_mr, INDEX = day_ends, FUN = sum, na.rm = TRUE)
      daily_net_mr   <- period.apply(pnl_net_mr,   INDEX = day_ends, FUN = sum, na.rm = TRUE)
      daily_ntrans   <- period.apply(ntransNQ_mr + ntransSP_mr, INDEX = day_ends, FUN = sum, na.rm = TRUE)
      
      grossSR <- mySR(daily_gross_mr, scale = scale_annual)
      netSR   <- mySR(daily_net_mr,   scale = scale_annual)
      
      grossCR <- myCalmarRatio(daily_gross_mr, scale = scale_annual)
      netCR   <- myCalmarRatio(daily_net_mr,   scale = scale_annual)
      
      avg_ntrades <- mean(daily_ntrans, na.rm = TRUE)
      grossPnL    <- sum(daily_gross_mr, na.rm = TRUE)
      netPnL      <- sum(daily_net_mr,   na.rm = TRUE)
      
      stat_ <- if (abs(netPnL) < 1e-10) {
        0
      } else {
        netCR * max(0, log(abs(netPnL / 1000)))
      }
      
      result_row <- data.frame(
        quarter          = selected_quarter,
        fastEMA          = fast_ema,
        slowEMA          = slow_ema,
        contractNQ       = contract_multiplier_NQ,
        contractSP       = contract_multiplier_SP,
        transaction_cost = transaction_cost,
        grossSR          = grossSR,
        netSR            = netSR,
        grossCR          = grossCR,
        netCR            = netCR,
        av_daily_ntrades = avg_ntrades,
        grossPnL         = grossPnL,
        netPnL           = netPnL,
        stat             = stat_,
        stringsAsFactors = FALSE
      )
      
      all_results_mr <- rbind(all_results_mr, result_row)
      
    }
  }
  rm(data_group1)
}

all_results_mr <- all_results_mr[order(-all_results_mr$stat), ]
cat("\nTop Mean-Reversion Results:\n")
print(head(all_results_mr, 20))

save(all_results_mr, file = "mr_multi_param_results_for_2MA.RData")
write.csv(all_results_mr, file = "mr_multi_param_results_for_2MA.csv", row.names = FALSE)
cat("Saved multi-parameter (Mean-Reversion) strategy results to 'mr_multi_param_results_for_2MA'\n")



summary_mom <- all_results_mom[order(-all_results_mom$stat), ]
summary_mom$signalEMA_slowEMA <- paste0(summary_mom$fastEMA, "_", summary_mom$slowEMA)

plotHeatmap(
  data_plot    = summary_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "2MAs Momentum Strategy: Sensitivity Analysis Using 'stat'",
  label_size   = 3
)

df_agg_mom <- summary_mom %>%
  group_by(fastEMA, slowEMA) %>%
  summarise(stat = mean(stat, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

plotHeatmap(
  data_plot    = df_agg_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "Aggregated 2MAs Momentum Sensitivity (Using 'stat')",
  label_size   = 3
)


###############################################################################
# SECTION 3: VOLATILITY BREAKOUT (MOMENTUM) - UPDATED, we use the same code to trade SP or NQ seprately.
###############################################################################

rm(list = setdiff(ls(), c("LOC_CODE","mySR","myCalmarRatio","plotHeatmap","build_pos_flat")))

library(xts)
library(zoo)
library(TTR)
library(lubridate)
library(tseries)
library(caTools)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

## Additional external function if needed:
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

# Flatten after the raw position
flatten_times <- function(pos, allowed_start = "09:55", allowed_end = "15:40") {
  hhmm <- format(index(pos), "%H:%M")
  pos[hhmm < allowed_start] <- 0
  pos[hhmm >= allowed_end]  <- 0
  return(pos)
}

quarters <- c("2022_Q1", "2022_Q3", "2022_Q4", 
              "2023_Q2", "2023_Q4", "2024_Q1", "2024_Q2")

contract_multiplier_NQ <- 20
contract_multiplier_SP <- 50
transaction_cost       <- 12.0
scale_annual           <- 252

fast_ema_list <- c(10, 20, 30)
slow_ema_list <- c(60, 75, 90)
vol_sd_list   <- c(30, 60, 90)
m_list        <- c(1.0, 1.5, 2.0)

all_results_vb <- data.frame()

cat("\n===== VOLATILITY BREAKOUT STRATEGY (Momentum) UPDATED =====\n\n")
for (selected_quarter in quarters) {
  cat("Processing quarter (VB):", selected_quarter, "...\n")
  
  filename <- paste0("data1_", selected_quarter, ".RData")
  if (!file.exists(filename)) {
    cat("File", filename, "not found. Skipping quarter", selected_quarter, "\n")
    next
  }
  
  load(filename)
  object_name <- paste0("data1_", selected_quarter)
  if (!exists(object_name)) {
    cat("Object", object_name, "not found after loading", filename, "\n")
    next
  }
  
  data_group1 <- get(object_name)
  if (!all(c("NQ", "SP") %in% colnames(data_group1))) {
    cat("Columns NQ or SP not found in", object_name, "\n")
    next
  }
  
  data_group1$NQ <- na.locf(data_group1$NQ, na.rm = FALSE)
  data_group1$SP <- na.locf(data_group1$SP, na.rm = FALSE)
  
  hhmm <- format(index(data_group1), format = "%H:%M")
  data_group1[hhmm >= "09:31" & hhmm <= "09:40", ] <- NA
  data_group1[hhmm >= "15:51" & hhmm <= "16:00", ] <- NA
  
  for (fast_ema in fast_ema_list) {
    for (slow_ema in slow_ema_list) {
      for (vol_sd in vol_sd_list) {
        for (m_ in m_list) {
          
          # 1) NQ - build raw position (no flatten). 
          NQ_ff       <- na.locf(data_group1$NQ, na.rm = FALSE)
          ema_fast_NQ <- EMA(NQ_ff, n = fast_ema)
          ema_slow_NQ <- EMA(NQ_ff, n = slow_ema)
          roll_std_NQ <- runsd(NQ_ff, vol_sd, endrule = "NA", align = "right")
          
          posNQ_raw <- positionVB_new(
            signal   = ema_fast_NQ,
            lower    = ema_slow_NQ - m_ * roll_std_NQ,
            upper    = ema_slow_NQ + m_ * roll_std_NQ,
            pos_flat = rep(0, NROW(NQ_ff)),  # disable flatten inside
            strategy = "mom"
          )
          posNQ_raw <- xts(posNQ_raw, order.by = index(data_group1))
          posNQ_raw <- na.locf(posNQ_raw, na.rm = FALSE)
          
          # flatten after raw
          posNQ_final <- flatten_times(posNQ_raw, "09:55", "15:40")
          
          # 2) SP - build raw position (no flatten)
          SP_ff       <- na.locf(data_group1$SP, na.rm = FALSE)
          ema_fast_SP <- EMA(SP_ff, n = fast_ema)
          ema_slow_SP <- EMA(SP_ff, n = slow_ema)
          roll_std_SP <- runsd(SP_ff, vol_sd, endrule = "NA", align = "right")
          
          posSP_raw <- positionVB_new(
            signal   = ema_fast_SP,
            lower    = ema_slow_SP - m_ * roll_std_SP,
            upper    = ema_slow_SP + m_ * roll_std_SP,
            pos_flat = rep(0, NROW(SP_ff)),  # disable flatten inside
            strategy = "mom"
          )
          posSP_raw <- xts(posSP_raw, order.by = index(data_group1))
          posSP_raw <- na.locf(posSP_raw, na.rm = FALSE)
          
          posSP_final <- flatten_times(posSP_raw, "09:55", "15:40")
          
          # 3) PnL
          dNQ <- diff.xts(data_group1$NQ)
          dSP <- diff.xts(data_group1$SP)
          dNQ[is.na(dNQ)] <- 0
          dSP[is.na(dSP)] <- 0
          
          pnl_grossNQ <- posNQ_final * dNQ * contract_multiplier_NQ
          pnl_grossSP <- posSP_final * dSP * contract_multiplier_SP
          
          ntransNQ <- abs(diff.xts(posNQ_final))
          ntransSP <- abs(diff.xts(posSP_final))
          ntransNQ[is.na(ntransNQ)] <- 0
          ntransSP[is.na(ntransSP)] <- 0
          
          pnl_netNQ <- pnl_grossNQ - (ntransNQ * transaction_cost)
          pnl_netSP <- pnl_grossSP - (ntransSP * transaction_cost)
          
          pnl_gross_mom <- pnl_grossNQ + pnl_grossSP
          pnl_net_mom   <- pnl_netNQ   + pnl_netSP
          
          # 4) daily
          day_ends <- endpoints(data_group1, on = "days")
          daily_gross_mom <- period.apply(pnl_gross_mom, INDEX = day_ends, FUN = sum, na.rm = TRUE)
          daily_net_mom   <- period.apply(pnl_net_mom,   INDEX = day_ends, FUN = sum, na.rm = TRUE)
          daily_ntrans    <- period.apply(ntransNQ + ntransSP, INDEX = day_ends, FUN = sum, na.rm = TRUE)
          
          grossSR <- mySR(daily_gross_mom, scale = scale_annual)
          netSR   <- mySR(daily_net_mom,   scale = scale_annual)
          grossCR <- myCalmarRatio(daily_gross_mom, scale = scale_annual)
          netCR   <- myCalmarRatio(daily_net_mom,   scale = scale_annual)
          
          avg_ntrades <- mean(daily_ntrans, na.rm = TRUE)
          grossPnL    <- sum(daily_gross_mom, na.rm = TRUE)
          netPnL      <- sum(daily_net_mom,   na.rm = TRUE)
          
          stat_ <- if (abs(netPnL) < 1e-10) {
            0
          } else {
            netCR * max(0, log(abs(netPnL / 1000)))
          }
          
          result_row <- data.frame(
            quarter          = selected_quarter,
            fastEMA          = fast_ema,
            slowEMA          = slow_ema,
            vol_sd           = vol_sd,
            m_               = m_,
            contractNQ       = contract_multiplier_NQ,
            contractSP       = contract_multiplier_SP,
            transaction_cost = transaction_cost,
            grossSR          = grossSR,
            netSR            = netSR,
            grossCR          = grossCR,
            netCR            = netCR,
            av_daily_ntrades = avg_ntrades,
            grossPnL         = grossPnL,
            netPnL           = netPnL,
            stat             = stat_,
            stringsAsFactors = FALSE
          )
          all_results_vb <- rbind(all_results_vb, result_row)
          
        }
      }
    }
  }
  rm(data_group1)
}

all_results_vb <- all_results_vb[order(-all_results_vb$stat), ]
cat("\nTop Vol-Breakout-Like Results (MOM):\n")
print(head(all_results_vb, 20))

save(all_results_vb, file = "mom_multi_param_results_for_VB.RData")
write.csv(all_results_vb, file = "mom_multi_param_results_for_VB.csv", row.names = FALSE)
cat("Saved multi-parameter strategy results to 'mom_multi_param_results_for_VB'\n")


summary_mom <- all_results_mom[order(-all_results_mom$stat), ]
summary_mom$signalEMA_slowEMA <- paste0(summary_mom$fastEMA, "_", summary_mom$slowEMA)

plotHeatmap(
  data_plot    = summary_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "2MAs Momentum Strategy: Sensitivity Analysis Using 'stat'",
  label_size   = 3
)

df_agg_mom <- summary_mom %>%
  group_by(fastEMA, slowEMA) %>%
  summarise(stat = mean(stat, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

plotHeatmap(
  data_plot    = df_agg_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "Aggregated 2MAs Momentum Sensitivity (Using 'stat')",
  label_size   = 3
)



###############################################################################
# SECTION 4: VOLATILITY BREAKOUT (MEAN‐REVERSION) - UPDATED
###############################################################################

rm(list = setdiff(ls(), c("LOC_CODE","mySR","myCalmarRatio","plotHeatmap","build_pos_flat")))

library(xts)
library(zoo)
library(TTR)
library(lubridate)
library(tseries)
library(caTools)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

flatten_times <- function(pos, allowed_start = "09:55", allowed_end = "15:40") {
  hhmm <- format(index(pos), "%H:%M")
  pos[hhmm < allowed_start] <- 0
  pos[hhmm >= allowed_end]  <- 0
  return(pos)
}

quarters <- c("2022_Q1", "2022_Q3", "2022_Q4", 
              "2023_Q2", "2023_Q4", "2024_Q1", "2024_Q2")

contract_multiplier_NQ <- 20
contract_multiplier_SP <- 50
transaction_cost <- 12.0
scale_annual <- 252

fast_ema_list <- c(10, 20, 30)
slow_ema_list <- c(60, 75, 90)
vol_sd_list   <- c(30, 60, 90)
m_list        <- c(1.0, 1.5, 2.0)

all_results_vb <- data.frame()

cat("\n===== VOLATILITY BREAKOUT STRATEGY (MEAN‐REVERSION, Multi‐Param, UPDATED) =====\n\n")
for (selected_quarter in quarters) {
  cat("Processing quarter (VB, MR):", selected_quarter, "...\n")
  
  filename <- paste0("data1_", selected_quarter, ".RData")
  if (!file.exists(filename)) {
    cat("File", filename, "not found. Skipping quarter", selected_quarter, "\n")
    next
  }
  
  load(filename)
  object_name <- paste0("data1_", selected_quarter)
  if (!exists(object_name)) {
    cat("Object", object_name, "not found after loading", filename, "\n")
    next
  }
  
  data_group1 <- get(object_name)
  if (!all(c("NQ", "SP") %in% colnames(data_group1))) {
    cat("Columns NQ or SP not found in", object_name, "\n")
    next
  }
  
  data_group1$NQ <- na.locf(data_group1$NQ, na.rm = FALSE)
  data_group1$SP <- na.locf(data_group1$SP, na.rm = FALSE)
  
  hhmm <- format(index(data_group1), "%H:%M")
  data_group1[hhmm >= "09:31" & hhmm <= "09:40", ] <- NA
  data_group1[hhmm >= "15:51" & hhmm <= "16:00", ] <- NA
  
  for (fast_ema in fast_ema_list) {
    for (slow_ema in slow_ema_list) {
      for (vol_sd in vol_sd_list) {
        for (m_ in m_list) {
          
          # 1) NQ raw position => strategy = "mr" w/out flatten
          NQ_ff       <- na.locf(data_group1$NQ, na.rm = FALSE)
          ema_fast_NQ <- EMA(NQ_ff, n = fast_ema)
          ema_slow_NQ <- EMA(NQ_ff, n = slow_ema)
          roll_std_NQ <- runsd(NQ_ff, vol_sd, endrule = "NA", align = "right")
          
          posNQ_raw <- positionVB_new(
            signal   = ema_fast_NQ,
            lower    = ema_slow_NQ - m_ * roll_std_NQ,
            upper    = ema_slow_NQ + m_ * roll_std_NQ,
            pos_flat = rep(0, NROW(NQ_ff)),  # disable flatten inside
            strategy = "mr"
          )
          posNQ_raw <- xts(posNQ_raw, order.by = index(data_group1))
          posNQ_raw <- na.locf(posNQ_raw, na.rm = FALSE)
          
          posNQ_final <- flatten_times(posNQ_raw, "09:55", "15:40")
          
          # 2) SP raw position => strategy = "mr"
          SP_ff       <- na.locf(data_group1$SP, na.rm = FALSE)
          ema_fast_SP <- EMA(SP_ff, n = fast_ema)
          ema_slow_SP <- EMA(SP_ff, n = slow_ema)
          roll_std_SP <- runsd(SP_ff, vol_sd, endrule = "NA", align = "right")
          
          posSP_raw <- positionVB_new(
            signal   = ema_fast_SP,
            lower    = ema_slow_SP - m_ * roll_std_SP,
            upper    = ema_slow_SP + m_ * roll_std_SP,
            pos_flat = rep(0, NROW(SP_ff)),  # disable flatten inside
            strategy = "mr"
          )
          posSP_raw <- xts(posSP_raw, order.by = index(data_group1))
          posSP_raw <- na.locf(posSP_raw, na.rm = FALSE)
          
          posSP_final <- flatten_times(posSP_raw, "09:55", "15:40")
          
          # 3) PnL from final
          dNQ <- diff.xts(data_group1$NQ)
          dSP <- diff.xts(data_group1$SP)
          dNQ[is.na(dNQ)] <- 0
          dSP[is.na(dSP)] <- 0
          
          pnl_grossNQ <- posNQ_final * dNQ * contract_multiplier_NQ
          pnl_grossSP <- posSP_final * dSP * contract_multiplier_SP
          
          ntransNQ <- abs(diff.xts(posNQ_final))
          ntransSP <- abs(diff.xts(posSP_final))
          ntransNQ[is.na(ntransNQ)] <- 0
          ntransSP[is.na(ntransSP)] <- 0
          
          pnl_netNQ <- pnl_grossNQ - (ntransNQ * transaction_cost)
          pnl_netSP <- pnl_grossSP - (ntransSP * transaction_cost)
          
          pnl_gross_mr <- pnl_grossNQ + pnl_grossSP
          pnl_net_mr   <- pnl_netNQ   + pnl_netSP
          
          # 4) Daily
          day_ends <- endpoints(data_group1, on = "days")
          daily_gross_mr <- period.apply(pnl_gross_mr, INDEX = day_ends, FUN = sum, na.rm = TRUE)
          daily_net_mr   <- period.apply(pnl_net_mr,   INDEX = day_ends, FUN = sum, na.rm = TRUE)
          daily_ntrans   <- period.apply(ntransNQ + ntransSP, INDEX = day_ends, FUN = sum, na.rm = TRUE)
          
          grossSR <- mySR(daily_gross_mr, scale = scale_annual)
          netSR   <- mySR(daily_net_mr,   scale = scale_annual)
          
          grossCR <- myCalmarRatio(daily_gross_mr, scale = scale_annual)
          netCR   <- myCalmarRatio(daily_net_mr,   scale = scale_annual)
          
          avg_ntrades <- mean(daily_ntrans, na.rm = TRUE)
          grossPnL    <- sum(daily_gross_mr, na.rm = TRUE)
          netPnL      <- sum(daily_net_mr,   na.rm = TRUE)
          
          stat_ <- if (abs(netPnL) < 1e-10) {
            0
          } else {
            netCR * max(0, log(abs(netPnL / 1000)))
          }
          
          result_row <- data.frame(
            quarter          = selected_quarter,
            fastEMA          = fast_ema,
            slowEMA          = slow_ema,
            vol_sd           = vol_sd,
            m_               = m_,
            contractNQ       = contract_multiplier_NQ,
            contractSP       = contract_multiplier_SP,
            transaction_cost = transaction_cost,
            grossSR          = grossSR,
            netSR            = netSR,
            grossCR          = grossCR,
            netCR            = netCR,
            av_daily_ntrades = avg_ntrades,
            grossPnL         = sum(daily_gross_mr, na.rm = TRUE),
            netPnL           = sum(daily_net_mr,   na.rm = TRUE),
            stat             = stat_,
            stringsAsFactors = FALSE
          )
          all_results_vb <- rbind(all_results_vb, result_row)
          
        }
      }
    }
  }
  rm(data_group1)
}

all_results_vb <- all_results_vb[order(-all_results_vb$stat), ]
cat("\nTop Vol-Breakout-Like (MR) Results:\n")
print(head(all_results_vb, 20))

save(all_results_vb, file = "mr_multi_param_results_for_VB.RData")
write.csv(all_results_vb, file = "mr_multi_param_results_for_VB.csv", row.names = FALSE)
cat("Saved multi-parameter (Mean‐Reversion) VB results to 'mr_multi_param_results_for_VB'\n")


summary_mom <- all_results_mom[order(-all_results_mom$stat), ]
summary_mom$signalEMA_slowEMA <- paste0(summary_mom$fastEMA, "_", summary_mom$slowEMA)

plotHeatmap(
  data_plot    = summary_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "2MAs Momentum Strategy: Sensitivity Analysis Using 'stat'",
  label_size   = 3
)

df_agg_mom <- summary_mom %>%
  group_by(fastEMA, slowEMA) %>%
  summarise(stat = mean(stat, na.rm = TRUE), .groups = "drop") %>%
  as.data.frame()

plotHeatmap(
  data_plot    = df_agg_mom,
  col_vlabels  = "fastEMA",
  col_hlabels  = "slowEMA",
  col_variable = "stat",
  main         = "Aggregated 2MAs Momentum Sensitivity (Using 'stat')",
  label_size   = 3
)


############## final strategy (UPDATED flatten approach) ##############################
rm(list = ls())

# 1) Load libraries & external functions
library(xts)
library(zoo)
library(TTR)
library(lubridate)
library(tseries)
library(caTools)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

# ------------------------------------------------------------------------------
# 2) flatten_times function & custom Calmar
# ------------------------------------------------------------------------------
flatten_times <- function(pos, allowed_start = "09:55", allowed_end = "15:40") {
  hhmm <- format(index(pos), "%H:%M")
  pos[hhmm < allowed_start] <- 0
  pos[hhmm >= allowed_end]  <- 0
  return(pos)
}

myCalmarRatio <- function(x, scale = 252) {
  avg_ <- mean(coredata(x), na.rm = TRUE)
  dd_obj <- maxdrawdown(cumsum(x))
  if (is.null(dd_obj$maxdrawdown) || dd_obj$maxdrawdown == 0) return(NA)
  return(scale * avg_ / dd_obj$maxdrawdown)
}

# ------------------------------------------------------------------------------
# 3) Hard‐coded best parameters for NQ & SP
# ------------------------------------------------------------------------------
# NQ => (fastEMA=20, slowEMA=60, vol_sd=60, m_=2)
param_NQ <- list(fast_ema = 20, slow_ema = 60, vol_sd = 60, m_ = 2)

# SP => (fastEMA=30, slowEMA=60, vol_sd=60, m_=2)
param_SP <- list(fast_ema = 30, slow_ema = 60, vol_sd = 60, m_ = 2)

# Constants
contract_multiplier_NQ <- 20
contract_multiplier_SP <- 50
transaction_cost       <- 12.0
scale_annual           <- 252

# 4) Quarter(s) or data file(s) to process
selected_quarter <- "2022_Q1"

filename <- paste0("data1_", selected_quarter, ".RData")
if (!file.exists(filename)) {
  stop("File not found: ", filename)
}
load(filename) 
object_name <- paste0("data1_", selected_quarter)
if (!exists(object_name)) {
  stop("Object not found after loading: ", object_name)
}
data_group1 <- get(object_name)

if (!all(c("NQ", "SP") %in% colnames(data_group1))) {
  stop("Columns 'NQ' or 'SP' missing in data")
}

# ------------------------------------------------------------------------------
# 5) Pre‐processing
# ------------------------------------------------------------------------------
data_group1$NQ <- na.locf(data_group1$NQ, na.rm = FALSE)
data_group1$SP <- na.locf(data_group1$SP, na.rm = FALSE)

hhmm <- format(index(data_group1), "%H:%M")
data_group1[hhmm >= "09:31" & hhmm <= "09:40", ] <- NA
data_group1[hhmm >= "15:51" & hhmm <= "16:00", ] <- NA

# ------------------------------------------------------------------------------
# 6) NQ raw position with best parameters
# ------------------------------------------------------------------------------
NQ_ff <- na.locf(data_group1$NQ, na.rm = FALSE)
ema_fast_NQ <- EMA(NQ_ff, n = param_NQ$fast_ema)
ema_slow_NQ <- EMA(NQ_ff, n = param_NQ$slow_ema)
roll_std_NQ <- runsd(NQ_ff, param_NQ$vol_sd, endrule = "NA", align = "right")

posNQ_raw <- positionVB_new(
  signal   = ema_fast_NQ,
  lower    = ema_slow_NQ - param_NQ$m_ * roll_std_NQ,
  upper    = ema_slow_NQ + param_NQ$m_ * roll_std_NQ,
  pos_flat = rep(0, NROW(NQ_ff)),  # disable auto-flatten
  strategy = "mom"
)
posNQ_raw <- xts(posNQ_raw, order.by = index(data_group1))
posNQ_raw <- na.locf(posNQ_raw, na.rm = FALSE)

# Flatten after computing raw
posNQ <- flatten_times(posNQ_raw, allowed_start = "09:55", allowed_end = "15:40")

# ------------------------------------------------------------------------------
# 7) SP raw position with best parameters
# ------------------------------------------------------------------------------
SP_ff <- na.locf(data_group1$SP, na.rm = FALSE)
ema_fast_SP <- EMA(SP_ff, n = param_SP$fast_ema)
ema_slow_SP <- EMA(SP_ff, n = param_SP$slow_ema)
roll_std_SP <- runsd(SP_ff, param_SP$vol_sd, endrule = "NA", align = "right")

posSP_raw <- positionVB_new(
  signal   = ema_fast_SP,
  lower    = ema_slow_SP - param_SP$m_ * roll_std_SP,
  upper    = ema_slow_SP + param_SP$m_ * roll_std_SP,
  pos_flat = rep(0, NROW(SP_ff)),  # disable auto-flatten
  strategy = "mom"
)
posSP_raw <- xts(posSP_raw, order.by = index(data_group1))
posSP_raw <- na.locf(posSP_raw, na.rm = FALSE)

posSP <- flatten_times(posSP_raw, allowed_start = "09:55", allowed_end = "15:40")

# ------------------------------------------------------------------------------
# 8) Calculate Gross & Net PnL for each, then COMBINE
# ------------------------------------------------------------------------------
dNQ <- diff.xts(data_group1$NQ)
dSP <- diff.xts(data_group1$SP)
dNQ[is.na(dNQ)] <- 0
dSP[is.na(dSP)] <- 0

pnl_grossNQ <- posNQ * dNQ * contract_multiplier_NQ
ntransNQ    <- abs(diff.xts(posNQ))
ntransNQ[is.na(ntransNQ)] <- 0
pnl_netNQ   <- pnl_grossNQ - (ntransNQ * transaction_cost)

pnl_grossSP <- posSP * dSP * contract_multiplier_SP
ntransSP    <- abs(diff.xts(posSP))
ntransSP[is.na(ntransSP)] <- 0
pnl_netSP   <- pnl_grossSP - (ntransSP * transaction_cost)

pnl_gross_combined <- pnl_grossNQ + pnl_grossSP
pnl_net_combined   <- pnl_netNQ   + pnl_netSP

# ------------------------------------------------------------------------------
# 9) Aggregate to daily
# ------------------------------------------------------------------------------
day_ends <- endpoints(data_group1, on = "days")

daily_gross <- period.apply(pnl_gross_combined, INDEX = day_ends, FUN = sum, na.rm = TRUE)
daily_net   <- period.apply(pnl_net_combined,   INDEX = day_ends, FUN = sum, na.rm = TRUE)
daily_ntrans <- period.apply(ntransNQ + ntransSP, INDEX = day_ends, FUN = sum, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 10) Performance metrics on COMBINED approach
# ------------------------------------------------------------------------------
grossSR <- mySR(daily_gross, scale = scale_annual)
netSR   <- mySR(daily_net,   scale = scale_annual)

grossCR <- myCalmarRatio(daily_gross, scale = scale_annual)
netCR   <- myCalmarRatio(daily_net,   scale = scale_annual)

avg_ntrades <- mean(daily_ntrans, na.rm = TRUE)
grossPnL    <- sum(daily_gross, na.rm = TRUE)
netPnL      <- sum(daily_net,   na.rm = TRUE)

stat_ <- netCR * max(0, log(abs(netPnL / 1000)))

# ------------------------------------------------------------------------------
# 11) Print results
# ------------------------------------------------------------------------------
cat("\n=== Combined NQ+SP (with best param sets, updated flatten) ===\n")
cat("Gross SR:", grossSR, "\n")
cat("Net   SR:", netSR,   "\n")
cat("Gross CR:", grossCR, "\n")
cat("Net   CR:", netCR,   "\n")
cat("Average daily trades:", avg_ntrades, "\n")
cat("Gross PnL:", grossPnL, "\n")
cat("Net   PnL:", netPnL,   "\n")
cat("Final stat (example):", stat_, "\n")