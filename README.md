

# Quantitative Strategies on High Frequency Data

## 📄 Description
This project explores quantitative trading strategies using high-frequency financial data. It aims to understand market dynamics through statistical analysis and algorithmic modeling, leveraging the nuances of tick-level order book data to develop profitable and risk-aware trading strategies.

---

## 📊 Data
- **Source:** High-frequency data comprising Level II order book snapshots.
- **Content:** Includes bid-ask prices, order volumes, mid-price movements, and time-stamped trade events.
- **Preprocessing:** Data was cleaned, normalized, and transformed into features such as mid-price changes, rolling spreads, and imbalance metrics.

---

## 🎯 Objective
- To design and evaluate trading strategies based on high-frequency data analysis.
- To predict short-term price movements and detect arbitrage opportunities.
- To balance profit potential with risk exposure using various backtesting metrics.

---

## 🔧 Methodology
1. **Feature Engineering:**
   - Mid-price computation
   - Spread and depth-based indicators
   - Order book imbalance metrics

2. **Strategy Design:**
   - Momentum-based strategy
   - Mean-reversion models
   - Imbalance threshold-based decision rules

3. **Backtesting Framework:**
   - Simulated trade execution using realistic latency
   - PnL, Sharpe Ratio, and hit ratio evaluation
   - Time-windowed evaluation and walk-forward testing

---

## 📈 Results
- **Performance Metrics:** 
  - Sharpe Ratios ranged from *X* to *Y* depending on the strategy.
  - Certain strategies exhibited high win rates but low risk-adjusted returns, highlighting the trade-off between aggressiveness and consistency.

- **Profit Curves:** 
  - Illustrated cumulative PnL over the test period, showing clearer outperformance in momentum strategies under volatile market conditions.

- **Drawdown Analysis:** 
  - Periods of strategy underperformance were correlated with market microstructure anomalies.

---

## 🧠 Strategies
| Strategy Name       | Core Idea                     | Key Parameter       | Result Summary             |
|---------------------|-------------------------------|---------------------|----------------------------|
| Momentum Strategy   | Price continues direction     | Lag window          | High PnL, moderate risk    |
| Mean-Reversion      | Reversal to average expected  | Lookback period     | Stable returns, lower PnL |
| Imbalance Trading   | Use order book signals        | Imbalance threshold | Effective in trending days |

---

## ✅ Conclusion
This project demonstrates the feasibility of applying quantitative methods to high-frequency financial data for real-time trading. Strategy effectiveness depends heavily on market conditions, choice of features, and execution latency. Adaptive strategies that combine multiple signals show the most promise.

---

## 📸 Pictures and Diagrams
Include these visuals in your final report or documentation:
- 📉 Mid-price movement vs. predicted signal chart
- 📊 Bar plots comparing strategy returns
- 🧮 Heatmaps showing correlation among features
- ⏱️ Timeline showing latency vs. execution success

---

## 🌍 Implications
- **Academic:** Provides a framework for further research in market microstructure and predictive modeling.
- **Industry:** Demonstrates how systematic trading models can be tuned for intraday profitability.
- **Ethical:** Highlights the importance of fair access to infrastructure in high-frequency trading.
