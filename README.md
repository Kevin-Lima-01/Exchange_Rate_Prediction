# Exchange Rate Forecasting: BRL/USD Real and Nominal Models

This repository contains the final project and supporting codes developed as part of an **International Finance and Crises** course. The study develops two forecasting models for the **Brazilian Real against the US Dollar (BRL/USD)** , covering both **nominal** and **real exchange rates**. The methodology builds on the modeling framework of Paiva (2006), discussed in class, and incorporates scenario simulations under alternative economic conditions.

## Research Objective

The primary goal is to construct predictive models for the BRL/USD exchange rate using a broad set of macroeconomic and financial variables. The analysis distinguishes between nominal and real exchange rate dynamics, recognizing that different fundamentals drive each series. Additionally, the project simulates alternative economic scenarios — including relevant market shocks — to assess model robustness and forecasting performance.

## Variables and Model Structure

### Nominal Exchange Rate Model

The nominal BRL/USD model incorporates the following variables:

| Variable | Description |
|----------|-------------|
| **CRB** | Commodity Research Bureau index — basket of commodity prices |
| **VIX** | CBOE Volatility Index — global risk aversion proxy |
| **CDS** | Credit Default Swap — Brazil sovereign risk measure |
| **DXY** | US Dollar Index — dollar strength against major currencies |
| **FIN** | Financial Uncertainty Index |
| **SELIC** | Brazilian policy interest rate |
| **FED** | US Federal Funds rate |
| **Interest Rate Differential** | SELIC minus FED rate |

### Real Exchange Rate Model

The real BRL/USD model incorporates:

| Variable | Description |
|----------|-------------|
| **VIX** | Global risk aversion proxy |
| **TOTS** | Terms of Trade index |
| **PASSEXTERNO** | External liabilities position |
| **SWAP Real** | Real interest rate swap differential |
| **DIV** | External debt indicators |
| **Ibovespa** | Quarterly performance of the Brazilian stock market |
| **S&P 500** | Quarterly performance of the US stock market |

## Methodology

The project follows the modeling framework of **Paiva (2006)** , adapted and extended for the current economic context. The approach involves:

1. **Variable Selection:** Identifying macroeconomic and financial drivers of exchange rate movements.
2. **Model Estimation:** Estimating nominal and real exchange rate models in **R**.
3. **Scenario Simulation:** Constructing alternative economic scenarios and relevant exchange market shocks in **Excel**.
4. **Forecasting:** Generating out-of-sample predictions and evaluating model performance across scenarios.

## Tools & Software

- **R:**
  - `tidyverse` (`dplyr`, `ggplot2`, `tidyr`) — data manipulation and visualization
  - `lm` / `dynlm` — linear and dynamic linear models
  - Additional packages as needed for data acquisition and diagnostic testing

- **Microsoft Excel:**
  - Scenario simulation engine
  - Shock propagation analysis
  - Supplementary forecasting outputs

