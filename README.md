# Steam Player Forecasting with Time Series Models

An time series forecasting project that analyzes historical Steam player activity for multiple video games using classical statistical forecasting and diffusion models implemented in R.

---

## Overview

This project investigates player population trends for three popular Steam games:

- Baldur's Gate 3
- Elden Ring
- Sid Meier's Civilization VI

Historical player counts and pricing data collected from SteamDB are analyzed to understand player adoption, market diffusion, and long-term player retention. Multiple forecasting approaches are compared to model player dynamics and predict future trends.

---

## Objectives

- Analyze player population changes over time
- Investigate the impact of price changes on player activity
- Compare multiple forecasting methods
- Model game adoption using diffusion theory
- Predict future player trends

---

## Methods

### Statistical Forecasting
- ARIMA
- SARIMAX
- Prophet

### Diffusion Models
- Bass Model (BM)
- Generalized Bass Model (GBM)

---

## Technologies

- R
- Prophet
- Forecast
- DIMORA
- Tseries
- Readxl

---

## Repository Structure

```text
steam-player-forecasting/
│
├── README.md
├── requirements.txt
├── Report.pdf
│
├── code/
│   ├── BG3.R
│   ├── CV6.R
│   └── EDR.R
│
└── data/
    ├── Baldur's Gate 3_Players.xls
    ├── Baldur's Gate 3_Price.xlsx
    ├── Elden Ring Players.xls
    ├── EDR Price.xlsx
    └── Sid Meier's Civilization VI.xlsx
```

---

## Workflow

1. Import SteamDB datasets
2. Data preprocessing
3. Exploratory data analysis
4. Bass / Generalized Bass Model fitting
5. Prophet forecasting
6. ARIMA and SARIMAX modelling
7. Model comparison
8. Forecast visualization

---

## Skills Demonstrated

- Time Series Forecasting
- Statistical Modelling
- Predictive Analytics
- Diffusion Models
- Data Visualization
- Exploratory Data Analysis
- R Programming

---

## Future Improvements

- Compare statistical models with deep learning approaches (e.g., LSTM)
- Build an interactive Shiny dashboard
- Automate data collection using the Steam Web API
- Evaluate additional forecasting models

---

## Author

**Shiyu Min**
