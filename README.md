# Bayesian Statistics 🚀

This repository supports the **Bayesian Statistics** lecture at ENSAE Paris (2024–2025). We build upon the techniques introduced by [Banbura, Giannone & Reichlin (2010)](https://onlinelibrary.wiley.com/doi/abs/10.1002/jae.1137) ([downloadable PDF](https://papers.ssrn.com/sol3/Delivery.cfm?abstractid=1292332)), focusing on **Bayesian Vector Autoregressions (BVARs)** and their high-dimensional extension, **Large Bayesian Vector Autoregressions (LBVARs)**.

## Motivation 
- **BVAR** models tend to overfit when the number of variables exceeds 10.
- **LBVAR** addresses this issue by incorporating adaptive shrinkage, enabling reliable estimation even in large-dimensional settings.

## What This Project Offers
- **Implementation:** We provide code to estimate both BVAR and LBVAR models.
- **Macroeconomic Dataset:** We apply these models to an extensive macroeconomic dataset.
- **Performance Comparison:** We evaluate how each model’s forecast accuracy evolves as the number of variables grows.

## Key Takeaways
1. **Scalability:** LBVAR can handle larger datasets without succumbing to overfitting.
2. **Forecasting Power:** LBVAR generally delivers more robust predictions, especially in high-dimensional scenarios.
