# Bayesian statistics

This project is part of the lecture **Bayesian Statistics** given at ENSAE Paris during 2024/2025. We aim to explore the techniques developed in **Banbura, M., Giannone, D. and L. Reichlin (2010). Large Bayesian Vector Autoregressions, Journal of Applied econometrics, Vol. 25, 71 – 92** (you can find this article [here](https://onlinelibrary.wiley.com/doi/abs/10.1002/jae.1137) and downloadable [here](https://papers.ssrn.com/sol3/Delivery.cfm?abstractid=1292332)). Especially, we apply both a BVAR and a large BVAR on a new macroeconomic dataset with many columns and we assess their performances when it comes to forecasting. 

In practice, it has been seen that BVARs overfit when the number of features exceeds 10. The need for larger models led to the creation of LBVARs where, by controlling the shrinkage in the prior, one can fit a BVAR on larger datasets without overfitting.

In this project, we compare the forecasting performance of BVAR and LBVAR when the number of features increase.
