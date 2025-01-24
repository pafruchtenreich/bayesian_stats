library(ggplot2)
library(reshape2)

correlation_matrix <- function(data) {
  numeric_data <- data[, .SD, .SDcols = sapply(data, is.numeric)]

  # Compute the correlation matrix
  correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

  # Convert the correlation matrix into a long format for ggplot
  correlation_long <- melt(correlation_matrix)

  # Plot the heatmap
  ggplot(correlation_long, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#009688",
      mid = "#F5F5F5",
      high = "#FF5722",
      midpoint = 0
    ) +
    theme_minimal() +
    labs(
      title = "",
      x = "Variables",
      y = "Variables",
      fill = "Correlation"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      axis.title.x = element_text(size = 24),
      axis.title.y = element_text(size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 24),
      panel.grid = element_blank(),
    )
}
