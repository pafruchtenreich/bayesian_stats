library(ggplot2)
library(reshape2)
library(stargazer)

summary_statistics <- function(data, latex = FALSE) {
  data <- data[, .SD, .SDcols = sapply(data, is.numeric)]

  # Compute summary statistics
  summary_table <- data[, .(
    Mean = sapply(.SD, mean, na.rm = TRUE),
    Std_Dev = sapply(.SD, sd, na.rm = TRUE),
    Min = sapply(.SD, min, na.rm = TRUE),
    Max = sapply(.SD, max, na.rm = TRUE)
  )]

  # Add variable names
  summary_table <- data.table(Variable = names(data), summary_table)

  # Print the table
  if (latex) {
    stargazer(summary_table, summary = FALSE, type = "latex", title = "Summary Statistics", digits = 1)
  } else {
    stargazer(summary_table, summary = FALSE, type = "text", title = "Summary Statistics", digits = 1)
  }
}

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
