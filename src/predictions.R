library(ggplot2)
library(data.table)
library(lubridate)

forecast_plot <- function(data, model_predictions, variable) {
  model_predictions$Date <- data[order(-Date)][1:3]$Date
  model_predictions$Type <- "Prediction"

  data$Type <- "Actual"

  time_series_data <- rbind(
    data[, .(Date, Value = get(variable), Type)],
    model_predictions[, .(Date, Value = get(variable), Type)]
  )

  plot <- ggplot(time_series_data, aes(x = Date, y = Value, color = Type)) +
    geom_line(size = 1.2) +
    geom_point(data = model_predictions, aes(x = Date, y = get(variable)), color = "#d62728", size = 3) +
    scale_color_manual(values = c("Actual" = "#1f77b4", "Prediction" = "#d62728")) +
    theme_minimal() +
    labs(
      title = "",
      x = "Date",
      y = paste(variable),
      color = "Legend"
    ) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray80")
    )

  return(plot)
}
