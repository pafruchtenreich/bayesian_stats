library(zoo)

forecast_plot <- function(zoo_data, model_predictions, variable, diffCount, base_values, horizon = 12) {
  actual_zoo <- zoo(zoo_data[, variable], order.by = index(zoo_data))

  dcount <- diffCount[variable]
  base_val <- base_values[[variable]]

  if (dcount == 1) {
    # actual_zoo[t] represents y[t] - y[t-1].
    # Use base_val[1] as the initial anchor.
    anchor_level <- base_val[1]

    temp_data <- coredata(actual_zoo)
    temp_data[is.na(temp_data)] <- 0

    # Invert the differencing: partial sum + anchor
    rebuilt_levels <- anchor_level + cumsum(temp_data)

    # Overwrite the zoo series with these reconstituted levels
    actual_zoo <- zoo(rebuilt_levels, order.by = index(actual_zoo))
  } else if (dcount == 2) {
    # actual_zoo[t] represents (y[t] - y[t-1]) - (y[t-1] - y[t-2])
    # base_val[1] = y[1]
    # base_val[2] = y[2]

    y1 <- base_val[1]
    y2 <- base_val[2]
    d12 <- y2 - y1

    temp_data <- coredata(actual_zoo)
    temp_data[is.na(temp_data)] <- 0

    # Invert 2nd diffs -> 1st diffs
    first_diff <- d12 + cumsum(temp_data)

    # Invert 1st diffs -> levels
    rebuilt_levels <- y2 + cumsum(first_diff)

    actual_zoo <- zoo(rebuilt_levels, order.by = index(actual_zoo))
  }

  pred_dates <- tail(index(zoo_data), horizon)
  model_predictions$Date <- pred_dates

  # Convert predicted differences to zoo
  predictions_zoo <- zoo(model_predictions[[variable]], order.by = model_predictions$Date)

  # Extract the base values from the tail
  base_val <- tail(actual_zoo, horizon + 2)[1:2]

  # Reconstitute the predictions to levels
  if (dcount == 1) {
    # One difference => base_val[2] is the last actual levels
    anchor_level <- coredata(base_val)[2]

    # predictions_zoo holds Δy forecasts
    pred_levels <- anchor_level + cumsum(coredata(predictions_zoo))

    # Overwrite the predictions_zoo in levels
    predictions_zoo <- zoo(pred_levels, order.by = index(predictions_zoo))
  } else if (dcount == 2) {
    # Two differences => base_val[1] & base_val[2] are the last two actual levels
    y_T_minus_1 <- coredata(base_val)[1]
    y_T <- coredata(base_val)[2]

    # The last known first-diff
    d_T <- y_T - y_T_minus_1

    # Invert second diffs => first diffs
    pred_first_diff <- d_T + cumsum(coredata(predictions_zoo))

    # Invert first diffs => levels
    pred_levels <- y_T + cumsum(pred_first_diff)

    # Overwrite in levels
    predictions_zoo <- zoo(pred_levels, order.by = index(predictions_zoo))
  }

  plot(
    actual_zoo,
    type = "l",
    col = "#1f77b4",
    lwd = 2,
    main = "",
    xlab = "Date",
    ylab = variable,
    ylim = range(actual_zoo, na.rm = TRUE)
  )

  lines(predictions_zoo, col = "#d62728", lwd = 3)

  legend(
    "topleft",
    legend = c("Actual", "Prediction"),
    col = c("#1f77b4", "#d62728"),
    lty = c(1, 1),
    pch = c(NA, 19),
    lwd = 3,
    bty = "n"
  )

  return(list(
    predictions = predictions_zoo,
    true_values = tail(actual_zoo, 12)
  ))
}
