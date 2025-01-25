library(zoo)
library(BVAR)
library(data.table)

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
    tail(actual_zoo, 12 * 6),
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

process_predictions <- function(predictions, col_names) {
  # Extract the specified quantile array
  predictions <- predictions$quants["50%", , ]

  # Convert the array to a data.table
  dt <- as.data.table(predictions)

  # Set the column names
  setnames(dt, col_names)

  # Return the processed data.table
  return(dt)
}

fit_bvar_model <- function(data, vars, lags) {
  BVAR::bvar(
    data[, .SD, .SDcols = vars][3:.N],
    lags = lags
  )
}

compute_bic_from_bvar <- function(model) {
  loglik_value <- logLik(model)
  k <- model$meta$K
  n <- model$meta$M * 251

  bic_value <- as.numeric(-2 * loglik_value + k * log(n))
  return(bic_value)
}


compute_mse <- function(results) {
  pred <- results$predictions
  true <- results$true_values

  errors <- true - pred
  MSE <- round(mean(errors^2), digits = 3)
  return(MSE)
}


evaluate_bvar_models <- function(all_model_vars,
                                 training_data,
                                 zoo_data,
                                 lag_candidates,
                                 diffCount,
                                 base_val) {
  # A list to store the final results
  mse_results <- list()

  for (model_name in names(all_model_vars)) {
    cat("\n----------\nProcessing:", model_name, "\n")

    # Extract the variables for this model
    vars <- all_model_vars[[model_name]]

    best_bic <- Inf
    best_lags <- NA

    # Search over possible lags by bic
    for (p in lag_candidates) {
      # Fit BVAR model with p lags
      temp_mod <- try(fit_bvar_model(training_data, vars, p), silent = TRUE)

      # Check if the model fit encountered an error:
      if (inherits(temp_mod, "try-error")) {
        cat("   Model failed at lag", p, " --> Skipping further lags for", model_name, "\n")
        break
      }

      # Compute the bic
      current_bic <- compute_bic_from_bvar(temp_mod)

      # Update best bic and best lag if this is the lowest bic so far
      if (!is.na(current_bic) && (current_bic < best_bic)) {
        best_bic <- current_bic
        best_lags <- p
      }
    }

    cat("   Best lag order for", model_name, "=", best_lags, "with bic =", best_bic, "\n")

    # Fit the final model at the best lag order
    final_model <- fit_bvar_model(training_data, vars, best_lags)

    # Predict out-of-sample (horizon = 12 in your example)
    predictions <- predict(final_model, horizon = 12)

    # Process the predictions
    predictions_proc <- process_predictions(predictions, vars)

    # Generate forecast plots and get forecasted series
    forecast_obj <- forecast_plot(
      zoo_data,
      predictions_proc,
      variable    = "Unemployment_Rate",
      diffCount   = diffCount,
      base_values = base_val,
      horizon     = 12
    )

    # Compute MSE
    mse_val <- compute_mse(forecast_obj)

    cat("   MSE for", model_name, "with", best_lags, "lags =", mse_val, "\n")

    # Store the results
    mse_results[[model_name]] <- list(
      best_lags = best_lags,
      bic = best_bic,
      MSE = mse_val
    )
  }

  # Print the final results
  print(mse_results)

  # Return them for further processing
  return(mse_results)
}

evaluate_large_bvar_models <- function(training_data, model_parameters, zoo_data, diffCount, base_val, horizon = 12) {
  results_list <- list()

  for (model_name in names(model_parameters)) {
    # Extract model parameters
    model_info <- model_parameters[[model_name]]
    variables <- model_info$variables
    best_lags <- model_info$best_lags

    # lambda <- lbvar::fitLambda(training_data[, .SD, .SDcols = !("Date")][3:.N],
    #   variables = variables,
    #   lambdaseq = seq(0, 1, 0.005),
    #   p = best_lags, p.reduced = best_lags
    # )
    # cat("Lambda", model_name, "=", lambda * 2)

    # Fit the BVAR model
    model_large_bvar <- lbvar::lbvar(
      training_data[, .SD, .SDcols = variables],
      p = best_lags,
      delta = 0,
      lambda = 1 / length(variables)
    )

    # Generate predictions
    predictions_large_bvar <- data.table(predict(model_large_bvar, h = horizon))

    # Evaluate results
    results_large_bvar <- forecast_plot(
      zoo_data,
      predictions_large_bvar,
      variable = "Unemployment_Rate",
      diffCount = diffCount,
      base_values = base_val,
      horizon = horizon
    )
    mse <- compute_mse(results_large_bvar)

    # Store the result
    results_list[[model_name]] <- list(
      predictions = predictions_large_bvar,
      results = results_large_bvar,
      mse = mse
    )

    # Print progress
    print(paste("Completed model:", model_name, "with MSE:", mse))
  }

  return(results_list)
}
