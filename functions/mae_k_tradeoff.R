mae_k_tradeoff <- function(ki, pp_recipe, df, df_train, df_test){

  kstep_df_train <- data.frame(k = rep(NA, length(ki)), mae = NA, sample = "train")
  kstep_df_test <- data.frame(k = rep(NA, length(ki)), mae = NA, sample = "test")

  for(kstep in ki){

    # Fit KNN model
    mod_knn <- caret::train(
      pp_recipe,
      data = df,
      method = "knn",
      trControl = caret::trainControl(method = "none"),
      tuneGrid = data.frame(k = kstep),
      metric = "RMSE"
    )

    # add predictions to the data frames
    df_train_new <- df_train |>
      drop_na()
    df_train_new$fitted <- predict(mod_knn, newdata = df_train_new)

    df_test_new <- df_test |>
      drop_na()
    df_test_new$fitted <- predict(mod_knn, newdata = df_test_new)

    # get metrics tables
    metrics_train <- df_train_new |>
      yardstick::metrics(GPP_NT_VUT_REF, fitted)

    metrics_test <- df_test_new |>
      yardstick::metrics(GPP_NT_VUT_REF, fitted)

    # extract values from metrics tables
    mae_train <- metrics_train |>
      filter(.metric == "mae") |>
      pull(.estimate)
    mae_test <- metrics_test |>
      filter(.metric == "mae") |>
      pull(.estimate)

    kstep_df_train[kstep, ]$k <- kstep
    kstep_df_train[kstep, ]$mae <- mae_train
    kstep_df_test[kstep, ]$k <- kstep
    kstep_df_test[kstep, ]$mae <- mae_test

  }

  kstep_df_score <- kstep_df_test
  kstep_df_score$sample <- "score"
  kstep_df_score$mae <- kstep_df_test$mae + abs(kstep_df_test$mae - kstep_df_train$mae)

  get_first_local_min <- function(scores) {

    for (i in 2:(length(scores) - 1)) {

      if (scores[i] < scores[i - 1] && scores[i] < scores[i + 1]) {

        return(i)

      }

    }
    return(which.min(scores)) # if there is no local
  }

  best_index <- get_first_local_min(kstep_df_score$mae)
  best_k <- kstep_df_score$k[best_index]

  return(list(
    mae_k = rbind(kstep_df_train, kstep_df_test),
    best_k = best_k
  ))

}

