rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)

flux_data <- read_csv("data-raw/df_for_stepwise_regression.csv")

# removing gaps using the complete case analysis approach
flux_data <- flux_data[complete.cases(flux_data), ]

# checking the type of data
# str(flux_data)
# unique(flux_data$siteid)

# clean data
flux_data <- flux_data[, -c(1, 2)]


predictors <- setdiff(names(flux_data), "GPP_NT_VUT_REF")

biv_res <- data.frame(predictor = character(), R2 = numeric(), AIC = numeric())

for (var in predictors) {
  model <- lm(as.formula(paste("GPP_NT_VUT_REF ~", var)), data = flux_data)
  r2 <- summary(model)$r.squared
  aic <- AIC(model)
  biv_res <- rbind(biv_res, data.frame(predictor = var, R2 = r2, AIC = aic))
}

biv_res <- biv_res[order(biv_res$R2, decreasing = TRUE), ]
biv_res$predictor <- factor(biv_res$predictor, levels = biv_res$predictor)
biv_res_tidy <- tidyr::pivot_longer(
  data = biv_res,
  cols = c("R2", "AIC"),
  names_to = "Metric",
  values_to = "Value")

ggplot(biv_res_tidy, aes(x = predictor, y = Value)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Metric, scale = "free_x") +
  xlab("Predictor") + ylab("Metric") +
  theme_bw()


flux_data <- flux_data[, -match(c("TA_F_MDS", "SW_IN_F_MDS", "LW_IN_F_MDS", "VPD_F_MDS"), colnames(flux_data))]

# Setting initial step: GPP only with intercept
current_predictors <- c()
remaining_predictors <- setdiff(names(flux_data), "GPP_NT_VUT_REF")
best_model <- lm(GPP_NT_VUT_REF ~ 1, data = flux_data)
best_aic <- AIC(best_model)
# df where is stored the results
aic_df <- data.frame(step = 0, predictor = "Intercept", AIC = best_aic)

max_step <- ncol(flux_data) - 1 # limit the number of predictors

# # Loop
# repeat {
#   test_results <- data.frame(predictor = character(), aic = numeric(), formula = character())
#
#   # loop over each model
#   for (var in remaining_predictors) {
#     candidate_formula <- as.formula(paste("GPP_NT_VUT_REF ~", paste(c(current_predictors, var), collapse = " + ")))
#     model <- lm(candidate_formula, data = flux_data)
#     test_results <- rbind(test_results, data.frame(
#       predictor = var,
#       aic = AIC(model),
#       formula = deparse(candidate_formula)
#     ))
#   }
#
#   best_candidate <- test_results %>% arrange(aic) %>% slice(1)
#
#   # condition based on best aic
#   if (best_candidate$aic >= best_aic) break
#
#   # update initial step + 1
#   current_predictors <- c(current_predictors, best_candidate$predictor)  # ensuring model with the best previous
#   remaining_predictors <- setdiff(remaining_predictors, best_candidate$predictor) # predictor
#   best_aic <- best_candidate$aic
#   aic_df <- rbind(aic_df, data.frame(
#     step = step,
#     predictor = best_candidate$predictor,
#     AIC = best_aic
#   ))
#
#   step <- step + 1
#   # condition based on max steps
#   if (step > max_step) break
# }


for (step in 1:max_step){

  test_results <- data.frame(predictor = character(), aic = numeric(), formula = character())

  # loop over each model
  for (var in remaining_predictors) {
    candidate_formula <- as.formula(paste("GPP_NT_VUT_REF ~", paste(c(current_predictors, var), collapse = " + ")))
    model <- lm(candidate_formula, data = flux_data)
    test_results <- rbind(test_results, data.frame(
      predictor = var,
      aic = AIC(model),
      formula = deparse(candidate_formula)
    ))
  }

  best_candidate <- test_results %>% arrange(aic) %>% slice(1)

  # condition based on best aic
  if (best_candidate$aic >= best_aic) break

  # update initial step
  current_predictors <- c(current_predictors, best_candidate$predictor)  # ensuring model with the best previous
  remaining_predictors <- setdiff(remaining_predictors, best_candidate$predictor) # predictor
  best_aic <- best_candidate$aic
  aic_df <- rbind(aic_df, data.frame(
    step = step,
    predictor = best_candidate$predictor,
    AIC = best_aic
  ))

}



aic_df$predictor <- factor(aic_df$predictor, levels = aic_df$predictor)

ggplot(aic_df, aes(x = step, y = AIC)) +
  geom_point(shape = 19) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 10, 1),
                     sec.axis = dup_axis(name = "predictor", labels = aic_df$predictor)) +
  theme_bw()
