rm(list=ls())
setwd('/Users/richard/Desktop/Stat632_Fall25/Final')
df <- read.table("plasma.txt", sep = "")

library(broom)
library(dplyr)
library(tidyr)
library(ggplot2)

View(df)

colnames(df) <-  c("age", "sex", "smokestat", "quetelet",
                   "vituse", "calories", "fat", "fiber",
                   "alcohol", "cholesterol", "betadiet", "retdiet",
                   "betaplasma", "retplasma")

df$sex <- factor(df$sex)
df$smokestat <- factor(df$smokestat)
df$vituse <- factor(df$vituse)

View(df)

# fit model
mod <- lm(cbind(betaplasma, retplasma) ~ ., data=df)
modsum <- summary(mod)


# get coefs and cis, make plot 
# can edit later to get simultaneous conf ints, etc.
coef_mat <- coef(mod)           
ci_mat   <- confint(mod)         


ci_df <- ci_mat %>%
  as.data.frame() %>%
  mutate(row = rownames(ci_mat)) %>%
  separate(row, into = c("response", "term"), sep = "\\:") %>%
  dplyr::rename(conf.low  = `2.5 %`,
         conf.high = `97.5 %`)

coef_df <- coef_mat %>%
  as.data.frame() %>%
  mutate(term = rownames(coef_mat)) %>%
  pivot_longer(cols = -term, names_to = "response", values_to = "estimate")

plot_df <- left_join(coef_df, ci_df, by = c("term", "response"))

View(plot_df)
library(ggplot2)

plot_df <- plot_df %>%
  group_by(response) %>%
  mutate(term = reorder(term, abs(estimate))) %>%
  ungroup()

# if conf int contains 0, color red
plot_df <- plot_df %>%
  mutate(
    sig = ifelse(conf.low <= 0 & conf.high >= 0, "nonsig", "sig")
  )

# dropping intercept, too diffuclt to read with it
plot_df_no_intercept <- plot_df %>%
  filter(term != "(Intercept)")

ggplot(plot_df_no_intercept, aes(x = term, y = estimate, color = sig)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15) +  # color now comes from aes(color = sig)
  scale_color_manual(values = c("sig" = "blue1", "nonsig" = "red1")) +
  facet_wrap(~ response, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Model Coefficients with 95% CI",
    x = "Predictor",
    y = "Estimate",
    color = "Significance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
df


# cor plot (maybe beautify later)
library(corrplot)
corrplot(cor(df[, c("age", "quetelet", "calories", "fat", "fiber", "alcohol", "cholesterol",
                    "betadiet", "retdiet")]), method = "color")
corrplot(cor(df[, 13:14]), method = "ellipse")

# correlation btwn calories, fat, fiber, alcohol use, cholesterol, betadiet and retdiet
# cholesterol/alcohol fairly sig, 




# plot continuous predictors against responses. loops through predictors
library(patchwork)

View(df)
names(df)
for (pred in names(df)) {
  
  if (pred %in% c("sex", "smokestat", "vituse", "betaplasma",
                  "retplasma")) next

  p1 <- ggplot(df, aes_string(x = pred, y = df$betaplasma)) +
    geom_point(color = "lightslateblue", alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "burlywood2", linetype = "dashed") +
    labs(title = paste("betaplasma vs", pred, "\n", "model coef: ", round(mod$coef[rownames(mod$coefficients)==pred,1], 3)), x = pred, y = "betaplasma") +
    theme_minimal(base_size = 14)
  
  p2 <- ggplot(df, aes_string(x = pred, y = df$retplasma)) +
    geom_point(color = "plum4", alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "seagreen2", linetype = "dashed") +
    labs(title = paste("retplasma vs", pred, "\n", "model coef: ", round(mod$coef[rownames(mod$coefficients)==pred,2], 3)), x = pred, y = "retplasma") +
    theme_minimal(base_size = 14)
  
  # Show side-by-side
  print(p1 | p2)
}


ggplot(df, aes(x = sex)) +
  geom_bar(fill = "darksalmon", color = "black", width = 0.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Sexes",
       x = "sex",
       y = "Count")

ggplot(df, aes(x = smokestat)) +
  geom_bar(fill = "darkorchid2", color = "black", width = 0.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Smoking Status",
       x = "smokestat",
       y = "Count")

ggplot(df, aes(x = vituse)) +
  geom_bar(fill = "deepskyblue2", color = "black", width = 0.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Vitamin Use",
       x = "vituse",
       y = "Count")


# test subsets of predictors

responses <- c("betaplasma", "retplasma")
predictors <- colnames(df)[1:12]


# test type choices : Pillai, Wilks, Roy, Hotelling-Lawley
get_stats <- function(vars, test_type = "Pillai") {
  
  if (length(vars) == 0) {
    return(data.frame(
      predictors = "",
      num_predictors = 0,
      F_value = NA,
      p_value = NA
    ))
  }
  
  form <- as.formula(
    paste0("cbind(", paste(responses, collapse = ", "), ") ~ ",
           paste(vars, collapse = " + "))
  )
  
  fit <- manova(form, data = df)
  
  # Can change test if desired
  sm <- summary(fit, test = test_type)
  
  # Extract F-statistic and p-value
  F_val <- sm$stats[1, "approx F"]
  p_val <- sm$stats[1, "Pr(>F)"]
  
  data.frame(
    predictors = paste(vars, collapse = ","),
    num_predictors = length(vars),
    F_value = F_val,
    p_value = p_val
  )
}

# Get all subsets of predictors
all_subsets <- unlist(
  lapply(0:length(predictors), function(k)
    combn(predictors, k, simplify = FALSE)
  ),
  recursive = FALSE
)

# Compute results
results_pillai <- do.call(rbind, lapply(all_subsets, get_stats, test_type = "Pillai"))

head(results_pillai)
sum(results_pillai$p_value < 0.05, na.rm = TRUE)

alpha <- 0.05

prop_sig_pillai <- aggregate(
  p_value ~ num_predictors,
  data = results_pillai,
  FUN = function(p) mean(p < alpha, na.rm = TRUE)
)

prop_sig_pillai <- prop_sig_pillai %>%
  mutate(test = "pillai")

# wilks

results_wilks <- do.call(rbind, lapply(all_subsets, get_stats, test_type = "Wilks"))

head(results_wilks)
sum(results_wilks$p_value < 0.05, na.rm = TRUE)

alpha <- 0.05

prop_sig_wilks <- aggregate(
  p_value ~ num_predictors,
  data = results_wilks,
  FUN = function(p) mean(p < alpha, na.rm = TRUE)
)

prop_sig_wilks <- prop_sig_wilks %>%
  mutate(test = "wilks")

# hotelling

results_hotelling_lawley <- do.call(rbind, lapply(all_subsets, get_stats, test_type = "Hotelling-Lawley"))

head(results_hotelling_lawley)
sum(results_hotelling_lawley$p_value < 0.05, na.rm = TRUE)

alpha <- 0.05

prop_sig_hotelling_lawley <- aggregate(
  p_value ~ num_predictors,
  data = results_hotelling_lawley,
  FUN = function(p) mean(p < alpha, na.rm = TRUE)
)

prop_sig_hotelling_lawley <- prop_sig_hotelling_lawley %>%
  mutate(test = "hotelling")

# roy 
results_roy <- do.call(rbind, lapply(all_subsets, get_stats, test_type = "Roy"))

head(results_roy)
sum(results_roy$p_value < 0.05, na.rm = TRUE)

alpha <- 0.05

prop_sig_roy <- aggregate(
  p_value ~ num_predictors,
  data = results_roy,
  FUN = function(p) mean(p < alpha, na.rm = TRUE)
)

prop_sig_roy <- prop_sig_roy %>%
  mutate(test = "roy")

# All tests produce the same proportion of significant subsets for each num predictors (sample size is larger, makes sense. Small discrepancies in F vals)

library(ggplot2)

ggplot(prop_sig_pillai, aes(x = num_predictors, y = p_value)) +
  geom_line(size = 1, color = "steelblue2") +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Significant Subsets",
    x = "Number of Predictors",
    y = "Proportion Significant"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )



