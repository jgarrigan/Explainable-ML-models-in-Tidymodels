# LOAD LIBRARIES
pacman::p_load(tidyverse, tidymodels, hstats, kernelshap, shapviz, patchwork, doParallel, finetune, vip, GGally, themis, gt, DALEX, DALEXtra, iBreakDown)

# READ DATA
df0 <- read_csv("C:/Users/John/Downloads/diabetes_prediction_dataset.csv/diabetes_prediction_dataset.csv")

summary(df0)

# Calculate the table with smoking history counts
smoking_table <- df0 %>% 
  count(smoking_history) %>% 
  rename(Count = n)

# Create a gt table
smoking_table %>% 
  gt() %>% 
  tab_header(
    title = "Smoking History Distribution" 
  ) %>%
  cols_label(
    smoking_history = "Smoking History"
  ) 

df0 %>%
  filter(duplicated(.)) %>%
  nrow()

df1 <- df0 %>%
  mutate(
    y = factor(diabetes, levels = 0:1, labels = c("No", "Yes")),
    female = (gender == "Female") * 1,
    smoking_history = factor(
      smoking_history,
      levels = c("No Info", "never", "former", "not current", "current", "ever")
    ),
    bmi_category = case_when(
      bmi < 18.5 ~ "UNDERWEIGHT",
      bmi >= 18.5 & bmi < 25 ~ "NORMAL_WEIGHT",
      bmi >= 25 & bmi < 30 ~ "OVERWEIGHT",
      bmi >= 30 ~ "OBESE",
      TRUE ~ "UNKNOWN"
    ),
    diabetes_type = case_when(
      HbA1c_level < 5.7 ~ "NORMAL",
      HbA1c_level >= 5.7 & HbA1c_level <= 6.4 ~ "PRE-DIABETES",
      HbA1c_level >= 6.5 ~ "DIABETES"
    ),
    age_group = case_when(
      age >= 18 & age <= 39 ~ "YOUNG_ADULTS",
      age >= 40 & age <= 64 ~ "MIDDLE_AGED",
      age >= 65 ~ "OLDER_ADULTS",
      TRUE ~ "UNKNOWN"
  )) %>%
  filter(gender != "Other")

df1 %>%
  slice_sample(n = 5000) %>%
  select(where(is.numeric),y) %>% 
  ggpairs(columns = 1:8, aes(color = y, alpha = 0.5))

# BAR PLOT OF DIABETES
ggplot(df1, aes(x = as.factor(diabetes), fill = y)) + # Convert diabetes to a factor
  geom_bar() +
  labs(
    title = "Diabetes Distribution",
    x = "Diabetes Status",
    y = "Count"
  ) +
  theme_minimal() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# BAR PLOT OF GENDER
ggplot(df1, aes(x = as.factor(gender), fill = gender)) + # Convert diabetes to a factor
  geom_bar() +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# HISTOGRAM BY AGE
ggplot(df1, aes(x = age, fill = gender, color = gender)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  theme(legend.position = "bottom") +
  labs(title = "Distribution of Age for Males and Females") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df1, aes(x = gender, fill = y)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(fill = "Diabetes") +  # Rename legend title
  theme(legend.position = "bottom") +
  labs(title = "Diabetes Prevalence by Gender",
       x = "") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df1, aes(x = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")), fill = y)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(
    title = "Diabetes Distribution by Hypertension",
    x = "Hypertension",
    y = "Count",
    fill = "Diabetes"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df1, aes(x = y, y = blood_glucose_level, fill = y)) +
  geom_boxplot() +
  labs(
    x = "Diabetes",
    y = "Blood Glucose Level (mmol/L)",
    title = "Blood Glucose Level By Diabetes Status",
    fill = "Diabetes"
  ) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

# HISTOGRAMS OF NUMERIC VARIABLES
df1 %>%
  select(age, bmi, HbA1c_level, blood_glucose_level) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value, fill = name)) +  # Map 'name' to the fill aesthetic
  geom_histogram(bins = 19) +       # Remove the fixed fill color
  facet_wrap(~name, scales = "free") + 
  scale_fill_discrete(name = "Variable") +  # Set legend title
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  labs(x = "",
       title = "Distribution of Age, BMI, HbA1c, and Blood Glucose")

# Calculate the order of bars based on count
smoking_order <- df1 %>%
  count(smoking_history) %>%
  arrange(desc(n)) %>%
  pull(smoking_history)

# BAR PLOT OF SMOKING HISTORY
ggplot(df1, aes(x = factor(smoking_history, levels = smoking_order), fill = smoking_history)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Smoking History Distribution",
    x = "Smoking History",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none", plot.title = element_text(hjust = 0.5))

df1 %>%
  filter(smoking_history == "No Info") %>%
  ggplot(aes(x = y, fill = y)) +
  geom_bar() +
  labs(
    title = "Diabetes Distribution for 'No Info' Smoking History",
    x = "Diabetes",
    y = "Count"
  ) +
  theme_minimal() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme(legend.position = "none")

cor_matrix <- df1 %>%
  mutate(
    smoking_history = case_when(
      smoking_history == "No Info" ~ 0,
      smoking_history == "never" ~ 0,
      smoking_history == "former" ~ 1,
      smoking_history == "current" ~ 1,
      smoking_history == "not current" ~ 1,
      smoking_history == "ever" ~ 1
    ),
    gender = case_when(
      gender == "Female" ~ 0,
      gender == "Other" ~ 0,
      gender == "Male" ~ 1
    )
  ) %>%
  select(
    where(is.numeric),
    -female
  ) %>%
  cor()

# Melt the correlation matrix
melted_cor_matrix <- reshape2::melt(cor_matrix)

ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = "blue",
    high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, vjust = 1,
      size = 12, hjust = 1
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right", # Place legend on the right
    legend.justification = "top" # Align legend to the top
  ) +
  coord_fixed() +
  labs(title = "Correlation Heatmap") +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10))

skimr::skim(df1)

# MODELLING ---------------------------------------------------------------

# SET SEED FOR REPRODUCIBILITY
set.seed(1)

# SPLIT DATA INTO TRAINING AND TESTING SETS
diabetes_splits <- initial_split(df1, strata = diabetes, prop = 0.8)
train <- training(diabetes_splits)
test <- testing(diabetes_splits)

# DEFINE PREDICTOR VARIABLES
xvars <- c("age", "bmi", "smoking_history", "heart_disease", "hypertension", "female")

# SPECIFY A PREPROCESSING RECIPE
diabetes_rec <- recipe(reformulate(xvars, "y"), data = train) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_downsample(y)

# DEFINE RANDOM FOREST MODEL SPECIFICATION
rf_spec <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger", num.threads = NULL, seed = 50, importance = "impurity")

# CREATE WORKFLOW
rf_wf <- workflow() %>%
  add_recipe(diabetes_rec) %>% 
  add_model(rf_spec)

# TRAIN HYPERPARAMETERS ---------------------------------------------------

set.seed(123)

# CREATE CROSS_VALIDATION FOLDS FOR HYPERPARAMETER TUNING
trees_folds <- vfold_cv(train)

# DETECT THE NUMBER OF PHYSICAL CORES ON THE MACHINE
cores <- parallel::detectCores(logical = FALSE)

# CREATE A CLUSTER OF WORKER PROCESSES FOR PARALLEL COMPUTATION
cl <- makePSOCKcluster(cores)

# REGISTER THE PARALLEL BACKEND
registerDoParallel(cl)

tictoc::tic()
rf_res_race <- tune_race_anova(
  object = rf_wf,
  resamples = trees_folds,
  grid = 50,
  control = control_race(verbose_elim = TRUE)
)
tictoc::toc()

stopCluster(cl)

saveRDS(object = rf_res_race, file = "rf_model.rds")

#rf_model = readRDS("rf_model.rds")

plot_race(rf_res_race) + labs(x = "Resample #")

show_best(rf_res_race, metric = "roc_auc")

# CHOOSE THE BEST MODEL ---------------------------------------------------

# FINALISE THE WORKFLOW USING THE BEST VALUES FOR THE HYPERPARAMETERS
rf_last <- rf_wf %>%
  finalize_workflow(select_best(rf_res_race, metric = "roc_auc")) %>%
  # FIT THE FINALIZED MODEL ON THE TRAINING DATA 
  last_fit(diabetes_splits, metrics = metric_set(
    recall, precision, f_meas,
    accuracy, kap,
    roc_auc, sens, spec
  ))

# COLLECT THE PERFORMANCE METRICS ON THE TEST DATASET TO EVALUATE THE MODEL PERFORMANCE
rf_last %>%
  collect_metrics()

# PLOT THE VARIABLE IMPORTANCE FOR THE MODEL
rf_last %>%
  pluck(".workflow", 1) %>%
  extract_fit_parsnip() %>%
  vip(num_features = 10) + 
  theme_minimal() +
  labs(title = "Variable Importance")

# PLOT THE CONFUSION MATRIX FOR THE MODEL
rf_last %>%
  collect_predictions() %>%
  conf_mat(y, .pred_class) %>%
  autoplot(type = "heatmap")

# PLOT THE ROC CURVE
rf_last %>%
  collect_predictions() %>%
  roc_curve(y, .pred_No) %>%
  autoplot()

# EXTRACT THE WORKFLOW
workflow_fit <- rf_last %>% 
  extract_workflow()

# FIT THE WORKFLOW TO THE TRAINING DATA
model_fit <- workflow_fit %>% 
  fit(train)

# FUNCTION TO EXTRACT "YES" PROBABILITIES FOR TIDYMODELS WORKFLOW
pf <- function(workflow, new_data) {
  workflow %>%
    predict(new_data, type = "prob") %>%
    pull(.pred_Yes) %>% 
    as_tibble()
}

# EXTRACT "YES" PROBABILITIES (SHOWING OUTPUT)
pf(model_fit, head(test))

# CLASSIC MODEL EXPLAINABILITY (Permutation Importance, Partial Dependence Plots (PDP), FRIEDMANS H STATISTICS) --------------------------------------------

# CALCULATE PERMUTATION IMPORTANCE
imp <- perm_importance(
  model_fit,
  X = test, y = "diabetes", v = xvars, pred_fun = pf, loss = "logloss"
)

# PLOT PERMUTATION IMPORTANCE
plot(imp) +
  xlab("Increase in test logloss")

# PARTIAL DEPENDENCE PLOT FOR AGE
partial_dep(model_fit, v = "age", train, pred_fun = pf) %>%
  plot()

# ALL PARTIAL DEPENDENCE PLOTS
p <- lapply(xvars, function(x) plot(partial_dep(model_fit, v = x, X = train, pred_fun = pf)))

wrap_plots(p) &
  #ylim(0, 0.23) &
  ylab("Probability")

# FRIEDMANS H STATISTICS
system.time( # 20 s
  H <- hstats(model_fit, train[xvars], approx = TRUE, pred_fun = pf)
)

H # 15% of prediction variability comes from interactions

# PLOT FRIEDMAN'S H STATISTICS
plot(H)

# STRATIFIED PARTIAL DEPENDENCE PLOT
partial_dep(model_fit, "age", BY = "bmi", X = train, pred_fun = pf) %>%
  plot(show_points = FALSE)


# SHAP Values -------------------------------------------------------------

# SAMPLE DATA FOR EXPLANATION
set.seed(1)
X_explain <- train[sample(1:nrow(train), 1000), xvars]

# SAMPLE BACKGROUND DATA
X_background <- train[sample(1:nrow(train), 200), ]

# CALCULATE SHAP VALUES
system.time( # 10 minutes
  shap_values <- permshap(model_fit, X = X_explain, bg_X = X_background, pred_fun = pf)
)

# CREATE SHAPVIZ OBJECT
shap_values <- shapviz(shap_values)

# PRINT SHAPVIZ OBJECT
shap_values # 'shapviz' object representing 1000 x 6 SHAP matrix

# SAVE SHAP VALUES
saveRDS(shap_values, file = "shap_values.rds")

# LOAD SHAP VALUES (COMMENTED OUT)
# shap_values <- readRDS("shap_values.rds")

# PLOT SHAP IMPORTANCE
sv_importance(shap_values, show_numbers = TRUE)

# PLOT SHAP IMPORTANCE (BEE SWARM PLOT)
sv_importance(shap_values, kind = "bee")

# PLOT SHAP DEPENDENCE
sv_dependence(shap_values, v = xvars) &
  ylim(-0.14, 0.24) &
  ylab("Probability")


# MODEL EXPLAINABILITY USING DALEX -------------------------------------------

# CREATE A MODEL EXPLAINER
explainer <- explain_tidymodels(
  model_fit,
  data = test,
  y = test$diabetes,
  label = "rf_classifier"
)

# CALCULATE VARIABLE ATTRIBUTION
rf_la <- local_attributions(explainer, test[52,])

# PLOT BREAK DOWN PROFILE
plot(rf_la)

plotD3(rf_la)

# CALCULATE UNCERTAINTY FOR VARIABLE ATTRIBUTIONS
rf_la_un <- break_down_uncertainty(explainer, test[52,],
                                   path = "average")

# PLOT THE UNCERTAINTY FOR VARIABLE ATTRIBUTIONS
plot(rf_la_un)
