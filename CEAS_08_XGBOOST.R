library(tidyverse)
library(tidytext)
library(SnowballC)
library(factoextra)
library(e1071)

raw_phish <- read_csv("CEAS_08.csv") 
################################ FORMAT DATA ###################################
phish = raw_phish |>
  rename(text = `body`, phishing = `label`) |>
  mutate(
    text = str_to_lower(text),
    text = str_replace_all(text, "[^a-z\\s]", " "),
    text = str_replace_all(text, "\\s+", " ")
  ) |>
  filter(!is.na(text), text != "empty")

############################## DTM AND TOKENIZATION ############################

tokens = phish |>
  mutate(ID = row_number()) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  mutate(stem = wordStem(word))

vocab = tokens |>
  count(phishing, stem, sort = TRUE) |>
  group_by(phishing) |>
  slice_max(order_by = n, n = 1000) |>
  ungroup() |>
  distinct(stem) |>
  pull(stem)

# Filter tokens and create DTM
dtm_labeled = tokens |>
  filter(stem %in% vocab) |>
  count(ID, stem) |>
  pivot_wider(names_from = stem, values_from = n, values_fill = 0) |>
  left_join(phish |> mutate(ID = row_number()) |> select(ID, phishing), by = "ID")

# Prepare matrix for PCA
dtm_matrix = dtm_labeled |>
  select(-ID, -phishing) |>
  as.matrix()

################################# PCA VIZ ######################################
pca_phish = prcomp(dtm_matrix, scale. = TRUE, center = TRUE)

# PCA biplot: show top 10 contributing stem features
fviz_pca_biplot(pca_phish,
                repel = TRUE,
                select.var = list(contrib = 5),
                col.ind = as.factor(dtm_labeled$phishing),
                col.var = "black",
                label = "var",
                axes = c(1, 2)) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "PCA Biplot of Phishing vs Legitimate Emails",
       color = "Phishing Label")

pca_scores = as.data.frame(pca_phish$x[, 1:20])
pca_scores$phishing = as.factor(dtm_labeled$phishing)

############################# TRAIN/TEST #######################################

set.seed(42)
train_idx = sample(nrow(pca_scores), size = 0.8 * nrow(pca_scores))
train_data = pca_scores[train_idx, ]
test_data  = pca_scores[-train_idx, ]

#Supervised learning models
#Random Forest
library(ranger)

rf1 <- ranger(factor(phishing) ~ PC1_score + PC2_score + PC3_score + PC4_score + PC5_score, 
              data = dtm_labeled |>
                mutate(PC1_score = pca_phish$x[,"PC1"],
                       PC2_score = pca_phish$x[,"PC2"],
                       PC3_score = pca_phish$x[,"PC3"],
                       PC4_score = pca_phish$x[,"PC4"],
                       PC5_score = pca_phish$x[,"PC5"]),
              importance = "impurity")

rf1$confusion.matrix

rf1$variable.importance
rf1

library(ranger)
library(caret)

# Combine all PCA scores with phishing label
all_pcs = as.data.frame(pca_phish$x) |> 
  mutate(phishing = as.factor(dtm_labeled$phishing))

# Define training and testing sets using same indices
train_data_rf = all_pcs[train_idx, ]
test_data_rf  = all_pcs[-train_idx, ]


library(xgboost)
library(shapviz)

#xgboost model

x_data <- as.data.frame(pca_phish$x[, 1:20])     # all PCA components
y_label <- as.numeric(dtm_labeled$phishing)  # labels: 0 or 1

# Train/test split (same as earlier)
x_train <- x_data[train_idx, ]
y_train <- y_label[train_idx]
x_test  <- x_data[-train_idx, ]
y_test  <- y_label[-train_idx]

# Create DMatrix
dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dtest  <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)

# Train XGBoost classification model
#list() inspired from eXtreme Gradient boosting Training manpage example 
set.seed(1)
xgb_model <- xgb.train(
  data = dtrain,
  nrounds = 100,
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",        
    eta = 0.1,
    max_depth = 6
  )
)

# Predict on test set
preds_prob <- predict(xgb_model, newdata = dtest)
preds_label <- as.numeric(preds_prob > 0.5)

# Confusion matrix and accuracy
table(Predicted = preds_label, Actual = y_test)
mean(preds_label == y_test)

shap_values <- shapviz(
  xgb_model,
  X = as.matrix(x_train),
  X_pred = as.matrix(x_train)
)

# SHAP Waterfall Plot for a specific row
sv_waterfall(shap_values, row_id = 10)

# SHAP Beeswarm: Overall variable influence
sv_importance(shap_values, kind = "beeswarm")
