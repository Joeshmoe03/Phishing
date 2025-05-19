library(tidyverse)
library(tidytext)
library(SnowballC)
library(factoextra)
library(e1071)

raw_phish = read_csv("CEAS_08.csv")
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
  slice_max(order_by = n, n = 500) |>
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

# PCA biplot: show top contributing stem features
fviz_pca_biplot(pca_phish,
                repel = TRUE,
                select.var = list(contrib = 10),
                col.ind = factor(dtm_labeled$phishing),
                col.var = "black",
                label = "var",
                axes = c(1, 2)) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "PCA Biplot of Phishing vs Legitimate Emails",
       color = "Phishing Label")

#pca_scores = as.data.frame(pca_phish$x)
pca_scores = as.data.frame(pca_phish$x[, 1:20])
pca_scores$phishing = factor(dtm_labeled$phishing)

############################# TRAIN/TEST #######################################

set.seed(42)
train_idx = sample(nrow(pca_scores), size = 0.8 * nrow(pca_scores))
train_data = pca_scores[train_idx, ]
test_data  = pca_scores[-train_idx, ]

################################# Linear Kernel ################################
svm_model = svm(phishing ~ ., data = train_data, 
                 kernel = "linear", scale = FALSE)

#EVALUATE
preds = predict(svm_model, newdata = test_data)

# Confusion matrix
table(Predicted = preds, Actual = test_data$phishing)

# Accuracy
mean(preds == test_data$phishing)

# DO NOT RUN PAST THIS POINT. TIME COMPLEXITY OF NON-LINEAR KERNELS IS EGREGIOUS
################################# Polyn. Kernel ################################
svm_model = svm(phishing ~ ., data = train_data, 
                 kernel = "polynomial", scale = FALSE)

#EVALUATE
preds = predict(svm_model, newdata = test_data)

# Confusion matrix
table(Predicted = preds, Actual = test_data$phishing)

# Accuracy
mean(preds == test_data$phishing)

################################# Polyn. Kernel deg 2 ##########################
svm_model = svm(phishing ~ ., data = train_data, 
                kernel = "polynomial", scale = FALSE,
                degree = 2)

#EVALUATE
preds = predict(svm_model, newdata = test_data)

# Confusion matrix
table(Predicted = preds, Actual = test_data$phishing)

# Accuracy
mean(preds == test_data$phishing)




