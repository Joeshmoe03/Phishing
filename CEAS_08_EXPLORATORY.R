library(tidyverse)
library(tidytext)
library(ggplot2)

raw_phish = read_csv("CEAS_08.csv")
############################# FORMATTING #######################################
phish = raw_phish |>
  rename(text = `body`, phishing = `label`) |>
  mutate(
    text = str_to_lower(text),
    # https://stackoverflow.com/questions/41984513/r-str-replace-all-except-periods-and-dashes/41984645
    # regex stuff was a bit annoying: https://stackoverflow.com/questions/15625629/regex-expressions-in-java-s-vs-s
    text = str_replace_all(text, "[^a-z\\s]", " "),
    text = str_replace_all(text, "\\s+", " ")
  ) |>
  filter(!is.na(text), text != "empty")
  
phish |>
  count(phishing) |>
  mutate(proportion = n / sum(n)) |>
  ggplot(aes(x = factor(phishing), y = proportion, fill = factor(phishing))) +
  geom_col() +
  labs(
    title = "Proportion of Phishing vs. Non-Phishing Emails",
    x = "Phishing (0 = No, 1 = Yes)",
    y = "Proportion",
    fill = "Phishing"
  ) +
  theme_minimal()

phish |>
  filter(urls == 1) |>
  count(phishing) |>
  mutate(proportion = n / sum(n)) |>
  ggplot(aes(x = factor(phishing), y = proportion, fill = factor(phishing))) +
  geom_col() +
  labs(
    title = "Proportion of Emails with URLs by Phishing Label",
    x = "Phishing (0 = No, 1 = Yes)",
    y = "Proportion (among emails with URLs)",
    fill = "Phishing"
  ) +
  theme_minimal()

phish |>
  # https://stackoverflow.com/questions/8920145/count-the-number-of-all-words-in-a-string
  mutate(text_length = str_count(text, "\\S+")) |>
  ggplot(aes(x = text_length, color = factor(phishing), fill = factor(phishing))) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density of Email Text Lengths",
    x = "Number of 'Tokens'",
    fill = "Phishing",
    color = "Phishing"
  ) +
  theme_minimal() +
  xlim(0, 600)

phish |>
  mutate(text_length = nchar(text)) |>
  ggplot(aes(x = text_length, color = factor(phishing), fill = factor(phishing))) +
  geom_density(alpha = 0.5) +
  scale_color_manual(values = c("blue3", "red1")) +
  scale_fill_manual(values = c("blue3", "red1")) +
  labs(
    title = "Density of Email Text Lengths",
    x = "Number of Characters",
    fill = "Phishing",
    color = "Phishing"
  ) +
  theme_minimal() +
  xlim(0, 2000)

raw_phish |>
  filter(label == 1) |>
  slice_sample(n = 1) |>
  pull(body) |> 
  cat()
