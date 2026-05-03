# ============================================================
# Injection Modality Heatmaps & Injection Number Line Plots
# ============================================================

library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemes)

# ============================================================
# Shared constants
# ============================================================

# Model recode maps (heatmap uses line-break in Grok label; lineplot does not)
MODEL_RECODE_HEATMAP <- c(
  "claude sonnet 4.5"          = "Claude Sonnet 4.5",
  "gemini-3-pro"               = "Gemini 3 Pro",
  "gpt-4o"                     = "GPT-4o",
  "gpt-o4-mini"                = "GPT-o4-mini",
  "grok4-fast-non-reasoning"   = "Grok 4 Fast\n(Non-Reasoning)"
)

MODEL_RECODE_LINE <- c(
  "claude sonnet 4.5"          = "Claude Sonnet 4.5",
  "gemini-3-pro"               = "Gemini 3 Pro",
  "gpt-4o"                     = "GPT-4o",
  "gpt-o4-mini"                = "GPT-o4-mini",
  "grok4-fast-non-reasoning"   = "Grok 4 Fast (Non-Reasoning)"
)

NUM_IMAGES_RECODE <- c(
  "1imgs" = "1 Image",
  "2imgs" = "2 Images",
  "3imgs" = "3 Images",
  "4imgs" = "4 Images",
  "5imgs" = "5 Images"
)

MODALITY_ORDER <- c(
  "CT+MRI+Clinical",
  "MRI+Clinical",
  "CT+Clinical",
  "Clinical",
  "CT+MRI",
  "MRI",
  "CT"
)

MODEL_ORDER_HEATMAP <- c(
  "Claude Sonnet 4.5",
  "Grok 4 Fast\n(Non-Reasoning)",
  "GPT-4o",
  "GPT-o4-mini",
  "Gemini 3 Pro"
)

MODEL_ORDER_LINE <- c(
  "Claude Sonnet 4.5",
  "Grok 4 Fast (Non-Reasoning)",
  "GPT-4o",
  "GPT-o4-mini",
  "Gemini 3 Pro"
)

NUM_IMAGES_ORDER <- c("1 Image", "2 Images", "3 Images", "4 Images", "5 Images")

MODEL_COLORS_LINE <- c(
  "Claude Sonnet 4.5"          = "#f9cdbf",
  "Grok 4 Fast (Non-Reasoning)"= "#9eaac4",
  "GPT-4o"                     = "#80d0c3",
  "GPT-o4-mini"                = "#a6ddea",
  "Gemini 3 Pro"               = "#f3a59a"
)

# Injection type label lookup (shared by both sections)
INJECTION_TITLES <- c(
  high_contrast  = "High Contrast",
  low_contrast   = "Low Contrast",
  small_font     = "Small Font",
  text_injection = "Text Injection",
  baseline       = "Baseline"
)

# ============================================================
# Shared helpers
# ============================================================

# Derive plot title from a CSV filename based on injection type keywords
get_title_from_filename <- function(filename) {
  matched <- names(Filter(function(pat) grepl(pat, filename), INJECTION_TITLES))
  if (length(matched) > 0) INJECTION_TITLES[[matched[1]]] else filename
}

# Load a CSV, standardise the first three column names, and recode models
load_csv <- function(file, model_recode) {
  df <- read.csv(file)
  colnames(df)[1:3] <- c(names(model_recode)[1] |> (\(.).)(), "model", "accuracy")
  # re-assign col 1 generically; actual name set by caller after load
  df %>% mutate(model = recode(model, !!!model_recode))
}

# Shared theme for heatmaps
heatmap_theme <- theme_minimal() +
  theme(
    axis.text.x      = element_text(size = 13),
    axis.text.y      = element_text(size = 13),
    axis.title       = element_text(size = 18, face = "bold"),
    plot.title       = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.text      = element_text(size = 10),
    legend.title     = element_text(size = 13),
    legend.position  = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Build a single heatmap; midpoint and limits can be per-file or global
build_heatmap <- function(df, plot_title, midpoint, limits = NULL) {
  ggplot(df, aes(x = model, y = modality, fill = accuracy)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", accuracy)), size = 6) +
    scale_fill_gradient2(
      low      = "#9BD5F4",
      mid      = "#FFFFFF",
      high     = "#E74538",
      midpoint = midpoint,
      limits   = limits,
      name     = "Accuracy"
    ) +
    heatmap_theme +
    labs(title = plot_title, x = "Model", y = "Modality", fill = "Accuracy") +
    coord_fixed(ratio = 0.5)
}

# ============================================================
# Section 1 — Heatmaps with per-file colour scale
# ============================================================

heatmap_dir  <- "heatmaps"
heatmap_csv_dir <- "injection_modality"   # folder containing the CSVs
if (!dir.exists(heatmap_dir)) dir.create(heatmap_dir)

heatmap_files <- list.files(path = heatmap_csv_dir, pattern = "\\.csv$", full.names = TRUE)

for (file in heatmap_files) {
  df         <- read.csv(file)
  colnames(df)[1:3] <- c("modality", "model", "accuracy")
  df         <- df %>% mutate(model = recode(model, !!!MODEL_RECODE_HEATMAP))
  df$modality <- factor(df$modality, levels = MODALITY_ORDER)
  df$model    <- factor(df$model,    levels = MODEL_ORDER_HEATMAP)
  
  file_name  <- tools::file_path_sans_ext(basename(file))
  plot_title <- get_title_from_filename(file_name)
  per_mid    <- mean(df$accuracy, na.rm = TRUE)
  
  pdf(file = file.path(heatmap_dir, paste0(file_name, "_heatmap.pdf")),
      width = 10, height = 8)
  print(build_heatmap(df, plot_title, midpoint = per_mid))
  dev.off()
  
  cat("Processed (per-file scale):", file_name, "->", plot_title, "\n")
}

# ============================================================
# Section 2 — Heatmaps with unified (global) colour scale
# ============================================================

# Pass 1: collect all accuracy values to establish global range
all_accuracy <- unlist(lapply(heatmap_files, function(file) {
  df_tmp <- read.csv(file)
  df_tmp[[3]]   # third column is accuracy
}))

global_min <- min(all_accuracy, na.rm = TRUE)
global_max <- max(all_accuracy, na.rm = TRUE)
global_mid <- (global_min + global_max) / 2

cat(sprintf("Global colour range: %.4f – %.4f  (midpoint: %.4f)\n",
            global_min, global_max, global_mid))

# Pass 2: render with fixed scale
for (file in heatmap_files) {
  df         <- read.csv(file)
  colnames(df)[1:3] <- c("modality", "model", "accuracy")
  df         <- df %>% mutate(model = recode(model, !!!MODEL_RECODE_HEATMAP))
  df$modality <- factor(df$modality, levels = MODALITY_ORDER)
  df$model    <- factor(df$model,    levels = MODEL_ORDER_HEATMAP)
  
  file_name  <- tools::file_path_sans_ext(basename(file))
  plot_title <- get_title_from_filename(file_name)
  
  pdf(file = file.path(heatmap_dir, paste0(file_name, "_heatmap.pdf")),
      width = 10, height = 8)
  print(build_heatmap(df, plot_title,
                      midpoint = global_mid,
                      limits   = c(global_min, global_max)))
  dev.off()
  
  cat("Processed (global scale):", file_name, "->", plot_title, "\n")
}

# ============================================================
# Section 3 — Line plots: accuracy vs number of images
# ============================================================

lineplot_dir     <- "lineplots"
lineplot_csv_dir <- "injection_num"   # folder containing the CSVs
if (!dir.exists(lineplot_dir)) dir.create(lineplot_dir)

lineplot_files <- list.files(path = lineplot_csv_dir, pattern = "\\.csv$", full.names = TRUE)

for (file in lineplot_files) {
  df <- read.csv(file)
  colnames(df)[1:3] <- c("num_images", "model", "accuracy")
  
  df <- df %>%
    mutate(
      model      = recode(model,      !!!MODEL_RECODE_LINE),
      num_images = recode(num_images, !!!NUM_IMAGES_RECODE)
    )
  
  df$num_images <- factor(df$num_images, levels = NUM_IMAGES_ORDER)
  df$model      <- factor(df$model,      levels = MODEL_ORDER_LINE)
  
  file_name  <- tools::file_path_sans_ext(basename(file))
  plot_title <- get_title_from_filename(file_name)
  
  p <- ggplot(df, aes(x = num_images, y = accuracy, color = model, group = model)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3.5) +
    scale_color_manual(values = MODEL_COLORS_LINE, name = "Model") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    theme_minimal(base_size = 14) +
    theme(
      panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text.x     = element_text(size = 20),
      axis.text.y     = element_text(size = 20),
      axis.title      = element_text(size = 22, face = "bold"),
      plot.title      = element_text(size = 26, face = "bold", hjust = 0.5),
      legend.position = "none"   # Legend hidden intentionally
    ) +
    labs(title = plot_title, x = "Number of Images", y = "Accuracy")
  
  pdf(file = file.path(lineplot_dir, paste0(file_name, "_lineplot.pdf")),
      width = 7, height = 6)
  print(p)
  dev.off()
  
  cat("Processed:", file_name, "->", plot_title, "\n")
}