# ============================================================
# Sequence Analysis: Heatmaps with Per-File and Global Colour Scales
# ============================================================

library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemes)

# ============================================================
# Constants
# ============================================================

MODEL_RECODE <- c(
  "claude sonnet 4.5"          = "Claude Sonnet 4.5",
  "gemini-3-pro"               = "Gemini 3 Pro",
  "gpt-4o"                     = "GPT-4o",
  "gpt-o4-mini"                = "GPT-o4-mini",
  "grok4-fast-non-reasoning"   = "Grok 4 Fast\n(Non-Reasoning)"
)

SEQUENCE_RECODE <- c(
  "MRI Seq Inject MRI" = "MRI+CT+Clinical\nInject MRI",
  "MRI Seq Inject CT"  = "MRI+CT+Clinical\nInject CT",
  "CT Seq Inject MRI"  = "CT+MRI+Clinical\nInject MRI",
  "CT Seq Inject CT"   = "CT+MRI+Clinical\nInject CT"
)

SEQUENCE_ORDER <- c(
  "MRI+CT+Clinical\nInject MRI",
  "MRI+CT+Clinical\nInject CT",
  "CT+MRI+Clinical\nInject MRI",
  "CT+MRI+Clinical\nInject CT"
)

MODEL_ORDER <- c(
  "Claude Sonnet 4.5",
  "Grok 4 Fast\n(Non-Reasoning)",
  "GPT-4o",
  "GPT-o4-mini",
  "Gemini 3 Pro"
)

# Title lookup: task-based patterns are checked before injection-type patterns
TITLE_LOOKUP <- c(
  "bile_duct_dilation_no_defense"       = "Bile Duct Dilation Detection",
  "diagnosis_no_defense"                = "Diagnosis",
  "gallbladder_distension_no_defense"   = "Gallbladder Distension Detection",
  "pancreatic_duct_dilation_no_defense" = "Pancreatic Duct Dilation Detection",
  "tumor_location_no_defense"           = "Tumor Location Classification",
  "high_contrast"                       = "High Contrast",
  "low_contrast"                        = "Low Contrast",
  "small_font"                          = "Small Font",
  "text_injection"                      = "Text Injection",
  "baseline"                            = "Baseline"
)

HEATMAP_COLORS <- c(low = "#55a0fb", mid = "#FFFFFF", high = "#ff8182")

# ============================================================
# Helpers
# ============================================================

# Derive plot title from filename using TITLE_LOOKUP
get_title_from_filename <- function(filename) {
  matched <- names(Filter(function(pat) grepl(pat, filename), TITLE_LOOKUP))
  if (length(matched) > 0) TITLE_LOOKUP[[matched[1]]] else filename
}

# Load CSV, standardise column names, and recode model + sequence_config
load_seq_csv <- function(file) {
  df <- read.csv(file)
  colnames(df)[1:3] <- c("sequence_config", "model", "accuracy")
  df %>% mutate(
    model           = recode(model,           !!!MODEL_RECODE),
    sequence_config = recode(sequence_config, !!!SEQUENCE_RECODE)
  )
}

# Apply factor ordering for model and sequence_config
apply_factors <- function(df) {
  df$sequence_config <- factor(df$sequence_config, levels = SEQUENCE_ORDER)
  df$model           <- factor(df$model,           levels = MODEL_ORDER)
  df
}

# Shared theme for all sequence heatmaps
seq_heatmap_theme <- theme_minimal() +
  theme(
    axis.text.x      = element_text(size = 13),
    axis.text.y      = element_text(size = 13, lineheight = 0.9),
    axis.title       = element_text(size = 18, face = "bold"),
    plot.title       = element_text(size = 26, face = "bold", hjust = 0.5),
    legend.text      = element_text(size = 10),
    legend.title     = element_text(size = 13),
    legend.position  = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Build heatmap; pass limits = NULL for per-file scale,
# or explicit numeric limits for the global-scale version
build_heatmap <- function(df, plot_title, midpoint, limits = NULL) {
  ggplot(df, aes(x = model, y = sequence_config, fill = accuracy)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", accuracy)), size = 6) +
    scale_fill_gradient2(
      low      = HEATMAP_COLORS["low"],
      mid      = HEATMAP_COLORS["mid"],
      high     = HEATMAP_COLORS["high"],
      midpoint = midpoint,
      limits   = limits,
      name     = "Accuracy"
    ) +
    seq_heatmap_theme +
    labs(title = plot_title, x = "Model", y = "Sequence Configuration", fill = "Accuracy") +
    coord_fixed(ratio = 0.5)
}

# ============================================================
# Setup: locate CSV files and create output directory
# ============================================================

csv_dir     <- "sequence_analysis"
heatmap_dir <- "heatmaps"
if (!dir.exists(heatmap_dir)) dir.create(heatmap_dir)

csv_files <- list.files(path = csv_dir, pattern = "\\.csv$", full.names = TRUE)

# ============================================================
# Section 1 — Per-file colour scale (midpoint = per-file mean)
# ============================================================

for (file in csv_files) {
  df         <- apply_factors(load_seq_csv(file))
  file_name  <- tools::file_path_sans_ext(basename(file))
  plot_title <- get_title_from_filename(file_name)
  
  pdf(file.path(heatmap_dir, paste0(file_name, "_heatmap.pdf")), width = 10, height = 6)
  print(build_heatmap(df, plot_title, midpoint = mean(df$accuracy, na.rm = TRUE)))
  dev.off()
  
  cat("Processed (per-file scale):", file_name, "->", plot_title, "\n")
}

# ============================================================
# Section 2 — Global colour scale (shared midpoint and limits)
# ============================================================

# Pass 1: collect all accuracy values to establish global range
all_accuracy <- unlist(lapply(csv_files, function(file) read.csv(file)[[3]]))

global_min <- min(all_accuracy, na.rm = TRUE)
global_max <- max(all_accuracy, na.rm = TRUE)
global_mid <- (global_min + global_max) / 2

cat(sprintf("Global colour range: %.4f - %.4f  (midpoint: %.4f)\n",
            global_min, global_max, global_mid))

# Pass 2: render with fixed scale
for (file in csv_files) {
  df         <- apply_factors(load_seq_csv(file))
  file_name  <- tools::file_path_sans_ext(basename(file))
  plot_title <- get_title_from_filename(file_name)
  
  pdf(file.path(heatmap_dir, paste0(file_name, "_heatmap.pdf")), width = 10, height = 6)
  print(build_heatmap(df, plot_title,
                      midpoint = global_mid,
                      limits   = c(global_min, global_max)))
  dev.off()
  
  cat("Processed (global scale):", file_name, "->", plot_title, "\n")
}