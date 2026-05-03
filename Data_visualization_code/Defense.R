# ============================================================
# Defense Analysis: Gap Plots and Supervisor Metric Heatmaps
# ============================================================

library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemes)

# ============================================================
# Constants
# ============================================================

CSV_DIR <- "part2_supervisor_metrics"

# Model recode maps — gap plots use line-break labels; heatmaps do not
MODEL_RECODE_GAP <- c(
  "claude sonnet 4.5"          = "Claude\nSonnet 4.5",
  "gemini-3-pro"               = "Gemini 3\nPro",
  "gpt-4o"                     = "GPT-4o",
  "gpt-o4-mini"                = "GPT-o4-mini",
  "grok4-fast-non-reasoning"   = "Grok-4-Fast"
)

MODEL_RECODE_HEATMAP <- c(
  "claude sonnet 4.5"          = "Claude Sonnet 4.5",
  "gemini-3-pro"               = "Gemini 3 Pro",
  "gpt-4o"                     = "GPT-4o",
  "gpt-o4-mini"                = "GPT-o4-mini",
  "grok4-fast-non-reasoning"   = "Grok 4 Fast\n(Non-Reasoning)"
)

METRIC_RECODE <- c(
  "no_defense"               = "No Defense",
  "supervisor_detection"     = "Supervisor Detection",
  "supervisor_attribution"   = "Supervisor Attribution",
  "supervisor_localization"  = "Supervisor Localization"
)

MODEL_ORDER_GAP <- c(
  "Claude\nSonnet 4.5", "Grok-4-Fast", "GPT-4o", "GPT-o4-mini", "Gemini 3\nPro"
)

MODEL_ORDER_HEATMAP <- c(
  "Claude Sonnet 4.5",
  "Grok 4 Fast\n(Non-Reasoning)",
  "GPT-4o",
  "GPT-o4-mini",
  "Gemini 3 Pro"
)

METRIC_ORDER_GAP <- c(
  "Detection", "Attribution", "Localization", "Override", "No-Defense Accuracy"
)

METRIC_ORDER_HEATMAP <- c(
  "Supervisor Localization",
  "Supervisor Attribution",
  "Supervisor Detection",
  "No Defense"
)

# Modality → plot title (used by gap plots)
MODALITY_TITLES <- c(
  "CT"          = "Partial CT Injection",
  "MRI"         = "Partial MRI Injection",
  "text"        = "Textual Injection",
  "visual_full" = "Visual Injection (Full)"
)

# Filename keyword → plot title (ordered: injection type > task > other)
TITLE_LOOKUP <- c(
  "CT_injection"           = "Partial CT Injection",
  "MRI_injection"          = "Partial MRI Injection",
  "text_injection"         = "Text Injection",
  "visual_full_injection"  = "Visual Injection (Full)",
  "bile_duct_dilation"     = "Bile Duct Dilation Detection",
  "diagnosis"              = "Diagnosis",
  "gallbladder_distension" = "Gallbladder Distension Detection",
  "pancreatic_duct_dilation"= "Pancreatic Duct Dilation Detection",
  "tumor_location"         = "Tumor Location Classification",
  "high_contrast"          = "High Contrast",
  "low_contrast"           = "Low Contrast",
  "small_font"             = "Small Font",
  "baseline"               = "Baseline"
)

# Visual encoding for gap-plot metrics
METRIC_COLORS <- c(
  "Detection"           = "green3",
  "Attribution"         = "#1565C0",
  "Localization"        = "#F9A825",
  "Override"            = "#9C27B0",
  "No-Defense Accuracy" = "#E53935"
)
METRIC_SHAPES <- c(
  "Detection"           = 16,
  "Attribution"         = 15,
  "Localization"        = 18,
  "Override"            = 17,
  "No-Defense Accuracy" = 4
)
METRIC_SIZES <- c(
  "Detection"           = 4.5,
  "Attribution"         = 4.5,
  "Localization"        = 5.5,
  "Override"            = 4.5,
  "No-Defense Accuracy" = 4
)

HEATMAP_COLORS <- c(low = "#43b284", mid = "#FFFFFF", high = "#fab255")

# ============================================================
# Helpers
# ============================================================

# Derive plot title from a filename using ordered TITLE_LOOKUP
get_title_from_filename <- function(filename) {
  matched <- names(Filter(function(pat) grepl(pat, filename), TITLE_LOOKUP))
  if (length(matched) > 0) TITLE_LOOKUP[[matched[1]]] else filename
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

# Build heatmap with fixed or per-file colour scale
build_heatmap <- function(df, plot_title, midpoint, limits = NULL) {
  ggplot(df, aes(x = model, y = metric, fill = accuracy)) +
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
    heatmap_theme +
    labs(title = plot_title, x = "Model", y = "Metric", fill = "Accuracy") +
    coord_fixed(ratio = 0.5)
}

# Build gap plot for one modality
build_gap_plot <- function(df, plot_title, present_metrics) {
  curr_colors <- METRIC_COLORS[present_metrics]
  curr_shapes <- METRIC_SHAPES[present_metrics]
  curr_sizes  <- METRIC_SIZES[present_metrics]
  
  has_gap <- all(c("Detection", "Override") %in% present_metrics)
  
  p <- ggplot()
  
  # Gap segments between Detection and Override
  if (has_gap) {
    det_df <- df %>% filter(metric == "Detection") %>%
      select(model, det = accuracy) %>% distinct(model, .keep_all = TRUE)
    ov_df  <- df %>% filter(metric == "Override")  %>%
      select(model, ov  = accuracy) %>% distinct(model, .keep_all = TRUE)
    
    gap_df <- inner_join(det_df, ov_df, by = "model") %>%
      filter(!is.na(det) & !is.na(ov)) %>%
      mutate(
        ymin    = pmin(det, ov),
        ymax    = pmax(det, ov),
        delta   = abs(det - ov),
        ymid    = (det + ov) / 2,
        x_num   = as.numeric(model),
        x_label = x_num + 0.2
      )
    
    p <- p +
      geom_segment(
        data      = gap_df,
        aes(x = model, xend = model, y = ymin, yend = ymax),
        color     = "gray75",
        linewidth = 7,
        alpha     = 0.6,
        lineend   = "butt"
      )
    
    # Delta labels for gaps >= 0.1
    gap_label_df <- gap_df %>% filter(delta >= 0.1)
    if (nrow(gap_label_df) > 0) {
      p <- p +
        geom_text(
          data     = gap_label_df,
          aes(x = x_label, y = ymid, label = sprintf("Delta==%.2f", delta)),
          size     = 5.2,
          color    = "gray35",
          hjust    = 0,
          vjust    = 0.5,
          fontface = "italic",
          parse    = TRUE
        )
    }
  }
  
  p +
    geom_point(
      data   = df,
      aes(x = model, y = accuracy, color = metric, shape = metric, size = metric),
      stroke = 1.1
    ) +
    scale_color_manual(values = curr_colors, name = NULL) +
    scale_shape_manual(values = curr_shapes, name = NULL) +
    scale_size_manual( values = curr_sizes,  name = NULL) +
    scale_y_continuous(limits = c(0, 1.12), breaks = seq(0, 1, 0.2), expand = c(0, 0)) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
    guides(
      color = guide_legend(
        override.aes = list(shape = unname(curr_shapes),
                            size  = unname(curr_sizes),
                            stroke = 1.1),
        nrow = 1
      ),
      shape = "none",
      size  = "none"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title         = element_text(size = 26, face = "bold", hjust = 0.5,
                                        margin = margin(b = 10)),
      axis.title.x       = element_text(size = 18, face = "bold", margin = margin(t = 8)),
      axis.title.y       = element_text(size = 18, face = "bold", margin = margin(r = 8)),
      axis.text.x        = element_text(size = 16, color = "gray20"),
      axis.text.y        = element_text(size = 16, color = "gray20"),
      axis.ticks         = element_line(color = "gray30", linewidth = 0.4),
      axis.ticks.length  = unit(3, "pt"),
      panel.border       = element_rect(color = "gray30", fill = NA, linewidth = 0.6),
      legend.position    = "bottom",
      legend.text        = element_text(size = 14),
      legend.key.size    = unit(1, "lines"),
      legend.spacing.x   = unit(0.4, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      plot.margin        = margin(12, 20, 8, 12)
    ) +
    labs(title = plot_title, x = "Model", y = "Performance")
}

# ============================================================
# Section 1 — Gap plots (one per modality)
# ============================================================

gap_dir <- "gap_plots"
if (!dir.exists(gap_dir)) dir.create(gap_dir)

df_all <- read.csv(file.path(CSV_DIR, "defense_results_v2.csv"),
                   stringsAsFactors = FALSE)

for (mod in unique(df_all$modality)) {
  tryCatch({
    plot_title <- MODALITY_TITLES[mod]
    if (is.na(plot_title)) plot_title <- mod
    
    cat("Processing modality:", mod, "->", plot_title, "\n")
    
    df <- df_all %>%
      filter(modality == mod) %>%
      select(metric = strategy_or_metric, model, accuracy) %>%
      mutate(
        metric = ifelse(metric == "No Defense", "No-Defense Accuracy", metric),
        model  = recode(model, !!!MODEL_RECODE_GAP)
      )
    
    present_metrics <- intersect(METRIC_ORDER_GAP, unique(df$metric))
    df$metric <- factor(df$metric, levels = present_metrics)
    df$model  <- factor(df$model,  levels = MODEL_ORDER_GAP)
    
    cat("  Models:", paste(unique(df$model), collapse = " | "), "\n")
    cat("  Metrics:", paste(present_metrics, collapse = ", "), "\n")
    
    pdf(file.path(gap_dir, paste0(mod, "_gap.pdf")),
        width = 8, height = 6, onefile = FALSE)
    print(build_gap_plot(df, plot_title, present_metrics))
    dev.off()
    
    cat("  Done. Gap drawn:", all(c("Detection", "Override") %in% present_metrics), "\n\n")
    
  }, error = function(e) {
    cat("  ERROR in modality", mod, ":", conditionMessage(e), "\n\n")
  })
}

# ============================================================
# Section 2 — Supervisor metric heatmaps (global colour scale)
# ============================================================

heatmap_dir <- "heatmaps"
if (!dir.exists(heatmap_dir)) dir.create(heatmap_dir)

csv_files <- list.files(path = CSV_DIR, pattern = "\\.csv$", full.names = TRUE)

# Pass 1: collect all accuracy values to establish global range
all_accuracy <- unlist(lapply(csv_files, function(file) read.csv(file)[[3]]))

global_min <- min(all_accuracy, na.rm = TRUE)
global_max <- max(all_accuracy, na.rm = TRUE)
global_mid <- (global_min + global_max) / 2

cat(sprintf("Global colour range: %.4f - %.4f  (midpoint: %.4f)\n",
            global_min, global_max, global_mid))

# Pass 2: render with fixed scale
for (file in csv_files) {
  df <- read.csv(file)
  colnames(df)[1:3] <- c("metric", "model", "accuracy")
  
  df <- df %>%
    mutate(
      model  = recode(model,  !!!MODEL_RECODE_HEATMAP),
      metric = recode(metric, !!!METRIC_RECODE)
    )
  
  df$metric <- factor(df$metric, levels = METRIC_ORDER_HEATMAP)
  df$model  <- factor(df$model,  levels = MODEL_ORDER_HEATMAP)
  
  file_name  <- tools::file_path_sans_ext(basename(file))
  plot_title <- get_title_from_filename(file_name)
  
  pdf(file.path(heatmap_dir, paste0(file_name, "_heatmap.pdf")),
      width = 10, height = 6)
  print(build_heatmap(df, plot_title,
                      midpoint = global_mid,
                      limits   = c(global_min, global_max)))
  dev.off()
  
  cat("Processed:", file_name, "->", plot_title, "\n")
}