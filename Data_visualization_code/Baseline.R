# ============================================================
# Pancreatic Imaging AI Model Performance Visualization
# ============================================================

library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemes)

set.seed(967)

# ============================================================
# Shared constants
# ============================================================

MODEL_LEVELS <- c(
  "Gemini 3 Pro",
  "GPT-o4-mini",
  "GPT-4o",
  "Grok 4 Fast (Non-Reasoning)",
  "Claude Sonnet 4.5"
)

MODEL_LABELS <- c(
  "Gemini 3 Pro"                  = "Gemini 3 Pro",
  "GPT-o4-mini"                   = "GPT-o4-mini",
  "GPT-4o"                        = "GPT-4o",
  "Grok 4 Fast (Non-Reasoning)"   = "Grok 4 Fast\n(Non-Reasoning)",
  "Claude Sonnet 4.5"             = "Claude Sonnet 4.5"
)

MODEL_RECODE <- c(
  "claude sonnet 4.5"            = "Claude Sonnet 4.5",
  "gemini-3-pro"                 = "Gemini 3 Pro",
  "gpt-4o"                       = "GPT-4o",
  "gpt-o4-mini"                  = "GPT-o4-mini",
  "grok4-fast-non-reasoning"     = "Grok 4 Fast (Non-Reasoning)"
)

METRIC_RECODE <- c(
  "accuracy" = "Accuracy",
  "recall"   = "Specificity"
)

# Shared theme settings applied to all plots
base_theme <- theme(
  plot.title   = element_text(size = 28, face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = 26, face = "bold"),
  axis.title.y = element_text(size = 26, face = "bold"),
  axis.text.x  = element_text(size = 22, color = "black"),
  axis.text.y  = element_text(size = 22, color = "black"),
  plot.margin  = unit(c(1, 1, 1, 1.5), "cm")
)

legend_theme <- theme(
  legend.position   = "right",
  legend.title      = element_blank(),
  legend.text       = element_text(size = 18),
  legend.background = element_rect(fill = "white", color = "black"),
  legend.key.size   = unit(1.2, "cm")
)

# ============================================================
# Helper function: load and recode a dual-metric CSV
# ============================================================

load_dual_metric_data <- function(csv_file) {
  df <- read.csv(csv_file)
  df <- df %>%
    mutate(
      model  = recode(model,  !!!MODEL_RECODE),
      metric = recode(metric, !!!METRIC_RECODE)
    )
  # Order levels: Specificity first so bars render correctly; reversed in legend
  df$model  <- factor(df$model,  levels = MODEL_LEVELS)
  df$metric <- factor(df$metric, levels = c("Specificity", "Accuracy"))
  df
}

# ============================================================
# Helper function: build a dual-metric grouped bar chart
# ============================================================

plot_dual_metric <- function(df, title) {
  ggbarplot(df,
            x        = "model",
            y        = "value",
            fill     = "metric",
            color    = "black",
            palette  = c("#75b7d1", "#ff7676"),   # Specificity = blue, Accuracy = red
            add      = "mean_se",
            position = position_dodge(0.8)) +
    
    # Jittered individual data points (grouped)
    geom_point(aes(x = model, y = value, fill = metric),
               size     = 1.2,
               alpha    = 0.6,
               shape    = 21,
               color    = "black",
               stroke   = 0.5,
               position = position_jitterdodge(
                 jitter.width  = 0.09,
                 jitter.height = 0.01,
                 dodge.width   = 0.8
               )) +
    
    labs(x = "", y = "Rate", title = title) +
    
    theme_base() +
    base_theme +
    legend_theme +
    
    # Reverse legend so Accuracy appears on top
    guides(fill = guide_legend(reverse = TRUE)) +
    
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    scale_x_discrete(labels = MODEL_LABELS) +
    
    coord_flip(ylim = c(0, 1.05), clip = "off")
}

# ============================================================
# Batch render: dual-metric plots
# ============================================================

dual_metric_tasks <- list(
  list(
    csv   = "diagnosis_performance.csv",
    pdf   = "diagnosis.pdf",
    title = "Diagnosis"
  ),
  list(
    csv   = "pancreatic_duct_dilation_performance.csv",
    pdf   = "Pancreatic Duct Dilation Detection.pdf",
    title = "Pancreatic Duct Dilation Detection"
  ),
  list(
    csv   = "bile_duct_dilation_performance.csv",
    pdf   = "Bile Duct Dilation Detection.pdf",
    title = "Bile Duct Dilation Detection"
  ),
  list(
    csv   = "gallbladder_distension_performance.csv",
    pdf   = "Gallbladder Distension Performance.pdf",
    title = "Gallbladder Distension Detection"
  )
)

for (task in dual_metric_tasks) {
  df <- load_dual_metric_data(task$csv)
  pdf(task$pdf, width = 13, height = 7)
  print(plot_dual_metric(df, task$title))
  dev.off()
}

# ============================================================
# Tumor Location Classification (single-metric + stat tests)
# ============================================================

df_tumor <- read.csv("tumor_location_performance.csv") %>%
  mutate(model = recode(model, !!!MODEL_RECODE))

df_tumor$model <- factor(df_tumor$model, levels = MODEL_LEVELS)

pdf("Tumor Location Classification.pdf", width = 13, height = 7)

ggbarplot(df_tumor,
          x        = "model",
          y        = "value",
          fill     = "model",
          color    = "black",
          palette  = c("#f3a59a", "#a6ddea", "#80d0c3", "#9eaac4", "#f9cdbf"),
          add      = "mean_se",
          position = position_dodge(0.8)) +
  
  # Jittered individual data points
  geom_jitter(aes(x = model, y = value, fill = model),
              width  = 0.075,
              height = 0.02,
              size   = 1.2,
              alpha  = 0.6,
              shape  = 21,
              color  = "black",
              stroke = 0.5) +
  
  labs(x = "", y = "Accuracy", title = "Tumor Location Classification") +
  
  theme_base() +
  base_theme +
  theme(legend.position = "none") +
  
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_x_discrete(labels = MODEL_LABELS) +
  
  coord_flip(ylim = c(0, 1.3), clip = "off") +
  
  # Pairwise significance annotations (Wilcoxon)
  stat_compare_means(
    comparisons = list(
      c("Claude Sonnet 4.5", "Gemini 3 Pro"),
      c("Gemini 3 Pro",      "Grok 4 Fast (Non-Reasoning)"),
      c("Gemini 3 Pro",      "GPT-o4-mini")
    ),
    method        = "wilcox.test",
    label         = "p.signif",
    step.increase = 0.08,
    tip.length    = 0.01,
    size          = 12
  )

dev.off()

# ============================================================
# Tumor Location: pairwise Wilcoxon tests (all combinations)
# ============================================================

combinations <- combn(MODEL_LEVELS, 2, simplify = FALSE)

pairwise_results <- do.call(rbind, lapply(combinations, function(pair) {
  data1  <- df_tumor %>% filter(model == pair[1]) %>% pull(value)
  data2  <- df_tumor %>% filter(model == pair[2]) %>% pull(value)
  p_val  <- wilcox.test(data1, data2)$p.value
  sig    <- cut(p_val,
                breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                labels = c("***", "**", "*", "ns"))
  data.frame(Group1 = pair[1], Group2 = pair[2],
             p_value = p_val, significance = as.character(sig))
}))

message("========== Pairwise Wilcoxon p-values ==========")
print(pairwise_results)

# ============================================================
# Tumor Location: descriptive statistics per model
# ============================================================

model_stats <- df_tumor %>%
  group_by(model) %>%
  summarise(
    n      = n(),
    mean   = mean(value),
    sd     = sd(value),
    median = median(value),
    min    = min(value),
    max    = max(value),
    .groups = "drop"
  )

message("========== Descriptive statistics by model ==========")
print(model_stats)