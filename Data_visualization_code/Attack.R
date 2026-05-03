# ============================================================
# Attack Analysis: Model Performance Under Injection Types
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

MODEL_RECODE <- c(
  "claude sonnet 4.5"          = "Claude Sonnet 4.5",
  "gemini-3-pro"               = "Gemini 3 Pro",
  "gpt-4o"                     = "GPT-4o",
  "gpt-o4-mini"                = "GPT-o4-mini",
  "grok4-fast-non-reasoning"   = "Grok 4 Fast\n(Non-Reasoning)"
)

INJECTION_RECODE <- c(
  "high_contrast"  = "High Contrast",
  "low_contrast"   = "Low Contrast",
  "small_font"     = "Small Font",
  "text_injection" = "Text Injection",
  "baseline"       = "Baseline"
)

TASK_RECODE <- c(
  "bile_duct_dilation"       = "Bile Duct Dilation",
  "diagnosis"                = "Diagnosis",
  "gallbladder_distension"   = "Gallbladder Distension",
  "pancreatic_duct_dilation" = "Pancreatic Duct Dilation",
  "tumor_location"           = "Tumor Location"
)

TASK_TITLES <- c(
  "Tumor Location"          = "Tumor Location Classification",
  "Bile Duct Dilation"      = "Bile Duct Dilation Detection",
  "Pancreatic Duct Dilation"= "Pancreatic Duct Dilation Detection",
  "Diagnosis"               = "Diagnosis",
  "Gallbladder Distension"  = "Gallbladder Distension Detection"
)

INJECTION_ORDER <- c("Baseline", "Text Injection", "High Contrast", "Low Contrast", "Small Font")

MODEL_ORDER <- c(
  "Claude Sonnet 4.5",
  "Grok 4 Fast\n(Non-Reasoning)",
  "GPT-4o",
  "GPT-o4-mini",
  "Gemini 3 Pro"
)

BAR_COLORS <- c("#4D9DC9", "#E18E4D", "#4EBB9D", "#E7B050", "#D8A1C9")

# ============================================================
# Helper: convert p-value to significance label
# ============================================================

p_to_sig <- function(p) {
  if      (p < 0.001) "***"
  else if (p < 0.01)  "**"
  else                "*"
}

# ============================================================
# Helper: run Wilcoxon test safely (suppress ties warnings)
# ============================================================

safe_wilcox <- function(x, y) {
  tryCatch(
    suppressWarnings(wilcox.test(x, y)),
    error = function(e) NULL
  )
}

# ============================================================
# Helper: add significance bracket annotation to a ggplot
# ============================================================

add_sig_bracket <- function(plot, x1, x2, y, label) {
  plot +
    annotate("segment", x = x1,  xend = x2,  y = y,        yend = y,        size = 0.5, color = "black") +
    annotate("segment", x = x1,  xend = x1,  y = y - 0.02, yend = y,        size = 0.5, color = "black") +
    annotate("segment", x = x2,  xend = x2,  y = y - 0.02, yend = y,        size = 0.5, color = "black") +
    annotate("text",    x = (x1 + x2) / 2,   y = y + 0.03, label = label,
             size = 7, fontface = "bold")
}

# ============================================================
# Helper: find significant injection types vs "Text Injection"
# Returns a named list: model -> list of (group1, group2, p_value)
# ============================================================

find_significant_vs_text <- function(task_data, model_order, available_types) {
  results <- setNames(vector("list", length(model_order)), model_order)
  
  if (!"Text Injection" %in% available_types) {
    message("  Warning: 'Text Injection' not found in data; skipping significance tests.")
    return(results)
  }
  
  comparators <- setdiff(available_types, "Text Injection")
  
  for (model_name in model_order) {
    model_data <- task_data %>% filter(model == model_name)
    if (nrow(model_data) == 0) next
    
    sig_list <- list()
    for (group2 in comparators) {
      d1 <- model_data %>% filter(injection_type == "Text Injection") %>% pull(accuracy_mean)
      d2 <- model_data %>% filter(injection_type == group2)            %>% pull(accuracy_mean)
      
      if (length(d1) == 0 || length(d2) == 0) next
      
      res <- safe_wilcox(d1, d2)
      if (!is.null(res) && !is.na(res$p.value) && res$p.value < 0.05) {
        sig_list <- c(sig_list, list(list(
          group1  = "Text Injection",
          group2  = group2,
          p_value = res$p.value
        )))
      }
    }
    
    results[[model_name]] <- sig_list
    message(sprintf("  Model %-35s — %d significant comparison(s) vs Text Injection",
                    model_name, length(sig_list)))
  }
  results
}

# ============================================================
# Helper: build significance-annotated ggplot for one task
# ============================================================

build_task_plot <- function(task_data, plot_title, available_types,
                            model_order, sig_comparisons) {
  
  n_types    <- length(available_types)
  bar_colors <- if (n_types <= length(BAR_COLORS)) BAR_COLORS[1:n_types]
  else colorRampPalette(BAR_COLORS)(n_types)
  
  dodge_width <- 0.85
  bar_width   <- dodge_width / n_types
  
  p <- ggplot(task_data, aes(x = model, y = accuracy_mean, fill = injection_type)) +
    
    # Mean bars
    stat_summary(fun      = mean,     geom = "bar",
                 position = position_dodge(dodge_width), width = 0.75,
                 color = "black", size = 0.5) +
    
    # Error bars (±SE)
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 position = position_dodge(dodge_width), width = 0.25, size = 0.5) +
    
    # Jittered individual points
    geom_point(aes(fill = injection_type),
               position = position_jitterdodge(jitter.width  = 0.12,
                                               jitter.height = 0.01,
                                               dodge.width   = dodge_width),
               size = 1.8, alpha = 0.6, shape = 21, color = "black", stroke = 0.5) +
    
    scale_fill_manual(values = bar_colors) +
    
    labs(x = "", y = "Accuracy", title = plot_title, fill = "Injection Type") +
    
    theme_base() +
    theme(
      plot.title   = element_text(size = 36, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 30, face = "bold"),
      axis.title.y = element_text(size = 30, face = "bold"),
      axis.text.x  = element_text(size = 22, color = "black", angle = 0,
                                  hjust = 0.5, vjust = 0.5, lineheight = 0.9),
      axis.text.y  = element_text(size = 26, color = "black"),
      legend.title = element_text(size = 24, face = "bold"),
      legend.text  = element_text(size = 20),
      legend.position = "right",
      plot.margin  = unit(c(1, 1, 2.5, 1.5), "cm")
    ) +
    
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(-0.03, 1.45))
  
  # Add significance brackets per model
  for (i in seq_along(model_order)) {
    model_name <- model_order[i]
    comparisons <- sig_comparisons[[model_name]]
    if (length(comparisons) == 0) next
    
    for (k in seq_along(comparisons)) {
      cmp    <- comparisons[[k]]
      idx1   <- which(available_types == cmp$group1)
      idx2   <- which(available_types == cmp$group2)
      x1     <- i + (idx1 - (n_types + 1) / 2) * bar_width
      x2     <- i + (idx2 - (n_types + 1) / 2) * bar_width
      y_pos  <- 1.05 + (k - 1) * 0.08
      p <- add_sig_bracket(p, x1, x2, y_pos, p_to_sig(cmp$p_value))
    }
  }
  
  p
}

# ============================================================
# Load and recode data
# ============================================================

cibersoft <- read.csv("case_level_data.csv") %>%
  mutate(
    model          = recode(model,          !!!MODEL_RECODE),
    injection_type = recode(injection_type, !!!INJECTION_RECODE),
    task           = recode(task,           !!!TASK_RECODE)
  )

message(sprintf("Loaded %d rows | %d tasks | %d models",
                nrow(cibersoft),
                length(unique(cibersoft$task)),
                length(unique(cibersoft$model))))

# ============================================================
# Main loop: one PDF + two CSVs per task
# ============================================================

for (task_name in unique(cibersoft$task)) {
  
  message(sprintf("\n=== Processing task: %s ===", task_name))
  
  task_data <- cibersoft %>% filter(task == task_name)
  if (nrow(task_data) == 0) { message("  No data — skipping."); next }
  
  # Restrict injection order to types present in this task
  available_types <- INJECTION_ORDER[INJECTION_ORDER %in% unique(task_data$injection_type)]
  
  task_data$injection_type <- factor(task_data$injection_type, levels = available_types)
  task_data$model          <- factor(task_data$model,          levels = MODEL_ORDER)
  
  # Significance tests (vs Text Injection)
  sig_comparisons <- find_significant_vs_text(task_data, MODEL_ORDER, available_types)
  
  # Plot title and safe file-name prefix
  plot_title <- TASK_TITLES[task_name]
  if (is.na(plot_title)) plot_title <- paste(task_name, "Classification")
  safe_prefix <- gsub("[^[:alnum:]_-]", "_", plot_title)
  
  # --- PDF ---
  pdf(paste0(safe_prefix, "_AllModels.pdf"), width = 26, height = 12)
  print(build_task_plot(task_data, plot_title, available_types, MODEL_ORDER, sig_comparisons))
  dev.off()
  message(sprintf("  Saved: %s_AllModels.pdf", safe_prefix))
  
  # --- Descriptive statistics CSV ---
  task_stats <- task_data %>%
    group_by(model, injection_type) %>%
    summarise(
      n      = n(),
      mean   = mean(accuracy_mean,   na.rm = TRUE),
      sd     = sd(accuracy_mean,     na.rm = TRUE),
      median = median(accuracy_mean, na.rm = TRUE),
      min    = min(accuracy_mean,    na.rm = TRUE),
      max    = max(accuracy_mean,    na.rm = TRUE),
      .groups = "drop"
    )
  stats_file <- paste0(safe_prefix, "_AllModels_statistics.csv")
  write.csv(task_stats, stats_file, row.names = FALSE)
  message(sprintf("  Saved: %s", stats_file))
  
  # --- Significant p-values CSV (vs Text Injection) ---
  sig_rows <- do.call(rbind, lapply(MODEL_ORDER, function(model_name) {
    comps <- sig_comparisons[[model_name]]
    if (length(comps) == 0) return(NULL)
    do.call(rbind, lapply(comps, function(cmp) {
      data.frame(
        Model        = model_name,
        Group1       = cmp$group1,
        Group2       = cmp$group2,
        p_value      = cmp$p_value,
        significance = p_to_sig(cmp$p_value),
        stringsAsFactors = FALSE
      )
    }))
  }))
  
  if (!is.null(sig_rows) && nrow(sig_rows) > 0) {
    pval_file <- paste0(safe_prefix, "_AllModels_vs_TextInjection_pvalues.csv")
    write.csv(sig_rows, pval_file, row.names = FALSE)
    message(sprintf("  Saved: %s", pval_file))
  }
}

message("\n=== All tasks complete ===")