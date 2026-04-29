library(googledrive)
setwd(here::here("analysis_visualization"))
drive_download(
  file = "https://docs.google.com/spreadsheets/d/1a5m1SgQ3ccZN6N1c-u0ZqJkkOKbmYXGjjOcxIefqmf0",
  path = "1.2_tools_annotated_manual.xlsx"
)
manual_annotation <- rio::import(
  "1.2_tools_annotated_manual.xlsx",
  sheet = "validation_DT"
)
llm_annotation <- rio::import("1.2_tools_annotated.xlsx")

library(dplyr)
library(tidyr)
library(irr)

# Columns to compare
compare_cols <- c("category", "opinion_measurement_general", "software")

# Merge on tool
merged <- inner_join(
  manual_annotation |> select(tool, all_of(compare_cols), annotator_comment),
  llm_annotation |> select(tool, all_of(compare_cols), note),
  by = "tool",
  suffix = c("_manual", "_llm")
)

# Macro-averaged F1 for nominal multi-class labels
macro_f1 <- function(true, pred) {
  classes <- union(unique(true), unique(pred))
  f1s <- sapply(classes, function(cls) {
    tp <- sum(true == cls & pred == cls)
    fp <- sum(true != cls & pred == cls)
    fn <- sum(true == cls & pred != cls)
    prec <- if (tp + fp == 0) 0 else tp / (tp + fp)
    rec  <- if (tp + fn == 0) 0 else tp / (tp + fn)
    if (prec + rec == 0) 0 else 2 * prec * rec / (prec + rec)
  })
  mean(f1s)
}

# --- Agreement per column ---
agreement_summary <- lapply(compare_cols, function(col) {
  manual_col <- paste0(col, "_manual")
  llm_col    <- paste0(col, "_llm")

  # Only rows where manual is not NA
  df <- merged |>
    filter(!is.na(.data[[manual_col]])) |>
    select(manual = all_of(manual_col), llm = all_of(llm_col))

  n_total <- nrow(df)
  n_agree <- sum(df$manual == df$llm, na.rm = TRUE)
  pct     <- round(100 * n_agree / n_total, 1)

  # Cohen's kappa
  kappa_val <- tryCatch(
    irr::kappa2(df)$value,
    error = function(e) NA_real_
  )

  # Krippendorff's alpha (nominal)
  # kripp.alpha expects a raters x units matrix
  alpha_val <- tryCatch(
    irr::kripp.alpha(rbind(df$manual, df$llm), method = "nominal")$value,
    error = function(e) NA_real_
  )

  # Macro-averaged F1 (LLM predictions vs manual as ground truth)
  f1_val <- macro_f1(df$manual, df$llm)

  tibble(
    column             = col,
    n_rated            = n_total,
    n_agree            = n_agree,
    pct_agree          = pct,
    cohens_kappa       = round(kappa_val, 3),
    krippendorff_alpha = round(alpha_val, 3),
    macro_f1           = round(f1_val, 3)
  )
}) |>
  bind_rows()

print(agreement_summary)

# --- Disagreements per column ---
disagreements <- lapply(compare_cols, function(col) {
  manual_col <- paste0(col, "_manual")
  llm_col <- paste0(col, "_llm")

  merged |>
    filter(
      !is.na(.data[[manual_col]]),
      .data[[manual_col]] != .data[[llm_col]]
    ) |>
    transmute(
      column = col,
      tool,
      manual = .data[[manual_col]],
      llm = .data[[llm_col]],
      annotator_comment,
      llm_note = note
    )
}) |>
  bind_rows()

print(disagreements, n = Inf)
