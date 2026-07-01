#############################
#
# Title: Merge and aggregate tool annotation to papers
# Description: Merges and aggregates LLM-annotated (clean) tool names (1.2_tools_annotated)
# via original names (1.2_tool_dict.csv) on paper-level (2._annotation-results.csv)
# Helper to be sourced from 2._tables_and_plots.qmd
#
# Author: Daniel Thiele
# Date: 2026-06-30
#
#############################

# 01: Function -----------------------------------------------------------

#' Merge and Aggregate Tool Annotations to Papers
#'
#' Loads three data sources (tool dictionary, LLM-annotated tools, paper annotations),
#' extracts raw tool mentions from papers, fuzzy-matches them to the dictionary,
#' and returns a long-format table with clean tool names and annotations merged in.
#'
#' @param dict_file Path to the tool dictionary (.csv, or any tabular format) (cols: `tool`, `tool_clean`).
#' @param tool_file Path to the LLM-annotated tool file in long format (.rds, or any tabular format accepted) (cols: `tool`, `category`,
#'   `task_representation`, `task_classification`, `task_target`, `software`, `note`;
#'   additional columns are retained but not processed).
#' @param paper_file Path to the paper annotation (.csv, or any tabular format) (cols: `id`, `doi`, `file`,
#'   `variable`, `result`; additional columns are ignored).
#' @param fuzzy_threshold Numeric similarity threshold passed to
#'   `sentiner::fuzzy_match_df()` to fuzzy-match a few (>200 out of ~12,000) raw tool names that are not in the harmoized list. Default: 0.1 (calibrated via manual inspection).
#' @param na_results Character vector of result values to treat as NA. Default "-", "IRRELEVANT", "No (unclear)".
#'
#' @return A `data.table` (one row per paper × matched tool) with columns:
#'   `paper_tool_idx`, `id`, `doi`, `file`, `question`, `tool_id`,
#'   `tool_name_raw`, `tool_clean`, and any annotation columns present in `tool_file`.
merge_paper_tools <- function(
    dict_file = "analysis_visualization/1.2_tool_dict.csv",
    tool_file = "analysis_visualization/1.2_tools_annotated_long.rds",
    paper_file = "analysis_visualization/2._annotation-results.csv",
    fuzzy_threshold = 0.1,
    na_results = c("-", "IRRELEVANT", "No (unclear)")
) {

  # 0. Check required packages
  if (!requireNamespace("rio", quietly = TRUE)) {
    stop("Package 'rio' is required but not installed. Install it with: pak::pak(\"rio\")",
         call. = FALSE)
  }
  if (!requireNamespace("sentiner", quietly = TRUE)) {
    stop("Package 'sentiner' is required but not installed. Install it with: pak::pak(\"thieled/sentiner\")",
         call. = FALSE)
  }

  # 1. Load data 
  dict_dt  <- data.table::as.data.table(rio::import(dict_file, trust = TRUE))
  tool_dt  <- data.table::as.data.table(rio::import(tool_file,  trust = TRUE))
  paper_dt <- data.table::as.data.table(rio::import(paper_file, trust = TRUE))

  # 2. Clean paper results 
  # Replace sentinel non-answer strings with NA
  paper_dt[result %in% na_results, result := NA_character_]

  # 3. Build tool ID in dictionary 
  # Sequential zero-padded integer ID per unique clean tool name
  dict_dt[, tool_id := sprintf("%04d", as.integer(
    factor(tool_clean, levels = unique(tool_clean))
  ))]

  # Drop empty tool entries
  dict_dt <- dict_dt[nchar(tool) > 0]

  # 4. Reshape paper data to long tool format 
  # Wide: one col per Q[12]_1_ variable
  paper_wide <- data.table::dcast(
    paper_dt[grepl("^Q[12]_1_", variable)],
    id + doi + file ~ variable,
    value.var = "result",
    fun.aggregate = function(x) x[1]
  )

  # Determine which tool-name measure columns are present
  measure_cols <- intersect(
    c("Q1_1_Tool-Name", "Q2_1_Tool-Name"),
    names(paper_wide)
  )

  # Long: one row per paper × question × raw tool mention
  paper_tools_long <- data.table::melt(
    paper_wide,
    id.vars      = c("id", "doi", "file"),
    measure.vars = measure_cols,
    variable.name = "question",
    value.name    = "tool_name_raw"
  )[
    !is.na(tool_name_raw)
  ][
    # Split semicolon/comma/plus-separated tool lists into individual rows
    , .(tool_name_raw = trimws(unlist(strsplit(tool_name_raw, "[;,+]")))),
    by = .(id, doi, file, question)
  ][
    nchar(tool_name_raw) > 0
  ][
    # Strip trailing "_Tool-Name" suffix from question labels
    , question := sub("_Tool-Name", "", question)
  ]

  # De-duplicate within paper: keep first mention of each raw tool name
  paper_tools_long <- unique(paper_tools_long, by = c("id", "tool_name_raw"))

  # Sequential index per paper for traceability
  paper_tools_long[, paper_tool_idx := paste0(id, "_", seq_len(.N)), by = id]

  # 5. Fuzzy match raw tool names to dictionary
  matched <- data.table::as.data.table(
    sentiner::fuzzy_match_df(
      input            = paper_tools_long,
      input_col        = "tool_name_raw",
      input_id_col     = "paper_tool_idx",
      target           = dict_dt,
      target_col       = "tool",
      target_id_col    = "tool_id",
      threshold        = fuzzy_threshold,
      tolower          = TRUE,
      best_by_input_id = TRUE,
      best_by_target_id = FALSE,
      verbose          = TRUE
    )
  )[poor_match == FALSE]

  # 6. Merge: paper tools → match IDs → clean names → annotations

  # Keep only matched rows; attach tool_id from fuzzy match result
  out <- merge(
    paper_tools_long[paper_tool_idx %in% matched$input_id],
    matched[, .(paper_tool_idx = input_id, tool_id = target_id)],
    by = "paper_tool_idx"
  )

  # Attach clean tool name (deduplicate lookup to avoid fan-out)
  out <- merge(
    out,
    unique(dict_dt[, .(tool_id, tool_clean)]),
    by = "tool_id",
    all.x = TRUE
  )

  # Determine which annotation columns are present in tool_dt
  annot_cols <- intersect(
    c("category", "task_representation", "task_classification",
      "task_target", "software", "note"),
    names(tool_dt)
  )

  # Attach tool annotations; rename `tool` → `tool_clean` for join key
  out <- merge(
    out,
    tool_dt[, c(list(tool_clean = tool), .SD), .SDcols = annot_cols],
    by = "tool_clean",
    all.x = TRUE
  )

  # 7. Column order and sorting 
  fixed_cols <- c(
    "paper_tool_idx", "id", "doi", "file", "question",
    "tool_id", "tool_name_raw", "tool_clean"
  )
  extra_cols <- setdiff(names(out), c(fixed_cols, annot_cols))
  col_order  <- c(fixed_cols, annot_cols, extra_cols)
  data.table::setcolorder(out, intersect(col_order, names(out)))

  # Sort by paper id, then by the numeric index within each paper
  out[order(id, as.integer(sub(".*_", "", paper_tool_idx)))]
}

