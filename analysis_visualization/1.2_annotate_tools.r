library(tidyverse)
library(rio)
library(rollama)
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}
setwd(here::here("analysis_visualization"))
options(
  rollama_server = "https://ai-openwebui.gesis.org/ollama/",
  rollama_headers = list(
    Authorization = paste("Bearer", Sys.getenv("GESIS_API"))
  )
)
rollama::ping_ollama()
annotated_tools_l <- jsonlite::stream_in(
  file("1.1_tools_dict.json"),
  simplifyVector = FALSE,
  verbose = FALSE
) |>
  unlist()

tool_dict <- tibble(
  tool = names(annotated_tools_l),
  tool_clean = annotated_tools_l
) |>
  distinct(tool, .keep_all = TRUE)

tools_df <- import("2._annotation-results.csv") |>
  filter(
    variable %in% c("Q1_1_Tool-Name", "Q2_1_Tool-Name"),
    !result %in% c("-", "IRRELEVANT"),
    nchar(result) > 1
  ) |>
  separate_longer_delim(cols = result, delim = "; ") |>
  rename(tool = result) |>
  left_join(tool_dict, by = "tool")


prompt <- readr::read_file("1.1_standardization_prompt.md")
schema <- list(
  type = "object",
  properties = list(
    category = list(
      type = "string",
      enum = c(
        "approach",
        "programming_language",
        "infrastructure",
        "ide",
        "general_library",
        "algorithm",
        "model",
        "linguistic_resource",
        "data_collection",
        "general_purpose_nlp",
        "single_purpose_nlp",
        "gui_tool",
        "commercial_api",
        "other_unclear"
      ),
      description = "The category of the tool"
    ),
    opinion_measurement_general = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this is a general opinion measurement tool"
    ),
    opinion_measurement_specific = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this is a specific opinion measurement tool"
    ),
    software = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this is software"
    ),
    note = list(
      type = "string",
      description = "Brief explanation of the tool (1-2 sentences)"
    )
  ),
  required = c(
    "category",
    "opinion_measurement_general",
    "opinion_measurement_specific",
    "software",
    "note"
  )
)

pb <- list(
  clear = TRUE,
  format = c(
    "{cli::pb_spin} {getOption('model')} {?is/are} thinking about ",
    "{cli::pb_total - cli::pb_current}/{cli::pb_total} question{?s}",
    "[ETA: {cli::pb_eta}]"
  )
)

req_perform_parallel_cache <- function(reqs, paths, ...) {
  cached <- file.exists(paths)

  httr2::req_perform_parallel(
    reqs = reqs[!cached],
    paths = paths[!cached],
    ...
  )
  map_chr(paths, readr::read_file)
}

path_safe <- function(x) {
  str_replace_all(x, "[^A-z.0-9]", "_")
}

tools_annotated <- tools_df |>
  count(tool = tool_clean) |>
  filter(!is.na(tool)) |>
  slice_max(n, n = 50) |>
  mutate(
    query = make_query(
      text = tool,
      prompt = prompt,
      template = "\n{prompt}\n{text}"
    ),
    request = query(
      query,
      screen = FALSE,
      model = "gpt-oss:120b",
      output = "httr2_request",
      format = schema,
      model_params = list(seed = 42, temperature = 0)
    ),
    cache_file = paste0("reqs/", path_safe(tool), ".json"),
    annotation = req_perform_parallel_cache(
      reqs = request,
      paths = cache_file,
      on_error = "continue",
      progress = pb
    )
  )

saveRDS(tools_annotated, "1.2_tools_annotated_raw.rds")

tools_annotated_df <- tools_annotated |>
  mutate(
    annotation_data = map(annotation, jsonlite::fromJSON),
    annotation_data = purrr::map_chr(annotation_data, c("message", "content")),
    annotation_data = map(annotation_data, jsonlite::fromJSON)
  ) |>
  unnest_wider(annotation_data)

saveRDS(tools_annotated, "1.2_tools_annotated_long.rds")
tools_annotated_df |>
  select(-request, -cache_file, -query, -annotation) |>
  export("1.2_tools_annotated.xlsx")
