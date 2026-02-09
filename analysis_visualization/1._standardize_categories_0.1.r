library(tidyverse)
library(rio)
setwd(here::here("analysis_visualization"))
results <- import("2._annotation-results.csv")
library(rollama)
Sys.setenv(GESIS_API = "sk-60c06822e07d486c8f0c8fff71a453da")
options(
  rollama_server = "https://ai-openwebui.gesis.org/ollama/",
  rollama_headers = list(
    Authorization = paste("Bearer", Sys.getenv("GESIS_API"))
  )
)
rollama::ping_ollama()
tools <- results |>
  filter(
    variable %in% c("Q1_1_Tool-Name", "Q2_1_Tool-Name"),
    !result %in% c("-", "IRRELEVANT"),
    nchar(result) > 1
  ) |>
  separate_longer_delim(cols = result, delim = "; ") |>
  mutate(
    tool = result,
    tool_clean = tool |>
      str_replace("[-_]", " ") |>
      tolower() |>
      trimws()
  ) |>
  select(tool, tool_clean)

tools_unique <- tools |>
  distinct(tool_clean, .keep_all = TRUE)


prompt <- readr::read_file("1._prompt_v0.5.md")
schema <- list(
  type = "object",
  properties = list(
    category = list(
      type = "string",
      enum = c(
        "algorithm",
        "model",
        "linguistic_resource",
        "general_purpose_nlp",
        "single_purpose_nlp",
        "general_library",
        "approach",
        "programming_language",
        "ide",
        "data_collection",
        "infrastructure",
        "gui_tool",
        "commercial_api",
        "other_unclear"
      ),
      description = "The category of the tool"
    ),
    opinion_measurement_general = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this tool is used for general opinion measurement"
    ),
    opinion_measurement_specific = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this tool is used for specific opinion measurement"
    ),
    software = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this is software"
    ),
    tool_name_clean = list(
      type = "string",
      description = "Cleaned/standardized tool name"
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
    "tool_name_clean",
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

tools_annotated <- tools_unique |>
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
    cache_file = paste0("reqs/", path_safe(tool_clean), ".json"),
    annotation = req_perform_parallel_cache(
      reqs = request,
      paths = cache_file,
      on_error = "continue",
      progress = pb
    )
  )

saveRDS(tools_annotated, "tools_annotated.rds")

tools_annotated_df <- tools_annotated |>
  mutate(
    annotation_data = map(annotation, jsonlite::fromJSON),
    annotation_data = purrr::map_chr(annotation_data, c("message", "content")),
    annotation_data = map(annotation_data, jsonlite::fromJSON)
  ) |>
  unnest_wider(annotation_data) |>
  mutate(
    tool_name_clean = ifelse(
      tool_name_clean == tool_name_clean,
      tool,
      tool_name_clean
    )
  )

saveRDS(tools_annotated, "tools_annotated.rds")
tools_annotated_df |>
  distinct(tool_clean, .keep_all = TRUE) |>
  select(-request, -cache_file, -query) |>
  export("tools_annotated_df.xlsx")

tools_annotated_df_clean <- tools_annotated_df |>
  select(-request, -cache_file, -query)

tools |>
  left_join(tools_annotated_df_clean, by = "tool") |>
  group_by(tool_name_clean) |>
  summarise(
    tool = toString(tool),
    category = toString(category),
    opinion_measurement_general = toString(opinion_measurement_general),
    opinion_measurement_specific = toString(opinion_measurement_specific),
    software = toString(software),
    n = n(),
    note = toString(note),
  ) |>
  mutate(note = str_trunc(note, 32000)) |>
  export("tools_annotated_counts.xlsx")
