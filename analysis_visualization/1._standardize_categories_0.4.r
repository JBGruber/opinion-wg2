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
results <- import("2._annotation-results.csv")
tools_unique <- results |>
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
  select(tool, tool_clean) |>
  distinct(tool_clean, .keep_all = TRUE)

standardization_prompt <- '
# Task: Standardize Tool Names

You are given a list of tool names extracted from academic papers. Many of these refer to the same tool but use different formatting, capitalization, or include extra words like "library", "package", "tool", etc.

Your task is to create a mapping from each original tool name to its standardized form.

## Guidelines for Standardization:

1. **Use the official/canonical name** of the tool (e.g., "scikit-learn" not "sklearn", "NLTK" not "nltk")
2. **Remove generic suffixes** like "library", "package", "tool", "API", "framework" unless they are part of the official name
3. **Preserve official capitalization** (e.g., "NLTK", "spaCy", "scikit-learn")
4. **Use hyphens or spaces as in the official name** (e.g., "scikit-learn" not "scikit learn")
5. **For versions**, remove version numbers unless the version is a distinct tool (e.g., "BERT" not "BERT-base", but "GPT-3" vs "GPT-4" are distinct)
6. **For acronyms**, use uppercase if that is the standard (e.g., "SVM", "LSTM", "BERT")
7. **Group variations together** - different capitalizations or separators of the same tool should map to one canonical form

## Output Format:

Return a JSON object where keys are the original tool names and values are the standardized names:

```json
{{
  "NLTK library": "NLTK",
  "nltk": "NLTK",
  "Natural Language Toolkit": "NLTK",
  "scikit learn": "scikit-learn",
  "sklearn": "scikit-learn"
}}
```

## Previously annotated tools

{tool_dict}

## Tool Name to Standardize:

{tool}
'

# Define schema for standardization output
standardization_schema <- list(
  type = "object",
  additionalProperties = list(
    type = "string"
  ),
  description = "Mapping from original tool names to standardized names"
)

for (tool in tools_unique$tool) {
  message(tool)
  if (file.exists("1._tools_dict.json")) {
    tool_dict <- readr::read_file("1._tools_dict.json")
    suppressWarnings({
      annotated_tools <- jsonlite::stream_in(
        file("1._tools_dict.json"),
        simplifyVector = FALSE,
        verbose = FALSE
      ) |>
        map(names) |>
        unlist()
    })
    if (tool %in% annotated_tools) next
  } else {
    tool_dict <- ""
  }
  res <- query(
    q = glue::glue(standardization_prompt),
    screen = FALSE,
    model = "gpt-oss:120b",
    output = "text",
    format = standardization_schema,
    verbose = FALSE,
    model_params = list(seed = 42, temperature = 0)
  )
  # test output
  jsonlite::fromJSON(res)
  res <- str_replace_all(res, "\n", "")
  res <- paste0("\n", res)
  readr::write_file(res, "1._tools_dict.json", append = TRUE)
}


prompt <- readr::read_file("1._prompt_v0.7.md")
schema <- list(
  type = "object",
  properties = list(
    category = list(
      type = "string",
      enum = c(
        "approach",
        "programming_language",
        "ide",
        "general_library",
        "nlp_library",
        "algorithm",
        "model",
        "linguistic_resource",
        "data_collection",
        "infrastructure",
        "gui_tool",
        "opinion_mining_tool",
        "commercial_api",
        "other_unclear"
      ),
      description = "The category of the tool"
    ),
    opinion_mining_tool = list(
      type = "string",
      enum = c("yes", "no", "unclear"),
      description = "Whether this is an opinion mining tool"
    ),
    note = list(
      type = "string",
      description = "Briefly describe what the tool does and why you assigned it to this category"
    )
  ),
  required = c("category", "opinion_mining_tool", "note")
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
  sample_n(size = 100) |>
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
  unnest_wider(annotation_data)

saveRDS(tools_annotated, "tools_annotated.rds")
tools_annotated_df |>
  distinct(tool_clean, .keep_all = TRUE) |>
  select(-request, -cache_file, -query) |>
  export("tools_annotated_df.xlsx")

# ---- Tool Name Standardization ----

# Get all unique tool names from annotated data
all_tool_names <- tools_unique |>
  pull(tool) |>
  unique() |>
  sort()

tools_annotated_final |>
  group_by(tool_name_clean) |>
  summarise(
    tool_original = toString(tool),
    category = toString(category),
    opinion_mining_tool = toString(opinion_mining_tool),
    n = n(),
    note = toString(note),
  ) |>
  mutate(note = str_trunc(note, 32000)) |>
  export("tools_annotated_final_counts.xlsx")
