library(tidyverse)
library(rio)
# remotes::install_github("JBGruber/rollama@engine")
library(rollama)
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
} else if (file.exists("../.Renviron")) {
  readRenviron("../.Renviron")
} else {
  cli::cli_alert_warning("NO API KEY")
}
setwd(here::here("analysis_visualization"))
options(
  rollama_server = "https://ai-openwebui.gesis.org",
  rollama_api_key = Sys.getenv("GESIS_API")
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
# Task: Clean and Standardize Tool Names

You are given a list of tool names extracted from academic papers. Many of these refer to the same tool but use different formatting, capitalization, or include extra words like "library", "package", "tool", etc.. A Second related issue is that some list entries contain multiple tools, which need to be separated.

Your task is to create a mapping from each original tool name to its standardized form.

## Guidelines for Standardization:

1. **Use the official/canonical name** of the tool (e.g., "scikit-learn" not "sklearn", "NLTK" not "nltk")
2. **Remove generic suffixes** like "library", "package", "tool", "API", "framework" unless they are part of the official name
3. **Preserve official capitalization** (e.g., "NLTK", "spaCy", "scikit-learn")
4. **Use hyphens or spaces as in the official name** (e.g., "scikit-learn" not "scikit learn")
5. **For versions**, remove version numbers unless the version is a distinct tool (e.g., "BERT" not "BERT-base", but "GPT-3" vs "GPT-4" are distinct)
6. **For acronyms**, use uppercase if that is the standard (e.g., "SVM", "LSTM", "BERT")
7. **Group variations together** - different capitalizations or separators of the same tool should map to one canonical form
8. **Split combined entries** - If the input contains multiple tools listed together (comma-separated, semicolon-separated, or in a list), extract and standardize each one separately


## Output Format

Return a JSON list with all tools mentioned in the input (I will add the rest of the json line later):

```json
["NLTK"]
```

## Examples

Input: "NLTK library"

Output: `["NLTK"]`

Input: "Natural Language Toolkit"

Output: `["NLTK"]`

Input: "Review Supervised Approaches: RF, NB, NBM, SVM"

Output: `["Random Forest", "Naive Bayes", "Multinomial Naive Bayes", "Support Vector Machine"]`

## Previously annotated tools

{tool_dict}

## Tool Name to Standardize:

{tool}
'

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
    model = "gpt-5",
    output = "text",
    verbose = FALSE,
    engine = "openwebui",
    model_params = list(seed = 42, temperature = 0)
  )
  # test output
  jsonlite::fromJSON(res)
  res <- str_replace_all(res, "\n", "")
  res <- paste0('{"', tool, '":', res, '}\n')
  readr::write_file(res, "1._tools_dict.json", append = TRUE)
}

annotated_tools_l <- jsonlite::stream_in(
  file("1._tools_dict.json"),
  simplifyVector = FALSE,
  verbose = FALSE
) |>
  unlist()

tool_dict <- tibble(
  tool = names(annotated_tools_l),
  tool_clean = annotated_tools_l
)

tool_dict$tool_clean |>
  unique() |>
  length()

# double check if all tools are in the tool dict
tools_unique$tool[!tools_unique$tool %in% tool_dict$tool] |>
  paste0(collapse = "\n") |>
  readr::write_file(file = "claude_prompt_2026.txt", append = T)


# intermediate resukt
results |>
  filter(
    variable %in% c("Q1_1_Tool-Name", "Q2_1_Tool-Name"),
    !result %in% c("-", "IRRELEVANT"),
    nchar(result) > 1
  ) |>
  separate_longer_delim(cols = result, delim = "; ") |>
  mutate(tool = result) |>
  left_join(tool_dict, by = "tool") |>
  filter(!is.na(tool_clean)) |>
  count(tool_clean, sort = TRUE) |>
  View()
