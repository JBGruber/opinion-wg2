library(tidyverse)
data <- rio::import("paper-annotation-gllm/2._annotation-results.csv")
export_long <- data |> 
  filter(str_detect(variable, "^Q5"),
         result != "IRRELEVANT") |> 
  mutate(result = ifelse(result == "-", NA_character_, result))
export_wide <- export_long |> 
  pivot_wider(id_cols = c(id, doi, file), names_from = variable, values_from = result)

zip(
  zipfile = "/home/johannes/Dropbox/opinion_pdfs/wg3_export.zip",
  files = file.path("/home/johannes/Dropbox/opinion_pdfs", export_wide$file)
)

rio::export(export_long, "wg3_export_long.csv")
rio::export(export_wide, "wg3_export_wide.csv")
