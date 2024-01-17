library(googlesheets4)
gs4_deauth()
library(tidyverse)
library(annotinder)

codebook <- create_codebook(
  question("relevant-1", 
           "Is this research that applies, reviews, or develops an approach for measuring human opinion in textual communication?",
           codes = list(
             code("No (or unclear)", color = "crimson", makes_irrelevant = "REMAINING"),
             code("Yes", color = "lightgreen")
           ),
           instruction = "- Related to “applies, reviews, or develops”: Select yes for empirical studies, overviews (e.g., systematic reviews, narrative reviews, meta analysis) and methodological papers.
- Related to “human opinion”: Select yes if
  - The abstract explicitly claims that it measures human opinion (Even if you judge the measurement approach unfit to measure actual human opinion)
  - The abstract implicitly claims that it measures opinion by stating that it investigates peoples \"views or thoughts about someone or something\" (the definition is from Cambridge dictionary)
  - The abstract explicitly measures one of the following subconcepts (e.g., sentiment, emotions, stance, polarity, evaluation, viewpoint, attitude).
- Related to “textual communication”: Select yes if the measurement of human opinion relates to textual communication in any form (even when e.g., transcribed from audio)
- If the abstract provides insufficient information for coders to evaluate whether this relevance criterion is fulfilled (i.e., because it is unclear whether opinion is measured), vote No (unclear). This also applies if the abstract is so poorly written that it is simply unclear what the paper is doing.",
type = "annotinder"
  ),
  # annotation_variable(
  #   "concept",
  #   instruction = "Is a specific understanding or sub-category of opinion mentioned here?",
  #   codes = c(concept = "darkgreen")
  # ),
  # question("relevant-2", 
  #          "What type of a study on human opinion is this?",
  #          codes = list(
  #            code("Methodological paper", color = "#5BBCD6"),
  #            code("Empirical study", color = "lightgreen"),
  #            code("Overview", makes_irrelevant = "REMAINING", color = "#FDD262"),
  #            code("something else", makes_irrelevant = "REMAINING", color = "#D3DDDC")
  #          ),
  #          type = "buttons"
  # ),
  # question("relevant-3",
  #          "Is this research that applies or develops a computational text analysis measurement approach?",
  #          codes = list(
  #            code("No (or unclear)", color = "crimson"),
  #            code("Yes", color = "lightgreen")
  #          ),
  #          instruction = "Most  methodological papers also contain an empirical part - to showcase a new method or approach. The category “methodological paper” should take precedence if this is the main focus of the article.",
  #          type = "annotinder"
  # )
  question("relevant-3",
           "Please explain your coding decision (you can also copy and paste from the abstract)",
           type = "inputs"
  )
)


welcome <- create_unit(
  "id",
  type = "train",
  set_text("text", "Welcome to AnnoTinder!"),
  set_question(name = "welcome", question = "Who are you", type = "inputs")
)

library(googledrive)
drive_deauth()
data_file <- "scopus_wos_merged.csv"
if (!file.exists(data_file)) {
  drive_download(file ="https://drive.google.com/file/d/16KU7tCFyWWy9doF3hkz8JYd4bRqLxMLK/view?usp=drive_link",
                 path = data_file)
}

data_abstracts <- read_csv(
  file = data_file, 
  col_names = c("id", "title", "author", "year", "doi", "outlet", "type", "abstract"), 
  col_types = "cccicccc",
  skip = 1L
) |>
  mutate(
    text = sprintf("## %s\n\n%s", title, abstract),
    text = str_replace_all(
      text,
      regex("(\\bopinion.*?\\b|\\battitud.*?\\b|\\bideolog.*?\\b|\\bstance.*?\\b|\\bposition.*?\\b|\\bbeli.*?\\b|\\bfeeling.*?\\b|\\bsentiment.*?\\b)", ignore_case = TRUE),
      "**\\1**"
    )
  )

set.seed(1)
units <- data_abstracts |>
  slice_sample(n = 50) |> 
  create_units(id = "id", 
               set_markdown("text", text))


units <- data_abstracts |>
  slice_sample(n = 10) |> 
  create_units(id = "id", 
               set_markdown("text", text))


units <- data_abstracts |>
  slice_sample(n = 10) |> 
  create_units(id = "id", 
               set_markdown("text", text))

units <- data_abstracts |>
  slice_sample(n = 15) |> 
  create_units(id = "id", 
               set_markdown("text", text))

 # job <- create_job("wg2-abstracts", units, codebook)
 # job_db <- create_job_db(job, overwrite = T)
 # start_annotator(job_db)

backend_connect(host = "https://cost-tools.up.railway.app",
                username = "johannesb.gruber@gmail.com",
                .password = Sys.getenv("ANNOTINDER_PW"))

job_id <- upload_job(
  title = "wg2-abstracts-icr2",
  units = units,
  debrief = debrief("Thank you for your participation in the coding task! You can add your name to the list of co-authors if you haven't done so yet", 
                    link = "https://docs.google.com/spreadsheets/d/1dk6LlYgliuD0OdF5p7_UAXeVrNDqiFkTtooX-BLh8Xs/edit?usp=sharing",
                    link_text = "Google Sheet of coauthors"),
  pre = welcome,
  codebook = codebook,
  rules = rules_fixedset()
)
job_id
