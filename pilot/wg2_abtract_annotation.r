library(googlesheets4)
library(tidyverse)
library(annotinder)

# todo
# welcome question: who are you?
# sent mail about annotation pilot
# brainstorming document, dataset, new zoom doodle
# 

codebook <- create_codebook(
  question("relevant", 
           "Is this research that measures a human opinion (however it is defined)?",
           codes = list(
             code("No", color = "crimson", makes_irrelevant = "REMAINING"),
             code("Yes", color = "lightgreen")
           ),
           type = "annotinder"
  ),
  question("relevant2", 
           "Is this research that uses a tool?",
           codes = list(
             code("Yes", color = "lightgreen"),
             code("No", color = "crimson", makes_irrelevant = "REMAINING"),
             code("Overview", color = "grey", makes_irrelevant = "tool")
           )
  ),
  question(
    "tool",
    question = "Which tool is mentioned in the abstract?",
    instruction = "What software application or model or dictionary is employed by  the research to measure the concept (e.g., R, Python, quanteda, sklearnAPI to access the model)?",
    type = "inputs"
  ),
  question(
    "concept",
    question = "Which concept is mentioned in the abstract?",
    instruction = "What understanding or sub-category of opinion is used here (e.g. political bias, stance, viewpoint that can’t be validated empirically)?",
    type = "inputs"
  ),
  question(
    "approach",
    question = "Which approach is mentioned in the abstract?",
    instruction = "How would you categorize the tool (one or several of: dictionary approach, machine learning, large language model, regression	, extraction of parts of text, unsupervised grouping, supervised classification, more?)",
    type = "inputs"
  ),
  question(
    "language",
    question = "Which natural language are the measured opinions expressed in?",
    type = "inputs"
  ),
  question(
    "definition",
    question = "If defined in the abstract, how do the authors understand opinion?",
    type = "inputs"
  )
)

welcome <- create_unit(
  "id",
  type = "train",
  set_text("text", "Welcome to AnnoTinder!"),
  set_question(name = "welcome", question = "Who are you", type = "inputs")
)

d <- read_sheet("https://docs.google.com/spreadsheets/d/1Yd-c8_nL89ue3tSU-QN2NyAIR3LOKtSVoHAqe_dT6Lw/edit#gid=0")
d_new <- d |>
  mutate(
    text = sprintf("## %s\n\n%s", `Article Title`, Abstract),
    text = str_replace_all(
      text,
      regex("(\\bopinion.*?\\b|\\battitud.*?\\b|\\bideolog.*?\\b|\\bstance.*?\\b|\\bposition.*?\\b)", ignore_case = TRUE),
      "**\\1**"
    )
  ) |>
  arrange(-`Times Cited, All Databases`)

units <- d_new |>
  head(120) |>
  create_units(id = "ID", 
               set_markdown("text", text),
               set_markdown("link", paste0('[', `DOI Link`,'](', `DOI Link`,')')))

# job <- create_job("wg2-abstracts", units, codebook)
# job_db <- create_job_db(job, overwrite = T)
# start_annotator(job_db)

backend_connect(host = "https://cost-tools.up.railway.app",
                username = "johannesb.gruber@gmail.com",
                .password = Sys.getenv("ANNOTINDER_PW"))

job_id <- upload_job(
  title = "wg2-abstracts",
  units = c(welcome, units),
  codebook = codebook,
  rules = rules_crowdcoding()
)



