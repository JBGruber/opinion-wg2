library(googlesheets4)
library(tidyverse)
library(annotinder)

codebook <- create_codebook(
  question("relevant-1", 
           "Is this research that analyses human opinion (however it is defined)?",
           codes = list(
             code("No", color = "crimson", makes_irrelevant = "REMAINING"),
             code("Yes", color = "lightgreen")
           ),
           type = "annotinder"
  ),
  question(
    "concept",
    question = "What understanding or sub-category of opinion is measured here?",
    instruction = "There should be at least one named in the abstract (otherwise, why did you select 'yes' in the first question?). Hint: you can look in the full paper if you are unsure. If an explicit definition is given, select the last option.",
    type = "dropdown",
    codes = list(
      code("attitude"),
      code("emotions"),
      code("evaluation"),
      code("other bias"),
      code("polarity"),
      code("political bias"),
      code("sentiment"),
      code("stance"),
      code("viewpoint"),
      code("opinion (no other definition)"),
      code("other or explicit specific definition of opinion", required_for = "concept-other")
    )
  ),
  question(
    "concept-other",
    question = "What understanding or sub-category of opinion is measured here?",
    instruction = "You selected other in the last question. Write in the concept mentioned in the abstract.",
    type = "inputs"
  ),
  question("relevant-2", 
           "Is this research that uses a tool?",
           instruction = "A tool is currently defined as \"a systematic mechanism or instrument, whether algorithmic or computational, designed or used to automate the extraction of opinions within textual data\". If a tool is hinted at, but not explicitly named in the abstract, select Yes (implicitly).",
           codes = list(
             code("Yes (explicitly)", color = "darkgreen"),
             code("Yes (implicitly)", color = "lightgreen", makes_irrelevant = "tool"),
             code("No", color = "crimson", makes_irrelevant = "REMAINING"),
             code("Overview", color = "grey", makes_irrelevant = c("approach", "approach-other", "tool", "tool-other"))
           ),
           type = "buttons"
  ),
  question(
    "approach",
    question = "How would you categorize the measurment approach of the tool?",
    type = "dropdown",
    codes = list(
      code("dictionary approach"),
      code("classic supervised machine learning classification"),
      code("classic unsupervised machine learning classification"),
      code("deep learning supervised classification"),
      code("deep learning unsupervised classification"),
      code("regression analysis"),
      code("large language model"),
      code("extraction of parts of text"),
      code("other or multiple", required_for = "approach-other")
    )
  ),
  question(
    "approach-other",
    question = "How would you categorize the measurment approach of the tool?",
    instruction = "You selected 'other or multiple' in the last question. Write in one or several concepts. The suggested categories were: dictionary approach, classic supervised machine learning classification, classic unsupervised machine learning classification, deep learning supervised classification, deep learning unsupervised classification, regression analysis, large language model, extraction of parts of text",
    type = "inputs"
  ),
  question(
    "tool",
    question = "What software application or model or dictionary is employed by the research to measure the concept?",
    instruction = "Our heuristic here is: does the abstract sound like a tool was developed or a specific tool was used to do the analysis (e.g., R, Python, Vader, quanteda, sklearn, a specific API to access a model)? We are looking for specific names of these tools",
    type = "dropdown",
    codes = list(
      code("aspect-based opinion mining (abom)"),
      code("BERT"),
      code("covidsenti"),
      code("ntuitionistic fuzzy sets (ifss)"),
      code("pair-wise aspect and opinion terms extraction (paote)"),
      code("relation aware collaborative learning (racl) framework"),
      code("rost content mining system"),
      code("sentic gcn"),
      code("vader"),
      code("vosviewer"),
      code("wordnet"),
      code("other", required_for = "tool-other")
    )
  ),
  question(
    "tool-other",
    question = "What software application or model or dictionary is employed by the research to measure the concept?",
    instruction = "You selected 'other' in the last question. Write in one or several tools mentioned in the abstract or go back to the question 'Is this research that uses a tool?' if no tool is mentioned after all.",
    type = "inputs"
  ),
  question("relevant-data", 
           question = "Does the abstract mention or hint which data(set) was analysed?",
           codes = list(
             code("No", color = "crimson", makes_irrelevant = "REMAINING"),
             code("Yes", color = "lightgreen")
           ),
           type = "annotinder"
  ),
  question(
    "data-kind",
    question = "What kind of dataset is referenced?",
    type = "dropdown",
    codes = list(
      code("Benchmark set", makes_irrelevant = c("islang", "data-lang", "iscountry", "data-country")),
      code("Own collection"),
      code("None explicitly", makes_irrelevant = "REMAINING")
    )
  ),
  question(
    "data-source",
    question = "Where does the data that opinions are measured in come from",
    instruction = "e.g., twitter, survey, video, audion, news, specific benchmark dataset(s)",
    type = "inputs"
  ),
  question("islang", 
           question = "Does the abstract mention or hint at the natural language analysed (e.g., if a unilingual country is mentioned)?",
           codes = list(
             code("No", color = "crimson", makes_irrelevant = "data-lang"),
             code("Yes", color = "lightgreen")
           ),
           type = "annotinder"
  ),
  question(
    "data-lang",
    question = "Which natural language are the measured opinions expressed in?",
    instruction = "e.g., English, German, Dutch, French etc.",
    type = "inputs"
  ),
  question("iscountry", 
           question = "Does the abstract mention or hint at the country analysed?",
           codes = list(
             code("No", color = "crimson", makes_irrelevant = "data-country"),
             code("Yes", color = "lightgreen")
           ),
           type = "annotinder"
  ),
  question(
    "data-country",
    question = "In which country was the data collected?",
    type = "inputs"
  )
)

welcome <- create_unit(
  "id",
  type = "train",
  set_text("text", "Welcome to AnnoTinder!"),
  set_question(name = "welcome", question = "Who are you", type = "inputs")
)



d <- read_sheet("https://docs.google.com/spreadsheets/d/1Yd-c8_nL89ue3tSU-QN2NyAIR3LOKtSVoHAqe_dT6Lw/edit#gid=0") |>
  mutate(
    text = sprintf("## %s\n\n%s", `Article Title`, Abstract),
    text = str_replace_all(
      text,
      regex("(\\bopinion.*?\\b|\\battitud.*?\\b|\\bideolog.*?\\b|\\bstance.*?\\b|\\bposition.*?\\b)", ignore_case = TRUE),
      "**\\1**"
    )
  ) |>
  arrange(-`Times Cited, All Databases`)

saveRDS(d, "data/abstracts.rds")

set.seed(1)
units <- d |>
  slice_sample(n = 50) |>
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
  units = units,
  debrief = debrief("Thank you for your participation in the coding task! You can add your name to the list of co-authors if you haven't done so yet", 
                    link = "https://docs.google.com/spreadsheets/d/1dk6LlYgliuD0OdF5p7_UAXeVrNDqiFkTtooX-BLh8Xs/edit?usp=sharing",
                    link_text = "Google Sheet of coauthors"),
  pre = welcome,
  codebook = codebook,
  rules = rules_fixedset()
)
job_id
