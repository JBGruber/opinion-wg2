if (!reticulate::virtualenv_exists("r-opinion")) {
  reticulate::virtualenv_create("r-opinion")
  reticulate::virtualenv_install("r-opinion", c(
    "openparse",
    "pypdf",
    "langchain_community",
    "pyautogui",
    "pyperclip",
    "Pillow",
    "opencv-python"
  ))
}
reticulate::use_virtualenv("r-opinion")
library(reticulate)
library(tidyverse)

source("paper-annotation-gllm/annotate_pdf.r")

files <- list.files("/home/johannes/Desktop/opinion_pdfs/") |>
  setdiff(c(
    "10.1007_978-3-030-01057-7_62.pdf",
    "10.1007_978-3-030-37051-0_23.pdf",
    "10.1002_cpe.5909.pdf",
    "10.1007_978-3-030-03493-1_23.pdf",
    "10.1007_978-3-030-01851-1_42.pdf",
    "10.1007_978-3-030-05453-3_10.pdf",
    "10.1007_978-3-030-30244-3_57.pdf",
    "10.1007_978-3-030-32233-5_51.pdf",
    "10.1007_978-981-10-6520-0_20.pdf",
    "10.1007_978-981-13-5802-9_60.pdf",
    "10.1007_s10772-020-09730-x.pdf",
    "10.1007_s42452-019-1926-x.pdf",
    "10.1017_S0020818321000175.pdf",
    "10.1080_10919392.2019.1654350.pdf",
    "10.1080_13504851.2019.1619013.pdf", 
    "10.1097_SPV.0000000000000584.pdf",
    "10.1109_ACCESS.2019.2938854.pdf", 
    "10.1109_ICAIIC51459.2021.9415191.pdf",
    "10.1109_ICCCIS51004.2021.9397135.pdf", 
    "10.1109_ICDEW49219.2020.000-7.pdf",
    "10.1109_ICPET.2018.00024.pdf",
    "10.1109_rivf48685.2020.9140757.pdf",
    "10.1109_UCC48980.2020.00049.pdf",
    "10.1145_3274250.3274263.pdf",
     "10.1145_3314074.3314091.pdf"
  ))



# f <- files[1]
start <- Sys.time()
for (f in files) {
  annotate_pdf(pdf = f, verbose = TRUE)
}
end <- Sys.time()



pdf <- "10.1007_978-3-030-01057-7_62.pdf"


annotate_pdf(pdf = "10.1007_978-3-030-01057-7_62.pdf")
annotate_pdf(pdf = "10.1007_978-3-030-37051-0_23.pdf")

library(rvest)
cont <- read_html("paper-annotation-gllm/10.1002_cpe.5909.pdf.html")
answers <- cont |> 
  html_elements(".agent-turn .text-message") |> 
  html_text2()

dat <- str_extract(answers, "\\{.*\\}") |> 
  lapply(jsonlite::fromJSON)
df <- tibble(
    id = names(unlist(dat)),
    value = unlist(dat)
  )

  jsonlite::stream_in()

cont |> 
  html_elements("[class*='group/conversation-turn']") |> 
  html_text2()

html <- read_html('<div class="group/conversation-turn test"> Some stuff </div>')
html %>% 
  html_nodes("[class*='group/conversation-turn']")
