library(tidyverse)
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
  skip = 1
)

ex1 <- data_abstracts |> 
  filter(id %in% c(1017, 9873))

ex2 <- data_abstracts |> 
  filter(id %in% c(4780, 9581))

ex3 <- data_abstracts |> 
  filter(id %in% c(4054, 12804))