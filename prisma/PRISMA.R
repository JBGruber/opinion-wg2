library(PRISMAstatement)

flow.chart <- flow_exclusions(
  incl_counts = c(17255, 5252, 1002, 554),
  total_label = "Hits in Scopus and WoS",
  incl_labels = c("Population of deduplicated articles", "Stratified sample of abstracts", "Sample of relevant abstracts"),
  excl_labels = c("Removal of duplicates", "Removal via sampling", "Removal of irrelevant abstracts"),
)

#has to be saved manually (500, 300 as size for export)
flow.chart


