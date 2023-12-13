# deduplicate

# Intro

We noticed that the dataset still contained some duplicates (abstracts
that refer to the same article). This script removes them from the
databas.

Get old data

``` r
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
data_abstracts
```

    # A tibble: 12,888 × 8
       id    title                          author  year doi   outlet type  abstract
       <chr> <chr>                          <chr>  <int> <chr> <chr>  <chr> <chr>   
     1 1     Phrase break prediction with … Futam…  2021 10.2… INTER… Proc… We prop…
     2 2     Salience-Aware Event Chain Mo… Zhang…  2021 <NA>  2021 … Proc… Storyte…
     3 3     In Validations We Trust? The … Song,…  2020 10.1… POLIT… Arti… Politic…
     4 4     The Internet in China: New Me… Zhang…  2018 10.1… JOURN… Revi… This es…
     5 5     Storytelling and the Italian … Faggi…  2019 <NA>  QUALI… Arti… The tra…
     6 6     Public opinion on megaproject… Kundu…  2023 10.1… PUBLI… Arti… Megapro…
     7 7     Don't Read the Comments: Exam… Murib…  2022 10.3… LAWS   Arti… How are…
     8 8     Deceptive (De)humanization: H… Marko…  2023 10.1… JOURN… Arti… This pa…
     9 9     The Twittering Presidents An … Wigne…  2021 10.1… JOURN… Arti… This pa…
    10 10    A Multitheoretical Approach t… Margo…  2018 10.1… COMMU… Arti… This ar…
    # ℹ 12,878 more rows

These are the examples we manually identified:

``` r
# 
ex1 <- data_abstracts |> 
  filter(id %in% c(1017, 9873))
ex1
```

    # A tibble: 2 × 8
      id    title                           author  year doi   outlet type  abstract
      <chr> <chr>                           <chr>  <int> <chr> <chr>  <chr> <chr>   
    1 1017  Classification of Code-Mixed B… Singh…  2021 10.4… INTER… Arti… The rap…
    2 9873  Classification of code-mixed b… Singh…  2021 10.4… Inter… Arti… The rap…

``` r
ex2 <- data_abstracts |> 
  filter(id %in% c(4780, 9581))
ex2
```

    # A tibble: 2 × 8
      id    title                           author  year doi   outlet type  abstract
      <chr> <chr>                           <chr>  <int> <chr> <chr>  <chr> <chr>   
    1 4780  Unveiling: An Unexpected Mid-c… Bridg…  2021 10.1… JOURN… Arti… Strong …
    2 9581  Unveiling: An unexpected mid-c… Bridg…  2021 10.1… Journ… Arti… Strong …

``` r
ex3 <- data_abstracts |> 
  filter(id %in% c(4054, 12804))
ex3
```

    # A tibble: 2 × 8
      id    title                           author  year doi   outlet type  abstract
      <chr> <chr>                           <chr>  <int> <chr> <chr>  <chr> <chr>   
    1 4054  Decompositional Argument Minin… Gemec…  2019 <NA>  57TH … Proc… This wo…
    2 12804 Decompositional argument minin… Gemec…  2020 <NA>  ACL 2… Conf… This wo…

We found two ways to get rid of duplicates. Articles with the same DOI:

``` r
# remove if same DOI
remove_df_doi <- data_abstracts |> 
  filter(!is.na(doi), duplicated(doi))
nrow(remove_df_doi)
```

    [1] 1323

Articles where the cosine similarity of the document feature matrix is
at or above 0.9. We validated the threshold by looking at examples
around the thresholds 0.8, 0.85, 0.88 and 0.89. For all values below
0.9, we found that a majority of articles pairs were actually different
articles, while the number of actual duplicates was negligible.

``` r
data_abstracts_small <- data_abstracts |>
  select(id, title, abstract, year, doi)

sim_df <- data_abstracts |> 
  # we check a combination of title and abstract
  mutate(text = paste(title, "-", abstract)) |> 
  corpus(docid_field = "id",
         text_field = "text") |> 
  tokens(remove_punct = TRUE, remove_symbols = TRUE) |> 
  dfm() |> 
  textstat_simil(method = "cosine", min_simil = 0.9) |> 
  igraph::graph_from_adjacency_matrix(weighted = "cosine") |> 
  tidygraph::as_tbl_graph() |> 
  tidygraph::activate("edges") |>
  tidygraph::mutate(id1 = .N()$name[from],
                    id2 = .N()$name[to]) |> 
  as_tibble() |> 
  filter(!is.na(cosine), from != to) |> 
  left_join(data_abstracts_small, by = c("id1" = "id")) |> 
  left_join(data_abstracts_small, by = c("id2" = "id"), suffix = c("_from", "_to"))

remove_df_sim <- sim_df |> 
  mutate(pair_id = row_number()) |> 
  select(pair_id, id1, id2) |> 
  pivot_longer(cols = -pair_id, values_to = "id") |> 
  left_join(data_abstracts_small, by = "id") |> 
  mutate(sel_val = ifelse(is.na(doi), year - 5, year)) |> 
  group_by(pair_id) |> 
  slice_max(order_by = sel_val, n = 1, with_ties = FALSE)

nrow(remove_df_sim)
```

    [1] 1959

Additionally, we noticed that articles about “Mean Opinion Score” are
never relevant to our research, so we remove them as well.

``` r
# remove "Mean Opinion Score" abstracts
remove_df_mos <- data_abstracts |> 
  filter(str_detect(abstract, fixed("Mean Opinion Score", ignore_case = TRUE)) |
           str_detect(title, fixed("Mean Opinion Score", ignore_case = TRUE)))
```

``` r
remove_df <- bind_rows(
  remove_df_doi,
  remove_df_mos,
  remove_df_sim
) |> 
  filter(!duplicated((id)))
```

The table below shows how many articles each exclusion criterion
identified:

| step               | n articles | percent |
|:-------------------|-----------:|:--------|
| Same Doi           |       1323 | 10.3%   |
| Same content       |       1959 | 15.2%   |
| Mean Opinion Score |        127 | 1.0%    |
| total              |       3281 | 25.5%   |

We upload the new dataset without duplicates to drive:

``` r
data_abstracts_clean <- data_abstracts |> 
  filter(!id %in% remove_df$id)

gs4_auth()
gs4_create("Opinion Papers Abstracts", 
           sheets = data_abstracts_clean)
```

<https://docs.google.com/spreadsheets/d/1FBm9O3B1cU-sYXXzwfPbhI4ktHDeN_8QI8cD9sDIadc/edit?usp=sharing>
