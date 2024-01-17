# deduplicate

# Intro

We noticed that the dataset still contained some duplicates (abstracts
that refer to the same article). This script removes them from the
database.

# merge

Get data from google drive with scopus abstracts:

``` r
drive_deauth()
data_scopus <- "scopus.csv"
if (!file.exists(data_scopus)) {
  drive_download(file ="https://drive.google.com/file/d/1uUKH5fNDYp6BWbXW-_bXCKfJ5Bu5vlNc/view?usp=drive_link",
                 path = data_scopus)
}

data_abstracts_scopus <- data.table::fread(data_scopus) |> 
  select(title = Title, author = `Author full names`, year = Year, doi = DOI, 
         outlet = `Source title`, type = `Document Type`, 
         abstract = Abstract) |> 
  as_tibble()
data_abstracts_scopus
```

    # A tibble: 11,447 × 7
       title                                author  year doi   outlet type  abstract
       <chr>                                <chr>  <int> <chr> <chr>  <chr> <chr>   
     1 Development of a patients’ satisfac… Khale…  2023 10.1… BMC H… Arti… Backgro…
     2 SENTIMENT ANALYSIS OF USER PREFEREN… Nurdi…  2023 10.2… Manag… Arti… The aim…
     3 The 2021 Nigerian Twitter ban: A te… Moham…  2023 10.1… Commu… Arti… The sus…
     4 AlgBERT: Automatic Construction of … Hamad…  2023 10.1… ACM T… Arti… Nowaday…
     5 Decoding consumer-centric transitio… Pani,…  2023 10.1… Resea… Arti… Climate…
     6 A Nationwide Sample of Adolescents … Allis…  2023 10.1… Journ… Arti… Purpose…
     7 A systematic review of applications… Kusal…  2023 10.1… Artif… Arti… Artific…
     8 Stakeholders' engagement through so… Shoko…  2023 10.1… Busin… Arti… The sta…
     9 Sentiment Analysis on the Classific… Kaban…  2023 10.1… AIP C… Conf… The sel…
    10 OKG: A Knowledge Graph for Fine-gra… Blin,…  2023 10.1… K-CAP… Conf… In rece…
    # ℹ 11,437 more rows

Get web of science data:

``` r
data_wos <- "wos.csv"
if (!file.exists(data_wos)) {
  curl::curl_download("https://ucloud.univie.ac.at/index.php/s/KcTY98rqT4rLj5S/download/wos.csv", 
                    destfile = data_wos)
}

data_abstracts_wos <- data.table::fread(data_wos) |> 
  select(title = `Article Title`, author = `Author Full Names`, year = `Publication Year`, 
         doi = DOI, outlet = `Source Title`, type = `Document Type`, 
         abstract = Abstract) |> 
  as_tibble()
data_abstracts_wos
```

    # A tibble: 5,778 × 7
       title                                author  year doi   outlet type  abstract
       <chr>                                <chr>  <int> <chr> <chr>  <chr> <chr>   
     1 The Political Biases of ChatGPT      Rozad…  2023 10.3… SOCIA… Arti… Recent …
     2 Inferring cancer disease response f… Tan, …  2023 10.1… JOURN… Arti… Objecti…
     3 The unreasonable effectiveness of l… Savel…  2023 10.3… FRONT… Arti… The eme…
     4 Co-Writing with Opinionated Languag… Jakes…  2023 10.1… PROCE… Proc… If larg…
     5 Sentiment analysis of online respon… Seong…  2023 10.1… HELIY… Arti… Opinion…
     6 Chat Generative Pre-trained Transfo… Chave…  2023 10.1… AMERI… Arti… With th…
     7 The promise and peril of using a la… Cherv…  2023 10.1… FERTI… Arti… Objecti…
     8 Phrase break prediction with bidire… Futam…  2021 10.2… INTER… Proc… We prop…
     9 A System for Interviewing and Colle… Shin,…  2023 10.3… APPLI… Arti… In case…
    10 Leveraging ChatGPT in the Pediatric… Karak…  2023 10.1… PEDIA… Arti… Backgro…
    # ℹ 5,768 more rows

1.  We merge these data and assign a unique ID:

``` r
data_abstracts <- bind_rows(data_abstracts_scopus, data_abstracts_wos) |> 
  mutate(id = as.character(row_number()), .before = 1L)
data_abstracts
```

    # A tibble: 17,225 × 8
       id    title                          author  year doi   outlet type  abstract
       <chr> <chr>                          <chr>  <int> <chr> <chr>  <chr> <chr>   
     1 1     Development of a patients’ sa… Khale…  2023 10.1… BMC H… Arti… Backgro…
     2 2     SENTIMENT ANALYSIS OF USER PR… Nurdi…  2023 10.2… Manag… Arti… The aim…
     3 3     The 2021 Nigerian Twitter ban… Moham…  2023 10.1… Commu… Arti… The sus…
     4 4     AlgBERT: Automatic Constructi… Hamad…  2023 10.1… ACM T… Arti… Nowaday…
     5 5     Decoding consumer-centric tra… Pani,…  2023 10.1… Resea… Arti… Climate…
     6 6     A Nationwide Sample of Adoles… Allis…  2023 10.1… Journ… Arti… Purpose…
     7 7     A systematic review of applic… Kusal…  2023 10.1… Artif… Arti… Artific…
     8 8     Stakeholders' engagement thro… Shoko…  2023 10.1… Busin… Arti… The sta…
     9 9     Sentiment Analysis on the Cla… Kaban…  2023 10.1… AIP C… Conf… The sel…
    10 10    OKG: A Knowledge Graph for Fi… Blin,…  2023 10.1… K-CAP… Conf… In rece…
    # ℹ 17,215 more rows

# deduplicate

We found two ways to get rid of duplicates. Articles with the same DOI:

``` r
# remove if same DOI
remove_df_doi <- data_abstracts |> 
  filter(!is.na(doi), duplicated(doi))
nrow(remove_df_doi)
```

    [1] 9407

2.  Articles where the cosine similarity of the document feature matrix
    is at or above 0.9. We validated the threshold by looking at
    examples around the thresholds 0.8, 0.85, 0.88 and 0.89. For all
    values below 0.9, we found that a majority of articles pairs were
    actually different articles, while the number of actual duplicates
    was negligible.

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
  slice_max(order_by = sel_val, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  filter(!duplicated(id))

nrow(remove_df_sim)
```

    [1] 9119

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
  filter(!duplicated(id))
```

The table below shows how many articles each exclusion criterion
identified:

| step               | n articles | percent |
|:-------------------|-----------:|:--------|
| Same Doi           |       9407 | 54.6%   |
| Same content       |       9119 | 52.9%   |
| Mean Opinion Score |        154 | 0.9%    |
| total              |      11973 | 69.5%   |

New dataset:

``` r
data_abstracts_clean <- data_abstracts |> 
  filter(!id %in% remove_df$id)
data_abstracts_clean
```

    # A tibble: 5,252 × 8
       id    title                          author  year doi   outlet type  abstract
       <chr> <chr>                          <chr>  <int> <chr> <chr>  <chr> <chr>   
     1 3     The 2021 Nigerian Twitter ban… Moham…  2023 10.1… Commu… Arti… The sus…
     2 4     AlgBERT: Automatic Constructi… Hamad…  2023 10.1… ACM T… Arti… Nowaday…
     3 6     A Nationwide Sample of Adoles… Allis…  2023 10.1… Journ… Arti… Purpose…
     4 7     A systematic review of applic… Kusal…  2023 10.1… Artif… Arti… Artific…
     5 9     Sentiment Analysis on the Cla… Kaban…  2023 10.1… AIP C… Conf… The sel…
     6 10    OKG: A Knowledge Graph for Fi… Blin,…  2023 10.1… K-CAP… Conf… In rece…
     7 12    Classifying Religious Trends … Jalil…  2023 10.1… AIP C… Conf… Social …
     8 13    Revealing People’s Sentiment … Calva…  2023 10.3… Compu… Arti… Social …
     9 14    The impact of the federal men… Hswen…  2023 10.1… Preve… Arti… The US …
    10 17    A CONCEPTUAL AQUILA MERGED AR… Sange…  2023 10.3… Journ… Arti… Sentime…
    # ℹ 5,242 more rows

We upload the new dataset without duplicates to drive:

``` r
gs4_auth()
gs4_create("Opinion Papers Abstracts", 
           sheets = data_abstracts_clean)
```

<https://docs.google.com/spreadsheets/d/1FBm9O3B1cU-sYXXzwfPbhI4ktHDeN_8QI8cD9sDIadc/edit?usp=sharing>
