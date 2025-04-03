# GISAIDR

[![Build](https://github.com/Wytamma/GISAIDR/actions/workflows/r.yml/badge.svg)](https://github.com/Wytamma/GISAIDR/actions/workflows/r.yml) 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5363500.svg)](https://doi.org/10.5281/zenodo.5363500)

Programmatically interact with the GISAID EpiCoV, EpiPox, and EpiRSV databases.

> [!TIP]
> Please consider moving your research focus to an open pathogen on [pathoplexus.org](https://pathoplexus.org/).


## Citation

If you use GISAIDR in your research please cite as:

Wytamma Wirth, & Sebastian Duchene. (2022). GISAIDR: Programmatically interact with the GISAID databases. Zenodo. <https://doi.org/10.5281/zenodo.6474693>

## Installation

Install from github using `devtools`.

``` r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("Wytamma/GISAIDR")
```

## Login

Get `username` and `password` from [GISAID](https://www.epicov.org/).

``` r
username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
```

Login and save your credentials (they are used for all future database queries)

``` r
credentials <- login(username = username, password = password)
```

## Select a database

The EpiCoV database is selected by default, however, GISAIDR also works with the EpiRSV and EpiPox databases (limited testing).

``` r
credentials <- login(username = username, password = password, database="EpiRSV")
# or
credentials <- login(username = username, password = password, database="EpiPox")
```

Note: You need a GISAID account with access to EpiRSV and EpiPox.

## Get Data

Query the database with `query()` using your credentials

``` r
df <- query(credentials = credentials)
head(df[0:6])
```

| \#  | id              | virus_name                    | passage_details_history | accession_id    | collection_date | submission_date |
|-----|-----------------|-------------------------------|-------------------------|-----------------|-----------------|-----------------|
| 1   | EPI_ISL_1789201 | hCoV-19/USA/IL-S21WGS954/2021 | Original                | EPI_ISL_1789201 | 2021-04-16      | 2021-04-29      |
| 2   | EPI_ISL_1789200 | hCoV-19/USA/IL-S21WGS885/2021 | Original                | EPI_ISL_1789200 | 2021-04-02      | 2021-04-29      |
| 3   | EPI_ISL_1789199 | hCoV-19/USA/IL-S21WGS884/2021 | Original                | EPI_ISL_1789199 | 2021-04-12      | 2021-04-29      |
| 4   | EPI_ISL_1789198 | hCoV-19/USA/IL-S21WGS883/2021 | Original                | EPI_ISL_1789198 | 2021-04-14      | 2021-04-29      |
| 5   | EPI_ISL_1789197 | hCoV-19/USA/IL-S21WGS882/2021 | Original                | EPI_ISL_1789197 | 2021-04-15      | 2021-04-29      |
| 6   | EPI_ISL_1789196 | hCoV-19/USA/IL-S21WGS881/2021 | Original                | EPI_ISL_1789196 | 2021-04-13      | 2021-04-29      |

### Pagination

Use `nrows` and `start_index` to page through results. GISAID limits the number of results returned with each request to 50. Internally GISAIDR runs a loop to batch queries with > 50 rows requested. See fast option below.

``` r
df <- query(credentials = credentials, nrows = 1000, start_index = 100)
nrow(df)
```

[1] 1000

### Fast query

Use `fast` to load all of the accesion_ids that match the query. These accesion_ids can then be used in the `download` function to download up to 5000 sequences at a time. 

``` r
df <- query(
  credentials = credentials, 
  location = "Oceania", 
  from_subm = "2022-07-26", 
  to_subm = "2022-07-28",
  fast = TRUE
)
head(df$accession_id)
```
Selecting all 484 accession_ids.  
Returning 0-484 of 484 accession_ids.  
[1] "EPI_ISL_14061265" "EPI_ISL_14061266" "EPI_ISL_14061267" "EPI_ISL_14061268" "EPI_ISL_14061269" "EPI_ISL_14061270"

### Ordering

Use `order_by` to order the results or `query` by a column. Use `order_asc` to change the direction of `order_by` (defaults to TRUE).  

``` r
df <- query(credentials = credentials, order_by = 'submission_date')
df$submission_date
```
[1] "2020-01-10" "2020-01-10" "2020-01-11" "2020-01-11" "2020-01-11" "2020-01-12" "2020-01-14"  
[8] "2020-01-14" "2020-01-14" "2020-01-14" "2020-01-16" "2020-01-17" "2020-01-17" ...

### Full text search

Use `text` for full text search.

``` r
accession_ids = c("EPI_ISL_17398411", "EPI_ISL_17199001", "EPI_ISL_17409201", "EPI_ISL_17243716")
df <- query(credentials = credentials, text = paste(accession_ids, collapse = "\n"))
> df$accession_id
```
[1] "EPI_ISL_17199001" "EPI_ISL_17243716" "EPI_ISL_17398411" "EPI_ISL_17409201"

### Search by location

Use `location` to search for entries based on geographic location.

``` r
df <- query(credentials = credentials, location = 'Australia')
df$location
```

[1] "Oceania / Australia / Western Australia" "Oceania / Australia / Queensland"  
[3] "Oceania / Australia / Queensland" "Oceania / Australia / Queensland"  
[5] "Oceania / Australia / Western Australia" ...

A list of GISAID locations (not complete) can be found in [GISAID_LOCATIONS.txt](https://github.com/Wytamma/GISAIDR/blob/master/GISAID_LOCATIONS.txt). The location search is hierarchical e.g. querying for 'Africa / ...' will return all the regions within Africa, while querying for 'Africa / Angola / ...' will only return the regions in Angola. Region can be further subdivided by specifying more levels e.g. 'North America / USA / Alabama / Butler County'. The search uses pattern matching and does not have to follow the hierarchical format above.

### Search by lineage (EpiCoV)

Use `lineage` to search for entries based on pango lineage designations.

``` r
df <- query(credentials = credentials, lineage = 'B.1.1.7')
full_df <- download(credentials = credentials, list_of_accession_ids = df$accession_id)  # see below for download() info.
full_df$pangolin_lineage
```

[1] "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7"  
[11] "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7"  
[21] ...

### Search by Variant (EpiCoV)

Variants can be queried by name e.g. 'omicron', 'gh/490r', 'delta', 'alpha', 'beta', 'gamma', 'lambda', or 'mu'. Unfortunately GISAID doesn't return the variant designation from the query or download so variants must be confirmed with pangolin_lineage or GISAID_clade.

``` r
# VOC Omicron GRA (B.1.1.529+BA.*) first detected in Botswana/Hong Kong/South Africa
omicron_df <- query(credentials = credentials, variant = 'omicron')
omicron_full_df <- download(credentials = credentials, list_of_accession_ids = omicron_df$accession_id)
omicron_full_df$pangolin_lineage
```

[1] "BA.2" "BA.2" "BA.2.10.1" "BA.2" "BA.2" "BA.5" "BA.2" "BA.2" "BA.2.3"  
[10] "BA.4" "BA.2" "BA.2" "BA.2" "BA.2" "BA.2" "BA.2" "BA.2" "BA.1.17"  
[19] ...

### Search by collection date

Use `from` and `to` to search for entries from specific dates.

``` r
df <- query(credentials = credentials, from = '2021-04-05', to = '2021-04-06')
df$collection_date
```

[1] "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05"  
[8] "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05"  
[15] ...

### Search by submission date

Use `from_subm` and `to_subm` to search for entries from specific dates.

``` r
df <- query(credentials = credentials, from_subm = '2021-04-05', to_subm = '2021-04-05')
df$submission_date
```

[1] "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05"  
[8] "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05"  
[15] ...

### Search by virus name

Use `virus_name` to search for entries using the virus name.

``` r
df <- query(credentials = credentials, virus_name="hCoV-19/Ireland/D-BHTEST/2022")
df$virus_name
```

[1] "hCoV-19/Ireland/D-BHTEST/2022"

Search for multiple virus names using a list.

```
virus_names <- list("hCoV-19/Ireland/KY-Enfer-230922004_A4/2022", "hCoV-19/Ireland/CO-Enfer-240922010_E9/2022")
df <- query(credentials = credentials, virus_name=virus_names)
df$virus_name
```
[1] "hCoV-19/Ireland/CO-Enfer-240922010_E9/2022" "hCoV-19/Ireland/KY-Enfer-230922004_A4/2022"

You can also match parts of the virus name e.g.

```
df <- query(credentials = credentials, virus_name="hCoV-19/Ireland")
df$virus_name
```
[1] "hCoV-19/Ireland/KY-Enfer-260922007_C6/2022"  "hCoV-19/Ireland/KY-Enfer-260922007_C4/2022"   
[3] "hCoV-19/Ireland/KY-Enfer-260922007_C2/2022"  "hCoV-19/Ireland/KY-Enfer-260922007_C10/2022"  
[5] "hCoV-19/Ireland/KY-Enfer-260922007_C1/2022"  "hCoV-19/Ireland/CO-Enfer-260922007_B7/2022"...  

### Search by AA Substitutions and Nucleotide Mutations

Use `aa_substitution` and `nucl_mutation` to search for entries using amino acid Substitutions and nucleotide mutations.

``` r
aa_substitution_df <- query(credentials = credentials, aa_substitution = 'Spike_E484Q, Spike_H69del, -N_P13L')
nucl_mutation_df <- query(credentials = credentials, nucl_mutation = '-T23599G, -C10029T')
```

### Exclude low coverage entries

Use `low_coverage_excl` to exclude low coverage entries from the results.

``` r
df <- query(credentials = credentials, low_coverage_excl = TRUE)
grep("Long stretches of NNNs", df$information)
```

integer(0)

### Include only complete entries

GISAID considers genomes \>29,000 nt as complete. Use `complete` to include only complete entries in the results.

``` r
df <- query(credentials = credentials, complete = TRUE)
all(df$length > 29000)
```

[1] TRUE

### Include only high coverage entries

GISAID considers genomes with \<1% Ns and \<0.05% unique amino acid mutations as high coverage . Use `high_coverage` to include only high coverage entries in the results.

``` r
df <- query(credentials = credentials, high_coverage = TRUE)
length(grep("warn_sign", df$information)) == 0
```

[1] TRUE

### Include only entries with complete collection date

Use `collection_date_complete` to include only entries with complete collection date.

``` r
df <- query(credentials = credentials, collection_date_complete = TRUE)
```

### Load all entries

Use `load_all` to get all the entries that match your query without having to specify `nrows`.

``` r
df <- query(credentials = credentials, lineage = 'W.1', load_all = TRUE)
nrow(df)
```

[1] 100

Note: you may end up downloading the entire GISAID database if your query is too general.

### Get total query matches

Use `total` to get the number of entries that match you query.

``` r
total <- query(credentials = credentials, total = TRUE)
total
```

[1] 10145747

## Download

To download the full data set you need a list of accession IDs (which can be obtained from `query` results). This will also download the sequence data for each entry.

``` r
full_df_with_seq <- download(
    credentials = credentials, 
    list_of_accession_ids = list_of_accession_ids, 
)
full_df_with_seq$sequence
```

[1] "AGATCTGTTCTCTAAACGAACTTTAAAATCT...  
[2] "AGATCTGTTCTCTAAACGAACTTTAAAATCT...  
[3] "AGATCTGTTCTCTAAACGAACTTTAAAATCT...  
...

You can stop GISAIDR from loading the sequence data into the memory by setting get_sequence=FALSE. Note: the sequence data will still be downloaded.

``` r
df <- query(credentials = credentials)
list_of_accession_ids <- df$accession_id
full_df <- download(credentials = credentials, list_of_accession_ids = list_of_accession_ids, get_sequence=FALSE)
colnames(full_df)
```

[1] "strain" "virus" "gisaid_epi_isl" "genbank_accession"  
[5] "date" "region" "country" "division"  
[9] "location" "region_exposure" "country_exposure" "division_exposure"  
[13] "segment" "length" "host" "age"  
[17] "sex" "Nextstrain_clade" "pangolin_lineage" "GISAID_clade"  
[21] "originating_lab" "submitting_lab" "authors" "url"  
[25] "title" "paper_url" "date_submitted" "purpose_of_sequencing"

Note: a maximum of 5000 results can be downloaded at a time.

### Export to fasta file

Use the export_fasta function to write sequence data to a file in fasta format. The sequence names will be [country\@pango_lineage\@accesion_id\@date](mailto:country@pango_lineage@accesion_id@date), with the date in decimal format (requires the [lubridate](https://cran.r-project.org/web/packages/lubridate/index.html) package). The default is to only export sequences for which a decimal date could be set. To prevent this, use the argument export_dated_only = F.

``` r
export_fasta(full_df_with_seq, out_file_name = 'GISAID_sequences.fasta')
```

Date format (default: decimal year) and delimiter (default: \@) can be set with the date_format and delimiter arguments respectively.

``` r
export_fasta(full_df_with_seq, out_file_name = 'GISAID_sequences.fasta', date_format='%Y-%m-%d', delimiter='|')
```

Use the `columns` argument to choose which columns are included in the export.

``` r
export_fasta(full_df_with_seq, out_file_name = 'GISAID_sequences.fasta', columns = c("accession_id", "country", "pangolin_lineage", "date"))
```

## Errors

GISAIDR relies on the custom selection interface of [gisaid.org](https://www.gisaid.org/). If GISAIDR is giving you errors, first check that it is not gisaid.org producing these errors. We can't do anything to fix errors with gisaid.org.

If you have an epiflu account (i.e. you were using GISAID before COVID-19) you may have issues logging in as GISAID may default you to the epiflu database.

## Updating

When updating GISAIDR run `detach("package:GISAIDR", unload=TRUE)` first to ensure the update is applied.

## Examples

Download all of the 2020 entries from Asia but outside China.

``` r
library(GISAIDR)

# country list from GISAID
Asia <- c('Asia / Afghanistan',
 'Asia / Armenia',
 'Asia / Bahrain',
 'Asia / Bangladesh',
 'Asia / Brunei',
 'Asia / Cambodia',
 # 'Asia / China', # remove China
 'Asia / Georgia',
 'Asia / Hong Kong',
 'Asia / India',
 'Asia / Indonesia / ...',
 'Asia / Iran',
 'Asia / Iraq',
 'Asia / Israel',
 'Asia / Japan',
 'Asia / Jordan / ...',
 'Asia / Kazakhstan / ...',
 'Asia / Kuwait',
 'Asia / Lebanon',
 'Asia / Malaysia',
 'Asia / Mongolia',
 'Asia / Myanmar',
 'Asia / Nepal',
 'Asia / Oman / ...',
 'Asia / Pakistan',
 'Asia / Palestine / ...',
 'Asia / Philippines',
 'Asia / Qatar / ...',
 'Asia / Saudi Arabia',
 'Asia / Singapore',
 'Asia / South Korea',
 'Asia / Sri Lanka',
 'Asia / Taiwan',
 'Asia / Thailand',
 'Asia / Timor-Leste',
 'Asia / United Arab Emirates',
 'Asia / Uzbekistan',
 'Asia / Vietnam')

credentials <- login(username = username, password = password)

asia_not_china_df <- data.frame()
for (country in Asia) {
  print(country)
  df <- query(credentials = credentials, location = country, load_all = TRUE, from = "2020-01-01", to = "2020-12-31")
  asia_not_china_df <- rbind(asia_not_china_df, df)
}
head(asia_not_china_df)
```

## Dev guide

1.  Go to the search interface on <https://www.epicov.org/epi3/frontend> (EpiCoV \> Seach).
2.  Right click on the feature you want to add (e.g. the `complete` checkbox) and inspect the source code.
3.  Find the `ceid` for this element (`<div id="ce_qxos9a_bi">`) e.g. `ce_qxos9a_bi`
4.  Find the the value of the checkbox element (`<input class="sys-event-hook" name="ce_qxos9a_bi_name" style="vertical-align: middle;" type="checkbox" value="complete">`) e.g. `complete`.
5.  Search for the id in the page source. In one of the `<script>` header tags you'll find a createFI() function e.g. `this.getForm().createFI('ce_qxos9a_bi','CheckboxWidget','quality'`. The `ceid` is dynamic so you need to use the widget name i.e. `quality` to find the `ceid` dynamically.
6.  In the GISAIDR `login.R` file add code to extract the `ceid` using the `extract_search_ceid()` function and the widget e.g.

``` r
# Complete 
quality_ceid <- extract_search_ceid("quality'", customSearch_page_text)
```

1.  Add the extracted `ceid` to the list of `credentials` e.g. `complete_ceid = complete_ceid`
2.  Add the new argument and default value to **all** `query()` function in `query.R` and `internal_query.R` e.g. `complete = FALSE`.
3.  Add the new argument and default value to the `(load_all && j$totalRecords > nrows)` load_all recursion loop so all paginations will continue using the argument.
4.  Create and append a search queue to the main queue if the `complete` argument is used. Create the command using the `create_search_queue()` function. Use the `complete_ceid` for the `ceid` and the checkbox value (identified in step 4) for the `cvalue` e.g.

``` r
if (complete) {
  queue <-
    append(
      queue,
      create_search_queue(credentials,
                          ceid=credentials$complete_ceid,
                          cvalue=list('complete'),
                          cmd='FilterChange')
    )
}
```

1.  Add the new argument to the recursive load all `query()` call e.g. `complete = complete`
2.  Add test for new feature in `test-query.R` e.g.

``` r
test_that("complete works", {
  df <- query(credentials = credentials, complete = TRUE)
  expect_true(all(df$length > 29000))
})
```

1.  Run **all** tests. If tests pass create a PR to merge the new feature.

See [this PR](https://github.com/Wytamma/GISAIDR/pull/9/commits/72e0a4456563d03ded0626b7d47e0f4f90d2ef44) for the full details.
