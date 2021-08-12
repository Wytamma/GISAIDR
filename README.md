# GISAIDR
[![Build](https://github.com/Wytamma/GISAIDR/actions/workflows/r.yml/badge.svg)](https://github.com/Wytamma/GISAIDR/actions/workflows/r.yml)

Programmatically interact with the GISAID database.

## Installation

Install from github using `devtools`.

```R
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("Wytamma/GISAIDR")
```

## Login 

Get `username` and `password` from [GISAID](https://www.epicov.org/).
```R
username = Sys.getenv("GISAIDR_USERNAME")
password = Sys.getenv("GISAIDR_PASSWORD")
```

Login and save your credentials (they are used for all future database queries)
```R
credentials <- login(username = username, password = password)
```

## Get Data

Query the database with `query()` using your credentials

```R
df <- query(credentials = credentials)
head(df[0:6])
```
|#     |id             |virus_name                            |passage_details_history|accession_id   |collection_date|submission_date|
|------|---------------|--------------------------------------|-----------------------|---------------|---------------|---------------|
|1     |EPI_ISL_1789201|hCoV-19/USA/IL-S21WGS954/2021         |Original               |EPI_ISL_1789201|2021-04-16     |2021-04-29     |
|2     |EPI_ISL_1789200|hCoV-19/USA/IL-S21WGS885/2021         |Original               |EPI_ISL_1789200|2021-04-02     |2021-04-29     |
|3     |EPI_ISL_1789199|hCoV-19/USA/IL-S21WGS884/2021         |Original               |EPI_ISL_1789199|2021-04-12     |2021-04-29     |
|4     |EPI_ISL_1789198|hCoV-19/USA/IL-S21WGS883/2021         |Original               |EPI_ISL_1789198|2021-04-14     |2021-04-29     |
|5     |EPI_ISL_1789197|hCoV-19/USA/IL-S21WGS882/2021         |Original               |EPI_ISL_1789197|2021-04-15     |2021-04-29     |
|6     |EPI_ISL_1789196|hCoV-19/USA/IL-S21WGS881/2021         |Original               |EPI_ISL_1789196|2021-04-13     |2021-04-29     |

### Pagination 

Use `nrows` and `start_index` to page through results.

```R
df <- query(credentials = credentials, nrows = 1000, start_index = 100)
nrow(df)
```
[1] 1000

### Search by location

Use `location` to search for entries based on geographic location. 

```R
df <- query(credentials = credentials, location = 'Australia')
df$location
```
[1] "Oceania / Australia / Western Australia" "Oceania / Australia / Queensland"       
[3] "Oceania / Australia / Queensland"        "Oceania / Australia / Queensland"       
[5] "Oceania / Australia / Western Australia"  ...     

A list of GISAID locations (not complete) can be found in [GISAID_LOCATIONS.txt](https://github.com/Wytamma/GISAIDR/blob/master/GISAID_LOCATIONS.txt). The location search is hierarchical e.g. querying for 'Africa / ...' will return all the regions within Africa, while querying for 'Africa / Angola / ...' will only return the regions in Angola. Region can be further subdivided by specifying more levels e.g. 'North America / USA / Alabama / Butler County'. The search uses pattern matching and does not have to follow the hierarchical format above.  

### Search by lineage

Use `lineage` to search for entries based on geographic location. 

```R
df <- query(credentials = credentials, lineage = 'B.1.1.7')
full_df <- download(credentials = credentials, list_of_accession_ids = df$accession_id)  # see below for download() info.
full_df$pangolin_lineage
```
[1] "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7"  
[11] "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7" "B.1.1.7"  
[21] ...   

### Search by Date

Use `from` and `to` to search for entries from specific dates.

```R
df <- query(credentials = credentials, from = '2021-04-05', to = '2021-04-05')
df$collection_date
```
[1] "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05"  
[8] "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05" "2021-04-05"  
[15] ...  


### Exclude low coverage entries 

Use `low_coverage_excl` to exclude low coverage entries from the results.

```R
df <- query(credentials = credentials, low_coverage_excl = TRUE)
grep("Long stretches of NNNs", df$information)
```
integer(0)


### Load all entries

Use `load_all` to get all the entries that match your query without having to specify `nrows`. 
```R
df <- query(credentials = credentials, lineage = 'W.1', load_all = TRUE)
nrow(df)
```
[1] 100

Note: you may end up downloading the entire GISAID database if your query is too general.  

## Download

To download the full data set you need a list of accession IDs (which can be obtained from `query` results).

```R
df <- query(credentials = credentials)
list_of_accession_ids <- df$accession_id
full_df <- download(credentials = credentials, list_of_accession_ids = list_of_accession_ids)
colnames(full_df)
```
[1] "strain"                "virus"              "gisaid_epi_isl"        "genbank_accession"    
[5] "date"                  "region"                "country"               "division"             
[9] "location"              "region_exposure"       "country_exposure"      "division_exposure"    
[13] "segment"               "length"                "host"                  "age"                  
[17] "sex"                   "Nextstrain_clade"      "pangolin_lineage"      "GISAID_clade"         
[21] "originating_lab"       "submitting_lab"        "authors"               "url"                  
[25] "title"                 "paper_url"             "date_submitted"        "purpose_of_sequencing"

Note: a maximum of 5000 results can be downloaded at a time. 

### Get sequence data 

Use the `get_sequence` argument to download the sequences with the full data.

```R
full_df_with_seq <- download(
    credentials = credentials, 
    list_of_accession_ids = list_of_accession_ids, 
    get_sequence=TRUE
)
full_df_with_seq$sequence
```
[1] "AGATCTGTTCTCTAAACGAACTTTAAAATCT...  
[2] "AGATCTGTTCTCTAAACGAACTTTAAAATCT...  
[3] "AGATCTGTTCTCTAAACGAACTTTAAAATCT...  
...

### Export to fasta file

Use the export_fasta function to write sequence data to a file in fasta format. The sequence names will be country@pango_lineage@accesion_id@date, with the date in decimal format (requires the [lubridate](https://cran.r-project.org/web/packages/lubridate/index.html) package). 

```R
export_fasta(full_df_with_seq, out_file_name = 'GISAID_sequences.fasta')
```
The default is to only export sequences for which a decimal date could be set. To prevent this, use the argument export_dated_only = F. 

## Errors

GISAIDR relies on the custom selection interface of [gisaid.org](https://www.gisaid.org/). If GISAIDR is giving you errors, first check that it is not gisaid.org producing these errors. We can't do anything to fix errors with gisaid.org.

## Examples 

Download all of the 2020 entries from Asia but outside China. 

```R
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

1. Go to the custom selection interface on https://www.epicov.org/epi3/frontend (Downloads > Genomic epidemiology > Custom Selection).
2. Right click on the feature you want to add (e.g. the `complete` checkbox)
3. Find the `ceid` for this element (`<div id="ce_qxos9a_bi">`) e.g. `ce_qxos9a_bi` 
4. Find the the value of the checkbox element (`<input class="sys-event-hook" name="ce_qxos9a_bi_name" style="vertical-align: middle;" type="checkbox" value="complete">`) e.g. `complete`.
5. Search for the id in the page source. In one of the `<script>` header tags you'll find a createFI() function e.g. `this.getForm().createFI('ce_qxos9a_bi','CheckboxWidget','quality'`. The `ce` ids are dynamic so you need to use the widget name i.e. `quality` to find the `ce` id dynamically. 
6. In the GISAIDR `login.R` file add code to extract the `ceid` using the `extract_search_ceid()` function and the widget e.g.

```R
# Complete 
complete_ceid <- extract_search_ceid('quality', t)
```
7. Add the extracted `ceid` to the list of `credentials` e.g. `complete_ceid = complete_ceid`
8. Add the new argument and default value to the `query()` function in `query.R` e.g. `complete = FALSE`.
9. Create and append a search queue to the main queue if the `complete` argument is used. Create the command using the `create_search_queue()` function. Use the `complete_ceid` for the `ceid` and the checkbox value (identified in step 4) for the `cvalue` e.g. 
```R
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
10. Add the new argument to the recursive load all `query()` call e.g. `complete = complete`
11. Add test for new feature in `test-query.R`  e.g.
```R
test_that("complete works", {
  df <- query(credentials = credentials, complete = TRUE)
  expect_true(all(df$length > 29000))
})
```
12. Run **all** tests. If tests pass create a PR to merge the new feature. 

See [this PR](https://github.com/Wytamma/GISAIDR/pull/9) for the full details. 