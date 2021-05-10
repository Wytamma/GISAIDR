# GISAIDR
[![Build](https://github.com/Wytamma/GISAIDR/actions/workflows/r.yml/badge.svg)](https://github.com/Wytamma/GISAIDR/actions/workflows/r.yml)

Programmatically interact with the GISAID database.

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
df <- query(credentials=credentials)
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
df <- query(credentials=credentials, nrows=1000, start_index=100)
nrow(df)
[1] 1000
```

## Download

To download the full data set you need a accession IDs (which can be obtained from `query` results).

```R
df <- query(credentials = credentials)
list_of_accession_ids <- df$accession_id
full_df <- download(credentials, list_of_accession_ids)
colnames(full_df)
```
[1] "strain"                "virus"              "gisaid_epi_isl"        "genbank_accession"    
[5] "date"                  "region"                "country"               "division"             
[9] "location"              "region_exposure"       "country_exposure"      "division_exposure"    
[13] "segment"               "length"                "host"                  "age"                  
[17] "sex"                   "Nextstrain_clade"      "pangolin_lineage"      "GISAID_clade"         
[21] "originating_lab"       "submitting_lab"        "authors"               "url"                  
[25] "title"                 "paper_url"             "date_submitted"        "purpose_of_sequencing"


## Installation

Install from github using `devtools`.

```R
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("Wytamma/GISAIDR")
```
