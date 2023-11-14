# Movie Database API Query

Contact a restful API to query and parse movie info from the open movie database. Perform data transformation and summarization.

Key features:

- API query
- JSON parsing
- Exploratory Data Analysis (EDA)
- Numerical Summaries and Contingency Tables
- Correlation Analysis
- Histogram, Bar Plot, Box Plot, Scatter Plot

R packages used:

- `here`: enables easy file referencing and builds file paths in a OS-independent way
- `keyring`: provides OS-independent 'API' to access system's credential store
- `stats`: loads this before loading `tidyverse` to avoid masking some `tidyverse` functions
- `tidyverse`: includes collections of useful packages like `dplyr` (data manipulation), `tidyr` (tidying data),  `ggplots` (creating graphs), etc.
- `rlang`: deals with missing arguments in our helper functions
- `lubridate`: handle date and datetime data type
- `glue`: offers interpreted string literals for easy creation of dynamic messages and labels
- `scales`: formats and labels scales nicely for better visualization
- `httr`: performs HTTP web APIs
- `jsonliet`: parses data stored in JSON format
- `GGally`: plots a very nice scatterplot matrix 

## Project Report

[Project report](https://rpubs.com/clh2021/1113658) ([Github Markdown](./project1_v9.md)) ([R Markdown](./project1_v9.Rmd))

Chien-Lan Hsueh (chienlan.hsueh at gmail.com)

## Overview and Project Goal

In this project, we contact an API provided on the open movie database ([OMDb](https://www.omdbapi.com/)) using functions created to query, parse, and return well-structured data. With the obtained data from the API, exploratory data analysis is done.

## Part 1 - Setup and Helper Functions of API Queries

We use `keyring` to store and manage login credentials for API queries. Several helper functions are created to make API queries and data parsing processes easy and reliable. 

A query is done with the following helper functions we created in this work:

1. `OMDb_setup_query()` to set up a query string including the API key and query criteria.
1. `OMDB_query()` to send a query request, convert and parse the response data, and check the execution status. 
1. `OMDb_parse_movie()` to parse JSON info of a movie and save in a data frame after data type conversion for numeric data

## Part 2 - API Queries

API queries are done to ensure efficiency and bug-free. 

- Single-movie queries by IMDb ID
- Single-movie queries by Title
- Search with a Partially Matched Value
- Tests with Bad Parameters
  - Bad (Invalid) IMDBb ID
  - Bad (Makeup) Title
  - Bad Search Value

## Part 3 - EDA

We focus on batman and Superman movies, series and games in this work. After the queries, we filter and transform the data sets. Histogram grouped by title types are used to show the growth by the released year. Stacked plots and side-by-side bar plots, box plots and scatter matrix plots are used for visual comparisons. At the end, we show there is a great dependency between box office and the popularity scores.
