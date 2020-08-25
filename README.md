
Class project instructions: https://poloclub.github.io/cse6242-2019fall-online/#project

## DESCRIPTION

This repo stores the code and dcoumentation of Team 65's project. The final deliverable is [this pubic Tableau dashboard](For the final visualization, go to [this public Tableau dashboard](https://public.tableau.com/profile/tony.elhabr#!/vizhome/AustinTXRealEstateAnalysis/Dashboard).

This repo has two main directories:

+ DOC (i.e. for documentation): Contains the report and poster.

+ CODE: Contains all code.

## INSTALLATION

All code (for scraping, cleaning, modeling, and evaluating) is performed with R. Thus, to run the code, you should have R >=3.4 installed. (All code was run with R 3.5.2.) The latest version of R can be downloaded from [the Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/). It's also recommended to install and use [RStudio](https://rstudio.com/products/rstudio/download/) for executing the code.

Once you have R (and RStudio) installed, use the following code to install all necessary packages.

```
install.packages(
  c(
    'dplyr',
    'readr',
    'tidyr',
    'purrr',
    'tibble',
    'stringr',
    'ggplot2',
    'ggridges',
    'rlang',
    'scales',
    'forcats',
    'magrittr',
    'tools',
    'snakecase',
    'prophet'
  )
)
devtools::install_github('clauswilke/ggtext')
```

## EXECUTION

1. Run "01-scape.R" (in the "CODE" directory).

This downloads files from Zillow's historical data webpage. Links are in the format "http://files.zillowstatic.com/research/public/Zip/{file}.csv", where file is something like "Zip_Zhvi_1bedroom".

2. Run "02-clean.R".

3. Run "03-predict.R".

4. Run "04-evaluate.R".

5. Run "05-visualize.R".

For the final visualization, go to [this public Tableau dashboard](https://public.tableau.com/profile/tony.elhabr#!/vizhome/AustinTXRealEstateAnalysis/Dashboard)
