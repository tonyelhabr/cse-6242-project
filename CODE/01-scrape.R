
# setup ----
library(dplyr)
library(tools)
library(purrr)
library(tibble)
library(magrittr)
library(snakecase)

download_file <- function(url, file, ..., dir = 'data-raw', quiet = TRUE) {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  path <- file.path(dir, file)
  status <- download.file(url = url, destfile = path, ..., quiet = quiet)
  tools::file_path_sans_ext(file)
}

# main ----
.url_template <-
  'http://files.zillowstatic.com/research/public/Zip/%s'
file_info <-
  tibble::tibble(file = sprintf(
    'Zip_Zhvi_%s.csv',
    c(
      sprintf('%dbedroom', 1:4),
      '5BedroomOrMore',
      'SingleFamilyResidence',
      'Condominum',
      'AllHomes'
    )
  )) %>%
  mutate(url = sprintf(.url_template, file))
file_info

file_dl_info <-
  file_info %>%
  mutate(
    series = purrr::map2_chr(url, file, ~ download_file(.x, .y)) %>% snakecase::to_snake_case()
  )
file_dl_info
