
# setup ----
library(dplyr)
library(readr)
library(purrr)
library(magrittr)
library(prophet)

.dir_export <- 'data'
import_rds <- function(file, ..., dir = .dir_export) {
  path <- file.path(dir, sprintf('%s.rds', file))
  readr::read_rds(path, ...)
}

import_csv <- function(file, ..., dir = .dir_export) {
  path <- file.path(dir, sprintf('%s.csv', file))
  readr::read_csv(path, ...)
}

export_rds <- function(x, file = deparse(substitute(x)), ..., dir = .dir_export) {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  path <- file.path(dir, sprintf('%s.rds', file))
  readr::write_rds(x, path, ...)
  invisible(path)
}

cv_verbosely <- function(..., .idx = 1, .n = 1) {
  cat(sprintf('Cross-validating model %s (of %s)', scales::comma(.idx), scales::comma(.n)), sep = '\n')
  prophet::cross_validation(...)
}

cv_verbosely_safely <- purrr::safely(cv_verbosely, otherwise = tibble())
perf_safely <-  purrr::safely(prophet::performance_metrics, otherwise = tibble())

# main ----
data_long <- 'data_long' %>% import_csv()
fits_nested <- 'fits_nested' %>% import_rds()
.n_fits <- fits_nested %>% nrow()

fits_cv_nested <-
  fits_nested %>%
  select(-data) %>%
  mutate(idx = row_number()) %>%
  mutate(
    fit_cv = purrr::map2(
      fit, idx,
      ~cv_verbosely_safely(
        .x,
        horizon = 365,
        units = 'days',
        .idx = .y,
        .n = .n_fits
      )$result
    )
  ) %>%
  select(-idx)
fits_cv_nested

fits_cv_nested_good <-
  fits_cv_nested %>%
  mutate(n_row = purrr::map_int(fit_cv, ~nrow(.x))) %>%
  filter(n_row > 0) %>%
  select(-n_row)
fits_cv_nested_good
export_rds(fits_cv_nested_good)

fits_cv_nested_10year <-
  fits_nested %>%
  mutate(n_row = purrr::map_int(data, ~nrow(.x))) %>%
  arrange(-n_row) %>%
  select(-data) %>%
  mutate(idx = row_number()) %>%
  mutate(
    fit_cv = purrr::map2(
      fit, idx,
      ~cv_verbosely_safely(
        .x,
        horizon = 365*10,
        units = 'days',
        init = 365.25,
        .idx = .y,
        .n = .n_fits
      )$result
    )
  ) %>%
  select(-idx)
fits_cv_nested_10year

fits_cv_nested_10year_good <-
  fits_cv_nested_10year %>%
  mutate(n_row = purrr::map_int(fit_cv, ~nrow(.x))) %>%
  filter(n_row > 0) %>%
  select(-n_row)
fits_cv_nested_10year_good
export_rds(fits_cv_nested_10year_good)

perfs_cv_nested <-
  fits_cv_nested_good %>%
  select(-fit) %>%
  mutate(
    perf = purrr::map(
      fit_cv,
      ~perf_safely(.x, rolling_window = 1/12)$result %>% as_tibble()
    )
  )
perfs_cv_nested

perfs_cv_nested_good <-
  perfs_cv_nested %>%
  mutate(n_row = purrr::map_int(perf, ~nrow(.x))) %>%
  filter(n_row > 0) %>%
  select(-n_row)
perfs_cv_nested_good
export_rds(perfs_cv_nested_good)

perfs_cv_nested_10year <-
  fits_cv_nested_10year_good %>%
  select(-fit) %>%
  mutate(
    perf = purrr::map(
      fit_cv,
      ~perf_safely(.x, rolling_window = 0.1)$result %>% as_tibble()
    )
  )
perfs_cv_nested_10year

perfs_cv_nested_10year_good <-
  perfs_cv_nested_10year %>%
  mutate(n_row = purrr::map_int(perf, ~nrow(.x))) %>%
  filter(n_row > 0) %>%
  select(-n_row)
perfs_cv_nested_10year_good
export_rds(perfs_cv_nested_10year_good)
