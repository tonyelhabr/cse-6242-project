
# setup ----
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(tools)
library(purrr)
library(tibble)
library(magrittr)
library(snakecase)

.zips <- c(78702, 78703, 78704, 78705, 78717, 78721, 78722, 78723, 78724, 78725, 78726, 78727, 78728, 78730, 78731, 78732, 78733, 78734, 78736, 78737, 78738, 78739, 78741, 78744, 78745, 78746, 78747, 78748, 78749, 78750, 78751, 78752, 78753, 78754, 78756, 78757, 78758, 78759, 78626, 78633, 78660, 78681) %>% as.integer()
# .zips_tableau <- c(78613, 78660, 78664, 78681, 78701, 78702, 78703, 78704, 78705, 78721, 78723, 78724, 78726, 78727, 78728, 78729, 78730, 78731, 78732, 78734, 78735, 78737, 78738, 78739, 78741, 78744, 78745, 78746, 78747, 78748, 78749, 78750, 78751, 78753, 78754, 78756, 78757, 78758, 78759) %>% as.integer()
# setdiff(.zips, .zips_tableau)
# setdiff(.zips_tableau, .zips)

.dir_export <- 'data'
export_csv <- function(x, file = deparse(substitute(x)), ..., dir = .dir_data, na = '') {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  path <- file.path(dir, sprintf('%s.csv', file))
  readr::write_csv(x, path, na = na, ...)
  invisible(path)
}

# main ----
paths <- list.files('data-raw', pattern = 'csv$', recursive = FALSE, full.names = TRUE)
paths

paths_info <-
  paths %>%
  tibble::tibble(path = .) %>%
  mutate(
    series =
      path %>%
      basename() %>%
      tools::file_path_sans_ext() %>%
      snakecase::to_snake_case()
  ) %>%
  mutate(idx = row_number()) %>%
  select(idx, series, path)
paths_info

data_info <-
  paths_info %>%
  mutate(
    size = purrr::map_int(path, ~file.info(.x) %>% pull(size) %>% as.integer()),
    data = purrr::map(path, readr::read_csv)
  ) %>%
  mutate(
    data = purrr::map(data, ~select(.x, matches('[A-Z]')))
  ) %>%
  mutate(
    n_col = purrr::map_int(data, ncol),
    n_row = purrr::map_int(data, nrow)
  ) %>%
  mutate(n_obs = n_col * n_row) %>%
  select(-data)
data_info
export_csv(data_info)

data_info_summ <-
  data_info %>%
  summarise_at(vars(size, n_obs), sum)
data_info_summ %>%
  mutate_at(vars(size), ~scales::number(., scale = 1e-6, suffix = 'Mb')) %>%
  mutate_at(vars(n_obs), ~scales::comma(.))
export_csv(data_info_summ)

data_nest <-
  paths_info %>%
  select(-series) %>%
  # slice(1) %>%
  mutate(
    data = purrr::map(
      path,
      ~read_csv(.x) %>%
        rename_at(vars(matches('[A-Z]')), snakecase::to_snake_case) %>%
        rename(zip = region_name) %>%
        mutate_at(vars(zip), as.integer) %>%
        inner_join(tibble(zip = .zips)) %>%
        select(zip, matches('^[1-2]')) %>%
        tidyr::gather(key = 'year_month', value = 'value', matches('^[1-2]')) %>%
        tidyr::separate(year_month, into = c('year', 'month'), sep = '-') %>%
        mutate_at(vars(year, month), as.integer)
    )
  ) %>%
  select(-path) %>%
  inner_join(paths_info) %>%
  select(series, data)
data_nest

data_long <-
  data_nest %>%
  tidyr::unnest(data) %>%
  arrange(series, zip, year, month) %>%
  mutate(date = sprintf('%04d-%02d-01', year, month) %>% lubridate::ymd()) %>%
  select(series, zip, year, month, date, value)
data_long
export_csv(data_long)
