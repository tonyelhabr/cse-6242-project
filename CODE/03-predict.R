
# setup ----
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(purrr)
library(magrittr)
library(prophet)

# import_xlsx <- function(file, ..., dir = 'data-raw') {
#   path <- file.path(dir, sprintf('%s.xlsx', file))
#   readxl::read_excel(path, ...)
# }

export_csv <- function(x, file = deparse(substitute(x)), ..., dir = .dir_export, na = '') {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  path <- file.path(dir, sprintf('%s.csv', file))
  readr::write_csv(x, path, na = na, ...)
  invisible(path)
}

export_rds <- function(x, file = deparse(substitute(x)), ..., dir = .dir_export) {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  path <- file.path(dir, sprintf('%s.rds', file))
  readr::write_rds(x, path, ...)
  invisible(path)
}

prophet_safely <- purrr::safely(prophet::prophet, otherwise = list())

predict_verbosely <- function(..., .idx = 1, .n = 1) {
  cat(sprintf('Making predictions for model %s (of %s)', scales::comma(.idx), scales::comma(.n)), sep = '\n')
  predict(...)
}

predict_verbosely_safely <- purrr::safely(predict_verbosely, otherwise = tibble())

# main ----
data_long <- 'data_long' %>% import_csv()

grid_ym_predict <-
  tidyr::crossing(
    year = seq(2019, 2029, by = 1),
    month = 1:12
  ) %>%
  mutate_at(vars(year, month), as.integer) %>%
  mutate(date = sprintf('%04d-%02d-01', year, month) %>% lubridate::ymd()) %>%
  filter(date > lubridate::ymd('2019-09-01'))
grid_ym_predict

fits_nested <-
  data_long %>%
  select(zip, series, ds = date, y = value) %>%
  group_by(zip, series) %>%
  nest() %>%
  mutate(
    fit = purrr::map(
      data,
      ~prophet_safely(
        .x,
        weekly.seasonality = FALSE,
        daily.seasonality = FALSE
      )$result)
  ) %>%
  ungroup()
fits_nested
export_rds(fits_nested)

# fits_prophet %>%
#   mutate(n_fit = purrr::map_int(fit, ~length(.x))) %>%
#   filter(n_fit < 7)

.n_fits <- fits_prophet_nested %>% nrow()
preds_nested <-
  fits_nested %>%
  mutate(idx = row_number()) %>%
  mutate(
    preds = purrr::map2(
      fit, idx,
      ~predict_verbosely_safely(
        .x,
        grid_ym_predict %>% select(ds = date),
        .idx = .y,
        .n = .n_fits
      )$result
    )
  ) %>%
  select(-idx)
preds_nested
export_rds(preds_nested)

preds <-
  preds_nested %>%
  select(zip, series, preds) %>%
  tidyr::unnest(preds)
preds
export_rds(preds)

data_and_preds <-
  bind_rows(
    data_long %>%
      mutate(is_prediction = FALSE),
    preds %>%
      rename(date = ds) %>%
      mutate_at(vars(date), lubridate::date) %>%
      mutate_at(
        vars(date),
        list(
          year = ~lubridate::year(.),
          month = ~lubridate::month(.)
        )
      ) %>%
      mutate_at(vars(year, month), as.integer) %>%
      select(zip, year, month, date, series, value = yhat) %>%
      mutate(is_prediction = TRUE)
  ) %>%
  select(zip, year, month, date, series, value, is_prediction) %>%
  arrange(zip, year, month, date, series, value)
data_and_preds
export_csv(data_and_preds)

.date_cutoff <- '2019-10-01' %>% lubridate::ymd()
data_and_preds_calcs <-
  data_and_preds %>%
  group_by(zip, series) %>%
  mutate(
    value_lag1 = lag(value)
  ) %>%
  mutate(
    value_last = ifelse(date == .date_cutoff, value_lag1, NA_real_)
  ) %>%
  tidyr::fill(value_last, .direction = 'down') %>%
  # ungroup() %>%
  mutate(
    pred_change_lag1 = value - value_lag1,
    pred_change_last = value - value_last
  ) %>%
  mutate(
    pred_change_frac_lag1 =  pred_change_lag1 / value_lag1,
    pred_change_frac_last = pred_change_last / value_last
  ) %>%
  mutate_at(
    vars(matches('pred_change_frac')),
    ~ifelse(is.infinite(.), NA_real_, .)
  ) %>%
  group_by(series, date) %>%
  mutate(n_zip = n()) %>%
  mutate_at(
    vars(matches('pred_change_frac')),
    list(rnk = ~row_number(desc(.)))
  ) %>%
  ungroup() %>%
  # mutate_at(vars(matches('lag1')), ~ifelse(date <= .date_cutoff, NA_integer_, .)) %>%
  select(-value_lag1, -value_last)
data_and_preds_calcs
data_and_preds_calcs %>% ggplot() + geom_histogram(aes(pred_change_frac_lag1_rnk))
export_csv(data_and_preds_calcs)

series_info <-
  tibble(
    series = c(
      'zip_zhvi_1_bedroom',
      'zip_zhvi_2_bedroom',
      'zip_zhvi_3_bedroom',
      'zip_zhvi_4_bedroom',
      'zip_zhvi_5_bedroom_or_more',
      'zip_zhvi_condominum',
      'zip_zhvi_single_family_residence',
      'zip_zhvi_all_homes'
    ),
    series_lab = c(
      '1 bedroom',
      '2 bedrooms',
      '3 bedrooms',
      '4 bedrooms',
      '5+ bedrooms',
      'Condominium',
      'Single Family',
      'All'
    )
  )

data_tableau_3 <-
  data_and_preds_calcs %>%
  inner_join(series_info) %>%
  mutate_at(vars(zip), as.character) %>%
  mutate_at(
    vars(matches('rnk$')),
    list(lab = ~sprintf('%d (of %d)', ., n_zip))
  ) %>%
  mutate_at(vars(matches('rnk_lab$')), ~ifelse(date < .date_cutoff, NA_character_, .)) %>%
  select(
    `Zip` = zip,
    `Series` = series,
    `Series Label` = series_lab,
    # `Year` = year,
    # `Month` = month,
    `Date` = date,
    # `Date Label` = date_lab,
    `Value` = value,
    `Is Prediction?` = is_prediction,
    # `Predicted Change` = pred_change,
    `Predicted 1-Month Change %` = pred_change_frac_lag1,
    `Change % (Since Sep. 2019)` = pred_change_frac_last,
    `Rank of Predicted 1-Month Change %` = pred_change_frac_lag1_rnk,
    `Rank of Predicted Change % (Since Sep. 2019)` = pred_change_frac_last_rnk,
    `Count of Zips For Series and Month` = n_zip
    # `Rank of Predicted 1-Month Change % Label` = pred_change_frac_lag1_rnk_lab,
    # `Rank of Predicted Change % (Since Sep. 2019) Label` = pred_change_frac_last_rnk_lab,
  )
data_tableau_3
data_tableau_3 %>% filter(Date == lubridate::ymd('2019-10-01'))
export_csv(data_tableau_3)

data_tableau_2_tidy <-
  data_and_preds_calcs %>%
  inner_join(series_info) %>%
  mutate_at(vars(zip), as.character) %>%
  select(zip, series, series_lab, date, is_prediction, value, pred_change_frac_lag1, pred_change_frac_last) %>%
  gather(key = 'key', value = 'value', -zip, -series, -series_lab, -date, -is_prediction) %>%
  select(
    `Zip` = zip,
    `Series` = series,
    `Series Label` = series_lab,
    `Date` = date,
    `Is Prediction?` = is_prediction,
    `Key` = key,
    `Value` = value
  )
data_tableau_2_tidy
export_csv(data_tableau_2_tidy)
