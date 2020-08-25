
# setup ----
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggridges)
library(rlang)
library(scales)
library(lubridate)
library(stringr)
library(purrr)
library(forcats)
library(stringr)
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

export_csv <- function(x, file = deparse(substitute(x)), ..., dir = .dir_export, na = '') {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  path <- file.path(dir, sprintf('%s.csv', file))
  readr::write_csv(x, path, na = na, ...)
  invisible(path)
}

export_png <- function(x, file = deparse(substitute(x)), ..., suffix = NULL, dir = .dir_export) {
  if(!dir.exists(dir)) {
    invisible(dir.create(dir))
  }
  if(!is.null(suffix)) {
    file <- sprintf('%s_%s', file, suffix)
  }
  path <- file.path(dir, sprintf('%s.png', file))
  ggsave(plot = x, file = path, ...)
  invisible(path)
}

.export_png_poster <- purrr::partial(
  export_png,
  suffix = 'poster',
  dir = file.path(.dir_export, 'figs'),
  ... =
)

.export_png_report <- purrr::partial(
  export_png,
  suffix = 'report',
  dir = file.path(.dir_export, 'figs'),
  ... =
)

export_png_poster <- purrr::partial(
  .export_png_poster,
  width = 12,
  height = 8,
  ... =
)

export_png_poster_wide <- purrr::partial(
  .export_png_poster,
  width = 12,
  height = 6,
  ... =
)

export_png_poster_tall <- purrr::partial(
  .export_png_poster,
  width = 8,
  height = 12,
  ... =
)

export_png_report <- purrr::partial(
  .export_png_report,
  width = 6.5,
  height = 8,
  ... =
)

theme_custom <- function(...) {
  hrbrthemes::theme_ipsum(
    base_size = 14,
    axis_title_size = 14,
    caption_size = 14,
    base_family = '',
    plot_margin = margin(5, 5, 5, 5),
    ...
  ) +
    theme(
      plot.title = ggtext::element_markdown(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = 'bottom'
    )
}

visualize_bar <- function(data, ...) {
  data %>%
    ggplot() +
    aes(x = year, group = is_prediction, fill = is_prediction) +
    geom_bar() +
    scale_fill_manual(values = c('grey0', 'grey70'), name = 'Is Prediction?') +
    theme_custom() +
    theme(
      panel.grid.major.x = element_blank()
    ) +
    labs(
      y = 'Count',
      x = 'Year'
    )
}


.horizon_grps <-
  tibble(
    horizon_month = 1L:12L,
    horizon_day_min = c(0, 30.4, 60.8, 91.3, 121.7, 152.1, 182.5, 212.9, 243.3, 273.8, 304.2, 334.6),
    horizon_day_max = c(30.4, 60.8, 91.3, 121.7, 152.1, 182.5, 212.9, 243.3, 273.8, 304.2, 334.6, 365)
  )
.horizon_grps_10year <-
  tibble(
    horizon_year = 1L:10L,
    horizon_day_min = c(0, 365, 730, 1095, 1460, 1825, 2190, 2555, 2920, 3285),
    horizon_day_max = c(365, 730, 1095, 1460, 1825, 2190, 2555, 2920, 3285, 3650)
  )

convert_horizon_day_to_months <- function(data, ...) {
  data %>%
    mutate_at(vars(horizon), ~as.numeric(., units = 'days')) %>%
    mutate_at(vars(horizon), list(horizon_grp = ~ggplot2::cut_number(., n = 12, labels = 1:12))) %>%
    rename(horizon_day = horizon) %>%
    mutate(horizon_month = horizon_grp %>% as.integer()) %>%
    select(-horizon_grp)
}

convert_horizon_day_to_years <- function(data, ...) {
  suppressWarnings(
    data %>%
      mutate_at(vars(horizon), ~as.numeric(., units = 'days')) %>%
      rename(horizon_day = horizon) %>%
      fuzzyjoin::fuzzy_inner_join(
        .horizon_grps_10year,
        by = c('horizon_day' = 'horizon_day_min', 'horizon_day' = 'horizon_day_max'),
        match_fun = c(`>`, `<=`)
      ) %>%
      select(-horizon_day_min, -horizon_day_max)
  )
}

# Reference: `prophet::plot_cross_validation_metric()`.
compute_perf_mape_noroll <- function(fit_cv, ...) {
  fit_cv %>%
    prophet::performance_metrics(metrics = 'mape', rolling_window = -1) %>%
    as_tibble()
}

do_compute_perf_mape_noroll <- function(fit_cv, ...) {
  fit_cv %>%
    compute_perf_mape_noroll() %>%
    convert_horizon_day_to_months() %>%
    mutate(horizon_year = horizon_month / 12)
}

do_compute_perf_mape_noroll_10year <- function(fit_cv, ...) {
  fit_cv %>%
    compute_perf_mape_noroll() %>%
    convert_horizon_day_to_years()
}

do_compute_perf_mape_noroll_safely <- purrr::safely(do_compute_perf_mape_noroll, otherwise = tibble())
do_compute_perf_mape_noroll_10year_safely <- purrr::safely(do_compute_perf_mape_noroll_10year, otherwise = tibble())

min2 <- function(...) {
  suppressWarnings(min(..., na.rm = T))
}

max2 <- function(...) {
  suppressWarnings(max(..., na.rm = T))
}

mean2 <- function(...) {
  suppressWarnings(mean(..., na.rm = T))
}

select_perf_cols <- function(data, ...) {
  data %>%
    select(zip, series, horizon_day, ..., mse, rmse, mae, mape, coverage)
}

visualize_ridges_perf_mape <-
  function(data,
           x = 'mape_mean',
           method = c('series', 'zip'),
           cutoff = NULL,
           option = 'E',
           ...) {
    method <- match.arg(method)
    x_sym <- rlang::sym(x)
    title_suffix_1 <- ifelse(method == 'series', 'Series', 'Zip Codes')
    title_suffix_2 <- ifelse(method == 'series', 'Zip Codes', 'Series')

    lab_caption <- 'y-axis is on two separate linear scales.'
    if(!is.null(cutoff)) {
      # Don't cutoff data. Cutoff x-axis instead.
      # data <-
      #   data %>%
      #   mutate_at(vars(!!x_sym), ~ifelse(. > cutoff, cutoff, .))
      lab_caption <- glue::glue(
        '{lab_caption} MAPE capped at {scales::percent(cutoff)}.'
      )
    } else {
      cutoff <- NA_real_
    }

    viz <-
      data %>%
      # mutate_at(vars(!!y_sym), ~factor(.) %>% forcats::fct_rev()) %>%
      ggplot() +
      # Reference: https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
      aes(y = horizon_lab, x = !!x_sym, fill = factor(..quantile..)) +
      ggridges::geom_density_ridges_gradient(quantile_lines = TRUE) +
      ggridges::stat_density_ridges(
        geom = 'density_ridges_gradient',
        calc_ecdf = TRUE,
        quantiles = 4,
        quantile_lines = TRUE
      ) +
      scale_fill_viridis_d(option = option, name = 'Quartiles') +
      scale_x_continuous(labels = scales::percent, limits = c(0, cutoff)) +
      theme_custom(...) +
      labs(
        title = glue::glue(
          'CV MAPE Averaged Across All {title_suffix_2} (All {title_suffix_1})'
        ),
        caption = lab_caption,
        x = 'MAPE',
        y = 'Horizon'
      )
    viz
  }

.summarise_by_x <- function(data, ...) {
  left_join(
    data %>% count(...),
    data %>%
      group_by(...) %>%
      summarise_at(
        vars(mape),
        list(min = min2, max = max2, mean = mean2)
      ) %>%
      ungroup()
  )
}

summarise_by_x <- function(data1, data2, ...) {
  bind_rows(
    .summarise_by_x(data1, ...) %>% mutate(method = 'roll'),
    .summarise_by_x(data2, ...) %>% mutate(method = 'noroll')
  ) %>%
    select(method, everything())
}

summarise_spread_by_x <- function(data1, data2, ...) {
  summ <- summarise_by_x(data1, data2, ...)
  summ %>%
    tidyr::gather(key = 'metric', value = 'value', n:mean) %>%
    tidyr::unite(col = 'metric_method', metric, method) %>%
    tidyr::spread(metric_method, value)
}

prettify_summary <- function(data, ...) {
  data %>%
    select(..., count_roll = n_roll, min_roll, max_roll, mean_roll) %>%
    rename_all(
      ~stringr::str_remove(., '_roll$') %>%
        stringr::str_replace_all('_', ' ') %>%
        stringr::str_to_title()
    ) %>%
    mutate_at(vars(`Count`), scales::comma) %>%
    mutate_at(vars(`Min`, `Max`, `Mean`), scales::percent)
}

.zip <- 78745
.series <- 'zip_zhvi_all_homes'
.filter_1zip <- function(data, ...) {
  data %>% filter(zip == .zip)
}
.filter_1series <- function(data, ...) {
  data %>% filter(series == .series)
}
.filter_1 <- function(data, ...) {
  data %>% .filter_1zip(...) %>% .filter_1series(...)
}

.lab_1zip <- glue::glue('<b style="color:#cb4b16">***{.zip}***</b>')
.lab_1series <- glue::glue('<span style="color:#268bd2">***ZHVI of All Homes***</span>')

# data and preds  ----
data_and_preds_calcs <- 'data_and_preds_calcs' %>% import_csv()
data_and_preds_calcs
# data_and_preds_calcs %>% count(year, sort = T)
# data_and_preds_calcs %>% count(series, sort = T)

viz_bar_all <-
  data_and_preds_calcs %>%
  visualize_bar() +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = 'k')) +
  labs(
    title = 'Count of Observation and Predictions (All Series, All Zip Codes)'
  )
viz_bar_all
export_png_poster_wide(viz_bar_all)

viz_bar_1zip <-
  data_and_preds_calcs %>%
  .filter_1zip() %>%
  visualize_bar() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = glue::glue('Count of Observations and Predictions for {.lab_1zip} (All Series)')
  )
viz_bar_1zip
export_png_poster_wide(viz_bar_1zip)

viz_bar_1series <-
  data_and_preds_calcs %>%
  .filter_1series() %>%
  visualize_bar() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = glue::glue('Count of Observations and Predictions  for {.lab_1series} Series (All Zip Codes)')
  )
viz_bar_1series
export_png_poster_wide(viz_bar_1series)

# preds ----
# preds <- data_and_preds_calcs %>% filter(is_prediction)
# preds
preds_nested <- 'preds_nested' %>% import_rds()
preds_nested_1 <- preds_nested %>% .filter_1()
preds_nested_1

# Reference: `prophet:::df_for_plotting()`
preds_1 <-
  full_join(
    preds_nested_1$fit[[1]]$history %>% select(ds, y),
    preds_nested_1$preds[[1]],
    by = 'ds'
  ) %>%
  arrange(ds)
preds_1

# Reference: `prophet:::plot.prophet()`
viz_preds_1 <-
  preds_1 %>%
  ggplot() +
  aes(x = ds, y = y) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2, na.rm = TRUE) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = 'k')) +
  geom_line(aes(y = yhat), size = 1.5, linetype = 'dotted') +
  geom_hline(aes(yintercept = 0)) +
  theme_custom() +
  # expand_limits(y = c(0, NA)) +
  labs(
    title = glue::glue('Historical and Predicted {.lab_1series} for {.lab_1zip}'),
    y = 'ZHVI',
    x = 'Year'
  ) +
  theme(
    plot.title = ggtext::element_markdown()
  )
viz_preds_1
export_png_poster(viz_preds_1)
export_png_report(viz_preds_1)

# cv ----
fits_cv_nested_good <- 'fits_cv_nested_good' %>% import_rds()
fits_cv_nested_good
fits_cv_nested_10year_good <- 'fits_cv_nested_10year_good' %>% import_rds()
fits_cv_nested_10year_good

# fits_cv ----
horizon_info <-
  tibble(
    horizon_year = c((1:12)/12, 1:10),
    horizon_lab = c('1 Month', sprintf('%d Months', 2:12), '1 Year', sprintf('%s Years', 2:10))
  ) %>%
  mutate_at(vars(horizon_lab), ~forcats::fct_reorder(., -horizon_year))
horizon_info

fits_cv_noroll_all <-
  bind_rows(
    fits_cv_nested_good %>%
      # slice(1) %>%
      mutate(res = purrr::map(fit_cv, ~do_compute_perf_mape_noroll_safely(.x)$result)) %>%
      select(-fit, -fit_cv),
    fits_cv_nested_10year_good %>%
      # slice(1) %>%
      mutate(res = purrr::map(fit_cv, ~do_compute_perf_mape_noroll_10year_safely(.x)$result)) %>%
      select(-fit, -fit_cv)
  ) %>%
  tidyr::unnest(res) %>%
  left_join(horizon_info) %>%
  # filter(horizon_lab != '12 Months') %>%
  filter(horizon_lab != '1 Year') %>%
  arrange(zip, series, horizon_year, horizon_year) %>%
  select(zip, series, horizon_day, horizon_month, horizon_year, horizon_lab, mape)
fits_cv_noroll_all
export_rds(fits_cv_noroll_all)

# perfs ----
# Actually, using the `fits_cv_noroll_all` stuff instead of this.
# Nonetheless, both are shown in the `summ_` variables at the end.
perfs_cv_nested_good <- 'perfs_cv_nested_good' %>% import_rds()
perfs_cv_nested_10year_good <- 'perfs_cv_nested_10year_good' %>% import_rds()

# This is pretty similar to what was done to create `fits_cv_noroll_all` above.
perfs_cv_all <-
  bind_rows(
    perfs_cv_nested_good %>%
      select(-fit_cv) %>%
      tidyr::unnest(perf) %>%
      convert_horizon_day_to_months() %>%
      select_perf_cols(horizon_month) %>%
      mutate(horizon_year = horizon_month / 12),
    perfs_cv_nested_10year_good %>%
      select(-fit_cv) %>%
      unnest(perf) %>%
      convert_horizon_day_to_years() %>%
      select_perf_cols(horizon_year)
  ) %>%
  left_join(horizon_info) %>%
  # filter(horizon_lab != '12 Months') %>%
  filter(horizon_lab != '1 Year') %>%
  arrange(zip, series, horizon_year, horizon_year) %>%
  select(zip, series, horizon_day, horizon_month, horizon_year, horizon_lab, mape)
perfs_cv_all
export_rds(perfs_cv_all)

viz_ridge_perfs_cv_1zip <-
  # perfs_cv_all %>%
  fits_cv_noroll_all %>%
  .filter_1series() %>%
  visualize_ridges_perf_mape('mape', 'series', 0.5, option = 'E') +
  labs(
    title =
      glue::glue(
        'CV MAPE For {.lab_1zip} (All Series)'
      )
  )
viz_ridge_perfs_cv_1zip
export_png_report(viz_ridge_perfs_cv_1zip)
export_png_poster_tall(viz_ridge_perfs_cv_1zip)

viz_ridge_perfs_cv_1series <-
  # perfs_cv_all %>%
  fits_cv_noroll_all %>%
  .filter_1zip() %>%
  visualize_ridges_perf_mape('mape', 'series', 0.5, option = 'B') +
  theme(plot.title = ggtext::element_markdown(size = 16)) +
  labs(
    title =
      glue::glue(
        'CV MAPE For {.lab_1series}
        (All Zip Codes)'
      )
  )
viz_ridge_perfs_cv_1series
export_png_report(viz_ridge_perfs_cv_1series)

viz_ridge_perfs_cv_1series <-
  # perfs_cv_all %>%
  fits_cv_noroll_all %>%
  .filter_1zip() %>%
  visualize_ridges_perf_mape('mape', 'series', 0.5, option = 'B') +
  labs(
    title =
      glue::glue(
        'CV MAPE For {.lab_1series} (All Zip Codes)'
      )
  )
viz_r
export_png_poster_tall(viz_ridge_perfs_cv_1series)

viz_ridge_perfs_cv_1 <-
  # perfs_cv_all %>%
  fits_cv_noroll_all %>%
  .filter_1() %>%
  visualize_ridges_perf_mape('mape', 'series', 0.5, option = 'C') +
  labs(
    title =
      glue::glue(
        'CV MAPE For {.lab_1series} For {.lab_1zip}'
      )
  )
viz_ridge_perfs_cv_1
export_png_report(viz_ridge_perfs_cv_1)
export_png_poster_tall(viz_ridge_perfs_cv_1)

viz_ridge_perfs_cv_all <-
  # perfs_cv_all %>%
  fits_cv_noroll_all %>%
  visualize_ridges_perf_mape('mape', 'series', 0.5, option = 'D') +
  labs(
    title = glue::glue(
      'CV MAPE (All Zip Codes, All Series)'
    )
  )
viz_ridge_perfs_cv_all
export_png_report(viz_ridge_perfs_cv_all)
export_png_poster_tall(viz_ridge_perfs_cv_all)

# summ ----
summ_wide_cv_by_zip_series <-
  summarise_spread_by_x(
    perfs_cv_all,
    fits_cv_noroll_all,
    series,
    zip
  ) %>%
  arrange(series, zip)
summ_wide_cv_by_zip_series
export_csv(summ_wide_cv_by_zip_series)

summ_wide_cv_by_zip_horizon <-
  summarise_spread_by_x(
    perfs_cv_all,
    fits_cv_noroll_all,
    horizon_year,
    zip
  ) %>%
  arrange(horizon_year, zip)
summ_wide_cv_by_zip_horizon
export_csv(summ_wide_cv_by_zip_horizon)

summ_wide_cv_by_series_horizon <-
  summarise_spread_by_x(
    perfs_cv_all,
    fits_cv_noroll_all,
    series,
    horizon_year
  ) %>%
  arrange(series, horizon_year)
summ_wide_cv_by_series_horizon
export_csv(summ_wide_cv_by_series_horizon)

summ_wide_cv_by_horizon <-
  summarise_spread_by_x(
    perfs_cv_all,
    fits_cv_noroll_all,
    horizon_year
  ) %>%
  arrange(horizon_year)
summ_wide_cv_by_horizon
export_csv(summ_wide_cv_by_horizon)

summ_wide_cv_by_series <-
  summarise_spread_by_x(
    perfs_cv_all,
    fits_cv_noroll_all,
    series
  ) %>%
  arrange(series)
summ_wide_cv_by_series
export_csv(summ_wide_cv_by_series)

summ_wide_cv_by_zip <-
  summarise_spread_by_x(
    perfs_cv_all,
    fits_cv_noroll_all,
    zip
  )
summ_wide_cv_by_zip
export_csv(summ_wide_cv_by_zip)

summ_wide_cv_by_horizon_pretty <-
  summ_wide_cv_by_horizon %>%
  inner_join(horizon_info) %>%
  filter(horizon_lab != '1 Year') %>%
  rename(horizon = horizon_lab) %>%
  prettify_summary(horizon)
summ_wide_cv_by_horizon_pretty
export_csv(summ_wide_cv_by_horizon_pretty)

summ_wide_cv_by_zip_pretty <-
  summ_wide_cv_by_zip %>%
  mutate_at(vars(zip), factor) %>% # To avoid `scales::percent()`
  prettify_summary(zip)
summ_wide_cv_by_zip_pretty
# export_csv(summ_wide_cv_by_zip_pretty)

summ_wide_cv_by_zip_trunc <-
  bind_rows(
    summ_wide_cv_by_zip %>% arrange(mean_roll) %>% head(),
    summ_wide_cv_by_zip %>% arrange(mean_roll) %>% tail()
  )
summ_wide_cv_by_zip_trunc
# export_csv(summ_wide_cv_by_zip_trunc)

summ_wide_cv_by_zip_trunc_pretty <-
  summ_wide_cv_by_zip_trunc %>%
  mutate_at(vars(zip), as.character) %>% # To avoid `scales::percent()`
  prettify_summary(zip)
summ_wide_cv_by_zip_trunc_pretty
export_csv(summ_wide_cv_by_zip_trunc_pretty)

summ_wide_cv_by_series_pretty <-
  summ_wide_cv_by_series %>%
  mutate_at(
    vars(series),
    ~stringr::str_remove(., 'zip_zhvi_') %>%
      stringr::str_replace_all('_', ' ') %>%
      stringr::str_replace('Condominum', 'Condominium') %>%
      stringr::str_to_title()
  ) %>%
  prettify_summary(series)
summ_wide_cv_by_series_pretty
export_csv(summ_wide_cv_by_series_pretty)

