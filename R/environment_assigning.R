
date_sequence <- function(dataframe, start, end, sequence_name = "date_sequence") {
  dataframe |>
    dplyr::rowwise() |>
    dplyr::mutate(
      !!rlang::sym(sequence_name) := list(seq(.data[[start]], .data[[end]], by = "day"))
    ) |>
    dplyr::ungroup() |>
    print()
}

find_factored_measurements = function(data, environmental_data, date_sequence_column = "date_sequence", environment_date_column, environment_variable_column, variable_column = "variable_values", data_fct_col = NA, env_fct_col = NA, fct_id = NA) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      !!rlang::sym(variable_column) := list(purrr::map(.data[[date_sequence_column]], function(dates) {
        if(!is.na(data_fct_col)) {
          factor_id = .data[[data_fct_col]]
        } else {
          factor_id = "NA"
        }

        environmental_data = environmental_data |>
          dplyr::filter(if (!is.na(env_fct_col)) {.data[[env_fct_col]] == factor_id}else {TRUE})

        row_indices <- match(dates, environmental_data[[environment_date_column]])

        as.numeric(environmental_data[[environment_variable_column]][row_indices]) # Convert to numeric

      }))
    ) |>
    dplyr::ungroup() |>
    print()
}

generate_values = function(dataframe, column_name = "variable_values", prefix = NA, unit = NA) {

  avg_variable = "avg_variable"
  max_variable = "max_variable"
  min_variable = "min_variable"

  if (!is.na(prefix)) {
    avg_variable = paste0(prefix,"_avg")
    max_variable = paste0(prefix,"_max")
    min_variable = paste0(prefix,"_min")
  }

  if(!is.na(unit)) {
    avg_variable = paste0(avg_variable,"_",unit)
    max_variable = paste0(max_variable,"_",unit)
    min_variable = paste0(min_variable,"_",unit)
  }

  dataframe |>
    dplyr::mutate(
      !!avg_variable := purrr::map_dbl(.data[[column_name]], ~ mean(as.numeric(.x))),
      !!max_variable := purrr::map_dbl(.data[[column_name]], ~ max(as.numeric(.x))),
      !!min_variable := purrr::map_dbl(.data[[column_name]], ~ min(as.numeric(.x)))
    ) |>
    print()
}


