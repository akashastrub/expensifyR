#' Manipulate master file data in preparation for waterfall plot
#'
#' @param df Dataframe containing master file data
#' @param master_currency_analytics User inputted master currency for waterfall plot
#'
#' @return Dataframe to be passed to plot_waterfall() function
#' @import dplyr
#' @export
#'
transform_master_for_waterfall <- function(df, master_currency_analytics) {

  # Find number of distinct months in dataframe
  n_distinct_months <- df |>
    dplyr::mutate(month = lubridate::month(date),
                  year = lubridate::year(date)) |>
    dplyr::summarise(n_distinct_months = dplyr::n_distinct(month, year)) |>
    dplyr::pull(n_distinct_months)

  # Find the order of variables for the master file
  df_variable_order <- df |>
    dplyr::mutate(month = lubridate::month(date)) |>
    dplyr::rename(amount_currency = master_currency_analytics) |>
    dplyr::mutate(category = stringr::str_replace(category, ' ', '_')) |>
    dplyr::group_by(category) |>
    dplyr::summarise(amount = round(sum(amount_currency)/n_distinct_months, 1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(amount))

  # Infer variable order for waterfall plot
  df_variable_order_in <- df_variable_order |> dplyr::filter(amount > 0)
  df_variable_order_out <- df_variable_order |> dplyr::filter(amount <= 0) |>
    dplyr::arrange(amount)
  l_variable_order <- df_variable_order_in$category |>
    append(df_variable_order_out$category)

  # Finalise dataframe for plot
  df_waterfall <- df_variable_order |> dplyr::mutate(
    category = factor(category, l_variable_order)) |>
    dplyr::mutate(measure = "relative") |>
    dplyr::mutate(
      text = dplyr::case_when(
        amount > 0 ~ stringr::str_c('+', as.character(amount), sep = ''),
        TRUE ~ as.character(amount))) |>
    rbind(
      data.frame(category = "Monthly profit after tax",
                 amount = 0,
                 measure = "total",
                  text = "Total")) |>
    dplyr::arrange(factor(
        category,
        levels = (l_variable_order |> append("Monthly profit after tax"))))

  # Return dataframe
  return(df_waterfall)
}

#' Create waterfall plot
#'
#' @param df Dataframe containing processed master file data
#'
#' @return Plotly graph
#' @import dplyr plotly
#' @export
#'
plot_waterfall <- function(df) {

  # Create waterfall plot
  fig_waterfall <- plotly::plot_ly(
    df, name = "20", type = "waterfall", measure = ~ measure,
    x = ~category, textposition = "outside", y= ~amount, text =~text,
    connector = list(line = list(color= "rgb(63, 63, 63)")))

  # Add title, axes, legend
  fig_waterfall <- fig_waterfall |>
    plotly::layout(title = "Profit and loss statement",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""),
                   autosize = TRUE,
                   showlegend = TRUE)

  # Return dataframe
  return(fig_waterfall)
}
