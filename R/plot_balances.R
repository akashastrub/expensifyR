#' Create balances plot
#'
#' @param df_balances Dataframe containing raw balances data
#' @param currency User inputted currency for balances plot
#'
#' @return Plotly graph
#' @import dplyr plotly
#' @export
#'
plot_balances <- function(df) {

  # Aggregate data for balances plot
  df_plot <- df |>
    group_by(date) |>
    summarise(total = sum(amount_usd))

  # Create balances plot
  fig_balances <- plot_ly(df_plot, type = 'scatter', mode = 'lines+markers') |>
    add_trace(x = ~date, y = ~total, name = 'net worth') #|> layout(showlegend = F)
  options(warn = -1)

  # Add minor stylistic preferences
  fig_balances <- fig_balances %>%
    layout(
      xaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      yaxis = list(zerolinecolor = '#ffff',
                   zerolinewidth = 2,
                   gridcolor = 'ffff'),
      plot_bgcolor='#e5ecf6')#, width = 900)

  # Return figure
  return(fig_balances)
}

