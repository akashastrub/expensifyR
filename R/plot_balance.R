library(plotly)

# Import data
df_balances <- readr::read_csv(here::here('data', 'personal_balance', 'balances_template.csv'))

# Manipulate data for plot
df_plot <- df_balances |>
  group_by(date) |>
  summarise(total = sum(amount_usd))

# Plot figure
fig <- plot_ly(df_plot, type = 'scatter', mode = 'lines+markers') |>
  add_trace(x = ~date, y = ~total, name = 'net worth') #|> layout(showlegend = F)
options(warn = -1)

fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6')#, width = 900)


fig
