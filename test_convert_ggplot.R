library(ggplot2)
library(plotly)
library(purrr)


df_type_date <- data.table::fread("dados/df_dimensions_type_date.csv")

p <- ggplot2::ggplot(data = df_type_date, aes(x = date_normal, y = count, color = type,
                                              text=(paste('<b>Data:</b>', date_normal, '<br>', '<b>Número:</b>', count, '<b>Tipo:</b>', type)))) + 
  geom_line() + 
  theme_minimal() + 
  labs(x = "Tempo", y = "Publicações de artigo", title = "Publicações por tipo de publicação")
p
ggplotly(p, tooltip = "text") %>%
  plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)


df_categ_count <- data.table::fread("dados/df_categ_count_ordered.csv")
p <- ggplot2::ggplot(df_categ_count, aes(x = Count, y = reorder(Category, Count), text=map(paste('<b>Número de publicações:</b>', Count, '<br>', '<b>Categoria:</b>', Category), HTML)))

ggplotly(p, tooltip = "text") %>%
  plotly::config(modeBarButtonsToRemove = c("zoom2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines"), displaylogo = FALSE)
