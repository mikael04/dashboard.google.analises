library(ggplot2)
library(plotly)

df_paises_count_ordered <- data.table::fread("dados/df_paises_count_ordered.csv") %>%
  dplyr::rename(Count = count)
df_paises_count_top20 <- df_paises_count_ordered %>%
  dplyr::slice_head(n = 20)

ggplot2::ggplot(df_paises_count_top20, aes(x = Count, y = (reorder(Paises, Count)))) +
  geom_bar(fill = "steelblue", stat = "identity") + 
  theme_minimal() + 
  labs(x = "Número de artigos", y = "Países", title = "Publicações por país")
