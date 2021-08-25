library(dbplyr)
library(bigrquery)
library(DBI)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "cidacs-ai-covid19-br",
  dataset = "backendDash",
  billing = "cidacs-ai-covid19-br"
)

bigrquery::bq_auth() # aqui vai ter uma primeira configuração, depois ele usa um token que ele salva

#df = tbl(con,"backend_dash_publi") banco raw
df_filtros = dplyr::tbl(con,"hml_base_filtro") #conexão com essa tabela, da para selecionar ou filtra direto com dplyr
d = df_filtros %>% collect() # download para dataframe
paises = df_filtros %>% select(paises) %>% collect()
glimpse(df_filtros)
glimpse(teste)
glimpse(paises)
