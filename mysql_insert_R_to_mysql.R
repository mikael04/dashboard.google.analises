

#install.packages("RMySQL")
library(RMySQL)
library(dplyr)

#MYSQL_ROOT_PASSWORD=password_riccovid20211826
mydb = dbConnect(MySQL(), user='root', password='password_riccovid20211826', 
                 dbname='db_riccovid', host='34.123.255.114',)
summary(mydb)

dbListTables(mydb)

#ler tabela a inserir
df_dim_au_co_jo <- fst::read_fst("data-raw/app/df_dimensions_tabelas_clean.fst")

#teste com sample pequeno
teste = sample_n(df_dim_au_co_jo,size = 10000)

dbWriteTable(mydb, name = 'temp_table2', value = teste, row.names = F, append = F)
dbWriteTable(mydb, name = 'temp_table', value = df_dim_au_co_jo, row.names = F, append = F)  #completo


#final do script

#ainda não deu certo migrar para uma tabela 100% tipada como deveria
#dbGetQuery(mydb, "insert into df_dim_au_co_jo select * from temp_table2")

library(dbplyr)


#usar via dplyr
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      host = "34.123.255.114",
                      user = "root",
                      #dbname = "db_riccovid",
                      password = rstudioapi::askForPassword("Database password")
)

dbGetInfo(con)
dbListTables(con)

see <- tbl(con, "temp_table2")
