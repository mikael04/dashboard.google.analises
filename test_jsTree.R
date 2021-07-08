perguntas_full <- data.table::fread("dados/perguntas_full.csv")
perguntas_full$pathString <- paste("COVID19",
                                    perguntas_full$EIXO, perguntas_full$TOPICS,
                                    perguntas_full$QUERIES, perguntas_full$QUESTIONS,
                                    sep = "/")
arvore <- data.tree::as.Node(perguntas_full)

# ?FromDataFrameTable

library(treemap)
data(GNI2014)
head(GNI2014)

arvore
list_perguntas <- apply(perguntas_full,1,as.list)

COVID19 <- apply(perguntas_full,1,as.list)
covid <- as.list(perguntas_full)
