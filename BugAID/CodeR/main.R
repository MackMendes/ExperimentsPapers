# Read CSV into R
datasetBugId <- read.csv(file="E:/Mestrado/Experimentos/DataSet/dataset_bugid_original_with_header.csv", header=TRUE, sep=",")


data("multishapes", package = "factoextra")
db <- datasetBugId[10:632]


# ====
# Rodando o DBScan
library("dbscan")
res.db <- dbscan::dbscan(db,  eps = 0.30, minPts = 5)

# Obter os clusters
datasetBugId$Cluster <- res.db$cluster
datasetBugId$Qtd = 1;

#Realizar um agrupamento 
library(data.table)
dt <- data.table(datasetBugId)
result <- dt[, sum(Qtd), by = Cluster]
result <- dt %>% group_by(Cluster, nome_coluna) %>% summarise(V1 = sum(Qtd))

#Ordenar pela quantidade
result <- result[order(-V1)]


# ====
# Rodando o KMeans
res.dbkm <- kmeans(x = db, centers=219)


# ====
# Rodando o Optics
res.dbopt <- dbscan::optics(db,  eps = 0.30, minPts = 5)



# ====
# Rodando o HDBScan
res.dbdbs <- dbscan::hdbscan(x = db,  minPts = 5)







# ====
# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
resultado_pilot_job_final <- showResult(dt)


# TODO: ainda não funciona bem o Tuning abaixo
# ==== 
# Tuning 
dbscan::kNNdistplot(db, k = 219)
abline(h = 4.30, lty = 2)