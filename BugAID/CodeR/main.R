# ====
# Leitura de CSV
datasetBugId <- read.csv(file="E:/Mestrado/ExperimentsPapers/BugAID/DataSet/dataset_bugid_original.csv", header=TRUE, sep=",")

# ====
# Retirando os metadados do DataSet
# Meta
db <- datasetBugId[11:ncol(datasetBugId)]


# ============================================================
# Rodando o DBScan
source("model/DbScanClustering.R")
dt_dbScan <- dbScanClustering(db, datasetBugId)

# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
result_dbScan <- showResult(dt_dbScan)

write.csv(x = result_dbScan, file="results/resultado-dbscan-original.csv")


# ============================================================
# Rodando o K-Means
source("model/KMeansClustering.R")
dt_kMeans <- kMeansClustering(db, datasetBugId)

# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
result_kMeans <- showResult(dt_kMeans)

write.csv(x = result_kMeans, file="results/resultado-kMeans-original.csv")


# ============================================================
# Rodando o Optics
source("model/OpticsClustering.R")
dt_optics <- opticsClustering(db, datasetBugId)

# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
result_optics <- showResult(dt_optics)

#colnames(dt_optics)

write.csv(x = result_optics, file="results/resultado-optics-original.csv")


# ============================================================
# Rodando o HDBScan
source("model/HDbScanClustering.R")
dt_hdbscan <- hdbscanClustering(db, datasetBugId)

# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
result_hdbscan <- showResult(dt_hdbscan)

write.csv(x = result_hdbscan, file="results/resultado-HDBScan-original.csv")








# TODO: ainda não funciona bem o Tuning abaixo
# ==== 
# Tuning 
dbscan::kNNdistplot(db, k = 219)
abline(h = 4.30, lty = 2)