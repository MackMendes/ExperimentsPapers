# ====
# Leitura de CSV
datasetBugId <- read.csv(file="E:/Mestrado/ExperimentsPapers/BugAID/DataSet/dataset_bugid_dinamyc.csv", header=TRUE, sep=",")

# ====
# Retirando os metadados do DataSet
db <- datasetBugId[10:632]


# ============================================================
# Rodando o DBScan
source("model/DbScanClustering.R")
dt_dbScan <- dbScanClustering(db)

# ====
# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
result_dbScan <- showResult(dt_dbScan)

write.csv(x = result_dbScan, file="results/resultado-dbscan.csv")






# ============================================================
# Rodando o K-Means
source("model/KMeansClustering.R")
dt_kMeans <- kMeansClustering(db)

# ====
# Montando estrutura para comparação dos valores
source("util/ShowResult.R")
result_kMeans <- showResult(dt_kMeans)

write.csv(x = result_kMeans, file="results/resultado-kMeans.csv")




# ============================================================
# Rodando o Optics
res.dbopt <- dbscan::optics(db,  eps = 0.30, minPts = 5)



# ====
# Rodando o HDBScan
res.dbdbs <- dbscan::hdbscan(x = db,  minPts = 5)








# TODO: ainda não funciona bem o Tuning abaixo
# ==== 
# Tuning 
dbscan::kNNdistplot(db, k = 219)
abline(h = 4.30, lty = 2)