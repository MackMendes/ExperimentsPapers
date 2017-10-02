showResult <- function (dt){
  library(dplyr)
  
  # Montando estrutura para comparação dos valores
  metadados <- c("ID", "ProjectID", "CommitURL", "BuggyCommitID", "BugFixingCommit", 
                 "RepairedCommitID", "Class", "Method", "ModifiedStatementCount", 
                 "Cluster", "Qtd")
  
  colunas <- colnames(dt)
  
  #colunas[!colunas %in% metadados]
  
  resultado_pilot <- tidyr::gather(dt, "nome_coluna", "valor_coluna", colunas[!colunas %in% metadados])
  
  # Obtendo apenas as linhas com item da Bag-of-Works
  resultado_pilot_job <- resultado_pilot[resultado_pilot$valor_coluna == 1,]
  
  # Obtendo apenas os itens  que estavam em algum Cluster
  resultado_pilot_job_final <- resultado_pilot_job[resultado_pilot_job$Cluster != 0, ]
  
  
  resultado_pilot_job_final <- resultado_pilot_job_final %>% group_by(Cluster, nome_coluna) %>% summarise(V1 = sum(Qtd))
  resultado_pilot_job_final <- resultado_pilot_job_final[order(-resultado_pilot_job_final$V1),]
  
  return(resultado_pilot_job_final)
}