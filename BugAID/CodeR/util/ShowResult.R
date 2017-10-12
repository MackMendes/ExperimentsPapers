showResult <- function (dt){
  if(!require(dplyr)) install.packages("dplyr")
    
  library(dplyr)
  library(tidyr)
  
  # Montando estrutura para comparação dos valores
  metadados <- c("ID", "ProjectID", "CommitURL", "BuggyCommitID", "BugFixingCommit", 
                 "RepairedCommitID", "Class", "Method", "ModifiedStatementCount", 
                 "Cluster", "Qtd")
  
  colunas <- colnames(dt)
  
  resultado_pilot <- tidyr::gather(dt, "nome_coluna", "valor_coluna", colunas[!colunas %in% metadados])
  
  # Obtendo apenas as linhas com item da Bag-of-Works
  resultado_pilot_job <- resultado_pilot[resultado_pilot$valor_coluna == 1,]
  
  # Obtendo apenas os itens  que estavam em algum Cluster
  resultado_pilot_job_final <- resultado_pilot_job[resultado_pilot_job$Cluster != 0, ]
  
  
  resultado_pilot_job_final <- resultado_pilot_job_final %>% 
    group_by(Cluster, nome_coluna) %>% 
    summarise(V1 = sum(Qtd))
  
  resultado_pilot_job_final <- resultado_pilot_job_final[order(-resultado_pilot_job_final$V1),]
  
  
  resultado_aggregate <- aggregate(nome_coluna ~ Cluster, data = resultado_pilot_job_final, c)
  
  
  resultado_select <- distinct(select(resultado_pilot_job_final, Cluster, V1))
  
  
  resultado_final <- resultado_aggregate %>% inner_join(resultado_select)

  
  resultado_final$nome_coluna <- as.character(resultado_final$nome_coluna)
  
  resultado_final$nome_coluna <- gsub("c(", "", resultado_final$nome_coluna, fixed = TRUE)
  resultado_final$nome_coluna <- gsub("\"", "", resultado_final$nome_coluna, fixed = TRUE)
  resultado_final$nome_coluna <- gsub(")", "", resultado_final$nome_coluna, fixed = TRUE)
  resultado_final$nome_coluna <- gsub(",", ";", resultado_final$nome_coluna, fixed = TRUE)
  
  resultado_final <- resultado_final[order(-resultado_final$V1),]
  
  return(resultado_final)
}