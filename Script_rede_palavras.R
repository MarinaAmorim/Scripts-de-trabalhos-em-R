#---------------------------------------
# Lista 2 - parte  1
# Rede com evocações
# rede de Palavras
#---------------------------------------
# Diretorio
setwd("C:/Users/marin/Downloads/Lista 2 grafos")

#Pacotes 
if(!require(devtools)){install.packages("devtools");require(devtools)}
if(!require(wordevok)){devtools::install_github("wesleyhspereira/wordevok");require(wordevok)}
require(igraph)
require(xlsx)
require(igraph)   # Pacote igraph - pacote de geração e manipulação de grafos
require(xlsx)     # Pacote xlsx - carregar dados da extensão .xlsx 
require(sand)     # Pacote sand -  
require(statnet)  # Pacote statnet -  
require(ergm)     # Pacote ergm -  
#install.packages("dummies")
require(dummies)
require(readxl)
#--------------------------------------------
#Dados
data = read.xlsx("dados.xlsx", header = T, sheetIndex = 1, encoding = "UTF-8")
head(data)
data=data[,1:19]
atributos = data[,c("Nome","Sexo","Curso")]
# Definir quec 9999 ele deve reconhecer como NA
data[data==9999] <- NA
is.na(data)
#------------------------------------------

names(data)
dim(data)
# Rede Geral

redes = data[,c("Nome",paste0("RPALA",1:5))]
dim(redes)
# Criando objetos wordevok

(wordred = as.wordevok(redes,index = "Nome"))

# Removendo loops

(wordred = removing_loops_wordevok(wordred))

######## Redes de pensamento #########
# Criando lista de palavras

(edgred = wordevok_meaning_list(wordred,"Simplify"))

edgred$type = "undirected"

# Exportando arquivos para CSV

write.csv(edgred,"edgred.csv",row.names = F)

#write.xlsx(edgred,"edgred_redes.xlsx")

# Gerando rede de pensamento
require(igraph)

p = graph_from_data_frame(edgred,directed=F)
class(p)
# Medidas descritivas
edge_density(p)
plot(p)
tkplot(p)
igraph::is.connected(p)
descp =cbind(igraph::degree(p),
             igraph::strength(p),
             igraph::betweenness(p),
             igraph::closeness(p))

colnames(descp) = c("Grau","Grau Ponderado","Intermediacao","Proximidade")
descp

class(descp)
require(xtable)
xtable(descp)
# Decomposicao da rede em comunidades

decmp = (cluster_louvain(p,weights = E(p)$weight))
decmp

# 2 grupos e modularidade de 0,079


#--------------------------------------------------
# Desenvolvimento

des = data[,c("Nome",paste0("DPALA",1:5))]

# Criando objetos wordevok

(wordredd = as.wordevok(des,index = "Nome"))

# Removendo loops

(wordredd = removing_loops_wordevok(wordredd))

######## Redes de pensamento #########
# Criando lista de palavras

(edgredd = wordevok_meaning_list(wordredd,"Simplify"))

edgredd$type = "undirected"

# Exportando arquivos para CSV

write.csv(edgredd,"edgredD.csv",row.names = F)

# Gerando rede de pensamento
require(igraph)
p2 = graph_from_data_frame(edgredd,directed=F)
class(p2)
# Medidas descritivas
plot(p2)
edge_density(p2)
igraph::is.connected(p2)
descpd =cbind(igraph::degree(p2),
              igraph::strength(p2),
              igraph::betweenness(p2),
              igraph::closeness(p2))

colnames(descpd) = c("Grau","Grau Ponderado","Intermediacao","Proximidade")
descpd
xtable(descpd)

# Decomposicao da rede em comunidades

decmpd = (cluster_louvain(p2,weights = E(p2)$weight))
decmpd[[3]]

# 3 grupos e modularidade de 0,3
#--------------------------------------------------
#Infereência

inf = data[,c("Nome",paste0("IPALA",1:5))]

# Criando objetos wordevok

(wordrede = as.wordevok(inf,index = "Nome"))

# Removendo loops

(wordrede = removing_loops_wordevok(wordrede))

######## Redes de pensamento #########
# Criando lista de palavras

(edgrede = wordevok_meaning_list(wordrede,"Simplify"))

edgrede$type = "undirected"

# Exportando arquivos para CSV

write.csv(edgrede,"edgrede.csv",row.names = F)

# Gerando rede de pensamento
require(igraph)
p3 = graph_from_data_frame(edgrede,directed=F)
class(p3)
# Medidas descritivas
plot(p3)

edge_density(p3)
igraph::is.connected(p3)
descpe =cbind(igraph::degree(p3),
              igraph::strength(p3),
              igraph::betweenness(p3),
              igraph::closeness(p3))

colnames(descpe) = c("Grau","Grau Ponderado","Intermediacao","Proximidade")
descpe

xtable(descpe)

# Decomposicao da rede em comunidades

decmpe = (cluster_louvain(p3,weights = E(p3)$weight))
decmpe

# 5 grupos e modularidade de 0,57

#############################
# Redes = p
# Desenvolvimento = p2
# inferencia = p3

plot(p)

plot(p2)

plot(p3)

p
p2
p3

