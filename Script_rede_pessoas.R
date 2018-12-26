#===================================
# Lista 2 - Grafos
# Rede de pessoas 
# Isabela , Marina e Silvio
#===================================
# Diretorio
setwd("C:/Users/marin/Downloads/Lista 2 grafos")
#setwd("C:\\Users\\Gisele\\Desktop")

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

######## Redes de pessoas ###########
# Criando matrizes de adjacência

#TERMO INDUTOR -  REDES 

# Rede Geral

redes = data[,c("Nome", paste0("RPALA",1:5))]

# Criando objetos wordevok

(wordred = as.wordevok(redes,index = "Nome"))

# Removendo loops

(wordred = removing_loops_wordevok(wordred))

a = wordevok_affinity_adjacency(wordred) # afinidade de pensamento entre as pessoas com todas as palavras

# Gerando rede de pessoas

g = graph_from_adjacency_matrix(a,mode = "undirected",weighted = T)

tkplot(g)
# Medidas descritivas

igraph::is.connected(g)
descp =cbind(igraph::degree(g),
             igraph::strength(g),
             igraph::betweenness(g),
             igraph::closeness(g))

edge_density(g)


colnames(descp) = c("Grau","Grau Ponderado","Intermediacao","Proximidade")
descp
xtable(descp)

# Decomposicao da rede em comunidades

decmp = (cluster_louvain(g,weights = E(g)$weight))
decmp
decmp[[3]]

#========================================================
# Termo inddutor inferencia
data
# Rede Geral

inf = data[,c("Nome", paste0("IPALA",1:5))]

# Criando objetos wordevok

(wordrede = as.wordevok(inf,index = "Nome"))

# Removendo loops

(wordrede = removing_loops_wordevok(wordrede))

a1 = wordevok_affinity_adjacency(wordrede) # afinidade de pensamento entre as pessoas com todas as palavras

g2 = graph_from_adjacency_matrix(a1,mode = "undirected",weighted = T)

tkplot(g2)
# Medidas descritivas

igraph::is.connected(g2)
descpe =cbind(igraph::degree(g2),
              igraph::strength(g2),
              igraph::betweenness(g2),
              igraph::closeness(g2))

edge_density(g)


colnames(descpe) = c("Grau","Grau Ponderado","Intermediacao","Proximidade")
descpe
xtable(descpe)

# Decomposicao da rede em comunidades

decmpe = (cluster_louvain(g2,weights = E(g2)$weight))
decmpe
decmpe[[2]]
#========================================================
# Termo inddutor Desenvolvimento

# Rede Geral

des = data[,c("Nome", paste0("DPALA",1:5))]

# Criando objetos wordevok

(wordredd = as.wordevok(des,index = "Nome"))

# Removendo loops

(wordredd = removing_loops_wordevok(wordredd))

a2 = wordevok_affinity_adjacency(wordredd) # afinidade de pensamento entre as pessoas com todas as palavras

# Gerando rede de pessoas

g3 = graph_from_adjacency_matrix(a2,mode = "undirected",weighted = T)

tkplot(g3)
# Medidas descritivas

igraph::is.connected(g3)
descpd =cbind(igraph::degree(g3),
             igraph::strength(g3),
             igraph::betweenness(g3),
             igraph::closeness(g3))

edge_density(g3)


colnames(descpd) = c("Grau","Grau Ponderado","Intermediacao","Proximidade")
descpd
xtable(descpd)

# Decomposicao da rede em comunidades

decmpd = (cluster_louvain(g3,weights = E(g3)$weight))
decmpd
decmpd[[2]]

#############################
# Redes = g
# Desenvolvimento = g2
# inferencia = g3

plot(g)

plot(g2)

plot(g3)

g
g2
g3
#=================================================================
# Decomposicao da rede em comunidades

slw = wordevok_comm_subsets(wordred,decmp)

# Lista de frequencia e ordem média de evocação

tw = telp_wordevok(wordred,decmp)

# Lista de frequencia e ordem média de evocação

twq = telp_wordevok_quad(tw, method = "mean")
# pode escolher entre media mediana e moda


# Grafico TELP
# analise de quadrantes -  qual seria o que restringe mais no 1 quadrante
# preto media
# vermelho mediana
# roxo moda
# cada bolinha é uma evolcacao da comunidade
# cada grafico e uma comunidade
telp_wordevok_plot(tw[[1]])
telp_wordevok_plot(tw[[2]])

# Lendo as classes para o radar do pensamento
# vai ter os super grupos
# faz a classificacao das palavras, isso vai ser feito para cada banco de dados
class = as.data.frame(cbind(wordred$Evocations,wordred$Evocations ))
colnames(class) = c("Evocaçoes", "classe")
# Tomando as coordenadas do radar do pensamento

wra = wordevok_radar_attr(twq,class)
# essas list de wra ja é o que vai ser utilizado no radar


wrgg = wordevok_radar_gg(wra)
# 10 Community 3    Redes Sociais 1.0000000 deu prioridade maxima no radar

require(ggplot2)

wrp = wordevok_radar_plot(wrgg) # cria um radar para cada comunidade usano ggplot2

wrp[[1]]
wrp[[2]]

wrp[[3]]

#=================================================================
# Decomposicao da rede em comunidades

slw = wordevok_comm_subsets(wordrede,decmpe)

# Lista de frequencia e ordem média de evocação

tw = telp_wordevok(wordrede,decmpe)

# Lista de frequencia e ordem média de evocação

twq = telp_wordevok_quad(tw, method = "mean")
# pode escolher entre media mediana e moda


# Grafico TELP
# analise de quadrantes -  qual seria o que restringe mais no 1 quadrante
# preto media
# vermelho mediana
# roxo moda
# cada bolinha é uma evolcacao da comunidade
# cada grafico e uma comunidade
telp_wordevok_plot(tw[[1]])
telp_wordevok_plot(tw[[2]])

# Lendo as classes para o radar do pensamento
# vai ter os super grupos
# faz a classificacao das palavras, isso vai ser feito para cada banco de dados
class = as.data.frame(cbind(wordrede$Evocations,wordrede$Evocations ))
colnames(class) = c("Evocaçoes", "classe")
# Tomando as coordenadas do radar do pensamento

wra = wordevok_radar_attr(twq,class)
# essas list de wra ja é o que vai ser utilizado no radar


wrgg = wordevok_radar_gg(wra)
# 10 Community 3    Redes Sociais 1.0000000 deu prioridade maxima no radar

require(ggplot2)

wrp = wordevok_radar_plot(wrgg) # cria um radar para cada comunidade usano ggplot2

wrp[[1]]
wrp[[2]]

#=================================================================
# Decomposicao da rede em comunidades

slw = wordevok_comm_subsets(wordredd,decmpd)

# Lista de frequencia e ordem média de evocação

tw = telp_wordevok(wordredd,decmpd)

# Lista de frequencia e ordem média de evocação

twq = telp_wordevok_quad(tw, method = "mean")
# pode escolher entre media mediana e moda


# Grafico TELP
# analise de quadrantes -  qual seria o que restringe mais no 1 quadrante
# preto media
# vermelho mediana
# roxo moda
# cada bolinha é uma evolcacao da comunidade
# cada grafico e uma comunidade
telp_wordevok_plot(tw[[1]])
telp_wordevok_plot(tw[[2]])

# Lendo as classes para o radar do pensamento
# vai ter os super grupos
# faz a classificacao das palavras, isso vai ser feito para cada banco de dados
class = as.data.frame(cbind(wordredd$Evocations,wordredd$Evocations ))
colnames(class) = c("Evocaçoes", "classe")
# Tomando as coordenadas do radar do pensamento

wra = wordevok_radar_attr(twq,class)
# essas list de wra ja é o que vai ser utilizado no radar


"
[[2]]$`Quad1`
Conexão Estrutura    Outros    Grafos Aplicação Compartilhamento Comunicação Modelagem
valor maximo do radar 1 0.8941322 0.8941322 0.8941322 0.8941322 0.8941322        0.8941322   0.8941322 0.8941322
valor mini do radar 2 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000        0.0000000   0.0000000 0.0000000
3 0.8941322 0.3465736 0.0000000 0.3465736 0.0000000        0.0000000   0.3960841 0.0000000
"


wrgg = wordevok_radar_gg(wra)
# 10 Community 3    Redes Sociais 1.0000000 deu prioridade maxima no radar

require(ggplot2)

wrp = wordevok_radar_plot(wrgg) # cria um radar para cada comunidade usano ggplot2

wrp[[1]]
wrp[[2]]

# =======================================

# Matriz de afinidade
#a = redes
# a1 = inferencia
#a2 = desenvolvimento
getwd()
write.csv(a,"redes_pessoas.csv",row.names = T)


# ========================

x=c("42,3%", "60,6%", "26,4%")
x1=c("36,11%","66,0%", "60,0%")
x2=c(2,2,3)
x3 = c(2,2,2)
x4=c(0.079,0.32,0.3)
x5=c(0.0031,0.077,0.25)

y=cbind(x,x2,x4)
y

y1 = cbind(x1,x3,x5)
y1

k = rbind(y,y1)
k
colnames(k)=c("Densidade", "Nº Grupos", "Moduralidade")
rownames(k)=c("Redes Manual", "Inferência Manual", "Desenvolvimento Manual",
              "Redes Automático", "Desenvolvimento Automático",
              "Infrência Automático")
k
require(xtable)
xtable(k)
