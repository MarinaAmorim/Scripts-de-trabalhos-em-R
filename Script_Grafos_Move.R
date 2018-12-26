

###########################################################################
#
# Analise de redes , grafos
# Marina Alves Amorim 
# marinaaamorim@hotmail.com
# Rede de onibus do MOVE 
# Descritivas e abordagem de alguns problemas
###########################################################################


rm(list = ls())
if(!require(network)){install.packages("network");require(network)}
if(!require(igraph)){install.packages("igraph");require(igraph)}
if(!require(intergraph)){install.packages("intergraph");require(intergraph)}
require(xtable)

setwd("/home/labpos/Documentos/isabela/trab1/")
dir()
setwd("F:\\UFMG\\MESTRADO\\redes socias\\12 outubro")
dados<-load("AV1.RData")
dados

head(Tarifa)
head(Quilometragem)
dim(Tarifa)
dim(Quilometragem)
###################################

# a. Gere o(s) grafo(s) que julgar necessário(s) para representar este problema. O(s) grafo(s) é(são) conexo(s)?
# estruturando o grafo

# foram gerados 2 grafos, um com penderacao de distancia e outro com tarifa
g <- graph_from_adjacency_matrix(Quilometragem, mode="undirected", weighted = T)
V(g)$name # nomes dos vertices
plot(g)

g1 <- graph_from_adjacency_matrix(Tarifa, mode="undirected", weighted = T)
V(g1)$name # nomes dos vertices
plot(g1)

is.connected(g)
is.connected(g1)

vcount(g)
ecount(g)


# b. Calcule o número localidades que podem ser alcançadas tomando apenas um ônibus para cada local.
# calcula a vizinhanca do vertice
ego(g, order = 1, nodes = V(g), mode = "all",  mindist = 0)
bsoma<-sum((ego_size(g, order = 1, nodes = V(g), mode = "all",  mindist = 1))==1)
# 38 localidades

# c Encontre a distância média percorrida para chegar de um ponto de ônibus que pode
# ser alcançado tomando-se apenas um ônibus.
# ego com peso 

ponto1<-(ego_size(g, order = 1, nodes = V(g), mode = "all",  mindist = 1))==1
# do vertice 12 ate 50 sao os que podem ser alcancados tomando apenas 1 onibus
soma<-0
for(i in 1:length(ponto1)){
  if(ponto1[i]==T)
    soma<-soma+sum(make_ego_graph(g, order = 1, nodes = V(g), mode = "all",  mindist = 0)[[i]][1])
}
soma/38

# media de 12.02763 km


# d Qual a porcentagem de rotas diretas (tomando apenas um ônibus) em relação ao total
# de rotas diretas possíveis?
# é a dendidade de g 
# rotas diretas (tomando apenas um ônibus) é o numero de arestas

# A proporcao de ligacoes observadas (arestas, arcos ou relacoes)
# numa rede em relacao ao numero maximo possivel de ligacoes.
# Para **redes não-direcionadas**, a densidade corresponde a
2*vcount(g)/(ecount(g)*(ecount(g)-1))


# e Qual o número máximo de ônibus que um passageiro precisa tomar para chegar a
# qualquer ponto de ônibus? Mostre o trajeto correspondente a este percurso, bem como
# a distância percorrida e a tarifa cobrada no percurso

# é o diametro da rede
# Corresponde ao mais comprido dos caminhos mais curtos entre todos os pares de vertices

diameter(g, weights=NA)
get_diameter(g, weights=NA)
# o numero maximo é de 4 onibus, 5 pontos
# existem varios trajetos com numero maximo é de 4 onibus
# aqui alguns exemplos
all_shortest_paths(g, "Kennedy", to = "Canaa",weights=NA)
# Kennedy--Vilarinho--Centro--Ibirite--Canaa
# ou Kennedy--Vilarinho--Area Hospitalar--Ibirite--Canaa
# ou Kennedy--Vilarinho--Cidade Industrial--Ibirite--Canaa 

plot(g1,mark.groups = list("Kennedy","Vilarinho","Centro","Cidade Industrial","Ibirite","Area Hospitalar","Canaa"))

path1<-c("Kennedy","Vilarinho","Centro","Ibirite","Canaa")
path2<-c("Kennedy","Vilarinho","Area Hospitalar","Ibirite","Canaa")
path3<-c("Kennedy","Vilarinho","Cidade Industrial","Ibirite","Canaa")

# distância percorrida dos 3 caminhos
gpath1<- induced.subgraph(g, path1)
diameter(gpath1)  # 59.3
gpath2<- induced.subgraph(g, path2)
diameter(gpath2)  # 62.3
gpath3<- induced.subgraph(g, path3)
diameter(gpath3) #55.8

# tarifa cobrada no percurso dos 3 caminhos
gpath11<- induced.subgraph(g1, path1)
diameter(gpath11)
gpath12<- induced.subgraph(g1, path2)
diameter(gpath12)
gpath13<- induced.subgraph(g1, path3)
diameter(gpath13)

# os 3 caminhos geram o mesmo custo ao usuario da linha = R$ 20,65

# f Repita o item e, encontrando a distância máxima percorrida. Mostre quantos ônibus
# cobrem esse percurso, bem como a tarifa cobrada.
diameter(g) # ja leva em consideracao a km 106
get_diameter(g) # precisa de 2 onibus e 3 estacoes
all_shortest_paths(g, "Caete", to = "Jaboticatubas")

# tarifa cobrada entre Caete--Sao Gabriel--Jaboticatubas
diameter(induced.subgraph(g1,c("Caete", "Sao Gabriel","Jaboticatubas")))
# R$ 25.65

# g Repita o item e, encontrando o custo agregado máximo. Mostre quantos ônibus
# cobrem esse percurso, bem como a distância percorrida.
diameter(g1) # ja leva em consideracao tarifa
get_diameter(g1) # precisa de 3 onibus e 4 estacoes
all_shortest_paths(g1, "Matozinhos", to = "Jaboticatubas")

# distancia entre Matozinhos--Vilarinho--Sao Gabriel--Jaboticatubas
diameter(induced.subgraph(g,c("Matozinhos","Vilarinho","Sao Gabriel","Jaboticatubas")))
# 99.7 km


# h. Encontre todos os menores caminhos entre a Estação Vilarinho e os 
# outros pontos em termos de distância.
all_shortest_paths(g, "Vilarinho", to = V(g)) 
vila<-shortest.paths(g,"Vilarinho", V(g))
xtable(t(vila))
# como o grafo ja tem atributo de peso como distancia entao nao é preciso usar weights



#i. Encontre todos os menores caminhos entre a Estação São Gabriel e os outros pontos
# em termos de número de ônibus necessários.
all_shortest_paths(g, "Sao Gabriel", to = V(g), weights=NA) 
gabriel<-shortest.paths(g,"Sao Gabriel", V(g), weights=NA)

xtable(t(gabriel))
# usando assim weights=NA so leva em consideracao o numero de onibus
# assim, aparece mais de um caminho para o msm destino


# j. Encontre todos os menores caminhos entre a Estação Justinópolis e os 
# outros pontos em termos de custo total.
all_shortest_paths(g1, "Justinopolis", to = V(g1)) 
just<-shortest.paths(g1, "Justinopolis", to = V(g1)) 
xtable(t(just))
# como muitas tarifas tem o msm preco tb gerou mais de uma opcao para alguns destinos

# k Aponte algumas diferenças entre os resultados dos itens h, i e j, se houver.
# em h, gerou apenas apenas um caminho para cada destino, levando em consideracao a menor distancia
# ja em i e j, como levou em consideracao arestas e tarifas, foi tratado como fator, e assim 
# gerou mais de um caminho para o mesmo destino

