###############################################
# Ajuste do modelo SBM - stochastic block model 
# Marina Amorim
# marinaaamorim@hotmail.com
###############################################

# OBJETIVO É OBTER A CLUSTERIZAÇÃO 
# ESTIMANDO A PARTIÇÃO ( INFERÊNCIA ) PROBABILIDADE DE ESTAR EM UMA CLASSE 
# clusteriza dado a inferencia do SBM

## generation of one SBM network
install.packages("blockmodels", dependencies = T)
require(blockmodels)

#Pacote aceita entrar apenas com matriz de adjacencias
#Ele já encontra as classes e melhor modelo

#Pacote usa criterio de kl e maxima verossimilhança para encontrar o melhor modelo
#########################################
# Tem que falar se quer testar se os vertices tem dist:
#Bernoulli, poisson ou gaussiana

#Latente no modelo é que vertice pertence a qual classe

# gerar um grafo com essas caracteristicas para ver se o modelo acerta
# gerando
npc <- 80 # nodes per class (numero de nos por classe)
Q <- 10 # classes numero
n <- npc * Q # nodes 
#Matriz Z é a matriz que fala a classe do no
Z<-diag(Q)%x%matrix(1,npc,1) #indice das classes / indica qual classe pertence cada nó
# Gerar a matriz de prob.
#Probabulidade de passar de uma classe para a outra
set.seed(1)
P<-matrix(runif(Q*Q),Q,Q) #probabilidade de transição entre classes
P 
#Gerar matriz de adjacencias
M<-1*(matrix(runif(n*n),n,n)<Z%*%P%*%t(Z)) ## adjacency matrix
M # Mostra a ligação das arestas

g = graph_from_adjacency_matrix(M, "undirected", weighted = T)
decmp = (cluster_louvain(g3,weights = E(g3)$weight))
decmp


# Nosso SBM tem matriz de adj. M e matriz de prob P

## estimation
# Como geramos um bernoulli acima
my_model <- BM_bernoulli("SBM",M ) # Estamos falando que o modelo é bernoulli
#Escolhe o modelo e da a matriz de adj.
my_model$estimate() #Gera o sbm
# Se nao sabe a dist. tem que pensar no que faz sentido para fazer o modelo.
# Ele vai comparar o número de classes
#ICL é criterio de KL, o menor indice indica o melhor modelo ( tipo um AIC)
#Integrated Classification Likelihood (ICL)

which.max(my_model$ICL)# mostra o que tem maior ICL
my_model$ICL # Qual o icl para cada numero de classes (1 classe, 2 classes e etc) (classe 3 melhor)
#Como deu negativo, pegar o mais proximo de zero 
my_model$PL #Maxima verossimilhança , pegar o menor também
my_model$memberships[[Q]]$Z #Divisão das classes (Q classes)
my_model$memberships[[1]]$Z #Todos os vértices da primeira classe
my_model$memberships[[Q]]$plot() # Mostra as classes e os vértices
my_model$model_parameters #parametros do modelo
# mostra as probabilidades de transição para cada uma das classes (se tivesse essas classe).
# prob de transição de ir da classe 2 para a classe 3 0.6221199 se tivessem 3 classes
#[[3]]
#[[3]]$`pi`
#[,1]       [,2]      [,3]
#[1,] 0.09626253 0.38713218 0.3393022
#[2,] 0.93772968 0.48968802 0.6221199
#[3,] 0.46316817 0.03975172 0.6240966
# PODEMOS COMPARAR COM A MATRIZ P
my_model$model_parameters[3]
P # ELE PLOTA DIFERENTE A MATRIZ

my_model$plot_obs_pred(Q) # mostra a ligação dos vértices
#distribuição das arestas
#Se ta branquinho quer dizer que nao tem ligação se preto tem ligação
#faz com quantas classes quiser
# o segundo grafico é a probabilidade de intensidade da ligação
#mesma coisa nos dois eixos

my_model$plot_obs_pred(Q) #
my_model$plot_parameters(Q) 


#-------------------------------------------------
#sbm dentro de cada classe é um bernoulli
# condicionado a classe o grafo é bernoulli ( pois temos a classe definida)
# sbm poisson , a dist dentro de cada classe é poisson.
#-------------------------------------------------

##
## SBM symmetric
##
## generation of one SBM_sym network
# Tem uma matrix M que seja simetrica
# Prob de ir de i para j é a maesma de j para i 
npc <- 30 # nodes per class
Q <- 3 # classes
n <- npc * Q # nodes
Z<-diag(Q)%x%matrix(1,npc,1)
P<-matrix(runif(Q*Q),Q,Q)
P[lower.tri(P)]<-t(P)[lower.tri(P)]
M<-1*(matrix(runif(n*n),n,n)<Z%*%P%*%t(Z)) ## adjacency matrix
M[lower.tri(M)]<-t(M)[lower.tri(M)] # muda neste comando
# Pega a matriz triangular inferior e transforma em triangular superior
## estimation
my_model <- BM_bernoulli("SBM_sym",M ) # Tem que falar que é SBM simetrico
my_model$estimate()
which.max(my_model$ICL)
my_model$ICL # 
my_model$PL
my_model$memberships[[Q]]$Z
my_model$memberships[[Q]]$plot()
my_model$model_parameters
my_model$plot_obs_pred(Q) 
my_model$plot_parameters(Q) 
#######################################
##
## LBM
# Mistura de SBM
##
## generation of one LBM network
######################################
# nao falou de que classe que os nós são
# mistura de SBM , tem duas matriz de transição
# Latente é porque nao sabemos se é Z1 ou Z2
npc <- c(50,40) # nodes per class
Q <- c(2,3) # classes
n <- npc * Q # nodes
Z1<-diag(Q[1])%x%matrix(1,npc[1],1)#confundir o algoritmo sobre qual classe pertence cada nó
Z2<-diag(Q[2])%x%matrix(1,npc[2],1)
# não sabemos de que classe cada nó veio
# Agora vamos estimar a Z1 , Z2 e alpha ?
# Ele faz dois SBM e mistura eles ?

#Tem duas candidatas de qual classe pertence o nó.
# Ele nao sabe de onde esta vindo (Z1 ou Z2)

P<-matrix(runif(Q[1]*Q[2]),Q[1],Q[2])
M<-1*(matrix(runif(n[1]*n[2]),n[1],n[2])<Z1%*%P%*%t(Z2)) ## adjacency matrix
# mistura 2 latentes, para nao saber de qual veio
## estimation
my_model <- BM_bernoulli("LBM",M )
my_model$estimate()
which.max(my_model$ICL)
my_model$ICL
my_model$PL

my_model$memberships[[Q]]$Z
my_model$memberships[[Q]]$plot()
my_model$model_parameters
my_model$plot_obs_pred(Q) 
my_model$plot_parameters(Q) 


##
## SBM Covariates
##
## generation of one SBM network
npc <- 30 # nodes per class
Q <- 3 # classes
n <- npc * Q # nodes
sigmo <- function(x){1/(1+exp(-x))}
Z<-diag(Q)%x%matrix(1,npc,1)
Mg<-8*matrix(runif(Q*Q),Q,Q)-4
Y1 <- matrix(runif(n*n),n,n)-.5
Y2 <- matrix(runif(n*n),n,n)-.5
M_in_expectation<-sigmo(Z%*%Mg%*%t(Z) + 5*Y1-3*Y2)
M<-1*(matrix(runif(n*n),n,n)<M_in_expectation)
## estimation
my_model <- BM_bernoulli_covariates("SBM",M,list(Y1,Y2) )
my_model$estimate()
which.max(my_model$ICL)

##
## SBM symmetric
##
## generation of one SBM_sym network
npc <- 30 # nodes per class
Q <- 3 # classes
n <- npc * Q # nodes
sigmo <- function(x){1/(1+exp(-x))}
Z<-diag(Q)%x%matrix(1,npc,1)
Mg<-8*matrix(runif(Q*Q),Q,Q)-4
Mg[lower.tri(Mg)]<-t(Mg)[lower.tri(Mg)]
Y1 <- matrix(runif(n*n),n,n)-.5
Y2 <- matrix(runif(n*n),n,n)-.5
Y1[lower.tri(Y1)]<-t(Y1)[lower.tri(Y1)]
Y2[lower.tri(Y2)]<-t(Y2)[lower.tri(Y2)]
M_in_expectation<-sigmo(Z%*%Mg%*%t(Z) + 5*Y1-3*Y2)
M<-1*(matrix(runif(n*n),n,n)<M_in_expectation)
M[lower.tri(M)]<-t(M)[lower.tri(M)]
## estimation
my_model <- BM_bernoulli_covariates("SBM_sym",M,list(Y1,Y2) )
my_model$estimate()
which.max(my_model$ICL)


