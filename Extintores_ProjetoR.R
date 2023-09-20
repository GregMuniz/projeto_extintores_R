library(readxl)
extintores <- read_excel('Acoustic_Extinguisher_Fire_Dataset.xlsx')
View(extintores)

colnames(extintores) <- c('Tamanho_Recipiente', 'Tipo_Combustivel', 'Distancia', 'Decibeis', 
                          'Fluxo_Ar', 'Frequencia', 'Extincao_Chamas')
View(extintores)

#Extincao_Chamas é a variável target portanto a variável a ser prevista, com base nas variáveis 
#Preditoras, é uma variavel categórica onde o número 0 representa falha/não extinção das chamas
#e o 1 representa sucesso na extinção das chamas.
#Será feita uma análise dos dados e uma limpeza caso seja necessario e depois seguir para o
#processamento e criação dos modelos de ML que nesse caso serão de classificação.

sum(is.na(extintores))


##ANALISE EXPLORATÓRIA##
summary(extintores)

#Proporção de dados por categoria da variável
table(extintores$Tipo_Combustivel)
table(extintores$Extincao_Chamas)


#Criação de um histograma para cada variável numérica do df
library(ggplot2)
extintores_numericas <- subset(extintores, select = c(-Tipo_Combustivel, -Extincao_Chamas))

histogramas <- lapply(names(extintores_numericas), function(col_name){
  ggplot(extintores_numericas, aes_string(x = col_name)) +
    geom_histogram(binwidth = 0.5, fill = 'blue', color = 'black') +
    labs(title = col_name, x = col_name, y = 'Contagem')
})
histogramas

#Analisando os histogramas da para ver que tamanho_recipiente pode ser interpretada como uma
#variável categórica de 1 a 7 onde 6 e 7 teve uma frequencia menor de testes
#As distancias foram testadas na mesma proporção
#Decibeis foram mais testados entre 90 e 96 e 102 a 108
#Fluxo de ar teve o maior numero de testes em torno de 0
#A frequencia também seguiu o mesma proporção de testes em todos os niveis


#Proporção de testes para cada combustivel
ggplot(extintores) +
  geom_bar(aes(x = Tipo_Combustivel), fill = 'blue', color = 'black')

#Numero de testes para cada combustivel e tamanho
ggplot(extintores) +
  geom_bar(mapping = aes(x = Tipo_Combustivel, fill = as.factor(Tamanho_Recipiente), legend = 'TamanhoRecipiente'), 
           position = 'dodge')
#Foram feitos os mesmos numeros de testes para cada tamanho de recipiente com cada combustivel
#Vemos que o lpg foram feitos com tamanhos diferentes dos outros 6 e 7, que representam meio e cheio.
#Por esse motivo que ele foi menos testado que os demais, pois foram feitos 1000 testes
#para cada tamanho, 2 tamanhos para ele e 5 para os demais.

#Relação entre o tamanho do recipiente e a quantidade de sucesso ou fracasso
#Vemos que a medida que sobe o tamanho do recipiente diminui a taxa de sucesso e vice versa.
ggplot(extintores) +
  geom_bar(mapping = aes(x = as.factor(Extincao_Chamas), fill = as.factor(Tamanho_Recipiente), legend = 'Tamanho Recipiente'),
           position = 'dodge')

#Correlação das variáveis preditoras numéricas com a variável alvo
library(corrplot)
matriz_cor <- cor(extintores[, -2])
corrplot(matriz_cor, type = 'upper', method = 'color')

#Forte correlação negativa entre a distancia do extintor e a extinçaõ das chamas ou seja
#Diminui a distancia aumenta a taxa de sucesso 

#Forte correlação positiva entre fluxo de ar e a variável alvo oque indica que 
#a medida que aumenta O fluxo de ar no local aumenta a taxa de sucesso.

#Pequena correlação positiva entre decibéis e a variavel preditiva o que pode implicar
#uma pequena taxa de sucesso maior a medida que aumenta a intensidade das ondas sonoras

#Pequena correlação negativa entre a frequencia e a variavel preditiva

##LEMBRANDO que todas essas correlações não são afirmações diante de uma situação real,
##Apenas que os dados indicam uma correlação, mais isso não implica causalidade##

#tabela de frequencia cruzada relacionando o número de sucesso e fracasso para cada combustivel
corr_categ <- extintores[, c(2, 7)]
frequencia <- table(corr_categ) 
frequencia

#Porcentagem de sucesso e fracasso para cada combustivel
round(prop.table(frequencia, margin = 1) * 100) 

#Teste do Qui-Qradrado para saber se há alguma correlãção entre as duas variáveis
#Hipotese Nula(H0) indica que as duas variáveis são independentes uma da outra
#Hipotese alternativa indica que as duas variaveis são dependentes uma da outra
chisq.test(frequencia)

#Valor p extremamente baixo logo rejeitamos H0 pois o valor é menor do que 0.05 logo
#existe evidencias de que as duas variáveis estão relacionadas.


##Pelo que aparenta pela a exploração todas as variáveis tem uma importancia significativa para
##A variável alvo, logo usaremos todas elas para a criação do nosso modelo de ML.


###----PRE-PROCESSAMENTO DE DADOS---###

#One hot encoding para a variavel categorica nominal Tipo combustivel
#Ja que é nominal e o dataset possui poucas váriaveis essa é uma boa opção!

extintores2 <- data.frame(model.matrix(~ . -1, data = extintores))
extintores2$Extincao_Chamas <- as.factor(extintores2$Extincao_Chamas)
str(extintores2)

#Salvando o dataset com as devidas mudanças pois ele será a base para crianção de vários modelos
write.csv(extintores2, file = 'C:/Users/greg/ProjetosML/Extintores_df')


#Normalização 


normalizacao <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
  
}

dados_norm <- as.data.frame(lapply(extintores2[6:9], normalizacao))
View(dados_norm)

colunas_norm <- c('Distancia', 'Decibeis', 'Fluxo_Ar', 'Frequencia')
extintores2[colunas_norm] <- dados_norm
View(extintores2)

###---CRIAÇÃO DOS MODELOS DE ML---###

#-------1* Modelo: KNN-------#

library(caret)
library(ISLR)
library(caTools)

proporcao_treino <- 0.7

indices <- sample.split(extintores2$Extincao_Chamas, SplitRatio = proporcao_treino)
dados_treino <- subset(extintores2, indices == TRUE)
dados_teste <- subset(extintores2, indices == FALSE)



KNN_Model_v1 <- train(Extincao_Chamas ~ .,
                      data = dados_treino,
                      method = 'knn')


KNN_Model_v1
plot(KNN_Model_v1)

#k = 7 foi o que apresentou melhor performance na criação do modelo

#Predição do modelo
KNN_Model_Predict <- predict(KNN_Model_v1, newdata = dados_teste)
KNN_Model_Predict

#ConfusionMatrix
confusionMatrix(KNN_Model_Predict, dados_teste$Extincao_Chamas)

#F1_score
F1_score <- 2 * (0.9593 * 0.9585) / (0.9593 + 0.9585)
F1_score

library(pROC)

#Vetor com as probabilidades positivas do modelo
KNN_Model_Predict_Prob <- predict(KNN_Model_v1, newdata = dados_teste, type = 'prob')
positive_odds <- KNN_Model_Predict_Prob[, '1']

# Calcular a curva ROC e a AUC
roc_obj <- roc(dados_teste$Extincao_Chamas, positive_odds)
auc_value <- auc(roc_obj)

# Plotar a curva ROC
plot(roc_obj, main = "Curva ROC", print.auc = TRUE)


#Conseguimos uma acurácia de praticamente 96% para essa primeira versão do modelo algo
#bastante relevante.
#Um F1_Score de mesmo valor 
#E uma curva AUC de 99%


#-------2* Modelo: Decision Tree-------#

library(rpart)

#Criação do modelo
Decision_Tree_Modelo_v1 <- rpart(Extincao_Chamas ~ .,
                                 data = dados_treino)

#Predição
Decision_Tree_Predict <- predict(Decision_Tree_Modelo_v1, newdata = dados_teste, type = 'class')

#Matrix
confusionMatrix(Decision_Tree_Predict, dados_teste$Extincao_Chamas)

#Visualização do modelo em um plot
library(rpart.plot)
prp(Decision_Tree_Modelo_v1, type = 0, extra = 1, under = T, compress = T)

#Segunda versão, adicionei mais parâmetros
Decision_Tree_Modelo_v2 <- rpart(Extincao_Chamas ~ .,
                                 data = dados_treino,
                                 method = 'class',
                                 parms = list(split = 'information'),
                                 control = rpart.control(minsplit = 5))


Decision_Tree_Predict_V2 <- predict(Decision_Tree_Modelo_v2, newdata = dados_teste,
                                    type = 'class')

confusionMatrix(Decision_Tree_Predict_V2, dados_teste$Extincao_Chamas)
#O maior ajuste dos hiperparâmetros não obtiveram diferenças significativas de performance

#Terceira versão do modelo, usando o algoritmo C5.0
library(C50)

Decision_Tree_Modelo_v3 <- C5.0(dados_treino[-10], dados_treino$Extincao_Chamas)
Decision_Tree_Modelo_v3

summary(Decision_Tree_Modelo_v3)
#Vemos acima que as variáveis mais relevantes para o modelo foram Fluxo_ar, Distancia e frequencia

#Performance do Modelo
Decision_Tree_Predict_V3 <- predict(Decision_Tree_Modelo_v3, dados_teste)

confusionMatrix(Decision_Tree_Predict_V3, dados_teste$Extincao_Chamas)

#O algoritmo C5.0 obteve uma performance superior ao algoritmo rpart


#Quarta versão do modelo, personalisando o hiperparâmetro trials para 15, o que 
#pode resultar em um modelo mais preciso ja que ele tentará achar a melhor divisão
Decision_Tree_Modelo_v4 <- C5.0(dados_treino[-10],
                                dados_treino$Extincao_Chamas,
                                trials = 15)

summary(Decision_Tree_Modelo_v4)
#Vemos agora que as variáveis que não obtiveram tanta relevância na versão3 agora
#aumentaram drasticamente o nivel de relevancia

Decision_Tree_Predict_V4 <- predict(Decision_Tree_Modelo_v4, dados_teste)

confusionMatrix(Decision_Tree_Predict_V4, dados_teste$Extincao_Chamas)

#Probabiliades positivas
Decision_Tree_Predict_V4_Prob <- predict(Decision_Tree_Modelo_v4, dados_teste, type = 'prob')
positive_odds2 <- Decision_Tree_Predict_V4_Prob[, '1']

roc_obj2 <- roc(dados_teste$Extincao_Chamas, positive_odds2)
auc_value2 <- auc(roc_obj2)
plot(roc_obj2, main = 'Curva ROC', print.auc = TRUE)


#Conseguimos aumentar ainda mais a performance, conseguindo uma acurácia de 98%,
#E um AUC de 99.8%, Até o momento a melhor versão.


#-------3* Modelo: Random Forest------#

library(randomForest)

randomforest_Model_V1 <- randomForest(dados_treino[-10],
                                      dados_treino$Extincao_Chamas,
                                      ntree = 50)

randomforest_predicao_V1 <- predict(randomforest_Model_V1, dados_teste)

confusionMatrix(randomforest_predicao_V1, dados_teste$Extincao_Chamas)
?randomForest

#Segunda versão do modelo, aumentando o numero de árvores

randomforest_Model_V2 <- randomForest(dados_treino[-10],
                                      dados_treino$Extincao_Chamas,
                                      ntree = 200)

randomforest_predicao_V2 <- predict(randomforest_Model_V2, dados_teste)

confusionMatrix(randomforest_predicao_V2, dados_teste$Extincao_Chamas)

#Testei vários valores diferentes para ntree e todos continuaram apresentando praticamente 
#o mesmo nivel de acurácia


#Poderia continuar criando novas versões modificando mais hiperparâmetros, mas como já
#atingimos uma performance bastante alta com algoritmos mais simples, o ideal é usá-los,
#pois quando menor a complexidade do modelo mais generalizável ele se torná, assim tendo menores
#chances de ocorrer overfitting.

#Modelo que decidi usar como modelo final para deploy e futuras previsões
#Versão 4 do modelo de árvore de decisão, usando o algoritmo C0.5
saveRDS(Decision_Tree_Modelo_v4,'Modelo_Classificação_Extintores_C0.5')
  
