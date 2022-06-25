#### INTRODUÇÃO À CIÊNCIA DE DADOS COM R ####

##  Visualizando a área de trabalho do R ##
getwd()

## Instalando o pacote "tidyverse" ##
install.packages('tidyverse')
# habilitando o pacote #
library(tidyverse)
# visualizando as funções presentes no pacote "tidyverse" #
print(tidyverse_packages())

## Importando o conjunto de dados ##
# para nossa parte prática, iremos utilizar o dataset já embutido no R Studio, o "mpg" #

?mpg # visualizando as informações sobre o conjunto de dados #
cars <- mpg # instanciando o dataset no objeto "cars" #

## Análise exploratória do conjunto de dados ##
class(cars)      # o tipo do objeto #
head(cars, 15)   # visualizando as 15 primeiras observações #
tail(cars, 15)   # visualizando as 15 últimas observações #
names(cars)      # visualizando o nome das colunas #
dim(cars)        # visualizando a dimensão - linhas e colunas #
str(cars)        # visualizando a estrutura #

# estatística descritiva do conjunto de dados #
summary(cars)

# visualizando todo o conjunto de dados #
View(cars)

# plotando gráfico da relação das colunas "displ" e "cty" #
ggplot(cars, aes(displ, cty)) +
  geom_point(fill='darkgray', colour = "#3366FF", binwidth=1)

# instalando o pacote "plyr" para auxiliar na renomeção das colunas #
install.packages('plyr')
library(plyr) # habilitando o pacote #

# renomeando as colunas do conjunto de dados

## Dicionário de dados ##
# "manufacturer": marca do fabricante #
# "model": modelo do carro #
# "displ": cilindradas do motor (litros) #
# "year": ano de fabricação#
# "cyl": número de cilindros #
# "trans": tipo de transmissão (câmbio) #
# "drv": tipo de tração #
  # onde f = tração dianteira, r = tração traseira, 4 = 4wd
# "cty": consumo na cidade (milhas por galão) #
# "hwy": consumo na rodovia (milhas por galão) #
# "fl": tipo de combustível #
# "class": tipo do carro #

cars <- rename(mpg, 
               c('manufacturer' = 'marca', 'model' = 'modelo', 
                 'displ' = 'motor', 'year' = 'ano',
                 'cyl' = 'cilindros', 'trans' = 'cambio',
                 'drv' = 'tracao', 'cty' ='cons_cidade', 
                 'hwy' = 'cons_rodovia', 'fl' = 'tipo_comb',
                 'class' = 'classe'))

head(cars) # visualizando as 6 primeiras observações #
View(cars) # visualizando todo o dataset #

# plotando gráfico de dispersão entre as variáveis "motor" e "cons_cidade" #
ggplot(cars, aes(motor, cons_cidade)) +
  geom_point(fill='darkgray', colour = "#3366FF") +
  labs(title = 'Relação: motor VS consumo na cidade')

# plotando gráfico de dispersão entre as variáveis "motor", "cons_cidade", "tracao" e "classe" #
ggplot(cars, aes(motor, cons_cidade, 
                 size = tracao,
                 color = classe)) +
  geom_point() + 
  labs(title = 'Relação: motor VS consumo na cidade VS tipo de tração VS tipo do veículo')

# visualizando os dados da coluna "cons_cidade" #
cars$cons_cidade

# estatística descritiva da coluna "cons_cidade" #
summary(cars$cons_cidade)

# visualizando as informações da coluna em forma tabular #
table(cars$cons_cidade)

# visualizando a porcentagem da distribuição dos veículos conforme o seu consumo na cidade #
prop.table(table(cars$cons_cidade))
# com a função "round" arredondando a exibição da porcentagem #
round(prop.table(table(cars$cons_cidade)) * 100, 0)

# plotando gráfico da distribuição das observações em relação a variável "cons_cidade" #
ggplot(cars, aes(cons_cidade)) + 
  geom_histogram(fill='darkgray', colour = "#3366FF", binwidth=1) +
  labs(title = 'Distribuição das observações: consumo na cidade')

# habilitando o pacote "dplyr" para uso da função "pipe" (%>%) #
library(dplyr)

# filtando veículos com consumo na cidade igual a 9
cars %>% filter(cons_cidade == 9)

# visualização tabular do veículos com consumo na cidade igual a 9 #
View(cars %>% filter(cons_cidade == 9))

# aplicando seleção dos veículos com consumo na cidade menor que 11 #
consumo_menor11 <- cars %>% 
  select(marca, cons_cidade) %>%
  filter(cons_cidade <= 11)

# visualizando a tabela da seleção acima #
View(consumo_menor11)

# aplicando a função "tally()" para relacionar e contabilizar o consumo à marca #
consumo_menor11 %>% group_by(marca) %>%
  tally()

# visualizando a média do consumo na cidade da seleção acima vs marca
consumo_menor11 %>% group_by(marca) %>%
  summarise(mean(cons_cidade))


### BI vs CIÊNCIA DE DADOS ###

### REGRESSÃO LINEAR ###

# nome das colunas #
names(cars)

# plot da relação: consumo cidade vs número cilindros vs tipo câmbio vs tipo tração #
# e traçando a reta de regressão linear #
ggplot(cars, aes(motor, cons_cidade)) + 
  geom_point(aes(size = tracao, color = classe)) +
  geom_smooth(method = 'lm', formula = 'y ~ x') + 
  labs(title = 'Relação linear entre as variáveis: "motor", "cons_cidade, "tracao" e "classe"')

# instanciando dados fictícios de peso e altura #
peso <- c(45,50,60,55,58,56,48,53)
altura <- c(1.54,1.56,1.65,1.60,1.55,1.63,1.58,1.59)

# plotando gráfico de dispersão dos dados #
plot(peso, altura)

# aplicando a "lm" (função linear) nos dados "altura" e "peso" #
lm(altura ~ peso) # coeficientes: intercepto do eixo x e inclinação da reta #

# instanciando o resultado do cálculo da equação no objeto "dados #
dados <- lm(altura ~ peso)
dados # visualizando os valores: intercepto e angular da reta #

# instanciando o intercepto no objeto "intercepto" #
intercepto <- dados$coefficients[1]
intercepto # visualizando a variável #

# instanciando o angular da reta no objeto "angular_reta" #
angular_reta <- dados$coefficients[2]
angular_reta # visualizando a variável #

## FUNÇÃO LINEAR ##

# A função linear é um tipo especial de função do 1° grau cuja lei de formação #
# é do tipo f(x) = a.x (a é real e diferente de zero). Confira como se caracteriza #
# uma função linear! Uma função do 1° grau ou função afim é definida pela lei # 
# de formação f(x) = a.x + b, na qual a e b são reais e a ≠ 0. #

# 'https://pt.wikipedia.org/wiki/Fun%C3%A7%C3%A3o_afim'

# y = a*x + b #

# aplicando a equação linear nos dados da variáveis "altura" e "peso" #
lm(altura ~ peso)

# resultado: b - coeficiente linear = 1.328161  ---> x = 0
#            a - coeficiente angular = 0.004882  ---> inclinação da reta

# inserindo a reta no gráfico
abline(1.328161, 0.004882)

# podemos aplicar a reta de regressão no gráfico diretamente #
abline(lm(altura ~ peso))

# criando um data frame com os dados das variáveis "peso" e "altura" #
df <- data.frame(
  peso = c(45,50,60,55,58,56,48,53),
  altura = c(1.54,1.56,1.65,1.60,1.55,1.63,1.58,1.59)
)

# visualizando o data frame #
df         # exibindo a informação no console #
View(df)   # visualização tabular dos dados #

# plotando gráfico de dispersão dos dados do objeto "df" #
# e juntamente com a reta de regressão #
ggplot(dados, aes(peso, altura)) + 
  geom_point() +
  geom_smooth(method = 'lm', formula='y ~ x')

### COEFICIENTE DE DETERMINAÇÃO - R2 ###

# O coeficiente de determinação, também chamado de R², é uma medida de ajuste #
# de um modelo estatístico linear generalizado, como a regressão linear simples # 
# ou múltipla, aos valores observados de uma variável aleatória. O R² varia entre # 
# 0 e 1, por vezes sendo expresso em termos percentuais. Nesse caso, expressa a # 
# quantidade da variância dos dados que é explicada pelo modelo linear. Assim, # 
# quanto maior o R², mais explicativo é o modelo linear, ou seja, melhor ele se # 
# ajusta à amostra. Por exemplo, um R² = 0,8234 significa que o modelo linear # 
# explica 82,34% da variância da variável dependente a partir do # 
# regressores (variáveis independentes) incluídas naquele modelo linear. #
# 'https://pt.wikipedia.org/wiki/Coeficiente_de_determina%C3%A7%C3%A3o'

# instanciando a reta da altura média no gráfico #
retas <- ggplot(mapping = aes(peso, altura)) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = 'lm', formula='y ~ x') + 
  geom_hline(yintercept = mean(altura))

# visualizando o gráfico #
retas

# visualizando o valor médio da altura #
mean(altura)

## distância entre a média da altura e o valor do peso ##
retas + 
  geom_segment(aes(x = peso, y = altura, 
               xend = peso, yend = mean(altura)), color = 'red')

## distância entre a reta de regressão e o valor do peso ##
retas + 
  geom_segment(aes(x = peso, y = altura, 
                   xend = peso, yend = predict(lm(altura ~ peso))), 
               color = 'red')

## calculando o valor do R2 ##
sqt <- sum((mean(altura) - altura) ** 2)
sqres <- sum((predict(lm(altura ~ peso)) - altura) ** 2)
r2 <- (sqt - sqres) / sqt
r2

# visualizando o resultado geral  #
summary(lm(altura ~ peso))

## aplicando regressão linear com machine learning #
modelo <- lm(altura ~peso)

# realizando as predições com o modelo #
round(predict(modelo), 3)
# comparando as predições com o resultado real #
altura

# prevendo com novos valores #
novos_dados <- data.frame(peso = c(48,51,62))
novos_dados # visualizando os novos dados #
# realizando a predição com o modelo dos novos dados #
predict(modelo, novos_dados)

## ÁRVORE DE DECISÃO ##

# para aplicação prática, utilizaremos o dataset "iris" #
?iris       # documentação do conjunto de dados #
View(iris)  # visualizando o conjunto de dados #
str(iris)   # visualizando a estrutura do conjunto de dados #

# separando dados para treino e teste com os valores respectivos de 70 e 30% #

# instanciando o valor de 70% dos dados para treino #
train <- nrow(iris) * 0.7
train # visualizando a quantidade de dados para treino #

# aplicando aleatoriedade para separar os dados de treino #
sample(1:nrow(iris), train)

# setando a semente aleatória #
set.seed(123)

# filtrando aleatórios os dados de treino #
filter <- sample(1:nrow(iris), nrow(iris) * 0.7)

# isntanciando os dados de treino e teste #
train <- iris[filter,]
test <- iris[-filter,]

# visualizando os dados de treino e teste #
View(train)
View(test)

# instalando e habilitando o pacote para aplicar o modelo de árvore de decisão #
install.packages('rpart')
library(rpart)

# instanciando o modelo de árvore de decisão #
tree_mdl <- rpart(Species ~ ., data = train)

# visualizando o modelo #
tree_mdl

## instalando o pacote para realizar o plot da árvore de decisão ##
install.packages('rpart.plot')
library(rpart.plot) # habilitando o pacote #

# plotando o objeto gráfico da árvore de decisão #
prp(tree_mdl, extra = 1)

# realizando as predições com o modelo nos dados de treino e visualizando o resultado #
pred <- predict(tree_mdl, test, type = 'class')
View(pred)

# instalando e habilitando o pacote "caret" para realizar a exibição da matriz de confusão #
install.packages('caret')
library(caret)

# visualizando a matriz de confusão
confusionMatrix(pred, test$Species)







