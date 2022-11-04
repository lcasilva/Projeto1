# Análise Exploratória de Dados - TEMPLATE EXEMPLO

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/Users/User/Cursos/DSA/FCD/1-BigDataRAzure/Cap11-Machine_Learning_em_linguagem_R")
getwd()

#### Carregando o dataset ####

# Carregando a biblioteca readr a ser usada na leitura do dataset
library(readr)

# Carregando o dataset
carros <- read_csv("carros-usados.csv")
spec(carros)
str(carros)
View(carros)
# BMI - Body Mass Index (Índice de Massa Corpórea)


##########################################################
# Análise Exploratória de dados para Variáveis Numéricas #
##########################################################

# Estatística Descritiva
# É a etapa inicial da análise utilizada para descrever e resumir os dados
# Com Estatística Descritiva podemos descrever os dados usando 2 tipos de medidas:

# 1.Medidas de Tendência Central (Medidas de Posição)
# Essas medidas nos ajudam a descrever a centralidade e o posicionamento dos dados
# Exemplos: Média, Mediana, Moda, Valores Máximo e Mínimo, Amplitude, Quartil, Percentil:
summary(carros$ano)
summary(carros[c('preco','kilometragem')])

mean(carros$preco)
median(carros$preco)
moda = function(dados) {
  vetor = table(as.vector(dados))
  names(vetor)[vetor == max(vetor)]
}
moda(carros$preco)

quantile(carros$preco)
quantile(carros$preco, probs = c(0.01, 0.99))
quantile(carros$preco, seq(from = 0, to = 1, by = 0.2))
quantile(carros$preco, seq(0, 1, 0.25))
IQR(carros$preco) # Inter Quartile Range = Diferença entre Q3 e Q1
?IQR
range(carros$preco)
diff(range(carros$preco)) # Amplitude = Diferença entre max e min (max - min)


# 2.Medidas de Dispersão: Desvio Padrão e Variância
# Medidas que descrevem o grau de variação de um conj. de dados em relação a média
var(carros$preco)
sd(carros$preco)
mean(carros$preco)

var(carros$kilometragem)
sd(carros$kilometragem)
mean(carros$kilometragem)

# Coeficiente de Variação (Não aplicado neste exemplo)
# CV = (desvio padrão / média) * 100

# Ao interpretar a variância, números maiores indicam que os dados estão espalhados 
# mais amplamente em torno da média. O desvio padrão indica, em média, a quantidade 
# de cada valor diferente da média.

# Em outras palavras, se tivermos valores muito altos para variância e desvio 
# padrão significa que os dados estão muito espalhados com relação a média. A
# média é nossa métrica inicial; em torno da média teremos a distribuição dos
# dados. Se os dados estão muito espalhados da média então se tem uma variância
# e desvio padrão muito grandes. Isso pode representar mais adiante problemas
# durante o treinamento do modelo de machine learning.


### Gráficos para análise de apenas uma variável - Boxplot e histograma ####

#### Boxplot - Leitura de baixa para cima: Q1, Q2(Mediana), Q3 etc ####
boxplot(carros$preco, main = "Boxplot para os preços de carros usados", ylab = "Preço (R$)")
boxplot(carros$kilometragem, main= "Boxplot para a Kilometragem de carros usados", ylab = "Km")

#### Histograma ####
hist(carros$preco, main = "Histograma para os preços dos carros usados", xlab = "Preço (R$)")
library(moments)
SK = skewness(carros$preco)
print(SK)
# O coeficiente de assimetria é o que permite dizer se uma determinada distribuição é assimétrica ou não.
# Interpretação do skewness:
# Se sk ≈ 0: Dados simétricos. Tanto a cauda do lado direito e a do lado esquerdo da função densidade de probabilidade são iguais;
# Se sk < 0: Assimetria negativa. A cauda do lado esquerdo da função densidade de probabilidade é maior do que a do lado direito;
# Se sk > 0: Assimetria positiva. A cauda do lado direito da função densidade de probabilidade é maior do que a do lado esquerdo.

# No exemplo citado, o coeficiente de assimetria é -0.4207157 (< 0). Portanto, diz-se que a curva apresenta assimetria negativa,
# a cauda do lado esquerdo da função de densidade de probabilidade é maior do que a cauda do lado direito.
# Ao observar também o histograma, percebe-se que há maior densidade de dados no lado esquerdo da função.

hist(carros$kilometragem, main = "Histograma para a Kilometragem dos carros usados", xlab = "Distância (Km)")
hist(carros$kilometragem, main = "Histograma para a Km de Carros Usados", breaks = 5, ylab = "Kilometragem (Km)")
SK = skewness(carros$kilometragem)
print(SK)
# Já neste caso, o coeficiente de assimetria é 1.244226 (> 0). Portanto, diz-se que a curva apresenta assimetria positiva,
# a cauda do lado direito da função de densidade de probabilidade é maior do que a cauda do lado esquerdo.
# Ao observar também o histograma, percebe-se que há maior densidade de dados no lado direito da função.

#### Em ambos os casos, com vistas a aprendizagem de máquina há a necessidade de se balancear os dados para se gerar um histograma simétrico.

#### Coeficiente de curtose ####
library(moments)
ck = kurtosis(carros$preco)
print(ck)

ck = kurtosis(carros$kilometragem)
print(ck)

# Interpretação da kurtosis:
# Se CK ≈ 0: Distribuição normal. Chamada de Curtose Mesocúrtica;
# Se CK < 0: Gráfico achatado. Para um coeficiente de Curtose negativo, tem-se uma Curtose Platicúrtica;
# Se CK > 0: Gráfico alongado. Para um coeficiente de Curtose positivo, tem-se uma Curtose Leptocúrtica.
# Obs. A grande maioria dos algoritmos de ML espera receber os dados em uma distribuição normal.

# Para o exercício em pauta, os coeficientes de Curtose são iguais a 3.479568 e 4.565416 (CK > 0), portanto a curva é Leptocúrtica.


### Gráficos para análise de duas variáveis - Gráfico de Dispersão - Scatter plot ###

# Obs. Boxplot e o histograma servem para analisar uma única variável ...
# Porém, para analisar mais de uma variável pode-se utilizar o scatterplot ...

#### Covariância e Correlação ####
# A covariância entre duas variáveis (x,y) é uma medida de variabilidade conjunta dessas duas variáveis aleatórias.
# Covariância é uma medida de como as alterações em uma variável estão associadas às mudanças em uma outra variável.
# Especificamente, a covariância mede o grau em que duas variáveis estão linearmente associadas.

# Correlação é uma versão em escala da covariância que assume valores [-1, +1] onde os valores +1 e -1 indicam 
# associação linear perfeita e o valor 0 indica nenhuma relação linear. 
# Esse escalonamento torna a correlação invariante às mudanças na escala das variáveis originais.
# A constante de escala é o produto dos desvios padrão das duas variáveis.

# Portanto, o coeficiente de correlação 'p' mede o grau de correlação entre duas variáveis da seguinte forma:
# Para p = 1, tem-se uma forte correlação positiva entre as variáveis: valores de y aumenta quando x aumenta;
# Para p = 0, As variáveis não depêndem linearmente uma da outra: não há associação entre as variáveis y e x;
# Para p = -1, tem-se uma forte correlação negativa entre as variáveis: valores de y diminui quando x aumenta.

install.packages("ggpubr")
library("ggpubr")

str(carros)
?ggscatter
ggscatter(carros, x = "kilometragem", y = "preco",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Distância Percorrida (Km)", ylab = "Preço do Carro")

cov(carros$kilometragem, carros$preco)
cor(carros$kilometragem, carros$preco)

# Coeficiente da Correlação p = -0.8061494
# Tem-se uma forte correlação negativa entre as variáveis: valores de y diminui quando x aumenta.
# Para este exemplo se traduz que a medida que aumenta a kilometragem diminui o preço do carro. 

## IMPORTANTE ##
# A correlação não implica causalidade!
# Não podemos afirmar que a distância percorrida (Km) é o responsável direto pela queda do preço do carro.
# Existe uma correlação e não uma causalidade.


# Scatter plot usando a função plot()

# Scatterplot Preço x Km
# Usando o preço como variável dependente (y)
plot(x = carros$kilometragem, y = carros$preco,
     main = "Scatterplot - Preço x Km",
     xlab = "Kilometragem",
     ylab = "Preço (R$)")



############################################################
# Análise Exploratória de Dados Para Variáveis Categóricas #
############################################################

# Criando tabelas de contingência - representam uma única variável categórica
# Lista as categorias das variáveis nominais
?table
str(carros)
table(carros$cor) # Tabulação - calcula as ocorrência para cada valor categórico
table(carros$modelo)

# Calculando a proporção de cada categoria
model_table <- table(carros$modelo)
prop.table(model_table)

cor_table <- table(carros$cor)
prop.table(cor_table)

# Gerando valor percentual com uma casa decimal
model_table <- table(carros$modelo)
model_table <- prop.table(model_table) * 100
round(model_table, digits = 1)

model_cor <- table(carros$cor)
model_cor <- prop.table(model_cor) * 100
round(model_cor, digits = 1)

model_transmissao <- table(carros$transmissao)
model_transmissao <- prop.table(model_transmissao) * 100
round(model_transmissao, digits = 1)

# Criando uma nova variável indicando cores conservadoras (que as pessoas compram com mais frequência)
head(carros)
carros$conserv <- carros$cor %in% c("Preto", "Cinza", "Prata", "Branco")
head(carros)

# Checando a variável
table(carros$conserv)
prop.table(table(carros$conserv)) * 100


# Verificando o relacionamento entre 2 variáveis categóricas
# Criando uma CrossTable
# Tabelas de contingência fornecem uma maneira de exibir as frequências e 
# frequências relativas de observações (lembra do capítulo de Estatística?),
# que são classificados de acordo com duas variáveis categóricas. Os elementos
# de uma categoria são exibidos através das colunas; os elementos de outra
# categoria são exibidos sobre as linhas.
install.packages("gmodels")
library(gmodels)
?CrossTable
CrossTable(x = carros$modelo, y = carros$conserv)


## Teste do Qui-quadrado

# Qui-quadrado, simbolizado por X^2 é um teste de hipóteses que se destina a 
# encontrar um valor da dispersão para duas variáveis nominais, avaliando a
# associação existente entre variáveis qualitativas.
# É um teste não paramétrico, ou seja, não depende dos parâmetros populacionais,
# como média e variância.

# O princípio básico deste método é comparar proporções, isto é, as possíveis
# divergências entre as frequências observadas e esperadas para um certo evento.
# Evidentemente, pode-se dizer que dois grupos se comportam de forma semelhante
# se as diferenças entre as frequências observadas e esperadas em cada categoria
# forem muito pequenas, próximas a zero. 

# Ou seja, se a probabilidade é muito baixa, ele fornece fortes evidências de
# que as duas variáveis estão associadas.

CrossTable(x = carros$modelo, y = carros$conserv, chisq = TRUE)
chisq.test(x = carros$modelo, y = carros$conserv)

# Trabalhamos com duas hipóteses:

# Hipótese nula (H0): As frequências observadas não são diferentes das frequências
# esperadas. Não existe diferença entre as frequências (contagens) dos grupos.
# Portanto, não há associação entre os grupos;

# Hipótese alternativa (Ha): As frequências observadas são diferentes das frequências
# esperadas, com isso existe diferença entre as frequências.
# Portanto, há associação entre os grupos.

# Para o exempo em questão, o valor de Chi-Quadrado = 0.15
# e graus de liberdade (df) = 2
# Portanto, não há associação entre os grupos
# O valor alto do p-valor (0.92) confirma esta conclusão.

# O estudo das hipóteses não é usado apenas para resolver problemas de negócio, 
# mas também pode ser usado durante a análise exploratória para verificar a relação 
# entre duas ou mais variáveis qualitativas.
