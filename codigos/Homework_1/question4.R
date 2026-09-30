rm(list = ls()) #Cleaning environment
cat("\014")
options(digits = 4)

#instale os pacotes para rodar o código
#install.packages("fpp3")
#install.packages("rstudioapi")
library(fpp3)
library("rstudioapi") 


#importando os dados que serão usado
k <- getSourceEditorContext()$path 
setwd(dirname(k)) 
load(paste0("data_group.RData"))
data_g10 <- data_group[1:10, ]


cat("CHECando se os dados est?ão corretos\n")
cat("   > data_g10\n")
print(head(data_g10))
cat("\n > data_group\n")
print(tail(data_group))


cat("\n")
cat("Question 4: Análise bivariada\n", sep="")

# ==============================================================================
# 1. Construa a serie temporal de total_user. Identifique pereodos de maior e 
# menor utilização e discuta os principais padroes observados
# ==============================================================================

cat("4.1 - series temporais\n", sep="")

# transformando a coluna dteday de uma strig para um objeto que a função autoplot
# possa usar para criar a serie temporal.
# |> é o pipe, coloca a fun????o da esquerda como parametro da direita
# mutate cria a coluna data, com o tipo, date, 
#select deleta a colunadteday
# as_tribble transforma o dataset em uma serie temporal
data_tempo = data_group

data_tempo <- data_tempo |>
  mutate(day = as.Date(dteday)) |>
  select(-dteday) |>
  as_tsibble(index = day)

data_tempo

#plotando a serie temporal com a funçãpo autoplot, usando o dataset que é a nossa serie
#temporal e a variavel que queremos plotar, no caso, o total de usuários
# geomline controla o tamanho da linha da serie
# geom_smooth faz uma linha de tendendia para melhor visualização
autoplot(data_tempo, total_user)+
  geom_line(linewidth = 1.2, linetype = "solid", alpha = 0.8)+
  geom_smooth(method="loess", se = FALSE, linewidth = 1)+
  labs(y = "total_user",x = "dia", title = "Serie temporal de uso de bicicletas ao longo dos dias")

autoplot(data_tempo, season)+
  geom_line(linewidth = 1.2, linetype = "solid", alpha = 0.8)+
  labs(y = "season",x = "dia", title = "Serie temporal de temporada")

cat(" observando o prot da serie temporal, em janeiro, come??a um periodo de pouco uso.
Por??m esse uso vai aumentando gradativamente at?? o seu apice em junho, no qual, 
come??a a cair gradativamente até o final do ano.\n")

cat(" Isso provavelmente tem sua origem nas mudanças de temporadas, por que, como foi falado na quest??o 2
as temporadas determinam a temperatura, que tem uma correlação forte com a quantidade de usu??rios.
    A exemplo que, no verão e outono , onde a temperatura é maior, a serie atinge o seu apice,
    Já no inverno e na primavera, com temperaturas menores, ela atinge seu ponto mais baixo\n")



# ==============================================================================
# 2. Retome as duas características selecionadas na Quest??o 3. Para cada uma, escolha
# uma visualização e uma medida estat??stica adequadas para analisar sua associa????o com
# total_user. Apresente e interprete os resultados, comparando-os com as conclus??es
# obtidas na Questão
# ==============================================================================
cat("4.2 - analises das caracteristicas selecionadas\n", sep="")

cat("as caracteristicas escolhidas na questão 1 foram, temperatura como o fator que
    determina o formato normal do total_user, e as condições atimosfericas, como fator
    que limita os usu??rios caso chova")

#temperatura
cat("variavel 1 - temperatura\n", sep="")
cat("A proposta em questão é, a temperatura esta relacionada com os usuarios totais, 
    de uma forma não linear, mas que, na região das amostras, tende a ser linear. PPara
    comprovar isso, poderia por exemplo, plotamos as linhas de tendencia, como na figura 2.
    Em vermelho é a tendencia não linear que mais se adapta a nuvem de pontos, ja em verde
    est?? a tendencia linear, da nuvem de pontos. A regia??o em cinza ?? um intervalo de confian??a
    com aproximadamente 5% de erro.\n")


#pltotando o grafico de dispersão com ggplot
#geom_point diz que ?? um grafico de dispersão com os pontos de tamanho 2
#geom_smoth cria a linha de tendencia, lm para linear, e loess para se adaptar aos pontos
ggplot(data = data_group, mapping = aes(x=temp, y=total_user)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "green") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "total_user em função da temperatura e condição de chuva")


cat("as medidas que mais nos d??o informa????es uteis para confirmar essa hip??tese s??o
    a correla????o de pearson ja calculada na quest??o 3 e a de sperman.\n")
#a medida de pearman avalia a rela????o monot??nica entre duas vari??veis cont??nuas ou ordinais. 
#Em uma rela????o monot??nica, as vari??veis tendem a mudar juntas mas n??o necessariamente a uma 
#taxa constante.O coeficiente de correla????o de Spearman baseia-se nos valores classificados 
#de cada vari??vel, em vez de os dados brutos.
#Ou se o aumento ?? puramente linear, ambos os coeficientes ser??o 1.
#se uma variavel aumenta quando a outra aumenta, mas sem ser consistente, person ser?? positivo, mas 
#menor que 1, e sperman ser?? 1.
#por??m, se a rela????o for n??o linear de forma que em uma parte esteja subindo e em outra descendo, amboso tendem a ir para 0
sperman <- cor.test(x=data_group$temp, y=data_group$total_user, method = 'spearman', exact = FALSE)
pearson <- cor.test(x=data_group$temp, y=data_group$total_user, method = 'pearson', exact = FALSE)

cat("sperman: ", sperman$estimate, sep=" ")
cat("pearson: ", pearson$estimate, sep=" ")

cat("\nAqui, uma percep????o que podemos ter, com o coeficiente de Sperman sendo menor que o 
    de pearson ?? que a nuvem segue uma tendencia linear forte de aumento, porem, em algum ponto
    essa tendendoa muda, o que abaixa o coeficiente de Sperman, o que ?? muito percepitivel na visualiza????o do grafico.\n")


#wheaater condition
cat("variavel 2 - clima\n", sep="")

#figura 3
cat("com essa condi????o, estamos querendo confirmar a hipótese de que a chuva é um fator limitante
    na correlação ente a temperatura e a quantidade de usuários.\n")

cat("aqui, dois graficos podem nos trazer informações importantes. O primeiro,  figura 3, sendo
    uma variação do anterior, nos da a visualizção da  classe weathersit na disperção.
    Nesse grafico, é possivel observar que todas as amostras de classes com chuva, apresentam
    uma variação maior da tendendcia dos pontos")

#grafico de disper????o separado por classes
#mesmo do anterior, mas as.factor() diz nossas classes e 
#scale_color_manual, edita as cores e legendas delas
ggplot(data = data_group, mapping = aes(x=temp, y=total_user, colour = as.factor(weathersit))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "green") +
  geom_smooth(method = "loess", color = "red") +
  scale_color_manual(values = c("purple", "black", "orange", "red"), labels = c("clear", "cloudy", "light Rain", "Heavy Rain"))+
  labs(title = "total_user em função da temperatura e condição de chuva")

#figura 4

cat("essa premissa pode ser melhor observada no grafico 4, que resalta a diferença nas medias
    observadas entre cada faixa de temperatura, sendo, frio abaixo de 15, normal, entre 15 e 25, 
    e quente maior que 25. As medias são as medidas estatisticas usadas para compara????o
    entre as classes nas temperaturas correspondentes.\n")

#criando as categorias de temperatura r clima
data_group$temp_cls <- ifelse(data_group$temp < 15, 0, ifelse(data_group$temp < 25, 1,2))
data_group$temp_cls <- factor(data_group$temp_cls, 
                              levels = c(0, 1, 2), 
                              labels = c("frio", "normal", "quente"))
data_group$weather_name <- factor(data_group$weathersit, 
                                  levels = c(1, 2, 3, 4), 
                                  labels = c("Clear", "Cloudy", "Light Rain", "Heavy Rain"))
#obtendo as medias gerais
#mutate cria nossas colunas
#sumarise resume nossos dados
#groupy_by separa em classes, e grupo ?? a nossa coluna de grupos
media_geral <- mutate(
  summarise(
    group_by( data_group, temp_cls),
    media = mean(total_user, na.rm = TRUE)), 
  grupo = "Geral")

#obtendo as medias de cada classe de clima
media_wheater <-select(
  mutate(
    summarise(
      group_by(data_group, temp_cls, weather_name),
      media = mean(total_user, na.rm = TRUE),
      .groups = "drop"),
    grupo = as.character(weather_name)),
  temp_cls,
  media,
  grupo)

# juntando ambas
medias <- bind_rows(
  media_geral,
  media_wheater
)

#ordenando
medias = arrange(medias, temp_cls)
medias

#plotando o grafico de colunas
#fill=grupos separa nossos grupos
#com um processamento anterior, ver no apendice A
ggplot(data = medias, mapping = aes(x=temp_cls, y=media, fill=grupo)) +
  geom_col(position = position_dodge()) +
  labs(title = "total_user por classe de temperatura",
       x="temperatura por classes",
       y="medias total_user") +
  theme_bw()




# ==============================================================================
# 3. Aprofunde a análise da relação entre temperatura e total_user, diferenciando 
#visualmente os dias classificados como low_usage dos restantes. Discuta os principais
#padr??es observados e avalie se a relação entre temperatura e utilização parece diferir 
#entre os grupos
# ==============================================================================
cat("4.3 - analise pela classe Low_Usage\n", sep="")


#novamente, utilizando a função, ggplot para plotar um grafico de disperção separado por classes
ggplot(data = data_group, mapping = aes(x=temp, y=total_user, colour = as.factor(low_usage))) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", color = "red") +
  scale_color_manual(values = c("violet", "brown"), labels = c("Normal", "Low_uSAGE"))+
  labs(title = "total_user em funçãoo da temperatura e condição de chuva")

cat("como Low usage já é uma variavel derivada de total_user, as amostras que est??o abaixo
    no grafico acabam sendo de low_usage 1, já as de cima, low_usage 0. é notorio que, como
    a tendendia do grafico é ter mais usuarios conforme a temperatura, a concentração de low_usage
    est?? principalmente em temperaturas menores.")
