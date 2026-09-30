# Matriculas: 554934, 546474, 578412

library("rstudioapi") 

rm(list = ls())
options(digits = 4)

# Carrega o dataset original
library("rstudioapi") 
k <- getSourceEditorContext()$path 
setwd(dirname(k)) 
dados_originais <- read.csv("HW1_bike_sharing.csv")

#folder_path <- "C:\Users\GUSTAVO\OneDrive\trabalhos\faculdade\estatistica\Estatistica\codigos\Homework_1"
#dados_originais <- read.csv(paste0(folder_path,"HW1_bike_sharing.csv"), row.names = 1)

# Questão 1
cat("\n")
cat("\nQuestão 1: Construção do datase\n")

# Números de matrícula do grupo
matriculas <- c(554934, 546474, 578412)

cat("\nInformações do Grupo\n")
cat("Números de matrícula: ", matriculas, "\n")

# Calcula M e r
M <- max(matriculas)
cat("Maior número de matrícula (M): ", M, "\n")

r <- 1 + (M %% 100)
cat("r = 1 + (M mod 100) = 1 + (", M, " mod 100) = ", r, "\n", sep = "")

# Seleciona 300 observações consecutivas
data_group <- dados_originais[r:(r+299), ] #[GGRM] + 300 estava selecionando no final 301, ajustei isso
#View(data_group)

cat("\n Amostra do Grupo \n")
cat("Dimensões de data_group: ", nrow(data_group), " x ", ncol(data_group), "\n", sep = "")
cat("Primeira data: ", as.character(data_group$dteday[1]), "\n", sep = "")
cat("Última data: ", as.character(data_group$dteday[nrow(data_group)]), "\n", sep = "")

cat("\nPrimeiras 10 observações de data_group:\n")
print(head(data_group, 10))


# PRIMEIRAS 10 OBSERVAÇÕES
cat("\n")
cat("As Primeiras 10 Observações\n")

dados_10 <- data_group[1:10, ]
dados_10$total_user <- dados_10$casual + dados_10$registered
total_user_10 <- dados_10$total_user

cat("\nValores de total_user (primeiras 10):\n")
print(total_user_10)

# MÉDIA
cat("\n Média \n")
soma <- sum(total_user_10)
n <- length(total_user_10)
media_manual <- soma / n
cat("Média = ", soma, " / ", n, " = ", media_manual, "\n", sep = "")

# MEDIANA
cat("\nMediana\n")
total_user_10_ordenado <- sort(total_user_10)
cat("Valores ordenados: ", total_user_10_ordenado, "\n", sep = " ")
mediana_manual <- (total_user_10_ordenado[5] + total_user_10_ordenado[6]) / 2
cat("Mediana = (", total_user_10_ordenado[5], " + ", total_user_10_ordenado[6], ") / 2 = ", mediana_manual, "\n", sep = "")

# DESVIO PADRÃO
cat("\nDesvio Padrão\n")
desvios_quadrados <- (total_user_10 - media_manual)^2
soma_dev_quad <- sum(desvios_quadrados)
variancia_manual <- soma_dev_quad / (n - 1)
desvio_padrao_manual <- sqrt(variancia_manual)
cat("Desvio Padrão = ", desvio_padrao_manual, "\n", sep = "")

# QUARTIS
cat("\nQuantis\n")
q1_pos <- (n + 1) * 0.25
q3_pos <- (n + 1) * 0.75
q1_idx_lower <- floor(q1_pos)
q1_idx_upper <- ceiling(q1_pos)
q1_frac <- q1_pos - floor(q1_pos)
q1_manual <- total_user_10_ordenado[q1_idx_lower] + q1_frac * (total_user_10_ordenado[q1_idx_upper] - total_user_10_ordenado[q1_idx_lower])

q3_idx_lower <- floor(q3_pos)
q3_idx_upper <- ceiling(q3_pos)
q3_frac <- q3_pos - floor(q3_pos)
q3_manual <- total_user_10_ordenado[q3_idx_lower] + q3_frac * (total_user_10_ordenado[q3_idx_upper] - total_user_10_ordenado[q3_idx_lower])

cat("Q1 = ", q1_manual, "\n", sep = "")
cat("Q2 = ", mediana_manual, "\n", sep = "")
cat("Q3 = ", q3_manual, "\n", sep = "")
cat("IQR = ", q3_manual - q1_manual, "\n", sep = "")

# VERIFICAÇÃO COM R
cat("\n")
cat("Verificação com R das primeiras 10 observações\n")

media_r <- mean(total_user_10)
mediana_r <- median(total_user_10)
desvio_r <- sd(total_user_10)

cat("\nMédia - Manual: ", media_manual, " | R: ", media_r, " | Diferença: ", abs(media_manual - media_r), "\n", sep = "")
cat("Mediana - Manual: ", mediana_manual, " | R: ", mediana_r, "\n", sep = "")
cat("Desvio Padrão - Manual: ", desvio_padrao_manual, " | R: ", desvio_r, "\n", sep = "")

# QUESTÃO 2
cat("\n")
cat("Questão 2: Caracterização do Dataset\n")

# Define total_user para todo o data_group
data_group$total_user <- data_group$casual + data_group$registered

# 2.1
cat("\n2.1 Classificação das variaveis\n")
cat("\nCategorias de SEASON:\n")
print(table(data_group$season))
cat("\nCategorias de WEATHERSIT:\n")
print(table(data_group$weathersit))
cat("\nValores ausentes:\n")
print(colSums(is.na(data_group)))

# 2.2
cat("\n\n2.2 Medidas de Tendência Central\n")
variaveis_numericas <- c("temp", "casual", "registered", "total_user")

for (var in variaveis_numericas) {
  cat("\n", var, ":\n", sep = "")
  media <- mean(data_group[[var]], na.rm = TRUE)
  mediana <- median(data_group[[var]], na.rm = TRUE)
  cat("  Média: ", round(media, 4), "\n", sep = "")
  cat("  Mediana: ", round(mediana, 4), "\n", sep = "")
  if (media > mediana) {
    cat("  Assimetria: Positiva\n")
  } else if (media < mediana) {
    cat("  Assimetria: Negativa\n")
  }
}

# 2.3
cat("\n\n2.3 Quartis e Intervalo Interquartil\n")
cat("\nVariavel analisada: total_user\n")

total_user <- data_group$total_user

q1 <- quantile(total_user, probs = 0.25)
q2 <- quantile(total_user, probs = 0.50)
q3 <- quantile(total_user, probs = 0.75)
iqr <- q3 - q1

cat("\nQ1 = ", q1, "\n", sep = "")
cat("Q2 = ", q2, "\n", sep = "")
cat("Q3 = ", q3, "\n", sep = "")
cat("IQR = ", iqr, "\n", sep = "")

lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

cat("\nLimites para outliers:\n")
cat("  Inferior: ", lim_inf, "\n", sep = "")
cat("  Superior: ", lim_sup, "\n", sep = "")

outliers <- which(total_user < lim_inf | total_user > lim_sup)
n_outliers <- length(outliers)

cat("\nNúmero de outliers: ", n_outliers, " (", round(n_outliers / nrow(data_group) * 100, 2), "%)\n", sep = "")

# 2.4 Gráficos
cat("\n\n2.4 Gráficos\n")

par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))

# Histograma
hist(total_user, 
     main = "Histograma: Total de Usuários",
     xlab = "Total de Usuários",
     ylab = "Frequência",
     col = "steelblue",
     breaks = 30)
abline(v = q1, col = "red", lty = 2, lwd = 2)
abline(v = q2, col = "green", lty = 2, lwd = 2)
abline(v = q3, col = "orange", lty = 2, lwd = 2)
legend("topleft", c("Q1", "Q2", "Q3"), col = c("red", "green", "orange"), lty = 2)

# Boxplot
boxplot(total_user, main = "Boxplot: Total de Usuários", ylab = "Total de Usuários", col = "steelblue")

par(mfrow = c(1, 1))

# Medidas de forma
skewness_val <- mean((total_user - mean(total_user))^3) / sd(total_user)^3
kurtosis_val <- mean((total_user - mean(total_user))^4) / sd(total_user)^4 - 3

cat("\nMedidas de forma:\n")
cat("  Assimetria: ", round(skewness_val, 4), "\n", sep = "")
cat("  Curtose: ", round(kurtosis_val, 4), "\n", sep = "")

# 2.5
cat("\n\n2.5 Variavel low_usage\n")

data_group$low_usage <- ifelse(data_group$total_user < q1, 1, 0)

n_low <- sum(data_group$low_usage)
prop_low <- mean(data_group$low_usage)

cat("\nQ1 = ", q1, "\n", sep = "")
cat("Dias com baixa utilização: ", n_low, "\n", sep = "")
cat("Proporção: ", round(prop_low * 100, 2), "%\n", sep = "")

cat("\nTabela de frequência:\n")
print(table(data_group$low_usage))

# SALVAR
write.csv(data_group, file = "data_group.csv", row.names = FALSE)
save(data_group, file = "data_group.RData")

# RESUMO
cat("\n")
cat("Resumo Final\n")

cat("\nGrupo:\n")
cat("  Matrículas: ", paste(matriculas, collapse = ", "), "\n", sep = "")
cat("  M = ", M, "\n", sep = "")
cat("  r = ", r, "\n", sep = "")
cat("  Período: ", as.character(data_group$dteday[1]), " a ", as.character(data_group$dteday[nrow(data_group)]), "\n", sep = "")
cat("  Observações: ", nrow(data_group), "\n", sep = "")

cat("\nEstatísticas de total_user:\n")
cat("  Média: ", round(mean(total_user), 2), "\n", sep = "")
cat("  Mediana: ", round(median(total_user), 2), "\n", sep = "")
cat("  Q1: ", round(q1, 2), "\n", sep = "")
cat("  Q3: ", round(q3, 2), "\n", sep = "")
cat("  IQR: ", round(iqr, 2), "\n", sep = "")
cat("  Desvio Padrão: ", round(sd(total_user), 2), "\n", sep = "")

cat("\nVariavel low_usage:\n")
cat("  Dias com baixa utilização: ", n_low, "\n", sep = "")
cat("  Proporção: ", round(prop_low * 100, 2), "%\n", sep = "")
