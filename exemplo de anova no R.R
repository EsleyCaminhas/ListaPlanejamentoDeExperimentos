
# Experimento com 4 tipos de ração e 5 amostras 

# Variável de interesse: ganho de peso em suínos


# Definindo os novos dados
tipo_racao <- factor(rep(c("A", "B", "C", "D"), each = 5))  # 4 tipos de ração com 5 amostras cada
ganho_peso <- c(2.3, 2.5, 2.6, 2.4, 2.5,  # Dados para A
                2.8, 3.0, 2.9, 3.1, 2.7,  # Dados para B
                1.7, 1.8, 1.9, 2.0, 1.8,  # Dados para C
                2.6, 2.7, 2.8, 2.9, 2.6)  # Dados para D

# Criando o dataframe
dados <- data.frame(tipo_racao, ganho_peso)

# Realizando a análise de ANOVA
anova_resultado <- aov(ganho_peso ~ tipo_racao, data = dados)

# Sumário da ANOVA
summary(anova_resultado)

# -----------------------------
# Teste de Normalidade dos Resíduos
# -----------------------------
# Calculando os resíduos da ANOVA
residuos <- residuals(anova_resultado)

# Teste de Shapiro-Wilk para normalidade dos resíduos
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)

# -----------------------------
# Teste de Homocedasticidade (Teste de Levene)
# -----------------------------
# Instalando e carregando o pacote car para o teste de Levene
if (!require(car)) install.packages("car", dependencies = TRUE)
library(car)

# Teste de Levene para verificar homogeneidade de variâncias
levene_test <- leveneTest(ganho_peso ~ tipo_racao, data = dados)
print(levene_test)

# -----------------------------
# Teste de Independência (Gráfico de Resíduos)
# -----------------------------
# Plotando os resíduos para verificar a independência (gráfico de dispersão)
plot(residuos, type = "p", main = "Gráfico de Resíduos", xlab = "Observações", ylab = "Resíduos")
abline(h = 0, col = "red")

# O gráfico ajuda a verificar a independência, pois, se os resíduos são independentes, eles não devem apresentar padrões visíveis.

# -----------------------------
# Teste de Autocorrelação dos Resíduos (para independência)
# -----------------------------
# Instalando e carregando o pacote lmtest para o teste de autocorrelação
if (!require(lmtest)) install.packages("lmtest", dependencies = TRUE)
library(lmtest)

# Teste de autocorrelação dos resíduos
dw_test <- dwtest(anova_resultado)
print(dw_test)
