
library(dplyr)
library(ggplot2)

# Remover linhas com NAs na coluna Class
Projeto_cristalização <- Projeto_cristalização %>% filter(!is.na(Class))

# Contagem das classes de imunoglobulina (usando a coluna Class)
contagem <- Projeto_cristalização %>% 
  count(Class)

# Transformar em data frame
dados_df <- as.data.frame(contagem)

# Adicionar coluna com porcentagens
dados_df$Porcentagem <- round(100 * dados_df$n / sum(dados_df$n), 1)

# Adicionar coluna para rótulos de legenda
dados_df$Legenda <- paste(dados_df$Class, ": ", dados_df$n, " (", dados_df$Porcentagem, "%)", sep = "")

# Criar o gráfico de pizza
ggplot(dados_df, aes(x = "", y = n, fill = Legenda)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "B") +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = -2, size = 20)  # Centralizar o título e ajustar a posição vertical
  )






# Carregar as bibliotecas necessárias
library(dplyr)
library(ggplot2)

# Selecionar as últimas 11 colunas do data frame
total_col <- ncol(Projeto_cristalização)
col_indices <- (total_col-16):total_col

Projeto_cristalização <- Projeto_cristalização %>% select(all_of(col_indices))

# Remover linhas com valores NA nas colunas pH e pI
Projeto_cristalização <- Projeto_cristalização %>% filter(!is.na(pH), !is.na(pI))

# Converter as colunas pI e pH para numérico, se necessário
Projeto_cristalização$pI <- as.numeric(as.character(Projeto_cristalização$pI))
Projeto_cristalização$pH <- as.numeric(as.character(Projeto_cristalização$pH))

# Criar um gráfico de dispersão usando ggplot2, com pI no eixo x e pH no eixo y
ggplot(data = Projeto_cristalização, aes(x = pI, y = pH)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão de pI vs pH",
       x = "pI",
       y = "pH") +
  theme_minimal() +
  scale_x_continuous(limits = c(3, 10), breaks = seq(3, 10, by = 1)) +
  scale_y_continuous(limits = c(3, 10), breaks = seq(3, 10, by = 1))

# Ver o resultado do data frame filtrado
View(Projeto_cristalização)







# Crie um vetor com os valores fornecidos
valores <- c(7, 6.0, 7, 6.75, 6.25, 6.5, 7.9, 6.6, 8.0, 5.1, 6.5, 6.5, 7.5, 8.4, 7.5, 7.5, 7.5, 7.5, 6.5, 6.8, 7.0, 8.3, 5.5, 4.5, 7.5, 7.35, 6.4, 4.8, 4.8, 7.5, 6.5, 6.5, 6.5, 7.5, 6.5, 4.5, 6.5, 4.5, 4.5, 8.5, 9.5, 8.5, 9.5, 8.2, 6.7, 6.5, 9.0, 6.5, 8.0, 7.7, 7.0, 7.0, 8, 8.5, 8, 8, 8.5, 7, 7, 9, 6, 6, 6, 7, 3.8, 3.8, 3.8, 3.8, 7.5, 7.6, 7.6, 6.25, 6.25, 6.25, 6.3, 6.8, 6.8, 6.8)

# Definir intervalos
intervalos <- cut(valores, breaks = seq(3.5, 9.5, by = 1), include.lowest = TRUE)

# Contar a frequência de valores em cada intervalo
frequencias <- table(intervalos)

# Criar o gráfico de barras ajustando o limite do eixo y
barplot_heights <- barplot(frequencias, col = "blue", main = "Distribuição dos Valores de pH", 
                           xlab = "Intervalos de pH", ylab = "Frequência de valores de pH ", ylim = c(0, max(frequencias) + 3))

#  Adiciona os valores no topo de cada barra com ajuste na posição y
text(x = barplot_heights, y = frequencias - 0.3, labels = frequencias, pos = 3, cex = 0.8, col = "black")








# Carregar a biblioteca necessária
library(ggplot2)

# Dados fornecidos
tamanho <- c(
  "single chain antibody",
  "Full antibody", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment",
  "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment",
  "Fab fragment", "complex", "complex", "complex", "complex", "complex", "complex", "complex", "complex",
  "complex", "complex", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment",
  "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment",
  "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment", "Fab fragment",
  "Fab fragment", "Fab fragment", "Full antibody", "Full antibody", "Full antibody", "Full antibody",
  "Full antibody", "Full antibody", "Full antibody", "Fab fragment", "Fab fragment", "Fab fragment", 
  "Full antibody", "Full antibody", "Full antibody", "Full antibody"
)

# Contagem das classes de imunoglobulina
contagem <- table(tamanho)

# Transformar em data frame
dados_df <- as.data.frame(contagem)

# Adicionar coluna com porcentagens
dados_df$Porcentagem <- round(100 * dados_df$Freq / sum(dados_df$Freq), 1)

# Adicionar coluna para rótulos de legenda
dados_df$Legenda <- paste(dados_df$tamanho, ": ", dados_df$Freq, " (", dados_df$Porcentagem, "%)", sep = "")

# Criar o gráfico de pizza
ggplot(dados_df, aes(x = "", y = Freq, fill = Legenda)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "A") +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = -2, size = 20)  # Centralizar o título e ajustar a posição vertical
  )








# Definir os dados
compostos <- c("Sodium sulfate", "Sodium chloride", "Ammonium tartrate", "Ammonium citrate", "Sodium formate",
               "Sodium tartrate", "Lithium citrate", "Lithium sulfate", "1,3-Propanodiol", "Dioxane", "Ethylene glycol",
               "Lithium chloride", "Magnesium chloride", "Zinc sulfate",
               "Calcium chloride", "Isopropanol", "Trehalose", "PEG", "Ammonium sulfate")
quantidades <- c(1, 5, 1, 3, 5, 2, 2, 2, 2, 3, 1, 1, 3, 1, 1, 1, 1, 64, 17)

# Criar um data frame com os dados
dados <- data.frame(Composto = compostos, Quantidade = quantidades)

# Ordenar os dados pela quantidade
dados <- dados[order(dados$Quantidade), ]

# Criar o gráfico de barras
library(ggplot2)
ggplot(dados, aes(x = Composto, y = Quantidade)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Quantidade), vjust = -0.5, size = 2, color = "black") +  # Adiciona os números acima das barras
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Precipitantes e Aditivos", y = "Quantidades de Precipitantes e aditivos", title = "")






