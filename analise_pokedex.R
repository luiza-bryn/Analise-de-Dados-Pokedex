#INVOCAR PACOTES

library(data.table)
library(pastecs)
library(agricolae)
library(ggplot2)
library(psych)

#INVOCAR POKÉDEX

Pokedex <- fread(input = paste0("pokedex.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=".")

#Filtrar pokémons LENDÁRIOS e NÃO LENDÁRIOS
Pokedex_Comum <- subset(Pokedex, status == "Normal")
Pokedex_Lendarios <- subset(Pokedex, status != "Normal")

#Filtrar pokémons comuns por tipo (FANTASMA + METAL, FOGO, GRAMA, ÁGUA)
Pokedex_ghost_steel <- subset(Pokedex_Comum,tipo_primario == "Ghost" | tipo_primario =="Steel")
Pokedex_grass <- subset(Pokedex_Comum,tipo_primario == "Grass")
Pokedex_fire <- subset(Pokedex_Comum,tipo_primario == "Fire")
Pokedex_water <- subset(Pokedex_Comum,tipo_primario == "Water")

#Filtrar pokémons muito pesados

Pokedex_peso<-subset(Pokedex,peso_kg<180)


#Sumário dos pesos (quilogramas) de TODOS Pokémons menos os Pokémons ABSURDAMENTE pesados:

summary(Pokedex_peso$peso_kg)
quantile(Pokedex_peso$peso_kg)
hist(Pokedex_peso$peso_kg)

describeBy(Pokedex_peso$peso_kg,Pokedex_peso$geracao)
library(doBy)
summaryBy(peso_kg ~ geracao, data = Pokedex_peso,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )



md <- median(Pokedex_peso$peso_kg)
m <- mean(Pokedex_peso$peso_kg)
q3 <- 54.0
q2 <- 8.0
lines(density(Pokedex_peso$peso_kg))
abline(v=md, col="red", lwd=2)
abline(v=m, col="blue", lwd=2)
abline(v=q3, col="black", lwd=2)
abline(v=q2, col="black", lwd=2)


sturges.freq(Pokedex_peso$peso_kg,k=0)
classes<-with(Pokedex_peso,graph.freq(peso_kg,plot=FALSE))
print(table.freq(classes),row.names=FALSE)

#Sumário das alturas (metros) de TODOS Pokémons menos os Pokémons com altura maior que 4 metros:

summary(Pokedex_peso$altura_m)
quantile(Pokedex_peso$altura_m)

hist(Pokedex_peso$altura_m)

md <- median(Pokedex_peso$altura_m)
m <- mean(Pokedex_peso$altura_m)
q3 <- 1.4
q2 <- 0.5

lines(density(Pokedex_peso$altura_m))
abline(v=md, col="red", lwd=2)
abline(v=m, col="blue", lwd=2)
abline(v=q3, col="black", lwd=2)
abline(v=q2, col="black", lwd=2)

#Grafico ggplot altura VS peso

ggplot(Pokedex_Comum, aes(x=altura_m, y=peso_kg)) + 
  geom_point(size=2)

#fazer altura VS peso com apenas tipo FANTASMA e METAL (PRIMARIO)

ggplot(Pokedex_ghost_steel, aes(x=altura_m, y=peso_kg, color=tipo_primario)) + 
  geom_point(size=2)


#pontos de status somados (melhor pokemon para batalha) COMUM

summary(Pokedex$total_points)
hist(Pokedex$total_points)

md <- median(Pokedex$total_points)
m <- mean(Pokedex$total_points)
q3 <- 515.00
q2 <- 330.0
lines(density(Pokedex$total_points))
abline(v=md, col="red", lwd=2)
abline(v=m, col="blue", lwd=2)
abline(v=q3, col="black", lwd=2)
abline(v=q2, col="black", lwd=2)

boxplot(Pokedex$total_points,
        main = "Total de pontos de atributos dos Pokémons",
        xlab = "Total de pontos",
        ylab = "",
        col = "purple",
        horizontal = TRUE)
boxplot(Pokedex_Comum$total_points,
        main = "Total de pontos de atributos dos Pokémons comuns",
        xlab = "Total de pontos",
        ylab = "",
        col = "red",
        horizontal = TRUE)
boxplot(Pokedex_Lendarios$total_points,
        main = "Total de pontos de atributos dos Pokémons não comuns",
        xlab = "Total de pontos",
        ylab = "",
        col = "blue",
        horizontal = TRUE)

summary(Pokedex_Lendarios$total_points)

# tabelas tipos primarios e status

Quantidade_tipos_pok_comum <- table(Pokedex_Comum$tipo_primario, useNA = "ifany") 
Porcentagem_tipos_pok_comum <- round(prop.table(Quantidade_tipos_pok_comum)*100,1)
Quantidade_tipos_pok_comum 
Porcentagem_tipos_pok_comum

Quantidade_tipos_pok_n_comum  <- table(Pokedex_Lendarios$tipo_primario, useNA = "ifany")
Porcentagem_tipos_pok_n_comum <- round(prop.table(Quantidade_tipos_pok_n_comum)*100,1)
Quantidade_tipos_pok_n_comum 
Porcentagem_tipos_pok_n_comum

# graficos de barra das tabelas acima

tabela <- data.frame(table(Pokedex_Comum$status,Pokedex_Comum$tipo_primario))
colnames(tabela) <- c("status","tipo_primario","Freq")

ggplot(tabela, aes(fill=tipo_primario, y=Freq, x=status)) + 
  geom_bar(position="dodge", stat="identity")

tabela <- data.frame(table(Pokedex_Lendarios$status,Pokedex_Lendarios$tipo_primario))
colnames(tabela) <- c("status","tipo_primario","Freq")

ggplot(tabela, aes(fill=tipo_primario, y=Freq, x=status)) + 
  geom_bar(position="fill", stat="identity")

# grafico de barras [ taxa de crescimento e status ]

tabela <- data.frame(table(Pokedex$status,Pokedex$taxa_crescimento))
colnames(tabela) <- c("status","taxa_crescimento","Freq")

ggplot(tabela, aes(fill=taxa_crescimento, y=Freq, x=status)) + 
  geom_bar(position="fill", stat="identity")

#catch_rate -> Pokemons que não são comuns, são mais dificeis de pegar

mean(Pokedex_Comum$catch_rate)
median(Pokedex_Comum$catch_rate)

mean(Pokedex_Lendarios$catch_rate)
median(Pokedex_Lendarios$catch_rate)

