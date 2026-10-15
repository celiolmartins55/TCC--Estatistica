# Instalar e carregar os pacotes
install.packages("dplyr")
install.packages("geosphere")
library(dplyr)
library(geosphere)

# Carregar o data.frame que contem as cidades e a longitude dos times mandante e visitante
dados_cidade <- read.csv("dados_seriea_lonlat.csv")

# Criar um data.frame com a distância percorrida pelo time visitante para cada partida
viagens = dados_cidade %>%
  rowwise () %>% 
  mutate(Distancia_KM = distHaversine(c(Lon_Vis, Lat_Vis), c(Lon_Mand, Lat_Mand)) / 1000) %>%
  ungroup()


# Calcular a distância percorrida por cada time em cada ano
distancia_total_anual <- viagens %>%
  group_by(Ano, Visitante) %>%
  summarise(
    Total_Viagens_KM = sum(Distancia_KM, na.rm = TRUE),
    Media_Viagem_KM = mean(Distancia_KM, na.rm = TRUE),
    .groups = "drop",
    Jogos_Fora = n(),
  ) %>%
  rename(Time = Visitante) %>%
  arrange(Ano, desc(Total_Viagens_KM))

# Criar um arquivo .csv com esses dados
write.csv(distancia_total_anual,"distanciapercorrida.csv")
