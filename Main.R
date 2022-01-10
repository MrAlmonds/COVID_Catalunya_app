
# Funciones Media de casos y acumulado
MediaCasos = function(df, n){
  df$mcasos = 0
  df$mexitus = 0
  
  df = df[order(df$DATA_f),]
  
  for(i in n:NROW(df)){
    df$mcasos[i] = mean(df$casos[(i-n):i])
    df$mexitus[i] = mean(df$exitus[(i-n):i])
  }  
  return(df)
}
#
AcuCasos = function(df, n){
  
  df = df[order(df$DATA_f),]
  df$cumcasos <- cumsum(df$casos)
  df$cumexitus <- cumsum(df$exitus)
  
  return(df)
}



# Abre los datos
data = read.csv2("Dades_di_ries_de_COVID-19_per_comarca.csv")
str(data)

# AGAs para el explorador de la App
agas = unique(data$NOM)
agas = agas[order(agas)]


## Transforma los datos:

# Edad
data$GRUP_EDAT[data$GRUP_EDAT %in% c("0 a 9","10 a 19", "20 a 29")] = "0-29 anys"
data$GRUP_EDAT[data$GRUP_EDAT %in% c("30 a 39", "40 a 49", "50 a 59", "60 a 69")] = "30-69 anys"
data$GRUP_EDAT[data$GRUP_EDAT %in% c("70 a 79","80 o més")] = "+70 anys"
data = data[!data$GRUP_EDAT %in% c("Tots", "N/A"),]

# Da formato a la fecha y ordena
data$DATA_f = as.Date(data$DATA, "%d/%m/%Y")
data = data[order(data$DATA_f),]


# Agrupa los datos y suma casos
df = data %>%
  group_by(GRUP_EDAT, DATA_f, NOM) %>%
  arrange(desc(GRUP_EDAT, DATA_f, NOM)) %>%
  summarise(casos = sum(CASOS_CONFIRMAT),
            exitus = sum(EXITUS))

df_t = data %>%
  group_by(GRUP_EDAT, DATA_f) %>%
  arrange(desc(GRUP_EDAT, DATA_f)) %>%
  summarise(casos = sum(CASOS_CONFIRMAT),
            exitus = sum(EXITUS))


####
# funcion principal para la app

FiltraDatos = function(metrica, aga, acum) {
  
  aga_sel = aga
  
  if (acum == F) {
    if (aga_sel != "Totes"){
      df_1 = MediaCasos(df[df$GRUP_EDAT == "0-29 anys" & df$NOM == aga,], 7)
      df_2 = MediaCasos(df[df$GRUP_EDAT == "30-69 anys" & df$NOM == aga,], 7)
      df_3 = MediaCasos(df[df$GRUP_EDAT == "+70 anys" & df$NOM == aga,], 7)
      
    
    } else{
      
      
      df_1 = MediaCasos(df_t[df_t$GRUP_EDAT == "0-29 anys",], 7)
      df_2 = MediaCasos(df_t[df_t$GRUP_EDAT == "30-69 anys",], 7)
      df_3 = MediaCasos(df_t[df_t$GRUP_EDAT == "+70 anys",], 7)
    }
    
    if (metrica == "Positius confirmats"){
      data_casos <- cbind(`De 0 a 29 anys` = df_1$mcasos,
                          `De 30 a 69 anys` = df_2$mcasos,
                          `Més de 70 anys` = df_3$mcasos)}
    if (metrica == "Exitus confirmats"){
      data_casos <- cbind(`De 0 a 29 anys` = df_1$exitus,
                          `De 30 a 69 anys` = df_2$exitus,
                          `Més de 70 anys` = df_3$exitus)}
    
  } else{
  
    if (aga_sel != "Totes"){

      df_1 = AcuCasos(df[df_t$GRUP_EDAT == "0-29 anys" & df$NOM == aga,], 7)
      df_2 = AcuCasos(df[df_t$GRUP_EDAT == "30-69 anys" & df$NOM == aga,], 7)
      df_3 = AcuCasos(df[df_t$GRUP_EDAT == "+70 anys" & df$NOM == aga,], 7)
    } else{
      df_1 = AcuCasos(df_t[df_t$GRUP_EDAT == "0-29 anys",], 7)
      df_2 = AcuCasos(df_t[df_t$GRUP_EDAT == "30-69 anys",], 7)
      df_3 = AcuCasos(df_t[df_t$GRUP_EDAT == "+70 anys",], 7)
    }
    
    if (metrica == "Positius confirmats"){
      data_casos <- cbind(`De 0 a 29 anys` = df_1$cumcasos,
                          `De 30 a 69 anys` = df_2$cumcasos,
                          `Més de 70 anys` = df_3$cumcasos)}
    if (metrica == "Exitus confirmats"){
      data_casos <- cbind(`De 0 a 29 anys` = df_1$cumexitus,
                          `De 30 a 69 anys` = df_2$cumexitus,
                          `Més de 70 anys` = df_3$cumexitus)}
  }
  
  # Formato xts para el gráfico
  rownames(data_casos) = as.character(df_1$DATA_f)
  xts <- as.xts(data_casos)
  
  return(xts)
}
