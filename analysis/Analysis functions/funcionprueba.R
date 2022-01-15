dataprep<-function(data){
  data %>% 
    select(
      year,V001,V005,V022,S411H,S413,SREGION,SPROVIN,S108N,S229A1,S621,
      V012,V025,V155,V190,V102,V024
    ) %>% 
    
    mutate(
      V005 = V005/1000000,
      S411H = as.factor(ifelse(S411H == 1,"Si","No")),
      SREGION = as.factor(SREGION),
      TIPORESIDENCIA = ifelse(V102 == 1,"URBANO","RURAL"),
      DEPARTAMEN = factor(V024, levels = c(1:25), labels = c("AMAZONAS","ANCASH","APURIMAC","AREQUIPA","AYACUCHO",
                                                             "CAJAMARCA","CALLAO","CUSCO","HUANCAVELICA","HUANUCO",
                                                             "ICA","JUNIN","LA LIBERTAD","LAMBAYEQUE","LIMA",
                                                             "LORETO","MADRE DE DIOS","MOQUEGUA","PASCO","PIURA",
                                                             "PUNO","SAN MARTIN","TACNA","TUMBES","UCAYALI")),
    )
}
