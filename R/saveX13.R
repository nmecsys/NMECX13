#' @title Read data file for seasonal adjustment
#' @description Read a .csv or .xlsx file with time series content. The file must have 2 or more columns. The first one must contain the initial date of the time series. Missings are supported. 
#' @param output Output from seasX13 
#' @param file a character string naming a file
#' @importFrom zoo as.Date
 
saveX13 <- function(output, file = ""){
  
  if(file == ""){ stop("Inserir nome do arquivo")}

  ajustadas <- data.frame(date = as.Date(output$x_as), output$x_as)
  fatores_totais <- data.frame(date = as.Date(output$fatores_totais), output$fatores_totais)
  fatores_sazonais <- data.frame(date = as.Date(output$fatores_sazonais), output$fatores_sazonais)
  fatores_calendario <- data.frame(date = as.Date(output$fatores_calendario), output$fatores_calendario)
  
  # Criar caminho para gravar series temporais Com Ajuste Sazonal
  path.saida <- paste0("./saida/", titulo, "_cas.xlsx")
  
  # salvar serie com ajuste sazonal
  write.csv2(ajustadas, file = paste0(file,"_sa"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(fatores_totais, file = paste0(file,"_tf"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(fatores_calendario, file = paste0(file,"_cf"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(fatores_sazonais, file = paste0(file,"_sf"), row.names = F, na = "", fileEncoding = "utf-8")
 
  # salvar especificacoes do modelo para ajustes posteriores.
  
  nomes <- c("arima.model", "transform.function",
             "regression.variables", "calendar.effects","outliers.estimated", "stability", 
             "qs.original", "qs.original.corrected","qs.series.sa")
  masc <- output$esp[[1]] == ""
  output$esp[[1]][masc] <- " " 
  
  tabela <- data.frame(output$esp[[1]][nomes][1:9], stringsAsFactors = FALSE)
  
  if(n > 1)
  {    
    for(i in 2:length(output$esp))
    {
      masc <- output$esp[[i]] == ""
      output$esp[[i]][masc] <- " " 
      tabela <- rbind(tabela, output$esp[[i]][1:9])
    }
  }
  tabela <- cbind(serie = colnames(output$xts), tabela)
  
  # salvar fatores calendario
  write.csv2(tabela, file = paste0(file,"_espec"), row.names = F, na = "", fileEncoding = "utf-8")
  
}