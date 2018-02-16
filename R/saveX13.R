#' @title Read data file for seasonal adjustment
#' @description Read a .csv or .xlsx file with time series content. The file must have 2 or more columns. The first one must contain the initial date of the time series. Missings are supported. 
#' @param output Output from seasX13 
#' @param file a character string naming a file
#' @importFrom zoo as.Date
 
saveX13 <- function(output, file = ""){
  
  if(file == ""){ stop("Insert the file name. No extensions are required.")}

  ajustadas <- data.frame(date = as.Date(output$xSA), output$xSA)
  fatores_totais <- data.frame(date = as.Date(output$totalFactors), output$totalFactors)
  fatores_sazonais <- data.frame(date = as.Date(output$seasonalFactors), output$seasonalFactors)
  fatores_calendario <- data.frame(date = as.Date(output$calendarFactors), output$calendarFactors)
  
  # salvar serie com ajuste sazonal
  write.csv2(ajustadas, file = paste0(file,"_seasonallyAdjusted.csv"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(fatores_totais, file = paste0(file,"_totalFactors.csv"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(fatores_calendario, file = paste0(file,"_calendarFactors.csv"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(fatores_sazonais, file = paste0(file,"_seasonalFactors.csv"), row.names = F, na = "", fileEncoding = "utf-8")
  write.csv2(output$espec, file = paste0(file,"_especifications.csv"), row.names = F, na = "", fileEncoding = "utf-8")
  
}