#' @title Sesonal adjustment for data file 
#' @description Calls the automatic procedures of X-13ARIMA-SEATS to perform a sesonal adjustment with time series. 
#' @param x output from readX13 function 
#' @param ... arguments to be passed to seas function from seasonal package
#' @importFrom zoo as.Date as.yearmon 
#' @importFrom seasonal seas 
#' @importFrom sfsmisc vcat

seasX13 <- function(x,...){
  
  # Extrair nome e dados do objeto obj
  path <- x$path 
  xts <- x$xts
  xts2 <- x$xtsNA  # indicando onde começa e termina a série
  nomes_menosde3anos <-  x$deniedNames
  nomes_maisde3anos <-  x$acceptedNames
  datas <- as.data.frame(rownames(x$xts))  # datas 
  
  # Extrair nomes das séries que serão ajustadas.
  nomes <- colnames(xts) 
  
  # definindo quantos anos têm as séries
  ll <- apply(xts2, MARGIN = 2, FUN = sum)
  #names36 <- names(ll[ll < 36])
  #namesresto <- names(ll[ll >= 49])
  #nomes_menos49meses <- names(ll[ll >= 36 & ll < 49])
  
  # Verificar quantas séries existem em obj
  n <- dim(xts)[2]  
  
  # crias as especificações de saída
  esp <- data.frame(matrix("",ncol = 9, nrow = n))
  colnames(esp) <- c("arima.model", "transform.function",
                     "regression.variables", "calendar.effects","outliers.estimated", "stability", 
                     "qs.original", "qs.original.corrected","qs.series.sa")
  rownames(esp) <- nomes
  
  # posições início e fim de cada série
  posicao_inicial <- apply(xts2 == 1, MARGIN = 2, FUN = function(x) min(which(x)))
  posicao_final <- apply(xts2 == 1, MARGIN = 2, FUN = function(x) max(which(x)))
  inicio <- as.yearmon(as.Date(datas[posicao_inicial,]))
  fim <- as.yearmon(as.Date(datas[posicao_final,]))
  names(inicio) = names(fim) <- nomes
  
  # posição inicial e final da previsão de cada série
  inicio_prev <- as.Date(datas[posicao_final,]) + months(1)
  fim_prev <- as.Date(datas[posicao_final,]) + months(12)
  names(inicio_prev) = names(fim_prev) <- nomes
  
  # criar data.frame para guardar as séries ajustadas.
  x_as <-  ts(xts*NA, start = start(xts[,1]), freq = 12)*NA # séries com ajuste sazonal
  x_s10 <- x_as*NA # fator sazonal
  fator1 <- ts(xts*NA, start = start(xts[,1]), end = as.yearmon(max(as.Date(xts[,1])) + months(12)), freq = 12)
  fator_s10 = fator_calendario = fator_td = fator_hol = fator_total <- fator1
  colnames(fator_s10) = colnames(fator_calendario) = colnames(fator_td) = colnames(fator_hol) = colnames(fator_total) <- nomes
  
  # fator_s10 <- data.frame(matrix(NA, nrow = length(fator1), ncol = length(nomes)))
  # fator_calendario <- data.frame(matrix(NA, nrow = length(fator1), ncol = length(nomes)))
  # fator_td <- data.frame(matrix(NA, nrow = length(fator1), ncol = length(nomes)))
  # fator_hol <- data.frame(matrix(NA, nrow = length(fator1), ncol = length(nomes)))
  # fator_total <- data.frame(matrix(NA, nrow = length(fator1), ncol = length(nomes)))
  # colnames(fator_s10) = colnames(fator_calendario) = colnames(fator_td) = colnames(fator_hol) = colnames(fator_total) <- nomes
  # 
  # criar lista para guardar objeto 'seas' de cada uma das séries ajustadas.
  outX13 <- list()
  erro_summary <- list()
  resul <- c() # guardar resultados de cada ajuste
  
  # séries menores de três anos
  if(nomes_menosde3anos != ""){
    esp[nomes_menosde3anos,"transform.function"]   <- "none"
    
    # fatores 0 para o comprimento da série, NA para o resto
    
    for(i in 1:n){
      window(fator_s10[,i], start = inicio[i], end = fim_prev[i], freq = 12) <- 0
      window(fator_td[,i], start = inicio[i], end = fim_prev[i], freq = 12) <- 0
      window(fator_hol[,i], start = inicio[i], end = fim_prev[i], freq = 12) <- 0
    }
    
    fator_calendario <- fator_hol + fator_td
    fator_total <- fator_s10 + fator_calendario
    colnames(fator_calendario) = colnames(fator_total) <- nomes
    
    x_as <- ts(xts, start = start(xts[,1]), freq = 12) - fator_total
    colnames(x_as) <- nomes
    x_s10 <- window(fator_total, start = start(x_s10), end = end(x_s10), freq = 12)
    
  }
  
  # ajuste automático para cada série
  outX13 <- lapply(do.call(list, xts), FUN = ajuste_automatico)
  
  # guardando o restante das especificações   
  esp$arima.model <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(x$model$arima$model, error = function(e) NULL)))
  esp$regression.variables <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(vcat(x$model$regression$variables, sep = ", "), error = function(e) "")))
  esp$calendar.effects <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(vcat(x$model$regression$user, sep = ", "), error = function(e) "")))
  esp$transform.function  <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(as.character(summary(x)["transform.function"]), error = function(e) "")))
  aux <-  strsplit(esp$regression.variables, ", ")
  esp$outliers.estimated  <-  do.call(c,lapply(outX13, FUN = function(x) tryCatch(vcat(as.numeric(x$est$reg[casefold(x$est$reg$variable, upper = F) %in% casefold(aux[[1]], upper = F),"estimate"], sep = ", ")), error = function(e) "")))
  qsX13 <- lapply(outX13, FUN = qs)
  esp$qs.original <- do.call(c, lapply(qsX13, FUN = function(x) tryCatch(x[1,2], error = function(e) "")))
  esp$qs.original.corrigida <- do.call(c, lapply(qsX13, FUN = function(x) tryCatch(x[2,2], error = function(e) "")))
  esp$qs.serie.cas <- do.call(c, lapply(qsX13, FUN = function(x) tryCatch(x[4,2], error = function(e) "")))
  
  # guardar resultados
  
  extrair_st <- function(x, type = ""){
    m <- series(outX13[[x]], type)
    if(is.null(m)){
      if(esp[x,"transform.function"] == "none"){ 
        m <- ts(0, start = start(fator1), end = end(fator1), freq = 12) 
      }else{ 
        m <- ts(1, start = start(fator1), end = end(fator1), freq = 12)
      }
    }
    m
  }
  
  fator_s10 <- window(do.call(cbind, lapply(names(outX13), FUN = extrair_st, type = "s10")), start = start(fator1), end = end(fator1), freq = 12)
  fator_td <-  window(do.call(cbind, lapply(names(outX13), FUN = extrair_st, type = "usr")), start = start(fator1), end = end(fator1), freq = 12)
  fator_hol <-  window(do.call(cbind, lapply(names(outX13), FUN = extrair_st, type = "hol")), start = start(fator1), end = end(fator1), freq = 12)
  colnames(fator_s10) = colnames(fator_td) = colnames(fator_hol) <- nomes
  
  for(i in nomes){
    if(esp[x,"transform.function"] == "none"){
      fator_calendario[,i] <- fator_hol[,i] + fator_td[,i]
      fator_calendario[as.Date(fator_calendario[,i]) > fim_prev[i],i] <- NA
      fator_s10[as.Date(fator_s10[,i]) > fim_prev[i],i] <- NA
      fator_total[,i] <- fator_s10[,i] + fator_calendario[,i]
      x_as[,i] <- xts[,i] - fator_total[,i]
    }else{
      fator_calendario[,i] <- fator_hol[,i] * fator_td[,i]
      fator_calendario[as.Date(fator_calendario[,i]) > fim_prev[i],i] <- NA
      fator_s10[as.Date(fator_s10[,i]) > fim_prev[i],i] <- NA
      fator_total[,i] <- fator_s10[,i] * fator_calendario[,i]
      x_as[,i] <- xts[,i] / fator_total[,i]
    }
  }
  
 
  # output
  output <- list()
  output$xSA <- x_as
  output$seasonalFactors <- fator_s10
  output$calendarFactors <- fator_calendario
  output$totalFactors <- fator_total
  output$espec <- esp
  output$model <- outX13
  output
 
  
}
