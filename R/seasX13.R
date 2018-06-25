#' @title Seasonal adjustment for data file 
#' @description Use X-13ARIMA-SEATS program by US Census Bureau to perform seasonal adjustment in time series. The function uses the \pkg{seasonal} package and applies its routine to a csv/xlsx file with multiple series simultaneously. The function also performs an automatic correction routine so that the results are properly diagnosed (residuals without autocorrelation and seasonality).  
#' @param x output from readX13 function 
#' @param autoCorrection a vector naming the time series should be auto corrected. See Details.
#' @param userCorrection a vector naming the time series should be corrected by user especifications. See Details.
#' @return A \code{list} containing the following elements:
#' \item{xSA}{seasonally adjusted time series}
#' \item{seasonalFactors}{seasonal factors for each series}
#' \item{calendarFactors}{calendar effects for each series}
#' \item{totalFactors}{seasonal plus calendar factors for each series}
#' \item{espec}{model especifications for each series}
#' \item{model}{output from seas function (package \pkg{seasonal})  for each series}
#' \item{read}{output items from readX13 function}
#' @details \code{autoCorrection} can assume \code{""} or \code{NULL}, or a vector naming the time series which should be corrected. If \code{autoCorrection = ""}, all time series will be corrected automatically (the final especification should be a model with no autocorrelated residuals, significant parameters at 5 percent and smaller BIC between a list of 48 possibles models). Default is \code{NULL} (no auto correction, automatic seasonal adjustment will be executed).
#' \code{userCorrection} modify the model especification for series chosen by the user. \code{autoCorrection} and \code{userCorrection} can not be executed at the same time. See Examples. 
#' @examples 
#' \dontrun{
#' ### Automatic seasonal adjustment and results
#' # load and read data example
#' data(serviceSurvey)
#' data <- readX13(serviceSurvey)
#' 
#' # auto seasonal adjustment
#' auto <- seasX13(data)
#' 
#' # some results: model especifications
#' auto$espec
#' 
#' # some results: SARIMA model (first series)
#' summary(auto$model$ICS)
#' 
#' # some results: plot (second series)
#' ts.plot(data$xts[,"IES"],auto$xSA[,"IES"], col = 1:2, lwd = 1:2)
#' legend("topright", legend = c("original", "seas. adjusted"), col = 1:2, lwd = 1:2, bty = "n")
#' 
#' ### correct automatic seasonal adjustment: autoCorrection
#' # all series (be patient, 48 models will be executed for each series)
#' correct1 <- seasX13(auto, autoCorrection = "")
#' 
#' # correct just one series
#' correct2 <- seasX13(auto, autoCorrection = c("ISAS"))
#' 
#' ### correct automatic seasonal adjustment: userCorrection
#' # edit the especification of output object from function seasX13 
#' auto$espec["IES","arima.model"] <- "(0 1 1)(0 1 1)"
#' auto$espec["IES","calendar.effects"] <- "td, carnival"
#' 
#' # run seasonal adjustment with userCorrection option
#' correct3 <- seasX13(auto, userCorrection = c("IES"))
#' correct3$espec
#' }
#' @importFrom zoo as.Date as.yearmon
#' @importFrom sfsmisc vcat
#' @importFrom stats Box.test end start ts window window<-
#' @import seasonal 
#' @import lubridate
#' @export

seasX13 <- function(x, autoCorrection = NULL, userCorrection = NULL){
  
  # Extrair nome e dados do objeto obj
  path <- x$path 
  if(is.null(x$xts)){
    xts <- x$read$xts
    xts2 <- x$read$xtsNA  # indicando onde começa e termina a série
    nomes_menosde3anos <- x$read$deniedNames
    nomes_maisde3anos <- x$read$acceptedNames
    datas <- as.data.frame(rownames(x$read$xts))  # datas 
  }else{
    xts <- x$xts
    xts2 <- x$xtsNA  # indicando onde começa e termina a série
    nomes_menosde3anos <- x$deniedNames
    nomes_maisde3anos <- x$acceptedNames
    datas <- as.data.frame(rownames(x$xts))  # datas 
  }  
  
  # Extrair nomes das séries que serão ajustadas.
  nomes <- colnames(xts) 
  
  # Verificar quantas séries existem em obj
  n <- dim(xts)[2]  
  
  # crias as especificações de saída
  esp <- data.frame(matrix("",ncol = 10, nrow = n), stringsAsFactors = F)
  colnames(esp) <- c("series","arima.model", "transform.function",
                     "regression.variables", "calendar.effects","outliers.estimated", "stability", 
                     "qs.original", "qs.original.corrected","qs.sa")
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
  x_as <-  ts(xts*NA, start = start(xts[,1]), frequency = 12)*NA # séries com ajuste sazonal
  x_s10 <- x_as*NA # fator sazonal
  fator1 <- ts(xts*NA, start = start(xts[,1]), end = as.yearmon(max(as.Date(xts[,1])) + months(12)), frequency = 12)
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
  
  # séries menores de três anos
  if(nomes_menosde3anos != ""){
    esp[nomes_menosde3anos,"transform.function"]   <- "none"
    
    # fatores 0 para o comprimento da série, NA para o resto
    
    for(i in 1:n){
      window(fator_s10[,i], start = inicio[i], end = fim_prev[i], frequency = 12) <- 0
      window(fator_td[,i], start = inicio[i], end = fim_prev[i], frequency = 12) <- 0
      window(fator_hol[,i], start = inicio[i], end = fim_prev[i], frequency = 12) <- 0
    }
    
    fator_calendario <- fator_hol + fator_td
    fator_total <- fator_s10 + fator_calendario
    colnames(fator_calendario) = colnames(fator_total) <- nomes
    
    x_as <- ts(xts, start = start(xts[,1]), frequency = 12) - fator_total
    colnames(x_as) <- nomes
    x_s10 <- window(fator_total, start = start(x_s10), end = end(x_s10), frequency = 12)
    
  }
  
  
  # executar ajuste sazonal --------------------
  
  if(!is.null(userCorrection)){    # ajuste definido pelo usário
    
    outX13 <- x$model
    for(i in userCorrection){
      outX13[[i]] <- ajuste_user(x = xts[,i], espec = x$espec[i,])
    }
    
  }else if(is.null(autoCorrection)){ # ajuste automático para cada série   
    
    #outX13 <- lapply(do.call(list, xts), FUN = function(x){ tryCatch(ajuste_automatico(x), error = function(e) x)})
    outX13 <- lapply(colnames(xts), FUN = function(x){
      message("performing automatic seasonal adjustment: ", x)
      suppressMessages({
        tryCatch(ajuste_automatico(xts[,x]), error = function(e) x)
      })
    })
    names(outX13) <- nomes
    
  }else{ # achar melhor ajuste para as séries
    
    outX13 <- tryCatch(x$model, error = function(e) x)
    testsModels <- NULL
    novosNomes <- NULL
    
    if(autoCorrection == "" & length(autoCorrection) == 1){
      novosNomes <- nomes
    }else{
      novosNomes <- autoCorrection[autoCorrection %in% nomes]
    }
    
    if(length(novosNomes) == 0) stop("autoCorrection names are incorrect!")

    message("be patient, 48 models will be executed for each series")
    message("------------------------------------------------------")
    models <- list()
    for(i in novosNomes){
      message("performing autoCorretion: ", i)
      suppressMessages({
        models[[i]] <- lapply(rownames(listModels), FUN = function(x) ajuste_correcao(x = xts[,i], model = x))
      })
      testsModels <- listModels
      testsModels$autocorrelation <- do.call(c, lapply(models[[i]], FUN = function(x) tryCatch(Box.test(x$series$rsd, type = "Ljung-Box", lag = 24)$p.value, error = function(e) 0)))
      testsModels$autocorrelation <- ifelse(testsModels$autocorrelation  < 0.05, "bad", "good")
      testsModels$qs.sa <- do.call(c, lapply(models[[i]], FUN = function(x) tryCatch(qs(x)[4,2], error = function(e) 0)))
      testsModels$parameters <- do.call(c, lapply(models[[i]], FUN = function(x) tryCatch(sum(summary(x)$coefficients[,"Pr(>|z|)"] > 0.05) == 0, error = function(e) T)))
      testsModels$parameters <- ifelse(testsModels$parameters == 0, "good", "bad")
      testsModels$bic <- do.call(c, lapply(models[[i]], FUN = function(x) tryCatch(summary(x)$bic, error = function(e) Inf)))
      
      melhores <-  testsModels[testsModels$parameters == "good" &  testsModels$autocorrelation == "good" & !is.na(testsModels$autocorrelation),]
      melhores <- tryCatch(melhores[order(melhores$bic),], error = function(e) melhores)
      
      if(nrow(melhores) == 0){
        outX13[[i]] <- tryCatch(x$model[[i]], error = function(e) NULL)
        if(is.null(outX13[[i]])){
           outX13[[i]] <- tryCatch(x$xts[,i], error = function(e) x$read$xts[,i])
        }
        message(paste("Attention! We couldn't find a good model for series", i))
      }else{
        best_model <- rownames(melhores[which(melhores$bic == min(melhores$bic)),])
        suppressMessages({
          outX13[[i]] <- ajuste_correcao(xts[,i], model = best_model)
        })
      }
      
    }
    names(outX13) <- nomes
  }
  
  # guardando o restante das especificações
  esp$series <- nomes                              
  esp$arima.model <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(x$model$arima$model, error = function(e) "")))
  esp$regression.variables <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(vcat(x$model$regression$variables, sep = ", "), error = function(e) "")))
  esp$calendar.effects <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(vcat(x$model$regression$user, sep = ", "), error = function(e) "")))
  esp$transform.function  <- do.call(c,lapply(outX13, FUN = function(x) tryCatch(as.character(summary(x)["transform.function"]), error = function(e) "")))
  esp$transform.function[is.na(esp$transform.function)] <- ""
  aux <- unlist(strsplit(esp$regression.variables, ", "))
  esp$outliers.estimated  <-  do.call(c,lapply(outX13, FUN = function(x) tryCatch(vcat((as.numeric(x$est$reg[,"estimate"])[casefold(x$est$reg$variable, upper = F) %in% casefold(aux, upper = F)]), sep = ", "), error = function(e) "")))
  qsX13 <- lapply(outX13, FUN = function(x) tryCatch(qs(x), error = function(e) ""))
  esp$qs.original <- do.call(c, lapply(qsX13, FUN = function(x) tryCatch(x[1,2], error = function(e) "")))
  esp$qs.original.corrected <- do.call(c, lapply(qsX13, FUN = function(x) tryCatch(x[2,2], error = function(e) "")))
  esp$qs.sa <- do.call(c, lapply(qsX13, FUN = function(x) tryCatch(x[4,2], error = function(e) "")))
  
  # guardar resultados
  
  extrair_st <- function(x, type = ""){
    m <- tryCatch(series(outX13[[x]], type), error = function(e) NULL)
    if(is.null(m)){
      if(esp[x,"transform.function"] == "none"){ 
        m <- ts(0, start = start(fator1), end = end(fator1), frequency = 12) 
      }else{ 
        m <- ts(1, start = start(fator1), end = end(fator1), frequency = 12)
      }
    }
    m
  }
  
  s10 <- lapply(names(outX13), FUN = function(x) tryCatch(outX13[[x]]$series$s10, error = function(e) NULL))
  names(s10) <- nomes
  s10ok <- lapply(names(outX13), FUN = function(x){
    if(is.null(s10[[x]])){
      if(esp[x,"transform.function"] == "none"){ 
        m <- ts(0, start = start(fator1), end = end(fator1), frequency = 12) 
      }else{ 
        m <- ts(1, start = start(fator1), end = end(fator1), frequency = 12)
      }
    }else{
      s10[[x]]
    }
  })
  names(s10ok) <- nomes
  fator_s10 <- window(do.call(cbind,s10ok), start = start(fator1), end = end(fator1), frequency = 12)
  
  model_000 <- tryCatch(nomes[esp$arima.model == "(0 0 0)"], error = function(e) NULL)
  if(!is.null(model_000)){
    for(kk in model_000){
      if(esp$transform.function[which(kk == nomes)] == "log"){
        fator_s10[,kk] <- 1  
      }else{
        fator_s10[,kk] <- 0  
      }
      
    }
  }

  td <- lapply(names(outX13), FUN = function(x) tryCatch(outX13[[x]]$series$usr, error = function(e) NULL))
  names(td) <- nomes
  tdok <- lapply(names(outX13), FUN = function(x){
    if(is.null(td[[x]])){
      if(esp[x,"transform.function"] == "none"){ 
        m <- ts(0, start = start(fator1), end = end(fator1), frequency = 12) 
      }else{ 
        m <- ts(1, start = start(fator1), end = end(fator1), frequency = 12)
      }
    }else{
      td[[x]]
    }
  })
  names(tdok) <- nomes
  fator_td <- window(do.call(cbind,tdok), start = start(fator1), end = end(fator1), frequency = 12)
  
  hol <- lapply(names(outX13), FUN = function(x) tryCatch(outX13[[x]]$series$hol, error = function(e) NULL))
  names(hol) <- nomes
  holok <- lapply(names(outX13), FUN = function(x){
    if(is.null(hol[[x]])){
      if(esp[x,"transform.function"] == "none"){ 
        m <- ts(0, start = start(fator1), end = end(fator1), frequency = 12) 
      }else{ 
        m <- ts(1, start = start(fator1), end = end(fator1), frequency = 12)
      }
    }else{
      hol[[x]]
    }
  })
  names(holok) <- nomes
  fator_hol <- window(do.call(cbind,holok), start = start(fator1), end = end(fator1), frequency = 12)
  
  for(i in nomes){
    if(esp[i,"transform.function"] == "none"){
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
  output$read$xts <- xts
  output$read$xtsNA <- xts2
  output$read$deniedNames <- nomes_menosde3anos
  output$read$acceptNames <- nomes_maisde3anos
  output$read$path <- path
  output
  
  
}
