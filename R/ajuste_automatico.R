
ajuste_automatico <- function(x){
  m <- seas(na.omit(x), 
            estimate.maxiter = 2500,
            xreg = calendarEffects$x,
            regression.usertype = c("user","holiday","holiday"),
            regression.aictest = NULL,
            forecast.save = "fct",
            seats.appendfcst = "yes",
            regression.save = c("usr","hol"),
            slidingspans.outlier = "keep",
            slidingspans.save = "sfs",
            spectrum.save = c("sp0", "s1s"),
            spectrum.savelog = "all")
  
  
  erro_summary <- ifelse(is.null(m), NULL, tryCatch(summary(m),error = function(e) NULL))
  
  if(is.null(erro_summary)){
    m <- seas(na.omit(x), 
              estimate.maxiter = 2500,
              xreg = calendarEffects$x,
              regression.usertype = c("user","holiday","holiday"),
              regression.aictest = NULL,
              outlier = NULL,
              forecast.save = "fct",
              seats.appendfcst = "yes",
              regression.save = c("usr","hol"),
              slidingspans.outlier = "keep",
              slidingspans.save = "sfs",
              spectrum.save = c("sp0", "s1s"),
              spectrum.savelog = "all",
              transform.function = "none")
    
    erro_summary <- ifelse(is.null(m), NULL, tryCatch(summary(m),error = function(e) NULL))
    
    if(is.null(erro_summary)){
      m <- seas(na.omit(x), 
                estimate.maxiter = 2500,
                xreg = calendarEffects$x,
                regression.usertype = c("user","holiday","holiday"),
                regression.aictest = NULL,
                outlier = NULL,
                arima.model = "(0 1 1)(0 1 1)",
                forecast.save = "fct",
                seats.appendfcst = "yes",
                regression.save = c("usr","hol"),
                slidingspans.outlier = "keep",
                slidingspans.save = "sfs",
                spectrum.save = c("sp0", "s1s"),
                spectrum.savelog = "all",
                transform.function = "none")
    }
  }
  
  if(is.null(m)){ stop("seasonal adjust object is null")
  }else{
    # Verificar se as variáveis do calendário são significativas
    ultima_variavel <- NULL 
    k <- summary(m)
    
    aux <- calendarEffects$desc  
    rownames(aux) <- calendarEffects$desc$names_x13
    aux$pvalue <- NA  
    
    # identificar o p-valor de cada variável
    aux[aux$names_x13,"pvalue"] <- k$coefficients[,"Pr(>|z|)"][aux$names_x13]
    
    aux$fica <- aux$pvalue < 0.05 
    
    var.novos <- NULL 
    tipos.novos <- NULL 
    
    # fazer loop para continuar verificando se tem variáveis não significativas
    while(sum(aux$fica == FALSE) > 0 & nrow(aux) != 0){
      
      aux <- subset(aux, aux$fica != FALSE)
      if(nrow(aux) == 0){
        var.novos <- NULL
        tipos.novos <- NULL
      }else{
        var.novos <- calendarEffects$x[,as.character(aux$names)]
        tipos.novos <- as.character(aux$types)
        # if(!(is.null(tipos.novos))){
        #   tipos.novos <- "holiday"
        # }else{
        #   tipos.novos <- NULL
        # }
      }
      
      m <- seas(na.omit(x), 
                estimate.maxiter = 2500,
                xreg = var.novos,
                regression.usertype = tipos.novos,
                regression.aictest = NULL,
                forecast.save = "fct",
                seats.appendfcst = "yes",
                regression.save = c("usr","hol"),
                slidingspans.outlier = "keep",
                slidingspans.save = "sfs",
                spectrum.save = c("sp0", "s1s"),
                spectrum.savelog = "all")
      
      
      erro_summary <- ifelse(is.null(m), NULL, tryCatch(summary(m),error = function(e) NULL))
      
      if(is.null(erro_summary)){
        m <- seas(na.omit(x), 
                  estimate.maxiter = 2500,
                  xreg = var.novos,
                  regression.usertype = tipos.novos,
                  regression.aictest = NULL,
                  outlier = NULL,
                  forecast.save = "fct",
                  seats.appendfcst = "yes",
                  regression.save = c("usr","hol"),
                  slidingspans.outlier = "keep",
                  slidingspans.save = "sfs",
                  spectrum.save = c("sp0", "s1s"),
                  spectrum.savelog = "all",
                  transform.function = "none")
        
        erro_summary <- ifelse(is.null(m), NULL, tryCatch(summary(m),error = function(e) NULL))
        
        if(is.null(erro_summary)){
          m <- seas(na.omit(x), 
                    estimate.maxiter = 2500,
                    xreg = var.novos,
                    regression.usertype = tipos.novos,
                    regression.aictest = NULL,
                    outlier = NULL,
                    arima.model = "(0 1 1)(0 1 1)",
                    forecast.save = "fct",
                    seats.appendfcst = "yes",
                    regression.save = c("usr","hol"),
                    slidingspans.outlier = "keep",
                    slidingspans.save = "sfs",
                    spectrum.save = c("sp0", "s1s"),
                    spectrum.savelog = "all",
                    transform.function = "none")
        }
      }
      
      if(!(is.null(m))){
        
        k <- summary(m)
        
        if(nrow(aux) > 1){
          aux$names_x13 <- paste0("xreg",1:length(aux$names_x13))
          rownames(aux) <- aux$names_x13
          # substituir os novos p-valores após o novo ajuste
          aux[aux$names_x13,"pvalue"] <- k$coefficients[,"Pr(>|z|)"][aux$names_x13]
          # testar ap?s o novo ajuste se as novas variáveis são significativas
          aux$fica <- aux$pvalue < 0.05
          # atualizar as variáveis e seus nomes
          var.novos <- calendarEffects$x[,as.character(aux$names[aux[,"fica"] == T])]
          tipos.novos <- as.character(aux$types[aux[,"fica"] == T])
          ultima_variavel <- as.character(aux$names[aux[,"fica"] == T])
          # se nenhuma variável é significativa, então rodar sem nenhuma variável
          if(!is.null(dim(var.novos))){
            if(dim(var.novos)[2] == 0){
              var.novos <- NULL
              tipos.novos <- NULL
            }
          }
          
        }else if(dim(aux)[1] == 1){
          # substituir os novos p-valores após o novo ajuste
          aux[,"pvalue"] <- k$coefficients[,"Pr(>|z|)"]["var.novos"]
          if(is.na(aux$pvalue)){ aux[,"pvalue"] <- k$coefficients[,"Pr(>|z|)"] }
          # testar após o novo ajuste se as novas variáveis são significativas
          aux$fica <- aux$pvalue < 0.05
          # atualizar as variáveis e seus nomes
          var.novos <- calendarEffects$x[,as.character(aux$names[aux[,"fica"] == T])]
          tipos.novos <- as.character(aux$types[aux[,"fica"] == T])
          ultima_variavel <-  as.character(aux$names[aux[,"fica"] == T])
          # se nenhuma variável é significativa, então rodar sem nenhuma variável
          if(!is.null(dim(var.novos))){
            if(dim(var.novos)[2] == 0){
              var.novos <- NULL
              tipos.novos <- NULL
            }
          }
          
        }else if(dim(aux)[1] == 0){
          var.novos <- NULL
          tipos.novos <- NULL
        }
      }  
    } # fim do while
    
  }
  
  if(!(is.null(m))){
    
    # renomear o nome das variáveis de calendário
    if(length(m$model$regression$user) == 1){
      names(m$est$coefficients)[names(m$est$coefficients) == "var.novos"] <- ultima_variavel
    }else if(length(m$model$regression$user) > 1){
      nomes_dentro <- m$model$regression$user
      names(m$est$coefficients)[names(m$est$coefficients) %in% nomes_dentro] <- as.character(aux$names[aux$names_x13 %in% nomes_dentro])
    }
    if(!is.null(ultima_variavel)){
      m$model$regression$user <- ultima_variavel
    }else if(length(m$model$regression$user) == 3){
      m$model$regression$user <- aux$names
    }  
    
  }
  
  
  # output
  m
  
}


