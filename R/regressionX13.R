#' @title SACE regression tool
#' @description The function estimates a regression to obtain the seasonal factors.
#' @param x output from readX13 function 
#' @param series a vector string naming the series you want to obtain the seasonal factors
#' @param espec a vector string that indicates the series transformation: log or none
#' @param file a character string naming the file
#' @importFrom stats end start ts aggregate median na.omit quantile sd decompose
#' @importFrom utils write.csv2
#' @export

regressionX13 <- function(x, series, espec = NULL, file = NULL){
  
  if(is.null(file)){ stop("Insert the file name. No extensions are required.") }
  # extraindo nomes e dados do obj 
  nomes <- series
  titulo <- x$path 
  xts <- x$xts
  xts2 <- x$xtsNA
  dados <- as.data.frame(rownames(x$xts))  # datas 
  #nomes_menos3anos <- series_menos3anos
  
  
  # posições (início e fim das séries)
  posicao_inicial <- data.frame(matrix(nrow=1,ncol=ncol(xts)))
  colnames(posicao_inicial) <- nomes 
  posicao_final <- data.frame(matrix(nrow=1,ncol=ncol(xts)))
  colnames(posicao_final) <- nomes 
  
  for(nome in nomes){
    posicao_inicial[,nome] <- min(which(xts2[,nome]==1))
    posicao_final[,nome]   <- max(which(xts2[,nome]==1))
  }
  
  # datas (início e fim das séries)
  inicio <- data.frame(matrix(nrow=2,ncol=length(nomes)))
  colnames(inicio) <- nomes 
  fim <- data.frame(matrix(nrow=2,ncol=length(nomes))) 
  colnames(fim) <- nomes 
  
  for(nome in nomes){
    inicio[,nome] <- as.numeric(c(substr(dados[posicao_inicial[,nome],1],1,4),substr(dados[posicao_inicial[,nome],1],6,7)))
    fim[,nome] <- as.numeric(c(substr(dados[posicao_final[,nome],1],1,4),substr(dados[posicao_final[,nome],1],6,7)))
  }
  
  # previsão de 12 meses
  prev <- list()
  for(no in nomes){
    if(fim[2,nome] == 12){
      prev[[no]] <- ts(xts[,no]*NA, start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no]+1, 12), frequency = 12)
    }else{
      prev[[no]] <- ts(xts[,no]*NA, start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no]+1, fim[2,no]), frequency = 12)
    }
  }
  
  # data máxima
  todas_datas <- seq.Date(as.Date(dados[1,1]),as.Date(dados[nrow(dados),1]),by="month")
  data_maxima <- max(todas_datas)
  ano_maximo <- as.numeric(substr(data_maxima,1,4)) 
  mes_maximo <- as.numeric(substr(data_maxima,6,7)) 
  
  # data mínima
  todas_datas <- seq.Date(as.Date(dados[1,1]),as.Date(dados[nrow(dados),1]),by="month")
  data_minima <- min(todas_datas)
  ano_minimo <- as.numeric(substr(data_minima,1,4)) 
  mes_minimo <- as.numeric(substr(data_minima,6,7))
  
  # fator1 (data.frame para todos os possíveis resultados)
  for(nome in nomes){
    if(fim[2,nome] == 12){
      fator1 <- ts(xts[,nome]*NA, start = c(ano_minimo,mes_minimo), end = c(ano_maximo+1, 12), frequency = 12)
    }else{
      fator1 <- ts(xts[,nome]*NA, start = c(ano_minimo,mes_minimo), end = c(ano_maximo+1, mes_maximo), frequency = 12)
    }
  }
  
  # posições finais para o fator1 em cada série ajustada
  ff <- data.frame(matrix(nrow=2,ncol=ncol(xts)))
  ff1 <- data.frame(matrix(nrow=2,ncol=ncol(xts)))
  colnames(ff) <- nomes 
  colnames(ff1) <- nomes
  
  for (no in nomes){
    if(inicio[1,no] != start(fator1)[1]){
      if(inicio[2,no] == 1){
        ff[1,no] <- (inicio[1,no])-1 
        ff[2,no] <- 12 
      }else{
        ff[1,no] <- (inicio[1,no]) 
        ff[2,no] <- (inicio[2,no]) -1
      }  
      if(inicio[2,no] != start(fator1)[2]){
        ff[1,no] <- inicio[1,no]
        ff[2,no] <- inicio[2,no]-1
      }else{
        ff[1,no] <- inicio[1,no]
        ff[2,no] <- inicio[2,no]
      }
    }else{
      if(inicio[2,no] != start(fator1)[2]){
        ff[1,no] <- inicio[1,no]
        ff[2,no] <- inicio[2,no]-1
      }else{
        ff[1,no] <- inicio[1,no]
        ff[2,no] <- inicio[2,no]
      }
    }
  }
  
  
  for (no in nomes){ 
    if(fim[1,no] != end(fator1)[1]){
      if(fim[2,no] == 12){
        ff1[1,no] <- (fim[1,no])+1 
        ff1[2,no] <- 1
      }else{
        ff1[1,no] <- (fim[1,no])
        ff1[2,no] <- (fim[2,no])+1
      }
    }else{
      ff1[1,no] <- (fim[1,no])
      ff1[2,no] <- (fim[2,no])+1
    }
  }
  
  
  
  
  # Criar data.frame para guardar os fatores sazonais e as séries ajustadas
  fs <- x$xts*NA 
  colnames(fs) <- nomes
  
  fat_saz <- as.data.frame(matrix(NA,ncol=length(nomes),nrow=length(fator1)))
  colnames(fat_saz) <- nomes
  
  serie_cas <- x$xts*NA
  colnames(serie_cas) <- nomes
  
  valores_previsao <- list()
  
  a <- list()
  h <- list()
  
  # data frame das especificações com os nomes das series
  esp <- data.frame(espec)
  esp$nomes <- nomes
  
  
  for(no in nomes){
    
    # condições para preencher os fatores sazonais
    
    if(ff[1,no] == start(fator1)[1]){       # para o in?cio
      if(ff[2,no] == start(fator1)[2]){
        a[[no]] <- NULL  
      }else{
        a[[no]] <- c(ts(NA,start=start(fator1),end=c(ff[,no]),frequency=12))
      }
    }else{
      a[[no]] <- c(ts(NA,start=start(fator1),end=c(ff[,no]),frequency=12))
    }
    
    
    
    if(ff1[1,no] == end(fator1)[1]){     # para o fim
      if(ff1[2,no] == end(fator1)[2]){
        h[[no]] <- NULL
      }else{
        h[[no]] <- c(ts(NA,start=c(ff1[,no]),end=end(fator1),frequency=12))
      }
    }else{
      h[[no]] <- c(ts(NA,start=c(ff1[,no]),end=end(fator1),frequency=12))
    }
    
  }
  
  
  for(no in nomes){  
    if(is.null(espec)){
      
      # fatores sazonais de cada série
      fs[,no] <- decompose(xts[,no], type = "additive")$seasonal
      
      # valores para a previsão
      datas_prev <- seq.Date(as.Date(paste0((end(fs[,no])[1]-1),"-",end(fs[,no])[2],"-", "01")),by = "months", length.out = 13)
      valores_previsao[[no]] <- fs[seq(which(as.Date(fs[,no])==datas_prev[2]),which(as.Date(fs[,no])==datas_prev[13])),no] 
      
      fat_saz[,no] <- ts(c(a[[no]],window(fs[,no], start = c(inicio[1,no], inicio[2,no]), end=c(fim[1,no], fim[2,no]), frequency=12),valores_previsao[[no]],h[[no]]),start = start(fator1), end = end(fator1), frequency = 12)
      
      # série com ajuste sazonal 
      serie_cas[,no] <- xts[,no] - fs[,no]
      
    }else if(!(is.null(espec))){
      
      
      if(esp[which(esp[,"nomes"]==no),1] == "none"){
        
        # fatores sazonais de cada série
        fs[,no] <- decompose(xts[,no], type = "additive")$seasonal
        
        # valores para a previsão
        datas_prev <- seq.Date(as.Date(paste0((end(fs[,no])[1]-1),"-",end(fs[,no])[2],"-", "01")),by = "months", length.out = 13)
        valores_previsao[[no]] <- fs[seq(which(as.Date(fs[,no])==datas_prev[2]),which(as.Date(fs[,no])==datas_prev[13])),no] 
        
        fat_saz[,no] <- ts(c(a[[no]],window(fs[,no], start = c(inicio[1,no], inicio[2,no]), end=c(fim[1,no], fim[2,no]), frequency=12),valores_previsao[[no]],h[[no]]),start = start(fator1), end = end(fator1), frequency = 12)          
        
        
        # série com ajuste sazonal 
        serie_cas[,no] <- xts[,no] - fs[,no]
        
      }else if(esp[which(esp[,"nomes"]==no),1] == "log"){
        
        # fatores sazonais de cada série
        fs[,no] <- decompose(xts[,no], type = "multiplicative")$seasonal
        
        # valores para a previsão
        datas_prev <- seq.Date(as.Date(paste0((end(fs[,no])[1]-1),"-",end(fs[,no])[2],"-", "01")),by = "months", length.out = 13)
        valores_previsao[[no]] <- fs[seq(which(as.Date(fs[,no])==datas_prev[2]),which(as.Date(fs[,no])==datas_prev[13])),no] 
        
        fat_saz[,no] <- ts(c(a[[no]],window(fs[,no], start = c(inicio[1,no], inicio[2,no]), end=c(fim[1,no], fim[2,no]), frequency=12),valores_previsao[[no]],h[[no]]),start = start(fator1), end = end(fator1), frequency = 12)          
        
        # série com ajuste sazonal 
        serie_cas[,no] <- xts[,no]/fs[,no]
      }
    }  
    
    
    
    # condições dos NA's
    # na_inicio <- which(row.names(fat_saz) == (paste0(inicio[1,no],"-","0",inicio[2,no],"-","01")))
    # na_fim <- which(row.names(fat_saz) == (paste0(fim[1,no],"-","0",fim[2,no],"-","01")))
    # 
    # fatores sazonais do tamanho das séries originais 
    # fat_saz[,no] <- c(rep(NA,na_inicio-1),fat_saz[na_inicio:na_fim,no],rep(NA,length(xts[,no])-na_fim))
    
  } #fim do for
  
  
  if(end(fator1)[2] == 12){
    data_final <- paste0(end(fator1)[1],"-",end(fator1)[2],"-","01")
  }else{
    data_final <- paste0(end(fator1)[1],"-","0",end(fator1)[2],"-","01")
  }
  
  datas1 <- as.data.frame(zoo::as.yearmon(seq.Date(as.Date(dados[1,]),as.Date(data_final),by = "month")))
  
  datas2 <- as.data.frame(zoo::as.yearmon(seq.Date(as.Date(dados[1,]),as.Date(dados[nrow(dados),]),by = "month")))
  
  fat_saz$datas <- datas1 
  colnames(fat_saz$datas) <- "datas" 
  fat_saz <- data.frame(fat_saz[,"datas"],fat_saz[,nomes])
  colnames(fat_saz) <- c("datas",nomes)
  
  serie_cas$datas <- datas2
  colnames(serie_cas$datas) <- "datas" 
  serie_cas <- data.frame(serie_cas[,"datas"],serie_cas[,nomes]) 
  colnames(serie_cas) <- c("datas",nomes)
  
  
  
  ## Aqui só entrarão séries com mais de 5 anos, pois não é possível achar todas as janelas para séries pequenas! 
  
  
  # definindo quantos anos têm as séries
  tam <- matrix(NA,nrow = 1,ncol=ncol(xts2))
  for (j in 1:ncol(xts2)){
    tam[1,j] <- sum(xts2[,j])
  }    
  colnames(tam) <- nomes
  
  tamanho <- data.frame(as.data.frame(matrix(tam,nrow=length(tam),ncol=1)),nomes)
  
  nomes_grandes <- tamanho[which(tamanho[,1]>60),"nomes"]
  nomes_pequenos <- tamanho[which(tamanho[,1]<=60),"nomes"]
  
  tabela2_3anos <- list()
  tabela2_4anos <- list()
  tabela2_5anos <- list()
  
  
  
  for (i in 1:nrow(tamanho)){
    if(tamanho[i,1] < 60){
      message(paste("A série",nomes[i],"tem menos de 5 anos de observação! Não é possível definir as janelas."))
      
      for(no in nomes_pequenos){
        tabela2_3anos[[no]] <- NULL
        tabela2_4anos[[no]] <- NULL
        tabela2_5anos[[no]] <- NULL
      }
      
    }else{
      
      ############################################################################
      ######### Fatores estimados para as séries definidas com janelas: ##########
      ############################################################################
      
      ###### - 3 anos
      
      cont <- list()
      fim_3anos <- list()
      inicio_3anos <- list()
      in_fim_serie <- list()
      a <- list()
      s <- list()
      tt <- list()
      
      for(no in nomes_grandes){
        a[[no]] <- window(xts[,no],start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no],fim[2,no]),frequency = 12) #s?rie sem NA
        s[[no]] <- seq(0,(length(a[[no]])/12)-1,1) # quantidade de anos na série
        
        for(i in 1:(length(s[[no]])-2)){
          cont[[no]][[i]] <- c(s[[no]][[i]],s[[no]][[i+1]],s[[no]][[i+2]])  # "combinações" de 3 em 3 anos
        }
        tt[[no]] <- seq(0,length(cont[[no]])-1,1)  # quantidade de anos para as possíveis "combinações" 
        
        for(j in 1:length(tt[[no]])){
          fim_3anos[[no]][[j]] <- c(fim[1,no]-tt[[no]][[j]],fim[2,no])
          if(fim[2,no]==12){
            inicio_3anos[[no]][[j]] <- c((fim_3anos[[no]][[j]][1]-2),1)
          }else{
            inicio_3anos[[no]][[j]] <- c((fim_3anos[[no]][[j]][1]-3),(fim_3anos[[no]][[j]][2]-1))
          }
        }
        
        in_fim_serie[[no]] <- list(fim_3anos[[no]],inicio_3anos[[no]])  # lista com o início e fim de cada janela de 3 anos de cada série
        # onde: $no[[1]] são os finais de cada janela de sua respectiva série 
        # e $no[[2]] são os inícios de cada janela de sua respectiva série 
        # OBS: no é o nome de cada série
      } 
      
      j_3anos <- list() 
      fatores_3anos <- list()
      j_3anos_serie <- list()   # lista com os fatores saz. de cada janela de 3 anos de cada série
      
      
      for(no in nomes_grandes){ 
        for (i in 1:length(in_fim_serie[[no]][[1]])){
          if(is.null(espec) == TRUE ){
            
            # fatores sazonais de cada série
            j_3anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie[[no]][[2]][[i]][1], in_fim_serie[[no]][[2]][[i]][2]), end = c(in_fim_serie[[no]][[1]][[i]][1], in_fim_serie[[no]][[1]][[i]][2]), frequency = 12)   
            fatores_3anos[[no]][[i]] <- decompose(j_3anos[[no]][[i]],type = "additive")$seasonal
            
          }else if (is.null(espec) == FALSE){
            for(w in 1:length(nomes)){
              if(espec[w] == "none"){
                
                # fatores sazonais de cada série
                j_3anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie[[no]][[2]][[i]][1], in_fim_serie[[no]][[2]][[i]][2]), end = c(in_fim_serie[[no]][[1]][[i]][1], in_fim_serie[[no]][[1]][[i]][2]), frequency = 12)   
                fatores_3anos[[no]][[i]] <- decompose(j_3anos[[no]][[i]],type = "additive")$seasonal
                
              }else if(espec[w] == "log"){
                
                # fatores sazonais de cada série
                j_3anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie[[no]][[2]][[i]][1], in_fim_serie[[no]][[2]][[i]][2]), end = c(in_fim_serie[[no]][[1]][[i]][1], in_fim_serie[[no]][[1]][[i]][2]), frequency = 12)   
                fatores_3anos[[no]][[i]] <- decompose(j_3anos[[no]][[i]],type = "multiplicative")$seasonal
              }
            }  
          }
        }
        j_3anos_serie[[no]] <- fatores_3anos[[no]]   # lista com os fat.saz de cada janela de 3 anos de cada série
        # OBS: no é o nome de cada série
      }
      
      
      
      # arrumando os fatores por m?s de cada janela (de cada série) e montando a tabela de saída 
      
      fat_jan <- list()
      fat_fev <- list()
      fat_mar <- list()
      fat_abr <- list()
      fat_mai <- list()
      fat_jun <- list()
      fat_jul <- list()
      fat_ago <- list()
      fat_set <- list()
      fat_out <- list()
      fat_nov <- list()
      fat_dez <- list()
      
      fatores_jan <- list()
      fatores_fev <- list() 
      fatores_mar <- list()
      fatores_abr <- list()
      fatores_mai <- list()
      fatores_jun <- list()
      fatores_jul <- list()
      fatores_ago <- list()
      fatores_set <- list()
      fatores_out <- list()
      fatores_nov <- list()
      fatores_dez <- list()
      
      fatores_totais_3anos <- list()
      fat.3anos_tabela <- list() 
      
      for(no in nomes_grandes){
        for( k in 1:length(j_3anos_serie[[no]])){
          
          # data inicial de cada janela de cada série
          ii <- as.Date(paste(start(j_3anos_serie[[no]][[k]])[1],start(j_3anos_serie[[no]][[k]])[2],"01",sep = "/"))
          # data final de cada janela de cada série
          fi <- as.Date(paste(end(j_3anos_serie[[no]][[k]])[1],end(j_3anos_serie[[no]][[k]])[2],"01",sep = "/"))
          
          seq_datas <- seq.Date(ii,fi,by="month") # sequência de datas de cada janela de cada série
          serie_data <- data.frame(seq_datas,j_3anos_serie[[no]][[k]])
          colnames(serie_data) <- c("datas",no)
          
          # fatores sazonais de cada janela de cada série
          fat_jan[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="01"),no][1] 
          fat_fev[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="02"),no][1] 
          fat_mar[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="03"),no][1] 
          fat_abr[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="04"),no][1] 
          fat_mai[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="05"),no][1] 
          fat_jun[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="06"),no][1] 
          fat_jul[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="07"),no][1] 
          fat_ago[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="08"),no][1] 
          fat_set[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="09"),no][1] 
          fat_out[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="10"),no][1] 
          fat_nov[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="11"),no][1] 
          fat_dez[[no]][[k]] <- serie_data[which(substr(seq_datas,6,7)=="12"),no][1] 
          
          fatores_jan[[no]] <- data.frame(matrix(c(fat_jan[[no]],NA),nrow=(length(fat_jan[[no]])+1),ncol=1))
          fatores_fev[[no]] <- data.frame(matrix(c(fat_fev[[no]],NA),nrow=(length(fat_fev[[no]])+1),ncol=1)) 
          fatores_mar[[no]] <- data.frame(matrix(c(fat_mar[[no]],NA),nrow=(length(fat_mar[[no]])+1),ncol=1)) 
          fatores_abr[[no]] <- data.frame(matrix(c(fat_abr[[no]],NA),nrow=(length(fat_abr[[no]])+1),ncol=1))
          fatores_mai[[no]] <- data.frame(matrix(c(fat_mai[[no]],NA),nrow=(length(fat_mai[[no]])+1),ncol=1))
          fatores_jun[[no]] <- data.frame(matrix(c(fat_jun[[no]],NA),nrow=(length(fat_jun[[no]])+1),ncol=1))
          fatores_jul[[no]] <- data.frame(matrix(c(fat_jul[[no]],NA),nrow=(length(fat_jul[[no]])+1),ncol=1))
          fatores_ago[[no]] <- data.frame(matrix(c(fat_ago[[no]],NA),nrow=(length(fat_ago[[no]])+1),ncol=1))
          fatores_set[[no]] <- data.frame(matrix(c(fat_set[[no]],NA),nrow=(length(fat_set[[no]])+1),ncol=1))
          fatores_out[[no]] <- data.frame(matrix(c(fat_out[[no]],NA),nrow=(length(fat_out[[no]])+1),ncol=1))
          fatores_nov[[no]] <- data.frame(matrix(c(fat_nov[[no]],NA),nrow=(length(fat_nov[[no]])+1),ncol=1))
          fatores_dez[[no]] <- data.frame(matrix(c(fat_dez[[no]],NA),nrow=(length(fat_dez[[no]])+1),ncol=1))
        }
        fatores_totais_3anos[[no]] <- list(fat_jan[[no]],fat_fev[[no]],fat_mar[[no]],fat_abr[[no]],fat_mai[[no]],fat_jun[[no]],fat_jul[[no]],fat_ago[[no]],fat_set[[no]],fat_out[[no]],fat_nov[[no]],fat_dez[[no]])
        fat.3anos_tabela[[no]] <- data.frame(fatores_jan[[no]],fatores_fev[[no]],fatores_mar[[no]],fatores_abr[[no]],fatores_mai[[no]],fatores_jun[[no]],fatores_jul[[no]],fatores_ago[[no]],fatores_set[[no]],fatores_out[[no]],fatores_nov[[no]],fatores_dez[[no]])
        colnames(fat.3anos_tabela[[no]]) <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      }
      
      # tabela dos fatores sazonais de cada janela de 3 anos de cada série
      # OBS: as janelas começam a contar a partir do último dado de cada série
      
      tabela1_3anos <- do.call(rbind,fat.3anos_tabela)
      
      tabela1_3anos$Serie.Janela <- row.names(tabela1_3anos) 
      
      tabela2_3anos[[no]] <- data.frame( tabela1_3anos[,"Serie.Janela"], tabela1_3anos[,c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")])
      colnames(tabela2_3anos[[no]]) <- c("Serie.Janela","Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      
      for(y in 1:length(row.names(tabela2_3anos[[no]]))){
        if(is.na(tabela2_3anos[[no]][y,"Jan"])==TRUE){
          tabela2_3anos[[no]][y,"Serie.Janela"] <- ""
          options(warn=-1)
        }
      }  
      
      
      
      
      
      ###### - 4 anos
      
      cont2 <- list()
      fim_4anos <- list()
      inicio_4anos <- list()
      in_fim_serie2 <- list()
      a2 <- list()
      s2 <- list()
      tt2 <- list()
      
      
      
      for(no in nomes_grandes){
        a2[[no]] <- window(xts[,no],start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no],fim[2,no]),frequency = 12) #série sem NA
        s2[[no]] <- seq(0,(length(a2[[no]])/12)-1,1) # quantidade de anos na série
        
        for(i in 1:(length(s2[[no]])-3)){
          cont2[[no]][[i]] <- c(s2[[no]][[i]],s2[[no]][[i+1]],s2[[no]][[i+2]],s2[[no]][[i+3]])  # "combinações" de 4 em 4 anos
        }
        tt2[[no]] <- seq(0,length(cont2[[no]])-1,1)  # quantidade de anos para as possíveis "combinações" 
        
        for(j in 1:length(tt2[[no]])){
          fim_4anos[[no]][[j]] <- c(fim[1,no]-tt2[[no]][j],fim[2,no])
          if(fim[2,no]==12){
            inicio_4anos[[no]][[j]] <- c((fim_4anos[[no]][[j]][1]-3),1)
          }else{
            inicio_4anos[[no]][[j]] <- c((fim_4anos[[no]][[j]][1]-4),(fim_4anos[[no]][[j]][2]-1))
          }
        }
        
        in_fim_serie2[[no]] <- list(fim_4anos[[no]],inicio_4anos[[no]])  # lista com o início e fim de cada janela de 4 anos de cada série
        # onde: $no[[1]] são os finais de cada janela de sua respectiva série 
        # e $no[[2]] são os inícios de cada janela de sua respectiva série 
        # OBS: no é o nome de cada série
      } 
      
      j_4anos <- list() 
      fatores_4anos <- list()
      j_4anos_serie <- list()   # lista com os fatores saz. de cada janela de 4 anos de cada série
      
      
      for(no in nomes_grandes){ 
        for (i in 1:length(in_fim_serie2[[no]][[1]])){
          if(is.null(espec) == TRUE ){
            
            # fatores sazonais de cada série
            j_4anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie2[[no]][[2]][[i]][1], in_fim_serie2[[no]][[2]][[i]][2]), end = c(in_fim_serie2[[no]][[1]][[i]][1], in_fim_serie2[[no]][[1]][[i]][2]), frequency = 12)   
            fatores_4anos[[no]][[i]] <- decompose(j_4anos[[no]][[i]],type = "additive")$seasonal
            
          }else if (is.null(espec) == FALSE){
            for(w in 1:length(nomes)){
              if(espec[w] == "none"){
                
                # fatores sazonais de cada série
                j_4anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie2[[no]][[2]][[i]][1], in_fim_serie2[[no]][[2]][[i]][2]), end = c(in_fim_serie2[[no]][[1]][[i]][1], in_fim_serie2[[no]][[1]][[i]][2]), frequency = 12)   
                fatores_4anos[[no]][[i]] <- decompose(j_4anos[[no]][[i]],type = "additive")$seasonal
                
              }else if(espec[w] == "log"){
                
                # fatores sazonais de cada série
                j_4anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie2[[no]][[2]][[i]][1], in_fim_serie2[[no]][[2]][[i]][2]), end = c(in_fim_serie2[[no]][[1]][[i]][1], in_fim_serie2[[no]][[1]][[i]][2]), frequency = 12)   
                fatores_4anos[[no]][[i]] <- decompose(j_4anos[[no]][[i]],type = "multiplicative")$seasonal
              }
            }  
          }
        }
        j_4anos_serie[[no]] <- fatores_4anos[[no]]   # lista com os fat.saz de cada janela de 4 anos de cada série
        # OBS: no é o nome de cada série
      }
      
      
      
      # arrumando os fatores por mês de cada janela (de cada série) e montando a tabela de saída 
      
      fat_jan2 <- list()
      fat_fev2 <- list()
      fat_mar2 <- list()
      fat_abr2 <- list()
      fat_mai2 <- list()
      fat_jun2 <- list()
      fat_jul2 <- list()
      fat_ago2 <- list()
      fat_set2 <- list()
      fat_out2 <- list()
      fat_nov2 <- list()
      fat_dez2 <- list()
      
      fatores_jan2 <- list()
      fatores_fev2 <- list() 
      fatores_mar2 <- list()
      fatores_abr2 <- list()
      fatores_mai2 <- list()
      fatores_jun2 <- list()
      fatores_jul2 <- list()
      fatores_ago2 <- list()
      fatores_set2 <- list()
      fatores_out2 <- list()
      fatores_nov2 <- list()
      fatores_dez2 <- list()
      
      fatores_totais_4anos <- list()
      fat.4anos_tabela <- list() 
      
      for(no in nomes_grandes){
        for( k in 1:length(j_4anos_serie[[no]])){
          
          # data inicial de cada janela de cada série
          ii2 <- as.Date(paste(start(j_4anos_serie[[no]][[k]])[1],start(j_3anos_serie[[no]][[k]])[2],"01",sep = "/"))
          # data final de cada janela de cada série
          fi2 <- as.Date(paste(end(j_4anos_serie[[no]][[k]])[1],end(j_3anos_serie[[no]][[k]])[2],"01",sep = "/"))
          
          seq_datas2 <- seq.Date(ii2,fi2,by="month") # sequência de datas de cada janela de cada série
          serie_data2 <- data.frame(seq_datas2,j_4anos_serie[[no]][[k]])
          colnames(serie_data2) <- c("datas",no)
          
          # fatores sazonais de cada janela de cada série
          fat_jan2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="01"),no][1] 
          fat_fev2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="02"),no][1] 
          fat_mar2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="03"),no][1] 
          fat_abr2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="04"),no][1] 
          fat_mai2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="05"),no][1] 
          fat_jun2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="06"),no][1] 
          fat_jul2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="07"),no][1] 
          fat_ago2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="08"),no][1] 
          fat_set2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="09"),no][1] 
          fat_out2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="10"),no][1] 
          fat_nov2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="11"),no][1] 
          fat_dez2[[no]][[k]] <- serie_data2[which(substr(seq_datas2,6,7)=="12"),no][1] 
          
          fatores_jan2[[no]] <- data.frame(matrix(c(fat_jan2[[no]],NA),nrow=(length(fat_jan2[[no]])+1),ncol=1))
          fatores_fev2[[no]] <- data.frame(matrix(c(fat_fev2[[no]],NA),nrow=(length(fat_fev2[[no]])+1),ncol=1)) 
          fatores_mar2[[no]] <- data.frame(matrix(c(fat_mar2[[no]],NA),nrow=(length(fat_mar2[[no]])+1),ncol=1)) 
          fatores_abr2[[no]] <- data.frame(matrix(c(fat_abr2[[no]],NA),nrow=(length(fat_abr2[[no]])+1),ncol=1))
          fatores_mai2[[no]] <- data.frame(matrix(c(fat_mai2[[no]],NA),nrow=(length(fat_mai2[[no]])+1),ncol=1))
          fatores_jun2[[no]] <- data.frame(matrix(c(fat_jun2[[no]],NA),nrow=(length(fat_jun2[[no]])+1),ncol=1))
          fatores_jul2[[no]] <- data.frame(matrix(c(fat_jul2[[no]],NA),nrow=(length(fat_jul2[[no]])+1),ncol=1))
          fatores_ago2[[no]] <- data.frame(matrix(c(fat_ago2[[no]],NA),nrow=(length(fat_ago2[[no]])+1),ncol=1))
          fatores_set2[[no]] <- data.frame(matrix(c(fat_set2[[no]],NA),nrow=(length(fat_set2[[no]])+1),ncol=1))
          fatores_out2[[no]] <- data.frame(matrix(c(fat_out2[[no]],NA),nrow=(length(fat_out2[[no]])+1),ncol=1))
          fatores_nov2[[no]] <- data.frame(matrix(c(fat_nov2[[no]],NA),nrow=(length(fat_nov2[[no]])+1),ncol=1))
          fatores_dez2[[no]] <- data.frame(matrix(c(fat_dez2[[no]],NA),nrow=(length(fat_dez2[[no]])+1),ncol=1))
        }
        fatores_totais_4anos[[no]] <- list(fat_jan2[[no]],fat_fev2[[no]],fat_mar2[[no]],fat_abr2[[no]],fat_mai2[[no]],fat_jun2[[no]],fat_jul2[[no]],fat_ago2[[no]],fat_set2[[no]],fat_out2[[no]],fat_nov2[[no]],fat_dez2[[no]])
        fat.4anos_tabela[[no]] <- data.frame(fatores_jan2[[no]],fatores_fev2[[no]],fatores_mar2[[no]],fatores_abr2[[no]],fatores_mai2[[no]],fatores_jun2[[no]],fatores_jul2[[no]],fatores_ago2[[no]],fatores_set2[[no]],fatores_out2[[no]],fatores_nov2[[no]],fatores_dez2[[no]])
        colnames(fat.4anos_tabela[[no]]) <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      }
      
      # tabela dos fatores sazonais de cada janela de 3 anos de cada série
      # OBS: as janelas começam a contar a partir do último dado de cada série
      
      tabela1_4anos <- do.call(rbind,fat.4anos_tabela)
      
      tabela1_4anos$Serie.Janela <- row.names(tabela1_4anos) 
      
      tabela2_4anos[[no]] <- data.frame( tabela1_4anos[,"Serie.Janela"], tabela1_4anos[,c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")])
      colnames(tabela2_4anos[[no]]) <- c("Serie.Janela","Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      
      for(y in 1:length(row.names(tabela2_4anos[[no]]))){
        if(is.na(tabela2_4anos[[no]][y,"Jan"])==TRUE){
          tabela2_4anos[[no]][y,"Serie.Janela"] <- ""
          options(warn=-1)
        }
      }  
      
      
      
      
      ###### - 5 anos
      
      cont3 <- list()
      fim_5anos <- list()
      inicio_5anos <- list()
      in_fim_serie3 <- list()
      a3 <- list()
      s3 <- list()
      tt3 <- list()
      
      
      for(no in nomes_grandes){
        a3[[no]] <- window(xts[,no],start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no],fim[2,no]),frequency = 12) #série sem NA
        s3[[no]] <- seq(0,(length(a3[[no]])/12)-1,1) # quantidade de anos na série
        
        for(i in 1:(length(s3[[no]])-4)){
          cont3[[no]][[i]] <- c(s3[[no]][[i]],s3[[no]][[i+1]],s3[[no]][[i+2]],s3[[no]][[i+3]],s3[[no]][[i+4]])  # "combinações" de 4 em 4 anos
        }
        tt3[[no]] <- seq(0,length(cont3[[no]])-1,1)  # quantidade de anos para as possíveis "combinações" 
        
        for(j in 1:length(tt3[[no]])){
          fim_5anos[[no]][[j]] <- c(fim[1,no]-tt3[[no]][j],fim[2,no])
          if(fim[2,no]==12){
            inicio_5anos[[no]][[j]] <- c((fim_5anos[[no]][[j]][1]-4),1)
          }else{
            inicio_5anos[[no]][[j]] <- c((fim_5anos[[no]][[j]][1]-5),(fim_5anos[[no]][[j]][2]-1))
          }
        }
        
        in_fim_serie3[[no]] <- list(fim_5anos[[no]],inicio_5anos[[no]])  # lista com o início e fim de cada janela de 4 anos de cada s?rie
        # onde: $no[[1]] são os finais de cada janela de sua respectiva série 
        # e $no[[2]] são os inícios de cada janela de sua respectiva série 
        # OBS: no é o nome de cada série
      } 
      
      j_5anos <- list() 
      fatores_5anos <- list()
      j_5anos_serie <- list()   # lista com os fatores saz. de cada janela de 5 anos de cada série
      
      
      for(no in nomes_grandes){ 
        for (i in 1:length(in_fim_serie3[[no]][[1]])){
          if(is.null(espec) == TRUE ){
            
            # fatores sazonais de cada série
            j_5anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie3[[no]][[2]][[i]][1], in_fim_serie3[[no]][[2]][[i]][2]), end = c(in_fim_serie3[[no]][[1]][[i]][1], in_fim_serie3[[no]][[1]][[i]][2]), frequency = 12)   
            fatores_5anos[[no]][[i]] <- decompose(j_5anos[[no]][[i]],type = "additive")$seasonal
            
          }else if (is.null(espec) == FALSE){
            for(w in 1:length(nomes)){
              if(espec[w] == "none"){
                
                # fatores sazonais de cada série
                j_5anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie3[[no]][[2]][[i]][1], in_fim_serie3[[no]][[2]][[i]][2]), end = c(in_fim_serie3[[no]][[1]][[i]][1], in_fim_serie3[[no]][[1]][[i]][2]), frequency = 12)   
                fatores_5anos[[no]][[i]] <- decompose(j_5anos[[no]][[i]],type = "additive")$seasonal
                
              }else if(espec[w] == "log"){
                
                # fatores sazonais de cada série
                j_5anos[[no]][[i]] <- window(xts[,no],start = c(in_fim_serie3[[no]][[2]][[i]][1], in_fim_serie3[[no]][[2]][[i]][2]), end = c(in_fim_serie3[[no]][[1]][[i]][1], in_fim_serie3[[no]][[1]][[i]][2]), frequency = 12)   
                fatores_5anos[[no]][[i]] <- decompose(j_5anos[[no]][[i]],type = "multiplicative")$seasonal
              }
            }  
          }
        }
        j_5anos_serie[[no]] <- fatores_5anos[[no]]   # lista com os fat.saz de cada janela de 5 anos de cada série
        # OBS: no é o nome de cada série
      }
      
      
      
      # arrumando os fatores por mês de cada janela (de cada série) e montando a tabela de saída 
      
      fat_jan3 <- list()
      fat_fev3 <- list()
      fat_mar3 <- list()
      fat_abr3 <- list()
      fat_mai3 <- list()
      fat_jun3 <- list()
      fat_jul3 <- list()
      fat_ago3 <- list()
      fat_set3 <- list()
      fat_out3 <- list()
      fat_nov3 <- list()
      fat_dez3 <- list()
      
      fatores_jan3 <- list()
      fatores_fev3 <- list() 
      fatores_mar3 <- list()
      fatores_abr3 <- list()
      fatores_mai3 <- list()
      fatores_jun3 <- list()
      fatores_jul3 <- list()
      fatores_ago3 <- list()
      fatores_set3 <- list()
      fatores_out3 <- list()
      fatores_nov3 <- list()
      fatores_dez3 <- list()
      
      fatores_totais_5anos <- list()
      fat.5anos_tabela <- list() 
      
      for(no in nomes_grandes){
        for( k in 1:length(j_5anos_serie[[no]])){
          
          # data inicial de cada janela de cada série
          ii3 <- as.Date(paste(start(j_5anos_serie[[no]][[k]])[1],start(j_5anos_serie[[no]][[k]])[2],"01",sep = "/"))
          # data final de cada janela de cada série
          fi3 <- as.Date(paste(end(j_5anos_serie[[no]][[k]])[1],end(j_5anos_serie[[no]][[k]])[2],"01",sep = "/"))
          
          seq_datas3 <- seq.Date(ii3,fi3,by="month") # sequência de datas de cada janela de cada série
          serie_data3 <- data.frame(seq_datas3,j_5anos_serie[[no]][[k]])
          colnames(serie_data3) <- c("datas",no)
          
          # fatores sazonais de cada janela de cada série
          fat_jan3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="01"),no][1] 
          fat_fev3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="02"),no][1] 
          fat_mar3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="03"),no][1] 
          fat_abr3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="04"),no][1] 
          fat_mai3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="05"),no][1] 
          fat_jun3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="06"),no][1] 
          fat_jul3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="07"),no][1] 
          fat_ago3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="08"),no][1] 
          fat_set3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="09"),no][1] 
          fat_out3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="10"),no][1] 
          fat_nov3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="11"),no][1] 
          fat_dez3[[no]][[k]] <- serie_data3[which(substr(seq_datas3,6,7)=="12"),no][1] 
          
          fatores_jan3[[no]] <- data.frame(matrix(c(fat_jan3[[no]],NA),nrow=(length(fat_jan3[[no]])+1),ncol=1))
          fatores_fev3[[no]] <- data.frame(matrix(c(fat_fev3[[no]],NA),nrow=(length(fat_fev3[[no]])+1),ncol=1)) 
          fatores_mar3[[no]] <- data.frame(matrix(c(fat_mar3[[no]],NA),nrow=(length(fat_mar3[[no]])+1),ncol=1)) 
          fatores_abr3[[no]] <- data.frame(matrix(c(fat_abr3[[no]],NA),nrow=(length(fat_abr3[[no]])+1),ncol=1))
          fatores_mai3[[no]] <- data.frame(matrix(c(fat_mai3[[no]],NA),nrow=(length(fat_mai3[[no]])+1),ncol=1))
          fatores_jun3[[no]] <- data.frame(matrix(c(fat_jun3[[no]],NA),nrow=(length(fat_jun3[[no]])+1),ncol=1))
          fatores_jul3[[no]] <- data.frame(matrix(c(fat_jul3[[no]],NA),nrow=(length(fat_jul3[[no]])+1),ncol=1))
          fatores_ago3[[no]] <- data.frame(matrix(c(fat_ago3[[no]],NA),nrow=(length(fat_ago3[[no]])+1),ncol=1))
          fatores_set3[[no]] <- data.frame(matrix(c(fat_set3[[no]],NA),nrow=(length(fat_set3[[no]])+1),ncol=1))
          fatores_out3[[no]] <- data.frame(matrix(c(fat_out3[[no]],NA),nrow=(length(fat_out3[[no]])+1),ncol=1))
          fatores_nov3[[no]] <- data.frame(matrix(c(fat_nov3[[no]],NA),nrow=(length(fat_nov3[[no]])+1),ncol=1))
          fatores_dez3[[no]] <- data.frame(matrix(c(fat_dez3[[no]],NA),nrow=(length(fat_dez3[[no]])+1),ncol=1))
        }
        fatores_totais_5anos[[no]] <- list(fat_jan3[[no]],fat_fev3[[no]],fat_mar3[[no]],fat_abr3[[no]],fat_mai3[[no]],fat_jun3[[no]],fat_jul3[[no]],fat_ago3[[no]],fat_set3[[no]],fat_out3[[no]],fat_nov3[[no]],fat_dez3[[no]])
        fat.5anos_tabela[[no]] <- data.frame(fatores_jan3[[no]],fatores_fev3[[no]],fatores_mar3[[no]],fatores_abr3[[no]],fatores_mai3[[no]],fatores_jun3[[no]],fatores_jul3[[no]],fatores_ago3[[no]],fatores_set3[[no]],fatores_out3[[no]],fatores_nov3[[no]],fatores_dez3[[no]])
        colnames(fat.5anos_tabela[[no]]) <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      }
      
      # tabela dos fatores sazonais de cada janela de 5 anos de cada série
      # OBS: as janelas começam a contar a partir do último dado de cada série
      
      tabela1_5anos <- do.call(rbind,fat.5anos_tabela)
      
      tabela1_5anos$Serie.Janela <- row.names(tabela1_5anos) 
      
      tabela2_5anos[[no]] <- data.frame(tabela1_5anos[,"Serie.Janela"], tabela1_5anos[,c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")])
      colnames(tabela2_5anos[[no]]) <- c("Serie.Janela","Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      
      for(y in 1:length(row.names(tabela2_5anos[[no]]))){
        if(is.na(tabela2_5anos[[no]][y,"Jan"])==TRUE){
          tabela2_5anos[[no]][y,"Serie.Janela"] <- ""
          options(warn=-1)
        }
      }   
      
    }
  }
  
  
  
  tabela2_3anos_final <- do.call(rbind,tabela2_3anos)
  tabela2_4anos_final <- do.call(rbind,tabela2_4anos)
  tabela2_5anos_final <- do.call(rbind,tabela2_5anos)
  
  
  
  
  
  options(warn=-1)
  
  # exportar resultados
  ifelse(!dir.exists(file.path("./", "regression")), dir.create(file.path("./", "regression")), FALSE)
  
  for (i in 1:ncol(xts)){
    if(tam[1,i] < 60){
      write.csv2(serie_cas, paste0("./regression/", file, "_seasonallyAdjusted.csv"), row.names = F, na = "")
      write.csv2(fat_saz, paste0("./regression/", file, "_seasonalFactors.csv"),  row.names = F, na = "")
    }else{
      write.csv2(serie_cas, paste0("./regression/", file, "_seasonallyAdjusted.csv"), row.names = F, na = "")
      write.csv2(fat_saz, paste0("./regression/", file, "_seasonalFactors.csv"),  row.names = F, na = "")
      write.csv2(tabela2_3anos_final, paste0("./regression/", file, "_seasonalFactors_3years.csv"), row.names = F, na = "")
      write.csv2(tabela2_4anos_final, paste0("./regression/", file, "_seasonalFactors_4years.csv"), row.names = F, na = "")
      write.csv2(tabela2_5anos_final, paste0("./regression/", file, "_seasonalFactors_5years.csv"), row.names = F, na = "")
    }
  }
}
