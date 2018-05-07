#' @title SACE diagnostic tool
#' @description The function uses some tests developed specifically for the SACE series to diagnose the seasonal adjustment.
#' @param x output from seasX13 function 
#' @param file a character string naming the file
#' @importFrom stats Box.test end start ts aggregate median na.omit quantile sd
#' @importFrom utils write.csv2
#' @export

diagX13 <- function(x, file = ""){
  
  nomes <- colnames(x$xSA)
  xts <- x$read$xts
  xts2 <- x$read$xtsNA
  dados <- as.data.frame(rownames(x$read$xts))   # datas
  serie_cas <- x$xSA
  
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
  inicio <- data.frame(matrix(nrow=2,ncol=ncol(xts)))
  colnames(inicio) <- nomes 
  fim <- data.frame(matrix(nrow=2,ncol=ncol(xts))) 
  colnames(fim) <- nomes 
  
  for(nome in nomes){
    inicio[,nome] <- as.numeric(c(substr(dados[posicao_inicial[,nome],1],1,4),substr(dados[posicao_inicial[,nome],1],6,7)))
    fim[,nome] <- as.numeric(c(substr(dados[posicao_final[,nome],1],1,4),substr(dados[posicao_final[,nome],1],6,7)))
  }
  
  
  # data da série
  inicio_serie <- start(x$seasonalFactors[,1])
  fim_serie <- end(x$seasonalFactors[,1])
  
  # guardar fatores sazonais
  fatores <- data.frame(x$seasonalFactors)
  
  # formatar a data e criar sequência da data para os fatores
  d1 <- as.Date(paste0(inicio_serie[1],"/", inicio_serie[2],"/1"))
  d2 <- as.Date(paste0(fim_serie[1],"/", fim_serie[2],"/1"))
  data <- seq(d1,d2, by = "month")
  
  # acrescentar coluna de ano e mês aos fatores
  fatores$ano <-  substr(data,1,4)
  fatores$mes <-  substr(data,6,7)
  
  # "novos_fatores" não leva em conta a previsão
  novos_fatores <- data.frame(matrix(nrow=nrow(xts),ncol=ncol(xts)),row.names = rownames(x$read$xts))
  colnames(novos_fatores) <- nomes
  for(no in nomes){
    d3 <- as.Date(paste0(fim[1,no],"/", fim[2,no],"/1"))  # datas sem a previsão 
    novos_fatores[,no] <- c(fatores[1:(which((row.names(xts)==c(d3))==TRUE)),no], rep(NA,nrow(xts)-((which((row.names(xts)==c(d3))==TRUE)))))
  } 
  
  # formatar a data e criar sequência da data para os novos_fatores
  d4 <- as.Date(row.names(xts)[1])
  d5 <- as.Date(row.names(xts)[nrow(xts)])
  data2 <- seq(d4,d5, by = "month")
  
  novos_fatores$ano <- substr(data2,1,4)
  novos_fatores$mes <- substr(data2,6,7)
  
  
  # desvios-padrão dos fatores mês
  if(length(nomes)==1){
    desvios_comprev <- aggregate(as.numeric(fatores[,colnames(x$read$xts)]), by = list(fatores$mes), FUN = sd, na.rm=TRUE)
  }else{
    desvios_comprev <- aggregate(fatores[,colnames(x$read$xts)], by = list(fatores$mes), FUN = sd, na.rm=TRUE)
  }
  desvios_semprev <- aggregate(novos_fatores[,colnames(x$read$xts)], by = list(novos_fatores$mes), FUN = sd, na.rm=TRUE)
  
  # médias dos fatores por mês
  if(length(nomes)==1){
    medias_comprev <- aggregate(as.numeric(fatores[,colnames(x$read$xts)]), by = list(fatores$mes), FUN = mean, na.rm=TRUE)
  }else{
    medias_comprev <- aggregate(fatores[,colnames(x$read$xts)], by = list(fatores$mes), FUN = mean, na.rm=TRUE)
  }
  medias_semprev <- aggregate(novos_fatores[,colnames(x$read$xts)], by = list(novos_fatores$mes), FUN = mean, na.rm=TRUE)
  
  # média dos últimos 3 anos
  ultimos3anos <- novos_fatores[-(1:(nrow(novos_fatores)-36)),]
  medias_ultimos3 <- aggregate(ultimos3anos[,colnames(x$read$xts)], by = list(ultimos3anos$mes), FUN = mean, na.rm=TRUE)
  
  colnames(desvios_semprev)[1] <- "Mês"
  colnames(desvios_comprev)[1] <- "Mês"
  colnames(medias_semprev)[1] <- "Mês"
  colnames(medias_comprev)[1] <- "Mês"
  colnames(medias_ultimos3)[1] <- "Mês"
  if(length(nomes)==1){
    colnames(desvios_semprev)[2] <- nomes
    colnames(desvios_comprev)[2] <- nomes
    colnames(medias_semprev)[2] <- nomes
    colnames(medias_comprev)[2] <- nomes
    colnames(medias_ultimos3)[2] <- nomes
  }
  
  # fatores sazonais por mês 
  fatores_jan <- fatores[which(fatores$mes == "01"),nomes]  
  fatores_fev <- fatores[which(fatores$mes == "02"),nomes]
  fatores_mar <- fatores[which(fatores$mes == "03"),nomes]
  fatores_abr <- fatores[which(fatores$mes == "04"),nomes]
  fatores_mai <- fatores[which(fatores$mes == "05"),nomes]
  fatores_jun <- fatores[which(fatores$mes == "06"),nomes]
  fatores_jul <- fatores[which(fatores$mes == "07"),nomes]
  fatores_ago <- fatores[which(fatores$mes == "08"),nomes]
  fatores_set <- fatores[which(fatores$mes == "09"),nomes]
  fatores_out <- fatores[which(fatores$mes == "10"),nomes]
  fatores_nov <- fatores[which(fatores$mes == "11"),nomes]
  fatores_dez <- fatores[which(fatores$mes == "12"),nomes]
  
  datas_jan <- (paste0("jan","/", fatores$ano[which(fatores$mes=="01")]))
  datas_fev <- (paste0("fev","/", fatores$ano[which(fatores$mes=="02")]))
  datas_mar <- (paste0("mar","/", fatores$ano[which(fatores$mes=="03")])) 
  datas_abr <- (paste0("abr","/", fatores$ano[which(fatores$mes=="04")])) 
  datas_mai <- (paste0("mai","/", fatores$ano[which(fatores$mes=="05")]))
  datas_jun <- (paste0("jun","/", fatores$ano[which(fatores$mes=="06")]))
  datas_jul <- (paste0("jul","/", fatores$ano[which(fatores$mes=="07")]))
  datas_ago <- (paste0("ago","/", fatores$ano[which(fatores$mes=="08")]))
  datas_set <- (paste0("set","/", fatores$ano[which(fatores$mes=="09")]))
  datas_out <- (paste0("out","/", fatores$ano[which(fatores$mes=="10")]))
  datas_nov <- (paste0("nov","/", fatores$ano[which(fatores$mes=="11")]))
  datas_dez <- (paste0("dez","/", fatores$ano[which(fatores$mes=="12")]))
  
  
  if(length(nomes)==1){
    fatores_pormes <- rbind(data.frame(c(fatores_jan,NA,fatores_fev,NA,fatores_mar,NA,fatores_abr,NA,fatores_mai,NA,fatores_jun,NA,
                                         fatores_jul,NA,fatores_ago,NA,fatores_set,NA,fatores_out,NA,fatores_nov,NA,fatores_dez)))
    colnames(fatores_pormes) <- c(nomes)
  }else{
    fatores_pormes <- rbind(fatores_jan,NA,fatores_fev,NA,fatores_mar,NA,fatores_abr,NA,fatores_mai,NA,fatores_jun,NA,
                            fatores_jul,NA,fatores_ago,NA,fatores_set,NA,fatores_out,NA,fatores_nov,NA,fatores_dez)
  }
  
  
  fatores_pormes$datas <-rbind(data.frame(c(datas_jan,NA,datas_fev,NA,datas_mar,NA,datas_abr,NA,datas_mai,NA,datas_jun,NA,
                                            datas_jul,NA,datas_ago,NA,datas_set,NA,datas_out,NA,datas_nov,NA,datas_dez)))  
  
  
  colnames(fatores_pormes$datas) <- "datas"
  
  fatores_pormes <- data.frame(fatores_pormes$data,fatores_pormes[,nomes])
  colnames(fatores_pormes) <- c("datas",nomes)
  
  
  # soma1: aditivo
  # soma2: multiplicativo
  if(length(nomes)==1){
    soma1 <- aggregate(as.numeric(fatores[,nomes]), by = list(fatores$ano), FUN = sum, na.rm=TRUE) 
    soma2 <- aggregate(as.numeric(fatores[,nomes]), by = list(fatores$ano), FUN = prod, na.rm=TRUE)
  }else{
    soma1 <- aggregate(fatores[,nomes], by = list(fatores$ano), FUN = sum, na.rm=TRUE) 
    soma2 <- aggregate(fatores[,nomes], by = list(fatores$ano), FUN = prod, na.rm=TRUE)
  }
  
  
  # identificar quem é aditivo/multiplicativo
  espec <- data.frame(unlist(x$espec))
  masc <- data.frame(espec[grepl("transform",rownames(espec)),] == "none")
  rownames(masc) <- nomes
  
  
  #soma_fatores2[,masc] <- soma_fatores1[,masc]
  soma_final <- as.data.frame(matrix(NA,nrow=nrow(soma1),ncol=ncol(soma1)))
  colnames(soma_final) <- c("Ano",nomes)
  
  
  tipo <- matrix(nrow=1,ncol=nrow(masc))
  for (i in 1:nrow(masc)){
    if(masc[i,] == TRUE){
      tipo[1,i] <- "none"
    }else{
      tipo[1,i] <- "log"
    }
  }
  colnames(tipo) <- nomes
  
  
  for(j in 2:ncol(soma_final)){
    if(tipo[,j-1]=="none"){
      soma_final[,j] <- soma1[,j]
    }else if(tipo[,j-1]=="log"){
      soma_final[,j] <- soma2[,j] 
    }
  }
  
  soma_final[,"Ano"] <- soma1[,1]
  
  
  # fatores sazonais
  fat.saz <- data.frame(x$seasonalFactors)
  fat.saz$datas <- as.data.frame(as.yearmon(data)) 
  fat.saz <- data.frame((fat.saz$datas),fat.saz[,nomes])  
  colnames(fat.saz) <- c("data",nomes) 
  
  
  
  # previsão de 1 ano para cada série
  com_previsao <- list()   #séries com previsão
  for(no in nomes){
    if(fim[2,no] == 12){
      com_previsao[[no]] <- ts(xts[,no]*NA, start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no]+1, 12), frequency = 12)
    }else{
      com_previsao[[no]] <- ts(xts[,no]*NA, start = c(inicio[1,no],inicio[2,no]), end = c(fim[1,no]+1, fim[2,no]), frequency = 12)
    }
  }
  
  previsao <- list()    #só a previsão
  for (no in nomes){
    previsao[[no]] <- ts(xts[,no]*NA, start = c(fim[1,no],fim[2,no]+1), end = end(com_previsao[[no]]), frequency = 12)
  }
  
  # datas (min e max) da previsão de cada série 
  dmin <- as.data.frame(matrix(NA,nrow=1,ncol=ncol(x$read$xts)))
  dmax <- as.data.frame(matrix(NA,nrow=1,ncol=ncol(x$read$xts)))
  colnames(dmin) <- nomes
  colnames(dmax) <- nomes
  
  for(no in nomes){
    dmin[,no] <- as.Date(min(as.Date(previsao[[no]])))
    dmax[,no] <- as.Date(max(as.Date(previsao[[no]])))
  }
  
  inicio_checar <- apply(dmin, 1, min)
  fim_checar <- apply(dmax, 1, max)
  
  datas_checar <- seq.Date(as.Date(inicio_checar),as.Date(fim_checar),"month")
  
  
  # definindo os fatores sazonais somente das previsões 
  fat_soprev <- as.data.frame(matrix(NA,ncol=length(nomes),nrow=12))
  fat_soprev_datas <- list()
  fatores_sprev <- list()
  fs <- list()
  fs_cdata <- as.data.frame(matrix(NA,ncol=length(nomes),nrow=length(datas_checar))) #fatores com as datas
  fatores_1anoantes <- as.data.frame(matrix(NA,ncol=length(nomes),nrow=12))
  colnames(fs_cdata)<-nomes
  colnames(fatores_1anoantes)<-nomes
  colnames(fat_soprev) <- nomes
  row.names(fs_cdata) <- datas_checar
  
  
  na.antes <- list()
  na.depois <- list()
  na.cond.antes <- list()
  na.cond.depois <- list()
  
  for(no in nomes){
    
    fs[[no]] <- fat.saz[which(as.yearmon(datas_checar)[1]==fat.saz[,"data"]):which(as.yearmon(datas_checar)[length(datas_checar)]==fat.saz[,"data"]),no]
    fs_cdata[,no] <-fs[[no]]
    
    na.cond.antes[[no]] <- which(row.names(fs_cdata)== as.Date(previsao[[no]])[1])
    if(na.cond.antes[[no]] == 1){
      na.antes[[no]] <- NULL
    }else{
      na.antes[[no]] <- rep(NA,(na.cond.antes[[no]]-1))
    }
    
    
    na.cond.depois[[no]] <- which(row.names(fs_cdata)== as.Date(previsao[[no]])[length(previsao[[no]])])
    if(na.cond.depois[[no]] == length(datas_checar)){
      na.depois[[no]] <- NULL
    }else{
      na.depois[[no]] <- rep(NA,(length(datas_checar)-(na.cond.depois[[no]])))
    }
    
    fat_soprev[,no] <- data.frame(fs_cdata[which(row.names(fs_cdata)==dmin[,no]):which(row.names(fs_cdata)==dmax[,no]),no]) # só a previsão
    fat_soprev_datas[[no]] <- row.names(fs_cdata)[which(row.names(fs_cdata)==dmin[,no]):which(row.names(fs_cdata)==dmax[,no])]
    fs_cdata[,no] <- unlist(c(na.antes[[no]],fat_soprev[,no],na.depois[[no]]))
    
    fatores_sprev[[no]] <- fat.saz[1:(which(fat.saz[,"data"]==as.yearmon(dmin[,no]-1))),no] #fatores sem a previsão de cada séries
    fatores_1anoantes[,no] <- fatores_sprev[[no]][(length(fatores_sprev[[no]])-11):length(fatores_sprev[[no]])]
  }
  
  
  # checar
  checar1 <- data.frame(matrix(NA,nrow=length(datas_checar),ncol=ncol(x$read$xts)),row.names = datas_checar)
  checar2 <- data.frame(matrix(NA,nrow=length(datas_checar),ncol=ncol(x$read$xts)),row.names = datas_checar)
  checar3 <- data.frame(matrix(NA,nrow=length(datas_checar),ncol=ncol(x$read$xts)),row.names = datas_checar)
  checar4 <- data.frame(matrix(NA,nrow=12,ncol=length(nomes)))
  
  colnames(checar1)<-nomes
  colnames(checar2)<-nomes
  colnames(checar3)<-nomes
  colnames(checar4)<-nomes
  
  for(no in nomes){
    for(l in 1:12){
      if (tipo[1,no]=="none"){    
        c1 <- fat_soprev[l,no] - fatores_1anoantes[l,no]
        p <- data.frame(fs_cdata[which(row.names(fs_cdata)==fat_soprev_datas[[no]][1]):which(row.names(fs_cdata)==fat_soprev_datas[[no]][length(fat_soprev_datas[[no]])]),no],row.names=fat_soprev_datas[[no]])
        c2 <- p[l,] - medias_ultimos3[which(substr(row.names(p),6,7)[l]==medias_ultimos3[,"Mês"]),no]
        c3 <- p[l,] - medias_semprev[which(substr(row.names(p),6,7)[l]==medias_semprev[,"Mês"]),no]
        c4 <- desvios_comprev[l,no] - desvios_semprev[l,no]
        
        checar1[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c1 > 1 | c1 < (-1),"checar","--") 
        checar2[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c2 > 1 | c2 < (-1),"checar","--")
        checar3[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c3 > 1 | c3 < (-1),"checar","--") 
        checar4[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c4 > 1 | c4 < (-1),"checar","--")
        
      }else if (tipo[1,no]=="log"){    
        
        c1 <- fat_soprev[l,no] - fatores_1anoantes[l,no]
        p <- data.frame(fs_cdata[which(row.names(fs_cdata)==fat_soprev_datas[[no]][1]):which(row.names(fs_cdata)==fat_soprev_datas[[no]][length(fat_soprev_datas[[no]])]),no],row.names=fat_soprev_datas[[no]])
        c2 <- p[l,] - medias_ultimos3[which(substr(row.names(p),6,7)[l]==medias_ultimos3[,"Mês"]),no]
        c3 <- p[l,] - medias_semprev[which(substr(row.names(p),6,7)[l]==medias_semprev[,"Mês"]),no]
        c4 <- desvios_comprev[l,no] - desvios_semprev[l,no]
        
        checar1[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c1 > 0.1 | c1 < (-0.1),"checar","--")
        checar2[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c2 > 0.1 | c2 < (-0.1),"checar","--")
        checar3[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c3 > 0.1 | c3 < (-0.1),"checar","--")
        checar4[which(row.names(checar1)==row.names(p)[l]),no] <- ifelse(c4 > 0.1 | c4 < (-0.1),"checar","--")
        
      } 
    }
  }
  
  
  
  checar1$che1 <- as.yearmon(row.names(checar1))
  checar1$che11 <- NA
  colnames(checar1) <- c(nomes,"Datas","Checar")
  checar1 <- data.frame(checar1[,"Checar"],checar1[,"Datas"],checar1[,nomes])  
  colnames(checar1) <- c("Checar","Datas",nomes)
  checar1[1,"Checar"] <- "Prev-Obs"
  
  
  checar2$che2 <- as.yearmon(row.names(checar2))
  checar2$che22 <- NA
  colnames(checar2) <- c(nomes,"Datas","Checar")
  checar2 <- data.frame(checar2[,"Checar"],checar2[,"Datas"],checar2[,nomes])    
  colnames(checar2) <- c("Checar","Datas",nomes)
  checar2[1,"Checar"] <- "Prev-media(ults 3 anos)"
  
  
  checar3$che3 <- as.yearmon(row.names(checar3))
  checar3$che33 <- NA
  colnames(checar3) <- c(nomes,"Datas","Checar")
  checar3 <- data.frame(checar3[,"Checar"],checar3[,"Datas"],checar3[,nomes])  
  colnames(checar3) <- c("Checar","Datas",nomes)
  checar3[1,"Checar"] <- "Prev-media"
  
  
  checar4$che4 <- substr(as.yearmon(seq.Date(as.Date("00/1/1"),as.Date("00/12/1"),"month")),1,3)
  checar4$che44 <- NA
  colnames(checar4) <- c(nomes,"Datas","Checar")
  checar4 <- data.frame(checar4[,"Checar"],checar4[,"Datas"],checar4[,nomes])  
  colnames(checar4) <- c("Checar","Datas",nomes)
  checar4[1,"Checar"] <- "Desvio (com prev.)-Desvio(sem.prev)"
  
  
  checar <- rbind(checar1,NA,NA,checar2,NA,NA,checar3,NA,NA,checar4)
  
  
  ## checar 5, 6 e 7 (NOVOS!) - A SAÍDA É SÓ O checar7
  
  datas <- as.yearmon(substr(dados[,1],1,10))
  dados_as <- data.frame(serie_cas)
  rownames(dados_as) <- NULL
  dados_as$datas <- datas
  colnames(dados_as) <- c(nomes,"datas")
  dados_as <- data.frame(dados_as[,"datas"],dados_as[,nomes])
  colnames(dados_as) <- c("datas",nomes)
  
  
  # media móvel centrada
  media_movel <- data.frame(matrix(NA, ncol = length(nomes),nrow = ((nrow(dados_as)-13)+1)))
  colnames(media_movel) <- nomes
  
  for(no in nomes){  
    for(i in 1:nrow(media_movel)){
      media_movel[i,no] <- mean(c(dados_as[i,no],dados_as[(i+1),no],dados_as[(i+2),no],dados_as[(i+3),no],dados_as[(i+4),no],
                                  dados_as[(i+5),no],dados_as[(i+6),no],dados_as[(i+7),no],dados_as[(i+8),no],dados_as[(i+9),no],
                                  dados_as[(i+10),no],dados_as[(i+11),no],dados_as[(i+12),no])) 
    }  
  }
  
  media_movel$datas <- datas[7:(length(datas)-6)]
  media_movel <- data.frame(media_movel[,"datas"],media_movel[,nomes])
  colnames(media_movel) <- c("datas",nomes)
  
  
  # dados_as ajustados - media móvel centrada
  
  dif_media <- data.frame(matrix(NA, ncol = length(nomes),nrow = ((nrow(dados_as)-13)+1)))
  colnames(dif_media) <- nomes
  
  for(no in nomes){
    dif_media[,no] <- (dados_as[which(dados_as$datas == media_movel$datas[1]):which(dados_as$datas == media_movel$datas[length(media_movel$datas)]),no]) - media_movel[,no]
  }
  
  dif_media$datas <- datas[7:(length(datas)-6)]
  dif_media <- data.frame(dif_media[,"datas"],dif_media[,nomes])
  colnames(dif_media) <- c("datas",nomes)
  
  
  # separar as diferenças por mês 
  
  janeiro   <- which(substr(dif_media$datas,1,3) == "jan")
  fevereiro <- which(substr(dif_media$datas,1,3) == "fev")
  marco     <- which(substr(dif_media$datas,1,3) == "mar")
  abril     <- which(substr(dif_media$datas,1,3) == "abr")
  maio      <- which(substr(dif_media$datas,1,3) == "mai")
  junho     <- which(substr(dif_media$datas,1,3) == "jun")
  julho     <- which(substr(dif_media$datas,1,3) == "jul")
  agosto    <- which(substr(dif_media$datas,1,3) == "ago")
  setembro  <- which(substr(dif_media$datas,1,3) == "set")
  outubro   <- which(substr(dif_media$datas,1,3) == "out")
  novembro  <- which(substr(dif_media$datas,1,3) == "nov")
  dezembro  <- which(substr(dif_media$datas,1,3) == "dez")
  
  dif_jan <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(janeiro)))
  dif_fev <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(fevereiro)))
  dif_mar <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(marco)))
  dif_abr <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(abril)))
  dif_mai <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(maio)))
  dif_jun <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(junho)))
  dif_jul <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(julho)))
  dif_ago <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(agosto)))
  dif_set <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(setembro)))
  dif_out <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(outubro)))
  dif_nov <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(novembro)))
  dif_dez <- data.frame(matrix(NA,ncol = length(nomes), nrow = length(dezembro)))
  
  colnames(dif_jan) <- nomes
  colnames(dif_fev) <- nomes
  colnames(dif_mar) <- nomes
  colnames(dif_abr) <- nomes
  colnames(dif_mai) <- nomes
  colnames(dif_jun) <- nomes
  colnames(dif_jul) <- nomes
  colnames(dif_ago) <- nomes
  colnames(dif_set) <- nomes
  colnames(dif_out) <- nomes
  colnames(dif_nov) <- nomes
  colnames(dif_dez) <- nomes
  
  for(no in nomes){ 
    for(i in 1:length(janeiro)){
      dif_jan[i,no] <- dif_media[janeiro,no][i] 
    }
    for(i in 1:length(fevereiro)){
      dif_fev[i,no] <- dif_media[fevereiro,no][i] 
    }
    for(i in 1:length(marco)){
      dif_mar[i,no] <- dif_media[marco,no][i] 
    }
    for(i in 1:length(abril)){
      dif_abr[i,no] <- dif_media[abril,no][i] 
    }
    for(i in 1:length(maio)){
      dif_mai[i,no] <- dif_media[maio,no][i] 
    }
    for(i in 1:length(junho)){
      dif_jun[i,no] <- dif_media[junho,no][i] 
    }
    for(i in 1:length(julho)){
      dif_jul[i,no] <- dif_media[julho,no][i] 
    }
    for(i in 1:length(agosto)){
      dif_ago[i,no] <- dif_media[agosto,no][i] 
    }
    for(i in 1:length(setembro)){
      dif_set[i,no] <- dif_media[setembro,no][i]
    }
    for(i in 1:length(outubro)){
      dif_out[i,no] <- dif_media[outubro,no][i] 
    }
    for(i in 1:length(novembro)){
      dif_nov[i,no] <- dif_media[novembro,no][i] 
    }
    for(i in 1:length(dezembro)){
      dif_dez[i,no] <- dif_media[dezembro,no][i] 
    }
    
  } 
  
  
  meses <- rbind(dif_jan,dif_fev,dif_mar,dif_abr,dif_mai,dif_jun,dif_jul,dif_ago,dif_set,dif_out,dif_nov,dif_dez)
  
  #datas por mês
  djan <- as.data.frame(dif_media$datas[janeiro])
  dfev <- as.data.frame(dif_media$datas[fevereiro])
  dmar <- as.data.frame(dif_media$datas[marco])
  dabr <- as.data.frame(dif_media$datas[abril])
  dmai <- as.data.frame(dif_media$datas[maio])
  djun <- as.data.frame(dif_media$datas[junho])
  djul <- as.data.frame(dif_media$datas[julho])
  dago <- as.data.frame(dif_media$datas[agosto])
  dset <- as.data.frame(dif_media$datas[setembro])
  dout <- as.data.frame(dif_media$datas[outubro])
  dnov <- as.data.frame(dif_media$datas[novembro])
  ddez <- as.data.frame(dif_media$datas[dezembro])
  
  colnames(djan) <- "datas"
  colnames(dfev) <- "datas"
  colnames(dmar) <- "datas"
  colnames(dabr) <- "datas"
  colnames(dmai) <- "datas"
  colnames(djun) <- "datas"
  colnames(djul) <- "datas"
  colnames(dago) <- "datas"
  colnames(dset) <- "datas"
  colnames(dout) <- "datas"
  colnames(dnov) <- "datas"
  colnames(ddez) <- "datas"
  
  datas_dif <- rbind(djan,dfev,dmar,dabr,dmai,djun,djul,dago,dset,dout,dnov,ddez) 
  
  
  tabela_dif_media <- cbind(datas_dif,meses)
  
  # sinal da diferença 
  
  sinal_dif <- tabela_dif_media*NA
  sinal_dif$datas <- tabela_dif_media$datas
  
  for(no in nomes){
    for(i in 1:nrow(tabela_dif_media)){
      if(is.na(tabela_dif_media[i,no])){
        sinal_dif[i,no] <- NA
      }else{
        if(tabela_dif_media[i,no] > 0){
          sinal_dif[i,no] <- 1
        }else{
          sinal_dif[i,no] <- 0
        }
      }
    }
  }
  
  
  # proporção dos sinais 
  
  sinal_jan <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(janeiro)))
  sinal_fev <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(fevereiro)))
  sinal_mar <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(marco)))
  sinal_abr <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(abril)))
  sinal_mai <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(maio)))
  sinal_jun <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(junho)))
  sinal_jul <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(julho)))
  sinal_ago <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(agosto)))
  sinal_set <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(setembro)))
  sinal_out <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(outubro)))
  sinal_nov <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(novembro)))
  sinal_dez <- data.frame(matrix(NA,ncol = length(nomes),nrow = length(dezembro)))
  
  colnames(sinal_jan) <- nomes
  colnames(sinal_fev) <- nomes
  colnames(sinal_mar) <- nomes
  colnames(sinal_abr) <- nomes
  colnames(sinal_mai) <- nomes
  colnames(sinal_jun) <- nomes
  colnames(sinal_jul) <- nomes
  colnames(sinal_ago) <- nomes
  colnames(sinal_set) <- nomes
  colnames(sinal_out) <- nomes
  colnames(sinal_nov) <- nomes
  colnames(sinal_dez) <- nomes
  
  prop_jan <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_fev <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_mar <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_abr <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_mai <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_jun <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_jul <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_ago <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_set <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_out <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_nov <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  prop_dez <- data.frame(matrix(NA,ncol = length(nomes),nrow = 2)) 
  
  colnames(prop_jan) <- nomes
  colnames(prop_fev) <- nomes
  colnames(prop_mar) <- nomes
  colnames(prop_abr) <- nomes
  colnames(prop_mai) <- nomes
  colnames(prop_jun) <- nomes
  colnames(prop_jul) <- nomes
  colnames(prop_ago) <- nomes
  colnames(prop_set) <- nomes
  colnames(prop_out) <- nomes
  colnames(prop_nov) <- nomes
  colnames(prop_dez) <- nomes
  
  
  
  for(no in nomes){
    
    # janeiro
    for(i in 1:length(janeiro)){
      sinal_jan[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "jan")),no][i] 
    }
    prop_jan[1,no] <- prop.table(table(sinal_jan[,no]))[1]
    prop_jan[2,no] <- prop.table(table(sinal_jan[,no]))[2]
    
    if(is.na(prop_jan[1,no])){
      prop_jan[1,no] <- 0
    }
    if(is.na(prop_jan[2,no])){
      prop_jan[2,no] <- 0
    }
    
    # fevereiro
    for(i in 1:length(fevereiro)){
      sinal_fev[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "fev")),no][i] 
    }
    prop_fev[1,no] <- prop.table(table(sinal_fev[,no]))[1]
    prop_fev[2,no] <- prop.table(table(sinal_fev[,no]))[2]
    
    if(is.na(prop_fev[1,no])){
      prop_fev[1,no] <- 0
    }
    if(is.na(prop_fev[2,no])){
      prop_fev[2,no] <- 0
    }
    
    # março
    for(i in 1:length(marco)){
      sinal_mar[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "mar")),no][i] 
    }
    prop_mar[1,no] <- prop.table(table(sinal_mar[,no]))[1]
    prop_mar[2,no] <- prop.table(table(sinal_mar[,no]))[2]
    
    if(is.na(prop_mar[1,no])){
      prop_mar[1,no] <- 0
    }
    if(is.na(prop_mar[2,no])){
      prop_mar[2,no] <- 0
    }
    
    # abril
    for(i in 1:length(abril)){
      sinal_abr[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "abr")),no][i] 
    }
    prop_abr[1,no] <- prop.table(table(sinal_abr[,no]))[1]
    prop_abr[2,no] <- prop.table(table(sinal_abr[,no]))[2]
    
    if(is.na(prop_abr[1,no])){
      prop_abr[1,no] <- 0
    }
    if(is.na(prop_abr[2,no])){
      prop_abr[2,no] <- 0
    }
    
    # maio
    for(i in 1:length(maio)){
      sinal_mai[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "mai")),no][i] 
    }
    prop_mai[1,no] <- prop.table(table(sinal_mai[,no]))[1]
    prop_mai[2,no] <- prop.table(table(sinal_mai[,no]))[2]
    
    if(is.na(prop_mai[1,no])){
      prop_mai[1,no] <- 0
    }
    if(is.na(prop_mai[2,no])){
      prop_mai[2,no] <- 0
    }
    
    # junho
    for(i in 1:length(junho)){
      sinal_jun[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "jun")),no][i] 
    }
    prop_jun[1,no] <- prop.table(table(sinal_jun[,no]))[1]
    prop_jun[2,no] <- prop.table(table(sinal_jun[,no]))[2]
    
    if(is.na(prop_jun[1,no])){
      prop_jun[1,no] <- 0
    }
    if(is.na(prop_jun[2,no])){
      prop_jun[2,no] <- 0
    }
    
    # julho
    for(i in 1:length(julho)){
      sinal_jul[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "jul")),no][i] 
    }
    prop_jul[1,no] <- prop.table(table(sinal_jul[,no]))[1]
    prop_jul[2,no] <- prop.table(table(sinal_jul[,no]))[2]
    
    if(is.na(prop_jul[1,no])){
      prop_jul[1,no] <- 0
    }
    if(is.na(prop_jul[2,no])){
      prop_jul[2,no] <- 0
    }
    
    # agosto
    for(i in 1:length(agosto)){
      sinal_ago[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "ago")),no][i] 
    }
    prop_ago[1,no] <- prop.table(table(sinal_ago[,no]))[1]
    prop_ago[2,no] <- prop.table(table(sinal_ago[,no]))[2]
    
    if(is.na(prop_ago[1,no])){
      prop_ago[1,no] <- 0
    }
    if(is.na(prop_ago[2,no])){
      prop_ago[2,no] <- 0
    }
    
    # setembro
    for(i in 1:length(setembro)){
      sinal_set[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "set")),no][i] 
    }
    prop_set[1,no] <- prop.table(table(sinal_set[,no]))[1]
    prop_set[2,no] <- prop.table(table(sinal_set[,no]))[2]
    
    if(is.na(prop_set[1,no])){
      prop_set[1,no] <- 0
    }
    if(is.na(prop_set[2,no])){
      prop_set[2,no] <- 0
    }
    
    # outubro
    for(i in 1:length(outubro)){
      sinal_out[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "out")),no][i] 
    }
    prop_out[1,no] <- prop.table(table(sinal_out[,no]))[1]
    prop_out[2,no] <- prop.table(table(sinal_out[,no]))[2]
    
    if(is.na(prop_out[1,no])){
      prop_out[1,no] <- 0
    }
    if(is.na(prop_out[2,no])){
      prop_out[2,no] <- 0
    }
    
    # novembro
    for(i in 1:length(novembro)){
      sinal_nov[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "nov")),no][i] 
    }
    prop_nov[1,no] <- prop.table(table(sinal_nov[,no]))[1]
    prop_nov[2,no] <- prop.table(table(sinal_nov[,no]))[2]
    
    if(is.na(prop_nov[1,no])){
      prop_nov[1,no] <- 0
    }
    if(is.na(prop_nov[2,no])){
      prop_nov[2,no] <- 0
    }
    
    # dezembro
    for(i in 1:length(dezembro)){
      sinal_dez[i,no] <- sinal_dif[(which(substr(sinal_dif$datas,1,3) == "dez")),no][i] 
    }
    prop_dez[1,no] <- prop.table(table(sinal_dez[,no]))[1]
    prop_dez[2,no] <- prop.table(table(sinal_dez[,no]))[2]
    
    if(is.na(prop_dez[1,no])){
      prop_dez[1,no] <- 0
    }
    if(is.na(prop_dez[2,no])){
      prop_dez[2,no] <- 0
    }
    
  }
  
  
  # checar caso tenha mais de 80% de 0 ou 1 (por mês)
  
  checar5 <- data.frame(matrix(NA,nrow=12,ncol=length(nomes)))
  colnames(checar5) <- nomes
  
  for(no in nomes){
    checar5[1,no]  <- ifelse(prop_jan[1,no] >= 0.8 | prop_jan[2,no] >= 0.8,"checar","--")
    checar5[2,no]  <- ifelse(prop_fev[1,no] >= 0.8 | prop_fev[2,no] >= 0.8,"checar","--")
    checar5[3,no]  <- ifelse(prop_mar[1,no] >= 0.8 | prop_mar[2,no] >= 0.8,"checar","--")
    checar5[4,no]  <- ifelse(prop_abr[1,no] >= 0.8 | prop_abr[2,no] >= 0.8,"checar","--")
    checar5[5,no]  <- ifelse(prop_mai[1,no] >= 0.8 | prop_mai[2,no] >= 0.8,"checar","--")
    checar5[6,no]  <- ifelse(prop_jun[1,no] >= 0.8 | prop_jun[2,no] >= 0.8,"checar","--")
    checar5[7,no]  <- ifelse(prop_jul[1,no] >= 0.8 | prop_jul[2,no] >= 0.8,"checar","--")
    checar5[8,no]  <- ifelse(prop_ago[1,no] >= 0.8 | prop_ago[2,no] >= 0.8,"checar","--")
    checar5[9,no]  <- ifelse(prop_set[1,no] >= 0.8 | prop_set[2,no] >= 0.8,"checar","--")
    checar5[10,no] <- ifelse(prop_out[1,no] >= 0.8 | prop_out[2,no] >= 0.8,"checar","--")
    checar5[11,no] <- ifelse(prop_nov[1,no] >= 0.8 | prop_nov[2,no] >= 0.8,"checar","--")
    checar5[12,no] <- ifelse(prop_dez[1,no] >= 0.8 | prop_dez[2,no] >= 0.8,"checar","--")
  } 
  
  
  # limites inferior e superior
  intervalos <- data.frame(matrix(NA, nrow = 2, ncol = length(nomes)))
  colnames(intervalos) <- nomes
  
  for(no in nomes){
    intervalos[1,no] <- quantile(dif_media[,no], probs = 0.1, na.rm = T)
    intervalos[2,no] <- quantile(dif_media[,no], probs = 0.9, na.rm = T)
  }
  
  
  # medianas das diferenças (por mês)
  
  medianas <- data.frame(matrix(NA, nrow = 12, ncol = length(nomes)))
  colnames(medianas) <- nomes
  
  for(no in nomes){
    medianas[1,no]  <- median(dif_jan[,no], na.rm = T)
    medianas[2,no]  <- median(dif_fev[,no], na.rm = T)
    medianas[3,no]  <- median(dif_mar[,no], na.rm = T)
    medianas[4,no]  <- median(dif_abr[,no], na.rm = T)
    medianas[5,no]  <- median(dif_mai[,no], na.rm = T)
    medianas[6,no]  <- median(dif_jun[,no], na.rm = T)
    medianas[7,no]  <- median(dif_jul[,no], na.rm = T)
    medianas[8,no]  <- median(dif_ago[,no], na.rm = T)
    medianas[9,no]  <- median(dif_set[,no], na.rm = T)
    medianas[10,no] <- median(dif_out[,no], na.rm = T)
    medianas[11,no] <- median(dif_nov[,no], na.rm = T)
    medianas[12,no] <- median(dif_dez[,no], na.rm = T)
  }
  
  
  checar6 <- data.frame(matrix(NA,nrow=12,ncol=length(nomes)))
  colnames(checar6) <- nomes
  
  for(no in nomes){
    checar6[1,no]  <- ifelse(medianas[1,no]  > intervalos[2,no] | medianas[1,no]  < intervalos[1,no] ,"checar","--")
    checar6[2,no]  <- ifelse(medianas[2,no]  > intervalos[2,no] | medianas[2,no]  < intervalos[1,no] ,"checar","--")
    checar6[3,no]  <- ifelse(medianas[3,no]  > intervalos[2,no] | medianas[3,no]  < intervalos[1,no] ,"checar","--")
    checar6[4,no]  <- ifelse(medianas[4,no]  > intervalos[2,no] | medianas[4,no]  < intervalos[1,no] ,"checar","--")
    checar6[5,no]  <- ifelse(medianas[5,no]  > intervalos[2,no] | medianas[5,no]  < intervalos[1,no] ,"checar","--")
    checar6[6,no]  <- ifelse(medianas[6,no]  > intervalos[2,no] | medianas[6,no]  < intervalos[1,no] ,"checar","--")
    checar6[7,no]  <- ifelse(medianas[7,no]  > intervalos[2,no] | medianas[7,no]  < intervalos[1,no] ,"checar","--")
    checar6[8,no]  <- ifelse(medianas[8,no]  > intervalos[2,no] | medianas[8,no]  < intervalos[1,no] ,"checar","--")
    checar6[9,no]  <- ifelse(medianas[9,no]  > intervalos[2,no] | medianas[9,no]  < intervalos[1,no] ,"checar","--")
    checar6[10,no] <- ifelse(medianas[10,no] > intervalos[2,no] | medianas[10,no] < intervalos[1,no] ,"checar","--")
    checar6[11,no] <- ifelse(medianas[11,no] > intervalos[2,no] | medianas[11,no] < intervalos[1,no] ,"checar","--")
    checar6[12,no] <- ifelse(medianas[12,no] > intervalos[2,no] | medianas[12,no] < intervalos[1,no] ,"checar","--")
  } 
  
  
  # o checar5 e o checar6 devem ser "checar" para o checar7 ser "checar"
  
  checar7 <- data.frame(matrix(NA,nrow=12,ncol=length(nomes)))
  colnames(checar7) <- nomes
  
  for(no in nomes){
    checar7[1,no]  <- ifelse(checar5[1,no]  == "checar" & checar6[1,no]  == "checar" ,"checar","--")
    checar7[2,no]  <- ifelse(checar5[2,no]  == "checar" & checar6[2,no]  == "checar" ,"checar","--")
    checar7[3,no]  <- ifelse(checar5[3,no]  == "checar" & checar6[3,no]  == "checar" ,"checar","--")
    checar7[4,no]  <- ifelse(checar5[4,no]  == "checar" & checar6[4,no]  == "checar" ,"checar","--")
    checar7[5,no]  <- ifelse(checar5[5,no]  == "checar" & checar6[5,no]  == "checar" ,"checar","--")
    checar7[6,no]  <- ifelse(checar5[6,no]  == "checar" & checar6[6,no]  == "checar" ,"checar","--")
    checar7[7,no]  <- ifelse(checar5[7,no]  == "checar" & checar6[7,no]  == "checar" ,"checar","--")
    checar7[8,no]  <- ifelse(checar5[8,no]  == "checar" & checar6[8,no]  == "checar" ,"checar","--")
    checar7[9,no]  <- ifelse(checar5[9,no]  == "checar" & checar6[9,no]  == "checar" ,"checar","--")
    checar7[10,no] <- ifelse(checar5[10,no] == "checar" & checar6[10,no] == "checar" ,"checar","--")
    checar7[11,no] <- ifelse(checar5[11,no] == "checar" & checar6[11,no] == "checar" ,"checar","--")
    checar7[12,no] <- ifelse(checar5[12,no] == "checar" & checar6[12,no] == "checar" ,"checar","--")
  } 
  
  
  ## TODOS OS CHECAR
  
  checar7$che7 <- substr(as.yearmon(seq.Date(as.Date("00/1/1"),as.Date("00/12/1"),"month")),1,3)
  checar7$che77 <- NA 
  colnames(checar7) <- c(nomes,"Datas","Checar") 
  checar7 <- data.frame(checar7[,"Checar"],checar7[,"Datas"],checar7[,nomes])    
  colnames(checar7) <- c("Checar","Datas",nomes) 
  checar7[1,"Checar"] <- "Média centrada" 
  
  #require(plyr) 
  
  # vazio <- data.frame(matrix(NA, nrow = 2, ncol = (length(nomes)+2)))  
  # colnames(vazio) <- c(nomes,"Datas","Checar") 
  # vazio <- data.frame(vazio[,"Checar"],vazio[,"Datas"],vazio[,nomes])   
  # colnames(vazio) <- c("Checar","Datas",nomes) 
  # 
  # 
  # checar <- rbind(checar1,vazio,checar2,vazio,checar3,vazio,checar4,vazio,checar7)
  # 
  # 
  # 
  checar <- data.frame(matrix(NA, ncol = (length(nomes)+2), nrow = 68))
  colnames(checar) <- c("Checar", "Datas", nomes)
  
  for(no in nomes){
    checar[1:12,no] <- as.character(checar1[,no])
    checar[13:14,no] <- NA
    checar[15:26,no] <- as.character(checar2[,no])
    checar[27:28,no] <- NA
    checar[29:40,no] <- as.character(checar3[,no])
    checar[41:42,no] <- NA
    checar[43:54,no] <- as.character(checar4[,no])
    checar[55:56,no] <- NA
    checar[57:68,no] <- as.character(checar7[,no])
  }
  
  checar[1:12,"Datas"] <- substr(row.names(checar1),1,7)
  checar[13:14,"Datas"] <- NA
  checar[15:26,"Datas"] <- substr(row.names(checar2),1,7)
  checar[27:28,"Datas"] <- NA
  checar[29:40,"Datas"] <- substr(row.names(checar3),1,7)
  checar[41:42,"Datas"] <- NA
  checar[43:54,"Datas"] <- substr(row.names(checar4),1,7)
  checar[55:56,"Datas"] <- NA
  checar[57:68,"Datas"] <- substr(seq.Date(as.Date("00/1/1"),as.Date("00/12/1"),"month"),6,7)
  
  
  checar[1,"Checar"] <- "Prev-Obs"
  checar[15,"Checar"] <- "Prev-media(ults 3 anos)"
  checar[29,"Checar"] <- "Prev-media"
  checar[43,"Checar"] <- "Desvio (com prev.)-Desvio(sem.prev)"
  checar[57,"Checar"] <- "Média centrada"
  
  ifelse(!dir.exists(file.path("./", "diag")), dir.create(file.path("./", "diag")), FALSE)
  
  write.csv2(desvios_semprev, paste0("./diag/", file, " - desvios - sem prev.csv"), row.names = F, na = "")
  write.csv2(desvios_comprev, paste0("./diag/", file, " - desvios - com prev.csv"), row.names = F, na = "")
  write.csv2(medias_semprev, paste0("./diag/", file, " - medias - sem prev.csv"), row.names = F, na = "")
  write.csv2(medias_comprev, paste0("./diag/", file, " - medias - com prev.csv"), row.names = F, na = "")
  write.csv2(medias_ultimos3, paste0("./diag/", file, " - medias - ultms 3 anos.csv"), row.names = F, na = "")
  write.csv2(soma_final, paste0("./diag/", file, " - fatores no ano.csv"), row.names = F, na = "")
  write.csv2(fatores_pormes, paste0("./diag/", file, " - fatores sazonais por mês.csv"), row.names = F, na = "")
  write.csv2(fat.saz, paste0("./diag/", file, " - fatores sazonais.csv"), row.names = F, na = "")
  write.csv2(checar, paste0("./diag/", file, " - checar.csv"), row.names = F, na = "")
  
}
