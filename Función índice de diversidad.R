
HShannon <- function (datos, sitio, abundancia) {
  
  #El usuario debe indicar en que columana está ubicado el sitio y la abundancia
  names(datos)[c(sitio, abundancia)] <- c("sitio", "abundancia") #El programa define estas columnas con un nombre predeterminado
  
  s <- unique(datos$sitio) #Saca los nombres que hay en la columna sitio
  n <- length(s) #Longitud del vector s
  HShannon <- rep(NA, n) #Crea un vector vacío con la logitud de s
  names(HShannon) <- s #Asigna al vector vacío los nombres ingresados por el usuario
  
  
  #Se crea un ciclo que realice el índice para cada uno de los sitios ingresados
  for (i in s) {
    
    sit <- subset(datos, sitio ==i) #Extrae del data frame sitio "x"
    abu <- sum(sit$abundancia) #Abundancia total del sitio "x"
    pi <- sit$abundancia/abu #Crea vector con los pi del sitio "x" (pi=relación de la abundancia relativa y abundancia total)
    logn <- pi*log(pi)
    loglimp <- na.omit(logn) #En caso de que se genere un NaN ("carácter especial") se elimina
    
    sum(loglimp)*-1 -> HShannon[i] #índice de shanon
  }
  
  #En HShannon quedan guardados los resultados con el respectivo nombre del sitio
  
  
  #Para interpretar el índice de Shannon
  
  interp <- function(HShannon1){
    
    if (HShannon1>3.2){
      
      if(HShannon1>3.8){
        
        if(HShannon1<4.2){
          "alto"} 
        
        else{"superior"}
        
      } else {"medio"}
      
    } else{
      
      if(HShannon1<2.4){
        
        if(HShannon1>1.2){
          "bajo"}
        
        else{"pobre"}
        
      } else{"medio bajo"}
      
    }  
  }
  
  #Crea un vector vacío para dar respuesta al usuario con la longitud del número de sitios ingresados
  resp <- rep(NA, n)
  
  #Crea un ciclo que interprete cada uno de los sitios
  for (i in 1:n) {
    
    resp[i] <- interp(HShannon=HShannon[i])
    
  }
  
  
  data.frame(Shannon=HShannon,Interpretacion=resp)
  
}




