#Algoritmo de cuadrados medios
cuadrados.medios <- function(sem,iter){
  semilla <- as.integer(sem) # convert character into integer
  longitud_semilla = nchar(semilla)
  
  #Detecta si la semilla tiene 4 digitos o no
  if(longitud_semilla >= 4){
    
    iteraciones <- as.integer(iter)
    
    # 10^2k-k/2
    base_elevada = 10^((2*longitud_semilla)-(longitud_semilla/2))
    
    # valor para convertir en decimal 
    decim = 10^longitud_semilla
    
    datos = cbind()
    # generacion del numero pseudoaleatorio n veces
    for (i in 1:iteraciones){
      xi_cuadrada = semilla^2
      
      division = floor((xi_cuadrada/base_elevada))
      division = division * base_elevada
      
      operacion = (xi_cuadrada - division)/10^(longitud_semilla/2)
      
      resultado = floor(operacion)
      semilla = resultado
      resultado = resultado/decim
      datos = cbind(datos, resultado)
    }
    print(datos)
  }else {
    print("La semilla debe tener al menos 4 digitos")
  }
}