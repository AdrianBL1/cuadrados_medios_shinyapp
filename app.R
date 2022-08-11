library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Cuadrados Medios"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Algorítmo de Cuadrados Medios"),
      br(),
      p("Uso"),
      p("Para el uso de este algoritmo es necesario el uso de los siguientes datos que deberán ser ingresador por el usuario."),
      br(),
      fluidRow(
        column(12, 
               numericInput("sem", 
                            p("Ingrese la semilla (4 digitos)"), 
                            value = 1234)),
        column(12, 
               numericInput("iter", 
                            p("Ingrese la cantidad de iteraciones"), 
                            value = 50)) 
      ),
      br(),
      checkboxInput("verificacion","Añadir funcion de densidad",value = FALSE),
      br(),
      submitButton("Ejecutar Algoritmo"),
      br(),
      fluidRow(
        column(12,
               selectInput("nclases", p("Selecciona el tipo de Clase"), 
                           choices = list("sturgess" = 1,
                                          "sqrt" = 2,
                                          "user" = 3), selected = 1))
      ),
      br(),
      submitButton("Ejecutar Prueba Estadistica"),
      br(),
      p("Descargar los números pseudoaletorios generados"),
      downloadButton("downloadData", "Descargar Data"),
      br(),
      br(),
      br(),
      img(src = "cuadrados_medios.png", height = 80, width = 205),
      br(),
      p("Algoritmo de Cuadrados Medios para la creación de números pseudoaleatorios, colaborado por: "), 
      span("Adrian Basilio Lopez,", style = "color:blue"),
      span("Teodoro del Angel Rodriguez Aguirre y ", style = "color:blue"),
      span("Lenin Alfonso Sanchez Sanchez", style = "color:blue"),
      br(),
      br(),
      p("Carrera de Ingeniería en Sistemas Computacionales \n"),
      h4("Instituto Tecnológico Superior de Misantla")
    ),
    mainPanel(
      h2("Resultados"),
      sidebarPanel(
        textOutput("datos"), width = 15
      ),
      br(),
      plotOutput('Hist'),
      br(),
      br(),
      h3("Pruebas de Estadística para Números Pseudoaleatorios"),
      br(),
      tableOutput("table"),
      br(),
      plotOutput("histsturgessRender"),
      br(),
      p("\nIntervalo de aceptacion para la media:\n"),
      tableOutput("media"),
      br(),
      p("\nIntervalo de aceptacion para la varianza:\n"),
      tableOutput("varianza"),
      br(),
      tableOutput("jicuadrada"),
      br(),
      plotOutput("aleatoriedad")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  datos <- reactive({
    #Algoritmo de cuadrados medios
    
    semilla <- as.integer(input$sem)
    longitud_semilla = nchar(semilla)
    
    #Detecta si la semilla tiene 4 digitos o no
    if(longitud_semilla >= 4){
      
      iteraciones <- as.integer(input$iter)
      
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
      print(t(datos))
    }else {
      print("La semilla debe tener al menos 4 digitos")
    }
    
  })
  
  output$datos <- renderText({
    datos()
  })
  
  output$Hist <- renderPlot({
    hist(datos(), breaks ="FD", col = 'skyblue', border = 'white',freq = FALSE,
         xlab="Valores del conjunto de datos",
         ylab="Densidad",
         main="Histograma para el conjunto de datos")
    if (input$verificacion == TRUE){
      curve(dunif(x,0,1),type="l",col="red",add=TRUE)
    }
  })
  
  histsturgess <- reactive({
    #jisq.test.cont(datos(),distribucion = "unif","sturgess",output = TRUE,alpha = 0.05,nparam = 2, 0, 1)
    x = datos()
    distribucion = "unif"
    output = TRUE
    alpha = 0.05
    nparam = c(2, 0, 1)
    
    k=floor(1+3.3*log10(length(x))) 
    q.distribucion=eval(parse(text=paste("q",distribucion,sep="")))
    d.distribucion=eval(parse(text=paste("d",distribucion,sep="")))
    q=q.distribucion((1:(k-1))/k)
    tol=sqrt(.Machine$double.eps)
    xbreaks=c(min(x)-tol,q,max(x)+tol)
    if (output==TRUE) {
      xhist=hist(x,
                 breaks = xbreaks,
                 freq=FALSE,
                 lty=2,border = "grey50",
                 xlab="Valores del conjunto de datos",
                 ylab="Frecuencia relativa",
                 main="Histograma para el conjunto"
                 ,col="skyblue")
      curve(d.distribucion(x),add=TRUE)
    }else {
      xhist=hist(x,breaks = xbreaks,plot = FALSE)
    }
    xhist
  })
  
  output$histsturgessRender <- renderPlot({
    histsturgess()
  })
  
  output$table <- renderTable({
    datos = round(datos(),4)
    n = input$iter
    maximo = max(datos())
    minimo = min(datos())
    rango = maximo - minimo
    k = trunc(sqrt(n))
    i = rango/k
    
    #Limites de clase
    limites <- c(minimo + i + .0001)
    for (j in 1:k-1) {
      limites<-round(append(limites, limites[j] + i + 0.00005), 4)
    }
    li <- c(minimo, limites[1],limites[2],limites[3],limites[4],limites[5],limites[6])
    ls <- c(limites[1],limites[2],limites[3],limites[4],limites[5],limites[6],limites[7])
    
    #Marca de clase
    marcaClase <- c(NULL)
    for(l in 1:k){
      marcaClase <- round(append(marcaClase,(li[l] + ls[l])/2),4)
    }
    
    #Frecuencia
    c1 = 0
    frecuencia <- c(NULL)
    for(m in 1:k){
      for(o in 1:n){
        if(datos[o] >= li[m] & datos[o] <= ls[m]){
          c1 = c1+ 1
        }
      }
      frecuencia <- append(frecuencia, c1)
      c1 = 0
    }
    
    #Frecuencia relativa
    frecuenciaR <- c(NULL)
    for(p in 1:k){
      frecuenciaR <- append(frecuenciaR, frecuencia[p]/n)
    }
    
    #Frecuencia acumulada
    c2 = 0
    frecuenciaA <- c(NULL)
    for(q in 1:k){
      for(r in 1:n){
        if(datos[r] >= li[q] & datos[r] <= ls[q]){
          c2 = c2+ 1
        }
      }
      frecuenciaA <- append(frecuenciaA, c2)
    }
    
    #Frecuencia relativa acumulada
    frecuenciaRA <- c(frecuenciaR[1])
    for(s in 2:k){
      frecuenciaRA <- append(frecuenciaRA, frecuenciaR[s] + frecuenciaRA[s-1])
    }
    df = data.frame(li,ls,marcaClase,frecuencia,frecuenciaR,frecuenciaA,frecuenciaRA)
    names(df)=c("Limite Inf","Marca de Clase","Limite Sup","Frecuencia","Frecuencia R","Frecuencia A","Frecuencia R.A")
    df
  })
  
  #Media
  output$media <- renderTable({
    alpha = 0.05
    linf=round((1/2+(qnorm((alpha)/2)*(1/sqrt(12*length(datos()))))),4)
    lsup=round((1/2+(qnorm((alpha/2),lower.tail = FALSE)*(1/sqrt(12*length(datos()))))),4)
    int_media=list(Liminf=linf,Promedio=round(mean(datos()),4),Limsup=lsup)
    df = as.data.frame(int_media)
    names(df) = c("Limite Inferior","Promedio","Limite Superior")
    df
  })
  
  #Varianza
  output$varianza <- renderTable({
    alpha = 0.05
    linf_var=round(qchisq((alpha/2),df=length(datos())-1,lower.tail = TRUE)/(12*(length(datos())-1)),4)
    lsup_var=round(qchisq((alpha/2),df=length(datos())-1,lower.tail = FALSE)/(12*(length(datos())-1)),4)
    int_varianza=list(Liminf=linf_var,Varianza=var(datos()),Limsup=lsup_var)
    df = as.data.frame(int_varianza)
    names(df) = c("Limite Inferior","Varianza","Limite Superior")
    df
  })
  
  output$jicuadrada <- renderTable({
    x = datos()
    xhist = histsturgess()
    k=floor(1+3.3*log10(length(x)))
    nparam = c(2, 0, 1)
    
    tol=sqrt(.Machine$double.eps)
    xbreaks=c(min(x)-tol,q,max(x)+tol)
    
    O=xhist$counts
    E=length(x)/k
    dname=deparse(substitute(x))
    dname=deparse(substitute(x))
    method="Ji-Cuadrada de Pearson"
    estadistico=sum((O-E)^2/E)
    names(estadistico)="Estadistico ji-cuadrada"
    parametro=k-nparam-1
    names(parametro)="Grados de libertad"
    peval=round(pchisq(estadistico,df=parametro,lower.tail = FALSE),4)
    criterio=list(p_value=peval,DF=parametro,Metodo=method)
    print(as.data.frame(criterio))
    clases=format(xbreaks)
    clases=paste("(",clases[-(k+1)],",",clases[-1],"]",sep="")
    resultados=list(Intervalos=clases,Frecuencia_Observada=O,Frecuencia_Esperada=E,Residuales=(O-E)/sqrt(E))
    if (output==TRUE){
      cat("\nTabla de la Distribución Ji-Cuadrada\n")
      print(as.data.frame(resultados))
    }
  })
  
  output$aleatoriedad <- renderPlot({
    x = datos()
    output = TRUE
    tol=sqrt(.Machine$double.eps)
    xbreaks=c(min(x)-tol,q,max(x)+tol)
    
    if (output==TRUE) {
      xhist=hist(x,
                 breaks = xbreaks,
                 freq=FALSE,
                 lty=2,
                 border = "grey50",
                 xlab="Valores del conjunto de datos",
                 ylab="Frecuencia relativa",
                 main="Histograma para el conjunto"
                 ,col="skyblue")
      curve(d.distribucion(x),add=TRUE)
    }else {
      xhist=hist(x,breaks = xbreaks,plot = FALSE)
    }
  })
  
  # Downloadable csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cuadrados_medios", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos(), file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)