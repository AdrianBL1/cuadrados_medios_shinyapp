jisq.test.cont=function(x,distribucion=NULL,
                        nclases=c("sturgess","sqrt","user"),
                        output=TRUE,
                        alpha=NULL,
                        nparam=2,...)
{
  if (nclases=="sturgess") {
    k=floor(1+3.3*log10(length(x))) 
    q.distribucion=eval(parse(text=paste("q",distribucion,sep="")))
    d.distribucion=eval(parse(text=paste("d",distribucion,sep="")))
    q=q.distribucion((1:(k-1))/k,...)
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
      curve(d.distribucion(x,...),add=TRUE)
    }else {
      xhist=hist(x,breaks = xbreaks,plot = FALSE)
    }
    cat("\nIntervalo de aceptacion para la media\n")
    linf=round((1/2+(qnorm((alpha)/2)*(1/sqrt(12*length(x))))),4)
    lsup=round((1/2+(qnorm((alpha/2),lower.tail = FALSE)*(1/sqrt(12*length(x))))),4)
    int_media=list(Liminf=linf,Promedio=round(mean(x),4),Limsup=lsup)
    print(as.data.frame(int_media))
    cat("\n")
    cat("\nIntervalo de aceptacion para la varianza\n")
    linf_var=round(qchisq((alpha/2),df=length(x)-1,lower.tail = TRUE)/(12*(length(x)-1)),4)
    lsup_var=round(qchisq((alpha/2),df=length(x)-1,lower.tail = FALSE)/(12*(length(x)-1)),4)
    int_varianza=list(Liminf=linf_var,Varianza=var(x),Limsup=lsup_var)
    print(as.data.frame(int_varianza))
    cat("\n")
    O=xhist$counts
    E=length(x)/k
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
    s=x[-length(x)]
    r=x[-1]
    plot(s,r,pch=19,
         xlab="Valores del conjunto de datos",
         ylab="Valores del conjunto de datos",
         main="Diagrama de dispersion",
         sub="Prueba de Aleatoriedad")
  } else {
    if (nclases=="sqrt"){ 
      k=floor(sqrt(length(x)))
      q.distribucion=eval(parse(text=paste("q",distribucion,sep="")))
      d.distribucion=eval(parse(text=paste("d",distribucion,sep="")))
      q=q.distribucion((1:(k-1))/k,...)
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
                   main="Histograma para el conjunto",
                   col="skyblue")
        curve(d.distribucion(x,...),add=TRUE)
      }else {
        xhist=hist(x,breaks = xbreaks,plot = FALSE)
      }
      cat("\nIntervalos de aceptación para la media\n")
      linf=round((1/2+(qnorm((alpha)/2)*(1/sqrt(12*length(x))))),4)
      lsup=round((1/2+(qnorm((alpha/2),lower.tail = FALSE)*(1/sqrt(12*length(x))))),4)
      int_media=list(Liminf=linf,Promedio=round(mean(x),4),Limsup=lsup)
      print(as.data.frame(int_media))
      cat("\n")
      cat("\nIntervalo de aceptacion para la varianza\n")
      linf_var=round(qchisq((alpha/2),df=length(x)-1,lower.tail = TRUE)/(12*(length(x)-1)),4)
      lsup_var=round(qchisq((alpha/2),df=length(x)-1,lower.tail = FALSE)/(12*(length(x)-1)),4)
      int_varianza=list(Liminf=linf_var,Varianza=var(x),Limsup=lsup_var)
      print(as.data.frame(int_varianza))
      cat("\n")
      O=xhist$counts
      E=length(x)/k
      dname=deparse(substitute(x))
      method="Ji-Cuadrada de Pearson"
      estadistico=sum((O-E)^2/E)
      names(estadistico)="Estadistico ji-cuadrada"
      parametro=k-nparam-1
      names(parametro)="Grados de libertad"
      peval=pchisq(estadistico,df=parametro, lower.tail = FALSE)
      criterio=list(p_value=peval,DF=parametro,Metodo=method)
      print(as.data.frame(criterio))
      clases=format(xbreaks)
      clases=paste("(",clases[-(k+1)],",",clases[-1],"]",sep="")
      resultados=list(Intervalos=clases,frecuencia_observada=O,frecuencia_esperada=E,residuales=(O-E)/sqrt(E))
      if (output==TRUE){
        cat("\nTabla de la Distribución Ji-Cuadrada\n")
        print(as.data.frame(resultados))
      }
      s=x[-length(x)]
      r=x[-1]
      plot(s,r,pch=19,
           xlab="Valores del conjunto de datos",
           ylab="Valores del conjunto de datos",
           main="Diagrama de dispersion",
           sub="Prueba de Aleatoriedad")
    }else{
      k=as.integer(readline(prompt = "Ingresa el número de clases:"))
      q.distribucion=eval(parse(text=paste("q",distribucion,sep="")))
      d.distribucion=eval(parse(text=paste("d",distribucion,sep="")))
      q=q.distribucion((1:(k-1))/k,...)
      tol=sqrt(.Machine$double.eps)
      xbreaks=c(min(x)-tol,q,max(x)+tol)
      if (output==TRUE) {
        xhist=hist(x,
                   breaks = xbreaks,
                   freq=FALSE,
                   lty=2,
                   border = "grey50",
                   xlab = "Valores del conjunto de datos",
                   ylab="Frecuencia relativa",
                   main="Histograma del conjunto",
                   col="skyblue")
        curve(d.distribucion(x,...),add=TRUE)
      }else {
        xhist=hist(x,breaks = xbreaks,plot = FALSE)
      }
      cat("\nIntervalos de aceptación para la media\n")
      linf=round((1/2+(qnorm((alpha)/2)*(1/sqrt(12*length(x))))),4)
      lsup=round((1/2+(qnorm((alpha/2),lower.tail = FALSE)*(1/sqrt(12*length(x))))),4)
      int_media=list(Liminf=linf,Promedio=round(mean(x),4),Limsup=lsup)
      print(as.data.frame(int_media))
      cat("\n")
      cat("\nIntervalo de aceptacion para la varianza\n")
      linf_var=round(qchisq((alpha/2),df=length(x)-1,lower.tail = TRUE)/(12*(length(x)-1)),4)
      lsup_var=round(qchisq((alpha/2),df=length(x)-1,lower.tail = FALSE)/(12*(length(x)-1)),4)
      int_varianza=list(Liminf=linf_var,Varianza=var(x),Limsup=lsup_var)
      print(as.data.frame(int_varianza))
      cat("\n")
      O=xhist$counts
      E=length(x)/k
      dname=deparse(substitute(x))
      method="Ji-Cuadrada de Pearson"
      estadistico=sum((O-E)^2/E)
      names(estadistico)="Estadistico ji-cuadrada"
      parametro=k-nparam-1
      names(parametro)="Grados de libertad"
      peval=pchisq(estadistico,df=parametro, lower.tail = FALSE)
      criterio=list(p_value=peval,DF=parametro,Metodo=method)
      print(as.data.frame(criterio))
      clases=format(xbreaks)
      clases=paste("(",clases[-(k+1)],",",clases[-1],"]",sep="")
      resultados=list(Intervalos=clases,frecuencia_observada=O,frecuencia_esperada=E,residuales=(O-E)/sqrt(E))
      if (output==TRUE){
        cat("\nTabla de la Distribución Ji-Cuadrada\n")
        print(as.data.frame(resultados))
      }
      s=x[-length(x)]
      r=x[-1]
      plot(s,r,pch=19,
           xlab="Valores del conjunto de datos",
           ylab="Valores del conjunto de datos",
           main="Diagrama de dispersion",
           sub="Prueba de Aleatoriedad")
    }
  }
}