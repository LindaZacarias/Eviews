# Eviews
Ejercicio 7.1

' Este programa resuelve el ejemplo 7.1 del libro de AZNAR que genera el cuadro 7.1 (pag.18) y que ilustra el cálculo de las funciones de autocovarianza y autocorrelación muestrales a partir de la serie temporal ¨Y¨ de 20 observaciones

' Crea la serie Y con los valores extraídos
series y
y(1) = 8
y(2) = 5
y(3) = 9
y(4) = 4
y(5) = 8
y(6) = 8
y(7) = 10
y(8) = 11
y(9) = 8
y(10) = 5
y(11) = 3
y(12) = 7
y(13) = 9
y(14) = 8
y(15) = 10
y(16) = 9
y(17) = 13
y(18) = 10
y(19) = 8
y(20) = 7

' Fija el número de periodos a rezagar y el número de observaciones
!k = 5
!n = 20

' Calcula la desviación respecto a la media y su cuadrado
series yd = y - @mean(y)
series yd2 = yd * yd

' Calcula series rezagadas hasta k = 5
series y1 = y(-1)
series y2 = y(-2)
series y3 = y(-3)
series y4 = y(-4)
series y5 = y(-5)

' Calcula desviaciones de las rezagadas
series y1d = y1 - @mean(y)
series y2d = y2 - @mean(y)
series y3d = y3 - @mean(y)
series y4d = y4 - @mean(y)
series y5d = y5 - @mean(y)

' Productos cruzados para autocovarianzas
series ydy1d = yd * y1d
series ydy2d = yd * y2d
series ydy3d = yd * y3d
series ydy4d = yd * y4d
series ydy5d = yd * y5d

' Varianza muestral (autocovarianza en lag 0)
!c0 = @sum(yd2) / !n

' Define vectores para autocovarianzas (cvec) y autocorrelaciones (rvec)
vector(!k) cvec
vector(!k) rvec

' Calcula autocovarianzas y autocorrelaciones
cvec(1) = @sum(ydy1d) / !n
cvec(2) = @sum(ydy2d) / !n
cvec(3) = @sum(ydy3d) / !n
cvec(4) = @sum(ydy4d) / !n
cvec(5) = @sum(ydy5d) / !n

for !i = 1 to !k
    rvec(!i) = cvec(!i) / !c0
next

' Mostrar vectores
show cvec
show rvec
'--------------------------
' CONFIGURACION INICIAL
!k=5
!n=20

'--------------------------
' PREPARACION DE DATOS
series yd = y - @mean(y)
series yd2 = yd*yd

' Series rezagadas
series y1 = y(-1)
series y2 = y(-2)
series y3 = y(-3)
series y4 = y(-4)
series y5 = y(-5)

' Desviaciones de las rezagadas
series y1d = y1 - @mean(y)
series y2d = y2 - @mean(y)
series y3d = y3 - @mean(y)
series y4d = y4 - @mean(y)
series y5d = y5 - @mean(y)

' Productos cruzados para autocovarianzas
series ydy1d = yd*y1d
series ydy2d = yd*y2d
series ydy3d = yd*y3d
series ydy4d = yd*y4d
series ydy5d = yd*y5d

'--------------------------
' CALCULO DE AUTOCOVARIANZAS Y AUTOCORRELACIONES
!c0 = @sum(yd2)/!n
vector(!k) cvec
cvec(1) = @sum(ydy1d)/!n
cvec(2) = @sum(ydy2d)/!n
cvec(3) = @sum(ydy3d)/!n
cvec(4) = @sum(ydy4d)/!n
cvec(5) = @sum(ydy5d)/!n

vector(!k) rvec
for !j = 1 to !k
  rvec(!j) = cvec(!j)/!c0
next

'--------------------------
' CALCULO DE AUTOCORRELACION PARCIAL (PACF)
vector(!k) fi
fi(1) = rvec(1)

matrix(2,2) n22
matrix(2,2) d22
n22(1,1)=1  
n22(1,2)=rvec(1)
n22(2,1)=rvec(1)
n22(2,2)=rvec(2)
d22(1,1)=1  
d22(1,2)=rvec(1)
d22(2,1)=rvec(1)
d22(2,2)=1
fi(2)=@det(n22)/@det(d22)

matrix(3,3) n33
matrix(3,3) d33
n33(1,1)=1   
n33(1,2)=rvec(1)
n33(1,3)=rvec(1)
n33(2,1)=rvec(1)
n33(2,2)=1
n33(2,3)=rvec(2)
n33(3,1)=rvec(2)
n33(3,2)=rvec(1)
n33(3,3)=rvec(3)
d33(1,1)=1   
d33(1,2)=rvec(1)
d33(1,3)=rvec(2)
d33(2,1)=rvec(1)
d33(2,2)=1
d33(2,3)=rvec(1)
d33(3,1)=rvec(2)
d33(3,2)=rvec(1)
d33(3,3)=1
fi(3)=@det(n33)/@det(d33)

matrix(4,4) n44
matrix(4,4) d44
n44(1,1)=1
n44(1,2)=rvec(1)
n44(1,3)=rvec(2)
n44(1,4)=rvec(1)
n44(2,1)=rvec(1)
n44(2,2)=1
n44(2,3)=rvec(1)
n44(2,4)=rvec(2)
n44(3,1)=rvec(2)
n44(3,2)=rvec(1)
n44(3,3)=1
n44(3,4)=rvec(3)
n44(4,1)=rvec(3)
n44(4,2)=rvec(2)
n44(4,3)=rvec(1)
n44(4,4)=rvec(4)
d44(1,1)=1
d44(1,2)=rvec(1)
d44(1,3)=rvec(2)
d44(1,4)=rvec(3)
d44(2,1)=rvec(1)
d44(2,2)=1
d44(2,3)=rvec(1)
d44(2,4)=rvec(2)
d44(3,1)=rvec(2)
d44(3,2)=rvec(1)
d44(3,3)=1
d44(3,4)=rvec(1)
d44(4,1)=rvec(3)
d44(4,2)=rvec(2)
d44(4,3)=rvec(1)
d44(4,4)=1
fi(4)=@det(n44)/@det(d44)

matrix(5,5) n55
matrix(5,5) d55
n55(1,1)=1
n55(1,2)=rvec(1)
n55(1,3)=rvec(2)
n55(1,4)=rvec(3)
n55(1,5)=rvec(1)
n55(2,1)=rvec(1)
n55(2,2)=1
n55(2,3)=rvec(1)
n55(2,4)=rvec(2)
n55(2,5)=rvec(2)
n55(3,1)=rvec(2)
n55(3,2)=rvec(1)
n55(3,3)=1
n55(3,4)=rvec(1)
n55(3,5)=rvec(3)
n55(4,1)=rvec(3)
n55(4,2)=rvec(2)
n55(4,3)=rvec(1)
n55(4,4)=1
n55(4,5)=rvec(4)
n55(5,1)=rvec(4)
n55(5,2)=rvec(3)
n55(5,3)=rvec(2)
n55(5,4)=rvec(1)
n55(5,5)=rvec(5)
d55(1,1)=1
d55(1,2)=rvec(1)
d55(1,3)=rvec(2)
d55(1,4)=rvec(3)
d55(1,5)=rvec(4)
d55(2,1)=rvec(1)
d55(2,2)=1
d55(2,3)=rvec(1)
d55(2,4)=rvec(2)
d55(2,5)=rvec(3)
d55(3,1)=rvec(2)
d55(3,2)=rvec(1)
d55(3,3)=1
d55(3,4)=rvec(1)
d55(3,5)=rvec(2)
d55(4,1)=rvec(3)
d55(4,2)=rvec(2)
d55(4,3)=rvec(1)
d55(4,4)=1
d55(4,5)=rvec(1)
d55(5,1)=rvec(4)
d55(5,2)=rvec(3)
d55(5,3)=rvec(2)
d55(5,4)=rvec(1)
d55(5,5)=1
fi(5)=@det(n55)/@det(d55)

'--------------------------
' GRAFICOS DE FAC Y FACP
series rvec_plot = na
series fi_plot = na
for !i = 1 to !k
  rvec_plot(!i) = rvec(!i)
  fi_plot(!i) = fi(!i)
next

' Graficos tipo stem (spike)
graph g_rvec.spike rvec_plot
g_rvec.name("FAC")
graph g_fi.spike fi_plot
g_fi.name("FACP")

show g_rvec
show g_fi
