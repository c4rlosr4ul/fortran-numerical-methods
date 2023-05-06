program raiz
implicit none
real*8 :: a, b ,c ,D ,x1 , x2, xi1, xi2
WRITE(*, *)"Ingrese a,b y c" 
READ(*, *) a, b, c
D = b ** 2 - 4 * a * c
if (D > 0) then 
x1 = (- b - SQRT(D))/(2 * a)
x2 = (- b + SQRT(D))/(2 * a)
    WRITE(*, *) "Las raices son", x1, x2
else if (D < 0) then
x1 = (- b )/(2 * a)
xi1 = - SQRT(-D)/(2 * a)
x2 = (- b + SQRT(-D))/(2 * a)
xi2 =     - SQRT(-D)/(2 * a) 
    WRITE(*, *) "Las raices son", x1, "+",xi1, "i", " y ", x2, "+", xi2, "i"
else
x1 = (- b )/(2 * a)
x2 = (- b )/(2 * a)
    WRITE(*, *) "Las raices son", x1, x2
end if
end program  raiz







