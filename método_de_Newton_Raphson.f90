program Newton 
use funciones 
implicit none 
real*8:: xn0,xn1,x,num,f,xn2
real*8::epsi=1e-6
character::r 
write(*,*)"Digite N para emplear el metodo de Newton"
write(*,*)"Digite S para emplear el metodo de Secante"
read(*,*)r
if(r=='N'.or.r=="n") then
    write(*,*)"Sea 0=x^3- W"
    write(*,*)"Ingrese W"
    read(*,*)num
    write(*,*)"Ingrese su aproximacion de la raiz 0=x^3 -",num,"." 
    read(*,*)xn0
        do  
            call fc(num,xn0,f)  
            if(abs(f)<epsi) exit
            xn1=xn0-(f/df(xn0))
            xn0=xn1
            write(*,*)"La aproximacion de la raiz es:",xn0
        end do
    321 format(1x,A,ES10.3,A)
    write(*,321)"La raiz de 0=x^3 -",num,"." 
    123 format(1x,A,ES15.1,1X,A)
    write(*,123)"Con un error de ",epsi,"."
    write(*,*)"Es",xn0,"."
else if (r=='S'.or.r=="s") then
    write(*,*)"Sea 0=(1.4e-5)x**(1.5) + (1.15e-5)x**(2) - (2*9.81)/1000 "
    write(*,*)"Ingrese el primer valor para la aproximacion de la raiz"
    read(*,*)xn0
    write(*,*)"Ingrese el segundo valor para la aproximacion de la raiz"
    read(*,*)xn1
        write(*,*)"Vn",xn0,"V*n",f2(xn0),"."
        write(*,*)"Vn",xn1,"V*n",f2(xn1),"."
        do
        xn2=xn1-((f2(xn0))*((xn1-xn0)/(f2(xn1)-f2(xn0))))
        write(*,*)"Vn",xn2,"V*n",f2(xn2),"."
        if((f2(xn2))<epsi)exit
        xn0=xn1
        xn1=xn2
        end do
    1234 format(A,ES15.5,1X,A,1X,ES15.1,A)
    write(*,1234)"La raiz es",xn2,"con un error de",epsi,"."
else if (r/='N'.or.r/='n'.or.r/='s'.or.r/='S') then
    write(*,*)"Opcion no valida, digite nuevamente"
end if
!Aloha Lokita
end program 
