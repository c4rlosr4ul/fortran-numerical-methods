program primos
implicit none
integer a,b,c,x,i,p,cero,n

    open(98,file="prueba_de_el_do_discimina_correctamente_a_los_numeros_primo.dat",status="unknown")
    open(99,file="buho2.dat",status="unknown")
    cero=0
    write(99,*)cero
    close(99)
    write(*,*)"Ingrese cuantos numeros primos desea sumar"
    read(*,*)n

do x=2,n

    if(x.eq.2)then
    goto 1010
    end if
    do i=2,((x/2)+1),1
    p=mod(x,i)
        if(p.eq.0)then
        goto 1000 
        end if
    end do
    1010 continue 
    write(98,*)x
    open(100,file="buho2.dat",status="old")
    read(100,*)a 
    b=a+x 
    close(100) 
    open(101,file="buho2.dat",status="old")
    write(101,*)b 
    close(101)
    1000 continue
end do
    open(102,file="buho2.dat",status="old")
    read(102,*)c
    write(*,*)"La suma de los primeros 100 numeros primos es ",c
end program 