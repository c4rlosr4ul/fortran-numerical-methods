module falsa
contains 
    subroutine fl(xi,xf,iteraciones,xc) 
    implicit none 
    real*4 :: xi,xf,xc,d,e,x,fxc,fxi,fxf
    integer :: iteraciones,j,i
    d=abs(xf-xi)
    j=0
    do i=1,iteraciones,1
    j=j+i
        xc=(((xi*(xf**4 + 3*xf**3 - 2))-(xf*(xi**4 + 3*xi**3 - 2))))/(xf**4 + (3*xf**3) -xi**4 -(3*xi**3))
        fxi=(xi**4 + 3*xi**3 - 2)
        fxf=xf**4 + 3*xf**3 - 2
        fxc=xc**4 + 3*xc**3 - 2
        if(fxf*fxc>0)then
        xf=xc
        else if(fxf*fxc<0) then
        xi=xc
        end if 
    end do 
    return
    end subroutine
end module falsa
