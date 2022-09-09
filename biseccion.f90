module biseccion
contains 
    subroutine bs(xi,xf,error,xc,i) 
    implicit none 
    real*4 :: xi,xf,error,xc,d,e,rg,x,fxc,fxi,fxf
    integer :: i
    i=0
    e=(error**(-1))
    d=abs(xf-xi)
    do 
    if(rg>=e)exit
    i=i+1
    rg=(2**(i))/d
        xc=0.5*xi+0.5*xf
        fxi=xi**4 + 3*xi**3 - 2
        fxf=xf**4 + 3*xf**3 - 2
        fxc=xc**4 + 3*xc**3 - 2
        if(fxi*fxc>0.and.fxf*fxc>0)then
        xi=0;xf=0;error=0;xc=0;i=0
        else if(fxi*fxc>0.and.fxf*fxc<0) then
        xi=xc
        else if(fxi*fxc<0.and.fxf*fxc>0) then
        xf=xc
        end if 
    end do 
    return
    end subroutine
end module biseccion
