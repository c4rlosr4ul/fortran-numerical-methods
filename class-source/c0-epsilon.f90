program epsilon
    implicit none 
    real*8 :: eps

    eps = 1.0d0
do while (1.0d0 + eps > 1.0d0)
    print *, eps
    eps = eps / 2.0
end do

end program epsilon
