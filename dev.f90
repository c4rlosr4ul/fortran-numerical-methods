program DiffractionIntensity
    implicit none 
    real :: theta, intensity, theta_increment
    integer :: i, num_steps
    integer, parameter :: file_unit = 10

    num_steps = 40000
    theta_increment = 4.0 / (num_steps - 1)  ! Adjusted to cover the range [-2.0, 2.0]
    theta = -2.0

    open(file_unit, file="data/intensity.dat", status='unknown')
    
    do i = 1, num_steps
        intensity = calculate_intensity(theta)
        write(*, *) theta, intensity
        write(file_unit, *) theta, intensity 
        theta = theta + theta_increment
    end do

    close(file_unit)
   
contains

    function calculate_intensity(theta) result(intensity)
        implicit none
        real, parameter :: intensity_ratio = 16.0, slit_width = 0.3e-3, wavelength = 480e-9, pi = 4.0 * atan(1.0)
        real :: intensity, theta, beta

        beta = 2.0 * pi * slit_width * sin(theta) / wavelength 

        if (abs(theta) < 1e-10) then
            intensity = intensity_ratio
        else
            intensity = intensity_ratio * (sin(beta / 2.0) / (beta / 2.0))**2
        end if
    end function calculate_intensity

end program DiffractionIntensity

