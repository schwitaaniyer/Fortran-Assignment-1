implicit none
integer, parameter :: N = 100000
real, parameter :: a = 3.8317
real, parameter :: pi = 3.14159265358979323846
real :: dx, x, J0, psi, x_exp, x2_exp
integer :: i
dx = a / N
x_exp = 0.0
x2_exp = 0.0

do i = 0, N
    x = i * dx
    J0 = dcos(x * pi / (2.0 * a))  ! Bessel function of first kind
    psi = J0 * J0
    x_exp = x_exp + psi * x
    x2_exp = x2_exp + psi * x * x
end do

x_exp = x_exp * dx * 2.0 / a
x2_exp = x2_exp * dx * 2.0 / a

write(*,*) 'Expectation value of x:', x_exp
write(*,*) 'Expectation value of x^2:', x2_exp
end program
