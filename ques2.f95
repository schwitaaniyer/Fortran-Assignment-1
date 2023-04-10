    implicit none   
    integer, parameter :: N = 1000
    real, parameter :: t0 = 0.0, tN = 10.0
    real, parameter :: dt = (tN - t0) / N
    real, parameter :: lambda = 0.693
    real, parameter :: N0 = 200.0
    real :: t, Nt
    real, dimension(N+1) :: time, Ntime
    
    ! initial conditions
    time(1) = t0
    Ntime(1) = N0
    
    ! numerical solution using Euler method
    do i = 1, N
        t = time(i) + dt
        Nt = Ntime(i) - lambda * Ntime(i) * dt
        time(i+1) = t
        Ntime(i+1) = Nt
    end do
    
    ! output solution to file
    open(unit=1, file='decay.dat')
    do i = 1, N+1
        write(1,*) time(i), Ntime(i)
    end do
    close(1)
    
    ! find half-life
    do i = 1, N
        if (Ntime(i) <= N0/2 .and. Ntime(i+1) > N0/2) then
            t = (N0/2 - Ntime(i)) / (Ntime(i+1) - Ntime(i)) * dt + time(i)
            write(*,*) 'Half-life:', t
            exit
        end if
    end do
end program