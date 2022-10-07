program main

    use UsedKCollateral
    implicit none

    type(gpf) :: gp
    type(solutions) :: sol
    type(configurations) :: conf
    integer(ik) :: indbk
    real(rk) :: vec(7, 7, 7)
    real(rk) :: time0, time1, time2, time3

    ! if (.not. .d. resDir) then
    !     write(*, *) "no ", resDir, "; exit."
    !     stop
    ! endif
    ! if (.not. .d. figDir) then
    !     write(*, *) "no ", figDir, "; exit."
    !     stop
    ! endif

    write(*, *) "Start!"

    call initGrids()
    call initSol(sol)

    conf%zval = zss
    conf%qsell = 0.918790087890625_rk
    conf%wval = 1.087975585937500_rk
    conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
    conf%pval = psi/conf%wval
    conf%pfval = conf%pval
    conf%qbval = beta * conf%pfval/conf%pval

    call cpu_time(time0)

    call wvalueiter(sol, conf)
    call minSavingPolicy(sol, conf)

    ! call loadResult(sol)
    ! call vvalueiter(sol, conf)

    call cpu_time(time1)

    write(*, *) 'elapsed time: ', time1 - time0, ' seconds.'

    call saveResult(sol)

end program main
