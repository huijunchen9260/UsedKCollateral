program main

    use UsedKCollateral
    use pyplot_module
    implicit none

    ! type(gpf) :: gp

    type(pyplot) :: plt
    type(solutions) :: sol
    type(configurations) :: conf
    integer(ik) :: indbk
    real(rk) :: vec(7, 7, 7)
    real(rk) :: time0, time1, time2, time3
    real(rk) :: fout, xout

    call checkdir(resDir)
    call checkdir(figDir)

    write(*, *) "Start!"

    call initGrids()
    call initSol(sol)

    conf%zval = zss
    conf%wval = 1.087975585937500_rk
    conf%qsell = 0.918790087890625_rk
    conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
    ! conf%qsell = 0.954_rk
    ! conf%Qbuy = 1.0_rk
    conf%pval = psi/conf%wval
    conf%pfval = conf%pval
    conf%qbval = beta * conf%pfval/conf%pval

    ! call loadResult(sol)

    ! call plt%initialize()
    ! call plt%add_plot(&
    !     logspace(-1.0_rk, 5.0_rk, 20), &
    !     linspace(-1.0_rk, 5.0_rk, 20), &
    !     label = 'test', &
    !     linestyle = 'r-', &
    !     linewidth = 2&
    !     )
    ! call plt%showfig()


    call cpu_time(time0)

    ! call wvalueiter(sol, conf)
    ! call minSavingPolicy(sol, conf)

    ! call vvalueiter(sol, conf)

    call cpu_time(time1)

    write(*, *) 'elapsed time: ', time1 - time0, ' seconds.'

    ! call saveResult(sol)

end program main
