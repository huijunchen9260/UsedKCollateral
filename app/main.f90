program main

    use UsedKCollateral
    implicit none

    integer(ik) :: indbk, indk, inde
    integer(ik) :: istat
    real(rk) :: vec(7, 7, 7)
    real(rk) :: time0, time1, time2, time3
    real(rk) :: fout, xout
    real(rk) :: muwmiddle(mubknum, muknum)

    call checkdir(resDir)
    call checkdir(figDir)

    write(*, *) "Start!"

    call initGrids()
    call initSol(sol)
    call initConf(conf)

    conf%zval = zss
    conf%wval = 1.087975585937500_rk
    ! conf%wval = 1.89_rk
    conf%qsell = 0.918790087890625_rk
    conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
    ! conf%qsell = 0.954_rk
    ! conf%Qbuy = 1.0_rk
    conf%pval = psi/conf%wval
    conf%pfval = conf%pval
    conf%qbval = beta * conf%pfval/conf%pval
    conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
    conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))


    ! call loadResult(sol)

    call cpu_time(time0)

    call wvalueiter(sol, conf)
    call minSavingPolicy(sol, conf)
    call vvalueiter(sol, conf)

    call steadyStateDistribution(sol, conf)


    call cpu_time(time1)

    write(*, *) 'elapsed time: ', time1 - time0, ' seconds.'

    call saveResult(sol)

end program main
