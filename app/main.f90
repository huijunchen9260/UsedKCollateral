program main

    use UsedKCollateral
    implicit none

    integer(ik) :: indbk, indk, inde
    integer(ik) :: istat
    integer(ik) :: i_jump
    real(rk) :: cc
    real(rk) :: vec(7, 7, 7)
    real(rk) :: time0, time1, time2, time3
    real(rk) :: fout, xout
    real(rk) :: muwmiddle(mubknum, muknum)
    real(rk) :: gamma_vec(5), rhoe_vec(5), sigmae_vec(5)

    real(rk) :: nval, yval, bprimemax, bthreshold, epsval, kval, bval

    call checkdir(resDir)
    call checkdir(figDir)

    write(*, *) "Start!"

    ! call initConf(conf)

    conf%zval = zss

    ! ! conf%wval = 1.087975585937500_rk
    ! ! conf%qsell = 0.918790087890625_rk
    ! conf%qsell = 0.962958_rk
    ! conf%wval = 1.060124_rk
    ! conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
    ! ! conf%qsell = 0.954_rk
    ! ! conf%Qbuy = 1.0_rk
    ! conf%pval = psi/conf%wval
    ! conf%pfval = conf%pval
    ! conf%qbval = beta * conf%pfval/conf%pval
    ! conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
    ! conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))

    ! gamma_vec = linspace(0.020_rk, 0.035_rk, 10)
    ! rhoe_vec = linspace(0.680_rk, 0.670_rk, 10)
    ! sigmae_vec = linspace(0.110_rk, 0.120_rk, 10)

    ! call cpu_time(time0)

    ! write(*, *) "calibration for parameters"
    ! do i_jump = 1, 5, 1

    !     write(*, '(a, (a10, F20.10))') "Test combination: ", &
    !         "gamma = ", gamma_vec(i_jump), &
    !         "rho_e = " , rhoe_vec(i_jump), &
    !         "sigma_e = ", sigmae_vec(i_jump)

    !     gamma = gamma_vec(i_jump)
    !     rho_e = rhoe_vec(i_jump)
    !     sigma_e = sigmae_vec(i_jump)

    !     call initGrids()
    !     call initSol(sol)
    !     call TwoDimBisection(conf)
    !       call firmSimulation(sol, conf)
    !     call calistats(sol, conf)

    ! enddo

    ! call initGrids()
    ! call initSol(sol)
    ! call TwoDimBisection(conf)
    ! call calistats(sol, conf)

    ! call initGrids()
    ! call initSol(sol)
    ! call NelderMeadSimplex(sol, conf)
    ! call calistats(sol, conf)


    ! call initGrids()
    ! call initSol(sol)
    ! call OneDimBisection(sol, conf)

    ! call initGrids()
    ! call initSol(sol)
    ! call loadResult(sol, conf)

    call initGrids()
    call initSol(sol)
    call TwoDimBisection(conf)

    call firmSimulation(sol, conf)
    call saveResult(sol, conf)

    call cpu_time(time1)

    write(*, *) 'elapsed time: ', time1 - time0, ' seconds.'

end program main
