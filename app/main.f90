program main

    use UsedKCollateral
    implicit none

    integer(ik) :: indbk, indk, inde
    integer(ik) :: istat
    real(rk) :: cc
    real(rk) :: vec(7, 7, 7)
    real(rk) :: time0, time1, time2, time3
    real(rk) :: fout, xout
    real(rk) :: muwmiddle(mubknum, muknum)

    real(rk) :: nval, yval, bprimemax, bthreshold, epsval, kval, bval

    call checkdir(resDir)
    call checkdir(figDir)

    write(*, *) "Start!"

    call initGrids()
    call initSol(sol)
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


    ! call loadResult(sol)

    call cpu_time(time0)

    ! call TwoDimBisection(conf)

    call OneDimBisection(sol, conf)

    ! call wvalueiter(sol, conf)
    ! call minSavingPolicy(sol, conf)
    ! call vvalueiter(sol, conf)
    ! call steadyStateDistribution(sol, conf)


    ! cc = 0.0_rk
    ! do inde = 1, enum, 1
    !     epsval = egrid(inde)
    !     do indk = 1, knum, 1
    !         kval = kgrid(indk)
    !         call debtThreshold(nval, yval, bprimemax, bthreshold, kval, epsval, conf)
    !         do indbk = 1, bknum, 1
    !             bval = kval*bkgrid(indbk)
    !             if (sol%gvb(indbk, indk, inde) / kval > bkbounds(2)) then
    !                 ! conf%xuval = yval - conf%wval*nval - bval + conf%Qbuy*(1.0_rk-delta)*kval
    !                 ! conf%xdval = yval - conf%wval*nval - bval + conf%qsell*(1.0_rk-delta)*kval
    !                 write(*, '(3(a, F9.6), a, 2(F12.6))') "over zeta at b' = ", sol%gvb(indbk, indk, inde), &
    !                     ", k = ", kval, &
    !                     ", b/k = ", sol%gvb(indbk, indk, inde) / kval, &
    !                     ", with cash = ", conf%xuval, conf%xdval

    !                 cc = cc + 1
    !             endif
    !             if (sol%gvb(indbk, indk, inde) / kval > bkbounds(2) .and. sol%wtrue(indbk, indk, inde)) then
    !                 write(*, *) "Also is w type"
    !             endif
    !         enddo
    !     enddo
    ! enddo
    ! write(*, *) cc

    ! write(*, *) 1 - count(sol%wtrue) / (dble(bknum*knum*enum))


    call cpu_time(time1)

    write(*, *) 'elapsed time: ', time1 - time0, ' seconds.'

    call saveResult(sol)

end program main
