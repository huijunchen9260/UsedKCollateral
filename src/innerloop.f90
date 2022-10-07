module innerloop

    use iso_Fortran_env, only: rk => real64, ik => int32
    use ogpf
    use numerics
    use io
    use parameters
    use optMod
    ! use nlopt_wrap
    ! use nlopt_enum
    implicit none

contains

    subroutine vvalueiter(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: iter
        integer(ik) :: indk, inde, indef, indbk
        real(rk) :: dist, distv, distgvk, distgvb
        real(rk) :: kwdnstar, ewdnstar, kwupstar, ewupstar
        real(rk) :: kstar, vstar, wstar, kstay
        real(rk) :: epsval, kval, bkval, bval, nval, yval, xuval, xdval
        real(rk) :: kvfval, bvfval, vval
        real(rk) :: bprimemax, bthreshold
        real(rk), dimension(:, :, :), allocatable :: ev, tv, tgvk, tgvb

        allocate(ev(bknum, knum, enum), source = 0.0_rk)
        allocate(tv(bknum, knum, enum), source = 0.0_rk)
        allocate(tgvk(bknum, knum, enum), source = 0.0_rk)
        allocate(tgvb(bknum, knum, enum), source = 0.0_rk)

        iter = 0_ik
        dist = 2.0_rk*vTol

        write(*, *) "value function iteration: V-type (constrained)"

        mainwhile: do while (dist > vTol .and. iter <= maxviter )

            iter = iter + 1

            ! ------------------- !
            ! DEBUG
            ! ------------------- !
            ! iter = maxviter + 1 !
            ! ------------------- !

            ! ----------------------- !
            ! conditional expectation !
            ! ----------------------- !

            do inde = 1, enum, 1
                do indk = 1, knum, 1
                    do indbk = 1, bknum, 1
                        ev(indbk, indk, inde) = &
                            dot_product(pie(inde, :), sol%v(indbk, indk, :))
                    enddo
                enddo
            enddo


            ! --------------------------------------------- !
            ! value function iteration for constrained firm !
            ! --------------------------------------------- !

            epsloop: do inde = 1, enum, 1
                ! write(*, *) 'Start e = ', inde
                epsval = egrid(inde)
                conf%evbk = ev(:, :, inde)
                ! ---------------------------------------------------------- !
                ! DEBUG: Can we reuse kwstars?                               !
                ! ---------------------------------------------------------- !
                kwupstar = sol%kwupvec(inde); ewupstar = sol%ewupvec(inde) !
                kwdnstar = sol%kwdnvec(inde); ewdnstar = sol%ewdnvec(inde) !
                ! ---------------------------------------------------------- !
                ! call cpu_time(t0)
                kloop: do indk = 1, knum, 1
                    ! write(*, *) 'Start k = ', indk
                    kval = kgrid(indk)
                    kstay = (1.0_rk - delta)*kval
                    nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))
                    yval = conf%zval * epsval * kval**alpha * nval**nu
                    bprimemax = zeta*kval
                    bthreshold = yval - conf%wval*nval + &
                        conf%qbval*bprimemax
                    bkloop: do indbk = 1, bknum, 1
                        bkval = bkgrid(indbk)
                        bval = bkval*kval
                        conf%xuval = yval - conf%wval*nval &
                                - bval + conf%Qbuy*kstay
                        conf%xdval = yval - conf%wval*nval &
                                - bval + conf%qsell*kstay
                        if (sol%wtrue(indbk, indk, inde)) then
                            ! call cpu_time(t0)
                            call kwrule(wstar, kstar, yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar, conf)
                            tv(indbk, indk, inde) = wstar - conf%pval * bval
                            tgvk(indbk, indk, inde) = kstar
                            tgvb(indbk, indk, inde) = sol%gwb(indk, inde)
                            ! call cpu_time(t1)
                            ! write(*, *) "W-type time: ", t1 - t0, ", at (bk, k, e) = (", indbk, indk, inde, ")"
                        else
                            ! call cpu_time(t0)
                            call kvrule(bvfval, kvfval, vval, bprimemax, bthreshold, kstay, bval, conf)
                            tv(indbk, indk, inde) = vval
                            tgvk(indbk, indk, inde) = kvfval
                            tgvb(indbk, indk, inde) = bvfval
                            ! call cpu_time(t1)
                            ! write(*, *) "V-type time: ", t1 - t0, ", at (bk, k, e) = (", indbk, indk, inde, ")"
                        endif
                    enddo bkloop
                    ! write(*, *) 'Finish k = ', indk
                enddo kloop
                ! write(*, *) 'Finish e = ', inde
            enddo epsloop

            distv = maxval(dabs(sol%v - tv))
            distgvk = maxval(dabs(sol%gvk - tgvk))
            distgvb = maxval(dabs(sol%gvb - tgvb))
            dist = dmax1(distv, distgvk, distgvb)

            sol%v = tv
            sol%gvk = tgvk
            sol%gvb = tgvb

            if (iter == 1 .and. show_valueiter) then
                ! print on terminal
                write(*, '(a4, 8(a20))') 'iter', 'distv', 'distgvk', 'distgvb', 'kmin', 'kmax', 'bkmin', 'bkmax'
                ! write the same content into the variable sep
                write(sep, '(a4, 8(a20))') 'iter', 'distv', 'distgvk', 'distgvb', 'kmin', 'kmax', 'bkmin', 'bkmax'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
                write(*, '(I4, 8(ES20.6))') iter, distv, distgvk, distgvb, &
                                            minval(sol%gvk), maxval(sol%gvk), &
                                            minval(sol%gvb), maxval(sol%gvb)
            endif

            if (show_valueiter) then
                write(*, '(I4, 8(ES20.6))') iter, distv, distgvk, distgvb, &
                                            minval(sol%gvk), maxval(sol%gvk), &
                                            minval(sol%gvb), maxval(sol%gvb)
            endif

        enddo mainwhile



    end subroutine vvalueiter

    subroutine minSavingPolicy(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf

        integer(ik) :: indk, inde, indef, indbk
        integer(ik) :: iter
        integer(ik) :: kidx
        real(rk) :: dist, kw, kstay
        real(rk) :: epsval, nval, yval, kval, kfval, bfwval, bfminval, Jval, bval
        integer(ik), dimension(:, :), allocatable :: kidxmat
        real(rk), dimension(:), allocatable :: bfw
        real(rk), dimension(:, :), allocatable :: tbtilde, kwmat, bfwmat

        allocate(bfw(enum), source = 0.0_rk)
        allocate(tbtilde(knum, enum), source = 0.0_rk)
        allocate(kidxmat(knum, enum), source = 0_ik)
        allocate(kwmat(knum, enum), source = 0.0_rk)
        allocate(bfwmat(knum, enum), source = 0.0_rk)

        write(*, *) "minimum saving policy for unconstrained firm (W-type)"

        ! ----------------------------------------------------- !
        ! calculate location and weight on decision on capital  !
        ! ----------------------------------------------------- !

        do inde = 1, enum, 1
            do indk = 1, knum, 1
                kfval = sol%gwk(indk, inde)
                call linear_interpolation(kgrid, knum, kfval, kidx, kw)
                kidxmat(indk, inde) = kidx
                kwmat(indk, inde) = kw
            enddo
        enddo

        iter = 0_ik
        dist = 2.0_rk*minBTol

        do while (dist > minBTol .and. iter <= maxbtildeiter)

            iter = iter + 1

            do inde = 1, enum, 1
                epsval = egrid(inde)
                do indk = 1, knum, 1
                    kval = kgrid(indk)
                    kfval = sol%gwk(indk, inde)
                    kstay = (1.0_rk - delta)*kval
                    kidx = kidxmat(indk, inde)
                    kw = kwmat(indk, inde)

                    do indef = 1, enum, 1
                        bfw(indef) = kw * sol%btilde(kidx, indef) + &
                                (1.0_rk - kw) * sol%btilde(kidx+1_rk, indef)
                    enddo

                    bfwval = minval(bfw, mask = pie(inde, :) > 0.0_rk)
                    bfwmat(indk, inde) = bfwval
                    nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))
                    yval = conf%zval * epsval * kval**alpha * nval**nu
                    if (kfval >= kstay) then
                        Jval = conf%Qbuy
                    else
                        Jval = conf%qsell
                    endif
                    bfminval = yval - conf%wval * nval + &
                        conf%qbval*dmin1(bfwval, zeta*kval) - &
                        Jval*(kfval - kstay)

                    tbtilde(indk, inde) = bfminval
                enddo
            enddo

            dist = maxval(dabs(sol%btilde - tbtilde))
            sol%btilde = tbtilde
            sol%gwb = bfwmat

            if (iter == 1 .and. show_minSavingPolicy) then
                ! print on terminal
                write(*, '(a4, 3(a20))') 'iter', 'dist', 'bwmin', 'bwmax'
                ! write the same content into the variable sep
                write(sep, '(a4, 3(a20))') 'iter', 'dist', 'bwmin', 'bwmax'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif

            if (show_minSavingPolicy) then
            ! if (show_valueiter) then
                write(*, '(I4, 3(ES20.6))') iter, dist, minval(sol%btilde), maxval(sol%btilde)
            endif

        enddo

        do inde = 1, enum, 1
            do indk = 1, knum, 1
                kval = kgrid(indk)
                do indbk = 1, bknum, 1
                    bval = kval * bkgrid(indbk)
                    if (sol%btilde(indk, inde) >= bval) then
                        sol%wtrue(indbk, indk, inde) = .false.
                    endif
                enddo
            enddo
        enddo

    end subroutine minSavingPolicy

    subroutine wvalueiter(sol, conf)
        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: indk, inde, indef, indbk
        integer(ik) :: iter
        integer(ik) :: bkidx

        real(rk), dimension(:, :, :), allocatable :: tw, ew
        real(rk), dimension(:, :), allocatable :: tgwk
        real(rk) :: dist, distw, distgwk
        real(rk) :: kwdnstar, ewdnstar, kwupstar, ewupstar, kstar, wstar, kstay
        real(rk) :: epsval, kval, bkval, bval, nval, yval
        real(rk) :: bkw

        allocate(tw(bknum, knum, enum), source = 0.0_rk)
        allocate(tgwk(knum, enum), source = 0.0_rk)
        allocate(ew(bknum, knum, enum), source = 0.0_rk)

        write(*, *) "value function iteration: W-type (unconstrained)"

        iter = 0_ik
        dist = 2.0_rk*wTol

        call linear_interpolation(bkgrid, bknum, 0.0_rk, bkidx, bkw)
        do while (dist > wTol .AND. iter <= maxwiter)

            iter = iter + 1

            ! ----------------------- !
            ! conditional expectation !
            ! ----------------------- !

            do inde = 1, enum, 1
                do indk = 1, knum, 1
                    do indbk = 1, bknum, 1
                        ew(indbk, indk, inde) = &
                            dot_product(pie(inde, :), sol%w(indbk, indk, :))
                    enddo
                enddo
            enddo

            ! ------------------------------- !
            ! solve efficient unit of capital !
            ! ------------------------------- !

            do inde = 1, enum, 1
                epsval = egrid(inde)
                conf%ewbk = ew(:, :, inde)
                conf%ewk = conf%ewbk(bkidx, :)*bkw + conf%ewbk(bkidx+1_ik, :)*(1.0_rk - bkw)

                ! For *any* level of current capital stock kval = knotsk(indk),
                ! find kwupstar*, the *upward*-target capital given epsilon,
                ! and the analogous downward-investment target, kwdnstar.
                ! call cpu_time(t0)
                call kwdnTarget(kwdnstar, ewdnstar, conf)
                ! call cpu_time(t1)
                ! write(*, *) "wdn: ", t1 - t0
                ! call cpu_time(t0)
                call kwupTarget(kwupstar, ewupstar, conf)
                ! call cpu_time(t1)
                ! write(*, *) "wup: ", t1 - t0
                sol%kwupvec(inde) = kwupstar; sol%ewupvec(inde) = ewupstar
                sol%kwdnvec(inde) = kwdnstar; sol%ewdnvec(inde) = ewdnstar
                do indk = 1, knum, 1
                    kval = kgrid(indk)
                    kstay = (1.0_rk - delta)*kval
                    nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))
                    yval = conf%zval * epsval * kval**alpha * nval**nu
                    call kwrule(wstar, kstar, yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar, conf)
                    tgwk(indk, inde) = kstar
                    do indbk = 1, bknum, 1
                        bkval = bkgrid(indbk)
                        bval = bkval*kval
                        tw(indbk, indk, inde) = wstar - conf%pval * bval
                    enddo
                enddo
            enddo

            distw = maxval(dabs(sol%w - tw))
            distgwk = maxval(dabs(sol%gwk - tgwk))
            dist = dmax1(distw, distgwk)

            sol%w = tw
            sol%gwk = tgwk

            if (iter == 1 .and. show_valueiter) then
                ! print on terminal
                write(*, '(a4, 4(a20))') 'iter', 'distw', 'distgwk', 'kmin', 'kmax'
                ! write the same content into the variable sep
                write(sep, '(a4, 4(a20))') 'iter', 'distw', 'distgwk', 'kmin', 'kmax'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif

            if (show_valueiter) then
            ! if (show_valueiter) then
                write(*, '(I4, 4(ES20.6))') iter, distw, distgwk, minval(sol%gwk), maxval(sol%gwk)
            endif


        enddo

    end subroutine wvalueiter

end module innerloop
