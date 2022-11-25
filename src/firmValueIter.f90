module firmValueIter

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use parameters
    use optMod
    implicit none

contains

    subroutine vvalueiter(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: iter
        integer(ik) :: indk, inde, indef, indbk
        integer(ik) :: choiceik
        real(rk) :: dist, distv, distgvk, distgvb
        real(rk) :: kwdnstar, ewdnstar, kwupstar, ewupstar
        real(rk) :: kstar, vstar, wstar, kstay
        real(rk) :: epsval, kval, bkval, bval, nval, yval
        real(rk) :: kvfval, bvfval, vval
        real(rk) :: bprimemax, bthreshold
        real(rk), dimension(:, :, :), allocatable :: ew, ev, tv, tgvk, tgvb, tgvbk
        logical :: isKUp

        allocate(ev(bknum, knum, enum), source = 0.0_rk)
        allocate(ew(bknum, knum, enum), source = 0.0_rk)
        allocate(tv(bknum, knum, enum), source = 0.0_rk)
        allocate(tgvk(bknum, knum, enum), source = 0.0_rk)
        allocate(tgvb(bknum, knum, enum), source = 0.0_rk)
        allocate(tgvbk(bknum, knum, enum), source = 0.0_rk)

        iter = 0_ik
        dist = 2.0_rk*vTol

        ! --------------- !
        ! DEBUG
        ! --------------- !
        ! iter = maxviter !
        ! --------------- !

        ! ------------------------ !
        ! initial V-value function !
        ! ------------------------ !
        sol%v = sol%w

        write(*, *) "value function iteration: V-type (constrained)"


        mainwhile: do while (dist > vTol .and. iter <= maxviter )

            ! iter = maxviter
            iter = iter + 1

            ! ----------------------- !
            ! conditional expectation !
            ! ----------------------- !

            do inde = 1, enum, 1
                do indk = 1, knum, 1
                    do indbk = 1, bknum, 1
                        ev(indbk, indk, inde) = &
                            dot_product(pie(inde, :), sol%v(indbk, indk, :))
                        ew(indbk, indk, inde) = &
                            dot_product(pie(inde, :), sol%w(indbk, indk, :))
                    enddo
                enddo
            enddo


            ! --------------------------------------------- !
            ! value function iteration for constrained firm !
            ! --------------------------------------------- !

            epsloop: do inde = 1, enum, 1
                epsval = egrid(inde)
                conf%evbk = ev(:, :, inde)
                ! conf%ewk = ew(bkidx0, :, inde)*bkw0 + ew(bkidx0+1_ik, :, inde)*(1.0_rk - bkw0)
                ! call gss(ewdnstar, kwdnstar, kwdnGSSObj, LB = kgrid(1), UB = kgrid(knum), func_data = conf)
                ! call gss(ewupstar, kwupstar, kwupGSSObj, LB = kgrid(1), UB = kgrid(knum), func_data = conf)

                kloop: do indk = 1, knum, 1
                    kval = kgrid(indk)
                    kstay = (1.0_rk - delta)*kval
                    call debtThreshold(nval, yval, bprimemax, bthreshold, kval, epsval, conf)
                    bkloop: do indbk = 1, bknum, 1
                        bkval = bkgrid(indbk)
                        bval = bkval*kval
                        conf%xuval = yval - conf%wval*nval - bval + conf%Qbuy*kstay
                        conf%xdval = yval - conf%wval*nval - bval + conf%qsell*kstay
                        if (sol%wtrue(indbk, indk, inde)) then
                            ! call kwrule(wstar, kstar, isKUp, yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar, conf)
                            ! tv(indbk, indk, inde) = wstar - conf%pval * bval
                            ! tgvk(indbk, indk, inde) = kstar
                            ! tgvb(indbk, indk, inde) = sol%gwb(indk, inde)
                            ! tgvbk(indbk, indk, inde) = sol%gwb(indk, inde) / kval
                            tv(indbk, indk, inde) = sol%w(indbk, indk, inde)
                            tgvk(indbk, indk, inde) = sol%gwk(indk, inde)
                            tgvb(indbk, indk, inde) = sol%gwb(indk, inde)
                            tgvbk(indbk, indk, inde) = sol%gwb(indk, inde) / kval
                        else
                            call kvrule(bvfval, kvfval, vval, choiceik, bprimemax, bthreshold, kstay, bval, conf)
                            tv(indbk, indk, inde) = vval
                            tgvk(indbk, indk, inde) = kvfval
                            tgvb(indbk, indk, inde) = bvfval
                            tgvbk(indbk, indk, inde) = bvfval / kval
                        endif
                    enddo bkloop
                enddo kloop
            enddo epsloop

            distv = maxval(dabs(sol%v - tv))
            distgvk = maxval(dabs(sol%gvk - tgvk))
            distgvb = maxval(dabs(sol%gvb - tgvb))
            ! dist = dmax1(distv, distgvk, distgvb)
            dist = distv

            sol%v = tv
            sol%gvk = tgvk
            sol%gvb = tgvb
            sol%gvbk = tgvbk

            if (iter == 1 .and. show_vvalueiter) then
                ! print on terminal
                write(*, '(a4, 8(a20))') 'iter', 'distv', 'distgvk', 'distgvb', 'kmin', 'kmax', 'bkmin', 'bkmax'
                ! write the same content into the variable sep
                write(sep, '(a4, 8(a20))') 'iter', 'distv', 'distgvk', 'distgvb', 'kmin', 'kmax', 'bkmin', 'bkmax'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif

            if (show_vvalueiter) then
                write(*, '(I4, 8(ES20.6))') iter, distv, distgvk, distgvb, &
                                            minval(sol%gvk), maxval(sol%gvk), &
                                            minval(sol%gvbk), maxval(sol%gvbk)
            endif

        enddo mainwhile

    end subroutine vvalueiter

    subroutine minSavingPolicy(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf

        integer(ik) :: indk, inde, indef, indbk
        integer(ik) :: iter
        integer(ik) :: kidx
        real(rk) :: dist, kw, kstay, bprimemax, bthreshold
        real(rk) :: epsval, nval, yval, kval, kfval, bfwval, bminval, Jval, bval
        integer(ik), dimension(:, :), allocatable :: kidxmat
        real(rk), dimension(:), allocatable :: bfw
        real(rk), dimension(:, :), allocatable :: tbtilde, kwmat, bfwmat, arraybkbounds

        allocate(bfw(enum), source = 0.0_rk)
        allocate(tbtilde(knum, enum), source = 0.0_rk)
        allocate(kidxmat(knum, enum), source = 0_ik)
        allocate(kwmat(knum, enum), source = 0.0_rk)
        allocate(bfwmat(knum, enum), source = 0.0_rk)
        allocate(arraybkbounds(2, knum), source = 0.0_rk)

        write(*, *) "minimum saving policy for unconstrained firm (W-type)"

        ! ----------------------------------------------------- !
        ! calculate location and weight on decision on capital  !
        ! ----------------------------------------------------- !

        do inde = 1, enum, 1
            do indk = 1, knum, 1
                kfval = sol%gwk(indk, inde)
                kidx = gridlookup(kgrid, knum, kfval)
                kw = gridweight(kgrid, knum, kfval, kidx)
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

                    call debtThreshold(nval, yval, bprimemax, bthreshold, kval, epsval, conf)

                    do indef = 1, enum, 1
                        bfw(indef) = kw * sol%btilde(kidx, indef) + &
                                (1.0_rk - kw) * sol%btilde(kidx+1_rk, indef)
                    enddo

                    bfwval = minval(bfw, mask = pie(inde, :) > 0.0_rk)
                    bfwmat(indk, inde) = min(bfwval, bprimemax)

                    ! nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))
                    ! yval = conf%zval * epsval * kval**alpha * nval**nu
                    if (kfval >= kstay) then
                        Jval = conf%Qbuy
                    else
                        Jval = conf%qsell
                    endif
                    bminval = yval - conf%wval * nval + &
                        conf%qbval*dmin1(bfwval, zeta*kval) - &
                        Jval*(kfval - kstay)

                    tbtilde(indk, inde) = bminval
                enddo

                arraybkbounds(1, inde) = minval(tbtilde(:, inde) / kgrid)
                arraybkbounds(2, inde) = maxval(tbtilde(:, inde) / kgrid)
            enddo

            dist = maxval(dabs(sol%btilde - tbtilde))
            sol%btilde = tbtilde
            sol%gwb = bfwmat


            if (iter == 1 .and. show_minSavingPolicy) then
                ! print on terminal
                write(*, '(a4, 5(a20))') 'iter', 'dist', 'bwmin', 'bwmax', 'bkmin', 'bkmax'
                ! write the same content into the variable sep
                write(sep, '(a4, 5(a20))') 'iter', 'dist', 'bwmin', 'bwmax', 'bkmin', 'bkmax'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif

            if (show_minSavingPolicy) then
                write(*, '(I4, 5(ES20.6))') iter, dist, minval(sol%btilde), maxval(sol%btilde), &
                    minval(arraybkbounds(1, :)), maxval(arraybkbounds(2, :))
            endif

        enddo

        do inde = 1, enum, 1
            do indk = 1, knum, 1
                kval = kgrid(indk)
                do indbk = 1, bknum, 1
                    bval = kval * bkgrid(indbk)
                    if (sol%btilde(indk, inde) - bval >= 0.0_rk) then
                        ! wtrue is .false. by default
                        sol%wtrue(indbk, indk, inde) = .true.
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

        real(rk), dimension(:, :, :), allocatable :: tw, ew
        real(rk), dimension(:, :), allocatable :: tgwk
        real(rk) :: dist, distw, distgwk
        real(rk) :: kwdnstar, ewdnstar, kwupstar, ewupstar, kstar, wstar, kstay
        real(rk) :: epsval, kval, bkval, bval, nval, yval

        logical :: isKUp

        allocate(tw(bknum, knum, enum), source = 0.0_rk)
        allocate(tgwk(knum, enum), source = 0.0_rk)
        allocate(ew(bknum, knum, enum), source = 0.0_rk)

        write(*, *) "value function iteration: W-type (unconstrained)"

        iter = 0_ik
        dist = 2.0_rk*wTol

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
            ! !$omp end parallel do

            ! ------------------------------- !
            ! solve efficient unit of capital !
            ! ------------------------------- !

            ! !$omp parallel do private(inde, epsval, conf, ewdnstar, kwdnstar, ewupstar, kwupstar), &
            ! !$omp& private(indk, kval, kstay, nval, yval, wstar, kstar, isKUp, indbk, bval)
            do inde = 1, enum, 1
                epsval = egrid(inde)
                conf%ewk = ew(bkidx0, :, inde)*bkw0 + ew(bkidx0+1_ik, :, inde)*(1.0_rk - bkw0)

                ! For *any* level of current capital stock kval = knotsk(indk),
                ! find kwupstar*, the *upward*-target capital given epsilon,
                ! and the analogous downward-investment target, kwdnstar.

                call gss(ewdnstar, kwdnstar, kwdnGSSObj, LB = kgrid(1), UB = kgrid(knum), func_data = conf)
                call gss(ewupstar, kwupstar, kwupGSSObj, LB = kgrid(1), UB = kgrid(knum), func_data = conf)
                sol%kwupvec(inde) = kwupstar; sol%ewupvec(inde) = ewupstar
                sol%kwdnvec(inde) = kwdnstar; sol%ewdnvec(inde) = ewdnstar
                ! !$omp parallel do private(indk, kval, kstay, nval, yval, wstar, kstar, isKUp)
                do indk = 1, knum, 1
                    kval = kgrid(indk)
                    kstay = (1.0_rk - delta)*kval
                    nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))
                    yval = conf%zval * epsval * kval**alpha * nval**nu
                    call kwrule(wstar, kstar, isKUp, yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar, conf)
                    tgwk(indk, inde) = kstar

                    ! !$omp parallel do private(indbk, bval)
                    do indbk = 1, bknum, 1
                        bval = bkgrid(indbk)*kval
                        tw(indbk, indk, inde) = wstar - conf%pval * bval
                    enddo
                enddo
            enddo

            distw = maxval(dabs(sol%w - tw))
            distgwk = maxval(dabs(sol%gwk - tgwk))
            ! dist = dmax1(distw, distgwk)
            dist = distw

            sol%w = tw
            sol%gwk = tgwk

            if (iter == 1 .and. show_wvalueiter) then
                ! print on terminal
                write(*, '(a4, 4(a20))') 'iter', 'distw', 'distgwk', 'kmin', 'kmax'
                ! write the same content into the variable sep
                write(sep, '(a4, 4(a20))') 'iter', 'distw', 'distgwk', 'kmin', 'kmax'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif

            if (show_wvalueiter) then
                write(*, '(I4, 4(ES20.6))') iter, distw, distgwk, minval(sol%gwk), maxval(sol%gwk)
            endif


        enddo

    end subroutine wvalueiter

end module firmValueIter
