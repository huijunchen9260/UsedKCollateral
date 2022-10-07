module optMod

    use iso_Fortran_env, only: rk => real64, ik => int32
    use ogpf
    use numerics
    use io
    use parameters
    use gss
    use nlopt_wrap
    use nlopt_enum
    implicit none

contains

    subroutine kvrule(bvfval, kvfval, vval, bprimemax, bthreshold, kstay, bval, conf)
        type(configurations), intent(inout) :: conf
        integer(ik) :: kidx, bkidx
        real(rk), intent(in) :: bprimemax, bthreshold, kstay, bval
        real(rk), intent(out) :: bvfval, kvfval, vval
        real(rk) :: kupmax, kdnmax, bkratio
        real(rk) :: bvupval, kvupval, evupval, bvdnval, kvdnval, evdnval
        real(rk) :: kw, bkw, evval

        if (conf%xdval + conf%qbval*bprimemax < kgrid(1)*conf%qbval*bprimemax) then
            kvfval = kgrid(1)
            bvfval = bprimemax
            bkratio = bvfval / kvfval
            call linear_interpolation(kgrid, knum, kvdnval, kidx, kw)
            call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
            evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
            evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
            evval = beta*evval
            vval = (1.0_rk - exitprob)*evval + exitprob*conf%pval*conf%xdval
        endif

        if (bval <= bthreshold) then
            kupmax = ( conf%qbval*bprimemax + conf%xuval ) / conf%Qbuy
            call kvup(bvupval, kvupval, evupval, conf, kstay, kupmax)
        endif

        kdnmax = ( conf%qbval*bprimemax + conf%xdval ) / conf%qsell
        call kvdn(bvdnval, kvdnval, evdnval, conf, kstay, kdnmax)

        if (bval > bthreshold) then
            kvfval = kvdnval
            bvfval = bvdnval
            vval = (1.0_rk - exitprob)*evdnval + exitprob*conf%pval*conf%xdval
        else
            if (evupval >= evdnval) then
                kvfval = kvupval
                bvfval = bvupval
                vval = (1.0_rk - exitprob)*evupval + exitprob*conf%pval*conf%xdval
            else
                kvfval = kvdnval
                bvfval = bvdnval
                vval = (1.0_rk - exitprob)*evdnval + exitprob*conf%pval*conf%xdval
            endif
        endif



    end subroutine kvrule

    subroutine kvdn(bvdnval, kvdnval, evdnval, conf, kstay, kdnmax)
        type(configurations), intent(inout) :: conf
        type(nlopt_opt) :: opt
        real(rk), intent(in) :: kstay, kdnmax
        real(rk), intent(out) :: bvdnval, kvdnval, evdnval
        real(rk), dimension(1) :: guess, UB, LB
        real(rk) :: fmax, bkratio, kw, bkw, evval
        integer :: stat
        integer(ik) :: kidx, bkidx

        if ( dmin1(kdnmax, kstay) <= kgrid(1) ) then
            kvdnval = kgrid(1)
            bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
            bkratio = bvdnval / kvdnval
            call linear_interpolation(kgrid, knum, kvdnval, kidx, kw)
            call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
            evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
            evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
            evdnval = beta*evval
            return
        endif


        LB = kgrid(1)
        UB = dmin1(kdnmax, kgrid(knum), kstay)

        guess = (LB + UB) / 2

        ! remember that the last number is in int4 but not 8
        call create(opt, algorithm_from_string("LN_SBPLX"), 1)
        call opt%set_lower_bounds(LB)
        call opt%set_upper_bounds(UB)
        associate( f => nlopt_func(kvdnObj, conf) )
            call opt%set_xtol_rel(tol)
            call opt%set_max_objective(f)
            call opt%optimize(guess, fmax, stat)
        end associate
        call destroy(opt)

        if (stat < NLOPT_SUCCESS) then
            write (*, "(A,I5)") "kvdn: NLopt failed with code ", stat
        end if

        kvdnval = guess(1)
        bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
        evdnval = fmax

    end subroutine kvdn

    function kvdnObj(guess, gradient, func_data) result(f)
        real(rk), dimension(:), intent(in) :: guess
        real(rk), dimension(:), intent(inout), optional :: gradient
        class(*), intent(in), optional :: func_data
        type(configurations) :: conf
        integer(ik) :: kidx, bkidx
        real(rk) :: f, kw, bkw, evval, kfval, bfval, bkratio

        select type(func_data)
            type is (configurations)
            conf = func_data
        end select

        IF (PRESENT(gradient)) THEN
            ! Why?!
        END IF

        kfval = guess(1)
        bfval = ( conf%qsell*kfval - conf%xdval ) / conf%qbval
        bkratio = bfval / kfval
        call linear_interpolation(kgrid, knum, kfval, kidx, kw)
        call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
        evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
        evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
        f = beta*evval

    end function kvdnObj

    subroutine kvup(bvupval, kvupval, evupval, conf, kstay, kupmax)
        type(configurations), intent(inout) :: conf
        type(nlopt_opt) :: opt
        real(rk), intent(in) :: kstay, kupmax
        real(rk), intent(out) :: bvupval, kvupval, evupval
        real(rk), dimension(1) :: guess, UB, LB
        real(rk) :: fmax, bkratio, kw, bkw, evval
        integer :: stat
        integer(ik) :: kidx, bkidx

        if (kupmax <= kstay) then
            kvupval = dmin1(kstay, kgrid(1))
            bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
            bkratio = bvupval / kvupval
            call linear_interpolation(kgrid, knum, kvupval, kidx, kw)
            call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
            evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
            evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
            evupval = beta*evval
            return
        endif

        LB = dmax1(kstay, kgrid(1))
        UB = dmin1(kupmax, kgrid(knum))

        guess = (LB + UB) / 2

        ! remember that the last number is in int4 but not 8
        call create(opt, algorithm_from_string("LN_SBPLX"), 1)
        call opt%set_lower_bounds(LB)
        call opt%set_upper_bounds(UB)
        associate( f => nlopt_func(kvupObj, conf) )
            call opt%set_xtol_rel(tol)
            call opt%set_max_objective(f)
            call opt%optimize(guess, fmax, stat)
        end associate
        call destroy(opt)

        if (stat < NLOPT_SUCCESS) then
            write (*, "(A,I5)") "kvup: NLopt failed with code ", stat
        end if

        kvupval = guess(1)
        bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
        evupval = fmax

    end subroutine kvup

    function kvupObj(guess, gradient, func_data) result(f)
        real(rk), dimension(:), intent(in) :: guess
        real(rk), dimension(:), intent(inout), optional :: gradient
        class(*), intent(in), optional :: func_data
        type(configurations) :: conf
        integer(ik) :: kidx, bkidx
        real(rk) :: f, kw, bkw, evval, kfval, bfval, bkratio

        select type(func_data)
            type is (configurations)
            conf = func_data
        end select

        IF (PRESENT(gradient)) THEN
            ! Why?!
        END IF

        kfval = guess(1)
        bfval = ( conf%Qbuy*kfval - conf%xuval ) / conf%qbval
        bkratio = bfval / kfval
        call linear_interpolation(kgrid, knum, kfval, kidx, kw)
        call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
        evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
        evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
        f = beta*evval

    end function kvupObj

    subroutine kwrule(wstar, kstar, yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar, conf)
        real(rk), intent(in) :: yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar
        type(configurations), intent(inout) :: conf
        real(rk), intent(out) :: wstar, kstar
        real(rk) :: kup, ewup, kdn, ewdn, wup, wdn, evval, kw
        real(rk) :: flowval
        integer(ik) :: kidx

        if (kwupstar >= kstay) then
            kup = kwupstar; ewup = ewupstar
        else
            kup = kstay
            call linear_interpolation(kgrid, knum, kstay, kidx, kw)
            evval = conf%ewk(kidx)*kw + conf%ewk(kidx+1_ik)*(1.0_rk - kw)
            ewup = -1.0_rk * conf%pval * conf%Qbuy * kstay + beta*evval
        endif

        if (kwdnstar <= kstay) then
            kdn = kwdnstar; ewdn = ewdnstar
        else
            kdn = kstay
            call linear_interpolation(kgrid, knum, kstay, kidx, kw)
            evval = conf%ewk(kidx)*kw + conf%ewk(kidx+1_ik)*(1.0_rk - kw)
            ewdn = -1.0_rk * conf%pval * conf%qsell * kstay + beta*evval
        endif

        flowval = conf%pval * (yval - conf%wval*nval)

        wup = conf%pval * conf%Qbuy * kstay + ewup
        wdn = conf%pval * conf%qsell * kstay + ewdn

        if (wup .ge. wdn) then
            ! considering exogenous firm exiting
            wstar = flowval + (1.0_rk - exitprob)*wup + exitprob * conf%pval * conf%qsell * kstay
            kstar = kup
        else
            ! considering exogenous firm exiting
            wstar = flowval + (1.0_rk - exitprob)*wdn + exitprob * conf%pval * conf%qsell * kstay
            kstar = kdn
        endif

    end subroutine kwrule

    subroutine kwdnTarget(kwdnstar, ewdnstar, conf)
        type(configurations), intent(in) :: conf
        real(rk), intent(out) :: kwdnstar, ewdnstar
        integer(ik) :: kidx, iter
        real(rk) :: a, b, c, d, z, fval, fc, fd
        real(rk) :: Jval, kfval, evval, kw, dist, gssTol

        iter = 0

        Jval = conf%qsell

        a = kgrid(1)
        b = kgrid(knum)
        c = a + rg*(b-a);
        d = a + (1-rg)*(b-a);

        ! initial c evaluation
        kfval = c
        fval = evaluate(conf, kfval, Jval)
        fc = -fval

        ! initial d evaluation
        kfval = d
        fval = evaluate(conf, kfval, Jval)
        fd = -fval

        gssTol = tol
        dist = 2.0_rk*gssTol

        do
            if (dist < gssTol) exit

            iter = iter + 1

            if (fc .ge. fd) then

                z = c + (1.0_rk-rg)*(b-c)
                ! case 1 [a c d b] <--- [c d z b]
                a = c
                c = d
                fc = fd
                d = z

                kfval = d
                fval = evaluate(conf, kfval, Jval)
                fd = -fval
            else
                z = a + rg*(d-a)
                ! case 2 [a c d b] <--- [a z c d]
                b = d
                d = c
                fd = fc
                c = z

                kfval = c
                fval = evaluate(conf, kfval, Jval)
                fc = -fval
            endif

            dist = b - a

            ! write (*, '((a, i3), 3(a, f9.6))') 'iter = ', iter, ', dist = ', dist, ', kfval = ', kfval, ', fval = ', fval

        enddo

        kwdnstar = kfval
        if (kfval >= 0.0_rk) then
            ewdnstar = fval
        else
            kfval = kgrid(1)
            fval = evaluate(conf, kfval, Jval)
            kwdnstar = kfval
            ewdnstar = fval
        endif

    end subroutine kwdnTarget

    subroutine kwupTarget(kwupstar, ewupstar, conf)
        type(configurations), intent(inout) :: conf
        real(rk), intent(out) :: kwupstar, ewupstar
        integer(ik) :: kidx, iter
        real(rk) :: a, b, c, d, z, fval, fc, fd
        real(rk) :: Jval, kfval, evval, kw, dist, gssTol

        iter = 0

        Jval = conf%Qbuy

        a = kgrid(1)
        b = kgrid(knum)
        c = a + rg*(b-a);
        d = a + (1-rg)*(b-a);

        ! initial c evaluation
        kfval = c
        fval = evaluate(conf, kfval, Jval)
        fc = -fval

        ! initial d evaluation
        kfval = d
        fval = evaluate(conf, kfval, Jval)
        fd = -fval

        gssTol = tol
        dist = 2.0_rk*gssTol

        do
            if (dist < gssTol) exit

            iter = iter + 1

            if (fc .ge. fd) then

                z = c + (1.0_rk-rg)*(b-c)
                ! case 1 [a c d b] <--- [c d z b]
                a = c
                c = d
                fc = fd
                d = z

                kfval = d
                fval = evaluate(conf, kfval, Jval)
                fd = -fval
            else
                z = a + rg*(d-a)
                ! case 2 [a c d b] <--- [a z c d]
                b = d
                d = c
                fd = fc
                c = z

                kfval = c
                fval = evaluate(conf, kfval, Jval)
                fc = -fval
            endif

            dist = b - a

            ! write (*, '((a, i3), 3(a, f9.6))') 'iter = ', iter, ', dist = ', dist, ', kfval = ', kfval, ', fval = ', fval

        enddo

        kwupstar = kfval
        ewupstar = fval
    end subroutine kwupTarget

    pure function evaluate(conf, kfval, Jval) result(fval)
        integer(ik) :: kidx
        real(rk) :: evvec(knum), kfval, fval, kw, evval, Jval
        type(configurations), intent(in) :: conf
        intent(in) :: kfval, Jval

        ! kidx = gridlookup(kgrid, knum, kfval)
        ! kw = gridweight(kgrid, knum, kfval, kidx)
        call linear_interpolation(kgrid, knum, kfval, kidx, kw)
        evval = conf%ewk(kidx)*kw + conf%ewk(kidx+1_ik)*(1.0_rk - kw)
        fval = -1.0_rk * conf%pval * Jval * kfval + beta*evval

    end function evaluate


    ! subroutine kwdnTarget(kwdnstar, ewdnstar, conf)
    !     type(configurations), intent(inout) :: conf
    !     real(rk), intent(out) :: kwdnstar, ewdnstar
    !     type(nlopt_opt) :: opt
    !     real(rk), dimension(1) :: guess
    !     real(rk) :: fmax
    !     integer :: stat

    !     guess = kgrid(knum/2)

    !     ! remember that the last number is in int4 but not 8
    !     call create(opt, algorithm_from_string("LN_SBPLX"), 1)
    !     ! call create(opt, algorithm_from_string("LN_NELDERMEAD"), 1)
    !     call opt%set_lower_bounds(kgrid(1))
    !     call opt%set_upper_bounds(kgrid(knum))
    !     associate( f => nlopt_func(kwdnObj, conf) )
    !         call opt%set_xtol_rel(tol)
    !         call opt%set_max_objective(f)
    !         call opt%optimize(guess, fmax, stat)
    !     end associate
    !     call destroy(opt)

    !     if (stat < NLOPT_SUCCESS) then
    !         write (*, "(A,I5)") "NLopt failed with code ", stat
    !     end if

    !     kwdnstar = guess(1)
    !     ewdnstar = fmax

    ! end subroutine kwdnTarget

    ! function kwdnObj(guess, gradient, func_data) result(f)
    !     real(rk), dimension(:), intent(in) :: guess
    !     real(rk), dimension(:), intent(inout), optional :: gradient
    !     class(*), intent(in), optional :: func_data
    !     type(configurations) :: conf
    !     integer(ik) :: kidx
    !     real(rk) :: f, kw, evval, kfval


    !     select type(func_data)
    !         type is (configurations)
    !         conf = func_data
    !     end select

    !     IF (PRESENT(gradient)) THEN
    !         ! Why?!
    !     END IF

    !     kfval = guess(1)

    !     call linear_interpolation(kgrid, knum, kfval, kidx, kw)
    !     ! kidx = gridlookup(kgrid, knum, kfval)
    !     ! kw = gridweight(kgrid, knum, kfval, kidx)
    !     evval = conf%ewk(kidx)*kw + conf%ewk(kidx+1_ik)*(1.0_rk - kw)

    !     f = -1.0_rk * conf%pval * conf%qsell * kfval + beta*evval

    ! end function kwdnObj

    ! subroutine kwupTarget(kwupstar, ewupstar, conf)
    !     type(configurations), intent(inout) :: conf
    !     real(rk), intent(out) :: kwupstar, ewupstar
    !     type(nlopt_opt) :: opt
    !     real(rk), dimension(1) :: guess
    !     real(rk) :: fmax
    !     integer :: stat

    !     guess = kgrid(knum/2)

    !     ! call create(opt, algorithm_from_string("LN_SBPLX"), int(knum, 4))

    !     ! remember that the last number is in int4 but not 8
    !     call create(opt, algorithm_from_string("LN_SBPLX"), 1)
    !     ! call create(opt, algorithm_from_string("LN_NELDERMEAD"), 1)
    !     call opt%set_lower_bounds(kgrid(1))
    !     call opt%set_upper_bounds(kgrid(knum))
    !     associate( f => nlopt_func(kwupObj, conf) )
    !         call opt%set_xtol_rel(tol)
    !         call opt%set_max_objective(f)
    !         call opt%optimize(guess, fmax, stat)
    !     end associate
    !     call destroy(opt)

    !     if (stat < NLOPT_SUCCESS) then
    !         write (*, "(A,I5)") "NLopt failed with code ", stat
    !     end if

    !     kwupstar = guess(1)
    !     ewupstar = fmax

    ! end subroutine kwupTarget

    ! function kwupObj(guess, gradient, func_data) result(f)
    !     real(rk), dimension(:), intent(in) :: guess
    !     real(rk), dimension(:), intent(inout), optional :: gradient
    !     class(*), intent(in), optional :: func_data
    !     type(configurations) :: conf
    !     integer(ik) :: kidx
    !     real(rk) :: f, kw, evval, kfval

    !     select type(func_data)
    !         type is (configurations)
    !         conf = func_data
    !     end select

    !     IF (PRESENT(gradient)) THEN
    !         ! Why?!
    !     END IF

    !     kfval = guess(1)

    !     call linear_interpolation(kgrid, knum, kfval, kidx, kw)
    !     evval = conf%ewk(kidx)*kw + conf%ewk(kidx+1_ik)*(1.0_rk - kw)

    !     f = -1.0_rk * conf%pval * conf%Qbuy * kfval + beta*evval

    ! end function kwupObj


end module optMod
