module optMod

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use parameters
    use goldenSectionSearch
    use nlopt_wrap
    use nlopt_enum
    use linear_interpolation_module
    implicit none

contains

    subroutine kvrule(bvfval, kvfval, vval, bprimemax, bthreshold, kstay, bval, conf)
        type(configurations), intent(inout) :: conf
        integer(ik) :: kidx, bkidx
        real(rk), intent(in) :: bprimemax, bthreshold, kstay, bval
        real(rk), intent(out) :: bvfval, kvfval, vval
        real(rk) :: kupmax, kdnmax, bkratio
        real(rk) :: bvupval, kvupval, evupval, bvdnval, kvdnval, evdnval, bvalsolp
        real(rk) :: kw, bkw, evval, LB, UB

        ! Check for feasibility if using very large zeta.
        if (conf%xdval + conf%qbval*bprimemax < kgrid(1)*conf%qsell) then
            bvalsolp = -kgrid(1)*conf%qsell + conf%xdval + bval + conf%qbval*bprimemax
            kvfval = kgrid(1)
            bvfval = bprimemax
            bkratio = bvfval / kvfval

            kidx = gridlookup(kgrid, knum, kvfval)
            kw = gridweight(kgrid, knum, kvfval, kidx)
            bkidx = gridlookup(bkgrid, bknum, bkratio)
            bkw = gridweight(bkgrid, bknum, bkratio, bkidx)
            evval = bkw * ( conf%evbk(bkidx, kidx)*kw + conf%evbk(bkidx, kidx+1_ik)*(1.0_rk - kw) )
            evval = evval + (1.0_rk - bkw) * ( conf%evbk(bkidx+1_rk, kidx)*kw + conf%evbk(bkidx+1_rk, kidx+1_ik)*(1.0_rk - kw) )
            evval = beta*evval
            vval = (1.0_rk - exitprob)*evval + exitprob*conf%pval*conf%xdval
            return
        endif

        ! If firm can do upward-adjustment
        if (bval <= bthreshold) then
            kupmax = ( conf%qbval*bprimemax + conf%xuval ) / conf%Qbuy
            call kvup(bvupval, kvupval, evupval, conf, kstay, kupmax)
        endif

        kdnmax = ( conf%qbval*bprimemax + conf%xdval ) / conf%qsell
        call kvdn(bvdnval, kvdnval, evdnval, conf, kstay, kdnmax)


        if (bval > bthreshold) then
            kvfval = kvdnval
            bvfval = bvdnval
            vval = evdnval
        else
            if (evupval >= evdnval) then
                kvfval = kvupval
                bvfval = bvupval
                vval = evupval
            else
                kvfval = kvdnval
                bvfval = bvdnval
                vval = evdnval
            endif
        endif

        vval = (1.0_rk - exitprob)*vval + exitprob*conf%pval*conf%xdval

    end subroutine kvrule

    subroutine kvdn(bvdnval, kvdnval, evdnval, conf, kstay, kdnmax)
        type(configurations), intent(inout) :: conf
        real(rk), intent(in) :: kstay, kdnmax
        real(rk), intent(out) :: bvdnval, kvdnval, evdnval
        real(rk) :: UB, LB
        real(rk) :: fmax, bkratio, kw, bkw, evval
        integer :: stat
        integer(ik) :: kidx, bkidx
        real(rk) :: ev00, ev01, ev10, ev11

        if ( dmin1(kdnmax, kstay) <= kgrid(1) ) then
            kvdnval = kgrid(1)
            bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
            bkratio = bvdnval / kvdnval

            kidx = gridlookup(kgrid, knum, kvdnval)
            kw = gridweight(kgrid, knum, kvdnval, kidx)
            bkidx = gridlookup(bkgrid, bknum, bkratio)
            bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

            ev00 = conf%evbk(bkidx, kidx)
            ev10 = conf%evbk(bkidx+1_ik, kidx)
            ev01 = conf%evbk(bkidx, kidx+1_ik)
            ev11 = conf%evbk(bkidx+1_ik, kidx+1_ik)

            evval = bkw * ( ev00*kw + ev01*(1.0_rk - kw) )
            evval = evval + (1.0_rk - bkw) * ( ev10*kw + ev11*(1.0_rk - kw) )
            evdnval = beta*evval
            return
        endif


        LB = kgrid(1)
        UB = dmin1(kdnmax, kgrid(knum), kstay)

        call gss(evdnval, kvdnval, kvdnGSSObj, LB = LB, UB = UB, func_data = conf)
        bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval


    end subroutine kvdn

    subroutine kvup(bvupval, kvupval, evupval, conf, kstay, kupmax)
        type(configurations), intent(inout) :: conf
        real(rk), intent(in) :: kstay, kupmax
        real(rk), intent(out) :: bvupval, kvupval, evupval
        real(rk) :: UB, LB
        real(rk) :: fmax, bkratio, kw, bkw, evval
        integer :: stat
        integer(ik) :: kidx, bkidx
        real(rk) :: ev00, ev01, ev10, ev11

        if (kupmax <= kstay) then
            kvupval = dmin1(kstay, kgrid(1))
            bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
            bkratio = bvupval / kvupval
            kidx = gridlookup(kgrid, knum, kvupval)
            kw = gridweight(kgrid, knum, kvupval, kidx)
            bkidx = gridlookup(bkgrid, bknum, bkratio)
            bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

            ev00 = conf%evbk(bkidx, kidx)
            ev10 = conf%evbk(bkidx+1_ik, kidx)
            ev01 = conf%evbk(bkidx, kidx+1_ik)
            ev11 = conf%evbk(bkidx+1_ik, kidx+1_ik)

            evval = bkw * ( ev00*kw + ev01*(1.0_rk - kw) )
            evval = evval + (1.0_rk - bkw) * ( ev10*kw + ev11*(1.0_rk - kw) )

            evupval = beta*evval
            return
        endif

        LB = dmax1(kstay, kgrid(1))
        UB = dmin1(kupmax, kgrid(knum))

        call gss(evupval, kvupval, kvupGSSObj, LB = LB, UB = UB, func_data = conf)
        bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval

    end subroutine kvup

    function kvdnGSSObj(kfval, func_data) result(fval)
        real(rk), intent(in) :: kfval
        class(*), intent(in), optional :: func_data
        integer(ik) :: kidx, bkidx
        real(rk) :: kw, bkw, bfval, bkratio
        real(rk) :: qsell, xdval, qbval, fval
        real(rk) :: ev00, ev10, ev01, ev11, evval

        select type(func_data)
            type is (configurations)
                qsell = func_data%qsell
                xdval = func_data%xdval
                qbval = func_data%qbval
        end select

        bfval = (qsell*kfval - xdval) / qbval
        bkratio = bfval / kfval


        kidx = gridlookup(kgrid, knum, kfval)
        kw = gridweight(kgrid, knum, kfval, kidx)
        bkidx = gridlookup(bkgrid, bknum, bkratio)
        bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

        select type(func_data)
            type is (configurations)
                ev00 = func_data%evbk(bkidx, kidx)
                ev10 = func_data%evbk(bkidx+1_ik, kidx)
                ev01 = func_data%evbk(bkidx, kidx+1_ik)
                ev11 = func_data%evbk(bkidx+1_ik, kidx+1_ik)
        end select

        evval = bkw * ( ev00*kw + ev01*(1.0_rk - kw) )
        evval = evval + (1.0_rk - bkw) * ( ev10*kw + ev11*(1.0_rk - kw) )
        fval = beta*evval

    end function kvdnGSSObj

    function kvupGSSObj(kfval, func_data) result(fval)
        real(rk), intent(in) :: kfval
        class(*), intent(in), optional :: func_data
        integer(ik) :: kidx, bkidx
        real(rk) :: kw, bkw, bfval, bkratio
        real(rk) :: Qbuy, xuval, qbval, fval
        real(rk) :: ev00, ev10, ev01, ev11, evval

        select type(func_data)
            type is (configurations)
                Qbuy = func_data%Qbuy
                xuval = func_data%xuval
                qbval = func_data%qbval
        end select

        bfval = (Qbuy*kfval - xuval) / qbval
        bkratio = bfval / kfval


        kidx = gridlookup(kgrid, knum, kfval)
        kw = gridweight(kgrid, knum, kfval, kidx)
        bkidx = gridlookup(bkgrid, bknum, bkratio)
        bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

        select type(func_data)
            type is (configurations)
                ev00 = func_data%evbk(bkidx, kidx)
                ev10 = func_data%evbk(bkidx+1_ik, kidx)
                ev01 = func_data%evbk(bkidx, kidx+1_ik)
                ev11 = func_data%evbk(bkidx+1_ik, kidx+1_ik)
        end select
        evval = bkw * ( ev00*kw + ev01*(1.0_rk - kw) )
        evval = evval + (1.0_rk - bkw) * ( ev10*kw + ev11*(1.0_rk - kw) )
        fval = beta*evval

    end function kvupGSSObj

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
            kidx = gridlookup(kgrid, knum, kstay)
            kw = gridweight(kgrid, knum, kstay, kidx)
            evval = conf%ewk(kidx)*kw + conf%ewk(kidx+1_ik)*(1.0_rk - kw)
            ewup = -1.0_rk * conf%pval * conf%Qbuy * kstay + beta*evval
        endif

        if (kwdnstar <= kstay) then
            kdn = kwdnstar; ewdn = ewdnstar
        else
            kdn = kstay
            kidx = gridlookup(kgrid, knum, kstay)
            kw = gridweight(kgrid, knum, kstay, kidx)
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

    pure function kwdnGSSObj(kfval, func_data) result(fval)
        real(rk), intent(in) :: kfval
        class(*), intent(in), optional :: func_data
        integer(ik) :: kidx
        real(rk) :: fval, evval, kw, pval, Jval
        real(rk) :: ewkLeft, ewkRight

        kidx = gridlookup(kgrid, knum, kfval)
        kw = gridweight(kgrid, knum, kfval, kidx)

        select type(func_data)
            type is (configurations)
            ewkLeft = func_data%ewk(kidx)
            ewkRight = func_data%ewk(kidx + 1_ik)
            pval = func_data%pval
            Jval = func_data%qsell
        end select

        evval = ewkLeft*kw + ewkRight*(1.0_rk - kw)
        fval = -1.0_rk * pval * Jval * kfval + beta*evval

    end function kwdnGSSObj

    pure function kwupGSSObj(kfval, func_data) result(fval)
        real(rk), intent(in) :: kfval
        class(*), intent(in), optional :: func_data
        integer(ik) :: kidx
        real(rk) :: fval, evval, kw, pval, Jval
        real(rk) :: ewkLeft, ewkRight

        kidx = gridlookup(kgrid, knum, kfval)
        kw = gridweight(kgrid, knum, kfval, kidx)

        select type(func_data)
            type is (configurations)
            ewkLeft = func_data%ewk(kidx)
            ewkRight = func_data%ewk(kidx + 1_ik)
            pval = func_data%pval
            Jval = func_data%Qbuy
        end select

        evval = ewkLeft*kw + ewkRight*(1.0_rk - kw)
        fval = -1.0_rk * pval * Jval * kfval + beta*evval

    end function kwupGSSObj

end module optMod
