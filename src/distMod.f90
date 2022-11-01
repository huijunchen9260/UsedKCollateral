module distMod

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use parameters
    use optMod
    use goldenSectionSearch
    use nlopt_wrap
    use nlopt_enum
    implicit none

contains

    subroutine vDistDecision(dividend, isKUp, isWType, kvfval, bvfval, choiceik, &
            kwdnstar, ewdnstar, kwupstar, ewupstar, bprimemax, bthreshold, &
            kval, bval, nval, yval, inde, sol, conf)

        real(rk), intent(in) :: kwdnstar, ewdnstar, kwupstar, ewupstar, bprimemax, bthreshold
        real(rk), intent(in) :: kval, bval, nval, yval
        integer(ik), intent(in) :: inde
        real(rk), intent(out) :: dividend, kvfval, bvfval
        integer(ik), intent(out) :: choiceik
        logical, intent(out) :: isWType, isKUp
        type(configurations), intent(inout) :: conf
        type(solutions), intent(inout) :: sol
        integer(ik) :: kidx, indef
        real(rk) :: maxb, kw, bfminval, vval, kstar, wstar
        real(rk), dimension(:), allocatable :: bfw

        allocate(bfw(enum), source = 0.0_rk)

        call kwrule(wstar, kstar, isKUp, yval, nval, (1.0_rk - delta)*kval, kwupstar, ewupstar, kwdnstar, ewdnstar, conf)

        ! --------------------------------------------------------- !
        ! linear interp minimum saving policy for distribution grid !
        ! --------------------------------------------------------- !
        kidx = gridlookup(kgrid, knum, kval)
        kw = gridweight(kgrid, knum, kval, kidx)
        maxb = sol%btilde(kidx, inde)*kw + sol%btilde(kidx+1, inde)*(1.0_rk - kw)
        do indef = 1, enum, 1
            bfw(indef) = sol%gwb(kidx, indef)*kw + sol%gwb(kidx+1, indef)*(1.0_rk-kw)
        enddo
        bfminval = minval(bfw, mask = pie(inde, :) > 0.0_rk)
        bfminval = min(bfminval, bprimemax)

        if (isKUp) then
            dividend = conf%xuval + conf%Qbuy*kstar
        else
            dividend = conf%xdval + conf%qsell*kstar
        endif

        ! ---------------------------------------------------------------- !
        ! A. Find kf implied by w-rule (unconstrained by borrowing limits) !
        ! ---------------------------------------------------------------- !
        isWType = .false.
        if (bval <= maxb) then
            isWType = .true.
            kvfval = kstar
            bvfval = bfminval
            dividend = (1.0_rk - exitprob) * (dividend + conf%qbval*bvfval) &
                + exitprob * conf%xdval

            ! choiceik entries for a wtype firm: 1:kstarup, 2:kstardn, 5:noadj
            if (abs(kvfval - kwupstar) < tol) then
                choiceik = 1_ik
            elseif (abs(kvfval - kwdnstar) < tol) then
                choiceik = 2_ik
            else
                choiceik = 5_ik
            endif

        endif


        ! -------------------------------------------------------- !
        ! B. If above check reveals that the firm is not a w-type, !
        !    then we retrieve its decisions as v-type              !
        ! -------------------------------------------------------- !
        if (.not. isWType) then
            call kvrule(bvfval, kvfval, vval, choiceik, bprimemax, bthreshold, (1.0_rk - delta)*kval, bval, conf)
            isKUp = merge(.true., .false., kvfval >= (1.0_rk - delta)*kval)
            dividend = (1.0_rk - exitprob)*0.0_rk + exitprob*conf%xdval

        endif

    end subroutine vDistDecision

end module distMod
