module calibrationStats

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use parameters
    use firmValueIter
    use firmDistribution
    implicit none

contains

    subroutine calistats(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: indbk, indk, inde
        ! big ratio
        real(rk) :: KY, IK, reall_share, qQratio, negInv_freq, negInv20_freq
        real(rk) :: ikw_mean, ikv_mean, ikw_std, ikv_std
        real(rk), dimension(:, :), allocatable :: negInvw, negInvw20, ikw, ikwstd
        real(rk), dimension(:, :, :), allocatable :: negInvv, negInvv20, ikv, ikvstd

        allocate(negInvw(muknum, enum), source = 0.0_rk)
        allocate(negInvw20(muknum, enum), source = 0.0_rk)
        allocate(ikw(muknum, enum), source = 0.0_rk)
        allocate(ikwstd(muknum, enum), source = 0.0_rk)

        allocate(negInvv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(negInvv20(mubknum, muknum, enum), source = 0.0_rk)
        allocate(ikv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(ikvstd(mubknum, muknum, enum), source = 0.0_rk)

        KY = sol%kagg / sol%yagg
        IK = sol%invagg / sol%kagg
        qQratio = conf%qsell / conf%Qbuy
        reall_share = conf%qsell*sol%divagg / ( conf%Qbuy*sol%invagg )

        negInvw = merge(1.0_rk, 0.0_rk, sol%divgridw > 0.0_rk)
        negInvv = merge(1.0_rk, 0.0_rk, sol%divgridv > 0.0_rk)
        negInv_freq = sum(negInvw*sol%muw) + sum(negInvv*sol%muv)

        do inde = 1, enum, 1
            do indk = 1, muknum, 1
                if (sol%divgridw(indk, inde) / mukgrid(indk) > 0.2_rk) then
                    negInvw20(indk, inde) = 1.0_rk
                endif
                ikw(indk, inde) = &
                    ( sol%kfgridw(indk, inde) - (1.0_rk - delta)*mukgrid(indk) ) / mukgrid(indk)
            enddo
        enddo

        do inde = 1, enum, 1
            do indk = 1, muknum, 1
                do indbk = 1, mubknum, 1
                    if (sol%divgridv(indbk, indk, inde) / mukgrid(indk) > 0.2_rk) then
                        negInvv20(indbk, indk, inde) = 1.0_rk
                    endif
                    ikv(indbk, indk, inde) = &
                        ( sol%kfgridv(indbk, indk, inde) - (1.0_rk - delta)*mukgrid(indk) ) / mukgrid(indk)
                enddo
            enddo
        enddo

        negInv20_freq = sum(negInvw20*sol%muw) + sum(negInvv20*sol%muv)

        ikw_mean = sum(sol%muw*ikw)
        ikv_mean = sum(sol%muv*ikv)

        do inde = 1, enum, 1
            do indk = 1, muknum, 1
                ikwstd(indk, inde) = (ikw(indk, inde) - ikw_mean)**2.0_rk
            enddo
        enddo

        ikw_std = ( sum(sol%muw*ikwstd) )**0.5_rk

        do inde = 1, enum, 1
            do indk = 1, muknum, 1
                do indbk = 1, mubknum, 1
                    ikvstd(indbk, indk, inde) = (ikv(indbk, indk, inde) - ikv_mean)**2.0_rk
                enddo
            enddo
        enddo
        ikv_std = ( sum(sol%muv*ikvstd) )**0.5_rk

        negInv_freq = sum(negInvw*sol%muw) + sum(negInvv*sol%muv)

        write(*, '(A30, F20.10)') "K/Y = ", KY
        write(*, '(A30, F20.10)') "Inew/K = ", IK
        write(*, '(A30, F20.10)') "reall share = ", reall_share
        write(*, '(A30, F20.10)') "q/Q = ", qQratio
        ! write(*, '(A30, F20.10)') "negInv_freq = ", negInv_freq
        ! write(*, '(A30, F20.10)') "negInv20_freq = ", negInv20_freq



        write(*, *) ikw_mean, ikv_mean, ikw_std, ikv_std

    end subroutine calistats

end module calibrationStats
