module UsedKCollateral

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use parameters
    use sim
    use firmValueIter
    use firmDistribution
    use economyBisection
    use lifeCycleSimulation
    use calibrationStats
    implicit none

contains


    subroutine loadResult(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: iunit

        open(newunit=iunit, file=resDir // "prices.tab")
        read (iunit, *) conf%wval
        read (iunit, *) conf%qsell
        read (iunit, *) conf%Qbuy
        read (iunit, *) conf%pval
        close(iunit)


        open(newunit=iunit, file=resDir // "solutions.tab")
        read(iunit, *) sol%scrapk
        read(iunit, *) sol%bornK
        read(iunit, *) sol%kagg
        read(iunit, *) sol%kfagg
        read(iunit, *) sol%nagg
        read(iunit, *) sol%invagg
        read(iunit, *) sol%divagg
        read(iunit, *) sol%invusedagg
        read(iunit, *) sol%invnewagg
        read(iunit, *) sol%yagg
        read(iunit, *) sol%cagg
        read(iunit, *) sol%bvfagg
        close(iunit)

        open (newunit=iunit, file=resDir // "sol%w.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%w
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gwk.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%gwk
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%kwupvec.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%kwupvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%kwdnvec.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%kwdnvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ewdnvec.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%ewdnvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ewupvec.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%ewupvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%btilde.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%btilde
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gwb.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%gwb
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%wtrue.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%wtrue
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%v.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%v
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gvk.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%gvk
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gvb.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%gvb
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gvbk.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%gvbk
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%muw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%muw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%muv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%muv
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ygridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%ygridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ngridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%ngridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%kfgridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%kfgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%invgridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%invgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%invnewgridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%invnewgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%invusedgridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%invusedgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%divgridw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%divgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ygridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%ygridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%ngridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%ngridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%kfgridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%kfgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%bkfgridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%bkfgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%invgridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%invgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%invnewgridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%invnewgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%invusedgridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%invusedgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%divgridv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%divgridv
        close (iunit)

    end subroutine loadResult

    subroutine saveResult(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: iunit
        character(len=*), parameter :: floatFormat = "(E20.10)"
        character(len=*), parameter :: integerFormat = "(I20)"

        ! ---------- !
        ! parameters !
        ! ---------- !

        open(newunit=iunit, file=resDir // "parameters.tab")
        write (iunit, floatFormat) zeta
        write (iunit, floatFormat) beta
        write (iunit, floatFormat) delta
        write (iunit, floatFormat) psi
        write (iunit, floatFormat) alpha
        write (iunit, floatFormat) nu
        write (iunit, floatFormat) rho_z
        write (iunit, floatFormat) sigma_z
        write (iunit, floatFormat) gamma
        write (iunit, floatFormat) eta
        write (iunit, floatFormat) s
        write (iunit, floatFormat) rho_e
        write (iunit, floatFormat) sigma_e
        write (iunit, floatFormat) exitprob
        write (iunit, integerFormat) knum
        write (iunit, integerFormat) bknum
        write (iunit, integerFormat) enum
        write (iunit, integerFormat) muknum
        write (iunit, integerFormat) mubknum
        ! TODO fill out later!
        close(iunit)

        ! ----- !
        ! grids !
        ! ----- !

        open (newunit=iunit, file=resDir // "egrid.bin", &
            form="unformatted", access="stream", status="unknown")
        write (iunit) egrid
        close (iunit)

        open (newunit=iunit, file=resDir // "kgrid.bin", &
            form="unformatted", access="stream", status="unknown")
        write (iunit) kgrid
        close (iunit)

        open (newunit=iunit, file=resDir // "bkgrid.bin", &
            form="unformatted", access="stream", status="unknown")
        write (iunit) bkgrid
        close (iunit)

        open (newunit=iunit, file=resDir // "mukgrid.bin", &
            form="unformatted", access="stream", status="unknown")
        write (iunit) mukgrid
        close (iunit)

        open (newunit=iunit, file=resDir // "mubkgrid.bin", &
            form="unformatted", access="stream", status="unknown")
        write (iunit) mubkgrid
        close (iunit)


        ! --------- !
        ! Solutions !
        ! --------- !

        open(newunit=iunit, file=resDir // "prices.tab")
        write (iunit, floatFormat) conf%wval
        write (iunit, floatFormat) conf%qsell
        write (iunit, floatFormat) conf%Qbuy
        write (iunit, floatFormat) conf%pval
        close(iunit)


        open(newunit=iunit, file=resDir // "solutions.tab")
        write(iunit, floatFormat) sol%scrapk
        write(iunit, floatFormat) sol%bornK
        write(iunit, floatFormat) sol%kagg
        write(iunit, floatFormat) sol%kfagg
        write(iunit, floatFormat) sol%nagg
        write(iunit, floatFormat) sol%invagg
        write(iunit, floatFormat) sol%divagg
        write(iunit, floatFormat) sol%invusedagg
        write(iunit, floatFormat) sol%invnewagg
        write(iunit, floatFormat) sol%yagg
        write(iunit, floatFormat) sol%cagg
        write(iunit, floatFormat) sol%bvfagg
        close(iunit)

        open (newunit=iunit, file=resDir // "sol%w.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%w
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gwk.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%gwk
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%kwupvec.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%kwupvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%kwdnvec.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%kwdnvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ewdnvec.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%ewdnvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ewupvec.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%ewupvec
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%btilde.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%btilde
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gwb.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%gwb
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%wtrue.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%wtrue
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%v.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%v
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gvk.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%gvk
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gvb.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%gvb
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%gvbk.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%gvbk
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%muw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%muw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%muv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%muv
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ygridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%ygridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ngridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%ngridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%kfgridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%kfgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%invgridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%invgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%invnewgridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%invnewgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%invusedgridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%invusedgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%divgridw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%divgridw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%ygridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%ygridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%ngridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%ngridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%kfgridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%kfgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%bkfgridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%bkfgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%invgridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%invgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%invnewgridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%invnewgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%invusedgridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%invusedgridv
        close (iunit)
        open (newunit=iunit, file=resDir // "sol%divgridv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%divgridv
        close (iunit)

    end subroutine saveResult

end module UsedKCollateral
