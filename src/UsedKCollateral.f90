module UsedKCollateral

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use parameters
    use firmValueIter
    use firmDistribution
    implicit none

contains


    subroutine loadResult(sol)

        type(solutions), intent(inout) :: sol
        integer(ik) :: iunit

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

        open (newunit=iunit, file=resDir // "sol%muw.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%muw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%muv.bin", &
          form="unformatted", access="stream", status="old")
        read (iunit) sol%muv
        close (iunit)

    end subroutine loadResult

    subroutine saveResult(sol)

        type(solutions), intent(inout) :: sol
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

        ! --------- !
        ! Solutions !
        ! --------- !

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

        open (newunit=iunit, file=resDir // "sol%muw.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%muw
        close (iunit)

        open (newunit=iunit, file=resDir // "sol%muv.bin", &
          form="unformatted", access="stream", status="unknown")
        write (iunit) sol%muv
        close (iunit)

    end subroutine saveResult

end module UsedKCollateral
