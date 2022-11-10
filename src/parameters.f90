module parameters

    ! use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use pyplot_module
    implicit none

    ! ------ !
    ! output !
    ! ------ !
    character(len=*), parameter :: resDir = "./results/"
    character(len=*), parameter :: figDir = "./figures/"
    character(len=*), parameter :: tmpFigDir = "./tmpFigures/"
    character(len=*), parameter :: figPyDir = "./pyfile/"
    character(len=1000) :: sep
    character(len=1), parameter :: tab = char(9)

    ! logical, parameter :: show_wvalueiter = .true.
    ! logical, parameter :: show_minSavingPolicy = .true.
    logical, parameter :: show_vvalueiter = .true.
    logical, parameter :: show_steadyStateDistribution = .true.

    logical, parameter :: show_wvalueiter = .false.
    logical, parameter :: show_minSavingPolicy = .false.
    ! logical, parameter :: show_vvalueiter = .false.
    ! logical, parameter :: show_steadyStateDistribution = .false.

    ! logical, parameter :: saveLog = .true.
    ! integer(ik) :: logunit = 20
    ! character(len=*), parameter :: logfile = "./log.txt"

    character(len=*), parameter :: strfmt = 'F8.3'

    real(rk) :: t0, t1


    ! --------- !
    ! Tolerance !
    ! --------- !
    real(rk), parameter :: tol = 1.0D-9           ! D is e in matlab, double precision
    real(rk), parameter :: wTol = 1.0D-8
    real(rk), parameter :: vTol = 1.0D-5
    real(rk), parameter :: minBTol = 1.0D-8
    real(rk), parameter :: ssDistTol = 1.0D-5
    real(rk), parameter :: bisectTol = 1.0D-5
    integer(ik), parameter :: maxwiter = 300_ik
    integer(ik), parameter :: maxviter = 50_ik
    integer(ik), parameter :: maxbtildeiter = 300_ik
    integer(ik), parameter :: maxssDistiter = 200_ik
    integer(ik), parameter :: maxbisectiter = 50_ik

    ! ----------- !
    ! grid points !
    ! ----------- !
    integer(ik), parameter :: knum = 500
    integer(ik), parameter :: bknum = 500
    integer(ik), parameter :: enum = 7_ik
    integer(ik), parameter :: muknum = 1000
    integer(ik), parameter :: mubknum = 1000

    ! ---------- !
    ! parameters !
    ! ---------- !
    real(rk), parameter :: zss = 1_ik
    real(rk), parameter :: zeta = 1.38_rk         ! credit parameter
    real(rk), parameter :: beta = 0.96_rk         ! yearly interest rate 4%
    real(rk), parameter :: delta = 0.065_rk       ! I/K ~ 10%
    real(rk), parameter :: psi = 2.15_rk          ! hours of worked = 1/3
    real(rk), parameter :: alpha = 0.27_rk        ! K/Y ~ 2.5
    real(rk), parameter :: nu = 0.6_rk            ! labor share: 60% in US
    real(rk), parameter :: rho_z = 0.909_rk       ! Autocor of AR(1) agg. shock
    real(rk), parameter :: sigma_z = 0.014_rk     ! std of AR(1) agg. shock
    real(rk), parameter :: gamma = 0.0260_rk      ! capital reallocation cost
    real(rk), parameter :: eta = 0.75_rk          ! I-tech: new v.s. used ratio
    real(rk), parameter :: s = 5.0_rk             ! I-tech: CES coefficient
    real(rk), parameter :: rho_e = 0.687_rk       ! Autocor of AR(1) idio. shock
    real(rk), parameter :: sigma_e = 0.117_rk     ! std of AR(1) idio. shock
    real(rk), parameter :: exitprob = 0.1_rk      ! exit probability
    real(rk), parameter :: chi = 0.1_rk           ! new firm capital size over average incumbent
    real(rk), parameter :: omegasw = 0.291_rk     ! no-constraint firm ratio

    real(rk), parameter :: qsell0 = 0.918790087890625_rk
    real(rk), parameter :: wval0 = 1.087975585937500_rk

    ! ------ !
    ! arrays !
    ! ------ !
    real(rk), dimension(2) :: kbounds = [ 0.05_rk, 6.0_rk ]
    real(rk), dimension(2) :: bkbounds = [ -25.0_rk, zeta ]
    real(rk), dimension(2) :: pbounds = [ 0.9_rk, 1.2_rk ]
    real(rk), dimension(knum) :: kgrid
    real(rk), dimension(enum) :: egrid
    real(rk), dimension(bknum) :: bkgrid
    real(rk), dimension(muknum) :: mukgrid
    real(rk), dimension(mubknum) :: mubkgrid
    real(rk), dimension(enum) :: piess
    real(rk), dimension(enum, enum) :: pie



    type solutions
        ! wvalueiter
        real(rk), dimension(:, :, :), allocatable :: w
        real(rk), dimension(:, :), allocatable :: gwk
        real(rk), dimension(:), allocatable :: kwupvec, kwdnvec, ewupvec, ewdnvec
        ! minSavingPolicy
        real(rk), dimension(:, :), allocatable :: btilde, gwb
        logical, dimension(:, :, :), allocatable :: wtrue
        ! vvalueiter
        real(rk), dimension(:, :, :), allocatable :: v, gvk, gvb, gvbk
        ! steadyStateDistribution
        real(rk), dimension(:, :), allocatable :: muw
        real(rk), dimension(:, :, :), allocatable :: muv
        real(rk) :: scrapk
        real(rk) :: bornK
        real(rk) :: kagg
        real(rk) :: kfagg
        real(rk) :: nagg
        real(rk) :: invagg
        real(rk) :: divagg
        real(rk) :: invusedagg
        real(rk) :: invnewagg
        real(rk) :: yagg
        real(rk) :: cagg
        real(rk) :: bvfagg
    end type

    type configurations
        real(rk) :: zval                        ! TFP shock
        real(rk) :: wval                        ! wage
        real(rk) :: pval                        ! Marginal utility
        real(rk) :: pfval                       ! future Marginal utility
        real(rk) :: qbval                       ! bond price
        real(rk) :: Qbuy                        ! capital purchasing price
        real(rk) :: qsell                       ! capital selling price
        real(rk) :: xuval                       ! cash on hand for upward-adj firm
        real(rk) :: xdval                       ! cash on hand for downward-adj firm
        real(rk), dimension(knum) :: ewk
        real(rk), dimension(bknum, knum) :: evbk! conditional expectation for v given eps
        real(rk) :: usednewratio                ! used/new investment ratio
        real(rk) :: invnewratio                 ! new/invagg ratio
    end type

    type(pyplot) :: plt
    type(solutions) :: sol
    type(configurations) :: conf

contains

    subroutine initGrids()

        integer(ik) :: i
        real(rk) :: tempterm(enum, enum)

        ! egrid and pie
        ! call tauchen(0.0_rk, sigma_e, rho_e, 2.575_rk, enum, egrid, pie)
        call tauchen(0.0_rk, sigma_e, rho_e, 2.25_rk, enum, egrid, pie)
        egrid = exp(egrid)

        ! steady state pie
        tempterm = eye(enum)
        do i = 1, 1000, 1
            tempterm = matmul(tempterm, pie)
        enddo
        piess = tempterm(1, :)

        kgrid = linspace(kbounds(1), kbounds(2), knum)
        bkgrid = linspace(bkbounds(1), bkbounds(2), bknum)

        mukgrid = linspace(kbounds(1), kbounds(2), muknum)
        mubkgrid = linspace(bkbounds(1), bkbounds(2), mubknum)

    end subroutine initGrids

    subroutine initSol(sol)

        type(solutions), intent(inout) :: sol

        ! wvalueiter
        if (allocated(sol%w)) deallocate(sol%w)
        if (allocated(sol%gwk)) deallocate(sol%gwk)
        if (allocated(sol%kwupvec)) deallocate(sol%kwupvec)
        if (allocated(sol%kwdnvec)) deallocate(sol%kwdnvec)
        if (allocated(sol%ewupvec)) deallocate(sol%ewupvec)
        if (allocated(sol%ewdnvec)) deallocate(sol%ewdnvec)
        ! minSavingPolicy
        if (allocated(sol%btilde)) deallocate(sol%btilde)
        if (allocated(sol%gwb)) deallocate(sol%gwb)
        if (allocated(sol%wtrue)) deallocate(sol%wtrue)
        ! vvalueiter
        if (allocated(sol%v)) deallocate(sol%v)
        if (allocated(sol%gvk)) deallocate(sol%gvk)
        if (allocated(sol%gvb)) deallocate(sol%gvb)
        if (allocated(sol%gvbk)) deallocate(sol%gvbk)
        ! aggregateDynamic
        if (allocated(sol%muw)) deallocate(sol%muw)
        if (allocated(sol%muv)) deallocate(sol%muv)


        ! wvalueiter
        allocate(sol%w(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gwk(knum, enum), source=0.0_rk)
        allocate(sol%kwupvec(enum), source=0.0_rk)
        allocate(sol%kwdnvec(enum), source=0.0_rk)
        allocate(sol%ewupvec(enum), source=0.0_rk)
        allocate(sol%ewdnvec(enum), source=0.0_rk)
        ! minSavingPolicy
        allocate(sol%btilde(knum, enum), source=0.0_rk)
        allocate(sol%gwb(knum, enum), source=0.0_rk)
        allocate(sol%wtrue(bknum, knum, enum), source = .false.)
        ! vvalueiter
        allocate(sol%v(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gvk(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gvb(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gvbk(bknum, knum, enum), source=0.0_rk)
        ! aggregateDynamic
        allocate(sol%muw(muknum, enum), source=0.0_rk)
        allocate(sol%muv(mubknum, muknum, enum), source=0.0_rk)

        sol%scrapk = 0.0_rk
        sol%bornK = 0.0_rk
        sol%kagg = 0.0_rk
        sol%kfagg = 0.0_rk
        sol%nagg = 0.0_rk
        sol%invagg = 0.0_rk
        sol%invusedagg = 0.0_rk
        sol%invnewagg = 0.0_rk
        sol%yagg = 0.0_rk
        sol%cagg = 0.0_rk
        sol%bvfagg = 0.0_rk

    end subroutine initSol

    ! subroutine initConf(conf)
    !     type(configurations), intent(inout) :: conf

    !     if (allocated(conf%ewk)) deallocate(conf%ewk)
    !     if (allocated(conf%evbk)) deallocate(conf%evbk)

    !     allocate(conf%ewk(knum), source = 0.0_rk)
    !     allocate(conf%evbk(bknum, knum), source = 0.0_rk)

    ! end subroutine initConf

end module parameters
