module parameters

    ! use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use ogpf
    implicit none

    ! ------ !
    ! output !
    ! ------ !
    character(len=*), parameter :: resDir = "./results/"
    character(len=*), parameter :: figDir = "./figures/"
    character(len=1000) :: sep
    character(len=1), parameter :: tab = char(9)
    ! logical, parameter :: drawfig = .true.
    logical, parameter :: show_valueiter = .true.
    logical, parameter :: show_minSavingPolicy = .true.

    real(rk) :: t0, t1


    ! --------- !
    ! Tolerance !
    ! --------- !
    real(rk), parameter :: tol = 1.0D-9           ! D is e in matlab, double precision
    real(rk), parameter :: wTol = tol*100.0_rk
    real(rk), parameter :: vTol = tol*1000.0_rk
    real(rk), parameter :: minBTol = tol*1000.0_rk
    integer(ik), parameter :: maxwiter = 500_ik
    integer(ik), parameter :: maxviter = 1000_ik
    integer(ik), parameter :: maxbtildeiter = 1000_ik

    ! ----------- !
    ! grid points !
    ! ----------- !
    integer(ik), parameter :: knum = 100_ik
    integer(ik), parameter :: bknum = 100_ik
    integer(ik), parameter :: enum = 7_ik

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
    real(rk), parameter :: exitprob = 0.1_rk

    ! golden ratio
    real(rk), parameter :: rg = (3.0_rk - dsqrt(5.0_rk)) / 2.0_rk

    ! ------ !
    ! arrays !
    ! ------ !
    real(rk), dimension(2) :: kbounds = (/0.05_rk, 6.0_rk/)
    real(rk), dimension(2) :: bkbounds = (/-25.0_rk, zeta/)
    real(rk), dimension(knum) :: kgrid
    real(rk), dimension(enum) :: egrid
    real(rk), dimension(bknum) :: bkgrid
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
        real(rk), dimension(:, :, :), allocatable :: v, gvk, gvb
    end type

    type configurations
        real(rk) :: zval                    ! TFP shock
        real(rk) :: wval                    ! wage
        real(rk) :: pval                    ! Marginal utility
        real(rk) :: pfval                   ! future Marginal utility
        real(rk) :: qbval                   ! bond price
        real(rk) :: Qbuy                    ! capital purchasing price
        real(rk) :: qsell                   ! capital selling price
        real(rk) :: xuval                   ! cash on hand for upward-adj firm
        real(rk) :: xdval                   ! cash on hand for downward-adj firm
        real(rk) :: ewbk(bknum, knum)
        real(rk) :: ewk(knum)
        real(rk) :: evbk(bknum, knum)
        real(rk) :: evk(knum)
    end type

    type(configurations) :: conf

contains

    subroutine initGrids()

        integer(ik) :: i
        real(rk) :: tempterm(enum, enum)

        ! egrid and pie
        call tauchen(0.0_rk, sigma_e, rho_e, 2.575_rk, enum, egrid, pie)
        egrid = exp(egrid)

        ! steady state pie
        tempterm = eye(enum)
        do i = 1, 1000, 1
            tempterm = matmul(tempterm, pie)
        enddo
        piess = tempterm(1, :)

        kgrid = linspace(kbounds(1), kbounds(2), knum)
        bkgrid = linspace(bkbounds(1), bkbounds(2), bknum)

    end subroutine initGrids

    subroutine initSol(sol)

        type(solutions), intent(inout) :: sol

        if (allocated(sol%w)) deallocate(sol%w)
        if (allocated(sol%gwk)) deallocate(sol%gwk)
        if (allocated(sol%kwupvec)) deallocate(sol%kwupvec)
        if (allocated(sol%kwdnvec)) deallocate(sol%kwdnvec)
        if (allocated(sol%ewupvec)) deallocate(sol%ewupvec)
        if (allocated(sol%ewdnvec)) deallocate(sol%ewdnvec)
        if (allocated(sol%btilde)) deallocate(sol%btilde)
        if (allocated(sol%gwb)) deallocate(sol%gwb)
        if (allocated(sol%wtrue)) deallocate(sol%wtrue)
        if (allocated(sol%v)) deallocate(sol%v)
        if (allocated(sol%gvk)) deallocate(sol%gvk)
        if (allocated(sol%gvb)) deallocate(sol%gvb)


        allocate(sol%w(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gwk(knum, enum), source=0.0_rk)
        allocate(sol%kwupvec(enum), source=0.0_rk)
        allocate(sol%kwdnvec(enum), source=0.0_rk)
        allocate(sol%ewupvec(enum), source=0.0_rk)
        allocate(sol%ewdnvec(enum), source=0.0_rk)
        allocate(sol%btilde(knum, enum), source=0.0_rk)
        allocate(sol%gwb(knum, enum), source=0.0_rk)
        allocate(sol%wtrue(bknum, knum, enum), source=.true.)
        allocate(sol%v(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gvk(bknum, knum, enum), source=0.0_rk)
        allocate(sol%gvb(bknum, knum, enum), source=0.0_rk)

    end subroutine initSol

end module parameters
