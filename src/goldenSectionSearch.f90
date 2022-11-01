module goldenSectionSearch

    ! ------------------------------------------------------------------ !
    ! Coping the whole derived type for every execution of the function  !
    ! is time and memory consuming.                                      !
    ! To prevent such efficiency loss, when user feed the func_data into !
    ! the Obj function, they should not copy the whole func_data.        !
    ! Instead, user should define the corresponding variables inside     !
    ! functions to avoid copying the matrices inside the derived type.   !
    ! ------------------------------------------------------------------ !

    ! ----------------------------------------------------------- !
    ! Sample Obj function:                                        !
    ! function kwdnGSSObj(kfval, func_data) result(fval)          !
    !     real(rk), intent(in) :: kfval                           !
    !     class(*), intent(in), optional :: func_data     !
    !     integer(ik) :: kidx                                     !
    !     real(rk) :: fval, evval, kw, pval, Jval                 !
    !     real(rk) :: ewkLeft, ewkRight                           !
    !                                                             !
    !     call linear_interpolation(kgrid, knum, kfval, kidx, kw) !
    !                                                             !
    !     select type(func_data)                                  !
    !         type is (configurations)                            !
    !         ewkLeft = func_data%ewk(kidx)                       !
    !         ewkRight = func_data%ewk(kidx + 1_ik)               !
    !         pval = func_data%pval                               !
    !         Jval = func_data%qsell                              !
    !     end select                                              !
    !                                                             !
    !     evval = ewkLeft*kw + ewkRight*(1.0_rk - kw)             !
    !     fval = -1.0_rk * pval * Jval * kfval + beta*evval       !
    !                                                             !
    ! end function kwdnGSSObj                                     !
    ! ----------------------------------------------------------- !


    use iso_Fortran_env, only: rk => real64, ik => int32
    implicit none
    private
    public :: gss

    real(rk), parameter :: goldenRatio = (3.0_rk - dsqrt(5.0_rk)) / 2.0_rk

    interface gss
        module procedure gss_1d
    end interface

contains

    subroutine gss_1d(fout, xout, Obj, LB, UB, tol, maxiter, findmax, show, func_data)
        real(rk), intent(out) :: fout, xout
        real(rk), intent(in) :: LB, UB
        real(rk), intent(in), optional :: tol
        integer(ik), intent(in), optional :: maxiter
        logical, intent(in), optional :: findmax
        logical, intent(in), optional :: show
        class(*), intent(in), optional :: func_data
        interface
            function Obj(x, func_data) result(f)
                import :: rk, ik
                class(*), intent(in), optional :: func_data
                real(rk), intent(in) :: x
                real(rk) :: f
            end function Obj
        end interface
        character(len=128) :: tmp
        integer(ik) :: iter, gssMaxIter
        logical :: isFindMax, isShow
        real(rk) :: a, b, c, d, z, fval, fc, fd, gssTol, gssDist

        ! ------- !
        ! default !
        ! ------- !
        iter = 0_ik
        gssTol = 1D-9
        gssMaxIter = 500_ik
        isFindMax = .true.
        isShow = .false.
        if (present(tol)) gssTol = tol
        if (present(maxiter)) gssMaxIter = maxiter
        if (present(findmax)) isFindMax = findmax
        if (present(show)) isShow = show

        ! --------------------- !
        ! golden section search !
        ! --------------------- !

        gssDist = 2.0_rk*gssTol

        a = LB
        b = UB
        c = a + goldenRatio*(b-a);
        d = a + (1-goldenRatio)*(b-a);

        fval = Obj(c, func_data); fc = merge(-fval, fval, isFindMax);
        fval = Obj(d, func_data); fd = merge(-fval, fval, isFindMax);

        do while (gssDist > gssTol .and. iter <= gssMaxIter)

            iter = iter + 1

            if (fc >= fd) then
                z = c + (1.0_rk-goldenRatio)*(b-c)
                ! case 1 [a c d b] <--- [c d z b]
                a = c
                c = d
                fc = fd
                d = z
                fval = Obj(d, func_data); fd = merge(-fval, fval, isFindMax);
            else
                z = a + goldenRatio*(d-a)
                ! case 2 [a c d b] <--- [a z c d]
                b = d
                d = c
                fd = fc
                c = z
                fval = Obj(c, func_data); fc = merge(-fval, fval, isFindMax);
            endif

            gssDist = b - a

            if (iter == 1 .and. isShow) then
                write(*, *) ''
                write(*, '(a4, 5(a20))') 'iter', '||b-a||', 'a', 'c', 'd', 'b'
                write(tmp, '(a4, 5(a20))') 'iter', '||b-a||', 'a', 'c', 'd', 'b'
                write(*, '(a)') repeat('=', len_trim(tmp))
            endif
            if (isShow) write (*, '(i4, 5(ES20.6))') iter, gssDist, a, c, d, b

        enddo

        if (z < LB) then
            z = LB
            fout = Obj(z, func_data);
            xout = z
        elseif (z > UB) then
            z = UB
            fout = Obj(z, func_data);
            xout = z
        else
            fout = fval
            xout = z
        endif


    end subroutine gss_1d

end module goldenSectionSearch
