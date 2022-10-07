module gss

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use parameters
    implicit none

contains

    subroutine gss_1d(optObj, LB, UB, conf, tol, maxiter)
        real(rk), intent(in) :: LB, UB, tol
        type(configurations), intent(in) :: conf
        real(rk), intent(in), optional :: maxiter
        interface
            function optObj(x, conf) result(f)
                use iso_Fortran_env, only: rk => real64, ik => int32
                real(rk), intent(in) :: x
                type(configurations), intent(in) :: conf
                real(rk) :: f
            end function optObj
        end interface
        integer(ik) :: iter
        real(rk) :: a, b, c, d, z, fval, fc, fd
    end subroutine gss_1d

end module gss
