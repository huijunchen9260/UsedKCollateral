
    ! subroutine kvdn(bvdnval, kvdnval, evdnval, conf, kstay, kdnmax)
    !     type(configurations), intent(inout) :: conf
    !     type(nlopt_opt) :: opt
    !     real(rk), intent(in) :: kstay, kdnmax
    !     real(rk), intent(out) :: bvdnval, kvdnval, evdnval
    !     real(rk), dimension(1) :: guess, UB, LB
    !     real(rk) :: fmax, bkratio, kw, bkw, evval
    !     integer :: stat
    !     integer(ik) :: kidx, bkidx

    !     if ( dmin1(kdnmax, kstay) <= kgrid(1) ) then
    !         kvdnval = kgrid(1)
    !         bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
    !         bkratio = bvdnval / kvdnval
    !         call linear_interpolation(kgrid, knum, kvdnval, kidx, kw)
    !         call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
    !         evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
    !         evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
    !         evdnval = beta*evval
    !         return
    !     endif


    !     LB = kgrid(1)
    !     UB = dmin1(kdnmax, kgrid(knum), kstay)

    !     guess = (LB + UB) / 2

    !     ! remember that the last number is in int4 but not 8
    !     call create(opt, algorithm_from_string("LN_SBPLX"), 1)
    !     call opt%set_lower_bounds(LB)
    !     call opt%set_upper_bounds(UB)
    !     associate( f => nlopt_func(kvdnObj, conf) )
    !         call opt%set_xtol_rel(tol)
    !         call opt%set_max_objective(f)
    !         call opt%optimize(guess, fmax, stat)
    !     end associate
    !     call destroy(opt)

    !     if (stat < NLOPT_SUCCESS) then
    !         write (*, "(A,I5)") "kvdn: NLopt failed with code ", stat
    !     end if

    !     kvdnval = guess(1)
    !     bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
    !     evdnval = fmax

    ! end subroutine kvdn

    ! function kvdnObj(guess, gradient, func_data) result(f)
    !     real(rk), dimension(:), intent(in) :: guess
    !     real(rk), dimension(:), intent(inout), optional :: gradient
    !     class(*), intent(in), optional :: func_data
    !     type(configurations) :: conf
    !     integer(ik) :: kidx, bkidx
    !     real(rk) :: f, kw, bkw, evval, kfval, bfval, bkratio

    !     select type(func_data)
    !         type is (configurations)
    !         conf = func_data
    !     end select

    !     IF (PRESENT(gradient)) THEN
    !         ! Why?!
    !     END IF

    !     kfval = guess(1)
    !     bfval = ( conf%qsell*kfval - conf%xdval ) / conf%qbval
    !     bkratio = bfval / kfval
    !     call linear_interpolation(kgrid, knum, kfval, kidx, kw)
    !     call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
    !     evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
    !     evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
    !     f = beta*evval

    ! end function kvdnObj

    ! subroutine kvup(bvupval, kvupval, evupval, conf, kstay, kupmax)
    !     type(configurations), intent(inout) :: conf
    !     type(nlopt_opt) :: opt
    !     real(rk), intent(in) :: kstay, kupmax
    !     real(rk), intent(out) :: bvupval, kvupval, evupval
    !     real(rk), dimension(1) :: guess, UB, LB
    !     real(rk) :: fmax, bkratio, kw, bkw, evval
    !     integer :: stat
    !     integer(ik) :: kidx, bkidx

    !     if (kupmax <= kstay) then
    !         kvupval = dmin1(kstay, kgrid(1))
    !         bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
    !         bkratio = bvupval / kvupval
    !         call linear_interpolation(kgrid, knum, kvupval, kidx, kw)
    !         call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
    !         evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
    !         evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
    !         evupval = beta*evval
    !         return
    !     endif

    !     LB = dmax1(kstay, kgrid(1))
    !     UB = dmin1(kupmax, kgrid(knum))

    !     guess = (LB + UB) / 2

    !     ! remember that the last number is in int4 but not 8
    !     call create(opt, algorithm_from_string("LN_SBPLX"), 1)
    !     call opt%set_lower_bounds(LB)
    !     call opt%set_upper_bounds(UB)
    !     associate( f => nlopt_func(kvupObj, conf) )
    !         call opt%set_xtol_rel(tol)
    !         call opt%set_max_objective(f)
    !         call opt%optimize(guess, fmax, stat)
    !     end associate
    !     call destroy(opt)

    !     if (stat < NLOPT_SUCCESS) then
    !         write (*, "(A,I5)") "kvup: NLopt failed with code ", stat
    !     end if

    !     kvupval = guess(1)
    !     bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
    !     evupval = fmax

    ! end subroutine kvup

    ! function kvupObj(guess, gradient, func_data) result(f)
    !     real(rk), dimension(:), intent(in) :: guess
    !     real(rk), dimension(:), intent(inout), optional :: gradient
    !     class(*), intent(in), optional :: func_data
    !     type(configurations) :: conf
    !     integer(ik) :: kidx, bkidx
    !     real(rk) :: f, kw, bkw, evval, kfval, bfval, bkratio

    !     select type(func_data)
    !         type is (configurations)
    !         conf = func_data
    !     end select

    !     IF (PRESENT(gradient)) THEN
    !         ! Why?!
    !     END IF

    !     kfval = guess(1)
    !     bfval = ( conf%Qbuy*kfval - conf%xuval ) / conf%qbval
    !     bkratio = bfval / kfval
    !     call linear_interpolation(kgrid, knum, kfval, kidx, kw)
    !     call linear_interpolation(bkgrid, bknum, bkratio, bkidx, bkw)
    !     evval = bkw * ( conf%evbk(kidx, bkidx)*kw + conf%evbk(kidx+1_ik, bkidx)*(1.0_rk - kw) )
    !     evval = evval + (1.0_rk - bkw) * ( conf%evbk(kidx, bkidx+1_ik)*kw + conf%evbk(kidx+1_ik, bkidx+1_ik)*(1.0_rk - kw) )
    !     f = beta*evval

    ! end function kvupObj

