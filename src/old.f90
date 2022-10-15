
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



    ! function kvupObj(guess, gradient, func_data) result(f)
    !     real(rk), dimension(:), intent(in) :: guess
    !     real(rk), dimension(:), intent(inout), optional :: gradient
    !     class(*), intent(in), optional :: func_data
    !     type(configurations) :: conf
    !     integer(ik) :: kidx, bkidx
    !     real(rk) :: f, kw, bkw, kfval, bfval, bkratio
    !     real(rk) :: Qbuy, xuval, qbval
    !     real(rk) :: ev00, ev10, ev01, ev11, evval

    !     select type(func_data)
    !         type is (configurations)
    !             Qbuy = func_data%Qbuy
    !             xuval = func_data%xuval
    !             qbval = func_data%qbval
    !     end select

    !     IF (PRESENT(gradient)) THEN
    !         ! Why?!
    !     END IF

    !     kfval = guess(1)
    !     bfval = ( Qbuy*kfval - xuval ) / qbval
    !     bkratio = bfval / kfval

    !     kidx = gridlookup(kgrid, knum, kfval)
    !     kw = gridweight(kgrid, knum, kfval, kidx)
    !     bkidx = gridlookup(bkgrid, bknum, bkratio)
    !     bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

    !     select type(func_data)
    !         type is (configurations)
    !             ev00 = func_data%evbk(bkidx, kidx)
    !             ev10 = func_data%evbk(bkidx+1_ik, kidx)
    !             ev01 = func_data%evbk(bkidx, kidx+1_ik)
    !             ev11 = func_data%evbk(bkidx+1_ik, kidx+1_ik)
    !     end select

    !     evval = bkw * ( ev00*kw + ev01*(1.0_rk - kw) )
    !     evval = evval + (1.0_rk - bkw) * ( ev10*kw + ev11*(1.0_rk - kw) )
    !     f = beta*evval

    ! end function kvupObj


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

    ! function kvdnObj(guess, gradient, func_data) result(f)
    !     real(rk), dimension(:), intent(in) :: guess
    !     real(rk), dimension(:), intent(inout), optional :: gradient
    !     class(*), intent(in), optional :: func_data
    !     type(configurations) :: conf
    !     integer(ik) :: kidx, bkidx
    !     real(rk) :: f, kw, bkw, kfval, bfval, bkratio
    !     real(rk) :: qsell, xdval, qbval, fval
    !     real(rk) :: ev00, ev10, ev01, ev11, evval

    !     select type(func_data)
    !         type is (configurations)
    !         qsell = func_data%qsell
    !         xdval = func_data%xdval
    !         qbval = func_data%qbval
    !     end select

    !     IF (PRESENT(gradient)) THEN
    !         ! Why?!
    !     END IF

    !     kfval = guess(1)
    !     bfval = ( qsell*kfval - xdval ) / qbval
    !     bkratio = bfval / kfval

    !     kidx = gridlookup(kgrid, knum, kfval)
    !     kw = gridweight(kgrid, knum, kfval, kidx)
    !     bkidx = gridlookup(bkgrid, bknum, bkratio)
    !     bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

    !     select type(func_data)
    !         type is (configurations)
    !             ev00 = func_data%evbk(bkidx, kidx)
    !             ev10 = func_data%evbk(bkidx+1_ik, kidx)
    !             ev01 = func_data%evbk(bkidx, kidx+1_ik)
    !             ev11 = func_data%evbk(bkidx+1_ik, kidx+1_ik)
    !     end select


    !     evval = bkw * ( ev00*kw + ev01*(1.0_rk - kw) )
    !     evval = evval + (1.0_rk - bkw) * ( ev10*kw + ev11*(1.0_rk - kw) )
    !     f = beta*evval

    ! end function kvdnObj


    ! subroutine kvrule(bvfval, kvfval, vval, bprimemax, bthreshold, kstay, bval, conf)
    !     type(configurations), intent(inout) :: conf
    !     integer(ik) :: kidx, bkidx
    !     real(rk), intent(in) :: bprimemax, bthreshold, kstay, bval
    !     real(rk), intent(out) :: bvfval, kvfval, vval
    !     real(rk) :: kupmax, kdnmax, bkratio
    !     real(rk) :: bvupval, kvupval, evupval, bvdnval, kvdnval, evdnval, bvalsolp
    !     real(rk) :: kw, bkw, evval, LB, UB

    !     kvupval = 0.0_rk
    !     kvdnval = 0.0_rk

    !     ! Check for feasibility if using very large zeta.
    !     if (conf%xdval + conf%qbval*bprimemax < kgrid(1)*conf%qsell) then
    !         bvalsolp = -kgrid(1)*conf%qsell + conf%xdval + bval + conf%qbval*bprimemax
    !         kvfval = kgrid(1)
    !         bvfval = bprimemax
    !         bkratio = bvfval / kvfval

    !         kidx = gridlookup(kgrid, knum, kvfval)
    !         kw = gridweight(kgrid, knum, kvfval, kidx)
    !         bkidx = gridlookup(bkgrid, bknum, bkratio)
    !         bkw = gridweight(bkgrid, bknum, bkratio, bkidx)
    !         evval = bkw * ( conf%evbk(bkidx, kidx)*kw + conf%evbk(bkidx, kidx+1_ik)*(1.0_rk - kw) )
    !         evval = evval + (1.0_rk - bkw) * ( conf%evbk(bkidx+1_rk, kidx)*kw + conf%evbk(bkidx+1_rk, kidx+1_ik)*(1.0_rk - kw) )
    !         evval = beta*evval
    !         vval = (1.0_rk - exitprob)*evval + exitprob*conf%pval*conf%xdval
    !         return
    !     endif

    !     ! If firm can do upward-adjustment
    !     if (bval <= bthreshold) then
    !         kupmax = ( conf%qbval*bprimemax + conf%xuval ) / conf%Qbuy

    !         if (kupmax <= kstay) then
    !             kvupval = dmin1(kstay, kgrid(1))
    !             bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
    !             bkratio = bvupval / kvupval

    !             kidx = gridlookup(kgrid, knum, kvupval)
    !             kw = gridweight(kgrid, knum, kvupval, kidx)
    !             bkidx = gridlookup(bkgrid, bknum, bkratio)
    !             bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

    !             evval = bkw * ( conf%evbk(bkidx, kidx)*kw + conf%evbk(bkidx, kidx+1_ik)*(1.0_rk - kw) )
    !             evval = evval + (1.0_rk - bkw) * ( conf%evbk(bkidx+1_rk, kidx)*kw + conf%evbk(bkidx+1_ik, kidx+1_ik)*(1.0_rk - kw) )
    !             evupval = beta*evval
    !         else
    !             LB = dmax1(kstay, kgrid(1))
    !             UB = dmin1(kupmax, kgrid(knum))
    !             call gss(evupval, kvupval, kvupGSSObj, LB = LB, UB = UB, func_data = conf)
    !             bvupval = ( conf%Qbuy*kvupval - conf%xuval ) / conf%qbval
    !         endif
    !     endif

    !     kdnmax = ( conf%qbval*bprimemax + conf%xdval ) / conf%qsell
    !     if ( dmin1(kdnmax, kstay) <= kgrid(1) ) then
    !         kvdnval = kgrid(1)
    !         bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
    !         bkratio = bvdnval / kvdnval

    !         kidx = gridlookup(kgrid, knum, kvdnval)
    !         kw = gridweight(kgrid, knum, kvdnval, kidx)
    !         bkidx = gridlookup(bkgrid, bknum, bkratio)
    !         bkw = gridweight(bkgrid, bknum, bkratio, bkidx)

    !         evval = bkw * ( conf%evbk(bkidx, kidx)*kw + conf%evbk(bkidx, kidx+1_ik)*(1.0_rk - kw) )
    !         evval = evval + (1.0_rk - bkw) * ( conf%evbk(bkidx+1_ik, kidx)*kw + conf%evbk(bkidx+1_ik, kidx+1_ik)*(1.0_rk - kw) )
    !         evdnval = beta*evval
    !     else
    !         LB = kgrid(1)
    !         UB = dmin1(kdnmax, kgrid(knum), kstay)
    !         call gss(evdnval, kvdnval, kvdnGSSObj, LB = LB, UB = UB, func_data = conf)
    !         bvdnval = ( conf%qsell*kvdnval - conf%xdval ) / conf%qbval
    !     endif

    !     if (bval > bthreshold) then
    !         kvfval = kvdnval
    !         bvfval = bvdnval
    !         vval = evdnval
    !     else
    !         if (evupval >= evdnval) then
    !             kvfval = kvupval
    !             bvfval = bvupval
    !             vval = evupval
    !         else
    !             kvfval = kvdnval
    !             bvfval = bvdnval
    !             vval = evdnval
    !         endif
    !     endif

    !     vval = (1.0_rk - exitprob)*vval + exitprob*conf%pval*conf%xdval

    ! end subroutine kvrule
