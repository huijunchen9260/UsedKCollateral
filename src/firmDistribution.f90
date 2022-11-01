module firmDistribution

    use io
    use numerics
    use parameters
    use optMod
    use distMod
    implicit none

contains

    subroutine steadyStateDistribution(sol, conf)
        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: iter, bkidx0, mubkidx0, choiceik, kfidx, bkfidx
        integer(ik) :: indk, indbk, inde, indef
        integer(ik) :: kEntryIdx, bkEntryIdx
        real(rk) :: dist, distmuw, distmuv
        real(rk) :: bkw0, mubkw0, kfwLeft, kfwRight, bkfwLeft, bkfwRight
        real(rk) :: kEntryLeft, kEntryRight, bkEntryLeft, bkEntryRight
        real(rk) :: kwdnstar, ewdnstar, kwupstar, ewupstar
        real(rk) :: kstar, vstar, wstar, kstay
        real(rk) :: epsval, kval, bkval, bval, nval, yval, xuval, xdval
        real(rk) :: kfval, bkfval, kvfval, bvfval, vval, muwval, muvval, pieval, pieswval
        real(rk) :: bprimemax, bthreshold, dividend
        real(rk) :: tempval
        real(rk) :: scrapk, kpaggf
        real(rk) :: VMass
        real(rk) :: maxb, bfminval
        real(rk), dimension(:), allocatable :: bfw
        real(rk), dimension(:, :, :), allocatable :: ew
        real(rk), dimension(:, :), allocatable :: tmuw, ygridw, ngridw, kfgridw
        real(rk), dimension(:, :), allocatable :: invgridw, invnewgridw, invusedgridw, divgridw
        real(rk), dimension(:, :, :), allocatable :: tmuv, ygridv, ngridv, kfgridv, bkfgridv
        real(rk), dimension(:, :, :), allocatable :: invgridv, invnewgridv, invusedgridv, divgridv
        real(rk), dimension(:, :, :), allocatable :: wdist
        logical, dimension(:, :, :), allocatable :: wtypedist

        logical :: isKUp, isWType

        ! -------------- !
        ! initialization !
        ! -------------- !

        choiceik = 0_ik

        kvfval = 0.0_rk
        bvfval = 0.0_rk

        allocate(bfw(enum), source = 0.0_rk)

        allocate(tmuw(muknum, enum), source = 0.0_rk)
        allocate(ygridw(muknum, enum), source = 0.0_rk)
        allocate(ngridw(muknum, enum), source = 0.0_rk)
        allocate(kfgridw(muknum, enum), source = 0.0_rk)
        allocate(invgridw(muknum, enum), source = 0.0_rk)
        allocate(divgridw(muknum, enum), source = 0.0_rk)
        allocate(invnewgridw(muknum, enum), source = 0.0_rk)
        allocate(invusedgridw(muknum, enum), source = 0.0_rk)

        allocate(tmuv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(ygridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(ngridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(kfgridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(bkfgridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(invgridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(divgridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(invnewgridv(mubknum, muknum, enum), source = 0.0_rk)
        allocate(invusedgridv(mubknum, muknum, enum), source = 0.0_rk)

        allocate(wdist(mubknum, muknum, enum), source = 0.0_rk)
        allocate(wtypedist(mubknum, muknum, enum), source = .false.)

        write(*, *) 'stationary distribution on both W-type and V-type firm'

        bkidx0 = gridlookup(bkgrid, bknum, 0.0_rk)
        bkw0 = gridweight(bkgrid, bknum, 0.0_rk, bkidx0)

        mubkidx0 = gridlookup(mubkgrid, mubknum, 0.0_rk)
        mubkw0 = gridweight(mubkgrid, mubknum, 0.0_rk, mubkidx0)

        ! ----------------------------------------- !
        ! initial distribution guess based on piess !
        ! ----------------------------------------- !

        sol%muw = 0.0_rk
        sol%muv = 0.0_rk
        sol%muv(mubkidx0, 1, :) = piess
        ! do indk = 1, muknum, 1
        ! do indbk = 1, mubknum, 1
        !     sol%muv(indbk, indk, :) = piess / (mubknum*muknum)
        ! enddo
        ! enddo

        write(*, '(3(a, ES15.6))') 'initial: sum(muw) = ', sum(sol%muw), &
                                    ', sum(muv) = ', sum(sol%muv), &
                                    ', total = ', sum(sol%muw) + sum(sol%muv)

        ! -------------------------------------------- !
        ! compute decision rules on distribution grids !
        ! -------------------------------------------- !

        do inde = 1, enum, 1
            epsval = egrid(inde)
            do indk = 1, muknum, 1

                kval = mukgrid(indk)
                kstay = (1.0_rk - delta)*kval

                kfidx = gridlookup(kgrid, knum, kval)
                kfwLeft = gridweight(kgrid, knum, kval, kfidx)
                kfwRight = 1.0_rk - kfwLeft
                ! wstar = sol%w(kfidx, inde)*kfwLeft + sol%w(kfidx+1, inde)*kfwRight
                kstar = sol%gwk(kfidx, inde)*kfwLeft + sol%gwk(kfidx+1, inde)*kfwRight
                isKUp = merge(.true., .false., kstar >= kstay)

                call debtThreshold(nval, yval, bprimemax, bthreshold, kval, epsval, conf)
                ! nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))
                ! yval = conf%zval * epsval * kval**alpha * nval**nu

                ngridw(indk, inde) = nval
                ygridw(indk, inde) = yval
                kfgridw(indk, inde) = kstar

                ! Here both inv and disinv (div) are not evaluated in market price
                if (isKUp) then
                    invgridw(indk, inde) = kstar - kstay
                else
                    divgridw(indk, inde) = -1.0_rk*(kstar - kstay)
                endif


                ! Retrive minimum saving policy on distribution grids
                ! maxb: threshold for current bond level
                maxb = sol%btilde(kfidx, inde)*kfwLeft + sol%btilde(kfidx+1, inde)*kfwRight
                ! bfminval: minimum saving policy for b'
                do indef = 1, enum, 1
                    bfw(indef) = sol%gwb(kfidx, indef)*kfwLeft + sol%gwb(kfidx+1, indef)*kfwRight
                enddo
                bfminval = minval(bfw, mask = pie(inde, :) > 0.0_rk)
                bfminval = min(bfminval, bprimemax)

                do indbk = 1, mubknum, 1

                    bkval = mubkgrid(indbk)
                    bval = mubkgrid(indbk)*kval
                    conf%xuval = yval - conf%wval*nval - bval + conf%Qbuy*kstay
                    conf%xdval = yval - conf%wval*nval - bval + conf%qsell*kstay
                    ygridv(indbk, indk, inde) = yval
                    ngridv(indbk, indk, inde) = nval

                    if ( bval <= maxb ) then
                        wtypedist(indbk, indk, inde) = .true.
                        kvfval = kstar
                        bvfval = bfminval
                        if (isKUp) then
                            dividend = conf%xuval + conf%Qbuy*kvfval + conf%qbval*bvfval
                            dividend = (1.0_rk - exitprob)*dividend + exitprob*conf%xdval
                        else
                            dividend = conf%xdval + conf%qsell*kvfval + conf%qbval*bvfval
                            dividend = (1.0_rk - exitprob)*dividend + exitprob*conf%xdval
                        endif
                    else
                        wtypedist(indbk, indk, inde) = .false.

                        ! call kvrule(bvfval, kvfval, vval, choiceik, bprimemax, bthreshold, kstay, bval, conf)

                        bkfidx = gridlookup(bkgrid, bknum, bkval)
                        bkfwLeft = gridweight(bkgrid, bknum, bkval, bkfidx)
                        bkfwRight = 1.0_rk - bkfwLeft

                        kvfval = bkfwLeft*( sol%gvk(bkfidx, kfidx, inde)*kfwLeft + &
                                            sol%gvk(bkfidx, kfidx+1, inde)*kfwRight )
                        kvfval = kvfval + bkfwRight*( sol%gvk(bkfidx+1, kfidx, inde)*kfwLeft + &
                                                      sol%gvk(bkfidx+1, kfidx+1, inde)*kfwRight )

                        bvfval = bkfwLeft*( sol%gvb(bkfidx, kfidx, inde)*kfwLeft + &
                                            sol%gvb(bkfidx, kfidx+1, inde)*kfwRight )
                        bvfval = bvfval + bkfwRight*( sol%gvb(bkfidx+1, kfidx, inde)*kfwLeft + &
                                                      sol%gvb(bkfidx+1, kfidx+1, inde)*kfwRight )

                        isKUp = merge(.true., .false., kvfval >= kstay)

                        dividend = (1.0_rk - exitprob)*0.0_rk + exitprob*conf%xdval

                    endif

                    kfgridv(indbk, indk, inde) = kvfval

                    if (wtypedist(indbk, indk, inde)) then
                        bkfgridv(indbk, indk, inde) = 0.0_rk
                    else
                        bkfgridv(indbk, indk, inde) = bvfval / kvfval
                    endif

                    if (isKUp) then
                        invgridv(indbk, indk, inde) = kvfval - kstay
                    else
                        divgridv(indbk, indk, inde) = -1.0_rk*(kvfval - kstay)
                    endif

                enddo

            enddo
        enddo

        ! inde = 4
        ! call plt%initialize(&
        !     usetex = .true., &
        !     ylabel = '$k$', &
        !     xlabel = '$\\frac{b}{k}$', &
        !     zlabel = "k'", &
        !     title = 'V-type (constrained) capital decision, $\\epsilon = $' // &
        !             num2str(egrid(inde), strfmt), &
        !     mplot3d = .true., &
        !     tight_layout = .true. &
        !     )

        ! call plt%plot_wireframe(&
        !     mubkgrid, &
        !     mukgrid, &
        !     kfgridv(:, :, inde), &
        !     label = "kvf", &
        !     antialiased = .true., &
        !     linestyle = '-' &
        !     )
        ! call plt%showfig()

        ! stop


! ------------------------------------------------------------------------------------------------------------------------- !
! DEBUG                                                                                                                     !
! ------------------------------------------------------------------------------------------------------------------------- !
            ! ------------------------------------------------- !                                                         !
            ! Find which v-type firm will change to w-type firm !                                                         !
            ! ------------------------------------------------- !                                                         !
                                                                                                                          !
            !calcVtoW_epsloop: do inde = 1, enum, 1                                                                        !
            !                                                                                                              !
            !    conf%ewk = sol%w(bkidx0, :, inde)*bkw0 + sol%w(bkidx0+1_ik, :, inde)*(1.0_rk - bkw0)                      !
            !    call gss(ewdnstar, kwdnstar, kwdnGSSObj, LB = kgrid(1), UB = kgrid(knum), func_data = conf)               !
            !    call gss(ewupstar, kwupstar, kwupGSSObj, LB = kgrid(1), UB = kgrid(knum), func_data = conf)               !
            !                                                                                                              !
            !    calcVtoW_mukloop: do indk = 1, muknum, 1                                                                  !
            !                                                                                                              !
            !        kval = mukgrid(indk)                                                                                  !
            !        kstay = (1.0_rk - delta)*kval                                                                         !
            !        nval = ( ( nu * conf%zval * epsval * kval**alpha ) / conf%wval )**(1/(1-nu))                          !
            !        yval = conf%zval * epsval * kval**alpha * nval**nu                                                    !
            !                                                                                                              !
            !        ! calcVtoW_muwcheck: if (sol%muw(indk, inde) > 0.0_rk) then                                           !
            !            call kwrule(wstar, kstar, isKUp, yval, nval, kstay, kwupstar, ewupstar, kwdnstar, ewdnstar, conf) !
            !            ygridw(indk, inde) = yval                                                                         !
            !            ngridw(indk, inde) = nval                                                                         !
            !            kfgridw(indk, inde) = kstar                                                                       !
            !                                                                                                              !
            !            if (isKUp) then                                                                                   !
            !                invgridw(indk, inde) = kstar - kstay                                                          !
            !            else                                                                                              !
            !                divgridw(indk, inde) = -1.0_rk*(kstar - kstay)                                                !
            !            endif                                                                                             !
            !        ! endif calcVtoW_muwcheck                                                                             !
            !                                                                                                              !
            !        call debtThreshold(nval, yval, bprimemax, bthreshold, kval, epsval, conf)                             !
            !                                                                                                              !
            !        calcVtoW_mubkloop: do indbk = 1, mubknum, 1                                                           !
            !                                                                                                              !
            !            ! calcVtoW_muvcheck: if (sol%muv(indbk, indk, inde) > 0.0_rk) then                                !
            !                                                                                                              !
            !                bval = mubkgrid(indbk)*kval                                                                   !
            !                conf%xuval = yval - conf%wval*nval - bval + conf%Qbuy*kstay                                   !
            !                conf%xdval = yval - conf%wval*nval - bval + conf%qsell*kstay                                  !
            !                                                                                                              !
            !                ygridv(indbk, indk, inde) = yval                                                              !
            !                ngridv(indbk, indk, inde) = nval                                                              !
            !                                                                                                              !
            !                call vDistDecision(dividend, isKUp, isWType, kvfval, bvfval, choiceik, &                      !
            !                        kwdnstar, ewdnstar, kwupstar, ewupstar, bprimemax, bthreshold, &                      !
            !                        kval, bval, nval, yval, inde, sol, conf)                                              !
            !                                                                                                              !
            !                kfgridv(indbk, indk, inde) = kvfval                                                           !
            !                wtypedist(indbk, indk, inde) = isWType                                                        !
            !                                                                                                              !
            !                                                                                                              !
            !                if (isWType) then                                                                             !
            !                    bkfgridv(indbk, indk, inde) = mubkgrid(1)                                                 !
            !                else                                                                                          !
            !                    bkfgridv(indbk, indk, inde) = bvfval / kvfval                                             !
            !                endif                                                                                         !
            !                                                                                                              !
            !                if (isKUp) then                                                                               !
            !                    invgridv(indbk, indk, inde) = kvfval - kstay                                              !
            !                else                                                                                          !
            !                    divgridv(indbk, indk, inde) = -1.0_rk*(kvfval - kstay)                                    !
            !                endif                                                                                         !
            !                                                                                                              !
            !            ! endif calcVtoW_muvcheck                                                                         !
            !                                                                                                              !
            !        enddo calcVtoW_mubkloop                                                                               !
            !                                                                                                              !
            !    enddo calcVtoW_mukloop                                                                                    !
            !                                                                                                              !
            !enddo calcVtoW_epsloop                                                                                        !
            !                                                                                                              !
            !wdist = merge(1.0_rk, 0.0_rk, wtypedist)                                                                      !
            !                                                                                                              !
            !VMass = 1.0_rk - (sum(sol%muw) + sum( wdist * sol%muv ))                                                      !

            !inde = 4                                                                                                      !
            !                call plt%initialize(&                                                                         !
            !                    usetex = .true., &                                                                        !
            !                    ylabel = '$k$', &                                                                         !
            !                    xlabel = '$\\frac{b}{k}$', &                                                              !
            !                    zlabel = 'muv', &                                                                         !
            !                    title = 'V-type (constrained) capital decision, $\\epsilon = $' // &                      !
            !                            num2str(egrid(inde), strfmt) // ', iter = ' //  num2str(iter), &                  !
            !                    mplot3d = .true., &                                                                       !
            !                    tight_layout = .true. &                                                                   !
            !                    )                                                                                         !
            !                                                                                                              !
            !                call plt%plot_wireframe(&                                                                     !
            !                    mubkgrid, &                                                                               !
            !                    mukgrid, &                                                                                !
            !                    kfgridv(:, :, inde), &                                                                    !
            !                    label = "kvf", &                                                                          !
            !                    antialiased = .true., &                                                                   !
            !                    linestyle = '-' &                                                                         !
            !                    )                                                                                         !
            !                call plt%showfig()                                                                            !
            !                                                                                                              !
            !                ! call plt%savefig( tmpFigDir // "muv_" // num2str(iter) // ".pdf" )                          !
!!------------------------------------------------------------------------------------------------------------------------- !

!stop

        ! -------------------------------- !
        ! calculate stational distribution !
        ! -------------------------------- !

        iter = 0_ik
        dist = 2.0_rk*ssDistTol

        ssDist_while: do while (dist > ssDistTol)

            iter = iter + 1

            tmuw = 0.0_rk
            tmuv = 0.0_rk
            sol%kagg = 0.0_rk


            wDist_epsloop: do inde = 1, enum, 1

                wDist_mukloop: do indk = 1, muknum, 1


                    ! ------------------------- !
                    ! Fit know w type into tmuw !
                    ! ------------------------- !

                    muwval = sol%muw(indk, inde)


                    wStayAtW: if (muwval > 0.0_rk) then

                        kval = mukgrid(indk)
                        sol%kagg = kval*muwval + sol%kagg

                        kfval = kfgridw(indk, inde)

                        kfidx = gridlookup(mukgrid, muknum, kfval)
                        kfwLeft = gridweight(mukgrid, muknum, kfval, kfidx)
                        kfwRight = 1.0_rk - kfwLeft

                        do indef = 1, enum, 1
                            pieval = pie(inde, indef)
                            if (kfidx < muknum) then
                                tempval = tmuw(kfidx, indef)
                                tmuw(kfidx, indef) = tempval + pieval*muwval*kfwLeft
                                tempval = tmuw(kfidx+1, indef)
                                tmuw(kfidx+1, indef) = tempval + pieval*muwval*kfwRight
                            else
                                tempval = tmuw(kfidx, indef)
                                tmuw(kfidx, indef) = tempval + pieval*muwval
                            endif
                        enddo

                    endif wStayAtW

                    wDist_mubkloop: do indbk = 1, mubknum, 1

                        ! ------------------------------------ !
                        ! Fit v type firm who switch to w type !
                        ! ------------------------------------ !

                        muvval = sol%muv(indbk, indk, inde)

                        vSwitchToW: if (muvval > 0.0_rk .and. wtypedist(indbk, indk, inde)) then

                            kval = mukgrid(indk)
                            sol%kagg = kval*muvval + sol%kagg

                            kfval = kfgridv(indbk, indk, inde)

                            kfidx = gridlookup(mukgrid, muknum, kfval)
                            kfwLeft = gridweight(mukgrid, muknum, kfval, kfidx)
                            kfwRight = 1.0_rk - kfwLeft

                            do indef = 1, enum, 1
                                pieval = pie(inde, indef)
                                if (kfidx < muknum) then
                                    tempval = tmuw(kfidx, indef)
                                    tmuw(kfidx, indef) = tempval + pieval*muvval*kfwLeft
                                    tempval = tmuw(kfidx+1, indef)
                                    tmuw(kfidx+1, indef) = tempval + pieval*muvval*kfwRight
                                else
                                    tempval = tmuw(kfidx, indef)
                                    tmuw(kfidx, indef) = tempval + pieval*muvval
                                endif
                            enddo

                        endif vSwitchToW

                    enddo wDist_mubkloop

                enddo wDist_mukloop

            enddo wDist_epsloop

            vDist_epsloop: do inde = 1, enum, 1
                vDist_mukloop: do indk = 1, muknum, 1
                    vDist_mubkloop: do indbk = 1, mubknum, 1

                        muvval = sol%muv(indbk, indk, inde)

                        ! ---------------------------- !
                        ! Fit v type remains at v type !
                        ! ---------------------------- !

                        vStayAtV: if ( muvval > 0.0_rk .and. ( .not. wtypedist(indbk, indk, inde) ) ) then

                            kval = mukgrid(indk)
                            sol%kagg = kval*muvval + sol%kagg

                            kfval = kfgridv(indbk, indk, inde)
                            kfidx = gridlookup(mukgrid, muknum, kfval)
                            kfwLeft = gridweight(mukgrid, muknum, kfval, kfidx)
                            kfwRight = 1.0_rk - kfwLeft

                            bkfval = bkfgridv(indbk, indk, inde)
                            bkfidx = gridlookup(mubkgrid, mubknum, bkfval)
                            bkfwLeft = gridweight(mubkgrid, mubknum, bkfval, bkfidx)
                            bkfwRight = 1.0_rk - bkfwLeft

                            do indef = 1, enum, 1
                                pieval = pie(inde, indef)

                                if (kfidx < muknum .and. bkfidx < mubknum) then

                                    tempval = tmuv(bkfidx, kfidx, indef)
                                    tmuv(bkfidx, kfidx, indef) = tempval + pieval*muvval*kfwLeft*bkfwLeft

                                    tempval = tmuv(bkfidx, kfidx+1, indef)
                                    tmuv(bkfidx, kfidx+1, indef) = tempval + pieval*muvval*kfwRight*bkfwLeft

                                    tempval = tmuv(bkfidx+1, kfidx, indef)
                                    tmuv(bkfidx+1, kfidx, indef) = tempval + pieval*muvval*kfwLeft*bkfwRight

                                    tempval = tmuv(bkfidx+1, kfidx+1, indef)
                                    tmuv(bkfidx+1, kfidx+1, indef) = tempval + pieval*muvval*kfwRight*bkfwRight

                                elseif (kfidx < muknum .and. bkfidx == mubknum ) then

                                    tempval = tmuv(bkfidx, kfidx, indef)
                                    tmuv(bkfidx, kfidx, indef) = tempval + pieval*muvval*kfwLeft

                                    tempval = tmuv(bkfidx, kfidx+1, indef)
                                    tmuv(bkfidx, kfidx+1, indef) = tempval + pieval*muvval*kfwRight

                                elseif (kfidx == muknum .and. bkfidx < mubknum ) then

                                    tempval = tmuv(bkfidx, kfidx, indef)
                                    tmuv(bkfidx, kfidx, indef) = tempval + pieval*muvval*bkfwLeft

                                    tempval = tmuv(bkfidx+1, kfidx, indef)
                                    tmuv(bkfidx+1, kfidx, indef) = tempval + pieval*muvval*bkfwRight

                                else

                                    tempval = tmuv(bkfidx, kfidx, indef)
                                    tmuv(bkfidx, kfidx, indef) = tempval + pieval*muvval

                                endif

                            enddo

                        endif vStayAtV

                    enddo vDist_mubkloop
                enddo vDist_mukloop
            enddo vDist_epsloop

            sol%scrapk = conf%qsell * (1.0_rk - delta) * sol%kagg

            kpaggf = 0.0_rk
            do indk = 1, muknum, 1
                do indef = 1, enum, 1
                    kpaggf = kpaggf + mukgrid(indk) * sum(tmuv(:, indk, indef))
                    kpaggf = kpaggf + mukgrid(indk) * tmuw(indk, indef)
                enddo
            enddo

            ! ------------------------------ !
            ! Taking care of entry and exit  !
            ! ------------------------------ !

            ! exit
            tmuw = (1.0_rk - exitprob) * tmuw
            tmuv = (1.0_rk - exitprob) * tmuv

            ! entry
            kfval = chi * sol%kagg
            bkfval = 0.0_rk

            kEntryIdx = gridlookup(mukgrid, muknum, kfval)
            kEntryLeft = gridweight(mukgrid, muknum, kfval, kEntryIdx)
            kEntryRight = 1.0_rk - kEntryLeft

            bkEntryIdx = gridlookup(mubkgrid, mubknum, bkfval)
            bkEntryLeft = gridweight(mubkgrid, mubknum, bkfval, bkEntryIdx)
            bkEntryRight = 1.0_rk - bkEntryLeft

            if (kEntryIdx < muknum) then
                sol%bornK = kEntryLeft*mukgrid(kEntryIdx) + kEntryRight*mukgrid(kEntryIdx+1)
            else
                sol%bornK = mukgrid(muknum)
            endif

            if ( kEntryIdx == 1 .and. kEntryLeft > 0.9_rk ) then
                write(*,'(1x,a,f8.4,a,i4,a,f7.4,a,f7.4,a,f8.4)') ' steadyStateDistribution has kfval = ', chi*sol%kagg, 'kidx = ', &
                        kEntryIdx, ' (kwLeft, kwRight) = (',kEntryLeft, ',', kEntryRight,') and born with = ', sol%bornK
            endif

            do indef = 1, enum, 1
                pieval = exitprob * (1.0_rk - omegasw) * piess(indef)
                pieswval = exitprob * omegasw * piess(indef)

                ! A: Ordinary type entrants
                if (kEntryIdx < muknum) then

                    tempval = tmuv(bkEntryIdx, kEntryIdx, indef)
                    tmuv(bkEntryIdx, kEntryIdx, indef) = tempval + pieval*kEntryLeft*bkEntryLeft

                    tempval = tmuv(bkEntryIdx, kEntryIdx+1, indef)
                    tmuv(bkEntryIdx, kEntryIdx+1, indef) = tempval + pieval*kEntryRight*bkEntryLeft

                    tempval = tmuv(bkEntryIdx+1, kEntryIdx, indef)
                    tmuv(bkEntryIdx+1, kEntryIdx, indef) = tempval + pieval*kEntryLeft*bkEntryRight

                    tempval = tmuv(bkEntryIdx+1, kEntryIdx+1, indef)
                    tmuv(bkEntryIdx+1, kEntryIdx+1, indef) = tempval + pieval*kEntryRight*bkEntryRight

                else

                    tempval = tmuv(bkEntryIdx, kEntryIdx, indef)
                    tmuv(bkEntryIdx, kEntryIdx, indef) = tempval + pieval*bkEntryLeft

                    tempval = tmuv(bkEntryIdx+1, kEntryIdx, indef)
                    tmuv(bkEntryIdx+1, kEntryIdx, indef) = tempval + pieval*bkEntryRight

                endif

                ! B: no-constraint firms (born as w types)

                if (kEntryIdx < muknum) then
                    tempval = tmuw(kEntryIdx, indef)
                    tmuw(kEntryIdx, indef) = tempval + pieswval*kEntryLeft
                    tempval = tmuw(kEntryIdx+1, indef)
                    tmuw(kEntryIdx+1, indef) = tempval + pieswval*kEntryRight
                else
                    tempval = tmuw(kEntryIdx, indef)
                    tmuw(kEntryIdx, indef) = tempval + pieswval
                endif

            enddo


            distmuw = maxval(abs(tmuw - sol%muw))
            distmuv = maxval(abs(tmuv - sol%muv))
            dist = max(distmuw, distmuv)

            sol%muw = tmuw
            sol%muv = tmuv

            if (iter == 1 .and. show_steadyStateDistribution) then
                write(*, '(a4, 7(a20))') 'iter', 'distmuw', 'distmuv', 'sum(muw)', 'sum(muv)', 'total', 'kagg', 'VMass'
                ! write the same content into the variable sep
                write(sep, '(a4, 7(a20))') 'iter', 'distmuw', 'distmuv', 'sum(muw)', 'sum(muv)', 'total', 'kagg', 'VMass'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif
            if (show_steadyStateDistribution) then
                write(*, '(I4, 7(ES20.6))') iter, distmuw, distmuv, &
                    sum(sol%muw), sum(sol%muv), sum(sol%muw) + sum(sol%muv), sol%kagg, VMass
            endif

        enddo ssDist_while


        ! infer invnew and invused from CES cost minimization problem
        invnewgridw = invgridw / conf%invnewratio
        invusedgridw = invnewgridw * conf%usednewratio

        invnewgridv = invgridv / conf%invnewratio
        invusedgridv = invnewgridv * conf%usednewratio

        sol%yagg = sum(ygridw*sol%muw) + sum(ygridv*sol%muv)
        sol%nagg = sum(ngridw*sol%muw) + sum(ngridv*sol%muv)

        sol%invagg = sum(invgridw*sol%muw) + sum(invgridv*sol%muv)
        sol%invnewagg = sum(invnewgridw*sol%muw) + sum(invnewgridv*sol%muv)
        sol%invusedagg = sum(invusedgridw*sol%muw) + sum(invusedgridv*sol%muv)

        sol%divagg = sum(divgridw*sol%muw) + sum(divgridv*sol%muv)

        ! sol%kfagg = sum(kfgridw*sol%muw) + sum(kfgridv*sol%muv)
        ! sol%bvfagg = sum(bkfgridv*sol%muv)

        ! do inde = 1, enum, 1
        !     do indk = 1, muknum, 1
        !         sol%kfagg = sol%kfagg + mukgrid(indk)*
        !     enddo
        ! enddo

        ! write(*, *) sol%kfagg, sol%bvfagg

    ! call plt%initialize(&
    !     usetex = .true., &
    !     xlabel = '$k$', &
    !     ylabel = '$\\epsilon$', &
    !     zlabel = 'muw', &
    !     title = 'W-type (unconstrained) firm distribution',&
    !     mplot3d = .true., &
    !     tight_layout = .true. &
    !     )
    ! call plt%plot_wireframe(&
    !     mukgrid, &
    !     egrid, &
    !     sol%muw, &
    !     label = "muv", &
    !     antialiased = .true., &
    !     linestyle = '-' &
    !     )
    ! call plt%showfig()


    ! inde = 4
    ! call plt%initialize(&
    !     usetex = .true., &
    !     xlabel = '$\\frac{b}{k}$', &
    !     ylabel = '$k$', &
    !     zlabel = 'muv', &
    !     title = 'V-type (constrained) firm distribution, $\\epsilon = $' // num2str(egrid(inde), strfmt),&
    !     mplot3d = .true., &
    !     tight_layout = .true. &
    !     )
    ! call plt%plot_wireframe(&
    !     mubkgrid, &
    !     mukgrid, &
    !     sol%muv(:, :, inde), &
    !     label = "muv", &
    !     antialiased = .true., &
    !     linestyle = '-' &
    !     )
    ! call plt%showfig()




    end subroutine steadyStateDistribution

end module firmDistribution
