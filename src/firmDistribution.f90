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

        allocate(tmuv(mubknum, muknum, enum), source = 0.0_rk)

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

                sol%ngridw(indk, inde) = nval
                sol%ygridw(indk, inde) = yval
                sol%kfgridw(indk, inde) = kstar

                ! Here both inv and disinv (div) are not evaluated in market price
                if (isKUp) then
                    sol%invgridw(indk, inde) = kstar - kstay
                else
                    sol%divgridw(indk, inde) = -1.0_rk*(kstar - kstay)
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
                    sol%ygridv(indbk, indk, inde) = yval
                    sol%ngridv(indbk, indk, inde) = nval

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

                    sol%kfgridv(indbk, indk, inde) = kvfval

                    if (wtypedist(indbk, indk, inde)) then
                        sol%bkfgridv(indbk, indk, inde) = 0.0_rk
                    else
                        sol%bkfgridv(indbk, indk, inde) = bvfval / kvfval
                    endif

                    if (isKUp) then
                        sol%invgridv(indbk, indk, inde) = kvfval - kstay
                    else
                        sol%divgridv(indbk, indk, inde) = -1.0_rk*(kvfval - kstay)
                    endif

                enddo

            enddo
        enddo

        ! -------------------------------- !
        ! calculate stational distribution !
        ! -------------------------------- !

        iter = 0_ik
        dist = 2.0_rk*ssDistTol

        ssDist_while: do while (dist > ssDistTol .and. iter <= maxssDistiter)

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

                        kfval = sol%kfgridw(indk, inde)

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

                            kfval = sol%kfgridv(indbk, indk, inde)

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

                            kfval = sol%kfgridv(indbk, indk, inde)
                            kfidx = gridlookup(mukgrid, muknum, kfval)
                            kfwLeft = gridweight(mukgrid, muknum, kfval, kfidx)
                            kfwRight = 1.0_rk - kfwLeft

                            bkfval = sol%bkfgridv(indbk, indk, inde)
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

            sol%scrapk = (1.0_rk - delta) * sol%kagg

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

            if ( kEntryIdx == 1 .and. kEntryLeft > 0.9_rk .and. show_steadyStateDistribution) then
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
                write(*, '(a4, 6(a20))') 'iter', 'distmuw', 'distmuv', 'sum(muw)', 'sum(muv)', 'total', 'kagg'
                ! write the same content into the variable sep
                write(sep, '(a4, 6(a20))') 'iter', 'distmuw', 'distmuv', 'sum(muw)', 'sum(muv)', 'total', 'kagg'
                ! repeatively print = with the length of sep
                write(*, '(a)') repeat('=', len_trim(sep))
            endif
            if (show_steadyStateDistribution) then
                write(*, '(I4, 6(ES20.6))') iter, distmuw, distmuv, &
                    sum(sol%muw), sum(sol%muv), sum(sol%muw) + sum(sol%muv), sol%kagg
            endif

        enddo ssDist_while


        ! infer invnew and invused from CES cost minimization problem
        sol%invnewgridw = sol%invgridw / conf%invnewratio
        sol%invusedgridw = sol%invnewgridw * conf%usednewratio

        sol%invnewgridv = sol%invgridv / conf%invnewratio
        sol%invusedgridv = sol%invnewgridv * conf%usednewratio

        sol%yagg = sum(sol%ygridw*sol%muw) + sum(sol%ygridv*sol%muv)
        sol%nagg = sum(sol%ngridw*sol%muw) + sum(sol%ngridv*sol%muv)

        sol%invagg = sum(sol%invgridw*sol%muw) + sum(sol%invgridv*sol%muv)
        sol%invnewagg = sum(sol%invnewgridw*sol%muw) + sum(sol%invnewgridv*sol%muv)
        sol%invusedagg = sum(sol%invusedgridw*sol%muw) + sum(sol%invusedgridv*sol%muv)

        sol%divagg = sum(sol%divgridw*sol%muw) + sum(sol%divgridv*sol%muv)

        ! I think kfagg and bvfagg should be calculated using kfgridw,
        ! kfgridv and bkfgridv because the policy function is off-grid,
        ! so I linearly interpolate the decision rule on firm's problem
        ! so build decision rule on distribution.

        ! This way
        sol%kfagg = sum(sol%kfgridw*sol%muw) + sum(sol%kfgridv*sol%muv)
        sol%bvfagg = sum(sol%bkfgridv*sol%muv)

        ! ! but not this way (which is how Khan & Thomas (2013) calculated
        ! do inde = 1, enum, 1
        !     do indk = 1, muknum, 1
        !         sol%kfagg = sol%kfagg + mukgrid(indk)*sol%muw(indk, inde)
        !         do indbk = 1, mubknum, 1
        !             bval = mukgrid(indk)*mubkgrid(indbk)
        !             sol%kfagg = sol%kfagg + mukgrid(indk)*sol%muv(indbk, indk, inde)
        !             sol%bvfagg = sol%bvfagg + bval*sol%muv(indbk, indk, inde)
        !         enddo
        !     enddo
        ! enddo

        sol%cagg = sol%yagg &
                   - (1.0_rk - exitprob) * conf%Qbuy * sol%invagg &
                   + (1.0_rk - exitprob) * conf%qsell * sol%divagg &
                   - exitprob * sol%bornK &
                   + exitprob * conf%qsell * sol%scrapk

    end subroutine steadyStateDistribution

end module firmDistribution
