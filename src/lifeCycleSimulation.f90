module lifeCycleSimulation

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use sim
    use parameters
    use firmValueIter
    use firmDistribution
    implicit none

contains

    subroutine firmSimulation(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        integer(ik) :: firmIdx, simIdx, inde, iter, epsIdx
        integer(ik) :: kidx, bkidx, eidx
        integer(ik) :: kidxTomorrow, bkidxTomorrow, eidxTomorrow
        real(rk) :: unifDraw, weighte, weight
        real(rk), dimension(:, :), allocatable :: cumpie
        real(rk), dimension(:), allocatable :: avgk, avgbk, avgik, stdik, rhoik, inaction
        real(rk), dimension(:), allocatable :: lumpy20, lumpyneg, lumpyneg20, cumpiess
        real(rk), dimension(:), allocatable :: reallocation
        integer(ik), dimension(:, :), allocatable :: epsSimIdx, kSimIdx, bkSimIdx
        real(rk), dimension(:, :), allocatable :: kSimWgt, bkSimWgt
        real(rk), dimension(:, :), allocatable :: epsSim, kSim, bkSim, ikSim
        real(rk) :: kagg, kwLeft, kwRight, bkwLeft, bkwRight

        allocate(epsSimIdx(simFirmSize, simLength), source = 0)
        allocate(kSimIdx(simFirmSize, simLength), source = 0)
        allocate(bkSimIdx(simFirmSize, simLength), source = 0)

        allocate(kSimWgt(simFirmSize, simLength), source = 0.0_rk)
        allocate(bkSimWgt(simFirmSize, simLength), source = 0.0_rk)

        allocate(epsSim(simFirmSize, simLength), source = 0.0_rk)
        allocate(kSim(simFirmSize, simLength), source = 0.0_rk)
        allocate(bkSim(simFirmSize, simLength), source = 0.0_rk)
        allocate(ikSim(simFirmSize, simLength), source = 0.0_rk)

        allocate(cumpie(enum, enum))
        allocate(cumpiess(enum))
        allocate(avgk(simLength), source = 0.0_rk)
        allocate(avgbk(simLength), source = 0.0_rk)
        allocate(avgik(simFirmSize), source = 0.0_rk)
        allocate(stdik(simFirmSize), source = 0.0_rk)
        allocate(rhoik(simFirmSize), source = 0.0_rk)
        allocate(inaction(simFirmSize), source = 0.0_rk)
        allocate(lumpy20(simFirmSize), source = 0.0_rk)
        allocate(lumpyneg(simFirmSize), source = 0.0_rk)
        allocate(lumpyneg20(simFirmSize), source = 0.0_rk)
        allocate(reallocation(simFirmSize), source = 0.0_rk)

        kagg = sol%kagg

        call fixSeed()

        do inde = 1, enum, 1
            cumpiess(inde) = sum(piess(1:inde))
        enddo

        cumpie = cumsum(pie, enum, enum, 'row')

        kSim(:, 1) = chi * kagg
        bkSim(:, 1) = 0.0_rk
        kSimIdx(:, 1) = gridlookup(kgrid, knum, chi * kagg)
        kSimWgt(:, 1) = gridweight(kgrid, knum, chi * kagg, kSimIdx(1, 1))
        bkSimIdx(:, 1) = gridlookup(bkgrid, bknum, 0.0_rk)
        bkSimWgt(:, 1) = gridweight(bkgrid, bknum, 0.0_rk, bkSimIdx(1, 1))
        avgk(1) = chi*kagg
        avgbk(1) = 0.0_rk

        do firmIdx = 1, simFirmSize, 1
            call simUniform(unifDraw)
            epsSimIdx(firmIdx, 1) &
                = minloc(cumpiess, 1, mask = cumpiess >= unifDraw)
        enddo

        ! ! generate idio. shock panel
        ! do simIdx = 1, simLength, 1
        !     do firmIdx = 1, simFirmSize, 1
        !         call simUniform(unifDraw)
        !         epsSimIdx(firmIdx, simIdx) &
        !             = minloc(cumpiess, 1, mask = cumpiess >= unifDraw)
        !     enddo
        ! enddo

        do simIdx = 2, simLength, 1
            do firmIdx = 1, simFirmSize, 1

                call simMarkov(pie, &
                    epsSimIdx(firmIdx, simIdx), &
                    epsSimIdx(firmIdx, simIdx - 1))

                kwLeft = kSimWgt(firmIdx, simIdx-1)
                kwRight = 1.0_rk - kwLeft
                bkwLeft = bkSimWgt(firmIdx, simIdx-1)
                bkwRight = 1.0_rk - bkwLeft
                kidx = kSimIdx(firmIdx, simIdx-1)
                bkidx = bkSimIdx(firmIdx, simIdx-1)
                eidx = epsSimIdx(firmIdx, simIdx-1)

                kSim(firmIdx, simIdx) &
                    = bkwLeft * ( kwLeft*sol%gvk(bkidx, kidx, eidx) + kwRight*sol%gvk(bkidx, kidx+1, eidx) ) &
                    + bkwRight * ( kwLeft*sol%gvk(bkidx+1, kidx, eidx) + kwRight*sol%gvk(bkidx+1, kidx+1, eidx) )
                kSimIdx(firmIdx, simIdx) &
                    = gridlookup(kgrid, knum, kSim(firmIdx, simIdx))
                kSimWgt(firmIdx, simIdx) &
                    = gridweight(kgrid, knum, kSim(firmIdx, simIdx), kSimIdx(firmIdx, simIdx))

                bkSim(firmIdx, simIdx) &
                    = bkwLeft * ( kwLeft*sol%gvb(bkidx, kidx, eidx) + kwRight*sol%gvb(bkidx, kidx+1, eidx) ) &
                    + bkwRight * ( kwLeft*sol%gvb(bkidx+1, kidx, eidx) + kwRight*sol%gvb(bkidx+1, kidx+1, eidx) )
                bkSimIdx(firmIdx, simIdx) &
                    = gridlookup(bkgrid, bknum, bkSim(firmIdx, simIdx) / kSim(firmIdx, simIdx))
                bkSimWgt(firmIdx, simIdx) &
                    = gridweight(bkgrid, bknum, bkSim(firmIdx, simIdx) / kSim(firmIdx, simIdx), bkSimIdx(firmIdx, simIdx))

                ! if (kSim(firmIdx, simIdx) - (1.0_rk - delta)*kSim(firmIdx, simIdx - 1) > 0.0_rk) then
                !     ikSim(firmIdx, simIdx - 1) &
                !         = ( conf%Qbuy*( kSim(firmIdx, simIdx) - (1.0_rk - delta)*kSim(firmIdx, simIdx - 1) ) ) &
                !             / kSim(firmIdx, simIdx - 1)
                ! else
                !     ikSim(firmIdx, simIdx - 1) &
                !         = ( conf%qsell*( kSim(firmIdx, simIdx) - (1.0_rk - delta)*kSim(firmIdx, simIdx - 1) ) ) &
                !             / kSim(firmIdx, simIdx - 1)
                ! endif

                ikSim(firmIdx, simIdx) &
                    = ( ( kSim(firmIdx, simIdx) - (1.0_rk - delta)*kSim(firmIdx, simIdx - 1) ) ) &
                        / kSim(firmIdx, simIdx - 1)

            enddo
            avgk(simIdx) = sum(kSim(:, simIdx)) / simFirmSize
            avgbk(simIdx) = sum(bkSim(:, simIdx)) / simFirmSize
        enddo

        do firmIdx = 1, simFirmSize, 1
            avgik(firmIdx) = sum(ikSim(firmIdx, simLength-simSampleLength + 1:simLength)) / simSampleLength
            stdik(firmIdx) &
                = sqrt( &
                    sum( &
                        ( &
                            ikSim(firmIdx, simLength-simSampleLength + 1:simLength) - avgik(firmIdx) &
                        )**2.0_rk &
                    ) / ( simSampleLength - 1 ) &
                )

            rhoik(firmIdx) &
                = sum( ( &
                           ( ikSim(firmIdx, simLength-simSampleLength + 2:simLength) - avgik(firmIdx) ) * &
                           ( ikSim(firmIdx, simLength-simSampleLength + 1:simLength - 1) - avgik(firmIdx) ) &
                        ) &
                    ) / ( simSampleLength - 1 )
            rhoik(firmIdx) = rhoik(firmIdx) / ( stdik(firmIdx)**2.0_rk )

            inaction(firmIdx) &
                = sum(&
                    merge(1.0_rk, 0.0_rk, abs(ikSim(firmIdx, simLength-simSampleLength+1:simLength)) <= 0.01_rk) &
                    ) / ( simSampleLength )

            ! reallocation(firmIdx) &
            !     = sum( merge(conf%qsell*ikSim(firmIdx, simLength-simSampleLength:simLength - 1)*&
            !             kSim(firmIdx, simLength-simSampleLength:simLength - 1), &
            !             0.0_rk, &
            !             ikSim(firmIdx, simLength-simSampleLength:simLength - 1) < 0.0_rk &
            !             ) ) / &
            !       sum( merge(conf%Qbuy*ikSim(firmIdx, simLength-simSampleLength:simLength - 1)*&
            !             kSim(firmIdx, simLength-simSampleLength:simLength - 1), &
            !             0.0_rk, &
            !             ikSim(firmIdx, simLength-simSampleLength:simLength - 1) > 0.0_rk &
            !             ) )

            lumpy20(firmIdx) &
                = sum(&
                    merge(1.0_rk, 0.0_rk, ikSim(firmIdx, simLength-simSampleLength+1:simLength) > 0.2_rk) &
                    ) / ( simSampleLength )

            lumpyneg(firmIdx) &
                = sum(&
                    merge(1.0_rk, 0.0_rk, ikSim(firmIdx, simLength-simSampleLength+1:simLength) < -0.015_rk) &
                    ) / ( simSampleLength )

            lumpyneg20(firmIdx) &
                = sum(&
                    merge(1.0_rk, 0.0_rk, ikSim(firmIdx, simLength-simSampleLength+1:simLength) < -0.2_rk) &
                    ) / ( simSampleLength )

        enddo

        write(*, '(a)') "Parameters:"
        write(*, '(A10, F20.10)') 'zeta   ', zeta
        write(*, '(A10, F20.10)') 'beta   ', beta
        write(*, '(A10, F20.10)') 'delta  ', delta
        write(*, '(A10, F20.10)') 'psi    ', psi
        write(*, '(A10, F20.10)') 'alpha  ', alpha
        write(*, '(A10, F20.10)') 'nu     ', nu
        write(*, '(A10, F20.10)') 'rho_z  ', rho_z
        write(*, '(A10, F20.10)') 'sigma_z', sigma_z
        write(*, '(A10, F20.10)') 'eta    ', eta
        write(*, '(A10, F20.10)') 's      ', s
        write(*, '(A10, F20.10)') 'gamma  ', gamma
        write(*, '(A10, F20.10)') 'rho_e  ', rho_e
        write(*, '(A10, F20.10)') 'sigma_e', sigma_e

        write(*, '(a)') "Calibration target:"
        write(*, '(A30, F20.10)') "K irreversibility (q/Q) = ", conf%qsell / conf%Qbuy
        write(*, '(A30, A20, A20)') '', 'model', 'data target'
        write(*, '(a70)') repeat('=', 70)
        write(*, '(A30, F20.10, F20.10)') "K/Y = ", sol%kagg / sol%yagg, 2.39_rk
        write(*, '(A30, F20.10, F20.10)') "I/K = ",  ( (1.0_rk - exitprob) * conf%Qbuy * sol%invagg &
                                                        - (1.0_rk - exitprob) * conf%qsell * sol%divagg &
                                                        + exitprob * sol%bornK &
                                                        - exitprob * conf%qsell * sol%scrapk ) / sol%kagg, 0.069_rk
        ! write(*, '(A30, F20.10, F20.10)') "Inew/K = ", (sol%invnewagg - conf%qsell*sol%divagg) / sol%kagg, 0.069_rk
        write(*, '(A30, F20.10, F20.10)') "reallocation share = ", &
                            ( (1.0_rk - exitprob)*conf%qsell*sol%divagg ) &
                            / ( (1.0_rk - exitprob)*conf%Qbuy*sol%invagg),&
                            0.28_rk
                            ! ( (1.0_rk - exitprob) * conf%qsell * sol%divagg + exitprob * conf%qsell * sol%scrapk ) / &
                            ! ( (1.0_rk - exitprob) * conf%Qbuy * sol%invagg &
                            ! + exitprob * sol%bornK ), 0.28_rk
        write(*, '(A30, f20.10, F20.10)') 'average std(i/k) = ', sum(stdik) / simFirmSize, 0.337_rk
        write(*, '(A30, f20.10, F20.10)') 'average rho(i/k) = ', sum(rhoik) / simFirmSize, 0.058_rk
        write(*, '(A30, f20.10, F20.10)') 'lumpy inv > 20% = ', sum(lumpy20) / simFirmSize, 0.186_rk
        write(*, '(a)')
        write(*, '(a)') "Untargeted moment:"
        write(*, '(A30, A20, A20)') '', 'model', 'data target'
        write(*, '(a70)') repeat('=', 70)
        write(*, '(A30, f20.10, F20.10)') 'average (i/k) = ', sum(avgik) / simFirmSize, 0.122_rk
        write(*, '(A30, f20.10, F20.10)') 'inaction (i/k < 1%) = ', sum(inaction) / simFirmSize, 0.081_rk
        write(*, '(A30, f20.10, F20.10)') 'lumpy disinv = ', sum(lumpyneg) / simFirmSize, 0.104_rk
        write(*, '(A30, f20.10, F20.10)') 'lumpy disinv > 20% = ', sum(lumpyneg20) / simFirmSize, 0.018_rk



    end subroutine firmSimulation


end module lifeCycleSimulation
