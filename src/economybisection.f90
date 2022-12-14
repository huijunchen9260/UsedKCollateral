module economyBisection

    use iso_Fortran_env, only: rk => real64, ik => int32
    use numerics
    use io
    use parameters
    use firmValueIter
    use firmDistribution
    use nlopt_wrap
    use nlopt_enum
    implicit none

    integer(ik) :: iter = 0

contains

    subroutine OneDimBisection(sol, conf)
        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf

        integer(ik) :: iter
        real(rk) :: wlow, whigh, fwlow, fwhigh, fwval, dist

        write(*, '(a)') ''
        write(*, *) "Run KT13 Replication"
        write(*, '(a)') ''

        wlow = 1.04_rk
        whigh = 1.1_rk

        write(*, '(A)') ''
        write(sep, '(a, 2F9.4)') "Bisection initization at wlow: ", wlow
        write(*, '(a)') repeat('*', len_trim(sep))
        write(*, '(a, 2F9.4)') "Bisection initization at wlow: ", wlow
        write(*, '(a)') repeat('*', len_trim(sep))
        write(*, '(A)') ''
        call initSol(sol)
        conf%wval = wlow
        conf%qsell = 0.954_rk
        conf%Qbuy = 1.0_rk
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))

        call wvalueiter(sol, conf)
        call minSavingPolicy(sol, conf)
        call vvalueiter(sol, conf)
        call steadyStateDistribution(sol, conf)

        fwlow = conf%wval - psi*sol%cagg

        write(*, '(A)') ''
        write(sep, '(a, 2F9.4)') "Bisection initization at whigh: ", whigh
        write(*, '(a)') repeat('*', len_trim(sep))
        write(*, '(a, 2F9.4)') "Bisection initization at whigh: ", whigh
        write(*, '(a)') repeat('*', len_trim(sep))
        write(*, '(A)') ''
        call initSol(sol)
        conf%wval = whigh
        conf%qsell = 0.954_rk
        conf%Qbuy = 1.0_rk
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))

        call wvalueiter(sol, conf)
        call minSavingPolicy(sol, conf)
        call vvalueiter(sol, conf)
        call steadyStateDistribution(sol, conf)

        fwhigh = conf%wval - psi*sol%cagg

        if (fwlow*fwhigh > 0.0_rk) error stop "fail to bisect between wlow and whigh"

        iter = 0_ik
        dist = 2.0_rk

        do while (dist >= bisectTol .and. iter <= maxbisectiter)

            iter = iter + 1

            conf%wval = (wlow + whigh) / 2.0_rk

            write(sep, '(a, I0, a, 2F9.4)') "Bisection iter ", iter, " at wage: ", conf%wval
            write(*, '(a)') repeat('*', len_trim(sep))
            write(*, '(a, I0, a, 2F9.4)') "Bisection iter ", iter, " at wage: ", conf%wval
            write(*, '(a)') repeat('*', len_trim(sep))

            conf%qsell = 0.954_rk
            conf%Qbuy = 1.0_rk
            conf%pval = psi/conf%wval
            conf%pfval = conf%pval
            conf%qbval = beta * conf%pfval/conf%pval
            conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
            conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))

            call wvalueiter(sol, conf)
            call minSavingPolicy(sol, conf)
            call vvalueiter(sol, conf)
            call steadyStateDistribution(sol, conf)

            fwval = conf%wval - psi*sol%cagg

            if (fwlow*fwval > 0.0_rk) then
                wlow = conf%wval; fwlow = fwval
            else
                whigh = conf%wval; fwhigh = fwval
            endif

            dist = whigh - wlow

            write(*, '(A)') ''
            write(*, '(a, I0)') "Bisection: iter = ", iter
            write(*, '(a, 3(a, F9.6))') tab, 'q = ', conf%qsell, &
                                        ', Q = ', conf%Qbuy, &
                                        ', w = ', conf%wval
            write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', fwval
            write(*, '(A)') ''

        enddo


    end subroutine OneDimBisection

    subroutine TwoDimBisection(conf)
        type(solutions):: solA, solB, solC, solD
        type(configurations), intent(inout) :: conf

        integer(ik) :: iter
        real(rk) :: qlow, qhigh, wlow, whigh
        real(rk) :: fqlow, fqhigh, fwlow, fwhigh, fwval, fqval
        real(rk) :: dist, longest, weight
        ! points
        real(rk), dimension(:), allocatable :: A, B, C, D, E, zeros
        ! points function value
        real(rk), dimension(:), allocatable :: evalA, evalB, evalC, evalD, evalE
        ! triangles
        real(rk), dimension(:, :), allocatable :: R, L, T
        ! triangles function value
        real(rk), dimension(:, :), allocatable :: evalR, evalL, evalT

        allocate(A(2), source = 0.0_rk)
        allocate(B(2), source = 0.0_rk)
        allocate(C(2), source = 0.0_rk)
        allocate(D(2), source = 0.0_rk)
        allocate(E(2), source = 0.0_rk)
        allocate(zeros(2), source = 0.0_rk)
        allocate(R(3, 2), source = 0.0_rk)
        allocate(L(3, 2), source = 0.0_rk)
        allocate(T(3, 2), source = 0.0_rk)
        allocate(evalR(3, 2), source = 0.0_rk)
        allocate(evalL(3, 2), source = 0.0_rk)

        wlow = 0.95_rk
        whigh = 1.2_rk
        qlow = 0.8_rk
        qhigh = 0.97_rk

        call initSol(sol)

        A = [0.8_rk, 0.8_rk]

        if (show_bisection) then

            write(*, '(a)')
            write(sep, '(a, 2F9.4)') "Bisection initization at point A: ", A(1), A(2)
            write(*, '(a)') repeat('*', len_trim(sep))
            write(*, '(a, 2F9.4)') "Bisection initization at point A: ", A(1), A(2)
            write(*, '(a)') repeat('*', len_trim(sep))
            write(*, '(a)')

        endif

        conf%wval = A(1)
        conf%qsell = A(2)
        conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))
        call wvalueiter(sol, conf)
        call minSavingPolicy(sol, conf)
        call vvalueiter(sol, conf)
        call steadyStateDistribution(sol, conf)
        fwval = conf%wval - psi*sol%cagg
        fqval = sol%invusedagg - sol%divagg
        evalA = [fwval, fqval]

        if (show_bisection) then
            write(*, '(a)')
            write(*, '(a, 4F9.6)') "Bisection initialized on (p, w, q, Q) = ", &
                                    conf%pval, conf%wval, conf%qsell, conf%Qbuy
            write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', evalA(1), &
                                        ', Inv_used - DisInv = ', evalA(2)
            write(*, '(a)')
        endif


        call initSol(sol)
        C = [1.3_rk, 1.0_rk - gamma - 0.001_rk]
        ! C = [1.3_rk, 0.99_rk]

        if (show_bisection) then
            write(*, '(a)')
        write(sep, '(a, 2F9.4)') "Bisection initization at point C: ", C(1), C(2)
        write(*, '(a)') repeat('*', len_trim(sep))
        write(*, '(a, 2F9.4)') "Bisection initization at point C: ", C(1), C(2)
        write(*, '(a)') repeat('*', len_trim(sep))
            write(*, '(a)')
        endif

        conf%wval = C(1)
        conf%qsell = C(2)
        conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))


        call wvalueiter(sol, conf)
        call minSavingPolicy(sol, conf)
        call vvalueiter(sol, conf)
        call steadyStateDistribution(sol, conf)

        fwhigh = conf%wval - psi*sol%cagg
        fqhigh = sol%invusedagg - sol%divagg
        fwval = fwhigh; fqval = fqhigh

        evalC = [fwval, fqval]
        if (show_bisection) then
            write(*, '(a)')
        write(*, '(a, 4F9.6)') "Bisection initialized on (p, w, q, Q) = ", &
                                conf%pval, conf%wval, conf%qsell, conf%Qbuy
        write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', fwval, &
                                    ', Inv_used - DisInv = ', fqval
            write(*, '(a)')
        endif

        call initSol(sol)
        ! B = [1.0_rk, 0.91_rk]
        ! B = [0.8_rk, 0.99_rk]
        B = [0.8_rk, 1.0_rk - gamma - 0.001_rk]
        if (show_bisection) then
            write(*, '(a)')
        write(sep, '(a, 2F9.4)') "Bisection initization at point B: w, q = ", B(1), B(2)
        write(*, '(a)') repeat('*', len_trim(sep))
        write(*, '(a, 2F9.4)') "Bisection initization at point B: ", B(1), B(2)
        write(*, '(a)') repeat('*', len_trim(sep))
            write(*, '(a)')
        endif
        conf%wval = B(1)
        conf%qsell = B(2)
        conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))
        call wvalueiter(sol, conf)
        call minSavingPolicy(sol, conf)
        call vvalueiter(sol, conf)
        call steadyStateDistribution(sol, conf)

        fwlow = conf%wval - psi*sol%cagg
        fqhigh = sol%invusedagg - sol%divagg
        fwval = fwlow; fqval = fqhigh

        evalB = [fwval, fqval]
        if (show_bisection) then
            write(*, '(a)')
        write(*, '(a, 4F9.6)') "Bisection initialized on (p, w, q, Q) = ", &
                                conf%pval, conf%wval, conf%qsell, conf%Qbuy
        write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', fwval, &
                                    ', Inv_used - DisInv = ', fqval
            write(*, '(a)')
        endif


        ! call initSol(sol)
        ! D = [1.1_rk, 0.85_rk]
        ! write(sep, '(a, 2F9.4)') "Bisection initization at point D: ", D(1), D(2)
        ! write(*, '(a)') repeat('*', len_trim(sep))
        ! write(*, '(a, 2F9.4)') "Bisection initization at point D: ", D(1), D(2)
        ! write(*, '(a)') repeat('*', len_trim(sep))
        ! conf%wval = D(1)
        ! conf%qsell = D(2)
        ! conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
        ! conf%pval = psi/conf%wval
        ! conf%pfval = conf%pval
        ! conf%qbval = beta * conf%pfval/conf%pval
        ! conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        ! conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))


        ! call wvalueiter(sol, conf)
        ! call minSavingPolicy(sol, conf)
        ! call vvalueiter(sol, conf)
        ! call steadyStateDistribution(sol, conf)

        ! fwhigh = conf%wval - psi*sol%cagg
        ! fqlow = sol%invusedagg - sol%divagg
        ! fwval = fwhigh; fqval = fqlow

        ! evalD = [fwval, fqval]
        ! write(*, *) "Bisection initialized on (w, q) = (high, low):"
        ! write(*, '(a, 3(a, F9.6))') tab, 'q = ', conf%qsell, &
        !                             ', Q = ', conf%Qbuy, &
        !                             ', w = ', conf%wval
        ! write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', fwval, &
        !                             ', Inv_used - DisInv = ', fqval

        ! R = transpose(reshape([ A, B, C ], [2, 3]))


        ! L = transpose(reshape([ A, D, C ], [2, 3]))


        ! evalR = transpose(reshape([ evalA, evalB, evalC ], [2, 3]))
        ! evalL = transpose(reshape([ evalA, evalD, evalC ], [2, 3]))

        T = transpose(reshape([ A, B, C ], [2, 3]))
        evalT = transpose(reshape([ evalA, evalB, evalC ], [2, 3]))

        ! E = (A+B+C+D) / 4
        call rotate(T, evalT)
        E = (A+B+C) / 3.0_rk
        D = (A+B) / 2.0_rk
        ! longest = max(norm(A-C), norm(D-B))

            call initSol(sol)
            conf%wval = D(1)
            conf%qsell = D(2)
            conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
            conf%pval = psi/conf%wval
            conf%pfval = conf%pval
            conf%qbval = beta * conf%pfval/conf%pval
            conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
            conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))
            call wvalueiter(sol, conf)
            call minSavingPolicy(sol, conf)
            call vvalueiter(sol, conf)
            call steadyStateDistribution(sol, conf)
            fwval = conf%wval - psi*sol%cagg
            fqval = sol%invusedagg - sol%divagg

            evalD = [fwval, fqval]
            longest = norm(B-A)
            R = transpose(reshape([ A, D, C ], [2, 3]))
            L = transpose(reshape([ D, B, C ], [2, 3]))
            evalR = transpose(reshape([evalA, evalD, evalC], [2, 3]))
            evalL = transpose(reshape([evalD, evalB, evalC], [2, 3]))

            ! evalE = (evalA + evalB + evalC) / 3.0_rk
            ! linear interpolate on f(E) using f(C) and f(D)
            weight = norm(E - C) / norm(D - C)
            if (weight >= 1.0_rk) weight = 1.0_rk
            if (weight <= 0.0_rk) weight = 0.0_rk
            evalE = weight*evalC + (1.0_rk - weight)*evalD

        iter = 0_ik
        dist = max(norm(evalE), longest)

        do while (dist >= bisectTol .and. iter <= maxbisectiter)

            iter = iter + 1

            if (Ltest(evalR, zeros)) then
                T = R
                evalT = evalR
            elseif (Ltest(evalL, zeros)) then
                T = L
                evalT = evalL
            else
                ! error stop "Neither F(R) or F(L) contains (0, 0)"
                write(*, *) "Neither F(R) or F(L) contains (0, 0)"
                iter = iter - 1
                exit
            endif

            ! rotate the triangle so that AB is the longest
            call rotate(T, evalT)

            A = T(1, :)
            B = T(2, :)
            C = T(3, :)
            evalA = evalT(1, :)
            evalB = evalT(2, :)
            evalC = evalT(3, :)

            E = (A+B+C)/3.0_rk
            D = (A+B)/2.0_rk

        if (show_bisection) then
            write(*, '(a)')
            write(sep, '(a, I0, a, 2F9.4)') "Bisection iter ", iter, " at point D: ", D(1), D(2)
            write(*, '(a)') repeat('*', len_trim(sep))
            write(*, '(a, I0, a, 2F9.4)') "Bisection iter ", iter, " at point D: ", D(1), D(2)
            write(*, '(a)') repeat('*', len_trim(sep))
            write(*, *) new_line('')
            write(*, '(a)')
        endif

            call initSol(sol)
            conf%wval = D(1)
            conf%qsell = D(2)
            conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
            conf%pval = psi/conf%wval
            conf%pfval = conf%pval
            conf%qbval = beta * conf%pfval/conf%pval
            conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
            conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))
            call wvalueiter(sol, conf)
            call minSavingPolicy(sol, conf)
            call vvalueiter(sol, conf)
            call steadyStateDistribution(sol, conf)
            fwval = conf%wval - psi*sol%cagg
            fqval = sol%invusedagg - sol%divagg

            evalD = [fwval, fqval]
            longest = norm(B-A)
            R = transpose(reshape([ A, D, C ], [2, 3]))
            L = transpose(reshape([ D, B, C ], [2, 3]))
            evalR = transpose(reshape([evalA, evalD, evalC], [2, 3]))
            evalL = transpose(reshape([evalD, evalB, evalC], [2, 3]))

            ! evalE = (evalA + evalB + evalC) / 3.0_rk
            ! linear interpolate on f(E) using f(C) and f(D)
            weight = norm(E - C) / norm(D - C)
            if (weight >= 1.0_rk) weight = 1.0_rk
            if (weight <= 0.0_rk) weight = 0.0_rk
            evalE = weight*evalC + (1.0_rk - weight)*evalD
            dist = max(norm(evalE), longest)

        if (show_bisection) then
            write(*, '(a)')
            write(*, '(a, I0, 2(a, F9.6))') "Bisection: iter = ", iter, &
                                            ", norm(E) = ", norm(evalE), &
                                            ", longest = ", longest

            write(*, '(2a, 4F9.6)') tab, "(p, w, q, Q) = ", &
                                    conf%pval, conf%wval, conf%qsell, conf%Qbuy
            write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', fwval, &
                                        ', Inv_used - DisInv = ', fqval
            write(*, '(a)')
        endif


        enddo


        conf%wval = E(1)
        conf%qsell = E(2)
        conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))

        ! if (show_bisection) then
            write(*, '(a)')
        write(*, '(a, I0, 2(a, F9.6))') "Bisection Ends: iter = ", iter, &
                                        ", norm(E) = ", norm(evalE), &
                                        ", longest = ", longest
        write(*, '(2a, 4F9.6)') tab, "(p, w, q, Q) = ", &
                                conf%pval, conf%wval, conf%qsell, conf%Qbuy
        write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', evalE(1), &
                                    ', Inv_used - DisInv = ', evalE(2)
            write(*, '(a)')
        ! endif



    end subroutine TwoDimBisection

    function norm(pt)
        real(rk), dimension(2), intent(in) :: pt
        real(rk) :: norm

        norm = sqrt(abs(pt(1))**2 + abs(pt(2))**2)
    end function norm

    function L(A, B, X)
        real(rk), dimension(2), intent(in) :: A, B, X
        real(rk) :: L

        L = ( B(2) - A(2) ) * ( X(1) - A(1) ) - ( B(1) - A(1) ) * ( X(2) - A(2) )
    end function L

    function Ltest(T, V)
        real(rk), intent(in) :: T(3, 2), V(2)
        logical :: Ltest
        real(rk) :: A(2), B(2), C(2), LAB, LBC, LCA

        A = T(1, :)
        B = T(2, :)
        C = T(3, :)

        LAB = L(A, B, V)*L(A, B, C)
        LBC = L(B, C, V)*L(B, C, A)
        LCA = L(C, A, V)*L(C, A, B)

        Ltest = .false.
        if (LAB >= 0.0_rk .and. LBC >= 0.0_rk .and. LCA >= 0.0_rk) Ltest = .true.
    end function Ltest

    subroutine rotate(T, evalT)
        real(rk), intent(inout) :: T(3, 2), evalT(3, 2)
        real(rk) :: A(2), B(2), C(2)
        real(rk) :: evalA(2), evalB(2), evalC(2)
        real(rk) :: AB, BC, CA

        A = T(1, :)
        B = T(2, :)
        C = T(3, :)

        evalA = evalT(1, :)
        evalB = evalT(2, :)
        evalC = evalT(3, :)


        AB = norm(B - A)
        BC = norm(B - C)
        CA = norm(C - A)

        if (AB >= BC .and. BC >= CA) then
            T = transpose(reshape([ A, B, C ], [2, 3]))
            evalT = transpose(reshape([evalA, evalB, evalC], [2, 3]))
        elseif (AB >= CA .and. CA >= BC) then
            T = transpose(reshape([ B, A, C ], [2, 3]))
            evalT = transpose(reshape([ evalB, evalA, evalC ], [2, 3]))
        elseif (BC >= CA .and. CA >= AB) then
            T = transpose(reshape([ B, C, A ], [2, 3]))
            evalT = transpose(reshape([ evalB, evalC, evalA ], [2, 3]))
        elseif (BC >= AB .and. AB >= CA) then
            T = transpose(reshape([ C, B, A ], [2, 3]))
            evalT = transpose(reshape([ evalC, evalB, evalA ], [2, 3]))
        elseif (CA >= AB .and. AB >= BC) then
            T = transpose(reshape([ C, A, B ], [2, 3]))
            evalT = transpose(reshape([ evalC, evalA, evalB ], [2, 3]))
        else
            T = transpose(reshape([ A, C, B ], [2, 3]))
            evalT = transpose(reshape([ evalA, evalC, evalB ], [2, 3]))
        endif

    end subroutine rotate

    subroutine NelderMeadSimplex(sol, conf)

        type(solutions), intent(inout) :: sol
        type(configurations), intent(inout) :: conf
        type(nlopt_opt) :: opt
        real(rk), dimension(2) :: guess, UB, LB
        real(rk) :: fmin
        integer(ik) :: stat

        ! [wage, qsell]
        LB(1) = 0.95_rk
        UB(1) = 1.1_rk
        LB(2) = 0.85_rk
        UB(2) = 0.98_rk

        guess(1) = 1.058_rk
        guess(2) = 0.965_rk

        call create(opt, algorithm_from_string("LN_NELDERMEAD"), 2)
        call opt%set_lower_bounds(LB)
        call opt%set_upper_bounds(UB)

        associate (f => nlopt_func(bisect_Obj, conf))
            call opt%set_xtol_rel(tol)
            call opt%set_min_objective(f)
            call opt%optimize(guess, fmin, stat)
        end associate
        call destroy(opt)

        if (stat < NLOPT_SUCCESS) then
            write (*, "(A,I5)") "HH_bh_decision: NLopt failed with code ", stat
        end if

        conf%wval = guess(1)
        conf%qsell = guess(2)

    end subroutine NelderMeadSimplex

    function bisect_Obj(guess, gradient, func_data) result(f)

        real(rk), dimension(:), intent(in) :: guess
        real(rk), dimension(:), intent(inout), optional :: gradient
        class(*), intent(in), optional :: func_data
        type(configurations) :: conf
        type(solutions) :: sol
        real(rk) :: f, fwval, fqval

        iter = iter + 1

        IF (PRESENT(gradient)) THEN
            ! Why?!
        END IF

        select type(func_data)
            type is (configurations)
                conf = func_data
        end select

        call initSol(sol)
        conf%wval = guess(1)
        conf%qsell = guess(2)
        conf%Qbuy = ( eta + (1 - eta)*(conf%qsell + gamma)**(1-s) )**( 1/(1-s) )
        conf%pval = psi/conf%wval
        conf%pfval = conf%pval
        conf%qbval = beta * conf%pfval/conf%pval
        conf%usednewratio = ( ( 1-eta ) / eta ) * ( ( conf%qsell + gamma )**(-s) )
        conf%invnewratio = ( eta**(1/s) + (1.0_rk-eta)**(1/s) * ( conf%usednewratio )**((s-1.0_rk)/s) )**(s/(s-1.0_rk))
        call wvalueiter(sol, conf)
        call minSavingPolicy(sol, conf)
        call vvalueiter(sol, conf)
        call steadyStateDistribution(sol, conf)
        fwval = conf%wval - psi*sol%cagg
        fqval = sol%invusedagg - sol%divagg

        f = sqrt(fwval**2 + fqval**2)

        if (show_bisection) then
            write(*, '(a)')
        write(*, '(a, I0, (a, F9.6))') "Bisection Ends: iter = ", iter, &
                                        ", norm = ", f
        write(*, '(2a, 4F9.6)') tab, "(p, w, q, Q) = ", &
                                conf%pval, conf%wval, conf%qsell, conf%Qbuy
        write(*, '(a, 2(a, F9.6))') tab, 'w - psi*cagg = ', fwval, &
                                    ', Inv_used - DisInv = ',fqval
            write(*, '(a)')
        endif

    end function bisect_Obj

end module economyBisection
