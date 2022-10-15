program plot

    use UsedKCollateral
    use pyplot_module
    implicit none

    type(solutions) :: sol
    type(configurations) :: conf

    type(pyplot) :: plt
    integer :: i
    integer(ik) :: istat

    integer(ik) :: inde, indk, indbk
    logical, parameter :: savefig = .true.
    ! logical, parameter :: savefig = .false.
    character(len=*), parameter :: strfmt = 'F8.3'
    character(len=128) :: figname

    real(rk), allocatable :: xgrid(:, :), ygrid(:, :)

    call checkdir(resDir)
    call checkdir(figDir)

    call initGrids()
    call initSol(sol)
    call loadResult(sol)


    write(*, *) "Start Plotting!"

    ! --------------------------------------- !
    ! w value function for unconstrained firm !
    ! --------------------------------------- !

    figname = 'w_bk'
    do indbk = 1, bknum, bknum/2 - 1
        call plt%initialize(&
            xlabel = '$k$', &
            ylabel = '$w$', &
            title = 'value function for unconstrained firm, $\\frac{b}{k} = $' // num2str(bkgrid(indbk), strfmt), &
            legend = .true., &
            tight_layout = .true. &
            )
        do inde = 1, enum, 2
            call plt%add_plot(kgrid, &
                sol%w(indbk, :, inde), &
                label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
                linestyle = '-', linewidth = 1, istat = istat)
        enddo
        if (savefig) then
            call plt%savefig(&
                figDir // trim(figname) // num2str(indbk) // ".pdf", &
                pyfile = figPyDir // trim(figname) // num2str(indbk) // ".py", &
                istat = istat&
                )
        else
            call plt%showfig()
        endif
    enddo

    ! ------------------------------------------------ !
    ! gwk capital decision rule for unconstrained firm !
    ! ------------------------------------------------ !

    figname = 'gwk'
    call plt%initialize(&
        xlabel = '$k$', &
        ylabel = "$k'$", &
        title = 'capital decision rule for unconstrained firm', &
        legend = .true., &
        tight_layout = .true. &
        )
    call plt%add_plot(kgrid, &
        (1.0_rk-delta)*kgrid, &
        label = '$(1-\\delta)k$', &
        linestyle = 'k-.', linewidth = 1, istat = istat)
    do inde = 1, enum, 2
        call plt%add_plot(kgrid, &
            sol%gwk(:, inde), &
            label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
            linestyle = '-', linewidth = 1, istat = istat)
    enddo
    if (savefig) then
        call plt%savefig(&
            figDir // trim(figname) // ".pdf", &
            pyfile = figPyDir // trim(figname) // ".py", &
            istat = istat&
            )
    else
        call plt%showfig()
    endif

    ! ----------------------------------------------- !
    ! gwb future bond decision for unconstrained firm !
    ! ----------------------------------------------- !

    figname = 'gwb'
    call plt%initialize(&
        xlabel = '$k$', &
        ylabel = "$B^{w}$", &
        title = 'future bond decision rule for unconstrained firm', &
        legend = .true., &
        tight_layout = .true. &
        )
    do inde = 1, enum, 2
        call plt%add_plot(kgrid, &
            sol%gwb(:, inde), &
            label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
            linestyle = '-', linewidth = 1, istat = istat)
    enddo
    if (savefig) then
        call plt%savefig(&
            figDir // trim(figname) // ".pdf", &
            pyfile = figPyDir // trim(figname) // ".py", &
            istat = istat&
            )
    else
        call plt%showfig()
    endif

    ! -------------------------------------------------- !
    ! btilde current bond holding for unconstrained firm !
    ! -------------------------------------------------- !

    figname = 'btilde'
    call plt%initialize(&
        xlabel = '$k$', &
        ylabel = "$\\tilde{B}$", &
        title = 'current bond stock for unconstrained firm', &
        legend = .true., &
        tight_layout = .true. &
        )
    do inde = 1, enum, 2
        call plt%add_plot(kgrid, &
            sol%gwb(:, inde), &
            label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
            linestyle = '-', linewidth = 1, istat = istat)
    enddo
    if (savefig) then
        call plt%savefig(&
            figDir // trim(figname) // ".pdf", &
            pyfile = figPyDir // trim(figname) // ".py", &
            istat = istat&
            )
    else
        call plt%showfig()
    endif

    ! ------------------------------------- !
    ! v value function for constrained firm !
    ! ------------------------------------- !

    figname = 'v_bk'
    do indbk = 1, bknum, bknum/2 - 1
        call plt%initialize(&
            xlabel = '$k$', &
            ylabel = '$v$', &
            title = 'value function for constrained firm, $\\frac{b}{k} = $' // num2str(bkgrid(indbk), strfmt), &
            legend = .true., &
            tight_layout = .true. &
            )
        do inde = 1, enum, 2
            call plt%add_plot(kgrid, &
                sol%v(indbk, :, inde), &
                label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
                linestyle = '-', linewidth = 1, istat = istat)
        enddo
        if (savefig) then
            call plt%savefig(&
                figDir // trim(figname) // num2str(indbk) // ".pdf", &
                pyfile = figPyDir // trim(figname) // num2str(indbk) // ".py", &
                istat = istat&
                )
        else
            call plt%showfig()
        endif
    enddo

    ! -------------------------------------------------- !
    ! gvk capital decision function for constrained firm !
    ! -------------------------------------------------- !

    figname = 'gvk_bk'
    do indbk = 1, bknum, bknum/2 - 1
        call plt%initialize(&
            xlabel = '$k$', &
            ylabel = "$k'$", &
            title = 'capital decision rule for constrained firm, $\\frac{b}{k} = $' // num2str(bkgrid(indbk), strfmt), &
            legend = .true., &
            tight_layout = .true. &
            )
        call plt%add_plot(kgrid, &
            (1.0_rk-delta)*kgrid, &
            label = '$(1-\\delta)k$', &
            linestyle = 'k-.', linewidth = 1, istat = istat)
        do inde = 1, enum, 2
            call plt%add_plot(kgrid, &
                sol%gvk(indbk, :, inde), &
                label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
                linestyle = '-', linewidth = 1, istat = istat)
        enddo
        if (savefig) then
            call plt%savefig(&
                figDir // trim(figname) // num2str(indbk) // ".pdf", &
                pyfile = figPyDir // trim(figname) // num2str(indbk) // ".py", &
                istat = istat&
                )
        else
            call plt%showfig()
        endif
    enddo

    ! ------------------------------------------- !
    ! gvb bond decision rule for constrained firm !
    ! ------------------------------------------- !

    figname = 'gvb_bk'
    do indbk = 1, bknum, bknum/2 - 1
        call plt%initialize(&
            xlabel = '$k$', &
            ylabel = "$b'$", &
            title = 'bond decision rule for constrained firm, $\\frac{b}{k} = $' // num2str(bkgrid(indbk), strfmt), &
            legend = .true., &
            tight_layout = .true. &
            )
        do inde = 1, enum, 2
            call plt%add_plot(kgrid, &
                sol%gvk(indbk, :, inde), &
                label = '$\\epsilon = $' // num2str(egrid(inde), strfmt), &
                linestyle = '-', linewidth = 1, istat = istat)
        enddo
        if (savefig) then
            call plt%savefig(&
                figDir // trim(figname) // num2str(indbk) // ".pdf", &
                pyfile = figPyDir // trim(figname) // num2str(indbk) // ".py", &
                istat = istat&
                )
        else
            call plt%showfig()
        endif
    enddo


contains

end program plot
