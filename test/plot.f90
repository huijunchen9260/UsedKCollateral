program plot

    use UsedKCollateral
    use pyplot_module
    implicit none

    ! type(solutions) :: sol
    ! type(configurations) :: conf
    ! type(pyplot) :: plt
    ! character(len=*), parameter :: strfmt = 'F8.3'

    integer :: i
    integer(ik) :: istat

    integer(ik) :: inde, indk, indbk
    ! logical, parameter :: savefig = .true.
    logical, parameter :: savefig = .false.
    character(len=128) :: figname

    real(rk), allocatable :: xgrid(:, :), ygrid(:, :)

    real(rk) :: wtruereal(bknum, knum, enum)

    call checkdir(resDir)
    call checkdir(figDir)

    call initGrids()
    call initSol(sol)
    call loadResult(sol)


    write(*, *) "Start Plotting!"

    inde = 4
    call plt%initialize(&
        usetex = .true., &
        xlabel = '$b/k$', &
        ylabel = '$k$', &
        zlabel = "$k'$", &
        title = 'V-type (constrained) capital decision function',&
        mplot3d = .true., &
        tight_layout = .true. &
        )
    call plt%plot_wireframe(&
        bkgrid, &
        kgrid, &
        sol%gvk(:, :, inde), &
        label = "gvk", &
        antialiased = .true., &
        linestyle = '-' &
        )
    if (savefig) then
        call plt%savefig(&
            figDir // "gvk_3D.pdf", &
            pyfile = figPyDir // "gvk_3D.py" &
            )
    else
        call plt%showfig()
    endif

    stop

    inde = 4
    call plt%initialize(&
        usetex = .true., &
        ylabel = '$k$', &
        xlabel = '$\\frac{b}{k}$', &
        zlabel = 'muv', &
        title = 'V-type (constrained) firm distribution, $\\epsilon = $' // num2str(egrid(inde), strfmt),&
        mplot3d = .true., &
        tight_layout = .true. &
        )

    call plt%plot_wireframe(&
        mubkgrid, &
        mukgrid, &
        sol%muv(:, :, inde), &
        label = "muv", &
        antialiased = .true., &
        linestyle = '-' &
        )
    if (savefig) then
        call plt%savefig(&
            figDir // "muv.pdf", &
            pyfile = figPyDir // "muv.py" &
            )
    else
        call plt%showfig()
    endif

    stop

    call plt%initialize(&
        usetex = .true., &
        xlabel = '$k$', &
        ylabel = '$\\epsilon$', &
        zlabel = "$k'$", &
        title = 'W-type (unconstrained) capital decision function',&
        mplot3d = .true., &
        tight_layout = .true. &
        )
    call plt%plot_wireframe(&
        kgrid, &
        egrid, &
        sol%gwk, &
        label = "gwk", &
        antialiased = .true., &
        linestyle = '-' &
        )
    if (savefig) then
        call plt%savefig(&
            figDir // "gwk_3D.pdf", &
            pyfile = figPyDir // "gwk_3D.py" &
            )
    else
        call plt%showfig()
    endif

    wtruereal = merge(1.0_rk, 0.0_rk, sol%wtrue)

    ! do inde = 1, enum, 2

    ! call plt%initialize(&
    !     usetex = .true., &
    !     xlabel = '$b/k$', &
    !     ylabel = '$k$', &
    !     zlabel = "$k'$", &
    !     title = 'V-type (constrained) capital decision function',&
    !     mplot3d = .true., &
    !     tight_layout = .true. &
    !     )
    ! call plt%plot_wireframe(&
    !     bkgrid, &
    !     kgrid, &
    !     wtruereal(:, :, inde), &
    !     label = "gvk", &
    !     antialiased = .true., &
    !     linestyle = '-' &
    !     )
    ! if (savefig) then
    !     call plt%savefig(&
    !         figDir // "gvk_3D.pdf", &
    !         pyfile = figPyDir // "gvk_3D.py" &
    !         )
    ! else
    !     call plt%showfig()
    ! endif
    ! enddo


    inde = 4
    call plt%initialize(&
        usetex = .true., &
        xlabel = '$\\frac{b}{k}$', &
        ylabel = '$k$', &
        zlabel = 'w', &
        title = 'W-type (unconstrained) value function',&
        mplot3d = .true., &
        tight_layout = .true. &
        )
    call plt%plot_wireframe(&
        bkgrid, &
        kgrid, &
        sol%w(:, :, inde), &
        label = "w", &
        antialiased = .true., &
        linestyle = '-' &
        )
    if (savefig) then
        call plt%savefig(&
            figDir // "w_3D.pdf", &
            pyfile = figPyDir // "w_3D.py" &
            )
    else
        call plt%showfig()
    endif

    inde = 4
    call plt%initialize(&
        usetex = .true., &
        xlabel = '$\\frac{b}{k}$', &
        ylabel = '$k$', &
        zlabel = 'v', &
        title = 'V-type (constrained) value function',&
        mplot3d = .true., &
        tight_layout = .true. &
        )
    call plt%plot_wireframe(&
        bkgrid, &
        kgrid, &
        sol%v(:, :, inde), &
        label = "v", &
        antialiased = .true., &
        linestyle = '-' &
        )
    if (savefig) then
        call plt%savefig(&
            figDir // "v_3D.pdf", &
            pyfile = figPyDir // "v_3D.py" &
            )
    else
        call plt%showfig()
    endif

    ! --------------------------------------- !
    ! muw distribution for unconstrained firm !
    ! --------------------------------------- !

    call plt%initialize(&
        usetex = .true., &
        xlabel = '$k$', &
        ylabel = '$\\epsilon$', &
        zlabel = 'muw', &
        title = 'W-type (unconstrained) firm distribution',&
        mplot3d = .true., &
        tight_layout = .true. &
        )
    call plt%plot_wireframe(&
        mukgrid, &
        egrid, &
        sol%muw, &
        label = "muw", &
        antialiased = .true., &
        linestyle = '-' &
        )
    if (savefig) then
        call plt%savefig(&
            figDir // "muw.pdf", &
            pyfile = figPyDir // "muw.py" &
            )
    else
        call plt%showfig()
    endif


    ! call plt%initialize(&
    !     usetex = .true., &
    !     xlabel = '$k$', &
    !     ylabel = '$\\epsilon$', &
    !     title = 'W-type (unconstrained) firm distribution',&
    !     tight_layout = .true. &
    !     )
    ! call plt%add_contour(&
    !     mukgrid, &
    !     egrid, &
    !     sol%muw, &
    !     ! levels = 10, &
    !     filled = .true., &
    !     cmap = 'jet', &
    !     colorbar = .true., &
    !     linestyle = '.' &
    !     )
    ! call plt%showfig()

    ! stop


    ! --------------------------------------- !
    ! w value function for unconstrained firm !
    ! --------------------------------------- !

    ! figname = 'w_bk'
    !     call plt%initialize(&
    !         usetex = .true., &
    !         xlabel = '$k$', &
    !         ylabel = '$\\frac{b}{k}$', &
    !         title = 'value function for unconstrained firm, $\\frac{b}{k} = $', &
    !         legend = .true., &
    !         tight_layout = .true. &
    !         )
    !     call plt%add_contour(bkgrid, kgrid, sol%w(:, :, 4), &
    !         linestyle = '-', filled = .true., colorbar = .true.)
    !     call plt%showfig()
    ! stop


    figname = 'w_bk'
    do indbk = 1, bknum, bknum/2 - 1
        call plt%initialize(&
            usetex = .true., &
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
        usetex = .true., &
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
        usetex = .true., &
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
        usetex = .true., &
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
            usetex = .true., &
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
            usetex = .true., &
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
            usetex = .true., &
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
