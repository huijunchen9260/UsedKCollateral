program plot

    use UsedKCollateral
    implicit none

    type(gpf) :: gp
    type(solutions) :: sol
    type(configurations) :: conf

    logical, parameter :: savefig = .true.
    ! logical, parameter :: savefig = .false.
    character(len=*), parameter :: strfmt = 'F8.3'

    real(rk), allocatable :: xgrid(:, :), ygrid(:, :)

    if (.not. .d. resDir) then
        write(*, *) "no ", resDir, "; exit."
        stop
    endif
    if (.not. .d. figDir) then
        write(*, *) "no ", figDir, "; exit."
        stop
    endif

    call initGrids()
    call initSol(sol)
    call loadResult(sol)

    write(*, *) "Start Plotting!"

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'w_bkhigh.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('w')
    call gp%title('value function for unconstrained firm, b/k = ' // num2str(bkgrid(bknum), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%w(bknum, :, 1), sol%w(bknum, :, 3), sol%w(bknum, :, 5), sol%w(bknum, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'w_bkmid.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('w')
    call gp%title('value function for unconstrained firm, b/k = ' // num2str(bkgrid(bknum/2), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%w(bknum/2, :, 1), sol%w(bknum/2, :, 3), sol%w(bknum/2, :, 5), sol%w(bknum/2, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'w_bklow.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('w')
    call gp%title('value function for unconstrained firm, b/k = ' // num2str(bkgrid(1), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%w(1, :, 1), sol%w(1, :, 3), sol%w(1, :, 5), sol%w(1, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gwk.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gwk')
    call gp%title('capital decision rule for unconstrained firm')
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/(1.0_rk-delta)*kgrid, sol%gwk(:, 1), sol%gwk(:, 5), sol%gwk(:, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "(1-delta)k" dt 2 lc -1; &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 3 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'btilde.pdf"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('btilde')
    call gp%title('current bond stock rule for unconstrained firm')
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%btilde(:, 1), sol%btilde(:, 3), sol%btilde(:, 5), sol%btilde(:, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gwb.pdf"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gwb')
    call gp%title('future bond decision rule for unconstrained firm')
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%gwb(:, 1), sol%gwb(:, 3), sol%gwb(:, 5), sol%gwb(:, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'v_bkhigh.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('v')
    call gp%title('value function for constrained firm, b/k = ' // num2str(bkgrid(bknum), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%v(bknum, :, 1), sol%v(bknum, :, 3), sol%v(bknum, :, 5), sol%v(bknum, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'v_bkmid.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('v')
    call gp%title('value function for constrained firm, b/k = ' // num2str(bkgrid(bknum/2), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%v(bknum/2, :, 1), sol%v(bknum/2, :, 3), sol%v(bknum/2, :, 5), sol%v(bknum/2, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'v_bklow.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('v')
    call gp%title('value function for constrained firm, b/k = ' // num2str(bkgrid(1), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%v(1, :, 1), sol%v(1, :, 3), sol%v(1, :, 5), sol%v(1, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gvk_bkhigh.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gvk')
    call gp%title('capital decision rule for constrained firm, b/k = ' // num2str(bkgrid(bknum), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/(1.0_rk-delta)*kgrid, &
                    sol%gvk(bknum, :, 1), &
                    sol%gvk(bknum, :, 3), &
                    sol%gvk(bknum, :, 5), &
                    sol%gvk(bknum, :, 7)/), &
                    (/knum, 5/) ), &
        lspec =' &
                title "(1-delta)k" dt 2 lc -1; &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ' &
        )
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gvk_bkmiddle.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gvk')
    call gp%title('capital decision rule for constrained firm, b/k = ' // num2str(bkgrid(bknum/2), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/(1.0_rk-delta)*kgrid, &
                    sol%gvk(bknum/2, :, 1), &
                    sol%gvk(bknum/2, :, 3), &
                    sol%gvk(bknum/2, :, 5), &
                    sol%gvk(bknum/2, :, 7)/), &
                    (/knum, 5/) ), &
        lspec =' &
                title "(1-\delta)k" dt 2 lc -1; &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ' &
        )
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gvk_bklow.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gvk')
    call gp%title('capital decision rule for constrained firm, b/k = ' // num2str(bkgrid(1), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/(1.0_rk-delta)*kgrid, &
                    sol%gvk(1, :, 1), &
                    sol%gvk(1, :, 3), &
                    sol%gvk(1, :, 5), &
                    sol%gvk(1, :, 7)/), &
                    (/knum, 5/) ), &
        lspec =' &
                title "(1-delta)k" dt 2 lc -1; &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ' &
        )
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gvb_bkhigh.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gvb')
    call gp%title('bond decision rule for constrained firm, b/k = ' // num2str(bkgrid(bknum), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%gvb(bknum, :, 1), &
                   sol%gvb(bknum, :, 3), &
                   sol%gvb(bknum, :, 5), &
                   sol%gvb(bknum, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gvb_bkmid.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gvb')
    call gp%title('bond decision rule for constrained firm, b/k = ' // num2str(bkgrid(bknum/2), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%gvb(bknum/2, :, 1), &
                   sol%gvb(bknum/2, :, 3), &
                   sol%gvb(bknum/2, :, 5), &
                   sol%gvb(bknum/2, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gvb_bklow.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('k')
    call gp%ylabel('gvb')
    call gp%title('bond decision rule for constrained firm, b/k = ' // num2str(bkgrid(1), strfmt))
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/sol%gvb(1, :, 1), &
                   sol%gvb(1, :, 3), &
                   sol%gvb(1, :, 5), &
                   sol%gvb(1, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "e = ' // num2str(egrid(1), strfmt) // '" ls 1; &
                title "e = ' // num2str(egrid(3), strfmt) // '" ls 2; &
                title "e = ' // num2str(egrid(5), strfmt) // '" ls 3; &
                title "e = ' // num2str(egrid(7), strfmt) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    ! if (savefig) then
    !     call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
    !     call gp%options('set output "' // figDir // 'gvk_elow_3D.pdf"')
    ! endif
    ! call gp%preset(.false.)
    ! allocate(xgrid(bknum, knum), ygrid(bknum, knum))
    ! call meshgrid(xgrid, ygrid, bkgrid, kgrid)
    ! call gp%xlabel('k')
    ! call gp%ylabel('b')
    ! call gp%zlabel('gvk')
    ! call gp%title('bond decision rule for constrained firm, e = ' // num2str(egrid(1), strfmt))
    ! call gp%options('set key outside')
    ! call gp%options('set xyplane at ' // num2str(minval(sol%v(:, :, 3))))
    ! call gp%surf(xgrid, ygrid, transpose(sol%v(:, :, 3)), 'title "g" with lines lt 5 lc rgb "#0008B0"')
    ! call gp%reset() ! use reset to be able to save next figure to pdf


contains

end program plot
