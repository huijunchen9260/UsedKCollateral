program plot

    use UsedKCollateral
    implicit none

    type(gpf) :: gp
    type(solutions) :: sol
    type(configurations) :: conf

    logical, parameter :: savefig = .false.
    ! logical, parameter :: savefig = .true.

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
        call gp%options('set output "' // figDir // 'w.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('$k$')
    call gp%ylabel('$w$')
    call gp%title('value function for unconstrained firm')
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/(1.0_rk-delta)*kgrid, sol%w(bknum, :, 1), sol%w(bknum, :, 5), sol%w(bknum, :, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "$(1-\\delta)k$" dt 2 lc -1; &
                title "$e = ' // num2str(real(egrid(1), 4)) // '$" ls 1; &
                title "$e = ' // num2str(real(egrid(5), 4)) // '$" ls 2; &
                title "$e = ' // num2str(real(egrid(7), 4)) // '$" ls 3 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf

    if (savefig) then
        call gp%options('set terminal pdfcairo enhanced color dashed font "Alegreya, 14" rounded size 16 cm, 9.6 cm')
        call gp%options('set output "' // figDir // 'gwk.pdf"')
        ! call gp%options('set terminal epslatex size 16cm, 9.6cm color colortext standalone')
        ! call gp%options('set output "' // figDir // 'gwk.tex"')
    endif
    call gp%preset(.false.)
    call gp%xlabel('$k$')
    call gp%ylabel('$gwk$')
    call gp%title('capital decision rule for unconstrained firm')
    call gp%options('set key outside')
    call gp%plot( &
        kgrid, &
        reshape( (/(1.0_rk-delta)*kgrid, sol%gwk(:, 1), sol%gwk(:, 5), sol%gwk(:, 7)/), (/knum, 4/) ), &
        lspec =' &
                title "$(1-\\delta)k$" dt 2 lc -1; &
                title "$e = ' // num2str(real(egrid(1), 4)) // '$" ls 1; &
                title "$e = ' // num2str(real(egrid(5), 4)) // '$" ls 2; &
                title "$e = ' // num2str(real(egrid(7), 4)) // '$" ls 3 &
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
                title "e = ' // num2str(real(egrid(1), 4)) // '" ls 1; &
                title "e = ' // num2str(real(egrid(3), 4)) // '" ls 2; &
                title "e = ' // num2str(real(egrid(5), 4)) // '" ls 3; &
                title "e = ' // num2str(real(egrid(7), 4)) // '" ls 4 &
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
                title "e = ' // num2str(real(egrid(1), 4)) // '" ls 1; &
                title "e = ' // num2str(real(egrid(3), 4)) // '" ls 2; &
                title "e = ' // num2str(real(egrid(5), 4)) // '" ls 3; &
                title "e = ' // num2str(real(egrid(7), 4)) // '" ls 4 &
        ')
    call gp%reset() ! use reset to be able to save next figure to pdf


contains

end program plot
