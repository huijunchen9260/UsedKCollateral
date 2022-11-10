let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +1 app/main.f90
badd +1 src/UsedKCollateral.f90
badd +1 src/numerics.f90
badd +1 test/plot.f90
badd +1 src/innerloop.f90
badd +1 src/parameters.f90
badd +28 term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//67366:/bin/zsh
badd +6 build/dependencies/ogpf/src/ogpf.f90
badd +1 src/optMod.f90
badd +1 src/firmValueIter.f90
badd +1 src/firmDistribution.f90
badd +65 src/distMod.f90
badd +1 src/goldenSectionSearch.f90
badd +83 src/io.f90
badd +3 economybisection.f90
badd +1647 term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//548520:/bin/zsh
badd +1 src/economybisection.f90
badd +1 term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//47974:/bin/zsh
argglobal
%argdel
$argadd app/main.f90
$argadd app/main.f90
set stal=2
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabnew +setlocal\ bufhidden=wipe
tabrewind
argglobal
if bufexists(fnamemodify("term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//548520:/bin/zsh", ":p")) | buffer term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//548520:/bin/zsh | else | edit term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//548520:/bin/zsh | endif
if &buftype ==# 'terminal'
  silent file term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//548520:/bin/zsh
endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 35 - ((34 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 35
normal! 0
tabnext
edit app/main.f90
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 18 - ((0 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 18
normal! 0
tabnext
edit src/UsedKCollateral.f90
argglobal
if bufexists(fnamemodify("src/UsedKCollateral.f90", ":p")) | buffer src/UsedKCollateral.f90 | else | edit src/UsedKCollateral.f90 | endif
if &buftype ==# 'terminal'
  silent file src/UsedKCollateral.f90
endif
balt app/main.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 34 - ((30 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 34
normal! 0
tabnext
edit src/economybisection.f90
argglobal
if bufexists(fnamemodify("src/economybisection.f90", ":p")) | buffer src/economybisection.f90 | else | edit src/economybisection.f90 | endif
if &buftype ==# 'terminal'
  silent file src/economybisection.f90
endif
balt src/UsedKCollateral.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 43 - ((15 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 43
normal! 018|
tabnext
edit src/firmDistribution.f90
argglobal
if bufexists(fnamemodify("src/firmDistribution.f90", ":p")) | buffer src/firmDistribution.f90 | else | edit src/firmDistribution.f90 | endif
if &buftype ==# 'terminal'
  silent file src/firmDistribution.f90
endif
balt src/firmValueIter.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 344 - ((6 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 344
normal! 076|
tabnext
edit src/firmValueIter.f90
argglobal
if bufexists(fnamemodify("src/firmValueIter.f90", ":p")) | buffer src/firmValueIter.f90 | else | edit src/firmValueIter.f90 | endif
if &buftype ==# 'terminal'
  silent file src/firmValueIter.f90
endif
balt src/UsedKCollateral.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 171 - ((0 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 171
normal! 060|
tabnext
edit src/optMod.f90
argglobal
if bufexists(fnamemodify("src/optMod.f90", ":p")) | buffer src/optMod.f90 | else | edit src/optMod.f90 | endif
if &buftype ==# 'terminal'
  silent file src/optMod.f90
endif
balt src/innerloop.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 178 - ((4 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 178
normal! 0
tabnext
edit src/goldenSectionSearch.f90
argglobal
if bufexists(fnamemodify("src/goldenSectionSearch.f90", ":p")) | buffer src/goldenSectionSearch.f90 | else | edit src/goldenSectionSearch.f90 | endif
if &buftype ==# 'terminal'
  silent file src/goldenSectionSearch.f90
endif
balt src/optMod.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 47 - ((0 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 47
normal! 0
tabnext
edit src/parameters.f90
argglobal
if bufexists(fnamemodify("src/parameters.f90", ":p")) | buffer src/parameters.f90 | else | edit src/parameters.f90 | endif
if &buftype ==# 'terminal'
  silent file src/parameters.f90
endif
balt src/innerloop.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 48 - ((11 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 48
normal! 052|
tabnext
edit test/plot.f90
argglobal
if bufexists(fnamemodify("test/plot.f90", ":p")) | buffer test/plot.f90 | else | edit test/plot.f90 | endif
if &buftype ==# 'terminal'
  silent file test/plot.f90
endif
balt src/UsedKCollateral.f90
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 1 - ((0 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 08|
tabnext 10
set stal=1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
