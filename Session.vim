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
badd +14 src/numerics.f90
badd +1 test/plot.f90
badd +299 src/innerloop.f90
badd +1 src/parameters.f90
badd +28 term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//67366:/bin/zsh
badd +12 term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//48796:/bin/zsh
badd +6 build/dependencies/ogpf/src/ogpf.f90
badd +1 src/optMod.f90
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
tabrewind
argglobal
if bufexists(fnamemodify("term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//48796:/bin/zsh", ":p")) | buffer term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//48796:/bin/zsh | else | edit term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//48796:/bin/zsh | endif
if &buftype ==# 'terminal'
  silent file term://~/mnt/home/huijunchen/Documents/Google_Drive/Ohio_related/Research/Used_capital_Collateral/code/UsedKCollateral//48796:/bin/zsh
endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 667 - ((34 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 667
normal! 0133|
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
let s:l = 41 - ((25 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 41
normal! 029|
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
let s:l = 126 - ((34 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 126
normal! 0
tabnext
edit src/innerloop.f90
argglobal
if bufexists(fnamemodify("src/innerloop.f90", ":p")) | buffer src/innerloop.f90 | else | edit src/innerloop.f90 | endif
if &buftype ==# 'terminal'
  silent file src/innerloop.f90
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
let s:l = 318 - ((24 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 318
normal! 046|
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
let s:l = 259 - ((17 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 259
normal! 022|
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
let s:l = 158 - ((27 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 158
normal! 05|
tabnext
edit src/numerics.f90
argglobal
if bufexists(fnamemodify("src/numerics.f90", ":p")) | buffer src/numerics.f90 | else | edit src/numerics.f90 | endif
if &buftype ==# 'terminal'
  silent file src/numerics.f90
endif
balt src/parameters.f90
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
let s:l = 266 - ((20 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 266
normal! 022|
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
let s:l = 41 - ((0 * winheight(0) + 17) / 35)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 41
normal! 093|
tabnext 8
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
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :