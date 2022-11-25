
params = dlmread(strcat(dir, 'parameters.tab')); %#ok<DLMRD>

ix = 1;

zeta = params(ix); ix = ix + 1;
beta = params(ix); ix = ix + 1;
delta = params(ix); ix = ix + 1;
psi = params(ix); ix = ix + 1;
alpha = params(ix); ix = ix + 1;
nu = params(ix); ix = ix + 1;
rho_z = params(ix); ix = ix + 1;
sigma_z = params(ix); ix = ix + 1;
gamma = params(ix); ix = ix + 1;
eta = params(ix); ix = ix + 1;
s = params(ix); ix = ix + 1;
rho_e = params(ix); ix = ix + 1;
sigma_e = params(ix); ix = ix + 1;
exitprob = params(ix); ix = ix + 1;
knum = params(ix); ix = ix + 1;
bknum = params(ix); ix = ix + 1;
enum = params(ix); ix = ix + 1;
muknum = params(ix); ix = ix + 1;
mubknum = params(ix); ix = ix + 1;

clear params ix;


prices = dlmread(strcat(dir, 'prices.tab')); %#ok<DLMRD>

ix = 1;

wval = prices(ix); ix = ix + 1;
qsell = prices(ix); ix = ix + 1;
Qbuy = prices(ix); ix = ix + 1;
pval = prices(ix); ix = ix + 1;

clear prices ix;

solutions = dlmread(strcat(dir, 'solutions.tab')); %#ok<DLMRD>

ix = 1;

scrapk = solutions(ix); ix = ix + 1;
bornK = solutions(ix); ix = ix + 1;
kagg = solutions(ix); ix = ix + 1;
kfagg = solutions(ix); ix = ix + 1;
nagg = solutions(ix); ix = ix + 1;
invagg = solutions(ix); ix = ix + 1;
divagg = solutions(ix); ix = ix + 1;
invusedagg = solutions(ix); ix = ix + 1;
invnewagg = solutions(ix); ix = ix + 1;
yagg = solutions(ix); ix = ix + 1;
cagg = solutions(ix); ix = ix + 1;
bvfagg = solutions(ix); ix = ix + 1;

clear solutions ix;

w = loadBinary(strcat(dir, 'sol%w.bin'), 'float64', [bknum, knum, enum]);
gwk = loadBinary(strcat(dir, 'sol%gwk.bin'), 'float64', [knum, enum]);
btilde = loadBinary(strcat(dir, 'sol%btilde.bin'), 'float64', [knum, enum]);
gwb = loadBinary(strcat(dir, 'sol%gwb.bin'), 'float64', [knum, enum]);
wtrue = loadBinary(strcat(dir, 'sol%wtrue.bin'), 'uint8', [bknum, knum, enum]);
v = loadBinary(strcat(dir, 'sol%v.bin'), 'float64', [bknum, knum, enum]);
gvk = loadBinary(strcat(dir, 'sol%gvk.bin'), 'float64', [bknum, knum, enum]);
gvb = loadBinary(strcat(dir, 'sol%gvb.bin'), 'float64', [bknum, knum, enum]);
gvbk = loadBinary(strcat(dir, 'sol%gvbk.bin'), 'float64', [bknum, knum, enum]);
muw = loadBinary(strcat(dir, 'sol%muw.bin'), 'float64', [muknum, enum]);
muv = loadBinary(strcat(dir, 'sol%muv.bin'), 'float64', [mubknum, muknum, enum]);

ygridw = loadBinary(strcat(dir, 'sol%ygridw.bin'), 'float64', [muknum, enum]);
ngridw = loadBinary(strcat(dir, 'sol%ngridw.bin'), 'float64', [muknum, enum]);
kfgridw = loadBinary(strcat(dir, 'sol%kfgridw.bin'), 'float64', [muknum, enum]);
invgridw = loadBinary(strcat(dir, 'sol%invgridw.bin'), 'float64', [muknum, enum]);
invnewgridw = loadBinary(strcat(dir, 'sol%invnewgridw.bin'), 'float64', [muknum, enum]);
invusedgridw = loadBinary(strcat(dir, 'sol%invusedgridw.bin'), 'float64', [muknum, enum]);
divgridw = loadBinary(strcat(dir, 'sol%divgridw.bin'), 'float64', [muknum, enum]);

ygridv = loadBinary(strcat(dir, 'sol%ygridv.bin'), 'float64', [mubknum, muknum, enum]);
ngridv = loadBinary(strcat(dir, 'sol%ngridv.bin'), 'float64', [mubknum, muknum, enum]);
kfgridv = loadBinary(strcat(dir, 'sol%kfgridv.bin'), 'float64', [mubknum, muknum, enum]);
bkfgridv = loadBinary(strcat(dir, 'sol%bkfgridv.bin'), 'float64', [mubknum, muknum, enum]);
invgridv = loadBinary(strcat(dir, 'sol%invgridv.bin'), 'float64', [mubknum, muknum, enum]);
invnewgridv = loadBinary(strcat(dir, 'sol%invnewgridv.bin'), 'float64', [mubknum, muknum, enum]);
invusedgridv = loadBinary(strcat(dir, 'sol%invusedgridv.bin'), 'float64', [mubknum, muknum, enum]);
divgridv = loadBinary(strcat(dir, 'sol%divgridv.bin'), 'float64', [mubknum, muknum, enum]);


kbounds = [ 0.05, 6.0 ];
bkbounds = [ -25.0, zeta ];
pbounds = [ 0.9, 1.2 ];

kgrid = loadBinary(strcat(dir, 'kgrid.bin'), 'float64', [knum, 1]);
bkgrid = loadBinary(strcat(dir, 'bkgrid.bin'), 'float64', [bknum, 1]);
mukgrid = loadBinary(strcat(dir, 'mukgrid.bin'), 'float64', [muknum, 1]);
mubkgrid = loadBinary(strcat(dir, 'mubkgrid.bin'), 'float64', [mubknum, 1]);
egrid = loadBinary(strcat(dir, 'egrid.bin'), 'float64', [enum, 1]);

[bkidx0, bkw0] = linear_interpolation( mubkgrid, mubknum, 0.0 )

% save results.mat;
