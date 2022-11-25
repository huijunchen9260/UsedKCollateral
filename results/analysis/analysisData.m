clear; clc;
% load results.mat;
matlabDir = pwd
dir = "../20221124SSBisectCalibration/"
figdir = strcat( dir, "figures/" )
isSaveFig = true
if not(exist(figdir, 'dir') == 7)
    mkdir(figdir)
end


run("readData.m")

cd(figdir)

sum(sum(muw))
sum(sum(sum(muv)))

KY = kagg/yagg
IK = invnewagg/kagg
qQ = qsell / Qbuy




mu = zeros( mubknum, muknum, enum );
invgrid = mu;
kfgrid = mu;
for inde = 1:1:enum
for indk = 1:1:muknum
    mu( bkidx0, indk, inde ) = muw( indk, inde );
    invgrid( bkidx0, indk, inde ) = invgridw( indk, inde );
    kfgrid( bkidx0, indk, inde ) = kfgridw( indk, inde );
end
end
mu = mu + muv;
invgrid = invgrid + invgridv;
kfgrid = kfgrid + kfgridv;
IKmean = sum(sum( (invgrid ./ kfgrid) .* mu, 'all' ))
IKstd = ( sum( mu .* ( invgrid - IKmean ).^2, 'all' ) )^.5

IKwmean = sum((invgridw ./ kfgridw) .* muw, 'all')
IKvmean = sum((invgridv ./ kfgridv) .* muv, 'all')

IKmean = IKwmean + IKvmean

for inde = 1:1:enum
    for indk = 1:1:muknum
        invgridw( indk, inde ) / kfgridw( indk,  )<++>
    end
end

( sum( (((invgridw ./ kfgridw) .* muw) - IKmean*ones(muknum, enum)).^2, 'all' ) / (muknum*enum))^.5 ...
+ ...
( sum( (((invgridv ./ kfgridv) .* muv) - IKmean*ones(mubknum, muknum, enum)).^2, 'all' ) / (mubknum*muknum*enum) )^.5



(std(std((invgridv ./ kfgridv) .* muv)))

figure
[ bkmat, kmat ] = meshgrid( bkgrid, kgrid );
inde = 4
mesh( bkmat, kmat, w(:, :, inde)' )

inde = enum / 2
figure
[ bkmat, kmat ] = meshgrid( bkgrid, kgrid );
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
[ mubkmat, mukmat ] = meshgrid( mubkgrid, mukgrid );
mesh( bkmat, kmat, w(:, :, inde)' )
hold on
xlabel( ' leverage ', 'Fontsize', 20 )
ylabel( ' capital ','Fontsize', 20  )
zlabel( ' mass ', 'Fontsize', 20 )
title ( ' value function for median productivity: unconstrained firm ','Fontsize', 20  )

[ kmat, emat ] = meshgrid( kgrid, egrid );
figure
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
inde = enum / 2
mesh( kmat, emat, gwk' )
hold on
xlabel( ' capital ', 'Fontsize', 20 )
ylabel( ' idio. productivity ','Fontsize', 20  )
title ( ' capital decision rule: unconstrained firm ','Fontsize', 20  )

[ kmat, emat ] = meshgrid( kgrid, egrid );
figure
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
inde = enum / 2
mesh( kmat, emat, gwb' )
hold on
xlabel( ' capital ', 'Fontsize', 20 )
ylabel( ' idio. productivity ','Fontsize', 20  )
zlabel( 'gwb' )
title ( ' minimum saving policy: unconstrained firm ','Fontsize', 20  )

[ kmat, emat ] = meshgrid( kgrid, egrid );
figure
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
inde = enum / 2
mesh( kmat, emat, btilde' )
hold on
xlabel( ' capital ', 'Fontsize', 20 )
ylabel( ' idio. productivity ','Fontsize', 20  )
zlabel( ' btilde ' )
title ( ' minimum saving policy for current bond level: unconstrained firm ','Fontsize', 20  )

inde = enum / 2
figure
[ bkmat, kmat ] = meshgrid( bkgrid, kgrid );
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
mesh( bkmat, kmat, v(:, :, inde)' )
hold on
xlabel( ' leverage ', 'Fontsize', 20 )
ylabel( ' capital ','Fontsize', 20  )
zlabel( ' mass ', 'Fontsize', 20 )
title ( ' value function for median productivity: constrained firm ','Fontsize', 20  )

inde = enum / 2
figure
[ bkmat, kmat ] = meshgrid( bkgrid, kgrid );
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
mesh( bkmat, kmat, gvk(:, :, inde)' )
hold on
xlabel( ' leverage ', 'Fontsize', 20 )
ylabel( ' capital ','Fontsize', 20  )
zlabel( ' future capital ', 'Fontsize', 20 )
title ( ' capital decision rule for median productivity: constrained firm ','Fontsize', 20  )

inde = enum / 2
figure
[ bkmat, kmat ] = meshgrid( bkgrid, kgrid );
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
mesh( bkmat, kmat, gvb(:, :, inde)' )
hold on
xlabel( ' bond ', 'Fontsize', 20 )
ylabel( ' capital ','Fontsize', 20  )
zlabel( " future bond ", 'Fontsize', 20 )
title ( ' bond decision rule for median productivity: constrained firm ','Fontsize', 20  )

inde = enum / 2
figure
[ bkmat, kmat ] = meshgrid( bkgrid, kgrid );
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
mesh( bkmat, kmat, gvbk(:, :, inde)' )
hold on
xlabel( ' leverage ', 'Fontsize', 20 )
ylabel( ' capital ','Fontsize', 20  )
zlabel( " b'/k ", 'Fontsize', 20 )
title ( ' leverage decision rule for median productivity: constrained firm ','Fontsize', 20  )

inde = enum / 2
muvplot = muv;
muvplot( muvplot == 0.0 ) = nan;
figure
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
[ mubkmat, mukmat ] = meshgrid( mubkgrid, mukgrid );
inde = enum / 2
mesh( mubkmat, mukmat, muvplot(:, :, inde)' )
hold on
xlabel( ' leverage ', 'Fontsize', 20 )
ylabel( ' capital ','Fontsize', 20  )
zlabel( ' mass ', 'Fontsize', 20 )
title ( ' steady state distribution: constrained firm (only positive mass) ','Fontsize', 20  )
% view(-80,20)

% muwplot = muw
% muwplot( muwplot == 0.0 ) = nan;
[ mukmat, emat ] = meshgrid( mukgrid, egrid );
figure
set(gca,'FontSize',30);
set(get(gca,'Xlabel'),'FontSize',20)
set(get(gca,'Ylabel'),'FontSize',20)
inde = enum / 2
mesh( mukmat, emat, muw' )
hold on
xlabel( ' capital ', 'Fontsize', 20 )
ylabel( ' idio. productivity ','Fontsize', 20  )
zlabel( ' mass ' )
title ( ' steady state distribution: unconstrained firm ','Fontsize', 20  )
view(-80,20)

cd (matlabDir)
