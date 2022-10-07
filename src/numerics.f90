module numerics

    use, intrinsic :: iso_Fortran_env, only : &
        ik => int32, rk => real64, OUTPUT_UNIT, INPUT_UNIT, ERROR_UNIT
    implicit none
    private
    public :: rk, ik, OUTPUT_UNIT, INPUT_UNIT, ERROR_UNIT
    ! public :: linspace, logspace, tauchen, gridlookup, gridweight, linear_interpolation, cumsum, rouwenhorst, eye
    public :: logspace, tauchen, gridlookup, gridweight, linear_interpolation, cumsum, rouwenhorst, eye

contains

! generate identity matrix
pure function eye(num) result(mat)
    integer(ik), intent(in) :: num
    integer(ik) :: i
    real(rk), dimension(num, num) :: mat
    mat = 0.0_rk
    do i = 1, num, 1
        mat(i, i) = 1.0_rk
    enddo
end function eye

! Calculate the cumulative sum of a matrix
!     mat: the input matrix
!     rownum: total row number of mat
!     colnum: total column number of mat
!     indicator: either "row" or "col", doing sum over row or column
pure function cumsum(mat, rownum, colnum, indicator)
    integer(ik) :: i, j, rownum, colnum
    character(3) :: indicator
    real(rk), dimension(rownum, colnum) :: mat, cumsum
    intent(in) :: mat, rownum, colnum, indicator

    if (indicator .eq. "col") then
        do j = 1, colnum
            cumsum(:, j) = [(sum(mat(1:i, j)), i = 1, rownum)]
        end do
    else if (indicator .eq. "row") then
        do i = 1, rownum
            cumsum(i, :) = [(sum(mat(i, 1:j)), j = 1, colnum)]
        end do
    end if
end function cumsum

! For linear interpolation
! Find the left index of xgrid based on xval
pure function gridlookup(xgrid, gridnum, xval)
    integer(ik):: gridnum, ixhigh, ixlow, ixplace, gridlookup
    real(rk):: xgrid(gridnum), xval
    intent(in):: gridnum, xgrid, xval
    ixhigh = gridnum; ixlow = 1_ik

    do
        if ((ixhigh	- ixlow).le.1_ik) exit
        ixplace	= (ixhigh +	ixlow)/2_ik

        if (xgrid(ixplace).ge.xval)	then
            ixhigh = ixplace
        else
            ixlow =	ixplace
        end	if
    end	do
    gridlookup = ixlow
end function gridlookup

! For linear interpolation
! Find the weight associate with xindex on xgrid based on xval
pure function gridweight(xgrid, gridnum, xval, xindex)
    integer(ik) :: gridnum, xindex
    real(rk) :: xgrid(gridnum), xval, gridweight
    intent(in) :: xgrid, gridnum, xval, xindex
    if (xval .le. xgrid(1)) then
        gridweight = 1.0_rk
    else if (xval .ge. xgrid(gridnum)) then
        gridweight = 0.0_rk
    else
        gridweight = xgrid(xindex + 1_ik) - xval
        gridweight = gridweight / (xgrid(xindex + 1_ik) - xgrid(xindex))
    endif
end function gridweight

! Combination of gridlookup and gridweight
pure subroutine linear_interpolation(xgrid, gridnum, xval, xindex, xweight)
    integer(ik) :: gridnum, xindex, ixhigh, ixlow, ixplace
    real(rk) :: xgrid(gridnum), xval, xweight
    intent(in) :: xgrid, gridnum, xval
    intent(out) :: xindex, xweight

    if (xval .le. xgrid(1_ik)) then
        xindex = 1_ik
        xweight = 1_rk
    else if (xval .ge. xgrid(gridnum)) then
        xindex = gridnum - 1_ik
        xweight = 0_rk
    else

        ixhigh = gridnum; ixlow = 1_ik

        do
            if ((ixhigh	- ixlow).le.1_ik) exit

            ixplace	= (ixhigh +	ixlow)/2_ik

            if (xgrid(ixplace).ge.xval)	then
                ixhigh = ixplace
            else
                ixlow =	ixplace
            end	if
        end	do

        xindex = ixplace
        xweight = (xgrid(xindex+1) - xval) / (xgrid(xindex+1) - xgrid(xindex))
    end if

end subroutine linear_interpolation


! G. Tauchen (1986) 'Finite State Markov-Chain Approximations to
! Univariate and Vector Autoregressions' Economics Leters 20: 177-181

subroutine tauchen(meaninnov, stdinnov, persistence, multiple, znum, zvec, pi)

integer(ik):: znum, j, gridsize, k
real(rk)::meaninnov, stdinnov, persistence, multiple, zvec(znum), pi(znum, znum), &
		  f1, f0, stdz, zlow, zhigh, meanz, w, z, lowerbound

intent(in):: meaninnov, stdinnov, persistence, multiple, znum
intent(out):: zvec, pi


stdz = stdinnov**2.0_rk
stdz = stdz/(1.0_rk - persistence**2.0_rk)
stdz = dsqrt(stdz)
meanz = meaninnov/(1.0_rk - persistence)
zlow = meanz - stdz*multiple
zhigh = meanz + stdz*multiple

lowerbound = meaninnov - stdinnov*dmax1(10.0_rk, 2.0_rk*multiple)
gridsize = 10000

! call linspace(zlow, zhigh, znum, zvec)
zvec = linspace(zlow, zhigh, znum)

pi = 0.0_rk

w = (zhigh - zlow)/dble(znum-1_ik)

do j = 1, znum

	z = zvec(1) - persistence*zvec(j)
	f1 = normal(z + w/2.0_rk, meaninnov, stdinnov, gridsize, lowerbound)
	pi(j,1) = f1

	do k = 2, znum - 1
		z = zvec(k) - persistence*zvec(j)
		f1 = normal(z + w/2.0_rk, meaninnov, stdinnov, gridsize, lowerbound)
		f0 = normal(z - w/2.0_rk, meaninnov, stdinnov, gridsize, lowerbound)
		pi(j,k) = f1 - f0
	end do

	z = zvec(znum) - persistence*zvec(j)
	f0 = normal(z - w/2.0_rk, meaninnov, stdinnov, gridsize, lowerbound)
	pi(j,znum) = 1.0_rk - f0

end do

end subroutine tauchen

! From KT13 code
! rho is the persistence of the continuous process
! sigmas is the standard deviation of innovations (epsilon)
! n is the number of states in the discretisation
! z is the discretised support
! pi is the transition matrix for the Markov Chain

! Rouwenhorst (1995) 'Asset Pricing Implications of Equilibrium Business Cycle Models,'
! Cooley (ed), Frontiers of Business Cycle Research. Princeton, 294-330.
subroutine rouwenhorst(rho, sigmas, n, z, pi)
    integer(ik), intent(in):: n
    integer(ik) :: i
    real(rk):: p, q, zvar, epsilon, y(n-2_ik)
    real(rk), intent(in):: rho, sigmas
    real(rk), intent(out):: z(n), pi(n,n)
    real(rk), dimension(:,:), allocatable:: hlag, h

    ! retrieve p and q
    p = (rho + 1.0_rk)/2.0_rk
    q = p

    allocate(hlag(1,1))

    hlag(1,1) = 1.0_rk

    do i = 2,n

        allocate (h(i,i))

        h = 0.0_rk
        h(1:i-1,1:i-1) = p*hlag
        h(1:i-1,2:i) = h(1:i-1,2:i) + (1.0_rk-p)*hlag
        h(2:i,1:i-1) = h(2:i,1:i-1) + (1.0_rk-q)*hlag
        h(2:i,2:i) = h(2:i,2:i) + q*hlag

        h(2:i-1,:) = h(2:i-1,:)/2.0_rk

        deallocate(hlag)

        allocate(hlag(i,i))

        hlag = h

        deallocate(h)

    end do

    pi = hlag

    deallocate(hlag)

    zvar = (sigmas**2.0_rk)/(1.0_rk - rho**2.0_rk)
    epsilon = sqrt((n-1.0_rk)*zvar)

    do i = 1_ik, n - 2_ik
        y(i) = dble(i)
    end do

    y = ((2.0_rk*epsilon)/dble(n-1_ik))*y - epsilon

    z(1_ik) = -1.0_rk*epsilon
    z(2_ik:n-1_ik) = y
    z(n) = epsilon

end subroutine rouwenhorst

! subroutine linspace(lb, ub, gridnum, x)
!     ! integer, parameter:: rk = selected_real_kind(15,307), ik = selected_int_kind(9)
!     integer(ik) :: gridnum, j1
!     real(rk) :: lb, ub, x(gridnum), y(gridnum-2_ik)

!     intent(in) :: lb, ub, gridnum
!     intent(out) :: x

!     do j1 = 1_ik, gridnum - 2_ik, 1_ik
!         y(j1) = dble(j1)
!     end do

!     y = ((ub - lb) / (gridnum - 1_ik))*y + lb

!     x(1_ik) = lb
!     x(2_ik:gridnum-1_ik) = dble(y)
!     x(gridnum) = ub

! end subroutine linspace

pure function linspace(lb, ub, gridnum)
    integer(ik) :: gridnum, j1
    real(rk), dimension(gridnum) :: linspace
    real(rk), dimension(gridnum-2_ik) :: y
    real(rk) :: lb, ub
    intent(in) :: lb, ub, gridnum
    do j1 = 1_ik, gridnum - 2_ik, 1_ik
        y(j1) = dble(j1)
    end do

    y = ((ub - lb) / (gridnum - 1_ik))*y + lb

    linspace(1_ik) = lb
    linspace(2_ik:gridnum-1_ik) = dble(y)
    linspace(gridnum) = ub
end function linspace

pure function logspace(lb, ub, gridnum)
    integer(ik) :: gridnum, j1
    real(rk), dimension(gridnum) :: logspace
    real(rk), dimension(gridnum-2_ik) :: y
    real(rk) :: lb, ub, loglb, logub
    intent(in) :: lb, ub, gridnum

    ! log in base 10
    loglb = dlog(1.0_rk) / dlog(10.0_rk)
    logub = dlog(ub - lb + 1.0_rk) / dlog(10.0_rk)

    do j1 = 1_ik, gridnum - 2_ik, 1_ik
        y(j1) = dble(j1)
    end do
    y = ((logub - loglb) / (gridnum - 1_ik))*y + loglb

    logspace(1_ik) = loglb
    logspace(2:gridnum-1_ik) = y
    logspace(gridnum) = logub

    logspace = 10.0_rk ** logspace
    logspace = logspace + lb - 1.0_rk


end function logspace

function normal(upperbound, mean, sd, gridsize, lowerbound)


optional:: mean, sd, gridsize, lowerbound
integer(ik):: gridsize
real(rk):: lowerbound, upperbound, mean, sd, increment, normal
real(rk), allocatable:: x0dl(:), x1du(:), f0dl(:), f1du(:), f(:)

if (present(mean)) then
	continue
else
	mean = 0.0_rk
end if

if(present(sd)) then
	continue
else
	sd = 1.0_rk
end if

if(present(lowerbound)) then
	continue
else
	lowerbound = -10.0_rk*sd + mean
end if


if(present(gridsize)) then
	continue
else
	gridsize = 100000_ik
end if

increment = (upperbound - lowerbound)/dble(gridsize)

allocate(x0dl(gridsize), x1du(gridsize), f0dl(gridsize), f1du(gridsize), f(gridsize))

! call linspace(lowerbound, upperbound - increment, gridsize, x0dl)
! call linspace(lowerbound + increment, upperbound, gridsize, x1du)
x0dl = linspace(lowerbound, upperbound - increment, gridsize)
x1du = linspace(lowerbound + increment, upperbound, gridsize)

call normaldensity(gridsize, x0dl, mean, sd, f0dl)
call normaldensity(gridsize, x1du, mean, sd, f1du)

f = (f0dl + f1du)/2.0_rk

normal = sum(f*increment)

contains

subroutine normaldensity(numberofpoints, vectorofpoints, mean, sd, densities)

integer(ik):: numberofpoints
real(rk):: vectorofpoints(numberofpoints), densities(numberofpoints), mean, sd, &
		   variance, pi, coefficient, transcend(numberofpoints)

intent(in):: numberofpoints, vectorofpoints, mean, sd
intent(out):: densities

variance = sd**2.0_rk
pi = 2.0_rk*dasin(1.0_rk)
coefficient = variance*2.0_rk*pi
coefficient = 1.0_rk/dsqrt(coefficient)

transcend = (vectorofpoints - mean)**2.0_rk
transcend = transcend/variance
transcend = transcend/2.0_rk
transcend = -1.0_rk*transcend
densities = coefficient*dexp(transcend)

end subroutine normaldensity

end function normal



end module numerics

