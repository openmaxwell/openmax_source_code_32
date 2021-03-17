! Copyright 2014, Aytac Alparslan.
! This file is part of OpenMaXwell.
!
! OpenMaXwell is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! OpenMaXwell is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with OpenMaXwell.  If not, see <http://www.gnu.org/licenses/>.
MODULE AALYR
USE CHBND
Integer(4), parameter :: mxlyr=9 ! maximum number of layers
complex*16 aaepsr(0:mxlyr) ! relative permittivity values for the layers exp(j{omega}t) is used
complex*16 aamur(0:mxlyr) ! relative permeability values for the layers exp(j{omega}t) is used
complex*16 aakz(0:mxlyr) ! wave numbers along the layers for a given krho: needed for the integrants
complex*16 aakk(0:mxlyr) ! wave numbers of different layers when kgamma.eq.0 (when kgamma.ne.0 aakk=sqrt(aakkgamma**2-kgamma**2))
complex*16 aakkgamma(0:mxlyr) ! wave numbers of different layers when kgamma.ne.0
complex*16 k00 ! wave number in air for the given frequency
complex*16 aadint ! the magnitude of the half sin function during the integration
complex*16 aaxte(10) ! te mode surface wave poles in the complex krho plane
complex*16 aaxtm(10) ! tm mode surface wave poles in the complex krho plane
complex*16 aaxdum(10) ! dummy needed for iHEK=2
complex*16 kgamma ! the out of screen component of the wavevector of monopoles (for the eigenvalue analysis)
real*8 aadd(0:mxlyr) ! (INPUT)thickness values of the layers (between layer 1 and layer aanoflyr-1)
real*8 aah !(INPUT) elevation of the source from the layer down (aaslyr-1)
real*8 aaz !the y component of the field point for internal calculations
real*8 aax !the x component of the field point for internal calculations
real*8 aay !the z component of the field point for internal calculations
real*8 aaxi !the x component of the field point for internal calculations (imaginary part)
real*8 aayi !the z component of the field point for internal calculations (imaginary part)
real*8 aazi !the y component of the field point for internal calculations (imaginary part)
complex*16 aaxc ! the complex location of the source point (x- component)
complex*16 aayc ! the complex location of the source point (z- component)
complex*16 aazc ! the complex location of the source point (y- component)
real*8 aarho ! the rho component of the field point for internal calculations (x in 2D, rho in 3D)
real*8 aarhoi !  the rho component of the field point for internal calculations (x in 2D, rho in 3D) (imaginary part)
complex*16 aarhoc ! the complex location of the source point (x in 2D, rho in 3D) (rho- component)
real*8 aakmax ! maximum value of (real part) of the wave numbers of the different layers: needed for surface wave pole search
real*8 aakrhomax ! the value after which the integration continues as a integrate_and_sum method
Integer(4) aacpflag !complex path flag (needed for complex origin green functions)
real*8 aaepsrel ! relative error criteria for the integrator taken to 1d-6 as default
real*8 aafreq_norm ! frequency value for the calculation in GHz
Integer(4) aanoflyr ! (INPUT) number of layers
Integer(4) aaolyr ! observation layer: automatically calculted inside and used for internal calculations (internal)
Integer(4) aakey ! order of integration: taken 6 as the default value
Integer(4) aante ! number of te mode surface wave poles: needed for residue calcultions
Integer(4) aantm ! number of tm mode surface wave poles: needed for residue calcultions
Integer(4) aandum ! dummy needed for iHEK=2
Integer(4) aaslyr ! source layer number (internal)
Integer(4) source_lyr ! (INPUT) source layer number
Integer(4) swpflag ! internal constant to see if the swps are on the real axis or not
Integer(4) peccheck(2) ! (INPUT) if pecheck(1)=1(2) lowermost layer is PEC(PMC); if pecheck(2)=1(2) uppermost layer is PEC(PMC) 

save
contains

subroutine getTwodlyrAux(ys)
! auxiliary routine that evaluates auxiliary parameters from OpenMaXwell boundary data
implicit none
Integer(4) l
Real(8) ys ! y component of the source (monopole) in global coordinates
do l=1,aanoflyr-2 ! compute layer heights from boundary data: domain l is between boundary l and l+1
  aadd(l)=tBndEdg(tBnd(l+1)%iEdgeOffset+1)%y-tBndEdg(tBnd(l)%iEdgeOffset+1)%y
end do
source_lyr=0
do l=1,aanoflyr-1 ! find source layer and distance aah of source from its lower boundary
  if(ys.lt.tBndEdg(tBnd(l)%iEdgeOffset+1)%y) then ! source layer found
    source_lyr=l
    aah=ys-tBndEdg(tBnd(l-1)%iEdgeOffset+1)%y
    Exit
  end if
end do
if(source_lyr.lt.1) then ! source lyaer not found, must be top layer
  source_lyr=aanoflyr
  aah=ys-tBndEdg(tBnd(aanoflyr-1)%iEdgeOffset+1)%y
end if
end subroutine getTwodlyrAux


subroutine twodlyr(kEx,kPa,maxPar,nP,iHEK,ri,rimag,kmaxmult,A,obs_lyr,gamma_re,gamma_im)
implicit none
Integer(4) kEx,kPa,nP,mPa,obs_lyr
Integer(2) maxPar,iHEK
complex*16 aaA(10),A(10,nParN)
real*8 ri(3),r(3),d,w,kmaxmult,rimag(3),gamma_re,gamma_im
if((kEx.lt.0).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
r=ri
d=r3Vec_Length(r)
w=1.0d0/Dble(kw0*fcFld)
aadint=0.0
if (kmaxmult.lt.0.0) then
  aadint=floor(dlog10(dabs(kmaxmult)))
  aadint=10**aadint
  kmaxmult=dabs(kmaxmult)/aadint
endif


nP=0
mPa=kPa+maxPar
if(lInitSurfWaves) then ! lInitSurfWaves is a logical variable of OpenMaXwell, true at the beginning
  swpflag=0
  lInitSurfWaves=.false.
end if

if (cdabs(dcmplx(gamma_re,-gamma_im)).gt.1e-6) then ! If there is kgamma component, then continue to 
!!!!!!!!!!!!!!!!!!!!!
if ((kPa.lt.mPa).and.(iHEK.eq.0)) then !Ez mode
call twodlyrbase_kgamma(0,r,rimag,kmaxmult,aaA,obs_lyr,gamma_re,gamma_im)
kPa=kPa+1
nP=nP+1
A(3,kPa)=A(3,kPa)-conjg(aaA(3))
A(4,kPa)=A(4,kPa)-conjg(aaA(4))
A(5,kPa)=A(5,kPa)-conjg(aaA(5))
A(1,kPa)=A(1,kPa)-conjg(aaA(1))
A(2,kPa)=A(2,kPa)-conjg(aaA(2))
A(6,kPa)=A(6,kPa)-conjg(aaA(6))
A(9,kPa)=A(9,kPa)+aaA(9)
elseif ((kPa.lt.mPa).and.(iHEK.eq.1)) then ! Hz mode
kPa=kPa+1
nP=nP+1
call twodlyrbase_kgamma(1,r,rimag,kmaxmult,aaA,obs_lyr,gamma_re,gamma_im)
A(1,kPa)=A(1,kPa)+conjg(aaA(1))*mue0/eps0
A(2,kPa)=A(2,kPa)+conjg(aaA(2))*mue0/eps0
A(6,kPa)=A(6,kPa)-conjg(aaA(6))
A(3,kPa)=A(3,kPa)+conjg(aaA(3))*mue0/eps0
A(4,kPa)=A(4,kPa)-conjg(aaA(4))
A(5,kPa)=A(5,kPa)-conjg(aaA(5))
A(9,kPa)=A(9,kPa)+aaA(9)
elseif ((kPa.lt.mPa).and.(iHEK.eq.2)) then ! hybrid mode
kPa=kPa+1
nP=nP+1
call twodlyrbase_kgamma(0,r,rimag,kmaxmult,aaA,obs_lyr,gamma_re,gamma_im)
A(3,kPa)=A(3,kPa)-conjg(aaA(3))
A(4,kPa)=A(4,kPa)-conjg(aaA(4))
A(5,kPa)=A(5,kPa)-conjg(aaA(5))
A(1,kPa)=A(1,kPa)-conjg(aaA(1))
A(2,kPa)=A(2,kPa)-conjg(aaA(2))
A(6,kPa)=A(6,kPa)-conjg(aaA(6))
A(9,kPa)=A(9,kPa)+aaA(9)
kPa=kPa+1
nP=nP+1
aaxdum=aaxte !interchange te and tm-mode swps
aaxte=aaxtm
aaxtm=aaxdum
aandum=aante
aante=aantm
aantm=aandum
call twodlyrbase_kgamma(1,r,rimag,kmaxmult,aaA,obs_lyr,gamma_re,gamma_im)
A(1,kPa)=A(1,kPa)+conjg(aaA(1))*mue0/eps0
A(2,kPa)=A(2,kPa)+conjg(aaA(2))*mue0/eps0
A(6,kPa)=A(6,kPa)-conjg(aaA(6))
A(3,kPa)=A(3,kPa)+conjg(aaA(3))*mue0/eps0
A(4,kPa)=A(4,kPa)-conjg(aaA(4))
A(5,kPa)=A(5,kPa)-conjg(aaA(5))
A(9,kPa)=A(9,kPa)+aaA(9)
aaxdum=aaxte !interchange te and tm-mode swps
aaxte=aaxtm
aaxtm=aaxdum
aandum=aante
aante=aantm
aantm=aandum
endif
!!!!!!!!!!!!!!!!!!!!!
else
if ((kPa.lt.mPa).and.(iHEK.eq.0)) then !Ez mode
call twodlyrbase(0,r,rimag,kmaxmult,aaA,obs_lyr)
kPa=kPa+1
nP=nP+1
A(3,kPa)=A(3,kPa)-conjg(aaA(3))
A(4,kPa)=A(4,kPa)-conjg(aaA(4))
A(5,kPa)=A(5,kPa)-conjg(aaA(5))
A(1,kPa)=A(1,kPa)-conjg(aaA(1))
A(2,kPa)=A(2,kPa)-conjg(aaA(2))
A(6,kPa)=A(6,kPa)-conjg(aaA(6))
A(9,kPa)=A(9,kPa)+aaA(9)
elseif ((kPa.lt.mPa).and.(iHEK.eq.1)) then ! Hz mode
kPa=kPa+1
nP=nP+1
call twodlyrbase(1,r,rimag,kmaxmult,aaA,obs_lyr)
A(1,kPa)=A(1,kPa)+conjg(aaA(1))
A(2,kPa)=A(2,kPa)+conjg(aaA(2))
A(6,kPa)=A(6,kPa)+conjg(aaA(6))
A(3,kPa)=A(3,kPa)+conjg(aaA(3))
A(4,kPa)=A(4,kPa)+conjg(aaA(4))
A(5,kPa)=A(5,kPa)+conjg(aaA(5))
A(9,kPa)=A(9,kPa)+aaA(9)
elseif ((kPa.lt.mPa).and.(iHEK.eq.2)) then ! hybrid mode
kPa=kPa+1
nP=nP+1
call twodlyrbase(0,r,rimag,kmaxmult,aaA,obs_lyr)
A(3,kPa)=A(3,kPa)-conjg(aaA(3))
A(4,kPa)=A(4,kPa)-conjg(aaA(4))
A(5,kPa)=A(5,kPa)-conjg(aaA(5))
A(1,kPa)=A(1,kPa)-conjg(aaA(1))
A(2,kPa)=A(2,kPa)-conjg(aaA(2))
A(6,kPa)=A(6,kPa)-conjg(aaA(6))
A(9,kPa)=A(9,kPa)+aaA(9)
kPa=kPa+1
nP=nP+1
aaxdum=aaxte !interchange te and tm-mode swps
aaxte=aaxtm
aaxtm=aaxdum
aandum=aante
aante=aantm
aantm=aandum
call twodlyrbase(1,r,rimag,kmaxmult,aaA,obs_lyr)
A(1,kPa)=A(1,kPa)+conjg(aaA(1))
A(2,kPa)=A(2,kPa)+conjg(aaA(2))
A(6,kPa)=A(6,kPa)+conjg(aaA(6))
A(3,kPa)=A(3,kPa)+conjg(aaA(3))
A(4,kPa)=A(4,kPa)+conjg(aaA(4))
A(5,kPa)=A(5,kPa)+conjg(aaA(5))
A(9,kPa)=A(9,kPa)+aaA(9)
aaxdum=aaxte !interchange te and tm-mode swps
aaxte=aaxtm
aaxtm=aaxdum
aandum=aante
aante=aantm
aantm=aandum
endif

endif

endsubroutine twodlyr

subroutine threedlyr(kEx,kPa,maxPar,nP,iHEK,iHVK,ri,rimag,kmaxmult,A,obs_lyr)
implicit none
Integer(4) kEx,kPa, nP, mPa, obs_lyr
Integer(2) maxPar,iHEK,iHVK
complex*16 aaAh(10,3),A(10,nParN)
real*8 ri(3),r(3),d,w,kmaxmult,rimag(3)
if((kEx.lt.0).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
r=ri
d=r3Vec_Length(r)
w=1.0d0/Dble(kw0*fcFld)

aadint=0.0
if (kmaxmult.lt.0.0) then
  aadint=floor(dlog10(dabs(kmaxmult)))
  aadint=10**aadint
  kmaxmult=dabs(kmaxmult)/aadint
endif

! The results are generated by the coordinate system used in OpenMaXwell
nP=0
mPa=kPa+maxPar
if ((kPa.lt.mPa).and.(iHEK.eq.0)) then !electric dipole mode
kPa=kPa+1
nP=nP+1
call threedlyrbasehor(0,r,rimag,kmaxmult,aaAh,obs_lyr,iHVK)
A(1,kPa)=A(1,kPa)-conjg(aaAh(1,1)) ! Horizontal dipole 1
A(2,kPa)=A(2,kPa)-conjg(aaAh(3,1))
A(3,kPa)=A(3,kPa)+conjg(aaAh(2,1))
A(4,kPa)=A(4,kPa)-conjg(aaAh(4,1))
A(5,kPa)=A(5,kPa)-conjg(aaAh(6,1))
A(6,kPa)=A(6,kPa)+conjg(aaAh(5,1))
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)-conjg(aaAh(1,3)) ! Vertical dipole
A(2,kPa)=A(2,kPa)-conjg(aaAh(3,3))
A(3,kPa)=A(3,kPa)+conjg(aaAh(2,3))
A(4,kPa)=A(4,kPa)-conjg(aaAh(4,3))
A(5,kPa)=A(5,kPa)-conjg(aaAh(6,3))
A(6,kPa)=A(6,kPa)+conjg(aaAh(5,3))
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)+conjg(aaAh(1,2)) ! Horizontal dipole 2
A(2,kPa)=A(2,kPa)+conjg(aaAh(3,2))
A(3,kPa)=A(3,kPa)-conjg(aaAh(2,2))
A(4,kPa)=A(4,kPa)+conjg(aaAh(4,2))
A(5,kPa)=A(5,kPa)+conjg(aaAh(6,2))
A(6,kPa)=A(6,kPa)-conjg(aaAh(5,2))

elseif ((kPa.lt.mPa).and.(iHEK.eq.1)) then !magnetic dipole mode
kPa=kPa+1
nP=nP+1
call threedlyrbasehor(1,r,rimag,kmaxmult,aaAh,obs_lyr,iHVK)
A(1,kPa)=A(1,kPa)+conjg(aaAh(4,1)) !Horizontal dipole 1
A(2,kPa)=A(2,kPa)+conjg(aaAh(6,1))
A(3,kPa)=A(3,kPa)-conjg(aaAh(5,1))
A(4,kPa)=A(4,kPa)-conjg(aaAh(1,1)*eps0/mue0)
A(5,kPa)=A(5,kPa)-conjg(aaAh(3,1)*eps0/mue0)
A(6,kPa)=A(6,kPa)+conjg(aaAh(2,1)*eps0/mue0)
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)+conjg(aaAh(4,3)) !Vertical dipole
A(2,kPa)=A(2,kPa)+conjg(aaAh(6,3))
A(3,kPa)=A(3,kPa)-conjg(aaAh(5,3))
A(4,kPa)=A(4,kPa)-conjg(aaAh(1,3)*eps0/mue0)
A(5,kPa)=A(5,kPa)-conjg(aaAh(3,3)*eps0/mue0)
A(6,kPa)=A(6,kPa)+conjg(aaAh(2,3)*eps0/mue0)
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)-conjg(aaAh(4,2)) !Horizontal dipole 2
A(2,kPa)=A(2,kPa)-conjg(aaAh(6,2))
A(3,kPa)=A(3,kPa)+conjg(aaAh(5,2))
A(4,kPa)=A(4,kPa)+conjg(aaAh(1,2)*eps0/mue0)
A(5,kPa)=A(5,kPa)+conjg(aaAh(3,2)*eps0/mue0)
A(6,kPa)=A(6,kPa)-conjg(aaAh(2,2)*eps0/mue0)

elseif ((kPa.lt.mPa).and.(iHEK.eq.2)) then !hybrid dipole mode
kPa=kPa+1
nP=nP+1
call threedlyrbasehor(0,r,rimag,kmaxmult,aaAh,obs_lyr,iHVK)
A(1,kPa)=A(1,kPa)-conjg(aaAh(1,1)) ! Horizontal dipole 1
A(2,kPa)=A(2,kPa)-conjg(aaAh(3,1))
A(3,kPa)=A(3,kPa)+conjg(aaAh(2,1))
A(4,kPa)=A(4,kPa)-conjg(aaAh(4,1))
A(5,kPa)=A(5,kPa)-conjg(aaAh(6,1))
A(6,kPa)=A(6,kPa)+conjg(aaAh(5,1))
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)-conjg(aaAh(1,3)) ! Vertical dipole
A(2,kPa)=A(2,kPa)-conjg(aaAh(3,3))
A(3,kPa)=A(3,kPa)+conjg(aaAh(2,3))
A(4,kPa)=A(4,kPa)-conjg(aaAh(4,3))
A(5,kPa)=A(5,kPa)-conjg(aaAh(6,3))
A(6,kPa)=A(6,kPa)+conjg(aaAh(5,3))
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)+conjg(aaAh(1,2)) ! Horizontal dipole 2
A(2,kPa)=A(2,kPa)+conjg(aaAh(3,2))
A(3,kPa)=A(3,kPa)-conjg(aaAh(2,2))
A(4,kPa)=A(4,kPa)+conjg(aaAh(4,2))
A(5,kPa)=A(5,kPa)+conjg(aaAh(6,2))
A(6,kPa)=A(6,kPa)-conjg(aaAh(5,2))
kPa=kPa+1
nP=nP+1
call threedlyrbasehor(1,r,rimag,kmaxmult,aaAh,obs_lyr,iHVK)
A(1,kPa)=A(1,kPa)+conjg(aaAh(4,1)) !Horizontal dipole 1
A(2,kPa)=A(2,kPa)+conjg(aaAh(6,1))
A(3,kPa)=A(3,kPa)-conjg(aaAh(5,1))
A(4,kPa)=A(4,kPa)-conjg(aaAh(1,1)*eps0/mue0)
A(5,kPa)=A(5,kPa)-conjg(aaAh(3,1)*eps0/mue0)
A(6,kPa)=A(6,kPa)+conjg(aaAh(2,1)*eps0/mue0)
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)+conjg(aaAh(4,3)) !Vertical dipole
A(2,kPa)=A(2,kPa)+conjg(aaAh(6,3))
A(3,kPa)=A(3,kPa)-conjg(aaAh(5,3))
A(4,kPa)=A(4,kPa)-conjg(aaAh(1,3)*eps0/mue0)
A(5,kPa)=A(5,kPa)-conjg(aaAh(3,3)*eps0/mue0)
A(6,kPa)=A(6,kPa)+conjg(aaAh(2,3)*eps0/mue0)
kPa=kPa+1
nP=nP+1
A(1,kPa)=A(1,kPa)-conjg(aaAh(4,2)) !Horizontal dipole 2
A(2,kPa)=A(2,kPa)-conjg(aaAh(6,2))
A(3,kPa)=A(3,kPa)+conjg(aaAh(5,2))
A(4,kPa)=A(4,kPa)+conjg(aaAh(1,2)*eps0/mue0)
A(5,kPa)=A(5,kPa)+conjg(aaAh(3,2)*eps0/mue0)
A(6,kPa)=A(6,kPa)-conjg(aaAh(2,2)*eps0/mue0)
endif
endsubroutine threedlyr

subroutine threedlyrbasehor(aaiHEK,r,rimag,kmaxmult,aaA,obs_lyr,iHVK)
implicit none
complex*16 freq,caafreq_norm
Integer(4) reflag,aaiHEK,i
Integer(2) iHVK
real*8 xdata,ydata,kmaxmult,zdata
complex*16 dumm(0:mxlyr)
complex*16 aarr
real*8 rimag(3),r(3)
Integer(4) halfcyc
complex*16 aafcFld
Integer(4) obs_lyr
Integer(4) path_id1, path_id2, path_id3, path_id4
complex*16 aaA(10,3)
complex*16 integrator_res(15)
aaA=(0.0d0,0.0d0)
aafcFld=dreal(fcFld)-Ci*dabs((dimag(fcFld)))
aafreq_norm=dreal(aafcFld)/1e9
caafreq_norm=aafcFld/1e9
freq=aafcFld/aafreq_norm ! frequency to GHz
k00=2*pi*freq*dsqrt(eps0*mue0) ! wave number of freespace
aakmax=dreal(k00)**2
call geomnormer(r,obs_lyr)
rimag=rimag*aafreq_norm !normalize the imaginary origin values

xdata=r(1) ! x-information (field point)
zdata=r(2) ! y-information (field point)
ydata=-r(3) ! z-information (field point)
aaxi=rimag(1) ! x-information (field point)
aazi=rimag(2) ! y-information (field point)
aayi=-rimag(3) ! z-information (field point)
if (obs_lyr.lt.1) then
  aaolyr=obslayer(zdata) ! observation layer calculation (comment if the obslayer will be given outsite)
endif
aakey=3 ! order of integration (default is 6)

if (aaiHEK.eq.1) then ! check if the polarization is TM if so interchange parameters
  dumm=aaepsr
  aaepsr=aamur
  aamur=dumm
  if (peccheck(1).eq.1) then
  peccheck(1)=2
  elseif (peccheck(1).eq.2) then
  peccheck(1)=1
  endif
  if (peccheck(2).eq.2) then
  peccheck(2)=1
  elseif (peccheck(2).eq.1) then
  peccheck(2)=2
  endif
endif

reflag=0 !assume all the swps are purely real
!CALCULATE THE k VECTORS IN ALL OF THE LAYERS

do i=0,aanoflyr-1 !wavenumbers of each layer
  if (i.eq.0) then
    if (peccheck(1).eq.0) then
    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    else
    aakk(i)=0.0
    endif
  elseif (i.eq.aanoflyr-1) then
    if (peccheck(2).eq.0) then
    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    else
    aakk(i)=0.0
    endif
  else
    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
  endif
  if (dimag(aakk(i)).lt.0.0) then
    reflag=1  !the swps have imaginary part
  endif
  if ((dreal(aakk(i))**2).gt.aakmax) then
    aakmax=dreal(aakk(i))**2 ! Find the maximum k vector
  endif
enddo
aakmax=sqrt(aakmax)*kmaxmult

if (swpflag.eq.0) then ! if not run before, run it once
  call swpoles(aaxte,aaxtm) ! determine the locations of surface wave poles
  swpflag=1
endif

!! START THE MAIN CODE !!

aaz=zdata
aax=xdata
aay=ydata

if (dabs(aax).lt.1d-10) then
  aax=(1d-10)*sign(1.0d0,aax)
endif
if (dabs(aay).lt.1d-10) then
  aay=(1d-10)*sign(1.0d0,aay)
endif

aarhoc=cdsqrt((aax+Ci*aaxi)**2+(aay+Ci*aayi)**2)
if (dreal(aarhoc).lt.0.0) aarhoc=-aarhoc ! must have a positive real part because of the setting in the code
aarho=dreal(aarhoc)
aarhoi=dimag(aarhoc)

aarr=cdsqrt((aax+Ci*aaxi)**2+(aay+Ci*aayi)**2+(aaz+Ci*aazi)**2)
if (dreal(aarr).lt.0.0) aarr=-aarr ! must have a positive real part because of the setting in the code

aaxc=aax+Ci*aaxi
aayc=aay+Ci*aayi
aazc=aaz+Ci*aazi
if (dreal(aadint).gt.0.0) then
  aadint=aadint*aakmax
else
  aadint=aakmax*1d-3
endif

! integration of all the integrants

if ((dabs(aarhoi) .gt. 1d-8).or.(dabs(aazi).gt.1d-8)) then  ! if there is some complex origin that effects the results
call path_chooser3d(path_id1,path_id2)
call path_chooser3d_opp(path_id3,path_id4)
halfcyc=31
call integrator3dc(halfcyc,integrator_res,path_id1,path_id2,path_id3,path_id4,iHVK) ! integration of all the integrants simultaneously by Gauss Kronrod
else ! if no complex origin is seen continue with the standart integrator
halfcyc=21
call integrator3d(halfcyc,integrator_res,iHVK)! integration of all the integrants simultaneously by Gauss Kronrod
endif

if (iHVK.ne.1) then
aaA(1,1)=-Ci*2*pi*freq*(aafreq_norm)*(integrator_res(1)+(integrator_res(2)/((2*pi*freq)**2))) ! Hor Exx
aaA(2,1)=integrator_res(3)*(aafreq_norm)/(Ci*2*pi*freq) ! Hor Eyx
aaA(3,1)=-Ci*2*pi*freq*(aafreq_norm)*(integrator_res(5)+integrator_res(4)/((2*pi*freq)**2)) ! Hor Ezx
aaA(4,1)=integrator_res(6)*aafreq_norm/aamur(aaolyr) !Hor Hxx
aaA(5,1)=aafreq_norm*(integrator_res(7)-integrator_res(8))/aamur(aaolyr) !Hor Hyx
aaA(6,1)=-integrator_res(9)*aafreq_norm/aamur(aaolyr)

aaA(1,2)=aaA(2,1) !Hor Exy
aaA(2,2)=-Ci*2*pi*freq*(aafreq_norm)*(integrator_res(1)+(integrator_res(14)/((2*pi*freq)**2))) ! Hor Eyy
aaA(3,2)=aaA(3,1)*aayc/aaxc !Hor Ezy
aaA(4,2)=-aafreq_norm*(integrator_res(7)-integrator_res(15))/aamur(aaolyr)
!aaA(4,2)=-aaA(5,1) !Hor Hxy (- times Hyx)
aaA(5,2)=-aaA(4,1) !Hor Hyy (- times Hxx)
aaA(6,2)=-aaA(6,1)*aaxc/aayc !Hor Hzy

if (aaslyr.eq.aaolyr) then !direct term inclusion
  aaA(1,1)=aaA(1,1)+(aafreq_norm*(cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*(aakk(aaslyr)**2-Ci*aakk(aaslyr)/aarr-(1.0+(aaxc**2)*(aakk(aaslyr)**2))/(aarr**2)+(3*Ci*aakk(aaslyr)*aaxc**2)/(aarr**3)+(3*aaxc**2)/(aarr**4)))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr))
  aaA(2,1)=aaA(2,1)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aayc*aaxc*(-aakk(aaslyr)**2+Ci*3*aakk(aaslyr)/aarr+3/(aarr**2))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr)*(aarr**2))
  aaA(3,1)=aaA(3,1)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aaxc*aazc*(-aakk(aaslyr)**2+Ci*3*aakk(aaslyr)/aarr+3/(aarr**2))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr)*(aarr**2))
  aaA(4,1)=aaA(4,1)+0.0
  aaA(5,1)=aaA(5,1)-(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aazc*(Ci*aakk(aaslyr)/aarr+1/(aarr**2))/(4*pi)
  aaA(6,1)=aaA(6,1)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aayc*(Ci*aakk(aaslyr)/aarr+1/(aarr**2))/(4*pi)
  aaA(1,2)=aaA(1,2)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aayc*aaxc*(-aakk(aaslyr)**2+Ci*3*aakk(aaslyr)/aarr+3/(aarr**2))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr)*(aarr**2)) 
  aaA(2,2)=aaA(2,2)+(aafreq_norm*(cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*(aakk(aaslyr)**2-Ci*aakk(aaslyr)/aarr-(1.0+(aayc**2)*(aakk(aaslyr)**2))/(aarr**2)+(3*Ci*aakk(aaslyr)*aayc**2)/(aarr**3)+(3*aayc**2)/(aarr**4)))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr))
  aaA(3,2)=aaA(3,2)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aayc*aazc*(-aakk(aaslyr)**2+Ci*3*aakk(aaslyr)/aarr+3/(aarr**2))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr)*(aarr**2))
  aaA(4,2)=aaA(4,2)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aazc*(Ci*aakk(aaslyr)/aarr+1/(aarr**2))/(4*pi)
  aaA(5,2)=aaA(5,2)+0.0
  aaA(6,2)=aaA(6,2)-(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aaxc*(Ci*aakk(aaslyr)/aarr+1/(aarr**2))/(4*pi)
endif
endif

if (iHVK.ne.0) then
aaA(1,3)=integrator_res(10)*(aafreq_norm)*aax/(Ci*2*pi*freq) !Exz
aaA(2,3)=aaA(1,3)*aay/aax ! Eyz
aaA(3,3)=-Ci*2*pi*freq*aafreq_norm*(integrator_res(11)+integrator_res(12)/((2*pi*freq)**2)) ! Ezz
aaA(4,3)=integrator_res(13)*aay*(aafreq_norm)/aamur(aaolyr) ! Hxz
aaA(5,3)=-aaA(4,3)*aax/aay ! Hyz
aaA(6,3)=0.0
if (aaslyr.eq.aaolyr) then !direct term inclusion
aaA(1,3)=aaA(1,3)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aaxc*aazc*(-aakk(aaslyr)**2+Ci*3*aakk(aaslyr)/aarr+3/(aarr**2))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr)*(aarr**2))
aaA(2,3)=aaA(2,3)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aayc*aazc*(-aakk(aaslyr)**2+Ci*3*aakk(aaslyr)/aarr+3/(aarr**2))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr)*(aarr**2))
aaA(3,3)=aaA(3,3)+(aafreq_norm*(cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*(aakk(aaslyr)**2-Ci*aakk(aaslyr)/aarr-(1.0+(aazc**2)*(aakk(aaslyr)**2))/(aarr**2)+(3*Ci*aakk(aaslyr)*aazc**2)/(aarr**3)+(3*aazc**2)/(aarr**4)))/(Ci*2*pi*freq*4*pi*eps0*aaepsr(aaslyr))
aaA(4,3)=aaA(4,3)-(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aayc*(Ci*aakk(aaslyr)/aarr+1/(aarr**2))/(4*pi)
aaA(5,3)=aaA(5,3)+(aafreq_norm*cdexp(-Ci*aakk(aaslyr)*aarr)/aarr)*aaxc*(Ci*aakk(aaslyr)/aarr+1/(aarr**2))/(4*pi)
aaA(6,3)=aaA(6,3)+0.0
endif
endif

call normer(aaA,r,aaiHEK,3)
rimag=rimag/aafreq_norm ! denomalize the imaginary parts of the origin 

end subroutine threedlyrbasehor


subroutine twodlyrbase(aaiHEK,r,rimag,kmaxmult,aaA,obs_lyr)
implicit none
complex*16 freq
Integer(4) reflag,aaiHEK,i
real*8 xdata,ydata,kmaxmult
complex*16 dumm(0:mxlyr)
real*8 zconst
real*8 r(3),rimag(3)
Integer(4) halfcyc
complex*16 aafcFld
Integer(4) obs_lyr
Integer(4) path_id1, path_id2, path_id3, path_id4, path_id0
complex*16 aaA(10),ress(3), distx, distz, dist
Integer(4) idumy1,idumy2,idumy3,iErr
complex*16 cBes(0:20)

!!! CONSTANT INITIALIZATION
aaA=0.0
aafcFld=dreal(fcFld)-Ci*dabs((dimag(fcFld)))
aafreq_norm=dreal(aafcFld)/1e9
freq=aafcFld/aafreq_norm ! frequency to GHz
k00=2*pi*freq*dsqrt(eps0*mue0) ! wave number of freespace
aakmax=dreal(k00)**2-kgamma**2
call geomnormer(r,obs_lyr)
rimag=rimag*aafreq_norm ! normalize the imaginary origin values
kgamma=0.0

xdata=r(1) ! aax-information (field point)
ydata=r(2) ! y-information (field point)

if (obs_lyr.lt.1) then
  aaolyr=obslayer(r(2)) ! observation layer calculation (comment if the obslayer will be given outsite)
endif
aakey=3 ! order of integration (default is 6)

if (aaiHEK.eq.1) then ! check if the polarization is TM if so interchange parameters
  dumm=aaepsr
  aaepsr=aamur
  aamur=dumm
  if (peccheck(1).eq.1) then
  peccheck(1)=2
  elseif (peccheck(1).eq.2) then
  peccheck(1)=1
  endif
  if (peccheck(2).eq.2) then
  peccheck(2)=1
  elseif (peccheck(2).eq.1) then
  peccheck(2)=2
  endif
endif

reflag=0 !assume all the swps are purely real
!CALCULATE THE k VECTORS IN ALL OF THE LAYERS
if ((peccheck(1).eq.0).and.(peccheck(2).eq.0)) then
  aaepsr=aaepsr-(1d-8)*Ci
elseif (peccheck(1).eq.0) then
  aaepsr(1:aanoflyr)=aaepsr(1:aanoflyr)-(1d-8)*Ci
elseif (peccheck(2).eq.0) then
  aaepsr(0:aanoflyr-1)=aaepsr(0:aanoflyr-1)-(1d-8)*Ci
endif

do i=0,aanoflyr-1 !wavenumbers of each layer
  if (i.eq.0) then
    if (peccheck(1).eq.0) then
    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    aakk(i)=sqrt(aakk(i)**2-kgamma**2)
    else
    aakk(i)=0.0
    endif
  elseif (i.eq.aanoflyr-1) then
    if (peccheck(2).eq.0) then
    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    aakk(i)=sqrt(aakk(i)**2-kgamma**2)
    else
    aakk(i)=0.0
    endif
  else
    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    aakk(i)=sqrt(aakk(i)**2-kgamma**2)
    endif
  if (dreal(aakk(i)).lt.0.0) then
    reflag=1  !the swps have imaginary part
    aakk(i)=-aakk(i)
  else
    reflag=1
  endif
  if ((dreal(aakk(i))**2).gt.aakmax) then
    aakmax=dreal(aakk(i))**2 ! Find the maximum k vector
  endif
enddo
aakmax=sqrt(aakmax)*kmaxmult

if (swpflag.eq.0) then ! if not run before, run it once
  call swpoles(aaxte,aaxtm) ! determine the locations of surface wave poles
  swpflag=1
endif

!Start the main code
zconst=ydata
aaz=zconst
aazi=rimag(2)
aarho=xdata
aarhoi=rimag(1)
aarhoc=aarho+Ci*aarhoi
aazc=aaz+Ci*aazi

if (dreal(aadint).gt.0.0) then
  aadint=aadint*aakmax
else
  aadint=aakmax*1d-6
endif

if ((dabs(aarhoi) .gt. 1d-8).or.(dabs(aazi).gt.1d-8)) then  ! if there is some complex origin
    ! determine the paths according to the causality thm.
    call path_chooser(path_id1,path_id2) ! find the paths for the out-going waves
    call path_chooser_opp(path_id3,path_id4) ! find the paths for the in-coming waves
    path_id0=-1
    halfcyc=31 ! the number of partial integrations for the aitken algorithm
    ress=0.0
    call integrator2dc(halfcyc,ress,path_id0,path_id1,path_id2,path_id3,path_id4)
else
    halfcyc=21
    ress=0.0
    call integrator2d(halfcyc,ress)
endif

if (aaiHEK.eq.0) then
    aaA(3)=-Ci*freq*2*pi*ress(1)
    aaA(5)=-ress(2)/(mue0*aamur(aaolyr)) !
    aaA(4)=ress(3)/(mue0*aamur(aaolyr)) !
    if (aaslyr.eq.aaolyr) then !direct term inclusion 
      distx=dcmplx(aarho,aarhoi)
      distz=dcmplx(aaz,aazi)
      dist=cdsqrt(distx**2+distz**2) !complex distance
      if (dreal(dist).lt.0.0) dist=-dist ! the real part of the distance must be positive
      if (dimag(aakk(aaslyr)).gt.0.0) then
        dist=-dist*aakk(aaslyr) ! argument of the hankel function
      else
        dist=dist*aakk(aaslyr) ! argument of the hankel function
      endif
      idumy1=2
      idumy2=20
      idumy3=14
      call ch2(dist,cBes(0),idumy1,idumy2,idumy3,ierr)
      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
      if((ierr.ne.0).and.lDispWarn) then
        write(*,*) 'WARNING: Error',ierr,' in Bessel(',dist,')'
        lDispWarn=.false.
      end if
      aaA(5)=aaA(5)-(Ci*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1))/(4*dist)
      aaA(4)=aaA(4)+(Ci*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1))/(4*dist)
      aaA(3)=aaA(3)-(freq*aamur(aaslyr)*mue0*pi*cBes(0))/2
    endif
    aaA(5)=aaA(5)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
    aaA(4)=aaA(4)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
elseif (aaiHEK.eq.1) then
!Warning: Here epsilon and mu values are interchanged to convert from TE to TM. (by geomnormer)
    aaA(6)=Ci*freq*2*pi*ress(1)*eps0/mue0 !Hzz
    aaA(2)=-ress(2)/(mue0*aamur(aaolyr)) !
    aaA(1)=ress(3)/(mue0*aamur(aaolyr)) !
    if (aaslyr.eq.aaolyr) then !direct term inclusion
      distx=dcmplx(aarho,aarhoi)
      distz=dcmplx(aaz,aazi)
      dist=cdsqrt(distx**2+distz**2) !complex distance
      if (dreal(dist).lt.0.0) dist=-dist ! the real part of the distance must be positive
      if (dimag(aakk(aaslyr)).gt.0.0) then
        dist=-dist*aakk(aaslyr) ! argument of the hankel function
      else
        dist=dist*aakk(aaslyr) ! argument of the hankel function
      endif
      idumy1=2
      idumy2=20
      idumy3=14
      call ch2(dist,cBes(0),idumy1,idumy2,idumy3,ierr)
      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
      if((ierr.ne.0).and.lDispWarn) then
        write(*,*) 'WARNING: Error',ierr,' in Bessel(',dist,')'
        lDispWarn=.false.
      end if
      aaA(2)=aaA(2)-(Ci*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1))/(4*dist)
      aaA(1)=aaA(1)+(Ci*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1))/(4*dist)
      aaA(6)=aaA(6)+(freq*aamur(aaslyr)*eps0*pi*cBes(0))/2
    endif
    aaA(2)=aaA(2)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
    aaA(1)=aaA(1)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
endif
  call normer(aaA,r,aaiHEK,1)
  rimag=rimag/aafreq_norm
endsubroutine twodlyrbase

!!!!!!!!!!!!!

!!subroutine twodlyrbase_kgamma(aaiHEK,r,rimag,kmaxmult,aaA,obs_lyr,gamma_re,gamma_im)
!!implicit none
!!complex*16 freq
!!Integer(4) reflag,aaiHEK,i
!!real*8 xdata,ydata,kmaxmult,gamma_re,gamma_im
!!complex*16 dumm(0:mxlyr)
!!real*8 zconst
!!real*8 r(3),rimag(3)
!!Integer(4) halfcyc
!!complex*16 aafcFld, kkphase(0:mxlyr)
!!Integer(4) obs_lyr
!!Integer(4) path_id1, path_id2, path_id3, path_id4, path_id0
!!complex*16 aaA(10),ress(9), distx, distz, dist
!!Integer(4) idumy1,idumy2,idumy3,iErr
!!complex*16 cBes(0:20)
!!
!!!!! CONSTANT INITIALIZATION
!!aaA=0.0
!!aafcFld=dreal(fcFld)-Ci*dabs((dimag(fcFld)))
!!aafreq_norm=dreal(aafcFld)/1e9
!!freq=aafcFld/aafreq_norm ! frequency to GHz
!!k00=2*pi*freq*dsqrt(eps0*mue0) ! wave number of freespace
!!kgamma=k00*dcmplx(gamma_re,-gamma_im) ! causality change for exp(-iwt)
!!aakmax=dreal(k00)**2-kgamma**2
!!call geomnormer(r,obs_lyr)
!!rimag=rimag*aafreq_norm ! normalize the imaginary origin values
!!
!!xdata=r(1) ! aax-information (field point)
!!ydata=r(2) ! y-information (field point)
!!
!!if (obs_lyr.lt.1) then
!!  aaolyr=obslayer(r(2)) ! observation layer calculation (comment if the obslayer will be given outsite)
!!endif
!!aakey=3 ! order of integration (default is 6)
!!
!!if (aaiHEK.eq.1) then ! check if the polarization is TM if so interchange parameters
!!  dumm=aaepsr
!!  aaepsr=aamur
!!  aamur=dumm
!!  if (peccheck(1).eq.1) then
!!  peccheck(1)=2
!!  elseif (peccheck(1).eq.2) then
!!  peccheck(1)=1
!!  endif
!!  if (peccheck(2).eq.2) then
!!  peccheck(2)=1
!!  elseif (peccheck(2).eq.1) then
!!  peccheck(2)=2
!!  endif
!!endif
!!
!!reflag=0 !assume all the swps are purely real
!!!CALCULATE THE k VECTORS IN ALL OF THE LAYERS
!!if ((peccheck(1).eq.0).and.(peccheck(2).eq.0)) then
!!  aaepsr=aaepsr-(1d-8)*Ci
!!elseif (peccheck(1).eq.0) then
!!  aaepsr(1:aanoflyr)=aaepsr(1:aanoflyr)-(1d-8)*Ci
!!elseif (peccheck(2).eq.0) then
!!  aaepsr(0:aanoflyr-1)=aaepsr(0:aanoflyr-1)-(1d-8)*Ci
!!endif
!!
!!do i=0,aanoflyr-1 !wavenumbers of each layer
!!  if (i.eq.0) then
!!    if (peccheck(1).eq.0) then
!!    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
!!    aakk(i)=sqrt(aakk(i)**2-kgamma**2)
!!    kkphase(i)=aakk(i)/cdabs(aakk(i))
!!    else
!!    aakk(i)=0.0
!!    endif
!!  elseif (i.eq.aanoflyr-1) then
!!    if (peccheck(2).eq.0) then
!!    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
!!    aakk(i)=sqrt(aakk(i)**2-kgamma**2)
!!    kkphase(i)=aakk(i)/cdabs(aakk(i))
!!    else
!!    aakk(i)=0.0
!!    endif
!!  else
!!    aakk(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
!!    aakk(i)=sqrt(aakk(i)**2-kgamma**2)
!!    kkphase(i)=aakk(i)/cdabs(aakk(i))
!!  endif
!!  if (dreal(aakk(i)).lt.0.0) then
!!    reflag=1  !the swps have imaginary part
!!    aakk(i)=-aakk(i)
!!  else
!!    reflag=1
!!  endif
!!  if ((dreal(aakk(i))**2).gt.aakmax) then
!!    aakmax=dreal(aakk(i))**2 ! Find the maximum k vector
!!  endif
!!enddo
!!aakmax=sqrt(aakmax)*kmaxmult
!!
!!zconst=ydata
!!aaz=zconst
!!aazi=rimag(2)
!!aarho=xdata
!!aarhoi=rimag(1)
!!aarhoc=aarho+Ci*aarhoi
!!aazc=aaz+Ci*aazi
!!
!!aadint=aakmax*1d-9
!!
!!if ((dabs(aarhoi) .gt. 1d-8).or.(dabs(aazi).gt.1d-8)) then  ! if there is some complex origin
!!    ! determine the paths according to the causality thm.
!!    call path_chooser(path_id1,path_id2) ! find the paths for the out-going waves
!!    call path_chooser_opp(path_id3,path_id4) ! find the paths for the in-coming waves
!!!    path_id0=-1
!!!    halfcyc=31 ! the number of partial integrations for the aitken algorithm
!!!    ress=0.0
!!!    call integrator2dc_kgamma(halfcyc,ress,path_id0,path_id1,path_id2,path_id3,path_id4)
!!else
!!    halfcyc=21
!!    ress=0.0
!!!    call integrator2d_kgamma(halfcyc,ress)
!!endif
!!
!!if (aaiHEK.eq.0) then
!!!    aaA(1)=ress(3)*(aafreq_norm)/(Ci*2*pi*freq)
!!!    aaA(2)=-Ci*2*pi*freq*(aafreq_norm)*(ress(5)+ress(4)/((2*pi*freq)**2))
!!!    aaA(6)=ress(6)*aafreq_norm/(mue0*aamur(aaolyr))
!!!    aaA(3)=-Ci*freq*2*pi*ress(1)
!!!    aaA(5)=-ress(9)/(mue0*aamur(aaolyr)) !
!!!    aaA(4)=ress(7)/(mue0*aamur(aaolyr)) !
!!!aaA(1)=ress(2)
!!!aaA(2)=ress(3)
!!!aaA(3)=ress(4)
!!!aaA(4)=ress(5)
!!!aaA(5)=ress(6)
!!!aaA(6)=ress(9)
!!
!!
!!!    if (aaslyr.eq.aaolyr) then !direct term inclusion 
!!      distx=dcmplx(aarho,aarhoi)
!!      distz=dcmplx(aaz,aazi)
!!      dist=cdsqrt(distx**2+distz**2) !complex distance
!!      if (dreal(dist).lt.0.0) dist=-dist ! the real part of the distance must be positive
!!      if (dimag(aakk(aaslyr)).gt.0.0) then
!!        dist=-dist*aakk(aaslyr) ! argument of the hankel function
!!      else
!!        dist=dist*aakk(aaslyr) ! argument of the hankel function
!!      endif
!!      idumy1=2
!!      idumy2=20
!!      idumy3=14
!!      call ch2(dist,cBes(0),idumy1,idumy2,idumy3,ierr)
!!      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
!!      if((ierr.ne.0).and.lDispWarn) then
!!        write(*,*) 'WARNING: Error',ierr,' in Bessel(',dist,')'
!!        lDispWarn=.false.
!!      end if
!!      aaA(3)=-0.5*pi*freq*cBes(0)*(mue0*aamur(aaslyr)-(kgamma**2/(((2*aafreq_norm*pi*freq)**2)*eps0*aaepsr(aaslyr))))
!!      aaA(2)=(1/(Ci*8*pi*freq*dist*eps0*aaepsr(aaslyr)))*kgamma*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1)
!!      aaA(1)=(1/(Ci*8*pi*freq*dist*eps0*aaepsr(aaslyr)))*kgamma*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1)
!!      aaA(6)=0.0
!!      aaA(5)=(1/(4*Ci*dist))*aakk(aaslyr)*distx*cBes(1)*aakk(aaslyr)
!!      aaA(4)=-(1/(4*Ci*dist))*aakk(aaslyr)*distz*cBes(1)*aakk(aaslyr)
!!!      aaA(5)=aaA(5)-(Ci*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1))/(4*dist)
!!!      aaA(4)=aaA(4)+(Ci*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1))/(4*dist)
!!!      aaA(3)=aaA(3)-(freq*aamur(aaslyr)*mue0*pi*cBes(0))/2
!!!    endif
!!!    aaA(5)=(kkphase(aaslyr)**2)*aaA(5)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
!!!    aaA(4)=(kkphase(aaslyr)**2)*aaA(4)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
!!!    aaA(3)=(kkphase(aaslyr)**2)*aaA(3)
!!!    aaA(2)=-(aaepsr(aaslyr)/aaepsr(aaolyr))*((kkphase(aaslyr)**2)*aaA(2)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2))*kgamma/(freq*2*pi*eps0*aaepsr(aaslyr))
!!!    aaA(1)=(aaepsr(aaolyr)/aaepsr(aaslyr))*((kkphase(aaslyr)**2)*aaA(1)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2))*kgamma/(freq*2*pi*eps0*aaepsr(aaslyr))
!!!    aaA(6)=aaA(6)
!!elseif (aaiHEK.eq.1) then
!!!Warning: Here epsilon and mu values are interchanged to convert from TE to TM. (by geomnormer)
!!    aaA(6)=Ci*freq*2*pi*ress(1)*eps0/mue0 !Hzz
!!    aaA(2)=-ress(2)/(mue0*aamur(aaolyr)) !
!!    aaA(1)=ress(3)/(mue0*aamur(aaolyr)) !
!!    if (aaslyr.eq.aaolyr) then !direct term inclusion
!!      distx=dcmplx(aarho,aarhoi)
!!      distz=dcmplx(aaz,aazi)
!!      dist=cdsqrt(distx**2+distz**2) !complex distance
!!      if (dreal(dist).lt.0.0) dist=-dist ! the real part of the distance must be positive
!!      if (dimag(aakk(aaslyr)).gt.0.0) then
!!        dist=-dist*aakk(aaslyr) ! argument of the hankel function
!!      else
!!        dist=dist*aakk(aaslyr) ! argument of the hankel function
!!      endif
!!      idumy1=2
!!      idumy2=20
!!      idumy3=14
!!      call ch2(dist,cBes(0),idumy1,idumy2,idumy3,ierr)
!!      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
!!      if((ierr.ne.0).and.lDispWarn) then
!!        write(*,*) 'WARNING: Error',ierr,' in Bessel(',dist,')'
!!        lDispWarn=.false.
!!      end if
!!      aaA(2)=aaA(2)-(Ci*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1))/(4*dist)
!!      aaA(1)=aaA(1)+(Ci*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1))/(4*dist)
!!      aaA(6)=aaA(6)+(freq*aamur(aaslyr)*eps0*pi*cBes(0))/2
!!    endif
!!    aaA(2)=aaA(2)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
!!    aaA(1)=aaA(1)*(k00**2)*aaepsr(aaslyr)*aamur(aaslyr)/(aakk(aaslyr)**2)
!!endif
!!  call normer(aaA,r,aaiHEK,1)
!!  rimag=rimag/aafreq_norm
!!
!!endsubroutine twodlyrbase_kgamma

subroutine normer(aaA,r,aaiHEK,ssaa)
implicit none
integer ssaa
complex*16 aaA(10,ssaa), dumm(0:mxlyr)
real*8 r(3)
Integer(4) i, aaiHEK

aaA=aaA*aafreq_norm
r=r/aafreq_norm
aah=aah/aafreq_norm
do i=1,aanoflyr-2
  aadd(i)=aadd(i)/aafreq_norm
enddo
if (aaiHEK.eq.1) then
  dumm=aaepsr
  aaepsr=aamur
  aamur=dumm
  if (peccheck(1).eq.1) then
  peccheck(1)=2
  elseif (peccheck(1).eq.2) then
  peccheck(1)=1
  endif
  if (peccheck(2).eq.2) then
  peccheck(2)=1
  elseif (peccheck(2).eq.1) then
  peccheck(2)=2
  endif
endif

endsubroutine normer

subroutine geomnormer(r,obs_lyr)
implicit none
real*8  r(3)
Integer(4) i,obs_lyr

aah=aah*aafreq_norm
aadd(1:aanoflyr-2)=aadd(1:aanoflyr-2)*aafreq_norm
r=r*aafreq_norm
do i=1,aanoflyr
  aaepsr(i-1)=dreal(eDom(i))-Ci*dimag(eDom(i))
  aamur(i-1)=dreal(uDom(i))-Ci*dimag(uDom(i))
enddo
aaslyr=source_lyr-1
aaolyr=obs_lyr-1
endsubroutine geomnormer


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! BESSEL FUNCTIONS J0 J1 and Y0 FOR ANY real*8 VARIABLE aax !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real*8 function bsj0(aax)
real*8 y,p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,xx,aax,z,ax
real*8 r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,s5,s6
data p1,p2,p3,p4,p5/1.d0,-0.1098628627d-2,0.2734510407d-4,-0.2073370639d-5,0.2093887211d-6/
data q1,q2,q3,q4,q5/-0.1562499995d-1,0.1430488765d-3,-0.6911147651d-5,0.7621095161d-6,-0.934945152d-7/
data r1,r2,r3,r4,r5,r6/57568490574.0d0,-13362590354.0d0,651619640.7d0,-11214424.18d0,77392.33017d0,-184.9052456d0/
data s1,s2,s3,s4,s5,s6/57568490411.0d0,1029532985.0d0,9494680.718d0,59272.64853d0,267.8532712d0,1.d0/
!
if(dabs(aax).lt.8.0d0) then
  y=aax**2
  bsj0=(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/(s1+y*(s2+y*(s3+y*(s4+y*(s5+y*s6)))))
else
  ax=dabs(aax)
  z=8.0d0/ax
  y=z**2
  xx=ax-.785398164d0
  bsj0=dsqrt(.636619772d0/ax)*(dcos(xx)*(p1+y*(p2+y*(p3+y*(p4+y*p5))))-z*dsin(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))
endif
return
end function

real*8 function bsj1(aax)
real*8 y,p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,xx,aax,z,ax
real*8 r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,s5,s6
DATA P1,P2,P3,P4,P5/1.D0,.183105D-2,-.3516396496D-4,.2457520174D-5,-.240337019D-6/
DATA Q1,Q2,Q3,Q4,Q5/.04687499995D0,-.2002690873D-3,.8449199096D-5,-.88228987D-6,.105787412D-6/
DATA R1,R2,R3,R4,R5,R6/72362614232.D0,-7895059235.D0,242396853.1D0,-2972611.439D0,15704.48260D0,-30.16036606D0/
DATA S1,S2,S3,S4,S5,S6/144725228442.D0,2300535178.D0,18583304.74D0,99447.43394D0,376.9991397D0,1.D0/

if(dabs(aax).lt.8.0d0) then
  y=aax**2
  bsj1=aax*(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/(s1+y*(s2+y*(s3+y*(s4+y*(s5+y*s6)))))
else
  AX=ABS(aax)
  Z=8./AX
  Y=Z**2
  XX=AX-2.356194491
  BSJ1=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))*SIGN(1.0d0,aax)
endif
return
end function

!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
real*8 function bsy0(aax)
real*8 y,p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,xx,z,aax
real*8 r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,s5,s6
data p1,p2,p3,p4,p5/1.d0,-0.1098628627d-2,0.2734510407d-4,-0.2073370639d-5,0.2093887211d-6/
data q1,q2,q3,q4,q5/-0.1562499995d-1,0.1430488765d-3,-0.6911147651d-5,0.7621095161d-6,-0.934945152d-7/
data r1,r2,r3,r4,r5,r6/-2957821389.d0,7062834065.d0,-512359803.6d0,10879881.29d0,-86327.92757d0,228.4622733d0/
data s1,s2,s3,s4,s5,s6/40076544269.d0,745249964.8d0,7189466.438d0,47447.26470d0,226.1030244d0,1.d0/
!
if(aax.lt.8.0d0) then
  y=aax**2
  bsy0=(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/(s1+y*(s2+y*(s3+y*(s4+y*(s5+y*s6)))))+.636619772d0*bsj0(aax)*dlog(aax)
else
  z=8.0d0/aax
  y=z**2
  xx=aax-.785398164d0
  bsy0=dsqrt(.636619772d0/aax)*(dsin(xx)*(p1+y*(p2+y*(p3+y*(p4+y*p5))))+z*dcos(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))
endif
return
end function

function besszero(mzero)
integer mzero
real*8 besszero,zero_locs(100)

zero_locs=[2.40482555769577,5.52007811028631,8.65372791291101,11.7915344390143,14.9309177084878,18.0710639679109,21.2116366298793,24.3524715307493,27.4934791320403,30.6346064684320,33.7758202135736,36.9170983536640,40.0584257646282,43.1997917131767,46.3411883716618,49.4826098973978,52.6240518411150,55.7655107550200,58.9069839260809,62.0484691902272,65.1899648002069,68.3314693298568,71.4729816035937,74.6145006437018,77.7560256303881,80.8975558711376,84.0390907769382,87.1806298436412,90.3221726372105,93.4637187819448,96.6052679509963,99.7468198586806,102.888374254195,106.029930916452,109.171489649805,112.313050280495,115.454612653667,118.596176630873,121.737742087951,124.879308913233,128.020877006008,131.162446275214,134.304016638305,137.445588020284,140.587160352854,143.728733573690,146.870307625797,150.011882456955,153.153458019228,156.295034268534,159.436611164263,162.578188668947,165.719766747955,168.861345369236,172.002924503078,175.144504121903,178.286084200074,181.427664713731,184.569245640639,187.710826960049,190.852408652582,193.993990700109,197.135573085661,200.277155793332,203.418738808199,206.560322116244,209.701905704294,212.843489559950,215.985073671534,219.126658028041,222.268242619084,225.409827434859,228.551412466099,231.692997704039,234.834583140383,237.976168767276,241.117754577268,244.259340563296,247.400926718653,250.542513036970,253.684099512193,256.825686138564,259.967272910605,263.108859823096,266.250446871066,269.392034049776,272.533621354705,275.675208781537,278.816796326153,281.958383984615,285.099971753160,288.241559628188,291.383147606255,294.524735684065,297.666323858459,300.807912126411,303.949500485021,307.091088931505,310.232677463195,313.374266077528]

! the location of the mzeroth zero of the zeroth order bessel function
if (mzero.lt.101) then
  besszero=zero_locs(mzero)
else
  besszero=pi*Dble(mzero*4-1)*0.25d0+1.0d0/(2.0d0*pi*Dble(4*mzero-1))-1.0d0/(6.0d0*pi**3*Dble(4*mzero-1)**3)+ &
  & 3779.0d0/(15.0d0*pi**5*Dble(4*mzero-1)**5)
endif
end function

function besszero1(mzero)
integer mzero
real*8 besszero1,zero_locs(100)

zero_locs=[3.83170597020751, 7.01558666981562, 10.1734681350627, 13.3236919363142, 16.4706300508776, 19.6158585104682, 22.7600843805928, 25.9036720876184, 29.0468285349169, 32.1896799109744, 35.3323075500839, 38.4747662347716,	41.6170942128145, 44.7593189976528, 47.9014608871854, 51.0435351835715, 54.1855536410613, 57.3275254379010, 60.4694578453475, 63.6113566984812,	66.7532267340985, 69.8950718374958, 73.0368952255738, 76.1786995846415, 79.3204871754763, 82.4622599143736, 85.6040194363502, 88.7457671449263,	91.8875042516950, 95.0292318080447, 98.1709507307908, 101.312661823039, 104.454365791283, 107.596063259509, 110.737754780899, 113.879440847595, 117.021121898892, 120.162798328149, 123.304470488636, 126.446138698517, 129.587803245104, 132.729464388510, 135.871122364789, 139.012777388660, 142.154429655859, 145.296079345196, 148.437726620342, 151.579371631401, 154.721014516286, 157.862655401930, 161.004294405362, 164.145931634650, 167.287567189744, 170.429201163227, 173.570833640976, 176.712464702764, 179.854094422788,182.995722870153, 186.137350109296, 189.278976200376, 192.420601199626, 195.562225159663, 198.703848129777, 201.845470156191, 204.987091282292, 208.128711548850, 211.270330994208, 214.411949654462, 217.553567563624, 220.695184753769, 223.836801255172, 226.978417096429, 230.120032304579, 233.261646905201, 236.403260922514, 239.544874379470, 242.686487297829, 245.828099698240, 248.969711600310, 252.111323022669, 255.252933983028, 258.394544498239, 261.536154584344, 264.677764256622, 267.819373529635, 270.960982417271, 274.102590932781, 277.244199088815, 280.385806897456, 283.527414370251, 286.669021518243, 289.810628351994, 292.952234881614, 296.093841116782, 299.235447066774, 302.377052740477, 305.518658146416, 308.660263292764, 311.801868187370, 314.943472837767]

! the location of the mzeroth zero of the first order bessel function
if (mzero.lt.101) then
  besszero1=zero_locs(mzero)
else
  besszero1=(pi*Dble(mzero*4-1)*0.35d0+1.0d0/(2.0d0*pi*Dble(4*mzero-1))-1.0d0/(6.0d0*pi**3*Dble(4*mzero-1)**3)+ &
  & 3779.0d0/(15.0d0*pi**5*Dble(4*mzero-1)**5))+pi*0.5d0
endif
end function




!! the function to determine the observation layer number
Integer(4) function obslayer(zz)
Integer(4) i
real*8 dddum,hh,zz

hh=aah
if ((zz+aah).eq.0.0) then
  obslayer=aaslyr
  aah=hh
  return
elseif (aanoflyr.eq.2) then
  if ((zz+aah).gt.0.0) then
    obslayer=1
    aah=hh
    return
  elseif ((zz+aah).lt.0.0) then
    obslayer=0
    aah=hh
    return
  endif
else
dddum=0.0
  if ((zz+aah).gt.0.0) then
    do i=aaslyr,aanoflyr-2
    dddum=dddum+aadd(i)
      if ((zz+aah).lt.dddum) then
        obslayer=i
        aah=hh
        return
      endif
    enddo
    obslayer=aanoflyr-1
    aah=hh
    return
  elseif ((zz+aah).lt.0.0) then
    do i=aaslyr-1,1,-1
    dddum=dddum+aadd(i)
      if (dabs(zz+aah).lt.dddum) then
        obslayer=i
        aah=hh
        return
      endif
    enddo
    obslayer=0
    aah=hh
    return
  endif  
endif
end function

!!AITKEN PROCEDURE TO INCREASE THE CONVERGENCE RATE OF THE INTEGRAL
! n: length of the series

subroutine aitkenvec(y,n,ress)
Integer(4) n,i,j
complex*16 y(15,n),ress(15),q(n-2)
do j=1,15
do i=0,n-3
  if ((y(j,i+3)-2*y(j,i+2)+y(j,i+1)).eq.0.0) then
    q(i+1)=y(j,i+1)-(y(j,i+2)-y(j,i+1))**2/1
  else
    q(i+1)=y(j,i+1)-(y(j,i+2)-y(j,i+1))**2/(y(j,i+3)-2*y(j,i+2)+y(j,i+1))
  endif
enddo
ress(j)=q(n-2)
enddo
end subroutine


subroutine aitkenvec2d(y,n,ress)
Integer(4) n,i,j
complex*16 y(3,n),ress(3),q(n-2)
do j=1,3
do i=0,n-3
  if ((y(j,i+3)-2*y(j,i+2)+y(j,i+1)).eq.0.0) then
    q(i+1)=y(j,i+1)-(y(j,i+2)-y(j,i+1))**2/1
  else
    q(i+1)=y(j,i+1)-(y(j,i+2)-y(j,i+1))**2/(y(j,i+3)-2*y(j,i+2)+y(j,i+1))
  endif
enddo
ress(j)=q(n-2)
enddo
end subroutine

subroutine integrator3d(halfcyc,ress,iHVK)
Integer(4) halfcyc,counter,i,checker,attn
Integer(2) iHVK
real*8 dummbess,a
real*8 krhor(1)
real*8 reslt(35),reslth(35),resltv(35)
real*8 aagk(1),bbgk(1)
integer mm,iOK
complex*16 resltorg(15,halfcyc)
complex*16 ress(15)

attn=0
checker=0

aagk(1)=1d-6 ! the first bound of the integration (1e-6 to inf by aitken)

counter=0
do
  counter=counter+1
  a=dabs(aarho)
  b=besszero(counter)
  dummbess=b/a
  if (dummbess.gt.15000) then
  dummbess=15000
  attn=1
  endif
  if (dummbess.gt.aakmax) then
    exit
  endif
enddo
bbgk(1)=dummbess
reslth=0.0
resltv=0.0
if (iHVK.ne.1) then
call intgk(aaintegrands3dhor,23,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslth,mm,iOK)
endif
if (iHVK.ne.0) then
call intgk(aaintegrands3dvert,9,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltv,mm,iOK)
endif
reslt(1:9)=reslth(2:10)
reslt(10:13)=resltv(2:5)
reslt(14)=reslth(11)
reslt(15:23)=reslth(12:20)
reslt(24:27)=resltv(6:9)
reslt(28)=reslth(21)

call resltorganizer(1,reslt,halfcyc,resltorg)
resltorg(15,1)=reslth(22)+Ci*reslth(23)

do i=2,halfcyc
  counter=counter+1
  aagk(1)=bbgk(1)
  if (attn.eq.1) then
    bbgk(1)=aagk(1)+2500
  else
    bbgk(1)=besszero(counter)/dabs(aarho)
  endif 
  
  if (iHVK.ne.1) then
  call intgk(aaintegrands3dhor,23,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslth,mm,iOK)
  endif
  if (iHVK.ne.0) then
  call intgk(aaintegrands3dvert,9,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltv,mm,iOK)
  endif
  reslt(1:9)=reslth(2:10)
  reslt(10:13)=resltv(2:5)
  reslt(14)=reslth(11)
  reslt(15:23)=reslth(12:20)
  reslt(24:27)=resltv(6:9)
  reslt(28)=reslth(21)
  call resltorganizer(i,reslt,halfcyc,resltorg)
  resltorg(15,i)=reslth(22)+Ci*reslth(23)
  resltorg(15,i)=resltorg(15,i-1)+resltorg(15,i)

  if ((dabs(reslt(1))+dabs(reslt(10))).lt.aaepsrel**2) then
    checker=1
    ress=resltorg(:,i)
    exit
  endif
enddo
if (checker.eq.0) then
  call aitkenvec(resltorg,halfcyc,ress)
endif
ress(1)=ress(1)*mue0*aamur(aaolyr)/(4*pi)
ress(2)=ress(2)/(4*pi*eps0*aaepsr(aaolyr))
ress(3)=ress(3)/(4*pi*eps0*aaepsr(aaolyr))
ress(4)=ress(4)/(4*pi*eps0*aaepsr(aaolyr))
ress(5)=ress(5)*mue0*aamur(aaolyr)/(4*pi)
ress(6)=ress(6)*aamur(aaolyr)/(4*pi)
ress(7)=ress(7)*aamur(aaolyr)/(4*pi)
ress(8)=ress(8)*aamur(aaolyr)/(4*pi)
ress(9)=ress(9)*aamur(aaolyr)/(4*pi)
ress(10)=ress(10)/(4*pi*eps0*aaepsr(aaolyr))
ress(11)=ress(11)*mue0*aamur(aaolyr)/(4*pi)
ress(12)=ress(12)/(4*pi*eps0*aaepsr(aaolyr))
ress(13)=ress(13)*aamur(aaolyr)/(4*pi)
ress(14)=ress(14)/(4*pi*eps0*aaepsr(aaolyr))
ress(15)=ress(15)*aamur(aaolyr)/(4*pi)
return
end subroutine integrator3d

subroutine resltorganizer(inndex,reslt,halfcycx,resltorg)
integer*4 inndex,i,halfcycx
complex*16 resltorg(15,halfcycx)
real*8 reslt(35)

if (inndex.eq.1) then
  do i=1,14
    resltorg(i,inndex)=reslt(i)+Ci*reslt(i+14)
  enddo
else
  do i=1,14
    resltorg(i,inndex)=reslt(i)+Ci*reslt(i+14)
    resltorg(i,inndex)=resltorg(i,inndex-1)+resltorg(i,inndex)
  enddo
endif
end subroutine


subroutine integrator2d(halfcyc,ress)
Integer(4) halfcyc,counter,i,checker,attn
real*8 dummbess,a,b
real*8 krhor(1)
real*8 resltpos(35),resltneg(35)
complex*16 respos(3,halfcyc), resneg(3,halfcyc)
real*8 aagk(1),bbgk(1)
integer mm,iOK
complex*16 ress(3), resspos(3), ressneg(3)

attn=0
checker=0

aagk(1)=1d-6 ! the first bound of the integration (1e-6 to inf by aitken)

counter=0
do
  counter=counter+1
  a=dabs(aarho)
  b=counter*pi
  dummbess=b/a
  if (dummbess.gt.15000) then
  dummbess=15000
  attn=1
  endif
  if (dummbess.gt.aakmax) then
    exit
  endif
enddo
bbgk(1)=dummbess

call intgk(aaintegrands2d,7,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
call intgk(aaintegrands2d,7,krhor,1,aagk,-bbgk,aakey,100000,aaepsrel,resltneg,mm,iOK)
respos(1,1)=dcmplx(resltpos(2),resltpos(5))
respos(2,1)=dcmplx(resltpos(3),resltpos(6))
respos(3,1)=dcmplx(resltpos(4),resltpos(7))
resneg(1,1)=dcmplx(resltneg(2),resltneg(5))
resneg(2,1)=dcmplx(resltneg(3),resltneg(6))
resneg(3,1)=dcmplx(resltneg(4),resltneg(7))

do i=2,halfcyc
  counter=counter+1
  aagk(1)=bbgk(1)
  if (attn.eq.1) then
    bbgk(1)=aagk(1)+2500
  else
    bbgk(1)=aagk(1)+pi/dabs(aarho)
  endif 
  call intgk(aaintegrands2d,7,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
  respos(1,i)=respos(1,i-1)+dcmplx(resltpos(2),resltpos(5))
  respos(2,i)=respos(2,i-1)+dcmplx(resltpos(3),resltpos(6))
  respos(3,i)=respos(3,i-1)+dcmplx(resltpos(4),resltpos(7))
  call intgk(aaintegrands2d,7,krhor,1,-aagk,-bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
  resneg(1,i)=resneg(1,i-1)+dcmplx(resltpos(2),resltpos(5))
  resneg(2,i)=resneg(2,i-1)+dcmplx(resltpos(3),resltpos(6))
  resneg(3,i)=resneg(3,i-1)+dcmplx(resltpos(4),resltpos(7))
  if (dabs(resltpos(1)).lt.aaepsrel**2) then
    checker=1
    ress=(respos(:,i)-resneg(:,i))
    exit
  endif
enddo
if (checker.eq.0) then
  call aitkenvec2d(respos,halfcyc,resspos)
  call aitkenvec2d(resneg,halfcyc,ressneg)
  ress=resspos-ressneg
endif
ress=ress*mue0*aamur(aaolyr)/(4*pi)
return
end subroutine integrator2d


!subroutine integrator2d_kgamma(halfcyc,ress)
!Integer(4) halfcyc,counter,i,checker,attn
!real*8 dummbess,a,b
!real*8 krhor(1)
!real*8 resltpos(35),resltneg(35)
!complex*16 respos(9,halfcyc), resneg(9,halfcyc)
!real*8 aagk(1),bbgk(1)
!integer mm,iOK
!complex*16 ress(9), resspos(9), ressneg(9)
!
!attn=0
!checker=0
!
!aagk(1)=1d-6 ! the first bound of the integration (1e-6 to inf by aitken)
!
!counter=0
!do
!  counter=counter+1
!  a=dabs(aarho)
!  b=counter*pi
!  dummbess=b/a
!  if (dummbess.gt.15000) then
!  dummbess=15000
!  attn=1
!  endif
!  if (dummbess.gt.aakmax) then
!    exit
!  endif
!enddo
!bbgk(1)=dummbess
!
!call intgk(aaintegrands2d_kgamma,19,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
!call intgk(aaintegrands2d_kgamma,19,krhor,1,-aagk,-bbgk,aakey,100000,aaepsrel,resltneg,mm,iOK)
!respos(1,1)=dcmplx(resltpos(2),resltpos(11))
!respos(2,1)=dcmplx(resltpos(3),resltpos(12))
!respos(3,1)=dcmplx(resltpos(4),resltpos(13))
!respos(4,1)=dcmplx(resltpos(5),resltpos(14))
!respos(5,1)=dcmplx(resltpos(6),resltpos(15))
!respos(6,1)=dcmplx(resltpos(7),resltpos(16))
!respos(7,1)=dcmplx(resltpos(8),resltpos(17))
!respos(8,1)=dcmplx(resltpos(9),resltpos(18))
!respos(9,1)=dcmplx(resltpos(10),resltpos(19))
!resneg(1,1)=dcmplx(resltneg(2),resltneg(11))
!resneg(2,1)=dcmplx(resltneg(3),resltneg(12))
!resneg(3,1)=dcmplx(resltneg(4),resltneg(13))
!resneg(4,1)=dcmplx(resltneg(5),resltneg(14))
!resneg(5,1)=dcmplx(resltneg(6),resltneg(15))
!resneg(6,1)=dcmplx(resltneg(7),resltneg(16))
!resneg(7,1)=dcmplx(resltneg(8),resltneg(17))
!resneg(8,1)=dcmplx(resltneg(9),resltneg(18))
!resneg(9,1)=dcmplx(resltneg(10),resltneg(19))
!
!do i=2,halfcyc
!  counter=counter+1
!  aagk(1)=bbgk(1)
!  if (attn.eq.1) then
!    bbgk(1)=aagk(1)+2500
!  else
!    bbgk(1)=aagk(1)+pi/dabs(aarho)
!  endif 
!  call intgk(aaintegrands2d_kgamma,19,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
!  respos(1,i)=respos(1,i-1)+dcmplx(resltpos(2),resltpos(11))
!  respos(2,i)=respos(2,i-1)+dcmplx(resltpos(3),resltpos(12))
!  respos(3,i)=respos(3,i-1)+dcmplx(resltpos(4),resltpos(13))
!  respos(4,i)=respos(4,i-1)+dcmplx(resltpos(5),resltpos(14))
!  respos(5,i)=respos(5,i-1)+dcmplx(resltpos(6),resltpos(15))
!  respos(6,i)=respos(6,i-1)+dcmplx(resltpos(7),resltpos(16))
!  respos(7,i)=respos(7,i-1)+dcmplx(resltpos(8),resltpos(17))
!  respos(8,i)=respos(8,i-1)+dcmplx(resltpos(9),resltpos(18))
!  respos(9,i)=respos(9,i-1)+dcmplx(resltpos(10),resltpos(19))
!
!  call intgk(aaintegrands2d_kgamma,19,krhor,1,-aagk,-bbgk,aakey,100000,aaepsrel,resltneg,mm,iOK)
!  resneg(1,i)=resneg(1,i-1)+dcmplx(resltneg(2),resltneg(11))
!  resneg(2,i)=resneg(2,i-1)+dcmplx(resltneg(3),resltneg(12))
!  resneg(3,i)=resneg(3,i-1)+dcmplx(resltneg(4),resltneg(13))
!  resneg(4,i)=resneg(4,i-1)+dcmplx(resltneg(5),resltneg(14))
!  resneg(5,i)=resneg(5,i-1)+dcmplx(resltneg(6),resltneg(15))
!  resneg(6,i)=resneg(6,i-1)+dcmplx(resltneg(7),resltneg(16))
!  resneg(7,i)=resneg(7,i-1)+dcmplx(resltneg(8),resltneg(17))
!  resneg(8,i)=resneg(8,i-1)+dcmplx(resltneg(9),resltneg(18))
!  resneg(9,i)=resneg(9,i-1)+dcmplx(resltneg(10),resltneg(19))
!  if (dabs(resltpos(1)).lt.aaepsrel**2) then
!    checker=1
!    ress=(respos(:,i)-resneg(:,i))
!    exit
!  endif
!enddo
!if (checker.eq.0) then
!  call aitkenvec2d(respos,halfcyc,resspos)
!  call aitkenvec2d(resneg,halfcyc,ressneg)
!  ress=resspos-ressneg
!endif
!ress=ress*mue0*aamur(aaolyr)/(4*pi)
!return
!end subroutine integrator2d_kgamma
!
!


!
!                              AMPREF
!
! subroutine AMPREF calculates the generalized reflection coefficients
! between all adjacent layers,  and the iterative wave amplitude factors
! at the layer where the observation  point is located.
!
!
!
subroutine ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!
real*8 zm
Integer(4) j
complex*16 sp,sm,tran,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
!


!initializations for the semi-infinite layers
rte(0,-1)=0
rte(-1,0)=0
rtm(0,-1)=0
rtm(-1,0)=0
rte(aanoflyr,aanoflyr-1)=0
rte(aanoflyr-1,aanoflyr)=0
rtm(aanoflyr,aanoflyr-1)=0
rtm(aanoflyr-1,aanoflyr)=0

if(peccheck(1).eq.1) then  ! check if the lowermost layer is PEC
  rte(1,0)=-1
  rtm(1,0)=1
elseif(peccheck(1).eq.2) then ! check if the lowermost layer is PMC
  rte(1,0)=1
  rtm(1,0)=-1
else
  rte(1,0)=(aamur(0)*aakz(1)-aamur(1)*aakz(0))/(aamur(0)*aakz(1)+aamur(1)*aakz(0))
  rtm(1,0)=(aaepsr(0)*aakz(1)-aaepsr(1)*aakz(0))/(aaepsr(0)*aakz(1)+aaepsr(1)*aakz(0))
end if
!
do j=2,aaslyr
  rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
  rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
  rte(j-1,j)=-rte(j,j-1)
  rtm(j-1,j)=-rtm(j,j-1)
enddo
!
if(peccheck(2).eq.1) then !the upper most layer is PEC
  rte(aanoflyr-2,aanoflyr-1)=-1
  rtm(aanoflyr-2,aanoflyr-1)=1
elseif(peccheck(2).eq.2) then !The uppermost layer is PMC
  rte(aanoflyr-2,aanoflyr-1)=1
  rtm(aanoflyr-2,aanoflyr-1)=-1
else
  rte(aanoflyr-2,aanoflyr-1)=(aamur(aanoflyr-1)*aakz(aanoflyr-2)-aamur(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aamur(aanoflyr-1)*aakz(aanoflyr-2)+aamur(aanoflyr-2)*aakz(aanoflyr-1))
  rtm(aanoflyr-2,aanoflyr-1)=(aaepsr(aanoflyr-1)*aakz(aanoflyr-2)-aaepsr(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aaepsr(aanoflyr-1)*aakz(aanoflyr-2)+aaepsr(aanoflyr-2)*aakz(aanoflyr-1))
end if
!
if(aaslyr.ne.aanoflyr-1) then
  do j=aanoflyr-2,aaslyr+1,-1
    rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
    rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
    rte(j-1,j)=-rte(j,j-1)
    rtm(j-1,j)=-rtm(j,j-1)
  end do
end if
!
!      generalized TE reflection coefficients
!
!      below the source layer
!

grte(0,-1)=0
grte(-1,0)=0
grtm(0,-1)=0
grtm(-1,0)=0
grte(aanoflyr,aanoflyr-1)=0
grte(aanoflyr-1,aanoflyr)=0
grtm(aanoflyr,aanoflyr-1)=0
grtm(aanoflyr-1,aanoflyr)=0

grte(1,0)=rte(1,0)
do j=1,aaslyr-1
  grte(j+1,j)=(rte(j+1,j)+grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grte(aanoflyr-2,aanoflyr-1)=rte(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grte(j-1,j)=(rte(j-1,j)+grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!      generalized TM reflection coefficients
!
!      below the source layer
!
grtm(1,0)=rtm(1,0)
do j=1,aaslyr-1
  grtm(j+1,j)=(rtm(j+1,j)+grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grtm(aanoflyr-2,aanoflyr-1)=rtm(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grtm(j-1,j)=(rtm(j-1,j)+grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!         calculate the amplitude factors (TE)
!
ampte=(1.0d0,0.0d0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0.0d0
  do while (j.le.aaolyr)
    tran=1+rte(j-1,j)
    sp=tran
    sp=tran/(1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aah))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aah))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rte(j+1,j)
    sm=tran
    sm=tran/(1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(Ci*aakz(j+1)*(-aah-zm))*sm/cdexp(Ci*aakz(j)*(-aah-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
!
!         calculate the amplitude factors (TM)
!
amptm=(1.,0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0
  do while (j.le.aaolyr)
    tran=1+rtm(j-1,j)
    sp=tran/(1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aah))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aah))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rtm(j+1,j)
    sm=tran/(1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(Ci*aakz(j+1)*(-aah-zm))*sm/cdexp(Ci*aakz(j)*(-aah-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
return
END subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                              AMPREFTM
!
! subroutine AMPREF calculates the generalized reflection coefficients
! between all adjacent layers,  and the iterative wave amplitude factors
! at the layer where the observation  point is located.
!
!
!
subroutine ampreftm(rtm,grtm,zm,amptm)
!
real*8 zm
Integer(4) j
complex*16 sp,sm,tran,amptm
complex*16 rtm(-1:mxlyr,-1:mxlyr)
complex*16 grtm(-1:mxlyr,-1:mxlyr)
!


!initializations for the semi-infinite layers
rtm(0,-1)=0
rtm(-1,0)=0
rtm(aanoflyr,aanoflyr-1)=0
rtm(aanoflyr-1,aanoflyr)=0

if(peccheck(1).eq.1) then  ! check if the lowermost layer is PEC
  rtm(1,0)=1
elseif(peccheck(1).eq.2) then ! check if the lowermost layer is PMC
  rtm(1,0)=-1
else
  rtm(1,0)=(aaepsr(0)*aakz(1)-aaepsr(1)*aakz(0))/(aaepsr(0)*aakz(1)+aaepsr(1)*aakz(0))
end if
!
do j=2,aaslyr
  rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
  rtm(j-1,j)=-rtm(j,j-1)
enddo
!
if(peccheck(2).eq.1) then !the upper most layer is PEC
  rtm(aanoflyr-2,aanoflyr-1)=1
elseif(peccheck(2).eq.2) then !The uppermost layer is PMC
  rtm(aanoflyr-2,aanoflyr-1)=-1
else
  rtm(aanoflyr-2,aanoflyr-1)=(aaepsr(aanoflyr-1)*aakz(aanoflyr-2)-aaepsr(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aaepsr(aanoflyr-1)*aakz(aanoflyr-2)+aaepsr(aanoflyr-2)*aakz(aanoflyr-1))
end if
!
if(aaslyr.ne.aanoflyr-1) then
  do j=aanoflyr-2,aaslyr+1,-1
    rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
    rtm(j-1,j)=-rtm(j,j-1)
  end do
end if

grtm(0,-1)=0
grtm(-1,0)=0
grtm(aanoflyr,aanoflyr-1)=0
grtm(aanoflyr-1,aanoflyr)=0
grtm(1,0)=rtm(1,0)
do j=1,aaslyr-1
  grtm(j+1,j)=(rtm(j+1,j)+grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grtm(aanoflyr-2,aanoflyr-1)=rtm(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grtm(j-1,j)=(rtm(j-1,j)+grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!         calculate the amplitude factors (TM)
!
amptm=(1.,0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0
  do while (j.le.aaolyr)
    tran=1+rtm(j-1,j)
    sp=tran/(1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aah))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aah))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rtm(j+1,j)
    sm=tran/(1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(Ci*aakz(j+1)*(-aah-zm))*sm/cdexp(Ci*aakz(j)*(-aah-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
return
END subroutine

!
!                              AMPREFTE
!
! subroutine AMPREF calculates the generalized reflection coefficients
! between all adjacent layers,  and the iterative wave amplitude factors
! at the layer where the observation  point is located.
!
!
!
subroutine amprefte(rte,grte,zm,ampte)
!
real*8 zm
Integer(4) j
complex*16 sp,sm,tran,ampte
complex*16 rte(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr)
!


!initializations for the semi-infinite layers
rte(0,-1)=0
rte(-1,0)=0
rte(aanoflyr,aanoflyr-1)=0
rte(aanoflyr-1,aanoflyr)=0

if(peccheck(1).eq.1) then  ! check if the lowermost layer is PEC
  rte(1,0)=-1
elseif(peccheck(1).eq.2) then ! check if the lowermost layer is PMC
  rte(1,0)=1
else
  rte(1,0)=(aamur(0)*aakz(1)-aamur(1)*aakz(0))/(aamur(0)*aakz(1)+aamur(1)*aakz(0))
end if
!
do j=2,aaslyr
  rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
  rte(j-1,j)=-rte(j,j-1)
enddo
!
if(peccheck(2).eq.1) then !the upper most layer is PEC
  rte(aanoflyr-2,aanoflyr-1)=-1
elseif(peccheck(2).eq.2) then !The uppermost layer is PMC
  rte(aanoflyr-2,aanoflyr-1)=1
else
  rte(aanoflyr-2,aanoflyr-1)=(aamur(aanoflyr-1)*aakz(aanoflyr-2)-aamur(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aamur(aanoflyr-1)*aakz(aanoflyr-2)+aamur(aanoflyr-2)*aakz(aanoflyr-1))
end if
!
if(aaslyr.ne.aanoflyr-1) then
  do j=aanoflyr-2,aaslyr+1,-1
    rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
    rte(j-1,j)=-rte(j,j-1)
  end do
end if
!
!      generalized TE reflection coefficients
!
!      below the source layer
!

grte(0,-1)=0
grte(-1,0)=0
grte(aanoflyr,aanoflyr-1)=0
grte(aanoflyr-1,aanoflyr)=0

grte(1,0)=rte(1,0)
do j=1,aaslyr-1
  grte(j+1,j)=(rte(j+1,j)+grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grte(aanoflyr-2,aanoflyr-1)=rte(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grte(j-1,j)=(rte(j-1,j)+grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!         calculate the amplitude factors (TE)
!
ampte=(1.0d0,0.0d0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0.0d0
  do while (j.le.aaolyr)
    tran=1+rte(j-1,j)
    sp=tran
    sp=tran/(1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aah))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aah))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rte(j+1,j)
    sm=tran
    sm=tran/(1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(Ci*aakz(j+1)*(-aah-zm))*sm/cdexp(Ci*aakz(j)*(-aah-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
return
END subroutine


!! THE COMPLEX ORIGIN REFLECTION AND AMPLITUDE TRANSFER FUNCTION CALCULATION !! 

subroutine amprefc(rte,rtm,grte,grtm,zm,ampte,amptm)
! the same as ampref but for complex origin
real*8 zm
Integer(4) j
complex*16 sp,sm,tran,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr), aahc
!
aahc=aah-Ci*aazi
!initializations for the semi-infinite layers
rte(0,-1)=0
rte(-1,0)=0
rtm(0,-1)=0
rtm(-1,0)=0
rte(aanoflyr,aanoflyr-1)=0
rte(aanoflyr-1,aanoflyr)=0
rtm(aanoflyr,aanoflyr-1)=0
rtm(aanoflyr-1,aanoflyr)=0

if(peccheck(1).eq.1) then  ! check if the lowermost layer is PEC
  rte(1,0)=-1
  rtm(1,0)=1
elseif(peccheck(1).eq.2) then ! check if the lowermost layer is PMC
  rte(1,0)=1
  rtm(1,0)=-1
else
  rte(1,0)=(aamur(0)*aakz(1)-aamur(1)*aakz(0))/(aamur(0)*aakz(1)+aamur(1)*aakz(0))
  rtm(1,0)=(aaepsr(0)*aakz(1)-aaepsr(1)*aakz(0))/(aaepsr(0)*aakz(1)+aaepsr(1)*aakz(0))
end if
!
do j=2,aaslyr
  rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
  rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
  rte(j-1,j)=-rte(j,j-1)
  rtm(j-1,j)=-rtm(j,j-1)
enddo
!
if(peccheck(2).eq.1) then !the upper most layer is PEC
  rte(aanoflyr-2,aanoflyr-1)=-1
  rtm(aanoflyr-2,aanoflyr-1)=1
elseif(peccheck(2).eq.2) then !The uppermost layer is PMC
  rte(aanoflyr-2,aanoflyr-1)=1
  rtm(aanoflyr-2,aanoflyr-1)=-1
else
  rte(aanoflyr-2,aanoflyr-1)=(aamur(aanoflyr-1)*aakz(aanoflyr-2)-aamur(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aamur(aanoflyr-1)*aakz(aanoflyr-2)+aamur(aanoflyr-2)*aakz(aanoflyr-1))
  rtm(aanoflyr-2,aanoflyr-1)=(aaepsr(aanoflyr-1)*aakz(aanoflyr-2)-aaepsr(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aaepsr(aanoflyr-1)*aakz(aanoflyr-2)+aaepsr(aanoflyr-2)*aakz(aanoflyr-1))
end if
!
if(aaslyr.ne.aanoflyr-1) then
  do j=aanoflyr-2,aaslyr+1,-1
    rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
    rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
    rte(j-1,j)=-rte(j,j-1)
    rtm(j-1,j)=-rtm(j,j-1)
  end do
end if
!
!      generalized TE reflection coefficients
!
!      below the source layer
!

grte(0,-1)=0
grte(-1,0)=0
grtm(0,-1)=0
grtm(-1,0)=0
grte(aanoflyr,aanoflyr-1)=0
grte(aanoflyr-1,aanoflyr)=0
grtm(aanoflyr,aanoflyr-1)=0
grtm(aanoflyr-1,aanoflyr)=0

grte(1,0)=rte(1,0)
do j=1,aaslyr-1
  grte(j+1,j)=(rte(j+1,j)+grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grte(aanoflyr-2,aanoflyr-1)=rte(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grte(j-1,j)=(rte(j-1,j)+grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!      generalized TM reflection coefficients
!
!      below the source layer
!
grtm(1,0)=rtm(1,0)
do j=1,aaslyr-1
  grtm(j+1,j)=(rtm(j+1,j)+grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grtm(aanoflyr-2,aanoflyr-1)=rtm(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grtm(j-1,j)=(rtm(j-1,j)+grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!         calculate the amplitude factors (TE)
!
ampte=(1.0d0,0.0d0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0.0d0
  do while (j.le.aaolyr)
    tran=1+rte(j-1,j)
    sp=tran
    sp=tran/(1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aahc))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aahc))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rte(j+1,j)
    sm=tran
    sm=tran/(1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(Ci*aakz(j+1)*(-aahc-zm))*sm/cdexp(Ci*aakz(j)*(-aahc-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
!
!         calculate the amplitude factors (TM)
!
amptm=(1.,0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0
  do while (j.le.aaolyr)
    tran=1+rtm(j-1,j)
    sp=tran/(1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aahc))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aahc))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rtm(j+1,j)
    sm=tran/(1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(Ci*aakz(j+1)*(-aahc-zm))*sm/cdexp(Ci*aakz(j)*(-aahc-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
return
END subroutine

subroutine ampreftmc(rtm,grtm,zm,amptm)
!
real*8 zm
Integer(4) j
complex*16 sp,sm,tran,amptm
complex*16 rtm(-1:mxlyr,-1:mxlyr)
complex*16 grtm(-1:mxlyr,-1:mxlyr), aahc
!

aahc=aah-Ci*aazi
!initializations for the semi-infinite layers
rtm(0,-1)=0
rtm(-1,0)=0
rtm(aanoflyr,aanoflyr-1)=0
rtm(aanoflyr-1,aanoflyr)=0

if(peccheck(1).eq.1) then  ! check if the lowermost layer is PEC
  rtm(1,0)=1
elseif(peccheck(1).eq.2) then ! check if the lowermost layer is PMC
  rtm(1,0)=-1
else
  rtm(1,0)=(aaepsr(0)*aakz(1)-aaepsr(1)*aakz(0))/(aaepsr(0)*aakz(1)+aaepsr(1)*aakz(0))
end if
!
do j=2,aaslyr
  rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
  rtm(j-1,j)=-rtm(j,j-1)
enddo
!
if(peccheck(2).eq.1) then !the upper most layer is PEC
  rtm(aanoflyr-2,aanoflyr-1)=1
elseif(peccheck(2).eq.2) then !The uppermost layer is PMC
  rtm(aanoflyr-2,aanoflyr-1)=-1
else
  rtm(aanoflyr-2,aanoflyr-1)=(aaepsr(aanoflyr-1)*aakz(aanoflyr-2)-aaepsr(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aaepsr(aanoflyr-1)*aakz(aanoflyr-2)+aaepsr(aanoflyr-2)*aakz(aanoflyr-1))
end if
!
if(aaslyr.ne.aanoflyr-1) then
  do j=aanoflyr-2,aaslyr+1,-1
    rtm(j,j-1)=(aaepsr(j-1)*aakz(j)-aaepsr(j)*aakz(j-1))/(aaepsr(j-1)*aakz(j)+aaepsr(j)*aakz(j-1))
    rtm(j-1,j)=-rtm(j,j-1)
  end do
end if

grtm(0,-1)=0
grtm(-1,0)=0
grtm(aanoflyr,aanoflyr-1)=0
grtm(aanoflyr-1,aanoflyr)=0
grtm(1,0)=rtm(1,0)
do j=1,aaslyr-1
  grtm(j+1,j)=(rtm(j+1,j)+grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grtm(aanoflyr-2,aanoflyr-1)=rtm(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grtm(j-1,j)=(rtm(j-1,j)+grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!         calculate the amplitude factors (TM)
!
amptm=(1.,0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0
  do while (j.le.aaolyr)
    tran=1+rtm(j-1,j)
    sp=tran/(1-rtm(j,j-1)*grtm(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aahc))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aahc))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rtm(j+1,j)
    sm=tran/(1-rtm(j,j+1)*grtm(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    amptm=amptm*cdexp(Ci*aakz(j+1)*(-aahc-zm))*sm/cdexp(Ci*aakz(j)*(-aahc-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
return
END subroutine




subroutine ampreftec(rte,grte,zm,ampte)
! the same as ampferte but with a complex origin
real*8 zm
Integer(4) j
complex*16 sp,sm,tran,ampte
complex*16 rte(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),aahc
!
!initializations for the semi-infinite layers
aahc=aah-Ci*aazi
rte(0,-1)=0
rte(-1,0)=0
rte(aanoflyr,aanoflyr-1)=0
rte(aanoflyr-1,aanoflyr)=0

if(peccheck(1).eq.1) then  ! check if the lowermost layer is PEC
  rte(1,0)=-1
elseif(peccheck(1).eq.2) then ! check if the lowermost layer is PMC
  rte(1,0)=1
else
  rte(1,0)=(aamur(0)*aakz(1)-aamur(1)*aakz(0))/(aamur(0)*aakz(1)+aamur(1)*aakz(0))
end if
!
do j=2,aaslyr
  rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
  rte(j-1,j)=-rte(j,j-1)
enddo
!
if(peccheck(2).eq.1) then !the upper most layer is PEC
  rte(aanoflyr-2,aanoflyr-1)=-1
elseif(peccheck(2).eq.2) then !The uppermost layer is PMC
  rte(aanoflyr-2,aanoflyr-1)=1
else
  rte(aanoflyr-2,aanoflyr-1)=(aamur(aanoflyr-1)*aakz(aanoflyr-2)-aamur(aanoflyr-2)*aakz(aanoflyr-1))/ &
  & (aamur(aanoflyr-1)*aakz(aanoflyr-2)+aamur(aanoflyr-2)*aakz(aanoflyr-1))
end if
!
if(aaslyr.ne.aanoflyr-1) then
  do j=aanoflyr-2,aaslyr+1,-1
    rte(j,j-1)=(aamur(j-1)*aakz(j)-aamur(j)*aakz(j-1))/(aamur(j-1)*aakz(j)+aamur(j)*aakz(j-1))
    rte(j-1,j)=-rte(j,j-1)
  end do
end if
!
!      generalized TE reflection coefficients
!
!      below the source layer
!

grte(0,-1)=0
grte(-1,0)=0
grte(aanoflyr,aanoflyr-1)=0
grte(aanoflyr-1,aanoflyr)=0

grte(1,0)=rte(1,0)
do j=1,aaslyr-1
  grte(j+1,j)=(rte(j+1,j)+grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
enddo
!
!      above the source layer
!
grte(aanoflyr-2,aanoflyr-1)=rte(aanoflyr-2,aanoflyr-1)
j=aanoflyr-2
do while (j.ge.aaslyr)
  grte(j-1,j)=(rte(j-1,j)+grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))/ &
  & (1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
  j=j-1
enddo
!
!         calculate the amplitude factors (TE)
!
ampte=(1.0d0,0.0d0)
if(aaolyr.gt.aaslyr) then
  j=aaslyr+1
  zm=0.0d0
  do while (j.le.aaolyr)
    tran=1+rte(j-1,j)
    sp=tran
    sp=tran/(1-rte(j,j-1)*grte(j,j+1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(-Ci*aakz(j-1)*(zm+aadd(aaslyr)-aahc))*sp/cdexp(-Ci*aakz(j)*(zm+aadd(aaslyr)-aahc))
    zm=zm+aadd(j)
    j=j+1
  enddo
else
  j=aaslyr-1
  zm=0
  do while (j.ge.aaolyr)
    tran=1+rte(j+1,j)
    sm=tran
    sm=tran/(1-rte(j,j+1)*grte(j,j-1)*cdexp(-Ci*aakz(j)*2*aadd(j)))
    ampte=ampte*cdexp(Ci*aakz(j+1)*(-aahc-zm))*sm/cdexp(Ci*aakz(j)*(-aahc-zm))
    zm=zm+aadd(j)
    j=j-1
  enddo
end if
return
END subroutine



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         SURFACE WAVE POLES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine swpoles(xtenew,xtmnew)
Integer(4), parameter :: nc=20
Integer(4), parameter :: nofswp=10
Integer(4) temp_slyr,temp_olyr,i
real*8 krho_step
real*8 temp_h,temp_z,kki_max
complex*16 temp_epsr(0:mxlyr),temp_mur(0:mxlyr),xtenew(nofswp),xtmnew(nofswp),aakk_dum(0:mxlyr)
complex*16 fxtenew(nofswp),fxtmnew(nofswp),x1,x2,x3

!external cfunte,cfuntm,cfuntemul,cfuntmmul
!
! Since the locations of the SWPs are not dependent on where the source and observation
!		are, we set the source and observation to the interface between the top layer and 
!		the layer below it. So, aaslyr=aanoflyr-1, aaolyr=aanoflyr-1, aah=aaz=0. To do that, we need to
!    	store the actual values into sime dummy variables and then perform the search for 
!		the SWPs. Once the SWPs are obtained, then the actual values will be re-stored.
!
temp_slyr=aaslyr
temp_olyr=aaolyr
temp_h=aah
temp_z=aaz
aaslyr=aanoflyr-1
aaolyr=aanoflyr-1
aah=0.0d0
aaz=0.0d0
!
! Find the maximum imaginary part of k values
!
kki_max=0.0
do i=0,aanoflyr-1
  if(dabs(dimag(aakk(i))).gt.kki_max) kki_max=dabs(dimag(aakk(i)))
enddo
kki_max=-kki_max
!
! Set all permittivities as real numbers with imaginary parts zero 
!
do i=0,aanoflyr-1
  temp_epsr(i)=aaepsr(i)
  temp_mur(i)=aamur(i)
  aaepsr(i)=dcmplx(dreal(aaepsr(i)),-0.0d0)
  aamur(i)=dcmplx(dreal(aamur(i)),-0.0d0)
enddo
! Then, update k values for these new permittivities
aakk_dum=aakk

do i=0,aanoflyr-1 !wavenumbers of each layer
  if (i.eq.0) then
    if (peccheck(1).eq.0) then
    aakk(i)=k00*sqrt(aaepsr(i))*sqrt(aamur(i)) ! aakk(i):wavenumber of ith layer
    else
    aakk(i)=0.0
    endif
  elseif (i.eq.aanoflyr-1) then
    if (peccheck(2).eq.0) then
    aakk(i)=k00*sqrt(aaepsr(i))*sqrt(aamur(i)) ! aakk(i):wavenumber of ith layer
    else
    aakk(i)=0.0
    endif
  else
    aakk(i)=k00*sqrt(aaepsr(i))*sqrt(aamur(i)) ! aakk(i):wavenumber of ith layer
  endif
enddo
!
! Get coarse estimates for SWPs. In this subroutine, the dielectric constants 
!		are assumed to be lossless, since SWPs are usually close to the real axis
!		of k_rho-plane due to small losses in practical materials
!
call guess_sw(aante,aantm,aaxte,aaxtm)
if(l4.and.l5) write(*,*)'aante=',aante,' aantm=',aantm  ! Print the estimates on the console
!
! Now, reload the actual values of permittivities and the wave numbers
!
do i=0,aanoflyr-1
  aaepsr(i)=temp_epsr(i)
  aamur(i)=temp_mur(i)
enddo
aakk=aakk_dum(0:mxlyr)
!
! Now, refine the locations of the SWPs by Mueller's Method
!
krho_step=(aakmax-k00-2.0d-7)/1000
do i=1,aante
  x1=aaxte(i)-krho_step
  x2=aaxte(i)
  x3=aaxte(i)+krho_step
  call c8_muller ( cfuntemul, 1.0d-10, 1000, x1, x2, x3, 1.0d-10, 1.0d-10, &
  xtenew(i), fxtenew(i) )
  if(l4.and.l5) write(*,101)i,dreal(xtenew(i)/k00),dimag(xtenew(i)/k00)
  101 format('Number of (normalized) TE-sw pole=',i2,'  Re[krho(TE)]=',f10.5,' Im[krho(TE)]=',f10.5)
  if(l4.and.l5) write(*,*)'1/f(krho(TE))=',fxtenew(i)
enddo

do i=1,aantm
  x1=aaxtm(i)-krho_step
  x2=aaxtm(i)
  x3=aaxtm(i)+krho_step
  call c8_muller ( cfuntmmul, 1.0d-10, 100, x1, x2, x3, 1.0d-10, 1.0d-10, &
  xtmnew(i), fxtmnew(i) )
  if(l4.and.l5) write(*,102)i,dreal(xtmnew(i)/k00),dimag(xtmnew(i)/k00)
  102 format('Number of (normalized) TM-sw pole=',i2,'  Re[krho(TM)]=',f10.5,' Im[krho(TM)]=',f10.5)
  if(l4.and.l5) write(*,*)'1/f(krho(TM))=',fxtmnew(i)
enddo

! Re-store the actual values of the source and observation locations.
!
aaslyr=temp_slyr
aaolyr=temp_olyr
aah=temp_h
aaz=temp_z
return
end subroutine

Subroutine getaakz(krho)
Implicit none
Integer(4) i
complex*16 krho
do i=0,aanoflyr-1
if (i.eq.0) then
    if (peccheck(1).eq.0) then
    aakz(i)=cdsqrt(aakk(i)**2-krho**2)
    else
    aakz(i)=0.0
    endif
  elseif (i.eq.aanoflyr-1) then
    if (peccheck(2).eq.0) then
    aakz(i)=cdsqrt(aakk(i)**2-krho**2)
    else
    aakz(i)=0.0
    endif
  else
    aakz(i)=cdsqrt(aakk(i)**2-krho**2)
endif
if(dimag(aakz(i)).gt.0.0) aakz(i)=-aakz(i)
enddo
end Subroutine getaakz

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
complex*16 function cfunte(krho)
real*8 zm
complex*16 krho
complex*16 ai,aaci,mite,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr) 
!

!
call getaakz(krho)
!
call ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!     
mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))  
aaci=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
  & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
ai=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah)) &
  & +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mite
!
cfunte=1.0d0/aaci
return
end function

subroutine cfuntemul(krho,ress)
real*8 zm
complex*16 krho
complex*16 aaci,mite,ampte,amptm,ress
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr) 
!

!
call getaakz(krho)
!
call ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!     
mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))  
aaci=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
  & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
!!ai=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah)) &
!!  & +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mite
!
ress=1.0d0/aaci
return
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
complex*16 function cfuntm(krho)
real*8 zm
complex*16 krho
complex*16 bi,di,mitm,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr) 
!

!
call getaakz(krho)
!
call ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!     
mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
bi=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))-grtm(aaslyr,aaslyr-1) &
  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mitm
di=cdexp(-Ci*aakz(aaslyr)*aah)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1) &
  & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mitm
!
cfuntm=1.0d0/di
return
end function

subroutine cfuntmmul(krho,ress)
real*8 zm
complex*16 krho
complex*16 di,mitm,ampte,amptm,ress
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
!

!
call getaakz(krho)
!
call ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!     
!mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))  
mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
!bi=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))-grtm(aaslyr,aaslyr-1) &
!  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mitm
di=cdexp(-Ci*aakz(aaslyr)*aah)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1) &
  & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mitm
!
ress=1.0d0/di
return
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine guess_sw(nte_guess,ntm_guess,krte_guess,krtm_guess)
Integer(4), parameter :: nofswp=10
Integer(4) nte_guess, ntm_guess,niter
real*8 zm,krho,krho_step
real*8 pkrho,dpcfte,dpcftm,eps
complex*16 krte_guess(nofswp),krtm_guess(nofswp)
complex*16 cfte,cftm,pcfte,pcftm
complex*16 aaci,di,mite,mitm,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr) 
!
!
eps=1.0d-40
nte_guess=0
ntm_guess=0
!epsr_max=aaepsr(0)
!do i=1,aanoflyr-1
!  if(dreal(aaepsr(i)).gt.dreal(epsr_max)) epsr_max=aaepsr(i)
!enddo
!aakmax=k00*dreal(cdsqrt(epsr_max))
krho_step=(aakmax-k00-2.0d-7)/1000
do niter=0,1000
  krho=k00+1.0d-7+niter*krho_step
call getaakz(dCmplx(krho,0.0d0))
!
  call ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!     
  mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
  di=cdexp(-Ci*aakz(aaslyr)*aah)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mitm
!
  cftm=1.0d0/di
  if(niter.eq.0) then
    pkrho=krho
    pcftm=cftm
    dpcftm=0.0d0
  else
    if(dpcftm.le.0.0d0) then
      if(((dabs(dreal(cftm))/dabs(dimag(cftm)+eps)).lt.1.0d7).and.(dabs(dreal(cftm))/dabs(dimag(cftm)+eps)).gt.1.0d-7) then
        if(dabs(dabs(dreal(cftm-pcftm))-(dabs(dreal(cftm))+dabs(dreal(pcftm)))).lt.1d-14) then
          if(dabs(dabs(dimag(cftm-pcftm))-(dabs(dimag(cftm))+dabs(dimag(pcftm)))).lt.1d-14) then
            ntm_guess=ntm_guess+1
            krtm_guess(ntm_guess)=dcmplx(krho,0.0d0)
          endif
        endif
      else
        if(dabs(dimag(cftm)).lt.dabs(dreal(cftm))/1.0d7) then
          if(dabs(dabs(dreal(cftm-pcftm))-(dabs(dreal(cftm))+dabs(dreal(pcftm)))).lt.1.0d-10) then
            ntm_guess=ntm_guess+1
            krtm_guess(ntm_guess)=dcmplx(krho,0.0d0)
          endif
        else
          if(dabs(dabs(dimag(cftm-pcftm))-(dabs(dimag(cftm))+dabs(dimag(pcftm)))).lt.1d-14) then
            ntm_guess=ntm_guess+1
            krtm_guess(ntm_guess)=dcmplx(krho,0.0d0)
          endif
        endif
      endif
    endif
    dpcftm=(cdabs(cftm)-cdabs(pcftm))/(krho-pkrho)
    pkrho=krho
    pcftm=cftm
  endif
enddo
!
do niter=0,1000
  krho=k00+1.0d-7+niter*krho_step
call getaakz(dCmplx(krho,0.0d0))
!
  call ampref(rte,rtm,grte,grtm,zm,ampte,amptm)
!     
  mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))  
  aaci=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
!
  cfte=1.0d0/aaci
  if(niter.eq.0) then
    pkrho=krho
    pcfte=cfte
    dpcfte=0.0d0
  else
    if(dpcfte.le.0.0d0) then
      if(((dabs(dreal(cfte))/(dabs(dimag(cfte))+eps)).lt.1.0d7).and.(dabs(dreal(cfte))/(dabs(dimag(cfte))+eps)).gt.1.0d-7) then
        if(dabs(dabs(dreal(cfte-pcfte))-(dabs(dreal(cfte))+dabs(dreal(pcfte)))).lt.1d-14) then
          if(dabs(dabs(dimag(cfte-pcfte))-(dabs(dimag(cfte))+dabs(dimag(pcfte)))).lt.1d-14) then
            nte_guess=nte_guess+1
            krte_guess(nte_guess)=dcmplx(krho,0.0d0)
          endif
        endif
      else
        if(dabs(dimag(cfte)).lt.dabs(dreal(cfte))/1.0d7) then
          if(dabs(dabs(dreal(cfte-pcfte))-(dabs(dreal(cfte))+dabs(dreal(pcfte)))).lt.1.0d-14) then
            nte_guess=nte_guess+1
            krte_guess(nte_guess)=dcmplx(krho,0.0d0)
          endif
        else
          if(dabs(dabs(dimag(cfte-pcfte))-(dabs(dimag(cfte))+dabs(dimag(pcfte)))).lt.1d-14) then
            nte_guess=nte_guess+1
            krte_guess(nte_guess)=dcmplx(krho,0.0d0)
          endif
        endif
      endif
    endif
    dpcfte=(cdabs(cfte)-cdabs(pcfte))/(krho-pkrho)
    pkrho=krho
    pcfte=cfte
  endif
enddo
!
return
end subroutine

subroutine c8_muller ( func, fatol, itmax, x1, x2, x3, xatol, xrtol, &
  xnew, fxnew )

!*****************************************************************************80
!
!! C8_MULLER carries out Muller's method, using C8 arithmetic.
!
!  Discussion:
!
!    "C8" arithmetic is complex double precision arithmetic.
!
!  Modified:
!
!    19 July 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Gisela Engeln-Muellges, Frank Uhlig,
!    Numerical Algorithms with C,
!    Springer, 1996,
!    ISBN: 3-540-60530-4,
!    LC: QA297.E56213.
!
!  Parameters:
!
!    Input, external FUNC, the name of the routine that evaluates the function.
!    FUNC should have the form:
!      subroutine func ( x, fx )
!      complex fx
!      complex x
!
!    Input, real ( kind = 8 ) FATOL, the absolute error tolerance for F(X).
!
!    Input, integer ITMAX, the maximum number of steps allowed.
!
!    Input, complex*16 X1, X2, X3, three distinct points to start the
!    iteration.
!
!    Input, real ( kind = 8 ) XATOL, XRTOL, absolute and relative
!    error tolerances for the root.
!
!    Output, complex*16 XNEW, the estimated root.
!
!    Output, complex*16 FXNEW, the value of the function at XNEW.
!
  implicit none

  complex*16 a
  complex*16 b
  complex*16 c
  complex*16 c8_temp
  complex*16 discrm
  real*8 fatol
  complex*16 fminus
  complex*16 fplus
  external func
  complex*16 fxmid
  complex*16 fxnew
  complex*16 fxold
  Integer(4) iterate
  Integer(4) itmax
  real*8 x_ave
  complex*16 x_inc
  complex*16 x1
  complex*16 x2
  complex*16 x3
  real*8 xatol
  complex*16 xlast
  complex*16 xmid
  complex*16 xminus
  complex*16 xnew
  complex*16 xold
  complex*16 xplus
  real*8 xrtol

  xnew = x1
  xmid = x2
  xold = x3

  call func ( xnew, fxnew )
  call func ( xmid, fxmid )
  call func ( xold, fxold )

!  write ( *, '(a)' ) ' '
!  write ( *, '(a)' ) ' '
!  write ( *, '(a)' ) 'C4_MULLER:'
!  write ( *, '(a)' ) '  Muller''s method (complex root version)'
!  write ( *, '(a)' ) ' '
!  write ( *, '(a)' ) ' '
!  write ( *, '(a)' ) &
!    '  Iteration     x_real              x_imag             ||fx||           ||disc||'
!  write ( *, '(a)' ) ' '

  iterate = -2
!  write ( *, '(i6,f20.10,f20.10,f20.10)' ) iterate, xold, abs ( fxold )
  iterate = -1
!  write ( *, '(i6,f20.10,f20.10,f20.10)' ) iterate, xmid, abs ( fxmid )
  iterate = 0
!  write ( *, '(i6,f20.10,f20.10,f20.10)' ) iterate, xnew, abs ( fxnew )

  if ( abs ( fxnew ) < fatol ) then
!    write ( *, '(a)' ) ' '
!    write ( *, '(a)' ) 'C4_MULLER:'
!    write ( *, '(a)' ) '  |F(X)| is below the tolerance.'
    return
  end if

  do
!
!  You may need to swap (XMID,FXMID) and (XNEW,FXNEW).
!
    if ( abs ( fxmid ) <= abs ( fxnew ) ) then

      c8_temp = xnew
      xnew = xmid
      xmid = c8_temp

      c8_temp = fxnew
      fxnew = fxmid
      fxmid = c8_temp

    end if

    xlast = xnew
    iterate = iterate + 1

    if ( itmax < iterate ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  Maximum number of steps taken.'
      exit
    end if

    a =  ( ( xmid - xnew ) * ( fxold - fxnew ) &
         - ( xold - xnew ) * ( fxmid - fxnew ) )

    b = ( ( xold - xnew )**2 * ( fxmid - fxnew ) &
        - ( xmid - xnew )**2 * ( fxold - fxnew ) )

    c = ( ( xold - xnew ) * ( xmid - xnew ) * ( xold - xmid ) * fxnew )

    xold = xmid
    xmid = xnew
!
!  Apply the quadratic formula to get roots XPLUS and XMINUS.
!
    discrm = b**2 - 4.0D+00 * a * c

    if ( a == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_MULLER:'
      write ( *, '(a)' ) '  The algorithm has broken down.'
      write ( *, '(a)' ) '  The quadratic coefficient A is zero.'
      exit
    end if

    xplus = xnew + ( ( - b + sqrt ( discrm ) ) / ( 2.0D+00 * a ) )

    call func ( xplus, fplus )

    xminus = xnew + ( ( - b - sqrt ( discrm ) ) / ( 2.0D+00 * a ) )

    call func ( xminus, fminus )
!
!  Choose the root with smallest function value.
!
    if ( abs ( fminus ) < abs ( fplus ) ) then
      xnew = xminus
    else
      xnew = xplus
    end if

    fxold = fxmid
    fxmid = fxnew
    call func ( xnew, fxnew )
!    write ( *, '(i6,f20.10,f20.10,f20.10,f20.10)' ) &
!      iterate, xnew, abs ( fxnew ), abs ( discrm )
!
!  Check for convergence.
!
    x_ave = abs ( xnew + xmid + xold ) / 3.0D+00
    x_inc = xnew - xmid

    if ( abs ( x_inc ) <= xatol ) then
!      write ( *, '(a)' ) ' '
!      write ( *, '(a)' ) 'C8_MULLER:'
!      write ( *, '(a)' ) '  Absolute convergence of the X increment.'
      exit
    end if

    if ( abs ( x_inc ) <= xrtol * x_ave ) then
!      write ( *, '(a)' ) ' '
!      write ( *, '(a)' ) 'C8_MULLER:'
!      write ( *, '(a)' ) '  Relative convergence of the X increment.'
      exit
    end if

    if ( abs ( fxnew ) <= fatol ) then
!      write ( *, '(a)' ) ' '
!      write ( *, '(a)' ) 'C8_MULLER:'
!      write ( *, '(a)' ) '  Absolute convergence of |F(X)|.'
      exit
    end if

  end do

  return
endsubroutine

real*8 function aaintegrands3dhor(krhor,outint)
real*8 outint(22)
complex*16 aaintdata(11)
real*8 zm,j1,j0,krhor,d
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
complex*16 y1g2,y2g2,c1ug2,c2ug2,c1dg2,c2dg2,y1g5,y2g5,y1g4,y2g4,c1ug5,c2ug5,c1dg5,c2dg5
complex*16 mitm,bihor,dihor,ampsptehor,ampsmtehor,ampsptmhor,ampsmtmhor
complex*16 CBJ0,CBJ1,cBes(0:20),crkap
complex*16 divider, upprop, downprop
Integer(4) idumy1,idumy2,idumy3,iErr

if(krhor.eq.0.0d0) krho=1.0d-6
if (krhor.lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
  idumy1=2
  idumy2=25
  idumy3=14
  crkap=krho*aarho
  d=cdabs(crkap)
  if(d.gt.15000.0d0) then
    d=15000.0d0/d
    crkap=crkap*d
  end if
  call cj(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
  if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',crkap,')'
    lDispWarn=.false.
  end if
  CBJ0=cBes(0)
  CBJ1=cBes(1)
else
  krho=krhor
  j1=bsj1(dabs(krhor*aarho)) !real bessel functions (since krho is on the real axis)
  j0=bsj0(dabs(krhor*aarho))
  CBJ0=dcmplx(j0,0.0)
  CBJ1=dcmplx(j1,0.0)
endif

call getaakz(krho)

call ampref(rte,rtm,grte,grtm,zm,ampte,amptm) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krho2=krho**2
if(aaslyr.eq.aaolyr) then
  upprop=cdexp(-Ci*aakz(aaslyr)*aaz)
  downprop=cdexp(Ci*aakz(aaslyr)*aaz)
  
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mite
  bihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))-grtm(aaslyr,aaslyr-1) &
    & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mitm
  cihor=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
  dihor=cdexp(-Ci*aakz(aaslyr)*aah)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mitm

  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(bihor)*dimag(bihor))) bihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
  if (isnan(dreal(dihor)*dimag(dihor))) dihor=0.0
!horizontal vector potential (parallel) related
aaintdata(1)=(aihor*downprop+cihor*upprop)*divider
aaintdata(7)=(Ci*aakz(aaslyr)*aihor*downprop-Ci*aakz(aaslyr)*cihor*upprop)*divider
aaintdata(9)=aaintdata(1)
!scalar potential related
y1g2=(aakz(aaslyr)**2*bihor+aakk(aaslyr)**2*aihor)*downprop/krho2
y2g2=(aakk(aaslyr)**2*cihor-aakz(aaslyr)**2*dihor)*upprop/krho2
aaintdata(2)=(y1g2+y2g2)*divider
aaintdata(3)=aaintdata(2)
aaintdata(4)=(Ci*aakz(aaslyr)*y1g2-Ci*aakz(aaslyr)*y2g2)/(Ci*aakz(aaslyr))
aaintdata(10)=aaintdata(2)
!horizontal vector potential (normal) related
y1g5=(aihor+bihor)*aakz(aaslyr)*downprop/(krho2)
y2g5=(dihor-cihor)*aakz(aaslyr)*upprop/(krho2)
aaintdata(5)=-(y1g5+y2g5)*Ci*divider
aaintdata(6)=aaintdata(5)
aaintdata(8)=aaintdata(5)
aaintdata(11)=aaintdata(5)
else
  if (aaolyr.gt.aaslyr) then
  upprop=cdexp(-Ci*aakz(aaolyr)*aaz)
  !horizontal vector potential (parallel) related
    ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(-Ci*aakz(aaolyr)*upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*Ci*aakz(aaolyr)*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
    aaintdata(9)=aaintdata(1)
  !scalar potential related
    ampsptehor=ampspg1
    ampsptmhor=(1-grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mitm
    c1ug2=ampsptmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte
    c2ug2=-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*aakz(aaslyr)*aakz(aaolyr)- &
      & aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))
    y2g2=-c2ug2*cdexp(Ci*aakz(aaolyr)*aaz)/krho2
    y1g2=-c1ug2*upprop/krho2
    aaintdata(2)=(y1g2+y2g2)*divider
    aaintdata(3)=aaintdata(2)
    y2g4=Ci*aakz(aaolyr)*y2g2
    y1g4=-Ci*aakz(aaolyr)*y1g2
    aaintdata(4)=(y1g4+y2g4)*divider
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    c1ug5=(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsptmhor*amptm
    c2ug5=-(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+ &
      & aadd(aaslyr)-aah))/aakz(aaslyr)-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))
    y2g5=-c2ug5*aakz(aaslyr)*cdexp(Ci*aakz(aaolyr)*aaz)/krho2
    y1g5=-c1ug5*aakz(aaslyr)*upprop/krho2
    aaintdata(5)=-(y1g5+y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
    aaintdata(11)=aaintdata(5)
  else
    !horizontal vector potential (parallel) related
    downprop=cdexp(Ci*aakz(aaolyr)*aaz)
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(downprop+grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(Ci*aakz(aaolyr)*downprop-grte(aaolyr,aaolyr-1)*Ci*aakz(aaolyr)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
    aaintdata(9)=aaintdata(1)
    !scalar potential related
    ampsmtehor=ampsmg1
    ampsmtmhor=(-1+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mitm
    c2dg2=-ampsmtmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte
    c1dg2=ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah))*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2 &
      & *(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah))
    y2g2=-c2dg2*downprop/krho2     
    y1g2=-c1dg2*cdexp(-Ci*aakz(aaolyr)*aaz)/krho2
    aaintdata(2)=(y1g2+y2g2)*divider
    aaintdata(3)=aaintdata(2)
    y2g4=Ci*aakz(aaolyr)*y2g2
    y1g4=-Ci*aakz(aaolyr)*y1g2
    aaintdata(4)=(y1g4+y2g4)*divider
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    c2dg5=-(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsmtmhor*amptm
    c1dg5=(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah)) &
      & /aakz(aaslyr)-ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aah+zm))
    y2g5=-c2dg5*aakz(aaslyr)*downprop/krho2
    y1g5=-c1dg5*aakz(aaslyr)*cdexp(-Ci*aakz(aaolyr)*aaz)/krho2
    aaintdata(5)=-(y1g5+y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
    aaintdata(11)=aaintdata(5)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0
if (isnan(dreal(aaintdata(5))*dimag(aaintdata(5)))) aaintdata(5)=0.0
if (isnan(dreal(aaintdata(6))*dimag(aaintdata(6)))) aaintdata(6)=0.0
if (isnan(dreal(aaintdata(7))*dimag(aaintdata(7)))) aaintdata(7)=0.0
if (isnan(dreal(aaintdata(8))*dimag(aaintdata(8)))) aaintdata(8)=0.0
if (isnan(dreal(aaintdata(9))*dimag(aaintdata(9)))) aaintdata(9)=0.0
if (isnan(dreal(aaintdata(10))*dimag(aaintdata(10)))) aaintdata(10)=0.0
if (isnan(dreal(aaintdata(11))*dimag(aaintdata(11)))) aaintdata(11)=0.0

outint(1)=dreal(aaintdata(1)*CBJ0*krho)
outint(11)=dimag(aaintdata(1)*CBJ0*krho)
outint(7)=dreal(aaintdata(7)*CBJ0*krho)
outint(17)=dimag(aaintdata(7)*CBJ0*krho)
outint(9)=dreal(-aaintdata(9)*krho*((krho*aay*CBJ1)/aarho))
outint(19)=dimag(-aaintdata(9)*krho*((krho*aay*CBJ1)/aarho))
outint(2)=dreal(aaintdata(2)*krho*((krho*aax*((aax*CBJ1)/(aarho**2) - (krho*aax*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aax**2)/(aarho**3)))
outint(12)=dimag(aaintdata(2)*krho*((krho*aax*((aax*CBJ1)/(aarho**2) - (krho*aax*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aax**2)/(aarho**3)))
outint(3)=dreal(aaintdata(3)*krho*((krho*aax*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho + (krho*aax*aay*CBJ1)/(aarho**3)))
outint(13)=dimag(aaintdata(3)*krho*((krho*aax*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho + (krho*aax*aay*CBJ1)/(aarho**3)))
outint(4)=dreal(-aaintdata(4)*krho*((krho*aax*CBJ1)/aarho))
outint(14)=dimag(-aaintdata(4)*krho*((krho*aax*CBJ1)/aarho))
outint(10)=dreal(aaintdata(10)*krho*((krho*aay*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aay**2)/(aarho**3)))
outint(20)=dimag(aaintdata(10)*krho*((krho*aay*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aay**2)/(aarho**3)))
outint(5)=dreal(-aaintdata(5)*krho*((krho*aax*CBJ1)/aarho))
outint(15)=dimag(-aaintdata(5)*krho*((krho*aax*CBJ1)/aarho))
outint(6)=dreal(aaintdata(6)*krho*((krho*aax*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho + (krho*aax*aay*CBJ1)/(aarho**3)))
outint(16)=dimag(aaintdata(6)*krho*((krho*aax*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho + (krho*aax*aay*CBJ1)/(aarho**3)))
outint(8)=dreal(aaintdata(8)*krho*((krho*aax*((aax*CBJ1)/(aarho**2) - (krho*aax*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aax**2)/(aarho**3)))
outint(18)=dimag(aaintdata(8)*krho*((krho*aax*((aax*CBJ1)/(aarho**2) - (krho*aax*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aax**2)/(aarho**3)))
outint(21)=dreal(aaintdata(11)*krho*((krho*aay*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aay**2)/(aarho**3)))
outint(22)=dimag(aaintdata(11)*krho*((krho*aay*((aay*CBJ1)/(aarho**2) - (krho*aay*CBJ0)/aarho))/aarho - (krho*CBJ1)/aarho + (CBJ1*krho*aay**2)/(aarho**3)))
aaintegrands3dhor=.srt.(cdabs(aaintdata(1))**2+cdabs(aaintdata(2))**2+cdabs(krho*aaintdata(5))**2)
end function



real*8 function aaintegrands3dvert(krhor,outint)
real*8 outint(8)
complex*16 aaintdata(4)
real*8 zm,j1,j0,krhor,d
complex*16 krho,krho2
complex*16 amptm
complex*16 rtm(-1:mxlyr,-1:mxlyr)
complex*16 grtm(-1:mxlyr,-1:mxlyr)
complex*16 mitm, aaa1u, aaa2u, aac1u, aac2u, aaa1d, aaa2d, aac1d, aac2d
complex*16 CBJ0,CBJ1,cBes(0:20),crkap
complex*16 apg10,amg10,apg12,amg12
complex*16 divider, upprop, downprop
Integer(4) idumy1,idumy2,idumy3,iErr

if(krhor.eq.0.0d0) krho=1.0d-6
if (krhor.lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
  idumy1=2
  idumy2=20
  idumy3=14
  crkap=krho*aarho
  d=cdabs(crkap)
  if(d.gt.15000.0d0) then
    d=15000.0d0/d
    crkap=crkap*d
  end if
  call cj(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
  if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',crkap,')'
    lDispWarn=.false.
  end if
  CBJ0=cBes(0)
  CBJ1=cBes(1)
else
  krho=krhor
  j1=bsj1(dabs(krhor*aarho)) !real bessel functions (since krho is on the real axis)
  j0=bsj0(dabs(krhor*aarho))
  CBJ0=dcmplx(j0,0.0)
  CBJ1=dcmplx(j1,0.0)
endif

call getaakz(krho)

call ampreftm(rtm,grtm,zm,amptm) ! amplitude and reflection coefficients

mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))

krho2=krho**2
divider=1.0/(Ci*aakz(aaslyr))

if(aaslyr.eq.aaolyr) then
!vertical scalar potential
upprop=cdexp(-Ci*aakz(aaslyr)*aaz)
downprop=cdexp(Ci*aakz(aaslyr)*aaz)
apg10=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aah)*(-cdexp(-Ci*aakz(aaslyr)*aah)-grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))
amg10=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*(-cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))-grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))
aaintdata(1)=(Ci*aakz(aaslyr)*apg10*upprop-amg10*Ci*aakz(aaslyr)*downprop)*divider
aaintdata(3)=((aakz(aaslyr)**2)*apg10*upprop+amg10*(aakz(aaslyr)**2)*downprop)*divider
!vertical vector potential
apg12=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aah)*(cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))
amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))+grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))
aaintdata(2)=(apg12*upprop+amg12*downprop)*divider
aaintdata(4)=aaintdata(2)
else
  if (aaolyr.gt.aaslyr) then
    !vertical scalar potential related 
    upprop=cdexp(-Ci*aakz(aaolyr)*aaz)
    aac1u=1.0d0+cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr))*grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*mitm
    aac2u=amptm*cdexp(Ci*(aakz(aaolyr)-aakz(aaslyr))*aah)
    aaa1u=aac1u*aac2u*aakz(aaolyr)*cdexp(Ci*aakz(aaslyr)*aah)
    aaa2u=aac2u*aakz(aaolyr)*grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aah)
    aaintdata(1)=-Ci*(aaa1u+aaa2u)*cdexp(-Ci*aakz(aaolyr)*aah)*(upprop-grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*(-aaz-2*aah+2*zm+2*aadd(aaslyr))))*divider
    aaintdata(3)=-Ci*(aaa1u+aaa2u)*cdexp(-Ci*aakz(aaolyr)*aah)*(-Ci*aakz(aaolyr)*upprop-grtm(aaolyr,aaolyr+1)*Ci*aakz(aaolyr)*cdexp(-Ci*aakz(aaolyr)*(-aaz-2*aah+2*zm+2*aadd(aaslyr))))*divider ! ?
    !vertical vector potential
    apg12=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aah)*(cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))
    aaintdata(2)=((1+apg12)*amptm*(upprop+grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
    aaintdata(4)=aaintdata(2)
  else
    !vertical scalar potential
    downprop=cdexp(Ci*aakz(aaolyr)*aaz)
    aac1d=1.0d0+cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr))*grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*mitm
    aac2d=amptm*cdexp(-Ci*(aakz(aaolyr)-aakz(aaslyr))*aah)
    aaa1d=-aac1d*aac2d*cdexp(-Ci*aakz(aaslyr)*aah)*aakz(aaolyr)
    aaa2d=-aac2d*grtm(aaslyr,aaslyr+1)*mitm*cdexp(Ci*aakz(aaslyr)*(aah-2*aadd(aaslyr)))*aakz(aaolyr)
    aaintdata(1)=-Ci*(aaa1d+aaa2d)*cdexp(Ci*aakz(aaolyr)*aah)*(downprop-grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(aaz+2*aah+2*zm)))*divider
    aaintdata(3)=-Ci*(aaa1d+aaa2d)*cdexp(Ci*aakz(aaolyr)*aah)*(Ci*aakz(aaolyr)*downprop+Ci*aakz(aaolyr)*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(aaz+2*aah+2*zm)))*divider
    !vertical vector potential
    amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))+grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))
    aaintdata(2)=((1+amg12)*amptm*(downprop+grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aah+zm))*cdexp(-Ci*aakz(aaolyr)*aaz)))*divider
    aaintdata(4)=aaintdata(2)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0

outint(1)=dreal(-aaintdata(1)*krho*((krho*CBJ1)/aarho))
outint(5)=dimag(-aaintdata(1)*krho*((krho*CBJ1)/aarho))
outint(2)=dreal(aaintdata(2)*krho*CBJ0)
outint(6)=dimag(aaintdata(2)*krho*CBJ0)
outint(3)=dreal(aaintdata(3)*krho*CBJ0)
outint(7)=dimag(aaintdata(3)*krho*CBJ0)
outint(4)=dreal(-aaintdata(4)*krho*((krho*CBJ1)/aarho))
outint(8)=dimag(-aaintdata(4)*krho*((krho*CBJ1)/aarho))
aaintegrands3dvert=.srt.(cdabs(aaintdata(2))**2+cdabs(aaintdata(1)/aakz(aaolyr))**2)
end function

real*8 function aaintegrands2d(krhor,outint)
real*8 outint(6)
complex*16 aaintdata(3)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte
complex*16 rte(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr)
complex*16 divider, upprop, downprop

if(dabs(krhor).lt.1.0d-6) krho=1.0d-6
if (dabs(krhor).lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
    krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
else
  krho=krhor
endif

call getaakz(krho)

call amprefte(rte,grte,zm,ampte) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krho2=krho**2

if(aaslyr.eq.aaolyr) then
  upprop=cdexp(-Ci*aakz(aaslyr)*aaz)
  downprop=cdexp(Ci*aakz(aaslyr)*aaz)
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mite 
  cihor=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
!
aaintdata(1)=(aihor*downprop+cihor*upprop)*divider
aaintdata(2)=aaintdata(1)
aaintdata(3)=(Ci*aakz(aaslyr)*aihor*downprop-Ci*aakz(aaslyr)*cihor*upprop)*divider
else
if (aaolyr.gt.aaslyr) then
  upprop=cdexp(-Ci*aakz(aaolyr)*aaz)
  ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mite
  aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(-Ci*aakz(aaolyr)*upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*Ci*aakz(aaolyr)*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
else
  downprop=cdexp(Ci*aakz(aaolyr)*aaz)
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(downprop+grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
    aaintdata(2)=aaintdata(1)
    aaintdata(3)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(Ci*aakz(aaolyr)*downprop+grte(aaolyr,aaolyr-1)*(-Ci*aakz(aaolyr))*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
  endif
endif  

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0

outint(1)=dreal(aaintdata(1)*cdexp(-Ci*krho*aarho))
outint(4)=dimag(aaintdata(1)*cdexp(-Ci*krho*aarho))
outint(2)=dreal(-Ci*krho*aaintdata(2)*cdexp(-Ci*krho*aarho))
outint(5)=dimag(-Ci*krho*aaintdata(2)*cdexp(-Ci*krho*aarho))
outint(3)=dreal(aaintdata(3)*cdexp(-Ci*krho*aarho))
outint(6)=dimag(aaintdata(3)*cdexp(-Ci*krho*aarho))
aaintegrands2d=.srt.(cdabs(aaintdata(1))**2+cdabs(aaintdata(3)/aakz(aaolyr))**2)
end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!real*8 function aaintegrands2d_kgamma(krhor,outint)
!real*8 outint(18)
!complex*16 aaintdata(9)
!real*8 zm,krhor
!complex*16 krho,krho2
!complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte,amptm
!complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
!complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
!complex*16 y1g2,y2g2,c1ug2,c2ug2,c1dg2,c2dg2,y1g5,y2g5,y1g4,y2g4,c1ug5,c2ug5,c1dg5,c2dg5
!complex*16 mitm,bihor,dihor,ampsptehor,ampsmtehor,ampsptmhor,ampsmtmhor
!complex*16 divider, upprop, downprop
!
!if(dabs(krhor).eq.0.0d0) krho=1.0d-6
!if (dabs(krhor).lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
!  krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
!else
!  krho=krhor
!endif
!
!call getaakz(krho)
!
!call ampref(rte,rtm,grte,grtm,zm,ampte,amptm) ! amplitude and reflection coefficients
!
!mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
!mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
!divider=1.0/(Ci*aakz(aaslyr))
!
!krho2=krho**2
!
!if(aaslyr.eq.aaolyr) then
!  upprop=cdexp(-Ci*aakz(aaslyr)*aaz)
!  downprop=cdexp(Ci*aakz(aaslyr)*aaz)
!  
!  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah)) &
!    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mite
!  bihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))-grtm(aaslyr,aaslyr-1) &
!    & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mitm
!  cihor=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
!    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
!  dihor=cdexp(-Ci*aakz(aaslyr)*aah)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1) &
!    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mitm
!
!  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
!  if (isnan(dreal(bihor)*dimag(bihor))) bihor=0.0
!  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
!  if (isnan(dreal(dihor)*dimag(dihor))) dihor=0.0
!!horizontal vector potential (parallel) related
!aaintdata(1)=(aihor*downprop+cihor*upprop)*divider ! GAXX
!aaintdata(7)=(Ci*aakz(aaslyr)*aihor*downprop-Ci*aakz(aaslyr)*cihor*upprop)*divider ! del(GAXX)/del(z)
!aaintdata(9)=aaintdata(1) ! GAXX
!!scalar potential related
!y1g2=(aakz(aaslyr)**2*bihor+aakk(aaslyr)**2*aihor)*downprop/krho2
!y2g2=(aakk(aaslyr)**2*cihor-aakz(aaslyr)**2*dihor)*upprop/krho2
!aaintdata(2)=(y1g2+y2g2)*divider ! Gqx
!aaintdata(3)=aaintdata(2) ! Gqx
!aaintdata(4)=(Ci*aakz(aaslyr)*y1g2-Ci*aakz(aaslyr)*y2g2)/(Ci*aakz(aaslyr)) ! del(Gqx)/del(z)
!!horizontal vector potential (normal) related
!y1g5=(aihor+bihor)*aakz(aaslyr)*downprop/(krho2)
!y2g5=(dihor-cihor)*aakz(aaslyr)*upprop/(krho2)
!aaintdata(5)=-(y1g5+y2g5)*Ci*divider ! GAzx
!aaintdata(6)=aaintdata(5) ! GAzx
!aaintdata(8)=aaintdata(5) ! GAzx
!else
!  if (aaolyr.gt.aaslyr) then
!  upprop=cdexp(-Ci*aakz(aaolyr)*aaz)
!  !horizontal vector potential (parallel) related
!    ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mite
!    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
!    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(-Ci*aakz(aaolyr)*upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*Ci*aakz(aaolyr)*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
!    aaintdata(9)=aaintdata(1)
!  !scalar potential related
!    ampsptehor=ampspg1
!    ampsptmhor=(1-grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mitm
!    c1ug2=ampsptmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte
!    c2ug2=-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*aakz(aaslyr)*aakz(aaolyr)- &
!      & aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))
!    y2g2=-c2ug2*cdexp(Ci*aakz(aaolyr)*aaz)/krho2
!    y1g2=-c1ug2*upprop/krho2
!    aaintdata(2)=(y1g2+y2g2)*divider
!    aaintdata(3)=aaintdata(2)
!    y2g4=Ci*aakz(aaolyr)*y2g2
!    y1g4=-Ci*aakz(aaolyr)*y1g2
!    aaintdata(4)=(y1g4+y2g4)*divider
!    !horizontal vector potential (normal) related
!    c1ug5=(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsptmhor*amptm
!    c2ug5=-(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+ &
!      & aadd(aaslyr)-aah))/aakz(aaslyr)-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))
!    y2g5=-c2ug5*aakz(aaslyr)*cdexp(Ci*aakz(aaolyr)*aaz)/krho2
!    y1g5=-c1ug5*aakz(aaslyr)*upprop/krho2
!    aaintdata(5)=-(y1g5+y2g5)*Ci*divider
!    aaintdata(6)=aaintdata(5)
!    aaintdata(8)=aaintdata(5)
!  else
!    !horizontal vector potential (parallel) related
!    downprop=cdexp(Ci*aakz(aaolyr)*aaz)
!    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mite
!    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(downprop+grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
!    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(Ci*aakz(aaolyr)*downprop-grte(aaolyr,aaolyr-1)*Ci*aakz(aaolyr)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
!    aaintdata(9)=aaintdata(1)
!    !scalar potential related
!    ampsmtehor=ampsmg1
!    ampsmtmhor=(-1+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mitm
!    c2dg2=-ampsmtmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte
!    c1dg2=ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah))*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2 &
!      & *(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah))
!    y2g2=-c2dg2*downprop/krho2     
!    y1g2=-c1dg2*cdexp(-Ci*aakz(aaolyr)*aaz)/krho2
!    aaintdata(2)=(y1g2+y2g2)*divider
!    aaintdata(3)=aaintdata(2)
!    y2g4=Ci*aakz(aaolyr)*y2g2
!    y1g4=-Ci*aakz(aaolyr)*y1g2
!    aaintdata(4)=(y1g4+y2g4)*divider
!    !horizontal vector potential (normal) related
!    c2dg5=-(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsmtmhor*amptm
!    c1dg5=(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah)) &
!      & /aakz(aaslyr)-ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aah+zm))
!    y2g5=-c2dg5*aakz(aaslyr)*downprop/krho2
!    y1g5=-c1dg5*aakz(aaslyr)*cdexp(-Ci*aakz(aaolyr)*aaz)/krho2
!    aaintdata(5)=-(y1g5+y2g5)*Ci*divider
!    aaintdata(6)=aaintdata(5)
!    aaintdata(8)=aaintdata(5)
!  end if
!end if
!
!if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
!if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
!if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
!if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0
!if (isnan(dreal(aaintdata(5))*dimag(aaintdata(5)))) aaintdata(5)=0.0
!if (isnan(dreal(aaintdata(6))*dimag(aaintdata(6)))) aaintdata(6)=0.0
!if (isnan(dreal(aaintdata(7))*dimag(aaintdata(7)))) aaintdata(7)=0.0
!if (isnan(dreal(aaintdata(8))*dimag(aaintdata(8)))) aaintdata(8)=0.0
!if (isnan(dreal(aaintdata(9))*dimag(aaintdata(9)))) aaintdata(9)=0.0
!
!
!outint(1)=dreal(aaintdata(1)*cdexp(-Ci*krho*aarho))
!outint(10)=dimag(aaintdata(1)*cdexp(-Ci*krho*aarho))
!outint(7)=dreal(aaintdata(7)*cdexp(-Ci*krho*aarho))
!outint(16)=dimag(aaintdata(7)*cdexp(-Ci*krho*aarho))
!outint(9)=dreal(-Ci*krho*aaintdata(9)*cdexp(-Ci*krho*aarho))
!outint(18)=dimag(-Ci*krho*aaintdata(9)*cdexp(-Ci*krho*aarho))
!outint(2)=dreal(-(kgamma**2)*aaintdata(2)*cdexp(-Ci*krho*aarho))
!outint(11)=dimag(-(kgamma**2)*aaintdata(2)*cdexp(-Ci*krho*aarho))
!outint(3)=dreal(-kgamma*krho*aaintdata(3)*cdexp(-Ci*krho*aarho))
!outint(12)=dimag(-kgamma*krho*aaintdata(3)*cdexp(-Ci*krho*aarho))
!outint(4)=dreal(-Ci*kgamma*aaintdata(4)*cdexp(-Ci*krho*aarho))
!outint(13)=dimag(-Ci*kgamma*aaintdata(4)*cdexp(-Ci*krho*aarho))
!outint(5)=dreal(-Ci*kgamma*aaintdata(5)*cdexp(-Ci*krho*aarho))
!outint(14)=dimag(-Ci*kgamma*aaintdata(5)*cdexp(-Ci*krho*aarho))
!outint(6)=dreal(-kgamma*krho*aaintdata(6)*cdexp(-Ci*krho*aarho))
!outint(15)=dimag(-kgamma*krho*aaintdata(6)*cdexp(-Ci*krho*aarho))
!outint(8)=dreal(-(kgamma**2)*aaintdata(8)*cdexp(-Ci*krho*aarho))
!outint(17)=dimag(-(kgamma**2)*aaintdata(8)*cdexp(-Ci*krho*aarho))
!aaintegrands2d_kgamma=.srt.(cdabs(aaintdata(1))**2+cdabs(aaintdata(2))**2+cdabs(krho*aaintdata(5))**2)
!end function aaintegrands2d_kgamma



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function aaintegrands2dc(krhor,outint)
real*8 outint(6)
complex*16 aaintdata(3)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte
complex*16 rte(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr)
complex*16 divider, upprop, downprop
complex*16 aahc

!only outgoing_wave components
!implementation of the complex origins
aahc=aah-Ci*aazi

if(krhor.eq.0.0d0) krho=1.0d-6
if (dabs(krhor).lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
    krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
else
  krho=krhor
endif

!! IMPORTANT PART: The shape of the integration path !!
if (aacpflag.eq.-1) then
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.-2) then
  krho=krhor-Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.1) then  ! just for krho causality NOT USED IN THE CODE
  krho=aakrhomax+Ci*krhor
  if (dreal(-Ci*aarho*krho).gt.1d-30) then
    krho=-krho
  endif
elseif (aacpflag.eq.2) then
  krho=-(aakrhomax+krhor)
elseif (aacpflag.eq.3) then
  krho=(aakrhomax+krhor) 
elseif (aacpflag.eq.4) then ! The 4 different choices after the integration between -kmax -- +kmax
  krho=-aakrhomax+Ci*krhor
elseif (aacpflag.eq.5) then
  krho=-aakrhomax-Ci*krhor
elseif (aacpflag.eq.6) then
  krho=aakrhomax+Ci*krhor
elseif (aacpflag.eq.7) then
  krho=aakrhomax-Ci*krhor
else
  write(*,*)'something wrong with the integrand flag!'
endif

!! IMPORTANT PART !! Causality of the wave vector in z direction
call getaakz(krho)

call ampreftec(rte,grte,zm,ampte) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krho2=krho**2

if(aaslyr.eq.aaolyr) then
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))*mite 
  cihor=cdexp(-Ci*aakz(aaslyr)*aahc)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))*mite
  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
!
  if (aaz.gt.0.0) then
  upprop=cdexp(-Ci*aakz(aaslyr)*aazc)
  aaintdata(1)=(cihor*upprop)*divider !no direct term included
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=(-Ci*aakz(aaslyr)*cihor*upprop)*divider
  else
  downprop=cdexp(Ci*aakz(aaslyr)*aazc)
  aaintdata(1)=(aihor*downprop)*divider !no direct term included
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=(Ci*aakz(aaslyr)*aihor*downprop)*divider
  endif
else
if (aaolyr.gt.aaslyr) then
  upprop=cdexp(-Ci*aakz(aaolyr)*aazc)
  ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aahc))*mite
  aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(upprop))*divider
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(-Ci*aakz(aaolyr)*upprop))*divider
else
    downprop=cdexp(Ci*aakz(aaolyr)*aazc)
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(downprop))*divider
    aaintdata(2)=aaintdata(1)
    aaintdata(3)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(Ci*aakz(aaolyr)*downprop))*divider
  endif
endif  

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0

outint(1)=dreal(aaintdata(1)*cdexp(-Ci*krho*aarhoc))
outint(4)=dimag(aaintdata(1)*cdexp(-Ci*krho*aarhoc))
outint(2)=dreal(-Ci*krho*aaintdata(2)*cdexp(-Ci*krho*aarhoc))
outint(5)=dimag(-Ci*krho*aaintdata(2)*cdexp(-Ci*krho*aarhoc))
outint(3)=dreal(aaintdata(3)*cdexp(-Ci*krho*aarhoc))
outint(6)=dimag(aaintdata(3)*cdexp(-Ci*krho*aarhoc))
aaintegrands2dc=.srt.(cdabs(aaintdata(1))**2+cdabs(aaintdata(3)/aakz(aaolyr))**2)
end function aaintegrands2dc
!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine integrator2dc(halfcyc,ress,path_id0,path_id1,path_id2,path_id3,path_id4)
Integer(4) halfcyc,counter,i,checker,attn
Integer(4) path_id1, path_id2, path_id3, path_id4, path_id0
real*8 krhor(1)
real*8 resltpos(35),resltneg(35)
complex*16 respos(3,halfcyc+2), resneg(3,halfcyc+2), check_1, check_2
real*8 aagk(1),bbgk(1)
Integer(4) mm,iOK
complex*16 ress(3), resspos(3), ressneg(3)

attn=0
checker=0

aagk(1)=1d-6 ! the first bound of the integration (1e-6 to inf by aitken)

i=(floor(aakmax/pi)+1) ! the next zero after the kmax value
bbgk(1)=i*pi+pi/2

aacpflag=path_id0
aakrhomax=bbgk(1)

call intgk(aaintegrands2dc,7,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
if (iOK.ne.0) then
iOK=0
endif

call intgk(aaintegrands2dc,7,krhor,1,-bbgk,-aagk,aakey,100000,aaepsrel,resltneg,mm,iOK)
  do i=2,7
    if (isnan(resltneg(i))) then
      resltneg(i)=0.0
    endif
  enddo
respos(1,1)=dcmplx(resltpos(2),resltpos(5))
respos(2,1)=dcmplx(resltpos(3),resltpos(6))
respos(3,1)=dcmplx(resltpos(4),resltpos(7))
resneg(1,1)=dcmplx(resltneg(2),resltneg(5))
resneg(2,1)=dcmplx(resltneg(3),resltneg(6))
resneg(3,1)=dcmplx(resltneg(4),resltneg(7))

ress=respos(:,1)+resneg(:,1)

call intgk(aaintegrands2dc_opp,7,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)

call intgk(aaintegrands2dc_opp,7,krhor,1,-bbgk,-aagk,aakey,100000,aaepsrel,resltneg,mm,iOK)
  do i=2,7
    if (isnan(resltpos(i))) then
      resltpos(i)=0.0
    endif
  enddo
respos(1,1)=dcmplx(resltpos(2),resltpos(5))
respos(2,1)=dcmplx(resltpos(3),resltpos(6))
respos(3,1)=dcmplx(resltpos(4),resltpos(7))
resneg(1,1)=dcmplx(resltneg(2),resltneg(5))
resneg(2,1)=dcmplx(resltneg(3),resltneg(6))
resneg(3,1)=dcmplx(resltneg(4),resltneg(7))

! the result between -kmax to kmax CAUSAL!
ress=respos(:,1)+resneg(:,1)+ress

counter=0
check_1=1
check_2=1

do while (((cdabs(check_1).gt.aaepsrel) .or. (cdabs(check_2).gt.aaepsrel)) .and. counter.lt.(halfcyc))
  
  if ((path_id1.eq.3).or.(path_id1.eq.2)) then
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarho).gt.1d-100) then
        bbgk=dabs((pi/2)/aarho)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarho).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarho)
      bbgk=dabs((counter*pi+pi/2)/aarho)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  
  else
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarhoi).gt.1d-100) then
        bbgk=dabs((pi/2)/aarhoi)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarhoi).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarhoi)
      bbgk=dabs((counter*pi+pi/2)/aarhoi)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  endif
  
!  path_id1=3
  aacpflag=path_id1 !integrate along the imaginary direction (positive Re(krho)) (ref. Lukas Novotny)
  call intgk(aaintegrands2dc,7,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
  do i=2,7
    if (isnan(resltpos(i))) then
      resltpos(i)=0.0
    endif
  enddo
  if (counter.eq.0) then ! put the values obtained depending on the path of integration
    if ((path_id1.eq.5).or.(path_id1.eq.7)) then
      respos(1,counter+1)=dcmplx(-resltpos(5),resltpos(2))
      respos(2,counter+1)=dcmplx(-resltpos(6),resltpos(3))
      respos(3,counter+1)=dcmplx(-resltpos(7),resltpos(4))
    elseif ((path_id1.eq.4).or.(path_id1.eq.6)) then
      respos(1,counter+1)=dcmplx(resltpos(5),-resltpos(2))
      respos(2,counter+1)=dcmplx(resltpos(6),-resltpos(3))
      respos(3,counter+1)=dcmplx(resltpos(7),-resltpos(4))
    elseif ((path_id1.eq.2).or.(path_id1.eq.3)) then
      respos(1,counter+1)=dcmplx(resltpos(2),resltpos(5))
      respos(2,counter+1)=dcmplx(resltpos(3),resltpos(6))
      respos(3,counter+1)=dcmplx(resltpos(4),resltpos(7)) 
    endif
  else
    if ((path_id1.eq.5).or.(path_id1.eq.7)) then
      respos(1,counter+1)=respos(1,counter)+dcmplx(-resltpos(5),resltpos(2))
      respos(2,counter+1)=respos(2,counter)+dcmplx(-resltpos(6),resltpos(3))
      respos(3,counter+1)=respos(3,counter)+dcmplx(-resltpos(7),resltpos(4))
    elseif ((path_id1.eq.4).or.(path_id1.eq.6)) then
      respos(1,counter+1)=respos(1,counter)+dcmplx(resltpos(5),-resltpos(2))
      respos(2,counter+1)=respos(2,counter)+dcmplx(resltpos(6),-resltpos(3))
      respos(3,counter+1)=respos(3,counter)+dcmplx(resltpos(7),-resltpos(4))
    elseif ((path_id1.eq.2).or.(path_id1.eq.3)) then
      respos(1,counter+1)=respos(1,counter)+dcmplx(resltpos(2),resltpos(5))
      respos(2,counter+1)=respos(2,counter)+dcmplx(resltpos(3),resltpos(6))
      respos(3,counter+1)=respos(3,counter)+dcmplx(resltpos(4),resltpos(7))
    endif 
  endif
  check_1=resltpos(2)+Ci*resltpos(5)
      
      
  if ((path_id3.eq.3).or.(path_id3.eq.2)) then
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarho).gt.1d-100) then
        bbgk=dabs((pi/2)/aarho)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarho).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarho)
      bbgk=dabs((counter*pi+pi/2)/aarho)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  
  else
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarhoi).gt.1d-100) then
        bbgk=dabs((pi/2)/aarhoi)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarhoi).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarhoi)
      bbgk=dabs((counter*pi+pi/2)/aarhoi)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  endif
  
      aacpflag=path_id3 !integrate along the imaginary direction (positive Re(krho)) (ref. Lukas Novotny)
  call intgk(aaintegrands2dc_opp,7,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
  do i=2,7
    if (isnan(resltpos(i))) then
      resltpos(i)=0.0
    endif
  enddo
    if ((path_id3.eq.5).or.(path_id3.eq.7)) then
      respos(1,counter+1)=respos(1,counter+1)+dcmplx(-resltpos(5),resltpos(2))
      respos(2,counter+1)=respos(2,counter+1)+dcmplx(-resltpos(6),resltpos(3))
      respos(3,counter+1)=respos(3,counter+1)+dcmplx(-resltpos(7),resltpos(4))
    elseif ((path_id3.eq.4).or.(path_id3.eq.6)) then
      respos(1,counter+1)=respos(1,counter+1)+dcmplx(resltpos(5),-resltpos(2))
      respos(2,counter+1)=respos(2,counter+1)+dcmplx(resltpos(6),-resltpos(3))
      respos(3,counter+1)=respos(3,counter+1)+dcmplx(resltpos(7),-resltpos(4))
    elseif ((path_id3.eq.2).or.(path_id3.eq.3)) then
      respos(1,counter+1)=respos(1,counter+1)+dcmplx(resltpos(2),resltpos(5))
      respos(2,counter+1)=respos(2,counter+1)+dcmplx(resltpos(3),resltpos(6))
      respos(3,counter+1)=respos(3,counter+1)+dcmplx(resltpos(4),resltpos(7)) 
    endif
  check_1=check_1+resltpos(2)+Ci*resltpos(5)

  if (iOK.ne.0) then
    iOK=0
  endif
!  path_id2=3

  if ((path_id2.eq.3).or.(path_id2.eq.2)) then
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarho).gt.1d-100) then
        bbgk=dabs((pi/2)/aarho)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarho).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarho)
      bbgk=dabs((counter*pi+pi/2)/aarho)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  
  else
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarhoi).gt.1d-100) then
        bbgk=dabs((pi/2)/aarhoi)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarhoi).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarhoi)
      bbgk=dabs((counter*pi+pi/2)/aarhoi)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  endif
  
  aacpflag=path_id2 !integrate along the imaginary direction (negative Re(krho)) (ref. Lukas Novotny)
  call intgk(aaintegrands2dc,7,krhor,1,bbgk,aagk,aakey,100000,aaepsrel,resltneg,mm,iOK)
  do i=2,7
    if (isnan(resltneg(i))) then
      resltneg(i)=0.0
    endif
  enddo
  
  if (counter.eq.0) then
    if ((path_id2.eq.7).or.(path_id2.eq.5)) then
      resneg(1,counter+1)=dcmplx(-resltneg(5),resltneg(2))
      resneg(2,counter+1)=dcmplx(-resltneg(6),resltneg(3))
      resneg(3,counter+1)=dcmplx(-resltneg(7),resltneg(4))
    elseif ((path_id2.eq.6).or.(path_id2.eq.4)) then
      resneg(1,counter+1)=dcmplx(resltneg(5),-resltneg(2))
      resneg(2,counter+1)=dcmplx(resltneg(6),-resltneg(3))
      resneg(3,counter+1)=dcmplx(resltneg(7),-resltneg(4))
    elseif ((path_id2.eq.3).or.(path_id2.eq.2)) then
      resneg(1,counter+1)=-dcmplx(resltneg(2),resltneg(5))
      resneg(2,counter+1)=-dcmplx(resltneg(3),resltneg(6))
      resneg(3,counter+1)=-dcmplx(resltneg(4),resltneg(7)) 
    endif
  else
    if ((path_id2.eq.7).or.(path_id2.eq.5)) then
      resneg(1,counter+1)=resneg(1,counter)+dcmplx(-resltneg(5),resltneg(2))
      resneg(2,counter+1)=resneg(2,counter)+dcmplx(-resltneg(6),resltneg(3))
      resneg(3,counter+1)=resneg(3,counter)+dcmplx(-resltneg(7),resltneg(4))
    elseif ((path_id2.eq.6).or.(path_id2.eq.4)) then
      resneg(1,counter+1)=resneg(1,counter)+dcmplx(resltneg(5),-resltneg(2))
      resneg(2,counter+1)=resneg(2,counter)+dcmplx(resltneg(6),-resltneg(3))
      resneg(3,counter+1)=resneg(3,counter)+dcmplx(resltneg(7),-resltneg(4))
    elseif ((path_id2.eq.3).or.(path_id2.eq.2)) then
      resneg(1,counter+1)=resneg(1,counter)-dcmplx(resltneg(2),resltneg(5))
      resneg(2,counter+1)=resneg(2,counter)-dcmplx(resltneg(3),resltneg(6))
      resneg(3,counter+1)=resneg(3,counter)-dcmplx(resltneg(4),resltneg(7)) 
    endif
  endif
  
  check_2=resltneg(2)+Ci*resltneg(5)
  
  if ((path_id4.eq.3).or.(path_id4.eq.2)) then
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarho).gt.1d-100) then
        bbgk=dabs((pi/2)/aarho)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarho).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarho)
      bbgk=dabs((counter*pi+pi/2)/aarho)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  
  else
    if (counter.eq.0) then
      aagk=0
      if (dabs(aarhoi).gt.1d-100) then
        bbgk=dabs((pi/2)/aarhoi)
      else
        bbgk=dabs(pi/2)
      endif
    elseif (dabs(aarhoi).gt.1d-100) then
      aagk=dabs(((counter-1)*pi+pi/2)/aarhoi)
      bbgk=dabs((counter*pi+pi/2)/aarhoi)
    else
      aagk=dabs(((counter-1)*pi+pi/2))
      bbgk=dabs((counter*pi+pi/2))
    endif
  endif
  
    aacpflag=path_id4 !integrate along the imaginary direction (negative Re(krho)) (ref. Lukas Novotny)
  call intgk(aaintegrands2dc_opp,7,krhor,1,bbgk,aagk,aakey,100000,aaepsrel,resltneg,mm,iOK)
    do i=2,7
    if (isnan(resltneg(i))) then
      resltneg(i)=0.0
    endif
  enddo
  
    if ((path_id4.eq.7).or.(path_id4.eq.5)) then
      resneg(1,counter+1)=resneg(1,counter+1)+dcmplx(-resltneg(5),resltneg(2))
      resneg(2,counter+1)=resneg(2,counter+1)+dcmplx(-resltneg(6),resltneg(3))
      resneg(3,counter+1)=resneg(3,counter+1)+dcmplx(-resltneg(7),resltneg(4))
    elseif ((path_id4.eq.6).or.(path_id4.eq.4)) then
      resneg(1,counter+1)=resneg(1,counter+1)+dcmplx(resltneg(5),-resltneg(2))
      resneg(2,counter+1)=resneg(2,counter+1)+dcmplx(resltneg(6),-resltneg(3))
      resneg(3,counter+1)=resneg(3,counter+1)+dcmplx(resltneg(7),-resltneg(4))
    elseif ((path_id4.eq.3).or.(path_id4.eq.2)) then
      resneg(1,counter+1)=resneg(1,counter+1)-dcmplx(resltneg(2),resltneg(5))
      resneg(2,counter+1)=resneg(2,counter+1)-dcmplx(resltneg(3),resltneg(6))
      resneg(3,counter+1)=resneg(3,counter+1)-dcmplx(resltneg(4),resltneg(7)) 
    endif
    check_2=check_2+resltneg(2)+Ci*resltneg(2)
    
counter=counter+1
enddo

if (counter.eq.(halfcyc)) then
  call aitkenvec2d(respos,halfcyc,resspos)
  call aitkenvec2d(resneg,halfcyc,ressneg)
  ress=ress+resspos+ressneg
else
  ress=ress+respos(:,counter)+resneg(:,counter)
endif

if (path_id0.eq.-2) then ! depending on the path chosen the result changes
    ress=-ress
endif

ress=ress*mue0*aamur(aaolyr)/(4*pi)
return
end subroutine integrator2dc

subroutine path_chooser(path_1,path_2)
implicit none
integer*4 path_1, path_2,num_zer, isokay, counter
real*8 p(2:7), mak_val, min_val, aarhoi_pc, aarho_pc, checkp(2:7),bbbound
real*8 outintxx(20), dumm(3), dumm_inp, aabound, aachecker(2:7)

min_val=1d-300
mak_val=1e7
p=-1
checkp=0 !if the integrand exceeds mak_val, it is not included in the calculation
num_zer=21
aachecker=1
aakrhomax=aakmax+1
isokay=0
counter=0
aabound=0.0
bbbound=0.0
path_1=2
path_2=3
if (dabs(aarhoi).gt.1d-100) then
  aarhoi_pc=aarhoi
else
  aarhoi_pc=1
endif

if (dabs(aarho).gt.1d-100) then
  aarho_pc=aarho
else
  aarho_pc=1
endif



do while (((sum(aachecker(2:7)).gt.1).and.(isokay.lt.4).and.(num_zer.gt.3)).or.(counter.lt.4))
num_zer=num_zer-2*counter
counter=counter+1
aacpflag=5
if (aabound.eq.0.0) then
aabound=dabs((num_zer*pi+pi/4)/aarhoi_pc)
if (aabound.gt.500) then
    aabound=500-25*counter
endif
else
aabound=aabound/2
endif
dumm_inp=aabound-1

dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(5)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(5).gt.mak_val) checkp(5)=1


if ((p(5).le.min_val).or.(p(5).ge.mak_val).or.(isnan(p(5)))) then
  aachecker(5)=1
  else
  aachecker(5)=0
endif

aacpflag=4
dumm_inp=aabound-1
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(4)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(4).gt.mak_val) checkp(4)=1
if ((p(4).le.min_val).or.(p(4).ge.mak_val).or.(isnan(p(4)))) then
  aachecker(4)=1
  else
  aachecker(4)=0
endif


aacpflag=2
if (bbbound.eq.0) then
bbbound=dabs((num_zer*pi+pi/4)/aarho_pc)
if (bbbound.gt.500) then
    bbbound=500-25*counter
endif
else
bbbound=bbbound/2
endif

dumm_inp=bbbound-1
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands2dc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands2dc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(2)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(2).gt.mak_val) checkp(2)=1
if ((p(2).le.min_val).or.(p(2).ge.mak_val).or.(isnan(p(2)))) then
  aachecker(2)=1
  else
  aachecker(2)=0
endif

if (checkp(5).eq.0) then
  if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=4
    endif
  elseif ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=2
    endif
  else
    path_1=5
  endif
elseif (checkp(4).eq.0) then
  if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=5
    endif
  elseif ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=2
    endif
  else
    path_1=4
  endif
elseif (checkp(2).eq.0) then
  if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=5
    endif
  elseif ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=4
    endif
  else
    path_1=2
  endif
endif


aacpflag=6
dumm_inp=aabound-1
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(6)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(6).gt.mak_val) checkp(6)=1
if ((p(6).le.min_val).or.(p(6).ge.mak_val).or.(isnan(p(6)))) then
  aachecker(6)=1
  else
  aachecker(6)=0
endif


aacpflag=7
dumm_inp=aabound-1
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(7)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(7).gt.mak_val) checkp(7)=1
if ((p(7).le.min_val).or.(p(7).ge.mak_val).or.(isnan(p(7)))) then
  aachecker(7)=1
  else
  aachecker(7)=0
endif

aacpflag=3
dumm_inp=bbbound-1
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands2dc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands2dc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(3)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(3).gt.mak_val) checkp(3)=1
if ((p(3).le.min_val).or.(p(3).ge.mak_val).or.(isnan(p(3)))) then
  aachecker(3)=1
  else
  aachecker(3)=0
endif



if (checkp(7).eq.0) then
  if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=6
    endif
  elseif ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=3
    endif
  else
    path_2=7
  endif
elseif (checkp(6).eq.0) then
  if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=7
    endif
  elseif ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=3
    endif
  else
    path_2=6
  endif
elseif (checkp(3).eq.0) then
  if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=7
    endif
  elseif ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=6
    endif
  else
    path_2=3
  endif
endif


if (sum(p(2:7)).le.min_val) then  !check if all of them are zero (3 times thaen it means no wave propagated by that integral)
  isokay=isokay+1
endif

!some last check
aacpflag=path_1
dumm_inp=aakmax/counter
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_1)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_1)=1
  else
  aachecker(path_1)=0
endif

aacpflag=path_2
dumm_inp=aakmax/counter
dumm(1)=aaintegrands2dc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_2)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_2)=1
  else
  aachecker(path_2)=0
endif



enddo
endsubroutine path_chooser


subroutine path_chooser_opp(path_1,path_2)
implicit none
integer*4 path_1, path_2, num_zer, aachecker(2:7), isokay, counter
real*8 p(2:7), min_val, mak_val, aarhoi_pc, aarho_pc, checkp(2:7)
real*8 outintxx(20), dumm(3), dumm_inp, aabound, bbbound

min_val=1d-300
mak_val=1e7
p=-1
checkp=0 !if the integrand exceeds mak_val, it is not included in the calculation
num_zer=21
aachecker=1
aakrhomax=aakmax+1
isokay=0
counter=0
aabound=0.0
bbbound=0.0
path_1=2
path_2=3
if (dabs(aarhoi).gt.1d-100) then
  aarhoi_pc=aarhoi
else
  aarhoi_pc=1
endif

if (dabs(aarho).gt.1d-100) then
  aarho_pc=aarho
else
  aarho_pc=1
endif


do while (((sum(aachecker(2:7)).gt.1).and.(isokay.lt.4).and.(num_zer.gt.3)).or.(counter.lt.4))
num_zer=num_zer-2*counter
counter=counter+1
aacpflag=5
if (aabound.eq.0.0) then
aabound=dabs((num_zer*pi+pi/4)/aarhoi_pc)
if (aabound.gt.500) then
    aabound=500-25*counter
endif
else
aabound=aabound/2
endif
dumm_inp=aabound-1

dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(5)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(5).gt.mak_val) checkp(5)=1


if ((p(5).le.min_val).or.(p(5).ge.mak_val).or.(isnan(p(5)))) then
  aachecker(5)=1
  else
  aachecker(5)=0
endif

aacpflag=4
dumm_inp=aabound-1
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(4)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(4).gt.mak_val) checkp(4)=1
if ((p(4).le.min_val).or.(p(4).ge.mak_val).or.(isnan(p(4)))) then
  aachecker(4)=1
  else
  aachecker(4)=0
endif


aacpflag=2
if (bbbound.eq.0) then
bbbound=dabs((num_zer*pi+pi/4)/aarho_pc)
if (bbbound.gt.500) then
    bbbound=500-25*counter
endif
else
bbbound=bbbound/2
endif

dumm_inp=bbbound-1
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(2)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(2).gt.mak_val) checkp(2)=1
if ((p(2).le.min_val).or.(p(2).ge.mak_val).or.(isnan(p(2)))) then
  aachecker(2)=1
  else
  aachecker(2)=0
endif

if (checkp(5).eq.0) then
  if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=4
    endif
  elseif ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=2
    endif
  else
    path_1=5
  endif
elseif (checkp(4).eq.0) then
  if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=5
    endif
  elseif ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=2
    endif
  else
    path_1=4
  endif
elseif (checkp(2).eq.0) then
  if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=5
    endif
  elseif ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=4
    endif
  else
    path_1=2
  endif
endif



aacpflag=6
dumm_inp=aabound-1
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(6)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(6).gt.mak_val) checkp(6)=1
if ((p(6).le.min_val).or.(p(6).ge.mak_val).or.(isnan(p(6)))) then
  aachecker(6)=1
  else
  aachecker(6)=0
endif


aacpflag=7
dumm_inp=aabound-1
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(7)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(7).gt.mak_val) checkp(7)=1
if ((p(7).le.min_val).or.(p(7).ge.mak_val).or.(isnan(p(7)))) then
  aachecker(7)=1
  else
  aachecker(7)=0
endif

aacpflag=3
dumm_inp=bbbound-1
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(3)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(3).gt.mak_val) checkp(3)=1
if ((p(3).le.min_val).or.(p(3).ge.mak_val).or.(isnan(p(3)))) then
  aachecker(3)=1
  else
  aachecker(3)=0
endif



if (checkp(7).eq.0) then
  if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=6
    endif
  elseif ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=3
    endif
  else
    path_2=7
  endif
elseif (checkp(6).eq.0) then
  if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=7
    endif
  elseif ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=3
    endif
  else
    path_2=6
  endif
elseif (checkp(3).eq.0) then
  if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=7
    endif
  elseif ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=6
    endif
  else
    path_2=3
  endif
endif



if (sum(p(2:7)).le.min_val) then  !check if all of them are zero (3 times thaen it means no wave propagated by that integral)
  isokay=isokay+1
endif

!some last check
aacpflag=path_1
dumm_inp=aakmax/counter
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_1)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_1)=1
  else
  aachecker(path_1)=0
endif

aacpflag=path_2
dumm_inp=aakmax/counter
dumm(1)=aaintegrands2dc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_2)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_2)=1
  else
  aachecker(path_2)=0
endif

enddo
endsubroutine path_chooser_opp


real*8 function aaintegrands2dc_opp(krhor,outint)
real*8 outint(6)
complex*16 aaintdata(3)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte
complex*16 rte(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr)
complex*16 divider, upprop, downprop
complex*16 aahc

!only incoming_wave components
!implementation of the complex origins
aahc=aah-Ci*aazi

if(krhor.eq.0.0d0) krho=1.0d-6
if (dabs(krhor).lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
    krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
else
  krho=krhor
endif

!! IMPORTANT PART: The shape of the integratino path !!
if (aacpflag.eq.-1) then
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.-2) then
  krho=krhor-Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.1) then  ! just for krho causality NOT USED IN THE CODE
  krho=aakrhomax+Ci*krhor
  if (dreal(-Ci*aarho*krho).gt.1d-30) then
    krho=-krho
  endif
elseif (aacpflag.eq.2) then
  krho=-(aakrhomax+krhor)
elseif (aacpflag.eq.3) then
  krho=(aakrhomax+krhor)
elseif (aacpflag.eq.4) then ! The 4 different choices after the integration between -kmax -- +kmax
  krho=-aakrhomax+Ci*krhor
elseif (aacpflag.eq.5) then
  krho=-aakrhomax-Ci*krhor
elseif (aacpflag.eq.6) then
  krho=aakrhomax+Ci*krhor
elseif (aacpflag.eq.7) then
  krho=aakrhomax-Ci*krhor
else
  write(*,*)'something wrong with the integrand flag!'
endif

!! IMPORTANT PART !! Causality of the wave vector in z direction
call getaakz(krho)

call ampreftec(rte,grte,zm,ampte) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krho2=krho**2

if(aaslyr.eq.aaolyr) then
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))*mite 
  cihor=cdexp(-Ci*aakz(aaslyr)*aahc)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))*mite
  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
!
  if (aaz.gt.0.0) then
  downprop=cdexp(Ci*aakz(aaslyr)*aazc)
  aaintdata(1)=(aihor*downprop)*divider !no direct term included
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=(Ci*aakz(aaslyr)*aihor*downprop)*divider
  else
  upprop=cdexp(-Ci*aakz(aaslyr)*aazc)
  aaintdata(1)=(cihor*upprop)*divider
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=(-Ci*aakz(aaslyr)*cihor*upprop)*divider
  endif
else
if (aaolyr.gt.aaslyr) then
  ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aahc))*mite
  aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))*cdexp(Ci*aakz(aaolyr)*aazc)))*divider
  aaintdata(2)=aaintdata(1)
  aaintdata(3)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))*Ci*aakz(aaolyr)*cdexp(Ci*aakz(aaolyr)*aazc)))*divider
else
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aahc)+aazc))))*divider
    aaintdata(2)=aaintdata(1)
    aaintdata(3)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(grte(aaolyr,aaolyr-1)*(-Ci*aakz(aaolyr))*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aahc)+aazc))))*divider
  endif
endif  

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0

outint(1)=dreal(aaintdata(1)*cdexp(-Ci*krho*aarhoc))
outint(4)=dimag(aaintdata(1)*cdexp(-Ci*krho*aarhoc))
outint(2)=dreal(-Ci*krho*aaintdata(2)*cdexp(-Ci*krho*aarhoc))
outint(5)=dimag(-Ci*krho*aaintdata(2)*cdexp(-Ci*krho*aarhoc))
outint(3)=dreal(aaintdata(3)*cdexp(-Ci*krho*aarhoc))
outint(6)=dimag(aaintdata(3)*cdexp(-Ci*krho*aarhoc))
aaintegrands2dc_opp=.srt.(cdabs(aaintdata(1))**2+cdabs(aaintdata(3)/aakz(aaolyr))**2)
end function aaintegrands2dc_opp


!!!THE 3d implementation for the complex origin
real*8 function aaintegrands3dhorc(krhor,outint)
real*8 outint(20)
complex*16 aaintdata(10)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
complex*16 y1g2,y2g2,c1ug2,y1g5,y2g5,y1g4,y2g4,c2dg5,c2dg2,c1ug5
complex*16 mitm,bihor,dihor,ampsptehor,ampsmtehor,ampsptmhor,ampsmtmhor
complex*16 CBH02,CBH12
complex*16 divider, upprop, downprop
complex*16 aahc, zzcheck
Integer(4) idumy1,idumy2,idumy3,iErr
complex*16 cBes(0:20)

!The functions to be integrated, only outgoing wave components


!only outgoing_wave components
!implementation of the complex origins
aahc=aah-Ci*aazi

!do not fall inside singularity
if(krhor.eq.0.0d0) krho=1.0d-6

! different path configurations
if (aacpflag.eq.-1) then
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.-2) then
  krho=krhor-Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.1) then  ! just for krho causality NOT USED IN THE CODE
  krho=aakrhomax+Ci*krhor
  if (dreal(-Ci*aarho*krho).gt.1d-30) then
    krho=-krho
  endif
elseif (aacpflag.eq.2) then
  krho=-(aakrhomax+krhor)
elseif (aacpflag.eq.3) then
  krho=(aakrhomax+krhor) 
elseif (aacpflag.eq.4) then ! The 4 different choices after the integration between -kmax -- +kmax
  krho=-aakrhomax+Ci*krhor
elseif (aacpflag.eq.5) then
  krho=-aakrhomax-Ci*krhor
elseif (aacpflag.eq.6) then
  krho=aakrhomax+Ci*krhor
elseif (aacpflag.eq.7) then
  krho=aakrhomax-Ci*krhor
else
  write(*,*)'something wrong with the integrand flag!'
endif

!call for the needed bessel/hankel functions
idumy1=2
idumy2=20
idumy3=14

if (aarhoi.lt.0.0) then ! causality criteria in rho direction
call ch1(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 1st kind
    CBH12=cBes(1)
else
call ch2(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 2nd kind
    CBH12=cBes(1)
endif
! causality of all k vector z components in different layers
call getaakz(krho)

call amprefc(rte,rtm,grte,grtm,zm,ampte,amptm) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krho2=krho**2

if(aaslyr.eq.aaolyr) then
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))*mite
  bihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))-grtm(aaslyr,aaslyr-1) &
    & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))*mitm
  cihor=cdexp(-Ci*aakz(aaslyr)*aahc)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))*mite
  dihor=cdexp(-Ci*aakz(aaslyr)*aahc)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aahc)+grtm(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))*mitm

  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(bihor)*dimag(bihor))) bihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
  if (isnan(dreal(dihor)*dimag(dihor))) dihor=0.0

  
if (aaz.gt.0.0) then
    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
!horizontal vector potential (parallel) related
upprop=cdexp(-Ci*aakz(aaslyr)*aazc*zzcheck)
aaintdata(1)=(cihor*upprop)*divider
aaintdata(7)=(-Ci*aakz(aaslyr)*cihor*upprop)*divider
aaintdata(9)=aaintdata(1)
!scalar potential related
y2g2=(aakk(aaslyr)**2*cihor-aakz(aaslyr)**2*dihor)*upprop/krho2
aaintdata(2)=(y2g2)*divider
aaintdata(3)=aaintdata(2)
aaintdata(4)=(-Ci*aakz(aaslyr)*y2g2)/(Ci*aakz(aaslyr))
aaintdata(10)=aaintdata(2)
!horizontal vector potential (normal) related
y2g5=(dihor-cihor)*aakz(aaslyr)*upprop/(krho2)
aaintdata(5)=-(y2g5)*Ci*divider
aaintdata(6)=aaintdata(5)
aaintdata(8)=aaintdata(5)
else
    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
downprop=cdexp(Ci*aakz(aaslyr)*aazc*zzcheck)
aaintdata(1)=(aihor*downprop)*divider
aaintdata(7)=(Ci*aakz(aaslyr)*aihor*downprop)*divider
aaintdata(9)=aaintdata(1)
!scalar potential related
y1g2=(aakz(aaslyr)**2*bihor+aakk(aaslyr)**2*aihor)*downprop/krho2
aaintdata(2)=(y1g2)*divider
aaintdata(3)=aaintdata(2)
aaintdata(4)=(Ci*aakz(aaslyr)*y1g2)/(Ci*aakz(aaslyr))
aaintdata(10)=aaintdata(2)
!horizontal vector potential (normal) related
y1g5=(aihor+bihor)*aakz(aaslyr)*downprop/(krho2)
aaintdata(5)=-(y1g5)*Ci*divider
aaintdata(6)=aaintdata(5)
aaintdata(8)=aaintdata(5)
endif

else
if (aaolyr.gt.aaslyr) then
    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif

  upprop=cdexp(-Ci*aakz(aaolyr)*aazc*zzcheck)
  !horizontal vector potential (parallel) related
    ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aahc))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(upprop))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(-Ci*aakz(aaolyr)*upprop))*divider
    aaintdata(9)=aaintdata(1)
  !scalar potential related
    ampsptehor=ampspg1
    ampsptmhor=(1-grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aahc))*mitm
    c1ug2=ampsptmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte
    y1g2=-c1ug2*upprop/krho2
    aaintdata(2)=(y1g2)*divider
    aaintdata(3)=aaintdata(2)
    y1g4=-Ci*aakz(aaolyr)*y1g2
    aaintdata(4)=(y1g4)*divider
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    c1ug5=(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsptmhor*amptm
    y1g5=-c1ug5*aakz(aaslyr)*upprop/krho2
    aaintdata(5)=-(y1g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  else
    !horizontal vector potential (parallel) related
    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
    downprop=cdexp(Ci*aakz(aaolyr)*aazc*zzcheck)
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(downprop))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(Ci*aakz(aaolyr)*downprop))*divider
    aaintdata(9)=aaintdata(1)
    !scalar potential related
    ampsmtehor=ampsmg1
    ampsmtmhor=(-1+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*mitm
    c2dg2=-ampsmtmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte
    y2g2=-c2dg2*downprop/krho2     
    aaintdata(2)=(y2g2)*divider
    aaintdata(3)=aaintdata(2)
    y2g4=Ci*aakz(aaolyr)*y2g2
    aaintdata(4)=(y2g4)*divider
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    c2dg5=-(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsmtmhor*amptm
    y2g5=-c2dg5*aakz(aaslyr)*downprop/krho2
    aaintdata(5)=-(y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0
if (isnan(dreal(aaintdata(5))*dimag(aaintdata(5)))) aaintdata(5)=0.0
if (isnan(dreal(aaintdata(6))*dimag(aaintdata(6)))) aaintdata(6)=0.0
if (isnan(dreal(aaintdata(7))*dimag(aaintdata(7)))) aaintdata(7)=0.0
if (isnan(dreal(aaintdata(8))*dimag(aaintdata(8)))) aaintdata(8)=0.0
if (isnan(dreal(aaintdata(9))*dimag(aaintdata(9)))) aaintdata(9)=0.0
if (isnan(dreal(aaintdata(10))*dimag(aaintdata(10)))) aaintdata(10)=0.0


!These are the values with H02 and H12
outint(1)=dreal(aaintdata(1)*CBH02*krho)
outint(11)=dimag(aaintdata(1)*CBH02*krho)
outint(7)=dreal(aaintdata(7)*CBH02*krho)
outint(17)=dimag(aaintdata(7)*CBH02*krho)
outint(9)=dreal(-aaintdata(9)*krho*((krho*aayc*CBH12)/aarhoc))
outint(19)=dimag(-aaintdata(9)*krho*((krho*aayc*CBH12)/aarhoc))
outint(2)=dreal(aaintdata(2)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
outint(12)=dimag(aaintdata(2)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
outint(3)=dreal(aaintdata(3)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(13)=dimag(aaintdata(3)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(4)=dreal(-aaintdata(4)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(14)=dimag(-aaintdata(4)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(10)=dreal(aaintdata(10)*krho*((krho*aayc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aayc**2)/(aarhoc**3)))
outint(20)=dimag(aaintdata(10)*krho*((krho*aayc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aayc**2)/(aarhoc**3)))
outint(5)=dreal(-aaintdata(5)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(15)=dimag(-aaintdata(5)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(6)=dreal(aaintdata(6)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(16)=dimag(aaintdata(6)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(8)=dreal(aaintdata(8)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
outint(18)=dimag(aaintdata(8)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
aaintegrands3dhorc=cdabs(cdsqrt((outint(1)+Ci*outint(11))**2+(outint(2)+Ci*outint(12))**2))
end function aaintegrands3dhorc


real*8 function aaintegrands3dhorc_opp(krhor,outint)
real*8 outint(20)
complex*16 aaintdata(10)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte,amptm
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
complex*16 y1g2,y2g2,c2ug2,c1dg2,y1g5,y2g5,y1g4,y2g4,c2ug5,c1dg5
complex*16 mitm,bihor,dihor,ampsptehor,ampsmtehor,ampsptmhor,ampsmtmhor
complex*16 CBH02,CBH12
complex*16 divider, upprop, downprop
complex*16 aahc, zzcheck
Integer(4) idumy1,idumy2,idumy3,iErr
complex*16 cBes(0:20)

!The functions to be integrated, only incoming wave components


!only imcoming_wave components
!implementation of the complex origins
aahc=aah-Ci*aazi

!do not fall inside singularity
if(krhor.eq.0.0d0) krho=1.0d-6

! different path configurations
if (aacpflag.eq.-1) then
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.-2) then
  krho=krhor-Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.1) then  ! just for krho causality NOT USED IN THE CODE
  krho=aakrhomax+Ci*krho
  if (dreal(-Ci*aarho*krho).gt.1d-30) then
    krho=-krho
  endif
elseif (aacpflag.eq.2) then
  krho=-(aakrhomax+krhor)
elseif (aacpflag.eq.3) then
  krho=(aakrhomax+krhor) 
elseif (aacpflag.eq.4) then ! The 4 different choices after the integration between -kmax -- +kmax
  krho=-aakrhomax+Ci*krhor
elseif (aacpflag.eq.5) then
  krho=-aakrhomax-Ci*krhor
elseif (aacpflag.eq.6) then
  krho=aakrhomax+Ci*krhor
elseif (aacpflag.eq.7) then
  krho=aakrhomax-Ci*krhor
else
  write(*,*)'something wrong with the integrand flag!'
endif

idumy1=2
idumy2=20
idumy3=14
!call for the needed bessel/hankel functions
if (aarhoi.lt.0.0) then ! causality criteria in rho direction
call ch1(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 1st kind
    CBH12=cBes(1)
else
call ch2(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 2nd kind
    CBH12=cBes(1)
endif

! causality of all k vector z components in different layers
call getaakz(krho)

call amprefc(rte,rtm,grte,grtm,zm,ampte,amptm) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krho2=krho**2

if(aaslyr.eq.aaolyr) then
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))*mite
  bihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))-grtm(aaslyr,aaslyr-1) &
    & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))*mitm
  cihor=cdexp(-Ci*aakz(aaslyr)*aahc)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))*mite
  dihor=cdexp(-Ci*aakz(aaslyr)*aahc)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aahc)+grtm(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))*mitm

  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(bihor)*dimag(bihor))) bihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
  if (isnan(dreal(dihor)*dimag(dihor))) dihor=0.0
  
  if (aaz.gt.0.0) then
    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
    
    downprop=cdexp(Ci*aakz(aaslyr)*aazc*zzcheck)
    !horizontal vector potential (parallel) related
    aaintdata(1)=(aihor*downprop)*divider
    aaintdata(7)=(Ci*aakz(aaslyr)*aihor*downprop)*divider
    aaintdata(9)=aaintdata(1)
    !scalar potential related
    y1g2=(aakz(aaslyr)**2*bihor+aakk(aaslyr)**2*aihor)*downprop/krho2

    aaintdata(2)=(y1g2)*divider
    aaintdata(3)=aaintdata(2)
    aaintdata(4)=(Ci*aakz(aaslyr)*y1g2)/(Ci*aakz(aaslyr))
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    y1g5=(aihor+bihor)*aakz(aaslyr)*downprop/(krho2)
    aaintdata(5)=-(y1g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  else
    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
    upprop=cdexp(-Ci*aakz(aaslyr)*aazc*zzcheck)
    aaintdata(1)=(cihor*upprop)*divider
    aaintdata(7)=(-Ci*aakz(aaslyr)*cihor*upprop)*divider
    aaintdata(9)=aaintdata(1)
    !scalar potential related
    y2g2=(aakk(aaslyr)**2*cihor-aakz(aaslyr)**2*dihor)*upprop/krho2
    aaintdata(2)=(y2g2)*divider
    aaintdata(3)=aaintdata(2)
    aaintdata(4)=(-Ci*aakz(aaslyr)*y2g2)/(Ci*aakz(aaslyr))
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    y2g5=(dihor-cihor)*aakz(aaslyr)*upprop/(krho2)
    aaintdata(5)=-(y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  endif
else
  if (aaolyr.gt.aaslyr) then
    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
  
  !horizontal vector potential (parallel) related
    ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aahc))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))*cdexp(Ci*aakz(aaolyr)*aazc*zzcheck)))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))*Ci*aakz(aaolyr)*cdexp(Ci*aakz(aaolyr)*aazc*zzcheck)))*divider
    aaintdata(9)=aaintdata(1)
  !scalar potential related
    ampsptehor=ampspg1
    ampsptmhor=(1-grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aahc))*mitm
    c2ug2=-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))*aakz(aaslyr)*aakz(aaolyr)- &
      & aakk(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))
    y2g2=-c2ug2*cdexp(Ci*aakz(aaolyr)*aazc*zzcheck)/krho2
    aaintdata(2)=(y2g2)*divider
    aaintdata(3)=aaintdata(2)
    y2g4=Ci*aakz(aaolyr)*y2g2
    aaintdata(4)=(y2g4)*divider
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    c2ug5=-(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+ &
      & aadd(aaslyr)-aahc))/aakz(aaslyr)-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))
    y2g5=-c2ug5*aakz(aaslyr)*cdexp(Ci*aakz(aaolyr)*aazc*zzcheck)/krho2
    aaintdata(5)=-(y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  else
    !horizontal vector potential (parallel) related
    
    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
        zzcheck=-(-1)
    else
        zzcheck=1
    endif
    
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aahc)+aazc*zzcheck))))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(-grte(aaolyr,aaolyr-1)*Ci*aakz(aaolyr)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aahc)+aazc*zzcheck))))*divider
    aaintdata(9)=aaintdata(1)
    !scalar potential related
    ampsmtehor=ampsmg1
    ampsmtmhor=(-1+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*mitm
    c1dg2=ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aahc))*aakz(aaslyr)*aakz(aaolyr)-aakk(aaolyr)**2 &
      & *(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aahc))
    y1g2=-c1dg2*cdexp(-Ci*aakz(aaolyr)*aazc*zzcheck)/krho2
    aaintdata(2)=(y1g2)*divider
    aaintdata(3)=aaintdata(2)
    y1g4=-Ci*aakz(aaolyr)*y1g2
    aaintdata(4)=(y1g4)*divider
    aaintdata(10)=aaintdata(2)
    !horizontal vector potential (normal) related
    c1dg5=(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aahc)) &
      & /aakz(aaslyr)-ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aahc+zm))
    y1g5=-c1dg5*aakz(aaslyr)*cdexp(-Ci*aakz(aaolyr)*aazc*zzcheck)/krho2
    aaintdata(5)=-(y1g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0
if (isnan(dreal(aaintdata(5))*dimag(aaintdata(5)))) aaintdata(5)=0.0
if (isnan(dreal(aaintdata(6))*dimag(aaintdata(6)))) aaintdata(6)=0.0
if (isnan(dreal(aaintdata(7))*dimag(aaintdata(7)))) aaintdata(7)=0.0
if (isnan(dreal(aaintdata(8))*dimag(aaintdata(8)))) aaintdata(8)=0.0
if (isnan(dreal(aaintdata(9))*dimag(aaintdata(9)))) aaintdata(9)=0.0
if (isnan(dreal(aaintdata(10))*dimag(aaintdata(10)))) aaintdata(10)=0.0

outint(1)=dreal(aaintdata(1)*CBH02*krho)
outint(11)=dimag(aaintdata(1)*CBH02*krho)
outint(7)=dreal(aaintdata(7)*CBH02*krho)
outint(17)=dimag(aaintdata(7)*CBH02*krho)
outint(9)=dreal(-aaintdata(9)*krho*((krho*aayc*CBH12)/aarhoc))
outint(19)=dimag(-aaintdata(9)*krho*((krho*aayc*CBH12)/aarhoc))
outint(2)=dreal(aaintdata(2)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
outint(12)=dimag(aaintdata(2)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
outint(3)=dreal(aaintdata(3)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(13)=dimag(aaintdata(3)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(4)=dreal(-aaintdata(4)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(14)=dimag(-aaintdata(4)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(10)=dreal(aaintdata(10)*krho*((krho*aayc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aayc**2)/(aarhoc**3)))
outint(20)=dimag(aaintdata(10)*krho*((krho*aayc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aayc**2)/(aarhoc**3)))
outint(5)=dreal(-aaintdata(5)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(15)=dimag(-aaintdata(5)*krho*((krho*aaxc*CBH12)/aarhoc))
outint(6)=dreal(aaintdata(6)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(16)=dimag(aaintdata(6)*krho*((krho*aaxc*((aayc*CBH12)/(aarhoc**2) - (krho*aayc*CBH02)/aarhoc))/aarhoc + (krho*aaxc*aayc*CBH12)/(aarhoc**3)))
outint(8)=dreal(aaintdata(8)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))
outint(18)=dimag(aaintdata(8)*krho*((krho*aaxc*((aaxc*CBH12)/(aarhoc**2) - (krho*aaxc*CBH02)/aarhoc))/aarhoc - (krho*CBH12)/aarhoc + (CBH12*krho*aaxc**2)/(aarhoc**3)))

aaintegrands3dhorc_opp=cdabs(cdsqrt((outint(1)+Ci*outint(11))**2+(outint(2)+Ci*outint(12))**2))
end function aaintegrands3dhorc_opp


real*8 function aaintegrands3dvertc(krhor,outint)
real*8 outint(8)
complex*16 aaintdata(4)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 amptm
complex*16 rtm(-1:mxlyr,-1:mxlyr)
complex*16 grtm(-1:mxlyr,-1:mxlyr)
complex*16 CBH02,CBH12
complex*16 apg10,amg10,apg12,amg12, aac1u,  aac2u,  aaa1u,  aaa2u, aac1d,  aac2d,  aaa1d,  aaa2d
complex*16 mitm
complex*16 divider, upprop, downprop
complex*16 aahc, zzcheck
Integer(4) idumy1,idumy2,idumy3,iErr
complex*16 cBes(0:20)


!only outgoing_wave components
!implementation of the complex origins
aahc=aah-Ci*aazi

if(krhor.eq.0.0d0) krho=1.0d-6

! different path configurations
if (aacpflag.eq.-1) then
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.-2) then
  krho=krhor-Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.1) then  ! just for krho causality NOT USED IN THE CODE
  krho=aakrhomax+Ci*krhor
  if (dreal(-Ci*aarho*krho).gt.1d-30) then
    krho=-krho
  endif
elseif (aacpflag.eq.2) then
  krho=-(aakrhomax+krhor)
elseif (aacpflag.eq.3) then
  krho=(aakrhomax+krhor) 
elseif (aacpflag.eq.4) then ! The 4 different choices after the integration between -kmax -- +kmax
  krho=-aakrhomax+Ci*krhor
elseif (aacpflag.eq.5) then
  krho=-aakrhomax-Ci*krhor
elseif (aacpflag.eq.6) then
  krho=aakrhomax+Ci*krhor
elseif (aacpflag.eq.7) then
  krho=aakrhomax-Ci*krhor
else
  write(*,*)'something wrong with the integrand flag!'
endif

!call for the needed bessel/hankel functions
idumy1=2
idumy2=20
idumy3=14

if (aarhoi.lt.0.0) then ! causality criteria in rho direction
call ch1(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 1st kind
    CBH12=cBes(1)
else
call ch2(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 2nd kind
    CBH12=cBes(1)
endif


call getaakz(krho)
zzcheck=1
call ampreftmc(rtm,grtm,zm,amptm) ! amplitude and reflection coefficients

mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))

krho2=krho**2
divider=1.0/(Ci*aakz(aaslyr))

if(aaslyr.eq.aaolyr) then
!vertical scalar potential
if (aaz.gt.0.0) then
!    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif
upprop=cdexp(-Ci*aakz(aaslyr)*aazc*zzcheck)
apg10=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)*(-cdexp(-Ci*aakz(aaslyr)*aahc)-grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))
aaintdata(1)=(Ci*aakz(aaslyr)*apg10*upprop)*divider
aaintdata(3)=((aakz(aaslyr)**2)*apg10*upprop)*divider
!vertical vector potential
apg12=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))
aaintdata(2)=(apg12*upprop)*divider
aaintdata(4)=aaintdata(2)

else
!    
!    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif
downprop=cdexp(Ci*aakz(aaslyr)*aazc*zzcheck)
amg10=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(-cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))-grtm(aaslyr,aaslyr-1) &
  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))
aaintdata(1)=(-amg10*Ci*aakz(aaslyr)*downprop)*divider
aaintdata(3)=(amg10*(aakz(aaslyr)**2)*downprop)*divider
!vertical vector potential
amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))+grtm(aaslyr,aaslyr-1) &
  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))
aaintdata(2)=(amg12*downprop)*divider
aaintdata(4)=aaintdata(2)
endif

else
  if (aaolyr.gt.aaslyr) then
 zzcheck=1 
!    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif
  
  upprop=cdexp(-Ci*aakz(aaolyr)*aazc*zzcheck)
  !horizontal vector potential (parallel) related
    !vertical scalar potential 
    aac1u=1.0d0+cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr))*grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*mitm
    aac2u=amptm*cdexp(Ci*(aakz(aaolyr)-aakz(aaslyr))*aahc)
    aaa1u=aac1u*aac2u*aakz(aaolyr)*cdexp(Ci*aakz(aaslyr)*aahc)
    aaa2u=aac2u*aakz(aaolyr)*grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)
    aaintdata(1)=-Ci*(aaa1u+aaa2u)*cdexp(-Ci*aakz(aaolyr)*aahc)*(upprop)*divider
    aaintdata(3)=-Ci*(aaa1u+aaa2u)*cdexp(-Ci*aakz(aaolyr)*aahc)*(-Ci*aakz(aaolyr)*upprop)*divider ! ?
    apg12=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))
    aaintdata(2)=(1+apg12)*amptm*(upprop)*divider
    aaintdata(4)=aaintdata(2)
  else
!    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
zzcheck=1
!    endif
    !vertical scalar potential

    downprop=cdexp(Ci*aakz(aaolyr)*aazc*zzcheck)
    aac1d=1.0d0+cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr))*grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*mitm
    aac2d=amptm*cdexp(-Ci*(aakz(aaolyr)-aakz(aaslyr))*aahc)
    aaa1d=-aac1d*aac2d*cdexp(-Ci*aakz(aaslyr)*aahc)*aakz(aaolyr)
    aaa2d=-aac2d*grtm(aaslyr,aaslyr+1)*mitm*cdexp(Ci*aakz(aaslyr)*(aahc-2*aadd(aaslyr)))*aakz(aaolyr)
    aaintdata(1)=-Ci*(aaa1d+aaa2d)*cdexp(Ci*aakz(aaolyr)*aahc)*(downprop)*divider
    aaintdata(3)=-Ci*(aaa1d+aaa2d)*cdexp(Ci*aakz(aaolyr)*aahc)*(Ci*aakz(aaolyr)*downprop)*divider
    amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))+grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))
    aaintdata(2)=(1+amg12)*amptm*(downprop)*divider
    aaintdata(4)=aaintdata(2)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0

outint(1)=dreal(-aaintdata(1)*krho*((krho*CBH12)/aarhoc))
outint(5)=dimag(-aaintdata(1)*krho*((krho*CBH12)/aarhoc))
outint(2)=dreal(aaintdata(2)*krho*CBH02)
outint(6)=dimag(aaintdata(2)*krho*CBH02)
outint(3)=dreal(aaintdata(3)*krho*CBH02)
outint(7)=dimag(aaintdata(3)*krho*CBH02)
outint(4)=dreal(-aaintdata(4)*krho*((krho*CBH12)/aarhoc))
outint(8)=dimag(-aaintdata(4)*krho*((krho*CBH12)/aarhoc))
aaintegrands3dvertc=dsqrt(cdabs(aaintdata(2))**2+cdabs(aaintdata(1)/aakz(aaolyr))**2)
end function aaintegrands3dvertc




real*8 function aaintegrands3dvertc_opp(krhor,outint)
real*8 outint(8)
complex*16 aaintdata(4)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 amptm
complex*16 rtm(-1:mxlyr,-1:mxlyr)
complex*16 grtm(-1:mxlyr,-1:mxlyr)
complex*16 CBH02,CBH12
complex*16 apg10,amg10,apg12,amg12, aac1u, aac2u, aaa1u, aaa2u, aac1d, aac2d, aaa1d, aaa2d
complex*16 mitm
complex*16 divider, upprop, downprop
complex*16 aahc, zzcheck
Integer(4) idumy1,idumy2,idumy3,iErr
complex*16 cBes(0:20)

!only incoming_wave components
!implementation of the complex origins
aahc=aah-Ci*aazi

if(krhor.eq.0.0d0) krho=1.0d-6

! different path configurations
if (aacpflag.eq.-1) then
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.-2) then
  krho=krhor-Ci*aadint*dsin(pi*krhor/aakrhomax)
elseif (aacpflag.eq.1) then  ! just for krho causality NOT USED IN THE CODE
  krho=aakrhomax+Ci*krho
  if (dreal(-Ci*aarho*krhor).gt.1d-30) then
    krho=-krho
  endif
elseif (aacpflag.eq.2) then
  krho=-(aakrhomax+krhor)
elseif (aacpflag.eq.3) then
  krho=(aakrhomax+krhor) 
elseif (aacpflag.eq.4) then ! The 4 different choices after the integration between -kmax -- +kmax
  krho=-aakrhomax+Ci*krhor
elseif (aacpflag.eq.5) then
  krho=-aakrhomax-Ci*krhor
elseif (aacpflag.eq.6) then
  krho=aakrhomax+Ci*krhor
elseif (aacpflag.eq.7) then
  krho=aakrhomax-Ci*krhor
else
  write(*,*)'something wrong with the integrand flag!'
endif

!call for the needed bessel/hankel functions
idumy1=2
idumy2=20
idumy3=14

if (aarhoi.lt.0.0) then ! causality criteria in rho direction
call ch1(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 1st kind
    CBH12=cBes(1)
else
call ch2(krho*aarhoc,cBes(0),idumy1,idumy2,idumy3,ierr)
if(ierr.gt.0) cBes=(0.0d0,0.0d0)
  if((ierr.ne.0).and.lDispWarn) then
    write(*,*) 'WARNING: Error',ierr,' in Bessel(',krho*aarhoc,')'
    lDispWarn=.false.
  end if
    CBH02=cBes(0) !hankel function, 2nd kind
    CBH12=cBes(1)
endif


call getaakz(krho)
zzcheck=1
call ampreftmc(rtm,grtm,zm,amptm) ! amplitude and reflection coefficients

mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))

krho2=krho**2
divider=1.0/(Ci*aakz(aaslyr))

if(aaslyr.eq.aaolyr) then

if (aaz.gt.0.0) then
!    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif
    
!vertical scalar potential
downprop=cdexp(Ci*aakz(aaslyr)*aazc)
amg10=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(-cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))-grtm(aaslyr,aaslyr-1) &
  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))
aaintdata(1)=(-amg10*Ci*aakz(aaslyr)*downprop)*divider
aaintdata(3)=(amg10*(aakz(aaslyr)**2)*downprop)*divider
!vertical vector potential
amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))+grtm(aaslyr,aaslyr-1) &
  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))
aaintdata(2)=(amg12*downprop)*divider
aaintdata(4)=aaintdata(2)

else
!    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif

upprop=cdexp(-Ci*aakz(aaslyr)*aazc)
apg10=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)*(-cdexp(-Ci*aakz(aaslyr)*aahc)-grtm(aaslyr,aaslyr+1) &
  & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))
aaintdata(1)=(Ci*aakz(aaslyr)*apg10*upprop)*divider
aaintdata(3)=((aakz(aaslyr)**2)*apg10*upprop)*divider
!vertical vector potential
apg12=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grtm(aaslyr,aaslyr+1) &
  & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))
aaintdata(2)=(apg12*upprop)*divider
aaintdata(4)=aaintdata(2)
endif

else
  if (aaolyr.gt.aaslyr) then
    
!    if ((aazi.gt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif
    zzcheck=1
    
    !vertical scalar potential 

    aac1u=1.0d0+cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr))*grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*mitm
    aac2u=amptm*cdexp(Ci*(aakz(aaolyr)-aakz(aaslyr))*aahc)
    aaa1u=aac1u*aac2u*aakz(aaolyr)*cdexp(Ci*aakz(aaslyr)*aahc)
    aaa2u=aac2u*aakz(aaolyr)*grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)
    aaintdata(1)=-Ci*(aaa1u+aaa2u)*cdexp(-Ci*aakz(aaolyr)*aahc)*(-grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*(-aazc-2*aahc+2*zm+2*aadd(aaslyr))))*divider
    aaintdata(3)=-Ci*(aaa1u+aaa2u)*cdexp(-Ci*aakz(aaolyr)*aahc)*(-grtm(aaolyr,aaolyr+1)*Ci*aakz(aaolyr)*cdexp(-Ci*aakz(aaolyr)*(-aazc-2*aahc+2*zm+2*aadd(aaslyr))))*divider ! ?
    apg12=grtm(aaslyr,aaslyr-1)*mitm*cdexp(-Ci*aakz(aaslyr)*aahc)*(cdexp(-Ci*aakz(aaslyr)*aahc)+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aahc)))
    aaintdata(2)=((1+apg12)*amptm*(grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aahc))*cdexp(Ci*aakz(aaolyr)*aazc)))*divider
    aaintdata(4)=aaintdata(2)

  else
!    if ((aazi.lt.0.0) .and. (dabs(aarho).le.dabs(aazi))) then
!        zzcheck=-(-1)
!    else
!        zzcheck=1
!    endif
    !vertical scalar potential

    aac1d=1.0d0+cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr))*grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*mitm
    aac2d=amptm*cdexp(-Ci*(aakz(aaolyr)-aakz(aaslyr))*aahc)
    aaa1d=-aac1d*aac2d*cdexp(-Ci*aakz(aaslyr)*aahc)*aakz(aaolyr)
    aaa2d=-aac2d*grtm(aaslyr,aaslyr+1)*mitm*cdexp(Ci*aakz(aaslyr)*(aahc-2*aadd(aaslyr)))*aakz(aaolyr)
    aaintdata(1)=-Ci*(aaa1d+aaa2d)*cdexp(Ci*aakz(aaolyr)*aahc)*(-grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(aazc+2*aahc+2*zm)))*divider
    aaintdata(3)=-Ci*(aaa1d+aaa2d)*cdexp(Ci*aakz(aaolyr)*aahc)*(Ci*aakz(aaolyr)*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(aazc+2*aahc+2*zm)))*divider
    amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))+grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))
    aaintdata(2)=((1+amg12)*amptm*(grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aahc+zm))*cdexp(-Ci*aakz(aaolyr)*aazc)))*divider
    aaintdata(4)=aaintdata(2)

!    c1dg10=-(-1.0d0+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aahc)))*amptm*mitm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aahc))
!    aaintdata(1)=(-Ci*aakz(aaolyr)*c1dg10*cdexp(-Ci*aakz(aaolyr)*aazc))*divider
!    aaintdata(3)=(-(aakz(aaolyr)**2)*c1dg10*cdexp(-Ci*aakz(aaolyr)*aazc))/(Ci*aakz(aaslyr))
!    amg12=grtm(aaslyr,aaslyr+1)*mitm*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aahc))+grtm(aaslyr,aaslyr-1) &
!  & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aahc)))
!    aaintdata(2)=((1+amg12)*amptm*(grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aahc+zm))*cdexp(-Ci*aakz(aaolyr)*aazc)))*divider
!    aaintdata(4)=aaintdata(2)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0

outint(1)=dreal(-aaintdata(1)*krho*((krho*CBH12)/aarhoc))
outint(5)=dimag(-aaintdata(1)*krho*((krho*CBH12)/aarhoc))
outint(2)=dreal(aaintdata(2)*krho*CBH02)
outint(6)=dimag(aaintdata(2)*krho*CBH02)
outint(3)=dreal(aaintdata(3)*krho*CBH02)
outint(7)=dimag(aaintdata(3)*krho*CBH02)
outint(4)=dreal(-aaintdata(4)*krho*((krho*CBH12)/aarhoc))
outint(8)=dimag(-aaintdata(4)*krho*((krho*CBH12)/aarhoc))
aaintegrands3dvertc_opp=dsqrt(cdabs(aaintdata(2))**2+cdabs(aaintdata(1)/aakz(aaolyr))**2)
end function aaintegrands3dvertc_opp


!!! Path choosers for 3dc

subroutine path_chooser3d(path_1,path_2)
implicit none
integer*4 path_1, path_2,num_zer, isokay, counter
real*8 p(2:7), mak_val, min_val, aarhoi_pc, aarho_pc, checkp(2:7),bbbound
real*8 outintxx(20), dumm(3), dumm_inp, aabound, aachecker(2:7)

min_val=1d-300
mak_val=1e7
p=-1
checkp=0 !if the integrand exceeds mak_val, it is not included in the calculation
num_zer=21
aachecker=1
aakrhomax=aakmax+1
isokay=0
counter=0
aabound=0.0
bbbound=0.0
path_1=2
path_2=3
if (dabs(aarhoi).gt.1d-100) then
  aarhoi_pc=aarhoi
else
  aarhoi_pc=1
endif

if (dabs(aarho).gt.1d-100) then
  aarho_pc=aarho
else
  aarho_pc=1
endif



do while (((sum(aachecker(2:7)).gt.1).and.(isokay.lt.4).and.(num_zer.gt.3)).or.(counter.lt.4))
num_zer=num_zer-2*counter
counter=counter+1
aacpflag=5
if (aabound.eq.0.0) then
aabound=dabs((num_zer*pi+pi/4)/aarhoi_pc)
if (aabound.gt.500) then
    aabound=500-25*counter
endif
else
aabound=aabound/2
endif
dumm_inp=aabound-1

dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(5)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(5).gt.mak_val) checkp(5)=1


if ((p(5).le.min_val).or.(p(5).ge.mak_val).or.(isnan(p(5)))) then
  aachecker(5)=1
  else
  aachecker(5)=0
endif

aacpflag=4
dumm_inp=aabound-1
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(4)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(4).gt.mak_val) checkp(4)=1
if ((p(4).le.min_val).or.(p(4).ge.mak_val).or.(isnan(p(4)))) then
  aachecker(4)=1
  else
  aachecker(4)=0
endif


aacpflag=2
if (bbbound.eq.0) then
bbbound=dabs((num_zer*pi+pi/4)/aarho_pc)
if (bbbound.gt.500) then
    bbbound=500-25*counter
endif
else
bbbound=bbbound/2
endif

dumm_inp=bbbound-1
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(2)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(2).gt.mak_val) checkp(2)=1
if ((p(2).le.min_val).or.(p(2).ge.mak_val).or.(isnan(p(2)))) then
  aachecker(2)=1
  else
  aachecker(2)=0
endif

if (checkp(5).eq.0) then
  if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=4
    endif
  elseif ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=2
    endif
  else
    path_1=5
  endif
elseif (checkp(4).eq.0) then
  if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=5
    endif
  elseif ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=2
    endif
  else
    path_1=4
  endif
elseif (checkp(2).eq.0) then
  if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=5
    endif
  elseif ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=4
    endif
  else
    path_1=2
  endif
endif


aacpflag=6
dumm_inp=aabound-1
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(6)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(6).gt.mak_val) checkp(6)=1
if ((p(6).le.min_val).or.(p(6).ge.mak_val).or.(isnan(p(6)))) then
  aachecker(6)=1
  else
  aachecker(6)=0
endif


aacpflag=7
dumm_inp=aabound-1
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(7)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(7).gt.mak_val) checkp(7)=1
if ((p(7).le.min_val).or.(p(7).ge.mak_val).or.(isnan(p(7)))) then
  aachecker(7)=1
  else
  aachecker(7)=0
endif

aacpflag=3
dumm_inp=bbbound-1
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(3)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(3).gt.mak_val) checkp(3)=1
if ((p(3).le.min_val).or.(p(3).ge.mak_val).or.(isnan(p(3)))) then
  aachecker(3)=1
  else
  aachecker(3)=0
endif

if (checkp(7).eq.0) then
  if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=6
    endif
  elseif ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=3
    endif
  else
    path_2=7
  endif
elseif (checkp(6).eq.0) then
  if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=7
    endif
  elseif ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=3
    endif
  else
    path_2=6
  endif
elseif (checkp(3).eq.0) then
  if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=7
    endif
  elseif ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=6
    endif
  else
    path_2=3
  endif
endif


if (sum(p(2:7)).le.min_val) then  !check if all of them are zero (3 times thaen it means no wave propagated by that integral)
  isokay=isokay+1
endif

!some last check
aacpflag=path_1
dumm_inp=aakmax/counter
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_1)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_1)=1
  else
  aachecker(path_1)=0
endif

aacpflag=path_2
dumm_inp=aakmax/counter
dumm(1)=aaintegrands3dhorc(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_2)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_2)=1
  else
  aachecker(path_2)=0
endif
enddo
endsubroutine path_chooser3d

subroutine path_chooser3d_opp(path_1,path_2)
implicit none
integer*4 path_1, path_2, num_zer, aachecker(2:7), isokay, counter
real*8 p(2:7), min_val, mak_val, aarhoi_pc, aarho_pc, checkp(2:7)
real*8 outintxx(20), dumm(3), dumm_inp, aabound, bbbound

min_val=1d-300
mak_val=1e7
p=-1
checkp=0 !if the integrand exceeds mak_val, it is not included in the calculation
num_zer=21
aachecker=1
aakrhomax=aakmax+1
isokay=0
counter=0
aabound=0.0
bbbound=0.0
path_1=2
path_2=3
if (dabs(aarhoi).gt.1d-100) then
  aarhoi_pc=aarhoi
else
  aarhoi_pc=1
endif

if (dabs(aarho).gt.1d-100) then
  aarho_pc=aarho
else
  aarho_pc=1
endif


do while (((sum(aachecker(2:7)).gt.1).and.(isokay.lt.4).and.(num_zer.gt.3)).or.(counter.lt.4))
num_zer=num_zer-2*counter
counter=counter+1
aacpflag=5
if (aabound.eq.0.0) then
aabound=dabs((num_zer*pi+pi/4)/aarhoi_pc)
if (aabound.gt.500) then
    aabound=500-25*counter
endif
else
aabound=aabound/2
endif
dumm_inp=aabound-1

dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(5)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(5).gt.mak_val) checkp(5)=1


if ((p(5).le.min_val).or.(p(5).ge.mak_val).or.(isnan(p(5)))) then
  aachecker(5)=1
  else
  aachecker(5)=0
endif

aacpflag=4
dumm_inp=aabound-1
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(4)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(4).gt.mak_val) checkp(4)=1
if ((p(4).le.min_val).or.(p(4).ge.mak_val).or.(isnan(p(4)))) then
  aachecker(4)=1
  else
  aachecker(4)=0
endif


aacpflag=2
if (bbbound.eq.0) then
bbbound=dabs((num_zer*pi+pi/4)/aarho_pc)
if (bbbound.gt.500) then
    bbbound=500-25*counter
endif
else
bbbound=bbbound/2
endif

dumm_inp=bbbound-1
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(2)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(2).gt.mak_val) checkp(2)=1
if ((p(2).le.min_val).or.(p(2).ge.mak_val).or.(isnan(p(2)))) then
  aachecker(2)=1
  else
  aachecker(2)=0
endif

if (checkp(5).eq.0) then
  if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=4
    endif
  elseif ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=2
    endif
  else
    path_1=5
  endif
elseif (checkp(4).eq.0) then
  if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(2)).and.(checkp(2).eq.0)) then
        path_1=2
    else
        path_1=5
    endif
  elseif ((p(4).gt.p(2)).and.(checkp(2).eq.0)) then
    if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=2
    endif
  else
    path_1=4
  endif
elseif (checkp(2).eq.0) then
  if ((p(2).gt.p(5)).and.(checkp(5).eq.0)) then
    if ((p(5).gt.p(4)).and.(checkp(4).eq.0)) then
        path_1=4
    else
        path_1=5
    endif
  elseif ((p(2).gt.p(4)).and.(checkp(4).eq.0)) then
    if ((p(4).gt.p(5)).and.(checkp(5).eq.0)) then
        path_1=5
    else
        path_1=4
    endif
  else
    path_1=2
  endif
endif



aacpflag=6
dumm_inp=aabound-1
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(6)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(6).gt.mak_val) checkp(6)=1
if ((p(6).le.min_val).or.(p(6).ge.mak_val).or.(isnan(p(6)))) then
  aachecker(6)=1
  else
  aachecker(6)=0
endif


aacpflag=7
dumm_inp=aabound-1
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound
dumm(2)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=aabound+1
dumm(3)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(7)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(7).gt.mak_val) checkp(7)=1
if ((p(7).le.min_val).or.(p(7).ge.mak_val).or.(isnan(p(7)))) then
  aachecker(7)=1
  else
  aachecker(7)=0
endif

aacpflag=3
dumm_inp=bbbound-1
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound
dumm(2)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(2)=dsqrt(outintxx(1)**2+outintxx(2)**2)
dumm_inp=bbbound+1
dumm(3)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(3)=dsqrt(outintxx(1)**2+outintxx(2)**2)
p(3)=(dumm(1)+2*dumm(2)+dumm(3))/2
if (p(3).gt.mak_val) checkp(3)=1
if ((p(3).le.min_val).or.(p(3).ge.mak_val).or.(isnan(p(3)))) then
  aachecker(3)=1
  else
  aachecker(3)=0
endif

if (checkp(7).eq.0) then
  if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=6
    endif
  elseif ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=3
    endif
  else
    path_2=7
  endif
elseif (checkp(6).eq.0) then
  if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(3)).and.(checkp(3).eq.0)) then
        path_2=3
    else
        path_2=7
    endif
  elseif ((p(6).gt.p(3)).and.(checkp(3).eq.0)) then
    if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=3
    endif
  else
    path_2=6
  endif
elseif (checkp(3).eq.0) then
  if ((p(3).gt.p(7)).and.(checkp(7).eq.0)) then
    if ((p(7).gt.p(6)).and.(checkp(6).eq.0)) then
        path_2=6
    else
        path_2=7
    endif
  elseif ((p(3).gt.p(6)).and.(checkp(6).eq.0)) then
    if ((p(6).gt.p(7)).and.(checkp(7).eq.0)) then
        path_2=7
    else
        path_2=6
    endif
  else
    path_2=3
  endif
endif

if (sum(p(2:7)).le.min_val) then  !check if all of them are zero (3 times thaen it means no wave propagated by that integral)
  isokay=isokay+1
endif

!some last check
aacpflag=path_1
dumm_inp=aakmax/counter
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_1)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_1)=1
  else
  aachecker(path_1)=0
endif

aacpflag=path_2
dumm_inp=aakmax/counter
dumm(1)=aaintegrands3dhorc_opp(dumm_inp,outintxx)
dumm(1)=dsqrt(outintxx(1)**2+outintxx(2)**2)
if (dumm(1).gt.mak_val) checkp(path_2)=1
if ((dumm(1).le.min_val).or.(dumm(1).ge.mak_val).or.(isnan(dumm(1)))) then
  aachecker(path_2)=1
  else
  aachecker(path_2)=0
endif
enddo
endsubroutine path_chooser3d_opp

subroutine integrator3dc(halfcyc,ress,path_id1,path_id2,path_id3,path_id4,iHVK)
Integer(2) iHVK
Integer(4) halfcyc,counter,i,checker,attn,ii
Integer(4) path_id1, path_id2, path_id3, path_id4, path_id(4)
real*8 krhor(1), dummbess
real*8 reslthpos(35),reslthneg(35),resltvpos(35),resltvneg(35),reslt(35),reslth(35),resltv(35)
real*8 aagk(1),bbgk(1),aa(4),bb(4)
Integer(4) mm,iOK
complex*16 resltorg(15,halfcyc),resl_mult(4)
complex*16 ress(15)

attn=0
checker=0
path_id(1)=path_id1
path_id(2)=path_id2
path_id(3)=path_id3
path_id(4)=path_id4

aagk(1)=1d-6 ! the first bound of the integration (1e-6 to inf by aitken)
resltorg=0.0
counter=0
do
  counter=counter+1
  dummbess=besszero(counter)/dabs(aarho)
  if (dummbess.gt.15000) then
  dummbess=15000
  attn=1
  endif
  if (dummbess.gt.aakmax) then
    exit
  endif
enddo
bbgk(1)=dummbess
reslthpos=0.0
resltvpos=0.0
reslthneg=0.0
resltvneg=0.0

aacpflag=-1
aakrhomax=bbgk(1)

!! Integral between -kmax to kmax (abs(bbgk)=abs(kmax))
if (iHVK.ne.1) then
call intgk(aaintegrands3dhorc,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslthpos,mm,iOK)
call intgk(aaintegrands3dhorc,21,krhor,1,-bbgk,-aagk,aakey,100000,aaepsrel,reslthneg,mm,iOK)
reslth=reslthpos+reslthneg
call intgk(aaintegrands3dhorc_opp,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslthpos,mm,iOK)
call intgk(aaintegrands3dhorc_opp,21,krhor,1,-bbgk,-aagk,aakey,100000,aaepsrel,reslthneg,mm,iOK)
reslth=reslth+reslthpos+reslthneg
endif
if (iHVK.ne.0) then
call intgk(aaintegrands3dvertc,9,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltvpos,mm,iOK)
call intgk(aaintegrands3dvertc,9,krhor,1,-bbgk,-aagk,aakey,100000,aaepsrel,resltvneg,mm,iOK)
resltv=resltvpos+resltvneg
call intgk(aaintegrands3dvertc_opp,9,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltvpos,mm,iOK)
call intgk(aaintegrands3dvertc_opp,9,krhor,1,-bbgk,-aagk,aakey,100000,aaepsrel,resltvneg,mm,iOK)
resltv=resltv+resltvpos+resltvneg
endif
reslt(1:9)=reslth(2:10)
reslt(10:13)=resltv(2:5)
reslt(14)=reslth(11)
reslt(15:23)=reslth(12:20)
reslt(24:27)=resltv(6:9)
reslt(28)=reslth(21)
do i=1,28
  if (isnan(reslt(i))) then
    reslt(i)=0.0
  endif
enddo

call resltorganizer(1,reslt,halfcyc,resltorg)
do i=2,halfcyc
  counter=counter+1
  if (i.eq.2) bb=0.0

  if (iHVK.ne.1) then !horizontal dipole calculations
    call bound_decider(path_id,counter,i,attn,aa,bb,resl_mult) 
    ! to the -real plane - direct wave
    aacpflag=path_id1
    aagk(1)=aa(1)
    bbgk(1)=bb(1)
    call intgk(aaintegrands3dhorc,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslthneg,mm,iOK)
    reslth=resl_mult(1)*reslthneg

    aacpflag=path_id2
    aagk(1)=aa(2)
    bbgk(1)=bb(2)
    call intgk(aaintegrands3dhorc,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslthpos,mm,iOK)
    reslth=reslth+resl_mult(2)*reslthpos
  
    aacpflag=path_id3
    aagk(1)=aa(3)
    bbgk(1)=bb(3)
    call intgk(aaintegrands3dhorc_opp,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslthneg,mm,iOK)
    reslth=reslth+resl_mult(3)*reslthneg

    aacpflag=path_id4
    aagk(1)=aa(4)
    bbgk(1)=bb(4)
    call intgk(aaintegrands3dhorc_opp,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,reslthpos,mm,iOK)
    reslth=reslth+resl_mult(4)*reslthpos
  endif

  if (iHVK.ne.0) then !vertical dipole calculations
    call bound_decider(path_id,counter,i,attn,aa,bb,resl_mult)
    aacpflag=path_id1 
    aagk(1)=aa(1)
    bbgk(1)=bb(1)
    call intgk(aaintegrands3dvertc,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltvpos,mm,iOK)
    resltv=resl_mult(1)*resltvpos

    aacpflag=path_id2 
    aagk(1)=aa(2)
    bbgk(1)=bb(2)
    call intgk(aaintegrands3dvertc,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltvneg,mm,iOK)
    resltv=resltv+resl_mult(2)*resltvneg
  
    aacpflag=path_id3
    aagk(1)=aa(3)
    bbgk(1)=bb(3)
    call intgk(aaintegrands3dvertc_opp,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltvpos,mm,iOK)
    resltv=resltv+resl_mult(3)*resltvpos

    aacpflag=path_id4
    aagk(1)=aa(4)
    bbgk(1)=bb(4)
    call intgk(aaintegrands3dvertc_opp,21,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltvpos,mm,iOK)
    resltv=resltv+resl_mult(4)*resltvneg
  endif
  reslt(1:9)=reslth(2:10)
  reslt(10:13)=resltv(2:5)
  reslt(14)=reslth(11)
  reslt(15:23)=reslth(12:20)
  reslt(24:27)=resltv(6:9)
  reslt(28)=reslth(21)

  do ii=1,28
    if (isnan(reslt(ii))) then
      reslt(i)=0.0
    endif
  enddo

  call resltorganizer(i,reslt,halfcyc,resltorg)

  if (dabs(reslt(1)).lt.aaepsrel**2) then
    checker=1
    ress=resltorg(:,i)
    exit
  endif
enddo
if (checker.eq.0) then
  call aitkenvec(resltorg,halfcyc,ress)
endif

ress(1)=ress(1)*mue0*aamur(aaolyr)/(8*pi)
ress(2)=ress(2)/(8*pi*eps0*aaepsr(aaolyr))
ress(3)=ress(3)/(8*pi*eps0*aaepsr(aaolyr))
ress(4)=ress(4)/(8*pi*eps0*aaepsr(aaolyr))
ress(5)=ress(5)*mue0*aamur(aaolyr)/(8*pi)
ress(6)=ress(6)*aamur(aaolyr)/(8*pi)
ress(7)=ress(7)*aamur(aaolyr)/(8*pi)
ress(8)=ress(8)*aamur(aaolyr)/(8*pi)
ress(9)=ress(9)*aamur(aaolyr)/(8*pi)
ress(10)=ress(10)/(8*pi*eps0*aaepsr(aaolyr))
ress(11)=ress(11)*mue0*aamur(aaolyr)/(8*pi)
ress(12)=ress(12)/(8*pi*eps0*aaepsr(aaolyr))
ress(13)=ress(13)*aamur(aaolyr)/(8*pi)
ress(14)=ress(14)/(8*pi*eps0*aaepsr(aaolyr))
ress(15)=-ress(8)
return
end subroutine integrator3dc

subroutine bound_decider(path_inf,cyc_num,indd,attn,aa,bb,reslt_mult)
Integer(4) path_inf(4), cyc_num, indd, attn, i
complex*16 reslt_mult(4)
real*8 aa(4), bb(4)

aa=bb
do i=1,4
if ((path_inf(i).eq.2).or.(path_inf(i).eq.3)) then
reslt_mult(i)=1
  if (attn.eq.1) then
    bb(i)=aa(i)+2500
  else
    bb(i)=(besszero(cyc_num)/dabs(aarho))-aakrhomax
  endif
else
  if (aarhoi.gt.1d-100) then
    bb(i)=dabs(((indd-2)*pi+pi/2)/aarhoi)
  else
    bb(i)=dabs(((indd-2)*pi+pi/2))
  endif
  if ((path_inf(i).eq.5).or.(path_inf(i).eq.7)) reslt_mult(i)=Ci
  if ((path_inf(i).eq.6).or.(path_inf(i).eq.4)) reslt_mult(i)=-Ci
endif
enddo
return
end subroutine


subroutine twodlyrbase_kgamma(aaiHEK,r,rimag,kmaxmult,aaA,obs_lyr,gamma_re,gamma_im)
implicit none
complex*16 freq
Integer(4) reflag,aaiHEK,i
real*8 xdata,ydata,kmaxmult,gamma_re,gamma_im
complex*16 dumm(0:mxlyr)
real*8 zconst
real*8 r(3),rimag(3)
Integer(4) halfcyc
complex*16 aafcFld
Integer(4) obs_lyr
complex*16 aaA(10),ress(9), distx, distz, dist
Integer(4) idumy1,idumy2,idumy3,iErr
complex*16 cBes(0:20)
!Integer(4) path_id1, path_id2, path_id3, path_id4, path_id0
!!! CONSTANT INITIALIZATION
aaA=0.0
aafcFld=dreal(fcFld)-Ci*dabs((dimag(fcFld)))
aafreq_norm=dreal(aafcFld)/1e9
freq=aafcFld/aafreq_norm ! frequency to GHz
k00=2*pi*freq*dsqrt(eps0*mue0) ! wave number of freespace
kgamma=k00*dcmplx(gamma_re,-gamma_im) ! causality change for exp(-iwt)
aakmax=dreal(k00)**2-kgamma**2
call geomnormer(r,obs_lyr)
rimag=rimag*aafreq_norm ! normalize the imaginary origin values

xdata=r(1) ! aax-information (field point)
ydata=r(2) ! y-information (field point)

if (obs_lyr.lt.1) then
  aaolyr=obslayer(r(2)) ! observation layer calculation (comment if the obslayer will be given outsite)
endif
aakey=3 ! order of integration (default is 6)

if (aaiHEK.eq.1) then ! check if the polarization is TM if so interchange parameters
  dumm=aaepsr
  aaepsr=aamur
  aamur=dumm
  if (peccheck(1).eq.1) then
  peccheck(1)=2
  elseif (peccheck(1).eq.2) then
  peccheck(1)=1
  endif
  if (peccheck(2).eq.2) then
  peccheck(2)=1
  elseif (peccheck(2).eq.1) then
  peccheck(2)=2
  endif
endif

reflag=0 !assume all the swps are purely real
!CALCULATE THE k VECTORS IN ALL OF THE LAYERS
if ((peccheck(1).eq.0).and.(peccheck(2).eq.0)) then
  aaepsr=aaepsr-(1d-8)*Ci
elseif (peccheck(1).eq.0) then
  aaepsr(1:aanoflyr)=aaepsr(1:aanoflyr)-(1d-8)*Ci
elseif (peccheck(2).eq.0) then
  aaepsr(0:aanoflyr-1)=aaepsr(0:aanoflyr-1)-(1d-8)*Ci
endif

do i=0,aanoflyr-1 !wavenumbers of each layer
  if (i.eq.0) then
    if (peccheck(1).eq.0) then
    aakkgamma(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    aakk(i)=sqrt(aakkgamma(i)**2-kgamma**2)
    else
    aakk(i)=0.0
    endif
  elseif (i.eq.aanoflyr-1) then
    if (peccheck(2).eq.0) then
    aakkgamma(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    aakk(i)=sqrt(aakkgamma(i)**2-kgamma**2)
    else
    aakk(i)=0.0
    endif
  else
    aakkgamma(i)=k00*sqrt(aaepsr(i)*aamur(i)) ! aakk(i):wavenumber of ith layer
    aakk(i)=sqrt(aakkgamma(i)**2-kgamma**2)
  endif
  if (dreal(aakk(i)).lt.0.0) then
    reflag=1  !the swps have imaginary part
    aakk(i)=-aakk(i)
    aakkgamma(i)=-aakkgamma(i)
  else
    reflag=1
  endif
  if ((dreal(aakk(i))**2).gt.aakmax) then
    aakmax=dreal(aakk(i))**2 ! Find the maximum k vector
  endif
enddo
aakmax=sqrt(aakmax)*kmaxmult


zconst=ydata
aaz=zconst
aazi=rimag(2)
aarho=xdata
aarhoi=rimag(1)
aarhoc=aarho+Ci*aarhoi
aazc=aaz+Ci*aazi

if (dreal(aadint).gt.0.0) then
  aadint=aadint*aakmax
else
  aadint=aakmax*1d-6
endif

!call residue_taker_kgamma
if ((dabs(aarhoi) .gt. 1d-8).or.(dabs(aazi).gt.1d-8)) then  ! if there is some complex origin
! in this version the k_gamma module only calculates the fields of real origin point sources.
    halfcyc=21
    ress=0.0
    call integrator2d_kgamma(halfcyc,ress) ! the complex origin part will come here
else
    halfcyc=21
    ress=0.0
    call integrator2d_kgamma(halfcyc,ress)
endif

if (aaiHEK.eq.0) then
aaA(3)=-Ci*2*pi*freq*(ress(1)+(ress(2)/((2*pi*freq)**2))) ! Hor Exx
aaA(2)=-Ci*2*pi*freq*(ress(5)+(ress(4)/((2*pi*freq)**2))) ! Ezx
aaA(1)=ress(3)/(Ci*2*pi*freq) ! Hor Eyx
aaA(4)=(ress(7)-ress(8))/aamur(aaolyr) !Hor Hyx
aaA(5)=-ress(9)/aamur(aaolyr) ! Hzx
aaA(6)=ress(6)/aamur(aaolyr) ! Hxx
    if (aaslyr.eq.aaolyr) then !direct term inclusion 
      distx=dcmplx(aarho,aarhoi)
      distz=dcmplx(aaz,aazi)
      dist=cdsqrt(distx**2+distz**2) !complex distance
      if (dreal(dist).lt.0.0) dist=-dist ! the real part of the distance must be positive
      if (dimag(aakk(aaslyr)).gt.0.0) then
        dist=-dist*aakk(aaslyr) ! argument of the hankel function
      else
        dist=dist*aakk(aaslyr) ! argument of the hankel function
      endif
      idumy1=2
      idumy2=20
      idumy3=14
      call ch2(dist,cBes(0),idumy1,idumy2,idumy3,ierr)
      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
      aaA(3)=aaA(3)-0.5*pi*freq*cBes(0)*(mue0*aamur(aaslyr)-(kgamma**2/(((2*pi*freq)**2)*eps0*aaepsr(aaslyr))))
      aaA(2)=aaA(2)+(1/(Ci*8*pi*freq*dist*eps0*aaepsr(aaslyr)))*kgamma*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1) 
      aaA(1)=aaA(1)+(1/(Ci*8*pi*freq*dist*eps0*aaepsr(aaslyr)))*kgamma*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1) 
      aaA(6)=aaA(6)
      aaA(5)=aaA(5)+(1/(4*Ci*dist))*aakk(aaslyr)*distx*cBes(1)*aakk(aaslyr)
      aaA(4)=aaA(4)-(1/(4*Ci*dist))*aakk(aaslyr)*distz*cBes(1)*aakk(aaslyr)
      endif
elseif (aaiHEK.eq.1) then
!Warning: Here epsilon and mu values are interchanged to convert from TE to TM. (by geomnormer)
    aaA(6)=-Ci*2*pi*freq*(ress(1)+(ress(2)/((2*pi*freq)**2))) ! Hor Exx
    aaA(5)=-Ci*2*pi*freq*(ress(5)+ress(4)/((2*pi*freq)**2)) ! Ezx
    aaA(4)=ress(3)/(Ci*2*pi*freq) ! Hor Eyx
    aaA(1)=(ress(7)-ress(8))/aamur(aaolyr) !Hor Hyx
    aaA(2)=-ress(9)/aamur(aaolyr) ! Hzx
    aaA(3)=ress(6)/aamur(aaolyr) ! Hxx
    if (aaslyr.eq.aaolyr) then !direct term inclusion
      distx=dcmplx(aarho,aarhoi)
      distz=dcmplx(aaz,aazi)
      dist=cdsqrt(distx**2+distz**2) !complex distance
      if (dreal(dist).lt.0.0) dist=-dist ! the real part of the distance must be positive
      if (dimag(aakk(aaslyr)).gt.0.0) then
        dist=-dist*aakk(aaslyr) ! argument of the hankel function
      else
        dist=dist*aakk(aaslyr) ! argument of the hankel function
      endif
      idumy1=2
      idumy2=20
      idumy3=14
      call ch2(dist,cBes(0),idumy1,idumy2,idumy3,ierr)
      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
      aaA(6)=aaA(6)-0.5*pi*freq*cBes(0)*(mue0*aamur(aaslyr)-(kgamma**2/(((2*pi*freq)**2)*eps0*aaepsr(aaslyr))))
      aaA(5)=aaA(5)+(1/(Ci*8*pi*freq*dist*eps0*aaepsr(aaslyr)))*kgamma*aakk(aaslyr)*aakk(aaslyr)*distz*cBes(1) 
      aaA(4)=aaA(4)+(1/(Ci*8*pi*freq*dist*eps0*aaepsr(aaslyr)))*kgamma*aakk(aaslyr)*aakk(aaslyr)*distx*cBes(1) 
      aaA(3)=aaA(3)
      aaA(2)=(aaA(2)+(1/(4*Ci*dist))*aakk(aaslyr)*distx*cBes(1)*aakk(aaslyr))
      aaA(1)=(aaA(1)-(1/(4*Ci*dist))*aakk(aaslyr)*distz*cBes(1)*aakk(aaslyr))
    endif
endif
  call normer(aaA,r,aaiHEK,1)
  rimag=rimag/aafreq_norm

endsubroutine twodlyrbase_kgamma

subroutine integrator2d_kgamma(halfcyc,ress)
Integer(4) halfcyc,counter,i,checker,attn
real*8 dummbess,a,b
real*8 krhor(1)
real*8 resltpos(35),resltneg(35)
complex*16 respos(9,halfcyc), resneg(9,halfcyc)
real*8 aagk(1),bbgk(1)
integer mm,iOK
complex*16 ress(9), resspos(9), ressneg(9)

attn=0
checker=0

aagk(1)=1d-6 ! the first bound of the integration (1e-6 to inf by aitken)

counter=0
do
  counter=counter+1
  a=dabs(aarho)
  b=counter*pi
  dummbess=b/a
  if (dummbess.gt.15000) then
  dummbess=15000
  attn=1
  endif
  if (dummbess.gt.aakmax) then
    exit
  endif
enddo
bbgk(1)=dummbess

call intgk(aaintegrands2d_kgamma,19,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
call intgk(aaintegrands2d_kgamma,19,krhor,1,-aagk,-bbgk,aakey,100000,aaepsrel,resltneg,mm,iOK)
respos(1,1)=dcmplx(resltpos(2),resltpos(11))
respos(2,1)=dcmplx(resltpos(3),resltpos(12))
respos(3,1)=dcmplx(resltpos(4),resltpos(13))
respos(4,1)=dcmplx(resltpos(5),resltpos(14))
respos(5,1)=dcmplx(resltpos(6),resltpos(15))
respos(6,1)=dcmplx(resltpos(7),resltpos(16))
respos(7,1)=dcmplx(resltpos(8),resltpos(17))
respos(8,1)=dcmplx(resltpos(9),resltpos(18))
respos(9,1)=dcmplx(resltpos(10),resltpos(19))
resneg(1,1)=dcmplx(resltneg(2),resltneg(11))
resneg(2,1)=dcmplx(resltneg(3),resltneg(12))
resneg(3,1)=dcmplx(resltneg(4),resltneg(13))
resneg(4,1)=dcmplx(resltneg(5),resltneg(14))
resneg(5,1)=dcmplx(resltneg(6),resltneg(15))
resneg(6,1)=dcmplx(resltneg(7),resltneg(16))
resneg(7,1)=dcmplx(resltneg(8),resltneg(17))
resneg(8,1)=dcmplx(resltneg(9),resltneg(18))
resneg(9,1)=dcmplx(resltneg(10),resltneg(19))

do i=2,halfcyc
  counter=counter+1
  aagk(1)=bbgk(1)
  if (attn.eq.1) then
    bbgk(1)=aagk(1)+2500
  else
    bbgk(1)=aagk(1)+pi/dabs(aarho)
  endif 
  call intgk(aaintegrands2d_kgamma,19,krhor,1,aagk,bbgk,aakey,100000,aaepsrel,resltpos,mm,iOK)
  respos(1,i)=respos(1,i-1)+dcmplx(resltpos(2),resltpos(11))
  respos(2,i)=respos(2,i-1)+dcmplx(resltpos(3),resltpos(12))
  respos(3,i)=respos(3,i-1)+dcmplx(resltpos(4),resltpos(13))
  respos(4,i)=respos(4,i-1)+dcmplx(resltpos(5),resltpos(14))
  respos(5,i)=respos(5,i-1)+dcmplx(resltpos(6),resltpos(15))
  respos(6,i)=respos(6,i-1)+dcmplx(resltpos(7),resltpos(16))
  respos(7,i)=respos(7,i-1)+dcmplx(resltpos(8),resltpos(17))
  respos(8,i)=respos(8,i-1)+dcmplx(resltpos(9),resltpos(18))
  respos(9,i)=respos(9,i-1)+dcmplx(resltpos(10),resltpos(19))

  call intgk(aaintegrands2d_kgamma,19,krhor,1,-aagk,-bbgk,aakey,100000,aaepsrel,resltneg,mm,iOK)
  resneg(1,i)=resneg(1,i-1)+dcmplx(resltneg(2),resltneg(11))
  resneg(2,i)=resneg(2,i-1)+dcmplx(resltneg(3),resltneg(12))
  resneg(3,i)=resneg(3,i-1)+dcmplx(resltneg(4),resltneg(13))
  resneg(4,i)=resneg(4,i-1)+dcmplx(resltneg(5),resltneg(14))
  resneg(5,i)=resneg(5,i-1)+dcmplx(resltneg(6),resltneg(15))
  resneg(6,i)=resneg(6,i-1)+dcmplx(resltneg(7),resltneg(16))
  resneg(7,i)=resneg(7,i-1)+dcmplx(resltneg(8),resltneg(17))
  resneg(8,i)=resneg(8,i-1)+dcmplx(resltneg(9),resltneg(18))
  resneg(9,i)=resneg(9,i-1)+dcmplx(resltneg(10),resltneg(19))
  if (dabs(resltpos(1)).lt.aaepsrel**2) then
    checker=1
    ress=(respos(:,i)-resneg(:,i))
    exit
  endif
enddo
if (checker.eq.0) then
  call aitkenvec2d_kgamma(respos,halfcyc,resspos)
  call aitkenvec2d_kgamma(resneg,halfcyc,ressneg)
  ress=resspos-ressneg
endif

ress(1)=ress(1)*mue0*aamur(aaolyr)/(4*pi)
ress(2)=ress(2)/(4*pi*eps0*aaepsr(aaolyr))
ress(3)=ress(3)/(4*pi*eps0*aaepsr(aaolyr))
ress(4)=ress(4)/(4*pi*eps0*aaepsr(aaolyr))
ress(5)=ress(5)*mue0*aamur(aaolyr)/(4*pi)
ress(6)=ress(6)*aamur(aaolyr)/(4*pi)
ress(7)=ress(7)*aamur(aaolyr)/(4*pi)
ress(8)=ress(8)*aamur(aaolyr)/(4*pi)
ress(9)=ress(9)*aamur(aaolyr)/(4*pi)

return
end subroutine integrator2d_kgamma

real*8 function aaintegrands2d_kgamma(krhor,outint)
real*8 outint(18)
complex*16 aaintdata(9)
real*8 zm,krhor
complex*16 krho,krho2
complex*16 aihor,cihor,mite,ampspg1,ampsmg1,ampte,amptm,krhodum,krhodum2
complex*16 rte(-1:mxlyr,-1:mxlyr),rtm(-1:mxlyr,-1:mxlyr)
complex*16 grte(-1:mxlyr,-1:mxlyr),grtm(-1:mxlyr,-1:mxlyr)
complex*16 y1g2,y2g2,c1ug2,c2ug2,c1dg2,c2dg2,y1g5,y2g5,y1g4,y2g4,c1ug5,c2ug5,c1dg5,c2dg5
complex*16 mitm,bihor,dihor,ampsptehor,ampsmtehor,ampsptmhor,ampsmtmhor
complex*16 divider, upprop, downprop


if(dabs(krhor).eq.0.0d0) krho=1.0d-6
if (dabs(krhor).lt.aakmax) then ! shift the path up by the halfsine function and get the bessel functions accordingly
  krho=krhor+Ci*aadint*dsin(pi*krhor/aakmax)
else
  krho=krhor
endif

call getaakz(krho)

call ampref(rte,rtm,grte,grtm,zm,ampte,amptm) ! amplitude and reflection coefficients

mite=1./(1-grte(aaslyr,aaslyr-1)*grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
mitm=1./(1-grtm(aaslyr,aaslyr-1)*grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*aadd(aaslyr)))
divider=1.0/(Ci*aakz(aaslyr))

krhodum=sign(1.0d0,krhor)*sqrt(kgamma**2+krhor**2)
krhodum2=krhodum**2
krho2=krho**2

if(aaslyr.eq.aaolyr) then
  upprop=cdexp(-Ci*aakz(aaslyr)*aaz)
  downprop=cdexp(Ci*aakz(aaslyr)*aaz)
  
  aihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grte(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah)) &
    &  +grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mite
  bihor=cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))*grtm(aaslyr,aaslyr+1)*(cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)-aah))-grtm(aaslyr,aaslyr-1) &
    & *cdexp(-Ci*aakz(aaslyr)*(aadd(aaslyr)+aah)))*mitm
  cihor=cdexp(-Ci*aakz(aaslyr)*aah)*grte(aaslyr,aaslyr-1)*(cdexp(-Ci*aakz(aaslyr)*aah)+grte(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mite
  dihor=cdexp(-Ci*aakz(aaslyr)*aah)*grtm(aaslyr,aaslyr-1)*(-cdexp(-Ci*aakz(aaslyr)*aah)+grtm(aaslyr,aaslyr+1) &
    & *cdexp(-Ci*aakz(aaslyr)*(2*aadd(aaslyr)-aah)))*mitm

  if (isnan(dreal(aihor)*dimag(aihor))) aihor=0.0
  if (isnan(dreal(bihor)*dimag(bihor))) bihor=0.0
  if (isnan(dreal(cihor)*dimag(cihor))) cihor=0.0
  if (isnan(dreal(dihor)*dimag(dihor))) dihor=0.0
!horizontal vector potential (parallel) related

aaintdata(1)=(aihor*downprop+cihor*upprop)*divider ! GAXX
aaintdata(7)=(Ci*aakz(aaslyr)*aihor*downprop-Ci*aakz(aaslyr)*cihor*upprop)*divider ! del(GAXX)/del(z)
aaintdata(9)=aaintdata(1) ! GAXX
!scalar potential related
y1g2=(aakz(aaslyr)**2*bihor+aakkgamma(aaslyr)**2*aihor)*downprop/krhodum2
y2g2=(aakkgamma(aaslyr)**2*cihor-aakz(aaslyr)**2*dihor)*upprop/krhodum2
aaintdata(2)=(y1g2+y2g2)*divider ! Gqx
aaintdata(3)=aaintdata(2) ! Gqx
aaintdata(4)=(Ci*aakz(aaslyr)*y1g2-Ci*aakz(aaslyr)*y2g2)/(Ci*aakz(aaslyr)) ! del(Gqx)/del(z) yukarda
!horizontal vector potential (normal) related
y1g5=(aihor+bihor)*aakz(aaslyr)*downprop/(krhodum2)
y2g5=(dihor-cihor)*aakz(aaslyr)*upprop/(krhodum2)
aaintdata(5)=-(y1g5+y2g5)*Ci*divider ! GAzx
aaintdata(6)=aaintdata(5) ! GAzx
aaintdata(8)=aaintdata(5) ! GAzx
else
  if (aaolyr.gt.aaslyr) then
  upprop=cdexp(-Ci*aakz(aaolyr)*aaz)
  !horizontal vector potential (parallel) related
    ampspg1=(1+grte(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampspg1*ampte*(-Ci*aakz(aaolyr)*upprop+grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*Ci*aakz(aaolyr)*cdexp(Ci*aakz(aaolyr)*aaz)))*divider
    aaintdata(9)=aaintdata(1)
  !scalar potential related
    ampsptehor=ampspg1
    ampsptmhor=(1-grtm(aaslyr,aaslyr-1)*cdexp(-Ci*aakz(aaslyr)*2*aah))*mitm
    c1ug2=ampsptmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakkgamma(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte
    c2ug2=-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))*aakz(aaslyr)*aakz(aaolyr)- &
      & aakkgamma(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))
    y2g2=-c2ug2*cdexp(Ci*aakz(aaolyr)*aaz)/krhodum2
    y1g2=-c1ug2*upprop/krhodum2
    aaintdata(2)=(y1g2+y2g2)*divider
    aaintdata(3)=aaintdata(2)
    y2g4=Ci*aakz(aaolyr)*y2g2
    y1g4=-Ci*aakz(aaolyr)*y1g2
    aaintdata(4)=(y1g4+y2g4)*divider
    !horizontal vector potential (normal) related
    c1ug5=(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsptmhor*amptm
    c2ug5=-(aamur(aaslyr)/aamur(aaolyr))*ampsptehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+ &
      & aadd(aaslyr)-aah))/aakz(aaslyr)-ampsptmhor*amptm*grtm(aaolyr,aaolyr+1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aadd(aaslyr)-aah))
    y2g5=-c2ug5*aakz(aaslyr)*cdexp(Ci*aakz(aaolyr)*aaz)/krhodum2
    y1g5=-c1ug5*aakz(aaslyr)*upprop/krhodum2
    aaintdata(5)=-(y1g5+y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  else
    !horizontal vector potential (parallel) related
    downprop=cdexp(Ci*aakz(aaolyr)*aaz)
    ampsmg1=(1+grte(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mite
    aaintdata(1)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(downprop+grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
    aaintdata(7)=((aamur(aaslyr)/aamur(aaolyr))*ampsmg1*ampte*(Ci*aakz(aaolyr)*downprop-grte(aaolyr,aaolyr-1)*Ci*aakz(aaolyr)*cdexp(-Ci*aakz(aaolyr)*(2*(zm+aah)+aaz))))*divider
    aaintdata(9)=aaintdata(1)
    !scalar potential related
    ampsmtehor=ampsmg1
    ampsmtmhor=(-1+grtm(aaslyr,aaslyr+1)*cdexp(-Ci*aakz(aaslyr)*2*(aadd(aaslyr)-aah)))*mitm
    c2dg2=-ampsmtmhor*amptm*aakz(aaslyr)*aakz(aaolyr)-aakkgamma(aaolyr)**2*(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte
    c1dg2=ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah))*aakz(aaslyr)*aakz(aaolyr)-aakkgamma(aaolyr)**2 &
      & *(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah))
    y2g2=-c2dg2*downprop/krhodum2
    y1g2=-c1dg2*cdexp(-Ci*aakz(aaolyr)*aaz)/krhodum2
    aaintdata(2)=(y1g2+y2g2)*divider
    aaintdata(3)=aaintdata(2)
    y2g4=Ci*aakz(aaolyr)*y2g2
    y1g4=-Ci*aakz(aaolyr)*y1g2
    aaintdata(4)=(y1g4+y2g4)*divider
    !horizontal vector potential (normal) related
    c2dg5=-(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)/aakz(aaslyr)-ampsmtmhor*amptm
    c1dg5=(aamur(aaslyr)/aamur(aaolyr))*ampsmtehor*ampte*aakz(aaolyr)*grte(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(zm+aah)) &
      & /aakz(aaslyr)-ampsmtmhor*amptm*grtm(aaolyr,aaolyr-1)*cdexp(-Ci*aakz(aaolyr)*2*(aah+zm))
    y2g5=-c2dg5*aakz(aaslyr)*downprop/krhodum2
    y1g5=-c1dg5*aakz(aaslyr)*cdexp(-Ci*aakz(aaolyr)*aaz)/krhodum2
    aaintdata(5)=-(y1g5+y2g5)*Ci*divider
    aaintdata(6)=aaintdata(5)
    aaintdata(8)=aaintdata(5)
  end if
end if

if (isnan(dreal(aaintdata(1))*dimag(aaintdata(1)))) aaintdata(1)=0.0
if (isnan(dreal(aaintdata(2))*dimag(aaintdata(2)))) aaintdata(2)=0.0
if (isnan(dreal(aaintdata(3))*dimag(aaintdata(3)))) aaintdata(3)=0.0
if (isnan(dreal(aaintdata(4))*dimag(aaintdata(4)))) aaintdata(4)=0.0
if (isnan(dreal(aaintdata(5))*dimag(aaintdata(5)))) aaintdata(5)=0.0
if (isnan(dreal(aaintdata(6))*dimag(aaintdata(6)))) aaintdata(6)=0.0
if (isnan(dreal(aaintdata(7))*dimag(aaintdata(7)))) aaintdata(7)=0.0
if (isnan(dreal(aaintdata(8))*dimag(aaintdata(8)))) aaintdata(8)=0.0
if (isnan(dreal(aaintdata(9))*dimag(aaintdata(9)))) aaintdata(9)=0.0


outint(1)=dreal(aaintdata(1)*cdexp(-Ci*krho*aarho))
outint(10)=dimag(aaintdata(1)*cdexp(-Ci*krho*aarho))
outint(7)=dreal(aaintdata(7)*cdexp(-Ci*krho*aarho))
outint(16)=dimag(aaintdata(7)*cdexp(-Ci*krho*aarho))
outint(9)=dreal(-Ci*krho*aaintdata(9)*cdexp(-Ci*krho*aarho))
outint(18)=dimag(-Ci*krho*aaintdata(9)*cdexp(-Ci*krho*aarho))
outint(2)=dreal(-(kgamma**2)*aaintdata(2)*cdexp(-Ci*krho*aarho))
outint(11)=dimag(-(kgamma**2)*aaintdata(2)*cdexp(-Ci*krho*aarho))
outint(3)=dreal(-kgamma*krho*aaintdata(3)*cdexp(-Ci*krho*aarho))
outint(12)=dimag(-kgamma*krho*aaintdata(3)*cdexp(-Ci*krho*aarho))
outint(4)=dreal(-Ci*kgamma*aaintdata(4)*cdexp(-Ci*krho*aarho))
outint(13)=dimag(-Ci*kgamma*aaintdata(4)*cdexp(-Ci*krho*aarho))
outint(5)=dreal(-Ci*kgamma*aaintdata(5)*cdexp(-Ci*krho*aarho))
outint(14)=dimag(-Ci*kgamma*aaintdata(5)*cdexp(-Ci*krho*aarho))
outint(6)=dreal(-kgamma*krho*aaintdata(6)*cdexp(-Ci*krho*aarho))
outint(15)=dimag(-kgamma*krho*aaintdata(6)*cdexp(-Ci*krho*aarho))
outint(8)=dreal(-(kgamma**2)*aaintdata(8)*cdexp(-Ci*krho*aarho))
outint(17)=dimag(-(kgamma**2)*aaintdata(8)*cdexp(-Ci*krho*aarho))
aaintegrands2d_kgamma=abs(dsqrt(cdabs(aaintdata(1))**2+cdabs(aaintdata(2))**2+cdabs(krhodum*aaintdata(5))**2))
end function aaintegrands2d_kgamma


subroutine aitkenvec2d_kgamma(y,n,ress)
Integer(4) n,i,j
complex*16 y(9,n),ress(9),q(n-2)
do j=1,9
do i=0,n-3
  if ((y(j,i+3)-2*y(j,i+2)+y(j,i+1)).eq.0.0) then
    q(i+1)=y(j,i+1)-(y(j,i+2)-y(j,i+1))**2/1
  else
    q(i+1)=y(j,i+1)-(y(j,i+2)-y(j,i+1))**2/(y(j,i+3)-2*y(j,i+2)+y(j,i+1))
  endif
enddo
ress(j)=q(n-2)
enddo
end subroutine aitkenvec2d_kgamma

END MODULE AALYR