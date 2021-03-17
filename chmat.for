c Copyright 2017, Christian Hafner
c
c This file is part of OpenMaXwell.
c
c OpenMaXwell is free software: you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation, either version 3 of the License, or
c (at your option) any later version.
c OpenMaXwell is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c You should have received a copy of the GNU General Public License
c along with OpenMaXwell.  If not, see <http://www.gnu.org/licenses/>.
c INTEGER AUXILIARIES

      integer(4) function idigit(irb,i)
c isolate i-th digit of irb (ordering of Lars)
      implicit none
      Integer(4) irb,i
      idigit=mod(int(irb/10**(6-i)),10)
      end

      integer(4) function iGetDigit(irb,i)
c isolate i-th digit of irb (ordering of CH)
      implicit none
      Integer(4) irb,i
      iGetDigit=mod(int(irb/10**(i-1)),10)
      end

c REAL AUXILIARIES

      Real(8) function TriAng2D(ra,rb,rc)
c compute the angle (degrees) at point ra of a 2D triangle with corners ra,rb,rc
      implicit none
      Real(8) ra(2),rb(2),rc(2),a,b,c,co
	a=(rb(1)-rc(1))**2+(rb(2)-rc(2))**2
	b=(rc(1)-ra(1))**2+(rc(2)-ra(2))**2
	c=(ra(1)-rb(1))**2+(ra(2)-rb(2))**2
	co=b+c-a
	if((b.lt.1.0d-300).or.(c.lt.1.0d-300)) then
	  TriAng2D=90.0d0
	  return
	end if
	a=2.0d0*dsqrt(b)*dsqrt(c)
	co=co/max(a,1.0d-300)
	if(co.gt.0.999999999999d0) then
	  TriAng2D=0.0d0
	else if(co.lt.-0.999999999999d0) then
	  TriAng2D=180.0d0
	else
	  TriAng2D=dacosD(co)
	end if
	end
	
	subroutine Triangle2D(ra,rb,rc,a,b,c,alpha,beta,gamma)
c compute the sides and the angles (degrees) at the corners of a 2D triangle with corners ra,rb,rc
      implicit none
      Real(8) ra(2),rb(2),rc(2),a,b,c,ao,bo,alpha,beta,gamma,a0
	a=(rb(1)-rc(1))**2+(rb(2)-rc(2))**2
	b=(rc(1)-ra(1))**2+(rc(2)-ra(2))**2
	c=(ra(1)-rb(1))**2+(ra(2)-rb(2))**2
	if(a.lt.1.0d-300) then
	  alpha=0.0d0
	  beta=90.0d0
	  gamma=90.0d0
	  return
	end if
	if(b.lt.1.0d-300) then
	  alpha=90.0d0
	  beta=0.0d0
	  gamma=90.0d0
	  return
	end if
	if(c.lt.1.0d-300) then
	  alpha=90.0d0
	  beta=90.0d0
	  gamma=0.0d0
	  return
	end if
	ao=b+c-a
	bo=c+a-b
	a=dsqrt(a)
	b=dsqrt(b)
	c=dsqrt(c)
	a0=2.0d0*b*c
	ao=ao/max(a0,1.0d-300)
	if(ao.gt.0.999999999999d0) then
	  alpha=0.0d0
	  beta=90.0d0
	  gamma=90.0d0
	else if(ao.lt.-0.999999999999d0) then
	  alpha=180.0d0
	  beta=0.0d0
	  gamma=0.0d0
	else
	  alpha=dacosD(ao)
	  if(alpha.gt.180.0d0) then
	    write(*,*) 'ao,alpha=',ao,alpha
	  end if
	  a0=2.0d0*c*a
	  bo=bo/max(a0,1.0d-300)
	  if(bo.gt.0.999999999999d0) then
	    beta=0.0d0
	    gamma=180.0d0-alpha
	  else if(bo.lt.-0.999999999999d0) then
	    alpha=0.0d0
	    beta=180.0d0
	    gamma=0.0d0
	  else
	    beta=dacosD(bo)
	    gamma=180.0d0-alpha-beta
	  end if
	end if
	end

      Real(8) function TriAng3D(ra,rb,rc)
c compute the angle (degrees) at point ra of a 3D triangle with corners ra,rb,rc
      implicit none
      Real(8) ra(3),rb(3),rc(3),a,b,c,co
	a=(rb(1)-rc(1))**2+(rb(2)-rc(2))**2+(rb(3)-rc(3))**2
	b=(rc(1)-ra(1))**2+(rc(2)-ra(2))**2+(rc(3)-ra(3))**2
	c=(ra(1)-rb(1))**2+(ra(2)-rb(2))**2+(ra(3)-rb(3))**2
	co=b+c-a
	if((b.lt.1.0d-300).or.(c.lt.1.0d-300)) then
	  TriAng3D=90.0d0
	  return
	end if
	a=2.0d0*dsqrt(b)*dsqrt(c)
	co=co/max(a,1.0d-300)
	if(co.gt.0.999999999999d0) then
	  TriAng3D=0.0d0
	else if(co.lt.-0.999999999999d0) then
	  TriAng3D=180.0d0
	else
	  TriAng3D=dacosD(co)
	end if
	end

      Real(8) function dsqrtc(dx)
c replaces intrinsic function dsqrt. important for compilers that
c cause errors if the arguments is close to zero.
      implicit none
      Real(8) dx
      if(dx.gt.1.0d-307) then
        dsqrtc=dsqrt(dx)
        return
      end if
      if(dx.gt.-1.0d-307) then
        dsqrtc=0.0d0
        return
      endif
      dsqrtc=dsqrt(-dx)
      end

      Real(8) function dintvl(a,b,n,i,l,iOK)
c return the location of the i-th point on the interval a...b with n points
c l=0: linear spacing of the points, l=1: logarithmic spacing
c iOK:error flag
      implicit none
      Real(8) a,b,a1,x
	Integer(4)  n,i,l,iOK
      iOK=0
      if(n.lt.2) then
        iOK=-1
        dintvl=0.5d0*(a+b)
        return
      end if
      if(l.ne.0) then
        if((a.lt.1.0d-307).or.(b.lt.1.0d-307)) then
          iOK=1
          dintvl=0.0d0
          return
        end if
        a1=dlog(a)
        x=a1+dble(i-1)*(dlog(b)-a1)/dble(n-1)
        dintvl=dexp(x)
      else
        dintvl=a+dble(i-1)*(b-a)/dble(n-1)
      end if
      end


c 3D VECTOR TRANSFORMATIONS

      subroutine vGlob2Loc(v,spac,vh)
c transformation of a real (position) vector v
c to a coordinate system spac (orthonormal new basis!)
      implicit none
      Real(8) v(3),vh(3),spac(3,0:3),d(3)
	d(1:3)=v(1:3)-spac(1:3,0)
	vh(1)=Dot_Product(d,spac(1:3,1))
	vh(2)=Dot_Product(d,spac(1:3,2))
	vh(3)=Dot_Product(d,spac(1:3,3))
      end

      subroutine cvGlob2Loc(v,spac,vh)
c transformation of a complex (field) vector v
c to a coordinate system spac (orthonormal new basis!)
      implicit none
      Real(8) spac(3,0:3),rr(3),ri(3),r(3)
      Complex(8) v(3),vh(3)
      r(1:3)=Dble(v(1:3))
	rr(1)=Dot_Product(r,spac(1:3,1))
	rr(2)=Dot_Product(r,spac(1:3,2))
	rr(3)=Dot_Product(r,spac(1:3,3))
      r(1:3)=DImag(v(1:3))
	ri(1)=Dot_Product(r,spac(1:3,1))
	ri(2)=Dot_Product(r,spac(1:3,2))
	ri(3)=Dot_Product(r,spac(1:3,3))
      vh(1:3)=DCmplx(rr(1:3),ri(1:3))
      end

      subroutine vLoc2Glob(v,spac,vh)
c transformation of a real (position) vector v
c form a coordinate system spac (orthonormal old basis!)
      implicit none
      Real(8) spac(3,0:3)
      Real(8) vh(3),v(3)
      vh(1:3)=spac(1:3,0)+v(1)*spac(1:3,1)+v(2)*spac(1:3,2)
     *                    +v(3)*spac(1:3,3)
      end

      subroutine rvLoc2Glob(v,spac,vh)
c transformation of a real (field) vector v
c form a coordinate system spac (orthonormal old basis!)
      implicit none
      Real(8) spac(3,0:3)
      Real(8) vh(3),v(3)
      vh(1:3)=v(1)*spac(1:3,1)+v(2)*spac(1:3,2)+v(3)*spac(1:3,3)
      end

      subroutine cvLoc2Glob(v,spac,vh)
c transformation of a complex (field) vector v
c form a coordinate system spac (orthonormal old basis!)
      implicit none
      Real(8) spac(3,0:3)
      Complex(8) vh(3),v(3)
      vh(1:3)=v(1)*spac(1:3,1)+v(2)*spac(1:3,2)+v(3)*spac(1:3,3)
      end

      subroutine sLoc2Glob(s,spac,sh)
c transformation of a real spac s
      implicit none
      Real(8) spac(3,0:3),sh(3,0:3),s(3,0:3)
	call vLoc2Glob(s(1:3,0),spac,sh(1:3,0))
	call rvLoc2Glob(s(1:3,1),spac,sh(1:3,1))
	call rvLoc2Glob(s(1:3,2),spac,sh(1:3,2))
	call rvLoc2Glob(s(1:3,3),spac,sh(1:3,3))
      end


c MATRIX-VECTOR OPERATIONS

      subroutine cMxcV(chmat,mmat,vin,kmax,imax,vout)
c vout=M*vin, where M is the rectangular MMP matrix M
c M is stored in the vector chmat with mmat elements
c the last column of M contains the right hand side.
c M has kmax columns and imax rows.
      implicit none
      Integer(4) mmat,kmax,imax,km1,i,k,ik
      Complex(8) vin(kmax),vout(imax),chmat(mmat),s
      km1=kmax-1
      ik=0
      do i=1,imax
        s=(0.0d0,0.0d0)
        do k=1,km1
          ik=ik+1
          s=s+chmat(ik)*vin(k)
        end do
        ik=ik+1
        vout(i)=s
      end do
      end

      subroutine cMTxcV(chmat,mmat,vin,kmax,imax,vout)
c vout=MT*vin, where MT is the adjoint of the rectangular MMP matrix M
c M is stored in the vector chmat with mmat elements
c the last column of M contains the right hand side.
c M has kmax columns and imax rows.
      implicit none
      Integer(4) mmat,kmax,imax,km1,i,k,ik,ik0
      Complex(8) vin(imax),vout(kmax),chmat(mmat),s
      km1=kmax-1
      ik0=-kmax
      do k=1,km1
        ik0=ik0+1
        ik=ik0
        s=(0.0d0,0.0d0)
        do i=1,imax
          ik=ik+kmax
          s=s+DConjg(chmat(ik))*vin(i)
        end do
        vout(k)=s
      end do
      end

      subroutine cVpRxcV(vin1,R,vin2,kmax,vout)
c vout = vin1 + R * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit Integer(4) (i-n)
      implicit Real(8) (a-h,o-z)
      Complex(8) vout(kmax),vin1(kmax),vin2(kmax)
      vout(1:kmax)=vin1(1:kmax)+R*vin2(1:kmax)
      end

      subroutine cVmRxcV(vin1,R,vin2,kmax,vout)
c vout = vin1 - R * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit none
      Integer(4) kmax
      Real(8) R
      Complex(8) vout(kmax),vin1(kmax),vin2(kmax)
      vout(1:kmax)=vin1(1:kmax)-R*vin2(1:kmax)
      end

      subroutine cVpCxcV(vin1,C,vin2,kmax,vout)
c vout = vin1 + C * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit Integer(4) (i-n)
      implicit Real(8) (a-h,o-z)
      Complex(8) vout(kmax),vin1(kmax),vin2(kmax),C
      vout(1:kmax)=vin1(1:kmax)+C*vin2(1:kmax)
      end

      subroutine cVmCxcV(vin1,C,vin2,kmax,vout)
c vout = vin1 - C * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit none
      Integer(4) kmax
      Complex(8) vout(kmax),vin1(kmax),vin2(kmax),C
      vout(1:kmax)=vin1(1:kmax)-C*vin2(1:kmax)
      end

      Real(8) Function cVnorm(v,kmax)
      implicit Integer(4) (i-n)
      implicit Real(8) (a-h,o-z)
      Complex(8) v(kmax),c
      c=Dot_Product(v(1:kmax),v(1:kmax))
	cVnorm=cdabs(c)
	end

c COMPLEX MATRIX ANALYSIS AND SCALING

      subroutine chchkM(chmat,mmat,kmax,imax,Norm,rscal,i0)
c analyze the rectangular matrix M stored in the vector chmat
c INPUT:
c mmat: length of vector chmat
c kmax: number of columns of M
c imax: number of rows of M
c Norm: vector norm for scaling: 0 no scaling, 1: abs Norm, 2: square Norm, 3: max Norm
c OUTPUT:
c rscal: scaling vector (kmax elements) for setting column norm = 1
c i0: number of zero elements
      implicit none
      Integer(4) mmat,kmax,imax,Norm,i0,ik0,ik,i,k
	Complex(8) chmat(mmat)
	Real(8) rscal(kmax),sum,amx0,am2
	Real(8), external:: dsqrtc
c compute squares of the norm of the columns and store in rscal
	i0=0
      ik0=-kmax
      do k=1,kmax
        ik0=ik0+1
        ik=ik0
        sum=0.0d0
        amx0=0.0d0
        do i=1,imax
          ik=ik+kmax
          am2=Dble(chmat(ik))**2+DImag(chmat(ik))**2
          if(am2.lt.1.0d-300) then
            chmat(ik)=(0.0d0,0.0d0)
	      am2=0.0d0
	      i0=i0+1
	    end if
          if(am2.gt.amx0) then
            amx0=am2
          end if
	    if(Norm.eq.1) then
            sum=sum+sqrt(am2)
	    else if(Norm.eq.2) then
            sum=sum+am2
	    end if
        end do
	  if(amx0.lt.1.0d-300) then
	    rscal(k)=0.0d0
	    Cycle
	  end if
	  if(Norm.eq.1) then
          rscal(k)=1.0d0/(1.0d-301+abs(sum))
	  else if(Norm.eq.2) then
          rscal(k)=1.0d0/(1.0d-301+dsqrtc(sum))
	  else if(Norm.gt.2) then
          rscal(k)=1.0d0/(1.0d-301+dsqrtc(amx0))
	  else
          rscal(k)=1.0d0
	  end if
      end do
      end

      subroutine chchkM2(chmat,kmax,imax,ld,Norm,rscal)
c analyze the rectangular matrix M stored in the matrix chmat
c INPUT:
c kmax: number of columns of M
c imax: number of rows of M
c Norm: vector norm for scaling: 0 no scaling, 1: abs Norm, 2: square Norm, 3: max Norm
c OUTPUT:
c rscal: scaling vector (kmax elements) for setting column norm = 1
      implicit none
      Integer(4) kmax,imax,ld,Norm,i0,i,k
	Complex(8) chmat(ld,kmax)
	Real(8) rscal(kmax),sum,amx0,am2
	Real(8), external:: dsqrtc
	i0=0
c compute squares of the norm of the columns and store in rscal
      do k=1,kmax
        sum=0.0d0
        amx0=0.0d0
        do i=1,imax
          am2=Dble(chmat(i,k))**2+DImag(chmat(i,k))**2
          if(am2.lt.1.0d-300) then
            chmat(i,k)=(0.0d0,0.0d0)
	      am2=0.0d0
	    end if
          if(am2.gt.amx0) then
            amx0=am2
          end if
	    if(Norm.eq.1) then
            sum=sum+sqrt(am2)
	    else if(Norm.eq.2) then
            sum=sum+am2
	    end if
        end do
	  if(amx0.lt.1.0d-300) then
	    rscal(k)=0.0d0
	    Cycle
	  end if
	  if(Norm.eq.1) then
          rscal(k)=1.0d0/(1.0d-301+abs(sum))
	  else if(Norm.eq.2) then
          rscal(k)=1.0d0/(1.0d-301+dsqrtc(sum))
	  else if(Norm.gt.2) then
          rscal(k)=1.0d0/(1.0d-301+dsqrtc(amx0))
	  else
          rscal(k)=1.0d0
	  end if
      end do
      end

      subroutine chsclM(mmat,kmax,imax,rscal,chmat)
c scale columns of the rectangular matrix M (set column norm = 1)
c INPUT:
c mmat: length of vector chmat
c kmax: number of columns
c imax: number of rows
c rscal: scaling vector (kmax elements) obtained from chchkM
c INPUT+OUTPUT:
c chmat: vector containing M
      implicit none
      Integer(4) mmat,kmax,imax,ik0,ik,i,k
      Real(8) rscal(kmax),sum
      Complex(8) chmat(mmat)
      ik0=-kmax
      do k=1,kmax
        ik0=ik0+1
        ik=ik0
        sum=rscal(k)
        do i=1,imax
          ik=ik+kmax
          chmat(ik)=chmat(ik)*sum
        end do
      end do
      end

      subroutine chsclM2(kmax,imax,ld,rscal,chmat)
c scale columns of the rectangular matrix M (set column norm = 1)
c INPUT:
c mmat: length of vector chmat
c kmax: number of columns
c imax: number of rows
c rscal: scaling vector (kmax elements) obtained from chchkM
c INPUT+OUTPUT:
c chmat: array containing M
      implicit none
      Integer(4) kmax,imax,ld,i,k
      Real(8) rscal(kmax),sum
      Complex(8) chmat(ld,kmax)
      do k=1,kmax
        sum=rscal(k)
        do i=1,imax
          chmat(i,k)=chmat(i,k)*sum
        end do
      end do
      end

      subroutine ch000M(mmat,kmax,imax,afac,chmat)
c set small elements of the matrix M = 0
      implicit none
      Integer(4) mmat,kmax,imax,i0,ik
	Real(8) afac,am
      Complex(8) chmat(mmat)
	i0=0
      do ik=1,imax*kmax
        am=Abs(chmat(ik))
        if(am.lt.afac) then
          chmat(ik)=(0.0d0,0.0d0)
	    i0=i0+1
	  end if
      end do
	write(*,*) 'ch000M number of zero elements=',i0
      end

      subroutine ch000M2(kmax,imax,ld,afac,chmat)
c set small elements of the matrix M = 0
      implicit none
      Integer(4) kmax,imax,ld,i0,i,k
	Real(8) afac,am
      Complex(8) chmat(ld,kmax)
	i0=0
      do i=1,imax
        do k=1,kmax
          am=Abs(chmat(i,k))
          if(am.lt.afac) then
            chmat(i,k)=(0.0d0,0.0d0)
	      i0=i0+1
	    end if
        end do
      end do
	write(*,*) 'ch000M2 number of zero elements=',i0
      end

      subroutine ch001M(mmat,kmax,imax,afac,chmat)
c set small elements of the matrix M = 0
      implicit none
      Integer(4) mmat,kmax,imax,i0,ik0,i,ik,k
	Real(8) afac,amx0,am2
      Complex(8) chmat(mmat)
c trace columns
	i0=0
      ik0=-kmax
      do k=1,kmax
        ik0=ik0+1
        ik=ik0
        amx0=0.0d0
        do i=1,imax
          ik=ik+kmax
          am2=Dble(chmat(ik))**2+DImag(chmat(ik))**2
          if(am2.gt.amx0) then
            amx0=am2
          end if
        end do
        amx0=amx0*afac
        ik=ik0
        do i=1,imax
          ik=ik+kmax
          am2=Dble(chmat(ik))**2+DImag(chmat(ik))**2
          if(am2.lt.amx0) then
            chmat(ik)=(0.0d0,0.0d0)
	      i0=i0+1
	    end if
        end do
      end do
	write(*,*) 'ch001M number of zero elements=',i0
      end

      subroutine ch002M(mmat,kmax,imax,afac,chmat)
c set small elements of the matrix M = 0
      implicit none
      Integer(4) mmat,kmax,imax,i0,ik0,i,ik,k
	Real(8) afac,amx0,am2
      Complex(8) chmat(mmat)
c trace rows
	i0=0
      ik0=-kmax
      do i=1,imax
        ik0=ik0+kmax
        ik=ik0
        amx0=0.0d0
        do k=1,kmax
          ik=ik+1
          am2=Dble(chmat(ik))**2+DImag(chmat(ik))**2
          if(am2.gt.amx0) then
            amx0=am2
          end if
        end do
        amx0=amx0*afac
        ik=ik0
        do k=1,kmax
          ik=ik+1
          am2=Dble(chmat(ik))**2+DImag(chmat(ik))**2
          if(am2.lt.amx0) then
            chmat(ik)=(0.0d0,0.0d0)
	      i0=i0+1
	    end if
        end do
      end do
	write(*,*) 'ch002M number of zero elements=',i0
      end


c COMPLEX GIVENS ALGORITHM

      subroutine cchud(r,x,km,kr,nupd,c,s,mam)
c updating of the trapezoidal matrix r stored in a vector
c x: vector to be updated
c km: number of columns of the triangular part of r
c kr: number of right hand sides (number of columns of the rectangular part)
c nupd: number of rows to be updated
c c,s: auxiliary vectors of length >=km+1
c mam: length of r
      implicit none
      Integer(4) km,kr,nupd,mam,km1,ir,i,j,jm1,nz
	Real(8) ci
      Real(8) c(km+1)
      Complex(8) r(mam),x(km+kr),s(km+1),si,ri,xj
      km1=km+1
      ir=1
      do j=1,km1
        xj=x(j)
        jm1=j-1
        nz=min(jm1,nupd)
        do i=1,nz
          ci=c(i)
          si=s(i)
          ri=r(ir)
          r(ir)=ci*ri+si*xj
          xj=ci*xj-DConjg(si)*ri
          ir=ir+1
        end do
        if(nz.lt.jm1) ir=ir+jm1-nz
        call crotg(r(ir),xj,c(j),s(j))
        ir=ir+1
      end do
      nz=min(km1,nupd)
      do j=km1+1,km+kr
        xj=x(j)
        do i=1,nz
          ci=c(i)
          si=s(i)
          ri=r(ir)
          r(ir)=ci*ri+si*xj
          xj=ci*xj-DConjg(si)*ri
          ir=ir+1
        end do
        if(nz.lt.km1) ir=ir+km1-nz
      end do
      end

      subroutine crotg(a,b,c,s)
c givens plane rotations
      implicit none
	Real(8) c,sa,sb,q,r
	Complex(8) a,b,s
	Real(8), external:: dsqrtc
      sa=cdabs(a)
      if(sa.lt.1.0d-306) then
        c=0.0d0
        s=(1.0d0,0.0d0)
        a=b
        return
	end if
      sb=cdabs(b)
      if(sb.lt.1.0d-306) then
        c=1.0d0
        s=(0.0d0,0.0d0)
        return
	end if
      if(sa.gt.sb) then
        q=sb/sa
        r=sa*dsqrtc(1.0d0+q**2)
      else
        q=sa/sb
        r=sb*dsqrtc(1.0d0+q**2)
      endif
      a=a/sa
      c=sa/r
      s=a*DConjg(b)/r
      a=a*r
      end

      subroutine cchsl(r,x,km,kr,kmm,mam)
c solution of (upper triangular) system of equations
c r: augmented matrix stored in a vector
c x: matrix containing the solution(s)
c km: columns of matrix (excluding right hand sides)
c kr: number of right hand sides
c kmm: leading dimension of x
c mam: length of matrix r
      implicit none
      Integer(4) km,kr,kmm,mam,km1,ikm,ikr0,ikr,ik,ir,i,k
	Real(8) de
      Complex(8) r(mam),x(kmm,kr),sum
      km1=km+1
      ikm=(km*km1)/2
      ikr0=ikm
      do ir=1,kr
        ikr0=ikr0+km1
        ikr=ikr0
        do i=km,1,-1
          ikr=ikr-1
          sum=r(ikr)
          ik=ikm+i
          do k=km,i+1,-1
            ik=ik-k
            sum=sum+r(ik)*x(k,ir)
          end do
          ik=ik-i
          de=dmax1(cdabs(sum)*1.0d-307,1.0d-307)
          if(cdabs(r(ik)).gt.de) then
            x(i,ir)=-sum/r(ik)
          else
            x(i,ir)=(1.0d0,0.0d0)
          end if
        end do
      end do
      end

c REAL GIVENS ALGORITHM

      subroutine dchud(r,x,km,kr,nupd,c,s,mam)
c updating of the trapezoidal matrix r stored in a vector
c x: vector to be updated
c km: number of columns of the triangular part of r
c kr: number of right hand sides (number of columns of the rectangular part)
c nupd: number of rows to be updated
c c,s: auxiliary vectors of length >=km+1
c mam: length of r
      implicit none
      Integer(4) km,kr,nupd,mam,km1,ir,j,i,jm1,nz
	Real(8) ci,si,rir,xj,r(mam),x(km+kr),c(km+1),s(km+1)
      km1=km+1
      ir=1
      do j=1,km1
        xj=x(j)
        jm1=j-1
        nz=min(jm1,nupd)
        do i=1,nz
          ci=c(i)
          si=s(i)
          rir=r(ir)
          r(ir)=ci*rir+si*xj
          xj=ci*xj-si*rir
          ir=ir+1
        end do
        if(nz.lt.jm1) ir=ir+jm1-nz
        call drotg(r(ir),xj,c(j),s(j))
        ir=ir+1
      end do
      nz=min(km1,nupd)
      do j=km1+1,km+kr
        xj=x(j)
        do i=1,nz
          ci=c(i)
          si=s(i)
          rir=r(ir)
          r(ir)=ci*rir+si*xj
          xj=ci*xj-si*rir
          ir=ir+1
        end do
        if(nz.lt.km1) ir=ir+km1-nz
      end do
      end

      subroutine drotg(a,b,c,s)
c givens plane rotations for dchud
      implicit none
      Real(8) a,b,c,s,r,q,sa,sb
	Real(8), external:: dsqrtc
      sa=dabs(a)
      if(sa.lt.1.0d-306) then
        c=0.0d0
        s=1.0d0
        a=b
        return
      end if
      sb=dabs(b)
      if(sb.lt.1.0d-306) then
        c=1.0d0
        s=0.0d0
        return
      end if
      if(sa.gt.sb) then
        q=sb/sa
        r=sa*dsqrtc(1.0d0+q**2)
      else
        q=sa/sb
        r=sb*dsqrtc(1.0d0+q**2)
      end if
      a=a/sa
      c=sa/r
      s=(a*b)/r
      a=a*r
      end

      subroutine dchsl(r,x,km,kr,kmm,mam)
c solution of (upper triangular) system of equations
c r: augmented matrix stored in a vector
c x: matrix containing the solution(s)
c km: columns of matrix (excluding right hand sides)
c kr: number of right hand sides
c kmm: leading dimension of x
c mam: length of matrix r
      implicit none
	Integer(4) km,kr,kmm,mam,km1,ikm,ikr0,ir,i,ikr,ik,k
      Real(8) sum,de,r(mam),x(kmm,kr)
      km1=km+1
      ikm=(km*km1)/2
      ikr0=ikm
      do ir=1,kr
        ikr0=ikr0+km1
        ikr=ikr0
        do i=km,1,-1
          ikr=ikr-1
          sum=r(ikr)
          ik=ikm+i
          do k=km,i+1,-1
            ik=ik-k
            sum=sum+r(ik)*x(k,ir)
          end do
          ik=ik-i
          de=dmax1(dabs(sum)*1.0d-307,1.0d-307)
          if(dabs(r(ik)).gt.de) then
            x(i,ir)=-sum/r(ik)
          else
            x(i,ir)=-dabs(sum)/de
          end if
        end do
      end do
      end

C Complex Cholesky

      subroutine cpngad(cap,cin,n,mam)
c adds exterior product of cin to normal equations cap
c cap is a columnwise packed upper triangular matrix
      implicit none
      Integer(4) n,mam,icap,j,i
      Complex(8) cap(mam),cin(n)
      icap=1
c     i: row; j: column
      do j=1,n
        if((dabs(Dble(cin(j))).gt.1.0d-300).or.
     *   (dabs(Dimag(cin(j))).gt.1.0d-300))then
          do i=1,j
          cap(icap)=cap(icap)+DConjg(cin(i))*cin(j)
          icap=icap+1
          end do
        else
          icap=icap+j
        endif
      end do
      end

      subroutine cdppfa(cap,n,info,mam)
c simple cholesky factorization of (columnwise upper triangular) packed
c symmetric matrix
      implicit none
      Integer(4) n,info,mam,jj,j,jm1,k,kk,kj,idum
	Real(8) s,t
      Complex(8) cap(mam),ct
      jj=0
      do j=1,n
        info=j
        s=0.0d0
        jm1=j-1
        kj=jj
        kk=0
        if(jm1.gt.0) then
          kj=kj+1
          ct=cap(kj)
          kk=kk+1
	    cap(kj)=ct/cap(kk)
          s=s+Dble(cap(kj))**2+DImag(cap(kj))**2
          do k=2,jm1
            kj=kj+1
            idum=k-1
            call cddotc(idum,cap(kk+1),cap(jj+1),ct)
            ct=cap(kj)-ct
            kk=kk+k
	      cap(kj)=ct/cap(kk)
            s=s+Dble(cap(kj))**2+DImag(cap(kj))**2
          end do
        end if
        jj=jj+j
        s=Dble(cap(jj))-s
        if((s.le.0.0d0).or.(dabs(Dimag(cap(jj))).gt.1.0d-300)) return
	  t=DImag(cap(jj))
        cap(jj)=DCmplx(dsqrt(s),t)
      end do
      info=0
      end

      subroutine cddotc(n,cx,cy,cd)
c blas dot product for complex vectors
      implicit none
      Integer(4) n,i
      Complex(8) cx(n),cy(n),cd
      cd=(0.0d0,0.0d0)
      do i=1,n
        cd=cd+DConjg(cx(i))*cy(i)
      end do
      end


c COMPLEX CG ALGORITHMS

      subroutine chCG(chmat,mmat,kmax,imax,itm,iacc,res2max,xi,si,pi,
     1                ri,qi,sr2,ite,iErr)
c perform CG iterations on a rectangular matrix M, stored in the 1D array chmat
c INPUT+OUTPUT:
c chmat: vector containing the rectangular matrix M
c INPUT:
c mmat: length of vector chmat
c kmax: number of columns
c imax: number of rows
c itm : stop when itm iterations were performed
c iacc: stop when relative residual decay is less then 10**-iacc
c res2max: stop when the sum of the squares of the residuals is less then res2max
c INPUT+OUTPUT:
c xi: parameter vector to be modified by the CG iterations
c AUXILIARY ARRAYS:
c si,pi,ri,qi
c OUTPUT:
c sr2: sum of the squares of the residuals
c ite: number of iterations performed
c iErr: error/warning flag
      implicit none
      Integer(4) mmat,kmax,imax,itm,iacc,ite,iErr,it
      Real(8) res2max,sr2,sr1,acc,ci
      Complex(8) xi(kmax),si(kmax),pi(kmax),ri(imax),qi(imax),
     1          chmat(mmat)
	Real(8), external:: cVnorm
	ite=0
      call chCGini(chmat,mmat,kmax,imax,xi,ri,si,pi,qi,ci,iErr)
      sr2=cVnorm(ri,imax)
      if(sr2.le.res2max) then
	  iErr=0
        return
	end if
      if(iErr.ne.0) return
	acc=10.0d0**(-iacc)
	sr1=sr2
      do it=1,itm
	  ite=it
        call chCGite(chmat,mmat,kmax,imax,xi,ri,si,pi,qi,ci,iErr)
        sr2=cVnorm(ri,imax)
        if(sr2.le.res2max) then
	    iErr=0
          return
	  end if
        if((abs(sr2-sr1).lt.acc*abs(sr2)).or.(sr2.gt.sr1)) then
	    iErr=-3
          return
	  end if
        if(iErr.ne.0) return
	  sr1=sr2
      end do
	iErr=-2
      end

      subroutine chCGini(chmat,mmat,kmax,imax,x0,r0,s0,p0,q0,c0,iErr)
c auxiliary for chCG: initialize
      implicit none
      Integer(4) mmat,kmax,imax,iErr,km1,ik,i,k
      Real(8) c0
      Complex(8) x0(kmax),s0(kmax),p0(kmax),r0(imax),
     1 q0(imax),chmat(mmat)
	Real(8), external:: cVnorm
	iErr=0
      km1=kmax-1
	x0(kmax)=(0.0d0,0.0d0)
      call cMxcV(chmat,mmat,x0,kmax,imax,r0)
      ik=0
      do i=1,imax
        ik=ik+kmax
        r0(i)=-r0(i)-chmat(ik)
      end do
      call cMTxcV(chmat,mmat,r0,kmax,imax,s0)
      do k=1,km1
        p0(k)=s0(k)
      end do
	p0(kmax)=(0.0d0,0.0d0)
      call cMxcV(chmat,mmat,p0,kmax,imax,q0)
      c0=cVnorm(s0,km1)
      if(c0.lt.1.0d-300) iErr=-1
      end

      subroutine chCGite(chmat,mmat,kmax,imax,xi,ri,si,pi,qi,ci,iErr)
c auxiliary for chCG: iterate
      implicit none
      Integer(4) mmat,kmax,imax,iErr,km1
      Real(8) qNrm2,ci,ai,ci1,bi
      Complex(8) xi(kmax),si(kmax),pi(kmax),ri(imax),qi(imax),
     1          chmat(mmat)
	Real(8), external:: cVnorm
	iErr=0
      km1=kmax-1
      qNrm2=cVnorm(qi,imax)
      if(qNrm2.lt.1.0d-300) then
	  iErr=1
	  return
	end if
      ai=ci/qNrm2
      call cVpRxcV(xi,ai,pi,km1,xi)
      call cVmRxcV(ri,ai,qi,imax,ri)
      call cMTxcV(chmat,mmat,ri,kmax,imax,si)
	si(kmax)=(0.0d0,0.0d0)
      ci1=ci
      ci=cVnorm(si,km1)
      if(ci.lt.1.0d-300) then
	  iErr=-1
	  return
	end if
      bi=ci/ci1
      call cVpRxcV(si,bi,pi,km1,pi)
	pi(kmax)=(0.0d0,0.0d0)
      call cMxcV(chmat,mmat,pi,kmax,imax,qi)
      end

      subroutine cMxcV2(chmat,mmat,vin,kmax,imax,vout)
c vout=M*vin, where M is the rectangular MMP matrix M
c M is stored in the vector chmat with mmat elements
c the last column of M contains the right hand side.
c M has kmax columns and imax rows.
      implicit none
      Integer(4) mmat,kmax,imax,km1,ik,i,k
      Real(8) s1,s2,vin(2,kmax),vout(2,imax),chmat(2,mmat)
      km1=kmax-1
      ik=0
      do i=1,imax
        s1=0.0d0
        s2=0.0d0
        do k=1,km1
          ik=ik+1
          s1=s1+chmat(1,ik)*vin(1,k)-chmat(2,ik)*vin(2,k)
          s2=s2+chmat(1,ik)*vin(2,k)+chmat(2,ik)*vin(1,k)
        end do
        ik=ik+1
        vout(1,i)=s1
        vout(2,i)=s2
      end do
      end

      subroutine cMTxcV2(chmat,mmat,vin,kmax,imax,vout)
c vout=MT*vin, where MT is the adjoint of the rectangular MMP matrix M
c M is stored in the vector chmat with mmat elements
c the last column of M contains the right hand side.
c M has kmax columns and imax rows.
      implicit none
      Integer(4) mmat,kmax,imax,km1,ik0,ik,i,k
      Real(8) s1,s2,vin(2,imax),vout(2,kmax),chmat(2,mmat)
      km1=kmax-1
      ik0=-kmax
      do k=1,km1
        ik0=ik0+1
        ik=ik0
        s1=0.0d0
        s2=0.0d0
        do i=1,imax
          ik=ik+kmax
          s1=s1+chmat(1,ik)*vin(1,i)+chmat(2,ik)*vin(2,i)
          s2=s2+chmat(1,ik)*vin(2,i)-chmat(2,ik)*vin(1,i)
        end do
        vout(1,k)=s1
        vout(2,k)=s2
      end do
      end

      subroutine cVpRxcV2(vin1,R,vin2,kmax,vout)
c vout = vin1 + R * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit none
      Integer(4) kmax,k
      Real(8) R,vout(2,kmax),vin1(2,kmax),vin2(2,kmax)
      do k=1,kmax
        vout(1,k)=vin1(1,k)+R*vin2(1,k)
        vout(2,k)=vin1(2,k)+R*vin2(2,k)
      end do
      end

      subroutine cVmRxcV2(vin1,R,vin2,kmax,vout)
c vout = vin1 - R * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit none
      Integer(4) kmax,k
      Real(8) R,vout(2,kmax),vin1(2,kmax),vin2(2,kmax)
      do k=1,kmax
        vout(1,k)=vin1(1,k)-R*vin2(1,k)
        vout(2,k)=vin1(2,k)-R*vin2(2,k)
      end do
      end

      subroutine cVpCxcV2(vin1,C,vin2,kmax,vout)
c vout = vin1 + C * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit none
      Integer(4) kmax,k
      Real(8) vout(2,kmax),vin1(2,kmax),vin2(2,kmax),C(2)
      do k=1,kmax
        vout(1,k)=vin1(1,k)+C(1)*vin2(1,k)-C(2)*vin2(2,k)
        vout(2,k)=vin1(2,k)+C(1)*vin2(2,k)+C(2)*vin2(1,k)
      end do
      end

      subroutine cVmCxcV2(vin1,C,vin2,kmax,vout)
c vout = vin1 - C * vin2
c kmax: length of the complex vectors vin1,vin2,vout
      implicit none
      Integer(4) kmax,k
      Real(8) vout(2,kmax),vin1(2,kmax),vin2(2,kmax),C(2)
      do k=1,kmax
        vout(1,k)=vin1(1,k)-C(1)*vin2(1,k)+C(2)*vin2(2,k)
        vout(2,k)=vin1(2,k)-C(1)*vin2(2,k)-C(2)*vin2(1,k)
      end do
      end


c PET subroutines

      subroutine extra(a,c,s,z,nord,npts,nfun,ifun,args,x,f,w,x0,f0,ier)
c extrapolation with nord functions of nfun real functions fi(x), x real
c fi is known in npts points (stored in the array x)
c f: array containing the fi in the points x in the following order:
c    f1(x1),f2(x1)...fn(x1),f1(x2),...
c w: weighting function stored in an array
c reslt: f0 = estimated value of f in a given point x0
c ier: error flag
      implicit none
      Integer(4) nord,npts,nfun,ifun(nord+1),ier,km,km1,ma,i,k,l
      Real(8) xi,x0,fv,x(npts),f(npts*nfun),w(npts),f0(nfun),
     1 a((nord+1)*(nord+2)/2+(nord+2)*nfun),z((nord+1)*nfun),
     2 c(nord+3),s(nord+3),args(2*nord+2)
	Real(8), external:: fval
      character st*128
      ier=1
      if(npts.le.nord) return
      ier=0
      km=nord+1
      km1=km+1
      ma=(km1*km)/2+nfun*km1
c reset matrix a
      a(1:ma)=0.0d0
c compute and update matrix a
      do i=1,npts
        xi=(x(i)-x(npts))/(x0-x(npts))
        do k=1,km
c value of basis function
          z(k)=w(i)*fval(ifun(k),args(k*2-1),args(k*2),xi,st)
        end do
        do l=1,nfun
          z(km+l)=-w(i)*f(l+(i-1)*nfun)
        end do
        call dchud(a,z,km,nfun,i,c,s,ma)
      end do
c solve a*z=e
      call dchsl(a,z,km,nfun,km,ma)
c estimate f0=z(1)*ifun(1)(arg1,arg2)+....z(nord)*ifun(nord)(arg1,arg2)
      do l=1,nfun
        f0(l)=0.0d0
      end do
      do i=1,km
        xi=1.0d0
        fv=fval(ifun(i),args(i*2-1),args(i*2),xi,st)
        do l=1,nfun
          f0(l)=f0(l)+z(i+(l-1)*km)*fv
        end do
      end do
      end

      Real(8) function fval(index,arg1,arg2,x,s)
      implicit none
      Integer(4) index,n
      Real(8) arg1,arg2,x,a
      character s*128
      goto(1,2,3,4,5,6,7,8,9,10,11,12) iabs(index)
      n=nint(arg2)
      if(n.eq.0) then
        fval=1.0d0
        write(s,'(a)') 'y=1'
      else
        fval=1.0d0+x**n
        write(s,'(a,i3)') 'y=1+x**',n
      end if
      goto 99
    1 n=nint(20.0d0*arg2)
      a=0.1d0+9.9d0*arg1
      if(n.eq.0) then
        fval=1.0d0
        if(index.lt.0) write(s,'(a)') 'y=1'
      else
        fval=1.0d0/(1.0d0+a*x)**n
        if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1    'y=1/(1+',a,'*x)**',n
      end if
      goto 99
    2 n=nint(20.0d0*arg2)+1
      a=0.1d0+9.9d0*arg1
      fval=(1.0d0-x/a)**n
      if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1  'y=(1-x/',a,')**',n
      goto 99
    3 n=nint(20.0d0*arg2)
      a=0.1d0+9.9d0*arg1
      if(n.eq.0) then
        fval=1.0d0
        if(index.lt.0) write(s,'(a)') 'y=1'
      else
        fval=2.0d0-1.0d0/(1.0d0+a*x)**n
        if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1    'y=2-1/(1+',a,'*x)**',n
      end if
      goto 99
    4 n=nint(20.0d0*arg2)+1
      a=0.1d0+9.9d0*arg1
      fval=2.0d0-(1.0d0-x/a)**n
      if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1  'y=2-(1-x/',a,')**',n
      goto 99
    5 n=nint(20.0d0*arg2)+1
      a=0.9d0*arg1
      fval=(1.0d0-a*x)**n
      if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1  'y=(1-',a,'*x)**',n
      goto 99
    6 n=nint(20.0d0*arg2)+1
      a=0.9d0*arg1
      fval=2.0d0-(1.0d0-a*x)**n
      if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1  'y=2-(1-',a,'*x)**',n
      goto 99
    7 n=nint(20.0d0*arg2)+1
      a=9.0d0*arg1
      fval=((1.0d0+a*x)/(1.0d0+a))**n
      if(index.lt.0) write(s,'(2(a,1pe10.2),a,i3)')
     1  'y=((1+',a,'*x)/',1.0d0+a,')**',n
      goto 99
    8 n=nint(20.0d0*arg2)+1
      a=9.0d0*arg1
      fval=1.0d0+1.0d0/(1.0d0+a)**n-((1.0d0+a*x)/(1.0d0+a))**n
      if(index.lt.0) write(s,'(3(a,1pe10.2),a,i3)')
     1 'y=',1.0d0+1.0d0/(1.0d0+a)**n,'-((1+',a,'*x)/',1.0d0+a,')**',n
      goto 99
    9 n=nint(20.0d0*arg2)+1
      a=9.0d0*arg1
      fval=1.0d0/(1.0d0+a*x)**n
      if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1   'y=1/(1+',a,'*x)**',n
      goto 99
   10 n=nint(20.0d0*arg2)+1
      a=9.0d0*arg1
      fval=2.0d0-1.0d0/(1.0d0+a*x)**n
      if(index.lt.0) write(s,'(a,1pe10.2,a,i3)')
     1  'y=2-1/(1+',a,'*x)**',n
      goto 99
   11 n=nint(20.0d0*arg2)+1
      a=0.9d0*arg1
      fval=((1.0d0-a)/(1.0d0-a*x))**n
      if(index.lt.0) write(s,'(2(a,1pe10.2),a,i3)')
     1   'y=(',1.0d0-a,'/(1-',a,'*x))**',n
      goto 99
   12 n=nint(20.0d0*arg2)+1
      a=0.9d0*arg1
      fval=1.0d0+(1.0d0-a)**n-((1.0d0-a)/(1.0d0-a*x))**n
      if(index.lt.0) write(s,'(3(a,1pe10.2),a,i3)')
     1 'y=',1.0d0+(1.0d0-a)**n,'-(',1.0d0-a,'/(1-',a,'*x))**',n
      goto 99
   99 continue
      end

c generate array of variables

      subroutine genVAR(mV,nxV,nyV,lxV,lyV,nV,cV,iOK)
      implicit Integer(4) (i-n)
      implicit Real(8) (a-h,o-z)
      dimension cV(2,mV)
      nV=nxV*nyV
      iOK=1
      if(nV.gt.mV) then
        write(*,*) 'genVAR: nV>mV nV,mV=',nV,mV
        return
      end if
      iOK=2
      if(nV.lt.2) then
        write(*,*) 'genVAR: nV<2 nV=',nV
        return
      end if
      iOK=3
      if((lxV.eq.1).and.(nxV.gt.2)) then
        if((cV(1,1).lt.1.0D-307).or.(cV(1,2).lt.1.0D-307)) then
          write(*,*) 'genVAR: neg.xV, log.scale cV=',cV(1,1),cV(1,2)
          return
        end if
        xll=dlog(cV(1,1))
        xur=dlog(cV(1,2))
      else
        xll=cV(1,1)
        xur=cV(1,2)
      end if
      if((lyV.eq.1).and.(nyV.gt.2)) then
        if((cV(2,1).lt.1.0D-307).or.(cV(2,2).lt.1.0D-307)) then
          write(*,*) 'genVAR: neg.yV, log.scale cV=',cV(2,1),cV(2,2)
          return
        end if
        yll=dlog(cV(2,1))
        yur=dlog(cV(2,2))
      else
        yll=cV(2,1)
        yur=cV(2,2)
      end if
      iOK=0
      if(nxV.eq.1) then
        xm=0.5d0*(xll+xur)
        dy=(yur-yll)/dble(nyV-1)
        y=yll-dy
        do i=1,nyV
          y=y+dy
          cV(1,i)=xm
          if((lyV.eq.1).and.(nyV.gt.2)) then
            cV(2,i)=dexp(y)
          else
            cV(2,i)=y
          end if
        end do
      else
        dx=(xur-xll)/dble(nxV-1)
        x=xll-dx
        if(nyV.eq.1) then
          ym=0.5d0*(yll+yur)
          do i=1,nxV
            x=x+dx
            if((lxV.eq.1).and.(nxV.gt.2)) then
              cV(1,i)=dexp(x)
            else
              cV(1,i)=x
            end if
            cV(2,i)=ym
          end do
        else
          dy=(yur-yll)/dble(nyV-1)
          j=0
          do i=1,nxV
            x=x+dx
            y=yll-dy
            do k=1,nyV
              y=y+dy
              j=j+1
              if((lxV.eq.1).and.(nxV.gt.2)) then
                cV(1,j)=dexp(x)
              else
                cV(1,j)=x
              end if
              if((lyV.eq.1).and.(nyV.gt.2)) then
                cV(2,j)=dexp(y)
              else
                cV(2,j)=y
              end if
            end do
          end do
        end if
      end if
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c numerical integration of several functions with several arguments

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine inthp(fun,nfun,var,ivar,aa,bb,intvl,ifcm,er,reslt,m,
     1                   iOK)
      implicit Real(8) (a-h,o-z)
      parameter(mfun=20)
      logical lStopInt,lAskQ
      dimension reslt(mfun)
      dimension var(1),aa(1),bb(1),intvlA(1),ifcmA(1)
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      ifcmA(1)=ifcm
      intvlA(1)=intvl
      call inthpA(fun,nfun,var,ivar,aa,bb,intvlA,ifcmA,er,reslt,m,iOK)
      return
      end

      subroutine inthpA(fun,nfun,var,ivar,aa,bb,intvl,ifcm,er,reslt,m,
     1                   iOK)
c hp integration of fun(var), var(ivar)=aa(ivar)...bb(ivar)
c with parallel integration of nfun additional functions cf
c intvl=1: infinite interval a=-infty, b=+infty
c      =2: semi-infinite interval a=aa, b=+infty
c      =3: like in=2 for oscillatory integrands
c      =4: finite interval a=aa, b=bb
c ifcm:    maximum number of function calls
c er:      relative accuracy
c reslt:  approximation of the integral
c m:       number of function calls
c iOK=0    normal and reliable termination of the routine
c    =1    maximum number of function calls exceeded
c    =2    truncation condition violated
c    =3    itm<3: no computation
c    =4    wrong interval type (in<1 or in>4): no computation
c    =-1   er too small, minimum value assumed (er=1.0d-14)
c IOK>4    nfun exceeds limit, IOK=limit
      implicit Real(8) (a-h,o-z)
      parameter(mfun=20)
      logical li1,li2,ltr,lStopInt,lAskQ
      dimension cf(mfun-1),reslt(mfun),sum(mfun),sum1(mfun),
     1          sum2(mfun),U(mfun),V(mfun),W1(mfun),COR(mfun)
      dimension var(1),aa(1),bb(1),intvl(1),ifcm(1)
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      iOK=0
      do j=1,nfun+1
        reslt(j)=0.0d0
      end do
      err=1.0d300
      m=0
      a=aa(ivar)
      b=bb(ivar)
      in=intvl(ivar)
      itm=ifcm(ivar)
      if((nfun+1).gt.mfun) then
        iOK=mfun-1
        return
      end if
      if(itm.lt.3) then
        iOK=3
        return
      elseif((in.lt.1).or.(in.gt.4)) then
        iOK=4
        return
      elseif((in.eq.4).and.(aa(ivar).ge.bb(ivar))) then
        a=bb(ivar)
        b=aa(ivar)
      end if
      iOK=0
      err=er
      if(err.lt.1.0d-14) then
        err=1.0d-14
        iOK=-1
      end if
      SQ2=DSQRT(2.0d0)
      BA=B-A
      ltr=.true.
   40 H=1.0d0
      H0=1.0d0
      err3=err/3.0d0
      SR=DSQRT(err)
      V1=err*10.0d0
      V2=V1
      M1=itm-1
      N=INT(dble(M1/2))
      M2=N
      L1=0
      li1=.TRUE.
      li2=.FALSE.
   50 continue
c initialize
      I=0
      if(IN.EQ.1) then
        var(ivar)=0.0d0
        sum(1)=fun(var,cf)
                if(lStopInt) return
        do j=2,nfun+1
          sum(j)=cf(j-1)
        end do
      elseif(IN.EQ.2) then
        var(ivar)=A+1.0d0
        sum(1)=fun(var,cf)
                if(lStopInt) return
        do j=2,nfun+1
          sum(j)=cf(j-1)
        end do
      elseif(IN.EQ.3) then
        var(ivar)=A+DLOG(1.0d0+SQ2)
        sum(1)=fun(var,cf)/SQ2
                if(lStopInt) return
        do j=2,nfun+1
          sum(j)=cf(j-1)/SQ2
        end do
      else
        var(ivar)=(A+B)/2.0d0
        sum(1)=fun(var,cf)/4.0d0*BA
                if(lStopInt) return
        do j=2,nfun+1
          sum(j)=cf(j-1)/4.0d0*BA
        end do
      end if
   60 continue
c compute weights and nodes
      EXPH=DEXP(H)
      EXPH0=DEXP(H0)
      H1=H0
      E1=EXPH0
      do j=1,nfun+1
        COR(j)=0.0d0
        U(j)=0.0d0
      end do
   70 continue
c compute function values
      if(IN.eq.1) then
        var(ivar)=H1
        V(1)=fun(var,cf)
                if(lStopInt) return
        do j=2,nfun+1
          V(j)=cf(j-1)
        end do
        H1=H1+H
      elseif(IN.eq.2) then
        var(ivar)=A+E1
        V(1)=E1*fun(var,cf)
                if(lStopInt) return
        do j=2,nfun+1
          V(j)=E1*cf(j-1)
        end do
        E1=E1*EXPH
      elseif(IN.eq.3) then
        do j=1,nfun+1
          W1(j)=DSQRT(E1+1.0d0/E1)
        end do
        W2=DSQRT(E1)
        if(E1.LT.0.10d0) goto 110
        S=DLOG(E1+W1(1)*W2)
        goto 130
  110   W3=E1
        W4=E1*E1
        C0=1.0d0
        S=E1
        S1=E1
        T=0.0d0
  120   C0=-C0*(0.5d0+T)*(2.0d0*T+1.0d0)/(2.0d0*T+3.0d0)/(T+1.0d0)
        T=T+1.0d0
        W3=W3*W4
        S=S+C0*W3
        if(S.EQ.S1) goto 130
        S1=S
        goto 120
  130   continue
        var(ivar)=A+S
        V(1)=W2/W1(1)*fun(var,cf)
                if(lStopInt) return
        do j=2,nfun+1
          V(j)=W2/W1(j)*cf(j-1)
        end do
        E1=E1*EXPH
      else
        do j=1,nfun+1
          W1(j)=E1+1.0d0
        end do
        if(W1(1).gt.1.0d18) then
          var(ivar)=A/W1(1)+B
          V(1)=fun(var,cf)*BA/W1(1)
                if(lStopInt) return
          do j=2,nfun+1
            V(j)=cf(j-1)*BA/W1(j)
          end do
        else
          var(ivar)=(A+B*E1)/W1(1)
          V(1)=E1*fun(var,cf)*BA/(W1(1)**2)
                if(lStopInt) return
          do j=2,nfun+1
            V(j)=E1*cf(j-1)*BA/(W1(j)**2)
          end do
        end if
        E1=E1*EXPH
      end if
C summation
      I=I+1
      do j=1,nfun+1
        SUM1(j)=U(j)+V(j)
        if(DABS(U(j)).LT.DABS(V(j))) then
          COR(j)=U(j)-(SUM1(j)-V(j))+COR(j)
        else
          COR(j)=V(j)-(SUM1(j)-U(j))+COR(j)
        end if
        U(j)=SUM1(j)
      end do
      if(I.LT.L1) goto 70
      if(li1) then
C check truncation condition
        V0=V1
        V1=V2
        V2=DABS(V(1))
        if((V0+V1+V2).gt.err3) then
          if(I.LT.M2) goto 70
          ltr=.false.
        end if
        if(li2) then
          K=I
        else
          L=I
        end if
        V1=10.0d0*err
        V2=V1
        M2=M1-L
        if(.not.li2) goto 180
        if(.not.ltr) then
C return with truncation condition violated
          do j=1,nfun+1
            reslt(j)=U(j)+COR(j)+sum(j)
          end do
          iOK=2
          M=K+L+1
          goto 99
        end if
C TRUNCATION CONDITION SATISFIED, SUM2=TRAPEZOIDAL APPROXIMATION
        do j=1,nfun+1
          SUM2(j)=SUM1(j)+COR(j)+SUM(j)
        end do
        M2=2*(K+L)
        if(M2.GT.M1) then
C return with maximum number of function calls exceeded
          do j=1,nfun+1
            reslt(j)=SUM2(j)
          end do
          iOK=1
          M=K+L+1
          goto 99
        end if
C INITIALIZE ITERATION
        li1=.FALSE.
        li2=.FALSE.
        L1=L
        I=0
        H=-H
        H0=H*0.5d0
        goto 60
      end if
      if(li2) then
C COMPUTE THE MIDORDINATE APPROXIMATION SUM1
        H=-H
        do j=1,nfun+1
          SUM1(j)=(SUM1(j)+COR(j))*H
          W1(j)=(SUM1(j)+SUM2(j))*0.5d0
        end do
C TERMINATION CONDITION
        if(DABS(SUM1(1)-SUM2(1)).LE.SR) then
          do j=1,nfun+1
            reslt(j)=W1(j)
          end do
          M=M2+1
          goto 99
        end if
C SET UP DATA FOR THE NEXT ITERATION
        M2=2*M2
        if(M2.GT.M1) then
          do j=1,nfun+1
            reslt(j)=W1(j)
          end do
          iOK=1
          M=M2/2+1
          goto 99
        end if
        I=0
        K=2*K
        L=2*L
        L1=L
        H=H*0.5d0
        H0=H*0.5d0
        do j=1,nfun+1
          SUM2(j)=W1(j)
        end do
        li2=.FALSE.
        goto 60
      end if
C SET UP PARAMETERS TO CONTINUE SUMMATION
      L1=K
  180 li2=.TRUE.
      I=0
      EXPH=1.0d0/EXPH
      H0=-H0
      E1=1.0d0/EXPH0
      H1=H0
      H=-H
      goto 70
   99 continue
      if((in.eq.4).and.(aa(ivar).ge.bb(ivar))) then
        do j=1,nfun+1
          reslt(j)=-reslt(j)
        end do
      end if
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine intgk(fun,nfun,var,ivar,aa,bb,ipt,ifcm,accrel,reslt,
     1                   m,iOK)
      implicit Real(8) (a-h,o-z)
      parameter (limit=100,mfun=35)
      dimension reslt(mfun)
      dimension var(1),aa(1),bb(1),iptA(1),ifcmA(1)
      Logical lStopInt,lAskQ
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      iptA(1)=ipt
      ifcmA(1)=ifcm
      call intgkA(fun,nfun,var,ivar,aa,bb,iptA,ifcmA,accrel,reslt,m,iOK)
      return
      end
      

      subroutine intgkA(fun,nfun,var,ivar,aa,bb,ipt,ifcm,accrel,reslt,
     1                   m,iOK)
c gauss-kronrod integration of fun(var), var(ivar)=aa(ivar)...bb(ivar)
c with parallel integration of nfun additional functions cf
c ipt:    1,2,...6 for 15,21,31,41,51,61 point integration
c ifcm:   maximum number of function calls
c accrel: relative accuracy
c reslt:  approximation of the integrals
c m:      number of function calls
c iOK=0   normal and reliable termination of the routine
c     1   maximum number of subdivisions (limit=100) has been achieved
c     2   the occurrence of roundoff error is detected
c     3   extremely bad integrand behaviour occurs at some points
c     4   itm too small for n-th approximation
c     5   itm too small for first approximation
c    -1   epsrel too small, minimum value assumed
c    -2   key too small, minimum value assumed: key=1
c    -3   key too large, maximum value assumed: key=6
c IOK>5 nfun exceeds limit, IOK=limit
      implicit Real(8) (a-h,o-z)
      parameter (limit=100,mfun=35)
      dimension alist(limit),blist(limit),elist(limit),
     1 rlist(limit,mfun),iord(limit),reslt(mfun),area1(mfun),
     2 area2(mfun)
      dimension var(1),aa(1),bb(1),ipt(1),ifcm(1)
      Logical lStopInt,lAskQ
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      iOK=0
      if((nfun+1).gt.mfun) then
        iOK=mfun-1
        return
      end if
      a=aa(ivar)
      b=bb(ivar)
      key=ipt(ivar)
      itm=ifcm(ivar)
      eps=1.0d-15
      e50=50.0d0*eps
      epsrel=accrel
      if(epsrel.lt.e50) then
        iOK=-1
        epsrel=e50
      end if
      keyf=key
      if(key.le.0) then
        iOK=-2
        keyf=1
      end if
      if(key.ge.7) then
        iOK=-3
        keyf=6
      end if
      small=1.0d-300
      m=0
      last=0
      do i=1,nfun+1
        reslt(i)=0.0d0
        rlist(1,i)=0.0d0
      end do
      abserr=0.0d0
      alist(1)=a
      blist(1)=b
      elist(1)=0.0d0
      iord(1)=0
c first approximation
      if(keyf.ne.1) then
        m=10*keyf+1
      else
        m=15
      end if
      if(m.gt.itm) then
        iOK=5
        m=0
        return
      end if
      call inigk(keyf,ipoint)
      call gk(fun,nfun,var,ivar,a,b,ipoint,reslt,abserr,defabs,resabs)
                if(lStopInt) return
      last=1
      do i=1,nfun+1
        rlist(1,i)=reslt(i)
      end do
      elist(1)=abserr
      iord(1)=1
c test on accuracy.
      errbnd=epsrel*dabs(reslt(1))
      if((abserr.le.e50*defabs).and.(abserr.gt.errbnd)) iOK=2
      if((iOK.gt.0).or.((abserr.le.errbnd).and.(abserr.ne.resabs))
     1  .or.(abserr.lt.small)) return
c initialization
      errmax=abserr
      maxerr=1
      area=reslt(1)
      errsum=abserr
      nrmax=1
      iroff1=0
      iroff2=0
c main do-loop
      do l=2,limit
        last=l
        m0=m
        if(keyf.ne.1) then
          m=m+20*keyf+2
        else
          m=m+30
        end if
        if(m.gt.itm) then
          iOK=4
          m=m0
          last=last-1
          Exit
        end if
c bisect the subinterval with the largest error estimate.
        a1=alist(maxerr)
        b1=0.5d+00*(alist(maxerr)+blist(maxerr))
        a2=b1
        b2=blist(maxerr)
        call gk(fun,nfun,var,ivar,a1,b1,ipoint,area1,error1,resabs,
     1          defab1)
                if(lStopInt) return
        call gk(fun,nfun,var,ivar,a2,b2,ipoint,area2,error2,resabs,
     1          defab2)
                if(lStopInt) return
c improve previous approximations to integral and error and test for accuracy.
        area12=area1(1)+area2(1)
        erro12=error1+error2
        errsum=errsum+erro12-errmax
        area=area+area12-rlist(maxerr,1)
        if((defab1.ne.error1).and.(defab2.ne.error2)) then
          if(dabs(rlist(maxerr,1)-area12).le.1.0d-05*dabs(area12)
     1    .and.erro12.ge.0.99d0*errmax) iroff1=iroff1+1
          if(last.gt.10.and.erro12.gt.errmax) iroff2=iroff2+1
	  end if
        do i=1,nfun+1
          rlist(maxerr,i)=area1(i)
          rlist(last,i)=area2(i)
        end do
        errbnd=epsrel*dabs(area)
        if(errsum.gt.errbnd) then
c test for roundoff error and eventually set error flag
          if((iroff1.ge.6).or.(iroff2.ge.20)) iOK=2
c set error flag in the case that the number of subintervals equals limit
          if(last.eq.limit) iOK=1
c set error flag in the case of bad integrand behaviour
c at a point of the integration range.
          if(dmax1(dabs(a1),dabs(b2)).le.(1.0d0+2.0d0*e50)*
     1    (dabs(a2)+1.0d3*small)) iOK=3
	  end if
c append the newly-created intervals to the list.
        if(error2.gt.error1) then
          alist(maxerr)=a2
          alist(last)=a1
          blist(last)=b1
          do i=1,nfun+1
            rlist(maxerr,i)=area2(i)
            rlist(last,i)=area1(i)
          end do
          elist(maxerr)=error2
          elist(last)=error1
	  else
          alist(last)=a2
          blist(maxerr)=b1
          blist(last)=b2
          elist(maxerr)=error1
          elist(last)=error2
	  end if
c call subroutine srtgk to maintain the descending ordering
c in the list of error estimates and select the subinterval
c with the largest error estimate (to be bisected next).
        call srtgk(limit,last,maxerr,errmax,elist,iord,nrmax)
c jump out of do-loop
        if((iOK.gt.0).or.(errsum.le.errbnd)) Exit
      end do
c compute final reslt.
      do i=1,nfun+1
        reslt(i)=0.0d0
      end do
      do k=1,last
        do i=1,nfun+1
          reslt(i)=reslt(i)+rlist(k,i)
        end do
      end do
      abserr=errsum
      end

      subroutine inigk(jord,ipoint)
c initialize weigts and abscissas for ingl
      implicit Real(8) (a-h,o-z)
      dimension wg15( 4),wgk15( 8),xgk15( 8)
      dimension wg21( 5),wgk21(11),xgk21(11)
      dimension wg31( 8),wgk31(16),xgk31(16)
      dimension wg41(10),wgk41(21),xgk41(21)
      dimension wg51(13),wgk51(26),xgk51(26)
      dimension wg61(15),wgk61(31),xgk61(31)
      common/gkwgt/wg(15),wgk(31),xgk(31)
      save jord0
      data  wg15/0.12948496616886969327d0,0.27970539148927666790d0,
     *           0.38183005050511894495d0,0.41795918367346938776d0/
      data xgk15/0.99145537112081263921d0,0.94910791234275852453d0,
     *           0.86486442335976907279d0,0.74153118559939443986d0,
     *           0.58608723546769113029d0,0.40584515137739716691d0,
     *           0.20778495500789846760d0,0.0d0/
      data wgk15/0.02293532201052922496d0,0.06309209262997855329d0,
     *           0.10479001032225018384d0,0.14065325971552591875d0,
     *           0.16900472663926790283d0,0.19035057806478540991d0,
     *           0.20443294007529889241d0,0.20948214108472782801d0/
      data  wg21/0.06667134430868813759d0,0.14945134915058059315d0,
     *           0.21908636251598204400d0,0.26926671930999635509d0,
     *           0.29552422471475287017d0/
      data xgk21/0.99565716302580808074d0,0.97390652851717172008d0,
     *           0.93015749135570822600d0,0.86506336668898451073d0,
     *           0.78081772658641689706d0,0.67940956829902440623d0,
     *           0.56275713466860468334d0,0.43339539412924719080d0,
     *           0.29439286270146019813d0,0.14887433898163121088d0,
     *           0.0d0/
      data wgk21/0.01169463886737187428d0,0.03255816230796472748d0,
     *           0.05475589657435199603d0,0.07503967481091995277d0,
     *           0.09312545458369760554d0,0.10938715880229764190d0,
     *           0.12349197626206585108d0,0.13470921731147332593d0,
     *           0.14277593857706008080d0,0.14773910490133849137d0,
     *           0.14944555400291690566d0/
      data  wg31/0.03075324199611726835d0,0.07036604748810812471d0,
     *           0.10715922046717193501d0,0.13957067792615431445d0,
     *           0.16626920581699393355d0,0.18616100001556221102d0,
     *           0.19843148532711157646d0,0.20257824192556127288d0/
      data xgk31/0.99800229869339706029d0,0.98799251802048542849d0,
     *           0.96773907567913913426d0,0.93727339240070590431d0,
     *           0.89726453234408190088d0,0.84820658341042721620d0,
     *           0.79041850144246593297d0,0.72441773136017004742d0,
     *           0.65099674129741697053d0,0.57097217260853884754d0,
     *           0.48508186364023968069d0,0.39415134707756336990d0,
     *           0.29918000715316881217d0,0.20119409399743452230d0,
     *           0.10114206691871749903d0,0.00d0/
      data wgk31/0.00537747987292334899d0,0.01500794732931612254d0,
     *           0.02546084732671532019d0,0.03534636079137584622d0,
     *           0.04458975132476487661d0,0.05348152469092808727d0,
     *           0.06200956780067064029d0,0.06985412131872825871d0,
     *           0.07684968075772037889d0,0.08308050282313302104d0,
     *           0.08856444305621177065d0,0.09312659817082532123d0,
     *           0.09664272698362367851d0,0.09917359872179195933d0,
     *           0.10076984552387559504d0,0.10133000701479154902d0/
      data wg41/0.01761400713915211832d0,0.04060142980038694133d0,
     *           0.06267204833410906357d0,0.08327674157670474872d0,
     *           0.10193011981724043504d0,0.11819453196151841731d0,
     *           0.13168863844917662690d0,0.14209610931838205133d0,
     *           0.14917298647260374679d0,0.15275338713072585070d0/
      data xgk41/0.99885903158827766384d0,0.99312859918509492479d0,
     *           0.98150787745025025919d0,0.96397192727791379127d0,
     *           0.94082263383175475352d0,0.91223442825132590587d0,
     *           0.87827681125228197608d0,0.83911697182221882339d0,
     *           0.79504142883755119835d0,0.74633190646015079261d0,
     *           0.69323765633475138481d0,0.63605368072651502545d0,
     *           0.57514044681971031534d0,0.51086700195082709800d0,
     *           0.44359317523872510320d0,0.37370608871541956067d0,
     *           0.30162786811491300432d0,0.22778585114164507808d0,
     *           0.15260546524092267551d0,0.07652652113349733375d0,
     *           0.0d0/
      data wgk41/0.00307358371852053150d0,0.00860026985564294220d0,
     *           0.01462616925697125298d0,0.02038837346126652360d0,
     *           0.02588213360495115883d0,0.03128730677703279896d0,
     *           0.03660016975820079803d0,0.04166887332797368626d0,
     *           0.04643482186749767472d0,0.05094457392372869193d0,
     *           0.05519510534828599474d0,0.05911140088063957237d0,
     *           0.06265323755478116803d0,0.06583459713361842211d0,
     *           0.06864867292852161935d0,0.07105442355344406831d0,
     *           0.07303069033278666750d0,0.07458287540049918899d0,
     *           0.07570449768455667466d0,0.07637786767208073671d0,
     *           0.07660071191799965645d0/
      data  wg51/0.01139379850102628795d0,0.02635498661503213726d0,
     *           0.04093915670130631266d0,0.05490469597583519193d0,
     *           0.06803833381235691721d0,0.08014070033500101801d0,
     *           0.09102826198296364981d0,0.10053594906705064420d0,
     *           0.10851962447426365312d0,0.11485825914571164834d0,
     *           0.11945576353578477223d0,0.12224244299031004169d0,
     *           0.12317605372671545120d0/
      data xgk51/0.99926210499260983419d0,0.99555696979049809791d0,
     *           0.98803579453407724764d0,0.97666392145951751150d0,
     *           0.96161498642584251242d0,0.94297457122897433941d0,
     *           0.92074711528170156175d0,0.89499199787827536885d0,
     *           0.86584706529327559545d0,0.83344262876083400142d0,
     *           0.79787379799850005941d0,0.75925926303735763058d0,
     *           0.71776640681308438819d0,0.67356636847346836449d0,
     *           0.62681009901031741279d0,0.57766293024122296772d0,
     *           0.52632528433471918260d0,0.47300273144571496052d0,
     *           0.41788538219303774885d0,0.36117230580938783774d0,
     *           0.30308953893110783017d0,0.24386688372098843205d0,
     *           0.18371893942104889202d0,0.12286469261071039639d0,
     *           0.06154448300568507889d0,0.0d0/
      data wgk51/0.00198738389233031593d0,0.00556193213535671376d0,
     *           0.00947397338617415161d0,0.01323622919557167481d0,
     *           0.01684781770912829823d0,0.02043537114588283546d0,
     *           0.02400994560695321622d0,0.02747531758785173780d0,
     *           0.03079230016738748889d0,0.03400213027432933784d0,
     *           0.03711627148341554356d0,0.04008382550403238207d0,
     *           0.04287284502017004948d0,0.04550291304992178891d0,
     *           0.04798253713883671391d0,0.05027767908071567196d0,
     *           0.05236288580640747586d0,0.05425112988854549014d0,
     *           0.05595081122041231731d0,0.05743711636156783285d0,
     *           0.05868968002239420796d0,0.05972034032417405998d0,
     *           0.06053945537604586295d0,0.06112850971705304831d0,
     *           0.06147118987142531666d0,0.06158081806783293508d0/
      data  wg61/0.00796819249616660562d0,0.01846646831109095914d0,
     *           0.02878470788332336935d0,0.03879919256962704960d0,
     *           0.04840267283059405290d0,0.05749315621761906648d0,
     *           0.06597422988218049513d0,0.07375597473770520627d0,
     *           0.08075589522942021535d0,0.08689978720108297980d0,
     *           0.09212252223778612872d0,0.09636873717464425964d0,
     *           0.09959342058679526706d0,0.10176238974840550460d0,
     *           0.10285265289355884034d0/
      data xgk61/0.99948441005049063757d0,0.99689348407464954027d0,
     *           0.99163099687040459486d0,0.98366812327974720997d0,
     *           0.97311632250112626837d0,0.96002186496830751222d0,
     *           0.94437444474855997942d0,0.92620004742927432588d0,
     *           0.90557330769990779855d0,0.88256053579205268154d0,
     *           0.85720523354606109896d0,0.82956576238276839744d0,
     *           0.79972783582183908301d0,0.76777743210482619492d0,
     *           0.73379006245322680473d0,0.69785049479331579693d0,
     *           0.66006106412662696137d0,0.62052618298924286114d0,
     *           0.57934523582636169176d0,0.53662414814201989926d0,
     *           0.49248046786177857499d0,0.44703376953808917678d0,
     *           0.40040125483039439254d0,0.35270472553087811347d0,
     *           0.30407320227362507737d0,0.25463692616788984644d0,
     *           0.20452511668230989144d0,0.15386991360858354696d0,
     *           0.10280693796673703015d0,0.05147184255531769583d0,
     *           0.00000000000000000000d0/
      data wgk61/0.00138901369867700762d0,0.00389046112709988405d0,
     *           0.00663070391593129217d0,0.00927327965951776343d0,
     *           0.01182301525349634174d0,0.01436972950704580481d0,
     *           0.01692088918905327263d0,0.01941414119394238117d0,
     *           0.02182803582160919230d0,0.02419116207808060137d0,
     *           0.02650995488233310161d0,0.02875404876504129284d0,
     *           0.03090725756238776247d0,0.03298144705748372603d0,
     *           0.03497933802806002414d0,0.03688236465182122922d0,
     *           0.03867894562472759295d0,0.04037453895153595911d0,
     *           0.04196981021516424615d0,0.04345253970135606932d0,
     *           0.04481480013316266319d0,0.04605923827100698812d0,
     *           0.04718554656929915395d0,0.04818586175708712914d0,
     *           0.04905543455502977889d0,0.04979568342707420636d0,
     *           0.05040592140278234684d0,0.05088179589874960649d0,
     *           0.05122154784925877217d0,0.05142612853745902593d0,
     *           0.05149472942945156756d0/
      data jord0/0/
      if(jord.eq.1) then
        ipoint=15
        if(jord0.eq.jord) return
        do i=1,4
          wg(i)=wg15(i)
        end do
        do i=1,8
          wgk(i)=wgk15(i)
          xgk(i)=xgk15(i)
        end do
      elseif(jord.eq.2) then
        ipoint=21
        if(jord0.eq.jord) return
        do i=1,5
          wg(i)=wg21(i)
        end do
        do i=1,11
          wgk(i)=wgk21(i)
          xgk(i)=xgk21(i)
        end do
      elseif(jord.eq.3) then
        ipoint=31
        if(jord0.eq.jord) return
        do i=1,8
          wg(i)=wg31(i)
        end do
        do i=1,16
          wgk(i)=wgk31(i)
          xgk(i)=xgk31(i)
        end do
      elseif(jord.eq.4) then
        ipoint=41
        if(jord0.eq.jord) return
        do i=1,10
          wg(i)=wg41(i)
        end do
        do i=1,21
          wgk(i)=wgk41(i)
          xgk(i)=xgk41(i)
        end do
      elseif(jord.eq.5) then
        ipoint=51
        if(jord0.eq.jord) return
        do i=1,13
          wg(i)=wg51(i)
        end do
        do i=1,26
          wgk(i)=wgk51(i)
          xgk(i)=xgk51(i)
        end do
      else
        ipoint=61
        if(jord0.eq.jord) return
        do i=1,15
          wg(i)=wg61(i)
        end do
        do i=1,31
          wgk(i)=wgk61(i)
          xgk(i)=xgk61(i)
        end do
      end if
      jord0=jord
      end

      subroutine gk(fun,nfun,var,ivar,a,b,jord,reslt,abserr,resabs,
     1              resasc)
      implicit Real(8) (a-h,o-z)
      parameter(mfun=35)
      dimension fv1(30),fv2(30)
      dimension cf1(mfun-1),cf2(mfun-1),resc(mfun-1),reslt(mfun),var(1)
      Logical lStopInt,lAskQ
      common/gkwgt/wg(15),wgk(31),xgk(31)
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      j3=jord/2
      j1=j3/2
      j2=j3-j1
      eps=1.0d-15
      small=1.0d-300
      c=0.5d0*(a+b)
      d=0.5d0*(b-a)
      dd=dabs(d)
      var(ivar)=c
      fc=fun(var,cf1)
                if(lStopInt) return
      if(j3.ne.(2*j1)) then
        resg=fc*wg(j2)
        resk=fc*wgk(2*j2)
        do i=1,nfun
          resc(i)=cf1(i)*wgk(2*j2)
        end do
      else
        resg=0.0d0
        resk=fc*wgk(j3+1)
        do i=1,nfun
          resc(i)=cf1(i)*wgk(j3+1)
        end do
      end if
      resabs=dabs(resk)
      do j=1,j1
        jtw=j*2
        absc=d*xgk(jtw)
        var(ivar)=c-absc
        fval1=fun(var,cf1)
        var(ivar)=c+absc
                if(lStopInt) return
        fval2=fun(var,cf2)
                if(lStopInt) return
        fv1(jtw)=fval1
        fv2(jtw)=fval2
        fsum=fval1+fval2
        resg=resg+wg(j)*fsum
        resk=resk+wgk(jtw)*fsum
        do i=1,nfun
          resc(i)=resc(i)+wgk(jtw)*(cf1(i)+cf2(i))
        end do
        resabs=resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
      end do
      do j=1,j2
        jtwm1=j*2-1
        absc=d*xgk(jtwm1)
        var(ivar)=c-absc
        fval1=fun(var,cf1)
                if(lStopInt) return
        var(ivar)=c+absc
        fval2=fun(var,cf2)
                if(lStopInt) return
        fv1(jtwm1)=fval1
        fv2(jtwm1)=fval2
        fsum=fval1+fval2
        resk=resk+wgk(jtwm1)*fsum
        do i=1,nfun
          resc(i)=resc(i)+wgk(jtwm1)*(cf1(i)+cf2(i))
        end do
        resabs=resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
      end do
      reskh=resk*0.5d0
      resasc=wgk(8)*dabs(fc-reskh)
      do j=1,j3
        resasc=resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
      end do
      reslt(1)=resk*d
      do i=1,nfun
        reslt(i+1)=resc(i)*d
      end do
      resabs=resabs*dd
      resasc=resasc*dd
      abserr=dabs((resk-resg)*d)
      if(resasc.ne.0.0d0.and.abserr.ne.0.0d0)
     1  abserr=resasc*dmin1(1.0d0,(2.0d2*abserr/resasc)**1.5d0)
      if(resabs.gt.small/(0.5d+02*eps)) abserr=dmax1
     1  ((eps*50.0d0)*resabs,abserr)
      end

      subroutine srtgk(limit,last,maxerr,ermax,elist,iord,nrmax)
c auxiliary for intgk
      implicit Real(8) (a-h,o-z)
      dimension elist(last),iord(last)
      if(last.lt.3) then
        iord(1)=1
        iord(2)=2
        maxerr=iord(nrmax)
        ermax=elist(maxerr)
	      return
	    end if
      errmax=elist(maxerr)
      if(nrmax.ne.1) then
        ido=nrmax-1
        do i=1,ido
          isucc=iord(nrmax-1)
          if(errmax.le.elist(isucc)) Exit
          iord(nrmax)=isucc
          nrmax=nrmax-1
        end do
	    end if
      jupbn=last
      if(last.gt.(limit/2+2)) jupbn=limit+3-last
      errmin=elist(last)
      jbnd=jupbn-1
      ibeg=nrmax+1
      if(ibeg.gt.jbnd) then
        iord(jbnd)=maxerr
        iord(jupbn)=last
        maxerr=iord(nrmax)
        ermax=elist(maxerr)
	      return
	    end if
      do 40 i=ibeg,jbnd
        isucc=iord(i)
        if(errmax.ge.elist(isucc)) goto 60
        iord(i-1)=isucc
   40 continue
   50 iord(jbnd)=maxerr
      iord(jupbn)=last
      goto 90
   60 iord(i-1)=maxerr
      k=jbnd
      do 70 j=i,jbnd
        isucc=iord(k)
        if(errmin.lt.elist(isucc)) goto 80
        iord(k+1)=isucc
        k=k-1
   70 continue
      iord(i)=last
      goto 90
   80 iord(k+1)=last
   90 maxerr=iord(nrmax)
      ermax=elist(maxerr)
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine intgl(fun,nfun,var,ivar,aa,bb,mo,nfcm,reslt,m,iOK)
c Gaussian-Legendre Quadrature of fun(var), var(ivar)=y1...y2
c with parallel integration of nfun additional functions cf
c The order mo is limited: 0 < mo < 13.
c The number of integration points nfcm should be >= mo.
c iOK=-1: order mo(ivar)<1  -> assume order 1
c iOK=-2: order mo(ivar)>12 -> assume order 12
c iOK=-3: order mo(ivar)>ifcm(ivar) -> assume nfcm(ivar) = ifcm(ivar)
c iOK>0   nfun exceeds limit, IOK=limit
      implicit Real(8) (a-h,o-z)
      parameter(mfun=20)
      dimension reslt(mfun)
      dimension var(1),aa(1),bb(1),moA(1),nfcmA(1)
      Logical lStopInt,lAskQ
      common/glwgt/wgt(12,12),pxy(12,12)
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      nfcmA(1)=nfcm
      moA(1)=mo
      call intglA(fun,nfun,var,ivar,aa,bb,moA,nfcmA,reslt,m,iOK)
      return
      end
      

      subroutine intglA(fun,nfun,var,ivar,aa,bb,mo,nfcm,reslt,m,iOK)
c Gaussian-Legendre Quadrature of fun(var), var(ivar)=y1...y2
c with parallel integration of nfun additional functions cf
c The order mo is limited: 0 < mo < 13.
c The number of integration points nfcm should be >= mo.
c iOK=-1: order mo(ivar)<1  -> assume order 1
c iOK=-2: order mo(ivar)>12 -> assume order 12
c iOK=-3: order mo(ivar)>ifcm(ivar) -> assume nfcm(ivar) = ifcm(ivar)
c iOK>0   nfun exceeds limit, IOK=limit
      implicit Real(8) (a-h,o-z)
      parameter(mfun=20)
      dimension cf(mfun-1),reslt(mfun),dreslt(mfun)
      dimension var(1),aa(1),bb(1),mo(1),nfcm(1)
      Logical lStopInt,lAskQ
      common/glwgt/wgt(12,12),pxy(12,12)
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      save ifirst
      data ifirst/1/
      if((nfun+1).gt.mfun) then
        iOK=mfun-1
        return
      end if
      y1=aa(ivar)
      y2=bb(ivar)
      iy=mo(ivar)
      my=nfcm(ivar)
      if(iy.lt.1) then
        iy=1
        iOK=-1
      end if
      if(iy.gt.12) then
        iy=12
        iOK=-2
      end if
      if(iy.gt.my) then
        my=iy
        iOK=-3
      end if
      ny=my/iy
      if(ny.lt.1) ny=1
      if(ifirst.eq.1) then
c initialization of weights and abscissas on first entry of subroutine
        call inigl
        ifirst=0
      endif
      iOK=0
      dny=dble(ny)
      dyba=(y2-y1)*0.5d0
      dyab=(y1+y2)*0.5d0
      do j=1,nfun+1
        reslt(j)=0.0d0
      end do
      m=0
      do il=1,ny
        dil=dble(2*il-1-ny)
        do jl=1,iy
          agy=(pxy(jl,iy)+dil)/dny
          pgy=wgt(jl,iy)/dny
          yl=agy*dyba+dyab
          fct=pgy
          m=m+1
          var(ivar)=yl
          dreslt(1)=fun(var,cf)
                if(lStopInt) return
          do j=1,nfun
            dreslt(j+1)=cf(j)
          end do
          do j=1,nfun+1
            reslt(j)=reslt(j)+dreslt(j)*fct
          end do
        end do
      end do
      fct=dyba
      do j=1,nfun+1
        reslt(j)=reslt(j)*fct
      end do
      end

      subroutine inigl
c initialize weigts and abscissas for ingl
      implicit Real(8) (a-h,o-z)
      dimension dwgt(42),dpxy(42)
      common/glwgt/wgt(12,12),pxy(12,12)
      data (dwgt(k),k=1,42)/
     *  2.0d0, 1.0d0,
     * .888888888888889d0, .555555555555556d0, .652145154862546d0,
     * .347854845137454d0, .568888888888889d0, .478628670499366d0,
     * .236926885056189d0, .467913934572691d0, .360761573048139d0,
     * .171324492379170d0, .417959183673469d0, .381830050505119d0,
     * .279705391489277d0, .129484966168870d0, .362683783378362d0,
     * .313706645877887d0, .222381034453374d0, .101228536290376d0,
     * .330239355001260d0, .312347077040003d0, .260610696402935d0,
     * .180648160694857d0, .081274388361574d0, .295524224714753d0,
     * .269266719309996d0, .219086362515982d0, .149451349150581d0,
     * .066671344308688d0, .2729251d0        , .2628045d0        ,
     * .2331938d0        , .1862902d0        , .1255804d0        ,
     * .0556686d0        , .249147045813403d0, .233492536538355d0,
     * .203167426723066d0, .160078328543346d0, .106939325995318d0,
     * .047175336386512d0/
      data (dpxy(k),k=1,42)/
     *  0.d0,
     * .577350269189626d0, .000000000000000d0, .774596669241483d0,
     * .339981043584856d0, .861136311594053d0, .000000000000000d0,
     * .538469310105683d0, .906179845938664d0, .238619186083197d0,
     * .661209386466265d0, .932469514203152d0, .000000000000000d0,
     * .405845151377397d0, .741531185599394d0, .949107912342759d0,
     * .183434642495650d0, .525532409916329d0, .796666477413627d0,
     * .960289856497536d0, .000000000000000d0, .324253423403809d0,
     * .613371432700590d0, .836031107326636d0, .968160239507626d0,
     * .148874338981631d0, .433395394129247d0, .679409568299024d0,
     * .865063366688985d0, .973906528517172d0, .0000000d0        ,
     * .2695432d0        , .5190961d0        , .7301520d0        ,
     * .8870626d0        , .9782287d0        , .125233408511469d0,
     * .367831498998180d0, .587317954286617d0, .769902674194305d0,
     * .904117256370475d0, .981560634246719d0/
      k=0
      do 1 j=1,12
      do 1 i=j/2+1,j
      k=k+1
      pxy(i,j)=dpxy(k)
      wgt(i,j)=dwgt(k)
    1 continue
      do 2 j=2,12
      do 2 i=1,j/2
      pxy(i,j)=-pxy(j+1-i,j)
      wgt(i,j)=wgt(j+1-i,j)
    2 continue
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine intch(fun,nfun,var,ivar,ax,bx,mx,mbas,mover,reslt,m,
     1                   iOK)
c IOK>0 nfun exceeds limit, IOK=limit
      implicit Real(8) (a-h,o-z)
      parameter (kmm=120,lmax=3,mfun=20)
      dimension fk(kmm),amat((kmm*(kmm+1))/2),ak(kmm,mfun),c(kmm),
     *          s(kmm),fksav(mfun,kmm),akopt(kmm,mfun)
      dimension cf(mfun-1),reslt(mfun),var(1)
      Logical lStopInt,lAskQ
      save fk,amat,ak,c,s,fksav,akopt
      Common/StopInt/lStopInt,lAskQ
      real(8), external:: fun
      iOK=0
      kr=nfun+1
      if(kr.gt.mfun) then
        iOK=mfun-1
        return
      end if
      m=0
      nx=mx
      if(nx.lt.1) nx=1
      nbas=mbas
      if(nbas.lt.1) nbas=1
      if(nbas.gt.(kmm-mfun)) nbas=kmm-mfun
      nover=mover
      if(nover.lt.0) nover=0
      nl=nbas+nover
      if(nl.lt.2) nl=2
      nover=nl-nbas
      ma1=((nbas+1)*(nbas+2))/2
      mam=(nbas+1)*(kr-1)+ma1
      d=(bx-ax)/dble(nx)
      b=ax
      do j=1,kr
        reslt(j)=0.0d0
      end do
      res=0.0d0
      do j=1,nx
        d01=1.0d0/dble(nl)
        a=b
        b=a+d
        dl=d*d01
        lopt=1
        resopt=1.0d300
        do l=1,lmax
          x=-0.5d0*d01
          xi=a-0.5d0*dl
          do k=1,mam
            amat(k)=0.0d0
          end do
          nupd=0
          do il=1,nl
            x=x+d01
            do k=1,nbas
              fk(k)=bch(l,k,x)
            end do
            if(l.eq.1) then
              m=m+1
              xi=xi+dl
              var(ivar)=xi
              fk(nbas+1)=-fun(var,cf)
                if(lStopInt) return
              do i=1,nfun
                fk(nbas+i+1)=-cf(i)
              end do
              do i=1,kr
                 fksav(i,il)=fk(nbas+i)
              end do
            else
              do i=1,kr
                 fk(nbas+i)=fksav(i,il)
              end do
            end if
            nupd=nupd+1
            call dchud(amat,fk,nbas,kr,nupd,c,s,mam)
          end do
          call dchsl(amat,ak,nbas,kr,kmm,mam)
          resl=dabs(amat(ma1))
          if(resl.lt.resopt) then
            lopt=l
            resopt=resl
            do kk=1,nbas
              do ii=1,kr
                akopt(kk,ii)=ak(kk,ii)
              end do
            end do
          end if
        end do
        l=lopt
        if(nover.gt.0) then
          res=res+dble(nbas)*resopt/(dble(nover)**2)
        end if
        do k=1,nbas
          fa=bintch(l,k,0.0d0)
          fb=bintch(l,k,1.0d0)
          do ir=1,kr
            reslt(ir)=reslt(ir)+akopt(k,ir)*(fb-fa)*d
          end do
        end do
      end do
      res=res/dble(nx)
      end

      Real(8) function bch(l,i,xi)
      implicit Real(8) (a-h,o-z)
      il=i-l+1
      x=xi
      if(il.eq.-1) then
        bch=-1.0d0/(1.01d0-x)
      elseif(il.eq.0) then
        bch=1.0d0/(0.01d0+x)
      else
        bch=dble(il)*(x**(il-1))
      end if
      end

      Real(8) function bintch(l,i,xi)
      implicit Real(8) (a-h,o-z)
      il=i-l+1
      x=xi
      if(il.eq.-1) then
        bintch=dlog(1.01d0-x)
      elseif(il.eq.0) then
        bintch=dlog(0.01d0+x)
      else
        bintch=x**il
      end if
      end


