! Copyright 2017, Christian Hafner
!
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
MODULE CHSPL

! spline and MBPE approximations (+ downhill simplex)

  USE CHMAT

! MBPE data, 2D and adaptive

  Integer(4) nMBPE,iMBPEfreq,iMBPEfr(49),iMBPEfi(49),iMBPEerr
  Complex(8) a0MBPE,a1MBPE,b0MBPE,b1MBPE,b2MBPE ! 2nd order complex MBPE for eigenvalue search
  Complex(8) zMBPE(25),fMBPE(25)
  Complex(8), Allocatable:: cMBPE(:,:),crMBPE(:,:),cwMBPE(:)
  real(8),    allocatable::MBPEX(:,:),MBPEFmax(:),MBPEFmin(:)       
  complex(8), allocatable::MBPEF(:,:,:),MBPEF1(:,:,:),MBPEN(:,:,:),MBPED(:,:,:),MBPEN0(:,:,:),MBPED0(:,:,:)  
  integer(4), allocatable::MBPEp(:),MBPEp0(:) 
  logical,    allocatable::MBPEdone(:)
  real(8)     MBPEfover,MBPEmaxerror,MBPEdeltax,MBPEftop,MBPEfbottom,MBPExstart,MBPExend,MBPExstartOut,MBPExendOut
  integer(4)  MBPEpp,MBPEnn,MBPEdd,MBPEpp0,MBPEnn0,MBPEdd0,MBPEioutput,MBPEcommandL
  Logical MBPEluseDerivative
  integer(4)  MBPEnparam,MBPEndomain,MBPEmaxorder,MBPEmaxncalc,MBPEmaxp,MBPEmaxndomain,MBPEndiv,MBPEntest,MBPEicalc

  CONTAINS

  Subroutine spline2(x,y,n1,n2,s,cx,cy,iErr)
! compute parametric cubic splines for points in the xy plane
    Implicit none
    Integer(4) n1,n2,iErr
    Real(8) x(n1:n2),y(n1:n2),cx(n1:n2),cy(n1:n2),s(n1:n2)
    call splineS(x,y,n1,n2,s)
    call spline1(s,x,n1,n2,.true.,cx,iErr) ! get spline x parameters
    if(iErr.ne.0) return
    call spline1(s,y,n1,n2,.false.,cy,iErr) ! get spline y parameters
  end Subroutine spline2

  Subroutine spline1(x,y,n1,n2,lfirst,c,iErr)
! cubic splines for y(x)
    Implicit none
    Logical, intent(in) :: lfirst
    Integer(4) n1,n2,i,iErr
    Real(8) x(n1:n2),y(n1:n2),c(n1:n2),p,q,qn,un
    Real(8), Save, Allocatable :: u(:),du(:),dm(:),cr(:),rs(:)
    if(n1.eq.1) then ! natural splines
      if(Allocated(u)) Deallocate(u)
      Allocate(u(n1:n2),Stat=iErr)
      if(iErr.ne.0) return
      c(1)=0.0d0
      u(1)=0.0d0
      do i=2,n2-1
        q=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=q*c(i-1)+2.0d0
        c(i)=(q-1.0d0)/p
        u(i)=(6.0d0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-q*u(i-1))/p
      end do
      c(n2)=0.0d0
      do i=n2-1,1,-1
        c(i)=c(i)*c(i+1)+u(i)
      end do
    else if(n1.eq.0) then ! periodic splines
      iErr=0
      if(lfirst) then
        if(Allocated(u)) Deallocate(u)
        if(Allocated(du)) Deallocate(du)
        if(Allocated(dm)) Deallocate(dm)
        if(Allocated(cr)) Deallocate(cr)
        if(Allocated(rs)) Deallocate(rs)
        Allocate(u(0:n2),du(n2),dm(n2),cr(n2),rs(n2),Stat=iErr)
      end if
      if(iErr.ne.0) return
      y(n2)=y(0)
      iErr=1
      if(lfirst) then
        u(0:n2-1)=x(1:n2)-x(0:n2-1)
        u(n2)=u(0)
        du(1:n2)=u(1:n2)
        dm(1:n2)=2.0d0*(u(0:n2-1)+u(1:n2))
      end if
      qn=(y(1)-y(0))/u(0)
      do i=1,n2-1
        un=(y(i+1)-y(i))/u(i)
        rs(i)=3.0d0*(un-qn)
        qn=un
      end do
      rs(n2)=3.0d0*((y(1)-y(0))/u(0)-qn)
      if(lfirst) then
        call CycD(n2,dm,du,cr,iErr)
        if(iErr.ne.0) return
      end if
      call CycS(n2,dm,du,cr,rs,c(1))
      c(0)=c(n2)
      iErr=0
    else ! error
      iErr=-1
    end if
  end Subroutine spline1

  Subroutine splint2(s,x,y,cx,cy,n1,n2,se,xe,ye,xs,ys,xss,yss,xsss,ysss)
! parametric cubic spline interpolation
    Implicit none
    Integer(4) n1,n2
    Real(8) s(n1:n2),x(n1:n2),y(n1:n2),cx(n1:n2),cy(n1:n2),se,xe,ye,xs,ys
    Real(8), Optional :: xss,yss,xsss,ysss
    if(Present(ysss)) then
      call splint1(s,x,cx,n1,n2,se,xe,xs,xss,xsss)
      call splint1(s,y,cy,n1,n2,se,ye,ys,yss,ysss)
    else
      call splint1(s,x,cx,n1,n2,se,xe,xs)
      call splint1(s,y,cy,n1,n2,se,ye,ys)
    end if
  end Subroutine splint2

  Subroutine splint1(xa,ya,c,n1,n2,x,y,ys,yss,ysss)
! cubic spline interpolation
    Implicit none
    Integer(4), Save ::i
    Integer(4) n1,n2,k,khi,klo
    Real(8) x,y,ys,xa(n1:n2),ya(n1:n2),c(n1:n2),t,ai,bi,di,hi,as,bs
    Real(8), Optional :: yss,ysss
    Data i/-1/
    if((i.ge.n1).and.(i.lt.n2)) then ! check current and neighbor intervals
      klo=i
      khi=i+1
      if(x.gt.xa(khi)) then
        klo=klo+1
        khi=khi+1
        if(khi.gt.n2) then
          i=-1
        else if(x.gt.xa(khi)) then
          i=-1
        end if
      else if(x.lt.xa(klo)) then
        klo=klo-1
        khi=khi-1
        if(klo.lt.n1) then
          i=-2
        else if(x.lt.xa(klo)) then
          i=-2
        end if
      end if
    else
      i=-3
    end if
    if(i.lt.0) then ! find interval with bisection
      klo=n1
      khi=n2
      do
        if(khi-klo.gt.1) then
          k=(khi+klo)/2
          if(xa(k).gt.x)then
            khi=k
          else
            klo=k
          endif
        else
          Exit
        endif
      end do
    end if
    if(n1.eq.1) then ! natural splines
      hi=xa(khi)-xa(klo)
      if(hi.eq.0.0d0) then
        y=0.5d0*(ya(klo)+ya(khi))
        ys=2.0d0*pBig
        if(Present(yss)) yss=0.0d0
        if(Present(ysss)) ysss=0.0d0
      else
        ai=(xa(khi)-x)/hi
        bi=(x-xa(klo))/hi
        y=ai*ya(klo)+bi*ya(khi)+((ai**3-ai)*c(klo)+(bi**3-bi)*c(khi))*(hi**2)/6.0d0
        bs=1.0d0/hi
        as=-bs
        ys=as*ya(klo)+bs*ya(khi)+((3.0d0*ai*ai-1.0d0)*as*c(klo)+(3.0d0*bi*bi-1.0d0)*bs*c(khi))*(hi**2)/6.0d0
        if(Present(yss)) yss=ai*c(klo)+bi*c(khi)
        if(Present(ysss)) ysss=as*c(klo)+bs*c(khi)
      end if
    else if(n1.eq.0) then ! periodic splines
      i=klo
      t=x-xa(i)
      if(i.lt.n2) then
        hi=xa(i+1)-xa(i)
        bi=(ya(i+1)-ya(i))/hi-hi*(c(i+1)+2.0d0*c(i))/3.0d0
        di=(c(i+1)-c(i))/(3.0d0*hi)
      else
        hi=xa(n1)-xa(n2)
        bi=(ya(n1)-ya(n2))/hi-hi*(c(n1)+2.0d0*c(n2))/3.0d0
        di=(c(n1)-c(n2))/(3.0d0*hi)
      end if
      y=ya(i)+(bi+(c(i)+di*t)*t)*t
      ys=bi+(2.0d0*c(i)+3.0d0*di*t)*t
      if(Present(yss)) yss=2.0d0*c(i)+6.0d0*di*t
      if(Present(ysss)) ysss=6.0d0*di
    end if
  end Subroutine splint1

  Subroutine CycD(n,dm,du,cr,iErr)
! Decomposition of cyclic tridiagonal system
    Implicit none
    Integer(4) n,iErr,i
    Real(8) dm(n),du(n),cr(n),d,dum,dumm
    iErr=1
    if(n.lt.3) return
    if(dm(1).lt.0.0d0) return
    d=dabs(dm(1))+dabs(du(1))+dabs(du(n))
    if(d.lt.pSmall) return
    d=1.0d0/d
    if((dm(1).lt.0.0d0).or.(d*dabs(dm(1)).lt.2.0d-14)) return
    dum=du(1)
    du(1)=du(1)/dm(1)
    cr(1)=du(n)/dm(1)
    do i=2,n-1
      d=dabs(dm(i))+dabs(du(i))+dabs(dum)
      if(d.lt.pSmall) return
      d=1.0d0/d
      dm(i)=dm(i)-dum*du(i-1)
      if((dm(i).lt.0.0d0).or.(d*dabs(dm(i)).lt.2.0d-14)) return
      if(i.lt.(n-1)) then
        cr(i)=-dum*cr(i-1)/dm(i)
        dum=du(i)
        du(i)=du(i)/dm(i)
      else
        dumm=du(i)
        du(i)=(du(i)-dum*cr(i-1))/dm(i)
      end if
    end do
    d=dabs(dm(n))+dabs(du(n))+dabs(dumm)
    if(d.lt.pSmall) return
    d=1.0d0/d
    dm(n)=dm(n)-dm(n-1)*du(n-1)*du(n-1)
    dum=0.0d0
    do i=1,n-2
      dum=dum+dm(i)*cr(i)*cr(i)
    end do
    dm(n)=dm(n)-dum
    if((dm(n).lt.0.0d0).or.(d*dabs(dm(n)).lt.2.0d-14)) return
    iErr=0
  end Subroutine CycD

  Subroutine CycS(n,dm,du,cr,rs,x)
! Solution of cyclic tridiagonal system
    Implicit none
    Integer(4) n,i
    Real(8) dm(n),du(n),cr(n),rs(n),x(n),dum,sum
    dum=rs(1)
    rs(1)=dum/dm(1)
    sum=cr(1)*dum
    do i=2,n-1
      dum=rs(i)-du(i-1)*dum
      rs(i)=dum/dm(i)
      if(i.ne.(n-1)) sum=sum+cr(i)*dum
    end do
    dum=rs(n)-du(n-1)*dum
    dum=dum-sum
    rs(n)=dum/dm(n)
    x(n)=rs(n)
    x(n-1)=rs(n-1)-du(n-1)*x(n)
    do i=n-2,1,-1
      x(i)=rs(i)-du(i)*x(i+1)-cr(i)*x(n)
    end do
  end Subroutine CycS

  Subroutine splineS(x,y,n1,n2,s)
! compute parameters s for parametric splines, s(n1) is known!
    Implicit none
    Integer(4) n1,n2,i,ie
    real(8) x(n1:n2),y(n1:n2),s(n1:n2),a,b,c,d,e,f,g,dn,dz,ds
    if(n1.lt.(n2-1)) then
      ie=0
      do i=n1+1,n2-1 ! s = length of arc
        a=x(i)-x(i-1)
        b=y(i)-y(i-1)
        c=x(i+1)-x(i)
        d=y(i+1)-y(i)
        e=x(i+1)-x(i-1)
        f=y(i+1)-y(i-1)
        dn=a*d-b*c
        if(dn.eq.0.0d0) then
          g=1.0d0
        else
          dz=c*e+d*f
          if(dz.eq.0.0d0) then
            g=Pi*0.5d0
          else
            dz=dz/dn
            g=dsqrt(1.0d0+dz*dz)*datan(1.0d0/dabs(dz))
          end if
        end if
        ds=g*dsqrt(a*a+b*b)
        if(ds.lt.0.0d0) then
          ie=1
          Exit
        else
          s(i)=s(i-1)+ds
        end if
      end do
      if(ie.eq.0) then
        g=a
        a=-c
        c=-g
        g=b
        b=-d
        d=-g
        e=-e
        f=-f
        dn=a*d-b*c
        if(dn.eq.0.0d0) then
          g=1.0d0
        else
          dz=c*e+d*f
          if(dz.eq.0.0d0) then
            g=Pi*0.5d0
          else
            dz=dz/dn
            g=dsqrt(1.0d0+dz*dz)*datan(1.0d0/dabs(dz))
          end if
        end if
        ds=g*dsqrt(a*a+b*b)
        if(ds.lt.0.0d0) then
          ie=1
        else
          s(n2)=s(n2-1)+ds
        end if
      end if
    else
      ie=-1
    end if
    if(ie.ne.0) then ! arc length computation failed, use approximation
      do i=n1+1,n2
        call DistPtPt(x(i-1),y(i-1),x(i),y(i),ds)
        s(i)=s(i-1)+ds
      end do
    end if
  end Subroutine splineS

! MBPE

  Subroutine MBPE_Defaults(lCheck)
! set default domain + project data
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    MBPEioutput=0       ! displays the output information if ioutput>0
    MBPExstart=0.0d0    ! beginning of an interesting interval of frequency 
    MBPExend=1.0d0      ! end of an interesting interval of frequency 
    MBPEndiv=100        ! initial number of uniformly distributed point on the [x_start,x_end] frequency interval
    MBPEnparam=1        ! number of parameters to be fitted simultaneously by using MBPE
    MBPEfover=1.05      ! overdetermination faqtor
    MBPEmaxerror=0.01   ! maximum value of desired fitting error
    MBPEftop=pBig       ! maximum value of function (for exaple in case of fitting filter S parameter, it is 1)
    MBPEfbottom=nBig    ! minimum value of function (for exaple in case of fitting filter S parameter, it is 0)
    MBPEmaxncalc=1000   ! maximum number of calculations
    MBPEmaxorder=10     ! required maximum orders of the power series of the nominator and denominator
    MBPExstartOut=0.0d0 ! beginning of an interesting interval of frequency 
    MBPExendOut=1.0d0   ! end of an interesting interval of frequency 
    MBPEntest=100       ! the number of test point over the entire frequency range
    MBPEmaxp=0          ! might be defined insead of MBPEfover
    MBPEluseDerivative=.false.
    MBPEcommandL=1      ! line of the movie directives, where adaptive MBPE is started
    iMovVarIn=996
    iMovVarOut1=997
    iMovVarOut2=998
    iMovVarErr=999
    iMBPEfreq=1
    iMBPEfr=2
    iMBPEfi=3
    nMBPE=5
  end Subroutine MBPE_Defaults

  Subroutine cMBPEsearch2O(nGrid,dGrid,zm)
! MBPE minima search of F(Z), based on the Cauchy approximation(a0+a1*z+z**2)/(b0+b1*z+b2*z**2)
! the Cauchy parameters are evaluated from 5 (nMBPE<6) or 9 points zMBPE with function values fMBPE
    Implicit none
    Integer(4) i,j,nRo,nCo,iWork,ierr,im,jm,nGrid
    Real(8) Fa,Fm,d,dGrid
    Complex(8) Z0,Zl,Fl,Zm
    iMBPEerr=0
    nCo=5 ! 2nd Order requires matrix with 5 columns
    nMBPE=max(5,min(9,nMBPE))
    nRo=nMBPE
! allocate matrices and determine optimal workspace cW
    Allocate(cMBPE(nRo,nCo),cRMBPE(nRo,1),cWMBPE(1),stat=ierr)
    if(ierr.ne.0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      DeAllocate(cMBPE,cRMBPE,cWMBPE,stat=ierr)
      iMBPEerr=1
      return
    end if
    call ZGELS('N',nRo,nCo,1,cMBPE,nRo,cRMBPE,nRo,cWMBPE,-1,ierr)
    iWork=nint(Dble(cWMBPE(1)),4)
    DeAllocate(cWMBPE,stat=ierr)
! allocate correct workspace
    if(ierr.eq.0) Allocate(cWMBPE(iWork),stat=ierr)
    if(ierr.ne.0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      DeAllocate(cMBPE,cRMBPE,cWMBPE,stat=ierr)
      iMBPEerr=1
      return
    end if
! setup matrix c
    do i=1,nRo
      cMBPE(i,1)=(-1.0d0,0.0d0)
      cMBPE(i,2)=-zMBPE(i)
      cMBPE(i,3)=fMBPE(i)
      cMBPE(i,4)=fMBPE(i)*zMBPE(i)
      cMBPE(i,5)=fMBPE(i)*zMBPE(i)**2
      cRMBPE(i,1)=zMBPE(i)**2
    end do
! solve
    call ZGELS('N',nRo,nCo,1,cMBPE,nRo,cRMBPE,nRo,cWMBPE,iWork,ierr)
    if(ierr.ne.0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE QR decomposition failed!'
      DeAllocate(cMBPE,cRMBPE,cWMBPE,stat=ierr)
      iMBPEerr=3
      return
    end if
    a0MBPE=crMBPE(1,1)
    a1MBPE=crMBPE(2,1)
    b0MBPE=crMBPE(3,1)
    b1MBPE=crMBPE(4,1)
    b2MBPE=crMBPE(5,1)
! deallocate memory
    DeAllocate(cMBPE,cRMBPE,cWMBPE,stat=ierr)
! search minimum on grid
    Fm=pBig
    Z0=zMBPE(1)
    if(nMBPE.gt.5) Z0=zMBPE(5)
    Z0=Z0-dCmplx(0.5d0*dGrid,0.5d0*dGrid)
    d=dGrid/Dble(nGrid-1)
    do i=1,nGrid
      do j=1,nGrid
        Zl=Z0+dCmplx(Dble(i-1)*d,0.0d0)+dCmplx(0.0d0,Dble(j-1)*d)
        Fl=cMBPEapprox2O(Zl)
        Fa=cdabs(Fl)
        if(Fa.lt.Fm) then
          Fm=Fa
          Zm=Zl
          im=i
          jm=j
        end if
      end do
    end do
    call SMP2D(rMBPEapprox2O,Zm-dCmplx(d,d),zM+dCmplx(d,d),1.0d-8,1.0d-8,1000,Zm,Zl,im,ierr)
  end Subroutine cMBPEsearch2O

  Complex(8) Function cMBPEapprox2O(Zl)
    Implicit none
    Complex(8) Zl
    cMBPEapprox2O=(A0MBPE+A1MBPE*Zl+Zl**2)/(B0MBPE+B1MBPE*Zl+B2MBPE*Zl**2)
  end Function cMBPEapprox2O

  Real(8) Function rMBPEapprox2O(Zl,ydum)
    Implicit none
    Complex(8) Zl,c
    Real(8) ydum(9)
    c=(A0MBPE+A1MBPE*Zl+Zl**2)/(B0MBPE+B1MBPE*Zl+B2MBPE*Zl**2)
    ydum(1)=cdAbs(c)
    ydum(2)=Dble(c)
    ydum(3)=dImag(c)
    ydum(4)=Dble(c)
    ydum(5)=dImag(c)
    ydum(6)=Dble(c)
    ydum(7)=dImag(c)
    ydum(8)=Dble(c)
    ydum(9)=dImag(c)
    rMBPEapprox2O=ydum(1)
  end Function rMBPEapprox2O

! adaptive_MBPE           - main function for adaptive MBPE, returns the fitting error value
! get_total_sample_number - returns the total number ot sample points
! get_domains_number      - returns the namber of domains
! get_samples_number      - returns the number of samples for given domain
! get_sample_point        - returns the sample point for given domain and sample index
! get_sample_value        - returns the sample value for given domain, parameter and sample index
! mbpevalue               - returns value of MBPE approximation for given parameter and frequency point
! deallocate_MBPE_data    - deallocates MBRE datas

  Real(8) function adaptive_MBPE(FunRef)
! FunRef        - the name of the external function that gives the response from electrodynamic system
! MBPEioutput   - displays the output information if ioutput>0
! MBPExstart    - the beginning of an interesting interval of frequency 
! MBPExend      - The end of an interesting interval of frequency 
! MBPEndiv      - the initial number of uniformly distributed point on the [x_start,x_end] frequency interval
! MBPEnparam    - the number of parameters to be fitted simultaneously by using MBPE
! MBPEfover     - overdetermination faqtor
! MBPEmaxerror  - maximum value of desired fitting error
! MBPEftop      - maximum value of function (for exaple in case of fitting filter S parameter, it is 1)
! MBPEfbottom   - minimum value of function (for exaple in case of fitting filter S parameter, it is 0)
! MBPEmaxncalc  - maximum number of calculations
! MBPEmaxorder  - required maximum orders of the power series of the nominator and denominator
! MBPEntest     - the number of test point over the entire frequency range
! Return value of the function is fitting error value
    implicit none
    integer(4),external::FunRef
    integer(4) i,idomain,ip,iparam,index,ierr,res
    real(8) maxmbpeerror,error,dx,Xt
    complex(8) ft(1:MBPEnparam),ft1(1:MBPEnparam)
    real(8),allocatable::xdiv(:)
    complex(8),allocatable::fdiv(:,:),fdiv1(:,:)
    iMBPEerr=0
    adaptive_MBPE=300.0d0
    MBPEicalc=0
    if(MBPEmaxp.lt.1) then
      MBPEmaxp=max(MBPEndiv,nint(2.d0*MBPEmaxorder*MBPEfover))
    else
      MBPEfover=0.5d0*Dble(MBPEndiv)/Dble(MBPEmaxorder)
    end if
    MBPEmaxndomain=dnint(dble(MBPEmaxncalc-1.0d0)/dble(MBPEmaxp-1.d0)+0.499999d0)
    MBPEmaxndomain=max(1,MBPEmaxndomain)
    if(allocated(MBPEp)) deallocate(MBPEp)
    if(allocated(MBPEp0)) deallocate(MBPEp0)
    allocate(MBPEp(1:MBPEmaxndomain),MBPEp0(1:MBPEmaxndomain),stat=ierr)
    if(ierr/=0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      iMBPEerr=1
      return
    end if
    MBPEp=0
    MBPEp0=0
    MBPEndomain=dnint(dble(MBPEndiv-1.0d0)/dble(MBPEmaxp-1.d0)+0.499999d0)
    if(MBPEndomain>MBPEmaxndomain) then
      if(MBPEioutput>-1) write(*,*) 'MBPE Wrong number of initial sample points!'
      iMBPEerr=2
      return
    end if
    if(MBPEndomain<1) MBPEndomain=1
    do idomain=1,MBPEndomain
      if(idomain<MBPEndomain) then
        MBPEp(idomain)=dnint((MBPEndiv-1.d0)/MBPEndomain+1.d0)
      else
        MBPEp(idomain)=dnint(MBPEndiv+(MBPEndomain-1)-(MBPEndomain-1)*dnint((MBPEndiv-1.d0)/MBPEndomain+1))
      end if
    end do 
    MBPEdeltax=(MBPExend-MBPExstart)/(MBPEntest-1)
    if(allocated(xdiv)) deallocate(xdiv) 
    if(allocated(fdiv)) deallocate(fdiv) 
    if(allocated(fdiv1)) deallocate(fdiv1) 
    if(allocated(MBPEX)) deallocate(MBPEX) 
    if(allocated(MBPEF)) deallocate(MBPEF) 
    if(allocated(MBPEF1)) deallocate(MBPEF1) 
    allocate(xdiv(1:MBPEndiv),fdiv(1:MBPEndiv,1:MBPEnparam),fdiv1(1:MBPEndiv,1:MBPEnparam), &
    & MBPEX(1:MBPEmaxndomain,1:MBPEmaxp),MBPEF(1:MBPEmaxndomain,1:MBPEnparam,1:MBPEmaxp), &
    & MBPEF1(1:MBPEmaxndomain,1:MBPEnparam,1:MBPEmaxp),stat=ierr)
    if(ierr/=0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      iMBPEerr=1
      return
    end if
    MBPEX=0.d0
    MBPEF=(0.d0,0.d0)
    MBPEF1=(0.d0,0.d0)
    dx=(MBPExend-MBPExstart)/(MBPEndiv-1)
    do i=1,MBPEndiv
      xdiv(i)=MBPExstart+(i-1)*dx
      res=FunRef(xdiv(i),ft,ft1,MBPEnparam)
      if(l4.and.l5.and.(MBPEioutput>0)) write(*,*) 'MBPE i,x,f=',i,xdiv(i),cdabs(ft(1)) !%
      MBPEicalc=MBPEicalc+1
      fdiv(i,1:MBPEnparam)=ft(1:MBPEnparam)
      fdiv1(i,1:MBPEnparam)=ft1(1:MBPEnparam)
    end do
    index=0      
    do idomain=1,MBPEndomain
      do ip=1,MBPEp(idomain)
        MBPEX(idomain,ip)=xdiv(index+ip)
        do iparam=1,MBPEnparam
          MBPEF(idomain,iparam,ip)=fdiv(index+ip,iparam)
          MBPEF1(idomain,iparam,ip)=fdiv1(index+ip,iparam)
        end do
      end do
      index=index+MBPEp(idomain)-1
    end do
    if(allocated(xdiv)) deallocate(xdiv) 
    if(allocated(fdiv)) deallocate(fdiv) 
    if(allocated(fdiv1))deallocate(fdiv1) 
    if(allocated(MBPEFmax)) deallocate(MBPEFmax) 
    if(allocated(MBPEFmin)) deallocate(MBPEFmin) 
    allocate(MBPEFmax(1:MBPEnparam),MBPEFmin(1:MBPEnparam),stat=ierr)
    if(ierr/=0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      iMBPEerr=1
      return
    end if
    do iparam=1,MBPEnparam
      MBPEFmax(iparam)=-1.d10
      MBPEFmin(iparam)= 1.d10
      do idomain=1,MBPEndomain
        do ip=1,MBPEp(idomain)
          if(MBPEFmax(iparam)<cdabs(MBPEF(idomain,iparam,ip))) MBPEFmax(iparam)=cdabs(MBPEF(idomain,iparam,ip))
          if(MBPEFmin(iparam)>cdabs(MBPEF(idomain,iparam,ip))) MBPEFmin(iparam)=cdabs(MBPEF(idomain,iparam,ip))
        end do
      end do  
    end do
    if(allocated(MBPEN)) deallocate(MBPEN) 
    if(allocated(MBPED)) deallocate(MBPED) 
    if(allocated(MBPEN0)) deallocate(MBPEN0) 
    if(allocated(MBPED0)) deallocate(MBPED0) 
    if(allocated(MBPEdone)) deallocate(MBPEdone) 
    allocate(MBPEN(1:MBPEmaxndomain,1:MBPEnparam,1:MBPEmaxorder),MBPED(1:MBPEmaxndomain,1:MBPEnparam,1:MBPEmaxorder+1), &
    & MBPEN0(1:MBPEmaxndomain,1:MBPEnparam,1:MBPEmaxorder),MBPED0(1:MBPEmaxndomain,1:MBPEnparam,1:MBPEmaxorder+1), &
    & MBPEdone(1:MBPEmaxndomain),stat=ierr)
    if(ierr/=0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      iMBPEerr=1
      return
    end if
    MBPEN=(0.d0,0.d0)
    MBPED=(1.d0,0.d0)
    MBPEN0=(0.d0,0.d0)
    MBPED0=(1.d0,0.d0)
    MBPEdone=.false.
    idomain=1       
    maxmbpeerror=0.d0     
    do while(.not.MBPEdone(idomain))
      if(MBPEluseDerivative) then
        call findpolecoef1(idomain)
        if(iMBPEerr.ne.0) return
      else
        call findpolecoef(idomain)
        if(iMBPEerr.ne.0) return
      end if
      error=getmaxdifference(idomain,xt)
      if(error<MBPEmaxerror) then
        maxmbpeerror=max(maxmbpeerror,error)
        MBPEdone(idomain)=.true.
        if(MBPEioutput>1) write(*,*) '(A,I0,A,e12.4,A)','domain ',idomain,' was successfully done with error ',error,' %'
        idomain=idomain+1
        if(idomain>MBPEndomain) Exit
      else
        if((MBPEnn>MBPEmaxorder).or.(MBPEdd>MBPEmaxorder)) then
          if(MBPEndomain>=MBPEmaxndomain) Exit
          call dividedomain(idomain)
        else
          MBPEN0(idomain,1:MBPEnparam,1:MBPEnn)=MBPEN(idomain,1:MBPEnparam,1:MBPEnn)
          MBPED0(idomain,1:MBPEnparam,1:MBPEdd)=MBPED(idomain,1:MBPEnparam,1:MBPEdd)
          MBPEp0(idomain)=MBPEp(idomain)
          call addpoint(idomain,xt,FunRef)
          MBPEicalc=MBPEicalc+1
          if(MBPEicalc>MBPEmaxncalc) Exit
        end if
      end if
    end do
    adaptive_MBPE=maxmbpeerror
    if(MBPEndiv.lt.MBPEmaxncalc) then
      do idomain=1,MBPEndomain
        if(.not.MBPEdone(idomain)) then 
          if(MBPEioutput>1) write(*,*) '(A,e16.8,A,e16.8,A)','MBPE for ',MBPEX(idomain,1),' :', &
          & MBPEX(idomain,MBPEp(idomain)),' range was not done successfully'
          adaptive_MBPE=200.0d0
        end if
      end do
    end if
    return
  end function adaptive_MBPE

  subroutine FindPoleCoef(idomain)  
! Evaluation the parameters Di and Ni of the power series of the nominator and denominator
    implicit none
    integer(4) i,j,info,lwork,ierr,idomain,iparam
    complex(8),allocatable:: A(:,:),B(:,:),work(:)
    iMBPEerr=0
    call getorder(idomain)
    if(allocated(A)) deallocate(A)
    if(allocated(B)) deallocate(B)
    allocate(A(MBPEpp,MBPEnn+MBPEdd-1),B(MBPEpp,1),stat=ierr)
    if(ierr/=0) then
      write(*,*) 'MBPE memory allocation failed!'
      iMBPEerr=1
      return
    end if
    do iparam=1,MBPEnparam
      A=(0.d0,0.d0)
      B=(0.d0,0.d0)
      do i=1,MBPEpp
        do j=1,MBPEdd-1
          A(i,j)=MBPEF(idomain,iparam,i)*MBPEX(idomain,i)**(j-1)
        end do
        do j=1,MBPEnn
          A(i,j+MBPEdd-1)=-MBPEX(idomain,i)**(j-1)
        end do
        B(i,1)=-MBPEF(idomain,iparam,i)*MBPEX(idomain,i)**(MBPEdd-1)
      end do
!       do i=1,MBPEpp
!         Amax=cdabs(A(i,1))
!         do j=1,MBPEnn+MBPEdd-1
!           Amax=max(Amax,cdabs(A(i,j)))
!         end do
!         do j=1,1,MBPEnn+MBPEdd-1
!           A(i,j)=A(i,j)/Amax
!         end do
!         B(i,1)=B(i,1)/Amax
!       end do       
      lwork=min(MBPEpp,MBPEnn+MBPEdd-1)+max(1,MBPEpp,MBPEnn+MBPEdd-1)        
      if(allocated(work)) deallocate(work)
      allocate(work(lwork),stat=ierr)
      call ZGELS('N',MBPEpp,MBPEnn+MBPEdd-1,1,A,MBPEpp,B,MBPEpp,work,lwork,info)
      do i=1,MBPEdd-1
        MBPED(idomain,iparam,i)=B(i,1)
      end do
      MBPED(idomain,iparam,MBPEdd)=1.d0
      do i=1,MBPEnn
        MBPEN(idomain,iparam,i)=B(MBPEdd+i-1,1)
      end do
    end do
    if(allocated(A)) deallocate(A)
    if(allocated(B)) deallocate(B)
    if(allocated(work)) deallocate(work)
    return
  end subroutine findpolecoef

  subroutine FindPoleCoef1(idomain)  
    implicit none
    integer(4) i,j,info,lwork,ierr,idomain,iparam
    complex(8),allocatable:: A(:,:),B(:,:),work(:)
    iMBPEerr=0
    call getorder(idomain)
    if(allocated(A)) deallocate(A)
    if(allocated(B)) deallocate(B)
    allocate(A(2*MBPEpp,MBPEnn+MBPEdd-1),B(2*MBPEpp,1),stat=ierr)
    if(ierr/=0) then
      if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
      iMBPEerr=1
      return
    end if
    do iparam=1,MBPEnparam
      A=(0.d0,0.d0)
      B=(0.d0,0.d0)
      do i=1,MBPEpp
        do j=1,MBPEdd-1
          A(2*i-1,j)=MBPEF(idomain,iparam,i)*MBPEX(idomain,i)**(j-1)
          A(2*i,  j)=MBPEF(idomain,iparam,i)*(j-1)*MBPEX(idomain,i)**(j-2)+  &
                     MBPEF1(idomain,iparam,i)*MBPEX(idomain,i)**(j-1)
        end do
        do j=1,MBPEnn
          A(2*i-1,j+MBPEdd-1)=-MBPEX(idomain,i)**(j-1)
          A(2*i,  j+MBPEdd-1)=-(j-1)*MBPEX(idomain,i)**(j-2)
        end do
        B(2*i-1,1)=-MBPEF(idomain,iparam,i)*MBPEX(idomain,i)**(MBPEdd-1)
        B(2*i,  1)=-MBPEF1(idomain,iparam,i)*MBPEX(idomain,i)**(MBPEdd-1)- &
                    (MBPEdd-1)*MBPEF(idomain,iparam,i)*MBPEX(idomain,i)**(MBPEdd-2)
      end do
!       do i=1,MBPEpp
!         Amax=cdabs(A(i,1))
!         do j=1,MBPEnn+MBPEdd-1
!           Amax=max(Amax,cdabs(A(i,j)))
!         end do
!         do j=1,1,MBPEnn+MBPEdd-1
!           A(i,j)=A(i,j)/Amax
!         end do
!         B(i,1)=B(i,1)/Amax
!       end do       
      lwork=min(2*MBPEpp,MBPEnn+MBPEdd-1)+max(1,2*MBPEpp,MBPEnn+MBPEdd-1)        
      if(allocated(work)) deallocate(work)
      allocate(work(lwork),stat=ierr)
      call ZGELS('N',2*MBPEpp,MBPEnn+MBPEdd-1,1,A,2*MBPEpp,B,2*MBPEpp,work,lwork,info)
      do i=1,MBPEdd-1
        MBPED(idomain,iparam,i)=B(i,1)
      end do
      MBPED(idomain,iparam,MBPEdd)=1.d0
      do i=1,MBPEnn
        MBPEN(idomain,iparam,i)=B(MBPEdd+i-1,1)
      end do
    end do
    if(allocated(A)) deallocate(A)
    if(allocated(B)) deallocate(B)
    if(allocated(work)) deallocate(work)
    return
  end subroutine findpolecoef1

  subroutine getorder(idomain)
! Determination of the order for given domain
    implicit none
    integer(4) idomain
    MBPEpp =MBPEp(idomain)
    MBPEpp0=MBPEp0(idomain)
    if(MBPEfover==1.d0) then
      MBPEnn=min(MBPEmaxorder,nint(0.5d0*Dble(MBPEpp))) !%
      MBPEdd=MBPEpp-MBPEnn+1
      MBPEnn0=min(MBPEmaxorder,nint(0.5d0*Dble(MBPEpp0))) !%
      MBPEdd0=MBPEpp0-MBPEnn0+1
    else
      MBPEnn=min(MBPEmaxorder,nint(0.5d0*Dble(MBPEpp)/MBPEfover)) !%
      MBPEdd=MBPEnn+1
      MBPEnn0=min(MBPEmaxorder,nint(0.5d0*Dble(MBPEpp0)/MBPEfover)) !%
      MBPEdd0=MBPEnn0+1
    end if 
  end subroutine getorder

  function getmaxdifference(idomain,xt)
! Determination of maximum difference between the current and previous MBPE approximations
    implicit none
    integer(4) idomain,i,num,iparam
    real(8) xp,xt,getmaxdifference,maxerror,minimum,maximum,dx,xx
    complex(8) ft1,ft2
    getmaxdifference=0.d0
    num=(MBPEX(idomain,MBPEp(idomain))-MBPEX(idomain,1))/MBPEdeltax
    dx=(MBPEX(idomain,MBPEp(idomain))-MBPEX(idomain,1))/dble(num-1)      
    do iparam=1,MBPEnparam
      maximum=max(MBPEftop,MBPEFmax(iparam))
      minimum=min(MBPEfbottom,MBPEFmin(iparam))
      maxerror=-1.d0
      do i=1,num
        xx=MBPEX(idomain,1)+(i-1)*dx
        ft1=getmbpevalue(idomain,iparam,xx,1)  ! return value from current MBPE
        if(maximum<cdabs(ft1)) then
          if(maxerror<cdabs(ft1)-maximum) then
            maxerror=cdabs(ft1)-maximum
            xp=xx
          end if
        else if(minimum>cdabs(ft1)) then
          if(maxerror<minimum-cdabs(ft1)) then
            maxerror=minimum-cdabs(ft1)
            xp=xx
          end if
        end if
      end do 
      maxerror=maxerror/(maximum-minimum)*100.d0       
      if(maxerror<0.d0) then !  All values are within range fbottom...MBPEftop -> search max.error
        do i=1,num
          xx=MBPEX(idomain,1)+(i-1)*dx
          ft1=getmbpevalue(idomain,iparam,xx,1)  ! return value from current MBPE
          ft2=getmbpevalue(idomain,iparam,xx,0)  ! return value from old MBPE
          if(maxerror<cdabs(ft1-ft2)) then 
            maxerror=cdabs(ft1-ft2)
            xp=xx
          end if
        end do   
        maxerror=maxerror/(MBPEFmax(iparam)-MBPEFmin(iparam))*100.d0
      end if
      if(getmaxdifference<maxerror) then
        getmaxdifference=maxerror
        xt=xp
      end if
    end do
    return
  end function getmaxdifference 

  function getmbpevalue(idomain,iparam,xt,index)
! returns the value of MBPE approximation for given domain, parameter and frequency point     
    implicit none
    integer(4) idomain,iparam,in,id,index
    real(8) xt
    complex(8) getmbpevalue,Dx,Nx
    Nx=(0.d0,0.d0)      
    Dx=(0.d0,0.d0) 
    call getorder(idomain)    
    if(index==1)then
      do in=1,MBPEnn
        Nx=Nx+MBPEN(idomain,iparam,in)*xt**(in-1)
      end do
      do id=1,MBPEdd
        Dx=Dx+MBPED(idomain,iparam,id)*xt**(id-1)
      end do
    else
      do in=1,MBPEnn0
        Nx=Nx+MBPEN0(idomain,iparam,in)*xt**(in-1)
      end do
      do id=1,MBPEdd0
        Dx=Dx+MBPED0(idomain,iparam,id)*xt**(id-1)
      end do
    end if
    getmbpevalue= Nx/Dx
    return
  end function getmbpevalue 

  function mbpevalue(iparam,xt)
! Returns the value of MBPE approximation for given parameter and frequency point         
    implicit none
    integer(4) idomain,iparam
    real(8) xt
    complex(8) mbpevalue
    mbpevalue=(0.d0,0.d0)
    if(xt<MBPEX(1,1)) then 
      mbpevalue=getmbpevalue(1,iparam,xt,1)
      return
    else if(xt>MBPEX(MBPEndomain,MBPEp(MBPEndomain))) then
      mbpevalue=getmbpevalue(MBPEndomain,iparam,xt,1)
      return
    end if
    do idomain=1,MBPEndomain
      if((xt>=MBPEX(idomain,1)).and.(xt<=MBPEX(idomain,MBPEp(idomain))))then
        mbpevalue=getmbpevalue(idomain,iparam,xt,1)
        return
      end if
    end do
    return
  end function mbpevalue     

! adds additional sample point into given domain
  subroutine addpoint(idomain,xt,FunRef)
    implicit none
    integer(4),external::FunRef
    integer(4) idomain,iparam,index,res
    real(8) xt
    complex(8) ft(1:MBPEnparam),ft1(1:MBPEnparam)
    call addr(idomain,xt,index)
    res=FunRef(xt,ft,ft1,MBPEnparam)
    if(l4.and.l5.and.(MBPEioutput>0)) write(*,*) 'MBPE x,f=',xt,cdabs(ft(1)) !%
    do iparam=1,MBPEnparam
      MBPEFmax(iparam)=max(MBPEFmax(iparam),cdabs(ft(iparam)))
      MBPEFmin(iparam)=min(MBPEFmin(iparam),cdabs(ft(iparam)))
      call addc(idomain,iparam,ft(iparam),ft1(iparam),index)
    end do
  end subroutine addpoint 

  subroutine addr(idomain,Xt,index)
! insert the point Xt in the MBPEX array at a reasonable position
    implicit none
    integer(4) i,ip,idomain,index,idxmax
    real(8) Xt,dxmax,dxmin,d
    dxmax=0.0d0
    dxmin=pBig
    idxmax=1
    do ip=2,MBPEpp ! find largest interval
      d=dabs(MBPEX(idomain,ip)-MBPEX(idomain,ip-1))
      if(d.gt.dxmax) then
        dxmax=d
        idxmax=ip
      end if
    end do
    do ip=1,MBPEpp ! find an appropriate location
      d=dabs(MBPEX(idomain,ip)-Xt)
      if(d.lt.dxmin) dxmin=d
      if(d<MBPEdeltax) then
        if(ip==MBPEpp) then
          Xt=MBPEX(idomain,ip)-min(MBPEdeltax,0.5d0*(MBPEX(idomain,ip)-MBPEX(idomain,ip-1)))
        else
          Xt=MBPEX(idomain,ip)+min(MBPEdeltax,0.5d0*(MBPEX(idomain,ip+1)-MBPEX(idomain,ip)))
        end if
        exit
      end if
    end do
    if((dxmin.lt.0.05d0*dxmax).and.(idxmax.gt.1)) then ! new position close to an existing position
      do ip=2,MBPEpp ! find interval containing Xt
        if((Xt>=MBPEX(idomain,ip-1)).and.(Xt<MBPEX(idomain,ip))) then
          d=dabs(MBPEX(idomain,ip)-MBPEX(idomain,ip-1))
          index=ip
          Exit
        end if
      end do
      if(d.gt.0.6*dxmax) then ! priviledge the center of Xt's own interval
        Xt=0.5d0*(MBPEX(idomain,index)+MBPEX(idomain,index-1))
      else
        Xt=0.5d0*(MBPEX(idomain,idxmax)+MBPEX(idomain,idxmax-1))
      end if
    end if
    do ip=1,MBPEpp-1 ! now insert Xt
      if((Xt>=MBPEX(idomain,ip)).and.(Xt<MBPEX(idomain,ip+1))) then
        do i=MBPEpp+1,ip+2,-1
          MBPEX(idomain,i)=MBPEX(idomain,i-1)
        end do
        MBPEX(idomain,ip+1)=Xt
        MBPEp(idomain)=MBPEpp+1
        index=ip+1
        exit
      end if
    end do
  end subroutine addr

  subroutine addc(idomain,iparam,ft,ft1,index)
    implicit none
    integer(4) ip,idomain,iparam,index
    complex(8) ft,ft1
    do ip=MBPEpp,index,-1
      MBPEF(idomain,iparam,ip+1)=MBPEF(idomain,iparam,ip)
      MBPEF1(idomain,iparam,ip+1)=MBPEF1(idomain,iparam,ip)
    end do
    MBPEF(idomain,iparam,index)=ft
    MBPEF1(idomain,iparam,index)=ft1
  end subroutine addc

  subroutine dividedomain(idomain)
! Divides domain into two domain with equal interval
    implicit none
    integer(4) p1,p2,idomain
    MBPEpp=MBPEp(idomain)
    p1=int(MBPEpp/2)+1
    p2=MBPEpp-p1+1
    MBPEp(MBPEndomain+1)=p2      
    MBPEX(MBPEndomain+1,1:p2)=MBPEX(idomain,p1:MBPEpp)
    MBPEF(MBPEndomain+1,1:MBPEnparam,1:p2)=MBPEF(idomain,1:MBPEnparam,p1:MBPEpp)
    MBPEF1(MBPEndomain+1,1:MBPEnparam,1:p2)=MBPEF1(idomain,1:MBPEnparam,p1:MBPEpp)
    MBPEN(MBPEndomain+1,1:MBPEnparam,1:MBPEmaxorder)=(0.d0,0.d0)
    MBPED(MBPEndomain+1,1:MBPEnparam,1:MBPEmaxorder)=(1.d0,0.d0)
    MBPEp(idomain)=p1
    MBPEN(idomain,1:MBPEnparam,1:MBPEmaxorder)=(0.d0,0.d0)
    MBPED(idomain,1:MBPEnparam,1:MBPEmaxorder)=(1.d0,0.d0)
    MBPEndomain=MBPEndomain+1
    return
  end subroutine dividedomain

  subroutine deallocate_MBPE_data
! Deallocates MBPE datas    
    implicit none
    if(allocated(MBPEX))    deallocate(MBPEX)             
    if(allocated(MBPEF))    deallocate(MBPEF)
    if(allocated(MBPEF1))   deallocate(MBPEF1)
    if(allocated(MBPEN))    deallocate(MBPEN) 
    if(allocated(MBPED))    deallocate(MBPED) 
    if(allocated(MBPEN0))   deallocate(MBPEN0) 
    if(allocated(MBPED0))   deallocate(MBPED0) 
    if(allocated(MBPEp))    deallocate(MBPEp)
    if(allocated(MBPEp0))   deallocate(MBPEp0)
    if(allocated(MBPEdone)) deallocate(MBPEdone)  
    if(allocated(MBPEFmax)) deallocate(MBPEFmax)
    if(allocated(MBPEFmin)) deallocate(MBPEFmin)       
    return
  end subroutine deallocate_MBPE_data

  function get_total_sample_number
    implicit none
    integer(4) get_total_sample_number
    get_total_sample_number=MBPEicalc
    return
  end function get_total_sample_number

  function get_domains_number
    implicit none
    integer(4) get_domains_number
    get_domains_number=MBPEndomain
    return
  end function get_domains_number

  function get_samples_number(idomain)
    implicit none
    integer(4) get_samples_number,idomain
    get_samples_number=MBPEp(idomain)
    return
  end function get_samples_number

  function get_sample_point(idomain,ip)
    implicit none
    real(8) get_sample_point
    integer(4) idomain,ip
    get_sample_point=MBPEX(idomain,ip)
    return
  end function get_sample_point

  function get_sample_value(idomain,iparam,ip)
    implicit none
    complex(8) get_sample_value
    integer(4) idomain,iparam,ip
    get_sample_value=MBPEF(idomain,iparam,ip)
    return
  end function get_sample_value  

  subroutine SaveMBPEdata(x0,x1,num,lcomplex,xi,fr,fi,ip0)
! save MBPE function approximation on the arrays xi,fr,fi
! use num points in the interval x0...x1
! write complex function values fr and fi when lcomplex is true; otherwise write fr only
    implicit none
    integer(4) i,iparam,num,ip0
    Logical lcomplex
    real(8) x0,x1,dl,xt
    real(8) xi(*),fi(MBPEnparam,*),fr(MBPEnparam,*)
    complex(8) ft(1:MBPEnparam)
    dl=(x1-x0)/(num-1)
    if(lcomplex) then 
      do i=1,num
        xt=x0+(i-1)*dl
        do iparam=1,MBPEnparam
          ft(iparam)=mbpevalue(iparam,xt)
        end do
        xi(ip0+i)=xt
        fr(1:MBPEnparam,ip0+i)=dreal(ft(1:MBPEnparam))
        fi(1:MBPEnparam,ip0+i)=dimag(ft(1:MBPEnparam))
      end do
    else
      do i=1,num
        xt=x0+(i-1)*dl
        do iparam=1,MBPEnparam
          ft(iparam)=mbpevalue(iparam,xt)
        end do
        xi(ip0+i)=xt
        fr(1:MBPEnparam,ip0+i)=dreal(ft(1:MBPEnparam))
      end do
    end if
  end subroutine SaveMBPEdata

  subroutine SaveMBPEdata0(num,lcomplex,xi,fr,fi)
! save MBPE function approximation on the arrays xi,fr,fi
! use num points in the array xi
! write complex function values fr and fi when lcomplex is true; otherwise write fr only
    implicit none
    integer(4) i,iparam,num
    Logical lcomplex
    real(8) xt
    real(8) xi(*),fi(MBPEnparam,*),fr(MBPEnparam,*)
    complex(8) ft(1:MBPEnparam)
    if(lcomplex) then 
      do i=1,num
        xt=xi(i)
        do iparam=1,MBPEnparam
          ft(iparam)=mbpevalue(iparam,xt)
        end do
        fr(1:MBPEnparam,i)=dreal(ft(1:MBPEnparam))
        fi(1:MBPEnparam,i)=dimag(ft(1:MBPEnparam))
      end do
    else
      do i=1,num
        xt=xi(i)
        do iparam=1,MBPEnparam
          ft(iparam)=mbpevalue(iparam,xt)
        end do
        fr(1:MBPEnparam,i)=dreal(ft(1:MBPEnparam))
      end do
    end if
  end subroutine SaveMBPEdata0

  subroutine SaveSamplePoints(filename)
! write the sample points used by MBPE and the corresponding function values on file
    implicit none
    character(*)filename
    integer(4) i,ipole,iparam
    complex(8) ft(1:MBPEnparam)
    open(10,file=filename)    
    call WriteStr(10,' CHFUN Version 1.0'C,i)
    write(10,*) '0 ',3*MBPEnparam+1
    write(10,'(a)') 'n MBPE'
    write(10,'(a)') 'z MBPE'
    do iparam=1,MBPEnparam
      write(10,'(a,1x,1i2)') 'fr MBPE',iparam
      write(10,'(a,1x,1i2)') 'fi MBPE',iparam
      write(10,'(a,1x,1i2)') 'fa MBPE',iparam
    end do
    do ipole=1,MBPEndomain
      do i=1,MBPEp(ipole)
        do iparam=1,MBPEnparam
          ft(iparam)=MBPEF(ipole,iparam,i)
          if(iparam.eq.1) then
            write(10,*) MBPEX(ipole,i),dreal(ft(iparam)),dimag(ft(iparam)),cdabs(ft(iparam))
          else
            write(10,*) dreal(ft(iparam)),dimag(ft(iparam)),cdabs(ft(iparam))
          end if
        end do
      end do
    end do
    close(10)
  end subroutine SaveSamplePoints

  Subroutine smoothMBPEsections(FunRef,x,tr,ti,npoints,nsections,noverlap,maxorder,errmax,xout,trout,tiout,ierr)
! a complex function t(x) is stored in the arrays x,tr,ti of lengths npoints
! subdivide the entire interval in nsections with overlap regions with noverlap points
! approximate with MBPE maximum order maxorder, find best order or exit when the error is below errmax (rel. error in %)
! compose a smooth function (contained in the arraxs xout,trout,tiout) form the MBPE approximations
! ierr is an error flag, should be 0
    implicit none
    Integer(4) npoints,nsections,noverlap,maxorder,np,nr,nr1,nrn,i,ierr,noverlap1,noverlap2
    Real(8) x(npoints),tr(npoints),ti(npoints),errmax,xout(npoints),trout(npoints),tiout(npoints)
    Real(8), allocatable:: xo(:,:),tro(:,:,:),tio(:,:,:),fover(:)
    Integer(4),external::FunRef
    Integer(4), Save:: ip1,ip2,no
    np=npoints/nsections    ! number of points for all sections, except first and last
    nr=npoints-np*nsections
    nr1=nr/2                ! first section gets nr1 additional points
    nrn=nr-nr1              ! last section gets nrn additional points
    nr1=np+nr1
    nrn=np+nrn
    no=max(np+2*noverlap,nr1+noverlap,nrn+noverlap)
    noverlap=min(noverlap,np)
    noverlap1=max(0,noverlap-1)
    noverlap2=2*noverlap
    allocate(xo(nsections,no),tro(nsections,1,no),tio(nsections,1,no),fover(max(1,noverlap2)),stat=ierr)
    if(ierr.ne.0) return
! factors in the overlap areas
    do i=1,noverlap2
      fover(i)=0.5d0*(1.0d0-dcos(dble(i)*3.1415926535898d0/dble(noverlap2+1)))
    end do
! MBPE analysis with optimal order for all sections
    if(nsections.gt.1) then
      ip1=1 ! first section
      ip2=nr1
      call getBestMBPE(FunRef,x,tr,ti,npoints,ip1,ip2+noverlap,maxorder,errmax,xo(1,1:nr1+noverlap), &
      &                tro(1,1,1:nr1+noverlap),tio(1,1,1:nr1+noverlap),nr1+noverlap)
      if(iMBPEerr.gt.0) return
      do i=2,nsections-1 ! inner sections
        ip1=ip2+1
        ip2=ip1+np-1
        call getBestMBPE(FunRef,x,tr,ti,npoints,ip1-noverlap,ip2+noverlap,maxorder,errmax,xo(i,1:np+noverlap2), &
        &                tro(i,1,1:np+noverlap2),tio(i,1,1:np+noverlap2),np+noverlap2)
        if(iMBPEerr.gt.0) return
        xo(i,1:np+noverlap2)=x(ip1-noverlap:ip2+noverlap)
        tro(i,1,1:np+noverlap2)=tr(ip1-noverlap:ip2+noverlap)
        tio(i,1,1:np+noverlap2)=ti(ip1-noverlap:ip2+noverlap)
      end do
      ip1=ip2+1 ! last section
      ip2=npoints
      call getBestMBPE(FunRef,x,tr,ti,npoints,ip1-noverlap,ip2,maxorder,errmax,xo(nsections,1:nrn+noverlap), &
      &                tro(nsections,1,1:nrn+noverlap),tio(nsections,1,1:nrn+noverlap),nrn+noverlap)
      if(iMBPEerr.gt.0) return
    else ! only first section
      ip1=1
      call getBestMBPE(FunRef,x,tr,ti,npoints,ip1,npoints,maxorder,errmax,xo(1,1:npoints),tro(1,1,1:npoints), &
      &                tio(1,1,1:npoints),npoints)
      if(iMBPEerr.gt.0) return
    end if
! compose result with overlapping MBPE approximations
    trout=0.0d0
    tiout=0.0d0
    if(nsections.gt.1) then
      ip1=1 ! first section
      ip2=nr1
      xout(ip1:ip2)=xo(1,1:nr1)
      trout(ip1:ip2-noverlap)=tro(1,1,1:nr1-noverlap)
      tiout(ip1:ip2-noverlap)=tio(1,1,1:nr1-noverlap)
      if(noverlap.gt.0) then
        trout(ip2-noverlap1:ip2+noverlap)=tro(1,1,ip2-noverlap1:ip2+noverlap)*(1.0d0-fover(1:noverlap2))
        tiout(ip2-noverlap1:ip2+noverlap)=tio(1,1,ip2-noverlap1:ip2+noverlap)*(1.0d0-fover(1:noverlap2))
      end if
      do i=2,nsections-1 ! inner sections
        ip1=ip2+1
        ip2=ip1+np-1
        xout(ip1:ip2)=xo(i,1:np)
        trout(ip1+noverlap:ip2-noverlap)=tro(i,1,noverlap2+1:np)
        tiout(ip1+noverlap:ip2-noverlap)=tio(i,1,noverlap2+1:np)
        if(noverlap.gt.0) then
          trout(ip1-noverlap:ip1+noverlap1)=trout(ip1-noverlap:ip1+noverlap1)+tro(i,1,1:noverlap2)*(fover(1:noverlap2))
          tiout(ip1-noverlap:ip1+noverlap1)=tiout(ip1-noverlap:ip1+noverlap1)+tio(i,1,1:noverlap2)*(fover(1:noverlap2))
          trout(ip2-noverlap1:ip2+noverlap)=tro(i,1,np+1:np+noverlap2)*(1.0d0-fover(1:noverlap2))
          tiout(ip2-noverlap1:ip2+noverlap)=tio(i,1,np+1:np+noverlap2)*(1.0d0-fover(1:noverlap2))
        end if
      end do
      ip1=ip2+1 ! last section
      ip2=npoints
      i=nsections
      xout(ip1:ip2)=xo(i,1:nrn)
      if(noverlap.gt.0) then
        trout(ip1-noverlap:ip1+noverlap1)=trout(ip1-noverlap:ip1+noverlap1)+tro(i,1,1:noverlap2)*(fover(1:noverlap2))
        tiout(ip1-noverlap:ip1+noverlap1)=tiout(ip1-noverlap:ip1+noverlap1)+tio(i,1,1:noverlap2)*(fover(1:noverlap2))
      end if
      trout(ip1+noverlap:ip2)=tro(i,1,noverlap2+1:nrn+noverlap)
      tiout(ip1+noverlap:ip2)=tio(i,1,noverlap2+1:nrn+noverlap)
    else ! only first section
      xout(1:npoints)=xo(1,1:npoints)
      trout(1:npoints)=tro(1,1,1:npoints)
      tiout(1:npoints)=tio(1,1,1:npoints)
    end if
    deallocate(xo,tro,tio,stat=ierr)
  end Subroutine smoothMBPEsections

  Subroutine getBestMBPE(FunRef,x,tr,ti,n,ip1,ip2,maxo,errmax,xo,tro,tio,no)
! find best MBPE order <=maxo of the function t(x) stored in the arrays x,tr,ti
! use only the array elements ip1...ip2
! start with order 1 and increase until the error is below errmax (rel. error in %) or maxo is reached
! return the MBPE approximation in the arrays xo,tro,tio
    implicit none
    Integer(4) n,no,res,i,ip0,ip1,ip2,np,iopt,maxo,io
    Real(8) errmax,x(n),tr(n),ti(n),xo(no),tro(1,no),tio(1,no),err,fun,errr,errr0
    Integer(4),external::FunRef
    Logical lcomplex
    np=ip2-ip1+1
    io=MBPEioutput
    MBPEioutput  = 0     ! no display output on screen
    MBPEndiv     = np    ! initial number of points = input points
    MBPEnparam   = 1     ! Number of functions to be analyzed simultaneously by MBPE
    MBPExstart   = x(ip1)! MBPE range xmin 
    MBPExend     = x(ip2)! MBPE range xmax
    MBPEmaxerror = 1.d0  ! no stopping error required for single run
    MBPEftop     = pBig  ! 
    MBPEfbottom  = nBig  !
    MBPEmaxncalc = np    !
    MBPEntest    = np    ! number of output points
    MBPEluseDerivative=.false.  ! Input: function t(x) without derivative
    lcomplex=.true.   ! Output File: x,tr,ti; if false: output c,tabs
    iopt=1
    errr0=1.0e30
    do MBPEmaxorder=1,maxo
      MBPEmaxp=MBPEndiv
      res=adaptive_MBPE(FunRef)
      if(iMBPEerr.gt.0) return
      ip0=0
      call SaveMBPEdata(MBPExstart,MBPExend,MBPEntest,lcomplex,xo(1:no),tro(MBPEnparam,1:no),tio(MBPEnparam,1:no),ip0)
      err=0.0d0
      fun=0.0d0
      do i=1,np
        err=err+abs(tr(i)-tro(1,i))+abs(ti(i)-tio(1,i))
        fun=fun+abs(tr(i))+abs(ti(i))
      end do
      err=err/dble(2*np)
      fun=fun/dble(2*np)
      errr=100.0d0*err/fun
      if(errr.lt.errr0) then
        iopt=MBPEmaxorder
        errr0=errr
      end if
      call deallocate_MBPE_data 
      if(errr0.lt.errmax) then
        if(MBPEioutput>-1) write(*,*) 'MBPE error boundary reached:',errr0
        iMBPEerr=-1
        exit
      end if
    end do
    MBPEmaxorder=iopt
    MBPEmaxp=MBPEndiv
    MBPEioutput=io
    res=adaptive_MBPE(FunRef)    
    if(iMBPEerr.gt.0) return
    ip0=0
    call SaveMBPEdata(MBPExstart,MBPExend,MBPEntest,lcomplex,xo(1:no),tro(MBPEnparam,1:no),tio(MBPEnparam,1:no),ip0)
  end Subroutine getBestMBPE

! auxiliary: 2D downhill simplex search

  Subroutine SMP2D(fk,zll,zur,dzr,dfr,itmax,zm,dzm,it,ier)
! downhill simplex for minimum search of the real function fk in the complex plane
! ier=-1: minimum zm inaccurate, function flat
! ier= 0: minimum zm accurate
! ier= 1: minimum zm inaccurate, limit of iterations reached
    Implicit none
    Integer(4) itmax,it,ier,ilo,ihi,inhi,i
    Real(8) Y(3),Y4,rtol,dev,dzr,dfr,ytry,ysave,ydum(9)
    Complex(8) X(3),X4,zll,zur,psum,zm,dzm
    Real(8), External :: fk
    it=0
    X(1)=zll
    X(2)=zur
    X(3)=0.5d0*(zll+zur+Dcmplx(-Dimag(zur-zll),Dble(zur-zll)))
    X4=0.5d0*(zll+zur+Dcmplx(Dimag(zur-zll),-Dble(zur-zll)))
	  do i=1,3
	    Y(i)=-fk(X(i),ydum)
      if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' SMP2D: A z,f=',X(i),-Y(i)
	  end do
	  Y4=-fk(X4,ydum)
    if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' SMP2D: B z,f=',X4,-Y4
	  it=it+4
    if((Y(1).gt.Y(2)).and.(Y(1).gt.Y(3)).and.(Y(1).gt.Y4)) then
      X(1)=X4
      Y(1)=Y4
    else if((Y(2).gt.Y(1)).and.(Y(2).gt.Y(3)).and.(Y(2).gt.Y4)) then
      X(2)=X4
      Y(2)=Y4
    else if((Y(3).gt.Y(1)).and.(Y(3).gt.Y(2)).and.(Y(3).gt.Y4)) then
      X(3)=X4
      Y(3)=Y4
    end if
! Optimize
	  psum=X(1)+X(2)+X(3)
	  do
	    ilo=1
	    if(Y(1).lt.Y(2)) then 
	      ihi=1
		    inhi=2
	    else
	      ihi=2
		    inhi=1
	    end if
	    do i=1,3
	      if(Y(i).ge.Y(ilo)) ilo=i
	      if(Y(i).lt.Y(ihi)) then
	        inhi=ihi
	        ihi=i
	      else 
		      if((Y(i).lt.Y(inhi)).and.(i.ne.ihi)) inhi=i
		    end if
	    end do
  ! distance of points
	    rtol=abs(X(ihi)-X(ilo))/abs(X(ilo))
  ! deviation of function values
	    dev=dabs(Y(ihi)-Y(ilo))/dabs(Y(ihi)+Y(ilo))
  ! termination criterion
	    if((rtol.lt.dzr).or.(dev.lt.dfr).or.(it.ge.itmax)) Exit
  ! next steps
	    ytry=Trial(fk,X,Y,psum,ihi,-1.0d0,it)
	    if(ytry.ge.Y(ilo)) then 
	      ytry=Trial(fk,X,Y,psum,ihi,2.0d0,it)
	    else 
	      if(ytry.le.Y(inhi)) then
	        ysave=Y(ihi)
	        ytry=Trial(fk,X,Y,psum,ihi,0.5d0,it)
	        if(ytry.le.ysave) then
	          do i=1,3
	            if(i.ne.ilo) then
			          X(i)=0.5d0*(X(i)+X(ilo))
		            Y(i)=-fk(X(i),ydum)
                if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' SMP2D: C z,f=',X(i),-Y(i)
			        end if
		        end do
	          it=it+2
	          psum=X(1)+X(2)+X(3)
		      end if
	      end if
	    end if 
	  end do
    zm=X(ilo)
    dzm=X(ihi)-X(ilo)
    ier=0
    if(it.ge.itmax) ier=1
    if(dev.lt.dfr) ier=-1
  end Subroutine SMP2D

  Real(8) Function Trial(fk,X,Y,psum,ihi,fac,it)
! auxiliary for SMP2D
    Implicit None
	  Integer(4) ihi,it
	  Real(8) Y(3),fac,fac1,fac2,factor,ytry,ydum(9)
	  Complex(8) X(3),psum,ptry
    Real(8), External :: fk
	  factor=fac
	  fac1=(1.0d0-fac)*0.5d0
	  fac2=fac1-fac
! new point	
	  ptry=psum*fac1-X(ihi)*fac2
! evaluate
	  ytry=-fk(ptry,ydum)
    if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' Trial: A z,f=',ptry,-ytry
    it=it+1
! introduce into simplex if better
	  if(ytry.gt.Y(ihi)) then
	    Y(ihi)=ytry
	    psum=psum+ptry-X(ihi)
	    X(ihi)=ptry
	  end if
	  Trial=ytry
  end Function Trial

END MODULE CHSPL