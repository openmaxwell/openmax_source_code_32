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
MODULE CHPFD

! Domains

  USE CHDOM

  SAVE

    Real(8) ddt,Ampl,fac,w,wmax,wmax1,dPFDwind
	  Integer(4) ia,ja,ka,ib,jb,kb,npml,ial,jal,kal,ibl,jbl,kbl,iEtype,iUtype
    Logical lloc,lPFDHfield,lPFDEfield,lPFDX,lPFDY,lPFDZ,lPFDpoint,lPFDMMP,lPFDsoft,lPFDhard,lPFDplane
    Integer(2), Allocatable :: iD(:,:,:,:)
    Real(8), Allocatable :: Dx(:,:,:),Dy(:,:,:),Dz(:,:,:),Ex(:,:,:),Ey(:,:,:),Ez(:,:,:), &
    &                       Bx(:,:,:),By(:,:,:),Bz(:,:,:),Hx(:,:,:),Hy(:,:,:),Hz(:,:,:), &
    & Ix(:,:,:),Iy(:,:,:),Iz(:,:,:),Jx(:,:,:),Jy(:,:,:),Jz(:,:,:), &
    & Kx(:,:,:),Ky(:,:,:),Kz(:,:,:),Lx(:,:,:),Ly(:,:,:),Lz(:,:,:), &
    & Mx(:,:,:),My(:,:,:),Mz(:,:,:),Nx(:,:,:),Ny(:,:,:),Nz(:,:,:), &
    & Ox(:,:,:),Oy(:,:,:),Oz(:,:,:),Px(:,:,:),Py(:,:,:),Pz(:,:,:), &
    & O1x(:,:,:),O1y(:,:,:),O1z(:,:,:),P1x(:,:,:),P1y(:,:,:),P1z(:,:,:), &
    & O2x(:,:,:),O2y(:,:,:),O2z(:,:,:),P2x(:,:,:),P2y(:,:,:),P2z(:,:,:), &
    & gax(:,:,:),gay(:,:,:),gaz(:,:,:),gbx(:,:,:),gby(:,:,:),gbz(:,:,:), &
    & gcx(:,:,:),gcy(:,:,:),gcz(:,:,:),gdx(:,:,:),gdy(:,:,:),gdz(:,:,:), &
    & fax(:,:,:),fay(:,:,:),faz(:,:,:),fbx(:,:,:),fby(:,:,:),fbz(:,:,:), &
    & fcx(:,:,:),fcy(:,:,:),fcz(:,:,:),fdx(:,:,:),fdy(:,:,:),fdz(:,:,:), &
    & EzInc(:),HyInc(:),HzInc(:),EyInc(:), &
    & gi1(:),gi2(:),gi3(:),gj1(:),gj2(:),gj3(:),gk1(:),gk2(:),gk3(:), &
    & fi1(:),fi2(:),fi3(:),fj1(:),fj2(:),fj3(:),fk1(:),fk2(:),fk3(:), &
    & iDxl(:,:,:),iDxh(:,:,:),iDyl(:,:,:),iDyh(:,:,:),iDzl(:,:,:),iDzh(:,:,:), &
    & iBxl(:,:,:),iBxh(:,:,:),iByl(:,:,:),iByh(:,:,:),iBzl(:,:,:),iBzh(:,:,:)

  CONTAINS

  Subroutine TransformPFD3D(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10)
    Real(8) xn,xxn,rot,ot,cosot,sinot,Rea(6),Ima(6),rF(6),r(3),t,ft,f,df,cosft,sinft,fl
	  Integer(4) i,j,k,n,ih,idum,ierr,lout,j1,j2,k1,k2,l
    Integer(4), intent(in) :: Initialize
    lfcFld=lfcFldAll
    lMMPstat=.false.
		call OutTxt('t2','PFD-transform'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
! check if all conditions are met
    if(.not.lEcFld) then
      idum=MessageBoxQQ('E must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(.not.lHcFld) then
      idum=MessageBoxQQ('H must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if((.not.lxcFld).or.(.not.lycFld).or.(.not.lzcFld)) then
      idum=MessageBoxQQ('X,Y,Z components must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C,&
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(.not.lrGrd) then
      idum=MessageBoxQQ('Regular must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! initialization
    ddt=dtrFld
    dPFDwind=sqrt((PFDxmax-PFDxmin)**2+(PFDymax-PFDymin)**2+(PFDzmax-PFDzmin)**2)
    if(Initialize.eq.1) then
      call getPFDl(iPFDt,lPFDHfield,lPFDEfield,lPFDX,lPFDY,lPFDZ,lPFDpoint,lPFDMMP,lPFDsoft,lPFDhard,lPFDplane)
      nPFDcFld=0_4
      nPFDsFld(1:nPFDsens)=0_4
      ia=max(1,nPFDil)
      ja=max(1,nPFDjl)
      ka=max(1,nPFDkl)
      ib=nPFDi-max(0,nPFDih)
      jb=nPFDj-max(0,nPFDjh)
      kb=nPFDk-max(0,nPFDkh)
      ial=ia
      jal=ja
      kal=ka
      ibl=ib
      jbl=jb
      kbl=kb
      if((nPFDil.gt.0).and.lPFDplane.and.lPFDsoft) then
        ia=ia+1
        ial=ia+nPFDsLayers+2
      end if
      if((nPFDjl.gt.0).and.lPFDplane.and.lPFDsoft) then
        ja=ja+1
        jal=ja+nPFDsLayers+2
      end if
      if((nPFDkl.gt.0).and.lPFDplane.and.lPFDsoft) then
        ka=ka+1
        kal=ka+nPFDsLayers+2
      end if
      if((nPFDih.gt.0).and.lPFDplane.and.lPFDsoft) then
        ib=ib-1
        ibl=ib-nPFDsLayers-2
      end if
      if((nPFDjh.gt.0).and.lPFDplane.and.lPFDsoft) then
        jb=jb-1
        jbl=jb-nPFDsLayers-2
      end if
      if((nPFDkh.gt.0).and.lPFDplane.and.lPFDsoft) then
        kb=kb-1
        kbl=kb-nPFDsLayers-2
      end if
      call getPFDdomType()
! allocate memory for arrays
      call DeAllocatePFD()
      Allocate(Dx(nPFDi,nPFDj,nPFDk),Dy(nPFDi,nPFDj,nPFDk),Dz(nPFDi,nPFDj,nPFDk), &
      &        Ex(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Ey(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Ez(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1), &
      &        Bx(nPFDi,nPFDj,nPFDk),By(nPFDi,nPFDj,nPFDk),Bz(nPFDi,nPFDj,nPFDk), &
      &        Hx(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Hy(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Hz(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1), &
      & fax(nPFDi,nPFDj,nPFDk),fay(nPFDi,nPFDj,nPFDk),faz(nPFDi,nPFDj,nPFDk), &
      & gax(nPFDi,nPFDj,nPFDk),gay(nPFDi,nPFDj,nPFDk),gaz(nPFDi,nPFDj,nPFDk), &
      & EzInc(0:nPFDi+1),HyInc(0:nPFDi+1), &
      & gi1(nPFDi),gi2(nPFDi),gi3(nPFDi),gj1(nPFDj),gj2(nPFDj),gj3(nPFDj),gk1(nPFDk),gk2(nPFDk),gk3(nPFDk), &
      & fi1(nPFDi),fi2(nPFDi),fi3(nPFDi),fj1(nPFDj),fj2(nPFDj),fj3(nPFDj),fk1(nPFDk),fk2(nPFDk),fk3(nPFDk), &
      & iDxl(ia,nPFDj,nPFDk),iDxh(nPFDih+1,nPFDj,nPFDk),iDyl(nPFDi,ja,nPFDk),iDyh(nPFDi,nPFDjh+1,nPFDk), &
      & iDzl(nPFDi,nPFDj,ka),iDzh(nPFDi,nPFDj,nPFDkh+1), &
      & iBxl(ia,nPFDj,nPFDk),iBxh(nPFDih+1,nPFDj,nPFDk),iByl(nPFDi,ja,nPFDk),iByh(nPFDi,nPFDjh+1,nPFDk), &
      & iBzl(nPFDi,nPFDj,ka),iBzh(nPFDi,nPFDj,nPFDkh+1), &
      & iD(6,nPFDi,nPFDj,nPFDk),stat=ierr)
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbx(nPFDi,nPFDj,nPFDk),gby(nPFDi,nPFDj,nPFDk),gbz(nPFDi,nPFDj,nPFDk), &
        & Ix(nPFDi,nPFDj,nPFDk),Iy(nPFDi,nPFDj,nPFDk),Iz(nPFDi,nPFDj,nPFDk),stat=ierr)
        Ix=0.0d0
        Iy=0.0d0
        Iz=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kx(nPFDi,nPFDj,nPFDk),Ky(nPFDi,nPFDj,nPFDk),Kz(nPFDi,nPFDj,nPFDk), &
        & Mx(nPFDi,nPFDj,nPFDk),My(nPFDi,nPFDj,nPFDk),Mz(nPFDi,nPFDj,nPFDk),stat=ierr)
        Kx=0.0d0
        Ky=0.0d0
        Kz=0.0d0
        Mx=0.0d0
        My=0.0d0
        Mz=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcx(nPFDi,nPFDj,nPFDk),gcy(nPFDi,nPFDj,nPFDk),gcz(nPFDi,nPFDj,nPFDk),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Ox(nPFDi,nPFDj,nPFDk),Oy(nPFDi,nPFDj,nPFDk),Oz(nPFDi,nPFDj,nPFDk),stat=ierr)
        Ox=0.0d0
        Oy=0.0d0
        Oz=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdx(nPFDi,nPFDj,nPFDk),gdy(nPFDi,nPFDj,nPFDk),gdz(nPFDi,nPFDj,nPFDk), &
        & O1x(nPFDi,nPFDj,nPFDk),O1y(nPFDi,nPFDj,nPFDk),O1z(nPFDi,nPFDj,nPFDk), &
        & O2x(nPFDi,nPFDj,nPFDk),O2y(nPFDi,nPFDj,nPFDk),O2z(nPFDi,nPFDj,nPFDk),stat=ierr)
        O1x=0.0d0
        O1y=0.0d0
        O1z=0.0d0
        O2x=0.0d0
        O2y=0.0d0
        O2z=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbx(nPFDi,nPFDj,nPFDk),fby(nPFDi,nPFDj,nPFDk),fbz(nPFDi,nPFDj,nPFDk), &
        & Jx(nPFDi,nPFDj,nPFDk),Jy(nPFDi,nPFDj,nPFDk),Jz(nPFDi,nPFDj,nPFDk),stat=ierr)
        Jx=0.0d0
        Jy=0.0d0
        Jz=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lx(nPFDi,nPFDj,nPFDk),Ly(nPFDi,nPFDj,nPFDk),Lz(nPFDi,nPFDj,nPFDk), &
        & Nx(nPFDi,nPFDj,nPFDk),Ny(nPFDi,nPFDj,nPFDk),Nz(nPFDi,nPFDj,nPFDk),stat=ierr)
        Lx=0.0d0
        Ly=0.0d0
        Lz=0.0d0
        Nx=0.0d0
        Ny=0.0d0
        Nz=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcx(nPFDi,nPFDj,nPFDk),fcy(nPFDi,nPFDj,nPFDk),fcz(nPFDi,nPFDj,nPFDk),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Px(nPFDi,nPFDj,nPFDk),Py(nPFDi,nPFDj,nPFDk),Pz(nPFDi,nPFDj,nPFDk),stat=ierr)
        Px=0.0d0
        Py=0.0d0
        Pz=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdx(nPFDi,nPFDj,nPFDk),fdy(nPFDi,nPFDj,nPFDk),fdz(nPFDi,nPFDj,nPFDk), &
        & P1x(nPFDi,nPFDj,nPFDk),P1y(nPFDi,nPFDj,nPFDk),P1z(nPFDi,nPFDj,nPFDk), &
        & P2x(nPFDi,nPFDj,nPFDk),P2y(nPFDi,nPFDj,nPFDk),P2z(nPFDi,nPFDj,nPFDk),stat=ierr)
        P1x=0.0d0
        P1y=0.0d0
        P1z=0.0d0
        P2x=0.0d0
        P2y=0.0d0
        P2z=0.0d0
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocatePFD()
        return
      end if
      lPFDalloc=.true.
! initialize arrays and constants
		  call OutTxt('t2','Initialize PFD'C)
      if(lfcFld) then ! original OpenMaXwell field
        cFld=(0.0d0,0.0d0)
      else
        dFld=0.0d0
      end if
      EzInc=0.0d0 ! Incident plane wave
      HyInc=0.0d0
      Ex=0.0d0 ! PFDfield
      Ey=0.0d0
      Ez=0.0d0
      Dx=0.0d0
      Dy=0.0d0
      Dz=0.0d0
      Bx=0.0d0
      By=0.0d0
      Bz=0.0d0
      Hx=0.0d0
      Hy=0.0d0
      Hz=0.0d0
      iDxl=0.0d0 ! UPML auxiliary arrays
      iDxh=0.0d0
      iBxl=0.0d0
      iBxh=0.0d0
      iDyl=0.0d0
      iDyh=0.0d0
      iByl=0.0d0
      iByh=0.0d0
      iDzl=0.0d0
      iDzh=0.0d0
      iBzl=0.0d0
      iBzh=0.0d0
      gi1=0.0d0 ! UPML parameters
      fi1=0.0d0
      gi2=1.0d0
      fi2=1.0d0
      gi3=1.0d0
      fi3=1.0d0
      gj1=0.0d0
      fj1=0.0d0
      gj2=1.0d0
      fj2=1.0d0
      gj3=1.0d0
      fj3=1.0d0
      gk1=0.0d0
      fk1=0.0d0
      gk2=1.0d0
      fk2=1.0d0
      gk3=1.0d0
      fk3=1.0d0
      do i=1,nPFDil
        xxn=Dble(nPFDil-i+1)/Dble(nPFDil)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fi1(i)=xn
        gi2(i)=1.0d0/(1.0d0+xn)
        gi3(i)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDil-i+1)-0.5d0)/Dble(nPFDil)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gi1(i)=xn
        fi2(i)=1.0d0/(1.0d0+xn)
        fi3(i)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do i=1,nPFDih
        xxn=Dble(nPFDih-i+1)/Dble(nPFDih)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fi1(nPFDi-i)=xn
        gi2(nPFDi-i)=1.0d0/(1.0d0+xn)
        gi3(nPFDi-i)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDih-i+1)-0.5d0)/Dble(nPFDih)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gi1(nPFDi-i)=xn
        fi2(nPFDi-i)=1.0d0/(1.0d0+xn)
        fi3(nPFDi-i)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do j=1,nPFDjl
        xxn=Dble(nPFDjl-j+1)/Dble(nPFDjl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fj1(j)=xn
        gj2(j)=1.0d0/(1.0d0+xn)
        gj3(j)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDjl-j+1)-0.5d0)/Dble(nPFDjl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gj1(j)=xn
        fj2(j)=1.0d0/(1.0d0+xn)
        fj3(j)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do j=1,nPFDjh
        xxn=Dble(nPFDjh-j+1)/Dble(nPFDjh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fj1(nPFDj-j)=xn
        gj2(nPFDj-j)=1.0d0/(1.0d0+xn)
        gj3(nPFDj-j)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDjh-j+1)-0.5d0)/Dble(nPFDjh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gj1(nPFDj-j)=xn
        fj2(nPFDj-j)=1.0d0/(1.0d0+xn)
        fj3(nPFDj-j)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do k=1,nPFDkl
        xxn=Dble(nPFDkl-k+1)/Dble(nPFDkl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fk1(k)=xn
        gk2(k)=1.0d0/(1.0d0+xn)
        gk3(k)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDkl-k+1)-0.5d0)/Dble(nPFDkl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gk1(k)=xn
        fk2(k)=1.0d0/(1.0d0+xn)
        fk3(k)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do k=1,nPFDkh
        xxn=Dble(nPFDkh-k+1)/Dble(nPFDkh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fk1(nPFDk-k)=xn
        gk2(nPFDk-k)=1.0d0/(1.0d0+xn)
        gk3(nPFDk-k)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDkh-k+1)-0.5d0)/Dble(nPFDkh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gk1(nPFDk-k)=xn
        fk2(nPFDk-k)=1.0d0/(1.0d0+xn)
        fk3(nPFDk-k)=(1.0d0-xn)/(1.0d0+xn)
      end do
      call getPFDmat3D(.false.)
      if(iPFDft.eq.0) then ! initialize field
        if(lPFDMMP) then ! MMP solution
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call getMMPFld(i,j,k,4_4,iD(4,i,j,k))
                Hx(i,j,k)=Dble(FldExp(4))
                call getMMPFld(i,j,k,5_4,iD(5,i,j,k))
                Hy(i,j,k)=Dble(FldExp(5))
                call getMMPFld(i,j,k,6_4,iD(6,i,j,k))
                Hz(i,j,k)=Dble(FldExp(6))
              end do
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                Hx(i,j,k)=CHrnd(-1.0d0,1.0d0)
                Hy(i,j,k)=CHrnd(-1.0d0,1.0d0)
                Hz(i,j,k)=CHrnd(-1.0d0,1.0d0)
              end do
            end do
          end do
        end if
      end if
      Ampl=1.0d0
! scale amplitude: Propagate incident wave through the UPML area
      if(lPFDplane) then
        t=trFld
        Ampl=0.0d0
        k=min(100000_4,int4((Dble(2_4*ia)*PFDdx*Dsqrt(Eps0*Mue0)+PFDfTmax+2.0d0*PFDfTau)/ddt))
        do n=1,k
          t=t+0.5d0*ddt
          ot=2.0d0*Pi*fcFld*t
          ft=timeDepPFD(t,0)
          do i=2,nPFDi
            EzInc(i)=gi3(i)*EzInc(i)+gi2(i)*ddt*(HyInc(i)-HyInc(i-1))/(PFDdx*Eps0)
          end do
          EzInc(1)=ft
          if(nPFDih.gt.-1) EzInc(nPFDi)=0.0d0 ! PEC
          Ampl=max(Ampl,EzInc(ia+1))
          t=t+0.5d0*ddt
          do i=1,nPFDi-1
            HyInc(i)=fi3(i)*HyInc(i)+fi2(i)*ddt*(EzInc(i+1)-EzInc(i))/(PFDdx*Mue0)
          end do
          HyInc(nPFDi)=2.0d0*HyInc(nPFDi-1)-HyInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HyInc(nPFDi)=0.0d0 ! PMC
        end do
        Ampl=Dsqrt(Zw0)/Max(Ampl,1.0d-100)
        EzInc=0.0d0
        HyInc=0.0d0
        t=trFld
      end if
    end if
    if((abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
! scale cFld
      if(lfcFld.and.(nPFDcFld.gt.0_4)) then
        fac=Dble(nPFDcFld)/(ddt*Dble(fcFld))
        cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
      end if
! scale sensor field array
      if(nPFDf.gt.0_4) then
        do k=1,nPFDsens
          if(nPFDsFld(k).lt.1) cycle
          fac=Dble(nPFDsFld(k))/(ddt*Dble(fcFld))
          cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
        end do
      end if
    end if
! iterate over time: main loop
    j1=1
    j2=nPFDj
    if(nPFDjl.ge.0) j1=2+nPFDjl
    if(nPFDjh.ge.0) j2=nPFDj-1-nPFDjh
    k1=1
    k2=nPFDk
    if(nPFDkl.ge.0) k1=2+nPFDkl
    if(nPFDkh.ge.0) k2=nPFDk-1-nPFDkh
    wmax1=0.0d0
    if(nIterPFD.lt.1) then
      PFDwmax=0.0d0
      do i=1,nPFDi
        do j=1,nPFDj
          do k=1,nPFDk
            Ex(i,j,k)=1.0d0/(Eps0*gax(i,j,k))
            Ey(i,j,k)=1.0d0/(Eps0*gay(i,j,k))
            Ez(i,j,k)=1.0d0/(Eps0*gaz(i,j,k))
            Hx(i,j,k)=1.0d0/(Mue0*fax(i,j,k))
            Hy(i,j,k)=1.0d0/(Mue0*fay(i,j,k))
            Hz(i,j,k)=1.0d0/(Mue0*faz(i,j,k))
          end do
        end do
      end do
    else
      do n=1,nIterPFD
        if(lStopThread) return
		    call OutTxt('t2','PFD transform'C)
        call IntToStr(n,0,0,SpaceTExt,lout)
		    call OutTxt('n2',SpaceTExt(1:lout))
        call IntToStr(Int4(nIterPFD),0,0,SpaceTExt,lout)
		    call OutTxt('m2',SpaceTExt(1:lout))
! time dependence
        trFld=trFld+0.5d0*dtrFld
        ot=2.0d0*Pi*fcFld*trFld
        cosot=cos(ot)
        sinot=sin(ot)
        ft=timeDepPFD(trFld,0)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=2,nPFDi
            EzInc(i)=gi3(i)*EzInc(i)+gi2(i)*ddt*(HyInc(i)-HyInc(i-1))/(PFDdx*Eps0)
          end do
          EzInc(1)=Ampl*ft
          if(nPFDih.gt.-1) EzInc(nPFDi)=0.0d0 ! PEC
        end if
! Dx
!!$OMP parallel do
        do i=1,nPFDi
          ih=i-ib
          do j=1,nPFDj
            do k=1,nPFDk
              rot=(Hz(i,j,k)-Hz(i,j-1,k))/PFDdy-(Hy(i,j,k)-Hy(i,j,k-1))/PFDdz
              if(i.lt.ia) then
                iDxl(i,j,k)=iDxl(i,j,k)+rot
                rot=rot+gi1(i)*iDxl(i,j,k)
              else if(i.gt.ib) then
                iDxh(ih,j,k)=iDxh(ih,j,k)+rot
                rot=rot+gi1(i)*iDxh(ih,j,k)
              end if
              if((j.lt.ja).or.(j.gt.jb).or.(k.lt.ka).or.(k.gt.kb)) then
                Dx(i,j,k)=gj3(j)*gk3(k)*Dx(i,j,k)+gj2(j)*gk2(k)*ddt*rot
              else
                Dx(i,j,k)=Dx(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Dx, plane wave
            if((nPFDkl.gt.0).and.(nPFDkh.gt.0)) then
              do i=ial,ibl
                do j=jal,jbl
                  Dx(i,j,kal)=Dx(i,j,kal)+ddt*HyInc(i)/PFDdz
                  Dx(i,j,kbl+1)=Dx(i,j,kbl+1)-ddt*HyInc(i)/PFDdz
                end do
              end do
            else if(nPFDkl.gt.0) then
              do i=ial,ibl
                do j=jal,jbl
                  Dx(i,j,kal)=Dx(i,j,kal)+ddt*HyInc(i)/PFDdz
                end do
              end do
            else if(nPFDkh.gt.0) then
              do i=ial,ibl
                do j=jal,jbl
                  Dx(i,j,kbl+1)=Dx(i,j,kbl+1)-ddt*HyInc(i)/PFDdz
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dx(iPFDs(l),jPFDs(l),kPFDs(l))=Dx(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(eDom(iD(1,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dx(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(eDom(iD(1,iPFDs(l),jPFDs(l),kPFDs(l))))*Eps0*fl
              end do
            end if
          end if
        end if
! Dy
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            ih=j-jb
            do k=1,nPFDk
              rot=(Hx(i,j,k)-Hx(i,j,k-1))/PFDdz-(Hz(i,j,k)-Hz(i-1,j,k))/PFDdx
              if(j.lt.ja) then
                iDyl(i,j,k)=iDyl(i,j,k)+rot
                rot=rot+gj1(j)*iDyl(i,j,k)
              else if(j.gt.jb) then
                iDyh(i,ih,k)=iDyh(i,ih,k)+rot
                rot=rot+gj1(j)*iDyh(i,ih,k)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(k.lt.ka).or.(k.gt.kb)) then
                Dy(i,j,k)=gk3(k)*gi3(i)*Dy(i,j,k)+gk2(k)*gi2(i)*ddt*rot
              else
                Dy(i,j,k)=Dy(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dy(iPFDs(l),jPFDs(l),kPFDs(l))=Dy(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(eDom(iD(2,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dy(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(eDom(iD(2,iPFDs(l),jPFDs(l),kPFDs(l))))*Eps0*fl
              end do
            end if
          end if
        end if
! Dz
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            do k=1,nPFDk
              ih=k-kb
              rot=(Hy(i,j,k)-Hy(i-1,j,k))/PFDdx-(Hx(i,j,k)-Hx(i,j-1,k))/PFDdy
              if(k.lt.ka) then
                iDzl(i,j,k)=iDzl(i,j,k)+rot
                rot=rot+gk1(k)*iDzl(i,j,k)
              else if(k.gt.kb) then
                iDzh(i,j,ih)=iDzh(i,j,ih)+rot
                rot=rot+gk1(k)*iDzh(i,j,ih)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(j.lt.ja).or.(j.gt.jb)) then
                Dz(i,j,k)=gi3(i)*gj3(j)*Dz(i,j,k)+gi2(i)*gj2(j)*ddt*rot
              else
                Dz(i,j,k)=Dz(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Dz, plane wave
            if(lPFDsoft) then ! scattered field formulation
              do j=jal,jbl
                do k=kal,kbl
                  Dz(ial,j,k)=Dz(ial,j,k)-ddt*HyInc(ial-1)/PFDdx
                  Dz(ibl+1,j,k)=Dz(ibl+1,j,k)+ddt*HyInc(ibl)/PFDdx
                end do
              end do
            else ! total field formulation
              do j=j1,j2
                do k=k1,k2
                  Dz(ia+1,j,k)=EzInc(ia+1)*Eps0
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dz(iPFDs(l),jPFDs(l),kPFDs(l))=Dz(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(eDom(iD(3,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dz(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(eDom(iD(3,iPFDs(l),jPFDs(l),kPFDs(l))))*Eps0*fl
              end do
            end if
          end if
        end if
! get E from D
        select case(iEtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,1_4,Dx(i,j,k),Ex(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k), &
                & gax(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k),iD(1,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,2_4,Dy(i,j,k),Ey(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k), & 
                & gay(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k),iD(2,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,3_4,Dz(i,j,k),Ez(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k), &
                & gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,1_4,Dx(i,j,k),Ex(i,j,k),Ix(i,j,k),Ix(i,j,k),Ix(i,j,k),Ix(i,j,k), &
                & Ix(i,j,k),Ix(i,j,k),gax(i,j,k),gbx(i,j,k),gax(i,j,k),gax(i,j,k),iD(1,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,2_4,Dy(i,j,k),Ey(i,j,k),Iy(i,j,k),Iy(i,j,k),Iy(i,j,k),Iy(i,j,k), & 
                & Iy(i,j,k),Iy(i,j,k),gay(i,j,k),gby(i,j,k),gay(i,j,k),gay(i,j,k),iD(2,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,3_4,Dz(i,j,k),Ez(i,j,k),Iz(i,j,k),Iz(i,j,k),Iz(i,j,k),Iz(i,j,k), &
                & Iz(i,j,k),Iz(i,j,k),gaz(i,j,k),gbz(i,j,k),gaz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,1_4,Dx(i,j,k),Ex(i,j,k),Ix(i,j,k),Kx(i,j,k),Mx(i,j,k),Ix(i,j,k), &
                & Ix(i,j,k),Ix(i,j,k),gax(i,j,k),gbx(i,j,k),gax(i,j,k),gax(i,j,k),iD(1,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,2_4,Dy(i,j,k),Ey(i,j,k),Iy(i,j,k),Ky(i,j,k),My(i,j,k),Iy(i,j,k), & 
                & Iy(i,j,k),Iy(i,j,k),gay(i,j,k),gby(i,j,k),gay(i,j,k),gay(i,j,k),iD(2,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,3_4,Dz(i,j,k),Ez(i,j,k),Iz(i,j,k),Kz(i,j,k),Mz(i,j,k),Iz(i,j,k), &
                & Iz(i,j,k),Iz(i,j,k),gaz(i,j,k),gbz(i,j,k),gaz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,1_4,Dx(i,j,k),Ex(i,j,k),Ix(i,j,k),Kx(i,j,k),Mx(i,j,k),Ix(i,j,k), &
                & Ix(i,j,k),Ix(i,j,k),gax(i,j,k),gbx(i,j,k),gcx(i,j,k),gax(i,j,k),iD(1,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,2_4,Dy(i,j,k),Ey(i,j,k),Iy(i,j,k),Ky(i,j,k),My(i,j,k),Iy(i,j,k), & 
                & Iy(i,j,k),Iy(i,j,k),gay(i,j,k),gby(i,j,k),gcy(i,j,k),gay(i,j,k),iD(2,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,3_4,Dz(i,j,k),Ez(i,j,k),Iz(i,j,k),Kz(i,j,k),Mz(i,j,k),Iz(i,j,k), &
                & Iz(i,j,k),Iz(i,j,k),gaz(i,j,k),gbz(i,j,k),gcz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,1_4,Dx(i,j,k),Ex(i,j,k),Ix(i,j,k),Kx(i,j,k),Mx(i,j,k),Ox(i,j,k), &
                & Ix(i,j,k),Ix(i,j,k),gax(i,j,k),gbx(i,j,k),gcx(i,j,k),gax(i,j,k),iD(1,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,2_4,Dy(i,j,k),Ey(i,j,k),Iy(i,j,k),Ky(i,j,k),My(i,j,k),Oy(i,j,k), & 
                & Iy(i,j,k),Iy(i,j,k),gay(i,j,k),gby(i,j,k),gcy(i,j,k),gay(i,j,k),iD(2,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,3_4,Dz(i,j,k),Ez(i,j,k),Iz(i,j,k),Kz(i,j,k),Mz(i,j,k),Oz(i,j,k), &
                & Iz(i,j,k),Iz(i,j,k),gaz(i,j,k),gbz(i,j,k),gcz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,1_4,Dx(i,j,k),Ex(i,j,k),Ix(i,j,k),Kx(i,j,k),Mx(i,j,k),Ox(i,j,k), &
                & O1x(i,j,k),O2x(i,j,k),gax(i,j,k),gbx(i,j,k),gcx(i,j,k),gdx(i,j,k),iD(1,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,2_4,Dy(i,j,k),Ey(i,j,k),Iy(i,j,k),Ky(i,j,k),My(i,j,k),Oy(i,j,k), & 
                & O1y(i,j,k),O2y(i,j,k),gay(i,j,k),gby(i,j,k),gcy(i,j,k),gdy(i,j,k),iD(2,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,3_4,Dz(i,j,k),Ez(i,j,k),Iz(i,j,k),Kz(i,j,k),Mz(i,j,k),Oz(i,j,k), &
                & O1z(i,j,k),O2z(i,j,k),gaz(i,j,k),gbz(i,j,k),gcz(i,j,k),gdz(i,j,k),iD(3,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        end select
! boundary conditions
        call PFDboundary('3','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        ot=2.0d0*Pi*fcFld*trFld
        cosot=cos(ot)
        sinot=sin(ot)
        ft=timeDepPFD(trFld,0)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=1,nPFDi-1
            HyInc(i)=fi3(i)*HyInc(i)+fi2(i)*ddt*(EzInc(i+1)-EzInc(i))/(PFDdx*Mue0)
          end do
          HyInc(nPFDi)=2.0d0*HyInc(nPFDi-1)-HyInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HyInc(nPFDi)=0.0d0 ! PMC
        end if
! Bx
!!$OMP parallel do
        do i=1,nPFDi
          ih=i-ib
          do j=1,nPFDj
            do k=1,nPFDk
              rot=(Ey(i,j,k+1)-Ey(i,j,k))/PFDdz-(Ez(i,j+1,k)-Ez(i,j,k))/PFDdy
              if(i.lt.ia) then
                iBxl(i,j,k)=iBxl(i,j,k)+rot
                rot=rot+fi1(i)*iBxl(i,j,k)
              else if(i.gt.ib) then
                iBxh(ih,j,k)=iBxh(ih,j,k)+rot
                rot=rot+fi1(i)*iBxh(ih,j,k)
              end if
              if((j.lt.ja).or.(j.gt.jb).or.(k.lt.ka).or.(k.gt.kb)) then
                Bx(i,j,k)=fj3(j)*fk3(k)*Bx(i,j,k)+fj2(j)*fk2(k)*ddt*rot
              else
                Bx(i,j,k)=Bx(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Bx, plane wave
            if((nPFDjl.gt.0).and.(nPFDjh.gt.0)) then
              do i=ial,ibl
                do k=kal,kbl
                  Bx(i,jal-1,k)=Bx(i,jal-1,k)+ddt*EzInc(i)/PFDdy
                  Bx(i,jbl,k)=Bx(i,jbl,k)-ddt*EzInc(i)/PFDdy
                end do
              end do
            else if(nPFDjl.gt.0) then
              do i=ial,ibl
                do k=kal,kbl
                  Bx(i,jal-1,k)=Bx(i,jal-1,k)+ddt*EzInc(i)/PFDdy
                end do
              end do
            else if(nPFDjh.gt.0) then
              do i=ial,ibl
                do k=kal,kbl
                  Bx(i,jbl,k)=Bx(i,jbl,k)-ddt*EzInc(i)/PFDdy
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bx(iPFDs(l),jPFDs(l),kPFDs(l))=Bx(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(uDom(iD(4,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bx(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(uDom(iD(4,iPFDs(l),jPFDs(l),kPFDs(l))))*Mue0*fl
              end do
            end if
          end if
        end if
! By
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            ih=j-jb
            do k=1,nPFDk
              rot=(Ez(i+1,j,k)-Ez(i,j,k))/PFDdx-(Ex(i,j,k+1)-Ex(i,j,k))/PFDdz
              if(j.lt.ja) then
                iByl(i,j,k)=iByl(i,j,k)+rot
                rot=rot+fj1(j)*iByl(i,j,k)
              else if(j.gt.jb) then
                iByh(i,ih,k)=iByh(i,ih,k)+rot
                rot=rot+fj1(j)*iByh(i,ih,k)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(k.lt.ka).or.(k.gt.kb)) then
                By(i,j,k)=fk3(k)*fi3(i)*By(i,j,k)+fk2(k)*fi2(i)*ddt*rot
              else
                By(i,j,k)=By(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident By, plane wave
            if(lPFDsoft) then
              do j=jal,jbl
                do k=kal,kbl
                  By(ial-1,j,k)=By(ial-1,j,k)-ddt*EzInc(ial)/PFDdx
                  By(ibl,j,k)=By(ibl,j,k)+ddt*EzInc(ibl+1)/PFDdx
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                By(iPFDs(l),jPFDs(l),kPFDs(l))=By(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(uDom(iD(5,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                By(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(uDom(iD(5,iPFDs(l),jPFDs(l),kPFDs(l))))*Mue0*fl
              end do
            end if
          end if
        end if
! Bz
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            do k=1,nPFDk
              ih=k-kb
              rot=(Ex(i,j+1,k)-Ex(i,j,k))/PFDdy-(Ey(i+1,j,k)-Ey(i,j,k))/PFDdx
              if(k.lt.ka) then
                iBzl(i,j,k)=iBzl(i,j,k)+rot
                rot=rot+fk1(k)*iBzl(i,j,k)
              else if(k.gt.kb) then
                iBzh(i,j,ih)=iBzh(i,j,ih)+rot
                rot=rot+fk1(k)*iBzh(i,j,ih)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(j.lt.ja).or.(j.gt.jb)) then
                Bz(i,j,k)=fi3(i)*fj3(j)*Bz(i,j,k)+fi2(i)*fj2(j)*ddt*rot
              else
                Bz(i,j,k)=Bz(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bz(iPFDs(l),jPFDs(l),kPFDs(l))=Bz(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(uDom(iD(6,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bz(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(uDom(iD(6,iPFDs(l),jPFDs(l),kPFDs(l))))*Mue0*fl
              end do
            end if
          end if
        end if
! get H from B
        select case(iUtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,4_4,Bx(i,j,k),Hx(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k), &
                & fax(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k),iD(4,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,5_4,By(i,j,k),Hy(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k), &
                & fay(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k),iD(5,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,6_4,Bz(i,j,k),Hz(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k), &
                & faz(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,4_4,Bx(i,j,k),Hx(i,j,k),Jx(i,j,k),Jx(i,j,k),Jx(i,j,k),Jx(i,j,k), &
                & Jx(i,j,k),Jx(i,j,k),fax(i,j,k),fbx(i,j,k),fax(i,j,k),fax(i,j,k),iD(4,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,5_4,By(i,j,k),Hy(i,j,k),Jy(i,j,k),Jy(i,j,k),Jy(i,j,k),Jy(i,j,k), &
                & Jy(i,j,k),Jy(i,j,k),fay(i,j,k),fby(i,j,k),fay(i,j,k),fay(i,j,k),iD(5,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,6_4,Bz(i,j,k),Hz(i,j,k),Jz(i,j,k),Jz(i,j,k),Jz(i,j,k),Jz(i,j,k), &
                & Jz(i,j,k),Jz(i,j,k),faz(i,j,k),fbz(i,j,k),faz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,4_4,Bx(i,j,k),Hx(i,j,k),Jx(i,j,k),Lx(i,j,k),Nx(i,j,k),Jx(i,j,k), &
                & Jx(i,j,k),Jx(i,j,k),fax(i,j,k),fbx(i,j,k),fax(i,j,k),fax(i,j,k),iD(4,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,5_4,By(i,j,k),Hy(i,j,k),Jy(i,j,k),Ly(i,j,k),Ny(i,j,k),Jy(i,j,k), &
                & Jy(i,j,k),Jy(i,j,k),fay(i,j,k),fby(i,j,k),fay(i,j,k),fay(i,j,k),iD(5,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,6_4,Bz(i,j,k),Hz(i,j,k),Jz(i,j,k),Lz(i,j,k),Nz(i,j,k),Jz(i,j,k), &
                & Jz(i,j,k),Jz(i,j,k),faz(i,j,k),fbz(i,j,k),faz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,4_4,Bx(i,j,k),Hx(i,j,k),Jx(i,j,k),Lx(i,j,k),Nx(i,j,k),Jx(i,j,k), &
                & Jx(i,j,k),Jx(i,j,k),fax(i,j,k),fbx(i,j,k),fcx(i,j,k),fax(i,j,k),iD(4,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,5_4,By(i,j,k),Hy(i,j,k),Jy(i,j,k),Ly(i,j,k),Ny(i,j,k),Jy(i,j,k), &
                & Jy(i,j,k),Jy(i,j,k),fay(i,j,k),fby(i,j,k),fcy(i,j,k),fay(i,j,k),iD(5,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,6_4,Bz(i,j,k),Hz(i,j,k),Jz(i,j,k),Lz(i,j,k),Nz(i,j,k),Jz(i,j,k), &
                & Jz(i,j,k),Jz(i,j,k),faz(i,j,k),fbz(i,j,k),fcz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,4_4,Bx(i,j,k),Hx(i,j,k),Jx(i,j,k),Lx(i,j,k),Nx(i,j,k),Px(i,j,k), &
                & Jx(i,j,k),Jx(i,j,k),fax(i,j,k),fbx(i,j,k),fcx(i,j,k),fax(i,j,k),iD(4,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,5_4,By(i,j,k),Hy(i,j,k),Jy(i,j,k),Ly(i,j,k),Ny(i,j,k),Py(i,j,k), &
                & Jy(i,j,k),Jy(i,j,k),fay(i,j,k),fby(i,j,k),fcy(i,j,k),fay(i,j,k),iD(5,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,6_4,Bz(i,j,k),Hz(i,j,k),Jz(i,j,k),Lz(i,j,k),Nz(i,j,k),Pz(i,j,k), &
                & Jz(i,j,k),Jz(i,j,k),faz(i,j,k),fbz(i,j,k),fcz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call DB2EH(i,j,k,lPFDX,4_4,Bx(i,j,k),Hx(i,j,k),Jx(i,j,k),Lx(i,j,k),Nx(i,j,k),Px(i,j,k), &
                & P1x(i,j,k),P2x(i,j,k),fax(i,j,k),fbx(i,j,k),fcx(i,j,k),fdx(i,j,k),iD(4,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDY,5_4,By(i,j,k),Hy(i,j,k),Jy(i,j,k),Ly(i,j,k),Ny(i,j,k),Py(i,j,k), &
                & P1y(i,j,k),P2y(i,j,k),fay(i,j,k),fby(i,j,k),fcy(i,j,k),fdy(i,j,k),iD(5,i,j,k),ft,cosot,sinot)
                call DB2EH(i,j,k,lPFDZ,6_4,Bz(i,j,k),Hz(i,j,k),Jz(i,j,k),Lz(i,j,k),Nz(i,j,k),Pz(i,j,k), &
                & P1z(i,j,k),P2z(i,j,k),faz(i,j,k),fbz(i,j,k),fcz(i,j,k),fdz(i,j,k),iD(6,i,j,k),ft,cosot,sinot)
              end do
            end do
          end do
        end select
! boundary conditions
        call PFDboundary('3','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getPFDrFld3D(r,rF(1:6))
                Rea(1:6)=Dble(cFld(1:6,i,j,k))+cosot*rF(1:6)
                Ima(1:6)=Dimag(cFld(1:6,i,j,k))+sinot*rF(1:6)
                cFld(1:6,i,j,k)=DCmplx(Rea(1:6),Ima(1:6))
              end do
            end do
          end do
        end if
        if(nPFDf.gt.0) then
! DFT in sensor points
          wmax1=wmax
          wmax=0.0d0
          do k=1,nPFDsens
            if((trFld.lt.PFDsensT(k)).or.(trFld.gt.(PFDsensT(k)+PFDsensD(k)))) Cycle
            nPFDsFld(k)=nPFDsFld(k)+1_4
            r(1)=PFDsensX(k)
            r(2)=PFDsensY(k)
            r(3)=PFDsensZ(k)
            call getPFDrFld3D(r,rF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              ot=2.0d0*Pi*f*trFld
              cosft=cos(ot)
              sinft=sin(ot)
              f=f+df
              Rea(1:6)=Dble(cPFDsens(1:6,i,k))+cosft*rF(1:6)
              Ima(1:6)=Dimag(cPFDsens(1:6,i,k))+sinft*rF(1:6)
              cPFDsens(1:6,i,k)=DCmplx(Rea(1:6),Ima(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(rF(1)**2+rF(2)**2+rF(3)**2)+Mue0*(rF(4)**2+rF(5)**2+rF(6)**2)
            wmax=max(wmax,w)
          end do
          PFDwmax=max(wmax,PFDwmax)
          if((wmax.lt.wmax1).and.(Dble(n)*3.0d8*ddt.gt.dPFDwind*PFDdfact)) then
            if(wmax.lt.PFDwmax*PFDwfact) then
              write(*,*) 'stopping criterion reached, n=',nPFDsFld(1)
              write(*,*) 'wmax,wmax1,PFDwmax=',wmax,wmax1,PFDwmax
              PFDwmax=0.0d0
              Exit
            end if
          end if
        end if
      end do
    end if
! main loop terminated, compute dFld or scale cFld
    if(.not.lfcFld) then
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            r=getcGrd(i,j,k)
            call getPFDrFld3D(r,dFld(1:6,i,j,k))
          end do
        end do
      end do
    else if((nPFDcFld.gt.0).and.(abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
      fac=ddt*Dble(fcFld)/Dble(nPFDcFld)
      cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
    end if
! unscale sensor field array
    if((nPFDf.gt.0).and.(abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
      do k=1,nPFDsens
        if(nPFDsFld(k).lt.1) cycle
        fac=ddt*Dble(fcFld)/Dble(nPFDsFld(k))
        cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
      end do
    end if
! get PFD field in sensor point
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getPFDrFld3D(r(1:3),dPFDsens(1:6,k))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformPFD3D

  Subroutine TransformPFD2E(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10)
    Real(8) xn,xxn,rot,ot,cosot,sinot,Rea(6),Ima(6),rF(6),t,ft,r(3),f,df,cosft,sinft,fl
	  Integer(4) i,j,k,n,idum,ierr,lout,j1,j2,l
    Integer(4), intent(in) :: Initialize
    lfcFld=lfcFldAll
    lMMPstat=.false.
		call OutTxt('t2','PFD-transform'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
! check if all conditions are met
    if(.not.lEcFld) then
      idum=MessageBoxQQ('E must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(.not.lHcFld) then
      idum=MessageBoxQQ('H must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if((.not.lxcFld).or.(.not.lycFld).or.(.not.lzcFld)) then
      idum=MessageBoxQQ('X,Y,Z components must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C,&
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(.not.lrGrd) then
      idum=MessageBoxQQ('Regular must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! initialization
    ddt=dtrFld
    dPFDwind=sqrt((PFDxmax-PFDxmin)**2+(PFDymax-PFDymin)**2)
    if(Initialize.eq.1) then
      call getPFDl(iPFDt,lPFDHfield,lPFDEfield,lPFDX,lPFDY,lPFDZ,lPFDpoint,lPFDMMP,lPFDsoft,lPFDhard,lPFDplane)
      nPFDcFld=0_4
      nPFDsFld(1:nPFDsens)=0_4
      ia=max(1,nPFDil)
      ja=max(1,nPFDjl)
      ib=nPFDi-max(0,nPFDih)
      jb=nPFDj-max(0,nPFDjh)
      ial=ia
      jal=ja
      ibl=ib
      jbl=jb
      if((nPFDil.gt.0).and.lPFDplane.and.lPFDsoft) then
        ia=ia+1
        ial=ia+nPFDsLayers+2
      end if
      if((nPFDjl.gt.0).and.lPFDplane.and.lPFDsoft) then
        ja=ja+1
        jal=ja+nPFDsLayers+2
      end if
      if((nPFDih.gt.0).and.lPFDplane.and.lPFDsoft) then
        ib=ib-1
        ibl=ib-nPFDsLayers-2
      end if
      if((nPFDjh.gt.0).and.lPFDplane.and.lPFDsoft) then
        jb=jb-1
        jbl=jb-nPFDsLayers-2
      end if
      call getPFDdomType()
! allocate memory for arrays
      call DeAllocatePFD()
      Allocate(Dz(nPFDi,nPFDj,1), &
      &        Ez(0:nPFDi+1,0:nPFDj+1,1), &
      &        Bx(nPFDi,nPFDj,1),By(nPFDi,nPFDj,1), &
      &        Hx(0:nPFDi+1,0:nPFDj+1,1),Hy(0:nPFDi+1,0:nPFDj+1,1), &
      & fax(nPFDi,nPFDj,1),fay(nPFDi,nPFDj,1), &
      & gaz(nPFDi,nPFDj,1), &
      & EzInc(0:nPFDi+1),HyInc(0:nPFDi+1), &
      & gi2(nPFDi),gi3(nPFDi),gj2(nPFDj),gj3(nPFDj), &
      & fi2(nPFDi),fi3(nPFDi),fj2(nPFDj),fj3(nPFDj), &
      & iD(3,nPFDi,nPFDj,1),stat=ierr)
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbz(nPFDi,nPFDj,1),Iz(nPFDi,nPFDj,1),stat=ierr)
        Iz=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kz(nPFDi,nPFDj,1),Mz(nPFDi,nPFDj,1),stat=ierr)
        Kz=0.0d0
        Mz=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcz(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Oz(nPFDi,nPFDj,1),stat=ierr)
        Oz=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdz(nPFDi,nPFDj,1),O1z(nPFDi,nPFDj,1),O2z(nPFDi,nPFDj,1),stat=ierr)
        O1z=0.0d0
        O2z=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbx(nPFDi,nPFDj,1),fby(nPFDi,nPFDj,1),Jx(nPFDi,nPFDj,1),Jy(nPFDi,nPFDj,1),stat=ierr)
        Jx=0.0d0
        Jy=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lx(nPFDi,nPFDj,1),Ly(nPFDi,nPFDj,1),Nx(nPFDi,nPFDj,1),Ny(nPFDi,nPFDj,1),stat=ierr)
        Lx=0.0d0
        Ly=0.0d0
        Nx=0.0d0
        Ny=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcx(nPFDi,nPFDj,1),fcy(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Px(nPFDi,nPFDj,1),Py(nPFDi,nPFDj,1),stat=ierr)
        Px=0.0d0
        Py=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdx(nPFDi,nPFDj,1),fdy(nPFDi,nPFDj,1), &
        & P1x(nPFDi,nPFDj,1),P1y(nPFDi,nPFDj,1),P2x(nPFDi,nPFDj,1),P2y(nPFDi,nPFDj,1),stat=ierr)
        P1x=0.0d0
        P1y=0.0d0
        P2x=0.0d0
        P2y=0.0d0
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocatePFD()
        return
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocatePFD()
        return
      end if
      lPFDalloc=.true.
! initialize arrays and constants
		  call OutTxt('t2','Initialize PFD'C)
      if(lfcFld) then ! original OpenMaXwell field
        cFld=(0.0d0,0.0d0)
      else
        dFld=0.0d0
      end if
      EzInc=0.0d0 ! Incident plane wave
      HyInc=0.0d0
      Ez=0.0d0 ! PFDfield
      Dz=0.0d0
      Bx=0.0d0
      By=0.0d0
      Hx=0.0d0
      Hy=0.0d0
      gi2=1.0d0 ! UPML parameters
      fi2=1.0d0
      gi3=1.0d0
      fi3=1.0d0
      gj2=1.0d0
      fj2=1.0d0
      gj3=1.0d0
      fj3=1.0d0
      do i=1,nPFDil
        xxn=Dble(nPFDil-i+1)/Dble(nPFDil)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gi2(i)=1.0d0/(1.0d0+xn)
        gi3(i)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDil-i+1)-0.5d0)/Dble(nPFDil)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fi2(i)=1.0d0/(1.0d0+xn)
        fi3(i)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do i=1,nPFDih
        xxn=Dble(nPFDih-i+1)/Dble(nPFDih)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gi2(nPFDi-i)=1.0d0/(1.0d0+xn)
        gi3(nPFDi-i)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDih-i+1)-0.5d0)/Dble(nPFDih)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fi2(nPFDi-i-1)=1.0d0/(1.0d0+xn)
        fi3(nPFDi-i-1)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do j=1,nPFDjl
        xxn=Dble(nPFDjl-j+1)/Dble(nPFDjl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gj2(j)=1.0d0/(1.0d0+xn)
        gj3(j)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDjl-j+1)-0.5d0)/Dble(nPFDjl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fj2(j)=1.0d0/(1.0d0+xn)
        fj3(j)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do j=1,nPFDjh
        xxn=Dble(nPFDjh-j+1)/Dble(nPFDjh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gj2(nPFDj-j)=1.0d0/(1.0d0+xn)
        gj3(nPFDj-j)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDjh-j+1)-0.5d0)/Dble(nPFDjh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fj2(nPFDj-j-1)=1.0d0/(1.0d0+xn)
        fj3(nPFDj-j-1)=(1.0d0-xn)/(1.0d0+xn)
      end do
      call getPFDmat2E(.false.)
      if(iPFDft.eq.0) then ! initialize field
        if(lPFDMMP) then ! MMP solution
          do i=1,nPFDi
            do j=1,nPFDj
              call getMMPFld(i,j,0_4,4_4,iD(1,i,j,1))
              Hx(i,j,1)=Dble(FldExp(4))
              call getMMPFld(i,j,0_4,5_4,iD(2,i,j,1))
              Hy(i,j,1)=Dble(FldExp(5))
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              Hx(i,j,1)=CHrnd(-1.0d0,1.0d0)
              Hy(i,j,1)=CHrnd(-1.0d0,1.0d0)
            end do
          end do
        end if
      end if
      Ampl=1.0d0
! scale amplitude: Propagate incident wave through the UPML area
      if(lPFDplane) then 
        t=trFld
        Ampl=0.0d0
        k=min(100000_4,int4((Dble(2_4*ia)*PFDdx*Dsqrt(Eps0*Mue0)+PFDfTmax+2.0d0*PFDfTau)/ddt))
        do n=1,k
          t=t+0.5d0*ddt
          ot=2.0d0*Pi*fcFld*t
          ft=timeDepPFD(t,0)
          do i=2,nPFDi
            EzInc(i)=gi3(i)*EzInc(i)+gi2(i)*ddt*(HyInc(i)-HyInc(i-1))/(PFDdx*Eps0)
          end do
          EzInc(1)=ft
          if(nPFDih.gt.-1) EzInc(nPFDi)=0.0d0 ! PEC
          Ampl=max(Ampl,EzInc(ia+1))
          t=t+0.5d0*ddt
          do i=1,nPFDi-1
            HyInc(i)=fi3(i)*HyInc(i)+fi2(i)*ddt*(EzInc(i+1)-EzInc(i))/(PFDdx*Mue0)
          end do
          HyInc(nPFDi)=2.0d0*HyInc(nPFDi-1)-HyInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HyInc(nPFDi)=0.0d0 ! PMC
        end do
        Ampl=Dsqrt(Zw0)/Max(Ampl,1.0d-100)
        EzInc=0.0d0
        HyInc=0.0d0
        t=trFld
      end if
    end if
    if((abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
! scale cFld
      if(lfcFld.and.(nPFDcFld.gt.0_4)) then
        fac=Dble(nPFDcFld)/(ddt*Dble(fcFld))
        cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
      end if
! scale sensor field array
      if(nPFDf.gt.0_4) then
        do k=1,nPFDsens
          if(nPFDsFld(k).lt.1) cycle
          fac=Dble(nPFDsFld(k))/(ddt*Dble(fcFld))
          cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
        end do
      end if
    end if
! iterate over time: main loop
    j1=1
    j2=nPFDj
    if(nPFDjl.ge.0) j1=2+nPFDjl
    if(nPFDjh.ge.0) j2=nPFDj-1-nPFDjh
    wmax1=0.0d0
    if(nIterPFD.lt.1) then
      PFDwmax=0.0d0
      do i=1,nPFDi
        do j=1,nPFDj
          Ez(i,j,1)=1.0d0/(Eps0*gaz(i,j,1))
          Hx(i,j,1)=1.0d0/(Mue0*fax(i,j,1))
          Hy(i,j,1)=1.0d0/(Mue0*fay(i,j,1))
        end do
      end do
    else
      do n=1,nIterPFD
        if(lStopThread) return
		    call OutTxt('t2','PFD transform'C)
        call IntToStr(n,0,0,SpaceTExt,lout)
		    call OutTxt('n2',SpaceTExt(1:lout))
        call IntToStr(Int4(nIterPFD),0,0,SpaceTExt,lout)
		    call OutTxt('m2',SpaceTExt(1:lout))
! time dependence
        trFld=trFld+0.5d0*dtrFld
        ot=2.0d0*Pi*fcFld*trFld
        cosot=cos(ot)
        sinot=sin(ot)
        ft=timeDepPFD(trFld,0)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=2,nPFDi
            EzInc(i)=gi3(i)*EzInc(i)+gi2(i)*ddt*(HyInc(i)-HyInc(i-1))/(PFDdx*Eps0)
          end do
          EzInc(1)=Ampl*ft
          if(nPFDih.gt.-1) EzInc(nPFDi)=0.0d0 ! PEC
        end if
! Dz
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Hy(i,j,1)-Hy(i-1,j,1))/PFDdx-(Hx(i,j,1)-Hx(i,j-1,1))/PFDdy
            Dz(i,j,1)=gi3(i)*gj3(j)*Dz(i,j,1)+gi2(i)*gj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Dz, plane wave
            if(lPFDsoft) then
              do j=jal,jbl
                Dz(ial,j,1)=Dz(ial,j,1)-ddt*HyInc(ial-1)/PFDdx
                Dz(ibl+1,j,1)=Dz(ibl+1,j,1)+ddt*HyInc(ibl)/PFDdx
              end do
            else ! total field formulation
              do j=j1,j2
                Dz(ia+1,j,1)=EzInc(ia+1)*Eps0
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dz(iPFDs(l),jPFDs(l),1)=Dz(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(3,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dz(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(3,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! get E from D
        select case(iEtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,3_4,Dz(i,j,1),Ez(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1), &
              & gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,3_4,Dz(i,j,1),Ez(i,j,1),Iz(i,j,1),Iz(i,j,1),Iz(i,j,1),Iz(i,j,1), &
              & Iz(i,j,1),Iz(i,j,1),gaz(i,j,1),gbz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,3_4,Dz(i,j,1),Ez(i,j,1),Iz(i,j,1),Kz(i,j,1),Mz(i,j,1),Iz(i,j,1), &
              & Iz(i,j,1),Iz(i,j,1),gaz(i,j,1),gbz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,3_4,Dz(i,j,1),Ez(i,j,1),Iz(i,j,1),Kz(i,j,1),Mz(i,j,1),Iz(i,j,1), &
              & Iz(i,j,1),Iz(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,3_4,Dz(i,j,1),Ez(i,j,1),Iz(i,j,1),Kz(i,j,1),Mz(i,j,1),Oz(i,j,1), &
              & Iz(i,j,1),Iz(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,3_4,Dz(i,j,1),Ez(i,j,1),Iz(i,j,1),Kz(i,j,1),Mz(i,j,1),Oz(i,j,1), &
              & O1z(i,j,1),O2z(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gdz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        end select
! boundary conditions
        call PFDboundary('E','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        ot=2.0d0*Pi*fcFld*trFld
        cosot=cos(ot)
        sinot=sin(ot)
        ft=timeDepPFD(trFld,0)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=1,nPFDi-1
            HyInc(i)=fi3(i)*HyInc(i)+fi2(i)*ddt*(EzInc(i+1)-EzInc(i))/(PFDdx*Mue0)
          end do
          HyInc(nPFDi)=2.0d0*HyInc(nPFDi-1)-HyInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HyInc(nPFDi)=0.0d0 ! PMC
        end if
! Bx
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            rot=-(Ez(i,j+1,1)-Ez(i,j,1))/PFDdy
            Bx(i,j,1)=fj3(j)*Bx(i,j,1)+fj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Bx, plane wave
            if((nPFDjl.gt.0).and.(nPFDjh.gt.0)) then
              do i=ial,ibl
                Bx(i,jal-1,1)=Bx(i,jal-1,1)+ddt*EzInc(i)/PFDdy
                Bx(i,jbl,1)=Bx(i,jbl,1)-ddt*EzInc(i)/PFDdy
              end do
            else if(nPFDjl.gt.0) then
              do i=ial,ibl
                Bx(i,jal-1,1)=Bx(i,jal-1,1)+ddt*EzInc(i)/PFDdy
              end do
            else if(nPFDjh.gt.0) then
              do i=ial,ibl
                Bx(i,jbl,1)=Bx(i,jbl,1)-ddt*EzInc(i)/PFDdy
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bx(iPFDs(l),jPFDs(l),1)=Bx(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(1,iPFDs(l),jPFDs(l),1))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bx(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(1,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! By
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Ez(i+1,j,1)-Ez(i,j,1))/PFDdx
            By(i,j,1)=fi3(i)*By(i,j,1)+fi2(i)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident By, plane wave
            if(lPFDsoft) then
              do j=jal,jbl
                By(ial-1,j,1)=By(ial-1,j,1)-ddt*EzInc(ial)/PFDdx
                By(ibl,j,1)=By(ibl,j,1)+ddt*EzInc(ibl+1)/PFDdx
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                By(iPFDs(l),jPFDs(l),1)=By(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(2,iPFDs(l),jPFDs(l),1))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                By(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(2,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! get H from B
        select case(iUtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,4_4,Bx(i,j,1),Hx(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1), &
              & fax(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,5_4,By(i,j,1),Hy(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1), &
              & fay(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,4_4,Bx(i,j,1),Hx(i,j,1),Jx(i,j,1),Jx(i,j,1),Jx(i,j,1),Jx(i,j,1), &
              & Jx(i,j,1),Jx(i,j,1),fax(i,j,1),fbx(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,5_4,By(i,j,1),Hy(i,j,1),Jy(i,j,1),Jy(i,j,1),Jy(i,j,1),Jy(i,j,1), &
              & Jy(i,j,1),Jy(i,j,1),fay(i,j,1),fby(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,4_4,Bx(i,j,1),Hx(i,j,1),Jx(i,j,1),Lx(i,j,1),Nx(i,j,1),Jx(i,j,1), &
              & Jx(i,j,1),Jx(i,j,1),fax(i,j,1),fbx(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,5_4,By(i,j,1),Hy(i,j,1),Jy(i,j,1),Ly(i,j,1),Ny(i,j,1),Jy(i,j,1), &
              & Jy(i,j,1),Jy(i,j,1),fay(i,j,1),fby(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,4_4,Bx(i,j,1),Hx(i,j,1),Jx(i,j,1),Lx(i,j,1),Nx(i,j,1),Jx(i,j,1), &
              & Jx(i,j,1),Jx(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,5_4,By(i,j,1),Hy(i,j,1),Jy(i,j,1),Ly(i,j,1),Ny(i,j,1),Jy(i,j,1), &
              & Jy(i,j,1),Jy(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,4_4,Bx(i,j,1),Hx(i,j,1),Jx(i,j,1),Lx(i,j,1),Nx(i,j,1),Px(i,j,1), &
              & Jx(i,j,1),Jx(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,5_4,By(i,j,1),Hy(i,j,1),Jy(i,j,1),Ly(i,j,1),Ny(i,j,1),Py(i,j,1), &
              & Jy(i,j,1),Jy(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,4_4,Bx(i,j,1),Hx(i,j,1),Jx(i,j,1),Lx(i,j,1),Nx(i,j,1),Px(i,j,1), &
              & P1x(i,j,1),P2x(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fdx(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,5_4,By(i,j,1),Hy(i,j,1),Jy(i,j,1),Ly(i,j,1),Ny(i,j,1),Py(i,j,1), &
              & P1y(i,j,1),P2y(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fdy(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        end select
! boundary conditions
        call PFDboundary('E','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getPFDrFld2E(r(1:2),rF(1:6))
                Rea(1:6)=Dble(cFld(1:6,i,j,k))+cosot*rF(1:6)
                Ima(1:6)=Dimag(cFld(1:6,i,j,k))+sinot*rF(1:6)
                cFld(1:6,i,j,k)=DCmplx(Rea(1:6),Ima(1:6))
              end do
            end do
          end do
        end if
        if(nPFDf.gt.0) then
! DFT in sensor point
          wmax1=wmax
          wmax=0.0d0
          do k=1,nPFDsens
            if((trFld.lt.PFDsensT(k)).or.(trFld.gt.(PFDsensT(k)+PFDsensD(k)))) Cycle
            nPFDsFld(k)=nPFDsFld(k)+1_4
            r(1)=PFDsensX(k)
            r(2)=PFDsensY(k)
            r(3)=PFDsensZ(k)
            call getPFDrFld2E(r(1:2),rF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              ot=2.0d0*Pi*f*trFld
              cosft=cos(ot)
              sinft=sin(ot)
              f=f+df
              Rea(1:6)=Dble(cPFDsens(1:6,i,k))+cosft*rF(1:6)
              Ima(1:6)=Dimag(cPFDsens(1:6,i,k))+sinft*rF(1:6)
              cPFDsens(1:6,i,k)=DCmplx(Rea(1:6),Ima(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(rF(1)**2+rF(2)**2+rF(3)**2)+Mue0*(rF(4)**2+rF(5)**2+rF(6)**2)
            wmax=max(wmax,w)
          end do
          PFDwmax=max(wmax,PFDwmax)
          if((wmax.lt.wmax1).and.(Dble(n)*3.0d8*ddt.gt.dPFDwind*PFDdfact)) then
            if(wmax.lt.PFDwmax*PFDwfact) then
              write(*,*) 'stopping criterion reached, n=',nPFDsFld(1)
              write(*,*) 'wmax,wmax1,PFDwmax=',wmax,wmax1,PFDwmax
              PFDwmax=0.0d0
              Exit
            end if
          end if
        end if
      end do
    end if
! main loop terminated, compute dFld or scale cFld
    if(.not.lfcFld) then
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            r=getcGrd(i,j,k)
            call getPFDrFld2E(r(1:2),dFld(1:6,i,j,k))
          end do
        end do
      end do
    else if((nPFDcFld.gt.0).and.(abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
      fac=ddt*Dble(fcFld)/Dble(nPFDcFld)
      cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
    end if
! unscale sensor field array
    if((nPFDf.gt.0).and.(abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
      do k=1,nPFDsens
        if(nPFDsFld(k).lt.1) cycle
        fac=ddt*Dble(fcFld)/Dble(nPFDsFld(k))
        cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
      end do
    end if
! get PFD field in sensor point
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getPFDrFld2E(r(1:2),dPFDsens(1:6,k))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformPFD2E

  Subroutine TransformPFD2H(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10)
    Real(8) xn,xxn,rot,ot,cosot,sinot,Rea(6),Ima(6),rF(6),t,ft,r(3),f,df,cosft,sinft,fl
	  Integer(4) i,j,k,n,idum,ierr,lout,j1,j2,l
    Integer(4), intent(in) :: Initialize
    lfcFld=lfcFldAll
    lMMPstat=.false.
		call OutTxt('t2','PFD-transform'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
! check if all conditions are met
    if(.not.lEcFld) then
      idum=MessageBoxQQ('E must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(.not.lHcFld) then
      idum=MessageBoxQQ('H must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if((.not.lxcFld).or.(.not.lycFld).or.(.not.lzcFld)) then
      idum=MessageBoxQQ('X,Y,Z components must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C,&
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(.not.lrGrd) then
      idum=MessageBoxQQ('Regular must be turned on in the field dialog!\rCannot apply PFD!'C,'PFD transform'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! initialization
    ddt=dtrFld
    dPFDwind=sqrt((PFDxmax-PFDxmin)**2+(PFDymax-PFDymin)**2)
    if(Initialize.eq.1) then
      call getPFDl(iPFDt,lPFDHfield,lPFDEfield,lPFDX,lPFDY,lPFDZ,lPFDpoint,lPFDMMP,lPFDsoft,lPFDhard,lPFDplane)
      nPFDcFld=0_4
      nPFDsFld(1:nPFDsens)=0_4
      ia=max(1,nPFDil)
      ja=max(1,nPFDjl)
      ib=nPFDi-max(0,nPFDih)
      jb=nPFDj-max(0,nPFDjh)
      ial=ia
      jal=ja
      ibl=ib
      jbl=jb
      if((nPFDil.gt.0).and.lPFDplane.and.lPFDsoft) then
        ia=ia+1
        ial=ia+nPFDsLayers+2
      end if
      if((nPFDjl.gt.0).and.lPFDplane.and.lPFDsoft) then
        ja=ja+1
        jal=ja+nPFDsLayers+2
      end if
      if((nPFDih.gt.0).and.lPFDplane.and.lPFDsoft) then
        ib=ib-1
        ibl=ib-nPFDsLayers-2
      end if
      if((nPFDjh.gt.0).and.lPFDplane.and.lPFDsoft) then
        jb=jb-1
        jbl=jb-nPFDsLayers-2
      end if
      call getPFDdomType()
! allocate memory for arrays
      call DeAllocatePFD()
      Allocate(Dx(nPFDi,nPFDj,1),Dy(nPFDi,nPFDj,1), &
      &        Ex(0:nPFDi+1,0:nPFDj+1,1),Ey(0:nPFDi+1,0:nPFDj+1,1), &
      &        Bz(nPFDi,nPFDj,1), &
      &        Hz(0:nPFDi+1,0:nPFDj+1,1), &
      & faz(nPFDi,nPFDj,1), &
      & gax(nPFDi,nPFDj,1),gay(nPFDi,nPFDj,1), &
      & HzInc(0:nPFDi+1),EyInc(0:nPFDi+1), &
      & gi2(nPFDi),gi3(nPFDi),gj2(nPFDj),gj3(nPFDj), &
      & fi2(nPFDi),fi3(nPFDi),fj2(nPFDj),fj3(nPFDj), &
      & iD(3,nPFDi,nPFDj,1),stat=ierr)
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbx(nPFDi,nPFDj,1),gby(nPFDi,nPFDj,1),Ix(nPFDi,nPFDj,1),Iy(nPFDi,nPFDj,1),stat=ierr)
        Ix=0.0d0
        Iy=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kx(nPFDi,nPFDj,1),Ky(nPFDi,nPFDj,1),Mx(nPFDi,nPFDj,1),My(nPFDi,nPFDj,1),stat=ierr)
        Kx=0.0d0
        Ky=0.0d0
        Mx=0.0d0
        My=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcx(nPFDi,nPFDj,1),gcy(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Ox(nPFDi,nPFDj,1),Oy(nPFDi,nPFDj,1),stat=ierr)
        Ox=0.0d0
        Oy=0.0d0
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdx(nPFDi,nPFDj,1),gdy(nPFDi,nPFDj,1), &
        & O1x(nPFDi,nPFDj,1),O1y(nPFDi,nPFDj,1),O2x(nPFDi,nPFDj,1),O2y(nPFDi,nPFDj,1),stat=ierr)
        O1x=0.0d0
        O1y=0.0d0
        O2x=0.0d0
        O2y=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbz(nPFDi,nPFDj,1),Jz(nPFDi,nPFDj,1),stat=ierr)
        Jz=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lz(nPFDi,nPFDj,1),Nz(nPFDi,nPFDj,1),stat=ierr)
        Lz=0.0d0
        Nz=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcz(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Pz(nPFDi,nPFDj,1),stat=ierr)
        Pz=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdz(nPFDi,nPFDj,1),P1z(nPFDi,nPFDj,1),P2z(nPFDi,nPFDj,1),stat=ierr)
        P1z=0.0d0
        P2z=0.0d0
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocatePFD()
        return
      end if
      lPFDalloc=.true.
! initialize arrays and constants
		  call OutTxt('t2','Initialize PFD'C)
      if(lfcFld) then ! original OpenMaXwell field
        cFld=(0.0d0,0.0d0)
      else
        dFld=0.0d0
      end if
      HzInc=0.0d0 ! Incident plane wave
      EyInc=0.0d0
      Ex=0.0d0 ! PFDfield
      Ey=0.0d0
      Dx=0.0d0
      Dy=0.0d0
      Bz=0.0d0
      Hz=0.0d0
      gi2=1.0d0 ! UPML parameters
      fi2=1.0d0
      gi3=1.0d0
      fi3=1.0d0
      gj2=1.0d0
      fj2=1.0d0
      gj3=1.0d0
      fj3=1.0d0
      do i=1,nPFDil
        xxn=Dble(nPFDil-i+1)/Dble(nPFDil)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gi2(i)=1.0d0/(1.0d0+xn)
        gi3(i)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDil-i+1)-0.5d0)/Dble(nPFDil)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fi2(i)=1.0d0/(1.0d0+xn)
        fi3(i)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do i=1,nPFDih
        xxn=Dble(nPFDih-i+1)/Dble(nPFDih)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gi2(nPFDi-i)=1.0d0/(1.0d0+xn)
        gi3(nPFDi-i)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDih-i+1)-0.5d0)/Dble(nPFDih)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fi2(nPFDi-i-1)=1.0d0/(1.0d0+xn)
        fi3(nPFDi-i-1)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do j=1,nPFDjl
        xxn=Dble(nPFDjl-j+1)/Dble(nPFDjl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gj2(j)=1.0d0/(1.0d0+xn)
        gj3(j)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDjl-j+1)-0.5d0)/Dble(nPFDjl)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fj2(j)=1.0d0/(1.0d0+xn)
        fj3(j)=(1.0d0-xn)/(1.0d0+xn)
      end do
      do j=1,nPFDjh
        xxn=Dble(nPFDjh-j+1)/Dble(nPFDjh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        gj2(nPFDj-j)=1.0d0/(1.0d0+xn)
        gj3(nPFDj-j)=(1.0d0-xn)/(1.0d0+xn)
        xxn=(Dble(nPFDjh-j+1)-0.5d0)/Dble(nPFDjh)
        xn=PFDpml*(xxn**3)/PFDdx
        if(PFDpml.lt.0.0d0) xn=-PFDpml*(xxn**3)
        fj2(nPFDj-j-1)=1.0d0/(1.0d0+xn)
        fj3(nPFDj-j-1)=(1.0d0-xn)/(1.0d0+xn)
      end do
      call getPFDmat2H(.false.)
      if(iPFDft.eq.0) then ! initialize field
        if(lPFDMMP) then ! MMP solution
          do i=1,nPFDi
            do j=1,nPFDj
              call getMMPFld(i,j,0_4,6_4,iD(3,i,j,1))
              Hz(i,j,1)=Dble(FldExp(6))
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              Hz(i,j,1)=CHrnd(-1.0d0,1.0d0)
            end do
          end do
        end if
      end if
      Ampl=1.0d0
! scale amplitude: Propagate incident wave through the UPML area
      if(lPFDplane) then ! scale amplitude: Propagate incident wave through the UPML area
        t=trFld
        Ampl=0.0d0
        k=min(100000_4,int4((Dble(2_4*ia)*PFDdx*Dsqrt(Eps0*Mue0)+PFDfTmax+2.0d0*PFDfTau)/ddt))
        do n=1,k
          t=t+0.5d0*ddt
          ot=2.0d0*Pi*fcFld*t
          ft=timeDepPFD(t,0)
          do i=1,nPFDi-1
            HzInc(i)=fi3(i)*HzInc(i)+fi2(i)*ddt*(EyInc(i)-EyInc(i+1))/(PFDdx*Mue0)
          end do
          HzInc(1)=ft
          HzInc(nPFDi)=2.0d0*HzInc(nPFDi-1)-HzInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HzInc(nPFDi)=0.0d0 ! PMC
          Ampl=max(Ampl,HzInc(ia+1))
          t=t+0.5d0*ddt
          do i=2,nPFDi
            EyInc(i)=gi3(i)*EyInc(i)+gi2(i)*ddt*(HzInc(i-1)-HzInc(i))/(PFDdx*Eps0)
          end do
          EyInc(1)=2.0d0*EyInc(2)-EyInc(3) ! extrapolate
          if(nPFDih.gt.-1) EyInc(nPFDi)=0.0d0 ! PEC
        end do
        Ampl=1.0d0/(Dsqrt(Zw0)*Max(Ampl,1.0d-100))
        HzInc=0.0d0
        EyInc=0.0d0
        t=trFld
      end if
    end if
    if((abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
! scale cFld
      if(lfcFld.and.(nPFDcFld.gt.0_4)) then
        fac=Dble(nPFDcFld)/(ddt*Dble(fcFld))
        cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
      end if
! scale sensor field array
      if(nPFDf.gt.0_4) then
        do k=1,nPFDsens
          if(nPFDsFld(k).lt.1) cycle
          fac=Dble(nPFDsFld(k))/(ddt*Dble(fcFld))
          cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
        end do
      end if
    end if
! iterate over time: main loop
    j1=1
    j2=nPFDj
    if(nPFDjh.lt.-1) j2=nPFDj
    if(nPFDjl.gt.0) j1=1+nPFDjl
    if(nPFDjh.gt.0) j2=nPFDj-1-nPFDjh
    wmax1=0.0d0
    if(nIterPFD.lt.1) then
      PFDwmax=0.0d0
      do i=1,nPFDi
        do j=1,nPFDj
          Ex(i,j,1)=1.0d0/(Eps0*gax(i,j,1))
          Ey(i,j,1)=1.0d0/(Eps0*gay(i,j,1))
          Hz(i,j,1)=1.0d0/(Mue0*faz(i,j,1))
        end do
      end do
    else
      do n=1,nIterPFD
        if(lStopThread) return
		    call OutTxt('t2','PFD transform'C)
        call IntToStr(n,0,0,SpaceTExt,lout)
		    call OutTxt('n2',SpaceTExt(1:lout))
        call IntToStr(Int4(nIterPFD),0,0,SpaceTExt,lout)
		    call OutTxt('m2',SpaceTExt(1:lout))
! time dependence
        trFld=trFld+0.5d0*dtrFld
        ot=2.0d0*Pi*fcFld*trFld
        cosot=cos(ot)
        sinot=sin(ot)
        ft=timeDepPFD(trFld,0)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=2,nPFDi
            EyInc(i)=gi3(i)*EyInc(i)+gi2(i)*ddt*(HzInc(i-1)-HzInc(i))/(PFDdx*Eps0)
          end do
          EyInc(1)=2.0d0*EyInc(2)-EyInc(3) ! extrapolate
          if(nPFDih.gt.-1) EyInc(nPFDi)=0.0d0 ! PEC
        end if
! Dx
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Hz(i,j,1)-Hz(i,j-1,1))/PFDdy
            Dx(i,j,1)=gj3(j)*Dx(i,j,1)+gj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Dx, plane wave
            if((nPFDjl.gt.0).and.(nPFDjh.gt.0)) then
              do i=ial,ibl-1
                Dx(i,jal,1)=Dx(i,jal,1)-ddt*HzInc(i)/PFDdy
                Dx(i,jbl,1)=Dx(i,jbl,1)+ddt*HzInc(i)/PFDdy
              end do
            else if(nPFDjl.gt.0) then
              do i=ial,ibl-1
                Dx(i,jal,1)=Dx(i,jal,1)-ddt*HzInc(i)/PFDdy
              end do
            else if(nPFDjh.gt.0) then
              do i=ial,ibl-1
                Dx(i,jbl,1)=Dx(i,jbl,1)+ddt*HzInc(i)/PFDdy
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dx(iPFDs(l),jPFDs(l),1)=Dx(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(1,iPFDs(l),jPFDs(l),1))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dx(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(1,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! Dy
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            rot=-(Hz(i,j,1)-Hz(i-1,j,1))/PFDdx
            Dy(i,j,1)=gi3(i)*Dy(i,j,1)+gi2(i)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Dy, plane wave
            if(lPFDsoft) then
              do j=jal,jbl-1
                Dy(ial,j,1)=Dy(ial,j,1)+ddt*HzInc(ial)/PFDdx
                Dy(ibl,j,1)=Dy(ibl,j,1)-ddt*HzInc(ibl)/PFDdx
              end do
            else ! total field formulation
              do j=j1,j2
                Dy(ia+1,j,1)=EyInc(ia+1)*Eps0
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dy(iPFDs(l),jPFDs(l),1)=Dy(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(2,iPFDs(l),jPFDs(l),1))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Dy(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(2,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! get E from D
!if(n.eq.1) then
!  write(*,*) 'ix,D,E,I,K,M,O='
!  do i=300,302
!    write(*,*) i,Dy(i,2,1),Ey(i,2,1),Iy(i,2,1),Ky(i,2,1),My(i,2,1),Oy(i,2,1)
!  end do
!end if
        select case(iEtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,1_4,Dx(i,j,1),Ex(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1), &
              & gax(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,2_4,Dy(i,j,1),Ey(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1), &
              & gay(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,1_4,Dx(i,j,1),Ex(i,j,1),Ix(i,j,1),Ix(i,j,1),Ix(i,j,1),Ix(i,j,1), &
              & Ix(i,j,1),Ix(i,j,1),gax(i,j,1),gbx(i,j,1),gax(i,j,1),gax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,2_4,Dy(i,j,1),Ey(i,j,1),Iy(i,j,1),Iy(i,j,1),Iy(i,j,1),Iy(i,j,1), &
              & Iy(i,j,1),Iy(i,j,1),gay(i,j,1),gby(i,j,1),gay(i,j,1),gay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,1_4,Dx(i,j,1),Ex(i,j,1),Ix(i,j,1),Kx(i,j,1),Mx(i,j,1),Ix(i,j,1), &
              & Ix(i,j,1),Ix(i,j,1),gax(i,j,1),gbx(i,j,1),gax(i,j,1),gax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,2_4,Dy(i,j,1),Ey(i,j,1),Iy(i,j,1),Ky(i,j,1),My(i,j,1),Iy(i,j,1), &
              & Iy(i,j,1),Iy(i,j,1),gay(i,j,1),gby(i,j,1),gay(i,j,1),gay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,1_4,Dx(i,j,1),Ex(i,j,1),Ix(i,j,1),Kx(i,j,1),Mx(i,j,1),Ix(i,j,1), &
              & Ix(i,j,1),Ix(i,j,1),gax(i,j,1),gbx(i,j,1),gcx(i,j,1),gax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,2_4,Dy(i,j,1),Ey(i,j,1),Iy(i,j,1),Ky(i,j,1),My(i,j,1),Iy(i,j,1), &
              & Iy(i,j,1),Iy(i,j,1),gay(i,j,1),gby(i,j,1),gcy(i,j,1),gay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,1_4,Dx(i,j,1),Ex(i,j,1),Ix(i,j,1),Kx(i,j,1),Mx(i,j,1),Ox(i,j,1), &
              & Ix(i,j,1),Ix(i,j,1),gax(i,j,1),gbx(i,j,1),gcx(i,j,1),gax(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,2_4,Dy(i,j,1),Ey(i,j,1),Iy(i,j,1),Ky(i,j,1),My(i,j,1),Oy(i,j,1), &
              & Iy(i,j,1),Iy(i,j,1),gay(i,j,1),gby(i,j,1),gcy(i,j,1),gay(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDX,1_4,Dx(i,j,1),Ex(i,j,1),Ix(i,j,1),Kx(i,j,1),Mx(i,j,1),Ox(i,j,1), &
              & O1x(i,j,1),O2x(i,j,1),gax(i,j,1),gbx(i,j,1),gcx(i,j,1),gdx(i,j,1),iD(1,i,j,1),ft,cosot,sinot)
              call DB2EH(i,j,0_4,lPFDY,2_4,Dy(i,j,1),Ey(i,j,1),Iy(i,j,1),Ky(i,j,1),My(i,j,1),Oy(i,j,1), &
              & O1y(i,j,1),O2y(i,j,1),gay(i,j,1),gby(i,j,1),gcy(i,j,1),gdy(i,j,1),iD(2,i,j,1),ft,cosot,sinot)
            end do
          end do
        end select
! boundary conditions
        call PFDboundary('H','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        ot=2.0d0*Pi*fcFld*trFld
        cosot=cos(ot)
        sinot=sin(ot)
        ft=timeDepPFD(trFld,0)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=1,nPFDi-1
            HzInc(i)=fi3(i)*HzInc(i)+fi2(i)*ddt*(EyInc(i)-EyInc(i+1))/(PFDdx*Mue0)
          end do
          HzInc(1)=Ampl*ft
          HzInc(nPFDi)=2.0d0*HzInc(nPFDi-1)-HzInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HzInc(nPFDi)=0.0d0 ! PMC
        end if
! Bz
!!$OMP parallel do
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Ex(i,j+1,1)-Ex(i,j,1))/PFDdy-(Ey(i+1,j,1)-Ey(i,j,1))/PFDdx
            Bz(i,j,1)=fi3(i)*fj3(j)*Bz(i,j,1)+fi2(i)*fj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Bz, plane wave
            if(lPFDsoft) then
              do j=jal,jbl-1
                Bz(ial,j,1)=Bz(ial,j,1)+ddt*EyInc(ial)/PFDdx
                Bz(ibl,j,1)=Bz(ibl,j,1)-ddt*EyInc(ibl)/PFDdx
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bz(iPFDs(l),jPFDs(l),1)=Bz(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(3,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepPFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*(Dble(cF(1))*cosot+DImag(cF(1))*sinot)
                end if
                Bz(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(3,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! get H from B
        select case(iUtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,6_4,Bz(i,j,1),Hz(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1), &
              & faz(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,6_4,Bz(i,j,1),Hz(i,j,1),Jz(i,j,1),Jz(i,j,1),Jz(i,j,1),Jz(i,j,1), &
              & Jz(i,j,1),Jz(i,j,1),faz(i,j,1),fbz(i,j,1),faz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,6_4,Bz(i,j,1),Hz(i,j,1),Jz(i,j,1),Lz(i,j,1),Nz(i,j,1),Jz(i,j,1), &
              & Jz(i,j,1),Jz(i,j,1),faz(i,j,1),fbz(i,j,1),faz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,6_4,Bz(i,j,1),Hz(i,j,1),Jz(i,j,1),Lz(i,j,1),Nz(i,j,1),Jz(i,j,1), &
              & Jz(i,j,1),Jz(i,j,1),faz(i,j,1),fbz(i,j,1),fcz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,6_4,Bz(i,j,1),Hz(i,j,1),Jz(i,j,1),Lz(i,j,1),Nz(i,j,1),Pz(i,j,1), &
              & Jz(i,j,1),Jz(i,j,1),faz(i,j,1),fbz(i,j,1),fcz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call DB2EH(i,j,0_4,lPFDZ,6_4,Bz(i,j,1),Hz(i,j,1),Jz(i,j,1),Lz(i,j,1),Nz(i,j,1),Pz(i,j,1), &
              & P1z(i,j,1),P2z(i,j,1),faz(i,j,1),fbz(i,j,1),fcz(i,j,1),fdz(i,j,1),iD(3,i,j,1),ft,cosot,sinot)
            end do
          end do
        end select
! boundary conditions
        call PFDboundary('H','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getPFDrFld2H(r(1:2),rF(1:6))
                Rea(1:6)=Dble(cFld(1:6,i,j,k))+cosot*rF(1:6)
                Ima(1:6)=Dimag(cFld(1:6,i,j,k))+sinot*rF(1:6)
                cFld(1:6,i,j,k)=DCmplx(Rea(1:6),Ima(1:6))
              end do
            end do
          end do
        end if
        if(nPFDf.gt.0) then
! DFT in sensor point
          wmax1=wmax
          wmax=0.0d0
          do k=1,nPFDsens
            if((trFld.lt.PFDsensT(k)).or.(trFld.gt.(PFDsensT(k)+PFDsensD(k)))) Cycle
            nPFDsFld(k)=nPFDsFld(k)+1_4
            r(1)=PFDsensX(k)
            r(2)=PFDsensY(k)
            r(3)=PFDsensZ(k)
            call getPFDrFld2H(r(1:2),rF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              ot=2.0d0*Pi*f*trFld
              cosft=cos(ot)
              sinft=sin(ot)
              f=f+df
              Rea(1:6)=Dble(cPFDsens(1:6,i,k))+cosft*rF(1:6)
              Ima(1:6)=Dimag(cPFDsens(1:6,i,k))+sinft*rF(1:6)
              cPFDsens(1:6,i,k)=DCmplx(Rea(1:6),Ima(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(rF(1)**2+rF(2)**2+rF(3)**2)+Mue0*(rF(4)**2+rF(5)**2+rF(6)**2)
            wmax=max(wmax,w)
          end do
          PFDwmax=max(wmax,PFDwmax)
          if((wmax.lt.wmax1).and.(Dble(n)*3.0d8*ddt.gt.dPFDwind*PFDdfact)) then
            if(wmax.lt.PFDwmax*PFDwfact) then
              write(*,*) 'stopping criterion reached, n=',nPFDsFld(1)
              write(*,*) 'wmax,wmax1,PFDwmax=',wmax,wmax1,PFDwmax
              PFDwmax=0.0d0
              Exit
            end if
          end if
        end if
      end do
    end if
! main loop terminated, compute dFld or scale cFld
    if(.not.lfcFld) then
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            r=getcGrd(i,j,k)
            call getPFDrFld2H(r(1:2),dFld(1:6,i,j,k))
          end do
        end do
      end do
    else if((nPFDcFld.gt.0).and.(abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
      fac=ddt*Dble(fcFld)/Dble(nPFDcFld)
      cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
    end if
! unscale sensor field array
    if((nPFDf.gt.0).and.(abs(iPFDft).gt.1).and.(abs(iPFDft).lt.4)) then ! only for ramp
      do k=1,nPFDsens
        if(nPFDsFld(k).lt.1) cycle
        fac=ddt*Dble(fcFld)/Dble(nPFDsFld(k))
        cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
      end do
    end if
! get PFD field in sensor point
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getPFDrFld2H(r(1:2),dPFDsens(1:6,k))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformPFD2H

  Subroutine getABC(Ep0,lf,eEff,ga,gb,gc,gd)
    Implicit None
    Integer(2), Intent(in):: lf
    Integer(2) idD
    Real(8), Intent(in):: Ep0,eEff
    Real(8), Intent(out):: ga,gb,gc,gd
    Real(8),save:: ex,q,a(6)
    gc=0.0d0
    gb=0.0d0
    ga=0.0d0
    if(Dabs(Ep0-Eps0).lt.Dabs(Ep0-Mue0)) then ! Lorentz parameters are in aDom, bDom, Drude parameter ex in dDom, gDom
      a(1:6)=aDom(1:6,lf)
      ex=dDom(lf)
      idD=idDom(1,lf)
    else
      a(1:6)=bDom(1:6,lf)
      ex=gDom(lf)
      idD=idDom(2,lf)
    end if
    select case(idD)
    case(1) ! loss-free dielectric: E=ga*D
      ga=1.0d0/(eEff*Ep0)
    case(2) ! lossy standard: E=ga*D-SL; SL=SL+gb*E; a contains epsilonR, sigma
      ga=1.0d0/(a(1)*Ep0+a(2)*ddt)
      gb=a(2)*ddt*ga
    case(3) ! Drude: E=ga*D-S; S=(1+ex)*S1-ex*S2+gb*E; S2=S1; S1=S; a contains epsilonR, omegaD, gammaD
      ga=1.0d0/(a(1)*Ep0)
      gb=((a(2)**2)*ddt*(1.0d0-ex)*ga*Ep0).div.a(3)
    case(4) ! Lorentz: E=D*ex-S; S=ga*S1-gb*S2+gc*E; S2=S1; S1=S; Sullivan: a(1)=epsilonR, a(2)=epsilon1, a(3)=omega0, a(4)=delta0
      q=1.0d0/(1.0d0+ddt*a(4)*a(3))
      gc=(ddt*a(3))**2
      ga=(2.0d0-gc)*q
      gb=(1.0d0-ddt*a(4)*a(3))*q
      gc=gc*a(2)*q*Ep0
    case(5) ! lossy + Drude: E=ga*D-S-SL; SL=SL+gb*E; S=(1+ex)*S1-ex*S2+gc*E; S2=S1; S1=S
      ga=1.0d0/(a(1)*Ep0+a(4)*ddt)
      gb=a(4)*ddt*ga
      gc=((a(2)**2)*ddt*(1.0d0-ex)*ga*Ep0).div.a(3)
    case(6) ! Drude + Lorentz
      q=1.0d0/(1.0d0+ddt*a(6)*a(5))
      gc=(ddt*a(5))**2
      ga=(2.0d0-gc)*q
      gb=(1.0d0-ddt*a(6)*a(5))*q
      gc=gc*a(4)*q*Ep0
      gd=((a(2)**2)*ddt*(1.0d0-ex)).div.(a(1)*a(3)) ! add Drude (no sigma, no epsilonD term)
    end select
  end Subroutine getABC

  Subroutine DB2EH(i,j,k,lcomp,icomp,D,E,S0,S1,S2,SD,SD1,SD2,ga,gb,gc,gd,iD0,ft,cosot,sinot)
    Implicit none
    Logical lcomp
    Integer(2) iD,iD0,idD
    Integer(4) icomp,i,j,k
    Real(8) D,E,S0,S1,S2,SD,SD1,SD2,ga,gb,gc,gd,ft,cosot,sinot,ex,Ep0,q
    Complex(8) cF
    if(ga.gt.pBig) then ! PEC or PMC
      E=0.0d0
      return
    else 
      iD=max(iD0,1_2)
      if(icomp.lt.4) then
        ex=dDom(iD)
        Ep0=Eps0
        idD=idDom(1,iD)
      else
        ex=gDom(iD)
        Ep0=Mue0
        idD=idDom(2,iD)
      end if
      select case(idD)
      case(1) ! loss-free dielectric
        E=ga*D
      case(2) ! lossy dielectric
        E=ga*D-SD
        SD=SD+gb*E
      case(3) ! standard Drude
        E=ga*D-S0
        S0=(1+ex)*S1-ex*S2+gb*E
        S2=S1
        S1=S0
      case(4) ! Lorentz only
        if(icomp.lt.4) then
          q=1.0d0/(Ep0*aDom(1,iD))
        else
          q=1.0d0/(Ep0*bDom(1,iD))
        end if
        E=(D-S0)*q
        S0=ga*S1-gb*S2+gc*E
        S2=S1
        S1=S0
      case(5) ! Drude + sigma
        E=ga*D-S0-SD
        SD=SD+gb*E
        S0=(1+ex)*S1-ex*S2+gc*E
        S2=S1
        S1=S0
      case(6) ! Drude + Lorentz
        if(icomp.lt.4) then
          q=1.0d0/(Ep0*aDom(1,iD))
        else
          q=1.0d0/(Ep0*bDom(1,iD))
        end if
        E=(D-S0)*q-SD
        S0=ga*S1-gb*S2+gc*E
        S2=S1
        S1=S0
        SD=(1+ex)*SD1-ex*SD2+gd*E
        SD2=SD1
        SD1=SD
      end select
    end if
    if(iPFDft.eq.0) return
    if(.not.(lPFDMMP.and.lcomp)) return
    if(icomp.lt.4) then
      if(.not.lPFDEfield) return
    else
      if(.not.lPFDHfield) return
    end if
    if(LSkipPFD(iD).or.lPFDpoint) return
! import MMP field
    call PFDgetMMPpoint(i,j,k,icomp,cF)
    if(lPFDsoft) then ! soft
      E=E+ft*(Dble(cF)*cosot+DImag(cF)*sinot)
    else ! hard
      E=ft*(Dble(cF)*cosot+DImag(cF)*sinot)
    end if
  end Subroutine DB2EH

  Subroutine PFDgetMMPpoint(i,j,k,icomp,cF)
    Implicit none
    Logical lloc
    Integer(2) iDl
    Integer(4) i,j,k,icomp,idum
    Real(8) r(3)
    Complex(8) cF
    call getPFDloc(i,j,k,icomp,r)
    lloc=lfcFld
    lfcFld=.true.
    iDl=iD(icomp,i,j,k)
    call GetFieldExp(nExp,1_4,r,iDl,iHEGlobal,idum)
    lfcFld=lloc
    cF=FldExp(icomp)
  end Subroutine PFDgetMMPpoint

  Subroutine getMMPFld(i,j,k,icomp,iD,rout)
    Implicit none
    Logical lloc
    Integer(2) iD
    Integer(4) i,j,k,icomp,idum
    Real(8) r(3)
    Real(8), Optional:: rout(3)
    call getPFDloc(i,j,k,icomp,r)
    if(Present(rout)) then
      rout(1:3)=r(1:3)
    else
      lloc=lfcFld
      lfcFld=.true.
      call GetFieldExp(nExp,1_4,r,iD,iHEGlobal,idum)
      lfcFld=lloc
    end if
  end Subroutine getMMPFld

  Subroutine getPFDloc(i,j,k,icomp,r) 
    Implicit none
    Integer(4) i,j,k,icomp
    Real(8) r(3)
    r(1)=PFDxmin+Dble(i-1)*PFDdx
    r(2)=PFDymin+Dble(j-1)*PFDdy
    if(k.gt.0) then
      r(3)=PFDzmin+Dble(k-1)*PFDdz
      Select Case(icomp)
      Case(1)
        r(1)=r(1)+0.5d0*PFDdx
      Case(2)
        r(2)=r(2)+0.5d0*PFDdy
      Case(3)
        r(3)=r(3)+0.5d0*PFDdz
      Case(4)
        r(2)=r(2)+0.5d0*PFDdy
        r(3)=r(3)+0.5d0*PFDdz
      Case(5)
        r(1)=r(1)+0.5d0*PFDdx
        r(3)=r(3)+0.5d0*PFDdz
      Case(6)
        r(1)=r(1)+0.5d0*PFDdx
        r(2)=r(2)+0.5d0*PFDdy
      end Select
    else
      r(3)=0.0d0
      Select Case(icomp)
      Case(1)
        r(1)=r(1)+0.5d0*PFDdx
      Case(2)
        r(2)=r(2)+0.5d0*PFDdy
      Case(4)
        r(2)=r(2)+0.5d0*PFDdy
      Case(5)
        r(1)=r(1)+0.5d0*PFDdx
      Case(6)
        r(1)=r(1)+0.5d0*PFDdx
        r(2)=r(2)+0.5d0*PFDdy
      end Select
    end if
  end Subroutine getPFDloc

  Subroutine getPFDmat3D(lIndex)
    Implicit none
    Logical lIndex
    Real(8),save:: r0(3),r(3),dr,dmin,rmin(3),val(2),gax0,gbx0,gcx0,gdx0,gay0,gby0,gcy0,gdy0,gaz0,gbz0,gcz0,gdz0, &
    & fax0,fbx0,fcx0,fdx0,fay0,fby0,fcy0,fdy0,faz0,fbz0,fcz0,fdz0
    Integer(4) i,j,k
    Integer(2),save:: iD0(6)
    r0(1)=PFDxmin-PFDdx ! material parameters
    dr=dsqrt(PFDdx**2+PFDdy**2+PFDdz**2)
    call setPFDdgDom()
    do i=1,nPFDi
      r0(1)=r0(1)+PFDdx
      r0(2)=PFDymin-PFDdy
      do j=1,nPFDj
        r0(2)=r0(2)+PFDdy
        r0(3)=PFDzmin-PFDdz
        do k=1,nPFDk
          r0(3)=r0(3)+PFDdz
          call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD0(1),val,.true.)
          if(dmin.gt.dr) then ! no boundary near r0 -> simple and quick
            iD0(2:6)=iD0(1)
            call getFaFbGaGb(3_2,r0,iD0(1),fax0,fbx0,fcx0,fdx0,gax0,gbx0,gcx0,gdx0)
            fay0=fax0
            faz0=fax0
            fby0=fbx0
            fbz0=fbx0
            fcy0=fcx0
            fcz0=fcx0
            fdy0=fdx0
            fdz0=fdx0
            gay0=gax0
            gaz0=gax0
            gby0=gbx0
            gbz0=gbx0
            gcy0=gcx0
            gcz0=gcx0
            gdy0=gdx0
            gdz0=gdx0
          else ! boundary near r0 -> be more careful
            r(1)=r0(1)
            r(2)=r0(2)+0.5d0*PFDdy
            r(3)=r0(3)+0.5d0*PFDdz
            call getFaFb(3_2,1_2,r,dr,iD0(4),fax0,fbx0,fcx0,fdx0)
            r(1)=r0(1)+0.5d0*PFDdx
            r(2)=r0(2)
            r(3)=r0(3)+0.5d0*PFDdz
            call getFaFb(3_2,2_2,r,dr,iD0(5),fay0,fby0,fcy0,fdy0)
            r(1)=r0(1)+0.5d0*PFDdx
            r(2)=r0(2)+0.5d0*PFDdy
            r(3)=r0(3)
            call getFaFb(3_2,3_2,r,dr,iD0(6),faz0,fbz0,fcz0,fdz0)
            r(1)=r0(1)+0.5d0*PFDdx
            r(2)=r0(2)
            r(3)=r0(3)
            call getGaGb(3_2,1_2,r,dr,iD0(1),gax0,gbx0,gcx0,gdx0)
            r(1)=r0(1)
            r(2)=r0(2+0.5d0*PFDdy)
            r(3)=r0(3)
            call getGaGb(3_2,2_2,r,dr,iD0(2),gay0,gby0,gcy0,gdy0)
            r(1)=r0(1)
            r(2)=r0(2)
            r(3)=r0(3)+0.5d0*PFDdz
            call getGaGb(3_2,3_2,r,dr,iD0(3),gaz0,gbz0,gcz0,gdz0)
          end if
          if(.not.lIndex) then
            iD(1:6,i,j,k)=iD0(1:6)
            gax(i,j,k)=gax0
            gay(i,j,k)=gay0
            gaz(i,j,k)=gaz0
            if(iEtype.gt.1) then
              gbx(i,j,k)=gbx0
              gby(i,j,k)=gby0
              gbz(i,j,k)=gbz0
            end if
            if(iEtype.gt.3) then
              gcx(i,j,k)=gcx0
              gcy(i,j,k)=gcy0
              gcz(i,j,k)=gcz0
            end if
            if(iEtype.gt.5) then
              gdx(i,j,k)=gdx0
              gdy(i,j,k)=gdy0
              gdz(i,j,k)=gdz0
            end if
            fax(i,j,k)=fax0
            fay(i,j,k)=fay0
            faz(i,j,k)=faz0
            if(iUtype.gt.1) then
              fbx(i,j,k)=fbx0
              fby(i,j,k)=fby0
              fbz(i,j,k)=fbz0
            end if
            if(iUtype.gt.3) then
              fcx(i,j,k)=fcx0
              fcy(i,j,k)=fcy0
              fcz(i,j,k)=fcz0
            end if
            if(iUtype.gt.5) then
              fdx(i,j,k)=fdx0
              fdy(i,j,k)=fdy0
              fdz(i,j,k)=fdz0
            end if
          end if
        end do
      end do
    end do
  end Subroutine getPFDmat3D

  Subroutine getPFDmat2E(lIndex)
    Implicit none
    Logical lIndex
    Real(8),save:: r(3),r0(3),dr,dmin,rmin(3),val(2),gaz0,gbz0,gcz0,gdz0,fax0,fbx0,fcx0,fdx0,fay0,fby0,fcy0,fdy0
    Integer(4) i,j
    Integer(2),save:: iD0(3)
    r0(1)=PFDxmin-PFDdx ! material parameters
    r0(3)=0.0d0
    dr=dsqrt(PFDdx**2+PFDdy**2)
    call setPFDdgDom()
    do i=1,nPFDi
      r0(1)=r0(1)+PFDdx
      r0(2)=PFDymin-PFDdy
      do j=1,nPFDj
        r0(2)=r0(2)+PFDdy
        call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD0(1),val,.true.)
        if(dmin.gt.dr) then ! no boundary near r0 -> simple and quick
          iD0(2:3)=iD0(1)
          call getFaFbGaGb(2_2,r0,iD0(1),fax0,fbx0,fcx0,fdx0,gaz0,gbz0,gcz0,gdz0)
          fay0=fax0
          fby0=fbx0
          fcy0=fcx0
          fdy0=fdx0
        else ! boundary near r0 -> be more careful
          r(1)=r0(1) !+0.5d0*PFDdx
          r(2)=r0(2)+0.5d0*PFDdy
          r(3)=0.0d0
          call getFaFb(2_2,1_2,r,dr,iD0(1),fax0,fbx0,fcx0,fdx0)
          r(1)=r0(1)+0.5d0*PFDdx
          r(2)=r0(2) !+0.5d0*PFDdy
          call getFaFb(2_2,2_2,r,dr,iD0(2),fay0,fby0,fcy0,fdy0)
          r(1)=r0(1) !+0.5d0*PFDdx
          r(2)=r0(2) !+0.5d0*PFDdy
          call getGaGb(2_2,3_2,r,dr,iD0(3),gaz0,gbz0,gcz0,gdz0)
        end if
        if(.not.lIndex) then
          iD(1:3,i,j,1)=iD0(1:3)
          gaz(i,j,1)=gaz0
          if(iEtype.gt.1) gbz(i,j,1)=gbz0
          if(iEtype.gt.3) gcz(i,j,1)=gcz0
          if(iEtype.gt.5) gdz(i,j,1)=gdz0
          fax(i,j,1)=fax0
          fay(i,j,1)=fay0
          if(iUtype.gt.1) then
            fbx(i,j,1)=fbx0
            fby(i,j,1)=fby0
          end if
          if(iUtype.gt.3) then
            fcx(i,j,1)=fcx0
            fcy(i,j,1)=fcy0
          end if
          if(iUtype.gt.5) then
            fdx(i,j,1)=fdx0
            fdy(i,j,1)=fdy0
          end if
        end if
      end do
    end do
  end Subroutine getPFDmat2E

  Subroutine getPFDmat2H(lIndex)
    Implicit none
    Logical lIndex
    Real(8),save:: r(3),r0(3),dr,dmin,rmin(3),val(2),faz0,fbz0,fcz0,fdz0,gax0,gbx0,gcx0,gdx0,gay0,gby0,gcy0,gdy0
    Integer(4) i,j
    Integer(2),save:: iD0(3)
    r0(1)=PFDxmin-PFDdx ! material parameters
    r0(3)=0.0d0
    dr=dsqrt(PFDdx**2+PFDdy**2)
    call setPFDdgDom()
    do i=1,nPFDi
      r0(1)=r0(1)+PFDdx
      r0(2)=PFDymin-PFDdy
      do j=1,nPFDj
        r0(2)=r0(2)+PFDdy
        call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD0(1),val,.true.)
        if(dmin.gt.dr) then ! no boundary near r0 -> simple and quick
          iD0(2:3)=iD0(1)
          call getFaFbGaGb(2_2,r0,iD0(1),faz0,fbz0,fcz0,fdz0,gax0,gbx0,gcx0,gdx0)
          gay0=gax0
          gby0=gbx0
          gcy0=gcx0
          gdy0=gdx0
        else ! boundary near r0 -> be more careful
          r(1)=r0(1)+0.5d0*PFDdx
          r(2)=r0(2)+0.5d0*PFDdy
          r(3)=0.0d0
          call getFaFb(2_2,3_2,r,dr,iD0(3),faz0,fbz0,fcz0,fdz0)
          r(1)=r0(1)+0.5d0*PFDdx
          r(2)=r0(2)!+0.5d0*PFDdy
          call getGaGb(2_2,1_2,r,dr,iD0(1),gax0,gbx0,gcx0,gdx0)
          r(1)=r0(1)!+0.5d0*PFDdx
          r(2)=r0(2)+0.5d0*PFDdy
          call getGaGb(2_2,2_2,r,dr,iD0(2),gay0,gby0,gcy0,gdy0)
        end if
        if(.not.lIndex) then
          iD(1:3,i,j,1)=iD0(1:3)
          gax(i,j,1)=gax0
          gay(i,j,1)=gay0
          if(iEtype.gt.1) then
            gbx(i,j,1)=gbx0
            gby(i,j,1)=gby0
          end if
          if(iEtype.gt.3) then
            gcx(i,j,1)=gcx0
            gcy(i,j,1)=gcy0
          end if
          if(iEtype.gt.5) then
            gdx(i,j,1)=gdx0
            gdy(i,j,1)=gdy0
          end if
          faz(i,j,1)=faz0
          if(iUtype.gt.1) fbz(i,j,1)=fbz0
          if(iUtype.gt.3) fcz(i,j,1)=fcz0
          if(iUtype.gt.5) fdz(i,j,1)=fdz0
        end if
      end do
    end do
  end Subroutine getPFDmat2H

  Subroutine setPFDdgDom()
! exponential function for Drude model (gamma term)
    implicit none
    Integer(4) i
    do i=1,nDom
     ! if(dabs(aDom(3,i)).gt.pSmall) dDom(i)=dexp(-ddt/aDom(3,i))
     ! if(dabs(bDom(3,i)).gt.pSmall) gDom(i)=dexp(-ddt/bDom(3,i))
      dDom(i)=dexp(-ddt*aDom(3,i))
      gDom(i)=dexp(-ddt*bDom(3,i))
    end do
  end Subroutine setPFDdgDom

  Subroutine getPFDrFld3D(r,F)
! get real field vector f at location r from the PFD field using linear interpolation
    implicit none
    Real(8) r(3),F(6),x,y,z,xx,yy,zz
    Integer(4) i,j,k,i1,j1,k1,ii,ii1,jj,jj1,kk,kk1
    call getPFDix(r(1),PFDxmin,PFDdx,nPFDi,i,i1,x)
    call getPFDix(r(1)-0.5d0*PFDdx,PFDxmin,PFDdx,nPFDi,ii,ii1,xx)
    call getPFDix(r(2),PFDymin,PFDdy,nPFDj,j,j1,y)
    call getPFDix(r(2)-0.5d0*PFDdy,PFDymin,PFDdy,nPFDj,jj,jj1,yy)
    call getPFDix(r(3),PFDzmin,PFDdz,nPFDk,k,k1,z)
    call getPFDix(r(3)-0.5d0*PFDdz,PFDzmin,PFDdz,nPFDk,kk,kk1,zz)
    F(1)=interpolatePFD3D(xx,y,z,Ex(ii,j,k),Ex(ii1,j,k),Ex(ii,j1,k),Ex(ii1,j1,k), &
    &                     Ex(ii,j,k1),Ex(ii1,j,k1),Ex(ii,j1,k1),Ex(ii1,j1,k1))
    F(2)=interpolatePFD3D(x,yy,z,Ey(i,jj,k),Ey(i1,jj,k),Ey(i,jj1,k),Ey(i1,jj1,k), &
    &                     Ey(i,jj,k1),Ey(i1,jj,k1),Ey(i,jj1,k1),Ey(i1,jj1,k1))
    F(3)=interpolatePFD3D(x,y,zz,Ez(i,j,kk),Ez(i1,j,kk),Ez(i,j1,kk),Ez(i1,j1,kk), &
    &                     Ez(i,j,kk1),Ez(i1,j,kk1),Ez(i,j1,kk1),Ez(i1,j1,kk1))
    F(4)=interpolatePFD3D(x,yy,zz,Hx(i,jj,kk),Hx(i1,jj,kk),Hx(i,jj1,kk),Hx(i1,jj1,kk), &
    &                     Hx(i,jj,kk1),Hx(i1,jj,kk1),Hx(i,jj1,kk1),Hx(i1,jj1,kk1))
    F(5)=interpolatePFD3D(xx,y,zz,Hy(ii,j,kk),Hy(ii1,j,kk),Hy(ii,j1,kk),Hy(ii1,j1,kk), &
    &                     Hy(ii,j,kk1),Hy(ii1,j,kk1),Hy(ii,j1,kk1),Hy(ii1,j1,kk1))
    F(6)=interpolatePFD3D(xx,yy,z,Hz(ii,jj,k),Hz(ii1,jj,k),Hz(ii,jj1,k),Hz(ii1,jj1,k), &
    &                     Hz(ii,jj,k1),Hz(ii1,jj,k1),Hz(ii,jj1,k1),Hz(ii1,jj1,k1))
  end Subroutine getPFDrFld3D

  Subroutine getPFDrFld2E(r,F)
! get real field vector f at location r from the PFD field using linear interpolation
    implicit none
    Real(8) r(2),F(6),x,y,xx,yy
    Integer(4) i,j,i1,j1,ii,ii1,jj,jj1
    call getPFDix(r(1),PFDxmin,PFDdx,nPFDi,i,i1,x)
    call getPFDix(r(1)-0.5d0*PFDdx,PFDxmin,PFDdx,nPFDi,ii,ii1,xx)
    call getPFDix(r(2),PFDymin,PFDdy,nPFDj,j,j1,y)
    call getPFDix(r(2)-0.5d0*PFDdy,PFDymin,PFDdy,nPFDj,jj,jj1,yy)
    F(1)=0.0d0
    F(2)=0.0d0
    F(3)=interpolatePFD2D(x,y,Ez(i,j,1),Ez(i1,j,1),Ez(i,j1,1),Ez(i1,j1,1))
    F(4)=interpolatePFD2D(x,yy,Hx(i,jj,1),Hx(i1,jj,1),Hx(i,jj1,1),Hx(i1,jj1,1))
    F(5)=interpolatePFD2D(xx,y,Hy(ii,j,1),Hy(ii1,j,1),Hy(ii,j1,1),Hy(ii1,j1,1))
    F(6)=0.0d0
  end Subroutine getPFDrFld2E

  Subroutine getPFDrFld2H(r,F)
! get real field vector f at location r from the PFD field using linear interpolation
    Implicit none
    Real(8) r(2),F(6),x,y,xx,yy
    Integer(4) i,j,i1,j1,ii,ii1,jj,jj1
    call getPFDix(r(1),PFDxmin,PFDdx,nPFDi,i,i1,x)
    call getPFDix(r(1)-0.5d0*PFDdx,PFDxmin,PFDdx,nPFDi,ii,ii1,xx)
    call getPFDix(r(2),PFDymin,PFDdy,nPFDj,j,j1,y)
    call getPFDix(r(2)-0.5d0*PFDdy,PFDymin,PFDdy,nPFDj,jj,jj1,yy)
    F(1)=interpolatePFD2D(xx,y,Ex(ii,j,1),Ex(ii1,j,1),Ex(ii,j1,1),Ex(ii1,j1,1))
    F(2)=interpolatePFD2D(x,yy,Ey(i,jj,1),Ey(i1,jj,1),Ey(i,jj1,1),Ey(i1,jj1,1))
    F(3)=0.0d0
    F(4)=0.0d0
    F(5)=0.0d0
    F(6)=interpolatePFD2D(xx,yy,Hz(ii,jj,1),Hz(ii1,jj,1),Hz(ii,jj1,1),Hz(ii1,jj1,1))
  end Subroutine getPFDrFld2H

  Subroutine getPFDix(xin,xmin,dx,n,i,i1,x)
! get the cell numbers i and i1=i+1 and the scaled location x for a point xin
! the PFD grid starts at xin=xmin with i=1, dx is the cell length
    Implicit none
    Real(8) xin,xmin,dx,x
    Integer(4) i,i1
    Integer(2) n
    i=Int((xin-xmin)/dx)+1_4
    i1=i+1_4
    i=max(1_4,min(Int4(n),i))
    i1=max(1_4,min(Int4(n),i1))
    x=(xin-Dble(i-1)*dx-xmin)/dx
  end Subroutine getPFDix

  Real(8) Function interpolatePFD2D(x,y,f00,f10,f01,f11)
! use linear interpolation for a rectangular cell with corner values f00,f10,f01,f11
! x,y are scaled local coordinates (x=0,y=0 at lower left corner, x=1,y=1 at upper right corner)
    Implicit none
    Real(8) x,y,f00,f10,f01,f11,f0,f1
    if(x.le.0.0d0) then
      f0=f00
      f1=f01
    else if(x.ge.1.0d0) then
      f0=f10
      f1=f11
    else
      f0=(1.0d0-x)*f00+x*f10
      f1=(1.0d0-x)*f01+x*f11
    end if
    if(y.le.0.0d0) then
      interpolatePFD2D=f0
    else if(y.ge.1.0d0) then
      interpolatePFD2D=f1
    else
      interpolatePFD2D=(1.0d0-y)*f0+y*f1
    end if
  end Function interpolatePFD2D

  Real(8) Function interpolatePFD3D(x,y,z,f000,f100,f010,f110,f001,f101,f011,f111)
! use linear interpolation for a rectangular box
    Implicit none
    Real(8) x,y,z,f000,f100,f010,f110,f001,f101,f011,f111,f0,f1
    if(z.le.0.0d0) then
      f0=interpolatePFD2D(x,y,f000,f100,f010,f110)
      interpolatePFD3D=f0
    else if(z.ge.1.0d0) then
      f1=interpolatePFD2D(x,y,f001,f101,f011,f111)
      interpolatePFD3D=f1
    else
      f0=interpolatePFD2D(x,y,f000,f100,f010,f110)
      f1=interpolatePFD2D(x,y,f001,f101,f011,f111)
      interpolatePFD3D=(1.0d0-z)*f0+z*f1
    end if
  end Function interpolatePFD3D

  Real(8) Function timeDepPFD(t,iP)
! simple Example of time dependence of the sources
    Integer(4) iP
    Real(8) t,f
    Complex(8) c
    Select Case(abs(iPFDft))
! envelope
    Case(0) ! always 0
      f=0.0d0
    Case(1) ! cos-sqr pulse
      if((t.le.0.0d0).or.(t.ge.2*PFDfTmax+PFDfTau)) then
        f=0.0d0
      else if((t.gt.PFDfTmax).and.(t.le.PFDfTmax+PFDfTau)) then
        f=1.0d0
      else if(t.lt.PFDfTmax) then
        f=sin(0.5d0*Pi*t/PFDfTmax)**2
      else
        f=sin(0.5d0*Pi*(t-PFDfTau)/PFDfTmax)**2
      end if
    Case(2) ! exp-...2 ramp
      if(t.lt.PFDfTmax) then
        f=-((t-PFDfTmax)/(PFDfTau))**2
        if(f.lt.-700.0d0) then
          f=0.0d0
        else
          f=dExp(f)
        end if
      else
        f=1.0d0
      end if
    Case(3) ! cos-sqr ramp
      if(t.lt.0.0d0) then
        f=0.0d0
      else if(t.gt.PFDfTmax) then
        f=1.0d0
      else
        f=sin(0.5d0*Pi*t/PFDfTmax)**2
      end if
    Case Default ! exp-...2 pulse
      f=-((t-PFDfTmax)/(PFDfTau))**2
      if(f.lt.-700.0d0) then
        f=0.0d0
      else
        f=dExp(f)
      end if
    end Select
    timeDepPFD=f
    if(iPFDft.lt.0) return
! harmonic part
    if(.not.lPFDMMP) then
      if(iP.eq.0) then
        timeDepPFD=timeDepPFD*dsin(Dble(fcFld)*2.0d0*Pi*t)
      else if(iP.gt.0) then
        c=cdexp(fcFld*(0.0d0,-2.0d0)*Pi*t)*PFDsourceA(iP)
        timeDepPFD=timeDepPFD*Dimag(c)
      end if
    end if
  end Function timeDepPFD

  Subroutine PFDboundary(Typ,Field)
    Implicit none
    Character(1) Typ,Field
    if(Typ.eq.'E') then ! 2DE
      if(Field.eq.'E') then ! E field
        if(nPFDil.lt.-1) then ! periodic
          Ez(0:1,0:nPFDj+1,1)=Ez(nPFDi-1:nPFDi,0:nPFDj+1,1)
        else if(nPFDil.eq.0) then ! PEC
          Ez(0,0:nPFDj+1,1)=-Ez(2,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Ez(0,0:nPFDj+1,1)=0.0d0
        else if(nPFDil.eq.-1) then ! PMC
          Ez(0,0:nPFDj+1,1)=Ez(2,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Ez(nPFDi:nPFDi+1,0:nPFDj+1,1)=Ez(1:2,0:nPFDj+1,1)
        else if(nPFDih.eq.0) then ! PEC
          Ez(nPFDi+1,0:nPFDj+1,1)=-Ez(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Ez(nPFDi+1,0:nPFDj+1,1)=0.0d0
        else if(nPFDih.eq.-1) then ! PMC
          Ez(nPFDi+1,0:nPFDj+1,1)=Ez(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Ez(0:nPFDi+1,0:1,1)=Ez(0:nPFDi+1,nPFDj-1:nPFDj,1)
        else if(nPFDjl.eq.0) then ! PEC
          Ez(0:nPFDi+1,0,1)=-Ez(0:nPFDi+1,2,1)
        else if(nPFDjl.gt.0) then ! UPML
          Ez(0:nPFDi+1,0,1)=0.0d0
        else if(nPFDjl.eq.-1) then ! PMC
          Ez(0:nPFDi+1,0,1)=Ez(0:nPFDi+1,2,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Ez(0:nPFDi+1,nPFDj:nPFDj+1,1)=Ez(0:nPFDi+1,1:2,1)
        else if(nPFDjh.eq.0) then ! PEC
          Ez(0:nPFDi+1,nPFDj+1,1)=-Ez(0:nPFDi+1,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Ez(0:nPFDi+1,nPFDj+1,1)=0.0d0
        else if(nPFDjh.eq.-1) then ! PMC
          Ez(0:nPFDi+1,nPFDj+1,1)=Ez(0:nPFDi+1,nPFDj-1,1)
        end if
      else ! H field
        if(nPFDil.lt.-1) then ! periodic
          Hx(0:1,0:nPFDj+1,1)=Hx(nPFDi-1:nPFDi,0:nPFDj+1,1)
          Hy(0:1,0:nPFDj+1,1)=Hy(nPFDi-1:nPFDi,0:nPFDj+1,1)
        else if(nPFDil.eq.0) then ! PEC
          Hx(0,0:nPFDj+1,1)=-Hx(2,0:nPFDj+1,1)
          Hy(0,0:nPFDj+1,1)=Hy(1,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Hx(0,0:nPFDj+1,1)=0.0d0
          Hy(0,0:nPFDj+1,1)=0.0d0
        else if(nPFDil.eq.-1) then ! PMC
          Hx(0,0:nPFDj+1,1)=Hx(2,0:nPFDj+1,1)
          Hy(0,0:nPFDj+1,1)=-Hy(1,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Hx(nPFDi:nPFDi+1,0:nPFDj+1,1)=Hx(1:2,0:nPFDj+1,1)
          Hy(nPFDi:nPFDi+1,0:nPFDj+1,1)=Hy(1:2,0:nPFDj+1,1)
        else if(nPFDih.eq.0) then ! PEC
          Hx(nPFDi+1,0:nPFDj+1,1)=-Hx(nPFDi-1,0:nPFDj+1,1)
          Hy(nPFDi,0:nPFDj+1,1)=Hy(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Hx(nPFDi+1,0:nPFDj+1,1)=0.0d0
          Hy(nPFDi+1,0:nPFDj+1,1)=0.0d0
        else if(nPFDih.eq.-1) then ! PMC
          Hx(nPFDi+1,0:nPFDj+1,1)=Hx(nPFDi-1,0:nPFDj+1,1)
          Hy(nPFDi,0:nPFDj+1,1)=-Hy(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Hx(0:nPFDi+1,0:1,1)=Hx(0:nPFDi+1,nPFDj-1:nPFDj,1)
          Hy(0:nPFDi+1,0:1,1)=Hy(0:nPFDi+1,nPFDj-1:nPFDj,1)
        else if(nPFDjl.eq.0) then ! PEC
          Hx(0:nPFDi+1,0,1)=Hx(0:nPFDi+1,1,1)
          Hy(0:nPFDi+1,0,1)=-Hy(0:nPFDi+1,2,1)
        else if(nPFDjl.gt.0) then ! UPML
          Hx(0:nPFDi+1,0,1)=0.0d0
          Hy(0:nPFDi+1,0,1)=0.0d0
        else if(nPFDjl.eq.-1) then ! PMC
          Hx(0:nPFDi+1,0,1)=-Hx(0:nPFDi+1,1,1)
          Hy(0:nPFDi+1,0,1)=Hy(0:nPFDi+1,2,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Hx(0:nPFDi+1,nPFDj:nPFDj+1,1)=Hx(0:nPFDi+1,1:2,1)
          Hy(0:nPFDi+1,nPFDj:nPFDj+1,1)=Hy(0:nPFDi+1,1:2,1)
        else if(nPFDjh.eq.0) then ! PEC
          Hx(0:nPFDi+1,nPFDj,1)=Hx(0:nPFDi+1,nPFDj-1,1)
          Hy(0:nPFDi+1,nPFDj+1,1)=-Hy(0:nPFDi+1,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Hx(0:nPFDi+1,nPFDj+1,1)=0.0d0
          Hy(0:nPFDi+1,nPFDj+1,1)=0.0d0
        else if(nPFDjh.eq.-1) then ! PMC
          Hx(0:nPFDi+1,nPFDj,1)=-Hx(0:nPFDi+1,nPFDj-1,1)
          Hy(0:nPFDi+1,nPFDj+1,1)=Hy(0:nPFDi+1,nPFDj-1,1)
        end if
      end if
    else if(Typ.eq.'H') then ! 2DH
      if(Field.eq.'E') then ! E field
        if(nPFDil.lt.-1) then ! periodic
          Ex(0:1,0:nPFDj+1,1)=Ex(nPFDi-1:nPFDi,0:nPFDj+1,1)
          Ey(0:1,0:nPFDj+1,1)=Ey(nPFDi-1:nPFDi,0:nPFDj+1,1)
        else if(nPFDil.eq.0) then ! PEC
          Ex(0,0:nPFDj+1,1)=Ex(1,0:nPFDj+1,1)
          Ey(0,0:nPFDj+1,1)=-Ey(2,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Ex(0,0:nPFDj+1,1)=0.0d0
          Ey(0,0:nPFDj+1,1)=0.0d0
        else if(nPFDil.eq.-1) then ! PMC
          Ex(0,0:nPFDj+1,1)=-Ex(1,0:nPFDj+1,1)
          Ey(0,0:nPFDj+1,1)=Ey(2,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Ex(nPFDi:nPFDi+1,0:nPFDj+1,1)=Ex(1:2,0:nPFDj+1,1)
          Ey(nPFDi:nPFDi+1,0:nPFDj+1,1)=Ey(1:2,0:nPFDj+1,1)
        else if(nPFDih.eq.0) then ! PEC
          Ex(nPFDi,0:nPFDj+1,1)=Ex(nPFDi-1,0:nPFDj+1,1)
          Ey(nPFDi+1,0:nPFDj+1,1)=-Ey(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Ex(nPFDi+1,0:nPFDj+1,1)=0.0d0
          Ey(nPFDi+1,0:nPFDj+1,1)=0.0d0
        else if(nPFDih.eq.-1) then ! PMC
          Ex(nPFDi,0:nPFDj+1,1)=-Ex(nPFDi-1,0:nPFDj+1,1)
          Ey(nPFDi+1,0:nPFDj+1,1)=Ey(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Ex(0:nPFDi+1,0:1,1)=Ex(0:nPFDi+1,nPFDj-1:nPFDj,1)
          Ey(0:nPFDi+1,0:1,1)=Ey(0:nPFDi+1,nPFDj-1:nPFDj,1)
        else if(nPFDjl.eq.0) then ! PEC
          Ex(0:nPFDi+1,0,1)=-Ex(0:nPFDi+1,2,1)
          Ey(0:nPFDi+1,0,1)=Ey(0:nPFDi+1,1,1)
        else if(nPFDjl.gt.0) then ! UPML
          Ex(0:nPFDi+1,0,1)=0.0d0
          Ey(0:nPFDi+1,0,1)=0.0d0
        else if(nPFDjl.eq.-1) then ! PMC
          Ex(0:nPFDi+1,0,1)=Ex(0:nPFDi+1,2,1)
          Ey(0:nPFDi+1,0,1)=-Ey(0:nPFDi+1,1,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Ex(0:nPFDi+1,nPFDj:nPFDj+1,1)=Ex(0:nPFDi+1,1:2,1)
          Ey(0:nPFDi+1,nPFDj:nPFDj+1,1)=Ey(0:nPFDi+1,1:2,1)
        else if(nPFDjh.eq.0) then ! PEC
          Ex(0:nPFDi+1,nPFDj+1,1)=-Ex(0:nPFDi+1,nPFDj-1,1)
          Ey(0:nPFDi+1,nPFDj,1)=Ey(0:nPFDi+1,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Ex(0:nPFDi+1,nPFDj+1,1)=0.0d0
          Ey(0:nPFDi+1,nPFDj+1,1)=0.0d0
        else if(nPFDjh.eq.-1) then ! PMC
          Ex(0:nPFDi+1,nPFDj+1,1)=Ex(0:nPFDi+1,nPFDj-1,1)
          Ey(0:nPFDi+1,nPFDj,1)=-Ey(0:nPFDi+1,nPFDj-1,1)
        end if
      else ! H field
        if(nPFDil.lt.-1) then ! periodic
          Hz(0:1,0:nPFDj+1,1)=Hz(nPFDi-1:nPFDi,0:nPFDj+1,1)
        else if(nPFDil.eq.0) then ! PEC
          Hz(0,0:nPFDj+1,1)=Hz(1,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Hz(0,0:nPFDj+1,1)=0.0d0
        else if(nPFDil.eq.-1) then ! PMC
          Hz(0,0:nPFDj+1,1)=-Hz(1,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Hz(nPFDi:nPFDi+1,0:nPFDj+1,1)=Hz(1:2,0:nPFDj+1,1)
        else if(nPFDih.eq.0) then ! PEC
          Hz(nPFDi,0:nPFDj+1,1)=Hz(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Hz(nPFDi+1,0:nPFDj+1,1)=0.0d0
        else if(nPFDih.eq.-1) then ! PMC
          Hz(nPFDi,0:nPFDj+1,1)=-Hz(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Hz(0:nPFDi+1,0:1,1)=Hz(0:nPFDi+1,nPFDj-1:nPFDj,1)
        else if(nPFDjl.eq.0) then ! PEC
          Hz(0:nPFDi+1,0,1)=Hz(0:nPFDi+1,1,1)
        else if(nPFDjl.gt.0) then ! UPML
          Hz(0:nPFDi+1,0,1)=0.0d0
        else if(nPFDjl.eq.-1) then ! PMC
          Hz(0:nPFDi+1,0,1)=-Hz(0:nPFDi+1,1,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Hz(0:nPFDi+1,nPFDj:nPFDj+1,1)=Hz(0:nPFDi+1,1:2,1)
        else if(nPFDjh.eq.0) then ! PEC
          Hz(0:nPFDi,nPFDj,1)=Hz(0:nPFDi,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Hz(0:nPFDi,nPFDj+1,1)=0.0d0
        else if(nPFDjh.eq.-1) then ! PMC
          Hz(0:nPFDi,nPFDj,1)=-Hz(0:nPFDi,nPFDj-1,1)
        end if
      end if
    else ! 3D
      if(Field.eq.'E') then ! E field
        if(nPFDil.lt.-1) then ! periodic
          Ex(0:1,0:nPFDj+1,0:nPFDk+1)=Ex(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)
          Ey(0:1,0:nPFDj+1,0:nPFDk+1)=Ey(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)
          Ez(0:1,0:nPFDj+1,0:nPFDk+1)=Ez(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDil.eq.0) then ! PEC
          Ex(0,0:nPFDj+1,0:nPFDk+1)=Ex(1,0:nPFDj+1,0:nPFDk+1)
          Ey(0,0:nPFDj+1,0:nPFDk+1)=-Ey(2,0:nPFDj+1,0:nPFDk+1)
          Ez(0,0:nPFDj+1,0:nPFDk+1)=-Ez(2,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDil.gt.0) then ! UPML
          Ex(0,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Ey(0,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Ez(0,0:nPFDj+1,0:nPFDk+1)=0.0d0
        else if(nPFDil.eq.-1) then ! PMC
          Ex(0,0:nPFDj+1,0:nPFDk+1)=-Ex(1,0:nPFDj+1,0:nPFDk+1)
          Ey(0,0:nPFDj+1,0:nPFDk+1)=Ey(2,0:nPFDj+1,0:nPFDk+1)
          Ez(0,0:nPFDj+1,0:nPFDk+1)=Ez(2,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Ex(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ex(1:2,0:nPFDj+1,0:nPFDk+1)
          Ey(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ey(1:2,0:nPFDj+1,0:nPFDk+1)
          Ez(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ez(1:2,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDih.eq.0) then ! PEC
          Ex(nPFDi,0:nPFDj+1,0:nPFDk+1)=Ex(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Ey(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=-Ey(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Ez(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=-Ez(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDih.gt.0) then ! UPML
          Ex(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Ey(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Ez(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=0.0d0
        else if(nPFDih.eq.-1) then ! PMC
          Ex(nPFDi,0:nPFDj+1,0:nPFDk+1)=-Ex(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Ey(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ey(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Ez(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ez(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Ex(0:nPFDi+1,0:1,0:nPFDk+1)=Ex(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)
          Ey(0:nPFDi+1,0:1,0:nPFDk+1)=Ey(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)
          Ez(0:nPFDi+1,0:1,0:nPFDk+1)=Ez(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)
        else if(nPFDjl.eq.0) then ! PEC
          Ex(0:nPFDi+1,0,0:nPFDk+1)=-Ex(0:nPFDi+1,2,0:nPFDk+1)
          Ey(0:nPFDi+1,0,0:nPFDk+1)=Ey(0:nPFDi+1,1,0:nPFDk+1)
          Ez(0:nPFDi+1,0,0:nPFDk+1)=-Ez(0:nPFDi+1,2,0:nPFDk+1)
        else if(nPFDjl.gt.0) then ! UPML
          Ex(0:nPFDi+1,0,0:nPFDk+1)=0.0d0
          Ey(0:nPFDi+1,0,0:nPFDk+1)=0.0d0
          Ez(0:nPFDi+1,0,0:nPFDk+1)=0.0d0
        else if(nPFDjl.eq.-1) then ! PMC
          Ex(0:nPFDi+1,0,0:nPFDk+1)=Ex(0:nPFDi+1,2,0:nPFDk+1)
          Ey(0:nPFDi+1,0,0:nPFDk+1)=-Ey(0:nPFDi+1,1,0:nPFDk+1)
          Ez(0:nPFDi+1,0,0:nPFDk+1)=Ez(0:nPFDi+1,2,0:nPFDk+1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Ex(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Ex(0:nPFDi+1,1:2,0:nPFDk+1)
          Ey(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Ey(0:nPFDi+1,1:2,0:nPFDk+1)
          Ez(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Ez(0:nPFDi+1,1:2,0:nPFDk+1)
        else if(nPFDjh.eq.0) then ! PEC
          Ex(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=-Ex(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Ey(0:nPFDi+1,nPFDj,0:nPFDk+1)=Ey(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Ez(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=-Ez(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        else if(nPFDjh.gt.0) then ! UPML
          Ex(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=0.0d0
          Ey(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=0.0d0
          Ez(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=0.0d0
        else if(nPFDjh.eq.-1) then ! PMC
          Ex(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=Ex(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Ey(0:nPFDi+1,nPFDj,0:nPFDk+1)=-Ey(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Ez(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=Ez(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        end if
        if(nPFDkl.lt.-1) then ! periodic
          Ex(0:nPFDi+1,0:nPFDj+1,0:1)=Ex(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)
          Ey(0:nPFDi+1,0:nPFDj+1,0:1)=Ey(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)
          Ez(0:nPFDi+1,0:nPFDj+1,0:1)=Ez(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)
        else if(nPFDkl.eq.0) then ! PEC
          Ex(0:nPFDi+1,0:nPFDj+1,0)=-Ex(0:nPFDi+1,0:nPFDj+1,2)
          Ey(0:nPFDi+1,0:nPFDj+1,0)=-Ey(0:nPFDi+1,0:nPFDj+1,2)
          Ez(0:nPFDi+1,0:nPFDj+1,0)=Ez(0:nPFDi+1,0:nPFDj+1,1)
        else if(nPFDkl.gt.0) then ! UPML
          Ex(0:nPFDi+1,0:nPFDj+1,0)=0.0d0
          Ey(0:nPFDi+1,0:nPFDj+1,0)=0.0d0
          Ez(0:nPFDi+1,0:nPFDj+1,0)=0.0d0
        else if(nPFDkl.eq.-1) then ! PMC
          Ex(0:nPFDi+1,0:nPFDj+1,0)=Ex(0:nPFDi+1,0:nPFDj+1,2)
          Ey(0:nPFDi+1,0:nPFDj+1,0)=Ey(0:nPFDi+1,0:nPFDj+1,2)
          Ez(0:nPFDi+1,0:nPFDj+1,0)=-Ez(0:nPFDi+1,0:nPFDj+1,1)
        end if
        if(nPFDkh.lt.-1) then ! periodic
          Ex(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Ex(0:nPFDi+1,0:nPFDj+1,1:2)
          Ey(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Ey(0:nPFDi+1,0:nPFDj+1,1:2)
          Ez(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Ez(0:nPFDi+1,0:nPFDj+1,1:2)
        else if(nPFDkh.eq.0) then ! PEC
          Ex(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=-Ex(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Ey(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=-Ey(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Ez(0:nPFDi+1,0:nPFDj+1,nPFDk)=Ez(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        else if(nPFDkh.gt.0) then ! UPML
          Ex(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=0.0d0
          Ey(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=0.0d0
          Ez(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=0.0d0
        else if(nPFDkh.eq.-1) then ! PMC
          Ex(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=Ex(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Ey(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=Ey(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Ez(0:nPFDi+1,0:nPFDj+1,nPFDk)=-Ez(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        end if
      else ! H field
        if(nPFDil.lt.-1) then ! periodic
          Hx(0:1,0:nPFDj+1,0:nPFDk+1)=Hx(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)
          Hy(0:1,0:nPFDj+1,0:nPFDk+1)=Hy(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)
          Hz(0:1,0:nPFDj+1,0:nPFDk+1)=Hz(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDil.eq.0) then ! PEC
          Hx(0,0:nPFDj+1,0:nPFDk+1)=-Hx(2,0:nPFDj+1,0:nPFDk+1)
          Hy(0,0:nPFDj+1,0:nPFDk+1)=Hy(1,0:nPFDj+1,0:nPFDk+1)
          Hz(0,0:nPFDj+1,0:nPFDk+1)=Hz(1,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDil.gt.0) then ! UPML
          Hx(0,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Hy(0,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Hz(0,0:nPFDj+1,0:nPFDk+1)=0.0d0
        else if(nPFDil.eq.-1) then ! PMC
          Hx(0,0:nPFDj+1,0:nPFDk+1)=Hx(2,0:nPFDj+1,0:nPFDk+1)
          Hy(0,0:nPFDj+1,0:nPFDk+1)=-Hy(1,0:nPFDj+1,0:nPFDk+1)
          Hz(0,0:nPFDj+1,0:nPFDk+1)=-Hz(1,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Hx(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hx(1:2,0:nPFDj+1,0:nPFDk+1)
          Hy(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hy(1:2,0:nPFDj+1,0:nPFDk+1)
          Hz(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hz(1:2,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDih.eq.0) then ! PEC
          Hx(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=-Hx(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hy(nPFDi,0:nPFDj+1,0:nPFDk+1)=Hy(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hz(nPFDi,0:nPFDj+1,0:nPFDk+1)=Hz(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDih.gt.0) then ! UPML
          Hx(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Hy(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=0.0d0
          Hz(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=0.0d0
        else if(nPFDih.eq.-1) then ! PMC
          Hx(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hx(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hy(nPFDi,0:nPFDj+1,0:nPFDk+1)=-Hy(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hz(nPFDi,0:nPFDj+1,0:nPFDk+1)=-Hz(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Hx(0:nPFDi+1,0:1,0:nPFDk+1)=Hx(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)
          Hy(0:nPFDi+1,0:1,0:nPFDk+1)=Hy(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)
          Hz(0:nPFDi+1,0:1,0:nPFDk+1)=Hz(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)
        else if(nPFDjl.eq.0) then ! PEC
          Hx(0:nPFDi+1,0,0:nPFDk+1)=Hx(0:nPFDi+1,1,0:nPFDk+1)
          Hy(0:nPFDi+1,0,0:nPFDk+1)=-Hy(0:nPFDi+1,2,0:nPFDk+1)
          Hz(0:nPFDi+1,0,0:nPFDk+1)=Hz(0:nPFDi+1,1,0:nPFDk+1)
        else if(nPFDjl.gt.0) then ! UPML
          Hx(0:nPFDi+1,0,0:nPFDk+1)=0.0d0
          Hy(0:nPFDi+1,0,0:nPFDk+1)=0.0d0
          Hz(0:nPFDi+1,0,0:nPFDk+1)=0.0d0
        else if(nPFDjl.eq.-1) then ! PMC
          Hx(0:nPFDi+1,0,0:nPFDk+1)=-Hx(0:nPFDi+1,1,0:nPFDk+1)
          Hy(0:nPFDi+1,0,0:nPFDk+1)=Hy(0:nPFDi+1,2,0:nPFDk+1)
          Hz(0:nPFDi+1,0,0:nPFDk+1)=-Hz(0:nPFDi+1,1,0:nPFDk+1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Hx(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Hx(0:nPFDi+1,1:2,0:nPFDk+1)
          Hy(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Hy(0:nPFDi+1,1:2,0:nPFDk+1)
          Hz(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Hz(0:nPFDi+1,1:2,0:nPFDk+1)
        else if(nPFDjh.eq.0) then ! PEC
          Hx(0:nPFDi+1,nPFDj,0:nPFDk+1)=Hx(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hy(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=-Hy(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hz(0:nPFDi+1,nPFDj,0:nPFDk+1)=Hz(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        else if(nPFDjh.gt.0) then ! UPML
          Hx(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=0.0d0
          Hy(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=0.0d0
          Hz(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=0.0d0
        else if(nPFDjh.eq.-1) then ! PMC
          Hx(0:nPFDi+1,nPFDj,0:nPFDk+1)=-Hx(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hy(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=Hy(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hz(0:nPFDi+1,nPFDj,0:nPFDk+1)=-Hz(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        end if
        if(nPFDkl.lt.-1) then ! periodic
          Hx(0:nPFDi+1,0:nPFDj+1,0:1)=Hx(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)
          Hy(0:nPFDi+1,0:nPFDj+1,0:1)=Hy(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)
          Hz(0:nPFDi+1,0:nPFDj+1,0:1)=Hz(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)
        else if(nPFDkl.eq.0) then ! PEC
          Hx(0:nPFDi+1,0:nPFDj+1,0)=Hx(0:nPFDi+1,0:nPFDj+1,1)
          Hy(0:nPFDi+1,0:nPFDj+1,0)=Hy(0:nPFDi+1,0:nPFDj+1,1)
          Hz(0:nPFDi+1,0:nPFDj+1,0)=-Hz(0:nPFDi+1,0:nPFDj+1,2)
        else if(nPFDkl.gt.0) then ! UPML
          Hx(0:nPFDi+1,0:nPFDj+1,0)=0.0d0
          Hy(0:nPFDi+1,0:nPFDj+1,0)=0.0d0
          Hz(0:nPFDi+1,0:nPFDj+1,0)=0.0d0
        else if(nPFDkl.eq.-1) then ! PMC
          Hx(0:nPFDi+1,0:nPFDj+1,0)=-Hx(0:nPFDi+1,0:nPFDj+1,1)
          Hy(0:nPFDi+1,0:nPFDj+1,0)=-Hy(0:nPFDi+1,0:nPFDj+1,1)
          Hz(0:nPFDi+1,0:nPFDj+1,0)=Hz(0:nPFDi+1,0:nPFDj+1,2)
        end if
        if(nPFDkh.lt.-1) then ! periodic
          Hx(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Hx(0:nPFDi+1,0:nPFDj+1,1:2)
          Hy(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Hy(0:nPFDi+1,0:nPFDj+1,1:2)
          Hz(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Hz(0:nPFDi+1,0:nPFDj+1,1:2)
        else if(nPFDkh.eq.0) then ! PEC
          Hx(0:nPFDi+1,0:nPFDj+1,nPFDk)=Hx(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hy(0:nPFDi+1,0:nPFDj+1,nPFDk)=Hy(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hz(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=-Hz(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        else if(nPFDkh.gt.0) then ! UPML
          Hx(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=0.0d0
          Hy(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=0.0d0
          Hz(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=0.0d0
        else if(nPFDkh.eq.-1) then ! PMC
          Hx(0:nPFDi+1,0:nPFDj+1,nPFDk)=-Hx(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hy(0:nPFDi+1,0:nPFDj+1,nPFDk)=-Hy(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hz(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=Hz(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        end if
      end if
    end if
  end Subroutine PFDboundary

  Subroutine DeAllocatePFD()
    Integer(4) ierr
    Deallocate(Dx,stat=ierr)
    Deallocate(Dy,stat=ierr)
    Deallocate(Dz,stat=ierr)
    Deallocate(Ex,stat=ierr)
    Deallocate(Ey,stat=ierr)
    Deallocate(Ez,stat=ierr)
    Deallocate(Bx,stat=ierr)
    Deallocate(By,stat=ierr)
    Deallocate(Bz,stat=ierr)
    Deallocate(Hx,stat=ierr)
    Deallocate(Hy,stat=ierr)
    Deallocate(Hz,stat=ierr)
    Deallocate(Ix,stat=ierr)
    Deallocate(Iy,stat=ierr)
    Deallocate(Iz,stat=ierr)
    Deallocate(Jx,stat=ierr)
    Deallocate(Jy,stat=ierr)
    Deallocate(Jz,stat=ierr)
    Deallocate(Kx,stat=ierr)
    Deallocate(Ky,stat=ierr)
    Deallocate(Kz,stat=ierr)
    Deallocate(Lx,stat=ierr)
    Deallocate(Ly,stat=ierr)
    Deallocate(Lz,stat=ierr)
    Deallocate(Mx,stat=ierr)
    Deallocate(My,stat=ierr)
    Deallocate(Mz,stat=ierr)
    Deallocate(Nx,stat=ierr)
    Deallocate(Ny,stat=ierr)
    Deallocate(Nz,stat=ierr)
    Deallocate(Ox,stat=ierr)
    Deallocate(Oy,stat=ierr)
    Deallocate(Oz,stat=ierr)
    Deallocate(O1x,stat=ierr)
    Deallocate(O1y,stat=ierr)
    Deallocate(O1z,stat=ierr)
    Deallocate(O2x,stat=ierr)
    Deallocate(O2y,stat=ierr)
    Deallocate(O2z,stat=ierr)
    Deallocate(Px,stat=ierr)
    Deallocate(Py,stat=ierr)
    Deallocate(Pz,stat=ierr)
    Deallocate(P1x,stat=ierr)
    Deallocate(P1y,stat=ierr)
    Deallocate(P1z,stat=ierr)
    Deallocate(P2x,stat=ierr)
    Deallocate(P2y,stat=ierr)
    Deallocate(P2z,stat=ierr)
    Deallocate(fax,stat=ierr)
    Deallocate(fay,stat=ierr)
    Deallocate(faz,stat=ierr)
    Deallocate(fbx,stat=ierr)
    Deallocate(fby,stat=ierr)
    Deallocate(fbz,stat=ierr)
    Deallocate(fcx,stat=ierr)
    Deallocate(fcy,stat=ierr)
    Deallocate(fcz,stat=ierr)
    Deallocate(fdx,stat=ierr)
    Deallocate(fdy,stat=ierr)
    Deallocate(fdz,stat=ierr)
    Deallocate(gax,stat=ierr)
    Deallocate(gay,stat=ierr)
    Deallocate(gaz,stat=ierr)
    Deallocate(gbx,stat=ierr)
    Deallocate(gby,stat=ierr)
    Deallocate(gbz,stat=ierr)
    Deallocate(gcx,stat=ierr)
    Deallocate(gcy,stat=ierr)
    Deallocate(gcz,stat=ierr)
    Deallocate(gdx,stat=ierr)
    Deallocate(gdy,stat=ierr)
    Deallocate(gdz,stat=ierr)
    Deallocate(EzInc,stat=ierr)
    Deallocate(HyInc,stat=ierr)
    Deallocate(HzInc,stat=ierr)
    Deallocate(EyInc,stat=ierr)
    Deallocate(gi1,stat=ierr)
    Deallocate(gi2,stat=ierr)
    Deallocate(gi3,stat=ierr)
    Deallocate(gj1,stat=ierr)
    Deallocate(gj2,stat=ierr)
    Deallocate(gj3,stat=ierr)
    Deallocate(gk1,stat=ierr)
    Deallocate(gk2,stat=ierr)
    Deallocate(gk3,stat=ierr)
    Deallocate(fi1,stat=ierr)
    Deallocate(fi2,stat=ierr)
    Deallocate(fi3,stat=ierr)
    Deallocate(fj1,stat=ierr)
    Deallocate(fj2,stat=ierr)
    Deallocate(fj3,stat=ierr)
    Deallocate(fk1,stat=ierr)
    Deallocate(fk2,stat=ierr)
    Deallocate(fk3,stat=ierr)
    Deallocate(iDxl,stat=ierr)
    Deallocate(iDxh,stat=ierr)
    Deallocate(iDyl,stat=ierr)
    Deallocate(iDyh,stat=ierr)
    Deallocate(iDzl,stat=ierr)
    Deallocate(iDzh,stat=ierr)
    Deallocate(iBxl,stat=ierr)
    Deallocate(iBxh,stat=ierr)
    Deallocate(iByl,stat=ierr)
    Deallocate(iByh,stat=ierr)
    Deallocate(iBzl,stat=ierr)
    Deallocate(iBzh,stat=ierr)
    Deallocate(iD,stat=ierr)
    lPFDalloc=.false.
  end Subroutine DeAllocatePFD

  Subroutine getPFDl(i,llh,lle,llx,lly,llz,llp,llm,lls,llha,llpw)
    Integer(2) i,i1,i2
    Logical llh,lle,llx,lly,llz,llp,llm,lls,llha,llpw
    llpw=.false.
    llp=.false.
    llm=.false.
    lls=.false.
    llha=.false.
    lle=.false.
    llh=.false.
    llx=.false.
    lly=.false.
    llz=.false.
    i2=iabs(i)
    i1=i2/256_2
    if(i1.gt.0_2) llpw=.true.
    i2=(i2-i1*256_2)
    i1=i2/128_2
    if(i1.gt.0_2) then
      lls=.true.
    else
      llha=.true.
    end if
    i2=(i2-i1*128_2)
    i1=i2/64_2
    if(i1.gt.0_2) llm=.true.
    i2=(i2-i1*64_2)
    i1=i2/32_2
    if(i1.gt.0_2) llp=.true.
    i2=(i2-i1*32_2)
    i1=i2/16_2
    if(i1.gt.0_2) llx=.true.
    i2=(i2-i1*16_2)
    i1=i2/8_2
    if(i1.gt.0_2) lly=.true.
    i2=(i2-i1*8_2)
    i1=i2/4_2
    if(i1.gt.0_2) llz=.true.
    i2=(i2-i1*4_2)
    i1=i2/2_2
    if(i1.gt.0_2) lle=.true.
    i2=(i2-i1*2_2)
    if(i2.gt.0_2) llh=.true.
  end Subroutine getPFDl

  Subroutine getPFDi(llh,lle,llx,lly,llz,llp,llm,lls,llpw,i)
    Integer(2) i
    Logical llh,lle,llx,lly,llz,llp,llm,lls,llpw
    i=0_2
    if(llh) i=i+1_2
    if(lle) i=i+2_2
    if(llz) i=i+4_2
    if(lly) i=i+8_2
    if(llx) i=i+16_2
    if(llp) i=i+32_2
    if(llm) i=i+64_2
    if(lls) i=i+128_2
    if(llpw) i=i+256_2
  end Subroutine getPFDi

  Subroutine getFaFbGaGb(nDim,r,iD,fa,fb,fc,fd,ga,gb,gc,gd)
! get material parameters fa,fb,ga,hb for the H and E field
    Implicit None
    Integer(2), Intent(in):: nDim
    Integer(2) lf,iD
    Real(8) r(3),fa,fb,fc,fd,ga,gb,gc,gd,eEff,uEff
    Logical lFo
    lf=Max(-4_2,Min(Int2(nDom+1_2),iD))
! special cses: PEC, PMC
    if((lf.lt.1_2).or.(lf.gt.nDom)) then
      fa=2.0d0*pBig
      fb=0.0d0
      fc=0.0d0
      ga=2.0d0*pBig
      gb=0.0d0
      gc=0.0d0
      if(lf.eq.-1_2) fa=Mue01 ! PEC: only E=0
      if(lf.eq.-2_2) ga=Eps01 ! PMC: only H=0
      return
    end if
! get local material properties
    call getEUST(Int4(lf),r,lFo)
    if((lFo.and.(nPFDiEff.gt.0)).or.(nPFDiEff.eq.4)) then ! lFo=.true. -> formula defines material properties -> use average material properties
      call EffectiveMaterialFo(nDim,r,eEff,uEff)
      call getABC(Eps0,lf,eEff,ga,gb,gc,gd)
      call getABC(Mue0,lf,uEff,fa,fb,fc,fd)
      return
    end if
! homogeneous material
    call getABC(Eps0,lf,Dble(eDom(lf)),ga,gb,gc,gd)
    call getABC(Mue0,lf,Dble(uDom(lf)),fa,fb,fc,fd)
  end Subroutine getFaFbGaGb

  Subroutine getFaFb(nDim,nDr,r,dr,iD,fa,fb,fc,fd)
! get Domain number iD of point r and compute material parameters fa,fb for the H field
    Implicit None
    Integer(2), Intent(in):: nDim,nDr
    Integer(2) lf,lf2,iD,iD2
    Real(8) r(3),dr,fa,fb,fc,fd,dmin,rmin(3),val(2),en(3),q,eEff,uEff,U1,U2,Ue1,Ue2
    Logical lRsideMin,lFo
    call DistPtObj(0_2,0_2,r(1:3),.true.,dmin,rmin,iD,val,.true.,lRsideMin,iD2)
    lf=Max(-4_2,Min(Int2(nDom+1_2),iD))
    if((lf.lt.1_2).or.(lf.gt.nDom)) then
      fa=2.0d0*pBig
      fb=0.0d0
      fc=0.0d0
      if(lf.eq.-1_2) fa=Mue01 ! PEC: only E=0
      return
    end if
    call getEUST(Int4(lf),r,lFo)
    if(nPFDiEff.lt.1) then ! no effective values
      call getABC(Mue0,lf,Dble(uDom(lf)),fa,fb,fc,fd)
    end if
    if(lFo.or.(nPFDiEff.eq.4)) then ! lFo=.true. -> formula defines material properties -> use average material properties
      call EffectiveMaterialFo(nDim,r,eEff,uEff)
      call getABC(Mue0,lf,uEff,fa,fb,fc,fd)
      return
    end if
    lf2=Max(-4_2,Min(Int2(nDom+1_2),iD2))
    if((lf2.gt.0_2).and.(lf2.le.nDom)) then !!!! was .lt. before !!!???
      call getEUST(Int4(lf2),r,lFo)
      if(lFo) then
        call EffectiveMaterialFo(nDim,r,eEff,uEff)
        call getABC(Mue0,lf,uEff,fa,fb,fc,fd)
        return
      end if
    end if
    if((lf2.lt.1_2).or.(lf2.gt.nDom).or.(nPFDiEff.lt.1)) then
      Ueff=Dble(uDom(lf))
    else
      U1=Dble(uDom(lf))
      U2=Dble(uDom(lf2))
      q=getPFDq(nDim,r,rmin,dmin,dr,lf,lf2,en)
      if(q.lt.1.0d-6) then
        Ueff=U1
      else if(nPFDiEff.eq.1) then
        Ueff=EffectiveMaterial(q,U1,U2,.true.)
      else
        Ue1=EffectiveMaterial(q,U1,U2,.true.)
        Ue2=EffectiveMaterial(q,U1,U2,.false.)
        if(nDr.eq.1_2) then
          Ueff=Ue2*en(1)*en(1)+Ue1*en(2)*en(2)+Ue1*en(3)*en(3)
        else if(nDr.eq.2_2) then
          Ueff=Ue1*en(1)*en(1)+Ue2*en(2)*en(2)+Ue1*en(3)*en(3)
        else
          Ueff=Ue1*en(1)*en(1)+Ue1*en(2)*en(2)+Ue2*en(3)*en(3)
        end if
      end if
    end if
    call getABC(Mue0,lf,uEff,fa,fb,fc,fd)
  end Subroutine getFaFb

  Subroutine getGaGb(nDim,nDr,r,dr,iD,ga,gb,gc,gd)
! get Domain number iD of point r and compute material parameters ga,gb for the E field
    Implicit None
    Integer(2), Intent(in):: nDim,nDr
    Integer(2) lf,lf2,iD,iD2
    Real(8) r(3),dr,ga,gb,gc,gd,dmin,rmin(3),val(2),en(3),q,Eeff,uEff,E1,E2,Ee1,Ee2
    Logical lRsideMin,lFo
    call DistPtObj(0_2,0_2,r(1:3),.true.,dmin,rmin,iD,val,.true.,lRsideMin,iD2)
    lf=Max(-4_2,Min(Int2(nDom+1_2),iD))
    if((lf.lt.1_2).or.(lf.gt.nDom)) then
      ga=2.0d0*pBig
      gb=0.0d0
      gc=0.0d0
      if(lf.eq.-2_2) ga=Eps01 ! PMC: only H=0
      return
    end if
    call getEUST(Int4(lf),r,lFo)
    if(nPFDiEff.lt.1) then ! no effective values
      call getABC(Eps0,lf,Dble(eDom(lf)),ga,gb,gc,gd)
    end if
    if(lFo.or.(nPFDiEff.eq.4)) then ! lFo=.true. -> formula defines material properties -> use average material properties
      call EffectiveMaterialFo(nDim,r,eEff,uEff)
      call getABC(Eps0,lf,eEff,ga,gb,gc,gd)
      return
    end if
    lf2=Max(-4_2,Min(Int2(nDom+1_2),iD2))
    if((lf2.gt.0_2).and.(lf2.le.nDom)) then !!!! was .lt. before !!!???
      call getEUST(Int4(lf2),r,lFo)
      if(lFo) then
        call EffectiveMaterialFo(nDim,r,eEff,uEff)
        call getABC(Eps0,lf,eEff,ga,gb,gc,gd)
        return
      end if
    end if
    if((lf2.lt.1_2).or.(lf2.gt.nDom).or.(nPFDiEff.lt.1)) then
      Eeff=Dble(eDom(lf))
    else
      E1=Dble(eDom(lf))
      call getEUST(Int4(lf2),r)
      E2=Dble(eDom(lf2))
      q=getPFDq(nDim,r,rmin,dmin,dr,lf,lf2,en)
      if(q.lt.1.0d-6) then
        Eeff=E1
      else if(nPFDiEff.eq.1) then
        Eeff=EffectiveMaterial(q,E1,E2,.true.)
      else
        Ee1=EffectiveMaterial(q,E1,E2,.true.)
        Ee2=EffectiveMaterial(q,E1,E2,.false.)
        if(nDr.eq.1_2) then
          Eeff=Ee2*en(1)*en(1)+Ee1*en(2)*en(2)+Ee1*en(3)*en(3)
        else if(nDr.eq.2_2) then
          Eeff=Ee1*en(1)*en(1)+Ee2*en(2)*en(2)+Ee1*en(3)*en(3)
        else
          Eeff=Ee1*en(1)*en(1)+Ee1*en(2)*en(2)+Ee2*en(3)*en(3)
        end if
      end if
    end if
    call getABC(Eps0,lf,eEff,ga,gb,gc,gd)
  end Subroutine getGaGb

  Subroutine EffectiveMaterialFo(nDim,r,eEff,uEff)
! average material properties in the cell: if material defined by formula or nPFDiEff=4
    Implicit None
    Integer(2), Intent(in):: nDim
    Integer(2) i,j,k,n,iD,nc
    Real(8) r(3),eEff,uEff,r0(3),r1(3),d(3),dmin2,rmin2(3),val(2),f
    eEff=0.0d0
    uEff=0.0d0
    n=10
    nc=0
    if(nDim.eq.2) then
      d(1)=PFDdx
      d(2)=PFDdy
      d(3)=0.0d0
      r0=r-0.5d0*d
      d=d/Dble(n)
      r1(1)=r0(1)-0.5d0*d(1)
      do i=1,n
        r1(1)=r1(1)+d(1)
        r1(2)=r0(2)-0.5d0*d(2)
        do j=1,n
          r1(2)=r1(2)+d(2)
          call DistPtObj(0_2,0_2,r1(1:3),.true.,dmin2,rmin2,iD,val,.true.)
          if((iD.lt.1_2).or.(iD.gt.nDom)) Cycle
          call getEUST(Int4(iD),r)
          eEff=eEff+Dble(eDom(iD))
          uEff=uEff+Dble(uDom(iD))
          nc=nc+1
        end do
      end do
    else
      d(1)=PFDdx
      d(2)=PFDdy
      d(3)=PFDdz
      r0=r-0.5d0*d
      d=d/Dble(n)
      r1(1)=r0(1)-0.5d0*d(1)
      do i=1,n
        r1(1)=r1(1)+d(1)
        r1(2)=r0(2)-0.5d0*d(2)
        do j=1,n
          r1(2)=r1(2)+d(2)
          r1(3)=r0(3)-0.5d0*d(3)
          do k=1,n
            r1(3)=r1(3)+d(3)
            call DistPtObj(0_2,0_2,r1(1:3),.true.,dmin2,rmin2,iD,val,.true.)
            if((iD.lt.1_2).or.(iD.gt.nDom)) Cycle
            call getEUST(Int4(iD),r)
            eEff=eEff+Dble(eDom(iD))
            uEff=uEff+Dble(uDom(iD))
            nc=nc+1
          end do
        end do
      end do
    end if
    f=1.0d0/Dble(max(1,nc))
    eEff=eEff*f
    uEff=uEff*f
  end Subroutine EffectiveMaterialFo

  Real(8) Function getPFDq(nDim,r,rmin,dmin,dr,lf,lf2,en0)
    Implicit None
    Integer(2), Intent(in):: lf,lf2,nDim
    Integer(4) i,j,k,n
    Real(8) r(3),rmin(3),en0(3),dmin,dr,q,r0(3),en(3),dxl,dxh,dyl,dyh,rl(3)
    if(nDim.eq.2) then ! 2D
      en0(1:2)=Unit2DVec(rmin(1:2)-r(1:2))
      en0(3)=0.0d0
    else
      en0=Unit3DVec(rmin-r)
    end if
    if(dmin.gt.0.5d0*dr) then ! no boundary inside rectangle
      getPFDq=0.0d0
      return
    end if
    if(nPFDiEff.eq.3) then
      getPFDq=getPFDqOld(nDim,r,dmin,dr,lf,lf2)
      return
    end if
    if(nDim.eq.2) then ! 2D: transform PFD cell to unit square, analytic evaluation of all cases
      r0(1)=(rmin(1)-r(1))/PFDdx
      r0(2)=(rmin(2)-r(2))/PFDdy
      en(1:2)=Unit2DVec(r0(1:2))
      en0(3)=0.0d0
      q=en(2).div.en(1)
      r0(1:2)=r0(1:2)+0.5d0
      dxl=r0(1)+q*(r0(2))
      dxh=r0(1)+q*(r0(2)-1.0d0)
      q=en(1).div.en(2)
      dyl=r0(2)+q*(r0(1))
      dyh=r0(2)+q*(r0(1)-1.0d0)
      if((dxl.ge.0.0d0).and.(dxl.le.1.0d0)) then
        if((dxh.ge.0.0d0).and.(dxh.le.1.0d0)) then
          q=0.5d0*(dxl+dxh)
        else if((dyl.ge.0.0d0).and.(dyl.le.1.0d0)) then
          q=0.5d0*dyl*dxl
        else if((dyh.ge.0.0d0).and.(dyh.le.1.0d0)) then
          q=0.5d0*dyh*(1.0d0-dxl)
        else
          q=0.0d0 ! should not occur!
        end if
      else if((dxh.ge.0.0d0).and.(dxh.le.1.0d0)) then
        if((dyh.ge.0.0d0).and.(dyh.le.1.0d0)) then
          q=0.5d0*(1.0d0-dyh)*(1.0d0-dxh)
        else if((dyl.ge.0.0d0).and.(dyl.le.1.0d0)) then
          q=0.5d0*(1.0d0-dyl)*dxh
        else
          q=0.0d0 ! should not occur!
        end if
      else
        if((dyl.ge.0.0d0).and.(dyl.le.1.0d0).and.(dyh.ge.0.0d0).and.(dyh.le.1.0d0)) then
          q=0.5d0*(dyl+dyh)
        else
          q=0.0d0
        end if
      end if
    else ! 3D: transform PFD cell to unit cube, numerical integration over 9x9x9 points in the cube
      r0(1)=(rmin(1)-r(1))/PFDdx
      r0(2)=(rmin(2)-r(2))/PFDdy
      r0(3)=(rmin(3)-r(3))/PFDdy
      en=Unit3DVec(r0)
      r0(1:3)=r0(1:3)+0.5d0
      n=0
      rl(1)=-0.05d0
      do i=1,9
        rl(1)=rl(1)+0.1d0
        rl(2)=-0.05d0
        do j=1,9
          rl(2)=rl(2)+0.1d0
          rl(3)=-0.05d0
          do k=1,9
            rl(3)=rl(3)+0.1d0
            q=r3Scl_Prod(rl-r0,en)
            if(q.gt.0.0d0) n=n+1
          end do
        end do
      end do
      q=Dble(n)/729.0d0
    end if
    if(q.gt.0.5d0) q=1.0d0-q
    getPFDq=q
  end Function getPFDq

  Real(8) Function getPFDqOld(nDim,r,dmin,dr,lf,lf2)
    Implicit None
    Integer(2), Intent(in):: lf,lf2,nDim
    Integer(2) i,j,k,n,iin,iout,iD
    Real(8) r(3),dmin,dr,q,r0(3),r1(3),d(3),dmin2,rmin2(3),val(2)
    if(dmin.gt.0.5d0*dr) then
      getPFDqOld=0.0d0
      return
    end if
    if(nDim.eq.2) then
      n=10
      d(1)=PFDdx
      d(2)=PFDdy
      d(3)=0.0d0
      r0=r-0.5d0*d
      d=d/Dble(n)
      iin=0
      iout=0
      r1(1)=r0(1)-0.5d0*d(1)
      do i=1,n
        r1(1)=r1(1)+d(1)
        r1(2)=r0(2)-0.5d0*d(2)
        do j=1,n
          r1(2)=r1(2)+d(2)
          call DistPtObj(0_2,0_2,r1(1:3),.true.,dmin2,rmin2,iD,val,.true.)
          if(iD.eq.lf) then
            iin=iin+1
          else if(iD.eq.lf2) then
            iout=iout+1
          end if
        end do
      end do
      q=max(0.0d0,min(1.0d0,Dble(iout)/Dble(iin+iout)))
    else
      n=10
      d(1)=PFDdx
      d(2)=PFDdy
      d(3)=PFDdz
      r0=r-0.5d0*d
      d=d/Dble(n)
      iout=0
      r1(1)=r0(1)-0.5d0*d(1)
      do i=1,n
        r1(1)=r1(1)+d(1)
        r1(2)=r0(2)-0.5d0*d(2)
        do j=1,n
          r1(2)=r1(2)+d(2)
          r1(3)=r0(3)-0.5d0*d(3)
          do k=1,n
            r1(3)=r1(3)+d(3)
            call DistPtObj(0_2,0_2,r1(1:3),.true.,dmin2,rmin2,iD,val,.true.)
            if(iD.eq.lf) then
              iin=iin+1
            else if(iD.eq.lf2) then
              iout=iout+1
            end if
          end do
        end do
      end do
      q=max(0.0d0,min(1.0d0,Dble(iout)/Dble(iin+iout)))
    end if
    getPFDqOld=q
  end Function getPFDqOld

  Real(8) Function EffectiveMaterial(f,e1,e2,lInverse)
    Implicit None
    Real(8) f,e1,e2
    Logical lInverse
    if(abs((e1-e2)/(e1+pSmall)).lt.1.0d-16) then
      EffectiveMaterial=e1
      return
    end if
    if(lInverse) then
      EffectiveMaterial=(e1*e2).div.(f*e1+(1.0d0-f)*e2)
    else
      EffectiveMaterial=f*e2+(1.0d0-f)*e1
    end if
  end Function EffectiveMaterial

  Subroutine n2fMultipole(kE1,kE2,kF)
    Implicit None
    Logical lf
    Integer(4) kE,kE1,kE2,kF,ierr,iWork,nCo,nRo,i,j,idum
    Integer(2) iD
    Real(8) r(3),dmin,rmin(3),val(2)
    Complex(8), Allocatable:: c(:,:),cR(:,:),cW(:),ArrExp(:,:)
    kF=min(nPFDf,max(1,kF))
    fcFld=PFDfmin+Dble(kF-1)*(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
    lf=lfcFld
    lfcFld=.true.
! prepare boundary data (for computing domain numbers in the sensor points)
    call cBndGetABO() ! get c-poly, splines, match.pts
    if(.not.lgcFld) then
      call get3DMatPts(1,nObj,1_4,3_2,.true.)
    end if
! get matrix size
    nRo=nPFDsens*3
    if(.not.lgcFld) nRo=2_4*nRo
    kE1=min(nExp,max(1,kE1))
    kE2=min(nExp,max(1,kE2))
    nCo=0
    do kE=kE1,kE2
      nCo=nCo+Int4(tExp(kE)%nPar)
    end do
    if(nCo.gt.nRo) then
      ierr=MessageBoxQQ('Not enough sensor points!'C,'PFD near2far'C, &
                        MB$OK.or.MB$IconExclamation)
      lfcFld=lf
      return
    end if
! allocate matrices and determine optimal workspace cW
    Allocate(c(nRo,nCo),cR(nRo,1),cW(1),stat=ierr)
    if(ierr.ne.0) then
      ierr=MessageBoxQQ('Memory allocation for QRMtr failed!'C,'PFD near2far'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(c,cR,cW,stat=ierr)
      lfcFld=lf
      return
    end if
    call ZGELS('N',nRo,nCo,1,c,nRo,cR,nRo,cW,-1,ierr)
    iWork=nint(Dble(cW(1)),4)
    if(l4.and.l5) write(*,*) 'ZGELS iWork,iWork/nRow=',iWork,iWork/nRo
    DeAllocate(cW,stat=ierr)
! allocate workspace
    if(ierr.eq.0) Allocate(cW(iWork),stat=ierr)
    if(ierr.ne.0) then
      ierr=MessageBoxQQ('Memory allocation for QRMtr failed!'C,'PFD near2far'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(c,cR,cW,stat=ierr)
      lfcFld=lf
      return
    end if
! setup matrix c
    j=1
    kE=kE1
    if(Allocated(ArrExp)) DeAllocate(ArrExp)
    Allocate(ArrExp(1:10,nParN),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for ArrExp failed!'C,'Allocate expansion parameter'C, &
                        MB$OK.or.MB$IconExclamation)
      if(Allocated(ParExp)) DeAllocate(ParExp)
      kPar=0
      nPar=0
      mPar=0
      return
    end if
    do i=1,nPFDsens
      r(1)=PFDsensX(i)
      r(2)=PFDsensY(i)
      r(3)=PFDsensZ(i)
      call DistPtObj(0_2,0_2,r(1:3),.true.,dmin,rmin,iD,val,.true.)
      call GetArrExp(0,r,iD,iHEGlobal,ArrExp)
      if(.not.lgcFld) then ! 3D
        c(j:j+2,1:nCo)=ArrExp(1:3,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j:j+2,1)=cPFDsens(1:3,kF,i)
        j=j+3
        c(j:j+2,1:nCo)=Zw0*ArrExp(4:6,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j:j+2,1)=Zw0*cPFDsens(4:6,kF,i)
        j=j+3
      else if(iHEGlobal.eq.1_2) then ! 2D Hz
        c(j,1:nCo)=ArrExp(1,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j,1)=cPFDsens(1,kF,i)
        j=j+1
        c(j,1:nCo)=ArrExp(2,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j,1)=cPFDsens(2,kF,i)
        j=j+1
        c(j,1:nCo)=Zw0*ArrExp(6,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j,1)=Zw0*cPFDsens(6,kF,i)
        j=j+1
      else ! 2D Ez
        c(j,1:nCo)=ArrExp(3,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j,1)=cPFDsens(3,kF,i)
        j=j+1
        c(j,1:nCo)=Zw0*ArrExp(4,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j,1)=Zw0*cPFDsens(4,kF,i)
        j=j+1
        c(j,1:nCo)=Zw0*ArrExp(5,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)
        cR(j,1)=Zw0*cPFDsens(5,kF,i)
        j=j+1
      end if
    end do
    if(Allocated(ArrExp)) DeAllocate(ArrExp)
! solve
    call ZGELS('N',nRo,nCo,1,c,nRo,cR,nRo,cW,iWork,ierr)
    if(ierr.ne.0) then
      ierr=MessageBoxQQ('QR decomposition failed'C,'MMP matrix setup'C,MB$OK.or.MB$ICONHAND)
      lfcFld=lf
      DeAllocate(c,cR,cW,stat=ierr)
      return
    end if
    ParExp(kExc,tExp(kE)%iOff+1:tExp(kE)%iOff+nCo)=cR(1:nCo,1)
! deallocate memory
    DeAllocate(c,cR,cW,stat=ierr)
    lfcFld=lf
  end Subroutine n2fMultipole

  Subroutine getPFDdomType()
! evaluate most complex material types (1: dielectric, 2: lossy, 3: Drude, 4: Lorentz, 5: Drude + loss, 6: Drude + Lorentz)
    Implicit none
    Integer(4) i
    iEtype=0
    iUtype=0
    do i=1,nDom
      iEtype=max(iEtype,idDom(1,i))
      iUtype=max(iUtype,idDom(2,i))
    end do
  end Subroutine getPFDdomType

end MODULE CHPFD