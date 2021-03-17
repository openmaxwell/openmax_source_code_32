! Copyright 2021, Christian Hafner
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
MODULE CHCFD

! Domains

  USE CHPFD

  SAVE

    Complex(8) CFDperX,CFDperY,CFDperZ
    Complex(8), Allocatable :: Dxc(:,:,:),Dyc(:,:,:),Dzc(:,:,:),Exc(:,:,:),Eyc(:,:,:),Ezc(:,:,:), &
    &                       Bxc(:,:,:),Byc(:,:,:),Bzc(:,:,:),Hxc(:,:,:),Hyc(:,:,:),Hzc(:,:,:), &
    & Ixc(:,:,:),Iyc(:,:,:),Izc(:,:,:),Jxc(:,:,:),Jyc(:,:,:),Jzc(:,:,:), &
    & Kxc(:,:,:),Kyc(:,:,:),Kzc(:,:,:),Lxc(:,:,:),Lyc(:,:,:),Lzc(:,:,:), &
    & Mxc(:,:,:),Myc(:,:,:),Mzc(:,:,:),Nxc(:,:,:),Nyc(:,:,:),Nzc(:,:,:), &
    & Oxc(:,:,:),Oyc(:,:,:),Ozc(:,:,:),Pxc(:,:,:),Pyc(:,:,:),Pzc(:,:,:), &
    & O1xc(:,:,:),O1yc(:,:,:),O1zc(:,:,:),P1xc(:,:,:),P1yc(:,:,:),P1zc(:,:,:), &
    & O2xc(:,:,:),O2yc(:,:,:),O2zc(:,:,:),P2xc(:,:,:),P2yc(:,:,:),P2zc(:,:,:),XYZ(:,:,:), &
    & EzcInc(:),HycInc(:),HzcInc(:),EycInc(:)

  CONTAINS

  Subroutine TransformCFD3D(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10),rot,expot,expft,fl
    Real(8) xn,xxn,r(3),t,ft,f,df
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
      call CFDinitPer()
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
      call DeAllocateCFD()
      Allocate(Dxc(nPFDi,nPFDj,nPFDk),Dyc(nPFDi,nPFDj,nPFDk),Dzc(nPFDi,nPFDj,nPFDk), &
      &        Exc(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Eyc(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Ezc(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1), &
      &        Bxc(nPFDi,nPFDj,nPFDk),Byc(nPFDi,nPFDj,nPFDk),Bzc(nPFDi,nPFDj,nPFDk), &
      &        Hxc(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Hyc(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1),Hzc(0:nPFDi+1,0:nPFDj+1,0:nPFDk+1), &
      & fax(nPFDi,nPFDj,nPFDk),fay(nPFDi,nPFDj,nPFDk),faz(nPFDi,nPFDj,nPFDk), &
      & gax(nPFDi,nPFDj,nPFDk),gay(nPFDi,nPFDj,nPFDk),gaz(nPFDi,nPFDj,nPFDk), &
      & EzcInc(0:nPFDi+1),HycInc(0:nPFDi+1), &
      & gi1(nPFDi),gi2(nPFDi),gi3(nPFDi),gj1(nPFDj),gj2(nPFDj),gj3(nPFDj),gk1(nPFDk),gk2(nPFDk),gk3(nPFDk), &
      & fi1(nPFDi),fi2(nPFDi),fi3(nPFDi),fj1(nPFDj),fj2(nPFDj),fj3(nPFDj),fk1(nPFDk),fk2(nPFDk),fk3(nPFDk), &
      & iDxl(ia,nPFDj,nPFDk),iDxh(nPFDih+1,nPFDj,nPFDk),iDyl(nPFDi,ja,nPFDk),iDyh(nPFDi,nPFDjh+1,nPFDk), &
      & iDzl(nPFDi,nPFDj,ka),iDzh(nPFDi,nPFDj,nPFDkh+1), &
      & iBxl(ia,nPFDj,nPFDk),iBxh(nPFDih+1,nPFDj,nPFDk),iByl(nPFDi,ja,nPFDk),iByh(nPFDi,nPFDjh+1,nPFDk), &
      & iBzl(nPFDi,nPFDj,ka),iBzh(nPFDi,nPFDj,nPFDkh+1), &
      & iD(6,nPFDi,nPFDj,nPFDk),stat=ierr)
      if((ierr.eq.0).and.(iEtype.eq.1)) then
        Allocate(XYZ(nPFDi,nPFDj,nPFDk),stat=ierr)
        XYZ=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbx(nPFDi,nPFDj,nPFDk),gby(nPFDi,nPFDj,nPFDk),gbz(nPFDi,nPFDj,nPFDk), &
        & Ixc(nPFDi,nPFDj,nPFDk),Iyc(nPFDi,nPFDj,nPFDk),Izc(nPFDi,nPFDj,nPFDk),stat=ierr)
        Ixc=(0.0d0,0.0d0)
        Iyc=(0.0d0,0.0d0)
        Izc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kxc(nPFDi,nPFDj,nPFDk),Kyc(nPFDi,nPFDj,nPFDk),Kzc(nPFDi,nPFDj,nPFDk), &
        & Mxc(nPFDi,nPFDj,nPFDk),Myc(nPFDi,nPFDj,nPFDk),Mzc(nPFDi,nPFDj,nPFDk),stat=ierr)
        Kxc=(0.0d0,0.0d0)
        Kyc=(0.0d0,0.0d0)
        Kzc=(0.0d0,0.0d0)
        Mxc=(0.0d0,0.0d0)
        Myc=(0.0d0,0.0d0)
        Mzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcx(nPFDi,nPFDj,nPFDk),gcy(nPFDi,nPFDj,nPFDk),gcz(nPFDi,nPFDj,nPFDk),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Oxc(nPFDi,nPFDj,nPFDk),Oyc(nPFDi,nPFDj,nPFDk),Ozc(nPFDi,nPFDj,nPFDk),stat=ierr)
        Oxc=(0.0d0,0.0d0)
        Oyc=(0.0d0,0.0d0)
        Ozc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdx(nPFDi,nPFDj,nPFDk),gdy(nPFDi,nPFDj,nPFDk),gdz(nPFDi,nPFDj,nPFDk), &
        & O1xc(nPFDi,nPFDj,nPFDk),O1yc(nPFDi,nPFDj,nPFDk),O1zc(nPFDi,nPFDj,nPFDk), &
        & O2xc(nPFDi,nPFDj,nPFDk),O2yc(nPFDi,nPFDj,nPFDk),O2zc(nPFDi,nPFDj,nPFDk),stat=ierr)
        O1xc=(0.0d0,0.0d0)
        O1yc=(0.0d0,0.0d0)
        O1zc=(0.0d0,0.0d0)
        O2xc=(0.0d0,0.0d0)
        O2yc=(0.0d0,0.0d0)
        O2zc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbx(nPFDi,nPFDj,nPFDk),fby(nPFDi,nPFDj,nPFDk),fbz(nPFDi,nPFDj,nPFDk), &
        & Jxc(nPFDi,nPFDj,nPFDk),Jyc(nPFDi,nPFDj,nPFDk),Jzc(nPFDi,nPFDj,nPFDk),stat=ierr)
        Jxc=(0.0d0,0.0d0)
        Jyc=(0.0d0,0.0d0)
        Jzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lxc(nPFDi,nPFDj,nPFDk),Lyc(nPFDi,nPFDj,nPFDk),Lzc(nPFDi,nPFDj,nPFDk), &
        & Nxc(nPFDi,nPFDj,nPFDk),Nyc(nPFDi,nPFDj,nPFDk),Nzc(nPFDi,nPFDj,nPFDk),stat=ierr)
        Lxc=(0.0d0,0.0d0)
        Lyc=(0.0d0,0.0d0)
        Lzc=(0.0d0,0.0d0)
        Nxc=(0.0d0,0.0d0)
        Nyc=(0.0d0,0.0d0)
        Nzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcx(nPFDi,nPFDj,nPFDk),fcy(nPFDi,nPFDj,nPFDk),fcz(nPFDi,nPFDj,nPFDk),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Pxc(nPFDi,nPFDj,nPFDk),Pyc(nPFDi,nPFDj,nPFDk),Pzc(nPFDi,nPFDj,nPFDk),stat=ierr)
        Pxc=(0.0d0,0.0d0)
        Pyc=(0.0d0,0.0d0)
        Pzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdx(nPFDi,nPFDj,nPFDk),fdy(nPFDi,nPFDj,nPFDk),fdz(nPFDi,nPFDj,nPFDk), &
        & P1xc(nPFDi,nPFDj,nPFDk),P1yc(nPFDi,nPFDj,nPFDk),P1zc(nPFDi,nPFDj,nPFDk), &
        & P2xc(nPFDi,nPFDj,nPFDk),P2yc(nPFDi,nPFDj,nPFDk),P2zc(nPFDi,nPFDj,nPFDk),stat=ierr)
        P1xc=(0.0d0,0.0d0)
        P1yc=(0.0d0,0.0d0)
        P1zc=(0.0d0,0.0d0)
        P2xc=(0.0d0,0.0d0)
        P2yc=(0.0d0,0.0d0)
        P2zc=(0.0d0,0.0d0)
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocateCFD()
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
      EzcInc=(0.0d0,0.0d0) ! Incident plane wave
      HycInc=(0.0d0,0.0d0)
      Exc=(0.0d0,0.0d0) ! PFDfield
      Eyc=(0.0d0,0.0d0)
      Ezc=(0.0d0,0.0d0)
      Dxc=(0.0d0,0.0d0)
      Dyc=(0.0d0,0.0d0)
      Dzc=(0.0d0,0.0d0)
      Bxc=(0.0d0,0.0d0)
      Byc=(0.0d0,0.0d0)
      Bzc=(0.0d0,0.0d0)
      Hxc=(0.0d0,0.0d0)
      Hyc=(0.0d0,0.0d0)
      Hzc=(0.0d0,0.0d0)
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
                Hxc(i,j,k)=FldExp(4)
                call getMMPFld(i,j,k,5_4,iD(5,i,j,k))
                Hyc(i,j,k)=FldExp(5)
                call getMMPFld(i,j,k,6_4,iD(6,i,j,k))
                Hzc(i,j,k)=FldExp(6)
              end do
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                Hxc(i,j,k)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
                Hyc(i,j,k)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
                Hzc(i,j,k)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
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
          ft=timeDepPFD(t,0)
          do i=2,nPFDi
            EzcInc(i)=gi3(i)*EzcInc(i)+gi2(i)*ddt*(HycInc(i)-HycInc(i-1))/(PFDdx*Eps0)
          end do
          EzcInc(1)=ft
          if(nPFDih.gt.-1) EzcInc(nPFDi)=(0.0d0,0.0d0) ! PEC
          Ampl=max(Ampl,cdAbs(EzcInc(ia+1)))
          t=t+0.5d0*ddt
          do i=1,nPFDi-1
            HycInc(i)=fi3(i)*HycInc(i)+fi2(i)*ddt*(EzcInc(i+1)-EzcInc(i))/(PFDdx*Mue0)
          end do
          HycInc(nPFDi)=2.0d0*HycInc(nPFDi-1)-HycInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HycInc(nPFDi)=(0.0d0,0.0d0) ! PMC
        end do
        Ampl=Dsqrt(Zw0)/Max(Ampl,1.0d-100)
        EzcInc=(0.0d0,0.0d0)
        HycInc=(0.0d0,0.0d0)
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
            Exc(i,j,k)=1.0d0/(Eps0*gax(i,j,k))
            Eyc(i,j,k)=1.0d0/(Eps0*gay(i,j,k))
            Ezc(i,j,k)=1.0d0/(Eps0*gaz(i,j,k))
            Hxc(i,j,k)=1.0d0/(Mue0*fax(i,j,k))
            Hyc(i,j,k)=1.0d0/(Mue0*fay(i,j,k))
            Hzc(i,j,k)=1.0d0/(Mue0*faz(i,j,k))
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
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=2,nPFDi
            EzcInc(i)=gi3(i)*EzcInc(i)+gi2(i)*ddt*(HycInc(i)-HycInc(i-1))/(PFDdx*Eps0)
          end do
          EzcInc(1)=Ampl*ft
          if(nPFDih.gt.-1) EzcInc(nPFDi)=(0.0d0,0.0d0) ! PEC
        end if
! Dx
        do i=1,nPFDi
          ih=i-ib
          do j=1,nPFDj
            do k=1,nPFDk
              rot=(Hzc(i,j,k)-Hzc(i,j-1,k))/PFDdy-(Hyc(i,j,k)-Hyc(i,j,k-1))/PFDdz
              if(i.lt.ia) then
                iDxl(i,j,k)=iDxl(i,j,k)+rot
                rot=rot+gi1(i)*iDxl(i,j,k)
              else if(i.gt.ib) then
                iDxh(ih,j,k)=iDxh(ih,j,k)+rot
                rot=rot+gi1(i)*iDxh(ih,j,k)
              end if
              if((j.lt.ja).or.(j.gt.jb).or.(k.lt.ka).or.(k.gt.kb)) then
                Dxc(i,j,k)=gj3(j)*gk3(k)*Dxc(i,j,k)+gj2(j)*gk2(k)*ddt*rot
              else
                Dxc(i,j,k)=Dxc(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Dx, plane wave
            if((nPFDkl.gt.0).and.(nPFDkh.gt.0)) then
              do i=ial,ibl
                do j=jal,jbl
                  Dxc(i,j,kal)=Dxc(i,j,kal)+ddt*HycInc(i)/PFDdz
                  Dxc(i,j,kbl+1)=Dxc(i,j,kbl+1)-ddt*HycInc(i)/PFDdz
                end do
              end do
            else if(nPFDkl.gt.0) then
              do i=ial,ibl
                do j=jal,jbl
                  Dxc(i,j,kal)=Dxc(i,j,kal)+ddt*HycInc(i)/PFDdz
                end do
              end do
            else if(nPFDkh.gt.0) then
              do i=ial,ibl
                do j=jal,jbl
                  Dxc(i,j,kbl+1)=Dxc(i,j,kbl+1)-ddt*HycInc(i)/PFDdz
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dxc(iPFDs(l),jPFDs(l),kPFDs(l))=Dxc(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(eDom(iD(1,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dxc(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(eDom(iD(1,iPFDs(l),jPFDs(l),kPFDs(l))))*Eps0*fl
              end do
            end if
          end if
        end if
! Dy
        do i=1,nPFDi
          do j=1,nPFDj
            ih=j-jb
            do k=1,nPFDk
              rot=(Hxc(i,j,k)-Hxc(i,j,k-1))/PFDdz-(Hzc(i,j,k)-Hzc(i-1,j,k))/PFDdx
              if(j.lt.ja) then
                iDyl(i,j,k)=iDyl(i,j,k)+rot
                rot=rot+gj1(j)*iDyl(i,j,k)
              else if(j.gt.jb) then
                iDyh(i,ih,k)=iDyh(i,ih,k)+rot
                rot=rot+gj1(j)*iDyh(i,ih,k)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(k.lt.ka).or.(k.gt.kb)) then
                Dyc(i,j,k)=gk3(k)*gi3(i)*Dyc(i,j,k)+gk2(k)*gi2(i)*ddt*rot
              else
                Dyc(i,j,k)=Dyc(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dyc(iPFDs(l),jPFDs(l),kPFDs(l))=Dyc(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(eDom(iD(2,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dyc(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(eDom(iD(2,iPFDs(l),jPFDs(l),kPFDs(l))))*Eps0*fl
              end do
            end if
          end if
        end if
! Dz
        do i=1,nPFDi
          do j=1,nPFDj
            do k=1,nPFDk
              ih=k-kb
              rot=(Hyc(i,j,k)-Hyc(i-1,j,k))/PFDdx-(Hxc(i,j,k)-Hxc(i,j-1,k))/PFDdy
              if(k.lt.ka) then
                iDzl(i,j,k)=iDzl(i,j,k)+rot
                rot=rot+gk1(k)*iDzl(i,j,k)
              else if(k.gt.kb) then
                iDzh(i,j,ih)=iDzh(i,j,ih)+rot
                rot=rot+gk1(k)*iDzh(i,j,ih)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(j.lt.ja).or.(j.gt.jb)) then
                Dzc(i,j,k)=gi3(i)*gj3(j)*Dzc(i,j,k)+gi2(i)*gj2(j)*ddt*rot
              else
                Dzc(i,j,k)=Dzc(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Dz, plane wave
            if(lPFDsoft) then ! scattered field formulation
              do j=jal,jbl
                do k=kal,kbl
                  Dzc(ial,j,k)=Dzc(ial,j,k)-ddt*HycInc(ial-1)/PFDdx
                  Dzc(ibl+1,j,k)=Dzc(ibl+1,j,k)+ddt*HycInc(ibl)/PFDdx
                end do
              end do
            else ! total field formulation
              do j=j1,j2
                do k=k1,k2
                  Dzc(ia+1,j,k)=EzcInc(ia+1)*Eps0
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dzc(iPFDs(l),jPFDs(l),kPFDs(l))=Dzc(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(eDom(iD(3,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dzc(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(eDom(iD(3,iPFDs(l),jPFDs(l),kPFDs(l))))*Eps0*fl
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
                call cDB2EH(i,j,k,lPFDX,1_4,Dxc(i,j,k),Exc(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k), &
                & XYZ(i,j,k),XYZ(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k),gax(i,j,k),iD(1,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,2_4,Dyc(i,j,k),Eyc(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k), & 
                & XYZ(i,j,k),XYZ(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k),gay(i,j,k),iD(2,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,3_4,Dzc(i,j,k),Ezc(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k), &
                & XYZ(i,j,k),XYZ(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft)
              end do
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,1_4,Dxc(i,j,k),Exc(i,j,k),Ixc(i,j,k),Ixc(i,j,k),Ixc(i,j,k),Ixc(i,j,k), &
                & Ixc(i,j,k),Ixc(i,j,k),gax(i,j,k),gbx(i,j,k),gax(i,j,k),gax(i,j,k),iD(1,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,2_4,Dyc(i,j,k),Eyc(i,j,k),Iyc(i,j,k),Iyc(i,j,k),Iyc(i,j,k),Iyc(i,j,k), & 
                & Iyc(i,j,k),Iyc(i,j,k),gay(i,j,k),gby(i,j,k),gay(i,j,k),gay(i,j,k),iD(2,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,3_4,Dzc(i,j,k),Ezc(i,j,k),Izc(i,j,k),Izc(i,j,k),Izc(i,j,k),Izc(i,j,k), &
                & Izc(i,j,k),Izc(i,j,k),gaz(i,j,k),gbz(i,j,k),gaz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft)
              end do
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,1_4,Dxc(i,j,k),Exc(i,j,k),Ixc(i,j,k),Kxc(i,j,k),Mxc(i,j,k),Ixc(i,j,k), &
                & Ixc(i,j,k),Ixc(i,j,k),gax(i,j,k),gbx(i,j,k),gax(i,j,k),gax(i,j,k),iD(1,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,2_4,Dyc(i,j,k),Eyc(i,j,k),Iyc(i,j,k),Kyc(i,j,k),Myc(i,j,k),Iyc(i,j,k), & 
                & Iyc(i,j,k),Iyc(i,j,k),gay(i,j,k),gby(i,j,k),gay(i,j,k),gay(i,j,k),iD(2,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,3_4,Dzc(i,j,k),Ezc(i,j,k),Izc(i,j,k),Kzc(i,j,k),Mzc(i,j,k),Izc(i,j,k), &
                & Izc(i,j,k),Izc(i,j,k),gaz(i,j,k),gbz(i,j,k),gaz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft)
              end do
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,1_4,Dxc(i,j,k),Exc(i,j,k),Ixc(i,j,k),Kxc(i,j,k),Mxc(i,j,k),Ixc(i,j,k), &
                & Ixc(i,j,k),Ixc(i,j,k),gax(i,j,k),gbx(i,j,k),gcx(i,j,k),gax(i,j,k),iD(1,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,2_4,Dyc(i,j,k),Eyc(i,j,k),Iyc(i,j,k),Kyc(i,j,k),Myc(i,j,k),Iyc(i,j,k), & 
                & Iyc(i,j,k),Iyc(i,j,k),gay(i,j,k),gby(i,j,k),gcy(i,j,k),gay(i,j,k),iD(2,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,3_4,Dzc(i,j,k),Ezc(i,j,k),Izc(i,j,k),Kzc(i,j,k),Mzc(i,j,k),Izc(i,j,k), &
                & Izc(i,j,k),Izc(i,j,k),gaz(i,j,k),gbz(i,j,k),gcz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft)
              end do
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,1_4,Dxc(i,j,k),Exc(i,j,k),Ixc(i,j,k),Kxc(i,j,k),Mxc(i,j,k),Oxc(i,j,k), &
                & Ixc(i,j,k),Ixc(i,j,k),gax(i,j,k),gbx(i,j,k),gcx(i,j,k),gax(i,j,k),iD(1,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,2_4,Dyc(i,j,k),Eyc(i,j,k),Iyc(i,j,k),Kyc(i,j,k),Myc(i,j,k),Oyc(i,j,k), & 
                & Iyc(i,j,k),Iyc(i,j,k),gay(i,j,k),gby(i,j,k),gcy(i,j,k),gay(i,j,k),iD(2,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,3_4,Dzc(i,j,k),Ezc(i,j,k),Izc(i,j,k),Kzc(i,j,k),Mzc(i,j,k),Ozc(i,j,k), &
                & Izc(i,j,k),Izc(i,j,k),gaz(i,j,k),gbz(i,j,k),gcz(i,j,k),gaz(i,j,k),iD(3,i,j,k),ft)
              end do
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,1_4,Dxc(i,j,k),Exc(i,j,k),Ixc(i,j,k),Kxc(i,j,k),Mxc(i,j,k),Oxc(i,j,k), &
                & O1xc(i,j,k),O2xc(i,j,k),gax(i,j,k),gbx(i,j,k),gcx(i,j,k),gdx(i,j,k),iD(1,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,2_4,Dyc(i,j,k),Eyc(i,j,k),Iyc(i,j,k),Kyc(i,j,k),Myc(i,j,k),Oyc(i,j,k), & 
                & O1yc(i,j,k),O2yc(i,j,k),gay(i,j,k),gby(i,j,k),gcy(i,j,k),gdy(i,j,k),iD(2,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,3_4,Dzc(i,j,k),Ezc(i,j,k),Izc(i,j,k),Kzc(i,j,k),Mzc(i,j,k),Ozc(i,j,k), &
                & O1zc(i,j,k),O2zc(i,j,k),gaz(i,j,k),gbz(i,j,k),gcz(i,j,k),gdz(i,j,k),iD(3,i,j,k),ft)
              end do
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('3','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=1,nPFDi-1
            HycInc(i)=fi3(i)*HycInc(i)+fi2(i)*ddt*(EzcInc(i+1)-EzcInc(i))/(PFDdx*Mue0)
          end do
          HycInc(nPFDi)=2.0d0*HycInc(nPFDi-1)-HycInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HycInc(nPFDi)=(0.0d0,0.0d0) ! PMC
        end if
! Bx
        do i=1,nPFDi
          ih=i-ib
          do j=1,nPFDj
            do k=1,nPFDk
              rot=(Eyc(i,j,k+1)-Eyc(i,j,k))/PFDdz-(Ezc(i,j+1,k)-Ezc(i,j,k))/PFDdy
              if(i.lt.ia) then
                iBxl(i,j,k)=iBxl(i,j,k)+rot
                rot=rot+fi1(i)*iBxl(i,j,k)
              else if(i.gt.ib) then
                iBxh(ih,j,k)=iBxh(ih,j,k)+rot
                rot=rot+fi1(i)*iBxh(ih,j,k)
              end if
              if((j.lt.ja).or.(j.gt.jb).or.(k.lt.ka).or.(k.gt.kb)) then
                Bxc(i,j,k)=fj3(j)*fk3(k)*Bxc(i,j,k)+fj2(j)*fk2(k)*ddt*rot
              else
                Bxc(i,j,k)=Bxc(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Bx, plane wave
            if((nPFDjl.gt.0).and.(nPFDjh.gt.0)) then
              do i=ial,ibl
                do k=kal,kbl
                  Bxc(i,jal-1,k)=Bxc(i,jal-1,k)+ddt*EzcInc(i)/PFDdy
                  Bxc(i,jbl,k)=Bxc(i,jbl,k)-ddt*EzcInc(i)/PFDdy
                end do
              end do
            else if(nPFDjl.gt.0) then
              do i=ial,ibl
                do k=kal,kbl
                  Bxc(i,jal-1,k)=Bxc(i,jal-1,k)+ddt*EzcInc(i)/PFDdy
                end do
              end do
            else if(nPFDjh.gt.0) then
              do i=ial,ibl
                do k=kal,kbl
                  Bxc(i,jbl,k)=Bxc(i,jbl,k)-ddt*EzcInc(i)/PFDdy
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bxc(iPFDs(l),jPFDs(l),kPFDs(l))=Bxc(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(uDom(iD(4,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bxc(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(uDom(iD(4,iPFDs(l),jPFDs(l),kPFDs(l))))*Mue0*fl
              end do
            end if
          end if
        end if
! By
        do i=1,nPFDi
          do j=1,nPFDj
            ih=j-jb
            do k=1,nPFDk
              rot=(Ezc(i+1,j,k)-Ezc(i,j,k))/PFDdx-(Exc(i,j,k+1)-Exc(i,j,k))/PFDdz
              if(j.lt.ja) then
                iByl(i,j,k)=iByl(i,j,k)+rot
                rot=rot+fj1(j)*iByl(i,j,k)
              else if(j.gt.jb) then
                iByh(i,ih,k)=iByh(i,ih,k)+rot
                rot=rot+fj1(j)*iByh(i,ih,k)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(k.lt.ka).or.(k.gt.kb)) then
                Byc(i,j,k)=fk3(k)*fi3(i)*Byc(i,j,k)+fk2(k)*fi2(i)*ddt*rot
              else
                Byc(i,j,k)=Byc(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident By, plane wave
            if(lPFDsoft) then
              do j=jal,jbl
                do k=kal,kbl
                  Byc(ial-1,j,k)=Byc(ial-1,j,k)-ddt*EzcInc(ial)/PFDdx
                  Byc(ibl,j,k)=Byc(ibl,j,k)+ddt*EzcInc(ibl+1)/PFDdx
                end do
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*cF(1)
                end if
                Byc(iPFDs(l),jPFDs(l),kPFDs(l))=Byc(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(uDom(iD(5,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*cF(1)
                end if
                Byc(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(uDom(iD(5,iPFDs(l),jPFDs(l),kPFDs(l))))*Mue0*fl
              end do
            end if
          end if
        end if
! Bz
        do i=1,nPFDi
          do j=1,nPFDj
            do k=1,nPFDk
              ih=k-kb
              rot=(Exc(i,j+1,k)-Exc(i,j,k))/PFDdy-(Eyc(i+1,j,k)-Eyc(i,j,k))/PFDdx
              if(k.lt.ka) then
                iBzl(i,j,k)=iBzl(i,j,k)+rot
                rot=rot+fk1(k)*iBzl(i,j,k)
              else if(k.gt.kb) then
                iBzh(i,j,ih)=iBzh(i,j,ih)+rot
                rot=rot+fk1(k)*iBzh(i,j,ih)
              end if
              if((i.lt.ia).or.(i.gt.ib).or.(j.lt.ja).or.(j.gt.jb)) then
                Bzc(i,j,k)=fi3(i)*fj3(j)*Bzc(i,j,k)+fi2(i)*fj2(j)*ddt*rot
              else
                Bzc(i,j,k)=Bzc(i,j,k)+ddt*rot
              end if
            end do
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bzc(iPFDs(l),jPFDs(l),kPFDs(l))=Bzc(iPFDs(l),jPFDs(l),kPFDs(l))+Dble(uDom(iD(6,iPFDs(l),jPFDs(l),kPFDs(l)))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bzc(iPFDs(l),jPFDs(l),kPFDs(l))=Dble(uDom(iD(6,iPFDs(l),jPFDs(l),kPFDs(l))))*Mue0*fl
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
                call cDB2EH(i,j,k,lPFDX,4_4,Bxc(i,j,k),Hxc(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k), &
                & XYZ(i,j,k),XYZ(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k),fax(i,j,k),iD(4,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,5_4,Byc(i,j,k),Hyc(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k), &
                & XYZ(i,j,k),XYZ(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k),fay(i,j,k),iD(5,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,6_4,Bzc(i,j,k),Hzc(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k),XYZ(i,j,k), &
                & XYZ(i,j,k),XYZ(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft)
              end do
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,4_4,Bxc(i,j,k),Hxc(i,j,k),Jxc(i,j,k),Jxc(i,j,k),Jxc(i,j,k),Jxc(i,j,k), &
                & Jxc(i,j,k),Jxc(i,j,k),fax(i,j,k),fbx(i,j,k),fax(i,j,k),fax(i,j,k),iD(4,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,5_4,Byc(i,j,k),Hyc(i,j,k),Jyc(i,j,k),Jyc(i,j,k),Jyc(i,j,k),Jyc(i,j,k), &
                & Jyc(i,j,k),Jyc(i,j,k),fay(i,j,k),fby(i,j,k),fay(i,j,k),fay(i,j,k),iD(5,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,6_4,Bzc(i,j,k),Hzc(i,j,k),Jzc(i,j,k),Jzc(i,j,k),Jzc(i,j,k),Jzc(i,j,k), &
                & Jzc(i,j,k),Jzc(i,j,k),faz(i,j,k),fbz(i,j,k),faz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft)
              end do
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,4_4,Bxc(i,j,k),Hxc(i,j,k),Jxc(i,j,k),Lxc(i,j,k),Nxc(i,j,k),Jxc(i,j,k), &
                & Jxc(i,j,k),Jxc(i,j,k),fax(i,j,k),fbx(i,j,k),fax(i,j,k),fax(i,j,k),iD(4,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,5_4,Byc(i,j,k),Hyc(i,j,k),Jyc(i,j,k),Lyc(i,j,k),Nyc(i,j,k),Jyc(i,j,k), &
                & Jyc(i,j,k),Jyc(i,j,k),fay(i,j,k),fby(i,j,k),fay(i,j,k),fay(i,j,k),iD(5,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,6_4,Bzc(i,j,k),Hzc(i,j,k),Jzc(i,j,k),Lzc(i,j,k),Nzc(i,j,k),Jzc(i,j,k), &
                & Jzc(i,j,k),Jzc(i,j,k),faz(i,j,k),fbz(i,j,k),faz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft)
              end do
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,4_4,Bxc(i,j,k),Hxc(i,j,k),Jxc(i,j,k),Lxc(i,j,k),Nxc(i,j,k),Jxc(i,j,k), &
                & Jxc(i,j,k),Jxc(i,j,k),fax(i,j,k),fbx(i,j,k),fcx(i,j,k),fax(i,j,k),iD(4,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,5_4,Byc(i,j,k),Hyc(i,j,k),Jyc(i,j,k),Lyc(i,j,k),Nyc(i,j,k),Jyc(i,j,k), &
                & Jyc(i,j,k),Jyc(i,j,k),fay(i,j,k),fby(i,j,k),fcy(i,j,k),fay(i,j,k),iD(5,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,6_4,Bzc(i,j,k),Hzc(i,j,k),Jzc(i,j,k),Lzc(i,j,k),Nzc(i,j,k),Jzc(i,j,k), &
                & Jzc(i,j,k),Jzc(i,j,k),faz(i,j,k),fbz(i,j,k),fcz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft)
              end do
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,4_4,Bxc(i,j,k),Hxc(i,j,k),Jxc(i,j,k),Lxc(i,j,k),Nxc(i,j,k),Pxc(i,j,k), &
                & Jxc(i,j,k),Jxc(i,j,k),fax(i,j,k),fbx(i,j,k),fcx(i,j,k),fax(i,j,k),iD(4,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,5_4,Byc(i,j,k),Hyc(i,j,k),Jyc(i,j,k),Lyc(i,j,k),Nyc(i,j,k),Pyc(i,j,k), &
                & Jyc(i,j,k),Jyc(i,j,k),fay(i,j,k),fby(i,j,k),fcy(i,j,k),fay(i,j,k),iD(5,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,6_4,Bzc(i,j,k),Hzc(i,j,k),Jzc(i,j,k),Lzc(i,j,k),Nzc(i,j,k),Pzc(i,j,k), &
                & Jzc(i,j,k),Jzc(i,j,k),faz(i,j,k),fbz(i,j,k),fcz(i,j,k),faz(i,j,k),iD(6,i,j,k),ft)
              end do
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              do k=1,nPFDk
                call cDB2EH(i,j,k,lPFDX,4_4,Bxc(i,j,k),Hxc(i,j,k),Jxc(i,j,k),Lxc(i,j,k),Nxc(i,j,k),Pxc(i,j,k), &
                & P1xc(i,j,k),P2xc(i,j,k),fax(i,j,k),fbx(i,j,k),fcx(i,j,k),fdx(i,j,k),iD(4,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDY,5_4,Byc(i,j,k),Hyc(i,j,k),Jyc(i,j,k),Lyc(i,j,k),Nyc(i,j,k),Pyc(i,j,k), &
                & P1yc(i,j,k),P2yc(i,j,k),fay(i,j,k),fby(i,j,k),fcy(i,j,k),fdy(i,j,k),iD(5,i,j,k),ft)
                call cDB2EH(i,j,k,lPFDZ,6_4,Bzc(i,j,k),Hzc(i,j,k),Jzc(i,j,k),Lzc(i,j,k),Nzc(i,j,k),Pzc(i,j,k), &
                & P1zc(i,j,k),P2zc(i,j,k),faz(i,j,k),fbz(i,j,k),fcz(i,j,k),fdz(i,j,k),iD(6,i,j,k),ft)
              end do
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('3','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getCFDcFld3D(r,cF(1:6))
                if(iPFDft.gt.0) then
                  cFld(1:6,i,j,k)=cFld(1:6,i,j,k)+cF(1:6)*dconjg(expot)
                else
                  cFld(1:6,i,j,k)=cF(1:6)*dconjg(expot)
                end if
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
            call getCFDcFld3D(r,cF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              expft=cdexp((0.0d0,-2.0d0)*Pi*f*trFld)
              f=f+df
              cPFDsens(1:6,i,k)=cPFDsens(1:6,i,k)+expft*Dble(cF(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(Dble(cF(1))**2+Dble(cF(2))**2+Dble(cF(3))**2+Dimag(cF(1))**2+Dimag(cF(2))**2+Dimag(cF(3))**2)+ &
            & Mue0*(Dble(cF(4))**2+Dble(cF(5))**2+Dble(cF(6))**2+Dimag(cF(4))**2+Dimag(cF(5))**2+Dimag(cF(6))**2)
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
            call getCFDcFld3D(r,cF(1:6))
            dFld(1:6,i,j,k)=Dble(cF(1:6))
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
! get time-dependent CFD field in sensor points
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getCFDcFld3D(r(1:3),cF(1:6))
      dPFDsens(1:6,k)=Dble(cF(1:6))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformCFD3D

  Subroutine TransformCFD2E(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10),rot,expot,expft,fl
    Real(8) xn,xxn,t,ft,r(3),f,df
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
    dPFDwind=sqrt((PFDxmax-PFDxmin)**2+(PFDymax-PFDymin)**2+(PFDzmax-PFDzmin)**2)
    if(Initialize.eq.1) then
      call CFDinitPer()
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
      call DeAllocateCFD()
      Allocate(Dzc(nPFDi,nPFDj,1), &
      &        Ezc(0:nPFDi+1,0:nPFDj+1,1), &
      &        Bxc(nPFDi,nPFDj,1),Byc(nPFDi,nPFDj,1), &
      &        Hxc(0:nPFDi+1,0:nPFDj+1,1),Hyc(0:nPFDi+1,0:nPFDj+1,1), &
      & fax(nPFDi,nPFDj,1),fay(nPFDi,nPFDj,1), &
      & gaz(nPFDi,nPFDj,1), &
      & EzcInc(0:nPFDi+1),HycInc(0:nPFDi+1), &
      & gi2(nPFDi),gi3(nPFDi),gj2(nPFDj),gj3(nPFDj), &
      & fi2(nPFDi),fi3(nPFDi),fj2(nPFDj),fj3(nPFDj), &
      & iD(3,nPFDi,nPFDj,1),stat=ierr)
      if((ierr.eq.0).and.(iEtype.eq.1)) then
        Allocate(XYZ(nPFDi,nPFDj,1),stat=ierr)
        XYZ=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbz(nPFDi,nPFDj,1),Izc(nPFDi,nPFDj,1),stat=ierr)
        Izc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kzc(nPFDi,nPFDj,1),Mzc(nPFDi,nPFDj,1),stat=ierr)
        Kzc=(0.0d0,0.0d0)
        Mzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcz(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Ozc(nPFDi,nPFDj,1),stat=ierr)
        Ozc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdz(nPFDi,nPFDj,1),O1zc(nPFDi,nPFDj,1),O2zc(nPFDi,nPFDj,1),stat=ierr)
        O1zc=(0.0d0,0.0d0)
        O2zc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbx(nPFDi,nPFDj,1),fby(nPFDi,nPFDj,1),Jxc(nPFDi,nPFDj,1),Jyc(nPFDi,nPFDj,1),stat=ierr)
        Jxc=0.0d0
        Jyc=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lxc(nPFDi,nPFDj,1),Lyc(nPFDi,nPFDj,1),Nxc(nPFDi,nPFDj,1),Nyc(nPFDi,nPFDj,1),stat=ierr)
        Lxc=(0.0d0,0.0d0)
        Lyc=(0.0d0,0.0d0)
        Nxc=(0.0d0,0.0d0)
        Nyc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcx(nPFDi,nPFDj,1),fcy(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Pxc(nPFDi,nPFDj,1),Pzc(nPFDi,nPFDj,1),stat=ierr)
        Pxc=(0.0d0,0.0d0)
        Pyc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdx(nPFDi,nPFDj,1),fdy(nPFDi,nPFDj,1), &
        & P1xc(nPFDi,nPFDj,1),P1yc(nPFDi,nPFDj,1),P2xc(nPFDi,nPFDj,1),P2yc(nPFDi,nPFDj,1),stat=ierr)
        P1xc=(0.0d0,0.0d0)
        P1yc=(0.0d0,0.0d0)
        P2xc=(0.0d0,0.0d0)
        P2yc=(0.0d0,0.0d0)
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocateCFD()
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
      EzcInc=(0.0d0,0.0d0) ! Incident plane wave
      HycInc=(0.0d0,0.0d0)
      Ezc=(0.0d0,0.0d0) ! PFDfield
      Dzc=(0.0d0,0.0d0)
      Bxc=(0.0d0,0.0d0)
      Byc=(0.0d0,0.0d0)
      Hxc=(0.0d0,0.0d0)
      Hyc=(0.0d0,0.0d0)
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
              Hxc(i,j,1)=FldExp(4)
              call getMMPFld(i,j,0_4,5_4,iD(2,i,j,1))
              Hyc(i,j,1)=FldExp(5)
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              Hxc(i,j,1)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
              Hyc(i,j,1)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
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
          ft=timeDepPFD(t,0)
          do i=2,nPFDi
            EzcInc(i)=gi3(i)*EzcInc(i)+gi2(i)*ddt*(HycInc(i)-HycInc(i-1))/(PFDdx*Eps0)
          end do
          EzcInc(1)=ft
          if(nPFDih.gt.-1) EzcInc(nPFDi)=(0.0d0,0.0d0) ! PEC
          Ampl=max(Ampl,cdAbs(EzcInc(ia+1)))
          t=t+0.5d0*ddt
          do i=1,nPFDi-1
            HycInc(i)=fi3(i)*HycInc(i)+fi2(i)*ddt*(EzcInc(i+1)-EzcInc(i))/(PFDdx*Mue0)
          end do
          HycInc(nPFDi)=2.0d0*HycInc(nPFDi-1)-HycInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HycInc(nPFDi)=(0.0d0,0.0d0) ! PMC
        end do
        Ampl=Dsqrt(Zw0)/Max(Ampl,1.0d-100)
        EzcInc=(0.0d0,0.0d0)
        HycInc=(0.0d0,0.0d0)
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
          Ezc(i,j,1)=1.0d0/(Eps0*gaz(i,j,1))
          Hxc(i,j,1)=1.0d0/(Mue0*fax(i,j,1))
          Hyc(i,j,1)=1.0d0/(Mue0*fay(i,j,1))
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
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=2,nPFDi
            EzcInc(i)=gi3(i)*EzcInc(i)+gi2(i)*ddt*(HycInc(i)-HycInc(i-1))/(PFDdx*Eps0)
          end do
          EzcInc(1)=Ampl*ft
          if(nPFDih.gt.-1) EzcInc(nPFDi)=(0.0d0,0.0d0) ! PEC
        end if
! Dz
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Hyc(i,j,1)-Hyc(i-1,j,1))/PFDdx-(Hxc(i,j,1)-Hxc(i,j-1,1))/PFDdy
            Dzc(i,j,1)=gi3(i)*gj3(j)*Dzc(i,j,1)+gi2(i)*gj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Dz, plane wave
            if(lPFDsoft) then
              do j=jal,jbl
                Dzc(ial,j,1)=Dzc(ial,j,1)-ddt*HycInc(ial-1)/PFDdx
                Dzc(ibl+1,j,1)=Dzc(ibl+1,j,1)+ddt*HycInc(ibl)/PFDdx
              end do
            else ! total field formulation
              do j=j1,j2
                Dzc(ia+1,j,1)=EzcInc(ia+1)*Eps0
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dzc(iPFDs(l),jPFDs(l),1)=Dzc(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(3,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dzc(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(3,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! get E from D
        select case(iEtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Izc(i,j,1),Izc(i,j,1),Izc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Izc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Izc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Ozc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Ozc(i,j,1), &
              & O1zc(i,j,1),O2zc(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gdz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('E','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=1,nPFDi-1
            HycInc(i)=fi3(i)*HycInc(i)+fi2(i)*ddt*(EzcInc(i+1)-EzcInc(i))/(PFDdx*Mue0)
          end do
          HycInc(nPFDi)=2.0d0*HycInc(nPFDi-1)-HycInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HycInc(nPFDi)=(0.0d0,0.0d0) ! PMC
        end if
! Bx
        do i=1,nPFDi
          do j=1,nPFDj
            rot=-(Ezc(i,j+1,1)-Ezc(i,j,1))/PFDdy
            Bxc(i,j,1)=fj3(j)*Bxc(i,j,1)+fj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Bx, plane wave
            if((nPFDjl.gt.0).and.(nPFDjh.gt.0)) then
              do i=ial,ibl
                Bxc(i,jal-1,1)=Bxc(i,jal-1,1)+ddt*EzcInc(i)/PFDdy
                Bxc(i,jbl,1)=Bxc(i,jbl,1)-ddt*EzcInc(i)/PFDdy
              end do
            else if(nPFDjl.gt.0) then
              do i=ial,ibl
                Bxc(i,jal-1,1)=Bxc(i,jal-1,1)+ddt*EzcInc(i)/PFDdy
              end do
            else if(nPFDjh.gt.0) then
              do i=ial,ibl
                Bxc(i,jbl,1)=Bxc(i,jbl,1)-ddt*EzcInc(i)/PFDdy
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bxc(iPFDs(l),jPFDs(l),1)=Bxc(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(1,iPFDs(l),jPFDs(l),1))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bxc(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(1,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! By
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Ezc(i+1,j,1)-Ezc(i,j,1))/PFDdx
            Byc(i,j,1)=fi3(i)*Byc(i,j,1)+fi2(i)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident By, plane wave
            if(lPFDsoft) then
              do j=jal,jbl
                Byc(ial-1,j,1)=Byc(ial-1,j,1)-ddt*EzcInc(ial)/PFDdx
                Byc(ibl,j,1)=Byc(ibl,j,1)+ddt*EzcInc(ibl+1)/PFDdx
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*cF(1)
                end if
                Byc(iPFDs(l),jPFDs(l),1)=Byc(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(2,iPFDs(l),jPFDs(l),1))) &
                & *Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*cF(1)
                end if
                Byc(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(2,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! get H from B
        select case(iUtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Jxc(i,j,1),Jxc(i,j,1),Jxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Jyc(i,j,1),Jyc(i,j,1),Jyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Jxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Jyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Jxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Jyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Pxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Pyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Pxc(i,j,1), &
              & P1xc(i,j,1),P2xc(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fdx(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Pyc(i,j,1), &
              & P1yc(i,j,1),P2yc(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fdy(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('E','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getCFDcFld2E(r(1:2),cF(1:6))
                if(iPFDft.gt.0) then
                  cFld(1:6,i,j,k)=cFld(1:6,i,j,k)+cF(1:6)*dconjg(expot)
                else
                  cFld(1:6,i,j,k)=cF(1:6)*dconjg(expot)
                end if
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
            call getCFDcFld2E(r(1:2),cF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              expft=cdexp((0.0d0,-2.0d0)*Pi*f*trFld)
              f=f+df
              cPFDsens(1:6,i,k)=cPFDsens(1:6,i,k)+expft*Dble(cF(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(Dble(cF(1))**2+Dble(cF(2))**2+Dble(cF(3))**2+Dimag(cF(1))**2+Dimag(cF(2))**2+Dimag(cF(3))**2)+ &
            & Mue0*(Dble(cF(4))**2+Dble(cF(5))**2+Dble(cF(6))**2+Dimag(cF(4))**2+Dimag(cF(5))**2+Dimag(cF(6))**2)
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
            call getCFDcFld2E(r(1:2),cF(1:6))
            dFld(1:6,i,j,k)=Dble(cF(1:6))
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
! get time-dependent CFD field in sensor points
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getCFDcFld2E(r(1:2),cF(1:6))
      dPFDsens(1:6,k)=Dble(cF(1:6))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformCFD2E

  Subroutine TransformCFD2H(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10),rot,expot,expft,fl
    Real(8) xn,xxn,t,ft,r(3),f,df
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
    dPFDwind=sqrt((PFDxmax-PFDxmin)**2+(PFDymax-PFDymin)**2+(PFDzmax-PFDzmin)**2)
    if(Initialize.eq.1) then
      call CFDinitPer()
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
      call DeAllocateCFD()
      Allocate(Dxc(nPFDi,nPFDj,1),Dyc(nPFDi,nPFDj,1), &
      &        Exc(0:nPFDi+1,0:nPFDj+1,1),Eyc(0:nPFDi+1,0:nPFDj+1,1), &
      &        Bzc(nPFDi,nPFDj,1), &
      &        Hzc(0:nPFDi+1,0:nPFDj+1,1), &
      & faz(nPFDi,nPFDj,1), &
      & gax(nPFDi,nPFDj,1),gay(nPFDi,nPFDj,1), &
      & HzcInc(0:nPFDi+1),EycInc(0:nPFDi+1), &
      & gi2(nPFDi),gi3(nPFDi),gj2(nPFDj),gj3(nPFDj), &
      & fi2(nPFDi),fi3(nPFDi),fj2(nPFDj),fj3(nPFDj), &
      & iD(3,nPFDi,nPFDj,1),stat=ierr)
      if((ierr.eq.0).and.(iEtype.eq.1)) then
        Allocate(XYZ(nPFDi,nPFDj,1),stat=ierr)
        XYZ=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbx(nPFDi,nPFDj,1),gby(nPFDi,nPFDj,1),Ixc(nPFDi,nPFDj,1),Iyc(nPFDi,nPFDj,1),stat=ierr)
        Ixc=(0.0d0,0.0d0)
        Iyc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kxc(nPFDi,nPFDj,1),Kyc(nPFDi,nPFDj,1),Mxc(nPFDi,nPFDj,1),Myc(nPFDi,nPFDj,1),stat=ierr)
        Kxc=(0.0d0,0.0d0)
        Kyc=(0.0d0,0.0d0)
        Mxc=(0.0d0,0.0d0)
        Myc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcx(nPFDi,nPFDj,1),gcy(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Oxc(nPFDi,nPFDj,1),Oyc(nPFDi,nPFDj,1),stat=ierr)
        Oxc=(0.0d0,0.0d0)
        Oyc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdx(nPFDi,nPFDj,1),gdy(nPFDi,nPFDj,1), &
        & O1xc(nPFDi,nPFDj,1),O1yc(nPFDi,nPFDj,1),O2xc(nPFDi,nPFDj,1),O2yc(nPFDi,nPFDj,1),stat=ierr)
        O1xc=(0.0d0,0.0d0)
        O1yc=(0.0d0,0.0d0)
        O2xc=(0.0d0,0.0d0)
        O2yc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbz(nPFDi,nPFDj,1),Jzc(nPFDi,nPFDj,1),stat=ierr)
        Jzc=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lzc(nPFDi,nPFDj,1),Nzc(nPFDi,nPFDj,1),stat=ierr)
        Lzc=(0.0d0,0.0d0)
        Nzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcz(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Pzc(nPFDi,nPFDj,1),stat=ierr)
        Pzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdz(nPFDi,nPFDj,1),P1zc(nPFDi,nPFDj,1),P2zc(nPFDi,nPFDj,1),stat=ierr)
        P1zc=(0.0d0,0.0d0)
        P2zc=(0.0d0,0.0d0)
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocateCFD()
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
      HzcInc=(0.0d0,0.0d0) ! Incident plane wave
      EycInc=(0.0d0,0.0d0)
      Exc=(0.0d0,0.0d0) ! PFDfield
      Eyc=(0.0d0,0.0d0)
      Dxc=(0.0d0,0.0d0)
      Dyc=(0.0d0,0.0d0)
      Bzc=(0.0d0,0.0d0)
      Hzc=(0.0d0,0.0d0)
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
              Hzc(i,j,1)=FldExp(6)
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              Hzc(i,j,1)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
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
          ft=timeDepPFD(t,0)
          do i=1,nPFDi-1
            HzcInc(i)=fi3(i)*HzcInc(i)+fi2(i)*ddt*(EycInc(i)-EycInc(i+1))/(PFDdx*Mue0)
          end do
          HzcInc(1)=ft
          HzcInc(nPFDi)=2.0d0*HzcInc(nPFDi-1)-HzcInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HzcInc(nPFDi)=(0.0d0,0.0d0) ! PMC
          Ampl=max(Ampl,cdAbs(HzcInc(ia+1)))
          t=t+0.5d0*ddt
          do i=2,nPFDi
            EycInc(i)=gi3(i)*EycInc(i)+gi2(i)*ddt*(HzcInc(i-1)-HzcInc(i))/(PFDdx*Eps0)
          end do
          EycInc(1)=2.0d0*EycInc(2)-EycInc(3) ! extrapolate
          if(nPFDih.gt.-1) EycInc(nPFDi)=(0.0d0,0.0d0) ! PEC
        end do
        Ampl=1.0d0/(Dsqrt(Zw0)*Max(Ampl,1.0d-100))
        HzcInc=(0.0d0,0.0d0)
        EycInc=(0.0d0,0.0d0)
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
    j2=nPFDj-1
    if(nPFDjh.lt.-1) j2=nPFDj
    if(nPFDjl.gt.0) j1=1+nPFDjl
    if(nPFDjh.gt.0) j2=nPFDj-1-nPFDjh
    wmax1=0.0d0
    if(nIterPFD.lt.1) then
      PFDwmax=0.0d0
      do i=1,nPFDi
        do j=1,nPFDj
          Exc(i,j,1)=1.0d0/(Eps0*gax(i,j,1))
          Eyc(i,j,1)=1.0d0/(Eps0*gay(i,j,1))
          Hzc(i,j,1)=1.0d0/(Mue0*faz(i,j,1))
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
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=2,nPFDi
            EycInc(i)=gi3(i)*EycInc(i)+gi2(i)*ddt*(HzcInc(i-1)-HzcInc(i))/(PFDdx*Eps0)
          end do
          EycInc(1)=2.0d0*EycInc(2)-EycInc(3) ! extrapolate
          if(nPFDih.gt.-1) EycInc(nPFDi)=(0.0d0,0.0d0) ! PEC
        end if
! Dx
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Hzc(i,j,1)-Hzc(i,j-1,1))/PFDdy
            Dxc(i,j,1)=gj3(j)*Dxc(i,j,1)+gj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane.and.lPFDsoft) then ! incident Dx, plane wave
            if((nPFDjl.gt.0).and.(nPFDjh.gt.0)) then
              do i=ial,ibl-1
                Dxc(i,jal,1)=Dxc(i,jal,1)-ddt*HzcInc(i)/PFDdy
                Dxc(i,jbl,1)=Dxc(i,jbl,1)+ddt*HzcInc(i)/PFDdy
              end do
            else if(nPFDjl.gt.0) then
              do i=ial,ibl-1
                Dxc(i,jal,1)=Dxc(i,jal,1)-ddt*HzcInc(i)/PFDdy
              end do
            else if(nPFDjh.gt.0) then
              do i=ial,ibl-1
                Dxc(i,jbl,1)=Dxc(i,jbl,1)+ddt*HzcInc(i)/PFDdy
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dxc(iPFDs(l),jPFDs(l),1)=Dxc(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(1,iPFDs(l),jPFDs(l),1))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,1_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dxc(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(1,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! Dy
        do i=1,nPFDi
          do j=1,nPFDj
            rot=-(Hzc(i,j,1)-Hzc(i-1,j,1))/PFDdx
            Dyc(i,j,1)=gi3(i)*Dyc(i,j,1)+gi2(i)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Dy, plane wave
            if(lPFDsoft) then
              do j=jal,jbl-1
                Dyc(ial,j,1)=Dyc(ial,j,1)+ddt*HzcInc(ial)/PFDdx
                Dyc(ibl,j,1)=Dyc(ibl,j,1)-ddt*HzcInc(ibl)/PFDdx
              end do
            else ! total field formulation
              do j=j1,j2
                Dyc(ia+1,j,1)=EycInc(ia+1)*Eps0
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dyc(iPFDs(l),jPFDs(l),1)=Dyc(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(2,iPFDs(l),jPFDs(l),1))) &
                & *Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,2_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dyc(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(2,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! get E from D
        select case(iEtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,1_4,Dxc(i,j,1),Exc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1),gax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,2_4,Dyc(i,j,1),Eyc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1),gay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,1_4,Dxc(i,j,1),Exc(i,j,1),Ixc(i,j,1),Ixc(i,j,1),Ixc(i,j,1),Ixc(i,j,1), &
              & Ixc(i,j,1),Ixc(i,j,1),gax(i,j,1),gbx(i,j,1),gax(i,j,1),gax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,2_4,Dyc(i,j,1),Eyc(i,j,1),Iyc(i,j,1),Iyc(i,j,1),Iyc(i,j,1),Iyc(i,j,1), &
              & Iyc(i,j,1),Iyc(i,j,1),gay(i,j,1),gby(i,j,1),gay(i,j,1),gay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,1_4,Dxc(i,j,1),Exc(i,j,1),Ixc(i,j,1),Kxc(i,j,1),Mxc(i,j,1),Ixc(i,j,1), &
              & Ixc(i,j,1),Ixc(i,j,1),gax(i,j,1),gbx(i,j,1),gax(i,j,1),gax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,2_4,Dyc(i,j,1),Eyc(i,j,1),Iyc(i,j,1),Kyc(i,j,1),Myc(i,j,1),Iyc(i,j,1), &
              & Iyc(i,j,1),Iyc(i,j,1),gay(i,j,1),gby(i,j,1),gay(i,j,1),gay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,1_4,Dxc(i,j,1),Exc(i,j,1),Ixc(i,j,1),Kxc(i,j,1),Mxc(i,j,1),Ixc(i,j,1), &
              & Ixc(i,j,1),Ixc(i,j,1),gax(i,j,1),gbx(i,j,1),gcx(i,j,1),gax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,2_4,Dyc(i,j,1),Eyc(i,j,1),Iyc(i,j,1),Kyc(i,j,1),Myc(i,j,1),Iyc(i,j,1), &
              & Iyc(i,j,1),Iyc(i,j,1),gay(i,j,1),gby(i,j,1),gcy(i,j,1),gay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,1_4,Dxc(i,j,1),Exc(i,j,1),Ixc(i,j,1),Kxc(i,j,1),Mxc(i,j,1),Oxc(i,j,1), &
              & Ixc(i,j,1),Ixc(i,j,1),gax(i,j,1),gbx(i,j,1),gcx(i,j,1),gax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,2_4,Dyc(i,j,1),Eyc(i,j,1),Iyc(i,j,1),Kyc(i,j,1),Myc(i,j,1),Oyc(i,j,1), &
              & Iyc(i,j,1),Iyc(i,j,1),gay(i,j,1),gby(i,j,1),gcy(i,j,1),gay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,1_4,Dxc(i,j,1),Exc(i,j,1),Ixc(i,j,1),Kxc(i,j,1),Mxc(i,j,1),Oxc(i,j,1), &
              & O1xc(i,j,1),O2xc(i,j,1),gax(i,j,1),gbx(i,j,1),gcx(i,j,1),gdx(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,2_4,Dyc(i,j,1),Eyc(i,j,1),Iyc(i,j,1),Kyc(i,j,1),Myc(i,j,1),Oyc(i,j,1), &
              & O1yc(i,j,1),O2yc(i,j,1),gay(i,j,1),gby(i,j,1),gcy(i,j,1),gdy(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('H','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! incident plane wave, free space assumed!
        if(lPFDplane) then
          do i=1,nPFDi-1
            HzcInc(i)=fi3(i)*HzcInc(i)+fi2(i)*ddt*(EycInc(i)-EycInc(i+1))/(PFDdx*Mue0)
          end do
          HzcInc(1)=Ampl*ft
          HzcInc(nPFDi)=2.0d0*HzcInc(nPFDi-1)-HzcInc(nPFDi-2) ! extrapolate
          if(nPFDih.eq.-1) HzcInc(nPFDi)=(0.0d0,0.0d0) ! PMC
        end if
! Bz
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Exc(i,j+1,1)-Exc(i,j,1))/PFDdy-(Eyc(i+1,j,1)-Eyc(i,j,1))/PFDdx
            Bzc(i,j,1)=fi3(i)*fj3(j)*Bzc(i,j,1)+fi2(i)*fj2(j)*ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDplane) then ! incident Bz, plane wave
            if(lPFDsoft) then
              do j=jal,jbl-1
                Bzc(ial,j,1)=Bzc(ial,j,1)+ddt*EycInc(ial)/PFDdx
                Bzc(ibl,j,1)=Bzc(ibl,j,1)-ddt*EycInc(ibl)/PFDdx
              end do
            end if
          end if
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bzc(iPFDs(l),jPFDs(l),1)=Bzc(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(3,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=timeDepCFD(trFld,l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,6_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bzc(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(3,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! get H from B
        select case(iUtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,6_4,Bzc(i,j,1),Hzc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,6_4,Bzc(i,j,1),Hzc(i,j,1),Jzc(i,j,1),Jzc(i,j,1),Jzc(i,j,1),Jzc(i,j,1), &
              & Jzc(i,j,1),Jzc(i,j,1),faz(i,j,1),fbz(i,j,1),faz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,6_4,Bzc(i,j,1),Hzc(i,j,1),Jzc(i,j,1),Lzc(i,j,1),Nzc(i,j,1),Nzc(i,j,1), &
              & Nzc(i,j,1),Nzc(i,j,1),faz(i,j,1),fbz(i,j,1),faz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,6_4,Bzc(i,j,1),Hzc(i,j,1),Jzc(i,j,1),Lzc(i,j,1),Nzc(i,j,1),Jzc(i,j,1), &
              & Jzc(i,j,1),Jzc(i,j,1),faz(i,j,1),fbz(i,j,1),fcz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,6_4,Bzc(i,j,1),Hzc(i,j,1),Jzc(i,j,1),Lzc(i,j,1),Nzc(i,j,1),Pzc(i,j,1), &
              & Jzc(i,j,1),Jzc(i,j,1),faz(i,j,1),fbz(i,j,1),fcz(i,j,1),faz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,6_4,Bzc(i,j,1),Hzc(i,j,1),Jzc(i,j,1),Lzc(i,j,1),Nzc(i,j,1),Pzc(i,j,1), &
              & P1zc(i,j,1),P2zc(i,j,1),faz(i,j,1),fbz(i,j,1),fcz(i,j,1),fdz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('H','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getCFDcFld2H(r(1:2),cF(1:6))
                if(iPFDft.gt.0) then
                  cFld(1:6,i,j,k)=cFld(1:6,i,j,k)+cF(1:6)*dconjg(expot)
                else
                  cFld(1:6,i,j,k)=cF(1:6)*dconjg(expot)
                end if
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
            call getCFDcFld2H(r(1:2),cF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              expft=cdexp((0.0d0,-2.0d0)*Pi*f*trFld)
              f=f+df
              cPFDsens(1:6,i,k)=cPFDsens(1:6,i,k)+expft*Dble(cF(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(Dble(cF(1))**2+Dble(cF(2))**2+Dble(cF(3))**2+Dimag(cF(1))**2+Dimag(cF(2))**2+Dimag(cF(3))**2)+ &
            & Mue0*(Dble(cF(4))**2+Dble(cF(5))**2+Dble(cF(6))**2+Dimag(cF(4))**2+Dimag(cF(5))**2+Dimag(cF(6))**2)
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
            call getCFDcFld2H(r(1:2),cF(1:6))
            dFld(1:6,i,j,k)=Dble(cF(1:6))
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
! get time-dependent CFD field in sensor points
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getCFDcFld2H(r(1:2),cF(1:6))
      dPFDsens(1:6,k)=Dble(cF(1:6))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformCFD2H

  Subroutine TransformCFD2EperXY(Initialize)
! transform the field with standard PFD operator
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10),rot,expot,expft,fl
    Real(8) ft,r(3),f,df
	  Integer(4) i,j,k,l,n,idum,ierr,lout

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
    if((.not.lxcFld).or.(.not.lycFld)) then
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
      call CFDinitPer()
      call getPFDl(iPFDt,lPFDHfield,lPFDEfield,lPFDX,lPFDY,lPFDZ,lPFDpoint,lPFDMMP,lPFDsoft,lPFDhard,lPFDplane)
      nPFDcFld=0_4
      nPFDsFld(1:nPFDsens)=0_4
      call getPFDdomType()
! allocate memory for arrays
      call DeAllocateCFD()
      Allocate(Dzc(nPFDi,nPFDj,1), &
      &        Ezc(0:nPFDi+1,0:nPFDj+1,1), &
      &        Bxc(nPFDi,nPFDj,1),Byc(nPFDi,nPFDj,1), &
      &        Hxc(0:nPFDi+1,0:nPFDj+1,1),Hyc(0:nPFDi+1,0:nPFDj+1,1), &
      & fax(nPFDi,nPFDj,1),fay(nPFDi,nPFDj,1), &
      & gaz(nPFDi,nPFDj,1), &
      & iD(3,nPFDi,nPFDj,1),stat=ierr)
      if((ierr.eq.0).and.(iEtype.eq.1)) then
        Allocate(XYZ(nPFDi,nPFDj,1),stat=ierr)
        XYZ=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.1)) then
        Allocate(gbz(nPFDi,nPFDj,1),Izc(nPFDi,nPFDj,1),stat=ierr)
        Izc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.2)) then
        Allocate(Kzc(nPFDi,nPFDj,1),Mzc(nPFDi,nPFDj,1),stat=ierr)
        Kzc=(0.0d0,0.0d0)
        Mzc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.3)) then
        Allocate(gcz(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iEtype.gt.4)) then
        Allocate(Ozc(nPFDi,nPFDj,1),stat=ierr)
        Ozc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iEtype.gt.5)) then
        Allocate(gdz(nPFDi,nPFDj,1),O1zc(nPFDi,nPFDj,1),O2zc(nPFDi,nPFDj,1),stat=ierr)
        O1zc=(0.0d0,0.0d0)
        O2zc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.1)) then
        Allocate(fbx(nPFDi,nPFDj,1),fby(nPFDi,nPFDj,1),Jxc(nPFDi,nPFDj,1),Jyc(nPFDi,nPFDj,1),stat=ierr)
        Jxc=0.0d0
        Jyc=0.0d0
      end if
      if((ierr.eq.0).and.(iUtype.gt.2)) then
        Allocate(Lxc(nPFDi,nPFDj,1),Lyc(nPFDi,nPFDj,1),Nxc(nPFDi,nPFDj,1),Nyc(nPFDi,nPFDj,1),stat=ierr)
        Lxc=(0.0d0,0.0d0)
        Lyc=(0.0d0,0.0d0)
        Nxc=(0.0d0,0.0d0)
        Nyc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.3)) then
        Allocate(fcx(nPFDi,nPFDj,1),fcy(nPFDi,nPFDj,1),stat=ierr)
      end if
      if((ierr.eq.0).and.(iUtype.gt.4)) then
        Allocate(Pxc(nPFDi,nPFDj,1),Pzc(nPFDi,nPFDj,1),stat=ierr)
        Pxc=(0.0d0,0.0d0)
        Pyc=(0.0d0,0.0d0)
      end if
      if((ierr.eq.0).and.(iUtype.gt.5)) then
        Allocate(fdx(nPFDi,nPFDj,1),fdy(nPFDi,nPFDj,1), &
        & P1xc(nPFDi,nPFDj,1),P1yc(nPFDi,nPFDj,1),P2xc(nPFDi,nPFDj,1),P2yc(nPFDi,nPFDj,1),stat=ierr)
        P1xc=(0.0d0,0.0d0)
        P1yc=(0.0d0,0.0d0)
        P2xc=(0.0d0,0.0d0)
        P2yc=(0.0d0,0.0d0)
      end if
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'PFD transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        call DeAllocateCFD()
        return
      end if
      lPFDalloc=.true.
! initialize arrays and constants
		  call OutTxt('t2','Initialize PFD'C)
      if(lfcFld) then
        cFld=(0.0d0,0.0d0)
      else
        dFld=0.0d0
      end if
      Ezc=(0.0d0,0.0d0) ! PFDfield
      Dzc=(0.0d0,0.0d0)
      Bxc=(0.0d0,0.0d0)
      Byc=(0.0d0,0.0d0)
      Hxc=(0.0d0,0.0d0)
      Hyc=(0.0d0,0.0d0)
      call getPFDmat2E(.false.)
      if(iPFDft.eq.0) then ! initialize field
        if(lPFDMMP) then ! MMP solution
          do i=1,nPFDi
            do j=1,nPFDj
              call getMMPFld(i,j,0_4,4_4,iD(1,i,j,1))
              Hxc(i,j,1)=FldExp(4)
              call getMMPFld(i,j,0_4,5_4,iD(2,i,j,1))
              Hyc(i,j,1)=FldExp(5)
            end do
          end do
        else ! random field
          r(1)=CHrnd(-1.0d0,1.0d0,0_4)
          do i=1,nPFDi
            do j=1,nPFDj
              Hxc(i,j,1)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
              Hyc(i,j,1)=DCmplx(CHrnd(-1.0d0,1.0d0),CHrnd(-1.0d0,1.0d0))
            end do
          end do
        end if
      end if
    end if
    if(iPFDft.gt.0) then ! inverse scaling
      if(lfcFld.and.(nPFDcFld.gt.0_4)) then
        fac=Dble(nPFDcFld)/(ddt*Dble(fcFld))
        cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
      end if
      do k=1,nPFDsens
        if(nPFDsFld(k).lt.1) cycle
        fac=Dble(nPFDsFld(k))/(ddt*Dble(fcFld))
        cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
      end do
    end if
! iterate over time: main loop
    wmax1=0.0d0
    if(nIterPFD.lt.1) then ! test of material properties
      PFDwmax=0.0d0
      do i=1,nPFDi
        do j=1,nPFDj
          Ezc(i,j,1)=1.0d0/(Eps0*gaz(i,j,1))
          Hxc(i,j,1)=1.0d0/(Mue0*fax(i,j,1))
          Hyc(i,j,1)=1.0d0/(Mue0*fay(i,j,1))
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
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! Dz
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Hyc(i,j,1)-Hyc(i-1,j,1))/PFDdx-(Hxc(i,j,1)-Hxc(i,j-1,1))/PFDdy
            Dzc(i,j,1)=Dzc(i,j,1)+ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDZ) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=ft*expot*PFDsourceA(l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dzc(iPFDs(l),jPFDs(l),1)=Dzc(iPFDs(l),jPFDs(l),1)+Dble(eDom(iD(3,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=ft*expot*PFDsourceA(l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,3_4,cF(1))
                  fl=fl*cF(1)
                end if
                Dzc(iPFDs(l),jPFDs(l),1)=Dble(eDom(iD(3,iPFDs(l),jPFDs(l),1)))*Eps0*fl
              end do
            end if
          end if
        end if
! get E from D
        select case(iEtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Izc(i,j,1),Izc(i,j,1),Izc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Izc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gaz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Izc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Ozc(i,j,1), &
              & Izc(i,j,1),Izc(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gaz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDZ,3_4,Dzc(i,j,1),Ezc(i,j,1),Izc(i,j,1),Kzc(i,j,1),Mzc(i,j,1),Ozc(i,j,1), &
              & O1zc(i,j,1),O2zc(i,j,1),gaz(i,j,1),gbz(i,j,1),gcz(i,j,1),gdz(i,j,1),iD(3,i,j,1),ft)
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('E','E')
! time dependence
        trFld=trFld+0.5d0*dtrFld
        expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
        ft=timeDepPFD(trFld,-1)
! Bx
        do i=1,nPFDi
          do j=1,nPFDj
            rot=-(Ezc(i,j+1,1)-Ezc(i,j,1))/PFDdy
            Bxc(i,j,1)=Bxc(i,j,1)+ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDX) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=ft*expot*PFDsourceA(l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bxc(iPFDs(l),jPFDs(l),1)=Bxc(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(1,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=ft*expot*PFDsourceA(l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,4_4,cF(1))
                  fl=fl*cF(1)
                end if
                Bxc(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(1,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! By
        do i=1,nPFDi
          do j=1,nPFDj
            rot=(Ezc(i+1,j,1)-Ezc(i,j,1))/PFDdx
            Byc(i,j,1)=Byc(i,j,1)+ddt*rot
          end do
        end do
        if(iPFDft.ne.0) then
          if(lPFDpoint.and.lPFDY) then ! point source
            if(lPFDsoft) then ! soft
              do l=1,nPFDsource
                fl=ft*expot*PFDsourceA(l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*cF(1)
                end if
                Byc(iPFDs(l),jPFDs(l),1)=Byc(iPFDs(l),jPFDs(l),1)+Dble(uDom(iD(2,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            else ! hard
              do l=1,nPFDsource
                fl=ft*expot*PFDsourceA(l)
                if(lPFDMMP) then
                  call PFDgetMMPpoint(i,j,k,5_4,cF(1))
                  fl=fl*cF(1)
                end if
                Byc(iPFDs(l),jPFDs(l),1)=Dble(uDom(iD(2,iPFDs(l),jPFDs(l),1)))*Mue0*fl
              end do
            end if
          end if
        end if
! get H from B
        select case(iUtype)
        case(1)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1),XYZ(i,j,1), &
              & XYZ(i,j,1),XYZ(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(2)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Jxc(i,j,1),Jxc(i,j,1),Jxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Jyc(i,j,1),Jyc(i,j,1),Jyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(3)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Jxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fax(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Jyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fay(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(4)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Jxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Jyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(5)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Pxc(i,j,1), &
              & Jxc(i,j,1),Jxc(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fax(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Pyc(i,j,1), &
              & Jyc(i,j,1),Jyc(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fay(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        case(6)
          do i=1,nPFDi
            do j=1,nPFDj
              call cDB2EH(i,j,0_4,lPFDX,4_4,Bxc(i,j,1),Hxc(i,j,1),Jxc(i,j,1),Lxc(i,j,1),Nxc(i,j,1),Pxc(i,j,1), &
              & P1xc(i,j,1),P2xc(i,j,1),fax(i,j,1),fbx(i,j,1),fcx(i,j,1),fdx(i,j,1),iD(1,i,j,1),ft)
              call cDB2EH(i,j,0_4,lPFDY,5_4,Byc(i,j,1),Hyc(i,j,1),Jyc(i,j,1),Lyc(i,j,1),Nyc(i,j,1),Pyc(i,j,1), &
              & P1yc(i,j,1),P2yc(i,j,1),fay(i,j,1),fby(i,j,1),fcy(i,j,1),fdy(i,j,1),iD(2,i,j,1),ft)
            end do
          end do
        end select
! boundary conditions
        call CFDboundary('E','H')
        if(lfcFld) then
! upgrade cFld (Fourier integral)
          nPFDcFld=nPFDcFld+1_4
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                r=getcGrd(i,j,k)
                call getCFDcFld2E(r(1:2),cF(1:6))
                if(iPFDft.gt.0) then
                  cFld(1:6,i,j,k)=cFld(1:6,i,j,k)+cF(1:6)*dconjg(expot)
                else
                  cFld(1:6,i,j,k)=cF(1:6)*dconjg(expot)
                end if
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
            call getCFDcFld2E(r(1:2),cF(1:6))
            f=PFDfmin
            df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
            do i=1,nPFDf
              expft=cdexp((0.0d0,-2.0d0)*Pi*f*trFld)
              f=f+df
              cPFDsens(1:6,i,k)=cPFDsens(1:6,i,k)+expft*Dble(cF(1:6))
            end do
            if(k.gt.1) Cycle
            w=Eps0*(Dble(cF(1))**2+Dble(cF(2))**2+Dble(cF(3))**2+Dimag(cF(1))**2+Dimag(cF(2))**2+Dimag(cF(3))**2)+ &
            & Mue0*(Dble(cF(4))**2+Dble(cF(5))**2+Dble(cF(6))**2+Dimag(cF(4))**2+Dimag(cF(5))**2+Dimag(cF(6))**2)
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
    if(iPFDft.gt.0) then ! scaling
      if(lfcFld.and.(nPFDcFld.gt.0_4)) then
        fac=(ddt*Dble(fcFld))/Dble(nPFDcFld)
        cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)=fac*cFld(1:6,1:nxcFld,1:nycFld,1:nzcFld)
      end if
      do k=1,nPFDsens
        if(nPFDsFld(k).lt.1) cycle
        fac=(ddt*Dble(fcFld))/Dble(nPFDsFld(k))
        cPFDsens(1:6,1:nPFDf,k)=fac*cPFDsens(1:6,1:nPFDf,k)
      end do
    end if
! main loop terminated, compute dFld
    if(.not.lfcFld) then
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            r=getcGrd(i,j,k)
            call getCFDcFld2E(r(1:2),cF(1:6))
            dFld(1:6,i,j,k)=Dble(cF(1:6))
          end do
        end do
      end do
    end if
! get time-dependent CFD field in sensor points
    do k=1,nPFDsens
      r(1)=PFDsensX(k)
      r(2)=PFDsensY(k)
      r(3)=PFDsensZ(k)
      call getCFDcFld2E(r(1:2),cF(1:6))
      dPFDsens(1:6,k)=Dble(cF(1:6))
    end do
		call OutTxt('t1',' 'C)
		call OutTxt('n1',' 'C)
		call OutTxt('m1',' 'C)
		call OutTxt('t2','field transformed'C)
		call OutTxt('n2',' 'C)
		call OutTxt('m2',' 'C)
	end Subroutine TransformCFD2EperXY

  Subroutine cDB2EH(i,j,k,lcomp,icomp,D,E,S0,S1,S2,SD,SD1,SD2,ga,gb,gc,gd,iD,ft)
    Implicit none
    Logical lcomp
    Integer(2) iD,idD
    Integer(4) icomp,i,j,k
    Complex(8) D,E,S0,S1,S2,SD,SD1,SD2,cF
    Real(8) ga,gb,gc,gd,ft,ex,Ep0,q
    if(ga.gt.pBig) then ! PEC or PMC
      E=(0.0d0,0.0d0)
      return
    else 
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
      E=E+ft*cF
    else ! hard
      E=ft*cF
    end if
  end Subroutine cDB2EH

  Subroutine getCFDcFld3D(r,F)
! get real field vector f at location r from the PFD field using linear interpolation
    implicit none
    Complex(8) F(6)
    Real(8) r(3),x,y,z,xx,yy,zz
    Integer(4) i,j,k,i1,j1,k1,ii,ii1,jj,jj1,kk,kk1
    call getPFDix(r(1),PFDxmin,PFDdx,nPFDi,i,i1,x)
    call getPFDix(r(1)-0.5d0*PFDdx,PFDxmin,PFDdx,nPFDi,ii,ii1,xx)
    call getPFDix(r(2),PFDymin,PFDdy,nPFDj,j,j1,y)
    call getPFDix(r(2)-0.5d0*PFDdy,PFDymin,PFDdy,nPFDj,jj,jj1,yy)
    call getPFDix(r(3),PFDzmin,PFDdz,nPFDk,k,k1,z)
    call getPFDix(r(3)-0.5d0*PFDdz,PFDzmin,PFDdz,nPFDk,kk,kk1,zz)
    F(1)=interpolateCFD3D(xx,y,z,Exc(ii,j,k),Exc(ii1,j,k),Exc(ii,j1,k),Exc(ii1,j1,k), &
    &                     Exc(ii,j,k1),Exc(ii1,j,k1),Exc(ii,j1,k1),Exc(ii1,j1,k1))
    F(2)=interpolateCFD3D(x,yy,z,Eyc(i,jj,k),Eyc(i1,jj,k),Eyc(i,jj1,k),Eyc(i1,jj1,k), &
    &                     Eyc(i,jj,k1),Eyc(i1,jj,k1),Eyc(i,jj1,k1),Eyc(i1,jj1,k1))
    F(3)=interpolateCFD3D(x,y,zz,Ezc(i,j,kk),Ezc(i1,j,kk),Ezc(i,j1,kk),Ezc(i1,j1,kk), &
    &                     Ezc(i,j,kk1),Ezc(i1,j,kk1),Ezc(i,j1,kk1),Ezc(i1,j1,kk1))
    F(4)=interpolateCFD3D(x,yy,zz,Hxc(i,jj,kk),Hxc(i1,jj,kk),Hxc(i,jj1,kk),Hxc(i1,jj1,kk), &
    &                     Hxc(i,jj,kk1),Hxc(i1,jj,kk1),Hxc(i,jj1,kk1),Hxc(i1,jj1,kk1))
    F(5)=interpolateCFD3D(xx,y,zz,Hyc(ii,j,kk),Hyc(ii1,j,kk),Hyc(ii,j1,kk),Hyc(ii1,j1,kk), &
    &                     Hyc(ii,j,kk1),Hyc(ii1,j,kk1),Hyc(ii,j1,kk1),Hyc(ii1,j1,kk1))
    F(6)=interpolateCFD3D(xx,yy,z,Hzc(ii,jj,k),Hzc(ii1,jj,k),Hzc(ii,jj1,k),Hzc(ii1,jj1,k), &
    &                     Hzc(ii,jj,k1),Hzc(ii1,jj,k1),Hzc(ii,jj1,k1),Hzc(ii1,jj1,k1))
  end Subroutine getCFDcFld3D

  Subroutine getCFDcFld2E(r,F)
! get real field vector f at location r from the PFD field using linear interpolation
    implicit none
    Complex(8) F(6)
    Real(8) r(2),x,y,xx,yy
    Integer(4) i,j,i1,j1,ii,ii1,jj,jj1
    call getPFDix(r(1),PFDxmin,PFDdx,nPFDi,i,i1,x)
    call getPFDix(r(1)-0.5d0*PFDdx,PFDxmin,PFDdx,nPFDi,ii,ii1,xx)
    call getPFDix(r(2),PFDymin,PFDdy,nPFDj,j,j1,y)
    call getPFDix(r(2)-0.5d0*PFDdy,PFDymin,PFDdy,nPFDj,jj,jj1,yy)
    F(1)=(0.0d0,0.0d0)
    F(2)=(0.0d0,0.0d0)
    F(3)=interpolateCFD2D(x,y,Ezc(i,j,1),Ezc(i1,j,1),Ezc(i,j1,1),Ezc(i1,j1,1))
    F(4)=interpolateCFD2D(x,yy,Hxc(i,jj,1),Hxc(i1,jj,1),Hxc(i,jj1,1),Hxc(i1,jj1,1))
    F(5)=interpolateCFD2D(xx,y,Hyc(ii,j,1),Hyc(ii1,j,1),Hyc(ii,j1,1),Hyc(ii1,j1,1))
    F(6)=(0.0d0,0.0d0)
  end Subroutine getCFDcFld2E

  Subroutine getCFDcFld2H(r,F)
! get real field vector f at location r from the PFD field using linear interpolation
    Implicit none
    Complex(8) F(6)
    Real(8) r(2),x,y,xx,yy
    Integer(4) i,j,i1,j1,ii,ii1,jj,jj1
    call getPFDix(r(1),PFDxmin,PFDdx,nPFDi,i,i1,x)
    call getPFDix(r(1)-0.5d0*PFDdx,PFDxmin,PFDdx,nPFDi,ii,ii1,xx)
    call getPFDix(r(2),PFDymin,PFDdy,nPFDj,j,j1,y)
    call getPFDix(r(2)-0.5d0*PFDdy,PFDymin,PFDdy,nPFDj,jj,jj1,yy)
    F(1)=interpolateCFD2D(xx,y,Exc(ii,j,1),Exc(ii1,j,1),Exc(ii,j1,1),Exc(ii1,j1,1))
    F(2)=interpolateCFD2D(x,yy,Eyc(i,jj,1),Eyc(i1,jj,1),Eyc(i,jj1,1),Eyc(i1,jj1,1))
    F(3)=(0.0d0,0.0d0)
    F(4)=(0.0d0,0.0d0)
    F(5)=(0.0d0,0.0d0)
    F(6)=interpolateCFD2D(xx,yy,Hzc(ii,jj,1),Hzc(ii1,jj,1),Hzc(ii,jj1,1),Hzc(ii1,jj1,1))
  end Subroutine getCFDcFld2H

  Complex(8) Function InterpolateCFD2D(x,y,f00,f10,f01,f11)
! use linear interpolation for a rectangular cell with corner values f00,f10,f01,f11
! x,y are scaled local coordinates (x=0,y=0 at lower left corner, x=1,y=1 at upper right corner)
    Implicit none
    Real(8) x,y
    Complex(8) f00,f10,f01,f11,f0,f1
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
      InterpolateCFD2D=f0
    else if(y.ge.1.0d0) then
      InterpolateCFD2D=f1
    else
      InterpolateCFD2D=(1.0d0-y)*f0+y*f1
    end if
  end Function InterpolateCFD2D

  Complex(8) Function interpolateCFD3D(x,y,z,f000,f100,f010,f110,f001,f101,f011,f111)
! use linear interpolation for a rectangular box
    Implicit none
    Real(8) x,y,z
    Complex(8) f000,f100,f010,f110,f001,f101,f011,f111,f0,f1
    if(z.le.0.0d0) then
      f0=interpolateCFD2D(x,y,f000,f100,f010,f110)
      interpolateCFD3D=f0
    else if(z.ge.1.0d0) then
      f1=interpolateCFD2D(x,y,f001,f101,f011,f111)
      interpolateCFD3D=f1
    else
      f0=interpolateCFD2D(x,y,f000,f100,f010,f110)
      f1=interpolateCFD2D(x,y,f001,f101,f011,f111)
      interpolateCFD3D=(1.0d0-z)*f0+z*f1
    end if
  end Function interpolateCFD3D

  Subroutine CFDboundary(Typ,Field)
    Implicit none
    Character(1) Typ,Field
    if(Typ.eq.'E') then ! 2DE
      if(Field.eq.'E') then ! E field
        if(nPFDil.lt.-1) then ! periodic
          Ezc(0:1,0:nPFDj+1,1)=Ezc(nPFDi-1:nPFDi,0:nPFDj+1,1)*conjg(CFDperX)
        else if(nPFDil.eq.0) then ! PEC
          Ezc(0,0:nPFDj+1,1)=-Ezc(2,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Ezc(0,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDil.eq.-1) then ! PMC
          Ezc(0,0:nPFDj+1,1)=Ezc(2,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Ezc(nPFDi:nPFDi+1,0:nPFDj+1,1)=Ezc(1:2,0:nPFDj+1,1)*CFDperX
        else if(nPFDih.eq.0) then ! PEC
          Ezc(nPFDi+1,0:nPFDj+1,1)=-Ezc(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Ezc(nPFDi+1,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDih.eq.-1) then ! PMC
          Ezc(nPFDi+1,0:nPFDj+1,1)=Ezc(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Ezc(0:nPFDi+1,0:1,1)=Ezc(0:nPFDi+1,nPFDj-1:nPFDj,1)*conjg(CFDperY)
        else if(nPFDjl.eq.0) then ! PEC
          Ezc(0:nPFDi+1,0,1)=-Ezc(0:nPFDi+1,2,1)
        else if(nPFDjl.gt.0) then ! UPML
          Ezc(0:nPFDi+1,0,1)=(0.0d0,0.0d0)
        else if(nPFDjl.eq.-1) then ! PMC
          Ezc(0:nPFDi+1,0,1)=Ezc(0:nPFDi+1,2,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Ezc(0:nPFDi+1,nPFDj:nPFDj+1,1)=Ezc(0:nPFDi+1,1:2,1)*CFDperY
        else if(nPFDjh.eq.0) then ! PEC
          Ezc(0:nPFDi+1,nPFDj+1,1)=-Ezc(0:nPFDi+1,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Ezc(0:nPFDi+1,nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDjh.eq.-1) then ! PMC
          Ezc(0:nPFDi+1,nPFDj+1,1)=Ezc(0:nPFDi+1,nPFDj-1,1)
        end if
      else ! H field
        if(nPFDil.lt.-1) then ! periodic
          Hxc(0:1,0:nPFDj+1,1)=Hxc(nPFDi-1:nPFDi,0:nPFDj+1,1)*conjg(CFDperX)
          Hyc(0:1,0:nPFDj+1,1)=Hyc(nPFDi-1:nPFDi,0:nPFDj+1,1)*conjg(CFDperX)
        else if(nPFDil.eq.0) then ! PEC
          Hxc(0,0:nPFDj+1,1)=-Hxc(2,0:nPFDj+1,1)
          Hyc(0,0:nPFDj+1,1)=Hyc(1,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Hxc(0,0:nPFDj+1,1)=(0.0d0,0.0d0)
          Hyc(0,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDil.eq.-1) then ! PMC
          Hxc(0,0:nPFDj+1,1)=Hxc(2,0:nPFDj+1,1)
          Hyc(0,0:nPFDj+1,1)=-Hyc(1,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Hxc(nPFDi:nPFDi+1,0:nPFDj+1,1)=Hxc(1:2,0:nPFDj+1,1)*CFDperX
          Hyc(nPFDi:nPFDi+1,0:nPFDj+1,1)=Hyc(1:2,0:nPFDj+1,1)*CFDperX
        else if(nPFDih.eq.0) then ! PEC
          Hxc(nPFDi+1,0:nPFDj+1,1)=-Hxc(nPFDi-1,0:nPFDj+1,1)
          Hyc(nPFDi,0:nPFDj+1,1)=Hyc(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Hxc(nPFDi+1,0:nPFDj+1,1)=(0.0d0,0.0d0)
          Hyc(nPFDi+1,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDih.eq.-1) then ! PMC
          Hxc(nPFDi+1,0:nPFDj+1,1)=Hxc(nPFDi-1,0:nPFDj+1,1)
          Hyc(nPFDi,0:nPFDj+1,1)=-Hyc(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Hxc(0:nPFDi+1,0:1,1)=Hxc(0:nPFDi+1,nPFDj-1:nPFDj,1)*conjg(CFDperY)
          Hyc(0:nPFDi+1,0:1,1)=Hyc(0:nPFDi+1,nPFDj-1:nPFDj,1)*conjg(CFDperY)
        else if(nPFDjl.eq.0) then ! PEC
          Hxc(0:nPFDi+1,0,1)=Hxc(0:nPFDi+1,1,1)
          Hyc(0:nPFDi+1,0,1)=-Hyc(0:nPFDi+1,2,1)
        else if(nPFDjl.gt.0) then ! UPML
          Hxc(0:nPFDi+1,0,1)=(0.0d0,0.0d0)
          Hyc(0:nPFDi+1,0,1)=(0.0d0,0.0d0)
        else if(nPFDjl.eq.-1) then ! PMC
          Hxc(0:nPFDi+1,0,1)=-Hxc(0:nPFDi+1,1,1)
          Hyc(0:nPFDi+1,0,1)=Hyc(0:nPFDi+1,2,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Hxc(0:nPFDi+1,nPFDj:nPFDj+1,1)=Hxc(0:nPFDi+1,1:2,1)*CFDperY
          Hyc(0:nPFDi+1,nPFDj:nPFDj+1,1)=Hyc(0:nPFDi+1,1:2,1)*CFDperY
        else if(nPFDjh.eq.0) then ! PEC
          Hxc(0:nPFDi+1,nPFDj,1)=Hxc(0:nPFDi+1,nPFDj-1,1)
          Hyc(0:nPFDi+1,nPFDj+1,1)=-Hyc(0:nPFDi+1,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Hxc(0:nPFDi+1,nPFDj+1,1)=(0.0d0,0.0d0)
          Hyc(0:nPFDi+1,nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDjh.eq.-1) then ! PMC
          Hxc(0:nPFDi+1,nPFDj,1)=-Hxc(0:nPFDi+1,nPFDj-1,1)
          Hyc(0:nPFDi+1,nPFDj+1,1)=Hyc(0:nPFDi+1,nPFDj-1,1)
        end if
      end if
    else if(Typ.eq.'H') then ! 2DH
      if(Field.eq.'E') then ! E field
        if(nPFDil.lt.-1) then ! periodic
          Exc(0:1,0:nPFDj+1,1)=Exc(nPFDi-1:nPFDi,0:nPFDj+1,1)*conjg(CFDperX)
          Eyc(0:1,0:nPFDj+1,1)=Eyc(nPFDi-1:nPFDi,0:nPFDj+1,1)*conjg(CFDperX)
        else if(nPFDil.eq.0) then ! PEC
          Exc(0,0:nPFDj+1,1)=Exc(1,0:nPFDj+1,1)
          Eyc(0,0:nPFDj+1,1)=-Eyc(2,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Exc(0,0:nPFDj+1,1)=(0.0d0,0.0d0)
          Eyc(0,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDil.eq.-1) then ! PMC
          Exc(0,0:nPFDj+1,1)=-Exc(1,0:nPFDj+1,1)
          Eyc(0,0:nPFDj+1,1)=Eyc(2,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Exc(nPFDi:nPFDi+1,0:nPFDj+1,1)=Exc(1:2,0:nPFDj+1,1)*CFDperX
          Eyc(nPFDi:nPFDi+1,0:nPFDj+1,1)=Eyc(1:2,0:nPFDj+1,1)*CFDperX
        else if(nPFDih.eq.0) then ! PEC
          Exc(nPFDi,0:nPFDj+1,1)=Exc(nPFDi-1,0:nPFDj+1,1)
          Eyc(nPFDi+1,0:nPFDj+1,1)=-Eyc(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Exc(nPFDi+1,0:nPFDj+1,1)=(0.0d0,0.0d0)
          Eyc(nPFDi+1,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDih.eq.-1) then ! PMC
          Exc(nPFDi,0:nPFDj+1,1)=-Exc(nPFDi-1,0:nPFDj+1,1)
          Eyc(nPFDi+1,0:nPFDj+1,1)=Eyc(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Exc(0:nPFDi+1,0:1,1)=Exc(0:nPFDi+1,nPFDj-1:nPFDj,1)*conjg(CFDperY)
          Eyc(0:nPFDi+1,0:1,1)=Eyc(0:nPFDi+1,nPFDj-1:nPFDj,1)*conjg(CFDperY)
        else if(nPFDjl.eq.0) then ! PEC
          Exc(0:nPFDi+1,0,1)=-Exc(0:nPFDi+1,2,1)
          Eyc(0:nPFDi+1,0,1)=Eyc(0:nPFDi+1,1,1)
        else if(nPFDjl.gt.0) then ! UPML
          Exc(0:nPFDi+1,0,1)=(0.0d0,0.0d0)
          Eyc(0:nPFDi+1,0,1)=(0.0d0,0.0d0)
        else if(nPFDjl.eq.-1) then ! PMC
          Exc(0:nPFDi+1,0,1)=Exc(0:nPFDi+1,2,1)
          Eyc(0:nPFDi+1,0,1)=-Eyc(0:nPFDi+1,1,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Exc(0:nPFDi+1,nPFDj:nPFDj+1,1)=Exc(0:nPFDi+1,1:2,1)*CFDperY
          Eyc(0:nPFDi+1,nPFDj:nPFDj+1,1)=Eyc(0:nPFDi+1,1:2,1)*CFDperY
        else if(nPFDjh.eq.0) then ! PEC
          Exc(0:nPFDi+1,nPFDj+1,1)=-Exc(0:nPFDi+1,nPFDj-1,1)
          Eyc(0:nPFDi+1,nPFDj,1)=Eyc(0:nPFDi+1,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Exc(0:nPFDi+1,nPFDj+1,1)=(0.0d0,0.0d0)
          Eyc(0:nPFDi+1,nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDjh.eq.-1) then ! PMC
          Exc(0:nPFDi+1,nPFDj+1,1)=Exc(0:nPFDi+1,nPFDj-1,1)
          Eyc(0:nPFDi+1,nPFDj,1)=-Eyc(0:nPFDi+1,nPFDj-1,1)
        end if
      else ! H field
        if(nPFDil.lt.-1) then ! periodic
          Hzc(0:1,0:nPFDj+1,1)=Hzc(nPFDi-1:nPFDi,0:nPFDj+1,1)*conjg(CFDperX)
        else if(nPFDil.eq.0) then ! PEC
          Hzc(0,0:nPFDj+1,1)=Hzc(1,0:nPFDj+1,1)
        else if(nPFDil.gt.0) then ! UPML
          Hzc(0,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDil.eq.-1) then ! PMC
          Hzc(0,0:nPFDj+1,1)=-Hzc(1,0:nPFDj+1,1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Hzc(nPFDi:nPFDi+1,0:nPFDj+1,1)=Hzc(1:2,0:nPFDj+1,1)*CFDperX
        else if(nPFDih.eq.0) then ! PEC
          Hzc(nPFDi,0:nPFDj+1,1)=Hzc(nPFDi-1,0:nPFDj+1,1)
        else if(nPFDih.gt.0) then ! UPML
          Hzc(nPFDi+1,0:nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDih.eq.-1) then ! PMC
          Hzc(nPFDi,0:nPFDj+1,1)=-Hzc(nPFDi-1,0:nPFDj+1,1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Hzc(0:nPFDi+1,0:1,1)=Hzc(0:nPFDi+1,nPFDj-1:nPFDj,1)*conjg(CFDperY)
        else if(nPFDjl.eq.0) then ! PEC
          Hzc(0:nPFDi+1,0,1)=Hzc(0:nPFDi+1,1,1)
        else if(nPFDjl.gt.0) then ! UPML
          Hzc(0:nPFDi+1,0,1)=(0.0d0,0.0d0)
        else if(nPFDjl.eq.-1) then ! PMC
          Hzc(0:nPFDi+1,0,1)=-Hzc(0:nPFDi+1,1,1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Hzc(0:nPFDi+1,nPFDj:nPFDj+1,1)=Hzc(0:nPFDi+1,1:2,1)*CFDperY
        else if(nPFDjh.eq.0) then ! PEC
          Hzc(0:nPFDi,nPFDj,1)=Hzc(0:nPFDi,nPFDj-1,1)
        else if(nPFDjh.gt.0) then ! UPML
          Hzc(0:nPFDi,nPFDj+1,1)=(0.0d0,0.0d0)
        else if(nPFDjh.eq.-1) then ! PMC
          Hzc(0:nPFDi,nPFDj,1)=-Hzc(0:nPFDi,nPFDj-1,1)
        end if
      end if
    else ! 3D
      if(Field.eq.'E') then ! E field
        if(nPFDil.lt.-1) then ! periodic
          Exc(0:1,0:nPFDj+1,0:nPFDk+1)=Exc(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)*conjg(CFDperX)
          Eyc(0:1,0:nPFDj+1,0:nPFDk+1)=Eyc(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)*conjg(CFDperX)
          Ezc(0:1,0:nPFDj+1,0:nPFDk+1)=Ezc(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)*conjg(CFDperX)
        else if(nPFDil.eq.0) then ! PEC
          Exc(0,0:nPFDj+1,0:nPFDk+1)=Exc(1,0:nPFDj+1,0:nPFDk+1)
          Eyc(0,0:nPFDj+1,0:nPFDk+1)=-Eyc(2,0:nPFDj+1,0:nPFDk+1)
          Ezc(0,0:nPFDj+1,0:nPFDk+1)=-Ezc(2,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDil.gt.0) then ! UPML
          Exc(0,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Eyc(0,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Ezc(0,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDil.eq.-1) then ! PMC
          Exc(0,0:nPFDj+1,0:nPFDk+1)=-Exc(1,0:nPFDj+1,0:nPFDk+1)
          Eyc(0,0:nPFDj+1,0:nPFDk+1)=Eyc(2,0:nPFDj+1,0:nPFDk+1)
          Ezc(0,0:nPFDj+1,0:nPFDk+1)=Ezc(2,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Exc(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Exc(1:2,0:nPFDj+1,0:nPFDk+1)*CFDperX
          Eyc(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Eyc(1:2,0:nPFDj+1,0:nPFDk+1)*CFDperX
          Ezc(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ezc(1:2,0:nPFDj+1,0:nPFDk+1)*CFDperX
        else if(nPFDih.eq.0) then ! PEC
          Exc(nPFDi,0:nPFDj+1,0:nPFDk+1)=Exc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Eyc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=-Eyc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Ezc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=-Ezc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDih.gt.0) then ! UPML
          Exc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Eyc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Ezc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDih.eq.-1) then ! PMC
          Exc(nPFDi,0:nPFDj+1,0:nPFDk+1)=-Exc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Eyc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Eyc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Ezc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Ezc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Exc(0:nPFDi+1,0:1,0:nPFDk+1)=Exc(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)*conjg(CFDperY)
          Eyc(0:nPFDi+1,0:1,0:nPFDk+1)=Eyc(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)*conjg(CFDperY)
          Ezc(0:nPFDi+1,0:1,0:nPFDk+1)=Ezc(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)*conjg(CFDperY)
        else if(nPFDjl.eq.0) then ! PEC
          Exc(0:nPFDi+1,0,0:nPFDk+1)=-Exc(0:nPFDi+1,2,0:nPFDk+1)
          Eyc(0:nPFDi+1,0,0:nPFDk+1)=Eyc(0:nPFDi+1,1,0:nPFDk+1)
          Ezc(0:nPFDi+1,0,0:nPFDk+1)=-Ezc(0:nPFDi+1,2,0:nPFDk+1)
        else if(nPFDjl.gt.0) then ! UPML
          Exc(0:nPFDi+1,0,0:nPFDk+1)=(0.0d0,0.0d0)
          Eyc(0:nPFDi+1,0,0:nPFDk+1)=(0.0d0,0.0d0)
          Ezc(0:nPFDi+1,0,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDjl.eq.-1) then ! PMC
          Exc(0:nPFDi+1,0,0:nPFDk+1)=Exc(0:nPFDi+1,2,0:nPFDk+1)
          Eyc(0:nPFDi+1,0,0:nPFDk+1)=-Eyc(0:nPFDi+1,1,0:nPFDk+1)
          Ezc(0:nPFDi+1,0,0:nPFDk+1)=Ezc(0:nPFDi+1,2,0:nPFDk+1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Exc(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Exc(0:nPFDi+1,1:2,0:nPFDk+1)*CFDperY
          Eyc(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Eyc(0:nPFDi+1,1:2,0:nPFDk+1)*CFDperY
          Ezc(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Ezc(0:nPFDi+1,1:2,0:nPFDk+1)*CFDperY
        else if(nPFDjh.eq.0) then ! PEC
          Exc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=-Exc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Eyc(0:nPFDi+1,nPFDj,0:nPFDk+1)=Eyc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Ezc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=-Ezc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        else if(nPFDjh.gt.0) then ! UPML
          Exc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Eyc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Ezc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDjh.eq.-1) then ! PMC
          Exc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=Exc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Eyc(0:nPFDi+1,nPFDj,0:nPFDk+1)=-Eyc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Ezc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=Ezc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        end if
        if(nPFDkl.lt.-1) then ! periodic
          Exc(0:nPFDi+1,0:nPFDj+1,0:1)=Exc(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)*conjg(CFDperZ)
          Eyc(0:nPFDi+1,0:nPFDj+1,0:1)=Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)*conjg(CFDperZ)
          Ezc(0:nPFDi+1,0:nPFDj+1,0:1)=Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)*conjg(CFDperZ)
        else if(nPFDkl.eq.0) then ! PEC
          Exc(0:nPFDi+1,0:nPFDj+1,0)=-Exc(0:nPFDi+1,0:nPFDj+1,2)
          Eyc(0:nPFDi+1,0:nPFDj+1,0)=-Eyc(0:nPFDi+1,0:nPFDj+1,2)
          Ezc(0:nPFDi+1,0:nPFDj+1,0)=Ezc(0:nPFDi+1,0:nPFDj+1,1)
        else if(nPFDkl.gt.0) then ! UPML
          Exc(0:nPFDi+1,0:nPFDj+1,0)=(0.0d0,0.0d0)
          Eyc(0:nPFDi+1,0:nPFDj+1,0)=(0.0d0,0.0d0)
          Ezc(0:nPFDi+1,0:nPFDj+1,0)=(0.0d0,0.0d0)
        else if(nPFDkl.eq.-1) then ! PMC
          Exc(0:nPFDi+1,0:nPFDj+1,0)=Exc(0:nPFDi+1,0:nPFDj+1,2)
          Eyc(0:nPFDi+1,0:nPFDj+1,0)=Eyc(0:nPFDi+1,0:nPFDj+1,2)
          Ezc(0:nPFDi+1,0:nPFDj+1,0)=-Ezc(0:nPFDi+1,0:nPFDj+1,1)
        end if
        if(nPFDkh.lt.-1) then ! periodic
          Exc(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Exc(0:nPFDi+1,0:nPFDj+1,1:2)*CFDperZ
          Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Eyc(0:nPFDi+1,0:nPFDj+1,1:2)*CFDperZ
          Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Ezc(0:nPFDi+1,0:nPFDj+1,1:2)*CFDperZ
        else if(nPFDkh.eq.0) then ! PEC
          Exc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=-Exc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=-Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk)=Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        else if(nPFDkh.gt.0) then ! UPML
          Exc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=(0.0d0,0.0d0)
          Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=(0.0d0,0.0d0)
          Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDkh.eq.-1) then ! PMC
          Exc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=Exc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=Eyc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk)=-Ezc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        end if
      else ! H field
        if(nPFDil.lt.-1) then ! periodic
          Hxc(0:1,0:nPFDj+1,0:nPFDk+1)=Hxc(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)*conjg(CFDperX)
          Hyc(0:1,0:nPFDj+1,0:nPFDk+1)=Hyc(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)*conjg(CFDperX)
          Hzc(0:1,0:nPFDj+1,0:nPFDk+1)=Hzc(nPFDi-1:nPFDi,0:nPFDj+1,0:nPFDk+1)*conjg(CFDperX)
        else if(nPFDil.eq.0) then ! PEC
          Hxc(0,0:nPFDj+1,0:nPFDk+1)=-Hxc(2,0:nPFDj+1,0:nPFDk+1)
          Hyc(0,0:nPFDj+1,0:nPFDk+1)=Hyc(1,0:nPFDj+1,0:nPFDk+1)
          Hzc(0,0:nPFDj+1,0:nPFDk+1)=Hzc(1,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDil.gt.0) then ! UPML
          Hxc(0,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Hyc(0,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Hzc(0,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDil.eq.-1) then ! PMC
          Hxc(0,0:nPFDj+1,0:nPFDk+1)=Hxc(2,0:nPFDj+1,0:nPFDk+1)
          Hyc(0,0:nPFDj+1,0:nPFDk+1)=-Hyc(1,0:nPFDj+1,0:nPFDk+1)
          Hzc(0,0:nPFDj+1,0:nPFDk+1)=-Hzc(1,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDih.lt.-1) then ! periodic
          Hxc(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hxc(1:2,0:nPFDj+1,0:nPFDk+1)*CFDperX
          Hyc(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hyc(1:2,0:nPFDj+1,0:nPFDk+1)*CFDperX
          Hzc(nPFDi:nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hzc(1:2,0:nPFDj+1,0:nPFDk+1)*CFDperX
        else if(nPFDih.eq.0) then ! PEC
          Hxc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=-Hxc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hyc(nPFDi,0:nPFDj+1,0:nPFDk+1)=Hyc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hzc(nPFDi,0:nPFDj+1,0:nPFDk+1)=Hzc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        else if(nPFDih.gt.0) then ! UPML
          Hxc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Hyc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Hzc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDih.eq.-1) then ! PMC
          Hxc(nPFDi+1,0:nPFDj+1,0:nPFDk+1)=Hxc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hyc(nPFDi,0:nPFDj+1,0:nPFDk+1)=-Hyc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
          Hzc(nPFDi,0:nPFDj+1,0:nPFDk+1)=-Hzc(nPFDi-1,0:nPFDj+1,0:nPFDk+1)
        end if
        if(nPFDjl.lt.-1) then ! periodic
          Hxc(0:nPFDi+1,0:1,0:nPFDk+1)=Hxc(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)*conjg(CFDperY)
          Hyc(0:nPFDi+1,0:1,0:nPFDk+1)=Hyc(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)*conjg(CFDperY)
          Hzc(0:nPFDi+1,0:1,0:nPFDk+1)=Hzc(0:nPFDi+1,nPFDj-1:nPFDj,0:nPFDk+1)*conjg(CFDperY)
        else if(nPFDjl.eq.0) then ! PEC
          Hxc(0:nPFDi+1,0,0:nPFDk+1)=Hxc(0:nPFDi+1,1,0:nPFDk+1)
          Hyc(0:nPFDi+1,0,0:nPFDk+1)=-Hyc(0:nPFDi+1,2,0:nPFDk+1)
          Hzc(0:nPFDi+1,0,0:nPFDk+1)=Hzc(0:nPFDi+1,1,0:nPFDk+1)
        else if(nPFDjl.gt.0) then ! UPML
          Hxc(0:nPFDi+1,0,0:nPFDk+1)=(0.0d0,0.0d0)
          Hyc(0:nPFDi+1,0,0:nPFDk+1)=(0.0d0,0.0d0)
          Hzc(0:nPFDi+1,0,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDjl.eq.-1) then ! PMC
          Hxc(0:nPFDi+1,0,0:nPFDk+1)=-Hxc(0:nPFDi+1,1,0:nPFDk+1)
          Hyc(0:nPFDi+1,0,0:nPFDk+1)=Hyc(0:nPFDi+1,2,0:nPFDk+1)
          Hzc(0:nPFDi+1,0,0:nPFDk+1)=-Hzc(0:nPFDi+1,1,0:nPFDk+1)
        end if
        if(nPFDjh.lt.-1) then ! periodic
          Hxc(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Hxc(0:nPFDi+1,1:2,0:nPFDk+1)*CFDperY
          Hyc(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Hyc(0:nPFDi+1,1:2,0:nPFDk+1)*CFDperY
          Hzc(0:nPFDi+1,nPFDj:nPFDj+1,0:nPFDk+1)=Hzc(0:nPFDi+1,1:2,0:nPFDk+1)*CFDperY
        else if(nPFDjh.eq.0) then ! PEC
          Hxc(0:nPFDi+1,nPFDj,0:nPFDk+1)=Hxc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hyc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=-Hyc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hzc(0:nPFDi+1,nPFDj,0:nPFDk+1)=Hzc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        else if(nPFDjh.gt.0) then ! UPML
          Hxc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Hyc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
          Hzc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDjh.eq.-1) then ! PMC
          Hxc(0:nPFDi+1,nPFDj,0:nPFDk+1)=-Hxc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hyc(0:nPFDi+1,nPFDj+1,0:nPFDk+1)=Hyc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
          Hzc(0:nPFDi+1,nPFDj,0:nPFDk+1)=-Hzc(0:nPFDi+1,nPFDj-1,0:nPFDk+1)
        end if
        if(nPFDkl.lt.-1) then ! periodic
          Hxc(0:nPFDi+1,0:nPFDj+1,0:1)=Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)*conjg(CFDperZ)
          Hyc(0:nPFDi+1,0:nPFDj+1,0:1)=Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)*conjg(CFDperZ)
          Hzc(0:nPFDi+1,0:nPFDj+1,0:1)=Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk-1:nPFDk)*conjg(CFDperZ)
        else if(nPFDkl.eq.0) then ! PEC
          Hxc(0:nPFDi+1,0:nPFDj+1,0)=Hxc(0:nPFDi+1,0:nPFDj+1,1)
          Hyc(0:nPFDi+1,0:nPFDj+1,0)=Hyc(0:nPFDi+1,0:nPFDj+1,1)
          Hzc(0:nPFDi+1,0:nPFDj+1,0)=-Hzc(0:nPFDi+1,0:nPFDj+1,2)
        else if(nPFDkl.gt.0) then ! UPML
          Hxc(0:nPFDi+1,0:nPFDj+1,0)=(0.0d0,0.0d0)
          Hyc(0:nPFDi+1,0:nPFDj+1,0)=(0.0d0,0.0d0)
          Hzc(0:nPFDi+1,0:nPFDj+1,0)=(0.0d0,0.0d0)
        else if(nPFDkl.eq.-1) then ! PMC
          Hxc(0:nPFDi+1,0:nPFDj+1,0)=-Hxc(0:nPFDi+1,0:nPFDj+1,1)
          Hyc(0:nPFDi+1,0:nPFDj+1,0)=-Hyc(0:nPFDi+1,0:nPFDj+1,1)
          Hzc(0:nPFDi+1,0:nPFDj+1,0)=Hzc(0:nPFDi+1,0:nPFDj+1,2)
        end if
        if(nPFDkh.lt.-1) then ! periodic
          Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Hxc(0:nPFDi+1,0:nPFDj+1,1:2)*CFDperZ
          Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Hyc(0:nPFDi+1,0:nPFDj+1,1:2)*CFDperZ
          Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk:nPFDk+1)=Hzc(0:nPFDi+1,0:nPFDj+1,1:2)*CFDperZ
        else if(nPFDkh.eq.0) then ! PEC
          Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk)=Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk)=Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=-Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        else if(nPFDkh.gt.0) then ! UPML
          Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=(0.0d0,0.0d0)
          Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=(0.0d0,0.0d0)
          Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=(0.0d0,0.0d0)
        else if(nPFDkh.eq.-1) then ! PMC
          Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk)=-Hxc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk)=-Hyc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
          Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk+1)=Hzc(0:nPFDi+1,0:nPFDj+1,nPFDk-1)
        end if
      end if
    end if
  end Subroutine CFDboundary

  Subroutine CFDinitPer()
! initialize constants for periodic boundary conditions
    Implicit none
    yPeriodVector(1)=0.0d0 ! only perpendicular vectors considered!
    yPeriodVector(3)=0.0d0
    zPeriodVector(1:2)=0.0d0
    if(lxPeriod) then
      nPFDil=-2
      nPFDih=-2
      PFDxmin=0.0d0
      PFDxmax=xPeriod
      CFDperX=cdexp((0.0d0,1.0d0)*cxPeriod*xPeriod)
    end if
    if(lyPeriod) then
      nPFDjl=-2
      nPFDjh=-2
      PFDymin=0.0d0
      PFDymax=yPeriodVector(2)
      CFDperY=cdexp((0.0d0,1.0d0)*cyPeriod*yPeriodVector(2))
    end if
    if(lzPeriod) then
      nPFDkl=-2
      nPFDkh=-2
      PFDzmin=0.0d0
      PFDzmax=zPeriodVector(3)
      CFDperZ=cdexp((0.0d0,1.0d0)*czPeriod*zPeriodVector(3))
    end if
  end Subroutine CFDinitPer

  Subroutine DeAllocateCFD()
    Integer(4) ierr
    Deallocate(XYZ,stat=ierr)
    Deallocate(Dxc,stat=ierr)
    Deallocate(Dyc,stat=ierr)
    Deallocate(Dzc,stat=ierr)
    Deallocate(Exc,stat=ierr)
    Deallocate(Eyc,stat=ierr)
    Deallocate(Ezc,stat=ierr)
    Deallocate(Bxc,stat=ierr)
    Deallocate(Byc,stat=ierr)
    Deallocate(Bzc,stat=ierr)
    Deallocate(Hxc,stat=ierr)
    Deallocate(Hyc,stat=ierr)
    Deallocate(Hzc,stat=ierr)
    Deallocate(Ixc,stat=ierr)
    Deallocate(Iyc,stat=ierr)
    Deallocate(Izc,stat=ierr)
    Deallocate(Jxc,stat=ierr)
    Deallocate(Jyc,stat=ierr)
    Deallocate(Jzc,stat=ierr)
    Deallocate(Kxc,stat=ierr)
    Deallocate(Kyc,stat=ierr)
    Deallocate(Kzc,stat=ierr)
    Deallocate(Lxc,stat=ierr)
    Deallocate(Lyc,stat=ierr)
    Deallocate(Lzc,stat=ierr)
    Deallocate(Mxc,stat=ierr)
    Deallocate(Myc,stat=ierr)
    Deallocate(Mzc,stat=ierr)
    Deallocate(Nxc,stat=ierr)
    Deallocate(Nyc,stat=ierr)
    Deallocate(Nzc,stat=ierr)
    Deallocate(Oxc,stat=ierr)
    Deallocate(Oyc,stat=ierr)
    Deallocate(Ozc,stat=ierr)
    Deallocate(Pxc,stat=ierr)
    Deallocate(Pyc,stat=ierr)
    Deallocate(Pzc,stat=ierr)
    Deallocate(O1xc,stat=ierr)
    Deallocate(O1yc,stat=ierr)
    Deallocate(O1zc,stat=ierr)
    Deallocate(P1xc,stat=ierr)
    Deallocate(P1yc,stat=ierr)
    Deallocate(P1zc,stat=ierr)
    Deallocate(O2xc,stat=ierr)
    Deallocate(O2yc,stat=ierr)
    Deallocate(O2zc,stat=ierr)
    Deallocate(P2xc,stat=ierr)
    Deallocate(P2yc,stat=ierr)
    Deallocate(P2zc,stat=ierr)
    Deallocate(EzcInc,stat=ierr)
    Deallocate(HycInc,stat=ierr)
    Deallocate(HzcInc,stat=ierr)
    Deallocate(EycInc,stat=ierr)
    call DeAllocatePFD()
  end Subroutine DeAllocateCFD

  Complex(8) Function timeDepCFD(t,iP)
! simple Example of time dependence of the sources
    Integer(4) iP
    Real(8) t,ft
    Complex(8) expot
    ft=timeDepPFD(t,-1_4)
    expot=cdexp((0.0d0,-2.0d0)*Pi*fcFld*trFld)
    timeDepCFD=ft*expot*PFDsourceA(iP)
  end Function timeDepCFD


end MODULE CHCFD