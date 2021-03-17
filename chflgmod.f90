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
MODULE CHFLG

! Graphic field representation

  USE CHMMP

  SAVE

  CONTAINS

! Threads

  Subroutine TDrawObject(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=4
    call StartFLDThread(lCheck)
  end Subroutine TDrawObject

  Subroutine TDrawField(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=3
    call StartFLDThread(lCheck)
  end Subroutine TDrawField

  Subroutine StartFLDThread(ldi)
! start a new thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start field thread'C)
		call OutTxt('n3',' 'C)
		call OutTxt('m3',' 'C)
    lThreadStarted=.true.
    iStack=0
    iArgument=0
    iCreation=0
    iThread=0
    if(ldi) iArgument=1_4
		if(iThreadHandle.ne.0) then
		  call OutTxt('t3','close thread handle'C)
      ldum=CloseHandle(iThreadHandle)
			if(.not.ldum) then
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start field thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start field thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(FLDThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start field thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
    end if
  end Subroutine StartFLDThread

  Integer(4) Function FLDThread(iWhat)
! tread calls.....
    Implicit none
    Integer(4) iWhat
    Logical ldum
    Include 'resource.fd'
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    FLDThread=0_4
    if(iThreadAction.eq.3) then
      call MTDrawField(ldum)
    else if(iThreadAction.eq.4) then
      call MTDrawObject(ldum)
    end if
    call endThread()
  end Function FLDThread

  Subroutine MTDrawField(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
    lWinFld(kWin)=.true.
   ! iWinAction=0_2
    if(.not.lCheck) then
      idum=MessageBoxQQ('Clear graphic window first?'C,'Draw field'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) then
        call LeaveCriticalSection(Loc(DrawLock))
        return
      end if
      if(idum.eq.MB$IDYES) then
        call DrawWindow(lCheck)
      end if
    end if
    lLimits=.false.
    call DrawField(.true.)
    if(lGRCallocated) call GRCflush()
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTDrawField

  Subroutine MTDrawObject(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
   ! iWinAction=0_2
    lWinFld(kWin)=.true.
    if(.not.lCheck) then
      idum=MessageBoxQQ('Clear graphic window first?'C,'Draw Object'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) then
        call LeaveCriticalSection(Loc(DrawLock))
        return
      end if
      if(idum.eq.MB$IDYES) then
        call DrawWindow(lCheck)
      end if
    end if
    call DrawObject(iDraOBJ)
    if(lGRCallocated) call GRCflush()
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTDrawObject

! field representation

  Subroutine DrawField(lCheck)
! draw the current field
    Implicit none
    Include 'resource.fd'
 	  Real(8) r(3),r0(3),r1(3),r2(3),rh(3),rp(3,4),f(4),dx(3),dy(3),dz(3),dx0(3),dy0(3),ez(3), &
    &       d,dum,dmin,scl,sclD,sclm,w,xp,yp,xmin,xmax,ymin,ymax,rjk0(3)
    Integer(4) istat,ia(1),i,j,k,j0,j1,j2,j1p,j2p,js,k0,k1,k2,k1p,k2p,ks,jk0,jkm,iOK
    Integer(2) idu,ix,iy
    Logical(4), intent(in) :: lCheck
    Logical(4) lxy
    if(.not.lgcFld) lGet3DMat=.true.
    if(lCheck.or.lLimits) lWinFld(kWin)=.true.
    lStopThread=.false.
! set window
    call GetKWin(.false.)
    istat=SetActiveQQ(10+kWin)
! GRC
    if(lGRCallocated) then
      iGRCcolorMin=minCIntensity
      iGRCcolorMax=maxCIntensity
      GRCcolorFMin=rMinFld
      GRCcolorFMax=rMaxFld
    end if
! prepare constants
    dmin=pBig
    xmin=pBig
    xmax=nBig
    ymin=pBig
    ymax=nBig
    call GetEz(ez)
    if(lrGrd) then
      call GetDxyz(ez,dx,dy,dz)
      if(lLimits) then
        r=spacecFld(1:3,0)+dz
        call Proj3D(r,ViewPlane,rEye,viewDist,xp,yp)
        xmin=Min(xp,xmin)
        xmax=Max(xp,xmax)
        ymin=Min(yp,ymin)
        ymax=Max(yp,ymax)
        r=r+dx
        call Proj3D(r,ViewPlane,rEye,viewDist,xp,yp)
        xmin=Min(xp,xmin)
        xmax=Max(xp,xmax)
        ymin=Min(yp,ymin)
        ymax=Max(yp,ymax)
        r=r+dy
        call Proj3D(r,ViewPlane,rEye,viewDist,xp,yp)
        xmin=Min(xp,xmin)
        xmax=Max(xp,xmax)
        ymin=Min(yp,ymin)
        ymax=Max(yp,ymax)
        r=r-dx
        call Proj3D(r,ViewPlane,rEye,viewDist,xp,yp)
        xmin=Min(xp,xmin)
        xmax=Max(xp,xmax)
        ymin=Min(yp,ymin)
        ymax=Max(yp,ymax)
        WinXmin(kWin)=xmin
        WinYmin(kWin)=ymin
        WinXmax(kWin)=WinXmin(kWin)+Max(xmax-xmin,ymax-ymin,pSmall,1.0000001*dAbs(WinXmin(kWin)))
        WinYmax(kWin)=WinYmin(kWin)+Max(xmax-xmin,ymax-ymin,pSmall,1.0000001*dAbs(WinYmin(kWin)))
        return
      end if
    end if
    sclm=ArrowLength
    scl=Max(dabs(rMinFld),dabs(rMaxFld))+1.0d-100
    sclD=Grid3D.div.scl
! start drawing in the background
    lxy=.false.
    k1=1
    k2=nxrFld
    ks=1
    j1=1
    j2=nyrFld
    js=1
    if(lrGrd) then
      r=spacecFld(1:3,0)+dz-rEye
      f(1)=r3Vec_Length(r)
      r=r+dble(k2-k1)*dx
      f(2)=r3Vec_Length(r)
      r=r+dble(j2-j1)*dy
      f(3)=r3Vec_Length(r)
      r=r-dble(k2-k1)*dx
      f(4)=r3Vec_Length(r)
    else
      r=getrGrd(k1,j1)-rEye
      f(1)=r3Vec_Length(r)
      r=getrGrd(k2,j1)-rEye
      f(2)=r3Vec_Length(r)
      r=getrGrd(k2,j2)-rEye
      f(3)=r3Vec_Length(r)
      r=getrGrd(k1,j2)-rEye
      f(4)=r3Vec_Length(r)
    end if
    ia=MaxLoc(f)
    i=ia(1)
    if(i.eq.1) then
      if(f(4).gt.f(2)) then
        lxy=.true.
        k2=nyrFld
        j2=nxrFld
      end if
    else if(i.eq.2) then
      if(f(3).gt.f(1)) then
        lxy=.true.
        k1=1
        k2=nyrFld
        ks=1
        j1=nxrFld
        j2=1
        js=-1
      else
        k1=nxrFld
        k2=1
        ks=-1
        j1=1
        j2=nyrFld
        js=1
      end if
    else if(i.eq.3) then
      if(f(2).gt.f(4)) then
        lxy=.true.
        k1=nyrFld
        k2=1
        ks=-1
        j1=nxrFld
        j2=1
        js=-1
      else
        k1=nxrFld
        k2=1
        ks=-1
        j1=nyrFld
        j2=1
        js=-1
      end if
    else
      if(f(1).gt.f(3)) then
        lxy=.true.
        k1=nyrFld
        k2=1
        ks=-1
        j1=1
        j2=nxrFld
        js=1
      else
        k1=1
        k2=nxrFld
        ks=1
        j1=nyrFld
        j2=1
        js=-1
      end if
    end if
! draw field intensity
    if((.not.lLimits).and.lCheck.and.(itIntensity.ne.0)) then
      scl=(scaleIntensity*sclm).div.Max(dabs(rMinFld),dabs(rMaxFld))
      if(lrGrd) then
        r=spacecFld(1:3,0)+dz
        if(lxy) then
          dx0=dble(ks)*dy
          dy0=dble(js)*dx
          if(js.lt.0) r=r+dble(nxrFld-2)*dx
          if(ks.lt.0) r=r+dble(nyrFld-2)*dy
        else
          dx0=dble(ks)*dx
          dy0=dble(js)*dy
          if(ks.lt.0) r=r+dble(nxrFld-2)*dx
          if(js.lt.0) r=r+dble(nyrFld-2)*dy
        end if
      end if
      if(abs(itIntensity).eq.1) then
        j1p=0
        j2p=0
        k1p=0
        k2p=0
      else
        j1p=Min(0,js)
        j2p=Max(0,js)
        k1p=Min(0,ks)
        k2p=Max(0,ks)
      end if
! compute distances
      jk0=0
      do j0=j1+j1p,j2-j2p,js
        if(lrGrd) rh=r
        do k0=k1+k1p,k2-k2p,ks
          if(lStopThread) return
          jk0=jk0+1
          jrFld(jk0)=j0
          krFld(jk0)=k0
          if(lxy) then
            j=k0
            k=j0
          else
            j=j0
            k=k0
          end if
          call Get3DCell(k,j,rp(1:3,1:4),f(1:4),dx,dy,ez,sclD,-1)
          rjk0=0.25d0*(rp(1:3,1)+rp(1:3,2)+rp(1:3,3)+rp(1:3,4))-rEye
          drFld(jk0)=r3Vec_Length(rjk0)
          if(lrGrd) r=r+dx0
        end do
        if(lrGrd) r=rh+dy0
      end do
! sort
      jkm=jk0
      if(jk0.le.10000) then
        if(lStopThread) return
        call QuickIndex(drFld,1,jkm,irFld,.true.)
        if(lStopThread) return
        call IIndexSort(jrFld,jkm,irFld,iOK)
        if(lStopThread) return
        call IIndexSort(krFld,jkm,irFld,iOK)
      end if
! draw cells
      do jk0=jkm,1,-1
        if(lStopThread) return
        j0=jrFld(jk0)
        k0=krFld(jk0)
        if(lxy) then
          j=k0
          k=j0
        else
          j=j0
          k=k0
        end if
        call Get3DCell(k,j,rp(1:3,1:4),f(1:4),dx,dy,ez,sclD,-1)
        call Draw3DCell(rp(1:3,1:4),f,1_2)
      end do
    end if
! draw field arrows
    if(lLimits.or.(.not.lCheck).or.((itArrow.ne.0).and.(dabs(ArrowLength).gt.pSmall))) then
      scl=(scaleArrow*sclm).div.Max(dabs(rMinFld),dabs(rMaxFld))
      if(lrGrd) then
        r=spacecFld(1:3,0)+dz
        if(lxy) then
          dx0=dble(ks*nsFld)*dy
          dy0=dble(js*nsFld)*dx
          if(js.lt.0) r=r+dble(nxrFld-1)*dx
          if(ks.lt.0) r=r+dble(nyrFld-1)*dy
        else
          dx0=dble(ks*nsFld)*dx
          dy0=dble(js*nsFld)*dy
          if(ks.lt.0) r=r+dble(nxrFld-1)*dx
          if(js.lt.0) r=r+dble(nyrFld-1)*dy
        end if
      end if
      do j0=j1,j2,js*nsFld
        if(lrGrd) rh=r
        do k0=k1,k2,ks*nsFld
          if(lStopThread) return
          if(lxy) then
            j=k0
            k=j0
          else
            j=j0
            k=k0
          end if
          if(.not.lrGrd) r=getrGrd(k,j)
          r0=r+sclD*rFld(0,k,j)*ez
          if(lCheck) then
            r1=rEye-r0
            call Unit3DV(r1)                 ! unit vector of the "Sehstrahl"
            d=r3Vec_Length(rFld(1:3,k,j))    ! length of the field vector
            if(d.gt.1.0d-100) then
              w=r3Scl_Prod(r1,rFld(1:3,k,j)) ! projection of the field vector on the "Sehstrahl"
              idu=Int2(in_Rinterval(w,-d,d,Int4(minCArrow),Int4(maxCArrow),.false.)) ! color (gray scale)
              dum=Min(sclm/d,scl)            ! max. scaling
              r2=dum*rFld(1:3,k,j)           ! scaled field vector
              w=dabs(dum*w)                  ! projection of the scaled field vector on the "Sehstrahl"
              r2=r0+r2                       ! end point of the arrow
              call Draw3DArrow(r0,r2,w,itArrow,1_2,idu,lArrowFill)
            end if
          else
            if(lLimits) then
              call Proj3D(r0,ViewPlane,rEye,viewDist,xp,yp)
              xmin=Min(xp,xmin)
              xmax=Max(xp,xmax)
              ymin=Min(yp,ymin)
              ymax=Max(yp,ymax)
            else
              call Blind3DPoint(r0,ix,iy)
              d=dsqrt(dble(ix-ixrFld)**2+dble(iy-iyrFld)**2)
              if(d.lt.dmin) then
                dmin=d
                call Getijk(k,j,ixcFld,iycFld,izcFld)
              end if
            end if
          end if
          if(lrGrd) r=r+dx0
        end do
        if(lrGrd) r=rh+dy0
      end do
      if(lLimits) then
        xmin=Min(xp,xmin)
        xmax=Max(xp,xmax)
        ymin=Min(yp,ymin)
        ymax=Max(yp,ymax)
        WinXmin(kWin)=xmin
        WinYmin(kWin)=ymin
        WinXmax(kWin)=WinXmin(kWin)+Max(xmax-xmin,ymax-ymin,pSmall,1.0000001*dAbs(WinXmin(kWin)))
        WinYmax(kWin)=WinYmin(kWin)+Max(xmax-xmin,ymax-ymin,pSmall,1.0000001*dAbs(WinYmin(kWin)))
      end if
    end if
  end Subroutine DrawField

! 3D objects

  Subroutine DrawObject(kOb)
! draw the 3D object kOb (all objects if kOb=0, object 1...-kOb if kOb<0)
! if lObjMat use matching point representation, otherwise use grid representation
    Implicit none
    Real(8) x,y,P(3),O(3),e(3),vt(3),s,s1,s2,ds,er,fi,r(3),r1(3),r2(3),rn(3),sclD,&
    & PA(2),PB(2),PC(2),vx(3),vy(3),vz(3),dmin,rNmin(3),val(2)
    Integer(4) kOb,k1,k2,kO,nP,mP,iP,k,i,ik,ier,idum,i1,i2,nmP,nmPA,ioff,nxy,lout,iW0
    Integer(2) ic0,iDL,iDR
    Logical lf1,lgetE
    Data nmPA/0_4/
    IntInter=0 ! do not iterpolate when computing field
    if(.not.lgcFld) lGet3DMat=.true.
! lf1 determines how the scalar field f(1) is derived from the vector field f(2:4)
    if((itrFld.eq.itWe).or.(itrFld.eq.itWh).or.(itrFld.eq.itWt).or. &
    &  (itrFld.eq.itPe).or.(itrFld.eq.itPh).or.(itrFld.eq.itPt).or. &
    &  (itrFld.eq.itPe)) then
      lf1=.true.
    else
      lf1=.false.
      i=0
      if(lxrFld) i=i+1
      if(lyrFld) i=i+1
      if(lzrFld) i=i+1
      if(i.lt.2) lf1=.true.
    end if
    nxy=0
    if(lObjFlg) nxy=nxrFld*nyrFld
! set window
    lWinFld(kWin)=.true.
    call GetKWin(.false.)
    k1=SetActiveQQ(10+kWin)
    ic0=SetColor(1_2) ! save current color
    nmP=0
! GRC
    if(lGRCallocated) then
      iGRCcolorMin=minCIntensity
      iGRCcolorMax=maxCIntensity
      GRCcolorFMin=rMinFld
      GRCcolorFMax=rMaxFld
    end if
! determine min/max Object numbers
    if((kOb.gt.0).and.(kOb.le.nObj)) then
      k1=kOb
      k2=k1
    else if(kOb.lt.0) then
      k1=1
      k2=min(-kOb,nObj)
    else
      k1=1
      k2=nObj
    end if
! get points
    if(lObjMat) then ! matching point representation
      call OutTxt('t2','Get match.pts.'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      iW0=0_4
      if(lObjMatW0) iW0=2_4
      if(iObjDra.eq.5) then ! assoc.expans.
        call get3DMatPts(k1,k2,iW0,3_2,.true.)
        call GetDminExpsMatch()
        call GetExpsNearMatPts(k1,k2)
      else if(iObjDra.eq.4) then ! field
        call get3DMatPts(k1,k2,iW0,2_2,.true.)
      else if(iObjDra.eq.3) then ! errors
        call get3DMatPts(k1,k2,iW0,1_2,.true.)
      else if(iObjDra.eq.0) then ! object
        call get3DMatPts(k1,k2,iW0,3_2,.true.)
      else
        call get3DMatPts(k1,k2,iW0,0_2,.true.)
      end if
      nmP=0
    else ! regular grid representation
      call cBndGetABO() ! get c-poly, splines, match.pts
      tOBJ(1:nObj)%iGrf=0
      tOBJ(1:nObj)%nGrf=0
      tOBJ(1:nObj)%mGrf=0
      call OutTxt('t2','Grid object'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      do kO=k1,k2
        call IntToStr(Int4(kO),0,0,SpaceText,lout)
        call OutTxt('n2',SpaceText(1:lout))
        call IntToStr(k2,0,0,SpaceText,lout)
        call OutTxt('m2',SpaceText(1:lout))
        ! call SleepQQ(1_4)
        if(lStopThread) Exit
        if((tOBJ(kO)%iTypO.ne.3).and.(tOBJ(kO)%iTypO.ne.4)) then
          if((tOBJ(kO)%iPar(1).gt.tOBJ(kO)%iPar(2)).or.(tOBJ(kO)%iPar(2).lt.1)) Cycle
          x=0.0d0 ! total boundary length x -> mP
          do k=max(1,tOBJ(kO)%iPar(1)),tOBJ(kO)%iPar(2)
            x=x+tBnd(k)%sLength
          end do
          mP=min(1000_4,max(2_4,1_4+Int4(x.div.tOBJ(kO)%GrfRes))) ! number of grid lines along boundary
        end if
        Select Case(tOBJ(kO)%iTypO) ! maximum length y along generator -> nP
        Case(0) ! Torus
          y=0.0d0
          O=0.0d0
          e=0.0d0
          e(2)=1.0d0
          ioff=tBnd(tOBJ(kO)%iPar(1))%iMatOffset
          s1=tBnd(tOBJ(kO)%iPar(1))%Start
          s2=s1+x
          ds=0.99999999999d0*(s2-s1)/Dble(max(1,mP-1))
          s=s1+1.0d-12*ds-ds
          do i=1,mP
            s=s+ds
            call GetBndPt(0,s,P(1),P(2),vt(1),vt(2),iDL,iDR,idum)
            P(3)=0.0d0
            y=max(y,Dist3DPtAxis(P,O,e))
          end do
          y=y*Pi*abs(tOBJ(kO)%Par(2))/180.0d0
        Case(1) ! Cylinder
          y=abs(tOBJ(kO)%Par(2))
        Case(2) ! Spiral
          y=0.0d0
          O=tOBJ(kO)%O
          e=tOBJ(kO)%e
          ioff=tBnd(tOBJ(kO)%iPar(1))%iMatOffset
          s1=tBnd(tOBJ(kO)%iPar(1))%Start
          s2=s1+x
          ds=0.99999999999d0*(s2-s1)/Dble(max(1,mP-1))
          s=s1+1.0d-12*ds-ds
          do i=1,mP
            s=s+ds
            call GetBndPt(0,s,P(1),P(2),vt(1),vt(2),iDL,iDR,idum)
            P(3)=0.0d0
            y=max(y,Dist3DPtAxis(P,O,e))
          end do
          y=SpiralLength(y,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3))
        Case(3) ! Triangle
          iDR=tBnd(tOBJ(kO)%iPar(1))%iLDom
          iDL=tBnd(tOBJ(kO)%iPar(1))%iRDom
          PB(1:2)=tOBJ(kO)%O(1:2)
          PC(1)=tOBJ(kO)%O(3)
          PC(2)=tOBJ(kO)%e(1)
          PA(1:2)=tOBJ(kO)%e(2:3)
          call getTriangle2DData(PA,PB,PC,x,s1,s2,s,ds,y)
          mP=min(1000_4,max(2_4,1_4+Int4(x.div.tOBJ(kO)%GrfRes))) ! number of grid lines along boundary
        Case(4) ! Rectangle
          iDR=tBnd(tOBJ(kO)%iPar(1))%iLDom
          iDL=tBnd(tOBJ(kO)%iPar(1))%iRDom
          x=tOBJ(kO)%Par(1)
          y=tOBJ(kO)%Par(2)
          mP=min(1000_4,max(2_4,1_4+Int4(x.div.tOBJ(kO)%GrfRes))) ! number of grid lines along boundary
        Case(5) !  ! Cone
          y=abs(tOBJ(kO)%Par(2))
        end Select
        nP=min(1000_4,max(2_4,1_4+Int4(y.div.tOBJ(kO)%GrfRes))) ! number of grid lines along generator
        tOBJ(kO)%nGrf=nP
        tOBJ(kO)%mGrf=mP
        nmP=nmP+nP*mP
        if(kO.lt.nObj) tOBJ(kO+1)%iGrf=nmP ! offset number
      end do
    end if
! free memory
    call OutTxt('t2','Allocate memory'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    if(Allocated(ObjWeb)) Deallocate(ObjWeb)
    if(Allocated(ObjWebX)) Deallocate(ObjWebX)
    if(Allocated(ObjWebY)) Deallocate(ObjWebY)
    if(Allocated(ObjWebZ)) Deallocate(ObjWebZ)
    if(Allocated(ObjWebV)) Deallocate(ObjWebV)
    if(Allocated(ObjWebF)) Deallocate(ObjWebF)
    if(Allocated(iObjWeb)) Deallocate(iObjWeb)
    if(Allocated(iCObjWeb)) Deallocate(iCObjWeb)
    if(Allocated(nxObjWeb)) Deallocate(nxObjWeb)
    if(Allocated(nyObjWeb)) Deallocate(nyObjWeb)
    if(Allocated(BndPt3DF)) Deallocate(BndPt3DF)
    if(Allocated(BndPt3DV)) Deallocate(BndPt3DV)
    if(lObjMat) then
      nObjWeb=0
    else
      nObjWeb=1+k2-k1
    end if
    if(lObjFlg) nObjWeb=nObjWeb+1
! allocate memory
    if((nmP+nxy).gt.0) then
      Allocate(ObjWeb(3,nmP+nxy),ObjWebV(3,nmP+nxy),ObjWebF(nmP+nxy),iObjWeb(nmP+nxy), &
      &        iCObjWeb(nObjWeb),nxObjWeb(nObjWeb),nyObjWeb(nObjWeb),stat=ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Memory alloction failed!'C,'DrawObject'C, &
                          MB$OK.or.MB$ICONSTOP)
        nmPA=0
        return
      end if
      nmPA=nmP+nxy
    end if
    if(lObjMat) then
      Allocate(BndPt3DF(nBndPt3D),BndPt3DV(3,nBndPt3D),stat=ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Memory alloction failed!'C,'DrawObject'C, &
                          MB$OK.or.MB$ICONSTOP)
        nmPA=0
        return
      end if
    end if
! auxiliary data of ObjWeb
    if((nmP+nxy).gt.0) then
      if(lObjFlg) then
        if(nObjWeb.gt.1) then
          nxObjWeb(1:nObjWeb-1)=tOBJ(k1:k2)%nGrf
          nyObjWeb(1:nObjWeb-1)=tOBJ(k1:k2)%mGrf
          if(iObjDra.eq.0) then
            do i=1,nObjWeb-1
              iCObjWeb(i)=iPackObjC(tOBJ(i)%iColMin,tOBJ(i)%iColMax) ! draw surface with object colors
            end do
          else
            iCObjWeb(1:nObjWeb-1)=-30000_2-iObjDra
          end if
        end if
        nxObjWeb(nObjWeb)=nxrFld
        nyObjWeb(nObjWeb)=nyrFld
        iCObjWeb(nObjWeb)=-30004_2
      else
        if(nObjWeb.gt.0) then
          nxObjWeb(1:nObjWeb)=tOBJ(k1:k2)%nGrf
          nyObjWeb(1:nObjWeb)=tOBJ(k1:k2)%mGrf
          if(iObjDra.eq.0) then
            do i=1,nObjWeb
              iCObjWeb(i)=iPackObjC(tOBJ(i)%iColMin,tOBJ(i)%iColMax) ! draw surface with object colors
            end do
          else
            iCObjWeb(1:nObjWeb)=-30000_2-iObjDra
          end if
        end if
      end if
      if(iObjDra.eq.3) then ! draw error requires tangential and normal vectors in each point
        Allocate(ObjWebX(3,nmP),ObjWebY(3,nmP),ObjWebZ(3,nmP),stat=ier)
        if(ier.ne.0) then
          idum=MessageBoxQQ('Memory alloction failed!'C,'DrawObject'C, &
                            MB$OK.or.MB$ICONSTOP)
          nmPA=0
          return
        end if
      end if
    end if
! generate all grid points -> ObjWeb
    if(.not.lObjMat) then
      call OutTxt('t2','Generate object'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      do kO=k1,k2 
        if((tOBJ(kO)%iTypO.ne.3).and.(tOBJ(kO)%iTypO.ne.4).and.((tOBJ(kO)%iPar(1).gt.tOBJ(kO)%iPar(2)) &
        & .or.(tOBJ(kO)%iPar(2).lt.1))) Cycle
        call IntToStr(Int4(kO),0,0,SpaceText,lout)
        call OutTxt('n2',SpaceText(1:lout))
        call IntToStr(k2,0,0,SpaceText,lout)
        call OutTxt('m2',SpaceText(1:lout))
        ! call SleepQQ(1_4)
        if(lStopThread) Exit
        nP=tOBJ(kO)%nGrf
        mP=tOBJ(kO)%mGrf
        iP=tOBJ(kO)%iGrf
        if((tOBJ(kO)%iTypO.ne.3).and.(tOBJ(kO)%iTypO.ne.4)) then
          s1=tBnd(tOBJ(kO)%iPar(1))%Start
          s2=s1
          do k=max(1,tOBJ(kO)%iPar(1)),tOBJ(kO)%iPar(2)
            s2=s2+tBnd(k)%sLength
          end do
          ds=0.99999999999d0*(s2-s1)/Dble(max(1,mP-1))
          s=s1+1.0d-12*ds-ds
        end if
        Select Case(tOBJ(kO)%iTypO)
        Case(0) ! Torus
          i1=iP+1
          i2=iP+nP
          do i=1,mP
            if(lStopThread) Exit
            s=s+ds
            call GetBndPt(0,s,P(1),P(2),vt(1),vt(2),iDL,iDR,k)
            P(3)=0.0d0
            if(iObjDra.eq.3) then
              vt(3)=0.0d0
              vt=ds*vt
              call Pt2Torus(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom,iDL,iDR, &
              &             iObjWeb(i1:i2),ObjWebX(1:3,i1:i2),ObjWebY(1:3,i1:i2),ObjWebZ(1:3,i1:i2),vt)
            else
              call Pt2Torus(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom,iDL,iDR, &
              &             iObjWeb(i1:i2))
            end if
            if(iObjDra.eq.0) iObjWeb(i1:i2)=kO
            if(iObjDra.eq.3) iObjWeb(i1:i2)=k
            i1=i1+nP
            i2=i2+nP
          end do
        Case(1) ! Cylinder
          i1=iP+1
          i2=iP+nP
          do i=1,mP
            if(lStopThread) Exit
            s=s+ds
            call GetBndPt(0,s,P(1),P(2),vt(1),vt(2),iDL,iDR,k)
            P(3)=0.0d0
            if(iObjDra.eq.3) then
              vt(3)=0.0d0
              vt=ds*vt
              call Pt2Cylinder(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom, &
              &                iDL,iDR,iObjWeb(i1:i2),ObjWebX(1:3,i1:i2),ObjWebY(1:3,i1:i2),ObjWebZ(1:3,i1:i2),vt)
            else
              call Pt2Cylinder(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom, &
              &                iDL,iDR,iObjWeb(i1:i2))
            end if
            if(iObjDra.eq.0) iObjWeb(i1:i2)=kO
            if(iObjDra.eq.3) iObjWeb(i1:i2)=k
            i1=i1+nP
            i2=i2+nP
          end do
        Case(2) ! Spiral
          i1=iP+1
          i2=iP+nP
          do i=1,mP
            if(lStopThread) Exit
            s=s+ds
            call GetBndPt(0,s,P(1),P(2),vt(1),vt(2),iDL,iDR,k)
            P(3)=0.0d0
            if(iObjDra.eq.3) then
              vt(3)=0.0d0
              vt=ds*vt
              call Pt2Spiral(P,tOBJ(kO)%O,tOBJ(kO)%e,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3), &
              &              nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom,iDL,iDR, &
              &              iObjWeb(i1:i2),ObjWebX(1:3,i1:i2),ObjWebY(1:3,i1:i2),ObjWebZ(1:3,i1:i2),vt)
            else
              call Pt2Spiral(P,tOBJ(kO)%O,tOBJ(kO)%e,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3), &
              &              nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom,iDL,iDR,iObjWeb(i1:i2))
            end if
            if(iObjDra.eq.0) iObjWeb(i1:i2)=kO
            if(iObjDra.eq.3) iObjWeb(i1:i2)=k
            i1=i1+nP
            i2=i2+nP
          end do
        Case(3) ! Triangle
          iDR=tBnd(tOBJ(kO)%iPar(1))%iLDom
          iDL=tBnd(tOBJ(kO)%iPar(1))%iRDom
          i1=iP+1
          i2=iP+nP*mP
          if(iDL.ne.-257_2) then
            if(iObjDra.gt.1) then ! use domain colors
              iObjWeb(i1)=iPackDom(nDom,iDL,iDR)
              if(i2.gt.i1) iObjWeb(i1+1:i2)=iObjWeb(i1)
            else if(iObjDra.eq.0) then
              iObjWeb(i1:i2)=kO
            else
              iObjWeb(i1:i2)=-1_2
            end if
          end if
          if(iObjDra.eq.3) iObjWeb(i1:i2)=tOBJ(kO)%iPar(1)
          i1=iP+1
          r1(1:2)=(PC(1:2)-PB(1:2))/Dble(mP-1)
          r1(3)=0.0d0
          if(iObjDra.eq.3) then
            r2(1)=-r1(2)
            r2(2)=r1(1)
            r2(3)=r1(3)
            rn=r3Vec_Prod(r1,r2)
            call rvLoc2Glob(r1,tOBJ(kO)%Plane,vx)
            call rvLoc2Glob(r2,tOBJ(kO)%Plane,vy)
            call rvLoc2Glob(rn,tOBJ(kO)%Plane,vz)
          end if
          P(1:2)=PB(1:2)
          do i=1,mP
            if(lStopThread) Exit
            r2(1:2)=(PA(1:2)-P(1:2))/Dble(nP-1)
            r2(3)=0.0d0
            do k=1,nP
              ObjWeb(1:2,i1)=P(1:2)+Dble(k-1)*r2(1:2)
              ObjWeb(3,i1)=0.0d0
              call vLoc2Glob(ObjWeb(1:3,i1),tOBJ(kO)%Plane,ObjWeb(1:3,i1))
              if(iObjDra.eq.3) then
                ObjWebX(1:3,i1)=vx
                ObjWebY(1:3,i1)=vy
                ObjWebZ(1:3,i1)=vz
              end if
              i1=i1+1
            end do
            P(1:2)=P(1:2)+r1(1:2)
          end do
        Case(4) ! Rectangle
          iDR=tBnd(tOBJ(kO)%iPar(1))%iLDom
          iDL=tBnd(tOBJ(kO)%iPar(1))%iRDom
          i1=iP+1
          i2=iP+nP*mP
          if(iDL.ne.-257_2) then
            if(iObjDra.gt.1) then ! use domain colors
              iObjWeb(i1)=iPackDom(nDom,iDL,iDR)
              if(i2.gt.i1) iObjWeb(i1+1:i2)=iObjWeb(i1)
            else if(iObjDra.eq.0) then
              iObjWeb(i1:i2)=kO
            else
              iObjWeb(i1:i2)=-1_2
            end if
          end if
          if(iObjDra.eq.3) iObjWeb(i1:i2)=tOBJ(kO)%iPar(1)
          ObjWeb(3,i1:i2)=0.0d0
          r1=0.0d0
          r2=0.0d0
          r1(1)=tOBJ(kO)%Par(1)/Dble(mP-1)
          r2(2)=tOBJ(kO)%Par(2)/Dble(nP-1)
          if(iObjDra.eq.3) then
            rn=r3Vec_Prod(r1,r2)
            call rvLoc2Glob(r1,tOBJ(kO)%Plane,vx)
            call rvLoc2Glob(r2,tOBJ(kO)%Plane,vy)
            call rvLoc2Glob(rn,tOBJ(kO)%Plane,vz)
          end if
          do i=1,mP
            if(lStopThread) Exit
            ObjWeb(1,i1:i1+nP-1)=Dble(i-1)*r1(1)
            do k=1,nP
              ObjWeb(2,i1)=Dble(k-1)*r2(2)
              call vLoc2Glob(ObjWeb(1:3,i1),tOBJ(kO)%Plane,ObjWeb(1:3,i1))
              if(iObjDra.eq.3) then
                ObjWebX(1:3,i1)=vx
                ObjWebY(1:3,i1)=vy
                ObjWebZ(1:3,i1)=vz
              end if
              i1=i1+1
            end do
          end do
        Case(5) ! Cone
          i1=iP+1
          i2=iP+nP
          do i=1,mP
            if(lStopThread) Exit
            s=s+ds
            call GetBndPt(0,s,P(1),P(2),vt(1),vt(2),iDL,iDR,k)
            P(3)=0.0d0
            if(iObjDra.eq.3) then
              vt(3)=0.0d0
              vt=ds*vt
              call Pt2Cone(P,tOBJ(kO)%O,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom,&
              &                iDL,iDR,iObjWeb(i1:i2),ObjWebX(1:3,i1:i2),ObjWebY(1:3,i1:i2),ObjWebZ(1:3,i1:i2),vt)
            else
              call Pt2Cone(P,tOBJ(kO)%O,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,tOBJ(kO)%Plane,ObjWeb(1:3,i1:i2),iObjDra,nDom,&
              &                iDL,iDR,iObjWeb(i1:i2))
            end if
            if(iObjDra.eq.0) iObjWeb(i1:i2)=kO
            if(iObjDra.eq.3) iObjWeb(i1:i2)=k
            i1=i1+nP
            i2=i2+nP
          end do
        end Select
      end do
      if(iObjDra.eq.3) then
        do k=1,nmP
          call Ortho3DSpace3(ObjWebX(1:3,k),ObjWebY(1:3,k),ObjWebZ(1:3,k))
        end do
      end if
    end if
! get field / error / color
    if(lObjMat) then ! matching points
      kO=1
      call OutTxt('t2','Match. points'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      call OutTxt('t1','Get field point'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
      lgetE=.true.
      if(Allocated(eBndPt3D).and.(nBndPt3D.eq.mBndPt3DAlloc)) then
        do i=1,nBndPt3D
          if(eBndPt3D(i).gt.0.0d0) then
            lgetE=.false.
            Exit
          end if
        end do
      end if
      do i=1,nBndPt3D
        if(.not.lgcFld) then
          if(iObjBndPt3D(i).lt.0) Cycle
        end if
        call IntToStr(i,0,0,SpaceText,lout)
        call OutTxt('n1',SpaceText(1:lout))
        call IntToStr(Int4(nBndPt3D),0,0,SpaceText,lout)
        call OutTxt('m1',SpaceText(1:lout))
        ! call SleepQQ(1_4)
        r(1:3)=BndPt3D(1:3,0,i)
        BndPt3DF(i)=0.0d0
        BndPt3DV(1:3,i)=0.0d0
        if(iObjDra.eq.0) then ! iBndPt3D shall contain object colors
          do while((i.gt.tOBJ(kO)%iMatOffset+tOBJ(kO)%nMat).and.(kO.lt.nObj))
            kO=kO+1
          end do
     !!!     if(iBndPt3D(i).eq.-30003) then
     !!!       iBndPt3D(i)=iPackObjC(tOBJ(kO)%iColMax,tOBJ(kO)%iColMin)
     !!!     else
     !!!       iBndPt3D(i)=iPackObjC(tOBJ(kO)%iColMin,tOBJ(kO)%iColMax)
     !!!     end if
          iWhatiBndPt3D=4_2
        else if(iObjDra.eq.3) then ! BndPt3DF=error
          if(lgetE) then
            r1(1:3)=BndPt3D(1:3,1,i)
            r2(1:3)=BndPt3D(1:3,2,i)
            rn(1:3)=BndPt3D(1:3,3,i)
            call Unit3DV(r1)
            call Unit3DV(r2)
            call Unit3DV(rn)
            call GetError(r,r1,r2,rn,iBndPt(iBndPt3D(i)),1.0d0,er,fi,idum)
          else
            idum=0
            er=eBndPt3D(i)
            fi=fBndPt3D(i)
          end if
          if(idum.ne.0) then
            er=0.0d0
          else
            if(errorScale.lt.0.0d0) then
              er=-errorScale*er.div.fi
            else
              er=errorScale*er
            end if
          end if
          BndPt3DF(i)=er
          BndPt3DV(1:3,i)=scaleArrow*er*rn(1:3)
        else if(iObjDra.eq.4) then ! BndPt3DF,V=field
          call unPackDom(1.0d0,iBndPt3D(i),iDl,iDr) ! iDl,iDr contain the domain numbers
          if(iDl.lt.0) call DistPtObj(0_2,0_2,BndPt3D(1:3,0,i),.true.,dmin,rNmin,iDl,val,.true.)
          if(iDr.lt.0) call DistPtObj(0_2,0_2,BndPt3D(1:3,0,i),.true.,dmin,rNmin,iDr,val,.true.)
          if(iDl.gt.0) then
            call GetLocrField(r,iDl,IntInter,r1)
            if(lf1) then
              fi=r1(1)+r1(3)+r1(2)
            else
              fi=r3Vec_Length(r1)
            end if
            BndPt3DV(1:3,i)=r1(1:3)
            BndPt3DF(i)=fi
          else
            BndPt3DV(1:3,i)=0.0d0
            BndPt3DF(i)=0.0d0
          end if
          if(iDr.gt.0) then
            call GetLocrField(r,iDr,IntInter,r1)
            if(lf1) then
              fi=r1(1)+r1(3)+r1(2)
            else
              fi=r3Vec_Length(r1)
            end if
            BndPt3DV(1:3,i)=BndPt3DV(1:3,i)+r1(1:3)
            BndPt3DF(i)=BndPt3DF(i)+fi
          end if
          if((iDl.ne.0).and.(iDr.ne.0)) then
            BndPt3DV(1:3,i)=0.5d0*BndPt3DV(1:3,i)
            BndPt3DF(i)=0.5d0*BndPt3DF(i)
          end if
        end if
      end do
      call OutTxt('t1','Point'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
    else ! regular grid
      call OutTxt('t2','Get field object'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      do kO=k1,k2
        call IntToStr(Int4(kO),0,0,SpaceText,lout)
        call OutTxt('n2',SpaceText(1:lout))
        call IntToStr(k2,0,0,SpaceText,lout)
        call OutTxt('m2',SpaceText(1:lout))
        ! call SleepQQ(1_4)
        if(lStopThread) Exit
        nP=tOBJ(kO)%nGrf
        mP=tOBJ(kO)%mGrf
        iP=tOBJ(kO)%iGrf
        i1=iP+1
        i2=iP+nP
        do k=1,mP
          call IntToStr(Int4(k),0,0,SpaceText,lout)
		      call OutTxt('n1',SpaceText(1:lout))
          call IntToStr(Int4(mP),0,0,SpaceText,lout)
		      call OutTxt('m1',SpaceText(1:lout))
          if(lStopThread) Exit
          if(iObjDra.lt.3) then
            ObjWebV(1:3,i1:i2)=0.0d0
            ObjWebF(i1:i2)=0.5d0
            Cycle
          end if
          do i=i1,i2
            r(1:3)=ObjWeb(1:3,i)
            if(iObjDra.eq.3) then ! ObjWebF=error
              r1(1:3)=ObjWebX(1:3,i)
              r2(1:3)=ObjWebY(1:3,i)
              rn(1:3)=ObjWebZ(1:3,i)
              call GetError(r,r1,r2,rn,iObjWeb(i),1.0d0,er,fi,idum)
              if(idum.ne.0) then
                er=0.0d0
              else
                if(errorScale.lt.0.0d0) then
                  er=-errorScale*er.div.fi
                else
                  er=errorScale*er
                end if
              end if
              ObjWebF(i)=er
              ObjWebV(1:3,i)=scaleArrow*er*rn(1:3)
            else if(iObjDra.eq.4) then ! ObjWebF,V=field
              call unPackDom(1.0d0,iObjWeb(i),iDl,iDr) ! iDl,iDr contain the domain numbers
              if(iDl.lt.0) call DistPtObj(0_2,0_2,ObjWeb(1:3,i),.true.,dmin,rNmin,iDl,val,.true.)
              if(iDr.lt.0) call DistPtObj(0_2,0_2,ObjWeb(1:3,i),.true.,dmin,rNmin,iDr,val,.true.)
              if(iDl.gt.0) then
                call GetLocrField(r,iDl,IntInter,r1)
                if(lf1) then
                  fi=r1(1)+r1(3)+r1(2)
                else
                  fi=r3Vec_Length(r1)
                end if
                ObjWebV(1:3,i)=r1(1:3)
                ObjWebF(i)=fi
              else
                ObjWebV(1:3,i)=0.0d0
                ObjWebF(i)=0.0d0
              end if
              if(iDr.gt.0) then
                call GetLocrField(r,iDr,IntInter,r1)
                if(lf1) then
                  fi=r1(1)+r1(3)+r1(2)
                else
                  fi=r3Vec_Length(r1)
                end if
                ObjWebV(1:3,i)=ObjWebV(1:3,i)+r1(1:3)
                ObjWebF(i)=ObjWebF(i)+fi
              end if
              if((iDl.ne.0).and.(iDr.ne.0)) then
                ObjWebV(1:3,i)=0.5d0*ObjWebV(1:3,i)
                ObjWebF(i)=0.5d0*ObjWebF(i)
              end if
            end if
          end do
          i1=i1+nP
          i2=i2+nP
        end do
      end do
    end if 
! points of derived field
    if(lObjFlg) then
      call GetEz(e)
      sclD=Max(dabs(rMinFld),dabs(rMaxFld))+1.0d-100
      sclD=Grid3D.div.sclD
      iObjWeb(nmP+1:nmPA)=-2_2
      ik=nmP
      call OutTxt('t2','Get derived field'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      do i=1,nyrFld
        call IntToStr(Int4(nyrFld),0,0,SpaceText,lout)
        call OutTxt('n2',SpaceText(1:lout))
        call IntToStr(i,0,0,SpaceText,lout)
        call OutTxt('m2',SpaceText(1:lout))
        ! call SleepQQ(1_4)
        if(lStopThread) Exit
        do k=1,nxrFld
          ik=ik+1
          ObjWeb(1:3,ik)=getrGrd(k,i)+sclD*rFld(0,k,i)*e
          ObjWebV(1:3,ik)=rFld(1:3,k,i)
          ObjWebF(ik)=rFld(0,k,i)
        end do
      end do
    end if 
! draw
    if((.not.lStopThread).and.(.not.lDrawOGL)) then
      call OutTxt('t1',' 'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
      call OutTxt('n2',' 'C)
      call OutTxt('m2',' 'C)
      if(lObjMat) then
        call OutTxt('t2','Draw Points'C)
        call draw3DMatPts(k1,k2,tOBJ(k1:k2)%nMat,tOBJ(k1:k2)%iColMin,tOBJ(k1:k2)%iColMax,tOBJ(k1:k2)%iCol)
      else
        call OutTxt('t2','Draw Web'C)
        call Draw3DWeb(k1,k2,tOBJ(k1:k2)%nGrf,tOBJ(k1:k2)%mGrf,tOBJ(k1:k2)%iGrf, &
        &              tOBJ(k1:k2)%iColMin,tOBJ(k1:k2)%iColMax,tOBJ(k1:k2)%iCol)
      end if
    end if
    call OutTxt('t2',' 'C)
    ic0=SetColor(ic0) ! old color
  end Subroutine DrawObject
  
  Subroutine Draw3DWeb(k1,k2,Ni,Mi,Ii,iCmin,iCmax,iCb)
! draw all grid points of 3D Objects k1...k2
! ObjWeb stores the grid points, iObjWeb the corresponding color numbers or generating boundary number
! the arrays Ni and Mi store the numbers of grid points in X and Y directions for all objects
! the array Ii stores the offset numbers of grids for all objects
! the arrays iCmin and iCmax store the fill colors for all objects (used if iC<0)
! the array iCb stores the color numbers for the grid lines for all objects (same as fill color if < 0)
    Implicit None
    Integer(4) k1,k2,nOb,kOb,Ni(*),Mi(*),Ii(*),i4(4),N,M,N1,M1,iErr,i,k,ik,ik0,NM1,NMtot,Ntot,idum
    Integer(2) iCmin(*),iCmax(*),iCb(*),iC,iC1,iCmi,iCma,iCbo
    Real(8), Allocatable :: Dist(:)
    Integer(4), Allocatable :: Indx(:)
    Real(8) r1(3),r2(3),rn(3),s,r4(3,4),f4(4),X,Y,dmin,rNmin(3),val(2)
    Logical lf1
    nOb=k2-k1+1
    if(.not.lgcFld) lGet3DMat=.true.
! lf1 determines how the scalar field f(1) is derived from the vector field f(2:4)
    if((itrFld.eq.itWe).or.(itrFld.eq.itWh).or.(itrFld.eq.itWt).or. &
    &  (itrFld.eq.itPe).or.(itrFld.eq.itPh).or.(itrFld.eq.itPt).or. &
    &  (itrFld.eq.itPe)) then
      lf1=.true.
    else
      lf1=.false.
      i=0
      if(lxrFld) i=i+1
      if(lyrFld) i=i+1
      if(lzrFld) i=i+1
      if(i.lt.2) lf1=.true.
    end if
    f4=0.0d0
! count web points and allocate memory
    NMtot=0 
    do kOb=1,nOb
      NMtot=NMtot+Ni(kOb)*Mi(kOb)
    end do
    Ntot=NMtot
    if(lObjFlg) Ntot=NMtot+nxrFld*nyrFld
    if(Allocated(Dist)) Deallocate(Dist)
    if(Allocated(Indx)) Deallocate(Indx)
    Allocate(Dist(Ntot),Indx(Ntot),Stat=iErr)
    if(iErr.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Draw 3D web'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! points of objects
    NM1=0
    do kOb=1,nOb
      if(lStopThread) Exit
      N=Ni(kOb)
      M=Mi(kOb)
      N1=N-1
      M1=M-1
      ik=Ii(kOb)
      do k=1,M ! get distances of web points from ViewPlane
        if(lStopThread) Exit
        do i=1,N
          ik=ik+1
          call Proj3D(ObjWeb(1:3,ik),ViewPlane,rEye,viewDist,X,Y,Dist(ik))
        end do
      end do
      ik=Ii(kOb)
      do k=1,M1 ! distances and index numbers of centers of quadrangles (save in first corner)
        if(lStopThread) Exit
        do i=1,N1
          ik=ik+1
          Dist(ik)=0.25d0*(Dist(ik)+Dist(ik+1)+Dist(ik+N)+Dist(ik+N+1))
          NM1=NM1+1
        end do
        ik=ik+1
        Dist(ik)=pBig
      end do
      if(lStopThread) Exit
      Dist(ik+1:ik+N)=pBig
    end do
! points of derived field
    if(lObjFlg) then
      ik=NMtot
      do i=1,nyrFld
        if(lStopThread) Exit
        do k=1,nxrFld
          ik=ik+1
          call Proj3D(ObjWeb(1:3,ik),ViewPlane,rEye,viewDist,X,Y,Dist(ik))
        end do
      end do
      ik=NMtot
      do i=1,nyrFld-1 ! distances and index numbers of centers of quadrangles (save in first corner)
        if(lStopThread) Exit
        do k=1,nxrFld-1
          ik=ik+1
          Dist(ik)=0.25d0*(Dist(ik)+Dist(ik+1)+Dist(ik+nxrFld)+Dist(ik+nxrFld+1))
        end do
        ik=ik+1
        Dist(ik)=pBig
      end do
      Dist(ik+1:ik+nxrFld)=pBig
    end if 
! sort
    if(.not.lStopThread) call QuickIndex(Dist,1,Ntot,Indx,.true.) 
! draw
    do ik0=1,Ntot
      if(lStopThread) Exit
      ik=Indx(ik0)
      if(Dist(ik0).gt.1.0d299) Cycle
      kOb=iGetObjNr(-ik)-k1+1 ! get object number
      if((kOb.le.nObj).and.(kOb.gt.0)) then ! number of lines in x direction
        N=Ni(kOb)
        iCbo=iCb(kOb)
        iC=iObjWeb(ik)-k1+1
        if(iC.eq.-30003_2) then
          iCma=iCmin(kOb)
          iCmi=iCmax(kOb)
          iC=-1_2
        else
          iCmi=iCmin(kOb)
          iCma=iCmax(kOb)
        end if
      else
        N=nxrFld
        iCmi=0_2
        iCma=235_2
        iCbo=1_2
        iC=-2_2
      end if
      iC1=iCbo
      i4(1)=ik
      i4(2)=ik+1
      i4(3)=ik+N+1
      i4(4)=ik+N
      r4(1:3,1:4)=ObjWeb(1:3,i4(1:4))
      if((lObjFlg.and.(iC.lt.-1_2).and.(kOb.gt.nObj)).or.(iObjDra.eq.3).or.(iObjDra.eq.4)) then ! draw error or field
        f4(1:4)=ObjWebF(i4(1:4))
        iC=-2_2
      else if(iObjDra.eq.1) then ! transparent
        iC=-1_2
      else
        r2=ObjWeb(1:3,ik+N+1)-ObjWeb(1:3,ik)
        r1=ObjWeb(1:3,ik+N)-ObjWeb(1:3,ik+1)
        rn=r3Vec_Prod(r1,r2)
        call Unit3DV(rn)
        r1=0.5d0*(ObjWeb(1:3,ik+N+1)+ObjWeb(1:3,ik))
        r2=rEye-r1
        call Unit3DV(r2)
        s=r3Scl_Prod(r2,rn)
        if(iObjDra.eq.2) then ! color represents domain
          call unPackDom(s,iC,iCmi,iCma) 
          if(iCmi.lt.0) call DistPtObj(0_2,0_2,ObjWeb(1:3,ik),.true.,dmin,rNmin,iCmi,val,.true.)
          iC=iDom(max(0_2,iCmi)) ! color represents domain
          if(iC1.lt.0) iC1=iC
        else ! color represents object
          if(iC.gt.0) then
            call unPackObjC(s,iCObjWeb(iC),iCmi,iCma) 
          else
            call unPackObjC(s,iCObjWeb(1),iCmi,iCma) 
          end if
          iC=max(0_2,iCmi)
        end if
      end if
      if(iC1.lt.0) iC1=1_2
      call Draw3DCell(r4,f4,iC1,iC)
    end do
    DeAllocate(Dist,Indx)
  end Subroutine Draw3DWeb 

  Subroutine draw3DMatPts(k1,k2,Ni,iCmin,iCmax,iCb)
! draw all matching points of 3D Objects k1...k2
! BndPt3D stores the matching points
! the array Ni stores the numbers of matching points for all objects
! the arrays iCmin and iCmax store the range of the fill colors for all objects (used if iC<0)
! the array iCb stores the color numbers for the grid lines for all objects (same as fill color if < 0)
    Implicit None
    Real(8), Allocatable :: Dist(:)
    Integer(4), Allocatable :: Indx(:)
    Real(8) r1(3),r2(3),rn(3),r4(1:3,1:4),f4(1:4),s,X,Y,ez(3),sclD,dmin,rNmin(3),val(2)
    Integer(4) k1,k2,kOb,nOb,Ni(*),N,iErr,i,k,ik,ik0,NMtot,Ntot,idum
    Integer(2) iCmin(*),iCmax(*),iCb(*),iC,iC1,iCmi,iCma,iCbo
    Logical lf1
    nOb=k2-k1+1
    if(.not.lgcFld) lGet3DMat=.true.
! lf1 determines how the scalar field f(1) is derived from the vector field f(2:4)
    if((itrFld.eq.itWe).or.(itrFld.eq.itWh).or.(itrFld.eq.itWt).or. &
    &  (itrFld.eq.itPe).or.(itrFld.eq.itPh).or.(itrFld.eq.itPt).or. &
    &  (itrFld.eq.itPe)) then
      lf1=.true.
    else
      lf1=.false.
      i=0
      if(lxrFld) i=i+1
      if(lyrFld) i=i+1
      if(lzrFld) i=i+1
      if(i.lt.2) lf1=.true.
    end if
    f4=0.0d0
! count points and allocate memory
    NMtot=0
    do kOb=1,nOb
      NMtot=NMtot+Ni(kOb)
    end do
    Ntot=NMtot
    if(lObjFlg) Ntot=NMtot+nxrFld*nyrFld
    if(Allocated(Dist)) Deallocate(Dist)
    if(Allocated(Indx)) Deallocate(Indx)
    Allocate(Dist(Ntot),Indx(Ntot),Stat=iErr)
    if(iErr.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Draw 3D points'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! points of objects
    do ik=1,NMtot ! get distances of points from ViewPlane
      if(lStopThread) Exit
      call Proj3D(BndPt3D(1:3,0,ik),ViewPlane,rEye,viewDist,X,Y,Dist(ik))
    end do
! points of derived field
    if(lObjFlg) then
      call GetEz(ez)
      sclD=Max(dabs(rMinFld),dabs(rMaxFld))+1.0d-100
      sclD=Grid3D.div.sclD
      iObjWeb(1:nxrFld*nyrFld)=-2_2
      if(.not.lStopThread) then
        ik=NMtot
        do i=1,nyrFld
          if(lStopThread) Exit
          do k=1,nxrFld
            ik=ik+1
            ObjWeb(1:3,ik-NMtot)=getrGrd(k,i)+sclD*rFld(0,k,i)*ez
            call Proj3D(ObjWeb(1:3,ik-NMtot),ViewPlane,rEye,viewDist,X,Y,Dist(ik))
          end do
        end do
        ik=NMtot
        do i=1,nyrFld-1 ! distances and index numbers of centers of quadrangles (save in first corner)
          if(lStopThread) Exit
          do k=1,nxrFld-1
            ik=ik+1
            Dist(ik)=0.25d0*(Dist(ik)+Dist(ik+1)+Dist(ik+nxrFld)+Dist(ik+nxrFld+1))
          end do
          ik=ik+1
          Dist(ik)=pBig
        end do
        Dist(ik+1:ik+nxrFld)=pBig
      end if 
    end if 
! sort
    if(.not.lStopThread) call QuickIndex(Dist,1,Ntot,Indx,.true.) ! sort
! draw
    do ik0=1,Ntot
      if(lStopThread) Exit
      ik=Indx(ik0)
      if(Dist(ik0).gt.1.0d299) Cycle
      kOb=iGetObjNr(ik)-k1+1 ! get object number
      if((kOb.le.nObj).and.(kOb.gt.0)) then ! number of lines in x direction
        N=Ni(kOb)
        iCmi=iCmin(kOb)
        iCma=iCmax(kOb)
        iCbo=iCb(kOb)
        iC=iBndPt3D(ik)
      else
        N=nxrFld
        iCmi=0_2
        iCma=235_2
        iCbo=1_2
        iC=-2_2
      end if
      if(lObjFlg.and.(iC.lt.-1_2).and.(kOb.gt.nObj)) then ! draw field point
        r4(1:3,1)=ObjWeb(1:3,ik-NMtot)
        r4(1:3,2)=ObjWeb(1:3,ik-NMtot+1)
        r4(1:3,3)=ObjWeb(1:3,ik-NMtot+N+1)
        r4(1:3,4)=ObjWeb(1:3,ik-NMtot+N)
        f4(1)=ObjWebF(ik-NMtot)
        f4(2)=ObjWebF(ik-NMtot+1)
        f4(3)=ObjWebF(ik-NMtot+N+1)
        f4(4)=ObjWebF(ik-NMtot+N)
        call  Draw3DCell(r4,f4,iCbo,iC)
        Cycle
      else if((iObjDra.eq.3).or.(iObjDra.eq.4)) then ! draw field or error on matching point
        r4(1:3,1)=BndPt3D(1:3,0,ik)-0.5d0*(BndPt3D(1:3,1,ik)+BndPt3D(1:3,2,ik))
        r4(1:3,2)=r4(1:3,1)+BndPt3D(1:3,1,ik)
        r4(1:3,3)=r4(1:3,2)+BndPt3D(1:3,2,ik)
        r4(1:3,4)=r4(1:3,3)-BndPt3D(1:3,1,ik)
        f4(1)=BndPt3DF(ik)
        f4(2:4)=f4(1)
        call  Draw3DCell(r4,f4,iCbo)
        Cycle
      else if(iObjDra.ne.1) then ! get orientation s -> fill color number
        rn=BndPt3D(1:3,3,ik)
        call Unit3DV(rn)
        r1=BndPt3D(1:3,0,ik)
        r2=rEye-r1
        call Unit3DV(r2)
        s=r3Scl_Prod(r2,rn)
        if(iObjDra.eq.0) then ! color represents object
          call unPackObjC(s,iC,iCmi,iCma) 
          iC=max(0_2,iCmi)
        else if(iObjDra.eq.5) then ! color represents number of associated expansions
          call unPackObjC(s,iC,iCmi,iCma) 
          iC=max(0_2,iCmi)
        else ! color represents domain
          call unPackDom(s,iC,iCmi,iCma)
          if(iCmi.lt.0) call DistPtObj(0_2,0_2,r1,.true.,dmin,rNmin,iCmi,val,.true.)
          iC=iDom(max(0_2,iCmi))
        end if
      end if
      iC1=iCbo
      if(iC1.lt.0) iC1=iC
      r4(1:3,1)=BndPt3D(1:3,0,ik)-0.5d0*(BndPt3D(1:3,1,ik)+BndPt3D(1:3,2,ik))
      r4(1:3,2)=r4(1:3,1)+BndPt3D(1:3,1,ik)
      r4(1:3,3)=r4(1:3,2)+BndPt3D(1:3,2,ik)
      r4(1:3,4)=r4(1:3,3)-BndPt3D(1:3,1,ik)
      if(iObjDra.eq.1) then ! transparent
        if(iC1.lt.0) iC1=1
        iC=-1_2
      end if
      call Draw3DCell(r4,f4,iC1,iC)
    end do
    DeAllocate(Dist,Indx)
  end Subroutine draw3DMatPts

  Subroutine drawPFDsensor(i1,i2,icl)
    Implicit None
    Integer(4) i1,i2,i,icl
    Integer(2) ic
    Real(8) r(3),r1(3)
    ic=SetColor(Int2(icl))
    do i=i1,i2
      r(1)=PFDsensX(i)-PFDdx
      r(2)=PFDsensY(i)
      r(3)=PFDsensZ(i)
      r1=r
      r1(1)=r1(1)+2.0d0*PFDdx
      call Draw3DLine(r,r1)
      r(1)=PFDsensX(i)
      r(2)=PFDsensY(i)-PFDdy
      r(3)=PFDsensZ(i)
      r1=r
      r1(2)=r1(2)+2.0d0*PFDdy
      call Draw3DLine(r,r1)
      r(1)=PFDsensX(i)
      r(2)=PFDsensY(i)
      r(3)=PFDsensZ(i)-PFDdx
      r1=r
      r1(3)=r1(3)+2.0d0*PFDdz
      call Draw3DLine(r,r1)
    end do
    ic=SetColor(ic)
  end Subroutine drawPFDsensor

  Subroutine drawPFDsource(i1,i2,icl)
    Implicit None
    Integer(4) i1,i2,i,icl
    Integer(2) ic
    Real(8) r(3),r1(3),r0(3)
    ic=SetColor(Int2(icl))
    do i=i1,i2
      r0(1)=PFDxmin+Dble(iPFDs(i)-1)*PFDdx
      r0(2)=PFDymin+Dble(jPFDs(i)-1)*PFDdy
      r0(3)=PFDzmin+Dble(kPFDs(i)-1)*PFDdz
      r=r0
      r(1)=r(1)-PFDdx
      r1=r0
      r1(1)=r1(1)+PFDdx
      call Draw3DLine(r,r1)
      r=r0
      r(2)=r(2)-PFDdy
      r1=r0
      r1(2)=r1(2)+PFDdy
      call Draw3DLine(r,r1)
      r=r0
      r(3)=r(3)-PFDdz
      r1=r0
      r1(3)=r1(3)+PFDdz
      call Draw3DLine(r,r1)
    end do
    ic=SetColor(ic)
  end Subroutine drawPFDsource

! auxiliaries

  Subroutine GetDxyz(ez,dx,dy,dz)
    Implicit None
    Real(8) ez(3),dx(3),dy(3),dz(3)
    if(lLimits) then
      if(iPlane.eq.3) then
        dx=spacecFld(1:3,1)
        dy=spacecFld(1:3,2)
        dz=ez*dble(levPlane-1)/dble(Max(1,nzcFld-1))
      else if(iPlane.eq.2) then
        dx=spacecFld(1:3,1)
        dy=spacecFld(1:3,3)
        dz=ez*dble(levPlane-1)/dble(Max(1,nycFld-1))
      else
        dx=spacecFld(1:3,2)
        dy=spacecFld(1:3,3)
        dz=ez*dble(levPlane-1)/dble(Max(1,nxcFld-1))
      end if
    else
      if(iPlane.eq.3) then
        dx=spacecFld(1:3,1)/dble(Max(1,nxrFld-1))
        dy=spacecFld(1:3,2)/dble(Max(1,nyrFld-1))
        dz=ez*dble(levPlane-1)/dble(Max(1,nzcFld-1))
      else if(iPlane.eq.2) then
        dx=spacecFld(1:3,1)/dble(Max(1,nxrFld-1))
        dy=spacecFld(1:3,3)/dble(Max(1,nyrFld-1))
        dz=ez*dble(levPlane-1)/dble(Max(1,nycFld-1))
      else
        dx=spacecFld(1:3,2)/dble(Max(1,nxrFld-1))
        dy=spacecFld(1:3,3)/dble(Max(1,nyrFld-1))
        dz=ez*dble(levPlane-1)/dble(Max(1,nxcFld-1))
      end if
    end if
  end Subroutine GetDxyz

  Subroutine Get3DCell(k,j,rp,f,dx0,dy0,ez0,sclD0,iGet)
! get location rp of the four corners and the corresponding field values f of the cell at k,j of the field grid
! iGet<0: dx,dy,dz,ez,sclD known input data
! iGet=0: dx,dy,dz,ez,sclD unknown input data, compute, but don't return values
! iGet>0: dx,dy,dz,ez,sclD unknown input data, compute and return values
    Implicit None
    Real(8) rp(3,4),f(4),dx0(3),dy0(3),ez0(3),sclD0,dx(3),dy(3),dz(3),ez(3),sclD,r(3),scl
    Integer(4) k,j,iGet
    if(iGet.ge.0) then
      call GetEz(ez)
      call GetDxyz(ez,dx,dy,dz)
      scl=Max(dabs(rMinFld),dabs(rMaxFld))+1.0d-100
      sclD=Grid3D.div.scl
      if(iGet.gt.0) then
        ez0=ez
        dx0=dx
        dy0=dy
        sclD0=sclD
      end if
    else
      ez=ez0
      dx=dx0
      dy=dy0
      sclD=sclD0
    end if
    r=getrGrd(k,j)
    if(abs(itIntensity).eq.1) then
      if(lrGrd) then
        rp(1:3,1)=r-0.5d0*(dx+dy)+sclD*rFld(0,k,j)*ez
        rp(1:3,2)=rp(1:3,1)+dx
        rp(1:3,3)=rp(1:3,1)+dx+dy
        rp(1:3,4)=rp(1:3,1)+dy
      else
        r=sclD*rFld(0,k,j)*ez
        rp(1:3,1)=0.5d0*(getrGrd(k,j)+getrGrd(k-1,j-1))+r
        rp(1:3,2)=0.5d0*(getrGrd(k,j)+getrGrd(k+1,j-1))+r
        rp(1:3,3)=0.5d0*(getrGrd(k,j)+getrGrd(k+1,j+1))+r
        rp(1:3,4)=0.5d0*(getrGrd(k,j)+getrGrd(k-1,j+1))+r
      end if
      f(1)=rFld(0,k,j)
      f(2:4)=f(1)
    else
      if(lrGrd) then
        rp(1:3,1)=r+sclD*rFld(0,k,j)*ez
        rp(1:3,2)=r+dx+sclD*rFld(0,k+1,j)*ez
        rp(1:3,3)=r+dx+dy+sclD*rFld(0,k+1,j+1)*ez
        rp(1:3,4)=r+dy+sclD*rFld(0,k,j+1)*ez
      else
        rp(1:3,1)=getrGrd(k,j)+sclD*rFld(0,k,j)*ez
        rp(1:3,2)=getrGrd(k+1,j)+sclD*rFld(0,k+1,j)*ez
        rp(1:3,3)=getrGrd(k+1,j+1)+sclD*rFld(0,k+1,j+1)*ez
        rp(1:3,4)=getrGrd(k,j+1)+sclD*rFld(0,k,j+1)*ez
      end if
      f(1)=rFld(0,k,j)
      f(2)=rFld(0,k+1,j)
      f(3)=rFld(0,k+1,j+1)
      f(4)=rFld(0,k,j+1)
    end if
  end Subroutine Get3DCell

  Subroutine MTModifyField(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    Logical ldum
    if(.not.lCheck) then
      idum=MessageBoxQQ('Set view plane = xy plane?'C,'Modify field'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) return
      if(idum.eq.MB$IDYES) then
        viewPlane(1:3,1:3)=0.0d0 ! view plane = xy, (origin unchanged)
        viewPlane(3,0)=0.0d0
        viewPlane(1,1)=1.0d0
        viewPlane(2,2)=1.0d0
        viewPlane(3,3)=1.0d0
      end if
    end if
    iWinAction=2_2
    call MTDrawField(lCheck)
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuChecked)   !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuUnChecked) !nothing
		call OutTxt('t1','Modify field!'C)
  end Subroutine MTModifyField

END MODULE CHFLG




