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
MODULE CHEXP

! Expansions

  USE AALYR

  SAVE

  CONTAINS

! Graphics

  Subroutine TDrawExpansion(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    ldum=lCheck
    call WaitEndThread()
    iThreadAction=4
    call StartEXPThread(ldum)
  end Subroutine TDrawExpansion

  Subroutine StartEXPThread(ldi)
! start a new thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical(4), intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start expansion thread'C)
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
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start expansion thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start expansion thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(EXPThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start expansion thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
      return
    end if
  end Subroutine StartEXPThread

  Integer(4) Function EXPThread(iWhat)
! tread calls.....
    Implicit none
    Include 'resource.fd'
    Integer(4) iWhat
    Logical(4) ldum
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    EXPThread=0_4
    if(iThreadAction.eq.4) then
      call MTDrawExpansion(ldum)
    end if
    call endThread()
  end Function EXPThread

  Subroutine DrawExpansion(kEx)
    Implicit none
    Real(8) x,y,r,r0,ra(3),rb(3),Pl(3,0:3),a
    Integer(4) kEx,k1,k2,kE
    Integer(2) ic0,ic,nP
    lWinFld(kWin)=.true.
! set window, auto determine borders
    call GetKWin(.false.)
    k1=SetActiveQQ(10+kWin)
! get pixel size
    call I2R(10_2,10_2,x,r)
    call I2R(11_2,10_2,y,r)
    r=4.0d0*Dabs(y-x)
! determine min/max expansion numbers
    k1=1
    k2=nExp
    if((kEx.gt.0).and.(kEx.le.nExp)) then
      k1=kEx
      k2=k1
    end if
! loop over expansions
    ic0=SetColor(1_2)
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      ic=SetColor(tExp(kE)%iCol)
      Select Case(tExp(kE)%iTypE)
      Case(0) ! Connection
        call Draw3DP(tExp(kE)%Plane(1:3,0),r)
      Case(1,2) ! 2d Hankel or Bessel
        if(lgcFld) then
          x=tExp(kE)%Plane(1,0)+2.0d0*r*dcosd(tExp(kE)%rE(1))
          y=tExp(kE)%Plane(2,0)+2.0d0*r*dsind(tExp(kE)%rE(1))
          call Draw23DLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),x,y)
          if(tExp(kE)%iTypE.eq.1) then
            call Draw23DX(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),r)
            if((abs(tExp(kE)%rE(3))+abs(tExp(kE)%rE(4))).gt.pSmall) then ! complex origin multipole
              ra(1)=-tExp(kE)%rE(4)
              ra(2)=tExp(kE)%rE(3)
              a=tExp(kE)%rE(1)*Pi/180.0d0
              rb(1)=ra(1)*cos(a)-ra(2)*sin(a)
              rb(2)=ra(1)*sin(a)+ra(2)*cos(a)
              ra(1:2)=tExp(kE)%Plane(1:2,0)+rb(1:2)
              rb(1:2)=tExp(kE)%Plane(1:2,0)-rb(1:2)
              call Draw23DLine(ra(1),ra(2),rb(1),rb(2))
              r0=dsqrt(tExp(kE)%rE(3)**2+tExp(kE)%rE(4)**2)
              a=(tExp(kE)%rE(1)+tExp(kE)%rE(5))*Pi/180.0d0+datan2(-tExp(kE)%rE(3),tExp(kE)%rE(4))
              call Draw23DLine(ra(1),ra(2),ra(1)+r0*cos(a),ra(2)+r0*sin(a))
              a=(tExp(kE)%rE(1)-tExp(kE)%rE(5))*Pi/180.0d0+datan2(-tExp(kE)%rE(3),tExp(kE)%rE(4))
              call Draw23DLine(rb(1),rb(2),rb(1)-r0*cos(a),rb(2)-r0*sin(a))
            end if
          else
            call Draw23DQ(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),r)
            if((abs(tExp(kE)%rE(3))+abs(tExp(kE)%rE(4))).gt.pSmall) then ! complex origin Bessel
              ra(1)=-tExp(kE)%rE(4)
              ra(2)=tExp(kE)%rE(3)
              a=tExp(kE)%rE(1)*Pi/180.0d0
              rb(1)=ra(1)*cos(a)-ra(2)*sin(a)
              rb(2)=ra(1)*sin(a)+ra(2)*cos(a)
              ra(1:2)=tExp(kE)%Plane(1:2,0)+rb(1:2)
              rb(1:2)=tExp(kE)%Plane(1:2,0)-rb(1:2)
              call Draw23DLine(ra(1),ra(2),rb(1),rb(2))
            end if
          end if
        else
          if(tExp(kE)%iTypE.eq.1) then
            call Draw3DX(tExp(kE)%Plane(1:3,0),r)
          else
            call Draw3DQ(tExp(kE)%Plane(1:3,0),r)
          end if
        end if
      Case(3,11) ! Plane wave and Gaussian beam
        if(lgcFld) then
          x=tExp(kE)%Plane(1,0)+2.0d0*r*dcosd(tExp(kE)%rE(1))
          y=tExp(kE)%Plane(2,0)+2.0d0*r*dsind(tExp(kE)%rE(1))
          call Draw23DLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),x,y)
          call Draw23DO(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),r)
        else
          call Draw3DP(tExp(kE)%Plane(1:3,0),r)
        end if
      Case(4) ! Rayleigh
        if(lgcFld) then
          x=tExp(kE)%Plane(1,0)+2.0d0*r*dcosd(tExp(kE)%rE(1))
          y=tExp(kE)%Plane(2,0)+2.0d0*r*dsind(tExp(kE)%rE(1))
          call Draw23DLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),x,y)
          call Draw23DO(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),r)
        else
          call Draw3DP(tExp(kE)%Plane(1:3,0),r)
        end if
      Case(5) ! Harmonic
        if(lgcFld) then
          x=tExp(kE)%Plane(1,0)+2.0d0*r*dcosd(tExp(kE)%rE(1))
          y=tExp(kE)%Plane(2,0)+2.0d0*r*dsind(tExp(kE)%rE(1))
          call Draw23DLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),x,y)
          call Draw23DO(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),r)
        else
          call Draw3DP(tExp(kE)%Plane(1:3,0),r)
        end if
      Case(6,7) ! 3d Hankel or Bessel
        if(tExp(kE)%iTypE.eq.6) then
          call Draw3DX(tExp(kE)%Plane(1:3,0),r)
        else
          call Draw3DQ(tExp(kE)%Plane(1:3,0),r)
        end if
        if((abs(tExp(kE)%rE(2))+abs(tExp(kE)%rE(3))+abs(tExp(kE)%rE(4))).gt.pSmall) then ! complex origin multipole
          Pl(1:3,0)=0.0d0
          Pl(1:3,3)=tExp(kE)%rE(2:4)
          call Unit3DV(Pl(1:3,3),r0)
          ra(1)=0.23417123098d0 ! some random vector
          ra(2)=-0.76327567541d0
          ra(3)=0.51234325468d0
          Pl(1:3,1)=r3Vec_Prod(ra,Pl(1:3,3))
          call Unit3DV(Pl(1:3,1))
          Pl(1:3,2)=r3Vec_Prod(Pl(1:3,3),Pl(1:3,1))
          call sLoc2Glob(Pl,tExp(kE)%Plane(1:3,0:3),Pl)
          nP=36
          call Draw3DArc(Pl,r0,0.0d0,360.0d0,nP,.false.,tExp(kE)%iCol,tExp(kE)%iCol)
          ra(1:3)=tExp(kE)%Plane(1:3,0)
	        call rvLoc2Glob(tExp(kE)%rE(2:4),tExp(kE)%Plane(1:3,0:3),rb(1:3))
          rb(1:3)=ra(1:3)+rb(1:3)
          call Draw3DLine(ra,rb)
        end if
      Case(8) ! 3d Ring
        call Draw3DX(tExp(kE)%Plane(1:3,0),r)
        Pl(1:3,0)=tExp(kE)%Plane(1:3,0)
        Pl(1:3,1)=tExp(kE)%Plane(1:3,1)
        Pl(1:3,2)=tExp(kE)%Plane(1:3,3)
        Pl(1:3,3)=-tExp(kE)%Plane(1:3,2)
        nP=Nint(min(1000.0d0,max(3.0d0,(abs(tExp(kE)%rE(3))*0.101d0))),2)
        call Draw3DArc(Pl,tExp(kE)%rE(1),tExp(kE)%rE(2),tExp(kE)%rE(2)+abs(tExp(kE)%rE(3)), &
        &              nP,.false.,tExp(kE)%iCol,tExp(kE)%iCol)
      Case(9) ! 3d Line
        call Draw3DX(tExp(kE)%Plane(1:3,0),r)
        ra(1:3)=tExp(kE)%Plane(1:3,0)+tExp(kE)%rE(1)*tExp(kE)%Plane(1:3,3)
        rb(1:3)=ra(1:3)+tExp(kE)%rE(2)*tExp(kE)%Plane(1:3,3)
        call Draw3DLine(ra,rb)
      Case(10) ! 3d Spiral
        call Draw3DX(tExp(kE)%Plane(1:3,0),r)
        nP=Nint(min(1000.0d0,max(3.0d0,(abs(tExp(kE)%rE(2))*0.101d0))),2)
        ra(1)=tExp(kE)%xo
        ra(2)=tExp(kE)%yo
        ra(3)=0.0d0
        call Draw3DSpiral(tExp(kE)%Plane,tExp(kE)%O,tExp(kE)%e,ra,tExp(kE)%rE(1),tExp(kE)%rE(2), &
        & tExp(kE)%rE(3),nP,.false.,tExp(kE)%iCol,tExp(kE)%iCol)
      Case(12,13) ! Multilayer expansions
        if(lgcFld) then
          call Draw23DX(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),r)
        else
          call Draw3DX(tExp(kE)%Plane(1:3,0),r)
        end if
      end Select
    end do
    ic0=SetColor(ic0)
  end Subroutine DrawExpansion

  Subroutine MTDrawExpansion(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
   ! iWinAction=0_2
    lWinFld(kWin)=.true.
    if(.not.lCheck) then
      idum=MessageBoxQQ('Clear graphic window first?'C,'Draw expansion'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) then
        call LeaveCriticalSection(Loc(DrawLock))
        return
      end if
      if(idum.eq.MB$IDYES) then
        call DrawWindow(lCheck)
      end if
    end if
    call DrawExpansion(iDraExp)
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTDrawExpansion

  Subroutine MTModifyExpansion(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    Integer(4) idum
    Logical(4) ldum
    if(.not.lCheck) then
      idum=MessageBoxQQ('Set view plane = xy plane?'C,'Modify expansion'C, &
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
    iDraBnd=0
    iDraExp=0
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    iWinAction=5_2
    call MTDrawExpansion(lCheck)
    call MTDrawBoundary(.true.)
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuChecked)   !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuUnChecked) !nothing
		call OutTxt('t1','Modify expansion!'C)
  end Subroutine MTModifyExpansion

! I/O

  Subroutine SaveExpansion(lCheck)
! save Expansion data in a file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) iOk,ios,i,idum,j,k
    if(.not.lCheck) then
      call Open2write(-1,'Select Expansion data file to be written!','Expansion data file ',ExpFileName,'EXP',ios)
      if(ios.gt.0) return
    end if
    open(1,file=ExpFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save expansion'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHExpIdent,iOK)
    ich(1)=nExp
    ich(2)=nRHS
    rch(1)=Dble(fcFld)
    rch(2)=DImag(fcFld)
    call chwrit2(1,ich,2,rch,2,sch,0,iOK)
    do i=1,nExp
      ich(1)=Int4(tExp(i)%iDom)
      ich(2)=Int4(tExp(i)%iTypE)
      ich(3)=Int4(tExp(i)%iHE)
      ich(4)=Int4(tExp(i)%iConn)
      ich(5)=Int4(tExp(i)%iCol)
      ich(6)=Int4(tExp(i)%iObj)
      call chwrit2(1,ich,6,rch,0,sch,0,iOK)
      ich(1:6)=tExp(i)%iE(1:6)
      call chwrit2(1,ich,6,rch,0,sch,0,iOK)
      rch(1:5)=tExp(i)%rE(1:5)
      call chwrit2(1,ich,0,rch,5,sch,0,iOK)
      rch(1)=Dble(tExp(i)%gc)
      rch(2)=Dimag(tExp(i)%gc)
      call chwrit2(1,ich,0,rch,2,sch,0,iOK)
      rch(1)=tExp(i)%xo
      rch(2)=tExp(i)%yo
      call chwrit2(1,ich,0,rch,2,sch,0,iOK)
      rch(1:3)=tExp(i)%Plane(1:3,0)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tExp(i)%Plane(1:3,1)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tExp(i)%Plane(1:3,2)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tExp(i)%Plane(1:3,3)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tExp(i)%O(1:3)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tExp(i)%e(1:3)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      ich(1)=tExp(i)%nPar
      call chwrit2(1,ich,1,rch,0,sch,0,iOK)
      do k=1,nRHS
        do j=1+tExp(i)%iOff,tExp(i)%iOff+tExp(i)%nPar
          ich(1)=Int4(iParExp(k,j))
          rch(1)=Dble(ParExp(k,j))
          rch(2)=DImag(ParExp(k,j))
          call chwrit2(1,ich,1,rch,2,sch,0,iOK)
        end do
      end do
    end do
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveExpansion

  Subroutine OpenExpansion(lCheck)
! read Expansion data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist,lA,lVers2,lVers3,lVers4
    Integer(4) iOk,ios,i,i1,idum,j,k,nP
    Character(20) text
    if(lAskExp.or.(.not.lCheck)) then
      nInsObj=nExp
      call InsertDialog(.true.)
      if(kInsObj.lt.0) return
      lInsertExp=lInsObj
    end if
    if(.not.lCheck) then
      call Open2read(-1,'Select Expansion data file to be read!','Expansion data file ',ExpFileName,'EXP',ios)
      if(ios.gt.0) return
    end if
    inquire(file=ExpFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=ExpFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open expansion'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHExpIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open expansion'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    lVers2=.false.
    lVers3=.false.
    lVers4=.false.
    if(text(16:16).eq.'2') lVers2=.true.
    if(text(16:16).eq.'3') then
      lVers2=.true.
      lVers3=.true.
      if(text(18:18).eq.'3') then
        lVers4=.true.
      end if
    end if
    lA=.false.
    i=GetSLength(text)
    if((i.gt.18).or.lVers3) lA=.true.
    if(lVers4) then
      call chread2(1,ich,2,rch,0,iOK)
      if(lInsertExp.and.(ich(2).ne.nRHS)) then
        idum=MessageBoxQQ('Cannot insert input file!\r(wrong number of excitations)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      if(nRHS.ne.ich(2)) then
        nRHS=ich(2)
        call AllocatePar(ldum)
      end if
      kExc=max(1,min(kExc,nRHS))
    else
      call chread2(1,ich,1,rch,0,iOK)
      nRHS=1
      kExc=1
    end if
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of Expansions)'C,'Open expansion'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nInsObj=ich(1)
    if(lInsertExp) then
      i=kInsObj
    else
      i=0
      nExp=0
      nPar=0
    end if
    do i1=1,nInsObj
      if(lVers2) then
        call chread2(1,ich,6,rch,0,iOK)
      else
        call chread2(1,ich,5,rch,0,iOK)
        ich(6)=0
      end if
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      call InsertExp(i,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert expansion!'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      i=i+1
      tExp(i)%iDom=Int2(ich(1))
      tExp(i)%iTypE=Int2(ich(2))
      tExp(i)%iHE=Int2(ich(3))
      tExp(i)%iConn=Int2(ich(4))
      tExp(i)%iCol=Int2(ich(5))
      tExp(i)%iObj=Int2(ich(6))
      if(lVers2) then
        call chread2(1,ich,6,rch,0,iOK)
        if(.not.lA) then
          if(tExp(i)%iTypE.eq.9_2) tExp(i)%iTypE=8_2
          if(tExp(i)%iTypE.eq.10_2) tExp(i)%iTypE=9_2
          if(tExp(i)%iTypE.eq.11_2) tExp(i)%iTypE=9_2
          if(tExp(i)%iTypE.eq.12_2) tExp(i)%iTypE=10_2
          if(tExp(i)%iTypE.eq.13_2) tExp(i)%iTypE=10_2
          if(tExp(i)%iTypE.eq.14_2) tExp(i)%iTypE=11_2
        end if
      else
        call chread2(1,ich,4,rch,0,iOK)
        ich(5)=1
        ich(6)=0
      end if
      if(.not.lVers3) then
        if(tExp(i)%iTypE.eq.0_2) tExp(i)%iE(2:4)=-1_2
      end if
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      tExp(i)%iE(1:6)=ich(1:6)
      if(lA.or.lVers2) then
        call chread2(1,ich,0,rch,5,iOK)
      else
        call chread2(1,ich,0,rch,4,iOK)
        rch(5)=0.0d0
      end if
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      tExp(i)%rE(1:5)=rch(1:5)
      tExp(i)%depend=1.0d100
      tExp(i)%gc=gcFld
      if(lVers2) then
        call chread2(1,ich,0,rch,2,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        tExp(i)%gc=Dcmplx(rch(1),rch(2))
        call chread2(1,ich,0,rch,2,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        tExp(i)%xo=rch(1)
        tExp(i)%yo=rch(2)
      end if
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      if(.not.lVers2) then
        tExp(i)%xo=rch(1)
        tExp(i)%yo=rch(2)
      end if
      tExp(i)%Plane(1:3,0)=rch(1:3)
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(i)%xo=tExp(i)%Plane(1,0)
        tExp(i)%yo=tExp(i)%Plane(2,0)
      end if
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      tExp(i)%Plane(1:3,1)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      tExp(i)%Plane(1:3,2)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      tExp(i)%Plane(1:3,3)=rch(1:3)
      if(lVers2) then
        call chread2(1,ich,0,rch,3,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        tExp(i)%O(1:3)=rch(1:3)
        call chread2(1,ich,0,rch,3,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Expansion data)'C,'Open expansion'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        tExp(i)%e(1:3)=rch(1:3)
        call Unit3DV(tExp(i)%e(1:3))
      else
        tExp(i)%O(1:3)=0.0d0
        tExp(i)%e(1:3)=0.0d0
        tExp(i)%e(1)=1.0d0
      end if
      if(.not.lVers3) then ! no complex origin multipoles, no planewave superpositions...
        select case(tExp(i)%iTypE)
        case(1,2)
          tExp(i)%rE(3:4)=0.0d0
        case(3)
          tExp(i)%rE(2:3)=0.0d0
          tExp(i)%iE(2:3)=0
        case(6,7)
          tExp(i)%rE(2:5)=0.0d0
        end select
      end if
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(number of parameters)'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      tExp(i)%nPar=0
      call InsertPar(i,0,max(1,ich(1)),ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert parameter!'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nP=ich(1)
      do k=1,nRHS
        do j=tExp(i)%iOff+1,tExp(i)%iOff+nP
          call chread2(1,ich,1,rch,2,iOK)
          if(iOK.ne.0) then
            idum=MessageBoxQQ('Error reading input file!\r(Parameter data)'C,'Open expansion'C, &
                              MB$OK.or.MB$ICONSTOP)
            close(1)
            return
          end if
          iParExp(k,j)=Int2(ich(1))
          ParExp(k,j)=DCmplx(rch(1),rch(2))
        end do
      end do
      if(.not.lVers2) then
        ich(1:6)=tExp(i)%iE(1:6)
        if((tExp(i)%iTypE.eq.1).or.(tExp(i)%iTypE.eq.2)) then ! 2d Hankel or Bessel
          ich(5)=max(1,ich(2))
          ich(2)=ich(1)+ich(5)*tExp(i)%nPar
        else if(tExp(i)%iTypE.eq.4) then ! Rayleigh
          ich(2)=tExp(i)%nPar
          ich(3:5)=0
        else if(tExp(i)%iTypE.eq.5) then ! Harmonic
          ich(5)=max(1,ich(4))
          ich(4)=ich(3)+ich(5)*tExp(i)%nPar
        end if
        tExp(i)%iE(1:6)=ich(1:6)
      end if
    end do
    close(1)
    call CorrExpPar(1000_4)
    kExp=nExp
    kPar=nPar
  end Subroutine OpenExpansion

  Subroutine OpenParameter(lCheck)
! read Parameter data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist
	  Integer(4) iOk,ios,j,n,idum
    if(.not.lCheck) then
      call Open2read(-1,'Select Parameter data file to be read!','Parameter data file ',ParFileName,'PAR',ios)
      if(ios.gt.0) return
    end if
    inquire(file=ParFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=ParFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open parameters'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of parameters)'C,'Open parameter'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    n=ich(1)
    if(n.gt.nPar) then
      idum=MessageBoxQQ('Error reading input file!\r(wrong number of parameters)'C,'Open parameter'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    do j=1,n
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(parameter values)'C,'Open parameter'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      ParExp(kExc,j)=DCmplx(rch(1),rch(2))
    end do
    close(1)
  end Subroutine OpenParameter

! Expansion operations

  Subroutine CorrExpPar(m,i1,i2)
! set correct number of parameters for all expansions
    Implicit none
    Integer(4), optional:: i1,i2
    Integer(4) k,m,k1,k2
    k=kExP
    if(.not.Present(i2)) then
      k1=1
      k2=nExp-nExc
    else
      k1=i1
      k2=i2
    end if
    do kExp=k1,k2
      call setOrient(kExp)
      call RepExpTest(m)
    end do
    kExp=k
  end Subroutine CorrExpPar

  Subroutine RepExpTyp(kP0)
! replace the expansion type -> insert/delete and reset parameters if neccessary
    Implicit none
    Integer(4) nP,kP,kP0,idum,lout
    Logical ldum
    kP=kP0
    if(kP.lt.1) then
      call IntToStr(kExp,0,0,SpaceText,lout)
      if(kExp.ne.nExp) then
        idum=MessageBoxQQ('Expansion '//SpaceText(1:lout)//' without any parameter!\rDelete expansion?'C, &
                          'Expansion check'C,MB$YESNO.or.MB$ICONQUESTION)
        if(idum.eq.MB$IDYES) then
          call InsertExp(kExp,-1,ldum)
          kExp=min(kExp,nExp)
          return
        end if
      else
        idum=MessageBoxQQ('Excitation without any parameter!\rModify expansion or symmetry data!'C,'Expansion check'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
      kP=1
    end if
    if(tExp(kExp)%nPar.eq.kP) return
    nP=tExp(kExp)%nPar
    call InsertPar(kExp,1,kP-nP,ldum)   ! insert/delete parameters
    if(ldum) then
      ParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)=(0.0d0,0.0d0)
      iParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)=0_2
    end if
  end Subroutine RepExpTyp

  Subroutine RepExpTest(kP0)
    Implicit none
    Complex(8), Allocatable:: AExp(:,:)
    Integer(4) kP0,iOK,idum,nP
    Real(8) r(3)
    Complex(8), allocatable:: PE(:,:)
    Integer(2), allocatable:: iPE(:,:)
    nP=tExp(kExp)%nPar
    Allocate(PE(nRHS,nP),iPE(nRHS,nP),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for expansion check failed!'C,'RepExpTest'C, &
                        MB$OK.or.MB$IconExclamation)
    else
      PE(1:nRHS,1:nP)=ParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)
      iPE(1:nRHS,1:nP)=iParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)
    end if
    call RepExpTyp(kP0)
    r(1)=1.234567890987d0
    r(2)=1.123456789012d0
    r(3)=1.011234567899d0
    r=(3.0d8/Min(1.0d100,Max(1.0d-100,CDAbs(fcFld))))*r
    lExpTest=.true.
    if(Allocated(AExp)) DeAllocate(AExp)
    Allocate(AExp(10,nParN),stat=iOK)
    if(tExp(kExp)%iTypE.ge.12) tExp(kExp)%iDom=0_2 ! multidomain expansion: set iDom always 0
    if(iOK.eq.0) call GetArExp(kExp,r,tExp(kExp)%iDom,iHEGlobal,-1_2,-1_2,-1_2,AExp)
    if(Allocated(AExp)) DeAllocate(AExp)
    if(nP.eq.tExp(kExp)%nPar) then
      ParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)=PE(1:nRHS,1:nP)
      iParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)=iPE(1:nRHS,1:nP)
    end if
    DeAllocate(PE,iPE,stat=idum)
    lExpTest=.false.
  end Subroutine RepExpTest

  Subroutine GetArrExp(kE,rE,iD,iHE,ArrExp)
! compute ArrExp of expansion kE at location rE in domain iD - all expansions if kE out of range
! apply field type filter iHE, move rE into the periodic cell if the problem is periodic
    Implicit none
    Complex(8), Allocatable:: AExp(:,:)
    Complex(8) ArrExp(10,nParN)
    Real(8) rE(3),rO(3),rg(3)
    Integer(4) kE,k,i1,i2,k1,k2,iOK,it,ixy,ixz,iyz
    Integer(2) iD,iHE
    Character(16) text
    k1=1
    k2=nExp
    if((kE.gt.0).and.(kE.le.nExp)) then
      k1=kE
      k2=k1
    else if((kE.lt.0).and.(-kE.le.nExp)) then
      k1=1
      k2=-kE
    end if
    rO=rE
    call MoveToOrigCell(rO)
    ArrExp(1:10,1:nParN)=(0.0d0,0.0d0)
    if(iD.eq.0_2) return
!!$OMP parallel private(k) private(AExp) shared(ArrExp)
!!$OMP  do
    do k=k1,k2
      if(LSkipExp(k)) Cycle
      if(igetiHE2(iHE,k).lt.0_2) Cycle
      if(tExp(k)%iObj.lt.0) Cycle
      rg=rO  
      if(Allocated(AExp)) DeAllocate(AExp)
      Allocate(AExp(10,nParN),stat=iOK)
      if(iOK.eq.0) then
        call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
        i1=tExp(k)%iOff+1
        i2=tExp(k)%iOff+tExp(k)%nPar
        ArrExp(1:10,i1:i2)=AExp(1:10,i1:i2)
        it=111 ! perform all symmetry operations
        if(((tExp(k)%iTypE.eq.6).and.(tExp(k)%rE(1).lt.nSmall)).or. & !!! No symmetry for 3D multipoles with negative rmin
        &  ((tExp(k)%iTypE.eq.7).and.(tExp(k)%rE(1).lt.nSmall))) then !!! No symmetry for 3D Bessel with negative rmax
          write(text,'(1PE11.3E3)') tExp(k)%rE(1)
          call StrToInt(text(4:6),it,iOK)
        end if
        if(((tExp(k)%iTypE.eq.1).and.(tExp(k)%rE(2).lt.nSmall)).or. & !!! No symmetry for 2D multipoles with negative rmin
        &  ((tExp(k)%iTypE.eq.2).and.(tExp(k)%rE(2).lt.nSmall))) then !!! No symmetry for 2D Bessel with negative rmax
          write(text,'(1PE11.3E3)') tExp(k)%rE(2)
          call StrToInt(text(4:6),it,iOK)
        end if
        if(((tExp(k)%iTypE.eq.9).and.(tExp(k)%rE(4).lt.nSmall))) then !!! No symmetry for line multipoles with negative rmin
          write(text,'(1PE11.3E3)') tExp(k)%rE(4)
          call StrToInt(text(4:6),it,iOK)
        end if
        if(((tExp(k)%iTypE.eq.8).and.(tExp(k)%rE(5).lt.nSmall)).or. & !!! No symmetry for ring multipoles with negative rmin
        &  ((tExp(k)%iTypE.eq.10).and.(tExp(k)%rE(5).lt.nSmall))) then !!! No symmetry for spiral multipoles with negative rmin
          write(text,'(1PE11.3E3)') tExp(k)%rE(5)
          call StrToInt(text(4:6),it,iOK)
        end if
        ixy=it/100
        it=it-100*ixy
        ixz=it/10
        iyz=it-10*ixz
        if((ixySymm.ne.0).and.(ixy.ne.0)) then
          rg(1)=rO(1)
          rg(2)=rO(2)
          rg(3)=-rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call xySymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
        if((ixzSymm.ne.0).and.(ixz.ne.0)) then
          rg(1)=rO(1)
          rg(2)=-rO(2)
          rg(3)=rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call xzSymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
        if((iyzSymm.ne.0).and.(iyz.ne.0)) then
          rg(1)=-rO(1)
          rg(2)=rO(2)
          rg(3)=rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call yzSymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
        if((ixySymm.ne.0).and.(ixzSymm.ne.0).and.(ixy.ne.0).and.(ixz.ne.0)) then
          rg(1)=rO(1)
          rg(2)=-rO(2)
          rg(3)=-rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call xySymm(i1,i2,AExp(1:10,1:i2))
          call xzSymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
        if((ixySymm.ne.0).and.(iyzSymm.ne.0).and.(ixy.ne.0).and.(iyz.ne.0)) then
          rg(1)=-rO(1)
          rg(2)=rO(2)
          rg(3)=-rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call xySymm(i1,i2,AExp(1:10,1:i2))
          call yzSymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
        if((ixzSymm.ne.0).and.(iyzSymm.ne.0).and.(ixz.ne.0).and.(iyz.ne.0)) then
          rg(1)=-rO(1)
          rg(2)=-rO(2)
          rg(3)=rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call xzSymm(i1,i2,AExp(1:10,1:i2))
          call yzSymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
        if((ixySymm.ne.0).and.(ixzSymm.ne.0).and.(iyzSymm.ne.0).and.(ixy.ne.0).and.(ixz.ne.0).and.(iyz.ne.0)) then
          rg(1)=-rO(1)
          rg(2)=-rO(2)
          rg(3)=-rO(3)
          call GetArExp(k,rg,iD,iHE,-1_2,-1_2,-1_2,AExp)
          call xySymm(i1,i2,AExp(1:10,1:i2))
          call xzSymm(i1,i2,AExp(1:10,1:i2))
          call yzSymm(i1,i2,AExp(1:10,1:i2))
          ArrExp(1:10,i1:i2)=ArrExp(1:10,i1:i2)+AExp(1:10,i1:i2)
        end if
      end if
      if(Allocated(AExp)) DeAllocate(AExp)
    end do
!!$OMP end do
!!$OMP end parallel
  end Subroutine GetArrExp

  Subroutine xySymm(i1,i2,A)
    Implicit none
    Integer(4) i1,i2
    Complex(8) A(10,i2)
    if(ixySymm.eq.1) then
      A(3,i1:i2)=-A(3,i1:i2)
      A(4,i1:i2)=-A(4,i1:i2)
      A(5,i1:i2)=-A(5,i1:i2)
      A(9,i1:i2)=-A(9,i1:i2)
    else
      A(1,i1:i2)=-A(1,i1:i2)
      A(2,i1:i2)=-A(2,i1:i2)
      A(6,i1:i2)=-A(6,i1:i2)
      A(10,i1:i2)=-A(10,i1:i2)
    end if
  end Subroutine xySymm

  Subroutine xzSymm(i1,i2,A)
    Implicit none
    Integer(4) i1,i2
    Complex(8) A(10,i2)
    if(ixzSymm.eq.1) then
      A(2,i1:i2)=-A(2,i1:i2)
      A(4,i1:i2)=-A(4,i1:i2)
      A(6,i1:i2)=-A(6,i1:i2)
      A(9,i1:i2)=-A(9,i1:i2)
    else
      A(1,i1:i2)=-A(1,i1:i2)
      A(3,i1:i2)=-A(3,i1:i2)
      A(5,i1:i2)=-A(5,i1:i2)
      A(10,i1:i2)=-A(10,i1:i2)
    end if
  end Subroutine xzSymm

  Subroutine yzSymm(i1,i2,A)
    Implicit none
    Integer(4) i1,i2
    Complex(8) A(10,i2)
    if(iyzSymm.eq.1) then
      A(1,i1:i2)=-A(1,i1:i2)
      A(5,i1:i2)=-A(5,i1:i2)
      A(6,i1:i2)=-A(6,i1:i2)
      A(9,i1:i2)=-A(9,i1:i2)
    else
      A(2,i1:i2)=-A(2,i1:i2)
      A(3,i1:i2)=-A(3,i1:i2)
      A(4,i1:i2)=-A(4,i1:i2)
      A(10,i1:i2)=-A(10,i1:i2)
    end if
  end Subroutine yzSymm

  Subroutine GetFieldExp(kE,kP,rE1,iD,iHE,nC)
! compute field of expansion kE - all expansions if kE out of range
!               of parameter kP - all parameters if kP out of range
! call GetArrExp first
! compute field in 1st half space / 1st quadrant / 1st octant, depending on symmetry
! and apply symmetry operations afterwards
    Implicit none
    Complex(8), Allocatable:: ArrExp(:,:)
    Real(8) rE(3),rE1(3)
    Integer(4) kE,kP,nC,idum
    Integer(2) iHE,iD
    rE=rE1
    if(ixySymm.ne.0) rE(3)=abs(rE(3))
    if(ixzSymm.ne.0) rE(2)=abs(rE(2))
    if(iyzSymm.ne.0) rE(1)=abs(rE(1))
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
    ArrExp=(0.0d0,0.0d0)
    call GetArrExp(kE,rE,iD,iHE,ArrExp)
    call GetFldExp(kE,kP,rE,iHE,nC,ArrExp)
    if((ixySymm.eq.1).and.(rE(3).ne.rE1(3))) then
      FldExp(3)=-FldExp(3)
      FldExp(4)=-FldExp(4)
      FldExp(5)=-FldExp(5)
      FldExp(10)=-FldExp(10)
    else if((ixySymm.eq.2).and.(rE(3).ne.rE1(3))) then
      FldExp(1)=-FldExp(1)
      FldExp(2)=-FldExp(2)
      FldExp(6)=-FldExp(6)
      FldExp(9)=-FldExp(9)
    end if
    if((ixzSymm.eq.1).and.(rE(2).ne.rE1(2))) then
      FldExp(2)=-FldExp(2)
      FldExp(4)=-FldExp(4)
      FldExp(6)=-FldExp(6)
      FldExp(9)=-FldExp(9)
    else if((ixzSymm.eq.2).and.(rE(2).ne.rE1(2))) then
      FldExp(1)=-FldExp(1)
      FldExp(3)=-FldExp(3)
      FldExp(5)=-FldExp(5)
      FldExp(10)=-FldExp(10)
    end if
    if((iyzSymm.eq.1).and.(rE(1).ne.rE1(1))) then
      FldExp(1)=-FldExp(1)
      FldExp(5)=-FldExp(5)
      FldExp(6)=-FldExp(6)
      FldExp(9)=-FldExp(9)
    else if((iyzSymm.eq.2).and.(rE(1).ne.rE1(1))) then
      FldExp(2)=-FldExp(2)
      FldExp(3)=-FldExp(3)
      FldExp(4)=-FldExp(4)
      FldExp(10)=-FldExp(10)
    end if
    if(Allocated(ArrExp)) DeAllocate(ArrExp)
  end Subroutine GetFieldExp

  Subroutine GetFldExp(kE,kP,rE,iHE,nC,ArrExp)
! compute field of expansion kE - all expansions if kE out of range
!               of parameter kP - all parameters if kP out of range
    Implicit none
    Complex(8) c,ArrExp(10,nParN)
    Real(8) rE(3),rP0(3)
    Integer(4) kE,kP,i1,i2,i,j,k1,k2,k,nC
    Integer(2) iHE
    FldExp=(0.0d0,0.0d0)
    nC=0
    k1=1
    k2=nExp
    if((kE.gt.0).and.(kE.le.nExp)) then
      k1=kE
      k2=k1
    else if((kE.lt.0).and.(-kE.le.nExp)) then
      k1=1
      k2=-kE
    end if
    do k=k1,k2
      if(LSkipExp(k)) Cycle
      if(igetiHE2(iHE,k).lt.0_2) Cycle
      if(tExp(k)%iObj.lt.0) Cycle
      i1=tExp(k)%iOff+1
      i2=tExp(k)%iOff+tExp(k)%nPar
      if((kP.gt.0).and.(kP.le.tExp(k)%nPar)) then
        i1=kP
        if(kE.gt.0) i1=i1+tExp(k)%iOff
        i2=i1
      end if
      do i=i1,i2
        do j=1,10
          FldExp(j)=FldExp(j)+ArrExp(j,i)*ParExp(kExc,i)
        end do
        nC=nC+1
      end do
    end do
    rP0=rE
    call MoveToOrigCell(rP0)
    if(lzPeriod) then
      c=cdexp((0.0d0,1.0d0)*czPeriod*Dble(nzPeriod)*r3Vec_Length(zPeriodVector))
      FldExp(1:10)=c*FldExp(1:10)
    end if
    if(lyPeriod) then
      c=cdexp((0.0d0,1.0d0)*cyPeriod*Dble(nyPeriod)*r3Vec_Length(yPeriodVector))
      FldExp(1:10)=c*FldExp(1:10)
    end if
    if(lxPeriod) then
      c=cdexp((0.0d0,1.0d0)*cxPeriod*Dble(nxPeriod)*xPeriod)
      FldExp(1:10)=c*FldExp(1:10)
    end if
  end Subroutine GetFldExp

  Subroutine GetArExp(kEx,rE,iD,iHEk,iXY,iXZ,iYZ,A)
! call recursive subroutine GetAExp
    Implicit none
    Complex(8) A(10,nParN)
    Real(8) rE(3)
    Integer(4) kEx
    Integer(2) iD,iHEk,iXY,iXZ,iYZ
    call GetAExp(kEx,rE,iD,iHEk,iXY,iXZ,iYZ,A)
  end Subroutine GetArExp

  Recursive Subroutine GetAExp(kEx,rE,iD,iHEk,iXY,iXZ,iYZ,A)
! compute A(rray) of expansion kEx at location r in domain iD
    Implicit none
    Complex(8) vh(3),gc,A(10,nParN),Aux(10,6)
    Real(8) rE(3),rE1,rg(3),rl(3),ri(3),al(3),alpha,ximag,yimag,zimag,kmaxmult,gar,gai
    Integer(4) kEx,kPa,idum,nP,nord,k1,kPa0,nP0
    Integer(2) iC,iD,iHE,iHEk,iHV,iType,ixT,iyT,izT,isT,minDeg,maxDeg,minOrd,maxOrd,nStep,maxPar,ixS,iyS,izS,iXY,iXZ,iYZ
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    rg=rE
    A(1:10,tExp(kEx)%iOff+1:tExp(kEx)%iOff+tExp(kEx)%nPar)=(0.0d0,0.0d0)
    iHE=igetiHE2(iHEk,kEx)
    nP=0
    if(iHE.lt.0_2) then
      if(lExpTest) then
        write(*,*) 'Expansion ',kEx,' without parameters!'
        idum=MessageBoxQQ('Expansion without parameters!'C,'Check expansions'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
      return
    end if
    nP=-1
    call setOrient(kEx,ixS,iyS,izS)
    ixS=min(2_2,max(0_2,ixS,iYZ))
    iyS=min(2_2,max(0_2,iyS,iXZ))
    izS=min(2_2,max(0_2,izS,iXY))
    if(lgcFld) then ! gc=propagation constant of the expansion. 2D: set to the global value gcFld
      gc=gcFld
    else
      gc=tExp(kEx)%gc
    end if
    Select Case(tExp(kEx)%iTypE)
    Case(0) ! 2d or 3d Connection
      if(lInverseConn) then
        if(tExp(kEx)%iConn.gt.tExp(kEx)%iE(1)) then
          call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
          iC=tExp(kEx)%iE(1)
          maxPar=tExp(kEx)%nPar
          kPa=Int4(tExp(kEx)%iOff)
          call Connection(kEx,kPa,rl,iD,iHE,iC,maxPar,nP,A)
        else
          idum=MessageBoxQQ('ERROR: Connection contained in connection with lower number. Try command SET CON N'C, &
                            'Compute connection'C,MB$OK.or.MB$ICONSTOP)
        end if
      else
        if(tExp(kEx)%iConn.lt.tExp(kEx)%iE(1)) then
          call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
          iC=tExp(kEx)%iE(1)
          maxPar=tExp(kEx)%nPar
          kPa=Int4(tExp(kEx)%iOff)
          call Connection(kEx,kPa,rl,iD,iHE,iC,maxPar,nP,A)
        else
          idum=MessageBoxQQ('ERROR: Connection contained in connection with higher number. Try command SET CON I'C, &
                            'Compute connection'C,MB$OK.or.MB$ICONSTOP)
        end if
      end if
    Case(1,2) ! 2d Hankel or Bessel
      minOrd=max(0_2,tExp(kEx)%iE(1))
      maxOrd=max(minOrd,tExp(kEx)%iE(2))
      minDeg=0
      maxDeg=0
      nStep=max(1_2,tExp(kEx)%iE(5))
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        iType=1
        if(tExp(kEx)%iTypE.eq.2) iType=2 ! Bessel
        izS=modulo(tExp(kEx)%iE(6),10_2)
        if(lfcFld.and.(iabs(izS).ge.1).and.(iabs(izS).le.6)) then ! multi-propagation-constant expansion
          if(izS.gt.0) then ! adapt local coordinate system
            select case(izS)
            case(1,4) ! propagation in global x direction
              tExp(kEx)%Plane(1:3,1:3)=0.0d0
              tExp(kEx)%Plane(1,3)=1.0d0
              tExp(kEx)%Plane(2,1)=1.0d0
              tExp(kEx)%Plane(3,2)=1.0d0
            case(2,5) ! propagation in yPeriodVector direction
              tExp(kEx)%Plane(1:3,1:2)=0.0d0
              tExp(kEx)%Plane(1:3,3)=unit3DVec(yPeriodVector)
              tExp(kEx)%Plane(3,1)=1.0d0
              tExp(kEx)%Plane(1:3,2)=r3Vec_Prod(tExp(kEx)%Plane(1:3,3),tExp(kEx)%Plane(1:3,1))
            case(3,6) ! propagation in zPeriodVector direction
              tExp(kEx)%Plane(1:3,1:2)=0.0d0
              tExp(kEx)%Plane(1:3,3)=unit3DVec(zPeriodVector)
              tExp(kEx)%Plane(1,1)=1.0d0
              tExp(kEx)%Plane(1:3,2)=unit3DVec(r3Vec_Prod(tExp(kEx)%Plane(1:3,3),tExp(kEx)%Plane(1:3,1)))
              tExp(kEx)%Plane(1:3,1)=unit3DVec(r3Vec_Prod(tExp(kEx)%Plane(1:3,2),tExp(kEx)%Plane(1:3,3)))
            end select
          end if
          if(lfcFld.and.(iabs(izS).lt.4)) then ! use periodic constants instead of gamma
            select case(iabs(izS))
            case(1)
              gc=CxPeriod/(2.0d0*Pi*kw0*fcFld)
            case(2)
              gc=CyPeriod/(2.0d0*Pi*kw0*fcFld)
            case(3)
              gc=CzPeriod/(2.0d0*Pi*kw0*fcFld)
            end select
          end if
          tExp(kEx)%gc=gc
          minDeg=max(0_2,tExp(kEx)%iE(3))
          maxDeg=max(minDeg,tExp(kEx)%iE(4))
        end if
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        rE1=tExp(kEx)%rE(2)
        ! Note: negative rE1 for special symmetry operations instead of those of project; min/max radius not used!
        ! Example: rE1=-1.201 -> symmetry numbers 201 (with respect to XY, XZ, YZ planes)
        ximag=tExp(kEx)%rE(3)
        yimag=tExp(kEx)%rE(4)
        maxPar=tExp(kEx)%nPar
        kPa=Int4(tExp(kEx)%iOff)
        call Multipole2Ds(kEx,kPa,iType,rl,gc,rE1,iD,iHE,minOrd,maxOrd,minDeg,maxDeg,nStep,maxPar,nP,ixS,iyS,izS, &
        & ximag,yimag,A)
        tExp(kEx)%iE(1)=minOrd
        tExp(kEx)%iE(2)=maxOrd
        tExp(kEx)%iE(5)=nStep
      end if
    Case(3) ! 2d or 3d Plane wave
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        maxPar=tExp(kEx)%nPar
        kPa=Int4(tExp(kEx)%iOff)
        isT=tExp(kEx)%iE(4)
        if(lgcFld) then
          alpha=tExp(kEx)%rE(1)
          call PlaneWave2D(kEx,kPa,rl,iD,iHE,isT,alpha,maxPar,nP,A)
        else
          alpha=tExp(kEx)%plane(3,3)
          if(tExp(kEx)%iE(2).lt.1) then
            call PlaneWave3D(kEx,kPa,A,rl,iD,iHE,isT,alpha,maxPar,nP)
          else
            call vGlob2Loc(tExp(kEx)%e,tExp(kEx)%Plane,al)
            call PlaneWave3D(kEx,kPa,A,rl,iD,iHE,isT,alpha,maxPar,nP,al,tExp(kEx)%rE(2),tExp(kEx)%rE(3),tExp(kEx)%iE(2), &
            &    tExp(kEx)%iE(3))
          end if
        end if
      end if
    Case(4) ! 2d or 3d Rayleigh
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        if(lgcFld) then
          call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
          iyT=tExp(kEx)%iE(1)
          maxOrd=max(0_2,tExp(kEx)%iE(2))
          isT=tExp(kEx)%iE(4)
          maxPar=tExp(kEx)%nPar
          kPa=Int4(tExp(kEx)%iOff)
          call Rayleigh2D(kEx,kPa,rl,gc,iD,iHE,iyT,isT,maxOrd,maxPar,nP,A)
          tExp(kEx)%iE(2)=maxOrd
        else
          call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
          iyT=tExp(kEx)%iE(1)
          maxOrd=max(0_2,tExp(kEx)%iE(2))
          maxDeg=max(0_2,tExp(kEx)%iE(3))
          isT=tExp(kEx)%iE(4)
          maxPar=tExp(kEx)%nPar
          kPa=Int4(tExp(kEx)%iOff)
          call Rayleigh3D(kEx,kPa,rl,iD,iHE,iyT,isT,maxOrd,maxDeg,maxPar,nP,A)
          tExp(kEx)%iE(2)=maxOrd
        end if
      end if
    Case(5) ! Harmonic
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        ixT=tExp(kEx)%iE(1)
        iyT=tExp(kEx)%iE(2)
        izT=tExp(kEx)%iE(6)
        if(lgcFld) then
          izT=0
        end if
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        minOrd=max(1_2,tExp(kEx)%iE(3))
        maxOrd=max(minOrd,tExp(kEx)%iE(4))
        nStep=max(1_2,tExp(kEx)%iE(5))
        maxPar=tExp(kEx)%nPar
        kPa=Int4(tExp(kEx)%iOff)
        call Harmonic2D(kEx,kPa,rl,gc,iD,iHE,ixT,iyT,izT,minOrd,maxOrd,nStep,maxPar,nP,A)
        tExp(kEx)%iE(3)=minOrd
        tExp(kEx)%iE(4)=maxOrd
        tExp(kEx)%iE(5)=nStep
      end if
    Case(6,7) ! 3d Hankel or Bessel
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        iType=1 ! Hankel 1
        if(tExp(kEx)%iTypE.eq.7) iType=2 ! Bessel
        if(tExp(kEx)%iE(6)/10.eq.99) iType=iType+2 ! Hankel 2 or Neumann
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        rE1=tExp(kEx)%rE(1)
        ! Note: negative rE1 for special symmetry operations instead of those of project; min/max radius not used!
        ! Example: rE1=-1.201 -> symmetry numbers 201 (with respect to XY, XZ, YZ planes)
        ximag=tExp(kEx)%rE(2)
        yimag=tExp(kEx)%rE(3)
        zimag=tExp(kEx)%rE(4)
        if(lMMPStat) then
          minOrd=max(0_2,tExp(kEx)%iE(1))
        else
          minOrd=max(1_2,tExp(kEx)%iE(1))
        end if
        maxOrd=max(minOrd,tExp(kEx)%iE(2))
        minDeg=max(0_2,tExp(kEx)%iE(3))
        maxDeg=max(minDeg,tExp(kEx)%iE(4))
        maxPar=tExp(kEx)%nPar
        kPa=Int4(tExp(kEx)%iOff)
        if((abs(ximag)+abs(yimag)+abs(zimag)).gt.pSmall) then
          call Multipole3DC(kEx,kPa,iType,rl,rE1,iD,iHE,minDeg,maxDeg,minOrd,maxOrd,maxPar,0_2,nP,ixS,iyS,izS,&
          &                 ximag,yimag,zimag,A)
        else
          call Multipole3D(kEx,kPa,iType,rl,rE1,iD,iHE,minDeg,maxDeg,minOrd,maxOrd,maxPar,0_2,nP,ixS,iyS,izS,A)
        end if
        tExp(kEx)%iE(1)=minOrd
        tExp(kEx)%iE(2)=maxOrd
        tExp(kEx)%iE(3)=minDeg
        tExp(kEx)%iE(4)=maxDeg
      end if
    Case(8) ! 3d Ring
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        kPa=Int4(tExp(kEx)%iOff)
        call Ring3D(kEx,kPa,rl,iD,nP,izS,A)
      end if
    Case(9) ! 3d Line
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        kPa=Int4(tExp(kEx)%iOff)
        call Line3D(kEx,kPa,rl,iD,nP,ixS,iyS,A)
      end if
    Case(10) ! 3d Spiral
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
        kPa=Int4(tExp(kEx)%iOff)
        call Spiral3D(kEx,kPa,rl,iD,nP,A)
      end if
    Case(11) ! 3d Gaussian beam
      if((tExp(kEx)%nPar.gt.0).and.(tExp(kEx)%iDom.eq.iD)) then
        if(lgcFld) then
          nP=0
        else
          call vGlob2Loc(rg,tExp(kEx)%Plane,rl)
          maxPar=tExp(kEx)%nPar
          kPa=Int4(tExp(kEx)%iOff)
          nord=tExp(kEx)%iE(1)
          call GaussBeam3D(kEx,kPa,rl,iD,tExp(kEx)%rE(1),nord,maxPar,nP,A)
          tExp(kEx)%iE(1)=nord
        end if
      end if
    Case(12) ! 2d monopole for multilayer
      if((tExp(kEx)%nPar.gt.0).and.(iD.ge.0).and.(iD.le.tExp(kEx)%iE(1))) then ! this is a multi-domain expansion for domains 1...tExp(kEx)%iE(1)
        nP=0
        izS=ixySymm
        if(((iHE.eq.0).and.((ixS.eq.2).or.(izS.eq.1))).or.((iHE.eq.1).and.((ixS.eq.1).or.(izS.eq.2)))) then ! wrong symmetry
          if(lExpTest) then
            write(*,*) 'Expansion ',kEx,' without parameters!'
            idum=MessageBoxQQ('Expansion without parameters!'C,'Check expansions'C, &
                              MB$OK.or.MB$ICONEXCLAMATION)
          end if
          return
        end if
        if(ixS.eq.2) iHE=1
        if(ixS.eq.1) iHE=0
        if(lgcFld) then
          gar=Dble(gcFld)
          gai=Dimag(gcFld)
        else
          gar=Dble(tExp(kEx)%gc)
          gai=Dimag(tExp(kEx)%gc)
        end if
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl) ! this global to local coordinate transform only shifts the origin
        maxPar=tExp(kEx)%nPar
        kPa=Int4(tExp(kEx)%iOff) ! standard: kPa is the offset for the parameter address in the array A
        if(tExp(kEx)%iE(1).lt.2) tExp(kEx)%iE(1)=2 ! current implementation does not allow less than 2 layers
        if(tExp(kEx)%iE(1).gt.mxlyr) tExp(kEx)%iE(1)=mxlyr ! current implementation does not allow more than mxlyr layers
        aanoflyr=tExp(kEx)%iE(1) ! number of layers (stored in iE1)
        peccheck(1)=tExp(kEx)%iE(2) ! bottom layer is standard/PEC/PMC (stored in iE2)
        peccheck(2)=tExp(kEx)%iE(3) ! top layer is standard/PEC/PMC (stored in iE3)
        kmaxmult=tExp(kEx)%rE(1) ! Sommerfeld integration parameter (stored in rE1)
        aaepsrel=max(1.0d-16,min(0.1d0,abs(tExp(kEx)%rE(2))))
        ri(1:2)=tExp(kEx)%rE(3:4)
        call getTwodlyrAux(tExp(kEx)%Plane(2,0)) ! compute the auxiliary input data (layer heights, source layer, source location in layer
        call twodlyr(kEx,kPa,maxPar,nP,iHE,rl,ri,kmaxmult,A,int4(iD),gar,gai)
      end if
    Case(13) ! 3d dipole for multilayer
      if((tExp(kEx)%nPar.gt.0).and.(iD.ge.0).and.(iD.le.tExp(kEx)%iE(1))) then ! this is a multi-domain expansion for domains 1...tExp(kEx)%iE(1)
        nP=0
        iHV=2
        if(tExp(kEx)%iE(4).le.0) then ! only hor. dipoles
          iHV=0
        end if
        if(tExp(kEx)%iE(4).eq.1) then ! only vert. dipoles
          if(((iHE.eq.0).and.((ixS.eq.2).or.(izS.eq.2))).or.((iHE.eq.1).and.((ixS.eq.1).or.(izS.eq.1)))) then ! wrong symmetry
            if(lExpTest) then
              write(*,*) 'Expansion ',kEx,' without parameters!'
              idum=MessageBoxQQ('Expansion without parameters!'C,'Check expansions'C, &
                                MB$OK.or.MB$ICONEXCLAMATION)
            end if
            return
          end if
          iHV=1
        end if
        if(lgcFld) then
          gcFld=(0.0d0,0.0d0) ! this expansion cannot handle problems with gamma different from 0!
        else
          tExp(kEx)%gc=(0.0d0,0.0d0) ! this expansion cannot handle problems with gamma different from 0!
        end if
        call vGlob2Loc(rg,tExp(kEx)%Plane,rl) ! this global to local coordinate transform only shifts the origin
        maxPar=tExp(kEx)%nPar
        kPa=Int4(tExp(kEx)%iOff) ! standard: kPa is the offset for the parameter address in the array A
        if(tExp(kEx)%iE(1).lt.2) tExp(kEx)%iE(1)=2 ! current implementation does not allow less than 2 layers
        if(tExp(kEx)%iE(1).gt.mxlyr) tExp(kEx)%iE(1)=mxlyr ! current implementation does not allow more than mxlyr layers
        aanoflyr=tExp(kEx)%iE(1) ! number of layers (stored in iE1)
        peccheck(1)=tExp(kEx)%iE(2) ! bottom layer is standard/PEC/PMC (stored in iE2)
        peccheck(2)=tExp(kEx)%iE(3) ! top layer is standard/PEC/PMC (stored in iE3)
        kmaxmult=tExp(kEx)%rE(1) ! Sommerfeld integration parameter (stored in rE1)
        aaepsrel=max(1.0d-16,min(0.1d0,abs(tExp(kEx)%rE(2))))
        ri(1:3)=tExp(kEx)%rE(3:5)
        call getTwodlyrAux(tExp(kEx)%Plane(2,0)) ! compute the auxiliary input data (layer heights, source layer, source location in layer
        kPa0=0
        nP0=0
        Aux=(0.0d0,0.0d0)
        call threedlyr(kEx,kPa0,maxPar,nP0,iHE,iHV,rl,ri,kmaxmult,Aux,int4(iD))
        kPa0=0
        if(iHE.ne.1) then ! electric dipoles
          kPa0=kPa0+1
          if(iHV.ne.1) then ! hor. dipole x
            if((ixS.ne.1).and.(izS.ne.2)) then
              kPa=kPa+1
              nP=nP+1
              A(1:10,kPa)=A(1:10,kPa)+Aux(1:10,kPa0)
            end if
          end if
          kPa0=kPa0+1
          if(iHV.ne.0) then ! vert. dipole y
            if((ixS.ne.2).and.(izS.ne.2)) then
              kPa=kPa+1
              nP=nP+1
              A(1:10,kPa)=A(1:10,kPa)+Aux(1:10,kPa0)
            end if
          end if
          kPa0=kPa0+1
          if(iHV.ne.1) then ! hor. dipole z
            if((ixS.ne.2).and.(izS.ne.1)) then
              kPa=kPa+1
              nP=nP+1
              A(1:10,kPa)=A(1:10,kPa)+Aux(1:10,kPa0)
            end if
          end if
        end if
        if(iHE.ne.0) then ! magnetic dipoles
          kPa0=kPa0+1
          if(iHV.ne.1) then ! hor. dipole x
            if((ixS.ne.2).and.(izS.ne.1)) then
              kPa=kPa+1
              nP=nP+1
              A(1:10,kPa)=A(1:10,kPa)+Aux(1:10,kPa0)
            end if
          end if
          kPa0=kPa0+1
          if(iHV.ne.0) then ! vert. dipole y
            if((ixS.ne.1).and.(izS.ne.1)) then
              kPa=kPa+1
              nP=nP+1
              A(1:10,kPa)=A(1:10,kPa)+Aux(1:10,kPa0)
            end if
          end if
          kPa0=kPa0+1
          if(iHV.ne.1) then ! hor. dipole z
            if((ixS.ne.1).and.(izS.ne.2)) then
              kPa=kPa+1
              nP=nP+1
              A(1:10,kPa)=A(1:10,kPa)+Aux(1:10,kPa0)
            end if
          end if
        end if
      end if
    end Select
    if(lExpTest.and.(nP.lt.tExp(kEx)%nPar)) then
      k1=kExp
      kExp=kEx
      call RepExpTyp(Int4(nP))
      kExp=k1
      if(nP.lt.1) then
        write(*,*) 'Expansion ',kEx,' without parameters!'
        idum=MessageBoxQQ('Expansion without parameters!'C,'Check expansions'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
    end if
    do kPa=tExp(kEx)%iOff+1,tExp(kEx)%iOff+tExp(kEx)%nPar
      call cvLoc2Glob(A(1:3,kPa),tExp(kEx)%Plane,vh)
      A(1:3,kPa)=vh
      call cvLoc2Glob(A(4:6,kPa),tExp(kEx)%Plane,vh)
      A(4:6,kPa)=vh
      call cvLoc2Glob(A(7:9,kPa),tExp(kEx)%Plane,vh)
      A(7:9,kPa)=vh
    end do
  end Subroutine GetAExp

  Recursive Subroutine GetFldAExp(kEx,A)
! compute field of expansion kEx in array A
    Implicit none
    Complex(8) A(10,nParN)
    Integer(4) kEx,i1,i2,i,j
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    i1=tExp(kEx)%iOff+1
    i2=tExp(kEx)%iOff+tExp(kEx)%nPar
    FldAExp(1:10)=(0.0d0,0.0d0)
    do i=i1,i2
      do j=1,10
        FldAExp(j)=FldAExp(j)+A(j,i)*ParExp(kExc,i)
      end do
    end do
  end Subroutine GetFldAExp

  Recursive Subroutine Connection(kEx,kPa0,rc,iD0,iHEt,iC,maxPar,nP,A)
! compute connection for domain iD0
! if iD0=0: use all domains
! if iD0<0: use domains 1...-iD0
    Implicit none
    Complex(8) A(10,nParN),Ah(10)
    Real(8) rc(3),r(3),ai,da,fact,fact2
    Real(8), save, Allocatable:: PL(:)
    Integer(4) kE,kEx,kPa,nP,idum,kPa0,iType,iErr
    Integer(2) iD0,iD,iD2,iC,maxPar,iHEt,iHEk,mOrd,nCos,nSin,i,nEx,nE,nEx1
    Integer(2), save:: iPL
    Data iPL/0/
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    nEx=0
    do kE=1,nExp ! count included expansions
      if(((tExp(kE)%iConn.gt.0).and.(tExp(kE)%iConn.ne.iC)).or.(tExp(kE)%iObj.lt.0).or. &
        ((tExp(kE)%iConn.lt.0).and.(-tExp(kE)%iConn.lt.iC)).or.(kE.eq.kEx)) Cycle
      if((tExp(kE)%iTypE.eq.0).and.(tExp(kE)%iConn.eq.0)) Cycle
      iHEk=igetiHE2(iHEt,kE)
      if(iHEk.lt.0) Cycle
      if((iD0.lt.0).and.(-iD0.gt.tExp(kE)%iDom)) Cycle
      nEx=nEx+1
    end do
    if(nEx.lt.1_2) return
    fact=tExp(kEx)%rE(5)
    if(dabs(fact).lt.1.0d-30) fact=1.0d0 ! compatibility with old versions (fact undefined, usually 0)
    iType=tExp(kEx)%iE(6)
    nEx1=nEx
    if(modulo(iType,2_4).eq.1_4) nEx1=nEx-1
    if(iType.ge.0) then ! Fourier basis
      iD2=igetiCS(kEx) ! even->cos, odd->sin terms
      mOrd=max(Int2(tExp(kEx)%iE(5)),0_2)
      nCos=0_2
      nSin=0_2
      if(iD2.ne.1_2) nCos=mOrd+1_2
      if(iD2.ne.2_2) nSin=mOrd
      nP=nCos+nSin
      if(maxPar.lt.nP) then
        if(iD2.eq.1_2) then ! only sin terms
          nSin=maxPar
        else if(iD2.eq.2_2) then ! only cos terms
          nCos=maxPar
        else ! all terms
          nSin=maxPar/2_2
          nCos=nSin+1_2
          nP=nCos+nSin
          if(maxPar.lt.nP) nSin=nSin-1_2
        end if
      end if
      nP=nCos+nSin
      mOrd=max(nSin,nCos-1_2)
      da=2.0d0*Pi*fact/dble(max(nEx1,1))
    else ! Legendre basis
      mOrd=max(Int2(tExp(kEx)%iE(5)),0_2)
      nCos=mOrd+1_2
      nSin=0_2
      if(maxPar.lt.nP) nCos=maxPar
      nP=nCos
      mOrd=nCos-1_2
      da=2.0d0*fact/dble(max(nEx1,1))
      if(iPL.lt.nCos) then
        if(Allocated(PL)) Deallocate(PL,Stat=iErr)
        Allocate(PL(0:nCos),Stat=iErr)
        if(iErr.ne.0) then
          idum=MessageBoxQQ('Cannot allocate buffer for Legendre basis!'C,'Connection'C, &
                            MB$OK.or.MB$ICONSTOP)
          Deallocate(PL,Stat=iErr)
          iPL=0
          return
        end if
        iPL=nCos
      end if
    end if
    A(1:10,tExp(kEx)%iOff+1:tExp(kEx)%iOff+nP)=(0.0d0,0.0d0)
    if(lExpTest) then
      if(tExp(kEx)%iDom.ne.0) then
        idum=MessageBoxQQ('Connection domain number is not 0!\rSet domain number 0?'C, &
                          'Expansion check'C,MB$YESNO.or.MB$ICONQUESTION)
        if(idum.eq.MB$IDYES) tExp(kEx)%iDom=0
      end if
      return
    end if
    if(kEx.gt.0) then
      if(((tExp(kEx)%rE(2).lt.nSmall).and.(-tExp(kEx)%rE(2).lt.dabs(rc(1)))).or. &
      &  ((tExp(kEx)%rE(2).gt.+pSmall).and.(+tExp(kEx)%rE(2).gt.rc(1)))) return
      if(((tExp(kEx)%rE(3).lt.nSmall).and.(-tExp(kEx)%rE(3).lt.dabs(rc(2)))).or. &
      &  ((tExp(kEx)%rE(3).gt.+pSmall).and.(+tExp(kEx)%rE(3).gt.rc(2)))) return
      if(((tExp(kEx)%rE(4).lt.nSmall).and.(-tExp(kEx)%rE(4).lt.dabs(rc(3)))).or. &
      &  ((tExp(kEx)%rE(4).gt.+pSmall).and.(+tExp(kEx)%rE(4).gt.rc(3)))) return
    end if
    ai=-da ! Fourier interval (0...2Pi)
    if(modulo(iType,2_4).ne.1_4) ai=-0.5d0*da
    if(iType.lt.0) ai=ai-1.0d0 ! Legendre interval (-1...+1)
    nE=0_2
    do kE=1,nExp
      if(((tExp(kE)%iConn.gt.0).and.(tExp(kE)%iConn.ne.iC)).or.(tExp(kE)%iObj.lt.0).or. &
        ((tExp(kE)%iConn.lt.0).and.(-tExp(kE)%iConn.lt.iC)).or.(kE.eq.kEx)) Cycle
      if((tExp(kE)%iTypE.eq.0).and.(tExp(kE)%iConn.eq.0)) Cycle
      iHEk=igetiHE2(iHEt,kE)
      if(iHEk.lt.0) Cycle
      if((iD0.lt.0).and.(-iD0.gt.tExp(kE)%iDom)) Cycle
      nE=nE+1_2
      if(((nE.eq.1_2).or.(nE.eq.nEx)).and.(nP.gt.1).and.(modulo(iabs(iType),10).gt.2)) then
        fact2=0.5d0 ! trapezoidal integration: first and last function value multipied with 0.5
      else
        fact2=1.0d0 ! simple sum
      end if
      iD=iD0
      if(iD.eq.0) iD=tExp(kE)%iDom
      r=rc
      ai=ai+da
      if(iType.lt.0) call Legenm(ai,0.0d0,PL,Int4(nCos-1_2),0_4)
      call GetAExp(kE,r,iD,iHEk,tExp(kEx)%iE(2),tExp(kEx)%iE(3),tExp(kEx)%iE(4),A)
      if(iabs(iType).gt.9) then ! set amplitudes for all parameters equal to 0, except for parameter number iabs(iType)/10
        i=iabs(iType)/10
        if((i.gt.0).and.(i.le.tExp(kE)%nPar)) then
          Ah(1:10)=A(1:10,tExp(kE)%iOff+i)
          A(1:10,tExp(kE)%iOff+1:tExp(kE)%iOff+tExp(kE)%nPar)=(0.0d0,0.0d0)
          A(1:10,tExp(kE)%iOff+i)=Ah(1:10)
        end if
      end if
      call GetFldAExp(kE,A)
      kPa=kPa0
      do i=0_2,nCos-1_2 ! cos(ai) weighted sums of expansions contained in the connection
        kPa=kPa+1
        if(iType.ge.0) then ! Fourier basis
          A(1:10,kPa)=A(1:10,kPa)+fact2*FldAExp(1:10)*dcos(ai*dble(i))
        else ! Legendre basis
          A(1:10,kPa)=A(1:10,kPa)+fact2*FldAExp(1:10)*PL(dble(i))
        end if
      end do
      do i=1_2,nSin ! sin(ai) weighted sums of expansions contained in the connection
        kPa=kPa+1
        A(1:10,kPa)=A(1:10,kPa)+fact2*FldAExp(1:10)*dsin(ai*dble(i))
      end do
    end do
  end Subroutine Connection

  Subroutine Multipole2Ds(kEx,kPa,iType,r,gci,rE1,iD,iHEk,minOrd,maxOrd,minDeg,maxDeg,nStep,maxPar,nPi,ixS,iyS,izS, &
  & ximag,yimag,Ar)
! compute 2D multipole or Bessel expansions by calling the appropriate Multipole2D and Multipole2DC routines
! Note: izS is not a symmetry flag! It indicates whether a 3D problem with different gamma values shall be evaluated and which period shall be used.
    Implicit none
    Integer(4) kEx,kPa,nP,nPi,k1,i
    Complex(8) gci,gc,Ar(10,nParN),A(10,nParN),period
    Real(8) rE1,r(3),ximag,yimag
    Integer(2) iD,iHEk,minOrd,maxOrd,minDeg,maxDeg,nStep,maxPar,iType,ixS,iyS,izS
    nPi=0
    if((kEx.lt.0).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    if(lgcFld.or.(izS.eq.0).or.(iabs(izS).gt.6)) then ! standard 2D multipole
      gc=gci
      if((abs(ximag)+abs(yimag)).gt.pSmall) then
        call Multipole2DC(kEx,kPa,iType,r,gc,rE1,iD,iHEk,minOrd,maxOrd,nStep,maxPar,nPi,ixS,iyS,ximag,yimag,Ar)
      else
        call Multipole2D(kEx,kPa,iType,r,gc,rE1,iD,iHEk,minOrd,maxOrd,nStep,maxPar,nPi,ixS,iyS,Ar)
      end if
    else ! superposition of different "degrees" with different gamma values
      if((iabs(izS).eq.1).or.(iabs(izS).eq.4)) then
        period=1.0d0/(xPeriod*kw0*fcFld)
      else if((iabs(izS).eq.2).or.(iabs(izS).eq.5)) then
        period=1.0d0/(r2Vec_Length(yPeriodVector(1:2))*kw0*fcFld)
      else
        period=1.0d0/(r3Vec_Length(zPeriodVector)*kw0*fcFld)
      end if
      k1=kPa
      do i=minDeg,maxDeg
        gc=gci+Dble(i)*period
        A=(0.0d0,0.0d0)
        nP=0
        if((abs(ximag)+abs(yimag)).gt.pSmall) then
          call Multipole2DC(kEx,kPa,iType,r,gc,rE1,iD,iHEk,minOrd,maxOrd,nStep,maxPar,nP,ixS,iyS,ximag,yimag,A)
        else
          call Multipole2D(kEx,kPa,iType,r,gc,rE1,iD,iHEk,minOrd,maxOrd,nStep,maxPar,nP,ixS,iyS,A)
        end if
        Ar(1:10,k1+1:k1+nP)=A(1:10,k1+1:k1+nP)
        k1=k1+nP
        nPi=nPi+nP
      end do
    end if
  end Subroutine Multipole2Ds

  Subroutine Multipole2D(kEx,kPa,iType,r,gc,rE1,iD,iHEk,minOrd,maxOrd,nStep,maxPar,nP,ixS,iyS,A)
! compute 2D multipole or Bessel expansion
    Implicit none
    Integer(4) kEx,kPa,nP,idumy1,idumy2,idumy3,idum,mPa,n,m,ierr,iEven,iOdd
    Complex(8), Allocatable :: cBes(:)
    Real(8), Allocatable :: asi(:),aco(:)
    Complex(8) gc,ckap2,crkap,cigr,cioeps,ciomu,ceigz,cerho,cephi,cez,cr,cf,ckz,A(10,nParN)
    Real(8) rE1,rho,rn,phicos,phisin,r(3),pfakth
    Real(8), external:: dsqrtc
    Integer(2) iD,iHEk,minOrd,maxOrd,nStep,maxPar,lf,iType,ixS,iyS
    idumy1=maxOrd+2
    idumy2=maxOrd+20
    idumy3=14
    if(Allocated(cBes)) DeAllocate(cBes)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(aco)) DeAllocate(aco)
    Allocate(cBes(0:idumy2),asi(0:idumy1),aco(0:idumy1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'2D-Multipole'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    rho=dsqrtc(r(1)**2+r(2)**2)
    if(.not.lExpTest) then
      if((abs(iType).eq.2).and.(rho.gt.rE1).and.(rE1.gt.pSmall)) return !!!! Bessel type = 0 outside circle
      if((abs(iType).eq.1).and.(rho.lt.rE1).and.(rE1.gt.pSmall)) return !!!! Hankel type = 0 inside circle
    end if
    if(rho.gt.1.0d-150) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-150
    end if
    if(lzPer) then
      ckz=Pi/dcFld
    else
      ckz=(2.0d0*Pi*kw0)*fcFld*gc
    end if
    ckap2=kcFld**2-ckz**2
    kapcFld=cdsqrt(ckap2)
    if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
    if((tExp(kEx)%iE(6)/10.eq.99).or.(tExp(kEx)%iE(6)/10.eq.69)) kapcFld=-kapcFld
    if((tExp(kEx)%iE(6)/10.eq.88).or.(tExp(kEx)%iE(6)/10.eq.68)) kapcFld=cdsqrt(ckap2)
    if((tExp(kEx)%iE(6)/10.eq.77).or.(tExp(kEx)%iE(6)/10.eq.67)) kapcFld=-cdsqrt(ckap2)
    crkap=rho*kapcFld
    cigr=(0.0d0,1.0d0)*ckz/rho
    cioeps=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Eps0*eDom(lf)/rho
    ciomu=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Mue0*uDom(lf)/rho
    ceigz=cdexp((0.0d0,1.0d0)*ckz*r(3))
    if(lMMPstat) then
      if(iType.eq.2) then
        cBes(0)=(1.0d0,0.0d0)
        cBes(1:idumy1)=(0.0d0,0.0d0)
        rn=1.0d0
        do n=1,idumy1
          if(rho.gt.1.0d0) then
            if(rn.gt.(pBig/rho)) Exit
          end if
          rn=rn*rho
          cBes(n)=dCmplx(rn,0.0d0)
        end do
      else
        cBes(0)=dCmplx(-dlog(rho),0.0d0)
        cBes(1:idumy1)=(0.0d0,0.0d0)
        rn=1.0d0
        do n=1,idumy1
          if(rho.lt.1.0d0) then
            if(rn.gt.(pBig*rho)) Exit
          end if
          rn=rn/rho
          cBes(n)=dCmplx(rn,0.0d0)
        end do
      endif
    else
      if(iType.eq.2) then
        call cj(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
      else if(tExp(kEx)%iE(6)/100.eq.6) then
        call ch2(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
      else
        call ch1(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
      endif
      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
      if((ierr.ne.0).and.lDispWarn) then
        if(iType.eq.2) then
          write(*,*) 'WARNING: Error',ierr,' in Bessel expansion ',kEx,' crkap=',crkap
        else
          write(*,*) 'WARNING: Error',ierr,' in Hankel expansion ',kEx,' crkap=',crkap
        end if
        write(*,*) '         Field computation inaccurate'
        lDispWarn=.false.
      end if
    end if
    call sicom(Int4(maxOrd),phisin,phicos,asi,aco)
! scaling
    pfakth=1.0d0/cdabs(cZw)
    if(minOrd.eq.0_2) then
! 2d expansion of degree 0
      if(lMMPstat) then
        if(iType.eq.2) then
          cerho=(0.0d0,0.0d0)
        else
          cerho=dCmplx(-1.0d0/rho,0.0d0)*ceigz
        end if
        cez=cBes(0)*ceigz
      else
        cerho=-crkap*cBes(1)*ceigz
        cez=ckap2*cBes(0)*ceigz
      end if
! e c 0
      if((kPa.lt.mPa).and.(iHEk.ne.1).and.(ixS.ne.2).and.(iyS.ne.2)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          A(10,kPa)=A(10,kPa)+cez               ! V
          cr=-cerho        ! Er
          A(1,kPa)=A(1,kPa)+phicos*cr           ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr           ! Ey
          A(3,kPa)=A(3,kPa)+(0.0d0,-1.0d0)*ckz*cez ! Ez
          cf=-pfakth*cerho ! Hf
          A(4,kPa)=A(4,kPa)-phisin*cf           ! Hx
          A(5,kPa)=A(5,kPa)+phicos*cf           ! Hy
        else
          cr=cigr*cerho    ! Er
          A(1,kPa)=A(1,kPa)+phicos*cr           ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr           ! Ey
          A(3,kPa)=A(3,kPa)+cez                 ! Ez
          cf=cioeps*cerho  ! Hf
          A(4,kPa)=A(4,kPa)-phisin*cf           ! Hx
          A(5,kPa)=A(5,kPa)+phicos*cf           ! Hy
        end if
      end if
! h c 0
      if((kPa.lt.mPa).and.(iHEk.ne.0).and.(ixS.ne.1).and.(iyS.ne.1)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          A(9,kPa)=A(9,kPa)+cez                 ! Az
          cf=-cerho              ! Ef
          A(1,kPa)=A(1,kPa)-phisin*cf           ! Ex
          A(2,kPa)=A(2,kPa)+phicos*cf           ! Ey
          cr=pfakth*cerho        ! Hr
          A(4,kPa)=A(4,kPa)+phicos*cr           ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr           ! Hy
          A(6,kPa)=A(6,kPa)+pfakth*(0.0d0,-1.0d0)*ckz*cez ! Hz
        else
          cf=-pfakth*ciomu*cerho ! Ef
          A(1,kPa)=A(1,kPa)-phisin*cf           ! Ex
          A(2,kPa)=A(2,kPa)+phicos*cf           ! Ey
          cr=pfakth*cigr*cerho   ! Hr
          A(4,kPa)=A(4,kPa)+phicos*cr           ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr           ! Hy
          A(6,kPa)=A(6,kPa)+pfakth*cez          ! Hz
        end if
      end if
    end if
    lf=minOrd
    if(lf.eq.0) lf=nStep
    do m=lf,maxOrd,nStep
! 2d expansion of degree m
      if(kPa.ge.mPa) Exit
      iEven=modulo(m,2)+1
      iOdd=3-iEven
      if(lMMPstat) then
        cez=cBes(m)*ceigz
        if(iType.eq.2) then
          cerho=dble(m)*cBes(m-1)*ceigz
          cephi=dble(m)*cBes(m-1)*ceigz
        else
          cerho=dble(-m)*cBes(m+1)*ceigz
          cephi=dble(m)*cBes(m+1)*ceigz
        end if
      else
        cez=ckap2*cBes(m)*ceigz
        cephi=dble(m)*cBes(m)*ceigz
        cerho=cephi-crkap*cBes(m+1)*ceigz
      end if
! e c m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          A(10,kPa)=A(10,kPa)+aco(m)*cez        ! V
          cr=-aco(m)*cerho        ! Er=...
          cf=asi(m)*cephi         ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          A(3,kPa)=A(3,kPa)+(0.0d0,-1.0d0)*ckz*aco(m)*cez ! Ez
          cr=-pfakth*asi(m)*cephi ! Hr=...
          cf=-pfakth*aco(m)*cerho ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
        else
          cr=aco(m)*cigr*cerho   ! Er=...
          cf=-asi(m)*cigr*cephi  ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          A(3,kPa)=A(3,kPa)+aco(m)*cez          ! Ez
          cr=asi(m)*cioeps*cephi ! Hr=...
          cf=aco(m)*cioeps*cerho ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
        end if
      end if
! e s m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          A(10,kPa)=A(10,kPa)+asi(m)*cez        ! V
          cr=-asi(m)*cerho        ! Er=...
          cf=-aco(m)*cephi        ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          A(3,kPa)=A(3,kPa)+(0.0d0,-1.0d0)*ckz*asi(m)*cez ! Ez
          cr=pfakth*aco(m)*cephi  ! Hr=...
          cf=-pfakth*asi(m)*cerho ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
        else
          cr=asi(m)*cigr*cerho    ! Er=...
          cf=aco(m)*cigr*cephi    ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          A(3,kPa)=A(3,kPa)+asi(m)*cez          ! Ez
          cr=-aco(m)*cioeps*cephi ! Hr=...
          cf=asi(m)*cioeps*cerho  ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
        end if
      end if
! h c m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          A(9,kPa)=A(9,kPa)+aco(m)*cez          ! Az
          cr=-asi(m)*cephi        ! Er=...
          cf=-aco(m)*cerho        ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*aco(m)*cerho  ! Hr=...
          cf=-pfakth*asi(m)*cephi ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
          A(6,kPa)=A(6,kPa)+pfakth*(0.0d0,-1.0d0)*ckz*aco(m)*cez ! Hz
        else
          cr=-pfakth*asi(m)*ciomu*cephi ! Er=...
          cf=-pfakth*aco(m)*ciomu*cerho ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*aco(m)*cigr*cerho   ! Hr=...
          cf=-pfakth*asi(m)*cigr*cephi  ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
          A(6,kPa)=A(6,kPa)+pfakth*aco(m)*cez   ! Hz
        end if
      end if
! h s m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          A(9,kPa)=A(9,kPa)+asi(m)*cez          ! Az
          cr=aco(m)*cephi         ! Er=...
          cf=-asi(m)*cerho        ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*asi(m)*cerho  ! Hr=...
          cf=pfakth*aco(m)*cephi  ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
          A(6,kPa)=A(6,kPa)+pfakth*(0.0d0,-1.0d0)*ckz*asi(m)*cez ! Hz
        else
          cr=pfakth*aco(m)*ciomu*cephi  ! Er=...
          cf=-pfakth*asi(m)*ciomu*cerho ! Ef=...
          A(1,kPa)=A(1,kPa)+phicos*cr-phisin*cf ! Ex
          A(2,kPa)=A(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*asi(m)*cigr*cerho   ! Hr=...
          cf=pfakth*aco(m)*cigr*cephi   ! Hf=...
          A(4,kPa)=A(4,kPa)+phicos*cr-phisin*cf ! Hx
          A(5,kPa)=A(5,kPa)+phisin*cr+phicos*cf ! Hy
          A(6,kPa)=A(6,kPa)+pfakth*asi(m)*cez   ! Hz
        end if
      end if
    end do
    if(Allocated(cBes)) DeAllocate(cBes)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(aco)) DeAllocate(aco)
  end Subroutine Multipole2D

  Subroutine Multipole2DC(kEx,kPa,iType,r,gc,rE1,iD,iHEk,minOrd,maxOrd,nStep,maxPar,nP,ixS,iyS,ximag,yimag,Ar)
! compute 2D multipole or Bessel expansion
    Implicit none
    Integer(4) kEx,kPa,nP,idum,idumy1,idumy2,idumy3,mPa,n,m,iEven,iOdd,ierr
    Complex(8), Allocatable :: cBes(:)
    Complex(8), Allocatable :: asi(:),aco(:)
    Complex(8) Ar(10,nParN)
    Complex(8) gc,ckap2,crkap,cigr,cioeps,ciomu,ceigz,cerho,cephi,cez,cr,cf,ckz
    Complex(8) rho,rn,phicos,phisin
    Real(8) rE1,rrho,r(3),pfakth,ximag,yimag,a,b,al,ai
    Real(8), external:: dsqrtc
    Integer(2) iD,iHEk,minOrd,maxOrd,nStep,maxPar,lf,iType,ixS,iyS
    idumy1=maxOrd+2
    idumy2=maxOrd+20
    idumy3=14
    if(Allocated(cBes)) DeAllocate(cBes)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(aco)) DeAllocate(aco)
    Allocate(cBes(0:idumy2),asi(0:idumy1),aco(0:idumy1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'2D-Multipole'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(.not.lExpTest) then
      if(abs(iType).eq.2) then !!!! Bessel type = 0 outside circle
        rrho=dsqrtc(r(1)**2+r(2)**2)
        if((rrho.gt.rE1).and.(rE1.gt.pSmall)) return 
      else                     !!!! Hankel type = 0 inside circle
        rrho=dsqrtc((r(1)-yimag)**2+(r(2)+ximag)**2)
        if((rrho.lt.rE1).and.(rE1.gt.pSmall)) return
        rrho=dsqrtc((r(1)+yimag)**2+(r(2)-ximag)**2)
        if((rrho.lt.rE1).and.(rE1.gt.pSmall)) return
      end if
    end if
    rho=(r(1)-(0.0d0,1.0d0)*ximag)**2+(r(2)-(0.0d0,1.0d0)*yimag)**2
    rho=sqrt(rho)
    if((abs(iType).ne.2).and.(abs(tExp(kEx)%rE(5)).gt.pSmall)) then ! cut line for complex origin multipoles
      al=tExp(kEx)%rE(5)*Pi/180.0d0
      rrho=ximag**2+yimag**2
      a=(r(1)*yimag-r(2)*ximag)/rrho
      b=(r(1)*ximag+r(2)*yimag)/rrho
      if(a.lt.0.0d0) then
        ai=datan2(b,(1.0d0+a))
      else
        ai=datan2(b,(1.0d0-a))
      end if
      if(((ai.gt.al).and.(ai.le.0.0d0)).or.((ai.lt.al).and.(ai.gt.0.0d0))) then
        rho=-rho
      end if
    end if
    if(abs(rho).gt.1.0d-150) then
      phicos=(r(1)-(0.0d0,1.0d0)*ximag)/rho
      phisin=(r(2)-(0.0d0,1.0d0)*yimag)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=(1.0d-150,0.0d0)
    end if
    if(lzPer) then
      ckz=Pi/dcFld
    else
      ckz=(2.0d0*Pi*kw0)*fcFld*gc
    end if
    ckap2=kcFld**2-ckz**2
    kapcFld=cdsqrt(ckap2)
    if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
    if((tExp(kEx)%iE(6)/10.eq.99).or.(tExp(kEx)%iE(6)/10.eq.69)) kapcFld=-kapcFld
    if((tExp(kEx)%iE(6)/10.eq.88).or.(tExp(kEx)%iE(6)/10.eq.68)) kapcFld=cdsqrt(ckap2)
    if((tExp(kEx)%iE(6)/10.eq.77).or.(tExp(kEx)%iE(6)/10.eq.67)) kapcFld=-cdsqrt(ckap2)
    crkap=rho*kapcFld
    cigr=(0.0d0,1.0d0)*ckz/rho
    cioeps=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Eps0*eDom(lf)/rho
    ciomu=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Mue0*uDom(lf)/rho
    ceigz=cdexp((0.0d0,1.0d0)*ckz*r(3))
    if(lMMPstat) then
      if(iType.eq.2) then
        cBes(0)=(1.0d0,0.0d0)
        cBes(1:idumy1)=(0.0d0,0.0d0)
        rn=(1.0d0,0.0d0)
        do n=1,idumy1
          if(abs(rho).gt.1.0d0) then
            if(abs(rn).gt.(pBig/abs(rho))) Exit
          end if
          rn=rn*rho
          cBes(n)=rn
        end do
      else
        cBes(0)=-log(rho)
        cBes(1:idumy1)=(0.0d0,0.0d0)
        rn=1.0d0
        do n=1,idumy1
          if(abs(rho).lt.1.0d0) then
            if(abs(rn).gt.(pBig*abs(rho))) Exit
          end if
          rn=rn/rho
          cBes(n)=rn
        end do
      endif
    else
      if(iType.eq.2) then
        call cj(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
      else if(tExp(kEx)%iE(6)/100.eq.6) then
        call ch2(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
      else
        call ch1(crkap,cBes(0),idumy1,idumy2,idumy3,ierr)
      endif
      if(ierr.gt.0) cBes=(0.0d0,0.0d0)
      if((ierr.ne.0).and.lDispWarn) then
        if(iType.eq.2) then
          write(*,*) 'WARNING: Error',ierr,' in Bessel expansion ',kEx,' crkap=',crkap
        else
          write(*,*) 'WARNING: Error',ierr,' in Hankel expansion ',kEx,' crkap=',crkap
        end if
        write(*,*) '         Field computation inaccurate'
        lDispWarn=.false.
      end if
    end if
    call sicomC(Int4(maxOrd),phisin,phicos,asi,aco)
! scaling
    pfakth=1.0d0/cdabs(cZw)
    if(minOrd.eq.0_2) then
! 2d expansion of degree 0
      if(lMMPstat) then
        if(iType.eq.2) then
          cerho=(0.0d0,0.0d0)
        else
          cerho=-ceigz/rho
        end if
        cez=cBes(0)*ceigz
      else
        cerho=-crkap*cBes(1)*ceigz
        cez=ckap2*cBes(0)*ceigz
      end if
! e c 0
      if((kPa.lt.mPa).and.(iHEk.ne.1).and.(ixS.ne.2).and.(iyS.ne.2)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          Ar(10,kPa)=Ar(10,kPa)+cez               ! V
          cr=-cerho        ! Er
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr           ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr           ! Ey
          Ar(3,kPa)=Ar(3,kPa)+(0.0d0,-1.0d0)*ckz*cez ! Ez
          cf=-pfakth*cerho ! Hf
          Ar(4,kPa)=Ar(4,kPa)-phisin*cf           ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phicos*cf           ! Hy
        else
          cr=cigr*cerho    ! Er
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr           ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr           ! Ey
          Ar(3,kPa)=Ar(3,kPa)+cez                 ! Ez
          cf=cioeps*cerho  ! Hf
          Ar(4,kPa)=Ar(4,kPa)-phisin*cf           ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phicos*cf           ! Hy
        end if
      end if
! h c 0
      if((kPa.lt.mPa).and.(iHEk.ne.0).and.(ixS.ne.1).and.(iyS.ne.1)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          Ar(9,kPa)=Ar(9,kPa)+cez                 ! Az
          cf=-cerho              ! Ef
          Ar(1,kPa)=Ar(1,kPa)-phisin*cf           ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phicos*cf           ! Ey
          cr=pfakth*cerho        ! Hr
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr           ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr           ! Hy
          Ar(6,kPa)=Ar(6,kPa)+pfakth*(0.0d0,-1.0d0)*ckz*cez ! Hz
        else
          cf=-pfakth*ciomu*cerho ! Ef
          Ar(1,kPa)=Ar(1,kPa)-phisin*cf           ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phicos*cf           ! Ey
          cr=pfakth*cigr*cerho   ! Hr
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr           ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr           ! Hy
          Ar(6,kPa)=Ar(6,kPa)+pfakth*cez          ! Hz
        end if
      end if
    end if
    lf=minOrd
    if(lf.eq.0) lf=nStep
    do m=lf,maxOrd,nStep
! 2d expansion of degree m
      if(kPa.ge.mPa) Exit
      iEven=modulo(m,2)+1
      iOdd=3-iEven
      if(lMMPstat) then
        cez=cBes(m)*ceigz
        if(iType.eq.2) then
          cerho=dble(m)*cBes(m-1)*ceigz
          cephi=dble(m)*cBes(m-1)*ceigz
        else
          cerho=dble(-m)*cBes(m+1)*ceigz
          cephi=dble(m)*cBes(m+1)*ceigz
        end if
      else
        cez=ckap2*cBes(m)*ceigz
        cephi=dble(m)*cBes(m)*ceigz
        cerho=cephi-crkap*cBes(m+1)*ceigz
      end if
! e c m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          Ar(10,kPa)=Ar(10,kPa)+aco(m)*cez        ! V
          cr=-aco(m)*cerho        ! Er=...
          cf=asi(m)*cephi         ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          Ar(3,kPa)=Ar(3,kPa)+(0.0d0,-1.0d0)*ckz*aco(m)*cez ! Ez
          cr=-pfakth*asi(m)*cephi ! Hr=...
          cf=-pfakth*aco(m)*cerho ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
        else
          cr=aco(m)*cigr*cerho   ! Er=...
          cf=-asi(m)*cigr*cephi  ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          Ar(3,kPa)=Ar(3,kPa)+aco(m)*cez          ! Ez
          cr=asi(m)*cioeps*cephi ! Hr=...
          cf=aco(m)*cioeps*cerho ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
        end if
      end if
! e s m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          Ar(10,kPa)=Ar(10,kPa)+asi(m)*cez        ! V
          cr=-asi(m)*cerho        ! Er=...
          cf=-aco(m)*cephi        ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          Ar(3,kPa)=Ar(3,kPa)+(0.0d0,-1.0d0)*ckz*asi(m)*cez ! Ez
          cr=pfakth*aco(m)*cephi  ! Hr=...
          cf=-pfakth*asi(m)*cerho ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
        else
          cr=asi(m)*cigr*cerho    ! Er=...
          cf=aco(m)*cigr*cephi    ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          Ar(3,kPa)=Ar(3,kPa)+asi(m)*cez          ! Ez
          cr=-aco(m)*cioeps*cephi ! Hr=...
          cf=asi(m)*cioeps*cerho  ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
        end if
      end if
! h c m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          Ar(9,kPa)=Ar(9,kPa)+aco(m)*cez          ! Az
          cr=-asi(m)*cephi        ! Er=...
          cf=-aco(m)*cerho        ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*aco(m)*cerho  ! Hr=...
          cf=-pfakth*asi(m)*cephi ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
          Ar(6,kPa)=Ar(6,kPa)+pfakth*(0.0d0,-1.0d0)*ckz*aco(m)*cez ! Hz
        else
          cr=-pfakth*asi(m)*ciomu*cephi ! Er=...
          cf=-pfakth*aco(m)*ciomu*cerho ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*aco(m)*cigr*cerho   ! Hr=...
          cf=-pfakth*asi(m)*cigr*cephi  ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
          Ar(6,kPa)=Ar(6,kPa)+pfakth*aco(m)*cez   ! Hz
        end if
      end if
! h s m
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPa=kPa+1
        nP=nP+1
        if(lMMPstat) then
          Ar(9,kPa)=Ar(9,kPa)+asi(m)*cez          ! Az
          cr=aco(m)*cephi         ! Er=...
          cf=-asi(m)*cerho        ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*asi(m)*cerho  ! Hr=...
          cf=pfakth*aco(m)*cephi  ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
          Ar(6,kPa)=Ar(6,kPa)+pfakth*(0.0d0,-1.0d0)*ckz*asi(m)*cez ! Hz
        else
          cr=pfakth*aco(m)*ciomu*cephi  ! Er=...
          cf=-pfakth*asi(m)*ciomu*cerho ! Ef=...
          Ar(1,kPa)=Ar(1,kPa)+phicos*cr-phisin*cf ! Ex
          Ar(2,kPa)=Ar(2,kPa)+phisin*cr+phicos*cf ! Ey
          cr=pfakth*asi(m)*cigr*cerho   ! Hr=...
          cf=pfakth*aco(m)*cigr*cephi   ! Hf=...
          Ar(4,kPa)=Ar(4,kPa)+phicos*cr-phisin*cf ! Hx
          Ar(5,kPa)=Ar(5,kPa)+phisin*cr+phicos*cf ! Hy
          Ar(6,kPa)=Ar(6,kPa)+pfakth*asi(m)*cez   ! Hz
        end if
      end if
    end do
    if(Allocated(cBes)) DeAllocate(cBes)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(aco)) DeAllocate(aco)
  end Subroutine Multipole2DC

  Subroutine PlaneWave2D(kEx,kPa,r,iD,iHEk,isT,alpha,maxPar,nP,A)
! compute plane wave - <= 2 parameters!
    Implicit none
    Integer(4) kEx,kPa,nP,mPa
    Complex(8) A(10,nParN)
    Complex(8) cf,cky,ckz,ckap2,cq,pfakte
    Real(8) r(3),alpha
    Integer(2) iD,iHEk,isT,maxPar,lf
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(lzPer) then
      ckz=Pi/dcFld
    else
      ckz=(2.0d0*Pi*kw0)*fcFld*gcFld
    end if
    ckap2=kcFld**2-ckz**2
    kapcFld=cdsqrt(ckap2)
    if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
    if(tExp(kEx)%iE(6)/10.eq.99) kapcFld=-kapcFld
    if(tExp(kEx)%iE(6)/10.eq.88) kapcFld=cdsqrt(ckap2)
    if(tExp(kEx)%iE(6)/10.eq.77) kapcFld=-cdsqrt(ckap2)
    cq=(1.0d0,0.0d0)
    if(isT.gt.0) then
      cky=kapcFld*dsin(alpha*Pi/180.0d0)
      cq=sqrt(kcFld.div.cky)
    end if
! scaling
    pfakte=dsqrt(cdabs(cZw))*cq
    if((kPa.lt.mPa).and.(iHEk.ne.1)) then
      kPa=kPa+1
      nP=nP+1
      if(cdabs(kapcFld).lt.1.0d-100) then
        cf=pfakte*cdexp((0.0d0,1.0d0)*(ckz*r(3)))
        A(1,kPa)=A(1,kPa)+cf
        A(5,kPa)=A(5,kPa)+cf/cZw
      else
        cf=pfakte*cdexp((0.0d0,1.0d0)*(kapcFld*r(1)+ckz*r(3)))
        A(1,kPa)=A(1,kPa)+cf*ckz/kcFld
        A(3,kPa)=A(3,kPa)-cf*kapcFld/kcFld
        A(5,kPa)=A(5,kPa)+cf/cZw
      end if
    end if
    if((kPa.lt.mPa).and.(iHEk.ne.0)) then
      kPa=kPa+1
      nP=nP+1
      if(cdabs(kapcFld).lt.1.0d-100) then
        cf=pfakte*cdexp((0.0d0,1.0d0)*(ckz*r(3)))
        A(2,kPa)=A(2,kPa)-cf
        A(4,kPa)=A(4,kPa)+cf/cZw
      else
        cf=pfakte*cdexp((0.0d0,1.0d0)*(kapcFld*r(1)+ckz*r(3)))
        A(2,kPa)=A(2,kPa)-cf
        A(4,kPa)=A(4,kPa)+cf*ckz/(cZw*kcFld)
        A(6,kPa)=A(6,kPa)-cf*kapcFld/(cZw*kcFld)
      end if
    end if
  end Subroutine PlaneWave2D

  Subroutine PlaneWave3D(kEx,kPa,A,r,iD,iHEk,isT,costheta,maxPar,nP,ax,am,da,na,no)
! compute plane wave - <= 2 parameters!
    Implicit none
    Real(8) r(3),costheta,dda,ra(3),ex(3),ey(3),f,a0,fi,vr(3),vi(3)
    Real(8), optional:: ax(3),am,da
    Integer(2), optional:: na,no
    Complex(8) A(10,nParN)
    Complex(8) cf,E1(3),H1(3),E2(3),H2(3)
    Integer(4) mPa,kEx,kPa,nP
    Integer(2) iD,iHEk,isT,maxPar,lf,k,n
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(Present(no)) then
      n=max(1,na)
      E1(1:3)=(0.0d0,0.0d0)
      H1(1:3)=(0.0d0,0.0d0)
      E2(1:3)=(0.0d0,0.0d0)
      H2(1:3)=(0.0d0,0.0d0)
      f=1.0d0/Dble(n)
      if(n.gt.1) then
        dda=da/Dble(n)
        a0=am-0.5d0*da+0.5d0*dda
      else
        dda=0.0d0
        a0=am
      end if
      fi=a0
      do k=1,n
        ra=Rot3DVecAxis0(ax,ex,ey,r,fi)
        cf=dsqrt(cdabs(cZw))*cdexp((0.0d0,1.0d0)*kcFld*ra(3))
        if(isT.gt.0) cf=cf/dsqrt(dabs(costheta))
        if(no.lt.0) then
          cf=cf*f*dsin(-Dble(no)*fi*Pi/180.0d0)
        else
          cf=cf*f*dcos(Dble(no)*fi*Pi/180.0d0)
        end if
        if(iHEk.ne.1) then
          vr(2:3)=0.0d0
          vi(2:3)=0.0d0
          vr(1)=Dble(cf)
          vi(1)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          E1(1:3)=E1(1:3)+DCmplx(vr(1:3),vi(1:3))
          vr(1:3)=0.0d0
          vi(1:3)=0.0d0
          vr(2)=Dble(cf)
          vi(2)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          H1(1:3)=H1(1:3)+DCmplx(vr(1:3),vi(1:3))/cZw
        end if
        if(iHEk.ne.0) then
          vr(1:3)=0.0d0
          vi(1:3)=0.0d0
          vr(2)=Dble(cf)
          vi(2)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          E2(1:3)=E2(1:3)+DCmplx(vr(1:3),vi(1:3))
          vr(2:3)=0.0d0
          vi(2:3)=0.0d0
          vr(1)=Dble(cf)
          vi(1)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          H2(1:3)=H2(1:3)+DCmplx(vr(1:3),vi(1:3))/cZw
        end if
        fi=fi+dda
      end do
      if((kPa.lt.mPa).and.(iHEk.ne.1)) then
        kPa=kPa+1
        nP=nP+1
        A(1:3,kPa)=A(1:3,kPa)+E1(1:3)
        A(4:6,kPa)=A(4:6,kPa)+H1(1:3)
      end if
      if((kPa.lt.mPa).and.(iHEk.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        A(1:3,kPa)=A(1:3,kPa)-E2(1:3)
        A(4:6,kPa)=A(4:6,kPa)+H2(1:3)
      end if
    else
      cf=dsqrt(cdabs(cZw))*cdexp((0.0d0,1.0d0)*kcFld*r(3))
      if(isT.gt.0) cf=cf/dsqrt(dabs(costheta))
      if((kPa.lt.mPa).and.(iHEk.ne.1)) then
        kPa=kPa+1
        nP=nP+1
        A(1,kPa)=A(1,kPa)+cf     ! Ex=exp(i*k*z)
        A(5,kPa)=A(5,kPa)+cf/cZw ! Hy=Ex/Zw
      end if
      if((kPa.lt.mPa).and.(iHEk.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        A(2,kPa)=A(2,kPa)-cf     ! Ey=exp(i*k*z)
        A(4,kPa)=A(4,kPa)+cf/cZw ! Hx=Ey/Zw
      end if
    end if
  end Subroutine PlaneWave3D

  Subroutine PlaneWave3Da(kEx,kPa,A,r,iD,iHEk,isT,costheta,maxPar,nP,ax,am,da,na,no)
! compute plane wave - <= 2 parameters!
    Implicit none
    Real(8) r(3),costheta,dda,ra(3),ex(3),ey(3),f,a0,fi,vr(3),vi(3)
    Real(8), optional:: ax(3),am,da
    Integer(2), optional:: na,no
    Complex(8) A(10,nParN)
    Complex(8) cf,E1(3),H1(3),E2(3),H2(3)
    Integer(4) mPa,kEx,kPa,nP
    Integer(2) iD,iHEk,isT,maxPar,lf,k,n
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(Present(no)) then
      n=max(1,na)
      E1(1:3)=(0.0d0,0.0d0)
      H1(1:3)=(0.0d0,0.0d0)
      E2(1:3)=(0.0d0,0.0d0)
      H2(1:3)=(0.0d0,0.0d0)
      f=1.0d0/Dble(n)
      if(n.gt.1) then
        dda=da/Dble(n-1)
        a0=am-0.5d0*da
      else
        dda=0.0d0
        a0=am
      end if
      ra=Rot3DVecAxis0(ax,ex,ey,r,a0)
      fi=a0
      do k=1,n
        cf=dsqrt(cdabs(cZw))*cdexp((0.0d0,1.0d0)*kcFld*ra(3))
        if(isT.gt.0) cf=cf/dsqrt(dabs(costheta))
        if(no.lt.0) then
          cf=cf*f*dsin(-Dble(no)*fi*Pi/180.0d0)
        else
          cf=cf*f*dcos(Dble(no)*fi*Pi/180.0d0)
        end if
        if(iHEk.ne.1) then
          vr(2:3)=0.0d0
          vi(2:3)=0.0d0
          vr(1)=Dble(cf)
          vi(1)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          E1(1:3)=E1(1:3)+DCmplx(vr(1:3),vi(1:3))
          vr(1:3)=0.0d0
          vi(1:3)=0.0d0
          vr(2)=Dble(cf)
          vi(2)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          H1(1:3)=H1(1:3)+DCmplx(vr(1:3),vi(1:3))
        end if
        if(iHEk.ne.0) then
          vr(1:3)=0.0d0
          vi(1:3)=0.0d0
          vr(2)=Dble(cf)
          vi(2)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          E2(1:3)=E2(1:3)+DCmplx(vr(1:3),vi(1:3))
          vr(2:3)=0.0d0
          vi(2:3)=0.0d0
          vr(1)=Dble(cf)
          vi(1)=Dimag(cf)
          vr=Rot3DVecAxis0(ax,ex,ey,vr,-fi)
          vi=Rot3DVecAxis0(ax,ex,ey,vi,-fi)
          H2(1:3)=H2(1:3)+DCmplx(vr(1:3),vi(1:3))
        end if
        ra=Rot3DVecAxis(ax,ex,ey,ra,dda)
        fi=a0+dda
      end do
      if((kPa.lt.mPa).and.(iHEk.ne.1)) then
        kPa=kPa+1
        nP=nP+1
        A(1:3,kPa)=A(1:3,kPa)+E1(1:3)
        A(4:6,kPa)=A(4:6,kPa)+H1(1:3)
      end if
      if((kPa.lt.mPa).and.(iHEk.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        A(1:3,kPa)=A(1:3,kPa)-E2(1:3)
        A(4:6,kPa)=A(4:6,kPa)+H2(1:3)
      end if
    end if
    cf=dsqrt(cdabs(cZw))*cdexp((0.0d0,1.0d0)*kcFld*r(3))
    if(isT.gt.0) cf=cf/dsqrt(dabs(costheta))
    if((kPa.lt.mPa).and.(iHEk.ne.1)) then
      kPa=kPa+1
      nP=nP+1
      A(1,kPa)=A(1,kPa)+cf     ! Ex=exp(i*k*z)
      A(5,kPa)=A(5,kPa)+cf/cZw ! Hy=Ex/Zw
    end if
    if((kPa.lt.mPa).and.(iHEk.ne.0)) then
      kPa=kPa+1
      nP=nP+1
      A(2,kPa)=A(2,kPa)-cf     ! Ey=exp(i*k*z)
      A(4,kPa)=A(4,kPa)+cf/cZw ! Hx=Ey/Zw
    end if
  end Subroutine PlaneWave3Da

  Subroutine GaussBeam3D(kEx,kPa,r,iD,radius,nord,maxPar,nP,A)
! nord>=0: compute Gaussian beam - <= 1 parameters!
! two gaussian beams with waist radius and a maximumintensity of 1 per beam
! Davis beam approximation ->Lock & Hovenac, Am. J. Phys. 61(8), August 1993
! e-field-vectors in x-z-plane, h-field-vectors in y-direction
! propagation in x-z-plane
! nord<0: electron beam
    Implicit Real(8) (a-h,o-z)
    Complex(8) A(10,nParN),Er,Ez,Hf,KBes(0:25),omega,const1,const2,v
    Real(8) r(3),g,rho,xr,yr
    Integer(4) mPa,kEx,nmax,ier,idumy1,idumy2,idumy3
    Integer(2) iD,maxPar,lf
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(kPa.lt.mPa) then
      kPa=kPa+1
      nP=nP+1
      x0=r(1)
      y0=r(2)
      z0=r(3)
      czr=Dble(cZw)
      czi=dimag(cZw)
      ckr=Dble(kcFld)
      cki=dimag(kcFld)
      if(nord.lt.0) then ! electron beam
        omega=2.0d0*Pi*fcFld
        v=radius*omega/kcFld ! here: radius is v/c
        g=1.0d0/sqrt(1-radius*radius)
        rho=sqrt(x0*x0+y0*y0)
        if(rho.lt.tExp(kEx)%rE(2)) then
          x0=tExp(kEx)%rE(2)
          y0=0
          rho=x0
        end if
        const1=omega*rho/(g*v)
        const2=-cdexp((0.0d0,1.0d0)*omega*z0/v)*2.0d0*omega*Qe/(Eps0*eDom(lf)*g*v*v)
        nmax=2
        idumy1=nmax+2
        idumy2=nmax+20
        idumy3=14
        call ck(const1,KBes(0),idumy1,idumy2,idumy3,ier)
        Er=const2*KBes(1)
        Ez=-const2*(0.0d0,1.0d0)*KBes(0)/g
        Hf=const2*KBes(1)*(1.0d0-1.0d0/(g*g))/(Mue0*uDom(lf)*v)
        xr=x0/rho
        yr=y0/rho
        A(1,kPa)=A(1,kPa)+Er*xr
        A(2,kPa)=A(2,kPa)+Er*yr
        A(3,kPa)=A(3,kPa)+Ez
        A(4,kPa)=A(4,kPa)-Hf*yr
        A(5,kPa)=A(5,kPa)+Hf*xr
      else ! Gauss beam
        if(nord.gt.5) nord=5
        n0=1
        n1=0
        n2=0
        n3=0
        n4=0
        n5=0
        if(nord.gt.0) n1=1
        if(nord.gt.1) n2=1
        if(nord.gt.2) n3=1
        if(nord.gt.3) n4=1
        if(nord.gt.4) n5=1
        if((radius*ckr).lt.2.0) radius=2.0/ckr
  ! A:
        t1=ckr**2
        t2=cki**2
        t3=t1+t2
        t4=1/t3
        t6=radius**2
        t7=1/t6
        t8=z0**2
        t9=t3**2
        t10=1/t9
        t11=t6**2
        t12=1/t11
        t13=t10*t12
        t15=t8*t1*t13
        t16=4*t15
        t17=t4*t7
        t19=z0*cki*t17
        t21=-1-2*t19
        t22=t21**2
        t24=1/(t16+t22)
        t25=x0**2
        t27=y0**2
        t29=t25*t7+t27*t7
        t32=exp(t29*t21*t24)
        t38=2*t29*z0*ckr*t4*t7*t24
        t39=cos(t38)
        t42=exp(-cki*z0)
        t43=ckr*z0
        t44=cos(t43)
        t46=t1-t2
        t47=t46**2
        t48=t1*t2
        t51=1/(t47+4*t48)
        t53=z0*ckr*t17
        t58=t8*ckr*t10*t12*cki
        t60=-4*t53-8*t58
        t63=t8*t2*t13
        t65=t16-1-4*t19-4*t63
        t66=t65**2
        t67=t60**2
        t69=1/(t66+t67)
        t72=t29**2
        t73=t8*z0
        t74=t1*ckr
        t76=1/t9/t3
        t78=1/t11/t6
        t79=t76*t78
        t81=t73*t74*t79
        t88=t73*ckr*t76*t78*t2
        t90=8*t81-6*t53-24*t58-24*t88
        t91=t90**2
        t96=t73*t1*t76*t78*cki
        t100=t2*cki
        t102=t73*t100*t79
        t104=-12*t15-24*t96+1+6*t19+12*t63+8*t102
        t105=t104**2
        t107=1/(t91+t105)
        t113=t51*t7*(t29*t60*t69-t72*t90*t107)
        t123=t51*t7*(-t29*t65*t69-t72*t104*t107)
        t129=t1**2
        t131=t2**2
        t132=t129-6*t48+t131
        t133=t132**2
        t138=4*t74*cki-4*ckr*t100
        t139=t138**2
        t141=1/(t133+t139)
        t145=t8**2
        t146=t9**2
        t147=1/t146
        t148=t11**2
        t149=1/t148
        t153=t145*t74*t147*t149*cki
        t159=t145*ckr*t147*t149*t100
        t161=8*t53+48*t58-32*t81-64*t153+96*t88+64*t159
        t165=t147*t149
        t167=t145*t129*t165
        t173=t145*t1*t147*t149*t2
        t177=t145*t131*t165
        t179=1-24*t15+8*t19+24*t63+16*t167-96*t96-96*t173+32*t102+16*t177
        t180=t179**2
        t181=t161**2
        t183=1/(t180+t181)
        t187=t72*t29
        t194=t145*z0
        t195=t129*ckr
        t197=1/t146/t3
        t199=1/t148/t6
        t200=t197*t199
        t202=t194*t195*t200
        t207=t194*t74*t197*t199*t2
        t212=t194*ckr*t197*t199*t131
        t214=10*t53+80*t58-80*t81+240*t88-320*t153+320*t159+32*t202-320*t207+160*t212
        t215=t214**2
        t226=t194*t129*t197*t199*cki
        t232=t194*t1*t197*t199*t100
        t235=t131*cki
        t237=t194*t235*t200
        t240=-1-10*t19+40*t15+240*t96-40*t63-80*t102-80*t167-160*t226+480*t173+320*t232-80*t177-32*t237
        t241=t240**2
        t243=1/(t215+t241)
        t247=t72**2
        t256=t145*t8
        t258=1/t146/t9
        t260=1/t148/t11
        t279=-12*t53-120*t58+160*t81+960*t153-480*t88-960*t159-192*t202-384*t256*t195*t258*t260*cki+1920*t207+1280*t256*&
        & t74*t258*t260*t100-960*t212-384*t256*ckr*t258*t260*t235
        t293=t258*t260
        t312=-1-192*t237-12*t19+60*t15-60*t63+480*t96-160*t102-240*t167+1440*t173-240*t177-960*t226+1920*t232+64*t256*&
        & t129*t1*t293-960*t256*t129*t258*t260*t2+960*t256*t1*t258*t260*t131-64*t256*t131*t2*t293
        t313=t312**2
        t314=t279**2
        t316=1/(t313+t314)
        t322=t141*t12*(-2*t72*t161*t183+3*t187*t214*t243+t247*t279*t316/2)
        t335=t141*t12*(2*t72*t179*t183+3*t187*t240*t243-t247*t312*t316/2)
        t340=n2*(t46*t113-2*ckr*cki*t123)+n4*(t132*t322-t138*t335)
        t343=sin(t43)
        t354=n0+n2*(t46*t123+2*ckr*cki*t113)+n4*(t132*t335+t138*t322)
        t357=t42*t44*t340+t42*t343*t354
        t360=sin(t38)
        t366=t42*t44*t354-t42*t343*t340
        t370=t24*(t32*t39*t357+t32*t360*t366)
        t382=t24*(t32*t39*t366-t32*t360*t357)
        Are=2*z0*ckr*t4*t7*t370-t21*t382
        Aim=-2*z0*ckr*t4*t7*t382-t21*t370
  ! B:
        t1=ckr**2
        t2=cki**2
        t3=t1+t2
        t4=1/t3
        t6=radius**2
        t7=1/t6
        t8=z0**2
        t9=t3**2
        t10=1/t9
        t11=t6**2
        t12=1/t11
        t13=t10*t12
        t15=t8*t1*t13
        t16=4*t15
        t17=t4*t7
        t19=z0*cki*t17
        t21=-1-2*t19
        t22=t21**2
        t24=1/(t16+t22)
        t25=x0**2
        t27=y0**2
        t29=t25*t7+t27*t7
        t32=exp(t29*t21*t24)
        t38=2*t29*z0*ckr*t4*t7*t24
        t39=cos(t38)
        t42=exp(-cki*z0)
        t43=ckr*z0
        t44=cos(t43)
        t46=t1-t2
        t47=t46**2
        t48=t1*t2
        t51=1/(t47+4*t48)
        t53=z0*ckr*t17
        t58=t8*ckr*t10*t12*cki
        t60=-4*t53-8*t58
        t63=t8*t2*t13
        t65=t16-1-4*t19-4*t63
        t66=t65**2
        t67=t60**2
        t69=1/(t66+t67)
        t72=t51*t7*t60*t69
        t77=t51*t7*t65*t69
        t84=t1**2
        t86=t2**2
        t87=t84-6*t48+t86
        t88=t87**2
        t89=t1*ckr
        t92=t2*cki
        t95=4*t89*cki-4*ckr*t92
        t96=t95**2
        t98=1/(t88+t96)
        t101=t8*z0
        t103=1/t9/t3
        t105=1/t11/t6
        t106=t103*t105
        t108=t101*t89*t106
        t110=t8**2
        t111=t9**2
        t112=1/t111
        t113=t11**2
        t114=1/t113
        t118=t110*t89*t112*t114*cki
        t123=t101*ckr*t103*t105*t2
        t128=t110*ckr*t112*t114*t92
        t130=8*t53+48*t58-32*t108-64*t118+96*t123+64*t128
        t134=t112*t114
        t136=t110*t84*t134
        t141=t101*t1*t103*t105*cki
        t146=t110*t1*t112*t114*t2
        t149=t101*t92*t106
        t152=t110*t86*t134
        t154=1-24*t15+8*t19+24*t63+16*t136-96*t141-96*t146+32*t149+16*t152
        t155=t154**2
        t156=t130**2
        t158=1/(t155+t156)
        t162=t29**2
        t169=t110*z0
        t172=1/t111/t3
        t174=1/t113/t6
        t175=t172*t174
        t189=10*t53+80*t58-80*t108+240*t123-320*t118+320*t128+32*t169*t84*ckr*t175-320*t169*t89*t172*t174*t2+160*t169*ckr* &
        & t172*t174*t86
        t190=t189**2
        t215=-1-10*t19+40*t15+240*t141-40*t63-80*t149-80*t136-160*t169*t84*t172*t174*cki+480*t146+320*t169*t1*t172*t174* &
        & t92-80*t152-32*t169*t86*cki*t175
        t216=t215**2
        t218=1/(t190+t216)
        t224=t98*t12*(-8*t29*t130*t158+2*t162*t189*t218)
        t234=t98*t12*(8*t29*t154*t158+2*t162*t215*t218)
        t239=-2*n2*(-t46*t72-2*ckr*cki*t77)+n4*(t87*t224-t95*t234)
        t242=sin(t43)
        t254=-2*n2*(t46*t77-2*ckr*cki*t72)+n4*(t87*t234+t95*t224)
        t257=t42*t44*t239+t42*t242*t254
        t260=sin(t38)
        t266=t42*t44*t254-t42*t242*t239
        t270=t24*(t32*t39*t257+t32*t260*t266)
        t282=t24*(t32*t39*t266-t32*t260*t257)
        Bre=2*z0*ckr*t4*t7*t270-t21*t282
        Bim=-2*z0*ckr*t4*t7*t282-t21*t270
  ! C:
        t1=ckr**2
        t2=cki**2
        t3=t1+t2
        t4=1/t3
        t5=radius
        t6=t5**2
        t7=1/t6
        t8=z0**2
        t9=t3**2
        t10=1/t9
        t11=t6**2
        t12=1/t11
        t13=t10*t12
        t15=t8*t1*t13
        t17=t4*t7
        t19=z0*cki*t17
        t21=-1-2*t19
        t22=t21**2
        t24=1/(4*t15+t22)
        t25=x0**2
        t27=y0**2
        t29=t25*t7+t27*t7
        t30=t21*t24
        t32=exp(t29*t30)
        t38=2*t29*z0*ckr*t4*t7*t24
        t39=cos(t38)
        t42=exp(-cki*z0)
        t43=ckr*z0
        t44=cos(t43)
        t47=t4/t5*t30
        t51=1/t6/t5
        t61=t1*ckr
        t64=t61-3*ckr*t2
        t65=t64**2
        t68=t2*cki
        t70=3*t1*cki-t68
        t71=t70**2
        t73=1/(t65+t71)
        t75=t8*z0
        t76=t9*t3
        t77=1/t76
        t78=t11*t6
        t79=1/t78
        t83=t75*t1*t77*t79*cki
        t87=t8*t2*t13
        t89=t77*t79
        t91=t75*t68*t89
        t93=-12*t15-24*t83+1+6*t19+12*t87+8*t91
        t95=t75*t61*t89
        t98=z0*ckr*t17
        t103=t8*ckr*t10*t12*cki
        t108=t75*ckr*t77*t79*t2
        t110=8*t95-6*t98-24*t103-24*t108
        t111=t110**2
        t112=t93**2
        t114=1/(t111+t112)
        t118=t29**2
        t122=t8**2
        t123=t1**2
        t124=t9**2
        t125=1/t124
        t126=t11**2
        t127=1/t126
        t128=t125*t127
        t130=t122*t123*t128
        t136=t122*t1*t125*t127*t2
        t139=t2**2
        t141=t122*t139*t128
        t143=1-24*t15+8*t19+24*t87+16*t130-96*t83-96*t136+32*t91+16*t141
        t144=t143**2
        t151=t122*t61*t125*t127*cki
        t157=t122*ckr*t125*t127*t68
        t159=8*t98+48*t103-32*t95-64*t151+96*t108+64*t157
        t160=t159**2
        t162=1/(t144+t160)
        t168=t73*t51*(-6*t29*t93*t114+2*t118*t143*t162)
        t178=t73*t51*(6*t29*t110*t114+2*t118*t159*t162)
        t183=t123*ckr
        t188=t183-10*t61*t2+5*ckr*t139
        t189=t188**2
        t194=t139*cki
        t195=5*t123*cki-10*t1*t68+t194
        t196=t195**2
        t198=1/(t189+t196)
        t200=1/t11/t5
        t208=t122*z0
        t210=1/t124/t3
        t212=1/t126/t6
        t216=t208*t123*t210*t212*cki
        t222=t208*t1*t210*t212*t68
        t225=t210*t212
        t227=t208*t194*t225
        t230=-1-10*t19+40*t15+240*t83-40*t87-80*t91-80*t130-160*t216+480*t136+320*t222-80*t141-32*t227
        t238=t208*t183*t225
        t243=t208*t61*t210*t212*t2
        t248=t208*ckr*t210*t212*t139
        t250=10*t98+80*t103-80*t95+240*t108-320*t151+320*t157+32*t238-320*t243+160*t248
        t251=t250**2
        t252=t230**2
        t254=1/(t251+t252)
        t258=t118*t29
        t271=t122*t8
        t272=t123*t1
        t274=1/t124/t9
        t276=1/t126/t11
        t277=t274*t276
        t279=t271*t272*t277
        t284=t271*t123*t274*t276*t2
        t289=t271*t1*t274*t276*t139
        t291=t139*t2
        t293=t271*t291*t277
        t296=-1-192*t227-12*t19+60*t15-60*t87+480*t83-160*t91-240*t130+1440*t136-240*t141-960*t216+1920*t222+64*t279-960* &
        & t284+960*t289-64*t293
        t297=t296**2
        t309=t271*t183*t274*t276*cki
        t315=t271*t61*t274*t276*t68
        t321=t271*ckr*t274*t276*t194
        t324=-12*t98-120*t103+160*t95+960*t151-480*t108-960*t157-192*t238-384*t309+1920*t243+1280*t315-960*t248-384*t321
        t325=t324**2
        t327=1/(t297+t325)
        t331=t118**2
        t343=t122*t75
        t345=1/t124/t76
        t347=1/t126/t78
        t368=t345*t347
        t374=1-6720*t222+560*t141+672*t227+3360*t216-3360*t136+560*t130+84*t87-84*t15-840*t83+280*t91-896*t343*t272*t345* &
        & t347*cki-448*t279+4480*t343*t123*t345*t347*t68-6720*t289-2688*t343*t1*t345*t347*t194+6720*t284+448*t293+128* &
        & t343*t139*t68*t368+14*t19
        t408=-14*t98-168*t103+280*t95-840*t108+2240*t151-2240*t157-672*t238+6720*t243-3360*t248-2688*t309+8960*t315-2688* &
        & t321-896*t343*t291*t345*t347*ckr-2688*t343*t2*t345*t347*t183+4480*t343*t139*t345*t347*t61+128*t343*t123*t61*t368
        t409=t408**2
        t410=t374**2
        t412=1/(t409+t410)
        t418=t198*t200*(20*t118*t230*t254-10*t258*t296*t327-t331*t374*t412)
        t430=t198*t200*(-20*t118*t250*t254-10*t258*t324*t327+t331*t408*t412)
        t435=-2*n1*(-ckr*t47-2*cki*t10*t51*z0*ckr*t24)+n3*(t64*t168-t70*t178)+n5*(t188*t418-t195*t430)
        t438=sin(t43)
        t457=-2*n1*(2*t1*t10*t51*z0*t24-cki*t47)+n3*(t64*t178+t70*t168)+n5*(t188*t430+t195*t418)
        t460=t42*t44*t435+t42*t438*t457
        t463=sin(t38)
        t469=t42*t44*t457-t42*t438*t435
        t473=t24*(t32*t39*t460+t32*t463*t469)
        t485=t24*(t32*t39*t469-t32*t463*t460)
        Cre=2*z0*ckr*t4*t7*t473-t21*t485
        Cim=-2*z0*ckr*t4*t7*t485-t21*t473
  ! Ex:
        t1=x0**2
        t3=radius**2
        t5=t1/t3
        Exre=Are+Bre*t5
        Exim=Aim+Bim*t5
  ! Ey:
        t1=ckr**2
        t2=cki**2
        t3=t1+t2
        t4=1/t3
        t6=radius**2
        t7=1/t6
        t8=z0**2
        t9=t3**2
        t10=1/t9
        t11=t6**2
        t12=1/t11
        t13=t10*t12
        t15=t8*t1*t13
        t16=4*t15
        t17=t4*t7
        t19=z0*cki*t17
        t21=-1-2*t19
        t22=t21**2
        t24=1/(t16+t22)
        t25=x0**2
        t27=y0**2
        t29=t25*t7+t27*t7
        t32=exp(t29*t21*t24)
        t38=2*t29*z0*ckr*t4*t7*t24
        t39=cos(t38)
        t42=exp(-cki*z0)
        t43=ckr*z0
        t44=cos(t43)
        t46=t1-t2
        t47=t46**2
        t48=t1*t2
        t51=1/(t47+4*t48)
        t53=z0*ckr*t17
        t58=t8*ckr*t10*t12*cki
        t60=-4*t53-8*t58
        t63=t8*t2*t13
        t65=t16-1-4*t19-4*t63
        t66=t65**2
        t67=t60**2
        t70=x0*y0
        t71=1/(t66+t67)*t70
        t74=t51*t12*t60*t71
        t79=t51*t12*t65*t71
        t86=t1**2
        t88=t2**2
        t89=t86-6*t48+t88
        t90=t89**2
        t91=t1*ckr
        t94=t2*cki
        t97=4*t91*cki-4*ckr*t94
        t98=t97**2
        t100=1/(t90+t98)
        t102=1/t11/t6
        t105=t8*z0
        t107=1/t9/t3
        t108=t107*t102
        t110=t105*t91*t108
        t112=t8**2
        t113=t9**2
        t114=1/t113
        t115=t11**2
        t116=1/t115
        t120=t112*t91*t114*t116*cki
        t125=t105*t107*t102*t2*ckr
        t130=t112*ckr*t114*t116*t94
        t132=8*t53+48*t58-32*t110-64*t120+96*t125+64*t130
        t136=t114*t116
        t138=t112*t86*t136
        t143=t105*t1*t107*t102*cki
        t148=t112*t1*t114*t116*t2
        t151=t105*t94*t108
        t154=t112*t88*t136
        t156=1-24*t15+8*t19+24*t63+16*t138-96*t143-96*t148+32*t151+16*t154
        t157=t156**2
        t158=t132**2
        t160=1/(t157+t158)
        t164=t29**2
        t171=t112*z0
        t174=1/t113/t3
        t176=1/t115/t6
        t177=t174*t176
        t191=10*t53+80*t58-80*t110+240*t125-320*t120+320*t130+32*t171*t86*ckr*t177-320*t171*t91*t174*t176*t2+160*t171*ckr* &
        & t174*t176*t88
        t192=t191**2
        t217=-1-10*t19+40*t15+240*t143-40*t63-80*t151-80*t138-160*t171*t86*t174*t176*cki+480*t148+320*t171*t1*t174*t176* &
        & t94-80*t154-32*t171*t88*cki*t177
        t218=t217**2
        t220=1/(t192+t218)
        t227=t100*t102*(-8*t29*t132*t160+2*t164*t191*t220)*t70
        t238=t100*t102*(8*t29*t156*t160+2*t164*t217*t220)*t70
        t243=-2*n2*(-t46*t74-2*ckr*t79*cki)+n4*(t89*t227-t97*t238)
        t246=sin(t43)
        t258=-2*n2*(t46*t79-2*ckr*cki*t74)+n4*(t89*t238+t97*t227)
        t261=t42*t44*t243+t42*t246*t258
        t264=sin(t38)
        t270=t42*t44*t258-t42*t246*t243
        t274=t24*(t32*t39*t261+t32*t264*t270)
        t286=t24*(t32*t39*t270-t32*t264*t261)
        Eyre=2*z0*ckr*t4*t7*t274-t21*t286
        Eyim=-2*z0*ckr*t4*t7*t286-t21*t274
  ! Ez:
        t2=1/radius
        Ezre=Cre*x0*t2
        Ezim=Cim*x0*t2
  ! Hx:
        t1=czr**2
        t2=czi**2
        t4=1/(t1+t2)
        t5=czr*t4
        t7=czi*t4
        Hxre=Eyre*t5+Eyim*t7
        Hxim=-Eyre*t7+Eyim*t5
  ! Hy:
        t1=y0**2
        t3=radius**2
        t5=t1/t3
        t7=Are+Bre*t5
        t8=czr**2
        t9=czi**2
        t11=1/(t8+t9)
        t12=czr*t11
        t15=Aim+Bim*t5
        t16=czi*t11
        Hyre=t7*t12+t15*t16
        Hyim=-t7*t16+t15*t12
  ! Hz:
        t2=1/radius
        t3=czr**2
        t4=czi**2
        t6=1/(t3+t4)
        t9=y0*t2*czr*t6
        t13=y0*t2*czi*t6
        Hzre=Cre*t9+Cim*t13
        Hzim=-Cre*t13+Cim*t9
        A(1,kPa)=A(1,kPa)+Dcmplx(Exre,ExIm)
        A(2,kPa)=A(2,kPa)+Dcmplx(Eyre,EyIm)
        A(3,kPa)=A(3,kPa)+Dcmplx(Ezre,EzIm)
        A(4,kPa)=A(4,kPa)+Dcmplx(Hxre,HxIm)
        A(5,kPa)=A(5,kPa)+Dcmplx(Hyre,HyIm)
        A(6,kPa)=A(6,kPa)+Dcmplx(Hzre,HzIm)
      end if
    end if
  end Subroutine GaussBeam3D

  Subroutine Harmonic2D(kEx,kPa,r,gc,iD,iHEk,ixT,iyT,izT,minOrd,maxOrd,nStep,maxPar,nP,A)
! Harmonic expansions of the form f=fx(m*kx0*x)*fy(ky*y)*exp(i*kz*z)
! f is either Ez or Hz.
! fx(.),fy(.) are sin(.), cos(.), exp(i*.), exp(-i*.)
    Implicit none
    Complex(8) A(10,nParN)
    Complex(8) gc,ckx0,ckx,cky,ckz,cOmEps,cOmMu,ckap2,ci,fx,fxs,fy,fys,fz,fzs,gx,gxs,gy,gys,gz,gzs
    Real(8) r(3),pfakte,pfakth,rE(2:5)
    Integer(4) mPa,kEx,kPa,nP,m
    Integer(2) iD,iHEk,ixT,iyT,izT,minOrd,maxOrd,nStep,maxPar,lf
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    rE(2:5)=0.0d0
    if(kEx.gt.0) then
      rE(2:5)=tExp(kEx)%rE(2:5)
    end if
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    ci=(0.0d0,1.0d0)
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(lzPer.and.lgcFld) then
      ckz=Pi/dcFld
    else
      ckz=(2.0d0*Pi*kw0)*fcFld*gc
    end if
    ckap2=kcFld**2-ckz**2
    cOmEps=(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    cOmMu=(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    if(ixT.gt.-1) then
      ckx0=Dcmplx(Pi/max(rE(2),1.0d-100),0.0d0)
    else
      ckx0=(2.0d0*Pi*kw0)*fcFld*Dcmplx(rE(2),rE(5))
    end if
! scaling
    pfakte=1.0d0/dsqrt(cdabs((2.0d0*Pi*eps0)*fcFld*eDom(lf)*ckap2*kcFld))
    pfakth=1.0d0/cdabs(cZw)
    do m=minOrd,maxOrd,nStep
! expansions of degree m
      ckx=Dble(m)*ckx0
      cky=cdsqrt(kcFld**2-ckx**2-ckz**2)
      if((abs(iyT).lt.5).and.(DImag(cky).lt.0.0d0)) cky=-cky
      if(((rE(3).lt.nSmall).and.(-rE(3).lt.dabs(r(1)))).or.((rE(3).gt.pSmall).and.(rE(3).gt.r(1)))) then
        fx=0.0d0
        fxs=0.0d0
        gx=0.0d0
        gxs=0.0d0
      else
        if(abs(ixT).eq.1) then
          fx=cdsin(ckx*r(1))
          fxs=ckx*cdcos(ckx*r(1))
          gx=cdcos(ckx*r(1))
          gxs=-ckx*cdsin(ckx*r(1))
        else if(abs(ixT).eq.2) then
          fx=cdcos(ckx*r(1))
          fxs=-ckx*cdsin(ckx*r(1))
          gx=cdsin(ckx*r(1))
          gxs=ckx*cdcos(ckx*r(1))
        else if(abs(ixT).eq.3) then
          fx=cdexp(ci*ckx*r(1))
          fxs=ci*ckx*fx
          gx=fx
          gxs=fxs
        else
          fx=cdexp(-ci*ckx*r(1))
          fxs=-ci*ckx*fx
          gx=fx
          gxs=fxs
        end if
      end if
      if(((rE(4).lt.nSmall).and.(-rE(4).lt.dabs(r(2)))).or.((rE(4).gt.pSmall).and.(rE(4).gt.r(2)))) then
        fy=0.0d0
        fys=0.0d0
        gy=0.0d0
        gys=0.0d0
      else
        if(mod(abs(iyT),4).eq.1) then
          fy=cdsin(cky*r(2))
          fys=cky*cdcos(cky*r(2))
          gy=cdcos(cky*r(2))
          gys=-cky*cdsin(cky*r(2))
        else if(mod(abs(iyT),4).eq.2) then
          fy=cdcos(cky*r(2))
          fys=-cky*cdsin(cky*r(2))
          gy=cdsin(cky*r(2))
          gys=cky*cdcos(cky*r(2))
        else if(mod(abs(iyT),4).eq.3) then
          fy=cdexp(ci*cky*r(2))
          fys=ci*cky*fy
          gy=fy
          gys=fys
        else
          fy=cdexp(-ci*cky*r(2))
          fys=-ci*cky*fy
          gy=fy
          gys=fys
        end if
      end if
      if(abs(izT).eq.1) then
        fz=cdsin(ckz*r(3))
        fzs=ckz*cdcos(ckz*r(3))
        gz=cdcos(ckz*r(3))
        gzs=-ckz*cdsin(ckz*r(3))
      else if(abs(izT).eq.2) then
        fz=cdcos(ckz*r(3))
        fzs=-ckz*cdsin(ckz*r(3))
        gz=cdsin(ckz*r(3))
        gzs=ckz*cdcos(ckz*r(3))
      else if(abs(izT).eq.4) then
        fz=cdexp(-ci*ckz*r(3))
        fzs=-ci*ckz*fz
        gz=fz
        gzs=fzs
      else ! default
        fz=cdexp(ci*ckz*r(3))
        fzs=ci*ckz*fz
        gz=fz
        gzs=fzs
      end if
! e
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.1).and.(m.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        A(1,kPa)=A(1,kPa)+pfakte*fxs*fy*fzs          ! Ex
        A(2,kPa)=A(2,kPa)+pfakte*fx*fys*fzs          ! Ey
        A(3,kPa)=A(3,kPa)+pfakte*ckap2*fx*fy*fz      ! Ez
        A(4,kPa)=A(4,kPa)-pfakte*ci*cOmEps*fx*fys*fz ! Hx
        A(5,kPa)=A(5,kPa)+pfakte*ci*cOmEps*fxs*fy*fz ! Hy
      end if
! h
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.0).and.(m.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        A(1,kPa)=A(1,kPa)+pfakte*pfakth*ci*cOmMu*gx*gys*gz ! Ex
        A(2,kPa)=A(2,kPa)-pfakte*pfakth*ci*cOmMu*gxs*gy*gz ! Ey
        A(4,kPa)=A(4,kPa)+pfakte*pfakth*gxs*gy*gzs         ! Hx
        A(5,kPa)=A(5,kPa)+pfakte*pfakth*gx*gys*gzs         ! Hy
        A(6,kPa)=A(6,kPa)+pfakte*pfakth*ckap2*gx*gy*gz     ! Hz
      end if
    end do
  end Subroutine Harmonic2D

  Subroutine Rayleigh2D(kEx,kPa,r,gc,iD,iHEk,iyT,isT,maxOrd,maxPar,nP,A)
! Rayleigh expansions of the form f=exp(i*(kx*x+ky*y+kz*z)) with
! kx=kxinc+2*n*Pi/dx, kz=kzinc, ky=+-sqrt(k**2-kx**2-kz**2).
! kxinc and kzinc are the x and z components of the incident plane
! wave, dx is the period of the structure in x direction,
! n is the corresponding order. f is either Ez or Hz.
! this expansion should only be used for cylindrical gratings in the
! xz plane, z=cylinder axis
    Implicit none
    Integer(4) kEx,kPa,nP,mPa,m
    Complex(8) A(10,nParN)
    Complex(8) gc,cz,ckx,cky,ckz,cOmEps,cOmMu,ckap2,ci,cq,cxperiod0
    Real(8) r(3),pfakte,pfakth
    Integer(2) iD,iyT,isT,iHEk,maxOrd,maxPar,lf,minOrd
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    minOrd=min(max(0_2,tExp(kEx)%iE(3)),maxOrd)
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    ci=(0.0d0,1.0d0)
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    if(lxPeriod) then
      cxperiod0=cxperiod
      tExp(kEx)%rE(3)=Dble(cxperiod)
      tExp(kEx)%rE(4)=DImag(cxperiod)
    else
      cxperiod0=DCmplx(tExp(kEx)%rE(3),tExp(kEx)%rE(4))
    end if
    if(lzPer) then
      ckz=Pi/dcFld
    else
      ckz=(2.0d0*Pi*kw0)*fcFld*gc
    end if
    ckap2=kcFld**2-ckz**2
    cOmEps=(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    cOmMu=(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
! scaling
    pfakte=1.0d0/dsqrt(cdabs((2.0d0*Pi*eps0)*fcFld*eDom(lf)*ckap2*kcFld))
    pfakth=1.0d0/cdabs(cZw)
    cq=(1.0d0,0.0d0)
    do m=minOrd,maxOrd
! expansions of degree m
      ckx=cxperiod0+2.0d0*Dble(m)*Pi/xperiod
      cky=cdsqrt(kcFld**2-ckx**2-ckz**2)
      if(iyT.eq.0) then      ! grating, transmitted
        if(DImag(cky).lt.0.0d0) cky=-cky
        cky=-cky
      else if(iyT.eq.1) then ! grating, reflected
        if(DImag(cky).lt.0.0d0) cky=-cky
      else                   ! non-grating, complex
        if(DImag(cky).lt.0.0d0) cky=-cky
        if(iyT.lt.0) cky=-cky
      end if
      if(isT.ge.0) cq=sqrt(kcFld.div.cky)
      cz=pfakte*cdexp(ci*(ckx*r(1)+cky*r(2)+ckz*r(3)))*cq
      if((iyT.ne.0).and.(iyT.ne.1)) then
        if(iabs(iyT).gt.2) then
          if(r(2).gt.tExp(kEx)%rE(2)) cz=(0.0d0,0.0d0)
        else
          if(r(2).lt.tExp(kEx)%rE(2)) cz=(0.0d0,0.0d0)
        end if
      end if
! e +
      if(kPa.ge.mPa) Exit
      if(iHEk.ne.1) then
        kPa=kPa+1
        nP=nP+1
        A(1,kPa)=A(1,kPa)-ckz*ckx*cz    ! Ex
        A(2,kPa)=A(2,kPa)-ckz*cky*cz    ! Ey
        A(3,kPa)=A(3,kPa)+ckap2*cz      ! Ez
        A(4,kPa)=A(4,kPa)+cOmEps*cky*cz ! Hx
        A(5,kPa)=A(5,kPa)-cOmEps*ckx*cz ! Hy
      end if
! h +
      if(kPa.ge.mPa) Exit
      if(iHEk.ne.0) then
        kPa=kPa+1
        nP=nP+1
        cz=cz*pfakth
        A(1,kPa)=A(1,kPa)-cOmMu*cky*cz ! Ex
        A(2,kPa)=A(2,kPa)+cOmMu*ckx*cz ! Ey
        A(4,kPa)=A(4,kPa)-ckz*ckx*cz   ! Hx
        A(5,kPa)=A(5,kPa)-ckz*cky*cz   ! Hy
        A(6,kPa)=A(6,kPa)+ckap2*cz     ! Hz
      end if
      if(m.eq.0) Cycle
      ckx=cxperiod0-2.0d0*Dble(m)*Pi/xperiod
      cky=cdsqrt(kcFld**2-ckx**2-ckz**2)
      if(iyT.eq.0) then      ! grating, transmitted
        if(DImag(cky).lt.0.0d0) cky=-cky
        cky=-cky
      else if(iyT.eq.1) then ! grating, reflected
        if(DImag(cky).lt.0.0d0) cky=-cky
      else                   ! non-grating, complex
        if(DImag(cky).lt.0.0d0) cky=-cky
        if(iyT.lt.0) cky=-cky
      end if
      if(isT.ge.0) cq=sqrt(kcFld.div.cky)
      cz=pfakte*cdexp(ci*(ckx*r(1)+cky*r(2)+ckz*r(3)))*cq
      if((iyT.ne.0).and.(iyT.ne.1)) then
        if(iabs(iyT).gt.2) then
          if(r(2).gt.tExp(kEx)%rE(2)) cz=(0.0d0,0.0d0)
        else
          if(r(2).lt.tExp(kEx)%rE(2)) cz=(0.0d0,0.0d0)
        end if
      end if
! e -
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.1).and.(m.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        A(1,kPa)=A(1,kPa)-ckz*ckx*cz    ! Ex
        A(2,kPa)=A(2,kPa)-ckz*cky*cz    ! Ey
        A(3,kPa)=A(3,kPa)+ckap2*cz      ! Ez
        A(4,kPa)=A(4,kPa)+cOmEps*cky*cz ! Hx
        A(5,kPa)=A(5,kPa)-cOmEps*ckx*cz ! Hy
      end if
! h -
      if(kPa.ge.mPa) Exit
      if((iHEk.ne.0).and.(m.ne.0)) then
        kPa=kPa+1
        nP=nP+1
        cz=cz*pfakth
        A(1,kPa)=A(1,kPa)-cOmMu*cky*cz ! Ex
        A(2,kPa)=A(2,kPa)+cOmMu*ckx*cz ! Ey
        A(4,kPa)=A(4,kPa)-ckz*ckx*cz   ! Hx
        A(5,kPa)=A(5,kPa)-ckz*cky*cz   ! Hy
        A(6,kPa)=A(6,kPa)+ckap2*cz     ! Hz
      end if
    end do
  end Subroutine Rayleigh2D

  Subroutine Rayleigh2DK(kEx,kP,ckx,cky)
! return K, Kx, and Ky values of the parameter kP of a 2D Rayleigh expansion
    Implicit none
    Integer(4) kEx,kP,nP,m
    Complex(8) ckx,cky,ckz,cOmEps,cOmMu,ckap2
    Integer(2) iD,iyT,iHEk,maxOrd,maxPar,lf,minOrd
    nP=0
    kcFld=dCmplx(0.0d0,pBig)
    ckx=dCmplx(0.0d0,pBig)
    cky=dCmplx(0.0d0,pBig)
    maxPar=tExp(kEx)%nPar
    if((kEx.lt.1).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    iD=tExp(kEx)%iDom
    iHEk=igetiHE2(iHEGlobal,kEx)
    iyT=tExp(kEx)%iE(1)
    maxOrd=max(0_2,tExp(kEx)%iE(2))
    minOrd=min(max(0_2,tExp(kEx)%iE(3)),maxOrd)
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    if(lzPer) then
      ckz=Pi/dcFld
    else
      ckz=(2.0d0*Pi*kw0)*fcFld*gcFld
    end if
    ckap2=kcFld**2-ckz**2
    cOmEps=(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    cOmMu=(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    do m=minOrd,maxOrd
! expansions of degree m
      ckx=cxperiod+2.0d0*Dble(m)*Pi/xperiod
      cky=cdsqrt(kcFld**2-ckx**2-ckz**2)
      if(iyT.eq.0) then      ! grating, transmitted
        if(DImag(cky).lt.0.0d0) cky=-cky
        cky=-cky
      else if(iyT.eq.1) then ! grating, reflected
        if(DImag(cky).lt.0.0d0) cky=-cky
      else                   ! non-grating, complex
        if(DImag(cky).lt.0.0d0) cky=-cky
        if(iyT.lt.0) cky=-cky
      end if
! e +
      if(iHEk.ne.1) then
        nP=nP+1
        if(nP.eq.kP) return
      end if
! h +
      if(iHEk.ne.0) then
        nP=nP+1
        if(nP.eq.kP) return
      end if
      if(m.eq.0) Cycle
      ckx=cxperiod-2.0d0*Dble(m)*Pi/xperiod
      cky=cdsqrt(kcFld**2-ckx**2-ckz**2)
      if(DImag(cky).lt.0.0d0) cky=-cky
      if(iyT.lt.1) cky=-cky
! e -
      if((iHEk.ne.1).and.(m.ne.0)) then
        nP=nP+1
        if(nP.eq.kP) return
      end if
! h -
      if((iHEk.ne.0).and.(m.ne.0)) then
        nP=nP+1
        if(nP.eq.kP) return
      end if
    end do
  end Subroutine Rayleigh2DK

  Subroutine Rayleigh3D(kEx,kPa,r,iD,iHEk,izT,isT,maxOrd,maxDeg,maxPar,nP,A)
! Rayleigh expansions of the form f=exp(i*(kx*x+ky'*y'+kz*z)) with
! kx=kxperiod+2*m*Pi/dx, ky'=kyperiod+2*n*Pi/dy', kz=+-sqrt(k**2-kx**2-ky'**2).
! kxperiod and kyperiod are the x and y' components of the incident plane
! wave, dx and dy' are the periods of the structure in x and y' directions,
! m and n are the corresponding orders. f is either Ez or Hz.
! y' can also be a vector in the xy plane that is not perpendicular to the x axis.
! This expansion should only be used for gratings in the xz plane.
    Implicit none
    Integer(4) kEx,kPa,nP,mPa,n,m
    Complex(8) A(10,nParN)
    Complex(8) cz,ckx,cky,ckz,cOmEps,cOmMu,ckap2,ci,cq,ck1,ck2,ckys
    Real(8) r(3),pfakte,pfakth,ax,ay,tga,dkx,dky,d1,d2,d
    Integer(2) iD,izT,isT,iHEk,maxOrd,maxDeg,maxPar,lf
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    mPa=kPa+maxPar
    lf=Max(1_2,Min(Int2(nDom),iD))
    ci=(0.0d0,1.0d0)
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    cOmEps=(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    cOmMu=(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    pfakth=1.0d0/cdabs(cZw)
    if(lxPeriod.and.lyPeriod) then
      tga=yPeriodVector(1).div.yPeriodVector(2)
      d=r2Vec_Length(yPeriodVector(1:2))
      ay=2.0d0*Pi/d
      d1=yPeriodVector(1)/d
      d2=yPeriodVector(2)/d
    else
      tga=0.0d0
      ay=0.0d0
      d=1.0d0
      d1=0.0d0
      d2=1.0d0
    end if
    ax=2.0d0*Pi/xPeriod
    cq=(1.0d0,0.0d0)
    do n=-maxDeg,maxDeg
! expansions of degree m, n>=0
      ckys=CyPeriod+Dble(n)*ay
      ck1=ckys*d1
      ck2=ckys*d2
      do m=-maxOrd,maxOrd
        ckx=CxPeriod+Dble(m)*ax
        cky=(ck1-ckx)*tga+ck2
        ckap2=ckx**2+cky**2
        ckz=cdsqrt(kcFld**2-ckap2)
        pfakte=dsqrt(cdabs(ckap2))
        if(izT.eq.0) then      ! grating, transmitted -> wave should decay in -z direction
          if(DImag(ckz).lt.0.0d0) ckz=-ckz
          ckz=-ckz
        else if(izT.eq.1) then ! grating, reflected
          if(DImag(ckz).lt.0.0d0) ckz=-ckz
        else                   ! non-grating, complex
          if(izT.lt.0) ckz=-ckz
          if((m.ne.0).and.(DImag(ckz).lt.0.0d0)) ckz=-ckz
        end if
        if(pfakte.lt.1.0d-15*abs(ckz)) then ! ckap2 almost 0 -> could cause overflow -> rotate slightly
          dkx=abs(ckx)
          dky=abs(cky)
          if((dkx.lt.pSmall).and.(dky.lt.pSmall)) then
            ckx=(0.0d0,0.0d0)
            cky=(0.0d0,0.0d0)
            if(dky.lt.dkx) then
              ckx=Dcmplx(1.0d-15*abs(ckz),0.0d0)
            else
              cky=Dcmplx(1.0d-15*abs(ckz),0.0d0)
            end if
          else
            ckx=(1.0d-15*abs(ckz).div.pfakte)*ckx
            cky=(1.0d-15*abs(ckz).div.pfakte)*cky
          end if
          ckap2=ckx**2+cky**2
          ckz=cdsqrt(kcFld**2-ckap2)
          pfakte=dsqrt(cdabs(ckap2))
          if(izT.eq.0) then      ! grating, transmitted -> wave should decay in -z direction
            if(DImag(ckz).lt.0.0d0) ckz=-ckz
            ckz=-ckz
          else if(izT.eq.1) then ! grating, reflected
            if(DImag(ckz).lt.0.0d0) ckz=-ckz
          else                   ! non-grating, complex
            if(izT.lt.0) ckz=-ckz
            if((m.ne.0).and.(DImag(ckz).lt.0.0d0)) ckz=-ckz
          end if
        end if
        pfakte=1.0d0/(pfakte*dsqrt(cdabs((2.0d0*Pi*eps0)*fcFld*eDom(lf)*kcFld)))
        if(isT.ge.0) cq=sqrt(kcFld.div.ckz)
        cz=pfakte*cdexp(ci*(ckx*r(1)+cky*r(2)+ckz*r(3)))*cq
        if((izT.ne.0).and.(izT.ne.1)) then
          if(iabs(izT).gt.2) then
            if(r(3).gt.tExp(kEx)%rE(2)) cz=(0.0d0,0.0d0)
          else
            if(r(3).lt.tExp(kEx)%rE(2)) cz=(0.0d0,0.0d0)
          end if
        end if
! e
        if(kPa.ge.mPa) Exit
        if(iHEk.ne.1) then
          kPa=kPa+1
          nP=nP+1
          A(1,kPa)=A(1,kPa)-ckz*ckx*cz    ! Ex
          A(2,kPa)=A(2,kPa)-ckz*cky*cz    ! Ey
          A(3,kPa)=A(3,kPa)+ckap2*cz      ! Ez
          A(4,kPa)=A(4,kPa)+cOmEps*cky*cz ! Hx
          A(5,kPa)=A(5,kPa)-cOmEps*ckx*cz ! Hy
        end if
! h
        if(kPa.ge.mPa) Exit
        if(iHEk.ne.0) then
          kPa=kPa+1
          nP=nP+1
          cz=cz*pfakth
          A(1,kPa)=A(1,kPa)-cOmMu*cky*cz ! Ex
          A(2,kPa)=A(2,kPa)+cOmMu*ckx*cz ! Ey
          A(4,kPa)=A(4,kPa)-ckz*ckx*cz   ! Hx
          A(5,kPa)=A(5,kPa)-ckz*cky*cz   ! Hy
          A(6,kPa)=A(6,kPa)+ckap2*cz     ! Hz
        end if
      end do
    end do
  end Subroutine Rayleigh3D

  Subroutine Rayleigh3DK(kEx,kP,ckx,cky,ckz)
! return K, Kx, Ky, and Kz values of the parameter kP of a 3D Rayleigh expansion
    Implicit none
    Integer(4) kEx,kP,nP,n,m
    Complex(8) ckx,cky,ckz,cOmEps,cOmMu,ckap2,ck1,ck2,ckys
    Real(8) pfakte,ax,ay,tga,dkx,dky,d1,d2,d
    Integer(2) iD,izT,iHEk,maxOrd,maxDeg,maxPar,lf
    nP=0
    kcFld=dCmplx(0.0d0,pBig)
    ckx=dCmplx(0.0d0,pBig)
    cky=dCmplx(0.0d0,pBig)
    maxPar=tExp(kEx)%nPar
    if((kEx.lt.1).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    iD=tExp(kEx)%iDom
    iHEk=igetiHE2(iHEGlobal,kEx)
    izT=tExp(kEx)%iE(1)
    maxOrd=max(0_2,tExp(kEx)%iE(2))
    maxDeg=max(0_2,tExp(kEx)%iE(3))
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cOmEps=(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    cOmMu=(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    if(lxPeriod.and.lyPeriod) then
      tga=yPeriodVector(1).div.yPeriodVector(2)
      d=r2Vec_Length(yPeriodVector(1:2))
      ay=2.0d0*Pi/d
      d1=yPeriodVector(1)/d
      d2=yPeriodVector(2)/d
    else
      tga=0.0d0
      ay=0.0d0
      d=1.0d0
      d1=0.0d0
      d2=1.0d0
    end if
    ax=2.0d0*Pi/xPeriod
    do n=-maxDeg,maxDeg
! expansions of degree m, n>=0
      ckys=CyPeriod+Dble(n)*ay
      ck1=ckys*d1
      ck2=ckys*d2
      do m=-maxOrd,maxOrd
        ckx=CxPeriod+Dble(m)*ax
        cky=(ck1-ckx)*tga+ck2
        ckap2=ckx**2+cky**2
        ckz=cdsqrt(kcFld**2-ckap2)
        if(izT.eq.0) then      ! grating, transmitted
          if(DImag(ckz).lt.0.0d0) ckz=-ckz
          ckz=-ckz
        else if(izT.eq.1) then ! grating, reflected
          if(DImag(ckz).lt.0.0d0) ckz=-ckz
        else                   ! non-grating, complex
          if(izT.lt.0) ckz=-ckz
          if((m.ne.0).and.(DImag(ckz).lt.0.0d0)) ckz=-ckz
        end if
        pfakte=dsqrt(cdabs(ckap2))
        if(pfakte.lt.1.0d-15*abs(ckz)) then ! ckap2 almost 0 -> could cause overflow
          dkx=abs(ckx)
          dky=abs(cky)
          if((dkx.lt.pSmall).and.(dky.lt.pSmall)) then
            ckx=(0.0d0,0.0d0)
            cky=(0.0d0,0.0d0)
            if(dky.lt.dkx) then
              ckx=Dcmplx(1.0d-15*abs(ckz),0.0d0)
            else
              cky=Dcmplx(1.0d-15*abs(ckz),0.0d0)
            end if
          else
            ckx=(1.0d-15*abs(ckz).div.pfakte)*ckx
            cky=(1.0d-15*abs(ckz).div.pfakte)*cky
          end if
        end if
! e
        if(iHEk.ne.1) then
          nP=nP+1
          if(nP.eq.kP) return
        end if
! h
        if(iHEk.ne.0) then
          nP=nP+1
          if(nP.eq.kP) return
        end if
      end do
    end do
  end Subroutine Rayleigh3DK

  Subroutine Rayleigh2DAng(kEx,kP,AngleY)
! return the Angle (in degrees) of the parameter kP of a 2D Rayleigh expansion
    Implicit none
    Integer(4) kEx,kP
    Complex(8) ckx,cky
    Real(8) rk,rkx,rky,aky,AngleY
    call Rayleigh2DK(kEx,kP,ckx,cky)
    rk=Dble(kcFld)
    rkx=Dble(ckx)
    rky=Dble(cky)
    aky=Dabs(DImag(cky))
    if(aky.lt.1.0d-14*dabs(rky)) then
      AngleY=dabs(rky.div.rk)
      AngleY=max(0.0d0,min(1.0d0,AngleY))
      AngleY=180.0d0*dacos(AngleY)/Pi
      if(rkx.lt.0.0d0) AngleY=-AngleY
    else
      AngleY=-1000.0d0
    end if
  end Subroutine Rayleigh2DAng

  Subroutine Rayleigh3DAng(kEx,kP,AngleXY,AngleZ)
! return the Angles (in degrees) of the parameter kP of a 3D Rayleigh expansion
    Implicit none
    Integer(4) kEx,kP
    Complex(8) ckx,cky,ckz
    Real(8) rk,rkx,rky,rkz,akx,aky,akz,AngleXY,AngleZ
    call Rayleigh3DK(kEx,kP,ckx,cky,ckz)
    rk=Dble(kcFld)
    rkx=Dble(ckx)
    rky=Dble(cky)
    rkz=Dble(ckz)
    akx=Dabs(DImag(ckx))
    aky=Dabs(DImag(cky))
    akz=Dabs(DImag(ckz))
    if(akz.lt.1.0d-14*dabs(rkz)) then
      AngleZ=rkz.div.rk
      AngleZ=max(-1.0d0,min(1.0d0,AngleZ))
      AngleZ=180.0d0*dacos(AngleZ)/Pi
    else
      AngleZ=-1000.0d0
    end if
    if((akx.lt.1.0d-14**dabs(rkx)).and.(aky.lt.1.0d-14**dabs(rky))) then
      AngleXY=180.0d0*datan2(rky,rkx)/Pi
    else
      AngleXY=-1000.0d0
    end if
  end Subroutine Rayleigh3DAng

  Subroutine Multipole3DC(kEx,kPa,iType,r,rE1,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,nP,ixS,iyS,izS,ximag,yimag,zimag,Ar)
! compute 3D multipole or Bessel expansion with complex origin
! iType=  1: Hankel 1,  2: Bessel, 3: Hankel 2 (outer loop: Order  n -> phi   dependence)
    Implicit None
    Complex(8), Allocatable :: cBes(:)
    Complex(8), Allocatable :: asi(:),aco(:),P(:)
    Real(8), Allocatable :: pfakt(:)
    Complex(8) Ar(10,nParN)
    Complex(8) crk,cioeps,ciomu,ceer,cethet,cephi,chphi,chthet,cv(3)
    Complex(8) rho,rabs,rinv,thcos,thsin,phicos,phisin
    Real(8) rE1,r(3),rrabs,riabs,pfaktm,pfakte,dsqrtc,ximag,yimag,zimag,a,b,al,ai
    Integer(4) kEx,kPa,mPa,idum,iEven,iOdd,jEven,jOdd,nP
    Integer(2) iType,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,ixS,iyS,izS,lf,n,m
    External dsqrtc
    if(lMMPStat) then
      call Multipole3DStat(kEx,kPa,iType,r,rE1,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,nP,ixS,iyS,izS,Ar)
      return
    end if
    nP=0
    if((kEx.lt.0).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    if(Allocated(pfakt)) DeAllocate(pfakt)
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(aco)) DeAllocate(aco)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(cBes)) DeAllocate(cBes)
    Allocate(cBes(0:maxOrd),asi(0:maxDeg),aco(0:maxDeg),P(2*maxOrd+2),pfakt(maxOrd+1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Multipole'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    mPa=kPa+Int4(maxPar)
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    rrabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
    if(.not.lExpTest) then
      if(abs(iType).eq.2) then !!!! Bessel type = 0 outside circle
        if(rE1.gt.pSmall) then
          rrabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
          if(rrabs.gt.rE1) return 
        end if
      else                     !!!! Hankel type = 0 inside circle
        if(rE1.gt.pSmall) then
          rrabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
          riabs=dsqrtc(ximag**2+yimag**2+zimag**2)
          if(abs(rrabs-riabs).lt.rE1) then
            if(rrabs.lt.0.1d0*rE1) return
            riabs=dsqrtc(ximag*r(1)+yimag*r(2)+zimag*r(3))/rrabs
            if(riabs.lt.rE1) return
          end if 
        end if
      end if
    end if
    rho=(r(1)-(0.0d0,1.0d0)*ximag)**2+(r(2)-(0.0d0,1.0d0)*yimag)**2
    rabs=rho+(r(3)-(0.0d0,1.0d0)*zimag)**2
    rho=sqrt(rho)
    if(Dble(rho).lt.0.0d0) rho=-rho
    rabs=sqrt(rabs)
    if(Dble(rabs).lt.0.0d0) rabs=-rabs
    if((abs(iType).ne.2).and.(abs(tExp(kEx)%rE(5)).gt.pSmall)) then ! cut of complex origin multipole
      al=tExp(kEx)%rE(5)*Pi/180.0d0
      riabs=dsqrt(ximag**2+yimag**2+zimag**2)
      rrabs=dsqrt(r(1)**2+r(2)**2+r(3)**2)
      b=(r(1)*ximag+r(2)*yimag+r(3)*zimag)/(riabs+pSmall)
      a=dsqrt(rrabs**2-b**2)
      ai=datan2(b,riabs-a)
      if(((al.lt.ai).and.(b.le.0.0d0)).or.((al.ge.ai).and.(b.gt.0.0d0))) then
        rho=-rho
        rabs=-rabs
      end if
    end if
    if(abs(rabs).gt.1.0d-30) then
      rinv=1.0d0/rabs
      thcos=(r(3)-(0.0d0,1.0d0)*zimag)*rinv
      thsin=rho*rinv
    else
      rabs=0.99999999d-30
      rinv=1.0d30
      thcos=0.0d0
      thsin=1.0d0
    endif
    if(abs(rho).gt.1.0d-30) then
      phicos=(r(1)-(0.0d0,1.0d0)*ximag)/rho
      phisin=(r(2)-(0.0d0,1.0d0)*yimag)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-30
    end if
    cioeps=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    ciomu=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    crk=rabs*kcFld
    if(abs(iType).eq.2) then
      call bessel(Int4(maxOrd),crk,cBes,1_4) ! Bessel
    else if(abs(iType).eq.1) then
      call bessel(Int4(maxOrd),crk,cBes,3_4) ! Hankel 1
    else if(abs(iType).eq.3) then
      call bessel(Int4(maxOrd),crk,cBes,4_4) ! Hankel 2
    else
      call bessel(Int4(maxOrd),crk,cBes,2_4) ! Neumann
    endif
    do n=minOrd,maxOrd
      if(kPa.ge.mPa) Exit
      if(minDeg.lt.1) then
! degree m=0
        call legenmC(thcos,thsin,P(1),Int4(n),0_4)
        call legenmC(thcos,thsin,P(maxOrd+2),Int4(n),1_4)
        call vkfnor(pfakt(1),Int4(n),0_4)
        jEven=modulo(n,2)+1
        jOdd=3-jEven
        chphi=thsin*P(n+1+maxOrd)*cBes(n)
        ceer=dble(n)*dble(n+1)*rinv*P(n+1)*cBes(n)
        cethet=thsin*P(n+1+maxOrd)*(kcFld*cBes(n-1)-dble(n)*rinv*cBes(n))
! scaling
        pfakte=pfakt(1+n)/abs(cZw)
        pfaktm=pfakt(1+n)
! E c 0n
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.2).and.(ixS.ne.2).and.(iyS.ne.2).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=pfaktm*ceer         ! Er
          cv(2)=pfaktm*cethet       ! Etheta
          cv(3)=(0.0d0,0.0d0)       ! Ephi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(1:3,kPa))
          cv(1)=(0.0d0,0.0d0)       ! Hr
          cv(2)=(0.0d0,0.0d0)       ! Htheta
          cv(3)=pfaktm*cioeps*chphi ! Hphi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(4:6,kPa))
        end if
! H c 0n
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.2).and.(ixS.ne.1).and.(iyS.ne.1).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=(0.0d0,0.0d0)       ! Er
          cv(2)=(0.0d0,0.0d0)       ! Etheta
          cv(3)=-pfakte*ciomu*chphi ! Ephi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(1:3,kPa))
          cv(1)=pfakte*ceer         ! Hr
          cv(2)=pfakte*cethet       ! Htheta
          cv(3)=(0.0d0,0.0d0)       ! Hphi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(4:6,kPa))
        end if
      end if
! degrees m>0
      call sicomC(Int4(maxDeg),phisin,phicos,asi,aco)
      do m=max(1_2,minDeg),maxDeg
        if(kPa.ge.mPa) Exit
        iEven=modulo(m,2)+1
        iOdd=3-iEven
        if(n.lt.m) Exit
        call legenmC(thcos,thsin,P(m),Int4(n),Int4(m))
        call vkfnor(pfakt(m),Int4(n),Int4(m))
        jEven=modulo(n+m,2)+1
        jOdd=3-jEven
        ceer=dble(n)*dble(n+1)*thsin*rinv*P(n)*cBes(n)
        chthet=dble(m)*P(n)*cBes(n)
        chphi=dble(n)*thcos*P(n)
        if(n.gt.m) chphi=chphi-dble(n+m)*P(n-1)
        cethet=kcFld*cBes(n-1)-dble(n)*rinv*cBes(n)
        cephi=-dble(m)*P(n)*cethet
        cethet=cethet*chphi
        chphi=cBes(n)*chphi
! scaling
        pfakte=pfakt(n)/abs(cZw)
        pfaktm=pfakt(n)
! E c mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.2).and.(ixS.ne.iOdd).and.(iyS.ne.2).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+pfaktm*aco(m)*ceer           ! Er
          cv(2)=+pfaktm*aco(m)*cethet         ! Etheta
          cv(3)=+pfaktm*asi(m)*cephi          ! Ephi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(1:3,kPa))
          cv(1)=(0.0d0,0.0d0)                 ! Hr
          cv(2)=+pfaktm*asi(m)*cioeps*chthet  ! Htheta
          cv(3)=+pfaktm*aco(m)*cioeps*chphi   ! Hphi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(4:6,kPa))
        end if
! E s mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.1).and.(ixS.ne.iEven).and.(iyS.ne.1).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+pfaktm*asi(m)*ceer           ! Er
          cv(2)=+pfaktm*asi(m)*cethet         ! Etheta
          cv(3)=-pfaktm*aco(m)*cephi          ! Ephi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(1:3,kPa))
          cv(1)=(0.0d0,0.0d0)                 ! Hr
          cv(2)=-pfaktm*aco(m)*cioeps*chthet  ! Htheta
          cv(3)=+pfaktm*asi(m)*cioeps*chphi   ! Hphi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(4:6,kPa))
        end if
! H c mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.2).and.(ixS.ne.iEven).and.(iyS.ne.1).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=(0.0d0,0.0d0)                 ! Er
          cv(2)=-pfakte*asi(m)*ciomu*chthet   ! Etheta
          cv(3)=-pfakte*aco(m)*ciomu*chphi    ! Ephi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(1:3,kPa))
          cv(1)=+pfakte*aco(m)*ceer           ! Hr
          cv(2)=+pfakte*aco(m)*cethet         ! Htheta
          cv(3)=+pfakte*asi(m)*cephi          ! Hphi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(4:6,kPa))
        end if
! H s mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.1).and.(ixS.ne.iOdd).and.(iyS.ne.2).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=(0.0d0,0.0d0)                 ! Er
          cv(2)=+pfakte*aco(m)*ciomu*chthet   ! Etheta
          cv(3)=-pfakte*asi(m)*ciomu*chphi    ! Ephi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(1:3,kPa))
          cv(1)=+pfakte*asi(m)*ceer           ! Hr
          cv(2)=+pfakte*asi(m)*cethet         ! Htheta
          cv(3)=-pfakte*aco(m)*cephi          ! Hphi
          call cvSphCarC(thsin,thcos,phisin,phicos,cv,Ar(4:6,kPa))
        end if
      end do
    end do
    if(Allocated(pfakt)) DeAllocate(pfakt)
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(aco)) DeAllocate(aco)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(cBes)) DeAllocate(cBes)
  end Subroutine Multipole3DC

  Subroutine Multipole3D(kEx,kPa,iType,r,rE1,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,nP,ixS,iyS,izS,A)
! compute 3D multipole or Bessel expansion with real origin
! iType=  1: Hankel,  2: Bessel (outer loop: Order  n -> phi   dependence)
    Implicit None
    Complex(8), Allocatable :: cBes(:)
    Real(8), Allocatable :: asi(:),aco(:),P(:)
    Real(8), Allocatable :: pfakt(:)
    Complex(8) A(10,nParN)
    Complex(8) crk,cioeps,ciomu,ceer,cethet,cephi,chphi,chthet,cv(3)
    Real(8) rho,rabs,rinv,thcos,thsin,phicos,phisin
    Real(8) rE1,r(3),rrabs,pfaktm,pfakte,dsqrtc
    Integer(4) kEx,kPa,mPa,idum,iEven,iOdd,jEven,jOdd,nP
    Integer(2) iType,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,ixS,iyS,izS,lf,n,m
    External dsqrtc
    if(lMMPStat) then
      call Multipole3DStat(kEx,kPa,iType,r,rE1,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,nP,ixS,iyS,izS,A)
      return
    end if
    nP=0
    if((kEx.lt.0).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    if(Allocated(pfakt)) DeAllocate(pfakt)
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(aco)) DeAllocate(aco)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(cBes)) DeAllocate(cBes)
    Allocate(cBes(0:maxOrd),asi(0:maxDeg),aco(0:maxDeg),P(2*maxOrd+2),pfakt(maxOrd+1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Multipole'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    mPa=kPa+Int4(maxPar)
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    rrabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
    if(.not.lExpTest) then
      if((abs(iType).eq.2).and.(rrabs.gt.rE1).and.(rE1.gt.pSmall)) return !!!! Bessel type = 0 outside sphere
      if((abs(iType).eq.1).and.(rrabs.lt.rE1).and.(rE1.gt.pSmall)) return !!!! Hankel type = 0 inside sphere
    end if
    rho=r(1)**2+r(2)**2
    rabs=rho+r(3)**2
    rho=sqrt(rho)
    rabs=sqrt(rabs)
    if(abs(rabs).gt.1.0d-30) then
      rinv=1.0d0/rabs
      thcos=r(3)*rinv
      thsin=rho*rinv
    else
      rabs=0.99999999d-30
      rinv=1.0d30
      thcos=0.0d0
      thsin=1.0d0
    endif
    if(abs(rho).gt.1.0d-30) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-30
    end if
    cioeps=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    ciomu=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    crk=rabs*kcFld
    if(abs(iType).eq.2) then
      call bessel(Int4(maxOrd),crk,cBes,1_4) ! Bessel
    else if(abs(iType).eq.1) then
      call bessel(Int4(maxOrd),crk,cBes,3_4) ! Hankel 1
    else if(abs(iType).eq.3) then
      call bessel(Int4(maxOrd),crk,cBes,4_4) ! Hankel 2
    else
      call bessel(Int4(maxOrd),crk,cBes,2_4) ! Neumann
    endif
    do n=minOrd,maxOrd
      if(kPa.ge.mPa) Exit
      if(minDeg.lt.1) then
! degree m=0
        call legenm(thcos,thsin,P(1),Int4(n),0_4)
        call legenm(thcos,thsin,P(maxOrd+2),Int4(n),1_4)
        call vkfnor(pfakt(1),Int4(n),0_4)
        jEven=modulo(n,2)+1
        jOdd=3-jEven
        chphi=thsin*P(n+1+maxOrd)*cBes(n)
        ceer=dble(n)*dble(n+1)*rinv*P(n+1)*cBes(n)
        cethet=thsin*P(n+1+maxOrd)*(kcFld*cBes(n-1)-dble(n)*rinv*cBes(n))
! scaling
        pfakte=pfakt(1+n)/abs(cZw)
        pfaktm=pfakt(1+n)
! E c 0n
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.2).and.(ixS.ne.2).and.(iyS.ne.2).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=pfaktm*ceer         ! Er
          cv(2)=pfaktm*cethet       ! Etheta
          cv(3)=(0.0d0,0.0d0)       ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          cv(1)=(0.0d0,0.0d0)       ! Hr
          cv(2)=(0.0d0,0.0d0)       ! Htheta
          cv(3)=pfaktm*cioeps*chphi ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
        end if
! H c 0n
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.2).and.(ixS.ne.1).and.(iyS.ne.1).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=(0.0d0,0.0d0)       ! Er
          cv(2)=(0.0d0,0.0d0)       ! Etheta
          cv(3)=-pfakte*ciomu*chphi ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          cv(1)=pfakte*ceer         ! Hr
          cv(2)=pfakte*cethet       ! Htheta
          cv(3)=(0.0d0,0.0d0)       ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
        end if
      end if
! degrees m>0
      call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      do m=max(1_2,minDeg),maxDeg
        if(kPa.ge.mPa) Exit
        iEven=modulo(m,2)+1
        iOdd=3-iEven
        if(n.lt.m) Exit
        call legenm(thcos,thsin,P(m),Int4(n),Int4(m))
        call vkfnor(pfakt(m),Int4(n),Int4(m))
        jEven=modulo(n+m,2)+1
        jOdd=3-jEven
        ceer=dble(n)*dble(n+1)*thsin*rinv*P(n)*cBes(n)
        chthet=dble(m)*P(n)*cBes(n)
        chphi=dble(n)*thcos*P(n)
        if(n.gt.m) chphi=chphi-dble(n+m)*P(n-1)
        cethet=kcFld*cBes(n-1)-dble(n)*rinv*cBes(n)
        cephi=-dble(m)*P(n)*cethet
        cethet=cethet*chphi
        chphi=cBes(n)*chphi
! scaling
        pfakte=pfakt(n)/abs(cZw)
        pfaktm=pfakt(n)
! E c mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.2).and.(ixS.ne.iOdd).and.(iyS.ne.2).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+pfaktm*aco(m)*ceer           ! Er
          cv(2)=+pfaktm*aco(m)*cethet         ! Etheta
          cv(3)=+pfaktm*asi(m)*cephi          ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          cv(1)=(0.0d0,0.0d0)                 ! Hr
          cv(2)=+pfaktm*asi(m)*cioeps*chthet  ! Htheta
          cv(3)=+pfaktm*aco(m)*cioeps*chphi   ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
        end if
! E s mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.1).and.(ixS.ne.iEven).and.(iyS.ne.1).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+pfaktm*asi(m)*ceer           ! Er
          cv(2)=+pfaktm*asi(m)*cethet         ! Etheta
          cv(3)=-pfaktm*aco(m)*cephi          ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          cv(1)=(0.0d0,0.0d0)                 ! Hr
          cv(2)=-pfaktm*aco(m)*cioeps*chthet  ! Htheta
          cv(3)=+pfaktm*asi(m)*cioeps*chphi   ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
        end if
! H c mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.2).and.(ixS.ne.iEven).and.(iyS.ne.1).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=(0.0d0,0.0d0)                 ! Er
          cv(2)=-pfakte*asi(m)*ciomu*chthet   ! Etheta
          cv(3)=-pfakte*aco(m)*ciomu*chphi    ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          cv(1)=+pfakte*aco(m)*ceer           ! Hr
          cv(2)=+pfakte*aco(m)*cethet         ! Htheta
          cv(3)=+pfakte*asi(m)*cephi          ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
        end if
! H s mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.1).and.(ixS.ne.iOdd).and.(iyS.ne.2).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=(0.0d0,0.0d0)                 ! Er
          cv(2)=+pfakte*aco(m)*ciomu*chthet   ! Etheta
          cv(3)=-pfakte*asi(m)*ciomu*chphi    ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          cv(1)=+pfakte*asi(m)*ceer           ! Hr
          cv(2)=+pfakte*asi(m)*cethet         ! Htheta
          cv(3)=-pfakte*aco(m)*cephi          ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
        end if
      end do
    end do
    if(Allocated(pfakt)) DeAllocate(pfakt)
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(aco)) DeAllocate(aco)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(cBes)) DeAllocate(cBes)
  end Subroutine Multipole3D

  Subroutine Multipole3D0(iType,r,rE1,iD,iHEk,minOrd,maxOrd,nStepOrd,maxPar,ixS,iyS,nP,AExp0)
! auxiliary for line multipoles
! compute 3D multipole or Bessel expansion, degree m=0
! iType=  1: standard    Hankel,  2: standard    Bessel
    Implicit None
    Complex(8), Allocatable :: cBes(:)
    Real(8), Allocatable :: P(:),Pr(:),pfakt(:)
    Integer(4) mPa,idum,nP,kPar0
    Integer(2) iType,iD,iHEk,minOrd,maxOrd,nStepOrd,maxPar,ixS,iyS,lf,n,iOdd,iEven
    Complex(8) AExp0(10,maxPar),crk,cioeps,ciomu,ceer,cethet,chphi,cv(3),cvr(3)
    Real(8) rE1,rho,rabs,rinv,thcos,thsin,phicos,phisin,r(3),pfaktm,pfakte,dsqrtc
    Real(8) xr,zr,rhor,thrsin,thrcos,phircos,phirsin,dthsin,dthcos
    External dsqrtc
    if(lMMPStat) then
      call Multipole3D0Stat(iType,r,rE1,iD,iHEk,minOrd,maxOrd,nStepOrd,maxPar,ixS,iyS,nP,AExp0)
      return
    end if
    nP=0
    if((maxPar.lt.1).or.(maxOrd.lt.minOrd)) return
    if(Allocated(pfakt)) DeAllocate(pfakt)
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(Pr)) DeAllocate(Pr)
    if(Allocated(cBes)) DeAllocate(cBes)
    Allocate(cBes(0:maxOrd),P(2*maxOrd+2),Pr(2*maxOrd+2),pfakt(maxOrd+1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Multipole, degree 0'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    kPar0=0
    mPa=kPar0+Int4(maxPar)
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cZw=Wimpedance(lf)
    rho=dsqrtc(r(1)**2+r(2)**2)
    rabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
    if(.not.lExpTest) then
      if((abs(iType).eq.2).and.(rabs.gt.rE1).and.(rE1.gt.pSmall)) return !!!! Bessel type = 0 outside sphere
      if((abs(iType).eq.1).and.(rabs.lt.rE1).and.(rE1.gt.pSmall)) return !!!! Hankel type = 0 inside sphere
    end if
    if(rabs.gt.1.0d-30) then
      rinv=1.0d0/rabs
      thcos=r(3)*rinv
      thsin=rho*rinv
    else
      rabs=0.99999999d-30
      rinv=1.0d30
      thcos=0.0d0
      thsin=1.0d0
    endif
    if(rho.gt.1.0d-150) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
    end if
    cioeps=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    ciomu=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    crk=rabs*kcFld
    if(abs(iType).eq.2) then
      call bessel(Int4(maxOrd),crk,cBes,1_4) ! Bessel
    else if(abs(iType).eq.1) then
      call bessel(Int4(maxOrd),crk,cBes,3_4) ! Hankel 1
    else if(abs(iType).eq.3) then
      call bessel(Int4(maxOrd),crk,cBes,4_4) ! Hankel 2
    else
      call bessel(Int4(maxOrd),crk,cBes,2_4) ! Neumann
    endif
    call legenm(thcos,thsin,P(1),Int4(maxOrd),0_4)
    call legenm(thcos,thsin,P(maxOrd+2),Int4(maxOrd),1_4)
    call vkfnor(pfakt(1),Int4(maxOrd),0_4)
    do n=minOrd,maxOrd,nStepOrd
      iEven=modulo(n,2)+1
      iOdd=3-iEven
! scaling
      pfakte=pfakt(1+n)/abs(cZw)
      pfaktm=pfakt(1+n)
! main field in spherical coordinates
      chphi=thsin*P(n+1+maxOrd)*cBes(n)
      ceer=dble(n)*dble(n+1)*rinv*P(n+1)*cBes(n)
      cethet=thsin*P(n+1+maxOrd)*(kcFld*cBes(n-1)-dble(n)*rinv*cBes(n))
! E c 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=pfaktm*ceer         ! Er
        cv(2)=pfaktm*cethet       ! Etheta
        cv(3)=(0.0d0,0.0d0)       ! Ephi
        call cvSphCar0(thsin,thcos,phisin,phicos,cv,AExp0(1:3,kPar0))
        cv(1)=(0.0d0,0.0d0)       ! Hr
        cv(2)=(0.0d0,0.0d0)       ! Htheta
        cv(3)=pfaktm*cioeps*chphi ! Hphi
        call cvSphCar0(thsin,thcos,phisin,phicos,cv,AExp0(4:6,kPar0))
      end if
! H c 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=(0.0d0,0.0d0)       ! Er
        cv(2)=(0.0d0,0.0d0)       ! Etheta
        cv(3)=-pfakte*ciomu*chphi ! Ephi
        call cvSphCar0(thsin,thcos,phisin,phicos,cv,AExp0(1:3,kPar0))
        cv(1)=pfakte*ceer         ! Hr
        cv(2)=pfakte*cethet       ! Htheta
        cv(3)=(0.0d0,0.0d0)       ! Hphi
        call cvSphCar0(thsin,thcos,phisin,phicos,cv,AExp0(4:6,kPar0))
      end if
! rotated field
      xr=(0.5d0+dble((n-1)/2))*Pi/dble(n)
      dthsin=dsin(xr)
      dthcos=dcos(xr)
      xr=r(1)*dthcos+r(3)*dthsin
      zr=r(3)*dthcos-r(1)*dthsin
      rhor=dsqrtc(xr**2+r(2)**2)
      if(rabs.gt.1.0d-30) then
        thrcos=zr*rinv
        thrsin=rhor*rinv
      else
        thrcos=0.0d0
        thrsin=1.0d0
      endif
      if(rhor.gt.1.0d-150) then
        phircos=xr/rhor
        phirsin=r(2)/rhor
      else
        phircos=1.0d0
        phirsin=0.0d0
      end if
      call legenm(thrcos,thrsin,Pr(1),Int4(maxOrd),0_4)
      call legenm(thrcos,thrsin,Pr(maxOrd+2),Int4(maxOrd),1_4)
      chphi=thrsin*Pr(n+1+maxOrd)*cBes(n)
      ceer=dble(n)*dble(n+1)*rinv*Pr(n+1)*cBes(n)
      cethet=thrsin*Pr(n+1+maxOrd)*(kcFld*cBes(n-1)-dble(n)*rinv*cBes(n))
! E c 0n rotated -> E s 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=pfaktm*ceer         ! Er
        cv(2)=pfaktm*cethet       ! Etheta
        cv(3)=(0.0d0,0.0d0)       ! Ephi
        call cvSphCar0(thrsin,thrcos,phirsin,phircos,cv,cvr)
        AExp0(1,kPar0)=cvr(1)*dthcos-cvr(3)*dthsin
        AExp0(2,kPar0)=cvr(2)
        AExp0(3,kPar0)=cvr(3)*dthcos+cvr(1)*dthsin
        cv(1)=(0.0d0,0.0d0)       ! Hr
        cv(2)=(0.0d0,0.0d0)       ! Htheta
        cv(3)=pfaktm*cioeps*chphi ! Hphi
        call cvSphCar0(thrsin,thrcos,phirsin,phircos,cv,cvr)
        AExp0(4,kPar0)=cvr(1)*dthcos-cvr(3)*dthsin
        AExp0(5,kPar0)=cvr(2)
        AExp0(6,kPar0)=cvr(3)*dthcos+cvr(1)*dthsin
      end if
! H c 0n rotated -> H s 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=(0.0d0,0.0d0)       ! Er
        cv(2)=(0.0d0,0.0d0)       ! Etheta
        cv(3)=-pfakte*ciomu*chphi ! Ephi
        call cvSphCar0(thrsin,thrcos,phirsin,phircos,cv,cvr)
        AExp0(1,kPar0)=cvr(1)*dthcos-cvr(3)*dthsin
        AExp0(2,kPar0)=cvr(2)
        AExp0(3,kPar0)=cvr(3)*dthcos+cvr(1)*dthsin
        cv(1)=pfakte*ceer         ! Hr
        cv(2)=pfakte*cethet       ! Htheta
        cv(3)=(0.0d0,0.0d0)       ! Hphi
        call cvSphCar0(thrsin,thrcos,phirsin,phircos,cv,cvr)
        AExp0(4,kPar0)=cvr(1)*dthcos-cvr(3)*dthsin
        AExp0(5,kPar0)=cvr(2)
        AExp0(6,kPar0)=cvr(3)*dthcos+cvr(1)*dthsin
      end if
    end do
    if(Allocated(pfakt)) DeAllocate(pfakt)
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(Pr)) DeAllocate(Pr)
    if(Allocated(cBes)) DeAllocate(cBes)
  end Subroutine Multipole3D0

  Subroutine Multipole3DStat(kEx,kPa,iType,r,rE1,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,nP,ixS,iyS,izS,A)
! compute static 3D multipole or Bessel expansion
! iType=  1: Hankel,  2: Bessel (outer loop: Order  n -> phi   dependence)
    Implicit None
    Real(8), Allocatable :: asi(:),aco(:),P(:),rn(:)
    Complex(8) A(10,nParN)
    Complex(8) cv(3)
    Real(8) cfer,cfthet,cfphi,cpot,rE1,rho,rabs,rinv,thcos,thsin,phicos,phisin,r(3),dsqrtc
    Integer(4) kEx,kPa,mPa,idum,iEven,iOdd,jEven,jOdd,nP
    Integer(2) iType,iD,iHEk,minDeg,maxDeg,minOrd,maxOrd,maxPar,isc,ixS,iyS,izS,lf,n,m
    External dsqrtc
    nP=0
    if((kEx.lt.0).or.(kEx.gt.nExp).or.(maxPar.lt.1)) return
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(aco)) DeAllocate(aco)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(rn)) DeAllocate(rn)
    Allocate(rn(0:maxOrd),asi(0:maxDeg),aco(0:maxDeg),P(2*maxOrd+2),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Multipole'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    mPa=kPa+Int4(maxPar)
    lf=Max(1_2,Min(Int2(nDom),iD))
    rho=dsqrtc(r(1)**2+r(2)**2)
    rabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
    if(.not.lExpTest) then
      if((abs(iType).eq.2).and.(rabs.gt.rE1).and.(rE1.gt.pSmall)) return !!!! Bessel type = 0 outside sphere
      if((abs(iType).eq.1).and.(rabs.lt.rE1).and.(rE1.gt.pSmall)) return !!!! Hankel type = 0 inside sphere
    end if
    if(rabs.gt.1.0d-30) then
      rinv=1.0d0/rabs
      thcos=r(3)*rinv
      thsin=rho*rinv
    else
      rabs=0.99999999d-30
      rinv=1.0d30
      thcos=0.0d0
      thsin=1.0d0
    endif
    if(rho.gt.1.0d-30) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-30
    end if
    if(abs(iType).eq.2) then
      rn(0)=1.0d0
      do n=1,maxOrd
        rn(n)=rn(n-1)*rabs
      end do
    else
      rn(0)=rinv
      do n=1,maxOrd
        rn(n)=rn(n-1).mul.rinv
      end do
    endif
    do n=minOrd,maxOrd
      if(kPa.ge.mPa) Exit
      if(minDeg.lt.1) then
! degree m=0
        if(n.gt.0) call legenm(thcos,thsin,P(1),Int4(n),0_4)
        if(n.gt.0) call legenm(thcos,thsin,P(maxOrd+2),Int4(n),1_4)
        jEven=modulo(n,2)+1
        jOdd=3-jEven
        if(n.gt.0) then
          cpot=P(n+1)*rn(n)
          cfphi=0.0d0
          cfthet=-thsin*P(n+1+maxOrd)*rinv*rn(n)
          if(abs(iType).eq.2) then
            cfer=-dble(n)*rinv*cpot
          else
            cfer=dble(n+1)*rinv*cpot
          end if
        else
          cfphi=0.0d0
          cfthet=0.0d0
          if(abs(iType).eq.2) then
            cpot=1.0d0
            cfer=0.0d0
          else
            cpot=rinv
            cfer=rinv*cpot
          end if
        end if
! E c 0n
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.2).and.(ixS.ne.2).and.(iyS.ne.2).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=cfer         ! Er
          cv(2)=cfthet       ! Etheta
          cv(3)=cfphi        ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          A(10,kPa)=A(10,kPa)+cpot
        end if
! H c 0n
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.2).and.(ixS.ne.1).and.(iyS.ne.1).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=cfer         ! Hr
          cv(2)=cfthet       ! Htheta
          cv(3)=cfphi        ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
          A(9,kPa)=A(9,kPa)+cpot
        end if
      end if
! degrees m>0
      call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      do m=max(1_2,minDeg),maxDeg
        if(kPa.ge.mPa) Exit
        iEven=modulo(m,2)+1
        iOdd=3-iEven
        if(n.lt.m) Exit
        call legenm(thcos,thsin,P(m),Int4(n),Int4(m))
        jEven=modulo(n+m,2)+1
        jOdd=3-jEven
        cpot=thsin*P(n)*rn(n)
        if(abs(iType).eq.2) then
          cfer=-dble(n)*rinv*cpot
        else
          cfer=dble(n+1)*rinv*cpot
        end if
        if(n.gt.m) then
          cfthet=rinv*rn(n)*(dble(n+m)*P(n-1)-dble(n)*thcos*P(n))
        else
          cfthet=rinv*rn(n)*(-dble(n)*thcos*P(n))
        end if
        cfphi=dble(m)*rinv*P(n)*rn(n)
! E c mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.2).and.(ixS.ne.iOdd).and.(iyS.ne.2).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+aco(m)*cfer           ! Er
          cv(2)=+aco(m)*cfthet         ! Etheta
          cv(3)=+asi(m)*cfphi          ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          A(10,kPa)=A(10,kPa)+aco(m)*cpot
        end if
! E s mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.1).and.(isc.ne.1).and.(ixS.ne.iEven).and.(iyS.ne.1).and.(izS.ne.jOdd)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+asi(m)*cfer           ! Er
          cv(2)=+asi(m)*cfthet         ! Etheta
          cv(3)=-aco(m)*cfphi          ! Ephi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(1:3,kPa))
          A(10,kPa)=A(10,kPa)+asi(m)*cpot
        end if
! H c mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.2).and.(ixS.ne.iEven).and.(iyS.ne.1).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+aco(m)*cfer           ! Hr
          cv(2)=+aco(m)*cfthet         ! Htheta
          cv(3)=+asi(m)*cfphi          ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
          A(9,kPa)=A(9,kPa)+aco(m)*cpot
        end if
! H s mn
        if(kPa.ge.mPa) Exit
        if((iHEk.ne.0).and.(isc.ne.1).and.(ixS.ne.iOdd).and.(iyS.ne.2).and.(izS.ne.jEven)) then
          kPa=kPa+1
          nP=nP+1
          cv(1)=+asi(m)*cfer           ! Hr
          cv(2)=+asi(m)*cfthet         ! Htheta
          cv(3)=-aco(m)*cfphi          ! Hphi
          call cvSphCar(thsin,thcos,phisin,phicos,cv,A(4:6,kPa))
          A(9,kPa)=A(9,kPa)+asi(m)*cpot
        end if
      end do
    end do
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(aco)) DeAllocate(aco)
    if(Allocated(asi)) DeAllocate(asi)
    if(Allocated(rn)) DeAllocate(rn)
  end Subroutine Multipole3DStat

  Subroutine Multipole3D0Stat(iType,r,rE1,iD,iHEk,minOrd,maxOrd,nStepOrd,maxPar,ixS,iyS,nP,AExp0)
! auxiliary for static line multipoles
! compute 3D multipole or Bessel expansion, degree m=0
! iType=  1: standard    Hankel,  2: standard    Bessel
    Implicit None
    Real(8), Allocatable :: P(:),Pr(:),rn(:)
    Integer(4) mPa,idum,nP,kPar0
    Integer(2) iType,iD,iHEk,minOrd,maxOrd,nStepOrd,maxPar,ixS,iyS,lf,n,l,iOdd,iEven
    Complex(8) AExp0(10,maxPar),cv(3),cvr(3)
    Real(8) cfer,cfthet,cfphi,cpot,rE1,rho,rabs,rinv,thcos,thsin,phicos,phisin,r(3),dsqrtc
    Real(8) xr,zr,rhor,thrsin,thrcos,phircos,phirsin,dthsin,dthcos
    External dsqrtc
    nP=0
    if((maxPar.lt.1).or.(maxOrd.lt.minOrd)) return
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(Pr)) DeAllocate(Pr)
    if(Allocated(rn)) DeAllocate(rn)
    Allocate(rn(0:maxOrd),P(2*maxOrd+2),Pr(2*maxOrd+2),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Multipole, degree 0'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    kPar0=0
    mPa=kPar0+Int4(maxPar)
    lf=Max(1_2,Min(Int2(nDom),iD))
    rho=dsqrtc(r(1)**2+r(2)**2)
    rabs=dsqrtc(r(1)**2+r(2)**2+r(3)**2)
    if(.not.lExpTest) then
      if((abs(iType).eq.2).and.(rabs.gt.rE1).and.(rE1.gt.pSmall)) return !!!! Bessel type = 0 outside sphere
      if((abs(iType).eq.1).and.(rabs.lt.rE1).and.(rE1.gt.pSmall)) return !!!! Hankel type = 0 inside sphere
    end if
    if(rabs.gt.1.0d-30) then
      rinv=1.0d0/rabs
      thcos=r(3)*rinv
      thsin=rho*rinv
    else
      rabs=0.99999999d-30
      rinv=1.0d30
      thcos=0.0d0
      thsin=1.0d0
    endif
    if(rho.gt.1.0d-150) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
    end if
    if(abs(iType).eq.2) then
      rn(0)=1.0d0
      do l=1,maxOrd
        rn(l)=rn(l-1)*rabs
      end do
    else
      rn(0)=rinv
      do l=1,maxOrd
        rn(l)=rn(l-1).mul.rinv
      end do
    endif
    if(maxOrd.gt.0) call legenm(thcos,thsin,P(1),Int4(maxOrd),0_4)
    if(maxOrd.gt.0) call legenm(thcos,thsin,P(maxOrd+2),Int4(maxOrd),1_4)
    do n=minOrd,maxOrd,nStepOrd
      iEven=modulo(n,2)+1
      iOdd=3-iEven
! main field in spherical coordinates
      if(n.gt.0) then
        cpot=P(n+1)*rn(n)
        cfphi=0.0d0
        cfthet=-thsin*P(n+1+maxOrd)*rinv*rn(n)
        if(abs(iType).eq.2) then
          cfer=-dble(n)*rinv*cpot
        else
          cfer=dble(n+1)*rinv*cpot
        end if
      else
        cfphi=0.0d0
        cfthet=0.0d0
        if(abs(iType).eq.2) then
          cpot=1.0d0
          cfer=0.0d0
        else
          cpot=rinv
          cfer=rinv*cpot
        end if
      end if
! E c 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=cfer         ! Er
        cv(2)=cfthet       ! Etheta
        cv(3)=cfphi        ! Ephi
        call cvSphCar0(thsin,thcos,phisin,phicos,cv,AExp0(1:3,kPar0))
        AExp0(10,kPar0)=cpot
      end if
! H c 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=cfer         ! Hr
        cv(2)=cfthet       ! Htheta
        cv(3)=cfphi        ! Hphi
        call cvSphCar0(thsin,thcos,phisin,phicos,cv,AExp0(4:6,kPar0))
        AExp0(9,kPar0)=cpot
      end if
! rotated field
      xr=(0.5d0+dble((n-1)/2))*Pi/dble(n)
      dthsin=dsin(xr)
      dthcos=dcos(xr)
      xr=r(1)*dthcos+r(3)*dthsin
      zr=r(3)*dthcos-r(1)*dthsin
      rhor=dsqrtc(xr**2+r(2)**2)
      if(rabs.gt.1.0d-30) then
        thrcos=zr*rinv
        thrsin=rhor*rinv
      else
        thrcos=0.0d0
        thrsin=1.0d0
      endif
      if(rhor.gt.1.0d-150) then
        phircos=xr/rhor
        phirsin=r(2)/rhor
      else
        phircos=1.0d0
        phirsin=0.0d0
      end if
      if(maxOrd.gt.0) call legenm(thrcos,thrsin,Pr(1),Int4(maxOrd),0_4)
      if(maxOrd.gt.0) call legenm(thrcos,thrsin,Pr(maxOrd+2),Int4(maxOrd),1_4)
      if(n.gt.0) then
        cpot=Pr(n+1)*rn(n)
        cfphi=0.0d0
        cfthet=-thsin*Pr(n+1+maxOrd)*rinv*rn(n)
        if(abs(iType).eq.2) then
          cfer=-dble(n)*rinv*cpot
        else
          cfer=dble(n+1)*rinv*cpot
        end if
      else
        cfphi=0.0d0
        cfthet=0.0d0
        if(abs(iType).eq.2) then
          cpot=1.0d0
          cfer=0.0d0
        else
          cpot=rinv
          cfer=rinv*cpot
        end if
      end if
! E c 0n rotated -> E s 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.1).and.(iyS.ne.1).and.(ixS.ne.iEven)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=cfer         ! Er
        cv(2)=cfthet       ! Etheta
        cv(3)=cfphi        ! Ephi
        call cvSphCar0(thrsin,thrcos,phirsin,phircos,cv,cvr)
        AExp0(1,kPar0)=cvr(1)*dthcos-cvr(3)*dthsin
        AExp0(2,kPar0)=cvr(2)
        AExp0(3,kPar0)=cvr(3)*dthcos+cvr(1)*dthsin
        AExp0(10,kPar0)=cpot
      end if
! H c 0n rotated -> H s 0n
      if(kPar0.ge.mPa) Exit
      if((iHEk.ne.0).and.(iyS.ne.2).and.(ixS.ne.iOdd)) then
        kPar0=kPar0+1
        nP=nP+1
        cv(1)=cfer         ! Hr
        cv(2)=cfthet       ! Htheta
        cv(3)=cfphi        ! Hphi
        call cvSphCar0(thrsin,thrcos,phirsin,phircos,cv,cvr)
        AExp0(4,kPar0)=cvr(1)*dthcos-cvr(3)*dthsin
        AExp0(5,kPar0)=cvr(2)
        AExp0(6,kPar0)=cvr(3)*dthcos+cvr(1)*dthsin
        AExp0(9,kPar0)=cpot
      end if
    end do
    if(Allocated(P)) DeAllocate(P)
    if(Allocated(Pr)) DeAllocate(Pr)
    if(Allocated(rn)) DeAllocate(rn)
  end Subroutine Multipole3D0Stat

  Subroutine cvSphCar(thsin,thcos,phisin,phicos,cv,ca)
! transformation of a complex vector cv from spherical to cartesian coordinates
! the result is added to the complex vector ca
! thsin, thcos, phisin, phicos must already be defined
    Implicit none
    Real(8) thsin,thcos,phisin,phicos
    Complex(8) cv(3),cvrho,ca(3)
    cvrho=thsin*cv(1)+thcos*cv(2)
    ca(3)=ca(3)+thcos*cv(1)-thsin*cv(2)
    ca(1)=ca(1)+phicos*cvrho-phisin*cv(3)
    ca(2)=ca(2)+phisin*cvrho+phicos*cv(3)
  end Subroutine cvSphCar

  Subroutine cvSphCarC(thsin,thcos,phisin,phicos,cv,ca)
! transformation of a complex vector cv from spherical to cartesian coordinates
! the result is added to the complex vector ca
! COMPLEX thsin, thcos, phisin, phicos must already be defined
    Implicit none
    Complex(8) thsin,thcos,phisin,phicos,cv(3),cvrho,ca(3)
    cvrho=thsin*cv(1)+thcos*cv(2)
    ca(3)=ca(3)+thcos*cv(1)-thsin*cv(2)
    ca(1)=ca(1)+phicos*cvrho-phisin*cv(3)
    ca(2)=ca(2)+phisin*cvrho+phicos*cv(3)
  end Subroutine cvSphCarC

  Subroutine cvSphCar0(thsin,thcos,phisin,phicos,cv,ca)
! transformation of a complex vector cv from spherical to cartesian coordinates
! the result is added to the complex vector ca
! thsin, thcos, phisin, phicos must already be defined
    Implicit none
    Real(8) thsin,thcos,phisin,phicos
    Complex(8) cv(3),cvrho,ca(3)
    cvrho=thsin*cv(1)+thcos*cv(2)
    ca(3)=thcos*cv(1)-thsin*cv(2)
    ca(1)=phicos*cvrho-phisin*cv(3)
    ca(2)=phisin*cvrho+phicos*cv(3)
  end Subroutine cvSphCar0

  Subroutine rvSph(r,rho,theta,phi)
! spherical coordinates of a 3D vector r
    Implicit none
    Real(8) r(3),rho,theta,phi
    rho=r3Vec_Length(r)
    if(dabs(rho).lt.pSmall) then
      theta=0.0d0
      phi=0.0d0
    else
      theta=dacos(r(3).div.rho)
      phi=datan2(r(2),r(1))
    end if
  end Subroutine rvSph

  Recursive Subroutine Line3D(kEx,kPa,r,iD,nP,ixS,iyS,A)
! compute 3d Line multipole along the z axis
    Implicit none
    Complex(8), Allocatable :: sumExp(:,:),oldExp(:,:),AExp0(:,:)
    Real(8), Allocatable :: asi(:),aco(:)
    Complex(8) vh(3),A(10,nParN)
    Real(8) r(3),rl(3),rE1,sLoc(3,0:3),rLine,zLine,aLine,z,dz,phisin,phicos,rho
    Integer(4) kEx,kPa,idum,nLine,nP,nPm,m
    Integer(2) iType,iD,iHEk,iHEkl,iCSk,iCSl,minDeg,maxDeg,minOrd,maxOrd,nStepOrd,maxPar,maxPar0,ixS,iyS,minOrd0,k,kP,&
    & nR
    Logical lPL
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((tExp(kEx)%nPar.lt.1).or.(tExp(kEx)%iDom.ne.iD)) return
    iType=1
! parameters
    zLine=tExp(kEx)%rE(1)
    rLine=abs(tExp(kEx)%rE(2))
    minDeg=max(0_2,tExp(kEx)%iE(3))
    lPL=.false.
    if(tExp(kEx)%iE(4).lt.0) lPL=.true.
    maxDeg=max(minDeg,abs(tExp(kEx)%iE(4)))
! run thin wire approximation if minOrd<0
    minOrd=max(-1_2,tExp(kEx)%iE(1))
    if(minOrd.lt.0) then
      maxOrd=minOrd
      tExp(kEx)%iE(1)=minOrd
      tExp(kEx)%iE(2)=maxOrd
      iHEk=igetiHE(kEx)
      if((iHEk.eq.1).and.((ixS.eq.1).or.(iyS.eq.1))) return
      if((iHEk.eq.0).and.((ixS.eq.2).or.(iyS.eq.2))) return
      if(iHEk.lt.2_2) iHEk=1_2-iHEk ! exchange E and H as for 0 order
      if((iHEk.eq.2).and.((ixS.eq.2).or.(iyS.eq.2))) iHEk=1
      if((iHEk.eq.2).and.((ixS.eq.1).or.(iyS.eq.1))) iHEk=0
      call ThinLine3D(kEx,r,iD,iHEk,minDeg,maxDeg,lPL,nP,A)
      return
    end if
! parameters
    aLine=tExp(kEx)%rE(3)*rLine
    if(abs(aLine).lt.pSmall) aLine=1.0d0
    aLine=Pi/aLine
    rE1=abs(tExp(kEx)%rE(4))
    if((rE1.gt.pSmall).and.(r(3).gt.zLine).and.(r(3).lt.(zLine+rLine))) then
      rho=dsqrt(r(1)**2+r(2)**2)
      if((.not.lExpTest).and.(rho.lt.rE1)) return
    end if
    iHEk=igetiHE(kEx)
    iCSk=igetiCS(kEx)
    minOrd=max(0_2,tExp(kEx)%iE(1))
    maxOrd=max(minOrd,tExp(kEx)%iE(2))
    nStepOrd=max(1_2,tExp(kEx)%iE(5))
    minOrd0=minOrd
    tExp(kEx)%iE(1)=minOrd
    tExp(kEx)%iE(2)=maxOrd
    tExp(kEx)%iE(3)=minDeg
    tExp(kEx)%iE(5)=nStepOrd
    maxPar=tExp(kEx)%nPar
    maxPar0=2*maxPar
    nLine=tExp(kEx)%iE(6)
    if(nLine.lt.1) then
      rho=dsqrt(r(1)**2+r(2)**2) ! distance of field point r from Line
      if(r(3).lt.zLine) rho=dsqrt(rho**2+(r(3)-zLine)**2)
      if(r(3).gt.zLine+rLine) rho=dsqrt(rho**2+(r(3)-zLine-rLine)**2)
      nLine=min(-nLine,max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nLine),(rLine.div.rho)),4)))
    end if
    DeAllocate(AExp0,oldExp,sumExp,asi,aco,stat=idum)
    Allocate(AExp0(1:10,maxPar0),oldExp(1:10,tExp(kEx)%nPar),sumExp(1:10,tExp(kEx)%nPar),asi(0:maxDeg),aco(0:maxDeg), &
    & stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Line'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    k=0
    do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+tExp(kEx)%nPar
      k=k+1
      oldExp(1:10,k)=A(1:10,kP)
    end do
    sLoc=0.0d0
    sLoc(2,1)=1.0d0
    sLoc(3,2)=1.0d0
    sLoc(1,3)=1.0d0
    dz=rLine/Dble(nLine)
    z=-0.5d0*dz
    sumExp=(0.0d0,0.0d0)
    do nR=1,nLine
      z=z+dz
      if(lPL) then
        call Legenm(z*aLine/Pi-1.0d0,0.0d0,aco,Int4(maxDeg),0_4)
      else
        phisin=dsin(z*aLine)
        phicos=dcos(z*aLine)
        call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      end if
      sLoc(3,0)=zLine+z
      call vGlob2Loc(r,sLoc,rl)
      k=0
      if(minOrd.lt.1) then ! Dipoles with m=1, n=1 ; Statics: Monopoles
        minOrd0=nStepOrd
        kP=kPa
        kPa=Int4(tExp(kEx)%iOff)
        A(1:10,tExp(kEx)%iOff+1:tExp(kEx)%iOff+tExp(kEx)%nPar)=(0.0d0,0.0d0)
        iHEkl=iHEk
        if((maxPar.lt.2).and.(iHEkl.eq.2).and.(iCSk.eq.1)) iHEkl=1_2
        if(lMMPStat) then
          call Multipole3D(0,kPa,iType,rl,rE1,iD,iHEkl,0_2,0_2,0_2,0_2,maxPar,0_2,nPm,0_2,iyS,ixS,A) 
        else
          if(iHEkl.lt.2_2) iHEkl=1_2-iHEkl ! exchange E and H because of symmetry
          call Multipole3D(0,kPa,iType,rl,rE1,iD,iHEkl,1_2,1_2,1_2,1_2,maxPar,2_2,nPm,0_2,iyS,ixS,A) 
        end if
        kPa=kP
        iCSl=1_2
        if(iHEkl.eq.1_2) iCSl=2_2
        if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
        do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+nPm
          if((minDeg.eq.0).and.(iCSk.ne.iCSl)) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(A(1:3,kP),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)/Dble(nLine)
            call cvLoc2Glob(A(4:6,kP),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)/Dble(nLine)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+A(9:10,kP)/Dble(nLine) ! electric and magnetic scalar pot.
          end if
          do m=max(1_2,minDeg),maxDeg
            if(iCSk.ne.iCSl) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(A(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nLine)
              call cvLoc2Glob(A(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nLine)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*A(9:10,kP)/Dble(nLine) ! electric and magnetic scalar pot.
            end if
            if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(A(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nLine)
              call cvLoc2Glob(A(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nLine)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*A(9:10,kP)/Dble(nLine) ! electric and magnetic scalar pot.
            end if
          end do
          if(k.gt.maxPar) Exit
          if(iHEk.eq.2_2) iCSl=modulo(iCSl,2_2)+1_2
        end do
      end if
      if((maxOrd.gt.0).and.(k.lt.maxPar)) then ! Multipoles with m=0, n=1,2,3,...
        kP=kPa
        kPa=Int4(tExp(kEx)%iOff)
        AExp0(1:10,1:maxPar0)=(0.0d0,0.0d0)
        call Multipole3D0(iType,rl,rE1,iD,iHEk,max(1_2,minOrd0),maxOrd,nStepOrd,maxPar0,ixS,iyS,nPm,AExp0) 
        kPa=kP
        iCSl=2_2
        if(iHEk.eq.1_2) iCSl=1_2
        if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
        do kP=1,min(maxPar0,nPm)
          if((minDeg.eq.0).and.(iCSk.ne.iCSl)) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AExp0(1:3,kP),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)/Dble(nLine)
            call cvLoc2Glob(AExp0(4:6,kP),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)/Dble(nLine)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+A(9:10,kP)/Dble(nLine) ! electric and magnetic scalar pot.
          end if
          do m=max(1_2,minDeg),maxDeg
            if(iCSk.ne.iCSl) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(AExp0(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nLine)
              call cvLoc2Glob(AExp0(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nLine)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*A(9:10,kP)/Dble(nLine) ! electric and magnetic scalar pot.
            end if
            if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(AExp0(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nLine)
              call cvLoc2Glob(AExp0(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nLine)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*A(9:10,kP)/Dble(nLine) ! electric and magnetic scalar pot.
            end if
          end do
          if(k.gt.maxPar) Exit
          if(iHEk.eq.2_2) iCSl=modulo(iCSl,2_2)+1_2
        end do
      end if
    end do
    nP=min(k,maxPar)
    k=0
    do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+min(tExp(kEx)%nPar,nP)
      k=k+1
      A(1:10,kP)=oldExp(1:10,k)+sumExp(1:10,k)
    end do
    DeAllocate(AExp0,oldExp,sumExp,asi,aco,stat=idum)
  end Subroutine Line3D

  Recursive Subroutine Ring3D(kEx,kPa,r,iD,nP,izS,A)
! compute 3d ring multipole in the xz plane, center at (0,0,0)
    Implicit none
    Complex(8), Allocatable :: sumExp(:,:),oldExp(:,:),AExp0(:,:)
    Real(8), Allocatable :: asi(:),aco(:)
    Complex(8) vh(3),A(10,nParN)
    Real(8) r(3),rl(3),rE1,sLoc(3,0:3),rRing,f1Ring,f2Ring,aRing,f,df,phisin,phicos,rho,&
    & rlen,xN,yN,sN
    Integer(4) kEx,kPa,idum,nRing,iEven,iOdd,nP,nPm,m
    Integer(2) iType,iD,iHEk,iHEkl,iCSk,iCSl,mD,minDeg,maxDeg,mStepDeg,minOrd,maxOrd,maxPar,maxPar0,izS,minOrd0,k,kP,nR
    Logical ldum,lPL,lFull
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((tExp(kEx)%nPar.lt.1).or.(tExp(kEx)%iDom.ne.iD)) return
    iType=1
! parameters
    rRing=tExp(kEx)%rE(1)
    lPL=.false.
    minDeg=max(0_2,tExp(kEx)%iE(3))
    if(tExp(kEx)%iE(4).lt.0) lPL=.true.
    maxDeg=max(minDeg,abs(tExp(kEx)%iE(4)))
    mStepDeg=max(1_2,tExp(kEx)%iE(5))
    mD=minDeg
    if(minDeg.lt.1) mD=mStepDeg
! run thin wire approximation if minOrd<0
    minOrd=max(-1_2,tExp(kEx)%iE(1))
    if(minOrd.lt.0) then
      maxOrd=minOrd
      tExp(kEx)%iE(1)=minOrd
      tExp(kEx)%iE(2)=maxOrd
      iHEk=igetiHE(kEx)
      if((iHEk.eq.1).and.(izS.eq.1)) return
      if((iHEk.eq.0).and.(izS.eq.2)) return
      if(iHEk.lt.2_2) iHEk=1_2-iHEk ! exchange E and H as for 0 order
      if((iHEk.eq.2).and.(izS.eq.2)) iHEk=1
      if((iHEk.eq.2).and.(izS.eq.1)) iHEk=0
      call ThinRing3D(kEx,r,iD,iHEk,minDeg,maxDeg,lPL,nP,A)
      return
    end if
! parameters
    f1Ring=Pi*tExp(kEx)%rE(2)/180.0d0
    f2Ring=f1Ring+Pi*abs(tExp(kEx)%rE(3))/180.0d0
    aRing=tExp(kEx)%rE(4)
    if(abs(aRing).lt.pSmall) aRing=1.0d0
    if(f1Ring.gt.f2Ring) then
      f=f1Ring
      f1Ring=f2Ring
      f2Ring=f
    end if
    if(f2Ring.gt.f1Ring+(2.0d0*Pi-1.0d-6)) then ! full circle
      f2Ring=f1Ring+2.0d0*Pi
      aRing=1.0d0/aRing
      lFull=.true.
    else ! arc
      aRing=Pi.div.((f2Ring-f1Ring)*aRing)
      lFull=.false.
    end if
    rE1=abs(tExp(kEx)%rE(5))
    if(rE1.gt.pSmall) then
      if(lFull) then
        call DistPtCircle(r(1),r(3),0.0d0,0.0d0,rRing,rho,xN,yN,sN,ldum)
      else
        call DistPtArc(r(1),r(3),0.0d0,0.0d0,rRing*dcos(f1Ring),rRing*dsin(f1Ring),f2Ring-f1Ring,2.0d300,1_2,rho,xN,yN,sN,ldum)
      end if
      rho=dsqrt(r(2)**2+rho**2) ! distance of field point r from ring
      if((.not.lExpTest).and.(rho.lt.rE1)) return
    end if
    iHEk=igetiHE(kEx)
    iCSk=igetiCS(kEx)
    minOrd=max(0_2,tExp(kEx)%iE(1))
    maxOrd=max(minOrd,tExp(kEx)%iE(2))
    minOrd0=minOrd
    tExp(kEx)%iE(1)=minOrd
    tExp(kEx)%iE(2)=maxOrd
    tExp(kEx)%iE(3)=minDeg
    tExp(kEx)%iE(5)=mStepDeg
    maxPar=tExp(kEx)%nPar
    maxPar0=2*maxPar
    nRing=tExp(kEx)%iE(6)
    if(nRing.lt.1) then
      if(lFull) then
        call DistPtCircle(r(1),r(3),0.0d0,0.0d0,rRing,rho,xN,yN,sN,ldum)
      else
        call DistPtArc(r(1),r(3),0.0d0,0.0d0,rRing*dcos(f1Ring),rRing*dsin(f1Ring),f2Ring-f1Ring,2.0d300,1_2,rho,xN,yN,sN,ldum)
      end if
      rho=dsqrt(r(2)**2+rho**2) ! distance of field point r from ring
      rlen=rRing*dabs(f2Ring-f1Ring) ! length of ring
      nRing=min(-nRing,max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nRing),(rlen.div.rho)),4)))
    end if
    DeAllocate(AExp0,oldExp,sumExp,asi,aco,stat=idum)
    Allocate(AExp0(1:10,maxPar0),oldExp(1:10,tExp(kEx)%nPar),sumExp(1:10,tExp(kEx)%nPar),asi(0:maxDeg),aco(0:maxDeg), &
    & stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Ring'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    k=0
    do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+tExp(kEx)%nPar
      k=k+1
      oldExp(1:10,k)=A(1:10,kP)
    end do
    sLoc=0.0d0
    sLoc(2,3)=-1.0d0
    df=(f2Ring-f1Ring)/Dble(nRing)
    f=-0.5d0*df
    sumExp=(0.0d0,0.0d0)
    do nR=1,nRing
      f=f+df
      phisin=dsin(f+f1Ring)
      phicos=dcos(f+f1Ring)
      sLoc(1,0)=rRing*phicos
      sLoc(3,0)=rRing*phisin
      sLoc(1,1)=phicos
      sLoc(1,2)=-phisin  !!! don't remember why I set these signs
      sLoc(3,1)=phisin   !!!
      sLoc(3,2)=phicos
      call vGlob2Loc(r,sLoc,rl)
      if(lPL) then
        call Legenm(f*aRing/Pi-1.0d0,0.0d0,aco,Int4(maxDeg),0_4)
      else
        phisin=dsin(f*aRing)
        phicos=dcos(f*aRing)
        call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      end if
      k=0
      if(minOrd.lt.1) then ! Dipoles with m=1, n=1 ; Statics: Monopoles
        minOrd0=1
        kP=kPar
        kPa=Int4(tExp(kEx)%iOff)
        A(1:10,tExp(kEx)%iOff+1:tExp(kEx)%iOff+tExp(kEx)%nPar)=(0.0d0,0.0d0)
        iHEkl=iHEk
        if((maxPar.lt.2).and.(iHEkl.eq.2).and.(iCSk.eq.1)) iHEkl=1_2
        if(lMMPStat) then
          call Multipole3D(0,kPa,iType,rl,rE1,iD,iHEkl,0_2,0_2,0_2,0_2,maxPar,0_2,nPm,0_2,0_2,izS,A) 
        else
          if(iHEkl.lt.2_2) iHEkl=1_2-iHEkl ! exchange E and H because of symmetry
          call Multipole3D(0,kPa,iType,rl,rE1,iD,iHEkl,1_2,1_2,1_2,1_2,maxPar,2_2,nPm,0_2,0_2,izS,A) 
        end if
        kPa=kP
        iCSl=1_2
        if(iHEkl.eq.1_2) iCSl=2_2
        if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
        do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+nPm
          if((minDeg.eq.0).and.(iCSk.ne.iCSl)) then ! degree 0, E or H ring multipoles, order 0
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(A(1:3,kP),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)/Dble(nRing)
            call cvLoc2Glob(A(4:6,kP),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)/Dble(nRing)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+A(9:10,kP)/Dble(nRing) ! electric and magnetic scalar pot.
          end if
          do m=mD,maxDeg,mStepDeg ! degree m, ring multipoles, order 0
            if(iCSk.ne.iCSl) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(A(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nRing)
              call cvLoc2Glob(A(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nRing)
              call cvLoc2Glob(A(7:9,kP),sLoc,vh)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*A(9:10,kP)/Dble(nRing) ! electric and magnetic scalar pot.
            end if
            if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(A(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nRing)
              call cvLoc2Glob(A(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nRing)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*A(9:10,kP)/Dble(nRing) ! electric and magnetic scalar pot.
            end if
          end do
          if(k.gt.maxPar) Exit
          if(iHEk.eq.2_2) iCSl=modulo(iCSl,2_2)+1_2
        end do
      end if
      if((maxOrd.gt.0).and.(k.lt.maxPar)) then ! Multipoles, m=0, n=1,2,3,...
        kP=kPa 
        kPa=Int4(tExp(kEx)%iOff)
        AExp0(1:10,1:maxPar0)=(0.0d0,0.0d0)
        call Multipole3D0(iType,rl,rE1,iD,iHEk,max(1_2,minOrd0),maxOrd,1_2,maxPar0,izS,0_2,nPm,AExp0)
        kPa=kP
        iCSl=2_2
        if(iHEk.eq.1_2) iCSl=1_2
        if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
        do kP=1,min(maxPar0,nPm)
          if((minDeg.eq.0).and.(iCSk.ne.iCSl)) then ! degree 0, E or H ring multipoles, order n
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AExp0(1:3,kP),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)/Dble(nRing)
            call cvLoc2Glob(AExp0(4:6,kP),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)/Dble(nRing)
           if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+A(9:10,kP)/Dble(nRing) ! electric and magnetic scalar pot.
          end if
          do m=mD,maxDeg,mStepDeg ! degree m, E or H ring multipoles, order n
            iEven=modulo(m,2)+1
            iOdd=3-iEven
            if(iCSk.ne.iCSl) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(AExp0(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nRing)
              call cvLoc2Glob(AExp0(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nRing)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*A(9:10,kP)/Dble(nRing) ! electric and magnetic scalar pot.
            end if
            if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(AExp0(1:3,kP),sLoc,vh)
              sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nRing)
              call cvLoc2Glob(AExp0(4:6,kP),sLoc,vh)
              sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nRing)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*A(9:10,kP)/Dble(nRing) ! electric and magnetic scalar pot.
            end if
          end do
          if(k.gt.maxPar) Exit
          if(iHEk.eq.2_2) iCSl=modulo(iCSl,2_2)+1_2
        end do
      end if
    end do
    nP=min(k,maxPar)
    k=0
    do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+min(tExp(kEx)%nPar,nP)
      k=k+1
      A(1:10,kP)=oldExp(1:10,k)+sumExp(1:10,k)
    end do
    DeAllocate(oldExp,sumExp,asi,aco,stat=idum)
  end Subroutine Ring3D

  Recursive Subroutine Spiral3D(kEx,kPa,r,iD,nP,A)
! compute 3d spiral multipole
    Implicit none
    Complex(8), Allocatable :: sumExp(:,:),oldExp(:,:),AExp0(:,:)
    Real(8), Allocatable :: asi(:),aco(:),sLoc(:,:,:)
    Complex(8) vh(3),A(10,nParN)
    Real(8) r(3),rl(3),dr,df,dz,aSpiral,rE1,ra(3),Planel(3,0:3),d,dmin,sL,s,ds,vt(3),phisin,phicos
    Integer(4) kEx,kPa,nP,nSpiral,idum,i,nPm,m
    Integer(2) iD,iType,iHEk,iHEkl,iCSk,iCSl,minDeg,maxDeg,minOrd,maxOrd,nStepOrd,maxPar,maxPar0,minOrd0,iCl(1),k,kP,nR
    Logical lPL
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((tExp(kEx)%nPar.lt.1).or.(tExp(kEx)%iDom.ne.iD)) return
    iType=1
    minDeg=max(0_2,tExp(kEx)%iE(3))
    lPL=.false.
    if(tExp(kEx)%iE(4).lt.0) lPL=.true.
    maxDeg=max(minDeg,abs(tExp(kEx)%iE(4)))
! run thin wire approximation if minOrd<0
    minOrd=max(-1_2,tExp(kEx)%iE(1))
    if(minOrd.lt.0) then
      maxOrd=minOrd
      tExp(kEx)%iE(1)=minOrd
      tExp(kEx)%iE(2)=maxOrd
      iHEk=igetiHE(kEx)
      if(iHEk.lt.2_2) iHEk=1_2-iHEk ! exchange E and H as for 0 order
      call ThinSpiral3D(kEx,r,iD,iHEk,minDeg,maxDeg,lPL,nP,A)
      return
    end if
! set real parameters
    dr=tExp(kEx)%rE(1)
    df=tExp(kEx)%rE(2)
    dz=tExp(kEx)%rE(3)
    ra(1)=tExp(kEx)%xo
    ra(2)=tExp(kEx)%yo
    ra(3)=0.0d0
    d=Dist3DPtAxis(ra,tExp(kEx)%O(1:3),tExp(kEx)%e(1:3))
    sL=SpiralLength(d,dr,df,dz)
    aSpiral=tExp(kEx)%rE(4)*sL
    if(abs(aSpiral).lt.pSmall) aSpiral=1.0d0
    aSpiral=Pi/aSpiral
    rE1=abs(tExp(kEx)%rE(5))
! find spiral length and distance of point r from spiral
    nSpiral=Nint(min(1000.0d0,max(3.0d0,(abs(tExp(kEx)%rE(2))*0.101d0))),4)
    Planel=0.0d0
    Planel(1,1)=1.0d0
    Planel(2,2)=1.0d0
    Planel(3,3)=1.0d0
    if(Allocated(sLoc)) DeAllocate(sLoc,stat=idum)
    Allocate(sLoc(3,nSpiral,1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Spiral'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    else
      call Pt2Spiral(ra,tExp(kEx)%O,tExp(kEx)%e,dr,df,dz,nSpiral,Planel,sLoc(1:3,1:nSpiral,1))
      if(rE1.gt.pSmall) then
        dmin=pBig
        do i=1,nSpiral
          d=Dist3DPtPt(r,sLoc(1:3,i,1))
          if(d.lt.dmin) dmin=d
        end do
        if((.not.lExpTest).and.(dmin.lt.rE1)) return
      end if
    end if
    if(Allocated(sLoc)) DeAllocate(sLoc,stat=idum)
! set integer parameters
    iHEk=igetiHE(kEx)
    iCSk=igetiCS(kEx)
    minOrd=max(0_2,tExp(kEx)%iE(1))
    maxOrd=max(minOrd,tExp(kEx)%iE(2))
    nStepOrd=max(1_2,tExp(kEx)%iE(5))
    minOrd0=minOrd
    tExp(kEx)%iE(1)=minOrd
    tExp(kEx)%iE(2)=maxOrd
    tExp(kEx)%iE(3)=minDeg
    tExp(kEx)%iE(5)=nStepOrd
    maxPar=tExp(kEx)%nPar
    maxPar0=2*maxPar
    nSpiral=tExp(kEx)%iE(6)
    if(nSpiral.lt.1) nSpiral=min(-nSpiral,max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nSpiral),(sL.div.dmin)),4)))
    if(nSpiral.lt.1) nSpiral=max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nSpiral),(sL.div.dmin)),4))
! auxiliary memory allocation
    DeAllocate(AExp0,sLoc,oldExp,sumExp,asi,aco,stat=idum)
    Allocate(AExp0(1:10,maxPar0),sLoc(3,0:3,nSpiral),oldExp(1:10,tExp(kEx)%nPar),sumExp(1:10,tExp(kEx)%nPar),asi(0:maxDeg), &
    &        aco(0:maxDeg),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Spiral'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! get local coordinate systems
    vt=ra-tExp(kEx)%O
    s=Dot_Product(tExp(kEx)%e,vt)
    vt=vt-s*tExp(kEx)%e
    call Unit3DV(vt)
    call Pt2Spiral(ra,tExp(kEx)%O,tExp(kEx)%e,dr,df,dz,-nSpiral,Planel, &
    & sLoc(1:3,0,1:nSpiral),0,0,-257_2,0_2,iCl,sLoc(1:3,1,1:nSpiral),sLoc(1:3,2,1:nSpiral),sLoc(1:3,3,1:nSpiral),vt)
    do nR=1,nSpiral
      call Unit3DV(sLoc(1:3,1,nR))
      call Unit3DV(sLoc(1:3,2,nR))
      call Unit3DV(sLoc(1:3,3,nR))
    end do
    k=0
    do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+tExp(kEx)%nPar
      k=k+1
      oldExp(1:10,k)=A(1:10,kP)
    end do
! loop over all multipole expansions
    sumExp=(0.0d0,0.0d0)
    ds=sL/Dble(nSpiral)
    s=-0.5d0*ds
    do nR=1,nSpiral
      s=s+ds
      if(lPL) then
        call Legenm(s*aSpiral/Pi-1.0d0,0.0d0,aco,Int4(maxDeg),0_4)
      else
        phisin=dsin(s*aSpiral)
        phicos=dcos(s*aSpiral)
        call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      end if
      call vGlob2Loc(r,sLoc(1:3,0:3,nR),rl)
      k=0
      if(minOrd.lt.1) then ! Dipoles with m=1, n=1 ; Statics: Monopoles
        minOrd0=nStepOrd
        kP=kPa
        kPa=Int4(tExp(kEx)%iOff)
        A(1:10,tExp(kEx)%iOff+1:tExp(kEx)%iOff+tExp(kEx)%nPar)=(0.0d0,0.0d0)
        iHEkl=iHEk
        if((maxPar.lt.2).and.(iHEkl.eq.2).and.(iCSk.eq.1)) iHEkl=1_2
        if(lMMPStat) then
          call Multipole3D(0,kPa,iType,rl,rE1,iD,iHEkl,0_2,0_2,0_2,0_2,maxPar,0_2,nPm,0_2,0_2,0_2,A) 
        else
          if(iHEkl.lt.2_2) iHEkl=1_2-iHEkl ! exchange E and H because of symmetry
          call Multipole3D(0,kPa,iType,rl,rE1,iD,iHEkl,1_2,1_2,1_2,1_2,maxPar,2_2,nPm,0_2,0_2,0_2,A) 
        end if
        kPa=kP
        iCSl=1_2
        if(iHEkl.eq.1_2) iCSl=2_2
        if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
        do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+nPm
          if((minDeg.eq.0).and.(iCSk.ne.iCSl)) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(A(1:3,kP),sLoc(1:3,0:3,nR),vh)
            sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)/Dble(nSpiral)
            call cvLoc2Glob(A(4:6,kP),sLoc(1:3,0:3,nR),vh)
            sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)/Dble(nSpiral)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+A(9:10,kP)/Dble(nSpiral) ! electric and magnetic scalar pot.
          end if
          do m=max(1_2,minDeg),maxDeg
            if(iCSk.ne.iCSl) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(A(1:3,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nSpiral)
              call cvLoc2Glob(A(4:6,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nSpiral)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*A(9:10,kP)/Dble(nSpiral) ! electric and magnetic scalar pot.
            end if
            if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
              call cvLoc2Glob(A(1:3,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nSpiral)
              call cvLoc2Glob(A(4:6,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nSpiral)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*A(9:10,kP)/Dble(nSpiral) ! electric and magnetic scalar pot.
            end if
          end do
          if(k.gt.maxPar) Exit
          if(iHEk.eq.2_2) iCSl=modulo(iCSl,2_2)+1_2
        end do
      end if
      if((maxOrd.gt.0).and.(k.lt.maxPar)) then ! Multipoles with m=0, n=1,2,3,...
        kP=kPa
        kPa=Int4(tExp(kEx)%iOff)
        AExp0(1:10,1:maxPar0)=(0.0d0,0.0d0)
        call Multipole3D0(iType,rl,rE1,iD,iHEk,max(1_2,minOrd0),maxOrd,nStepOrd,maxPar0,0_2,0_2,nPm,AExp0)
        kPa=kP
        iCSl=2_2
        if(iHEk.eq.1_2) iCSl=1_2
        if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
        do kP=1,min(maxPar0,nPm)
          if((minDeg.eq.0).and.(iCSk.ne.iCSl)) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AExp0(1:3,kP),sLoc(1:3,0:3,nR),vh)
            sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)/Dble(nSpiral)
            call cvLoc2Glob(AExp0(4:6,kP),sLoc(1:3,0:3,nR),vh)
            sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)/Dble(nSpiral)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+A(9:10,kP)/Dble(nSpiral) ! electric and magnetic scalar pot.
          end if
          do m=max(1_2,minDeg),maxDeg
            if(iCSk.ne.iCSl) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(AExp0(1:3,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nSpiral)
              call cvLoc2Glob(AExp0(4:6,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nSpiral)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*A(9:10,kP)/Dble(nSpiral) ! electric and magnetic scalar pot.
            end if
            if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
              k=k+1
              if(k.gt.maxPar) Exit
              call cvLoc2Glob(AExp0(1:3,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nSpiral)
              call cvLoc2Glob(AExp0(4:6,kP),sLoc(1:3,0:3,nR),vh)
              sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nSpiral)
              if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*A(9:10,kP)/Dble(nSpiral) ! electric and magnetic scalar pot.
            end if
          end do
          if(k.gt.maxPar) Exit
          if(iHEk.eq.2_2) iCSl=modulo(iCSl,2_2)+1_2
        end do
      end if
    end do
    nP=min(k,maxPar)
    k=0
    do kP=tExp(kEx)%iOff+1,tExp(kEx)%iOff+min(tExp(kEx)%nPar,nP)
      k=k+1
      A(1:10,kP)=oldExp(1:10,k)+sumExp(1:10,k)
    end do
    DeAllocate(AExp0,sLoc,oldExp,sumExp,asi,aco,stat=idum)
  end Subroutine Spiral3D

  Subroutine ThinWireElementE3D(r0,iD,d,AFld)
! compute thin wire element with electric current along z axis from 0 to d, current:=sin(kz)
    Implicit none
    Real(8) r0(3),r(3),d,rho,rho2,dz,dz2,rr,rr2,phicos,phisin
    Complex(8) Ez,Er,Hf,cf,cioeps,cikr,cs,cc,AFld(10),A0Fld(4)
    Integer(2) iD,lf
    AFld=(0.0d0,0.0d0)
    if(lMMPStat) then
      call ThinWireElement0(r0,d,A0Fld)
      AFld(4:6)=A0Fld(1:3) ! for statics: el. current generates mag. field, no need to sum up!
      AFld(9)=A0Fld(4)
      return
    end if
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    cioeps=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Eps0*eDom(lf)
    r=r0
    rho2=r(1)**2+r(2)**2
    rho=dsqrt(rho2)
    if(rho.gt.1.0d-150) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-30
      rho2=1.0d-60
    end if
    dz=r(3)-d ! upper bound (zs=d)
    dz2=dz**2
    rr2=rho2+dz2
    rr=dsqrt(rr2)
    cikr=(0.0d0,1.0d0)*kcFld*rr
    cf=cdexp(cikr)/(4.0d0*Pi*rr)
    cs=cdsin(kcFld*d)
    cc=cdcos(kcFld*d)
    Ez=-(cf/cioeps)*(kcFld*cs+dz*(1.0d0-cikr)*cc/rr2)
    Er=(cf/(rho*cioeps))*(kcFld*dz*cs-(rho2+dz2*cikr)*cc/rr2)
    Hf=-(cf/rho)*((0.0d0,1.0d0)*rr*cs+dz*cc)
    dz=r(3) ! lower bound (zs=0)
    dz2=dz**2
    rr2=rho2+dz2
    rr=dsqrt(rr2)
    cikr=(0.0d0,1.0d0)*kcFld*rr
    cf=cdexp(cikr)/(4.0d0*Pi*rr)
    Ez=Ez+(cf/cioeps)*(dz*(1.0d0-cikr)/rr2)
    Er=Er+(cf/(rho*cioeps))*((rho2+dz2*cikr)/rr2)
    Hf=Hf+(cf/rho)*(dz)
    AFld(1)=phicos*Er
    AFld(2)=phisin*Er
    AFld(3)=Ez
    AFld(4)=-phisin*Hf
    AFld(5)=+phicos*Hf
  end Subroutine ThinWireElementE3D

  Subroutine ThinWireElementM3D(r0,iD,d,AFld)
! compute thin wire element with magnetic current along z axis from 0 to d, current:=sin(kz)
    Implicit none
    Real(8) r0(3),r(3),d,rho,rho2,dz,dz2,rr,rr2,phicos,phisin
    Complex(8) Hz,Hr,Ef,cf,ciomu,cikr,cs,cc,AFld(10),A0Fld(4)
    Integer(2) iD,lf
    AFld=(0.0d0,0.0d0)
    if(lMMPStat) then
      call ThinWireElement0(r0,d,A0Fld)
      AFld(1:3)=A0Fld(1:3) ! for statics: mag. current generates el. field, no need to sum up!
      AFld(10)=A0Fld(4)
      return
    end if
    lf=Max(1_2,Min(Int2(nDom),iD))
    kcFld=Wnumber(lf)
    ciomu=(0.0d0,1.0d0)*(2.0d0*Pi)*fcFld*Mue0*uDom(lf)
    r=r0
    rho2=r(1)**2+r(2)**2
    rho=dsqrt(rho2)
    if(rho.gt.1.0d-150) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-30
      rho2=1.0d-60
    end if
    dz=r(3)-d ! upper bound (zs=d)
    dz2=dz**2
    rr2=rho2+dz2
    rr=dsqrt(rr2)
    cikr=(0.0d0,1.0d0)*kcFld*rr
    cf=cdexp(cikr)/(4.0d0*Pi*rr)
    cs=cdsin(kcFld*d)
    cc=cdcos(kcFld*d)
    Hz=(cf/ciomu)*(kcFld*cs+dz*(1.0d0-cikr)*cc/rr2)
    Hr=-(cf/(rho*ciomu))*(kcFld*dz*cs-(rho2+dz2*cikr)*cc/rr2)
    Ef=-(cf/rho)*((0.0d0,1.0d0)*rr*cs+dz*cc)
    dz=r(3) ! lower bound (zs=0)
    dz2=dz**2
    rr2=rho2+dz2
    rr=dsqrt(rr2)
    cikr=(0.0d0,1.0d0)*kcFld*rr
    cf=cdexp(cikr)/(4.0d0*Pi*rr)
    Hz=Hz-(cf/ciomu)*(dz*(1.0d0-cikr)/rr2)
    Hr=Hr-(cf/(rho*ciomu))*((rho2+dz2*cikr)/rr2)
    Ef=Ef+(cf/rho)*(dz)
    AFld(1)=-phisin*Ef
    AFld(2)=phicos*Ef
    AFld(4)=phicos*Hr
    AFld(5)=phisin*Hr
    AFld(6)=Hz
  end Subroutine ThinWireElementM3D 
  
  Subroutine ThinWireElement0(r,d,A0Fld)
! compute static thin wire element constant charge along z axis from 0 to d
    Implicit none
    Real(8) r(3),d,rho,rho2,z,srd,sr0,sr0z,srdz,phicos,phisin
    Complex(8) A0Fld(4),Er
    rho2=r(1)**2+r(2)**2
    rho=dsqrt(rho2)
    z=r(3)
    if(rho.gt.1.0d-150) then
      phicos=r(1)/rho
      phisin=r(2)/rho
    else
      phicos=1.0d0
      phisin=0.0d0
      rho=1.0d-30
      rho2=1.0d-60
    end if
    srd=dsqrt((d-z)**2+rho2)
    sr0=dsqrt(z**2+rho2)
    srdz=srd+d-z
    if(dabs(srdz).lt.1.0d-30) srdz=1.0d-30
    sr0z=sr0-z
    if(dabs(sr0z).lt.1.0d-30) sr0z=1.0d-30
    A0Fld(4)=dlog(srdz)-dlog(sr0z) ! V
    A0Fld(3)=1.0d0/sr0-1.0d0/srd   ! Ez
    Er=rho/(srd*(srdz))-rho/(sr0*(sr0z))
    A0Fld(1)=phicos*Er             ! Ex
    A0Fld(2)=phisin*Er             ! Ey
  end Subroutine ThinWireElement0

  Recursive Subroutine ThinLine3D(kEx,r,iD,iHEk,minDeg,maxDeg,lPL,nP,A)
! compute 3d thin wire line
    Implicit none
    Complex(8), Allocatable :: sumExp(:,:)
    Real(8), Allocatable :: asi(:),aco(:)
    Complex(8) A(10,nParN)
    Complex(8) AEFld(10),AMFld(10)
    Real(8) r(3),rl(3),rE1,rLine,zLine,aLine,z,dz,phisin,phicos,rho
    Integer(4) kEx,idum,nLine,nP,m
    Integer(2) iD,iHEk,iCSk,iCSl,minDeg,maxDeg,maxPar,k,kPa,nR
    Logical lPL
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((tExp(kEx)%nPar.lt.1).or.(tExp(kEx)%iDom.ne.iD)) return
    zLine=tExp(kEx)%rE(1)
    rLine=abs(tExp(kEx)%rE(2))
    aLine=tExp(kEx)%rE(3)*rLine
    if(abs(aLine).lt.pSmall) aLine=1.0d0
    aLine=Pi/aLine
    rE1=tExp(kEx)%rE(4)
    if((rE1.gt.pSmall).and.(r(3).gt.zLine).and.(r(3).lt.(zLine+rLine))) then
      rho=dsqrt(r(1)**2+r(2)**2)
      if(.not.lExpTest) then
        if(rho.lt.rE1) return
      end if
    end if
    iCSk=igetiCS(kEx)
    maxPar=tExp(kEx)%nPar
    nLine=tExp(kEx)%iE(6)
    if(nLine.lt.1) then
      rho=dsqrt(r(1)**2+r(2)**2) ! distance of field point r from Line
      if(r(3).lt.zLine) rho=dsqrt(rho**2+(r(3)-zLine)**2)
      if(r(3).gt.zLine+rLine) rho=dsqrt(rho**2+(r(3)-zLine-rLine)**2)
      nLine=min(-nLine,max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nLine),(rLine.div.rho)),4)))
    end if
    DeAllocate(sumExp,asi,aco,stat=idum)
    Allocate(sumExp(1:10,tExp(kEx)%nPar),asi(0:maxDeg),aco(0:maxDeg),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Thin-Wire-Line'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    dz=rLine/Dble(nLine)
    z=0.0d0
    sumExp=(0.0d0,0.0d0)
    rl=r
    do nR=1,nLine
      if(lPL) then
        call Legenm((z+0.5d0*dz)*aLine/Pi-1.0d0,0.0d0,aco,Int4(maxDeg),0_4)
      else
        phisin=dsin((z+0.5d0*dz)*aLine)
        phicos=dcos((z+0.5d0*dz)*aLine)
        call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      end if
      if(iHEk.ne.1) then
        call ThinWireElementE3D(rl,iD,dz,AEFld) 
      end if
      if(iHEk.ne.0) then
        call ThinWireElementM3D(rl,iD,dz,AMFld) 
      end if
      k=0
      iCSl=1_2
      if(iHEk.eq.1_2) iCSl=2_2
      if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
      if((minDeg.le.0).and.(iCSk.ne.iCSl)) then
        if(iHEk.ne.1) then
          k=k+1
          if(k.gt.maxPar) Exit
          sumExp(1:3,k)=sumExp(1:3,k)+AEFld(1:3)
          sumExp(4:6,k)=sumExp(4:6,k)+AEFld(4:6)
          if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+AEFld(9:10)
        end if
        if(iHEk.ne.0) then
          k=k+1
          if(k.gt.maxPar) Exit
          sumExp(1:3,k)=sumExp(1:3,k)+AMFld(1:3)
          sumExp(4:6,k)=sumExp(4:6,k)+AMFld(4:6)
          if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+AMFld(9:10)
        end if
      end if
      do m=max(1_2,minDeg),maxDeg
        if(iHEk.ne.1_2) then
          if(iCSk.ne.iCSl) then
            k=k+1
            if(k.gt.maxPar) Exit
            sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*AEFld(1:3)
            sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*AEFld(4:6)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*AEFld(9:10)
          end if
          if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
            sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*AEFld(1:3)
            sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*AEFld(4:6)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*AEFld(9:10)
          end if
        end if
        if(iHEk.ne.0_2) then
          if(iCSk.ne.iCSl) then
            k=k+1
            if(k.gt.maxPar) Exit
            sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*AMFld(1:3)
            sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*AMFld(4:6)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*AMFld(9:10)
          end if
          if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
            sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*AMFld(1:3)
            sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*AMFld(4:6)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*AMFld(9:10)
          end if
        end if
      end do
      if(k.gt.maxPar) Exit
      z=z+dz
      rl(3)=rl(3)-dz
    end do
    nP=min(k,maxPar)
    k=0
    do kPa=tExp(kEx)%iOff+1,tExp(kEx)%iOff+min(tExp(kEx)%nPar,nP)
      k=k+1
      A(1:10,kPa)=A(1:10,kPa)+sumExp(1:10,k)
    end do
    DeAllocate(sumExp,asi,aco,stat=idum)
  end Subroutine ThinLine3D

  Recursive Subroutine ThinRing3D(kEx,r,iD,iHEk,minDeg,maxDeg,lPL,nP,A)
! compute 3d thin wire ring
    Implicit none
    Complex(8), Allocatable :: sumExp(:,:)
    Real(8), Allocatable :: asi(:),aco(:)
    Complex(8) vh(3),AEFld(10),AMFld(10),A(10,nParN)
    Real(8) r(3),rl(3),rE1,sLoc(3,0:3),rRing,f1Ring,f2Ring,aRing,f,df,phisin,phicos,rho,&
    & rlen,xN,yN,sN,delta
    Integer(4) kEx,idum,nRing,nP,m
    Integer(2) iD,iHEk,iCSk,iCSl,minDeg,maxDeg,maxPar,k,kPa,nR
    Logical ldum,lPL,lFull
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((tExp(kEx)%nPar.lt.1).or.(tExp(kEx)%iDom.ne.iD)) return
    rRing=tExp(kEx)%rE(1)
    f1Ring=Pi*tExp(kEx)%rE(2)/180.0d0
    f2Ring=f1Ring+Pi*abs(tExp(kEx)%rE(3))/180.0d0
    aRing=tExp(kEx)%rE(4)
    if(abs(aRing).lt.pSmall) aRing=1.0d0
    if(f1Ring.gt.f2Ring) then
      f=f1Ring
      f1Ring=f2Ring
      f2Ring=f
    end if
    if(f2Ring.gt.f1Ring+(2.0d0*Pi-1.0d-6)) then ! full circle
      f2Ring=f1Ring+2.0d0*Pi
      aRing=1.0d0/aRing
      lFull=.true.
    else ! arc
      aRing=Pi.div.((f2Ring-f1Ring)*aRing)
      lFull=.false.
    end if
    rE1=tExp(kEx)%rE(5)
    if(rE1.gt.pSmall) then
      if(lFull) then
        call DistPtCircle(r(1),r(3),0.0d0,0.0d0,rRing,rho,xN,yN,sN,ldum)
      else
        call DistPtArc(r(1),r(3),0.0d0,0.0d0,rRing*dcos(f1Ring),rRing*dsin(f1Ring),f2Ring-f1Ring,2.0d300,1_2,rho,xN,yN,sN,ldum)
      end if
      rho=dsqrt(r(2)**2+rho**2) ! distance of field point r from ring
      if(.not.lExpTest) then
        if(rho.lt.rE1) return
      end if
    end if
    iCSk=igetiCS(kEx)
    maxPar=tExp(kEx)%nPar
    nRing=tExp(kEx)%iE(6)
    if(nRing.lt.1) then
      if(lFull) then
        call DistPtCircle(r(1),r(3),0.0d0,0.0d0,rRing,rho,xN,yN,sN,ldum)
      else
        call DistPtArc(r(1),r(3),0.0d0,0.0d0,rRing*dcos(f1Ring),rRing*dsin(f1Ring),f2Ring-f1Ring,2.0d300,1_2,rho,xN,yN,sN,ldum)
      end if
      rho=dsqrt(r(2)**2+rho**2) ! distance of field point r from ring
      rlen=rRing*dabs(f2Ring-f1Ring) ! length of ring
      nRing=min(-nRing,max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nRing),(rlen.div.rho)),4)))
    end if
    DeAllocate(sumExp,asi,aco,stat=idum)
    Allocate(sumExp(1:10,tExp(kEx)%nPar),asi(0:maxDeg),aco(0:maxDeg),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Thin-Wire-Ring'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    sLoc=0.0d0
    sLoc(2,2)=1.0d0
    df=(f2Ring-f1Ring)/Dble(nRing)
    f=0.0d0
    sumExp=(0.0d0,0.0d0)
    xn=dcos(df)
    yn=dsin(df)
    delta=rRing*dsqrt((xn-1.0d0)**2+yn**2)
    do nR=1,nRing
      sLoc(1,1)=dcos(f+f1Ring)
      sLoc(3,1)=dsin(f+f1Ring)
      sLoc(1,0)=rRing*sLoc(1,1)
      sLoc(3,0)=rRing*sLoc(3,1)
      sLoc(1,3)=dcos(f+f1Ring+df)-sLoc(1,1)
      sLoc(3,3)=dsin(f+f1Ring+df)-sLoc(3,1)
      call Ortho3DSpace3(sLoc(1:3,3),sLoc(1:3,1),sLoc(1:3,2))
      call vGlob2Loc(r,sLoc,rl)
      if(lPL) then
        call Legenm((f+0.5d0*df)*aRing/Pi-1.0d0,0.0d0,aco,Int4(maxDeg),0_4)
      else
        phisin=dsin((f+0.5d0*df)*aRing)
        phicos=dcos((f+0.5d0*df)*aRing)
        call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      end if
      if(iHEk.ne.1) then
        call ThinWireElementE3D(rl,iD,delta,AEFld)
      end if
      if(iHEk.ne.0) then
        call ThinWireElementM3D(rl,iD,delta,AMFld)
      end if
      k=0
      iCSl=1_2
      if(iHEk.eq.1_2) iCSl=2_2
      if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
      if((minDeg.le.0).and.(iCSk.ne.iCSl)) then
        if(iHEk.ne.1) then
          k=k+1
          if(k.gt.maxPar) Exit
          call cvLoc2Glob(AEFld(1:3),sLoc,vh)
          sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)
          call cvLoc2Glob(AEFld(4:6),sLoc,vh)
          sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)
          if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+AEFld(9:10)
        end if
        if(iHEk.ne.0) then
          k=k+1
          if(k.gt.maxPar) Exit
          call cvLoc2Glob(AMFld(1:3),sLoc,vh)
          sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)
          call cvLoc2Glob(AMFld(4:6),sLoc,vh)
          sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)
          if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+AMFld(9:10)
        end if
      end if
      do m=max(1_2,minDeg),maxDeg
        if(iHEk.ne.1_2) then
          if(iCSk.ne.iCSl) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AEFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nRing)
            call cvLoc2Glob(AEFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nRing)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*AEFld(9:10)
          end if
          if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AEFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nRing)
            call cvLoc2Glob(AEFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nRing)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*AEFld(9:10)
        end if
        end if
        if(iHEk.ne.0_2) then
          if(iCSk.ne.iCSl) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AMFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nRing)
            call cvLoc2Glob(AMFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nRing)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*AMFld(9:10)
          end if
          if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AMFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nRing)
            call cvLoc2Glob(AMFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nRing)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*AMFld(9:10)
          end if
        end if
      end do
      if(k.gt.maxPar) Exit
      f=f+df
    end do
    nP=min(k,maxPar)
    k=0
    do kPa=tExp(kEx)%iOff+1,tExp(kEx)%iOff+min(tExp(kEx)%nPar,nP)
      k=k+1
      A(1:10,kPa)=A(1:10,kPa)+sumExp(1:10,k)
    end do
    DeAllocate(sumExp,asi,aco,stat=idum)
  end Subroutine ThinRing3D

  Recursive Subroutine ThinSpiral3D(kEx,r,iD,iHEk,minDeg,maxDeg,lPL,nP,A)
! compute 3d thin wire spiral
    Implicit none
    Complex(8), Allocatable :: sumExp(:,:)
    Real(8), Allocatable :: asi(:),aco(:),sLoca(:,:,:)
    Complex(8) vh(3),AEFld(10),AMFld(10),A(10,nParN)
    Real(8) r(3),rl(3),dr,df,dz,aSpiral,rE1,ra(3),Planel(3,0:3),d,dmin,sL,s,ds,vt(3),phisin,phicos,&
    & sLoc(1:3,0:3),delta
    Integer(4) kEx,nP,nSpiral,idum,i,m
    Integer(2) iD,iHEk,iCSk,iCSl,minDeg,maxDeg,maxPar,iCl(1),k,kPa,nR
    Logical lPL
    nP=0
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((tExp(kEx)%nPar.lt.1).or.(tExp(kEx)%iDom.ne.iD)) return
! set real parameters
    dr=tExp(kEx)%rE(1)
    df=tExp(kEx)%rE(2)
    dz=tExp(kEx)%rE(3)
    ra(1)=tExp(kEx)%xo
    ra(2)=tExp(kEx)%yo
    ra(3)=0.0d0
    d=Dist3DPtAxis(ra,tExp(kEx)%O(1:3),tExp(kEx)%e(1:3))
    sL=SpiralLength(d,dr,df,dz)
    aSpiral=tExp(kEx)%rE(4)*sL
    if(abs(aSpiral).lt.pSmall) aSpiral=1.0d0
    aSpiral=Pi/aSpiral
    rE1=tExp(kEx)%rE(5)
! find spiral length and distance of point r from spiral
    nSpiral=Nint(min(1000.0d0,max(3.0d0,(abs(tExp(kEx)%rE(2))*0.101d0))),4)
    Planel=0.0d0
    Planel(1,1)=1.0d0
    Planel(2,2)=1.0d0
    Planel(3,3)=1.0d0
    if(Allocated(sLoca)) DeAllocate(sLoca,stat=idum)
    Allocate(sLoca(3,nSpiral,1),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Thin-Spiral'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    else
      call Pt2Spiral(ra,tExp(kEx)%O,tExp(kEx)%e,dr,df,dz,nSpiral,Planel,sLoca(1:3,1:nSpiral,1))
      if(rE1.gt.pSmall) then
        dmin=pBig
        do i=1,nSpiral
          d=Dist3DPtPt(r,sLoca(1:3,i,1))
          if(d.lt.dmin) dmin=d
        end do
        if((.not.lExpTest).and.(dmin.lt.rE1)) return
      end if
    end if
    if(Allocated(sLoca)) DeAllocate(sLoca,stat=idum)
! set integer parameters
    iCSk=igetiCS(kEx)
    maxPar=tExp(kEx)%nPar
    nSpiral=tExp(kEx)%iE(6)
    if(nSpiral.lt.1) nSpiral=min(-nSpiral,max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nSpiral),(sL.div.dmin)),4)))
    if(nSpiral.lt.1) nSpiral=max(10*(maxDeg+1),nint(ExpSumAcc*min(dble(-nSpiral),(sL.div.dmin)),4))
    nSpiral=nSpiral+1
! auxiliary memory allocation
    DeAllocate(sLoca,sumExp,asi,aco,stat=idum)
    Allocate(sLoca(3,0:3,nSpiral),sumExp(1:10,tExp(kEx)%nPar),asi(0:maxDeg),aco(0:maxDeg), &
    &        stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'3D-Thin-Spiral'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
! get all local coordinate systems
    vt=ra-tExp(kEx)%O
    s=Dot_Product(tExp(kEx)%e,vt)
    vt=vt-s*tExp(kEx)%e
    call Unit3DV(vt)
    call Pt2Spiral(ra,tExp(kEx)%O,tExp(kEx)%e,dr,df,dz,nSpiral,Planel, &
    & sLoca(1:3,0,1:nSpiral),0,0,-257_2,0_2,iCl,sLoca(1:3,1,1:nSpiral),sLoca(1:3,2,1:nSpiral),sLoca(1:3,3,1:nSpiral),vt)
! loop over all thin wire expansions
    sumExp=(0.0d0,0.0d0)
    nSpiral=nSpiral-1
    ds=sL/Dble(nSpiral)
    s=0.0d0
    do nR=1,nSpiral
      sLoc(1:3,0:1)=sLoca(1:3,0:1,nR)
      sLoc(1:3,2)=-sLoca(1:3,3,nR)
      sLoc(1:3,3)=sLoca(1:3,0,nR+1)-sLoc(1:3,0)
      delta=r3Vec_Length(sLoc(1:3,3))
      call Ortho3DSpace3(sLoc(1:3,3),sLoc(1:3,1),sLoc(1:3,2))
      call vGlob2Loc(r,sLoc,rl)
      if(lPL) then
        call Legenm((s+0.5d0*ds)*aSpiral/Pi-1.0d0,0.0d0,aco,Int4(maxDeg),0_4)
      else
        phisin=dsin((s+0.5d0*ds)*aSpiral)
        phicos=dcos((s+0.5d0*ds)*aSpiral)
        call sicom(Int4(maxDeg),phisin,phicos,asi,aco)
      end if
      if(iHEk.ne.1) then
        call ThinWireElementE3D(rl,iD,delta,AEFld)
      end if
      if(iHEk.ne.0) then
        call ThinWireElementM3D(rl,iD,delta,AMFld)
      end if
      k=0
      iCSl=1_2
      if(iHEk.eq.1_2) iCSl=2_2
      if(lPL) iCSk=3_2-iCSl ! Legendre type uses aco
      if((minDeg.le.0).and.(iCSk.ne.iCSl)) then
        if(iHEk.ne.1) then
          k=k+1
          if(k.gt.maxPar) Exit
          call cvLoc2Glob(AEFld(1:3),sLoc,vh)
          sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)
          call cvLoc2Glob(AEFld(4:6),sLoc,vh)
          sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)
          if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+AEFld(9:10)
        end if
        if(iHEk.ne.0) then
          k=k+1
          if(k.gt.maxPar) Exit
          call cvLoc2Glob(AMFld(1:3),sLoc,vh)
          sumExp(1:3,k)=sumExp(1:3,k)+vh(1:3)
          call cvLoc2Glob(AMFld(4:6),sLoc,vh)
          sumExp(4:6,k)=sumExp(4:6,k)+vh(1:3)
          if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+AMFld(9:10)
        end if
      end if
      do m=max(1_2,minDeg),maxDeg
        if(iHEk.ne.1_2) then
          if(iCSk.ne.iCSl) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AEFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nSpiral)
            call cvLoc2Glob(AEFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nSpiral)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*AEFld(9:10)
          end if
          if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AEFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nSpiral)
            call cvLoc2Glob(AEFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nSpiral)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*AEFld(9:10)
          end if
        end if
        if(iHEk.ne.0_2) then
          if(iCSk.ne.iCSl) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AMFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+aco(m)*vh(1:3)/Dble(nSpiral)
            call cvLoc2Glob(AMFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+aco(m)*vh(1:3)/Dble(nSpiral)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+aco(m)*AMFld(9:10)
          end if
          if(iCSk.ne.modulo(iCSl,2_2)+1_2) then
            k=k+1
            if(k.gt.maxPar) Exit
            call cvLoc2Glob(AMFld(1:3),sLoc,vh)
            sumExp(1:3,k)=sumExp(1:3,k)+asi(m)*vh(1:3)/Dble(nSpiral)
            call cvLoc2Glob(AMFld(4:6),sLoc,vh)
            sumExp(4:6,k)=sumExp(4:6,k)+asi(m)*vh(1:3)/Dble(nSpiral)
            if(lMMPStat) sumExp(9:10,k)=sumExp(9:10,k)+asi(m)*AMFld(9:10)
          end if
        end if
      end do
      if(k.gt.maxPar) Exit
      s=s+ds
    end do
    nP=min(k,maxPar)
    k=0
    do kPa=tExp(kEx)%iOff+1,tExp(kEx)%iOff+min(tExp(kEx)%nPar,nP)
      k=k+1
      A(1:10,kPa)=A(1:10,kPa)+sumExp(1:10,k)
    end do
    DeAllocate(sumExp,asi,aco,stat=idum)
  end Subroutine ThinSpiral3D

  Subroutine setOrient(kEx,ixS0,iyS0,izS0)
! set orientation of expansions
    Implicit none
    Integer(2), optional:: ixS0,iyS0,izS0
    Integer(2) ixS,iyS,izS
    Integer(4) kEx
    if(Present(izS0)) then
      ixS0=0
      iyS0=0
      izS0=0
    end if
    if((kEx.lt.1).or.(kEx.gt.nExp)) return
    if((.not.lExpTest).and.(Present(izS0))) then
      ixS0=tExp(kEx)%iS/100_2
      iyS0=(tExp(kEx)%iS-(100_2*ixS0))/10_2
      izS0=tExp(kEx)%iS-(100_2*ixS0)-(10_2*iyS0)
      return
    end if
    ixS=0
    iyS=0
    izS=0
    if(lgcFld) then
      call set2DOrient(kEx,ixS,iyS)
    else
      call set3DOrient(kEx,ixS,iyS,izS)
    end if
    tExp(kEx)%iS=100_2*ixS+10_2*iyS+izS
    if(Present(izS0)) then
      ixS0=ixS
      iyS0=iyS
      izS0=izS
    end if
  end Subroutine setOrient

  Subroutine set2DOrient(kEx,ixS,iyS)
! set orientation of 2d expansions
    Implicit none
    Real(8) c,s,rE1
    Integer(2) ixS,iyS
    Integer(4) kEx
    if(tExp(kEx)%iTypE.eq.12_2) then ! 2D monopole for multilayer
      tExp(kEx)%Plane(1:3,1:3)=0.0d0 ! this expansion only works with a local coordinate system with unit vectors in x,y,z
      tExp(kEx)%Plane(1,1)=1.0d0
      tExp(kEx)%Plane(2,2)=1.0d0
      tExp(kEx)%Plane(3,3)=1.0d0
      if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) ixS=iyzSymm
      return
    end if
    if(tExp(kEx)%iTypE.gt.5_2) return ! 3D expansions
    if(tExp(kEx)%iTypE.eq.4_2) then ! Rayleigh
      tExp(kEx)%rE(1)=0.0d0
    end if
    rE1=tExp(kEx)%rE(1)
    c=dcosd(rE1)
    s=dsind(rE1)
    tExp(kEx)%Plane(1:3,1:3)=0.0d0
    tExp(kEx)%Plane(1,1)=c
    tExp(kEx)%Plane(2,1)=s
    tExp(kEx)%Plane(1,2)=-s
    tExp(kEx)%Plane(2,2)=c
    tExp(kEx)%Plane(3,3)=1.0d0
    tExp(kEx)%gc=gcFld
    tExp(kEx)%depend=1.0d100
    if((tExp(kEx)%iTypE.eq.1_2).or.(tExp(kEx)%iTypE.eq.2_2)) then ! 2D multipole symmetry consideration
      if(dabs(rE1).lt.pSmall)then
        tExp(kEx)%rE(1)=0.0d0
        if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.pSmall)) iyS=ixzSymm
        if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) ixS=iyzSymm
      end if
    end if
  end Subroutine set2DOrient

  Subroutine set3DOrient(kEx,ixS,iyS,izS)
! set orientation of 3d expansions and return symmetry numbers
    Implicit none
    Real(8) rE1,r1(3),rl(3),dz,rRing,rLine,zLine
    Integer(2) ixS,iyS,izS,nS
    Integer(4) kEx
    if(.not.lExpTest) then
      ixS=tExp(kEx)%iS/100_2
      iyS=(tExp(kEx)%iS-(100_2*ixS))/10_2
      izS=tExp(kEx)%iS-(100_2*ixS)-(10_2*iyS)
      return
    end if
    if(tExp(kEx)%iTypE.ge.12_2) then ! multilayer
      tExp(kEx)%Plane(1:3,1:3)=0.0d0 ! these expansions only work with a local coordinate system with unit vectors in x,y,z
      tExp(kEx)%Plane(1,1)=1.0d0
      tExp(kEx)%Plane(2,2)=1.0d0
      tExp(kEx)%Plane(3,3)=1.0d0
      if(tExp(kEx)%iTypE.eq.12_2) tExp(kEx)%Plane(3,0)=0.0D0
      if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) ixS=iyzSymm
      if((ixySymm.ne.0).and.(dabs(tExp(kEx)%Plane(3,0)).lt.pSmall)) izS=ixySymm
      return
    end if
    if((tExp(kEx)%iTypE.eq.1_2).or.(tExp(kEx)%iTypE.eq.2_2)) then ! 2D multipole symmetry consideration
      rE1=tExp(kEx)%rE(1)
      if(dabs(rE1).lt.pSmall) then
        if(dabs(tExp(kEx)%Plane(3,3)).gt.0.999999d0) then ! 2D multipole, standard orientation (propagation along z)
          tExp(kEx)%Plane(1:3,1:3)=0.0d0
          tExp(kEx)%Plane(1,1)=1.0d0
          tExp(kEx)%Plane(2,2)=1.0d0
          tExp(kEx)%Plane(3,3)=1.0d0
          if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.pSmall)) iyS=ixzSymm
          if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) ixS=iyzSymm
        else if(dabs(tExp(kEx)%Plane(1,3)).gt.0.999999d0) then ! 2D multipole (propagation along x)
          tExp(kEx)%Plane(1:3,1:3)=0.0d0
          tExp(kEx)%Plane(2,1)=1.0d0
          tExp(kEx)%Plane(3,2)=1.0d0
          tExp(kEx)%Plane(1,3)=1.0d0
          if((ixySymm.ne.0).and.(dabs(tExp(kEx)%Plane(3,0)).lt.pSmall)) iyS=ixySymm
          if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.pSmall)) ixS=ixzSymm
        else if(dabs(tExp(kEx)%Plane(2,3)).gt.0.999999d0) then ! 2D multipole (propagation along y)
          tExp(kEx)%Plane(1:3,1:3)=0.0d0
          tExp(kEx)%Plane(3,1)=1.0d0
          tExp(kEx)%Plane(1,2)=1.0d0
          tExp(kEx)%Plane(2,3)=1.0d0
          if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) iyS=iyzSymm
          if((ixySymm.ne.0).and.(dabs(tExp(kEx)%Plane(3,0)).lt.pSmall)) ixS=ixySymm
        end if
      end if
    else if(tExp(kEx)%iTypE.eq.4_2) then ! Rayleigh
      tExp(kEx)%Plane(1:3,1:3)=0.0d0
      tExp(kEx)%Plane(1,1)=1.0d0
      tExp(kEx)%Plane(2,2)=1.0d0
      tExp(kEx)%Plane(3,3)=1.0d0
    else if((tExp(kEx)%iTypE.eq.6_2).or.(tExp(kEx)%iTypE.eq.7_2)) then ! 3D multipole symmetry consideration
      nS=0_2
      if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) nS=nS+1_2
      if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.pSmall)) nS=nS+1_2
      if((ixySymm.ne.0).and.(dabs(tExp(kEx)%Plane(3,0)).lt.pSmall)) nS=nS+1_2
      if(nS.gt.0_2) then ! 1 or more symmetry plane
        if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) then
          if(dabs(tExp(kEx)%Plane(1,1)).gt.0.5d0) then
            ixS=iyzSymm
            tExp(kEx)%Plane(1:3,1)=0.0d0
            tExp(kEx)%Plane(1,1)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,1)
          else if(dabs(tExp(kEx)%Plane(1,2)).gt.0.5d0) then
            iyS=iyzSymm
            tExp(kEx)%Plane(1:3,2)=0.0d0
            tExp(kEx)%Plane(1,2)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,2)
          else
            izS=iyzSymm
            tExp(kEx)%Plane(1:3,3)=0.0d0
            tExp(kEx)%Plane(1,3)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,3)
          end if
        else if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.pSmall)) then
          if(dabs(tExp(kEx)%Plane(2,1)).gt.0.5d0) then
            ixS=ixzSymm
            tExp(kEx)%Plane(1:3,1)=0.0d0
            tExp(kEx)%Plane(2,1)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,1)
          else if(dabs(tExp(kEx)%Plane(2,2)).gt.0.5d0) then
            iyS=ixzSymm
            tExp(kEx)%Plane(1:3,2)=0.0d0
            tExp(kEx)%Plane(2,2)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,2)
          else
            izS=ixzSymm
            tExp(kEx)%Plane(1:3,3)=0.0d0
            tExp(kEx)%Plane(2,3)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,3)
          end if
        else
          if(dabs(tExp(kEx)%Plane(3,1)).gt.0.5d0) then
            ixS=ixySymm
            tExp(kEx)%Plane(1:3,1)=0.0d0
            tExp(kEx)%Plane(3,1)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,1)
          else if(dabs(tExp(kEx)%Plane(3,2)).gt.0.5d0) then
            iyS=ixySymm
            tExp(kEx)%Plane(1:3,2)=0.0d0
            tExp(kEx)%Plane(3,2)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,2)
          else
            izS=ixySymm
            tExp(kEx)%Plane(1:3,3)=0.0d0
            tExp(kEx)%Plane(3,3)=1.0d0
            call Ortho3DSpace2(tExp(kEx)%Plane,3)
          end if
        end if
      end if
      if(nS.gt.1_2) then ! 2 or more symmetry planes
        if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.pSmall)) then ! iyzSymm has already been considered
          if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.pSmall)) then ! iyzSymm (already OK) and ixzSymm
            if(ixS.ne.0_2) then
              if(dabs(tExp(kEx)%Plane(2,2)).gt.0.5d0) then
                iyS=ixzSymm
                if(nS.eq.3_2) izS=ixySymm 
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,1)=1.0d0
                tExp(kEx)%Plane(2,2)=1.0d0
                tExp(kEx)%Plane(3,3)=1.0d0
              else
                izS=ixzSymm
                if(nS.eq.3_2) iyS=ixySymm 
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,1)=1.0d0
                tExp(kEx)%Plane(2,3)=1.0d0
                tExp(kEx)%Plane(3,2)=-1.0d0
              end if
            else if(iyS.ne.0_2) then
              if(dabs(tExp(kEx)%Plane(2,3)).gt.0.5d0) then
                izS=ixzSymm
                if(nS.eq.3_2) ixS=ixySymm 
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,2)=1.0d0
                tExp(kEx)%Plane(2,3)=1.0d0
                tExp(kEx)%Plane(3,1)=1.0d0
              else
                ixS=ixzSymm
                if(nS.eq.3_2) izS=ixySymm 
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,2)=1.0d0
                tExp(kEx)%Plane(2,1)=1.0d0
                tExp(kEx)%Plane(3,3)=-1.0d0
              end if
            else
              if(dabs(tExp(kEx)%Plane(2,1)).gt.0.5d0) then
                ixS=ixzSymm
                if(nS.eq.3_2) iyS=ixySymm 
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,3)=1.0d0
                tExp(kEx)%Plane(2,1)=1.0d0
                tExp(kEx)%Plane(3,2)=1.0d0
              else
                iyS=ixzSymm
                if(nS.eq.3_2) ixS=ixySymm 
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,3)=1.0d0
                tExp(kEx)%Plane(2,2)=1.0d0
                tExp(kEx)%Plane(3,1)=-1.0d0
              end if
            end if
          else ! iyzSymm (already OK) and ixySymm
            if(ixS.ne.0_2) then
              if(dabs(tExp(kEx)%Plane(3,3)).gt.0.5d0) then
                izS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,1)=1.0d0
                tExp(kEx)%Plane(2,2)=1.0d0
                tExp(kEx)%Plane(3,3)=1.0d0
              else
                iyS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,1)=1.0d0
                tExp(kEx)%Plane(2,3)=1.0d0
                tExp(kEx)%Plane(3,2)=-1.0d0
              end if
            else if(iyS.ne.0_2) then
              if(dabs(tExp(kEx)%Plane(3,1)).gt.0.5d0) then
                ixS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,2)=1.0d0
                tExp(kEx)%Plane(2,3)=1.0d0
                tExp(kEx)%Plane(3,1)=1.0d0
              else
                izS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,2)=1.0d0
                tExp(kEx)%Plane(2,1)=1.0d0
                tExp(kEx)%Plane(3,3)=-1.0d0
              end if
            else
              if(dabs(tExp(kEx)%Plane(3,2)).gt.0.5d0) then
                iyS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,3)=1.0d0
                tExp(kEx)%Plane(2,1)=1.0d0
                tExp(kEx)%Plane(3,2)=1.0d0
              else
                ixS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,3)=1.0d0
                tExp(kEx)%Plane(2,2)=1.0d0
                tExp(kEx)%Plane(3,1)=-1.0d0
              end if
            end if
          end if
        else ! ixzSymm (already OK) and ixySymm
            if(ixS.ne.0_2) then
              if(dabs(tExp(kEx)%Plane(3,2)).gt.0.5d0) then
                iyS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,3)=1.0d0
                tExp(kEx)%Plane(2,1)=1.0d0
                tExp(kEx)%Plane(3,2)=1.0d0
              else
                izS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,2)=1.0d0
                tExp(kEx)%Plane(2,1)=1.0d0
                tExp(kEx)%Plane(3,3)=-1.0d0
              end if
            else if(iyS.ne.0_2) then
              if(dabs(tExp(kEx)%Plane(3,3)).gt.0.5d0) then
                izS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,1)=1.0d0
                tExp(kEx)%Plane(2,2)=1.0d0
                tExp(kEx)%Plane(3,3)=1.0d0
              else
                ixS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,3)=1.0d0
                tExp(kEx)%Plane(3,2)=1.0d0
                tExp(kEx)%Plane(2,1)=-1.0d0
              end if
            else
              if(dabs(tExp(kEx)%Plane(3,1)).gt.0.5d0) then
                ixS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,2)=1.0d0
                tExp(kEx)%Plane(2,3)=1.0d0
                tExp(kEx)%Plane(3,1)=1.0d0
              else
                iyS=ixySymm
                tExp(kEx)%Plane(1:3,1:3)=0.0d0
                tExp(kEx)%Plane(1,1)=1.0d0
                tExp(kEx)%Plane(3,3)=1.0d0
                tExp(kEx)%Plane(2,2)=-1.0d0
              end if
            end if
        end if
      end if
    else if(tExp(kEx)%iTypE.eq.8_2) then ! 3D ring symmetry consideration
      rRing=tExp(kEx)%rE(1)
      if(kEx.gt.0) then
        if((ixySymm.ne.0).and.(dabs(tExp(kEx)%Plane(3,0)).lt.1.0d-10*dabs(rRing))) then
          if((dabs(tExp(kEx)%Plane(3,1)).lt.1.0d-10).and.(dabs(tExp(kEx)%Plane(3,3)).lt.1.0d-10)) then
            tExp(kEx)%Plane(3,0:1)=0.0d0
            tExp(kEx)%Plane(3,3)=0.0d0
            call Ortho3DSpace(tExp(kEx)%Plane)
            izS=ixySymm
          end if
        end if
        if((ixzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(2,0)).lt.1.0d-10*dabs(rRing))) then
          if((dabs(tExp(kEx)%Plane(2,1)).lt.1.0d-10).and.(dabs(tExp(kEx)%Plane(2,3)).lt.1.0d-10)) then
            tExp(kEx)%Plane(2,0:1)=0.0d0
            tExp(kEx)%Plane(2,3)=0.0d0
            call Ortho3DSpace(tExp(kEx)%Plane)
            izS=ixzSymm
          end if
        end if
        if((iyzSymm.ne.0).and.(dabs(tExp(kEx)%Plane(1,0)).lt.1.0d-10*dabs(rRing))) then
          if((dabs(tExp(kEx)%Plane(1,1)).lt.1.0d-10).and.(dabs(tExp(kEx)%Plane(1,3)).lt.1.0d-10)) then
            tExp(kEx)%Plane(1,0:1)=0.0d0
            tExp(kEx)%Plane(1,3)=0.0d0
            call Ortho3DSpace(tExp(kEx)%Plane)
            izS=iyzSymm
          end if
        end if
      end if
    else if(tExp(kEx)%iTypE.eq.9_2) then ! 3D line symmetry consideration
      zLine=tExp(kEx)%rE(1)
      rLine=abs(tExp(kEx)%rE(2))
      r1(1:3)=tExp(kEx)%Plane(1:3,0)+zLine*tExp(kEx)%Plane(1:3,3)
      rl(1:3)=r1(1:3)+rLine*tExp(kEx)%Plane(1:3,3)
      dz=1.0d-10*dabs(rLine)
      if(kEx.gt.0) then
        if((ixySymm.ne.0).and.(dabs(r1(3)).lt.dz).and.(dabs(rl(3)).lt.dz)) then
          if((ixzSymm.ne.0).and.(dabs(r1(2)).lt.dz).and.(dabs(rl(2)).lt.dz)) then
            tExp(kEx)%Plane(2:3,0)=0.0d0
            tExp(kEx)%Plane(1:3,1:3)=0.0d0
            tExp(kEx)%Plane(2,1)=1.0d0
            tExp(kEx)%Plane(3,2)=1.0d0
            tExp(kEx)%Plane(1,3)=1.0d0
            ixS=ixzSymm
            iyS=ixySymm
          else if((iyzSymm.ne.0).and.(dabs(r1(1)).lt.dz).and.(dabs(rl(1)).lt.dz)) then
            tExp(kEx)%Plane(1,0)=0.0d0
            tExp(kEx)%Plane(3,0)=0.0d0
            tExp(kEx)%Plane(1:3,1:3)=0.0d0
            tExp(kEx)%Plane(3,1)=1.0d0
            tExp(kEx)%Plane(1,2)=1.0d0
            tExp(kEx)%Plane(2,3)=1.0d0
            ixS=ixySymm
            iyS=iyzSymm
          else
            tExp(kEx)%Plane(3,0)=0.0d0
            tExp(kEx)%Plane(1:3,1)=0.0d0
            tExp(kEx)%Plane(3,1)=1.0d0
            tExp(kEx)%Plane(3,3)=0.0d0
            call Unit3DV(tExp(kEx)%Plane(1:3,3))
            tExp(kEx)%Plane(1:3,2)=r3Vec_Prod(tExp(kEx)%Plane(1:3,3),tExp(kEx)%Plane(1:3,1))
            ixS=ixySymm
          end if
        else if((ixzSymm.ne.0).and.(dabs(r1(2)).lt.dz).and.(dabs(rl(2)).lt.dz)) then
          if((iyzSymm.ne.0).and.(dabs(r1(1)).lt.dz).and.(dabs(rl(1)).lt.dz)) then
            tExp(kEx)%Plane(1:2,0)=0.0d0
            tExp(kEx)%Plane(1:3,1:3)=0.0d0
            tExp(kEx)%Plane(1,1)=1.0d0
            tExp(kEx)%Plane(2,2)=1.0d0
            tExp(kEx)%Plane(3,3)=1.0d0
            ixS=iyzSymm
            iyS=ixzSymm
          else
            tExp(kEx)%Plane(2,0)=0.0d0
            tExp(kEx)%Plane(1:3,1)=0.0d0
            tExp(kEx)%Plane(2,1)=1.0d0
            tExp(kEx)%Plane(2,3)=0.0d0
            call Unit3DV(tExp(kEx)%Plane(1:3,3))
            tExp(kEx)%Plane(1:3,2)=r3Vec_Prod(tExp(kEx)%Plane(1:3,3),tExp(kEx)%Plane(1:3,1))
            ixS=ixzSymm
          end if
        else if((iyzSymm.ne.0).and.(dabs(r1(1)).lt.dz).and.(dabs(rl(1)).lt.dz)) then
          tExp(kEx)%Plane(1,0)=0.0d0
          tExp(kEx)%Plane(1:3,1)=0.0d0
          tExp(kEx)%Plane(1,1)=1.0d0
          tExp(kEx)%Plane(1,3)=0.0d0
          call Unit3DV(tExp(kEx)%Plane(1:3,3))
          tExp(kEx)%Plane(1:3,2)=r3Vec_Prod(tExp(kEx)%Plane(1:3,3),tExp(kEx)%Plane(1:3,1))
          ixS=iyzSymm
        end if
      end if
    end if
    tExp(kEx)%iS=100_2*ixS+10_2*iyS+izS
  end Subroutine set3DOrient

  Subroutine BlowAll(x,y,f)
    Implicit none
    Real(8) f,x,y
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    call BlowExpansion(0,x,y,f)
    call BlowBoundary(0,x,y,f)
    call MulBoundaryAmp(0,f)
  end Subroutine BlowAll

  Subroutine ReflAll(ix)
    Implicit none
    Integer(4) ix
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    call ReflBoundary(0,ix)
    call ReflExpansion(0,ix)
  end Subroutine ReflAll

  Subroutine MoveAll(dx,dy)
    Implicit none
    Real(8) dx,dy
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    call MoveBoundary(0,dx,dy)
    call MoveExpansion(0,dx,dy)
  end Subroutine MoveAll

  Subroutine RotateAll(xo,yo,fo,ldegree)
    Implicit none
    Real(8) xo,yo,fo
    Logical, intent(in) :: ldegree
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    call RotateBoundary(0,xo,yo,fo,ldegree)
    call RotateExpansion(0,xo,yo,fo,ldegree)
  end Subroutine RotateAll

  Subroutine BlowDomain(kD,x,y,f)
    Implicit none
    Real(8) f,x,y
    Integer(4) kD
    iDomBnd=Int2(kD)
    iDomExp=Int2(kD)
    iColBnd=0_2
    iConBnd=0_2
    iColExp=0_2
    iConExp=0_2
    call BlowExpansion(0,x,y,f)
    call BlowBoundary(0,x,y,f)
    iDomBnd=0_2
    iDomExp=0_2
  end Subroutine BlowDomain

  Subroutine ReflDomain(kD,ix)
    Implicit none
    Integer(4) kD,ix
    iDomBnd=Int2(kD)
    iDomExp=Int2(kD)
    iColBnd=0_2
    iConBnd=0_2
    iColExp=0_2
    iConExp=0_2
    call ReflBoundary(0,ix)
    call ReflExpansion(0,ix)
    iDomBnd=0_2
    iDomExp=0_2
  end Subroutine ReflDomain

  Subroutine MoveDomain(kD,dx,dy)
    Implicit none
    Real(8) dx,dy
    Integer(4) kD
    iDomBnd=Int2(kD)
    iDomExp=Int2(kD)
    iColBnd=0_2
    iConBnd=0_2
    iColExp=0_2
    iConExp=0_2
    call MoveBoundary(0,dx,dy)
    call MoveExpansion(0,dx,dy)
    iDomBnd=0_2
    iDomExp=0_2
  end Subroutine MoveDomain

  Subroutine RotateDomain(kD,xo,yo,fo,ldegree)
    Implicit none
    Real(8) xo,yo,fo
    Integer(4) kD
    Logical, intent(in) :: ldegree
    iDomBnd=Int2(kD)
    iDomExp=Int2(kD)
    iColBnd=0_2
    iConBnd=0_2
    iColExp=0_2
    iConExp=0_2
    call RotateBoundary(0,xo,yo,fo,ldegree)
    call RotateExpansion(0,xo,yo,fo,ldegree)
    iDomBnd=0_2
    iDomExp=0_2
  end Subroutine RotateDomain

  Subroutine BlowColor(kD,x,y,f)
    Implicit none
    Real(8) f,x,y
    Integer(4) kD
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=Int2(kD)
    iConBnd=0_2
    iColExp=Int2(kD)
    iConExp=0_2
    call BlowExpansion(0,x,y,f)
    call BlowBoundary(0,x,y,f)
    iColBnd=0_2
    iColExp=0_2
  end Subroutine BlowColor

  Subroutine ReflColor(kD,ix)
    Implicit none
    Integer(4) kD,ix
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=Int2(kD)
    iConBnd=0_2
    iColExp=Int2(kD)
    iConExp=0_2
    call ReflBoundary(0,ix)
    call ReflExpansion(0,ix)
    iColBnd=0_2
    iColExp=0_2
  end Subroutine ReflColor

  Subroutine MoveColor(kD,dx,dy)
    Implicit none
    Real(8) dx,dy
    Integer(4) kD
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=Int2(kD)
    iConBnd=0_2
    iColExp=Int2(kD)
    iConExp=0_2
    call MoveBoundary(0,dx,dy)
    call MoveExpansion(0,dx,dy)
    iColBnd=0_2
    iColExp=0_2
  end Subroutine MoveColor

  Subroutine RotateColor(kD,xo,yo,fo,ldegree)
    Implicit none
    Real(8) xo,yo,fo
    Integer(4) kD
    Logical, intent(in) :: ldegree
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=Int2(kD)
    iConBnd=0_2
    iColExp=Int2(kD)
    iConExp=0_2
    call RotateBoundary(0,xo,yo,fo,ldegree)
    call RotateExpansion(0,xo,yo,fo,ldegree)
    iColBnd=0_2
    iColExp=0_2
  end Subroutine RotateColor

  Subroutine BlowConnection(kD,x,y,f)
    Implicit none
    Real(8) f,x,y
    Integer(4) kD
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=0_2
    iConBnd=Int2(kD)
    iColExp=0_2
    iConExp=Int2(kD)
    call BlowExpansion(0,x,y,f)
    call BlowBoundary(0,x,y,f)
    iConBnd=0_2
    iConExp=0_2
  end Subroutine BlowConnection

  Subroutine ReflConnection(kD,ix)
    Implicit none
    Integer(4) kD,ix
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=0_2
    iConBnd=Int2(kD)
    iColExp=0_2
    iConExp=Int2(kD)
    call ReflBoundary(0,ix)
    call ReflExpansion(0,ix)
    iConBnd=0_2
    iConExp=0_2
  end Subroutine ReflConnection

  Subroutine MoveConnection(kD,dx,dy)
    Implicit none
    Real(8) dx,dy
    Integer(4) kD
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=0_2
    iConBnd=Int2(kD)
    iColExp=0_2
    iConExp=Int2(kD)
    call MoveBoundary(0,dx,dy)
    call MoveExpansion(0,dx,dy)
    iConBnd=0_2
    iConExp=0_2
  end Subroutine MoveConnection

  Subroutine RotateConnection(kD,xo,yo,fo,ldegree)
    Implicit none
    Real(8) xo,yo,fo
    Integer(4) kD
    Logical, intent(in) :: ldegree
    iDomBnd=0_2
    iDomExp=0_2
    iColBnd=0_2
    iConBnd=Int2(kD)
    iColExp=0_2
    iConExp=Int2(kD)
    call RotateBoundary(0,xo,yo,fo,ldegree)
    call RotateExpansion(0,xo,yo,fo,ldegree)
    iConBnd=0_2
    iConExp=0_2
  end Subroutine RotateConnection

  Subroutine BlowExpansion(kEx,x,y,f,kEx2)
    Implicit none
    Real(8) f,x,y
    Integer(4) kEx,k1,k2,kE
    Integer(4), Optional:: kEx2
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      tExp(kE)%Plane(1,0)=x+(tExp(kE)%Plane(1,0)-x)*f
      tExp(kE)%Plane(2,0)=y+(tExp(kE)%Plane(2,0)-y)*f
      tExp(kE)%Plane(3,0)=(tExp(kE)%Plane(3,0))*f
      tExp(kE)%O(1)=x+(tExp(kE)%O(1)-x)*f
      tExp(kE)%O(2)=y+(tExp(kE)%O(2)-y)*f
      tExp(kE)%O(3)=(tExp(kE)%O(3))*f
      tExp(kE)%dmin=(tExp(kE)%dmin)*f
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kE)%xo=tExp(kE)%Plane(1,0)
        tExp(kE)%yo=tExp(kE)%Plane(2,0)
      else
        tExp(kE)%xo=tExp(kE)%xo*f
        tExp(kE)%yo=tExp(kE)%yo*f
      end if
      select case(tExp(kE)%iTypE)
      case(0_2) ! connection
        tExp(kE)%rE(2:4)=tExp(kE)%rE(2:4)*f
      case(1_2) ! 2D Hankel
        tExp(kE)%rE(2:4)=tExp(kE)%rE(2:4)*f
      case(2_2) ! 2D Bessel
        tExp(kE)%rE(2:4)=tExp(kE)%rE(2:4)*f
      case(4_2) ! Rayleigh
        if(tExp(kE)%rE(2).gt.0.0d0) tExp(kE)%rE(2)=tExp(kE)%rE(2)*f
      case(5_2) ! Harmonic
        if(tExp(kE)%rE(2).gt.0.0d0) tExp(kE)%rE(2)=tExp(kE)%rE(2)*f
        tExp(kE)%rE(3:4)=tExp(kE)%rE(3:4)*f
      case(6_2) ! 3D Hankel
        tExp(kE)%rE(1:4)=tExp(kE)%rE(1:4)*f
      case(7_2) ! 3D Bessel
        tExp(kE)%rE(1:4)=tExp(kE)%rE(1:4)*f
      case(8_2) ! Ring
        tExp(kE)%rE(1)=tExp(kE)%rE(1)*f
        tExp(kE)%rE(5)=tExp(kE)%rE(5)*f
      case(9_2) ! Line
        tExp(kE)%rE(1)=tExp(kE)%rE(1)*f
        tExp(kE)%rE(2)=tExp(kE)%rE(2)*f
        tExp(kE)%rE(4)=tExp(kE)%rE(4)*f
      case(10_2) ! Spiral
        tExp(kE)%rE(1)=tExp(kE)%rE(1)*f
        tExp(kE)%rE(3)=tExp(kE)%rE(3)*f
        tExp(kE)%rE(5)=tExp(kE)%rE(5)*f
      case(11_2) ! Gauss
        tExp(kE)%rE(1)=tExp(kE)%rE(1)*f
      end select
    end do
  end Subroutine BlowExpansion

  Subroutine ReflExpansion(kEx,ix,kEx2)
    Implicit none
    Integer(4) kEx,k1,k2,kE,ix
    Integer(4), Optional:: kEx2
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      if(ix.eq.1) then
        tExp(kE)%Plane(1,0:3)=-tExp(kE)%Plane(1,0:3)
      else
        tExp(kE)%Plane(2,0:3)=-tExp(kE)%Plane(2,0:3)
      end if
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kE)%xo=tExp(kE)%Plane(1,0)
        tExp(kE)%yo=tExp(kE)%Plane(2,0)
      else
        if(ix.eq.1) then
          tExp(kE)%xo=-tExp(kE)%xo
        else
          tExp(kE)%yo=-tExp(kE)%yo
        end if
      end if
    end do
  end Subroutine ReflExpansion

  Subroutine MoveExpansion(kEx,dx,dy,kEx2,dz,nxP,nyP,nzP)
    Implicit none
    Real(8) dx,dy
    Real(8), Optional:: dz
    Integer(4) kEx,k1,k2,kE
    Integer(4), Optional:: kEx2,nxP,nyP,nzP
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      tExp(kE)%Plane(1,0)=tExp(kE)%Plane(1,0)+dx
      tExp(kE)%Plane(2,0)=tExp(kE)%Plane(2,0)+dy
      if(Present(dz)) tExp(kE)%Plane(3,0)=tExp(kE)%Plane(3,0)+dz
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kE)%xo=tExp(kE)%Plane(1,0)
        tExp(kE)%yo=tExp(kE)%Plane(2,0)
      else
        tExp(kE)%xo=tExp(kE)%xo+dx
        tExp(kE)%yo=tExp(kE)%yo+dy
      end if
      if(Present(nzP)) then ! adapt parameters of the expansion
        do kPar=tExp(kE)%iOff+1,tExp(kE)%iOff+tExp(kE)%nPar
          if(nxP.ne.0) ParExp(1:nRHS,kPar)=ParExp(1:nRHS,kPar)*cdexp((0.0d0,1.0d0)*cxPeriod*Dble(nxP)*xPeriod)
          if(nyP.ne.0) ParExp(1:nRHS,kPar)=ParExp(1:nRHS,kPar)*cdexp((0.0d0,1.0d0)*cyPeriod*Dble(nyP) &
          & *r3Vec_Length(yPeriodVector))
          if(nzP.ne.0) ParExp(1:nRHS,kPar)=ParExp(1:nRHS,kPar)*cdexp((0.0d0,1.0d0)*czPeriod*Dble(nzP) &
          & *r3Vec_Length(zPeriodVector))
        end do
      end if
    end do
  end Subroutine MoveExpansion

  Subroutine Move3DE(kEx,x,y,z,kEx2)
! move 3D expansions with vector (x,y,z)
    Implicit none
    Real(8) x,y,z
    Integer(4) kEx,k1,k2,i
    Integer(4), Optional:: kEx2
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    do i=k1,k2
      tExp(i)%plane(1,0)=tExp(i)%plane(1,0)+x
      tExp(i)%plane(2,0)=tExp(i)%plane(2,0)+y
      tExp(i)%plane(3,0)=tExp(i)%plane(3,0)+z
    end do
  end Subroutine Move3DE

  Subroutine RotateExpansion(kEx,xo,yo,fo,ldegree,kEx2)
    Implicit none
    Real(8) xo,yo,fo,f,x,y,r
    Integer(4) kEx,k1,k2,kE
    Logical, intent(in) :: ldegree
    Integer(4), Optional:: kEx2
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      x=tExp(kE)%Plane(1,0)-xo
      y=tExp(kE)%Plane(2,0)-yo
      call Cart2Pol(x,y,r,f)
      if(ldegree) then
        f=f+Pi*fo/180.0d0
        tExp(kE)%rE(1)=tExp(kE)%rE(1)+fo
      else
        f=f+fo
        tExp(kE)%rE(1)=tExp(kE)%rE(1)+fo*180.0d0/Pi
      end if
      call Pol2Cart(r,f,x,y)
      tExp(kE)%Plane(1,0)=xo+x
      tExp(kE)%Plane(2,0)=yo+y
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kE)%xo=tExp(kE)%Plane(1,0)
        tExp(kE)%yo=tExp(kE)%Plane(2,0)
      else
        x=tExp(kE)%xo-xo
        y=tExp(kE)%yo-yo
        call Cart2Pol(x,y,r,f)
        if(ldegree) then
          f=f+Pi*fo/180.0d0
        else
          f=f+fo
        end if
        call Pol2Cart(r,f,x,y)
        tExp(kE)%xo=xo+x
        tExp(kE)%yo=yo+y
      end if
    end do
  end Subroutine RotateExpansion

  Subroutine Rotate3DE(kEx,x0,y0,z0,ldegree,kEx2)
! rotate expansions around x axis with angle x0, y axis with angle y0, z axis wit angle z0
    Implicit none
    Real(8) x0,y0,z0,x,y,z
    Real(8) spa(3,0:3)
    Integer(4) kEx,k1,k2,i
    Logical, intent(in) :: ldegree
    Integer(4), Optional:: kEx2
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    x=x0
    y=y0
    z=z0
    if(.not.ldegree) then
      x=x0*180.0d0/Pi
      y=y0*180.0d0/Pi
      z=z0*180.0d0/Pi
    end if
    do i=k1,k2
      spa=tExp(i)%plane
      spa=Rot3DSpaceX(spa,x)
      spa=Rot3DSpaceY(spa,y)
      spa=Rot3DSpaceZ(spa,z)
      tExp(i)%plane=spa
    end do
  end Subroutine Rotate3DE

  Subroutine SetExpansion(kEx,xo,yo,fo,ldegree,kEx2)
    Implicit none
    Real(8) xo,yo
    Real(8), Optional:: fo
    Integer(4) kEx,k1,k2,kE
    Logical, Optional, intent(in) :: ldegree
    Integer(4), Optional:: kEx2
    if(Present(kEx2)) then
      call GetRange(kEx,nExp,k1,k2,kEx2)
    else
      call GetRange(kEx,nExp,k1,k2)
    end if
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      if(Present(ldegree)) then
        if(ldegree) then
          tExp(kE)%rE(1)=fo
        else
          tExp(kE)%rE(1)=fo*180.0d0/Pi
        end if
        call setOrient(kE)
      end if
      tExp(kE)%Plane(1,0)=xo
      tExp(kE)%Plane(2,0)=yo
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kE)%xo=tExp(kE)%Plane(1,0)
        tExp(kE)%yo=tExp(kE)%Plane(2,0)
      end if
    end do
  end Subroutine SetExpansion

  Subroutine GetExpHandle(x,y,dmin,kEx,kE,iType)
    Implicit none
    Real(8) x,y,d,dmin
    Integer(4) kEx,k1,k2,kE,iType,lout
    kE=1
    iType=0
    if(kEx.lt.1) then
      if(kEx.eq.0) then
        k1=1
        k2=nExp
      else
        k1=1
        k2=Min(nExp,-kEx)
      end if
    else
      k1=Min(nExp,kEx)
      k2=k1
    end if
    do kE=k1,k2
      if(LSkipExp(kE)) Cycle
      call DistPtPt(x,y,tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),d)
      iType=1
      if(d.lt.dmin) Exit
      iType=0
    end do
    kE=Min(kE,k2)
    if(iType.ne.0) then
      call IntToStr(Int4(kE),0,0,SpaceText,lout)
		  call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(Int4(nExp),0,0,SpaceText,lout)
		  call OutTxt('m1',SpaceText(1:lout))
    else
		  call OutTxt('n1',' 'C)
		  call OutTxt('m1',' 'C)
    end if
		call OutTxt('t2','handle type'C)
    call IntToStr(Int4(iType),0,0,SpaceText,lout)
		call OutTxt('n2',SpaceText(1:lout))
  end Subroutine GetExpHandle

  Subroutine updateExp(x,y,kE,iType)
    Implicit none
    Real(8) x,y,dx,dy
    Integer(4) kE,iType
    if(iType.eq.1) then
      dx=x-tExp(kE)%Plane(1,0)
      dy=y-tExp(kE)%Plane(2,0)
      iDomExp=0_2
      iColExp=0_2
      iConExp=0_2
      call MoveExpansion(kE,dx,dy)
    end if
  end Subroutine updateExp

  Subroutine GenExpGMSH(jE,iCl,iCn,iDm,fmini)
! generate expansions with data of expansion jE along the domain jDM using GMSH
    Implicit none
    Integer(4), Intent(in) :: jE
    Integer(2), Intent(in) :: iCl,iCn,iDm
    Integer(2) iC
    Integer(4) iFile,idum,lFO,nE,nP,nnE,nnP,n1,n2,n3,m1,m2,m3,iB,i,lF
    Real(8) x,y,fmini
    Logical lOrdered,ldum
    Character(256) FileString
    iC=nint(fmini,2) ! color of resulting expansions
    iFile=1
    GeoFileName='mmpexp.geo'
    lFO=GetSLength(GeoFileName)
		iB=jE+1
! write geometry file for GMSH
    call GMSHwriteGeoFile(iFile,GeoFileName,iGMSHbnd1,iGMSHbnd2,iDm,iCl)
! run GMS for creating the *.MSH file
    call FindGMSH(FileString,lF,ldum)
    if(.not.ldum) then
      if(l4) write(*,*) 'GMSH error: gmsh.exe not found!'
      return
    end if
    idum=1
   ! idum=RunQQ(FileString(1:lF-4),'mmpexp.geo -1 -2')
    idum=RunQQ(FileString(1:lF-4),GeoFileName(1:lFO)//' -1 -2')
   ! idum=RunQQ(FileString(1:lF-4),) ! FileString without ending ".exe"
    if(idum.ne.0) then
      if(l4) write(*,*) 'GMSH error: gmsh.exe returned error code:',idum
      if(l4) write(*,*) 'Command line was: ',FileString(1:lF-4),' ',GeoFileName(1:lFO)//' -1 -2'
      return
    end if
! read *.MSH file
    MshFileName(1:lFO-3)=GeoFileName(1:lFO-3)
    MshFileName(lFO-2:lFO+1)='msh'//char(0)
    call GMSHcountPoints(iFile,MshFileName,lFO,nP,n1,n2,n3,m1,m2,m3,lOrdered)
    if(.not.lOrdered) then
      if(l4) write(*,*) 'GMSH error:: *.MSH file is not ordered or cannot be read!'
      if(l4) write(*,*) 'File name:',MshFileName(1:lFO)
      return
    end if
    nE=n1+n2+n3
! analyze mesh file and generate expansions on the mesh near the boundary
    Allocate(ieGMSH(nE),ipGMSH(nP))
    m2=m2+1
    if(m3.ge.m2) then
      call GMSHgetNearElements(iFile,MshFileName,lFO,n1,n2,n3,ieGMSH,nE,ipGMSH,nP,nnE,nnP)
      if(l4.and.l5) write(*,*) 'Elements / points near boundary found:',nnE,nnP
      if((nnE.gt.0).and.(nnP.gt.0)) then
        Allocate(rpGMSH(3,nnP))
        call GMSHreadNearPoints(iFile,MshFileName,lFO,ipGMSH,nP,rpGMSH,nnP)
        do i=1,nnP
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				  if(.NOT.ldum) return
          x=rpGMSH(1,i)
          y=rpGMSH(2,i)
          call SetExp(iB,iDm,iCn,iC,x,y)
          call DrawX(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),1.0d-2)
        end do
      end if
    end if
    if(allocated(rpGMSH)) DeAllocate(rpGMSH)
    DeAllocate(ieGMSH,ipGMSH)
    if(nnP.lt.1) return
! adapt the generated expansions
    iColExp=iC ! consider only expansions with the color of the generated expansions
    iModExpMin=1
    iModExpMax=nExp-nExc ! omit excitations
    iModExpLoop=1 ! perorm adapt loop only once
    call adaptExp2D0f(.false.) ! delete expansions near fictitous boundaries
    iModExpMin=1
    iModExpMax=nExp-nExc
    iModExpWFA=1 ! if this would be zero, the following would be skipped
    call adaptExp2D3(.false.) ! move expansions outside
    iModExpMin=1
    iModExpMax=nExp-nExc
    call adaptExp2D0(.false.) ! delete expansions that are still inside
    iColExp=0 ! back to all colors
    iModExpMin=1
    iModExpMax=nExp-nExc
  End Subroutine GenExpGMSH

  Subroutine GenExp1(kB,n,jE,jCl,jCn,jDm,fmin,fmax,fact)
! generate n expansions with data of expansion jE along the boundary kB, set color iCl, connection flag iCn
    Implicit none
    Integer(4), Intent(in) :: jE,kB,n
    Integer(2), Intent(in) :: jCl,jCn,jDm
    Real(8), Intent(in) :: fmin,fmax,fact
    Real(8) s1,s2,xn,yn,vt1,vt2,ds,s,rmin,am,xdum,ydum,a,rkm,x,y,w
    Integer(2) iCl,iCn,iDL,iDR,iDL1,iDR1,j
    Integer(4) k1,k2,m,iB,k,idum
    Logical(4) ldum
    if(n.lt.1) return
    if((jE.lt.1).or.(jE.gt.nExp)) return
    if((kB.ge.1).and.(kB.le.nBnd)) then
      k1=kB
      k2=k1
    else if((kB.lt.0).and.(-kB.lt.nBnd)) then
      k1=1
      k2=-kB
    else
      k1=1
      k2=nBnd
    end if
    m=2*n+1
		iB=jE+1
    if(abs(iWf_genExp).gt.nFunA) iWf_genExp=0_4
    do k=k1,k2
      if(jDm.gt.0) then
        if((jDm.ne.tBnd(k)%iLDom).and.(jDm.ne.tBnd(k)%iRDom)) Cycle
      else if(jDm.lt.0) then
        if((-jDm.lt.tBnd(k)%iLDom).and.(-jDm.lt.tBnd(k)%iRDom)) Cycle
      end if
      iCl=jCl
      if(iCl.eq.-1) iCl=tBnd(k)%iCol
      iCn=jCn
      if(iCn.lt.-32000) iCn=tBnd(k)%iConn
      s1=tBnd(k)%Start
      s2=s1+tBnd(k)%sLength
      call GetBndPt(k,0.5d0*(s1+s2),Xn,Yn,vt1,vt2,iDL,iDR,idum)
      if(jDm.ne.0) then
        if(jDm.ne.iDL) iDL=0
        if(jDm.ne.iDR) iDR=0
      end if
      ds=(s2-s1)/(Dble(m-1)+0.002d0)
      s=s1-0.999d0*ds
      Rmin=pBig
      do j=1,m
        s=s+ds
        call GetBndPt(k,s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
        if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
        Am=Datan2(vt1,-vt2)
        call GetBndPt(k,s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
        if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
        A=Datan2(vt1,-vt2)
        Rkm=min(1.0d100,(0.0005d0*ds).div.(dabs(Am-A)))
        If(Rkm.Lt.Rmin) Rmin=Rkm
      end do
      Rmin=0.5d0*Rmin
      if((abs(fmin).gt.pSmall).and.(Rmin.lt.(fmin*ds))) Rmin=fmin*ds
      if((abs(fmax).gt.pSmall).and.(Rmin.gt.(fmax*ds))) Rmin=fmax*ds
      Rmin=fact*Rmin
      s=s1-0.9999d0*ds
      ds=2.0d0*ds
      Do j=1,n
        s=s+ds
        call GetBndPt(k,s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
        A=Datan2(vt1,-vt2)
        if(iDL.gt.0_2) then
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				  if(.NOT.ldum) return
          x=Xn-Rmin*DCos(A)
          y=Yn-Rmin*DSin(A)
          call adaptExpD(s,Rmin,w)
          x=Xn-w*Rmin*DCos(A)
          y=Yn-w*Rmin*DSin(A)
          call SetExp(iB,iDL,iCn,iCl,x,y)
          call DrawLine(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),Xn,Yn)
        end if
        if(iDR.gt.0_2) then
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				  if(.NOT.ldum) return
          x=Xn+Rmin*DCos(A)
          y=Yn+Rmin*DSin(A)
          call adaptExpD(s,Rmin,w)
          x=Xn+w*Rmin*DCos(A)
          y=Yn+w*Rmin*DSin(A)
          call SetExp(iB,iDR,iCn,iCl,x,y)
          call DrawLine(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),Xn,Yn)
        end if
      End Do
    End Do
  End Subroutine GenExp1

  Subroutine GenExp2(kB,n,jE,jCl,jCn,jDm,fmin,fmax,r_thr,r_first,r_last,fr_inn,fr_out,d_l,d_r,a_inn,a_out,nS)
! Moreno's generate multipoles with n spline points, along the boundary kB, set color iCl, connection flag iCn
    Implicit none
    Integer(4), Intent(in) :: jE,kB,n,nS
    Integer(2), Intent(in) :: jCl,jCn,jDm
    Real(8), Intent(in) :: fmin,fmax,r_thr,r_first(2),r_last(2),fr_inn,fr_out,d_l,d_r,a_inn,a_out
    Real(8) s1,s2,xn,yn,vt1,vt2
    Integer(2) iCl,iCn,iDL,iDR
    Integer(4) idum,k1,k2,k,num_points
    Logical(4) lclosed
    if((jE.lt.1).or.(jE.gt.nExp)) return
    if((kB.ge.1).and.(kB.le.nBnd)) then
      k1=kB
      k2=k1
    else if((kB.lt.0).and.(-kB.lt.nBnd)) then
      k1=1
      k2=-kB
    else
      k1=1
      k2=nBnd
    end if
    do k=k1,k2
      if(jDm.ne.0) then
        if((jDm.ne.tBnd(k)%iLDom).and.(jDm.ne.tBnd(k)%iRDom)) Cycle
      end if
! parameters
      lclosed=.false.
      if(iiabs(tBnd(k)%iTypB).eq.1_2) lclosed=.true.
      iCl=jCl
      if(iCl.eq.-1) iCl=tBnd(k)%iCol
      iCn=jCn
      if(iCn.lt.-32000) iCn=tBnd(k)%iConn
      s1=tBnd(k)%Start
      s2=s1+tBnd(k)%sLength
      call GetBndPt(k,0.5d0*(s1+s2),Xn,Yn,vt1,vt2,iDL,iDR,idum)
      if(jDm.ne.0) then
        if(jDm.ne.iDL) iDL=0
        if(jDm.ne.iDR) iDR=0
      end if
! generate points along the boundary
      num_points=nS
      if(abs(num_points).lt.2) then
        num_points=abs(tBnd(k)%nSpline)
      else if(num_points.lt.-1) then
        num_points=min(abs(tBnd(k)%nSpline),-n)
      end if
      if(num_points.lt.2) num_points=1000
! generate multipoles
      call GenExpM(k,lclosed,iDL,iDR,iCn,iCl,jE,r_first,r_last,fr_inn,fr_out,d_l,d_r,a_inn,a_out, &
      &  r_thr,fmin,fmax,num_points)
    end do
  end Subroutine GenExp2

  Subroutine GenExpC(kB,n0,jE,jCl,jCn,jDm,fmini,fmaxi,facti,r_first,r_last,fr_inn,fr_out,d_l,d_r,a_inn,a_out,n_points)
! generate expansions with data of expansion jE along the boundary kB, set color iCl, connection flag iCn
    Implicit none
    Integer(4), Intent(in) :: jE,kB,n0,n_points
    Integer(2), Intent(in) :: jCl,jCn,jDm
    Real(8), Intent(in) :: fmini,fmaxi,facti,r_first(2),r_last(2),fr_inn,fr_out,d_l,d_r,a_inn,a_out
    Real(8), Allocatable:: s(:),dL(:),dR(:),x(:),y(:),xL(:),yL(:),xR(:),yR(:),vn(:,:)
    Real(8) s1,s2,ds,d0L,d0R,ddL,ddR,vt1,vt2,aL,aR,da,de
    Integer(2) iCl,iCn,iDL,iDR,ic,ic0
    Integer(4) n,k1,k2,k,iB,i,idum,j,la,le
    Logical(4) lReduce,lAllBnd,ldum,lclosed
    n=max(10,abs(n_points)) ! number of points along the boundary
    Deallocate(s,dL,dR,stat=idum)
    Allocate(s(n+1),dL(n+1),dR(n+1),x(n+1),y(n+1),xL(n+1),yL(n+1),xR(n+1),yR(n+1),vn(2,n+1),stat=idum)
    if(idum.ne.0) then
      write(*,*) 'Generate expansion cannot allocate memory!'
      Deallocate(s,dL,dR,x,y,xL,yL,xR,yR,vn,stat=idum)
      return
    end if
    lReduce=.false.
    if((facti.gt.pSmall).and.(facti.lt.1.0d0)) lReduce=.true.
    lAllBnd=.false.
    if(abs(n0).gt.1) lAllBnd=.true.
    ic0=SetColor(1_2)
    aL=a_inn*Pi/180.0d0
    aR=a_out*Pi/180.0d0
    da=d_L
    if(da.lt.0.0d0) da=max(r_first(1),r_first(2))
    de=d_R
    if(de.lt.0.0d0) de=max(r_last(1),r_last(2))
    if((kB.ge.1).and.(kB.le.nBnd)) then
      k1=kB
      k2=k1
    else if((kB.lt.0).and.(-kB.lt.nBnd)) then
      k1=1
      k2=-kB
    else
      k1=1
      k2=nBnd
    end if
    iB=jE+1
    do k=k1,k2
      if(jDm.gt.0) then
        if((jDm.ne.tBnd(k)%iLDom).and.(jDm.ne.tBnd(k)%iRDom)) Cycle
      else if(jDm.lt.0) then
        if((-jDm.lt.tBnd(k)%iLDom).and.(-jDm.lt.tBnd(k)%iRDom)) Cycle
      end if
      iCl=jCl
      if(iCl.eq.-1) iCl=tBnd(k)%iCol
      iCn=jCn
      if(iCn.lt.-32000) iCn=tBnd(k)%iConn
      s1=tBnd(k)%Start
      s2=s1+tBnd(k)%sLength
      ds=0.9999999999*(s2-s1)/Dble(n-1)
      call GetBndPt(k,ds,x(2),y(2),vt1,vt2,iDL,iDR,idum) ! dummy point on boundary -> domain numbers
      if(jDm.gt.0) then
        if(jDm.ne.iDL) iDL=0
        if(jDm.ne.iDR) iDR=0
      else if(jDm.lt.0) then
        if(-jDm.gt.iDL) iDL=0
        if(-jDm.gt.iDR) iDR=0
      end if
      lclosed=.false.
      if(iiabs(tBnd(k)%iTypB).eq.1_2) lclosed=.true.
      s(1)=s1 ! save parameter along the boundary in the array s
      la=1
      do i=2,n
        s(i)=s(i-1)+ds
        if(s(i).lt.da) la=i
      end do
      le=n
      do i=n-1,1,-1
        if((s(n)-s(i)).lt.de) le=i
      end do
      dL(1)=r_first(1) ! generate distances dL and dR along the boundary
      dR(1)=r_first(2)
      ddL=ds*tan(aL)
      ddR=ds*tan(aR)
      do i=2,n
        dL(i)=dL(i-1)+ddL
        dR(i)=dR(i-1)+ddR
      end do
      d0L=r_last(1)
      d0R=r_last(2)
      do i=n,1,-1
        dL(i)=max(min(dL(i),d0L,fmaxi),fmini)
        d0L=d0L+ddL
        dR(i)=max(min(dR(i),d0R,fmaxi),fmini)
        d0R=d0R+ddR
      end do
      do i=1,n ! generate points on boundary and initial points to the left and right side
        call GetBndPt(k,s(i),x(i),y(i),vt1,vt2,iDL,iDR,idum)
        vn(1,i)=vt2 ! Normalenvektor auf Randpunkt (x,y)
        vn(2,i)=-vt1
        xL(i)=x(i)+dL(i)*vn(1,i) ! Punkt im Abstand dL auf der Normalen
        yL(i)=y(i)+dL(i)*vn(2,i)
        xR(i)=x(i)-dR(i)*vn(1,i) ! Punkt im Abstand -dR auf der Normalen
        yR(i)=y(i)-dR(i)*vn(2,i)
      end do
      do j=1,max(iabs(n0),1) ! adapt positions
        if(lReduce) call GenExpCreduce(n,x,y,xL,yL,xR,yR,dL,dR,vn,ds,facti)
        call GenExpCcorrect(k,n,x,y,xL,yL,xR,yR,dL,dR,iDL,iDR,vn,lAllBnd)
        call GenExpCsmooth(n,x,y,xL,yL,xR,yR,dL,dR,vn,ddL,ddR,lclosed)
        if(n0.lt.0) call GenExpCsetEqualLR(n,x,y,xL,yL,xR,yR,dL,dR,vn)
        call GenExpCcorrectSymm(n,x,y,xL,yL,xR,yR,dL,dR)
        call GenExpClimits(n,dL,dR,fmini,fmaxi)
      end do
      if(iDL.gt.0) call GenExpCdraw(n,x,y,xL,yL,dL,0.5d0*ds,iCl,0_2)
      if(iDR.gt.0) call GenExpCdraw(n,x,y,xR,yR,dR,0.5d0*ds,iCl,0_2)
      call GenExpCselect1(n,xL,yL,xR,yR,dL,dR,fr_inn,fr_out)
      dL(1:la)=0.0d0 ! don't set expansions near beginning / end of boundary
      dR(1:la)=0.0d0
      dL(le:n)=0.0d0
      dR(le:n)=0.0d0
      if(iDL.gt.0_2) then ! insert new expansions
        call GenExpCdraw(n,x,y,xL,yL,dL,0.5d0*ds,0_2,iCl)
        do i=1,n
          if((dL(i).lt.pSmall).or.(iDL.lt.1_2)) Cycle
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
          call SetExp(iB,iDL,iCn,iCl,xL(i),yL(i))
        end do
      end if
      if(iDR.gt.0_2) then ! insert new expansions
        call GenExpCdraw(n,x,y,xR,yR,dR,0.5d0*ds,0_2,iCl)
        do i=1,n
          if((dR(i).lt.pSmall).or.(iDR.lt.1_2)) Cycle
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
          call SetExp(iB,iDR,iCn,iCl,xR(i),yR(i))
        end do
      end if
      ic=SetColor(ic0)
    end do
    Deallocate(s,dL,dR,x,y,xL,yL,xR,yR,vn,stat=idum)
  end Subroutine GenExpC

  Subroutine GenExpClimits(n,dL,dR,fmini,fmaxi)
! keep distances within limits
    Implicit none
    Integer(4) i,n
    Real(8) dL(n+1),dR(n+1),fmini,fmaxi
    do i=1,n+1
      dL(i)=max(min(dL(i),fmaxi),fmini)
      dR(i)=max(min(dR(i),fmaxi),fmini)
    end do
  end Subroutine GenExpClimits

  Subroutine GenExpCnearest(n,d,k)
! correct distances using curvature
    Implicit none
    Integer(4) i,n,k
    Real(8) d(n+1),dm
    dm=pBig
    k=0
    do i=1,n
      if(d(i).lt.pSmall) Cycle
      if(d(i).gt.dm) Cycle
      k=i
      dm=d(i)
    end do
  end Subroutine GenExpCnearest

  Subroutine GenExpCselect1(n,xL,yL,xR,yR,dL,dR,fr_inn,fr_out)
! correct distances using curvature
    Implicit none
    Integer(4) i,n,k,j
    Real(8) xL(n+1),yL(n+1),xR(n+1),yR(n+1),dL(n+1),dR(n+1),fr_inn,fr_out,d,dm,fi,fo
    fi=dabs(fr_inn)
    fo=dabs(fr_out)
    do j=1,n
      call GenExpCnearest(n,dL,k)
      if(k.lt.1) Exit
      dm=dL(k)*fi
      do i=1,n
        if(dL(i).lt.pSmall) Cycle
        if(i.eq.k) Cycle
        d=dsqrt((xL(i)-xL(k))**2+(yL(i)-yL(k))**2)
        if(d.lt.dm) dL(i)=0.0d0 ! excluded point marked by zero dL
      end do
      dL(k)=min(-dL(k),nSmall) ! selected point characterized by negative dL
    end do
    do j=1,n
      call GenExpCnearest(n,dR,k)
      if(k.lt.1) Exit
      dm=dR(k)*fo
      do i=1,n
        if(dR(i).lt.pSmall) Cycle
        if(i.eq.k) Cycle
        d=dsqrt((xR(i)-xR(k))**2+(yR(i)-yR(k))**2)
        if(d.lt.dm) dR(i)=0.0d0 ! excluded point marked by zero dR
      end do
      dR(k)=min(-dR(k),nSmall) ! selected point characterized by negative dR
    end do
    do i=1,n ! set positive dL, dR again (excluded points remain 0)
      dL(i)=dabs(dL(i))
      dR(i)=dabs(dR(i))
    end do
  end Subroutine GenExpCselect1

  Subroutine GenExpCdraw(n,x,y,xL,yL,dL,dX,icL,icXL)
! correct distances using curvature
    Implicit none
    Integer(2) ic,icL,icXL
    Integer(4) i,n
    Real(8) x(n+1),y(n+1),xL(n+1),yL(n+1),dL(n+1),dX
    do i=1,n
      if(dL(i).lt.pSmall) Cycle
      if(icL.gt.0_2) then
        ic=SetColor(icL)
        call DrawLine(x(i),y(i),xL(i),yL(i))
      end if
      if(icXL.gt.0_2) then
        ic=SetColor(icXL)
        call DrawX(xL(i),yL(i),dX)
      end if
    end do
  end Subroutine GenExpCdraw

  Subroutine GenExpCreduce(n,x,y,xL,yL,xR,yR,dL,dR,vn,ds,facti)
! correct distances using curvature
    Implicit none
   ! Integer(2) ic
    Integer(4) i,n
    Real(8) x(n+1),y(n+1),xL(n+1),yL(n+1),xR(n+1),yR(n+1),dL(n+1),dR(n+1),vn(2,n+1),ds,facti,a,v1(2),v2(2),r,s
    v1=vn(1:2,2)-vn(1:2,1)
    a=r2Vec_Length(v1)
    if(a.gt.1.0d-30) then
      v1=v1/a
      v2(1)=-vn(2,1)
      v2(2)=vn(1,1)
      s=r2Scl_Prod(v1,v2)
      a=2.0d0*atan(0.5d0*a) ! angle between neighbor normal vectors
      r=abs(ds/a) ! radius of curvature
      if((dL(1).gt.r).and.(s.lt.0.0d0)) then
        dL(1)=abs(r)*facti
        xL(1)=x(1)+dL(1)*vn(1,1)
        yL(1)=y(1)+dL(1)*vn(2,1)
      end if
      if((dR(1).gt.r).and.(s.gt.0.0d0)) then
        dR(1)=abs(r)*facti
        xR(1)=x(1)-dR(1)*vn(1,1)
        yR(1)=y(1)-dR(1)*vn(2,1)
      end if
    end if
    do i=2,n
      v1=vn(1:2,i)-vn(1:2,i-1)
      a=r2Vec_Length(v1)
      if(a.gt.1.0d-30) then
        v1=v1/a
        v2(1)=-vn(2,i) ! tangential unit vector
        v2(2)=vn(1,i)
        s=r2Scl_Prod(v1,v2)
        a=2.0d0*atan(0.5d0*a) ! angle between neighbor normal vectors
        r=abs(ds/a)*facti ! radius of curvature times factor
        if((dL(i).gt.r).and.(s.lt.0.0d0)) then
          dL(i)=r
          xL(i)=x(i)+dL(i)*vn(1,i)
          yL(i)=y(i)+dL(i)*vn(2,i)
        end if
        if((dR(i).gt.r).and.(s.gt.0.0d0)) then
          dR(i)=r
          xR(i)=x(i)-dR(i)*vn(1,i)
          yR(i)=y(i)-dR(i)*vn(2,i)
        end if
      end if
    end do
  end Subroutine GenExpCreduce

  Subroutine GenExpCcorrect(k,n,x,y,xL,yL,xR,yR,dL,dR,iDL,iDR,vn,lAllBnd)
! correct distances using nearest boundary points
    Implicit none
    Logical(4) lAllBnd,lRSidemin
    Integer(2) iDL,iDR,iDL1,iDL2,iDR1,iDR2,kBmink
    Integer(4) i,n,k
    Real(8) x(n+1),y(n+1),xL(n+1),yL(n+1),xR(n+1),yR(n+1),dL(n+1),dR(n+1),vn(2,n+1),dmin,xNmin,yNmin,v1(2),smink,a
    do i=1,n 
      call DistPtBnd(0,0,xL(i),yL(i),.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDL1,v1,iDL2,smink,kBmink)
      if((1.01d0*dmin.lt.dL(i)).and.(lAllBnd.or.(iDL.eq.iDL1).or.(kBmink.eq.k))) then ! nearest boundary point is closer to (x1,y1) then (x,y) -> reduce distance
        v1(1)=y(i)-yNmin
        v1(2)=xNmin-x(i)
        call unit2DV(v1) ! Einheitsvektor der Mittelsenkrechten
        xNmin=0.5d0*(xNmin+x(i))
        yNmin=0.5d0*(yNmin+y(i)) ! Fusspunkt der Mittelsenkrechten
        a=(v1(1)*(yNmin-y(i))+v1(2)*(x(i)-xNmin)).div.(v1(1)*vn(2,i)-v1(2)*vn(1,i))
        xL(i)=x(i)+a*vn(1,i)
        yL(i)=y(i)+a*vn(2,i) ! Schnittpunkt Mittelsenkrechte mit Normale im Randpunkt
        call DistPtBnd(0,0,xL(i),yL(i),.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDL1,v1,iDL2,smink,kBmink)
        if((1.01d0*dmin.lt.dL(i)).and.(lAllBnd.or.(iDL.eq.iDL1).or.(kBmink.eq.k))) then ! ONCE MORE: nearest boundary point is closer to (x1,y1) then (x,y) -> reduce distance
          v1(1)=y(i)-yNmin
          v1(2)=xNmin-x(i)
          call unit2DV(v1) ! Einheitsvektor der Mittelsenkrechten
          xNmin=0.5d0*(xNmin+x(i))
          yNmin=0.5d0*(yNmin+y(i)) ! Fusspunkt der Mittelsenkrechten
          a=(v1(1)*(yNmin-y(i))+v1(2)*(x(i)-xNmin)).div.(v1(1)*vn(2,i)-v1(2)*vn(1,i))
        end if
        dL(i)=min(abs(a),dL(i))
        xL(i)=x(i)+dL(i)*vn(1,i)
        yL(i)=y(i)+dL(i)*vn(2,i)
      end if
      call DistPtBnd(0,0,xR(i),yR(i),.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDR1,v1,iDR2,smink,kBmink)
      if((1.01d0*dmin.lt.dR(i)).and.(lAllBnd.or.(iDR.eq.iDR1).or.(kBmink.eq.k))) then ! nearest boundary point is closer to (x1,y1) then (x,y) -> reduce distance
        v1(1)=y(i)-yNmin
        v1(2)=xNmin-x(i)
        call unit2DV(v1) ! Einheitsvektor der Mittelsenkrechten
        xNmin=0.5d0*(xNmin+x(i))
        yNmin=0.5d0*(yNmin+y(i)) ! Fusspunkt der Mittelsenkrechten
        a=(v1(1)*(yNmin-y(i))+v1(2)*(x(i)-xNmin)).div.(v1(1)*vn(2,i)-v1(2)*vn(1,i))
        xR(i)=x(i)+a*vn(1,i)
        yR(i)=y(i)+a*vn(2,i) ! Schnittpunkt Mittelsenkrechte mit Normale im Randpunkt
        call DistPtBnd(0,0,xR(i),yR(i),.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDR1,v1,iDR2,smink,kBmink)
        if((1.01d0*dmin.lt.dR(i)).and.(lAllBnd.or.(iDR.eq.iDR1).or.(kBmink.eq.k))) then ! ONCE MORE: nearest boundary point is closer to (x1,y1) then (x,y) -> reduce distance
          v1(1)=y(i)-yNmin
          v1(2)=xNmin-x(i)
          call unit2DV(v1) ! Einheitsvektor der Mittelsenkrechten
          xNmin=0.5d0*(xNmin+x(i))
          yNmin=0.5d0*(yNmin+y(i)) ! Fusspunkt der Mittelsenkrechten
          a=(v1(1)*(yNmin-y(i))+v1(2)*(x(i)-xNmin)).div.(v1(1)*vn(2,i)-v1(2)*vn(1,i))
        end if
        dR(i)=min(abs(a),dR(i))
        xR(i)=x(i)-dR(i)*vn(1,i)
        yR(i)=y(i)-dR(i)*vn(2,i)
      end if
    end do
  end Subroutine GenExpCcorrect

  Subroutine GenExpCsmooth(n,x,y,xL,yL,xR,yR,dL,dR,vn,ddL,ddR,lclosed)
! correct distances: smooth increase / decrease of dL, dR
    Implicit none
    Logical(4) lclosed
    Integer(4) i,n
    Real(8) x(n+1),y(n+1),xL(n+1),yL(n+1),xR(n+1),yR(n+1),dL(n+1),dR(n+1),vn(2,n+1),ddL,ddR
    if(lclosed) then
      if(dL(1).lt.dL(n)) then
        dL(1)=Max(dL(n)-ddL,dL(1))
      else
        dL(1)=Min(dL(n)+ddL,dL(1))
      end if
    end if
    do i=2,n
      if(dL(i).lt.dL(i-1)) then
        dL(i)=Max(dL(i-1)-ddL,dL(i))
      else
        dL(i)=Min(dL(i-1)+ddL,dL(i))
      end if
    end do
    if(lclosed) then
      if(dL(n).lt.dL(1)) then
        dL(n)=Max(dL(1)-ddL,dL(n))
      else
        dL(n)=Min(dL(1)+ddL,dL(n))
      end if
    end if
    do i=n-1,1,-1
      if(dL(i).lt.dL(i+1)) then
        dL(i)=Max(dL(i+1)-ddL,dL(i))
      else
        dL(i)=Min(dL(i+1)+ddL,dL(i))
      end if
    end do
    if(lclosed) then
      if(dR(1).lt.dR(n)) then
        dR(1)=Max(dR(n)-ddR,dR(1))
      else
        dR(1)=Min(dR(n)+ddR,dR(1))
      end if
    end if
    do i=2,n
      if(dR(i).lt.dR(i-1)) then
        dR(i)=Max(dR(i-1)-ddR,dR(i))
      else
        dR(i)=Min(dR(i-1)+ddR,dR(i))
      end if
    end do
    if(lclosed) then
      if(dR(n).lt.dR(1)) then
        dR(n)=Max(dR(1)-ddR,dR(n))
      else
        dR(n)=Min(dR(1)+ddR,dR(n))
      end if
    end if
    do i=n-1,1,-1
      if(dR(i).lt.dR(i+1)) then
        dR(i)=Max(dR(i+1)-ddR,dR(i))
      else
        dR(i)=Min(dR(i+1)+ddR,dR(i))
      end if
    end do
    do i=1,n
      xL(i)=x(i)+dL(i)*vn(1,i)
      yL(i)=y(i)+dL(i)*vn(2,i)
      xR(i)=x(i)-dR(i)*vn(1,i)
      yR(i)=y(i)-dR(i)*vn(2,i)
    end do
  end Subroutine GenExpCsmooth

  Subroutine GenExpCcorrectSymm(n,x,y,xL,yL,xR,yR,dL,dR)
! correct distances using nearest boundary points
    Implicit none
    Integer(4) i,n
    Real(8) x(n+1),y(n+1),xL(n+1),yL(n+1),xR(n+1),yR(n+1),dL(n+1),dR(n+1)
    do i=1,n ! handle problems with symmetry planes
      if(ixzSymm.ne.0) then
        if(y(i)*yL(i).lt.0.0d0) then
          if(dabs(y(i)).gt.dabs(yL(i))) then
            xL(i)=x(i)+(xL(i)-x(i))*abs(y(i))/(abs(y(i))+abs(yL(i)))
          end if
          yL(i)=0.0d0
          dL(i)=dsqrt((xL(i)-x(i))**2+(yL(i)-y(i))**2)
        end if
      end if
      if(iyzSymm.ne.0) then
        if(x(i)*xL(i).lt.0.0d0) then
          if(dabs(x(i)).gt.dabs(xL(i))) then
            yL(i)=y(i)+(yL(i)-y(i))*abs(x(i))/(abs(x(i))+abs(xL(i)))
          end if
          xL(i)=0.0d0
          dL(i)=dsqrt((xL(i)-x(i))**2+(yL(i)-y(i))**2)
        end if
      end if
      if(ixzSymm.ne.0) then 
        if(y(i)*yR(i).lt.0.0d0) then
          if(dabs(y(i)).gt.dabs(yR(i))) then
            xR(i)=x(i)+(xR(i)-x(i))*abs(y(i))/(abs(y(i))+abs(yR(i)))
          end if
          yR(i)=0.0d0
          dR(i)=dsqrt((xR(i)-x(i))**2+(yR(i)-y(i))**2)
        end if
      end if
      if(iyzSymm.ne.0) then
        if(x(i)*xR(i).lt.0.0d0) then
          if(dabs(x(i)).gt.dabs(xR(i))) then
            yR(i)=y(i)+(yR(i)-y(i))*abs(x(i))/(abs(x(i))+abs(xR(i)))
          end if
          xR(i)=0.0d0
          dR(i)=dsqrt((xR(i)-x(i))**2+(yR(i)-y(i))**2)
        end if
      end if
    end do
  end Subroutine GenExpCcorrectSymm

  Subroutine GenExpCsetEqualLR(n,x,y,xL,yL,xR,yR,dL,dR,vn)
! set equal right and left distances
    Implicit none
    Integer(4) i,n
    Real(8) x(n+1),y(n+1),xL(n+1),yL(n+1),xR(n+1),yR(n+1),dL(n+1),dR(n+1),vn(2,n+1)
    do i=1,n
      dR(i)=min(dR(i),dL(i))
      xR(i)=x(i)-dR(i)*vn(1,i)
      yR(i)=y(i)-dR(i)*vn(2,i)
      dL(i)=dR(i)
      xL(i)=x(i)+dL(i)*vn(1,i)
      yL(i)=y(i)+dL(i)*vn(2,i)
    end do
  end Subroutine GenExpCsetEqualLR

  Subroutine GenExp(kB,n0,jE,jCl,jCn,jDm,fmini,fmaxi,facti,r_first,r_last,fr_inn,fr_out,d_l,d_r,a_inn,a_out,n_points)
! generate n expansions with data of expansion jE along the boundary kB, set color iCl, connection flag iCn
    Implicit none
    Integer(4), Intent(in) :: jE,kB,n0,n_points
    Integer(2), Intent(in) :: jCl,jCn,jDm
    Real(8), Intent(in) :: fmini,fmaxi,facti,r_first(2),r_last(2),fr_inn,fr_out,d_l,d_r,a_inn,a_out
    Real(8) fmin,fmax,fact,val(2),s1,s2,xdum,ydum,vt1,vt2,dsm,ds,s,dmin,dmax,d,c,xn,yn,Am,A,R1,R2,R1s,R2s,R,fR,Xm,Ym, &
    & xNmin,yNmin
    Logical lRSidemin
    Integer(2) iCl,iCn,iDl,iDR,iDl1,iDR1
    Integer(4) idum,n,k1,k2,k,iB
    Logical(4) ldum
    if((jE.lt.1).or.(jE.gt.nExp)) return
    call GetKWin(.false.)
    idum=SetActiveQQ(10+kWin)
    idum=SetColor(2_4)
    fmin=fmini
    fmax=fmaxi
    fact=facti
    ldum=.false.
    call cBndGetABO() ! get c-poly, splines, match.pts
    if(n_points.lt.1) then
      call GenExpC(kB,n0,jE,jCl,jCn,jDm,fmini,fmaxi,facti,r_first,r_last,fr_inn,fr_out,d_l,d_r,a_inn,a_out,n_points)
      return
    end if
    if(n0.gt.999998) then
      call GenExpGMSH(jE,jCl,jCn,jDm,fmini)
      return
    else if(n0.gt.0) then
      if(abs(fmax).lt.pSmall) fmax=2.0d0
      if(abs(fact).lt.pSmall) fact=1.0d0
      call GenExp1(kB,n0,jE,jCl,jCn,jDm,fmin,fmax,fact)
      return
    else if(n0.eq.0) then
      call GenExp2(kB,n0,jE,jCl,jCn,jDm,fmin,fmax,fact,r_first,r_last,fr_inn,fr_out,d_l,d_r,a_inn,a_out,n_points)
      return
    end if
    if(abs(fmax).lt.pSmall) fmax=2.0d0
    if(abs(fact).lt.pSmall) fact=1.0d0
    n=-n0
    if((kB.ge.1).and.(kB.le.nBnd)) then
      k1=kB
      k2=k1
    else if((kB.lt.0).and.(-kB.lt.nBnd)) then
      k1=1
      k2=-kB
    else
      k1=1
      k2=nBnd
    end if
    iB=jE+1
    do k=k1,k2
      if(jDm.ne.0) then
        if((jDm.ne.tBnd(k)%iLDom).and.(jDm.ne.tBnd(k)%iRDom)) Cycle
      end if
      iCl=jCl
      if(iCl.eq.-1) iCl=tBnd(k)%iCol
      iCn=jCn
      if(iCn.lt.-32000) iCn=tBnd(k)%iConn
      s1=tBnd(k)%Start
      s2=s1+tBnd(k)%sLength
      call GetBndPt(k,0.5d0*(s1+s2),Xdum,Ydum,vt1,vt2,iDL,iDR,idum)
      if(jDm.ne.0) then
        if(jDm.ne.iDL) iDL=0
        if(jDm.ne.iDR) iDR=0
      end if
      dsm=(s2-s1)/(Dble(n)+0.002d0)
      if(iDL.gt.0_2) then
        ds=dsm
        s=s1+0.001d0*ds
        dmin=fmin*ds
        dmax=fmax*ds
        d=0.5d0*(dmax-dmin)
        c=0.5d0*(dmax+dmin)
        do while (s.lt.s2)
          call GetBndPt(k,s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          Am=Datan2(vt1,-vt2)
          call GetBndPt(k,s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          A=Datan2(vt1,-vt2)
          R1=min(1.0d100,0.25d0*(0.0005d0*ds).div.(Am-A))
          if(R1.lt.0.0d0) R1=dmax
          R1s=d*dtanh((R1-c)/d)+c
          call GetBndPt(k,s+R1s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          Am=Datan2(vt1,-vt2)
          call GetBndPt(k,s+R1s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          A=Datan2(vt1,-vt2)
          R2=min(1.0d100,0.25d0*(0.0005d0*ds).div.(Am-A))
          if(R2.lt.0.0d0) R2=dmax
          R2s=d*dtanh((R2-c)/d)+c
          ds=max((dsm*fmin),min(dsm,0.5d0*(R1s+R2s)))
          s=s+ds
          if(s.gt.s2) Exit
          call GetBndPt(k,s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          Am=Datan2(vt1,-vt2)
          call GetBndPt(k,s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          A=Datan2(vt1,-vt2)
          R=min(1.0d100,0.25d0*(0.0005d0*ds).div.(Am-A))
          if(R.lt.0.0d0) R=dmax
          R=d*dtanh((R-c)/d)+c
          fR=2.0d0*fact*R ! set multipole in doubled distance and check domain
          Xm=Xn-fR*DCos(Am)
          Ym=Yn-fR*DSin(Am)
          call DistPtBnd(0,0,Xm,Ym,.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDL1,val)
          if(iDL.eq.iDL1) then ! multipole in domain -> reduce distance
            fR=0.333d0*(fR-dmin)
            Xm=Xn-fR*DCos(Am)
            Ym=Yn-fR*DSin(Am)
          else ! multipole outside domain -> set original distance, reduce if...
            fR=0.5d0*fR
            if(dmin.lt.1.001d0*fR) then
              fR=0.333d0*(2.0d0*fR+dmin)
            end if
            Xm=Xn-fR*DCos(Am)
            Ym=Yn-fR*DSin(Am)
          end if
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				  if(.NOT.ldum) return
          call SetExp(iB,iDL,iCn,iCl,Xm,Ym)
          call DrawLine(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),Xn,Yn)
          s=s+ds
        end do
      end if
      if(iDR.gt.0_2) then
        ds=dsm
        s=s1+0.001d0*ds
        dmin=fmin*ds
        dmax=fmax*ds
        d=0.5d0*(dmax-dmin)
        c=0.5d0*(dmax+dmin)
        do while (s.lt.s2)
          call GetBndPt(k,s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          Am=Datan2(vt1,-vt2)
          call GetBndPt(k,s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          A=Datan2(vt1,-vt2)
          R1=min(1.0d100,0.25d0*(0.0005d0*ds).div.(Am-A))
          if(R1.gt.0.0d0) R1=dmax
          R1s=d*dtanh((dabs(R1)-c)/d)+c
          call GetBndPt(k,s+R1s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
          Am=Datan2(vt1,-vt2)
          call GetBndPt(k,s+R1s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          A=Datan2(vt1,-vt2)
          R2=min(1.0d100,0.25d0*(0.0005d0*ds).div.(Am-A))
          if(R2.gt.0.0d0) R2=dmax
          R2s=d*dtanh((dabs(R2)-c)/d)+c
          ds=max((dsm*fmin),min(dsm,0.5d0*(R1s+R2s)))
          s=s+ds
          if(s.gt.s2) Exit
          call GetBndPt(k,s,Xn,Yn,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          Am=Datan2(vt1,-vt2)
          call GetBndPt(k,s+0.0005d0*ds,Xdum,Ydum,vt1,vt2,iDL1,iDR1,idum)
          if((iDL1.ne.iDL).or.(iDR1.ne.iDR)) Exit
          A=Datan2(vt1,-vt2)
          R=min(1.0d100,0.25d0*(0.0005d0*ds).div.(Am-A))
          if(R.gt.0.0d0) R=dmax
          R=d*dtanh((dabs(R)-c)/d)+c
          R=0.5d0*(R+2.0d0*min(R1s,R2s))
          fR=2.0d0*fact*R ! set multipole in doubled distance and check domain
          Xm=Xn+fR*DCos(Am)
          Ym=Yn+fR*DSin(Am)
          call DistPtBnd(0,0,Xm,Ym,.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDR1,val)
          if(iDR.eq.iDR1) then ! multipole in domain -> reduce distance
            fR=0.333d0*(fR-dmin)
            Xm=Xn+fR*DCos(Am)
            Ym=Yn+fR*DSin(Am)
          else ! multipole outside domain -> set original distance, reduce if...
            fR=0.5d0*fR
            if(dmin.lt.1.001d0*fR) then
              fR=0.333d0*(2.0d0*fR+dmin)
            end if
            Xm=Xn+fR*DCos(Am)
            Ym=Yn+fR*DSin(Am)
          end if
          call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				  if(.NOT.ldum) return
          call SetExp(iB,iDR,iCn,iCl,Xm,Ym)
          call DrawLine(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),Xn,Yn)
          s=s+ds
        end do
      end if
    end do
  end Subroutine GenExp

	Subroutine GenExpM(kB,lclosed,iDL,iDR,iCn,iCl,jE,r_inii,r_fini,fr_inni,fr_outi,d_l,d_r,a_inn,a_out, &
  & r_thri,dmini,dmaxi,num_points)
!		=======================================================================
!		=AUTOMATIC MULTIPOLE SETTING (2-D)=====================================
!		=======================================================================
!		1. Last version:9/11/00
!		2. Esteban Moreno Soriano
!		3. The idea is: given a discretized curve, two "parallel" curves are constructed. To construct the "parallel" curves, the radius of 
!				curvature of the original curve is used whenever possible, otherwise a interpolation procedure is employed. The multipoles are
!				located on the parallel curves (not too close to each other).
!		4. Input parameters that control the geometry:
!				r_inii = distance (left and right) at the beginning of the curve from original curve to parallel curves (for open curves)(units=meters)
!				r_fini = distance (left and right) at the end of the curve from original curve to parallel curves (for open curves)(units=meters)
!				fr_inni, fr_outi = distance from the original curve to the parallel curves (inn=inner part=concave;out=outer part=convex) 
!													(units=radius of curvature, i.e. 0.263 would mean 0.263*local radius of curvature). (Note: the inputs
!													fr_inni, fr_outi are the inverses of the variables used inside the program (fr_inn, fr_out))
!				r_thri = a radius of curvature bigger than r_thri will be interpolated. IMPORTANT CONTROL PARAMETER! (units=meters)
!				dmini = minimum distance between original curve and parallel curves (units=meters).
!				dmaxi = maximum distance between original curve and parallel curves (units=wavelength).
!		=======================================================================
!		=PARAMETERS and VARIABLES==============================================
!		=======================================================================
		implicit none
		integer(4) dtheta_max,alphaR,securityN,big_betaI,small_betaI
		parameter (dtheta_max=15,alphaR=25,securityN=5)
		real(8) fr_inn_def,fr_out_def,renumbering_parameter,length_fr_def,interp,mult_sep(2),&
						Factor_globalshape,Factor_multip_dist,d_l,d_r,a_inn,a_out
		parameter (fr_inn_def=3.8,fr_out_def=2.2,renumbering_parameter=0.1,&
							length_fr_def=8.0,interp=2.0,Factor_globalshape=0.8, Factor_multip_dist=0.8)
!		FOR TUNING THE PARAMETERS:
!		dtheta_max=maximum allowed angle between the normals of two consecutive points in the curve (in grades); otherwise warning
!								dtheta_max = "positive and small", please!
!		alphaR=maximum 'slope angle' for a couple of consecutive points in the parallel curves to be 'Regular' (i.e. NOT black spots) (in grades)
!		securityN=between 2 blackspots there must be at least a minimum number of white points equal to securityN, otherwise they are joined. The same
!							is true between the blackspot and the beginning or end of a section
!		big_betaI=divergence angle for the 'bow Interpolation' in the inner part of the curve (in grades)
!		small_betaI=divergence angle for the 'bow Interpolation' in the outer part of the curve (in grades)
!								alphaR > big_betaI > small_betaI > 0, alphaR = not "too big", please!
!		fr_inn_def=fraction of radius of curvature by default (inner part of the curve) (3 would mean that the distance from parallel curve to 
!								boundary is one third of the radius of curvature; in case that this radius is smaller than threshold, of course)
!		fr_out_def=fraction of radius of curvature by default (outer part of the curve)
!								fr_inn_def > fr_out_def > 1
!		renumbering_parameter=controls wether the center of the renumbering will be in a HIGH or in a LOW section (in case that the longest LOW section
!													is only 'slightly' longer than the longest HIGH section). Positive and small, please. If it is 0.1, that means that the 
!													renumbering point will be in the HIGH section even if the LOW section is up to 10% longer than the HIGH.
!		length_fr_def=fraction of total length of the curve. controls the distance to the parallel curves at the beginning and at the end
!									(it is used when there is only a "high" section) > 1
!		interp=factor that decides if the interpolation is "normal" or "bow" (2 would mean that the 'bow' interpolation is used when the slope of a 
!						linear interpolation would be 1/2 of the betaI)
!								interp > 1
!		mult_sep=separation factor between contiguous multipoles (1 would mean that the distance between multipoles is equal to the distance 
!							from multipole to the boundary)
!								mult_sep = near 1.3
!		Factor_globalshape=given a point in the parallel curve, the nearest point in the original curve should be the point that is right 'bellow' it.
!												If this is not the case, the length of the segment 'pointinCurve-pointinParallelCurve' will be the distance 'nearestpointinCurve
!												-pointinParallelCurve' times this Factor_globalshape
!								0 < Factor_globalshape < 1, better a bit smaller than 1
!		Factor_multip_dist=factor that controls how close can two multipoles be.
!												0 < Factor_multip_dist < 0.8 aproximately
!
		Logical renumbering_tag,tag_firstblsp,modification_tag,ldum,lwarn1,lwarn2,lwarn3,lwarn4,lwarn5,lwarn6,lwarn7,lwarn8
		Logical, intent(in):: lclosed
		Logical(1), allocatable:: HIGH(:)
		Integer(1), allocatable:: MULTI_TAG(:,:),SIDE(:)
		Integer(2) L,iDL1,iDR1
		Integer(2), intent(in):: iDL, iDR, iCn, iCl
		Integer(4) i,num_sec,tag_sec1,tag_sec2,idum,num_blsp, &
							tag_blsp1,j,tag_blsp2,pointdown,num_blspT,sec_renumbering_high,sec_renumbering_low, &
							previousblsp_renumbering,point_renumbering,in1,in2,in3,in4,k,betaI,k_thr,point1,point2, &
							mult_num(2),mult_numT,iB
		Integer(4), intent(in):: kB,num_points,jE
		Integer(4), allocatable:: SEC(:,:),NUM_BLSPOT(:),BL_SP(:,:),POINT(:,:)
		Real(8) r_thr,dmax(2),dmin,length,fr_inn,fr_out,s,s1,s2,ds,dtheta1,dtheta2,dFR, &
						length_renumbering_high,length_renumbering_low,r_ini,r_fin,sum, &
						ds_perp,lengthfragment,x_ref,y_ref,dist_to_point_j,cosine,dist, &
						fr_shift,fr_max,fr_min,dist_multipoles,rC
		Real(8), intent(in):: r_thri,dmaxi,dmini,fr_inni,fr_outi,r_inii(2),r_fini(2)
    Real(8), Allocatable:: CRV(:,:),MULT(:,:,:),CRVrenumbered(:,:),FR(:,:),CRVp(:,:,:)
    lwarn1=.true.
    lwarn2=.true.
    lwarn3=.true.
    lwarn4=.true.
    lwarn5=.true.
    lwarn6=.true.
    lwarn7=.true.
    lwarn8=.true.
! allocate memory
    if(Allocated(CRV)) DeAllocate(CRV)
		if(allocated(SIDE)) deallocate(SIDE)
		if(allocated(CRVrenumbered)) deallocate(CRVrenumbered)
		if(allocated(FR)) deallocate(FR)
		if(allocated(CRVp)) deallocate(CRVp)
    Allocate(CRV(4,num_points),SIDE(num_points),CRVrenumbered(4,num_points),FR(2,num_points),CRVp(2,2,num_points), &
    & Stat=idum)
		if(idum.ne.0) then
			idum=MessageBoxQQ('Memory allocation for multipole setting failed!'C,'Generate expansions'C,MB$OK.or.MB$IconExclamation)
			return
		end if
! compute num_points points along boundary kB
    s1=tBnd(kB)%Start
    s2=s1+tBnd(kB)%sLength
    length=s2-s1
		r_thr=r_thri
		if(r_thr<=0) r_thr=length*0.5d0/Pi
    ds=length/Dble(num_points)
    call cBndGetABO() ! get c-poly, splines, match.pts if not yet done
    s=s1-0.5d0*ds
    do i=1,num_points
      s=s+ds
      call GetBndPt(kB,s,x_ref,y_ref,s1,s2,iDL1,iDR1,idum,rC)
      CRV(1,i)=x_ref
      CRV(2,i)=y_ref
      CRV(3,i)=Datan2(s1,-s2) ! angle
      CRV(4,i)=max(min(rC,r_thr),ds) ! ds < radius of curvature < r_thr
    end do
! radius of curvature shall not grow to rapidly
    if(lclosed) then
      if(CRV(4,1).gt.1.5d0*CRV(4,num_points)) CRV(4,1)=1.5d0*CRV(4,num_points)
    end if
    do i=2,num_points
      if(CRV(4,i).gt.1.5d0*CRV(4,i-1)) CRV(4,i)=1.5d0*CRV(4,i-1)
    end do
! radius of curvature shall also not grow to rapidly in the inverse direction
    if(lclosed) then
      if(CRV(4,num_points).gt.1.5d0*CRV(4,1)) CRV(4,num_points)=1.5d0*CRV(4,1)
    end if
    do i=num_points-1,1,-1
      if(CRV(4,i).gt.1.5d0*CRV(4,i+1)) CRV(4,i)=1.5d0*CRV(4,i+1)
    end do
!		=======================================================================
!		=BEGIN=================================================================
!		=======================================================================
!		inputs
    big_betaI=abs(min(25,nint(a_inn,4)))
    small_betaI=abs(min(25,nint(a_out,4)))
    mult_sep(1)=1.3/max(d_l,0.01)
    mult_sep(2)=1.3/max(d_r,0.01)
		dmin=dmini
		if(dmin<0.0) dmin=0.0
		if(dmaxi<=0.0)then
			if(Dble(fcFld)/=0.AND.iDR/=0)then
				dmax(1)=0.5d0*Wlength(iDR) ! distance multipole-boubdary should not exceet half wavelength
			else
				dmax(1)=100.0*r_thr
			end if
			if(Dble(fcFld)/=0.AND.iDL/=0)then
				dmax(2)=0.5d0*Wlength(iDL)
			else
				dmax(2)=100.0*r_thr
			end if
		else ! I think the previous statements of Esteban with wavelength-dependent dmax were confusing (CH)
      dmax(1)=dmaxi
      dmax(2)=dmaxi
    end if
		if(dmin>dmax(1).OR.dmin>dmax(2))then
			if(dmax(1)<dmax(2))then
				dmin=dmax(1)
			else
				dmin=dmax(2)
			end if
			if(lwarn2) write(*,*) 'WARNING! Minimum distance WAS bigger than maximum distance.'
      lwarn2=.false.
		end if
!		--------------------------------------------------------
!		Computing FR (1st time: FRACTION of RADIUS of CURVATURE)
!		--------------------------------------------------------
!		inputs and defaults
		if(fr_inni<=0.0)then
			fr_inn=fr_inn_def
		else
			fr_inn=1.0/fr_inni ! this is only because I use as input the inverse of the variable used inside the routine
		end if
		if(fr_outi<=0.0)then
			fr_out=fr_out_def
		else
			fr_out=1.0/fr_outi ! this is only because I use as input the inverse of the variable used inside the routine
		end if
		if(fr_out>fr_inn)then
			if(lwarn3) write(*,*) 'WARNING! F outside IS smaller than F inside.'
      lwarn1=.false.
		end if
		if(fr_out<1.0)then
			if(lwarn4) write(*,*) 'WARNING! F outside IS bigger than 1 radius of curvature.'
      lwarn4=.false.
		end if
		if(fr_inn<1.0)then
			if(lwarn5) write(*,*) 'WARNING! F inside IS bigger than 1 radius of curvature.'
      lwarn5=.false.
		end if
!		computing FR and where is the inner part of the curve
		renumbering_tag=.true. ! this allows the renumbering to be done
10	if(not(renumbering_tag))then ! this is done only after the renumbering
			CRV=CRVrenumbered
		end if
!		first point
!		for closed curves
		if(lclosed)then
!		computing differential of arc and angle (left limit)
			dtheta1=(CRV(3,1)-CRV(3,num_points))
!		paying atention to the branch at pi and the coarseness of the discretization
			if(     dtheta1>=2*pi-2*dtheta_max*pi/180.0) then
				dtheta1=dtheta1-2*pi
			else if(dtheta1<=-2*pi+2*dtheta_max*pi/180.0) then
				dtheta1=dtheta1+2*pi
			else if((( 2*dtheta_max*pi/180.0<dtheta1).AND.( dtheta1<2*pi-2*dtheta_max*pi/180.0)).OR.&
							((-2*pi+2*dtheta_max*pi/180.0<dtheta1).AND.(dtheta1<-2*dtheta_max*pi/180.0))) then
				if(lwarn6) write(*,*) 'WARNING! You may need round the corners or/and increase the number of spline points.'
        lwarn6=.false.
			end if
!		computing differential of arc and angle (right limit)
			dtheta2=(CRV(3,2)-CRV(3,1))
!		paying atention to the branch at pi and the coarseness of the discretization
			if(     dtheta2>=2*pi-2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2-2*pi
			else if(dtheta2<=-2*pi+2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2+2*pi
			else if((( 2*dtheta_max*pi/180.0<dtheta2).AND.( dtheta2<2*pi-2*dtheta_max*pi/180.0)).OR.&
							((-2*pi+2*dtheta_max*pi/180.0<dtheta2).AND.(dtheta2<-2*dtheta_max*pi/180.0))) then
				if(lwarn6) write(*,*) 'WARNING! You may need round the corners or/and increase the number of spline points.'
        lwarn6=.false.
			end if
!		computing where is the inner part of the curve (avoiding divergences) and FR
			if(abs(dtheta1)<ds/pBig.AND.abs(dtheta2)<ds/pBig)then ! both diverge
				SIDE(1)=0 ! the curve is flat (i.e. center of curvature is NOT defined (0))
				FR(1,1)=r_thr
				FR(2,1)=r_thr
			else if(abs(dtheta1)>=ds/pBig.AND.abs(dtheta2)<ds/pBig)then ! left limit is OK and right limit diverges
				if(abs(dtheta1)<ds/r_thr)then
					if(dtheta1>0)then
						SIDE(1)=1 ! center of curvature is to the LEFT (+1) of the curve
					else
						SIDE(1)=-1 ! center of curvature is to the RIGHT (-1) of the curve
					end if
					FR(1,1)=r_thr
					FR(2,1)=r_thr
				else
					if(dtheta1>0)then
						SIDE(1)=1
						FR(1,1)=CRV(4,1)/fr_inn ! LEFT (1) fraction of radius of curvarure is inner
						FR(2,1)=CRV(4,1)/fr_out ! RIGHT (2) fraction of radius of curvarure is outer
					else
						SIDE(1)=-1
						FR(1,1)=CRV(4,1)/fr_out
						FR(2,1)=CRV(4,1)/fr_inn
					end if
				end if
			else if(abs(dtheta1)<ds/pBig.AND.abs(dtheta2)>=ds/pBig)then ! left limit diverges and right limit is OK
				if(abs(dtheta2)<ds/r_thr)then
					if(dtheta2>0)then
						SIDE(1)=1
					else
						SIDE(1)=-1
					end if
					FR(1,1)=r_thr
					FR(2,1)=r_thr
				else
					if(dtheta2>0)then
						SIDE(1)=1
						FR(1,1)=CRV(4,1)/fr_inn
						FR(2,1)=CRV(4,1)/fr_out
					else
						SIDE(1)=-1
						FR(1,1)=CRV(4,1)/fr_out
						FR(2,1)=CRV(4,1)/fr_inn
					end if
				end if
			else ! both limits are OK
				if(abs(dtheta1)<ds/r_thr.AND.abs(dtheta2)<ds/r_thr)then ! both radii of curvature are too large
					if(dtheta1>0.AND.dtheta2>0)then
						SIDE(1)=1
					else if(dtheta1<0.AND.dtheta2<0)then
						SIDE(1)=-1
					else
						SIDE(1)=0
					end if
					FR(1,1)=r_thr
					FR(2,1)=r_thr
				else if(abs(dtheta1)>=ds/r_thr.AND.abs(dtheta2)<ds/r_thr)then ! left limit is fine, right limit is too large
					if(dtheta1>0)then
						SIDE(1)=1
						FR(1,1)=CRV(4,1)/fr_inn
						FR(2,1)=CRV(4,1)/fr_out
					else
						SIDE(1)=-1
						FR(1,1)=CRV(4,1)/fr_out
						FR(2,1)=CRV(4,1)/fr_inn
					end if
				else if(abs(dtheta1)<ds/r_thr.AND.abs(dtheta2)>=ds/r_thr)then ! left limit is too large, right limit is fine
					if(dtheta2>0)then
						SIDE(1)=1
						FR(1,1)=CRV(4,1)/fr_inn
						FR(2,1)=CRV(4,1)/fr_out
					else
						SIDE(1)=-1
						FR(1,1)=CRV(4,1)/fr_out
						FR(2,1)=CRV(4,1)/fr_inn
					end if
				else ! both are fine
					if(ds/abs(dtheta1)<=ds/abs(dtheta2))then
						if(dtheta1>0)then
							SIDE(1)=1
							FR(1,1)=CRV(4,1)/fr_inn
							FR(2,1)=CRV(4,1)/fr_out
						else
							SIDE(1)=-1
							FR(1,1)=CRV(4,1)/fr_out
							FR(2,1)=CRV(4,1)/fr_inn
						end if
					else
						if(dtheta2>0)then
							SIDE(1)=1
							FR(1,1)=CRV(4,1)/fr_inn
							FR(2,1)=CRV(4,1)/fr_out
						else
							SIDE(1)=-1
							FR(1,1)=CRV(4,1)/fr_out
							FR(2,1)=CRV(4,1)/fr_inn
						end if
					end if
				end if
			end if
!		for open curves
		else
!		computing differential of arc and angle
			dtheta2=(CRV(3,2)-CRV(3,1))
!		paying atention to the branch at pi and the coarseness of the discretization
			if(     dtheta2>=2*pi-2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2-2*pi
			else if(dtheta2<=-2*pi+2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2+2*pi
			else if((( 2*dtheta_max*pi/180.0<dtheta2).AND.( dtheta2<2*pi-2*dtheta_max*pi/180.0)).OR.&
							((-2*pi+2*dtheta_max*pi/180.0<dtheta2).AND.(dtheta2<-2*dtheta_max*pi/180.0))) then
				if(lwarn6) write(*,*) 'WARNING! You may need round the corners or/and increase the number of spline points.'
        lwarn6=.false.
			end if
!		computing where is the inner part of the curve (avoiding divergences) and FR
			if(abs(dtheta2)<ds/pBig)then
				SIDE(1)=0
				FR(1,1)=r_thr
				FR(2,1)=r_thr
			else if(abs(dtheta2)<ds/r_thr)then
				if(dtheta2>0)then
					SIDE(1)=1
				else
					SIDE(1)=-1
				end if
				FR(1,1)=r_thr
				FR(2,1)=r_thr
			else
				if(dtheta2>0)then
					SIDE(1)=1
					FR(1,1)=CRV(4,1)/fr_inn
					FR(2,1)=CRV(4,1)/fr_out
				else
					SIDE(1)=-1
					FR(1,1)=CRV(4,1)/fr_out
					FR(2,1)=CRV(4,1)/fr_inn
				end if
			end if
		end if
!	all the points but the first and last ones
		do i=2,num_points-1
!		computing differential of arc and angle
			dtheta1=dtheta2
			dtheta2=(CRV(3,i+1)-CRV(3,i))
!		paying atention to the branch at pi and the coarseness of the discretization
			if(     dtheta2>=2*pi-2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2-2*pi
			else if(dtheta2<=-2*pi+2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2+2*pi
			else if((( 2*dtheta_max*pi/180.0<dtheta2).AND.( dtheta2<2*pi-2*dtheta_max*pi/180.0)).OR.&
							((-2*pi+2*dtheta_max*pi/180.0<dtheta2).AND.(dtheta2<-2*dtheta_max*pi/180.0))) then
				if(lwarn6) write(*,*) 'WARNING! You may need round the corners or/and increase the number of spline points.'
        lwarn6=.false.
			end if
!		computing where is the inner part of the curve (avoiding divergences) and FR
			if(abs(dtheta1)<ds/pBig.AND.abs(dtheta2)<ds/pBig)then ! both diverge
				SIDE(i)=0
				FR(1,i)=r_thr
				FR(2,i)=r_thr
			else if(abs(dtheta1)>=ds/pBig.AND.abs(dtheta2)<ds/pBig)then ! left limit is OK and right limit diverges
				if(abs(dtheta1)<ds/r_thr)then
					if(dtheta1>0)then
						SIDE(i)=1
					else
						SIDE(i)=-1
					end if
					FR(1,i)=r_thr
					FR(2,i)=r_thr
				else
					if(dtheta1>0)then
						SIDE(i)=1
						FR(1,i)=CRV(4,i)/fr_inn
						FR(2,i)=CRV(4,i)/fr_out
					else
						SIDE(i)=-1
						FR(1,i)=CRV(4,i)/fr_out
						FR(2,i)=CRV(4,i)/fr_inn
					end if
				end if
			else if(abs(dtheta1)<ds/pBig.AND.abs(dtheta2)>=ds/pBig)then ! left limit diverges and right limit is OK
				if(abs(dtheta2)<ds/r_thr)then
					if(dtheta2>0)then
						SIDE(i)=1
					else
						SIDE(i)=-1
					end if
					FR(1,i)=r_thr
					FR(2,i)=r_thr
				else
					if(dtheta2>0)then
						SIDE(i)=1
						FR(1,i)=CRV(4,i)/fr_inn
						FR(2,i)=CRV(4,i)/fr_out
					else
						SIDE(i)=-1
						FR(1,i)=CRV(4,i)/fr_out
						FR(2,i)=CRV(4,i)/fr_inn
					end if
				end if
			else ! both limits are OK
				if(abs(dtheta1)<ds/r_thr.AND.abs(dtheta2)<ds/r_thr)then ! both radii of curvature are too large
					if(dtheta1>0.AND.dtheta2>0)then
						SIDE(i)=1
					else if(dtheta1<0.AND.dtheta2<0)then
						SIDE(i)=-1
					else
						SIDE(i)=0
					end if
					FR(1,i)=r_thr
					FR(2,i)=r_thr
				else if(abs(dtheta1)>=ds/r_thr.AND.abs(dtheta2)<ds/r_thr)then ! left limit is fine, right limit is too large
					if(dtheta1>0)then
						SIDE(i)=1
						FR(1,i)=CRV(4,i)/fr_inn
						FR(2,i)=CRV(4,i)/fr_out
					else
						SIDE(i)=-1
						FR(1,i)=CRV(4,i)/fr_out
						FR(2,i)=CRV(4,i)/fr_inn
					end if
				else if(abs(dtheta1)<ds/r_thr.AND.abs(dtheta2)>=ds/r_thr)then ! left limit is too large, right limit is fine
					if(dtheta2>0)then
						SIDE(i)=1
						FR(1,i)=CRV(4,i)/fr_inn
						FR(2,i)=CRV(4,i)/fr_out
					else
						SIDE(i)=-1
						FR(1,i)=CRV(4,i)/fr_out
						FR(2,i)=CRV(4,i)/fr_inn
					end if
				else ! both are fine
					if(ds/abs(dtheta1)<=ds/abs(dtheta2))then
						if(dtheta1>0)then
							SIDE(i)=1
							FR(1,i)=CRV(4,i)/fr_inn
							FR(2,i)=CRV(4,i)/fr_out
						else
							SIDE(i)=-1
							FR(1,i)=CRV(4,i)/fr_out
							FR(2,i)=CRV(4,i)/fr_inn
						end if
					else
						if(dtheta2>0)then
							SIDE(i)=1
							FR(1,i)=CRV(4,i)/fr_inn
							FR(2,i)=CRV(4,i)/fr_out
						else
							SIDE(i)=-1
							FR(1,i)=CRV(4,i)/fr_out
							FR(2,i)=CRV(4,i)/fr_inn
						end if
					end if
				end if
			end if
		end do
!		last point
!		for closed curves
		if(lclosed)then
!		computing differential of arc and angle
			dtheta1=dtheta2
			dtheta2=(CRV(3,1)-CRV(3,num_points))
!		paying atention to the branch at pi and the coarseness of the discretization
			if(     dtheta2>=2*pi-2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2-2*pi
			else if(dtheta2<=-2*pi+2*dtheta_max*pi/180.0) then
				dtheta2=dtheta2+2*pi
			else if((( 2*dtheta_max*pi/180.0<dtheta2).AND.( dtheta2<2*pi-2*dtheta_max*pi/180.0)).OR.&
							((-2*pi+2*dtheta_max*pi/180.0<dtheta2).AND.(dtheta2<-2*dtheta_max*pi/180.0))) then
				if(lwarn6) write(*,*) 'WARNING! You may need round the corners or/and increase the number of spline points.'
        lwarn6=.false.
			end if
!		computing where is the inner part of the curve (avoiding divergences) and FR
			if(abs(dtheta1)<ds/pBig.AND.abs(dtheta2)<ds/pBig)then ! both diverge
				SIDE(num_points)=0
				FR(1,num_points)=r_thr
				FR(2,num_points)=r_thr
			else if(abs(dtheta1)>=ds/pBig.AND.abs(dtheta2)<ds/pBig)then ! left limit is OK and right limit diverges
				if(abs(dtheta1)<ds/r_thr)then
					if(dtheta1>0)then
						SIDE(num_points)=1
					else
						SIDE(num_points)=-1
					end if
					FR(1,num_points)=r_thr
					FR(2,num_points)=r_thr
				else
					if(dtheta1>0)then
						SIDE(num_points)=1
						FR(1,num_points)=CRV(4,num_points)/fr_inn
						FR(2,num_points)=CRV(4,num_points)/fr_out
					else
						SIDE(num_points)=-1
						FR(1,num_points)=CRV(4,num_points)/fr_out
						FR(2,num_points)=CRV(4,num_points)/fr_inn
					end if
				end if
			else if(abs(dtheta1)<ds/pBig.AND.abs(dtheta2)>=ds/pBig)then ! left limit diverges and right limit is OK
				if(abs(dtheta2)<ds/r_thr)then
					if(dtheta2>0)then
						SIDE(num_points)=1
					else
						SIDE(num_points)=-1
					end if
					FR(1,num_points)=r_thr
					FR(2,num_points)=r_thr
				else
					if(dtheta2>0)then
						SIDE(num_points)=1
						FR(1,num_points)=CRV(4,num_points)/fr_inn
						FR(2,num_points)=CRV(4,num_points)/fr_out
					else
						SIDE(num_points)=-1
						FR(1,num_points)=CRV(4,num_points)/fr_out
						FR(2,num_points)=CRV(4,num_points)/fr_inn
					end if
				end if
			else ! both limits are OK
				if(abs(dtheta1)<ds/r_thr.AND.abs(dtheta2)<ds/r_thr)then ! both radii of curvature are too large
					if(dtheta1>0.AND.dtheta2>0)then
						SIDE(num_points)=1
					else if(dtheta1<0.AND.dtheta2<0)then
						SIDE(num_points)=-1
					else
						SIDE(num_points)=0
					end if
					FR(1,num_points)=r_thr
					FR(2,num_points)=r_thr
				else if(abs(dtheta1)>=ds/r_thr.AND.abs(dtheta2)<ds/r_thr)then ! left limit is fine, right limit is too large
					if(dtheta1>0)then
						SIDE(num_points)=1
						FR(1,num_points)=CRV(4,num_points)/fr_inn
						FR(2,num_points)=CRV(4,num_points)/fr_out
					else
						SIDE(num_points)=-1
						FR(1,num_points)=CRV(4,num_points)/fr_out
						FR(2,num_points)=CRV(4,num_points)/fr_inn
					end if
				else if(abs(dtheta1)<ds/r_thr.AND.abs(dtheta2)>=ds/r_thr)then ! left limit is too large, right limit is fine
					if(dtheta2>0)then
						SIDE(num_points)=1
						FR(1,num_points)=CRV(4,num_points)/fr_inn
						FR(2,num_points)=CRV(4,num_points)/fr_out
					else
						SIDE(num_points)=-1
						FR(1,num_points)=CRV(4,num_points)/fr_out
						FR(2,num_points)=CRV(4,num_points)/fr_inn
					end if
				else ! both are fine
					if(ds/abs(dtheta1)<=ds/abs(dtheta2))then
						if(dtheta1>0)then
							SIDE(num_points)=1
							FR(1,num_points)=CRV(4,num_points)/fr_inn
							FR(2,num_points)=CRV(4,num_points)/fr_out
						else
							SIDE(num_points)=-1
							FR(1,num_points)=CRV(4,num_points)/fr_out
							FR(2,num_points)=CRV(4,num_points)/fr_inn
						end if
					else
						if(dtheta2>0)then
							SIDE(num_points)=1
							FR(1,num_points)=CRV(4,num_points)/fr_inn
							FR(2,num_points)=CRV(4,num_points)/fr_out
						else
							SIDE(num_points)=-1
							FR(1,num_points)=CRV(4,num_points)/fr_out
							FR(2,num_points)=CRV(4,num_points)/fr_inn
						end if
					end if
				end if
			end if
!		for open curves
		else
!		computing differential of arc and angle
			dtheta1=dtheta2
!		computing where is the inner part of the curve (avoiding divergences) and FR
			if(abs(dtheta1)<ds/pBig)then
				SIDE(num_points)=0
				FR(1,num_points)=r_thr
				FR(2,num_points)=r_thr
			else if(abs(dtheta1)<ds/r_thr)then
				if(dtheta1>0)then
					SIDE(num_points)=1
				else
					SIDE(num_points)=-1
				end if
				FR(1,num_points)=r_thr
				FR(2,num_points)=r_thr
			else
				if(dtheta1>0)then
					SIDE(num_points)=1
					FR(1,num_points)=CRV(4,num_points)/fr_inn
					FR(2,num_points)=CRV(4,num_points)/fr_out
				else
					SIDE(num_points)=-1
					FR(1,num_points)=CRV(4,num_points)/fr_out
					FR(2,num_points)=CRV(4,num_points)/fr_inn
				end if
			end if
		end if
		do L=1,2 ! left and right hand side
!		-------------------------------
!		Computing SECTIONS of the curve
!		-------------------------------
!		computing number of sections
			num_sec=1
!		first point
			if(FR(L,1)==r_thr)then
				tag_sec1=1
			else
				tag_sec1=0
			end if
!		next points
			do i=2,num_points
				if(FR(L,i)==r_thr)then
					tag_sec2=1
				else
					tag_sec2=0
				end if
				if(tag_sec2/=tag_sec1)then
					num_sec=num_sec+1
					tag_sec1=tag_sec2
				end if
			end do
!		saving the first and last point of each section,and type of section
			if(allocated(SEC)) deallocate(SEC)
			if(allocated(HIGH)) deallocate(HIGH)
			allocate(SEC(2,num_sec),HIGH(num_sec),Stat=idum)
			if(idum/=0) then
				idum=MessageBoxQQ('Memory allocation for SEC,HIGH failed!'C,'Generate expansions'C,MB$OK.or.MB$IconExclamation)
				return
			end if
!		first point of first section
			num_sec=1
			SEC(1,1)=1
!		computation of boundaries of sections
			if(FR(L,1)==r_thr)then
				tag_sec1=1
				HIGH(1)=.TRUE.
			else
				tag_sec1=0
				HIGH(1)=.FALSE.
			end if
			do i=2,num_points
				if(FR(L,i)==r_thr)then 
					tag_sec2=1
				else
					tag_sec2=0
				end if
				if(tag_sec2/=tag_sec1)then
					if(tag_sec1==1)then
						HIGH(num_sec+1)=.FALSE.
					else
						HIGH(num_sec+1)=.TRUE.
					end if
					SEC(2,num_sec)= i-1
					SEC(1,num_sec+1)=i
					num_sec=num_sec+1
					tag_sec1=tag_sec2
				end if
			end do
!		last point of last section
			SEC(2,num_sec)=num_points
!		-------------------------------------------
!		Computing BLACK SPOTS in the "LOW" sections
!		-------------------------------------------
!		computing number of black spots in each section
			if(allocated(NUM_BLSPOT)) deallocate(NUM_BLSPOT)
			allocate(NUM_BLSPOT(num_sec),Stat=idum)
			if(idum/=0) then
				idum=MessageBoxQQ('Memory allocation for NUM_BLSPOT failed!'C,'Generate expansions'C,MB$OK.or.MB$IconExclamation)
				return
			end if
			do i=1,num_sec
!		if it is a "high" section,it has no black spots
				if(HIGH(i))then
					NUM_BLSPOT(i)=0
!		special cases
				else if((SEC(2,i)-SEC(1,i))==0)then
					if(i/=1.AND.i/=num_sec)then
						NUM_BLSPOT(i)=0
					else
						if(lclosed)then
							if(i==1.AND.HIGH(num_sec)) NUM_BLSPOT(i)=0
							if(i==1.AND.not(HIGH(num_sec))) NUM_BLSPOT(i)=1
							if(i==num_sec.AND.HIGH(1)) NUM_BLSPOT(i)=0
							if(i==num_sec.AND.not(HIGH(1))) NUM_BLSPOT(i)=1
						else
							NUM_BLSPOT(i)=0
						end if
					end if
!		normal case
				else
!		first point
					num_blsp=0
					dFR=abs(FR(L,SEC(1,i)+1)-FR(L,SEC(1,i)))
					if(abs(dFR)>ds*tan(alphaR*pi/180.0))then
						tag_blsp1=1
						num_blsp=1
						tag_firstblsp=.false.
					else
						tag_blsp1=0
						tag_firstblsp=.true.
					end if
!		next points
					do j=SEC(1,i)+1,SEC(2,i)-1
						dFR=abs(FR(L,j+1)-FR(L,j))
						if(abs(dFR)>ds*tan(alphaR*pi/180.0))then
							tag_blsp2=1
						else
							tag_blsp2=0
						end if
						if(tag_blsp2/=tag_blsp1)then
							if(tag_blsp1==0) then
								if(tag_firstblsp)then
									num_blsp=num_blsp+1
									tag_firstblsp=.false.
								else
									if((j-pointdown)>=securityN) num_blsp=num_blsp+1 ! atention: 2 blackspots very close to each other are joined in one
								end if
							else
								pointdown=j
							end if
							tag_blsp1=tag_blsp2
						end if
					end do
!		(only if it is closed), last point of the last section,when the first section is "low"
					if(lclosed.AND.i==num_sec.AND.not(HIGH(1)))then
						dFR=abs(FR(L,1)-FR(L,num_points))
						if(abs(dFR)>ds*tan(alphaR*pi/180.0))then
							tag_blsp2=1
						else
							tag_blsp2=0
						end if
						if(tag_blsp2/=tag_blsp1)then
							if(tag_blsp1==0) then
								if(tag_firstblsp)then
									num_blsp=num_blsp+1
									tag_firstblsp=.false.
								else
									if((j-pointdown)>=securityN) num_blsp=num_blsp+1
								end if
							else
								pointdown=j
							end if
							tag_blsp1=tag_blsp2
						end if
					end if
					NUM_BLSPOT(i)=num_blsp
				end if
			end do
!		saving the first and last point of each black spot
!		total number of black spots
			num_blspT=0
			do i=1,num_sec
				num_blspT=num_blspT+NUM_BLSPOT(i)
			end do
			if(allocated(BL_SP)) deallocate(BL_SP)
			allocate(BL_SP(2,max(num_blspT,1)),Stat=idum)
			if(idum/=0) then
				idum=MessageBoxQQ('Memory allocation for BL_SP failed!'C,'Generate expansions'C,MB$OK.or.MB$IconExclamation)
				return
			end if
!		saving the boundaries of the black spots
			num_blsp=0
			do i=1,num_sec
				if(NUM_BLSPOT(i)/=0)then
!		special cases
					if((SEC(2,i)-SEC(1,i))==0)then
						if(i==1)then
							num_blsp=1
							BL_SP(1,i)=1
							BL_SP(2,i)=1
						else if(i==num_sec)then
							BL_SP(1,max(num_blspT,1))=num_points
							BL_SP(2,max(num_blspT,1))=num_points
						end if
!		normal cases
					else if((SEC(2,i)-SEC(1,i))/=0)then
!		first point
						dFR=abs(FR(L,SEC(1,i)+1)-FR(L,SEC(1,i)))
						if(abs(dFR)>ds*tan(alphaR*pi/180.0))then
							tag_blsp1=1
							num_blsp=num_blsp+1
							BL_SP(1,num_blsp)=SEC(1,i)
							tag_firstblsp=.false.
						else
							tag_blsp1=0
							tag_firstblsp=.true.
						end if
						tag_blsp2=tag_blsp1 ! see the last comment of this section
!		next points
						do j=SEC(1,i)+1,SEC(2,i)-1
							dFR=abs(FR(L,j+1)-FR(L,j))
							if(abs(dFR)>ds*tan(alphaR*pi/180.0))then
								tag_blsp2=1
							else
								tag_blsp2=0
							end if
							if(tag_blsp2/=tag_blsp1)then
								if(tag_blsp1==0) then
									if(tag_firstblsp)then
										num_blsp=num_blsp+1
										if((j-SEC(1,i))>=securityN)then
											BL_SP(1,num_blsp)=j
										else
											BL_SP(1,num_blsp)=SEC(1,i)
										end if
										tag_firstblsp=.false.
									else
										if((j-pointdown)>=securityN)then
											num_blsp=num_blsp+1
											BL_SP(1,num_blsp)=j
										end if
									end if
								else
									pointdown=j
									BL_SP(2,num_blsp)=j-1
								end if
								tag_blsp1=tag_blsp2
							end if
						end do
!		(only if it is closed), last point of the last section,when the first section is "low"
						if(lclosed.AND.i==num_sec.AND.not(HIGH(1)))then
							dFR=abs(FR(L,1)-FR(L,num_points))
							if(abs(dFR)>ds*tan(alphaR*pi/180.0))then
								tag_blsp2=1
							else
								tag_blsp2=0
							end if
							if(tag_blsp2/=tag_blsp1)then
								if(tag_blsp1==0) then
									if(tag_firstblsp)then
										num_blsp=num_blsp+1
										if((j-SEC(1,i))>=securityN)then
											BL_SP(1,num_blsp)=j
										else
											BL_SP(1,num_blsp)=SEC(1,i)
										end if
										tag_firstblsp=.false.
									else
										if((j-pointdown)>=securityN)then
											num_blsp=num_blsp+1
											BL_SP(1,num_blsp)=j
										end if
									end if
								else
									pointdown=j
									BL_SP(2,num_blsp)=j-1
								end if
								tag_blsp1=tag_blsp2
							end if
						end if
!		if, when exiting a section, tag_blsp2 is 1, then the section ends in black
						if(tag_blsp2==1)then
							BL_SP(2,num_blsp)=SEC(2,i)
						end if
!		if, when exiting a section, it ends in white, but only a few points, then the black spot is prolongated till the end of the section
						if((SEC(2,i)-BL_SP(2,num_blsp))<securityN)then
							BL_SP(2,num_blsp)=SEC(2,i)
						end if
					end if
				end if
			end do
!		---------------------------------------------------------------------
!		RENUMBERING the curve to locate the cut in the less "dangerous" point
!		---------------------------------------------------------------------
			if(lclosed.AND.renumbering_tag)then ! renumbering only for closed curves which I have not yet renumbered
				renumbering_tag=.false.
				length_renumbering_high=0
				length_renumbering_low=0
				sec_renumbering_high=0
				sec_renumbering_low=0
				previousblsp_renumbering=0
				num_blsp=0
				do i=1,num_sec
					if(HIGH(i))then ! looking for the longest HIGH section
						length=0.0
						do j=SEC(1,i),SEC(2,i)-1
							length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
						end do
						if(i==1.AND.HIGH(num_sec).AND.num_sec>1)then
							do j=SEC(1,num_sec),SEC(2,num_sec)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
						end if
						if(length>length_renumbering_high)then
							length_renumbering_high=length
							sec_renumbering_high=i
						end if
					else ! looking for the longest LOW and WHITE section
						if(NUM_BLSPOT(i)==0)then ! it is a completely white section
							length=0.0
							do j=SEC(1,i),SEC(2,i)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							if(i==1.AND.not(HIGH(num_sec)).AND.num_sec>1)then
								do j=max(SEC(1,num_sec),BL_SP(2,max(num_blspT,1))+1),SEC(2,num_sec)-1
									length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
							end if
							if(length>length_renumbering_low)then
								length_renumbering_low=length
								sec_renumbering_low=i
								previousblsp_renumbering=num_blsp
							end if
						else ! it is not a completely white section
							do j=1,NUM_BLSPOT(i)
								num_blsp=num_blsp+1
								if(num_blsp>1)then ! it is after the first blackspot and before the last blackspot of the section
									length=0.0
									do k=max(SEC(1,i),BL_SP(2,num_blsp-1)+1),BL_SP(1,num_blsp)-2
										length=length+sqrt((CRV(1,k+1)-CRV(1,k))**2+(CRV(2,k+1)-CRV(2,k))**2)
									end do
								else ! it is before the first blackspot
									length=0.0
									do k=SEC(1,i),BL_SP(1,num_blsp)-2
										length=length+sqrt((CRV(1,k+1)-CRV(1,k))**2+(CRV(2,k+1)-CRV(2,k))**2)
									end do
									if(i==1.AND.not(HIGH(num_sec)).AND.BL_SP(1,1)>1)then
										do k=max(SEC(1,num_sec),BL_SP(2,max(num_blspT,1))+1),num_points-1
											length=length+sqrt((CRV(1,k+1)-CRV(1,k))**2+(CRV(2,k+1)-CRV(2,k))**2)
										end do
									end if
								end if
								if(length>length_renumbering_low)then
									length_renumbering_low=length
									sec_renumbering_low=i
									previousblsp_renumbering=num_blsp-1
								end if
							end do
							length=0.0 ! this is between the last blackspot of a section and the end of the section
							do j=BL_SP(2,num_blsp)+1,SEC(2,i)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							if(length>length_renumbering_low)then
								length_renumbering_low=length
								sec_renumbering_low=i
								previousblsp_renumbering=num_blsp
							end if
						end if
					end if
				end do
				if(length_renumbering_high>=length_renumbering_low)then ! the longest section was HIGH
					if((1<sec_renumbering_high.AND.sec_renumbering_high<num_sec).OR.(1==sec_renumbering_high.AND.not(HIGH(num_sec))).OR.&
																		(num_sec==sec_renumbering_high.AND.not(HIGH(1))))then
						point_renumbering=floor((SEC(1,sec_renumbering_high)+SEC(2,sec_renumbering_high))/2.0)
					else
						point_renumbering=floor((SEC(1,num_sec)+num_points+SEC(2,1))/2.0)
						if(point_renumbering>num_points) point_renumbering=point_renumbering-num_points
					end if
				else if((length_renumbering_low-length_renumbering_high)/length_renumbering_low<renumbering_parameter)then ! the longest section was LOW, but only slightly longer than the longest HIGH section
					if((1<sec_renumbering_high.AND.sec_renumbering_high<num_sec).OR.(1==sec_renumbering_high.AND.not(HIGH(num_sec))).OR.&
																		(num_sec==sec_renumbering_high.AND.not(HIGH(1))))then
						point_renumbering=floor((SEC(1,sec_renumbering_high)+SEC(2,sec_renumbering_high))/2.0)
					else
						point_renumbering=floor((SEC(1,num_sec)+num_points+SEC(2,1))/2.0)
						if(point_renumbering>num_points) point_renumbering=point_renumbering-num_points
					end if
				else ! the longest section was LOW
					if(previousblsp_renumbering>0.AND.previousblsp_renumbering<num_blspT)then ! intermediate
						point_renumbering=floor((max(SEC(1,sec_renumbering_low),BL_SP(2,previousblsp_renumbering)+1)+min(SEC(2,sec_renumbering_low),BL_SP(1,previousblsp_renumbering+1)-1))/2.0)
					else if(previousblsp_renumbering>0.AND.previousblsp_renumbering==num_blspT)then ! after the last blackspot
						point_renumbering=floor((max(SEC(1,sec_renumbering_low),BL_SP(2,previousblsp_renumbering)+1)+SEC(2,sec_renumbering_low))/2.0)
					else if(previousblsp_renumbering==0.0)then ! before the first blackspot
						if(num_blspT>0)then ! there is at least one blackspot
							if(HIGH(num_sec).OR.BL_SP(2,num_blspT)==num_points)then
								point_renumbering=floor((SEC(1,1)+min(SEC(2,1),BL_SP(1,1)-1))/2.0)
							else
								point_renumbering=floor((max(SEC(1,num_sec),BL_SP(2,num_blspT)+1)+num_points+min(SEC(2,1),BL_SP(1,1)-1))/2.0)
							end if
						else ! there are no blackspots
							if(HIGH(num_sec))then
								point_renumbering=floor((SEC(1,1)+SEC(2,1))/2.0)
							else
								if(num_sec>1)then ! more than one section
									point_renumbering=floor((SEC(1,num_sec)+num_points+SEC(2,1))/2.0)
									if(point_renumbering>num_points) point_renumbering=point_renumbering-num_points
								else ! only one section
									point_renumbering=floor((SEC(1,1)+SEC(2,1))/2.0)
								end if
							end if
						end if
					end if
				end if
				do i=1,num_points
					j=i+point_renumbering-1
					if(j>num_points) j=j-num_points
					CRVrenumbered(1:4,i)=CRV(1:4,j)
				end do
				go to 10
			end if
!		---------------------------------------------------------
!		Computing FR (2nd time: REGULARIZATION OF "LOW" SECTIONS)
!		---------------------------------------------------------
			num_blsp=1
			do i=1,num_sec
				if(NUM_BLSPOT(i)/=0)then
					do j=1,NUM_BLSPOT(i)
!		if the black spot begins with the section
						if(BL_SP(1,num_blsp)==SEC(1,i))then
!		in a first section of closed curve with a last "down" section
							if(i==1.AND.lclosed.AND.not(HIGH(num_sec)))then
!		this first section is not completely black
								if(BL_SP(2,1)/=SEC(2,1))then
!		if the last section ends in white
									if(BL_SP(2,num_blspT)/=num_points)then
										in1=max(BL_SP(2,num_blspT)+1,SEC(1,num_sec))
										if(num_blspT>1)then
											in2=min(BL_SP(1,2)-1,SEC(2,1))
										else
											in2=SEC(2,1)
										end if
										in3=floor((in1+num_points)/2.0)
										in4=floor((BL_SP(2,1)+1+in2)/2.0)
										if(in3==in4)then
											in3=floor((in1+3*num_points)/4.0)
											in4=floor((3*(BL_SP(2,1)+1)+in2)/4.0)
										end if
										do k=1,num_points-in3
											FR(L,in3+k)=FR(L,in3)+(FR(L,in4)-FR(L,in3))*k/(in4+num_points-in3)
										end do
										do k=1,in4-1
											FR(L,in4-k)=FR(L,in4)-(FR(L,in4)-FR(L,in3))*k/(in4+num_points-in3)
										end do
!		if the last section ends in black but it is not completely black
									else if(BL_SP(1,num_blspT)/=SEC(1,num_sec))then
										in1=max(BL_SP(2,num_blspT-1)+1,SEC(1,num_sec))
										in2=min(BL_SP(1,2)-1,SEC(2,1))
										in3=floor((in1+BL_SP(1,num_blspT)-1)/2.0)
										in4=floor((BL_SP(2,1)+1+in2)/2.0)
										if(in3==in4)then
											in3=floor((in1+3*(BL_SP(1,num_blspT)-1))/4.0)
											in4=floor((3*(BL_SP(2,1)+1)+in2)/4.0)
										end if
										do k=1,num_points-in3
											FR(L,in3+k)=FR(L,in3)+(FR(L,in4)-FR(L,in3))*k/(in4+num_points-in3)
										end do
										do k=1,in4-1
											FR(L,in4-k)=FR(L,in4)-(FR(L,in4)-FR(L,in3))*k/(in4+num_points-in3)
										end do
!		if the last section is completely black
									else
										in1=BL_SP(2,num_blsp)+1
										dFR=FR(L,in1+1)-FR(L,in1)
										do k=1,in1-1
											FR(L,in1-k)=FR(L,in1)-k*dFR
											if(FR(L,in1-k)<=0.0)then
												FR(L,in1-k)=FR(L,in1-k+1)
											end if
										end do
										do k=1,num_points-SEC(1,num_sec)+1
											FR(L,num_points+1-k)=FR(L,1)-k*dFR
											if(FR(L,num_points+1-k)<=0.0)then
												if(k/=1)then
													FR(L,num_points+1-k)=FR(L,num_points+1-k+1)
												else
													FR(L,num_points+1-k)=FR(L,1)
												end if
											end if
										end do
									end if
!		this first section is completely black
								else if(BL_SP(2,1)==SEC(2,1))then
!		if the last section ends in white
									if(BL_SP(2,num_blspT)/=num_points)then
										dFR=FR(L,1)-FR(L,num_points)
										do k=1,SEC(2,1)
											FR(L,k)=FR(L,num_points)+k*dFR
											if(FR(L,k)<=0.0)then
												if(k/=1)then
													FR(L,k)=FR(L,k-1)
												else
													FR(L,k)=FR(L,num_points)
												end if
											end if
										end do
!		if the last section ends in black but it is not completely black
									else if(BL_SP(1,num_blspT)/=SEC(1,num_sec))then
										in1=BL_SP(1,num_blspT)-1
										dFR=FR(L,in1+1)-FR(L,in1)
										do k=1,num_points-in1
											FR(L,in1+k)=FR(L,in1)+k*dFR
											if(FR(L,in1+k)<=0.0)then
												FR(L,in1+k)=FR(L,in1+k-1)
											end if
										end do
										do k=1,SEC(2,1)
											FR(L,k)=FR(L,num_points)+k*dFR
											if(FR(L,k)<=0.0)then
												if(k/=1)then
													FR(L,k)=FR(L,k-1)
												else
													FR(L,k)=FR(L,num_points)
												end if
											end if
										end do
!		if the last section is completely black
									else
										if(lwarn7) write(*,*) 'WARNING! Regularization was not possible.' ! (closed curve; bad last section+bad first section)
										if(lwarn7) write(*,*) 'You may need round the corners or/and increase the number of spline points.'
                    lwarn7=.false.
									end if
								end if
!		in a NOT(the first section of closed curves with a last "down" section)
							else
!		if it is not a completely black section
								if(BL_SP(2,num_blsp)/=SEC(2,i))then
									in1=BL_SP(2,num_blsp)+1
									if(in1==num_points)then
										dFR=FR(L,1)-FR(L,num_points)
									else
										dFR=FR(L,in1+1)-FR(L,in1)
									end if
									do k=1,in1-SEC(1,i)
										FR(L,in1-k)=FR(L,in1)-k*dFR
										if(FR(L,in1-k)<=0.0)then
											FR(L,in1-k)=FR(L,in1-k+1)
										end if
									end do
!		if it is a completely black section
								else
!		it is not the last section
									if(i/=num_sec)then
										if(lwarn7) write(*,*) 'WARNING! Regularization was not possible.' ! (bad notlast section)
										if(lwarn7) write(*,*) 'You may need round the corners or/and increase the number of spline points.'
                    lwarn7=.false.
!		it is the last section
									else
!		closed curve with first section low
										if(lclosed.AND.not(HIGH(1)))then
!		first section does not begin with black spot
											if(BL_SP(1,1)/=1)then
												dFR=FR(L,2)-FR(L,1)
												do k=1,num_points+1-SEC(1,i)
													FR(L,num_points+1-k)=FR(L,1)-k*dFR
													if(FR(L,num_points+1-k)<=0.0)then
														if(k/=1)then
															FR(L,num_points+1-k)=FR(L,num_points+1-k+1)
														else
															FR(L,num_points+1-k)=FR(L,1)
														end if
													end if
												end do
											end if
!		NOT(closed curve with first section low)
										else
											if(lwarn7) write(*,*) 'WARNING! Regularization was not possible.' ! (bad last section)
											if(lwarn7) write(*,*) 'You may need round the corners or/and increase the number of spline points.'
                      lwarn7=.false.
										end if
									end if
								end if
							end if
!		if the black spot doesn't begin with the section and ends with the section
						else if(BL_SP(2,num_blsp)==SEC(2,i))then
!		in a last section of closed curves with a first "down" section
							if(i==num_sec.AND.lclosed.AND.not(HIGH(1)))then
!		first section starts white
								if(BL_SP(1,1)/=1)then
									if(num_blspT>1)then
										in1=max(BL_SP(2,num_blspT-1)+1,SEC(1,num_sec))
									else
										in1=SEC(1,num_sec)
									end if
									in2=min(BL_SP(1,1)-1,SEC(2,1))
									in3=floor((in1+BL_SP(1,num_blspT)-1)/2.0)
									in4=floor((1+in2)/2.0)
									if(in3==in4)then
										in3=floor((in1+3*(BL_SP(1,num_blspT)-1))/4.0)
										in4=floor((3*1+in2)/4.0)
									end if
									do k=1,num_points-in3
										FR(L,in3+k)=FR(L,in3)+(FR(L,in4)-FR(L,in3))*k/(in4+num_points-in3)
									end do
									do k=1,in4-1
										FR(L,k)=FR(L,num_points)+(FR(L,in4)-FR(L,in3))*k/(in4+num_points-in3)
									end do
								end if
!		in a NOT(the last section of closed curves with a first "down" section)
							else
								in2=BL_SP(1,num_blsp)
								dFR=FR(L,in2)-FR(L,in2-1)
								do k=1,SEC(2,i)-in2+1
									FR(L,in2-1+k)=FR(L,in2-1)+k*dFR
									if(FR(L,in2-1+k)<=0.0)then
										FR(L,in2-1+k)=FR(L,in2-1+k-1)
									end if
								end do
							end if
!		intermediate black spot (doesn't begin nor ends with the section)
						else
!		computing in1 and in2
							if(num_blsp/=1)then ! it is not the first blackspot
								in1=max(BL_SP(2,num_blsp-1)+1,SEC(1,i))
							else ! it is the first blackspot
								if(SEC(1,i)>1)then ! it is not included in the first section
									in1=SEC(1,i)
								else ! it is included in the first section
									if(.NOT.lclosed)then ! open curve
										in1=SEC(1,i)
									else ! closed curve
										if(HIGH(num_sec))then ! last section is high
											in1=SEC(1,i)
										else ! last section is low
											if(BL_SP(2,num_blspT)==num_points)then ! last blackpoint ends at the last point
												in1=SEC(1,i)
											else
												in1=max(BL_SP(2,num_blspT)+1,SEC(1,num_sec))
											end if
										end if
									end if
								end if
							end if
							if(num_blsp/=num_blspT)then ! it is not the last blackspot
								in2=min(BL_SP(1,num_blsp+1)-1,SEC(2,i))
							else ! it is the last blackspot
								if(SEC(2,i)<num_points)then ! it is not included in the last section
									in2=SEC(2,i)
								else ! it is included in the last section
									if(.NOT.lclosed)then ! open curve
										in2=SEC(2,i)
									else ! closed curve
										if(HIGH(1))then ! first section is high
											in2=SEC(2,i)
										else ! first section is low
											if(BL_SP(1,1)==1)then ! first blackpoint starts at the first point
												in2=SEC(2,i)
											else
												in2=min(BL_SP(1,1)-1,SEC(2,1))
											end if
										end if
									end if
								end if
							end if
!		computing in3 and in4
							if(in1<BL_SP(1,num_blsp))then
								in3=floor((in1+BL_SP(1,num_blsp)-1)/2.0)
							else
								in3=floor((in1+num_points+BL_SP(1,num_blsp)-1)/2.0)
								if(in3>num_points) in3=in3-num_points
							end if
							if(in2>BL_SP(2,num_blsp))then
								in4=floor((BL_SP(2,num_blsp)+1+in2)/2.0)
							else
								in4=floor((BL_SP(2,num_blsp)+1+num_points+in2)/2.0)
								if(in4>num_points) in4=in4-num_points
							end if
							if(in3==in4)then ! correction in case in3 is equal to in4
								if(in3<BL_SP(1,num_blsp))then
									in3=floor((in3+BL_SP(1,num_blsp)-1)/2.0)
									in4=floor((BL_SP(2,num_blsp)+1+num_points+in4)/2.0)
									if(in4>num_points) in4=in4-num_points
								else
									in4=floor((BL_SP(2,num_blsp)+1+in4)/2.0)
									in3=floor((in3+num_points+BL_SP(1,num_blsp)-1)/2.0)
									if(in3>num_points) in3=in3-num_points
								end if
							end if
!		performing the interpolations
							if(in3<BL_SP(1,num_blsp).AND.in4>BL_SP(2,num_blsp))then ! in3 and in4 are in the "expected" positions
								do k=1,in4-in3-1
									FR(L,in3+k)=FR(L,in3)+(FR(L,in4)-FR(L,in3))*k/(in4-in3)
								end do
							else if(in3<BL_SP(1,num_blsp).AND.in4<BL_SP(2,num_blsp))then ! in4 "was bigger" than num_points
								do k=1,num_points-in3
									FR(L,in3+k)=FR(L,in3)+(FR(L,in4)-FR(L,in3))*k/(num_points+in4-in3)
								end do
								do k=1,in4-1
									FR(L,k)=FR(L,num_points)+(FR(L,in4)-FR(L,in3))*k/(num_points+in4-in3)
								end do
							else if(in3>BL_SP(1,num_blsp).AND.in4>BL_SP(2,num_blsp))then ! in3 "was smaller" than 1
								do k=1,num_points-in3
									FR(L,in3+k)=FR(L,in3)+(FR(L,in4)-FR(L,in3))*k/(num_points+in4-in3)
								end do
								do k=1,in4-1
									FR(L,k)=FR(L,num_points)+(FR(L,in4)-FR(L,in3))*k/(num_points+in4-in3)
								end do
							end if
						end if
						num_blsp=num_blsp+1
					end do
				end if
			end do
!		---------------------------------------------------------
!		Computing FR (3rd time: INTERPOLATION OF "HIGH" SECTIONS)
!		---------------------------------------------------------
			do i=1,num_sec
				if(HIGH(i))then
!		for open curves
					if(.NOT.lclosed)then
!		there is only one section and it is "high"
						if(SEC(1,i)==1.AND.SEC(2,i)==num_points)then
!		first and last FR
							r_ini=r_inii(L)
							r_fin=r_fini(L)
!		compute length of the section
							length=0.0
							length=length+sqrt((CRV(1,2)-CRV(1,1))**2+(CRV(2,2)-CRV(2,1))**2)
							do j=1,num_points-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							length=length+sqrt((CRV(1,num_points)-CRV(1,num_points-1))**2+(CRV(2,num_points)-CRV(2,num_points-1))**2)
							if (r_ini<=0.0.OR.r_fin<=0.0) then
								if(r_ini<=0.0) r_ini=length/length_fr_def
								if(r_fin<=0.0) r_fin=length/length_fr_def
							end if
							if(length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
								r_fin=r_ini
							end if
!		computing the divergence angle to be used
							sum=0.0
							do j=1,num_points
								sum=sum+SIDE(j)
							end do
							sum=sum/(num_points)
							betaI=big_betaI
							if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI ! if l=1 (we interpole the left part) and sum<-0.5
!										(the center is in the right part) we are in the outer part of the curve and we have to use the small divergence angle
!										(small_betaI) (in case of 'bow' interpolation)
!		linear interpolation
							if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
								do k=1,num_points
									FR(L,k)=r_ini+k*(r_fin-r_ini)/(num_points+1)
								end do
!		'bow' interpolation
							else
							!!!!!!??????	ds=length/(num_points+1)
								ds_perp=ds*tan(betaI*pi/180.0)
								k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
								do k=1,k_thr
									FR(L,k)=r_ini+k*ds_perp
								end do
!		second half
								do k=1,num_points-k_thr
									FR(L,num_points+1-k)=r_fin+k*ds_perp
								end do
							end if
!		the first section is "high" but there are more sections
						else if(SEC(1,i)==1.AND.SEC(2,i)/=num_points)then
!		first and last FR
							r_ini=r_inii(L)
							r_fin=FR(L,SEC(1,i+1))
!		compute length of the section
							length=0.0
							length=length+sqrt((CRV(1,2)-CRV(1,1))**2+(CRV(2,2)-CRV(2,1))**2)
							do j=SEC(1,i),SEC(2,i)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							length=length+sqrt((CRV(1,SEC(2,i)+1)-CRV(1,SEC(2,i)))**2+(CRV(2,SEC(2,i)+1)-CRV(2,SEC(2,i)))**2)
!		trying with "external" interpolation
							if(r_ini>0.AND.length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0)) then
								in2=floor((SEC(1,i+1)+SEC(2,i+1))/2.0)
								lengthfragment=0.0
								do j=SEC(1,i+1),in2-1
									lengthfragment=lengthfragment+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								if(length+lengthfragment>=abs(FR(L,in2)-r_ini)/tan(alphaR*pi/180.0)) then
									in1=0
									do k=1,in2-in1-1
										FR(L,in1+k)=r_ini+(FR(L,in2)-r_ini)*k/(in2-in1)
									end do
								else
									r_ini=r_fin
								end if
							end if
							if(r_ini<=0) r_ini=r_fin
!		with "internal interpolation"
							if((r_ini==r_fin).OR.length>=abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
!		computing the divergence angle to be used
								sum=0.0
								do j=SEC(1,i),SEC(2,i)
									sum=sum+SIDE(j)
								end do
								sum=sum/(SEC(2,i)-SEC(1,i)+1)
								betaI=big_betaI
								if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
								if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
									do k=1,SEC(2,i)-SEC(1,i)+1
										FR(L,SEC(1,i)-1+k)=r_ini+k*(r_fin-r_ini)/(SEC(2,i)-SEC(1,i)+2)
									end do
!		'bow' interpolation
								else
								!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+2)
									ds_perp=ds*tan(betaI*pi/180.0)
									k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
									do k=1,k_thr
										FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
									end do
!		second half
									do k=1,SEC(2,i)+1-SEC(1,i)-k_thr
										FR(L,SEC(2,i)+1-k)=r_fin+k*ds_perp
									end do
								end if
							end if
!		the last section is "high" but there are more sections
						else if(SEC(1,i)/=1.AND.SEC(2,i)==num_points)then
!		first and last FR
							r_ini=FR(L,SEC(2,i-1))
							r_fin=r_fini(L)
!		compute length of the section
							length=0.0
							length=length+sqrt((CRV(1,SEC(1,i))-CRV(1,SEC(1,i)-1))**2+(CRV(2,SEC(1,i))-CRV(2,SEC(1,i)-1))**2)
							do j=SEC(1,i),SEC(2,i)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							length=length+sqrt((CRV(1,num_points)-CRV(1,num_points-1))**2+(CRV(2,num_points)-CRV(2,num_points-1))**2)
!		trying with "external" interpolation
							if(r_fin>0.AND.length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0)) then
								in1=floor((SEC(1,i-1)+SEC(2,i-1))/2.0)
								lengthfragment=0.0
								do j=in1,SEC(2,i-1)-1
									lengthfragment=lengthfragment+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								if(length+lengthfragment>=abs(r_fin-FR(L,in1))/tan(alphaR*pi/180.0)) then
									in2=num_points+1
									do k=1,in2-in1-1
										FR(L,in1+k)=FR(L,in1)+(r_fin-FR(L,in1))*k/(in2-in1)
									end do
								else
									r_fin=r_ini
								end if
							end if
							if(r_fin<=0) r_fin=r_ini
!		with "internal interpolation"
							if((r_fin==r_ini).OR.length>=abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
!		computing the divergence angle to be used
								sum=0.0
								do j=SEC(1,i),SEC(2,i)
									sum=sum+SIDE(j)
								end do
								sum=sum/(SEC(2,i)-SEC(1,i)+1)
								betaI=big_betaI
								if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
								if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
									do k=1,SEC(2,i)-SEC(1,i)+1
										FR(L,SEC(1,i)-1+k)=r_ini+k*(r_fin-r_ini)/(SEC(2,i)-SEC(1,i)+2)
									end do
!		'bow' interpolation
								else
								!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+2)
									ds_perp=ds*tan(betaI*pi/180.0)
									k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
									do k=1,k_thr
										FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
									end do
!		second half
									do k=1,SEC(2,i)+1-SEC(1,i)-k_thr
										FR(L,SEC(2,i)+1-k)=r_fin+k*ds_perp
									end do
								end if
							end if
!		it is a central section
						else
!		first and last FR
							r_ini=FR(L,SEC(2,i-1))
							r_fin=FR(L,SEC(1,i+1))
!		compute length of the section
							length=0.0
							length=length+sqrt((CRV(1,SEC(1,i))-CRV(1,SEC(1,i)-1))**2+(CRV(2,SEC(1,i))-CRV(2,SEC(1,i)-1))**2)
							do j=SEC(1,i),SEC(2,i)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							length=length+sqrt((CRV(1,SEC(2,i)+1)-CRV(1,SEC(2,i)))**2+(CRV(2,SEC(2,i)+1)-CRV(2,SEC(2,i)))**2)
!		"external" interpolation
							if(length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
								in1=floor((SEC(1,i-1)+SEC(2,i-1))/2.0)
								in2=floor((SEC(1,i+1)+SEC(2,i+1))/2.0)
								do k=1,in2-in1-1
									FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(in2-in1)
								end do
!		"internal" interpolation
							else
!		computing the divergence angle to be used
								sum=0.0
								do j=SEC(1,i),SEC(2,i)
									sum=sum+SIDE(j)
								end do
								sum=sum/(SEC(2,i)-SEC(1,i)+1)
								betaI=big_betaI
								if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
								if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
									do k=1,SEC(2,i)-SEC(1,i)+1
										FR(L,SEC(1,i)-1+k)=r_ini+k*(r_fin-r_ini)/(SEC(2,i)-SEC(1,i)+2)
									end do
!		'bow' interpolation
								else
								!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+2)
									ds_perp=ds*tan(betaI*pi/180.0)
									k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
									do k=1,k_thr
										FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
									end do
!		second half
									do k=1,SEC(2,i)+1-SEC(1,i)-k_thr
										FR(L,SEC(2,i)+1-k)=r_fin+k*ds_perp
									end do
								end if
							end if
						end if
!		for closed curves
					else if(lclosed)then
!		it is the first section
						if(i==1)then
!		only for last section "low"
							if(not(HIGH(num_sec)))then
!		determine first and last FR
								r_ini=FR(L,SEC(2,num_sec))
								r_fin=FR(L,SEC(1,i+1))
!		compute length of the section
								length=0.0
								length=length+sqrt((CRV(1,1)-CRV(1,num_points))**2+(CRV(2,1)-CRV(2,num_points))**2)
								do j=SEC(1,i),SEC(2,i)-1
									length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								length=length+sqrt((CRV(1,SEC(2,i)+1)-CRV(1,SEC(2,i)))**2+(CRV(2,SEC(2,i)+1)-CRV(2,SEC(2,i)))**2)
!		"external" interpolation
								if(length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
									in1=floor((SEC(1,num_sec)+SEC(2,num_sec))/2.0)
									in2=floor((SEC(1,2)+SEC(2,2))/2.0)
									if(in1==in2)then
										in1=floor((SEC(1,2)+3*SEC(2,2))/4.0)
										in2=floor((3*SEC(1,2)+SEC(2,2))/4.0)
									end if
									do k=1,num_points-in1
										FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
									end do
									do k=1,in2-1
										FR(L,k)=FR(L,num_points)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
									end do
!		"internal" interpolation
								else
!		computing the divergence angle to be used
									sum=0.0
									do j=SEC(1,i),SEC(2,i)
										sum=sum+SIDE(j)
									end do
									sum=sum/(SEC(2,i)-SEC(1,i)+1)
									betaI=big_betaI
									if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
									if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
										do k=1,SEC(2,i)-SEC(1,i)+1
											FR(L,SEC(1,i)-1+k)=r_ini+k*(r_fin-r_ini)/(SEC(2,i)-SEC(1,i)+2)
										end do
!		'bow' interpolation
									else
									!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+2)
										ds_perp=ds*tan(betaI*pi/180.0)
										k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
										do k=1,k_thr
											FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
										end do
!		second half
										do k=1,SEC(2,i)+1-SEC(1,i)-k_thr 
											FR(L,SEC(2,i)+1-k)=r_fin+k*ds_perp
										end do
									end if
								end if
							end if
!		there is only one section and it is HIGH
							if(HIGH(num_sec).AND.num_sec==1)then
								if(lwarn8) write(*,1000) r_thr
1000						format('WARNING! Rfact,threshold=',e9.2,' is probably too low. You may need to increase this value.')
                lwarn8=.false.
!		first and last FR
								r_ini=r_inii(L)
								r_fin=r_fini(L)
!		compute length of the section
								length=0.0
								do j=1,num_points-1
									length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								length=length+sqrt((CRV(1,1)-CRV(1,num_points))**2+(CRV(2,1)-CRV(2,num_points))**2)
								if (r_ini<=0.0.OR.r_fin<=0.0) then
									if(r_ini<=0.0) r_ini=length/length_fr_def
									if(r_fin<=0.0) r_fin=length/length_fr_def
								end if
								do k=1,num_points
									FR(L,k)=(r_ini+r_fin)/2.0
								end do
							end if
!		it is a central section
						else if(i<num_sec)then
!		determine first and last FR
							r_ini=FR(L,SEC(2,i-1))
							r_fin=FR(L,SEC(1,i+1))
!		compute length of the section
							length=0.0
							length=length+sqrt((CRV(1,SEC(1,i))-CRV(1,SEC(1,i)-1))**2+(CRV(2,SEC(1,i))-CRV(2,SEC(1,i)-1))**2)
							do j=SEC(1,i),SEC(2,i)-1
								length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
							end do
							length=length+sqrt((CRV(1,SEC(2,i)+1)-CRV(1,SEC(2,i)))**2+(CRV(2,SEC(2,i)+1)-CRV(2,SEC(2,i)))**2)
!		"external" interpolation
							if(length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
!		computing in1 and in2
								if(i==2.AND.not(HIGH(num_sec)))then
									in1=floor((SEC(1,num_sec)+num_points+SEC(2,1))/2.0)
									if(in1>num_points) in1=in1-num_points
								else
									in1=floor((SEC(1,i-1)+SEC(2,i-1))/2.0)
								end if
								if(i==num_sec-1.AND.not(HIGH(1)))then
									in2=floor((SEC(1,num_sec)+num_points+SEC(2,1))/2.0)
									if(in2>num_points) in2=in2-num_points
								else
									in2=floor((SEC(1,i+1)+SEC(2,i+1))/2.0)
								end if
								if(in1==in2)then ! correction in case in1==in2
									in1=floor((SEC(1,num_sec)+3*(num_points+SEC(2,1)))/4.0)
									in2=floor((3*SEC(1,num_sec)+(num_points+SEC(2,1)))/4.0)
									if(in1>num_points) in1=in1-num_points
									if(in2>num_points) in2=in2-num_points
								end if
!		performing the interpolations
								if(in1<SEC(1,i).AND.in2>SEC(2,i))then ! in1 and in2 are in the "expected" positions
									do k=1,in2-in1-1
										FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(in2-in1)
									end do
								else if(in1<SEC(1,i).AND.in2<SEC(2,i))then ! in2 "was bigger" than num_points
									do k=1,num_points-in1
										FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(num_points+in2-in1)
									end do
									do k=1,in2-1
										FR(L,k)=FR(L,num_points)+(FR(L,in2)-FR(L,in1))*k/(num_points+in2-in1)
									end do
								else if(in1>SEC(1,i).AND.in2>SEC(2,i))then ! in1 "was smaller" than 1
									do k=1,num_points-in1
										FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(num_points+in2-in1)
									end do
									do k=1,in2-1
										FR(L,k)=FR(L,num_points)+(FR(L,in2)-FR(L,in1))*k/(num_points+in2-in1)
									end do
								end if
!		"internal" interpolation
							else
!		computing the divergence angle to be used
								sum=0.0
								do j=SEC(1,i),SEC(2,i)
									sum=sum+SIDE(j)
								end do
								sum=sum/(SEC(2,i)-SEC(1,i)+1)
								betaI=big_betaI
								if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
								if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
									do k=1,SEC(2,i)-SEC(1,i)+1
										FR(L,SEC(1,i)-1+k)=r_ini+k*(r_fin-r_ini)/(SEC(2,i)-SEC(1,i)+2)
									end do
!		'bow' interpolation
								else
								!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+2)
									ds_perp=ds*tan(betaI*pi/180.0)
									k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
									do k=1,k_thr
										FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
									end do
!		second half
									do k=1,SEC(2,i)+1-SEC(1,i)-k_thr
										FR(L,SEC(2,i)+1-k)=r_fin+k*ds_perp
									end do
								end if
							end if
!		it is the last section
						else if(i==num_sec)then
!		if the first section is not "high"
							if(not(HIGH(1)))then
!		determine first and last FR
								r_ini=FR(L,SEC(2,num_sec-1))
								r_fin=FR(L,SEC(1,1))
!		compute length of the section
								length=0.0
								length=length+sqrt((CRV(1,SEC(1,i))-CRV(1,SEC(1,i)-1))**2+(CRV(2,SEC(1,i))-CRV(2,SEC(1,i)-1))**2)
								do j=SEC(1,i),SEC(2,i)-1
									length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								length=length+sqrt((CRV(1,1)-CRV(1,num_points))**2+(CRV(2,1)-CRV(2,num_points))**2)
!		"external" interpolation
								if(length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
									in1=floor((SEC(1,num_sec-1)+SEC(2,num_sec-1))/2.0)
									in2=floor((SEC(1,1)+SEC(2,1))/2.0)
									if(in1==in2)then
										in1=floor((SEC(1,1)+3*SEC(2,1))/4.0)
										in2=floor((3*SEC(1,1)+SEC(2,1))/4.0)
									end if
									do k=1,num_points-in1
										FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
									end do
									do k=1,in2-1
										FR(L,k)=FR(L,num_points)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
									end do
!		"internal" interpolation
								else
!		computing the divergence angle to be used
									sum=0.0
									do j=SEC(1,i),SEC(2,i)
										sum=sum+SIDE(j)
									end do
									sum=sum/(SEC(2,i)-SEC(1,i)+1)
									betaI=big_betaI
									if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
									if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
										do k=1,SEC(2,i)-SEC(1,i)+1
											FR(L,SEC(1,i)-1+k)=r_ini+k*(r_fin-r_ini)/(SEC(2,i)-SEC(1,i)+2)
										end do
!		'bow' interpolation
									else
									!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+2)
										ds_perp=ds*tan(betaI*pi/180.0)
										k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		first half
										do k=1,k_thr
											FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
										end do
!		second half
										do k=1,SEC(2,i)+1-SEC(1,i)-k_thr
											FR(L,SEC(2,i)+1-k)=r_fin+k*ds_perp
										end do
									end if
								end if
!		if the first section is "high"
							else
!		(we only arrive here if there are more than one section)
!		determine first and last FR
								r_ini=FR(L,SEC(2,num_sec-1))
								r_fin=FR(L,SEC(1,2))
!		computing length of the two sections
								length=0.0
								length=length+sqrt((CRV(1,SEC(1,i))-CRV(1,SEC(1,i)-1))**2+(CRV(2,SEC(1,i))-CRV(2,SEC(1,i)-1))**2)
								do j=SEC(1,i),SEC(2,i)-1
									length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								length=length+sqrt((CRV(1,1)-CRV(1,num_points))**2+(CRV(2,1)-CRV(2,num_points))**2)
								do j=SEC(1,1),SEC(2,1)-1
									length=length+sqrt((CRV(1,j+1)-CRV(1,j))**2+(CRV(2,j+1)-CRV(2,j))**2)
								end do
								length=length+sqrt((CRV(1,SEC(2,1)+1)-CRV(1,SEC(2,1)))**2+(CRV(2,SEC(2,1)+1)-CRV(2,SEC(2,1)))**2)
!		"external" interpolation
								if(length<abs(r_fin-r_ini)/tan(alphaR*pi/180.0))then
									in1=floor((SEC(1,num_sec-1)+SEC(2,num_sec-1))/2.0)
									in2=floor((SEC(1,2)+SEC(2,2))/2.0)
									if(in1==in2)then
										in1=floor((SEC(1,2)+3*SEC(2,2))/4.0)
										in2=floor((3*SEC(1,2)+SEC(2,2))/4.0)
									end if
									do k=1,num_points-in1
										FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
									end do
									do k=1,in2-1
										FR(L,k)=FR(L,num_points)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
									end do
!		"internal" interpolation
								else
!		computing the divergence angle to be used
									sum=0.0
									do j=SEC(1,i),SEC(2,i)
										sum=sum+SIDE(j)
									end do
									do j=SEC(1,1),SEC(2,1)
										sum=sum+SIDE(j)
									end do
									sum=sum/(SEC(2,i)-SEC(1,i)+1+SEC(2,1))
									betaI=big_betaI
									if(((l==1).and.(sum<-0.5)).or.((l==2).and.(sum>0.5))) betaI=small_betaI
!		linear interpolation
									if(length<interp*abs(r_fin-r_ini)/tan(betaI*pi/180.0))then
										in1=SEC(2,num_sec-1)
										in2=SEC(1,2)
										do k=1,num_points-in1
											FR(L,in1+k)=FR(L,in1)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
										end do
										do k=1,in2-1
											FR(L,k)=FR(L,num_points)+(FR(L,in2)-FR(L,in1))*k/(in2-in1+num_points)
										end do
!		'bow' interpolation
									else
									!!!!!!??????	ds=length/(SEC(2,i)-SEC(1,i)+SEC(2,1)+2)
										ds_perp=ds*tan(betaI*pi/180.0)
										k_thr=floor((length+(r_fin-r_ini)/tan(betaI*pi/180.0))/2.0/ds)
!		if the threshold is within the last section
										if(k_thr<=SEC(2,i)-SEC(1,i)+1)then
!		first part
											do k=1,k_thr
												FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
											end do
!		third part
											do k=1,SEC(2,1)
												FR(L,SEC(2,1)+1-k)=r_fin+k*ds_perp
											end do
!		second part
											do k=1,num_points+1-SEC(1,i)-k_thr
												FR(L,num_points+1-k)=FR(L,1)+k*ds_perp
											end do 
!		if the threshold is within the first section
										else if(k_thr>SEC(2,i)-SEC(1,i)+1)then
!		first part
											do k=1,SEC(2,i)-SEC(1,i)+1
												FR(L,SEC(1,i)-1+k)=r_ini+k*ds_perp
											end do
!		second part
											do k=1,k_thr-SEC(2,i)+SEC(1,i)-1
												FR(L,k)=FR(L,num_points)+k*ds_perp
											end do
!		third part
											do k=1,SEC(2,1)+1-k_thr+SEC(2,i)-SEC(1,i)
												FR(L,SEC(2,1)+1-k)=r_fin+k*ds_perp
											end do
										end if
									end if
								end if
							end if
						end if
					end if
				end if
			end do
!		----------------
!		set min distance
!		----------------
			do i=1,num_points
				if(FR(L,i)<dmin) FR(L,i)=dmin
			end do
!		-------------------------------------------------------------------------
!		Computing FR (4th time: paying atention to the GLOBAL SHAPE of the CURVE)
!		-------------------------------------------------------------------------
			do i=1,num_points
				if(L==1)then
					x_ref=CRV(1,i)+FR(L,i)*cos(CRV(3,i))
					y_ref=CRV(2,i)+FR(L,i)*sin(CRV(3,i))
				else
					x_ref=CRV(1,i)-FR(L,i)*cos(CRV(3,i))
					y_ref=CRV(2,i)-FR(L,i)*sin(CRV(3,i))
				end if
				modification_tag=.false.
				do j=1,num_points
					if(j/=i)then
						dist_to_point_j=sqrt((CRV(1,j)-x_ref)**2+(CRV(2,j)-y_ref)**2)
						if(dist_to_point_j<FR(L,i))then
							if(.NOT.modification_tag) modification_tag=.true.
							cosine=(((x_ref-CRV(1,i))*(CRV(1,j)-x_ref))+((y_ref-CRV(2,i))*(CRV(2,j)-y_ref)))/FR(L,i)/dist_to_point_j
							FR(L,i)=(FR(L,i)**2+dist_to_point_j**2+2*FR(L,i)*dist_to_point_j*cosine)/(FR(L,i)+dist_to_point_j*cosine)/2.0
							if(L==1)then
								x_ref=CRV(1,i)+FR(L,i)*cos(CRV(3,i))
								y_ref=CRV(2,i)+FR(L,i)*sin(CRV(3,i))
							else
								x_ref=CRV(1,i)-FR(L,i)*cos(CRV(3,i))
								y_ref=CRV(2,i)-FR(L,i)*sin(CRV(3,i))
							end if
						end if
					end if
				end do
				if(modification_tag) FR(L,i)=Factor_globalshape*FR(L,i)
			end do
			deallocate(SEC,HIGH,NUM_BLSPOT,BL_SP)
		end do ! left and right hand side
		if(allocated(SIDE)) deallocate(SIDE)
		if(allocated(CRVrenumbered)) deallocate(CRVrenumbered)
!		----------------
!		set max distance
!		----------------
		do i=1,num_points
			if(FR(1,i)>dmax(1)) FR(1,i)=dmax(1)
			if(FR(2,i)>dmax(2)) FR(2,i)=dmax(2)
		end do
!		----------------------------
!		Computing the PARALEL curves
!		----------------------------
    if(abs(iWf_genExp).gt.nFunA) iWf_genExp=0_4
    s=s1-0.5d0*ds
		do i=1,num_points
      s=s+ds
			CRVp(1,1,i)=CRV(1,i)+FR(1,i)*cos(CRV(3,i)) ! left, x
			CRVp(1,2,i)=CRV(2,i)+FR(1,i)*sin(CRV(3,i)) ! left, y
			CRVp(2,1,i)=CRV(1,i)-FR(2,i)*cos(CRV(3,i)) ! right, x
			CRVp(2,2,i)=CRV(2,i)-FR(2,i)*sin(CRV(3,i)) ! right, y
      call adaptExpD(s,FR(1,i))
      call adaptExpD(s,FR(2,i))
			CRVp(1,1,i)=CRV(1,i)+FR(1,i)*cos(CRV(3,i)) ! left, x
			CRVp(1,2,i)=CRV(2,i)+FR(1,i)*sin(CRV(3,i)) ! left, y
			CRVp(2,1,i)=CRV(1,i)-FR(2,i)*cos(CRV(3,i)) ! right, x
			CRVp(2,2,i)=CRV(2,i)-FR(2,i)*sin(CRV(3,i)) ! right, y
		end do
!		-----------------------------------------
!		Computing the POSITIONS of the MULTIPOLES
!		-----------------------------------------
!		computing number of multipoles (mult_num)
		do L=1,2 ! left and right hand side
!		first multipole
			point1=1
			mult_num(L)=0
			x_ref=CRVp(L,1,point1)
			y_ref=CRVp(L,2,point1)
			point2=point1
			dist=0.0
			do while (dist<mult_sep(L)*FR(L,point2)/2.0.AND.point2<num_points)
				point2=point2+1
				dist=sqrt((CRVp(L,1,point2)-x_ref)**2+(CRVp(L,2,point2)-y_ref)**2)
			end do
			point1=point2
			if(point2<num_points) mult_num(L)=mult_num(L)+1
!		next multipoles
			do while(point1<num_points)
				x_ref=CRVp(L,1,point1)
				y_ref=CRVp(L,2,point1)
				point2=point1
				dist=0.0
				do while (dist<mult_sep(L)*FR(L,point1).AND.point2<num_points)
					point2=point2+1
					dist=sqrt((CRVp(L,1,point2)-x_ref)**2+(CRVp(L,2,point2)-y_ref)**2)
				end do
				do while (dist<mult_sep(L)*FR(L,point2).AND.point2<num_points)
					point2=point2+1
					dist=sqrt((CRVp(L,1,point2)-x_ref)**2+(CRVp(L,2,point2)-y_ref)**2)
				end do
				point1=point2
				if(point2<num_points) mult_num(L)=mult_num(L)+1
			end do
		end do ! left and right hand side
		mult_numT=mult_num(1)+mult_num(2)
!		saving coordinate of multipoles and point to which they are attached
		if(allocated(MULT)) deallocate(MULT)
		if(allocated(POINT)) deallocate(POINT)
		allocate(MULT(2,2,max(mult_num(1),mult_num(2))),POINT(2,max(mult_num(1),mult_num(2))),Stat=idum)
		if(idum/=0) then
			idum=MessageBoxQQ('Memory allocation for MULT,POINT failed!'C,'Generate expansions'C,MB$OK.or.MB$IconExclamation)
			return
		end if
!		computing coordinates of the multipoles
		do L=1,2 ! left and right hand side
!		first multipole
			point1=1
			mult_num(L)=0
			x_ref=CRVp(L,1,point1)
			y_ref=CRVp(L,2,point1)
			point2=point1
			dist=0.0
			do while(dist<mult_sep(L)*FR(L,point2)/2.0.AND.point2<num_points)
				point2=point2+1
				dist=sqrt((CRVp(L,1,point2)-x_ref)**2+(CRVp(L,2,point2)-y_ref)**2)
			end do
			point1=point2
			if(point2<num_points)then
				j=1
				POINT(L,j)=point1
				mult_num(L)=mult_num(L)+1
				MULT(L,1,mult_num(L))=CRVp(L,1,point1)
				MULT(L,2,mult_num(L))=CRVp(L,2,point1)
			end if
!		next multipoles
			do while(point1<num_points)
				x_ref=CRVp(L,1,point1)
				y_ref=CRVp(L,2,point1)
				point2=point1
				dist=0.0
				do while(dist<mult_sep(L)*FR(L,point1).AND.point2<num_points)
					point2=point2+1
					dist=sqrt((CRVp(L,1,point2)-x_ref)**2+(CRVp(L,2,point2)-y_ref)**2)
				end do
				do while(dist<mult_sep(L)*FR(L,point2).AND.point2<num_points)
					point2=point2+1
					dist=sqrt((CRVp(L,1,point2)-x_ref)**2+(CRVp(L,2,point2)-y_ref)**2)
				end do
				point1=point2
				if(point2<num_points)then
					j=j+1
					POINT(L,j)=point1
					mult_num(L)=mult_num(L)+1
					MULT(L,1,mult_num(L))=CRVp(L,1,point1)
					MULT(L,2,mult_num(L))=CRVp(L,2,point1)
				end if
			end do
!		taking care of the last multipole...
			if(mult_num(L)>0)then
				point1=POINT(L,j)
				dist=sqrt((CRV(1,num_points)-CRV(1,point1))**2+(CRV(2,num_points)-CRV(2,point1))**2)
				fr_shift=FR(L,point1)
!		there is a piece of curve "not covered" by the last multipole
				if(dist>fr_shift*mult_sep(L)/2.0)then
					do while(dist>fr_shift*mult_sep(L)/2.0.AND.point1<num_points)
						point1=point1+1
						fr_shift=fr_shift+2.0*ds/mult_sep(L)
						dist=sqrt((CRV(1,num_points)-CRV(1,point1))**2+(CRV(2,num_points)-CRV(2,point1))**2)
					end do
					point1=point1-1
					fr_shift=fr_shift-2.0*ds/mult_sep(L)
					POINT(L,j)=point1
					FR(L,point1)=fr_shift
					if(L==1)then
						MULT(L,1,mult_num(L))=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
						MULT(L,2,mult_num(L))=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
					else
						MULT(L,1,mult_num(L))=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
						MULT(L,2,mult_num(L))=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
					end if
!		checking that the previous maneuver has not violated the following two criteria:
!		1:global shape
					if(L==1)then
						x_ref=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
						y_ref=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
					else
						x_ref=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
						y_ref=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
					end if
					modification_tag=.false.
					do j=1,num_points
						if(j/=point1)then
							dist_to_point_j=sqrt((CRV(1,j)-x_ref)**2+(CRV(2,j)-y_ref)**2)
							if(dist_to_point_j<FR(L,point1))then
								if(.NOT.modification_tag) modification_tag=.true.
								cosine=(((x_ref-CRV(1,point1))*(CRV(1,j)-x_ref))+((y_ref-CRV(2,point1))*(CRV(2,j)-y_ref)))/FR(L,point1)/dist_to_point_j
								FR(L,point1)=(FR(L,point1)**2+dist_to_point_j**2+2*FR(L,point1)*dist_to_point_j*cosine)/(FR(L,point1)+dist_to_point_j*cosine)/2.0
								if(L==1)then
									x_ref=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
									y_ref=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
								else
									x_ref=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
									y_ref=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
								end if
							end if
						end if
					end do
					if(modification_tag)then
						FR(L,point1)=Factor_globalshape*FR(L,point1)
						if(L==1)then
							MULT(L,1,mult_num(L))=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
						else
							MULT(L,1,mult_num(L))=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
						end if
					end if
!		2:maximum distance
					if(FR(L,point1)>dmax(L))then
						FR(1,point1)=dmax(L)
						if(L==1)then
							MULT(L,1,mult_num(L))=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
						else
							MULT(L,1,mult_num(L))=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
						end if
					end if
!		the multipole is acting "outside" the curve
				else
					do while(dist<fr_shift*mult_sep(L)/2.0.AND.point1>POINT(L,j-1))
						point1=point1-1
						fr_shift=fr_shift-2.0*ds/mult_sep(L)
						dist=sqrt((CRV(1,num_points)-CRV(1,point1))**2+(CRV(2,num_points)-CRV(2,point1))**2)
					end do
					point1=point1+1
					fr_shift=fr_shift+2.0*ds/mult_sep(L)
					POINT(L,j)=point1
					FR(L,point1)=fr_shift
					if(L==1)then
						MULT(L,1,mult_num(L))=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
						MULT(L,2,mult_num(L))=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
					else
						MULT(L,1,mult_num(L))=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
						MULT(L,2,mult_num(L))=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
					end if
!		checking that the previous maneuver has not violated the following two criteria:
!		1:minimum distance
					if(FR(L,point1)<dmin)then
						FR(1,point1)=dmin
						if(L==1)then
							MULT(L,1,mult_num(L))=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
						else
							MULT(L,1,mult_num(L))=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
						end if
					end if
!		2:global shape
					if(L==1)then
						x_ref=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
						y_ref=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
					else
						x_ref=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
						y_ref=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
					end if
					modification_tag=.false.
					do j=1,num_points
						if(j/=point1)then
							dist_to_point_j=sqrt((CRV(1,j)-x_ref)**2+(CRV(2,j)-y_ref)**2)
							if(dist_to_point_j<FR(L,point1))then
								if(.NOT.modification_tag) modification_tag=.true.
								cosine=(((x_ref-CRV(1,point1))*(CRV(1,j)-x_ref))+((y_ref-CRV(2,point1))*(CRV(2,j)-y_ref)))/FR(L,point1)/dist_to_point_j
								FR(L,point1)=(FR(L,point1)**2+dist_to_point_j**2+2*FR(L,point1)*dist_to_point_j*cosine)/(FR(L,point1)+dist_to_point_j*cosine)/2.0
								if(L==1)then
									x_ref=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
									y_ref=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
								else
									x_ref=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
									y_ref=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
								end if
							end if
						end if
					end do
					if(modification_tag)then
						FR(L,point1)=Factor_globalshape*FR(L,point1)
						if(L==1)then
							MULT(L,1,mult_num(L))=CRV(1,point1)+FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)+FR(L,point1)*sin(CRV(3,point1))
						else
							MULT(L,1,mult_num(L))=CRV(1,point1)-FR(L,point1)*cos(CRV(3,point1))
							MULT(L,2,mult_num(L))=CRV(2,point1)-FR(L,point1)*sin(CRV(3,point1))
						end if
					end if
				end if
			end if
		end do	! left and right hand side
    if(Allocated(CRV)) DeAllocate(CRV)
		if(allocated(CRVp)) deallocate(CRVp)
!		----------------------------------------------------
!		Computing the NUMBER of PARAMETERS of the MULTIPOLES
!		----------------------------------------------------
!		find minimum and maximum of short FR (only for points where multipoles are located)
		fr_max=0.0
		fr_min=pBig
		do L=1,2 ! left and right side
			i=0
			do while(i<=mult_num(L)-1) ! for every multipole
				i=i+1
				if(iDL==0)then
					if(FR(1,POINT(L,i))>fr_max) fr_max=FR(1,POINT(L,i))
					if(FR(1,POINT(L,i))<fr_min) fr_min=FR(1,POINT(L,i))
				else if(iDR==0)then
					if(FR(2,POINT(L,i))>fr_max) fr_max=FR(2,POINT(L,i))
					if(FR(2,POINT(L,i))<fr_min) fr_min=FR(2,POINT(L,i))
				else				
					if(FR(1,POINT(L,i))<FR(2,POINT(L,i)))then ! the short side is the left
						if(FR(1,POINT(L,i))>fr_max) fr_max=FR(1,POINT(L,i))
						if(FR(1,POINT(L,i))<fr_min) fr_min=FR(1,POINT(L,i))
					else ! the short side is the right
						if(FR(2,POINT(L,i))>fr_max) fr_max=FR(2,POINT(L,i))
						if(FR(2,POINT(L,i))<fr_min) fr_min=FR(2,POINT(L,i))
					end if
				end if
			end do
		end do
!		perfect electric conductors...
		if(iDR==0) mult_num(1)=0
		if(iDL==0) mult_num(2)=0
		mult_numT=mult_num(1)+mult_num(2)
!		---------------------------------------
!		Testing the DISTANCE BETWEEN MULTIPOLES
!		---------------------------------------
		if(allocated(MULTI_TAG)) deallocate(MULTI_TAG)
		allocate(MULTI_TAG(2,max(mult_num(1),mult_num(2))),Stat=idum)
		if(idum/=0) then
			idum=MessageBoxQQ('Memory allocation for MULTI_TAG failed!'C,'Generate expansions'C,MB$OK.or.MB$IconExclamation)
			return
		end if
		do i=1,max(mult_num(1),mult_num(2))
			MULTI_TAG(1,i)=0
			MULTI_TAG(2,i)=0
		end do
		do L=1,2 ! left and right multipoles
			do i=1,mult_num(L) ! for every multipole...
				if(MULTI_TAG(L,i)==0)then ! ...that has not already deleted by substitution
					do j=1,mult_num(L) ! it is compared with every multipole...
						if(lclosed)then ! closed curves
							if(i==1)then
								if(MULTI_TAG(L,j)==0.AND.j/=mult_num(L).AND.j/=1.AND.j/=2)then !...except the previous, itself, and the following (if not already deleted, of course)
									dist_multipoles=sqrt((MULT(L,1,j)-MULT(L,1,i))**2+(MULT(L,2,j)-MULT(L,2,i))**2)
									if(dist_multipoles<Factor_multip_dist*FR(L,POINT(L,i)).AND.dist_multipoles<Factor_multip_dist*FR(L,POINT(L,j)))then
										MULTI_TAG(L,j)=1
										MULT(L,1,i)=(MULT(L,1,i)+MULT(L,1,j))/2.0
										MULT(L,2,i)=(MULT(L,2,i)+MULT(L,2,j))/2.0
									end if
								end if
							else if(i==mult_num(L))then
								if(MULTI_TAG(L,j)==0.AND.j/=mult_num(L)-1.AND.j/=mult_num(L).AND.j/=1)then !...except the previous, itself, and the following (if not already deleted, of course)
									dist_multipoles=sqrt((MULT(L,1,j)-MULT(L,1,i))**2+(MULT(L,2,j)-MULT(L,2,i))**2)
									if(dist_multipoles<Factor_multip_dist*FR(L,POINT(L,i)).AND.dist_multipoles<Factor_multip_dist*FR(L,POINT(L,j)))then
										MULTI_TAG(L,j)=1
										MULT(L,1,i)=(MULT(L,1,i)+MULT(L,1,j))/2.0
										MULT(L,2,i)=(MULT(L,2,i)+MULT(L,2,j))/2.0
									end if
								end if
							else
								if(MULTI_TAG(L,j)==0.AND.j/=i-1.AND.j/=i.AND.j/=i+1)then !...except the previous, itself, and the following (if not already deleted, of course)
									dist_multipoles=sqrt((MULT(L,1,j)-MULT(L,1,i))**2+(MULT(L,2,j)-MULT(L,2,i))**2)
									if(dist_multipoles<Factor_multip_dist*FR(L,POINT(L,i)).AND.dist_multipoles<Factor_multip_dist*FR(L,POINT(L,j)))then
										MULTI_TAG(L,j)=1
										MULT(L,1,i)=(MULT(L,1,i)+MULT(L,1,j))/2.0
										MULT(L,2,i)=(MULT(L,2,i)+MULT(L,2,j))/2.0
									end if
								end if
							end if
						else ! open curves
							if(MULTI_TAG(L,j)==0.AND.j/=i-1.AND.j/=i.AND.j/=i+1)then !...except the previous, itself, and the following (if not already deleted, of course)
								dist_multipoles=sqrt((MULT(L,1,j)-MULT(L,1,i))**2+(MULT(L,2,j)-MULT(L,2,i))**2)
								if(dist_multipoles<Factor_multip_dist*FR(L,POINT(L,i)).AND.dist_multipoles<Factor_multip_dist*FR(L,POINT(L,j)))then
									MULTI_TAG(L,j)=1
									MULT(L,1,i)=(MULT(L,1,i)+MULT(L,1,j))/2.0
									MULT(L,2,i)=(MULT(L,2,i)+MULT(L,2,j))/2.0
								end if
							end if
						end if
					end do
				end if
			end do
		end do
		if(allocated(FR)) deallocate(FR)
		in1=0
		in2=0
		do i=1,max(mult_num(1),mult_num(2))
			if(MULTI_TAG(1,i)==1) in1=in1+1
			if(MULTI_TAG(2,i)==1) in2=in2+1
		end do
		mult_numT=mult_numT-in1-in2
!		=======================================================================
!		=OUTPUT BLOCK==========================================================
!		=======================================================================
!		left multipoles
		iB=jE+1
		do i=1,mult_num(1)
			if(MULTI_TAG(1,i)==0)then
        call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				if(.NOT.ldum) return
        call SetExp(iB,iDR,iCn,iCl,MULT(1,1,i),MULT(1,2,i))
				if(i>1) call DrawLine(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),tExp(iB+1)%Plane(1,0),tExp(iB+1)%Plane(2,0))
			end if
		end do
!		right multipoles
	  do i=1,mult_num(2)
			if(MULTI_TAG(2,i)==0)then
        call CopyExp(jE,jE,0.0d0,0.0d0,-3_4,ldum)
				if(.NOT.ldum) return
        call SetExp(iB,iDL,iCn,iCl,MULT(2,1,i),MULT(2,2,i))
				if(i>1) call DrawLine(tExp(iB)%Plane(1,0),tExp(iB)%Plane(2,0),tExp(iB+1)%Plane(1,0),tExp(iB+1)%Plane(2,0))
			end if
	  end do
		if(allocated(MULT)) deallocate(MULT)
		if(allocated(POINT)) deallocate(POINT)
		if(allocated(MULTI_TAG)) deallocate(MULTI_TAG)
  end Subroutine GenExpM

  Subroutine adaptExpD(s,dist,wgt)
! adapt the distance of the expansion to be generated using data in the function array
! Note: column 4 of the function array must contain the location (s) of the point along the boundaries
    Implicit none
    Real(8), optional:: wgt
    Real(8) s,dist,w,wav,ws,a
    Integer(4) iWf,ipos,k
    if(Present(wgt)) wgt=1.0d0
    if((iWf_genExp.eq.0_4).or.(nFunA.lt.4)) return
    iWf=iWf_genExp
    if(Present(wgt)) iWf=-iWf
    call FindFunPosition(4,s,ipos)
    ipos=min(max(ipos,1),nFun)
    w=abs(Fun(abs(iWf_genExp),ipos))
    wav=0.0d0
    do k=1,nFun
      ws=abs(Fun(abs(iWf_genExp),k))
      wav=wav+ws
    end do
    wav=wav/Dble(max(1,nFun))
    a=1.0/Real(abs(iWe_genExp))
    if(w.lt.wav) then
      w=(1.0d0+log10(wav/w))**a
      if(iWe_genExp.gt.0) w=min(4.0d0,w)
      if(iWf.gt.0) w=1.0d0/w
    else
      w=(1.0d0+log10(w/wav))**a
      if(iWe_genExp.gt.0) w=min(4.0d0,w)
      if(iWf.lt.0) w=1.0d0/w
    end if
    if(Present(wgt)) then
      wgt=w
    else
      dist=w*dist
    end if
  end Subroutine adaptExpD

  Subroutine adaptExp2D0(ldum)
! Adapt 2D expansions with method 0: 
! Delete expansions inside the domain.
    Implicit none
    Integer(4) kE,kB
    Integer(2) iDom0,iCol0,iCon0,iDm
    Real(8) d,x,y,s
    Logical, intent(in) :: ldum
    Logical lInside,ldu
    if(ldum) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
! get boundary information for each expansion
    do kE=iModExpMax,iModExpMin,-1
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      iDm=tExp(kE)%iDom
      call DistPtDom(iDm,tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),d,x,y,s,kB,lInside)
      if(lInside) call InsertExp(kE,-1,ldu)
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D0

  Subroutine adaptExp2D0f(ldum)
! Adapt 2D expansions with method 0: 
! Delete expansions near a fictitous boundary with same domain number on both sides.
    Implicit none
    Integer(4) kE,kB
    Integer(2) iDom0,iCol0,iCon0,iDm
    Real(8) d,x,y,s
    Logical, intent(in) :: ldum
    Logical lInside,ldu
    if(ldum) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
! get boundary information for each expansion
    do kE=iModExpMax,iModExpMin,-1
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      iDm=0_2
      call DistPtDom(iDm,tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),d,x,y,s,kB,lInside)
      if(tBnd(kB)%iLDom.eq.tBnd(kB)%iRDom) call InsertExp(kE,-1,ldu)
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D0f

  Subroutine adaptExp2D1(ldum)
! Adapt 2D expansions with method 1: 
! Compute a "force" acting on the expansions from the values stored in the function array, column abs(iModExpWFA)
! If iModExpWFA<0: draw lines from old to new positions, but do not move
! Note: column 4 of the function array must contain the location (s) of the point along the boundaries
    Implicit none
    Integer(4) i
    Integer(2) iDom0,iCol0,iCon0
    Logical, intent(in) :: ldum
    if(ldum.or.(abs(iModExpWFA).lt.1).or.(abs(iModExpWFA).gt.nFunA)) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
    do i=1,max(1,abs(iModExpLoop))
      call adaptExp2D1k(ldum)
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D1

  Subroutine adaptExp2D1k(ldum)
! Adapt 2D expansions with method 1
    Implicit none
    Integer(4) kE,k,idum
    Integer(2) iDomL,iDomR,ic0
    Real(8) fmax,x,y,fx,fy,f,sk,ds,dx,dy,d2,d1,xk,yk,vt1,vt2,dmin
    Logical, intent(in) :: ldum
    if(ldum) return
    fmax=0.0d0
    do kE=iModExpMin,iModExpMax ! get fmax
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      x=tExp(kE)%Plane(1,0)
      y=tExp(kE)%Plane(2,0)
      fx=0.0d0
      fy=0.0d0
      do k=1,nFun
        f=abs(Fun(abs(iModExpWFA),k))
        sk=Fun(4,k)
        if((k.gt.1).and.(k.lt.nFun)) then
          ds=0.5d0*(Fun(4,k+1)-Fun(4,k-1))
        else if(k.gt.1) then
          ds=Fun(4,k)-Fun(4,k-1)
        else
          ds=Fun(4,k+1)-Fun(4,k)
        end if
        call GetBndPt(0,sk,xk,yk,vt1,vt2,iDomL,iDomR,idum)
        dx=x-xk
        dy=y-yk
        d2=dx**2+dy**2
        fx=fx+f*ds*dx/sqrt(d2)**iModExpWFE
        fy=fy+f*ds*dy/sqrt(d2)**iModExpWFE
      end do
      f=dsqrt(fx**2+fy**2)
      if(f.gt.fmax) fmax=f
    end do
    do kE=iModExpMin,iModExpMax ! get new positions
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      x=tExp(kE)%Plane(1,0)
      y=tExp(kE)%Plane(2,0)
      fx=0.0d0
      fy=0.0d0
      dmin=pBig
      do k=1,nFun
        f=abs(Fun(abs(iModExpWFA),k))
        sk=Fun(4,k)
        if((k.gt.1).and.(k.lt.nFun)) then
          ds=0.5d0*(Fun(4,k+1)-Fun(4,k-1))
        else if(k.gt.1) then
          ds=Fun(4,k)-Fun(4,k-1)
        else
          ds=Fun(4,k+1)-Fun(4,k)
        end if
        call GetBndPt(0,sk,xk,yk,vt1,vt2,iDomL,iDomR,idum)
        dx=x-xk
        dy=y-yk
        d2=dx**2+dy**2
        if(d2.lt.dmin) dmin=d2
        d1=sqrt(dx**2+dy**2)**iModExpWFE
        fx=fx+f*ds*dx/sqrt(d2)**iModExpWFE
        fy=fy+f*ds*dy/sqrt(d2)**iModExpWFE
      end do
      dmin=dsqrt(dmin)
      fx=-fModExpFac*dmin*fx/fmax
      fy=-fModExpFac*dmin*fy/fmax
      ic0=setcolor(2_2)
      call DrawLine(x,y,x+fx,y+fy)
      ic0=setcolor(ic0)
      if(iModExpWFA.gt.0) then ! move to new position
        call SetExpansion(kE,x+fx,y+fy)
      end if
    end do
  end Subroutine adaptExp2D1k

  Subroutine adaptExp2D2(ldum)
! Adapt 2D expansions with method 2: 
! Try to obtain squares and adapt orders according to associated boundary segment
    Implicit none
    Integer(2) iDom0,iCol0,iCon0,iDm
    Integer(4) i,nMove,iBnd0,k,k1,k2
    Logical, intent(in) :: ldum
    if(ldum) return
    iBnd0=iModExpBnd
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iColExp=iModExpCol
    iConExp=iModExpCon
    if(iBnd0.eq.0) then
      k1=1
      k2=nBnd
    else if(iBnd0.gt.0) then
      k1=min(iBnd0,nBnd)
      k2=k1
    else
      k1=1
      k2=min(-iBnd0,nBnd)
    end if
    do k=k1,k2 ! loop over boundaries
      iModExpBnd=k
      do iDm=1,nDom ! adapt locations
        if((iModExpDom.gt.0).and.(iDm.ne.iModExpDom)) Cycle
        if((iModExpDom.lt.0).and.(iDm.gt.-iModExpDom)) Cycle
        iDomExp=iDm
        if(iModExpLoop.gt.0) then ! balance locations
          do i=1,iModExpLoop
            nMove=0
            call adaptExp2D2k(.true.,nMove)
            if(nMove.lt.1) Exit
          end do
        else if(iModExpLoop.lt.0) then ! balance gaps
          do i=1,-iModExpLoop
            nMove=0
            call adaptExp2D2k(.false.,nMove)
          end do
        else
          nMove=-1
          call adaptExp2D2k(.false.,nMove) ! adapt orders
        end if
    end do
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
    iModExpBnd=iBnd0
  end Subroutine adaptExp2D2

  Subroutine adaptExp2D2k(lLoc,nMove)
! Adapt 2D expansions in domain iDm with method 2
    Implicit none
    Real(8) xAver,yAver,xk,yk,fmax,v1(2),sA,sE
    Integer(4) nMove,kE,mE,nE,kEx,idum,iOK
    Integer(2) iDomL,iDomR,ic0,iD1,iD2,kAB
    Logical lLoc,lInside
    Real(8), allocatable:: xE(:),yE(:),xB(:),yB(:),xBt(:),yBt(:),sB(:),dBE(:),sB1(:),sB2(:)
    Integer(4), allocatable:: iE(:),indx(:)
    if((iDomExp.lt.1).or.(iDomExp.gt.nDom)) return
    if((iModExpBnd.gt.nBnd).or.(iModExpBnd.lt.1)) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    fmax=0.0d0
    nE=0
    do kEx=iModExpMin,iModExpMax ! get number of 2D multipoles
      if(LSkipExp(kEx)) Cycle
      if((tExp(kEx)%iTypE.ne.1).or.(tExp(kEx)%iObj.ne.iModExpObj)) Cycle
      nE=nE+1
    end do
    if(nE.lt.3) return ! nothing to do
! deallocate and reallocate memory
    DeAllocate(xE,yE,xB,yB,xBt,yBt,sB,sB1,sB2,dBE,iE,indx,stat=idum)
    Allocate(xE(nExp),yE(nExp),xB(nExp),yB(nExp),xBt(nExp),yBt(nExp),sB(nExp),sB1(nExp),sB2(nExp),dBE(nExp),iE(nExp), &
    & indx(nExp),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Adapt multipoles method 2'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(xE,yE,xB,yB,xBt,yBt,sB,sB1,sB2,dBE,iE,indx,stat=idum)
      return
    end if
! get boundary information for each expansion
    kE=0
    mE=nE ! number of expansions outside the domain
    do kEx=iModExpMin,iModExpMax ! get multipole data
      if(LSkipExp(kEx)) Cycle
      if((tExp(kEx)%iTypE.ne.1).or.(tExp(kEx)%iObj.ne.iModExpObj)) Cycle
      if(tExp(kEx)%iDom.ne.iModExpDom) Cycle
      if((iModExpCol.gt.0).and.(tExp(kEx)%iCol.ne.iModExpCol)) Cycle
      if((iModExpCol.lt.0).and.(tExp(kEx)%iCol.gt.-iModExpCol)) Cycle
      if((iModExpCon.gt.0).and.(tExp(kEx)%iConn.ne.iModExpCon)) Cycle
      if((iModExpCon.lt.0).and.(tExp(kEx)%iConn.gt.-iModExpCon)) Cycle
      kE=kE+1
      iE(kE)=kEx
      xE(kE)=tExp(kEx)%Plane(1,0)
      yE(kE)=tExp(kEx)%Plane(2,0)
      call DistPtBnd(Int2(iModExpBnd),0_2,xE(kE),yE(kE),.true.,.true.,dBE(kE),xB(kE),yB(kE),lInside,iD1,v1,iD2,sB(kE),kAB)
      if(iD1.eq.iModExpDom) then
        mE=mE-1
        kE=kE-1
        Cycle
      end if
      call GetBndPt(iModExpBnd,sB(kE),xk,yk,xBt(kE),yBt(kE),iDomL,iDomR,idum)
    end do
    if(mE.lt.3) then ! nothing to do
      DeAllocate(xE,yE,xB,yB,xBt,yBt,sB,dBE,iE,indx,stat=idum)
      return
    end if
    nE=mE
! sort arrays
    call QuickIndex(sB(1:nE),1,nE,indx,.true.)
    call RIndexSort(xE(1:nE),nE,indx,iOK)
    call RIndexSort(yE(1:nE),nE,indx,iOK)
    call RIndexSort(dBE(1:nE),nE,indx,iOK)
    call RIndexSort(xB(1:nE),nE,indx,iOK)
    call RIndexSort(yB(1:nE),nE,indx,iOK)
    call RIndexSort(xBt(1:nE),nE,indx,iOK)
    call RIndexSort(yBt(1:nE),nE,indx,iOK)
    call IIndexSort(iE(1:nE),nE,indx,iOK)
    if(iOK.ne.0) then
      call Cursor(.false.,IDC_WAIT)
      idum=MessageBoxQQ('Index sorting failed!'C,'Adapt multipoles method 2'C, &
                         MB$OK.or.MB$IconExclamation)
      DeAllocate(xE,yE,xB,yB,xBt,yBt,sB,sB1,sB2,dBE,iE,indx,stat=idum)
      return
    end if
    do kE=2,mE
      ic0=setcolor(3_2)
      call DrawLine(xE(kE-1),yE(kE-1),xE(kE),yE(kE))
    end do
    if(lLoc) then ! adapt locations
      call adaptExp2D2Loc(mE,iE,xE,yE,dBE,sB,nMove)
    else ! adapt orders / avoid gaps along boundary
      if(nMove.lt.0) then ! adapt orders
        call adaptExp2D2Ord(mE,iE,xE,yE,dBE,sB,sB1,sB2,xBt,yBt,xAver,yAver,.true.)
      else ! balance gaps
        call adaptExp2D2Ord(mE,iE,xE,yE,dBE,sB,sB1,sB2,xBt,yBt,xAver,yAver,.false.)
        sA=tBnd(iModExpBnd)%Start
        sE=sA+tBnd(iModExpBnd)%sLength
        call adaptExp2D2Gap(mE,iE,xE,yE,dBE,sB,sB1,sB2,sA,sE)
      end if
    end if
! deallocate memory
    DeAllocate(xE,yE,xB,yB,xBt,yBt,sB,sB1,sB2,dBE,iE,indx,stat=idum)
  end Subroutine adaptExp2D2k

  Subroutine adaptExp2D2Loc(mE,iE,xE,yE,dBE,sB,nMove)
! move expansions towards "optimal location" (balance distances)
    Implicit none
    Integer(4) kE,mE,iE(mE),idum,nMove,knew
    Integer(2) ic0,iD1,iD2,iDomL,iDomR,kAB
    Real(8) xE(mE),yE(mE),dBE(mE),sB(mE),xAB,yAB,v1(2),vAB(2),vE(2),sAB,rad,dAB,vh(2),vh0(2),vC(2),dnew,xnew,ynew,snew
    Logical(4) lOld,linside,lInsideNew
    do kE=2,mE-1
      if((iE(kE).lt.iModExpMin).or.(iE(kE).gt.iModExpMax)) Cycle
      vE(1)=xE(kE)
      vE(2)=yE(kE)
      vAB(1)=0.5d0*(xE(kE+1)+xE(kE-1))
      vAB(2)=0.5d0*(yE(kE+1)+yE(kE-1))
      lOld=.false.
      call DistPtBnd(Int2(iModExpBnd),0_2,vAB(1),vAB(2),.true.,.true.,dAB,xAB,yAB,lInside,iD1,v1,iD2,sAB,kAB)
      call GetBndPt(iModExpBnd,sAB,vAB(1),vAB(2),vh(1),vh(2),iDomL,iDomR,idum,rad)
      if(iabs(iDomL).gt.30000_2) Cycle
      if(fModExpFac.gt.0.0d0) then
        rad=0.5d0*(dBE(kE-1)+dBE(kE+1)) ! new distance from boundary = arithmetic average
      else
        rad=dsqrt(dBE(kE-1)+dBE(kE+1)) ! new distance from boundary = geometric average
      end if
      if(iDomExp.eq.iDomL) then
        vh0(1)=+rad*vh(2)
        vh0(2)=-rad*vh(1)
      else if(iDomExp.eq.iDomR) then
        vh0(1)=-rad*vh(2)
        vh0(2)=+rad*vh(1)
      else
        Cycle
      end if
      vC=vAB+vh0-vE ! vector from old to "optimal" location
      vC=dabs(fModExpFac)*vC+vE ! new location between old and "optimal" location
      if((ixzSymm.ne.0).and.(vC(2).lt.0.2d0*rad)) vC(2)=0.0d0
      if((iyzSymm.ne.0).and.(vC(1).lt.0.2d0*rad)) vC(1)=0.0d0
      call DistPtDom(iDomExp,vC(1),vC(2),dnew,xnew,ynew,snew,knew,lInsideNew) ! check new position
      call DistPtPt(vE(1),vE(2),vC(1),vC(2),dAB)
      if(lInsideNew.or.(dabs(sB(kE)-snew).gt.10.0d0*dAB)) then ! reset distance from boundary to old distance and check again
        rad=dBE(kE)
        vh0(1)=-rad*vh(2)
        vh0(2)=+rad*vh(1)
        vC=vAB+vh0-vE
        vC=dabs(fModExpFac)*vC(1:2)+vE(1:2)
        if((ixzSymm.ne.0).and.(vC(2).lt.0.2d0*rad)) vC(2)=0.0d0
        if((iyzSymm.ne.0).and.(vC(1).lt.0.2d0*rad)) vC(1)=0.0d0
        call DistPtDom(iDomExp,vC(1),vC(2),dnew,xnew,ynew,snew,knew,lInsideNew) ! test new position
        call DistPtPt(vE(1),vE(2),vC(1),vC(2),dAB)
        if(lInsideNew.or.(dabs(sB(kE)-snew).gt.dAB)) lOld=.true. ! reset to old position
      end if
      if(.not.lOld) then ! move to new position
        ic0=setcolor(2_2)
        call DrawLine(vC(1),vC(2),vE(1),vE(2))
        ic0=setcolor(ic0)
        call SetExpansion(iE(kE),vC(1),vC(2))
        xE(kE)=vC(1)
        yE(kE)=vC(2)
        nMove=nMove+1
      end if
    end do
  end Subroutine adaptExp2D2Loc

  Subroutine adaptExp2D2Ord(mE,iE,xE,yE,dBE,sB,sB1,sB2,xBt,yBt,xAver,yAver,lOrd)
! adapt maximum orders of expansions using associated boundary lengths
    Implicit none
    Integer(4) kE,mE,iE(mE),ns,idum
    Integer(2) ic0,ic1,icl,iD1,iD2,iDomL,iDomR,kAB,iO
    Real(8) xE(mE),yE(mE),dBE(mE),sB(mE),sB1(mE),sB2(mE),xBt(mE),yBt(mE),xAB,yAB,v1(2),vAB(2),sAB,ds,rad,xAver,yAver,vh(2)
    Logical(4) linside,lOrd
    xAver=0.0d0
    yAver=0.0d0
    icl=131_2
    ic0=setcolor(icl)
    do kE=1,mE ! get associated length of boundary / distance from boundary -> save in xBt; yBt: max. order of expansion
      if((iE(kE).lt.iModExpMin).or.(iE(kE).gt.iModExpMax)) Cycle
      call DistPtBnd(Int2(iModExpBnd),0_2,xE(kE),yE(kE),.true.,.true.,dBE(kE),xAB,yAB,lInside,iD1,v1,iD2,sB(kE),kAB)
      sAB=sB(kE)
      ds=0.01d0*dBE(kE)
      ns=0
      ic1=setcolor(icl)
      do
        sAB=sAB-ds
        call GetBndPt(iModExpBnd,sAB,vAB(1),vAB(2),vh(1),vh(2),iDomL,iDomR,idum)
        call DistPtPt(xE(kE),yE(kE),vAB(1),vAB(2),rad)
        if((iabs(iDomL).gt.30000_2).or.(rad.gt.(dBE(kE)*1.4d0))) Exit
        call DrawLine(vAB(1),vAB(2),xE(kE),yE(kE))
        ns=ns+1
      end do
      sAB=sAB+ds
      sB1(kE)=sB(kE)-Dble(ns)*ds
      sAB=sB(kE)
      ns=0
      do
        sAB=sAB+ds
        call GetBndPt(iModExpBnd,sAB,vAB(1),vAB(2),vh(1),vh(2),iDomL,iDomR,idum)
        call DistPtPt(xE(kE),yE(kE),vAB(1),vAB(2),rad)
        if((iabs(iDomL).gt.30000_2).or.(rad.gt.(dBE(kE)*1.4d0))) Exit
        call DrawLine(vAB(1),vAB(2),xE(kE),yE(kE))
        ns=ns+1
      end do
      sAB=sAB-ds
      sB2(kE)=sB(kE)+Dble(ns)*ds
      icl=icl+10_2
      if(icl.gt.200_2) icl=icl-67_2
      xBt(kE)=Dble(ns)*ds/dBE(kE)
      xAver=xAver+xBt(kE)
      yBt(kE)=Dble(tExp(iE(kE))%iE(2)-tExp(iE(kE))%iE(1))
      yAver=yAver+yBt(kE)
    end do
    ic0=setcolor(ic0)
    xAver=xAver/Dble(mE)
    yAver=yAver/Dble(mE)
    if(lOrd) then
      do kE=1,mE ! modify maximum orders
        if((iE(kE).lt.iModExpMin).or.(iE(kE).gt.iModExpMax)) Cycle
        iO=tExp(iE(kE))%iE(1)+max(1_2,nint((xBt(kE)/xAver)*yAver,2))
        if((ixzSymm.ne.0).and.(abs(yE(kE)).lt.pSmall)) iO=2_2*iO
        if((iyzSymm.ne.0).and.(abs(xE(kE)).lt.pSmall)) iO=2_2*iO
        tExp(iE(kE))%iE(2)=min(iO,70_2)
      end do
      call CorrExpPar(1000_4)
    end if
  end Subroutine adaptExp2D2Ord

  Subroutine adaptExp2D2Gap(mE,iE,xE,yE,dBE,sB,sB1,sB2,sA,sE)
    Implicit none
    Integer(4) kE,mE,idum
    Integer(4) iE(mE)
    Integer(2) iDomL,iDomR,ic0
    Real(8) xE(mE),yE(mE),dBE(mE),sB(mE),sB1(mE),sB2(mE),vh(2),vh0(2),vAB(2),g1,g2,dg,dd,sA,sE
    ic0=setcolor(4_2)
    do kE=1,mE ! check gaps on boundary where no expansion associated
      if(kE.eq.1) then
        g1=sA-sB1(kE)
        g2=sB2(kE)-sB1(kE+1)
      else if(kE.eq.mE) then
        g1=sB2(kE-1)-sB1(kE)
        g2=sB2(kE)-sE
      else
        g1=sB2(kE-1)-sB1(kE)
        g2=sB2(kE)-sB1(kE+1)
      end if
      dg=0.0d0
      if((g1.gt.0.0d0).and.(g2.gt.0.0d0)) then ! no gap before and after kE
        dg=0.0d0
        dd=0.0d0
      else if((g1.lt.0.0d0).and.(g2.lt.0.0d0)) then ! gaps before and after
        dg=0.01d0*(g1-g2)
        dd=0.5d0*(-g1-g2)
      else if((g1.gt.0.0d0).and.(g2.lt.0.0d0)) then ! gap after
        dg=min(g1,-1.5d0*g2)
        dd=0.5d0*dabs(dg)
      else if((g1.lt.0.0d0).and.(g2.gt.0.0d0)) then ! gap before
        dg=-min(g2,-1.5d0*g1)
        dd=0.5d0*dabs(dg)
      end if
      dg=dabs(fModExpFac)*dg
      dd=dabs(fModExpFac)*dd
      sB(kE)=sB(kE)+dg
      call GetBndPt(iModExpBnd,sB(kE),vAB(1),vAB(2),vh(1),vh(2),iDomL,iDomR,idum)
      if(iabs(iDomL).gt.30000_2) Cycle
      dBE(kE)=dBE(kE)+dabs(dd)
      if(iDomExp.eq.iDomL) then
        vh0(1)=+dBE(kE)*vh(2)
        vh0(2)=-dBE(kE)*vh(1)
      else if(iDomExp.eq.iDomR) then
        vh0(1)=-dBE(kE)*vh(2)
        vh0(2)=+dBE(kE)*vh(1)
      else
        Cycle
      end if
      vh(1)=xE(kE)
      vh(2)=yE(kE)
      xE(kE)=vAB(1)+vh0(1)
      yE(kE)=vAB(2)+vh0(2)
      if((ixzSymm.ne.0).and.(yE(kE).lt.0.2d0*dBE(kE))) yE(kE)=0.0d0
      if((iyzSymm.ne.0).and.(xE(kE).lt.0.2d0*dBE(kE))) xE(kE)=0.0d0
      call DrawLine(vh(1),vh(2),xE(kE),yE(kE))
      call SetExpansion(iE(kE),xE(kE),yE(kE))
    end do
    ic0=setcolor(ic0)
  end Subroutine adaptExp2D2Gap

  Subroutine adaptExp2D3(ldum)
! Adapt 2D expansions with method 3: 
! Invert location of expansions inside the domain
    Implicit none
    Integer(4) i
    Integer(2) iDom0,iCol0,iCon0
    Logical, intent(in) :: ldum
    if(ldum.or.(abs(iModExpWFA).lt.1).or.(abs(iModExpWFA).gt.nFunA)) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
    do i=1,max(1,abs(iModExpLoop))
      call adaptExp2D3k(ldum)
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D3

  Subroutine adaptExp2D3k(ldum)
! Adapt 2D expansions with method 3
    Implicit none
    Real(8) vE(2),d,x,y,s
    Integer(2) iDm,ic0
    Integer(4) kE,kB
    Logical, intent(in) :: ldum
    Logical lInside
    if(ldum) return
! get boundary information for each expansion
    do kE=iModExpMax,iModExpMin,-1
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      iDm=tExp(kE)%iDom
      call DistPtDom(iDm,tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),d,x,y,s,kB,lInside)
      if(lInside) then
        vE(1)=2.0d0*x-tExp(kE)%Plane(1,0)
        vE(2)=2.0d0*y-tExp(kE)%Plane(2,0)
        call DistPtDom(iDm,vE(1),vE(2),d,x,y,s,kB,lInside)
        if(lInside) then
          vE(1)=(1.0d0+fModExpFac)*x-fModExpFac*tExp(kE)%Plane(1,0)
          vE(2)=(1.0d0+fModExpFac)*y-fModExpFac*tExp(kE)%Plane(2,0)
        end if
        ic0=setcolor(2_2)
        call DrawLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),vE(1),vE(2))
        ic0=setcolor(ic0)
        if(iModExpWFA.gt.0) then ! move to new position
          call SetExpansion(kE,vE(1),vE(2))
        end if
      end if
    end do
  end Subroutine adaptExp2D3k

  Subroutine adaptExp2D4(ldum)
! Adapt 2D expansions with method 4: 
! New distance from boundary = fModExpFac * old distance
    Implicit none
    Integer(4) i
    Integer(2) iDom0,iCol0,iCon0
    Logical, intent(in) :: ldum
    if(ldum.or.(abs(iModExpWFA).lt.1).or.(abs(iModExpWFA).gt.nFunA)) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
    do i=1,max(1,abs(iModExpLoop))
      call adaptExp2D4k(ldum)
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D4

  Subroutine adaptExp2D4k(ldum)
! Adapt 2D expansions with method 4
    Implicit none
    Real(8) vE(2),d,x,y,s,d1
    Integer(4) kE,kB
    Integer(2) iDm,ic0
    Logical, intent(in) :: ldum
    Logical lInside
    if(ldum) return
! get boundary information for each expansion
    do kE=iModExpMax,iModExpMin,-1
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      iDm=tExp(kE)%iDom
      call DistPtDom(iDm,tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),d,x,y,s,kB,lInside)
      vE(1)=(1.0d0-fModExpFac)*x+fModExpFac*tExp(kE)%Plane(1,0)
      vE(2)=(1.0d0-fModExpFac)*y+fModExpFac*tExp(kE)%Plane(2,0)
      call DistPtDom(iDm,vE(1),vE(2),d1,x,y,s,kB,lInside)
      if(lInside.or.((fModExpFac.gt.1.0d0).and.(d1.lt.d))) Cycle
      ic0=setcolor(2_2)
      call DrawLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),vE(1),vE(2))
      ic0=setcolor(ic0)
      if(iModExpWFA.gt.0) then ! move to new position
        call SetExpansion(kE,vE(1),vE(2))
      end if
    end do
  end Subroutine adaptExp2D4k

  Subroutine adaptExp2D5(ldum)
! Adapt 2D expansions with method 5: 
! Randomized location
    Implicit none
    Integer(4) i
    Integer(2) iDom0,iCol0,iCon0
    Logical, intent(in) :: ldum
    if(ldum.or.(abs(iModExpWFA).lt.1).or.(abs(iModExpWFA).gt.nFunA)) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
    do i=1,max(1,abs(iModExpLoop))
      call adaptExp2D5k(ldum)
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D5

  Subroutine adaptExp2D5k(ldum)
! Adapt 2D expansions with method 5
    Implicit none
    Real(8) vE(2),d,x,y,s,r,d1
    Real(4) rando
    Integer(4) kE,kB
    Integer(2) iDm,ic0
    Logical, intent(in) :: ldum
    Logical lInside
    if(ldum) return
! get boundary information for each expansion
    do kE=iModExpMax,iModExpMin,-1
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      iDm=tExp(kE)%iDom
      call DistPtDom(iDm,tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),d,x,y,s,kB,lInside)
      call random_number(rando)
      R=2.0d0*Dble(rando-0.5)*fModExpFac*d
      vE(1)=tExp(kE)%Plane(1,0)+R
      call random_number(rando)
      R=2.0d0*Dble(rando-0.5)*fModExpFac*d
      vE(2)=tExp(kE)%Plane(2,0)+R
      call DistPtDom(iDm,vE(1),vE(2),d1,x,y,s,kB,lInside)
      if(lInside) Cycle
      ic0=setcolor(2_2)
      call DrawLine(tExp(kE)%Plane(1,0),tExp(kE)%Plane(2,0),vE(1),vE(2))
      ic0=setcolor(ic0)
      if(iModExpWFA.gt.0) then ! move to new position
        call SetExpansion(kE,vE(1),vE(2))
      end if
    end do
  end Subroutine adaptExp2D5k

  Subroutine adaptExp2D6(ldum)
! Adapt 2D expansions with method 6: 
! Remove dependent expansions.
    Implicit none
    Integer(4) i
    Integer(2) iDom0,iCol0,iCon0,iDm
    Logical, intent(in) :: ldum
    if(ldum) return
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iColExp=iModExpCol
    iConExp=iModExpCon
    do iDm=1,nDom
      if((iModExpDom.gt.0).and.(iDm.ne.iModExpDom)) Cycle
      if((iModExpDom.lt.0).and.(iDm.gt.-iModExpDom)) Cycle
      iDomExp=iDm
      do i=1,max(1,abs(iModExpLoop))
        call adaptExp2D6k(iDm,i)
      end do
    end do
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D6

  Subroutine adaptExp2D6k(iDm,i)
! Adapt 2D expansions in domain iDm with method 6
    Implicit none
    Integer(4) kE,kB,i,idum,k,nE
    Real(8) f,d,sb,xb,yb
    Integer(2) iDm
    Logical lInside,ldum
    Real(8), allocatable:: xE(:),yE(:),dBE(:)
    if((iDm.lt.1).or.(iDm.gt.nDom)) return
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    f=abs(fModExpFac)*Dble(i)/Dble(max(1,abs(iModExpLoop)))
    nE=iModExpMax-iModExpMin+1
    if(nE.lt.1) return ! nothing to do
! deallocate and reallocate memory
    DeAllocate(xE,yE,dBE,stat=idum) 
    Allocate(xE(nExp),yE(nExp),dBE(nExp),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Adapt multipoles method 6'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(xE,yE,dBE,stat=idum) 
      return
    end if
! get boundary information for each expansion
    do kE=1,nExp 
      dBE(kE)=1.0d0
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      xE(kE)=tExp(kE)%Plane(1,0)
      yE(kE)=tExp(kE)%Plane(2,0)
      iDm=tExp(kE)%iDom
      call DistPtDom(iDm,xE(kE),yE(kE),dBE(kE),xB,yB,sB,kB,lInside)
      dBE(kE)=f*dBE(kE)
    end do
! set dBE of dependent expansions = -1
    do kE=iModExpMin,iModExpMax
      if(dBE(kE).lt.0.0d0) Cycle
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      do k=1,nExp
        if((k.eq.kE).or.(dBE(k).lt.0.0d0)) Cycle
        if(LSkipExp(k)) Cycle
        if((tExp(k)%iTypE.ne.1).or.(tExp(k)%iObj.ne.iModExpObj)) Cycle
        call DistPtPt(xE(k),yE(k),xE(kE),yE(kE),d)
        if(d.lt.dBE(kE)) dBE(k)=-1.0d0
      end do
    end do
! delete dependent expansions
    do kE=nExp,1,-1
      if(dBE(kE).lt.0.0d0) call InsertExp(kE,-1,ldum)
    end do
! deallocate memory
    DeAllocate(xE,yE,dBE,stat=idum) 
  end Subroutine adaptExp2D6k

  Subroutine adaptExp2D7(ldum)
! Adapt 2D expansions with method 7:
! Compute a "force" acting on the expansions from the values stored in the function array, column abs(iModExpWFA)
! Use it to adapt the maximum multipole orders
! Note: column 4 of the function array must contain the location (s) of the point along the boundaries
    Implicit none
    Integer(4) kE,nE,k1,k,idum
    Integer(2) iDom0,iCol0,iCon0,iDomL,iDomR
    Real(8) x,y,fmax,faver,fx,fy,sk,ds,f,xk,yk,vt1,vt2,dx,dy,d2,q
    Logical, intent(in) :: ldum
    if(ldum.or.(abs(iModExpWFA).lt.1).or.(abs(iModExpWFA).gt.nFunA)) return
    k1=kExp
    iModExpMin=max(1,iModExpMin)
    iModExpMax=min(nExp-nExc,iModExpMax)
    iDom0=iDomExp
    iCol0=iColExp
    iCon0=iConExp
    iDomExp=iModExpDom
    iColExp=iModExpCol
    iConExp=iModExpCon
    fmax=0.0d0
    faver=0.0d0
    nE=0
    do kE=iModExpMin,iModExpMax ! get fmax and faver
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      x=tExp(kE)%Plane(1,0)
      y=tExp(kE)%Plane(2,0)
      fx=0.0d0
      fy=0.0d0
      do k=1,nFun
        f=abs(Fun(abs(iModExpWFA),k))
        sk=Fun(4,k)
        if((k.gt.1).and.(k.lt.nFun)) then
          ds=0.5d0*(Fun(4,k+1)-Fun(4,k-1))
        else if(k.gt.1) then
          ds=Fun(4,k)-Fun(4,k-1)
        else
          ds=Fun(4,k+1)-Fun(4,k)
        end if
        call GetBndPt(0,sk,xk,yk,vt1,vt2,iDomL,iDomR,idum)
        dx=x-xk
        dy=y-yk
        d2=sqrt(dx**2+dy**2)**iModExpWFE
        fx=fx+f*ds*dx/d2
        fy=fy+f*ds*dy/d2
      end do
      f=dsqrt(fx**2+fy**2)
      if(f.gt.fmax) fmax=f
      faver=faver+f
      nE=nE+1
    end do
    faver=faver/Dble(max(1_4,nE))
    do kE=iModExpMin,iModExpMax ! set new orders
      if(LSkipExp(kE)) Cycle
      if((tExp(kE)%iTypE.ne.1).or.(tExp(kE)%iObj.ne.iModExpObj)) Cycle
      x=tExp(kE)%Plane(1,0)
      y=tExp(kE)%Plane(2,0)
      fx=0.0d0
      fy=0.0d0
      do k=1,nFun
        f=abs(Fun(abs(iModExpWFA),k))
        sk=Fun(4,k)
        if((k.gt.1).and.(k.lt.nFun)) then
          ds=0.5d0*(Fun(4,k+1)-Fun(4,k-1))
        else if(k.gt.1) then
          ds=Fun(4,k)-Fun(4,k-1)
        else
          ds=Fun(4,k+1)-Fun(4,k)
        end if
        call GetBndPt(0,sk,xk,yk,vt1,vt2,iDomL,iDomR,idum)
        dx=x-xk
        dy=y-yk
        d2=sqrt(dx**2+dy**2)**iModExpWFE
        fx=fx+f*ds*dx/d2
        fy=fy+f*ds*dy/d2
      end do
      f=dsqrt(fx**2+fy**2)
      if((f.lt.faver).and.(fModExpFac.gt.0.0d0)) Cycle
      q=abs(fModExpFac)*f/faver
      nE=nint(Dble(tExp(kE)%iE(2)+1)*q,4)-1
      tExp(kE)%iE(1)=0_2
      tExp(kE)%iE(2)=Int2(nE)
      kExp=kE
      call RepExpTest(4*nE+2)
    end do
    kExp=k1
    iDomExp=iDom0
    iColExp=iCol0
    iConExp=iCon0
  end Subroutine adaptExp2D7

! RECURRENCE OF HARMONIC FUNCTIONS

  subroutine sicom(m,si,co,sm,cm)
! recursive computation of cos(m*phi), sin(m*phi) etc.
  implicit none
  Integer(4) m,i
  Real(8) si,co,sm(0:m),cm(0:m),si1,si2,co1,co2
  sm(0)=0.0d0
  cm(0)=1.0d0
  if(m.lt.1) return
  sm(1)=si
  si1=si
  cm(1)=co
  co1=co
  do i=2,m
    si2=si1*co+co1*si
    co2=co1*co-si1*si
    sm(i)=si2
    cm(i)=co2
    si1=si2
    co1=co2
  end do
  end subroutine sicom

  subroutine sicomC(m,si,co,sm,cm)
! recursive computation of cos(m*phi), sin(m*phi) etc.
  implicit none
  Integer(4) m,i
  Complex(8) si,co,sm(0:m),cm(0:m),si1,si2,co1,co2
  sm(0)=(0.0d0,0.0d0)
  cm(0)=(1.0d0,0.0d0)
  if(m.lt.1) return
  sm(1)=si
  si1=si
  cm(1)=co
  co1=co
  do i=2,m
    si2=si1*co+co1*si
    co2=co1*co-si1*si
    sm(i)=si2
    cm(i)=co2
    si1=si2
    co1=co2
  end do
  end subroutine sicomC

END MODULE CHEXP
