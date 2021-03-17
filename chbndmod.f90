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
MODULE CHBND

! computation of boundaries

  USE CHGRA

  SAVE

  CONTAINS

! Graphics

  Subroutine TDrawBoundary(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    ldum=lCheck
    call WaitEndThread()
    iThreadAction=1
    call StartBNDThread(ldum)
  end Subroutine TDrawBoundary

  Subroutine StartBNDThread(ldi)
! start a new thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical(4), intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start boundary thread'C)
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
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start boundary thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start boundary thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(BNDThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start boundary thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
      return
    end if
  end Subroutine StartBNDThread

  Integer(4) Function BNDThread(iWhat)
! tread calls.....
    Implicit none
    Integer(4) iWhat
    Logical(4) ldum
    Include 'resource.fd'
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    BNDThread=0_4
    if(iThreadAction.eq.1) then
      call MTDrawBoundary(ldum)
    end if
    call endThread()
  end Function BNDThread

  Subroutine DrawBoundary(kPl)
    Implicit none
    Real(8) x1,x2,dx,e,vt(2),x,y,xs,ys,s,ds,x0,y0,xPt,yPt,xPtn,yPtn
    Integer(4) kPl,kP,k1,k2,kE,nE,i,m,istat,nxP,idum,iClose
    Integer(2) iDomL,iDomR,ic0,ic
    lWinFld(kWin)=.true.
    lGRF_Mark=.false.
    if(iWinAction.eq.1_2) lGRF_Mark=.true.
! set window, auto determine borders
    call GetKWin(.false.)
    istat=SetActiveQQ(10+kWin)
! get pixel size
    call I2R(10_2,10_2,x1,dx)
    call I2R(11_2,10_2,x2,dx)
    dx=4.0d0*(x2-x1)
! determine min/max polygon numbers
    if(kPl.lt.1) then
      if(kPl.eq.0) then
        k1=1
        k2=nBnd
      else
        k1=1
        k2=Min(nBnd,Max(1,kPl))
      end if
    else
      if(kPl.gt.nBnd) return
      k1=kPl
      k2=k1
    end if
    call cBndGetABO() ! get c-poly, splines, match.pts
! lines in the matching points, normal to the boundary
    if((kPl.eq.0).and.(dAbs(errorScale).gt.pSmall)) then
      x2=BndLtot
      nxP=nBndPt
      do i=1,nxP
        x1=sBndPt(i)
        x1=dmin1(x1,x2*0.9999999999999d0)
        call GetBndPt(0,x1,xPt,yPt,vt(1),vt(2),iDomL,iDomR,idum)
        if(abs(iDomL).gt.30000_2) Cycle
        call unit2DV(vt)
        if(AerrorM.gt.pSmall) then
          if(eBndPt(i).lt.pSmall) then
            e=0.0d0
          else
            if(errorScale.lt.0.0d0) then
              e=-errorScale*(eBndPt(i).div.fBndPt(i))
            else
              e=errorScale*eBndPt(i)
            !  e=errorScale*(eBndPt(i).div.AerrorM)
            end if
          end if
        else
          e=2.0d0*dx
        end if
        if(idomR.gt.0) then
          if(iDom(Min(iDomR,Int2(nDom))).gt.0) then 
            xPtn=xPt+vt(2)*e
            yPtn=yPt-vt(1)*e
            ic0=SetColor(iDom(Min(iDomR,Int2(nDom))))
            if(ic0.gt.0) call Draw23DLine(xPt,yPt,xPtn,yPtn)
            ic0=SetColor(ic0)
          end if
        end if
        if(idomL.gt.0) then
          if(iDom(Min(iDomL,Int2(nDom))).gt.0) then 
            xPtn=xPt-vt(2)*e
            yPtn=yPt+vt(1)*e
            ic0=SetColor(iDom(Min(iDomL,Int2(nDom))))
            call Draw23DLine(xPt,yPt,xPtn,yPtn)
            ic0=SetColor(ic0)
          end if
        end if
      end do
    end if
! loop over polygons
    ic0=SetColor(1_2)
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      ic=SetColor(tBnd(kP)%iCol)
      if((iBound.gt.2_2).and.((tBnd(kP)%nSpline.gt.1).or.((tBnd(kP)%nSpline.gt.0).and.(tBnd(kP)%nEdge.gt.1)))) then
        x0=xSpline(tBnd(kP)%iSplineOffset+1)
        y0=ySpline(tBnd(kP)%iSplineOffset+1)
        m=tBnd(kP)%nSpline
        if(m.lt.2) m=tBnd(kP)%nEdge
        iClose=0
        if(iiabs(tBnd(kP)%iTypB).eq.1_2) iClose=1
        m=m+iClose
        s=sSpline(tBnd(kP)%iSplineOffset+1)
        ds=(sSpline(tBnd(kP)%iSplineOffset+m)-sSpline(tBnd(kP)%iSplineOffset+1))/dble(5*m)
        do i=0,5*m
          if(iClose.gt.0) then
            call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
            &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
            &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,s,x,y,xs,ys)
          else
            call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
            &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
            &   ysSpline(tBnd(kP)%iSplineOffset+1),1,m,s,x,y,xs,ys)
          end if
          call Draw23DLine(x0,y0,x,y)
          s=s+ds
          x0=x
          y0=y
        end do
        if(.not.lGRF_Mark) Cycle
      end if
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.lt.1) Cycle
      if(tBnd(kP)%nEdge.eq.1) then ! circle
        call Draw23DCircleM(tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE)%r,dx)
        Cycle
      end if
      if(tBnd(kP)%nEdge.eq.2) then
        call Draw23DLineM(tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE+1)%x,tBndEdg(kE+1)%y,dx)
        Cycle
      end if
      if(iiabs(tBnd(kP)%iTypB).eq.1_2) then
        if(lGRF_Mark) then
          call Draw23DLineM(tBndEdg(nE)%x,tBndEdg(nE)%y,tBndEdg(kE)%x,tBndEdg(kE)%y,dx)
        else
          call Draw23DLineM(tBndEdg(nE)%xb,tBndEdg(nE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya,dx)
        end if
        if(tBndEdg(kE)%r.gt.pSmall) then
          if(tBndEdg(kE)%iOrient.eq.1_2) then
            call Draw23DArcM(tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xa,tBndEdg(kE)%ya, &
            &             tBndEdg(kE)%xb,tBndEdg(kE)%yb,dx)
          else
            call Draw23DArcM(tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xb,tBndEdg(kE)%yb, &
            &             tBndEdg(kE)%xa,tBndEdg(kE)%ya,dx)
          end if
        end if
      end if
      do i=kE+1,nE
        if(lGRF_Mark) then
          call Draw23DLineM(tBndEdg(i-1)%x,tBndEdg(i-1)%y,tBndEdg(i)%x,tBndEdg(i)%y,dx)
        else
          call Draw23DLineM(tBndEdg(i-1)%xb,tBndEdg(i-1)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya,dx)
        end if
        if(tBndEdg(i)%r.gt.pSmall) then
          if(tBndEdg(i)%iOrient.eq.1_2) then
            call Draw23DArcM(tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xa,tBndEdg(i)%ya, &
            &             tBndEdg(i)%xb,tBndEdg(i)%yb,dx)
          else
            call Draw23DArcM(tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xb,tBndEdg(i)%yb, &
            &             tBndEdg(i)%xa,tBndEdg(i)%ya,dx)
          end if
        end if
      end do
    end do
    ic0=SetColor(ic0)
    lGRF_Mark=.true.
  end Subroutine DrawBoundary

  Subroutine MTDrawBoundary(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
    lWinFld(kWin)=.true.
    if(.not.lCheck) then
      idum=MessageBoxQQ('Clear graphic window first?'C,'Draw boundary'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) then
        call LeaveCriticalSection(Loc(DrawLock))
        return
      end if
      if(idum.eq.MB$IDYES) then
        call DrawWindow(lCheck)
      end if
    end if
    call DrawBoundary(iDraBnd)
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTDrawBoundary

  Subroutine MTModifyBoundary(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    if(.not.lCheck) then
      idum=MessageBoxQQ('Set view plane = xy plane?'C,'Modify boundary'C, &
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
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iWinAction=1_2
    call MTDrawBoundary(lCheck)
    ldum=ModifyMenuFlagsQQ(3,1,$MenuChecked)   !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuUnChecked) !nothing
		call OutTxt('t1','Modify boundary!'C)
  end Subroutine MTModifyBoundary

! I/O

  Subroutine SaveBoundary(lCheck)
! save Boundary data in a file
    Implicit none
    Logical(4), intent(in) :: lCheck
	  Integer(4) iOk,ios,i,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select Boundary data file to be written!','Boundary data file ',BndFileName,'BND',ios)
      if(ios.gt.0) return
    end if
    open(1,file=BndFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save boundary'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHBndIdent,iOK)
    ich(1)=nBnd
    sch(1:11)=' boundaries'
    call chwrit2(1,ich,1,rch,0,sch,11,iOK)
    do i=1,nBnd
      ich(1)=tBnd(i)%nEdge
      ich(2)=Int4(tBnd(i)%iLDom)
      ich(3)=Int4(tBnd(i)%iRDom)
      ich(4)=Int4(tBnd(i)%iCond)
      ich(5)=Int4(tBnd(i)%iConn)
      ich(6)=Int4(tBnd(i)%iTypB)
      ich(7)=Int4(tBnd(i)%iCol)
      ich(8)=tBnd(i)%nMatPts
      call chwrit2(1,ich,8,rch,0,sch,0,iOK)
      rch(1)=tBnd(i)%Weight
      rch(2)=tBnd(i)%val(1)
      rch(3)=tBnd(i)%val(2)
      rch(4)=tBnd(i)%Weight2
      call chwrit2(1,ich,0,rch,4,sch,0,iOK)
      if(tBnd(i)%iTypB.lt.0_2) then
        ich(1)=tBnd(i)%nSpline
        rch(1)=tBnd(i)%fAmpl
        call chwrit2(1,ich,1,rch,1,sch,0,iOK)
        call WriteStr(1,tBnd(i)%Formula,iOK)
      end if
    end do
    ich(1)=nBndEdg
    sch(1:8)=' corners'
    call chwrit2(1,ich,1,rch,0,sch,8,iOK)
    do i=1,nBndEdg
      ich(1)=tBndEdg(i)%iOrient
      rch(1)=tBndEdg(i)%x
      rch(2)=tBndEdg(i)%y
      rch(3)=tBndEdg(i)%r
      rch(4)=tBndEdg(i)%d
      rch(5)=tBndEdg(i)%xa
      rch(6)=tBndEdg(i)%ya
      rch(7)=tBndEdg(i)%xb
      rch(8)=tBndEdg(i)%yb
      rch(9)=tBndEdg(i)%xo
      rch(10)=tBndEdg(i)%yo
      call chwrit2(1,ich,1,rch,10,sch,0,iOK)
    end do
    ich(1)=nBndSpl
    sch(1:8)=' splines'
    call chwrit2(1,ich,1,rch,0,sch,8,iOK)
    do i=1,nBndSpl
      rch(1)=sSpline(i)
      rch(2)=xSpline(i)
      rch(3)=ySpline(i)
      rch(4)=xsSpline(i)
      rch(5)=ysSpline(i)
      call chwrit2(1,ich,0,rch,5,sch,0,iOK)
    end do
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveBoundary

  Subroutine OpenBoundary(lCheck)
! read Boundary data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist,lTxt
	  Integer(4) iVers
	  Integer(4) iOk,ios,i,i1,idum
    Character(20) text
    if(lAskBnd.or.(.not.lCheck)) then
      nInsObj=nBnd
      call InsertDialog(.true.)
      if(kInsObj.lt.0) return
      lInsertBnd=lInsObj
    end if
    if(.not.lCheck) then
      call Open2read(-1,'Select Boundary data file to be read!','Boundary data file ',BndFileName,'BND',ios)
      if(ios.gt.0) return
    end if
    inquire(file=BndFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=BndFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open boundary'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    call StrToInt(text(16:16)//text(18:18),iVers,iOK)
    lTxt=.false.
    if(CHBndIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open boundary'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      else ! read as primitive TXT file: Dummy line / number of polygons / for each polygon: number of points / for each point: x y
        lTxt=.true.
        iVers=0
      end if
    end if
    errorM=0.0d0
    errorMA=0.0d0
    errorA=0.0d0
    AerrorM=0.0d0
    AerrorA=0.0d0
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of Boundarys)'C,'Open boundary'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    nInsObj=ich(1)
    if(lInsertBnd) then
      i=kInsObj
    else
      i=0
      nBnd=0
      nBndEdg=0
    end if
    do i1=1,nInsObj
      if(lTxt) then
        call chread2(1,ich,1,rch,0,iOK)
        ich(2)=1
        ich(3)=2
        ich(4)=0
        ich(5)=0
        ich(6)=-2
        ich(7)=ich(1)
        ich(8)=0
      else if(iVers.gt.19) then
        call chread2(1,ich,8,rch,0,iOK)
      else
        call chread2(1,ich,7,rch,0,iOK)
        ich(8)=0
      end if
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Boundary data)'C,'Open boundary'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      call InsertBnd(i,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert boundary!'C,'Open boundary'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      i=i+1
      tBnd(i)%nEdge=0
      tBnd(i)%iLDom=Int2(ich(2))
      tBnd(i)%iRDom=Int2(ich(3))
      tBnd(i)%iCond=Int2(ich(4))
      call GetBndBC(i)
      tBnd(i)%iConn=Int2(ich(5))
      tBnd(i)%iTypB=Int2(ich(6))
      tBnd(i)%iCol=Int2(ich(7))
      tBnd(i)%nMatPts=ich(8)
      if(tBnd(i)%iTypB.eq.0_2) tBnd(i)%iTypB=2_2
      if(lTxt) then
        tBnd(i)%Weight=1.0
        tBnd(i)%val(1)=0.0
        tBnd(i)%val(2)=0.0
        tBnd(i)%Weight2=tBnd(i)%Weight
      else if(iVers.gt.20) then
        call chread2(1,ich,0,rch,4,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Boundary data)'C,'Open boundary'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        tBnd(i)%Weight=rch(1)
        tBnd(i)%val(1)=rch(2)
        tBnd(i)%val(2)=rch(3)
        tBnd(i)%Weight2=rch(4)
      else
        call chread2(1,ich,0,rch,3,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Boundary data)'C,'Open boundary'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        tBnd(i)%Weight=rch(1)
        tBnd(i)%val(1)=rch(2)
        tBnd(i)%val(2)=rch(3)
        tBnd(i)%Weight2=tBnd(i)%Weight
      end if
      call InsertBndEdg(i,0,ich(1),ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert corner!'C,'Open boundary'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tBnd(i)%nSpline=0
      tBnd(i)%fAmpl=0.0d0
      tBnd(i)%Formula='0'C
      ich(1)=0
      if(lTxt) then
        tBnd(i)%nSpline=1
        tBnd(i)%fAmpl=0.0d0
      else if(tBnd(i)%iTypB.lt.0_2) then
        call chread2(1,ich,1,rch,1,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Boundary formula)'C,'Open boundary'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        tBnd(i)%nSpline=ich(1)
        tBnd(i)%fAmpl=rch(1)
        call ReadStr(1,tBnd(i)%Formula,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Boundary formula)'C,'Open boundary'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
      end if
    end do
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of Corners)'C,'Open boundary'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    nInsObj=ich(1)
    if(lInsertBnd) then
      i=tBnd(kInsObj+1)%iEdgeOffset
    else
      i=0
    end if
    do i1=1,nInsObj
      if(lTxt) then
        call chread2(1,ich,0,rch,2,iOK)
        ich(1)=1
        rch(3:10)=0.0d0
      else if(ivers.gt.29) then
        call chread2(1,ich,1,rch,10,iOK)
      else
        call chread2(1,ich,1,rch,3,iOK)
        rch(4:10)=0.0d0
      end if
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Corner data)'C,'Open boundary'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      i=i+1
      tBndEdg(i)%iOrient=ich(1)
      tBndEdg(i)%x=rch(1)
      tBndEdg(i)%y=rch(2)
      tBndEdg(i)%r=rch(3)
      tBndEdg(i)%d=rch(4)
    end do
    close(1)
    kBnd=nBnd
    kBndEdg=nBndEdg
    call cBndGetABO()
  end Subroutine OpenBoundary

  Subroutine SaveMaS(lCheck)
! save Boundary data in MaS files
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) iOk,ios,i,n,idum,n1
	  Integer(2) iDL,iDR
    Real(8) x,y,s1,s2,ds,s,vt1,vt2,rc
    if(.not.lCheck) then
      call Open2write(-1,'Select Contour data file to be written!','Contour data file ',ConFileName,'DAT',ios)
      if(ios.gt.0) return
    end if
    open(1,file=ConFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save contour'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    n=abs(tBnd(kBnd)%nSpline)
    if(n.lt.2) n=1000
    n1=n
    if(iiabs(tBnd(kBnd)%iTypB).eq.1_2) n1=n+1
    ich(1)=n1
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    s1=tBnd(kBnd)%Start
    s2=s1+tBnd(kBnd)%sLength
    ds=(s2-s1)/Dble(n)
    s=s1-0.5d0*ds
    do i=1,n
      s=s+ds
      call GetBndPt(kBnd,s,x,y,vt1,vt2,iDL,iDR,idum,rc)
      rch(1)=x
      rch(2)=y
      rch(3)=vt1
      rch(4)=vt2
      rch(5)=rc
      rch(6)=s
      rch(7)=ds
      call chwrit2(1,ich,0,rch,7,sch,0,iOK)
    end do
    if(n1.gt.n) then
      s=s1+0.5d0*ds
      call GetBndPt(kBnd,s,x,y,vt1,vt2,iDL,iDR,idum,rc)
      rch(1)=x
      rch(2)=y
      rch(3)=vt1
      rch(4)=vt2
      rch(5)=rc
      rch(6)=s
      rch(7)=ds
      call chwrit2(1,ich,0,rch,7,sch,0,iOK)
    end if
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveMaS

! boundary operations

  Subroutine SetBoundaryAmp(kPl,v1,kB2)
    Implicit none
    Real(8) v1
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%fAmpl=v1
    end do
  end Subroutine SetBoundaryAmp

  Subroutine SetBoundaryCnd(kPl,str,spe,kB2)
    Implicit none
    Integer(2) iC
    Integer(4) kPl,kP,k1,k2
    Character(3) str
    Character(16) spe
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      select case (str(1:3))
      case('spe')
        iC=0_2
        if(spe(1:2).eq.'et') iC=iC+1_2
        if(spe(3:4).eq.'ez') iC=iC+2_2
        if(spe(5:6).eq.'dn') iC=iC+4_2
        if(spe(7:8).eq.'ht') iC=iC+8_2
        if(spe(9:10).eq.'hz') iC=iC+16_2
        if(spe(11:12).eq.'bn') iC=iC+32_2
        if(spe(13:14).eq.'az') iC=iC+64_2
        if(spe(15:15).eq.'v') iC=iC+128_2
        tBnd(kP)%iCond=iC
      case('sib')
        tBnd(kP)%iCond=-1_2
      case('x-p')
        tBnd(kP)%iCond=-101_2
      case('y-p')
        tBnd(kP)%iCond=-102_2
      case('z-p')
        tBnd(kP)%iCond=-103_2
      case default ! usual
        tBnd(kP)%iCond=0_2
      end select
    end do
  end Subroutine SetBoundaryCnd

  Subroutine SetBoundaryCol(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%iCol=Int2(ic)
    end do
  end Subroutine SetBoundaryCol

  Subroutine SetBoundaryCon(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%iConn=Int2(ic)
    end do
  end Subroutine SetBoundaryCon

  Subroutine SetBoundaryDom(kPl,ic,jc,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic,jc
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%iLDom=Int2(ic)
      tBnd(kP)%iRDom=Int2(jc)
    end do
  end Subroutine SetBoundaryDom

  Subroutine SetBoundaryFor(kPl,str,lstr,kB2)
    Implicit none
    Integer(4) kPl,lstr,kP,k1,k2,idum,lfdum
    Integer(4), Optional:: kB2
    character(256) str
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%Formula=str(1:lstr)//char(0)
      cForm(0)=0.0d0
      cForm(1)=Pi
      vForm(0)=0.789123456 ! any value between 0 and 1 is OK
      vForm(1)=vForm(0)
      call checkFormula(tBnd(kP)%Formula,lfdum,1,0,1,idum)
    end do
  end Subroutine SetBoundaryFor

  Subroutine IncBoundaryAmp(kPl,v1,kB2)
    Implicit none
    Real(8) v1
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%fAmpl=tBnd(kP)%fAmpl+v1
    end do
  end Subroutine IncBoundaryAmp

  Subroutine MulBoundaryAmp(kPl,v1,kB2)
    Implicit none
    Real(8) v1
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%fAmpl=tBnd(kP)%fAmpl*v1
    end do
  end Subroutine MulBoundaryAmp

  Subroutine SetBoundaryPts(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%nMatPts=ic
    end do
  end Subroutine SetBoundaryPts

  Subroutine IncBoundaryPts(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%nMatPts=tBnd(kP)%nMatPts+ic
    end do
  end Subroutine IncBoundaryPts

  Subroutine MulBoundaryPts(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%nMatPts=tBnd(kP)%nMatPts*ic
    end do
  end Subroutine MulBoundaryPts

  Subroutine SetBoundarySpl(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%nSpline=ic
    end do
  end Subroutine SetBoundarySpl

  Subroutine IncBoundarySpl(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%nSpline=tBnd(kP)%nSpline+ic
    end do
  end Subroutine IncBoundarySpl

  Subroutine MulBoundarySpl(kPl,ic,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ic
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%nSpline=tBnd(kP)%nSpline*ic
    end do
  end Subroutine MulBoundarySpl

  Subroutine SetBoundaryVal(kPl,v1,v2,kB2)
    Implicit none
    Real(8) v1,v2
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%val(1)=v1
      tBnd(kP)%val(2)=v2
    end do
  end Subroutine SetBoundaryVal

  Subroutine IncBoundaryVal(kPl,v1,v2,kB2)
    Implicit none
    Real(8) v1,v2
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%val(1)=tBnd(kP)%val(1)+v1
      tBnd(kP)%val(2)=tBnd(kP)%val(2)+v2
    end do
  end Subroutine IncBoundaryVal

  Subroutine MulBoundaryVal(kPl,v1,v2,kB2)
    Implicit none
    Real(8) v1,v2
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%val(1)=tBnd(kP)%val(1)*v1
      tBnd(kP)%val(2)=tBnd(kP)%val(2)*v2
    end do
  end Subroutine MulBoundaryVal

  Subroutine SetBoundaryWei(kPl,v1,v2,kB2)
    Implicit none
    Real(8) v1,v2
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%weight=v1
      tBnd(kP)%weight2=v2
    end do
  end Subroutine SetBoundaryWei

  Subroutine IncBoundaryWei(kPl,v1,v2,kB2)
    Implicit none
    Real(8) v1,v2
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%weight=tBnd(kP)%weight+v1
      tBnd(kP)%weight2=tBnd(kP)%weight2+v2
    end do
  end Subroutine IncBoundaryWei

  Subroutine MulBoundaryWei(kPl,v1,v2,kB2)
    Implicit none
    Real(8) v1,v2
    Integer(4) kPl,kP,k1,k2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      tBnd(kP)%weight=tBnd(kP)%weight*v1
      tBnd(kP)%weight2=tBnd(kP)%weight2*v2
    end do
  end Subroutine MulBoundaryWei

  Subroutine BlowBoundary(kPl,x,y,f,kB2)
    Implicit none
    Real(8) f,x,y
    Integer(4) kPl,kP,k1,k2,kE,nE
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      tBndEdg(kE:nE)%x=x+(tBndEdg(kE:nE)%x-x)*f
      tBndEdg(kE:nE)%y=y+(tBndEdg(kE:nE)%y-y)*f
      tBndEdg(kE:nE)%r=tBndEdg(kE:nE)%r*f
      tBnd(kP)%fAmpl=tBnd(kP)%fAmpl*f
    end do
    call cBndGetABO()
  end Subroutine BlowBoundary

  Subroutine MoveBoundary(kPl,dx,dy,kB2)
    Implicit none
    Real(8) dx,dy
    Integer(4) kPl,kP,k1,k2,kE,nE
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      tBndEdg(kE:nE)%x=tBndEdg(kE:nE)%x+dx
      tBndEdg(kE:nE)%y=tBndEdg(kE:nE)%y+dy
    end do
    call cBndGetABO()
  end Subroutine MoveBoundary

  Subroutine ReflBoundary(kPl,ix,kB2)
    Implicit none
    Integer(4) kPl,kP,k1,k2,kE,nE,ix
    Integer(2) i2
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(ix.eq.1) then
        tBndEdg(kE:nE)%x=-tBndEdg(kE:nE)%x
      else
        tBndEdg(kE:nE)%y=-tBndEdg(kE:nE)%y
      end if
      i2=tBnd(kP)%iRDom
      tBnd(kP)%iRDom=tBnd(kP)%iLDom
      tBnd(kP)%iLDom=i2
    end do
    call cBndGetABO()
  end Subroutine ReflBoundary

  Subroutine RotateBoundary(kPl,xo,yo,fo,ldegree,kB2)
    Implicit none
    Real(8) xo,yo,fo,f,x,y,r
    Integer(4) kPl,kP,k1,k2,kE,nE,i
    Logical, intent(in) :: ldegree
    Integer(4), Optional:: kB2
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      do i=kE,nE
        x=tBndEdg(i)%x-xo
        y=tBndEdg(i)%y-yo
        call Cart2Pol(x,y,r,f)
        if(ldegree) then
          f=f+Pi*fo/180.0d0
        else
          f=f+fo
        end if
        call Pol2Cart(r,f,x,y)
        tBndEdg(i)%x=xo+x
        tBndEdg(i)%y=yo+y
      end do
    end do
    call cBndGetABO()
  end Subroutine RotateBoundary

  Subroutine BlowCorner(kPl,kEd,x,y,f,kB2)
    Implicit none
    Real(8) f,x,y
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%x=x+(tBndEdg(kE)%x-x)*f
      tBndEdg(kE)%y=y+(tBndEdg(kE)%y-y)*f
      tBndEdg(kE)%r=tBndEdg(kE)%r*f
    end do
    call cBndGetABO()
  end Subroutine BlowCorner

  Subroutine ReflCorner(kPl,kEd,ix,kB2)
    Implicit none
    Integer(4) kPl,kEd,kE,k1,k2,kP,ix
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      if(ix.eq.1) then
        tBndEdg(kE)%x=-tBndEdg(kE)%x
      else
        tBndEdg(kE)%y=-tBndEdg(kE)%y
      end if
    end do
    call cBndGetABO()
  end Subroutine ReflCorner

  Subroutine MoveCorner(kPl,kEd,dx,dy,kB2)
    Implicit none
    Real(8) dx,dy
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%x=tBndEdg(kE)%x+dx
      tBndEdg(kE)%y=tBndEdg(kE)%y+dy
    end do
    call cBndGetABO()
  end Subroutine MoveCorner

  Subroutine RotateCorner(kPl,kEd,xo,yo,fo,ldegree,kB2)
    Implicit none
    Real(8) xo,yo,fo,f,x,y,r
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Logical, intent(in) :: ldegree
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      x=tBndEdg(kE)%x-xo
      y=tBndEdg(kE)%y-yo
      call Cart2Pol(x,y,r,f)
      if(ldegree) then
        f=f+Pi*fo/180.0d0
      else
        f=f+fo
      end if
      call Pol2Cart(r,f,x,y)
      tBndEdg(kE)%x=xo+x
      tBndEdg(kE)%y=yo+y
    end do
    call cBndGetABO()
  end Subroutine RotateCorner

  Subroutine SetCorner(kPl,kEd,x,y,r,kB2)
    Implicit none
    Real(8) x,y,r
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%x=x
      tBndEdg(kE)%y=y
      tBndEdg(kE)%r=r
    end do
    call cBndGetABO()
  end Subroutine SetCorner

  Subroutine SetCornerR(kPl,kEd,r,kB2)
    Implicit none
    Real(8) r
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%r=r
    end do
    call cBndGetABO()
  end Subroutine SetCornerR

  Subroutine IncCornerR(kPl,kEd,r,kB2)
    Implicit none
    Real(8) r
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%r=tBndEdg(kE)%r+r
    end do
    call cBndGetABO()
  end Subroutine IncCornerR

  Subroutine MulCornerR(kPl,kEd,r,kB2)
    Implicit none
    Real(8) r
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%r=tBndEdg(kE)%r*r
    end do
    call cBndGetABO()
  end Subroutine MulCornerR

  Subroutine BloCornerR(kPl,kEd,fac,kB2)
    Implicit none
    Real(8) fac
    Integer(4) kPl,kEd,kE,k1,k2,kP
    Integer(4), Optional:: kB2
    if(kEd.lt.1) return
    if(Present(kB2)) then
      call GetRange(kPl,nBnd,k1,k2,kB2)
    else
      call GetRange(kPl,nBnd,k1,k2)
    end if
    do kP=k1,k2
      if(kEd.gt.tBnd(kP)%nEdge) Cycle
      kE=kEd+tBnd(kP)%iEdgeOffset
      tBndEdg(kE)%r=tBndEdg(kE)%r*fac
    end do
    call cBndGetABO()
  end Subroutine BloCornerR

  Subroutine cBndGetABO()
! get locations of centers of circles etc. of a 2D boundary
    Implicit none
    Real(8) ea(2),eb(2),ew(2),a,dum,dmin
    Integer(4) kP,kE,nE,i
    Logical ldum
    iBound=-5_2 ! ignore splines
    do kP=1,nBnd
      if(LSkipBnd(kP)) Cycle
      call getBndBC(kP)
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      tBndEdg(kE:nE)%xo=tBndEdg(kE:nE)%x
      tBndEdg(kE:nE)%yo=tBndEdg(kE:nE)%y
      tBndEdg(kE:nE)%xa=tBndEdg(kE:nE)%x
      if(tBnd(kP)%nEdge.eq.1) tBndEdg(kE)%xa=tBndEdg(kE)%x+tBndEdg(kE)%r
      tBndEdg(kE:nE)%ya=tBndEdg(kE:nE)%y
      tBndEdg(kE:nE)%xb=tBndEdg(kE:nE)%xa
      tBndEdg(kE:nE)%yb=tBndEdg(kE:nE)%ya
      tBndEdg(kE:nE)%iOrient=1_2
      if(tBnd(kP)%nEdge.eq.1) then ! circle
        if(iiabs(tBnd(kP)%iTypB).ne.1_2) tBnd(kP)%iTypB=iisign(1_2,tBnd(kP)%iTypB) ! set iType = 1
        Cycle
      end if
      do while(kE.lt.nE)                ! remove zero length sides of polygon
        kE=kE+1
        ea(1)=tBndEdg(kE)%x-tBndEdg(kE-1)%x
        ea(2)=tBndEdg(kE)%y-tBndEdg(kE-1)%y
        a=r2Vec_Length(ea)
        if(a.lt.pSmall) then
          call InsertBndEdg(kP,kE-tBnd(kP)%iEdgeOffset,-1,ldum)
          nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
        end if
      end do
      if((iiabs(tBnd(kP)%iTypB).ne.1_2).and.(tBnd(kP)%nEdge.eq.1)) then ! circle -> set iType=1
        tBnd(kP)%iTypB=iisign(1_2,tBnd(kP)%iTypB)
        Cycle
      end if
      if((iiabs(tBnd(kP)%iTypB).eq.1_2).and.(tBnd(kP)%nEdge.eq.2)) tBnd(kP)%iTypB=2_2*tBnd(kP)%iTypB
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.eq.2) then     ! line of zero length -> circle
        ea(1)=tBndEdg(kE)%x-tBndEdg(nE)%x
        ea(2)=tBndEdg(kE)%y-tBndEdg(nE)%y
        if(iWinAction.eq.1_2) then
	        call I2R(2_2,1_2,dmin,dum)
	        call I2R(1_2,1_2,a,dum)
          dmin=dabs(dmin-a)
        else
          dmin=1.0d-150
        end if
        a=r2Vec_Length(ea)
        if(a.lt.dmin) then
          call InsertBndEdg(kP,nE-tBnd(kP)%iEdgeOffset,-1,ldum)
        end if
        Cycle
      end if
      if((iiabs(tBnd(kP)%iTypB).ne.1_2).and.(tBnd(kP)%nEdge.eq.1)) then ! circle -> set iType=1
        tBnd(kP)%iTypB=iisign(1_2,tBnd(kP)%iTypB)
        Cycle
      end if
      if((iiabs(tBnd(kP)%iTypB).eq.1_2).and.(tBnd(kP)%nEdge.eq.2)) tBnd(kP)%iTypB=2_2*tBnd(kP)%iTypB
      if(lCloseNow.and.(iiabs(tBnd(kP)%iTypB).ne.1_2).and.(tBnd(kP)%nEdge.gt.3)) then
        ea(1)=tBndEdg(kE)%x-tBndEdg(nE)%x ! if open polygon is closed: remove last edge
        ea(2)=tBndEdg(kE)%y-tBndEdg(nE)%y
	      call I2R(2_2,1_2,dmin,dum)
	      call I2R(1_2,1_2,a,dum)
        dmin=dabs(dmin-a)
        a=r2Vec_Length(ea)
        if(a.lt.dmin) then
          call InsertBndEdg(kP,nE-tBnd(kP)%iEdgeOffset,-1,ldum)
          if(iiabs(tBnd(kP)%iTypB).eq.2_2) tBnd(kP)%iTypB=tBnd(kP)%iTypB/2_2
          nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
        end if
      end if
      do i=kE+1,nE-1                    ! inner corners of polygon
        ea(1)=tBndEdg(i)%x-tBndEdg(i-1)%x
        ea(2)=tBndEdg(i)%y-tBndEdg(i-1)%y
        call Unit2DV(ea)
        eb(1)=tBndEdg(i)%x-tBndEdg(i+1)%x
        eb(2)=tBndEdg(i)%y-tBndEdg(i+1)%y
        call Unit2DV(eb)
        ew=r2Vec_Ortho(ea)
        a=r2Scl_Prod(eb,ew)
        if(a.gt.0.0d0) tBndEdg(i)%iOrient=0_2
        ew=ea+eb
        call Unit2DV(ew)
        a=r2Scl_Prod(ea,ew)
        a=a.div.(.srt.(1.0d0-a**2))
        a=a*tBndEdg(i)%r
        tBndEdg(i)%xa=tBndEdg(i)%x-a*ea(1)
        tBndEdg(i)%ya=tBndEdg(i)%y-a*ea(2)
        tBndEdg(i)%xb=tBndEdg(i)%x-a*eb(1)
        tBndEdg(i)%yb=tBndEdg(i)%y-a*eb(2)
        ea(1)=a
        ea(2)=tBndEdg(i)%r
        a=r2Vec_Length(ea)
        tBndEdg(i)%xo=tBndEdg(i)%x-a*ew(1)
        tBndEdg(i)%yo=tBndEdg(i)%y-a*ew(2)
      end do
      if(iiabs(tBnd(kP)%iTypB).eq.1_2) then   ! first+last corner of closed polygon
! first corner
        ea(1)=tBndEdg(kE)%x-tBndEdg(nE)%x
        ea(2)=tBndEdg(kE)%y-tBndEdg(nE)%y
        call Unit2DV(ea)
        eb(1)=tBndEdg(kE)%x-tBndEdg(kE+1)%x
        eb(2)=tBndEdg(kE)%y-tBndEdg(kE+1)%y
        call Unit2DV(eb)
        ew=r2Vec_Ortho(ea)
        a=r2Scl_Prod(eb,ew)
        if(a.gt.0.0d0) tBndEdg(kE)%iOrient=0_2
        ew=ea+eb
        call Unit2DV(ew)
        a=r2Scl_Prod(ea,ew)
        a=a.div.(.srt.(1.0d0-a**2))
        a=a*tBndEdg(kE)%r
        tBndEdg(kE)%xa=tBndEdg(kE)%x-a*ea(1)
        tBndEdg(kE)%ya=tBndEdg(kE)%y-a*ea(2)
        tBndEdg(kE)%xb=tBndEdg(kE)%x-a*eb(1)
        tBndEdg(kE)%yb=tBndEdg(kE)%y-a*eb(2)
        ea(1)=a
        ea(2)=tBndEdg(kE)%r
        a=r2Vec_Length(ea)
        tBndEdg(kE)%xo=tBndEdg(kE)%x-a*ew(1)
        tBndEdg(kE)%yo=tBndEdg(kE)%y-a*ew(2)
! last corner
        ea(1)=tBndEdg(nE)%x-tBndEdg(nE-1)%x
        ea(2)=tBndEdg(nE)%y-tBndEdg(nE-1)%y
        call Unit2DV(ea)
        eb(1)=tBndEdg(nE)%x-tBndEdg(kE)%x
        eb(2)=tBndEdg(nE)%y-tBndEdg(kE)%y
        call Unit2DV(eb)
        ew=r2Vec_Ortho(ea)
        a=r2Scl_Prod(eb,ew)
        if(a.gt.0.0d0) tBndEdg(nE)%iOrient=0_2
        ew=ea+eb
        call Unit2DV(ew)
        a=r2Scl_Prod(ea,ew)
        a=a.div.(.srt.(1.0d0-a**2))
        a=a*tBndEdg(nE)%r
        tBndEdg(nE)%xa=tBndEdg(nE)%x-a*ea(1)
        tBndEdg(nE)%ya=tBndEdg(nE)%y-a*ea(2)
        tBndEdg(nE)%xb=tBndEdg(nE)%x-a*eb(1)
        tBndEdg(nE)%yb=tBndEdg(nE)%y-a*eb(2)
        ea(1)=a
        ea(2)=tBndEdg(nE)%r
        a=r2Vec_Length(ea)
        tBndEdg(nE)%xo=tBndEdg(nE)%x-a*ew(1)
        tBndEdg(nE)%yo=tBndEdg(nE)%y-a*ew(2)
      end if
    end do
    iBound=0_2 ! reset splines
    call GetSpline()
    call genMatPts(ldum)
    iBound=iBound+1_2
    if(.not.ldum) iBound=-iBound
  end Subroutine cBndGetABO

  recursive Subroutine GetCpoly()
! get lengths of all c-polygons
    Implicit none
    Real(8) ds,s
    Integer(4) kP,kE,nE,i
    BndLtot=0.0d0
    tBnd(1)%Start=0.0d0
    do kP=1,nBnd
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.lt.1) then ! blank
        s=0.0d0
      else if(tBnd(kP)%nEdge.eq.1) then  ! circle
        s=2.0d0*Pi*tBndEdg(kE)%r
      else if(tBnd(kP)%nEdge.eq.2) then  ! line
        call DistPtPt(tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE+1)%x,tBndEdg(kE+1)%y,s)
      else
        s=0.0d0
        do i=kE+1,nE  ! loop over corners of the polygon
          call DistPtPt(tBndEdg(i-1)%xb,tBndEdg(i-1)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya,ds)
          s=s+ds
          if(tBndEdg(i)%r.gt.(1.0d-8*ds)) then
            if(tBndEdg(i)%iOrient.eq.1_2) then
              ds=ArcLength(tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xa,tBndEdg(i)%ya,tBndEdg(i)%xb,tBndEdg(i)%yb)
              s=s+ds
            else
              ds=ArcLength(tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xb,tBndEdg(i)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya)
              s=s+ds
            end if
          end if
        end do
        if(iiabs(tBnd(kP)%iTypB).eq.1_2) then ! closed polygon
          call DistPtPt(tBndEdg(nE)%xb,tBndEdg(nE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya,ds)
          s=s+ds
          if(tBndEdg(kE)%r.gt.(1.0d-8*ds)) then
            if(tBndEdg(kE)%iOrient.eq.1_2) then
              ds=ArcLength(tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xa,tBndEdg(kE)%ya,tBndEdg(kE)%xb,tBndEdg(kE)%yb)
              s=s+ds
            else
              ds=ArcLength(tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xb,tBndEdg(kE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya)
              s=s+ds
            end if
          end if
        end if
      end if
      tBnd(kP)%Length=s
      tBnd(kP)%sLength=s
      BndLtot=BndLtot+s
      if(kP.lt.nBnd) tBnd(kP+1)%Start=tBnd(kP)%Start+s
    end do
  end Subroutine GetCpoly

  recursive Subroutine GetSpline()
! get lengths of c-polygons and construct splines for all boundaries
    Implicit none
    Real(8) xPt,yPt,vt(2),s,ds,s0,xO,yO,angle
    Integer(4) i,n,m,iErr,iClose,k,idum,lf,l1,l2
    Integer(2) iDomL,iDomR
    Logical(4) ldum
    Character(1151) s1,s2
    if(iBound.lt.3_2) then
      call GetCpoly()
      iBound=1_2
      n=0 ! count points
      do i=1,nBnd
        if(tBnd(i)%nSpline.eq.1) then
          if(tBnd(i)%nEdge.gt.1) then
            n=n+tBnd(i)%nEdge
            if(iiabs(tBnd(i)%iTypB).eq.1_2) n=n+1
          end if
        else if(tBnd(i)%nSpline.gt.1) then
          n=n+tBnd(i)%nSpline
          if(iiabs(tBnd(i)%iTypB).eq.1_2) n=n+1
        end if
      end do
      if(n.lt.2) return
      mBndSpl=n ! allocate memory
      call AllocateBndSpl(ldum)
      if(.not.ldum) then
        iBound=-9_2
        return
      end if
      tBnd(1)%iSplineOffset=0 ! compute spline points
      s0=0.0d0
      do i=1,nBnd
        m=0
        iClose=0
        if(iiabs(tBnd(i)%iTypB).eq.1_2) iClose=1
        if(tBnd(i)%Formula(1:1).eq.'/') then ! parametric boundary description
          ds=0.999999999999d0*tBnd(i)%Length/Dble(tBnd(i)%nSpline+iClose-1)
          s=pSmall
          xO=tBndEdg(tBnd(i)%iEdgeOffset+1)%x
          yO=tBndEdg(tBnd(i)%iEdgeOffset+1)%y
          angle=0.0d0
          if(tBnd(i)%nEdge.gt.1) then
            angle=atan2(tBndEdg(tBnd(i)%iEdgeOffset+2)%y-yO,tBndEdg(tBnd(i)%iEdgeOffset+2)%x-xO)
          end if
          call GetFormulaParts(tBnd(i)%Formula,s1,s2,l1,l2)
          do k=1,tBnd(i)%nSpline
            cForm(0)=0.0d0
            cForm(1)=Pi
            vForm(0)=s/tBnd(i)%Length
            vForm(1)=vForm(0)
            lf=-1
            dFormu=Formula(s1,lf,cForm,pForm,vForm,1,0,1,1,1,1,iErr)
            if((iErr.ne.0).or.(s1(1:1).eq.'!')) dFormu=0.0d0
            xPt=dFormu(1)*tBnd(i)%fAmpl
            lf=-1
            dFormu=Formula(s2,lf,cForm,pForm,vForm,1,0,1,1,1,1,iErr)
            if((iErr.ne.0).or.(s2(1:1).eq.'!')) dFormu=0.0d0
            yPt=dFormu(1)*tBnd(i)%fAmpl
            xSpline(tBnd(i)%iSplineOffset+k)=xO+xPt*cos(angle)-yPt*sin(angle)
            ySpline(tBnd(i)%iSplineOffset+k)=yO+yPt*cos(angle)+xPt*sin(angle)
            s=s+ds
          end do
          if(iClose.gt.0) then
            k=tBnd(i)%nSpline+iClose
            xSpline(tBnd(i)%iSplineOffset+k)=xSpline(tBnd(i)%iSplineOffset+1)
            ySpline(tBnd(i)%iSplineOffset+k)=ySpline(tBnd(i)%iSplineOffset+1)
          end if
          m=tBnd(i)%nSpline
        else if((tBnd(i)%nSpline.eq.1).and.(tBnd(i)%nEdge.gt.1)) then ! nEdge c-poly corners -> splines
          do k=1,tBnd(i)%nEdge
            xSpline(tBnd(i)%iSplineOffset+k)=tBndEdg(tBnd(i)%iEdgeOffset+k)%x
            ySpline(tBnd(i)%iSplineOffset+k)=tBndEdg(tBnd(i)%iEdgeOffset+k)%y
          end do
          if(iClose.gt.0) then
            k=tBnd(i)%nEdge+iClose
            xSpline(tBnd(i)%iSplineOffset+k)=tBndEdg(tBnd(i)%iEdgeOffset+1)%x
            ySpline(tBnd(i)%iSplineOffset+k)=tBndEdg(tBnd(i)%iEdgeOffset+1)%y
          end if
          m=tBnd(i)%nEdge
        else if(tBnd(i)%nSpline.gt.1) then ! nSpline points on c-poly -> splines
          ds=0.999999999999d0*tBnd(i)%Length/Dble(tBnd(i)%nSpline+iClose-1)
          s=pSmall
          do k=1,tBnd(i)%nSpline
            call GetBndPt(i,s+tBnd(i)%Start,xPt,yPt,vt(1),vt(2),iDomL,iDomR,idum)
            if(abs(iDomL).gt.30000_2) Cycle
            if(dabs(tBnd(i)%fAmpl).gt.pSmall) then
              cForm(0)=0.0d0
              cForm(1)=Pi
              vForm(0)=s/tBnd(i)%Length
              vForm(1)=vForm(0)
              lf=-1
              dFormu=Formula(tBnd(i)%Formula,lf,cForm,pForm,vForm,1,0,1,1,1,1,iErr)
              if((iErr.ne.0).or.(tBnd(i)%Formula(1:1).eq.'!')) dFormu=0.0d0
              call unit2DV(vt)
              xPt=xPt-dFormu(1)*vt(2)*tBnd(i)%fAmpl
              yPt=yPt+dFormu(1)*vt(1)*tBnd(i)%fAmpl
            end if
            xSpline(tBnd(i)%iSplineOffset+k)=xPt
            ySpline(tBnd(i)%iSplineOffset+k)=yPt
            s=s+ds
          end do
          if(iClose.gt.0) then
            k=tBnd(i)%nSpline+iClose
            xSpline(tBnd(i)%iSplineOffset+k)=xSpline(tBnd(i)%iSplineOffset+1)
            ySpline(tBnd(i)%iSplineOffset+k)=ySpline(tBnd(i)%iSplineOffset+1)
          end if
          m=tBnd(i)%nSpline
        end if
        if(m.gt.1) then ! compute spline parameters
          sSpline(tBnd(i)%iSplineOffset+1)=s0
          if(iClose.gt.0) then
            call spline2(xSpline(tBnd(i)%iSplineOffset+1),ySpline(tBnd(i)%iSplineOffset+1),0,m, &
            &   sSpline(tBnd(i)%iSplineOffset+1),xsSpline(tBnd(i)%iSplineOffset+1),ysSpline(tBnd(i)%iSplineOffset+1),iErr)
          else
            call spline2(xSpline(tBnd(i)%iSplineOffset+1),ySpline(tBnd(i)%iSplineOffset+1),1,m, &
            &   sSpline(tBnd(i)%iSplineOffset+1),xsSpline(tBnd(i)%iSplineOffset+1),ysSpline(tBnd(i)%iSplineOffset+1),iErr)
          end if
          if(iErr.ne.0) then
            iBound=-10_2
            return
          end if
          tBnd(i)%sLength=sSpline(tBnd(i)%iSplineOffset+m+iClose)-s0
          s0=sSpline(tBnd(i)%iSplineOffset+m+iClose)
          if(i.lt.nBnd) tBnd(i+1)%iSplineOffset=tBnd(i)%iSplineOffset+m+iClose
        else ! no splines
          tBnd(i)%sLength=tBnd(i)%Length
          s0=s0+tBnd(i)%sLength
          if(i.lt.nBnd) tBnd(i+1)%iSplineOffset=tBnd(i)%iSplineOffset
        end if
      end do
      iBound=3_2
    end if
    tBnd(1)%Start=0.0d0
    BndLtot=tBnd(1)%sLength
    do i=2,nBnd
      tBnd(i)%Start=tBnd(i-1)%Start+tBnd(i-1)%sLength
      BndLtot=BndLtot+tBnd(i)%sLength
    end do
  end Subroutine GetSpline

  Subroutine DistPtDom(iDm,x,y,dmin,xNmin,yNmin,smin,kBmin,lInside)
! find the boundary point xNmin,yNmin of Domain iDm with the shortest distance dmin from the given
! point x,y. Check boundary kB (all boundaries for kB=0,...)
! ignore periodic boundaries if lNoPer is true.
! return the boundary side, domain number, and boundary values in addition to dmin,xNmin,yNmin
    Implicit none
    Real(8) x,y,dmin,xNmin,yNmin,val(2),smin,dmink,xNmink,yNmink,smink
    Integer(4) kBmin
    Integer(2) kB,iDm,iDmk,kBmink,iDm2
    Logical lInside,lRSidemin
    dmin=pBig
    do kB=1,nBnd
      if(iDm.gt.0) then
        if((tBnd(kB)%iLDom.ne.iDm).and.(tBnd(kB)%iRDom.ne.iDm)) Cycle
      else if(iDm.lt.0) then
        if((tBnd(kB)%iLDom.le.-iDm).and.(tBnd(kB)%iRDom.le.-iDm)) Cycle
      end if
      call DistPtBnd(kB,kB,x,y,.true.,.true.,dmink,xNmink,yNmink,lRSidemin,iDmk,val,iDm2,smink,kBmink)
      if(dmink.lt.dmin) then
        dmin=dmink
        xNmin=xNmink
        yNmin=yNmink
        dmin=dmink
        smin=smink
        kBmin=kBmink
        lInside=.false.
        if(iDmk.eq.iDm) lInside=.true.
      end if
    end do
    if(dmin.lt.pBig) return
    xNmin=0.0d0
    yNmin=0.0d0
    smin=0.0d0
    kBmin=1
    lInside=.false.
  end Subroutine DistPtDom

  Subroutine DistPtBnd(kB,lB,x,y,lOrig,lNoPer0,dmin,xNmin,yNmin,lRSidemin,iDm,val,iDm2,smin,kBmin,iPerBndOnly)
! find the boundary point xNmin,yNmin with the shortest distance dmin from the given
! point x,y. Check boundary kB (all boundaries for kB=0,...)
! ignore periodic boundaries if lNoPer is true.
! return the boundary side, domain number, and boundary values in addition to dmin,xNmin,yNmin
    Implicit none
    Real(8) x,y,xP,yP,r,d,xN,yN,dmin,xNmin,yNmin,val(2),fv,x1,y1,xs,ys,s,v(2),vn(2),d0,d1,s0,sP,sN,rP0(3),dl,dm,ds
    Real(8), optional:: smin
    Integer(2), optional:: kBmin,iPerBndOnly,iDm2
    Integer(4) k1,k2,kP,kE,nE,i,i0,nP,ms,iClose,m
    Integer(2) kB,lB,iDm,iDm1
    Logical, intent(in) :: lOrig,lNoPer0
    Logical lRSide,lRSidemin,lNoPer,lZero,lxPerBndOnly,lyPerBndOnly,lP
    lNoPer=lNoPer0
    lP=.false.
    if(Present(kBmin)) lP=.true.
    lxPerBndOnly=.false.
    lyPerBndOnly=.false.
    if(Present(iPerBndOnly)) then
      if(iabs(iPerBndOnly).eq.1) then
        lxPerBndOnly=.true.
      else if(iabs(iPerBndOnly).eq.2) then
        lyPerBndOnly=.true.
      end if
    end if
    if(lxPerBndOnly.or.lyPerBndOnly) lNoPer=.false.
    s0=0.0d0
    if((kB.ge.1).and.(kB.le.nBnd)) then
      if((lB.ge.1).and.(lB.le.nBnd)) then
        k1=min(kB,lB)
        k2=max(kB,lB)
      else
        k1=kB
        k2=k1
      end if
    else if((kB.lt.0).and.(-kB.lt.nBnd)) then
      k1=1
      k2=-kB
    else
      k1=1
      k2=nBnd
    end if
    xP=x
    yP=y
    fv=1.0d0
    if(iyzSymm.ne.0) xP=dabs(xP)
    if(ixzSymm.ne.0) yP=dabs(yP)
    if((iyzSymm.eq.2).and.(dabs(x-xP).gt.pSmall)) fv=-fv 
    if((ixzSymm.eq.2).and.(dabs(y-yP).gt.pSmall)) fv=-fv
    if((lOrig).and.(lxPeriod.or.lyPeriod).and.(.not.(lxPerBndOnly.or.lyPerBndOnly))) then
      rP0(1)=xP
      rP0(2)=yP
      rP0(3)=0.0d0
      call MoveToOrigCell(rP0)
      xP=rP0(1)
      yP=rP0(2)
    end if
    dmin=pBig
    xNmin=0.0d0
    yNmin=0.0d0
    lRSidemin=.true.
    iDm=-9_2
    iDm1=-9_2
    val(1)=0.0d0
    val(2)=0.0d0
    if(lNoPer) then ! use periodic boundaries when no other boundaries can be used
      nP=0
      do kP=k1,k2
        if((tBnd(kP)%iLDom.lt.-4).or.(tBnd(kP)%iRDom.lt.-4)) Cycle
        if((tBnd(kP)%iLDom.lt.0).and.(tBnd(kP)%iRDom.lt.0)) Cycle
        if(LSkipBnd(kP)) Cycle
        if(tBnd(kP)%iCond.lt.-100_2) Cycle
        nP=nP+1
      end do
      if(nP.lt.1) lNoPer=.false.
    end if
    do kP=k1,k2
      if((tBnd(kP)%iLDom.lt.-4).or.(tBnd(kP)%iRDom.lt.-4)) Cycle
      if((tBnd(kP)%iLDom.lt.0).and.(tBnd(kP)%iRDom.lt.0)) Cycle
      if(LSkipBnd(kP)) Cycle
      if((tBnd(kP)%iCond.lt.-100_2).and.lNoPer) Cycle
      if((tBnd(kP)%iCond.ne.-101_2).and.lxPerBndOnly) Cycle
      if((tBnd(kP)%iCond.ne.-102_2).and.lyPerBndOnly) Cycle
      if((iBound.gt.2_2).and.((tBnd(kP)%nSpline.gt.1).or.((tBnd(kP)%nSpline.gt.0).and.(tBnd(kP)%nEdge.gt.1)))) then ! spline
        m=tBnd(kP)%nSpline
        if(m.lt.2) m=tBnd(kP)%nEdge
        iClose=0
        if(iiabs(tBnd(kP)%iTypB).eq.1_2) iClose=1
        m=m+iClose
        d0=pBig
        i0=1
        do i=1,m ! find nearest spline point
          call DistPtPt(xP,yP,xSpline(tBnd(kP)%iSplineOffset+i),ySpline(tBnd(kP)%iSplineOffset+i),d1)
          if(d1.lt.d0) then
            i0=i
            d0=d1
          end if
        end do
        if((iClose.eq.1).and.((i0.eq.1).or.(i0.eq.m))) then ! either point 1 or m might be better
          ds=0.00001d0*(sSpline(tBnd(kP)%iSplineOffset+2)-sSpline(tBnd(kP)%iSplineOffset+1))
          s=sSpline(tBnd(kP)%iSplineOffset+1)+ds
          call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
          &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
          &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,s,x1,y1,xs,ys)
          call DistPtPt(xP,yP,x1,y1,d1)
          s=sSpline(tBnd(kP)%iSplineOffset+m)-ds
          call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
          &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
          &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,s,x1,y1,xs,ys)
          call DistPtPt(xP,yP,x1,y1,dm)
          if(dm.lt.d1) then
            i0=m
            d0=dm
          else
            i0=1
            d0=d1
          end if
        end if
        if(d0.lt.dmin) then ! nearest spline point found, refine search
          if(i0.lt.2) then
            ds=0.999999d0*(sSpline(tBnd(kP)%iSplineOffset+2)-sSpline(tBnd(kP)%iSplineOffset+1))
            s=sSpline(tBnd(kP)%iSplineOffset+1)
          else if(i0.ge.m) then
            ds=0.999999d0*(sSpline(tBnd(kP)%iSplineOffset+m)-sSpline(tBnd(kP)%iSplineOffset+m-1))
            s=sSpline(tBnd(kP)%iSplineOffset+m-1)
          else
            ds=0.999999d0*(sSpline(tBnd(kP)%iSplineOffset+i0+1)-sSpline(tBnd(kP)%iSplineOffset+i0-1))
            s=sSpline(tBnd(kP)%iSplineOffset+i0-1)
          end if
          ms=nint(min(1.0d4,(10.0d0*ds)/(d0+1.0d-150)),4)+2
          ds=ds/dble(ms-1)
          i0=max(min(m-1,i0),2)
          d0=pBig
          s0=s
          i0=1
          do i=1,ms
            if(iClose.gt.0) then
              call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
              &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
              &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,s,x1,y1,xs,ys)
            else
              call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
              &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
              &   ysSpline(tBnd(kP)%iSplineOffset+1),1,m,s,x1,y1,xs,ys)
            end if
            call DistPtPt(xP,yP,x1,y1,d1)
            if(d1.lt.d0) then
              i0=i
              d0=d1
              s0=s
            end if
            s=s+ds
          end do
          if(d0.lt.0.9d300) then ! distance from interpolation line -> domain data
            if(iClose.gt.0) then
              call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
              &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
              &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,s0,x1,y1,xs,ys)
            else
              call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
              &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
              &   ysSpline(tBnd(kP)%iSplineOffset+1),1,m,s0,x1,y1,xs,ys)
            end if
            lRSide=.true.
            vn(1)=-ys
            vn(2)=xs
            v(1)=xP-x1
            v(2)=yP-y1
            sP=r2Scl_Prod(v,vn)
            if(sP.gt.0.0d0) lRSide=.false.
            dmin=d0
            xNmin=x1
            yNmin=y1
            lRSidemin=lRSide
            val=fv*tBnd(kP)%val
            if(lRSide) then
              iDm=tBnd(kP)%iRDom
              iDm1=tBnd(kP)%iLDom
            else
              iDm=tBnd(kP)%iLDom
              iDm1=tBnd(kP)%iRDom
            end if
            if(lP) smin=s0
            if(lP) kBmin=kP
          end if
        end if
        Cycle
      end if
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.lt.1) Cycle ! blank
      if(tBnd(kP)%nEdge.eq.1) then  ! circle
        call DistPtCircle(xP,yP,tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE)%r,d,xN,yN,sN,lRSide)
        if(d.lt.dmin) then
          dmin=d
          xNmin=xN
          yNmin=yN
          lRSidemin=lRSide
          val=fv*tBnd(kP)%val
          if(lRSide) then
            iDm=tBnd(kP)%iRDom
            iDm1=tBnd(kP)%iLDom
          else
            iDm=tBnd(kP)%iLDom
            iDm1=tBnd(kP)%iRDom
          end if
          s0=tBnd(kP)%Start+sN
          if(lP) smin=s0
          if(lP) kBmin=kP
        end if
        Cycle
      end if
      if(tBnd(kP)%nEdge.eq.2) then  ! line
        r=0.0d0
        call DistPtLine(xP,yP,tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE+1)%x,tBndEdg(kE+1)%y,r,d,xN,yN,sN,lRSide,lZero)
        if((.not.lZero).and.(d.lt.dmin)) then
          dmin=d
          xNmin=xN
          yNmin=yN
          lRSidemin=lRSide
          val=fv*tBnd(kP)%val
          if(lRSide) then
            iDm=tBnd(kP)%iRDom
            iDm1=tBnd(kP)%iLDom
          else
            iDm=tBnd(kP)%iLDom
            iDm1=tBnd(kP)%iRDom
          end if
          s0=tBnd(kP)%Start+sN
          if(lP) smin=s0
          if(lP) kBmin=kP
        end if
        Cycle
      end if
      s0=tBnd(kP)%Start
      do i=kE+1,nE  ! loop over corners of the polygon
        r=max(tBndEdg(i-1)%r,tBndEdg(i)%r)
        call DistPtLine(xP,yP,tBndEdg(i-1)%xb,tBndEdg(i-1)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya,r,d,xN,yN,sN,lRSide,lZero,dl)
        if((.not.lZero).and.(d.lt.dmin)) then
          dmin=d
          xNmin=xN
          yNmin=yN
          lRSidemin=lRSide
          val=fv*tBnd(kP)%val
          if(lRSide) then
            iDm=tBnd(kP)%iRDom
            iDm1=tBnd(kP)%iLDom
          else
            iDm=tBnd(kP)%iLDom
            iDm1=tBnd(kP)%iRDom
          end if
          if(lP) smin=s0+sN
          if(lP) kBmin=kP
        end if
        s0=s0+dl
        if(tBndEdg(i)%r.gt.pSmall) then
          call DistPtArc(xP,yP,tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xa,tBndEdg(i)%ya, &
          &              tBndEdg(i)%xb,tBndEdg(i)%yb,tBndEdg(i)%iOrient,d,xN,yN,sN,lRSide,dl)
          if(d.lt.dmin) then
            dmin=d
            xNmin=xN
            yNmin=yN
            lRSidemin=lRSide
            val=fv*tBnd(kP)%val
            if(lRSide) then
              iDm=tBnd(kP)%iRDom
              iDm1=tBnd(kP)%iLDom
            else
              iDm=tBnd(kP)%iLDom
              iDm1=tBnd(kP)%iRDom
            end if
            if(lP) smin=s0+sN
            if(lP) kBmin=kP
          end if
          s0=s0+dl
        end if
      end do
      if(iiabs(tBnd(kP)%iTypB).eq.1_2) then ! closed polygon
        r=max(tBndEdg(nE)%r,tBndEdg(kE)%r)
        call DistPtLine(xP,yP,tBndEdg(nE)%xb,tBndEdg(nE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya,r,d,xN,yN,sN,lRSide,lZero,dl)
        if((.not.lZero).and.(d.lt.dmin)) then
          dmin=d
          xNmin=xN
          yNmin=yN
          lRSidemin=lRSide
          val=fv*tBnd(kP)%val
          if(lRSide) then
            iDm=tBnd(kP)%iRDom
            iDm1=tBnd(kP)%iLDom
          else
            iDm=tBnd(kP)%iLDom
            iDm1=tBnd(kP)%iRDom
          end if
          if(lP) smin=s0+sN
          if(lP) kBmin=kP
        end if
        s0=s0+dl
        if(tBndEdg(kE)%r.gt.pSmall) then
          call DistPtArc(xP,yP,tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xa,tBndEdg(kE)%ya, &
          &              tBndEdg(kE)%xb,tBndEdg(kE)%yb,tBndEdg(kE)%iOrient,d,xN,yN,sN,lRSide,dl)
          if(d.lt.dmin) then
            dmin=d
            xNmin=xN
            yNmin=yN
            lRSidemin=lRSide
            val=fv*tBnd(kP)%val
            if(lRSide) then
              iDm=tBnd(kP)%iRDom
              iDm1=tBnd(kP)%iLDom
            else
              iDm=tBnd(kP)%iLDom
              iDm1=tBnd(kP)%iRDom
            end if
            if(lP) smin=s0+sN
            if(lP) kBmin=kP
          end if
          s0=s0+dl
        end if
      end if
    end do
    if(val(1).gt.pBig) val(1)=0.0d0
    if(val(2).gt.pBig) val(2)=0.0d0
    if(Present(iDm2)) iDm2=iDm1
  end Subroutine DistPtBnd

  Subroutine MoveToOrigCell(r)
! Given a point r in 3D space. Move the point into the original periodic cell.
    Implicit none
    Real(8) r(3),rO(3),f,x,y,dmin,xNmin,yNmin,val(2),smin
    Integer(2) iDm,kBmin,iDm2
    Logical lRSidemin
    nxPeriod=0
    nyPeriod=0
    nzPeriod=0
    if(lxPeriod.or.lyPeriod.or.lzPeriod) then
! get period numbers
      rO=r
      if(lzPeriod) then
        f=rO(3)/zPeriodVector(3)
        nzPeriod=idInt(Dabs(f))
        if(rO(3).lt.0.0d0) nzPeriod=-nzPeriod-1
        rO(1:3)=rO(1:3)-f*zPeriodVector(1:3)
      end if
      if(lyPeriod) then
        f=rO(2)/yPeriodVector(2)
        nyPeriod=idInt(Dabs(f))
        if(rO(2).lt.0.0d0) nyPeriod=-nyPeriod-1
        rO(1:3)=rO(1:3)-f*yPeriodVector(1:3)
      end if
      if(lxPeriod) then
        f=rO(1)/xPeriod
        nxPeriod=idInt(Dabs(rO(1)/xPeriod))
        if(rO(1).lt.0.0d0) nxPeriod=-nxPeriod-1
      end if
! move vector
      if(lzPeriod) then
        r(1:3)=r(1:3)-Dble(nzPeriod)*zPeriodVector(1:3)
      end if
      if(lyPeriod) then
        r(1:3)=r(1:3)-Dble(nyPeriod)*yPeriodVector(1:3)
      end if
      if(lxPeriod) then
        r(1)=r(1)-Dble(nxPeriod)*xPeriod
      end if
    end if
! test curved periodic boundaries -> 2D ONLY!!!
    if(lgcFld.and.(.not.lregularPeriod)) then
      if(lxPeriod) then
        lRSidemin=.true.
        do while(lRSidemin)
          x=r(1)
          y=r(2)
          call DistPtBnd(0_2,0_2,x,y,.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDm,val,iDm2,smin,kBmin,1_2)
          if(lRSidemin) then
            nxPeriod=nxPeriod+1
            r(1)=r(1)-xPeriod
          end if
        end do
        do while(.not.lRSidemin)
          x=r(1)+xPeriod
          y=r(2)
          call DistPtBnd(0_2,0_2,x,y,.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDm,val,iDm2,smin,kBmin,1_2)
          if(.not.lRSidemin) then
            nxPeriod=nxPeriod-1
            r(1)=r(1)+xPeriod
          end if
        end do
      end if
      if(lyPeriod) then
        lRSidemin=.true.
        do while(lRSidemin)
          x=r(1)
          y=r(2)
          call DistPtBnd(0_2,0_2,x,y,.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDm,val,iDm2,smin,kBmin,2_2)
          if(lRSidemin) then
            nyPeriod=nyPeriod+1
            r(1:3)=r(1:3)-yPeriodVector(1:3)
          end if
        end do
        do while(.not.lRSidemin)
          x=r(1)+yPeriodVector(1)
          y=r(2)+yPeriodVector(2)
          call DistPtBnd(0_2,0_2,x,y,.true.,.true.,dmin,xNmin,yNmin,lRSidemin,iDm,val,iDm2,smin,kBmin,2_2)
          if(.not.lRSidemin) then
            nyPeriod=nyPeriod-1
            r(1:3)=r(1:3)+yPeriodVector(1:3)
          end if
        end do
      end if
    end if
  end Subroutine MoveToOrigCell

  recursive Subroutine GetBndPt(kB,sPt0,xPt,yPt,xtPt,ytPt,iDomL,iDomR,kPBnd,rPt)
! get position and data of the boundary point on the boundary at sPt
    Implicit none
    Real(8) sPt0,sPt,xPt,yPt,xtPt,ytPt,sact,sloc,slen,ss,sl,xttPt,yttPt,xtttPt,ytttPt !!! ,slen0
    Real(8), Optional :: rPt
    Integer(4) kB,k1,k2,kP,kE,nE,i,ns,kPBnd,m,iClose
    Integer(2) iDomL,iDomR
    if(kB.gt.0) then
      ss=tBnd(kB)%Start
      sl=tBnd(kB)%sLength
      sPt=sPt0-ss
      if((sPt.gt.sl).and.(iiabs(tBnd(kB)%iTypB).eq.1_2)) then ! closed boundary
        if(((sPt-sl)/sl).lt.1.0d-7) then ! assume rounding error
          sPt=sl
        else ! closed boundary 
          sPt=mod(sPt,sl)
        end if
      end if
      sPt=sPt+ss
    else
      sPt=sPt0
    end if
! check if sPt is in the interval 0...smax
    xPt=0.0d0
    yPt=0.0d0
    xtPt=0.0d0
    ytPt=1.0d0
    iDomL=-30001_2
    iDomR=-30001_2
    kPBnd=1
    if(sPt.lt.nSmall) return
    if((kB.gt.0).and.(sPt.lt.(tBnd(kB)%Start-pSmall))) return
    iDomL=30001_2
    iDomR=30001_2
    if(kB.eq.0) then
      kPBnd=nBnd
      kP=nBnd
      k1=1
      k2=nBnd
    else
      kPBnd=kB
      kP=kB
      k1=kB
      k2=kB
    end if
! loop over all boundaries
    do kP=k1,k2
      ss=tBnd(kP)%Start
      sl=tBnd(kP)%sLength
      if(sPt.gt.(ss+sl)) then
        if(kP.lt.nBnd) Cycle
        if(((sPt-ss-sl)/(ss+sl)).lt.1.0d-9) then
          sPt=ss+sl
        else
          Cycle
        end if
      end if
      if(LSkipBnd(kP)) Cycle
      sact=tBnd(kP)%Start
      if((iBound.gt.2_2).and.((tBnd(kP)%nSpline.gt.1).or.((tBnd(kP)%nSpline.gt.0).and.(tBnd(kP)%nEdge.gt.1)))) then ! spline
        m=tBnd(kP)%nSpline
        if(m.lt.2) m=tBnd(kP)%nEdge
        iClose=0
        if(iiabs(tBnd(kP)%iTypB).eq.1_2) iClose=1
        m=m+iClose
        do i=2,m
          sloc=sPt-sact
          slen=sSpline(tBnd(kP)%iSplineOffset+i)-sSpline(tBnd(kP)%iSplineOffset+i-1)
          if((sloc.ge.0.0d0).and.(sloc.le.slen)) then
            if(iClose.gt.0) then
              if(Present(rPt)) then
                call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
                &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
                &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,sPt,xPt,yPt,xtPt,ytPt,xttPt,yttPt,xtttPt,ytttPt)
                rPt=dabs(((dsqrt(xtPt**2+ytPt**2))**3).div.(xtPt*yttPt-ytPt*xttPt))
              else
                call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
                &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
                &   ysSpline(tBnd(kP)%iSplineOffset+1),0,m-1,sPt,xPt,yPt,xtPt,ytPt)
              end if
            else
              if(Present(rPt)) then
                call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
                &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
                &   ysSpline(tBnd(kP)%iSplineOffset+1),1,m,sPt,xPt,yPt,xtPt,ytPt,xttPt,yttPt,xtttPt,ytttPt)
                rPt=dabs(((dsqrt(xtPt**2+ytPt**2))**3).div.(xtPt*yttPt-ytPt*xttPt))
              else
                call splint2(sSpline(tBnd(kP)%iSplineOffset+1),xSpline(tBnd(kP)%iSplineOffset+1), &
                &   ySpline(tBnd(kP)%iSplineOffset+1),xsSpline(tBnd(kP)%iSplineOffset+1), &
                &   ysSpline(tBnd(kP)%iSplineOffset+1),1,m,sPt,xPt,yPt,xtPt,ytPt)
              end if
            end if
            iDomL=tBnd(kP)%iLDom
            iDomR=tBnd(kP)%iRDom
            kPBnd=kP
            return
          end if
          sact=sact+slen
        end do
        Cycle
      end if
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.lt.1) Cycle ! blank
      if(tBnd(kP)%nEdge.eq.1) then  ! circle
        sloc=sPt-sact
        call PtOnCircle(sloc,tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%r,xPt,yPt,xtPt,ytPt,slen,ns)
        if(Present(rPt)) rPt=tBndEdg(kE)%r
        if(ns.eq.0) then
          iDomL=tBnd(kP)%iLDom
          iDomR=tBnd(kP)%iRDom
          kPBnd=kP
          return
        end if
        sact=sact+slen
        Cycle
      end if
      if(tBnd(kP)%nEdge.eq.2) then  ! line
        sloc=sPt-sact
        slen=0.0d0
        call PtOnLine(sloc,tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE+1)%x,tBndEdg(kE+1)%y,xPt,yPt,xtPt,ytPt,slen,ns)
        if(Present(rPt)) rPt=2.0d0*pBig
        if(ns.eq.0) then
          iDomL=tBnd(kP)%iLDom
          iDomR=tBnd(kP)%iRDom
          kPBnd=kP
          return
        end if
        sact=sact+slen
        Cycle
      end if
      do i=kE+1,nE  ! loop over corners of the polygon
        sloc=sPt-sact
     !   call DistPtPt(tBndEdg(i-1)%x,tBndEdg(i-1)%y,tBndEdg(i)%x,tBndEdg(i)%y,slen0)
     !   call DistPtPt(tBndEdg(i-1)%xb,tBndEdg(i-1)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya,slen)
     !   if(slen.gt.slen0*0.0000001d0) then
          call PtOnLine(sloc,tBndEdg(i-1)%xb,tBndEdg(i-1)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya,xPt,yPt,xtPt,ytPt,slen,ns)
          if(Present(rPt)) rPt=2.0d0*pBig
          if(ns.eq.0) then
            iDomL=tBnd(kP)%iLDom
            iDomR=tBnd(kP)%iRDom
            kPBnd=kP
            return
          end if
          sact=sact+slen
     !   end if
        if(tBndEdg(i)%r.gt.pSmall) then
          sloc=sPt-sact
          call PtOnArc(sloc,tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xa,tBndEdg(i)%ya, &
          &              tBndEdg(i)%xb,tBndEdg(i)%yb,tBndEdg(i)%iOrient,xPt,yPt,xtPt,ytPt,slen,ns)
          if(Present(rPt)) rPt=tBndEdg(i)%r
          if(ns.eq.0) then
            iDomL=tBnd(kP)%iLDom
            iDomR=tBnd(kP)%iRDom
            kPBnd=kP
            return
          end if
          sact=sact+slen
        end if
      end do
      if(iiabs(tBnd(kP)%iTypB).eq.1_2) then ! closed polygon
        sloc=sPt-sact
      !  call DistPtPt(tBndEdg(nE)%x,tBndEdg(nE)%y,tBndEdg(kE)%x,tBndEdg(kE)%y,slen0)
      !  call DistPtPt(tBndEdg(nE)%xb,tBndEdg(nE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya,slen)
      !  if(slen.gt.slen0*0.0000001d0) then
          call PtOnLine(sloc,tBndEdg(nE)%xb,tBndEdg(nE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya,xPt,yPt,xtPt,ytPt,slen,ns)
          if(Present(rPt)) rPt=2.0d0*pBig
          if(ns.eq.0) then
            iDomL=tBnd(kP)%iLDom
            iDomR=tBnd(kP)%iRDom
            kPBnd=kP
            return
          end if
          sact=sact+slen
      !  end if
        if(tBndEdg(kE)%r.gt.pSmall) then
          sloc=sPt-sact
          call PtOnArc(sloc,tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xa,tBndEdg(kE)%ya, &
          &              tBndEdg(kE)%xb,tBndEdg(kE)%yb,tBndEdg(kE)%iOrient,xPt,yPt,xtPt,ytPt,slen,ns)
          if(Present(rPt)) rPt=tBndEdg(kE)%r
          if(ns.eq.0) then
            iDomL=tBnd(kP)%iLDom
            iDomR=tBnd(kP)%iRDom
            kPBnd=kP
            return
          end if
          sact=sact+slen
        end if
      end if
    end do
! point not found
    xPt=0.0d0
    yPt=0.0d0
    xtPt=0.0d0
    ytPt=1.0d0
    iDomL=-30001_2
    iDomR=-30001_2
    kPBnd=1
    if(Present(rPt)) rPt=-2.0d0*pBig
  end Subroutine GetBndPt

  recursive Real(8) Function GetBndLength(DMax,PpW,nSeg,minMat)
! return 1) the length of all boundaries
! 2) the minimum number of matching points minMat required
! (>=nSeg pts per line/arc, max. distance between point = min(dstmax,wavelength/PpW) )
! 3) and the locations sBndPt(1...minMat)
    Implicit none
    Integer(4), Intent(In) :: nSeg
    Real(8), Intent(In) :: DMax,PpW
    Integer(4) kP,kE,nE,i,minMat,kMat,m,minMat0,km
    Real(8) s,ds,ss,distMax,wl1,wl2,sl,sloc,dsloc
    Integer(2) lf
	  minMat=0
	  minMat0=0
    kMat=0
    distMax=DMax
    sl=0.0d0
    do kP=1,nBnd
      if(kP.gt.1) then
        tBnd(kP-1)%nMat=minMat-minMat0
        minMat0=minMat
      end if
      if(LSkipBnd(kP)) Cycle
      s=0.0d0 ! local parameter in AuxBndLength
      if(tBnd(kP)%nMatPts.gt.0) then ! fixed number of matching points (add one more -> will be deleted later)
        dsloc=tBnd(kP)%sLength/dble(tBnd(kP)%nMatPts)
        sloc=s-dsloc
        ss=tBnd(kP)%Start
        sl=tBnd(kP)%sLength
        do kM=1,tBnd(kP)%nMatPts+1
          if(kMat.ge.10000) Exit
          kMat=kMat+1
          sloc=sloc+dsloc
          sBndPt(kMat)=min(ss+0.999999999d0*sl,max(ss+1.0d-9*sl,sloc+tBnd(kP)%Start))
        end do
        minMat=kMat
        sl=sl+tBnd(kP)%sLength
        Cycle
      end if
      if(lfcFld.and.(Dble(fcFld).gt.pSmall).and.(PpW.gt.1.0d0)) then
        if((tBnd(kP)%iLDom.gt.0_2).and.(tBnd(kP)%iCond.ne.-1_2)) then
          lf=Min(Int2(nDom),tBnd(kP)%iLDom)
          wl1=Wlength(lf)
        else if((tBnd(kP)%iLDom.gt.-5_2).and.(tBnd(kP)%iLDom.lt.0_2).and.(tBnd(kP)%iCond.ne.-1_2)) then
          wl1=Wlength(1_2)
        else
          wl1=pBig
        end if
        if(tBnd(kP)%iRDom.gt.0_2) then
          lf=Min(Int2(nDom),tBnd(kP)%iRDom)
          wl2=Wlength(lf)
        else if((tBnd(kP)%iRDom.gt.-5_2).and.(tBnd(kP)%iRDom.lt.0_2)) then
          wl2=Wlength(1_2)
        else
          wl2=pBig
        end if
        distMax=dmin1(DMax,wl1/PpW,wl2/PpW)
      end if
      if((iBound.gt.2_2).and.((tBnd(kP)%nSpline.gt.1).or.((tBnd(kP)%nSpline.gt.0).and.(tBnd(kP)%nEdge.gt.1)))) then ! spline
      !  m=tBnd(kP)%nSpline
      !  if(m.lt.2) m=tBnd(kP)%nEdge
      !  if(iiabs(tBnd(kP)%iTypB).eq.1_2) m=m+1
      !  ss=tBnd(kP)%Start
      !  sl=tBnd(kP)%sLength
      !  do i=1,m
      !    kMat=kMat+1
      !    sBndPt(kMat)=min(ss+0.999999999d0*sl,max(ss+1.0d-9*sl,sSpline(tBnd(kP)%iSplineOffset+i)))
      !    s=s+ds
      !  end do
      !  minMat=kMat
        m=nSeg+1 !
        ds=tBnd(kP)%sLength !
		    if(ds.gt.pSmall) call AuxBndLength(kP,m,distmax,s,ds,kMat,minMat) !
        s=s+ds !
        sl=sl+s
        Cycle
      end if
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.lt.1) Cycle ! blank
      if(tBnd(kP)%nEdge.eq.1) then  ! circle
        ds=2.0d0*Pi*tBndEdg(kE)%r
		    if(ds.gt.pSmall) call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
        s=s+ds
        sl=sl+s
        Cycle
      end if
      if(tBnd(kP)%nEdge.eq.2) then  ! line
        call DistPtPt(tBndEdg(kE)%x,tBndEdg(kE)%y,tBndEdg(kE+1)%x,tBndEdg(kE+1)%y,ds)
		    if(ds.gt.pSmall) call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
        s=s+ds
        sl=sl+s
        Cycle
      end if
      do i=kE+1,nE  ! loop over corners of the polygon
        call DistPtPt(tBndEdg(i-1)%xb,tBndEdg(i-1)%yb,tBndEdg(i)%xa,tBndEdg(i)%ya,ds)
		    if((ds.gt.pSmall).and.(ds.gt.dabs(1.0d-8*tBndEdg(i-1)%r)).and.(ds.gt.dabs(1.0d-8*tBndEdg(i)%r))) &
        & call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
        s=s+ds
        if(tBndEdg(i)%r.gt.(1.0d-8*ds)) then
          if(tBndEdg(i)%iOrient.eq.1_2) then
            ds=ArcLength(tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xa,tBndEdg(i)%ya, &
            &            tBndEdg(i)%xb,tBndEdg(i)%yb)
		        if(ds.gt.pSmall) call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
            s=s+ds
          else
            ds=ArcLength(tBndEdg(i)%xo,tBndEdg(i)%yo,tBndEdg(i)%xb,tBndEdg(i)%yb, &
            &            tBndEdg(i)%xa,tBndEdg(i)%ya)
		        if(ds.gt.pSmall) call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
            s=s+ds
          end if
        end if
      end do
      if(iiabs(tBnd(kP)%iTypB).eq.1_2) then ! closed polygon
        call DistPtPt(tBndEdg(nE)%xb,tBndEdg(nE)%yb,tBndEdg(kE)%xa,tBndEdg(kE)%ya,ds)
		    if((ds.gt.pSmall).and.(ds.gt.dabs(1.0d-8*tBndEdg(nE)%r)).and.(ds.gt.dabs(1.0d-8*tBndEdg(kE)%r))) &
        & call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
        s=s+ds
        if(tBndEdg(kE)%r.gt.(1.0d-8*ds)) then
          if(tBndEdg(kE)%iOrient.eq.1_2) then
            ds=ArcLength(tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xa,tBndEdg(kE)%ya, &
            &            tBndEdg(kE)%xb,tBndEdg(kE)%yb)
		        if(ds.gt.pSmall) call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
            s=s+ds
          else
            ds=ArcLength(tBndEdg(kE)%xo,tBndEdg(kE)%yo,tBndEdg(kE)%xb,tBndEdg(kE)%yb, &
            &            tBndEdg(kE)%xa,tBndEdg(kE)%ya)
		        if(ds.gt.pSmall) call AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
            s=s+ds
          end if
        end if
      end if
      sl=sl+s
      if((iiabs(tBnd(kP)%iTypB).eq.1_2).and.(tBnd(kP)%nMatPts.lt.1)) then ! closed boundary: add end point
        kMat=kMat+1
        minMat=minMat+1
        sBndPt(kMat)=tBnd(kP)%Start+0.999999999d0*tBnd(kP)%sLength
      end if
    end do
    tBnd(nBnd)%nMat=minMat-minMat0
    tBnd(1)%iMatOffset=0
    do kP=2,nBnd
      tBnd(kP)%iMatOffset=tBnd(kP-1)%iMatOffset+tBnd(kP-1)%nMat
    end do
    GetBndLength=sl
  end Function GetBndLength

  recursive Subroutine AuxBndLength(kP,nSeg,distmax,s,ds,kMat,minMat)
! auxiliary for GetBndLength, inserts matching points
    Implicit none
    Integer(4) minMat,nMat,kMat,kP,kM,nSeg
    Real(8) distmax,s,ds,sloc,dsloc,ss,sl
    if(LSkipBnd(kP)) return
! get minimum number of points
    if(ds.gt.distMax*1000000) then ! one added -> will be removed later
      nMat=nSeg+1
    else
      nMat=Max(nSeg,idInt(ds/distMax)+1)+1
    end if
    if(nMat.le.0_4) return ! don't set points
! set equidistant points
    ss=tBnd(kP)%Start
    sl=tBnd(kP)%sLength
    if(nMat.gt.1) then
      dsloc=ds/dble(nMat-1)
    else
      dsloc=0.5d0*ds
    end if
    sloc=s
    do kM=1,nMat
      if(kMat.ge.10000) Exit
      kMat=kMat+1
      sBndPt(kMat)=min(ss+0.999999999d0*sl,max(ss+1.0d-9*sl,sloc+ss))
      sloc=sloc+dsloc
    end do
    minMat=kMat
  end Subroutine AuxBndLength

  Subroutine checkOver(kP,k1,k2,kMat,nMat,over)
! check overdetermination between matching points from k1 to k2 along boundary kP
! if necessary, insert matching points after kMat and increase kMat and nMat accordingly
! over: overdetermination factor, ss: boundary start velue, sl: boundary length
! if new points generated: sort the points k1 up to kMat
    Implicit None
    Integer(2) iDomL,iDomR
    Integer(4) k1,k2,kP,kMat,nMat,kM,nMl,kE,idum,nM
    Real(8) over,ss,sl,slo,dslo,xtPt,ytPt,ra(2),rb(2),rc(2),a,alpha,beta,gamma,a3,b3,c3
    Real(8), external:: TriAng2D
    if(k1.ge.k2) return
    ss=tBnd(kP)%Start
    sl=tBnd(kP)%sLength
    do kM=k1,k2-1
      nMl=1
      call GetBndPt(kP,sBndPt(kM),rb(1),rb(2),xtPt,ytPt,iDomL,iDomR,idum)
      if(abs(iDomL).gt.30000_2) then
        write(*,*) 'Warning: Unexpected error A in point of boundary ',kP
        Cycle
      end if
      call GetBndPt(kP,sBndPt(kM+1),rc(1),rc(2),xtPt,ytPt,iDomL,iDomR,idum)
      if(abs(iDomL).gt.30000_2) then
        write(*,*) 'Warning: Unexpected error B in point of boundary ',kP
        Cycle
      end if
      a=360.0d0
      ra(1:2)=0.0d0
      do kE=1,nExp
        if((tExp(kE)%iDom.ne.iDomL).and.(tExp(kE)%iDom.ne.iDomR)) Cycle
        if((tExp(kE)%iTypE.ne.1_2).and.(tExp(kE)%iTypE.ne.2_2)) Cycle
        a=AngleMultipole2D(kE)/max(over,0.001d0)
        if(a.lt.180.0d0) then
          if((abs(tExp(kE)%rE(3))+abs(tExp(kE)%rE(4))).gt.pSmall) then ! complex origin multipole
            ra(1)=tExp(kE)%Plane(1,0)+tExp(kE)%rE(4)
            ra(2)=tExp(kE)%Plane(2,0)-tExp(kE)%rE(3)
            alpha=TriAng2D(ra,rb,rc)
            if(alpha.gt.a) nMl=Max(nMl,1_4+idInt(alpha/a))
            ra(1)=tExp(kE)%Plane(1,0)-tExp(kE)%rE(4)
            ra(2)=tExp(kE)%Plane(2,0)+tExp(kE)%rE(3)
            alpha=TriAng2D(ra,rb,rc)
            if(alpha.gt.a) nMl=Max(nMl,1_4+idInt(alpha/a))
          else ! real origin multipole
            ra(1:2)=tExp(kE)%Plane(1:2,0)
            call TriAngle2D(ra,rb,rc,a3,b3,c3,alpha,beta,gamma)
            if(alpha.gt.a) nMl=Max(nMl,1_4+idInt(alpha/a))
          end if
        end if
      end do
      nMl=nMl-1
      if((nMl.gt.0).and.((nMat+nMl).lt.10000)) then ! insert nMl-1 points
        do nM=nMat,kMat+1,-1
          sBndPt(nM+nMl)=sBndPt(nM)
        end do
        nMat=nMat+nMl
        dslo=(sBndPt(kM+1)-sBndPt(kM))/Dble(nMl+1)
        slo=sBndPt(kM)+dslo
        do nM=1,nMl
          kMat=kMat+1
          sBndPt(kMat)=min(ss+0.999999999d0*sl,max(ss+1.0d-9*sl,slo))
          slo=slo+dslo
        end do
      end if
    end do
    if(kMat.le.k2) return
    call QuickSort(sBndPt(1:kMat),k1,kMat)
    do kM=k1,kMat-1
    end do
  end Subroutine checkOver

  Subroutine genMatPts(ldum)
! generate matching points on all boundaries
    Implicit none
    Integer(4) idum,kMat,kP,k1,k2,nAdd
    Integer(2) iColLoc,iConLoc,iDomLoc
    Real(8) slen,ss,sl,d,d1
    Logical ldum
    iColLoc=iColBnd
    iConLoc=iConBnd
    iDomLoc=iDomBnd
    iColBnd=0_2
    iConBnd=iMMPCon
    iDomBnd=0_2
    call getEUST()
    if(iBound.lt.3_2) call GetSpline()
    mBndPt=0
    sBndPt=-1.0d0
    slen=GetBndLength(BndDMax,BndPpW,nSegPt,mBndPt) ! get initial number of matching points mBndPt
    if(mBndPt.gt.1) then
      if(BndOver.gt.pSmall) then ! check overdetermination and insert required points
        do kP=1,nBnd
          if(tBnd(kP)%nMatPts.gt.0) Cycle
          if(tBnd(kP)%nMat.lt.1) Cycle
          ss=tBnd(kP)%Start
          sl=tBnd(kP)%sLength
          k1=tBnd(kP)%iMatOffset+1
          k2=tBnd(kP)%iMatOffset+tBnd(kP)%nMat
          kMat=k2
          nAdd=k2
          call checkOver(kP,k1,k2,kMat,mBndPt,BndOver)
          k2=kMat
          call checkOver(kP,k1,k2,kMat,mBndPt,BndOver) ! check once more
          nAdd=kMat-nAdd
          if(nAdd.gt.0) then
            tBnd(kP)%nMat=tBnd(kP)%nMat+nAdd
            tBnd(kP+1:nBnd)%iMatOffset=tBnd(kP+1:nBnd)%iMatOffset+nAdd
          end if
        end do
      end if
    end if
    do kP=nBnd,1,-1 ! remove points of a boundary with too small distance from neighbor
      if(tBnd(kP)%nMatPts.gt.0) Cycle
      if(tBnd(kP)%nMat.lt.1) Cycle
      k1=tBnd(kP)%iMatOffset+1
      k2=tBnd(kP)%iMatOffset+tBnd(kP)%nMat
      ss=tBnd(kP)%Start
      sl=tBnd(kP)%sLength
      if(k2.gt.k1) then
        do idum=k2-1,k1,-1
          d=abs(sBndPt(idum+1)-sBndPt(idum))
          if(d/max(1.0d-16,smallMatDist).lt.sl) then ! distance too small
            sBndPt(idum:mBndPt-1)=sBndPt(idum+1:mBndPt)
            mBndPt=mBndPt-1
            tBnd(kP)%nMat=tBnd(kP)%nMat-1
            if(kP.lt.nBnd) tBnd(kP+1:nBnd)%iMatOffset=tBnd(kP+1:nBnd)%iMatOffset-1
          end if
        end do
      end if
    end do
    if(lSmoothMat) then
      do kP=1,nBnd ! smoothen distances
        if(tBnd(kP)%nMatPts.gt.0) Cycle
        if(tBnd(kP)%nMat.lt.1) Cycle
        k1=tBnd(kP)%iMatOffset+1
        k2=tBnd(kP)%iMatOffset+tBnd(kP)%nMat
        ss=tBnd(kP)%Start
        sl=tBnd(kP)%sLength
        if(k2-1.gt.k1+1) then
          idum=k1
          d1=abs(sBndPt(idum)-ss)
          d=abs(sBndPt(idum+1)-sBndPt(idum))
          sBndPt(idum)=sBndPt(idum)+0.25d0*(d-d1)
          do idum=k1+1,k2-1
            d1=abs(sBndPt(idum)-sBndPt(idum-1))
            d=abs(sBndPt(idum+1)-sBndPt(idum))
            sBndPt(idum)=sBndPt(idum)+0.25d0*(d-d1)
          end do
          idum=k2
          d1=abs(sBndPt(idum)-sBndPt(idum-1))
          d=abs(ss+sl-sBndPt(idum))
          sBndPt(idum)=sBndPt(idum)+0.25d0*(d-d1)
        end if
      end do
    end if
    call AllocateBndPt(ldum) ! allocate memory for the matching points
    if((.not.ldum).or.(mBndPt.lt.1)) return
    nBndPt=mBndPt
    do kP=nBnd,1,-1 ! get wBndPt and move sBndPt to the center of the interval after sBndPt
      if(tBnd(kP)%nMat.lt.1) Cycle
      k1=tBnd(kP)%iMatOffset+1
      k2=tBnd(kP)%iMatOffset+tBnd(kP)%nMat
      if(k2.gt.k1) then ! weight=distance from following point, except last: remove it
        do idum=k1,k2-1
          wBndPt(idum)=sBndPt(idum+1)-sBndPt(idum)
          sBndPt(idum)=sBndPt(idum)+0.5d0*wBndPt(idum)
        end do
        tBnd(kP)%nMat=tBnd(kP)%nMat-1
        tBnd(kP+1:nBnd)%iMatOffset=tBnd(kP+1:nBnd)%iMatOffset-1
        sBndPt(k2:nBndPt-1)=sBndPt(k2+1:nBndPt)
        wBndPt(k2:nBndPt-1)=wBndPt(k2+1:nBndPt)
        nBndPt=nBndPt-1
      else
        wBndPt(k1)=tBnd(kP)%sLength
      end if
    end do
    nBndEq=0 ! set iBndPt and compute nBndEq (number of equations obtained from all 2D boundaries)
    do kP=1,nBnd
      if(tBnd(kP)%nMat.lt.1) Cycle
      k1=tBnd(kP)%iMatOffset+1
      k2=tBnd(kP)%iMatOffset+tBnd(kP)%nMat
      iBndPt(k1:k2)=Int2(kP)
      if(tBnd(kP)%Weight.le.pSmall) Cycle
      nBndEq=nBndEq+tBnd(kP)%nMat*tBnd(kP)%nBC
    end do
    iColBnd=iColLoc
    iConBnd=iConLoc
    iDomBnd=iDomLoc
    ldum=.true.
  end Subroutine genMatPts

  Real(8) Function AngleMultipole2D(kE)
! get 'Sehwinkel' of the multipole kE
    Implicit none
    Integer(2) iHEk
    Integer(4) kE,nOrd
    nOrd=Max(0_4,tExp(kE)%iE(1),tExp(kE)%iE(2))
    AngleMultipole2D=180.0d0/Dble(2*nOrd+1)
    if(.not.lgcFld) return
    AngleMultipole2D=360.0d0
    if(tExp(kE)%nPar.lt.1) return
    iHEk=igetiHE2(iHEGlobal,kE)
    if(iHEk.lt.0_2) return
    tExp(kE)%iE(5)=Max(1,tExp(kE)%iE(5))
    nOrd=Max(1_4,Int4(tExp(kE)%iE(1)))+Int4(tExp(kE)%iE(5))*(Int4(tExp(kE)%nPar+1_2)/4_4)
    nOrd=min(nOrd,tExp(kE)%iE(2))
    if(iHEk.ne.2_2) nOrd=2*nOrd
    if((ixzSymm.ne.0).and.(dabs(tExp(kE)%Plane(1,0)).lt.pSmall).and.(dabs(tExp(kE)%rE(1)).lt.pSmall)) then
      nOrd=2*nOrd
    end if
    if((iyzSymm.ne.0).and.(dabs(tExp(kE)%Plane(2,0)).lt.pSmall).and.(dabs(tExp(kE)%rE(1)).lt.pSmall)) then
      nOrd=2*nOrd
    end if
    AngleMultipole2D=90.0d0/Dble(nOrd)
  end Function AngleMultipole2D

  Subroutine GetBndHandle(x,y,dmin,kP,kPE,iType)
    Implicit none
    Real(8) x,y,d,dmin
    Integer(4) kP,kPE,kE,nE,i,iType,lout
    kP=1
    kPE=1
    iType=0
    do kP=1,nBnd
      if(LSkipBnd(kP)) Cycle
      kE=1+tBnd(kP)%iEdgeOffset
      nE=tBnd(kP)%nEdge+tBnd(kP)%iEdgeOffset
      if(tBnd(kP)%nEdge.lt.1) Cycle      ! nothing
      if(tBnd(kP)%nEdge.eq.1) then       ! circle (kE always 1)
        call DistPtPt(x,y,tBndEdg(kE)%xa,tBndEdg(kE)%ya,d)
        iType=11
        if(d.lt.dmin) Exit
        call DistPtPt(x,y,tBndEdg(kE)%xo,tBndEdg(kE)%yo,d)
        iType=10
        if(d.lt.dmin) Exit
        call DistPtPt(x,y,tBndEdg(kE)%xo-tBndEdg(kE)%r,tBndEdg(kE)%yo,d)
        iType=19
        if(d.lt.dmin) Exit
        iType=0
        Cycle
      else if(tBnd(kP)%nEdge.eq.2) then  ! line
        call DistPtPt(x,y,tBndEdg(kE)%x,tBndEdg(kE)%y,d)
        iType=21
        if(d.lt.dmin) Exit
        call DistPtPt(x,y,tBndEdg(kE+1)%x,tBndEdg(kE+1)%y,d)
        iType=22
        if(d.lt.dmin) Exit
        call DistPtPt(x,y,0.5d0*(tBndEdg(kE)%x+tBndEdg(kE+1)%x),0.5d0*(tBndEdg(kE)%y+tBndEdg(kE+1)%y),d)
        iType=29
        if(d.lt.dmin) Exit
        iType=0
        Cycle
      end if
      i=kE
      do kE=i,nE                          ! loop over corners of the polygon
        if((iiabs(tBnd(kP)%iTypB).eq.1_2).or.((kE.ne.i).and.(kE.ne.nE))) then
          call DistPtPt(x,y,tBndEdg(kE)%xa,tBndEdg(kE)%ya,d)
          iType=31
          if(d.lt.dmin) Exit
          call DistPtPt(x,y,tBndEdg(kE)%xb,tBndEdg(kE)%yb,d)
          iType=32
          if(d.lt.dmin) Exit
          call DistPtPt(x,y,tBndEdg(kE)%xo,tBndEdg(kE)%yo,d)
          iType=30
          if(d.lt.dmin) Exit
        end if
        call DistPtPt(x,y,tBndEdg(kE)%x,tBndEdg(kE)%y,d)
        iType=33
        if(d.lt.dmin) Exit
        if(kE.lt.nE) then
          call DistPtPt(x,y,0.5d0*(tBndEdg(kE)%x+tBndEdg(kE+1)%x),0.5d0*(tBndEdg(kE)%y+tBndEdg(kE+1)%y),d)
          iType=39
          if(d.lt.dmin) Exit
        else if(iiabs(tBnd(kP)%iTypB).eq.1_2) then
          call DistPtPt(x,y,0.5d0*(tBndEdg(kE)%x+tBndEdg(1)%x),0.5d0*(tBndEdg(kE)%y+tBndEdg(1)%y),d)
          iType=39
          if(d.lt.dmin) Exit
        end if
        iType=0
      end do
      if(iType.ne.0) Exit
    end do
    kP=Min(kP,nBnd)
    if(iType.gt.29) then
		  call OutTxt('t1','polygon'C)
    else if(iType.eq.29) then
		  call OutTxt('t1','line->polygon'C)
    else if(iType.gt.19) then
		  call OutTxt('t1','line'C)
    else if(iType.eq.19) then
		  call OutTxt('t1','circle->line'C)
    else if(iType.gt.9) then
		  call OutTxt('t1','circle'C)
    else
		  call OutTxt('t1','nothing'C)
    end if
    if(iType.ne.0) then
      call IntToStr(Int4(kP),0,0,SpaceText,lout)
		  call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(Int4(nBnd),0,0,SpaceText,lout)
		  call OutTxt('m1',SpaceText(1:lout))
    else
		  call OutTxt('n1',' 'C)
		  call OutTxt('m1',' 'C)
    end if
		call OutTxt('t2','handle type'C)
    call IntToStr(Int4(iType),0,0,SpaceText,lout)
		call OutTxt('n2',SpaceText(1:lout))
    kPE=kE-tBnd(kP)%iEdgeOffset
  end Subroutine GetBndHandle

  Subroutine updateBnd(x,y,kP,kPE,iType)
    Implicit none
    Real(8) x,y,dx,dy,v(2),dmin,dum,a
    Integer(4) kP,kPE,kE,nE,iType
    Logical ldum
    kE=tBnd(kP)%iEdgeOffset+kPE
    nE=kE+1
    if((nE-tBnd(kP)%iEdgeOffset).gt.tBnd(kP)%nEdge) nE=1+tBnd(kP)%iEdgeOffset
    if(iType.eq.10) then ! circle, center
      if(lMoveInsert) then
        dx=x-(tBndEdg(kE)%x-tBndEdg(kE)%r)
        dy=y-tBndEdg(kE)%y
      else
        dx=x-tBndEdg(kE)%x
        dy=y-tBndEdg(kE)%y
      end if
      iDomBnd=0_2
      iColBnd=0_2
      iConBnd=0_2
      call MoveBoundary(kP,dx,dy)
    else if(iType.eq.11) then ! circle, start
      v(1)=x-tBndEdg(kE)%xo
      v(2)=y-tBndEdg(kE)%yo
	    call I2R(2_2,1_2,dmin,dum)
	    call I2R(1_2,1_2,a,dum)
      dmin=dabs(dmin-a)
      a=r2Vec_Length(v)
      if(a.lt.dmin) a=0.0d0
      tBndEdg(kE)%r=a
    else if(iType.eq.19) then ! circle, insert point
      call InsertBndEdg(kP,kE-tBnd(kP)%iEdgeOffset,1,ldum)
      if(.not.ldum) return
      tBndEdg(kE+1)%x=x
      tBndEdg(kE+1)%y=y
      tBndEdg(kE+1)%r=0
      tBndEdg(kE)%r=0
      iType=22 ! new type: end point of a line
    else if(iType.eq.20) then ! line, move
      if(lMoveInsert) then
        dx=x-0.5d0*(tBndEdg(kE)%x+tBndEdg(nE)%x)
        dy=y-0.5d0*(tBndEdg(kE)%y+tBndEdg(nE)%y)
      else
        dx=x-tBndEdg(kE)%x
        dy=y-tBndEdg(kE)%y
      end if
      iDomBnd=0_2
      iColBnd=0_2
      iConBnd=0_2
      call MoveBoundary(kP,dx,dy)
    else if(iType.eq.21) then ! line, start
      tBndEdg(kE)%x=x
      tBndEdg(kE)%y=y
    else if(iType.eq.22) then ! line, end
      tBndEdg(kE+1)%x=x
      tBndEdg(kE+1)%y=y
    else if(iType.eq.29) then ! line, insert point
      call InsertBndEdg(kP,kE-tBnd(kP)%iEdgeOffset,1,ldum)
      if(.not.ldum) return
      tBndEdg(kE+1)%x=x
      tBndEdg(kE+1)%y=y
      tBndEdg(kE+1)%r=0
      kE=kE+1
      tBnd(kP)%iTypB=2_2
      if(tBnd(kBnd)%nSpline.gt.0) tBnd(kP)%iTypB=-2_2
      iType=33 ! new type: point of an open polygon
    else if(iType.eq.30) then ! poly, center
      if(lMoveInsert) then
        dx=x-0.5d0*(tBndEdg(kE)%x+tBndEdg(nE)%x)
        dy=y-0.5d0*(tBndEdg(kE)%y+tBndEdg(nE)%y)
      else
        dx=x-tBndEdg(kE)%xo
        dy=y-tBndEdg(kE)%yo
      end if
      iDomBnd=0_2
      iColBnd=0_2
      iConBnd=0_2
      call MoveBoundary(kP,dx,dy)
    else if(iType.eq.31) then ! poly, start
      v(1)=x-tBndEdg(kE)%x
      v(2)=y-tBndEdg(kE)%y
	    call I2R(2_2,1_2,dmin,dum)
	    call I2R(1_2,1_2,a,dum)
      dmin=dabs(dmin-a)
      a=r2Vec_Length(v)
      if(a.lt.dmin) a=0.0d0
      tBndEdg(kE)%r=a
    else if(iType.eq.32) then ! poly, end
      v(1)=x-tBndEdg(kE)%x
      v(2)=y-tBndEdg(kE)%y
	    call I2R(2_2,1_2,dmin,dum)
	    call I2R(1_2,1_2,a,dum)
      dmin=dabs(dmin-a)
      a=r2Vec_Length(v)
      if(a.lt.dmin) a=0.0d0
      tBndEdg(kE)%r=a
    else if(iType.eq.33) then ! poly, corner
      tBndEdg(kE)%x=x
      tBndEdg(kE)%y=y
    else if(iType.eq.39) then ! poly, insert point
      call InsertBndEdg(kP,kE-tBnd(kP)%iEdgeOffset,1,ldum)
      if(.not.ldum) return
      tBndEdg(kE+1)%x=x
      tBndEdg(kE+1)%y=y
      tBndEdg(kE+1)%r=0
      kE=kE+1
      iType=33 ! new type: point of a polygon
    end if
    kPE=kE-tBnd(kP)%iEdgeOffset
    call cBndGetABO()
  end Subroutine updateBnd

  Subroutine getBndCnd(iC,lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz)
! get the boundary condition details lusual,lspec,lxper,lyper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz
    Implicit none
    Integer(2) iC
    Logical lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz
    lusual=.false.
    lspec=.false.
    lxper=.false.
    lyper=.false.
    lzper=.false.
    lsibc=.false.
    let=.false.
    lez=.false.
    ldn=.false.
    lht=.false.
    lhz=.false.
    lbn=.false.
    laz=.false.
    lvz=.false.
    if(iC.lt.-102) then
      lzper=.true.
    else if(iC.lt.-101) then
      lyper=.true.
    else if(iC.lt.-100) then
      lxper=.true.
    else if(iC.lt.0) then
      lsibc=.true.
    else if(iC.lt.1) then
      lusual=.true.
    else
      lspec=.true.
      if(iand(iC,1_2).eq.1_2) lEt=.true.
      if(iand(iC,2_2).eq.2_2) lEz=.true.
      if(iand(iC,4_2).eq.4_2) lDn=.true.
      if(iand(iC,8_2).eq.8_2) lHt=.true.
      if(iand(iC,16_2).eq.16_2) lHz=.true.
      if(iand(iC,32_2).eq.32_2) lBn=.true.
      if(iand(iC,64_2).eq.64_2) lAz=.true.
      if(iand(iC,128_2).eq.128_2) lVz=.true.
    end if
  end Subroutine getBndCnd

  Subroutine setBndCnd(iC,lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz)
! set the boundary condition number iC, when lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz are known
    Implicit none
    Integer(2) iC
    Logical lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz
    if(lusual) then
      iC=0_2
    else if(lxper) then
      iC=-101_2
    else if(lyper) then
      iC=-102_2
    else if(lzper) then
      iC=-103_2
    else if(lsibc) then
      iC=-1_2
    else if(lspec) then
      iC=0_2
      if(let) iC=iC+1_2
      if(lez) iC=iC+2_2
      if(ldn) iC=iC+4_2
      if(lht) iC=iC+8_2
      if(lhz) iC=iC+16_2
      if(lbn) iC=iC+32_2
      if(laz) iC=iC+64_2
      if(lvz) iC=iC+128_2
    end if
  end Subroutine setBndCnd

  Subroutine GetBndBC(kB)
    Implicit none
    Integer(4) kB,nBC,i
    Integer(2) iDomL,iDomR,iCond
    Logical lBC(10),lusual,lspec,lxper,lyper,lzper,lsibc
    nBC=0
    tBnd(kB)%nBC=nBC
    tBnd(kB)%lBC(1:10)=.false.
    iCond=tBnd(kB)%iCond
    iDomL=tBnd(kB)%iLDom
    iDomR=tBnd(kB)%iRDom
    if((iDomL.gt.30000_2).or.(iDomL.lt.-30000_2)) return
    call getBndCnd(iCond,lusual,lspec,lxper,lyper,lzper,lsibc,lBC(1),lBC(2),lBC(3),lBC(4),lBC(5),lBC(6), &
    &              lBC(8),lBC(10))
    if(lusual) then
      lBC(1:10)=.false.
      if(iDomL.eq.iDomR) return
      if(iDomL.lt.1_2) then
        if(iDomR.lt.1_2) return
        if(iDomL.gt.-2_2) then ! PEC
          lBC(1:2)=.true.
          lBC(6)=.true.
        else if(iDomL.eq.-2_2) then ! PMC
          lBC(4:5)=.true.
          lBC(3)=.true.
        else if(iDomL.eq.-3_2) then ! Az only
          lBC(8)=.true.
        else if(iDomL.eq.-4_2) then ! V only
          lBC(10)=.true.
        else ! impressed field
          lBC(1:6)=.true.
        end if
      else
        if((iDomR.lt.1_2).and.(iDomR.gt.-5_2)) then
          if(iDomR.gt.-2_2) then ! PEC
            lBC(1:2)=.true.
            lBC(6)=.true.
          else if(iDomR.eq.-2_2) then ! PMC
            lBC(4:5)=.true.
            lBC(3)=.true.
          else if(iDomR.eq.-3_2) then ! Az only
            lBC(8)=.true.
          else if(iDomR.eq.-4_2) then ! V only
            lBC(10)=.true.
          else ! impressed field
            lBC(1:6)=.true.
          end if
        else ! standard EM field
          lBC(1:6)=.true.
        end if
      end if
      if(lgcFld) then
        if(lMMPstat) lBC(8)=.true.
        if(lMMPstat) lBC(10)=.true.
        if(iHEGlobal.eq.0) then
          lBC(1)=.false.
          lBC(3)=.false.
          lBC(5)=.false.
        else if(iHEGlobal.eq.1) then
          lBC(2)=.false.
          lBC(4)=.false.
          lBC(6)=.false.
        end if
      end if
    else if(lxper.or.lyper.or.lzper) then
      lBC(1:10)=.false.
      if((iDomL.ne.iDomR).or.(iDomL.lt.1_2).or.(iDomR.lt.1_2)) return
      lBC(1:6)=.true.
    else if(lsibc) then
      lBC(1:10)=.false.
      if((iDomL.eq.iDomR).or.(iDomL.lt.1_2).or.(iDomR.lt.1_2)) return
      lBC(1:2)=.true.
      lBC(4:5)=.true.
    end if
    do i=1,10
      tBnd(kB)%lBC(i)=lBC(i)
      if(lBC(i)) nBC=nBC+1
    end do
    tBnd(kB)%nBC=nBC
  end Subroutine GetBndBC

  Subroutine genBndArc(xO,yO,xS,yS,alpha,iDL,iDR,iCl,iCn,nMP,w1,w2)
! generate a 2D boundary representing an arc
! if alpha<0: line from (xO,yO) to xS,yS
! if distance (xO,yO)-(xS,yS)<1.0d-100: circle around (xO,yO), radius alpha
    Implicit none
    Integer(4) N,i,kP,iDL,iDR,iCl,iCn,nMP
    Real(8) xO,yO,xS,yS,alpha,xA(6),yA(6),w1,w2,r
    Logical ldum
    call DistPtPt(xO,yO,xS,yS,r)
    if(r.lt.pSmall) then ! circle
      N=1
      xA(1)=xO
      yA(1)=yO
      r=alpha
    else
      if(alpha.gt.nSmall) then
        call genArc2(xO,yO,xS,yS,alpha,xA,yA,N)
        if(N.lt.3) return
        r=r*0.999999999d0 ! avoid numerical problems with very small negative segments
      else ! line
        N=2
        xA(1)=xO
        yA(1)=yO
        xA(2)=xS
        yA(2)=yS
        r=0.0d0
      end if
    end if
    call InsertBnd(nBnd,1,ldum)
    if(.not.ldum) return
    call InsertBndEdg(nBnd,0,N,ldum)
    if(.not.ldum) return
    if(N.ne.1) then
      tBnd(nBnd)%iTypB=0_2
    else
      tBnd(nBnd)%iTypB=1_2
    end if
    tBnd(nBnd)%iLDom=Int2(iDL)
    tBnd(nBnd)%iRDom=Int2(iDR)
    tBnd(nBnd)%iCol =Int2(iCl)
    tBnd(nBnd)%iConn=Int2(iCn)
    tBnd(nBnd)%nMatPts=nMP
    tBnd(nBnd)%Weight =w1
    tBnd(nBnd)%Weight2=w2
    tBnd(nBnd)%nSpline=0_2
    kP=tBnd(nBnd)%iEdgeOffset
    do i=1,N
      kP=kP+1
      tBndEdg(kP)%x=xA(i)
      tBndEdg(kP)%y=yA(i)
      tBndEdg(kP)%xa=xA(i)
      tBndEdg(kP)%ya=yA(i)
      tBndEdg(kP)%xb=xA(i)
      tBndEdg(kP)%yb=yA(i)
      tBndEdg(kP)%xo=xA(i)
      tBndEdg(kP)%yo=yA(i)
      tBndEdg(kP)%r=r
    end do
    iDomBnd=0_2
    iColBnd=2_2
    iConBnd=0_2
    call cBndGetABO()
  end Subroutine genBndArc

! Binding GMSH

  subroutine GMSHwriteBndPoints(iFile,iBnd,iPoint,iPoint1,iLine,x1,y1,xn,yn,lClosed)
! Write corner point data of an OpenMaXwell 2D boundary (C-polygon or splines)
! and the resulting line and circle elements for GMSH on unit iFile.
! iTypeB is the boundary type: <0 for spline representation, +-1 for closed boundary.
! The nEdge corners of a C-polygon are stored in the tBndEdg array.
! The nSpline spline points are stored in the xSpline and ySpline arrays.
! The GHMS points are numbered successively, starting with number iPoint+1.
! Duplicate GMSH points must be avoided!
! iPoint1 ist the number of the first point of the first previous open boundary boundary 
! (0 if not yet defined or previous boundary closed).
! The GHMS lines and circles are numbered successively, starting with number iLine+1.
! On input, x1,y1 are the coordinates of the first point of the first previous open boundary boundary.
! x1=x2=-1d300 indicates that no such boundary exists or the previous boundary is closed.
! On output, x1,y1 are set to the coordinates of the first point of the current boundary if it is open and the previous one closed.
! On input, xn,yn are the coordinates of the last point of the previous boundary.
! On output, xn,yn are set to the coordinates of the last point of the current boundary.
! On input, lClosed is true if the previous boundary is closed or no previous bundary exists.
! On output, lClosed is true if the current boundary is closed.
  implicit none
  Integer(4) iFile,iBnd,i,iPoint,iLine,l,iPoint1,iP1,iP,i1,iOff,nE,iOffS,nS1
  Real(8) x,y,r,d,xa,xb,ya,yb,xo,yo,r0,xP,yP,dP,x1,y1,xn,yn,d1
  Logical lClosed
  Character(256) s
  iP1=iPoint+1
  xP=xn
  yP=yn
  iOffS=tBnd(iBnd)%iSplineOffset
  nS1=tBnd(iBnd)%nSpline
  iOff=tBnd(iBnd)%iEdgeOffset
  nE=tBnd(iBnd)%nEdge
! Spline type boundary
  if(tBnd(iBnd)%iTypB.lt.0_2) then ! splines
    i1=1
    r0=1.0d-8*max(abs(xSpline(iOffS+2)-xSpline(iOffS+1)),abs(ySpline(iOffS+2)-ySpline(iOffS+1)))
    if(iiabs(tBnd(iBnd)%iTypB).eq.1_2) then ! closed boundary -> don't write last point, which equals the first one
      nS1=nS1-1
      d1=pBig
    else ! open boundary -> check if linked to previous one
      d1=max(abs(xSpline(iOffS+1)-xn),abs(ySpline(iOffS+1)-yn)) ! distance from previous boundary
      if(d1.lt.r0) i1=2 ! skip first point
    end if
    do i=i1,nS1 ! add GMSH points
      x=xSpline(iOffS+i)
      y=ySpline(iOffS+i)
      d=tBndEdg(iOff+1)%d ! GMSH seems not to use individual d for each spline point -> no need to compute this precisely
      if(i.gt.(nS1/2)) d=tBndEdg(iOff+nE)%d
      iPoint=iPoint+1
      write(s,*) 'Point(',iPoint,')={',x,',',y,',0,',d,'};'
      l=-1
      call DelBlanks(s,l)
      write(iFile,'(A)') s(1:l)
    end do
    iLine=iLine+1 ! add a GMSH spline
    write(s,*) 'Spline(',iLine,')={'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    if(d1.lt.r0) then ! first point is last point of previous boundary!
      write(s,*) iP1-1,','
      l=-1
      call DelBlanks(s,l)
      write(iFile,'(A)') s(1:l)
    end if
    iP=iP1
    do i=1,tBnd(iBnd)%nSpline-1
      write(s,*) iP,','
      l=-1
      call DelBlanks(s,l)
      write(iFile,'(A)') s(1:l)
      iP=iP+1
    end do
    if(iiabs(tBnd(iBnd)%iTypB).eq.1_2) then ! closed curve: last point is first point
      lClosed=.true.
      iPoint1=0
      x1=nBig
      y1=nBig
      xn=xSpline(iOffS+nS1) ! coordinates of last point
      yn=ySpline(iOffS+nS1)
    else ! open curve
      lClosed=.false.
      if(x1.lt.-1.0d299) x1=xSpline(iOffS+1) ! coordinates of first point
      if(y1.lt.-1.0d299) y1=ySpline(iOffS+1)
      xn=xSpline(iOffS+nS1) ! coordinates of last point
      yn=ySpline(iOffS+nS1)
      d1=max(abs(xn-x1),abs(yn-y1)) ! distance from first point of first previous open boundary
      if(d1.lt.r0) then ! close the open curve
        iP1=iPoint1
        lClosed=.true.
        iPoint1=0
        x1=nBig
        y1=nBig
      end if
    end if
    write(s,*) iP1,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    return
  end if
! C-polygon type boundary
  if(nE.eq.1) then ! circle - 5 points, 4 arcs for GMSH
    lClosed=.true.
    iPoint1=0
    x1=nBig
    y1=nBig
    r=tBndEdg(iOff+1)%r
    x=tBndEdg(iOff+1)%x
    y=tBndEdg(iOff+1)%y
    d=tBndEdg(iOff+1)%d
    iPoint=iPoint+1
    write(s,*) 'Point(',iPoint,')={',x,',',y,',0,',d,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iPoint=iPoint+1
    write(s,*) 'Point(',iPoint,')={',x+r,',',y,',0,',d,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iPoint=iPoint+1
    write(s,*) 'Point(',iPoint,')={',x,',',y+r,',0,',d,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iPoint=iPoint+1
    write(s,*) 'Point(',iPoint,')={',x-r,',',y,',0,',d,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iPoint=iPoint+1
    write(s,*) 'Point(',iPoint,')={',x,',',y-r,',0,',d,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iLine=iLine+1
    write(s,*) 'Circle(',iLine,')={',iPoint-3,',',iPoint-4,',',iPoint-2,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iLine=iLine+1
    write(s,*) 'Circle(',iLine,')={',iPoint-2,',',iPoint-4,',',iPoint-1,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iLine=iLine+1
    write(s,*) 'Circle(',iLine,')={',iPoint-1,',',iPoint-4,',',iPoint,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iLine=iLine+1
    write(s,*) 'Circle(',iLine,')={',iPoint,',',iPoint-4,',',iPoint-3,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    xn=x+r ! coordinates of last point equal to first point
    yn=y
  else if(nE.eq.2) then ! line
    if(iPoint1.eq.0) then ! previous boundary was not open -> set new "first point"
      iPoint1=iP1
      x1=tBndEdg(iOff+1)%x
      y1=tBndEdg(iOff+1)%y
    end if
    d=tBndEdg(iOff+1)%d
    r0=1.0d-8*max(abs(tBndEdg(iOff+2)%x-tBndEdg(iOff+1)%x),abs(tBndEdg(iOff+2)%y-tBndEdg(iOff+1)%y))
    d1=max(abs(tBndEdg(iOff+1)%x-xn),abs(tBndEdg(iOff+1)%y-yn))
    if(d1.ge.r0) then ! don't skip first point
      iPoint=iPoint+1
      write(s,*) 'Point(',iPoint,')={',tBndEdg(iOff+1)%x,',',tBndEdg(iOff+1)%y,',0,',d,'};'
      l=-1
      call DelBlanks(s,l)
      write(iFile,'(A)') s(1:l)
    end if
    xn=tBndEdg(iOff+2)%x ! second point is last point
    yn=tBndEdg(iOff+2)%y
    d=tBndEdg(iOff+2)%d
    d1=max(abs(x1-xn),abs(y1-yn))
    if(d1.lt.r0) then ! second point of the line coincides with first point of first previous open boundary
      iP=iPoint1
      lClosed=.true.
      iLine=iLine+1
      write(s,*) 'Line(',iLine,')={',iPoint,',',iP,'};'
    else ! insert new point
      lClosed=.false.
      iPoint=iPoint+1
      write(s,*) 'Point(',iPoint,')={',xn,',',yn,',0,',d,'};'
      l=-1
      call DelBlanks(s,l)
      write(iFile,'(A)') s(1:l)
      iP=iPoint
      iLine=iLine+1
      write(s,*) 'Line(',iLine,')={',iPoint-1,',',iP,'};'
    end if
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    if(lClosed) then ! current line forms closed loop with previous lines
      iPoint1=0
      x1=nBig
      y1=nBig
    end if
  else ! Cpolygon with at least 3 corners
    if(iiabs(tBnd(iBnd)%iTypB).eq.1_2) then ! closed
      lClosed=.true.
      iPoint1=0
      x1=nBig
      y1=nBig
    else ! open
      lClosed=.false.
      if(iPoint1.eq.0) then ! previous boundary was not open -> set new "first point"
        iPoint1=iP1
        x1=tBndEdg(iOff+1)%x
        y1=tBndEdg(iOff+1)%y
      end if
    end if
    r0=1.0d-8*max(abs(tBndEdg(iOff+2)%x-tBndEdg(iOff+1)%x),abs(tBndEdg(iOff+2)%y-tBndEdg(iOff+1)%y))
    d1=max(abs(tBndEdg(iOff+1)%x-xn),abs(tBndEdg(iOff+1)%y-yn))
    do i=1,nE
      x=tBndEdg(iOff+i)%x
      y=tBndEdg(iOff+i)%y
      r=tBndEdg(iOff+i)%r
      if((iiabs(tBnd(iBnd)%iTypB).ne.1_2).and.((i.eq.1).or.(i.eq.1))) r=0.0d0 ! no arcs at end points of open lines
      d=tBndEdg(iOff+i)%d
      xa=tBndEdg(iOff+i)%xa
      ya=tBndEdg(iOff+i)%ya
      xb=tBndEdg(iOff+i)%xb
      yb=tBndEdg(iOff+i)%yb
      xo=tBndEdg(iOff+i)%xo
      yo=tBndEdg(iOff+i)%yo
      if(r.gt.r0) then ! circle (arc)
        if((i.eq.1).and.(d1.ge.r0)) then ! skip first point if it is close to last point of previous boundary
          iPoint=iPoint+1
          write(s,*) 'Point(',iPoint,')={',xa,',',ya,',0,',d,'};'
          l=-1
          call DelBlanks(s,l)
          write(iFile,'(A)') s(1:l)
        end if
        iPoint=iPoint+1
        write(s,*) 'Point(',iPoint,')={',xo,',',yo,',0,',d,'};'
        l=-1
        call DelBlanks(s,l)
        write(iFile,'(A)') s(1:l)
        iPoint=iPoint+1
        write(s,*) 'Point(',iPoint,')={',xb,',',yb,',0,',d,'};'
        l=-1
        call DelBlanks(s,l)
        write(iFile,'(A)') s(1:l)
        xP=xb
        yP=yb
        iLine=iLine+1
        write(s,*) 'Circle(',iLine,')={',iPoint-2,',',iPoint-1,',',iPoint,'};'
        l=-1
        call DelBlanks(s,l)
        write(iFile,'(A)') s(1:l)
      else ! insert point (if it is not close to the previous point xP,yP)
        dP=max(abs(xP-x),abs(yP-y))
        if(dP.gt.r0) then
          iPoint=iPoint+1
          write(s,*) 'Point(',iPoint,')={',x,',',y,',0,',d,'};'
          l=-1
          call DelBlanks(s,l)
          write(iFile,'(A)') s(1:l)
        end if
      end if
      if(i.lt.nE) then ! line
        xP=tBndEdg(iOff+i+1)%xa
        yP=tBndEdg(iOff+i+1)%ya
        d=tBndEdg(iOff+i+1)%d
        if(i.eq.nE-1) then
          d1=max(abs(x1-xP),abs(y1-yP))
          if(d1.lt.r0) then ! point coincides with first point of first previous open boundary
            iP=iPoint1
          else
            iPoint=iPoint+1
            iP=iPoint
          end if
        else
          d1=pBig
          iPoint=iPoint+1
          iP=iPoint
        end if
        if(iP.ne.iPoint1) then ! insert new point
          write(s,*) 'Point(',iPoint,')={',xP,',',yP,',0,',tBndEdg(iOff+i+1)%d,'};'
          l=-1
          call DelBlanks(s,l)
          write(iFile,'(A)') s(1:l)
          iLine=iLine+1
          write(s,*) 'Line(',iLine,')={',iPoint-1,',',iPoint,'};'
        else
          iLine=iLine+1
          write(s,*) 'Line(',iLine,')={',iPoint,',',iP,'};'
        end if
        l=-1
        call DelBlanks(s,l)
        write(iFile,'(A)') s(1:l)
      end if
    end do
    if(iiabs(tBnd(iBnd)%iTypB).eq.1_2) then ! closed boundary: line back to start point
      d1=max(abs(x1-xP),abs(y1-yP))
      if(d1.gt.r0) then
        iLine=iLine+1
        write(s,*) 'Line(',iLine,')={',iPoint,',',iP1,'};'
        l=-1
        call DelBlanks(s,l)
        write(iFile,'(A)') s(1:l)
      end if
      lClosed=.true.
      iPoint1=0
      x1=nBig
      y1=nBig
    else
      lClosed=.false.
      if(iPoint1.eq.0) then ! previous boundary was not open
        iPoint1=iP1
        x1=tBndEdg(iOff+1)%x
        y1=tBndEdg(iOff+1)%y
      end if
      if(d1.lt.r0) then  ! current line forms closed loop with previous lines
        iPoint1=0
        x1=nBig
        y1=nBig
        lClosed=.true.
      end if
    end if
    xn=xP
    yn=yP
  end if
  end subroutine GMSHwriteBndPoints

  subroutine GMSHwriteGeoFile(iFile,FileName,i1Bnd,i2Bnd,iDom,iCol)
! Write a *.GEO file "FileName" for GMSH on unit iFile.
! Export all boundaries with numbers i1Bnd up to i2Bnd with domain number iDom either to the left or right side and color number iCol.
  implicit none
  Real(8) x1,y1,xn,yn
  Integer(4) iFile,iPoint,iPoint1,iLine,iBnd,i1Bnd,i2Bnd,iL,i1,l
  Integer(4), allocatable:: iLL(:)
  Integer(2) iDom,iCol
  Logical lClosed,l1
  Character(256) FileName,s
  Open(iFile,file=FileName)
  iPoint=0
  iLine=0
  allocate(iLL(nBnd)) ! store numbers of the "Line Loop" elements
! write "Line Loop" for each boundary
  x1=nBig ! previous boundary not yet defined -> set impossible coordinates
  y1=nBig
  xn=x1
  yn=y1
  lClosed=.true.
  iPoint1=0
  iLL=0
  do iBnd=i1Bnd,i2Bnd
    if(iDom.gt.0) then ! consider only the specified domain
      if((iDom.ne.tBnd(iBnd)%iLDom).and.(iDom.ne.tBnd(iBnd)%iRDom)) Cycle
    else if(iDom.lt.0) then ! consider domains with numbers up to -iDom
      if((-iDom.lt.tBnd(iBnd)%iLDom).and.(-iDom.lt.tBnd(iBnd)%iRDom)) Cycle
    end if
    if(iCol.gt.0) then ! consider only the specified color
      if(iCol.ne.tBnd(iBnd)%iCol) Cycle
    else if(iCol.lt.0) then ! consider color numbers up to -iCol
      if(-iCol.lt.tBnd(iBnd)%iCol) Cycle
    end if
    i1=iLine+1
    call GMSHwriteBndPoints(iFile,iBnd,iPoint,iPoint1,iLine,x1,y1,xn,yn,lClosed)
    write(iFile,*) 'Line Loop(',iLine+1,')={' ! the blank between Line and Loop must be present!
    do iL=i1,iLine-1
      write(s,*) iL,','
      l=-1
      call DelBlanks(s,l)
      write(iFile,'(A)') s(1:l)
    end do
    write(s,*) iLine,'};'
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
    iLine=iLine+1
    iLL(iBnd)=iLine
  end do
! write "Plane Surface" containing all "Line Loop"s
  write(iFile,*) 'Plane Surface(',iLine+1,')={' ! the blank between Plane and Surface must be present!
  l1=.true.
  do iBnd=i1Bnd,i2Bnd
    if(iLL(iBnd).lt.1) Cycle
    if(l1) then
      l1=.false.
      write(s,*) iLL(iBnd)
    else
      write(s,*) ',',iLL(iBnd)
    end if
    l=-1
    call DelBlanks(s,l)
    write(iFile,'(A)') s(1:l)
  end do
  write(s,*) '};'
  l=-1
  call DelBlanks(s,l)
  write(iFile,'(A)') s(1:l)
  close(iFile)
  deallocate(iLL)
  end subroutine GMSHwriteGeoFile

  subroutine GMSHcountPoints(iFile,FileName,lFN,nP,n1,n2,n3,m1,m2,m3,lOrdered)
! count point numbers in a *.MSH file with name FileName, name length lFN
! read file on unit iFile
! n1: number of 1-node points (type it=15)
! n2: number of points on 2-node lines (type it=1)
! n3: number points on 3-node triangle (type it=2)
! if the *.msh file is correctly ordered, the node list should start with n1 1-node points, followed by n2 2-node points, and n3 3-node points
! the element list should start with m1 point elements, followed by m2 line elements, and m3 triangle elements
! all elements should be characterized by number / type / 3 / arg1 / arg2 / arg3 / point numbers (1-3 depending on type)
  Implicit none
  Integer(4) iFile,lFN,nP,n1,n2,n3,m1,m2,m3,ios,nEle,in,it,na,i,i1,i2,i3,nE1,nE2,nE3
  Logical lOrdered
  Character(256) FileName,str
! check if ordered standard file
  nP=0
  n1=0
  n2=0
  n3=0
  lOrdered=.false.
  open(iFile,file=FileName(1:lFN)//Char(0),status='old',iostat=ios)
  if(ios.ne.0) return
  do
    read(iFile,'(A)',iostat=ios) str
    if(ios.ne.0) then
      ios=-1
      Exit
    end if
    if(str(1:4).eq.'$Nod') then
      read(iFile,*,iostat=ios) nP
      if(ios.ne.0) then
        ios=-1
        Exit
      end if
    else if(str(1:4).eq.'$Ele') then
      ios=1
      Exit
    end if
  end do
  if(ios.eq.1) then
    read(iFile,*) nEle
    lOrdered=.true.
    do i=1,nEle
      read(iFile,*,iostat=ios) in,it,na
      if(ios.ne.0) Exit
      if(in.ne.i) lOrdered=.false.
      if(na.ne.3) lOrdered=.false.
      if((it.ne.15).or.(.not.lOrdered)) Exit ! no point element
      n1=n1+1
    end do
    if(lOrdered.and.(it.eq.1)) then ! no error, continue with line elements
      write(*,*) 'point elements:',n1
      n2=n2+1
      do i=n1+2,nEle
        read(iFile,*,iostat=ios) in,it,na
        if(ios.ne.0) Exit
        if(in.ne.i) lOrdered=.false.
        if(na.ne.3) lOrdered=.false.
        if((it.ne.1).or.(.not.lOrdered)) Exit ! no line element
        n2=n2+1
      end do
      if(lOrdered.and.(it.eq.2)) then ! no error, continue with triangular elements
        write(*,*) 'line elements:',n2
        n3=n3+1
        do i=n1+n2+2,nEle
          read(iFile,*,iostat=ios) in,it,na
          if(ios.ne.0) Exit
          if(in.ne.i) lOrdered=.false.
          if(na.ne.3) lOrdered=.false.
          if((it.ne.2).or.(.not.lOrdered)) Exit ! no triangular element
          n3=n3+1
        end do
        write(*,*) 'triangular elements, ordered:',n3,lOrdered
      end if
    end if
  end if
  close(iFile)
  if(.not.lOrdered) return
! now, read node point ranges m1, m2, m3
  m1=0
  m2=0
  m3=0
  open(iFile,file=FileName(1:lFN)//Char(0),status='old',iostat=ios)
  if(ios.ne.0) return
  do
    read(iFile,'(A)',iostat=ios) str
    if(ios.ne.0) then
      ios=-1
      Exit
    end if
    if(str(1:4).eq.'$Ele') then
      ios=1
      Exit
    end if
  end do
  if(ios.eq.1) then
    read(iFile,*) nEle
    do i=1,n1
      read(iFile,*,iostat=ios) in,it,na,i1,i2,i3,nE1
      if(ios.ne.0) Exit
      m1=max(m1,nE1)
    end do
    do i=1,n2
      read(iFile,*,iostat=ios) in,it,na,i1,i2,i3,nE1,nE2
      if(ios.ne.0) Exit
      m2=max(m2,nE1,nE2)
    end do
    do i=1,n3
      read(iFile,*,iostat=ios) in,it,na,i1,i2,i3,nE1,nE2,nE3
      if(ios.ne.0) Exit
      m3=max(m3,nE1,nE2,nE3)
    end do
  end if
  close(iFile)
  write(*,*) 'n1,m1=',n1,m1
  write(*,*) 'n2,m2=',n2,m2
  write(*,*) 'n3,m3=',n3,m3
  end subroutine GMSHcountPoints

  subroutine GMSHgetNearElements(iFile,FileName,lFN,n1,n2,n3,iE,nE,iP,nP,nnE,nnP)
! get GMSH elements and points that are near the boundary (read unit iFile, file name FileName with length lFN)
! input: n1,n2,n3 number of 1-, 2-, 3-point elements; nP: number of elements, iP: number of points (obtained from GMSHcountPoints)
! element ID number iE is set as follows: 0: 1-point element, 1: 2-point element, 2: 3-point element near boundary, 4:3-point element away from boundary
! point ID number iP is set as follows: 0: point on boundary or auxiliary point, 1: point near boundary, 2: point away from boundary
! nnE: number of elements near the boundary, nnP: number of points near the boundary
  Implicit none
  Integer(4) iFile,lFN,n1,n2,n3,ios,n,i,in,it,na,i1,i2,i3,nP1,nP2,nP3,nE,nP,nnE,nnP
  Integer(1) iE(nE),iP(nP)
  Character(256) FileName,str
  iE(1:n1)=0_1
  iE(n1+1:n1+n2)=1_1
  iE(n1+n2+1:n1+n2+n3)=3_1
  iP(1:nP)=2_1
  open(iFile,file=FileName(1:lFN)//Char(0),status='old',iostat=ios)
  if(ios.ne.0) return
  do
    read(iFile,'(A)',iostat=ios) str
    if(ios.ne.0) then
      ios=-1
      Exit
    end if
    if(str(1:4).eq.'$Ele') then
      ios=1
      Exit
    end if
  end do
  if(ios.eq.1) then
    read(iFile,*) n
    do i=1,n1
      read(iFile,*,iostat=ios) in,it,na,i1,i2,i3,nP1
      if(ios.ne.0) Exit
      iP(nP1)=0_1 ! this point is on the boundary
    end do
    do i=n1+1,n1+n2
      read(iFile,*,iostat=ios) in,it,na,i1,i2,i3,nP1,nP2
      if(ios.ne.0) Exit
      iP(nP1)=0_1 ! this point is on the boundary
      iP(nP2)=0_1 ! this point is on the boundary
    end do
    nnE=0
    do i=n1+n2+1,n1+n2+n3
      read(iFile,*,iostat=ios) in,it,na,i1,i2,i3,nP1,nP2,nP3
      if(ios.ne.0) Exit
      if((iP(nP1).lt.1_1).or.(iP(nP2).lt.1_1).or.(iP(nP3).lt.1_1)) then ! element near boundary
        iE(i)=2_1
        nnE=nnE+1
        if(iP(nP1).eq.2_1) iP(nP1)=1_1 ! this point is near the boundary
        if(iP(nP2).eq.2_1) iP(nP2)=1_1 ! this point is near the boundary
        if(iP(nP3).eq.2_1) iP(nP3)=1_1 ! this point is near the boundary
      end if
    end do
  end if
  close(iFile)
  nnP=0
  do i=1,nP
    if(iP(i).eq.1_1) nnP=nnP+1
  end do
  end subroutine GMSHgetNearElements

  subroutine GMSHreadNearPoints(iFile,FileName,lFN,iP,nP,rP,nnP)
! read the Cartesian coordinates (vector rP) of the nnP points near the boundary (in a GMSH mesh file FileName)
! use unit iFile for reading, length of file name is lFN
! input: iP,nP,nnP is obtained from GMSHgetNearElements)
  Implicit none
  Integer(4) iFile,lFN,i,n,id,nP,nnP,ios,kP
  Integer(1) iP(nP)
  real(8) rP(3,nnP),r(3)
  Character(256) FileName,str
  open(iFile,file=FileName(1:lFN)//Char(0),status='old',iostat=ios)
  if(ios.ne.0) return
  do
    read(iFile,'(A)',iostat=ios) str
    if(ios.ne.0) then
      ios=-1
      Exit
    end if
    if(str(1:4).eq.'$Nod') then
      ios=1
      Exit
    end if
  end do
  if(ios.eq.1) then
    read(iFile,*) n
    kP=0
    do i=1,min(n,nP)
      read(iFile,*,iostat=ios) id,r(1:3)
      if(ios.ne.0) then
        write(*,*) 'Error reading line / point ',i,kP
      else
        if(iP(i).eq.1_1) then
          kP=kP+1
          rP(1:3,kP)=r(1:3)
        end if
        if(kP.ge.nnP) Exit
      end if
    end do
  end if
  close(iFile)
  nnP=kP
  end subroutine GMSHreadNearPoints

  Subroutine FindGMSH(FileString,lF,lExist)
! locate the gmsh.exe code in some reasonable directory
    Integer(4) lF
    Character(256) FileString
    Logical lExist
    inquire(file=MaxFileDir(1:lMaxFileDir)//'gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString=MaxFileDir(1:lMaxFileDir)//'gmsh.exe'//Char(0)
      lF=lMaxFileDir+8
      return
    end if
    inquire(file='gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString='gmsh.exe'//Char(0)
      lF=8
      return
    end if
    inquire(file='\gmsh\gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString='\gmsh\gmsh.exe'//Char(0)
      lF=14
      return
    end if
    inquire(file='\bin\gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString='\bin\gmsh.exe'//Char(0)
      lF=13
      return
    end if
    inquire(file='\codes\gmsh\gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString='\codes\gmsh\gmsh.exe'//Char(0)
      lF=20
      return
    end if
    inquire(file='\OpenMaXwell\gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString='\OpenMaXwell\gmsh.exe'//Char(0)
      lF=17
      return
    end if
    inquire(file='\OpenMaXwell\gmsh\gmsh.exe'//Char(0),Exist=lExist)
    if(lExist) then
      FileString='\OpenMaXwell\gmsh\gmsh.exe'//Char(0)
      lF=22
      return
    end if
  end Subroutine FindGMSH

END MODULE CHBND


