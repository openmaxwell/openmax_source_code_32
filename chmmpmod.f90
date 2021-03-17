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
MODULE CHMMP

! Expansions

  USE CHINT

  SAVE

  Character(20), Parameter :: CHMmpIdent=' CHMMP Version 3.1  '
  Complex(8), Allocatable :: MMPMtr(:),TriMtr(:),FldMtr(:,:),sMtr(:,:),xMtr(:),CGsi(:),CGpi(:),CGri(:),CGqi(:), &
  & QRMtr(:,:),QRRMtr(:,:),QRWMtr(:)
  Real(8), Allocatable :: cMtr(:),rMtr(:)
  Real(8) residual,resUnscaled,resEigen,resCG,resPET,fMatrix0
  Integer(4), Allocatable :: iMtr(:)
  Integer(4) mCol,nRow,mRow,mTri,mMtr,iMtrSolver,iResCount,iunitRes,itmCG,itCG,iaccCG, &
  &          iAmpl,iAmplTyp,iupTri,ithvmax0,iScaleMatrix,mColsMtr,iGetEigFld,iEvlTest
  Integer(2) iMMPfindExp,iMMPeigen,iMMPresV,iEvlDraw,iEvlCol
  Logical lMMPrMtr,lMMPtMtr,lMMPrough,lMMPfine,lMMPuseRes,lMMP3F,lEvlSMP
  Character(256) MmpFileName,CfkFileName

  CONTAINS

  Subroutine MMP_Defaults(lCheck)
! set default MMP parameters
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
	  ldum=lCheck
    if(.not.lgcFld) lGet3DMat=.true.
    call setNameExt(ProFileName,'MMP',MmpFileName)
    call setNameExt(ProFileName,'BND',CfkFileName)
    call setNameExt(ProFileName,'BAS',PETFileName)
    sPET(1)='1'C
    sPET(2)='x'C
    sPET(3)='pow(x,2)'C
    sPET(4)='pow(x,3)'C
    sPET(5)='pow(x,4)'C
    sPET(6)='pow(x,5)'C
    sPET(7)='pow(x,6)'C
    sPET(8)='pow(x,7)'C
    sPET(9)='pow(x,8)'C
    sPET(10)='pow(x,9)'C
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
    lMMPBndVal=.false.
    iMtrSolver=5
    iAmpl=1
    iAmplTyp=0
    iResCount=-1
    iunitRes=0
    residual=pBig
    resUnscaled=pBig
    resEigen=pBig
    rMMPResV(1:9)=1.0d0
    errorM=0.0d0
    errorMA=0.0d0
    errorA=0.0d0
    Condition=-1.0d0
    errorScale=0.0d0
    BndDMax=0.2
    BndPpW=5.0
    BndOver=2.0
    smallMatDist=1.0d-5
    nSegPt=2
    iMMPeigen=0_2
    lMMPrMtr=.false.
    lMMPtMtr=.false.
    lMMPrough=.true.
    lMMPfine=.true.
    lMMPuseRes=.true.
    lMMP3F=.false.
    iMMPfindExp=0_2
    iMMPresV=0_2
    lPET=.false.
    lEvlSMP=.false.
    iEvlDraw=0_2
    iEvlCol=1_2
    nPET=3
    kPET=0
    kPETD=1
    npPET=0
    iMMPCon=0_2
    iFunWeight=0_4
    ithvmax0=7_4
    itCG=0
    itmCG=10
    iaccCG=10
    resCG=0.0d0
    resPET=4.0d0
    fMatrix0=0.0d0
    iScaleMatrix=2
    lMMPDeterminant=.false.
    iMMPlast=0
    nRow=1
    mRow=100
    iEvlTest=3
    iWorkSpace=-1 ! check and set without display
    call AllocateMtr(ldum)
    if(.not.ldum) return
    if(Allocated(MMPMtr)) then
      MMPMtr=(1.0d0,0.0d0)
    end if
  end Subroutine MMP_Defaults

! Allocations

  Subroutine AllocateMtr(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(QRMtr)) DeAllocate(QRMtr)
    if(Allocated(QRRMtr)) DeAllocate(QRRMtr)
    if(Allocated(QRWMtr)) DeAllocate(QRWMtr)
    if(Allocated(MMPMtr)) DeAllocate(MMPMtr)
    if(Allocated(FldMtr)) DeAllocate(FldMtr)
    if(Allocated(TriMtr)) DeAllocate(TriMtr)
    if(Allocated(cMtr)) DeAllocate(cMtr)
    if(Allocated(xMtr)) DeAllocate(xMtr)
    if(Allocated(rMtr)) DeAllocate(rMtr)
    if(Allocated(sMtr)) DeAllocate(sMtr,stat=idum)
    if(Allocated(iMtr)) DeAllocate(iMtr)
    if(Allocated(CGri)) DeAllocate(CGri)
    if(Allocated(CGsi)) DeAllocate(CGsi)
    if(Allocated(CGpi)) DeAllocate(CGpi)
    if(Allocated(CGqi)) DeAllocate(CGqi)
    call getmCol()
    mTri=max(mTri,1)
    mRow=max(mRow,1)
    mCol=max(mCol,1)
    mColsMtr=max(mColsMtr,1)
    nRHS=max(nRHS,1)
    idum=0
    if(iMtrSolver.ne.5) then
      Allocate(TriMtr(1:mTri),stat=idum)
      if(idum.eq.0) Allocate(cMtr(1:mCol),stat=idum)
    end if
    if(idum.eq.0) Allocate(FldMtr(1:10,1:mCol),stat=idum)
    if(idum.eq.0) Allocate(rMtr(1:mCol),stat=idum)
    if(iMtrSolver.lt.2) then
      if(idum.eq.0) Allocate(sMtr(1:mCol,1:nRHS),stat=idum)
    else
      if(idum.eq.0) Allocate(sMtr(1:mColsMtr,1:nRHS),stat=idum)
    end if
    if(idum.eq.0) Allocate(xMtr(1:mCol),stat=idum)
    if(idum.eq.0) Allocate(iMtr(1:mCol),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for TriMtr failed!'C,'Allocate MMP matrices'C, &
                        MB$OK.or.MB$IconExclamation)
      if(Allocated(TriMtr)) DeAllocate(TriMtr)
      if(Allocated(FldMtr)) DeAllocate(FldMtr)
      if(Allocated(cMtr)) DeAllocate(cMtr)
      if(Allocated(rMtr)) DeAllocate(rMtr)
      if(Allocated(sMtr)) DeAllocate(sMtr)
      if(Allocated(xMtr)) DeAllocate(xMtr)
      if(Allocated(iMtr)) DeAllocate(iMtr)
      nRow=0
      mRow=0
      mCol=0
      mTri=0
      mMtr=0
      return
    end if
    iupTri=0
    if(iMtrSolver.ne.5) TriMtr(1:mTri)=(0.0d0,0.0d0)
    FldMtr(1:10,1:mCol)=(0.0d0,0.0d0)
    sMtr=(0.0d0,0.0d0)
    xMtr(1:mCol)=(0.0d0,0.0d0)
    if(iMtrSolver.ne.5) cMtr(1:mCol)=0.0d0
    rMtr(1:mCol)=0.0d0
    iMtr(1:mCol)=0_4
    ldum=.true.
    if((iMtrSolver.eq.2).or.(iMtrSolver.eq.3)) then ! allocate auxiliary arrays for CG
      mMtr=mRow*mCol
      mMtr=max(mMtr,1)
      Allocate(MMPMtr(1:mMtr),CGsi(1:mCol),CGpi(1:mCol),CGri(1:mRow),CGqi(1:mRow),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for MMPMtr failed!'C,'Allocate MMP-CG matrices'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(MMPMtr)) DeAllocate(MMPMtr)
        if(Allocated(CGsi)) DeAllocate(CGsi)
        if(Allocated(CGpi)) DeAllocate(CGpi)
        if(Allocated(CGri)) DeAllocate(CGri)
        if(Allocated(CGqi)) DeAllocate(CGqi)
        mMtr=0
        iMtrSolver=0
      else
        MMPMtr(1:mMtr)=(0.0d0,0.0d0)
        CGsi(1:mCol)=(0.0d0,0.0d0)
        CGpi(1:mCol)=(0.0d0,0.0d0)
        CGri(1:mRow)=(0.0d0,0.0d0)
        CGqi(1:mRow)=(0.0d0,0.0d0)
      end if
    else if(iMtrSolver.eq.1) then ! allocate auxiliary arrays for GUR
      mMtr=mRow*mCol
      Allocate(MMPMtr(1:mMtr),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for MMPMtr failed!'C,'Allocate MMP-Rect matrices'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(MMPMtr)) DeAllocate(MMPMtr)
        mMtr=0
        iMtrSolver=0
      else
        MMPMtr(1:mMtr)=(0.0d0,0.0d0)
      end if
    else if(iMtrSolver.eq.5) then ! allocate auxiliary arrays for QR
      if(iWorkSpace.gt.0) then
        Allocate(QRMtr(mRow,mCol-nRHS),QRRMtr(mRow,nRHS),QRWMtr(iWorkSpace*mRow),stat=idum)
      else
        Allocate(QRMtr(mRow,mCol-nRHS),QRRMtr(mRow,nRHS),QRWMtr(1),stat=idum)
      end if
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for QRMtr failed!'C,'Allocate MMP-Rect matrices'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(QRMtr)) DeAllocate(QRMtr)
        if(Allocated(QRRMtr)) DeAllocate(QRRMtr)
        if(Allocated(QRWMtr)) DeAllocate(QRWMtr)
        iMtrSolver=0
      end if
    end if
	end Subroutine AllocateMtr

! threads

  Subroutine TMMPRes(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=1_4
		call StartMMPThread(lCheck)
  end Subroutine TMMPRes

  Subroutine TMMPEvl(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=2_4
		call StartMMPThread(lCheck)
  end Subroutine TMMPEvl

  Subroutine TMMPErr(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=3_4
		call StartMMPThread(lCheck)
  end Subroutine TMMPErr

  Subroutine TMMPResErr(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=4_4
		call StartMMPThread(lCheck)
  end Subroutine TMMPResErr

  Subroutine TMMPEvlErr(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=5_4
		call StartMMPThread(lCheck)
  end Subroutine TMMPEvlErr

  Subroutine TMMPCnd(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=6_4
		call StartMMPThread(lCheck)
  end Subroutine TMMPCnd

  Subroutine StartMMPThread(ldi)
! start the MMP thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start MMP thread'C)
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
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start MMP thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start MMP thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(MMPThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start MMP thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
    end if
  end Subroutine StartMMPThread

  Integer(4) Function MMPThread(iwhat)
! MMP tread: calls.....
    Implicit none
    Include 'resource.fd'
    Integer(4) iwhat
    Logical ldum
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    MMPThread=0_4
    if(.not.lgcFld) lGet3DMat=.true.
    if(iThreadAction.eq.1) then
      resEigen=getRes(fcFld)
    else if(iThreadAction.eq.2) then
      call getEigen(ldum)
      lEigen=.false.
      ldum=lPET
      lPET=.false.
      resEigen=getRes(fcFld)
      lPET=ldum
      lEigen=.true.
    else if(iThreadAction.eq.3) then
      call getErrors(.true.)
    else if(iThreadAction.eq.4) then
      resEigen=getRes(fcFld)
      call getErrors(.true.)
    else if(iThreadAction.eq.5) then
      call getEigen(ldum)
      lEigen=.false.
      ldum=lPET
      lPET=.false.
      resEigen=getRes(fcFld)
      lPET=ldum
      call getErrors(.true.)
      lEigen=.true.
    else if(iThreadAction.eq.6) then
      if(Allocated(MMPMtr)) then
        condition=getCondition(MMPMtr,mCol,nRow)
      else
        condition=-1.0d0
      end if
    else
      MMPThread=1_4
    end if
    call endThread()
  end Function MMPThread

! I/O

  Subroutine SaveError(lCheck,l2D)
! save all current MMP error data in a function file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical l2
    Logical, Optional, intent(in):: l2D
	  Integer(4) i,iOK,ios,idum
    if(Present(l2D)) then
      l2=l2D
    else
      l2=lgcFld
    end if
    if(.not.lCheck) then
      call Open2write(-1,'Select error data file to be written!','Function data file ',FunFileName,'FUN',ios)
      if(ios.gt.0) then
	      call closeFunction(1)
        return
      end if
      if((.not.Present(l2D)).and.(.not.lgcFld)) then
        l2=.false.
        idum=MessageBoxQQ('Save errors along 2D boundaries?'C,'Save errors'C, &
                          MB$YESNO.or.MB$IconQuestion)
        if(idum.eq.MB$IDYes) l2=.true.
      end if
    end if
    open(1,file=FunFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save errors'C, &
                        MB$OK.or.MB$IconExclamation)
	    call closeFunction(1)
      return
    end if
    call WriteStr(1,CHFunIdent,iOK)
    ich(1)=0
    ich(2)=3
    if(l2.or.lgcFld) ich(2)=4
    call chwrit2(1,ich,2,rch,0,sch,0,iOK)
    call WriteStr(1,'n'C,iOK)
    call WriteStr(1,'error'C,iOK)
    call WriteStr(1,'field'C,iOK)
    call WriteStr(1,'relative error'C,iOK)
    if(l2.or.lgcFld) then
      call WriteStr(1,'location'C,iOK)
      do i=1,nBndPt
        rch(1)=eBndPt(i)
        rch(2)=fBndPt(i)
        rch(3)=100.0d0*(eBndPt(i).div.fBndPt(i))
        rch(4)=sBndPt(i)
        call chwrit2(1,ich,0,rch,4,sch,0,iOK)
      end do
    else
      do i=1,nBndPt3D
        if(.not.lgcFld) then
          if(iObjBndPt3D(i).lt.0) Cycle
        end if
        rch(1)=eBndPt3D(i)
        rch(2)=fBndPt3D(i)
        rch(3)=100.0d0*(eBndPt3D(i).div.fBndPt3D(i))
        call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      end do
    end if
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
	  call closeFunction(1)
  end Subroutine SaveError

  Subroutine SaveRmatrix(lCheck)
! save the current rectangular MMP matrix in a field file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) i,iOK,ios,idum,ik,j
	  Character(3) EXT
    if(.not.lCheck) then
      call Open2write(-1,'Select matrix data file to be written!','Field file ',FldFileName,'FLD',ios)
      if(ios.gt.0) then
        call closeField(1)
        return
      end if
    end if
    open(1,file=FldFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save R matrix'C, &
                        MB$OK.or.MB$IconExclamation)
      call closeField(1)
      return
    end if
    call getNameExt(FldFileName,ext)
    if((ext(1:3).ne.'fld').and.(ext(1:3).ne.'FLD')) then ! standard TXT file
      ich(1)=mCol
      ich(2)=nRow
      sch(1:10)=' mCol,nRow'
      call chwrit2(1,ich,2,rch,0,sch,10,iOK)
      if(Allocated(MMPMtr)) then
! matrix data
        ik=0
        do j=1,nRow
          do i=1,mCol
            ik=ik+1
            rch(1)=Dble(MMPMtr(ik))
            rch(2)=DImag(MMPMtr(ik))
            write(1,*) rch(1:2)
          end do
        end do
      end if
      ich(1)=nExp
      sch(1:5)=' nExp'
      call chwrit2(1,ich,1,rch,0,sch,5,iOK)
      do i=1,nExp
        ich(1)=tExp(i)%nPar
        rch(1)=tExp(i)%Plane(1,0)
        rch(2)=tExp(i)%Plane(2,0)
        write(1,*) ich(1),rch(1:2)
      end do
    else
      call WriteStr(1,CHFldIdent,iOK)
      ich(1)=0
      sch(1:23)=' no representation data'
      call chwrit2(1,ich,1,rch,0,sch,23,iOK)
! cfield data
      ich(2:6)=0
      ich(1)=1
      ich(7:10)=1
      sch(1:10)=' lxcFld,...'
      call chwrit2(1,ich,10,rch,0,sch,10,iOK)
      ich(1)=mCol
      ich(2)=nRow
      ich(3)=1
      sch(1:10)=' nxcFld,..'
      call chwrit2(1,ich,3,rch,0,sch,10,iOK)
      rch(1:3)=0.0d0
      rch(2)=1.0d0
      sch(1:8)=' origin '
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=0.0d0
      rch(1)=1.0d0
      sch(1:8)=' x tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=0.0d0
      rch(2)=-1.0d0
      sch(1:8)=' y tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=0.0d0
      rch(3)=1.0d0
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      if(Allocated(MMPMtr)) then
! matrix data
        ich(1)=1
        sch(1:13)=' field values'
        call chwrit2(1,ich,1,rch,0,sch,13,iOK)
        ik=0
        do j=1,nRow
          do i=1,mCol
            ich(1)=1
            call chwrit2(1,ich,1,rch,0,sch,0,iOK)
            ik=ik+1
            rch(1)=Dble(MMPMtr(ik))
            rch(2)=DImag(MMPMtr(ik))
            call chwrit2(1,ich,0,rch,2,sch,0,iOK)
          end do
        end do
      else
        ich(1)=0
        sch(1:16)=' no field values'
        call chwrit2(1,ich,1,rch,0,sch,16,iOK)
      end if
    end if
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    call closeField(1)
  end Subroutine SaveRmatrix

  Subroutine SaveTmatrix(lCheck)
! save the current trapezoidal MMP matrix in a field file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) i,iOK,ios,idum,ik,ik0,j,ikd
	  Character(3) EXT
    if(.not.lCheck) then
      call Open2write(-1,'Select matrix data file to be written!','Field file ',FldFileName,'FLD',ios)
      if(ios.gt.0) then
        call closeField(1)
        return
      end if
    end if
    open(1,file=FldFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save R matrix'C, &
                        MB$OK.or.MB$IconExclamation)
      call closeField(1)
      return
    end if
    call getNameExt(FldFileName,ext)
    if((ext(1:3).ne.'fld').and.(ext(1:3).ne.'FLD')) then ! standard TXT file
      ich(1)=mCol
      ich(2)=mColsMtr
      sch(1:10)=' mCol,nRow'
      call chwrit2(1,ich,2,rch,0,sch,10,iOK)
      if(Allocated(TriMtr)) then
! matrix data
        ik0=0
        do j=1,mColsMtr
          ik0=ik0+j
          do i=1,mCol
            ikd=j
            ik=ik0
            if(i.ge.j) then
              rch(1)=Dble(TriMtr(ik))
              rch(2)=DImag(TriMtr(ik))
              ik=ik+ikd
              ikd=min(ikd+1,mColsMtr)
            else
              rch(1)=0.0d0
              rch(2)=0.0d0
            end if
            call chwrit2(1,ich,0,rch,2,sch,0,iOK)
          end do
        end do
      end if
    else ! FLD file
      call WriteStr(1,CHFldIdent,iOK)
      ich(1)=0
      sch(1:23)=' no representation data'
      call chwrit2(1,ich,1,rch,0,sch,23,iOK)
! cfield data
      ich(2:6)=0
      ich(1)=1
      ich(7:10)=1
      sch(1:10)=' lxcFld,...'
      call chwrit2(1,ich,10,rch,0,sch,10,iOK)
      ich(1)=mCol
      ich(2)=mColsMtr
      ich(3)=1
      sch(1:10)=' nxcFld,..'
      call chwrit2(1,ich,3,rch,0,sch,10,iOK)
      rch(1:3)=0.0d0
      rch(2)=1.0d0
      sch(1:8)=' origin '
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=0.0d0
      rch(1)=1.0d0
      sch(1:8)=' x tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=0.0d0
      rch(2)=-1.0d0
      sch(1:8)=' y tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=0.0d0
      rch(3)=1.0d0
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      if(Allocated(TriMtr)) then
! matrix data
        ich(1)=1
        sch(1:13)=' field values'
        call chwrit2(1,ich,1,rch,0,sch,13,iOK)
        ik0=0
        do j=1,mColsMtr
          ik0=ik0+j
          do i=1,mCol
            ikd=j
            ik=ik0
            ich(1)=1
            call chwrit2(1,ich,1,rch,0,sch,0,iOK)
            if(i.ge.j) then
              rch(1)=Dble(TriMtr(ik))
              rch(2)=DImag(TriMtr(ik))
              ik=ik+ikd
              ikd=min(ikd+1,mColsMtr)
            else
              rch(1)=0.0d0
              rch(2)=0.0d0
            end if
            call chwrit2(1,ich,0,rch,2,sch,0,iOK)
          end do
        end do
      else
        ich(1)=0
        sch(1:16)=' no field values'
        call chwrit2(1,ich,1,rch,0,sch,16,iOK)
      end if
    end if
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    call closeField(1)
  end Subroutine SaveTmatrix

  Subroutine saveBasis(lCheck)
! save PET basis data in a file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) iOk,ios,i,l,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select PET basis data file to be written!','PET data file ',PETFileName,'BAS',ios)
      if(ios.gt.0) return
    end if
    open(1,file=PETFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save PET data'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHPETIdent,iOK)
    ich(1)=nPET
    rch(1)=fPET
    sch(1:13)=' nBas,fitness'
    call chwrit2(1,ich,1,rch,1,sch,13,iOK)
    do i=1,nPET
      l=GetSLength(sPET(i))
      write(1,'(a)') sPET(i)(1:l)
      write(1,*) '1 0 0'
    end do
    close(1)
  end Subroutine saveBasis

  Subroutine openBasis(lCheck)
! read PET basis data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist
	  Integer(4) iOk,ios,i,ls,np,mp,ic,lci,lc,idum
    Integer(2) l1s
    Real(8) a,p,f
    Character(1151) so
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select PET basis data file to be read!','PET data file ',PETFileName,'BAS',ios)
      if(ios.gt.0) return
    end if
    inquire(file=PETFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=PETFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open PET data'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHPETIdent(1:18).ne.text(1:18)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open PET data'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    read(1,*) nPET,f
    nPET=min(nPET,mPET)
    do i=1,nPET
      read(1,'(a)') sPET(i)
      read(1,*) a,p,f
      ls=-1
      call DelBlanks(sPET(i),ls)
      call ToLower(sPET(i),ls)
! count number of parameters in the string
      np=nParInString(sPET(i),ls)
! replace all parameters by the constant value p
      call rswrit2(p,5_2,sch,l1s)
      so(1:ls)=sPET(i)(1:ls)
      do mp=1,np
        ic=iParInString(sPET(i),ls,mp) ! get its position and value
        lci=2
        if(sPET(i)(ic+1:ic+1).ne.'0') lci=1
        lc=ls+Int4(l1s)-lci
        if(lc.gt.1150) then
          Exit
        end if
        ls=lc
        so(ic:ic+Int4(l1s-1_2))=sch(1:l1s)
        so(ic+Int4(l1s):lc)=sPET(i)(ic+lci:ls)
      end do
      sPET(i)(1:ls)=so(1:ls)
    end do
    close(1)
    fPET=0.0d0
  end Subroutine openBasis

  Subroutine SaveMmp(lCheck)
! save MMP Matrix data in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Integer(4) iOk,ios,i,k,ik,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select MMP data file to be written!','MMP data file ',MmpFileName,'MMP',ios)
      if(ios.gt.0) return
      lMMPrMtr=.false.
      if(Allocated(MMPMtr).and.(iMtrSolver.gt.0).and.(iMtrSolver.ne.4)) then
        idum=MessageBoxQQ('Save rectangular MMP matrix?'C,'Save MMP data'C, &
                          MB$YesNo.or.MB$IconQuestion)
        if(idum.eq.MB$IDYES) lMMPrMtr=.true.
      end if
      lMMPtMtr=.false.
      if(Allocated(TriMtr).and.(iMtrSolver.ne.2).and.(iMtrSolver.ne.3)) then
        idum=MessageBoxQQ('Save trapezoidal MMP matrix?'C,'Save MMP data'C, &
                          MB$YesNo.or.MB$IconQuestion)
        if(idum.eq.MB$IDYES) lMMPtMtr=.true.
      end if
    end if
    open(1,file=MmpFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save MMP data'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHMmpIdent,iOK)
    ich(1)=nSegPt
    rch(1)=BndDMax
    rch(2)=BndPpW
    rch(3)=BndOver
    sch(1:11)=' nSegPt,...'
    call chwrit2(1,ich,1,rch,3,sch,11,iOK)
    ich(1)=iMtrSolver
    if(.not.lMMPuseRes) ich(1)=ich(1)+100
    ich(1)=ich(1)+1000*Int4(abs(iMMPfindExp))
    if(iMMPfindExp.lt.0) ich(1)=ich(1)+10000
    if(lMMP3F) ich(1)=ich(1)+1000000
    ich(2)=iAmplTyp
    ich(3)=iScaleMatrix
    ich(4)=iAmpl
    rch(1)=fMatrix0
    sch(1:15)=' iMtrSolver,...'
    call chwrit2(1,ich,4,rch,1,sch,15,iOK)
    ich(1)=itmCG
    ich(2)=iaccCG
    ich(3)=iMMPLast
    rch(1)=resCG
    sch(1:25)=' itmCG,iaccCG,iLast,resCG'
    call chwrit2(1,ich,3,rch,1,sch,25,iOK)
    ich(1)=0
    if(lMMPrough) ich(1)=1
    ich(2)=0
    if(lMMPfine) ich(2)=1
    sch(1:13)=' lRough,lFine'
    call chwrit2(1,ich,2,rch,0,sch,13,iOK)
    rch(1)=resPET
    if(lPET) then
      ich(1)=nPET-1
      sch(1:15)=' use PET,resPET'
      call chwrit2(1,ich,1,rch,1,sch,15,iOK)
    else
      ich(1)=-1
      sch(1:14)=' no PET,resPET'
      call chwrit2(1,ich,1,rch,1,sch,14,iOK)
    end if
    ich(1)=Int4(iMMPCon)
    sch(1:11)=' Connection'
    call chwrit2(1,ich,1,rch,0,sch,11,iOK)
    rch(1)=ErrorScale
    sch(1:14)=' error scaling'
    call chwrit2(1,ich,0,rch,1,sch,14,iOK)
    ich(1)=iFunWeight
    sch(1:25)=' Weight function argument'
    call chwrit2(1,ich,1,rch,0,sch,25,iOK)
    if(lMMPrMtr.and.Allocated(MMPMtr)) then
      ich(1)=nRow
      ich(2)=mCol
      sch(1:19)=' rectangular matrix'
      call chwrit2(1,ich,2,rch,0,sch,19,iOK)
      ik=0
      do i=1,nRow
        ich(1)=i
        call chwrit2(1,ich,1,rch,0,sch,0,iOK)
        do k=1,mCol
          ik=ik+1
          rch(1)=Dble(MMPMtr(ik))
          rch(2)=DImag(MMPMtr(ik))
          call chwrit2(1,ich,0,rch,2,sch,0,iOK)
        end do
      end do
    else
      ich(1)=0
      ich(2)=0
      sch(1:22)=' no rectangular matrix'
      call chwrit2(1,ich,2,rch,0,sch,22,iOK)
    end if
    if(lMMPtMtr.and.Allocated(TriMtr)) then
      ich(1)=mCol
      sch(1:18)=' trapezoidal matrix'
      call chwrit2(1,ich,1,rch,0,sch,18,iOK)
      do i=1,mTri
        rch(1)=Dble(TriMtr(i))
        rch(2)=DImag(TriMtr(i))
        call chwrit2(1,ich,0,rch,2,sch,0,iOK)
      end do
    else
      ich(1)=0
      sch(1:21)=' no trapezoidal matrix'
      call chwrit2(1,ich,1,rch,0,sch,21,iOK)
    end if
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveMmp

  Subroutine OpenMmp(lCheck)
! read MMP data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist
	  Integer(4) iOk,ios,i,k,ik,i1,k1,iVers,idum
    Character(20) text
    if(.not.lgcFld) lGet3DMat=.true.
    iBound=0_2
    if(.not.lCheck) then
      call Open2read(-1,'Select MMP data file to be read!','MMP data file ',MmpFileName,'MMP',ios)
      if(ios.gt.0) return
      lMMPrMtr=.true.
      lMMPtMtr=.true.
      idum=MessageBoxQQ('Skip MMP matrix data?'C,'Open MMP data'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDYES) then
        lMMPrMtr=.false.
        lMMPtMtr=.false.
      end if
    end if
    inquire(file=MmpFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=MmpFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open MMP data'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHMmpIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open MMP data'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call StrToInt(text(16:16)//text(18:18),iVers,iOK)
    call chread2(1,ich,1,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Matching point data)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nSegPt=ich(1)
    BndDMax=rch(1)
    BndPpW=rch(2)
    BndOver=rch(3)
    if(iVers.gt.29) then
      call chread2(1,ich,4,rch,1,iOK)
      iAmplTyp=ich(2)
      iAmpl=ich(4)
    else if(iVers.gt.20) then
      call chread2(1,ich,3,rch,1,iOK)
      if(ich(2).gt.-1) then
        iAmplTyp=1
        iAmpl=ich(4)
      else if(ich(2).eq.-1) then
        iAmplTyp=0
        iAmpl=0
      else
        iAmplTyp=2
        iAmpl=0
      end if
    else
      call chread2(1,ich,2,rch,0,iOK)
      if(ich(2).gt.-1) then
        iAmplTyp=1
        iAmpl=ich(4)
      else if(ich(2).eq.-1) then
        iAmplTyp=0
        iAmpl=0
      else
        iAmplTyp=2
        iAmpl=0
      end if
      ich(3)=2
      rch(1)=0.0d0
    end if
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Matrix solver type)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    lMMP3F=.false.
    if(ich(1).ge.1000000) then
      ich(1)=ich(1)-1000000
      lMMP3F=.true.
    end if
    if(ich(1).gt.500) then
      if(ich(1).gt.9999) then
        ich(1)=ich(1)-10000*Int4(ich(1)/10000)
        iMMPfindExp=Int2(ich(1)/1000)
        ich(1)=ich(1)-1000*Int4(iMMPfindExp)
        iMMPfindExp=-iMMPfindExp
      else
        iMMPfindExp=Int2(ich(1)/1000)
        ich(1)=ich(1)-1000*Int4(iMMPfindExp)
      end if
    else
      iMMPfindExp=0
    end if
    if(ich(1).gt.50) then
      lMMPuseRes=.false.
      iMtrSolver=max(0,min(5,ich(1)-100))
    else
      lMMPuseRes=.true.
      iMtrSolver=max(0,min(5,ich(1)))
    end if
    iScaleMatrix=ich(3)
    fMatrix0=rch(1)
    if((iMtrSolver.eq.0).or.(iMtrSolver.eq.4).or.(iMtrSolver.eq.5)) lMMPrMtr=.false.
    ich(3)=0
    if(iVers.gt.30) then
      call chread2(1,ich,3,rch,1,iOK)
    else if(iVers.gt.20) then
      call chread2(1,ich,2,rch,1,iOK)
    else
      call chread2(1,ich,1,rch,1,iOK)
      ich(2)=10
    end if
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(CG data)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    itmCG=ich(1)
    iaccCG=ich(2)
    iMMPLast=ich(3)
    resCG=rch(1)
    call chread2(1,ich,2,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Search data)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    lMMPrough=.false.
    if(ich(1).ne.0) lMMPrough=.true.
    lMMPfine=.false.
    if(ich(2).ne.0) lMMPfine=.true.
    if(iVers.gt.20) then
      call chread2(1,ich,1,rch,1,iOK)
    else
      call chread2(1,ich,1,rch,0,iOK)
      rch(1)=4.0d0
    end if
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PET data)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    resPET=rch(1)
    if(ich(1).gt.-1) then
      nPET=ich(1)+1
      lPET=.true.
    else
      lPET=.false.
    end if
    if(.not.lPET) kPET=0
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Connection data)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    iMMPCon=Int2(ich(1))
    call chread2(1,ich,0,rch,1,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Error scaling)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    ErrorScale=rch(1)
    if(text(16:16).ne.'1') then
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Weight data)'C,'Open MMP data'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      iFunWeight=ich(1)
    else
      iFunWeight=0_4
    end if
    call chread2(1,ich,2,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(R matrix size)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    if((ich(1).gt.0).and.(ich(2).gt.0)) then
      if(lMMPrMtr) then
        nRow=ich(1)
        mRow=nRow
        call AllocateMtr(ldum)
        if((.not.ldum).or.(mMtr.lt.1)) then
  	      close(1)
          return
        end if
        if(mCol.ne.ich(2)) then
          idum=MessageBoxQQ('Matrix size incompatible with current expansions!\r(R matrix data)'C,'Open MMP data'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
        end if
      end if
      i1=ich(1)
      k1=ich(2)
      ik=0
      do i=1,i1
        call chread2(1,ich,1,rch,0,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(R matrix data)'C,'Open MMP data'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        do k=1,k1
          call chread2(1,ich,0,rch,2,iOK)
          if(iOK.ne.0) then
            idum=MessageBoxQQ('Error reading input file!\r(R matrix data)'C,'Open MMP data'C, &
                              MB$OK.or.MB$ICONSTOP)
  	        close(1)
		        return
	        end if
          ik=ik+1
          if(lMMPrMtr) MMPMtr(ik)=DCmplx(rch(1),rch(2))
        end do
      end do
    end if
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(T matrix size)'C,'Open MMP data'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    if(lMMPtMtr.and.(ich(1).gt.0).and.(iMtrSolver.ne.5)) then
      call AllocateMtr(ldum)
      if((.not.ldum).or.(mMtr.lt.1)) then
  	    close(1)
        return
      end if
      if(mCol.ne.ich(1)) then
        idum=MessageBoxQQ('Matrix size incompatible with current expansions!\r(T matrix data)'C,'Open MMP data'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
        return
      end if
      do i=1,mTri
        call chread2(1,ich,0,rch,2,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(T matrix data)'C,'Open MMP data'C, &
                            MB$OK.or.MB$ICONSTOP)
  	  close(1)
	  return
	end if
       TriMtr(i)=DCmplx(rch(1),rch(2))
      end do
    end if
    close(1)
  end Subroutine OpenMmp

! MMP operations

  Subroutine getmCol()
! count columns of the MMP matrix
    Implicit none
    Integer(4) k
    mCol=0 ! count columns of the MMP matrix
    do k=1,nExp
      if(LSkipExp(k)) Cycle
      if(igetiHE2(iHEGlobal,k).lt.0_2) Cycle
      if(tExp(k)%iObj.lt.0) Cycle
      mCol=mCol+tExp(k)%nPar
    end do
    mColsMtr=mCol-nRHS+1 ! columns of the triangular matrix = unknown parameters
    mTri=((nRHS-1)*mColsMtr)+((mColsMtr+1)*mColsMtr)/2 ! size of the trapezoidal matrix
  end Subroutine getmCol

  Subroutine setupMtr(ldum)
! setup the rectangular matrix
    Implicit none
    Include 'resource.fd'
    Real(8) r(3),rmi,rma
    Integer(4) k,ik,idum,kPt,lout,iFw,i0,nBndCnd
    Logical ldum
    ldum=.false.
    call OutTxt('t1','Set MMP matrix'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    call cBndGetABO() ! get c-poly, splines, match.pts
    if((iBound.ne.2_2).and.(iBound.ne.4_2)) return
    if(lgcFld) then
      nBndPt3D=nBndPt
      nInhBndPt3D=0
      nBndEq3D=nBndEq
    else ! generate 3D matching points
      call get3DMatPts(1,nObj,0_4,1_2,.true.)
    end if
    r(1:3)=0.0d0
    call getmCol()
    if(mCol.lt.1) then
      idum=MessageBoxQQ('Cannot solve problem!\r(matrix has no rows!)'C,'MMP matrix setup'C, &
                        MB$OK.or.MB$ICONHAND)
      ldum=.false.
      return
    end if
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
    lMMPBndVal=.false.
    mRow=nBndEq3D-nInhEqu3D
    call AllocateMtr(ldum)
    if(.not.ldum) return ! insufficient memory
    ldum=.true.
    nRow=0
    if((iMtrSolver.eq.0).or.(iMtrSolver.eq.4)) then ! GUT,CHOL -> no memory for the rectangular MMP matrix allocated
      rMtr(1:mCol)=1.0d0
      return
    end if
    ik=0
    lDispWarn=.true.
    if(nBndPt3D.ne.nFun) iFunWeight=0_4
    iFw=abs(iFunWeight)
    if((iFw.lt.1).or.(iFw.gt.nFunA)) then
      rma=1.0d0
    else
      call MinMax(Fun,mFun,mFunA,nFun,iFw,rmi,rma)
      rma=abs(rma)
    end if
    nBndCnd=0
!!$OMP parallel do
    do kPt=1,nBndPt3D
      if(.not.lgcFld) then
        if(iObjBndPt3D(kPt).lt.0) Cycle
      end if
      if(nRow.ge.mRow) Cycle
      if(lStopThread) Cycle
      call IntToStr(kPt,0,0,SpaceText,lout)
	    call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nBndPt3D,0,0,SpaceText,lout)
	    call OutTxt('m1',SpaceText(1:lout))
      call BndCond(kPt,ik,rma,nBndCnd)
      if(nRow.ge.mRow) Cycle
    end do
    if(lStopThread) return
    nRow=nBndCnd
    if((lEigen.and.(mCol.gt.nBndCnd)).or.((.not.lEigen).and.(mCol.gt.(nBndCnd+1)))) then
      idum=MessageBoxQQ('Cannot solve problem!\r(matrix has not enough rows)'C,'MMP matrix setup'C, &
                        MB$OK.or.MB$ICONHAND)
      ldum=.false.
      return
    end if
    if(iMtrSolver.eq.5) then
      call chchkM2(QRMtr,mCol-nRHS,nRow,mRow,iabs(iScaleMatrix),rMtr)
      call chchkM2(QRRMtr,nRHS,nRow,mRow,iabs(iScaleMatrix),rMtr(mCol-nRHS+1:mCol))
    else
      call chchkM(MMPMtr,mMtr,mCol,nRow,iabs(iScaleMatrix),rMtr,i0)
    end if
    if(lStopThread) return
    ldum=.true.
    do k=1,mCol
      if((rMtr(k).gt.pBig).or.(rMtr(k).lt.pSmall)) then
        ldum=.false.
        call IntToStr(k,0,0,SpaceText,lout)
        idum=MessageBoxQQ('Cannot solve problem!\rMatrix column '//SpaceText(1:lout)//' is zero!&
                          \rContinue checking matrix?'C,'MMP matrix setup'C, &
                          MB$YESNO.or.MB$ICONQUESTION)
        if(idum.eq.MB$IDNO) return
        rMtr(k)=1.0d0
      end if
    end do
    if(.not.ldum) return
    if(lMMPstat.or.lMMPBndVal) rMtr(mCol-nRHS+1:mCol)=1.0d0
    if(iabs(iScaleMatrix).gt.0) then
      if(iMtrSolver.eq.5) then
        call chsclM2(mCol-nRHS,nRow,mRow,rMtr,QRMtr)
        call chsclM2(nRHS,nRow,mRow,rMtr(mCol-nRHS+1:mCol),QRRMtr)
      else
        call chsclM(mMtr,mCol,nRow,rMtr,MMPMtr)
      end if
    end if
    if(fMatrix0.gt.pSmall) then
      rmi=fMatrix0
      if(iMtrSolver.eq.5) then
        call ch000M2(mCol-nRHS,nRow,mRow,rmi,QRMtr) !! set small values = 0
        call ch000M2(nRHS,nRow,mRow,rmi,QRRMtr) !! set small values = 0
      else
        call ch000M(mMtr,mCol,nRow,rmi,MMPMtr) !! set small values = 0
      end if
    end if
    call OutTxt('t1','terminated'C)
  end Subroutine setupMtr

  Subroutine BndCond(kPt,ik,rma,nBndCnd)
! setup the regular boundary conditions in the matching point kPt
! update the trapezoidal matrix if ik<0
    Implicit none
    Include 'resource.fd'
    Complex(8), Allocatable:: ArrExp(:,:)
    Complex(8) cZw,Cp,cFldBnd(10),P,wEz,wEt,wEn,wHz,wHt,wHn,wAz,wAt,wAn,wV,cF
    Real(8) rma,et(3),ez(3),en(3),r(3),rp(3),sPt,wgt,xPt,yPt,xtPt,ytPt,row, &
    &       cVnorm,dmin,val(2),rNmin(3),w1,w2
    Integer(4) i,j,k,ik,i0,i1,i2,idum,kPBnd,kPt,ipos,nBndCnd,kExpFF,kR
    Integer(2) iD0,iCond,iDomL,iDomR,iL,iR,iColLoc,iConLoc,iDomLoc
    Logical lusual,lspec,lxper,lyper,lzper,lxperio,lyperio,lzperio,lsibc,lBC(10),lRSidemin
    External cVnorm
    if(lgcFld) then
      kBndPt=kPt
      w1=getWeight(kBndPt)
      w2=dsqrt(wBndPt(kBndPt))
    else
      kBndPt=iBndPt3D(kPt)
      w1=getWeight(kBndPt)
      w2=wBndPt3D(kPt)
    end if
    wgt=w1*w2
    if(tBnd(iBndPt(kBndPt))%nBC.lt.1) return
    if((iFunWeight.ne.0_4).and.(nFunA.gt.3)) then
      call FindFunPosition(4,sBndPt(kBndPt),ipos)
      ipos=min(max(ipos,1),nFun)
      if(iFunWeight.gt.0_4) then
        wgt=wgt*rma/abs(Fun(iFunWeight,ipos))
      else
        wgt=wgt*abs(Fun(-iFunWeight,ipos))/rma
      end if
    end if
    if(wgt.lt.pSmall) return
    sPt=sBndPt(kBndPt)
    iCond=tBnd(iBndPt(kBndPt))%iCond
    iBound=abs(iBound)
    if((iBound.ne.2_2).and.(iBound.ne.4_2)) call cBndGetABO() ! get c-poly, splines, match.pts
    if((iBound.ne.2_2).and.(iBound.ne.4_2)) return
    call GetBndPt(0,sPt,xPt,yPt,xtPt,ytPt,iDomL,iDomR,kPBnd)
    if((iDomL.gt.30000_2).or.(iDomL.lt.-30000_2)) return
    call getBndCnd(iCond,lusual,lspec,lxper,lyper,lzper,lsibc,lBC(1),lBC(2),lBC(3),lBC(4),lBC(5),lBC(6), &
    &              lBC(8),lBC(10))
    lBC(1:10)=tBnd(iBndPt(kBndPt))%lBC(1:10)
    if(lgcFld) then
      r(1)=xPt
      r(2)=yPt
      r(3)=0.0d0
      et(1)=xtPt
      et(2)=ytPt
      et(3)=0.0d0
      ez(1)=0.0d0
      ez(2)=0.0d0
      ez(3)=1.0d0
      en(1)=-ytPt
      en(2)=xtPt
      en(3)=0.0d0
    else
      r(1:3)=BndPt3D(1:3,0,kPt)
      et(1:3)=BndPt3D(1:3,1,kPt)
      ez(1:3)=BndPt3D(1:3,2,kPt)
      en(1:3)=BndPt3D(1:3,3,kPt)
    end if
    call Unit3DV(et)
    call Unit3DV(ez)
    call Unit3DV(en)
    FldMtr(1:10,1:mCol)=(0.0d0,0.0d0)
    iL=Max(1_2,Min(Int2(nDom),iDomL))
    iR=Max(1_2,Min(Int2(nDom),iDomR))
    if(iDomL.lt.0) iL=iR
    if(iDomR.lt.0) iR=iL
    call getWeights(iL,iR,wgt,wEz,wEt,wEn,wHz,wHt,wHn,wAz,wAt,wAn,wV)
    if((iDomL.lt.0).or.(iDomR.lt.0)) then ! impressed field on this boundary
      lMMPBndVal=.true.
      iColLoc=iColBnd
      iConLoc=iConBnd
      iDomLoc=iDomBnd
      iColBnd=Int2(kColFFD)
      iConBnd=Int2(kConFFD)
      iDomBnd=Int2(kDomFFD)
      call DistPtObj(0_2,0_2,r,.true.,dmin,rNmin,iD0,val,.true.,lRSidemin)
      if(iD0.lt.0) iD0=0
      iColBnd=iColLoc
      iConBnd=iConLoc
      iDomBnd=iDomLoc
      iColLoc=iColExp
      iConLoc=iConExp
      iDomLoc=iDomExp
      iColExp=Int2(kColFFD)
      iConExp=Int2(kConFFD)
      iDomExp=Int2(kDomFFD)
      cFldBnd=(0.0d0,0.0d0)
      if(lExpFFD) then
        kExpFF=kExpFFD
        if(lExcFFD) then
          if((kExpFFD.eq.nExp).or.(kExpFFD.eq.0).or.(kExpFFD.le.-nExp)) kExpFF=-(nExp-nExc)
        end if
        call GetFieldExp(kExpFF,0,r,iD0,iHEGlobal,idum)
        cFldBnd=FldExp
      else
        if((iEx.eq.0_2).and.(iEy.eq.0_2)) lBC(1:2)=.false.
        if(iEz.eq.0_2) lBC(3)=.false.
        if((iHx.eq.0_2).and.(iHy.eq.0_2)) lBC(4:5)=.false.
        if(iHz.eq.0_2) lBC(6)=.false.
        if((iAx.eq.0_2).and.(iAy.eq.0_2)) lBC(7:8)=.false.
        if(iAz.eq.0_2) lBC(9)=.false.
        if(iV.eq.0_2) lBC(10)=.false.
        call GetLoccField(r,iD0,3,cFldBnd)
      end if
      iColExp=iColLoc
      iConExp=iConLoc
      iDomExp=iDomLoc
    end if
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
    if(lusual.or.lspec) then ! ordinary boundary conditions
      if(iDomL.gt.0_2) then
        call GetArrExp(0,r,iL,iHEGlobal,ArrExp)
        i0=0
        do k=1,nExp
          if(LSkipExp(k)) Cycle
          if(igetiHE2(iHEGlobal,k).lt.0_2) Cycle
         !!!  if(.not.lgcFld) then ! 3D problem
            if(tExp(k)%iObj.lt.0) Cycle
         !!!  end if
          i1=tExp(k)%iOff+1
          i2=tExp(k)%iOff+tExp(k)%nPar
          do i=i1,i2
            i0=i0+1
            if((i0.gt.mCol).or.(i.gt.nPar)) then
              idum=MessageBoxQQ('Strange problem!\r(not enough parameters allocated)'C,'MMP boundary condition setup'C, &
                                MB$OK.or.MB$ICONHAND)
              Cycle
            end if
            P=(1.0d0,0.0d0)
            if((i0.ge.mCol-nRHS+1).and.(.not.lMMPStat).and.(.not.lEigen).and.(nRHS.gt.1)) then
              do kR=1,nRHS
                P=ParExp(kR,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+kR)
                if(cdabs(P).lt.pSmall) then
                  P=(1.0d0,0.0d0)
                  ParExp(kR,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+1:tExp(nExcit)%iOff+tExp(nExcit)%nPar)=(0.0d0,0.0d0)
                  ParExp(kR,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+kR)=P
                end if
              end do
              P=ParExp(kExc,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+kExc)
            end if
            if(lBC(1)) then
              FldMtr(1,i0)=FldMtr(1,i0)+P*wEt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
            end if
            if(lBC(2)) then
              FldMtr(2,i0)=FldMtr(2,i0)+P*wEz*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
            end if
            if(lBC(3)) then
              FldMtr(3,i0)=FldMtr(3,i0)+P*wEn*eDom(iL)*(ArrExp(1,i)*en(1)+ArrExp(2,i)*en(2)+ArrExp(3,i)*en(3))
              if(lBC(8)) FldMtr(3,i0)=FldMtr(3,i0)+tDom(iL)*P*wHn*uDom(iL)* & ! topological isolator
              &          (ArrExp(4,i)*en(1)+ArrExp(5,i)*en(2)+ArrExp(6,i)*en(3))
            end if
            if(lBC(4)) then
              FldMtr(4,i0)=FldMtr(4,i0)+P*wHt*(ArrExp(4,i)*et(1)+ArrExp(5,i)*et(2)+ArrExp(6,i)*et(3))
              if(lBC(8)) FldMtr(4,i0)=FldMtr(4,i0)-tDom(iL)*P*wEt* & ! topological isolator
              &          (ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
              if(lBC(10)) FldMtr(4,i0)=FldMtr(4,i0) &
              & +sDom(iL)*P*wHt*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3)) & ! electric surface current
              & +tDom(iL)*P*wHt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3)) ! electric surface current (anisotropic part)
            end if
            if(lBC(5)) then
              FldMtr(5,i0)=FldMtr(5,i0)+P*wHz*(ArrExp(4,i)*ez(1)+ArrExp(5,i)*ez(2)+ArrExp(6,i)*ez(3))
              if(lBC(8)) FldMtr(5,i0)=FldMtr(5,i0)-tDom(iL)*P*wEz* & ! topological isolator
              &          (ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
              if(lBC(10)) FldMtr(5,i0)=FldMtr(5,i0) &
              & -sDom(iL)*P*wHz*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3)) & ! electric surface current
              & +tDom(iL)*P*wHz*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3)) ! electric surface current (anisotropic part)
            end if
            if(lBC(6)) FldMtr(6,i0)=FldMtr(6,i0)+P*wHn*uDom(iL)*(ArrExp(4,i)*en(1)+ArrExp(5,i)*en(2)+ArrExp(6,i)*en(3))
            if(lBC(7)) FldMtr(7,i0)=FldMtr(7,i0)+P*wAt*(ArrExp(7,i)*et(1)+ArrExp(8,i)*et(2)+ArrExp(9,i)*et(3))
            if(lBC(8)) FldMtr(8,i0)=FldMtr(8,i0)+P*wAz*(ArrExp(7,i)*ez(1)+ArrExp(8,i)*ez(2)+ArrExp(9,i)*ez(3))
            if(lBC(9)) FldMtr(9,i0)=FldMtr(9,i0)+P*wAn*(ArrExp(7,i)*en(1)+ArrExp(8,i)*en(2)+ArrExp(9,i)*en(3))
            if(lBC(10)) FldMtr(10,i0)=FldMtr(10,i0)+P*wV*ArrExp(10,i)
          end do
        end do
      end if
      if(iDomR.gt.0_2) then
        call GetArrExp(0,r,iR,iHEGlobal,ArrExp)
        i0=0
        do k=1,nExp
          if(LSkipExp(k)) Cycle
          if(igetiHE2(iHEGlobal,k).lt.0_2) Cycle
          if(tExp(k)%iObj.lt.0) Cycle
          i1=tExp(k)%iOff+1
          i2=tExp(k)%iOff+tExp(k)%nPar
          do i=i1,i2
            i0=i0+1
            if((i0.gt.mCol).or.(i.gt.nPar)) then
              idum=MessageBoxQQ('Strange problem!\r(not enough parameters allocated)'C,'MMP boundary condition setup'C, &
                                MB$OK.or.MB$ICONHAND)
              Cycle
            end if
            P=(1.0d0,0.0d0)
            if((i0.ge.mCol-nRHS+1).and.(.not.lMMPStat).and.(.not.lEigen).and.(nRHS.gt.1)) then
              do kR=1,nRHS
                P=ParExp(kR,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+kR)
                if(cdabs(P).lt.pSmall) then
                  P=(1.0d0,0.0d0)
                  ParExp(kR,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+1:tExp(nExcit)%iOff+tExp(nExcit)%nPar)=(0.0d0,0.0d0)
                  ParExp(kR,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+kR)=P
                end if
              end do
              P=ParExp(kExc,tExp(nExcit)%iOff+tExp(nExcit)%nPar-nRHS+kExc)
            end if
            if(lBC(1)) then
              FldMtr(1,i0)=FldMtr(1,i0)-P*wEt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
            end if
            if(lBC(2)) then
              FldMtr(2,i0)=FldMtr(2,i0)-P*wEz*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
            end if
            if(lBC(3)) then
              FldMtr(3,i0)=FldMtr(3,i0)-P*wEn*eDom(iR)*(ArrExp(1,i)*en(1)+ArrExp(2,i)*en(2)+ArrExp(3,i)*en(3))
              if(lBC(8)) FldMtr(3,i0)=FldMtr(3,i0)-tDom(iR)*P*wHn*uDom(iR)* & ! topological isolator
              &          (ArrExp(4,i)*en(1)+ArrExp(5,i)*en(2)+ArrExp(6,i)*en(3))
            end if
            if(lBC(4)) then
              FldMtr(4,i0)=FldMtr(4,i0)-P*wHt*(ArrExp(4,i)*et(1)+ArrExp(5,i)*et(2)+ArrExp(6,i)*et(3))
              if(lBC(8)) FldMtr(4,i0)=FldMtr(4,i0)+tDom(iR)*P*wEt* & ! topological isolator
              &          (ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
              if(lBC(10)) FldMtr(4,i0)=FldMtr(4,i0) &
              & -sDom(iR)*P*wHt*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3)) & ! electric surface current
              & -tDom(iR)*P*wHt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3)) ! electric surface current (anisotropic part)
            end if
            if(lBC(5)) then
              FldMtr(5,i0)=FldMtr(5,i0)-P*wHz*(ArrExp(4,i)*ez(1)+ArrExp(5,i)*ez(2)+ArrExp(6,i)*ez(3))
              if(lBC(8)) FldMtr(5,i0)=FldMtr(5,i0)+tDom(iR)*P*wEz* & ! topological isolator
              &          (ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
              if(lBC(10)) FldMtr(5,i0)=FldMtr(5,i0) &
              & +sDom(iR)*P*wHz*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3)) & ! electric surface current
              & -tDom(iR)*P*wHz*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3)) ! electric surface current (anisotropic part)
            end if
            if(lBC(6)) FldMtr(6,i0)=FldMtr(6,i0)-P*wHn*uDom(iR)*(ArrExp(4,i)*en(1)+ArrExp(5,i)*en(2)+ArrExp(6,i)*en(3))
            if(lBC(7)) FldMtr(7,i0)=FldMtr(7,i0)-P*wAt*(ArrExp(7,i)*et(1)+ArrExp(8,i)*et(2)+ArrExp(9,i)*et(3))
            if(lBC(8)) FldMtr(8,i0)=FldMtr(8,i0)-P*wAz*(ArrExp(7,i)*ez(1)+ArrExp(8,i)*ez(2)+ArrExp(9,i)*ez(3))
            if(lBC(9)) FldMtr(9,i0)=FldMtr(9,i0)-P*wAn*(ArrExp(7,i)*en(1)+ArrExp(8,i)*en(2)+ArrExp(9,i)*en(3))
            if(lBC(10)) FldMtr(10,i0)=FldMtr(10,i0)-P*wV*ArrExp(10,i)
          end do
        end do
      end if
      if((iDomL.lt.0).or.(iDomR.lt.0)) then
        if(lBC(1))  FldMtr(1,mCol) =FldMtr(1,mCol) +wEt*(cFldBnd(1)*et(1)+cFldBnd(2)*et(2)+cFldBnd(3)*et(3))
        if(lBC(2))  FldMtr(2,mCol) =FldMtr(2,mCol) +wEz*(cFldBnd(1)*ez(1)+cFldBnd(2)*ez(2)+cFldBnd(3)*ez(3))
        if(lBC(3))  FldMtr(3,mCol) =FldMtr(3,mCol) +wEn*(cFldBnd(1)*en(1)+cFldBnd(2)*en(2)+cFldBnd(3)*en(3))
        if(lBC(4))  FldMtr(4,mCol) =FldMtr(4,mCol) +wHt*(cFldBnd(4)*et(1)+cFldBnd(5)*et(2)+cFldBnd(6)*et(3))
        if(lBC(5))  FldMtr(5,mCol) =FldMtr(5,mCol) +wHz*(cFldBnd(4)*ez(1)+cFldBnd(5)*ez(2)+cFldBnd(6)*ez(3))
        if(lBC(6))  FldMtr(6,mCol) =FldMtr(6,mCol) +wHn*(cFldBnd(4)*en(1)+cFldBnd(5)*en(2)+cFldBnd(6)*en(3))
        if(lBC(7))  FldMtr(7,mCol) =FldMtr(7,mCol) +wAt*(cFldBnd(7)*et(1)+cFldBnd(8)*et(2)+cFldBnd(9)*et(3))
        if(lBC(8))  FldMtr(8,mCol) =FldMtr(8,mCol) +wAz*(cFldBnd(7)*ez(1)+cFldBnd(8)*ez(2)+cFldBnd(9)*ez(3))
        if(lBC(9))  FldMtr(9,mCol) =FldMtr(9,mCol) +wAn*(cFldBnd(7)*en(1)+cFldBnd(8)*en(2)+cFldBnd(9)*en(3))
        if(lBC(10)) FldMtr(10,mCol)=FldMtr(10,mCol)+wV *cFldBnd(10)
      else if(iDomL.lt.1_2) then
        if(lBC(8).and.(tBnd(kPBnd)%val(2).le.pBig)) FldMtr(8,mCol)=wAz*dCmplx(tBnd(kPBnd)%val(2),0.d0)
        if(lBC(10).and.(tBnd(kPBnd)%val(1).le.pBig)) FldMtr(10,mCol)=wV*dCmplx(tBnd(kPBnd)%val(1),0.d0)
      else if(iDomR.lt.1_2) then
        if(lBC(8).and.(tBnd(kPBnd)%val(2).le.pBig)) FldMtr(8,mCol)=-wAz*dCmplx(tBnd(kPBnd)%val(2),0.d0)
        if(lBC(10).and.(tBnd(kPBnd)%val(1).le.pBig)) FldMtr(10,mCol)=-wV*dCmplx(tBnd(kPBnd)%val(1),0.d0)
      end if
    else if(lxper.or.lyper.or.lzper) then ! periodic boundary conditions
      rp(1:3)=r(1:3)
      if(lxper) then
        rp(1)=rp(1)-Dabs(xPeriod)
        Cp=cdexp((0.0d0,1.0d0)*cxPeriod*xPeriod)
      else if(lyper) then
        rp(1:3)=rp(1:3)-yPeriodVector(1:3)
        Cp=cdexp((0.0d0,1.0d0)*cyPeriod*r3Vec_Length(yPeriodVector))
      else
        rp(1:3)=rp(1:3)-zPeriodVector(1:3)
        Cp=cdexp((0.0d0,1.0d0)*czPeriod*r3Vec_Length(zPeriodVector))
      end if
      lxPerio=lxPeriod
      lyPerio=lyPeriod
      lzPerio=lzPeriod
      lxPeriod=.false.
      lyPeriod=.false.
      lzPeriod=.false.
      call GetArrExp(0,r,iL,iHEGlobal,ArrExp)
      i0=0
      do k=1,nExp
        if(LSkipExp(k)) Cycle
        if(igetiHE2(iHEGlobal,k).lt.0_2) Cycle
       !!!  if(.not.lgcFld) then ! 3D problem
          if(tExp(k)%iObj.lt.0) Cycle
       !!!  end if
        i1=tExp(k)%iOff+1
        i2=tExp(k)%iOff+tExp(k)%nPar
        do i=i1,i2
          i0=i0+1
          if((i0.gt.mCol).or.(i.gt.nPar)) then
            idum=MessageBoxQQ('Strange problem!\r(not enough parameters allocated)'C,'MMP boundary condition setup'C, &
                              MB$OK.or.MB$ICONHAND)
            Cycle
          end if
          if(lBC(1)) FldMtr(1,i0)=FldMtr(1,i0)+wEt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
          if(lBC(2)) FldMtr(2,i0)=FldMtr(2,i0)+wEz*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
          if(lBC(3)) FldMtr(3,i0)=FldMtr(3,i0)+wEn*eDom(iL)*(ArrExp(1,i)*en(1)+ArrExp(2,i)*en(2)+ArrExp(3,i)*en(3))
          if(lBC(4)) FldMtr(4,i0)=FldMtr(4,i0)+wHt*(ArrExp(4,i)*et(1)+ArrExp(5,i)*et(2)+ArrExp(6,i)*et(3))
          if(lBC(5)) FldMtr(5,i0)=FldMtr(5,i0)+wHz*(ArrExp(4,i)*ez(1)+ArrExp(5,i)*ez(2)+ArrExp(6,i)*ez(3))
          if(lBC(6)) FldMtr(6,i0)=FldMtr(6,i0)+wHn*uDom(iL)*(ArrExp(4,i)*en(1)+ArrExp(5,i)*en(2)+ArrExp(6,i)*en(3))
          if(lBC(7)) FldMtr(7,i0)=FldMtr(7,i0)+wAt*(ArrExp(7,i)*et(1)+ArrExp(8,i)*et(2)+ArrExp(9,i)*et(3))
          if(lBC(8)) FldMtr(8,i0)=FldMtr(8,i0)+wAz*(ArrExp(7,i)*ez(1)+ArrExp(8,i)*ez(2)+ArrExp(9,i)*ez(3))
          if(lBC(9)) FldMtr(9,i0)=FldMtr(9,i0)+wAn*(ArrExp(7,i)*en(1)+ArrExp(8,i)*en(2)+ArrExp(9,i)*en(3))
          if(lBC(10)) FldMtr(10,i0)=FldMtr(10,i0)+wV*ArrExp(10,i)
        end do
      end do
      call GetArrExp(0,rp,iR,iHEGlobal,ArrExp)
      i0=0
      do k=1,nExp
        if(LSkipExp(k)) Cycle
        if(igetiHE2(iHEGlobal,k).lt.0_2) Cycle
       !!!  if(.not.lgcFld) then ! 3D problem
          if(tExp(k)%iObj.lt.0) Cycle
       !!!  end if
        i1=tExp(k)%iOff+1
        i2=tExp(k)%iOff+tExp(k)%nPar
        do i=i1,i2
          i0=i0+1
          if((i0.gt.mCol).or.(i.gt.nPar)) then
            idum=MessageBoxQQ('Strange problem!\r(not enough parameters allocated)'C,'MMP boundary condition setup'C, &
                              MB$OK.or.MB$ICONHAND)
            Cycle
          end if
          if(lBC(1)) FldMtr(1,i0)=FldMtr(1,i0)-Cp*wEt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
          if(lBC(2)) FldMtr(2,i0)=FldMtr(2,i0)-Cp*wEz*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
          if(lBC(3)) FldMtr(3,i0)=FldMtr(3,i0)-Cp*wEn*eDom(iR)*(ArrExp(1,i)*en(1)+ArrExp(2,i)*en(2)+ArrExp(3,i)*en(3))
          if(lBC(4)) FldMtr(4,i0)=FldMtr(4,i0)-Cp*wHt*(ArrExp(4,i)*et(1)+ArrExp(5,i)*et(2)+ArrExp(6,i)*et(3))
          if(lBC(5)) FldMtr(5,i0)=FldMtr(5,i0)-Cp*wHz*(ArrExp(4,i)*ez(1)+ArrExp(5,i)*ez(2)+ArrExp(6,i)*ez(3))
          if(lBC(6)) FldMtr(6,i0)=FldMtr(6,i0)-Cp*wHn*uDom(iR)*(ArrExp(4,i)*en(1)+ArrExp(5,i)*en(2)+ArrExp(6,i)*en(3))
          if(lBC(7)) FldMtr(7,i0)=FldMtr(7,i0)-Cp*wAt*(ArrExp(7,i)*et(1)+ArrExp(8,i)*et(2)+ArrExp(9,i)*et(3))
          if(lBC(8)) FldMtr(8,i0)=FldMtr(8,i0)-Cp*wAz*(ArrExp(7,i)*ez(1)+ArrExp(8,i)*ez(2)+ArrExp(9,i)*ez(3))
          if(lBC(9)) FldMtr(9,i0)=FldMtr(9,i0)-Cp*wAn*(ArrExp(7,i)*en(1)+ArrExp(8,i)*en(2)+ArrExp(9,i)*en(3))
          if(lBC(10)) FldMtr(10,i0)=FldMtr(10,i0)-Cp*wV*ArrExp(10,i)
        end do
      end do
      lxPeriod=lxPerio
      lyPeriod=lyPerio
      lzPeriod=lzPerio
    else if(lsibc) then ! surface impedance boundary conditions
      cZw=Zw0*cdsqrt(uDom(iL)/eDom(iL))
      call GetArrExp(0,r,iR,iHEGlobal,ArrExp)
      i0=0
      do k=1,nExp
        if(LSkipExp(k)) Cycle
        if(igetiHE2(iHEGlobal,k).lt.0_2) Cycle
       !!!  if(.not.lgcFld) then ! 3D problem
          if(tExp(k)%iObj.lt.0) Cycle
       !!!  end if
        i1=tExp(k)%iOff+1
        i2=tExp(k)%iOff+tExp(k)%nPar
        do i=i1,i2
          i0=i0+1
          if((i0.gt.mCol).or.(i.gt.nPar)) then
            idum=MessageBoxQQ('Strange problem!\r(not enough parameters allocated)'C,'MMP boundary condition setup'C, &
                              MB$OK.or.MB$ICONHAND)
            Cycle
          end if
          FldMtr(1,i0)=FldMtr(1,i0)+wgt*(ArrExp(1,i)*et(1)+ArrExp(2,i)*et(2)+ArrExp(3,i)*et(3))
          FldMtr(2,i0)=FldMtr(2,i0)+wgt*(ArrExp(1,i)*ez(1)+ArrExp(2,i)*ez(2)+ArrExp(3,i)*ez(3))
          FldMtr(2,i0)=FldMtr(2,i0)-cZw*wgt*(ArrExp(4,i)*et(1)+ArrExp(5,i)*et(2)+ArrExp(6,i)*et(3))
          FldMtr(1,i0)=FldMtr(1,i0)+cZw*wgt*(ArrExp(4,i)*ez(1)+ArrExp(5,i)*ez(2)+ArrExp(6,i)*ez(3))
        end do
      end do
    end if
    if(Allocated(ArrExp)) DeAllocate(ArrExp)
    do j=1,10
      if(.not.lBC(j)) Cycle
      row=DSqrt(cVnorm(FldMtr(j,1:mCol),mCol))
      if(row.gt.pSmall) then
        if(nRow.ge.mRow) then
          idum=MessageBoxQQ('Strange problem!\r(Too many equations)'C,'MMP boundary condition setup'C, &
                            MB$OK.or.MB$ICONHAND)
          return
        end if
        nRow=nRow+1
	      if((iMMPlast.gt.0).and.(iMMPlast.lt.mCol)) then ! exchange columns
	        cF=FldMtr(j,mCol-iMMPlast)
	        FldMtr(j,mCol-iMMPlast)=FldMtr(j,mCol)
	        FldMtr(j,mCol)=cF
	      end if
        if(ik.ge.0) then
          if(iMtrSolver.eq.5) then
            ik=ik+1
            if(ik.gt.nRow) then
              idum=MessageBoxQQ('Strange problem!\r(QR matrix too small)'C,'MMP boundary condition setup'C, &
                                MB$OK.or.MB$ICONHAND)
              return
            end if
            QRMtr(ik,1:mCol-nRHS)=FldMtr(j,1:mCol-nRHS)
            QRRMtr(ik,1:nRHS)=-FldMtr(j,mCol-nRHS+1:mCol)
          else
            do k=1,mCol
              ik=ik+1
              if(ik.gt.mMtr) then
                idum=MessageBoxQQ('Strange problem!\r(MMP matrix too small)'C,'MMP boundary condition setup'C, &
                                  MB$OK.or.MB$ICONHAND)
                return
              end if
              MMPMtr(ik)=FldMtr(j,k)
            end do
          end if
          nBndCnd=nBndCnd+1
        else
          xMtr(1:mCol)=FldMtr(j,1:mCol)
          iupTri=iupTri+1
          if(iMtrSolver.ne.4) then
            call cchud(TriMtr,xMtr,mCol-nRHS,nRHS,iupTri,cMtr,sMtr,mTri)
          else
            call cpngad(TriMtr,xMtr,mCol,mTri)
          end if
          nBndCnd=nBndCnd+1
        end if
      end if
    end do
  end Subroutine BndCond

  Subroutine getWeights(iL,iR,wgt,wEz,wEt,wEn,wHz,wHt,wHn,wAz,wAt,wAn,wV)
    Implicit none
    Integer(2) iL,iR
    Real(8) wgt
    Complex(8) wEz,wEt,wEn,wHz,wHt,wHn,wAz,wAt,wAn,wV
    wEz=DCmplx(wgt,0.0d0)
    wEt=wEz
    wEn=wEz/sqrt(eDom(iL)*eDom(iR))
   ! wHz=wEz*Zw0*sqrt(sqrt(uDom(iL)*uDom(iR))/sqrt(eDom(iL)*eDom(iR)))
    wHz=wEz*Zw0*sqrt(sqrt(uDom(iL)/eDom(iL))*sqrt(uDom(iR)/eDom(iR)))
    wHt=wHz
    wHn=wHz/sqrt(uDom(iL)*uDom(iR))
    wAz=wEz
    wAt=wEz
    wAn=wEz
    wV=wEz
  end Subroutine getWeights

  real(8) Function getWeight(kPt)
    Implicit none
    Integer(4) kPt,iB
    Real(8) w1,w2,a,b,d,x,w
    iB=Int4(iBndPt(kPt))
    w1=tBnd(iB)%Weight
    w2=tBnd(iB)%Weight2
    x=sBndPt(kPt)-tBnd(iB)%Start
    d=tBnd(iB)%sLength
    if(w2.ge.0.0d0) then
      a=w1
      b=(w2-w1)/d
      w=a+b*x ! linear distribution
    else
      a=w1
      b=(-w2-w1)/(d**2)
      w=a+b*(x**2) ! quadratic distribution
    end if
    getWeight=w
  endFunction getWeight

  Subroutine updateMtr()
! update the trapezoidal matrix
    Implicit none
    Include 'resource.fd'
    Integer(4) i,ik,kPt,iFw,info,nBndCnd,ierr,iWork,idum,lout
    Real(8) rmi,rma
	  call OutTxt('t1','Updating'C)
	  call OutTxt('n1',' 'C)
	  call OutTxt('m1',' 'C)
    if(iMtrSolver.eq.1) then ! GUR -> update only
      ik=1
      do i=1,nRow
        call IntToStr(i,0,0,SpaceText,lout)
	      call OutTxt('n1',SpaceText(1:lout))
        call IntToStr(nRow,0,0,SpaceText,lout)
	      call OutTxt('m1',SpaceText(1:lout))
        iupTri=iupTri+1
        call cchud(TriMtr,MMPMtr(ik),mCol-nRHS,nRHS,iupTri,cMtr,sMtr,mTri)
        ik=ik+mCol
        if(lStopThread) return
      end do
    else if(iMtrSolver.eq.5) then ! QR -> solve (QRMtr contains rectangular matrix, QRRMtr contains right hand side)
      if(iWorkspace.gt.0) then
	      iWork=iWorkSpace*mRow
      else
        call ZGELS('N',nRow,mCol-nRHS,nRHS,QRMtr,mRow,QRRMtr,mRow,QRWMtr,-1,ierr)
        iWork=nint(Dble(QRWMtr(1)),4)
        if(l4.and.(iWorkspace.eq.0)) write(*,*) 'ZGELS iWork,iWork/nRow,iWork/mRow=',iWork,iWork/nRow,iWork/mRow
        DeAllocate(QRWMtr,stat=ierr)
        if(ierr.eq.0) Allocate(QRWMtr(iWork),stat=ierr)
        if(ierr.ne.0) then
          idum=MessageBoxQQ('Memory allocation for QRMtr failed!'C,'Allocate MMP-Rect matrices'C, &
                            MB$OK.or.MB$IconExclamation)
          if(Allocated(QRMtr)) DeAllocate(QRMtr)
          if(Allocated(QRRMtr)) DeAllocate(QRRMtr)
          if(Allocated(QRWMtr)) DeAllocate(QRWMtr)
          iMtrSolver=0
          return
        end if
      end if
      call ZGELS('N',nRow,mCol-nRHS,nRHS,QRMtr,mRow,QRRMtr,mRow,QRWMtr,iWork,ierr)
      if(ierr.ne.0) then
        idum=MessageBoxQQ('QR decomposition failed'C,'MMP matrix setup'C,MB$OK.or.MB$ICONHAND)
        return
      end if
    else if((iMtrSolver.eq.0).or.(iMtrSolver.eq.4)) then ! GUT,CHOL -> compute and update
      ik=-1
      lDispWarn=.true.
      if(lgcFld) then
        nBndPt3D=nBndPt
        nInhBndPt3D=0
      end if
      if(nBndPt3D.ne.nFun) iFunWeight=0_4
      iFw=abs(iFunWeight)
      if((iFw.lt.1).or.(iFw.gt.nFunA)) then
        rma=1.0d0
      else
        call MinMax(Fun,mFun,mFunA,nFun,iFw,rmi,rma)
        rma=abs(rma)
      end if
      nBndCnd=0
!!$OMP parallel do
      do kPt=1,nBndPt3D
        if(lStopThread) Cycle
        if(.not.lgcFld) then
          if(iObjBndPt3D(kPt).lt.0) Cycle
        end if
        call IntToStr(kPt,0,0,SpaceText,lout)
	      call OutTxt('n1',SpaceText(1:lout))
        call IntToStr(nBndPt3D,0,0,SpaceText,lout)
	      call OutTxt('m1',SpaceText(1:lout))
        call BndCond(kPt,ik,rma,nBndCnd)
      end do
      if(lStopThread) return
      nRow=nBndCnd
      if((lEigen.and.(mCol.gt.nBndCnd)).or.((.not.lEigen).and.(mCol.gt.(nBndCnd+1)))) then
        idum=MessageBoxQQ('Cannot solve problem!\r(matrix has not enough rows)'C,'MMP matrix setup'C, &
                          MB$OK.or.MB$ICONHAND)
        return
      end if
      if(iMtrSolver.eq.4) then
        call cdppfa(TriMtr,mCol,info,mTri)
        if(l4.and.(info.ne.0)) write(*,*) 'Error in Cholesky factorization, info=',info
      end if
    end if
    call OutTxt('t1','Updating done'C)
  end Subroutine updateMtr

  Subroutine solveMtr()
! solve the matrix equation
    Implicit none
    Include 'resource.fd'
    Real(8) sr2,res2max,q
    Real(8), external:: dznrm2
    Integer(4) idum,i1,i2,k,kE,kP,nord,nfun,kTri,kCol,i,iExc0,kEc
    Complex(8) Det,cf(10),cV(3)
    Complex(8), Allocatable:: cP(:)
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    call OutTxt('t1','Solve MMP matrix'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    cMMPeigenPhase=(1.0d0,0.0d0)
    cMMPeigenVector(1:3)=(1.0d0,0.0d0)
    if((iMtrSolver.eq.2).or.(iMtrSolver.eq.3)) then ! CG,PET -> iterate and solve
      res2max=abs(resCG)*(rMtr(mCol-nRHS+kExc)**2)
      if(lPET.and.(.not.lEigen)) then ! -> extrapolate
        if(kPET.gt.0) then
          nord=kPET-1
          nfun=mCol
          do kP=1,kPET
            xPET(kP)=Dble(kP-1)/Dble(kPET)
          end do
          wPET(1:kPET)=1.0d0
          call crExtra(mPET,npPET,kPET,nfun,nord,xPET,pPET,wPET,sPET,1.0d0,sMtr,idum)
        end if
      else if(iScaleMatrix.gt.0) then
        sMtr(1:mColsMtr,kExc)=(0.0d0,0.0d0)
      end if
      call chCG(MMPMtr,mMtr,mCol,nRow,itmCG,iaccCG,res2max,sMtr,CGsi,CGpi,CGri,CGqi,sr2,itCG,idum)
      fPET=fPET+Dble(itCG)
      if(lPET.and.(.not.lEigen)) then ! -> save new PET parameters
        kPET=min(kPET+1,nPET)
        if(nPET.gt.1) pPET(1:mCol,1:nPET-1)=pPET(1:mCol,2:nPET)
        pPET(1:mCol,kPET)=sMtr(1:mCol,kExc)
      end if
      resUnscaled=dabs(sr2)/(rMtr(mCol-nRHS+kExc)**2)
      if(lPET.and.(.not.lEigen).and.(sr2.gt.res2max)) then ! missing accuracy -> restart MMP-PET
        kPET=0
        fPET=fPET+2.0d0*Dble(itmCG)
      end if
    else if(iMtrSolver.eq.5) then ! QR -> system is already solved (result in QRRMtr)
      sMtr(1:mCol-nRHS,1:nRHS)=QRRMtr(1:mCol-nRHS,1:nRHS)
      sMtr(mColsMtr,1:nRHS)=1.0d0
      resUnscaled=DZNRM2(nRow-mCol+1,QRRMtr(mCol:nRow,kExc),1)
      resUnscaled=dabs(resUnscaled/rMtr(mCol-nRHS+kExc))**2
      if(lPET.and.(.not.lEigen)) then ! -> save first PET parameter set
        if(resCG.gt.nSmall) resCG=resPET*resUnscaled
        kPET=max(1,min(kPET+1,nPET))
        if(nPET.gt.1) pPET(1:mCol,1:nPET-1)=pPET(1:mCol,2:nPET)
        pPET(1:mCol,kPET)=sMtr(1:mCol,kExc)
      end if
    else ! GUT+GUR+CHOL -> solve trapezoidal matrix (CHOL: only 1 excitation or right hand side)
      call cchsl(TriMtr,sMtr,mCol-nRHS,nRHS,mCol,mTri)
      if(iMtrSolver.eq.1) then
        resUnscaled=cdabs(TriMtr(mTri)/rMtr(mCol-nRHS+kExc))**2
      else
        if(lMMPDeterminant) then
          det=1.0d0
          kTri=0
          do kCol=1,mCol
            kTri=kTri+kCol
            det=det*TriMtr(kTri)
          end do
          resUnscaled=cdabs(det)**2
        else
          resUnscaled=cdabs(TriMtr(mTri))**2
        end if
      end if
      if(lPET.and.(.not.lEigen)) then ! -> save first PET parameter set
        if(resCG.gt.nSmall) resCG=resPET*resUnscaled
        kPET=max(1,min(kPET+1,nPET))
        if(nPET.gt.1) pPET(1:mCol,1:nPET-1)=pPET(1:mCol,2:nPET)
        pPET(1:mCol,kPET)=sMtr(1:mCol,kExc)
      end if
    end if
    k=0 ! unscale parameters
    iExc0=0
    do kE=1,nExp
      if(LSkipExp(kE)) Cycle
      if(igetiHE2(iHEGlobal,kE).lt.0_2) Cycle
      if(tExp(kE)%iObj.lt.0) Cycle
      i1=tExp(kE)%iOff+1
      i2=tExp(kE)%iOff+tExp(kE)%nPar
      do i=i1,i2
        k=k+1
        if(k.ge.mColsMtr) then
          if(k.gt.mCol) Exit
          iExc0=i
          if(.not.(lMMPstat.or.lMMPBndVal)) then
            ParExp(1:nRHS,i)=(0.0d0,0.0d0)
            ParExp(k-mColsMtr+1,iExc0)=(1.0d0,0.0d0)
            iExc0=iExc0+1
          end if
          Exit
        end if
        if((iMtrSolver.gt.0).and.(iMtrSolver.ne.4)) then
          do kEc=1,nRHS
            ParExp(kEc,i)=sMtr(k,kEc)*(rMtr(k)/rMtr(mCol-nRHS+kEc))
          end do
        else
          ParExp(1:nRHS,i)=sMtr(k,1:nRHS)
        end if
      end do
    end do
    if((iMMPlast.gt.0).and.(iMMPlast.lt.mCol)) then ! exchange columns
      Allocate(cP(nRHS),stat=i1)
      if(i1.eq.0) then
        idum=tExp(nExp)%iOff+tExp(nExp)%nPar
        cP(1:nRHS)=ParExp(1:nRHS,idum-iMMPlast)
        ParExp(1:nRHS,idum-iMMPlast)=ParExp(1:nRHS,idum)
        ParExp(1:nRHS,idum)=cP(1:nRHS)
        DeAllocate(cP)
      end if
    end if
    lAskQ=.false.
    if(iAmplTyp.eq.1) then ! compute amplitude and scale results
      iBndInt=max(1,min(iabs(iAmpl),nBnd))
      IntInter=0
      if(lThreadStarted) then
        lIntgSave=.false.
        call BndIntg(.true.)
      else
        call TBndInt(.true.)
      end if
    else if(iAmplTyp.eq.2) then
      IntInter=0
      if(lThreadStarted) then
        call RecIntg(.true.)
      else
        call TRecInt(.true.)
      end if
    else if(iAmplTyp.eq.3) then
      iObjInt=max(1,min(iabs(iAmpl),nObj))
      IntInter=0
      iIntgEc=0_2
      iIntgHc=0_2
      if(lThreadStarted) then
        call ObjIntg(.true.)
      else
        call TObjInt(.true.)
      end if
    else if(iAmplTyp.eq.4) then ! get Field in the origin of the coordinate system for RecIntg 
      call GetLoccField(spaceInt(1:3,0),-9_2,0_4,cf)
      if(iAmpl.gt.9) iAmpl=9
      if(iAmpl.lt.-9) iAmpl=-9
      select case(iAmpl)
      case(9)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cdSqrt(cV(3))
        cMMPeigenVector(1)=cdSqrt(cV(3))
        cMMPeigenVector(2:3)=cdSqrt(cV(1:2))
      case(8)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cdSqrt(cV(2))
        cMMPeigenVector(1)=cdSqrt(cV(2))
        cMMPeigenVector(2)=cdSqrt(cV(1))
        cMMPeigenVector(3)=cdSqrt(cV(3))
      case(7)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cdSqrt(cV(1))
        cMMPeigenVector(1)=cdSqrt(cV(1))
        cMMPeigenVector(2:3)=cdSqrt(cV(2:3))
      case(6)
        cMMPeigenPhase=cf(6)
        cMMPeigenVector(1)=cf(6)
        cMMPeigenVector(2:3)=cf(4:5)
      case(5)
        cMMPeigenPhase=cf(5)
        cMMPeigenVector(1)=cf(5)
        cMMPeigenVector(2)=cf(4)
        cMMPeigenVector(3)=cf(6)
      case(4)
        cMMPeigenPhase=cf(4)
        cMMPeigenVector(1)=cf(4)
        cMMPeigenVector(2:3)=cf(5:6)
      case(3)
        cMMPeigenPhase=cf(3)
        cMMPeigenVector(1)=cf(3)
        cMMPeigenVector(2:3)=cf(1:2)
      case(2)
        cMMPeigenPhase=cf(2)
        cMMPeigenVector(1)=cf(2)
        cMMPeigenVector(2)=cf(1)
        cMMPeigenVector(3)=cf(3)
      case(1)
        cMMPeigenPhase=cf(1)
        cMMPeigenVector(1)=cf(1)
        cMMPeigenVector(2:3)=cf(2:3)
      case(-9)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cV(3)
        cMMPeigenVector(1)=cV(3)
        cMMPeigenVector(2:3)=cV(1:2)
      case(-8)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cV(2)
        cMMPeigenVector(1)=cV(2)
        cMMPeigenVector(2)=cV(1)
        cMMPeigenVector(3)=cV(3)
      case(-7)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cV(1)
        cMMPeigenVector(1)=cV(1)
        cMMPeigenVector(2:3)=cV(2:3)
      case(-6)
        cMMPeigenPhase=cf(6)**2
        cMMPeigenVector(1)=cf(6)**2
        cMMPeigenVector(2:3)=cf(4:5)**2
      case(-5)
        cMMPeigenPhase=cf(5)**2
        cMMPeigenVector(1)=cf(5)**2
        cMMPeigenVector(2)=cf(4)**2
        cMMPeigenVector(3)=cf(6)**2
      case(-4)
        cMMPeigenPhase=cf(4)**2
        cMMPeigenVector(1)=cf(4)**2
        cMMPeigenVector(2:3)=cf(5:6)**2
      case(-3)
        cMMPeigenPhase=cf(3)**2
        cMMPeigenVector(1)=cf(3)**2
        cMMPeigenVector(2:3)=cf(1:2)**2
      case(-2)
        cMMPeigenPhase=cf(2)**2
        cMMPeigenVector(1)=cf(2)**2
        cMMPeigenVector(2)=cf(1)**2
        cMMPeigenVector(3)=cf(3)**2
      case(-1)
        cMMPeigenPhase=cf(1)**2
        cMMPeigenVector(1)=cf(1)**2
        cMMPeigenVector(2:3)=cf(2:3)**2
      case(0)
        cV=c3Vec_Prod(cf(1:3),Conjg(cf(4:6)))
        cMMPeigenPhase=cV(1)+cV(2)+cV(3)
        cMMPeigenVector(1:3)=cV(1:3)
      end select
      currentIntegral=cdabs(cMMPeigenPhase)
      cMMPeigenPhase=dCmplx(currentIntegral,0.0d0).div.cMMPeigenPhase
      cMMPeigenVector(1)=(1.0d0,0.0d0).div.cMMPeigenVector(1)
      cMMPeigenVector(2)=(1.0d0,0.0d0).div.cMMPeigenVector(2)
      cMMPeigenVector(3)=(1.0d0,0.0d0).div.cMMPeigenVector(3)
    else
      currentIntegral=0.0d0
    end if
    lAskQ=.true.
    if(lStopThread) return
    if(dabs(currentIntegral).gt.pSmall) then ! scale with integral
      if((itrFld.eq.itS).or.(itrFld.eq.itWe).or.(itrFld.eq.itWh).or.(itrFld.eq.itWt).or. &
      & (itrFld.eq.itPe).or.(itrFld.eq.itPh).or.(itrFld.eq.itPt)) then
        q=1.0d0/dsqrt(dabs(currentIntegral))
      else
        q=1.0d0/dabs(currentIntegral)
      end if
      idum=mCol
      if((lMMPstat.or.lMMPBndVal)) idum=idum-1
      k=0
      do kE=1,nExp
        if(LSkipExp(kE)) Cycle
        if(igetiHE2(iHEGlobal,kE).lt.0_2) Cycle !! Hatte gefehlt ?????
       !!!  if(.not.lgcFld) then ! 3D problem
          if(tExp(kE)%iObj.lt.0) Cycle
       !!!  end if
        i1=tExp(kE)%iOff+1
        i2=tExp(kE)%iOff+tExp(kE)%nPar
        do i=i1,i2
          k=k+1
          if(k.gt.idum) Exit
          ParExp(1:nRHS,i)=ParExp(1:nRHS,i)*q
        end do
      end do
      residual=resUnscaled*q*q
    else
      currentIntegral=1.0d0
      residual=resUnscaled
    end if
    call OutTxt('t1','terminated'C)
  end Subroutine solveMtr

  Subroutine getErrors(ldum)
! get the mmp errors
    Implicit None
    Include 'resource.fd'
    Logical, Intent(in):: ldum
    Real(8) et(3),ez(3),en(3),r(3),xPt,yPt,xtPt,ytPt,err,fM,fA,eM,eA,wgt,fMA
    Integer(4) kPBnd,nBP,kPt,iErr,lout
    Integer(2) iDomL,iDomR,iConExp0,iConBnd0
    if(.not.lgcFld) lGet3DMat=.true.
    if(nBndPt.lt.1) return
    call OutTxt('t1','Compute errors'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    lMMPBndVal=.false.
    iConExp0=iConExp
    iConBnd0=iConBnd
    iConExp=iMMPCon
    iConBnd=iMMPCon
    call getEUST()
    iBound=abs(iBound)
    if((iBound.ne.2_2).and.(iBound.ne.4_2)) call cBndGetABO() ! get c-poly, splines, match.pts
    if((iBound.ne.2_2).and.(iBound.ne.4_2)) then
      iConExp=iConExp0
      iConBnd=iConBnd0
      lMMPBndVal=.false.
      return
    end if
    if(lgcFld) then
      nBndPt3D=nBndPt
      nInhBndPt3D=0
    else
      call get3DMatPts(1,nObj,0_4,1_2,.true.) ! generate 3D matching points
    end if
    eBndPt=0.0d0
    fBndPt=0.0d0
    jBndPt=0
    errorM=0.0d0
    errorMA=0.0d0
    errorA=0.0d0
    eM=0.0d0
    eA=0.0d0
    fM=0.0d0
    fA=0.0d0
    nBP=0
    lDispWarn=.true.
    do kPt=1,nBndPt3D
      if(.not.lgcFld) then
        if(iObjBndPt3D(kPt).lt.0) Cycle
      end if
      if(lStopThread) Exit
      call IntToStr(kPt,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nBndPt3D,0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      kBndPt=kPt
      if(.not.lgcFld) kBndPt=iBndPt3D(kPt)
      if(tBnd(iBndPt(kBndPt))%Weight.lt.pSmall) Cycle
      call GetBndPt(0,sBndPt(kBndPt),xPt,yPt,xtPt,ytPt,iDomL,iDomR,kPBnd)
      if((iDomL.gt.30000_2).or.(iDomL.lt.-30000_2)) Cycle
      if(lgcFld) then
        r(1)=xPt
        r(2)=yPt
        r(3)=0.0d0
        et(1)=xtPt
        et(2)=ytPt
        et(3)=0.0d0
        ez(1)=0.0d0
        ez(2)=0.0d0
        ez(3)=1.0d0
        en(1)=-ytPt
        en(2)=xtPt
        en(3)=0.0d0
        call Unit3DV(et)
        call Unit3DV(ez)
        call Unit3DV(en)
        wgt=dsqrt(wBndPt(kBndPt))*getWeight(kBndPt)
        call GetError(r,et,ez,en,iBndPt(kBndPt),wgt,eBndPt(kPt),fBndPt(kPt),iErr)
      else
        r(1:3)=BndPt3D(1:3,0,kPt)
        et(1:3)=BndPt3D(1:3,1,kPt)
        ez(1:3)=BndPt3D(1:3,2,kPt)
        en(1:3)=BndPt3D(1:3,3,kPt)
        call Unit3DV(et)
        call Unit3DV(ez)
        call Unit3DV(en)
        wgt=wBndPt3D(kPt)*getWeight(kBndPt) ! kPt is the current 3D matching point, kBndPt associated the 2D matching point
        call GetError(r,et,ez,en,iBndPt(kBndPt),wgt,eBndPt3D(kPt),fBndPt3D(kPt),iErr)
        if(iErr.lt.1) then
          eBndPt(kBndPt)=eBndPt(kBndPt)+eBndPt3D(kPt)
          fBndPt(kBndPt)=fBndPt(kBndPt)+fBndPt3D(kPt)
          jBndPt(kBndPt)=jBndPt(kBndPt)+1
        end if
      end if
      if(iErr.eq.1) Cycle
      if(iErr.gt.1) then
        iConExp=iConExp0
        iConBnd=iConBnd0
        lMMPBndVal=.false.
        return
      end if
      nBP=nBP+1
      if(lgcFld) then
        eA=eA+eBndPt(kPt)
        if(eBndPt(kPt).gt.eM) eM=eBndPt(kPt)
        fA=fA+fBndPt(kPt)
        fMA=fBndPt(kPt)
        if(fBndPt(kPt).gt.fM) fM=fBndPt(kPt)
        err=100.0d0*(eBndPt(kPt).div.fBndPt(kPt))
      else
        eA=eA+eBndPt3D(kPt)
        if(eBndPt3D(kPt).gt.eM) eM=eBndPt3D(kPt)
        fA=fA+fBndPt3D(kPt)
        fMA=fBndPt3D(kPt)
        if(fBndPt3D(kPt).gt.fM) fM=fBndPt3D(kPt)
        err=100.0d0*(eBndPt3D(kPt).div.fBndPt3D(kPt))
      end if
      errorA=errorA+err
      if(err.gt.errorM) then
        errorM=err
        errorMA=fMA
      end if
    end do
    if(.not.lgcFld) then
      do kPt=1,nBndPt
        wgt=1.0d0/Dble(max(jBndPt(kPt),1))
        eBndPt(kPt)=eBndPt(kPt)*wgt
        fBndPt(kPt)=fBndPt(kPt)*wgt
      end do
    end if
    eA=eA/Dble(Max(1,nBP))
    fA=fA/Dble(Max(1,nBP))
    errorA=errorA/Dble(Max(1,nBP))
    AerrorA=eA
    AerrorM=eM
    iConExp=iConExp0
    iConBnd=iConBnd0
    lMMPBndVal=.false.
! save results in array
    vErrStat(1)=AerrorA
    vErrStat(2)=AerrorM
    vErrStat(3)=errorA
    vErrStat(4)=errorM
    vErrStat(5)=fA
    vErrStat(6)=fM
    if(ldum) then ! output results
      if(l4) write(*,*) ' Average of the field =',fA
      if(l4) write(*,*) ' Maximum of the field =',fM
      if(l4) write(*,*) ' Average of the absolute error =',AerrorA
      if(l4) write(*,*) ' Maximum of the absolute error =',AerrorM
      if(l4) write(*,*) ' Average of the relative error =',errorA,' %'
      if(l4) write(*,*) ' Maximum of the relative error =',errorM,' %'
      if(l4) write(*,*) ' Field at  max. relative error =',errorMA
    end if
    call OutTxt('t1','terminated'C)
  end Subroutine getErrors

  Subroutine GetError(r,et,ez,en,kPBnd,wBPt,eBPt,fBPt,iErr)
! get mismatching error in point r with tangential unit vectors et,ez, normal unit vector en
! weight wBPt, corresponding 2D boundary: kPBnd
! return error eBPt and field fBPt
    Implicit None
    Complex(8) e(10),Cp,cFldBnd(10),wEz,wEt,wEn,wHz,wHt,wHn,wAz,wAt,wAn,wV
    Real(8) r(3),et(3),ez(3),en(3),wBPt,eBPt,fBPt,dmin,val(2),f(10),rp(3),rNmin(3)
    Integer(4) iErr,nBC,iBC,kBC,idum,kExpFF
    Integer(2) kPBnd,iDomL,iDomR,iBPt,iL,iR,iColLoc,iConLoc,iDomLoc,iD0
    Logical lBC(10),lusual,lspec,lxper,lyper,lzper,lxperio,lyperio,lzperio,lsibc,lRSidemin
    iErr=1
    nBC=tBnd(kPBnd)%nBC
    eBPt=0.0d0
    fBPt=pSmall
    if((nBC.lt.1).or.(dabs(wBPt).lt.pSmall)) return
    iDomL=tBnd(kPBnd)%iLDom
    iDomR=tBnd(kPBnd)%iRDom
    iBPt=tBnd(kPBnd)%iCond
    call getBndCnd(iBPt,lusual,lspec,lxper,lyper,lzper,lsibc,lBC(1),lBC(2),lBC(3),lBC(4),lBC(5),lBC(6), &
    &              lBC(8),lBC(10))
    if(lusual) then
      if(iDomL.eq.iDomR) return
      if((iDomL.lt.1_2).and.(iDomL.gt.-4_2)) then
        if(iDomR.lt.1_2) then
          iErr=2
          return
        end if
      end if
    end if
    if(lsibc) nBC=2
    lBC(1:10)=tBnd(kPBnd)%lBC(1:10)
    iL=Max(1_2,Min(Int2(nDom),iDomL))
    iR=Max(1_2,Min(Int2(nDom),iDomR))
    if(iDomL.lt.0) iL=iR
    if(iDomR.lt.0) iR=iL
    FldMtr(1:10,1:mCol)=(0.0d0,0.0d0)
    call getWeights(iL,iR,1.0d0,wEz,wEt,wEn,wHz,wHt,wHn,wAz,wAt,wAn,wV)
    if((iDomL.lt.0).or.(iDomR.lt.0)) then ! impressed field on this boundary
      lMMPBndVal=.true.
      iColLoc=iColBnd
      iConLoc=iConBnd
      iDomLoc=iDomBnd
      iColBnd=Int2(kColFFD)
      iConBnd=Int2(kConFFD)
      iDomBnd=Int2(kDomFFD)
      call DistPtObj(0_2,0_2,r,.true.,dmin,rNmin,iD0,val,.true.,lRSidemin)
      if(iD0.lt.0) iD0=0
      iColBnd=iColLoc
      iConBnd=iConLoc
      iDomBnd=iDomLoc
      iColLoc=iColExp
      iConLoc=iConExp
      iDomLoc=iDomExp
      iColExp=Int2(kColFFD)
      iConExp=Int2(kConFFD)
      iDomExp=Int2(kDomFFD)
      cFldBnd=(0.0d0,0.0d0)
      if(lExpFFD) then
        kExpFF=kExpFFD
        if(lExcFFD) then
          if((kExpFFD.eq.nExp).or.(kExpFFD.eq.0).or.(kExpFFD.le.-nExp)) kExpFF=-(nExp-nExc)
        end if
        call GetFieldExp(kExpFF,0,r,iD0,iHEGlobal,idum)
        cFldBnd=FldExp
      else
        if((iEx.eq.0_2).and.(iEy.eq.0_2)) lBC(1:2)=.false.
        if(iEz.eq.0_2) lBC(3)=.false.
        if((iHx.eq.0_2).and.(iHy.eq.0_2)) lBC(4:5)=.false.
        if(iHz.eq.0_2) lBC(6)=.false.
        if((iAx.eq.0_2).and.(iAy.eq.0_2)) lBC(7:8)=.false.
        if(iAz.eq.0_2) lBC(9)=.false.
        if(iV.eq.0_2) lBC(10)=.false.
        call GetLoccField(r,iD0,3,cFldBnd)
      end if
      iColExp=iColLoc
      iConExp=iConLoc
      iDomExp=iDomLoc
    end if
    e=(0.0d0,0.0d0)
    f=0.0d0
    if(lusual.or.lspec) then
! ordinary boundary conditions
      if(iDomL.gt.0_2) then
        call GetFieldExp(0,0,r,iL,iHEGlobal,idum)
        if(lBC(1)) then
          e(1)=wEt*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))
        end if
        if(lBC(2)) then
          e(2)=wEz*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3))
        end if
        if(lBC(3)) then
          e(3)=wEn*eDom(iL)*(FldExp(1)*en(1)+FldExp(2)*en(2)+FldExp(3)*en(3))/sqrt(eDom(iL)*eDom(iR))
          if(lBC(8)) e(3)=e(3)+tDom(iL)*wHn*uDom(iL)* & ! topological isolator
          &           (FldExp(4)*en(1)+FldExp(5)*en(2)+FldExp(6)*en(3))/sqrt(eDom(iL)*eDom(iR))
        end if
        if(lBC(4)) then
          e(4)=wHt*(FldExp(4)*et(1)+FldExp(5)*et(2)+FldExp(6)*et(3))
          if(lBC(8)) e(4)=e(4)-tDom(iL)*wEt* & ! topological isolator
          &          (FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))
          if(lBC(10)) e(4)=e(4) &
          & +sDom(iL)*wHt*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3)) & ! electric surface current
          & +tDom(iL)*wHt*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3)) ! electric surface current (anisotropic part)
        end if
        if(lBC(5)) then
          e(5)=wHz*(FldExp(4)*ez(1)+FldExp(5)*ez(2)+FldExp(6)*ez(3))
          if(lBC(8)) e(5)=e(5)-tDom(iL)*wEz* & ! topological isolator
          &          (FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3))
          if(lBC(10)) e(5)=e(5) &
          & -sDom(iL)*wHz*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3)) & ! electric surface current
          & +tDom(iL)*wHz*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3)) ! electric surface current (anisotropic part)
        end if
        if(lBC(6)) e(6)=wHn*uDom(iL)*(FldExp(4)*en(1)+FldExp(5)*en(2)+FldExp(6)*en(3))/sqrt(uDom(iL)*uDom(iR))
        if(lBC(7)) e(7)=wAt*(FldExp(7)*et(1)+FldExp(8)*et(2)+FldExp(9)*et(3))
        if(lBC(8)) e(8)=wAz*(FldExp(7)*ez(1)+FldExp(8)*ez(2)+FldExp(9)*ez(3))
        if(lBC(9)) e(9)=wAn*(FldExp(7)*en(1)+FldExp(8)*en(2)+FldExp(9)*en(3))
        if(lBC(10)) e(10)=wV*FldExp(10)
        FldExp(4:6)=FldExp(4:6)*Zw0*sqrt(sqrt(uDom(iL)/eDom(iL))*sqrt(uDom(iR)/eDom(iR)))
        f(1:10)=cdabs(FldExp(1:10))
      end if
      if(iDomR.gt.0_2) then
        call GetFieldExp(0,0,r,iR,iHEGlobal,idum)
        if(lBC(1)) then
          e(1)=e(1)-wEt*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))
        end if
        if(lBC(2)) then
          e(2)=e(2)-wEz*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3))
        end if
        if(lBC(3)) then
          e(3)=e(3)-wEn*eDom(iR)*(FldExp(1)*en(1)+FldExp(2)*en(2)+FldExp(3)*en(3))/sqrt(eDom(iL)*eDom(iR))
          if(lBC(8)) e(3)=e(3)-tDom(iR)*wHn*uDom(iR)* & ! topological isolator
          &            (FldExp(4)*en(1)+FldExp(5)*en(2)+FldExp(6)*en(3))/sqrt(eDom(iL)*eDom(iR))
        end if
        if(lBC(4)) then
          e(4)=e(4)-wHt*(FldExp(4)*et(1)+FldExp(5)*et(2)+FldExp(6)*et(3))
          if(lBC(8)) e(6)=e(6)+tDom(iR)*wEt* & ! topological isolator
          &          (FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))
          if(lBC(10)) e(4)=e(4) &
          & -sDom(iR)*wHt*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3)) & ! electric surface current
          & -tDom(iR)*wHt*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3)) ! electric surface current (anisotropic part)
        end if
        if(lBC(5)) then
          e(5)=e(5)-wHz*(FldExp(4)*ez(1)+FldExp(5)*ez(2)+FldExp(6)*ez(3))
          if(lBC(8)) e(5)=e(5)+tDom(iR)*wEz* & ! topological isolator
          &          (FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3))
          if(lBC(10)) e(5)=e(5) &
          & +sDom(iR)*wHz*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))& ! electric surface current
          & -tDom(iR)*wHz*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3)) ! electric surface current (anisotropic part)
        end if
        if(lBC(6)) e(6)=e(6)-wHn*uDom(iR)*(FldExp(4)*en(1)+FldExp(5)*en(2)+FldExp(6)*en(3))/sqrt(uDom(iL)*uDom(iR))
        if(lBC(7)) e(7)=e(7)-wAt*(FldExp(7)*et(1)+FldExp(8)*et(2)+FldExp(9)*et(3))
        if(lBC(8)) e(8)=e(8)-wAz*(FldExp(7)*ez(1)+FldExp(8)*ez(2)+FldExp(9)*ez(3))
        if(lBC(9)) e(9)=e(9)-wAn*(FldExp(7)*en(1)+FldExp(8)*en(2)+FldExp(9)*en(3))
        if(lBC(10)) e(10)=e(10)-wV*FldExp(10)
        FldExp(4:6)=FldExp(4:6)*Zw0*sqrt(sqrt(uDom(iL)/eDom(iL))*sqrt(uDom(iR)/eDom(iR)))
        f(1:10)=f(1:10)+cdabs(FldExp(1:10))
      end if
      if((iDomL.lt.0).or.(iDomR.lt.0)) then
        if(lBC(1)) e(1)=e(1)+wEt*(cFldBnd(1)*et(1)+cFldBnd(2)*et(2)+cFldBnd(3)*et(3))
        if(lBC(2)) e(2)=e(2)+wEz*(cFldBnd(1)*ez(1)+cFldBnd(2)*ez(2)+cFldBnd(3)*ez(3))
        if(lBC(3)) e(3)=e(3)+wEn*(cFldBnd(1)*en(1)+cFldBnd(2)*en(2)+cFldBnd(3)*en(3))
        if(lBC(4)) e(4)=e(4)+wHt*(cFldBnd(4)*et(1)+cFldBnd(5)*et(2)+cFldBnd(6)*et(3))
        if(lBC(5)) e(5)=e(5)+wHz*(cFldBnd(4)*ez(1)+cFldBnd(5)*ez(2)+cFldBnd(6)*ez(3))
        if(lBC(6)) e(6)=e(6)+wHn*(cFldBnd(4)*en(1)+cFldBnd(5)*en(2)+cFldBnd(6)*en(3))
        if(lBC(7)) e(7)=e(7)+wAt*(cFldBnd(7)*et(1)+cFldBnd(8)*et(2)+cFldBnd(9)*et(3))
        if(lBC(8)) e(8)=e(8)+wAz*(cFldBnd(7)*ez(1)+cFldBnd(8)*ez(2)+cFldBnd(9)*ez(3))
        if(lBC(9)) e(9)=e(9)+wAn*(cFldBnd(7)*en(1)+cFldBnd(8)*en(2)+cFldBnd(9)*en(3))
        if(lBC(10)) e(10)=e(10)+wV*cFldBnd(10)
      else if(iDomL.lt.1_2) then
        if(lBC(8).and.(tBnd(kPBnd)%val(2).le.pBig)) e(8)=e(8)+wAz*dCmplx(tBnd(kPBnd)%val(2),0.0d0)
        if(lBC(10).and.(tBnd(kPBnd)%val(1).le.pBig)) e(10)=e(10)+wV*dCmplx(tBnd(kPBnd)%val(1),0.0d0)
      else if(iDomR.lt.1_2) then
        if(lBC(8).and.(tBnd(kPBnd)%val(2).le.pBig)) e(8)=e(8)-wAz*dCmplx(tBnd(kPBnd)%val(2),0.0d0)
        if(lBC(10).and.(tBnd(kPBnd)%val(1).le.pBig)) e(10)=e(10)-wV*dCmplx(tBnd(kPBnd)%val(1),0.0d0)
      end if
      kBC=0
      if(lBC(1).or.lBC(2).or.lBC(3).or.lBC(4).or.lBC(5).or.lBC(6)) then
        fBPt=sum(f(1:6))
        kBC=6
      end if
      do iBC=7,10
        if(lBC(iBC)) then
          fBPt=fBPt+f(iBC)
          kBC=kBC+1
        end if 
      end do
      fBPt=fBPt/Dble(kBC)
      eBPt=sum(cdabs(e(1:10)))/Dble(nBC)
    else if(lxper.or.lyper.or.lzper) then
! periodic boundary conditions
      rp(1:3)=r(1:3)
      if(lxper) then
        rp(1)=rp(1)-xPeriod
        Cp=cdexp((0.0d0,1.0d0)*cxPeriod*xPeriod)
      else if(lyper) then
        rp(1:3)=rp(1:3)-yPeriodVector(1:3)
        Cp=cdexp((0.0d0,1.0d0)*cyPeriod*r3Vec_Length(yPeriodVector))
      else
        rp(1:3)=rp(1:3)-zPeriodVector(1:3)
        Cp=cdexp((0.0d0,1.0d0)*czPeriod*r3Vec_Length(zPeriodVector))
      end if
      lxPerio=lxPeriod
      lyPerio=lyPeriod
      lzPerio=lzPeriod
      lxPeriod=.false.
      lyPeriod=.false.
      lzPeriod=.false.
      call GetFieldExp(0,0,r,iL,iHEGlobal,idum)
      if(lBC(1)) e(1)=wEt*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))
      if(lBC(2)) e(2)=wEz*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3))
      if(lBC(3)) e(3)=wEn*(eDom(iL)*(FldExp(1)*en(1)+FldExp(2)*en(2)+FldExp(3)*en(3))/dsqrt(cdabs(eDom(iL)*eDom(iR))))
      if(lBC(4)) e(4)=wHt*(FldExp(4)*et(1)+FldExp(5)*et(2)+FldExp(6)*et(3))
      if(lBC(5)) e(5)=wHz*(FldExp(4)*ez(1)+FldExp(5)*ez(2)+FldExp(6)*ez(3))
      if(lBC(6)) e(6)=wHn*(uDom(iL)*(FldExp(4)*en(1)+FldExp(5)*en(2)+FldExp(6)*en(3))/dsqrt(cdabs(uDom(iL)*uDom(iR))))
      if(lBC(7)) e(7)=wAt*(FldExp(7)*et(1)+FldExp(8)*et(2)+FldExp(9)*et(3))
      if(lBC(8)) e(8)=wAz*(FldExp(7)*ez(1)+FldExp(8)*ez(2)+FldExp(9)*ez(3))
      if(lBC(9)) e(9)=wAn*(FldExp(7)*en(1)+FldExp(8)*en(2)+FldExp(9)*en(3))
      if(lBC(10)) e(10)=wV*FldExp(10)
      FldExp(4:6)=FldExp(4:6)*Zw0*dsqrt(dsqrt(cdabs(uDom(iL)/eDom(iL)))*dsqrt(cdabs(uDom(iR)/eDom(iR))))
      f(1:10)=cdabs(FldExp(1:10))
      call GetFieldExp(0,0,rp,iL,iHEGlobal,idum)
      if(lBC(1)) e(1)=e(1)-Cp*wEt*(FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3))
      if(lBC(2)) e(2)=e(2)-Cp*wEz*(FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3))
      if(lBC(3)) e(3)=e(3)-Cp*wEn*(eDom(iL)*(FldExp(1)*en(1)+FldExp(2)*en(2)+FldExp(3)*en(3))/dsqrt(cdabs(eDom(iL)*eDom(iR))))
      if(lBC(4)) e(4)=e(4)-Cp*wHt*(FldExp(4)*et(1)+FldExp(5)*et(2)+FldExp(6)*et(3))
      if(lBC(5)) e(5)=e(5)-Cp*wHz*(FldExp(4)*ez(1)+FldExp(5)*ez(2)+FldExp(6)*ez(3))
      if(lBC(6)) e(6)=e(6)-Cp*wHn*(uDom(iL)*(FldExp(4)*en(1)+FldExp(5)*en(2)+FldExp(6)*en(3))/dsqrt(cdabs(uDom(iL)*uDom(iR))))
      if(lBC(7)) e(7)=e(7)-Cp*wAt*(FldExp(7)*et(1)+FldExp(8)*et(2)+FldExp(9)*et(3))
      if(lBC(8)) e(8)=e(8)-Cp*wAz*(FldExp(7)*ez(1)+FldExp(8)*ez(2)+FldExp(9)*ez(3))
      if(lBC(9)) e(9)=e(9)-Cp*wAn*(FldExp(7)*en(1)+FldExp(8)*en(2)+FldExp(9)*en(3))
      if(lBC(10)) e(10)=e(10)-Cp*wV*FldExp(10)
      FldExp(4:6)=FldExp(4:6)*Zw0*dsqrt(dsqrt(cdabs(uDom(iL)/eDom(iL)))*dsqrt(cdabs(uDom(iR)/eDom(iR))))
      f(1:10)=f(1:10)+cdabs(Cp*FldExp(1:10))
      kBC=0
      if(lBC(1).or.lBC(2).or.lBC(3).or.lBC(4).or.lBC(5).or.lBC(6)) then
        fBPt=sum(f(1:6))
        kBC=6
      end if
      do iBC=7,10
        if(lBC(iBC)) then
          fBPt=fBPt+f(iBC)
          kBC=kBC+1
        end if 
      end do
      fBPt=fBPt/Dble(kBC)
      eBPt=sum(cdabs(e(1:10)))/Dble(nBC)
      lxPeriod=lxPerio
      lyPeriod=lyPerio
      lzPeriod=lzPerio
    else if(lsibc) then
! surface impedance boundary conditions
      call GetFieldExp(0,0,r,iR,iHEGlobal,idum)
      cZw=Zw0*cdsqrt(uDom(iL)/eDom(iL))
      e(1)=FldExp(1)*et(1)+FldExp(2)*et(2)+FldExp(3)*et(3)+cZw*(FldExp(4)*ez(1)+FldExp(5)*ez(2)+FldExp(6)*ez(3))
      e(2)=FldExp(1)*ez(1)+FldExp(2)*ez(2)+FldExp(3)*ez(3)-cZw*(FldExp(4)*et(1)+FldExp(5)*et(2)+FldExp(6)*et(3))
      FldExp(4:6)=FldExp(4:6)*Zw0*dsqrt(cdabs(uDom(iR)/eDom(iR)))
      f(1:6)=cdabs(FldExp(1:6))
      fBPt=sum(f(1:6))/6.0d0
      eBPt=sum(cdabs(e(1:2)))/2.0d0
    end if
    iErr=0
  end Subroutine getError

  Subroutine getEigen(lCheck)
! find eigenvalue
! iEigen=0: frequency is eigenvalue, 1: gamma is eigenvalue, iEigen=2: CX is eigenvalue, 3: CY, 4: CZ, 5:CX+Y, 6:CX+Z, 7:CY+Z, 8:CX+Y+Z
! nrEigen,niEigen rough search grid
! c1Eigen,c2Eigen corners of the rough search grid in the complex plane
! c1EigenC,c2EigenC corners of the fine search clip area in the complex plane
! imEigen: eigenvalue number for the fine search
! itmEigen,aEigen,fEigen: max.number of iterations, accuracy, flatness
    Implicit none
    Include 'resource.fd'
    Complex(8) c1fine,c2fine,ct,dct,cest,cdest,ces(2)
    Complex(8), Save:: zmin(100),dzmin(100)
    Real(8) fkEVL,dE,ReE,AiE,q
    Integer(4) iOKeigen,iOK,ios,iunit,ithvmax,ifine,kP,itmE,idum,lout,nord,itEigen1,k1
    Integer(2) ic
    Logical, intent(in) :: lCheck
    Logical lUopen,ldum
	  call OutTxt('t2','Find eigenvalue'C)
	  call OutTxt('n2',' 'C)
	  call OutTxt('m2',' 'C)
    if(kPet.lt.1) itEigen=0
    resEigen=pBig
    errorM=0.0d0
    errorA=0.0d0
    iunit=0_4
    if(lEigenSave.or.(nEigenSave.gt.0)) then
      nEigenSave=max(0,nEigenSave-1)
      iUnit=3_4
      inquire(unit=iUnit,opened=lUopen)
      if(lUopen) then
        idum=MessageBoxQQ('Unit 3 is open!\rCannot save eigenvalue data!\rClose unit 8 first?'C,'Save eigenvalues'C, &
                          MB$YesNo.or.MB$IconQuestion)
        if(idum.eq.MB$IDYES) then
          close(3)
        else
          iunit=0_4
        end if
      end if
    end if
    if(lPET.and.(kPET.eq.0)) then ! restart rough search
      lMMPrough=.true.
    else if((kPET.lt.0).and.(.not.lMMPrough)) then ! use previous rough search values
      if((imEigen.gt.mmEigen).or.(imEigen.lt.1)) then
        ct=c1EigenC
        if(iEigen.eq.1) then ! gamma
          if(Dble(ct).lt.1.0d300) gcFld=ct
          dcFld=0.5d0/(gcFld*fcFld*kw0)
        else if(iEigen.lt.1) then ! frequency
          if(Dble(ct).lt.1.0d300) fcFld=ct
        else ! CX,Y,Z
          if(Dble(ct).lt.1.0d300) call setCxyz(iEigen,ct)
        end if
        return
      end if
      cest=zmin(imEigen)
      cdest=dzmin(imEigen)
      kPET=0
    end if
    if((nrEigen.gt.0).and.lMMPrough) then
! rough search
      kPET=0
      if(iunit.gt.0_4) then
! open FUN or FLD file for saving the rough search data
        if((.not.lWriteRoughFld).or.(niEigen.lt.2)) then
          call setNameExt(CfkFileName,'FUN',CfkFileName)
        else
          call setNameExt(CfkFileName,'FLD',CfkFileName)
        end if
        if(.not.lCheck) then
          if((.not.lWriteRoughFld).or.(niEigen.lt.2)) then
            call Open2write(-1,'Select Function data file to be written!','Function data file ',CfkFileName,'FUN',ios)
          else
            call Open2write(-1,'Select Field data file to be written!','Field data file ',CfkFileName,'FLD',ios)
          end if
          if(ios.gt.0) iunit=0_4
        end if
        if(iunit.gt.0_4) then
          open(iunit,file=CfkFileName,status='unknown',iostat=ios)
          if(ios.ne.0) then
            idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save eigenvalues'C, &
                              MB$OK.or.MB$IconExclamation)
            inquire(unit=iUnit,opened=ldum)
	          if(ldum) close(iUnit)
            return
          end if
          if((.not.lWriteRoughFld).or.(niEigen.lt.2)) then
            call incName(CfkFileName,'MAX','FUN',1)
          else
            call incName(CfkFileName,'MAX','FLD',1)
          end if
        end if
      end if
! start rough search
	    call OutTxt('t2','Rough search'C)
      call IntToStr(Int4(nrEigen*niEigen),0,0,SpaceText,lout)
		  call OutTxt('m2',SpaceText(1:lout))
      iMMPeigen=1_2
      iResCount=0
      call wricfk(getRes,c1Eigen,c2Eigen,nrEigen,niEigen,iunit,imEigen,mmEigen,zmin,dzmin,fkEVL)
      if(mmEigen.gt.0) then
        if(niEigen.lt.1) then
          if(Dabs(Dble(c2Eigen)-Dble(c1Eigen)).lt.Dabs(Dimag(c2Eigen)-Dimag(c1Eigen))) then
            dzmin(1:mmEigen)=DCmplx(0.0d0,cdabs(dzmin(1:mmEigen)))
          else
            dzmin(1:mmEigen)=DCmplx(cdabs(dzmin(1:mmEigen)),0.0d0)
          end if
        else if(niEigen.eq.1) then
          dzmin(1:mmEigen)=DCmplx(cdabs(dzmin(1:mmEigen)),cdabs(dzmin(1:mmEigen)))
        end if
      end if
      iResCount=-1
	    call OutTxt('t2','End rough search'C)
      iMMPeigen=0_2
      if(lPET) lMMPrough=.false.
      if(iunit.gt.0_4) then
! close the data file
        sch(1:5)=' '
        call chwrit2(iunit,ich,0,rch,0,sch,1,iOK)
        EndFile(iunit)
        close(iunit)
      end if
      if((imEigen.gt.mmEigen).or.(imEigen.lt.1)) then
        ct=zmin(1)
        if(iEigen.eq.1) then ! gamma
          if(Dble(ct).lt.1.0d300) gcFld=ct
          dcFld=0.5d0/(gcFld*fcFld*kw0)
        else if(iEigen.lt.1) then ! frequency
          if(Dble(ct).lt.1.0d300) fcFld=ct
        else ! CX,Y,Z
          if(Dble(ct).lt.1.0d300) call setCxyz(iEigen,ct)
        end if
        resEigen=fkEVL
        return
      end if
! set bounds for the fine search
      c1fine=zmin(imEigen)-dzmin(imEigen)
      c2fine=zmin(imEigen)+dzmin(imEigen)
      dPET=DCmplx(Dabs(Dble(c2Eigen-c1Eigen))/Dble(Max(nrEigen,1)),Dabs(Dimag(c2Eigen-c1Eigen))/Dble(Max(niEigen,1)))
    else if(lPET) then ! polynomial extrapolation for cdest
   !!??   if(kPET.gt.3) then
      if(kPET.gt.2) then
        ReE=min(abs(3.0d0*Dble(ePET(2,3))-3.0d0*Dble(ePET(2,2))+Dble(ePET(2,1))), &
        &       abs(2.0d0*Dble(ePET(2,3))-Dble(ePET(2,2))), &
        & abs(Dble(ePET(2,3))))
        AiE=min(abs(3.0d0*Aimag(ePET(2,3))-3.0d0*Aimag(ePET(2,2))+Aimag(ePET(2,1))), &
        &       abs(2.0d0*Aimag(ePET(2,3))-Aimag(ePET(2,2))), &
        & abs(Aimag(ePET(2,3))))
        cdest=DCmplx(ReE,AiE)
  !!??    else if(kPET.gt.2) then
      else if(kPET.gt.1) then
        ReE=min(abs(2.0d0*Dble(ePET(2,2))-Dble(ePET(2,1))),abs(Dble(ePET(2,2))))
        AiE=min(abs(2.0d0*Aimag(ePET(2,2))-Aimag(ePET(2,1))),abs(Aimag(ePET(2,2))))
        cdest=DCmplx(ReE,AiE)
   !!??   else if(kPET.gt.1) then
      else if(kPET.gt.0) then
        ReE=abs(Dble(ePET(2,1)))
        AiE=abs(Aimag(ePET(2,1)))
        cdest=DCmplx(ReE,AiE)
   !!??   else if(kPET.gt.0) then
      else if(kPET.gt.-1) then
        ReE=abs(Dble(dzmin(imEigen)))
        AiE=abs(Aimag(dzmin(imEigen)))
        cdest=DCmplx(ReE,AiE)
      end if
      if(kPET.gt.0) then ! PET extrapolation for cest
        nord=kPET-1
        nfun=1
        do kP=1,kPET
          xPET(kP)=Dble(kP-1)/Dble(kPET)
        end do
        wPET(1:kPET)=1.0d0
        call crExtra(mPET,2_4,kPET,nfun,nord,xPET,ePET,wPET,sPET,1.0d0,ces,idum)
        cest=ces(1)
      end if
  !!??    q=cdabs(cdest)/cdabs(cest)
  !!??    if(q.lt.0.1d0*aEigen) then ! search interval too short
  !!??      ifine=3
  !!??      if(Dabs(Dble(cdest)).lt.pSmall) ifine=ifine-1
  !!??      if(Dabs(Dimag(cdest)).lt.pSmall) ifine=ifine-2
  !!??      de=0.1d0*aEigen*cdabs(cest)
  !!??      if(ifine.gt.2) then ! set cdest depending on search type
  !!??        cdest=(1.0d0,1.0d0)*de
  !!??      else if(ifine.eq.2) then
  !!??        cdest=DCmplx(0.0d0,de)
  !!??      else
  !!??        cdest=DCmplx(de,0.0d0)
  !!??      end if
  !!??    end if
      c1fine=cest-cdest
      c2fine=cest+cdest
    else
      if((imEigen.le.mmEigen).and.(imEigen.gt.0)) then
        c1fine=zmin(imEigen)-dzmin(imEigen)
        c2fine=zmin(imEigen)+dzmin(imEigen)
      else
        c1fine=c1Eigen
        c2fine=c2Eigen
      end if
    end if
    if(lStopThread) return
    if(lMMPfine.and.(iabs(itmEigen).gt.0)) then
! fine search
      if(iunit.gt.0_4) then
! open FUN file for saving the fine search data
        call setNameExt(CfkFileName,'FUN',CfkFileName)
        if(.not.lCheck) then
          call Open2write(-1,'Select Function data file to be written!','Function data file ',CfkFileName,'FUN',ios)
          if(ios.gt.0) iunit=0_4
        end if
        if(iunit.gt.0_4) then
          open(iunit,file=CfkFileName,status='unknown',iostat=ios)
          if(ios.ne.0) then
            idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save eigenvalues'C, &
                              MB$OK.or.MB$IconExclamation)
            inquire(unit=iUnit,opened=ldum)
	          if(ldum) close(iUnit)
            return
          end if
          call incName(CfkFileName,'MAX','FUN',1)
          if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
            call WriteStr(iunit,' CHFUN Version 1.0'C,idum)
            call WriteStr(iunit,'0 11'C,idum)
            call WriteStr(iunit,'n rough search'C,idum)
            call WriteStr(iunit,'Real(z)'C,idum)
            call WriteStr(iunit,'Imag(z)'C,idum)
            call WriteStr(iunit,'SearchFunction1(z)'C,idum)
            call WriteStr(iunit,'SearchFunction2(z)'C,idum)
            call WriteStr(iunit,'SearchFunction3(z)'C,idum)
            call WriteStr(iunit,'SearchFunction4(z)'C,idum)
            call WriteStr(iunit,'SearchFunction5(z)'C,idum)
            call WriteStr(iunit,'SearchFunction6(z)'C,idum)
            call WriteStr(iunit,'SearchFunction7(z)'C,idum)
            call WriteStr(iunit,'SearchFunction8(z)'C,idum)
            call WriteStr(iunit,'SearchFunction9(z)'C,idum)
          else
            call WriteStr(iunit,' CHFUN Version 1.0'C,idum)
            call WriteStr(iunit,'0 5'C,idum)
            call WriteStr(iunit,'n rough search'C,idum)
            call WriteStr(iunit,'Real(z)'C,idum)
            call WriteStr(iunit,'Imag(z)'C,idum)
            call WriteStr(iunit,'SearchFunction1(z)'C,idum)
            call WriteStr(iunit,'SearchFunction2(z)'C,idum)
            call WriteStr(iunit,'SearchFunction3(z)'C,idum)
          end if
        end if
      end if
! start fine search
      cest=0.5d0*(c1fine+c2fine)
      ifine=3
      if(Dabs(Dble(c2fine-c1fine)).lt.pSmall) ifine=ifine-1
      if(Dabs(Dimag(c2fine-c1fine)).lt.pSmall) ifine=ifine-2
      if(ifine.lt.1) then ! avoid zero search interval
        c1fine=cest*1.0000001
        c2fine=cest*0.9999999
        ifine=3
        if(Dabs(Dble(c2fine-c1fine)).lt.pSmall) ifine=ifine-1
        if(Dabs(Dimag(c2fine-c1fine)).lt.pSmall) ifine=ifine-2
      end if
      if((ifine.gt.2).and.(itmEigen.gt.0)) then ! set max. 1D iterations (getMin1) for search in rectangle/on line
        ithvmax=max((itmEigen/3),ithvmax0,3)
        if((kPET.gt.1).and.(itEigen.lt.(ithvmax*2))) ithvmax=max((itEigen/2),ithvmax0,3)
      else if((ifine.gt.0).and.(itmEigen.gt.0)) then
        ithvmax=max(itmEigen,3)
      else if(itmEigen.lt.0) then
        ithvmax=max(-itmEigen,3)
      else ! zero search interval -> stop
        idum=MessageBoxQQ('No eigenvalue search!\r(search area is zero)'C,'Eigenvalue search'C, &
                          MB$OK.or.MB$ICONHAND)
        if(iEigen.eq.1) then ! gamma
          if(Dble(cest).lt.1.0d300) gcFld=cest
          dcFld=0.5d0/(gcFld*fcFld*kw0)
        else if(iEigen.lt.1) then ! frequency
          if(Dble(cest).lt.1.0d300) fcFld=cest
        else ! CX,Y,Z
          if(Dble(cest).lt.1.0d300) call setCxyz(iEigen,cest)
        end if
        return
      end if
	    call OutTxt('t2','Fine search'C)
      call IntToStr(Int4(iabs(itmEigen)),0,0,SpaceText,lout)
		  call OutTxt('m2',SpaceText(1:lout))
      iMMPeigen=2_2
      iResCount=0
      iunitRes=iunit
      if((ifine.ne.2).and.((Dble(cest).lt.Dble(c1EigenC)).or.(Dble(cest).gt.Dble(c2EigenC)))) then ! outside clip area -> don't search
        ct=cest
        itEigen=0
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getEigen: Value outside clip cest=',cest
        iOKeigen=7
      else if((ifine.ne.1).and.((DImag(cest).lt.DImag(c1EigenC)).or.(DImag(cest).gt.DImag(c2EigenC)))) then ! outside clip area -> don't search
        ct=cest
        itEigen=0
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getEigen: Value outside clip cest=',cest
        iOKeigen=8
      else ! set search area inside clip area and start search
        ReE=Max(Dble(c1fine),Dble(c1EigenC))
        AiE=Max(DImag(c1fine),DImag(c1EigenC))
        c1fine=DCmplx(ReE,AiE)
        ReE=Min(Dble(c2fine),Dble(c2EigenC))
        AiE=Min(DImag(c2fine),DImag(c2EigenC))
        c2fine=DCmplx(ReE,AiE)
        itmE=iabs(itmEigen)
        if(ithvmax0.lt.3) itmE=-itmE
        if(l4) write(*,*) ' Start search:',c1fine,c2fine
        if((itmEigen.lt.0).and.(niEigen.gt.0)) then
          call getMin2(getRes,c1EigenC,c2EigenC,c1fine,c2fine,aEigen,fEigen,itmE,ct,dct,fkEVL,itEigen,iOKeigen)
          if(iOKeigen.eq.2) then ! MBPE problem, continue with standard search
            c1fine=ct-dct
            c2fine=ct+dct
            itEigen1=itEigen
            call getMin2a(getRes,c1EigenC,c2EigenC,c1fine,c2fine,aEigen,fEigen,itmE,ithvmax,ct,dct,fkEVL,itEigen,iOKeigen)
            itEigen=itEigen+itEigen1
          end if
        else
          call getMin2a(getRes,c1EigenC,c2EigenC,c1fine,c2fine,aEigen,fEigen,itmE,ithvmax,ct,dct,fkEVL,itEigen,iOKeigen)
        end if
      end if
      if(iOKeigen.eq.0) then ! search OK
        fPET=fPET+Dble(itEigen)
      else if(iOKeigen.eq.5) then ! outside clip area
        kPET=0
        cest=ct
        if(l4) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' Warning fine search error: Outside clip area cest=',cest
      else ! search terminated with error
        fPET=fPET+10.0d0*Dble(iabs(itmEigen))
        kPET=0
        cest=0.5d0*(c1fine+c2fine)
        de=cdabs(c1fine-c2fine)
        if(l4) then
          Select case(iOKeigen)
          case(-2)
            write(*,*) ' Warning fine search error: minimum OUTside! function flat!'
          case(-1)
            write(*,*) ' Warning fine search error: inaccurate solution! function FLAT!'
          case(1)
            write(*,*) ' Warning fine search error: inaccurate solution! iteration LIMIT reached!'
          case(2)
            write(*,*) ' Warning fine search error: minimum OUTside! iteration step small!'
          case(3)
            write(*,*) ' Warning fine search error: minimum OUTside! iteration limit reached!'
          case(4)
            write(*,*) ' Warning fine search error: MAXimum instead of minimum detected!'
          case(6)
            write(*,*) ' Warning fine search error: Capture problem! -> reduce desired accuracy!?'
          case Default
            write(*,*) ' Warning fine search error: Unknown error number iOKeigen=',iOKeigen
          end select
        end if
      end if
      iResCount=-1
      iunitRes=0
      if(l4) write(*,*) ' End search:',ct,' iter.,value,iOK=',itEigen,fkEVL,iOKeigen
	    call OutTxt('t2','End fine search'C)
      iMMPeigen=0_2
      select case(iOKeigen)
      case(-2)
	      call OutTxt('t2','outside, flat'C)
      case(-1)
	      call OutTxt('t2','inaccurate, flat'C)
      case(0)
	      call OutTxt('t2','accurate'C)
      case(1)
	      call OutTxt('t2','inacc., max.iter.'C)
      case(2)
	      call OutTxt('t2','outside, small step'C)
      case(3)
	      call OutTxt('t2','outside, max.iter.'C)
      case(4)
	      call OutTxt('t2','maximum detected'C)
      case(5,6)
	      call OutTxt('t2','outside search area'C)
      case Default
	      call OutTxt('t2','unknown search error'C)
      end select
      if(iunit.gt.0_4) then
! close the data file
        sch(1:5)=' '
        call chwrit2(iunit,ich,0,rch,0,sch,1,iOK)
        EndFile(iunit)
        close(iunit)
      end if
    else
      ct=zmin(max(1,min(imEigen,mmEigen)))
    end if
    if(iEvlDraw.gt.1_2) then
      call GetKWin(.false.)
      k1=SetActiveQQ(10+kWin)
      ic=SetColor(iEvlCol)
      call DrawX(Dble(ct),Dimag(ct),1.0d-2*(WinXmax(kWin)-WinXmin(kWin)))
      ic=SetColor(ic)
    end if
    if(iEigen.eq.1) then ! gamma
      if(Dble(ct).lt.1.0d300) gcFld=(2.0d0*Pi*kw0)*ct*fcFld
      if(l4) write(*,*) ' gamma,wav./propag.length=',gcFld,2.0d0*Pi/Dble(gcFld),0.5d0/DImag(gcFld)
      if(Dble(ct).lt.1.0d300) gcFld=ct
      dcFld=0.5d0/(gcFld*fcFld*kw0)
    else if(iEigen.lt.1) then ! frequency
      if(Dble(ct).lt.1.0d300) fcFld=ct
    else ! CX,Y,Z
      if(Dble(ct).lt.1.0d300) call setCxyz(iEigen,ct)
    end if
    resEigen=fkEVL
    if(lPET) then ! new PET parameter set
      kPET=kPET+1
      if(kPET.gt.nPET) then
        kPET=nPET
        ePET(1:2,1:nPET-1)=ePET(1:2,2:nPET)
      end if
      ePET(1,kPET)=ct
      de=cdabs(cest-ct)
      q=de/cdabs(ct)
      if(q.lt.0.1d0*aEigen) de=0.1d0*aEigen*cdabs(ct) ! search interval too short: increase
      if(ifine.gt.2) then ! set ePET(2,.) depending on search type
        ePET(2,kPET)=(2.0d0,2.0d0)*de
      else if(ifine.eq.2) then
        ePET(2,kPET)=DCmplx(0.0d0,2.0d0*de)
      else
        ePET(2,kPET)=DCmplx(2.0d0*de,0.0d0)
      end if
    end if
    if((.not.lPet).and.(abs(iOKeigen).lt.2)) resEigen=getRes(ct) ! make sure that current MMP parameters correspond to eigenvalue that was found
  end Subroutine getEigen

  Real(8) Function getRes(z,ResV)
! compute the residual (MMP solution for a fixed eigenvalue z)
    Implicit none
    Include 'resource.fd'
    Complex(8) z
    Real(8), Optional:: ResV(9)
    Real(8) rt0,rt1,rt2,rt3 !time
    Integer(4) k1,idum,lout,itime(8) !time
    Integer(2) iColExp0,iConExp0,iDomExp0,iColBnd0,iConBnd0,iDomBnd0,ic
    Character*10 datech(3) !time
    Logical ldum
    if(iEvlDraw.gt.2_2) then
      call GetKWin(.false.)
      k1=SetActiveQQ(10+kWin)
      ic=SetColor(iEvlCol)
      call DrawX(Dble(z),Dimag(z),0.5d-2*(WinXmax(kWin)-WinXmin(kWin)))
      ic=SetColor(ic)
    end if
    if((.not.lEigen).and.(iMtrSolver.eq.3)) lPET=.true.
    if(l4.and.l6) then
      write(*,*) 'Subroutine getRes, iMtr,kPET,lPET=',iMtrSolver,kPET,lPET
      call cpu_time(rt0) !time
      call date_and_time(datech(1),datech(2),datech(3),itime) !time
      rt2=3600.0d0*Dble(itime(5))+60.0d0*Dble(itime(6))+Dble(itime(7))+0.001d0*Dble(itime(8)) !time
    end if
    residual=pBig
    lMMPBndVal=.false.
    iColExp0=iColExp
    iConExp0=iConExp
    iDomExp0=iDomExp
    iColBnd0=iColBnd
    iConBnd0=iConBnd
    iDomBnd0=iDomBnd
    iColExp=0_2
    iConExp=iMMPCon
    iDomExp=0_2
    iColBnd=0_2
    iConBnd=iMMPCon
    iDomBnd=0_2
    call getEUST()
    iBound=0_2
    iMMPresV=0_2
    if(iResCount.gt.-1) then
      iResCount=iResCount+1
      call IntToStr(Int4(iResCount),0,0,SpaceText,lout)
		  call OutTxt('n2',SpaceText(1:lout))
      if(lEigen) then
        if(iMMPeigen.eq.1_2) then
          call IntToStr(Int4(nrEigen*niEigen),0,0,SpaceText,lout)
		      call OutTxt('m2',SpaceText(1:lout))
        else if(iMMPeigen.eq.2_2) then
          call IntToStr(Int4(iabs(itmEigen)),0,0,SpaceText,lout)
		      call OutTxt('m2',SpaceText(1:lout))
        end if
      end if
    end if
    if(lEigen) then
      if(iEigen.eq.1) then ! gamma
        gcFld=z
        dcFld=0.5d0/(gcFld*fcFld*kw0)
      else if(iEigen.lt.1) then ! frequency
        fcFld=z
      else ! Cx,y,z
        call setCxyz(iEigen,z)
      end if
    end if
    if(lPET.and.(.not.lEigen)) then
      if(kPET.lt.1) then
        iMtrSolver=5
      else if(iMtrSolver.ne.3) then
        iMtrSolver=2
      end if
    end if
    getRes=pBig
    if(Present(ResV)) then
      ResV(1:3)=pBig
      rMMPResV(1:3)=ResV(1:3)
    end if
    call setupMtr(ldum)
    if(l4.and.l6) then
      call cpu_time(rt1) !time
      call date_and_time(datech(1),datech(2),datech(3),itime) !time
      rt3=3600.0d0*Dble(itime(5))+60.0d0*Dble(itime(6))+Dble(itime(7))+0.001d0*Dble(itime(8)) !time
      do while(rt3.lt.rt2) 
        rt3=rt3+24.0d0*3600.0d0 ! add one day in seconds !time
      end do
      write(*,*) 'Matrix setup: CPU,elapsed time=',Real(rt1-rt0),Real(rt3-rt2),' seconds' !time
      rt0=rt1 !time
      rt2=rt3 !time
    end if
    if((.not.lStopThread).and.ldum) then
      idum=0
      if(lPET.and.(.not.lEigen)) then ! allocate PET memory if necessary
        if((kPET.lt.1).or.(npPET.ne.mCol)) then ! allocate PET memory
          npPET=mCol
          if(Allocated(pPET)) DeAllocate(pPET)
          Allocate(pPET(1:npPET,1:mPET),stat=idum)
          if(idum.ne.0) then
            idum=MessageBoxQQ('Memory allocation for MMP-PET failed!'C,'Allocate PET matrix'C, &
                              MB$OK.or.MB$IconExclamation)
            idum=1
          else
            pPET(1:npPET,1:mPET)=(0.0d0,0.0d0)
            kPET=0
          end if
        end if
      end if
      if(idum.eq.0) then
        call updateMtr()
        if(.not.lStopThread) then
          call solveMtr()
          if(lPET.and.(.not.lEigen).and.(kPET.lt.1).and.(iMtrSolver.gt.1).and.(iMtrSolver.lt.4)) then ! PET was inaccurate -> solve with Givens
            if(l4.and.l6) then
              write(*,*) 'Subroutine getRes, PET inaccurate-> restart QR'
              call cpu_time(rt1) !time
              call date_and_time(datech(1),datech(2),datech(3),itime) !time
              rt3=3600.0d0*Dble(itime(5))+60.0d0*Dble(itime(6))+Dble(itime(7))+0.001d0*Dble(itime(8)) !time
              do while(rt3.lt.rt2) 
                rt3=rt3+24.0d0*3600.0d0 ! add one day in seconds !time
              end do
              write(*,*) 'Matrix solve: CPU,elapsed time=',Real(rt1-rt0),Real(rt3-rt2),' seconds' !time
              rt0=rt1 !time
              rt2=rt3 !time
            end if
            iMtrSolver=5
            call setupMtr(ldum)
            if((.not.lStopThread).and.ldum) then
              call updateMtr()
              if(.not.lStopThread) then
                call solveMtr()
              end if
            end if
            iMtrSolver=3
          end if
          if(Present(ResV)) then
            if(iAmplTyp.eq.4) then ! new with sensor point, complex phase
              ResV(1)=cdAbs(cMMPeigenVector(1))
              ResV(2)=cdAbs(cMMPeigenVector(2))
              ResV(3)=cdAbs(cMMPeigenVector(3))
              ResV(4)=Dble(cMMPeigenVector(1))
              ResV(5)=dImag(cMMPeigenVector(1))
              ResV(6)=Dble(cMMPeigenVector(2))
              ResV(7)=dImag(cMMPeigenVector(2))
              ResV(8)=Dble(cMMPeigenVector(3))
              ResV(9)=dImag(cMMPeigenVector(3))
              iMMPresV=9_2
            else
              ResV(1)=residual
              ResV(2)=resUnscaled ! 1.0d0.div.resUnscaled
              ResV(3)=residual.div.resUnscaled ! residual/resUnscaled = 1/amplitude
              iMMPresV=3_2
              ResV(4:9)=1.0d0
              if(lMMPuseRes.and.(iMMPfindExp.gt.2)) then
                call getErrors(.false.)
                ResV(4)=vErrStat(3) ! relative error average
                ResV(5)=vErrStat(4) ! relative error maximum
                ResV(6)=vErrStat(1)/vErrStat(5) ! absolute error average / field average
                ResV(7)=vErrStat(2)/vErrStat(5) ! absolute error maximum / field average
                ResV(8)=vErrStat(1)/vErrStat(6) ! absolute error average / field maximum
                ResV(9)=vErrStat(2)/vErrStat(6) ! absolute error maximum / field maximum
                iMMPresV=9_2
              end if
            end if
            rMMPResV(1:9)=ResV(1:9)
          end if
          if(lMMPuseRes) then
            if(iMMPfindExp.ne.0) then
              if(Present(ResV)) then
                if((iMMPfindExp.gt.0).and.(iMMPfindExp.lt.10)) then
                  getRes=ResV(iMMPfindExp)
                else
                  getRes=residual
                end if
              else
                getRes=residual**(1.0d0/Dble(iMMPfindExp)) ! don't know if this helps
              end if
            else
              getRes=residual ! standard MMP minimum (scaled) residual method
            end if
          else ! don't take residual into account: MAS type eigenvalue search
            if(iMMPfindExp.ne.0) then ! complicated function might help avoiding double-peak phenomena
              getRes=((residual.div.resUnscaled)**(1.0d0/Dble(iMMPfindExp))).div.(resUnscaled**iMMPfindExp)
            else ! standard MAS: 1 / amplitude in detector point(s)
              getRes=residual.div.resUnscaled
            end if
            if(Present(ResV)) then
              ResV(1)=getRes
              rMMPResV(1)=ResV(1)
            end if
          end if
          if(iunitRes.gt.0) then
            rch(1)=dble(z)
            rch(2)=dimag(z)
            if(Present(ResV)) then
              rch(3:11)=ResV(1:9)
              call chwrit2(iunitRes,ich,0,rch(1:2),2,sch,0,idum)
              call chwrit2(iunitRes,ich,0,rch(3:5),3,sch,0,idum)
              if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
                call chwrit2(iunitRes,ich,0,rch(6:8),3,sch,0,idum)
                call chwrit2(iunitRes,ich,0,rch(9:11),3,sch,0,idum)
              end if
            else
              rch(3)=getRes
              call chwrit2(iunitRes,ich,0,rch,3,sch,0,idum)
            end if
          end if
        end if
      end if
    end if
    iColExp=iColExp0
    iConExp=iConExp0
    iDomExp=iDomExp0
    iColBnd=iColBnd0
    iConBnd=iConBnd0
    iDomBnd=iDomBnd0
    lMMPBndVal=.false.
    if(lPET.and.(.not.lEigen)) then
      iMtrSolver=3
    end if
    if(l4.and.l6) then
      call cpu_time(rt1) !time
      call date_and_time(datech(1),datech(2),datech(3),itime) !time
      rt3=3600.0d0*Dble(itime(5))+60.0d0*Dble(itime(6))+Dble(itime(7))+0.001d0*Dble(itime(8)) !time
      do while(rt3.lt.rt2) 
        rt3=rt3+24.0d0*3600.0d0 ! add one day in seconds !time
      end do
      write(*,*) 'Matrix solve: CPU,elapsed time=',Real(rt1-rt0),Real(rt3-rt2),' seconds' !time
      rt0=rt1 !time
      rt2=rt3 !time
    end if
  end Function getRes

! Minimum search algorithms

  subroutine getMin1(fk,zll0,zur0,z1,z2,z3,y1,y2,y3,io,dzr,dfr,itmax,it1,ier)
! computation of the minimum of the positive function fk(x) in the interval z1-z3
! y1-y3: values of the function in the points z1-z3.
! If one of the values y1-y3<=0, the function is called for computing the correct
! value, otherwise the value is assumed to be correct.
! The itertion is stopped when
! 1) the iteration step is small enough, i.e., step/z < dzr
! 2) the function is flat (variation/value of function < dfr)
! 3) the maximum number of iterations (itmax) has been performed
! 4) a severe error is detected (maximum of fk in the interval)
! During the iteration, the values z1-z3, y1-y3 are modified!
! it1: number of iterations performed
! ier=-2: minimum z2 outside interval z1-z3, function flat
! ier=-1: minimum z2 inaccurate, function flat
! ier= 0: minimum z2 accurate
! ier= 1: minimum z2 inaccurate, limit of iterations reached
! ier= 2: minimum z2 outside interval z1-z3, iteration step small
! ier= 3: minimum z2 outside interval z1-z3, limit of iterations reached
! ier= 4: maximum z2 detected
! ier= 5: capture reached clip border
! ier= 6: capture found too short interval
    Implicit none
    Integer(4) io,itmax,it1,ier
    Real(8), External :: fk
    Real(8) y1(9),y2(9),y3(9),y(9),qy,dzr,dfr,ydum,x2,x3,d2,dx,x,y2s,dxmax,dz,dy,ymr
    Real(8), external:: dsqrtc
    Complex(8) zll0,zur0,z,z1,z2,z3,v
    ier=0
	  it1=0
! recompute values of the function if the given values are zero
    if(y1(1).lt.1.0D-306) then
	    it1=it1+1
      if((Dble(z1).lt.Dble(zll0)).or.(Dble(z1).gt.Dble(zur0)).or.(DImag(z1).lt.DImag(zll0)).or.&
      &  (DImag(z1).gt.DImag(zur0))) then
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getMin1: First value outside clip z1=',z1
        ier=5
        return
      end if
      ydum=fk(z1,y1)
      if(lStopThread) return
	  end if
    if(y2(1).lt.1.0D-306) then
	    it1=it1+1
      if((Dble(z2).lt.Dble(zll0)).or.(Dble(z2).gt.Dble(zur0)).or.(DImag(z2).lt.DImag(zll0)).or.&
      &  (DImag(z2).gt.DImag(zur0))) then
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getMin1: Second value outside clip z2=',z2
        ier=5
        return
      end if
      ydum=fk(z2,y2)
      if(lStopThread) return
	  end if
    if(y3(1).lt.1.0D-306) then
	    it1=it1+1
      ydum=fk(z3,y3)
      if((Dble(z3).lt.Dble(zll0)).or.(Dble(z3).gt.Dble(zur0)).or.(DImag(z3).lt.DImag(zll0)).or.&
      &  (DImag(z3).gt.DImag(zur0))) then
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getMin1: Third value outside clip z3=',z3
        ier=5
        return
      end if
      if(lStopThread) return
	  end if
    call getOptY(y1,y2,y3,io)
! iteration
    do while(it1.lt.itmax)
      call getOptY(y1,y2,y3,io)
      if((y2(io).gt.y1(io)).or.(y2(io).gt.y3(io))) then ! minimum lost
        if(it1.ge.itmax) then
          ier=3
          return
        else ! try to capture a minimum
          call CaptureMin(fk,zll0,zur0,z1,z2,z3,y1,y2,y3,io,it1,itmax,ier)
          if((y2(io).gt.y1(io)).and.(y2(io).gt.y3(io))) then ! maximum
            ier=4
            return
          else if(ier.ne.0) then
            return
          end if
        end if
      end if
      v=z2-z1
      x2=cdabs(v)
      v=z3-z1
      x3=cdabs(v)
      v=v/x3
      d2=x3-x2
      dx=x2/d2
      if((dx.gt.5.0d0).and.((x2*d2).ge.0.0d0)) then
! geometric interpolation
        x=x2-dsqrtc(x2*d2)
      else
        if((dx.lt.0.2d0).and.((x2*d2).ge.0.0d0)) then
          x=x2+dsqrtc(x2*d2)
        else
          y2s=y1(io)+(y3(io)-y1(io))*x2/x3
          if(y2(io).lt.y2s) then
! parabolic interpolation
            if(dabs(y2(io)-y1(io)).gt.dabs(y2(io)-y3(io))) then
              qy=(y2(io)-y3(io))/(y2(io)-y1(io))
              x=0.5d0*(qy*(x2*x2)-x2*x2+x3*x3)/(qy*x2+d2)
            else
              qy=(y2(io)-y1(io))/(y2(io)-y3(io))
              x=-0.5d0*(qy*(x2*x2-x3*x3)-x2*x2)/(qy*d2+x2)
            end if
          else
! fixed step outside interval
            if(y1(io).lt.y3(io)) then
              x=-2.0d0*x3
            else
              x=3.0d0*x3
            end if
          end if
        end if
      end if
! avoid very small steps
      dx=dabs(x-x2)
      dxmax=0.1d0*dmin1(d2,x2)
      dxmax=dmin1(dxmax,0.4d0*dzr*cdabs(z2))
      if(dx.lt.dxmax) then
        if(x.gt.x2) then
          x=x2+dxmax
        else
          x=x2-dxmax
        end if
      end if
! set new interval depending on the location z of the minimum
      z=z1+x*v
      if((Dble(z).lt.Dble(zll0)).or.(Dble(z).gt.Dble(zur0)).or.(DImag(z).lt.DImag(zll0)).or.&
      &  (DImag(z).gt.DImag(zur0))) then
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getMin1: Search value outside clip z=',z
        ier=5
        return
      end if
      if(x.lt.0.0d0) then
        z=2.0d0*z1-z3
        z3=z2
        z2=z1
        z1=z
        y3=y2
        y2=y1
	      it1=it1+1
        ydum=fk(z1,y1)
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' getMin1: D z,f=',z1,ydum
        call getOptY(y1,y2,y3,io)
        if(lStopThread) return
      else
        if(x.lt.x2) then
	        it1=it1+1
          ydum=fk(z,y)
          if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' getMin1: E z,f=',z,ydum
          if(y(io).le.y2(io)) then
            z3=z2
            z2=z
            y3=y2
            y2=y
          else
            z1=z
            y1=y
          end if
          call getOptY(y1,y2,y3,io)
          if(lStopThread) return
        else
          if(x.le.x3) then
	          it1=it1+1
            ydum=fk(z,y)
            if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' getMin1: F z,f=',z,ydum
            if(y(io).le.y2(io)) then
              z1=z2
              z2=z
              y1=y2
              y2=y
            else
              z3=z
              y3=y
            end if
            call getOptY(y1,y2,y3,io)
            if(lStopThread) return
          else
            z=2.0d0*z3-z1
            z1=z2
            z2=z3
            z3=z
            y1=y2
            y2=y3
	          it1=it1+1
            ydum=fk(z3,y3)
            if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' getMin1: G z,f=',z3,ydum
            call getOptY(y1,y2,y3,io)
            if(lStopThread) return
          end if
        end if
      end if
      if((y2(io).gt.y1(io)).or.(y2(io).gt.y3(io))) then ! minimum lost
        if(it1.ge.itmax) then
          ier=3
          return
        else ! try to capture a minimum
          call CaptureMin(fk,zll0,zur0,z1,z2,z3,y1,y2,y3,io,it1,itmax,ier)
          if((y2(io).gt.y1(io)).and.(y2(io).gt.y3(io))) then ! maximum
            ier=4
            return
          else if(ier.ne.0) then
            return
          end if
        end if
      end if
! test accuracy of z
      dz=cdabs(z3-z1)
      if(dz/(1.0D-306+cdabs(z2)).le.dzr) then
        if((y2(io).le.y1(io)).and.(y2(io).le.y3(io))) then
          ier=0
          return
        else
          ier=2
          return
        end if
      end if
! test flat function
      dy=dmax1(dabs(y2(io)-y1(io)),dabs(y3(io)-y2(io)))
      ymr=(3.0d0*dy)/(dabs(y1(io))+dabs(y2(io))+dabs(y3(io)))
      if(ymr.le.dfr) then
        if((y2(io).le.y1(io)).and.(y2(io).le.y3(io))) then
          ier=-1
        else
          ier=-2
        end if
        return
      end if
    end do
    if((y2(io).le.y1(io)).and.(y2(io).le.y3(io))) then
      ier=1
    else
      ier=3
    end if
  end subroutine getMin1

  subroutine getOptY(y1,y2,y3,io)
    Implicit none
    Real(8) y1(9),y2(9),y3(9),q(9),qm
    Integer(4) io,i
    io=1
    if(lMMPuseRes.and.(iMMPresV.gt.0)) io=max(1,min(9,iMMPresV))
    if((iMMPresV.gt.3).and.(iMMPFindExp.ne.0)) io=min(9,iabs(iMMPFindExp))
    if((iMMPFindExp.eq.0).or.(.not.lMMP3F).or.((iMMPresV.ne.3)).and.(iAmplTyp.ne.4)) return ! routine only for 3 function strategy (double peak remedy)
    q(1:iMMPresV)=min(y1(1:iMMPresV),y3(1:iMMPresV))/y2(1:iMMPresV)
    if(q(1).gt.1.0d0) then ! y2(1) is minimum for standard search function -> trivial case
      io=1
      return
    end if
    qm=q(1) ! check if another search function has a minimum at y2
    do i=2_4,min(3_4,Int4(iMMPresV))
      if(q(i).gt.qm) then
        qm=q(i)
        io=i
      end if
    end do
    if(q(io).gt.1.0d0) return ! y2(io) is minimum for non-standard search function number io
    io=1 ! none of the search function has a local minimum, continue with standard search function
  end subroutine getOptY

  subroutine CextraClip(zll0,zur0,z1,z2,z3,iErr)
    implicit none
    Complex(8) zll0,zur0,z1,z2,z3
    Real(8) d
    Integer(4) iErr
    d=cdabs(z2-z1)
    if(d/(1.0d-306+cdabs(z2)).le.1.0d-14) then
      if(l4.and.l5) write(*,*) ' CextraClip error 6'
      iErr=6
      return
    end if
    z3=z2+(z2-z1)
    iErr=0
    if(Dble(zll0).gt.Dble(z3)) z3=DCmplx(Dble(zll0),DImag(z3))
    if(Dble(zur0).lt.Dble(z3)) z3=DCmplx(Dble(zur0),DImag(z3))
    if(DImag(zll0).gt.DImag(z3)) z3=DCmplx(Dble(z3),DImag(zll0))
    if(DImag(zur0).lt.DImag(z3)) z3=DCmplx(Dble(z3),DImag(zur0))
    d=cdabs(z3-z1)
    if(d/(1.0d-306+cdabs(z2)).le.1.0d-14) then
      if(l4.and.l5) write(*,*) ' CextraClip error 5'
      iErr=5
      return
    end if
    d=cdabs(z3-z2)
    if(d/(1.0d-306+cdabs(z2)).le.1.0d-14) then
      if(l4.and.l5) write(*,*) ' CextraClip error 5'
      iErr=5
      return
    end if
  end subroutine CextraClip

  subroutine CaptureMin(fk,zll0,zur0,z1,z2,z3,y1,y2,y3,io,it1,itmax,iErr)
    Implicit none
    Integer(4) io,it1,itmax,ierr
    Real(8), External :: fk
    Real(8) y1(9),y2(9),y3(9),y1a(9),y3a(9),ydum
    Complex(8) zll0,zur0,z1,z2,z3,z1a,z3a
    do while(it1.lt.itmax-1)
      call getOptY(y1,y2,y3,io)
      if((y3(io).gt.y2(io)).and.(y1(io).gt.y2(io))) then
        iErr=0
        return
      end if
      if(y1(io).lt.y3(io)) then ! search left side first
	      it1=it1+1
        call CextraClip(zll0,zur0,z3,z1,z1a,iErr)
        if(iErr.ne.0) return
        ydum=fk(z1a,y1a)
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' CaptureMin: A z,f=',z1a,ydum
        call getOptY(y1a,y1,y2,io)
        if((y1a(io).gt.y1(io)).and.(y2(io).gt.y1(io))) then ! left minimum found
          iErr=0
          z3=z2
          z2=z1
          z1=z1a
          y3=y2
          y2=y1
          y1=y1a
          return
        end if
	      it1=it1+1
        call CextraClip(zll0,zur0,z1,z3,z3a,iErr)
        if(iErr.ne.0) return
        ydum=fk(z3a,y3a)
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' CaptureMin: B z,f=',z3a,ydum
        call getOptY(y2,y3,y3a,io)
        if((y3a(io).gt.y3(io)).and.(y2(io).gt.y3(io))) then ! right minimum found
          iErr=0
          z1=z2
          z2=z3
          z3=z3a
          y1=y2
          y2=y3
          y3=y3a
          return
        end if
      else ! search right side first
	      it1=it1+1
        call CextraClip(zll0,zur0,z1,z3,z3a,iErr)
        if(iErr.ne.0) return
        ydum=fk(z3a,y3a)
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' CaptureMin: C z,f=',z3a,ydum
        call getOptY(y2,y3,y3a,io)
        if((y3a(io).gt.y3(io)).and.(y2(io).gt.y3(io))) then ! right minimum found
          iErr=0
          z1=z2
          z2=z3
          z3=z3a
          y1=y2
          y2=y3
          y3=y3a
          return
        end if
	      it1=it1+1
        call CextraClip(zll0,zur0,z3,z1,z1a,iErr)
        if(iErr.ne.0) return
        ydum=fk(z1a,y1a)
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' CaptureMin: D z,f=',z1a,ydum
        call getOptY(y1a,y1,y2,io)
        if((y1a(io).gt.y1(io)).and.(y2(io).gt.y1(io))) then ! left minimum found
          iErr=0
          z3=z2
          z2=z1
          z1=z1a
          y3=y2
          y2=y1
          y1=y1a
          return
        end if
      end if
! search minima for all 5 points
      call getOptY(y1,y3,y3a,io)
      if((y3a(io).gt.y3(io)).and.(y1(io).gt.y3(io))) then ! minimum found
        iErr=0
        z2=z3
        z3=z3a
        y2=y3
        y3=y3a
        return
      end if
      call getOptY(y1,y2,y3a,io)
      if((y3a(io).gt.y2(io)).and.(y1(io).gt.y2(io))) then ! minimum found
        iErr=0
        z3=z3a
        y3=y3a
        return
      end if
      call getOptY(y1a,y2,y3,io)
      if((y3(io).gt.y2(io)).and.(y1a(io).gt.y2(io))) then ! minimum found
        iErr=0
        z1=z1a
        y1=y1a
        return
      end if
      call getOptY(y1a,y1,y3,io)
      if((y3(io).gt.y1(io)).and.(y1a(io).gt.y1(io))) then ! minimum found
        iErr=0
        z1=z1a
        z2=z1
        y1=y1a
        y2=y1
        return
      end if
      call getOptY(y1a,y2,y3a,io)
      if((y3a(io).gt.y2(io)).and.(y1a(io).gt.y2(io))) then ! minimum found
        iErr=0
        z1=z1a
        z3=z3a
        y1=y1a
        y3=y3a
        return
      end if
      call getOptY(y1a,y1,y3a,io)
      if((y3a(io).gt.y1(io)).and.(y1a(io).gt.y1(io))) then ! minimum found
        iErr=0
        z1=z1a
        z2=z1
        z3=z3a
        y1=y1a
        y2=y1
        y3=y3a
        return
      end if
      call getOptY(y1a,y3,y3a,io)
      if((y3a(io).gt.y3(io)).and.(y1a(io).gt.y3(io))) then ! minimum found
        iErr=0
        z1=z1a
        z2=z3
        z3=z3a
        y1=y1a
        y2=y3
        y3=y3a
        return
      end if
      if(y3a(io).gt.y1a(io)) then ! no minimum found -> steepest descent
        z3=z2
        z2=z1
        z1=z1a
        y3=y2
        y2=y1
        y1=y1a
      else
        z1=z2
        z2=z3
        z3=z3a
        y1=y2
        y2=y3
        y3=y3a
      end if
      iErr=1
      if(lStopThread) return
    end do
  end subroutine CaptureMin

  subroutine getMin2(fk,zll0,zur0,zll,zur,dzr,dfr,itmax,zm,dzm,ym0,it,ier)
! computation of the minimum of the function fk(z) in the area
! with the lower left corner zll and the upper right corner zur
! in the complex plane
    Implicit none
    Integer(4) itmax,it,ier,i,im
    Real(8), External :: fk
    Real(8) dzr,dfr,ym(9),ym0,a,dy,ymr,am
    Real(8) fa(0:5),fan
    Complex(8) zll0,zur0,zll,zur,zm,zn,dzm
    it=0
    zm=0.5d0*(zll+zur)
	  a=0.35d0*cdAbs(zll-zur) ! approx. diagonal/(2*sqrt(2))
    dzm=dCmplx(a,a)
    nMBPE=5
    zMBPE(1)=zm
    ym0=fk(zMBPE(1),ym)
    fMBPE(1)=dCmplx(ym(4),ym(5))
    it=it+1
    do while(itmax.gt.it+4)
      am=a
      a=0.25d0*a ! 5 point MBPE also good extrapolation
      zMBPE(2)=zm-dCmplx(a,0.0d0)
      ym0=fk(zMBPE(2),ym)
      fMBPE(2)=dCmplx(ym(4),ym(5))
      zMBPE(3)=zm+dCmplx(a,0.0d0)
      ym0=fk(zMBPE(3),ym)
      fMBPE(3)=dCmplx(ym(4),ym(5))
      zMBPE(4)=zm-dCmplx(0.0d0,a)
      ym0=fk(zMBPE(4),ym)
      fMBPE(4)=dCmplx(ym(4),ym(5))
      zMBPE(5)=zm+dCmplx(0.0d0,a)
      ym0=fk(zMBPE(5),ym)
      fMBPE(5)=dCmplx(ym(4),ym(5))
      fa(0)=pBig
      im=1
      do i=1,5
        fa(i)=cdAbs(fMBPE(i))
        if(fa(0).gt.fa(i)) then
          fa(0)=fa(i)
          im=i
        end if
      end do
      if(l4.and.l5) write(*,*) 'Best of 5: n,f=',im,fa(0)
      it=it+4
      ier=1
      dy=dmax1(cdabs(fMBPE(1)-fMBPE(2)),cdabs(fMBPE(1)-fMBPE(3)), &
      &        cdabs(fMBPE(1)-fMBPE(4)),cdabs(fMBPE(1)-fMBPE(5)))
      ymr=(5.0d0*dy).div.(cdAbs(fMBPE(1))+cdAbs(fMBPE(2))+cdAbs(fMBPE(3))+cdAbs(fMBPE(4))+cdAbs(fMBPE(5)))
      if(ymr.le.dfr) then
        if((cdAbs(fMBPE(1)).lt.cdAbs(fMBPE(2))).and.(cdAbs(fMBPE(1)).lt.cdAbs(fMBPE(3))).and. &
        &  (cdAbs(fMBPE(1)).lt.cdAbs(fMBPE(4))).and.(cdAbs(fMBPE(1)).lt.cdAbs(fMBPE(5)))) then
          ier=-1
        else
          ier=-2
        end if
      end if
      nMBPE=5
      call cMBPEsearch2O(61_4,6.0d0*cdabs(zMBPE(1)-zMBPE(2)),zn) ! search also surrounding area
      if(iMBPEerr.ne.0) return ! cMBPEsearch2O error
      if((ier.lt.0).and.(cdAbs(zm-zn).gt.4.0d0*a)) return ! flat area had been detected -> return when it runs away
      a=cdAbs(zm-zn)
      zm=zn
      dzm=dCmplx(a,a)
      zn=cMBPEapprox2O(zm)
      zMBPE(1)=zm
      ym0=fk(zMBPE(1),ym)
      it=it+1
      fMBPE(1)=dCmplx(ym(4),ym(5))
      fan=cdAbs(fMBPE(1))
      if(fan.gt.fa(0)) then
        if(l4.and.l5) write(*,*) 'MBPE warning: quality decreased! fold,fnew,fapprox=',fa(0),fan,cdAbs(zn)
        ier=2
        zm=zMBPE(im)
        a=am
        dzm=dCmplx(a,a)
        return
      end if
      if(a/(1.0D-306+cdabs(zm)).le.dzr) then ! accurate
        ier=0
        return
      end if
      if((it.ge.itmax).or.lStopThread) then ! itmax reached
        ier=1
        return
      end if
      if((Dble(zm).lt.Dble(zll0)).or.(Dble(zm).gt.Dble(zur0)).or.(DImag(zm).lt.DImag(zll0)).or.(DImag(zm).gt.DImag(zur0))) then ! outside clip area
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getMin2: Value outside clip zm=',zm
        ier=5
        return
      end if
    end do
    ier=1
  end subroutine getMin2

  subroutine getMin2a(fk,zll0,zur0,zll,zur,dzr,dfr,itmax,ithvmax,zm,dzm,ym1,it,ier)
! computation of the minimum of the function fk(z) in the area
! with the lower left corner zll and the upper right corner zur
! in the complex plane
! the itertion is stopped when
! 1) the iteration step is small enough, i.e., step/z < dzr
! 2) the function is flat (variation/value of function < dfr)
! 3) the maximum number of iterations (itmax) has been performed
! 4) a severe error is detected (maximum of fk in the interval)
! 5) the search runs outside the rectangle with the corners zll0,zur0
! in each iteration getMin1 is called in horizontal and vertical
! direction with the maximum number of iterations ithvmax
! zm: location in the complex plane where the iteration stopped
! ym=fk(zm)
! it: number of iterations performed
! ier=-2: minimum zm outside interval z1-z3, function flat
! ier=-1: minimum zm inaccurate, function flat
! ier= 0: minimum zm accurate
! ier= 1: minimum zm inaccurate, limit of iterations reached
! ier= 2: minimum zm outside interval z1-z3, iteration step small
! ier= 3: minimum zm outside interval z1-z3, limit of iterations reached
! ier= 4: maximum zm detected
! ier= 5: outside search area
! ier= 6: capture problem
    Implicit none
    Integer(4) itmax,ithvmax,it,ier,ithvm,io,it1,ier1
    Real(8), External :: fk
    Real(8) yl(9),ym(9),yr(9),yu(9),yo(9),dzr,dfr,ym1,azm,rzm,al,ar,ao,au,dz,dy,ymr
    Complex(8) zll0,zur0,zll,zur,zm,zl,zr,zo,zu,dzm
    if(lEvlSMP) then
      call SMP2D(fk,zll,zur,dzr,dfr,iabs(itmax),zm,dzm,it,ier)
      return
    end if
    it=0
    zm=0.5d0*(zll+zur)
    azm=0.0d0
    rzm=0.0d0
    dzm=(0.0d0,0.0d0)
    ym(1:9)=0.0d0
    ym1=ym(1)
    zl=DCmplx(Dble(zll),DImag(zm))
    zr=DCmplx(Dble(zur),DImag(zm))
    zo=DCmplx(Dble(zm),DImag(zur))
    zu=DCmplx(Dble(zm),DImag(zll))
	  al=0.99999999d0*Dabs(Dble(zm)-Dble(zl))
	  ar=0.99999999d0*Dabs(Dble(zm)-Dble(zr))
	  ao=0.99999999d0*Dabs(DImag(zm)-DImag(zo))
	  au=0.99999999d0*Dabs(DImag(zm)-DImag(zu))
    ithvm=min(ithvmax,itmax-it)
    do while(ithvm.gt.2)
! hor. search
      if((Dabs(Dble(zl)-Dble(zr)).gt.pSmall).and.(ithvm.gt.2)) then
	      yl(1:3)=0.0d0
	      yr(1:3)=0.0d0
        do
          zl=zm-al
          zr=zm+ar
          ithvm=min(ithvmax,itmax-it)
          if(ithvm.lt.3) exit
          call getMin1(fk,zll0,zur0,zl,zm,zr,yl,ym,yr,io,dzr,dfr,ithvm,it1,ier1)
          it=it+it1
          if(l4) write(*,*) 'horizontal search minimum location,ierror:',zm,ier1
          ym1=ym(1)
          rzm=cdabs(zr-zl)
          dzm=DCmplx(rzm,azm)
          if((ier1.gt.3).or.lStopThread) then
            ier=ier1
            return
          end if
	        al=Dabs(Dble(zm)-Dble(zl))
	        ar=Dabs(Dble(zm)-Dble(zr))
          if(it.ge.itmax) then
            ier=1
            if((ier1.eq.-2).or.(ier1.eq.2).or.(ier1.eq.3)) ier=3
            return
          end if
          if((ier1.le.1).and.(ier1.ge.-1)) Exit
 ! try to capture minimum -> increase search interval
          if(l4) write(*,*) 'increasing horizontal search interval'
	        ar=ar+al
	        al=ar
	        if((yl(io).lt.ym(io)).and.(yl(io).le.yr(io))) then
            zm=zl
            ym(1:3)=yl(1:3)
            ym1=ym(1)
 	          yl(1:3)=0.0d0
	        end if
	        if(yr(io).lt.ym(io)) then
            zm=zr
            ym(1:3)=yr(1:3)
            ym1=ym(1)
 	          yr(1:3)=0.0d0
	        end if
        end do
      end if
! ver. search
      if((Dabs(DImag(zu)-DImag(zo)).gt.pSmall).and.(ithvm.gt.2)) then
	      yu(1:3)=0.0d0
	      yo(1:3)=0.0d0
        do
          zu=zm-(0.0d0,1.0d0)*au
          zo=zm+(0.0d0,1.0d0)*ao
          ithvm=min(ithvmax,itmax-it)
          if(ithvm.lt.3) exit
          call getMin1(fk,zll0,zur0,zu,zm,zo,yu,ym,yo,io,dzr,dfr,ithvm,it1,ier1)
          it=it+it1
          if(l4) write(*,*) 'vertical search minimum location,ierror:',zm,ier1
          ym1=ym(1)
          azm=cdabs(zo-zu)
          dzm=DCmplx(rzm,azm)
          if((ier1.gt.3).or.lStopThread) then
            ier=ier1
            return
          end if
	        ao=Dabs(DImag(zm)-DImag(zo))
	        au=Dabs(DImag(zm)-DImag(zu))
          if(it.ge.itmax) then
            ier=1
            if((ier1.eq.-2).or.(ier1.eq.2).or.(ier1.eq.3)) ier=3
            return
          end if
          if((ier1.le.1).and.(ier1.ge.-1)) Exit
          if((Dble(zm).lt.Dble(zll0)).or.(Dble(zm).gt.Dble(zur0)).or.(DImag(zm).lt.DImag(zll0)).or.&
          &  (DImag(zm).gt.DImag(zur0))) then
            if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3)') ' getMin2a: Value outside clip zm=',zm
            ier=5
            return
          end if
 ! try to capture minimum -> increase search interval
          if(l4) write(*,*) 'increasing vertical search interval'
	        ao=ao+au
	        au=ao
	        if((yu(io).lt.ym(io)).and.(yu(io).le.yo(io))) then
            zm=zu
            ym(1:3)=yu(1:3)
            ym1=ym(1)
 	          yu(1:3)=0.0d0
	        end if
	        if(yo(io).lt.ym(io)) then
            zm=zo
            ym(1:3)=yo(1:3)
            ym1=ym(1)
 	          yo(1:3)=0.0d0
	        end if
        end do
	    end if
! test
      if((Dble(zm).lt.Dble(zll0)).or.(Dble(zm).gt.Dble(zur0)).or.(DImag(zm).lt.DImag(zll0)).or.(DImag(zm).gt.DImag(zur0))) then
        ier=5
        return
      end if
      dz=0.5d0*(cdabs(zo-zu)+cdabs(zr-zl))
      if(dz/(1.0D-306+cdabs(zm)).le.dzr) then
        ier=0
        if((ier1.eq.-2).or.(ier1.eq.2).or.(ier1.eq.3)) ier=2
        return
      end if
      dy=dmax1(dabs(yl(io)-ym(io)),dabs(yr(io)-ym(io)),dabs(yu(io)-ym(io)),dabs(yo(io)-ym(io)))
      ymr=(5.0d0*dy)/(dabs(yl(io))+dabs(ym(io))+dabs(yr(io))+dabs(yu(io))+dabs(yo(io)))
      if(ymr.le.dfr) then
        ier=-1
        if((ier1.eq.-2).or.(ier1.eq.2).or.(ier1.eq.3)) ier=-2
        return
      end if
    end do
    ier=1
    if((ier1.eq.-2).or.(ier1.eq.2).or.(ier1.eq.3)) ier=3
  end subroutine getMin2a

  Subroutine wricfk(fk,zll,zur,nh,nv,iunit,nmin,mmin,zmin,dzmin,fkmin)
! 1) Write the real valued function fk(z) of the complex argument z
! in the rectangle (in the complex plane) with the lower left corner
! zll and the upper right corner zur in the complex plane on the
! output file (unit iunit).
! 2) Store the minima of fk(z) on zmin.
! iunit<1: do not write data on file
! nh = number of horizontal (real direction) grid lines; nh<3 : no search, set zmin=(zll+zur)/2
! nv = number of vertical (imaginary direction) grid lines; nv<2 : search along line
! mmin = number of detected minima 
! zmin = location of the minima
! dzmin = distance of zmin to neighbour grid point
! fkmin = fk(zmin(nmin))
! Notes: If mmin exceeds 100, only 100 minima are stored and mmin=100 is returned.
!        The rectangular area is replaced by a line if nv<2
    Implicit none
    Integer(4) nh,nv,iunit,nmin,mmin,mmina(9),nmi,idum,i,j,k,iErr,ia,i0
    Real(8) fv(9),fkmin,fkz,dhor,dver,y,x,dx,dy
    Complex(8) zll,zur,zmin(100),dzmin(100),zmina(9,100),z,dz,zmin0
    Real(8), allocatable:: fa(:,:,:)
    Complex(8), allocatable:: za(:,:)
    Real(8), External :: fk
    mmin=0
    if(nh.lt.3) then
      zmin(nmin)=0.5d0*(zll+zur)
      dzmin(nmin)=0.5d0*(zur-zll)
      fkmin=fk(zmin,fv)
      if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3)') ' wricfk: A z,f=',zmin,fkmin
      return
    end if
    nmi=0
    zmin(1)=zll
    dzmin(1)=zur-zll
    fkmin=pBig
    zmin0=zll
    ia=3
    if(lMMPuseRes.and.(iMMPfindExp.gt.2)) ia=9 ! number of search function values
    if(nv.lt.2) then
! search on line
      Deallocate(za,fa,Stat=iErr)
      Allocate(za(nh,1),fa(0:ia,nh,1),Stat=iErr)
		  if(iErr.ne.0) then
        idum=MessageBoxQQ('Memory alloction for rough search failed!'C,'subroutine wricfk'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        Deallocate(za,fa,Stat=iErr)
			  return
		  end if
      if(iunit.gt.0_4) then
        if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
          call WriteStr(iunit,' CHFUN Version 1.0'C,idum)
          call WriteStr(iunit,'0 11'C,idum)
          call WriteStr(iunit,'n rough search'C,idum)
          call WriteStr(iunit,'Real(z)'C,idum)
          call WriteStr(iunit,'Imag(z)'C,idum)
          call WriteStr(iunit,'SearchFunction1(z)'C,idum)
          call WriteStr(iunit,'SearchFunction2(z)'C,idum)
          call WriteStr(iunit,'SearchFunction3(z)'C,idum)
          call WriteStr(iunit,'SearchFunction4(z)'C,idum)
          call WriteStr(iunit,'SearchFunction5(z)'C,idum)
          call WriteStr(iunit,'SearchFunction6(z)'C,idum)
          call WriteStr(iunit,'SearchFunction7(z)'C,idum)
          call WriteStr(iunit,'SearchFunction8(z)'C,idum)
          call WriteStr(iunit,'SearchFunction9(z)'C,idum)
        else
          call WriteStr(iunit,' CHFUN Version 1.0'C,idum)
          call WriteStr(iunit,'0 5'C,idum)
          call WriteStr(iunit,'n rough search'C,idum)
          call WriteStr(iunit,'Real(z)'C,idum)
          call WriteStr(iunit,'Imag(z)'C,idum)
          call WriteStr(iunit,'SearchFunction1(z)'C,idum)
          call WriteStr(iunit,'SearchFunction2(z)'C,idum)
          call WriteStr(iunit,'SearchFunction3(z)'C,idum)
        end if
	    end if
! evaluate and store search functions
      dz=(zur-zll)/dble(max(1,nh-1))
      z=zll-dz
      do i=1,nh
        if(lStopThread) then
          Deallocate(za,fa,Stat=iErr)
          return
        end if
        z=z+dz
        fkz=fk(z,fv)
        if(fkz.lt.fkmin) then
          fkmin=fkz
          zmin0=z
        end if
        za(i,1)=z
        fa(0,i,1)=fkz
        fa(1:ia,i,1)=fv(1:ia)
        if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3,2H, 1PE11.3)') ' wricfk: B z,f,fmin=',z,fkz,fkmin
        if(iunit.gt.0_4) then
          rch(1)=dble(z)
          rch(2)=dimag(z)
          rch(3:11)=fv(1:9)
          call chwrit2(iunit,ich,0,rch(1:2),2,sch,0,idum)
          call chwrit2(iunit,ich,0,rch(3:5),3,sch,0,idum)
          if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
            call chwrit2(iunit,ich,0,rch(6:8),3,sch,0,idum)
            call chwrit2(iunit,ich,0,rch(9:11),3,sch,0,idum)
          end if
        end if
      end do
      zmin(1)=zmin0 ! location of lowest fkz value (to be used when no minimum detected)
! detect minima of search function s
      if((ia.eq.9).and.lMMP3F) then
        zmina=(1.1d300,1.1d300)
        do i=4,ia
          if((iMMPfindExp.eq.4).and.(i.ne.6)) Cycle !!! iMMPfindExp 4 and 6 seem to be most reliable -> focus on them
          if((i.eq.iMMPfindExp).or.(i.eq.5)) Cycle
          if(l4.and.l5) write(*,*) 'Search function ',i
          nmi=0
          call RoughSearch1(nh,za(1:nh,1),fa(i,1:nh,1),dz,nmi,mmina(i),zmina(i,1:100),dzmin,l5)
          if(mmina(i).gt.100) then
            write(*,*) 'Warning: >100 minima found in RoughSearch1! Search function number=',i
            mmina(i)=100
          end if
        end do
      end if
      if(l4.and.l5) write(*,*) 'Search function 0'
      nmi=0
      call RoughSearch1(nh,za(1:nh,1),fa(0,1:nh,1),dz,nmi,mmin,zmin,dzmin,.true.)
      if(mmin.gt.100) then
        write(*,*) 'Warning: >100 minima found in RoughSearch1! Search function number 0'
        mmin=100
      end if
      if((ia.eq.9).and.lMMP3F) then ! check if other minima are close
        do i=1,mmin
          dy=0.0d0
          do k=4,9
            if((k.eq.iMMPfindExp).or.(k.eq.5)) Cycle
            dx=1.1d300
            do j=1,min(mmina(k),100_4)
              dx=min(dx,cdabs(zmin(i)-zmina(k,j))) ! smallest distance from any of the zmina(k,.)
            end do
         !   dy=max(dy,dx) ! maximum of the smallest distances
            dy=min(dy,dx) ! minimum of the smallest distances
          end do
          if(dy.gt.cdabs(dzmin(i))) then ! none of the additional search functions has a minimum near zmin(i) -> mark to elliminate
            zmin(i)=(1.1d300,1.1d300)
          end if
        end do
        i0=0 ! eliminate minima that failed the check
        do i=1,mmin
          if(Dble(zmin(i)).lt.pBig) then
            i0=i0+1
            if(i0.gt.100_4) Exit
            zmina(1,i0)=zmin(i)
          end if
        end do
        i0=min(i0,100_4)
        nmi=i0
        mmin=i0
        zmin(1:i0)=zmina(1,1:i0)
        if(l4) then 
          write(*,*) 'approved minima:',mmin
          do i=1,mmin
            write(*,*) i,zmin(i)
          end do
        end if
      end if
    else
! search in rectangle
      Deallocate(za,fa,Stat=iErr)
      Allocate(za(nh,nv),fa(0:ia,nh,nv),Stat=iErr)
		  if(iErr.ne.0) then
        idum=MessageBoxQQ('Memory alloction for rough search failed!'C,'subroutine wricfk'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        Deallocate(za,fa,Stat=iErr)
			  return
		  end if
      if(iunit.gt.0_4) then
        if(lWriteRoughFld) then
          call WriteStr(iunit,' CHFLD Version 1.0'C,idum)
          call WriteStr(iunit,'1 representation data'C,idum)
          call WriteStr(iunit,'1 0 0 0 1  lxrFld,...'C,idum)
          call WriteStr(iunit,'0 1 0  min/max rFld, time'C,idum)
          call WriteStr(iunit,'3 1 1E10 view...'C,idum)
          call WriteStr(iunit,'0 0 0  origin'C,idum)
          call WriteStr(iunit,'1 0 0  x tang.'C,idum)
          call WriteStr(iunit,'0 1 0  y tang.'C,idum)
          call WriteStr(iunit,'0 0 -1  z tang.'C,idum)
          call WriteStr(iunit,'3 0 0 1 16 115  intensity plot'C,idum)
          call WriteStr(iunit,'1 -1.1 0'C,idum)
          call WriteStr(iunit,'0 1 1 216 235  arrow plot'C,idum)
          call WriteStr(iunit,'1E1 0.1'C,idum)
          if(lgcFld) then
            call WriteStr(iunit,'1 1 1 1 1 0 0 1 1 1  lxcFld,..'C,idum)
          else
            call WriteStr(iunit,'1 1 1 1 1 0 0 1 0 1  lxcFld,..'C,idum)
          end if
          ich(1)=nh
          ich(2)=nv
          ich(3)=1
          sch(1:10)=' nxcFld,..'
          call chwrit2(iunit,ich,3,rch,0,sch,10,idum)
          rch(1)=dble(zll)
          rch(2)=dimag(zll)
          rch(3)=0.0d0
          sch(1:7)=' origin'
          call chwrit2(iunit,ich,0,rch,3,sch,7,idum)
          rch(1)=dble(zur)-dble(zll)
          rch(2)=0.0d0
          rch(3)=0.0d0
          sch(1:8)=' x tang.'
          call chwrit2(iunit,ich,0,rch,3,sch,8,idum)
          rch(1)=0.0d0
          rch(2)=dimag(zur)-dimag(zll)
          rch(3)=0.0d0
          sch(1:8)=' y tang.'
          call chwrit2(iunit,ich,0,rch,3,sch,7,idum)
          call WriteStr(iunit,'0 0 1  z tang.'C,idum)
          call WriteStr(iunit,'1  Field values'C,idum)
        else
          if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
            call WriteStr(iunit,' CHFUN Version 1.0'C,idum)
            call WriteStr(iunit,'0 11'C,idum)
            call WriteStr(iunit,'n rough search'C,idum)
            call WriteStr(iunit,'Real(z)'C,idum)
            call WriteStr(iunit,'Imag(z)'C,idum)
            call WriteStr(iunit,'SearchFunction1(z)'C,idum)
            call WriteStr(iunit,'SearchFunction2(z)'C,idum)
            call WriteStr(iunit,'SearchFunction3(z)'C,idum)
            call WriteStr(iunit,'SearchFunction4(z)'C,idum)
            call WriteStr(iunit,'SearchFunction5(z)'C,idum)
            call WriteStr(iunit,'SearchFunction6(z)'C,idum)
            call WriteStr(iunit,'SearchFunction7(z)'C,idum)
            call WriteStr(iunit,'SearchFunction8(z)'C,idum)
            call WriteStr(iunit,'SearchFunction9(z)'C,idum)
          else
            call WriteStr(iunit,' CHFUN Version 1.0'C,idum)
            call WriteStr(iunit,'0 5'C,idum)
            call WriteStr(iunit,'n rough search'C,idum)
            call WriteStr(iunit,'Real(z)'C,idum)
            call WriteStr(iunit,'Imag(z)'C,idum)
            call WriteStr(iunit,'SearchFunction1(z)'C,idum)
            call WriteStr(iunit,'SearchFunction2(z)'C,idum)
            call WriteStr(iunit,'SearchFunction3(z)'C,idum)
          end if
        end if
      end if
! evaluate and store search functions
      dhor=(dble(zur)-dble(zll))/dble(nh-1)
      dver=(dimag(zur)-dimag(zll))/dble(nv-1)
      dx=dabs(dhor)
      dy=dabs(dver)
      y=dimag(zll)-dver
      if((nxcFld.ne.nh).or.(nycFld.ne.nv).or.(nzcFld.ne.1)) iGetEigFld=0
      do k=1,nv
        y=y+dver
        x=dble(zll)-dhor
        do i=1,nh
          if(lStopThread) then
            Deallocate(za,fa,Stat=iErr)
            return
          end if
          x=x+dhor
          z=dcmplx(x,y)
          if(iGetEigFld.gt.0) then ! read from cFld
            fv(1)=Dble(cFld(1,i,k,1))
            fv(2)=Dble(cFld(2,i,k,1))
            fv(3)=Dble(cFld(3,i,k,1))
            fv(4)=Dble(cFld(4,i,k,1))
            fv(5)=Dimag(cFld(4,i,k,1))
            fv(6)=Dble(cFld(5,i,k,1))
            fv(7)=Dimag(cFld(5,i,k,1))
            fv(8)=Dble(cFld(6,i,k,1))
            fv(9)=Dimag(cFld(6,i,k,1))
            fkz=fv(min(max(iGetEigFld,1),9))
            if(fkz.lt.fkmin) then
              fkmin=fkz
              zmin0=z
            end if
          else
            fkz=fk(z,fv)
            if(fkz.lt.fkmin) then
              fkmin=fkz
              zmin0=z
            end if
            if(l4.and.l5) write(*,'(A,1PE11.3,3H+i*,1PE11.3,2H, 1PE11.3,2H, 1PE11.3)') ' wricfk: C z,f,fmin=',z,fkz,fkmin
          end if
          za(i,k)=z
          fa(0,i,k)=fkz
          fa(1:ia,i,k)=fv(1:ia)
          if(iunit.gt.0_4) then
            if(lWriteRoughFld) then
              call WriteStr(iunit,'1'C,idum)
              rch(1:12)=0.0d0
              rch(1)=fv(1)
              rch(3)=fv(2)
              rch(5)=fv(3)
              if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
                rch(7:12)=fv(4:9)
              end if
              call chwrit2(iunit,ich,0,rch(1:6),6,sch,0,idum)
              call chwrit2(iunit,ich,0,rch(7:12),6,sch,0,idum)
            else
              rch(1)=dble(z)
              rch(2)=dimag(z)
              rch(3:11)=fv(1:9)
              call chwrit2(iunit,ich,0,rch(1:2),2,sch,0,idum)
              call chwrit2(iunit,ich,0,rch(3:5),3,sch,0,idum)
              if((lMMPuseRes.and.(iMMPfindExp.gt.2)).or.(iAmplTyp.eq.4)) then
                call chwrit2(iunit,ich,0,rch(6:8),3,sch,0,idum)
                call chwrit2(iunit,ich,0,rch(9:11),3,sch,0,idum)
              end if
            end if
	        end if
        end do
      end do
      zmin(1)=zmin0 ! location of lowest fkz value (to be used when no minimum detected)
      iGetEigFld=0
! search minima
      if((ia.eq.9).and.lMMP3F) then
        zmina=(1.1d300,1.1d300)
        do i=1,ia
          if((iMMPfindExp.eq.4).and.(i.ne.6)) Cycle !!! iMMPfindExp 4 and 6 seem to be most reliable -> focus on them
          if((i.eq.iMMPfindExp).or.(i.eq.2).or.(i.eq.3).or.(i.eq.5)) Cycle
          if(l4.and.l5) write(*,*) 'Search function ',i
          nmi=0
          call RoughSearch2(nh,nv,za(1:nh,1:nv),fa(i,1:nh,1:nv),dx,dy,nmi,mmina(i),zmina(i,1:100),dzmin,l5)
          if(mmina(i).gt.100_4) then
            write(*,*) 'Warning: >100 minima found in RoughSearch2! Search function number=',i
            mmina(i)=100_4
          end if
        end do
      end if
      if(l4.and.l5) write(*,*) 'Search function 0'
      nmi=0
      call RoughSearch2(nh,nv,za(1:nh,1:nv),fa(0,1:nh,1:nv),dx,dy,nmi,mmin,zmin,dzmin,.true.)
      if(mmin.gt.100) then
        write(*,*) 'Warning: >100 minima found in RoughSearch2! Search function number 0'
        mmin=100
      end if
      if((ia.eq.9).and.lMMP3F) then ! check if other minima are close
        do i=1,mmin
          dy=1.1d300 !!!0.0d0
          do k=4,9
            if((k.eq.iMMPfindExp).or.(k.eq.5)) Cycle
            dx=1.1d300
            do j=1,min(mmina(k),100_4)
              dx=min(dx,cdabs(zmin(i)-zmina(k,j))) ! samllest distance from any of the zmina(k,.)
            end do
          !!!  dy=max(dy,dx) ! maximum of the smallest distances
            dy=min(dy,dx) ! minimum of the smallest distances
          end do
          if(dy.gt.cdabs(dzmin(i))) then ! none of the additional search functions has a minimum near zmin(i) -> mark to elliminate
            zmin(i)=(1.1d300,1.1d300)
          end if
        end do
        i0=0 ! eliminate minima that failed the check
        do i=1,mmin,1
          if(Dble(zmin(i)).lt.pBig) then
            i0=i0+1
            if(i0.gt.100_4) Exit
            zmina(1,i0)=zmin(i)
          end if
        end do
        i0=min(i0,100_4)
        nmi=i0
        mmin=i0
        zmin(1:i0)=zmina(1,1:i0)
        if(l4) then 
          write(*,*) 'approved minima:',mmin
          do i=1,mmin
            write(*,*) i,zmin(i)
          end do
        end if
      end if
    end if
    mmin=min(100,mmin)
    if(l4) write(*,'(A,1I3,A)') 'Rough search found ',nmi,' possible eigenvalues'
    Deallocate(za,fa,Stat=iErr)
  end Subroutine wricfk

  Subroutine RoughSearch1(nh,za,fa,dz,nmi,mmin,zmin,dzmin,lout)
! detect minima of function fa(za), stored in nh points za
    Implicit none
    Logical lout
    Integer(4) nh,nmi,mmin,i
    Real(8) fa(nh),fk1,fk2,fk3,x,dx,dy
    Complex(8) za(nh),dz,zmin(100),dzmin(100),z
    fk1=fa(1)
    fk2=fa(2)
    fk3=fa(3)
    mmin=0
    dx=Dble(dz)
    dy=Dimag(dz)
    if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for first interval (local coord. x=-1,0,1 assumed)
      x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
      if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
        z=za(2)+x*dz
        nmi=nmi+1
        mmin=nmi
        if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
        &      Dble(z),Dimag(z),' estimated value=',fk2
        if(mmin.le.100) then
          zmin(mmin)=z
          dzmin(mmin)=dz !max(abs(x),1.0d0-abs(x))*dz
        end if
      end if
    end if
    do i=2,nh-1 ! inner minima
      fk1=fa(i-1)
      fk2=fa(i)
      fk3=fa(i+1)
      if((fk1.gt.fk2).and.(fk3.gt.fk2)) then
        x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2) ! parabolic interpolation
        z=za(i)+x*dz
        if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
          nmi=nmi+1
          mmin=nmi
          if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
          &      Dble(z),Dimag(z),' estimated value=',fk2
          if(mmin.le.100) then
            zmin(mmin)=z
            dzmin(mmin)=dz !max(abs(x),1.0d0-abs(x))*dz
          end if
        end if
      end if
    end do
    fk1=fa(nh)
    fk2=fa(nh-1)
    fk3=fa(nh-2)
    if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for last interval (inverse x direction)
      x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
      if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
        z=za(nh-1)-x*dz
        if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
          nmi=nmi+1
          mmin=nmi
          if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
          &      Dble(z),Dimag(z),' estimated value=',fk2
          if(mmin.le.100) then
            zmin(mmin)=z
            dzmin(mmin)=dz !max(abs(x),1.0d0-abs(x))*dz
          end if
        end if
      end if
    end if
  end Subroutine RoughSearch1

  Subroutine RoughSearch2(nh,nv,za,fa,dx,dy,nmi,mmin,zmin,dzmin,lout)
! detect minima of function fa(za), stored in nh*nv points za
    Implicit none
    Logical lout
    Integer(2) icl,icb
    Integer(4) nh,nv,i,k,nmi,mmin,nCo,nRo,iWork,ierr,k1
    Real(8) fa(nh,nv),fkz,fk1,fk2,fk3,fk4,x,y,dx,dy,Matr(9,6),Resu(9,1),Work(300),r1,r2,d1,d2,a,el
    Complex(8) za(nh,nv),zmin(100),dzmin(100),z
! first row
    fk1=fa(1,1) ! first zell
    fk2=fa(2,1)
    fk3=fa(3,1)
    mmin=0
    if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for first interval (local coord. x=-1,0,1 assumed)
      x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
      if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
        fkz=fa(1,2)
        fk3=fa(1,1)
        fk4=fa(1,3)
        y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
        if(((y.gt.-1.0d0).and.(y.le.0.0d0)).or.((nv.eq.3).and.(y.gt.-1.0d0).and.(y.le.1.0d0))) then
          z=za(2,2)+DCmplx(x*dx,y*dy)
          nmi=nmi+1
          mmin=nmi
          if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
          &      Dble(z),Dimag(z),' estimated value=',fk2
          if(mmin.le.100) then
            zmin(mmin)=z
            dzmin(mmin)=Dcmplx(dx,dy) !max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
          end if
        end if
      end if
    end if
    do i=2,nh-1 ! inner minima
      fk1=fa(i-1,1)
      fk2=fa(i,1)
      fk3=fa(i+1,1)
      if((fk1.gt.fk2).and.(fk3.gt.fk2)) then
        x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2) ! parabolic interpolation
        if((x.gt.-1.0d0).and.(x.le.0.0d0)) then
          fkz=fa(i,2)
          fk3=fa(i,1)
          fk4=fa(i,3)
          y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
          if(((y.gt.-1.0d0).and.(y.le.0.0d0)).or.((nv.eq.3).and.(y.gt.-1.0d0).and.(y.le.1.0d0))) then
            z=za(i,2)+DCmplx(x*dx,y*dy)
            if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
              nmi=nmi+1
              mmin=nmi
              if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
              &      Dble(z),Dimag(z),' estimated value=',fk2
              if(mmin.le.100) then
                zmin(mmin)=z
                dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
              end if
            end if
          end if
        end if
      end if
    end do
    fk1=fa(nh,1) ! last zell
    fk2=fa(nh-1,1)
    fk3=fa(nh-2,1)
    if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for last interval (inverse x direction)
      x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
      if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
        fkz=fa(nh,2)
        fk3=fa(nh,1)
        fk4=fa(nh,3)
        y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
        if(((y.gt.-1.0d0).and.(y.le.0.0d0)).or.((nv.eq.3).and.(y.gt.-1.0d0).and.(y.le.1.0d0))) then
          z=za(nh-1,2)+DCmplx(-x*dx,y*dy)
          if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
            nmi=nmi+1
            mmin=nmi
            if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
            &      Dble(z),Dimag(z),' estimated value=',fk2
            if(mmin.le.100) then
              zmin(mmin)=z
              dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
            end if
          end if
        end if
      end if
    end if
! inner rows
    do k=2,nv-1
      fk1=fa(1,k)
      fk2=fa(2,k)
      fk3=fa(3,k)
      if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for first interval (local coord. x=-1,0,1 assumed)
        x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
        if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
          fkz=fk1
          fk3=fa(1,k-1)
          fk4=fa(1,k+1)
          y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
          if((y.ge.-1.0d0).and.(y.le.1.0d0)) then
            z=za(2,k)+DCmplx(x*dx,y*dy)
            if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
              nmi=nmi+1
              mmin=nmi
              if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
              &      Dble(z),Dimag(z),' estimated value=',fk2
              if(mmin.le.100) then
                zmin(mmin)=z
                dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
              end if
            end if
          end if
        end if
      end if
      do i=2,nh-1 ! inner minima
        fkz=fa(i,k)
        fk1=fa(i-1,k)
        fk2=fa(i+1,k)
        fk3=fa(i,k-1)
        fk4=fa(i,k+1)
        if((fk1.gt.fkz).and.(fk2.gt.fkz).and.(fk3.gt.fkz).and.(fk4.gt.fkz)) then
          x=0.5d0*(fk1-fk2).div.(fk1+fk2-2.0d0*fkz) ! hor. parabolic interpolation
          y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
          z=za(i,k)+DCmplx(x*dx,y*dy)
          if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
            nmi=nmi+1
            mmin=nmi
            if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
            &      Dble(z),Dimag(z),' estimated value=',fkz
            if(mmin.le.100) then
              zmin(mmin)=z
              dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
            end if
            nCo=6 ! 2nd Order requires matrix with 6 columns
            nRo=9 ! center point and 8 nearest neighbors
            iWork=300 ! should be enough!
            Matr(1,1)=1.0 ! set up 3x3 matrix for 9 points
            Matr(1,2)=1.0
            Matr(1,3)=1.0
            Matr(1,4)=-1.0
            Matr(1,5)=-1.0
            Matr(1,6)=1.0
            Matr(2,1)=0.0
            Matr(2,2)=1.0
            Matr(2,3)=0.0
            Matr(2,4)=0.0
            Matr(2,5)=-1.0
            Matr(2,6)=1.0
            Matr(3,1)=1.
            Matr(3,2)=1.0
            Matr(3,3)=-1.0
            Matr(3,4)=1.0
            Matr(3,5)=-1.0
            Matr(3,6)=1.0
            Matr(4,1)=1.0
            Matr(4,2)=0.0
            Matr(4,3)=0.0
            Matr(4,4)=-1.0
            Matr(4,5)=0.0
            Matr(4,6)=1.0
            Matr(5,1)=0.0
            Matr(5,2)=0.0
            Matr(5,3)=0.0
            Matr(5,4)=0.0
            Matr(5,5)=0.0
            Matr(5,6)=1.0
            Matr(6,1)=1.0
            Matr(6,2)=0.0
            Matr(6,3)=0.0
            Matr(6,4)=1.0
            Matr(6,5)=0.0
            Matr(6,6)=1.0
            Matr(7,1)=1.0
            Matr(7,2)=1.0
            Matr(7,3)=-1.0
            Matr(7,4)=-1.0
            Matr(7,5)=1.0
            Matr(7,6)=1.0
            Matr(8,1)=0.0
            Matr(8,2)=1.0
            Matr(8,3)=0.0
            Matr(8,4)=0.0
            Matr(8,5)=1.0
            Matr(8,6)=1.0
            Matr(9,1)=1.0
            Matr(9,2)=1.0
            Matr(9,3)=1.0
            Matr(9,4)=1.0
            Matr(9,5)=1.0
            Matr(9,6)=1.0
            Resu(1,1)=fa(i-1,k-1)! set up Resu (right hand side -> result)
            Resu(2,1)=fa(i,k-1)
            Resu(3,1)=fa(i+1,k-1)
            Resu(4,1)=fa(i-1,k)
            Resu(5,1)=fa(i,k)
            Resu(6,1)=fa(i+1,k)
            Resu(7,1)=fa(i-1,k+1)
            Resu(8,1)=fa(i,k+1)
            Resu(9,1)=fa(i+1,k+1)
            call DGELS('N',nRo,nCo,1,Matr,nRo,Resu,nRo,Work,iWork,ierr) ! solve
            if(ierr.ne.0) then
              write(*,*) 'Rough eigenvalue search QR decomposition failed!'
            else
              icl=3
              icb=1
              x=(Resu(5,1)*Resu(3,1)-2.0d0*Resu(2,1)*Resu(4,1)).div.(Resu(3,1)**2-4.0d0*Resu(1,1)*Resu(2,1))
              y=(Resu(4,1)*Resu(3,1)-2.0d0*Resu(1,1)*Resu(5,1)).div.(Resu(3,1)**2-4.0d0*Resu(1,1)*Resu(2,1))
              call getParabRD(Resu(1,1),Resu(2,1),Resu(3,1),Resu(4,1),Resu(5,1),x,y,r1,r2,d1,d2)
              if((abs(x).gt.1.0).or.(abs(y).gt.1.0)) then ! first check: minimum outside square
                icl=4
                if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' outside square!'
              end if
              if((r1.lt.0.0d0).or.(r2.lt.0.0d0)) then ! second check: negative radius of curvature in the point x,y
                icb=0
                if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' negative curvature!'
              end if
              z=za(i,k)+DCmplx(x*dx,y*dy)
              a=atan(d1*dy/dx)
              el=max(dabs(r1.div.r2),dabs(r2.div.r1))
              if(el.gt.10.0d0) then ! third check: ellipticity
                if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' very elliptic!'
                if(icl.eq.3) then
                  icl=5
                else
                  icl=2
                end if
              end if 
              el=dlog(el+1.0d0)
              if(iEvlDraw.gt.0_2) then ! graphic output of solutions found: Arrow with border color icb (white: neg.curvature), fill color icf (green:OK) 
                call GetKWin(.false.)
                k1=SetActiveQQ(10+kWin)
                call DrawArrow(Dble(z),Dimag(z),Dble(z)+el*dx*dcos(a),Dimag(z)+el*dx*dsin(a),2.0d0*dx,3_2, &
                & .true.,icb,icl)
              end if
              if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' refined', &
              & Dble(z),Dimag(z),' interpolated value=',(Resu(1,1)-Resu(2,1))/Resu(3,1)
              select case(iEvlTest) ! most likely useless minima -> delete
              case(1)
                if(icb.eq.0) then
                  if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' deleted!'
                  nmi=nmi-1
                  mmin=nmi
                end if
              case(2)
                if((icb.eq.0).or.(icl.eq.2)) then
                  if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' deleted!'
                  nmi=nmi-1
                  mmin=nmi
                end if
              case(3)
                if((icb.eq.0).or.(icl.eq.2).or.(icl.eq.4)) then
                  if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' deleted!'
                  nmi=nmi-1
                  mmin=nmi
                end if
              case(4)
                if((icb.eq.0).or.(icl.eq.2).or.(icl.eq.4).or.(icl.eq.5)) then
                  if(l4.and.lout) write(*,*) ' Minimum #',Int2(nmi),' deleted!'
                  nmi=nmi-1
                  mmin=nmi
                end if
              end select
            end if
          end if
        end if
      end do
      fk1=fa(nh,k)
      fk2=fa(nh-1,k)
      fk3=fa(nh-2,k)
      if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for last interval (inverse x direction)
        x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
        if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
          fkz=fk1
          fk3=fa(nh,k-1)
          fk4=fa(nh,k+1)
          y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
          if((y.ge.-1.0d0).and.(y.le.1.0d0)) then
            z=za(nh-1,k)+DCmplx(-x*dx,y*dy)
            if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
              nmi=nmi+1
              mmin=nmi
              if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
              &      Dble(z),Dimag(z),' estimated value=',fk2
              if(mmin.le.100) then
                zmin(mmin)=z
                dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
              end if
            end if
          end if
        end if
      end if
    end do
! last row
    fk1=fa(1,nv) ! first zell
    fk2=fa(2,nv)
    fk3=fa(3,nv)
    if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for first interval (local coord. x=-1,0,1 assumed)
      x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
      if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
        fkz=fa(1,nv-1)
        fk3=fa(1,nv)
        fk4=fa(1,nv-2)
        y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
        if(((y.gt.-1.0d0).and.(y.le.0.0d0)).or.((nv.eq.3).and.(y.gt.-1.0d0).and.(y.le.1.0d0))) then
          z=za(2,nv-1)+DCmplx(x*dx,-y*dy)
          if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
            nmi=nmi+1
            mmin=nmi
            if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
            &      Dble(z),Dimag(z),' estimated value=',fk2
            if(mmin.le.100) then
              zmin(mmin)=z
              dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
            end if
          end if
        end if
      end if
    end if
    do i=2,nh-1 ! inner minima
      fk1=fa(i-1,nv)
      fk2=fa(i,nv)
      fk3=fa(i+1,nv)
      if((fk1.gt.fk2).and.(fk3.gt.fk2)) then
        x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2) ! parabolic interpolation
        if((x.gt.-1.0d0).and.(x.le.0.0d0)) then
          fkz=fa(i,nv-1)
          fk3=fa(i,nv)
          fk4=fa(i,nv-2)
          y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
          if(((y.gt.-1.0d0).and.(y.le.0.0d0)).or.((nv.eq.3).and.(y.gt.-1.0d0).and.(y.le.1.0d0))) then
            z=za(i,nv-1)+DCmplx(x*dx,-y*dy)
            if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
              nmi=nmi+1
              mmin=nmi
              if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
              &      Dble(z),Dimag(z),' estimated value=',fk2
              if(mmin.le.100) then
                zmin(mmin)=z
                dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
              end if
            end if
          end if
        end if
      end if
    end do
    fk1=fa(nh,nv) ! last zell
    fk2=fa(nh-1,nv)
    fk3=fa(nh-2,nv)
    if((fk1.lt.fk2).and.(fk2.lt.fk3)) then ! parabolic interpolation for last interval (inverse x direction)
      x=0.5d0*(fk1-fk3).div.(fk1+fk3-2.0d0*fk2)
      if(((x.gt.-1.0d0).and.(x.le.0.0d0)).or.((nh.eq.3).and.(x.gt.-1.0d0).and.(x.le.1.0d0))) then
        fkz=fa(nh,nv-1)
        fk3=fa(nh,nv)
        fk4=fa(nh,nv-2)
        y=0.5d0*(fk3-fk4).div.(fk3+fk4-2.0d0*fkz) ! ver. parabolic interpolation
        if(((y.gt.-1.0d0).and.(y.le.0.0d0)).or.((nv.eq.3).and.(y.gt.-1.0d0).and.(y.le.1.0d0))) then
          z=za(nh-1,nv-1)+DCmplx(-x*dx,-y*dy)
          if(.not.lExistingMin(z,zmin,mmin,dx,dy)) then
            nmi=nmi+1
            mmin=nmi
            if(l4.and.lout) write(*,'(A,1I3,A,1PE15.7,3H +i,1PE15.7,A,1PE10.2)') ' Minimum #',nmi,' near', &
            &      Dble(z),Dimag(z),' estimated value=',fk2
            if(mmin.le.100) then
              zmin(mmin)=z
              dzmin(mmin)=Dcmplx(dx,dy) !Dcmplx(max(abs(x),1.0d0-abs(x))*dx,max(abs(y),1.0d0-abs(y))*dy)
            end if
          end if
        end if
      end if
    end if
  end Subroutine RoughSearch2
  
  Logical Function lExistingMin(z,zmin,mmin,dx,dy)
    Integer(4) mmin,i
    Real(8) dx,dy
    Complex(8) z,zmin(100),dz
    lExistingMin=.false.
    if(mmin.lt.1) return
    do i=1,mmin
      dz=z-zmin(i)
      if((Dble(dz).lt.dx).and.(Dimag(dz).lt.dy)) then
        lExistingMin=.true.
        Exit
      end if
    end do
  end Function lExistingMin

  Subroutine SolveQuadratic(a,b,c,x1,x2)
! solve quadratic equation a*x*x+b*x+c=0
    Implicit none
    Real(8) a,b,c,x1,x2,d
    d=b**2-4.0d0*a*c
    if(d.ge.0.0d0) then
      d=dsqrt(d)
      x1=(-b+d)/(2.0d0*a)
      x2=(-b-d)/(2.0d0*a)
    else
      x1=1.1d300
      x2=-1.1d300
    end if
  end Subroutine SolveQuadratic

  Subroutine getParabRD(a,b,c,d,e,x,y,r1,r2,d1,d2)
! given parabolic function in the xy-plane: z=a*x*x+b*y*y+c*x*y+d*x+e*y+f
! compute main radii of curvature r1,r2 and orientations d1,d2 (dx/dy) in the point (x,y)
    Implicit none
    Real(8) a,b,c,d,e,x,y,r1,r2,d1,d2,p,q,r,s,t,h,aa,bb,cc
    p=2.0d0*a*x+c*y+d
    q=2.0d0*b*y+c*x+e
    r=2.0d0*a
    s=c
    t=2.0d0*b
    h=dsqrt(1.0d0+p*p+q*q)
    aa=r*t-s*s
    bb=h*(2.0d0*p*q*s-(1+p*p)*t-(1.0d0+q*q)*r)
    cc=h**4
    call SolveQuadratic(aa,bb,cc,r1,r2)
    aa=t*p*q-s*(1.0d0+q*q)
    bb=t*(1.0d0+p*p)-r*(1.0d0+q*q)
    cc=s*(1.0d0+p*p)-r*p*q
    call SolveQuadratic(aa,bb,cc,d1,d2)
  end Subroutine getParabRD

! PET extrapolations

  Subroutine crExtra(mpts,mfun,npts0,nfun0,nord0,x,f,w,str,x0,f0,ier)
! extrapolation with nord functions of nfun real functions fi(x), x real
! fi is known in npts points (stored in the array x)
! f: array containing the fi in the points x in the following order:
! w: weighting function stored in an array
! str: array of strings containing the basis functions
! result: f0 = estimated value of f in a given point x0
! ier: error flag
    Implicit none
    Complex(8), Allocatable :: a(:),z(:),s(:)
    Real(8), Allocatable :: c(:)
    Integer(4) nord0,npts0,nfun0,nord,npts,nfun,ier,i,k,l,km,km1,ma,idum,mfun,mpts
    Complex(8) f(mfun,mpts),f0(mfun)
    Real(8) x(mpts),w(mpts),x0
    Character(1151) str(mpts)
    npts=min(npts0,mpts)
    nfun=min(nfun0,mfun)
    nord=min(nord0,npts-1)
    if(npts.lt.1) then
      idum=MessageBoxQQ('ERROR: No extrapolation points!'C,'Extrapolate'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      ier=1
			f0(1:nfun)=0.0d0
      return
    end if
    if(nord.lt.0) then
      idum=MessageBoxQQ('WARNING: Negative extrapolation order set to zero!'C,'Extrapolate'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      nord=0
    end if
    if(nfun.lt.1) then
      ier=2
      idum=MessageBoxQQ('ERROR: Nothing to extrapolate!'C,'Extrapolate'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
			return
    end if
    km=nord+1
    km1=km+1
    ma=(km1*km)/2+nfun*km1
    if(Allocated(a)) DeAllocate(a)
    if(Allocated(z)) DeAllocate(z)
    if(Allocated(c)) DeAllocate(c)
    if(Allocated(s)) DeAllocate(s)
    Allocate(a(ma),z((nord+3)*nfun),c(nord+3),s(nord+3),stat=ier)
		if(ier.ne.0) then
      ier=-1
      idum=MessageBoxQQ('ERROR: Memory alloction for extrapolation failed!'C,'Extrapolate'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
			f0(1:nfun)=f(1:nfun,npts)
			return
		end if
    ier=0
    a=(0.0d0,0.0d0)
! compute and update matrix a
    do i=1,npts
      do k=1,km
! value of basis function
        l=-1
        vCForm(1)=DCmplx(x(i),0.0d0)
        z(k:k)=CFormula(str(k),l,cCForm,pCForm,vCForm,0,0,1,1,1,1,idum)
        z(k)=w(i)*z(k)
      end do
      do l=1,nfun
        z(km+l)=-w(i)*f(l,i)
      end do
      call cchud(a,z,km,nfun,i,c,s,ma)
    end do
! solve a*z=e
    call cchsl(a,z,km,nfun,km,ma)
! estimate f0=z(1)*ifun(1)(arg1,arg2)+....z(nord)*ifun(nord)(arg1,arg2)
    do l=1,nfun
      f0(l)=0.0
    end do
    do i=1,km
      l=-1
      vCForm(1)=DCmplx(x0,0.0d0)
      cFormu=CFormula(str(i),l,cCForm,pCForm,vCForm,0,0,1,1,1,1,idum)
      do l=1,nfun
        f0(l)=f0(l)+z(i+(l-1)*km)*cFormu(1)
      end do
    end do
  end Subroutine crExtra

  Subroutine ScaleConnection(kCon,iT,kBnd1,kBnd2,x1,x2,y1,y2,s)
    Implicit none
    Integer(4), Intent(in):: kCon,iT,kBnd1,kBnd2
    Real(8), Intent(in):: x1,x2,y1,y2,s
    Integer(4) j,k,kC,kK,kE,kD,kP
    Real(8) f
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    f=0.0d0
    kC=kConFFD
    kConFFD=kCon
    kK=kColFFD
    kColFFD=0
    kE=kExpFFD
    kExpFFD=0
    kD=kDomFFD
    kDomFFD=0
    kP=kParFFD
    kParFFD=0
    lAskQ=.false.
    if(iT.lt.2) then
      do k=kBnd1,kBnd2
        iBndInt=k
        call BndIntg(.true.)
        f=f+currentIntegral
      end do
    else if(iT.eq.2) then
      XminInt=x1
      XmaxInt=x2
      YminInt=y1
      YmaxInt=y2
      IntNx=kBnd1
      IntNy=kBnd2
      call RecIntg(.true.)
      f=f+currentIntegral
    else
      iIntgEc=0_2
      iIntgHc=0_2
      do k=kBnd1,kBnd2
        iObjInt=k
        call ObjIntg(.true.)
        f=f+currentIntegral
      end do
    end if
    lAskQ=.true.
    kConFFD=kC
    kColFFD=kK
    kExpFFD=kE
    kDomFFD=kD
    kParFFD=kP
    f=abs(f)
    if(f.lt.1.0d-100) return
    f=s/f
    do k=1,nExp
      if(tExp(k)%iConn.ne.kCon) Cycle
      do j=1+tExp(k)%iOff,tExp(k)%iOff+tExp(k)%nPar
        ParExp(1:nRHS,j)=f*ParExp(1:nRHS,j)
      end do
    end do
  end Subroutine ScaleConnection

  Subroutine setCxyz(i,ct)
    Implicit none
    Integer(4) i
    Complex(8) ct
    if(i.eq.2) then ! CX
      CxPeriod=ct
      CxPeriodDLG=ct
    else if(i.eq.3) then ! CY
      CyPeriod=ct
      CyPeriodDLG=ct
    else if(i.eq.4) then ! CZ
      CzPeriod=ct
      CzPeriodDLG=ct
    else if(i.eq.5) then ! CX+Y
      CxPeriod=ct
      CxPeriodDLG=ct
      CyPeriod=ct
      CyPeriodDLG=ct
    else if(i.eq.6) then ! CX+Z
      CxPeriod=ct
      CxPeriodDLG=ct
    else if(i.eq.7) then ! CY+Z
      CyPeriod=ct
      CyPeriodDLG=ct
      CzPeriod=ct
      CzPeriodDLG=ct
    else if(i.eq.8) then ! CX+Y+Z
      CxPeriod=ct
      CxPeriodDLG=ct
      CyPeriod=ct
      CyPeriodDLG=ct
      CzPeriod=ct
      CzPeriodDLG=ct
    end if
  end Subroutine setCxyz

END MODULE CHMMP
