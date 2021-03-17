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
MODULE CHFUN

! Function evaluation

  USE CHBDE

  SAVE

  Character(20), Parameter :: CHFunIdent=' CHFUN Version 1.3  '
  Real(8), Allocatable :: Fun(:,:),hFun(:,:)
  Character(64), Allocatable :: FunATitle(:),hFunATitle(:)
  Real(8) rFunOff,rFunSca
  Integer(4) nFun,nFunA,mFun,mFunA,iFunA1,iFunA2,iFunA3,iFunA4,iSaveFunction,iFunUnit,iIntFunUnit,iFunFunUnit, &
  & nDiff,iFunKol,iFunRow,nFunKol,nFunRow,iFun1,iFun2
  Integer(2) iFunMarkColor,iFunStyleColor,iFunMarkStep,iFunStyleStep
  Integer(2) iFunPolar,iFunMarkQ,iFunMarkP,iFunMarkX,iFunMarkSize, &
             iFunStyleP,iFunStyleL,iFunStyleD,iFunStyleWidth
  Logical    lAppFun,lSkipFun,lSkipFunHead,lAskFun
  Character(256) FunFileName,InfFileName
  Character(64) sdum64
  Character(1151) string_Pro,string_Opr,DiffWeight

  CONTAINS

  Subroutine Functions_Defaults(lCheck)
! set default data of functions
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    call setNameExt(ProFileName,'FUN',FunFileName)
    InfFileName='MAXINT000.FUN'C
    iSaveFunction=0
    iFunUnit=2_4
    iFunFunUnit=2_4
    iIntFunUnit=4_4
    lAskFun=.true.
    lSkipFun=.false.
    lSkipFunHead=.false.
	  mFun=100
	  nFun=100
    mFunA=1
    nFunA=1
    rFunOff=0.0d0
    rFunSca=1.0d0
    iFunA1=0
    iFunA2=1
    iFunA3=0
    iFunA4=0
    iFun1=1
    iFun2=nFun
    iFunKol=0
    iFunRow=0
    nFunKol=0
    nFunRow=0
    iFunPolar=0_2
    iFunMarkQ=0_2
    iFunMarkP=0_2
    iFunMarkX=0_2
    iFunMarkSize=4_2
    iFunMarkStep=10_2
    iFunMarkColor=-1_2
    iFunStyleP=1_2
    iFunStyleL=0_2
    iFunStyleD=0_2
    iFunStyleWidth=1_2
    iFunStyleStep=1_2
    iFunStyleColor=1_2
    nDiff=2_4
    string_Pro='exp(v1)'C
    string_Opr='v1'C
    DiffWeight='exp(neg(sqr(div(v0,p0))))'C
    call AllocateFun(ldum)
  end Subroutine Functions_Defaults

! Allocations

  Subroutine AllocateFun(ldum)
    Implicit none
    Integer(4) i,lout,idum
    Logical ldum
    Character(4) text
    ldum=.false.
    mFun=Max(2,mFun)
    mFunA=Max(1,mFunA)
    lAppFun=.false.
    if(Allocated(FunATitle)) DeAllocate(FunATitle)
    Allocate(FunATitle(0:mFunA),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for FunATitle failed!'C,'Allocate function'C, &
                        MB$OK.or.MB$IconExclamation)
      nFun=0
      nFunA=0
      mFun=0
      mFunA=0
      return
    end if
    do i=0,mFunA
      call IntToStr(i,3,0,text,lout)
      FunATitle(i)(1:16)='Argument number '
      FunATitle(i)(17:16+lout)=text(1:lout)
      FunATitle(i)(17+lout:17+lout)=Char(0)
    end do
    if(Allocated(Fun)) DeAllocate(Fun)
    Allocate(Fun(mFunA,mFun),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for Fun failed!'C,'Allocate function'C, &
                        MB$OK.or.MB$IconExclamation)
      nFun=0
      nFunA=0
      mFun=0
      mFunA=0
      return
    end if
    Fun=0.0d0
    ldum=.true.
    nFun=min(nFun,mFun)
    if(nFun.lt.1) nFun=mFun
    nFunA=min(nFunA,mFunA)
	end Subroutine AllocateFun

  Subroutine IncreaseFun(inc,incA,ldum)
    Implicit none
    Integer(4) i,lout,inc,incA,idum,mF,mFA
    Logical ldum
    Character(4) text
    ldum=.false.
    mF=mFun
    if(inc.lt.0) mF=mF+inc
    if(mF.lt.2) then
      idum=MessageBoxQQ('Function with less than 2 values not allowed!'C,'Increase functions'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    mFA=mFunA
    if(incA.lt.0) mFA=mFA+incA
    if(mFA.lt.1) then
      idum=MessageBoxQQ('Function with less than 1 argument not allowed!'C,'Increase functions'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    if(Allocated(hFunATitle)) DeAllocate(hFunATitle)
    Allocate(hFunATitle(0:mFA),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Increase functions'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    hFunATitle(0:mFA)=FunATitle(0:mFA)
    ldum=Allocated(FunATitle)
    if(ldum) then
      DeAllocate(FunATitle)
    end if
    ldum=.false.
    Allocate(FunATitle(0:mFunA+incA),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Increase functions'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    FunATitle(0:mFA)=hFunATitle(0:mFA)
    if(incA.gt.0) then
      do i=mFunA+1,mFunA+incA
        call IntToStr(i,3,0,text,lout)
        FunATitle(i)(1:16)='Argument number '
        FunATitle(i)(17:16+lout)=text(1:lout)
        FunATitle(i)(17+lout:17+lout)=Char(0)
      end do
    end if
    if(Allocated(hFunATitle)) DeAllocate(hFunATitle)
    if(Allocated(hFun)) DeAllocate(hFun)
    Allocate(hFun(mFA,mF),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Increase functions'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    hFun(1:mFA,1:mF)=Fun(1:mFA,1:mF)
    if(Allocated(Fun)) DeAllocate(Fun)
    Allocate(Fun(mFunA+incA,mFun+inc),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Increase functions'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    Fun=0.0d0
    Fun(1:mFA,1:mF)=hFun(1:mFA,1:mF)
    if(Allocated(hFun)) DeAllocate(hFun)
    mFun=mFun+inc
    mFunA=mFunA+incA
    nFun=min(nFun,mFun)
    if(nFun.lt.1) nFun=mFun
    nFunA=min(nFunA,mFunA)
    ldum=.true.
	end Subroutine IncreaseFun

! I/O

  Subroutine SaveFunction(lCheck,lIntFile)
! save all current functions in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lIntFile
	  Integer(4) i,j,k,jk,mA,mAR,iOK,ios,idum
    Logical(4) lopen
    Character(256) FileName
    if(lIntFile) then
      FileName=InfFileName
      iFunUnit=iIntFunUnit
    else
      FileName=FunFileName
      iFunUnit=iFunFunUnit
    end if
    if(iSaveFunction.eq.0) iFunUnit=2_4
    if((iSaveFunction.eq.0).or.(iSaveFunction.eq.-1)) then
      if(lAskFun) then
        idum=MessageBoxQQ('Save function representation data?'C,'Save function'C, &
                          MB$YesNo.or.MB$IconQuestion)
        lSkipFunHead=.true.
        if(idum.eq.MB$IDYES) lSkipFunHead=.false.
        idum=MessageBoxQQ('Save function values?'C,'Save function'C, &
                          MB$YesNo.or.MB$IconQuestion)
        lSkipFun=.true.
        if(idum.eq.MB$IDYES) lSkipFun=.false.
      end if
      if(.not.lCheck) then
        call Open2write(-1,'Select function data file to be written!','Function data file ',FileName,'FUN',ios)
        if(ios.gt.0) then
	        call closeFunction(iFunUnit)
          return
        end if
      end if
      if(iSaveFunction.eq.-1) then
        do iFunUnit=11_4,99_4
          inquire(unit=iFunUnit,opened=lopen)
          if(.not.lopen) Exit
        end do
        if(lopen) then
          idum=MessageBoxQQ('Units 11-99 are open!\rCannot save function data!'C,'Save functions'C, &
                            MB$OK.or.MB$IconExclamation)
	        call closeFunction(iFunUnit)
          return
        end if
        if(lIntFile) then
          iIntFunUnit=iFunUnit
        else
          iFunFunUnit=iFunUnit
        end if
      end if
      open(iFunUnit,file=FileName,iostat=ios)
      if(ios.ne.0) then
        write(*,*) 'Error opening file ',FileName
        write(*,*) 'Unit=',iFunUnit,' Status=',ios
        idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save functions'C, &
                          MB$OK.or.MB$IconExclamation)
	      call closeFunction(iFunUnit)
        return
      end if
      call WriteStr(iFunUnit,CHFunIdent,iOK)
      ich(1)=1
      if(lSkipFunHead) ich(1)=0
      ich(2)=nFunA
      call chwrit2(iFunUnit,ich,2,rch,0,sch,0,iOK)
      if(.not.lSkipFunHead) then
        ich(1)=iFunA1
        ich(2)=iFunA2
        ich(3)=iFunA3
        ich(4)=iFunA4
        ich(5)=iFun1
        ich(6)=iFun2
        call chwrit2(iFunUnit,ich,6,rch,0,sch,0,iOK)
        ich(1)=iFunMarkQ
        ich(2)=iFunMarkP
        ich(3)=iFunMarkX
        ich(4)=iFunMarkSize
        ich(5)=iFunMarkStep
        ich(6)=iFunMarkColor
        call chwrit2(iFunUnit,ich,6,rch,0,sch,0,iOK)
        ich(1)=iFunStyleP
        ich(2)=iFunStyleL
        ich(3)=iFunStyleD
        ich(4)=iFunStyleWidth
        ich(5)=iFunStyleStep
        ich(6)=iFunStyleColor
        call chwrit2(iFunUnit,ich,6,rch,0,sch,0,iOK)
        ich(1)=iFunPolar
        rch(1)=rFunOff
        rch(2)=rFunSca
        call chwrit2(iFunUnit,ich,1,rch,2,sch,0,iOK)
      end if
    end if
    if(iSaveFunction.eq.0) then
      do i=0,nFunA
        call WriteStr(iFunUnit,FunATitle(i),iOK)
      end do
    else if(iSaveFunction.eq.-2) then
      i=GetSLength(sch)
      call WriteStr(iFunUnit,sch(1:i),iOK)
    end if
    if(iSaveFunction.eq.0) then
      if(.not.lSkipFun) then
        mA=nFunA/6
        mAR=nFunA-6*mA
        do i=1,nFun
          jk=0
          do j=1,mA
            do k=1,6
              jk=jk+1
              rch(k)=Fun(jk,i)
            end do
            call chwrit2(iFunUnit,ich,0,rch,6,sch,0,iOK)
          end do
          if(mAR.gt.0) then
            do k=1,mAR
              jk=jk+1
              rch(k)=Fun(jk,i)
            end do
            call chwrit2(iFunUnit,ich,0,rch,mAR,sch,0,iOK)
          end if
        end do
      end if
    else if(iSaveFunction.gt.0) then
      call chwrit2(iFunUnit,ich,0,rch,iSaveFunction,sch,0,iOK)
    end if
    if((iSaveFunction.eq.0).or.(iSaveFunction.eq.-3)) then
      sch(1:1)=' '
      call chwrit2(iFunUnit,ich,0,rch,0,sch,1,iOK)
      EndFile(iFunUnit)
	    call closeFunction(iFunUnit)
    end if
  end Subroutine SaveFunction

  Subroutine openFunction(lCheck,iWarn)
! read functions from file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist,lNoFunHead,lWarn
	  Integer(4) ier,i,mF,mFA,mF1,idum,ios,nFunOld,nFunAOld,iVers,mFtitles,iWarn
    Character(20) text
    lWarn=.true.
    if(iWarn.eq.0) lWarn=.false.
    iWarn=0
    nFunOld=nFun
    nFunAOld=nFunA
    iFun1=1
    iFun2=nFun
    if(lAskFun) then
      idum=MessageBoxQQ('Read function representation data?'C,'Open function'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipFunHead=.true.
      if(idum.eq.MB$IDYES) lSkipFunHead=.false.
      if(lSkipFunHead) then
        nFunKol=nFunAOld
        nFunRow=nFunOld
      end if
      idum=MessageBoxQQ('Read function values?'C,'Open function'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipFun=.true.
      if(idum.eq.MB$IDYES) lSkipFun=.false.
    end if
    if(.not.lCheck) then
      if((nFunA.gt.0_4).and.(nFun.gt.0_4).and.(.not.lSkipFun)) then
        idum=MessageBoxQQ('Overwrite existing functions?'C,'Open function'C, &
                          MB$YESNO.or.MB$ICONQUESTION)
        if(idum.eq.MB$IDNO) then
          idum=MessageBoxQQ('Append columns to function array?'C,'Open function'C, &
                            MB$YESNO.or.MB$ICONQUESTION)
          if(idum.eq.MB$IDNO) then
            iFunRow=nFunOld
            iFunKol=0
          else
            iFunRow=0
            iFunKol=nFunAOld
          end if
        else
          iFunRow=0
          iFunKol=0
        end if
      end if
      call Open2read(-1,'Select function data file to be read!','Function data file ',FunFileName,'FUN',ios)
      if(ios.gt.0) then
	      call closeFunction(1_4)
        return
      end if
    end if
    lAppFun=.false.
    inquire(file=FunFileName,Exist=lFileExist)
    if(.not.lFileExist) then
	    call closeFunction(1_4)
      return
    end if
    open(1,file=FunFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open function'C, &
                      MB$OK.or.MB$IconExclamation)
	    call closeFunction(1_4)
      return
    end if
! identification
    call ReadStr(1,text,ier)
    iVers=0
    if(text(18:18).eq.'1') iVers=1
    if(text(18:18).eq.'2') iVers=2
    if(text(18:18).eq.'3') iVers=3
    if(CHFunIdent(1:17).ne.text(1:17)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open function'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
	      call closeFunction(1_4)
        return
      end if
    end if
! header
    call chread2(1,ich,2,rch,0,ier)
    mFtitles=ich(2)
    if(nFunKol.gt.0) then
      mFA=nFunKol
    else
      mFA=mFtitles
    end if
    if((ier.ne.0).or.(mFA.lt.1)) then
      idum=MessageBoxQQ('Error reading input file!\r(number of functions)'C,'Open function'C, &
                        MB$OK.or.MB$ICONSTOP)
	    call closeFunction(1_4)
		  return
	  end if
    lNoFunHead=.false.
    if(ich(1).eq.0) lNoFunHead=.true.
    if(lSkipFun.and.(lSkipFunHead.or.lNoFunHead)) then
	    call closeFunction(1_4)
      return
    end if
    if(.not.lNoFunHead) then
! read function representation
      ich(1:6)=0
      if(iVers.eq.0) then
        call chread2(1,ich,2,rch,0,ier)
      else if(iVers.gt.2) then
        call chread2(1,ich,6,rch,0,ier)
      else
        call chread2(1,ich,4,rch,0,ier)
      end if
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(argument numbers)'C,'Open function'C, &
                          MB$OK.or.MB$ICONSTOP)
	      call closeFunction(1_4)
		    return
	    end if
      if(.not.lSkipFunHead) then
        iFunA1=ich(1)
        iFunA2=ich(2)
        iFunA3=ich(3)
        iFunA4=ich(4)
        iFun1=ich(5)
        iFun2=ich(6)
      end if
      call chread2(1,ich,6,rch,0,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(mark data)'C,'Open function'C, &
                          MB$OK.or.MB$ICONSTOP)
	      call closeFunction(1_4)
		    return
	    end if
      if(.not.lSkipFunHead) then
        iFunMarkQ=ich(1)
        iFunMarkP=ich(2)
        iFunMarkX=ich(3)
        iFunMarkSize=ich(4)
        iFunMarkStep=ich(5)
        iFunMarkColor=ich(6)
	    end if
      call chread2(1,ich,6,rch,0,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(representation data)'C,'Open function'C, &
                          MB$OK.or.MB$ICONSTOP)
	      call closeFunction(1_4)
		    return
	    end if
      if(.not.lSkipFunHead) then
        iFunStyleP=ich(1)
        iFunStyleL=ich(2)
        iFunStyleD=ich(3)
        iFunStyleWidth=ich(4)
        iFunStyleStep=ich(5)
        iFunStyleColor=ich(6)
	    end if
      if(iVers.gt.1) then
        call chread2(1,ich,1,rch,2,ier)
        if(ier.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(polar data)'C,'Open function'C, &
                            MB$OK.or.MB$ICONSTOP)
	        call closeFunction(1_4)
		      return
	      end if
        if(.not.lSkipFunHead) then
          iFunPolar=ich(1)
          rFunOff=rch(1)
          rFunSca=rch(2)
	      end if
      end if
	  end if
    if(lSkipFun) then
	    call closeFunction(1_4)
      return
    end if
! skip titles
    do i=0,mFtitles
      call ReadStr(1,text,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(titles of functions)'C,'Open function'C, &
                          MB$OK.or.MB$ICONSTOP)
	      call closeFunction(1_4)
		    return
	    end if
    end do
! count number of lines
    mF=0
    call Cursor(.true.,IDC_WAIT)
    do
      call chread2(1,ich,0,rch,mFA,ier)
      if(ier.ne.0) Exit
      mF=mF+1
    end do
!    write(*,*) 'Function lines counted: mF,mFA,mFun=',mF,mFA,mFun
    if(nFunRow.gt.0) then
      mF=min(mF,nFunRow)
    end if
    call Cursor(.false.,IDC_WAIT)
! memory allocation
    if(mF.lt.2) then ! don't modify number of rows
      call IncreaseFun(iFunRow,iFunKol+mFA-mFunA,ldum)
    else
      call IncreaseFun(iFunRow+mF-mFun,iFunKol+mFA-mFunA,ldum)
    end if
    if(.not.ldum) then
      idum=MessageBoxQQ('Error reading input file!\r(memory allocation)'C,'Open function'C, &
                        MB$OK.or.MB$ICONSTOP)
	    call closeFunction(1_4)
		  return
    end if
! rewind and read header again
    rewind(1)
    call ReadStr(1,text,ier)
    call chread2(1,ich,2,rch,0,ier)
    if(.not.lNoFunHead) then
      ich(1:6)=0
      if(iVers.eq.0) then
        call chread2(1,ich,2,rch,0,ier)
      else if(iVers.gt.2) then
        call chread2(1,ich,6,rch,0,ier)
      else
        call chread2(1,ich,4,rch,0,ier)
      end if
      call chread2(1,ich,6,rch,0,ier)
      call chread2(1,ich,6,rch,0,ier)
      if(iVers.gt.1) call chread2(1,ich,1,rch,2,ier)
    else
      iFunA1=0
      iFunA2=1
      iFunA3=0
      iFunA4=0
      iFun1=1
      iFun2=0
      iFunPolar=0
      rFunOff=0.0d0
      rFunSca=1.0d0
    end if
! read function titles
    do i=iFunKol,iFunKol+mFtitles
      if(((i.eq.iFunKol).and.(i.ne.0)).or.(i.gt.iFunKol+mFA)) then
        call ReadStr(1,sch,ier)
      else
        call ReadStr(1,FunATitle(i),ier)
        idum=min(64,GetSLength(FunATitle(i))+1)
        FunATitle(i)(idum:idum)=Char(0)
      end if
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(titles of functions)'C,'Open function'C, &
                          MB$OK.or.MB$ICONSTOP)
	      call closeFunction(1_4)
		    return
	    end if
    end do
! read functions
    call Cursor(.true.,IDC_WAIT)
!    write(*,*) 'Read function lines mF,mFA,mFun:',mF,mFA,mFun
    if(mF.gt.1) then
      mF1=0
	    do i=1,mF
        call chread2(1,ich,0,rch,mFA,ier)
		    if(ier.ne.0) then
          iWarn=1
          if(lWarn) then
            call Cursor(.false.,IDC_WAIT)
            idum=MessageBoxQQ('WARNING: Error reading function!\rRepeat reading might help!'C, &
                              'WARNING in OpenFunction'C, &
                              MB$OK.or.MB$ICONEXCLAMATION)
          end if
          Exit
        end if
        mF1=mF1+1
        Fun(iFunKol+1:iFunKol+mFA,iFunRow+i)=rch(1:mFA)
		  end do
      call chread2(1,ich,0,rch,mFA,ier) ! try reading one more line because the line count might have failed
		  if(ier.eq.0) then
        iWarn=2
        if(lWarn) then
          call Cursor(.false.,IDC_WAIT)
          idum=MessageBoxQQ('WARNING: Line count error when reading function!\rRepeat reading might help!'C, &
                            'WARNING in OpenFunction'C, &
                            MB$OK.or.MB$ICONEXCLAMATION)
        end if
      end if
      nFun=min(mFun,max(nFunOld,iFunRow+mF1))
    end if
    nFunA=min(mFunA,max(nFunAOld,iFunKol+mFA))
    iFunA1=max(0,min(iFunA1,nFunA))
    iFunA2=max(0,min(iFunA2,nFunA))
    iFunA3=max(0,min(iFunA3,nFunA))
    iFunA4=max(0,min(iFunA4,nFunA))
    iFun1=max(1,min(iFun1,nFun))
    if(iFun2.lt.1) iFun2=nFun
    iFun2=max(iFun2,iFun1)
    call Cursor(.false.,IDC_WAIT)
	  if(nFun.lt.2) then
      idum=MessageBoxQQ('Functions with less than 2 values!\rThis will cause problems!'C,'Open function'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
    end if
	  call closeFunction(1_4)
    iFunKol=0
    iFunRow=0
    nFunKol=0
    nFunRow=0
  end Subroutine openFunction

  Subroutine setFunctionPolar(lpolar,lclip,lsquare,ldb)
    Implicit none
    Logical lpolar,lclip,lsquare,ldb
    iFunPolar=0_2
    if(lpolar)  iFunPolar=iFunPolar+1_2
    if(lclip)   iFunPolar=iFunPolar+2_2
    if(lsquare) iFunPolar=iFunPolar+4_2
    if(ldb)     iFunPolar=iFunPolar+8_2
  end Subroutine setFunctionPolar

  Subroutine getFunctionPolar(lpolar,lclip,lsquare,ldb)
    Implicit none
    Integer(2) i1,i2
    Logical lpolar,lclip,lsquare,ldb
    lpolar=.false.
    lclip=.false.
    lsquare=.false.
    ldb=.false.
    i1=iabs(iFunPolar)
    i2=i1/8_2
    if(i2.gt.0_2) ldb=.true.
    i1=i1-8_2*i2
    i2=i1/4_2
    if(i2.gt.0_2) lsquare=.true.
    i1=i1-4_2*i2
    i2=i1/2_2
    if(i2.gt.0_2) lclip=.true.
    i1=i1-2_2*i2
    i2=i1
    if(i2.gt.0_2) lpolar=.true.
  end Subroutine getFunctionPolar

  Subroutine openFun(lCheck)
! read functions from file with plain list
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
	  Integer(4) ier,i,ia,ib,mF,mFA,idum,ios
    Character(256) Name
    Name='fun.dat'
    call Open2read(1,'Select data file to be read!','Function file ',Name,'DAT',ios)
    if(ios.gt.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open function'C, &
                      MB$OK.or.MB$IconExclamation)
	    close(1)
      return
    end if
! count number of values in first line
    mFA=0
    call ReadStr(1,Name,ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error reading file!\rCannot read first line!'C,'Open function'C, &
                      MB$OK.or.MB$IconExclamation)
	    close(1)
      return
    end if
    idum=GetSLength(Name)
    i=1
    do while(i.le.idum)
      call BstrB(Name,i,ia,ib)
      if(ib.lt.ia) Exit
      mFA=mFA+1
      i=ib+1
    end do
! rewind and count number of lines
    rewind(1)
    mF=0
    call Cursor(.true.,IDC_WAIT)
    do
      call chread2(1,ich,0,rch,mFA,ier)
      if(ier.ne.0) Exit
      mF=mF+1
    end do
    call Cursor(.false.,IDC_WAIT)
    if(mF.lt.2) then
	    close(1)
		  return
	  end if
! memory allocation
    mFun=mF
    mFunA=mFA
    nFun=mFun
    nFunA=mFunA
    call AllocateFun(ldum)
    if(.not.ldum) then
      idum=MessageBoxQQ('Error reading input file!\r(memory allocation)'C,'Open function'C, &
                        MB$OK.or.MB$ICONSTOP)
	    close(1)
		  return
    end if
! rewind and read again
    rewind(1)
    call Cursor(.true.,IDC_WAIT)
	  do i=1,nFun
      ier=0
      call chread2(1,ich,0,rch,nFunA,ier)
		  if(ier.ne.0) then
        call Cursor(.false.,IDC_WAIT)
        idum=MessageBoxQQ('Error reading input file!\r(the file is too short)'C,'Open function'C, &
                          MB$OK.or.MB$ICONHAND)
	      close(1)
		    return
		  end if
      Fun(1:nFunA,i)=rch(1:nFunA)
		end do
    call Cursor(.false.,IDC_WAIT)
	  if(nFun.lt.2) then
      idum=MessageBoxQQ('Functions with less than 2 values!\rThis will cause problems!'C,'Open function'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
    end if
	  close(1)
  end Subroutine openFun

  Subroutine closeFunction(iUnit)
    Implicit none
    Integer(4) iUnit
    Logical ldum
    inquire(unit=iUnit,opened=ldum)
	  if(ldum) close(iUnit)
    lAskFun=.true.
    lSkipFun=.false.
    lSkipFunHead=.false.
  end Subroutine closeFunction

	Subroutine ProcessFun(lo)
! Process function with formula
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) cFunPro(0:0),pFunPro(0:0)
    Real(8), Allocatable :: vFunPro(:)
    Logical(1) lSet
    Logical, intent(in) :: lo
    Logical ldum
    Integer(4) i,idum,k,m,lf,iErr
    Real(8) f
    lAppFun=.false.
		if(nFunA.lt.1) then
      if(lo) idum=MessageBoxQQ('Function not defined!'C,'Process function'C, &
                               MB$OK.or.MB$ICONHAND)
    else if(nFunA.ge.mFunA) then
      call IncreaseFun(0,(nFunA-mFunA+1),ldum)
      if(.not.ldum) nFunA=0
    end if
		if(nFunA.gt.0) then
      nFunA=nFunA+1_2
      idum=GetSLength(string_Pro)
      FunATitle(nFunA)(1:idum)=string_Pro(1:idum)
      FunATitle(nFunA)(idum+1:idum+1)=Char(0)
      lf=-1_4
      m=Int4(nFunA)-1_4
      Allocate(vFunPro(-10:m),stat=ierr)
      if(iErr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate auxiliary array!'C,'Process function'C, &
                          MB$OK.or.MB$ICONSTOP)
		  else
        cFunPro=0.0d0
        pFunPro=0.0d0
        vFunPro=0.0d0
        call Cursor(.true.,IDC_WAIT)
        do i=1,nFun
          do k=0,m
            call GetFun(k,i,vFunPro(k))
          end do
          dFormu=Formula(string_Pro,lf,cFunPro,pFunPro,vFunPro,0,0,m,1,1,1,idum)
          f=dFormu(1)
          call SetFun(nFunA,i,f,lSet)
        end do
        call Cursor(.false.,IDC_WAIT)
        DeAllocate(vFunPro,stat=ierr)
      end if
    end if
	end Subroutine ProcessFun

	Subroutine OperatorFun(iA)
! Apply operator to function argument iA
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) cFunPro(0:0),pFunPro(0:0)
    Real(8), Allocatable :: vFunPro(:)
    Logical(1) lSet
    Logical ldum
    Integer(4) iA,i,idum,k,m,lf,iErr
    Real(8) f
    lAppFun=.false.
		if(nFunA.lt.1) then
      idum=MessageBoxQQ('Function not defined!'C,'Operate on function'C, &
                               MB$OK.or.MB$ICONHAND)
    else if(nFunA.ge.mFunA) then
      call IncreaseFun(0,(nFunA-mFunA+1),ldum)
      if(.not.ldum) nFunA=0
    end if
		if(nFunA.gt.0) then
      nFunA=nFunA+1_2
      idum=GetSLength(string_Opr)
      FunATitle(nFunA)(1:idum)=string_Opr(1:idum)
      FunATitle(nFunA)(idum+1:idum+1)=Char(0)
      lf=-1_4
      m=20_4
      Allocate(vFunPro(-10:m),stat=ierr)
      if(iErr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate auxiliary array!'C,'Operate on function'C, &
                          MB$OK.or.MB$ICONSTOP)
		  else
        cFunPro=0.0d0
        pFunPro=0.0d0
        vFunPro=0.0d0
        call Cursor(.true.,IDC_WAIT)
        do i=1,nFun
          do k=0,m
            call GetFun(iA,i+k,vFunPro(k))
          end do
          dFormu=Formula(string_Opr,lf,cFunPro,pFunPro,vFunPro,0,0,m,1,1,1,idum)
          f=dFormu(1)
          call SetFun(nFunA,i,f,lSet)
        end do
        call Cursor(.false.,IDC_WAIT)
        DeAllocate(vFunPro,stat=ierr)
      end if
    end if
	end Subroutine OperatorFun

  Subroutine ExtendFun(x0,x1,nx,lo)
! extend x range of the function array
    Implicit none
	  Include 'RESOURCE.FD'
    Logical, intent(in) :: lo
    Logical ldum
    Integer(4) nx,idum,i,n0
    Real(8) x0,x1,dx,x
    lAppFun=.false.
		if(nFunA.lt.1) then
      if(lo) idum=MessageBoxQQ('Function not defined!'C,'Extend function'C, &
                               MB$OK.or.MB$ICONHAND)
    else if(nFunA.ge.mFunA) then
      call IncreaseFun(0,(nFunA-mFunA+1),ldum)
      if(.not.ldum) nFunA=0
    end if
		if(nFunA.gt.0) then
      n0=nFun
      if((nFun+nx).gt.mFun) then
        call IncreaseFun((nFun+nx-mFun),0,ldum)
        if(.not.ldum) return
      end if
      nFun=min(nFun+nx,mFun)
      dx=(x1-x0)/Dble(max(1,(nFun-n0-1)))
      x=x0-dx
      call Cursor(.true.,IDC_WAIT)
      do i=n0+1,nFun
        x=x+dx
        Fun(1,i)=x
        if(nFunA.gt.1) then
          Fun(2:nFunA,i)=Fun(2:nFunA,i-1)
        end if
      end do
      call Cursor(.false.,IDC_WAIT)
    end if
  end Subroutine ExtendFun

  Subroutine CompressFun(icomp,igetMMi,lo)
! compress x range of the function array
    Implicit none
	  Include 'RESOURCE.FD'
    Logical, intent(in) :: lo
    Logical ldum
    Integer(4) icomp,igetMMi,igetMM,n0,nr,i,j,k,nFA,idum
    Real(8) q,fmin,fmax
    if(icomp.lt.2) return
    igetMM=min(igetMMi,nFunA)
    nFA=nFunA
    if(igetMM.gt.0) nFunA=nFunA+2
    lAppFun=.false.
		if(nFA.lt.1) then
      if(lo) idum=MessageBoxQQ('Function not defined!'C,'Compress function'C, &
                               MB$OK.or.MB$ICONHAND)
    else if(nFunA.gt.mFunA) then
      call IncreaseFun(0,(nFunA-mFunA),ldum)
      if(.not.ldum) nFA=0
    end if
		if(nFA.gt.0) then
      n0=nFun/icomp
      if(n0.lt.2) then
        if(lo) idum=MessageBoxQQ('Function has not enough values!'C,'Compress function'C, &
                                 MB$OK.or.MB$ICONHAND)
        return
      end if
      j=0
      q=1.0d0/Dble(icomp)
      call Cursor(.true.,IDC_WAIT)
      do i=1,n0
        if(igetMM.gt.0) then
          fmin=pBig
          fmax=nBig
        end if
        if(i.gt.1) Fun(1:nFA,i)=0.0d0
        do k=1,icomp
          j=j+1
          if(igetMM.gt.0) then
            fmin=min(fmin,Fun(igetMM,j))
            fmax=max(fmax,Fun(igetMM,j))
          end if
          if(j.eq.i) Cycle
          Fun(1:nFA,i)=Fun(1:nFA,i)+Fun(1:nFA,j)
        end do
        Fun(1:nFA,i)=Fun(1:nFA,i)*q
        if(igetMM.gt.0) then
          Fun(nFunA-1,i)=fmin
          Fun(nFunA,i)=fmax
        end if
      end do
      i=n0
      nr=nFun-n0*icomp
      if(nr.gt.0) then
        i=n0+1
        q=1.0d0/Dble(nr)
        if(igetMM.gt.0) then
          fmin=pBig
          fmax=nBig
        end if
        if(i.gt.1) Fun(1:nFA,i)=0.0d0
        do k=1,nr
          j=j+1
          if(igetMM.gt.0) then
            fmin=min(fmin,Fun(igetMM,j))
            fmax=max(fmax,Fun(igetMM,j))
          end if
          if(j.eq.i) Cycle
          Fun(1:nFA,i)=Fun(1:nFA,i)+Fun(1:nFA,j)
        end do
        Fun(1:nFA,i)=Fun(1:nFA,i)*q
        if(igetMM.gt.0) then
          Fun(nFunA-1,i)=fmin
          Fun(nFunA,i)=fmax
        end if
      end if
      call Cursor(.false.,IDC_WAIT)
      call IncreaseFun((i-mFun),0,ldum)
      nFun=min(i,mFun)
    end if
  end Subroutine CompressFun

! Function evaluations

  Subroutine GetLimits(ilogX,ilogY,xmi,ymi,xma,yma,idigit,iOK)
! get the limits of the current function
    Implicit none
    Real(8) xmi,xma,ymi,yma,a,dx,dy
    Integer(2) ilogX,ilogY,idigit,iOK,len
    Integer(4) idum
    Character(20) rss
    iOK=0_2
    if(nFun.lt.1) then
      idum=MessageBoxQQ('Cannot compute limits!\r(no function defined)'C,'Get window limits'C, &
                        MB$OK.or.MB$ICONHAND)
      iOK=1_2
      return
    end if
    if((iFunA1.lt.1_2).or.(iFunA1.gt.nFunA)) then
      xmi=1.0_8
      xma=dble(nFun)
    else
      call MinMax(Fun,mFun,mFunA,nFun,iFunA1,xmi,xma)
    end if
    if((dabs(xma-xmi).lt.1.0d-99).or. &
      ((iabs(ilogX).gt.1).and. &
       ((xmi.lt.1.0d-99).or.(xma.lt.1.0d-99)))) then
      idum=MessageBoxQQ('Setting of X limits failed!'C,'Get window limits'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      iOK=iOK-1_2
      if(iabs(ilogX).gt.1) then
        xmi=max(1.0d-99,xmi,xma*1.0d-10)
        xma=max(1.0d-99,xma,xmi+1.0d-99,xmi*1.0000001d0)
      else
        xma=max(xmi+1.0d-99,xmi*1.0000001d0)
      end if
    end if
    if((iFunA2.lt.1_2).or.(iFunA2.gt.nFunA)) then
      ymi=1.0_8
      yma=dble(nFun)
    else
      call MinMax(Fun,mFun,mFunA,nFun,iFunA2,ymi,yma)
    end if
    if((dabs(yma-ymi).lt.1.0d-99).or. &
      ((iabs(ilogY).gt.1).and. &
       ((ymi.lt.1.0d-99).or.(yma.lt.1.0d-99)))) then
      idum=MessageBoxQQ('Setting of Y limits failed!'C,'Get window limits'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      iOK=iOK-1_2
      if(iabs(ilogY).gt.1) then
        ymi=max(1.0d-99,ymi,yma*1.0d-10)
        yma=max(1.0d-99,yma,ymi+1.0d-99,ymi*1.0000001d0)
      else
        yma=max(ymi+1.0d-99,ymi*1.0000001d0)
      end if
    end if
    if(xmi.gt.xma) then
      a=xma
      xma=xmi
      xmi=a
    end if
    if(ymi.gt.yma) then
      a=yma
      yma=ymi
      ymi=a
    end if
    dx=max(dabs(xma-xmi),1.0d-99)
    dy=max(dabs(yma-ymi),1.0d-99)
    if((idigit.gt.0).and.(idigit.lt.11)) then
      if(dabs(xmi).gt.1.0d-307) then
        a=xmi-0.2_8*10.0_8**int(dlog10(dabs(xmi))-dble(idigit))
        if(iabs(ilogX).gt.1) a=max(1.0d-99,a)
        call rswrit2(a,idigit,rss,len)
        xmi=GetSValue(rss,len)
      end if
      if(dabs(xma).gt.1.0d-307) then
        a=xma+0.2_8*10.0_8**int(dlog10(dabs(xma))-dble(idigit))
        if(iabs(ilogX).gt.1) a=max(1.0d-99,a)
        call rswrit2(a,idigit,rss,len)
        xma=GetSValue(rss,len)
      end if
      if(dabs(ymi).gt.1.0d-307) then
        a=ymi-0.2_8*10.0_8**int(dlog10(dabs(ymi))-dble(idigit))
        if(iabs(ilogY).gt.1) a=max(1.0d-99,a)
        call rswrit2(a,idigit,rss,len)
        ymi=GetSValue(rss,len)
      end if
      if(dabs(yma).gt.1.0d-307) then
        a=yma+0.2_8*10.0_8**int(dlog10(dabs(yma))-dble(idigit))
        if(iabs(ilogY).gt.1) a=max(1.0d-99,a)
        call rswrit2(a,idigit,rss,len)
        yma=GetSValue(rss,len)
      end if
    end if
    if(xma.le.xmi) xma=xmi+dx
    if(yma.le.ymi) yma=ymi+dy
  end Subroutine GetLimits

  Subroutine FindFunPosition(iArg,x,ipos)
! find position ipos of the element of Fun near x for the argument number iArg
    Implicit none
    Integer(4) iArg,ipos,i,nd
    Real(8) x,x0,dx,d,dmin
    if((iArg.lt.1_2).or.(iArg.gt.nFunA)) then
      ipos=nint(x,4)
    else
      if(x.ge.Fun(iArg,nFun)) then
        dx=Fun(iArg,nFun)-Fun(iArg,1)
        nd=IdInt((x-Fun(iArg,1))/dx)
        x0=x-dble(nd)*dx
      else if(x.lt.Fun(iArg,1)) then
        dx=Fun(iArg,nFun)-Fun(iArg,1)
        nd=IdInt((x-Fun(iArg,1))/dx)-1
        x0=x-dble(nd)*dx
      else
        nd=0
        x0=x
      end if
      dmin=dabs(x0-Fun(iArg,1))
      ipos=1_4
      do i=2_4,nFun
        d=dabs(x0-Fun(iArg,i))
        if(d.lt.dmin) then
          dmin=d
          ipos=i
        end if
      end do
      ipos=ipos+nd*(nFun-1)
    end if
  end Subroutine FindFunPosition

  Subroutine GetFun(iArg,nVal,x)
! x = value number nVal of the function with the argument number iArg
    Integer(4) iArg,nVal,nd,nx
    Real(8) x,d
    if((iArg.lt.1).or.(iArg.gt.nFunA).or.(nFun.lt.2)) then
      x=dble(nVal)
    else
      if((nVal.lt.1).or.(nVal.gt.nFun)) then
        d=Fun(iArg,nFun)-Fun(iArg,1)
        if(nVal.lt.1) then
          nd=nVal/max(1,(nFun-1))-1
        else
          nd=(nVal-1)/max(1,(nFun-1))
        end if
        nx=nVal-nd*(nFun-1)
        x=Fun(iArg,nx)+dble(nd)*d
      else
        x=Fun(iArg,nVal)
      end if
    end if
  end Subroutine GetFun

  Subroutine SetFun(iArg,nVal,x,lSet)
! Set x = value number nVal of the function with the argument number iArg
    Logical(1) lSet
    Integer(4) iArg,nVal
    Real(8) x
    if((iArg.lt.1).or.(iArg.gt.nFunA).or.(nVal.lt.1).or.(nVal.gt.nFun)) then
      lSet=.false.
    else
      Fun(iArg,nVal)=x
      lSet=.true.
    end if
  end Subroutine SetFun
  
  Subroutine MaxFun(iArg,x,nmin,nmax)
! x = maximum value of the function with the argument number iArg
    Integer(4) iArg,n,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if
    if((iArg.lt.0).or.(iArg.gt.nFunA).or.(nFun.lt.1)) then
      x=-1.0d300
    else if(iArg.eq.0) then
      x=dble(nFun)
    else
      x=-1.0d300
      do n=n1,n2
        if(Fun(iArg,n).gt.x) x=Fun(iArg,n)
      end do
    end if
  end Subroutine MaxFun  

  Subroutine MinFun(iArg,x,nmin,nmax)
! x = minimum value of the function with the argument number iArg
    Integer(4) iArg,n,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if    
    if((iArg.lt.0).or.(iArg.gt.nFunA).or.(nFun.lt.1)) then
      x=1.0d300
    else if(iArg.eq.0) then
      x=1.0d0
    else
      x=1.0d300
      do n=n1,n2
        if(Fun(iArg,n).lt.x) x=Fun(iArg,n)
      end do
    end if
  end Subroutine MinFun 
  
  Subroutine SumFun(iArg,x,nmin,nmax)
! x = sum of all values of the function with the argument number iArg
    Integer(4) iArg,n,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if    
    if((iArg.lt.0).or.(iArg.gt.nFunA).or.(nFun.lt.1)) then
      x=0.0d0
    else if(iArg.eq.0) then
      x=0.5d0*dble(nFun)*dble(nFun+1)
    else
      x=0.0d0
      do n=n1,n2
        x=x+Fun(iArg,n)
      end do
    end if
  end Subroutine SumFun 
  
  Subroutine SumFun1(iArg,x,nmin,nmax)
! x = sum of all abs(values) of the function with the argument number iArg
    Integer(4) iArg,n,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if    
    if((iArg.lt.0).or.(iArg.gt.nFunA).or.(nFun.lt.1)) then
      x=0.0d0
    else if(iArg.eq.0) then
      x=0.5d0*dble(nFun)*dble(nFun+1)
    else
      x=0.0d0
      do n=n1,n2
        x=x+dabs(Fun(iArg,n))
      end do
    end if
  end Subroutine SumFun1
  
  Subroutine SumFun2(iArg,x,nmin,nmax)
! x = square root of the sum of all square values of the function with the argument number iArg
    Integer(4) iArg,n,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if    
    if((iArg.lt.0).or.(iArg.gt.nFunA).or.(nFun.lt.1)) then
      x=0.0d0
    else if(iArg.eq.0) then
      x=0.0d0
      do n=1,nFun
        x=x+dble(n)**2
      end do
    else
      x=0.0d0
      do n=n1,n2
        x=x+Fun(iArg,n)**2
      end do
    end if
    x=dsqrt(x)
  end Subroutine SumFun2  
  
  Subroutine AveFun(iArg,x,nmin,nmax)
! x = average of all values of the function with the argument number iArg
    Integer(4) iArg,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if
    call SumFun(iArg,x,n1,n2)
    x=x/dble(1+n2-n1)
  end Subroutine AveFun
  
  Subroutine AveFun1(iArg,x,nmin,nmax)
! x = average of all absolute values of the function with the argument number iArg
    Integer(4) iArg,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if
    call SumFun1(iArg,x,n1,n2)
    x=x/dble(1+n2-n1)
  end Subroutine AveFun1
  
  Subroutine AveFun2(iArg,x,nmin,nmax)
! x =square root of the average of all square values of the function with the argument number iArg
    Integer(4) iArg,n1,n2
    Real(8) x
    Integer(4), Optional:: nmin,nmax
    if(Present(nmax)) then
      n1=min(nFun,max(1,nmin))
      n2=max(n1,min(nFun,nmax))
    else if(Present(nmin)) then
      n1=1
      n2=max(n1,min(nFun,nmax))
    else
      n1=1
      n2=nFun
    end if
    call SumFun2(iArg,x,n1,n2)
    x=dsqrt(x*x/dble(1+n2-n1))
  end Subroutine AveFun2

END MODULE CHFUN




