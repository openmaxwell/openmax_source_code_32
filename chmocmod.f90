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
MODULE CHMOC

! movie commands

  USE CHPRT

  SAVE

  Character(20), Parameter :: CHDirIdent=' CHDIR Version 2.0  '
  Real(8) spa(3,0:3),rMovCPU0,rMovCPU1,rMovELA0,rMovELA1
  Integer(4), Parameter :: mMovCommand=10000,lsMovCommand=128,lsMovCommands=32000
  Integer(4) nMovPic,nMovCommand,kMovCommand,kMovCommand1,nMovSeq,lArgument,lsMovComm,lsMovComms,kFunRow,kFunCol,iMoveTim(8)
  Integer(4) iMovLab(-1000:1000)
  Logical(1) loopStart(1000)
  Logical(4) lRestart
  Character*10 charMoveTim(3)
  Character(256) DirFileName
  Character(32000) Mov_Command,Mov_Command1,Mov_Command2(5),Argument
  Character(32000) Mov_Commands


CONTAINS

! I/O

  Subroutine SaveDirectives(lCheck)
! save movie directives in a file
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) iOk,ios,idum,k
    Logical lEx
    Logical, intent(in) :: lCheck
    if(.not.lCheck) then
      call Open2write(-1,'Select directive file to be written!','Directive file ',DirFileName,'DIR',ios)
      if(ios.gt.0) return
    end if
    open(1,file=DirFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save directives'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHDirIdent,iOK)
    ios=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
    k=kMovCommand
    do kMovCommand=1,nMovCommand
      call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
      if(lsMovComm.lt.1) cycle
      call WriteStr(1,Mov_Command,iOK)
      call ToLower(Mov_Command,lsMovComm)
      lEx=.false.
      if(lsMovComm.gt.2) then
        if(Mov_Command(1:3).eq.'exi') then
          lEx=.true.
        end if
      end if
    end do
    kMovCommand=k
    if(.not.lEx) then
      sch(1:4)='exit' ! append exit statement
      call chwrit2(1,ich,0,rch,0,sch,4,iOK)
    end if
    sch(1:3)='   ' ! append a blank line, maybe reduces risk of closing before everything is written
    call chwrit2(1,ich,0,rch,0,sch,3,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveDirectives

  Subroutine OpenDirectives(lCheck)
! read movie directives from a file
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) iOk,ios,idum,iVers,lText,lsMovComm
    Logical, intent(in) :: lCheck
    Logical lFileExist,ldum
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select directive file to be read!','Directive file ',DirFileName,'DIR',ios)
      if(ios.gt.0) return
    end if
    inquire(file=DirFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=DirFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open directives'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    call StrToInt(text(16:16)//text(18:18),iVers,iOK)
    if(CHDirIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open directives'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    if(iVers.lt.20) then ! old version with 2 nested loops
      call chread2(1,ich,2,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(first line)'C,'Open directives'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      nMovSeq=ich(1)
      nMovPic=ich(2)
    else
      nMovSeq=0
      nMovPic=0
    end if
    lsMovComm=0
    lsMovComms=0
    ios=0
    do kMovCommand=1,mMovCommand
      call ReadStr(1,Mov_Command,iOK)
      if(iOK.ne.0) then
        if(iVers.lt.20) then
          write(text,'(I3)') kMovCommand
          idum=MessageBoxQQ('Error reading input file!\rDirective string '//text(1:3)//' corrupt!'C,'Open directives'C, &
                            MB$OK.or.MB$ICONSTOP)
        else
          write(*,*) 'Number of directive lines=',kMovCommand-1
        end if       
  	    Exit
	    end if
	    call DelLeadingBlanks(Mov_Command,lsMovCommand)
      if(lsMovCommand.lt.1) Cycle ! ignore blank command
      call ToLower(Mov_Command,lsMovCommand)
      lsMovComm=lsMovCommand
      if(Mov_Command(1:3).eq.'end') then ! replace end statements of older version by loop statements
        ios=ios+1
        lsMovComm=5
        Mov_Command(1:5)='! end' ! no end statements anymore!
        if(iVers.lt.20) then ! older version
          Mov_Command(1:5)='! end'
          Select Case(ios)
          Case(1) ! first end -> outer loop start: label / variable / start value
            if(nMovSeq.gt.1) then
              Mov_Command(1:15)='loop 999 999 1 '
              write(text,*) nMovSeq
              lText=0
	            call DelLeadingBlanks(text,lText)
              lsMovComm=15+lText
              Mov_Command(16:lsMovComm)=text(1:lText)
            end if
          Case(2) ! second end -> inner loop start
            if(nMovPic.gt.1) then
              Mov_Command(1:15)='loop 998 998 1 '
              write(text,*) nMovPic
              lText=0
	            call DelLeadingBlanks(text,lText)
              lsMovComm=15+lText
              Mov_Command(16:lsMovComm)=text(1:lText)
            end if
          Case(3) ! third end -> inner loop end
            if(nMovPic.gt.1) then
              Mov_Command(1:9)='loop -998'
              lsMovComm=9
            end if
          Case(4) ! fourth end -> outer loop end
            if(nMovSeq.gt.1) then
              Mov_Command(1:9)='loop -999'
              lsMovComm=9
            end if
          end Select
        end if
        Mov_Command(lsMovComm+1:lsMovComm+1)=Char(0)
      end if
      call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,Mov_Command(1:lsMovComm)//Char(0))
      if(ios.gt.4) exit
    end do
    close(1)
    idum=nStrInStr(Mov_Commands,lsMovCommands-1)
    nMovCommand=min(mMovCommand,idum)
    kMovCommand=min(kMovCommand,nMovCommand)
    ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.false.)
  !  idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands//Char(0)) ! caused problems!
    ldum=DlgSet(outputdlg,IDC_OUT_TEXT4,Mov_Commands//Char(0))
    ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.true.)
    lRestart=.true.
  end Subroutine OpenDirectives

  Subroutine SaveProAll(lCheck)
    Implicit none
	  Include 'RESOURCE.FD'
    Logical(4), intent(in) :: lCheck
    Logical lOpened
    Integer(4) idum
    call SaveProject(lCheck,lOpened)
    if(.not.lOpened) return
    if(.not.lCheck) then
      idum=MessageBoxQQ('Save associated files?'C,'Save project'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) return
    end if
    call Cursor(.true.,IDC_WAIT)
    call setNameExt(ProFileName,'WIN',WinFileName)
    call SaveWindow(.true.)
    call setNameExt(ProFileName,'OGL',OGLFileName)
    call SaveCHGLWindow(.true.)
    call setNameExt(ProFileName,'FUN',FunFileName)
    lAskFun=.false.
    lSkipFun=.false.
    lSkipFunHead=.false.
    iSaveFunction=0
    call SaveFunction(.true.,.false.)
    call setNameExt(ProFileName,'INT',IntFileName)
    call SaveIntegral(.true.)
    call setNameExt(ProFileName,'FLD',FldFileName)
    lDiffField=.false.
    lDiffRel=.false.
    lAskFld=.false.
    lSkipFld=.true.
    lSkipDerFld=.true.
    lSkipFldHead=.false.
    if(.not.lCheck) then
      idum=MessageBoxQQ('Save complex field values?'C,'Save project'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDYES) then
        lSkipFld=.false.
      end if
    end if
    call SaveField(.true.)
    call setNameExt(ProFileName,'FLF',FlfFileName)
    call SaveFField(.true.)
    call setNameExt(ProFileName,'PFD',PFDFileName)
    call SavePFD(.true.)
    call setNameExt(ProFileName,'GRF',GrfFileName)
    call SaveFGrid(.true.)
    call setNameExt(ProFileName,'GRT',GrtFileName)
    call SaveTGrid(.true.)
    call setNameExt(ProFileName,'DOM',DomFileName)
    call SaveDomain(.true.)
    lAskBnd=.false.
    lInsertBnd=.false.
    call setNameExt(ProFileName,'BND',BndFileName)
    call SaveBoundary(.true.)
    lAskExp=.false.
    lInsertExp=.false.
    call setNameExt(ProFileName,'EXP',ExpFileName)
    call SaveExpansion(.true.)
    lAskObj=.false.
    lInsertObj=.false.
    call setNameExt(ProFileName,'3DO',ObjFileName)
    call SaveObject(.true.)
    lMMPrMtr=.false.
    lMMPtMtr=.false.
    call setNameExt(ProFileName,'MMP',MmpFileName)
    call SaveMmp(.true.)
    call setNameExt(ProFileName,'BAS',PETFileName)
    call SaveBasis(.true.)
    call setNameExt(ProFileName,'DIR',DirFileName)
    idum=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
    call SaveDirectives(.true.)
    call SaveProject(.true.,lOpened)
    call Cursor(.false.,IDC_WAIT)
  end Subroutine SaveProAll

  Subroutine OpenProAll(lCheck)
    Implicit none
    Logical(4), intent(in) :: lCheck
    Logical(4) lOpened
    Integer(4) iWarn
    call Cursor(.true.,IDC_WAIT)
    lFLDset0=.true.
    write(*,*) 'OpenProAll, lCheck=',lCheck
    call openProject(lCheck,lOpened)
    if(lOpened) then
      call setNameExt(ProFileName,'WIN',WinFileName)
      call openWindow(.true.)
      call setNameExt(ProFileName,'PAL',PalFileName)
      call openPalette(.true.)
      call setNameExt(ProFileName,'OGL',OGLFileName)
      call openCHGLWindow(.true.)
      call setNameExt(ProFileName,'FUN',FunFileName)
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      iFunRow=0
      iFunKol=0
      nFunRow=0
      nFunKol=0
      iWarn=0
      call openFunction(.true.,iWarn)
      if(iWarn.ne.0) call openFunction(.true.,iWarn) 
      call setNameExt(ProFileName,'INT',IntFileName)
      call openIntegral(.true.)
      call setNameExt(ProFileName,'FLF',FlfFileName)
      call openFField(.true.)
      call setNameExt(ProFileName,'FLT',FltFileName)
      call setNameExt(ProFileName,'PFD',PFDFileName)
      call openPFD(.true.)
      call setNameExt(ProFileName,'GRF',GrfFileName)
      call openFGrid(.true.)
      call setNameExt(ProFileName,'GRT',GrtFileName)
      call openTGrid(.true.)
      call setNameExt(ProFileName,'DOM',DomFileName)
      call openDomain(.true.)
      lAskBnd=.false.
      lInsertBnd=.false.
      call setNameExt(ProFileName,'BND',BndFileName)
      call openBoundary(.true.)
      lAskExp=.false.
      lInsertExp=.false.
      call setNameExt(ProFileName,'EXP',ExpFileName)
      call openExpansion(.true.)
      lAskObj=.false.
      lInsertObj=.false.
      call setNameExt(ProFileName,'3DO',ObjFileName)
      call openObject(.true.)
      call setNameExt(ProFileName,'MMP',MmpFileName)
      lMMPrMtr=.true.
      lMMPtMtr=.true.
      call openMmp(.true.)
      call setNameExt(ProFileName,'FLD',FldFileName)
      lDiffField=.false.
      lDiffRel=.false.
      lAskFld=.false.
      lSkipFld=.false.
      lSkipDerFld=.false.
      lSkipVecFld=.false.
      lSkipScaFld=.false.
      lSkipFldHead=.false.
      call openField(.true.)
      call setNameExt(ProFileName,'BAS',PETFileName)
      call openBasis(.true.)
      call setNameExt(ProFileName,'DIR',DirFileName)
      call openDirectives(.true.)
      call openProject(.true.,lOpened)
      if(.not.lFLDset0) call GetrField(.false.)
      lFLDset0=.false.
      kPET=0
    end if
    call Cursor(.false.,IDC_WAIT)
  end Subroutine OpenProAll

! movie commands / directives

  Subroutine MovieCommand(lPerform,kComL,iErr,kMoCo)
    Implicit none
    Real(8) rCond1,rCond2
    Integer(4), Optional:: kMoCo
    Integer(4) kMoCo1,kComL,lCommand,lCommObj,iErr,i,i1,idum,lCond1,lCond,lCond2,iErr2
    Logical ldum
    Logical, intent(in) :: lPerform
    Character(32) Command,CommObj,Cond1,Cond,Cond2
	  if(Present(kMoCo)) then
      kMoCo1=kMovCommand
      kMovCommand=kMoCo
      kMoCo=kMoCo1
    end if
    lDrawOGL=.false.
    lSaveOGLavi=.false.
    lSaveOGLbmp=.false.
    iWinAction=0_2
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
    iErr=9
    if((kMovCommand.ge.1).and.(kMovCommand.le.nMovCommand)) then
      call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
      if(l4.and.l7) write(*,*) ' Command:',Mov_Command(1:lsMovComm)
    end if
    iErr=0
    if((Mov_Command(1:1).eq.'!').or.(lsMovComm.lt.1)) return
	  if(Present(kMoCo)) then
      if(Mov_Command(1:1).eq.'%') then
        idum=lsMovComm+1
        do i=2,lsMovComm
          if(Mov_Command(i:i).ne.' ') then
            idum=i
            Exit
          end if
        end do
        if(idum.gt.lsMovComm) return
        i1=0
        do i=idum,lsMovComm
          i1=i1+1
          Mov_Command(i1:i1)=Mov_Command(i:i)
        end do
        lsMovComm=lsMovComm-idum+1
        i1=lsMovComm+1
        Mov_Command(i1:i1)=Char(0)
      else
        iErr=1952
        return
      end if
    else
      if(Mov_Command(1:1).eq.'%') return
    end if
    Command=' 'C
    CommObj=' 'C
    Argument=' 'C
    if(Mov_Command(1:1).eq.'?') then ! conditioned execution
      call ExtractStr(Mov_Command,-1,Cond1,lCond1)
      if(lCond1.lt.1) return 
      call ExtractStr(Mov_Command,-2,Cond,lCond)
      if((lCond.gt.2).or.(lCond.lt.1)) return 
      if((Cond(1:1).ne.'>').and.(Cond(1:1).ne.'<').and.(Cond(1:1).ne.'=')) return
      if(lCond.gt.1) then
        if(Cond(2:2).ne.'=') return
      end if
      call ExtractStr(Mov_Command,-3,Cond2,lCond2)
      if(lCond2.lt.1) then
        lCond2=1
        Cond2='0'
      end if
      call StrToRV(Cond1(1:lCond1),rCond1,iErr2)
      if(iErr2.ne.0) then
        call GetRch(Cond1,-1_4,idum,iErr2)
        if((iErr2.ne.0).or.(idum.lt.1)) return
        rCond1=rch(1)
      end if
      call StrToRV(Cond2(1:lCond2),rCond2,iErr2)
      if(iErr2.ne.0) then
        call GetRch(Cond2,-1_4,idum,iErr2)
        if((iErr2.ne.0).or.(idum.lt.1)) return
        rCond2=rch(1)
      end if
      select case(Cond(1:1))
      case('<')
        if(lCond.gt.1) then
          if(rCond1.gt.rCond2) return
        else
          if(rCond1.ge.rCond2) return
        end if
      case('>')
        if(lCond.gt.1) then
          if(rCond1.lt.rCond2) return
        else
          if(rCond1.le.rCond2) return
        end if
      case('=')
        if(rCond1.ne.rCond2) return
      case default
        return
      end select
    end if
    call ExtractStr(Mov_Command,1,Command,lCommand)
    iErr=1
    if(lCommand.lt.3) return
    iErr=0
    if(Command(1:3).eq.'end') return
    call ExtractStr(Mov_Command,2,CommObj,lCommObj)
    if(Command(1:3).eq.'exi') then
      if(lPerform) then
        if(lCommObj.gt.0) then
          if(CommObj(1:1).eq.'?') then
            call ExtractTxt(Mov_Command,1,Argument,lArgument)
            if(lArgument.lt.1) call ExtractStr(Mov_Command,3,Argument,lArgument)
            if(lArgument.lt.1) then
              idum=MessageBoxQQ('Exit movie processing?'C,'Movie command EXIt'C, &
                                MB$YesNo.or.MB$IconQuestion)
            else
              idum=MessageBoxQQ('Exit '//Argument(1:lArgument)//char(0),'Movie command EXIt'C, &
                                MB$YesNo.or.MB$IconQuestion)
            end if
            if(idum.eq.MB$IDYES) then
              idum=SetExitQQ(QWin$ExitNoPersist)
              iErr=99
            end if
          else
            idum=SetExitQQ(QWin$ExitNoPersist)
            iErr=99
          end if
        else
          idum=SetExitQQ(QWin$ExitNoPersist)
          iErr=99
        end if
      end if
      return
    end if
    iErr=2
    if(lCommObj.lt.1) return
    if(lCommObj.lt.3) then ! labels instead of objects might have shorter lengths: insert leading zeros
      if(lCommObj.eq.2) then
        CommObj(3:3)=CommObj(2:2)
        CommObj(2:2)=CommObj(1:1)
        CommObj(1:1)='0'
      else
        CommObj(3:3)=CommObj(1:1)
        CommObj(1:2)='00'
      end if
    end if
    call ExtractStr(Mov_Command,3,Argument,lArgument)
    iErr=0
    if(l4.and.l7) write(*,*) ' Execute:',Command(1:3),' ',CommObj(1:3)
    Select Case(Command(1:3))
    Case('ada')
      call MovAda(CommObj,lPerform,iErr)
    Case('add')
      call MovAdd(CommObj,lPerform,iErr)
    Case('blo')
      call MovBlo(CommObj,lPerform,iErr)
    Case('cle')
      call MovCle(CommObj,lPerform,iErr)
    Case('con')
      call MovCon(CommObj,lPerform,iErr)
    Case('cop')
      call MovCop(CommObj,lPerform,iErr)
    Case('del')
      call MovDel(CommObj,lPerform,iErr)
    Case('dra')
      call MovDra(CommObj,lPerform,iErr)
    Case('exc')
      call MovExc(CommObj,lPerform,iErr)
    Case('gen')
      call MovGen(CommObj,lPerform,iErr)
    Case('get')
      call MovGet(CommObj,lPerform,iErr)
    Case('got')
      call MovGot(lPerform,kComL,iErr)
    Case('if=')
      call MovIfE(lPerform,kComL,iErr)
    Case('if>')
      call MovIfG(lPerform,kComL,iErr)
    Case('if<')
      call MovIfS(lPerform,kComL,iErr)
    Case('inc')
      call MovInc(CommObj,lPerform,iErr)
    Case('ite')
      call MovIte(CommObj,lPerform,iErr)
    Case('loo')
      call MovLoo(lPerform,kComL,iErr)
    Case('lab')
      call MovLab(iErr)
    Case('mmp')
      call MovMmp(CommObj,lPerform,iErr)
    Case('mov')
      call MovMov(CommObj,lPerform,iErr)
    Case('mul')
      call MovMul(CommObj,lPerform,iErr)
    Case('pro')
      call MovPro(CommObj,lPerform,iErr)
    Case('rea')
      call MovRea(CommObj,lPerform,iErr)
    Case('ref')
      call MovRef(CommObj,lPerform,iErr)
    Case('ren')
      call MovRen(CommObj,lPerform,iErr)
    Case('rot')
      call MovRot(CommObj,lPerform,iErr)
    Case('run')
      call MovRun(CommObj,lPerform,iErr)
    Case('set')
      call MovSet(CommObj,lPerform,iErr)
    Case('sor')
      call MovSor(CommObj,lPerform,iErr)
    Case('sub')
      call MovSub(CommObj,lPerform,iErr)
    Case('wri')
      call MovWri(CommObj,lPerform,iErr)
    Case Default
      iErr=-9
      if(l4.and.l7) write(*,*) ' Command execution error: Unknown command: ',Command(1:3)
      return
    end Select
    if(l4.and.l5.and.(iErr.ne.0)) then
      if(iErr.eq.-9) then
        write(*,*) ' Command execution error: Unknown command object: ',CommObj(1:3)
      else
        write(*,*) ' Command execution error flag:',iErr
      end if
    end if
   ! iErr=0
  end Subroutine MovieCommand

  Subroutine MovAda(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr2,ir
    Real(8) r
    Logical lPerform
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('exp')
      if(lArgument.lt.1) return
      call ExtractStr(Mov_Command,3,Argument,lArgument)
      if((lArgument.lt.3).or.(.not.lPerform)) return
      call ExtractIV(Mov_Command,4,ir,iErr2)
      if(iErr2.eq.0) iModExpMin=ir
      call ExtractIV(Mov_Command,5,ir,iErr2)
      if(iErr2.eq.0) iModExpMax=ir
      call ExtractIV(Mov_Command,6,ir,iErr2)
      if(iErr2.eq.0) iModExpWFA=ir
      call ExtractIV(Mov_Command,7,ir,iErr2)
      if(iErr2.eq.0) iModExpWFE=ir
      call ExtractRV(Mov_Command,8,r,iErr2)
      if(iErr2.eq.0) fModExpFac=r
      call ExtractIV(Mov_Command,9,ir,iErr2)
      if(iErr2.eq.0) iModExpLoop=ir
      call ExtractIV(Mov_Command,10,ir,iErr2)
      if(iErr2.eq.0) iModExpBnd=ir
      call ExtractIV(Mov_Command,11,ir,iErr2)
      if(iErr2.eq.0) iModExpDom=ir
      call ExtractIV(Mov_Command,12,ir,iErr2)
      if(iErr2.eq.0) iModExpObj=ir
      call ExtractIV(Mov_Command,13,ir,iErr2)
      if(iErr2.eq.0) iModExpCon=ir
      call ExtractIV(Mov_Command,14,ir,iErr2)
      if(iErr2.eq.0) iModExpCol=ir
      Select Case(Argument(1:3))
      Case('bal')
        if(lPerform) call adaptExp2D2(.false.)
      Case('dep')
        if(lPerform) call adaptExp2D6(.false.)
      Case('fac')
        if(lPerform) call adaptExp2D4(.false.)
      Case('fic') ! delete expansions near fictitious boundaries (same domain on both sides)
        if(lPerform) call adaptExp2D0f(.false.)
      Case('ins')
        if(lPerform) call adaptExp2D0(.false.)
      Case('loc')
        if(lPerform) call adaptExp2D1(.false.)
      Case('ord')
        if(lPerform) call adaptExp2D7(.false.)
      Case('out')
        if(lPerform) call adaptExp2D3(.false.)
      Case('ran')
        if(lPerform) call adaptExp2D5(.false.)
      Case Default
        iErr=-9
        return
      end Select
      if(lPerform) then
        call CorrExpPar(1000_4)
        kExp=nExp
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovAda

  Subroutine MovAdd(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr2,idum,n,nx,ny,nz,i,j,k,i1,j1,k1,i2,j2,k2,l
    Integer(2) idu
    Real(8) r,x,y,z,t,d,x1,x2,y1,y2,z1,z2,rl(3),dmin,rmin(3),val(2)
    Complex(8) cf(10)
    Logical lPerform,ldum
    Character(256) sch
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('3do')
      call ExtractStr(Mov_Command,3,Argument,lArgument)
      if((lArgument.lt.3).or.(.not.lPerform)) return
      Select Case(Argument(1:3))
      Case('con')
        call ExtractRV(Mov_Command,4,z,iErr2) ! start z
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,d,iErr2) ! length
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,i,iErr2,nBnd,i2) ! boundary number
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,dmin,iErr2) ! multipole distance
        if(iErr2.ne.0) dmin=1.0d0
        call ExtractRV(Mov_Command,8,rl(1),iErr2) ! origin x
        if(iErr2.ne.0) rl(1)=0.0d0
        call ExtractRV(Mov_Command,9,rl(2),iErr2) ! origin y
        if(iErr2.ne.0) rl(2)=0.0d0
        call ExtractRV(Mov_Command,10,rl(3),iErr2) ! origin z
        if(iErr2.ne.0) rl(3)=1.0d0
        call ExtractRV(Mov_Command,11,r,iErr2) ! matching point aspect ratio
        if(iErr2.ne.0) r=0.3d0
        if(lPerform) call genObjCone(z,d,dmin,r,rl(1:3),i,i2)
      Case('cyl')
        call ExtractRV(Mov_Command,4,z,iErr2) ! start z
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,d,iErr2) ! length
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,i,iErr2,nBnd,i2) ! boundary number
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,r,iErr2) ! matching point aspect ratio
        if(iErr2.ne.0) r=1.0d0
        if(lPerform) call genObjCylinder(z,d,r,i,i2)
      Case('rec')
        call ExtractRV(Mov_Command,4,x,iErr2) ! x side length
        if(iErr2.ne.0) x=1.0d0
        call ExtractRV(Mov_Command,5,y,iErr2) ! y side length
        if(iErr2.ne.0) y=1.0d0
        call ExtractRV(Mov_Command,6,d,iErr2) ! matching point side
        if(iErr2.ne.0) d=1.0d0
        call ExtractRV(Mov_Command,7,z,iErr2) ! multipole density
        if(iErr2.ne.0) z=1.0d0
        call ExtractRV(Mov_Command,8,dmin,iErr2) ! multipole distance
        if(iErr2.ne.0) dmin=1.0d0
        call ExtractIV(Mov_Command,9,i,iErr2,nBnd) ! boundary number
        if(iErr2.ne.0) i=1
        call ExtractIV(Mov_Command,10,j,iErr2,nBnd) ! max. multipoles per side
        if(iErr2.ne.0) j=0
        call ExtractIV(Mov_Command,11,k,iErr2,nBnd) ! max. multipole order and degree
        if(iErr2.ne.0) k=1
        if(lPerform) call genObjRectangle(x,y,d,z,dmin,i,j,k)
      Case('spi')
        call ExtractRV(Mov_Command,4,x,iErr2) ! dr
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2) ! dphi
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,z,iErr2) ! dz
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,7,i,iErr2,nBnd,i2) ! boundary number
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,r,iErr2) ! matching point aspect ratio
        if(iErr2.ne.0) r=1.0d0
        if(lPerform) call genObjSpiral(x,y,z,r,i,i2)
      Case('tor')
        call ExtractRV(Mov_Command,4,y,iErr2) ! phi0
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,d,iErr2) ! dphi
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,i,iErr2,nBnd,i2) ! boundary number
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,r,iErr2) ! matching point aspect ratio
        if(iErr2.ne.0) r=1.0d0
        if(lPerform) call genObjTorus(y,d,r,i,i2,0)
      Case('tri')
        call ExtractRV(Mov_Command,4,x,iErr2) ! ax
        if(iErr2.ne.0) x=0.0d0
        call ExtractRV(Mov_Command,5,y,iErr2) ! ay
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2) ! az
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,x1,iErr2) ! bx
        if(iErr2.ne.0) x1=1.0d0
        call ExtractRV(Mov_Command,8,y1,iErr2) ! by
        if(iErr2.ne.0) y1=0.0d0
        call ExtractRV(Mov_Command,9,z1,iErr2) ! bz
        if(iErr2.ne.0) z1=0.0d0
        call ExtractRV(Mov_Command,10,x2,iErr2) ! cx
        if(iErr2.ne.0) x2=0.0d0
        call ExtractRV(Mov_Command,11,y2,iErr2) ! cy
        if(iErr2.ne.0) y2=1.0d0
        call ExtractRV(Mov_Command,12,z2,iErr2) ! cz
        if(iErr2.ne.0) z2=1.0d0
        call ExtractRV(Mov_Command,13,d,iErr2) ! matching point side
        if(iErr2.ne.0) d=1.0d0
        call ExtractRV(Mov_Command,14,t,iErr2) ! multipole density
        if(iErr2.ne.0) t=1.0d0
        call ExtractRV(Mov_Command,15,dmin,iErr2) ! multipole distance
        if(iErr2.ne.0) dmin=1.0d0
        call ExtractIV(Mov_Command,16,i,iErr2,nBnd) ! boundary number
        if(iErr2.ne.0) i=1
        call ExtractIV(Mov_Command,17,j,iErr2,nBnd) ! max. multipoles per side
        if(iErr2.ne.0) j=0
        call ExtractIV(Mov_Command,18,k,iErr2,nBnd) ! max. multipole order and degree
        if(iErr2.ne.0) k=1
        if(lPerform) call genObjTriangle(x,y,z,x1,y1,z1,x2,y2,z2,d,t,dmin,i,j,k)
      end Select
    Case('arc')
      if(lPerform) then
        call ExtractRV(Mov_Command,3,x,iErr2) ! origin 
        if(iErr2.ne.0) x=0.0d0
        call ExtractRV(Mov_Command,4,y,iErr2) ! origin
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,5,x1,iErr2) ! start point
        if(iErr2.ne.0) x1=1.0d0
        call ExtractRV(Mov_Command,6,y1,iErr2)
        if(iErr2.ne.0) y1=0.0d0
        call ExtractRV(Mov_Command,7,d,iErr2)  ! angle 
        if(iErr2.ne.0) d=90.0d0
        call ExtractIV(Mov_Command,8,i1,iErr2) ! left domain
        if(iErr2.ne.0) i1=0
        call ExtractIV(Mov_Command,9,i2,iErr2) ! right domain
        if(iErr2.ne.0) i2=1
        call ExtractIV(Mov_Command,10,i,iErr2) ! color
        if(iErr2.ne.0) i=1
        call ExtractIV(Mov_Command,11,k,iErr2) ! connection
        if(iErr2.ne.0) k=0
        call ExtractIV(Mov_Command,12,n,iErr2) ! number of matching points
        if(iErr2.ne.0) n=0
        call ExtractRV(Mov_Command,13,z1,iErr2) ! weight 1
        if(iErr2.ne.0) z1=1.0d0
        call ExtractRV(Mov_Command,14,z2,iErr2) ! weight 2
        if(iErr2.ne.0) z2=1.0d0
        call genBndArc(x,y,x1,y1,d,i1,i2,i,k,n,z1,z2)
      end if
    Case('bou')
      if(lArgument.lt.1) return
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,BndFileName,'MAX','BND')
        lAskBnd=.false.
        lInsertBnd=.true.
        call ExtractIV(Mov_Command,4,kInsObj,iErr2)
        if(iErr2.ne.0) kInsObj=nBnd
        kInsObj=max(0,min(nBnd,kInsObj))
        call OpenBoundary(.true.)
      end if
    Case('cir')
      if(lPerform) then
        call ExtractRV(Mov_Command,3,x,iErr2) ! origin 
        if(iErr2.ne.0) x=0.0d0
        call ExtractRV(Mov_Command,4,y,iErr2) ! origin
        if(iErr2.ne.0) y=0.0d0
        x1=x
        y1=y
        call ExtractRV(Mov_Command,5,d,iErr2)   ! radius 
        if(iErr2.ne.0) d=1.0d0
        call ExtractIV(Mov_Command,6,i1,iErr2)  ! left domain
        if(iErr2.ne.0) i1=0
        call ExtractIV(Mov_Command,7,i2,iErr2)  ! right domain
        if(iErr2.ne.0) i2=1
        call ExtractIV(Mov_Command,8,i,iErr2)   ! color
        if(iErr2.ne.0) i=1
        call ExtractIV(Mov_Command,9,k,iErr2)   ! connection
        if(iErr2.ne.0) k=0
        call ExtractIV(Mov_Command,10,n,iErr2)  ! number of matching points
        if(iErr2.ne.0) n=0
        call ExtractRV(Mov_Command,11,z1,iErr2) ! weight 1
        if(iErr2.ne.0) z1=1.0d0
        call ExtractRV(Mov_Command,12,z2,iErr2) ! weight 2
        if(iErr2.ne.0) z2=1.0d0
        call genBndArc(x,y,x1,y1,d,i1,i2,i,k,n,z1,z2)
      end if
    Case('exp')
      if(lArgument.lt.1) return
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ExpFileName,'MAX','EXP')
        lAskExp=.false.
        lInsertExp=.true.
        call ExtractIV(Mov_Command,4,kInsObj,iErr2)
        if(iErr2.ne.0) kInsObj=nExp
        kInsObj=max(0,min(nExp,kInsObj))
        call OpenExpansion(.true.)
      end if
    Case('fie')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        cFld=dcmplx(x,y)+cFld
      end if
    Case('fun')
      call ExtractIV(Mov_Command,3,i1,iErr2,nFunA,i2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        Fun(i1:i2,1:nFun)=x+Fun(i1:i2,1:nFun)
      end if
    Case('inh')
      if(lArgument.lt.1) return
      if(lPerform) then
        call AddInhibit(nInhibit+1,Argument(1:min(32,lArgument)),ldum)
      end if
    Case('lin')
      if(lPerform) then
        call ExtractRV(Mov_Command,3,x,iErr2) ! start point 
        if(iErr2.ne.0) x=0.0d0
        call ExtractRV(Mov_Command,4,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,5,x1,iErr2) ! end point
        if(iErr2.ne.0) x1=1.0d0
        call ExtractRV(Mov_Command,6,y1,iErr2)
        if(iErr2.ne.0) y1=0.0d0
        d=-1.0d0
        call ExtractIV(Mov_Command,7,i1,iErr2) ! left domain
        if(iErr2.ne.0) i1=0
        call ExtractIV(Mov_Command,8,i2,iErr2) ! right domain
        if(iErr2.ne.0) i2=1
        call ExtractIV(Mov_Command,9,i,iErr2) ! color
        if(iErr2.ne.0) i=1
        call ExtractIV(Mov_Command,10,k,iErr2) ! connection
        if(iErr2.ne.0) k=0
        call ExtractIV(Mov_Command,11,n,iErr2) ! number of matching points
        if(iErr2.ne.0) n=0
        call ExtractRV(Mov_Command,12,z1,iErr2) ! weight 1
        if(iErr2.ne.0) z1=1.0d0
        call ExtractRV(Mov_Command,13,z2,iErr2) ! weight 2
        if(iErr2.ne.0) z2=1.0d0
        call genBndArc(x,y,x1,y1,d,i1,i2,i,k,n,z1,z2)
      end if
    Case('obj')
      if(lArgument.lt.1) return
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ObjFileName,'MAX','3DO')
        lAskObj=.false.
        lInsertObj=.true.
        call ExtractIV(Mov_Command,4,kInsObj,iErr2)
        if(iErr2.ne.0) kInsObj=nObj
        kInsObj=max(0,min(nObj,kInsObj))
        call OpenObject(.true.)
      end if
    Case('pfd')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'sen') then
        call ExtractIV(Mov_Command,4,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,ny,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,nz,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,x1,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,x2,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,9,y1,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,10,y2,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,11,z1,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,12,z2,iErr2)
        if(iErr2.ne.0) z1=0.0d0
        call ExtractRV(Mov_Command,13,t,iErr2)
        if(iErr2.ne.0) t=0.0d0
        call ExtractRV(Mov_Command,14,d,iErr2)
        if(iErr2.ne.0) d=pBig
        if(lPerform) then
          n=nPFDsens
          call InsertPFDsens(nPFDsens,nx*ny*nz,ldum)
          if(ldum) then
            do i=1,nx
              x=x1+Dble(i-1)*(x2-x1)/Dble(max(1,nx-1))
              do j=1,ny
                y=y1+Dble(j-1)*(y2-y1)/Dble(max(1,ny-1))
                do k=1,nz
                  z=z1+Dble(k-1)*(z2-z1)/Dble(max(1,nz-1))
                  n=n+1
                  PFDsensX(n)=x
                  PFDsensY(n)=y
                  PFDsensZ(n)=z
                  PFDsensT(n)=t
                  PFDsensD(n)=d
                end do
              end do
            end do
          end if
        end if
      else if(Argument(1:3).eq.'sou') then
        call ExtractIV(Mov_Command,4,i1,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,i2,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,j1,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,7,j2,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,8,k1,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,9,k2,iErr2)
        if(iErr2.ne.0) return
        call ExtractStr(Mov_Command,10,sch,idum)
        if(lPerform) then
          n=nPFDsource
          nx=i2-i1+1
          ny=j2-j1+1
          nz=k2-k1+1
          call InsertPFDsource(nPFDsource,nx*ny*nz,ldum)
          if(ldum) then
            do i=i1,i2
              do j=j1,j2
                do k=k1,k2
                  n=n+1
                  iPFDs(n)=i
                  jPFDs(n)=j
                  kPFDs(n)=k
                  if(idum.gt.0) then
                    l=GetSLength(sch)
                    rl(1)=PFDxmin+Dble(iPFDs(n)-1)*PFDdx
                    rl(2)=PFDymin+Dble(jPFDs(n)-1)*PFDdy
                    rl(3)=PFDzmin+Dble(kPFDs(n)-1)*PFDdz
                    cCForm(0)=(0.0d0,1.0d0)
                    cCForm(1)=DCmplx(Pi,0.0d0)
                    cCForm(2)=DCmplx(Eps0,0.0d0)
                    cCForm(3)=DCmplx(Mue0,0.0d0)
                    cCForm(4)=DCmplx(Kw0,0.0d0)
                    cCForm(5)=DCmplx(Zw0,0.0d0)
                    pCForm(0)=(1.0d0,0.0d0)
                    vCForm(0)=DCmplx(Dble(n),0.0d0)
                    vCForm(1)=DCmplx(rl(1),0.0d0)
                    vCForm(2)=DCmplx(rl(2),0.0d0)
                    vCForm(3)=DCmplx(rl(3),0.0d0)
                    if(sch(1:3).eq.'mmp') then
                      call DistPtObj(0_2,0_2,rl(1:3),.true.,dmin,rmin,idu,val,.true.)
                      call GetLoccField(rl,idu,0,cf)
                      if(lgcFld) then
                        if(iHEGlobal.eq.1_2) then ! Hz
                          PFDsourceA(n)=cf(6)
                        else ! Ez
                          PFDsourceA(n)=cf(3)
                        end if
                      else ! 3D !!!!! improve !!!!
                        PFDsourceA(n)=cf(3)
                      end if
                    else
                      cFormu=cFormula(sch,l,cCForm,pCForm,vCForm,5,0,3,1,1,1,iErr2)
                      if(iErr2.eq.0) PFDsourceA(n)=cFormu(1)
                    end if
                  end if
                end do
              end do
            end do
          end if
        end if
      else
        return
      end if
    Case('p2d')
      call ExtractIV(Mov_Command,3,i1,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,j1,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,k1,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,d,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,x1,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,8,y1,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,9,x2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,10,y2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,11,r,iErr2)
      if(iErr2.ne.0) r=0.0d0
      if(lPerform) then
        call InsertParticle2D(nParticle2D,1_4,ldum)
        if(ldum) then
          tParticle2D(nParticle2D)%iBnd=Int2(i1)
          tParticle2D(nParticle2D)%iColMirror=Int2(j1)
          tParticle2D(nParticle2D)%iColSurface=Int2(k1)
          tParticle2D(nParticle2D)%sMass=d
          tParticle2D(nParticle2D)%Friction(1)=x1
          tParticle2D(nParticle2D)%Friction(2)=y1
          tParticle2D(nParticle2D)%Velocity(1)=x2
          tParticle2D(nParticle2D)%Velocity(2)=y2
          tParticle2D(nParticle2D)%RandomForce=r
          call PRTinitialize2D(nParticle2D)
          if(abs(tParticle2D(nParticle2D)%Mass).lt.pSmall) call InsertParticle2D(nParticle2D,-1_4,ldum)
        end if
      end if
    Case('p3d')
      call ExtractIV(Mov_Command,3,i1,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,j1,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,k1,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,d,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,x1,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,8,y1,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,9,x2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,10,y2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,11,z2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,12,r,iErr2)
      if(iErr2.ne.0) r=0.0d0
      if(lPerform) then
        call InsertParticle3D(nParticle3D,1_4,ldum)
        if(ldum) then
          tParticle3D(nParticle3D)%iObj=Int2(i1)
          tParticle3D(nParticle3D)%iColMirror=Int2(j1)
          tParticle3D(nParticle3D)%iColSurface=Int2(k1)
          tParticle3D(nParticle3D)%sMass=d
          tParticle3D(nParticle3D)%Friction(1)=x1
          tParticle3D(nParticle3D)%Friction(2)=y1
          tParticle3D(nParticle3D)%Velocity(1)=x2
          tParticle3D(nParticle3D)%Velocity(2)=y2
          tParticle3D(nParticle3D)%Velocity(3)=z2
          tParticle3D(nParticle3D)%RandomForce=r
          call PRTinitialize3D(nParticle3D)
          if(abs(tParticle3D(nParticle3D)%Mass).lt.pSmall) call InsertParticle3D(nParticle3D,-1_4,ldum)
        end if
      end if
    Case('win')
      if(lPerform.and.(nWin.lt.mWin)) call AddWindow(.true.)
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovAdd

  Subroutine MovBlo(CommObj,lPerform,iErr)
    Implicit none
    Real(8) x,y,r
    Integer(4) iErr,iErr2,ir,ir2,nx,nt,i,j,k
    Logical lPerform
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('all')
      if(lArgument.lt.1) return
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call blowAll(x,y,r)
    Case('bou')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'amp') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundaryAmp(ir,x,ir2)
      else if(Argument(1:3).eq.'rad') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call BloCornerR(ir,nx,r,ir2)
      else if(Argument(1:3).eq.'val') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundaryVal(ir,x,y,ir2)
      else
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          iDomBnd=0_2
          iColBnd=0_2
          iConBnd=0_2
          call blowBoundary(ir,x,y,r,ir2)
        end if
      end if
    Case('col')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call blowColor(ir,x,y,r)
    Case('con')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call blowConnection(ir,x,y,r)
    Case('cor')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nt,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call blowCorner(ir,nt,x,y,r,ir2)
    Case('dom')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call blowDomain(ir,x,y,r)
    Case('exp')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        iDomExp=0_2
        iColExp=0_2
        iConExp=0_2
        call blowExpansion(ir,x,y,r,ir2)
      end if
    Case('gri')
      if(lArgument.lt.1) return
      call ExtractRV(Mov_Command,3,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        if(lrGrd) then
          spacecFld(1:3,0:3)=spacecFld(1:3,0:3)*r
        else
          do k=1,nzcFld
            do j=1,nycFld
              do i=1,nxcFld
                rGrd(1,i,j,k)=rGrd(1,i,j,k)*r
                rGrd(2,i,j,k)=rGrd(2,i,j,k)*r
                rGrd(3,i,j,k)=rGrd(3,i,j,k)*r
              end do
            end do
          end do
        end if
      end if
    Case('win')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,Int4(nWin))
      if(iErr2.ne.0) return
      if(ir.lt.0) ir=nWin+ir+1
      ir=min(nWin,max(0,ir))
      call ExtractRV(Mov_Command,4,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call blowWindows(ir,r)
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovBlo

  Subroutine MovCle(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr2,ir,idum
    Logical lPerform,ldum
    Character(32) CommObj
    iErr=0
    Select Case(CommObj(1:3))
    Case('all')
      if(lPerform) then
        call StrToIV(Argument(1:lArgument),ir,iErr2,nExp)
        if(iErr2.eq.0) kExpFFD=ir
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.eq.0) kColFFD=ir
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.eq.0) kConFFD=ir
        call ExtractIV(Mov_Command,6,ir,iErr2,nDom)
        if(iErr2.eq.0) kDomFFD=ir
        call ExtractIV(Mov_Command,7,ir,iErr2)
        if(iErr2.eq.0) kParFFD=ir
        call ExtractStr(Mov_Command,8,Argument,lArgument)
        if(lArgument.gt.0) then
          if(Argument(1:1).eq.'e') lExpFFD=.true.
          if(Argument(1:1).eq.'f') lExpFFD=.false.
          lExcFFD=.false.
        end if
        if(lArgument.gt.1) then
          if(Argument(2:2).eq.'s') lExcFFD=.true.
          if(Argument(2:2).eq.'t') lExcFFD=.false.
        end if
        call AllocateIFld(ldum)
        if(.not.ldum) return
        call AllocateGrd(ldum)
        if(.not.ldum) return
        call AllocateFld(ldum)
        if(.not.ldum) return
        if(.not.lgcFld) lGet3DMat=.true.
        call ClearGrid(.true.)
        call ClearDomain(.true.)
        call ClearField(.true.)
        if(iPlane.eq.3) then
          nxrFld=nxcFld
          nyrFld=nycFld
        else if(iPlane.eq.2) then
          nxrFld=nxcFld
          nyrFld=nzcFld
        else
          nxrFld=nycFld
          nyrFld=nzcFld
        end if
        call GetrField(.false.)
      end if
    Case('cpu')
      if(lPerform) then
        call cpu_time(rMovCPU0) !time
        call date_and_time(charMoveTim(1),charMoveTim(2),charMoveTim(3),iMoveTim) !time
        rMovELA0=3600.0d0*Dble(iMoveTim(5))+60.0d0*Dble(iMoveTim(6))+Dble(iMoveTim(7))+0.001d0*Dble(iMoveTim(8)) !time
        rMovCPU1=rMovCPU0
        rMovELA1=rMovELA0
      end if
    Case('dom')
      if(lPerform) then
        call AllocateIFld(ldum)
        if(.not.ldum) return
        iBound=0_2 ! reset splines !!??
        if(.not.lgcFld) lGet3DMat=.true.
        call ClearDomain(.true.)
        call ClearField(.true.)
      end if
    Case('fie')
      if(lPerform) then
        call ExtractIV(Mov_Command,3,ir,iErr2,nExp)
        if(iErr2.eq.0) kExpFFD=ir
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.eq.0) kColFFD=ir
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.eq.0) kConFFD=ir
        call ExtractIV(Mov_Command,6,ir,iErr2,nDom)
        if(iErr2.eq.0) kDomFFD=ir
        call ExtractIV(Mov_Command,7,ir,iErr2)
        if(iErr2.eq.0) kParFFD=ir
        call ExtractStr(Mov_Command,8,Argument,lArgument)
        if(lArgument.gt.0) then
          if(Argument(1:1).eq.'e') lExpFFD=.true.
          if(Argument(1:1).eq.'f') lExpFFD=.false.
          lExcFFD=.false.
        end if
        if(lArgument.gt.1) then
          if(Argument(2:2).eq.'s') lExcFFD=.true.
          if(Argument(2:2).eq.'t') lExcFFD=.false.
        end if
        call AllocateFld(ldum)
        if(.not.ldum) return
        if(.not.lgcFld) lGet3DMat=.true.
        call ClearField(.true.)
        if(iPlane.eq.3) then
          nxrFld=nxcFld
          nyrFld=nycFld
        else if(iPlane.eq.2) then
          nxrFld=nxcFld
          nyrFld=nzcFld
        else
          nxrFld=nycFld
          nyrFld=nzcFld
        end if
        call GetrField(.false.)
      end if
    Case('fun')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if(iErr2.eq.0) ir=1
      call ExtractIV(Mov_Command,4,idum,iErr2)
      if(iErr2.eq.0) idum=2
      if(lPerform) then
        mFunA=max(1,ir)
	      mFun=max(2,idum)
        nFunA=max(0,ir)
	      nFun=max(0,idum)
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
        call AllocateFun(ldum)
      end if
    Case('gri')
      if(lPerform) then
        call AllocateGrd(ldum)
        if(.not.ldum) return
        call ClearGrid(.true.)
      end if
    Case('pfd')
      if(lPerform) then
        PFDwmax=0.0d0
        call DeAllocateCFD()
        call DeAllocatePFD()
        if(Allocated(nPFDsFld)) nPFDsFld=0
        if(Allocated(dPFDsens)) dPFDsens=0.0d0
        if(Allocated(cPFDsens)) cPFDsens=(0.0d0,0.0d0)
        lPFDalloc=.false.
      end if
    Case('p2d')
      if(lPerform) then
        call InsertParticle2D(nParticle2D,-nParticle2D,ldum)
      end if
    Case('p3d')
      if(lPerform) then
        call InsertParticle3D(nParticle3D,-nParticle3D,ldum)
      end if
    Case('win')
      if(lArgument.ge.1) then
        call StrToIV(Argument(1:lArgument),ir,iErr2,Int4(nWin))
        if(iErr2.ne.0) return
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.lt.3) sch(1:3)='fie'
      else
        ir=kWin
        sch(1:3)='fie'
      end if
      if(lPerform) then
        lWinFld(kWin)=.true.
        if(sch(1:3).eq.'fun') lWinFld(kWin)=.false.
        GRFtime=trFld
        if(ir.gt.0) then
          ir=min(nWin,ir)
          kWin=ir
          call DrawWindow(.true.)
        else
          if(ir.eq.0) ir=-nWin
          ir=max(-nWin,ir)
          do kWin=1,-ir
            call DrawWindow(.true.)
          end do
          kWin=-ir
        end if
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovCle

  Subroutine MovCon(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr2,ir,ir2,i,n,j,id,id2,ic,k,lS,iCl,iCn,iOb,lbot,ltop
    Real(8) r,v(3)
    Logical lPerform,ldum
    Character(32) CommObj,S1,S2
    iErr=0
    Select Case(CommObj(1:3))
    Case('2dm') ! convert 2d multipole to 2d monopole for multilayer
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2) ! expansion number
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,iCl,iErr2)         ! color number filter
      if(iErr2.ne.0) iCl=0
      call ExtractIV(Mov_Command,5,iCn,iErr2)         ! connection number filter
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,6,iOb,iErr2)         ! object number filter
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,7,n,iErr2)           ! number of layers
      if(iErr2.ne.0) n=2
      call ExtractIV(Mov_Command,8,lbot,iErr2)        ! bottom layer type
      if(iErr2.ne.0) lbot=0
      call ExtractIV(Mov_Command,9,ltop,iErr2)        ! top layer type
      if(iErr2.ne.0) ltop=0
      call ExtractRV(Mov_Command,10,r,iErr2)          ! Sommerfeld integration k
      if(iErr2.ne.0) r=1.0d0
      call ExtractRV(Mov_Command,11,v(1),iErr2)       ! integration accuracy
      if(iErr2.ne.0) v(1)=1.0d-6
      if(lPerform) then
        do i=ir,ir2
          if((tExp(i)%iTypE.ne.1).and.(tExp(i)%iTypE.ne.12)) Cycle ! only convert 2D multipoles or 2D monopoles for multilayer
          if((iCl.gt.0).and.(tExp(i)%iCol.ne.iCl)) Cycle
          if((iCl.lt.0).and.(tExp(i)%iCol.le.-iCl)) Cycle
          if((iCn.gt.0).and.(tExp(i)%iConn.ne.iCn)) Cycle
          if((iCn.lt.0).and.(tExp(i)%iConn.le.-iCn)) Cycle
          if((iOb.gt.0).and.(tExp(i)%iObj.ne.iOb)) Cycle
          if((iOb.lt.0).and.(tExp(i)%iObj.le.-iOb)) Cycle
          tExp(i)%iTypE=12             ! 2D monopole for multilayer
          tExp(i)%Plane(1:3,1:3)=0.0d0 ! standard orientation in x,y,z directions
          tExp(i)%Plane(1,1)=1.0d0
          tExp(i)%Plane(2,2)=1.0d0
          tExp(i)%Plane(3,3)=1.0d0
          tExp(i)%iE(1)=n              ! set additional parameters
          tExp(i)%iE(2)=lbot
          tExp(i)%iE(3)=ltop
          tExp(i)%iE(4:5)=0
          tExp(i)%rE(1)=r
          tExp(i)%rE(2)=v(1)
          tExp(i)%rE(3:5)=0.0d0
        end do
        call CorrExpPar(1000_4)
        kExp=nExp
      end if
    Case('3dm') ! convert 3d multipole to 3d dipole for multilayer
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2) ! expansion number
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,iCl,iErr2)         ! color number filter
      if(iErr2.ne.0) iCl=0
      call ExtractIV(Mov_Command,5,iCn,iErr2)         ! connection number filter
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,6,iOb,iErr2)         ! object number filter
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,7,n,iErr2)           ! number of layers
      if(iErr2.ne.0) n=2
      call ExtractIV(Mov_Command,8,lbot,iErr2)        ! bottom layer type
      if(iErr2.ne.0) lbot=0
      call ExtractIV(Mov_Command,9,ltop,iErr2)        ! top layer type
      if(iErr2.ne.0) ltop=0
      call ExtractIV(Mov_Command,10,id2,iErr2)       ! hor/vert dipoles (0:hor, 1:vert. 2: both
      if(iErr2.ne.0) id2=2
      call ExtractRV(Mov_Command,11,r,iErr2)          ! Sommerfeld integration k
      if(iErr2.ne.0) r=1.0d0
      call ExtractRV(Mov_Command,12,v(1),iErr2)       ! integration accuracy
      if(iErr2.ne.0) v(1)=1.0d-6
      if(lPerform) then
        do i=ir,ir2
          if((tExp(i)%iTypE.ne.6).and.(tExp(i)%iTypE.ne.13)) Cycle ! only convert 2D multipoles or 2D monopoles for multilayer
          if((iCl.gt.0).and.(tExp(i)%iCol.ne.iCl)) Cycle
          if((iCl.lt.0).and.(tExp(i)%iCol.le.-iCl)) Cycle
          if((iCn.gt.0).and.(tExp(i)%iConn.ne.iCn)) Cycle
          if((iCn.lt.0).and.(tExp(i)%iConn.le.-iCn)) Cycle
          if((iOb.gt.0).and.(tExp(i)%iObj.ne.iOb)) Cycle
          if((iOb.lt.0).and.(tExp(i)%iObj.le.-iOb)) Cycle
          tExp(i)%iTypE=13             ! 3D dipole for multilayer
          tExp(i)%Plane(1:3,1:3)=0.0d0 ! standard orientation in x,y,z directions
          tExp(i)%Plane(1,1)=1.0d0
          tExp(i)%Plane(2,2)=1.0d0
          tExp(i)%Plane(3,3)=1.0d0
          tExp(i)%iE(1)=n              ! set additional parameters
          tExp(i)%iE(2)=lbot
          tExp(i)%iE(3)=ltop
          tExp(i)%iE(4)=id2
          tExp(i)%iE(5)=0
          tExp(i)%rE(1)=r
          tExp(i)%rE(2)=v(1)
          tExp(i)%rE(3:5)=0.0d0
        end do
        call CorrExpPar(1000_4)
        kExp=nExp
      end if
    Case('fun') ! copy fun argument ir on cFld or dFld array argument nr id
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if((iErr2.ne.0).or.(ir.lt.1).or.(ir.gt.nFunA)) return
      call ExtractIV(Mov_Command,4,id,iErr2)
      if((iErr2.ne.0).or.(id.lt.1).or.(id.gt.ncFld)) return
      call ExtractStr(Mov_Command,5,S1,lS)
      if(lS.lt.1) S1(1:1)='r'
      if((S1(1:1).ne.'0').and.(S1(1:1).ne.'r').and.(S1(1:1).ne.'i').and.(S1(1:1).ne.'c')) return
      call ExtractStr(Mov_Command,6,S2,lS)
      if(lS.lt.3) then
        if(lS.lt.1) then
          S2(1:3)='xyz'
        else if(S2(1:1).eq.'x') then
          S2(1:3)='xyz'
        else
          S2(1:3)='zyx'
        end if
      end if
      if(lPerform) then
        n=0
        if(S2(1:3).eq.'zyx') then
          do k=1,nzcFld
            do j=1,nycFld
              do i=1,nxcFld
                n=n+1
                if(n.gt.nFun) return
                iFld(i,j,k)=1
                if(lfcFld) then
                  if(S1(1:1).eq.'0') then
                    cFld(id,i,j,k)=DCmplx(0.0d0,0.0d0)
                  else if(S1(1:1).eq.'r') then
                    r=DImag(cFld(id,i,j,k))
                    cFld(id,i,j,k)=DCmplx(Fun(ir,n),r)
                  else if(S1(1:1).eq.'i') then
                    r=Dble(cFld(id,i,j,k))
                    cFld(id,i,j,k)=DCmplx(r,Fun(ir,n))
                  else if(S1(1:1).eq.'c') then
                    if(n.gt.nFun) return
                    cFld(id,i,j,k)=DCmplx(Fun(ir,n),Fun(ir,n+1))
                    n=n+1
                  end if
                else
                  dFld(id,i,j,k)=Fun(ir,n)
                end if
              end do
            end do
          end do
        else
          do i=1,nxcFld
            do j=1,nycFld
              do k=1,nzcFld
                n=n+1
                if(n.gt.nFun) return
                iFld(i,j,k)=1
                if(lfcFld) then
                  if(S1(1:1).eq.'0') then
                    cFld(id,i,j,k)=DCmplx(0.0d0,0.0d0)
                  else if(S1(1:1).eq.'r') then
                    r=DImag(cFld(id,i,j,k))
                    cFld(id,i,j,k)=DCmplx(Fun(ir,n),r)
                  else if(S1(1:1).eq.'i') then
                    r=Dble(cFld(id,i,j,k))
                    cFld(id,i,j,k)=DCmplx(r,Fun(ir,n))
                  else if(S1(1:1).eq.'c') then
                    if(n.gt.nFun) return
                    cFld(id,i,j,k)=DCmplx(Fun(ir,n),Fun(ir,n+1))
                    n=n+1
                  end if
                else
                  dFld(id,i,j,k)=Fun(ir,n)
                end if
              end do
            end do
          end do
        end if
      end if
    Case('rin') ! convert ring into complex origin expansiom
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2) ! expansion number
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,id,iErr2,nDom,id2) ! domain number
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,ic,iErr2)          ! 0->Bes, 1->Han, 2->-Han, 3->2Han, <0->2Han+Bes(dom=-ic)
      if(iErr2.ne.0) ic=0
      call ExtractRV(Mov_Command,6,r,iErr2)           ! cut angle
      if(iErr2.ne.0) r=0.0d0
      call ExtractIV(Mov_Command,7,iCl,iErr2)         ! color number filter
      if(iErr2.ne.0) iCl=0
      call ExtractIV(Mov_Command,8,iCn,iErr2)         ! connection number filter
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,9,iOb,iErr2)         ! object number filter
      if(iErr2.ne.0) iOb=0
      if(lPerform) then
        n=3*(ir2-ir+1)
        i=ir-1
        do j=ir,n
          i=i+1
          if(i.gt.ir2) Exit
          if(tExp(i)%iTypE.ne.8) Cycle
          if((tExp(i)%iDom.lt.id).or.(tExp(i)%iDom.gt.id2)) Cycle
          if((iCl.gt.0).and.(tExp(i)%iCol.ne.iCl)) Cycle
          if((iCl.lt.0).and.(tExp(i)%iCol.le.-iCl)) Cycle
          if((iCn.gt.0).and.(tExp(i)%iConn.ne.iCn)) Cycle
          if((iCn.lt.0).and.(tExp(i)%iConn.le.-iCn)) Cycle
          if((iOb.gt.0).and.(tExp(i)%iObj.ne.iOb)) Cycle
          if((iOb.lt.0).and.(tExp(i)%iObj.le.-iOb)) Cycle
          v(1:3)=tExp(i)%Plane(1:3,3)
          tExp(i)%Plane(1:3,3)=tExp(i)%Plane(1:3,2)
          tExp(i)%Plane(1:3,2)=tExp(i)%Plane(1:3,1)
          tExp(i)%Plane(1:3,1)=v(1:3)
          tExp(i)%iE(5)=0
          tExp(i)%rE(2)=0.0d0
          tExp(i)%rE(3)=0.0d0
          tExp(i)%rE(4)=tExp(i)%rE(1)
          tExp(i)%rE(1)=tExp(i)%rE(5)
          tExp(i)%rE(5)=r
          tExp(i)%iTypE=6 ! complex origin multipole
          if(ic.eq.0) then
            tExp(i)%iTypE=7 ! complex origin Bessel
            tExp(i)%rE(5)=0.0d0
          end if
          if(ic.eq.2) then ! complex origin multipole with opposite direction
            tExp(i)%rE(4)=-tExp(i)%rE(4)
            tExp(i)%rE(5)=-r
          end if
          if((ic.gt.2).or.(ic.lt.0)) then ! add complex origin multipole with opposite direction
            call copyExp(i,i,0.0d0,0.0d0,-3_4,ldum)
            i=i+1
            ir2=ir2+1
            tExp(i)%rE(4)=-tExp(i)%rE(4)
            tExp(i)%rE(5)=-r
            if(ic.lt.0) then ! add complex origin Bessel with domain number -ic
              call copyExp(i-1,i,0.0d0,0.0d0,-3_4,ldum)
              i=i+1
              ir2=ir2+1
              tExp(i)%iTypE=7
              tExp(i)%iDom=min(max(1,-ic),nDom)
              tExp(i)%rE(5)=0.0d0
            end if
          end if
        end do
        call CorrExpPar(1000_4)
        kExp=nExp
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovCon

  Subroutine MovCop(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) i,iErr,ir,kr,nx,ny,n,iC,iErr2
    Real(8) dx,dy
    Logical lPerform,ldum
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('bou')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,kr)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nx,iErr2,nBnd)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,iC,iErr2)
      if((iErr2.ne.0).or.(iC.lt.0_4)) iC=-2_4
      call ExtractRV(Mov_Command,6,dx,iErr2)
      if(iErr2.ne.0) dx=0.0d0
      call ExtractRV(Mov_Command,7,dy,iErr2)
      if(iErr2.ne.0) dy=0.0d0
      ir=min(nBnd,max(1,ir))
      nx=min(nBnd,max(0,nx))
      kr=min(nBnd,max(1,kr))
      if(kr.lt.ir) then
        i=ir
        ir=kr
        kr=i
      end if
      if(lPerform) then
        do i=kr,ir,-1
          call CopyBnd(i,nx,dx,dy,iC,ldum)
        end do
      end if
    Case('col')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nx,iErr2,nBnd)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,ny,iErr2,nExp)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,6,iC,iErr2)
      if((iErr2.ne.0).or.(iC.lt.0_4)) iC=-2_4
      call ExtractRV(Mov_Command,7,dx,iErr2)
      if(iErr2.ne.0) dx=0.0d0
      call ExtractRV(Mov_Command,8,dy,iErr2)
      if(iErr2.ne.0) dy=0.0d0
      nx=min(nBnd,max(0,nx))
      ny=min(nExp,max(0,ny))
      if(lPerform) then
        call CopyColor(ir,iC,nx,ny,dx,dy)
      end if
    Case('con')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nx,iErr2,nBnd)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,ny,iErr2,nExp)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,6,n,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,7,iC,iErr2)
      if((iErr2.ne.0).or.(iC.lt.0_4)) iC=-2_4
      call ExtractRV(Mov_Command,8,dx,iErr2)
      if(iErr2.ne.0) dx=0.0d0
      call ExtractRV(Mov_Command,9,dy,iErr2)
      if(iErr2.ne.0) dy=0.0d0
      nx=min(nBnd,max(0,nx))
      ny=min(nExp,max(0,ny))
      if(lPerform) then
        i=nBnd
        do while(i.gt.0)
          if((ir.eq.0).or.((ir.gt.0).and.(tBnd(i)%iConn.eq.ir)).or.((ir.lt.0).and.(tBnd(i)%iConn.le.-ir))) then
            call CopyBnd(i,nx,dx,dy,-1_4,ldum)
            tBnd(nx+1)%iConn=-32000
            if((i.le.nx).or.(i.eq.1)) i=i-1
          else
            i=i-1
          end if
        end do
        do i=1,nBnd
          if(tBnd(i)%iConn.eq.-32000) tBnd(i)%iConn=n
          if(tBnd(i)%iCol.eq.-1_4) tBnd(i)%iCol=Int2(iC)
        end do
        i=nExp
        do while(i.gt.0)
          if((ir.eq.0).or.((ir.gt.0).and.(tExp(i)%iConn.eq.ir)).or.((ir.lt.0).and.(tExp(i)%iConn.le.-ir))) then
            call CopyExp(i,ny,dx,dy,-1_4,ldum)
            tExp(ny+1)%iConn=-32000
            if((i.le.ny).or.(i.eq.1)) i=i-1
          else
            i=i-1
          end if
        end do
        do i=1,nExp
          if(tExp(i)%iConn.eq.-32000) tExp(i)%iConn=n
          if(tExp(i)%iCol.eq.-1_4) tExp(i)%iCol=Int2(iC)
        end do
      end if
    Case('dom')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nx,iErr2,nBnd)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,ny,iErr2,nExp)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,6,n,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,7,iC,iErr2)
      if((iErr2.ne.0).or.(iC.lt.-10_4)) iC=-2_4
      call ExtractRV(Mov_Command,8,dx,iErr2)
      if(iErr2.ne.0) dx=0.0d0
      call ExtractRV(Mov_Command,9,dy,iErr2)
      if(iErr2.ne.0) dy=0.0d0
      nx=min(nBnd,max(0,nx))
      ny=min(nExp,max(0,ny))
      if(lPerform) then
        i=nBnd
        do while(i.gt.0)
          if((ir.eq.0).or.((ir.gt.0).and.((tBnd(i)%ilDom.eq.ir).or.(tBnd(i)%irDom.eq.ir))).or. &
          & ((ir.lt.0).and.((tBnd(i)%ilDom.le.-ir).or.(tBnd(i)%irDom.le.-ir)))) then
            call CopyBnd(i,nx,dx,dy,-1_4,ldum)
            if(((ir.gt.0).and.(tBnd(i)%ilDom.eq.ir)).or.((ir.lt.0).and.(tBnd(i)%ilDom.le.-ir))) tBnd(nx+1)%ilDom=-32000
            if(((ir.gt.0).and.(tBnd(i)%irDom.eq.ir)).or.((ir.lt.0).and.(tBnd(i)%irDom.le.-ir))) tBnd(nx+1)%irDom=-32000
            if((i.le.nx).or.(i.eq.1)) i=i-1
          else
            i=i-1
          end if
        end do
        do i=1,nBnd
          if(tBnd(i)%ilDom.eq.-32000_2) tBnd(i)%ilDom=Int2(n)
          if(tBnd(i)%irDom.eq.-32000_2) tBnd(i)%irDom=Int2(n)
          if(tBnd(i)%iCol.eq.-1_4) tBnd(i)%iCol=Int2(iC)
        end do
        i=nExp
        do while(i.gt.0)
          if((ir.eq.0).or.((ir.gt.0).and.(tExp(i)%iDom.eq.ir)).or.((ir.lt.0).and.(tExp(i)%iDom.le.-ir))) then
            call CopyExp(i,ny,dx,dy,-1_4,ldum)
            tExp(ny+1)%iDom=-32000_2
            if((i.le.ny).or.(i.eq.1)) i=i-1
          else
            i=i-1
          end if
        end do
        do i=1,nExp
          if(tExp(i)%iDom.eq.-32000_2) tExp(i)%iDom=Int2(n)
          if(tExp(i)%iCol.eq.-1_4) tExp(i)%iCol=Int2(iC)
        end do
      end if
    Case('exp')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,kr)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nx,iErr2,nExp)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,iC,iErr2)
      if((iErr2.ne.0).or.(iC.lt.0_4)) iC=-2_4
      call ExtractRV(Mov_Command,6,dx,iErr2)
      if(iErr2.ne.0) dx=0.0d0
      call ExtractRV(Mov_Command,7,dy,iErr2)
      if(iErr2.ne.0) dy=0.0d0
      ir=min(nExp,max(1,ir))
      nx=min(nExp,max(0,nx))
      kr=min(nExp,max(1,kr))
      if(kr.lt.ir) then
        i=ir
        ir=kr
        kr=i
      end if
      if(lPerform) then
        do i=kr,ir,-1
          call CopyExp(i,nx,dx,dy,iC,ldum)
        end do
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovCop

  Subroutine MovDel(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,ir,ir2,kr,iErr2,i
    Real(8) r
    Logical lPerform,ldum
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('bou')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      ir=min(nBnd,max(1,ir))
      call ExtractIV(Mov_Command,4,kr,iErr2,nBnd)
      if(iErr2.ne.0) kr=ir2
      kr=min(nBnd,max(1,kr))
      if(kr.lt.ir) then
        i=ir
        ir=kr
        kr=i
      end if
      if(lPerform) call InsertBnd(ir,-(kr-ir+1),ldum)
    Case('col')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,sch,iErr2)
      if(iErr2.lt.1) sch(1:1)='a'
      if(lPerform) then
        if(sch(1:1).ne.'e') then
          i=nBnd
          do while((i.gt.0).and.(nBnd.gt.1))
            if((ir.eq.0).or.((ir.gt.0).and.(tBnd(i)%iCol.eq.ir)).or.((ir.lt.0).and.(tBnd(i)%iCol.le.-ir))) &
            & call InsertBnd(i,-1,ldum)
            i=i-1
          end do
        end if
        if(sch(1:1).ne.'b') then
          i=nExp
          do while((i.gt.0).and.(nExp.gt.1))
            if((ir.eq.0).or.((ir.gt.0).and.(tExp(i)%iCol.eq.ir)).or.((ir.lt.0).and.(tExp(i)%iCol.le.-ir))) &
            & call InsertExp(i,-1,ldum)
            i=i-1
          end do
        end if
      end if
    Case('con')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        i=nBnd
        do while((i.gt.0).and.(nBnd.gt.1))
          if((ir.eq.0).or.((ir.gt.0).and.(tBnd(i)%iConn.eq.ir)).or.((ir.lt.0).and.(tBnd(i)%iConn.le.-ir))) &
          & call InsertBnd(i,-1,ldum)
          i=i-1
        end do
        i=nExp
        do while((i.gt.0).and.(nExp.gt.1))
          if((ir.eq.0).or.((ir.gt.0).and.(tExp(i)%iConn.eq.ir)).or.((ir.lt.0).and.(tExp(i)%iConn.le.-ir))) &
          & call InsertExp(i,-1,ldum)
          i=i-1
        end do
      end if
    Case('dom')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
      if(iErr2.ne.0) return
      if(lPerform) then
        i=nBnd
        do while((i.gt.0).and.(nBnd.gt.1))
          if((ir.eq.0).or.((ir.gt.0).and.((tBnd(i)%ilDom.eq.ir).or.(tBnd(i)%irDom.eq.ir))).or. &
          & ((ir.lt.0).and.((tBnd(i)%ilDom.le.-ir).or.(tBnd(i)%irDom.le.-ir)))) call InsertBnd(i,-1,ldum)
          i=i-1
        end do
        i=nExp
        do while((i.gt.0).and.(nExp.gt.1))
          if((ir.eq.0).or.((ir.gt.0).and.(tExp(i)%iDom.eq.ir)).or.((ir.lt.0).and.(tExp(i)%iDom.le.-ir))) &
          & call InsertExp(i,-1,ldum)
          i=i-1
        end do
      end if
    Case('exp')
      if(lArgument.lt.1) return
      iErr2=1
      if(lArgument.gt.3) then
        if(Argument(1:3).eq.'dep') then
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.eq.0) dep_delExp=r
          if(lPerform) call DelDependExp(.true.)
          iErr2=0
        end if
      end if
      if(iErr2.eq.1) then
        call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,4,kr,iErr2,nExp)
        if(iErr2.ne.0) kr=ir2
        kr=min(nExp,max(1,kr))
        if(kr.lt.ir) then
          i=ir
          ir=kr
          kr=i
        end if
        if(lPerform) call InsertExp(ir,-(kr-ir+1),ldum)
      end if
    Case('fun')
      if(lArgument.lt.3) return
      iErr2=1
      if(lArgument.ge.3) then
        if(Argument(1:3).eq.'arg') then
          call ExtractIV(Mov_Command,4,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            iFunA2=ir
            if(iFunA2.gt.mFunA) iFunA2=mFunA
            if(iFunA2.le.0) iFunA2=mFunA-iFunA2
            if((iFunA2.gt.0).and.(mFunA.gt.1)) then
              do ir=iFunA2,mFunA-1
                FunATitle(ir)=FunATitle(ir+1)
                Fun(ir,1:mFun)=Fun(ir+1,1:mFun)
              end do
              call IncreaseFun(0,-1,ldum)
              iFunA2=min(iFunA2,mFunA)
            end if
          end if
        end if
      end if
    Case('inh')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nInhibit)
      if(iErr2.ne.0) return
      if(lPerform) then
        call DelInhibit(ir)
      end if
    Case('obj')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nObj)
      if(iErr2.ne.0) return
      ir=min(nObj,max(1,ir))
      call ExtractIV(Mov_Command,4,kr,iErr2,nObj)
      if(iErr2.ne.0) kr=ir
      kr=min(nObj,max(1,kr))
      if(kr.lt.ir) then
        i=ir
        ir=kr
        kr=i
      end if
      if(lPerform) then
        call InsertObj(ir,-(kr-ir+1),ldum)
        do i=nExp-1,1,-1
          if(tExp(i)%iObj.ne.ir) Cycle
          call InsertExp(i,-1,ldum)
        end do
      end if
    Case('pfd')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'sen') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsens,kr)
        if(iErr2.ne.0) return
        if(lPerform) then
          kr=min(nPFDsens,max(1,kr))
          if(kr.lt.ir) then
            i=ir
            ir=kr
            kr=i
          end if
          call InsertPFDsens(ir,-(kr-ir+1),ldum)
        end if
      else if(Argument(1:3).eq.'sou') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsource,kr)
        if(iErr2.ne.0) return
        if(lPerform) then
          kr=min(nPFDsource,max(1,kr))
          if(kr.lt.ir) then
            i=ir
            ir=kr
            kr=i
          end if
          call InsertPFDsource(ir,-(kr-ir+1),ldum)
        end if
      else
        return
      end if
    Case('p2d')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        call InsertParticle2D(ir,-1_4,ldum)
      end if
    Case('p3d')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        call InsertParticle3D(ir,-1_4,ldum)
      end if
    Case('win')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,Int4(nWin))
      if(iErr2.ne.0) return
      ir=min(nWin,max(1,ir))
      if(nWin.lt.2) return
      if(lPerform) then
        kWin=ir
        call DeleteWindow(.true.)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovDel

  Subroutine MovDra(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,ir,ir2,iErr2,nx,ny,nz,idum,kOb,istat,m
    Integer(2) iOK,iO1,iO2,ic
    Real(8) r,r0(3),d,x,y
    Real(8), allocatable:: Lines(:,:,:)
    Logical lPerform,lin
    Character(32) CommObj,sch1,sch2
    Character(3) chr3
    Select Case(CommObj(1:3))
    Case('bou')
      if(lArgument.lt.1) then
        ir=0
        r=ErrorScale
      else
        call ExtractIV(Mov_Command,3,ir,iErr2,nBnd)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,4,r,iErr2)
      end if
      if(lPerform) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        if(iErr2.eq.0) ErrorScale=r
        call DrawBoundary(ir)
      end if
    Case('cir')
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,r,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,6,ir,iErr2)
      if(iErr2.ne.0) ir=1
      if(lPerform) then
        ic=SetColor(Int2(ir))
        call DrawCircle(x,y,r)
        ic=SetColor(ic)
      end if
    Case('col')
      if(lArgument.lt.1) then
        ir=0
      else
        call ExtractIV(Mov_Command,3,ir2,iErr2,235,m)
        if(iErr2.ne.0) return
      end if
      if(lPerform) then
        do ir=ir2,m
          iDomBnd=0_2
          iColBnd=Int2(ir)
          iConBnd=0_2
          call DrawBoundary(0)
          iColBnd=0_2
          iDomExp=0_2
          iColExp=Int2(ir)
          iConExp=0_2
          call DrawExpansion(0)
          iColExp=0_2
        end do
      end if
    Case('con')
      if(lArgument.lt.1) then
        ir=0
      else
        call ExtractIV(Mov_Command,3,ir2,iErr2,3200,m)
        if(iErr2.ne.0) return
      end if
      if(lPerform) then
        do ir=ir2,m
          iDomBnd=0_2
          iConBnd=Int2(ir)
          iColBnd=0_2
          call DrawBoundary(0)
          iConBnd=0_2
          iDomExp=0_2
          iConExp=Int2(ir)
          iColExp=0_2
          call DrawExpansion(0)
          iConExp=0_2
        end do
      end if
    Case('der')
      if(lPerform) then
        call DrawField(.true.)
        if(lGRCallocated) call GRCflush()
      end if
    Case('dom')
      if(lArgument.lt.1) then
        ir=0
      else
        call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
        if(iErr2.ne.0) return
      end if
      if(lPerform) then
        iDomBnd=Int2(ir)
        iColBnd=0_2
        iConBnd=0_2
        call DrawBoundary(0)
        iDomBnd=0_2
        iDomExp=Int2(ir)
        iColExp=0_2
        iConExp=0_2
        call DrawExpansion(0)
        iDomExp=0_2
      end if
    Case('exp')
      if(lArgument.lt.1) then
        ir=1
        ir2=nExp
      else
        call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
        if(iErr2.ne.0) return
      end if
      if(lPerform) then
        iDomExp=0_2
        iColExp=0_2
        iConExp=0_2
        do idum=ir,ir2
          call DrawExpansion(idum)
        end do
      end if
    Case('fie')
      if(lArgument.ge.1) then
        if(Argument(1:1).eq.'l') then ! field lines and tubes
          call ExtractRV(Mov_Command,4,r0(1),iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,5,r0(2),iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,r0(3),iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,r,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,8,d,iErr2)
          if(iErr2.ne.0) return
          call ExtractIV(Mov_Command,9,nx,iErr2)
          if(iErr2.ne.0) return
          call ExtractIV(Mov_Command,10,ny,iErr2)
          if(iErr2.ne.0) return
          call ExtractIV(Mov_Command,11,nz,iErr2)
          if(iErr2.ne.0) nz=1
        else if((Argument(1:1).ne.'c').and.(Argument(1:1).ne.'g')) then ! neither "current" nor "gradient" field
          call setitrFld(lperform,Argument(1:lArgument),lArgument,iErr2)
          if(iErr2.ne.0) return
          call ExtractStr(Mov_Command,4,Argument,lArgument)
          if(lArgument.gt.0) then
            if(lPerform) call setlxyzrFld(lPerform,Argument(1:lArgument),lArgument,iErr2)
            call ExtractStr(Mov_Command,5,Argument,lArgument)
            if(lPerform.and.(lArgument.gt.0)) then
              if(Argument(1:1).eq.'a') then
                larFld=.true.
                lprFld=.false.
              else if(Argument(1:1).eq.'b') then
                larFld=.true.
                lprFld=.true.
              else if(Argument(1:1).eq.'p') then
                larFld=.false.
                lprFld=.true.
              else
                larFld=.false.
                lprFld=.false.
              end if
            else
              Argument(1:1)='0'
            end if
          end if
        end if
      end if
      if(lPerform) then
        if(Argument(1:1).eq.'l') then
          if(nx.lt.1) return
          nz=min(max(0,nz),235)
          Allocate(Lines(3,0:ny,nx),stat=iErr)
          if(iErr.ne.0) return
          idum=ny
          call getFieldTube(r0,r,d,nx,idum,Lines,ny)
          if(ny.gt.1) then
            call GetKWin(.false.)
            istat=SetActiveQQ(10+kWin)
            do idum=1,nx
              call Draw3DPolyLine(Lines(1:3,0:ny,idum),ny+1,Int2(nz))
            end do
            do idum=0,ny
              call Draw3DPolyLineC(Lines(1:3,idum,1:nx),nx,Int2(nz))
            end do
          end if
          DeAllocate(Lines,stat=iErr)
        else
          if(lArgument.ge.1) then
            if(iPlane.eq.3) then
              nxrFld=nxcFld
              nyrFld=nycFld
            else if(iPlane.eq.2) then
              nxrFld=nxcFld
              nyrFld=nzcFld
            else
              nxrFld=nycFld
              nyrFld=nzcFld
            end if
          end if
          if(Argument(1:1).eq.'g') then
            call getGradrField()
          end if
          if((Argument(1:1).ne.'c').and.(Argument(1:1).ne.'g')) then
            call GetrField(.false.)
          end if
          call DrawField(.true.)
          if(lGRCallocated) call GRCflush()
        end if
      end if
    Case('fun')
      if(lArgument.ge.1) then
        call StrToIV(Argument(1:lArgument),nx,iErr2,nFunA)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,4,ny,iErr2,nFunA)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nz,iErr2)
        if(iErr2.ne.0) nz=-1
        call ExtractRV(Mov_Command,6,x,iErr2)
        if(iErr2.ne.0) x=1.0d0
        call ExtractRV(Mov_Command,7,y,iErr2)
        if(iErr2.ne.0) y=1.0d0
        call ExtractIV(Mov_Command,8,ir,iErr2)
        if(iErr2.ne.0) ir=1
        call ExtractIV(Mov_Command,9,ir2,iErr2)
        if(iErr2.ne.0) ir2=nFun
        if(lPerform) then
          iFunA1=Min(nFunA,Max(0,nx))
          iFunA2=Min(nFunA,Max(0,ny))
          if(nz.ge.0) iFunStyleColor=Int2(max(-1_4,min(235_4,nz)))
          iFun1=Min(Max(1,ir),nFun)
          iFun2=Min(Max(iFun1,ir2),nFun)
        end if
      end if
      if(lPerform) call DrawFunction(.true.,x,y)
    Case('lin')
      call ExtractStr(Mov_Command,3,sch,idum)
      if(idum.lt.1) return
      select case(sch(1:1))
      case('h')
        call ExtractRV(Mov_Command,4,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.ne.0) ir=1
        r0(2)=y
        x=WinXmin(kWin)
        r0(1)=WinXmax(kWin)
      case('v')
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.ne.0) ir=1
        r0(1)=x
        y=WinYmin(kWin)
        r0(2)=WinYmax(kWin)
      case default
        call ExtractRV(Mov_Command,3,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,4,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,r0(1),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r0(2),iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,7,ir,iErr2)
        if(iErr2.ne.0) ir=1
      end select
      if(lPerform) then
        ic=SetColor(Int2(ir))
        call DrawLine(x,y,r0(1),r0(2))
        ic=SetColor(ic)
      end if
    Case('obj')
      call ExtractIV(Mov_Command,3,kOb,iErr2,nObj)
      if(iErr2.ne.0) kOb=iDraOBJ
      if(lPerform) then
        iDraOBJ=min(nObj,max(-nObj,kOb))
        call DrawObject(iDraOBJ)
      end if
    Case('ogl')
      call ExtractIV(Mov_Command,3,kOb,iErr2,nObj)
      if(iErr2.ne.0) kOb=iDraOBJ
      call ExtractStr(Mov_Command,4,sch,idum)
      if(lPerform) then
        iDraOBJ=min(nObj,max(-nObj,kOb))
        if(idum.gt.0) then
          lCHGLlist=.false.
          do ir=1,idum
            select case(sch(ir:ir))
            case('a')
              lCHGLlist(1)=.true.
            case('s')
              lCHGLlist(2)=.true.
            case('g')
              lCHGLlist(3)=.true.
            case('i')
              lCHGLlist(4)=.true.
            case('v')
              lCHGLlist(5)=.true.
            case('e')
              lCHGLlist(6)=.true.
            case('t')
              lCHGLlist(7)=.true.
            case('p')
              lCHGLlist(8)=.true.
            case('x')
              lCHGLlist(9)=.true.
            end select
          end do
        end if
        lDrawOGL=.true.
      end if
    Case('pfd')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'sen') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsens,ir2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) nx=3
        if(lPerform) then
          call DrawPFDsensor(ir,ir2,nx)
        end if
      else if(Argument(1:3).eq.'sou') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsource,ir2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) nx=2
        if(lPerform) then
          call DrawPFDsource(ir,ir2,nx)
        end if
      end if
    Case('tex')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),nx,iErr2)
      if(iErr2.ne.0) nx=10
      call ExtractIV(Mov_Command,4,ny,iErr2)
      if(iErr2.ne.0) ny=10
      call ExtractIV(Mov_Command,5,ir,iErr2)
      if(iErr2.ne.0) ir=1
      call ExtractTxt(Mov_Command,1,sch,idum)
      if(idum.le.0) return
      if(lPerform) then
        call drawTxt(Int2(nx),Int2(ny),Int2(ir),sch(1:idum))
      end if
    Case('var')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),nx,iErr2)
      if(iErr2.ne.0) nx=10
      call ExtractIV(Mov_Command,4,ny,iErr2)
      if(iErr2.ne.0) ny=10
      call ExtractIV(Mov_Command,5,ir,iErr2)
      if(iErr2.ne.0) ir=1
      call ExtractStr(Mov_Command,6,sch,idum)
      if(idum.lt.3) return
      if(lPerform) then
        if(sch(1:3).eq.'loc') then
          call ExtractIV(Mov_Command,7,nz,iErr2,nExp)
          if(iErr2.ne.0) nz=nExp
          if(nz.lt.0) nz=nExp-nz+1
          nz=max(1,min(nExp,nz))
          call rswrit2(Dble(tExp(nz)%plane(1,0)),iWriteDigits,sch1,iO1)
          call rswrit2(Dble(tExp(nz)%plane(2,0)),iWriteDigits,sch2,iO2)
          iOK=iO1+iO2+5
          sch(1:1)='('
          sch(2:iO1+1)=sch1(1:iO1)
          sch(iO1+2:iO1+2)=','
          sch(iO1+3:iOK-3)=sch1(1:iO2)
          sch(iOK-2:iOK)=',0)'
          if(dabs(Dble(tExp(nz)%plane(3,0))).gt.pSmall) then
            call rswrit2(Dble(tExp(nz)%plane(2,0)),iWriteDigits,sch2,iO2)
            iO1=iOK-1
            iOK=iOK+iO2-1
            sch(iO1:iOK-1)=sch1(1:iO2)
            sch(iOK:iOK)=')'
          end if
        else
          chr3(1:3)=sch(1:3)
          call GetRch(chr3,6_4,m,iErr2)
          if(iErr2.ne.0) return
          if(m.eq.2) then
            call rswrit2(rch(1),iWriteDigits,sch1,iO1)
            call rswrit2(rch(2),iWriteDigits,sch2,iO2)
            iOK=iO1+iO2+3
            sch(1:1)='('
            sch(2:iO1+1)=sch1(1:iO1)
            sch(iO1+2:iO1+2)=','
            sch(iO1+3:iOK-1)=sch2(1:iO2)
            sch(iOK:iOK)=')'
          else if(m.eq.3) then
            call rswrit2(rch(1),iWriteDigits,sch1,iO1)
            call rswrit2(rch(2),iWriteDigits,sch2,iO2)
            iOK=iO1+iO2+3
            sch(1:1)='('
            sch(2:iO1+1)=sch1(1:iO1)
            sch(iO1+2:iO1+2)=','
            sch(iO1+3:iOK-1)=sch1(1:iO2)
            sch(iOK:iOK)=','
            call rswrit2(rch(3),iWriteDigits,sch2,iO2)
            iO1=iOK+1
            iOK=iOK+iO2+1
            sch(iO1:iOK-1)=sch1(1:iO2)
            sch(iOK:iOK)=')'
          else
            call rswrit2(rch(1),iWriteDigits,sch,iOK)
          end if
        end if
        call drawTxt(Int2(nx),Int2(ny),Int2(ir),sch(1:iOK))
      end if
    Case('win')
      if(lArgument.ge.1) then
        call StrToIV(Argument(1:lArgument),ir,iErr2,Int4(nWin))
        if(iErr2.ne.0) return
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.lt.3) sch(1:3)='fie'
      else
        ir=kWin
        sch(1:3)='fie'
      end if
      if(lPerform) then
        lWinFld(kWin)=.true.
        if(sch(1:3).eq.'fun') lWinFld(kWin)=.false.
        GRFtime=trFld
        lin=lWinInit
        lWinInit=.false.
        if(ir.gt.0) then
          ir=min(nWin,ir)
          kWin=ir
          call DrawWindow(.true.)
        else
          if(ir.eq.0) ir=-nWin
          ir=max(-nWin,ir)
          do kWin=1,-ir
            call DrawWindow(.true.)
          end do
          kWin=-ir
        end if
        lWinInit=lin
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovDra

  Subroutine MovExc(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr0,i,i0,i1,i2
    Logical lPerform,ldum
    Character(32) CommObj
    if(CommObj(1:3).eq.'bou') then
      call ExtractIV(Mov_Command,3,i1,iErr0,nBnd)
      if(iErr0.ne.0) return
      call ExtractIV(Mov_Command,4,i2,iErr0,nBnd)
      if(iErr0.ne.0) return
    else if(CommObj(1:3).eq.'dom') then
      call ExtractIV(Mov_Command,3,i1,iErr0,nDom)
      if(iErr0.ne.0) return
      call ExtractIV(Mov_Command,4,i2,iErr0,nDom)
      if(iErr0.ne.0) return
    else if(CommObj(1:3).eq.'exp') then
      call ExtractIV(Mov_Command,3,i1,iErr0,nExp)
      if(iErr0.ne.0) return
      call ExtractIV(Mov_Command,4,i2,iErr0,nExp)
      if(iErr0.ne.0) return
    else
      call ExtractIV(Mov_Command,3,i1,iErr0)
      if(iErr0.ne.0) return
      call ExtractIV(Mov_Command,4,i2,iErr0)
      if(iErr0.ne.0) return
    end if
    if(i1.eq.i2) return
    if(i1.gt.i2) then
      i0=i1
      i1=i2
      i2=i0
    end if
    Select Case(CommObj(1:3))
    Case('bou')
      if((i1.lt.1).or.(i1.gt.nBnd)) return
      if((i2.lt.1).or.(i2.gt.nBnd)) return
      if(lPerform) then
        call copyBnd(i2,i1-1,0.0d0,0.0d0,-2_4,ldum)
        call InsertBnd(i2+1,-1,ldum)
        call copyBnd(i1+1,i2,0.0d0,0.0d0,-2_4,ldum)
        call InsertBnd(i1+1,-1,ldum)
      end if
    Case('col')
      if((i1.lt.0).or.(i1.gt.255)) return
      if((i2.lt.0).or.(i2.gt.255)) return
      if(lPerform) then
        do i=1,nBnd
          if(tBnd(i)%iCol.eq.Int2(i1)) then
            tBnd(i)%iCol=Int2(i2)
          else if(tBnd(i)%iCol.eq.Int2(i2)) then
            tBnd(i)%iCol=Int2(i1)
          end if
        end do
        do i=1,nExp
          if(tExp(i)%iCol.eq.Int2(i1)) then
            tExp(i)%iCol=Int2(i2)
          else if(tExp(i)%iCol.eq.Int2(i2)) then
            tExp(i)%iCol=Int2(i1)
          end if
        end do
      end if
    Case('con')
      if((i1.lt.0).or.(i2.lt.0)) return
      if(lPerform) then
        do i=1,nBnd
          if(tBnd(i)%iConn.eq.Int2(i1)) then
            tBnd(i)%iConn=Int2(i2)
          else if(tBnd(i)%iConn.eq.Int2(i2)) then
            tBnd(i)%iConn=Int2(i1)
          end if
        end do
        do i=1,nExp
          if(tExp(i)%iConn.eq.Int2(i1)) then
            tExp(i)%iConn=Int2(i2)
          else if(tExp(i)%iConn.eq.Int2(i2)) then
            tExp(i)%iConn=Int2(i1)
          end if
        end do
      end if
    Case('dom')
      if((i1.lt.1).or.(i1.gt.nDom)) return
      if((i2.lt.1).or.(i2.gt.nDom)) return
      if(lPerform) then
        iDom(0)=iDom(i1)
        iDom(i1)=iDom(i2)
        iDom(i2)=iDom(0)
        iDom(0)=0_2
        eDom(0)=eDom(i1)
        eDom(i1)=eDom(i2)
        eDom(i2)=eDom(0)
        uDom(0)=uDom(i1)
        uDom(i1)=uDom(i2)
        uDom(i2)=uDom(0)
        sDom(0)=sDom(i1)
        sDom(i1)=sDom(i2)
        sDom(i2)=sDom(0)
        tDom(0)=tDom(i1)
        tDom(i1)=tDom(i2)
        tDom(i2)=tDom(0)
        Dom_Form(1:4,0)=Dom_Form(1:4,i1)
        Dom_Form(1:4,i1)=Dom_Form(1:4,i2)
        Dom_Form(1:4,i2)=Dom_Form(1:4,0)
        do i=1,nBnd
          if(tBnd(i)%iLDom.eq.Int2(i1)) then
            tBnd(i)%iLDom=Int2(i2)
          else if(tBnd(i)%iLDom.eq.Int2(i2)) then
            tBnd(i)%iLDom=Int2(i1)
          end if
          if(tBnd(i)%iRDom.eq.Int2(i1)) then
            tBnd(i)%iRDom=Int2(i2)
          else if(tBnd(i)%iRDom.eq.Int2(i2)) then
            tBnd(i)%iRDom=Int2(i1)
          end if
        end do
        do i=1,nExp
          if(tExp(i)%iDom.eq.Int2(i1)) then
            tExp(i)%iDom=Int2(i2)
          else if(tExp(i)%iDom.eq.Int2(i2)) then
            tExp(i)%iDom=Int2(i1)
          end if
        end do
      end if
    Case('exp')
      if((i1.lt.1).or.(i1.gt.nExp)) return
      if((i2.lt.1).or.(i2.gt.nExp)) return
      if(lPerform) then
        call copyExp(i2,i1-1,0.0d0,0.0d0,-2_4,ldum)
        call InsertExp(i2+1,-1,ldum)
        call copyExp(i1+1,i2,0.0d0,0.0d0,-2_4,ldum)
        call InsertExp(i1+1,-1,ldum)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovExc

  Subroutine MovGen(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr2,k,k1,k2,nE,jE,iCl,iCn,iOb,numpoints,iDm,kOb,iWf,iWe,nx,ny,nz,iHEk
    Real(8) r,rfirst(2),rlast(2),frinn,frout,Dl,Dr,Ainn,Aout,fmin,fmax,fact,ax,ay,az,a,x,y,z,x0,y0,z0, &
    & dlx,drx,dly,dry,dlz,drz
    Logical lPerform,ldum
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('2da') ! rotate prototype expansions around the point (x,y) in the xy plane for creating nE new expansions along an Arc
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,k1,iErr2,235)      ! prototype expansion color
      if(iErr2.ne.0) k1=1
      call ExtractIV(Mov_Command,4,jE,iErr2,nExp)     ! insert after expansion jE
      if(iErr2.ne.0) jE=0
      call ExtractIV(Mov_Command,5,iDm,iErr2,nDom)    ! domain number
      if(iErr2.ne.0) iDm=1
      call ExtractIV(Mov_Command,6,iCl,iErr2,235)     ! color number
      if(iErr2.ne.0) iCl=1
      call ExtractIV(Mov_Command,7,iCn,iErr2)         ! connection number
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,8,iOb,iErr2)         ! object number
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,9,nE,iErr2)          ! number of angles
      if(iErr2.ne.0) nE=1
      call ExtractRV(Mov_Command,10,fmin,iErr2)       ! min. angle
      if(iErr2.ne.0) fmin=0.0d0
      call ExtractRV(Mov_Command,11,fmax,iErr2)       ! max. angle
      if(iErr2.ne.0) fmax=360.0d0
      call ExtractRV(Mov_Command,12,x,iErr2)          ! x
      if(iErr2.ne.0) x=0.0d0
      call ExtractRV(Mov_Command,13,y,iErr2)          ! y
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        do k=1,nE
          do kExp=1,nExp ! all expansions
            if(tExp(kExp)%iCol.ne.Int2(k1)) Cycle
            call CopyExp(k1,jE,0.0d0,0.0d0,iCl,ldum)
            if(jE.lt.k1) k1=k1+1
            jE=jE+1
            tExp(jE)%iDom=Int2(iDm)
            tExp(jE)%iConn=Int2(iCn)
            tExp(jE)%iObj=Int2(iOb)
            call rotateExpansion(jE,x,y,fmin+(fmax-fmin)*Dble(k-1)/Dble(max(nE-1,1)),.true.)
          end do
        end do
        do kExp=1,nExp ! test all expansions
          k=tExp(kExp)%nPar
          call RepExpTest(k)
        end do
        kExp=1
      end if
    Case('2dl') ! move prototype expansions (color k1) along a line in the xy plane for creating nE new expansions along a Line
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,k1,iErr2,235)      ! prototype expansion color
      if(iErr2.ne.0) k1=1
      call ExtractIV(Mov_Command,4,jE,iErr2,nExp)     ! insert after expansion jE
      if(iErr2.ne.0) jE=0
      call ExtractIV(Mov_Command,5,iDm,iErr2,nDom)    ! domain number
      if(iErr2.ne.0) iDm=1
      call ExtractIV(Mov_Command,6,iCl,iErr2,235)     ! color number
      if(iErr2.ne.0) iCl=1
      call ExtractIV(Mov_Command,7,iCn,iErr2)         ! connection number
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,8,iOb,iErr2)         ! object number
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,9,nE,iErr2)          ! number of expansions per prototype expansion
      if(iErr2.ne.0) nE=1
      call ExtractRV(Mov_Command,10,x0,iErr2)         ! x0
      if(iErr2.ne.0) x0=0.0d0
      call ExtractRV(Mov_Command,11,y0,iErr2)         ! y0
      if(iErr2.ne.0) y0=0.0d0
      call ExtractRV(Mov_Command,12,x,iErr2)          ! x
      if(iErr2.ne.0) x=1.0d0
      call ExtractRV(Mov_Command,13,y,iErr2)          ! y
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        dlx=(x-x0)/Dble(max(nE-1,1))
        dly=(y-y0)/Dble(max(nE-1,1))
        do k=1,nE
          do k2=1,nExp ! all expansions
            if(tExp(k2)%iCol.ne.Int2(k1)) Cycle
            call CopyExp(k1,jE,0.0d0,0.0d0,iCl,ldum)
            if(jE.lt.k1) k1=k1+1
            jE=jE+1
            tExp(jE)%iDom=Int2(iDm)
            tExp(jE)%iConn=Int2(iCn)
            tExp(jE)%iObj=Int2(iOb)
            tExp(jE)%xO=0.0d0
            tExp(jE)%yO=0.0d0
            tExp(jE)%Plane(1:2,0)=0.0d0
            call moveExpansion(jE,x0+dlx*Dble(k-1),y0+dly*Dble(k-1))
          end do
        end do
        do kExp=1,nExp ! test all expansions
          k=tExp(kExp)%nPar
          call RepExpTest(k)
        end do
        kExp=1
      end if
    Case('2dp') ! generate a set of 2D planewaves propagating in different directions
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,k,iErr2,nDom)      ! insert after expansion k
      if(iErr2.ne.0) k=0
      call ExtractIV(Mov_Command,4,iDm,iErr2,nDom)    ! domain number
      if(iErr2.ne.0) iDm=1
      call ExtractIV(Mov_Command,5,iCl,iErr2)         ! color number
      if(iErr2.ne.0) iCl=1
      call ExtractIV(Mov_Command,6,iCn,iErr2)         ! connection number
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,7,iOb,iErr2)         ! object number
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,8,nE,iErr2)          ! number of angles
      if(iErr2.ne.0) nE=1
      call ExtractRV(Mov_Command,9,fmin,iErr2)        ! min. angle
      if(iErr2.ne.0) fmin=0.0d0
      call ExtractRV(Mov_Command,10,fmax,iErr2)       ! max. angle
      if(iErr2.ne.0) fmax=360.0d0
      call ExtractIV(Mov_Command,11,k1,iErr2)         ! iHE
      if(iErr2.ne.0) k1=2
      jE=k
      if(lPerform) then
        call InsertExp(k,nE,ldum)
        if(.not.ldum) return
        do k=1,nE
          tExp(jE+k)%iTypE=3_2
          tExp(jE+k)%rE(1:5)=0.0
          tExp(jE+k)%iE(1:6)=0
          k1=Min(2,Max(0,k1))
          tExp(jE+k)%iHE=Int2(k1)
          tExp(jE+k)%iDom=Int2(iDm)
          tExp(jE+k)%iCol=Int2(iCl)
          tExp(jE+k)%iConn=Int2(iCn)
          tExp(jE+k)%iObj=Int2(iOb)
          nx=1
          iHEk=igetiHE2(iHEGlobal,jE+k)
          if(iHEk.eq.2) nx=2
          tExp(jE+k)%rE(1)=fmin+(fmax-fmin)*Dble(k-1)/Dble(max(nE-1,1))
          tExp(jE+k)%xO=0.0d0
          tExp(jE+k)%yO=0.0d0
          tExp(jE+k)%Plane(1:3,0:3)=0.0d0
          if(lgcFld) then
            tExp(jE+k)%Plane(1,1)=dcos(tExp(jE+k)%rE(1)*Pi/180.0d0)
            tExp(jE+k)%Plane(2,1)=dsin(tExp(jE+k)%rE(1)*Pi/180.0d0)
            tExp(jE+k)%Plane(1,2)=-tExp(jE+k)%Plane(2,1)
            tExp(jE+k)%Plane(2,2)=tExp(jE+k)%Plane(1,1)
            tExp(jE+k)%Plane(3,3)=1.0d0
          else
            tExp(jE+k)%Plane(1,3)=dcos(tExp(jE+k)%rE(1)*Pi/180.0d0)
            tExp(jE+k)%Plane(2,3)=dsin(tExp(jE+k)%rE(1)*Pi/180.0d0)
            tExp(jE+k)%Plane(1,1)=-tExp(jE+k)%Plane(2,3)
            tExp(jE+k)%Plane(2,1)=tExp(jE+k)%Plane(1,3)
            tExp(jE+k)%Plane(3,2)=1.0d0
            tExp(jE+k)%rE(1)=0.0d0
          end if
          call InsertPar(jE+k,0,nx,ldum)
          if(.not.ldum) return
        end do
        do k=1,nE ! set all parameters of new expansions = 1
          do k2=1,tExp(jE+k)%nPar
            ParExp(1:nRHS,tExp(jE+k)%iOff+k2)=(1.0d0,0.0d0)
            iParExp(1:nRHS,tExp(jE+k)%iOff+k2)=0_2
          end do
        end do
      end if
    Case('3da') ! rotate prototype expansions around the y axis for creating nE new expansions along an Arc
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,k1,iErr2,235)      ! prototype expansion color
      if(iErr2.ne.0) k1=1
      call ExtractIV(Mov_Command,4,jE,iErr2,nExp)     ! insert after expansion jE
      if(iErr2.ne.0) jE=0
      call ExtractIV(Mov_Command,5,iDm,iErr2,nDom)    ! domain number
      if(iErr2.ne.0) iDm=1
      call ExtractIV(Mov_Command,6,iCl,iErr2,235)     ! color number
      if(iErr2.ne.0) iCl=1
      call ExtractIV(Mov_Command,7,iCn,iErr2)         ! connection number
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,8,iOb,iErr2)         ! object number
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,9,nE,iErr2)          ! number of angles
      if(iErr2.ne.0) nE=1
      call ExtractRV(Mov_Command,10,fmin,iErr2)       ! min. angle
      if(iErr2.ne.0) fmin=0.0d0
      call ExtractRV(Mov_Command,11,fmax,iErr2)       ! max. angle
      if(iErr2.ne.0) fmax=360.0d0
      if(lPerform) then
        do k=1,nE
          do kExp=1,nExp ! all expansions
            if(tExp(kExp)%iCol.ne.Int2(k1)) Cycle
            call CopyExp(k1,jE,0.0d0,0.0d0,iCl,ldum)
            if(jE.lt.k1) k1=k1+1
            jE=jE+1
            tExp(jE)%iDom=Int2(iDm)
            tExp(jE)%iConn=Int2(iCn)
            tExp(jE)%iObj=Int2(iOb)
            call rotate3DE(jE,0.0d0,fmin+(fmax-fmin)*Dble(k-1)/Dble(max(nE-1,1)),0.0d0,.true.)
          end do
        end do
        do kExp=1,nExp ! test all expansions
          k=tExp(kExp)%nPar
          call RepExpTest(k)
        end do
        kExp=1
      end if
    Case('3de') ! generate 3D expansions
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,kOb,iErr2,nObj)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,fact,iErr2)
      if(iErr2.ne.0) fact=1.0d0
      call ExtractIV(Mov_Command,5,jE,iErr2,nExp)
      if(iErr2.ne.0) jE=-1                             ! insert after last-1 (before excitation)
      call ExtractIV(Mov_Command,6,k1,iErr2,nExp)
      if(iErr2.ne.0) k1=0
      call ExtractIV(Mov_Command,7,k2,iErr2,nExp)
      if(iErr2.ne.0) k2=-1
      call ExtractIV(Mov_Command,8,k,iErr2,nExp)
      if(iErr2.ne.0) k=-32000
      call ExtractIV(Mov_Command,9,iCl,iErr2)          ! color number filter
      if(iErr2.ne.0) iCl=0
      call ExtractIV(Mov_Command,10,iCn,iErr2)         ! connection number filter
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,11,iOb,iErr2)         ! object number filter
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,12,nx,iErr2)          ! new color
      if(iErr2.ne.0) nx=0
      call ExtractIV(Mov_Command,13,ny,iErr2)          ! suppress previous delete
      if(iErr2.ne.0) ny=0
      call ExtractIV(Mov_Command,14,nz,iErr2)          ! - max. number of multipoles along ring
      if(iErr2.ne.0) nz=0
      if(lPerform) then
        fGenExpObj=fact
        iGenExpObj=kOb
        iGenExpIns=jE
        iGenExpMinE=k1
        iGenExpMaxE=k2
        iGenExpMaxM=k
        iGenExpCl=iCl
        iGenExpCl2=nx
        iGenExpCn=iCn
        iGenExpOb=iOb
        lGenExpDel=.true.
        if(ny.lt.1) lGenExpDel=.false.
        call GenObjExp(iGenExpObj,iGenExpIns,iGenExpMinE,iGenExpMaxE,iGenExpMaxM,iGenExpCl,iGenExpCl2, &
        & iGenExpCn,iGenExpOb,lGenExpDel,nz)
      end if
    Case('3dl') ! move prototype expansions along the line for creating nE new expansions along a Line
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,k1,iErr2,235)      ! prototype expansion color
      if(iErr2.ne.0) k1=1
      call ExtractIV(Mov_Command,4,jE,iErr2,nExp)     ! insert after expansion jE
      if(iErr2.ne.0) jE=0
      call ExtractIV(Mov_Command,5,iDm,iErr2,nDom)    ! domain number
      if(iErr2.ne.0) iDm=1
      call ExtractIV(Mov_Command,6,iCl,iErr2,235)     ! color number
      if(iErr2.ne.0) iCl=1
      call ExtractIV(Mov_Command,7,iCn,iErr2)         ! connection number
      if(iErr2.ne.0) iCn=0
      call ExtractIV(Mov_Command,8,iOb,iErr2)         ! object number
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,9,nE,iErr2)          ! number of expansions along the line
      if(iErr2.ne.0) nE=1
      call ExtractRV(Mov_Command,10,x0,iErr2)         ! start point x0
      if(iErr2.ne.0) x0=0.0d0
      call ExtractRV(Mov_Command,11,y0,iErr2)         ! start point y0
      if(iErr2.ne.0) y0=0.0d0
      call ExtractRV(Mov_Command,12,z0,iErr2)         ! start point z0
      if(iErr2.ne.0) z0=0.0d0
      call ExtractRV(Mov_Command,13,x,iErr2)          ! end point x
      if(iErr2.ne.0) x=0.0d0
      call ExtractRV(Mov_Command,14,y,iErr2)          ! end point y
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,15,z,iErr2)          ! end point z
      if(iErr2.ne.0) z=0.0d0
      if(lPerform) then
        do k=1,nE
          do kExp=1,nExp ! all expansions
            if(tExp(kExp)%iCol.ne.Int2(k1)) Cycle
            call CopyExp(k1,jE,0.0d0,0.0d0,iCl,ldum)
            if(jE.lt.k1) k1=k1+1
            jE=jE+1
            tExp(jE)%iDom=Int2(iDm)
            tExp(jE)%iConn=Int2(iCn)
            tExp(jE)%iObj=Int2(iOb)
            call move3DE(jE,x0+(x-x0)*Dble(k-1)/Dble(max(nE-1,1)),y0+(y-y0)*Dble(k-1)/Dble(max(nE-1,1)), &
            &               z0+(z-z0)*Dble(k-1)/Dble(max(nE-1,1)))
          end do
        end do
        do kExp=1,nExp ! test all expansions
          k=tExp(kExp)%nPar
          call RepExpTest(k)
        end do
        kExp=1
      end if
    Case('axi') ! generate 3D objects and expansions for axisymmetric
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,kOb,iErr2,nObj) ! use boundaries defined by kOb
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,jE,iErr2,nExp)  ! insert expansions after jE
      if(iErr2.ne.0) jE=-1                         ! insert after last-1 (before excitation)
      call ExtractIV(Mov_Command,5,k1,iErr2,nExp)  ! if k1>0: set min./max. expansion numbers of the objects equal to k1,k2
      if(iErr2.ne.0) k1=1
      call ExtractIV(Mov_Command,6,k2,iErr2,nExp)  ! if k2<=0: max. expansion number = nExp-k2
      if(iErr2.ne.0) k2=-1
      call ExtractIV(Mov_Command,7,nx,iErr2)       ! new color
      if(iErr2.ne.0) nx=3
      call ExtractIV(Mov_Command,8,ny,iErr2)       ! suppress previous delete
      if(iErr2.ne.0) ny=0
      call ExtractIV(Mov_Command,9,nz,iErr2)       ! multiplication factor for number of multipoles along ring
      if(iErr2.ne.0) nz=1
      call ExtractIV(Mov_Command,10,k,iErr2)       ! maximum degree of expansions
      if(iErr2.ne.0) k=1
      call ExtractRV(Mov_Command,11,fact,iErr2)    ! resolution of object
      if(iErr2.ne.0) fact=1.0d0
      call ExtractRV(Mov_Command,12,x,iErr2)       ! overdetermination factor in phy direction
      if(iErr2.ne.0) x=1.0d0
      if(lPerform) then
        iGenExpObj=kOb
        iGenExpIns=jE
        iGenExpMinE=k1
        iGenExpMaxE=k2
        iGenExpCl2=nx
        lGenExpDel=.true.
        if(ny.lt.1) lGenExpDel=.false.
        call GenObjAxi(iGenExpObj,iGenExpIns,iGenExpMinE,iGenExpMaxE,iGenExpCl2,lGenExpDel,nz,k,fact,x)
      end if
    Case('con') ! generate a connection type expansion
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,jE,iErr2,nExp)     ! insert after expansion jE
      if(iErr2.ne.0) jE=0
      call ExtractIV(Mov_Command,4,k1,iErr2,235)      ! connection flag
      if(iErr2.ne.0) k1=2
      call ExtractIV(Mov_Command,5,iDm,iErr2,nDom)    ! domain number
      if(iErr2.ne.0) iDm=1
      call ExtractIV(Mov_Command,6,iCl,iErr2,235)     ! color number
      if(iErr2.ne.0) iCl=1
      call ExtractIV(Mov_Command,7,iCn,iErr2)         ! connection number
      if(iErr2.ne.0) iCn=1
      call ExtractIV(Mov_Command,8,iOb,iErr2)         ! object number
      if(iErr2.ne.0) iOb=0
      call ExtractIV(Mov_Command,9,nE,iErr2)          ! max. order
      if(iErr2.ne.0) nE=0
      call ExtractIV(Mov_Command,10,iWf,iErr2)        ! basis type
      if(iErr2.ne.0) iWf=0
      call ExtractRV(Mov_Command,11,fact,iErr2)       ! Fourier factor
      if(iErr2.ne.0) fact=0.0d0
      if(lPerform) then
        call InsertExp(jE,1,ldum)
        if(.not.ldum) return
        jE=jE+1
        tExp(jE)%iType=0_2
        tExp(jE)%iDom=Int2(iDm)
        tExp(jE)%iCol=Int2(iCl)
        tExp(jE)%iConn=Int2(iCn)
        tExp(jE)%iObj=Int2(iOb)
        tExp(jE)%iE(1)=Int2(k1)
        tExp(jE)%iE(2:4)=0_2
        tExp(jE)%iE(5)=Int2(nE)
        tExp(jE)%iE(6)=Int2(iWf)
        tExp(jE)%iE(2:4)=0_2
        tExp(jE)%rE(1:4)=0.0d0
        tExp(jE)%rE(5)=fact
        call InsertPar(jE,0,max(1,2*nE+1),ldum)
        if(.not.ldum) return
        do kExp=1,nExp ! test all expansions
          k=tExp(kExp)%nPar
          call RepExpTest(k)
        end do
        kExp=1
      end if
    Case('exp','2de') ! generate 2D expansions
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,k1,iErr2,nBnd,k2)
      if(iErr2.ne.0) then
        k1=kB_genExp
        k2=kB_genExp
      end if
      call ExtractIV(Mov_Command,4,nE,iErr2)
      if(iErr2.ne.0) nE=nE_genExp
      call ExtractIV(Mov_Command,5,jE,iErr2,nExp)
      if(iErr2.ne.0) jE=iE_genExp
      call ExtractIV(Mov_Command,6,iDm,iErr2,nDom)
      if(iErr2.ne.0) iDm=iDm_genExp
      call ExtractIV(Mov_Command,7,iCl,iErr2)
      if(iErr2.ne.0) iCl=iCl_genExp
      call ExtractIV(Mov_Command,8,iCn,iErr2)
      if(iErr2.ne.0) iCn=iCn_genExp
      call ExtractRV(Mov_Command,9,fmin,iErr2)
      if(iErr2.ne.0) fmin=fmin_genExp
      call ExtractRV(Mov_Command,10,fmax,iErr2)
      if(iErr2.ne.0) fmax=fmax_genExp
      call ExtractRV(Mov_Command,11,fact,iErr2)
      if(iErr2.ne.0) fact=fact_genExp
      call ExtractIV(Mov_Command,12,numpoints,iErr2)
      if(iErr2.ne.0) numpoints=nS_genExp
      call ExtractRV(Mov_Command,13,frinn,iErr2)
      if(iErr2.ne.0) frinn=finn_genExp
      call ExtractRV(Mov_Command,14,frout,iErr2)
      if(iErr2.ne.0) frout=fout_genExp
      call ExtractRV(Mov_Command,15,rfirst(1),iErr2)
      if(iErr2.ne.0) rfirst(1)=df_genExp(1)
      call ExtractRV(Mov_Command,16,rfirst(2),iErr2)
      if(iErr2.ne.0) rfirst(2)=df_genExp(2)
      call ExtractRV(Mov_Command,17,rlast(1),iErr2)
      if(iErr2.ne.0) rlast(1)=dl_genExp(1)
      call ExtractRV(Mov_Command,18,rlast(2),iErr2)
      if(iErr2.ne.0) rlast(2)=dl_genExp(2)
      call ExtractRV(Mov_Command,19,Dl,iErr2)
      if(iErr2.ne.0) Dl=dinn_genExp
      call ExtractRV(Mov_Command,20,Dr,iErr2)
      if(iErr2.ne.0) Dr=dout_genExp
      call ExtractRV(Mov_Command,21,Ainn,iErr2)
      if(iErr2.ne.0) Ainn=ainn_genExp
      call ExtractRV(Mov_Command,22,Aout,iErr2)
      if(iErr2.ne.0) Aout=aout_genExp
      call ExtractIV(Mov_Command,23,iWf,iErr2)
      if(iErr2.ne.0) iWf=iWf_genExp
      call ExtractIV(Mov_Command,24,iWe,iErr2)
      if(iErr2.ne.0) iWe=iWe_genExp
      if(lPerform) then
        kB_genExp=k1
        nE_genExp=nE
        iE_genExp=jE
        iDm_genExp=iDm
        iCl_genExp=iCl
        iCn_genExp=iCn
        fmin_genExp=fmin
        fmax_genExp=fmax
        fact_genExp=fact
        nS_genExp=numpoints
        finn_genExp=frinn
        fout_genExp=frout
        df_genExp=rfirst
        dl_genExp=rlast
        dinn_genExp=Dl
        dout_genExp=Dr
        ainn_genExp=Ainn
        aout_genExp=Aout
        iWf_genExp=iWf
        iWe_genExp=iWe
        if(nE.gt.999998) then
          iGMSHbnd1=k1
          iGMSHbnd2=k2
          k1=1
          k2=1
        end if
        do k=k1,k2
          call GenExp(k,nE,jE,Int2(iCl),Int2(iCn),Int2(iDm),fmin,fmax,fact,rfirst,rlast,frinn,frout,Dl,Dr,Ainn,Aout, &
          & numpoints)
        end do
        call CorrExpPar(1000_4)
      end if
    Case('hex')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,iCl,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,iCn,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,k1,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,6,k2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,Dl,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,8,Dr,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,9,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        call generateColorHex(iCl,iCn,k1,k2,Dl,Dr,r)
      end if
    Case('pfd')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'sen') then
        call ExtractIV(Mov_Command,4,numpoints,iErr2)
        if((iErr2.ne.0).or.(numpoints.lt.8)) return
        call ExtractRV(Mov_Command,5,dlx,iErr2)
        if(iErr2.ne.0) dlx=Dble(max(0,nPFDil)+nPFDsLayers)+0.5d0
        call ExtractRV(Mov_Command,6,drx,iErr2)
        if(iErr2.ne.0) drx=Dble(max(0,nPFDih)+nPFDsLayers)+1.5d0
        call ExtractRV(Mov_Command,7,dly,iErr2)
        if(iErr2.ne.0) dly=Dble(max(0,nPFDjl)+nPFDsLayers)+0.5d0
        call ExtractRV(Mov_Command,8,dry,iErr2)
        if(iErr2.ne.0) dry=Dble(max(0,nPFDjh)+nPFDsLayers)+1.5d0
        call ExtractRV(Mov_Command,9,dlz,iErr2)
        if(iErr2.ne.0) dlz=Dble(max(0,nPFDkl)+nPFDsLayers)+0.5d0
        call ExtractRV(Mov_Command,10,drz,iErr2)
        if(iErr2.ne.0) drz=Dble(max(0,nPFDkh)+nPFDsLayers)+1.5d0
        if(lPerform) then
          ax=(PFDxmax-PFDxmin)-PFDdx*(drx+dlx)
          ay=(PFDymax-PFDymin)-PFDdy*(dry+dly)
          if(lgcFld) then
            a=2.0d0*(ax+ay)/Dble(numpoints)
            nx=max(2,Int4(ax/a))
            ny=max(2,Int4(ay/a))
            x0=PFDxmin+PFDdx*dlx
            y0=PFDymin+PFDdy*dly
            ax=ax/Dble(nx-1)
            ay=ay/Dble(ny-1)
            nPFDsens=2*(nx+ny-2)
            call AllocatePFDsens(ldum)
            if(ldum) then
              k=0
              y=y0
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                k=k+1
                PFDsensX(k)=x
                PFDsensY(k)=y
                PFDsensZ(k)=0.0d0
                PFDsensT(k)=0.0d0
                PFDsensD(k)=pBig
              end do
              x=PFDxmax-PFDdx*drx
              do k1=2,ny-1
                y=y0+Dble(k1-1)*ay
                k=k+1
                PFDsensX(k)=x
                PFDsensY(k)=y
                PFDsensZ(k)=0.0d0
                PFDsensT(k)=0.0d0
                PFDsensD(k)=pBig
              end do
              y=PFDymax-PFDdy*dry
              do k1=nx,1,-1
                x=x0+Dble(k1-1)*ax
                k=k+1
                PFDsensX(k)=x
                PFDsensY(k)=y
                PFDsensZ(k)=0.0d0
                PFDsensT(k)=0.0d0
                PFDsensD(k)=pBig
              end do
              x=x0
              do k1=ny-1,2,-1
                y=y0+Dble(k1-1)*ay
                k=k+1
                PFDsensX(k)=x
                PFDsensY(k)=y
                PFDsensZ(k)=0.0d0
                PFDsensT(k)=0.0d0
                PFDsensD(k)=pBig
              end do
            end if
          else
            az=(PFDzmax-PFDzmin)-PFDdz*(drz+dlz)
            a=dsqrt(2.0d0*(ax*ay+ax*az+ay*az)/Dble(numpoints))
            nx=max(2,Int4(ax/a))
            ny=max(2,Int4(ay/a))
            nz=max(2,Int4(az/a))
            x0=PFDxmin+PFDdx*dlx
            y0=PFDymin+PFDdy*dly
            z0=PFDzmin+PFDdz*dlz
            ax=ax/Dble(nx-1)
            ay=ay/Dble(ny-1)
            az=az/Dble(ny-1)
            nPFDsens=2*(nx*ny+nx*(nz-2)+(ny-2)*(nz-2))
            call AllocatePFDsens(ldum)
            if(ldum) then
              k=0
              z=z0
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=1,ny
                  y=y0+Dble(k2-1)*ay
                  k=k+1
                  PFDsensX(k)=x
                  PFDsensY(k)=y
                  PFDsensZ(k)=z
                  PFDsensT(k)=0.0d0
                  PFDsensD(k)=pBig
                end do
              end do
              z=PFDzmax-PFDdz*drz
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=1,ny
                  y=y0+Dble(k2-1)*ay
                  k=k+1
                  PFDsensX(k)=x
                  PFDsensY(k)=y
                  PFDsensZ(k)=z
                  PFDsensT(k)=0.0d0
                  PFDsensD(k)=pBig
                end do
              end do
              y=y0
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  PFDsensX(k)=x
                  PFDsensY(k)=y
                  PFDsensZ(k)=z
                  PFDsensT(k)=0.0d0
                  PFDsensD(k)=pBig
                end do
              end do
              y=PFDymax-PFDdy*dry
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  PFDsensX(k)=x
                  PFDsensY(k)=y
                  PFDsensZ(k)=z
                  PFDsensT(k)=0.0d0
                  PFDsensD(k)=pBig
                end do
              end do
              x=x0
              do k1=2,ny-1
                y=y0+Dble(k1-1)*ay
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  PFDsensX(k)=x
                  PFDsensY(k)=y
                  PFDsensZ(k)=z
                  PFDsensT(k)=0.0d0
                  PFDsensD(k)=pBig
                end do
              end do
              x=PFDxmax-PFDdx*drx
              do k1=2,ny-1
                y=y0+Dble(k1-1)*ay
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  PFDsensX(k)=x
                  PFDsensY(k)=y
                  PFDsensZ(k)=z
                  PFDsensT(k)=0.0d0
                  PFDsensD(k)=pBig
                end do
              end do
            end if
          end if
        end if
      else if(Argument(1:3).eq.'sou') then
        call ExtractIV(Mov_Command,4,numpoints,iErr2)
        if((iErr2.ne.0).or.(numpoints.lt.8)) return
        call ExtractRV(Mov_Command,5,dlx,iErr2)
        if(iErr2.ne.0) dlx=Dble(max(0,nPFDil)+nPFDsLayers)+0.5d0
        call ExtractRV(Mov_Command,6,drx,iErr2)
        if(iErr2.ne.0) drx=Dble(max(0,nPFDih)+nPFDsLayers)+1.5d0
        call ExtractRV(Mov_Command,7,dly,iErr2)
        if(iErr2.ne.0) dly=Dble(max(0,nPFDjl)+nPFDsLayers)+0.5d0
        call ExtractRV(Mov_Command,8,dry,iErr2)
        if(iErr2.ne.0) dry=Dble(max(0,nPFDjh)+nPFDsLayers)+1.5d0
        call ExtractRV(Mov_Command,9,dlz,iErr2)
        if(iErr2.ne.0) dlz=Dble(max(0,nPFDkl)+nPFDsLayers)+0.5d0
        call ExtractRV(Mov_Command,10,drz,iErr2)
        if(iErr2.ne.0) drz=Dble(max(0,nPFDkh)+nPFDsLayers)+1.5d0
        if(lPerform) then
          ax=Dble(nPFDi-1)-(drx+dlx)
          ay=Dble(nPFDj-1)-(dry+dly)
          if(lgcFld) then
            a=2.0d0*(ax+ay)/Dble(numpoints)
            nx=max(2,Int4(ax/a))
            ny=max(2,Int4(ay/a))
            x0=1.0d0+dlx
            y0=1.0d0+dly
            ax=ax/Dble(nx-1)
            ay=ay/Dble(ny-1)
            nPFDsource=2*(nx+ny-2)
            call AllocatePFDsource(ldum)
            if(ldum) then
              k=0
              y=y0
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                k=k+1
                iPFDs(k)=nint(x,4)
                jPFDs(k)=nint(y,4)
                kPFDs(k)=0.0d0
                PFDsourceA(k)=(1.0d0,0.0d0)
              end do
              x=Dble(nPFDi)-drx
              do k1=2,ny-1
                y=y0+Dble(k1-1)*ay
                k=k+1
                iPFDs(k)=nint(x,4)
                jPFDs(k)=nint(y,4)
                kPFDs(k)=0.0d0
                PFDsourceA(k)=(1.0d0,0.0d0)
              end do
              y=Dble(nPFDj)-dry
              do k1=nx,1,-1
                x=x0+Dble(k1-1)*ax
                k=k+1
                iPFDs(k)=nint(x,4)
                jPFDs(k)=nint(y,4)
                kPFDs(k)=0.0d0
                PFDsourceA(k)=(1.0d0,0.0d0)
              end do
              x=x0
              do k1=ny-1,2,-1
                y=y0+Dble(k1-1)*ay
                k=k+1
                iPFDs(k)=nint(x,4)
                jPFDs(k)=nint(y,4)
                kPFDs(k)=0.0d0
                PFDsourceA(k)=(1.0d0,0.0d0)
              end do
            end if
          else
            az=Dble(nPFDk)-(drz+dlz)
            a=dsqrt(2.0d0*(ax*ay+ax*az+ay*az)/Dble(numpoints))
            nx=max(2,Int4(ax/a))
            ny=max(2,Int4(ay/a))
            nz=max(2,Int4(az/a))
            x0=1.0d0+dlx
            y0=1.0d0+dly
            z0=1.0d0+dlz
            ax=ax/Dble(nx-1)
            ay=ay/Dble(ny-1)
            az=az/Dble(ny-1)
            nPFDsource=2*(nx*ny+nx*(nz-2)+(ny-2)*(nz-2))
            call AllocatePFDsource(ldum)
            if(ldum) then
              k=0
              z=z0
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=1,ny
                  y=y0+Dble(k2-1)*ay
                  k=k+1
                  iPFDs(k)=nint(x,4)
                  jPFDs(k)=nint(y,4)
                  kPFDs(k)=0.0d0
                  PFDsourceA(k)=(1.0d0,0.0d0)
                end do
              end do
              z=Dble(nPFDk)-drz
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=1,ny
                  y=y0+Dble(k2-1)*ay
                  k=k+1
                  iPFDs(k)=nint(x,4)
                  jPFDs(k)=nint(y,4)
                  kPFDs(k)=0.0d0
                  PFDsourceA(k)=(1.0d0,0.0d0)
                end do
              end do
              y=y0
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  iPFDs(k)=nint(x,4)
                  jPFDs(k)=nint(y,4)
                  kPFDs(k)=0.0d0
                  PFDsourceA(k)=(1.0d0,0.0d0)
                end do
              end do
              y=Dble(nPFDj)-dry
              do k1=1,nx
                x=x0+Dble(k1-1)*ax
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  iPFDs(k)=nint(x,4)
                  jPFDs(k)=nint(y,4)
                  kPFDs(k)=0.0d0
                  PFDsourceA(k)=(1.0d0,0.0d0)
                end do
              end do
              x=x0
              do k1=2,ny-1
                y=y0+Dble(k1-1)*ay
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  iPFDs(k)=nint(x,4)
                  jPFDs(k)=nint(y,4)
                  kPFDs(k)=0.0d0
                  PFDsourceA(k)=(1.0d0,0.0d0)
                end do
              end do
              x=Dble(nPFDi)-drx
              do k1=2,ny-1
                y=y0+Dble(k1-1)*ay
                do k2=2,nz-1
                  z=z0+Dble(k2-1)*az
                  k=k+1
                  iPFDs(k)=nint(x,4)
                  jPFDs(k)=nint(y,4)
                  kPFDs(k)=0.0d0
                  PFDsourceA(k)=(1.0d0,0.0d0)
                end do
              end do
            end if
          end if
        end if
      else
        return
      end if
    Case('rec')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,iCl,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,iCn,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,k1,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,6,k2,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,Dl,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,8,Dr,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        call generateColorRec(iCl,iCn,k1,k2,Dl,Dr)
      end if
    Case('set')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('bou')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) kB_genExp=k
      Case('dom')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iDm_genExp=k
      Case('col')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iCl_genExp=k
      Case('con')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iCn_genExp=k
      Case('met')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) nE_genExp=k
      Case('spl')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) nS_genExp=k
      Case('exp')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iE_genExp=k
      Case('wfa')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iWf_genExp=k
      Case('wfe')
        call ExtractIV(Mov_Command,4,k,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iWe_genExp=k
      Case('thr')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) fact_genExp=r
      Case('dmi')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) fmin_genExp=r
      Case('dma')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) fmax_genExp=r
      Case('dfl')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) df_genExp(1)=r
      Case('dfr')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) df_genExp(2)=r
      Case('dll')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) dl_genExp(1)=r
      Case('dlr')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) dl_genExp(2)=r
      Case('del')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) dinn_genExp=r
      Case('der')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) dout_genExp=r
      Case('fin')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) finn_genExp=r
      Case('fou')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) fout_genExp=r
      Case('ain')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) ainn_genExp=r
      Case('aou')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) aout_genExp=r
      end Select
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovGen

  Subroutine MovGet(CommObj,lPerform,iErr)
    Implicit none
    Real(8) r,x,y,z
    Integer(4) iErr,iErr0,iErr1,iErr2,iErr3,iErr4,iErr5,iErr6,ir,nx,ny,nz,nt,m,i,i1,i2,k,iOK
    Logical lPerform,ldum
    Character(32) CommObj
    Character(20) text
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    Select Case(CommObj(1:3))
    Case('cpu')
      if(lPerform) then
        call cpu_time(rMovCPU1) !time
        call date_and_time(charMoveTim(1),charMoveTim(2),charMoveTim(3),iMoveTim) !time
        rMovELA1=3600.0d0*Dble(iMoveTim(5))+60.0d0*Dble(iMoveTim(6))+Dble(iMoveTim(7))+0.001d0*Dble(iMoveTim(8)) !time
        rch(1)=rMovCPU1-rMovCPU0
        rch(2)=rMovELA1-rMovELA0
        write(*,*) 'CPU,elapsed time=',Real(rch(1)),Real(rch(2)),' seconds' !time
        rMovCPU0=rMovCPU1
        rMovELA0=rMovELA1
      end if
    Case('fie')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('der')
        if(lPerform) then
          if(iPlane.eq.3) then
            nxrFld=nxcFld
            nyrFld=nycFld
          else if(iPlane.eq.2) then
            nxrFld=nxcFld
            nyrFld=nzcFld
          else
            nxrFld=nycFld
            nyrFld=nzcFld
          end if
          call GetrField(.true.)
        end if
      Case('err')
        if(lPerform) then
          if(iPlane.eq.3) then
            nxrFld=nxcFld
            nyrFld=nycFld
          else if(iPlane.eq.2) then
            nxrFld=nxcFld
            nyrFld=nzcFld
          else
            nxrFld=nycFld
            nyrFld=nzcFld
          end if
          call ExtractStr(Mov_Command,4,FldFileName,ir)
          call GetrError(.true.)
          call GetrField(.true.)
        end if
      Case('gra')
        if(lPerform) then
          if(iPlane.eq.3) then
            nxrFld=nxcFld
            nyrFld=nycFld
          else if(iPlane.eq.2) then
            nxrFld=nxcFld
            nyrFld=nzcFld
          else
            nxrFld=nycFld
            nyrFld=nzcFld
          end if
          call GetGradrField()
        end if
      Case('lim')
        if(lPerform) then
          if(iPlane.eq.3) then
            nxrFld=nxcFld
            nyrFld=nycFld
          else if(iPlane.eq.2) then
            nxrFld=nxcFld
            nyrFld=nzcFld
          else
            nxrFld=nycFld
            nyrFld=nzcFld
          end if
          call GetrField(.true.)
        end if
      Case Default
        iErr=-9
        return
      end Select
    Case('fun')
      if(lArgument.lt.3) return
      call ExtractIV(Mov_Command,4,ir,iErr0)
      if((iErr0.ne.0).or.(ir.lt.1).or.(ir.gt.nFunA)) return
      call ExtractIV(Mov_Command,5,m,iErr0)
      if((iErr0.ne.0).or.(m.lt.2).or.(m.gt.nFun)) m=nFun
      call ExtractRV(Mov_Command,6,x,iErr0)
      if(iErr0.ne.0) x=-1.1d300
      call ExtractRV(Mov_Command,7,y,iErr0)
      if(iErr0.ne.0) y=1.1d300
      call ExtractIV(Mov_Command,8,i1,iErr0)
      if(iErr0.ne.0) i1=1
      call ExtractIV(Mov_Command,9,i2,iErr0)
      if(iErr0.ne.0) i2=nFun
      i1=Max(1,Min(nFun,i1))
      i2=Max(1,Min(nFun,i2))
      Select Case(Argument(1:3))
      Case('abo')
        if(lPerform) then
          if((x.lt.nBig).or.(y.gt.pBig)) call MinMax(Fun,mFun,mFunA,nFun,ir,x,y)
          ldum=.true.
          if(mFunA.le.nFunA) then
            call IncreaseFun(0,1,ldum)
            nFunA=mFunA
          else
            nFunA=nFunA+1
          end if
          if(ldum) then
            Fun(nFunA,1:mFun)=0.0d0
            r=(y-x)/Dble(m-1)
            z=x-r
            do i=1,m
              z=z+r
              do k=i1,i2
                if(Fun(ir,k-i1+1).gt.z) Fun(nFunA,i)=Fun(nFunA,i)+1.0d0
              end do
            end do
          end if
        end if
      Case('bel')
        if(lPerform) then
          if((x.lt.nBig).or.(y.gt.pBig)) call MinMax(Fun,mFun,mFunA,nFun,ir,x,y)
          ldum=.true.
          if(mFunA.le.nFunA) then
            call IncreaseFun(0,1,ldum)
            nFunA=mFunA
          else
            nFunA=nFunA+1
          end if
          if(ldum) then
            Fun(nFunA,1:mFun)=0.0d0
            r=(y-x)/Dble(m-1)
            z=x-r
            do i=1,m
              z=z+r
              do k=i1,i2
                if(Fun(ir,k-i1+1).lt.z) Fun(nFunA,i)=Fun(nFunA,i)+1.0d0
              end do
            end do
          end if
        end if
      Case('pro')
        if(lPerform) then
          if((x.lt.nBig).or.(y.gt.pBig)) call MinMax(Fun,mFun,mFunA,nFun,ir,x,y)
          ldum=.true.
          if(mFunA.le.nFunA) then
            call IncreaseFun(0,1,ldum)
            nFunA=mFunA
          else
            nFunA=nFunA+1
          end if
          if(ldum) then
            Fun(nFunA,1:mFun)=0.0d0
            r=(y-x)/Dble(m)
            z=x-r
            do i=1,m
              z=z+r
              do k=i1,i2
                if((Fun(ir,k-i1+1).gt.z).and.(Fun(ir,k-i1+1).le.(z+r))) Fun(nFunA,i)=Fun(nFunA,i)+1.0d0
              end do
            end do
          end if
        end if
      Case Default
        iErr=-9
        return
      end Select
    Case('gam')
      if(lArgument.lt.1) return
      open(1,file=Argument,status='old',iostat=i1)
      if(i1.ne.0) return
      close(1)
      call ExtractIV(Mov_Command,4,ir,iErr0,nExp)
      if(iErr0.ne.0) ir=0
      if(lPerform) then
        open(1,file=Argument,status='old',iostat=i1)
        if(i1.ne.0) return
        call ReadStr(1,text,iOK)
        if(CHProIdent(1:18).ne.text(1:18)) return
        call chread2(1,ich,1,rch,0,i1)
        if((i1.ne.0).or.(ich(1).eq.0)) return
        call chread2(1,ich,0,rch,2,i1)
        if(i1.ne.0) return
        call chread2(1,ich,2,rch,0,i1)
        if((i1.ne.0).or.(ich(1).eq.0)) return
        call chread2(1,ich,0,rch,2,i1)
        if(i1.ne.0) return
        close(1)
        if(ir.eq.0) then
          i1=1
          i2=nExp
        else if(ir.lt.0) then
          i1=1
          i2=min(nExp,-ir)
        else
          i1=min(nExp,ir)
          i2=i1
        end if
        do i=i1,i2
          if(tExp(i)%iTypE.eq.5) then
            tExp(i)%iE(1)=-abs(tExp(i)%iE(1))
            tExp(i)%rE(2)=rch(1)
          end if
        end do
      end if
    Case('int')
      if(Argument(1:3).eq.'bou') then
        call ExtractIV(Mov_Command,4,ir,iErr0,nBnd)
        call ExtractIV(Mov_Command,5,nx,iErr1)
        call ExtractIV(Mov_Command,6,ny,iErr2)
        call ExtractIV(Mov_Command,7,nz,iErr3)
        call ExtractIV(Mov_Command,8,nt,iErr4)
        call ExtractRV(Mov_Command,9,r, iErr5)
        call ExtractIV(Mov_Command,10,m,iErr6)
        if(lPerform) then
          if(iErr0.eq.0) iBndInt=ir
          if(iErr1.eq.0) IntWhat=nx
          if(iErr2.eq.0) IntInter=ny
          if(iErr3.eq.0) IntField=nz
          if(iErr4.eq.0) MaxIntIter=nt
          if(iErr5.eq.0) AccIntegral=r
          if(iErr6.eq.0) IntType=m
          lIntgSave=.false.
          call ExtractStr(Mov_Command,11,sch,i1)
          if(i1.gt.0) then
            lIntgSave=.true.
            i1=0
            if(sch(1:1).eq.'?') then
              sch(2:2)='w'
              i1=2
            end if
            call setNameS(sch,i1,ProFileName,InfFileName,'MAX','FUN',iOK,'INT')
          end if
          lAskQ=.false.
          call BndIntg(.true.)
          lAskQ=.true.
        end if
      else if(Argument(1:3).eq.'obj') then
        call ExtractIV(Mov_Command,4,ir,iErr0,nObj)
        call ExtractIV(Mov_Command,6,nx,iErr1)
        call ExtractIV(Mov_Command,7,ny,iErr2)
        if(iErr1.ne.0) nx=0
        if(iErr2.ne.0) ny=0
        if(lPerform) then
          if(iErr0.eq.0) iObjInt=ir
          lIntgSave=.false.
          call ExtractStr(Mov_Command,5,sch,i1)
          if(i1.gt.0) then
            lIntgSave=.true.
            i1=0
            if(sch(1:1).eq.'?') then
              sch(2:2)='w'
              i1=2
            end if
            call setNameS(sch,i1,ProFileName,InfFileName,'MAX','FUN',iOK,'INT')
          end if
          iIntgEc=Int2(nx)
          iIntgHc=Int2(ny)
          lAskQ=.false.
          call ObjIntg(.true.)
          lAskQ=.true.
        end if
      else if(Argument(1:3).eq.'rec') then
        call ExtractIV(Mov_Command,4,ir,iErr0)
        call ExtractIV(Mov_Command,5,nx,iErr1)
        call ExtractIV(Mov_Command,6,ny,iErr2)
        call ExtractRV(Mov_Command,7,r, iErr3)
        call ExtractRV(Mov_Command,8,x, iErr4)
        call ExtractRV(Mov_Command,9,y, iErr5)
        call ExtractRV(Mov_Command,10,z,iErr6)
        if(lPerform) then
          if(iErr0.eq.0) IntInter=ir
          if(iErr1.eq.0) IntNx=nx
          if(iErr2.eq.0) IntNy=ny
          if(iErr3.eq.0) XminInt=r
          if(iErr4.eq.0) XmaxInt=x
          if(iErr5.eq.0) YminInt=y
          if(iErr6.eq.0) YmaxInt=z
          lIntgSave=.false.
          call ExtractStr(Mov_Command,11,sch,i1)
          if(i1.gt.0) then
            lIntgSave=.true.
            i1=0
            if(sch(1:1).eq.'?') then
              sch(2:2)='w'
              i1=2
            end if
            call setNameS(sch,i1,ProFileName,InfFileName,'MAX','FUN',iOK,'INT')
          end if
          lAskQ=.false.
          call RecIntg(.true.)
          lAskQ=.true.
        end if
      end if
    Case('pfd')
      if(Argument(1:3).eq.'n2f') then
        call ExtractIV(Mov_Command,4,i1,iErr0)
        if((iErr0.ne.0).or.(i1.lt.1).or.(i1.gt.nExp)) return
        call ExtractIV(Mov_Command,5,i2,iErr0)
        if(iErr0.ne.0) i2=i1
        if((i2.lt.1).or.(i2.gt.nExp)) return
        call ExtractIV(Mov_Command,6,nx,iErr0)
        if(iErr0.ne.0) nx=1
        if((nx.lt.1).or.(nx.gt.nPFDf)) return
        if(lPerform) call n2fMultipole(i1,i2,nx) 
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovGet

  Subroutine MovLoo(lPerform,kComL,iErr)
    Implicit none
    Real(8) a,b,s
    Integer(4) iErr,ir2,iErr2,iv,kComL,ioff,ioffp
    Logical lPerform
    call ExtractIV(Mov_Command,2,ir2,iErr2) ! label number
    if((iErr2.ne.0).or.(ir2.lt.-999).or.(ir2.gt.999)) then
      iErr=-9
      return
    end if
    if(ir2.gt.0) then ! loop start has additional parameters
      call ExtractIV(Mov_Command,3,iv,iErr2) ! variable number
      if((iErr2.ne.0).or.(iv.lt.0).or.(iv.gt.999)) then
        iErr=-9
        return
      end if
      ioff=4
      call GetRch1(ioff,a,iErr2,ioffp) ! start value
      if(iErr2.gt.0) a=rMovVar(iv)
      ioff=ioff+ioffp+1
      call GetRch1(ioff,b,iErr2,ioffp) ! end value
      if(iErr2.ne.0) then
        iErr=-9
        return
      end if
      ioff=ioff+ioffp+1
      call GetRch1(ioff,s,iErr2,ioffp) ! increment
      if(iErr2.ne.0) s=1.0d0
    end if
    if(lPerform.and.(kComL.gt.0)) then
      if(ir2.gt.0) then ! loop start
        if(loopStart(ir2)) then
          rMovVar(iv)=A
        else
          rMovVar(iv)=rMovVar(iv)+S
        end if
        if(rMovVar(iv).gt.b) then
          loopStart(ir2)=.true.
          kComL=iMovLab(-ir2)+1 ! goto command after loop end
        end if
      else ! loop end: goto start
        loopStart(-ir2)=.false.
        kComL=iMovLab(-ir2)
      end if
    end if
  end Subroutine MovLoo

  Subroutine MovGot(lPerform,kComL,iErr)
    Implicit none
    Integer(4) iErr,ir2,iErr2,kComL
    Logical lPerform
    call ExtractIV(Mov_Command,2,ir2,iErr2)
    if((iErr2.ne.0).or.(ir2.lt.0).or.(ir2.gt.999)) then
      iErr=-9
      return
    end if
    if(lPerform.and.(kComL.gt.0)) then
      kComL=iMovLab(ir2)
    end if
  end Subroutine MovGot

  Subroutine MovIfE(lPerform,kComL,iErr)
  ! if v1=v2: jump to label
    Implicit none
    Real(8) r1,r2
    Integer(4) iErr,ir,iErr2,kComL,idum
    Logical lPerform
    Character(32) str
    iErr=-9
    call ExtractStr(Mov_Command,2,str,idum)
    if(str(1:1).eq.'v') then ! argument is a movie variable
      read(str(2:idum),*,iostat=iErr2) ir
      if(iErr2.ne.0) return
      r1=rMovVar(ir)
    else ! argument is a number
      call StrToRea(str(1:idum),r1,idum)
    end if
    call ExtractStr(Mov_Command,3,str,idum)
    if(str(1:1).eq.'v') then ! argument is a movie variable
      read(str(2:idum),*,iostat=iErr2) ir
      if(iErr2.ne.0) return
      r2=rMovVar(ir)
    else ! argument is a number
      call StrToRea(str(1:idum),r2,idum)
    end if
    call ExtractIV(Mov_Command,4,ir,iErr2)
    if((iErr2.ne.0).or.(ir.lt.0).or.(ir.gt.999)) then
      return
    end if
    iErr=0
    if(lPerform.and.(kComL.gt.0)) then
      if(dabs(r1-r2).lt.1.0d-10*max(dabs(r1),dabs(r2))) kComL=iMovLab(ir)
    end if
  end Subroutine MovifE

  Subroutine MovIfG(lPerform,kComL,iErr)
    Implicit none
    Real(8) r1,r2
    Integer(4) iErr,ir,iErr2,kComL,idum
    Logical lPerform
    Character(32) str
    iErr=-9
    call ExtractStr(Mov_Command,2,str,idum)
    if(str(1:1).eq.'v') then ! argument is a movie variable
      read(str(2:idum),*,iostat=iErr2) ir
      if(iErr2.ne.0) return
      r1=rMovVar(ir)
    else ! argument is a number
      call StrToRea(str(1:idum),r1,idum)
    end if
    call ExtractStr(Mov_Command,3,str,idum)
    if(str(1:1).eq.'v') then ! argument is a movie variable
      read(str(2:idum),*,iostat=iErr2) ir
      if(iErr2.ne.0) return
      r2=rMovVar(ir)
    else ! argument is a number
      call StrToRea(str(1:idum),r2,idum)
    end if
    call ExtractIV(Mov_Command,4,ir,iErr2)
    if((iErr2.ne.0).or.(ir.lt.0).or.(ir.gt.999)) then
      return
    end if
    iErr=0
    if(lPerform.and.(kComL.gt.0)) then
      if(r1.gt.r2) kComL=iMovLab(ir)
    end if
  end Subroutine MovIfG

  Subroutine MovIfS(lPerform,kComL,iErr)
    Implicit none
    Real(8) r1,r2
    Integer(4) iErr,ir,iErr2,kComL,idum
    Logical lPerform
    Character(32) str
    iErr=-9
    call ExtractStr(Mov_Command,2,str,idum)
    if(str(1:1).eq.'v') then ! argument is a movie variable
      read(str(2:idum),*,iostat=iErr2) ir
      if(iErr2.ne.0) return
      r1=rMovVar(ir)
    else ! argument is a number
      call StrToRea(str(1:idum),r1,idum)
    end if
    call ExtractStr(Mov_Command,3,str,idum)
    if(str(1:1).eq.'v') then ! argument is a movie variable
      read(str(2:idum),*,iostat=iErr2) ir
      if(iErr2.ne.0) return
      r2=rMovVar(ir)
    else ! argument is a number
      call StrToRea(str(1:idum),r2,idum)
    end if
    call ExtractIV(Mov_Command,4,ir,iErr2)
    if((iErr2.ne.0).or.(ir.lt.0).or.(ir.gt.999)) then
      return
    end if
    iErr=0
    if(lPerform.and.(kComL.gt.0)) then
      if(r1.lt.r2) kComL=iMovLab(ir)
    end if
  end Subroutine MovIfS

  Subroutine MovLab(iErr) ! this directive is handled before the execution of the ordinary directives
    Implicit none
    Integer(4) iErr,ir2,iErr2
    call ExtractIV(Mov_Command,2,ir2,iErr2)
    if((iErr2.ne.0).or.(ir2.lt.0).or.(ir2.gt.999)) then
      iErr=-9
      return
    end if
  end Subroutine MovLab

  Subroutine MovInc(CommObj,lPerform,iErr)
    Implicit none
    Real(8) r,x,y,z,u,v
    Integer(4) iErr,ir,ir2,iErr2,nx,ny,nz,nt,idum,n,m
    Logical lPerform,ldum
    Character(32) CommObj,cStr,str
    Character(3) chr3
    if(lArgument.lt.1) return
    Select Case(CommObj(1:3))
    Case('bou')
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'amp') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call IncBoundaryAmp(ir,x,ir2)
      else if(Argument(1:3).eq.'pts') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call IncBoundaryPts(ir,nx,ir2)
      else if(Argument(1:3).eq.'rad') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call IncCornerR(ir,nx,r,ir2)
      else if(Argument(1:3).eq.'spl') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call IncBoundarySpl(ir,nx,ir2)
      else if(Argument(1:3).eq.'val') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call IncBoundaryVal(ir,x,y,ir2)
      else if(Argument(1:3).eq.'wei') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call IncBoundaryWei(ir,x,y,ir2)
      else
        return
      end if
    Case('dom')
      call ExtractIV(Mov_Command,3,ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'eps') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          eDom(ir)=eDom(ir)+DCmplx(x,y)
          x=Dble(eDom(ir))
          y=DImag(eDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(1,ir)=cStr
        end if
      else if(Argument(1:3).eq.'mue') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          uDom(ir)=uDom(ir)+DCmplx(x,y)
          x=Dble(uDom(ir))
          y=DImag(uDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(2,ir)=cStr
        end if
      else if(Argument(1:3).eq.'sig') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          sDom(ir)=sDom(ir)+DCmplx(x,y)
          x=Dble(sDom(ir))
          y=DImag(sDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(3,ir)=cStr
        end if
      else if(Argument(1:3).eq.'tau') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          tDom(ir)=tDom(ir)+DCmplx(x,y)
          x=Dble(tDom(ir))
          y=DImag(tDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(4,ir)=cStr
        end if
      else
        return
      end if
    Case('eig')
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        imEigen=imEigen+ir
        if(lEigen.and.lPET.and.(kPET.gt.0)) kPET=-1
      end if
    Case('exc')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      kExc=max(1,min(kExc+ir,nRHS))
    Case('exp')
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'ang') then
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%rE(1)=r+tExp(ir:ir2)%rE(1)
          do kExp=ir,ir2
            call setOrient(kExp)
          end do
        end if
      else if(Argument(1:3).eq.'deg') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do kExp=ir,ir2
            if((tExp(kExp)%iTypE.lt.1_2).or.((tExp(kExp)%iTypE.gt.2_2).and.(tExp(kExp)%iTypE.lt.6_2))) Cycle
            nx=4
            tExp(kExp)%iE(nx)=tExp(kExp)%iE(nx)+Int2(nt)
          end do
        end if
      else if(Argument(1:3).eq.'gam') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%gc=tExp(ir:ir2)%gc+DCmplx(x,y)
        end if
      else if(Argument(1:2).eq.'ie') then
        call StrToIV(Argument(3:3),n,iErr2)
        if((iErr2.ne.0).or.(n.lt.1).or.(n.gt.6)) return
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(lPerform) then
          tExp(ir:ir2)%iE(n)=tExp(nt)%iE(n)+nt
        end if
      else if(Argument(1:2).eq.'re') then
        call StrToIV(Argument(3:3),n,iErr2)
        if((iErr2.ne.0).or.(n.lt.1).or.(n.gt.5)) return
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(lPerform) then
            tExp(ir:ir2)%rE(n)=tExp(ir:ir2)%rE(n)+r
        end if
      else if(Argument(1:3).eq.'ord') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do kExp=ir,ir2
            if((tExp(kExp)%iTypE.lt.1_2).or.(tExp(kExp)%iTypE.eq.3_2)) Cycle
            nx=2
            if(tExp(kExp)%iTypE.eq.5_2) nx=4
            tExp(kExp)%iE(nx)=tExp(kExp)%iE(nx)+Int2(nt)
          end do
        end if
      else if(Argument(1:3).eq.'par') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do kExp=ir,ir2
            call RepExpTest(nt+tExp(kExp)%nPar)
          end do
        end if
      end if
      if(lPerform) then
        call CorrExpPar(1000_4)
        kExp=ir2
      end if
    Case('fie')
      if(Argument(1:3).eq.'for') then
        call ExtractStr(Mov_Command,4,Argument,lArgument)
        if(lArgument.lt.3) return
        if(lPerform) then
          select case(Argument(1:3))
          case('col')
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            kColFFD=kColFFD+ir
          case('con')
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            kConFFD=kConFFD+ir
          case('dom')
            call ExtractIV(Mov_Command,5,ir,iErr2,nDom)
            if(iErr2.ne.0) return
            kDomFFD=kDomFFD+ir
          case('exp')
            call ExtractIV(Mov_Command,5,ir,iErr2,nExp)
            if(iErr2.ne.0) return
            kExpFFD=kExpFFD+ir
          case('par')
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            kParFFD=kParFFD+ir
          end select
        end if
      end if
    Case('fre')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        fcFld=fcFld+x
        call ExtractRV(Mov_Command,4,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        fcFld=fcFld+(0.0d0,1.0d0)*y
      end if
    Case('gam')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        if(lzPer) then
          dcFld=dcFld+x
          call ExtractRV(Mov_Command,4,y,iErr2)
          if(iErr2.ne.0) y=0.0d0
          dcFld=dcFld+(0.0d0,1.0d0)*y
          gcFld=0.5d0/(dcFld*fcFld*kw0)
        else
          gcFld=gcFld+x
          call ExtractRV(Mov_Command,4,y,iErr2)
          if(iErr2.ne.0) y=0.0d0
          gcFld=gcFld+(0.0d0,1.0d0)*y
          dcFld=0.5d0/(gcFld*fcFld*kw0)
        end if
      end if
    Case('gri')
      call StrToIV(Argument(1:lArgument),nx,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ny,iErr2)
      if(iErr2.ne.0) ny=0
      call ExtractIV(Mov_Command,5,nz,iErr2)
      if(iErr2.ne.0) nz=0
      if(lPerform) then
        nxcFld=Max(1,nxcFld+nx)
        nycFld=Max(1,nycFld+ny)
        nzcFld=Max(1,nzcFld+nz)
        call AllocateIFld(ldum)
        if(.not.ldum) return
        call AllocateGrd(ldum)
        if(.not.ldum) return
        call AllocateFld(ldum)
        if(.not.ldum) return
        if(.not.lgcFld) lGet3DMat=.true.
        call ClearGrid(.true.)
        call ClearDomain(.true.)
        call ClearField(.true.)
      end if
    Case('lev')
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) ir=1
      if(lPerform) then
        if(iPlane.eq.1) levPlane=Modulo(levPlane+ir-1,nxcFld)+1
        if(iPlane.eq.2) levPlane=Modulo(levPlane+ir-1,nycFld)+1
        if(iPlane.eq.3) levPlane=Modulo(levPlane+ir-1,nzcFld)+1
      end if
    Case('mmp')
      if(Argument(1:3).eq.'las') then
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) iMMPlast=iMMPlast+ir
      end if
    Case('per')
      if(Argument(1:1).eq.'c') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,r,iErr2)
        if(iErr2.ne.0) r=0.0d0
        call ExtractRV(Mov_Command,8,u,iErr2)
        if(iErr2.ne.0) u=0.0d0
        call ExtractRV(Mov_Command,9,v,iErr2)
        if(iErr2.ne.0) v=0.0d0
        if(lPerform) then
          CxPeriodDlg=CxPeriodDlg+DCmplx(x,y)
          CyPeriodDlg=CyPeriodDlg+DCmplx(z,r)
          CzPeriodDlg=CzPeriodDlg+DCmplx(u,v)
          call setupPeriod()
        end if
      elseif(Argument(1:1).eq.'d') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,r,iErr2)
        if(iErr2.ne.0) r=0.0d0
        call ExtractRV(Mov_Command,8,u,iErr2)
        if(iErr2.ne.0) u=0.0d0
        call ExtractRV(Mov_Command,9,v,iErr2)
        if(iErr2.ne.0) v=0.0d0
        if(lPerform) then
          xPeriod=xPeriod+x
          yPeriodVector(1)=yPeriodVector(1)+y
          yPeriodVector(2)=yPeriodVector(2)+z
          yPeriodVector(3)=0.0d0
          zPeriodVector(1)=zPeriodVector(1)+r
          zPeriodVector(2)=zPeriodVector(2)+u
          zPeriodVector(3)=zPeriodVector(3)+v
        end if
      elseif(Argument(1:1).eq.'p') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        if(lPerform) then
          if(dabs(xPeriod).gt.pSmall) then
            CxPeriodDlg=CxPeriodDlg+Pi*DCmplx(x.div.xPeriod,0.0d0)/180.0d0
          end if
          if(r3Vec_Length(yPeriodVector).gt.pSmall) then
            CyPeriodDlg=CyPeriodDlg+Pi*DCmplx(y.div.r3Vec_Length(yPeriodVector),0.0d0)/180.0d0
          end if
          if(r3Vec_Length(zPeriodVector).gt.pSmall) then
            CzPeriodDlg=CzPeriodDlg+Pi*DCmplx(z.div.r3Vec_Length(zPeriodVector),0.0d0)/180.0d0
          end if
          call setupPeriod()
        end if
      else
        iErr=-1
      end if
    Case('phi')
      call StrToRV(Argument(1:lArgument),r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        prFld=prFld+r
        if(Dabs(Dble(fcFld)).gt.pSmall) trFld=trFld+r/(360.0d0*Dble(fcFld))
      end if
    Case('pla')
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) iPlane=Modulo(iPlane+ir-1,3)+1
    Case('sym')
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        ixySymm=Modulo(ixySymm+nint(x,4),3)
        ixzSymm=Modulo(ixzSymm+nint(y,4),3)
        iyzSymm=Modulo(iyzSymm+nint(z,4),3)
      end if
    Case('tim')
      call StrToRV(Argument(1:lArgument),r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        trFld=trFld+r
        prFld=prFld+r*(360.0d0*Dble(fcFld))
      end if
    Case('var')
      call ExtractStr(Mov_Command,3,str,idum)
      if(str(1:1).eq.'v') then ! syntax: mul var vn ..., where n is the integer number of the movie variable
        read(str(2:idum),*,iostat=iErr2) ir
        if(iErr2.ne.0) return
        call ExtractStr(Mov_Command,4,str,idum)
        if((str(1:3).eq.'for')) then ! syntax: mul var vn formula ...
          call ExtractStr(Mov_Command,5,str,idum)
          if(lPerform) then
            call StrToRV(str,r,iErr2)
            rMovVar(ir)=rMovVar(ir)+r
          end if
        else ! syntax: mul var vn r
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.eq.0) then
            if(lPerform) rMovVar(ir)=rMovVar(ir)+r
          else
            if(idum.lt.2) return
            call ExtractStr(Mov_Command,4,str,idum)
            chr3(1:3)=str(1:3)
            call GetRch(chr3,4_4,m,iErr2)
            if((iErr2.ne.0).or.(m.ne.1)) return
            if(lPerform) rMovVar(ir)=rMovVar(ir)+rch(1)
          end if
        end if
      else
        call StrToRV(Argument(1:lArgument),r,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) ir=0
        ir=Max(0,Min(999,ir))
        if(lPerform) rMovVar(ir)=rMovVar(ir)+r
      end if
    Case('wav')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        x=1.0d0/(dsqrt(Mue0*Eps0)*Dble(fcFld))+x
        fcFld=DCmplx(1.0d0/(dsqrt(Mue0*Eps0)*x),0.0d0)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovInc

  Subroutine MovIte(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,ir,iErr2
    Logical lPerform
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('con')
      if(lrGrd) then
        iErr=-2
        return
      end if
      if(lPerform) call transformGrid(.true.)
    Case('fie')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lPerform) then
        nIterPFD=ir
        if(lArgument.gt.0) then
          if((Argument(1:1).eq.'c').or.(Argument(1:1).eq.'C')) lPFDc=.true.
        end if
        call transformField(.false.)
        lPFDc=.false.
      end if
    Case('gri')
      if(lrGrd) then
        iErr=-2
        return
      end if
      if(lPerform) call transformGrid(.false.)
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovIte

  Subroutine MovMmp(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,ir,ir1,ir2,it,idum,iErr2
    Real(8) x1,x2,y1,y2,s
    Logical lPerform,ldum
    Character(32) CommObj
    Character(256) LocFileName
    if(lArgument.gt.0) then
      ir=Int4(iMMPCon)
      it=kExc
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) then
        if(Argument(1:1).eq.'+') then
          ir=Int4(iMMPCon)+1_4
        else if(Argument(1:1).eq.'-') then
          ir=Int4(iMMPCon)-1_4
        end if
      end if
      call ExtractIV(Mov_Command,4,it,iErr2)
      if(iErr2.ne.0) then
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.gt.0) then
          if(sch(1:1).eq.'+') then
            it=kExc+1_4
          else if(sch(1:1).eq.'-') then
            it=kExc-1_4
          end if
        end if
      end if
      it=max(1,min(it,nRHS))
      if(lPerform) then
        iMMPCon=Int2(ir)
        kExc=it
      end if
    end if
    Select Case(CommObj(1:3))
    Case('cnd')
      if(lPerform) then
        if(Allocated(MMPMtr)) then
          condition=getCondition(MMPMtr,mCol,nRow)
        else
          condition=-1.0d0
        end if
		  end if
    Case('err')
      if(lPerform) then
        call getErrors(.true.)
		  end if
    Case('mat')
      if(lPerform) then
        if(.not.lgcFld) lGet3DMat=.true.
        call genMatPts(ldum)
        if(.not.lgcFld)  call get3DMatPts(1,nObj,0_4,-1_2,.true.) 
		  end if
    Case('sca') ! scale connection ir using some integrals
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,it,iErr2)
      if(iErr2.ne.0) return
      if(it.eq.2) then
        call ExtractRV(Mov_Command,5,x1,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,x2,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,y1,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,y2,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,9,s,iErr2)
        if(iErr2.ne.0) return
        ir1=1
        ir2=1
      else if(it.lt.2) then
        call ExtractIV(Mov_Command,5,ir1,iErr2,nBnd,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,s,iErr2)
        if(iErr2.ne.0) return
        x1=0.0d0
        x2=1.0d0
        y1=0.0d0
        y2=1.0d0
      else
        call ExtractIV(Mov_Command,5,ir1,iErr2,nObj,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,s,iErr2)
        if(iErr2.ne.0) return
        x1=0.0d0
        x2=1.0d0
        y1=0.0d0
        y2=1.0d0
      end if
      if(lPerform) then
        call ScaleConnection(ir,it,ir1,ir2,x1,x2,y1,y2,s)
		  end if
    Case('sol')
      if(lPerform) then
        if(.not.lgcFld) lGet3DMat=.true.
        if(lEigen) then
          call getEigen(.true.)
          lEigen=.false.
          ldum=lPET
          lPET=.false.
          resEigen=getRes(fcFld)
          lPET=ldum
          lEigen=.true.
        else
          resEigen=getRes(fcFld)
        end if
		  end if
    Case('wri')
      if(lPerform) then
        call ExtractStr(Mov_Command,3,Argument,lArgument)
        if(lArgument.lt.1) return
        call ExtractStr(Mov_Command,4,sch,idum)
        if(Argument(1:1).eq.'e') then
          LocFileName=FunFileName
          if(idum.gt.0) FunFileName=sch(1:idum)//char(0)
          if((lArgument.gt.1).and.(.not.lgcFld)) then
            if(Argument(2:2).eq.'2') then
              call SaveError(.true.,.true.)
            else
              call SaveError(.true.,.false.)
            end if
          else
            call SaveError(.true.)
          end if
          FunFileName=LocFileName
        else if(Argument(1:1).eq.'r') then
          LocFileName=FldFileName
          if(idum.gt.0) FldFileName=sch(1:idum)//char(0)
          call SaveRmatrix(.true.)
          FldFileName=LocFileName
        else if(Argument(1:1).eq.'t') then
          LocFileName=FldFileName
          if(idum.gt.0) FldFileName=sch(1:idum)//char(0)
          call SaveTmatrix(.true.)
          FldFileName=LocFileName
        end if
		  end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovMmp

  Subroutine MovMov(CommObj,lPerform,iErr)
    Implicit none
    Real(8) x,y,z
    Integer(4) iErr,ir,ir2,iErr2,nt,k1,k2,k3,i,j,k
    Logical lPerform
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('3de')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      if(lPerform) then
        do i=ir,ir2
          call move3DE(i,x,y,z)
        end do
      end if
    Case('all')
      if(lArgument.lt.1) return
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        call moveAll(x,y)
      end if
    Case('bou')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call moveBoundary(ir,x,y,ir2)
      end if
    Case('col')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) call moveColor(ir,x,y)
    Case('con')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) call moveConnection(ir,x,y)
    Case('cor')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nt,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) call moveCorner(ir,nt,x,y,ir2)
    Case('dom')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) call moveDomain(ir,x,y)
    Case('exc')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExc,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) then
        call ExtractStr(Mov_Command,4,Argument,lArgument)
        if(lArgument.lt.3) return
        if(Argument(1:3).eq.'per') then
          call ExtractIV(Mov_Command,5,k1,iErr2)
          if(iErr2.ne.0) return
          call ExtractIV(Mov_Command,6,k2,iErr2)
          if(iErr2.ne.0) k2=0
          call ExtractIV(Mov_Command,7,k3,iErr2)
          if(iErr2.ne.0) k3=0
          x=k1*xPeriod+k2*yPeriodVector(1)+k3*zPeriodVector(1)
          y=k2*yPeriodVector(2)+k3*zPeriodVector(2)
          z=k3*zPeriodVector(3)
          if(lPerform) then
            iDomExp=0_2
            iColExp=0_2
            iConExp=0_2
            call moveExpansion(nExp-nExc+ir,x,y,nExp-nExc+ir2,z,k1,k2,k3)
          end if
        end if
      else
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        if(lPerform) then
          iDomExp=0_2
          iColExp=0_2
          iConExp=0_2
          call moveExpansion(nExp-nExc+ir,x,y,nExp-nExc+ir2,z)
        end if
      end if
    Case('exp')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) then
        call ExtractStr(Mov_Command,4,Argument,lArgument)
        if(lArgument.lt.3) return
        if(Argument(1:3).eq.'per') then
          call ExtractIV(Mov_Command,5,k1,iErr2)
          if(iErr2.ne.0) return
          call ExtractIV(Mov_Command,6,k2,iErr2)
          if(iErr2.ne.0) k2=0
          call ExtractIV(Mov_Command,7,k3,iErr2)
          if(iErr2.ne.0) k3=0
          x=k1*xPeriod+k2*yPeriodVector(1)+k3*zPeriodVector(1)
          y=k2*yPeriodVector(2)+k3*zPeriodVector(2)
          z=k3*zPeriodVector(3)
          if(lPerform) then
            iDomExp=0_2
            iColExp=0_2
            iConExp=0_2
            call moveExpansion(ir,x,y,ir2,z,k1,k2,k3)
          end if
        end if
      else
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        if(lPerform) then
          iDomExp=0_2
          iColExp=0_2
          iConExp=0_2
          call moveExpansion(ir,x,y,ir2,z)
        end if
      end if
    Case('gri')
      if(lArgument.lt.1) return
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) x=0.0d0
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,5,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      if(lPerform) then
        if(lrGrd) then
          spacecFld(1,0)=spacecFld(1,0)+x
          spacecFld(2,0)=spacecFld(2,0)+y
          spacecFld(3,0)=spacecFld(3,0)+z
        else
          do k=1,nzcFld
            do j=1,nycFld
              do i=1,nxcFld
                rGrd(1,i,j,k)=rGrd(1,i,j,k)+x
                rGrd(2,i,j,k)=rGrd(2,i,j,k)+y
                rGrd(3,i,j,k)=rGrd(3,i,j,k)+z
              end do
            end do
          end do
        end if
      end if
    Case('obj')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nObj)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      if(lPerform) then
        if(ir.gt.0) then
          k1=min(ir,nObj)
          k2=k1
        else if(ir.lt.0) then
          k1=1
          k2=min(-ir,nObj)
        else
          k1=1
          k2=nObj
        end if
        do kObj=k1,k2
          tObj(kObj)%plane(1,0)=tObj(kObj)%plane(1,0)+x
          tObj(kObj)%plane(2,0)=tObj(kObj)%plane(2,0)+y
          tObj(kObj)%plane(3,0)=tObj(kObj)%plane(3,0)+z
          do i=1,nExp
            if(tExp(i)%iObj.ne.kObj) Cycle
            call move3DE(i,x,y,z)
          end do
        end do
      end if
    Case('pfd')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'sen') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsens,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,7,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        if(lPerform) then
          PFDsensX(ir:ir2)=PFDsensX(ir:ir2)+x
          PFDsensY(ir:ir2)=PFDsensY(ir:ir2)+y
          PFDsensZ(ir:ir2)=PFDsensZ(ir:ir2)+z
        end if
      else if(Argument(1:3).eq.'sou') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsource,ir2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,i,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,j,iErr2)
        if(iErr2.ne.0) j=0
        call ExtractIV(Mov_Command,7,k,iErr2)
        if(iErr2.ne.0) k=0
        if(lPerform) then
          iPFDs(ir:ir2)=iPFDs(ir:ir2)+i
          jPFDs(ir:ir2)=jPFDs(ir:ir2)+j
          kPFDs(ir:ir2)=kPFDs(ir:ir2)+k
        end if
      end if
    Case('p2d')
      call ExtractIV(Mov_Command,3,i,iErr2)
      if(iErr2.ne.0) i=0
      if(lPerform) then
        call PRTgetPosition2D(i)
      end if
    Case('p3d')
      call ExtractIV(Mov_Command,3,i,iErr2)
      if(iErr2.ne.0) i=0
      if(lPerform) then
        call PRTgetPosition3D(i)
      end if
    Case('win')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,Int4(nWin))
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) call moveWindows(ir,x,y)
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovMov

  Subroutine MovMul(CommObj,lPerform,iErr)
    Implicit none
    Complex(8) c
    Real(8) r,x,y
    Integer(4) iErr,ir,ir2,iErr2,nx,ny,nt,idum,n,m
    Logical lPerform
    Character(32) CommObj,cStr,str
    Character(3) chr3
    if(lArgument.lt.1) return
    Select Case(CommObj(1:3))
    Case('all')
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        call BlowAll(0.0d0,0.0d0,x)
        call BlowObject(0,x)
        fcFld=fcFld/x
        if(iEigen.ne.1) then
          c1Eigen=c1Eigen/x
          c2Eigen=c2Eigen/x
          c1EigenC=c1EigenC/x
          c2EigenC=c2EigenC/x
        end if
        cxPeriod=cxPeriod/x
        cyPeriod=cyPeriod/x
        czPeriod=czPeriod/x
        cxPeriodDlg=cxPeriodDlg/x
        cyPeriodDlg=cyPeriodDlg/x
        czPeriodDlg=czPeriodDlg/x
        xPeriod=x*xPeriod
        yPeriodVector(1:3)=x*yPeriodVector(1:3)
        zPeriodVector(1:3)=x*zPeriodVector(1:3)
        XminInt=x*XminInt
        XmaxInt=x*XmaxInt
        YminInt=x*YminInt
        YmaxInt=x*YmaxInt
        spacecFld(1:3,0:3)=x*spacecFld(1:3,0:3)
        do nx=1,nWin
          if(lWinFld(nx)) then
            WinXmin(nx)=WinXmin(nx)*x
            WinXmax(nx)=WinXmax(nx)*x
            WinYmin(nx)=WinYmin(nx)*x
            WinYmax(nx)=WinYmax(nx)*x
          end if
        end do
        BndDMax=BndDMax*x
        CHGLVectorMaxLength=CHGLVectorMaxLength*x
        CHGLExpLen=CHGLExpLen*x
        CHGLxAxisL=CHGLxAxisL*x
        CHGLyAxisL=CHGLyAxisL*x
        CHGLzAxisL=CHGLzAxisL*x
        CHGLlookAt(1:3)=CHGLlookAt(1:3)*x
        CHGLlookFrom(1:3)=CHGLlookFrom(1:3)*x
        CHGLViewNear=CHGLViewNear*x
        CHGLViewFar=CHGLViewFar*x
        do nx=1,nCHGLTube
          CHGLTubeStart(1:3,nx)=CHGLTubeStart(1:3,nx)*x
          CHGLTubeR(nx)=CHGLTubeR(nx)*x
          CHGLTubeD(nx)=CHGLTubeD(nx)*x
        end do
        PFDxmin=PFDxmin*x
        PFDxmax=PFDxmax*x
        PFDymin=PFDymin*x
        PFDymax=PFDymax*x
        PFDzmin=PFDzmin*x
        PFDzmax=PFDzmax*x
        PFDsensX(1:nPFDsens)=PFDsensX(1:nPFDsens)*x
        PFDsensY(1:nPFDsens)=PFDsensY(1:nPFDsens)*x
        PFDsensZ(1:nPFDsens)=PFDsensZ(1:nPFDsens)*x
        PFDsensT(1:nPFDsens)=PFDsensT(1:nPFDsens)*x
        PFDsensD(1:nPFDsens)=PFDsensD(1:nPFDsens)*x
        PFDfmin=PFDfmin/x
        PFDfmax=PFDfmax/x
        PFDfTmax=PFDfTmax*x
        PFDfTau=PFDfTau*x
        dtrFld=dtrFld*x
      end if
    Case('bou')
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'amp') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundaryAmp(ir,x,ir2)
      else if(Argument(1:3).eq.'pts') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundaryPts(ir,nx,ir2)
      else if(Argument(1:3).eq.'rad') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulCornerR(ir,nx,r,ir2)
      else if(Argument(1:3).eq.'spl') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundarySpl(ir,nx,ir2)
      else if(Argument(1:3).eq.'val') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundaryVal(ir,x,y,ir2)
      else if(Argument(1:3).eq.'wei') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call MulBoundaryWei(ir,x,y,ir2)
      else
        return
      end if
    Case('dom')
      call ExtractIV(Mov_Command,3,ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'epi') then
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          call getEUST(ir)
          x=Dble(eDom(ir))
          y=DImag(eDom(ir))*y
          eDom(ir)=DCmplx(x,y)
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(1,ir)=cStr
        end if
      else if(Argument(1:3).eq.'epr') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          call getEUST(ir)
          x=Dble(eDom(ir))*x
          y=DImag(eDom(ir))
          eDom(ir)=DCmplx(x,y)
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(1,ir)=cStr
        end if
      else if(Argument(1:3).eq.'eps') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          eDom(ir)=eDom(ir)*DCmplx(x,y)
          x=Dble(eDom(ir))
          y=DImag(eDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(1,ir)=cStr
        end if
      else if(Argument(1:3).eq.'mue') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          uDom(ir)=uDom(ir)*DCmplx(x,y)
          x=Dble(uDom(ir))
          y=DImag(uDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(2,ir)=cStr
        end if
      else if(Argument(1:3).eq.'mui') then
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          call getEUST(ir)
          x=Dble(uDom(ir))
          y=DImag(uDom(ir))*y
          uDom(ir)=DCmplx(x,y)
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(2,ir)=cStr
        end if
      else if(Argument(1:3).eq.'mur') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          call getEUST(ir)
          x=Dble(uDom(ir))*x
          y=DImag(uDom(ir))
          uDom(ir)=DCmplx(x,y)
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(2,ir)=cStr
        end if
      else if(Argument(1:3).eq.'sig') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          sDom(ir)=sDom(ir)*DCmplx(x,y)
          x=Dble(sDom(ir))
          y=DImag(sDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(3,ir)=cStr
        end if
      else if(Argument(1:3).eq.'tau') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          call getEUST(ir)
          tDom(ir)=tDom(ir)*DCmplx(x,y)
          x=Dble(tDom(ir))
          y=DImag(tDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(4,ir)=cStr
        end if
      else
        return
      end if
    Case('exp')
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'ang') then
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%rE(1)=r*tExp(ir:ir2)%rE(1)
          do kExp=ir,ir2
            call setOrient(kExp)
          end do
          kExp=ir2
        end if
      else if(Argument(1:3).eq.'gam') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%gc=tExp(ir:ir2)%gc*DCmplx(x,y)
        end if
      else if(Argument(1:2).eq.'ie') then
        call StrToIV(Argument(3:3),n,iErr2)
        if((iErr2.ne.0).or.(n.lt.1).or.(n.gt.6)) return
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(lPerform) then
          tExp(ir:ir2)%iE(n)=nint(Dble(tExp(ir:ir2)%iE(n))*r,2)
        end if
      else if(Argument(1:2).eq.'re') then
        call StrToIV(Argument(3:3),n,iErr2)
        if((iErr2.ne.0).or.(n.lt.1).or.(n.gt.5)) return
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(lPerform) then
            tExp(ir:ir2)%rE(n)=tExp(ir:ir2)%rE(n)*r
        end if
      else if(Argument(1:3).eq.'par') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          c=x*cdexp((0.0d0,1.0d0)*Pi*y/180.0d0)
          do ny=ir,ir2
            nx=ny
            if(nt.le.0) then
              if(nt.eq.0) nt=-tExp(nx)%nPar
              nx=max(-nExp,nx)
              ParExp(kExc,tExp(nx)%iOff+1:tExp(nx)%iOff-nt)=ParExp(kExc,tExp(nx)%iOff+1:tExp(nx)%iOff-nt)*c
            else
              nt=min(tExp(nx)%nPar,nt)
              ParExp(kExc,tExp(nx)%iOff+nt)=ParExp(kExc,tExp(nx)%iOff+nt)*c
            end if
          end do
        end if
      end if
      if(lPerform) then
        call CorrExpPar(1000_4)
        kExp=ir2
      end if
    Case('fie')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        cFld=dcmplx(x,y)*cFld
      end if
    Case('fre')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        fcFld=fcFld*x*cdexp((0.0d0,1.0d0)*Pi*y/180.0d0)
      end if
    Case('fun')
      call ExtractIV(Mov_Command,3,ir,iErr2,nFunA,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        Fun(ir:ir2,1:nFun)=x*Fun(ir:ir2,1:nFun)
      end if
    Case('gam')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      if(lPerform) then
        if(lzPer) then
          dcFld=dcFld+x*cdexp((0.0d0,1.0d0)*Pi*y/180.0d0)
          gcFld=0.5d0/(dcFld*fcFld*kw0)
        else
          gcFld=gcFld+x*cdexp((0.0d0,1.0d0)*Pi*y/180.0d0)
          dcFld=0.5d0/(gcFld*fcFld*kw0)
        end if
      end if
    Case('var')
      call ExtractStr(Mov_Command,3,str,idum)
      if(str(1:1).eq.'v') then ! syntax: mul var vn ..., where n is the integer number of the movie variable
        read(str(2:idum),*,iostat=iErr2) ir
        if(iErr2.ne.0) return
        call ExtractStr(Mov_Command,4,str,idum)
        if((str(1:3).eq.'for')) then ! syntax: mul var vn formula ...
          call ExtractStr(Mov_Command,5,str,idum)
          if(lPerform) then
            call StrToRV(str,r,iErr2)
            rMovVar(ir)=rMovVar(ir)*r
          end if
        else ! syntax: mul var vn r
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.eq.0) then
            if(lPerform) rMovVar(ir)=rMovVar(ir)*r
          else
            if(idum.lt.2) return
            call ExtractStr(Mov_Command,4,str,idum)
            chr3(1:3)=str(1:3)
            call GetRch(chr3,4_4,m,iErr2)
            if((iErr2.ne.0).or.(m.ne.1)) return
            if(lPerform) rMovVar(ir)=rMovVar(ir)*rch(1)
          end if
        end if
      else
        call StrToRV(Argument(1:lArgument),x,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) ir=0
        ir=Max(0,Min(999,ir))
        if(lPerform) rMovVar(ir)=rMovVar(ir)*x
      end if
    Case('wav')
      call StrToRV(Argument(1:lArgument),x,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        fcFld=fcFld/x
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovMul

  Subroutine MovPro(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr
    Logical lPerform
    Character(32) CommObj
    if(lArgument.lt.1) return
    Select Case(CommObj(1:3))
    Case('fun')
      if(lPerform) then
        string_Pro=Argument(1:lArgument)//Char(0)
        call ProcessFun(.false.)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovPro

  Subroutine MovRea(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,nx,ny,iErr2,idum,i,ir,ir2,n,ios,iWarn
    Logical lPerform
    Character(3) CHR
    Character(32) CommObj
    if(lArgument.lt.1) return
    if(Argument(1:1).eq.'?') then
      Argument(2:2)='w'
      lArgument=2
    end if
    Select Case(CommObj(1:3))
    Case('3dd')
      call ExtractIV(Mov_Command,4,nx,iErr2)
      if(iErr2.ne.0) nx=-1
      call ExtractIV(Mov_Command,5,ny,iErr2)
      if(iErr2.ne.0) ny=0
      call ExtractIV(Mov_Command,6,ir,iErr2)
      if(iErr2.ne.0) ir=0
      call ExtractIV(Mov_Command,7,ir2,iErr2)
      if(iErr2.ne.0) ir2=1
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,D3DFileName,'MAX','3Dd',iErr)
        if(iErr.gt.0) return
        lAskObj=.false.
        lInsertExp=.false.
        if(nx.ge.0) then
          kInsObj=max(0,min(nObj,nx))
          lInsertExp=.true.
        end if
        call Open3DD(.true.,ny,ir,ir2)
      end if
    Case('3dm')
      call ExtractIV(Mov_Command,4,nx,iErr2)
      if(iErr2.ne.0) nx=-1
      call ExtractIV(Mov_Command,5,ir2,iErr2)
      if(iErr2.ne.0) ir2=1
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,M3DFileName,'MAX','3DM',iErr)
        if(iErr.gt.0) return
        lAskObj=.false.
        lInsertObj=.false.
        if(nx.ge.0) then
          kInsObj=max(0,min(nObj,nx))
          lInsertObj=.true.
        end if
        call Open3DM(.true.,ir2)
      end if
    Case('bas')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,PETFileName,'MAX','BAS',iErr)
        if(iErr.gt.0) return
        call openBasis(.true.)
        fPET=1.0d0
      end if
    Case('bit','bmp')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,BmpFileName,'MAX','BMP',iErr)
        if(iErr.gt.0) return
        call OpenBitmap(.true.)
      end if
    Case('bou')
      call ExtractIV(Mov_Command,4,nx,iErr2)
      if(iErr2.ne.0) nx=-1
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,BndFileName,'MAX','BND',iErr)
        if(iErr.gt.0) return
        lAskBnd=.false.
        lInsertBnd=.false.
        if(nx.ge.0) then
          kInsObj=max(0,min(nBnd,nx))
          lInsertBnd=.true.
        end if
        call OpenBoundary(.true.)
      end if
    Case('dir')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,DirFileName,'MAX','DIR',iErr)
        if(iErr.gt.0) return
        call OpenDirectives(.true.)
      end if
    Case('dom')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,DomFileName,'MAX','DOM',iErr)
        if(iErr.gt.0) return
        call OpenDomain(.true.)
      end if
    Case('exp')
      call ExtractIV(Mov_Command,4,nx,iErr2)
      if(iErr2.eq.0) then
        idum=0
      else
        nx=-1
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.gt.0) then
          idum=0
          if((sch(1:1).eq.'f').or.(sch(1:1).eq.'F')) idum=1
        end if
      end if
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ExpFileName,'MAX','EXP',iErr)
        if(iErr.gt.0) return
        if(idum.gt.0) then
          open(1,file=ExpFileName,status='old',iostat=idum)
          if(idum.eq.0) then
            call ReadStr(1,sch,idum)
            if(idum.eq.0) call chread2(1,ich,1,rch,2,idum)
          end if
          close(1)
          if(idum.eq.0) fcFld=DCmplx(rch(1),rch(2))
        else
          lAskExp=.false.
          lInsertExp=.false.
          if(nx.ge.0) then
            kInsObj=max(0,min(nExp,nx))
            lInsertExp=.true.
          end if
          call OpenExpansion(.true.)
        end if
      end if
    Case('fie')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,FldFileName,'MAX','FLD',iErr)
        if(iErr.gt.0) return
        lDiffField=.false.
        lDiffRel=.false.
        lAskFld=.false.
        lSkipFld=.false.
        lSkipDerFld=.false.
        lSkipVecFld=.false.
        lSkipScaFld=.false.
        lSkipFldHead=.false.
        lFLDset0=.true.
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.ge.3) then
          if(sch(1:3).eq.'rep') then
            lSkipFld=.true.
            lSkipDerFld=.true.
          else if(sch(1:3).eq.'val') then
            lSkipFldHead=.true.
            lFLDset0=.false.
          else if(sch(1:3).eq.'der') then
            lSkipFldHead=.true.
            lSkipFld=.true.
          end if
        end if
        call OpenField(.true.)
        lFLDset0=.false.
      end if
    Case('flf')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,FlfFileName,'MAX','FLF',iErr)
        if(iErr.gt.0) return
        call OpenFField(.true.)
      end if
    Case('flt','PFD')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,PFDFileName,'MAX','PFD',iErr)
        if(iErr.gt.0) return
        call OpenPFD(.true.)
      end if
    Case('fun')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,FunFileName,'MAX','FUN',iErr)
        if(iErr.gt.0) return
        lAskFun=.false.
        lSkipFun=.false.
        lSkipFunHead=.false.
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.ge.3) then
          if(sch(1:3).eq.'rep') then
            lSkipFun=.true.
          else if(sch(1:3).eq.'val') then
            lSkipFunHead=.true.
          end if
        end if
        call ExtractIV(Mov_Command,5,nx,idum)
        if(idum.ne.0) nx=0
        call ExtractIV(Mov_Command,6,ny,idum)
        if(idum.ne.0) ny=0
        iFunRow=nx
        iFunKol=ny
        call ExtractIV(Mov_Command,7,nx,idum)
        if(idum.ne.0) nx=0
        call ExtractIV(Mov_Command,8,ny,idum)
        if(idum.ne.0) ny=0
        nFunRow=nx
        nFunKol=ny
        iWarn=0
        call openFunction(.true.,iWarn)
        if(iWarn.ne.0) call openFunction(.true.,iWarn)
      end if
    Case('grf')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,GrfFileName,'MAX','GRF',iErr)
        if(iErr.gt.0) return
        call OpenFGrid(.true.)
      end if
    Case('grt')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,GrtFileName,'MAX','GRT',iErr)
        if(iErr.gt.0) return
        call OpenTGrid(.true.)
      end if
    Case('int')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,IntFileName,'MAX','INT',iErr)
        if(iErr.gt.0) return
        call OpenIntegral(.true.)
      end if
    Case('mmp')
      if(lPerform) then
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.lt.1) then
          idum=1
          sch(1:1)='h'
        end if
        if(sch(1:1).eq.'a') then
          lMMPrMtr=.true.
          lMMPtMtr=.true.
        else if(sch(1:1).eq.'r') then
          lMMPrMtr=.true.
          lMMPtMtr=.false.
        else if(sch(1:1).eq.'t') then
          lMMPrMtr=.false.
          lMMPtMtr=.true.
        else
          lMMPrMtr=.false.
          lMMPtMtr=.false.
        end if
        call setNameS(Argument,lArgument,ProFileName,MmpFileName,'MAX','MMP',iErr)
        if(iErr.gt.0) return
        call OpenMmp(.true.)
      end if
    Case('obj')
      call ExtractIV(Mov_Command,4,nx,iErr2)
      if(iErr2.ne.0) nx=-1
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ObjFileName,'MAX','3DO',iErr)
        if(iErr.gt.0) return
        lAskObj=.false.
        lInsertObj=.false.
        if(nx.ge.0) then
          kInsObj=max(0,min(nObj,nx))
          lInsertObj=.true.
        end if
        call OpenObject(.true.)
      end if
    Case('ogl')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,OGLFileName,'MAX','OGL',iErr)
        if(iErr.gt.0) return
        call OpenCHGLWindow(.true.)
      end if
    Case('pal')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,PalFileName,'MAX','PAL',iErr)
        if(iErr.gt.0) return
        call OpenPalette(.true.)
      end if
    Case('par')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ParFileName,'MAX','PAR',iErr)
        if(iErr.gt.0) return
        call OpenParameter(.false.)
      end if
    Case('pro')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ProFileName,'MAX','PRO',iErr)
        if(iErr.gt.0) return
        call OpenProAll(.true.)
      end if
    Case('var')
      call ExtractIV(Mov_Command,3,ir,iErr2,99,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,5,n,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        open(1,file=Argument(1:lArgument)//Char(0),status='old',iostat=ios)
        if(ios.ne.0) return
        do i=1,n-1
          read(1,'(a)',iostat=ios) chr(1:1)
          if(ios.ne.0) then
            close(1)
            return
          end if
        end do
        read(1,*,iostat=ios) rMovVar(ir:ir2)
        close(1)
      end if
    Case('win')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,WinFileName,'MAX','WIN',iErr)
        if(iErr.gt.0) return
        call OpenWindow(.true.)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovRea

  Subroutine MovRef(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,ir,ir2,iErr2,nt,ix,i,j,k
    Logical lPerform
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('all')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        call ReflAll(ix)
      end if
    Case('bou')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call reflBoundary(ir,ix,ir2)
      end if
    Case('col')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call reflColor(ir,ix)
    Case('con')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call reflConnection(ir,ix)
    Case('cor')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nt,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,5,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call reflCorner(ir,nt,ix,ir2)
    Case('dom')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call reflDomain(ir,ix)
    Case('exp')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        iDomExp=0_2
        iColExp=0_2
        iConExp=0_2
        call reflExpansion(ir,ix,ir2)
      end if
    Case('gri')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ix,iErr2)
      if(iErr2.ne.0) ix=1
      if(lPerform) then
        if(lrGrd) then
          if(ix.eq.1) spacecFld(1,0)=-spacecFld(1,0)
          if(ix.eq.2) spacecFld(2,0)=-spacecFld(2,0)
          if(ix.eq.3) spacecFld(3,0)=-spacecFld(3,0)
        else
          do k=1,nzcFld
            do j=1,nycFld
              do i=1,nxcFld
                if(ix.eq.1) rGrd(1,i,j,k)=-rGrd(1,i,j,k)
                if(ix.eq.2) rGrd(2,i,j,k)=-rGrd(2,i,j,k)
                if(ix.eq.3) rGrd(3,i,j,k)=-rGrd(3,i,j,k)
              end do
            end do
          end do
        end if
      end if
    Case('win')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,Int4(nWin))
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ix,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call reflWindows(ir,ix)
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovRef

  Subroutine MovRen(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,iErr0,i1,i2,i
    Logical lPerform
    Character(32) CommObj
    call StrToIV(Argument(1:lArgument),i1,iErr0)
    if(iErr0.ne.0) return
    call ExtractIV(Mov_Command,4,i2,iErr0)
    if(iErr0.ne.0) return
    if(.not.lPerform) return
    Select Case(CommObj(1:3))
    Case('col')
      if((i1.lt.0).or.(i1.gt.255)) return
      if((i2.lt.0).or.(i2.gt.255)) return
      do i=1,nBnd
        if(tBnd(i)%iCol.eq.Int2(i1)) tBnd(i)%iCol=Int2(i2)
      end do
      do i=1,nExp
        if(tExp(i)%iCol.eq.Int2(i1)) tExp(i)%iCol=Int2(i2)
      end do
    Case('con')
      if((i1.lt.0).or.(i2.lt.0)) return
      do i=1,nBnd
        if(tBnd(i)%iConn.eq.Int2(i1)) tBnd(i)%iConn=Int2(i2)
      end do
      do i=1,nExp
        if(tExp(i)%iConn.eq.Int2(i1)) tExp(i)%iConn=Int2(i2)
      end do
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovRen

  Subroutine MovRot(CommObj,lPerform,iErr)
    Implicit none
    Real(8) x,y,z
    Integer(4) iErr,ir,ir2,iErr2,nt,i,k1,k2
    Logical lPerform
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('3de')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      if(lPerform) then
        do i=ir,ir2
          call rotate3DE(i,x,y,z,.true.)
        end do
      end if
    Case('all')
      if(lArgument.lt.1) return
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call rotateAll(x,y,z,.true.)
    Case('bou')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call rotateBoundary(ir,x,y,z,.true.,ir2)
      end if
    Case('col')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call rotateColor(ir,x,y,z,.true.)
    Case('con')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call rotateConnection(ir,x,y,z,.true.)
    Case('cor')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nt,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call rotateCorner(ir,nt,x,y,z,.true.,ir2)
    Case('dom')
      if(lArgument.lt.1) return
      call StrToIV(Argument(1:lArgument),ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call rotateDomain(ir,x,y,z,.true.)
    Case('exp')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        iDomExp=0_2
        iColExp=0_2
        iConExp=0_2
        call rotateExpansion(ir,x,y,z,.true.,ir2)
      end if
    Case('exc')
      if(lArgument.lt.1) return
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,5,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      call ExtractIV(Mov_Command,6,ir,iErr2,nExc,ir2)
      if(iErr2.ne.0) return
      if(lPerform) then
        do i=ir,ir2
          call rotate3DE(nExp-nExc+i,x,y,z,.true.)
        end do
      end if
    Case('obj')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nObj)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,6,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      if(lPerform) then
        if(ir.gt.0) then
          k1=min(ir,nObj)
          k2=k1
        else if(ir.lt.0) then
          k1=1
          k2=min(-ir,nObj)
        else
          k1=1
          k2=nObj
        end if
        do kObj=k1,k2
          spa=tObj(kObj)%plane
          spa=Rot3DSpaceX(spa,x)
          spa=Rot3DSpaceY(spa,y)
          spa=Rot3DSpaceZ(spa,z)
          tObj(kObj)%plane=spa
          do i=1,nExp
            if(tExp(i)%iObj.ne.kObj) Cycle
            call rotate3DE(i,x,y,z,.true.)
          end do
        end do
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovRot

  Integer(4) Function MBPEfunA(z,f,f1,n)
! get MBPE input function values from linear interpolation in the Fun array
    implicit none
    integer(4) n,i,k
    complex(8) f(n),f1(n)
    real(8) z,fr,fi
    MBPEfunA=1
    f1(1)=(0.0d0,0.0d0)
    if(Fun(iMBPEfreq,1).lt.Fun(iMBPEfreq,nFun)) then ! z increasing when iMBPEfreq increasing
      if(z.le.Fun(iMBPEfreq,1)) then
        f(1:n)=dcmplx(Fun(iMBPEfr(1:n),1),Fun(iMBPEfi(1:n),1))
        return
      end if
      do i=1,nFun-1
        if((z.ge.Fun(iMBPEfreq,i)).and.(z.le.Fun(iMBPEfreq,i+1))) then
          do k=1,n
            fr=Fun(iMBPEfr(k),i)+(z-Fun(iMBPEfreq,i))*(Fun(iMBPEfr(k),i+1)-Fun(iMBPEfr(k),i))/ &
            & (Fun(iMBPEfreq,i+1)-Fun(iMBPEfreq,i))
            fi=Fun(iMBPEfi(k),i)+(z-Fun(iMBPEfreq,i))*(Fun(iMBPEfi(k),i+1)-Fun(iMBPEfi(k),i))/ &
            & (Fun(iMBPEfreq,i+1)-Fun(iMBPEfreq,i))
            f(k)=dcmplx(fr,fi)
          end do
          return
        end if
      end do    
      f(1:n)=dcmplx(Fun(iMBPEfr(1:n),nFun),Fun(iMBPEfi(1:n),nFun))
    else                                             ! z decreasing when iMBPEfreq increasing
      if(z.ge.Fun(iMBPEfreq,1)) then
        f(1:n)=dcmplx(Fun(iMBPEfr(1:n),1),Fun(iMBPEfi(1:n),1))
        return
      end if
      do i=1,nFun-1
        if((z.le.Fun(iMBPEfreq,i)).and.(z.ge.Fun(iMBPEfreq,i+1))) then
          do k=1,n
            fr=Fun(iMBPEfr(k),i)+(z-Fun(iMBPEfreq,i))*(Fun(iMBPEfr(k),i+1)-Fun(iMBPEfr(k),i))/ &
            & (Fun(iMBPEfreq,i+1)-Fun(iMBPEfreq,i))
            fi=Fun(iMBPEfi(k),i)+(z-Fun(iMBPEfreq,i))*(Fun(iMBPEfi(k),i+1)-Fun(iMBPEfi(k),i))/ &
            & (Fun(iMBPEfreq,i+1)-Fun(iMBPEfreq,i))
            f(k)=dcmplx(fr,fi)
          end do
          return
        end if
      end do    
      f(1:n)=dcmplx(Fun(iMBPEfr(1:n),nFun),Fun(iMBPEfi(1:n),nFun))
    end if
  end Function MBPEfunA   

  Integer(4) Function MBPEfun(z,f,f1,n)
! get MBPE input function values from % movie directives
    implicit none
    integer(4) n,k,k1,k2,idum,i,l
    complex(8) f(n),f1(n)
    real(8) z
    f=(0.0d0,0.0d0)
    f1=(0.0d0,0.0d0)
    MBPEfun=1
    k1=nMovCommand
    rMovVar(iMovVarIn)=z
    do k=MBPEcommandL+1,nMovCommand
      i=k
      call MovieCommand(.true.,k,idum,i)
      if(idum.ne.1952) then
        k1=k+1 ! first command line with % sign
        exit
      end if
    end do
    do k=k1,nMovCommand
      i=k
      call MovieCommand(.true.,k,idum,i)
      if(idum.eq.1952) then
        k2=k-1 ! last command line with % sign (usually defines rMovVar(iMovVarOut1),rMovVar(iMovVarOut2))
        exit
      end if
    end do
    do l=1,n
      f(l)=dcmplx(rMovVar(iMovVarOut1+2*(l-1)),rMovVar(iMovVarOut2+2*(l-1)))
    end do
    if(l4.and.l5.and.(MBPEioutput>0)) write(*,*) 'MBPE z,f=',z,cdabs(f)
    kMovCommand=k2
  end Function MBPEfun   

  Subroutine MovRun(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,i1,i2,idum,i,j,k,l,ll,n,maxo,lFile,kMBP
    Integer(2) idu
    Real(8) r(3),d
    Real(8), allocatable :: xo(:),tro(:,:),tio(:,:),xout(:),trout(:),tiout(:)
    Complex(8) zm
    Logical lPerform,ldum
    Character(32) CommObj
    Character(256) FileName
    if(lArgument.lt.1) return
    Select Case(CommObj(1:3))
    Case('dos')
      call BStrB(Mov_Command,1,i1,i2)
      call BStrB(Mov_Command,i2+1,i1,i2)
      if(lPerform) ldum=SystemQQ(Mov_Command(i2+2:lsMovCommand)//Char(0))
    Case('mbp')
      kMBP=kMovCommand
      ierr=1
      call ExtractStr(Mov_Command,3,sch,idum)
      if(idum.lt.3) return
      Select case(sch(1:3))
      Case('ada') ! test for MBPE commands
        ierr=0
        call ExtractIV(Mov_Command,4,iMBPEfreq,iErr)       ! argument containing the variable
        if(iErr.ne.0) return
        call ExtractIV(Mov_Command,5,iMBPEfr(1),iErr)      ! argument containing the first real part input
        if(iErr.ne.0) return
        call ExtractIV(Mov_Command,6,iMBPEfi(1),iErr)      ! argument containing the first imaginary part input
        if(iErr.ne.0) iMBPEfi(1)=iMBPEfr(1)
        call ExtractIV(Mov_Command,7,i,iErr)               ! argument containing the variable output
        if(iErr.ne.0) i=iMBPEfreq
        call ExtractIV(Mov_Command,8,j,iErr)               ! argument containing the first real part output
        if(iErr.ne.0) j=iMBPEfr(1)
        call ExtractIV(Mov_Command,9,k,iErr)               ! argument containing the first imaginary part output
        if(iErr.ne.0) k=iMBPEfi(1)
        call ExtractStr(Mov_Command,10,Argument,lArgument) ! file name string
        if(lArgument.lt.5) then
          lArgument=8
          Argument(1:8)='MBPE.FUN'
        end if
        FileName=Argument
        lFile=lArgument
        call ExtractIV(Mov_Command,11,n,iErr)              ! number of output values
        if(iErr.ne.0) n=nFun
        if(n.lt.1) n=nFun
        if(lPerform) then
          MBPEmaxp=0 ! use overdetermination factor for adaptive MBPE
          if((iMBPEfreq.lt.1).or.(iMBPEfr(1).lt.1).or.(iMBPEfi(1).lt.1)) then
            MBPEcommandL=kMovCommand
            d=adaptive_MBPE(MBPEfun) ! get MBPE input function values from % movie directives
            iMBPEfreq=i
            iMBPEfr(1:MBPEnparam)=j
            iMBPEfi(1:MBPEnparam)=k
          else
            MBPEcommandL=kMovCommand
            d=adaptive_MBPE(MBPEfunA) ! get MBPE input function values from linear interpolation in the Fun array
          end if
          rMovVar(iMovVarErr)=d
          if(iMBPEerr.gt.0) return
          Allocate(xo(MBPEntest),tro(MBPEnparam,MBPEntest),tio(MBPEnparam,MBPEntest),stat=iErr)
          if(iErr.ne.0) then
            if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
            return
          end if
          if((i.gt.0).or.(j.gt.0).or.(k.gt.0)) then    ! overwrite Fun array
            i=max(0,min(nFunA,i))
            j=max(0,min(nFunA,j))
            k=max(0,min(nFunA,k))
            MBPEntest=nFun
            xo(1:nFun)=Fun(iMBPEfreq,1:nFun)
            call SaveMBPEdata0(MBPEntest,.true.,xo,tro,tio)
            if(i.gt.0) Fun(i,1:nFun)=xo(1:nFun)        ! MBPE variable
            do ll=1,MBPEnparam
              if(j.gt.0) Fun(j+2*ll-2,1:nFun)=tro(ll,1:nFun)     ! MBPE approximation of real parts
              if(k.gt.0) Fun(k+2*ll-2,1:nFun)=tio(ll,1:nFun)     ! MBPE approximation of imaginary parts
            end do
          else                                         ! write result on FUN file
            call SaveMBPEdata(MBPExstartOut,MBPExendOut,MBPEntest,.true.,xo,tro,tio,0_4)
            open(1,file=FileName(1:lFile),iostat=iErr)
            if(iErr.ne.0) return
            call WriteStr(1,' CHFUN Version 1.0'C,iErr)
            write(1,*) '0 ',3*MBPEnparam+1
            write(1,'(a)') 'n MBPE'
            write(1,'(a)') 'z MBPE'
            do ll=1,MBPEnparam
              write(1,'(a,1x,1i2)') 'fr MBPE',ll
              write(1,'(a,1x,1i2)') 'fi MBPE',ll
              write(1,'(a,1x,1i2)') 'fa MBPE',ll
            end do
            do l=1,MBPEntest
              write(1,*) xo(l),tro(1,l),tio(1,l),dsqrt(tro(1,l)**2+tio(1,l)**2)
              if(MBPEnparam.gt.1) then
                do ll=2,MBPEnparam
                  write(1,*) tro(ll,l),tio(ll,l),dsqrt(tro(ll,l)**2+tio(ll,l)**2)
                end do
              end if
            end do
            write(1,*) ' '
            close(1,iostat=iErr)
          end if
          lFile=lFile+1
          FileName(lFile-4:lFile)='S.FUN' ! MBPE sample data filename
          call SaveSamplePoints(FileName(1:lFile))
          call deallocate_MBPE_data
          Deallocate(xo,tro,tio,stat=iErr)
        end if
        kMovCommand=kMBP
      Case('com') ! run all MBPE commands (with % flags)
        if(lPerform) then
          do k=1,nMovCommand
            idum=0
            call MovieCommand(.true.,k,idum,i)
            if(idum.ne.1952) write(*,*) 'k,i,ierr:',k,i,idum
          end do
        end if
      Case('fun') ! complex nth-order MBPE approximation of data stored in Fun array
        call ExtractIV(Mov_Command,4,iMBPEfreq,iErr)       ! argument containing the variable
        if(iErr.ne.0) return
        call ExtractIV(Mov_Command,5,iMBPEfr(1),iErr)      ! argument containing the first real part input
        if(iErr.ne.0) return
        call ExtractIV(Mov_Command,6,iMBPEfi(1),iErr)      ! argument containing the first imaginary part input
        if(iErr.ne.0) iMBPEfi(1)=iMBPEfr(1)
        call ExtractIV(Mov_Command,7,i,iErr)               ! argument containing the variable output
        if(iErr.ne.0) i=iMBPEfreq
        call ExtractIV(Mov_Command,8,j,iErr)               ! argument containing the first real part output
        if(iErr.ne.0) j=iMBPEfr(1)
        call ExtractIV(Mov_Command,9,k,iErr)               ! argument containing the first imaginary part output
        if(iErr.ne.0) k=iMBPEfi(1)
        call ExtractStr(Mov_Command,10,Argument,lArgument) ! file name string
        if(lArgument.lt.5) then
          lArgument=8
          Argument(1:8)='MBPE.FUN'
        end if
        FileName=Argument
        lFile=lArgument
        call ExtractIV(Mov_Command,11,n,iErr)              ! number of output values
        if(iErr.ne.0) n=MBPEntest
        if(n.lt.1) n=nFun
        i=max(0,min(nFunA,i))
        j=max(0,min(nFunA,j))
        k=max(0,min(nFunA,k))
        iMBPEfreq=min(nFunA,iMBPEfreq)
        if(iMBPEfreq.lt.1) iMBPEfreq=max(1,i)
        iMBPEfr(1)=min(nFunA,iMBPEfr(1))
        iMBPEfi(1)=min(nFunA,iMBPEfi(1))
        if(iMBPEfr(1).lt.1) iMBPEfr(1)=max(1,j)
        if(iMBPEfi(1).lt.1) iMBPEfi(1)=max(1,k)
        do l=2,MBPEnparam
          iMBPEfr(l)=min(nFunA,iMBPEfr(l-1)+2)
          iMBPEfi(l)=min(nFunA,iMBPEfi(l-l)+2)
          if(iMBPEfr(l).lt.1) iMBPEfr(l)=max(1,j)
          if(iMBPEfi(l).lt.1) iMBPEfi(l)=max(1,k)
        end do
        if(n.eq.nFun) then ! use smooth MBPE
          call ExtractIV(Mov_Command,12,i1,iErr)           ! number of segments
          if(iErr.ne.0) i1=1
          call ExtractIV(Mov_Command,13,i2,iErr)           ! number of overlapping points between segments
          if(iErr.ne.0) i2=0
          call ExtractRV(Mov_Command,14,d,iErr)            ! max. error in %
          if(iErr.ne.0) d=0.0d0
          if(lPerform) then
            maxo=MBPEmaxorder
            i2=min(max(0,i2),nFun/(2*i1))
            i1=min(max(1,i1),nFun/(2*i1))
            allocate(xout(n),trout(n),tiout(n))
            do ll=1,MBPEnparam
              call smoothMBPEsections(MBPEfunA,Fun(iMBPEfreq,1:nFun),Fun(iMBPEfr(ll),1:nFun),Fun(iMBPEfi(ll),1:nFun), &
              &                       nFun,i1,i2,maxo,d,xout,trout,tiout,iErr)
              if((i.gt.0).or.(j.gt.0).or.(k.gt.0)) then ! overwrite Fun array
                if(i.gt.0) Fun(i,1:nFun)=xout(1:nFun)   ! MBPE variable
                if(j.gt.0) Fun(j+2*ll-2,1:nFun)=trout(1:nFun)  ! MBPE approximation of real part
                if(k.gt.0) Fun(k+2*ll-2,1:nFun)=tiout(1:nFun)  ! MBPE approximation of imaginary part
              else                                      ! write MBPE results on MBPEnparam files
                if(ll.gt.1) call setNameS('+',1,ProFileName,FileName,'MAX','FUN') ! increase file number
                open(1,file=FileName(1:lFile),iostat=iErr)
                if(iErr.ne.0) return
                call WriteStr(1,' CHFUN Version 1.0'C,iErr)
                write(1,*) '0 4'
                write(1,'(a)') 'n MBPE'
                write(1,'(a)') 'z MBPE'
                write(1,'(a)') 'fr MBPE'
                write(1,'(a)') 'fi MBPE'
                write(1,'(a)') 'fa MBPE'
                do l=1,MBPEntest
                  write(1,*) xo(l),tro(1,l),tio(1,l),dsqrt(tro(1,l)**2+tio(1,l)**2)
                end do
                write(1,*) ' '
                close(1,iostat=iErr)
              end if
            end do
            deallocate(xout,trout,tiout,stat=iErr)
          end if
        else               ! use single interval MBPE approximation
          n=max(2,n)
          MBPEndiv     =nFun                              ! initial number of points = input points
          MBPExstart   =Fun(iMBPEfreq,1)                  ! MBPE range xmin 
          MBPExend     =Fun(iMBPEfreq,nFun)               ! MBPE range xmax
          MBPExstartOut=MBPExstart
          MBPExendOut=  MBPExend
          call ExtractRV(Mov_Command,12,r(1),iErr)        ! xmin
          if(iErr.ne.0) r(1)=-2.0d0*pBig
          call ExtractRV(Mov_Command,13,r(2),iErr)        ! xmax
          if(iErr.ne.0) r(2)=-2.0d0*pBig
          if(r(1).gt.nBig) MBPExstart=r(1)
          if(r(2).gt.nBig) MBPExend  =r(2)
          call ExtractRV(Mov_Command,14,r(1),iErr)        ! xminOut
          if(iErr.ne.0) r(1)=-2.0d0*pBig
          call ExtractRV(Mov_Command,15,r(2),iErr)        ! xmaxOut
          if(iErr.ne.0) r(2)=-2.0d0*pBig
          if(r(1).gt.nBig) MBPExstartOut=r(1)
          if(r(2).gt.nBig) MBPExendOut  =r(2)
          MBPEmaxncalc = nFun
          MBPEntest    = n                                ! number of output points
          MBPEmaxp=MBPEndiv
          if((lArgument.lt.5).and.(MBPEntest.ne.nFun)) then
            lArgument=8
            Argument(1:8)='MBPE.FUN'
          end if
          if(lPerform) then
            MBPEcommandL=kMovCommand
            d=adaptive_MBPE(MBPEfunA) ! get MBPE input function values from linear interpolation in the Fun array
            rMovVar(iMovVarErr)=d
            if(iMBPEerr.gt.0) return
            Allocate(xo(MBPEntest),tro(MBPEnparam,MBPEntest),tio(MBPEnparam,MBPEntest),stat=iErr)
            if(iErr.ne.0) then
              if(MBPEioutput>-1) write(*,*) 'MBPE memory allocation failed!'
              return
            end if
            if((MBPEntest.eq.nFun).and.((i.gt.0).or.(j.gt.0).or.(k.gt.0))) then ! overwrite Fun array
              xo(1:nFun)=Fun(iMBPEfreq,1:nFun)
              call SaveMBPEdata0(MBPEntest,.true.,xo,tro,tio)
              if(i.gt.0) Fun(i,1:nFun)=xo(1:nFun)    ! MBPE variable
              if(j.gt.0) Fun(j,1:nFun)=tro(1,1:nFun) ! MBPE approximation of real part
              if(k.gt.0) Fun(k,1:nFun)=tio(1,1:nFun) ! with MBPE approximation of imaginary part
            else                                     ! write MBPE result on file
              call SaveMBPEdata(MBPExstartOut,MBPExendOut,MBPEntest,.true.,xo,tro,tio,0_4)
              open(1,file=FileName(1:lFile),iostat=iErr)
              if(iErr.ne.0) return
              call WriteStr(1,' CHFUN Version 1.0'C,iErr)
              write(1,*) ' 0 4'
              write(1,*) ' n MBPE'
              write(1,*) ' z MBPE'
              write(1,*) ' fr MBPE'
              write(1,*) ' fi MBPE'
              write(1,*) ' fa MBPE'
              do l=1,n
                write(1,*) xo(l),tro(1,l),tio(1,l),dsqrt(tro(1,l)**2+tio(1,l)**2)
              end do
              write(1,*) ' '
              close(1,iostat=iErr)
            end if
            call deallocate_MBPE_data
            Deallocate(xo,tro,tio,stat=iErr)
          end if
        end if
      Case('fie') ! 2nd order MBPE minimum search using data stored in cFld (from previous rough eigenvalue search)
        call ExtractIV(Mov_Command,3,i1,iErr)   ! ix location on cFld grid
        if(iErr.ne.0) return
        call ExtractIV(Mov_Command,4,i2,iErr)   ! iy location on cFld grid
        if(iErr.ne.0) return
        call ExtractIV(Mov_Command,5,idum,iErr) ! number of MBPE points (5 or 9)
        if(iErr.ne.0) idum=5
        nMBPE=idum
        call ExtractIV(Mov_Command,6,idum,iErr) ! use component (1..6) in cFld for search
        if(iErr.ne.0) idum=4
        if(lPerform) then
          idum=max(1,min(6,idum))
          i1=max(2,min(nxcFld-1,i1))
          i2=max(2,min(nycFld-1,i2))
          if(nMBPE.lt.6) then                   ! 5 point star
            nMBPE=5
            r=getcGrd(i1,i2,1)
            zMBPE(1)=dCmplx(r(1),r(2))
            fMBPE(1)=cFld(idum,i1,i2,1)
            r=getcGrd(i1-1,i2,1)
            zMBPE(2)=dCmplx(r(1),r(2))
            fMBPE(2)=cFld(idum,i1-1,i2,1)
            r=getcGrd(i1+1,i2,1)
            zMBPE(3)=dCmplx(r(1),r(2))
            fMBPE(3)=cFld(idum,i1+1,i2,1)
            r=getcGrd(i1,i2-1,1)
            zMBPE(4)=dCmplx(r(1),r(2))
            fMBPE(4)=cFld(idum,i1,i2-1,1)
            r=getcGrd(i1,i2+1,1)
            zMBPE(5)=dCmplx(r(1),r(2))
            fMBPE(5)=cFld(idum,i1,i2+1,1)
          else                                  ! 9 point square
            nMBPE=9
            k=0
            do i=-1,1
              do j=-1,1
                r=getcGrd(i1+i,i2+j,1)
                k=k+1
                zMBPE(k)=dCmplx(r(1),r(2))
                fMBPE(k)=cFld(idum,i1+i,i2+j,1)
              end do
            end do
          end if
          call cMBPEsearch2O(41,4.0d0*cdabs(zMBPE(1)-zMBPE(2)),zm)
          if(MBPEioutput>0) write(*,*) 'MBPE minimum:',zm
          d=0.1d0*cdAbs(zMBPE(1)-zMBPE(2))      ! set eigenvalue search area
          c1Eigen=zm+dCmplx(d,d)
          c2Eigen=zm+dCmplx(d,d)
          idum=idum+1                           ! overwrite cFld(idum+1...) with MBPapprox.
          if(idum.gt.6) idum=1
          do i=1,nxcFld
            do j=1,nycFld
              r=getcGrd(i,j,1)
              zm=dCmplx(r(1),r(2))
              cFld(idum,i,j,1)=cMBPEapprox2O(zm)
            end do
          end do
        end if
      Case default
        ierr=-9
      end select
    Case('pro')
      call ExtractStrRest(Mov_Command,4,sch,idum)
      if(idum.lt.1) then
        idum=1
        sch(1:1)=' '
      end if
      if(lPerform) idu=RunQQ(Argument(1:lArgument),sch(1:idum))
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovRun

  Subroutine MovSet(CommObj,lPerform,iErr)
    Implicit none
    Complex(8) c,cf(10)
    Real(8) r,x,y,z,t,u,v,w,u1,v1,w1,f,df,d,rl(3),dmin,rmin(3),val(2)
    Integer(4) iErr,ir,ir2,iErr1,iErr2,iErr3,iErr4,iErr5,iErr6,nx,ny,nz,nt,m,idum,n,iErr0,i,l,nx1,nx2
    Integer(4), external:: iGetDigit
    Integer(2) idu
    Integer(2) iOK
    Logical lPerform,ldum,lch,lh,le,lx,ly,lz,lp,lm,ls,lha,lpw,lpolar,lclip,lsquare,ldb
    Character(1151) str
    Character(32) CommObj,cStr
    Character(3) chr3
    if(lArgument.lt.1) return
    Select Case(CommObj(1:3))
    Case('bou')
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'amp') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundaryAmp(ir,x,ir2)
      else if(Argument(1:3).eq.'cnd') then
        call ExtractStr(Mov_Command,5,Argument,lArgument)
        if(lArgument.lt.3) return
        if(Argument(1:3).eq.'spe') then
          call ExtractStr(Mov_Command,6,sch,idum)
          if(idum.lt.2) return
        end if
        if(lPerform) then
          do i=idum+1,15
            sch(i:i)='-'
          end do
          call SetBoundaryCnd(ir,Argument(1:3),sch(1:15),ir2)
        end if
      else if(Argument(1:3).eq.'col') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundaryCol(ir,nx,ir2)
      else if(Argument(1:3).eq.'con') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundaryCon(ir,nx,ir2)
      else if(Argument(1:3).eq.'cor') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetCorner(ir,nx,x,y,r,ir2)
      else if(Argument(1:3).eq.'dom') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,ny,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundaryDom(ir,nx,ny,ir2)
      else if(Argument(1:3).eq.'for') then
        call ExtractStr(Mov_Command,5,Argument,lArgument)
        if(lArgument.lt.1) return
        if(lPerform) call SetBoundaryFor(ir,Argument,lArgument,ir2)
      else if(Argument(1:3).eq.'pts') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundaryPts(ir,nx,ir2)
      else if(Argument(1:3).eq.'rad') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetCornerR(ir,nx,r,ir2)
      else if(Argument(1:3).eq.'spl') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundarySpl(ir,nx,ir2)
      else if(Argument(1:3).eq.'val') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) call SetBoundaryVal(ir,x,y,ir2)
      else if(Argument(1:3).eq.'wei') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=x
        if(lPerform) call SetBoundaryWei(ir,x,y,ir2)
      else
        return
      end if
    Case('cfk')
      if(lPerform) then
        if(Argument(1:1).eq.'%') then
          nEigenSave=0
          lEigenSave=.false.
        else
          if(Argument(1:1).eq.'?') then
            Argument(2:2)='w'
            lArgument=2
          end if
          call setNameS(Argument,lArgument,ProFileName,CfkFileName,'MAX','CFK',iErr)
          if(iErr.gt.0) return
          nEigenSave=nEigenSave+1
        end if
      end if
    Case('con')
      if(Argument(1:1).eq.'+') then
        ir=Int4(iMMPCon)+1_4
      else if(Argument(1:1).eq.'-') then
        ir=Int4(iMMPCon)-1_4
      else if(Argument(1:1).eq.'i') then
        lInverseConn=.true.
      else if(Argument(1:1).eq.'n') then
        lInverseConn=.false.
      else
        call StrToIV(Argument(1:lArgument),ir,iErr2)
        if(iErr2.ne.0) return
      end if
      if(lPerform) iMMPCon=Int2(ir)
    Case('cor')
      if(lArgument.lt.1) return
      call ExtractIV(Mov_Command,3,ir,iErr2,nBnd,ir2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,nt,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,r,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) call setCorner(ir,nt,x,y,r,ir2)
    Case('dom')
      call ExtractIV(Mov_Command,3,ir,iErr2,nDom)
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.1) return
      if(Argument(1:3).eq.'eps') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          eDom(ir)=DCmplx(x,y)
          x=Dble(eDom(ir))
          y=DImag(eDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(1,ir)=cStr
        end if
      else if(Argument(1:3).eq.'mue') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          uDom(ir)=DCmplx(x,y)
          x=Dble(uDom(ir))
          y=DImag(uDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(2,ir)=cStr
        end if
      else if(Argument(1:3).eq.'sig') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          sDom(ir)=DCmplx(x,y)
          x=Dble(sDom(ir))
          y=DImag(sDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(3,ir)=cStr
        end if
      else if(Argument(1:3).eq.'tau') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        if(lPerform) then
          tDom(ir)=DCmplx(x,y)
          x=Dble(tDom(ir))
          y=DImag(tDom(ir))
          write(cStr,'(1H(,1PE14.6,1H,,1PE14.6,1H))') x,y
          idum=32
          call DelBlanks(cStr,idum)
          Dom_Form(4,ir)=cStr
        end if
      else
        return
      end if
    Case('eig')
      if(lArgument.lt.2) return
      if(lPerform) then
        if(lEigen.and.lPET.and.(kPET.gt.0)) kPET=-1
      end if
      Select Case(Argument(1:2))
      Case('1d') !s')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          ithvmax0=ir
        end if
      Case('cl') !i')
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,r,iErr2)
        if(iErr2.ne.0) r=0.0d0
        if(lPerform) then
          lEigen=.true.
          c1EigenC=DCmplx(min(x,z),min(y,r))
          c2EigenC=DCmplx(max(x,z),max(y,r))
        end if
      Case('co') !r')
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,r,iErr2)
        if(iErr2.ne.0) r=0.0d0
        if(lPerform) then
          lEigen=.true.
          c1Eigen=DCmplx(x,y)
          c2Eigen=DCmplx(z,r)
        end if
      Case('cx')
        if(lPerform) iEigen=2
      Case('de') !t')
        if(lPerform) then
          lMMPDeterminant=.true.
          iMtrSolver=0
        end if
      Case('dn') !o')
        if(lPerform) then
          iEvlDraw=0_2
        end if
      Case('dr') !a')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) ir=1
        if(lPerform) then
          iEvlDraw=1_2
          if(Argument(3:3).eq.'x') iEvlDraw=2_2
          if(Argument(3:3).eq.'y') iEvlDraw=3_2
          iEvlCol=Int2(ir)
        end if
      Case('fi') !n')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) nt=-1000001_4
        call ExtractRV(Mov_Command,6,x,iErr2)
        if(iErr2.ne.0) x=-1.0d0
        call ExtractRV(Mov_Command,7,y,iErr2)
        if(iErr2.ne.0) y=-1.0d0
        if(lPerform) then
          lEigen=.true.
          imEigen=ir
          if(nt.gt.-1000001_4) itmEigen=nt
          if(x.gt.0.0d0) aEigen=x
          if(y.gt.0.0d0) fEigen=y
        end if
      Case('fl') !e')
        if(lPerform) lWriteRoughFld=.true.
      Case('fr') !e')
        if(lPerform) iEigen=0
      Case('fu') !e')
        if(lPerform) lWriteRoughFld=.false.
      Case('ga') !m')
        if(lPerform) iEigen=1
      Case('gr') !i')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) nt=0_4
        if(lPerform) then
          lEigen=.true.
          nrEigen=ir
          niEigen=nt
        end if
      Case('no') !p')
        if(lPerform) then
          kPET=0
          lPET=.false.
        end if
      Case('pe') !t')
        if(lPerform) then
          kPET=0
          lPET=.true.
        end if
      Case('re') !a')
        call ExtractIV(Mov_Command,4,ir,iErr)
        if(iErr.ne.0) return
        if(lPerform) then
          iGetEigFld=ir
        end if
      Case('sm') !p') ! enable SMP search
        if(lPerform) then
          lEvlSMP=.true.
        end if
      Case('sn') !p') ! disable SMP search
        if(lPerform) then
          lEvlSMP=.false.
        end if
      Case('te') !s') ! test level for eigenvalue rough search
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          iEvlTest=ir
        end if
      Case Default
        iErr=-9
        return
      end Select
    Case('exc')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if(ir.lt.0) then
        nRHS=max(1,min(-ir,nPar-1))
        call AllocatePar(ldum)
      end if
      kExc=max(1,min(ir,nRHS))
    Case('exp')
      lch=.true.
      call ExtractIV(Mov_Command,3,ir,iErr2,nExp,ir2)
      if(iErr2.ne.0) then
        iErr2=1
        call ExtractStr(Mov_Command,3,Argument,lArgument)
        if(lArgument.lt.3) return
        if(Argument(1:3).ne.'acc') return
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        ExpSumAcc=r
      end if
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.3) return
      if(lArgument.gt.3) then
        if(Argument(4:4).eq.'-') lch=.false.
      end if
      if(Argument(1:3).eq.'3da') then
        call ExtractRV(Mov_Command,5,spa(1,0),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,spa(2,0),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,spa(3,0),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,spa(1,1),iErr2)
        if(iErr2.ne.0) spa(1,1)=1.0d0
        call ExtractRV(Mov_Command,9,spa(2,1),iErr2)
        if(iErr2.ne.0) spa(2,1)=0.0d0
        call ExtractRV(Mov_Command,10,spa(3,1),iErr2)
        if(iErr2.ne.0) spa(3,1)=0.0d0
        if(lPerform) then
          do kExp=ir,ir2
            tExp(kExp)%O(1:3)=spa(1:3,0)
            tExp(kExp)%e(1:3)=spa(1:3,1)
            call Unit3DV(tExp(kExp)%e)
          end do
          kExp=ir2
        end if
      else if(Argument(1:3).eq.'3dl') then
        call ExtractRV(Mov_Command,5,spa(1,0),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,spa(2,0),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,spa(3,0),iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,spa(1,1),iErr2)
        if(iErr2.ne.0) spa(1,1)=1.0d0
        call ExtractRV(Mov_Command,9,spa(2,1),iErr2)
        if(iErr2.ne.0) spa(2,1)=0.0d0
        call ExtractRV(Mov_Command,10,spa(3,1),iErr2)
        if(iErr2.ne.0) spa(3,1)=0.0d0
        call ExtractRV(Mov_Command,11,spa(1,2),iErr2)
        if(iErr2.ne.0) spa(1,2)=0.0d0
        call ExtractRV(Mov_Command,12,spa(2,2),iErr2)
        if(iErr2.ne.0) spa(2,2)=1.0d0
        call ExtractRV(Mov_Command,13,spa(3,2),iErr2)
        if(iErr2.ne.0) spa(3,2)=0.0d0
        if(lPerform) then
          do kExp=ir,ir2
            spa(1:3,3)=0.0d0
            tExp(kExp)%Plane=spa
            call Ortho3DSpace(tExp(kExp)%Plane)
          end do
        end if
      else if(Argument(1:3).eq.'ang') then
        lch=.false.
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%rE(1)=r
        end if
      else if(Argument(1:3).eq.'col') then
        lch=.false.
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%icol=nx
        end if
      else if(Argument(1:3).eq.'con') then
        lch=.false.
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%iconn=nx
        end if
      else if(Argument(1:3).eq.'dom') then
        lch=.false.
        call ExtractIV(Mov_Command,5,nx,iErr2,nDom)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%idom=nx
        end if
      else if(Argument(1:3).eq.'ics') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do n=ir,ir2
            idu=igetiHE(n)
            tExp(n)%ihe=10_2*Int2(nx)+idu
          end do
        end if
      else if(Argument(1:3).eq.'ihe') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do n=ir,ir2
            idu=igetiCS(n)
            tExp(n)%ihe=Int2(nx)+10_2*idu
          end do
        end if
      else if(Argument(1:3).eq.'obj') then
        lch=.false.
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%iObj=nx
        end if
      else if(Argument(1:3).eq.'typ') then
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%iTypE=nx
        end if
      else if(Argument(1:3).eq.'gam') then
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          tExp(ir:ir2)%gc=DCmplx(x,y)
        end if
      else if(Argument(1:2).eq.'ie') then
        call StrToIV(Argument(3:3),n,iErr2)
        if((iErr2.ne.0).or.(n.lt.1).or.(n.gt.6)) return
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) then
          call ExtractRV(Mov_Command,6,x,iErr2)
          if(iErr2.ne.0) return
          nt=-99999999
        end if
        if(lPerform) then
          if(nt.eq.-99999999) then
            do nt=ir,ir2
              tExp(nt)%iE(n)=nint(Dble(tExp(nt)%iE(n))*x,4)
            end do
          else
            tExp(ir:ir2)%iE(n)=nt
          end if
        end if
      else if(Argument(1:2).eq.'re') then
        call StrToIV(Argument(3:3),n,iErr2)
        if((iErr2.ne.0).or.(n.lt.1).or.(n.gt.5)) return
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(iErr2.ne.0) then
          call ExtractRV(Mov_Command,6,x,iErr2)
          if(iErr2.ne.0) return
          r=-2.0d0*pBig
        end if
        if(lPerform) then
          if(r.lt.nBig) then
            do nt=ir,ir2
              tExp(nt)%rE(n)=tExp(nt)%rE(n)*x
            end do
          else
            tExp(ir:ir2)%rE(n)=r
          end if
        end if
      else if(Argument(1:3).eq.'loc') then
        lch=.false.
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do kExp=ir,ir2
            iDomExp=0_2
            iColExp=0_2
            iConExp=0_2
            r=tExp(kExp)%rE(1)
            call setExpansion(kExp,x,y,r,.true.)
          end do
        end if
      else if(Argument(1:3).eq.'deg') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do kExp=ir,ir2
            if((tExp(kExp)%iTypE.lt.1_2).or.((tExp(kExp)%iTypE.gt.2_2).and.(tExp(kExp)%iTypE.lt.6_2))) Cycle
            nx=4
            tExp(kExp)%iE(nx)=Int2(nt)
          end do
        end if
      else if(Argument(1:3).eq.'ord') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do kExp=ir,ir2
            if((tExp(kExp)%iTypE.lt.1_2).or.(tExp(kExp)%iTypE.eq.3_2)) Cycle
            nx=2
            if(tExp(kExp)%iTypE.eq.5_2) nx=4
            tExp(kExp)%iE(nx)=Int2(nt)
          end do
        end if
      else if(Argument(1:3).eq.'par') then
        call ExtractIV(Mov_Command,5,nt,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,r,iErr2)
        if(iErr2.ne.0) then
          if(lPerform) then
            call CorrExpPar(nt,ir,ir2)
          end if
        else
          call ExtractRV(Mov_Command,7,x,iErr2)
          if(iErr2.ne.0) x=0.0d0
          call ExtractIV(Mov_Command,8,nx,iErr2)
          if(iErr2.ne.0) nx=-1
          if(lPerform) then
            c=dcmplx(r,x) ! r*cdexp((0.0d0,1.0d0)*Pi*x/180.0d0)
            do kExp=ir,ir2
              if(nt.le.0) then
                if(nt.eq.0) nt=-tExp(kExp)%nPar
                do kPar=1,Min(tExp(kExp)%nPar,-nt)
                  if(nx.lt.0) then
                    ParExp(kExc,tExp(kExp)%iOff+kPar)=c
                  else if(nx.gt.0) then
                    nx=min(nx,nRHS)
                    ParExp(nx,tExp(kExp)%iOff+kPar)=c
                  else
                    ParExp(1:nRHS,tExp(kExp)%iOff+kPar)=c
                  end if
                end do
                kPar=Min(tExp(kExp)%nPar,-nt)
              else
                kPar=Min(tExp(kExp)%nPar,nt)
                if(nx.lt.0) then
                  ParExp(kExc,tExp(kExp)%iOff+kPar)=c
                else if(nx.gt.0) then
                  nx=min(nx,nRHS)
                  ParExp(nx,tExp(kExp)%iOff+kPar)=c
                else
                  ParExp(1:nRHS,tExp(kExp)%iOff+kPar)=c
                end if
              end if
            end do
          end if
        end if
        return
      end if
      if(lPerform) then
        if(lch) then
          call CorrExpPar(1000_4)
        else
          do kExp=ir,ir2
            call setOrient(kExp)
          end do
        end if
        kExp=ir2
      end if
    Case('fie')
      if(.not.lgcFld) lGet3DMat=.true.
      if(Argument(1:3).eq.'com') then
        lfcFld=.true.
        if(lPerform.and.(lfcFld.xor.lfcFldAll)) call ClearField(.true.)
      else if(Argument(1:3).eq.'rea') then
        lfcFld=.false.
        if(lPerform.and.(lfcFld.xor.lfcFldAll)) call ClearField(.true.)
      else if(Argument(1:3).eq.'2-d') then
        lgcFld=.true.
      else if(Argument(1:3).eq.'3-d') then
        lgcFld=.false.
      else if(Argument(1:3).eq.'for') then
        call ExtractStr(Mov_Command,4,Argument,lArgument)
        if(lArgument.lt.3) return
        if(lPerform) then
          select case(Argument(1:3))
          case('col')
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            kColFFD=ir
          case('con')
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            kConFFD=ir
          case('dom')
            call ExtractIV(Mov_Command,5,ir,iErr2,nDom)
            if(iErr2.ne.0) return
            kDomFFD=ir
          case('exp')
            call ExtractIV(Mov_Command,5,ir,iErr2,nExp)
            if(iErr2.ne.0) return
            kExpFFD=ir
          case('inc')
            kExpFFD=nExp
          case('par')
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            kParFFD=ir
          case('sca')
            kExpFFD=-nExp+1
          case('tot')
            kExpFFD=0
          end select
        end if
      else if(Argument(1:3).eq.'ori') then
        call ExtractStr(Mov_Command,4,Argument,lArgument)
        if(lArgument.gt.0) call setitcFld(lPerform,Argument(1:lArgument),lArgument,iErr2)
        if(iErr2.eq.0) then
          call ExtractStr(Mov_Command,5,Argument,lArgument)
          if(lArgument.gt.0) call setlxyzcFld(lPerform,Argument(1:lArgument),lArgument,iErr2)
        end if
      else if(Argument(1:3).eq.'der') then
        call ExtractStr(Mov_Command,4,Argument,lArgument)
        if(lArgument.gt.0) then
          call setitrFld(lperform,Argument(1:lArgument),lArgument,iErr2)
        else
          iErr=-1
          iErr2=-1
        end if
        if(iErr2.eq.0) then
          call ExtractStr(Mov_Command,5,Argument,lArgument)
          if(lArgument.gt.0) then
            if(lPerform) call setlxyzrFld(lPerform,Argument(1:lArgument),lArgument,iErr2)
            call ExtractStr(Mov_Command,6,Argument,lArgument)
            if(lPerform.and.(lArgument.gt.0)) then
              if(Argument(1:1).eq.'a') then
                larFld=.true.
                lprFld=.false.
              else if(Argument(1:1).eq.'b') then
                larFld=.true.
                lprFld=.true.
              else if(Argument(1:1).eq.'p') then
                larFld=.false.
                lprFld=.true.
              else
                larFld=.false.
                lprFld=.false.
              end if
            end if
          end if
        end if
      end if
    Case('fil')
      call ExtractStr(Mov_Command,3,str,l)
      if(l.lt.3) return
      call ExtractStr(Mov_Command,4,Argument,lArgument)
      if(lArgument.lt.5) return
      if(lperform) then
        if(Argument(1:1).eq.'?') then
          Argument(2:2)='w'
          lArgument=2
        end if
        select case(str(1:3))
        case('avi')
          call setNameS(Argument,lArgument,ProFileName,AVIFileName,'MAX','AVI',iErr)
        case('bas')
          call setNameS(Argument,lArgument,ProFileName,PETFileName,'MAX','BAS',iErr)
        case('bit','bmp')
          call setNameS(Argument,lArgument,ProFileName,BmpFileName,'MAX','BMP',iErr)
        case('bou')
          call setNameS(Argument,lArgument,ProFileName,BndFileName,'MAX','BND',iErr)
        case('cfk')
          call setNameS(Argument,lArgument,ProFileName,CfkFileName,'MAX','CFK',iErr)
        case('dir')
          call setNameS(Argument,lArgument,ProFileName,DirFileName,'MAX','DIR',iErr)
        case('dom')
          call setNameS(Argument,lArgument,ProFileName,DomFileName,'MAX','DOM',iErr)
        case('exp')
          call setNameS(Argument,lArgument,ProFileName,ExpFileName,'MAX','EXP',iErr)
        case('fie')
          call setNameS(Argument,lArgument,ProFileName,FldFileName,'MAX','FLD',iErr)
        case('flf')
          call setNameS(Argument,lArgument,ProFileName,FlfFileName,'MAX','FLF',iErr)
        case('flt')
          call setNameS(Argument,lArgument,ProFileName,FltFileName,'MAX','FLT',iErr)
        case('PFD')
          call setNameS(Argument,lArgument,ProFileName,PFDFileName,'MAX','PFD',iErr)
        case('fun')
          call setNameS(Argument,lArgument,ProFileName,FunFileName,'MAX','FUN',iErr)
        case('grf')
          call setNameS(Argument,lArgument,ProFileName,GrfFileName,'MAX','GRF',iErr)
        case('grt')
          call setNameS(Argument,lArgument,ProFileName,GrtFileName,'MAX','GRT',iErr)
        case('inf')
          call setNameS(Argument,lArgument,ProFileName,InfFileName,'MAX','FUN',iErr)
        case('int')
          call setNameS(Argument,lArgument,ProFileName,IntFileName,'MAX','INT',iErr)
        case('mmp')
          call setNameS(Argument,lArgument,ProFileName,MMPFileName,'MAX','MMP',iErr)
        case('obj')
          call setNameS(Argument,lArgument,ProFileName,ObjFileName,'MAX','OBJ',iErr)
        case('ogl')
          call setNameS(Argument,lArgument,ProFileName,OGLFileName,'MAX','OGL',iErr)
        case('pal')
          call setNameS(Argument,lArgument,ProFileName,PalFileName,'MAX','PAL',iErr)
        case('par')
          call setNameS(Argument,lArgument,ProFileName,ParFileName,'MAX','PAR',iErr)
        case('pro')
          call setNameS(Argument,lArgument,ProFileName,ProFileName,'MAX','PRO',iErr)
        case('win')
          call setNameS(Argument,lArgument,ProFileName,WinFileName,'MAX','WIN',iErr)
        end select
      end if
    Case('fre')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'var') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) ir=0
            ir=Max(0,Min(98,ir))
            fcFld=DCmplx(rMovVar(ir),rMovVar(ir+1))
          else
            call ExtractRV(Mov_Command,3,x,iErr2)
            if(iErr2.eq.0) fcFld=DCmplx(x,0.0d0)
            call ExtractRV(Mov_Command,4,x,iErr2)
            if(iErr2.eq.0) fcFld=fcFld+DCmplx(0.0d0,x)
          end if
        else
          call ExtractRV(Mov_Command,3,x,iErr2)
          if(iErr2.eq.0) fcFld=DCmplx(x,0.0d0)
          call ExtractRV(Mov_Command,4,x,iErr2)
          if(iErr2.eq.0) fcFld=fcFld+DCmplx(0.0d0,x)
        end if
      end if
    Case('fun')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'arg') then
        call ExtractIV(Mov_Command,4,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,ny,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,ir,iErr2)
        if(iErr2.ne.0) ir=1
        call ExtractIV(Mov_Command,7,ir2,iErr2)
        if(iErr2.ne.0) ir2=nFun
        if(lPerform) then
          iFunA1=Int2(nx)
          if(iFunA1.lt.0) iFunA1=nFunA-iFunA1+1
          iFunA1=max(0_2,min(nFunA,iFunA1))
          iFunA2=Int2(ny)
          if(iFunA2.lt.0) iFunA2=nFunA-iFunA2+1
          iFunA2=max(0_2,min(nFunA,iFunA2))
          iFun1=Min(Max(1,ir),nFun)
          iFun2=Min(Max(iFun1,ir2),nFun)
        end if
      else if(Argument(1:3).eq.'dlw') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,ny,iErr2)
        if((iErr2.ne.0).or.(ny.lt.1)) return
        call ExtractRV(Mov_Command,7,u,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,8,v,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,9,w,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,10,u1,iErr2)
        if(iErr2.ne.0) u1=0.0d0
        call ExtractRV(Mov_Command,11,v1,iErr2)
        if(iErr2.ne.0) v1=v
        call ExtractRV(Mov_Command,12,w1,iErr2)
        if(iErr2.ne.0) w1=w
        if(lPerform) then
          mFun=ny
          mFunA=3
          nFun=mFun
          nFunA=mFunA
          call AllocateFun(ldum)
          if(.not.ldum) return
          FunATitle(0)="n"C
          FunATitle(1)="wavelength"C
          FunATitle(2)="real(drude-lorentz(w))"C
          FunATitle(3)="imag(drude-lorentz(w))"C
          z=(y-x)/Dble(max(1,ny-1))
          do nz=1,ny
            Fun(1,nz)=x+Dble(nz-1)*z
            c=cDrudeLorentz(Fun(1,nz),'w',u,v,w,u1,v1,w1)
            Fun(2,nz)=Dble(c)
            Fun(3,nz)=DImag(c)
          end do
        end if
      else if(Argument(1:3).eq.'dom') then
        call ExtractIV(Mov_Command,4,ir,iErr2,nDom)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if((iErr2.ne.0).or.(x.lt.pSmall)) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if((iErr2.ne.0).or.(y.lt.pSmall)) return
        call ExtractIV(Mov_Command,7,ny,iErr2)
        if((iErr2.ne.0).or.(ny.eq.0)) return
        if(lPerform) then
          mFun=abs(ny)
          mFunA=5
          nFun=mFun
          nFunA=mFunA
          call AllocateFun(ldum)
          if(.not.ldum) return
          FunATitle(0)="n"C
          FunATitle(1)="f"C
          FunATitle(2)="Re(Eps)"C
          FunATitle(3)="Im(Eps)"C
          FunATitle(4)="Re(Mue)"C
          FunATitle(5)="Im(Mue)"C
          z=(y-x)/Dble(max(1,abs(ny)-1))
          y=fcFld
          do nz=1,abs(ny)
            Fun(1,nz)=x+Dble(nz-1)*z
            if(ny.gt.0) then
              fcFld=DCmplx(Fun(1,nz),0.0d0)
            else
              fcFld=DCmplx(1.0d0/(dsqrt(Mue0*Eps0)*Fun(1,nz)),0.0d0)
            end if
            call getEUST(ir)
            Fun(2,nz)=Dble(eDom(ir))
            Fun(3,nz)=Dimag(eDom(ir))
            Fun(4,nz)=Dble(uDom(ir))
            Fun(5,nz)=Dimag(uDom(ir))
          end do
          fcFld=y
        end if
      else if(Argument(1:3).eq.'pol') then
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) ir=0
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) nx=0
        call ExtractIV(Mov_Command,6,ny,iErr2)
        if(iErr2.ne.0) ny=0
        call ExtractIV(Mov_Command,7,nz,iErr2)
        if(iErr2.ne.0) nz=0
        call ExtractRV(Mov_Command,8,x,iErr2)
        if(iErr2.ne.0) x=0.0d0
        call ExtractRV(Mov_Command,9,y,iErr2)
        if(iErr2.ne.0) y=1.0d0
        if(lPerform) then
          lPolar=.false.
          lclip=.false.
          lsquare=.false.
          ldb=.false.
          if(ir.gt.0) lPolar=.true.
          if(nx.gt.0) ldb=.true.
          if(ny.gt.0) lsquare=.true.
          if(nz.gt.0) lclip=.true.
          call setFunctionPolar(lpolar,lclip,lsquare,ldb)
          rFunOff=x
          rFunSca=y
        end if
      else if(Argument(1:3).eq.'mar') then
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,ny,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,7,nz,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          iFunMarkX=Int2(max(0_4,min(1_4,iGetDigit(ir,1_4))))
          iFunMarkP=Int2(max(0_4,min(1_4,iGetDigit(ir,2_4))))
          iFunMarkQ=Int2(max(0_4,min(1_4,iGetDigit(ir,3_4))))
          iFunMarkSize=Int2(max(0_4,min(20_4,nx)))
          iFunMarkStep=Int2(max(1_4,min(1000_4,ny)))
          iFunMarkColor=Int2(max(-1_4,min(235_4,nz)))
        end if
      else if(Argument(1:3).eq.'sca') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) x=0.0d0
        if(lPerform) then
          rFunSca=x
          if(abs(rFunSca).lt.1.0d-300) then
            call MinMax(Fun,mFun,mFunA,nFun,iFunA2,x,y)
            rFunSca=1.0d0.div.y
          end if
        end if
      else if(Argument(1:3).eq.'sen') then
        call ExtractIV(Mov_Command,4,nx1,iErr2,nPFDsens,nx2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if((iErr2.ne.0).or.(ir.lt.1)) return
        call ExtractStr(Mov_Command,6,str,idum)
        if(idum.lt.3) return
        call ExtractIV(Mov_Command,7,ny,iErr2)
        if(iErr2.ne.0) ny=0
        if((ny.lt.1).and.(nPFDf.lt.2)) return
        if(lPerform) then
          if(ny.lt.1) then
            call IncreaseFun(nPFDf-mFun,0,ldum)
            nFun=nPFDf
          else if(ny.gt.mFun) then
            call IncreaseFun(ny-mFun,0,ldum)
            nFun=mFun
          end if
          if(mFunA.lt.ir) then
            call IncreaseFun(0,ir-mFunA,ldum)
          else if(ir.lt.1) then
            call IncreaseFun(0,1,ldum)
            ir=mFunA
          end if
          nFunA=mFunA
          if(ny.lt.1) then
            Fun(ir,1:nPFDf)=0.0d0
          else
            Fun(ir,ny)=0.0d0
          end if
          do nx=nx1,nx2
            Select Case(str(1:3))
            Case('der')
              lx=lfcFldAll
              ly=lfcFld
              lz=lSingleFldPoint
              lfcFldAll=.true.
              lfcFld=.true.
              lSingleFldPoint=.true.
              if(ny.lt.1) then
                do i=1,nPFDf
                  cSingleFldPoint(1:6)=cPFDsens(1:6,i,nx)
                  call GetrField(.false.) ! compute field to be printed
                  Fun(ir,i)=Fun(ir,i)+r3Vec_Length(fSingleFldPoint(1:3))
                end do
              else
                cSingleFldPoint(1:6)=cPFDsens(1:6,1,nx)
                call GetrField(.false.) ! compute field to be printed
                Fun(ir,ny)=Fun(ir,ny)+r3Vec_Length(fSingleFldPoint(1:3))
              end if
              lfcFldAll=lx
              lfcFld=ly
              lSingleFldPoint=lz
              FunATitle(ir)="derived field"C
            Case('exa')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+cdAbs(cPFDsens(1,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+cdAbs(cPFDsens(1,1,nx))
              end if
              FunATitle(ir)="Abs(Ex)"C
            Case('exi')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dimag(cPFDsens(1,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+DImag(cPFDsens(1,1,nx))
              end if
              FunATitle(ir)="Imag(Ex)"C
            Case('exp')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+180.0d0*datan2(Dimag(cPFDsens(1,1:nPFDf,nx)), &
                & Dble(cPFDsens(1,1:nPFDf,nx)))/Pi
              else
                Fun(ir,ny)=Fun(ir,ny)+180.0d0*datan2(Dimag(cPFDsens(1,1,nx)),Dble(cPFDsens(1,1,nx)))/Pi
              end if
              FunATitle(ir)="Phi(Ex)"C
            Case('exr')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dble(cPFDsens(1,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(cPFDsens(1,1,nx))
              end if
              FunATitle(ir)="Real(Ex)"C
            Case('eya')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+cdAbs(cPFDsens(2,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+cdAbs(cPFDsens(2,1,nx))
              end if
              FunATitle(ir)="Abs(Ey)"C
            Case('eyi')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+DImag(cPFDsens(2,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+DImag(cPFDsens(2,1,nx))
              end if
              FunATitle(ir)="Imag(Ey)"C
            Case('eyp')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+180.0d0*datan2(Dimag(cPFDsens(2,1:nPFDf,nx)), &
                & Dble(cPFDsens(2,1:nPFDf,nx)))/Pi
              else
                Fun(ir,ny)=Fun(ir,ny)+180.0d0*datan2(Dimag(cPFDsens(2,1,nx)),Dble(cPFDsens(2,1,nx)))/Pi
              end if
              FunATitle(ir)="Phi(Ey)"C
            Case('eyr')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dble(cPFDsens(2,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(cPFDsens(2,1,nx))
              end if
              FunATitle(ir)="Real(Ey)"C
            Case('eza')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+cdAbs(cPFDsens(3,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+cdAbs(cPFDsens(3,1,nx))
              end if
              FunATitle(ir)="Abs(Ez)"C
            Case('ezi')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+DImag(cPFDsens(3,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+DImag(cPFDsens(3,1,nx))
              end if
              FunATitle(ir)="Imag(Ez)"C
            Case('ezp')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+180.0d0*datan2(Dimag(cPFDsens(3,1:nPFDf,nx)), &
                & Dble(cPFDsens(3,1:nPFDf,nx)))/Pi
              else
                Fun(ir,ny)=Fun(ir,ny)+180.0d0*datan2(Dimag(cPFDsens(3,1,nx)),Dble(cPFDsens(3,1,nx)))/Pi
              end if
              FunATitle(ir)="Phi(Ez)"C
            Case('ezr')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dble(cPFDsens(3,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(cPFDsens(3,1,nx))
              end if
              FunATitle(ir)="Real(Ez)"C
            Case('fre')
              if(ny.lt.1) then
                f=PFDfmin
                df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
                do i=1,nPFDf
                  Fun(ir,i)=Fun(ir,i)+f
                  f=f+df
                end do
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(fcFld)
              end if
              FunATitle(ir)="frequency"C
            Case('hxa')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+cdAbs(cPFDsens(4,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+cdAbs(cPFDsens(4,1,nx))
              end if
              FunATitle(ir)="Abs(Hx)"C
            Case('hxi')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+DImag(cPFDsens(4,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+DImag(cPFDsens(4,1,nx))
              end if
              FunATitle(ir)="Imag(Hx)"C
            Case('hxp')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+180.0d0*datan2(Dimag(cPFDsens(4,1:nPFDf,nx)), &
                & Dble(cPFDsens(4,1:nPFDf,nx)))/Pi
              else
                Fun(ir,ny)=Fun(ir,ny)+180.0d0*datan2(Dimag(cPFDsens(4,1,nx)),Dble(cPFDsens(4,1,nx)))/Pi
              end if
              FunATitle(ir)="Phi(Hx)"C
            Case('hxr')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dble(cPFDsens(4,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(cPFDsens(4,1,nx))
              end if
              FunATitle(ir)="Real(Hx)"C
            Case('hya')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+cdAbs(cPFDsens(5,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+cdAbs(cPFDsens(5,1,nx))
              end if
              FunATitle(ir)="Abs(Hy)"C
            Case('hyi')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+DImag(cPFDsens(5,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+DImag(cPFDsens(5,1,nx))
              end if
              FunATitle(ir)="Imag(Hy)"C
            Case('hyp')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+180.0d0*datan2(Dimag(cPFDsens(5,1:nPFDf,nx)), &
                & Dble(cPFDsens(5,1:nPFDf,nx)))/Pi
              else
                Fun(ir,ny)=Fun(ir,ny)+180.0d0*datan2(Dimag(cPFDsens(5,1,nx)),Dble(cPFDsens(5,1,nx)))/Pi
              end if
              FunATitle(ir)="Phi(Hy)"C
            Case('hyr')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dble(cPFDsens(5,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(cPFDsens(5,1,nx))
              end if
              FunATitle(ir)="Real(Hy)"C
            Case('hza')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+cdAbs(cPFDsens(6,1:nPFDf,nx))
              else
                Fun(ir,ny)=cdAbs(cPFDsens(6,1,nx))
              end if
              FunATitle(ir)="Abs(Hz)"C
            Case('hzi')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+DImag(cPFDsens(6,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Fun(ir,ny)+DImag(cPFDsens(6,1,nx))
              end if
              FunATitle(ir)="Imag(Hz)"C
            Case('hzp')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+180.0d0*datan2(Dimag(cPFDsens(6,1:nPFDf,nx)), &
                & Dble(cPFDsens(6,1:nPFDf,nx)))/Pi
              else
                Fun(ir,ny)=Fun(ir,ny)+180.0d0*datan2(Dimag(cPFDsens(6,1,nx)),Dble(cPFDsens(6,1,nx)))/Pi
              end if
              FunATitle(ir)="Phi(Hz)"C
            Case('hzr')
              if(ny.lt.1) then
                Fun(ir,1:nPFDf)=Fun(ir,1:nPFDf)+Dble(cPFDsens(6,1:nPFDf,nx))
              else
                Fun(ir,ny)=Fun(ir,ny)+Dble(cPFDsens(6,1,nx))
              end if
              FunATitle(ir)="Real(Hz)"C
            Case('ome')
              if(ny.lt.1) then
                f=PFDfmin
                df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
                f=2.0d0*Pi*f
                df=2.0d0*Pi*df
                do i=1,nPFDf
                  Fun(ir,i)=Fun(ir,i)+f
                  f=f+df
                end do
              else
                Fun(ir,ny)=Fun(ir,ny)+2.0d0*Pi*Dble(fcFld)
              end if
              FunATitle(ir)="omega"C
            Case('sxa')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=Fun(ir,n)+cdAbs(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=cdAbs(c)
                end do
              end if
              FunATitle(ir)="Abs(Sx)"C
            Case('sxi')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=Fun(ir,n)+dImag(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=dImag(c)
                end do
              end if
              FunATitle(ir)="Imag(Sx)"C
            Case('sxp')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=Fun(ir,n)+180.0d0*datan2(Dimag(c),Dble(c))/Pi
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=180.0d0*datan2(Dimag(c),Dble(c))/Pi
                end do
              end if
              FunATitle(ir)="Phi(Sx)"C
            Case('sxr')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=Fun(ir,n)+Dble(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(2,n,nx)*Conjg(cPFDsens(6,n,nx))-cPFDsens(3,n,nx)*Conjg(cPFDsens(5,n,nx)) ! Sx = Ey x Hz* - Ez x Hy*
                  Fun(ir,n)=Dble(c)
                end do
              end if
              FunATitle(ir)="Real(Sx)"C
            Case('sya')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=Fun(ir,n)+cdAbs(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=cdAbs(c)
                end do
              end if
              FunATitle(ir)="Abs(Sy)"C
            Case('syi')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=Fun(ir,n)+dImag(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=dImag(c)
                end do
              end if
              FunATitle(ir)="Imag(Sy)"C
            Case('syp')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=Fun(ir,n)+180.0d0*datan2(Dimag(c),Dble(c))/Pi
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=180.0d0*datan2(Dimag(c),Dble(c))/Pi
                end do
              end if
              FunATitle(ir)="Phi(Sy)"C
            Case('syr')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=Fun(ir,n)+Dble(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(3,n,nx)*Conjg(cPFDsens(4,n,nx))-cPFDsens(1,n,nx)*Conjg(cPFDsens(6,n,nx)) ! Sy = Ez x Hx* - Ex x Hz*
                  Fun(ir,n)=Dble(c)
                end do
              end if
              FunATitle(ir)="Real(Sy)"C
            Case('sza')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=Fun(ir,n)+cdAbs(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=cdAbs(c)
                end do
              end if
              FunATitle(ir)="Abs(Sz)"C
            Case('szi')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=Fun(ir,n)+dImag(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=dImag(c)
                end do
              end if
              FunATitle(ir)="Imag(Sz)"C
            Case('szp')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=Fun(ir,n)+180.0d0*datan2(Dimag(c),Dble(c))/Pi
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=180.0d0*datan2(Dimag(c),Dble(c))/Pi
                end do
              end if
              FunATitle(ir)="Phi(Sz)"C
            Case('szr')
              if(ny.lt.1) then
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=Fun(ir,n)+Dble(c)
                end do
              else
                do n=1,nPFDf
                  c=cPFDsens(1,n,nx)*Conjg(cPFDsens(5,n,nx))-cPFDsens(2,n,nx)*Conjg(cPFDsens(4,n,nx)) ! Sz = Ex x Hy* - Ey x Hx*
                  Fun(ir,n)=Dble(c)
                end do
              end if
              FunATitle(ir)="Real(Sz)"C
            Case('wav')
              if(ny.lt.1) then
                f=PFDfmin
                df=(PFDfmax-PFDfmin)/Dble(max(1_2,nPFDf-1_2))
                do i=1,nPFDf
                  Fun(ir,i)=Fun(ir,i)+1.0d0/(dsqrt(Mue0*Eps0)*f)
                  f=f+df
                end do
              else
                Fun(ir,ny)=Fun(ir,ny)+1.0d0/(dsqrt(Mue0*Eps0)*Dble(fcFld))
              end if
              FunATitle(ir)="wavelength"C
            end Select
          end do
        end if
      else if(Argument(1:3).eq.'sty') then
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,ny,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,7,nz,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          iFunStyleD=Int2(max(0_4,min(1_4,iGetDigit(ir,1_4))))
          iFunStyleL=Int2(max(0_4,min(1_4,iGetDigit(ir,2_4))))
          iFunStyleP=Int2(max(0_4,min(1_4,iGetDigit(ir,3_4))))
          iFunStyleWidth=Int2(max(1_4,min(10_4,nx)))
          iFunStyleStep=Int2(max(1_4,min(1000_4,ny)))
          iFunStyleColor=Int2(max(-1_4,min(235_4,nz)))
        end if
      else if(Argument(1:3).eq.'tim') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,ny,iErr2)
        if((iErr2.ne.0).or.(ny.lt.1)) return
        if(lPerform) then
          mFun=ny
          mFunA=2
          nFun=mFun
          nFunA=mFunA
          call AllocateFun(ldum)
          if(.not.ldum) return
          FunATitle(0)="n"C
          FunATitle(1)="t"C
          FunATitle(2)="f(t)"C
          z=(y-x)/Dble(max(1,ny-1))
          do nz=1,ny
            Fun(1,nz)=x+Dble(nz-1)*z
            Fun(2,nz)=timeDepPFD(Fun(1,nz),0)
          end do
        end if
      else if(Argument(1:3).eq.'tit') then
        call ExtractIV(Mov_Command,4,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractTxt(Mov_Command,1,str,idum)
        if(idum.le.0) return
        if(lPerform) then
          idu=Int2(nx)
          if(idu.lt.0) idu=nFunA-idu+1
          idu=max(0_2,min(nFunA,idu))
          idum=max(1_4,min(63_4,idum))
          FunATitle(idu)(1:idum)=str(1:idum)
          FunATitle(idu)(idum+1:idum+1)=char(0)
        end if
      else
        iErr=-9
        return
      end if
    Case('gam')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'var') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) ir=0
            ir=Max(0,Min(98,ir))
            if(lzPer) then
              dcFld=DCmplx(rMovVar(ir),rMovVar(ir+1))
              gcFld=0.5d0/(dcFld*fcFld*kw0)
            else
              gcFld=DCmplx(rMovVar(ir),rMovVar(ir+1))
              dcFld=0.5d0/(gcFld*fcFld*kw0)
            end if
          else
            call ExtractRV(Mov_Command,3,x,iErr2)
            if(iErr2.eq.0) then
              if(lzPer) then
                dcFld=DCmplx(x,0.0d0)
                gcFld=0.5d0/(dcFld*fcFld*kw0)
              else
                gcFld=DCmplx(x,0.0d0)
                dcFld=0.5d0/(gcFld*fcFld*kw0)
              end if
            end if
            call ExtractRV(Mov_Command,4,y,iErr2)
            if(iErr2.eq.0) then
              if(lzPer) then
                dcFld=DCmplx(x,y)
                gcFld=0.5d0/(dcFld*fcFld*kw0)
              else
                gcFld=DCmplx(x,y)
                dcFld=0.5d0/(gcFld*fcFld*kw0)
              end if
            end if
          end if
        else
          call ExtractRV(Mov_Command,3,x,iErr2)
          if(iErr2.eq.0) then
            if(lzPer) then
              dcFld=DCmplx(x,0.0d0)
              gcFld=0.5d0/(dcFld*fcFld*kw0)
            else
              gcFld=DCmplx(x,0.0d0)
              dcFld=0.5d0/(gcFld*fcFld*kw0)
            end if
          end if
          call ExtractRV(Mov_Command,4,y,iErr2)
          if(iErr2.eq.0) then
            if(lzPer) then
              dcFld=DCmplx(x,y)
              gcFld=0.5d0/(dcFld*fcFld*kw0)
            else
              gcFld=DCmplx(x,y)
              dcFld=0.5d0/(gcFld*fcFld*kw0)
            end if
          end if
        end if
      end if
    Case('gri')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('irr')
        if(lPerform) lrGrd=.false.
      Case('lin')
        call ExtractIV(Mov_Command,4,nx,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,5,ny,iErr2)
        if(iErr2.ne.0) return
        call ExtractIV(Mov_Command,6,nz,iErr2)
        if(iErr2.ne.0) nz=1
        if(lPerform) then
          nxcFld=nx
          nycFld=ny
          nzcFld=nz
        end if
      Case('reg')
        if(lPerform) lrGrd=.true.
      Case('spa')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.1) return
        Select Case(str(1:1))
        Case('o')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) z=0.0d0
          if(lPerform) then
            spacecFld(1,0)=x
            spacecFld(2,0)=y
            spacecFld(3,0)=z
          end if
        Case('x')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) z=0.0d0
          if(lPerform) then
            spacecFld(1,1)=x
            spacecFld(2,1)=y
            spacecFld(3,1)=z
          end if
        Case('y')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) z=0.0d0
          if(lPerform) then
            spacecFld(1,2)=x
            spacecFld(2,2)=y
            spacecFld(3,2)=z
          end if
        Case('z')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) z=0.0d0
          if(lPerform) then
            spacecFld(1,3)=x
            spacecFld(2,3)=y
            spacecFld(3,3)=z
          end if
        Case('n')
          call Ortho3DSpace(spacecFld)
        Case Default
          iErr=-9
          return
        end Select
      Case Default
        iErr=-9
        return
      end Select
      if(lPerform) then
        call AllocateIFld(ldum)
        if(.not.ldum) return
        call AllocateGrd(ldum)
        if(.not.ldum) return
        call AllocateFld(ldum)
        if(.not.ldum) return
        if(.not.lgcFld) lGet3DMat=.true.
        call ClearGrid(.true.)
        call ClearDomain(.true.)
        call ClearField(.true.)
      end if
    Case('int')
      if(Argument(1:3).eq.'bou') then
        call ExtractIV(Mov_Command,4,ir,iErr0,nBnd)
        if(iErr0.ne.0) return
        call ExtractIV(Mov_Command,5,nx,iErr1)
        call ExtractIV(Mov_Command,6,ny,iErr2)
        call ExtractIV(Mov_Command,7,nz,iErr3)
        call ExtractIV(Mov_Command,8,nt,iErr4)
        call ExtractRV(Mov_Command,9,r, iErr5)
        call ExtractIV(Mov_Command,10,m,iErr6)
        if(lPerform) then
          if(iErr0.eq.0) iBndInt=ir
          if(iErr1.eq.0) IntWhat=nx
          if(iErr2.eq.0) IntInter=ny
          lBndIntgX=.false.
          if(IntInter.gt.9) then
            IntInter=IntInter-10
            lBndIntgX=.true.
          end if
          if(iErr3.eq.0) IntField=nz
          if(iErr4.eq.0) MaxIntIter=nt
          if(iErr5.eq.0) AccIntegral=r
          if(iErr6.eq.0) IntType=m
        end if
      else if(Argument(1:3).eq.'obj') then
        call ExtractIV(Mov_Command,4,ir,iErr0,nObj)
        if(iErr0.ne.0) return
        if(lPerform) iObjInt=min(nObj,max(1,ir))
      else if(Argument(1:3).eq.'ori') then
        call ExtractRV(Mov_Command,5,x,iErr4)
        call ExtractRV(Mov_Command,6,y,iErr5)
        call ExtractRV(Mov_Command,7,z,iErr6)
        if(lPerform) then
          if(iErr4.eq.0) spaceInt(1,0)=x
          if(iErr5.eq.0) spaceInt(2,0)=y
          if(iErr6.eq.0) spaceInt(3,0)=z
        end if
      else if(Argument(1:3).eq.'pow') then
        call ExtractRV(Mov_Command,4,r,iErr3)
        if(lPerform) then
          if(iErr3.eq.0) PowerInt=r
        end if
      else if(Argument(1:3).eq.'rad') then
        call ExtractRV(Mov_Command,4,r,iErr3)
        if(lPerform) then
          if(iErr3.eq.0) rSphInt=r
        end if
      else if(Argument(1:3).eq.'rec') then
        call ExtractIV(Mov_Command,4,ir,iErr0)
        if(iErr0.ne.0) return
        call ExtractIV(Mov_Command,5,nx,iErr1)
        call ExtractIV(Mov_Command,6,ny,iErr2)
        call ExtractRV(Mov_Command,7,r, iErr3)
        call ExtractRV(Mov_Command,8,x, iErr4)
        call ExtractRV(Mov_Command,9,y, iErr5)
        call ExtractRV(Mov_Command,10,z,iErr6)
        if(lPerform) then
          if(iErr0.eq.0) IntInter=ir
          if(iErr1.eq.0) IntNx=nx
          if(iErr2.eq.0) IntNy=ny
          if(iErr3.eq.0) XminInt=r
          if(iErr4.eq.0) XmaxInt=x
          if(iErr5.eq.0) YminInt=y
          if(iErr6.eq.0) YmaxInt=z
        end if
      else if(Argument(1:3).eq.'sca') then
        call ExtractRV(Mov_Command,4,r,iErr3)
        if(lPerform) then
          if(iErr3.eq.0) FactorInt=r
        end if
      else if(Argument(1:3).eq.'sid') then
        call ExtractIV(Mov_Command,4,ir,iErr3)
        if(lPerform) then
          if(iErr3.eq.0) IntSide=ir
        end if
      else if(Argument(1:3).eq.'wha') then
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.1) then
          iErr=-1
          return
        end if
        if(idum.lt.3) then
          str(2:2)=str(1:1)
          str(3:3)=str(1:1)
        end if
        Select Case(str(1:3))
        Case('abs')
          lAbsInt=.true.
        Case('nab')
          lAbsInt=.false.
        Case('ave')
          IntField=-abs(IntField)
        Case('nav')
          IntField=abs(IntField)
        Case('cur')
          IntField=0
        Case('eee')
          if(IntField.gt.0) then
            IntField=1
          else
            IntField=-1
          end if
        Case('fff')
          if(IntField.gt.0) then
            IntField=4
          else
            IntField=-4
          end if
        Case('hhh')
          if(IntField.gt.0) then
            IntField=2
          else
            IntField=-2
          end if
        Case('sss')
          if(IntField.gt.0) then
            IntField=3
          else
            IntField=-3
          end if
        Case default
          iErr=-1
        end select
      else if(Argument(1:1).eq.'x') then
        call ExtractRV(Mov_Command,5,x,iErr4)
        call ExtractRV(Mov_Command,6,y,iErr5)
        call ExtractRV(Mov_Command,7,z,iErr6)
        if(lPerform) then
          if(iErr4.eq.0) spaceInt(1,1)=x
          if(iErr5.eq.0) spaceInt(2,1)=y
          if(iErr6.eq.0) spaceInt(3,1)=z
        end if
      else if(Argument(1:1).eq.'y') then
        call ExtractRV(Mov_Command,5,x,iErr4)
        call ExtractRV(Mov_Command,6,y,iErr5)
        call ExtractRV(Mov_Command,7,z,iErr6)
        if(lPerform) then
          if(iErr4.eq.0) spaceInt(1,2)=x
          if(iErr5.eq.0) spaceInt(2,2)=y
          if(iErr6.eq.0) spaceInt(3,2)=z
        end if
      else if(Argument(1:1).eq.'z') then
        call ExtractRV(Mov_Command,5,x,iErr4)
        call ExtractRV(Mov_Command,6,y,iErr5)
        call ExtractRV(Mov_Command,7,z,iErr6)
        if(lPerform) then
          if(iErr4.eq.0) spaceInt(1,3)=x
          if(iErr5.eq.0) spaceInt(2,3)=y
          if(iErr6.eq.0) spaceInt(3,3)=z
        end if
      end if
    Case('lev')
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) ir=1
      if(lPerform) then
        if(iPlane.eq.1) levPlane=Max(1,Min(ir,nxcFld))
        if(iPlane.eq.2) levPlane=Max(1,Min(ir,nycFld))
        if(iPlane.eq.3) levPlane=Max(1,Min(ir,nzcFld))
      end if
    Case('max')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'var') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) ir=0
            ir=Max(0,Min(999,ir))
            rmaxFld=rMovVar(ir)
          else if(Argument(1:3).eq.'max') then
            rmaxFld=max(abs(rminFld),abs(rmaxFld))
            rminFld=-rmaxFld
          else if(Argument(1:3).eq.'min') then
            rmaxFld=-rminFld
          else
            call ExtractRV(Mov_Command,3,x,iErr2)
            if(iErr2.eq.0) rmaxFld=x
          end if
        else
          call ExtractRV(Mov_Command,3,x,iErr2)
          if(iErr2.eq.0) rmaxFld=x
        end if
      end if
    Case('min')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'var') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) ir=0
            ir=Max(0,Min(999,ir))
            rminFld=rMovVar(ir)
          else if(Argument(1:3).eq.'div') then
            call ExtractRV(Mov_Command,4,x,iErr2)
            if(iErr2.eq.0) rminFld=rmaxFld/x
          else if(Argument(1:3).eq.'max') then
            rminFld=-rmaxFld
          else
            call ExtractRV(Mov_Command,3,x,iErr2)
            if(iErr2.eq.0) rminFld=x
          end if
        else
          call ExtractRV(Mov_Command,3,x,iErr2)
          if(iErr2.eq.0) rminFld=x
        end if
      end if
    Case('mbp')
      if(lArgument.gt.2) then
        Select Case(Argument(1:3))
        Case('cal')
          call ExtractIV(Mov_Command,4,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) MBPEmaxncalc=ir
        Case('err')
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) MBPEmaxerror=abs(r)
        Case('lim')
          call ExtractRV(Mov_Command,4,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,5,y,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            MBPEftop=x
            MBPEfbottom=y
          end if
        Case('ord')
          call ExtractIV(Mov_Command,4,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) MBPEmaxorder=ir
        Case('out')
          call ExtractIV(Mov_Command,4,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) MBPEioutput=ir
        Case('ove')
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) MBPEfover=r
        Case('ran')
          call ExtractRV(Mov_Command,4,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,5,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractIV(Mov_Command,6,n,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,u,iErr2)
          if(iErr2.ne.0) u=x
          call ExtractRV(Mov_Command,8,v,iErr2)
          if(iErr2.ne.0) v=y
          call ExtractIV(Mov_Command,9,m,iErr2)
          if(iErr2.ne.0) m=n
          call ExtractIV(Mov_Command,10,ir,iErr2)
          if(iErr2.ne.0) ir=1
          if(lPerform) then
            MBPExstart=x
            MBPExend=y
            MBPEndiv=n
            MBPExstartOut=u
            MBPExendOut=v
            MBPEntest=m
            MBPEnparam=max(1,min(ir,49))
          end if
        Case('var')
          call ExtractIV(Mov_Command,4,ir,iErr2)
          if(iErr2.ne.0) ir=996
          call ExtractIV(Mov_Command,5,n,iErr2)
          if(iErr2.ne.0) n=997
          call ExtractIV(Mov_Command,6,m,iErr2)
          if(iErr2.ne.0) m=998
          call ExtractIV(Mov_Command,7,l,iErr2)
          if(iErr2.ne.0) l=999
          if(lPerform) then
            iMovVarIn=ir
            iMovVarOut1=n
            iMovVarOut2=m
            iMovVarErr=l
          end if
        case Default
          iErr=9
        end select
      end if
    Case('mmp')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'amp') then
            call ExtractStr(Mov_Command,4,str,idum)
            if(idum.lt.1) return
            Select Case(str(1:1))
            Case('1')
              iAmplTyp=0
            Case('b')
              call ExtractIV(Mov_Command,5,ir,iErr2)
              if(iErr2.ne.0) ir=0
              iAmpl=ir
              iAmplTyp=1
            Case('o')
              call ExtractIV(Mov_Command,5,ir,iErr2)
              if(iErr2.ne.0) ir=0
              iAmpl=ir
              iAmplTyp=3
            Case('p')
              call ExtractIV(Mov_Command,5,ir,iErr2)
              if(iErr2.ne.0) ir=0
              iAmpl=ir
              iAmplTyp=4
            Case('r')
              call ExtractIV(Mov_Command,5,ir,iErr2)
              if(iErr2.ne.0) ir=9
              iAmpl=ir
              iAmplTyp=2
            end Select
          else if(Argument(1:3).eq.'con') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) return
            iMMPCon=Int2(ir)
          else if(Argument(1:3).eq.'err') then
            call ExtractRV(Mov_Command,4,r,iErr2)
            if(iErr2.ne.0) return
            ErrorScale=r
          else if(Argument(1:3).eq.'las') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) return
            iMMPlast=ir
          else if(Argument(1:3).eq.'mat') then
            call ExtractRV(Mov_Command,4,r,iErr2)
            if(iErr2.ne.0) return
            BndDmax=r
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            nSegPt=ir
            call ExtractRV(Mov_Command,6,r,iErr2)
            if(iErr2.ne.0) return
            BndPpW=r
            call ExtractRV(Mov_Command,7,r,iErr2)
            if(iErr2.ne.0) return
            BndOver=r
            call ExtractRV(Mov_Command,8,r,iErr2)
            if(iErr2.ne.0) return
            smallMatDist=r
            call ExtractRV(Mov_Command,9,r,iErr2)
            if(iErr2.ne.0) return
            lSmoothMat=.true.
            if(r.lt.1.0d-30) lSmoothMat=.false.
          else if(Argument(1:3).eq.'sea') then
            call ExtractStr(Mov_Command,4,str,idum)
            lMMPfine=.false.
            lMMPrough=.false.
            lPET=.false.
            if(idum.lt.1) return
            Select Case(str(1:1))
            Case('f')
              lMMPfine=.true.
            Case('p')
              lPET=.true.
            Case('r')
              lMMPrough=.true.
            end Select
            if(idum.lt.2) return
            Select Case(str(2:2))
            Case('f')
              lMMPfine=.true.
            Case('p')
              lPET=.true.
            Case('r')
              lMMPrough=.true.
            end Select
            if(idum.lt.3) return
            Select Case(str(3:3))
            Case('f')
              lMMPfine=.true.
            Case('p')
              lPET=.true.
            Case('r')
              lMMPrough=.true.
            end Select
          else if(Argument(1:3).eq.'sol') then
            call ExtractStr(Mov_Command,4,str,idum)
            if(idum.lt.2) return
            Select Case(str(1:2))
            Case('cg')
              iMtrSolver=2
            Case('ch')
              iMtrSolver=4
            Case('gu')
              iMtrSolver=0
              if(idum.gt.2) then
                if(str(3:3).eq.'r') iMtrSolver=1
              end if
            Case('pe')
              iMtrSolver=3
              kPET=0
              lPET=.true.
            Case('qr')
              iMtrSolver=5
            end Select
            call ExtractIV(Mov_Command,5,ir,iErr2)
            if(iErr2.ne.0) return
            itmCG=ir
            call ExtractIV(Mov_Command,6,ir,iErr2)
            if(iErr2.ne.0) return
            nPET=ir
            call ExtractRV(Mov_Command,7,r,iErr2)
            if(iErr2.ne.0) return
            resCG=r
            call ExtractIV(Mov_Command,8,ir,iErr2)
            if(iErr2.ne.0) return
            iScaleMatrix=ir
            call ExtractIV(Mov_Command,9,ir,iErr2)
            if(iErr2.ne.0) return
            iaccCG=ir
            call ExtractRV(Mov_Command,10,r,iErr2)
            if(iErr2.ne.0) return
            resPET=r
            call ExtractRV(Mov_Command,11,r,iErr2)
            if(iErr2.ne.0) return
            fMatrix0=r
          else if(Argument(1:3).eq.'wor') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) iWorkSpace=65
            iWorkSpace=ir
          end if
        end if
      end if
    Case('per')
      if(Argument(1:1).eq.'c') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,r,iErr2)
        if(iErr2.ne.0) r=0.0d0
        call ExtractRV(Mov_Command,8,u,iErr2)
        if(iErr2.ne.0) u=0.0d0
        call ExtractRV(Mov_Command,9,v,iErr2)
        if(iErr2.ne.0) v=0.0d0
        if(lPerform) then
          CxPeriodDlg=DCmplx(x,y)
          CyPeriodDlg=DCmplx(z,r)
          CzPeriodDlg=DCmplx(u,v)
          call setupPeriod()
        end if
      elseif(Argument(1:1).eq.'d') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,7,r,iErr2)
        if(iErr2.ne.0) r=0.0d0
        call ExtractRV(Mov_Command,8,u,iErr2)
        if(iErr2.ne.0) u=0.0d0
        call ExtractRV(Mov_Command,9,v,iErr2)
        if(iErr2.ne.0) v=0.0d0
        if(lPerform) then
          xPeriod=x
          yPeriodVector(1)=y
          yPeriodVector(2)=z
          yPeriodVector(3)=0.0d0
          zPeriodVector(1)=r
          zPeriodVector(2)=u
          zPeriodVector(3)=v
        end if
      elseif(Argument(1:1).eq.'p') then
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=0.0d0
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        if(lPerform) then
          if(dabs(xPeriod).gt.pSmall) then
            CxPeriodDlg=Pi*DCmplx(x.div.xPeriod,0.0d0)/180.0d0
          else
            CxPeriodDlg=(0.0d0,0.0d0)
          end if
          if(r3Vec_Length(yPeriodVector).gt.pSmall) then
            CyPeriodDlg=Pi*DCmplx(y.div.r3Vec_Length(yPeriodVector),0.0d0)/180.0d0
          else
            CyPeriodDlg=(0.0d0,0.0d0)
          end if
          if(r3Vec_Length(zPeriodVector).gt.pSmall) then
            CzPeriodDlg=Pi*DCmplx(z.div.r3Vec_Length(zPeriodVector),0.0d0)/180.0d0
          else
            CzPeriodDlg=(0.0d0,0.0d0)
          end if
          call setupPeriod()
        end if
      elseif(Argument(1:1).eq.'n') then
        lxPeriod=.false.
        lyPeriod=.false.
        lzPeriod=.false.
        call setupPeriod()
      elseif(Argument(1:1).eq.'x') then
        lxPeriod=.true.
        lyPeriod=.false.
        lzPeriod=.false.
        call setupPeriod()
      elseif(Argument(1:1).eq.'y') then
        lxPeriod=.true.
        lyPeriod=.true.
        lzPeriod=.false.
        call setupPeriod()
      elseif(Argument(1:1).eq.'z') then
        lxPeriod=.true.
        lyPeriod=.true.
        lzPeriod=.true.
        call setupPeriod()
      else
        iErr=-1
      end if
    Case('phi')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'var') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) ir=0
            ir=Max(0,Min(999,ir))
            prFld=rMovVar(ir)
          else
            call ExtractRV(Mov_Command,3,x,iErr2)
            if(iErr2.eq.0) prFld=x
          end if
        else
          call ExtractRV(Mov_Command,3,x,iErr2)
          if(iErr2.eq.0) prFld=x
        end if
        if(Dabs(Dble(fcFld)).gt.pSmall) trFld=prFld.div.(360.0d0*Dble(fcFld))
      end if
    Case('pei')
      if(Argument(1:1).eq.'t') then
        lEigen=.true.
      else
        lEigen=.false.
      end if
	  Case('obj')
      if(lArgument.lt.1) return
      ir=1000000
      if(lArgument.gt.2) then
        Select Case(Argument(1:3))
        Case('num')
          call ExtractIV(Mov_Command,4,nt,iErr2,nObj)
          if(iErr2.ne.0) nt=0
          if(lPerform) iDraObj=max(-nObj,min(nObj,nt))
          ir=-1000000
        Case('che')
          if(lPerform) lObjHid=.false.
          ir=-1000000
        Case('der')
          if(lPerform) iObjDra=4
          ir=-1000000
        Case('dom')
          if(lPerform) iObjDra=2
          ir=-1000000
        Case('dra')
          call ExtractRV(Mov_Command,4,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,5,y,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            GRChmin=x
            GRChmax=y
          end if
          ir=-1000000
        Case('err')
          if(lPerform) iObjDra=3
          ir=-1000000
        Case('exp')
          if(lPerform) iObjDra=5
          ir=-1000000
        Case('fie')
          if(lPerform) lObjFlg=.true.
          ir=-1000000
        Case('gri')
          if(lPerform) lObjMat=.false.
          ir=-1000000
        Case('hid')
          if(lPerform) lObjHid=.true.
          ir=-1000000
        Case('mat')
          if(lPerform) lObjMat=.true.
          ir=-1000000
        Case('nof')
          if(lPerform) lObjFlg=.false.
          ir=-1000000
        Case('obj')
          if(lPerform) iObjDra=0
          ir=-1000000
        Case('tra')
          if(lPerform) iObjDra=1
          ir=-1000000
        Case default
          call StrToIV(Argument(1:lArgument),ir,iErr2)
          if(iErr2.ne.0) ir=1000000
        end Select
        lCHGLdoubleSide=.true.
        if((iObjDra.eq.3).or.(iObjDra.eq.4)) lCHGLdoubleSide=.false.
      else
        call StrToIV(Argument(1:lArgument),ir,iErr2)
        if(iErr2.ne.0) ir=1000000
      end if
      if(ir.ge.1000000) return
      if(ir.gt.-1000000) then
        if(lPerform) kObj=ir
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.gt.2) then
          Select Case(str(1:3))
          Case('axi')
            call ExtractRV(Mov_Command,5,spa(1,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,6,spa(2,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,7,spa(3,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,8,spa(1,1),iErr2)
            if(iErr2.ne.0) spa(1,1)=1.0d0
            call ExtractRV(Mov_Command,9,spa(2,1),iErr2)
            if(iErr2.ne.0) spa(2,1)=0.0d0
            call ExtractRV(Mov_Command,10,spa(3,1),iErr2)
            if(iErr2.ne.0) spa(3,1)=0.0d0
            if(lPerform) then
              tObj(kObj)%O(1:3)=spa(1:3,0)
              tObj(kObj)%e(1:3)=spa(1:3,1)
              call Unit3DV(tOBJ(kOBJ)%e)
            end if
          Case('col')
            call ExtractIV(Mov_Command,5,nx,iErr2)
            if(iErr2.ne.0) return
            call ExtractIV(Mov_Command,6,ny,iErr2)
            if(iErr2.ne.0) return
            call ExtractIV(Mov_Command,7,nz,iErr2)
            if(iErr2.ne.0) return
            if(lPerform) then
              tObj(kObj)%iCol=Int2(nx)
              tObj(kObj)%iColmin=Int2(ny)
              tObj(kObj)%iColmax=Int2(nz)
            end if
          Case('int')
            call ExtractIV(Mov_Command,5,nx,iErr2)
            if(iErr2.ne.0) return
            call ExtractIV(Mov_Command,6,ny,iErr2)
            if(iErr2.ne.0) ny=0
            call ExtractIV(Mov_Command,7,nz,iErr2)
            if(iErr2.ne.0) nz=0
            call ExtractIV(Mov_Command,8,nt,iErr2)
            if(iErr2.ne.0) nt=0
            call ExtractIV(Mov_Command,9,m,iErr2)
            if(iErr2.ne.0) m=0
            if(lPerform) then
              tObj(kObj)%iPar(1)=Int2(nx)
              tObj(kObj)%iPar(2)=Int2(ny)
              tObj(kObj)%iPar(3)=Int2(nz)
              tObj(kObj)%iPar(4)=Int2(nt)
              tObj(kObj)%iPar(5)=Int2(m)
            end if
          Case('loc')
            call ExtractRV(Mov_Command,5,spa(1,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,6,spa(2,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,7,spa(3,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,8,spa(1,1),iErr2)
            if(iErr2.ne.0) spa(1,1)=1.0d0
            call ExtractRV(Mov_Command,9,spa(2,1),iErr2)
            if(iErr2.ne.0) spa(2,1)=0.0d0
            call ExtractRV(Mov_Command,10,spa(3,1),iErr2)
            if(iErr2.ne.0) spa(3,1)=0.0d0
            call ExtractRV(Mov_Command,11,spa(1,2),iErr2)
            if(iErr2.ne.0) spa(1,2)=0.0d0
            call ExtractRV(Mov_Command,12,spa(2,2),iErr2)
            if(iErr2.ne.0) spa(2,2)=1.0d0
            call ExtractRV(Mov_Command,13,spa(3,2),iErr2)
            if(iErr2.ne.0) spa(3,2)=0.0d0
            if(lPerform) then
              spa(1:3,3)=0.0d0
              tOBJ(kOBJ)%Plane=spa
              call Ortho3DSpace(tOBJ(kOBJ)%Plane)
            end if
          Case('rea')
            call ExtractRV(Mov_Command,5,spa(1,0),iErr2)
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,6,spa(2,0),iErr2)
            if(iErr2.ne.0) spa(2,0)=0.0d0
            call ExtractRV(Mov_Command,7,spa(3,0),iErr2)
            if(iErr2.ne.0) spa(3,0)=0.0d0
            call ExtractRV(Mov_Command,8,spa(1,1),iErr2)
            if(iErr2.ne.0) spa(1,1)=0.0d0
            call ExtractRV(Mov_Command,9,spa(2,1),iErr2)
            if(iErr2.ne.0) spa(2,1)=0.0d0
            if(lPerform) then
              tObj(kObj)%Par(1)=spa(1,0)
              tObj(kObj)%Par(2)=spa(2,0)
              tObj(kObj)%Par(3)=spa(3,0)
              tObj(kObj)%Par(4)=spa(1,1)
              tObj(kObj)%Par(5)=spa(2,1)
            end if
          Case('res')
            call ExtractRV(Mov_Command,5,r,iErr2)
            if(iErr2.ne.0) return
            if(lPerform) then
              tObj(kObj)%GrfRes=r
            end if
          Case('sec')
            call ExtractRV(Mov_Command,5,spa(1,0),iErr2) ! center angle
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,6,spa(2,0),iErr2) ! sector angle
            if(iErr2.ne.0) return
            call ExtractRV(Mov_Command,7,spa(3,0),iErr2) ! center angle divisor
            if(iErr2.ne.0) spa(3,0)=1.0d0
            if(lPerform) then
              tObj(kObj)%Par(1)=spa(1,0)/spa(3,0)-0.5d0*spa(2,0)
              tObj(kObj)%Par(2)=spa(2,0)
            end if
          Case default
            return
          end Select
        end if
      end if
    Case('pla')
      call StrToIV(Argument(1:lArgument),ir,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) iPlane=Modulo(ir-1,3)+1
    Case('pfd')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('eff')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          nPFDiEff=ir
        end if
      Case('fre')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) y=x
        if(lPerform) then
          nPFDf=max(1,ir)
          nPFDsens=max(1,nPFDsens)
          PFDfmin=x
          PFDfmax=y
          DeAllocate(cPFDsens,stat=idum)
          Allocate(cPFDsens(0:6,nPFDf,nPFDsens),stat=idum)
          nPFDsFld=0
          cPFDsens=(0.0d0,0.0d0)
        end if
      Case('gri')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.1) return
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          Select Case(str(1:1))
          Case('x')
            nPFDi=ir
            PFDxmin=x
            PFDxmax=y            
            PFDdx=(PFDxmax-PFDxmin)/Dble(max(1,nPFDi-1))
          Case('y')
            nPFDj=ir
            PFDymin=x
            PFDymax=y
            PFDdy=(PFDymax-PFDymin)/Dble(max(1,nPFDj-1))
          Case('z')
            nPFDk=ir
            PFDzmin=x
            PFDzmax=y
            PFDdz=(PFDzmax-PFDzmin)/Dble(max(1,nPFDk-1))
          end select
        end if
      Case('pml')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.1) return
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.ne.0) ir=0
        call ExtractIV(Mov_Command,6,m,iErr2)
        if(iErr2.ne.0) m=ir
        if(lPerform) then
          Select Case(str(1:1))
          Case('d')
            call ExtractRV(Mov_Command,5,x,iErr2)
            if(iErr2.ne.0) x=-0.33d0
            PFDpml=x
          Case('x')
            nPFDil=ir
            nPFDih=m
          Case('y')
            nPFDjl=ir
            nPFDjh=m
          Case('z')
            nPFDkl=ir
            nPFDkh=m
          end select
        end if
      Case('sca')
        call ExtractIV(Mov_Command,4,ir,iErr2)
        if((iErr2.ne.0).or.(ir.lt.-2)) return
        if(lPerform) nPFDsLayers=ir
      Case('sen')
        call ExtractIV(Mov_Command,4,ir,iErr2,nPFDsens)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,z,iErr2)
        if(iErr2.ne.0) z=0.0d0
        call ExtractRV(Mov_Command,8,t,iErr2)
        if(iErr2.ne.0) t=0.0d0
        call ExtractRV(Mov_Command,9,d,iErr2)
        if(iErr2.ne.0) d=1.0d100
        if(lPerform) then
          kPFDsens=max(1,min(nPFDsens,ir))
          PFDsensX(kPFDsens)=x
          PFDsensY(kPFDsens)=y
          PFDsensZ(kPFDsens)=z
          PFDsensT(kPFDsens)=t
          PFDsensD(kPFDsens)=d
        end if
      Case('sou')
        call ExtractStr(Mov_Command,4,cStr,idum)
        if(idum.lt.3) return
        if(lPerform) then
          call getPFDl(iPFDt,lh,le,lx,ly,lz,lp,lm,ls,lha,lpw)
          Select case(cStr(1:3))
          case('amp') ! set amplitudes of point sources using MMP or cFormula
            lm=.false.
            lp=.true.
            lpw=.false.
            call ExtractIV(Mov_Command,5,n,iErr2,Int4(nPFDsource),m)
            if(iErr2.ne.0) return
            call ExtractStr(Mov_Command,6,str,idum)
            if(idum.lt.1) return
            l=GetSLength(str)
            do ir=n,m
              rl(1)=PFDxmin+Dble(iPFDs(ir)-1)*PFDdx
              rl(2)=PFDymin+Dble(jPFDs(ir)-1)*PFDdy
              rl(3)=PFDzmin+Dble(kPFDs(ir)-1)*PFDdz
              if(str(1:3).eq.'mmp') then
                call DistPtObj(0_2,0_2,rl(1:3),.true.,dmin,rmin,idu,val,.true.)
                call GetLoccField(rl,idu,0,cf)
                if(lgcFld) then
                  if(iHEGlobal.eq.1_2) then ! Hz
                    PFDsourceA(ir)=cf(6)
                  else ! Ez
                    PFDsourceA(ir)=cf(3)
                  end if
                else ! 3D
                  if(lE) then
                    if(lx) then
                      PFDsourceA(ir)=cf(1)
                    else if(ly) then
                      PFDsourceA(ir)=cf(2)
                    else
                      PFDsourceA(ir)=cf(3)
                    end if
                  else
                    if(lx) then
                      PFDsourceA(ir)=cf(4)
                    else if(ly) then
                      PFDsourceA(ir)=cf(5)
                    else
                      PFDsourceA(ir)=cf(6)
                    end if
                  end if
                end if
              else
                cCForm(0)=(0.0d0,1.0d0)
                cCForm(1)=DCmplx(Pi,0.0d0)
                cCForm(2)=DCmplx(Eps0,0.0d0)
                cCForm(3)=DCmplx(Mue0,0.0d0)
                cCForm(4)=DCmplx(Kw0,0.0d0)
                cCForm(5)=DCmplx(Zw0,0.0d0)
                pCForm(0)=(1.0d0,0.0d0)
                vCForm(0)=DCmplx(Dble(ir),0.0d0)
                vCForm(1)=DCmplx(rl(1),0.0d0)
                vCForm(2)=DCmplx(rl(2),0.0d0)
                vCForm(3)=DCmplx(rl(3),0.0d0)
                cFormu=cFormula(str,l,cCForm,pCForm,vCForm,5,0,3,1,1,1,iErr2)
                if(iErr2.eq.0) PFDsourceA(ir)=cFormu(1)
              end if
            end do
          case('eof')
            le=.false.
          case('eon')
            le=.true.
          case('har','tot')
            ls=.false.
          case('hof')
            lh=.false.
          case('hon')
            lh=.true.
          case('mmp')
            lm=.true.
            lp=.false.
            lpw=.false.
          case('pla')
            lm=.false.
            lp=.false.
            lpw=.true.
          case('poi') ! set locations of source points
            lm=.false.
            lp=.true.
            lpw=.false.
            call ExtractIV(Mov_Command,5,n,iErr2,Int4(nPFDsource),m)
            if(iErr2.ne.0) return
            do ir=n,m
              call ExtractIV(Mov_Command,6,nx,iErr2)
              if((iErr2.ne.0).or.(nx.lt.1).or.(nx.gt.Int4(nPFDi))) nx=iPFDs(ir)
              call ExtractIV(Mov_Command,7,ny,iErr2)
              if((iErr2.ne.0).or.(ny.lt.1).or.(ny.gt.Int4(nPFDj))) ny=jPFDs(ir)
              call ExtractIV(Mov_Command,8,nz,iErr2)
              if((iErr2.ne.0).or.(nz.lt.1).or.(nz.gt.Int4(nPFDk))) nz=kPFDs(ir)
              iPFDs(ir)=max(1,min(nPFDi,nx))
              jPFDs(ir)=max(1,min(nPFDi,ny))
              kPFDs(ir)=max(1,min(nPFDi,nz))
            end do
          case('sof','sca')
            ls=.true.
          case('xof')
            lx=.false.
          case('xon')
            lx=.true.
          case('yof')
            ly=.false.
          case('yon')
            ly=.true.
          case('zof')
            lz=.false.
          case('zon')
            lz=.true.
          end select
          call getPFDi(lh,le,lx,ly,lz,lp,lm,ls,lpw,iPFDt)
        end if
      Case('sto')
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) x=4.0d0
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) y=1.0d-8
        if(lPerform) then
          PFDdfact=x
          PFDwfact=y
        end if
      Case('tim')
        call ExtractStr(Mov_Command,4,cStr,idum)
        if(idum.lt.3) return
        if(lPerform) then
          le=.true.
          lp=.true.
          lh=.true.
          if(iPFDft.lt.0) lh=.false.
          if((abs(iPFDft).eq.1).or.(abs(iPFDft).eq.3)) le=.false.
          if((abs(iPFDft).eq.2).or.(abs(iPFDft).eq.3)) lp=.false.
          Select case(cStr(1:3))
          case('cos')
            le=.false.
            call ExtractRV(Mov_Command,5,x,iErr2)
            if(iErr2.ne.0) PFDfTmax=x
            call ExtractRV(Mov_Command,6,y,iErr2)
            if(iErr2.ne.0) PFDfTau=y
          case('exp','gau')
            le=.true.
            call ExtractRV(Mov_Command,5,x,iErr2)
            if(iErr2.ne.0) PFDfTmax=x
            call ExtractRV(Mov_Command,6,y,iErr2)
            if(iErr2.ne.0) PFDfTau=y
          case('pul')
            lp=.true.
            call ExtractRV(Mov_Command,5,x,iErr2)
            if(iErr2.ne.0) PFDfTmax=x
            call ExtractRV(Mov_Command,6,y,iErr2)
            if(iErr2.ne.0) PFDfTau=y
          case('ram')
            lp=.false.
            call ExtractRV(Mov_Command,5,x,iErr2)
            if(iErr2.ne.0) PFDfTmax=x
            call ExtractRV(Mov_Command,6,y,iErr2)
            if(iErr2.ne.0) PFDfTau=y
          end select
          iPFDft=0
          if(.not.le) iPFDft=iPFDft+1
          if(.not.lp) iPFDft=iPFDft+2
          if(iPFDft.lt.1) iPFDft=4
          if(.not.lh) iPFDft=-iPFDft
        end if
      Case default
        return
      end select
    Case('p2d')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('mir')
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,rl(1),iErr2)
        if(iErr2.ne.0) rl(1)=Particle2DMirror(2,1)
        call ExtractRV(Mov_Command,7,rl(2),iErr2)
        if(iErr2.ne.0) rl(2)=Particle2DMirror(2,2)
        if(lPerform) then
          Particle2DMirror(1,1)=x
          Particle2DMirror(1,2)=y
          call Unit2DV(rl(1:2))
          Particle2DMirror(2,1)=rl(1)
          Particle2DMirror(2,2)=rl(2)
          Particle2DMirror(3,1)=-rl(2)
          Particle2DMirror(3,2)=rl(1)
        end if
      Case('spe')
        call ExtractIV(Mov_Command,4,ir,iErr2,nParticle2D,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do i=ir,ir2
            y=r2Vec_Length(tParticle2D(i)%Velocity(1:2))
            z=x.div.y
            tParticle2D(i)%Velocity(1:2)=z*tParticle2D(i)%Velocity(1:2)
          end do
        end if
      Case('ste')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          ParticleStepLength=r
        end if
      Case('tim')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          if(r.gt.0.0d0) then
            ParticleTimeStep=r
          else
            x=0
            do i=1,nParticle2D
              y=r2Vec_Length(tParticle2D(i)%Velocity(1:2))
              if(x.lt.y) x=y
            end do
            if(abs(r).gt.pSmall) then
              ParticleTimeStep=-r.div.y ! -r is the maximum step length
            else
            x=0
            do i=1,nParticle2D
              z=tParticle2D(i)%r
              if(x.lt.z) x=z
            end do
              ParticleTimeStep=z.div.y ! maximum step length = radius of largest particle
            end if
          end if
          ParticleTimeStep=min(ParticleTimeStep,1.0d30)
          write(*,*) 'Particle time step set to ',ParticleTimeStep
        end if
      Case('vel')
        call ExtractIV(Mov_Command,4,ir,iErr2,nParticle2D,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do i=ir,ir2
            tParticle2D(i)%Velocity(1)=x
            tParticle2D(i)%Velocity(2)=y
          end do
        end if
      Case default
        return
      end select
    Case('p3d')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('mir')
        call ExtractRV(Mov_Command,4,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,z,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,rl(1),iErr2)
        if(iErr2.ne.0) rl(1)=Particle3DMirror(4,1)
        call ExtractRV(Mov_Command,8,rl(2),iErr2)
        if(iErr2.ne.0) rl(2)=Particle3DMirror(4,2)
        call ExtractRV(Mov_Command,9,rl(3),iErr2)
        if(iErr2.ne.0) rl(3)=Particle3DMirror(4,3)
        if(lPerform) then
          Particle3DMirror(1,1)=x
          Particle3DMirror(1,2)=y
          Particle3DMirror(1,3)=z
          Particle2DMirror(4,1:3)=rl(1:3)
          Particle2DMirror(2,1)=CHRnd(-1.0d0,1.0d0,0_4)
          Particle2DMirror(2,2)=CHRnd(-1.0d0,1.0d0)
          Particle2DMirror(2,3)=CHRnd(-1.0d0,1.0d0)
          Particle2DMirror(3,1)=CHRnd(-1.0d0,1.0d0)
          Particle2DMirror(3,2)=CHRnd(-1.0d0,1.0d0)
          Particle2DMirror(3,3)=CHRnd(-1.0d0,1.0d0)
          call Ortho3DSpace3(Particle2DMirror(4,1:3),Particle2DMirror(2,1:3),Particle2DMirror(3,1:3))
        end if
      Case('spe')
        call ExtractIV(Mov_Command,4,ir,iErr2,nParticle3D,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do i=ir,ir2
            y=r3Vec_Length(tParticle2D(i)%Velocity(1:3))
            z=x.div.y
            tParticle2D(i)%Velocity(1:3)=z*tParticle2D(i)%Velocity(1:3)
          end do
        end if
      Case('ste')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          ParticleStepLength=r
        end if
      Case('tim')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          if(r.gt.0.0d0) then
            ParticleTimeStep=r
          else
            x=0
            do i=1,nParticle3D
              y=r3Vec_Length(tParticle3D(i)%Velocity(1:3))
              if(x.lt.y) x=y
            end do
            if(abs(r).gt.pSmall) then
              ParticleTimeStep=-r.div.y ! -r is the maximum step length
            else
            x=0
            do i=1,nParticle3D
              z=tParticle3D(i)%r
              if(x.lt.z) x=z
            end do
              ParticleTimeStep=z.div.y ! maximum step length = radius of largest particle
            end if
          end if
          ParticleTimeStep=min(ParticleTimeStep,1.0d30)
          write(*,*) 'Particle time step set to ',ParticleTimeStep
        end if
      Case('vel')
        call ExtractIV(Mov_Command,4,ir,iErr2,nParticle3D,ir2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,5,x,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,6,y,iErr2)
        if(iErr2.ne.0) return
        call ExtractRV(Mov_Command,7,z,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) then
          do i=ir,ir2
            tParticle2D(i)%Velocity(1)=x
            tParticle2D(i)%Velocity(2)=y
            tParticle2D(i)%Velocity(3)=z
          end do
        end if
      Case default
        return
      end select
    Case('rep')
      if(lArgument.lt.3) return
      Select Case(Argument(1:3))
      Case('arr')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.2) return
        Select Case(str(1:3))
        Case('fil')
          call ExtractStr(Mov_Command,5,str,idum)
          if(idum.lt.1) return
          if(lPerform) then
            lArrowFill=.true.
            if(str(1:1).eq.'f') lArrowFill=.false.
          end if
        Case('len')
          call ExtractRV(Mov_Command,5,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) ArrowLength=r
        Case('max')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) maxCArrow=ir
        Case('min')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) minCArrow=ir
        Case('sca')
          call ExtractRV(Mov_Command,5,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) scaleArrow=r
        Case('ste')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) nsFld=ir
        Case('typ')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) itArrow=ir
        Case Default
          iErr=-9
          return
        end Select
      Case('iso','int')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.3) return
        Select Case(str(1:3))
        Case('3-d')
          call ExtractRV(Mov_Command,5,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) Grid3D=r
        Case('fil')
          call ExtractStr(Mov_Command,5,str,idum)
          if(idum.lt.1) return
          if(lPerform) then
            lIsoFill=.true.
            if(str(1:1).eq.'f') lIsoFill=.false.
          end if
        Case('gri')
          call ExtractStr(Mov_Command,5,str,idum)
          if(idum.lt.1) return
          if(lPerform) then
            lGrid3D=.true.
            if(str(1:1).eq.'f') lGrid3D=.false.
          end if
        Case('iso')
          call ExtractStr(Mov_Command,5,str,idum)
          if(idum.lt.1) return
          if(lPerform) then
            lIsoLine=.true.
            if(str(1:1).eq.'f') lIsoLine=.false.
          end if
        Case('max')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) maxCIntensity=ir
        Case('min')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) minCIntensity=ir
        Case('sca')
          call ExtractRV(Mov_Command,5,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) scaleIntensity=r
        Case('ste')
          call ExtractRV(Mov_Command,5,r,iErr2)
          if(iErr2.ne.0) then
            call ExtractStr(Mov_Command,5,str,idum)
            if(idum.lt.3) return
            if(str(1:3).ne.'div') return
            call ExtractRV(Mov_Command,6,r,iErr2)
            if(iErr2.ne.0) return
            if(lPerform) rIsoStep=max(pSmall,dabs(rmaxFld-rminFld)/dabs(r))
          else
            if(lPerform) rIsoStep=r
          end if
        Case('typ')
          call ExtractIV(Mov_Command,5,ir,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) itIntensity=ir
        Case Default
          iErr=-9
          return
        end Select
      Case('pla')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.2) return
        if(str(1:2).eq.'xy') then
          if(lPerform) iPlane=3
        else if(str(1:2).eq.'xz') then
          if(lPerform) iPlane=2
        else if(str(1:2).eq.'yz') then
          if(lPerform) iPlane=1
        else
          return
        end if
        call ExtractIV(Mov_Command,5,ir,iErr2)
        if(iErr2.ne.0) ir=1
        if(lPerform) then
          if(iPlane.eq.1) levPlane=Modulo(ir-1,nxcFld)+1
          if(iPlane.eq.2) levPlane=Modulo(ir-1,nycFld)+1
          if(iPlane.eq.3) levPlane=Modulo(ir-1,nzcFld)+1
        end if
      Case('pow')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) PowerFld=r
      Case('sca')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) FactorFld=r
      Case('sci')
        call ExtractRV(Mov_Command,4,r,iErr2)
        if(iErr2.ne.0) return
        if(lPerform) FactorFld=.inv.r
      Case('vie')
        call ExtractStr(Mov_Command,4,str,idum)
        if(idum.lt.1) return
        Select Case(str(1:1))
        Case('d')
          call ExtractRV(Mov_Command,5,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) viewDist=r
        Case('o')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,z,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            viewPlane(1,0)=x
            viewPlane(2,0)=y
            viewPlane(3,0)=z
          end if
        Case('x')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            viewPlane(1,1)=x
            viewPlane(2,1)=y
            viewPlane(3,1)=z
          end if
        Case('y')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            viewPlane(1,2)=x
            viewPlane(2,2)=y
            viewPlane(3,2)=z
          end if
        Case('z')
          call ExtractRV(Mov_Command,5,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,6,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,z,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            viewPlane(1,3)=x
            viewPlane(2,3)=y
            viewPlane(3,3)=z
          end if
        Case('n')
          call Ortho3DSpace(viewPlane)
        Case Default
          iErr=-9
          return
        end Select
      Case Default
        iErr=-9
        return
      end Select
    Case('rhs')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      nRHS=max(1,min(ir,nPar-1))
      call AllocatePar(ldum)
    Case('sym')
      call ExtractRV(Mov_Command,3,x,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,y,iErr2)
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,z,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        ixySymm=max(0,min(2,nint(x,4)))
        ixzSymm=max(0,min(2,nint(y,4)))
        iyzSymm=max(0,min(2,nint(z,4)))
      end if
    Case('tim')
      if(lPerform) then
        if(lArgument.gt.2) then
          if(Argument(1:3).eq.'ste') then
            call ExtractRV(Mov_Command,4,r,iErr2)
            if(iErr2.ne.0) r=-1.1d0
            if(r.lt.0.0d0) then ! set the time step using stability criterion for PFD
              x=Dble(max(1,nPFDi-1))/Dabs(PFDxmax-PFDxmin) ! x=1/dx !
              ir=1
              if(nPFDj.gt.1) then
                y=Dble(nPFDj-1)/Dabs(PFDymax-PFDymin)
                ir=ir+1
              else
                y=0.0d0
              end if
              if(nPFDk.gt.1) then
                z=Dble(nPFDk-1)/Dabs(PFDzmax-PFDzmin)
                ir=ir+1
              else
                z=0.0d0
              end if
              dtrFld=1.0d0/(-r*dsqrt(x**2+y**2+z**2)*3.0d8)
            else
              dtrFld=r
            end if
          else if(Argument(1:3).eq.'var') then
            call ExtractIV(Mov_Command,4,ir,iErr2)
            if(iErr2.ne.0) ir=0
            ir=Max(0,Min(999,ir))
            trFld=rMovVar(ir)
          else
            call ExtractRV(Mov_Command,3,x,iErr2)
            if(iErr2.eq.0) trFld=x
          end if
        else
          call ExtractRV(Mov_Command,3,x,iErr2)
          if(iErr2.eq.0) trFld=x
        end if
        prFld=trFld*(360.0d0*Dble(fcFld))
      end if
    Case('vaa')
      call ExtractIV(Mov_Command,3,ir,iErr2)
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,4,ir2,iErr2)
      if(iErr2.ne.0) return
      if(lPerform) then
        n=4
        do i=ir,ir2
          n=n+1
          call ExtractRV(Mov_Command,n,r,iErr2)
          if(iErr2.eq.0) rMovVar(i)=r
        end do
      end if
    Case('var')
      call ExtractStr(Mov_Command,3,str,idum)
      if(str(1:1).eq.'v') then ! syntax: set var vn ..., where n is the integer number of the movie variable
        read(str(2:idum),*,iostat=iErr2) ir
        if(iErr2.ne.0) return
        call ExtractStr(Mov_Command,4,str,idum)
        if((str(1:3).eq.'for')) then ! syntax: set var vn formula ...
          call ExtractStr(Mov_Command,5,str,idum)
          if(lPerform) then
            call StrToRV(str,r,iErr2)
            rMovVar(ir)=r
          end if
        else ! syntax: set var vn r
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.eq.0) then
            if(lPerform) rMovVar(ir)=r
          else
            if(idum.lt.2) return
            call ExtractStr(Mov_Command,4,str,idum)
            chr3(1:3)=str(1:3)
            call GetRch(chr3,4_4,m,iErr2)
            if((iErr2.ne.0).or.(m.ne.1)) return
            if(lPerform) rMovVar(ir)=rch(1)
          end if
        end if
      else
        call ExtractStr(Mov_Command,4,str,idum)
        iErr2=1
        if(idum.gt.0) then
          read(str(1:idum),*,iostat=iErr2) ir
          if((iErr2.eq.0).and.(ichar(str(1:1)).gt.64).and.(ichar(str(1:1)).lt.123)) iErr2=999 ! first character f or t would be OK
          if(iErr2.eq.0) then ! give new syntax better chance!
            if(ir.eq.0) then
              if(str(1:1).ne.'0') then
                iErr2=2
              end if
            else
              do i=1,idum
                if(str(i:i).eq.'.') then
                  iErr2=2
                  Exit
                end if
              end do
            end if
            if(iErr2.eq.0) then
              if((str(1:1).eq.'+').or.(str(1:1).eq.'-')) iErr2=2
              if(iErr2.eq.0) then
                call ExtractStr(Mov_Command,3,str,idum)
                if(idum.gt.1) then
                  if((str(1:1).eq.'v').or.(str(1:1).eq.'V')) iErr2=3
                end if
              end if
            end if
          end if
        end if
        if((iErr2.eq.0).or.(iErr2.eq.1)) then ! old syntax
          if(iErr2.eq.1) ir=0
          ir=Max(0,Min(999,ir))
          call ExtractRV(Mov_Command,3,r,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) rMovVar(ir)=r
        else ! new syntax
          if(iErr2.eq.3) then
            read(str(2:idum),*,iostat=iErr2) ir
            if((iErr2.eq.0).and.(ichar(str(2:2)).gt.64).and.(ichar(str(2:2)).lt.123)) iErr2=999 ! first character f or t would be OK
          else
            call ExtractIV(Mov_Command,3,ir,iErr2)
          end if
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,4,r,iErr2)
          if(iErr2.eq.0) then
            if(lPerform) rMovVar(ir)=r
          else
            if(idum.lt.3) return
            call ExtractStr(Mov_Command,4,str,idum)
            chr3(1:3)=str(1:3)
            call GetRch(chr3,4_4,m,iErr2)
            if((iErr2.ne.0).or.(m.ne.1)) return
            if(lPerform) rMovVar(ir)=rch(1)
          end if
        end if
      end if
    Case('wav')
      if(lPerform) then
        call ExtractRV(Mov_Command,3,x,iErr2)
        if(iErr2.eq.0) fcFld=DCmplx(1.0d0/(dsqrt(Mue0*Eps0)*x),0.0d0)
      end if
    Case('win')
      call ExtractIV(Mov_Command,3,ir,iErr2,Int4(nWin))
      if((iErr2.ne.0).or.(ir.lt.1).or.(ir.gt.nWin)) return
      if(lPerform) then
        kWin=Max(1,Min(nWin,ir))
        idum=FocusQQ(10+kWin)
      end if
      call ExtractStr(Mov_Command,4,str,idum)
      if(idum.lt.3) return
      if(str(1:3).eq.'lim') then
        call ExtractRV(Mov_Command,5,r,iErr2)
        if(iErr2.ne.0) then
          call ExtractStr(Mov_Command,5,str,idum)
          if(idum.lt.3) return
          if(lPerform) then
            Select Case(str(1:3))
            Case('aut')
              call ExtractIV(Mov_Command,6,m,iErr2)
              if(iErr2.ne.0) m=1_4
              call ExtractRV(Mov_Command,7,r,iErr2)
              if(iErr2.ne.0) r=2.0d0*pBig
              call ExtractRV(Mov_Command,8,x,iErr2)
              if(iErr2.ne.0) x=2.0d0*pBig
              call ExtractRV(Mov_Command,9,y,iErr2)
              if(iErr2.ne.0) y=2.0d0*pBig
              call ExtractRV(Mov_Command,10,z,iErr2)
              if(iErr2.ne.0) z=2.0d0*pBig
              if(.not.lWinFld(kWin)) then
                call GetLimits(iWinXLog(kWin),iWinYLog(kWin),WinXmin(kWin), &
                               WinYmin(kWin),WinXmax(kWin),WinYmax(kWin),Int2(m),iOK)
                if(r.lt.pBig) WinXmin(kWin)=r
                if(x.lt.pBig) WinXmax(kWin)=x
                if(y.lt.pBig) WinYmin(kWin)=y
                if(z.lt.pBig) WinYmax(kWin)=z
              else
                lLimits=.true.
                call DrawField(.false.)
                lLimits=.false.
              end if
            Case('fun')
              m=0
              call ExtractIV(Mov_Command,6,m,iErr2)
              if((iErr2.ne.0).or.(m.lt.1).or.(m.gt.nFunA)) return
              call ExtractStr(Mov_Command,7,str,idum)
              if(idum.lt.3) return
              call MinMax(Fun,mFun,mFunA,nFun,m,x,y)
              Select Case(str(1:3))
              Case('xma')
                if(x.lt.pBig) WinXmax(kWin)=y
              Case('xmi')
                if(x.lt.pBig) WinXmin(kWin)=x
              Case('xmm')
                if(x.lt.pBig) WinXmax(kWin)=y
                if(x.lt.pBig) WinXmin(kWin)=x
              Case('yma')
                if(x.lt.pBig) WinYmax(kWin)=y
              Case('ymi')
                if(x.lt.pBig) WinYmin(kWin)=x
              Case('ymm')
                if(x.lt.pBig) WinYmax(kWin)=y
                if(x.lt.pBig) WinYmin(kWin)=x
              end Select
            end Select
          end if
        else
          call ExtractRV(Mov_Command,6,x,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,7,y,iErr2)
          if(iErr2.ne.0) return
          call ExtractRV(Mov_Command,8,z,iErr2)
          if(iErr2.ne.0) return
          if(lPerform) then
            WinXmin(kWin)=r
            WinXmax(kWin)=x
            WinYmin(kWin)=y
            WinYmax(kWin)=z
          end if
        end if
      else if(str(1:3).eq.'tit') then
        if(lPerform) then
          call ExtractTxt(Mov_Command,1,WinTitle(kWin),idum)
          lWinInit=.true.
        end if
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovSet

  Subroutine MovSor(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,i,i1,i2,j,j0
    Logical lPerform,ldum
    Character(32) CommObj
    Select Case(CommObj(1:3))
    Case('bou')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'col') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nBnd
            if(Int4(tBnd(i)%iCol).lt.i1) i1=Int4(tBnd(i)%iCol)
            if(Int4(tBnd(i)%iCol).gt.i2) i2=Int4(tBnd(i)%iCol)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nBnd
              if(Int4(tBnd(j)%iCol).eq.i) then
                call copyBnd(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertBnd(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      else if(Argument(1:3).eq.'con') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nBnd
            if(Int4(tBnd(i)%iConn).lt.i1) i1=Int4(tBnd(i)%iConn)
            if(Int4(tBnd(i)%iConn).gt.i2) i2=Int4(tBnd(i)%iConn)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nBnd
              if(Int4(tBnd(j)%iConn).eq.i) then
                call copyBnd(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertBnd(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      else if(Argument(1:3).eq.'cor') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nBnd
            if(Int4(tBnd(i)%nEdge).lt.i1) i1=Int4(tBnd(i)%nEdge)
            if(Int4(tBnd(i)%nEdge).gt.i2) i2=Int4(tBnd(i)%nEdge)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nBnd
              if(Int4(tBnd(j)%nEdge).eq.i) then
                call copyBnd(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertBnd(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      else if(Argument(1:3).eq.'dom') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nBnd
            if(Int4(tBnd(i)%iLDom).lt.i1) i1=Int4(tBnd(i)%iLDom)
            if(Int4(tBnd(i)%iRDom).lt.i1) i1=Int4(tBnd(i)%iRDom)
            if(Int4(tBnd(i)%iLDom).gt.i2) i2=Int4(tBnd(i)%iLDom)
            if(Int4(tBnd(i)%iRDom).gt.i2) i2=Int4(tBnd(i)%iRDom)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nBnd
              if((Int4(tBnd(j)%iLDom).eq.i).or.(Int4(tBnd(j)%iRDom).eq.i)) then
                call copyBnd(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertBnd(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      end if
    Case('exp')
      if(lArgument.lt.3) return
      if(Argument(1:3).eq.'col') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nExp
            if(Int4(tExp(i)%iCol).lt.i1) i1=Int4(tExp(i)%iCol)
            if(Int4(tExp(i)%iCol).gt.i2) i2=Int4(tExp(i)%iCol)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nExp-1
              if(Int4(tExp(j)%iCol).eq.i) then
                call copyExp(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertExp(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      else if(Argument(1:3).eq.'con') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nExp
            if(Int4(tExp(i)%iConn).lt.i1) i1=Int4(tExp(i)%iConn)
            if(Int4(tExp(i)%iConn).gt.i2) i2=Int4(tExp(i)%iConn)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nExp-1
              if(Int4(tExp(j)%iConn).eq.i) then
                call copyExp(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertExp(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      else if(Argument(1:3).eq.'dom') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nExp
            if(Int4(tExp(i)%iDom).lt.i1) i1=Int4(tExp(i)%iDom)
            if(Int4(tExp(i)%iDom).gt.i2) i2=Int4(tExp(i)%iDom)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nExp-1
              if(Int4(tExp(j)%iDom).eq.i) then
                call copyExp(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertExp(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      else if(Argument(1:3).eq.'typ') then
        if(lPerform) then
          i1=1000000000
          i2=-i1
          do i=1,nExp
            if(Int4(tExp(i)%iTypE).lt.i1) i1=Int4(tExp(i)%iTypE)
            if(Int4(tExp(i)%iTypE).gt.i2) i2=Int4(tExp(i)%iTypE)
          end do
          j0=0
          do i=i1,i2
            do j=j0+1,nExp-1
              if(Int4(tExp(j)%iTypE).eq.i) then
                call copyExp(j,j0,0.0d0,0.0d0,-2_4,ldum)
                call insertExp(j+1,-1,ldum)
                j0=j0+1
              end if
            end do
          end do
        end if
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovSor

  Subroutine MovSub(CommObj,lPerform,iErr)
    Implicit none
    Integer(4) iErr,idum
    Logical lPerform
    Character(32) CommObj
    if(lArgument.lt.1) return
    Select Case(CommObj(1:3))
    Case('fie')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,FldFileName,'MAX','FLD')
        lDiffField=.true.
        lDiffRel=.false.
        lAskFld=.false.
        lSkipFld=.false.
        lSkipDerFld=.true.
        lSkipFldHead=.true.
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.ge.3) then
          if(sch(1:3).eq.'der') lSkipDerFld=.false.
          call ExtractStr(Mov_Command,5,sch,idum)
          if(idum.ge.3) then
            if(sch(1:3).eq.'rel') lDiffRel=.true.
          end if
        end if
        call OpenField(.true.)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovSub

  Subroutine MovWri(CommObj,lPerform,iErr)
    Implicit none
	  Include 'RESOURCE.FD'
    Complex(8) cf(10)
    Real(8) r,xPt,yPt,zPt,xtPt,ytPt,rf(3),v(3),val(2)
    Integer(4) idum,iErr,iErr0,iErr1,iErr2,iErr3,iErr4,iErr5,iErr6,ir,i,nx,ny,nz,nt,m,OpenAviFile,closeAVIfile,ios
    Integer(2) iDomL,iDomR
    Logical lPerform
    Character(32) CommObj
    Character(3) chr3
    Character sPosition
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    if(lArgument.lt.1) return
    if(Argument(1:1).eq.'?') then
      Argument(2:2)='w'
      lArgument=2
    end if
    Select Case(CommObj(1:3))
    Case('avi')
      if(lPerform) then
        lAVI=.true.
        if(Argument(1:1).eq.'!') then
          if(lAVIopen) then
            idum=closeAVIfile()
            lAVIopen=.false.
          end if
        else if(Argument(1:1).eq.'/') then
          if(.not.lAVIopen) then
            kAVIframes=0_4
            nAVIframes=1000000_4
            if(lAVI) then
              if(lOGLavi) then
                idum=openAVIfile(iAVIdlg,AviFileName(1:lAviFileName)//char(0), &
                & Int4(iCHGLWidthCurrent),Int4(iCHGLHeightCurrent),nAVIframes,nAVIfpsec)
              else
                call GetKWin(.false.)
                idum=SetActiveQQ(10+kWin)
                idum=FocusQQ(10+kWin)
                idum=openAVIfile(iAVIdlg,AviFileName(1:lAviFileName)//char(0), &
                &    Int4(iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)-1_2), &
                &    Int4(iWinHeight(kWin)+iWinBottom(kWin)+iWinTop(kWin)-1_2),nAVIframes,nAVIfpsec)
              end if
            end if
            if(idum.eq.0) lAVIopen=.true.
          end if
          if(lAVIopen) then
            if(lOGLavi) then
              lSaveOGLavi=.true.
            else
              call SaveBitmap(.true.)
            end if
          else
            if(lOGLavi) then
              lSaveOGLbmp=.true.
              call incName(BmpFileName,'MAX','BMP',1)
            end if
          end if
        else
        !  if(lAVIopen) idum=closeAVIfile()
          if(.not.lAVIopen) then
            call setNameS(Argument,lArgument,ProFileName,AviFileName,'MAX','AVI')
            lAviFileName=GetSLength(AviFileName)
            kAVIframes=0_4
            nAVIframes=1000000_4
            if(lAVI) then
              if(lOGLavi) then
                idum=openAVIfile(iAVIdlg,AviFileName(1:lAviFileName)//char(0), &
                & Int4(iCHGLWidthCurrent),Int4(iCHGLHeightCurrent),nAVIframes,nAVIfpsec)
              else
                call GetKWin(.false.)
                idum=SetActiveQQ(10+kWin)
                idum=FocusQQ(10+kWin)
                idum=openAVIfile(iAVIdlg,AviFileName(1:lAviFileName)//char(0), &
                &    Int4(iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)-1_2), &
                &    Int4(iWinHeight(kWin)+iWinBottom(kWin)+iWinTop(kWin)-1_2),Int4(nAVIframes),Int4(nAVIfpsec))
              end if
            end if
            if(idum.eq.0) lAVIopen=.true.
          end if
        end if
        lAVI=.false.
      end if
    Case('bas')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,PETFileName,'MAX','BAS')
        fPET=1.0d0.div.fPET
        call saveBasis(.true.)
        fPET=1.0d0
      end if
    Case('bit','bmp')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,BmpFileName,'MAX','BMP')
        lBmpFileName=GetSLength(BmpFileName)
        lAVI=.false.
        if(lOGLavi) then
          lSaveOGLbmp=.true.
        else
          call SaveBitmap(.true.)
        end if
      end if
    Case('bmf')
      if(lPerform) then
        BmpFileName=FunFileName
        lBmpFileName=GetSLength(BmpFileName)
        BmpFileName(lBmpFileName-2:lBmpFileName)='bmp'
        call setNameS(Argument,lArgument,FunFileName,BmpFileName,'MAX','BMP')
        lBmpFileName=GetSLength(BmpFileName)
        lAVI=.false.
        if(lOGLavi) then
          lSaveOGLbmp=.true.
        else
          call SaveBitmap(.true.)
        end if
      end if
    Case('bou')
      call ExtractIV(Mov_Command,3,m,iErr2)
      if(iErr2.eq.0) then ! Save matching point locations of boundary n on a file
        if((m.gt.0).and.(m.lt.nBnd)) then
          call ExtractStr(Mov_Command,4,sch,idum)
          if(idum.gt.0) then
            if(sch(1:1).eq.'?') then
              sch(2:2)='w'
              idum=2
            end if
            call setNameS(sch,idum,ProFileName,MatFileName,'MAX','MAT')
            open(1,file=MatFileName,iostat=ios)
            if(ios.eq.0) then
              write(1,*) tBnd(m)%nMat
              do i=tBnd(m)%iMatOffset+1,tBnd(m)%iMatOffset+tBnd(m)%nMat
                call GetBndPt(0,sBndPt(i),xPt,yPt,xtPt,ytPt,iDomL,iDomR,idum)
                write(1,*) xPt,yPt
              end do
            end if
            close(1,iostat=ios)
          end if
        end if
      end if
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,BndFileName,'MAX','BND')
        lAskBnd=.false.
        lInsertBnd=.false.
        call SaveBoundary(.true.)
      end if
    Case('cfi') ! complex field on n by m grid -> file
      call ExtractRV(Mov_Command,3,xPt,iErr2) ! x coordinate of first point
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,yPt,iErr2) ! y coordinate of first point
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,zPt,iErr2) ! z coordinate of first point
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,xtPt,iErr2) ! 1st length of rectangle
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,ytPt,iErr2) ! 2nd length of rectangle
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,8,nx,iErr2) ! number of points in 1st direction
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,9,ny,iErr2) ! number of points in 2nd direction
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,10,nt,iErr2) ! orientation of rectangle 1: yz, 2: xz, 3: xy plane
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,11,ir,iErr2) ! domain number of field calculation
      if(iErr2.ne.0) return
      iDomL=Int2(ir)
      call ExtractIV(Mov_Command,12,ir,iErr2) ! interpolation type
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,13,sch,iErr2) ! output file name
      if(iErr2.lt.1) return
      if(lPerform) then
        open(1,file=sch,iostat=ios)
        if(ios.eq.0) then
          write(1,'(1PE14.6,1X,1PE14.6,1X,1PE14.6)') xPt,yPt,zPt
          write(1,'(1PE14.6,1X,1PE14.6,1X,3I5)') xtPt,ytPt,nx,ny,nt
          xtPt=xtPt/Dble(max(nx-1,1))
          ytPt=ytPt/Dble(max(ny-1,1))
          v(1)=xPt
          v(2)=yPt
          v(3)=zPt
          do i=1,ny
            do m=1,nx
              call GetLoccField(v,iDomL,ir,cf)
              write(1,'(1i3)') iDomL
              write(1,'(6(1PE14.6,1X))') Dble(cf(1)),dImag(cf(1)),Dble(cf(2)),dImag(cf(2)),Dble(cf(3)),dImag(cf(3))
              write(1,'(6(1PE14.6,1X))') Dble(cf(4)),dImag(cf(4)),Dble(cf(5)),dImag(cf(5)),Dble(cf(6)),dImag(cf(6))
              if(nt.eq.1) then
                v(2)=v(2)+xtPt
              else
                v(1)=v(1)+xtPt
              end if
            end do
            if(nt.eq.1) then
              v(2)=v(2)-Dble(nx)*xtPt
              v(3)=v(3)+ytPt
            else if(nt.eq.2) then
              v(1)=v(1)-Dble(nx)*xtPt
              v(3)=v(3)+ytPt
            else
              v(1)=v(1)-Dble(nx)*xtPt
              v(2)=v(2)+ytPt
            end if
          end do
        end if
        close(1)
      end if
    Case('dir')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,DirFileName,'MAX','DIR')
        idum=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
        call SaveDirectives(.true.)
      end if
    Case('dom')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,DomFileName,'MAX','DOM')
        call SaveDomain(.true.)
      end if
    Case('exp')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ExpFileName,'MAX','EXP')
        lAskExp=.false.
        lInsertExp=.false.
        call SaveExpansion(.true.)
      end if
    Case('fie')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,FldFileName,'MAX','FLD')
        lAskFld=.false.
        lSkipFld=.false.
        lSkipDerFld=.false.
        lSkipVecFld=.false.
        lSkipScaFld=.false.
        lSkipFldHead=.false.
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.ge.3) then
          if(sch(1:3).eq.'rep') then
            lSkipFld=.true.
            lSkipDerFld=.true.
          else if(sch(1:3).eq.'val') then
            lSkipFldHead=.true.
          else if(sch(1:3).eq.'der') then
            lSkipFldHead=.true.
            lSkipFld=.true.
          else if(sch(1:3).eq.'vec') then
            lSkipFldHead=.true.
            lSkipFld=.true.
            lSkipScaFld=.true.
          else if(sch(1:3).eq.'sca') then
            lSkipFldHead=.true.
            lSkipFld=.true.
            lSkipVecFld=.true.
          else ! Argument 4: name of file with input data (field points: n, list of n lines: iDom,x,y,z)
            open(1,file=sch,status='old',iostat=ios)
            if(ios.ne.0) then
              write(*,*) 'WRIte FIEld: Cannot open input file!'
              close(1)
              return
            end if
            read(1,*,iostat=ios) m
            if(ios.ne.0) then
              write(*,*) 'WRIte FIEld: Cannot read input file!'
              close(1)
              return
            end if
            open(2,file=FldFileName,status='unknown',iostat=ios)
            if(ios.ne.0) then
              write(*,*) 'WRIte FIEld: Cannot open output file!'
              close(1)
              close(2)
              return
            end if
            write(2,*) m,' Field points, list of complex E and H vectors'
            do i=1,m
              read(1,*,iostat=ios) ir,v(1:3)
              if(ios.ne.0) Cycle
              iDomL=Int2(ir)
              if(ir.lt.0) call DistPtObj(0_2,0_2,v,.true.,xPt,rf,iDomL,val,.true.)
              call GetFieldExp(0,0,v,iDomL,iHEGlobal,idum)
              write(2,'(6(1PE14.6,1X))') Dble(FldExp(1)),DImag(FldExp(1)),Dble(FldExp(2)),DImag(FldExp(2)), &
              & Dble(FldExp(3)),DImag(FldExp(3))
              write(2,'(6(1PE14.6,1X))') Dble(FldExp(4)),DImag(FldExp(4)),Dble(FldExp(5)),DImag(FldExp(5)), &
              & Dble(FldExp(6)),DImag(FldExp(6))
            end do
            if(ios.ne.0) then
              write(*,*) 'WRIte FIEld: Error reading input file!'
              return
            end if
            close(1)
            close(2)
            return
          end if
        end if
        call SaveField(.true.)
      end if
    Case('flf')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,FlfFileName,'MAX','FLF')
        call SaveFField(.true.)
      end if
    Case('gms')
      call setNameS(Argument,lArgument,ProFileName,GeoFileName,'MAX','GEO')
      call ExtractIV(Mov_Command,4,ir,iErr2) ! first boundary
      if(iErr2.ne.0) ir=1
      call ExtractIV(Mov_Command,5,m,iErr2) ! last boundary
      if(iErr2.ne.0) m=nBnd
      call ExtractIV(Mov_Command,6,idum,iErr2) ! domain number
      if(iErr2.ne.0) idum=0
      iDomL=Int2(idum)
      call ExtractIV(Mov_Command,7,idum,iErr2) ! color number
      if(iErr2.ne.0) idum=0
      iDomL=Int2(idum)
      if(lPerform) then
        call GMSHwriteGeoFile(1,GeoFileName,ir,m,iDomL,iDomR)
      end if
    Case('PFD')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,PFDFileName,'MAX','PFD')
        call SavePFD(.true.)
      end if
    Case('fit')
      call ExtractStr(Mov_Command,3,sch,idum)
      if(idum.lt.3) then
        iErr=1
        return
      end if
      chr3(1:3)=sch(1:3)
      select case(chr3(1:3))
      Case('beg') ! start fitness value computation (nOptFit values)
        call ExtractIV(Mov_Command,4,nOptFit,iErr2) ! number of fitness values
        if(iErr2.ne.0) nOptFit=1
        OptFit(1:nOptFit)=-2.0d300
        iOptFit=0
        write(*,*) 'FITness BEGin, nOptFit,nOptPar=',nOptFit,nOptPar
      Case('end') ! write fitness values on exchange file
        if(lPerform) then
          iMovVar(0:nOptPar-1)=nint(rMovVar(0:nOptPar-1),4)
          if(iOptFile.ge.0) then
            do
              inquire(unit=iOptFile,position=sPosition)
              if(sPosition.eq.'R') then
                if(lOptBin) then
                  write(iOptFile) nOptFit,nOptPar,iMovVar(0:nOptPar-1),OptFit(1:nOptFit)
                  rewind(iOptFile)
                else
                  write(iOptFile) nOptFit,nOptPar,rMovVar(0:nOptPar-1),OptFit(1:nOptFit)
                  rewind(iOptFile)
                end if
                Exit
              else
                call sleepQQ(10_4)
              end if
            end do
            write(*,*) 'FITness END, nOptFit,nOptPar=',nOptFit,nOptPar
          else
            write(*,*) 'FITness END problem: no data exchange file open'
          end if
        end if
      Case Default ! get next (single) fitness value
        call GetRch(chr3,3_4,iSaveFunction,iErr)
        if(iSaveFunction.ne.1) iErr=2
        if(iErr.ne.0) return
        iOptFit=min(100,iOptFit+1)
        OptFit(iOptFit)=rch(1)
        write(*,*) 'FITness value computed:',OptFit(iOptFit)
      end Select
    Case('fun')
      if(lPerform) then
        lAskFun=.false.
        lSkipFun=.false.
        if(lArgument.lt.3) Argument(lArgument+1:3)=' '
        if(Argument(1:1).eq.'!') then
          iSaveFunction=-3
          lAskFun=.false.
          lSkipFun=.false.
          lSkipFunHead=.false.
          call SaveFunction(.true.,.false.)
          iSaveFunction=0
        else
          call setNameS(Argument,lArgument,ProFileName,FunFileName,'MAX','FUN')
          call ExtractStr(Mov_Command,4,sch,idum)
          if(idum.lt.3) sch(idum+1:3)=' '
          chr3(1:3)=sch(1:3)
          if(chr3(1:1).eq.' ') then
            iSaveFunction=0
            lAskFun=.false.
            lSkipFun=.false.
            lSkipFunHead=.false.
            call SaveFunction(.true.,.false.)
          else if(chr3(1:3).eq.'clo') then
            iSaveFunction=-3
            lAskFun=.false.
            lSkipFun=.false.
            lSkipFunHead=.false.
            call SaveFunction(.true.,.false.)
            iSaveFunction=0
          else if(chr3(1:3).eq.'cpu') then
            call ExtractIV(Mov_Command,5,idum,iErr2)
            if(iErr2.ne.0) idum=1
            if(lPerform) then
              idum=min(2,max(0,idum))
              call cpu_time(rMovCPU1) !time
              call date_and_time(charMoveTim(1),charMoveTim(2),charMoveTim(3),iMoveTim) !time
              rMovELA1=3600.0d0*Dble(iMoveTim(5))+60.0d0*Dble(iMoveTim(6))+Dble(iMoveTim(7))+0.001d0*Dble(iMoveTim(8)) !time
              do while(rMovELA1.lt.rMovELA0) 
                rMovELA1=rMovELA1+24.0d0*3600.0d0 ! add one day in seconds !time
              end do
              rch(1)=rMovCPU1-rMovCPU0
              rch(2)=rMovELA1-rMovELA0
              if(l4.and.l6) write(*,*) 'CPU,elapsed time=',Real(rch(1)),Real(rch(2)),' seconds' !time
              rMovCPU0=rMovCPU1
              rMovELA0=rMovELA1
              lAskFun=.false.
              lSkipFun=.false.
              lSkipFunHead=.false.
              iSaveFunction=idum
              if(idum.eq.0) then ! write "elapsed time" instead of CPU time (idum=2: write both times)
                iSaveFunction=1
                rch(1)=rch(2)
              end if
              call SaveFunction(.true.,.false.)
              iSaveFunction=0
            end if
          else if(chr3(1:3).eq.'hea') then
            iSaveFunction=-1
            nx=nFun
            ny=nFunA
            call ExtractIV(Mov_Command,5,nFun,iErr2)
            if(iErr2.ne.0) return
            call ExtractIV(Mov_Command,6,nFunA,iErr2)
            if(iErr2.ne.0) return
            lAskFun=.false.
            lSkipFun=.false.
            lSkipFunHead=.false.
            call SaveFunction(.true.,.false.)
            nFun=nx
            nFunA=ny
            iSaveFunction=0
          else if(chr3(1:3).eq.'rep') then
            iSaveFunction=0
            lAskFun=.false.
            lSkipFun=.true.
            lSkipFunHead=.false.
            call SaveFunction(.true.,.false.)
            iSaveFunction=0
          else if(chr3(1:3).eq.'tex') then
            call ExtractTxt(Mov_Command,1,sch,idum)
            if(idum.lt.1) sch(1:1)='!'
            iSaveFunction=-2
            lAskFun=.false.
            lSkipFun=.false.
            lSkipFunHead=.false.
            call SaveFunction(.true.,.false.)
            iSaveFunction=0
          else if(chr3(1:3).eq.'val') then
            iSaveFunction=0
            lAskFun=.false.
            lSkipFun=.false.
            lSkipFunHead=.true.
            call SaveFunction(.true.,.false.)
            iSaveFunction=0
          else
            call GetRch(chr3,4_4,iSaveFunction,iErr2)
            if(iErr2.ne.0) return
            lAskFun=.false.
            lSkipFun=.false.
            lSkipFunHead=.false.
            call SaveFunction(.true.,.false.)
            iSaveFunction=0
          end if
        end if
      end if
    Case('grf')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,GrfFileName,'MAX','GRF')
        call SaveFGrid(.true.)
      end if
    Case('grt')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,GrtFileName,'MAX','GRT')
        call SaveTGrid(.true.)
      end if
    Case('int')
      call ExtractStr(Mov_Command,4,sch,iErr0)
      if(iErr0.lt.3) sch(1:3)='nnn'
      if(sch(1:3).eq.'bou') then
        call ExtractIV(Mov_Command,5,ir,iErr0,nBnd)
        call ExtractIV(Mov_Command,6,nx,iErr1)
        call ExtractIV(Mov_Command,7,ny,iErr2)
        call ExtractIV(Mov_Command,8,nz,iErr3)
        call ExtractIV(Mov_Command,9,nt,iErr4)
        call ExtractRV(Mov_Command,10,r,iErr5)
        call ExtractIV(Mov_Command,11,m,iErr6)
        if(lPerform) then
          call setNameS(Argument,lArgument,ProFileName,InfFileName,'MAX','FUN')
          if(iErr0.eq.0) iBndInt=ir
          if(iErr1.eq.0) IntWhat=nx
          if(iErr2.eq.0) IntInter=ny
          if(iErr3.eq.0) IntField=nz
          if(iErr4.eq.0) MaxIntIter=nt
          if(iErr5.eq.0) AccIntegral=r
          if(iErr6.eq.0) IntType=m
          lIntgSave=.true.
          lAskQ=.false.
          call BndIntg(.true.)
          lAskQ=.true.
        end if
      else if(sch(1:3).eq.'obj') then
        call ExtractIV(Mov_Command,5,ir,iErr0,nObj)
        call ExtractIV(Mov_Command,6,nx,iErr1)
        call ExtractIV(Mov_Command,7,ny,iErr2)
        if(iErr1.ne.0) nx=0
        if(iErr2.ne.0) ny=0
        if(lPerform) then
          call setNameS(Argument,lArgument,ProFileName,InfFileName,'MAX','FUN')
          if(iErr0.eq.0) iObjInt=ir
          lIntgSave=.true.
          iIntgEc=Int2(nx)
          iIntgHc=Int2(ny)
          lAskQ=.false.
          call ObjIntg(.true.)
          lAskQ=.true.
        end if
      else
        if(lPerform) then
          call setNameS(Argument,lArgument,ProFileName,IntFileName,'MAX','INT')
          call SaveIntegral(.true.)
        end if
      end if
    Case('mat')
      if(lPerform) then
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.lt.1) then
          idum=1
          sch(1:1)='t'
        end if
        call setNameS(Argument,lArgument,ProFileName,FldFileName,'MAX','FLD')
        if(sch(1:1).eq.'r') then
          call SaveRmatrix(.true.)
          return
        else
          call SaveTmatrix(.true.)
          return
        end if
      end if
    Case('mmp')
      if(lPerform) then
        call ExtractStr(Mov_Command,4,sch,idum)
        if(idum.lt.1) then
          idum=1
          sch(1:1)='h'
        end if
        if(sch(1:1).eq.'a') then
          lMMPrMtr=.true.
          lMMPtMtr=.true.
        else if(sch(1:1).eq.'e') then
          call setNameS(Argument,lArgument,ProFileName,FunFileName,'MAX','FUN')
          if((idum.gt.1).and.(.not.lgcFld)) then
            if(sch(2:2).eq.'2') then
              call SaveError(.true.,.true.)
            else
              call SaveError(.true.,.false.)
            end if
          else
            call SaveError(.true.)
          end if
          return
        else if(sch(1:1).eq.'r') then
          lMMPrMtr=.true.
          lMMPtMtr=.false.
        else if(sch(1:1).eq.'t') then
          lMMPrMtr=.false.
          lMMPtMtr=.true.
        else
          lMMPrMtr=.false.
          lMMPtMtr=.false.
        end if
        call setNameS(Argument,lArgument,ProFileName,MmpFileName,'MAX','MMP')
        call SaveMmp(.true.)
      end if
    Case('obj')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ObjFileName,'MAX','3DO')
        lAskObj=.false.
        lInsertObj=.false.
        call SaveObject(.true.)
      end if
    Case('ogl')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,OGLFileName,'MAX','OGL')
        call SaveCHGLWindow(.true.)
      end if
    Case('pro')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,ProFileName,'MAX','PRO')
        call SaveProAll(.true.)
      end if
    Case('rfi') ! real field on n by m grid -> file
      call ExtractRV(Mov_Command,3,xPt,iErr2) ! x coordinate of first point
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,4,yPt,iErr2) ! y coordinate of first point
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,5,zPt,iErr2) ! z coordinate of first point
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,6,xtPt,iErr2) ! 1st length of rectangle
      if(iErr2.ne.0) return
      call ExtractRV(Mov_Command,7,ytPt,iErr2) ! 2nd length of rectangle
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,8,nx,iErr2) ! number of points in 1st direction
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,9,ny,iErr2) ! number of points in 2nd direction
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,10,nt,iErr2) ! orientation of rectangle 1: yz, 2: xz, 3: xy plane
      if(iErr2.ne.0) return
      call ExtractIV(Mov_Command,11,ir,iErr2) ! domain number of field calculation
      if(iErr2.ne.0) return
      iDomL=Int2(ir)
      call ExtractIV(Mov_Command,12,ir,iErr2) ! interpolation type
      if(iErr2.ne.0) return
      call ExtractStr(Mov_Command,13,sch,iErr2) ! output file name
      if(iErr2.lt.1) return
      if(lPerform) then
        open(1,file=sch,iostat=ios)
        if(ios.eq.0) then
          write(1,'(1PE14.6,1X,1PE14.6,1X,1PE14.6)') xPt,yPt,zPt
          write(1,'(1PE14.6,1X,1PE14.6,1X,3I5)') xtPt,ytPt,nx,ny,nt
          xtPt=xtPt/Dble(max(nx-1,1))
          ytPt=ytPt/Dble(max(ny-1,1))
          v(1)=xPt
          v(2)=yPt
          v(3)=zPt
          do i=1,ny
            do m=1,nx
              call GetLocrField(v,iDomL,ir,rf)
              write(1,'(1I3)') iDomL
              write(1,'(6(1PE14.6,1X))') rf(1:3)
              if(nt.eq.1) then
                v(2)=v(2)+xtPt
              else
                v(1)=v(1)+xtPt
              end if
            end do
            if(nt.eq.1) then
              v(2)=v(2)-Dble(nx)*xtPt
              v(3)=v(3)+ytPt
            else if(nt.eq.2) then
              v(1)=v(1)-Dble(nx)*xtPt
              v(3)=v(3)+ytPt
            else
              v(1)=v(1)-Dble(nx)*xtPt
              v(2)=v(2)+ytPt
            end if
          end do
        end if
        close(1)
      end if
    Case('win')
      if(lPerform) then
        call setNameS(Argument,lArgument,ProFileName,WinFileName,'MAX','WIN')
        call SaveWindow(.true.)
      end if
    Case Default
      iErr=-9
      return
    end Select
  end Subroutine MovWri

  Subroutine setitrFld(lperform,s,ls1,iErr)
    Implicit none
    Integer(4) iErr,ls,ls1
    Logical, intent(in) :: lperform
    Character(*) s
    iErr=0
    ls=ls1
    if(s(ls:ls).eq.char(0)) ls=ls-1
    if(ls.eq.1) then
      Select Case(s(1:1))
      Case('e')
        if(lperform) itrFld=itE
      Case('h')
        if(lperform) itrFld=itH
      Case('s')
        if(lperform) itrFld=itS
      Case('d')
        if(lperform) itrFld=itD
      Case('b')
        if(lperform) itrFld=itB
      Case('j')
        if(lperform) itrFld=itJ
      Case('a')
        if(lperform) itrFld=itA
      Case('v')
        if(lperform) itrFld=itV
      Case Default
        iErr=1
        return
      end Select
    else if(ls.eq.2) then
      Select Case(s(1:2))
      Case('we')
        if(lperform) itrFld=itWe
      Case('wm')
        if(lperform) itrFld=itWh
      Case('wt')
        if(lperform) itrFld=itWt
      Case('pe')
        if(lperform) itrFld=itPe
      Case('pm')
        if(lperform) itrFld=itPh
      Case('pt')
        if(lperform) itrFld=itPt
      Case Default
        iErr=2
        return
      end Select
    else
      iErr=3
    end if
  end Subroutine setitrFld

  Subroutine setlxyzrFld(lperform,s,ls1,iErr)
    Implicit none
    Integer(4) iErr,ls,ls1
    Logical, intent(in) :: lperform
    Character(*) s
    iErr=0
    ls=ls1
    if(s(ls:ls).eq.char(0)) ls=ls-1
    if(ls.eq.1) then
      if(lperform) lxrFld=.false.
      if(lperform) lyrFld=.false.
      if(lperform) lzrFld=.false.
      Select Case(s(1:1))
      Case('x')
        if(lperform) lxrFld=.true.
      Case('y')
        if(lperform) lyrFld=.true.
      Case('z')
        if(lperform) lzrFld=.true.
      Case Default
        if(lperform) lxrFld=.true.
        if(lperform) lyrFld=.true.
        if(lperform) lzrFld=.true.
        iErr=1
        return
      end Select
    else if(ls.eq.2) then
      if(lperform) lxrFld=.true.
      if(lperform) lyrFld=.true.
      if(lperform) lzrFld=.true.
      Select Case(s(1:2))
      Case('xy')
        if(lperform) lzrFld=.false.
      Case('xz')
        if(lperform) lyrFld=.false.
      Case('yz')
        if(lperform) lxrFld=.false.
      Case Default
        if(lperform) lxrFld=.true.
        if(lperform) lyrFld=.true.
        if(lperform) lzrFld=.true.
        if(lperform) iErr=2
        return
      end Select
    else if(ls.eq.3) then
      if(lperform) lxrFld=.true.
      if(lperform) lyrFld=.true.
      if(lperform) lzrFld=.true.
      if(s(1:3).ne.'xyz') iErr=3
    else
      if(lperform) lxrFld=.true.
      if(lperform) lyrFld=.true.
      if(lperform) lzrFld=.true.
      iErr=4
    end if
  end Subroutine setlxyzrFld

  Subroutine setitcFld(lperform,s,ls1,iErr)
    Implicit none
    Integer(4) iErr,ls,ls1
    Logical, intent(in) :: lperform
    Character(*) s
    iErr=0
    ls=ls1
    if(s(ls:ls).eq.char(0)) ls=ls-1
    if(ls.eq.1) then
      if(lperform) lEcFld=.false.
      if(lperform) lHcFld=.false.
      if(lperform) lAcFld=.false.
      if(lperform) lVcFld=.false.
      Select Case(s(1:1))
      Case('e')
        if(lperform) lEcFld=.true.
      Case('h')
        if(lperform) lHcFld=.true.
      Case('a')
        if(lperform) lAcFld=.true.
      Case('v')
        if(lperform) lVcFld=.true.
      Case Default
        if(lperform) lEcFld=.true.
        if(lperform) lHcFld=.true.
        if(lperform) lAcFld=.true.
        if(lperform) lVcFld=.true.
        iErr=1
        return
      end Select
    else if(ls.eq.2) then
      if(lperform) lEcFld=.true.
      if(lperform) lHcFld=.true.
      if(lperform) lAcFld=.true.
      if(lperform) lVcFld=.true.
      Select Case(s(1:2))
      Case('eh')
        if(lperform) lEcFld=.false.
        if(lperform) lHcFld=.false.
      Case('ea')
        if(lperform) lEcFld=.false.
        if(lperform) lAcFld=.false.
      Case('ev')
        if(lperform) lEcFld=.false.
        if(lperform) lVcFld=.false.
      Case('ha')
        if(lperform) lHcFld=.false.
        if(lperform) lAcFld=.false.
      Case('hv')
        if(lperform) lHcFld=.false.
        if(lperform) lVcFld=.false.
      Case('av')
        if(lperform) lAcFld=.false.
        if(lperform) lVcFld=.false.
      Case Default
        if(lperform) iErr=2
        return
      end Select
    else if(ls.eq.3) then
      if(lperform) lEcFld=.true.
      if(lperform) lHcFld=.true.
      if(lperform) lAcFld=.true.
      if(lperform) lVcFld=.true.
      Select Case(s(1:3))
      Case('eha')
        if(lperform) lVcFld=.false.
      Case('ehv')
        if(lperform) lAcFld=.false.
      Case('eav')
        if(lperform) lHcFld=.false.
      Case('hav')
        if(lperform) lEcFld=.false.
      Case Default
        if(lperform) iErr=2
        return
      end Select
    else if(ls.eq.4) then
      if(lperform) lEcFld=.true.
      if(lperform) lHcFld=.true.
      if(lperform) lAcFld=.true.
      if(lperform) lVcFld=.true.
      if(s(1:4).ne.'ehav') iErr=4
    else
      if(lperform) lEcFld=.true.
      if(lperform) lHcFld=.true.
      if(lperform) lAcFld=.true.
      if(lperform) lVcFld=.true.
      iErr=5
    end if
  end Subroutine setitcFld

  Subroutine setlxyzcFld(lperform,s,ls1,iErr)
    Implicit none
    Integer(4) iErr,ls,ls1
    Logical, intent(in) :: lperform
    Character(*) s
    iErr=0
    ls=ls1
    if(s(ls:ls).eq.char(0)) ls=ls-1
    if(ls.eq.1) then
      if(lperform) lxcFld=.false.
      if(lperform) lycFld=.false.
      if(lperform) lzcFld=.false.
      Select Case(s(1:1))
      Case('x')
        if(lperform) lxcFld=.true.
      Case('y')
        if(lperform) lycFld=.true.
      Case('z')
        if(lperform) lzcFld=.true.
      Case Default
        if(lperform) lxcFld=.true.
        if(lperform) lycFld=.true.
        if(lperform) lzcFld=.true.
        iErr=1
        return
      end Select
    else if(ls.eq.2) then
      if(lperform) lxcFld=.true.
      if(lperform) lycFld=.true.
      if(lperform) lzcFld=.true.
      Select Case(s(1:2))
      Case('xy')
        if(lperform) lzcFld=.false.
      Case('xz')
        if(lperform) lycFld=.false.
      Case('yz')
        if(lperform) lxcFld=.false.
      Case Default
        if(lperform) lxcFld=.true.
        if(lperform) lycFld=.true.
        if(lperform) lzcFld=.true.
        if(lperform) iErr=2
        return
      end Select
    else if(ls.eq.3) then
      if(lperform) lxcFld=.true.
      if(lperform) lycFld=.true.
      if(lperform) lzcFld=.true.
      if(s(1:3).ne.'xyz') iErr=3
    else
      if(lperform) lxcFld=.true.
      if(lperform) lycFld=.true.
      if(lperform) lzcFld=.true.
      iErr=4
    end if
  end Subroutine setlxyzcFld
  
  Recursive Subroutine StrToIV(str,i,iOK,m)
    Implicit none
    Character(*) str
    Real(8) r
    Integer(4) i,iOK
    Integer(4), Optional :: m
    if(Present(m)) then
      call StrToIntV(str,i,iOK,m)
    else
      call StrToIntV(str,i,iOK)
    end if
    if(iOK.eq.-1) then ! standard integer string
      iOK=0
      return
    else if(iOK.eq.-4) then ! string of form F(i1,i2) -> use function array
      i=0
      call StrToRV(str,r,iOK)
      if(iOK.eq.0) i=nint(r,4)
    else if(iOK.ge.0) then ! use movie variable
      i=nint(rMovVar(min(999,max(0,iOK))),4)
      if((iOK.lt.1000).and.(iOK.gt.-1)) iOK=0
    else ! error
      i=0
    end if
  end Subroutine StrToIV

  Recursive Subroutine StrToRV(str,r,iOK)
    Implicit none
    Character(*) str
    Real(8) r
    Integer(4) iOK,k,l,i,ib1,ib2,ic
    call StrToReaV(str,r,iOK)
    if(iOK.eq.-1) then ! standard real string
      iOK=0
      return
    else if(iOK.ge.0) then  ! use movie variable
      r=rMovVar(min(999,max(0,iOK)))
      if((iOK.lt.1000).and.(iOK.gt.-1)) iOK=0
    else if(iOK.eq.-4) then ! string of form F(i1,i2) -> use function array
      l=GetSLength(str)
      r=0.0d0
      if(l.lt.1) then
        iOK=2
        return
      end if
      if((str(1:1).eq.'f').or.(str(1:1).eq.'F')) then
        if(l.lt.6) then
          iOK=2
          return
        end if
        ib1=0
        ic=0
        ib2=0
        do i=2,l
          if(str(i:i).eq.'(') then
            ib1=i
            Exit
          end if
        end do
        do i=ib1+2,l
          if(str(i:i).eq.',') then
            ic=i
            Exit
          end if
        end do
        do i=ic+2,l
          if(str(i:i).eq.')') then
            ib2=i
            Exit
          end if
        end do
        if(((ic-ib1).lt.2).or.((ib2-ic).lt.2)) then
          iOK=2
          return
        else
          l=ic-ib1-1
          k=kFunRow
          call GetK(str(ib1+1:ic-1),l,k,iOK) 
          if(iOK.ne.0) return
          kFunRow=k
          l=ib2-ic-1
          k=kFunCol
          call GetK(str(ic+1:ib2-1),l,k,iOK) 
          if(iOK.ne.0) return
          kFunCol=k
          call GetFun(kFunCol,kFunRow,r)
        end if
      else
        iOK=1
        return
      end if
    else if(iOK.eq.-5) then ! formula evaluation
      l=GetSLength(str)
      cForm(0)=exp(1.0d0)
      cForm(1)=Pi
      cForm(2)=Eps0
      cForm(3)=Mue0
      cForm(4)=Kw0
      cForm(5)=Zw0
      pForm(0)=1.0d0
      vForm(0:999)=rMovVar(0:999)
      dFormu=Formula(str,l,cForm,pForm,vForm,5,0,999,1,1,1,iOK)
      if(iOK.eq.0) r=dFormu(1)
    else ! strange error
      r=0.0d0
      iOK=2
    end if
  end Subroutine StrToRV

  Subroutine ExtractIV(s,n,i,iErr,m,i2)
! extract nth non-blank part se of the string s and read its integer value i
    Implicit none
    Integer(4) n,i,iErr,lse,ls,l,iErr2,i1
    Real(8) r
    Logical lFormula
    Character(*) s
    Integer(4), Optional :: m,i2
    call ExtractStr(s,n,sch,lse)
    i=0
    lFormula=.false.
    if(lse.gt.5) then
      if(sch(3:3).eq.'(') lFormula=.true.
    end if
    if(lse.lt.1) then
      iErr=1
    else if(lFormula.or.(sch(1:1).eq.'f').or.(sch(1:1).eq.'F').or.(sch(1:1).eq.'v').or.(sch(1:1).eq.'V')) then
      call ExtractRV(s,n,r,iErr)
      if(iErr.ne.0) return
      i=nint(r,4)
      if(Present(m)) then ! m limits the integer range
        if(Present(i2)) then ! return 2 integer numbers
          if(i.eq.0) then
            i=1
            i2=m
          else if(i.lt.0) then
            i=1
            i2=min(m,-i)
          else
            i=min(m,i)
            i2=i
          end if
        else
          i=max(min(m,i),-m)
        end if
      end if
    else
      iErr=0
      if(Present(m)) then ! m limits the integer range
        if(Present(i2)) then ! return 2 integer numbers
          if(sch(1:1).eq.'-') then ! string has form "-123" (return i=1, i2=123) or "-" (return i=1,i2=m)
            if(lse.lt.2) then
              i=1
              i2=m
            else if((sch(2:2).eq.'n').or.(sch(2:2).eq.'N').or.(sch(2:2).eq.'m').or.(sch(2:2).eq.'M')) then ! string has form "-n+123" (return i=1,i2=m-123) or "-n" (return i=1,i2=m)
              if(lse.lt.4) then
                i=1
                i2=m
              else if(sch(3:3).eq.'+') then
                call StrToIV(sch(4:lse),i2,iErr,m)
                i=1
                i2=m-i2
              else
                i=0
                i2=0
                iErr=2
              end if
            else
              call StrToIV(sch(2:lse),i2,iErr,m)
              i=1
            end if
          else if((sch(1:1).eq.'n').or.(sch(1:1).eq.'N').or.(sch(1:1).eq.'m').or.(sch(1:1).eq.'M')) then ! string has form "n-123" (return i=i2=m-123) or "n" (return i=i2=m)
            if(lse.lt.3) then
              i=m
              i2=m
            else if(sch(2:2).eq.'-') then
              call StrToIV(sch(3:lse),i2,iErr,m)
              i=m-i2
              i2=i
            else
              i=0
              i2=0
              iErr=2
            end if
          else ! string has the form "123" or "123-456"
            ls=0
            do l=2,lse
              if(sch(l:l).eq.'-') then
                ls=l
                Exit
              end if
            end do
            if(ls.lt.1) then
              call StrToIV(sch(1:lse),i,iErr,m)
              i2=i
              if(i.eq.0) then
                i=1
                i2=m
                iErr=0
              end if
            else if(ls.eq.lse) then
              call StrToIV(sch(1:lse-1),i,iErr,m)
              i2=m
            else
              call StrToIV(sch(1:ls-1),i,iErr,m)
              call StrToIV(sch(ls+1:lse),i2,iErr2,m)
              if(iErr2.ne.0) iErr=iErr2
              if(i2.lt.i) then
                i1=i
                i=i2
                i2=i1
              end if
            end if
          end if
        else
          call StrToIV(sch(1:lse),i,iErr,m)
        end if
      else
        call StrToIV(sch(1:lse),i,iErr)
      end if
    end if
  end Subroutine ExtractIV

  Subroutine ExtractRV(s,n,r,iErr)
! extract nth non-blank part se of the string s and read its real value r
    Implicit none
    Integer(4) n,iErr,lse
    Real(8) r
    Character(*) s
    call ExtractStr(s,n,sch,lse)
    r=0.0d0
    if(lse.lt.1) then
      iErr=1
    else
      if(sch(1:1).eq.'/') then
        if(lse.lt.2) then
          iErr=1
        else
          call StrToRV(sch(2:lse),r,iErr)
          r=.inv.r
        end if
      else
        call StrToRV(sch(1:lse),r,iErr)
      end if
    end if
  end Subroutine ExtractRV

  Subroutine GetK(s,n,k,iErr)
    Implicit none
    Integer(4) n,k,iErr
    Character(*) s
    iErr=0
    Select Case(s(1:1))
    Case('+')
      k=k+1
    Case('-')
      k=k-1
    Case('/')
      k=k
    Case Default
      call StrToIV(s(1:n),k,iErr)
    end Select
  end Subroutine GetK

  Subroutine GetRch(chr3,ioff,nout,iErr,ioffp)
  ! Read some values (specified by 3 characters in chr3) from Mov_Command, start at position ioff
  ! Put the nout values in the array rch.
  ! iErr is an error flag
  ! the optional ioffp indicates how many items were read from Mov_Command. Next position for reading would be ioff+ioffp+1
    Implicit none
    Integer(4), Optional :: ioffp
    Integer(4) ioff,nout,iErr,iErr2,i,ir,nx,ny,nz,idum
    Integer(2) iOK
    Real(8) a1,a2,rv(3),x,y,z,dmin,rNmin(3),val(2)
    Complex(8) cv(3),c
    Character(32) sch1
    Character(3) chr3
    Character(1) chr1,chr2
    if(Present(ioffp)) ioffp=0
    nout=1
    iErr=0
    rch(1)=0.0d0
    Select Case(chr3(1:3))
    case('ang') 
      if(Present(ioffp)) ioffp=1
      call ExtractIV(Mov_Command,ioff+1,ich(1),iErr)
      if(iErr.ne.0) return
      if(ich(1).lt.0) ich(1)=nExp+ich(1)+1
      ich(1)=min(nExp,max(1,ich(1)))
      rch(1)=tExp(ich(1))%rE(1)
    case('bou')
      if(Present(ioffp)) ioffp=1
      call ExtractIV(Mov_Command,ioff+1,ich(1),iErr2)
      rch(1)=Dble(nBnd+ich(1))
    case('con')
      nx=0
      do i=1,40
        call ExtractRV(Mov_Command,ioff+i,rch(i),iErr)
        if(Present(ioffp)) ioffp=ioffp+1
        if(iErr.ne.0) Exit
        nx=i
      end do
      if(nx.lt.1) return
      iErr=0
      nout=nx
    case('cnd')
      rch(1)=condition
    case('cxi')
      rch(1)=Dimag(cxPeriod)
    case('cxr')
      rch(1)=Dble(cxPeriod)
    case('cyi')
      rch(1)=Dimag(cyPeriod)
    case('cyr')
      rch(1)=Dble(cyPeriod)
    case('czi')
      rch(1)=Dimag(czPeriod)
    case('czr')
      rch(1)=Dble(czPeriod)
    case('dom')
      if(Present(ioffp)) ioffp=1
      call ExtractIV(Mov_Command,ioff+1,ir,iErr,nDom)
      if(iErr.eq.0) then
        if(Present(ioffp)) ioffp=2
        call ExtractStr(Mov_Command,ioff+2,sch1,idum)
        if(idum.gt.1) then
          call getEUST(ir)
          select case(sch1(1:2))
          Case('ec')
            rch(1)=Dble(eDom(ir))
            rch(2)=DImag(eDom(ir))
            nout=2
          Case('ei')
            rch(1)=DImag(eDom(ir))
          Case('er')
            rch(1)=Dble(eDom(ir))
          Case('uc')
            rch(1)=Dble(uDom(ir))
            rch(2)=DImag(uDom(ir))
            nout=2
          Case('ui')
            rch(1)=DImag(uDom(ir))
          Case('ur')
            rch(1)=Dble(uDom(ir))
          Case('sc')
            rch(1)=Dble(sDom(ir))
            rch(2)=DImag(sDom(ir))
            nout=2
          Case('si')
            rch(1)=DImag(sDom(ir))
          Case('sr')
            rch(1)=Dble(sDom(ir))
          Case('tc')
            rch(1)=Dble(tDom(ir))
            rch(2)=DImag(tDom(ir))
            nout=2
          Case('ti')
            rch(1)=DImag(tDom(ir))
          Case('tr')
            rch(1)=Dble(tDom(ir))
          end select
        end if
      end if
    case('eff')
      if(Present(ioffp)) ioffp=2
      call ExtractIV(Mov_Command,ioff+1,ich(1),iErr2,nExp,ich(3))
      if(iErr2.ne.0) then ! sum over all Rayleigh expansions
        ich(1)=1
        ich(3)=nExp
      end if
      call ExtractIV(Mov_Command,ioff+2,ich(2),iErr2)
      if(iErr2.ne.0) ich(2)=0
      rch(1)=0.0d0
      do nx=ich(1),ich(3)
        if(tExp(nx)%iTypE.ne.4) Cycle
        if(ich(2).eq.0) then
          ny=1
          nz=tExp(nx)%nPar
        else if(ich(2).lt.0) then
          ny=1
          nz=min(tExp(nx)%nPar,-ich(2))
        else
          nz=min(tExp(nx)%nPar,ich(2))
          ny=nz
        end if
        do i=ny,nz
          if(lgcFld) then
            call Rayleigh2DAng(nx,i,a1)
            a2=a1
          else
            call Rayleigh3DAng(nx,i,a1,a2)
          end if
          if((a1.gt.-500.0d0).and.(a2.gt.-500.0d0)) then
            rch(1)=rch(1)+cdAbs2(ParExp(kExc,tExp(nx)%iOff+i))
          end if
        end do
      end do
    case('err')
      if(Present(ioffp)) ioffp=2
      call ExtractStr(Mov_Command,ioff+1,sch,idum)
      if(idum.lt.1) sch(1:1)=' '
      chr1(1:1)=sch(1:1)
      call ExtractStr(Mov_Command,ioff+2,sch,idum)
      if(idum.lt.1) sch(1:1)=' '
      chr2(1:1)=sch(1:1)
      rch(1)=Dble(errFld)
      if(chr1(1:1).eq.'a') then
        if(chr2(1:1).eq.'a') then
          rch(1)=vErrStat(1) ! absolute, average error
        else if(chr2(1:1).eq.'m') then
          rch(1)=vErrStat(2) ! absolute, maximum error
        end if
      else if(chr1(1:1).eq.'f') then
        if(chr2(1:1).eq.'a') then
          rch(1)=vErrStat(5) ! avarage field
        else if(chr2(1:1).eq.'m') then
          rch(1)=vErrStat(6) ! maximum field
        end if
      else if(chr1(1:1).eq.'r') then
        if(chr2(1:1).eq.'a') then ! relative, average error
          rch(1)=vErrStat(3)
        else if(chr2(1:1).eq.'m') then
          rch(1)=vErrStat(4) ! relative, maximum error
        end if
      else if(chr1(1:1).eq.'s') then
        if(chr2(1:1).eq.'a') then ! average error / average field
          rch(1)=vErrStat(3).div.vErrStat(5)
        else if(chr2(1:1).eq.'m') then ! maximum error / average field
          rch(1)=vErrStat(4).div.vErrStat(5)
        end if
      else if(chr1(1:1).eq.'t') then
        if(chr2(1:1).eq.'a') then ! average error / maximum field
          rch(1)=vErrStat(3).div.vErrStat(6)
        else if(chr2(1:1).eq.'m') then ! maximum error / maximum field
          rch(1)=vErrStat(4).div.vErrStat(6)
        end if
      end if
    case('er2')
      rch(1)=Dble(err2Fld)
    case('exp')
      if(Present(ioffp)) ioffp=1
      call ExtractIV(Mov_Command,ioff+1,ich(1),iErr2)
      rch(1)=Dble(nExp+ich(1))
    case('fie')
      if(Present(ioffp)) ioffp=6
      call ExtractStr(Mov_Command,ioff+1,sch1,idum)
      if(idum.lt.4) sch1(idum+1:4)=' '
      call ExtractRV(Mov_Command,ioff+2,x,iErr2)
      if(iErr2.ne.0) x=0.0d0
      call ExtractRV(Mov_Command,ioff+3,y,iErr2)
      if(iErr2.ne.0) y=0.0d0
      call ExtractRV(Mov_Command,ioff+4,z,iErr2)
      if(iErr2.ne.0) z=0.0d0
      rv(1)=x
      rv(2)=y
      rv(3)=z
      call ExtractIV(Mov_Command,ioff+5,ir,iErr2)
      if(iErr2.ne.0) ir=-1
      iOK=Int2(ir)
      if(ir.le.0) call DistPtObj(0_2,0_2,rv,.true.,dmin,rNmin,iOK,val,.true.)
      if(sch1(4:4).eq.'p') then
        rv(1)=x*dcosd(y)
        rv(2)=x*dsind(y)
      end if
      call ExtractIV(Mov_Command,ioff+6,nx,iErr2)
      if(iErr2.ne.0) nx=0
      if(sch1(1:1).eq.'r') then
        call GetLocrField(rv,iOK,nx,rv)
        if(sch1(2:2).eq.'x') then
          rch(1)=rv(1)
        else if(sch1(2:2).eq.'y') then
          rch(1)=rv(2)
        else if(sch1(2:2).eq.'z') then
          rch(1)=rv(3)
        else if(sch1(2:2).eq.'f') then
          rch(1)=(-y*rv(1)+x*rv(2))/dsqrt(x**2+y**2)
        else if(sch1(2:2).eq.'r') then
          rch(1)=(x*rv(1)+y*rv(2))/dsqrt(x**2+y**2)
        else if(sch1(2:2).eq.'2') then
          rch(1)=r3Scl_Prod(rv,rv)
        else
          rch(1)=dsqrt(r3Scl_Prod(rv,rv))
        end if
        if(sch1(3:3).eq.'a') rch(1)=dAbs(rch(1))
        if(sch1(3:3).eq.'2') rch(1)=rch(1)**2
      else
        if(Present(ioffp)) ioffp=7
        call ExtractIV(Mov_Command,ioff+7,ny,iErr2)
        if(iErr2.ne.0) ny=0
        lDispWarn=.true.
        call GetFieldExp(nx,ny,rv,iOK,iHEGlobal,idum)
        if(sch1(1:1).eq.'a') then
          cv=FldExp(7:9)
        else if(sch1(1:1).eq.'e') then
          cv=FldExp(1:3)
        else if(sch1(1:1).eq.'h') then
          cv=FldExp(4:6)
        else if(sch1(1:1).eq.'v') then
          cv=(0.0d0,0.0d0)
          cv(1:1)=FldExp(10:10)
        else
          cv=c3Vec_Prod(FldExp(1:3),Conjg(FldExp(4:6)))
        end if
        if(sch1(2:2).eq.'x') then
          c=cv(1)
        else if(sch1(2:2).eq.'y') then
          c=cv(2)
        else if(sch1(2:2).eq.'z') then
          c=cv(3)
        else if(sch1(2:2).eq.'f') then
          c=(-y*cv(1)+x*cv(2))/dsqrt(x**2+y**2)
        else if(sch1(2:2).eq.'r') then
          c=(x*cv(1)+y*cv(2))/dsqrt(x**2+y**2)
        else if(sch1(2:2).eq.'2') then
          c=c3Scl_Prod(cv,Conjg(cv))
        else
          c=cdsqrt(c3Scl_Prod(cv,Conjg(cv)))
        end if
        if(sch1(3:3).eq.'r') then
          rch(1)=Dble(c)
        else if(sch1(3:3).eq.'i') then
          rch(1)=DImag(c)
        else if(sch1(3:3).eq.'a') then
          rch(1)=cdAbs1(c)
        else if(sch1(3:3).eq.'2') then
          rch(1)=cdAbs2(c)
        else
          rch(1)=Dble(c)
          rch(2)=DImag(c)
          nout=2
        end if
      end if
    case('fre')
      if(Present(ioffp)) ioffp=1
      call ExtractStr(Mov_Command,ioff+1,sch,idum)
      if(idum.lt.3) sch(idum+1:3)=' '
      if(sch(1:3).eq.'abs') then
        rch(1)=cdAbs1(fcFld)
      else if(sch(1:3).eq.'ima') then
        rch(1)=DImag(fcFld)
      else if(sch(1:3).eq.'rea') then
        rch(1)=Dble(fcFld)
      else
        rch(1)=Dble(fcFld)
        rch(2)=DImag(fcFld)
        nout=2
      end if
    case('fun')
      if(Present(ioffp)) ioffp=4
      call ExtractStr(Mov_Command,ioff+1,sch1,idum)
      if(idum.lt.4) sch1(idum+1:4)=' '
      call ExtractIV(Mov_Command,ioff+2,ir,iErr2)
      if(iErr2.ne.0) ir=1
      call ExtractIV(Mov_Command,ioff+3,nx,iErr2)
      if(iErr2.ne.0) nx=1
      call ExtractIV(Mov_Command,ioff+4,ny,iErr2)
      if(iErr2.ne.0) ny=nFun
      Select Case(sch1(1:3))
      case('max') 
        call MaxFun(ir,rch(1),nx,ny)
      case('min') 
        call MinFun(ir,rch(1),nx,ny)
      case('sum') 
        call SumFun(ir,rch(1),nx,ny)
      case('su1') 
        call SumFun1(ir,rch(1),nx,ny)
      case('su2') 
        call SumFun2(ir,rch(1),nx,ny)
      case('ave') 
        call AveFun(ir,rch(1),nx,ny)
      case('av1') 
        call AveFun1(ir,rch(1),nx,ny)
      case('av2') 
        call AveFun2(ir,rch(1),nx,ny)
      case default
        rch(1)=0.0d0
      end select
      nout=1
    case('gam')
      if(Present(ioffp)) ioffp=1
      call ExtractStr(Mov_Command,ioff+1,sch,idum)
      if(idum.lt.3) sch(idum+1:3)=' '
      if(sch(1:3).eq.'abs') then
        rch(1)=cdAbs1(gcFld)
        if(lzPer) rch(1)=cdAbs1(dcFld)
      else if(sch(1:3).eq.'ima') then
        rch(1)=DImag(gcFld)
        if(lzPer) rch(1)=DImag(dcFld)
      else if(sch(1:3).eq.'rea') then
        rch(1)=Dble(gcFld)
        if(lzPer) rch(1)=Dble(dcFld)
      else
        rch(1)=Dble(gcFld)
        rch(2)=DImag(gcFld)
        if(lzPer) then
          rch(1)=Dble(dcFld)
          rch(2)=DImag(dcFld)
        end if
        nout=2
      end if
    case('ima')
      rch(1)=currentIntegralMax
    case('ixa')
      rch(1)=currentIntegralMaxL(1)
    case('iya')
      rch(1)=currentIntegralMaxL(2)
    case('iza')
      rch(1)=currentIntegralMaxL(3)
    case('imi')
      rch(1)=currentIntegralMin
    case('isa')
      rch(1)=currentIntegralMax1S
    case('isi')
      rch(1)=currentIntegralMin1S
    case('ixi')
      rch(1)=currentIntegralMinL(1)
    case('iyi')
      rch(1)=currentIntegralMinL(2)
    case('izi')
      rch(1)=currentIntegralMinL(3)
    case('int')
      rch(1)=currentIntegral
    case('kw0') ! free space wave number
      rch(1)=2.0d0*Pi*Dble(fcFld)*kw0
    case('la0') ! wavelength in free space (same as 'wav')
      rch(1)=1.0d0/(Dble(fcFld)*kw0)
    case('lam') ! wave length of waveguide mode
      rch(1)=1.0d0/(Dble(fcFld)*Dble(gcFld)*kw0)
    case('len') ! propagation length of waveguide mode
      rch(1)=0.25d0/(Pi*Dble(fcFld)*DImag(gcFld)*kw0)
    case('lma')
      rch(1)=currentIntegralMax1
    case('lmi')
      rch(1)=currentIntegralMin1
    case('max')
      rch(1)=rMaxFld
    case('min')
      rch(1)=rMinFld
    case('mmp')
      if(Present(ioffp)) ioffp=2
      call ExtractIV(Mov_Command,ioff+2,nx,iErr2)
      if(iErr2.ne.0) nx=0
      call ExtractStr(Mov_Command,ioff+1,sch,idum)
      if(idum.lt.3) return
      select case(sch(1:3))
      case('amp')
        rch(1)=currentIntegral
      case('ave')
        rch(1)=errorA
      case('cgi')
        rch(1)=Dble(itCG)
      case('cnd')
        rch(1)=condition
      case('col')
        rch(1)=Dble(mCol)
      case('eig')
        rch(1)=Dble(itEigen)
      case('mat')
        rch(1)=Dble(nBndPt)
      case('max')
        rch(1)=errorM
      case('res')
        rch(1)=resEigen
        if(nx.gt.0) rch(1)=rMMPResV(min(9,nx))
      case('row')
        rch(1)=Dble(nRow)
      end select
    case('ome')
      if(Present(ioffp)) ioffp=1
      call ExtractStr(Mov_Command,ioff+1,sch,idum)
      if(idum.lt.3) sch(idum+1:3)=' '
      if(sch(1:3).eq.'abs') then
        rch(1)=2.0d0*Pi*cdAbs1(fcFld)
      else if(sch(1:3).eq.'ima') then
        rch(1)=2.0d0*Pi*DImag(fcFld)
      else if(sch(1:3).eq.'rea') then
        rch(1)=2.0d0*Pi*Dble(fcFld)
      else
        rch(1)=2.0d0*Pi*Dble(fcFld)
        rch(2)=2.0d0*Pi*DImag(fcFld)
        nout=2
      end if
    case('p2d')
      if(Present(ioffp)) ioffp=2
      call ExtractIV(Mov_Command,ioff+1,nx,iErr,nParticle2D)
      if(iErr.ne.0) return
      call ExtractStr(Mov_Command,ioff+2,sch,idum)
      if(idum.lt.2) then
        iErr=1
        return
      end if
      select case(sch(1:2))
      case('ma')
        rch(1)=tParticle2D(nx)%Mass
      case('px')
        rch(1)=tParticle2D(nx)%Position(1)
      case('py')
        rch(1)=tParticle2D(nx)%Position(2)
      case('vx')
        rch(1)=tParticle2D(nx)%Velocity(1)
      case('vy')
        rch(1)=tParticle2D(nx)%Velocity(2)
      case('ax')
        rch(1)=tParticle2D(nx)%Acceleration(1)
      case('ay')
        rch(1)=tParticle2D(nx)%Acceleration(2)
      case('fx')
        rch(1)=tParticle2D(nx)%Force(1)
      case('fy')
        rch(1)=tParticle2D(nx)%Force(2)
      end select
    case('p3d')
      if(Present(ioffp)) ioffp=2
      call ExtractIV(Mov_Command,ioff+1,nx,iErr,nParticle2D)
      if(iErr.ne.0) return
      call ExtractStr(Mov_Command,ioff+2,sch,idum)
      if(idum.lt.2) then
        iErr=1
        return
      end if
      select case(sch(1:2))
      case('ma')
        rch(1)=tParticle2D(nx)%Mass
      case('px')
        rch(1)=tParticle2D(nx)%Position(1)
      case('py')
        rch(1)=tParticle2D(nx)%Position(2)
      case('pz')
        rch(1)=tParticle2D(nx)%Position(3)
      case('vx')
        rch(1)=tParticle2D(nx)%Velocity(1)
      case('vy')
        rch(1)=tParticle2D(nx)%Velocity(2)
      case('vz')
        rch(1)=tParticle2D(nx)%Velocity(3)
      case('ax')
        rch(1)=tParticle2D(nx)%Acceleration(1)
      case('ay')
        rch(1)=tParticle2D(nx)%Acceleration(2)
      case('az')
        rch(1)=tParticle2D(nx)%Acceleration(3)
      case('fx')
        rch(1)=tParticle2D(nx)%Force(1)
      case('fy')
        rch(1)=tParticle2D(nx)%Force(2)
      case('fz')
        rch(1)=tParticle2D(nx)%Force(3)
      end select
    case('par')
      if(Present(ioffp)) ioffp=3
      call ExtractIV(Mov_Command,ioff+1,ich(1),iErr,nExp)
      if(iErr.ne.0) return
      if(ich(1).lt.0) ich(1)=nExp+ich(1)+1
      ich(1)=min(nExp,max(1,ich(1)))
      call ExtractIV(Mov_Command,ioff+2,ich(2),iErr)
      if(iErr.ne.0) return
      if(ich(2).lt.0) ich(2)=tExp(ich(1))%nPar+ich(2)+1
      call ExtractStr(Mov_Command,ioff+3,sch,idum)
      if(idum.lt.3) sch(idum+1:3)=' '
      if(ich(2).eq.0) then
        do i=1,tExp(ich(1))%nPar-1
          ich(2)=i
          if(sch(1:3).eq.'abs') then
            rch(1)=cdAbs1(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
          else if(sch(1:3).eq.'ang') then
            rch(1)=datan2(DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2))),Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2))))
            rch(1)=rch(1)*180.0d0/Pi
          else if(sch(1:3).eq.'ima') then
            rch(1)=DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
          else if(sch(1:3).eq.'rad') then
            rch(1)=datan2(DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2))),Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2))))
          else if(sch(1:3).eq.'rea') then
            rch(1)=Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
          else if((sch(1:3).eq.'sqa').or.(sch(1:3).eq.'squ')) then
            rch(1)=cdAbs2(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
          else
            rch(1)=Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
            rch(2)=DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
            nout=2
          end if
          lAskFun=.false.
          lSkipFun=.false.
          lSkipFunHead=.false.
          call SaveFunction(.true.,.false.)
          iSaveFunction=0
        end do
        ich(2)=tExp(ich(1))%nPar
      end if
      ich(2)=min(tExp(ich(1))%nPar,max(1,ich(2)))
      if(sch(1:3).eq.'abs') then
        rch(1)=cdAbs1(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
      else if(sch(1:3).eq.'ang') then
        rch(1)=datan2(DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2))),Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2))))
        rch(1)=rch(1)*180.0d0/Pi
      else if(sch(1:3).eq.'ima') then
        rch(1)=DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
      else if(sch(1:3).eq.'rad') then
        rch(1)=datan2(DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2))),Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2))))
      else if(sch(1:3).eq.'rea') then
        rch(1)=Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
      else if((sch(1:3).eq.'sqa').or.(sch(1:3).eq.'squ')) then
        rch(1)=cdAbs2(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
      else
        rch(1)=Dble(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
        rch(2)=DImag(ParExp(kExc,tExp(ich(1))%iOff+ich(2)))
        nout=2
      end if
    case('pfd')
      if(Present(ioffp)) ioffp=2
      call ExtractIV(Mov_Command,ioff+1,nx,iErr,nPFDsens)
      if(iErr.ne.0) return
      call ExtractStr(Mov_Command,ioff+2,sch,idum)
      if(idum.lt.2) then
        iErr=1
        return
      end if
      select case(sch(1:2))
      case('ea')
        rch(1)=r3Vec_Length(dPFDsens(1:3,nx))
      case('ex')
        rch(1)=dPFDsens(1,nx)
      case('ey')
        rch(1)=dPFDsens(2,nx)
      case('ez')
        rch(1)=dPFDsens(3,nx)
      case('ha')
        rch(1)=r3Vec_Length(dPFDsens(4:6,nx))
      case('hx')
        rch(1)=dPFDsens(4,nx)
      case('hy')
        rch(1)=dPFDsens(5,nx)
      case('hz')
        rch(1)=dPFDsens(6,nx)
      end select
    case('phi')
      rch(1)=modulo(Dble(trFld*360.0d0*fcFld),360.0d0)
    case('ray')
      if(Present(ioffp)) ioffp=2
      call ExtractIV(Mov_Command,ioff+1,ich(1),iErr,nExp)
      if(iErr.ne.0) return
      if(ich(1).lt.0) ich(1)=nExp+ich(1)+1
      ich(1)=min(nExp,max(1,ich(1)))
      call ExtractIV(Mov_Command,ioff+2,ich(2),iErr)
      if(iErr.ne.0) return
      if(ich(2).lt.0) ich(2)=tExp(ich(1))%nPar+ich(2)+1
      ich(2)=min(tExp(ich(1))%nPar,max(1,ich(2)))
      rch(1)=0.0d0
      if(tExp(ich(1))%iTypE.eq.4) then ! Rayleigh only
        if(lgcFld) then
          call Rayleigh2DAng(ich(1),ich(2),rch(1))
        else
          call Rayleigh3DAng(ich(1),ich(2),rch(1),rch(2))
          nout=2
        end if
      end if
    case('tim')
      rch(1)=Dble(trFld)
    case('var')
      if(Present(ioffp)) ioffp=1
      call ExtractIV(Mov_Command,ioff+1,nz,iErr2)
      if(iErr2.ne.0) nz=0
      iErr2=0
      nz=Max(0,Min(999,nz))
      rch(1)=Dble(rMovVar(nz))
    case('wav')
      rch(1)=1.0d0/(dsqrt(Mue0*Eps0)*Dble(fcFld))
    case Default
      call ExtractRV(Mov_Command,ioff,rch(1),iErr)
    end Select
  end Subroutine GetRch
  
  Subroutine GetRch1(ioff,a,iErr,ioffp)
  ! Get a single real value a from Mov_Command. Start reading at position ioff
  ! iErr is an error flag, ioffp the number of items that hat to be read from Mov_Command, new position for reading ioff+ioffp+1
    Implicit none
    Real(8) a
    Integer(4) ioff,ioffp,iErr,m
    Character(3) chr3
    a=0.0d0
    call ExtractStr(Mov_Command,ioff,sch,iErr)
    chr3(1:3)=sch(1:3)
    call GetRch(chr3,ioff,m,iErr,ioffp)
    if(iErr.ne.0) return
    if(m.ne.1) iErr=-1
    a=rch(1)
  end Subroutine GetRch1

END MODULE CHMOC


