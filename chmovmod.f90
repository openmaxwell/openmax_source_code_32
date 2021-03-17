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
MODULE CHMOV

! movies

  USE CHMOC

  CONTAINS

  Subroutine Movie_Defaults(lCheck)
! set default data
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    call setNameExt(ProFileName,'AVI',AviFileName)
    call setNameExt(ProFileName,'BMP',BmpFileName)
    call setNameExt(ProFileName,'DIR',DirFileName)
    LBmpFileName=GetSLength(BmpFileName)
    LAviFileName=GetSLength(AviFileName)
    iAVIdlg=1_4
    lRestart=.true.
    lAVI=.false.
    lAVIopen=.false.
    kAVIframes=0_4
    nAVIframes=0_4
    nAVIfpsec=5_4
    nMovSeq=1_4
    nMovPic=20_4
    rMovVar=0.0d0
    rMovCPU0=0.0d0
    rMovCPU1=0.0d0
    rMovELA0=0.0d0
    rMovELA1=0.0d0
    Mov_Command1=' 'C
    Mov_Command2(1:5)=' 'C
    lsMovComm=0
    lsMovComms=0
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'! Default directives'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Get field limits'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw Window'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw Field V x'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw Boundary'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw text 10 10  5 "iterations="'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw variable 80 10 5 variable'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'!Write AVI MAX000.AVI'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'!Write AVI /'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Loop 1 1 1 20 1 ! loop 1, var 1, limits 1...20, increment 1'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Iterate Field 1'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Increase variable v1 1'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw Window'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw Field'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw Boundary'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw text 10 10  5 "iterations="'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Draw variable 80 10 5 variable 1'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'!Write AVI /'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'Loop -1 ! end of loop 1'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'!Write AVI !'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,'! End'C)
    call AppendStr(Mov_Commands,lsMovCommands-1,lsMovComms,' 'C)
    idum=nStrInStr(Mov_Commands,lsMovCommands-1)
    nMovCommand=min(mMovCommand,idum)
    kMovCommand=min(kMovCommand,nMovCommand)
   ! idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands(1:lsMovComms)//Char(0)) ! caused problems!
    ldum=DlgSet(outputdlg,IDC_OUT_TEXT4,Mov_Commands(1:lsMovComms)//Char(0))
 end Subroutine Movie_Defaults

! threads

  Subroutine TSaveMovie(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) ios
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=1
    if(.not.lCheck) then
      call setNameExt(ProFileName,'AVI',AviFileName)
      call Open2write(-1,'Select AVI file to be written!','AVI file ',AviFileName,'AVI',ios)
      LAviFileName=GetSLength(AviFileName)
    end if
		call StartMovieThread(lCheck)
  end Subroutine TSaveMovie

  Subroutine StartMovieThread(ldi)
! start the movie thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start movie thread'C)
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
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start movie thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start movie thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(MovieThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start Movie thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
    end if
  end Subroutine StartMovieThread

  Integer(4) Function MovieThread(iWhat)
! movie tread: calls.....
    Implicit none
    Include 'resource.fd'
    Integer(4) iWhat
    Logical ldum
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    MovieThread=0_4
    if(iThreadAction.eq.1) then
	    call SaveMovie(ldum)
    end if
    call endThread()
  end Function MovieThread

! I/O

  Subroutine SaveMovie(lCheck)
! save movie data in a file - run all directives
    Implicit none
    Include 'resource.fd'
    Real(8) rt0,rt1,rt2,rt3 !time 
	  Integer(4) k,kM,iErr2,idum,lout,itime(8) !time
    Character*10 datech(3) !time
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    call OutTxt('t3','Save movie'C)
    lRestart=.true.
    idum=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
    ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.false.)
    do while(lRestart)
      lRestart=.false.
! check directives and find the end mark positions
      call chkDir(iErr2)
      if(iErr2.ne.0) return
! generate the movie
      if(l4.and.l6) then
        call cpu_time(rt0) !time
        call date_and_time(datech(1),datech(2),datech(3),itime) !time
        rt2=3600.0d0*Dble(itime(5))+60.0d0*Dble(itime(6))+Dble(itime(7))+0.001d0*Dble(itime(8)) !time
      end if
      idum=0
      kM=kMovCommand
      kMovCommand=0
      do
        kMovCommand=kMovCommand+1
        if(kMovCommand.gt.nMovCommand) Exit
        call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
        if(lsMovComm.lt.1) exit
        call OutTxt('t3','A:'//Mov_Command(1:lsMovComm)//Char(0))
        call IntToStr(Int4(kMovCommand),0,0,SpaceText,lout)
		    call OutTxt('n3',SpaceText(1:lout))
        call IntToStr(nMovCommand,0,0,SpaceText,lout)
		    call OutTxt('m3',SpaceText(1:lout))
        k=kMovCommand
        call MovieCommand(.true.,k,idum)
        if(k.ne.kMovCommand) kMovCommand=k-1 ! jump to kMovCommand=k (+1 will be added)
        if(lRestart.or.lStopThread.or.(idum.gt.0).or.(idum.eq.-99)) Exit
      end do
      kMovCommand=kM
      if(l4.and.l6) then
        call cpu_time(rt1) !time
        call date_and_time(datech(1),datech(2),datech(3),itime) !time
        rt3=3600.0d0*Dble(itime(5))+60.0d0*Dble(itime(6))+Dble(itime(7))+0.001d0*Dble(itime(8)) !time
        do while(rt3.lt.rt2) 
          rt3=rt3+24.0d0*3600.0d0 ! add one day in seconds !time
        end do
        write(*,*) 'Movie: CPU,elapsed time=',Real(rt1-rt0),Real(rt3-rt2),' seconds' !time
        rt0=rt1 !time
        rt2=rt3 !time
      end if
    end do
    call OutTxt('t3','End movie'C)
    ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.true.)
  end Subroutine SaveMovie
  
  Subroutine ChkDir(iErr2)
! check label and loop positions
    Implicit none
    Include 'resource.fd'
	  Integer(4) idum,ir2,iErr2,k
    iErr2=0
    iMovLab=0
    k=kMovCommand
    do kMovCommand=1,nMovCommand
      call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
      if(lsMovComm.lt.1) exit
      call ToLower(Mov_Command,lsMovComm)
      if(Mov_Command(1:3).eq.'lab') then
        call ExtractIV(Mov_Command,2,ir2,iErr2)
        if((iErr2.eq.0).and.(ir2.gt.-1000).and.(ir2.lt.1000)) then
          iMovLab(ir2)=kMovCommand
        end if
      elseif(Mov_Command(1:3).eq.'loo') then
        call ExtractIV(Mov_Command,2,ir2,iErr2)
        if((iErr2.eq.0).and.(ir2.gt.-1000).and.(ir2.lt.1000)) then
          iMovLab(ir2)=kMovCommand
          if(ir2.gt.0) loopStart(ir2)=.true.
          if(ir2.lt.0) then ! loop end: check if start is present
            if(iMovLab(-ir2).eq.0) then ! Start was not defined
              write(*,*) 'Loop end on line ',kMovCommand
              write(*,*) 'Without loop start before!'
              write(*,*) 'Cannot execute directives!'
              idum=MessageBoxQQ('Loop start is missing!'C,'SaveMovie'C,MB$OK.or.MB$IconExclamation)
              iErr2=1
              return
            end if
          end if
        end if
      end if
    end do
    do kMovCommand=1,nMovCommand ! check if all loops have an end
      call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
      if(lsMovComm.lt.1) exit
      call ToLower(Mov_Command,lsMovComm)
      if(Mov_Command(1:3).eq.'loo') then
        call ExtractIV(Mov_Command,2,ir2,iErr2)
        if((iErr2.eq.0).and.(ir2.gt.0).and.(ir2.lt.1000)) then ! a loop starts here, search end
          if(iMovLab(-ir2).lt.iMovLab(ir2)) then
            write(*,*) 'Loop end on line ',iMovLab(-ir2)
            write(*,*) 'Before start on line ',iMovLab(ir2)
            write(*,*) 'Cannot execute directives!'
            idum=MessageBoxQQ('Loop end before loop start!'C,'SaveMovie'C,MB$OK.or.MB$IconExclamation)
            iErr2=1
            return
          elseif(iMovLab(-ir2).lt.1) then
            write(*,*) 'Loop start on line ',iMovLab(ir2)
            write(*,*) 'Without loop end!'
            write(*,*) 'Cannot execute directives!'
            idum=MessageBoxQQ('Loop end is missing!'C,'SaveMovie'C,MB$OK.or.MB$IconExclamation)
            iErr2=1
            return
          end if
        end if
      end if
    end do
    kMovCommand=k
  end Subroutine ChkDir

END MODULE CHMOV


