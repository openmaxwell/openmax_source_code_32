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
!
Program MaX
  USE CHoGL
  USE CHDIA
  use opengl_glut
  External MouseAction
  Integer(4) winid,menuid,nRows,nCols
  Integer(4) idum,ievent,iwhi,iwwi,iErr,initializing,it,ios,lenPro,lenOld
  Integer lCommLine,lInfStr,iFilExt
  Logical ldum,lWrit,License
  Character sPosition
  Character(256) InfStr
  Character(3) ExtStr
  Character(256) OldProFileName
  Type(windowconfig) logScreen
  Type(T_RECT) wrect
  Include 'RESOURCE.FD'
  Common/init/initializing
  pSmall=1.0d-300
  nSmall=-1.0d-300
  pBig=1.0d300
  nBig=-1.0d300
  ProFileName='MAX000.PRO'//char(0)
  lenPro=len_trim(ProFileName)-1
  lInverseConn=.false.
  lFLDset0=.false.
  maxXGrd=0 
  iWriteDigits=7_2
  idum=SetExitQQ(QWIN$EXITNOPERSIST)
! get name and directory of the OpenMaXwell executable
  call GetArg(0,MaxFileName,lMaxFileName)
  MaxFileDir=MaxFileName
  lMaxFileDir=lMaxFileName
  do idum=lMaxFileName,0,-1
    if(MaxFileName(idum:idum).eq.'\') then
      lMaxFileDir=idum
      Exit
    end if
  end do
! Set the log screen for write(*...) output
  logScreen.numxpixels =-1
  logScreen.numypixels =-1
  logScreen.numtextcols=85
  logScreen.numtextrows=200
  logScreen.numcolors  =-1
  logScreen.fontsize   =-1
  logScreen.title      ="Info"C
  ldum=SetWindowConfig(logScreen)
  if(.not.ldum) ldum=SetWindowConfig(logScreen)
! set palette
  icPalette(0_4)=#FFFFFF
  icPalette(1:235)=#000000
  icPalette(2_4)=#0000FF
  icPalette(3_4)=#00FF00
  icPalette(4_4)=#FF0000
  iColorPalette=RemapAllPaletteRGB(icPalette(0))
  idum=SetTextColor(1_4)
  idum=SetBKColor(0_4)
  call ClearScreen($GCLEARSCREEN)
  nRows=logScreen.numtextrows
  nCols=logScreen.numtextcols
! output on info window
  write(*,*) 'OpenMaXwell, a general MaXwell solver, Version 2017A'
  write(*,*) ' '
  write(*,*) 'Copyright 2017, Christian Hafner'
  write(*,*) 'WWW home page: http://OpenMaX.ethz.ch'
  write(*,*) ' '
  write(*,*) 'This program is free software: you can redistribute it and/or modify'
  write(*,*) 'it under the terms of the GNU General Public License as published by'
  write(*,*) 'the Free Software Foundation, either version 3 of the License, or'
  write(*,*) '(at your option) any later version.'
  write(*,*) 'This program is distributed in the hope that it will be useful,'
  write(*,*) 'but WITHOUT ANY WARRANTY; without even the implied warranty of'
  write(*,*) 'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'
  write(*,*) 'GNU General Public License for more details.'
  write(*,*) 'You should have received a copy of the GNU General Public License'
  write(*,*) 'along with this program.  If not, see <http://www.gnu.org/licenses/>.'
  write(*,*) ' '
  write(*,*) 'If you publish results obtained with this software you must mention'
  write(*,*) 'which of the results were obtained with OpenMaXwell.'
  write(*,*) ' '
  call Cursor(.true.,IDC_WAIT)
! check OpenMaXwell.txt file (contains previous project file name and indicates that licence agreement was accepted)
  call FindOpenMaXwellTxt(iErr)
! license dialog
  if((iabs(iErr).ne.2).or.(maxXGrd.ne.0)) then
    if(.not.License()) stop
  end if
  initializing=1
  l4=.true.
  l5=.true.
  l6=.false.
! initialize protection for multiple threads
  call InitializeCriticalSection(Loc(DrawLock))
! define the size and position of the MaX window
  ldum=GetWindowRect(GetDesktopWindow(),wrect)
  iwhi=min(wrect%bottom-80_4,600_4)
  iwwi=min(wrect%right-40_4,800_4)
  ldum=MoveWindow(GetHwndQQ(QWIN$FRAMEWINDOW),wrect%right-iwwi-5_4,5_4,iwwi,iwhi,.true.)
! File menu
  ldum=DeleteMenuQQ(1,1)
  ldum=DeleteMenuQQ(1,1)
  ldum=InsertMenuQQ(1,1,$MenuSeparator,''C,NUL)
  ldum=InsertMenuQQ(1,1,$MenuEnabled,'Save project...'C,   SaveProAll)
  ldum=InsertMenuQQ(1,1,$MenuEnabled,'Save bitmap...'C,    SaveBitmap)
  ldum=InsertMenuQQ(1,1,$MenuSeparator,''C,NUL)
  ldum=InsertMenuQQ(1,1,$MenuEnabled,'Open project...'C,   OpenProAll)
  ldum=InsertMenuQQ(1,1,$MenuEnabled,'Open bitmap...'C,    OpenBitmap)
! No status menu, view menu, edit menu
  ldum=DeleteMenuQQ(4,0)
  ldum=DeleteMenuQQ(3,0)
  ldum=DeleteMenuQQ(2,0)
! Draw menu
  ldum=InsertMenuQQ(2,0,$MenuEnabled,'&Draw'C,NUL)
  ldum=InsertMenuQQ(2,1,$MenuEnabled,'&3D objects'C,  TDrawObject)
  ldum=InsertMenuQQ(2,2,$MenuEnabled,'&Boundaries'C,  TDrawBoundary)
  ldum=InsertMenuQQ(2,3,$MenuEnabled,'&Domain'C,      TDrawDomain)
  ldum=InsertMenuQQ(2,4,$MenuEnabled,'&Expansions'C,  TDrawExpansion)
  ldum=InsertMenuQQ(2,5,$MenuEnabled,'&Field'C,       TDrawField)
  ldum=InsertMenuQQ(2,6,$MenuEnabled,'Function'C,     TDrawFunction)
  ldum=InsertMenuQQ(2,7,$MenuEnabled,'&Window'C,      TDrawWindow)
! Modify menu
  ldum=InsertMenuQQ(3,0,$MenuEnabled,'&Modify'C,NUL)
  ldum=InsertMenuQQ(3,1,$MenuEnabled,'&Boundaries...'C,  MTModifyBoundary)
  ldum=InsertMenuQQ(3,2,$MenuEnabled,'&Expansions...'C,  MTModifyExpansion)
  ldum=InsertMenuQQ(3,3,$MenuEnabled,'&Field...'C,       MTModifyField)
  ldum=InsertMenuQQ(3,4,$MenuEnabled,'Function...'C,     MTModifyFunction)
  ldum=InsertMenuQQ(3,5,$MenuEnabled,'&2D Objects...'C,  MTModifyObject)
  ldum=InsertMenuQQ(3,6,$MenuEnabled,'&Nothing'C,        MTModifyNothing)
  ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
  ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
  ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
  ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
  ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
  ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
! Tools menu
  ldum=InsertMenuQQ(4,0,$MenuEnabled,'&Tools'C,NUL)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Window...'C,              WindowDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Project...'C,             ProjectDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Movie...'C,               MovieDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'MMP...'C,                 MMPDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Integral...'C,            IntegralDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'FDTD...'C,                PFDDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Function...'C,            FunctionDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Field...'C,               FieldDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Expansions...'C,          ExpansionDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Domains...'C,             DomainDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'Boundaries...'C,          BoundaryDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'3D objects...'C,          ObjectDialog)
  ldum=InsertMenuQQ(4,1,$MenuSeparator,''C,NUL)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'&PET basis...'C,          PETDialog)
  ldum=InsertMenuQQ(4,1,$MenuEnabled,'&Open GL...'C,            CHGLwindowDialog)
! Window menu
  ldum=DeleteMenuQQ(5,5)
  ldum=DeleteMenuQQ(5,4)
  ldum=DeleteMenuQQ(5,3)
  ldum=InsertMenuQQ(5,1,$MenuEnabled,'&Delete window'C,  DeleteWindow)
  ldum=InsertMenuQQ(5,1,$MenuEnabled,'&Add window'C,     MTAddWindow)
! Help menu, About Box, status bar off
  ldum=DeleteMenuQQ(6,2)
  ldum=DeleteMenuQQ(6,1)
  idum=AboutBoxQQ('MaXwell solver\rVersion 2.0\rCopyright 2017, Christian Hafner\rWWW: http://OpenMaX.ethz.ch/'C)
  idum=ClickMenuQQ(QWIN$STATUS) ! turn the status bar off (was leaky, is not of much help here)
  lenPro=len_trim(ProFileName)
  ProFileName(lenPro+1:lenPro+1)=Char(0)
! Modeless dialogs
  iOutCheck1=0_4
  iOutCheck2=1_4
  iOutCheck3=1_4
  iOutCheck4=1_4
  iOutCheck5=1_4
  iOutCheck6=1_4
  iOutCheck7=1_4
  call ShowActionModelessDialog 
  call ShowOutputModelessDialog 
  call ShowMouseModelessDialog 
! initialize data
  call GRC_Defaults(.true.)
  call Functions_Defaults(.true.)
  call Domain_Defaults(.true.)
  call Boundary_Defaults(.true.)
  call Expansion_Defaults(.true.)
  call Object_Defaults(.true.)
  call Field_Defaults(.true.)
  call MMP_Defaults(.true.)
  call Integral_Defaults(.true.)
  call Movie_Defaults(.true.)
  call cBndGetABO()
  call Particle_Defaults(.true.)
  call MBPE_Defaults(.true.)
  write(*,*) 'All defaults set'
! add and draw a graphic window
  nWin=0
  call MTAddWindow(.true.)
  write(*,*) 'Number of graphic windows:',nWin
  lWinInit=.true.
  GRFtime=trFld
  lWinFld(kWin)=.true.
  if(lGRCused.eq.lObjHid) lGRCused=.not.lObjHid
  call MTDrawWindow(.true.)
! draw all boundaries
  call MTDrawBoundary(.true.)
! mouse events
  ievent=MOUSE$MOVE
  ievent=ior(ievent,MOUSE$LBUTTONUP)
  ievent=ior(ievent,MOUSE$LBUTTONDOWN)
  ievent=ior(ievent,MOUSE$RBUTTONUP)
  ievent=ior(ievent,MOUSE$RBUTTONDOWN)
  iOptFile=-9
! get command line arguments: max.exe, input file name, infinite loop string
  call GetArg(1,InpFileName,lCommLine)
  if(lCommLine.gt.0_2) then ! use command line arguments
    write(*,*) 'input file is: ',InpFileName(1:lCommLine)
    call GetArg(2,InfStr,lInfStr)
    if(lInfStr.lt.3) then
      InfStr='ooo'
      lInfStr=3
    end if
    write(*,*) 'InfStr=',InfStr(1:3)
    if(InfStr(1:3).eq.'inf') then ! run in infinite loop
      write(*,*) 'running in infinite loop'
      lWrit=.true.
      it=2_4
      iFilExt=0
      ExtStr(1:3)=InpFileName(lCommLine-2:lCommLine)
      if(ExtStr(1:3).eq.'pro') then ! new optimizer version: input file contains project name
        OldProFileName=ProFileName ! open project if project is new
        lenOld=len_trim(ProFileName)
        ProFileName(1:lCommLine)=InpFileName(1:lCommLine)
        lenPro=lCommLine
        ProFileName(lenPro+1:lenPro+1)=Char(0)
        if(lenPro.eq.lenOld) then
          if(ProFileName(1:lenPro).ne.OldProFileName(1:lenPro)) then ! new project
            write(*,*) 'open project ',ProFileName(1:lenPro)
            call openProAll(.true.)
          end if
        else ! new project
          write(*,*) 'open project ',ProFileName(1:lenPro)
          call openProAll(.true.)
        end if
        InpFileName(1:7)='opt.txt' ! input file name for new optimizer must have this name! read it (first line must contain nOptPar,nOptFit)!
        iOptFile=9
        lOptBin=.false.
        open(8,file='opt.txt',status='old',iostat=ios)
        if(ios.ne.0) stop 'Cannot open opt.txt'
        read(8,*,iostat=ios) nOptPar,nOptFit
        if(ios.ne.0) stop 'Cannot read opt.txt'
        close(8)
        write(*,*) 'Optimization nOptPar,nOptFit=',nOptPar,nOptFit
        if(nOptFit.lt.0) then
          lOptBin=.true.
          nOptFit=-nOptFit
        end if
        iOptProc=0 ! count existing exchange files -> processor number
        do it=0,999
          write(ExtStr(1:3),'(1I3.3)') it
          inquire(file='Opt.'//ExtStr(1:3)//char(0),exist=ldum)
          if(ldum) Cycle
          iOptProc=it
          Exit
        end do
        write(*,*) 'iOptProc,nOptPar,nOptFit=',iOptProc,nOptPar,nOptFit
        ! create exchange file (name must be Opt.xxx, where xxx is an integer) with dummy data, tells optimizer: ready!
        OptFit(1:nOptFit)=-2.0d300
        open(iOptFile,SHARED,file='Opt.'//ExtStr(1:3)//char(0),form='unformatted') ! open shared exchange file
        rMovVar(0:nOptPar-1)=0.0d0
        write(iOptFile) nOptFit,nOptPar,rMovVar(0:nOptPar-1),OptFit(1:nOptFit) ! write initial content
        write(*,*) 'file Opt.'//ExtStr(1:3)
        write(*,*) 'written:',nOptFit,nOptPar,rMovVar(0:nOptPar-1),OptFit(1:nOptFit)
        rewind(iOptFile)
      end if
      do ! run in infinite loop
        if(InpFileName(1:7).eq.'opt.txt') then ! new optimizer version: file contains numer of parameters and range
          write(*,*) 'waiting for job nOptPar,nOptFit=',nOptPar,nOptFit
          do ! wait for job and read
            inquire(unit=iOptFile,position=sPosition)
            if(sPosition.eq.'R') then
              if(lOptBin) then
                read(iOptFile) jOptP,nOptPar,iMovVar(0:nOptPar-1),OptFit(1:nOptFit)
                rMovVar(0:nOptPar-1)=Dble(iMovVar(0:nOptPar-1))
                rewind(iOptFile)
              else
                read(iOptFile) jOptP,nOptPar,rMovVar(0:nOptPar-1),OptFit(1:nOptFit)
                rewind(iOptFile)
              end if
              if(jOptP.eq.0) Exit ! jOptP=0 means: this is a new job!
              if(jOptP.eq.-9) Stop 'received stop from exchange file' ! jOptP=-9 means: stop OpenMaXwell now!
            end if
            call sleepQQ(10_4)
          end do
          if(lOptBin) then
            write(*,*) 'job:',jOptP,nOptPar,nint(rMovVar(0:nOptPar-1),4)
          else
            write(*,*) 'job:',jOptP,nOptPar,rMovVar(0:nOptPar-1)
          end if
          call SaveMovie(.true.)
        else
          if(InpFileName(lCommLine-2:lCommLine).eq.'###') then ! old version
            write(ExtStr,'(1I3.3)') iFilExt
            iFilExt=iFilExt+1_2
            if(iFilExt.gt.999_2) iFilExt=0_2
          end if
          open(7,file=InpFileName(1:lCommLine-3)//ExtStr(1:3),status='old',iostat=ios)
          if(ios.eq.0) then
            write(*,*) 'Opened input file: ',InpFileName(1:lCommLine-3)//ExtStr(1:3)
            it=2_4
            OldProFileName=ProFileName
            lenOld=len_trim(ProFileName)
            read(7,'(a)') ProFileName
            lenPro=len_trim(ProFileName)
            ProFileName(lenPro+1:lenPro+1)=Char(0)
            if(lenPro.eq.lenOld) then
              if(ProFileName(1:lenPro).ne.OldProFileName(1:lenPro)) then ! new project
                write(*,*) 'open project ',ProFileName(1:lenPro)
                call openProAll(.true.)
              end if
            else ! new project
              write(*,*) 'open project ',ProFileName(1:lenPro)
              call openProAll(.true.)
            end if
            nOptPar=0
            do idum=0,999
              read(7,*,IOStat=ios) rMovVar(idum)
              if(ios.ne.0) Exit
              write(*,*) 'Movie variable',idum,' read:',rMovVar(idum)
              nOptPar=nOptPar+1
            end do
            call SaveMovie(.true.)
            ios=1
            do while(ios.ne.0)
              close(7,status='DELETE',iostat=ios)
            end do
            lWrit=.true.
          else if((InpFileName(lCommLine-2:lCommLine).ne.'###').or. &
               & ((InpFileName(lCommLine-2:lCommLine).eq.'###').and.(iFilExt.eq.0_2))) then
            if(lWrit) write(*,*) 'waiting for input file ',InpFileName(1:lCommLine)
            lWrit=.false.
            call sleepQQ(10_4)
          end if
        end if
      end do
    else ! input file is the project file
      ProFileName(1:lCommLine)=InpFileName(1:lCommLine)
      inquire(file=ProFileName(1:lCommLine),exist=ldum)
      if(ldum) then
        lenPro=lCommLine
        ProFileName(lenPro+1:lenPro+1)=Char(0)
        write(*,*) 'open project ',ProFileName(1:lenPro)
        call openProAll(.true.)
        if((lInfStr.gt.2).and.(InfStr(1:3).eq.'exi')) then
          call SaveMovie(.true.)
          stop
        else
          call TSaveMovie(.true.)
        end if
      else
        write(*,*) 'missing project ',ProFileName(1:lCommLine)
      end if
    end if
  else ! no command line arguments: open project dialog
   ! call openProAll(.false.) ! some unknown reason, this may let the 64 bit version crash sometimes on some PCs when compiled with some compiler version!
  end if
! wait for the task to terminate
  initializing=0
  call Cursor(.false.,IDC_WAIT)
  idum=MessageBoxQQ('Display OpenGL window?'C,'OpenGL'C, &
                    MB$YesNo.or.MB$IconQuestion)
  call CHGLDefaults(.true.)
  if(idum.eq.MB$IDNO) then
    ldum=DeleteMenuQQ(4,13)
    ! idum=setexitqq(QWIN$EXITPERSIST)
     idum=WaitForSingleObject(GetCurrentThread(),INFINITE)
  else
! Initialize OpenGL
    call glutInit
    idum=MessageBoxQQ('Try using color index mode?'C,'OpenGL'C, &
                      MB$YesNo.or.MB$IconQuestion)
    if(idum.eq.MB$IDYES) then
      iCHGLmode=1_2
      call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_INDEX,GLUT_DEPTH)))
      idum=glutGet(GLUT_DISPLAY_MODE_POSSIBLE)
    else
      idum=0
    end if
    if(idum.lt.1) then
      iCHGLmode=0_2
      call glutInitDisplayMode(ior(GLUT_DOUBLE,ior(GLUT_RGB,GLUT_DEPTH)))
    end if
! Create OpenGL window
    winid=glutCreateWindow("OpenGL window for OpenMaXwell")
    lCHGLdoubleSide=.true.    
    menuid=ChGLviewInit()
    call CHGLColorPalette()
    call glutAttachMenu(GLUT_RIGHT_BUTTON)
! Set the display callback
    call glutDisplayFunc(ChGLdisplay)
    nObjWeb=0_2
! Create 9 lists of images
    call CHGLDrawAxes()
    call CHGLDrawSurfaces()
    call CHGLDrawGrids()
    call CHGLDrawIsoLines()
    call CHGLDrawVectors()
    call CHGLDrawExpansions()
    call CHGLDrawTubes()
    call CHGLDrawPFDsensors()
    call CHGLDrawPFDsources()
! clear and read OGL data file
    call CHGLclear()
    call openCHGLWindow(.true.)
    lOGLavi=.false.
! Let glut take over
    call glutMainLoop
  end if
end Program MaX

Subroutine FindOpenMaXwellTxt(iFound)
! locate file OpenMaXwell.txt and read/write current project file name
  USE CHBDE
  Integer(4) iErr,iFound
  Character(10) tex
  iFound=0
  open(1,file=MaxFileDir(1:lMaxFileDir)//'OpenMaXwell.txt'//Char(0),Status='Old',IOStat=iErr)
  if(iErr.eq.0) then
    iFound=2
    read(1,'(a)',IOStat=iErr) tex
    if(iErr.eq.0) then
      ifound=-2
    else if(tex(1:8).ne.'OMX1.1CH') then
      ifound=-2
    end if
    close(1)
  end if
end Subroutine FindOpenMaXwellTxt

logical function License()
! display license agreement if OpenMaXwell.txt is missing
  USE CHBDE
  implicit none
	Include 'RESOURCE.FD'
  integer(4) iErr
  Character(2) c
  License=.false.
  call BeepQQ(660_4,1000_4)
  write(*,*) ' '
  write(*,*) 'You are running OpenMaXwell for the first time on this machine.'
  write(*,*) 'Please, read the following LICENSE AGREEMENT carefully.'
  write(*,*) 'You are not allowed to use OpenMaXwell without accepting the agreement.'
  write(*,*) ' '
  write(*,*) 'This program is free software: you can redistribute it and/or modify'
  write(*,*) ' it under the terms of the GNU General Public License as published by'
  write(*,*) 'the Free Software Foundation, either version 3 of the License, or'
  write(*,*) '(at your option) any later version.'
  write(*,*) ' '
  write(*,*) 'This program is distributed in the hope that it will be useful,'
  write(*,*) 'but WITHOUT ANY WARRANTY; without even the implied warranty of'
  write(*,*) 'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the'
  write(*,*) 'GNU General Public License for more details.'
  write(*,*) ' '
  write(*,*) 'You should have received a copy of the GNU General Public License'
  write(*,*) 'along with this program.  If not, see <http://www.gnu.org/licenses/>.'
  write(*,*) 'You run it on your own risk.'
  write(*,*) ' '  
  write(*,*) 'If you publish any results obtained with OpenMaXwell, you agree to'
  write(*,*) 'mention in the text, which of the results were computed with OpenMaXwell.'
  write(*,*) ' '  
  write(*,*) 'ENTER Y TO ACCEPT the AGREEMENT, ENTER ANY OTHER CHARACTER TO DECLINE'
  read(*,'(a)') c
  if((c(1:1).ne.'Y').and.(c(1:1).ne.'y')) stop
! license agreement accepted: create OpenMaXwell.txt and write current project file name on it
  License=.true.
  open(3,file=MaxFileDir(1:lMaxFileDir)//'OpenMaXwell.txt',IOStat=iErr)
  if(iErr.eq.0) then
    call WriteStr(3,'OMX1.1CH',iErr)
  end if
  close(3)
end function License

Subroutine MouseAction(iunit,ievent,ikey,ixp,iyp)
! call back for mouse events
  USE CHDIA
  Include 'resource.fd'
  Real(8) x,y,xg,yg,xv,yv,rm(3),dmin
  Integer(4) iunit,ievent,ikey,ixp,iyp
  Integer(4) iRGB,ierr,ipos,ivm,jvm,kP,kPE,kEx,iHnd,iWnd,initializing,idum,lout
  Integer(2) ixpos,iypos,ix0,iy0,ixprev,iyprev,ixg,iyg,iMWhat,iDrawObj,iWRM,iiRGB,icol
  Logical ldum
  Logical(1) lSet
  Character(20) text
  Common/init/initializing
  Common/DrawObj/iDrawObj
  save ixprev,iyprev,ix0,iy0,iMWhat,iRGB,iWRM,iiRGB,kP,kPE,kEx,iHnd,x,y,xg,yg
  data ixprev,iyprev,ix0,iy0,iMWhat/0,0,0,0,0/
  if(lThreadStarted) return
  if(initializing.eq.1) return
  call EnterCriticalSection(Loc(DrawLock))
! compute cursor location
  ixpos=ixp-iWinLeft(kWin)
  iypos=iyp-iWinTop(kWin)
  ixg=max(1,iabs(iWinXGrid(kWin)))
  iyg=max(1,iabs(iWinYGrid(kWin)))
  if(iWinAction.eq.6_2) then ! modify objects in a rectangle
    iDrawObj=3
    icol=1_2
    call I2R(ixpos,iypos,xv,yv)
    call getProj3Dxy(xv,yv,ViewPlane,rEye,viewDist,x,y)
  else if(iWinAction.eq.5_2) then ! modify expansion
    iDrawObj=6
    icol=1_2
    call I2R(ixpos+1_2,iypos,dmin,yv)
    call I2R(ixpos,iypos,xv,yv)
    dmin=4.0d0*dabs(dmin-xv)
    call getProj3Dxy(xv,yv,ViewPlane,rEye,viewDist,x,y)
    ixpos=ixg*((ixpos+ixg/2_2)/ixg)
    iypos=iyg*((iypos+iyg/2_2)/iyg)
    call I2R(ixpos,iypos,xv,yv)
    call getProj3Dxy(xv,yv,ViewPlane,rEye,viewDist,xg,yg)
  else if(iWinAction.eq.4_2) then ! NO palette anymore
  else if(iWinAction.eq.3_2) then ! modify function values -> snap to x values of the function and to y grid
    iDrawObj=4
    icol=1_2
    if(iMWhat.eq.1) ixpos=ixprev
    iypos=iyg*((iypos+iyg/2_2)/iyg)
    call I2R(ixpos,iypos,x,y)
    call FindFunPosition(iFunA1,x,ipos)
    call GetFun(iFunA1,ipos,x)
    call R2I(x,y,ixpos,iypos)
  else if(iWinAction.eq.2_2) then ! modify field values in a rectangle
    iDrawObj=3
    icol=1_2
    call I2R(ixpos,iypos,xv,yv)
    call getProj3Dxy(xv,yv,ViewPlane,rEye,viewDist,x,y)
  else if(iWinAction.eq.1_2) then ! modify boundary
    iDrawObj=5
    icol=1_2
    call I2R(ixpos+1_2,iypos,dmin,yv)
    call I2R(ixpos,iypos,xv,yv)
    dmin=4.0d0*dabs(dmin-xv)
    call getProj3Dxy(xv,yv,ViewPlane,rEye,viewDist,x,y)
    ixpos=ixg*((ixpos+ixg/2_2)/ixg)
    iypos=iyg*((iypos+iyg/2_2)/iyg)
    call I2R(ixpos,iypos,xv,yv)
    call getProj3Dxy(xv,yv,ViewPlane,rEye,viewDist,xg,yg)
  else
    iDrawObj=0
    icol=0_2
    call I2R(ixpos,iypos,x,y)
  end if
! display cursor location and correlated data
  if((iWinAction.eq.2_2).or.((iWinAction.eq.0_2).and.lWinFld(kWin))) then ! display real field values / modify complex field data
    text='x='                                ! set texts first
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_XT,text(1:2)//Char(0))
    text='y='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_YT,text(1:2)//Char(0))
    text='z='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_ZT,text(1:2)//Char(0))
    text='vx='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vXT,text(1:3)//Char(0))
    text='vy='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vYT,text(1:3)//Char(0))
    text='vz='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vZT,text(1:3)//Char(0))
    text='¦v¦'
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vAT,text(1:3)//Char(0))
    ixrFld=ixpos                             ! find nearest field point
    iyrFld=iypos
    lLimits=.false.
    call DrawField(.false.)                  ! this drawing is invisible!
    call Getij(ixcFld,iycFld,izcFld,ivm,jvm) ! get real field point indices
    rm(1:3)=GetcGrd(ixcFld,iycFld,izcFld)    ! get field point location
    call RealToStr(rm(1),0,4,text,lout)      ! display real field data
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_X,text(1:lout)//Char(0))
    call RealToStr(rm(2),0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_Y,text(1:lout)//Char(0))
    call RealToStr(rm(3),0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_Z,text(1:lout)//Char(0))
    call IntToStr(Int4(ivm),0,0,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_I,text(1:lout)//Char(0))
    call IntToStr(Int4(jvm),0,0,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_J,text(1:lout)//Char(0))
    call RealToStr(rFld(1,ivm,jvm),0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vX,text(1:lout)//Char(0))
    call RealToStr(rFld(2,ivm,jvm),0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vY,text(1:lout)//Char(0))
    call RealToStr(rFld(3,ivm,jvm),0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vZ,text(1:lout)//Char(0))
    call RealToStr(r3Vec_Length(rFld(1:3,ivm,jvm)),0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_vA,text(1:lout)//Char(0))
    call IntToStr(Int4(iFld(ixcFld,iycFld,izcFld)),0,0,text,lout) ! domain number
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_ID,text(1:lout)//Char(0))
    if(ievent.eq.MOUSE$LBUTTONDOWN) then
      iWnd=setCapture(GetHwndQQ(iunit))
      jxcFld=ixcFld
      jycFld=iycFld
      jzcFld=izcFld
    end if
    if((ievent.eq.MOUSE$LBUTTONUP).and.((iWinAction.eq.2_2).or.(iWinAction.eq.6_2))) then
      ldum=releaseCapture()
      call MFieldDialog(.true.) ! dialog for the modification of complex field data
    end if
  else if(iWinAction.eq.6_2) then ! display window coordinates and modify object data
    text='x='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_XT,text(1:2)//Char(0))
    text='y='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_YT,text(1:2)//Char(0))
    call RealToStr(x,0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_X,text(1:lout)//Char(0))
    call RealToStr(y,0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_Y,text(1:lout)//Char(0))
    if(ievent.eq.MOUSE$LBUTTONDOWN) then
      iWnd=setCapture(GetHwndQQ(iunit))
      xminMobj=x
      yminMobj=y
    end if
    if(ievent.eq.MOUSE$LBUTTONUP) then
      ldum=releaseCapture()
      xmaxMobj=x
      ymaxMobj=y
      if(xmaxMobj.lt.xminMobj) then
        xmaxMobj=xminMobj
        xminMobj=x
      end if
      if(ymaxMobj.lt.yminMobj) then
        ymaxMobj=yminMobj
        yminMobj=y
      end if
      call MObjectDialog(.true.) ! dialog for the modification of object data
    end if
  else                                         ! display window coordinates
    text='x='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_XT,text(1:2)//Char(0))
    text='y='
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_YT,text(1:2)//Char(0))
    call RealToStr(x,0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_X,text(1:lout)//Char(0))
    call RealToStr(y,0,4,text,lout)
    idum=SetDlgItemText(mousedlg%hwnd,IDC_MOUSE_Y,text(1:lout)//Char(0))
  end if
! perform window action in one of the graphic windows
  if((iunit.gt.10_4).and.((iunit-10_4).le.mWin)) then
    ierr=SetActiveQQ(iunit)
    iWRM=SetWriteMode($GXOR)
    if(icol.gt.0_2) iiRGB=SetColor(icol)
    if(ievent.eq.MOUSE$LBUTTONDOWN) then
      iWnd=setCapture(GetHwndQQ(iunit))
! save initial position and pixel
      ix0=ixpos
      iy0=iypos
      iRGB=GetPixelRGB(ix0,iy0)
      iMWhat=1
      if(iWinAction.eq.1_2) then
        lCloseNow=.false.
        lMoveInsert=.false.
        call getBndHandle(x,y,dmin,kP,kPE,iHnd)
        if(iHnd.ne.0) then
          call DrawBoundary(kP)
          call updateBnd(xg,yg,kP,kPE,iHnd)
          call DrawBoundary(kP)
        else
          nInsObj=nBnd
          call InsertDialog(.false.)
          if(kInsObj.ge.0) then
            lInsertBnd=lInsObj
            call InsertBnd(kInsObj,1,ldum)
            kP=kInsObj+1
            call InsertBndEdg(kP,0,1,ldum)
            kBnd=kP
            kP=tBnd(kBnd)%iEdgeOffset+1
            tBndEdg(kP)%x=xg
            tBndEdg(kP)%y=yg
            tBndEdg(kP)%xa=xg
            tBndEdg(kP)%ya=yg
            tBndEdg(kP)%xb=xg
            tBndEdg(kP)%yb=yg
            tBndEdg(kP)%xo=xg
            tBndEdg(kP)%yo=yg
            tBndEdg(kP)%r=0.0d0
            iDomBnd=0_2
            iColBnd=0_2
            iConBnd=0_2
            call BoundaryDialog(.false.)
            call DrawBoundary(kP)
          end if
        end if
      else if(iWinAction.eq.5_2) then
        call getExpHandle(x,y,dmin,0,kEx,iHnd)
        if(iHnd.ne.0) then
          call DrawExpansion(kEx)
          call updateExp(xg,yg,kEx,iHnd)
          call DrawExpansion(kEx)
        else
          nInsObj=nExp
          call InsertDialog(.false.)
          if(kInsObj.ge.0) then
            call InsertExp(kInsObj,1,ldum)
            kEx=kInsObj+1
            tExp(kEx)%Plane(1:3,0:3)=0.0d0
            if(tExp(kEx)%iTypE.eq.10) then
              tExp(kEx)%xo=tExp(kEx)%xo+xg-tExp(kEx)%Plane(1,0)
              tExp(kEx)%yo=tExp(kEx)%yo+yg-tExp(kEx)%Plane(2,0)
            end if
            tExp(kEx)%Plane(1,0)=xg
            tExp(kEx)%Plane(2,0)=yg
            tExp(kEx)%Plane(1,1)=1.0d0
            tExp(kEx)%Plane(2,2)=1.0d0
            tExp(kEx)%Plane(3,3)=1.0d0
            if(tExp(kEx)%iTypE.ne.10) then
              tExp(kEx)%xo=tExp(kEx)%Plane(1,0)
              tExp(kEx)%yo=tExp(kEx)%Plane(2,0)
            end if
            tExp(kEx)%O(1:3)=0.0d0
            tExp(kEx)%e(1:3)=0.0d0
            tExp(kEx)%e(1)=1.0d0
            call InsertPar(kEx,0,1,ldum)
            iDomExp=0_2
            iParExp=0_2
            iColExp=0_2
            iConExp=0_2
            kExp=kEx
            call ExpansionDialog(.false.)
            call DrawExpansion(kEx)
          end if
        end if
      end if
    elseif(ievent.eq.MOUSE$RBUTTONDOWN) then
      iWnd=setCapture(GetHwndQQ(iunit))
! save initial position and pixel
      ix0=ixpos
      iy0=iypos
      iRGB=GetPixelRGB(ix0,iy0)
      iMWhat=1
      if(iWinAction.eq.1_2) then
        call getBndHandle(x,y,dmin,kP,kPE,iHnd)
        if(iHnd.ne.0) then
          if((modulo(iHnd,10).eq.0).and.(nBnd.gt.1)) then
            if((kP.lt.1).or.(kP.gt.nBnd)) kP=nBnd
            kBnd=kP
            call BoundaryDialog(.false.)
          else if(modulo(iHnd,10).ne.9) then
            if(tBnd(kP)%nEdge.gt.1) then
              if(iHnd.eq.22) then
                call InsertBndEdg(kP,kPE+1,-1,ldum) ! delete end point of line
              else
                call InsertBndEdg(kP,kPE,-1,ldum) ! delete any other point
              end if
            else if(tBnd(kP)%nEdge.eq.1) then
              kBnd=kP
              call BoundaryDialog(.false.)
            end if
            GRFtime=trFld
            lWinInit=.true.
            call DrawWindow(.true.)
            call cBndGetABO()
            call DrawBoundary(0)
            iHnd=0
          else
            iHnd=iHnd-9
            lCloseNow=.false.
            lMoveInsert=.true.
            call DrawBoundary(kP)
            call updateBnd(xg,yg,kP,kPE,iHnd)
            call DrawBoundary(kP)
          end if
        end if
      else if(iWinAction.eq.5_2) then
        call getExpHandle(x,y,dmin,0,kEx,iHnd)
        if(iHnd.ne.0) then
          kExp=kEx
          call ExpansionDialog(.false.)
          lWinInit=.true.
          call DrawWindow(.true.)
          call DrawBoundary(0)
          call DrawExpansion(0)
          iHnd=0
        end if
      end if
    elseif((ievent.eq.MOUSE$LBUTTONUP).or.(ievent.eq.MOUSE$RBUTTONUP)) then
! hide previous graphic object by redrawing
      ldum=releaseCapture()
      if(iDrawObj.eq.6) then
        if(iHnd.ne.0) call DrawExpansion(kEx)
      else if(iDrawObj.eq.5) then
        if(iHnd.ne.0) call DrawBoundary(kP)
      else if(iDrawObj.eq.4) then
        if((iFunA2.gt.0_2).and.(iFunA2.le.nFunA)) call DrawFunction(.true.)
      else if(iDrawObj.eq.3) then
        call DrawIRectangle(ix0,iy0,ixprev,iyprev,.false.)
      else if(iDrawObj.eq.2) then
        call DrawIArc(ix0-30_2,iy0-10_2,ix0,iy0,ixprev,iyprev)
      else if(iDrawObj.eq.1) then
        call DrawILine(ix0,iy0,ixprev,iyprev)
      end if
      iWRM=SetWriteMode($GPSET)
      iRGB=SetPixelRGB(ix0,iy0,iRGB)
      iMWhat=-1
      if(iWinAction.eq.5_2) then
! save expansion and redraw
        if(iHnd.ne.0) then
          call updateExp(xg,yg,kEx,iHnd)
          lWinInit=.true.
          call DrawWindow(.true.)
          call DrawBoundary(0)
          call DrawExpansion(0)
        end if
      else if(iWinAction.eq.4_2) then ! NO palette anymore
      else if(iWinAction.eq.3_2) then
! save function value and redraw function
        if((iFunA2.gt.0_2).and.(iFunA2.le.nFunA)) then
          call FindFunPosition(iFunA1,x,ipos)
          call SetFun(iFunA2,ipos,y,lSet)
          call DrawFunction(.true.)
        end if
      else if(iWinAction.eq.1_2) then
! save boundary and redraw
        if(iHnd.ne.0) then
          lCloseNow=.true.
          call updateBnd(xg,yg,kP,kPE,iHnd)
          lCloseNow=.false.
          GRFtime=trFld
          lWinInit=.true.
          call DrawWindow(.true.)
          call cBndGetABO()
          call DrawBoundary(0)
        ! call DrawExpansion(0)
        end if
      end if
    elseif(ievent.eq.MOUSE$MOVE) then
! hide previous graphic object by redrawing
      if(iMWhat.gt.0) then
        if(iDrawObj.eq.6) then
          if(iHnd.ne.0) call DrawExpansion(kEx)
        else if(iDrawObj.eq.5) then
          if(iHnd.ne.0) call DrawBoundary(kP)
        else if(iDrawObj.eq.4) then
          if((iFunA2.gt.0_2).and.(iFunA2.le.nFunA)) call DrawFunction(.true.)
        else if(iDrawObj.eq.3) then
          call DrawIRectangle(ix0,iy0,ixprev,iyprev,.false.)
        else if(iDrawObj.eq.2) then
          call DrawIArc(ix0-30_2,iy0-10_2,ix0,iy0,ixprev,iyprev)
        else if(iDrawObj.eq.1) then
          call DrawILine(ix0,iy0,ixprev,iyprev)
        end if
        iRGB=SetPixelRGB(ix0,iy0,iRGB)
        iMWhat=-1
      end if
! draw new graphic object
      if(ikey.eq.1) then
        iRGB=GetPixelRGB(ix0,iy0)
        if(iDrawObj.eq.6) then
          if(iHnd.ne.0) then
            call updateExp(xg,yg,kEx,iHnd)
            call DrawExpansion(kEx)
          end if
        else if(iDrawObj.eq.5) then
          if(iHnd.ne.0) then
            call updateBnd(xg,yg,kP,kPE,iHnd)
            call DrawBoundary(kP)
          end if
        else if(iDrawObj.eq.4) then
          if((iFunA2.gt.0_2).and.(iFunA2.le.nFunA)) then
            call FindFunPosition(iFunA1,x,ipos)
            call SetFun(iFunA2,ipos,y,lSet)
            call DrawFunction(.true.)
          end if
        else if(iDrawObj.eq.3) then
          call DrawIRectangle(ix0,iy0,ixpos,iypos,.false.)
        else if(iDrawObj.eq.2) then
          call DrawIArc(ix0-30_2,iy0-10_2,ix0,iy0,ixpos,iypos)
        else if(iDrawObj.eq.1) then
          call DrawILine(ix0,iy0,ixpos,iypos)
        end if
        iMWhat=1
      end if
    end if
! save current position and restore writing mode and color
    ixprev=ixpos
    iyprev=iypos
    iWRM=SetWriteMode($GPSET)
    if(icol.gt.0) iiRGB=SetColor(iiRGB)
  end if
  call LeaveCriticalSection(Loc(DrawLock))
end Subroutine MouseAction



!---------------------------------------------------------------------------------

  subroutine ShowMouseModelessDialog
  USE CHDLG
  implicit none
  Integer(INT_PTR_KIND()) iThread
  Integer(4), Save:: iArgument
  Integer(4) iStack,iCreation
  iStack=0
  iArgument=0
  iCreation=0
  iThread=0
  iThreadHandle=CreateThread(NULL,iStack,Loc(MouseThreadProc),Loc(iArgument),iCreation,iThread)
  return
  end

  subroutine ShowOutputModelessDialog
  USE CHDLG
  implicit none
  Integer(INT_PTR_KIND()) iThread
  Integer(4), Save:: iArgument
  Integer(4) iStack,iCreation
  iStack=0
  iArgument=0
  iCreation=0
  iThread=0
  iThreadHandle=CreateThread(NULL,iStack,Loc(OutPutThreadProc),Loc(iArgument),iCreation,iThread)
  return
  end

  subroutine ShowActionModelessDialog
  USE CHDLG
  implicit none
  Integer(INT_PTR_KIND()) iThread
  Integer(4), Save:: iArgument
  Integer(4) iStack,iCreation
  iStack=0
  iArgument=0
  iCreation=0
  iThread=0
  iThreadHandle=CreateThread(NULL,iStack,Loc(ActionThreadProc),Loc(iArgument),iCreation,iThread)
  return
  end

  SUBROUTINE ActionSub(dlg,Controlid,callbacktype )
  USE CHoGL
  USE CHDIA
  TYPE (dialog) dlg,dlg1
  INTEGER Controlid,callbacktype,callbacktype1
  Logical ldum
  Include 'resource.fd'
  dlg1=dlg ! avoid unused warning
  callbacktype1=callbacktype ! avoid unused warning
  if(Controlid.eq.IDC_ACTION_STOP) then
    iWinAction=0_2
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !objects
    ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
    call OutTxt('t3',' 'C)
    call OutTxt('m3',' 'C)
    call OutTxt('n3',' 'C)
    call OutTxt('t2',' 'C)
    call OutTxt('m2',' 'C)
    call OutTxt('n2',' 'C)
    call OutTxt('t1','Stop'C)
    call OutTxt('m1',' 'C)
    call OutTxt('n1',' 'C)
    if(lThreadStarted) call StopThread()
  else if(.not.lThreadStarted) then
    call GetkWin(.true.)
    SELECT CASE (Controlid)
	  case(IDC_ACTION_WINDOW_OPTIONS)
      call WindowDialog(.false.)
	  case(IDC_ACTION_OGL_WINDOW_OPTIONS)
      call CHGLwindowDialog(.false.)
	  case(IDC_ACTION_CLEAR)
      call TDrawWindow(.true.)
	  case(IDC_ACTION_COMPUTE_FUN)
      call FunctionDialog(.false.)
	  case(IDC_ACTION_DRAW_FUN)
      iThreadAction=1
      call StartGRFThread(.true.)
	  case(IDC_ACTION_COMPUTE_FLD)
      call FieldDialog(.false.)
	  case(IDC_ACTION_DRAW_FLD)
      call TDrawField(.true.)
	  case(IDC_ACTION_COMPUTE_BND)
      call BoundaryDialog(.false.)
	  case(IDC_ACTION_DRAW_BND)
      iDraBnd=0
      iDomBnd=0_2
      iColBnd=0_2
      iConBnd=0_2
      call TDrawBoundary(.true.)
	  case(IDC_ACTION_COMPUTE_DOM)
      call DomainDialog(.false.)
	  case(IDC_ACTION_DRAW_DOM)
      iDraBnd=0
      iDomBnd=0_2
      iColBnd=0_2
      iConBnd=0_2
      iDraExp=0
      iDomExp=0_2
      iParExp=0_2
      iColExp=0_2
      iConExp=0_2
      call TDrawDomain(.true.)
	  case(IDC_ACTION_COMPUTE_EXP)
      call ExpansionDialog(.false.)
	  case(IDC_ACTION_DRAW_EXP)
      iThreadAction=4
      iDraExp=0
      iDomExp=0_2
      iParExp=0_2
      iColExp=0_2
      iConExp=0_2
      call TDrawExpansion(.true.)
	  case(IDC_ACTION_COMPUTE_OBJ)
      call ObjectDialog(.false.)
	  case(IDC_ACTION_DRAW_OBJ)
      iThreadAction=4
      lDrawOGL=.false.
      call TDrawObject(.true.)
	  case(IDC_ACTION_COMPUTE_MMP)
      call MMPDialog(.false.)
	  case(IDC_ACTION_COMPUTE_GFD)
      call PFDDialog(.false.)
	  case(IDC_ACTION_COMPUTE_INT)
      call IntegralDialog(.false.)
	  case(IDC_ACTION_COMPUTE_PRO)
      call ProjectDialog(.false.)
	  case(IDC_ACTION_MOVIE_SCRIPT)
      call MovieDialog(.false.)
	  case(IDC_ACTION_GENERATE_MOVIE)
      call TSaveMovie(.true.)
	  end select
  end if
  END SUBROUTINE ActionSub

