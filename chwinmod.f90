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
MODULE CHWIN

! handling of graphic windows

  USE CHDAT

  SAVE

  Character(20), Parameter :: CHWinIdent=' CHWIN Version 1.0  ',CHPalIdent=' CHPAL Version 1.0  '
  Real(8), Parameter :: WinXminD=-1.0_8,WinXmaxD=1.0_8,WinYminD=-1.0_8,WinYmaxD=1.0_8
  Integer(4), Parameter :: iWinCGridD=230_4
  Integer(2), Parameter :: mWin=9_2,kWinD=1_2, &
      iWinWidthD=400_2,iWinHeightD=400_2,iWinXLabD=11_2,iWinYLabD=11_2, &
      iWinLeftD=65_2,iWinBottomD=45_2,iWinTopD=10_2,iWinRightD=10_2, &
      iWinXGridD=-10_2,iWinYGridD=-10_2
  Integer(2), Parameter :: iWinXLogD=1_2,iWinYLogD=1_2, &
      iWinPalD=0_2
  Integer(4) iWinCR(0:10,mWin),iWinCG(0:10,mWin),iWinCB(0:10,mWin), &
      iWinCGrid(mWin),kWinDisp,iColorPalette,icPalette(0:235)
  Integer(2) kWin,nWin,iWinAction, &
      iWinWidth(mWin),iWinHeight(mWin),iWinXLab(mWin),iWinYLab(mWin), &
      iWinLeft(mWin),iWinBottom(mWin),iWinTop(mWin),iWinRight(mWin), &
      iWinXGrid(mWin),iWinYGrid(mWin)
  Integer(2) iWinXLog(mWin),iWinYLog(mWin),iWinPal(mWin)
  Logical(4) lDrawRange,lWinInit
  Logical(1) lWinOpen(0:mWin),lWinFld(0:mWin)
  Real(8) WinXmin(mWin),WinXmax(mWin),WinYmin(mWin),WinYmax(mWin)
  Real(8) viewPlane(3,0:3),viewDist,rEye(3)
  Character(256) PalFileName,WinFileName
  Character(256) WinTitle(mWin)
  Type(windowconfig) drwScreen
  Type(T_RTL_CRITICAL_SECTION) DrawLock

  CONTAINS

! Default setting

  Subroutine Window_Defaults(lCheck)
! set the default data for the current graphic window
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    Integer(4) lf
    lWinFld(0)=.true.
    lWinFld(nWin+1:mWin)=.true.
    lWinOpen(0)=.false.
    lWinOpen(nWin+1:mWin)=.false.
    ldum=lCheck
    iWinAction=0_2
    lDrawRange=.true.
    lDrawRange=.false.
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
    call setNameExt(ProFileName,'PAL',PalFileName)
    call setNameExt(ProFileName,'WIN',WinFileName)
    iWinWidth(kWin)=iWinWidthD
    iWinHeight(kWin)=iWinHeightD
    iWinXLab(kWin)=iWinXLabD
    iWinYLab(kWin)=iWinYLabD
    iWinXLog(kWin)=iWinXLogD
    iWinYLog(kWin)=iWinYLogD
    iWinLeft(kWin)=iWinLeftD
    iWinRight(kWin)=iWinRightD
    iWinTop(kWin)=iWinTopD
    iWinBottom(kWin)=iWinBottomD
    iWinXGrid(kWin)=iWinXGridD
    iWinYGrid(kWin)=iWinYGridD
    iWinCGrid(kWin)=iWinCGridD
    iWinPal(kWin)=iWinPalD
	  WinXmin(kWin)=WinXminD
	  WinYmin(kWin)=WinYminD
	  WinXmax(kWin)=WinXmaxD
	  WinYmax(kWin)=WinYmaxD
    lf=min(GetSLength(ProFileName),200)
    WinTitle(kWin)=ProFileName(1:lf)//char(0)
    call ColorPalette(.false.)
    lWinInit=.true.
  end Subroutine Window_Defaults

  Subroutine ColorPalette(lCheck)
! generate the default color palette
    Implicit none
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    iWinCR(0,kWin)=0_4
    iWinCG(0,kWin)=0_4
    iWinCB(0,kWin)=0_4
    iWinCR(1,kWin)=0_4
    iWinCG(1,kWin)=58_4
    iWinCB(1,kWin)=127_4
    iWinCR(2,kWin)=0_4
    iWinCG(2,kWin)=25_4
    iWinCB(2,kWin)=191_4
    iWinCR(3,kWin)=127_4
    iWinCG(3,kWin)=0_4
    iWinCB(3,kWin)=191_4
    iWinCR(4,kWin)=191_4
    iWinCG(4,kWin)=0_4
    iWinCB(4,kWin)=127_4
    iWinCR(5,kWin)=244_4
    iWinCG(5,kWin)=0_4
    iWinCB(5,kWin)=0_4
    iWinCR(6,kWin)=255_4
    iWinCG(6,kWin)=127_4
    iWinCB(6,kWin)=0_4
    iWinCR(7,kWin)=255_4
    iWinCG(7,kWin)=191_4
    iWinCB(7,kWin)=0_4
    iWinCR(8,kWin)=255_4
    iWinCG(8,kWin)=240_4
    iWinCB(8,kWin)=64_4
    iWinCR(9,kWin)=255_4
    iWinCG(9,kWin)=255_4
    iWinCB(9,kWin)=165_4
    iWinCR(10,kWin)=255_4
    iWinCG(10,kWin)=255_4
    iWinCB(10,kWin)=255_4
    if(lCheck) idum=MessageBoxQQ('Initialize window to setup new palette!'C,'Set color palette'C, &
                                 MB$OK.or.MB$ICONEXCLAMATION)
  end Subroutine ColorPalette

  Subroutine GrayPalette(lCheck)
! generate the default gray palette
    Implicit none
    Integer(2) i
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    do i=0_2,10_2
      iWinCR(i,kWin)=i*25_4
      iWinCG(i,kWin)=i*25_4
      iWinCB(i,kWin)=i*25_4
    end do
    if(lCheck) idum=MessageBoxQQ('Initialize window to setup new palette!'C,'Set gray palette'C, &
                                 MB$OK.or.MB$ICONEXCLAMATION)
  end Subroutine GrayPalette

! I/O

  Subroutine SaveWindow(lCheck)
! save the data of the current graphic window in a file
    Implicit none
    Integer(4) iOK,ios,idum
    Logical, intent(in) :: lCheck
    if(.not.lCheck) then
      call Open2write(-1,'Select window data file to be written!','Window data file ',WinFileName,'WIN',ios)
      if(ios.gt.0) return
    end if
    open(1,file=WinFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save window'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHWinIdent,iOK)
    call WriteStr(1,WinTitle(kWin),iOK)
    rch(1)=WinXmin(kWin)
    rch(2)=WinXmax(kWin)
    rch(3)=WinYmin(kWin)
    rch(4)=WinYmax(kWin)
    call chwrit2(1,ich,0,rch,4,sch,0,iOK)
    ich(1)=iWinLeft(kWin)
    ich(2)=iWinWidth(kWin)
    ich(3)=iWinRight(kWin)
    ich(4)=iWinTop(kWin)
    ich(5)=iWinHeight(kWin)
    ich(6)=iWinBottom(kWin)
    call chwrit2(1,ich,6,rch,0,sch,0,iOK)
    ich(1)=iWinXLab(kWin)
    ich(2)=iWinXLog(kWin)
    ich(3)=iWinXGrid(kWin)
    ich(4)=iWinYLab(kWin)
    ich(5)=iWinYLog(kWin)
    ich(6)=iWinYGrid(kWin)
    call chwrit2(1,ich,6,rch,0,sch,0,iOK)
    ich(1)=iWinCGrid(kWin)
    ich(2)=iWinPal(kWin)
    call chwrit2(1,ich,2,rch,0,sch,0,iOK)
    ich(1)=0
    if(lWinFld(kWin)) ich(1)=1
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveWindow

  Subroutine OpenWindow(lCheck)
! read the window data from a file
    Implicit none
    Integer(4) ier,ios,idum
    Logical, intent(in) :: lCheck
    Logical lFileExist
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select window data file to be read!','Window data file ',WinFileName,'WIN',ios)
      if(ios.gt.0) return
    end if
    inquire(file=WinFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=WinFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open window'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,ier)
    if(ier.ne.0) goto 999
    if(CHWinIdent(1:18).ne.text(1:18)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open window'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call ReadStr(1,WinTitle(kWin),ier)
    if(ier.ne.0) goto 999
    lWinInit=.true.
    call chread2(1,ich,0,rch,4,ier)
    if(ier.ne.0) goto 999
    WinXmin(kWin)=rch(1)
    WinXmax(kWin)=rch(2)
    WinYmin(kWin)=rch(3)
    WinYmax(kWin)=rch(4)
    call chread2(1,ich,6,rch,0,ier)
    if(ier.ne.0) goto 999
    iWinLeft(kWin)=ich(1)
    iWinWidth(kWin)=ich(2)
    iWinRight(kWin)=ich(3)
    iWinTop(kWin)=ich(4)
    iWinHeight(kWin)=ich(5)
    iWinBottom(kWin)=ich(6)
    call chread2(1,ich,6,rch,0,ier)
    if(ier.ne.0) goto 999
    iWinXLab(kWin)=ich(1)
    iWinXLog(kWin)=ich(2)
    iWinXGrid(kWin)=ich(3)
    iWinYLab(kWin)=ich(4)
    iWinYLog(kWin)=ich(5)
    iWinYGrid(kWin)=ich(6)
    call chread2(1,ich,2,rch,0,ier)
    if(ier.ne.0) goto 999
    iWinCGrid(kWin)=ich(1)
    iWinPal(kWin)=ich(2)
    call chread2(1,ich,1,rch,0,ier)
    lWinFld(kWin)=.false.
    if((ier.ne.0).or.(ich(1).ne.0)) lWinFld(kWin)=.true.
    close(1)
    call InitWindow(.false.)
    return
    999 continue
    idum=MessageBoxQQ('Error reading input file!'C,'Open window'C, &
                      MB$OK.or.MB$ICONSTOP)
  	close(1)
  end Subroutine OpenWindow

  Subroutine OpenPalette(lCheck)
! read the palette data from a file
    Implicit none
    Integer(4) idum,i,ios,iEr
    Logical(4), intent(in) :: lCheck
    Logical(4) lFileExist
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select palette data file to be read!','Palette data file ',PalFileName,'PAL',ios)
      if(ios.gt.0) return
    end if
    inquire(file=PalFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=PalFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open palette'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    iWinPal=0_2
    call ReadStr(1,text,ier)
    if(ier.ne.0) goto 999
    if(CHPalIdent(1:18).ne.text(1:18)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open palette'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    do i=0_4,10_4
      call chread2(1,ich,3,rch,0,ier)
      if(ier.ne.0) goto 999
      iWinCR(i,kWin)=ich(1)
      iWinCG(i,kWin)=ich(2)
      iWinCB(i,kWin)=ich(3)
    end do
    close(1)
    lWinInit=.true.
    if(.not.lCheck) idum=MessageBoxQQ('Initialize window to setup new palette!'C,'Open palette'C, &
                         MB$OK.or.MB$ICONEXCLAMATION)
    return
    999 continue
    idum=MessageBoxQQ('Error reading input file!'C,'Open palette'C, &
                      MB$OK.or.MB$ICONSTOP)
  	close(1)
  end Subroutine OpenPalette

! Window handling

  Subroutine GetKWin(lFocus)
! get the active graphic window - if lFocus=true: get the window having the focus
    Implicit none
    Integer(4) idum,iunit
    Logical, intent(in) :: lFocus
    iunit=kWin
    idum=1
    if(lFocus) then
      idum=InqFocusQQ(iunit)
      if(idum.eq.0) then
        idum=SetActiveQQ(iunit)
        iunit=iunit-10_4
      end if
    else
      idum=GetActiveQQ()
      if(idum.ne.QWIN$NOACTIVEWINDOW) iunit=idum-10_4
    end if
    if((iunit.gt.0).and.(iunit.le.mWin)) kWin=iunit
    rEye=ViewPlane(1:3,0)+ &
    &    0.5d0*(WinXmin(kWin)+WinXmax(kWin))*ViewPlane(1:3,1)+ &
    &    0.5d0*(WinYmin(kWin)+WinYmax(kWin))*ViewPlane(1:3,2)+ &
    &    ViewDist*Unit3DVec(ViewPlane(1:3,3))
  end Subroutine GetKWin

  Subroutine InitWindow(lCheck)
! initialize a graphic window
    Implicit none
    Integer(4) i,ii,ired,igreen,iblue,idum
    Integer(2) i1
    Logical(4) ldum,lCheck
    Type(xycoord) xy
! clear and set viewport
    if(lCheck) call GetKWin(.false.)
    idum=FocusQQ(10+kWin)
    if(idum.ne.0) then
      idum=MessageBoxQQ('FocusQQ failed!'C,'Initialize window'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    end if
    idum=SetActiveQQ(10+kWin)
    i=min(64,GetSLength(WinTitle(kWin)))
    ldum=GetWindowConfig(drwScreen)
    ii=GetSLength(drwScreen%title)
    if(ii.eq.i) then
      if(drwScreen%title(1:i).ne.WinTitle(kWin)(1:i)) ii=i-1
    end if
    if((.not.ldum).or.(drwScreen%numxpixels.ne.iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)).or. &
    &                 (drwScreen%numypixels.ne.iWinHeight(kWin)+iWinBottom(kWin)+iWinTop(kWin)).or. &
    &                 (i.ne.ii)) then
      i1=0
      drwScreen%numxpixels =iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)+i1
      drwScreen%numypixels =iWinHeight(kWin)+iWinBottom(kWin)+iWinTop(kWin)+i1
      drwScreen%numtextcols=-1
      drwScreen%numtextrows=-1
      drwScreen%numcolors  =-1
      drwScreen%fontsize   =-1
      drwScreen%title(1:i) =WinTitle(kWin)(1:i)
      drwScreen%title(i+1:i+1)=Char(0)
      ldum=SetWindowConfig(drwScreen)
      if(.not.ldum) ldum=SetWindowConfig(drwScreen)
    end if
    if(drwScreen%numcolors.ne.256_2) then
      if(l4) write(*,*) 'current number of colors=',drwScreen%numcolors
      idum=MessageBoxQQ('Display not set to 256 colors!\r Movies (AVI files) will be big!'C,&
                        &'Initialize window'C,MB$OK.or.MB$ICONSTOP)
    end if
    if(drwScreen%numcolors.gt.250_2) then
! set palette
      icPalette(0_4)=#FFFFFF
      icPalette(1_4)=#000000
      icPalette(2_4)=#0000FF
      icPalette(3_4)=#00FF00
      icPalette(4_4)=#FF0000
      icPalette(5_4)=#00FFFF
      icPalette(6_4)=#FF00FF
      icPalette(7_4)=#FFFF00
      icPalette(8_4)=#FFFFFF
      icPalette(9_4)=#000077
      icPalette(10_4)=#007700
      icPalette(11_4)=#770000
      icPalette(12_4)=#007777
      icPalette(13_4)=#770077
      icPalette(14_4)=#777700
      icPalette(15_4)=#777777
      do i=16_4,115_4
        ii=(i-16_4)/10_4
		    ired=  iWinCR(ii,kWin)+(iWinCR(ii+1_4,kWin)-iWinCR(ii,kWin))*(i-10_4*ii-16_4)/10_4
		    igreen=iWinCG(ii,kWin)+(iWinCG(ii+1_4,kWin)-iWinCG(ii,kWin))*(i-10_4*ii-16_4)/10_4
		    iblue= iWinCB(ii,kWin)+(iWinCB(ii+1_4,kWin)-iWinCB(ii,kWin))*(i-10_4*ii-16_4)/10_4
		    ired=min(255_4,max(0_4,ired))
		    igreen=min(255_4,max(0_4,igreen))
		    iblue=min(255_4,max(0_4,iblue))
		    icPalette(i)=RGBToInteger(ired,igreen,iblue)
		    icPalette(i+100)=RGBToInteger(255_4-ired,255_4-igreen,255_4-iblue)
      end do
      do i=216_4,235_4
        ired=(i-215_4)*12_4
        igreen=ired
        iblue=ired
		    icPalette(i)=RGBToInteger(ired,igreen,iblue)
      end do
      iColorPalette=RemapAllPaletteRGB(icPalette(0))
      iColorPalette=RemapAllPaletteRGB(icPalette(0))
    end if
! set colors, clear screen
    idum=SetBKColor(0_4)
    idum=SetColor(1_4)
    idum=SetBKColor(0_4)
    call ClearScreen($GCLEARSCREEN)
! set viewport, clip region
    call SetViewPort(0_2,0_2,drwScreen%numxpixels-1_2,drwScreen%numypixels-1_2)
    call SetViewOrg(iWinLeft(kWin),iWinTop(kWin),xy)
    call SetClipRgn(iWinLeft(kWin)+1_2,iWinTop(kWin)+1_2,iWinWidth(kWin)+iWinLeft(kWin)-1_2,iWinHeight(kWin)+iWinTop(kWin)-1_2)
  end Subroutine InitWindow

  Subroutine AddWindow(lCheck)
! add a new graphic window with default data
    Implicit none
    Integer(4) iunit,idum,ievent
    Integer(2) kW
    Logical lCheck,ldum
    External MouseAction
    ldum=lCheck
    iWinAction=0_2
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
    do kW=1,mWin
      if(.not.lWinOpen(kW)) then
        iunit=kW+10_4
        open(unit=iunit,file='user',title=' ',iostat=idum)
        if(idum.eq.0) then
          kWin=kW
          lWinOpen(kWin)=.true.
          nWin=nWin+1
! register mouse events
          ievent=MOUSE$MOVE
          ievent=ior(ievent,MOUSE$LBUTTONUP)
          ievent=ior(ievent,MOUSE$LBUTTONDOWN)
          ievent=ior(ievent,MOUSE$RBUTTONUP)
          ievent=ior(ievent,MOUSE$RBUTTONDOWN)
          idum=RegisterMouseEvent(10_4+Int4(kWin),ievent,MouseAction)
          call Window_Defaults(.true.)
          call InitWindow(.false.)
        else
          idum=MessageBoxQQ('Cannot open unit!'C,'Add window'C, &
                            MB$OK.or.MB$IconHand)
        end if
        return
      end if
    end do
    idum=MessageBoxQQ('Cannot add new window!'C,'Add window'C, &
                      MB$OK.or.MB$ICONSTOP)
  end Subroutine AddWindow

  Subroutine MTAddWindow(lCheck)
! add a new graphic window
    Implicit none
    Logical, intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
    call AddWindow(lCheck)
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTAddWindow

  Subroutine DeleteWindow(lCheck)
! delete the current graphic window (if lCheck=false: Window that has the focus is deleted)
    Implicit none
    Integer(4) iunit,idum,ievent
    Integer(2) kW
    Logical, intent(in) :: lCheck
    Logical ldum
    External MouseAction
    ldum=lCheck
    iWinAction=0_2
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !objects
    ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
    call GetkWin(.not.lCheck)
    if((nWin.lt.2).or.(kWin.lt.1).or.(kWin.gt.mWin).or.(.not.lWinOpen(kWin))) then
      idum=MessageBoxQQ('Cannot delete window!'C,'Delete window'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    end if
    lWinOpen(kWin)=.false.
    iunit=kWin+10_4
    close(iunit)
! unregister mouse events
    ievent=MOUSE$MOVE
    ievent=ior(ievent,MOUSE$LBUTTONUP)
    ievent=ior(ievent,MOUSE$LBUTTONDOWN)
    ievent=ior(ievent,MOUSE$RBUTTONUP)
    ievent=ior(ievent,MOUSE$RBUTTONDOWN)
    idum=UnRegisterMouseEvent(iunit,ievent)
    nWin=nWin-1
! make another window active
    do kW=1,mWin
      if(lWinOpen(kW)) then
        kWin=kW
        Exit
      end if
    end do
    idum=FocusQQ(10+kWin)
    idum=SetActiveQQ(10+kWin)
  end Subroutine DeleteWindow

  Subroutine BlowWindows(kPl,f)
    Implicit none
    Real(8) f
    Integer(4) kPl,kP,k1,k2
    if(kPl.lt.1) then
      if(kPl.eq.0) then
        k1=1
        k2=Int4(nWin)
      else
        k1=1
        k2=Min(Int4(nWin),-kPl)
      end if
    else
      if(kPl.gt.Int4(nWin)) return
      k1=Min(Int4(nWin),kPl)
      k2=k1
    end if
    do kP=k1,k2
      WinXmin(kP)=WinXmin(kP)*f
      WinXmax(kP)=WinXmax(kP)*f
      WinYmin(kP)=WinYmin(kP)*f
      WinYmax(kP)=WinYmax(kP)*f
    end do
  end Subroutine BlowWindows

  Subroutine ReflWindows(kPl,ix)
    Implicit none
    Integer(4) kPl,kP,k1,k2,ix
    Real(8) r
    if(kPl.lt.1) then
      if(kPl.eq.0) then
        k1=1
        k2=Int4(nWin)
      else
        k1=1
        k2=Min(Int4(nWin),-kPl)
      end if
    else
      if(kPl.gt.Int4(nWin)) return
      k1=Min(Int4(nWin),kPl)
      k2=k1
    end if
    do kP=k1,k2
      if(ix.eq.1) then
        r=WinXmin(kP)
        WinXmin(kP)=-WinXmax(kP)
        WinXmax(kP)=-r
      else
        r=WinYmin(kP)
        WinYmin(kP)=-WinYmax(kP)
        WinYmax(kP)=-r
      end if
    end do
  end Subroutine ReflWindows

  Subroutine MoveWindows(kPl,dx,dy)
    Implicit none
    Real(8) dx,dy
    Integer(4) kPl,kP,k1,k2
    if(kPl.lt.1) then
      if(kPl.eq.0) then
        k1=1
        k2=Int4(nWin)
      else
        k1=1
        k2=Min(Int4(nWin),-kPl)
      end if
    else
      if(kPl.gt.Int4(nWin)) return
      k1=Min(Int4(nWin),kPl)
      k2=k1
    end if
    do kP=k1,k2
      WinXmin(kP)=WinXmin(kP)+dx
      WinXmax(kP)=WinXmax(kP)+dx
      WinYmin(kP)=WinYmin(kP)+dy
      WinYmax(kP)=WinYmax(kP)+dy
    end do
  end Subroutine MoveWindows

! coordinate transforms

  Subroutine R2I(x,y,ix,iy)
! conversion Real to Integer coordinates
    Implicit none
    Real(8) x,y,r
    Integer(2) ix,iy
    if(iabs(iWinXLog(kWin)).gt.1_2) then
      if((x.gt.pSmall).and.(WinXmin(kWin).gt.pSmall).and.(WinXmax(kWin).gt.pSmall)) then
        r=(log(x)-log(WinXmin(kWin)))*dble(iWinWidth(kWin))/(log(WinXmax(kWin))-log(WinXmin(kWin)))
        ix=nint(max(min(r,32767.0d0),-32767.0d0),2)
      else
        ix=iWinLeft(kWin)
      end if
    else
      r=(x-WinXmin(kWin))*dble(iWinWidth(kWin))/(WinXmax(kWin)-WinXmin(kWin))
      ix=nint(max(min(r,32767.0d0),-32767.0d0),2)
    end if
    if(iabs(iWinYLog(kWin)).gt.1_2) then
      if((y.gt.pSmall).and.(WinYmin(kWin).gt.pSmall).and.(WinYmax(kWin).gt.pSmall)) then
        r=(log(WinYmax(kWin))-log(y))*dble(iWinHeight(kWin))/(log(WinYmax(kWin))-log(WinYmin(kWin)))
        iy=nint(max(min(r,32767.0d0),-32767.0d0),2)
      else
        iy=iWinHeight(kWin)+iWinTop(kWin)
      end if
    else
      r=(WinYmax(kWin)-y)*dble(iWinHeight(kWin))/(WinYmax(kWin)-WinYmin(kWin))
      iy=nint(max(min(r,32767.0d0),-32767.0d0),2)
    end if
  end Subroutine R2I

  Subroutine I2R(ix,iy,x,y)
! conversion Integer to Real coordinates
    Implicit none
    Real(8) x,y,q,e
    Integer(2) ix,iy
    Integer(4) idum
    if(iabs(iWinXLog(kWin)).gt.1_2) then
      if(dabs(WinXmin(kWin)).lt.pSmall) then
        WinXmin(kWin)=pSmall
        idum=MessageBoxQQ('WinXmin too small!\r Set to 1e-300'C,'Integer to real coordinates'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
      q=dabs(WinXmax(kWin)/WinXmin(kWin))
      if(q.lt.pSmall) then
        q=pSmall
        idum=MessageBoxQQ('Q in x direction too small!\r Set to 1e-300'C,'Integer to real coordinates'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
      e=dabs(dble(ix)/dble(iWinWidth(kWin)))
      q=dexp(dlog(q)*e)
      x=dabs(WinXmin(kWin)*q)
    else
      x=WinXmin(kWin)+dble(ix)*(WinXmax(kWin)-WinXmin(kWin))/dble(iWinWidth(kWin))
    end if
    if(iabs(iWinYLog(kWin)).gt.1_2) then
      if(dabs(WinYmin(kWin)).lt.pSmall) then
        WinYmin(kWin)=pSmall
        idum=MessageBoxQQ('WinYmin too small!\r Set to 1e-300'C,'Integer to real coordinates'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
      q=dabs(WinYmin(kWin)/WinYmax(kWin))
      if(q.lt.pSmall) then
        q=pSmall
        idum=MessageBoxQQ('Q in y direction too small!\r Set to 1e-300'C,'Integer to real coordinates'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
      end if
      e=dabs(dble(iy)/dble(iWinHeight(kWin)))
      q=q**e
      y=dabs(WinYmax(kWin)*q)
    else
      y=WinYmax(kWin)-dble(iy)*(WinYmax(kWin)-WinYmin(kWin))/dble(iWinHeight(kWin))
    end if
  end Subroutine I2R

END MODULE CHWIN




