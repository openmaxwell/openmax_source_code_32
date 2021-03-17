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
MODULE CHDLG
! compiler dependent libraries: Modify for using old Compaq Visual Fortran
  USE IFLOGM !IVF uses this libraray
  USE IFWIN !IVF uses this libraray - alternative for IFWIN
  USE IFQWIN !IVF uses this libraray
  USE IFPORT !IVF uses this libraray
  !CVF USE DFLOGM !CVF uses this libraray
  !CVF USE DFWIN  !CVF uses this libraray
  !CVF USE DFLIB  !CVF uses this libraray
! Modeless dialogs, file dialogs, memory leaks

  Save

  Integer(4), Parameter :: mMLDlg=10
  Integer(4) iMLDlg(mMLDlg),ich(1000)
  Integer(INT_PTR_KIND()) iThreadHandle,iOutCheck1,iOutCheck2,iOutCheck3,iOutCheck4,iOutCheck5,iOutCheck6,iOutCheck7
  Integer(4) iThreadAction,iDlgExit,iMovVarIn,iMovVarOut1,iMovVarOut2,iMovVarErr,iMovVar(0:999)
  Integer(2) iWriteDigits,iSpaceRead,iPoints2DRead,iPoints3DRead
  Real(8) space(3,0:3),SpaceAngle,Points2D(2,3),Points3D(3,3),rch(1000),rMovVar(0:999)
  Logical lStopThread,lThreadStarted,lMatErr,l4,l5,l6,l7,lAutoStart
  Character(32) SpaceText,Points2DText,Points3DText
  Character(256) sch
  Real(8) pSmall,nSmall,pBig,nBig

  Public
  type(dialog) mousedlg,outputdlg,actiondlg

contains

  subroutine Cursor(begin,id)
! Show Cursor (begin = .true.) or restore the standard Cursor(begin = .false.).
! Predefined: id=
! IDC_APPSTARTING	Standard arrow and small hourglass
! IDC_ARROW	        Standard arrow
! IDC_CROSS	        Crosshair
! IDC_WAIT	        Hourglass
  implicit none
  logical, intent(in) :: begin
  integer(INT_PTR_KIND()) id,idum,i0
  integer(4), static :: CursorRefCount = 0
  type(T_POINT) cursorPos
  logical(4) lfoo
  i0=0
  if(begin) then
    if(CursorRefCount==0) then
      Select Case(id)
      case(IDC_APPSTARTING,IDC_ARROW,IDC_CROSS,IDC_WAIT)
        idum=LoadCursor(i0,id)
        lfoo=SetCursor(idum)
      case Default
        idum=LoadCursor(GetModuleHandle(Null),id)
        lfoo=SetCursor(idum)
      end Select
    endif
    CursorRefCount=CursorRefCount+1
  else
    CursorRefCount=CursorRefCount-1
    if(CursorRefCount==0) then
      lfoo=GetCursorPos(cursorPos)
      lfoo=SetCursorPos(cursorPos%x, cursorPos%y)
    endif
  endif
  end subroutine Cursor

  subroutine InitIconButton(Dlg,IDButton,IDIcon)
! put the icon IDIcon on the button IDButton
  implicit none
  include 'resource.fd'
  integer(INT_PTR_KIND()), intent(in) :: Dlg,IDIcon
  integer(4), intent(in) :: IDButton
  integer(INT_PTR_KIND()) hwndButton,idum,hInstance,hIcon
  hInstance=GetModuleHandle(NULL)
  hIcon=LoadIcon(hInstance,IDIcon)
  hwndButton=GetDlgItem(Dlg,IDButton)
  idum=SendMessage(hwndButton,BM_SETIMAGE,IMAGE_ICON,hIcon)
  end subroutine InitIconButton

  subroutine IntToStr(i,iwidth,ipadZeroes,str,lout)
    Integer(4) i,iwidth,ipadZeroes
    Character(*) str
    Integer(4) lout
    lout=iwidth
    if(ipadZeroes.lt.1) then
      select case(iwidth)
      case(1)
        write(str,'(1I1)') i
      case(2)
        write(str,'(1I2)') i
      case(3)
        write(str,'(1I3)') i
      case(4)
        write(str,'(1I4)') i
      case(5)
        write(str,'(1I5)') i
      case(6)
        write(str,'(1I6)') i
      case(7)
        write(str,'(1I7)') i
      case(8)
        write(str,'(1I8)') i
      case(9)
        write(str,'(1I9)') i
      case default
        write(str,'(1I10)') i
      end select
      lout=GetSLength(str)
      call DelBlanks(str,lout)
    else
      select case(iwidth)
      case(1)
        write(str,'(1I1.1)') i
      case(2)
        write(str,'(1I2.2)') i
      case(3)
        write(str,'(1I3.3)') i
      case(4)
        write(str,'(1I4.4)') i
      case(5)
        write(str,'(1I5.5)') i
      case(6)
        write(str,'(1I6.6)') i
      case(7)
        write(str,'(1I7.7)') i
      case(8)
        write(str,'(1I8.8)') i
      case(9)
        write(str,'(1I9.9)') i
      case default
        write(str,'(1I3.3)') i
      end select
      lout=iwidth
    end if
  end subroutine IntToStr

  subroutine RealToStr(r,iwidth,iprecision,str,lout)
    Real(8) r
    Integer(4) iwidth,iprecision,ip,iOK
    Character(*) str
    Integer(4) lout
    ip=iprecision
    if(iwidth.gt.0) ip=iwidth-8
    select case(ip)
    case(0)
      write(str,'(1PE8.0E3)') r
    case(1)
      write(str,'(1PE9.1E3)') r
    case(2)
      write(str,'(1PE10.2E3)') r
    case(3)
      write(str,'(1PE11.3E3)') r
    case(4)
      write(str,'(1PE12.4E3)') r
    case(5)
      write(str,'(1PE13.5E3)') r
    case(6)
      write(str,'(1PE14.6E3)') r
    case(7)
      write(str,'(1PE15.7E3)') r
    case(8)
      write(str,'(1PE16.8E3)') r
    case(9)
      write(str,'(1PE17.9E3)') r
    case(10)
      write(str,'(1PE18.10E3)') r
    case(11)
      write(str,'(1PE19.11E3)') r
    case(12)
      write(str,'(1PE20.12E3)') r
    case(13)
      write(str,'(1PE21.13E3)') r
    case default
      write(str,'(1PE22.14E3)',iostat=iOK) r
      if(iOK.ne.0) then
        write(*,*) 'Unhandled problem in RealToStr!'
        write(str,'(1PE18.10E3)') r
      end if
    end select
    lout=GetSLength(str)
    if((iwidth.lt.lout).and.(iwidth.gt.0)) then
      write(*,*) 'Unhandled exception in RealToStr!'
    end if
  end subroutine RealToStr

  subroutine StrToInt(str,i,iOK)
    Character(*) str
    Integer(4) i,iOK,l
    l=GetSLength(str)
    if(l.lt.1) then
      iOK=-1
      i=0
      return
    end if
    if(str(l:l).eq.Char(0)) l=l-1
    if(l.lt.1) then
      iOK=-1
      i=0
      return
    end if
    read(str(1:l),*,iostat=iOK) i
    if((iOK.eq.0).and.(ichar(str(1:1)).gt.64).and.(ichar(str(1:1)).lt.123)) iOK=999 ! first character f or t would be OK
  end subroutine StrToInt
  
  subroutine StrToRea(str,r,iOK)
    Character(*) str
    Real(8) r
    Integer(4) iOK,i,l
    l=GetSLength(str)
    if(l.lt.1) then
      iOK=-1
      r=0.0d0
      return
    end if
    if(str(l:l).eq.Char(0)) l=l-1
    if(l.lt.1) then
      iOK=-1
      r=0.0d0
      return
    end if
    if((ichar(str(1:1)).gt.64).and.(ichar(str(1:1)).lt.123)) then ! first character f or t would be OK for fortran iostat
      iOK=999
      return
    end if
    read(str(1:l),*,iostat=iOK) r
    if(iOK.ne.0) then ! might be an iteger value that causes the iostat error
      read(str(1:l),*,iostat=iOK) i
      r=Dble(i)
    end if
  end subroutine StrToRea

  Subroutine StopThread()
! set stop flag for stopping the current thread
    Implicit none
    Logical ldu
    Integer(4) iUnit,idum
    if(.not.lThreadStarted) return
    lStopThread=.true.
		call OutTxt('t3','Stop thread')
    do iUnit=1,10
      if((iUnit.gt.4).and.(iUnit.lt.7)) Cycle ! 5: keyboard in, 6: screen out
		  call SleepQQ(10_4)
      inquire(unit=iUnit,opened=ldu)
      if(ldu) then
        write(iUnit,'(1H )',iostat=idum)
        EndFile(iUnit)
        close(iUnit)
      end if
    end do
		call OutTxt('t3','Thread stopped')
  end Subroutine StopThread

  Subroutine WaitEndThread()
! wait until the current thread ends
    Implicit none
    Integer(4) it
    it=1_4
    do
      if(.not.lThreadStarted) Exit
		  call SleepQQ(it)
      it=min(it*2_2,1000_4)
    end do
  end Subroutine WaitEndThread

  Subroutine EndThread()
! end of the current thread
    Implicit none
    if(.not.lThreadStarted) return
    lThreadStarted=.false.
    lStopThread=.false.
    call StopThread()
    call WaitEndThread()
		call OutTxt('t3','End thread')
  end Subroutine EndThread

! items of modeless dialogs

  logical(4) function EnableDlg(Dlg, lEnable)
    implicit none
    integer(INT_PTR_KIND()), intent(in) :: Dlg
    logical(4), intent(in) :: lEnable
    Integer i
    i=lEnable
    EnableDlg = EnableWindow(dlg,i)
  end function EnableDlg

  logical(4) function EnableDlgItem(Dlg, id, lEnable)
    implicit none
    integer(INT_PTR_KIND()), intent(in) :: Dlg
    integer(4), intent(in) :: id
    logical(4), intent(in) :: lEnable
    integer(INT_PTR_KIND()) hwnd
    Integer i
    i=lEnable
    hwnd = GetDlgItem(Dlg, id)
    EnableDlgItem = EnableWindow(hwnd,i)
  end function EnableDlgItem

  integer(4) function GetCheck(Dlg, id)
! radio buttons and check boxes
    implicit none
    integer(INT_PTR_KIND()), intent(in) :: Dlg
    integer(4), intent(in) :: id
    integer(INT_PTR_KIND()) hwndButton
    hwndButton = GetDlgItem(Dlg, id)
    GetCheck = SendMessage(hwndButton, BM_GETCHECK, 0, 0)
  end function GetCheck

  subroutine SetCheck(Dlg, id, check)
! radio buttons and check boxes
    implicit none
    integer(INT_PTR_KIND()), intent(in) :: Dlg
    integer(4), intent(in) :: id
    integer(INT_PTR_KIND()), intent(in) :: check
    integer(INT_PTR_KIND()) hwndButton
    integer(4) iFoo
    hwndButton = GetDlgItem(Dlg, id)
    iFoo = SendMessage(hwndButton, BM_SETCHECK, check, 0)
  end subroutine SetCheck

  subroutine OutTxt(s,str)
! set text in one of the output dialog text boxes
    implicit none
    include 'resource.fd'
    character(2), intent(in) :: s
    character(*), intent(in) :: str
    integer(4) iFoo,idum
    iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK4)
    if(iFoo.ne.0_4) then
      l4=.true.
    else
      l4=.false.
    end if
    iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK5)
    if(iFoo.ne.0_4) then
      l5=.true.
    else
      l5=.false.
    end if
    iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK6)
    if(iFoo.ne.0_4) then
      l6=.false.
    else
      l6=.true.
    end if
    iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK7)
    if(iFoo.ne.0_4) then
      l7=.false.
    else
      l7=.true.
    end if
    if(s(1:1).eq.'!') return
    select case(s)
    case('t1','T1')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK1)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT1,str//Char(0))
    case('n1','N1')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK1)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_N1,str//Char(0))
    case('m1','M1')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK1)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_M1,str//Char(0))
    case('t2','T2')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK2)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT2,str//Char(0))
    case('n2','N2')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK2)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_N2,str//Char(0))
    case('m2','M2')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK2)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_M2,str//Char(0))
    case('t3','T3')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK3)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT3,str//Char(0))
    case('n3','N3')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK3)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_N3,str//Char(0))
    case('m3','M3')
      iFoo=getCheck(outputdlg%hwnd,IDC_OUT_CHECK3)
      if(iFoo.ne.0_4) idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_M3,str//Char(0))
    end select
  end subroutine OutTxt

  subroutine SetSel(dlg, id, istart, iend)
!  Nur eine Hilfsfunktion zum Setzen der Selektion in Edit-Feldern
    implicit none
    integer(INT_PTR_KIND()), intent(in) :: dlg
    integer(4), intent(in) :: id
    integer(INT_PTR_KIND()), intent(in) :: istart
    integer(INT_PTR_KIND()), intent(in) :: iend
    integer(INT_PTR_KIND()) hwndEdit
    integer(4) iFoo
    hwndEdit = GetDlgItem(dlg, id)
    iFoo = SendMessage(hwndEdit, EM_SETSEL, istart, iend)
  end subroutine SetSel

  subroutine GetSel(dlg, id, istart, iend)
!  Nur eine Hilfsfunktion zum Ablesen der Selektion in Edit-Feldern
    implicit none
    integer(INT_PTR_KIND()), intent(in) :: dlg
    integer(4), intent(in) :: id
    integer(INT_PTR_KIND()), intent(out) :: istart
    integer(INT_PTR_KIND()), intent(out) :: iend
    integer(INT_PTR_KIND()) hwndEdit
    integer(4) iFoo
    hwndEdit = GetDlgItem(dlg, id)
    iFoo = SendMessage(hwndEdit, EM_GETSEL, NULL, NULL)
    istart = LOWORD(iFoo)
    iend = HIWORD(iFoo)
  end subroutine GetSel

!  file open/save dialogs

  subroutine CHGetFileName(title,filter1,extension,lExist,file_spec,ios)
! display file open dialog with title for getting the file name file_spec
! use filter_spec for file types. lExist defines whether file must exist or not
! returns file_spec and ios (0: no file selected, 1: file selected)
    use comdlg32
    use user32 ! Interface for GetForegroundWindow
    implicit none
    Integer(4) ios
    Logical lExist
! Declare structure used to pass and receive attributes
    type(T_OPENFILENAME) ofn
! Declare filter specification: concatenation of pairs of null-terminated strings.
! The first string in each pair is the file type name, the second is a semicolon-separated list
! of file types for the given name. The list ends with a trailing null-terminated empty string.
    character*(*) filter1,extension
    character*(512):: filter_spec
    character*(256):: file_spec ! file to be opened
    character*(256):: title     ! dialog title
    ios=0
    filter_spec=filter1//'(*.'//Extension//')'//Char(0)//'*.'//Extension// &
	         Char(0)//'All Files (*.*)'//Char(0)//'*.*'//Char(0)
    ofn%lStructSize=SIZEOF(ofn)
    ofn%hwndOwner=GetForegroundWindow()
    ofn%hInstance=NULL  ! For Win32 applications, might be set to the appropriate hInstance
    ofn%lpstrFilter=loc(filter_spec)
    ofn%lpstrCustomFilter=NULL
    ofn%nMaxCustFilter=0
    ofn%nFilterIndex=1 ! Specifies initial filter value
    ofn%lpstrFile=loc(file_spec)
    ofn%nMaxFile=sizeof(file_spec)
    ofn%nMaxFileTitle=0
    ofn%lpstrInitialDir=NULL  ! Use Windows default directory
    ofn%lpstrTitle=loc(title)
    if(lExist) then
      ofn%Flags=IOR(IOR(OFN_EXPLORER,OFN_FILEMUSTEXIST),OFN_HIDEREADONLY)
    else
      ofn%Flags=IOR(IOR(OFN_EXPLORER,OFN_OVERWRITEPROMPT),OFN_HIDEREADONLY)
    end if
    ofn%lpstrDefExt=loc(Extension//Char(0))
    ofn%lpfnHook=NULL
    ofn%lpTemplateName=NULL
    ios=GetOpenFileName(ofn) ! Call GetOpenFileName and check status
  end subroutine CHGetFileName

  Subroutine Open2read(ifnr,Title,filter1,fileName,Extension,ios)
    Implicit none
    integer(4) ifnr,ios
    character*(*) Title,filter1,fileName,Extension
    call CHGetFileName(title,filter1,Extension,.true.,fileName,ios)
    if((ios.gt.0).and.(ifnr.gt.0)) then
      open(ifnr,file=fileName,status='old',action='read',iostat=ios)
    else
      if(ios.gt.0) then
        ios=0
      else
        ios=1
      end if
    end if
  end Subroutine Open2read

  Subroutine Open2write(ifnr,Title,filter1,fileName,Extension,ios)
    Implicit none
    integer(4) ifnr,ios
    character*(*) Title,filter1,fileName,Extension
    call CHGetFileName(title,filter1,Extension,.false.,fileName,ios)
    if((ios.gt.0).and.(ifnr.gt.0)) then
      open(ifnr,file=fileName,status='unknown',action='write',iostat=ios)
    else
      if(ios.gt.0) then
        ios=0
      else
        ios=1
      end if
    end if
  end Subroutine Open2write

! I/O dialogs

  Subroutine DlgSetI(Dlg,idc,idcS,i0,imin,imax)
    Implicit none
    Integer(4), Intent(in):: idc,idcS,i0,imin,imax
    Integer(4) i,lout
	  Logical ldum
	  Type(dialog) Dlg
	  Character(64) text
    i=i0
    if(imax.ge.imin) then
	    i=min(imax,max(imin,i0))
	  end if
    call IntToStr(i,0_4,0_4,text,lout)
	  ldum=DlgSet(Dlg,idc,text)
    if(idcS.gt.0_4) then
	    ldum=DlgSet(Dlg,idcS,1_4,Dlg_RangeMin)
	    ldum=DlgSet(Dlg,idcS,imax-imin+1_4,Dlg_RangeMax)
	    ldum=DlgSet(Dlg,idcS,imax-i+1_4,Dlg_position)
	    ldum=DlgSet(Dlg,idcS,1_4,Dlg_smallstep)
	    ldum=DlgSet(Dlg,idcS,1_4,Dlg_bigstep)
    end if
  end Subroutine DlgSetI

  Subroutine DlgSetR(Dlg,idc,r,idigit)
    Implicit none
    Integer(4), Intent(in):: idc,idigit
    Integer(4) lout
    Real(8), Intent(in):: r
	  Logical ldum
	  Type(dialog) Dlg
	  Character(64) text
    call RealToStr(r,0,idigit,text,lout)
	  ldum=DlgSet(Dlg,idc,text)
  end Subroutine DlgSetR

  Subroutine DlgSetSn(Dlg,idc,s,n,sn)
    Implicit none
    Integer(4), Intent(in):: idc,n
    Integer(4) idum,ls
	  Logical ldum
	  Type(dialog) Dlg
    Character(*), Intent(in):: s,sn(n)
	  ldum=DlgSet(Dlg,idc,n,Dlg_numitems)
    ls=GetSLength(s)
	  ldum=DlgSetChar(Dlg,idc,s(1:ls)//char(0))
    do idum=1,n
      ls=GetSLength(sn(idum))
      if(ls.lt.256) then
	      ldum=DlgSetChar(Dlg,idc,sn(idum)(1:ls),idum)
      else
	      ldum=DlgSetChar(Dlg,idc,'!! LONG STRING !!'C,idum)
      end if
    end do
  end Subroutine DlgSetSn

  Subroutine DlgSetS1(Dlg,idc,s,s1)
    Implicit none
    Integer(4), Intent(in):: idc
    Integer(4) ls
	  Logical ldum
	  Type(dialog) Dlg
    Character(*), Intent(in):: s,s1
	  ldum=DlgSet(Dlg,idc,1,Dlg_numitems)
    ls=GetSLength(s1)
    if(ls.lt.256) then
	    ldum=DlgSet(Dlg,idc,s1(1:ls),1)
    else
	    ldum=DlgSet(Dlg,idc,'!! LONG STRING !!'C,1)
    end if
    call DlgSetS0(Dlg,idc,s)
  end Subroutine DlgSetS1

  Subroutine DlgSetS0(Dlg,idc,s)
    Implicit none
    Integer(4), Intent(in):: idc
    Integer(4) ls
	  Logical ldum
	  Type(dialog) Dlg
    Character(*), Intent(in):: s
    ls=GetSLength(s)
    if(ls.lt.256) then
	    ldum=DlgSet(Dlg,idc,s(1:ls))
    else
	    ldum=DlgSet(Dlg,idc,'!! LONG STRING !!'C)
    end if
  end Subroutine DlgSetS0

  Subroutine DlgSetL(Dlg,idc,l)
    Implicit none
    Integer(4), Intent(in):: idc
	  Logical, Intent(in):: l
	  Logical ldum
	  Type(dialog) Dlg
		ldum=DlgSet(Dlg,idc,l)
  end Subroutine DlgSetL

  Subroutine DlgGetI(Dlg,idc,idcS,id,i,imin,imax,idef)
    Implicit none
    Integer(4), Intent(in):: idc,idcS,id,imin,imax,idef
    Integer(4), Intent(out):: i
    Integer(4) i0,idum
	  Logical ldum
	  Type(dialog) Dlg
	  Character(64) text
    if((id.lt.1).or.(idcS.lt.1)) then
		  ldum=DlgGet(Dlg,idc,text)
		  call StrToInt(text,i0,idum)
		  if(idum.ne.0) i0=idef
    else
		  ldum=DlgGet(Dlg,idcS,i0,Dlg_position)
      i0=imax-i0+1_4
    end if
    i=i0
    if(imax.ge.imin) then
	    i=min(imax,max(imin,i0))
	  end if
    call DlgSetI(Dlg,idc,idcS,i,imin,imax)
  end Subroutine DlgGetI

  Subroutine DlgGetR(Dlg,idc,r,rmin,rmax,rdef,idigit)
    Implicit none
    Integer(4), Intent(in):: idc,idigit
    Integer(4) idum
    Real(8), Intent(in):: rmin,rmax,rdef
    Real(8), Intent(out):: r
	  Logical ldum
	  Type(dialog) Dlg
	  Character(64) text
		ldum=DlgGet(Dlg,idc,text)
		call StrToRea(text,r,idum)
		if(idum.ne.0) r=rdef
    r=max(rmin,min(rmax,r))
    call DlgSetR(Dlg,idc,r,idigit)
  end Subroutine DlgGetR

  Subroutine DlgGetS(Dlg,idc,s)
    Implicit none
    Integer(4), Intent(in):: idc
	  Include 'RESOURCE.FD'
	  Logical ldum
	  Type(dialog) Dlg
	  Character(*), Intent(out):: s
	  Character(256) sg
		ldum=DlgGet(Dlg,idc,sg)
    if(sg(1:1).ne.'!') s=sg
  end Subroutine DlgGetS

  Subroutine DlgGetL(Dlg,idc,l)
    Implicit none
    Integer(4), Intent(in):: idc
	  Include 'RESOURCE.FD'
	  Logical, Intent(out):: l
	  Logical ldum
	  Type(dialog) Dlg
		ldum=DlgGet(Dlg,idc,l)
  end Subroutine DlgGetL

! read/write

  Subroutine WriteStr0(unit,str,iostat)
! Write a string to a unit
    implicit none
    Integer(4) unit
    character*(*) str
    Integer(4) iostat
    write(unit,'(a)',iostat=iostat) str
  end Subroutine WriteStr0

  Subroutine WriteStr(unit,str,iostat)
! Write a string to a unit
    implicit none
    Integer(4) unit,l
    character*(*) str
    Integer(4) iostat
    l=GetSLength(str)
    if(l.gt.0) then
      write(unit,'(a)',iostat=iostat) str(1:l)
    else
      write(unit,'(a)',iostat=iostat) ' '
    end if
  end Subroutine WriteStr

  Subroutine ReadStr(unit, str, iostat)
! Read a string from a unit.
! Reading stops when either
! - the string is full
! - we hit an end-of-line
! - an error hits us
    implicit none
    Integer(4) unit
    character*(*) str
    Integer(4) iostat
    read(unit,'(a)',iostat=iostat) str
  end Subroutine ReadStr

  Subroutine chwrit2(iunit,i,ni,r,nr,cs,ns,iostat)
! write ni Integer(4) numbers contained in the array i followed by
! nr Real numbers contained in the array r, followed by ns characters
! contained in the character string cs on the unit number iunit
! in compressed form.
! Note: length of the output string <80
!       cs: considered to be dummy comment -> may be cut
    Implicit none
    Integer(4) i(*),iunit,ni,nr,ns,iostat,len4
    Real(8) r(*)
    Integer(2) leng,ls
    Character(82) rs
    Character(*) cs
    if(iWriteDigits.gt.10_2) then
      if(ni.gt.0) then
        if(nr.gt.0) then
          write(iunit,*,iostat=iostat) i(1:ni),r(1:nr),cs(1:ns)
        else
          write(iunit,*,iostat=iostat) i(1:ni),cs(1:ns)
        end if
      else
        if(nr.gt.0) then
          write(iunit,*,iostat=iostat) r(1:nr),cs(1:ns)
        else
          write(iunit,'(a)',iostat=iostat) cs(1:ns)
        end if
      end if
      return
    end if
    call cswrit2(i,Int2(ni),r,Int2(nr),rs,leng)
    if(leng.lt.0) then ! string too long -> split
      if(ni.gt.0_2) then
        call cswrit2(i,Int2(ni),r,0_2,rs,leng) ! Integer(4) part
        if(leng.lt.0) then
          iostat=leng
          return
        end if
        call WriteStr(iunit,rs(1:leng),iostat)
      end if
      call cswrit2(i,0_2,r,Int2(nr),rs,leng) ! real part
      if(leng.lt.0) then ! real part too long -> split
        call cswrit2(i,0_2,r(1:((nr+1)/2)),Int2((nr+1)/2),rs,leng) ! first half
        if(leng.lt.0) then
          iostat=leng
          return
        end if
        call WriteStr(iunit,rs(1:leng),iostat)
        call cswrit2(i,0_2,r((1+(nr+1)/2):nr),Int2(nr-(nr+1)/2),rs,leng) ! second half
        if(leng.lt.0) then
          iostat=leng
          return
        end if
      end if
    end if
    if((ns.gt.0).and.(leng.lt.80_2)) then ! text string
      leng=leng+1
      rs(leng:leng)=' '
      ls=Min(Int2(ns),80_2-leng)
      if(ls.gt.0) then
        rs(leng+1:leng+ls)=cs(1:ls)
        leng=leng+ls
      end if
    end if
    if((ni.lt.1).and.(nr.lt.1)) then
      len4=Int(leng)
      call DelLeadingBlanks(rs,len4)
      leng=Int2(len4)
      if(leng.lt.1) then ! set single blank if string is empty
        rs(1:1)=' '
        leng=1
      end if
    end if
    call WriteStr(iunit,rs(1:leng),iostat)
  end Subroutine chwrit2

  Subroutine chread2(iunit,i,ni,r,nr,iostat)
! read ni Integer(4) numbers contained in the array i followed by
! nr Real numbers contained in the array r on the unit number iunit
    Implicit none
    Integer(4) i(*),iunit,ni,nr,iostat,i0,i1,i2,k
    Real(8) r(*)
    Character(82) cs
    if((ni.lt.1).and.(nr.lt.1)) then
      iostat=-9
      return
    end if
    call ReadStr(iunit,cs,iostat)
    if(iostat.ne.0) return
    i0=1
    call BStrB(cs,i0,i1,i2) ! test if the line is blank
    do while(i2.lt.i1)
      call ReadStr(iunit,cs,iostat)
      if(iostat.ne.0) return
      i0=1
      call BStrB(cs,i0,i1,i2)
    end do
    k=1
    do while(k.le.ni)
      call BStrB(cs,i0,i1,i2)
      do while(i2.lt.i1)
        call ReadStr(iunit,cs,iostat)
        if(iostat.ne.0) return
        i0=1
        call BStrB(cs,i0,i1,i2)
      end do
      i0=i2+1
      call StrToInt(cs(i1:i2),i(k),iostat)
      if(iostat.ne.0) then
        call ReadStr(iunit,cs,iostat)
        if(iostat.ne.0) return
        i0=1
        Cycle
      end if
      k=k+1
    end do
    k=1
    do while(k.le.nr)
      call BStrB(cs,i0,i1,i2)
      do while(i2.lt.i1)
        call ReadStr(iunit,cs,iostat)
        if(iostat.ne.0) return
        i0=1
        call BStrB(cs,i0,i1,i2)
      end do
      i0=i2+1
      call StrToRea(cs(i1:i2),r(k),iostat)
      if(iostat.ne.0) then
        call ReadStr(iunit,cs,iostat)
        if(iostat.ne.0) return
        i0=1
        Cycle
      end if
      k=k+1
    end do
  end Subroutine chread2

  Subroutine cswrit2(i,ni,r,nr,rs,leng)
! write ni Integer(4) numbers contained in the array i followed by
! nr Real numbers contained in the array r on the string rs
! in compressed form. leng=length of the string (leng<81)
    Implicit none
    Integer(2) leng,ni,nr,j,j1,l
    Integer(4) i(*)
    Real(8) r(*)
    Character(82) rs
    Character(20) rn
    leng=0
    if(ni.gt.0) then
      call iswrit2(i(1),rs,leng)
      do j=2,ni
        call iswrit2(i(j),rn,l)
        if((leng+l+1).gt.80) then
          leng=-leng
          return
        end if
        leng=leng+1
        rs(leng:leng)=' '
        rs(leng+1:leng+l)=rn(1:l)
        leng=leng+l
      end do
    end if
    if(nr.gt.0) then
      if(leng.gt.0) then
        j1=1
      else
        call rswrit2(r(1),iWriteDigits,rs,leng)
        j1=2
      end if
      do j=j1,nr
        call rswrit2(r(j),iWriteDigits,rn,l)
        if((leng+l+1).gt.80) then
          leng=-leng
          return
        end if
        leng=leng+1
        rs(leng:leng)=' '
        rs(leng+1:leng+l)=rn(1:l)
        leng=leng+l
      end do
    end if
  end Subroutine cswrit2

  Subroutine rswrit2(r,n,rss,leng)
! write Real number r on the string rs, use n digits (0<n<11),
! compress the string, leng=length of the string
    Implicit none
    Integer(2) n,n0,leng,ipos,i,man
    Integer(4) lout,iprec
    Real(8) r
    Character(20) rs,rss
    lMatErr=.false.
    rs='                  '
    rss='                  '
    iprec=min(10,max(0,Int4(n-1)))
    n0=Int2(iprec+1)
    call RealToStr(r,0,iprec,rs,lout)
    leng=Int2(lout)
    if((rs(1:1).ne.' ').and.(rs(1:1).ne.'-').and.(rs(1:1).ne.'+')) then
      lMatErr=.true.
      return
    end if
    if(rs(3:3).ne.'.') then
      lMatErr=.true.
      return
    end if
    if((rs(n0+3_2:n0+3_2).ne.'E').and.(rs(n0+3_2:n0+3_2).ne.'e')) then
      lMatErr=.true.
      return
    end if
    if((rs(n0+4_2:n0+4_2).ne.' ').and.(rs(n0+4_2:n0+4_2).ne.'-').and.(rs(n0+4_2:n0+4_2).ne.'+')) then
      lMatErr=.true.
      return
    end if
    if(rs(n0+4_2:n0+7_2).eq.'-001') then
      if(rs(1_2:1_2).eq.'-') then
        rss(1_2:3_2)='-0.'
        rss(4_2:4_2)=rs(2_2:2_2)
        leng=4_2
        if(n0.gt.1_2) then
          rss(5_2:n0+3_2)=rs(4_2:n0+2_2)
          leng=n0+3_2
        end if
      else
        rss(1_2:2_2)='0.'
        rss(3_2:3_2)=rs(2_2:2_2)
        leng=3_2
        if(n0.gt.1_2) then
          rss(4_2:n0+2_2)=rs(4_2:n0+2_2)
          leng=n0+2_2
        end if
      end if
      do i=leng,1_2,-1_2
        if(rss(i:i).ne.'0') exit
        leng=leng-1_2
      end do
    else
      if(rs(1:1).eq.'-') then
        rss(1:1)=rs(1:1)
        ipos=2_2
      else
        ipos=1_2
      end if
      if(rs(2:2).eq.'0') then
        rss='0'
        leng=1_2
      else
        rss(ipos:ipos)=rs(2:2)
        man=n0-1_2
        do i=n0+2_2,4_2,-1_2
          if(rs(i:i).ne.'0') exit
          man=man-1
        end do
        if(man.ne.0_2) then
          ipos=ipos+1_2
          rss(ipos:ipos)='.'
          ipos=ipos+1_2
          rss(ipos:ipos+man-1_2)=rs(4_2:3_2+man)
          ipos=ipos+man-1_2
        end if
        if(rs(n0+5_2:n0+7_2).eq.'000') then
          leng=ipos
        else
          if(rs(n0+4_2:n0+4_2).eq.'-') then
            ipos=ipos+2_2
            rss(ipos-1_2:ipos)='E-'
          else
            ipos=ipos+1_2
            rss(ipos:ipos)='E'
          end if
          if(rs(n0+5_2:n0+5_2).ne.'0') then
            ipos=ipos+3_2
            rss(ipos-2_2:ipos)=rs(n0+5_2:n0+7_2)
          else if(rs(n0+6_2:n0+6_2).ne.'0') then
            ipos=ipos+2_2
            rss(ipos-1_2:ipos)=rs(n0+6_2:n0+7_2)
          else
            ipos=ipos+1_2
            rss(ipos:ipos)=rs(n0+7_2:n0+7_2)
          end if
          leng=ipos
        end if
      end if
    end if
  end Subroutine rswrit2

  Subroutine iswrit2(i,is,leng)
! write Integer(4) number i on the string is
! compress the string, leng=length of the string
    Implicit none
    Integer(4) i,lout
    Integer(2) leng
    Character(*) is
    call IntToStr(i,0,0,is,lout)
    call DelBlanks(is,lout)
    leng=Int2(lout)
  end Subroutine iswrit2

  Subroutine rswrit1(r,n,rss,leng)
    Implicit none
    Integer(2) n,leng
    Integer(4) lout,iprec
    Real(8) r
    Character(20) rss
    rss='                  '
    iprec=min(10,max(0,Int4(n-1)))
    call RealToStr(r,0,iprec,rss,lout)
    call DelBlanks(rss,lout)
    leng=Int2(lout)
  end Subroutine rswrit1

! string manipulations

  Recursive Subroutine StrToIntV(str,i,iOK,m)
! Read Integer contained in the string str. 
! If reading fails, check if str has the form v, V, vn or Vn, where n denotes a number.
! Return the Integer i and an error flag iOK. iOK is -1, when str is an integer, -2 when it starts with f or F,
! equal to n when str has the form vn or Vn, equal to 0 (str=v or V).
    Implicit none
    Character(*) str
    Integer(4) i,iOK,k,l
    Integer(4), Optional :: m
    l=GetSLength(str)
    call StrToInt(str(1:l),i,iOK)
    if(iOK.eq.0) then
      iOK=-1
      return
    end if
    if(l.gt.0) then
      if(Present(m)) then
        if((sch(1:1).eq.'n').or.(sch(1:1).eq.'N')) then
          if(l.gt.1) then
            call StrToIntV(sch(2:l),i,iOK)
            i=i+m
          else
            i=m
            iOK=-1
          end if
        else
          if(l.gt.1) then
            if((sch(1:2).eq.'-n').or.(sch(1:2).eq.'-N')) then
              if(l.gt.2) then
                call StrToIntV(sch(3:l),i,iOK)
                i=i-m
              else
                i=-m
                iOK=-1
              end if
            else
              call StrToIntV(sch(1:l),i,iOK)
            end if
          else
            call StrToIntV(sch(1:l),i,iOK)
          end if
        end if
        return
      end if
      if((str(1:1).eq.'f').or.(str(1:1).eq.'F')) then
        iOK=-4
        return
      end if
      if((str(1:1).eq.'v').or.(str(1:1).eq.'V')) then
        if(l.gt.1) then
          call StrToInt(str(2:l),k,iOK)
          if(iOK.ne.0) then
            iOK=0
          else
            iOK=k
          end if
        else
          iOK=0
        end if
      else
        iOK=-2
      end if
    else
      iOK=-3
    end if
  end Subroutine StrToIntV

  Recursive Subroutine StrToReaV(str,r,iOK)
! Read real contained in the string str. 
! If reading fails, check if str has the form vn or Vn, where n denotes a number.
! Return the real r and an error flag iOK. 
! iOK is:
! >0: str = vn or Vn; iOK=n
! =0: str = v or V
! -1: str is the real number r that is returned
! -2: the first character is neither v nor V nor f nor F nor a number nor a sign
! -3: strange error: string length<0 detected
! -4: the first character is f or F,
! -5: the string is probably a formula
    Implicit none
    Character(*) str
    Real(8) r
    Integer(4) iOK,k,l
    call StrToRea(str,r,iOK)
    if(iOK.eq.0) then
      iOK=-1
      return
    end if
    l=GetSLength(str)
    if(l.gt.5) then
      if(str(4:4).eq.'(') then
        iOK=-5
        return
      end if
    end if
    if(l.gt.0) then
      if((str(1:1).eq.'f').or.(str(1:1).eq.'F')) then
        iOK=-4
        return
      else if((str(1:1).eq.'v').or.(str(1:1).eq.'V')) then
        if(l.gt.1) then
          call StrToInt(str(2:l),k,iOK)
          if(iOK.ne.0) then
            iOK=0
          else
            iOK=k
          end if
        else
          iOK=0
        end if
      else
        iOK=-2
      end if
    else
      iOK=-3
    end if
  end Subroutine StrToReaV

  Real(8) Function GetSValue(sFor,lFor)
    Implicit none
    Character(*) sFor
    Character(32) s
    Integer(4) ir,idum
    Integer(2) lFor
    Real(8) r
    lMatErr=.false.
    if(lfor.lt.1) then
      lMatErr=.true.
      r=0.0d0
    else
      ir=min(Int4(lFor),30_4)
     ! s(1:1)=' '
     ! s(2:ir+1)=sFor(1:ir)
     ! s(ir+2:ir+2)=' '
		 ! call StrToRea(s,r,idum)
      s(1:ir)=sFor(1:ir)
		  call StrToRea(s(:ir),r,idum)
      if(idum.ne.0) then
        lMatErr=.true.
        r=0.0d0
      end if
    end if
    getSValue=r
  end Function GetSValue

  Complex(8) Function GetCSValue(sFor,lFor)
    Implicit none
    Character(*) sFor
    Character(64) s
    Integer(4) ir,ls
    Integer(2) lFor,i
    Real(8) ri,rr
    lMatErr=.false.
    ls=Min(Int4(lFor),64_4)
    s(1:ls)=sFor(1:ls)
    i=0_2
    do ir=1,ls
      if(s(ir:ir).eq.'(') then
        s(ir:ir)=' '
        i=i+1_2
      end if
      if(s(ir:ir).eq.',') then
        s(ir:ir)=' '
        i=i+2_2
      end if
      if(s(ir:ir).eq.')') then
        s(ir:ir)=' '
        i=i+4_2
      end if
    end do
    if(i.eq.0_2) then
      rr=GetSValue(sFor,lFor)
      ri=0.0d0
    else if(i.eq.7_2) then
      call ExtractRea(s,1,rr,ir)
      if(ir.ne.0) lMatErr=.true.
      call ExtractRea(s,2,ri,ir)
      if(ir.ne.0) lMatErr=.true.
    else
      lMatErr=.true.
      rr=0.0d0
      ri=0.0d0
    end if
    GetCSValue=DCmplx(rr,ri)
  end Function GetCSValue

  Integer(4) Function GetSLength(s)
    Implicit none
    Integer(4) i,l
    Character(*) s
    l=Len_Trim(s)
    do i=1,l
      if(s(i:i).eq.char(0)) then
        l=i-1
        Exit
      end if
    end do
    do i=l,1,-1
      if(s(i:i).ne.' ') then
        GetSLength=i
        return
      end if
    end do
    GetSLength=0
  end Function GetSLength

  Subroutine DelBlanks(s,l)
    Implicit none
    Integer(4) l,l0,j,i
    Character(*) s
    l0=abs(l)
    if(l.lt.1) then
      l=Len_Trim(s)
      if(l.lt.1) then
        l=1
        return
      end if
      l0=l
      if(s(l:l).eq.Char(0)) l=l-1
      l=Len_Trim(s(1:l))
    end if
    j=0
    do i=1,l
      if(s(i:i).eq.char(0)) exit
      if(s(i:i).eq.' ') cycle
      j=j+1
      s(j:j)=s(i:i)
    end do
    l=j
    if(l.lt.l0) s(l+1:l+1)=char(0)
  end Subroutine DelBlanks

  Subroutine DelLeadingBlanks(s,l)
! if l=0: get sring length l first
! delete leading blanks and move them to the end
! append char(0) if possible
    Implicit none
    Integer(4) l,l0,j,i
    Character(*) s
    l0=abs(l)
    if(l.lt.1) then
      l=Len_Trim(s)
      if(l.lt.1) then
        s(1:1)=' '
        l=1
        if(l.lt.l0) s(l+1:l+1)=char(0)
        return
      end if
      l0=l
      if(s(l:l).eq.Char(0)) l=l-1
      l=Len_Trim(s(1:l))
    end if
    j=0
    do i=1,l
      if(s(i:i).eq.char(0)) exit
      if(s(i:i).eq.' ') then
        j=j+1
      else
        exit
      end if
    end do
    if(j.ge.l) then ! blank string
    else if(j.gt.0) then ! move leading blanks to the end of the string
      do i=1,l-j
        s(i:i)=s(i+j:i+j)
      end do
      do i=l-j+1,l
        s(i:i)=' '
      end do
    end if
    if(l.lt.l0) s(l+1:l+1)=char(0)
  end Subroutine DelLeadingBlanks

  Subroutine repChar0(s)
! replace the characters £ in the string s by char(0)
    Implicit none
    Integer(4) i,l
    Character(*) s
    l=Len_Trim(s)
    do i=1,l
      if(s(i:i).eq.'£') s(i:i)=char(0)
    end do
  end Subroutine repChar0

  Subroutine BStrB(s,i0,i1,i2)
! find part of the string s between blanks, start search at position i0
! i1>=i0: first non-blank character, i2>=i1: last non-blank character
    Implicit none
    Integer(4) i1,i0,is,i2
    Character(*) s
    Character(1) c
    i1=i0
    is=GetSLength(s)
    c=s(i1:i1)
    do while(c.eq.' ')
      if((c.eq.char(0)).or.(i1.ge.is)) then
        i2=0
        return
      end if
      i1=i1+1
      c=s(i1:i1)
    end do
    i2=i1
    do while((c.ne.' ').and.(c.ne.char(0)).and.(i2.le.is))
      i2=i2+1
      c=s(i2:i2)
    end do
    i2=i2-1
  end Subroutine BStrB

  Subroutine MStrM(s,i0,i1,i2)
! find part of the string s between "", start search at position i0
! i1>=i0: first non-" character, i2>=i1: last non-" character
    Implicit none
    Integer(4) i0,i1,i2,n,is,i
    Character(*) s
    n=0
    is=GetSLength(s)
    do i=i0,is
      if(s(i:i).ne.'"') Cycle
      n=n+1
      if(n.eq.1) i1=i+1
      if(n.eq.2) then
        i2=i-1
        return
      end if
    end do
    i1=0
    i2=-1
  end Subroutine MStrM

  Subroutine CStrC(s,c1,c2,i0,i1,i2)
! find part of the string s between two characters c1 and c2, start search at position i0
! i1>=i0: first non-" character, i2>=i1: last non-" character
    Implicit none
    Integer(4) i0,i1,i2,n,is,i
    Character(*) s
    Character(1) c1,c2
    n=0
    is=GetSLength(s)
    do i=i0,is
      if(n.eq.0) then
        if(s(i:i).ne.c1(1:1)) Cycle
        n=n+1
        i1=i+1
      else
        if(s(i:i).ne.c2(1:1)) Cycle
        i2=i-1
        return
      end if
    end do
    i1=0
    i2=-1
  end Subroutine CStrC

  Integer(4) Function nStrInStr(si,m)
! get number of substrings in si (terminated by char(13))
    Implicit none
    Integer(4) m,n,i
    Character(*) si
    n=0
    do i=2,m
      if(si(i:i).eq.char(0)) Exit
      if(si(i:i).eq.char(13)) n=n+1
    end do
    nStrInStr=n
  end Function nStrInStr

  Subroutine GetStrInStr(si,m,n,so,lso)
! get n-th line so in the string si (max. length m)
    Implicit none
    Integer(4) m,n,lso,i1,k13,i,i2
    Character(*) si,so
    i1=1
    k13=1
    lso=0
    so=' 'C
    do i=2,m
      if((si(i-1:i-1).eq.char(13)).or.(si(i-1:i-1).eq.char(0))) then ! end of substring found: increase number of substrings
        k13=k13+1
        i2=i-2
        if(k13.gt.n) then ! desired substring found
          lso=i2-i1+1
          if(lso.gt.0) then
            so(1:lso)=si(i1:i2)
            so(lso+1:lso+1)=char(0)
            return
          else
            lso=1
            so(1:1)='!'
            so(2:2)=char(0)
            return
          end if
        end if
        i1=i
        if(si(i:i).eq.char(10)) i1=i+1
      end if
    end do
  end Subroutine GetStrInStr

  Subroutine AppendStr(si,m,lsi,sa)
! append string sa (length lsa) to string si of current length lsi
    Implicit none
    Integer(4) lsa,lsi,ld,m
    Character(*) si,sa
    lsa=GetSLength(sa)
    if((lsi+lsa+3).gt.m) return
    if(lsi.gt.0) then
      ld=lsa+2
      si(lsi+1:lsi+1)=char(13)
      si(lsi+2:lsi+2)=char(10)
      si(lsi+3:lsi+ld)=sa(1:lsa)
      lsi=lsi+ld
    else
      lsi=lsa
      si(1:lsi)=sa(1:lsi)
    end if
    si(lsi+1:lsi+1)=char(0)
  end Subroutine AppendStr

  Subroutine ExtractTxt(s,n,se,lse)
! extract n-th text string (within "") se of s
    Implicit none
    Integer(4) n,lse,is,i,i1,i2
    Character(*) s,se
    is=1
    do i=1,n
      call MStrM(s,is,i1,i2)
      if(i2.lt.i1) then
        se(1:1)='!'
        se(2:2)=char(0)
        lse=0
        return
      end if
      is=i2+1
    end do
    lse=i2-i1+1
    se(1:lse)=s(i1:i2)
    se(lse+1:lse+1)=char(0)
  end Subroutine ExtractTxt

  Subroutine ExtractStrRest(s,n,se,lse)
! extract part se of the string s starting from n-th non blank part till last non-blank character
! when first character is ?: ignore characters before the second ?
! when n is negative: search -n-th non-blank part within the ?...? area
    Implicit none
    Integer(4) n,lse,is,i,i1,i2,isL
    Character(*) s,se
    isL=GetSLength(s)
    is=1
    do i=1,n
      call BStrB(s,is,i1,i2)
      if(i2.lt.i1) then
        se(1:1)='!'
        se(2:2)=char(0)
        lse=0
        return
      end if
      is=i2+1
    end do
    lse=isL-i1+1
    se(1:lse)=s(i1:isL)
    se(lse+1:lse+1)=char(0)
  end Subroutine ExtractStrRest

  Subroutine ExtractStr(s,n,se,lse)
! extract n-th non-blank part se of the string s
! when first character is ?: ignore characters before the second ?
! when n is negative: search -n-th non-blank part within the ?...? area
    Implicit none
    Integer(4) n,lse,is,i,i1,i2
    Character(*) s,se
    if(s(1:1).eq.'?') then
      is=1
      call CStrC(s,'?','?',is,i1,i2)
      if(n.lt.0) then
        is=i1
        do i=1,-n
          call BStrB(s,is,i1,i2)
          if(i2.lt.i1) then
            se(1:1)='!'
            se(2:2)=char(0)
            lse=0
            return
          end if
          is=i2+1
        end do
        lse=i2-i1+1
        se(1:lse)=s(i1:i2)
        call ToLower(se,lse)
        se(lse+1:lse+1)=char(0)
        return
      end if
      is=i2+2
    else
      is=1
    end if
    do i=1,n
      call BStrB(s,is,i1,i2)
      if(i2.lt.i1) then
        se(1:1)='!'
        se(2:2)=char(0)
        lse=0
        return
      end if
      is=i2+1
    end do
    lse=i2-i1+1
    se(1:lse)=s(i1:i2)
    call ToLower(se,lse)
    se(lse+1:lse+1)=char(0)
  end Subroutine ExtractStr

  Subroutine ExtractInt(s,n,i,iErr)
! extract nth non-blank part se of the string s and read its Integer(4) value i
    Implicit none
    Integer(4) n,i,iErr,lse
    Character(*) s
    call ExtractStr(s,n,sch,lse)
    if(lse.lt.1) then
      i=0
      iErr=1
    else
      call StrToInt(sch(1:lse),i,iErr)
    end if
  end Subroutine ExtractInt

  Subroutine ExtractRea(s,n,r,iErr)
! extract nth non-blank part se of the string s and read its real value r
    Implicit none
    Integer(4) n,iErr,lse
    Real(8) r
    Character(*) s
    call ExtractStr(s,n,sch,lse)
    if(lse.lt.1) then
      r=0.0d0
      iErr=1
    else
      call StrToRea(sch(1:lse),r,iErr)
    end if
  end Subroutine ExtractRea

  Subroutine ToLower(s,l)
    Implicit none
    Integer(4) l,i,j
    Character(*) s
    do i=1,l
      j=ichar(s(i:i))
      if((j.gt.64).and.(j.lt.91)) then
        j=j+32
        s(i:i)=char(j)
      end if
    end do
  end Subroutine ToLower

  Subroutine getNameNr(name,n)
! get number of the file "name" (form:namxxx.yyy, where xxx ist the number with 3 digits)
    Implicit none
    Integer(4) n,l,idum
    Character(4) text
    Character(256) name
    l=GetSLength(name)
    if(l.lt.7) then
      n=-1
      return
    end if
    text(1:3)=name(l-6:l-4)
		call StrToInt(text(1:3),n,idum)
		if(idum.ne.0) n=-1
  end Subroutine getNameNr

  Subroutine incName(name,nam,ext,i,n)
! increase the number of a file name by i, 000<=number<=999
! if the length of name ist too short: set default name and extension: nam000.ext
    Implicit none
    Integer(4) i,k,l,idum,lout
    Integer(4), optional:: n
    Character(3) nam,ext
    Character(4) text
    Character(256) name
    l=GetSLength(name)
    if(l.lt.7) then
      name='MAX000.???'C
      name(1:3)=nam(1:3)
      name(8:10)=ext(1:3)
      l=10
    end if
    text(1:3)=name(l-6:l-4)
		call StrToInt(text(1:3),k,idum)
		if(idum.ne.0) k=-i
    k=modulo(k+i,1000)
    call IntToStr(k,3,1,text,lout)
    name(l-6:l-4)=text(1:3)
    if(Present(n)) n=k
  end Subroutine incName

  Subroutine setName(name,nam,ext,i)
! set the number of a file name to i, 000<=number<=999
! if the length of name ist too short: set default name and extension: namxxx.ext
    Implicit none
    Integer(4) i,k,l,lout
    Character(3) nam,ext
    Character(4) text
    Character(256) name
    l=GetSLength(name)
    if(l.lt.7) then
      name='MAX000.???'C
      name(1:3)=nam(1:3)
      name(8:10)=ext(1:3)
      l=10
    end if
    k=modulo(i,1000)
    call IntToStr(k,3,1,text,lout)
    name(l-6:l-4)=text(1:3)
  end Subroutine setName

  Subroutine setNameExt(nam,ext,name)
! set the file name name equal to nam, but use ext instead of the extension of nam
    Implicit none
    Integer(4) l
    Character(3) ext
    Character(256) name,nam
    l=GetSLength(nam)
    name(1:l-3)=nam(1:l-3)
    name(l-2:l)=ext(1:3)
    name(l+1:l+1)=char(0)
  end Subroutine setNameExt

  Subroutine getNameExt(nam,ext)
! get the file name extension of nam
    Implicit none
    Integer(4) l
    Character(3) ext
    Character(256) nam
    l=GetSLength(nam)
    ext(1:3)=nam(l-2:l)
  end Subroutine getNameExt

  Subroutine setNameS(A,l,Pname,Fname,pro,ext,iErr,ADD)
    Implicit none
    Integer(4) l,n,i,iOK,ios
    Integer(4), Optional:: iErr
    Logical le
    Character(3) pro,ext
    Character(256) strins
    Character(3), optional::ADD
    Character(*) A,Pname,Fname
    if(Present(iErr)) iErr=0
    if(l.lt.1) then
	    n=GetSLength(A)
	  else
	    n=l
	  end if
    l=n
    if((A(1:1).eq.'"').and.((A(l:l).eq.'"'))) then ! delete ""
      A(1:l-2)=A(2:l-1)
      l=l-2
    end if
    if(l.gt.1) then
      if(A(1:2).eq.'++') then
        call getNameNr(Fname,n)
        if((n.gt.998).or.(n.lt.0)) then
          if(Present(iErr)) iErr=1
          return
        end if
        do l=n+1,999
          call incName(Fname,pro,ext,1,n)
          inquire(file=Fname,Exist=le)
          if(le.or.(n.gt.999)) return
        end do
      else if(A(1:2).eq.'--') then
        call getNameNr(Fname,n)
        if(n.lt.1) then
          if(Present(iErr)) iErr=1
          return
        end if
        do l=n-1,0,-1
          call incName(Fname,pro,ext,-1,n)
          inquire(file=Fname,Exist=le)
          if(le.or.(n.lt.1)) return
        end do
      else if(A(1:2).eq.'//') then
        if(l.gt.2) then
          n=GetSLength(Fname)
          strins(1:n-7)=Fname(1:n-7)
          strins(n-6:n-9+l)=A(3:l)
          strins(n-8+l:n+l-2)=Fname(n-6:n)
          strins(n+l-1:n+l-1)=char(0)
          Fname=strins
          return
        end if
      else if(A(1:1).eq.'+') then
        call getNameNr(Fname,n)
        call StrToIntV(A(2:l),i,iOK)
        if(iOK.ge.0) then
          i=nint(rMovVar(min(999,max(0,iOK))),4)
          if((iOK.lt.1000).and.(iOK.gt.-1)) iOK=0
        end if
        if((n+i.gt.999).or.(n.lt.0)) then
          if(Present(iErr)) iErr=1
          return
        end if
        call incName(Fname,pro,ext,i)
      else if(A(1:1).eq.'-') then
        call getNameNr(Fname,n)
        call StrToIntV(A(2:l),i,iOK)
        if(iOK.ge.0) then
          i=nint(rMovVar(min(999,max(0,iOK))),4)
          if((iOK.lt.1000).and.(iOK.gt.-1)) iOK=0
        end if
        if(n-i.lt.0) then
          if(Present(iErr)) iErr=1
          return
        end if
        call incName(Fname,pro,ext,-i)
      else if(A(1:2).eq.'?r') then
        call Open2read(-1,'Select data file to be read!',ext//' data file ',Fname,ext,ios)
        return
      else if(A(1:2).eq.'?w') then
        call Open2write(-1,'Select data file to be written!',ext//' data file ',Fname,ext,ios)
        return
      end if
    end if
    if(A(1:1).eq.'*') then
      call setNameExt(Pname,ext,Fname)
      if(Present(ADD)) then
        l=GetSLength(Fname)
        do n=l,l-6,-1
          Fname(n+3:n+3)=Fname(n:n)
        end do
        Fname(l-6:l-4)=ADD(1:3)
      end if
      return
    else if(A(1:1).eq.'+') then
      call getNameNr(Fname,n)
      if((n.gt.998).or.(n.lt.0)) then
        if(Present(iErr)) iErr=1
        return
      end if
      call incName(Fname,pro,ext,1)
      return
    else if(A(1:1).eq.'-') then
      call getNameNr(Fname,n)
      if(n.lt.1) then
        if(Present(iErr)) iErr=1
        return
      end if
      call incName(Fname,pro,ext,-1)
      return
    else if(A(1:1).eq.'?') then
      call Open2read(-1,'Select data file to be read!',ext//' data file ',Fname,ext,ios)
      return
    end if
    if(A(1:1).eq.'/') call incName(Fname,pro,ext,0)
    if(A(1:1).eq.'0') call setName(Fname,pro,ext,0)
    if((A(1:1).ne.'*').and.(A(1:1).ne.'0').and.(A(1:1).ne.'+').and.(A(1:1).ne.'-').and.(A(1:1).ne.'/')) &
    &  Fname=A(1:l)//char(0)
  end Subroutine setNameS

  Integer(4) Function MouseThreadProc()
    Implicit none
    Include 'resource.fd'
    type (T_MSG)  mesg
    integer(4)	 ret
    logical(4)	 lret
    type(T_RECT) mainFrame
    type(T_RECT) DlgFrame
    lret=DlgInit(IDD_MOUSE, mousedlg)
    if(.not.lret) then
      ret=MessageBoxQQ('Dialog initialization failed!'C,'Mouse dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    end if
    lret=DlgSet(mousedlg,IDC_MOUSE_I,'Unknown'C)
    lret=DlgSet(mousedlg,IDC_MOUSE_J,'Unknown'C)
    lret=DlgSet(mousedlg,IDC_MOUSE_ID,'Unknown'C)
    lret=DlgSet(mousedlg,IDC_MOUSE_X ,'Unknown'C)
    lret=DlgSet(mousedlg,IDC_MOUSE_Y ,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_Z ,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_XT,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_YT,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_ZT,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vX ,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vY ,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vZ ,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vA ,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vXT,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vYT,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vZT,'Unknown'C)
	  lret=DlgSet(mousedlg,IDC_MOUSE_vAT,'Unknown'C)
    lret=DlgModeless(mousedlg)
	  lret=GetWindowRect(GetHwndQQ(QWIN$FRAMEWINDOW),mainFrame)
	  lret=GetWindowRect(mousedlg%hwnd,DlgFrame)
	  lret=SetWindowPos(mousedlg%hwnd,NULL,mainFrame.right-(DlgFrame.right-DlgFrame.left),mainFrame.top+121,0,0,(SWP_NOACTIVATE.or.SWP_NOSIZE.or.SWP_NOZORDER))
    do while(GetMessage(mesg,NULL,0,0) /= 0)
      if(DlgIsDlgMessage(mesg) .eqv. .false.) then
        lret=TranslateMessage(mesg)
        ret=DispatchMessage(mesg)
      end if
    end do
    call DlgUninit(mousedlg)
    MouseThreadProc=0
  end Function MouseThreadProc

  Integer(4) Function OutputThreadProc()
    Implicit none
    Include 'resource.fd'
    type (T_MSG)  mesg
    integer(4)	 ret
    logical(4)	 lret
    type(T_RECT) mainFrame
    type(T_RECT) DlgFrame
    lret=DlgInit(IDD_OUTPUT, outputdlg)
    if(.not.lret) then
      ret=MessageBoxQQ('Dialog initialization failed!'C,'Output dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    end if
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK1,iOutCheck1)
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK2,iOutCheck2)
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK3,iOutCheck3)
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK4,iOutCheck4)
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK5,iOutCheck5)
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK6,iOutCheck6)
    call setCheck(outputdlg%hwnd,IDC_OUT_CHECK7,iOutCheck7)
    lret=DlgSet(outputdlg,IDC_OUT_TEXT1,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_N1   ,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_M1   ,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_TEXT2,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_N2   ,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_M2   ,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_TEXT3,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_N3   ,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_M3   ,' 'C)
    lret=DlgSet(outputdlg,IDC_OUT_TEXT3,' 'C)
    lret=DlgModeless(outputdlg)
    lret=GetWindowRect(GetHwndQQ(QWIN$FRAMEWINDOW),mainFrame)
    lret=GetWindowRect(outputdlg%hwnd,DlgFrame)
    lret=SetWindowPos(outputdlg%hwnd,NULL,mainFrame.right-(DlgFrame.right-DlgFrame.left),mainFrame.top+256,0,0,(SWP_NOACTIVATE.or.SWP_NOSIZE.or.SWP_NOZORDER))
    do while(GetMessage(mesg,NULL,0,0) /= 0)
      if(DlgIsDlgMessage(mesg) .eqv. .false.) then
        lret=TranslateMessage(mesg)
        ret=DispatchMessage(mesg)
      end if
    end do
    call DlgUninit(outputdlg)
    OutputThreadProc=0
  end Function OutputThreadProc

  Integer(4) Function ActionThreadProc()
    Implicit none
    Logical(4) ldum
    Include 'resource.fd'
    type (T_MSG)  mesg
    integer(4)	 ret
    logical(4)	 lret
    type(T_RECT) mainFrame
    type(T_RECT) DlgFrame
    External ActionSub
    lret=DlgInit(IDD_ACTION, actiondlg)
    if(.not.lret) then
      ret=MessageBoxQQ('Dialog initialization failed!'C,'Action dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    end if
    lret=DlgModeless(actiondlg)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_STOP,IDI_ICON1)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_WINDOW_OPTIONS,IDI_ICON2)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_OGL_WINDOW_OPTIONS,IDI_ICON10)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_CLEAR,IDI_ICON3)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_FUN,IDI_ICON4)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_DRAW_FUN,IDI_ICON5)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_FLD,IDI_ICON6)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_DRAW_FLD,IDI_ICON7)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_MOVIE_SCRIPT,IDI_ICON8)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_GENERATE_MOVIE,IDI_ICON9)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_BND,IDI_ICON12)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_DRAW_BND,IDI_ICON11)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_DOM,IDI_ICON20)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_DRAW_DOM,IDI_ICON21)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_EXP,IDI_ICON13)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_DRAW_EXP,IDI_ICON14)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_OBJ,IDI_ICON23)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_DRAW_OBJ,IDI_ICON22)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_MMP,IDI_ICON15)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_GFD,IDI_ICON16)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_PRO,IDI_ICON17)
	  call InitIconButton(actiondlg%hwnd,IDC_ACTION_COMPUTE_INT,IDI_ICON18)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_STOP,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_WINDOW_OPTIONS,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_OGL_WINDOW_OPTIONS,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_CLEAR,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_FUN,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_DRAW_FUN,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_FLD,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_DRAW_FLD,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_MOVIE_SCRIPT,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_GENERATE_MOVIE,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_BND,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_DRAW_BND,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_DOM,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_DRAW_DOM,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_EXP,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_DRAW_EXP,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_OBJ,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_DRAW_OBJ,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_MMP,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_GFD,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_PRO,ActionSub)
	  lret=DLGSETSUB(actiondlg,IDC_ACTION_COMPUTE_INT,ActionSub)
	  ldum=GetWindowRect(GetHwndQQ(QWIN$FRAMEWINDOW),mainFrame)
	  ldum=GetWindowRect(actiondlg%hwnd,DlgFrame)
	  ldum=SetWindowPos(actiondlg%hwnd,NULL,mainFrame.right-(DlgFrame.right-DlgFrame.left),mainFrame.top+29,0,0,(SWP_NOACTIVATE.or.SWP_NOSIZE.or.SWP_NOZORDER))    
    do while(GetMessage(mesg,NULL,0,0) /= 0)
      if(DlgIsDlgMessage(mesg) .eqv. .false.) then
        lret=TranslateMessage(mesg)
        ret=DispatchMessage(mesg)
      end if
    end do
    call DlgUninit(actiondlg)
    ActionThreadProc=0
  end Function ActionThreadProc

END MODULE CHDLG
