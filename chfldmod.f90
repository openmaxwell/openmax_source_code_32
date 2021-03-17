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
MODULE CHFLD

! Field evaluation

  USE CHCFD

  SAVE

  CONTAINS

! Defaults

  Subroutine Field_Defaults(lCheck)
! set default field
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    call Cursor(.true.,IDC_WAIT)
    ldum=lCheck
    lWinFld(1:nWin)=.true.
    lAskFld=.true.
    lSkipFld=.false.
    lSkipDerFld=.false.
    lSkipVecFld=.false.
    lSkipScaFld=.false.
    lSkipFldHead=.false.
    lStopThread=.false.
    lDiffField=.false.
    lDiffRel=.false.
    lSingleFldPoint=.false.
    call setNameExt(ProFileName,'FLD',FldFileName)
    call setNameExt(ProFileName,'FLF',FlfFileName)
    call setNameExt(ProFileName,'FLT',FltFileName)
    call setNameExt(ProFileName,'PFD',PFDFileName)
    call setNameExt(ProFileName,'GRF',GrfFileName)
    call setNameExt(ProFileName,'GRT',GrtFileName)
! Grid definition Formula
    Grd_Form(1)='add(-1,mul(2,div(sub(x,1),max(1,sub(p1,1)))))'C
    Grd_Form(2)='add(-1,mul(2,div(sub(y,1),max(1,sub(p2,1)))))'C
    Grd_Form(3)='add(0,mul(2,div(sub(z,1),max(1,sub(p3,1)))))'C
    Grd_Def_Form(0,1:3)=Grd_Form(1:3)
    Grd_Def_Form(1,1:3)=Grd_Form(1:3)
    Grd_Def_Form(2,1)='mul(div(sub(x,1),max(1,sub(p1,1))),com(2,mul(c1,div(sub(y,1),sub(p2,1)))))'C
    Grd_Def_Form(2,2)='mul(div(sub(x,1),max(1,sub(p1,1))),sim(2,mul(c1,div(sub(y,1),sub(p2,1)))))'C
    Grd_Def_Form(2,3)='div(sub(z,1),max(1,sub(p3,1)))'C
    Grd_Def_Form(3,1:3)='function'C
! Conformal mapping Formula
    Con_Form='div(add(mul(p0,v),p1),add(mul(p2,v),p3))'C
    Con_Def_Form(0)=Con_Form
    Con_Def_Form(1)=Con_Form
    Con_Def_Form(2)='log(v)'C
    Con_Def_Form(3)='exp(v)'C
! 3D Grid transform Formula
    Trns_Form(1)='add(x,mul(0.2,com(c1,x)))'C
    Trns_Form(2)='add(y,mul(0.2,com(c1,y)))'C
    Trns_Form(3)='z'C
    Trns_Def_Form(0,1:3)=Trns_Form(1:3)
    Trns_Def_Form(1,1:3)=Trns_Form(1:3)
    Trns_Def_Form(2,1)='mul(x,p0)'C
    Trns_Def_Form(2,2)='mul(y,p0)'C
    Trns_Def_Form(2,3)='mul(z,p0)'C
    Trns_Def_Form(2,1)='add(x,p1)'C
    Trns_Def_Form(2,2)='add(y,p2)'C
    Trns_Def_Form(2,3)='add(z,p3)'C
! Field definition Formula
    idFld=0
    iFFldE=1       ! auxiliary for the FField dialog
    lExpFFD=.true. ! use expansions instead of formulae
    lExcFFD=.false.! get MMP field including excitation
    kExpFFD=0      ! use all expansions
    kColFFD=0      !     with any color
    kConFFD=0      !     in any connection
    kDomFFD=0      !     in any domain
    kParFFD=0      !     all parameters
    LIQ=.false.    ! always compute abbreviations
    Q_Form(1,1)='inv(sub(log(0.25),log(0.95)))'C
    Q_Form(2,1)='neg(mul(q1,log(0.95)))'C
    Q_Form(3,1)='srt(add(sqr(x),sqr(y)))'C
    Q_Form(4,1)=' 'C
    Fld_Form(1:3,1:4)='0'C
    Fld_Def_Form(0,1:3,1:4)=Fld_Form(1:3,1:4)
    Fld_Def_Form(1,1:3,1:4)=Fld_Form(1:3,1:4)
    Fld_Def_Form(2,1:3,1:4)='0'C
    Fld_Def_Form(2,1,1)='neg(mul(q1,div(x,sqr(q3))))'C
    Fld_Def_Form(2,2,1)='neg(mul(q1,div(y,sqr(q3))))'C
    Fld_Def_Form(2,3,1)='0'C
    Fld_Def_Form(2,1,3)='x'C
    Fld_Def_Form(2,2,3)='y'C
    Fld_Def_Form(2,3,3)='z'C
    Fld_Def_Form(2,1,4)='add(mul(q1,log(q3)),q2)'C
    Fld_Def_Form(3,1,1:4)='p0'C
    Fld_Def_Form(3,2,1:4)='p6'C
    Fld_Def_Form(3,3,1:4)='p7'C
! Field transformation Formula
    iQFFD=1
    literE=.true.
    nIterPFD=10
    dtrFld=1.d-10
    Q_Form(5,2)='mul(mul(dx+,dx-),mul(dy+,dy-))'C
    Q_Form(6,2)='add(mul(dx+,dx-),mul(dy+,dy-))'C
    Q_Form(7,2)='div(q5,q6)'C
    Q_Form(8,2)='mul(mul(dx+,dx-),add(dx+,dx-))'C
    Q_Form(9,2)='mul(mul(dy+,dy-),add(dy+,dy-))'C
    Q_Form(10,2)='add(inv(mul(dx+,dx-)),inv(mul(dy+,dy-)))'C
    Q_Form(11,2)='inv(mul(dx+,add(dx+,dx-)))'C
    Q_Form(12,2)='inv(mul(dy+,add(dy+,dy-)))'C
    Q_Form(13,2)='inv(mul(dx-,add(dx+,dx-)))'C
    Q_Form(14,2)='inv(mul(dy-,add(dy+,dy-)))'C
    Q_Form(15,2)='sub(p6,mul(p8,dt))'C
    Q_Form(16,2)='inv(add(p6,mul(p8,dt)))'C
    Q_Form(17,2)='inv(p7)'C
    Q_Form(18,2)='div(mul(2,dt),dx)'C
    Q_Form(19,2)='div(mul(2,dt),dy)'C
    Q_Form(20,2)='div(mul(2,dt),dz)'C
    Q_Def_Form(0,1:20)=Q_Form(1:20,2)
    Q_Def_Form(1,1:20)=Q_Form(1:20,2)
    Q_Def_Form(2,1:20)=Q_Form(1:20,2)
    Q_Def_Form(3,1:20)=Q_Form(1:20,2)
! Constants
    pCForm(0:11)=DCmplx(1.0d0,0.0d0)
    cForm(0)=1.0d0
    cForm(1)=Pi
    cForm(2)=Eps0
    cForm(3)=Mue0
    cForm(4)=Kw0
    cForm(5)=Zw0
    c678(1:3,1:3)=1.0d0
    cForm(6:8)=c678(1:3,1)
    cCForm=DCmplx(cForm,0.0d0)
    cCForm(0)=(0.0d0,1.0d0)
! complex field
    spacecFld(1,0)=-1.0d0  ! space occupied by the field: origin,x-tangent,y-tangent,z-tangent
    spacecFld(2,0)=-1.0d0
    spacecFld(3,0)=0.0d0
    spacecFld(1,1)=2.0d0
    spacecFld(2,1)=0.0d0
    spacecFld(3,1)=0.0d0
    spacecFld(1,2)=0.0d0
    spacecFld(2,2)=2.0d0
    spacecFld(3,2)=0.0d0
    spacecFld(1,3)=0.0d0
    spacecFld(2,3)=0.0d0
    spacecFld(3,3)=2.0d0
    lrGrd=.false.         ! field on irregular grid
    lxcFld=.true.         ! x component is known
    lycFld=.true.         ! y component is known
    lzcFld=.true.         ! z component is known
    lEcFld=.true.         ! E field is known
    lHcFld=.false.        ! H field is unknown
    lAcFld=.false.        ! A field is unknown
    lVcFld=.true.         ! V field is known
    iPlane=3              ! show the field in the xy(z=const) plane
    nxcFld=11
    nycFld=11
    nzcFld=1
! real (derived) field
    trFld=0.0d0           ! current time
    prFld=0.0d0           ! current phase
    itrFld=itV            ! field type = V field
    lxrFld=.true.         ! show x components
    lyrFld=.false.
    lzrFld=.false.
    larFld=.false.        ! no average, i.e., time-dependent value
    lprFld=.false.        ! no phase, i.e., time-dependent value
    rminFld=0.0d0         ! scaling limits
    rmaxFld=1.0d0
    rSumFld=0.0d0
    rSum2Fld=0.0d0
    ErrFld=0.0d0
    Err2Fld=0.0d0
! View
    viewPlane(1:3,0:3)=0.0d0 ! view plane = xy
    viewPlane(1,1)=1.0d0
    viewPlane(2,2)=1.0d0
    viewPlane(3,3)=1.0d0
    viewDist=1.0d10       ! view distance
    nxrFld=nxcFld         ! grid lines
    nyrFld=nycFld
    levPlane=1            ! level 1 of the grid
! Field representation
    itIntensity=3         ! intensity type (iso lines etc.)
    minCIntensity=16      ! color range
    maxCIntensity=115
    scaleIntensity=1.0d0  ! scaling
    rIsoStep=0.1d0        ! step size of iso-lines
    Grid3D=0.0d0          ! Max. grid dz
    lGrid3D=.true.        ! show Grid lines
    lIsoFill=.true.       ! show filled Iso lines
    lIsoLine=.true.       ! show Iso lines
    itArrow=0             ! arrow type (don't show)
    minCArrow=216         ! color range
    maxCArrow=235
    scaleArrow=10.0d0     ! arrow scaling
    ArrowLength=0.1d0     ! max. length of the field arrows
    nsFld=1               ! step for drawing arrows
    lArrowFill=.true.     ! show filled Arrows
    PowerFld=1.0d0        ! power for derived field representation
    FactorFld=1.0d0       ! scaling factor for derived field representation
! allocate memory and initialize field
    if(.not.lgcFld) lGet3DMat=.true.
    call AllocateGrd(ldum)
    if(ldum) then
      call AllocateIFld(ldum)
      if(ldum) then
        call AllocateFld(ldum)
        if(ldum) then
          call ClearGrid(.true.)
          call ClearDomain(.true.)
          call ClearField(.true.)
          call GetrField(.false.)
        end if
      end if
    end if
! PFD data
    lPFDalloc=.false.
    lPFDc=.false.
    iPFDt=191_2 !159_2 ! PFD source type (soft, plane wave, E+H, X+Y+Z)
    iPFDft=4_2 ! time function type: exp pulse
    nPFDi=41_2 ! grid lines in i direction
    nPFDj=41_2
    nPFDk=41_2
    nPFDil=5_2 ! PML layers low side of i direction
    nPFDih=5_2
    nPFDjl=5_2
    nPFDjh=5_2
    nPFDkl=5_2
    nPFDkh=5_2
    nPFDsLayers=0_2
    nPFDiEff=2_2 ! effective material parameter computation type
    nPFDcFld=0_4 ! Fourier integral additions performed
    PFDxmin=-1.0 ! PDF array limits
    PFDxmax=1.0
    PFDymin=-1.0
    PFDymax=1.0
    PFDzmin=-1.0
    PFDzmax=1.0
    PFDdx=(PFDxmax-PFDxmin)/Dble(max(1,nPFDi-1))
    PFDdy=(PFDymax-PFDymin)/Dble(max(1,nPFDj-1))
    PFDdz=(PFDzmax-PFDzmin)/Dble(max(1,nPFDk-1))
    PFDfTau=1.0d-9
    PFDfTmax=3.0d-9
    PFDpml=-0.33
    PFDfmin=1.0d6 ! frequency range for DFT
    PFDfmax=1.0d8
    PFDwfact=0.0d0 ! stopping criterion
    PFDdfact=3.0d0
    PFDwmax=0.0d0
    nPFDsource=1
    kPFDsource=1
    call AllocatePFDsource(ldum) ! PFD point sources
    nPFDsens=1
    kPFDsens=1
    nPFDf=1 ! Frequency points for DFT
    call AllocatePFDsens(ldum) ! PFD sensors
    call Cursor(.false.,IDC_WAIT)
  end Subroutine Field_Defaults

! threads

  Subroutine TClrDom(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=1_4
    call StartMAXThread(lCheck)
  end Subroutine TClrDom

  Subroutine TClrGrd(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=2_4
    call StartMAXThread(lCheck)
  end Subroutine TClrGrd

  Subroutine TClrFld(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=3_4
    call StartMAXThread(lCheck)
  end Subroutine TClrFld

  Subroutine TTransGrd(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=4_4
    call StartMAXThread(lCheck)
  end Subroutine TTransGrd

  Subroutine TTransFld(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=5_4
    call StartMAXThread(lCheck)
  end Subroutine TTransFld

  Subroutine TClrAll(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=6_4
    call StartMAXThread(lCheck)
  end Subroutine TClrAll

  Subroutine StartMAXThread(ldi)
! start the MAX thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
    call OutTxt('t3','start MaX thread'C)
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
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start MaX thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
      iThreadHandle=0
    endif
    call OutTxt('t3','Start MaX thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(MAXThread),Loc(iArgument),iCreation,iThread)
    if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start MaX thread'C, &
                    MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
    end if
  end Subroutine StartMAXThread

  Integer(4) Function MAXThread(iWhat)
! MAX tread: calls.....
    Implicit none
    Include 'resource.fd'
    Integer(4) iWhat
    Logical ldum
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    MAXThread=0_4
    if(.not.lgcFld) lGet3DMat=.true.
    if(iThreadAction.eq.1) then
      call ClearDomain(ldum)
    else if(iThreadAction.eq.2) then
      call ClearGrid(ldum)
    else if(iThreadAction.eq.3) then
      call ClearField(ldum)
    else if(iThreadAction.eq.4) then
      call TransformGrid(ldum)
    else if(iThreadAction.eq.5) then
      call TransformField(ldum)
    else if(iThreadAction.eq.6) then
      call ClearGrid(ldum)
      call ClearDomain(ldum)
      call ClearField(ldum)
      call GetrField(.false.)
    else
      MAXThread=1_4
    end if
    call endThread()
  end Function MAXThread

! I/O

  Subroutine SaveFGrid(lCheck)
! save grid formula data in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Integer(4) iOk,ios,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select grid formula file to be written!','Grid formula file ',GrfFileName,'GRF',ios)
      if(ios.gt.0) return
    end if
    open(1,file=GrfFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save grid formula'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHGrfIdent,iOK)
    rch(1:3)=c678(1:3,3)
    call chwrit2(1,ich,0,rch,3,sch,0,iOK)
    call WriteStr(1,Grd_Form(1),iOK)
    call WriteStr(1,Grd_Form(2),iOK)
    call WriteStr(1,Grd_Form(3),iOK)
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveFGrid

  Subroutine OpenFGrid(lCheck)
! read grid formula data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist
    Integer(4) iOk,ios,idum
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select grid formula file to be read!','Grid formula file ',GrfFileName,'GRF',ios)
      if(ios.gt.0) return
    end if
    inquire(file=GrfFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=GrfFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open grid formula'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHGrfIdent(1:18).ne.text(1:18)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open grid formula'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(formula constants)'C,'Open grid formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    c678(1:3,3)=rch(1:3)
    call ReadStr(1,Grd_Form(1),iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid formula 1)'C,'Open grid formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call ReadStr(1,Grd_Form(2),iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid formula 2)'C,'Open grid formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call ReadStr(1,Grd_Form(3),iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid formula 3)'C,'Open grid formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    close(1)
  end Subroutine OpenFGrid

  Subroutine SaveTGrid(lCheck)
! save grid transform formula data in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Integer(4) iOk,ios,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select grid transform file to be written!','Grid transform file ',GrtFileName,'GRT',ios)
      if(ios.gt.0) return
    end if
    open(1,file=GrtFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save grid transformation formula'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHGrtIdent,iOK)
    rch(1)=Dble(pCForm(0))
    rch(2)=DImag(pCForm(0))
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    rch(1)=Dble(pCForm(1))
    rch(2)=DImag(pCForm(1))
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    rch(1)=Dble(pCForm(2))
    rch(2)=DImag(pCForm(2))
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    rch(1)=Dble(pCForm(3))
    rch(2)=DImag(pCForm(3))
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    call WriteStr(1,Con_Form,iOK)
    rch(1:4)=pForm(0:3)
    call chwrit2(1,ich,0,rch,4,sch,0,iOK)
    call WriteStr(1,Trns_Form(1),iOK)
    call WriteStr(1,Trns_Form(2),iOK)
    call WriteStr(1,Trns_Form(3),iOK)
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveTGrid

  Subroutine OpenTGrid(lCheck)
! read grid transform data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist
    Integer(4) iOk,ios,i,idum
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select grid transform file to be read!','Grid transform file ',GrtFileName,'GRT',ios)
      if(ios.gt.0) return
    end if
    inquire(file=GrtFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=GrtFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open grid transformation formula'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHGrtIdent(1:18).ne.text(1:18)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open grid transformation formula'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    do i=0,3
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(complex formula constants)'C,'Open grid transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      pcForm(i)=DCmplx(rch(1),rch(2))
    end do
    call ReadStr(1,Con_Form,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(conformal mapping formula)'C,'Open grid transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call chread2(1,ich,0,rch,4,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(real formula constants)'C,'Open grid transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    pForm(0:3)=rch(1:4)
    call ReadStr(1,Trns_Form(1),iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid formula 1)'C,'Open grid transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call ReadStr(1,Trns_Form(2),iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid formula 2)'C,'Open grid transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call ReadStr(1,Trns_Form(3),iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid formula 3)'C,'Open grid transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    close(1)
  end Subroutine OpenTGrid

  Subroutine SaveFField(lCheck)
! save field formula data in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Integer(4) iOk,ios,i,k,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select field formula file to be written!','Field formula file ',FlfFileName,'FLF',ios)
      if(ios.gt.0) return
    end if
    open(1,file=FlfFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save field formula'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHFlfIdent,iOK)
    ich(1)=0
    if(lExpFFD) ich(1)=1
    if(lExcFFD) ich(1)=ich(1)+10
    ich(2)=kExpFFD
    ich(3)=0
    if(LIQ) ich(3)=1
    ich(4)=kColFFD
    ich(5)=kConFFD
    ich(6)=kDomFFD
    ich(7)=kParFFD
    call chwrit2(1,ich,7,rch,0,sch,0,iOK)
    rch(1:3)=c678(1:3,1)
    call chwrit2(1,ich,0,rch,3,sch,0,iOK)
    call WriteStr(1,' abbreviations'C,iOK)
    do i=1,20
      call WriteStr(1,Q_Form(i,1),iOK)
    end do
    do k=1,4
      if(k.eq.1) call WriteStr(1,' E-field'C,iOK)
      if(k.eq.2) call WriteStr(1,' H-field'C,iOK)
      if(k.eq.3) call WriteStr(1,' A-field'C,iOK)
      if(k.eq.4) call WriteStr(1,' V-field'C,iOK)
      do i=1,3
        call WriteStr(1,Fld_Form(i,k),iOK)
      end do
    end do
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveFField

  Subroutine OpenFField(lCheck)
! read field formula data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist
    Integer(4) iOk,ios,i,k,iVers,iErr,idum
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select field formula file to be read!','Field formula file ',FlfFileName,'FLF',ios)
      if(ios.gt.0) return
    end if
    inquire(file=FlfFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=FlfFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open field formula'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHFlfIdent(1:14).ne.text(1:14)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open field formula'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    text(17:17)=text(18:18)
    call StrToInt(text(16:17),iVers,iErr)
    if(iErr.ne.0) iVers=0
    if(iVers.gt.19) then
      call chread2(1,ich,7,rch,0,iOK)
    else if(iVers.gt.10) then
      call chread2(1,ich,6,rch,0,iOK)
      ich(7)=0
    else
      call chread2(1,ich,3,rch,0,iOK)
      ich(4:7)=0
    end if
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(logicals)'C,'Open field formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    lExcFFD=.false.
    lExpFFD=.false.
    if(ich(1).gt.9) then
      lExcFFD=.true.
      ich(1)=ich(1)-10
    end if
    if(ich(1).gt.0) lExpFFD=.true.
    kExpFFD=ich(2)
    LIQ=.false.
    if(ich(3).gt.0) LIQ=.true.
    kColFFD=ich(4)
    kConFFD=ich(5)
    kDomFFD=ich(6)
    kParFFD=ich(7)
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(formula constants)'C,'Open field formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    c678(1:3,1)=rch(1:3)
    call ReadStr(1,text,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(dummy text)'C,'Open field formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    do i=1,20
      call ReadStr(1,Q_Form(i,1),iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(abbreviations)'C,'Open field formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
    end do
    do k=1,4
      call ReadStr(1,text,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(dummy text)'C,'Open field formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      do i=1,3
        call ReadStr(1,Fld_Form(i,k),iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(field formula)'C,'Open field formula'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
      end do
    end do
    close(1)
  end Subroutine OpenFField

  Subroutine SavePFD(lCheck)
! save FDTD data in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Integer(4) iOk,ios,k,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select FDTD file to be written!','Field transform file ',PFDFileName,'PFD',ios)
      if(ios.gt.0) return
    end if
    open(1,file=PFDFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save FDTD data'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHPFDIdent,iOK)
    ich(1)=nIterPFD
    rch(1)=dtrFld
    sch(1:8)=' iter,dt'
    call chwrit2(1,ich,1,rch,1,sch,8,iOK)
    ich(1)=nPFDi
    ich(2)=nPFDj
    ich(3)=nPFDk
    sch(1:6)=' i,j,k'
    call chwrit2(1,ich,3,rch,0,sch,6,iOK)
    rch(1)=PFDxmin
    rch(2)=PFDxmax
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    rch(1)=PFDymin
    rch(2)=PFDymax
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    rch(1)=PFDzmin
    rch(2)=PFDzmax
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    ich(1)=nPFDil
    ich(2)=nPFDih
    ich(3)=nPFDjl
    ich(4)=nPFDjh
    ich(5)=nPFDkl
    ich(6)=nPFDkh
    rch(1)=PFDpml
    call chwrit2(1,ich,6,rch,1,sch,0,iOK)
    ich(1)=iPFDt
    ich(2)=iPFDft
    rch(1)=PFDfTmax
    rch(2)=PFDfTau
    call chwrit2(1,ich,2,rch,2,sch,0,iOK)
    ich(1)=nPFDsource
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    do k=1,nPFDsource
      rch(1)=Dble(PFDsourceA(k))
      rch(2)=DImag(PFDsourceA(k))
      ich(1)=iPFDs(k)
      ich(2)=jPFDs(k)
      ich(3)=kPFDs(k)
      call chwrit2(1,ich,3,rch,2,sch,0,iOK)
    end do
    ich(1)=nPFDf
    rch(1)=PFDfmin
    rch(2)=PFDfmax
    call chwrit2(1,ich,1,rch,2,sch,0,iOK)
    ich(1)=nPFDsens
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    do k=1,nPFDsens
      rch(1)=PFDsensX(k)
      rch(2)=PFDsensY(k)
      rch(3)=PFDsensZ(k)
      rch(4)=PFDsensT(k)
      rch(5)=PFDsensD(k)
      call chwrit2(1,ich,0,rch,5,sch,0,iOK)
    end do
    ich(1)=nPFDiEff
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    ich(1)=nPFDsLayers
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SavePFD

  Subroutine OpenPFD(lCheck)
! read FDTD data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist,ldum
    Integer(4) iOk,ios,k,idum
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select FDTD file to be read!','Field transform file ',PFDFileName,'PFD',ios)
      if(ios.gt.0) return
    end if
    inquire(file=PFDFileName,Exist=lFileExist)
    if(.not.lFileExist) then ! try reading PFD data from FLT file (created by MaX-1)
      call setNameExt(PFDFileName,'FLT',FltFileName)
      call OpenTField(lCheck)
      return
    end if
    open(1,file=PFDFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open FDTD data'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHPFDIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open FDTD data'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call chread2(1,ich,1,rch,1,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(iterations)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nIterPFD=ich(1)
    dtrFld=rch(1)
    call chread2(1,ich,3,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD grid lines)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nPFDi=ich(1)
    nPFDj=ich(2)
    nPFDk=ich(3)
    call chread2(1,ich,0,rch,2,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD x limits)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    PFDxmin=rch(1)
    PFDxmax=rch(2)
    call chread2(1,ich,0,rch,2,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD y limits)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    PFDymin=rch(1)
    PFDymax=rch(2)
    call chread2(1,ich,0,rch,2,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD z limits)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    PFDzmin=rch(1)
    PFDzmax=rch(2)
    PFDdx=(PFDxmax-PFDxmin)/Dble(max(1,nPFDi-1))
    PFDdy=(PFDymax-PFDymin)/Dble(max(1,nPFDj-1))
    PFDdz=(PFDzmax-PFDzmin)/Dble(max(1,nPFDk-1))
    call chread2(1,ich,6,rch,1,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD UPML)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nPFDil=ich(1)
    nPFDih=ich(2)
    nPFDjl=ich(3)
    nPFDjh=ich(4)
    nPFDkl=ich(5)
    nPFDkh=ich(6)
    PFDpml=rch(1)
    call chread2(1,ich,2,rch,2,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD source)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    iPFDt=ich(1)
    iPFDft=ich(2)
    PFDfTmax=rch(1)
    PFDfTau=rch(2)
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD sources)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nPFDsource=max(1,ich(1))
    call AllocatePFDsource(ldum)
    do k=1,ich(1)
      call chread2(1,ich,3,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD sources)'C,'Open FDTD data'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      PFDsourceA(k)=DCmplx(rch(1),rch(2))
      iPFDs(k)=ich(1)
      jPFDs(k)=ich(2)
      kPFDs(k)=ich(3)
    end do
    call chread2(1,ich,1,rch,2,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD DFT)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nPFDf=max(1,ich(1))
    PFDfmin=rch(1)
    PFDfmax=rch(2)
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD sensors)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nPFDsens=max(1,ich(1))
    call AllocatePFDsens(ldum)
    do k=1,ich(1)
      call chread2(1,ich,0,rch,5,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD sensors)'C,'Open FDTD data'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      PFDsensX(k)=rch(1)
      PFDsensY(k)=rch(2)
      PFDsensZ(k)=rch(3)
      PFDsensT(k)=rch(4)
      PFDsensD(k)=rch(5)
    end do
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD effective material)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nPFDiEff=ich(1)
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(PFD scattering layers)'C,'Open FDTD data'C, &
                        MB$OK.or.MB$ICONSTOP)
      ich(1)=0
    end if
    nPFDsLayers=max(-2_2,Int2(ich(1)))
    close(1)
  end Subroutine OpenPFD

  Subroutine OpenTField(lCheck)
! read PFD data from a *.FLT file (created by MaX-1 - read for compatibility reasons: MaX-1 creates *.FLT instead of *.PFD files)
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist,ldum
    Integer(4) iOk,ios,i,j,k,idum,iVers
    Character(20) text
    Character(3) PRO
    common/project/PRO
    if(.not.lCheck) then
      call Open2read(-1,'Select field transform file to be read!','Field transform file ',FltFileName,'FLT',ios)
      if(ios.gt.0) return
    end if
    inquire(file=FltFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=FltFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open field transformation formula'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHFltIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open field transformation formula'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call StrToInt(text(16:16)//text(18:18),iVers,iOK)
    call chread2(1,ich,4,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(logicals)'C,'Open field transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(iterations)'C,'Open field transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    nIterPFD=ich(1)
    call chread2(1,ich,0,rch,1,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(time step)'C,'Open field transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    dtrFld=rch(1)
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(formula constants)'C,'Open field transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(dummy text)'C,'Open field transformation formula'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    do i=1,20
      call ReadStr(1,text,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(formula abbreviations)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
    end do
    do k=0,6
      call ReadStr(1,text,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(dummy text)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      do j=1,4
        call ReadStr(1,text,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(dummy text)'C,'Open field transformation formula'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        do i=1,3
          call ReadStr(1,text,iOK)
          if(iOK.ne.0) then
            idum=MessageBoxQQ('Error reading input file!\r(field formula)'C,'Open field transformation formula'C, &
                              MB$OK.or.MB$ICONSTOP)
            close(1)
            return
          end if
        end do
      end do
    end do
    if(iVers.gt.10) then
      call chread2(1,ich,3,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD grid lines)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nPFDi=ich(1)
      nPFDj=ich(2)
      nPFDk=ich(3)
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD x limits)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      PFDxmin=rch(1)
      PFDxmax=rch(2)
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD y limits)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      PFDymin=rch(1)
      PFDymax=rch(2)
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD z limits)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      PFDzmin=rch(1)
      PFDzmax=rch(2)
      PFDdx=(PFDxmax-PFDxmin)/Dble(max(1,nPFDi-1))
      PFDdy=(PFDymax-PFDymin)/Dble(max(1,nPFDj-1))
      PFDdz=(PFDzmax-PFDzmin)/Dble(max(1,nPFDk-1))
      call chread2(1,ich,6,rch,1,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD UPML)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nPFDil=ich(1)
      nPFDih=ich(2)
      nPFDjl=ich(3)
      nPFDjh=ich(4)
      nPFDkl=ich(5)
      nPFDkh=ich(6)
      PFDpml=rch(1)
      call chread2(1,ich,2,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD source)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      iPFDt=ich(1)
      iPFDft=ich(2)
      PFDfTmax=rch(1)
      PFDfTau=rch(2)
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD sources)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nPFDsource=max(1,ich(1))
      call AllocatePFDsource(ldum)
      do k=1,ich(1)
        call chread2(1,ich,3,rch,2,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(PFD sources)'C,'Open field transformation formula'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        PFDsourceA(k)=DCmplx(rch(1),rch(2))
        iPFDs(k)=ich(1)
        jPFDs(k)=ich(2)
        kPFDs(k)=ich(3)
      end do
      call chread2(1,ich,1,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD DFT)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nPFDf=max(1,ich(1))
      PFDfmin=rch(1)
      PFDfmax=rch(2)
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD sensors)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nPFDsens=max(1,ich(1))
      call AllocatePFDsens(ldum)
      do k=1,ich(1)
        call chread2(1,ich,0,rch,5,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(PFD sensors)'C,'Open field transformation formula'C, &
                            MB$OK.or.MB$ICONSTOP)
          close(1)
          return
        end if
        PFDsensX(k)=rch(1)
        PFDsensY(k)=rch(2)
        PFDsensZ(k)=rch(3)
        PFDsensT(k)=rch(4)
        PFDsensD(k)=rch(5)
      end do
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(PFD effective material)'C,'Open field transformation formula'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      nPFDiEff=ich(1)
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) ich(1)=0
      nPFDsLayers=max(-2_2,Int2(ich(1)))
    end if
    close(1)
  end Subroutine OpenTField

  Subroutine SaveField(lCheck)
! save field in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Integer(4) i,j,k,iOk,idum,ios
    if(lAskFld) then
      idum=MessageBoxQQ('Save field representation data?'C,'Save field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipFldHead=.true.
      if(idum.eq.MB$IDYES) lSkipFldHead=.false.
      idum=MessageBoxQQ('Save original field values?'C,'Save field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipFld=.true.
      if(idum.eq.MB$IDYES) lSkipFld=.false.
      idum=MessageBoxQQ('Save derived field values?'C,'Save field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipDerFld=.true.
      if(idum.eq.MB$IDYES) then
        lSkipDerFld=.false.
        idum=MessageBoxQQ('Save vector field?'C,'Save field'C, &
                          MB$YesNo.or.MB$IconQuestion)
        lSkipVecFld=.true.
        if(idum.eq.MB$IDYES) lSkipVecFld=.false.
        if(lSkipVecFld) then
          lSkipScaFld=.false.
        else
          idum=MessageBoxQQ('Save scalar field?'C,'Save field'C, &
                            MB$YesNo.or.MB$IconQuestion)
          lSkipScaFld=.true.
          if(idum.eq.MB$IDYES) lSkipScaFld=.false.
        end if
      end if
    end if
    if(.not.lCheck) then
      call Open2write(-1,'Select field file to be written!','Field file ',FldFileName,'FLD',ios)
      if(ios.gt.0) then
        call closeField(1)
        return
      end if
    end if
    open(1,file=FldFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save field'C, &
                        MB$OK.or.MB$IconExclamation)
      call closeField(1)
      return
    end if
    lfcFld=lfcFldAll
    call WriteStr(1,CHFldIdent,iOK)
    if(lSkipFldHead) then
      ich(1)=0
      sch(1:23)=' no representation data'
      call chwrit2(1,ich,1,rch,0,sch,23,iOK)
    else
      ich(1)=1
      sch(1:20)=' representation data'
      call chwrit2(1,ich,1,rch,0,sch,20,iOK)
    end if
! rfield
    if(.not.lSkipFldHead) then
      ich(1)=0
      if(lxrFld) ich(1)=1
      ich(2)=0
      if(lyrFld) ich(2)=1
      ich(3)=0
      if(lzrFld) ich(3)=1
      ich(4)=0
      if(larFld) ich(4)=1
      if(lprFld) ich(4)=ich(4)+2
      ich(5)=itrFld
      sch(1:11)=' lxrFld,...'
      call chwrit2(1,ich,5,rch,0,sch,11,iOK)
      rch(1)=rMinFld
      rch(2)=rMaxFld
      rch(3)=trFld
      sch(1:19)=' min/max rFld, time'
      call chwrit2(1,ich,0,rch,3,sch,19,iOK)
! view
      ich(1)=iPlane
      ich(2)=levPlane
      rch(1)=viewDist
      sch(1:8)=' view...'
      call chwrit2(1,ich,2,rch,1,sch,8,iOK)
      rch(1:3)=viewPlane(1:3,0)
      sch(1:8)=' origin '
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=viewPlane(1:3,1)
      sch(1:8)=' x tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=viewPlane(1:3,2)
      sch(1:8)=' y tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
      rch(1:3)=viewPlane(1:3,3)
      sch(1:8)=' z tang.'
      call chwrit2(1,ich,0,rch,3,sch,8,iOK)
! intensity
      ich(1)=itIntensity
      ich(2)=0
      if(lIsoLine) ich(2)=1
      ich(3)=0
      if(lGrid3D) ich(3)=1
      ich(4)=0
      if(lIsoFill) ich(4)=1
      ich(5)=minCIntensity
      ich(6)=maxCIntensity
      sch(1:15)=' intensity plot'
      call chwrit2(1,ich,6,rch,0,sch,15,iOK)
      rch(1)=scaleIntensity
      rch(2)=rIsoStep
      rch(3)=Grid3D
      sch(1:1)=' '
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
! arrows
      ich(1)=itArrow
      ich(2)=0
      if(lArrowFill) ich(2)=1
      ich(3)=nsFld
      ich(4)=minCArrow
      ich(5)=maxCArrow
      sch(1:11)=' arrow plot'
      call chwrit2(1,ich,5,rch,0,sch,11,iOK)
      rch(1)=scaleArrow
      rch(2)=ArrowLength
      sch(1:1)=' '
      call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    end if
! cfield
    ich(1)=0
    if(lxcFld) ich(1)=1
    ich(2)=0
    if(lycFld) ich(2)=1
    ich(3)=0
    if(lzcFld) ich(3)=1
    ich(4)=0
    if(lEcFld) ich(4)=1
    ich(5)=0
    if(lHcFld) ich(5)=1
    ich(6)=0
    if(lAcFld) ich(6)=1
    ich(7)=0
    if(lVcFld) ich(7)=1
    ich(8)=0
    if(lfcFld) ich(8)=1
    ich(9)=0
    if(lgcFld) ich(9)=1
    ich(10)=0
    if(lrGrd) ich(10)=1
    sch(1:10)=' lxcFld,...'
    call chwrit2(1,ich,10,rch,0,sch,10,iOK)
    ich(1)=nxcFld
    ich(2)=nycFld
    ich(3)=nzcFld
    sch(1:10)=' nxcFld,..'
    call chwrit2(1,ich,3,rch,0,sch,10,iOK)
    rch(1:3)=spacecFld(1:3,0)
    sch(1:8)=' origin '
    call chwrit2(1,ich,0,rch,3,sch,8,iOK)
    rch(1:3)=spacecFld(1:3,1)
    sch(1:8)=' x tang.'
    call chwrit2(1,ich,0,rch,3,sch,8,iOK)
    rch(1:3)=spacecFld(1:3,2)
    sch(1:8)=' y tang.'
    call chwrit2(1,ich,0,rch,3,sch,8,iOK)
    rch(1:3)=spacecFld(1:3,3)
    sch(1:8)=' z tang.'
    call chwrit2(1,ich,0,rch,3,sch,8,iOK)
    if(lSkipFld) then
      ich(1)=0
      sch(1:16)=' no field values'
      call chwrit2(1,ich,1,rch,0,sch,16,iOK)
    else
      ich(1)=1
      sch(1:13)=' field values'
      call chwrit2(1,ich,1,rch,0,sch,13,iOK)
    end if
    if(.not.lSkipFld) then
      do k=1,nzcFld
        do j=1,nycFld
          do i=1,nxcFld
            ich(1)=Int4(iFld(i,j,k))
            call chwrit2(1,ich,1,rch,0,sch,0,iOK)
            if(.not.lrGrd) then
              rch(1)=rGrd(1,i,j,k)
              rch(2)=rGrd(2,i,j,k)
              rch(3)=rGrd(3,i,j,k)
              call chwrit2(1,ich,0,rch,3,sch,0,iOK)
            end if
            idum=0
            if(lxcFld.and.lEcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iEx,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iEx,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iEx,i,j,k)
              end if
            end if
            if(lycFld.and.lEcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iEy,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iEy,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iEy,i,j,k)
              end if
            end if
            if(lzcFld.and.lEcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iEz,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iEz,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iEz,i,j,k)
              end if
            end if
            if(idum.gt.0) call chwrit2(1,ich,0,rch,idum,sch,0,iOK)
            idum=0
            if(lxcFld.and.lHcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iHx,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iHx,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iHx,i,j,k)
              end if
            end if
            if(lycFld.and.lHcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iHy,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iHy,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iHy,i,j,k)
              end if
            end if
            if(lzcFld.and.lHcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iHz,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iHz,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iHz,i,j,k)
              end if
            end if
            if(idum.gt.0) call chwrit2(1,ich,0,rch,idum,sch,0,iOK)
            idum=0
            if(lxcFld.and.lAcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iAx,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iAx,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iAx,i,j,k)
              end if
            end if
            if(lycFld.and.lAcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iAy,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iAy,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iAy,i,j,k)
              end if
            end if
            if(lzcFld.and.lAcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iAz,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iAz,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iAz,i,j,k)
              end if
            end if
            if(idum.gt.0) call chwrit2(1,ich,0,rch,idum,sch,0,iOK)
            idum=0
            if(lVcFld) then
              if(lfcFld) then
                idum=idum+1
                rch(idum)=Dble(cFld(iV,i,j,k))
                idum=idum+1
                rch(idum)=Dimag(cFld(iV,i,j,k))
              else
                idum=idum+1
                rch(idum)=dFld(iV,i,j,k)
              end if
            end if
            if(idum.gt.0) call chwrit2(1,ich,0,rch,idum,sch,0,iOK)
          end do
        end do
      end do
    end if
    if(lSkipDerFld) then
      ich(1)=0
      sch(1:24)=' no derived field values'
      call chwrit2(1,ich,1,rch,0,sch,24,iOK)
    else
      ich(1)=4
      if(lSkipVecFld) ich(1)=1
      if(lSkipScaFld) ich(1)=3
      sch(1:21)=' derived field values'
      call chwrit2(1,ich,1,rch,0,sch,21,iOK)
      ich(1)=nxrFld
      ich(2)=nyrFld
      call chwrit2(1,ich,2,rch,0,sch,0,iOK)
      if(lSkipVecFld) then
        do j=1,nyrFld
          do i=1,nxrFld
            rch(1:1)=rFld(0:0,i,j)
            call chwrit2(1,ich,0,rch,1,sch,0,iOK)
          end do
        end do
      else if(lSkipScaFld) then
        do j=1,nyrFld
          do i=1,nxrFld
            rch(1:3)=rFld(1:3,i,j)
            call chwrit2(1,ich,0,rch,3,sch,0,iOK)
          end do
        end do
      else
        do j=1,nyrFld
          do i=1,nxrFld
            rch(1:4)=rFld(0:3,i,j)
            call chwrit2(1,ich,0,rch,4,sch,0,iOK)
          end do
        end do
      end if
    end if
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    call closeField(1)
  end Subroutine SaveField

  Subroutine OpenField(lCheck)
! read field from file
    Implicit none
    Integer(4) ier,i,j,k,idum,ios,iWarn
    Logical, intent(in) :: lCheck
    Logical llxcFld,llycFld,llzcFld,llEcFld,llHcFld,llAcFld,llVcFld,llfcFld,llgcFld,llrGrd,ldum,lFileExist, &
    &       lNoHead,lDiffDom
    Character(20) text
    iWarn=0
    lDiffDom=.false.
    inquire(file=FldFileName,Exist=lFileExist)
    if(lAskFld) then
      idum=MessageBoxQQ('Read field representation data?'C,'Open field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipFldHead=.true.
      if(idum.eq.MB$IDYES) lSkipFldHead=.false.
      idum=MessageBoxQQ('Read original field values?'C,'Open field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipFld=.true.
      if(idum.eq.MB$IDYES) lSkipFld=.false.
      idum=MessageBoxQQ('Read derived field values?'C,'Open field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lSkipDerFld=.true.
      if(idum.eq.MB$IDYES) lSkipDerFld=.false.
      if(lSkipFld) lFLDset0=.true.
    end if
    if((.not.lCheck).or.((.not.lFileExist).and.lDiffField)) then
      ldum=lCheck
      if(lDiffField) then
        call Open2read(-1,'Select reference field file to be read!','Field file ',FldFileName,'FLD',ios)
      else
        call Open2read(-1,'Select field file to be read!','Field file ',FldFileName,'FLD',ios)
      end if
      if(ios.gt.0) return
      if((.not.lDiffField).and.(.not.lSkipFld)) then
        idum=MessageBoxQQ('Read differences to existing field?'C,'Open field'C, &
                          MB$YESNO.or.MB$IconQuestion)
        lDiffField=.true.
        if(idum.eq.MB$IDNO) lDiffField=.false.
      end if
    end if
    inquire(file=FldFileName,Exist=lFileExist)
    if(.not.lFileExist) then
      call closeField(1)
      return
    end if
    open(1,file=FldFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open field'C, &
                      MB$OK.or.MB$IconExclamation)
      call closeField(1)
      return
    end if
    call ReadStr(1,text,ier)
    if(CHFldIdent(1:18).ne.text(1:18)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open field'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        call closeField(1)
        return
      end if
    end if
! header
    call chread2(1,ich,1,rch,0,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(header)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    lNoHead=.false.
    if(ich(1).eq.0) lNoHead=.true.
    if(.not.lNoHead) then
! rfield
      call chread2(1,ich,5,rch,0,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(rfield)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        lxrFld=.false.
        if(ich(1).ne.0) lxrFld=.true.
        lyrFld=.false.
        if(ich(2).ne.0) lyrFld=.true.
        lzrFld=.false.
        if(ich(3).ne.0) lzrFld=.true.
        larFld=.false.
        lprFld=.false.
        if((ich(4).eq.1).or.(ich(4).eq.3)) larFld=.true.
        if(ich(4).gt.1) lprFld=.true.
        itrFld=ich(5)
      end if
      call chread2(1,ich,0,rch,3,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(rfield)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        rMinFld=rch(1)
        rMaxFld=rch(2)
        trFld=rch(3)
      end if
! view
      call chread2(1,ich,2,rch,1,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(view)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        iPlane=ich(1)
        levPlane=ich(2)
        viewDist=rch(1)
      end if
      call chread2(1,ich,0,rch,3,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(view)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) viewPlane(1:3,0)=rch(1:3)
      call chread2(1,ich,0,rch,3,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(view)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) viewPlane(1:3,1)=rch(1:3)
      call chread2(1,ich,0,rch,3,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(view)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) viewPlane(1:3,2)=rch(1:3)
      call chread2(1,ich,0,rch,3,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(view)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) viewPlane(1:3,3)=rch(1:3)
! intensity
      call chread2(1,ich,6,rch,0,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(intensity)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        itIntensity=abs(ich(1))
        lIsoLine=.false.
        if(ich(2).ne.0) lIsoLine=.true.
        lGrid3D=.false.
        if(ich(3).ne.0) lGrid3D=.true.
        lIsoFill=.false.
        if(ich(4).ne.0) lIsoFill=.true.
        minCIntensity=ich(5)
        maxCIntensity=ich(6)
      end if
      call chread2(1,ich,0,rch,3,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(intensity)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        scaleIntensity=rch(1)
        rIsoStep=rch(2)
        Grid3D=rch(3)
      end if
! arrows
      call chread2(1,ich,5,rch,0,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(arrows)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        itArrow=ich(1)
        lArrowFill=.false.
        if(ich(2).ne.0) lArrowFill=.true.
        nsFld=ich(3)
        minCArrow=ich(4)
        maxCArrow=ich(5)
      end if
      call chread2(1,ich,0,rch,2,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(arrows)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call closeField(1)
        return
      end if
      if(.not.lSkipFldHead) then
        scaleArrow=rch(1)
        ArrowLength=rch(2)
      end if
    end if
! cfield (read always)
    call chread2(1,ich,10,rch,0,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(logicals)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    llxcFld=.false.
    if(ich(1).gt.0) llxcFld=.true.
    llycFld=.false.
    if(ich(2).gt.0) llycFld=.true.
    llzcFld=.false.
    if(ich(3).gt.0) llzcFld=.true.
    llEcFld=.false.
    if(ich(4).gt.0) llEcFld=.true.
    llHcFld=.false.
    if(ich(5).gt.0) llHcFld=.true.
    llAcFld=.false.
    if(ich(6).gt.0) llAcFld=.true.
    llVcFld=.false.
    if(ich(7).gt.0) llVcFld=.true.
    llfcFld=.false.
    if(ich(8).gt.0) llfcFld=.true.
    llgcFld=.false.
    if(ich(9).gt.0) llgcFld=.true.
    llrGrd=.false.
    if(ich(10).gt.0) llrGrd=.true.
    lfcFld=lfcFldAll
    if(lDiffField.and.((llxcFld.xor.lxcFld).or.(llycFld.xor.lycFld).or.(llzcFld.xor.lzcFld).or. &
    &  (llEcFld.xor.lEcFld).or.(llHcFld.xor.lHcFld).or.(llAcFld.xor.lAcFld).or.(llVcFld.xor.lVcFld).or. &
    &  (llfcFld.xor.lfcFld).or.(llgcFld.xor.lgcFld).or.(llrGrd.xor.lrGrd))) then
      idum=MessageBoxQQ('Incompatible input file!\r(logicals)'C,'ERROR in Subroutine OpenField'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if(lAskFld.and.(llfcFld.xor.lfcFld)) then
      idum=MessageBoxQQ('Input file modifies time dependence!\rContinue reading?'C,'WARNING in Subroutine OpenField'C, &
                        MB$YESNO.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDNO) then
        call closeField(1)
        return
      end if
    end if
    lxcFld=llxcFld
    lycFld=llycFld
    lzcFld=llzcFld
    lEcFld=llEcFld
    lHcFld=llHcFld
    lAcFld=llAcFld
    lVcFld=llVcFld
    lfcFld=llfcFld
    lrGrd=llrGrd
    if(lAskFld.and.(llgcFld.xor.lgcFld)) then
      idum=MessageBoxQQ('Input file modifies z dependence!\rKeep old z dependence?'C,'WARNING in Subroutine OpenField'C, &
                        MB$YESNO.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDNO) lgcFld=llgcFld
    end if
    call chread2(1,ich,3,rch,0,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(grid lines)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if(lDiffField.and.((nxcFld.ne.ich(1)).or.(nycFld.ne.ich(2)).or.(nzcFld.ne.ich(3)))) then
      idum=MessageBoxQQ('Incompatible input file!\r(grid lines)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    nxcFld=ich(1)
    nycFld=ich(2)
    nzcFld=ich(3)
    call chread2(1,ich,0,rch,3,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(space origin)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if(lDiffField.and.((dabs(spacecFld(1,0)-rch(1)).gt.(1.0d-10*dabs(spacecFld(1,0)))).or. &
    &  (dabs(spacecFld(2,0)-rch(2)).gt.(1.0d-10*dabs(spacecFld(2,0)))).or. &
    &  (dabs(spacecFld(3,0)-rch(3)).gt.(1.0d-10*dabs(spacecFld(3,0)))))) then
      iWarn=1
    else
      spacecFld(1:3,0)=rch(1:3)
    end if
    call chread2(1,ich,0,rch,3,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(space x tangent)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if(lDiffField.and.((Abs(spacecFld(1,1)-rch(1)).gt.(1.0d-10*dabs(spacecFld(1,1)))).or. &
    &  (dabs(spacecFld(2,1)-rch(2)).gt.(1.0d-10*dabs(spacecFld(2,1)))).or. &
    &  (dabs(spacecFld(3,1)-rch(3)).gt.(1.0d-10*dabs(spacecFld(3,1)))))) then
      iWarn=1
    else
      spacecFld(1:3,1)=rch(1:3)
    end if
    call chread2(1,ich,0,rch,3,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(space y tangent)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if(lDiffField.and.((dabs(spacecFld(1,2)-rch(1)).gt.(1.0d-10*dabs(spacecFld(1,2)))).or. &
    &  (dabs(spacecFld(2,2)-rch(2)).gt.(1.0d-10*dabs(spacecFld(2,2)))).or. &
    &  (dabs(spacecFld(3,2)-rch(3)).gt.(1.0d-10*dabs(spacecFld(3,2)))))) then
      iWarn=1
    else
      spacecFld(1:3,2)=rch(1:3)
    end if
    call chread2(1,ich,0,rch,3,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(space z tangent)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if(lDiffField.and.((dabs(spacecFld(1,3)-rch(1)).gt.(1.0d-10*dabs(spacecFld(1,3)))).or. &
    &  (dabs(spacecFld(2,3)-rch(2)).gt.(1.0d-10*dabs(spacecFld(2,3)))).or. &
    &  (dabs(spacecFld(3,3)-rch(3)).gt.(1.0d-10*dabs(spacecFld(3,3)))))) then
      iWarn=1
    else
      spacecFld(1:3,3)=rch(1:3)
    end if
    if(.not.lDiffField) then
      call AllocateGrd(ldum)
      if(.not.ldum) then
        call closeField(1)
        return
      end if
      call AllocateIFld(ldum)
      if(.not.ldum) then
        call closeField(1)
        return
      end if
      call AllocateFld(ldum)
      if(.not.ldum) then
        call closeField(1)
        return
      end if
    end if
! original field values
    call chread2(1,ich,1,rch,0,ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(field)'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
      call closeField(1)
      return
    end if
    if((.not.lSkipFld).and.(ich(1).ne.0)) then
      do k=1,nzcFld
        do j=1,nycFld
          do i=1,nxcFld
            call chread2(1,ich,1,rch,0,ier)
            if(ier.ne.0) then
              idum=MessageBoxQQ('Error reading input file!\r(domain number)'C,'Open field'C, &
                                MB$OK.or.MB$ICONSTOP)
              call closeField(1)
              return
            end if
            lDiffDom=.false.
            if(lDiffField.and.(iFld(i,j,k).ne.Int2(ich(1)))) then
              iWarn=1
              lDiffDom=.true.
            else
              iFld(i,j,k)=Int2(ich(1))
            end if
            if(.not.lrGrd) then
              call chread2(1,ich,0,rch,3,ier)
              if(ier.ne.0) then
                idum=MessageBoxQQ('Error reading input file!\r(grid coordinates)'C,'Open field'C, &
                                  MB$OK.or.MB$ICONSTOP)
                call closeField(1)
                return
              end if
              if(lDiffField.and.((dabs(rGrd(1,i,j,k)-rch(1)).gt.(1.0d-100+1.0d-6*dabs(rGrd(1,i,j,k)))).or. &
              &  (dabs(rGrd(2,i,j,k)-rch(2)).gt.(1.0d-100+1.0d-6*dabs(rGrd(2,i,j,k)))).or. &
              &  (dabs(rGrd(3,i,j,k)-rch(3)).gt.(1.0d-100+1.0d-6*dabs(rGrd(3,i,j,k)))))) then
                iWarn=1
              else
                rGrd(1,i,j,k)=rch(1)
                rGrd(2,i,j,k)=rch(2)
                rGrd(3,i,j,k)=rch(3)
              end if
            end if
            idum=0
            if(lxcFld.and.lEcFld) idum=idum+1
            if(lycFld.and.lEcFld) idum=idum+1
            if(lzcFld.and.lEcFld) idum=idum+1
            if(lfcFld) idum=idum*2
            if(idum.gt.0) then
              call chread2(1,ich,0,rch,idum,ier)
              if(ier.ne.0) then
                idum=MessageBoxQQ('Error reading input file!\r(E field)'C,'Open field'C, &
                                  MB$OK.or.MB$ICONSTOP)
                call closeField(1)
                return
              end if
            end if
            idum=0
            if(lxcFld.and.lEcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iEx,i,j,k)=cFld(iEx,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iEx,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iEx,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iEx,i,j,k)=dFld(iEx,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iEx,i,j,k)=0.0d0
                else
                  dFld(iEx,i,j,k)=rch(idum)
                end if
              end if
            end if
            if(lycFld.and.lEcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iEy,i,j,k)=cFld(iEy,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iEy,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iEy,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iEy,i,j,k)=dFld(iEy,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iEy,i,j,k)=0.0d0
                else
                  dFld(iEy,i,j,k)=rch(idum)
                end if
              end if
            end if
            if(lzcFld.and.lEcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iEz,i,j,k)=cFld(iEz,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iEz,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iEz,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iEz,i,j,k)=dFld(iEz,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iEz,i,j,k)=0.0d0
                else
                  dFld(iEz,i,j,k)=rch(idum)
                end if
              end if
            end if
            idum=0
            if(lxcFld.and.lHcFld) idum=idum+1
            if(lycFld.and.lHcFld) idum=idum+1
            if(lzcFld.and.lHcFld) idum=idum+1
            if(lfcFld) idum=idum*2
            if(idum.gt.0) then
              call chread2(1,ich,0,rch,idum,ier)
              if(ier.ne.0) then
                idum=MessageBoxQQ('Error reading input file!\r(H field)'C,'Open field'C, &
                                  MB$OK.or.MB$ICONSTOP)
                call closeField(1)
                return
              end if
            end if
            idum=0
            if(lxcFld.and.lHcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iHx,i,j,k)=cFld(iHx,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iHx,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iHx,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iHx,i,j,k)=dFld(iHx,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iHx,i,j,k)=0.0d0
               else
                  dFld(iHx,i,j,k)=rch(idum)
                end if
              end if
            end if
            if(lycFld.and.lHcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iHy,i,j,k)=cFld(iHy,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iHy,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iHy,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iHy,i,j,k)=dFld(iHy,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iHy,i,j,k)=0.0d0
                else
                  dFld(iHy,i,j,k)=rch(idum)
                end if
              end if
            end if
            if(lzcFld.and.lHcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iHz,i,j,k)=cFld(iHz,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iHz,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iHz,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iHz,i,j,k)=dFld(iHz,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iHz,i,j,k)=0.0d0
                else
                  dFld(iHz,i,j,k)=rch(idum)
                end if
              end if
            end if
            idum=0
            if(lxcFld.and.lAcFld) idum=idum+1
            if(lycFld.and.lAcFld) idum=idum+1
            if(lzcFld.and.lAcFld) idum=idum+1
            if(lfcFld) idum=idum*2
            if(idum.gt.0) then
              call chread2(1,ich,0,rch,idum,ier)
              if(ier.ne.0) then
                idum=MessageBoxQQ('Error reading input file!\r(A field)'C,'Open field'C, &
                                  MB$OK.or.MB$ICONSTOP)
                call closeField(1)
                return
              end if
            end if
            idum=0
            if(lxcFld.and.lAcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iAx,i,j,k)=cFld(iAx,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iAx,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iAx,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iAx,i,j,k)=dFld(iAx,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iAx,i,j,k)=0.0d0
                else
                  dFld(iAx,i,j,k)=rch(idum)
                end if
              end if
            end if
            if(lycFld.and.lAcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iAy,i,j,k)=cFld(iAy,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iAy,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iAy,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iAy,i,j,k)=dFld(iAy,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iAy,i,j,k)=0.0d0
                else
                  dFld(iAy,i,j,k)=rch(idum)
                end if
              end if
            end if
            if(lzcFld.and.lAcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iAz,i,j,k)=cFld(iAz,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iAz,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iAz,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iAz,i,j,k)=dFld(iAz,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iAz,i,j,k)=0.0d0
                else
                  dFld(iAz,i,j,k)=rch(idum)
                end if
              end if
            end if
            idum=0
            if(lVcFld) idum=idum+1
            if(lfcFld) idum=idum*2
            if(idum.gt.0) then
              call chread2(1,ich,0,rch,idum,ier)
              if(ier.ne.0) then
                idum=MessageBoxQQ('Error reading input file!\r(V field)'C,'Open field'C, &
                                  MB$OK.or.MB$ICONSTOP)
                call closeField(1)
                return
              end if
            end if
            idum=0
            if(lVcFld) then
              idum=idum+1
              if(lfcFld) then
                idum=idum+1
                if(lDiffField) then
                  cFld(iV,i,j,k)=cFld(iV,i,j,k)-DCmplx(rch(idum-1),rch(idum))
                  if(lDiffDom) cFld(iV,i,j,k)=(0.0d0,0.0d0)
                else
                  cFld(iV,i,j,k)=DCmplx(rch(idum-1),rch(idum))
                end if
              else
                if(lDiffField) then
                  dFld(iV,i,j,k)=dFld(iV,i,j,k)-rch(idum)
                  if(lDiffDom) dFld(iV,i,j,k)=0.0d0
                else
                  dFld(iV,i,j,k)=rch(idum)
                end if
              end if
            end if
          end do
        end do
      end do
      lFLDset0=.false.
    else if(.not.lFLDset0) then
      if(.not.lgcFld) lGet3DMat=.true.
      call ClearGrid(.true.)
      call ClearDomain(.true.)
      call ClearField(.true.)
    end if
    if(lSkipDerFld) then
      if(.not.lFLDset0) call GetrField(.false.)
      call closeField(1)
      return
    else
! derived field values
      call chread2(1,ich,1,rch,0,ier)
      if((ier.ne.0).or.(ich(1).lt.1)) then
        call GetrField(.false.)
        call closeField(1)
        return
      end if
      lSkipVecFld=.false.
      lSkipScaFld=.false.
      if(ich(1).eq.1) then
        lSkipVecFld=.true.
      else if(ich(1).eq.3) then
        lSkipScaFld=.true.
      end if
      call chread2(1,ich,2,rch,0,ier)
      if(ier.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(derived grid lines)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call GetrField(.false.)
        call closeField(1)
        return
      end if
      if(lDiffField.and.((nxrFld.ne.ich(1)).or.(nyrFld.ne.ich(2)))) then
        idum=MessageBoxQQ('Incompatible input file!\r(derived grid lines)'C,'Open field'C, &
                          MB$OK.or.MB$ICONSTOP)
        call GetrField(.false.)
        call closeField(1)
        return
      end if
      if((.not.lCheck).and.(lDiffField)) then
        lDiffRel=.false.
        idum=MessageBoxQQ('Read relative differences?'C,'Open field'C, &
                          MB$YESNO.or.MB$IconQuestion)
        if(idum.eq.MB$IDYES) lDiffRel=.true.
      end if
      if(.not.lDiffField) then ! computing is usually faster and saver than reading
        call GetrField(.false.)
        call closeField(1)
        return
      end if
      if((itrFld.eq.itWe).or.(itrFld.eq.itWh).or.(itrFld.eq.itWt).or. &
      &  (itrFld.eq.itPe).or.(itrFld.eq.itPh).or.(itrFld.eq.itPt).or. &
      &  (itrFld.eq.itPe)) then
        ldum=.true.
      else
        ldum=.false.
        idum=0
        if(lxrFld) idum=idum+1
        if(lyrFld) idum=idum+1
        if(lzrFld) idum=idum+1
        if(idum.lt.2) ldum=.true.
      end if
      if((nxrFld.ne.ich(1)).and.(nyrFld.ne.ich(2))) then
        idum=MessageBoxQQ('Error reading input file!\rWrong grid line numbers of derived field!&
                          &\rDervied field recomputed!'C,'Open field'C,MB$OK.or.MB$ICONSTOP)
        call GetrField(.false.)
        call closeField(1)
        return
      end if
      do j=1,nyrFld
        do i=1,nxrFld
          if(lSkipVecFld) then
            call chread2(1,ich,0,rch,1,ier)
            rch(2:3)=0.0d0
            rch(4)=rch(1)
          else if(lSkipScaFld) then
            call chread2(1,ich,0,rch,3,ier)
            rch(4)=rch(3)
            rch(3)=rch(2)
            rch(2)=rch(1)
            rch(1)=r3Vec_Length(rch(1:3))
          else
            call chread2(1,ich,0,rch,4,ier)
          end if
          if(ier.ne.0) then
            idum=MessageBoxQQ('Error reading input file!\r(derived field)'C,'Open field'C, &
                              MB$OK.or.MB$ICONSTOP)
            call GetrField(.false.)
            call closeField(1)
            return
          end if
          if(lDiffField) then
            if(lDiffRel) then
              rFld(1:3,i,j)=(rFld(1:3,i,j)-rch(2:4))/rch(1)
              if(ldum) then
                rFld(0,i,j)=rFld(1,i,j)+rFld(2,i,j)+rFld(3,i,j)
              else
                rFld(0,i,j)=r3Vec_Length(rFld(1:3,i,j))
              end if
            else
              rFld(0:3,i,j)=rFld(0:3,i,j)-rch(1:4)
              rFld(0,i,j)=abs(rFld(0,i,j))
            end if
          else
            rFld(0:3,i,j)=rch(1:4)
          end if
        end do
      end do
      call closeField(1)
    end if
    if((iWarn.ne.0).and.(.not.lCheck)) then
      idum=MessageBoxQQ('Input file might be incompatible!'C,'Open field'C, &
                        MB$OK.or.MB$ICONSTOP)
    end if
  end Subroutine OpenField

  Subroutine closeField(iUnit)
    Implicit none
    Integer(4) iUnit
    Logical ldum
    inquire(unit=iUnit,opened=ldum)
    if(ldum) close(iUnit)
    lAskFld=.true.
    lSkipFld=.false.
    lSkipDerFld=.false.
    lSkipVecFld=.false.
    lSkipScaFld=.false.
    lSkipFldHead=.false.
    lDiffField=.false.
    lDiffRel=.false.
  end Subroutine closeField

! Field handling

  Subroutine ClearDomain(lCheck)
! clear the domain numbers
    Implicit none
    Include 'resource.fd'
    Real(8) dmin,rNmin(3),val(2)
    Integer(4) i,j,k,kk,km,lout
    Logical, intent(in) :: lCheck
    if(.not.lCheck) then
      iFld=1_2
      return
    end if
    call OutTxt('t1','Set domain numbers'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    call cBndGetABO() ! get c-poly, splines, match.pts
    if(.not.lgcFld) then
      call get3DMatPts(1,nObj,1_4,3_2,.true.)
    end if
    kk=0
    km=Int4(nxcFld)*Int4(nycFld)*Int4(nzcFld)
    if(lExpFFD.and.(kDomFFD.gt.0).and.(kDomFFD.le.nDom)) then
      iFld=Int2(kDomFFD)
    else
!!$OMP parallel do
      do k=1,nzcFld
        do j=1,nycFld
          do i=1,nxcFld
            if(lStopThread) return
            kk=kk+1
            call IntToStr(Int4(kk),0,0,SpaceText,lout)
            call OutTxt('n1',SpaceText(1:lout))
            call IntToStr(km,0,0,SpaceText,lout)
            call OutTxt('m1',SpaceText(1:lout))
            vForm(1:3)=getcGrd(i,j,k)
            call DistPtObj(0_2,0_2,vForm(1:3),.true.,dmin,rNmin,iFld(i,j,k),val,.true.)
            iFld(i,j,k)=Max(-5_2,Min(Int2(nDom),iFld(i,j,k)))
          end do
        end do
      end do
    end if
    call OutTxt('t1','Domain numbers set'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
  end Subroutine ClearDomain

  Subroutine ClearField(lCheck)
! clear the complex field
    Implicit none
    Include 'resource.fd'
    Integer(4) i,j,k,kk,km,nx,ny,nz,iColExp0,iConExp0,iDomExp0,lout,iAbb,kExpFF
    Integer(2) lf
    Logical, intent(in) :: lCheck
    Logical lAbb,ldum
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
    if(lfcFld.xor.lfcFldAll) then
      call AllocateFld(ldum)
      if(.not.ldum) return
    end if
    call getEUST()
    if(lExpFFD) then
      iColExp0=iColExp
      iConExp0=iConExp
      iDomExp0=iDomExp
      iColExp=Int2(kColFFD)
      iConExp=Int2(kConFFD)
      iDomExp=Int2(kDomFFD)
      kExpFF=kExpFFD
      if(lExcFFD) then
        if((kExpFFD.eq.nExp).or.(kExpFFD.eq.0).or.(kExpFFD.le.-nExp)) kExpFF=-(nExp-nExc)
      end if
      call SetFldOnGrid(kExpFF)
      iColExp=iColExp0
      iConExp=iConExp0
      iDomExp=iDomExp0
      return
    end if
    if(.not.lSingleFldPoint) then
      call OutTxt('t1','Set field values'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
    end if
    cForm(6:8)=c678(1:3,1)
    cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
    call SetIDFld()
! set constant parameters
    if(lfcFld) then
      pCForm(1)=Dcmplx(Dble(nxcFld),0.0d0)
      pCForm(2)=Dcmplx(Dble(nycFld),0.0d0)
      pCForm(3)=Dcmplx(Dble(nzcFld),0.0d0)
      pCForm(4)=fcFld
      if(lzPer) then
        pCForm(5)=Pi/dcFld
      else
        pCForm(5)=(2.0d0*Pi*kw0)*fcFld*gcFld
      end if
    else
      pForm(1)=dble(nxcFld)
      pForm(2)=dble(nycFld)
      pForm(3)=dble(nzcFld)
      pForm(4)=Dble(fcFld)
      if(lzPer) then
        pForm(5)=Dble(Pi/dcFld)
      else
        pForm(5)=Dble((2.0d0*Pi*kw0)*fcFld*gcFld)
      end if
    end if
! set field values
    vForm(0)=trFld
    lAbb=.true.
    if(lSingleFldPoint) then
      nx=1
      ny=1
      nz=1
    else
      nx=nxcFld
      ny=nycFld
      nz=nzcFld
    end if
    kk=0
    km=Int4(nx)*Int4(ny)*Int4(nz)
!!$OMP parallel do
    do k=1,nz
      do j=1,ny
        do i=1,nx
          if(lStopThread) return
          kk=kk+1
          if(.not.lSingleFldPoint) then
            call IntToStr(Int4(kk),0,0,SpaceText,lout)
            call OutTxt('n1',SpaceText(1:lout))
            call IntToStr(km,0,0,SpaceText,lout)
            call OutTxt('m1',SpaceText(1:lout))
          end if
! compute parameters and variables
          if(lSingleFldPoint) then
            vForm(1:3)=rSingleFldPoint(1:3)
            lf=Max(1_2,Min(Int2(nDom),iSingleFldPoint))
          else
            vForm(1:3)=getcGrd(i,j,k)
            lf=Max(1_2,Min(Int2(nDom),iFld(i,j,k)))
          end if
          if(lfcFld) then
            kcFld=Wnumber(lf)
            if(lzPer) then
              kapcFld=cdsqrt(kcFld**2-(Pi/dcFld)**2)
            else
              kapcFld=cdsqrt(kcFld**2-((2.0d0*Pi*kw0)*fcFld*gcFld)**2)
            end if
            if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
            pCForm(0)=DCmplx(Dble(iFld(i,j,k)),0.0d0)
            pCForm(6)=eDom(lf)*Eps0
            pCForm(7)=uDom(lf)*Mue0
            pCForm(8)=sDom(lf)
            pCForm(11)=tDom(lf)
            pCForm(9)=kcFld
            pCForm(10)=kapcFld
            vCForm(0:3)=DCmplx(vForm(0:3),0.0d0)
          else
            kcFld=DCmplx(Dble(Wnumber(lf)),0.0d0)
            if(lzPer) then
              kapcFld=cdsqrt(kcFld**2-(Pi/dcFld)**2)
            else
              kapcFld=cdsqrt(kcFld**2-((2.0d0*Pi*kw0)*fcFld*gcFld)**2)
            end if
            if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
            pForm(0)=Dble(iFld(i,j,k))
            pForm(6)=Dble(eDom(lf))*Eps0
            pForm(7)=Dble(uDom(lf))*Mue0
            pForm(8)=Dble(sDom(lf))
            pForm(11)=Dble(tDom(lf))
            pForm(9)=Dble(kcFld)
            pForm(10)=cdabs(kapcFld)
          end if
! compute abbreviations
          if(lAbb.or.(.not.lIQ)) then
            iQ_Form=1
            do iAbb=1,20
              if(Q_Form(iAbb,1)(1:1).eq.' ') Cycle
              call AbbrFld(Int2(iAbb),i,j,k)
            end do
            if(lIQ) lAbb=.false.
          end if
! compute field values
          if(lxcFld.and.lEcFld) then
            call ClrFld(lCheck,iEx,1,1,i,j,k)
          end if
          if(lycFld.and.lEcFld) then
            call ClrFld(lCheck,iEy,2,1,i,j,k)
          end if
          if(lzcFld.and.lEcFld) then
            call ClrFld(lCheck,iEz,3,1,i,j,k)
          end if
          if(lxcFld.and.lHcFld) then
            call ClrFld(lCheck,iHx,1,2,i,j,k)
          end if
          if(lycFld.and.lHcFld) then
            call ClrFld(lCheck,iHy,2,2,i,j,k)
          end if
          if(lzcFld.and.lHcFld) then
            call ClrFld(lCheck,iHz,3,2,i,j,k)
          end if
          if(lxcFld.and.lAcFld) then
            call ClrFld(lCheck,iAx,1,3,i,j,k)
          end if
          if(lycFld.and.lAcFld) then
            call ClrFld(lCheck,iAy,2,3,i,j,k)
          end if
          if(lzcFld.and.lAcFld) then
            call ClrFld(lCheck,iAz,3,3,i,j,k)
          end if
          if(lVcFld) then
            call ClrFld(lCheck,iV,1,4,i,j,k)
          end if
        end do
      end do
    end do
    if(.not.lSingleFldPoint) then
      call OutTxt('t1','Field values set'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
    end if
  end Subroutine ClearField

  Subroutine ClrFld(lCheck,id,iFor1,iFor2,i,j,k)
! auxiliary for ClearField
    Implicit none
    Real(8) dmin,rNmin(3),val(2)
    Integer(4) i,j,k,iErr,lf,iFor1,iFor2
    Integer(2) id,idl,ifl
    Logical lCheck
    lfcFld=lfcFldAll
    if(lSingleFldPoint) then
      ifl=iSingleFldPoint
    else
      ifl=iFld(i,j,k)
    end if
    if(ifl.lt.1) then
      if(lCheck.and.(nBnd.gt.0).and.(id.eq.iV)) then
        if(lSingleFldPoint) then
          vForm(1:3)=rSingleFldPoint(1:3)
        else
          vForm(1:3)=getcGrd(i,j,k)
        end if
        call DistPtObj(0_2,0_2,vForm(1:3),.true.,dmin,rNmin,idl,val,.true.)
        if(lfcFld) then
          cSingleFldPoint(id)=DCmplx(val(1),0.0d0)
        else
          dSingleFldPoint(id)=val(1)
        end if
      else if(lCheck.and.(nBnd.gt.0).and.(id.eq.iAz)) then
        if(lSingleFldPoint) then
          vForm(1:3)=rSingleFldPoint(1:3)
        else
          vForm(1:3)=getcGrd(i,j,k)
        end if
        call DistPtObj(0_2,0_2,vForm(1:3),.true.,dmin,rNmin,idl,val,.true.)
        if(lfcFld) then
          cSingleFldPoint(id)=DCmplx(val(2),0.0d0)
        else
          dSingleFldPoint(id)=val(2)
        end if
      else
        if(lfcFld) then
          cSingleFldPoint(id)=DCmplx(0.0d0,0.0d0)
        else
          dSingleFldPoint(id)=0.0d0
        end if
      end if
    else
      if(lCheck) then
        lf=-1
        if(lfcFld) then
          cSingleFldPoint(id:id)=cFormula(Fld_Form(iFor1,iFor2),lf,cCForm,pCForm,vCForm,8,11,999,1,1,1,iErr)
        else
          dSingleFldPoint(id:id)=Formula(Fld_Form(iFor1,iFor2),lf,cForm,pForm,vForm,8,11,999,1,1,1,iErr)
        end if
      else
        if(lfcFld) then
          cSingleFldPoint(id)=DCmplx(0.0d0,0.0d0)
        else
          dSingleFldPoint(id)=0.0d0
        end if
      end if
    end if
    if(lfcFld) then
      cFld(id,i,j,k)=cSingleFldPoint(id)
    else
      dFld(id,i,j,k)=dSingleFldPoint(id)
    end if
  end Subroutine ClrFld

  Subroutine SetFldOnGrid(kE)
! compute field of expansion kE - all expansions if kE out of range
    Implicit none
    Include 'resource.fd'
    Real(8) r(3),dmin,rNmin(3),val(2)
    Integer(4) i,j,k,kk,km,nx,ny,nz,kE,idum,lout
    Integer(2) iD
    Logical lAbb
    if(.not.lSingleFldPoint) then
      call OutTxt('t1','Set field values'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
    end if
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
    call SetIDFld()
! set field values
    vForm(0)=trFld
    lAbb=.true.
    if(lSingleFldPoint) then
      nx=1
      ny=1
      nz=1
    else
      nx=nxcFld
      ny=nycFld
      nz=nzcFld
    end if
    kk=0
    km=Int4(nx)*Int4(ny)*Int4(nz)
    lDispWarn=.true.
!!$OMP parallel do
    do k=1,nz
      do j=1,ny
        do i=1,nx
          if(lStopThread) return
          kk=kk+1
          if(.not.lSingleFldPoint) then
            call IntToStr(Int4(kk),0,0,SpaceText,lout)
            call OutTxt('n1',SpaceText(1:lout))
            call IntToStr(km,0,0,SpaceText,lout)
            call OutTxt('m1',SpaceText(1:lout))
          end if
! compute field in point
          if(lSingleFldPoint) then
            r(1:3)=rSingleFldPoint(1:3)
            iD=iSingleFldPoint
          else
            r(1:3)=getcGrd(i,j,k)
            iD=iFld(i,j,k)
          end if
          if(iD.lt.0) call DistPtObj(0_2,0_2,r,.true.,dmin,rNmin,iD,val,.true.)
          if(iD.gt.0) then
            call GetFieldExp(kE,Int4(kParFFD),r,iD,iHEGlobal,idum)
          else
            FldExp(1:10)=(0.0d0,0.0d0)
            call DistPtObj(0_2,0_2,r,.true.,dmin,rNmin,iD,val,.true.)
            FldExp(8)=DCmplx(val(2),0.0d0)
            FldExp(10)=DCmplx(val(1),0.0d0)
          end if
! compute field values
          if(lxcFld.and.lEcFld) then
            call SetFld(iEx,1,i,j,k)
          end if
          if(lycFld.and.lEcFld) then
            call SetFld(iEy,2,i,j,k)
          end if
          if(lzcFld.and.lEcFld) then
            call SetFld(iEz,3,i,j,k)
          end if
          if(lxcFld.and.lHcFld) then
            call SetFld(iHx,4,i,j,k)
          end if
          if(lycFld.and.lHcFld) then
            call SetFld(iHy,5,i,j,k)
          end if
          if(lzcFld.and.lHcFld) then
            call SetFld(iHz,6,i,j,k)
          end if
          if(lxcFld.and.lAcFld) then
            call SetFld(iAx,7,i,j,k)
          end if
          if(lycFld.and.lAcFld) then
            call SetFld(iAy,8,i,j,k)
          end if
          if(lzcFld.and.lAcFld) then
            call SetFld(iAz,9,i,j,k)
          end if
          if(lVcFld) then
            call SetFld(iV,10,i,j,k)
          end if
        end do
      end do
    end do
    if(.not.lSingleFldPoint) then
      call OutTxt('t1','Field values set'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
    end if
  end Subroutine SetFldOnGrid

  Subroutine SetFld(id,iE,i,j,k)
    Implicit none
    Integer(4) i,j,k,iE
    Integer(2) id
    lfcFld=lfcFldAll
    if(.not.lSingleFldPoint) then
      if(lfcFld) then
        cFld(id,i,j,k)=FldExp(iE)
      else
        dFld(id,i,j,k)=Dble(FldExp(iE))
      end if
    else
      if(lfcFld) then
        cSingleFldPoint(id)=FldExp(iE)
      else
        dSingleFldPoint(id)=Dble(FldExp(iE))
      end if
    end if
  end Subroutine SetFld

  Subroutine getNearestEps2D(i,j,epsij,epsnj,epspj,epsin,epsip)
! compute epsilon values in the 2D FD grid point ij and its nearest neigbours
  Integer(2) iD
  Integer(4) i,j,k
  Real(8) epsij,epsnj,epspj,epsin,epsip,r0(3),dmin,rmin(3),val(2)
  k=1
  r0(1:3)=getcGrd(i,j,k)
  call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
  if(iD.lt.1) then ! point in electrode: no material properties needed
    epsij=1.0d0
    epsnj=0.0d0
    epspj=0.0d0
    epsin=0.0d0
    epsip=0.0d0
  else
    epsij=Dble(eDom(iD))
    r0(1:3)=getcGrd(i-1,j,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsnj=Dble(eDom(iD))
    if(iD.lt.1) epsnj=epsij
    r0(1:3)=getcGrd(i+1,j,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epspj=Dble(eDom(iD))
    if(iD.lt.1) epspj=epsij
    r0(1:3)=getcGrd(i,j-1,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsin=Dble(eDom(iD))
    if(iD.lt.1) epsin=epsij
    r0(1:3)=getcGrd(i,j+1,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsip=Dble(eDom(iD))
    if(iD.lt.1) epsip=epsij
  end if
  end Subroutine getNearestEps2D
  
  Subroutine TransformLaplace2D(Initialize)
! transform the field with standard 2D Laplace operator
    Implicit none
    Include 'resource.fd'
    Real(8) epsij,epsnj,epspj,epsin,epsip
    Integer(4) i,j,k,iter,kk,km,i1,lout,ierr,idum
    Integer(2) lf
    Integer(4), intent(in) :: Initialize
    lfcFld=lfcFldAll
    call OutTxt('t2','L-transform field'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    call OutTxt('t1','Field point'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    if(Initialize.eq.1) then
! allocate memory for arrays
      DeAllocate(gax,gay,stat=ierr)
      if(.not.lVcFld) then
        idum=MessageBoxQQ('Scalar potential V not selected!'C,'2DL transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        return
      end if
      if(lfcFld) then
        idum=MessageBoxQQ('Frequncy must be 0!'C,'2DL transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        return
      end if
      Allocate(gax(nxcFld,nycFld,nzcFld),gay(nxcFld,nycFld,nzcFld),stat=ierr)
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'2DL transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        DeAllocate(gax,gay,stat=ierr)
        return
      end if
      lPFDalloc=.true.
! initialize material parameters (containing grad(eps))
      k=1
      gax=0.0d0
      gay=0.0d0
      do i=2,nxcFld-1
        do j=2,nycFld-1
          if(lStopThread) return
          call getNearestEps2D(i,j,epsij,epsnj,epspj,epsin,epsip)
          gax(i,j,k)=(epspj-epsnj)/(16.0d0*epsij)
          gay(i,j,k)=(epsip-epsin)/(16.0d0*epsij)
        end do
      end do
    end if
! copy current to auxiliary field
    dAuxFld=dFld
! only 1 field component to be used: V (last one of the original field)
    i1=0
    if(lxcFld.and.lEcFld) i1=i1+1
    if(lycFld.and.lEcFld) i1=i1+1
    if(lzcFld.and.lEcFld) i1=i1+1
    if(lxcFld.and.lHcFld) i1=i1+1
    if(lycFld.and.lHcFld) i1=i1+1
    if(lzcFld.and.lHcFld) i1=i1+1
    if(lxcFld.and.lAcFld) i1=i1+1
    if(lycFld.and.lAcFld) i1=i1+1
    if(lzcFld.and.lAcFld) i1=i1+1
    if(lVcFld) i1=i1+1
! iterations and loops over entire grid
    do iter=1,nIterPFD
      call OutTxt('t2','Field transform'C)
      call IntToStr(Int4(nIterPFD),0,0,SpaceText,lout)
      call OutTxt('m2',SpaceText(1:lout))
      call IntToStr(Int4(iter),0,0,SpaceText,lout)
      call OutTxt('n2',SpaceText(1:lout))
      kk=0
      km=Int4(nxcFld)*Int4(nycFld)*Int4(nzcFld)
      k=1
      do i=1,nxcFld
        do j=1,nycFld
          if(lStopThread) return
          kk=kk+1
          call OutTxt('t1','Field point'C)
          call IntToStr(Int4(kk),0,0,SpaceText,lout)
          call OutTxt('n1',SpaceText(1:lout))
          call IntToStr(km,0,0,SpaceText,lout)
          call OutTxt('m1',SpaceText(1:lout))
! skip if 1) field domain number out of range or 2) point on border
          if((i.eq.1).or.(i.eq.nxcFld).or.(j.eq.1).or.(j.eq.nycFld)) Cycle
          lf=Max(1_2,Min(Int2(nDom),iFld(i,j,k)))
          if(lf.ne.iFld(i,j,k)) Cycle
! perform transformations of the existing field components
          dAuxFld(i1,i,j,k)=(dFld(i1,i+1,j,k)+dFld(i1,i-1,j,k)+ &
          &                    dFld(i1,i,j+1,k)+dFld(i1,i,j-1,k))/4.0d0 ! 5-point star operator
          if(dabs(gax(i,j,k)).gt.1e-30) dAuxFld(i1,i,j,k)=dAuxFld(i1,i,j,k)+gax(i,j,k)*(dFld(i1,i+1,j,k)-dFld(i1,i-1,j,k))
          if(dabs(gay(i,j,k)).gt.1e-30) dAuxFld(i1,i,j,k)=dAuxFld(i1,i,j,k)+gay(i,j,k)*(dFld(i1,i,j+1,k)-dFld(i1,i,j-1,k))
        end do
      end do
! copy auxiliary to current field
      dFld=dAuxFld
    end do
    call OutTxt('t1',' 'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    call OutTxt('t2','Field transformed'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
  end Subroutine TransformLaplace2D

  Subroutine getNearestEps3D(i,j,k,epsijk,epsnjk,epspjk,epsink,epsipk,epsijn,epsijp)
! compute epsilon values in the 3D FD grid point ijk and its nearest neigbours
  Integer(2) iD
  Integer(4) i,j,k
  Real(8) epsijk,epsnjk,epspjk,epsink,epsipk,epsijn,epsijp,r0(3),dmin,rmin(3),val(2)
  k=1
  r0(1:3)=getcGrd(i,j,k)
  call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
  if(iD.lt.1) then ! point in electrode: no material properties needed
    epsijk=1.0d0
    epsnjk=0.0d0
    epspjk=0.0d0
    epsink=0.0d0
    epsipk=0.0d0
    epsijn=0.0d0
    epsijp=0.0d0
  else
    epsijk=Dble(eDom(iD))
    r0(1:3)=getcGrd(i-1,j,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsnjk=Dble(eDom(iD))
    if(iD.lt.1) epsnjk=epsijk
    r0(1:3)=getcGrd(i+1,j,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epspjk=Dble(eDom(iD))
    if(iD.lt.1) epspjk=epsijk
    r0(1:3)=getcGrd(i,j-1,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsink=Dble(eDom(iD))
    if(iD.lt.1) epsink=epsijk
    r0(1:3)=getcGrd(i,j+1,k)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsipk=Dble(eDom(iD))
    if(iD.lt.1) epsipk=epsijk
    r0(1:3)=getcGrd(i,j,k-1)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsijn=Dble(eDom(iD))
    if(iD.lt.1) epsijn=epsijk
    r0(1:3)=getcGrd(i,j,k+1)
    call DistPtObj(0_2,0_2,r0(1:3),.true.,dmin,rmin,iD,val,.true.)
    epsijp=Dble(eDom(iD))
    if(iD.lt.1) epsijp=epsijk
  end if
  end Subroutine getNearestEps3D

  Subroutine TransformLaplace3D(Initialize)
! transform the field with standard 3D Laplace operator
    Implicit none
    Include 'resource.fd'
    Real(8) epsijk,epsnjk,epspjk,epsink,epsipk,epsijn,epsijp
    Integer(4) i,j,k,iter,kk,km,i1,lout,ierr,idum
    Integer(2) lf
    Integer(4), intent(in) :: Initialize
    lfcFld=lfcFldAll
    call OutTxt('t2','L-transform field'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    call OutTxt('t1','Field point'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    if(Initialize.eq.1) then
! allocate memory for arrays
      DeAllocate(gax,gay,gaz,stat=ierr)
      if(.not.lVcFld) then
        idum=MessageBoxQQ('Scalar potential V not selected!'C,'3DL transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        return
      end if
      if(lfcFld) then
        idum=MessageBoxQQ('Frequncy must be 0!'C,'3DL transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        return
      end if
      Allocate(gax(nxcFld,nycFld,nzcFld),gay(nxcFld,nycFld,nzcFld),gaz(nxcFld,nycFld,nzcFld),stat=ierr)
      if(ierr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate memory!'C,'3DL transform'C, &
                          MB$OK.or.MB$ICONSTOP)
        DeAllocate(gax,gay,gaz,stat=ierr)
        return
      end if
      lPFDalloc=.true.
! initialize material parameters (containing grad(eps))
      gax=0.0d0
      gay=0.0d0
      gaz=0.0d0
      do i=2,nxcFld-1
        do j=2,nycFld-1
          do k=2,nzcFld-1
            if(lStopThread) return
            call getNearestEps3D(i,j,k,epsijk,epsnjk,epspjk,epsink,epsipk,epsijn,epsijp)
            gax(i,j,k)=(epspjk-epsnjk)/(24.0d0*epsijk)
            gay(i,j,k)=(epsipk-epsink)/(24.0d0*epsijk)
            gaz(i,j,k)=(epsijp-epsijn)/(24.0d0*epsijk)
          end do
        end do
      end do
    end if
! copy current to auxiliary field
    dAuxFld=dFld
! count field components in the array
    i1=0
    if(lxcFld.and.lEcFld) i1=i1+1
    if(lycFld.and.lEcFld) i1=i1+1
    if(lzcFld.and.lEcFld) i1=i1+1
    if(lxcFld.and.lHcFld) i1=i1+1
    if(lycFld.and.lHcFld) i1=i1+1
    if(lzcFld.and.lHcFld) i1=i1+1
    if(lxcFld.and.lAcFld) i1=i1+1
    if(lycFld.and.lAcFld) i1=i1+1
    if(lzcFld.and.lAcFld) i1=i1+1
    if(lVcFld) i1=i1+1
! iterations and loops over entire grid
    do iter=1,nIterPFD
      call OutTxt('t2','Field transform'C)
      call IntToStr(Int4(nIterPFD),0,0,SpaceText,lout)
      call OutTxt('m2',SpaceText(1:lout))
      call IntToStr(Int4(iter),0,0,SpaceText,lout)
      call OutTxt('n2',SpaceText(1:lout))
      kk=0
      km=Int4(nxcFld)*Int4(nycFld)*Int4(nzcFld)
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            if(lStopThread) return
            kk=kk+1
            call OutTxt('t1','Field point'C)
            call IntToStr(Int4(kk),0,0,SpaceText,lout)
            call OutTxt('n1',SpaceText(1:lout))
            call IntToStr(km,0,0,SpaceText,lout)
            call OutTxt('m1',SpaceText(1:lout))
! skip if 1) field domain number out of range or 2) point on border
            if((i.eq.1).or.(i.eq.nxcFld).or.(j.eq.1).or.(j.eq.nycFld).or.(k.eq.1).or.(k.eq.nzcFld)) Cycle
            lf=Max(1_2,Min(Int2(nDom),iFld(i,j,k)))
            if(lf.ne.iFld(i,j,k)) Cycle
! perform transformations of the existing field components
            dAuxFld(i1,i,j,k)=(dFld(i1,i+1,j,k)+dFld(i1,i-1,j,k)+dFld(i1,i,j+1,k)+ &
            &                    dFld(i1,i,j-1,k)+dFld(i1,i,j,k+1)+dFld(i1,i,j,k-1))/6.0d0
            if(dabs(gax(i,j,k)).gt.1e-30) dAuxFld(i1,i,j,k)=dAuxFld(i1,i,j,k)+gax(i,j,k)*(dFld(i1,i+1,j,k)-dFld(i1,i-1,j,k))
            if(dabs(gay(i,j,k)).gt.1e-30) dAuxFld(i1,i,j,k)=dAuxFld(i1,i,j,k)+gay(i,j,k)*(dFld(i1,i,j+1,k)-dFld(i1,i,j-1,k))
            if(dabs(gaz(i,j,k)).gt.1e-30) dAuxFld(i1,i,j,k)=dAuxFld(i1,i,j,k)+gaz(i,j,k)*(dFld(i1,i,j,k+1)-dFld(i1,i,j,k-1))
          end do
        end do
      end do
! copy auxiliary to current field
      dFld=dAuxFld
    end do
    call OutTxt('t1',' 'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    call OutTxt('t2','Field transformed'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
  end Subroutine TransformLaplace3D

  Subroutine TransformField(lCheck)
! transform the field using (predefined) FD schemes
    Implicit none
    Include 'resource.fd'
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
    call getEUST()
    if(abs(fcFld).gt.1.0d-30) then
      if(lxPeriod.or.lPFDc) then
        if(lgcFld) then
          if(iHEGlobal.eq.1_2) then
            if(lPFDalloc) then
              call TransformCFD2H(0_4) ! iterate without initialization
            else
              call TransformCFD2H(1_4) ! initialize and iterate
            end if
          else
            if(lyPeriod) then
              if(lPFDalloc) then
                call TransformCFD2EperXY(0_4) ! iterate without initialization
              else
                call TransformCFD2EperXY(1_4) ! initialize and iterate
              end if
            else
              if(lPFDalloc) then
                call TransformCFD2E(0_4) ! iterate without initialization
              else
                call TransformCFD2E(1_4) ! initialize and iterate
              end if
            end if
          end if
          call GetrField(.false.) ! compute field to be displayed
        else
          if(lPFDalloc) then
            call TransformCFD3D(0_4) ! iterate without initialization
          else
            call TransformCFD3D(1_4) ! initialize and iterate
          end if
          call GetrField(.false.) ! compute field to be displayed
        end if
      else
        if(lgcFld) then
          if(iHEGlobal.eq.1_2) then
            if(lPFDalloc) then
              call TransformPFD2H(0_4) ! iterate without initialization
            else
              call TransformPFD2H(1_4) ! initialize and iterate
            end if
          else
            if(lPFDalloc) then
              call TransformPFD2E(0_4) ! iterate without initialization
            else
              call TransformPFD2E(1_4) ! initialize and iterate
            end if
          end if
          call GetrField(.false.) ! compute field to be displayed
        else
          if(lPFDalloc) then
            call TransformPFD3D(0_4) ! iterate without initialization
          else
            call TransformPFD3D(1_4) ! initialize and iterate
          end if
          call GetrField(.false.) ! compute field to be displayed
        end if
      end if
    else
      if(lgcFld) then
        if(lPFDalloc) then
          call TransformLaplace2D(0_4) ! iterate without initialization
        else
          call TransformLaplace2D(1_4) ! initialize and iterate
        end if
      else
        if(lPFDalloc) then
          call TransformLaplace3D(0_4) ! iterate without initialization
        else
          call TransformLaplace3D(1_4) ! initialize and iterate
        end if
      end if
      call GetrField(.false.) ! compute field to be displayed
    end if
  end Subroutine TransformField

  Subroutine AbbrFld(id,i,j,k)
! auxiliary for ClearField: compute abbreviations 
    Implicit none
    Integer(4) i,j,k,lf,iErr
    Integer(2) id
    lfcFld=lfcFldAll
    lf=-1
    if(lfcFld) then
      vCForm(279+id:279+id)=cFormula(Q_Form(id,iQ_Form),lf,cCForm,pCForm,vCForm,8,11,999,i,j,k,iErr)
    else
      vForm(279+id:279+id)=Formula(Q_Form(id,iQ_Form),lf,cForm,pForm,vForm,8,11,999,i,j,k,iErr)
    end if
  end Subroutine AbbrFld

  Subroutine setF2V(n,i0,j0,k0)
! auxiliary for TransformField
    Implicit none
    Integer(4) n,i0,j0,k0,i,j,k
    i=i0
    j=j0
    k=k0
    if(i.lt.1) i=nxcFld
    if(i.gt.nxcFld) i=1
    if(j.lt.1) j=nycFld
    if(j.gt.nycFld) j=1
    if(k.lt.1) k=nzcFld
    if(k.gt.nzcFld) k=1
    if(lxcFld.and.lEcFld) then
      vForm(10*n+10)=dFld(iEx,i,j,k)
    end if
    if(lycFld.and.lEcFld) then
      vForm(10*n+11)=dFld(iEy,i,j,k)
    end if
    if(lzcFld.and.lEcFld) then
      vForm(10*n+12)=dFld(iEz,i,j,k)
    end if
    if(lxcFld.and.lHcFld) then
      vForm(10*n+13)=dFld(iHx,i,j,k)
    end if
    if(lycFld.and.lHcFld) then
      vForm(10*n+14)=dFld(iHy,i,j,k)
    end if
    if(lzcFld.and.lHcFld) then
      vForm(10*n+15)=dFld(iHz,i,j,k)
    end if
    if(lxcFld.and.lAcFld) then
      vForm(10*n+16)=dFld(iAx,i,j,k)
    end if
    if(lycFld.and.lAcFld) then
      vForm(10*n+17)=dFld(iAy,i,j,k)
    end if
    if(lzcFld.and.lAcFld) then
      vForm(10*n+18)=dFld(iAz,i,j,k)
    end if
    if(lVcFld) then
      vForm(10*n+19)=dFld(iV,i,j,k)
    end if
  end Subroutine setF2V

  Subroutine setCF2V(n,i0,j0,k0)
! auxiliary for TransformField
    Implicit none
    Integer(4) n,i0,j0,k0,i,j,k
    i=i0
    j=j0
    k=k0
    if(i.lt.1) i=nxcFld
    if(i.gt.nxcFld) i=1
    if(j.lt.1) j=nycFld
    if(j.gt.nycFld) j=1
    if(k.lt.1) k=nzcFld
    if(k.gt.nzcFld) k=1
    if(lxcFld.and.lEcFld) then
      vCForm(10*n+10)=cFld(iEx,i,j,k)
    end if
    if(lycFld.and.lEcFld) then
      vCForm(10*n+11)=cFld(iEy,i,j,k)
    end if
    if(lzcFld.and.lEcFld) then
      vCForm(10*n+12)=cFld(iEz,i,j,k)
    end if
    if(lxcFld.and.lHcFld) then
      vCForm(10*n+13)=cFld(iHx,i,j,k)
    end if
    if(lycFld.and.lHcFld) then
      vCForm(10*n+14)=cFld(iHy,i,j,k)
    end if
    if(lzcFld.and.lHcFld) then
      vCForm(10*n+15)=cFld(iHz,i,j,k)
    end if
    if(lxcFld.and.lAcFld) then
      vCForm(10*n+16)=cFld(iAx,i,j,k)
    end if
    if(lycFld.and.lAcFld) then
      vCForm(10*n+17)=cFld(iAy,i,j,k)
    end if
    if(lzcFld.and.lAcFld) then
      vCForm(10*n+18)=cFld(iAz,i,j,k)
    end if
    if(lVcFld) then
      vCForm(10*n+19)=cFld(iV,i,j,k)
    end if
  end Subroutine setCF2V

  Subroutine GetrField(lCheck)
! get real field, compute min/max values if lCheck=.true.
    Implicit none
    Include 'resource.fd'
    Logical, intent(in) :: lCheck
    Logical ldum
    Integer(4) j,k,i1,j1,k1,idum,nx,ny
    Real(8) dV(3),dV1(3),dV2(3),dum
    Complex(8) cV(3),cV1(3),cV2(3),ch,ch1
    lfcFld=lfcFldAll
    call getEUST()
    if(lSingleFldPoint) then
      nx=1
      ny=1
    else
      call OutTxt('t1','Compute real field'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
! allocate memory and clear rFld
      call Getnxy()
      if(Allocated(rFld)) DeAllocate(rFld)
      if(Allocated(drFld)) DeAllocate(drFld)
      if(Allocated(irFld)) DeAllocate(irFld)
      if(Allocated(jrFld)) DeAllocate(jrFld)
      if(Allocated(krFld)) DeAllocate(krFld)
      Allocate(rFld(0:3,nxrFld,nyrFld),drFld(nxrFld*nyrFld),irFld(nxrFld*nyrFld),jrFld(nxrFld*nyrFld), &
      &        krFld(nxrFld*nyrFld),stat=idum)
      if(idum.ne.0) then
        if(Allocated(rFld)) DeAllocate(rFld)
        if(Allocated(drFld)) DeAllocate(drFld)
        if(Allocated(irFld)) DeAllocate(irFld)
        if(Allocated(jrFld)) DeAllocate(jrFld)
        if(Allocated(krFld)) DeAllocate(krFld)
        Allocate(rFld(0:3,nxrFld,nyrFld),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for rFld failed!'C,'Get real field'C, &
                            MB$OK.or.MB$IconExclamation)
          nxrFld=0
          nyrFld=0
          return
        end if
      end if
      do j=1,nyrFld
        do k=1,nxrFld
          rFld(0,k,j)=0.0d0
          rFld(1,k,j)=0.0d0
          rFld(2,k,j)=0.0d0
          rFld(3,k,j)=0.0d0
        end do
      end do
      nx=nxrFld
      ny=nyrFld
    end if
    if(lfcFld) then
      ch=cdexp(-Ci*Modulo(Dble(fcFld)*trFld,1.0d0)*2.0d0*Pi)*dexp(DImag(fcFld)*trFld*2.0d0*Pi)
    end if
    do j=1,ny
      do k=1,nx
        fSingleFldPoint=0.0d0
        if(.not.lSingleFldPoint) then
          call Getijk(k,j,k1,j1,i1)
          iSingleFldPoint=iFld(k1,j1,i1)
          if(lfcFld) then
            cSingleFldPoint(1:10)=(0.0d0,0.0d0)
            cSingleFldPoint(1:ncFld)=cFld(1:ncFld,k1,j1,i1)
          else
            dSingleFldPoint(1:10)=0.0d0
            dSingleFldPoint(1:ncFld)=dFld(1:ncFld,k1,j1,i1)
          end if
        end if
        Select Case(itrFld)
        Case(itE)
          if(.not.lEcFld) return
          if(larFld) then
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiAbs(lprFld,cSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiAbs(lprFld,cSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiAbs(lprFld,cSingleFldPoint(iEz))
            else
              if(lxrFld) fSingleFldPoint(1)=dabs(dSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=dabs(dSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=dabs(dSingleFldPoint(iEz))
            end if
          else
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,ch*cSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,ch*cSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,ch*cSingleFldPoint(iEz))
            else
              if(lxrFld) fSingleFldPoint(1)=dSingleFldPoint(iEx)
              if(lyrFld) fSingleFldPoint(2)=dSingleFldPoint(iEy)
              if(lzrFld) fSingleFldPoint(3)=dSingleFldPoint(iEz)
            end if
          end if
        Case(itH)
          if(.not.lHcFld) return
          if(larFld) then
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiAbs(lprFld,cSingleFldPoint(iHx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiAbs(lprFld,cSingleFldPoint(iHy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiAbs(lprFld,cSingleFldPoint(iHz))
            else
              if(lxrFld) fSingleFldPoint(1)=dabs(dSingleFldPoint(iHx))
              if(lyrFld) fSingleFldPoint(2)=dabs(dSingleFldPoint(iHy))
              if(lzrFld) fSingleFldPoint(3)=dabs(dSingleFldPoint(iHz))
            end if
          else
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,ch*cSingleFldPoint(iHx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,ch*cSingleFldPoint(iHy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,ch*cSingleFldPoint(iHz))
            else
              if(lxrFld) fSingleFldPoint(1)=dSingleFldPoint(iHx)
              if(lyrFld) fSingleFldPoint(2)=dSingleFldPoint(iHy)
              if(lzrFld) fSingleFldPoint(3)=dSingleFldPoint(iHz)
            end if
          end if
        Case(itS)
          if(.not.lEcFld) return
          if(.not.lHcFld) return
          if(larFld) then
            if(lfcFld) then
              cV1(1)=cSingleFldPoint(iEx)
              cV1(2)=cSingleFldPoint(iEy)
              cV1(3)=cSingleFldPoint(iEz)
              cV2(1)=cSingleFldPoint(iHx)
              cV2(2)=cSingleFldPoint(iHy)
              cV2(3)=cSingleFldPoint(iHz)
              cV=c3Vec_Prod(cV1,Conjg(cV2))
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,cV(1))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,cV(2))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,cV(3))
            else
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              dV2(1)=dSingleFldPoint(iHx)
              dV2(2)=dSingleFldPoint(iHy)
              dV2(3)=dSingleFldPoint(iHz)
              dV=r3Vec_Prod(dV1,dV2)
              if(lxrFld) fSingleFldPoint(1)=Dble(dV(1))
              if(lyrFld) fSingleFldPoint(2)=Dble(dV(2))
              if(lzrFld) fSingleFldPoint(3)=Dble(dV(3))
            end if
          else
            if(lfcFld) then
              cV1(1)=ch*cSingleFldPoint(iEx)
              cV1(2)=ch*cSingleFldPoint(iEy)
              cV1(3)=ch*cSingleFldPoint(iEz)
              cV2(1)=ch*cSingleFldPoint(iHx)
              cV2(2)=ch*cSingleFldPoint(iHy)
              cV2(3)=ch*cSingleFldPoint(iHz)
              if(lprFld) then
                cV=c3Vec_Prod(cV1,cV2)
                if(lxrFld) fSingleFldPoint(1)=cdPhi(cV(1))
                if(lyrFld) fSingleFldPoint(2)=cdPhi(cV(2))
                if(lzrFld) fSingleFldPoint(3)=cdPhi(cV(3))
              else
                dV=r3Vec_Prod(Dble(cV1),Dble(cV2))
                if(lxrFld) fSingleFldPoint(1)=dV(1)
                if(lyrFld) fSingleFldPoint(2)=dV(2)
                if(lzrFld) fSingleFldPoint(3)=dV(3)
              end if
            else
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              dV2(1)=dSingleFldPoint(iHx)
              dV2(2)=dSingleFldPoint(iHy)
              dV2(3)=dSingleFldPoint(iHz)
              dV=r3Vec_Prod(dV1,dV2)
              if(lxrFld) fSingleFldPoint(1)=dV(1)
              if(lyrFld) fSingleFldPoint(2)=dV(2)
              if(lzrFld) fSingleFldPoint(3)=dV(3)
            end if
          end if
        Case(itD)
          if(.not.lEcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          ch1=eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0
          if(larFld) then
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iEz))
            else
              if(lxrFld) fSingleFldPoint(1)=dabs(Dble(ch1)*dSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=dabs(Dble(ch1)*dSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=dabs(Dble(ch1)*dSingleFldPoint(iEz))
            end if
          else
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iEz))
            else
              if(lxrFld) fSingleFldPoint(1)=Dble(ch1)*dSingleFldPoint(iEx)
              if(lyrFld) fSingleFldPoint(2)=Dble(ch1)*dSingleFldPoint(iEy)
              if(lzrFld) fSingleFldPoint(3)=Dble(ch1)*dSingleFldPoint(iEz)
            end if
          end if
        Case(itJ)
          if(.not.lEcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          if(lfcFld) then
            ch1=Dimag(eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Eps0*2.0d0*Pi*fcFld
          else
            ch1=sDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))
          end if
          if(larFld) then
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iEz))
            else
              if(lxrFld) fSingleFldPoint(1)=dabs(Dble(ch1)*dSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=dabs(Dble(ch1)*dSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=dabs(Dble(ch1)*dSingleFldPoint(iEz))
            end if
          else
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iEx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iEy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iEz))
            else
              if(lxrFld) fSingleFldPoint(1)=Dble(ch1)*dSingleFldPoint(iEx)
              if(lyrFld) fSingleFldPoint(2)=Dble(ch1)*dSingleFldPoint(iEy)
              if(lzrFld) fSingleFldPoint(3)=Dble(ch1)*dSingleFldPoint(iEz)
            end if
          end if
        Case(itB)
          if(.not.lHcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
          if(larFld) then
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iHx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iHy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiAbs(lprFld,ch1*cSingleFldPoint(iHz))
            else
              if(lxrFld) fSingleFldPoint(1)=dabs(Dble(ch1)*dSingleFldPoint(iHx))
              if(lyrFld) fSingleFldPoint(2)=dabs(Dble(ch1)*dSingleFldPoint(iHy))
              if(lzrFld) fSingleFldPoint(3)=dabs(Dble(ch1)*dSingleFldPoint(iHz))
            end if
          else
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iHx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iHy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,ch1*ch*cSingleFldPoint(iHz))
            else
              if(lxrFld) fSingleFldPoint(1)=Dble(ch1)*dSingleFldPoint(iHx)
              if(lyrFld) fSingleFldPoint(2)=Dble(ch1)*dSingleFldPoint(iHy)
              if(lzrFld) fSingleFldPoint(3)=Dble(ch1)*dSingleFldPoint(iHz)
            end if
          end if
        Case(itA)
          if(.not.lAcFld) return
          if(larFld) then
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiAbs(lprFld,cSingleFldPoint(iAx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiAbs(lprFld,cSingleFldPoint(iAy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiAbs(lprFld,cSingleFldPoint(iAz))
            else
              if(lxrFld) fSingleFldPoint(1)=dabs(dSingleFldPoint(iAx))
              if(lyrFld) fSingleFldPoint(2)=dabs(dSingleFldPoint(iAy))
              if(lzrFld) fSingleFldPoint(3)=dabs(dSingleFldPoint(iAz))
            end if
          else
            if(lfcFld) then
              if(lxrFld) fSingleFldPoint(1)=cdPhiRea(lprFld,ch*cSingleFldPoint(iAx))
              if(lyrFld) fSingleFldPoint(2)=cdPhiRea(lprFld,ch*cSingleFldPoint(iAy))
              if(lzrFld) fSingleFldPoint(3)=cdPhiRea(lprFld,ch*cSingleFldPoint(iAz))
            else
              if(lxrFld) fSingleFldPoint(1)=dSingleFldPoint(iAx)
              if(lyrFld) fSingleFldPoint(2)=dSingleFldPoint(iAy)
              if(lzrFld) fSingleFldPoint(3)=dSingleFldPoint(iAz)
            end if
          end if
        Case(itV)
          if(.not.lVcFld) return
          if(larFld) then
            if(lfcFld) then
              fSingleFldPoint(1)=cdPhiAbs(lprFld,cSingleFldPoint(iV))
            else
              fSingleFldPoint(1)=dabs(dSingleFldPoint(iV))
            end if
          else
            if(lfcFld) then
              fSingleFldPoint(1)=cdPhiRea(lprFld,ch*cSingleFldPoint(iV))
            else
              fSingleFldPoint(1)=dSingleFldPoint(iV)
            end if
          end if
        Case(itWe)
          if(.not.lEcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          ch1=eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0
          if(larFld) then
            if(lfcFld) then
              cV1(1)=cSingleFldPoint(iEx)
              cV1(2)=cSingleFldPoint(iEy)
              cV1(3)=cSingleFldPoint(iEz)
              cV2=Conjg(cV1)
              fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
            else
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dabs(Dble(ch1)*r3Scl_Prod(dV1,dV1))
            end if
          else
            if(lfcFld) then
              cV1(1)=ch*cSingleFldPoint(iEx)
              cV1(2)=ch*cSingleFldPoint(iEy)
              cV1(3)=ch*cSingleFldPoint(iEz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(1)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(1)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
            else
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=Dble(ch1)*r3Scl_Prod(dV1,dV1)
            end if
          end if
        Case(itWh)
          if(.not.lHcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
          if(larFld) then
            if(lfcFld) then
              cV1(1)=cSingleFldPoint(iHx)
              cV1(2)=cSingleFldPoint(iHy)
              cV1(3)=cSingleFldPoint(iHz)
              cV2=Conjg(cV1)
              fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
            else
              dV1(1)=dSingleFldPoint(iHx)
              dV1(2)=dSingleFldPoint(iHy)
              dV1(3)=dSingleFldPoint(iHz)
              fSingleFldPoint(1)=dabs(Dble(ch1)*r3Scl_Prod(dV1,dV1))
            end if
          else
            if(lfcFld) then
              cV1(1)=ch*cSingleFldPoint(iHx)
              cV1(2)=ch*cSingleFldPoint(iHy)
              cV1(3)=ch*cSingleFldPoint(iHz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(1)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(1)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
            else
              dV1(1)=dSingleFldPoint(iHx)
              dV1(2)=dSingleFldPoint(iHy)
              dV1(3)=dSingleFldPoint(iHz)
              fSingleFldPoint(1)=Dble(ch1)*r3Scl_Prod(dV1,dV1)
            end if
          end if
        Case(itWt)
          if(.not.lEcFld) return
          if(.not.lHcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          if(larFld) then
            if(lfcFld) then
              ch1=eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0
              cV1(1)=cSingleFldPoint(iEx)
              cV1(2)=cSingleFldPoint(iEy)
              cV1(3)=cSingleFldPoint(iEz)
              cV2=Conjg(cV1)
              fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
              ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
              cV1(1)=cSingleFldPoint(iHx)
              cV1(2)=cSingleFldPoint(iHy)
              cV1(3)=cSingleFldPoint(iHz)
              cV2=Conjg(cV1)
              fSingleFldPoint(2)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
            else
              ch1=eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dabs(Dble(ch1)*r3Scl_Prod(dV1,dV1))
              ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
              dV1(1)=dSingleFldPoint(iHx)
              dV1(2)=dSingleFldPoint(iHy)
              dV1(3)=dSingleFldPoint(iHz)
              fSingleFldPoint(2)=dabs(Dble(ch1)*r3Scl_Prod(dV1,dV1))
            end if
          else
            if(lfcFld) then
              ch1=eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0
              cV1(1)=ch*cSingleFldPoint(iEx)
              cV1(2)=ch*cSingleFldPoint(iEy)
              cV1(3)=ch*cSingleFldPoint(iEz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(1)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(1)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
              ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
              cV1(1)=ch*cSingleFldPoint(iHx)
              cV1(2)=ch*cSingleFldPoint(iHy)
              cV1(3)=ch*cSingleFldPoint(iHz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(2)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(2)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
            else
              ch1=eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=Dble(ch1)*r3Scl_Prod(dV1,dV1)
              ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
              dV1(1)=dSingleFldPoint(iHx)
              dV1(2)=dSingleFldPoint(iHy)
              dV1(3)=dSingleFldPoint(iHz)
              fSingleFldPoint(2)=Dble(ch1)*r3Scl_Prod(dV1,dV1)
            end if
          end if
        Case(itF)
          if(.not.lEcFld) return
          if(.not.lHcFld) return
          if(.not.lfcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          ch1=-0.25d0*eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Eps0 ! -eps/4
          cV1(1)=cSingleFldPoint(iEx)
          cV1(2)=cSingleFldPoint(iEy)
          cV1(3)=cSingleFldPoint(iEz) ! E
          cV2=Conjg(cV1)              ! E*
          ch=c3Scl_Prod(cV1,cV2)      ! (E.E*)
          cV(1:3)=ch1*cdAbs(ch)*vBndNorm(1:3) ! -(eps/4).(E.E*)n
          fSingleFldPoint(1:3)=cV(1:3)
          ch1=-2.0d0*ch1              ! +eps/2
          ch=Dot_Product(cV2,vBndNorm)! (E.n) Note: dot_product(a,b)=a*.b
          cV2=ch*cV2                  ! (E.n).E*
          cV(1:3)=ch1*Dble(cV2)       ! (eps/2).Real((E.n).E*)
          fSingleFldPoint(1:3)=fSingleFldPoint(1:3)+cV(1:3)
          ch1=-0.25d0*uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0 ! -mue/4
          cV1(1)=cSingleFldPoint(iHx)
          cV1(2)=cSingleFldPoint(iHy)
          cV1(3)=cSingleFldPoint(iHz) ! H
          cV2=Conjg(cV1)              ! H*
          ch=c3Scl_Prod(cV1,cV2)      ! (H.H*)
          cV(1:3)=ch1*cdAbs(ch)*vBndNorm(1:3) ! -(mue/4).(H.H*)n
          fSingleFldPoint(1:3)=fSingleFldPoint(1:3)+cV(1:3)
          ch1=-2.0d0*ch1              ! +mue/2
          ch=Dot_Product(cV2,vBndNorm)! (H.n) Note: dot_product(a,b)=a*.b
          cV2=ch*cV2                  ! (H.n).H*
          cV(1:3)=ch1*Dble(cV2)       ! (mue/2).Real((H.n).H*)
          fSingleFldPoint(1:3)=fSingleFldPoint(1:3)+cV(1:3)
        Case(itPe)
          if(.not.lEcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          if(larFld) then
            if(lfcFld) then
              ch1=Dimag(eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Eps0*2.0d0*Pi*fcFld
              cV1(1)=cSingleFldPoint(iEx)
              cV1(2)=cSingleFldPoint(iEy)
              cV1(3)=cSingleFldPoint(iEz)
              cV2=Conjg(cV1)
              fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
            else
              dum=Dble(sDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dabs(dum*r3Scl_Prod(dV1,dV1))
            end if
          else
            if(lfcFld) then
              ch1=Dimag(eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Eps0*2.0d0*Pi*fcFld
              cV1(1)=ch*cSingleFldPoint(iEx)
              cV1(2)=ch*cSingleFldPoint(iEy)
              cV1(3)=ch*cSingleFldPoint(iEz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(1)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(1)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
            else
              dum=Dble(sDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dum*r3Scl_Prod(dV1,dV1)
            end if
          end if
        Case(itPh)
          if(.not.lHcFld) return
          if((iSingleFldPoint.lt.1).or.(iSingleFldPoint.gt.nDom)) Cycle
          ch1=uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint)))*Mue0
          if(larFld) then
            if(lfcFld) then
              ch1=Dimag(uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Mue0*2.0d0*Pi*fcFld
              cV1(1)=cSingleFldPoint(iHx)
              cV1(2)=cSingleFldPoint(iHy)
              cV1(3)=cSingleFldPoint(iHz)
              cV2=Conjg(cV1)
              fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
            else
              dum=Dble(tDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dabs(dum*r3Scl_Prod(dV1,dV1))
            end if
          else
            if(lfcFld) then
              ch1=Dimag(uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Mue0*2.0d0*Pi*fcFld
              cV1(1)=ch*cSingleFldPoint(iHx)
              cV1(2)=ch*cSingleFldPoint(iHy)
              cV1(3)=ch*cSingleFldPoint(iHz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(1)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(1)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
            else
              dum=Dble(tDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dum*r3Scl_Prod(dV1,dV1)
            end if
          end if
        Case(itPt)
          if(.not.lEcFld) return
          if(.not.lHcFld) return
          if((iSingleFldPoint.lt.1_2).or.(iSingleFldPoint.gt.Int2(nDom))) Cycle
          if(larFld) then
            if(lfcFld) then
              ch1=Dimag(eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Eps0*2.0d0*Pi*fcFld
              cV1(1)=cSingleFldPoint(iEx)
              cV1(2)=cSingleFldPoint(iEy)
              cV1(3)=cSingleFldPoint(iEz)
              cV2=Conjg(cV1)
              fSingleFldPoint(1)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
              ch1=Dimag(uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Mue0*2.0d0*Pi*fcFld
              cV1(1)=cSingleFldPoint(iHx)
              cV1(2)=cSingleFldPoint(iHy)
              cV1(3)=cSingleFldPoint(iHz)
              cV2=Conjg(cV1)
              fSingleFldPoint(2)=cdPhiAbs(lprFld,ch1*c3Scl_Prod(cV1,cV2))
            else
              dum=Dble(sDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dabs(dum*r3Scl_Prod(dV1,dV1))
              dum=Dble(tDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(2)=dabs(dum*r3Scl_Prod(dV1,dV1))
            end if
          else
            if(lfcFld) then
              ch1=Dimag(eDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Eps0*2.0d0*Pi*fcFld
              cV1(1)=ch*cSingleFldPoint(iEx)
              cV1(2)=ch*cSingleFldPoint(iEy)
              cV1(3)=ch*cSingleFldPoint(iEz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(1)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(1)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
              ch1=Dimag(uDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))*Mue0*2.0d0*Pi*fcFld
              cV1(1)=ch*cSingleFldPoint(iHx)
              cV1(2)=ch*cSingleFldPoint(iHy)
              cV1(3)=ch*cSingleFldPoint(iHz)
              cV2=ch1*cV1
              if(lprFld) then
                fSingleFldPoint(2)=cdPhi(c3Scl_Prod(cV1,cV2))
              else
                fSingleFldPoint(2)=r3Scl_Prod(Dble(cV1),Dble(cV2))
              end if
            else
              dum=Dble(sDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(1)=dum*r3Scl_Prod(dV1,dV1)
              dum=Dble(tDom(Max(1_2,Min(Int2(nDom),iSingleFldPoint))))
              dV1(1)=dSingleFldPoint(iEx)
              dV1(2)=dSingleFldPoint(iEy)
              dV1(3)=dSingleFldPoint(iEz)
              fSingleFldPoint(2)=dum*r3Scl_Prod(dV1,dV1)
            end if
          end if
	      end Select
        if(lSingleFldPoint) return
        rFld(1:3,k,j)=fSingleFldPoint
      end do
    end do
    if((itrFld.eq.itWe).or.(itrFld.eq.itWh).or.(itrFld.eq.itWt).or. &
    &  (itrFld.eq.itPe).or.(itrFld.eq.itPh).or.(itrFld.eq.itPt).or. &
    &  (itrFld.eq.itPe)) then
      ldum=.true.
    else
      ldum=.false.
      idum=0
      if(lxrFld) idum=idum+1
      if(lyrFld) idum=idum+1
      if(lzrFld) idum=idum+1
      if(idum.lt.2) ldum=.true.
    end if
    do j=1,nyrFld
      do k=1,nxrFld
        if(ldum) then
          rFld(0,k,j)=rFld(1,k,j)+rFld(2,k,j)+rFld(3,k,j)
        else
          rFld(0,k,j)=r3Vec_Length(rFld(1:3,k,j))
        end if
      end do
    end do
    if(abs(FactorFld).gt.pSmall) then ! scale derived field with factor
      if(abs(FactorFld-1.0d0).gt.1.0d-14) then
        do j=1,nyrFld
          do k=1,nxrFld
            rFld(0,k,j)=rFld(0,k,j)*FactorFld
          end do
        end do
      end if
    end if
    if(abs(PowerFld).gt.pSmall) then ! post processing: power or logarithm
      if(abs(PowerFld-1.0d0).gt.1.0d-14) then
        do j=1,nyrFld
          do k=1,nxrFld
            rFld(0,k,j)=rFld(0,k,j).pow.PowerFld
          end do
        end do
      end if
    else
      do j=1,nyrFld
        do k=1,nxrFld
          rFld(0,k,j)=.log.rFld(0,k,j)
        end do
      end do
    end if
    if(lCheck) call getLimitsrField()
    if(.not.lSingleFldPoint) then
      call OutTxt('t1','Real field computed'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
    end if
  end Subroutine GetrField

  Subroutine getLimitsrField()
    Implicit none
    Integer(4) j,k
    rSumFld=0.0d0
    rSum2Fld=0.0d0
    rMinFld=pBig
    rMaxFld=nBig
    do j=1,nyrFld
      do k=1,nxrFld
        if(rMinFld.gt.rFld(0,k,j)) rMinFld=rFld(0,k,j)
        if(rMaxFld.lt.rFld(0,k,j)) rMaxFld=rFld(0,k,j)
        rSumFld=rSumFld+dabs(rFld(0,k,j))
        rSum2Fld=rSum2Fld+(rFld(0,k,j))**2
      end do
    end do
    rSumFld=rSumFld/(Dble(nxrFld)*Dble(nyrFld))
    rSum2Fld=dsqrt(rSum2Fld)/(Dble(nxrFld)*Dble(nyrFld))
  end Subroutine getLimitsrField

  Subroutine getGradrField()
    Implicit none
    Integer(4) i,k
    Real(8) dxrFld,dyrFld
    dxrFld=dble(Max(1,nxcFld-1))/r3Vec_Length(spacecFld(1:3,1))
    dyrFld=dble(Max(1,nycFld-1))/r3Vec_Length(spacecFld(1:3,2))
    rFld(1,1,1:nyrFld)=(rFld(0,2,1:nyrFld)-rFld(0,1,1:nyrFld))*dxrFld
    do i=2,nxrFld-1
      rFld(1,i,1:nyrFld)=(rFld(0,i+1,1:nyrFld)-rFld(0,i-1,1:nyrFld))*0.5*dxrFld
    end do
    rFld(1,nxrFld,1:nyrFld)=(rFld(0,nxrFld,1:nyrFld)-rFld(0,nxrFld-1,1:nyrFld))*dxrFld
    rFld(2,1:nxrFld,1)=(rFld(0,1:nxrFld,2)-rFld(0,1:nxrFld,1))*dyrFld
    do k=2,nyrFld-1
      rFld(2,1:nxrFld,k)=(rFld(0,1:nxrFld,k+1)-rFld(0,1:nxrFld,k-1))*0.5*dyrFld
    end do
    rFld(2,1:nxrFld,nyrFld)=(rFld(0,1:nxrFld,nyrFld)-rFld(0,1:nxrFld,nyrFld-1))*dyrFld
    do i=1,nxrFld
      do k=1,nyrFld
        rFld(0,i,k)=r3Vec_Length(rFld(1:3,i,k))
      end do
    end do
  end Subroutine getGradrField

  Subroutine GetLocrField(r,iDomLoc,inter,f,z)
! compute (real) field f in the point r within domain iDomLoc
! inter=type of interpolation
!        0: use field definition formula
!        1: field of nearest point, no interpolation
!        2: 3 point interpolation in xy plane
!        3: 4 point interpolation in xy plane
!        4: 3 point interpolation in 3D space
!        5: 4 point interpolation in 3D space
    Implicit none
    Real(8) r(3),f(3),f1(3),f2(3),f3(3),f4(3),d1,d2,r1(3),r2(3),r3(3),r4(3),dmin,rNmin(3),val(2),omega,v
    real(8), optional:: z
    Integer(4) i0min,j0min,i1,i2,j1,j2,inter
    Integer(2) iDomLoc
    if(inter.le.0) then ! use Fld_Form formula
      rSingleFldPoint=r
      iSingleFldPoint=iDomLoc
      if(iSingleFldPoint.lt.0_2) then
        call DistPtObj(0_2,0_2,r,.true.,dmin,rNmin,iSingleFldPoint,val,.true.)
        !!! iSingleFldPoint=Max(1_2,iSingleFldPoint)
      end if
      if(iSingleFldPoint.lt.1_2) then
        f(1:3)=0.0d0
      else
        lSingleFldPoint=.true.
        call ClearField(.true.)
        if(present(z)) then ! eBeam
          omega=2.0d0*Pi*fcFld
          v=tExp(nExp)%rE(1)*omega/kcFld ! here: radius is v/c
          f(1:3)=Dble(FldExp(1:3)*exp((0.0d0,-1.0d0)*omega*z/v))
          lSingleFldPoint=.false.
          return
        end if
        call GetrField(.false.)
        f=fSingleFldPoint
        lSingleFldPoint=.false.
      end if
      return
    end if
    call getNextrGrd(r,i0min,j0min)
    if(inter.eq.1) then
      f(1:3)=rFld(1:3,i0min,j0min)
      return
    end if
    if(i0min.lt.2) then ! get i1,i2
      i1=1
      i2=2
    else if(i0min.gt.(nxrFld-1)) then
      i1=nxrFld-1
      i2=nxrFld
    else
      f1=GetrGrd(i0min-1,j0min)-r
      d1=r3Vec_Length(f1)
      f1=GetrGrd(i0min+1,j0min)-r
      d2=r3Vec_Length(f1)
      if(d1.gt.d2) then
        i1=i0min
        i2=i1+1
      else
        i2=i0min
        i1=i2-1
      end if
    end if
    if(j0min.lt.2) then ! get j1,j2
      j1=1
      j2=2
    else if(j0min.gt.(nyrFld-1)) then
      j1=nyrFld-1
      j2=nyrFld
    else
      f1=GetrGrd(i0min,j0min-1)-r
      d1=r3Vec_Length(f1)
      f1=GetrGrd(i0min,j0min+1)-r
      d2=r3Vec_Length(f1)
      if(d1.gt.d2) then
        j1=j0min
        j2=j1+1
      else
        j2=j0min
        j1=j2-1
      end if
    end if
    r1=GetrGrd(i1,j1)
    r2=GetrGrd(i2,j1)
    r3=GetrGrd(i2,j2)
    r4=GetrGrd(i1,j2)
    f1(1:3)=rFld(1:3,i1,j1)
    f2(1:3)=rFld(1:3,i2,j1)
    f3(1:3)=rFld(1:3,i2,j2)
    f4(1:3)=rFld(1:3,i1,j2)
    if(inter.eq.2) then
      f=Inter32Fld(r(1:2),r1(1:2),r2(1:2),r3(1:2),f1,f2,f3)
    else if(inter.eq.4) then
      f=Inter33Fld(r,r1,r2,r3,f1,f2,f3)
    else if(inter.eq.3) then
      f=Inter42Fld(r(1:2),r1(1:2),r2(1:2),r3(1:2),r4(1:2),f1,f2,f3,f4)
    else
      f=Inter43Fld(r,r1,r2,r3,r4,f1,f2,f3,f4)
    end if
  end Subroutine GetLocrField

  Subroutine GetLoccField(r,iDomLoc,inter,cf)
! compute (complex) field cf in the point r within domain iDomLoc
! inter=type of interpolation
!        0: use field definition formula
!        1: field of nearest point, no interpolation
!        2: 3 point interpolation in xy plane
!        3: 4 point interpolation in xy plane
!        4: 3 point interpolation in 3D space
!        5: 4 point interpolation in 3D space
    Implicit none
    Complex(8) cf(10),cf1(10),cf2(10),cf3(10),cf4(10)
    Real(8) d1,d2,f1(3),r(3),r1(3),r2(3),r3(3),r4(3),dmin,rNmin(3),val(2)
    Integer(4) inter,j0min,i1,i2,j1,j2,i1a,j1a,k1a,i1b,j1b,k1b,i1c,j1c,k1c,i1d,j1d,k1d,i0min
    Integer(2) iDomLoc
    cf=(0.0d0,0.0d0)
    if(inter.eq.0) then ! use Fld_Form formula
      rSingleFldPoint=r
      iSingleFldPoint=iDomLoc
      if(iSingleFldPoint.lt.0_2) then
        call DistPtObj(0_2,0_2,r,.true.,dmin,rNmin,iSingleFldPoint,val,.true.)
        iSingleFldPoint=Max(1_2,iSingleFldPoint)
      end if
      lSingleFldPoint=.true.
      call ClearField(.true.)
      if(lfcFld) then
        call get10cFld(cSingleFldPoint(1:ncFld),ncFld,cf)
      else
        call get10dcFld(dSingleFldPoint(1:ncFld),ncFld,cf)
      end if
      if(iEx.gt.0_2) cf(1)=cSingleFldPoint(iEx)
      lSingleFldPoint=.false.
      return
    end if
    call getNextrGrd(r,i0min,j0min)
    if(inter.eq.1) then
      if(lfcFld) then
        call get10cFld(cFld(1:ncFld,i0min,j0min,1),ncFld,cf)
      else
        call get10dcFld(dFld(1:ncFld,i0min,j0min,1),ncFld,cf)
      end if
      return
    end if
    if(i0min.lt.2) then ! get i1,i2
      i1=1
      i2=2
    else if(i0min.gt.(nxrFld-1)) then
      i1=nxrFld-1
      i2=nxrFld
    else
      f1=GetrGrd(i0min-1,j0min)-r
      d1=r3Vec_Length(f1)
      f1=GetrGrd(i0min+1,j0min)-r
      d2=r3Vec_Length(f1)
      if(d1.gt.d2) then
        i1=i0min
        i2=i1+1
      else
        i2=i0min
        i1=i2-1
      end if
    end if
    if(j0min.lt.2) then ! get j1,j2
      j1=1
      j2=2
    else if(j0min.gt.(nyrFld-1)) then
      j1=nyrFld-1
      j2=nyrFld
    else
      f1=GetrGrd(i0min,j0min-1)-r
      d1=r3Vec_Length(f1)
      f1=GetrGrd(i0min,j0min+1)-r
      d2=r3Vec_Length(f1)
      if(d1.gt.d2) then
        j1=j0min
        j2=j1+1
      else
        j2=j0min
        j1=j2-1
      end if
    end if
    call Getijk(i1,j1,i1a,j1a,k1a)
    r=GetcGrd(i1a,j1a,k1a)
    call Getijk(i1,j1,i1b,j1b,k1b)
    r=GetcGrd(i1b,j1b,k1b)
    call Getijk(i1,j1,i1c,j1c,k1c)
    r=GetcGrd(i1c,j1c,k1c)
    call Getijk(i1,j1,i1d,j1d,k1d)
    r=GetcGrd(i1d,j1d,k1d)
    if(lfcFld) then
      call get10cFld(cFld(1:ncFld,i1a,j1a,k1a),ncFld,cf1)
      call get10cFld(cFld(1:ncFld,i1b,j1b,k1b),ncFld,cf2)
      call get10cFld(cFld(1:ncFld,i1c,j1c,k1c),ncFld,cf3)
      call get10cFld(cFld(1:ncFld,i1d,j1d,k1d),ncFld,cf4)
    else
      call get10dcFld(dFld(1:ncFld,i1a,j1a,k1a),ncFld,cf1)
      call get10dcFld(dFld(1:ncFld,i1b,j1b,k1b),ncFld,cf2)
      call get10dcFld(dFld(1:ncFld,i1c,j1c,k1c),ncFld,cf3)
      call get10dcFld(dFld(1:ncFld,i1d,j1d,k1d),ncFld,cf4)
    end if
    if(inter.eq.2) then
      cf=Inter32cFld(r(1:2),r1(1:2),r2(1:2),r3(1:2),cf1,cf2,cf3)
    else if(inter.eq.4) then
      cf=Inter33cFld(r,r1,r2,r3,cf1,cf2,cf3)
    else if(inter.eq.3) then
      cf=Inter42cFld(r(1:2),r1(1:2),r2(1:2),r3(1:2),r4(1:2),cf1,cf2,cf3,cf4)
    else
      cf=Inter43cFld(r,r1,r2,r3,r4,cf1,cf2,cf3,cf4)
    end if
  end Subroutine GetLoccField

  Subroutine get10cFld(cin,n,cout)
! compute complex field cout with 10 components 
! from compressed complex field cin with n components
    Implicit none
    Integer(4) n,i
    Complex(8) cin(n),cout(10)
    cout=(0.0d0,0.0d0)
    i=1
    if(i.gt.n) return
    if(iEx.gt.0_2) then
      cout(1)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iEy.gt.0_2) then
      cout(2)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iEz.gt.0_2) then
      cout(3)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iHx.gt.0_2) then
      cout(4)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iHy.gt.0_2) then
      cout(5)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iHz.gt.0_2) then
      cout(6)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iAx.gt.0_2) then
      cout(7)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iAy.gt.0_2) then
      cout(8)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iAz.gt.0_2) then
      cout(9)=cin(i)
      i=i+1
    end if
    if(i.gt.n) return
    if(iV.gt.0_2) then
      cout(10)=cin(i)
      i=i+1
    end if
  end Subroutine get10cFld

  Subroutine get10dcFld(din,n,cout)
! compute complex field cout with 10 components 
! from compressed real field din with n components
    Implicit none
    Integer(4) n,i
    Complex(8) cout(10)
    Real(8) din(n)
    cout=(0.0d0,0.0d0)
    i=1
    if(i.gt.n) return
    if(iEx.gt.0_2) then
      cout(1)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iEy.gt.0_2) then
      cout(2)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iEz.gt.0_2) then
      cout(3)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iHx.gt.0_2) then
      cout(4)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iHy.gt.0_2) then
      cout(5)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iHz.gt.0_2) then
      cout(6)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iAx.gt.0_2) then
      cout(7)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iAy.gt.0_2) then
      cout(8)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iAz.gt.0_2) then
      cout(9)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
    if(i.gt.n) return
    if(iV.gt.0_2) then
      cout(10)=Dcmplx(din(i),0.0d0)
      i=i+1
    end if
  end Subroutine get10dcFld

  Subroutine GetrError(lCheck)
! compute error of the real field with respect to the field saved in the reference file
    Implicit none
    Logical, intent(in) :: lCheck
    lfcFld=lfcFldAll
    call OutTxt('t1','Compute error'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
! evaluate data of the current field
    call getrField(.true.)
! get and evaluate difference field
    lDiffField=.true.
    lDiffRel=.false.
    lAskFld=.false.
    lSkipFld=.false.
    lSkipDerFld=.true.
    lSkipVecFld=.true.
    lSkipScaFld=.true.
    lSkipFldHead=.true.
    call OpenField(lCheck)
    call getLimitsrField()
    ErrFld=rSumFld
    Err2Fld=rSum2Fld
    call OutTxt('t1','Error computed'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
  end Subroutine GetrError

  Subroutine getFieldLine(Start,nPoints,d,Line,nP)
    Implicit none
    Integer(4) nPoints,i,nP
    Real(8) Start(3),d,Line(3,0:nPoints),r0(3),r1(3),r2(3),d1(3),d2(3),d3(3),f(3)
    nP=0
    if(nPoints.lt.0) return
    Line(1:3,0)=Start(1:3)
    if(nPoints.lt.1) return
    do i=1,nPoints
      r0(1:3)=Line(1:3,i-1)
      call GetLocrField(r0,-9_2,0_4,f)
      if(iSingleFldPoint.lt.1_2) Exit
      call Unit3DV(f)
      d1=f*0.5d0*d
      r1=r0+d1
      call GetLocrField(r1,-9_2,0_4,f)
      if(iSingleFldPoint.lt.1_2) Exit
      call Unit3DV(f)
      d2=f*0.5d0*d
      r2=r0+0.5d0*(d1+d2)
      call GetLocrField(r2,-9_2,0_4,f)
      if(iSingleFldPoint.lt.1_2) Exit
      call Unit3DV(f)
      d3=f*0.5d0*d
      nP=i
      Line(1:3,i)=r0+d2+d3 !!! more general: r0+a*d1+b*d2+c*d3
    end do
  end Subroutine getFieldLine

  Subroutine getFieldTube(Start,r,d,mPoints,nPoints,Tube,nP)
    Implicit none
    Integer(4) mPoints,nPoints,i,nP,nPl
    Real(8) Start(3),r,d,Tube(3,0:nPoints,mPoints),r0(3),phi,spac(3,0:3)
    nP=0
    if((nPoints.lt.1).or.(mPoints.lt.1)) return
    spac(1:3,0)=Start(1:3)
    call GetLocrField(Start,-9_2,0_4,spac(1:3,1))
    if(iSingleFldPoint.lt.1_2) return
    call Unit3DV(spac(1:3,1))
    spac(1:3,2:3)=0.0d0
    spac(2,2)=1.0d0
    spac(3,3)=1.0d0
    call Ortho3DSpace(spac)
    spac(1:3,2:3)=r*spac(1:3,2:3)
    nP=nPoints
    do i=1,mPoints
      phi=Dble(i-1)*2.0d0*Pi/Dble(mPoints)
      r0(1:3)=spac(1:3,0)+dcos(phi)*spac(1:3,2)+dsin(phi)*spac(1:3,3)
      call getFieldLine(r0,nPoints,d,Tube(1:3,0:nPoints,i),nPl)
      nP=min(nPl,nP)
    end do
  end Subroutine getFieldTube

! Grid handling

  Subroutine ClearGrid(lCheck)
! clear the grid
    Implicit none
    Include 'resource.fd'
    Integer(4) i,j,k,kk,km,lout,lf,iErr
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    if(lrGrd) return
      call OutTxt('t1','Clear grid'C)
      call OutTxt('n1',' 'C)
      call OutTxt('m1',' 'C)
! set constant parameters
    pForm(0)=1.0d0
    pForm(1)=dble(nxcFld)
    pForm(2)=dble(nycFld)
    pForm(3)=dble(nzcFld)
    pForm(4)=Dble(fcFld)
    pForm(5)=DImag(fcFld)
    vForm(0)=1.0d0
    kk=0
    km=Int4(nxcFld)*Int4(nycFld)*Int4(nzcFld)
    cForm(6:8)=c678(1:3,3)
    cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
    do k=1,nzcFld
      vForm(3)=dble(k)
      do j=1,nycFld
        vForm(2)=dble(j)
        do i=1,nxcFld
          if(lStopThread) return
          kk=kk+1
          call OutTxt('t1','Grid point'C)
          call IntToStr(Int4(kk),0,0,SpaceText,lout)
          call OutTxt('n1',SpaceText(1:lout))
          call IntToStr(km,0,0,SpaceText,lout)
          call OutTxt('m1',SpaceText(1:lout))
          vForm(1)=dble(i)
          if(Grd_Form(1)(1:8).eq.'function') then
            call getFun(1,i,rGrd(1,i,j,k))
          else
            lf=-1
            dFormu=Formula(Grd_Form(1),lf,cForm,pForm,vForm,8,5,3,1,1,1,iErr)
            rGrd(1,i,j,k)=dFormu(1)
            if((iErr.ne.0).or.(Grd_Form(1)(1:1).eq.'!')) rGrd(1,i,j,k)=vForm(1)*pForm(1)
          end if
          if(Grd_Form(2)(1:8).eq.'function') then
            call getFun(1,j,rGrd(2,i,j,k))
          else
            lf=-1
            dFormu=Formula(Grd_Form(2),lf,cForm,pForm,vForm,8,5,3,1,1,1,iErr)
            rGrd(2,i,j,k)=dFormu(1)
            if((iErr.ne.0).or.(Grd_Form(2)(1:1).eq.'!')) rGrd(2,i,j,k)=vForm(2)*pForm(2)
          end if
          if(Grd_Form(3)(1:8).eq.'function') then
            call getFun(1,k,rGrd(3,i,j,k))
          else
            lf=-1
            dFormu=Formula(Grd_Form(3),lf,cForm,pForm,vForm,8,5,3,1,1,1,iErr)
            rGrd(3,i,j,k)=dFormu(1)
            if((iErr.ne.0).or.(Grd_Form(3)(1:1).eq.'!')) rGrd(3,i,j,k)=vForm(3)*pForm(3)
          end if
        end do
      end do
    end do
    call OutTxt('t1','Grid cleared'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
  end Subroutine ClearGrid

  Subroutine TransformGrid(lCheck)
! transform the grid
    Implicit none
    Include 'resource.fd'
    Real(8) r(3),ralt
    Integer(4) i,j,k,kk,km,idum,lout,lf,iErr
    Logical, intent(in) :: lCheck
    if(lrGrd) then
      idum=MessageBoxQQ('Cannot transform regular grid!'C,'Transform grid'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    call OutTxt('t1','Transform grid'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    cForm(6:8)=c678(1:3,3)
    cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
    if(lCheck) then ! conformal mapping of the xy plane
      kk=0
      km=Int4(nxcFld)*Int4(nycFld)*Int4(nzcFld)
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            if(lStopThread) return
            kk=kk+1
            call OutTxt('t1','Grid point'C)
            call IntToStr(Int4(kk),0,0,SpaceText,lout)
            call OutTxt('n1',SpaceText(1:lout))
            call IntToStr(km,0,0,SpaceText,lout)
            call OutTxt('m1',SpaceText(1:lout))
            if(Con_Form(1:1).ne.'!') then
              r=getcGrd(i,j,k)
              vcForm(0)=DCmplx(r(1),r(2))
              lf=-1
              cFormu=CFormula(Con_Form,lf,cCForm,pCForm,vCForm,8,3,0,1,1,1,iErr)
              if(iErr.ne.0) Cycle
              rGrd(1,i,j,k)=Dble(cFormu(1))
              rGrd(2,i,j,k)=DImag(cFormu(1))
            end if
          end do
        end do
      end do
    else ! 3D transformation
      vForm(0)=1.0d0
      kk=0
      km=Int4(nxcFld)*Int4(nycFld)*Int4(nzcFld)
      do i=1,nxcFld
        do j=1,nycFld
          do k=1,nzcFld
            if(lStopThread) return
            kk=kk+1
            call OutTxt('t1','Grid point'C)
            call IntToStr(Int4(kk),0,0,SpaceText,lout)
            call OutTxt('n1',SpaceText(1:lout))
            call IntToStr(km,0,0,SpaceText,lout)
            call OutTxt('m1',SpaceText(1:lout))
            r=getcGrd(i,j,k)
            vForm(1)=r(1)
            vForm(2)=r(2)
            vForm(3)=r(3)
            if(Trns_Form(1)(1:1).ne.'!') then
              lf=-1
              ralt=rGrd(1,i,j,k)
              dFormu=Formula(Trns_Form(1),lf,cForm,pForm,vForm,8,3,3,1,1,1,iErr)
              rGrd(1,i,j,k)=dFormu(1)
              if(iErr.ne.0) rGrd(1,i,j,k)=ralt
            end if
            if(Trns_Form(2)(1:1).ne.'!') then
              lf=-1
              ralt=rGrd(2,i,j,k)
              dFormu=Formula(Trns_Form(2),lf,cForm,pForm,vForm,8,3,3,1,1,1,iErr)
              rGrd(2,i,j,k)=dFormu(1)
              if(iErr.ne.0) rGrd(2,i,j,k)=ralt
            end if
            if(Trns_Form(2)(1:1).ne.'!') then
              lf=-1
              ralt=rGrd(3,i,j,k)
              dFormu=Formula(Trns_Form(3),lf,cForm,pForm,vForm,8,3,3,1,1,1,iErr)
              rGrd(3,i,j,k)=dFormu(1)
              if(iErr.ne.0) rGrd(3,i,j,k)=ralt
            end if
          end do
        end do
      end do
    end if
    call OutTxt('t1','Grid transformed'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
  end Subroutine TransformGrid

END MODULE CHFLD


