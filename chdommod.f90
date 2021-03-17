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
MODULE CHDOM

! Domains

  USE CHOBJ

  SAVE

  CONTAINS

! Graphics

  Subroutine TDrawDomain(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    ldum=lCheck
    call WaitEndThread()
    iThreadAction=2
    call StartDOMThread(ldum)
  end Subroutine TDrawDomain

  Subroutine StartDOMThread(ldi)
! start a new thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start domain thread'C)
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
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start domain thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start domain thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(DOMThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start domain thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
      return
    end if
  end Subroutine StartDOMThread

  Integer(4) Function DOMThread(iWhat)
! tread calls.....
    Implicit none
    Integer(4) iWhat
    Logical ldum
    Include 'resource.fd'
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    DOMThread=0_4
    if(iThreadAction.eq.2) then
      call MTDrawBoundary(ldum)
      call MTDrawExpansion(.true.)
    end if
    call endThread()
  end Function DOMThread

! I/O

  Subroutine SaveDomain(lCheck)
! save domain data in a file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) iOk,ios,i,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select domain data file to be written!','Domain data file ',DomFileName,'DOM',ios)
      if(ios.gt.0) return
    end if
    open(1,file=DomFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save domains'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHDomIdent,iOK)
    ich(1)=nDom
    sch(1:8)=' domains'
    call chwrit2(1,ich,1,rch,0,sch,8,iOK)
    do i=1,nDom
      ich(1)=Int4(iDom(i))
      call chwrit2(1,ich,1,rch,0,sch,0,iOK)
      call WriteStr(1,Dom_Form(1,i),iOK)
      call WriteStr(1,Dom_Form(2,i),iOK)
      call WriteStr(1,Dom_Form(3,i),iOK)
      call WriteStr(1,Dom_Form(4,i),iOK)
    end do
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveDomain

  Subroutine OpenDomain(lCheck)
! read domain data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist
	  Integer(4) iOK,ios,i,idum,iVers
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select domain data file to be read!','Domain data file ',DomFileName,'DOM',ios)
      if(ios.gt.0) return
    end if
    inquire(file=DomFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=DomFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open domains'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHDomIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open domains'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call StrToInt(text(16:16)//text(18:18),iVers,iOK)
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of domains)'C,'Open domains'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    nDom=ich(1)
    if(nDom.gt.mDom) then
      mDom=nDom
      call AllocateDom(ldum)
      if(.not.ldum) return
    end if
    do i=1,nDom
      call chread2(1,ich,1,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(domain color)'C,'Open domains'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      iDom(i)=Int2(ich(1))
      call ReadStr(1,Dom_Form(1,i),iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(domain formula 1)'C,'Open domains'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      call ReadStr(1,Dom_Form(2,i),iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(domain formula 2)'C,'Open domains'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      call ReadStr(1,Dom_Form(3,i),iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(domain formula 3)'C,'Open domains'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      if(iVers.gt.10) then
      call ReadStr(1,Dom_Form(4,i),iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(domain formula 3)'C,'Open domains'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
      else
        Dom_Form(4,i)='0.0'C
      end if
    end do
    close(1)
    call getEUST()
  end Subroutine OpenDomain

  Subroutine SaveProject(lCheck,lOpened)
! save Project data in a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lOpened
	  Integer(4) iOk,ios,idum
    lOpened=.false.
    if(.not.lCheck) then
      call Open2write(-1,'Select Project data file to be written!','Project data file ',ProFileName,'PRO',ios)
      if(ios.gt.0) return
    end if
    open(1,file=ProFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save project'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHProIdent,iOK)
    ich(1)=0
    sch(1:10)=' time dep.'
    if(lfcFld) then
      ich(1)=1
      sch(1:10)=' freq.dep.'
    end if
    call chwrit2(1,ich,1,rch,0,sch,10,iOK)
    rch(1)=Dble(fcFld)
    rch(2)=DImag(fcFld)
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    ich(1)=0
    sch(1:14)=' 3D, wave type'
    if(lgcFld) then
      ich(1)=1
      sch(1:14)=' 2D'
    end if
    ich(2)=Int4(iHEGlobal)
    call chwrit2(1,ich,2,rch,0,sch,14,iOK)
    if(lgcFld) then
      if(lzPer) then
        rch(1)=Dble(dcFld)
        rch(2)=2.0d0*pBig
      else
        rch(1)=Dble(gcFld)
        rch(2)=DImag(gcFld)
      end if
      call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    end if
    ich(1:3)=0
    sch(1:13)=' non-periodic'
    if(lxPeriod) then
      ich(1)=1
      if(.not.lregularPeriod) ich(1)=-1
    end if
    if(lyPeriod) ich(2)=1
    if(lzPeriod) ich(3)=1
    if(lxPeriod.or.lyPeriod.or.lzPeriod)  sch(1:13)=' periodic    '
    call chwrit2(1,ich,3,rch,0,sch,13,iOK)
    if(lxPeriod.or.lyPeriod.or.lzPeriod) then
      rch(1)=xPeriod
      rch(2:3)=yPeriodVector(1:2)
      rch(4:6)=zPeriodVector(1:3)
      call chwrit2(1,ich,0,rch,6,sch,0,iOK)
      rch(1)=Dble(cxPeriodDlg)
      rch(2)=DImag(cxPeriodDlg)
      rch(3)=Dble(cyPeriodDlg)
      rch(4)=DImag(cyPeriodDlg)
      rch(5)=Dble(czPeriodDlg)
      rch(6)=DImag(czPeriodDlg)
      call chwrit2(1,ich,0,rch,6,sch,0,iOK)
    end if
    ich(1)=0
    sch(1:11)=' scattering'
    if(lEigen) then
      ich(1)=1
      sch(1:11)=' eigenvalue'
    end if
    call chwrit2(1,ich,1,rch,0,sch,11,iOK)
    if(lEigen) then
      ich(1)=iEigen
      ich(2)=0
      if(lEigenSave) ich(2)=1
      if(.not.lWriteRoughFld) ich(2)=ich(2)+2
      call chwrit2(1,ich,2,rch,0,sch,0,iOK)
      ich(1)=nrEigen
      ich(2)=niEigen
      rch(1)=Dble(c1Eigen)
      rch(2)=DImag(c1Eigen)
      rch(3)=Dble(c2Eigen)
      rch(4)=DImag(c2Eigen)
      call chwrit2(1,ich,2,rch,4,sch,0,iOK)
      ich(1)=imEigen
      ich(2)=itmEigen
      rch(1)=aEigen
      rch(2)=fEigen
      call chwrit2(1,ich,2,rch,2,sch,0,iOK)
      rch(1)=Dble(c1EigenC)
      rch(2)=DImag(c1EigenC)
      rch(3)=Dble(c2EigenC)
      rch(4)=DImag(c2EigenC)
      call chwrit2(1,ich,0,rch,4,sch,0,iOK)
    end if
    ich(1)=ixySymm
    ich(2)=ixzSymm
    ich(3)=iyzSymm
    call chwrit2(1,ich,3,rch,0,sch,0,iOK)
    ich(1)=Int4(iWriteDigits)
    call chwrit2(1,ich,1,rch,0,sch,0,iOK)
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
    lOpened=.true.
  end Subroutine SaveProject

  Subroutine OpenProject(lCheck,lOpened)
! read Project data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical lFileExist,lOpened
    Integer(4) iOk,ios,iVers,idum
    Character(20) text
    lOpened=.false.
    if(.not.lCheck) then
      call Open2read(-1,'Select Project data file to be read!','Project data file ',ProFileName,'PRO',ios)
      if(ios.gt.0) return
    end if
    inquire(file=ProFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=ProFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open project'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHProIdent(1:15).ne.text(1:15)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open project'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call StrToInt(text(16:16)//text(18:18),iVers,iOK)
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(frequency/time dependence)'C,'Open project'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    lfcFld=.false.
    if(ich(1).ne.0) lfcFld=.true.
    if((iVers.gt.30).or.lfcFld) then
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(frequency)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      fcFld=DCmplx(rch(1),rch(2))
    end if
    call chread2(1,ich,2,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(2D/3D)'C,'Open project'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    iHEGlobal=Int2(ich(2))
    lgcFld=.false.
    if(ich(1).ne.0) then
      lgcFld=.true.
      call chread2(1,ich,0,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(gamma)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      if(rch(2).gt.pBig) then
        lzPer=.true.
        dcFld=DCmplx(rch(1),0.0d0)
        gcFld=0.5d0/(dcFld*fcFld*kw0)
      else
        lzPer=.false.
        gcFld=DCmplx(rch(1),rch(2))
        dcFld=0.5d0/(gcFld*fcFld*kw0)
      end if
    end if
    lxPeriod=.false.
    lyPeriod=.false.
    lzPeriod=.false.
    lregularPeriod=.true.
    if(iVers.gt.19) then
      call chread2(1,ich,3,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Periodic Y/N)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      if(ich(1).ne.0) lxPeriod=.true.
      if(ich(1).lt.0) lregularPeriod=.false.
      if(ich(2).ne.0) lyPeriod=.true.
      if(ich(3).ne.0) lzPeriod=.true.
      if(lxPeriod.or.lyPeriod.or.lzPeriod) then
        call chread2(1,ich,0,rch,6,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Period)'C,'Open project'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        xPeriod=rch(1)
        yPeriodVector(1:2)=rch(2:3)
        yPeriodVector(3)=0.0d0
        zPeriodVector(1:3)=rch(4:6)
        call chread2(1,ich,0,rch,6,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Period)'C,'Open project'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        cxPeriod=DCmplx(rch(1),rch(2))
        cyPeriod=DCmplx(rch(3),rch(4))
        czPeriod=DCmplx(rch(5),rch(6))
        CxPeriodDlg=CxPeriod
        CyPeriodDlg=CyPeriod
        CzPeriodDlg=CzPeriod
      end if
    else
      call chread2(1,ich,2,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Periodic Y/N)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      if(ich(1).ne.0) lxPeriod=.true.
      if(ich(1).lt.0) lregularPeriod=.false.
      if(ich(2).ne.0) lyPeriod=.true.
      if(lxPeriod.or.lyPeriod.or.lzPeriod) then
        call chread2(1,ich,0,rch,6,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Period)'C,'Open project'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        xPeriod=rch(1)
        yPeriodVector(1)=0.0d0
        yPeriodVector(2)=rch(2)
        yPeriodVector(3)=0.0d0
        zPeriodVector(1)=0.0d0
        zPeriodVector(2)=0.0d0
        zPeriodVector(3)=1.0d0
        cxPeriod=DCmplx(rch(3),rch(4))
        cyPeriod=DCmplx(rch(5),rch(6))
        czPeriod=(0.0d0,0.0d0)
        CxPeriodDlg=CxPeriod
        CyPeriodDlg=CyPeriod
        CzPeriodDlg=CzPeriod
      end if
    end if
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Eigenvalue Y/N)'C,'Open project'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    lEigen=.false.
    if(ich(1).ne.0) then
      lEigen=.true.
      call chread2(1,ich,2,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Eigenvalue type)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      iEigen=ich(1)
      lEigenSave=.false.
      lWriteRoughFld=.true.
      if((ich(2).eq.1).or.(ich(2).gt.2)) lEigenSave=.true.
      if(ich(2).gt.1) lWriteRoughFld=.false.
      call chread2(1,ich,2,rch,4,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Rough search data)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      nrEigen=ich(1)
      niEigen=ich(2)
      c1Eigen=DCmplx(rch(1),rch(2))
      c2Eigen=DCmplx(rch(3),rch(4))
      c1EigenC=DCmplx(min(rch(1),rch(3)),min(rch(2),rch(4)))
      c2EigenC=DCmplx(max(rch(1),rch(3)),max(rch(2),rch(4)))
      call chread2(1,ich,2,rch,2,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Fine search data)'C,'Open project'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      imEigen=min(100,ich(1))
      itmEigen=ich(2)
      aEigen=rch(1)
      fEigen=rch(2)
      if(iVers.gt.20) then
        call chread2(1,ich,0,rch,4,iOK)
        if(iOK.ne.0) then
          idum=MessageBoxQQ('Error reading input file!\r(Fine search clip)'C,'Open project'C, &
                            MB$OK.or.MB$ICONSTOP)
  	      close(1)
		      return
	      end if
        c1EigenC=DCmplx(min(rch(1),rch(3)),min(rch(2),rch(4)))
        c2EigenC=DCmplx(max(rch(1),rch(3)),max(rch(2),rch(4)))
      end if
    end if
    call chread2(1,ich,3,rch,0,iOK)
    if(iOK.ne.0) ich(1:3)=0
    ixySymm=ich(1)
    ixzSymm=ich(2)
    iyzSymm=ich(3)
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) ich(1)=7
    iWriteDigits=Int2(ich(1))
    close(1)
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
    lOpened=.true.
  end Subroutine OpenProject

  Complex(8) function cDrudeLorentz(x,c,e,omd,gamd,de,oml,gaml)
    Implicit none
    Integer(4) l,iErr2
    Real(8) x,e,omd,gamd,de,oml,gaml
    Character(1) c
    Character(256) sch
    cDrudeLorentz=(0.0d0,0.0d0)
    cCForm(0)=(0.0d0,1.0d0)
    pCForm(0)=(1.0d0,0.0d0)
    vCForm(0)=DCmplx(1.0d0,0.0d0)
    vCForm(1)=DCmplx(x,0.0d0)
    vCForm(2)=DCmplx(e,0.0d0)
    vCForm(3)=DCmplx(omd,0.0d0)
    vCForm(4)=DCmplx(gamd,0.0d0)
    vCForm(5)=DCmplx(de,0.0d0)
    vCForm(6)=DCmplx(oml,0.0d0)
    vCForm(7)=DCmplx(gaml,0.0d0)
    sch='dlf(v1,v2,v3,v4,v5,v6,v7)'C
    sch(3:3)=c(1:1)
    l=25
    cFormu=cFormula(sch,l,cCForm,pCForm,vCForm,0,0,7,1,1,1,iErr2)
    if(iErr2.eq.0) cDrudeLorentz=cFormu(1)
  end function cDrudeLorentz

END MODULE CHDOM




