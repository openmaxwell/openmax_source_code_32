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
MODULE CHINT

! integrals

  USE CHFLD

  SAVE

  CONTAINS

  Subroutine Integral_Defaults(lCheck)
! set default field
    Logical, intent(in) :: lCheck
    Logical ldum,lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    lStopInt=.false.
    lAskQ=.true.
    lIntgSave=.false.
    lBndIntgX=.false.
    iIntgEc=0_2
    iIntgHc=0_2
    ldum=lCheck
    IntField=0
    IntInter=0
    IntType=2
    IntWhat=2
    MaxIntIter=1000
    IntOrd=1
    IntNx=10
    IntNy=10
    iBndInt=1
    iObjInt=1
    IntErr=0
    IntCall=0
    AccIntegral=1.0d-2
    currentIntegral=0.0d0
    currentIntegralMax=nBig
    currentIntegralMin=pBig
    currentIntegralMaxL(1:3)=0.0d0
    currentIntegralMinL(1:3)=0.0d0
    currentIntegralMax1=nBig
    currentIntegralMin1=pBig
    currentIntegralMax1S=0.0d0
    currentIntegralMin1S=0.0d0
    XminInt=-1.0d0
    XmaxInt=1.0d0
    YminInt=-1.0d0
    YmaxInt=1.0d0
    rSphInt=-1.0d0
    spaceInt(1:3,0:3)=0.0d0
    spaceInt(1,1)=1.0d0
    spaceInt(2,2)=1.0d0
    spaceInt(3,3)=1.0d0
    PowerInt=1.0d0
    FactorInt=1.0d0
    IntSide=0
    lAbsInt=.false.
    lebInt=.false.
  end Subroutine Integral_Defaults

! threads

  Subroutine TBndInt(lCheck)
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=1_4
		call StartINTThread(lCheck)
  end Subroutine TBndInt

  Subroutine TRecInt(lCheck)
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=2_4
		call StartINTThread(lCheck)
  end Subroutine TRecInt

  Subroutine TObjInt(lCheck)
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=3_4
		call StartINTThread(lCheck)
  end Subroutine TObjInt

  Subroutine StartINTThread(ldi)
! start the INT thread
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start integral thread'C)
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
		call OutTxt('t3','Start integral thread'C)
    iThreadHandle=CreateThread(NULL,iStack,Loc(INTThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start integral thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
    end if
  end Subroutine StartINTThread

  Integer(4) Function INTThread(iWhat)
! INT tread: calls.....
    Implicit Integer(4) (i-n)
    Implicit Real(8) (a-h,o-z)
    Include 'resource.fd'
    Logical ldum
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    INTThread=0_4
    if(iThreadAction.eq.1) then
      call BndIntg(ldum)
    else if(iThreadAction.eq.2) then
      call RecIntg(ldum)
    else if(iThreadAction.eq.3) then
      call ObjIntg(ldum)
    else
      INTThread=1_4
    end if
    call endThread()
  end Function INTThread

! I/O

  Subroutine SaveIntegral(lCheck)
! save Integral data in a file
    Logical, intent(in) :: lCheck
	  Integer(4) iOk,ios
    if(.not.lCheck) then
      call Open2write(-1,'Select Integral data file to be written!','Integral data file ',IntFileName,'INT',ios)
      if(ios.gt.0) return
    end if
    open(1,file=IntFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save integral'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHIntIdent,iOK)
    ich(1)=IntType
    ich(2)=iBndInt
    ich(3)=IntWhat
    ich(4)=IntField
    ich(5)=IntInter
    ich(6)=IntOrd
    ich(7)=MaxIntIter
    rch(1)=AccIntegral
    sch(1:11)=' IntType...'
    call chwrit2(1,ich,7,rch,1,sch,11,iOK)
    ich(1)=IntNx
    ich(2)=IntNy
    rch(1)=XminInt
    rch(2)=YminInt
    rch(3)=XmaxInt
    rch(4)=YmaxInt
    sch(1:9)=' IntNx...'
    call chwrit2(1,ich,2,rch,4,sch,9,iOK)
    ich(1)=IntErr
    ich(2)=IntCall
    rch(1)=currentIntegral
    rch(2)=currentIntegralMin
    rch(3)=currentIntegralMax
    sch(1:10)=' IntErr...'
    call chwrit2(1,ich,2,rch,3,sch,10,iOK)
    ich(1)=iObjInt
    sch(1:7)=' ObjInt'
    call chwrit2(1,ich,1,rch,0,sch,7,iOK)
    ich(1)=0
    if(lAbsInt) ich(1)=1
    if(leBInt) ich(1)=ich(1)+2
    if(rSphInt.gt.pBig) then
      rch(1)=rSphInt
      sch(1:10)=' spherical'
      call chwrit2(1,ich,1,rch,1,sch,10,iOK)
    else
      rch(1)=rSphInt
      sch(1:12)=' rectangular'
      call chwrit2(1,ich,1,rch,1,sch,12,iOK)
    end if
    rch(1:3)=spaceInt(1:3,0)
    sch(1:7)=' Origin'
    call chwrit2(1,ich,0,rch,3,sch,7,iOK)
    rch(1:3)=spaceInt(1:3,1)
    sch(1:2)=' X'
    call chwrit2(1,ich,0,rch,3,sch,2,iOK)
    rch(1:3)=spaceInt(1:3,2)
    sch(1:2)=' Y'
    call chwrit2(1,ich,0,rch,3,sch,2,iOK)
    rch(1:3)=spaceInt(1:3,3)
    sch(1:2)=' Z'
    call chwrit2(1,ich,0,rch,3,sch,2,iOK)
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveIntegral

  Subroutine OpenIntegral(lCheck)
! read Integral data from a file
    Logical, intent(in) :: lCheck
    Logical lFileExist,lOld
	  Integer(4) iOk,ios
    Character(20) text
    if(.not.lCheck) then
      call Open2read(-1,'Select Integral data file to be read!','Integral data file ',IntFileName,'INT',ios)
      if(ios.gt.0) return
    end if
    inquire(file=IntFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=IntFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open integral'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHIntIdent(1:18).ne.text(1:18)) then
      lOld=.true.
      if(text(16:16).ne.'1') then
        idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open integral'C, &
                          MB$YesNo.or.MB$IconQuestion)
        if(idum.eq.MB$IDNO) then
          close(1)
          return
        end if
      end if
    end if
    call chread2(1,ich,7,rch,1,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(line 2)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    IntType=ich(1)
    iBndInt=ich(2)
    IntWhat=ich(3)
    IntField=ich(4)
    IntInter=ich(5)
    IntOrd=ich(6)
    MaxIntIter=ich(7)
    AccIntegral=rch(1)
    call chread2(1,ich,2,rch,4,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(line 3)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    IntNx=ich(1)
    IntNy=ich(2)
    XminInt=rch(1)
    YminInt=rch(2)
    XmaxInt=rch(3)
    YmaxInt=rch(4)
    if(lOld) then
      call chread2(1,ich,2,rch,1,iOK)
      rch(2)=pBig
      rch(3)=nBig
    else
      call chread2(1,ich,2,rch,3,iOK)
    end if
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(line 4)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    IntErr=ich(1)
    IntCall=ich(2)
    currentIntegral=rch(1)
    currentIntegralMin=rch(2)
    currentIntegralMax=rch(3)
    currentIntegralMaxL(1:3)=0.0d0
    currentIntegralMaxL(1:3)=0.0d0
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(line 5)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    iObjInt=ich(1)
    call chread2(1,ich,1,rch,1,iOK)
    lAbsInt=.false.
    lebInt=.false.
    if(iOK.ne.0) then ! new data (integration over sphere...) missing -> set defaults
      rSphInt=-1.0d0
      spaceInt(1:3,0:3)=0.0d0
      spaceInt(1,1)=1.0d0
      spaceInt(2,2)=1.0d0
      spaceInt(3,3)=1.0d0
  	  close(1)
		  return
	  end if
    rSphInt=rch(1)
    if((ich(1).gt.0).and.(ich(1).ne.2)) lAbsInt=.true.
    if(ich(1).gt.1) lebInt=.true.
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(origin)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    spaceInt(1:3,0)=rch(1:3)
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(X)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    spaceInt(1:3,1)=rch(1:3)
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Y)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    spaceInt(1:3,2)=rch(1:3)
    call chread2(1,ich,0,rch,3,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Z)'C,'Open integral'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    spaceInt(1:3,3)=rch(1:3)
    close(1)
  end Subroutine OpenIntegral

! Integral computations

  Real(8) Function BndIntgFun(v,cf1)
    Real(8) s,v(1),cf1(1),xtPt,ytPt,r(3),f(3),fL(3),fR(3),z
    Integer(4) idum
    Integer(2) iDomL,iDomR,iDm
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    if(lStopThread) then
      lStopInt=.true.
      return
    end if
    IntCall=IntCall+1
		call OutTxt('t1','BndIntgFun'C)
    call IntToStr(Int4(IntCall),0,0,SpaceText,lout)
		call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(Int4(MaxIntIter),0,0,SpaceText,lout)
		call OutTxt('m1',SpaceText(1:lout))
    cf1(1)=1.0d0
    if(lebInt) then ! evaluate integral for eBeam: real(Ex*exp(-i*omega*x/v)), makes sense only for eBeam excitation, straight path along eBeam!
      z=v(1)-0.5d0*tBnd(kBnd)%Length ! integrate from -length/2 to + length/2
      r(1:3)=tExp(nExp)%plane(1:3,0)+z*tExp(nExp)%plane(1:3,3) ! line direction along Z direction of last expansion, not along boundary!
      call GetLocrField(r,-9_2,0_4,f,z)
	    BndIntgFun=(tExp(nExp)%plane(1,3)*f(1)+tExp(nExp)%plane(2,3)*f(2)+tExp(nExp)%plane(3,3)*f(3)) ! vector component along eBeam excitation
    else
      z=v(1)
      s=z+tBnd(kBnd)%Start
      call GetBndPt(kBnd,s,r(1),r(2),xtPt,ytPt,iDomL,iDomR,idum)
      vBndNorm(1)=ytPt 
      vBndNorm(2)=-xtPt
      vBndNorm(3)=0.0d0
      call Unit3DV(vBndNorm) ! required for force integrals
      if((iDomL.gt.30000_2).or.(iDomL.lt.-30000_2)) then
	      BndIntgFun=0.0d0
        return
      end if
      r(3)=0.0d0
      if((iDomL.eq.iDomR).or.(iDomL.lt.1).or.(iDomR.lt.1)) then
        iDm=Max(iDomL,iDomR)
        call GetLocrField(r,iDm,IntInter,f)
      else if(itrFld.eq.itF) then
        call GetLocrField(r,iDomR,IntInter,f)
      else
        if(IntSide.eq.1) then
          call GetLocrField(r,iDomL,IntInter,fL)
          f=fL
        else if(IntSide.eq.-1) then
          call GetLocrField(r,iDomR,IntInter,fR)
          f=fR
        else
          call GetLocrField(r,iDomL,IntInter,fL)
          call GetLocrField(r,iDomR,IntInter,fR)
          f=0.5d0*(fL+fR)
        end if
      end if
      f=FactorInt*f
      if(lAbsInt) then
        if(intWhat.eq.0) then
          BndIntgFun=1.0d0                 ! scalar constant -> integral=length of boundary
        else
          BndIntgFun=r3Vec_Length(f)       ! length of vector f
        end if
      else
        if(intWhat.eq.0) then
	        BndIntgFun=1.0d0                 ! scalar constant -> integral=length of boundary
        else if(intWhat.eq.-1) then
	        BndIntgFun=f(1)                  ! scalar fx
        else if(intWhat.eq.-2) then
	        BndIntgFun=f(2)                  ! scalar fy
        else if(intWhat.eq.-3) then
	        BndIntgFun=f(3)                  ! scalar fz
        else if(intWhat.eq.1) then
	        BndIntgFun=(xtPt*f(1)+ytPt*f(2)) ! 2D vector integral along boundary
        else
	        BndIntgFun=(ytPt*f(1)-xtPt*f(2)) ! 2D flow perpendicular to boundary
        end if
      end if
      if(abs(PowerInt).gt.pSmall) then ! post processing: power or logarithm
        if(abs(PowerInt-1.0d0).gt.1.0d-14) BndIntgFun=BndIntgFun.pow.PowerInt
      else
        BndIntgFun=.log.BndIntgFun
      end if
    end if
    if(lBndIntgX) BndIntgFun=BndIntgFun*2.0d0*Pi*r(1)
    if(BndIntgFun.lt.currentIntegralMin) then
      currentIntegralMin=BndIntgFun
      currentIntegralMinL(1:3)=r(1:3)
    end if
    if(BndIntgFun.gt.currentIntegralMax) then
      currentIntegralMax=BndIntgFun
      currentIntegralMaxL(1:3)=r(1:3)
    end if
    if(lIntgSave) then
      iSaveFunction=5
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      rch(1)=z
      rch(2)=BndIntgFun
      rch(3:5)=f(1:3)
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
    end if
	end Function BndIntgFun

  Subroutine BndIntg(lCheck)
    Real(8) v(1),cf1(1),aa(1),bb(1),resu(35),sum,ds,vi,vi1,vi2
    Integer(4) kPl,kP,k1,k2,nTestFun,iv,ipt,m,i,nFun0,nFunA0,iFunA10,iFunA20
    Logical, intent(in) :: lCheck
    Logical lStopInt,lAskQ,lFoundMin,lFoundMax
    Common/StopInt/lStopInt,lAskQ
    IntErr=0
    if((.not.lCheck).or.lAskQ) then
      idum=MessageBoxQQ('Save the values of the integrand on a function file?'C,'Boundary integral'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lIntgSave=.true.
      if(idum.eq.MB$IDNO) lIntgSave=.false.
    end if
    currentIntegralMin=pBig
    currentIntegralMax=nBig
    currentIntegralMinL(1:3)=0.0d0
    currentIntegralMaxL(1:3)=0.0d0
    currentIntegralMin1=pBig
    currentIntegralMax1=nBig
    currentIntegralMin1S=0.0d0
    currentIntegralMax1S=0.0d0
    if(lIntgSave) then
      iSaveFunction=-1
      nFun0=nFun
      nFunA0=nFunA
      iFunA10=iFunA1
      iFunA20=iFunA2
      nFun=0
      nFunA=5
      iFunA1=1
      iFunA2=5
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      call SaveFunction(lCheck,.true.)
      iSaveFunction=-2
      sch='n'
      call SaveFunction(.true.,.true.)
      sch='x'
      call SaveFunction(.true.,.true.)
      sch='f(s)'
      call SaveFunction(.true.,.true.)
      sch='fx(s)'
      call SaveFunction(.true.,.true.)
      sch='fy(s)'
      call SaveFunction(.true.,.true.)
      sch='fz(s)'
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
    end if
    lStopInt=.false.
    sum=0.0d0
    IntCall=0
    if(IntField.ne.0) then
      lxrFld=.true.
      lyrFld=.true.
      lzrFld=.true.
      if(iabs(IntField).eq.1) itrFld=itE
      if(iabs(IntField).eq.2) itrFld=itH
      if(iabs(IntField).eq.3) itrFld=itS
      if(iabs(IntField).eq.4) itrFld=itF
      if(IntField.ne.0) then
        larFld=.false.
        lprFld=.false.
      end if
      if(IntField.lt.0) larFld=.true.
      if(IntInter.ne.0) call getrField(.false.)
    end if
    kPl=iBndInt
    if(kPl.lt.1) then
      if(kPl.eq.0) then
        k1=1
        k2=nBnd
      else
        k1=1
        k2=Min(nBnd,Max(1,-kPl))
      end if
    else
      if(kPl.gt.nBnd) kPl=nBnd
      k1=kPl
      k2=k1
    end if
    iBound=abs(iBound)
    lFoundMin=.false.
    lFoundMax=.false.
    do kP=k1,k2
      if(LSkipBnd(kP)) Cycle
      kBnd=kP
      aa(1)=0.0d0
      bb(1)=tBnd(kP)%sLength
      if(IntType.eq.0) then
        resu(1)=0.0d0
        ds=bb(1)/dble(Max(1_4,MaxIntIter))
        v(1)=-ds*0.5d0
        do i=1,MaxIntIter
          v(1)=v(1)+ds
          if(i.gt.2) vi2=vi1
          if(i.gt.1) vi1=vi
          vi=BndIntgFun(v,cf1)
          if(i.gt.2) then
            if(.not.lFoundMin) then
              if((vi1.le.vi).and.(vi1.le.vi2)) then
                currentIntegralMin1=vi1
                currentIntegralMin1S=v(1)-ds
                lFoundMin=.true.
              end if
            end if
            if(.not.lFoundMax) then
              if((vi1.ge.vi).and.(vi1.ge.vi2)) then
                currentIntegralMax1=vi1
                currentIntegralMax1S=v(1)-ds
                lFoundMax=.true.
              end if
            end if
          end if
          resu(1)=resu(1)+ds*vi
          if(lStopThread) Exit
        end do
        m=MaxIntIter
      else if(IntType.eq.1) then
        nTestFun=0
        iv=1
        ipt=IntOrd
        call intgl(BndIntgFun,nTestFun,v,iv,aa,bb,ipt,MaxIntIter,resu,m,IntErr)
      else if(IntType.eq.2) then
        nTestFun=0
        iv=1
        ipt=IntOrd
        call intgk(BndIntgFun,nTestFun,v,iv,aa,bb,ipt,MaxIntIter,AccIntegral,resu,m,IntErr)
      else if(IntType.eq.3) then
        nTestFun=0
        iv=1
        call inthp(BndIntgFun,nTestFun,v,iv,aa,bb,4,MaxIntIter,AccIntegral,resu,m,IntErr)
      else
        nTestFun=0
        iv=1
        ipt=IntOrd
        call intch(BndIntgFun,nTestFun,v,iv,aa(1),bb(1),MaxIntIter/(ipt+1),ipt,1,resu,m,IntErr)
      end if
      sum=sum+resu(1)
    end do
    if(lIntgSave) then
      iSaveFunction=-3
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
      nFun=nFun0
      nFunA=nFunA0
      iFunA1=iFunA10
      iFunA2=iFunA20
    end if
    currentIntegral=sum
    call RealToStr(currentIntegral,0,6,SpaceText,lout)
		call OutTxt('t2','Intg='//SpaceText(1:lout)//Char(0))
    call IntToStr(Int4(IntErr),0,0,SpaceText,lout)
		call OutTxt('n2','Err='//SpaceText(1:lout)//Char(0))
		call OutTxt('m2',' 'C)
  end Subroutine BndIntg

  Real(8) Function RecIntgFun(x,y)
    Implicit none
    Real(8) x,y,r(3),f(3)
    Integer(4) lout
    Integer(2) iDm
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    if(lStopThread) then
      lStopInt=.true.
      return
    end if
    IntCall=IntCall+1
		call OutTxt('t1','RecIntgFun'C)
    call IntToStr(Int4(IntCall),0,0,SpaceText,lout)
		call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(Int4(IntNx*IntNy),0,0,SpaceText,lout)
		call OutTxt('m1',SpaceText(1:lout))
    if(rSphInt.gt.nSmall) then
      r(1:3)=spaceInt(1:3,0)+x*spaceInt(1:3,1)+y*spaceInt(1:3,2)
    else
      r(1)=x
      r(2)=y
      r(3)=0.0d0
    end if
    iDm=-9_2
    call GetLocrField(r,iDm,IntInter,f)
    f=FactorInt*f
    if(lAbsInt) then
      if(intWhat.eq.0) then
        RecIntgFun=1.0d0                         ! scalar constant -> integral=length of boundary
      else
        RecIntgFun=r3Vec_Length(f)               ! length of vector f
      end if
    else
      if(intWhat.eq.0) then
	      RecIntgFun=1.0d0                         ! scalar constant -> integral=size of rectangle
      else if(intWhat.eq.-1) then
	      RecIntgFun=f(1)                          ! scalar fx
      else if(intWhat.eq.-2) then
	      RecIntgFun=f(2)                          ! scalar fy
      else if(intWhat.eq.-3) then
	      RecIntgFun=f(3)                          ! scalar fz
      else if(intWhat.eq.1) then
	      RecIntgFun=r3Vec_Length(f)               ! length of f
      else
	      RecIntgFun=r3Scl_Prod(f,spaceInt(1:3,3)) ! flow through rectangle
      end if
    end if
    if(abs(PowerInt).gt.pSmall) then ! post processing: power or logarithm
      if(abs(PowerInt-1.0d0).gt.1.0d-14) RecIntgFun=RecIntgFun.pow.PowerInt
    else
      RecIntgFun=.log.RecIntgFun
    end if
    if(lIntgSave) then
      iSaveFunction=7
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      rch(1:3)=r(1:3)
      rch(4)=RecIntgFun
      rch(5:7)=f(1:3)
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
    end if
	end Function RecIntgFun

  Real(8) Function SphIntgFun(phi,theta)
    Implicit none
    Real(8) phi,theta,x,y,z,r(3),f(3),rn(3)
    Integer(4) lout
    Integer(2) iDm
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    if(lStopThread) then
      lStopInt=.true.
      return
    end if
    IntCall=IntCall+1
		call OutTxt('t1','SphIntgFun'C)
    call IntToStr(Int4(IntCall),0,0,SpaceText,lout)
		call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(Int4(IntNx*IntNy),0,0,SpaceText,lout)
		call OutTxt('m1',SpaceText(1:lout))
    x=rSphInt*dsin(theta*Pi/180.0d0)*dcos(phi*Pi/180.0d0)
    y=rSphInt*dsin(theta*Pi/180.0d0)*dsin(phi*Pi/180.0d0)
    z=rSphInt*dcos(theta*Pi/180.0d0)
    rn(1:3)=x*spaceInt(1:3,1)+y*spaceInt(1:3,2)+z*spaceInt(1:3,3)
    vBndNorm=Unit3DVec(rn) ! required for force integrals
    r(1:3)=spaceInt(1:3,0)+rn(1:3)
    iDm=-9_2
    call GetLocrField(r,iDm,IntInter,f)
    f=FactorInt*f
    if(lAbsInt) then
      if(intWhat.eq.0) then
        SphIntgFun=1.0d0                         ! scalar constant -> integral=length of boundary
      else
        SphIntgFun=r3Vec_Length(f)               ! length of vector f
      end if
    else
      if(intWhat.eq.0) then
	      SphIntgFun=1.0d0                         ! scalar constant -> integral=size of rectangle
      else if(intWhat.eq.-1) then
	      SphIntgFun=f(1)                          ! scalar fx
      else if(intWhat.eq.-2) then
	      SphIntgFun=f(2)                          ! scalar fy
      else if(intWhat.eq.-3) then
	      SphIntgFun=f(3)                          ! scalar fz
      else if(intWhat.eq.1) then
	      SphIntgFun=r3Vec_Length(f)               ! length of f
      else
	      SphIntgFun=r3Scl_Prod(f,spaceInt(1:3,3)) ! flow through sphere
      end if
    end if
    if(abs(PowerInt).gt.pSmall) then ! post processing: power or logarithm
      if(abs(PowerInt-1.0d0).gt.1.0d-14) SphIntgFun=SphIntgFun.pow.PowerInt
    else
      SphIntgFun=.log.SphIntgFun
    end if
    if(lIntgSave) then
      iSaveFunction=7
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      rch(1:3)=r(1:3)
      rch(4)=SphIntgFun
      rch(5:7)=f(1:3)
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
    end if
	end Function SphIntgFun

  Subroutine RecIntg(lCheck)
    Implicit none
    Include 'resource.fd'
    Real(8) sum,dx,dy,df,x,y,s
    Integer(4) i,j,lout,nFun0,nFunA0,iFunA10,iFunA20,idum
    Logical, intent(in) :: lCheck
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    IntErr=0
    if((.not.lCheck).or.lAskQ) then
      idum=MessageBoxQQ('Save the values of the integrand on a function file?'C,'Surface integral'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lIntgSave=.true.
      if(idum.eq.MB$IDNO) lIntgSave=.false.
    end if
    lStopInt=.false.
    sum=0.0d0
    IntCall=0
    currentIntegralMin=pBig
    currentIntegralMax=nBig
    currentIntegralMinL(1:3)=0.0d0
    currentIntegralMaxL(1:3)=0.0d0
    if(lIntgSave) then
      iSaveFunction=-1
      nFun0=nFun
      nFunA0=nFunA
      iFunA10=iFunA1
      iFunA20=iFunA2
      nFun=0
      nFunA=7
      iFunA1=1
      iFunA2=7
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      call SaveFunction(lCheck,.true.)
      iSaveFunction=-2
      sch='n'
      call SaveFunction(.true.,.true.)
      sch='x'
      call SaveFunction(.true.,.true.)
      sch='y'
      call SaveFunction(.true.,.true.)
      sch='z'
      call SaveFunction(.true.,.true.)
      sch='f(x,y,z)'
      call SaveFunction(.true.,.true.)
      sch='fx(x,y,z)'
      call SaveFunction(.true.,.true.)
      sch='fy(x,y,z)'
      call SaveFunction(.true.,.true.)
      sch='fz(x,y,z)'
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
    end if
    if(IntField.ne.0) then
      lxrFld=.true.
      lyrFld=.true.
      lzrFld=.true.
      if(iabs(IntField).eq.1) itrFld=itE
      if(iabs(IntField).eq.2) itrFld=itH
      if(iabs(IntField).eq.3) itrFld=itS
      if(iabs(IntField).eq.4) itrFld=itF
      if(IntField.ne.0) then
        larFld=.false.
        lprFld=.false.
      end if
      if(IntField.lt.0) larFld=.true.
      if(IntInter.ne.0) call getrField(.false.)
    end if
    dx=(XmaxInt-XminInt)/Dble(Max(1,IntNx))
    dy=(YmaxInt-YminInt)/Dble(Max(1,IntNy))
    df=dx*dy
    if(rSphInt.gt.nSmall) then ! new integration
      spaceInt(1:3,3)=r3Vec_Prod(spaceInt(1:3,1),spaceInt(1:3,2))
      df=df*r3Vec_Length(spaceInt(1:3,3))
    else
      spaceInt(1:3,0:3)=0.0d0
      spaceInt(1,1)=1.0d0
      spaceInt(2,2)=1.0d0
      spaceInt(3,3)=1.0d0
    end if
    if(rSphInt.gt.pSmall) then ! integrate over spherical shape
      x=XminInt-0.5d0*dx
      do i=1,IntNx
        x=x+dx
        y=YminInt-0.5d0*dy
        do j=1,IntNy
          y=y+dy
          df=dsin(y*Pi/180.0d0)*dx*dy*((rSphInt*Pi/180.0d0)**2)
          s=sphIntgFun(x,y)
          sum=sum+df*s
          if(s.lt.currentIntegralMin) then
            currentIntegralMin=s
            currentIntegralMinL(1)=x
            currentIntegralMinL(2)=y
            currentIntegralMinL(3)=0.0d0
          end if
          if(s.gt.currentIntegralMax) then
            currentIntegralMax=s
            currentIntegralMaxL(1)=x
            currentIntegralMaxL(2)=y
            currentIntegralMaxL(3)=0.0d0
          end if
          if(lStopThread) return
        end do
      end do
    else ! integrate over rectangular area
      vBndNorm=Unit3DVec(spaceInt(1:3,3)) ! required for force integrals
      x=XminInt-0.5d0*dx
      do i=1,IntNx
        x=x+dx
        y=YminInt-0.5d0*dy
        do j=1,IntNy
          y=y+dy
          s=recIntgFun(x,y)
          sum=sum+df*s
          if(s.lt.currentIntegralMin) then
            currentIntegralMin=s
            currentIntegralMinL(1)=x
            currentIntegralMinL(2)=y
            currentIntegralMinL(3)=0.0d0
          end if
          if(s.gt.currentIntegralMax) then
            currentIntegralMax=s
            currentIntegralMaxL(1)=x
            currentIntegralMaxL(2)=y
            currentIntegralMaxL(3)=0.0d0
          end if
          if(lStopThread) return
        end do
      end do
    end if
    if(lIntgSave) then
      iSaveFunction=-3
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
      nFun=nFun0
      nFunA=nFunA0
      iFunA1=iFunA10
      iFunA2=iFunA20
    end if
    currentIntegral=sum
    call RealToStr(currentIntegral,0,7,SpaceText,lout)
		call OutTxt('t2','Intg='//SpaceText(1:lout)//Char(0))
    call IntToStr(Int4(IntErr),0,0,SpaceText,lout)
		call OutTxt('n2','Err='//SpaceText(1:lout)//Char(0))
		call OutTxt('m2',' 'C)
  end Subroutine RecIntg

  Subroutine ObjIntg(lCheck)
    Implicit none
    Include 'resource.fd'
    Complex(8) cf(10)
    Real(8) sum,xPt,yPt,xtPt,ytPt,r(3),en(3),en0(3),f(3),fl(3),fr(3),s,sf
    Integer(4) kPt,kPBnd,lout,k1,k2,nFun0,nFunA0,iFunA10,iFunA20,idum
    Integer(2) iDomL,iDomR,iDm
    Logical, intent(in) :: lCheck
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    IntErr=0
    if((.not.lCheck).or.lAskQ) then
      idum=MessageBoxQQ('Save the values of the integrand on a function file?'C,'Object integral'C, &
                        MB$YesNo.or.MB$IconQuestion)
      lIntgSave=.true.
      if(idum.eq.MB$IDNO) lIntgSave=.false.
    end if
    lStopInt=.false.
    sum=0.0d0
    IntCall=0
    currentIntegralMin=pBig
    currentIntegralMax=nBig
    currentIntegralMinL(1:3)=0.0d0
    currentIntegralMaxL(1:3)=0.0d0
    if(lIntgSave) then
      iSaveFunction=-1
      nFun0=nFun
      nFunA0=nFunA
      iFunA10=iFunA1
      iFunA20=iFunA2
      nFun=0
      nFunA=7
      if(iIntgEc.ne.0) nFunA=nFunA+6
      if(iIntgHc.ne.0) nFunA=nFunA+6
      iFunA1=1
      iFunA2=nFunA
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      call SaveFunction(lCheck,.true.)
      iSaveFunction=-2
      sch='n'
      call SaveFunction(.true.,.true.)
      sch='x'
      call SaveFunction(.true.,.true.)
      sch='y'
      call SaveFunction(.true.,.true.)
      sch='z'
      call SaveFunction(.true.,.true.)
      sch='f(x,y,z)'
      call SaveFunction(.true.,.true.)
      sch='fx(x,y,z)'
      call SaveFunction(.true.,.true.)
      sch='fy(x,y,z)'
      call SaveFunction(.true.,.true.)
      sch='fz(x,y,z)'
      call SaveFunction(.true.,.true.)
      if(iIntgEc.ne.0) then
        sch='Exr(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Exi(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Eyr(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Eyi(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Ezr(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Ezi(x,y,z)'
        call SaveFunction(.true.,.true.)
      end if
      if(iIntgHc.ne.0) then
        sch='Hxr(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Hxi(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Hyr(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Hyi(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Hzr(x,y,z)'
        call SaveFunction(.true.,.true.)
        sch='Hzi(x,y,z)'
        call SaveFunction(.true.,.true.)
      end if
      iSaveFunction=0
    end if
    if(nObj.lt.1) return
    if(iObjInt.lt.0) then
      k1=1
      k2=Min(nObj,-iObjInt)
    else if(iObjInt.eq.0) then
      k1=1
      k2=nObj
    else
      k1=Min(nObj,iObjInt)
      k2=k1
    end if
    if(IntField.ne.0) then
      lxrFld=.true.
      lyrFld=.true.
      lzrFld=.true.
      if(iabs(IntField).eq.1) itrFld=itE
      if(iabs(IntField).eq.2) itrFld=itH
      if(iabs(IntField).eq.3) itrFld=itS
      if(iabs(IntField).eq.4) itrFld=itF
      if(IntField.ne.0) then
        larFld=.false.
        lprFld=.false.
      end if
      if(IntField.lt.0) larFld=.true.
      if(IntInter.ne.0) call getrField(.false.)
    end if
    lMMPBndVal=.false.
    call getEUST()
    iBound=abs(iBound)
    call cBndGetABO() ! get c-poly, splines, match.pts
    if((iBound.ne.2_2).and.(iBound.ne.4_2)) then
      lMMPBndVal=.false.
      return
    end if
    if(lgcFld) then
      nBndPt3D=nBndPt
      nInhBndPt3D=0
    else
      call get3DMatPts(iObjInt,iObjInt,2_4,3_2,.true.) ! generate 3D matching points (also when weight = 0)
    end if
		call OutTxt('t1','ObjIntgFun'C)
    do kPt=1,nBndPt3D
      if(.not.lgcFld) then
        if(iObjBndPt3D(kPt).lt.0) Cycle
      end if
      if(lStopThread) then
        lStopInt=.true.
        return
      end if
      call IntToStr(kPt,0,0,SpaceText,lout)
	    call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nBndPt3D,0,0,SpaceText,lout)
	    call OutTxt('m1',SpaceText(1:lout))
      if(lgcFld) then
        call GetBndPt(0,sBndPt(kPt),xPt,yPt,xtPt,ytPt,iDomL,iDomR,kPBnd)
        if((iDomL.gt.30000_2).or.(iDomL.lt.-30000_2)) Cycle
        r(1)=xPt
        r(2)=yPt
        r(3)=0.0d0
        en(1)=-ytPt
        en(2)=xtPt
        en(3)=0.0d0
      else
        iDomL=tBnd(iBndPt3D(kPt))%iLDom
        iDomR=tBnd(iBndPt3D(kPt))%iRDom
        if((iDomL.gt.30000_2).or.(iDomL.lt.-30000_2)) Cycle
        r(1:3)=BndPt3D(1:3,0,kPt)
        en(1:3)=BndPt3D(1:3,3,kPt)
      end if
      en0=Unit3DVec(en)
      vBndNorm=en0 ! required for force integrals; sign +-?
      if((iDomL.eq.iDomR).or.(iDomL.lt.1).or.(iDomR.lt.1)) then
        iDm=Max(iDomL,iDomR)
        call GetLocrField(r,iDm,IntInter,f)
      else if(itrFld.eq.itF) then
        call GetLocrField(r,iDomR,IntInter,f)
      else
        if(IntSide.eq.1) then
          call GetLocrField(r,iDomL,IntInter,fL)
          f=fL
        else if(IntSide.eq.-1) then
          call GetLocrField(r,iDomR,IntInter,fR)
          f=fR
        else
          call GetLocrField(r,iDomL,IntInter,fL)
          call GetLocrField(r,iDomR,IntInter,fR)
          f=0.5d0*(fL+fR)
        end if
      end if
      f=FactorInt*f
      if(lAbsInt) then
        if(intWhat.eq.0) then
          sf=1.0d0             ! scalar constant -> integral=length of boundary
        else
          sf=r3Vec_Length(f)   ! length of vector f
        end if
      else
        if(intWhat.eq.0) then
	        sf=1.0d0             ! scalar constant -> integral=size of rectangle
        else if(intWhat.eq.-1) then
	        sf=f(1)              ! scalar fx
        else if(intWhat.eq.-2) then
	        sf=f(2)              ! scalar fy
        else if(intWhat.eq.-3) then
	        sf=f(3)              ! scalar fz
        else if(intWhat.eq.1) then
	        sf=r3Vec_Length(f)   ! length of f
        else
	        sf=r3Scl_Prod(f,en0) ! flow perpendicular to object
        end if
      end if
      if(abs(PowerInt).gt.pSmall) then ! post processing: power or logarithm
        if(abs(PowerInt-1.0d0).gt.1.0d-14) sf=sf.pow.PowerInt
      else
        sf=.log.sf
      end if
      s=sf*r3Vec_Length(en)
      if(s.lt.currentIntegralMin) then
        currentIntegralMin=sf
        currentIntegralMinL(1:3)=r(1:3)
      end if
      if(s.gt.currentIntegralMax) then
        currentIntegralMax=sf
        currentIntegralMaxL(1:3)=r(1:3)
      end if
      sum=sum+s
      IntCall=IntCall+1
      if(lIntgSave) then
        iSaveFunction=7
        lAskFun=.false.
        lSkipFun=.false.
        lSkipFunHead=.false.
        rch(1:3)=r(1:3)
        rch(4)=sf
        rch(5:7)=f(1:3)
        call SaveFunction(.true.,.true.)
        if(iIntgEc.ne.0) then
          call GetLoccField(r,iIntgEc,0,cf)
          iSaveFunction=6
          rch(1)=Dble(cf(1))
          rch(2)=DImag(cf(1))
          rch(3)=Dble(cf(2))
          rch(4)=DImag(cf(2))
          rch(5)=Dble(cf(3))
          rch(6)=DImag(cf(3))
          call SaveFunction(.true.,.true.)
        end if
        if(iIntgHc.ne.0) then
          call GetLoccField(r,iIntgHc,0,cf)
          iSaveFunction=6
          rch(1)=Dble(cf(4))
          rch(2)=DImag(cf(4))
          rch(3)=Dble(cf(5))
          rch(4)=DImag(cf(5))
          rch(5)=Dble(cf(6))
          rch(6)=DImag(cf(6))
          call SaveFunction(.true.,.true.)
        end if
        iSaveFunction=0
      end if
    end do
    if(lIntgSave) then
      iSaveFunction=-3
      lAskFun=.false.
      lSkipFun=.false.
      lSkipFunHead=.false.
      call SaveFunction(.true.,.true.)
      iSaveFunction=0
      nFun=nFun0
      nFunA=nFunA0
      iFunA1=iFunA10
      iFunA2=iFunA20
    end if
    currentIntegral=sum
    call RealToStr(currentIntegral,0,7,SpaceText,lout)
		call OutTxt('t2','Intg='//SpaceText(1:lout)//Char(0))
    call IntToStr(Int4(IntErr),0,0,SpaceText,lout)
		call OutTxt('n2','Err='//SpaceText(1:lout)//Char(0))
		call OutTxt('m2',' 'C)
  end Subroutine ObjIntg

END MODULE CHINT


