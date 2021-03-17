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
MODULE CHOBJ

! Objects

  USE CHEXP

  SAVE

  CONTAINS

! Threads

  Subroutine MTModifyObject(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    if(.not.lCheck) then
      idum=MessageBoxQQ('Set view plane = xy plane?'C,'Modify objects'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) return
      if(idum.eq.MB$IDYES) then
        viewPlane(1:3,1:3)=0.0d0 ! view plane = xy, (origin unchanged)
        viewPlane(3,0)=0.0d0
        viewPlane(1,1)=1.0d0
        viewPlane(2,2)=1.0d0
        viewPlane(3,3)=1.0d0
      end if
    end if
    iDraBnd=0
    iDraExp=0
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    iWinAction=6_2
    call MTDrawExpansion(.true.)
    call MTDrawBoundary(.true.)
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuChecked)   !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuUnChecked) !nothing
		call OutTxt('t1','Modify objects!'C)
  end Subroutine MTModifyObject

! I/O

  Subroutine SaveObject(lCheck)
! save Object data in a file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) iOk,ios,i,idum
    if(.not.lCheck) then
      call Open2write(-1,'Select Object data file to be written!','Object data file ',OBJFileName,'3DO',ios)
      if(ios.gt.0) return
    end if
    open(1,file=OBJFileName,iostat=ios)
    if(ios.ne.0) then
      idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save Object'C, &
                        MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call WriteStr(1,CHOBJIdent,iOK)
    ich(1)=nOBJ
    sch(1:8)=' Objects'
    call chwrit2(1,ich,1,rch,0,sch,8,iOK)
    ich(1)=iObjDra
    ich(2)=0
    if(lObjMat) ich(2)=1
    if(lObjMatW0) ich(2)=ich(2)+2
    ich(3)=0
    if(lObjFlg) ich(3)=1
    ich(4)=iDraObj
    ich(5)=0
    if(lObjHid) ich(5)=1
    call chwrit2(1,ich,5,rch,0,sch,0,iOK)
    rch(1)=GRChmin
    rch(2)=GRChmax
    call chwrit2(1,ich,0,rch,2,sch,0,iOK)
    do i=1,nOBJ
      ich(1)=Int4(tOBJ(i)%iTypO)
      ich(2:6)=Int4(tOBJ(i)%iPar(1:5))
      call chwrit2(1,ich,6,rch,0,sch,0,iOK)
      rch(1)=tOBJ(i)%GrfRes
      call chwrit2(1,ich,0,rch,1,sch,0,iOK)
      ich(1)=Int4(tOBJ(i)%iCol)
      ich(2)=Int4(tOBJ(i)%iColMin)
      ich(3)=Int4(tOBJ(i)%iColMax)
      call chwrit2(1,ich,3,rch,0,sch,0,iOK)
      rch(1:5)=tOBJ(i)%Par(1:5)
      call chwrit2(1,ich,0,rch,5,sch,0,iOK)
      rch(1:3)=tOBJ(i)%Plane(1:3,0)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tOBJ(i)%Plane(1:3,1)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tOBJ(i)%Plane(1:3,2)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tOBJ(i)%Plane(1:3,3)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tOBJ(i)%O(1:3)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
      rch(1:3)=tOBJ(i)%e(1:3)
      call chwrit2(1,ich,0,rch,3,sch,0,iOK)
    end do
    sch(1:1)=' '
    call chwrit2(1,ich,0,rch,0,sch,1,iOK)
    EndFile(1)
    close(1)
  end Subroutine SaveObject

  Subroutine OpenObject(lCheck)
! read Object data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist
	  Integer(4) iOk,ios,i,i1,idum
    Character(20) text
    if(.not.lgcFld) lGet3DMat=.true.
    if(lAskOBJ.or.(.not.lCheck)) then
      nInsObj=nOBJ
      call InsertDialog(.true.)
      if(kInsObj.lt.0) return
      lInsertOBJ=lInsObj
    end if
    if(.not.lCheck) then
      call Open2read(-1,'Select Object data file to be read!','Object data file ',OBJFileName,'3DO',ios)
      if(ios.gt.0) return
    end if
    inquire(file=OBJFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=OBJFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open Object'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
    call ReadStr(1,text,iOK)
    if(CHOBJIdent(1:17).ne.text(1:17)) then
      idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open Object'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        close(1)
        return
      end if
    end if
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of Objects)'C,'Open Object'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    nInsObj=ich(1)
    if(lInsertOBJ) then
      i=kInsObj
    else
      i=0
      nOBJ=0
    end if
    call chread2(1,ich,5,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Header data)'C,'Open Object'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    iObjDra=ich(1)
    lCHGLdoubleSide=.true.
    if((iObjDra.eq.3).or.(iObjDra.eq.4)) lCHGLdoubleSide=.false.
    lObjMat=.false.
    lObjMatW0=.false.
    if((ich(2).eq.1).or.(ich(2).eq.1)) lObjMat=.true.
    if(ich(2).gt.1) lObjMatW0=.true.
    lObjFlg=.false.
    if(ich(3).eq.1) lObjFlg=.true.
    iDraObj=ich(4)
    lObjHid=.false.
    if(ich(5).eq.1) lObjHid=.true.
    lGRCused=.not.lObjHid
    call GRCclear()
    call chread2(1,ich,0,rch,2,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Header data)'C,'Open Object'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
	  end if
    GRChmin=rch(1)
    GRChmax=rch(2)
    do i1=1,nInsObj
      call InsertOBJ(i,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert Object!'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      i=i+1
      call chread2(1,ich,6,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%iTypO=Int2(ich(1))
      tOBJ(i)%iPar(1:5)=Int2(ich(2:6))
      call chread2(1,ich,0,rch,1,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%GrfRes=rch(1)
      call chread2(1,ich,3,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%iCol=Int2(ich(1))
      tOBJ(i)%iColMin=Int2(ich(2))
      tOBJ(i)%iColMax=Int2(ich(3))
      call chread2(1,ich,0,rch,5,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%Par(1:5)=rch(1:5)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%Plane(1:3,0)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%Plane(1:3,1)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%Plane(1:3,2)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%Plane(1:3,3)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%O(1:3)=rch(1:3)
      call chread2(1,ich,0,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(Object data)'C,'Open Object'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%e(1:3)=rch(1:3)
    end do
    close(1)
    kOBJ=nOBJ
    kPar=nPar
  end Subroutine OpenObject

  Subroutine Open3DM(lCheck,iCM)
! read 3D matching point data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist
    Integer(2) iD1,iD2
	  Integer(4) iOk,ios,i,i1,idum,k,n,iCM
    Real(8) d
    if(.not.lgcFld) lGet3DMat=.true.
    if(lAskOBJ.or.(.not.lCheck)) then
      nInsObj=nOBJ
      call InsertDialog(.true.)
      if(kInsObj.lt.0) return
      lInsertOBJ=lInsObj
    end if
    if(.not.lCheck) then
      call Open2read(-1,'Select 3D matching point data file to be read!','3DMP data file ',M3DFileName,'3DM',ios)
      if(ios.gt.0) return
    end if
    inquire(file=M3DFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=M3DFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open 3DMP'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
! read number of identifiers and create a fictitious 2D boundary for each identifier
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of identifiers)'C,'Open 3DMP'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
    end if
    n=ich(1)
    i=0
    do i1=1,n
      call chread2(1,ich,2,rch,0,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(identifiers)'C,'Open 3DMP'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
      end if
      call InsertBnd(i,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert boundary!'C,'Open 3DMP'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      i=i+1
      tBnd(i)%nEdge=0
      tBnd(i)%iRDom=Int2(ich(1))
      tBnd(i)%iLDom=Int2(ich(2))
      tBnd(i)%iCond=0_2 ! default continuity condition
      call GetBndBC(i)
      tBnd(i)%iConn=0_2
      tBnd(i)%iTypB=2_2
      tBnd(i)%iCol=Int2(iCM)
      tBnd(i)%nMatPts=1
      tBnd(i)%Weight=1.0d0
      tBnd(i)%val(1)=0.0d0
      tBnd(i)%val(2)=0.0d0
      tBnd(i)%Weight2=tBnd(i)%Weight
      tBnd(i)%nSpline=0
      tBnd(i)%fAmpl=0.0d0
      tBnd(i)%Formula='0'C
      call InsertBndEdg(i,0,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert corner!'C,'Open 3DMP'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
      end if
      k=tBnd(i)%iEdgeOffset+1
      tBndEdg(k)%x=1.0d10 ! dummy large circle
      tBndEdg(k)%y=1.0d10
      tBndEdg(k)%r=1.0d9
      tBndEdg(k)%xa=tBndEdg(k)%x+tBndEdg(k)%r
      tBndEdg(k)%ya=tBndEdg(k)%y
      tBndEdg(k)%xb=tBndEdg(k)%x+tBndEdg(k)%r
      tBndEdg(k)%yb=tBndEdg(k)%y
      tBndEdg(k)%xo=tBndEdg(k)%x
      tBndEdg(k)%yo=tBndEdg(k)%y
    end do
! now read 3D matching points and convert to rectangles
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(Number of 3D matching points)'C,'Open 3DMP'C, &
                        MB$OK.or.MB$ICONSTOP)
      close(1)
      return
    end if
    n=ich(1)
    if(lInsertOBJ) then
      i=kInsObj
    else
      i=0
      nOBJ=0
    end if
    do i1=1,n
      call InsertOBJ(i,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert Object!'C,'Open 3DMP'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      i=i+1
      tOBJ(i)%iTypO=4_2 ! rectangle
      call chread2(1,ich,1,rch,6,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(3D matching point data)'C,'Open 3DMP'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
	    end if
      tOBJ(i)%iPar(1)=Int2(ich(1))
      iD1=tBnd(ich(1))%iRDom
      iD2=tBnd(ich(1))%iLDom
      tOBJ(i)%iPar(2:5)=0_2
      tOBJ(i)%iCol=Int2(iCM)
      tOBJ(i)%iColMin=30_2
      tOBJ(i)%iColMax=90_2
      tOBJ(i)%Plane(1:3,0)=rch(1:3)
      tOBJ(i)%Plane(1:3,1)=rch(4:6) ! this is wrong direction!
      tOBJ(i)%Plane(1:3,2)=rch(4:6) ! this is wrong direction!
      tOBJ(i)%Plane(1:3,3)=rch(4:6) ! this is correct direction!
      call Ortho3DSpace3(tOBJ(i)%Plane(1:3,3),tOBJ(i)%Plane(1:3,1),tOBJ(i)%Plane(1:3,2)) ! correct wrong directions (perpendicular)
      d=dsqrt(r3Vec_Length(rch(4:6)))
      tOBJ(i)%GrfRes=d/10.0d0
      tOBJ(i)%Plane(1:3,0)=tOBJ(i)%Plane(1:3,0)-0.5d0*d*tOBJ(i)%Plane(1:3,1)-0.5d0*d*tOBJ(i)%Plane(1:3,2) ! origin of rectangle -> 3DMP in center
      tOBJ(i)%Par(1:5)=d ! dx,dy,mPsideLength,...
      tOBJ(i)%Par(4)=1.0d0 ! multipole density
      tOBJ(i)%O(1:3)=0.0d0
      tOBJ(i)%e(1)=1.0d0
      tOBJ(i)%e(2:3)=0.0d0
    end do
    close(1)
    kOBJ=nOBJ
    kPar=nPar
  end Subroutine Open3DM
  
  Subroutine Open3DD(lCheck,nOb,iTyD,iCD)
! read 3D dipole data from a file
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum,lFileExist
    Integer(4) iOk,ios,i,i1,idum,n,nOb,iTyD,iCD
    if(lAskExp.or.(.not.lCheck)) then
      nInsObj=nExp
      call InsertDialog(.true.)
      if(kInsObj.lt.0) return
      lInsertExp=lInsObj
    end if
    if(.not.lCheck) then
      call Open2read(-1,'Select 3D dipole data file to be read!','Expansion data file ',D3DFileName,'EXP',ios)
      if(ios.gt.0) return
    end if
    inquire(file=D3DFileName,Exist=lFileExist)
    if(.not.lFileExist) return
    open(1,file=D3DFileName,status='old',iostat=ios)
    if(ios.ne.0) then
      if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open 3D dipoles'C, &
                      MB$OK.or.MB$IconExclamation)
      close(1)
      return
    end if
! read number of 3D dipoles
    call chread2(1,ich,1,rch,0,iOK)
    if(iOK.ne.0) then
      idum=MessageBoxQQ('Error reading input file!\r(number of 3D dipoles)'C,'Open 3DD'C, &
                        MB$OK.or.MB$ICONSTOP)
  	  close(1)
		  return
    end if
    n=ich(1)
    if(lInsertExp) then
      i=kInsObj
    else
      i=0
      nExp=0
      nPar=0
    end if
    do i1=1,n
      call chread2(1,ich,1,rch,3,iOK)
      if(iOK.ne.0) then
        idum=MessageBoxQQ('Error reading input file!\r(3D dipole data)'C,'Open 3DD'C, &
                          MB$OK.or.MB$ICONSTOP)
  	    close(1)
		    return
      end if
      call InsertExp(i,1,ldum)
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert expansion!'C,'Open 3DD'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
      i=i+1
      tExp(i)%iDom=Int2(ich(1))
      tExp(i)%iTypE=6_2 ! dipole is a 3D multipole
      tExp(i)%iHE=Min(2_2,Max(0_2,Int2(iTyD))) ! only electric dipoles in Uli Koch's *.3dd file -> iTyD=0
      tExp(i)%iConn=0_2
      tExp(i)%iCol=Int2(iCD)
      tExp(i)%iObj=Int2(nOb)
      tExp(i)%iE(1:5)=1
      tExp(i)%iE(3)=0
      tExp(i)%iE(6)=0
      tExp(i)%rE(1:5)=0.0d0
      tExp(i)%depend=1.0d100
      tExp(i)%gc=(0.0d0,0.0d0)
      tExp(i)%xo=rch(1)
      tExp(i)%yo=rch(2)
      tExp(i)%Plane(1:3,0)=rch(1:3)
      tExp(i)%Plane(1:3,1:3)=0.0d0
      tExp(i)%Plane(1,1)=1.0d0
      tExp(i)%Plane(2,2)=1.0d0
      tExp(i)%Plane(3,3)=1.0d0
      tExp(i)%O(1:3)=0.0d0
      tExp(i)%e(1:3)=0.0d0
      tExp(i)%nPar=0
      if(tExp(i)%iHE.ne.2_2) then
        call InsertPar(i,0,3,ldum) ! electric and magnetic dipoles have 3 parameters
      else
        call InsertPar(i,0,6,ldum) ! electric + magnetic dipoles have 6 parameters
      end if
      if(.not.ldum) then
        idum=MessageBoxQQ('Cannot insert parameter!'C,'Open expansion'C, &
                          MB$OK.or.MB$ICONSTOP)
        close(1)
        return
      end if
    end do
    close(1)
    call CorrExpPar(1000_4)
    kExp=nExp
    kPar=nPar
  end Subroutine Open3DD

! Object operations

  Subroutine gen3DMatPts(k1,k2,lcount,iAlsoW,nPts,Points,iC,iEQ,iOC)
! generate 3D matching points for the 3D objects k1...k2 -> Points
! if iC is present and iC(1)=0, save the color number for the points in iC
! if iC is present and iC(1)=1, save the number of the original 2D matching point in iC
! if iC is present and iC(1)=2, save the domain numbers of the original 2D matching point in iC
! if iC is present and iC(1)=3, save the boundary number of the original 2D matching point in iC
! if iEQ is present: save number of equations in iEQ
! if iOC is present: save number of 3D object in iOC
! nPts stores the numbers of matching points for each object
! iAlsoW=0: ignore boundaries with weight 0;
!        1: use boundaries with weight 0 if domain numbers are different
!        2: use all boundaries
    Implicit none
    Integer(4) k1,k2,kO,k,i1,i2,i,nPts(*),nP,mP,idum,nEq2D,na,nha,nBndPtI1,nBndPtI2
    Integer(2) iDl,iDR,iCl(1)
    Integer(2), Optional:: iC(*),iEQ(*),iOC(*)
    Real(8) Points(3,0:3,*),P(3),vt(3),s,ds,r0,PA(2),PB(2),PC(2),a,b,c,F,ha,h,dh,xa,x,dx
    Logical lcount,l2DBnd,l2DDom,lsav,l2DNr,lAlsoW
    Integer(4), intent(in) :: iAlsoW
    if(((iBound.ne.2_2).and.(iBound.ne.4_2))) call cBndGetABO() ! get c-poly, splines, match.pts
    nBndEq3D=0
    nBndPt3D=0
    lsav=.false.
    l2DBnd=.false.
    l2DDom=.false.
    l2DNr=.false.
    if(Present(iC)) then
      lsav=.true.
      l2DBnd=.true.
      l2DDom=.true.
      if(iC(1).eq.0_2) l2DBnd=.false.
      if(iC(1).eq.1_2) l2DDom=.false.
      if(iC(1).eq.3_2) l2DNr=.true.
    end if
    i1=0
    do kO=k1,k2
      mP=0
      if((tOBJ(kO)%iTypO.eq.3).or.(tOBJ(kO)%iTypO.eq.4)) then ! Triangle or Rectangle
        if(tBnd(max(1,tOBJ(kO)%iPar(1)))%nMat.lt.1) Cycle ! no matching points for this object
        nEq2D=0
        iDR=tBnd(tOBJ(kO)%iPar(1))%iLDom
        iDL=tBnd(tOBJ(kO)%iPar(1))%iRDom
        lAlsoW=.true.
        if((iAlsoW.lt.1).or.((iAlsoW.eq.1).and.(iDR.eq.IDL))) lAlsoW=.false.
        if((iDR.lt.-30000_2).or.((tBnd(tOBJ(kO)%iPar(1))%weight.lt.pSmall)).and.(.not.lAlsoW)) Cycle
        nEq2D=nEq2D+tBnd(tOBJ(kO)%iPar(1))%nBC
!        if(iDL.ne.0_2) nEq2D=nEq2D+3
!        if(iDR.ne.0_2) nEq2D=nEq2D+3
        if(tOBJ(kO)%iTypO.eq.3) then ! Triangle
          PB(1:2)=tOBJ(kO)%O(1:2)
          PC(1)=tOBJ(kO)%O(3)
          PC(2)=tOBJ(kO)%e(1)
          PA(1:2)=tOBJ(kO)%e(2:3)
          call getTriangle2DData(PA,PB,PC,a,b,c,s,F,ha)
          nP=0
          nha=min(100_4,max(1_4,nint(ha/tOBJ(kO)%Par(1),4)))
          dh=ha/Dble(nha)
          h=-0.5d0*dh
          do i=1,nha
            h=h+dh
            na=min(100_4,max(1_4,nint(a*(1.0d0-h/ha)/tOBJ(kO)%Par(1),4)))
            nP=nP+na
          end do
        else ! Rectangle
          na= min(100_4,max(1_4,nint(tOBJ(kO)%Par(1)/tOBJ(kO)%Par(3),4)))
          nha=min(100_4,max(1_4,nint(tOBJ(kO)%Par(2)/tOBJ(kO)%Par(3),4)))
          dx=tOBJ(kO)%Par(1)/Dble(na)
          dh=tOBJ(kO)%Par(2)/Dble(nha)
          nP=na*nha
        end if
        mP=mP+nP
        nBndEq3D=nBndEq3D+nEq2D*nP
        nBndPt3D=nBndPt3D+nP
        if(.not.lcount) then
          i2=i1+nP
          i1=i1+1
          if(iDL.ne.-257_2) then
            if(iObjDra.gt.1) then ! use domain colors
              iC(i1)=iPackDom(nDom,iDL,iDR)
              if(i2.gt.i1) iC(i1+1:i2)=iC(i1)
            else
              iC(i1:i2)=-30001_2
            end if
          end if
          i=tBnd(tOBJ(kO)%iPar(1))%iMatOffset+1
          if(lsav.and.l2DBnd.and.(.not.l2DDom)) iC(i1:i2)=Int2(i)
          if(l2DNr) iC(i1:i2)=Int2(tOBJ(kO)%iPar(1))
          if(Present(iEQ)) iEQ(i1:i2)=nEq2D
          if(Present(iOC)) iOC(i1:i2)=kO
          Points(1:2,3,i1)=0.0d0
          Points(3,3,i1)=dh**2
          if(tOBJ(kO)%iTypO.eq.3) then ! Triangle
            Points(1:2,1,i1)=PC(1:2)-PB(1:2)
            call Unit2DV(Points(1:2,1,i1))
            Points(1:2,1,i1)=tOBJ(kO)%Par(1)*Points(1:2,1,i1)
            Points(3,1:2,i1)=0.0d0
            Points(1,2,i1)=-Points(2,1,i1)
            Points(2,2,i1)=Points(1,1,i1)
            call rvLoc2Glob(Points(1:3,1,i1),tObj(kO)%Plane,Points(1:3,1,i1))
            call rvLoc2Glob(Points(1:3,2,i1),tObj(kO)%Plane,Points(1:3,2,i1))
            call rvLoc2Glob(Points(1:3,3,i1),tObj(kO)%Plane,Points(1:3,3,i1))
            do i=i1+1,i2
              Points(1:3,0:3,i)=Points(1:3,0:3,i1)
            end do
            i1=i1-1
            h=-0.5d0*dh
            do i=1,nha
              h=h+dh
              xa=a*(1.0d0-h/ha)
              na=min(100_4,max(1_4,nint(xa/tOBJ(kO)%Par(1),4)))
              dx=xa/Dble(na)
              x=-0.5d0*dx
              xa=h*dsqrt(dabs((c/ha)**2-1.0d0))
              do k=1,na
                x=x+dx
                i1=i1+1
                Points(1,0,i1)=xa+x
                Points(2,0,i1)=h
                Points(3,0,i1)=0.0d0
                call vLoc2Glob(Points(1:3,0,i1),tObj(kO)%Plane,Points(1:3,0,i1))
              end do
            end do
          else ! Rectangle
            Points(1:3,1:2,i1)=0.0d0
            Points(1,1,i1)=dx
            Points(2,2,i1)=dh
            call rvLoc2Glob(Points(1:3,1,i1),tObj(kO)%Plane,Points(1:3,1,i1))
            call rvLoc2Glob(Points(1:3,2,i1),tObj(kO)%Plane,Points(1:3,2,i1))
            call rvLoc2Glob(Points(1:3,3,i1),tObj(kO)%Plane,Points(1:3,3,i1))
            do i=i1+1,i2
              Points(1:3,0:3,i)=Points(1:3,0:3,i1)
            end do
            i1=i1-1
            h=-0.5d0*dh
            do i=1,nha
              h=h+dh
              x=-0.5d0*dx
              do k=1,na
                x=x+dx
                i1=i1+1
                Points(1,0,i1)=x
                Points(2,0,i1)=h
                Points(3,0,i1)=0.0d0
                call vLoc2Glob(Points(1:3,0,i1),tObj(kO)%Plane,Points(1:3,0,i1))
              end do
            end do
          end if
        end if
        nPts(kO-k1+1)=mP
        Cycle
      end if
      if((tOBJ(kO)%iPar(1).gt.tOBJ(kO)%iPar(2)).or.(tOBJ(kO)%iPar(2).lt.1)) Cycle
      do k=max(1,tOBJ(kO)%iPar(1)),tOBJ(kO)%iPar(2)
        nBndPtI1=tBnd(k)%iMatOffset+1
        nBndPtI2=tBnd(k)%iMatOffset+tBnd(k)%nMat
        do i=nBndPtI1,nBndPtI2
          s=sBndPt(i)
          call GetBndPt(k,s,P(1),P(2),vt(1),vt(2),iDL,iDR,idum)
          nEq2D=0
          lAlsoW=.true.
          if((iAlsoW.lt.1).or.((iAlsoW.eq.1).and.(iDR.eq.IDL))) lAlsoW=.false.
          if((iDL.lt.-30000_2).or.((tBnd(idum)%weight.lt.pSmall)).and.(.not.lAlsoW)) Cycle
          nEq2D=nEq2D+tBnd(k)%nBC
 !         if(iDL.ne.0_2) nEq2D=nEq2D+3
 !         if(iDR.ne.0_2) nEq2D=nEq2D+3
          P(3)=0.0d0
          vt(3)=0.0d0
          if(i.gt.nBndPtI1) then
            if(i.lt.nBndPtI2) then
              ds=0.5d0*(sBndPt(i+1)-sBndPt(i-1))
            else
              ds=sBndPt(i)-sBndPt(i-1)
            end if
          else if(nBndPtI2.gt.nBndPtI1) then
            ds=sBndPt(i+1)-sBndPt(i)
          else
            ds=0.5d0*tBnd(k)%sLength
          end if
          vt=ds*vt
          Select Case(tOBJ(kO)%iTypO)
          Case(0) ! Torus
            s=P(1)*Pi*abs(tOBJ(kO)%Par(2))/180.0d0
          Case(1) ! Cylinder
            s=abs(tOBJ(kO)%Par(2))
          Case(2) ! Spiral
            r0=Dist3DPtAxis(P,tOBJ(kO)%O(1:3),tOBJ(kO)%e(1:3))
            s=SpiralLength(r0,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3))
          Case(5) ! Cone
            s=abs(tOBJ(kO)%Par(2))
          end Select
          if(tOBJ(kO)%Par(4).le.0.0d0) then ! fixed number of points
            nP=max(1_4,min(10000_4,nint(-tOBJ(kO)%Par(4),4)))
          else
            nP=max(1_4,min(10000_4,nint(tOBJ(kO)%Par(4)*abs(s.div.ds),4)))
          end if
          mP=mP+nP
          nBndEq3D=nBndEq3D+nEq2D*nP
          nBndPt3D=nBndPt3D+nP
          if(.not.lcount) then
            i2=i1+nP
            i1=i1+1
            Select Case(tOBJ(kO)%iTypO)
            Case(0) ! Torus
              if(lsav.and.((.not.l2DBnd).or.l2DDom)) then
                call Pt2Torus(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,iDL,iDR,iC(i1:i2),Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              else
                call Pt2Torus(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              end if
            Case(1) ! Cylinder
              if(lsav.and.((.not.l2DBnd).or.l2DDom)) then
                call Pt2Cylinder(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,iDL,iDR,iC(i1:i2),Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              else
                call Pt2Cylinder(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              end if
            Case(2) ! Spiral
              if(lsav.and.((.not.l2DBnd).or.l2DDom)) then
                call Pt2Spiral(P,tOBJ(kO)%O,tOBJ(kO)%e,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3), &
                & -nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,iDL,iDR,iC(i1:i2),Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              else
                call Pt2Spiral(P,tOBJ(kO)%O,tOBJ(kO)%e,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3), &
                & -nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              end if
            Case(5) ! Cone
              if(lsav.and.((.not.l2DBnd).or.l2DDom)) then
                call Pt2Cone(P,tOBJ(kO)%O,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,iDL,iDR,iC(i1:i2),Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              else
                call Pt2Cone(P,tOBJ(kO)%O,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,i1:i2), &
                & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,i1:i2),Points(1:3,2,i1:i2),Points(1:3,3,i1:i2),vt)
              end if
            end Select
            if(lsav.and.l2DBnd.and.(.not.l2DDom)) iC(i1:i2)=Int2(i)
            if(l2DNr) iC(i1:i2)=Int2(k)
            if(Present(iEQ)) iEQ(i1:i2)=nEq2D
            if(Present(iOC)) iOC(i1:i2)=kO
            i1=i2
          end if
        end do
      end do
      nPts(kO-k1+1)=mP
    end do
  end Subroutine gen3DMatPts

  Subroutine get3DMatPts(k1,k2,iAlsoW,iWhat0,lInhibit)
! count 3D matching points, allocate memory, compute
! iWhat=0: iBndPt3D contains color number
! iWhat=1: iBndPt3D contains number of corresponding 2D matching point
! iWhat=2: iBndPt3D contains domain numbers of corresponding 2D matching point
! iWhat=3: iBndPt3D contains boundary number of corresponding 2D matching point
! iWhat<0: use same as before
! iAlsoW=0: ignore boundaries with weight 0;
!        1: use boundaries with weight 0 if domain numbers are different
!        2: use all boundaries
    Implicit none
    Real(8) Pdum(3)
    Integer(4) k1,k2,kO,ier,idum,kPt
    Integer(2) iWhat0,iWhat
    Integer(4), intent(in) :: iAlsoW
    Logical, intent(in) :: lInhibit
    iWhat=iWhat0
    if(iWhat.lt.0) iWhat=iWhatiBndPt3D
    if((iWhat.eq.iWhatiBndPt3D).and.(.not.lGet3DMat)) return
! count 3D matching points
    tOBJ(1:nObj)%nMat=0
    tOBJ(1:nObj)%iMatOffset=0
    call gen3DMatPts(k1,k2,.true.,iAlsoW,tOBJ(k1:k2)%nMat,Pdum)
! allocate memory
    if(.not.lStopThread) then
      if((.not.Allocated(eBndPt3D)).or.(nBndPt3D.ne.mBndPt3DAlloc)) then 
        if(Allocated(eBndPt3D)) DeAllocate(eBndPt3D)
        if(Allocated(fBndPt3D)) DeAllocate(fBndPt3D)
        if(Allocated(BndPt3D)) Deallocate(BndPt3D)
        if(Allocated(iBndPt3D)) Deallocate(iBndPt3D)
        if(Allocated(iObjBndPt3D)) Deallocate(iObjBndPt3D)
        if(Allocated(iEquBndPt3D)) Deallocate(iEquBndPt3D)
        idum=max(1_4,nBndPt3D)
        Allocate(eBndPt3D(idum),fBndPt3D(idum),BndPt3D(3,0:3,idum),iBndPt3D(idum),iObjBndPt3D(idum),&
        &        iEquBndPt3D(idum),stat=ier)
        if(ier.ne.0) then
          idum=MessageBoxQQ('Memory alloction failed!'C,'Setup MMP matrix'C, &
                            MB$OK.or.MB$ICONSTOP)
          mBndPt3DAlloc=-1
          return
        end if
        mBndPt3DAlloc=idum
        eBndPt3D=0.0d0
        fBndPt3D=1.0d0
        iWhatiBndPt3D=-1_2
      end if
    end if
! compute 3D matching points, count max. number of equations, set offset info
    if(.not.lStopThread) then 
      iBndPt3D(1)=iWhat
      call gen3DMatPts(k1,k2,.false.,iAlsoW,tOBJ(k1:k2)%nMat,BndPt3D,iBndPt3D,iEquBndPt3D,iObjBndPt3D)
      tOBJ(k1)%iMatOffset=0
      do kO=k1+1,k2
        tOBJ(kO)%iMatOffset=tOBJ(kO-1)%iMatOffset+tOBJ(kO-1)%nMat
      end do
    end if
    iWhatiBndPt3D=iWhat
    lGet3DMat=.false.
! inhibit 3D matching points
    if(lInhibit) then
      call InhibitMatPts()
      nInhBndPt3D=0
      nInhEqu3D=0
      do kPt=1,nBndPt3D
        if(iObjBndPt3D(kPt).lt.0) then
          nInhBndPt3D=nInhBndPt3D+1
          nInhEqu3D=nInhEqu3D+iEquBndPt3D(kPt)
        end if
      end do
    end if
  end Subroutine get3DMatPts

  Subroutine GetExpsNearMatPts(k1,k2)
  ! count number of expansions associated with 3D matching point and store in iBndPt3D
  ! for objects k1...k2
    Real(8) f,fminL,fminR,f0
    Integer(4) k1,k2,k,kPt,kEx
    Integer(2) iL,iR,iD
    if(lgcFld) return
    f0=1.4d0
    do k=k1,k2 ! loop over 3D objects
      do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat ! loop over matching points
        if(iObjBndPt3D(kPt).lt.0) Cycle ! ignore dummy object (for integrals)
        iL=0
        iR=0
        fminL=1.0d300
        fminR=1.0d300
        do kEx=1,nExp
          if(LSkipExp(kEx)) Cycle
          if(igetiHE2(iHEGlobal,kEx).lt.0_2) Cycle
          if(tExp(kEx)%iObj.lt.0) Cycle
          iD=tExp(kEx)%iDom
          if((iD.gt.0).and.(iD.ne.tBnd(iBndPt3D(kPt))%iRDom).and.(iD.ne.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
          Select Case(tExp(kEx)%iTypE)
          Case(1_2) ! 2D multipole for 3D
          Case(6_2) ! 3D multipole
            f=r3Vec_Length(tExp(kEx)%Plane(1:3,0)-BndPt3D(1:3,0,kPt)).div.tExp(kEx)%dmin
            if(iD.eq.tBnd(iBndPt3D(kPt))%iLDom) then
              if(f.lt.f0) iL=min(215_2,iL+1_2)
              if(f.lt.fminL) fminL=f
            else if(iD.eq.tBnd(iBndPt3D(kPt))%iRDom) then
              if(f.lt.f0) iR=min(215_2,iR+1_2)
              if(f.lt.fminR) fminR=f
            end if
          Case(8_2) ! 3D ring multipole
          Case(9_2) ! 3D line multipole
          Case(10_2) ! 3D spiral multipole
          Case(12_2) ! 2D multilayer
          Case(13_2) ! 3D multilayer
          end Select
        end do
        if(iL.gt.3_2) then
          iL=5_2
        else if(iL.eq.3_2) then
          iL=2_2
        else if(iL.eq.2_2) then
          iL=3_2
        else if(iL.eq.1_2) then
          iL=4_2
        else
          iL=210_2-nint(Max(Min((20.0d0*fminL/f0),193.0d0),20.0d0),2) ! color numbers 190-17
        end if
        if(iR.gt.3_2) then
          iR=5_2
        else if(iR.eq.3_2) then
          iR=2_2
        else if(iR.eq.2_2) then
          iR=3_2
        else if(iR.eq.1_2) then
          iR=4_2
        else
          iR=210_2-nint(Max(Min((20.0d0*fminR/f0),193.0d0),20.0d0),2)
        end if
        iBndPt3D(kPt)=iPackObjC(iL,iR)
      end do
    end do
  end Subroutine GetExpsNearMatPts

  Subroutine DistPtObj(kO,lO,r,lNoPer,dmin0,rNmin0,iDom0,val0,lInh,lRSidemin0,iDom2)
! find the boundary point rNmin0 with the shortest distance dmin0 from the given
! point r. Check objects kO...lO (all objects except object lO for kO=0,...)
! return domain number iDom0, and boundary values val0 in addition to dmin0,rNmin0
    Implicit none
    Real(8) r(3),dmin,dmi,rNmin(3),rNmi(3),dmin0,rNmin0(3),val(2),val0(2),rP0(3),rP(3),fv,d,f0,r0,x0,f1,f2,fNmin,a,b
    Integer(4) k1,k2,k,kPtmin,kPt,ixzSymm0,iyzSymm0
    Integer(2), Intent(in):: kO,lO
    Integer(2) iDom,iDom0,iTO,iDom1
    Integer(2), Optional:: iDom2
    Logical, intent(in) :: lNoPer,lInh
    Logical lLoc,lRSidemin
    Logical, Optional:: lRSidemin0
    if(lgcFld) then
      call DistPtBnd(kO,lO,r(1),r(2),.true.,lNoPer,dmin0,rNmin0(1),rNmin0(2),lRSidemin,iDom0,val0,iDom1)
      if(Present(lRSidemin0)) lRSidemin0=lRSidemin
      if(Present(iDom2)) iDom2=iDom1
      rNmin0(3)=r(3)
      return
    end if
    if((kO.ge.1).and.(kO.le.nObj)) then
      if((lO.ge.1).and.(lO.le.nObj)) then
        k1=min(kO,lO)
        k2=max(kO,lO)
      else
        k1=kO
        k2=k1
      end if
    else if((kO.lt.0).and.(-kO.lt.nObj)) then
      k1=1
      k2=-kO
    else
      k1=1
      k2=nObj
    end if
    rP0=r
    fv=1.0d0
    if(iyzSymm.ne.0) rP0(1)=dabs(rP0(1))
    if(ixzSymm.ne.0) rP0(2)=dabs(rP0(2))
    if(ixySymm.ne.0) rP0(3)=dabs(rP0(3))
    if((iyzSymm.eq.2).and.(dabs(r(1)-rP0(1)).gt.pSmall)) fv=-fv 
    if((ixzSymm.eq.2).and.(dabs(r(2)-rP0(2)).gt.pSmall)) fv=-fv 
    if((ixySymm.eq.2).and.(dabs(r(3)-rP0(3)).gt.pSmall)) fv=-fv 
    call MoveToOrigCell(rP0)
    dmin0=pBig
    rNmin0(1:3)=0.0d0
    iDom0=-9_2
    iDom1=-9_2
    val0(1)=0.0d0
    val0(2)=0.0d0
    do k=k1,k2
      if((kO.eq.0).and.(lO.eq.k)) Cycle
      if((tOBJ(k)%iTypO.ne.3).and.(tOBJ(k)%iTypO.ne.4).and.((tOBJ(k)%iPar(1).gt.tOBJ(k)%iPar(2)) &
      & .or.(tOBJ(k)%iPar(2).lt.1))) Cycle
      iTO=tOBJ(k)%iTypO
      if(lInh.and.(tOBJ(k)%nInhibited.gt.0)) then
        if(tOBJ(k)%nInhibited.ge.tOBJ(k)%nMat) Cycle
        iTO=-1
      end if
      Select Case(iTO)
      Case(0) ! Torus around ey
        call vGlob2Loc(rP0,tOBJ(k)%Plane,rP)
        lLoc=.true.
        f0=modulo(datan2(rP(3),rP(1)),2.0d0*Pi) ! polar coordinates in x,z-plane
        r0=dsqrt(rP(3)**2+rP(1)**2)
        f1=modulo(Pi*tOBJ(k)%Par(1)/180.0d0,2.0d0*Pi) ! min. and max. rotation angles within the interval 0...2Pi
        f2=modulo(Pi*(tOBJ(k)%Par(1)+abs(tOBJ(k)%Par(2)))/180.0d0,2.0d0*Pi)
        if((f2-f1).lt.1.0d-12) f2=f2+2.0d0*Pi
        if((f0-f1).lt.0.0d0) f0=f0+2.0d0*Pi
        if(f0.le.f2) then ! inside arc area
          d=r0
          fNmin=f0
        else
          a=f0-f1
          if(a.gt.Pi) a=2.0d0*Pi-a
          b=f0-f2
          if(b.gt.Pi) b=2.0d0*Pi-b
          if(abs(a).le.abs(b)) then
            d=r0*dcos(a)
            fNmin=f1
          else
            d=r0*dcos(b)
            fNmin=f2
          end if
        end if
        iyzSymm0=iyzSymm
        ixzSymm0=ixzSymm
        iyzSymm=0 ! no symmetry for local coordinates
        ixzSymm=0
        call DistPtBnd(tOBJ(k)%iPar(1),tOBJ(k)%iPar(2),d,rP(2),.false.,lNoPer,dmin,x0,rNmin(2),lRSidemin,iDom,val,iDom1)
        iyzSymm=iyzSymm0 ! restore old symmetry
        ixzSymm=ixzSymm0
        if((iDom.lt.-4_2).or.((iDom.lt.0_2).and.(iDom1.lt.0_2))) Cycle
        rNmin(1)=x0*dcos(fNmin)
        rNmin(3)=x0*dsin(fNmin)
      Case(1) ! Cylinder along ez
        call vGlob2Loc(rP0,tOBJ(k)%Plane,rP)
        lLoc=.true.
        if(rP(3).lt.tOBJ(k)%Par(1)) then
          rNmin(3)=tOBJ(k)%Par(1)
        else if(rP(3).gt.(tOBJ(k)%Par(1)+abs(tOBJ(k)%Par(2)))) then
          rNmin(3)=tOBJ(k)%Par(1)+abs(tOBJ(k)%Par(2))
        else
          rNmin(3)=rP(3)
        end if
        iyzSymm0=iyzSymm
        ixzSymm0=ixzSymm
        iyzSymm=0 ! no symmetry for local coordinates
        ixzSymm=0
        call DistPtBnd(tOBJ(k)%iPar(1),tOBJ(k)%iPar(2),rP(1),rP(2),.false.,lNoPer,dmin,rNmin(1),rNmin(2),lRSidemin,iDom, &
        & val,iDom1)
        iyzSymm=iyzSymm0 ! restore old symmetry
        ixzSymm=ixzSymm0
        if((iDom.lt.-4_2).or.((iDom.lt.0_2).and.(iDom1.lt.0_2))) Cycle
      Case(3) ! Triangle
        if(.not.((tBnd(tObj(k)%iPar(1))%iCond.lt.-100_2).and.lNoPer)) then
          call vGlob2Loc(rP0,tOBJ(k)%Plane,rP)
          lLoc=.true.
          call DistPtTriangle(rP(1),rP(2),tOBJ(k)%O(1),tOBJ(k)%O(2),tOBJ(k)%O(3),tOBJ(k)%e(1),tOBJ(k)%e(2),tOBJ(k)%e(3),dmin,rNmin(1),rNmin(2),lRSidemin)
          rNmin(3)=0.0d0
          if(rP(3).lt.0.0d0) then
            iDom=tBnd(tObj(k)%iPar(1))%iRDom
            iDom1=tBnd(tObj(k)%iPar(1))%iLDom
            lRSidemin=.true.
          else
            iDom=tBnd(tObj(k)%iPar(1))%iLDom
            iDom1=tBnd(tObj(k)%iPar(1))%iRDom
            lRSidemin=.false.
          end if
          if((iDom.lt.-4_2).or.((iDom.lt.0_2).and.(iDom1.lt.0_2))) Cycle
          val=tBnd(tObj(k)%iPar(1))%val
        end if
      Case(4) ! Rectangle
        if(.not.((tBnd(tObj(k)%iPar(1))%iCond.lt.-100_2).and.lNoPer)) then
          call vGlob2Loc(rP0,tOBJ(k)%Plane,rP)
          lLoc=.true.
          call DistPtRectangle(rP(1),rP(2),tOBJ(k)%Par(1),tOBJ(k)%Par(2),dmin,rNmin(1),rNmin(2),lRSidemin)
          rNmin(3)=0.0d0
          if(rP(3).lt.0.0d0) then
            iDom=tBnd(tObj(k)%iPar(1))%iRDom
            iDom1=tBnd(tObj(k)%iPar(1))%iLDom
            lRSidemin=.true.
          else
            iDom=tBnd(tObj(k)%iPar(1))%iLDom
            iDom1=tBnd(tObj(k)%iPar(1))%iRDom
            lRSidemin=.false.
          end if
          if((iDom.lt.-4_2).or.((iDom.lt.0_2).and.(iDom1.lt.0_2))) Cycle
          val=tBnd(tObj(k)%iPar(1))%val
        end if
      Case(5) ! Cone
        call vGlob2Loc(rP0,tOBJ(k)%Plane,rP)
        lLoc=.true.
        if(rP(3).lt.tOBJ(k)%Par(1)) then
          rNmin(3)=tOBJ(k)%Par(1)
        else if(rP(3).gt.(tOBJ(k)%Par(1)+abs(tOBJ(k)%Par(2)))) then
          rNmin(3)=tOBJ(k)%Par(1)+abs(tOBJ(k)%Par(2))
        else
          rNmin(3)=rP(3)
        end if
        iyzSymm0=iyzSymm
        ixzSymm0=ixzSymm
        iyzSymm=0 ! no symmetry for local coordinates
        ixzSymm=0
        call DistPtBnd(tOBJ(k)%iPar(1),tOBJ(k)%iPar(2),rP(1),rP(2),.false.,lNoPer,dmin,rNmin(1),rNmin(2),lRSidemin,iDom, &
        & val,iDom1)
        iyzSymm=iyzSymm0 ! restore old symmetry
        ixzSymm=ixzSymm0
        if((iDom.lt.-4_2).or.((iDom.lt.0_2).and.(iDom1.lt.0_2))) Cycle
      Case Default ! use matching points
        call get3DMatPts(1,nObj,1_4,3_2,.true.)
        rP=rP0
        lLoc=.false.
        dmin=pBig
        rNmin(1:3)=pBig
        kPtmin=1
        do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
          if(iObjBndPt3D(kPt).lt.0) Cycle
          rNmi(1:3)=BndPt3D(1:3,0,kPt)
          dmi=dsqrt((rNmi(1)-rP(1))**2+(rNmi(2)-rP(2))**2+(rNmi(3)-rP(3))**2)
          if(dmi.lt.dmin) then
            dmin=dmi
            rNmin=rNmi
            kPtmin=kPt
          end if
        end do
        dmi=Dot_Product(BndPt3D(1:3,3,kPtmin),rNmin-rP)
        if(dmi.lt.0.0d0) then
          iDom=tBnd(iBndPt3D(kPtmin))%iRDom
          iDom1=tBnd(tObj(k)%iPar(1))%iLDom
          lRSidemin=.true.
        else
          iDom=tBnd(iBndPt3D(kPtmin))%iLDom
          iDom1=tBnd(tObj(k)%iPar(1))%iRDom
          lRSidemin=.false.
        end if
        if((iDom.lt.-4_2).or.((iDom.lt.0_2).and.(iDom1.lt.0_2))) Cycle
        val=tBnd(iBndPt3D(kPtmin))%val
      end Select
      dmin=dsqrt((rNmin(1)-rP(1))**2+(rNmin(2)-rP(2))**2+(rNmin(3)-rP(3))**2)
      if(dmin.lt.dmin0) then
        dmin0=dmin
        rNmin0=rNmin
        iDom0=iDom
        val0=val
        if(Present(lRSidemin0)) lRSidemin0=lRSidemin
        if(Present(iDom2)) iDom2=iDom1
        if(lLoc) call vLoc2Glob(rNmin0,tOBJ(k)%Plane,rNmin0)
      end if
    end do
    if(val0(1).gt.pBig) val0(1)=0.0d0
    if(val0(2).gt.pBig) val0(2)=0.0d0
  end Subroutine DistPtObj
  
  Subroutine GenObjCylinder(z0,dz,a,iB1,iB2)
! generate a new 3D object in form of a cylinder, based on the 2D Boundaries iB1...iB2
    Implicit none
    Real(8) z0,dz,a
    Integer(4) iB1,iB2
    Logical ldum
    call InsertObj(nOBJ,1,ldum)
    tObj(nOBJ)%iTypO=1_2
    tObj(nOBJ)%iPar(1)=Int2(iB1)
    tObj(nOBJ)%iPar(2)=Int2(iB2)
    tObj(nOBJ)%Par(1)=z0
    tObj(nOBJ)%Par(2)=dz
    tObj(nOBJ)%Par(4)=a ! matching point aspect ratio
  end Subroutine GenObjCylinder
  
  Subroutine GenObjRectangle(dx,dy,d,dens,dm,iB,nM,mO)
! generate a new 3D object in form of a rectangle, based on the 2D Boundary iB
    Implicit none
    Real(8) dx,dy,d,dens,dm
    Integer(4) iB,nM,mO
    Logical ldum
    call InsertObj(nOBJ,1,ldum)
    tObj(nOBJ)%iTypO=4_2
    tObj(nOBJ)%iPar(1)=Int2(iB) ! use data of boundary iB
    tObj(nOBJ)%iPar(2)=Int2(nM) ! max. multipoles per side
    tObj(nOBJ)%iPar(2)=Int2(mO) ! max. multipole order and degree
    tObj(nOBJ)%Par(1)=dx
    tObj(nOBJ)%Par(2)=dy
    tObj(nOBJ)%Par(3)=d ! matching point side length
    tObj(nOBJ)%Par(4)=dens ! multipole density
    tObj(nOBJ)%Par(5)=dm ! multipole distance
  end Subroutine GenObjRectangle
  
  Subroutine GenObjSpiral(dr,dphi,dz,a,iB1,iB2)
! generate a new 3D object in form of a spiral, based on the 2D Boundaries iB1...iB2
    Implicit none
    Real(8) dr,dphi,dz,a
    Integer(4) iB1,iB2
    Logical ldum
    call InsertObj(nOBJ,1,ldum)
    tObj(nOBJ)%iTypO=2_2
    tObj(nOBJ)%iPar(1)=Int2(iB1)
    tObj(nOBJ)%iPar(2)=Int2(iB2)
    tObj(nOBJ)%Par(1)=dr
    tObj(nOBJ)%Par(2)=dphi
    tObj(nOBJ)%Par(3)=dz
    tObj(nOBJ)%Par(4)=a ! matching point aspect ratio
  end Subroutine GenObjSpiral
  
  Subroutine GenObjTorus(a0,da,a,iB1,iB2,iOb)
! generate a new 3D object in form of a torus, based on the 2D Boundaries iB1...iB2
! if 0<iOb<nObj: overwrite existing object iOb
    Implicit none
    Real(8) a0,da,a
    Integer(4) iB1,iB2,iOb
    Logical ldum
    if((iOb.lt.1).or.(iOb.gt.nObj)) then
      call InsertObj(nOBJ,1,ldum)
      tObj(nOBJ)%iTypO=0_2
      tObj(nOBJ)%iPar(1)=Int2(iB1)
      tObj(nOBJ)%iPar(2)=Int2(iB2)
      tObj(nOBJ)%Par(1)=a0
      tObj(nOBJ)%Par(2)=da
      tObj(nOBJ)%Par(4)=a ! matching point aspect ratio
    else
      tObj(iOb)%iTypO=0_2
      tObj(iOb)%iPar(1)=Int2(iB1)
      tObj(iOb)%iPar(2)=Int2(iB2)
      tObj(iOb)%Par(1)=a0
      tObj(iOb)%Par(2)=da
      tObj(iOb)%Par(4)=a ! matching point aspect ratio
    end if
  end Subroutine GenObjTorus
  
  Subroutine GenObjTriangle(Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,d,dens,dm,iB,nM,mO)
! generate a new 3D object in form of a triangle, based on the 2D Boundariy iB
    Implicit none
    Real(8) Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,d,dens,dm,dum
    Integer(4) iB,nM,mO
    Logical ldum
    call InsertObj(nOBJ,1,ldum)
    tObj(nOBJ)%iTypO=3_2
    tObj(nOBJ)%iPar(1)=Int2(iB) ! use data of boundary iB
    tObj(nOBJ)%iPar(2)=Int2(nM) ! max. multipoles per side
    tObj(nOBJ)%iPar(2)=Int2(mO) ! max. multipole order and degree
    tObj(nOBJ)%Par(1)=d ! matching point side length
    tObj(nOBJ)%Par(2)=dens ! multipole density
    tObj(nOBJ)%Par(3)=dm ! multipole distance
    points3D(1,1)=Ax
    points3D(2,1)=Ay
    points3D(3,1)=Az
    points3D(1,2)=Bx
    points3D(2,2)=By
    points3D(3,2)=Bz
    points3D(1,3)=Cx
    points3D(2,3)=Cy
    points3D(3,3)=Cz
    call vLoc2Glob(points3D(1:3,1),tOBJ(nOBJ)%plane,points3D(1:3,1))
    call vLoc2Glob(points3D(1:3,2),tOBJ(nOBJ)%plane,points3D(1:3,2))
    call vLoc2Glob(points3D(1:3,3),tOBJ(nOBJ)%plane,points3D(1:3,3))
    tOBJ(nOBJ)%plane(1:3,0)=points3D(1:3,1)
    tOBJ(nOBJ)%plane(1:3,1)=points3D(1:3,2)-points3D(1:3,1)
    tOBJ(nOBJ)%plane(1:3,2)=points3D(1:3,3)-points3D(1:3,1)
    tOBJ(nOBJ)%plane(1:3,3)=r3Vec_Prod(tOBJ(nOBJ)%plane(1:3,1),tOBJ(nOBJ)%plane(1:3,2))
    call Ortho3DSpace(tOBJ(nOBJ)%plane)
    tOBJ(nOBJ)%O(1)=0.0d0
    tOBJ(nOBJ)%O(2)=0.0d0
    tOBJ(nOBJ)%O(3)=r3Vec_Length(points3D(1:3,2)-points3D(1:3,1))
    tOBJ(nOBJ)%e(1)=0.0d0
    call Proj3D(points3D(1:3,3),tOBJ(nOBJ)%plane,points3D(1:3,3),pBig,tOBJ(nOBJ)%e(2),tOBJ(nOBJ)%e(3),dum)
  end Subroutine GenObjTriangle
  
  Subroutine GenObjCone(z0,dz,dM,a,O,iB1,iB2)
! generate a new 3D object in form of a cone, based on the 2D Boundaries iB1...iB2
    Implicit none
    Real(8) z0,dz,dM,O(3),a
    Integer(4) iB1,iB2
    Logical ldum
    call InsertObj(nOBJ,1,ldum)
    tObj(nOBJ)%iTypO=5_2
    tObj(nOBJ)%iPar(1)=Int2(iB1)
    tObj(nOBJ)%iPar(2)=Int2(iB2)
    tObj(nOBJ)%Par(1)=z0
    tObj(nOBJ)%Par(2)=dz
    tObj(nOBJ)%Par(3)=dM
    tObj(nOBJ)%Par(4)=a ! matching point aspect ratio
    tObj(nOBJ)%O(1:3)=O(1:3)
  end Subroutine GenObjCone
  
  Subroutine GenObjExp(kOb,jE0,kE1,kE2,kE3,iCl0,iCl2,iCn,iOb,lDel,nMP)
! generate 3D expansions for the object kOb (all objects if kOb=0, object 1...-kOb if kOb<0)
! insert after expansion jE0
! if kE1>0: set min./max. expansion numbers of the objects equal to kE1,kE2
! if kE2<=0: max. expansion number = nExp-kE2
! use same color as 3d object if iCl2>-1: use this as new color instead of object color
! filers: color (iCl0), connection (iCn), object (iOb) (only for cylinder, torus, spiral, cone!)
! delete previously generated expansions if lDel=.true.
    Implicit none
    Real(8), Allocatable :: Points(:,:,:),Orig(:,:)
    Integer(2), Allocatable :: iOrd(:)
    Real(8) dmin,Pn(3),Ps(3),val(2),s,P(3),vt(3),r0,PA(2),PB(2),PC(2),a,b,c,F,P3,rminmax,zs,z1,dz,aA,d,zO
    Integer(4) kOb,jE0,jE,k1,k2,k,kk,kO,iB,nP,mP,j,iEr,idum,kE1,kE2,kE3,iCl0,iCl2,iCn,iOb,nMultiP,nMP
    Integer(2) iDL1,iDL,iDR,iCl(1)
    Logical ldum,lRSidemin,lDel
    nMultiP=nMP
    lGet3DMat=.true.
    if((kOb.gt.0).and.(kOb.le.nObj)) then
      k1=kOb
      k2=k1
    else if(kOb.lt.0) then
      k1=1
      k2=min(-kOb,nObj)
    else
      k1=1
      k2=nObj
    end if
! delete previous expansions that were generated automatically
    if(lDel) then
      do k=nExp-1,1,-1
        if((tExp(k)%iObj.lt.k1).or.(tExp(k)%iObj.gt.k2)) Cycle
        call InsertExp(k,-1,ldum)
      end do
    end if
    jE=jE0
    if(jE.lt.0) jE=nExp+jE0
! generate new expansions and insert after expansion jE
    do kO=k1,k2
      if(kE1.gt.0) then
        tObj(kO)%iPar(3)=Int2(min(kE1,nExp))
        if(kE3.gt.-32000) tObj(kO)%iPar(5)=Int2(kE3)
        if(kE2.gt.0) then
          tObj(kO)%iPar(4)=Int2(min(kE2,nExp))
        else
          tObj(kO)%iPar(4)=Int2(max(kE1,nExp+kE2))
        end if
      end if
      if((tOBJ(kO)%iTypO.eq.3).or.(tOBJ(kO)%iTypO.eq.4)) then ! Triangle or Rectangle
        iDL=tBnd(tOBJ(kO)%iPar(1))%iLDom
        iDR=tBnd(tOBJ(kO)%iPar(1))%iRDom
        if((iDL.lt.-30000_2).or.(iDL.eq.iDR).or.(tOBJ(kO)%iPar(2).eq.0)) Cycle
        nP=min(100,abs(tOBJ(kO)%iPar(2)))
        if(Allocated(iOrd)) Deallocate(iOrd)
        if(Allocated(Orig)) Deallocate(Orig)
        Allocate(Orig(3,nP),iOrd(nP),stat=ier)
        if(ier.ne.0) then
          idum=MessageBoxQQ('Memory alloction failed!'C,'Generate Expansions'C, &
                          MB$OK.or.MB$ICONSTOP)
          return
        end if
        call gen3DMatPts(kO,kO,.true.,1_4,tOBJ(kO:kO)%nMat,P)
        if(tOBJ(kO)%iTypO.eq.3) then ! Triangle
          PA(1:2)=tOBJ(kO)%O(1:2)
          PB(1)=tOBJ(kO)%O(3)
          PB(2)=tOBJ(kO)%e(1)
          PC(1:2)=tOBJ(kO)%e(2:3)
          call getMPforTriangle2D(PA,PB,PC,nP,tOBJ(kO)%Par(2),Orig(1:2,1:nP),Orig(3,1:nP))
          call getTriangle2DData(PA,PB,PC,a,b,c,s,F)
          P3=tOBJ(kO)%Par(3)
          do j=1,nP ! compute max. orders
            s=Pi*Orig(3,j)**2
            mP=Int4(Dble(tOBJ(kO)%nMat)*s/(F*max(1.0d0,BndOver)))
            iOrd(j)=Min(tOBJ(kO)%iPar(3),Int2(dsqrt(Dble(mP)-1.0d0))-1_2)
            if(iOrd(j).lt.1) Exit
          end do
        else ! Rectangle
          if(tOBJ(kO)%iPar(2).gt.0) then ! discrete set of multipoles
            call getMPforRectangle2D(tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),nP,Orig(1:2,1:nP),Orig(3,1:nP))
            F=tOBJ(kO)%Par(1)*tOBJ(kO)%Par(2)
            do j=1,nP ! compute max. orders
              s=Pi*Orig(3,j)**2
              mP=Int4(Dble(tOBJ(kO)%nMat)*s/(F*max(1.0d0,BndOver)))
              iOrd(j)=Min(tOBJ(kO)%iPar(3),Int2(dsqrt(Dble(mP)-1.0d0))-1_2)
              if(iOrd(j).lt.1) Exit
            end do
          else ! line multipoles
            c=tOBJ(kO)%Par(2)/Dble(nP)
            a=tOBJ(kO)%Par(1)-c
            if(a.gt.(0.99999d0*c)) then
              b=0.5d0*c
            else
              b=0.0d0
              a=tOBJ(kO)%Par(1)
            end if
            mP=Int4(a/(tOBJ(kO)%Par(3)*max(1.0d0,BndOver)))
            do j=1,nP
              Orig(1,j)=b
              Orig(2,j)=(0.5d0+Dble(j-1))*c
              Orig(3,j)=0.5d0*c
              iOrd(j)=Min(tOBJ(kO)%iPar(3),Int2(Dble(mP)/2.0d0)-1_2)
              if(iOrd(j).lt.1) Exit
            end do
          end if
          F=tOBJ(kO)%Par(1)*tOBJ(kO)%Par(2)
          P3=tOBJ(kO)%Par(5)
        end if
        nP=j-1
        Orig(3,1:nP)=P3*Orig(3,1:nP)
        if(iDR.gt.0_2) then
          do j=1,nP
            iB=jE
            call InsertExp(iB,1,ldum)
            if(.not.ldum) return
            iB=iB+1
            tExp(iB)=tExp(1)
            rminmax=0.0d0
            if((tExp(iB)%iTypE.eq.1).or.(tExp(iB)%iTypE.eq.2)) rminmax=tExp(iB)%rE(2)
            if((tExp(iB)%iTypE.eq.6).or.(tExp(iB)%iTypE.eq.7)) rminmax=tExp(iB)%rE(1)
            tExp(iB)%rE(1:5)=0.0
            tExp(iB)%iE(1:6)=0
            tExp(iB)%iE(5)=1
            call vLoc2Glob(Orig(1:3,j),tObj(kO)%Plane,tExp(iB)%Plane(1:3,0))
            if(tOBJ(kO)%iPar(2).gt.0) then ! discrete set of multipoles
              tExp(iB)%Plane(1:3,1:3)=tObj(kO)%Plane(1:3,1:3)
              tExp(iB)%iTypE=6
              tExp(iB)%rE(1)=rminmax
              tExp(iB)%iE(2)=iOrd(j)
              tExp(iB)%iE(4)=iOrd(j)
            else ! line multipoles
              tExp(iB)%Plane(1:3,1)=tObj(kO)%Plane(1:3,2)
              tExp(iB)%Plane(1:3,2)=tObj(kO)%Plane(1:3,3)
              tExp(iB)%Plane(1:3,3)=tObj(kO)%Plane(1:3,1)
              tExp(iB)%iTypE=9_2
              tExp(iB)%rE(4)=rminmax
              tExp(iB)%iE(1)=-1 
              tExp(iB)%iE(2)=-1
              tExp(iB)%iE(4)=iOrd(j)
              tExp(iB)%iE(6)=4*iOrd(j)
              tExp(iB)%rE(2)=a
              tExp(iB)%rE(3)=1.0d0
            end if
            tExp(iB)%nPar=0
            tExp(iB)%iDom=iDR
            tExp(iB)%iHE=iHEGlobal
            tExp(iB)%iS=0
            tExp(iB)%xo=tExp(iB)%Plane(1,0)
            tExp(iB)%yo=tExp(iB)%Plane(2,0)
            tExp(iB)%iCol=tOBJ(kO)%iCol
            if(iCl2.gt.-1) tExp(iB)%iCol=iCl2
            tExp(iB)%iConn=tBnd(tOBJ(kO)%iPar(1))%iConn
            tExp(iB)%iObj=kO
            tExp(iB)%gc=gcFld
            tExp(iB)%depend=1.0d100
            mP=4*(1+tExp(iB)%iE(2)-tExp(iB)%iE(1))*(1+tExp(iB)%iE(4)-tExp(iB)%iE(3))
            if(igetiHE(iB).gt.1_2) mP=2*mP
            call InsertPar(iB,0,mP,ldum)
            if(.not.ldum) return
            do kk=1,tExp(iB)%nPar
              ParExp(1:nRHS,tExp(iB)%iOff+kk)=(0.0d0,0.0d0)
              iParExp(1:nRHS,tExp(iB)%iOff+kk)=0_2
            end do
            k1=kExp
            kExp=iB
            call RepExpTest(mP)
            kExp=k1
          end do
        end if
        if(iDL.gt.0_2) then
          Orig(3,1:nP)=-Orig(3,1:nP)
          do j=1,nP
            iB=jE
            call InsertExp(iB,1,ldum)
            if(.not.ldum) return
            iB=iB+1
            tExp(iB)=tExp(1)
            rminmax=0.0d0
            if((tExp(iB)%iTypE.eq.1).or.(tExp(iB)%iTypE.eq.2)) rminmax=tExp(iB)%rE(2)
            if((tExp(iB)%iTypE.eq.6).or.(tExp(iB)%iTypE.eq.7)) rminmax=tExp(iB)%rE(1)
            tExp(iB)%rE(1:5)=0.0
            tExp(iB)%iE(1:6)=0
            tExp(iB)%iE(5)=1
            call vLoc2Glob(Orig(1:3,j),tObj(kO)%Plane,tExp(iB)%Plane(1:3,0))
            if(tOBJ(kO)%iPar(2).gt.0) then ! discrete set of multipoles
              tExp(iB)%Plane(1:3,1:3)=tObj(kO)%Plane(1:3,1:3)
              tExp(iB)%iTypE=6_2
              tExp(iB)%rE(1)=rminmax
              tExp(iB)%iE(2)=iOrd(j)
              tExp(iB)%iE(4)=iOrd(j)
            else ! line multipoles
              tExp(iB)%Plane(1:3,1)=tObj(kO)%Plane(1:3,2)
              tExp(iB)%Plane(1:3,2)=tObj(kO)%Plane(1:3,3)
              tExp(iB)%Plane(1:3,3)=tObj(kO)%Plane(1:3,1)
              tExp(iB)%iTypE=9_2
              tExp(iB)%rE(4)=rminmax
              tExp(iB)%iE(1)=-1 
              tExp(iB)%iE(2)=-1
              tExp(iB)%iE(4)=iOrd(j)
              tExp(iB)%iE(6)=4*iOrd(j)
              tExp(iB)%rE(2)=a
              tExp(iB)%rE(3)=1.0d0
            end if
            tExp(iB)%nPar=0
            tExp(iB)%iDom=iDL
            tExp(iB)%iHE=iHEGlobal
            tExp(iB)%iS=0
            tExp(iB)%xo=tExp(iB)%Plane(1,0)
            tExp(iB)%yo=tExp(iB)%Plane(2,0)
            tExp(iB)%iCol=tOBJ(kO)%iCol
            if(iCl2.gt.-1) tExp(iB)%iCol=iCl2
            tExp(iB)%iConn=tBnd(tOBJ(kO)%iPar(1))%iConn
            tExp(iB)%iObj=kO
            tExp(iB)%gc=gcFld
            tExp(iB)%depend=1.0d100
            mP=4*(1+tExp(iB)%iE(2)-tExp(iB)%iE(1))*(1+tExp(iB)%iE(4)-tExp(iB)%iE(3))
            if(igetiHE(iB).gt.1_2) mP=2*mP
            call InsertPar(iB,0,mP,ldum)
            if(.not.ldum) return
            do kk=1,tExp(iB)%nPar
              ParExp(1:nRHS,tExp(iB)%iOff+kk)=(0.0d0,0.0d0)
              iParExp(1:nRHS,tExp(iB)%iOff+kk)=0_2
            end do
            k1=kExp
            kExp=iB
            call RepExpTest(mP)
            kExp=k1
          end do
        end if
        Deallocate(Orig,iOrd)
        Cycle
      end if
      do k=max(1,tOBJ(kO)%iPar(3)),tOBJ(kO)%iPar(4)
        if((iCl0.gt.0).and.(tExp(k)%iCol.ne.iCl0)) Cycle
        if((iCl0.lt.0).and.(tExp(k)%iCol.le.-iCl0)) Cycle
        if((iCn.gt.0).and.(tExp(k)%iConn.ne.iCn)) Cycle
        if((iCn.lt.0).and.(tExp(k)%iConn.le.-iCn)) Cycle
        if((iOb.gt.0).and.(tExp(k)%iObj.ne.iOb)) Cycle
        if((iOb.lt.0).and.(tExp(k)%iObj.le.-iOb)) Cycle
        tExp(k)%iObj=-1 ! ignore this expansion when MMP matrix is created
        P(1)=tExp(k)%xo
        P(2)=tExp(k)%yo
        P(3)=0.0d0
        call DistPtBnd(0,0,P(1),P(2),.true.,.true.,dmin,Pn(1),Pn(2),lRSidemin,iDL1,val)
        Pn(3)=0.0d0
        if(iDL1.lt.0) iDL1=0
        Select Case(tOBJ(kO)%iTypO)
        Case(0) ! Torus -> ring
          s=P(1)*Pi*tOBJ(kO)%Par(2)/180.0d0
          r0=P(1)
        Case(1) ! Cylinder -> line
          s=tOBJ(kO)%Par(2)
          r0=s
        Case(2) ! Spiral -> spiral
          r0=Dist3DPtAxis(P(1:3),tOBJ(kO)%O(1:3),tOBJ(kO)%e(1:3))
          s=SpiralLength(r0,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3))
        Case(5) ! Cone -> line
          call Dist3DPtLine(Pn,P,tOBJ(kO)%O,Ps,aA,d)
          zs=Ps(3)
          zO=tOBJ(kO)%O(3)
          z1=tOBJ(kO)%Par(1)+zs*(zO-tOBJ(kO)%Par(1))/zO
          dz=(zO-zs)*tOBJ(kO)%Par(2)/zO
          s=r3Vec_Length(Pn-tOBJ(kO)%O)*tOBJ(kO)%Par(2)/zO
          r0=s
        end Select
        nP=tOBJ(kO)%iPar(5)      
        if(nMP.gt.1) then
          nMultiP=nMP*min(-nint(2.0d0*s/dmin),-2)
        else if(nMP.gt.-1) then
          nMultiP=min(-nint(2.0d0*s/dmin),-2)
        end if
        if(abs(r0).lt.0.01d0*abs(dmin)) nP=1 ! set single multipole on axis
        if(nP.ne.0_2) then ! generate set of expansions
          if(nP.lt.0) nP=min(-nP,nint(min(32000.0d0,abs(s.div.dmin)),4))
          if(Allocated(Points)) Deallocate(Points)
          Allocate(Points(3,0:3,nP),stat=ier)
          if(ier.ne.0) then
            idum=MessageBoxQQ('Memory alloction failed!'C,'Generate Expansions'C, &
                              MB$OK.or.MB$ICONSTOP)
            return
          end if
          vt(1:3)=tExp(k)%Plane(1:3,1)
          Select Case(tOBJ(kO)%iTypO)
          Case(0) ! Torus -> ring
            if(nP.eq.1) P(1)=0.0d0 ! set point on axis
            call Pt2Torus(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,1:nP), &
            & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,1:nP),Points(1:3,2,1:nP),Points(1:3,3,1:nP),vt)
          Case(1) ! Cylinder -> line
            call Pt2Cylinder(P,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),-nP,tOBJ(kO)%Plane,Points(1:3,0,1:nP), &
            & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,1:nP),Points(1:3,2,1:nP),Points(1:3,3,1:nP),vt)
          Case(2) ! Spiral -> spiral
            if(nP.eq.1) call Set3DPtOnAxis(P,tOBJ(kO)%O,tOBJ(kO)%e)
            call Pt2Spiral(P,tOBJ(kO)%O,tOBJ(kO)%e,tOBJ(kO)%Par(1),tOBJ(kO)%Par(2),tOBJ(kO)%Par(3), &
            & -nP,tOBJ(kO)%Plane,Points(1:3,0,1:nP), &
            & iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,1:nP),Points(1:3,2,1:nP),Points(1:3,3,1:nP),vt)
          Case(5) ! Cone -> line
            c=max(0.0d0,min(1.0d0,tOBJ(kO)%Par(3)))
            call Pt2Cone(P,tOBJ(kO)%O,z1+c*d,dz-c*d*(1.0d0+(zO-z1-dz)/(zO-z1)),-nP,tOBJ(kO)%Plane, &
            & Points(1:3,0,1:nP),iObjDra,nDom,-257_2,iDR,iCl,Points(1:3,1,1:nP),Points(1:3,2,1:nP),Points(1:3,3,1:nP),vt)
          end Select
          do j=1,nP
            call Ortho3DSpace(Points(1:3,0:3,j))
            iB=jE
            call InsertExp(iB,1,ldum)
            if(.not.ldum) return
            iB=iB+1
            tExp(iB)=tExp(k)
            rminmax=0.0d0
            if((tExp(iB)%iTypE.eq.1).or.(tExp(iB)%iTypE.eq.2)) rminmax=tExp(iB)%rE(2)
            if((tExp(iB)%iTypE.eq.6).or.(tExp(iB)%iTypE.eq.7)) rminmax=tExp(iB)%rE(1)
            tExp(iB)%nPar=0
            tExp(iB)%iTypE=tExp(k)%iTypE+5
            tExp(iB)%rE(2:5)=0.0
            tExp(iB)%rE(1)=rminmax
            if(tExp(iB)%iE(1).lt.1) tExp(iB)%iE(1)=0
            tExp(iB)%iE(2)=max(tExp(iB)%iE(1),tExp(k)%iE(2))
            tExp(iB)%iE(3)=max(0,tExp(k)%iE(3))
            tExp(iB)%iE(4)=max(tExp(iB)%iE(3),tExp(k)%iE(4))
            tExp(iB)%iE(5)=1
            tExp(iB)%iE(6)=0
            tExp(iB)%Plane(1:3,0)=Points(1:3,0,j)
            if((tOBJ(kO)%iTypO.eq.0).and.(nP.eq.1)) then
              tExp(iB)%Plane(1:3,1:3)=Points(1:3,1:3,j)
            else
              tExp(iB)%Plane(1:3,1)=Points(1:3,3,j)
              tExp(iB)%Plane(1:3,2)=Points(1:3,1,j)
              tExp(iB)%Plane(1:3,3)=Points(1:3,2,j)
            end if
            tExp(iB)%xo=tExp(iB)%Plane(1,0)
            tExp(iB)%yo=tExp(iB)%Plane(2,0)
            tExp(iB)%iCol=tOBJ(kO)%iCol
            if(iCl2.gt.-1) tExp(iB)%iCol=iCl2
            tExp(iB)%iObj=kO
            tExp(iB)%gc=gcFld
            tExp(iB)%depend=1.0d100
            mP=4*(1+tExp(iB)%iE(2)-tExp(iB)%iE(1))*(1+tExp(iB)%iE(4)-tExp(iB)%iE(3))
            if(igetiHE(iB).gt.1_2) mP=2*mP
            call InsertPar(iB,0,mP,ldum)
            if(.not.ldum) return
            do kk=1,tExp(iB)%nPar
              ParExp(1:nRHS,tExp(iB)%iOff+kk)=(0.0d0,0.0d0)
              iParExp(1:nRHS,tExp(iB)%iOff+kk)=0_2
            end do
            k1=kExp
            kExp=iB
            call RepExpTest(mP)
            kExp=k1
          end do
          if(Allocated(Points)) Deallocate(Points)
        else ! generate ring/line/spiral/cone expansions
          if((tExp(k)%iTypE.lt.1).or.(tExp(k)%iTypE.gt.2)) Cycle
          iB=jE
          call InsertExp(iB,1,ldum)
          if(.not.ldum) return
          iB=iB+1
          tExp(iB)=tExp(k)
          rminmax=0.0d0
          if((tExp(iB)%iTypE.eq.1).or.(tExp(iB)%iTypE.eq.2)) rminmax=tExp(iB)%rE(2)
          if((tExp(iB)%iTypE.eq.6).or.(tExp(iB)%iTypE.eq.7)) rminmax=tExp(iB)%rE(1)
          tExp(iB)%Plane=tOBJ(kO)%Plane
          tExp(iB)%iCol=tOBJ(kO)%iCol
          if(iCl2.gt.-1) tExp(iB)%iCol=iCl2
          tExp(iB)%iObj=kO
          tExp(iB)%nPar=0
          tExp(iB)%iE(1)=max(0,tExp(k)%iE(1))
          tExp(iB)%iE(2)=max(tExp(k)%iE(1),tExp(k)%iE(2))
          if(tOBJ(kO)%Par(4).gt.0.0d0) then
            tExp(iB)%iE(4)=Int(min(Dble(tExp(iB)%iE(4)),Dble(tExp(iB)%iE(2))*s*tOBJ(kO)%Par(4)/dmin))
          else
            tExp(iB)%iE(4)=min(tExp(iB)%iE(4),-Int(tOBJ(kO)%Par(4)/2.0d0))
          end if
          tExp(iB)%iE(4)=max(0,tExp(iB)%iE(3),tExp(iB)%iE(4))
          tExp(iB)%gc=gcFld
          tExp(iB)%depend=1.0d100
          call DistPtBnd(0,0,P(1),P(2),.true.,.true.,dmin,Pn(1),Pn(2),lRSidemin,iDL1,val)
          Pn(3)=0.0d0
          if(iDL1.lt.0) iDL1=0
          Select Case(tOBJ(kO)%iTypO)
          Case(0) ! Torus -> ring
            tExp(iB)%iTypE=8_2
            tExp(iB)%Plane(1:3,0)=tExp(iB)%Plane(1:3,0)+P(2)*tExp(iB)%Plane(1:3,2)
            tExp(iB)%xo=tExp(iB)%Plane(1,0)
            tExp(iB)%yo=tExp(iB)%Plane(2,0)
            tExp(iB)%rE(1)=P(1)
            tExp(iB)%rE(2)=tOBJ(kO)%Par(1)
            tExp(iB)%rE(3)=tOBJ(kO)%Par(2)
            tExp(iB)%rE(4)=1.0d0
            if(fGenExpObj.gt.pSmall) tExp(iB)%rE(4)=fGenExpObj
            tExp(iB)%rE(5)=rminmax
          Case(1) ! Cylinder -> line
            tExp(iB)%iTypE=9_2
            tExp(iB)%Plane(1:3,0)=tExp(iB)%Plane(1:3,0)+P(1)*tExp(iB)%Plane(1:3,1)
            tExp(iB)%Plane(1:3,0)=tExp(iB)%Plane(1:3,0)+P(2)*tExp(iB)%Plane(1:3,2)
            tExp(iB)%xo=tExp(iB)%Plane(1,0)
            tExp(iB)%yo=tExp(iB)%Plane(2,0)
            tExp(iB)%rE(1)=tOBJ(kO)%Par(1)
            tExp(iB)%rE(2)=tOBJ(kO)%Par(2)
            tExp(iB)%rE(3)=1.0d0
            if(fGenExpObj.gt.pSmall) tExp(iB)%rE(3)=fGenExpObj
            tExp(iB)%rE(4)=rminmax
            tExp(iB)%rE(5)=0.0d0
          Case(2) ! Spiral -> spiral
            tExp(iB)%iTypE=10_2
            tExp(iB)%xo=P(1)
            tExp(iB)%yo=P(2)
            tExp(iB)%O=tOBJ(kO)%O
            tExp(iB)%e=tOBJ(kO)%e
            tExp(iB)%rE(1)=tOBJ(kO)%Par(1)
            tExp(iB)%rE(2)=tOBJ(kO)%Par(2)
            tExp(iB)%rE(3)=tOBJ(kO)%Par(3)
            tExp(iB)%rE(4)=1.0d0
            if(fGenExpObj.gt.pSmall) tExp(iB)%rE(4)=fGenExpObj
            tExp(iB)%rE(5)=rminmax
          Case(5) ! Cone -> line
            tExp(iB)%iTypE=9_2
            tExp(iB)%Plane(1:3,0)=P+(tOBJ(kO)%O-P)*(z1/tOBJ(kO)%O(3))
            tExp(iB)%Plane(1:3,3)=(tOBJ(kO)%O-P)*(dz/tOBJ(kO)%O(3))
            tExp(iB)%rE(1)=0.0d0
            tExp(iB)%rE(2)=s
            call Ortho3DSpace2(tExp(iB)%Plane,3)
            tExp(iB)%xo=tExp(iB)%Plane(1,0)
            tExp(iB)%yo=tExp(iB)%Plane(2,0)
            tExp(iB)%rE(3)=1.0d0
            if(fGenExpObj.gt.pSmall) tExp(iB)%rE(3)=fGenExpObj
            tExp(iB)%rE(4)=rminmax
            tExp(iB)%rE(5)=0.0d0
          end Select
          if(fGenExpObj.lt.nSmall) then
            mP=Int4(min(100.0d0,dble(max(1,tExp(iB)%iE(2)))*abs((-fGenExpObj*s).div.(6.3d0*dmin))))
            tExp(iB)%iE(3)=0
            tExp(iB)%iE(4)=max(0,mP)
          end if
          if(tExp(iB)%iE(2).lt.1) tExp(iB)%iE(1:2)=-1_2
          tExp(iB)%iE(5)=1
          tExp(iB)%iE(6)=tExp(k)%iE(6)
          if((tExp(iB)%iE(6).gt.-1).and.(tExp(iB)%iE(6).lt.2)) tExp(iB)%iE(6)=nMultiP
          mP=4*(1+tExp(iB)%iE(2)-tExp(iB)%iE(1))*(1+tExp(iB)%iE(4)-tExp(iB)%iE(3))
          if(igetiHE(iB).gt.1_2) mP=2*mP
          call InsertPar(iB,0,mP,ldum)
          if(.not.ldum) return
          do kk=1,tExp(iB)%nPar
            ParExp(1:nRHS,tExp(iB)%iOff+kk)=(0.0d0,0.0d0)
            iParExp(1:nRHS,tExp(iB)%iOff+kk)=0_2
          end do
          k1=kExp
          kExp=iB
          call RepExpTest(mP)
          kExp=k1
        end if
      end do
    end do
  end Subroutine GenObjExp 

  Subroutine GenObjAxi(kBn,jE0,kE1,kE2,iCl2,lDel,nMP,maxDeg,resol,over)
! generate 3D objects and expansions (mostly ring multipoles) for the boundary kBn (all objects if kBn=0, boundary 1...-kBn if kBn<0)
! insert object after last object
! insert expansions after expansion jE0
! if kE1>0: set min./max. expansion numbers of the objects equal to kE1,kE2
! if kE2<=0: max. expansion number = nExp-kE2
! if iCl2>-1: use this as color for all expansions, otherwise use same color as 3d object
! delete previously generated expansions if lDel=.true.
! multiply number of multipoles along ring by nMP
! set maxDeg for all expansions
    Implicit none
    Real(8) dmin,Pn(3),val(2),s,P(3),r0,rminmax,phi0,dphi,aspRatio,dFactor,resol,over
    Integer(4) kBn,jE0,jE,k1,k2,k,kk,kO,iB,kB,nP,mP,kE1,kE2,iCl2,nMultiP,nMP,minDeg,maxDeg,mDegStep,nSymm
    Integer(2) iDL1,iEvenOdd
    Logical ldum,lRSidemin,lDel
    nMultiP=nMP ! number of multipoles along ring
    lGet3DMat=.true.
    if((kBn.gt.0).and.(kBn.le.nBnd)) then
      k1=kBn
      k2=k1
    else if(kBn.lt.0) then
      k1=1
      k2=min(-kBn,nBnd)
    else
      k1=1
      k2=nBnd
    end if
! delete previous expansions that were generated automatically
    if(lDel) then
      do k=nExp-1,1,-1
        if((tExp(k)%iObj.lt.k1).or.(tExp(k)%iObj.gt.k2)) Cycle
        call InsertExp(k,-1,ldum)
      end do
    end if
! generate objects for each boundary
    phi0=0.0d0
    dphi=360.0d0
    dFactor=1.0d0
    minDeg=0
    mDegStep=1
    nSymm=1
    iEvenOdd=Int2(ixySymm)
    if(ixySymm.ne.0) then
      nSymm=2
      dphi=180.0d0
      if(iyzSymm.ne.0) then
        nSymm=4
        mDegStep=2
        dphi=90.0d0
        dFactor=2.0d0
        if(iyzSymm.eq.1) minDeg=1
      end if
    end if
    aspRatio=-over*(1.0d0+Dble((2*maxDeg)/nSymm)) ! number of points in phi direction
    do kB=k1,k2
      call GenObjTorus(phi0,dphi,aspRatio,kB,kB,kB) ! generate new object (overwite if object kB already there, otherwise, append)
      jE=jE0
      if(jE.lt.0) jE=nExp+jE0
      kO=min(max(1,kB),nObj) ! generate new expansions for new object kO and insert after expansion jE
      if(kE1.gt.0) then
        tObj(kO)%iPar(3)=Int2(min(kE1,nExp))
        tObj(kO)%iPar(5)=0_2
        if(kE2.gt.0) then
          tObj(kO)%iPar(4)=Int2(min(kE2,nExp))
        else
          tObj(kO)%iPar(4)=Int2(max(kE1,nExp+kE2))
        end if
      end if
      if(resol.lt.1.0d-300) then
        tObj(kO)%GrfRes=tBnd(kO)%Length/(-resol)
      else
        tObj(kO)%GrfRes=resol
      end if
      do k=max(1,tOBJ(kO)%iPar(3)),tOBJ(kO)%iPar(4)
        tExp(k)%iObj=-1 ! ignore this expansion when MMP matrix is created
        P(1)=tExp(k)%xo
        P(2)=tExp(k)%yo
        P(3)=0.0d0
        call DistPtBnd(0,0,P(1),P(2),.true.,.true.,dmin,Pn(1),Pn(2),lRSidemin,iDL1,val)
        Pn(3)=0.0d0
        if(iDL1.lt.0) iDL1=0
        s=P(1)*Pi*tOBJ(kO)%Par(2)/180.0d0
        r0=P(1)
        nP=tOBJ(kO)%iPar(5)      
        if(nMP.gt.1) then
          nMultiP=nMP*min(-nint(2.0d0*s/dmin),-2)
        else if(nMP.gt.-1) then
          nMultiP=min(-nint(2.0d0*s/dmin),-2)
        end if
        if((tExp(k)%iTypE.lt.1).or.(tExp(k)%iTypE.gt.2)) Cycle
        iB=jE ! generate expansion
        call InsertExp(iB,1,ldum)
        if(.not.ldum) return
        iB=iB+1
        tExp(iB)=tExp(k)
        tExp(iB)%Plane=tOBJ(kO)%Plane
        tExp(iB)%iCol=tOBJ(kO)%iCol
        if(iCl2.gt.-1) tExp(iB)%iCol=iCl2
        tExp(iB)%iObj=kO
        tExp(iB)%nPar=0
        tExp(iB)%iE(1)=max(0,tExp(k)%iE(1)) ! min order
        tExp(iB)%iE(2)=max(tExp(k)%iE(1),tExp(k)%iE(2)) ! max order
        tExp(iB)%iE(3)=minDeg
        tExp(iB)%iE(4)=maxDeg
        tExp(iB)%gc=gcFld
        tExp(iB)%depend=1.0d100
        call DistPtBnd(0,0,P(1),P(2),.true.,.true.,dmin,Pn(1),Pn(2),lRSidemin,iDL1,val)
        rminmax=dmin*0.5d0
        Pn(3)=0.0d0
        tExp(iB)%Plane(1:3,0)=tExp(iB)%Plane(1:3,0)+P(2)*tExp(iB)%Plane(1:3,2)
        tExp(iB)%xo=tExp(iB)%Plane(1,0)
        tExp(iB)%yo=tExp(iB)%Plane(2,0)
        if(iDL1.lt.0) iDL1=0
        if(abs(r0).lt.0.01d0*abs(dmin)) then ! set 3D standard multipole on axis
          tExp(iB)%iE(5)=1
          tExp(iB)%iE(6)=0
          tExp(iB)%iHE=0_2 ! both E and H, even and odd
          tExp(iB)%iTypE=6_2
          if(maxDeg.le.tExp(iB)%iE(2)) then ! 3D multipole type in Y direction
            tExp(iB)%Plane(1:3,1:3)=0.0d0
            tExp(iB)%Plane(1,1)=1.0d0
            tExp(iB)%Plane(3,2)=1.0d0
            tExp(iB)%Plane(2,3)=-1.0d0
          else ! 3D multipole type in Z direction, degree <-> order
            tExp(iB)%Plane(1:3,1:3)=0.0d0
            tExp(iB)%Plane(1,1)=1.0d0
            tExp(iB)%Plane(2,2)=1.0d0
            tExp(iB)%Plane(3,3)=1.0d0
            tExp(iB)%iE(3)=tExp(k)%iE(1) ! min order -> degree
            tExp(iB)%iE(4)=tExp(k)%iE(2) ! max order -> degree
            tExp(iB)%iE(1)=minDeg
            tExp(iB)%iE(2)=maxDeg
          end if
          tExp(iB)%rE(1)=rminmax ! minimum radius
          tExp(iB)%rE(2)=0.0d0   ! imag. x
          tExp(iB)%rE(3)=0.0d0   ! imag. y
          tExp(iB)%rE(4)=0.0d0   ! imag.z
          tExp(iB)%rE(5)=0.0d0   ! cut angle
          mP=8*(1+tExp(iB)%iE(2)-tExp(iB)%iE(1))*(1+tExp(iB)%iE(4)-tExp(iB)%iE(3))/nSymm ! number of parameters
        else ! ring multipole
          tExp(iB)%iE(5)=mDegStep
          tExp(iB)%iHE=0_2+10_2*iEvenOdd ! both E and H, even odd depending on symmetry
          tExp(iB)%iTypE=8_2 ! ring type
          tExp(iB)%rE(1)=P(1) ! ring radius
          tExp(iB)%rE(2)=phi0 ! minimum angle
          tExp(iB)%rE(3)=dphi ! sector angle
          tExp(iB)%rE(4)=dFactor ! degree factor
          tExp(iB)%rE(5)=rminmax ! minimum radius
          if(tExp(iB)%iE(2).lt.1) tExp(iB)%iE(1:2)=-1_2 ! min,max orders (thin ring)
          tExp(iB)%iE(6)=tExp(k)%iE(6) ! N multipoles
          if((tExp(iB)%iE(6).gt.-1).and.(tExp(iB)%iE(6).lt.2)) tExp(iB)%iE(6)=nMultiP
          mP=8*(1+tExp(iB)%iE(2)-tExp(iB)%iE(1))*(1+tExp(iB)%iE(4)-tExp(iB)%iE(3))/nSymm ! number of parameters
        end if
        call InsertPar(iB,0,mP,ldum)
        if(.not.ldum) return
        do kk=1,tExp(iB)%nPar
          ParExp(1:nRHS,tExp(iB)%iOff+kk)=(0.0d0,0.0d0)
          iParExp(1:nRHS,tExp(iB)%iOff+kk)=0_2
        end do
        k1=kExp
        kExp=iB
        call RepExpTest(mP)
        kExp=k1
      end do
    end do
  end Subroutine GenObjAxi 

  Subroutine GetDminSpiralMatch(spac,x0,y0,dr,da,dz,iD,dmin)
! get 3D matching point with domain number iD that is closest to a spiral and return the distance dmin
    Implicit none
    Real(8) spac(3,0:3),x0,y0,dr,da,dz,Pn(3),dmi,dmin,dmx
    Integer(4) k,kPt
    Integer(2) iD
    dmin=pBig
    if(lgcFld) return
    do k=1,nObj
      do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
        if(iObjBndPt3D(kPt).lt.0) Cycle ! ignore dummy object (for integrals)
        if((iD.gt.0).and.(iD.ne.tBnd(iBndPt3D(kPt))%iRDom).and.(iD.ne.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        if((iD.lt.0).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iRDom).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        call DistPtSpiral3D(BndPt3D(1:3,0,kPt),spac,x0,y0,dr,da,dz,dmx,Pn,dmi)
        if(dmi.lt.dmin) dmin=dmi
      end do
    end do
  end Subroutine GetDminSpiralMatch

  Subroutine GetDminRingMatch(spac,r,f0,da,iD,dmin)
! get 3D matching point with domain number iD that is closest to a ring and return the distance dmin
    Implicit none
    Real(8) spac(3,0:3),r,f0,da,Pn(3),dmi,dmin,dsPn
    Integer(4) k,kPt
    Integer(2) iD
    dmin=pBig
    if(lgcFld) return
    do k=1,nObj
      do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
        if(iObjBndPt3D(kPt).lt.0) Cycle ! ignore dummy object (for integrals)
        if((iD.gt.0).and.(iD.ne.tBnd(iBndPt3D(kPt))%iRDom).and.(iD.ne.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        if((iD.lt.0).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iRDom).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        call DistPtRing3D(BndPt3D(1:3,0,kPt),spac,r,f0,da,Pn,dsPn,dmi)
        if(dmi.lt.dmin) dmin=dmi
      end do
    end do
  end Subroutine GetDminRingMatch

  Subroutine GetDminInfiniteLineMatch(O,e,iD,dmin)
! get 3D matching point with domain number iD that is closest to an infinite line (origin o, direction e) and return the distance dmin
    Implicit none
    Real(8) O(3),e(3),Pn(3),dmi,dmin,aO
    Integer(4) k,kPt
    Integer(2) iD
    dmin=pBig
    if(lgcFld) return
    do k=1,nObj
      do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
        if(iObjBndPt3D(kPt).lt.0) Cycle ! ignore dummy object (for integrals)
        if((iD.gt.0).and.(iD.ne.tBnd(iBndPt3D(kPt))%iRDom).and.(iD.ne.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        if((iD.lt.0).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iRDom).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        call Dist3DPtInfiniteLine(BndPt3D(1:3,0,kPt),O,e,Pn,aO,dmi)
        if(dmi.lt.dmin) dmin=dmi
      end do
    end do
  end Subroutine GetDminInfiniteLineMatch

  Subroutine GetDminFiniteLineMatch(A,B,iD,dmin)
! get 3D matching point with domain number iD that is closest to an finite line (from A to B) and return the distance dmin
    Implicit none
    Real(8) A(3),B(3),Pn(3),dmi,dmin,aO
    Integer(4) k,kPt
    Integer(2) iD
    dmin=pBig
    if(lgcFld) return
    do k=1,nObj
      do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
        if(iObjBndPt3D(kPt).lt.0) Cycle ! ignore dummy object (for integrals)
        if((iD.gt.0).and.(iD.ne.tBnd(iBndPt3D(kPt))%iRDom).and.(iD.ne.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        if((iD.lt.0).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iRDom).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        call Dist3DPtFiniteLine(BndPt3D(1:3,0,kPt),A,B,Pn,aO,dmi)
        if(dmi.lt.dmin) dmin=dmi
      end do
    end do
  end Subroutine GetDminFiniteLineMatch

  Subroutine GetDminPtMatch(r,iD,dmin)
! get 3D matching point with domain number iD that is closest to point r and return the distance dmin
    Implicit none
    Real(8) r(3),rNmi(3),dmi,dmin
    Integer(4) k,kPt
    Integer(2) iD
    dmin=pBig
    if(lgcFld) return
    do k=1,nObj
      do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
        if(iObjBndPt3D(kPt).lt.0) Cycle ! ignore dummy object (for integrals)
        if((iD.gt.0).and.(iD.ne.tBnd(iBndPt3D(kPt))%iRDom).and.(iD.ne.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        if((iD.lt.0).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iRDom).and.(-iD.lt.tBnd(iBndPt3D(kPt))%iLDom)) Cycle
        rNmi(1:3)=BndPt3D(1:3,0,kPt)-r(1:3)
        dmi=r3Vec_Length(rNmi)
        if(dmi.lt.dmin) dmin=dmi
      end do
    end do
  end Subroutine GetDminPtMatch

  Subroutine GetDminExpsMatch()
! get shortest distances of expansions from 3D matching points with same domain numbers
! use only expansions of certain types
    Implicit none
    Integer(4) i
    Real(8) A(3),B(3)
    if(lgcFld) return
    call get3DMatPts(1,nObj,1_4,3_2,.true.)
    do i=1,nExp
      Select Case(tExp(i)%iTypE)
      Case(1_2) ! 2D multipole
        call GetDminInfiniteLineMatch(tExp(i)%plane(1:3,0),tExp(i)%plane(1:3,3),tExp(i)%iDom,tExp(i)%dmin)
      Case(6_2) ! 3D multipole
        call GetDminPtMatch(tExp(i)%plane(1:3,0),tExp(i)%iDom,tExp(i)%dmin)
      Case(8_2) ! 3D ring multipole
        call GetDminRingMatch(tExp(i)%plane,tExp(i)%rE(1),tExp(i)%rE(2)*Pi/180.0d0,tExp(i)%rE(3)*Pi/180.0d0, &
        & tExp(i)%iDom,tExp(i)%dmin)
      Case(9_2) ! 3D line multipole
        A(1:3)=tExp(i)%plane(1:3,0)+tExp(i)%rE(1)*tExp(i)%plane(1:3,3)
        B(1:3)=A(1:3)+tExp(i)%rE(2)*tExp(i)%plane(1:3,3)
        call GetDminFiniteLineMatch(A,B,tExp(i)%iDom,tExp(i)%dmin)
      Case(10_2) ! 3D spiral multipole
        call GetDminSpiralMatch(tExp(i)%plane,tExp(i)%xO,tExp(i)%yO,tExp(i)%rE(1),tExp(i)%rE(2)*Pi/180.0d0,tExp(i)%rE(3), &
        & tExp(i)%iDom,tExp(i)%dmin)
      Case(12_2) ! 2D multilayer
        call GetDminInfiniteLineMatch(tExp(i)%plane(1:3,0),tExp(i)%plane(1:3,3),-tExp(i)%iE(1),tExp(i)%dmin) ! multidomain!
      Case(13_2) ! 3D multilayer
        call GetDminPtMatch(tExp(i)%plane(1:3,0),-tExp(i)%iE(1),tExp(i)%dmin) ! multidomain!
      end Select
    end do
  end Subroutine GetDminExpsMatch

  Subroutine GetDminExpExp(k,dmin)
! get shortest distance dmin of expansion k from all other expansions with identical domain numbers
! use only expansions of type 6, i.e., 3D multipoles
    Implicit none
    Real(8) r(3),rNmi(3),dmi,dmin
    Integer(4) i,k
    dmin=pBig
    if((k.lt.1).or.(k.gt.nExp)) return
    r(1:3)=tExp(k)%plane(1:3,0)
    do i=1,nExp
      if((k.eq.i).or.(tExp(k)%iDom.ne.tExp(i)%iDom)) Cycle
      if((tExp(k)%iTypE.eq.6).and.(tExp(i)%iTypE.eq.6)) then
        rNmi(1:3)=tExp(i)%plane(1:3,0)
        dmi=dsqrt((rNmi(1)-r(1))**2+(rNmi(2)-r(2))**2+(rNmi(3)-r(3))**2)
        if(dmi.lt.dmin) dmin=dmi
      end if
    end do
  end Subroutine GetDminExpExp

  Subroutine GetDependExp(lgetDmin)
! get dependence factors for all expansions
! When lAll is false: use only expansions of type 6, i.e., 3D multipoles
! When lgetDmin is true: evaluate the distances of the expansions from the closest matching points first
    Implicit none
    Real(8) dmin
    Integer(4) k
    Logical, intent(in) :: lgetDmin
    if(lgetDmin) call GetDminExpsMatch()
    do k=1,nExp
      if(tExp(k)%iTypE.eq.6) then
        call GetDminExpExp(k,dmin)
        if(dmin.gt.1.0d299) then
          tExp(k)%depend=pBig
        else
          tExp(k)%depend=dmin.div.tExp(k)%dmin
        end if
      else
        tExp(k)%depend=pBig
      end if
    end do
  end Subroutine GetDependExp

  Subroutine DelDependExp(lExc)
! delete all 3D multipoles with identical domain numbers that are too close to each other
! when lExc is true: never delete the last expansion (excitation)
    Implicit none
    Integer(4) i,k,n,nE,idel
    Logical, intent(in) :: lExc
    Logical ldum
    call GetDependExp(.true.)
    n=nExp
    do k=1,n
      if(nExp.lt.2) Exit
      idel=0
      nE=nExp
      if(lExc) nE=nE-1
      do i=1,nE
        if(tExp(i)%depend.lt.dep_delExp) then
          if(idel.gt.0) then
            if(tExp(i)%depend.lt.tExp(idel)%depend) idel=i
          else
            idel=i
          end if
        end if
      end do
      if(idel.lt.1) Exit
      call InsertExp(idel,-1,ldum)
      call GetDependExp(.false.)
    end do
  end Subroutine DelDependExp

  Subroutine InhibitMatPts()
! Inhibit matching points of 3D objects
    Implicit none
    Real(8) val0(2),dmin0,rNmin0(3),r(3)
    Integer(4) iOK,kPt,i
    Integer(2) ip,lp,kO,lO,iDom0
    Logical lRSidemin0,lLef,lRig,lAdd,lDel
    Character(32) s
    if(lgcFld) return
    do kInhibit=1,nInhibit
      s=sInhibit(kInhibit)
      if((s(1:1).eq.'a').or.(s(1:1).eq.'A')) then
        lAdd=.true.
        lDel=.false.
      else if((s(1:1).eq.'d').or.(s(1:1).eq.'D')) then
        lAdd=.false.
        lDel=.true.
      else
        Cycle
      end if
      lp=1
      do ip=2,32
        if((s(ip:ip).eq.'l').or.(s(ip:ip).eq.'L')) then
          lLef=.true.
          lRig=.false.
          lp=ip
        else if((s(ip:ip).eq.'r').or.(s(ip:ip).eq.'R')) then
          lLef=.false.
          lRig=.true.
          lp=ip
        else
          Cycle
        end if
      end do
      if((lp.lt.3).or.(lp.gt.31)) Cycle
      call StrToInt(s(2:lp-1),i,iOK)
      if(iOK.ne.0) Cycle
      kO=i
      call StrToInt(s(lp+1:32),i,iOK)
      lO=i
      if(kO.eq.lO) Cycle
      do kPt=tOBJ(kO)%iMatOffset+1,tOBJ(kO)%iMatOffset+tOBJ(kO)%nMat
        if((lDel.and.(iObjBndPt3D(kPt).lt.0)).or.(lAdd.and.(iObjBndPt3D(kPt).gt.0))) Cycle
        r(1:3)=BndPt3D(1:3,0,kPt)
        call DistPtObj(lO,lO,r,.true.,dmin0,rNmin0,iDom0,val0,.false.,lRSidemin0)
        if((lRSidemin0.and.lRig).or.((.not.lRSidemin0).and.lLef)) iObjBndPt3D(kPt)=-iObjBndPt3D(kPt)
        if(lStopThread) return
      end do
      call GetInhibited(Int4(kO))
    end do
  end Subroutine InhibitMatPts

  Subroutine GetInhibited(k)
    Implicit none
    Integer(4) k,kPt
    tOBJ(k)%nInhibited=0
    do kPt=tOBJ(k)%iMatOffset+1,tOBJ(k)%iMatOffset+tOBJ(k)%nMat
      if(iObjBndPt3D(kPt).lt.0) tOBJ(k)%nInhibited=tOBJ(k)%nInhibited+1
    end do
  end Subroutine GetInhibited

  Subroutine BlowObject(kOb,f,kOb2)
    Implicit none
    Real(8) f
    Integer(4) kOb,k1,k2,kO
    Integer(4), Optional:: kOb2
    if(Present(kOb2)) then
      call GetRange(kOb,nObj,k1,k2,kOb2)
    else
      call GetRange(kOb,nObj,k1,k2)
    end if
    do kO=k1,k2
      tObj(kO)%Plane(1:3,0)=tObj(kO)%Plane(1:3,0)*f
      tObj(kO)%GrfRes=tObj(kO)%GrfRes*f
      select case(tObj(kO)%iTypO)
      case(1_2)
        tObj(kO)%Par(1)=tObj(kO)%Par(1)*f
        tObj(kO)%Par(2)=tObj(kO)%Par(2)*f
      case(2_2)
        tObj(kO)%Par(1)=tObj(kO)%Par(1)*f
        tObj(kO)%Par(3)=tObj(kO)%Par(3)*f
        tObj(kO)%O(1:3)=tObj(kO)%O(1:3)*f
      case(3_2)
        tObj(kO)%Par(1)=tObj(kO)%Par(1)*f
        tObj(kO)%Par(3)=tObj(kO)%Par(3)*f
        tObj(kO)%O(1:3)=tObj(kO)%O(1:3)*f
        tObj(kO)%e(1:3)=tObj(kO)%e(1:3)*f
      case(4_2)
        tObj(kO)%Par(1)=tObj(kO)%Par(1)*f
        tObj(kO)%Par(2)=tObj(kO)%Par(2)*f
        tObj(kO)%Par(3)=tObj(kO)%Par(3)*f
      end select
    end do
  end Subroutine BlowObject

END MODULE CHOBJ
