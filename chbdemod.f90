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
MODULE CHBDE

! General and auxiliary routines for boundaries, domains, expansions

  USE CHFOR

  SAVE

  CONTAINS

! Dialogs

  Subroutine InsertDialog(lCheck)
! dialog for the space data
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical, intent(in) :: lCheck
    Logical ldum
	  Type(dialog) dlg
    Integer(4), save:: IsOpen
    Integer(4) idum
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Insert,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Insert dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      kInsObj=nInsObj
      lInsObj=.true.
      call SetInsertData(dlg)
      if(lCheck) then
        ldum=DlgSet(Dlg,IDC_INSERT_AFTER,.true.,DLG_ENABLE)
      else
        ldum=DlgSet(Dlg,IDC_INSERT_AFTER,.false.,DLG_ENABLE)
      end if
	    ldum=DlgSetSub(dlg,IDC_INSERT_KOBJ,updateInsert)
	    ldum=DlgSetSub(dlg,IDC_INSERT_AFTER,updateInsert)
	    ldum=DlgSetSub(dlg,idOK,updateInsert)
	    ldum=DlgSetSub(dlg,IDCANCEL,updateInsert)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine InsertDialog

  Subroutine SetInsertData(dlg)
! set the Insert data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    call DlgSetI(Dlg,idc_Insert_kObj,0,kInsObj,0,nInsObj)
    call DlgSetL(Dlg,IDC_INSERT_AFTER,lInsObj)
  end Subroutine SetInsertData

  Subroutine GetInsertData(dlg)
! get the Insert data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
	  call DlgGetI(Dlg,idc_Insert_kObj,0,0,kInsObj,0,nInsObj,nInsObj)
    call DlgGetL(Dlg,IDC_INSERT_AFTER,lInsObj)
  end Subroutine GetInsertData

  Subroutine updateInsert(dlg,control_name,callbackType)
  ! callback for InsertDialog
    Implicit none
    Integer(4) idum
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(IDC_INSERT_KOBJ,IDC_INSERT_AFTER)
      call GetInsertData(dlg)
      call SetInsertData(dlg)
    Case(IDCANCEL)
      kInsObj=-1
	    call DlgExit(dlg)
    Case(idok)
      call GetInsertData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateInsert

  Subroutine SpaceDialog(lCheck)
! dialog for the space data
    Implicit none
    Integer(4) idum
	  Include 'RESOURCE.FD'
    Logical, intent(in) :: lCheck
	  Logical ldum
	  Type(dialog) dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Space,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Space dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      ldum=DlgSet(Dlg,idc_Space_X0,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Y0,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Z0,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_X1,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Y1,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Z1,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_X2,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Y2,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Z2,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_X3,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Y3,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Space_Z3,.false.,DLG_ENABLE)
      if(iSpaceRead.gt.-1_2) then
        ldum=DlgSet(Dlg,idc_Space_X0,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Y0,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Z0,.true.,DLG_ENABLE)
      end if
      if(iSpaceRead.gt.0_2) then
        ldum=DlgSet(Dlg,idc_Space_X1,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Y1,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Z1,.true.,DLG_ENABLE)
      end if
      if(iSpaceRead.gt.1_2) then
        ldum=DlgSet(Dlg,idc_Space_X2,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Y2,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Z2,.true.,DLG_ENABLE)
      end if
      if(iSpaceRead.gt.2_2) then
        ldum=DlgSet(Dlg,idc_Space_X3,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Y3,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Space_Z3,.true.,DLG_ENABLE)
      end if
      SpaceAngle=0.0d0
      call SetSpaceData(dlg)
	    ldum=DlgSetSub(dlg,idcancel,updateSpace)
	    ldum=DlgSetSub(dlg,IDC_SPACE_XAXIS,updateSpace)
	    ldum=DlgSetSub(dlg,IDC_SPACE_YAXIS,updateSpace)
	    ldum=DlgSetSub(dlg,IDC_SPACE_ZAXIS,updateSpace)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine SpaceDialog

  Subroutine SetSpaceData(dlg)
! set the Space data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    call DlgSetS0(Dlg,idc_Space_Text,SpaceText)
    call DlgSetR(Dlg,idc_Space_X0,Space(1,0),5)
    call DlgSetR(Dlg,idc_Space_Y0,Space(2,0),5)
    call DlgSetR(Dlg,idc_Space_Z0,Space(3,0),5)
    call DlgSetR(Dlg,idc_Space_X1,Space(1,1),5)
    call DlgSetR(Dlg,idc_Space_Y1,Space(2,1),5)
    call DlgSetR(Dlg,idc_Space_Z1,Space(3,1),5)
    call DlgSetR(Dlg,idc_Space_X2,Space(1,2),5)
    call DlgSetR(Dlg,idc_Space_Y2,Space(2,2),5)
    call DlgSetR(Dlg,idc_Space_Z2,Space(3,2),5)
    call DlgSetR(Dlg,idc_Space_X3,Space(1,3),5)
    call DlgSetR(Dlg,idc_Space_Y3,Space(2,3),5)
    call DlgSetR(Dlg,idc_Space_Z3,Space(3,3),5)
    call DlgSetR(Dlg,IDC_SPACE_ANGLE,SpaceAngle,5)
  end Subroutine SetSpaceData

  Subroutine GetSpaceData(dlg)
! get the Space data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
	  call DlgGetR(Dlg,idc_Space_X0,Space(1,0),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Y0,Space(2,0),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Z0,Space(3,0),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_X1,Space(1,1),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Y1,Space(2,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Z1,Space(3,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_X2,Space(1,2),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Y2,Space(2,2),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Z2,Space(3,2),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_X3,Space(1,3),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Y3,Space(2,3),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Space_Z3,Space(3,3),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_SPACE_ANGLE,SpaceAngle,nBig,pBig,0.0d0,5)
  end Subroutine GetSpaceData

  Subroutine updateSpace(dlg,control_name,callbackType)
  ! callback for SpaceDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) idum,control_name,callbackType
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(IDC_SPACE_XAXIS)
      call GetSpaceData(dlg)
      Space=Rot3DSpaceX(Space,SpaceAngle)
      call SetSpaceData(dlg)
    Case(IDC_SPACE_YAXIS)
      call GetSpaceData(dlg)
      Space=Rot3DSpaceY(Space,SpaceAngle)
      call SetSpaceData(dlg)
    Case(IDC_SPACE_ZAXIS)
      call GetSpaceData(dlg)
      Space=Rot3DSpaceZ(Space,SpaceAngle)
      call SetSpaceData(dlg)
    Case(idcancel)
      call GetSpaceData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateSpace

  Subroutine Points2DDialog(lCheck)
! dialog for 2D points data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical, intent(in) :: lCheck
    Logical ldum
	  Type(dialog) dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Points2D,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Points2D dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      ldum=DlgSet(Dlg,idc_Points2D_XA,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points2D_YA,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points2D_XB,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points2D_YB,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points2D_XC,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points2D_YC,.false.,DLG_ENABLE)
      if(iPoints2DRead.gt.0_2) then
        ldum=DlgSet(Dlg,idc_Points2D_XA,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points2D_YA,.true.,DLG_ENABLE)
      end if
      if(iPoints2DRead.gt.1_2) then
        ldum=DlgSet(Dlg,idc_Points2D_XB,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points2D_YB,.true.,DLG_ENABLE)
      end if
      if(iPoints2DRead.gt.2_2) then
        ldum=DlgSet(Dlg,idc_Points2D_XC,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points2D_YC,.true.,DLG_ENABLE)
      end if
      call SetPoints2DData(dlg)
	    ldum=DlgSetSub(dlg,idcancel,updatePoints2D)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine Points2DDialog

  Subroutine SetPoints2DData(dlg)
! set the Points2D data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    call DlgSetS0(Dlg,idc_Points2D_Text,Points2DText)
    call DlgSetR(Dlg,idc_Points2D_XA,Points2D(1,1),5)
    call DlgSetR(Dlg,idc_Points2D_YA,Points2D(2,1),5)
    call DlgSetR(Dlg,idc_Points2D_XB,Points2D(1,2),5)
    call DlgSetR(Dlg,idc_Points2D_YB,Points2D(2,2),5)
    call DlgSetR(Dlg,idc_Points2D_XC,Points2D(1,3),5)
    call DlgSetR(Dlg,idc_Points2D_YC,Points2D(2,3),5)
  end Subroutine SetPoints2DData

  Subroutine GetPoints2DData(dlg)
! get the Points2D data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
	  call DlgGetR(Dlg,idc_Points2D_XA,Points2D(1,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points2D_YA,Points2D(2,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points2D_XB,Points2D(1,2),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Points2D_YB,Points2D(2,2),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points2D_XC,Points2D(1,3),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points2D_YC,Points2D(2,3),nBig,pBig,1.0d0,5)
  end Subroutine GetPoints2DData

  Subroutine updatePoints2D(dlg,control_name,callbackType)
  ! callback for Points2DDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) idum,control_name,callbackType
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idcancel)
      call GetPoints2DData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updatePoints2D

  Subroutine Points3DDialog(lCheck)
! dialog for 3D points data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical, intent(in) :: lCheck
    Logical ldum
	  Type(dialog) dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Points3D,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Points3D dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      ldum=DlgSet(Dlg,idc_Points3D_XA,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_YA,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_ZA,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_XB,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_YB,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_ZB,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_XC,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_YC,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Points3D_ZC,.false.,DLG_ENABLE)
      if(iPoints3DRead.gt.0_2) then
        ldum=DlgSet(Dlg,idc_Points3D_XA,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points3D_YA,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points3D_ZA,.true.,DLG_ENABLE)
      end if
      if(iPoints3DRead.gt.1_2) then
        ldum=DlgSet(Dlg,idc_Points3D_XB,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points3D_YB,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points3D_ZB,.true.,DLG_ENABLE)
      end if
      if(iPoints3DRead.gt.2_2) then
        ldum=DlgSet(Dlg,idc_Points3D_XC,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points3D_YC,.true.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Points3D_ZC,.true.,DLG_ENABLE)
      end if
      call SetPoints3DData(dlg)
	    ldum=DlgSetSub(dlg,idcancel,updatePoints3D)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine Points3DDialog

  Subroutine SetPoints3DData(dlg)
! set the Points3D data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    call DlgSetS0(Dlg,idc_Points3D_Text,Points3DText)
    call DlgSetR(Dlg,idc_Points3D_XA,Points3D(1,1),5)
    call DlgSetR(Dlg,idc_Points3D_YA,Points3D(2,1),5)
    call DlgSetR(Dlg,idc_Points3D_ZA,Points3D(3,1),5)
    call DlgSetR(Dlg,idc_Points3D_XB,Points3D(1,2),5)
    call DlgSetR(Dlg,idc_Points3D_YB,Points3D(2,2),5)
    call DlgSetR(Dlg,idc_Points3D_ZB,Points3D(3,2),5)
    call DlgSetR(Dlg,idc_Points3D_XC,Points3D(1,3),5)
    call DlgSetR(Dlg,idc_Points3D_YC,Points3D(2,3),5)
    call DlgSetR(Dlg,idc_Points3D_ZC,Points3D(3,3),5)
  end Subroutine SetPoints3DData

  Subroutine GetPoints3DData(dlg)
! get the Points3D data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
	  call DlgGetR(Dlg,idc_Points3D_XA,Points3D(1,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_YA,Points3D(2,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_ZA,Points3D(3,1),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_XB,Points3D(1,2),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_YB,Points3D(2,2),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_ZB,Points3D(3,2),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_XC,Points3D(1,3),nBig,pBig,0.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_YC,Points3D(2,3),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Points3D_ZC,Points3D(3,3),nBig,pBig,1.0d0,5)
  end Subroutine GetPoints3DData

  Subroutine updatePoints3D(dlg,control_name,callbackType)
  ! callback for Points3DDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) idum,control_name,callbackType
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idcancel)
      call GetPoints3DData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updatePoints3D

! auxiliary routines

	Subroutine Getnxy()
    Implicit none
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
  end	Subroutine Getnxy

	Subroutine Getijk(i,j,i1,j1,k1)
    Implicit none
    Integer(4) i,j,i1,j1,k1
    if(iPlane.eq.3) then
      i1=i
      j1=j
      k1=levPlane
    else if(iPlane.eq.2) then
      i1=i
      j1=levPlane
      k1=j
    else
      i1=levPlane
      j1=i
      k1=j
    end if
	end Subroutine Getijk

	Subroutine Getij(i,j,k,i1,j1)
    Implicit none
    Integer(4) i,j,k,i1,j1
    if(iPlane.eq.3) then
      i1=i
      j1=j
    else if(iPlane.eq.2) then
      i1=i
      j1=k
    else
      i1=j
      j1=k
    end if
	end Subroutine Getij

  Function getcGrd(i0,j0,k0) Result(r)
    Implicit none
    Real(8) r(3),d(3)
    Integer(4) i,j,k,i0,j0,k0,n,nx,ny,nz
    i=i0
    j=j0
    k=k0
    if(lrGrd) then
      r=spacecFld(1:3,0)
      r=r+dble(i-1)*spacecFld(1:3,1)/dble(Max(1,nxcFld-1))
      r=r+dble(j-1)*spacecFld(1:3,2)/dble(Max(1,nycFld-1))
      r=r+dble(k-1)*spacecFld(1:3,3)/dble(Max(1,nzcFld-1))
    else
      if((i0.gt.0).and.(i0.le.nxcFld).and.(j0.gt.0).and.(j0.le.nycFld).and.(k0.gt.0).and.(k0.le.nzcFld)) then
        r=rGrd(1:3,i0,j0,k0)
        return
      end if
      nx=max(1,nxcFld-1)
      ny=max(1,nycFld-1)
      nz=max(1,nzcFld-1)
      i=Modulo(i0-1,nx)+1
      j=Modulo(j0-1,ny)+1
      k=Modulo(k0-1,nz)+1
      r=rGrd(1:3,i,j,k)
      if((i.eq.i0).and.(j.eq.j0).and.(k.eq.k0)) return
      if(i.ne.i0) then
        if(nxcFld.gt.1) then
          d=rGrd(1:3,nxcFld,j,k)-rGrd(1:3,1,j,k)
        else
          d=1.0d0
        end if
        if(i0.gt.0) then
          n=(i0-1)/nx
        else
          n=(i0-nx)/nx
        end if
        r=r+dble(n)*d
      end if
      if(j.ne.j0) then
        if(nycFld.gt.1) then
          d=rGrd(1:3,i,nycFld,k)-rGrd(1:3,i,1,k)
        else
          d=1.0d0
        end if
        if(j0.gt.0) then
          n=(j0-1)/ny
        else
          n=(j0-ny)/ny
        end if
        r=r+dble(n)*d
      end if
      if(k.ne.k0) then
        if(nzcFld.gt.1) then
          d=rGrd(1:3,i,j,nzcFld)-rGrd(1:3,i,j,1)
        else
          d=1.0d0
        end if
        if(k0.gt.0) then
          n=(k0-1)/nz
        else
          n=(k0-nz)/nz
        end if
        r=r+dble(n)*d
      end if
    end if
  end Function getcGrd

  Function getrGrd(i0,j0) Result(r)
    Implicit none
    Real(8) r(3)
    Integer(4) i,j,k,i0,j0
    call Getijk(i0,j0,i,j,k)
    r=GetcGrd(i,j,k)
  end Function getrGrd

  Subroutine getNextrGrd(r,i0min,j0min)
    Implicit none
    Real(8) r(3),rg(3),dr(3),d,dmin
    Integer(4) i,j,k,i0,j0,i0min,j0min
    dmin=pBig
    i0min=1
    j0min=1
    do i0=1,nxrFld
      do j0=1,nyrFld
        call Getijk(i0,j0,i,j,k)
        rg=GetcGrd(i,j,k)
        dr=rg-r
        d=r3Vec_Length(dr)
        if(d.lt.dmin) then
          dmin=d
          i0min=i0
          j0min=j0
        end if
      end do
    end do
  end Subroutine getNextrGrd

  Integer(2) Function igetiCS(k)
! get iCS value of expansion k
    Implicit none
    Integer(4), Intent(in):: k
    igetiCS=tExp(k)%iHE/10_2
  end Function igetiCS

  Integer(2) Function igetiHE(k)
! get iHE value of expansion k
    Implicit none
    Integer(4), Intent(in):: k
    Integer(2) iCS
    iCS=tExp(k)%iHE/10_2
    igetiHE=tExp(k)%iHE-10_2*iCS
  end Function igetiHE

  Integer(2) Function igetiHE2(iHE1,k)
! merge iHE1 value with iHE of expansion k
    Implicit none
    Integer(4), Intent(in):: k
    Integer(2) iHE1,iHE2
    iHE2=igetiHE(k)
    igetiHE2=Min(iHE1,iHE2)
    if((iHE1.eq.iHE2).or.(iHE1.eq.2_2).or.(iHE2.eq.2_2)) return
    igetiHE2=-1_2
  end Function igetiHE2

  Subroutine getEUST(kD,r,lFo)
! get values epsilon, mue, sigma, tau of all domains
    Implicit none
    !Complex(8) cAux(nDom+64)
    Complex(8) cAux(mForA)
    Integer(4), Optional :: kD
    Integer(4) k,lf,iErr,k1,k2
    Logical lr
    Real(8) aux(6)
    Real(8), Optional:: r(3)
    Logical, Optional:: lFo ! indicates whether a formula was used for the evaluation
    lInitSurfWaves=.true.
    lr=.false.
    if(Present(r)) lr=.true.
    if(Present(lFo)) lFo=.false.
    cCForm(0)=(0.0d0,1.0d0)
    cCForm(1)=DCmplx(Pi,0.0d0)
    cCForm(2)=DCmplx(Eps0,0.0d0)
    cCForm(3)=DCmplx(Mue0,0.0d0)
    cCForm(4)=DCmplx(Kw0,0.0d0)
    cCForm(5)=DCmplx(Zw0,0.0d0)
    pCForm(0)=(1.0d0,0.0d0)
    vCForm(0)=DCmplx(trFld,0.0d0)
    vCForm(1)=fcFld
    vCForm(2)=2.0d0*Pi*fcFld
    if(lr) then
      vCForm(3:5)=DCmplx(r(1:3),0.0d0)
    else
      vCForm(3:5)=DCmplx(0.0d0,0.0d0)
    end if
    if(Present(kD)) then
      kD=max(min(kD,nDom),1_4)
      k1=kD
      k2=kD
    else
      k1=1
      k2=nDom
    end if
    do k=k1,k2
      aDom(1:6,k:k)=0.0d0
      bDom(1:6,k:k)=0.0d0
      idDom(1,k)=1_2
      idDom(2,k)=1_2
      lf=-1
      call DelBlanks(Dom_Form(1,k),lf)
      call ToLower(Dom_Form(1,k),lf)
      if(Dom_Form(1,k)(1:1).eq.'#') then
        call FindInFile(Dom_Form(1,k),lf,Dble(fcFld),eDom(k),iErr)
      else
        !eDom(k:k)=cFormula(Dom_Form(1,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
        cAux(k:k)=cFormula(Dom_Form(1,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
        eDom(k)=cAux(k)
        if((Dom_Form(1,k)(1:3).eq.'lmf').or.(Dom_Form(1,k)(1:3).eq.'lmo').or.(Dom_Form(1,k)(1:3).eq.'lmw')) then
          call getABdom(Dom_Form(1,k),lf,aux,3)
          aDom(1:2,k)=aux(2:3)
          idDom(1,k)=2_2
        end if
        if((Dom_Form(1,k)(1:3).eq.'drf').or.(Dom_Form(1,k)(1:3).eq.'dro').or.(Dom_Form(1,k)(1:3).eq.'drw')) then
          call getABdom(Dom_Form(1,k),lf,aDom(1:3,k:k),3)
     !     aDom(3,k)=.inv.aDom(3,k)
          idDom(1,k)=3_2
        end if
        if((Dom_Form(1,k)(1:3).eq.'dsf').or.(Dom_Form(1,k)(1:3).eq.'dso').or.(Dom_Form(1,k)(1:3).eq.'dsw')) then
          call getABdom(Dom_Form(1,k),lf,aux,5)
          aDom(1:4,k)=aux(2:5)
     !     aDom(3,k)=.inv.aDom(3,k)
          idDom(1,k)=5_2
        end if
        if((Dom_Form(1,k)(1:3).eq.'dlf').or.(Dom_Form(1,k)(1:3).eq.'dlo').or.(Dom_Form(1,k)(1:3).eq.'dlw')) then
          call getABdom(Dom_Form(1,k),lf,aDom(1:6,k:k),6)
     !     aDom(3,k)=.inv.aDom(3,k)
          idDom(1,k)=6_2
          if(dabs(aDom(2,k)).lt.pSmall) then
            idDom(1,k)=4_2
            aDom(2,k)=aDom(4,k)
            aDom(3,k)=aDom(5,k)
            aDom(4,k)=aDom(6,k)
          end if
        end if
      end if
      if(idDom(1,k).lt.2_2) then ! no material formula defined
        if(Present(lFo).and.(iErr.eq.0).and.(lf.gt.3)) then
          if(Dom_Form(1,k)(4:4).eq.'(') lFo=.true.
        end if
        if((iErr.ne.0).or.(Dom_Form(1,k)(1:1).eq.'!')) eDom(k)=(1.0d0,0.0d0)
        lf=-1
        call DelBlanks(Dom_Form(3,k),lf)
        call ToLower(Dom_Form(3,k),lf)
        if(Dom_Form(3,k)(1:1).eq.'#') then
          call FindInFile(Dom_Form(3,k),lf,Dble(fcFld),sDom(k),iErr)
        else
          !sDom(k:k)=cFormula(Dom_Form(3,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
          cAux(k:k)=cFormula(Dom_Form(1,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
          sDom(k)=cAux(k)
        end if
        if(Present(lFo).and.(iErr.eq.0).and.(lf.gt.3)) then
          if(Dom_Form(1,k)(4:4).eq.'(') lFo=.true.
        end if
        if((iErr.ne.0).or.(Dom_Form(3,k)(1:1).eq.'!')) sDom(k)=(0.0d0,0.0d0)
        aDom(1,k)=dble(eDom(k))
        if(dabs(dble(sDom(k))).gt.pSmall) then
          idDom(1,k)=2_2
          aDom(2,k)=dble(sDom(k))
        end if
      end if
      lf=-1
      call DelBlanks(Dom_Form(2,k),lf)
      call ToLower(Dom_Form(2,k),lf)
      if(Dom_Form(2,k)(1:1).eq.'#') then
        call FindInFile(Dom_Form(2,k),lf,Dble(fcFld),uDom(k),iErr)
      else
        !uDom(k:k)=cFormula(Dom_Form(2,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
          cAux(k:k)=cFormula(Dom_Form(1,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
          uDom(k)=cAux(k)
        if((Dom_Form(2,k)(1:3).eq.'lmf').or.(Dom_Form(2,k)(1:3).eq.'lmo').or.(Dom_Form(2,k)(1:3).eq.'lmw')) then
          call getABdom(Dom_Form(2,k),lf,aux,3)
          bDom(1:2,k)=aux(2:3)
          idDom(2,k)=2_2
        end if
        if((Dom_Form(2,k)(1:3).eq.'drf').or.(Dom_Form(2,k)(1:3).eq.'dro').or.(Dom_Form(2,k)(1:3).eq.'drw')) then
          call getABdom(Dom_Form(2,k),lf,bDom(1:3,k:k),3)
    !      bDom(3,k)=.inv.bDom(3,k)
          idDom(2,k)=3_2
        end if
        if((Dom_Form(2,k)(1:3).eq.'dsf').or.(Dom_Form(2,k)(1:3).eq.'dso').or.(Dom_Form(2,k)(1:3).eq.'dsw')) then
          call getABdom(Dom_Form(2,k),lf,aux,5)
          bDom(1:4,k)=aux(2:5)
    !      bDom(3,k)=.inv.bDom(3,k)
          idDom(2,k)=5_2
        end if
        if((Dom_Form(2,k)(1:3).eq.'dlf').or.(Dom_Form(2,k)(1:3).eq.'dlo').or.(Dom_Form(2,k)(1:3).eq.'dlw')) then
          call getABdom(Dom_Form(2,k),lf,bDom(1:6,k:k),6)
          idDom(2,k)=6_2
    !      bDom(3,k)=.inv.bDom(3,k)
          if(dabs(bDom(2,k)).lt.pSmall) then
            idDom(2,k)=4_2
            bDom(2,k)=bDom(4,k)
            bDom(3,k)=bDom(5,k)
            bDom(4,k)=bDom(6,k)
          end if
        end if
      end if
      if(idDom(2,k).lt.2_2) then ! no material formula defined
        if(Present(lFo).and.(iErr.eq.0).and.(lf.gt.3)) then
          if(Dom_Form(1,k)(4:4).eq.'(') lFo=.true.
        end if
        if((iErr.ne.0).or.(Dom_Form(2,k)(1:1).eq.'!')) uDom(k)=(1.0d0,0.0d0)
        lf=-1
        call DelBlanks(Dom_Form(4,k),lf)
        call ToLower(Dom_Form(4,k),lf)
        if(Dom_Form(4,k)(1:1).eq.'#') then
          call FindInFile(Dom_Form(4,k),lf,Dble(fcFld),tDom(k),iErr)
        else
          !tDom(k:k)=cFormula(Dom_Form(4,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
          cAux(k:k)=cFormula(Dom_Form(1,k),lf,cCForm,pCForm,vCForm,5,0,5,1,1,1,iErr)
          tDom(k)=cAux(k)
        end if
        if(Present(lFo).and.(iErr.eq.0).and.(lf.gt.3)) then
          if(Dom_Form(1,k)(4:4).eq.'(') lFo=.true.
        end if
        if((iErr.ne.0).or.(Dom_Form(4,k)(1:1).eq.'!')) tDom(k)=(0.0d0,0.0d0)
        bDom(1,k)=dble(uDom(k))
        if(dabs(dble(tDom(k))).gt.pSmall) then
          idDom(2,k)=2_2
          bDom(2,k)=dble(tDom(k))
        end if
      end if
    end do
    call setupPeriod()
  end Subroutine getEUST

  Subroutine FindInFile(File,lf,f,c,iErr)
    Implicit none
    Integer(4) lf,iErr,ios,idum
    Real(8) f,r0,s0,f1,r1,s1,f2,r2,s2,c0,scal
    Complex(8) c
    Character(1151) File
    open(1,file=File(2:lf)//char(0),status='old',Action='Read',iostat=iErr)
    if(iErr.ne.0) then
      idum=MessageBoxQQ('Cannot open material file '//File(2:lf)//'!\rStop OpenMaXwell?'C,'Find material in file'C, &
                        MB$YESNO.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDYES) stop
      iErr=-1
      return
    end if
    iErr=1
    read(1,*,IOstat=ios) scal
    if(ios.ne.0) then
      close(1)
      return
    end if
    iErr=2
    if(lf.lt.5) then
      close(1)
      return
    end if
    iErr=3
    if(File(lf-3:lf-3).ne.'.') then
      close(1)
      return
    end if
    iErr=4
    c0=1.0d0/dsqrt(Eps0*Mue0)
    Select case(File(lf-2:lf))
    Case('fr2')
      read(1,*,IOstat=ios) f1,r1,s1
      if(ios.ne.0) then
        close(1)
        return
      end if
      f1=f1*scal
      if(f1.gt.f) then
        c=DCmplx(r1,s1)
        iErr=0
        close(1)
        return
      else
        do
          read(1,*,IOstat=ios) f2,r2,s2
          if(ios.ne.0) then ! not found, error reading -> use last values
            c=DCmplx(r1,s1)
            iErr=0
            close(1)
            return
          end if
          f2=f2*scal
          if(f2.gt.f) then ! found -> interpolate
            r0=r1+(f-f1)*(r2-r1)/(f2-f1)
            s0=s1+(f-f1)*(s2-s1)/(f2-f1)
            c=DCmplx(r0,s0)
            iErr=0
            close(1)
            return
          end if
          f1=f2
          r1=r2
          s1=s2
        end do
      end if
    Case('fr1')
      read(1,*,IOstat=ios) f1,r1
      if(ios.ne.0) then
        close(1)
        return
      end if
      f1=f1*scal
      if(f1.gt.f) then
        c=DCmplx(r1,0.0d0)
        iErr=0
        close(1)
        return
      else
        do
          read(1,*,IOstat=ios) f2,r2
          f2=f2*scal
          if(ios.ne.0) then ! not found, error reading -> use last values
            c=DCmplx(r1,0.0d0)
            iErr=0
            close(1)
            return
          end if
          if(f2.gt.f) then ! found -> interpolate
            r0=r1+(f-f1)*(r2-r1)/(f2-f1)
            c=DCmplx(r1,0.0d0)
            iErr=0
            close(1)
            return
          end if
          f1=f2
          r1=r2
        end do
      end if
    Case('om2')
      read(1,*,IOstat=ios) f1,r1,s1
      if(ios.ne.0) then
        close(1)
        return
      end if
      f1=f1*scal/(2.0d0*Pi)
      if(f1.gt.f) then
        c=DCmplx(r1,s1)
        iErr=0
        close(1)
        return
      else
        do
          read(1,*,IOstat=ios) f2,r2,s2
          if(ios.ne.0) then ! not found, error reading -> use last values
            c=DCmplx(r1,s1)
            iErr=0
            close(1)
            return
          end if
          f2=f2*scal/(2.0d0*Pi)
          if(f2.gt.f) then ! found -> interpolate
            r0=r1+(f-f1)*(r2-r1)/(f2-f1)
            s0=s1+(f-f1)*(s2-s1)/(f2-f1)
            c=DCmplx(r0,s0)
            iErr=0
            close(1)
            return
          end if
          f1=f2
          r1=r2
          s1=s2
        end do
      end if
    Case('om1')
      read(1,*,IOstat=ios) f1,r1
      if(ios.ne.0) then
        close(1)
        return
      end if
      f1=f1*scal/(2.0d0*Pi)
      if(f1.gt.f) then
        c=DCmplx(r1,0.0d0)
        iErr=0
        close(1)
        return
      else
        do
          read(1,*,IOstat=ios) f2,r2
          if(ios.ne.0) then ! not found, error reading -> use last values
            c=DCmplx(r1,0.0d0)
            iErr=0
            close(1)
            return
          end if
          f2=f2*scal/(2.0d0*Pi)
          if(f2.gt.f) then ! found -> interpolate
            r0=r1+(f-f1)*(r2-r1)/(f2-f1)
            c=DCmplx(r1,0.0d0)
            iErr=0
            close(1)
            return
          end if
          f1=f2
          r1=r2
        end do
      end if
    Case('wl2')
      read(1,*,IOstat=ios) f1,r1,s1
      if(ios.ne.0) then
        close(1)
        return
      end if
      f1=c0/(f1*scal)
      if(f1.lt.f) then
        c=DCmplx(r1,s1)
        iErr=0
        close(1)
        return
      else
        do
          read(1,*,IOstat=ios) f2,r2,s2
          if(ios.ne.0) then ! not found, error reading -> use last values
            c=DCmplx(r1,s1)
            iErr=0
            close(1)
            return
          end if
          f2=c0/(f2*scal)
          if(f2.lt.f) then ! found -> interpolate
            r0=r1+(f-f1)*(r2-r1)/(f2-f1)
            s0=s1+(f-f1)*(s2-s1)/(f2-f1)
            c=DCmplx(r0,s0)
            iErr=0
            close(1)
            return
          end if
          f1=f2
          r1=r2
          s1=s2
        end do
      end if
    Case('wl1')
      read(1,*,IOstat=ios) f1,r1
      if(ios.ne.0) then
        close(1)
        return
      end if
      f1=c0/(f1*scal)
      if(f1.lt.f) then
        c=DCmplx(r1,0.0d0)
        iErr=0
        close(1)
        return
      else
        do
          read(1,*,IOstat=ios) f2,r2
          if(ios.ne.0) then ! not found, error reading -> use last values
            c=DCmplx(r1,0.0d0)
            iErr=0
            close(1)
            return
          end if
          f2=c0/(f2*scal)
          if(f2.lt.f) then ! found -> interpolate
            r0=r1+(f-f1)*(r2-r1)/(f2-f1)
            c=DCmplx(r0,0.0d0)
            iErr=0
            close(1)
            return
          end if
          f1=f2
          r1=r2
        end do
      end if
    end Select
    close(1)
  end Subroutine FindInFile

  Subroutine setupPeriod()
! setup constants CxPeriod, CyPeriod and CzPeriod for periodic problems
    Implicit none
    Integer(2) lf
    Real(8) v2(2)
    Complex(8) ckz,ckap2,kapcFld
    call getExcitation()
    if(nExcit.lt.1) return
    if(tExp(nExcit)%iTypE.eq.3_2) then
      lf=Max(1_2,Min(Int2(nDom),tExp(nExcit)%iDom))
      kcFld=Wnumber(lf)
      if(lgcFld) then
        if(lzPer) then
          ckz=Pi/dcFld
        else
          ckz=(2.0d0*Pi*kw0)*fcFld*gcFld
        end if
        ckap2=kcFld**2-ckz**2
        kapcFld=cdsqrt(ckap2)
        if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
        v2(1)=dcosd(tExp(nExcit)%rE(1))
        v2(2)=dsind(tExp(nExcit)%rE(1))
        if(lxPeriod) CxPeriod=kapcFld*v2(1)
        if(lyPeriod) CyPeriod=kapcFld*Dot_Product(yPeriodVector(1:2),v2)/r2Vec_Length(yPeriodVector(1:2))
      else
        if(lxPeriod) CxPeriod=kcFld*tExp(nExcit)%Plane(1,3)
        if(lyPeriod) CyPeriod=kcFld*Dot_Product(yPeriodVector,tExp(nExcit)%Plane(1:3,3))/r3Vec_Length(yPeriodVector)
        if(lzPeriod) CzPeriod=kcFld*Dot_Product(zPeriodVector,tExp(nExcit)%Plane(1:3,3))/r3Vec_Length(zPeriodVector)
      end if
      CxPeriodDlg=CxPeriod
      CyPeriodDlg=CyPeriod
      CzPeriodDlg=CzPeriod
    else
      if(lxPeriod) CxPeriod=CxPeriodDlg
      if(lyPeriod) CyPeriod=CyPeriodDlg
      if(lzPeriod) CzPeriod=CzPeriodDlg
    end if
  end Subroutine setupPeriod

  Subroutine getExcitation()
! get expansion number nE of the current excitation
    Implicit none
    Integer(4) kE,idum,nP,nR,nE,nExci
    nExcit=0
    do kE=nExp,1,-1
      if(LSkipExp(kE)) Cycle
      nExcit=kE
      Exit
    end do
    if(nExcit.lt.1) idum=MessageBoxQQ('Cannot find excitation!'C,'get excitation'C, &
                         MB$OK.or.MB$IconExclamation)
    nExc=1 ! count number of expansions used as excitations
    nE=nExcit
    nP=tExp(nE)%nPar
    nR=nRHS
    do while(nR.gt.nP) 
      nE=nE-1
      if(nE.lt.1) idum=MessageBoxQQ('Cannot find enough excitations!'C,'get excitation'C, &
                           MB$OK.or.MB$IconExclamation)
      nExc=nExc+1
      nR=nR-nP
      nP=tExp(nE)%nPar
      nExci=0
      do kE=nE,1,-1
        if(LSkipExp(kE)) Cycle
        nExci=kE
        Exit
      end do
    end do
  end Subroutine getExcitation

  Logical Function LSkipExp(k)
    Implicit none
    Integer(4) k
    LSkipExp=.false.
    if(iDomExp.ne.0_2) then
      if(iDomExp.gt.0_2) then
        if((iDomExp.ne.tExp(k)%iDom).and.(0_2.ne.tExp(k)%iDom).and.(iDomExp.gt.-tExp(k)%iDom)) then
          LSkipExp=.true.
          return
        end if
      else if(iDomExp.lt.0_2) then
        if((tExp(k)%iDom.gt.0_2).and.(-iDomExp.lt.tExp(k)%iDom)) then
          LSkipExp=.true.
          return
        end if
      end if
    end if
    if((iColExp.ne.0_2).and.(tExp(k)%iCol.ne.0_2)) then
      if(iColExp.gt.0_2) then
        if((iColExp.ne.tExp(k)%iCol).and.(iColExp.gt.-tExp(k)%iCol)) then
          LSkipExp=.true.
          return
        end if
      else if(iColExp.lt.0_2) then
        if((tExp(k)%iCol.gt.0_2).and.(-iColExp.lt.tExp(k)%iCol)) then
          LSkipExp=.true.
          return
        end if
      end if
    end if
    if((iConExp.ne.0_2).and.(tExp(k)%iConn.ne.0_2)) then
      if(iConExp.gt.0_2) then
       if((iConExp.ne.tExp(k)%iConn).and.(iConExp.gt.-tExp(k)%iConn)) then
          LSkipExp=.true.
          return
        end if
      else if(iConExp.lt.0_2) then
       if((tExp(k)%iConn.gt.0_2).and.(-iConExp.lt.tExp(k)%iConn)) then
          LSkipExp=.true.
          return
        end if
      end if
    end if
  end Function LSkipExp

  Logical Function LSkipBnd(k)
    Implicit none
    Integer(4) k
    LSkipBnd=.false.
    if(iDomBnd.ne.0_2) then
      if(iDomBnd.gt.0_2) then
        if((iDomBnd.ne.tBnd(k)%iLDom).and.(iDomBnd.ne.tBnd(k)%iRDom)) LSkipBnd=.true.
      else if(iDomBnd.lt.0_2) then
        if((tBnd(k)%iLDom.gt.0_2).and.(tBnd(k)%iRDom.gt.0_2).and. &
        &  (-iDomBnd.lt.tBnd(k)%iLDom).and.(-iDomBnd.lt.tBnd(k)%iRDom)) LSkipBnd=.true.
      end if
    end if
    if((iColBnd.ne.0_2).and.(tBnd(k)%iCol.ne.0_2)) then
      if(iColBnd.gt.0_2) then
        if((iColBnd.ne.tBnd(k)%iCol).and.(iColBnd.gt.-tBnd(k)%iCol)) LSkipBnd=.true.
      else if(iColBnd.lt.0_2) then
        if((tBnd(k)%iCol.gt.0_2).and.(-iColBnd.lt.tBnd(k)%iCol)) LSkipBnd=.true.
      end if
    end if
    if((iConBnd.ne.0_2).and.(tBnd(k)%iConn.ne.0_2)) then
      if(iConBnd.gt.0_2) then
        if((iConBnd.ne.tBnd(k)%iConn).and.(iConBnd.gt.-tBnd(k)%iConn)) LSkipBnd=.true.
      else if(iConBnd.lt.0_2) then
        if((tBnd(k)%iConn.gt.0_2).and.(-iConBnd.lt.tBnd(k)%iConn)) LSkipBnd=.true.
      end if
    end if
  end Function LSkipBnd

  Logical Function LSkipPFD(iD)
    Implicit none
    Integer(2) iD
    LSkipPFD=.false.
    if(iD.eq.tExp(nExp)%iDom) return
    if(tExp(nExp)%iDom.eq.0) return
    LSkipPFD=.true.
  end Function LSkipPFD

  Subroutine GetEz(ez)
    Implicit None
    Real(8) ez(3)
    if(iPlane.eq.3) then
      ez=spacecFld(1:3,3)
    else if(iPlane.eq.2) then
      ez=spacecFld(1:3,2)
    else
      ez=spacecFld(1:3,1)
    end if
  end Subroutine GetEz

  Integer(2) Function iGetObjNr(kP)
! get object number of the 3D matching point kP if kP>0
! get object number of the 3D web point kP if kP<=0
    Implicit none
    Integer(4) kP,kOb
    if(kP.gt.0) then
      do kOb=1,nObj 
        if(tOBJ(kOb)%nMat.lt.1) Cycle
        if((kP.gt.tOBJ(kOb)%iMatOffset).and.(kP.le.(tOBJ(kOb)%iMatOffset+tOBJ(kOb)%nMat))) Exit
      end do
    else
      do kOb=1,nObj ! get object number
        if((tOBJ(kOb)%nGrf.lt.1).or.(tOBJ(kOb)%mGrf.lt.1)) Cycle
        if((-kP.gt.tOBJ(kOb)%iGrf).and.(-kP.le.(tOBJ(kOb)%iGrf+tOBJ(kOb)%nGrf*tOBJ(kOb)%mGrf))) Exit
      end do
    end if
    iGetObjNr=Int2(kOb)
  end Function iGetObjNr

! I/O

Subroutine SaveCHGLwindow(lCheck)
! save the data of the OpenGL window in a file
    Implicit none
  Integer(4) iOK,ios,idum
  Logical, intent(in) :: lCheck
  if(.not.lCheck) then
    call Open2write(-1,'Select OpenGL data file to be written!','OpenGL data file ',OGLFileName,'OGL',ios)
    if(ios.gt.0) return
  end if
  open(1,file=OGLFileName,iostat=ios)
  if(ios.ne.0) then
    idum=MessageBoxQQ('Error opening file!\rCannot save data!'C,'Save OpenGL data'C, &
                      MB$OK.or.MB$IconExclamation)
    close(1)
    return
  end if
  call WriteStr(1,CHOGLIdent,iOK)
  ich(1)=iCHGLxAxisColor
  ich(2)=iCHGLyAxisColor
  ich(3)=iCHGLzAxisColor
  call chwrit2(1,ich,3,rch,0,sch,0,iOK)
  rch(1)=CHGLxAxisW
  rch(2)=CHGLyAxisW
  rch(3)=CHGLzAxisW
  call chwrit2(1,ich,0,rch,3,sch,0,iOK)
  rch(1)=CHGLxAxisL
  rch(2)=CHGLyAxisL
  rch(3)=CHGLzAxisL
  call chwrit2(1,ich,0,rch,3,sch,0,iOK)
  ich(1)=iCHGLxGridColor
  ich(2)=iCHGLyGridColor
  ich(3)=iCHGLzGridColor
  call chwrit2(1,ich,3,rch,0,sch,0,iOK)
  rch(1)=CHGLxGridLineW
  rch(2)=CHGLyGridLineW
  rch(3)=CHGLzGridLineW
  call chwrit2(1,ich,0,rch,3,sch,0,iOK)
  rch(1)=CHGLExpLen
  call chwrit2(1,ich,0,rch,1,sch,0,iOK)
  ich(1)=iCHGLMovie
  rch(1)=rCHGLMovie
  call chwrit2(1,ich,1,rch,1,sch,0,iOK)
  ich(1)=iCHGLwidth
  ich(2)=iCHGLheight
  call chwrit2(1,ich,2,rch,0,sch,0,iOK)
  rch(1:3)=CHGLlookFrom(1:3)
  call chwrit2(1,ich,0,rch,3,sch,0,iOK)
  rch(1:3)=CHGLlookAt(1:3)
  call chwrit2(1,ich,0,rch,3,sch,0,iOK)
  rch(1)=CHGLViewAngle
  rch(2)=CHGLViewAspect
  rch(3)=CHGLViewNear
  rch(4)=CHGLViewFar
  call chwrit2(1,ich,0,rch,4,sch,0,iOK)
  ich(1)=nCHGLTube
  sch(1:6)=' Tubes'
  call chwrit2(1,ich,1,rch,0,sch,6,iOK)
  do idum=1,nCHGLTube
    rch(1:3)=CHGLTubeStart(1:3,idum)
    rch(4)=CHGLTubeR(idum)
    rch(5)=CHGLTubeD(idum)
    call chwrit2(1,ich,0,rch,5,sch,0,iOK)
    ich(1)=iCHGLTubeR(idum)
    ich(2)=iCHGLTubeD(idum)
    ich(3:5)=iCHGLTubeColor(1:3,idum)
    call chwrit2(1,ich,5,rch,0,sch,0,iOK)
  end do
  sch(1:1)=' '
  call chwrit2(1,ich,0,rch,0,sch,1,iOK)
  EndFile(1)
  close(1)
end Subroutine SaveCHGLwindow

Subroutine OpenCHGLwindow(lCheck)
! read the window data from a file
    Implicit none
  Integer(4) ier,ios,idum,iVers,iOK
  Logical, intent(in) :: lCheck
  Logical lFileExist
  Character(20) text
  if(.not.lCheck) then
    call Open2read(-1,'Select OpenGL data file to be read!','OpenGL data file ',OGLFileName,'OGL',ios)
    if(ios.gt.0) return
  end if
  inquire(file=OGLFileName,Exist=lFileExist)
  if(.not.lFileExist) return
  open(1,file=OGLFileName,status='old',iostat=ios)
  if(ios.ne.0) then
    if(.not.lCheck) idum=MessageBoxQQ('Error opening file!\rCannot read data!'C,'Open OpenGL data'C, &
                    MB$OK.or.MB$IconExclamation)
    close(1)
    return
  end if
  call ReadStr(1,text,ier)
  if(ier.ne.0) goto 999
  if(CHOGLIdent(1:17).ne.text(1:17)) then
    idum=MessageBoxQQ('Wrong file Type!\rContinue reading?'C,'Open OpenGL data'C, &
                      MB$YesNo.or.MB$IconQuestion)
    if(idum.eq.MB$IDNO) then
      close(1)
      return
    end if
  end if
  call StrToInt(text(18:18)//text(18:18),iVers,iOK)
  call chread2(1,ich,3,rch,0,ier)
  if(ier.ne.0) goto 999
  iCHGLxAxisColor=ich(1)
  iCHGLyAxisColor=ich(2)
  iCHGLzAxisColor=ich(3)
  call chread2(1,ich,0,rch,3,ier)
  if(ier.ne.0) goto 999
  CHGLxAxisW=rch(1)
  CHGLyAxisW=rch(2)
  CHGLzAxisW=rch(3)
  call chread2(1,ich,0,rch,3,ier)
  if(ier.ne.0) goto 999
  CHGLxAxisL=rch(1)
  CHGLyAxisL=rch(2)
  CHGLzAxisL=rch(3)
  call chread2(1,ich,3,rch,0,ier)
  if(ier.ne.0) goto 999
  iCHGLxGridColor=ich(1)
  iCHGLyGridColor=ich(2)
  iCHGLzGridColor=ich(3)
  call chread2(1,ich,0,rch,3,ier)
  if(ier.ne.0) goto 999
  CHGLxGridLineW=rch(1)
  CHGLyGridLineW=rch(2)
  CHGLzGridLineW=rch(3)
  call chread2(1,ich,0,rch,1,ier)
  if(ier.ne.0) goto 999
  CHGLExpLen=rch(1)
  call chread2(1,ich,1,rch,1,ier)
  if(ier.ne.0) goto 999
  iCHGLMovie=ich(1)
  rCHGLMovie=rch(1)
  call chread2(1,ich,2,rch,0,ier)
  if(ier.eq.0) then
    iCHGLwidth=max(64_2,ich(1))
    iCHGLheight=max(64_2,ich(2))
  end if
  call chread2(1,ich,0,rch,3,ier)
  if(ier.eq.0) CHGLlookFrom(1:3)=rch(1:3)
  call chread2(1,ich,0,rch,3,ier)
  if(ier.eq.0) CHGLlookAt(1:3)=rch(1:3)
  call chread2(1,ich,0,rch,4,ier)
  if(ier.eq.0) then
    CHGLViewAngle=max(1.0d-30,min(180.0d0,rch(1)))
    CHGLViewAspect=max(0.1d0,min(10.0d0,rch(2)))
    CHGLViewNear=max(1.0d-30,min(1.0d30,rch(3)))
    CHGLViewFar=max(CHGLViewNear,min(1.0d30,rch(4)))
  end if
! Field tubes
  call chread2(1,ich,1,rch,0,ier)
  if(ier.eq.0) then
    nCHGLTube=ich(1)
    DeAllocate(CHGLTubeStart,CHGLTubeR,CHGLTubeD,iCHGLTubeR,iCHGLTubeD,iCHGLTubeColor,Stat=ier)
    Allocate(CHGLTubeStart(3,nCHGLTube),CHGLTubeR(nCHGLTube),CHGLTubeD(nCHGLTube),iCHGLTubeR(nCHGLTube), &
    & iCHGLTubeD(nCHGLTube),iCHGLTubeColor(3,nCHGLTube),Stat=ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Memory allocation error!'C,'Open OpenGL data'C, &
                        MB$OK.or.MB$ICONSTOP)
    end if
    do idum=1,nCHGLTube
      call chread2(1,ich,0,rch,5,ier)
      if(ier.ne.0) goto 999
      CHGLTubeStart(1:3,idum)=rch(1:3)
      CHGLTubeR(idum)=rch(4)
      CHGLTubeD(idum)=rch(5)
      if(iVers.gt.0) then
        call chread2(1,ich,5,rch,0,ier)
        if(ier.ne.0) goto 999
      else
        call chread2(1,ich,3,rch,0,ier)
        if(ier.ne.0) goto 999
        ich(4)=224_2
        ich(5)=232_2
      end if
      iCHGLTubeR(idum)=ich(1)
      iCHGLTubeD(idum)=ich(2)
      iCHGLTubeColor(1:3,idum)=ich(3:5)
    end do
  else
    nCHGLTube=1
    DeAllocate(CHGLTubeStart,CHGLTubeR,CHGLTubeD,iCHGLTubeR,iCHGLTubeD,iCHGLTubeColor,Stat=ier)
    Allocate(CHGLTubeStart(3,nCHGLTube),CHGLTubeR(nCHGLTube),CHGLTubeD(nCHGLTube),iCHGLTubeR(nCHGLTube), &
    & iCHGLTubeD(nCHGLTube),iCHGLTubeColor(3,nCHGLTube),Stat=ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Memory allocation error!'C,'Open OpenGL data'C, &
                        MB$OK.or.MB$ICONSTOP)
    end if
    CHGLTubeStart(1:3,nCHGLTube)=0.0d0
    CHGLTubeR(nCHGLTube)=0.0d0
    CHGLTubeD(nCHGLTube)=0.0d0
    iCHGLTubeR(nCHGLTube)=0
    iCHGLTubeD(nCHGLTube)=0
    iCHGLTubeColor(1:3,nCHGLTube)=0
  end if
  close(1)
  return
999 continue
  idum=MessageBoxQQ('Error reading input file!'C,'Open OpenGL data'C, &
                    MB$OK.or.MB$ICONSTOP)
  close(1)
end Subroutine OpenCHGLwindow

END MODULE CHBDE




