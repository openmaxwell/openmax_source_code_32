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
MODULE CHDIB

! Objects

  USE CHDIC

  SAVE

  CONTAINS

  Subroutine MFieldDialog(lCheck)
! dialog for the Mfield data (Modify field point)
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical, intent(in) :: lCheck
    Logical ldum
	  Type(dialog) dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_MFIELD,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Field point dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      lfcFld=lfcFldAll
      call SetMFieldData(dlg)
	    ldum=DlgSetSub(dlg,idok,updateMField)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine MFieldDialog

  Subroutine SetMFieldData(dlg)
! set the Mfield data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) nx,ny,nz
    Character(32) text
	  Type(dialog) dlg
    Text='UNDEFINED'C
    nx=jxcFld
    ny=jycFld
    nz=jzcFld
    call DlgSetI(Dlg,idc_MField_NX,0,nx,1,10000)
    call DlgSetI(Dlg,idc_MField_NY,0,ny,1,10000)
    call DlgSetI(Dlg,idc_MField_NZ,0,nz,1,10000)
    call DlgSetI(Dlg,idc_MField_IDom,0,Int4(iFld(nx,ny,nz)),-4,nDom)
    call DlgSetI(Dlg,idc_MField_NX2,0,ixcFld,1,10000)
    call DlgSetI(Dlg,idc_MField_NY2,0,iycFld,1,10000)
    call DlgSetI(Dlg,idc_MField_NZ2,0,izcFld,1,10000)
    call DlgSetS0(Dlg,idc_MField_X,Text)
    call DlgSetS0(Dlg,idc_MField_Y,Text)
    call DlgSetS0(Dlg,idc_MField_Z,Text)
    call DlgSetS0(Dlg,idc_MField_EXR,Text)
    call DlgSetS0(Dlg,idc_MField_EYR,Text)
    call DlgSetS0(Dlg,idc_MField_EZR,Text)
    call DlgSetS0(Dlg,idc_MField_HXR,Text)
    call DlgSetS0(Dlg,idc_MField_HYR,Text)
    call DlgSetS0(Dlg,idc_MField_HZR,Text)
    call DlgSetS0(Dlg,idc_MField_AZR,Text)
    call DlgSetS0(Dlg,idc_MField_AXR,Text)
    call DlgSetS0(Dlg,idc_MField_AYR,Text)
    call DlgSetS0(Dlg,idc_MField_VR,Text)
    call DlgSetS0(Dlg,idc_MField_EXI,Text)
    call DlgSetS0(Dlg,idc_MField_EYI,Text)
    call DlgSetS0(Dlg,idc_MField_EZI,Text)
    call DlgSetS0(Dlg,idc_MField_HXI,Text)
    call DlgSetS0(Dlg,idc_MField_HYI,Text)
    call DlgSetS0(Dlg,idc_MField_HZI,Text)
    call DlgSetS0(Dlg,idc_MField_AZI,Text)
    call DlgSetS0(Dlg,idc_MField_AXI,Text)
    call DlgSetS0(Dlg,idc_MField_AYI,Text)
    call DlgSetS0(Dlg,idc_MField_VI,Text)
    if(.not.lrGrd) then
      call DlgSetR(Dlg,idc_MField_X,rGrd(1,nx,ny,nz),5)
      call DlgSetR(Dlg,idc_MField_Y,rGrd(2,nx,ny,nz),5)
      call DlgSetR(Dlg,idc_MField_Z,rGrd(3,nx,ny,nz),5)
    end if
    if(lEcFld) then
      if(lxcFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_EXR,Dble (cFld(iEx,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_EXI,DImag(cFld(iEx,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_EXR,      dFld(iEx,nx,ny,nz),5)
        end if
      end if
      if(lycFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_EYR,Dble (cFld(iEy,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_EYI,DImag(cFld(iEy,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_EYR,      dFld(iEy,nx,ny,nz),5)
        end if
      end if
      if(lzcFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_EZR,Dble (cFld(iEz,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_EZI,DImag(cFld(iEz,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_EZR,      dFld(iEz,nx,ny,nz),5)
        end if
      end if
    end if
    if(lHcFld) then
      if(lxcFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_HXR,Dble (cFld(iHx,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_HXI,DImag(cFld(iHx,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_HXR,      dFld(iHx,nx,ny,nz),5)
        end if
      end if
      if(lycFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_HYR,Dble (cFld(iHy,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_HYI,DImag(cFld(iHy,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_HYR,      dFld(iHy,nx,ny,nz),5)
        end if
      end if
      if(lzcFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_HZR,Dble (cFld(iHz,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_HZI,DImag(cFld(iHz,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_HZR,      dFld(iHz,nx,ny,nz),5)
        end if
      end if
    end if
    if(lAcFld) then
      if(lxcFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_AXR,Dble (cFld(iAx,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_AXI,DImag(cFld(iAx,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_AXR,      dFld(iAx,nx,ny,nz),5)
        end if
      end if
      if(lycFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_AYR,Dble (cFld(iAy,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_AYI,DImag(cFld(iAy,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_AYR,      dFld(iAy,nx,ny,nz),5)
        end if
      end if
      if(lzcFld) then
        if(lFcFld) then
          call DlgSetR(Dlg,idc_MField_AZR,Dble (cFld(iAz,nx,ny,nz)),5)
          call DlgSetR(Dlg,idc_MField_AZI,DImag(cFld(iAz,nx,ny,nz)),5)
        else
          call DlgSetR(Dlg,idc_MField_AZR,      dFld(iAz,nx,ny,nz),5)
        end if
      end if
    end if
    if(lVcFld) then
      if(lFcFld) then
        call DlgSetR(Dlg,idc_MField_VR,Dble (cFld(iV,nx,ny,nz)),5)
        call DlgSetR(Dlg,idc_MField_VI,DImag(cFld(iV,nx,ny,nz)),5)
      else
        call DlgSetR(Dlg,idc_MField_VR,      dFld(iV,nx,ny,nz),5)
      end if
    end if
  end Subroutine SetMFieldData

  Subroutine GetMFieldData(dlg)
! get the Mfield data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) exr,exi,eyr,eyi,ezr,ezi,hxr,hxi,hyr,hyi,hzr,hzi, &
    &       axr,axi,ayr,ayi,azr,azi,vr,vi
    Integer(4) idum,nx,ny,nz,jx,jy,jz,mx,my,mz,ix,iy,iz
	  Type(dialog) dlg
! get borders of the selected area
    call DlgGetI(Dlg,idc_MField_NX,0,0,jx,1,nxcFld,1)
    call DlgGetI(Dlg,idc_MField_NY,0,0,jy,1,nycFld,1)
    call DlgGetI(Dlg,idc_MField_NZ,0,0,jz,1,nzcFld,1)
    call DlgGetI(Dlg,idc_MField_NX2,0,0,mx,1,nxcFld,1)
    call DlgGetI(Dlg,idc_MField_NY2,0,0,my,1,nycFld,1)
    call DlgGetI(Dlg,idc_MField_NZ2,0,0,mz,1,nzcFld,1)
    ix=1
    iy=1
    iz=1
    if(mx.lt.jx) ix=-1
    if(my.lt.jy) iy=-1
    if(mz.lt.jz) iz=-1
! get field values in the dialog boxes
    call DlgGetI(Dlg,idc_MField_IDom,0,0,idum,-4,nDom,1)
    if(lEcFld) then
      if(lxcFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_EXR,exR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_EXI,exI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_EXR,exr,nBig,pBig,0.0d0,5)
        end if
      end if
      if(lycFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_EYR,eyR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_EYI,eyI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_EYR,eyr,nBig,pBig,0.0d0,5)
        end if
      end if
      if(lzcFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_EZR,ezR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_EZI,ezI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_EZR,ezr,nBig,pBig,0.0d0,5)
        end if
      end if
    end if
    if(lHcFld) then
      if(lxcFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_HXR,hxR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_HXI,hxI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_HXR,hxr,nBig,pBig,0.0d0,5)
        end if
      end if
      if(lycFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_HYR,hyR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_HYI,hyI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_HYR,hyr,nBig,pBig,0.0d0,5)
        end if
      end if
      if(lzcFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_HZR,hzR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_HZI,hzI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_HZR,hzr,nBig,pBig,0.0d0,5)
        end if
      end if
    end if
    if(lAcFld) then
      if(lxcFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_AXR,axR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_AXI,axI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_AXR,axr,nBig,pBig,0.0d0,5)
        end if
      end if
      if(lycFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_AYR,ayR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_AYI,ayI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_AYR,ayr,nBig,pBig,0.0d0,5)
        end if
      end if
      if(lzcFld) then
        if(lFcFld) then
	        call DlgGetR(Dlg,idc_MField_AZR,azR,nBig,pBig,0.0d0,5)
	        call DlgGetR(Dlg,idc_MField_AZI,azI,nBig,pBig,0.0d0,5)
        else
	        call DlgGetR(Dlg,idc_MField_AZR,azr,nBig,pBig,0.0d0,5)
        end if
      end if
    end if
    if(lVcFld) then
      if(lFcFld) then
	      call DlgGetR(Dlg,idc_MField_VR,vR,nBig,pBig,0.0d0,5)
	      call DlgGetR(Dlg,idc_MField_VI,vI,nBig,pBig,0.0d0,5)
      else
	      call DlgGetR(Dlg,idc_MField_VR,vr,nBig,pBig,0.0d0,5)
      end if
    end if
! set all values in the selected area
    do nx=jx,mx,ix
      do ny=jy,my,iy
        do nz=jz,mz,iz
          iFld(nx,ny,nz)=Int2(idum)
          if(lEcFld) then
            if(lxcFld) then
              if(lFcFld) then
                cFld(iEx,nx,ny,nz)=DCmplx(exR,exI)
              else
                dFld(iEx,nx,ny,nz)=exR
              end if
            end if
            if(lycFld) then
              if(lFcFld) then
                cFld(iEy,nx,ny,nz)=DCmplx(eyR,eyI)
              else
                dFld(iEy,nx,ny,nz)=eyR
              end if
            end if
            if(lzcFld) then
              if(lFcFld) then
                cFld(iEz,nx,ny,nz)=DCmplx(ezR,ezI)
              else
                dFld(iEz,nx,ny,nz)=ezR
              end if
            end if
          end if
          if(lHcFld) then
            if(lxcFld) then
              if(lFcFld) then
                cFld(iHx,nx,ny,nz)=DCmplx(hxR,hxI)
              else
                dFld(iHx,nx,ny,nz)=hxR
              end if
            end if
            if(lycFld) then
              if(lFcFld) then
                cFld(iHy,nx,ny,nz)=DCmplx(hyR,hyI)
              else
                dFld(iHy,nx,ny,nz)=hyR
              end if
            end if
            if(lzcFld) then
              if(lFcFld) then
                cFld(iHz,nx,ny,nz)=DCmplx(hzR,hzI)
              else
                dFld(iHz,nx,ny,nz)=hzR
              end if
            end if
          end if
          if(lAcFld) then
            if(lxcFld) then
              if(lFcFld) then
                cFld(iAx,nx,ny,nz)=DCmplx(axR,axI)
              else
                dFld(iAx,nx,ny,nz)=axR
              end if
            end if
            if(lycFld) then
              if(lFcFld) then
                cFld(iAy,nx,ny,nz)=DCmplx(ayR,ayI)
              else
                dFld(iAy,nx,ny,nz)=azR
              end if
            end if
            if(lzcFld) then
              if(lFcFld) then
                cFld(iAz,nx,ny,nz)=DCmplx(azR,azI)
              else
                dFld(iAz,nx,ny,nz)=azR
              end if
            end if
          end if
          if(lVcFld) then
            if(lFcFld) then
                cFld(iV,nx,ny,nz)=DCmplx(vR,vI)
              else
                dFld(iV,nx,ny,nz)=vR
            end if
          end if
        end do
      end do
    end do
  end Subroutine GetMFieldData

  Subroutine updateMField(dlg,control_name,callbackType)
  ! callback for MFieldDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idok)
      call GetMFieldData(dlg)
      call GetrField(.false.)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateMField

  Subroutine MObjectDialog(lCheck)
! dialog for the Mobject data (Modify Object data)
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical, intent(in) :: lCheck
    Logical ldum
	  Type(dialog) dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_MObject,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Object data dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetMObjectData(dlg)
	    ldum=DlgSetSub(dlg,idok,updateMObject)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine MObjectDialog

  Subroutine SetMObjectData(dlg)
! set the MObject data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) ic
    Logical ldum
	  Type(dialog) dlg
    ic=abs(Int4(icolMobj))
    call DlgSetI(Dlg,idc_MObject_color,0,ic,0,235)
    ldum=.true.
    if(icolMobj.lt.0_2) ldum=.false.
	  call DlgSetL(Dlg,idc_MObject_setcolor,ldum)
    ic=abs(Int4(iconMobj))
    call DlgSetI(Dlg,idc_MObject_conn,0,ic,-32000,32000)
    ldum=.true.
    if(iconMobj.lt.0_2) ldum=.false.
	  call DlgSetL(Dlg,idc_MObject_setconn,ldum)
  end Subroutine SetMObjectData

  Subroutine GetMObjectData(dlg)
! get the MObject data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) ic
    Logical ldum
	  Type(dialog) dlg
	  call DlgGetL(Dlg,idc_MObject_setcolor,ldum)
    call DlgGetI(Dlg,idc_MObject_color,0,0,ic,0,235,2)
    icolMobj=Int2(ic)
    if(.not.ldum) icolMobj=-icolMobj
	  call DlgGetL(Dlg,idc_MObject_setconn,ldum)
    call DlgGetI(Dlg,idc_MObject_conn,0,0,ic,-32000,32000,0)
    iconMobj=Int2(ic)
    if(.not.ldum) iconMobj=-iconMobj
  end Subroutine GetMObjectData

  Subroutine updateMObject(dlg,control_name,callbackType)
  ! callback for MObjectDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum,kdum
    Logical ldum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idok)
      call GetMObjectData(dlg)
      if(icolMobj.gt.-1_2) then
        do idum=1,nExp
          if((tExp(idum)%xo.gt.xminMobj).and.(tExp(idum)%xo.lt.xmaxMobj).and. &
          &  (tExp(idum)%yo.gt.yminMobj).and.(tExp(idum)%yo.lt.ymaxMobj)) tExp(idum)%iCol=icolMobj
        end do
        do kdum=1,nBnd
          ldum=.true.
          do idum=tBnd(kdum)%iEdgeOffset+1,tBnd(kdum)%iEdgeOffset+tBnd(kdum)%nEdge
            if((tBndEdg(idum)%xo.le.xminMobj).or.(tBndEdg(idum)%xo.ge.xmaxMobj).or. &
            &  (tBndEdg(idum)%yo.le.yminMobj).or.(tBndEdg(idum)%yo.ge.ymaxMobj)) then
              ldum=.false.
              Exit
            end if
          end do
          if(ldum) tBnd(kdum)%iCol=icolMobj
        end do
      end if
      if(iconMobj.gt.-1_2) then
        do idum=1,nExp
          if((tExp(idum)%xo.gt.xminMobj).and.(tExp(idum)%xo.lt.xmaxMobj).and. &
          &  (tExp(idum)%yo.gt.yminMobj).and.(tExp(idum)%yo.lt.ymaxMobj)) tExp(idum)%iConn=iconMobj
        end do
        do kdum=1,nBnd
          ldum=.true.
          do idum=tBnd(kdum)%iEdgeOffset+1,tBnd(kdum)%iEdgeOffset+tBnd(kdum)%nEdge
            if((tBndEdg(idum)%xo.le.xminMobj).and.(tBndEdg(idum)%xo.le.xmaxMobj).and. &
            &  (tBndEdg(idum)%yo.le.yminMobj).and.(tBndEdg(idum)%yo.le.ymaxMobj)) then
              ldum=.false.
              Exit
            end if
          end do
          if(ldum) tBnd(kdum)%iConn=iconMobj
        end do
      end if
      call DlgExit(dlg)
    end Select
  end Subroutine updateMObject

  recursive Subroutine MMPDialog(lCheck)
! Dialog for setting parameters of the MMP computation
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) idum
    Logical, intent(in) :: lCheck
    Logical ldum
    Type(dialog) Dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
      ldum=lCheck
      ldum=DlgInit(IDD_MMP,Dlg)
      if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'MMP dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    else
      IsOpen=1
      if(.not.lgcFld) lGet3DMat=.true.
      call SetMMPData(Dlg)
      ldum=DlgSetSub(Dlg,idc_mmp_cond,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_points,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_solve,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_solvee,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_error,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_read,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_write,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_writErr,updateMMP)
      ldum=DlgSetSub(Dlg,idc_mmp_writeMtrx,updateMMP)
      ldum=DlgSetSub(Dlg,idcancel,updateMMP)
    end if
    ldum=DlgModal(Dlg)
    call DlgUnInit(Dlg)
    IsOpen=0
    iBound=0_2
    select case(iDlgExit)
    case(1)
      call genMatPts(ldum)
      if(.not.lgcFld)  call get3DMatPts(1,nObj,0_4,-1_2,.true.) 
    case(2)
      if(lEigen) then
        call TMMPEvl(.false.)
      else
        call TMMPRes(.false.)
      end if
    case(3)
      if(lEigen) then
        call TMMPEvlErr(.false.)
      else
        call TMMPResErr(.false.)
      end if
    case(4)
      call TMMPErr(.false.)
    case(5)
      call TMMPCnd(.false.)
    end select
  end Subroutine MMPDialog

  Subroutine SetMMPData(Dlg)
! set dialog boxes containing MMP data
    Implicit none
    Include 'RESOURCE.FD'
    Type(dialog) Dlg
    if(lgcFld) then
      call DlgSetI(Dlg,idc_mmp_nBndPt,0,nBndPt,0,1000000)
      call DlgSetI(Dlg,idc_mmp_nBndPt2,0,nBndPt,0,1000000)
    else
      call DlgSetI(Dlg,idc_mmp_nBndPt,0,nBndPt3D-nInhBndPt3D,0,1000000)
      call DlgSetI(Dlg,idc_mmp_nBndPt2,0,nBndPt,0,1000000)
    end if
    call DlgSetI(Dlg,idc_mmp_nSegPt,0,nSegPt,0,1000000)
    call DlgSetI(Dlg,idc_mmp_iAmpl,0,iAmpl,-1000000,1000000)
    call DlgSetI(Dlg,idc_mmp_iLast,0,iMMPLast,0,1000000)
    call DlgSetI(Dlg,idc_mmp_itmCG,0,itmCG,1,1000000)
    call DlgSetI(Dlg,idc_mmp_iaccCG,0,iaccCG,0,16)
    call DlgSetI(Dlg,idc_mmp_itCG,0,itCG,0,1000000)
    call DlgSetI(Dlg,idc_mmp_MtrCol,0,mCol,0,1000000)
    call DlgSetI(Dlg,idc_mmp_MtrRow,0,nRow,0,1000000)
    call DlgSetI(Dlg,idc_mmp_itEigen,0,itEigen,0,1000000)
    call DlgSetI(Dlg,idc_mmp_nPET,0,nPET-1,0,mPET-1)
    call DlgSetI(Dlg,idc_mmp_CONN,0,Int4(iMMPCon),-32000,32000)
    call DlgSetI(Dlg,idc_mmp_IFUNW,0,Int4(iFunWeight),-32000,32000)
    call DlgSetI(Dlg,IDC_MMP_IFINDEXP,0,Int4(iMMPFindExp),-9,9)
    call DlgSetI(Dlg,IDC_MMP_ISM,0,iScaleMatrix,-3,3)
    call DlgSetR(Dlg,idc_mmp_residual,resEigen,5)
    call DlgSetR(Dlg,idc_mmp_resCG,resCG,5)
    call DlgSetR(Dlg,idc_mmp_resPET,resPET,5)
    call DlgSetR(Dlg,idc_mmp_Matrix0,fMatrix0,5)
    call DlgSetR(Dlg,idc_mmp_Amplitude,currentIntegral,5)
    call DlgSetR(Dlg,idc_mmp_errm,errorM,5)
    call DlgSetR(Dlg,idc_mmp_erra,errorA,5)
    call DlgSetR(Dlg,idc_mmp_condition,condition,4)
    call DlgSetR(Dlg,idc_mmp_BndDMax,BndDMax,5)
    call DlgSetR(Dlg,idc_mmp_BndPpW,BndPpW,5)
    call DlgSetR(Dlg,idc_mmp_BndOver,BndOver,5)
    call DlgSetR(Dlg,idc_mmp_SCALERROR,ErrorScale,3)
    call DlgSetL(Dlg,idc_mmp_rough,lMMPrough)
    call DlgSetL(Dlg,idc_mmp_fine,lMMPfine)
    call DlgSetL(Dlg,idc_mmp_lPET,lPET)
    if(lEigen.and.(iMtrSolver.eq.3)) iMtrSolver=2
    if(iMtrSolver.eq.0) then
      call DlgSetL(Dlg,idc_mmp_GUT,.true.)
    else if(iMtrSolver.eq.1) then
      call DlgSetL(Dlg,idc_mmp_GUR,.true.)
    else if(iMtrSolver.eq.2) then
      call DlgSetL(Dlg,idc_mmp_CG,.true.)
    else if(iMtrSolver.eq.3) then
      call DlgSetL(Dlg,idc_mmp_PET,.true.)
    else if(iMtrSolver.eq.4) then
      call DlgSetL(Dlg,idc_mmp_CHO,.true.)
    else if(iMtrSolver.eq.5) then
      call DlgSetL(Dlg,idc_mmp_QRR,.true.)
    else
      iMtrSolver=5
      call DlgSetL(Dlg,idc_mmp_QRR,.true.)
    end if
    call DlgSetL(Dlg,idc_mmp_UseRes,lMMPuseRes)
    call DlgSetL(Dlg,idc_mmp_l3F,lMMP3F)
    if(iAmplTyp.eq.1) then
      call DlgSetL(Dlg,idc_mmp_intB,.true.)
    else if(iAmplTyp.eq.2) then
      call DlgSetL(Dlg,idc_mmp_intR,.true.)
    else if(iAmplTyp.eq.3) then
      call DlgSetL(Dlg,idc_mmp_intO,.true.)
    else if(iAmplTyp.eq.4) then
      call DlgSetL(Dlg,idc_mmp_intP,.true.)
    else
      call DlgSetL(Dlg,idc_mmp_int1,.true.)
    end if
  end Subroutine SetMMPData

  Subroutine GetMMPData(Dlg)
! get MMP data
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) i
    Logical ldum
    Type(dialog) Dlg
    call DlgGetI(Dlg,idc_mmp_nSegPt,0,0,nSegPt,0,1000000,2)
    call DlgGetI(Dlg,idc_mmp_iAmpl,0,0,iAmpl,-1000000,1000000,0)
    call DlgGetI(Dlg,idc_mmp_itmCG,0,0,itmCG,1,1000000,10)
    call DlgGetI(Dlg,idc_mmp_iLast,0,0,iMMPLast,0,1000000,0)
    call DlgGetI(Dlg,idc_mmp_iaccCG,0,0,iaccCG,0,16,10)
    call DlgGetI(Dlg,idc_mmp_nPET,0,0,nPET,0,mPET-1,2)
    nPET=nPET+1
    call DlgGetI(Dlg,idc_mmp_CONN,0,0,i,-32000,32000,0)
    iMMPCon=Int2(i)
    call DlgGetI(Dlg,idc_mmp_IFUNW,0,0,i,-32000,32000,0)
    iFunWeight=Int2(i)
    call DlgGetI(Dlg,IDC_MMP_IFINDEXP,0,0,i,-9,9,0)
    iMMPFindExp=Int2(i)
    call DlgGetI(Dlg,idc_mmp_ISM,0,0,iScaleMatrix,-3,3,2)
    call DlgGetR(Dlg,idc_mmp_resCG,resCG,nBig,pBig,0.0d0,5)
    call DlgGetR(Dlg,idc_mmp_resPET,resPET,1.0d0,1.0d10,4.0d0,5)
    call DlgGetR(Dlg,idc_mmp_Matrix0,fMatrix0,0.0d0,pBig,0.0d0,5)
    call DlgGetR(Dlg,idc_mmp_BndDMax,BndDMax,0.0d0,pBig,0.1d0,5)
    call DlgGetR(Dlg,idc_mmp_BndPpW,BndPpW,0.0d0,pBig,5.0d0,5)
    call DlgGetR(Dlg,idc_mmp_BndOver,BndOver,0.0d0,pBig,2.0d0,5)
    call DlgGetR(Dlg,idc_mmp_SCALERROR,ErrorScale,nBig,pBig,0.0d0,3)
    call DlgGetL(Dlg,idc_mmp_rough,lMMPrough)
    call DlgGetL(Dlg,idc_mmp_fine,lMMPfine)
    call DlgGetL(Dlg,idc_mmp_LPET,lPET)
    iMtrSolver=5
	  call DlgGetL(Dlg,idc_mmp_GUT,ldum)
    if(ldum) iMtrSolver=0
	  call DlgGetL(Dlg,idc_mmp_GUR,ldum)
    if(ldum) iMtrSolver=1
	  call DlgGetL(Dlg,idc_mmp_CG,ldum)
    if(ldum) iMtrSolver=2
	  call DlgGetL(Dlg,idc_mmp_PET,ldum)
    if(ldum) iMtrSolver=3
	  call DlgGetL(Dlg,idc_mmp_CHO,ldum)
    if(ldum) iMtrSolver=4
	  call DlgGetL(Dlg,idc_mmp_QRR,ldum)
    if(ldum) iMtrSolver=5
	  call DlgGetL(Dlg,idc_mmp_UseRes,lMMPuseRes)
	  call DlgGetL(Dlg,idc_mmp_l3F,lMMP3F)
	  call DlgGetL(Dlg,idc_mmp_int1,ldum)
    if(ldum) iAmplTyp=0
	  call DlgGetL(Dlg,idc_mmp_intB,ldum)
    if(ldum) iAmplTyp=1
	  call DlgGetL(Dlg,idc_mmp_intR,ldum)
    if(ldum) iAmplTyp=2
	  call DlgGetL(Dlg,idc_mmp_intO,ldum)
    if(ldum) iAmplTyp=3
	  call DlgGetL(Dlg,idc_mmp_intP,ldum)
    if(ldum) iAmplTyp=4
  end Subroutine GetMMPData

	Subroutine updateMMP(Dlg,control_name,callbackType)
! callback for MMPDialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum
	  Type (dialog) Dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_mmp_points)
      call GetMMPData(Dlg)
      iDlgExit=1
     	call DlgExit(Dlg)
	  Case(idc_mmp_solve)
      call GetMMPData(Dlg)
      iDlgExit=2
     	call DlgExit(Dlg)
	  Case(idc_mmp_solvee)
      call GetMMPData(Dlg)
      iDlgExit=3
     	call DlgExit(Dlg)
	  Case(idc_mmp_error)
      call GetMMPData(Dlg)
      iDlgExit=4
     	call DlgExit(Dlg)
	  Case(idc_mmp_cond)
      call GetMMPData(Dlg)
      iDlgExit=5
     	call DlgExit(Dlg)
	  Case(idc_mmp_read)
      call GetMMPData(Dlg)
      call OpenMmp(.false.)
      call SetMMPData(Dlg)
	  Case(idc_mmp_write)
      call GetMMPData(Dlg)
      call SaveMmp(.false.)
      call SetMMPData(Dlg)
	  Case(idc_mmp_writErr)
      call GetMMPData(Dlg)
      call SaveError(.false.)
      call SetMMPData(Dlg)
	  Case(idc_mmp_writeMtrx)
      call GetMMPData(Dlg)
      if(Allocated(MMPMtr).and.(iMtrSolver.gt.0).and.(iMtrSolver.ne.4)) then
        idum=MessageBoxQQ('Save rectangular MMP matrix?'C,'Save MMP data'C, &
                          MB$YesNo.or.MB$IconQuestion)
        if(idum.eq.MB$IDYES) call SaveRmatrix(.false.)
      end if
      if(Allocated(TriMtr).and.((iMtrSolver.lt.2).or.(iMtrSolver.eq.4))) then
        idum=MessageBoxQQ('Save trapezoidal MMP matrix?'C,'Save MMP data'C, &
                          MB$YesNo.or.MB$IconQuestion)
        if(idum.eq.MB$IDYES) call SaveTmatrix(.false.)
      else
        idum=MessageBoxQQ('No trapezoidal MMP matrix available!'C,'Save MMP data'C, &
                          MB$OK.or.MB$IconExclamation)
      end if
      call SetMMPData(Dlg)
	  Case(idcancel)
      call GetMMPData(Dlg)
      iDlgExit=0
      if(.not.lPET) kPET=0
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateMMP

  Subroutine MovieDialog(lCheck)
! dialog for the movie data
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical, intent(in) :: lCheck
    Logical ldum
	  Type(dialog) dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Movie,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Movie dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      idum=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
      ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.false.)
      call SetMovieData(dlg)
      ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.true.)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_KCOMMANDS,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_CHECK,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_CHECK2,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_CHECK3,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_CHECK4,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_CHECK5,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_CHECK6,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_GENERATE,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_PERFORM,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_PERFORM2,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_PERFORM3,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_PERFORM4,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_PERFORM5,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_PERFORM6,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_READ,updateMovie)
	    ldum=DlgSetSub(dlg,IDC_MOVIE_WRITE,updateMovie)
	    ldum=DlgSetSub(dlg,idcancel,updateMovie)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine MovieDialog

  Subroutine SetMovieData(dlg)
! set the Movie data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Type(dialog) dlg
    call DlgSetI(Dlg,IDC_MOVIE_NPIC,0,nMovPic,1,1000000)
    call DlgSetI(Dlg,IDC_MOVIE_NSEQ,0,nMovSeq,1,1000000)
    call DlgSetI(Dlg,IDC_MOVIE_KCOMMAND,IDC_MOVIE_KCOMMANDS,kMovCommand,1,nMovCommand)
    call DlgSetI(Dlg,IDC_MOVIE_DELAY,0,nAVIfpsec,1,100)
    call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
    if(lsMovComm.lt.1) Mov_Command='!UNDEFINED!'C
    call DlgSetS0(Dlg,IDC_MOVIE_COMMAND,Mov_Command)
    call DlgSetS0(Dlg,IDC_MOVIE_COMMAND2,Mov_Command2(1))
    call DlgSetS0(Dlg,IDC_MOVIE_COMMAND3,Mov_Command2(2))
    call DlgSetS0(Dlg,IDC_MOVIE_COMMAND4,Mov_Command2(3))
    call DlgSetS0(Dlg,IDC_MOVIE_COMMAND5,Mov_Command2(4))
    call DlgSetS0(Dlg,IDC_MOVIE_COMMAND6,Mov_Command2(5))
    idum=GetSLength(Mov_Commands)
    lsMovComms=idum
    idum=nStrInStr(Mov_Commands,lsMovCommands-1)
    nMovCommand=min(mMovCommand,idum)
    kMovCommand=min(kMovCommand,nMovCommand)
    idum=SetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands(1:lsMovComms)//Char(0))
  end Subroutine SetMovieData

  Subroutine GetMovieData(dlg)
! get the Movie data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Type(dialog) dlg
    idum=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
    call DlgGetI(Dlg,IDC_MOVIE_NPIC,0,0,nMovPic,1,1000000,10)
    call DlgGetI(Dlg,IDC_MOVIE_NSEQ,0,0,nMovSeq,1,1000000,10)
    call DlgGetI(Dlg,IDC_MOVIE_KCOMMAND,IDC_MOVIE_KCOMMANDS,1,kMovCommand,1,nMovCommand,1)
    call DlgGetI(Dlg,IDC_MOVIE_DELAY,0,0,nAVIfpsec,1,100,10)
    call DlgGetS(Dlg,IDC_MOVIE_COMMAND,Mov_Command)
    call DlgGetS(Dlg,IDC_MOVIE_COMMAND2,Mov_Command2(1))
    call DlgGetS(Dlg,IDC_MOVIE_COMMAND3,Mov_Command2(2))
    call DlgGetS(Dlg,IDC_MOVIE_COMMAND4,Mov_Command2(3))
    call DlgGetS(Dlg,IDC_MOVIE_COMMAND5,Mov_Command2(4))
    call DlgGetS(Dlg,IDC_MOVIE_COMMAND6,Mov_Command2(5))
  end Subroutine GetMovieData

  Subroutine updateMovie(dlg,control_name,callbackType)
  ! callback for MovieDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum,k
    Type (dialog) dlg
    idum=callbackType
    k=1
    Select Case(control_name)
    Case(IDC_MOVIE_KCOMMANDS)
      call GetMovieData(dlg)
      call SetMovieData(dlg)
    Case(IDC_MOVIE_check)
      call GetMovieData(dlg)
      call MovieCommand(.false.,k,idum)
      if(idum.eq.1) then
        idum=MessageBoxQQ('Blank line causes stop!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.gt.0) then
        idum=MessageBoxQQ('Cannot successfully run this command!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      elseif(idum.lt.0) then
        idum=MessageBoxQQ('This command is ignored!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      elseif(k.ne.1) then
        idum=MessageBoxQQ('Jump command, cannot check address!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else
        kMovCommand=kMovCommand+1
        call SetMovieData(dlg)
      end if
    Case(IDC_MOVIE_check2,IDC_MOVIE_check3,IDC_MOVIE_check4,IDC_MOVIE_check5,IDC_MOVIE_check6)
      call GetMovieData(dlg)
      Mov_Command1=Mov_Command
      Select Case(control_name)
      Case(IDC_MOVIE_check2)
        Mov_Command=Mov_Command2(1)
      Case(IDC_MOVIE_check3)
        Mov_Command=Mov_Command2(2)
      Case(IDC_MOVIE_check4)
        Mov_Command=Mov_Command2(3)
      Case(IDC_MOVIE_check5)
        Mov_Command=Mov_Command2(4)
      Case(IDC_MOVIE_check6)
        Mov_Command=Mov_Command2(5)
      end Select
      kMovCommand1=kMovCommand
      kMovCommand=0
      call MovieCommand(.false.,k,idum)
      if(idum.eq.1) then
        idum=MessageBoxQQ('Blank line causes stop!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.gt.0) then
        idum=MessageBoxQQ('Cannot successfully run this command!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.lt.0) then
        idum=MessageBoxQQ('This command is ignored!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      elseif(k.ne.1) then
        idum=MessageBoxQQ('Jump command, cannot check address!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('No syntax errors detected!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      Mov_Command=Mov_Command1
      kMovCommand=kMovCommand1
    Case(IDC_MOVIE_perform)
      call GetMovieData(dlg)
      call MovieCommand(.true.,k,idum)
      if(idum.eq.1) then
        idum=MessageBoxQQ('Blank line causes stop!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.gt.0) then
        idum=MessageBoxQQ('Cannot successfully run this command!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.lt.0) then
        idum=MessageBoxQQ('This command is ignored!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      elseif(k.ne.1) then
        idum=MessageBoxQQ('Jump command, cannot perform here!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Movie command completed!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      end if
    Case(IDC_MOVIE_perform2,IDC_MOVIE_perform3,IDC_MOVIE_perform4,IDC_MOVIE_perform5,IDC_MOVIE_perform6)
      call GetMovieData(dlg)
      Mov_Command1=Mov_Command
      Select Case(control_name)
      Case(IDC_MOVIE_perform2)
        Mov_Command=Mov_Command2(1)
      Case(IDC_MOVIE_perform3)
        Mov_Command=Mov_Command2(2)
      Case(IDC_MOVIE_perform4)
        Mov_Command=Mov_Command2(3)
      Case(IDC_MOVIE_perform5)
        Mov_Command=Mov_Command2(4)
      Case(IDC_MOVIE_perform6)
        Mov_Command=Mov_Command2(5)
      end Select
      kMovCommand1=kMovCommand
      kMovCommand=0
      k=kMovCommand1
      call MovieCommand(.true.,k,idum)
      if(idum.eq.1) then
        idum=MessageBoxQQ('Blank line causes stop!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.gt.0) then
        idum=MessageBoxQQ('Cannot successfully run this command!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      else if(idum.lt.0) then
        idum=MessageBoxQQ('This command is ignored!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      elseif(k.ne.kMovCommand1) then
        idum=MessageBoxQQ('Jump command!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
        kMovCommand1=k
      else
        idum=MessageBoxQQ('Movie command completed!'C, &
                          'Movie dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      Mov_Command=Mov_Command1
      kMovCommand=kMovCommand1
    Case(IDC_MOVIE_read)
      call OpenDirectives(.false.)
      call SetMovieData(dlg)
    Case(IDC_MOVIE_write)
      call GetMovieData(dlg)
      call SaveDirectives(.false.)
      call SetMovieData(dlg)
    Case(IDC_MOVIE_generate)
      call GetMovieData(dlg)
	    call DlgExit(dlg)
	    call TSaveMovie(.true.)
    Case(idcancel)
      call GetMovieData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateMovie

  Subroutine ObjectDialog(lCheck)
! Dialog for setting parameters of the various 3D objects
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical ldum,lCheck
	  Type(dialog) Dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Object,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Object dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      if(.not.lgcFld) lGet3DMat=.true.
      call SetObjectData(Dlg)
      ldum=DlgSetSub(Dlg,idc_OBJECT_kOBJ,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_kOBJS,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_srOBJS,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_siOBJS,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_TORUS,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_CYLINDER,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_SPIRAL,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_TRIANGLE,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_RECTANGLE,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_CONE,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_ADD,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_COPY,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_DELETE,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_LOCATION,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_AXIS,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_ORIGIN,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_POINTS,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_POINTS3,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_read,updateObject)
      ldum=DlgSetSub(Dlg,idc_OBJECT_write,updateObject)
      ldum=DlgSetSub(Dlg,idcancel,updateObject)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine ObjectDialog

  Subroutine SetObjectData(Dlg)
! set dialog boxes containing Object data
    Implicit none
    Include 'RESOURCE.FD'
    Type(dialog) Dlg
    kObj=max(1,min(kObj,nObj))
    call DlgSetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,kOBJ,1,nOBJ)
    call DlgSetI(Dlg,idc_OBJECT_iCol,0,Int4(tOBJ(kOBJ)%iCol),-1,235)
    call DlgSetI(Dlg,idc_OBJECT_iColMin,0,Int4(tOBJ(kOBJ)%iColMin),0,235)
    call DlgSetI(Dlg,idc_OBJECT_iColMax,0,Int4(tOBJ(kOBJ)%iColMax),0,235)
    call DlgSetR(Dlg,idc_OBJECT_iGrf,tOBJ(kOBJ)%GrfRes,8)
    call DlgSetR(Dlg,idc_OBJECT_GrfH1,GRChmin,6)
    call DlgSetR(Dlg,idc_OBJECT_GrfH2,GRChmax,6)
    select case(iObjDra)
    case(1_2)
      call DlgSetL(dlg,idc_OBJECT_lTra,.true.)
    case(2_2)
      call DlgSetL(dlg,idc_OBJECT_lDom,.true.)
    case(3_2)
      call DlgSetL(dlg,idc_OBJECT_lErr,.true.)
    case(4_2)
      call DlgSetL(dlg,idc_OBJECT_lFld,.true.)
    case(5_2)
      call DlgSetL(dlg,idc_OBJECT_lExp,.true.)
    case default
      call DlgSetL(dlg,idc_OBJECT_lDef,.true.)
    end select
    lCHGLdoubleSide=.true.
    if((iObjDra.eq.3).or.(iObjDra.eq.4)) lCHGLdoubleSide=.false.
    call DlgSetL(dlg,idc_OBJECT_lMat,lObjMat)
    call DlgSetL(dlg,idc_OBJECT_lMatW0,lObjMatW0)
    call DlgSetL(dlg,idc_OBJECT_lFlg,lObjFlg)
    call DlgSetL(dlg,idc_OBJECT_lHid,lObjHid)
    call DlgSetI(Dlg,IDC_OBJECT_IDRAOBJ,0,iDraObj,-nObj,nObj)
    select case(tOBJ(kOBJ)%iTypO)
    case(0_2)
      call DlgSetL(dlg,idc_OBJECT_Torus,.true.)
    case(1_2)
      call DlgSetL(dlg,idc_OBJECT_Cylinder,.true.)
    case(2_2)
      call DlgSetL(dlg,idc_OBJECT_Spiral,.true.)
    case(3_2)
      call DlgSetL(dlg,idc_OBJECT_Triangle,.true.)
    case(4_2)
      call DlgSetL(dlg,idc_OBJECT_Rectangle,.true.)
    case(5_2)
      call DlgSetL(dlg,idc_OBJECT_Cone,.true.)
    end select
    call DlgSetI(Dlg,0,idc_OBJECT_srOBJs,Int4(krOBJ),1,5)
    call DlgSetS0(Dlg,idc_OBJECT_srOBJ,srOBJ(krOBJ,tOBJ(kOBJ)%iTypO))
    call DlgSetI(Dlg,0,idc_OBJECT_siOBJs,Int4(kiOBJ),1,5)
    call DlgSetS0(Dlg,idc_OBJECT_siOBJ,siOBJ(kiOBJ,tOBJ(kOBJ)%iTypO))
    call DlgSetR(Dlg,idc_OBJECT_rOBJ,tOBJ(kOBJ)%Par(krOBJ),10)
    call DlgSetI(Dlg,idc_OBJECT_iOBJ,0,Int4(tOBJ(kOBJ)%iPar(kiOBJ)),-32000,32000)
    call DlgSetI(Dlg,idc_OBJECT_nOBJ,0,nOBJ,1,32767)
  end Subroutine SetObjectData

  Subroutine GetObjectData(Dlg,ls)
! get Object data
    Implicit none
    Include 'RESOURCE.FD'
    Real(8) dum1
    Integer(4) kP,idum
    Logical, intent(in) :: ls
    Logical ldum
    Type(dialog) Dlg
    if(ls) then
      call DlgGetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,1,kOBJ,1,nOBJ,nOBJ)
    else
      call DlgGetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,0,kOBJ,1,nOBJ,nOBJ)
    end if
    call DlgGetI(Dlg,idc_OBJECT_iCol,0,0,kP,-1,235,1)
    tOBJ(kOBJ)%iCol=Int2(kP)
    call DlgGetI(Dlg,idc_OBJECT_iColMin,0,0,kP,0,235,1)
    tOBJ(kOBJ)%iColMin=Int2(kP)
    call DlgGetI(Dlg,idc_OBJECT_iColMax,0,0,kP,0,235,1)
    tOBJ(kOBJ)%iColMax=Int2(kP)
    call DlgGetR(Dlg,idc_OBJECT_iGrf,dum1,nBig,pBig,0.0d0,8)
    tOBJ(kOBJ)%GrfRes=dum1
    call DlgGetR(Dlg,idc_OBJECT_GrfH1,GRChmin,nBig,pBig,nBig,6)
    call DlgGetR(Dlg,idc_OBJECT_GrfH2,GRChmax,nBig,pBig,pBig,6)
    call DlgGetL(dlg,idc_OBJECT_lDef,ldum)
    if(ldum) iObjDra=0_2
    call DlgGetL(dlg,idc_OBJECT_lTra,ldum)
    if(ldum) iObjDra=1_2
    call DlgGetL(dlg,idc_OBJECT_lDom,ldum)
    if(ldum) iObjDra=2_2
    call DlgGetL(dlg,idc_OBJECT_lErr,ldum)
    if(ldum) iObjDra=3_2
    call DlgGetL(dlg,idc_OBJECT_lFld,ldum)
    if(ldum) iObjDra=4_2
    call DlgGetL(dlg,idc_OBJECT_lExp,ldum)
    if(ldum) iObjDra=5_2
    lCHGLdoubleSide=.true.
    if((iObjDra.eq.3).or.(iObjDra.eq.4)) lCHGLdoubleSide=.false.
    call DlgGetL(dlg,idc_OBJECT_lMat,lObjMat)
    call DlgGetL(dlg,idc_OBJECT_lMatW0,lObjMatW0)
    call DlgGetL(dlg,idc_OBJECT_lFlg,lObjFlg)
    call DlgGetL(dlg,idc_OBJECT_lHid,lObjHid)
    call DlgGetI(Dlg,IDC_OBJECT_IDRAOBJ,0,0,iDraObj,-nObj,nObj,0)
    call DlgGetL(dlg,idc_OBJECT_Torus,ldum)
    if(ldum) then
      tOBJ(kOBJ)%iTypO=0_2
    else
      call DlgGetL(dlg,idc_OBJECT_Cylinder,ldum)
      if(ldum) then
        tOBJ(kOBJ)%iTypO=1_2
      else
        call DlgGetL(dlg,idc_OBJECT_Spiral,ldum)
        if(ldum) then
          tOBJ(kOBJ)%iTypO=2_2
        else
          call DlgGetL(dlg,idc_OBJECT_Triangle,ldum)
          if(ldum) then
            tOBJ(kOBJ)%iTypO=3_2
          else
            call DlgGetL(dlg,idc_OBJECT_Rectangle,ldum)
            if(ldum) then
              tOBJ(kOBJ)%iTypO=4_2
            else
              tOBJ(kOBJ)%iTypO=5_2 ! cone
            end if
          end if
        end if
      end if
    end if
    call DlgGetI(Dlg,0,idc_OBJECT_srOBJs,1,krOBJ,1,5,1)
    call DlgGetI(Dlg,0,idc_OBJECT_siOBJs,1,kiOBJ,1,5,1)
    call DlgGetR(Dlg,idc_OBJECT_rOBJ,dum1,nBig,pBig,0.0d0,10)
    tOBJ(kOBJ)%Par(krOBJ)=dum1
    call DlgGetI(Dlg,idc_OBJECT_iOBJ,0,0,idum,-32000,32000,0)
    tOBJ(kOBJ)%iPar(kiOBJ)=idum
  end Subroutine GetObjectData

	Subroutine updateObject(Dlg,control_name,callbackType)
! callback for ObjectDialog
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) dum
	  Integer(4) control_name,callbackType,idum
    Logical ldum
	  Type (dialog) Dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_OBJECT_kOBJ)
      call DlgGetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,0,idum,1,nOBJ,nOBJ)
      call DlgSetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,kOBJ,1,nOBJ)
      call GetObjectData(Dlg,.false.)
      kOBJ=idum
      call SetObjectData(Dlg)
	  Case(idc_OBJECT_kOBJS)
      call DlgGetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,1,idum,1,nOBJ,nOBJ)
      call DlgSetI(Dlg,idc_OBJECT_kOBJ,idc_OBJECT_kOBJS,kOBJ,1,nOBJ)
      call GetObjectData(Dlg,.false.)
      kOBJ=idum
      call SetObjectData(Dlg)
	  Case(idc_OBJECT_siOBJS)
	    call DlgGetI(Dlg,idc_OBJECT_iOBJ,0,0,idum,-32000,32000,0)
      tOBJ(kOBJ)%iPar(kiOBJ)=idum
      call DlgGetI(Dlg,0,idc_OBJECT_siOBJs,1,kiOBJ,1,5,1)
      call DlgSetS0(Dlg,idc_OBJECT_siOBJ,siOBJ(kiOBJ,tOBJ(kOBJ)%iTypO))
      call DlgSetI(Dlg,idc_OBJECT_iOBJ,0,Int4(tOBJ(kOBJ)%iPar(kiOBJ)),-32000,32000)
	  Case(idc_OBJECT_srOBJS)
	    call DlgGetR(Dlg,idc_OBJECT_rOBJ,dum,nBig,pBig,0.0d0,7)
      tOBJ(kOBJ)%Par(krOBJ)=dum
      call DlgGetI(Dlg,0,idc_OBJECT_srOBJs,1,krOBJ,1,5,1)
      call DlgSetS0(Dlg,idc_OBJECT_srOBJ,srOBJ(krOBJ,tOBJ(kOBJ)%iTypO))
      call DlgSetR(Dlg,idc_OBJECT_rOBJ,tOBJ(kOBJ)%Par(krOBJ),10)
	  Case(idc_OBJECT_Torus,idc_OBJECT_Cylinder,idc_OBJECT_Spiral,idc_OBJECT_Triangle,idc_OBJECT_Rectangle,idc_OBJECT_Cone)
      call GetObjectData(Dlg,.false.)
      call SetObjectData(Dlg)
	  Case(idc_OBJECT_delete)
      if(nOBJ.gt.1) then
        idum=kOBJ
        call GetObjectData(Dlg,.false.)
        call InsertOBJ(kOBJ,-1,ldum)
        kOBJ=min(idum,nOBJ)
        call SetObjectData(Dlg)
      end if
	  Case(idc_OBJECT_add)
      call GetObjectData(Dlg,.false.)
      call InsertOBJ(kOBJ,1,ldum)
      kOBJ=kOBJ+1
      call SetObjectData(Dlg)
	  Case(idc_OBJECT_copy)
      call GetObjectData(Dlg,.false.)
      nInsObj=nOBJ
      kInsObj=kOBJ
      call InsertDialog(.false.)
      if(kInsObj.ge.0) then
        call InsertOBJ(kInsObj,1,ldum)
        tObj(kInsObj+1)=tObj(kOBJ)
      end if
      call SetObjectData(Dlg)
	  Case(idc_OBJECT_Location)
      call GetObjectData(Dlg,.false.)
      space=tOBJ(kOBJ)%Plane
      SpaceText='Object location'C
      iSpaceRead=3
      call SpaceDialog(.true.)
      tOBJ(kOBJ)%Plane=space
      call Ortho3DSpace(tOBJ(kOBJ)%Plane)
	  Case(idc_OBJECT_Axis)
      if(tOBJ(kOBJ)%iTypO.eq.2) then
        call GetObjectData(Dlg,.false.)
        space(1:3,0)=tOBJ(kOBJ)%O
        space(1:3,1)=tOBJ(kOBJ)%e
        space(1:3,2:3)=0.0d0
        SpaceText='Object axis'C
        iSpaceRead=1
        call SpaceDialog(.true.)
        tOBJ(kOBJ)%O=space(1:3,0)
        tOBJ(kOBJ)%e=space(1:3,1)
        call Unit3DV(tOBJ(kOBJ)%e)
      end if
	  Case(idc_OBJECT_Origin)
      if(tOBJ(kOBJ)%iTypO.eq.5) then
        call GetObjectData(Dlg,.false.)
        space(1:3,0)=tOBJ(kOBJ)%O
        space(1:3,1:3)=0.0d0
        SpaceText='Object origin'C
        iSpaceRead=0
        call SpaceDialog(.true.)
        tOBJ(kOBJ)%O=space(1:3,0)
        call Unit3DV(tOBJ(kOBJ)%e)
      end if
	  Case(idc_OBJECT_Points)
      if(tOBJ(kOBJ)%iTypO.eq.3) then
        call GetObjectData(Dlg,.false.)
        points2D(1,1)=tOBJ(kOBJ)%O(1)
        points2D(2,1)=tOBJ(kOBJ)%O(2)
        points2D(1,2)=tOBJ(kOBJ)%O(3)
        points2D(2,2)=tOBJ(kOBJ)%e(1)
        points2D(1,3)=tOBJ(kOBJ)%e(2)
        points2D(2,3)=tOBJ(kOBJ)%e(3)
        Points2DText='Corners in local plane'C
        iPoints2DRead=3
        call Points2DDialog(.true.)
        tOBJ(kOBJ)%O(1)=points2D(1,1)
        tOBJ(kOBJ)%O(2)=points2D(2,1)
        tOBJ(kOBJ)%O(3)=points2D(1,2)
        tOBJ(kOBJ)%e(1)=points2D(2,2)
        tOBJ(kOBJ)%e(2)=points2D(1,3)
        tOBJ(kOBJ)%e(3)=points2D(2,3)
        points3D(1,1)=tOBJ(kOBJ)%O(1) ! imitate 3D points dialog open and close
        points3D(2,1)=tOBJ(kOBJ)%O(2)
        points3D(1,2)=tOBJ(kOBJ)%O(3)
        points3D(2,2)=tOBJ(kOBJ)%e(1)
        points3D(1,3)=tOBJ(kOBJ)%e(2)
        points3D(2,3)=tOBJ(kOBJ)%e(3)
        points3D(3,1:3)=0.0d0
        call vLoc2Glob(points3D(1:3,1),tOBJ(kOBJ)%plane,points3D(1:3,1))
        call vLoc2Glob(points3D(1:3,2),tOBJ(kOBJ)%plane,points3D(1:3,2))
        call vLoc2Glob(points3D(1:3,3),tOBJ(kOBJ)%plane,points3D(1:3,3))
        tOBJ(kOBJ)%plane(1:3,0)=points3D(1:3,1)
        tOBJ(kOBJ)%plane(1:3,1)=points3D(1:3,2)-points3D(1:3,1)
        tOBJ(kOBJ)%plane(1:3,2)=points3D(1:3,3)-points3D(1:3,1)
        tOBJ(kOBJ)%plane(1:3,3)=r3Vec_Prod(tOBJ(kOBJ)%plane(1:3,1),tOBJ(kOBJ)%plane(1:3,2))
        call Ortho3DSpace(tOBJ(kOBJ)%plane)
        tOBJ(kOBJ)%O(1)=0.0d0
        tOBJ(kOBJ)%O(2)=0.0d0
        tOBJ(kOBJ)%O(3)=r3Vec_Length(points3D(1:3,2)-points3D(1:3,1))
        tOBJ(kOBJ)%e(1)=0.0d0
        call Proj3D(points3D(1:3,3),tOBJ(kOBJ)%plane,points3D(1:3,3),pBig,tOBJ(kOBJ)%e(2),tOBJ(kOBJ)%e(3),dum)
      end if
	  Case(idc_OBJECT_Points3)
      if(tOBJ(kOBJ)%iTypO.eq.3) then
        call GetObjectData(Dlg,.false.)
        points3D(1,1)=tOBJ(kOBJ)%O(1)
        points3D(2,1)=tOBJ(kOBJ)%O(2)
        points3D(1,2)=tOBJ(kOBJ)%O(3)
        points3D(2,2)=tOBJ(kOBJ)%e(1)
        points3D(1,3)=tOBJ(kOBJ)%e(2)
        points3D(2,3)=tOBJ(kOBJ)%e(3)
        points3D(3,1:3)=0.0d0
        call vLoc2Glob(points3D(1:3,1),tOBJ(kOBJ)%plane,points3D(1:3,1))
        call vLoc2Glob(points3D(1:3,2),tOBJ(kOBJ)%plane,points3D(1:3,2))
        call vLoc2Glob(points3D(1:3,3),tOBJ(kOBJ)%plane,points3D(1:3,3))
        Points3DText='Corners in 3D space'C
        iPoints3DRead=3
        call Points3DDialog(.true.)
        tOBJ(kOBJ)%plane(1:3,0)=points3D(1:3,1)
        tOBJ(kOBJ)%plane(1:3,1)=points3D(1:3,2)-points3D(1:3,1)
        tOBJ(kOBJ)%plane(1:3,2)=points3D(1:3,3)-points3D(1:3,1)
        tOBJ(kOBJ)%plane(1:3,3)=r3Vec_Prod(tOBJ(kOBJ)%plane(1:3,1),tOBJ(kOBJ)%plane(1:3,2))
        call Ortho3DSpace(tOBJ(kOBJ)%plane)
        tOBJ(kOBJ)%O(1)=0.0d0
        tOBJ(kOBJ)%O(2)=0.0d0
        tOBJ(kOBJ)%O(3)=r3Vec_Length(points3D(1:3,2)-points3D(1:3,1))
        tOBJ(kOBJ)%e(1)=0.0d0
        call Proj3D(points3D(1:3,3),tOBJ(kOBJ)%plane,points3D(1:3,3),pBig,tOBJ(kOBJ)%e(2),tOBJ(kOBJ)%e(3),dum)
      end if
	  Case(idc_OBJECT_read)
      call GetObjectData(Dlg,.false.)
      call OpenObject(.false.)
      call SetObjectData(Dlg)
	  Case(idc_OBJECT_write)
      call GetObjectData(Dlg,.false.)
      call SaveObject(.false.)
      call SetObjectData(Dlg)
	  Case(idcancel)
      call GetObjectData(Dlg,.false.)
      if(lGRCused.eq.lObjHid) then
        lGRCused=.not.lObjHid
        call GRCclear()
      end if
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateObject

  Subroutine PETDialog(lCheck)
! Dialog for the PET basis
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical ldum,lCheck
	  Type(dialog) Dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_PET,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'PET dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetPETData(Dlg)
      ldum=DlgSetSub(Dlg,idc_PET_KPETS,updatePET)
      ldum=DlgSetSub(Dlg,idc_PET_KPET,updatePET)
      ldum=DlgSetSub(Dlg,idc_PET_check,updatePET)
      ldum=DlgSetSub(Dlg,idc_PET_read,updatePET)
      ldum=DlgSetSub(Dlg,idc_PET_write,updatePET)
      ldum=DlgSetSub(Dlg,idcancel,updatePET)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine PETDialog

  Subroutine SetPETData(Dlg)
! set dialog boxes containing PET data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
	  call DlgSetI(Dlg,idc_PET_kPET,idc_PET_kPETS,kPETD,1,10)
    call DlgSetS0(Dlg,IDC_PET_SPET,sPET(kPETD))
  end Subroutine SetPETData

  Subroutine GetPETData(Dlg)
! get PET data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
	  call DlgGetI(Dlg,idc_PET_kPET,idc_PET_kPETS,0,kPETD,1,mPET,1)
    call DlgGetS(Dlg,IDC_PET_SPET,sPET(kPETD))
  end Subroutine GetPETData

	Subroutine updatePET(Dlg,control_name,callbackType)
! callback for PETDialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum,ie,lfdum
	  Type (dialog) Dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_PET_KPET)
	    call DlgGetI(Dlg,idc_PET_kPET,idc_PET_kPETS,0,kPETD,1,mPET,1)
      call SetPETData(Dlg)
	  Case(idc_PET_KPETS)
	    call DlgGetI(Dlg,idc_PET_kPET,idc_PET_kPETS,1,kPETD,1,mPET,1)
      call SetPETData(Dlg)
	  Case(idc_PET_check)
      call GetPETData(Dlg)
      ie=0
      do kPETD=1,20
        call checkCFormula(sPET(kPETD),lfdum,0,0,1,idum)
        if(idum.ne.0) ie=ie+1
      end do
      kPETD=1
      call SetPETData(Dlg)
      if(ie.eq.0) then
        idum=MessageBoxQQ('No errors found!'C,'PET basis dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Errors found!\rCheck formula with leading exclamation mark!'C,&
        &                 'PET basis dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      call SetPETData(Dlg)
	  Case(idc_PET_read)
      call GetPETData(Dlg)
      call OpenBasis(.false.)
      call SetPETData(Dlg)
	  Case(idc_PET_write)
      call GetPETData(Dlg)
      call SaveBasis(.false.)
      call SetPETData(Dlg)
	  Case(idcancel)
      call GetPETData(Dlg)
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updatePET

END MODULE CHDIB
