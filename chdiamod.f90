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
MODULE CHDIA

! Objects

  USE CHDIB

  SAVE

  CONTAINS

  Subroutine BoundaryDialog(lCheck)
! Dialog for setting parameters of the various boundaries
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical ldum,lCheck
	  Type(dialog) Dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Boundary,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Boundary dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetBoundaryData(Dlg)
      ldum=DlgSetSub(Dlg,idc_bnd_kBnd,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_kBndS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_Color,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_ColorS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_RDom,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_RDomS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_LDom,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_LDomS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_kVal,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_kValS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_kEdge,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_kEdgeS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_bloPoly,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_movPoly,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_rotPoly,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_insPoly,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_insPolyE,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_delPoly,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_delPolyE,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_modPoly,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_modPolyE,updateBoundary)
	    ldum=DlgSetSub(dlg,idc_bnd_check,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_read,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_write,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_MaS,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_Geo,updateBoundary)
      ldum=DlgSetSub(Dlg,idc_bnd_draw,updateBoundary)
      ldum=DlgSetSub(Dlg,idcancel,updateBoundary)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine BoundaryDialog

  Subroutine SetBoundaryData(Dlg)
! set dialog boxes containing boundary data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) kE
    Logical ldum,lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz
	  Type(dialog) Dlg
    call cBndGetABO()
    kBnd=max(1,min(kBnd,nBnd))
    call DlgSetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,kBnd,1,nBnd)
    call DlgSetI(Dlg,idc_bnd_kVal,idc_bnd_kValS,kVal,1,2)
    call DlgSetI(Dlg,idc_bnd_Conn,0,Int4(tBnd(kBnd)%iConn),-32000,32000)
    call DlgSetI(Dlg,idc_bnd_Color,idc_bnd_ColorS,Int4(tBnd(kBnd)%iCol),0,235)
    call DlgSetI(Dlg,idc_bnd_RDom,idc_bnd_RDomS,Int4(tBnd(kBnd)%iRDom),-9,150)
    call DlgSetI(Dlg,idc_bnd_LDom,idc_bnd_LDomS,Int4(tBnd(kBnd)%iLDom),-9,150)
    call DlgSetI(Dlg,idc_bnd_nBnd,0,nBnd,1,1000)
    call DlgSetI(Dlg,idc_bnd_Pts,0,tBnd(kBnd)%nSpline,-1000000,1000000)
    call DlgSetI(Dlg,idc_bnd_MatPts,0,tBnd(kBnd)%nMatPts,-1000000,1000000)
    call DlgSetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge)
    call DlgSetI(Dlg,idc_bnd_nEdge,0,tBnd(kBnd)%nEdge,1,1000)
    ldum=.false.
    if(iiabs(tBnd(kBnd)%iTypB).eq.1_2) ldum=.true.
    call DlgSetL(Dlg,idc_bnd_Closed,ldum)
    call getBndCnd(tBnd(kBnd)%iCond,lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz)
    call GetBndBC(kBnd)
    if(lusual) call DlgSetL(Dlg,idc_bnd_Lusual,lusual)
    if(lspec) call DlgSetL(Dlg,idc_bnd_Lspec,lspec)
    if(lxper) call DlgSetL(Dlg,idc_bnd_lxper,lxper)
    if(lyper) call DlgSetL(Dlg,idc_bnd_lyper,lyper)
    if(lzper) call DlgSetL(Dlg,idc_bnd_lzper,lzper)
    if(lsibc) call DlgSetL(Dlg,idc_bnd_Lsibc,lsibc)
    call DlgSetL(Dlg,idc_bnd_Let,let)
    call DlgSetL(Dlg,idc_bnd_Lez,lez)
    call DlgSetL(Dlg,idc_bnd_Ldn,ldn)
    call DlgSetL(Dlg,idc_bnd_Lht,lht)
    call DlgSetL(Dlg,idc_bnd_Lhz,lhz)
    call DlgSetL(Dlg,idc_bnd_Lbn,lbn)
    call DlgSetL(Dlg,idc_bnd_Laz,laz)
    call DlgSetL(Dlg,idc_bnd_Lv ,lvz)
	  call DlgSetR(Dlg,idc_bnd_weight,tBnd(kBnd)%Weight,4)
	  call DlgSetR(Dlg,idc_bnd_w,tBnd(kBnd)%Weight2,4)
	  call DlgSetR(Dlg,idc_bnd_val,tBnd(kBnd)%val(kVal),7)
	  call DlgSetR(Dlg,idc_bnd_amp,tBnd(kBnd)%fAmpl,7)
    kE=Min(nBndEdg,Max(0,tBnd(kBnd)%kEdge+tBnd(kBnd)%iEdgeOffset))
	  call DlgSetR(Dlg,idc_bnd_x,tBndEdg(kE)%x,10)
	  call DlgSetR(Dlg,idc_bnd_y,tBndEdg(kE)%y,10)
	  call DlgSetR(Dlg,idc_bnd_r,tBndEdg(kE)%r,10)
	  call DlgSetR(Dlg,idc_bnd_d,tBndEdg(kE)%d,10)
	  call DlgSetR(Dlg,idc_bnd_xa,tBndEdg(kE)%xa,7)
	  call DlgSetR(Dlg,idc_bnd_ya,tBndEdg(kE)%ya,7)
	  call DlgSetR(Dlg,idc_bnd_xb,tBndEdg(kE)%xb,7)
	  call DlgSetR(Dlg,idc_bnd_yb,tBndEdg(kE)%yb,7)
	  call DlgSetR(Dlg,idc_bnd_xo,tBndEdg(kE)%xo,7)
	  call DlgSetR(Dlg,idc_bnd_yo,tBndEdg(kE)%yo,7)
	  call DlgSetS0(Dlg,idc_bnd_form,tBnd(kBnd)%Formula)
  end Subroutine SetBoundaryData

  Subroutine GetBoundaryData(Dlg)
! get boundary data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) kE
    Logical ldum,lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz
	  Type(dialog) Dlg
    if(iGetData.ge.0) then
	    call DlgGetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,0,kBnd,1,nBnd,nBnd)
	    call DlgGetI(Dlg,idc_bnd_kVal,idc_bnd_kValS,0,kVal,1,2,1)
      call DlgGetI(Dlg,idc_bnd_Pts,0,0,tBnd(kBnd)%nSpline,-1000000,1000000,2)
      call DlgGetI(Dlg,idc_bnd_MatPts,0,0,tBnd(kBnd)%nMatPts,-1000000,1000000,2)
      call DlgGetI(Dlg,idc_bnd_Conn,0,0,kE,-32000,32000,0)
      tBnd(kBnd)%iConn=Int2(kE)
      call DlgGetI(Dlg,idc_bnd_Color,idc_bnd_ColorS,0,kE,0,235,1)
      tBnd(kBnd)%iCol=Int2(kE)
      call DlgGetI(Dlg,idc_bnd_RDom,idc_bnd_RDomS,0,kE,-9,150,0)
      tBnd(kBnd)%iRDom=Int2(kE)
      call DlgGetI(Dlg,idc_bnd_LDom,idc_bnd_LDomS,0,kE,-9,150,1)
      tBnd(kBnd)%iLDom=Int2(kE)
      call DlgGetL(Dlg,idc_bnd_Closed,ldum)
      tBnd(kBnd)%iTypB=2_2
      if(ldum) tBnd(kBnd)%iTypB=1_2
      call DlgGetL(Dlg,idc_bnd_Lusual,lusual)
      call DlgGetL(Dlg,idc_bnd_Lspec,lspec)
      call DlgGetL(Dlg,idc_bnd_lxper,lxper)
      call DlgGetL(Dlg,idc_bnd_lyper,lyper)
      call DlgGetL(Dlg,idc_bnd_lzper,lzper)
      call DlgGetL(Dlg,idc_bnd_Lsibc,lsibc)
      call DlgGetL(Dlg,idc_bnd_Let,let)
      call DlgGetL(Dlg,idc_bnd_Lez,lez)
      call DlgGetL(Dlg,idc_bnd_Ldn,ldn)
      call DlgGetL(Dlg,idc_bnd_Lht,lht)
      call DlgGetL(Dlg,idc_bnd_Lhz,lhz)
      call DlgGetL(Dlg,idc_bnd_Lbn,lbn)
      call DlgGetL(Dlg,idc_bnd_Laz,laz)
      call DlgGetL(Dlg,idc_bnd_Lv ,lvz)
      call setBndCnd(tBnd(kBnd)%iCond,lusual,lspec,lxper,lyper,lzper,lsibc,let,lez,ldn,lht,lhz,lbn,laz,lvz)
      call GetBndBC(kBnd)
	    call DlgGetR(Dlg,idc_bnd_weight,tBnd(kBnd)%Weight,nBig,pBig,1.0d0,4)
	    call DlgGetR(Dlg,idc_bnd_w,tBnd(kBnd)%Weight2,nBig,pBig,1.0d0,4)
	    call DlgGetR(Dlg,idc_bnd_val,tBnd(kBnd)%val(kVal),nBig,1.0d301,0.0d0,7)
	    call DlgGetR(Dlg,idc_bnd_amp,tBnd(kBnd)%fAmpl,nBig,pBig,0.0d0,7)
      if(tBnd(kBnd)%nSpline.gt.0) tBnd(kBnd)%iTypB=-tBnd(kBnd)%iTypB 
	    call DlgGetS(Dlg,idc_bnd_form,tBnd(kBnd)%Formula)
    end if
    if(iGetData.le.0) then
	    call DlgGetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,0,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge,tBnd(kBnd)%nEdge)
      kE=Min(nBndEdg,Max(0,tBnd(kBnd)%kEdge+tBnd(kBnd)%iEdgeOffset))
	    call DlgGetR(Dlg,idc_bnd_x,tBndEdg(kE)%x,nBig,pBig,0.0d0,10)
	    call DlgGetR(Dlg,idc_bnd_y,tBndEdg(kE)%y,nBig,pBig,0.0d0,10)
	    call DlgGetR(Dlg,idc_bnd_r,tBndEdg(kE)%r,nBig,pBig,0.0d0,10)
	    call DlgGetR(Dlg,idc_bnd_d,tBndEdg(kE)%d,nBig,pBig,0.0d0,10)
    end if
    call cBndGetABO()
  end Subroutine GetBoundaryData

	Subroutine updateBoundary(Dlg,control_name,callbackType)
! callback for BoundaryDialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum,ie,kB,kE,lfdum
    Logical ldum
	  Type (dialog) Dlg
	  idum=callbackType
    iGetData=0
	  Select Case(control_name)
	  Case(idc_bnd_Color)
      call DlgGetI(Dlg,idc_bnd_Color,idc_bnd_ColorS,0,idum,0,235,1)
      call DlgSetI(Dlg,idc_bnd_Color,idc_bnd_ColorS,idum,0,235)
	  Case(idc_bnd_ColorS)
      call DlgGetI(Dlg,idc_bnd_Color,idc_bnd_ColorS,1,idum,0,235,1)
      call DlgSetI(Dlg,idc_bnd_Color,idc_bnd_ColorS,idum,0,235)
	  Case(idc_bnd_RDom)
      call DlgGetI(Dlg,idc_bnd_RDom,idc_bnd_RDomS,0,idum,-9,150,0)
      call DlgSetI(Dlg,idc_bnd_RDom,idc_bnd_RDomS,idum,-9,150)
	  Case(idc_bnd_RDomS)
      call DlgGetI(Dlg,idc_bnd_RDom,idc_bnd_RDomS,1,idum,-9,150,0)
      call DlgSetI(Dlg,idc_bnd_RDom,idc_bnd_RDomS,idum,-9,150)
	  Case(idc_bnd_LDom)
      call DlgGetI(Dlg,idc_bnd_LDom,idc_bnd_LDomS,0,idum,-9,150,1)
      call DlgSetI(Dlg,idc_bnd_LDom,idc_bnd_LDomS,idum,-9,150)
	  Case(idc_bnd_LDomS)
      call DlgGetI(Dlg,idc_bnd_LDom,idc_bnd_LDomS,1,idum,-9,150,1)
      call DlgSetI(Dlg,idc_bnd_LDom,idc_bnd_LDomS,idum,-9,150)
	  Case(idc_bnd_kBnd)
	    call DlgGetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,0,kBnd,1,nBnd,1)
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_kBndS)
	    call DlgGetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,1,kBnd,1,nBnd,1)
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_kVal)
	    call DlgGetI(Dlg,idc_bnd_kVal,idc_bnd_kValS,0,kVal,1,2,1)
      call DlgSetI(Dlg,idc_bnd_kVal,0,kVal,1,2)
	    call DlgSetR(Dlg,idc_bnd_val,tBnd(kBnd)%val(kVal),7)
	  Case(idc_bnd_kValS)
	    call DlgGetI(Dlg,idc_bnd_kVal,idc_bnd_kValS,1,kVal,1,2,1)
      call DlgSetI(Dlg,idc_bnd_kVal,0,kVal,1,2)
	    call DlgSetR(Dlg,idc_bnd_val,tBnd(kBnd)%val(kVal),7)
	  Case(idc_bnd_kEdge)
	    call DlgGetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,0,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge,tBnd(kBnd)%nEdge)
      idum=Min(nBndEdg,Max(0,tBnd(kBnd)%kEdge+tBnd(kBnd)%iEdgeOffset))
      tBnd(kBnd)%kEdge=idum-tBnd(kBnd)%iEdgeOffset
      call DlgSetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge)
	    call DlgSetR(Dlg,idc_bnd_x,tBndEdg(idum)%x,10)
	    call DlgSetR(Dlg,idc_bnd_y,tBndEdg(idum)%y,10)
	    call DlgSetR(Dlg,idc_bnd_r,tBndEdg(idum)%r,10)
	    call DlgSetR(Dlg,idc_bnd_d,tBndEdg(idum)%d,10)
	    call DlgSetR(Dlg,idc_bnd_xa,tBndEdg(idum)%xa,7)
	    call DlgSetR(Dlg,idc_bnd_ya,tBndEdg(idum)%ya,7)
	    call DlgSetR(Dlg,idc_bnd_xb,tBndEdg(idum)%xb,7)
	    call DlgSetR(Dlg,idc_bnd_yb,tBndEdg(idum)%yb,7)
	    call DlgSetR(Dlg,idc_bnd_xo,tBndEdg(idum)%xo,7)
	    call DlgSetR(Dlg,idc_bnd_yo,tBndEdg(idum)%yo,7)
	  Case(idc_bnd_kEdgeS)
	    call DlgGetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,1,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge,tBnd(kBnd)%nEdge)
      idum=Min(nBndEdg,Max(0,tBnd(kBnd)%kEdge+tBnd(kBnd)%iEdgeOffset))
      tBnd(kBnd)%kEdge=idum-tBnd(kBnd)%iEdgeOffset
      call DlgSetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge)
	    call DlgSetR(Dlg,idc_bnd_x,tBndEdg(idum)%x,10)
	    call DlgSetR(Dlg,idc_bnd_y,tBndEdg(idum)%y,10)
	    call DlgSetR(Dlg,idc_bnd_r,tBndEdg(idum)%r,10)
	    call DlgSetR(Dlg,idc_bnd_d,tBndEdg(idum)%d,10)
	    call DlgSetR(Dlg,idc_bnd_xa,tBndEdg(idum)%xa,7)
	    call DlgSetR(Dlg,idc_bnd_ya,tBndEdg(idum)%ya,7)
	    call DlgSetR(Dlg,idc_bnd_xb,tBndEdg(idum)%xb,7)
	    call DlgSetR(Dlg,idc_bnd_yb,tBndEdg(idum)%yb,7)
	    call DlgSetR(Dlg,idc_bnd_xo,tBndEdg(idum)%xo,7)
	    call DlgSetR(Dlg,idc_bnd_yo,tBndEdg(idum)%yo,7)
	  Case(idc_bnd_bloPoly)
      TransText(1)='Center, x-coord'C
      TransText(2)='Center, y-coord'C
      TransText(3)='Blow factor'C
      call TransDialog(.true.)
      if(lTrans) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call BlowBoundary(kBnd,trans(1),trans(2),trans(3))
        call SetBoundaryData(Dlg)
      end if
	  Case(idc_bnd_movPoly)
      TransText(1)='Delta x'C
      TransText(2)='Delta y'C
      TransText(3)='unused'C
      call TransDialog(.true.)
      if(lTrans) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call MoveBoundary(kBnd,trans(1),trans(2))
        call SetBoundaryData(Dlg)
      end if
	  Case(idc_bnd_rotPoly)
      TransText(1)='Center, x-coord'C
      TransText(2)='Center, y-coord'C
      TransText(3)='Rotation angle'C
      call TransDialog(.true.)
      if(lTrans) then
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call RotateBoundary(kBnd,trans(1),trans(2),trans(3),.true.)
        call SetBoundaryData(Dlg)
      end if
	  Case(idc_bnd_delPoly)
      if(nBnd.gt.1) then
        idum=kBnd
	      call DlgGetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,0,kBnd,1,nBnd,nBnd)
        call InsertBnd(kBnd,-1,ldum)
        kBnd=min(idum,nBnd)
        call SetBoundaryData(Dlg)
      end if
	  Case(idc_bnd_insPoly)
	    call DlgGetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,0,kBnd,1,nBnd,nBnd)
      nInsObj=nBnd
      kInsObj=kBnd
      call InsertDialog(.false.)
      if(kInsObj.ge.0) then
        call copyBnd(kBnd,kInsObj,0.0d0,0.0d0,-2_4,ldum)
      end if
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_insPolyE,idc_bnd_delPolyE)
	    call DlgGetI(Dlg,idc_bnd_kBnd,idc_bnd_kBndS,0,kBnd,1,nBnd,nBnd)
	    call DlgGetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,0,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge,tBnd(kBnd)%nEdge)
      kE=tBnd(kBnd)%kEdge
      if(control_name.eq.idc_bnd_insPolyE) then
        idum=1
      else
        idum=-1
        if(tBnd(kBnd)%nEdge.lt.2) idum=0
      end if
      call InsertBndEdg(kBnd,tBnd(kBnd)%kEdge,idum,ldum)
      if(control_name.eq.idc_bnd_insPolyE) then
        iGetData=-1
        tBnd(kBnd)%kEdge=kE+1
        call DlgSetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge)
        call GetBoundaryData(Dlg)
      end if
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_modPoly)
      call GetBoundaryData(Dlg)
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_modPolyE)
      iGetData=-1
      call GetBoundaryData(Dlg)
      idum=Min(nBndEdg,Max(0,tBnd(kBnd)%kEdge+tBnd(kBnd)%iEdgeOffset))
      tBnd(kBnd)%kEdge=idum-tBnd(kBnd)%iEdgeOffset
      call DlgSetI(Dlg,idc_bnd_kEdge,idc_bnd_kEdgeS,tBnd(kBnd)%kEdge,1,tBnd(kBnd)%nEdge)
	    call DlgSetR(Dlg,idc_bnd_x,tBndEdg(idum)%x,10)
	    call DlgSetR(Dlg,idc_bnd_y,tBndEdg(idum)%y,10)
	    call DlgSetR(Dlg,idc_bnd_r,tBndEdg(idum)%r,10)
	    call DlgSetR(Dlg,idc_bnd_d,tBndEdg(idum)%d,10)
	    call DlgSetR(Dlg,idc_bnd_xa,tBndEdg(idum)%xa,7)
	    call DlgSetR(Dlg,idc_bnd_ya,tBndEdg(idum)%ya,7)
	    call DlgSetR(Dlg,idc_bnd_xb,tBndEdg(idum)%xb,7)
	    call DlgSetR(Dlg,idc_bnd_yb,tBndEdg(idum)%yb,7)
	    call DlgSetR(Dlg,idc_bnd_xo,tBndEdg(idum)%xo,7)
	    call DlgSetR(Dlg,idc_bnd_yo,tBndEdg(idum)%yo,7)
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_check)
      call GetBoundaryData(dlg)
      ie=0
      kB=kBnd
      do kBnd=1,nBnd
        cForm(0)=0.0d0
        cForm(1)=Pi
        vForm(0)=0.789123456 ! any value between 0 and 1 is OK
        vForm(1)=vForm(0)
        call checkFormula(tBnd(kBnd)%Formula,lfdum,1,0,1,idum)
        if(idum.ne.0) ie=ie+1
      end do
      kBnd=kB
      if(ie.eq.0) then
        idum=MessageBoxQQ('No errors found!'C,'Boundary dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Errors found!\rCheck formula with leading exclamation mark!'C, &
        &                 'Boundary dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      call SetBoundaryData(dlg)
	  Case(idc_bnd_read)
      call OpenBoundary(.false.)
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_write)
      call SaveBoundary(.false.)
      call SetBoundaryData(Dlg)
	  Case(idc_bnd_MaS)
      call SaveMaS(.false.)
	  Case(idc_bnd_Geo)
      call GeoDialog(.false.)
	  Case(idc_bnd_draw)
      call GetBoundaryData(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        iDraBnd=kBnd
        iDomBnd=0_2
        iColBnd=0_2
        iConBnd=0_2
        call TDrawBoundary(.true.)
      end if
	  Case(idcancel)
      call GetBoundaryData(Dlg)
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateBoundary

  Subroutine GeoDialog(lCheck)
! dialog for the GMSH-Geo data
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
	  ldum=DlgInit(IDD_Geo,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'GMSH geometry dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetGeoData(dlg)
	    ldum=DlgSetSub(dlg,idok,updateGeo)
	    ldum=DlgSetSub(dlg,idcancel,updateGeo)
	    ldum=DlgSetSub(dlg,idc_write_geo,updateGeo)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine GeoDialog

  Subroutine SetGeoData(dlg)
! set the GMSH geometry data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    kDom=max(1,min(kDom,nDom))
    call DlgSetI(Dlg,idc_Geo_Bnd1,0,iGMSHbnd1,1,nBnd)
    call DlgSetI(Dlg,idc_Geo_Bnd2,0,iGMSHbnd2,1,nBnd)
    call DlgSetI(Dlg,idc_Geo_Dom,0,iGMSHdom,-nDom,nDom)
    call DlgSetI(Dlg,idc_Geo_Col,0,iGMSHCol,-235,235)
  end Subroutine SetGeoData

  Subroutine GetGeoData(dlg)
! get the GMSH geometry from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
	  call DlgGetI(Dlg,idc_Geo_Bnd1,0,0,iGMSHbnd1,1,nBnd,1)
	  call DlgGetI(Dlg,idc_Geo_Bnd2,0,0,iGMSHbnd2,1,nBnd,nBnd)
	  call DlgGetI(Dlg,idc_Geo_Dom,0,0,iGMSHdom,-nDom,nDom,0)
	  call DlgGetI(Dlg,idc_Geo_Col,0,0,iGMSHCol,-235,235,0)
  end Subroutine GetGeoData

  Subroutine updateGeo(dlg,control_name,callbackType)
! callback for GeoDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idcancel)
	    call DlgExit(dlg)
    Case(idok)
      call GetGeoData(dlg)
	    call DlgExit(dlg)
    Case(idc_write_geo)
      call GetGeoData(dlg)
      call GMSHwriteGeoFile(1,GeoFileName,iGMSHbnd1,iGMSHbnd2,Int2(iGMSHdom),Int2(iGMSHcol))
	    call DlgExit(dlg)
    end Select
  end Subroutine updateGeo

  Subroutine DomainDialog(lCheck)
! dialog for the domain data
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
	  ldum=DlgInit(IDD_Domain,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Domain dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetDomainData(dlg)
	    ldum=DlgSetSub(dlg,idc_kDom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_kDomS,updateDomain)
	    ldum=DlgSetSub(dlg,idc_Add_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_Del_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_Adj_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_Draw_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_Chk_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_read_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idc_write_Dom,updateDomain)
	    ldum=DlgSetSub(dlg,idcancel,updateDomain)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine DomainDialog

  Subroutine SetDomainData(dlg)
! set the Domain data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    kDom=max(1,min(kDom,nDom))
    call DlgSetI(Dlg,idc_kDom,idc_kDomS,kDom,1,nDom)
    call DlgSetI(Dlg,idc_nDom,0,nDom,1,10000)
    call DlgSetI(Dlg,idc_kDomColor,0,Int4(iDom(kDom)),0,235)
	  call DlgSetS0(Dlg,idc_eDom,Dom_Form(1,kDom))
	  call DlgSetS0(Dlg,idc_uDom,Dom_Form(2,kDom))
	  call DlgSetS0(Dlg,idc_sDom,Dom_Form(3,kDom))
	  call DlgSetS0(Dlg,idc_tDom,Dom_Form(4,kDom))
  end Subroutine SetDomainData

  Subroutine GetDomainData(dlg)
! get the Domain data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Type(dialog) dlg
	  call DlgGetI(Dlg,idc_kDom,idc_kDomS,0,kDom,1,nDom,1)
	  call DlgGetI(Dlg,idc_kDomColor,0,0,idum,0,235,1)
    iDom(0)=Int2(idum)
		call DlgGetS(Dlg,idc_eDom,Dom_Form(1,kDom))
		call DlgGetS(Dlg,idc_uDom,Dom_Form(2,kDom))
		call DlgGetS(Dlg,idc_sDom,Dom_Form(3,kDom))
		call DlgGetS(Dlg,idc_tDom,Dom_Form(4,kDom))
  end Subroutine GetDomainData

  Subroutine updateDomain(dlg,control_name,callbackType)
  ! callback for DomainDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum,ie,kD,lfdum,id,lf,i
    Logical ldum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idc_kDom)
	    call DlgGetI(Dlg,idc_kDom,idc_kDomS,0,kDom,1,nDom,1)
      call SetDomainData(dlg)
    Case(idc_kDomS)
	    call DlgGetI(Dlg,idc_kDom,idc_kDomS,1,kDom,1,nDom,1)
      call SetDomainData(dlg)
    Case(idc_Chk_Dom)
      call GetDomainData(dlg)
      ie=0
      kD=kDom
      do kDom=1,nDom
        do id=1,4
          lf=-1
          call DelBlanks(Dom_Form(id,kDom),lf)
          call ToLower(Dom_Form(id,kDom),lf)
          if(Dom_Form(id,kDom)(1:1).eq.'#') then
            inquire(file=Dom_Form(id,kDom)(2:lf),Exist=ldum)
            if(.not.ldum) then
              do i=lf,2,-1
                Dom_Form(id,kDom)(i:i)=Dom_Form(id,kDom)(i-1:i-1)
              end do
              Dom_Form(id,kDom)(1:1)='!'
              ie=ie+1
            end if
          else
            call checkCFormula(Dom_Form(id,kDom),lfdum,5,0,5,idum)
            if(idum.ne.0) ie=ie+1
          end if
        end do
      end do
      kDom=kD
      if(ie.eq.0) then
        idum=MessageBoxQQ('No errors found!'C,'Domain dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Errors found!\rCheck formula with leading exclamation mark!'C, &
        &                 'Domain dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      call SetDomainData(dlg)
    Case(idc_Add_Dom)
      ldum=.true.
      if(nDom.ge.mDom) call IncreaseDom(1,ldum)
      if(ldum) then
        nDom=nDom+1
        kDom=nDom
		    call DlgGetS(Dlg,idc_eDom,Dom_Form(1,kDom))
		    call DlgGetS(Dlg,idc_uDom,Dom_Form(2,kDom))
		    call DlgGetS(Dlg,idc_sDom,Dom_Form(3,kDom))
		    call DlgGetS(Dlg,idc_tDom,Dom_Form(4,kDom))
	      call DlgGetI(Dlg,idc_kDomColor,0,0,idum,0,235,1)
        iDom(0)=Int2(idum)
        iDom(kDom)=iDom(0)
      end if
      call SetDomainData(dlg)
    Case(idc_Del_Dom)
      call GetDomainData(dlg)
      if(nDom.gt.1) then
        nDom=nDom-1
        do idum=kDom,nDom
          iDom(idum)=iDom(idum+1)
          Dom_Form(1:4,idum)=Dom_Form(1:4,idum+1)
        end do
      end if
      call SetDomainData(dlg)
    Case(idc_Adj_Dom)
      call GetDomainData(dlg)
      iDom(kDom)=iDom(0)
      call SetDomainData(dlg)
    Case(idc_Draw_Dom)
      ! call GetDomainData(dlg)
      if(.not.lThreadStarted) then
        call getEUST()
	      call DlgExit(dlg)
        iDraBnd=0
        iDomBnd=Int2(kDom)
        iColBnd=0_2
        iConBnd=0_2
        call DrawBoundary(0)
        iDomBnd=0_2
        iDraExp=0
        iDomExp=Int2(kDom)
        iColExp=0_2
        iConExp=0_2
        call DrawExpansion(0)
        iDomExp=0_2
      end if
    Case(idc_read_dom)
      ! call GetDomainData(dlg)
      call OpenDomain(.false.)
      call SetDomainData(dlg)
    Case(idc_write_dom)
      ! call GetDomainData(dlg)
      call SaveDomain(.false.)
      call SetDomainData(dlg)
    Case(idcancel)
      ! call GetDomainData(dlg)
      call getEUST()
      iDom(0)=0_2
	    call DlgExit(dlg)
    end Select
  end Subroutine updateDomain

  Subroutine ExpansionDialog(lCheck)
! Dialog for setting parameters of the various expansions
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical ldum,lCheck
	  Type(dialog) Dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
    if(kExp.gt.nExp) then
      kExp=nExp
    end if
	  ldum=lCheck
	  ldum=DlgInit(IDD_Expansion,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Expansion dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetExpansionData(Dlg)
      ldum=DlgSetSub(Dlg,idc_Exp_kExp,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_kExc,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_nExc,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_kPar,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_iCol,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_iDom,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_iConn,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_kExpS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_kExcS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_kParS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_iColS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_iDomS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_iConnS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_siExpS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_srExpS,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_LE,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_LH,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_LHE,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_lConn,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l2DPol,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l2DBessel,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_lPlW,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l2DHarmonic,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3DPol,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3DBessel,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3DRing,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3DLine,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3DSpiral,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3DGauss,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l2Dlayer,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_l3Dlayer,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_copExp,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_insExp,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_delExp,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_delDepend,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_getDepend,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_all0,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_all1,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_GenExpO,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_GenExp,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_ModExp,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_Loc,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_Axis,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_read,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_write,updateExpansion)
      ldum=DlgSetSub(Dlg,idc_Exp_draw,updateExpansion)
      ldum=DlgSetSub(Dlg,idcancel,updateExpansion)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine ExpansionDialog

  Subroutine SetExpansionData(Dlg)
! set dialog boxes containing Expansion data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) kP,idum
    Integer(2) id2
    Logical ldum
	  Type(dialog) Dlg
    if(lgcFld) then
      srExp(1,0:3)='Angle (XY-plane)'C
      srExp(1,5)='Angle (XY-plane)'C
      srExp(2,4)='Min y'C
      siExp(3,4)='Minimum x order'C
    else
      srExp(1,0:3)='Unused'C
      srExp(1,5)='Unused'C
      srExp(2,4)='Min z'C
      siExp(3,4)='Maximum y order'C
    end if
    if((tExp(kExp)%iTypE.lt.6).or.(tExp(kExp)%iTypE.eq.10).or.(tExp(kExp)%iTypE.eq.12)) then
      ldum=DlgSet(Dlg,idc_Exp_Origx,.true.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Exp_Origy,.true.,DLG_ENABLE)
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kExp)%xo=tExp(kExp)%Plane(1,0)
        tExp(kExp)%yo=tExp(kExp)%Plane(2,0)
      end if
    else
      ldum=DlgSet(Dlg,idc_Exp_Origx,.false.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Exp_Origy,.false.,DLG_ENABLE)
    end if
    kExp=max(1,min(kExp,nExp))
    call DlgSetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,kExp,1,nExp)
    nRHS=max(1,min(nPar-1,nRHS))
    if(lEigen.and.(nRHS.gt.1)) then
      idum=MessageBoxQQ('No multiple excitations for eigenvalue problems!'C,'Set number of excitations'C, &
                        MB$OK.or.MB$IconExclamation)
      nRHS=1
    end if
    if(.not.lfcFld.and.(nRHS.gt.1)) then
      idum=MessageBoxQQ('Multiple excitations only for frequency domain problems!'C,'Set number of excitations'C, &
                        MB$OK.or.MB$IconExclamation)
      nRHS=1
    end if
    if((iMtrSolver.ne.0).and.(iMtrSolver.ne.1).and.(iMtrSolver.ne.5).and.(nRHS.gt.1)) then
      idum=MessageBoxQQ('Multiple excitations only for QR, GUT, GUR solvers!'C,'Set number of excitations'C, &
                        MB$OK.or.MB$IconExclamation)
      nRHS=1
    end if
    call DlgSetI(Dlg,idc_Exp_nExc,0,nRHS,1,nPar-1)
    kExc=max(1,min(kExc,nRHS))
    call DlgSetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,kExc,1,nRHS)
    call DlgSetI(Dlg,idc_Exp_iCol,idc_Exp_iColS,Int4(tExp(kExp)%iCol),0,235)
    call DlgSetI(Dlg,idc_Exp_iDom,idc_Exp_iDomS,Int4(tExp(kExp)%iDom),0,150)
    call DlgSetI(Dlg,idc_Exp_iConn,idc_Exp_iConnS,Int4(tExp(kExp)%iConn),-32000,32000)
    select case(tExp(kExp)%iTypE)
    case(0_2)
      call DlgSetL(dlg,idc_Exp_lConn,.true.)
    case(1_2)
      call DlgSetL(dlg,idc_Exp_l2DPol,.true.)
    case(2_2)
      call DlgSetL(dlg,idc_Exp_l2DBessel,.true.)
    case(3_2)
      call DlgSetL(dlg,idc_Exp_lPlW,.true.)
    case(4_2)
      call DlgSetL(dlg,idc_Exp_lRay,.true.)
    case(5_2)
      call DlgSetL(dlg,idc_Exp_l2DHarmonic,.true.)
    case(6_2)
      call DlgSetL(dlg,idc_Exp_l3DPol,.true.)
    case(7_2)
      call DlgSetL(dlg,idc_Exp_l3DBessel,.true.)
    case(8_2)
      call DlgSetL(dlg,idc_Exp_l3DRing,.true.)
    case(9_2)
      call DlgSetL(dlg,idc_Exp_l3DLine,.true.)
    case(10_2)
      call DlgSetL(dlg,idc_Exp_l3DSpiral,.true.)
    case(11_2)
      call DlgSetL(dlg,idc_Exp_l3DGauss,.true.)
    case(12_2)
      call DlgSetL(dlg,idc_Exp_l2Dlayer,.true.)
    case(13_2)
      call DlgSetL(dlg,idc_Exp_l3Dlayer,.true.)
    end select
    id2=igetiHE(kExp)
    if(id2.eq.0_2) then
      call DlgSetL(dlg,idc_Exp_lE,.true.)
    else if(id2.eq.1_2) then
      call DlgSetL(dlg,idc_Exp_lH,.true.)
    else if(id2.eq.2_2) then
      call DlgSetL(dlg,idc_Exp_lHE,.true.)
    end if
    id2=igetiCS(kExp)
    if(id2.eq.0_2) then
      call DlgSetL(dlg,idc_Exp_lCS,.true.)
    else if(id2.eq.1_2) then
      call DlgSetL(dlg,idc_Exp_lS,.true.)
    else if(id2.eq.2_2) then
      call DlgSetL(dlg,idc_Exp_lC,.true.)
    end if
    call DlgSetI(Dlg,0,idc_Exp_siexps,Int4(kiExp),1,6)
    call DlgSetI(Dlg,0,idc_Exp_srexps,Int4(krExp),1,5)
    call DlgSetS0(Dlg,idc_Exp_siexp,siExp(kiExp,tExp(kExp)%iTypE))
    call DlgSetS0(Dlg,idc_Exp_srexp,srExp(krExp,tExp(kExp)%iTypE))
    call DlgSetI(Dlg,idc_Exp_iexp,0,Int4(tExp(kExp)%iE(kiExp)),-30000,30000)
    call DlgSetR(Dlg,idc_Exp_rexp,tExp(kExp)%rE(krExp),10)
	  call DlgSetR(Dlg,idc_Exp_Gam1,Dble(tExp(kExp)%gc),10)
	  call DlgSetR(Dlg,idc_Exp_Gam2,DImag(tExp(kExp)%gc),10)
    call DlgSetR(Dlg,idc_Exp_Origx,tExp(kExp)%xo,10)
    call DlgSetR(Dlg,idc_Exp_Origy,tExp(kExp)%yo,10)
    call DlgSetR(Dlg,idc_Exp_Depend,tExp(kExp)%depend,10)
    call DlgSetR(Dlg,idc_Exp_DependMax,dep_delExp,10)
    call DlgSetI(Dlg,idc_Exp_nPar,0,Int4(tExp(kExp)%nPar),0,32767)
    call DlgSetI(Dlg,idc_Exp_nExp,0,nExp,1,32767)
    call DlgSetI(Dlg,idc_Exp_iObj,0,Int4(tExp(kExp)%iObj),-32000,32000)
    kP=kPar-tExp(kExp)%iOff
    if(kP.gt.tExp(kExp)%nPar) kP=tExp(kExp)%nPar
    if(kP.lt.1) kP=1
    kPar=kP+tExp(kExp)%iOff
    call DlgSetI(Dlg,idc_Exp_kPar,idc_Exp_kParS,Int4(kP),1,Int4(tExp(kExp)%nPar))
    call DlgSetI(Dlg,idc_Exp_iPar,0,Int4(iParExp(kExc,kPar)),-127,127)
	  call DlgSetR(Dlg,idc_Exp_Par1,Dble(ParExp(kExc,kPar)),10)
	  call DlgSetR(Dlg,idc_Exp_Par2,DImag(ParExp(kExc,kPar)),10)
	  call DlgSetR(Dlg,idc_Exp_Amp,cdAbs(ParExp(kExc,kPar)),7)
  end Subroutine SetExpansionData

  Subroutine GetExpansionData(Dlg,ls)
! get Expansion data
    Implicit none
    Include 'RESOURCE.FD'
    Real(8) dum1,dum2
    Integer(4) kP,idum
    Logical, intent(in) :: ls
    Logical ldum
    Type(dialog) Dlg
    if(iGetExpData.ge.0) then
      if(ls) then
        call DlgGetI(Dlg,idc_Exp_nExc,0,0,nRHS,1,nPar-1,1)
        call DlgGetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,1,kExc,1,nRHS,1)
        call DlgGetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,1,kExp,1,nExp,nExp)
        call DlgGetI(Dlg,idc_Exp_iCol,idc_Exp_iColS,1,kP,0,235,1)
        tExp(kExp)%iCol=Int2(kP)
        call DlgGetI(Dlg,idc_Exp_iDom,idc_Exp_iDomS,1,kP,0,150,0)
        tExp(kExp)%iDom=Int2(kP)
        call DlgGetI(Dlg,idc_Exp_iConn,idc_Exp_iConnS,1,kP,-32000,32000,0)
        tExp(kExp)%iConn=Int2(kP)
      else
        call DlgGetI(Dlg,idc_Exp_nExc,0,0,nRHS,1,nPar-1,1)
        call DlgGetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,0,kExc,1,nRHS,1)
        call DlgGetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,0,kExp,1,nExp,nExp)
        call DlgGetI(Dlg,idc_Exp_iCol,idc_Exp_iColS,0,kP,0,235,1)
        tExp(kExp)%iCol=Int2(kP)
        call DlgGetI(Dlg,idc_Exp_iDom,idc_Exp_iDomS,0,kP,0,150,0)
        tExp(kExp)%iDom=Int2(kP)
        call DlgGetI(Dlg,idc_Exp_iConn,idc_Exp_iConnS,0,kP,-32000,32000,0)
        tExp(kExp)%iConn=Int2(kP)
      end if
      call DlgGetL(dlg,idc_Exp_lConn,ldum)
      if(ldum) tExp(kExp)%iTypE=0_2
      call DlgGetL(dlg,idc_Exp_l2DPol,ldum)
      if(ldum) tExp(kExp)%iTypE=1_2
      call DlgGetL(dlg,idc_Exp_l2DBessel,ldum)
      if(ldum) tExp(kExp)%iTypE=2_2
      call DlgGetL(dlg,idc_Exp_lPlW,ldum)
      if(ldum) tExp(kExp)%iTypE=3_2
      call DlgGetL(dlg,idc_Exp_lRay,ldum)
      if(ldum) tExp(kExp)%iTypE=4_2
      call DlgGetL(dlg,idc_Exp_l2DHarmonic,ldum)
      if(ldum) tExp(kExp)%iTypE=5_2
      call DlgGetL(dlg,idc_Exp_l3DPol,ldum)
      if(ldum) tExp(kExp)%iTypE=6_2
      call DlgGetL(dlg,idc_Exp_l3DBessel,ldum)
      if(ldum) tExp(kExp)%iTypE=7_2
      call DlgGetL(dlg,idc_Exp_l3DRing,ldum)
      if(ldum) tExp(kExp)%iTypE=8_2
      call DlgGetL(dlg,idc_Exp_l3DLine,ldum)
      if(ldum) tExp(kExp)%iTypE=9_2
      call DlgGetL(dlg,idc_Exp_l3DSpiral,ldum)
      if(ldum) tExp(kExp)%iTypE=10_2
      call DlgGetL(dlg,idc_Exp_l3DGauss,ldum)
      if(ldum) tExp(kExp)%iTypE=11_2
      call DlgGetL(dlg,idc_Exp_l2Dlayer,ldum)
      if(ldum) tExp(kExp)%iTypE=12_2
      call DlgGetL(dlg,idc_Exp_l3Dlayer,ldum)
      if(ldum) tExp(kExp)%iTypE=13_2
      call DlgGetL(dlg,idc_Exp_lE,ldum)
      if(ldum) then
        tExp(kExp)%iHE=0_2
      else
        call DlgGetL(dlg,idc_Exp_lH,ldum)
        if(ldum) then
          tExp(kExp)%iHE=1_2
        else
          tExp(kExp)%iHE=2_2
        end if
      end if
      call DlgGetL(dlg,idc_Exp_lC,ldum)
      if(ldum) then
        tExp(kExp)%iHE=tExp(kExp)%iHE+20_2
      else
        call DlgGetL(dlg,idc_Exp_lS,ldum)
        if(ldum) tExp(kExp)%iHE=tExp(kExp)%iHE+10_2
      end if
      call DlgGetR(Dlg,idc_Exp_Gam1,dum1,nBig,pBig,0.0d0,10)
      call DlgGetR(Dlg,idc_Exp_Gam2,dum2,nBig,pBig,0.0d0,10)
      tExp(kExp)%gc=DCmplx(dum1,dum2)
      call DlgGetI(Dlg,0,idc_Exp_siexps,1,kiExp,1,6,1)
      call DlgGetI(Dlg,0,idc_Exp_srexps,1,krExp,1,5,1)
      call DlgGetI(Dlg,idc_Exp_iexp,0,0,kP,-30000,30000,0)
      tExp(kExp)%iE(kiExp)=Int2(kP)
      call DlgGetR(Dlg,idc_Exp_rexp,dum1,nBig,pBig,0.0d0,10)
      tExp(kExp)%rE(krExp)=dum1
      ldum=DlgSet(Dlg,idc_Exp_Origx,.true.,DLG_ENABLE)
      ldum=DlgSet(Dlg,idc_Exp_Origy,.true.,DLG_ENABLE)
      if((tExp(kExp)%iTypE.lt.6).or.(tExp(kExp)%iTypE.eq.12)) then
        call DlgGetR(Dlg,idc_Exp_Origx,dum1,nBig,pBig,0.0d0,10)
        tExp(kExp)%Plane(1,0)=dum1
        tExp(kExp)%xo=dum1
        call DlgGetR(Dlg,idc_Exp_Origy,dum1,nBig,pBig,0.0d0,10)
        tExp(kExp)%Plane(2,0)=dum1
        tExp(kExp)%yo=dum1
      else if(tExp(kExp)%iTypE.gt.9) then
        call DlgGetR(Dlg,idc_Exp_Origx,dum1,nBig,pBig,0.0d0,10)
        tExp(kExp)%xo=dum1
        call DlgGetR(Dlg,idc_Exp_Origy,dum1,nBig,pBig,0.0d0,10)
        tExp(kExp)%yo=dum1
      else
        ldum=DlgSet(Dlg,idc_Exp_Origx,.false.,DLG_ENABLE)
        ldum=DlgSet(Dlg,idc_Exp_Origy,.false.,DLG_ENABLE)
      end if
      call DlgGetI(Dlg,idc_Exp_nPar,0,0,kP,1,32767,1)
      call RepExpTest(kP)
    end if
    if(iGetExpData.le.0) then
      if(ls) then
        call DlgGetI(Dlg,idc_Exp_nExc,0,0,nRHS,1,nPar-1,1)
        call DlgGetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,1,kExc,1,nRHS,1)
        call DlgGetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,1,kExp,1,nExp,nExp)
        call DlgGetI(Dlg,idc_Exp_kPar,idc_Exp_kParS,1,kP,1,Int4(tExp(kExp)%nPar),1)
        kPar=kP+tExp(kExp)%iOff
      else
        call DlgGetI(Dlg,idc_Exp_nExc,0,0,nRHS,1,nPar-1,1)
        call DlgGetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,0,kExc,1,nRHS,1)
        call DlgGetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,0,kExp,1,nExp,nExp)
        call DlgGetI(Dlg,idc_Exp_kPar,idc_Exp_kExpS,0,kP,1,Int4(tExp(kExp)%nPar),1)
        kPar=kP+tExp(kExp)%iOff
      end if
      call DlgGetI(Dlg,idc_Exp_iPar,0,0,kP,-127,127,0)
      iParExp(kExc,kPar)=Int2(kP)
      call DlgGetR(Dlg,idc_Exp_Par1,dum1,nBig,pBig,0.0d0,10)
      call DlgGetR(Dlg,idc_Exp_Par2,dum2,nBig,pBig,0.0d0,10)
      ParExp(kExc,kPar)=DCmplx(dum1,dum2)
    end if
    call DlgGetR(Dlg,idc_Exp_DependMax,dep_delExp,nBig,pBig,1.0d0,10)
    call DlgGetI(Dlg,idc_Exp_iObj,0,0,idum,-32000,32000,0)
    tExp(kExp)%iObj=Int2(idum)
  end Subroutine GetExpansionData

  Subroutine updateExpansion(Dlg,control_name,callbackType)
! callback for ExpansionDialog
    Implicit none
    Include 'RESOURCE.FD'
    Real(8) dum
    Integer(4) control_name,callbackType,idum,kP
    Logical ldum
    Type (dialog) Dlg
    idum=callbackType
    iGetExpData=0
    Select Case(control_name)
    Case(idc_Exp_nExc)
      call DlgGetI(Dlg,idc_Exp_nExc,0,0,idum,1,nPar-1,1)
      idum=max(1,min(idum,nPar-1))
      if(lEigen) idum=1
      if(idum.ne.nExc) then
        nRHS=idum
        call AllocatePar(ldum)
        idum=max(1,min(kExc,nRHS))
        call GetExpansionData(Dlg,.false.)
        kExc=idum
        call SetExpansionData(Dlg)
      end if
    Case(idc_Exp_kExc)
      call DlgGetI(Dlg,idc_Exp_nExc,0,0,idum,1,nPar-1,1)
      nRHS=max(1,min(idum,nPar-1))
      call DlgGetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,0,idum,1,nRHS,1)
      call DlgSetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,kExc,1,nRHS)
      call GetExpansionData(Dlg,.false.)
      kExc=idum
      call SetExpansionData(Dlg)
    Case(idc_Exp_kExcS)
      call DlgGetI(Dlg,idc_Exp_nExc,0,0,idum,1,nPar-1,1)
      nRHS=max(1,min(idum,nPar-1))
      call DlgGetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,1,idum,1,nRHS,1)
      call DlgSetI(Dlg,idc_Exp_kExc,idc_Exp_kExcS,kExc,1,nRHS)
      call GetExpansionData(Dlg,.false.)
      kExc=idum
      call SetExpansionData(Dlg)
    Case(idc_Exp_kExp)
      call DlgGetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,0,idum,1,nExp,nExp)
      call DlgSetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,kExp,1,nExp)
      call GetExpansionData(Dlg,.false.)
      kExp=idum
      call SetExpansionData(Dlg)
    Case(idc_Exp_kExpS)
      call DlgGetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,1,idum,1,nExp,nExp)
      call DlgSetI(Dlg,idc_Exp_kExp,idc_Exp_kExpS,kExp,1,nExp)
      call GetExpansionData(Dlg,.false.)
      kExp=idum
      call SetExpansionData(Dlg)
    Case(idc_Exp_kPar)
      call DlgGetI(Dlg,idc_Exp_kPar,idc_Exp_kParS,0,idum,1,Int4(tExp(kExp)%nPar),1)
      kP=kPar-tExp(kExp)%iOff
      call DlgSetI(Dlg,idc_Exp_kPar,idc_Exp_kParS,kP,1,Int4(tExp(kExp)%nPar))
      call GetExpansionData(Dlg,.false.)
      kPar=idum+tExp(kExp)%iOff
      call SetExpansionData(Dlg)
    Case(idc_Exp_kParS)
      call DlgGetI(Dlg,idc_Exp_kPar,idc_Exp_kParS,1,idum,1,Int4(tExp(kExp)%nPar),1)
      kP=kPar-tExp(kExp)%iOff
      call DlgSetI(Dlg,idc_Exp_kPar,idc_Exp_kParS,kP,1,Int4(tExp(kExp)%nPar))
      call GetExpansionData(Dlg,.false.)
      kPar=idum+tExp(kExp)%iOff
      call SetExpansionData(Dlg)
    Case(idc_Exp_iDom,idc_Exp_iConn,idc_Exp_iCol)
      call GetExpansionData(Dlg,.false.)
      call SetExpansionData(Dlg)
    Case(idc_Exp_iDomS,idc_Exp_iConnS,idc_Exp_iColS)
      call GetExpansionData(Dlg,.true.)
      call SetExpansionData(Dlg)
    Case(idc_Exp_siExpS)
      call DlgGetI(Dlg,idc_Exp_iexp,0,0,idum,-30000,30000,0)
      tExp(kExp)%iE(kiExp)=Int2(idum)
      call DlgGetI(Dlg,0,idc_Exp_siexps,1,kiExp,1,6,1)
      call DlgSetS0(Dlg,idc_Exp_siexp,siExp(kiExp,tExp(kExp)%iTypE))
      call DlgSetI(Dlg,idc_Exp_iexp,0,Int4(tExp(kExp)%iE(kiExp)),-30000,30000)
    Case(idc_Exp_srExpS)
      call DlgGetR(Dlg,idc_Exp_rexp,dum,nBig,pBig,0.0d0,7)
      tExp(kExp)%rE(krExp)=dum
      call DlgGetI(Dlg,0,idc_Exp_srexps,1,krExp,1,5,1)
      call DlgSetS0(Dlg,idc_Exp_srexp,srExp(krExp,tExp(kExp)%iTypE))
      call DlgSetR(Dlg,idc_Exp_rexp,tExp(kExp)%rE(krExp),10)
    Case(idc_Exp_LE,idc_Exp_LH,idc_Exp_LHE,idc_Exp_lConn,idc_Exp_l2DPol,idc_Exp_l2DBessel,idc_Exp_lPlW, &
    &    idc_Exp_lRay,idc_Exp_l2DHarmonic,idc_Exp_l3DPol,idc_Exp_l3DBessel,idc_Exp_l3DRing, &
         idc_Exp_l3DLine,idc_Exp_l3DSpiral,idc_Exp_l3DGauss,idc_Exp_l2Dlayer,idc_Exp_l3Dlayer)
      call GetExpansionData(Dlg,.true.)
      call SetExpansionData(Dlg)
    Case(idc_Exp_delExp)
      if(nExp.gt.1) then
        idum=kExp
        call GetExpansionData(Dlg,.false.)
        call InsertExp(kExp,-1,ldum)
        kExp=min(idum,nExp)
        call SetExpansionData(Dlg)
      end if
    Case(idc_Exp_delDepend)
      if(nExp.gt.1) then
        idum=kExp
        call GetExpansionData(Dlg,.false.)
        call DelDependExp(.true.)
        kExp=min(idum,nExp)
        call SetExpansionData(Dlg)
      end if
    Case(idc_Exp_getDepend)
      call GetExpansionData(Dlg,.false.)
      call GetDependExp(.true.)
      call SetExpansionData(Dlg)
    Case(idc_Exp_copExp)
      call GetExpansionData(Dlg,.false.)
      nInsObj=nExp
      kInsObj=kExp
      call InsertDialog(.false.)
      if(kInsObj.ge.0) then
        call CopyExp(kExp,kInsObj,0.0d0,0.0d0,-2_4,ldum)
      end if
      call SetExpansionData(Dlg)
    Case(idc_Exp_all0)
      call GetExpansionData(Dlg,.false.)
      ParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)=(0.0d0,0.0d0)
      call SetExpansionData(Dlg)
    Case(idc_Exp_all1)
      call GetExpansionData(Dlg,.false.)
      ParExp(1:nRHS,tExp(kExp)%iOff+1:tExp(kExp)%iOff+tExp(kExp)%nPar)=(1.0d0,0.0d0)
      call SetExpansionData(Dlg)
    Case(idc_Exp_insExp)
      call GetExpansionData(Dlg,.false.)
      call InsertExp(kExp,1,ldum)
      kExp=kExp+1
      call InsertPar(kExp,0,1,ldum)
      call SetExpansionData(Dlg)
    Case(idc_Exp_Loc)
      call GetExpansionData(Dlg,.false.)
      space=tExp(kExp)%Plane
      SpaceText='Expansion location'C
      iSpaceRead=-1
      if(((tExp(kExp)%iTypE.gt.5).and.(tExp(kExp)%iTypE.ne.12)).or.(.not.lgcFld)) iSpaceRead=3
      call SpaceDialog(.true.)
      tExp(kExp)%Plane=space
      call Ortho3DSpace(tExp(kExp)%Plane)
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kExp)%xo=tExp(kExp)%Plane(1,0)
        tExp(kExp)%yo=tExp(kExp)%Plane(2,0)
      end if
      call SetExpansionData(Dlg)
    Case(idc_Exp_Axis)
      call GetExpansionData(Dlg,.false.)
      space(1:3,0)=tExp(kExp)%O(1:3)
      space(1:3,1)=tExp(kExp)%e(1:3)
      space(1:3,2:3)=0.0d0
      SpaceText='Expansion axis'C
      iSpaceRead=1
      call SpaceDialog(.true.)
      tExp(kExp)%O(1:3)=space(1:3,0)
      tExp(kExp)%e(1:3)=space(1:3,1)
      call Unit3DV(tExp(kExp)%e)
      call SetExpansionData(Dlg)
    Case(idc_Exp_genExp)
      call GetExpansionData(Dlg,.false.)
      call ExpGen2Dialog(.true.)
      kExp=1
      call SetExpansionData(Dlg)
    Case(idc_Exp_GenExpO)
      call GetExpansionData(Dlg,.false.)
      call ExpGen3Dialog(.true.)
      do while(lThreadStarted)
       call SleepQQ(100_4)
      end do
      kExp=1
      call SetExpansionData(Dlg)
    Case(idc_Exp_modExp)
      call GetExpansionData(Dlg,.false.)
      call ExpMod2Dialog(.true.)
      kExp=1
      call SetExpansionData(Dlg)
    Case(idc_Exp_read)
      call GetExpansionData(Dlg,.false.)
      call OpenExpansion(.false.)
      call SetExpansionData(Dlg)
    Case(idc_Exp_write)
      call GetExpansionData(Dlg,.false.)
      call SaveExpansion(.false.)
      call SetExpansionData(Dlg)
    Case(idc_Exp_draw)
      call GetExpansionData(Dlg,.false.)
      if(.not.lThreadStarted) then
        call DlgExit(dlg)
        iDraExp=kExp
        iDomExp=0_2
        iColExp=0_2
        iConExp=0_2
        call TDrawExpansion(.true.)
      end if
    Case(idcancel)
      call GetExpansionData(Dlg,.false.)
      call DlgExit(Dlg)
    end Select
  end Subroutine updateExpansion

  Subroutine ExpGen2Dialog(lCheck)
! Dialog for generating expansions along boundaries
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
    ldum=DlgInit(IDD_ExpGen2,Dlg)
    if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'ExpGen2 dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    else
      IsOpen=1
      call SetExpGen2Data(Dlg)
      ldum=DlgSetSub(Dlg,idc_ExpGen2_delExpCol,updateExpGen2)
      ldum=DlgSetSub(Dlg,idc_ExpGen2_delExpCon,updateExpGen2)
      ldum=DlgSetSub(Dlg,idc_ExpGen2_GenExp,updateExpGen2)
      ldum=DlgSetSub(Dlg,idcancel,updateExpGen2)
    end if
    ldum=DlgModal(Dlg)
    call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine ExpGen2Dialog

  Subroutine SetExpGen2Data(Dlg)
! set dialog boxes containing ExpGen2 data
    Implicit none
    Include 'RESOURCE.FD'
    Type(dialog) Dlg
    call DlgSetI(Dlg,idc_ExpGen2_kBnd,0,kB_genExp,-nBnd,nBnd)
    call DlgSetI(Dlg,idc_ExpGen2_nExpBnd,0,nE_genExp,-9999999,9999999)
    call DlgSetI(Dlg,idc_ExpGen2_nExp,0,iE_genExp,1,nExp)
    call DlgSetI(Dlg,idc_ExpGen2_nSpl,0,nS_genExp,-32767,32767)
    call DlgSetI(Dlg,idc_ExpGen2_iCol2,0,iCl_genExp,-2,235)
    call DlgSetI(Dlg,idc_ExpGen2_iConn2,0,iCn_genExp,-32001,32000)
    call DlgSetI(Dlg,idc_ExpGen2_iWf,0,iWf_genExp,-32001,32000)
    call DlgSetI(Dlg,idc_ExpGen2_iWe,0,iWe_genExp,-32001,32000)
    call DlgSetI(Dlg,idc_ExpGen2_iDom2,0,iDm_genExp,0,150)
    call DlgSetR(Dlg,idc_ExpGen2_fmin,fmin_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_fmax,fmax_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_fact,fact_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_dfl,df_genExp(1),7)
    call DlgSetR(Dlg,idc_ExpGen2_dfr,df_genExp(2),7)
    call DlgSetR(Dlg,idc_ExpGen2_dll,dl_genExp(1),7)
    call DlgSetR(Dlg,idc_ExpGen2_dlr,dl_genExp(2),7)
    call DlgSetR(Dlg,idc_ExpGen2_finn,finn_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_fout,fout_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_dinn,dinn_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_dout,dout_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_ainn,ainn_genExp,7)
    call DlgSetR(Dlg,idc_ExpGen2_aout,aout_genExp,7)
  end Subroutine SetExpGen2Data

  Subroutine GetExpGen2Data(Dlg)
! get ExpGen2 data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
	  call DlgGetI(Dlg,idc_ExpGen2_kBnd,0,0,kB_genExp,-nBnd,nBnd,1)
	  call DlgGetI(Dlg,idc_ExpGen2_nExpBnd,0,0,nE_genExp,-9999999,9999999,0)
	  call DlgGetI(Dlg,idc_ExpGen2_nExp,0,0,iE_genExp,1,nExp,1)
	  call DlgGetI(Dlg,idc_ExpGen2_nSpl,0,0,nS_genExp,-32767,32767,1000)
	  call DlgGetI(Dlg,idc_ExpGen2_iCol2,0,0,iCl_genExp,-2,235,-1)
	  call DlgGetI(Dlg,idc_ExpGen2_iConn2,0,0,iCn_genExp,-32001,32000,-32001)
	  call DlgGetI(Dlg,idc_ExpGen2_iWf,0,0,iWf_genExp,-32001,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen2_iWe,0,0,iWe_genExp,-32001,32000,3)
	  call DlgGetI(Dlg,idc_ExpGen2_iDom2,0,0,iDm_genExp,0,150,0)
	  call DlgGetR(Dlg,idc_ExpGen2_fmin,fmin_genExp,0.0d0,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_fmax,fmax_genExp,0.0d0,pBig,pBig,7)
	  call DlgGetR(Dlg,idc_ExpGen2_fact,fact_genExp,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_dfl,df_genExp(1),nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_dfr,df_genExp(2),nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_dll,dl_genExp(1),nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_dlr,dl_genExp(2),nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_finn,finn_genExp,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_fout,fout_genExp,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_dinn,dinn_genExp,0.01d0,1.0d2,1.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_dout,dout_genExp,0.01d0,1.0d2,1.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_ainn,ainn_genExp,0.0d0,25.0d0,15.0d0,7)
	  call DlgGetR(Dlg,idc_ExpGen2_aout,aout_genExp,0.0d0,25.0d0,15.0d0,7)
  end Subroutine GetExpGen2Data

	Subroutine updateExpGen2(Dlg,control_name,callbackType)
! callback for ExpGen2Dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum
    Integer(2) ir
    Logical ldum
	  Type (dialog) Dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_ExpGen2_delExpCol)
      call GetExpGen2Data(Dlg)
      idum=nExp
      ir=Int2(iCl_genExp)
      do while((idum.gt.0).and.(nExp.gt.1))
        if((ir.eq.0).or.((ir.gt.0).and.(tExp(idum)%iCol.eq.ir)).or.((ir.lt.0).and.(tExp(idum)%iCol.le.-ir))) &
        & call InsertExp(idum,-1,ldum)
        idum=idum-1
      end do
      kExp=1
      call SetExpGen2Data(Dlg)
	  Case(idc_ExpGen2_delExpCon)
      call GetExpGen2Data(Dlg)
      idum=nExp
      ir=Int2(iCn_genExp)
      do while((idum.gt.0).and.(nExp.gt.1))
        if((ir.eq.0).or.((ir.gt.0).and.(tExp(idum)%iConn.eq.ir)).or.((ir.lt.0).and.(tExp(idum)%iConn.le.-ir))) &
        & call InsertExp(idum,-1,ldum)
        idum=idum-1
      end do
      kExp=1
      call SetExpGen2Data(Dlg)
	  Case(idc_ExpGen2_genExp)
      call GetExpGen2Data(Dlg)
      call GenExp(kB_genExp,nE_genExp,iE_genExp,Int2(iCl_genExp),Int2(iCn_genExp),Int2(iDm_genExp),fmin_genExp, &
      & fmax_genExp,fact_genExp,Df_genExp,Dl_genExp,Finn_genExp,Fout_genExp,Dinn_genExp,Dout_genExp,Ainn_genExp, &
      & Aout_genExp,nS_genExp)
      call CorrExpPar(1000_4)
      kExp=1
      call SetExpGen2Data(Dlg)
     	call DlgExit(Dlg)
	  Case(idcancel)
      call GetExpGen2Data(Dlg)
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateExpGen2

  Subroutine ExpMod2Dialog(lCheck)
! Dialog for automatic modifications of expansions along boundaries
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
	  ldum=DlgInit(IDD_ExpMod2,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'ExMod2 dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetExpMod2Data(Dlg)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method0,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method1,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method2,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method3,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method4,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method5,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method6,updateExpMod2)
      ldum=DlgSetSub(Dlg,idc_ExpMod2_Method7,updateExpMod2)
      ldum=DlgSetSub(Dlg,idcancel,updateExpMod2)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine ExpMod2Dialog

  Subroutine SetExpMod2Data(Dlg)
! set dialog boxes containing ExpMod2 data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
	  call DlgSetI(Dlg,idc_ExpMod2_iExpMin,0,iModExpMin,1,nExp)
	  call DlgSetI(Dlg,idc_ExpMod2_iExpMax,0,iModExpMax,0,nExp-nExc)
	  call DlgSetI(Dlg,idc_ExpMod2_iBnd,0,iModExpBnd,-nBnd,nBnd)
	  call DlgSetI(Dlg,idc_ExpMod2_iDom,0,iModExpDom,-nDom,nDom)
	  call DlgSetI(Dlg,idc_ExpMod2_iObj,0,iModExpObj,-nObj,nObj)
	  call DlgSetI(Dlg,idc_ExpMod2_iCol,0,iModExpCol,-235,235)
	  call DlgSetI(Dlg,idc_ExpMod2_iCon,0,iModExpCon,-1000000,1000000)
	  call DlgSetI(Dlg,idc_ExpMod2_iLoop,0,iModExpLoop,-1000000,1000000)
	  call DlgSetI(Dlg,idc_ExpMod2_iWFA,0,iModExpWFA,-nFunA,nFunA)
	  call DlgSetI(Dlg,idc_ExpMod2_iWFE,0,iModExpWFE,-1000000,1000000)
	  call DlgSetR(Dlg,idc_ExpMod2_Factor,fModExpFac,8)
  end Subroutine SetExpMod2Data

  Subroutine GetExpMod2Data(Dlg)
! get ExpMod2 data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
	  call DlgGetI(Dlg,idc_ExpMod2_iExpMin,0,0,iModExpMin,1,nExp,1)
	  call DlgGetI(Dlg,idc_ExpMod2_iExpMax,0,0,iModExpMax,0,nExp-nExc,nExp-nExc)
	  call DlgGetI(Dlg,idc_ExpMod2_iBnd,0,0,iModExpBnd,-nBnd,nBnd,0)
	  call DlgGetI(Dlg,idc_ExpMod2_iDom,0,0,iModExpDom,-nDom,nDom,0)
	  call DlgGetI(Dlg,idc_ExpMod2_iObj,0,0,iModExpObj,-nObj,nObj,0)
	  call DlgGetI(Dlg,idc_ExpMod2_iCol,0,0,iModExpCol,-235,235,0)
	  call DlgGetI(Dlg,idc_ExpMod2_iCon,0,0,iModExpCon,-1000000,1000000,0)
	  call DlgGetI(Dlg,idc_ExpMod2_iLoop,0,0,iModExpLoop,-1000000,1000000,1)
	  call DlgGetI(Dlg,idc_ExpMod2_iWFA,0,0,iModExpWFA,-nFunA,nFunA,0)
	  call DlgGetI(Dlg,idc_ExpMod2_iWFE,0,0,iModExpWFE,-1000000,1000000,1)
    call DlgGetR(Dlg,idc_ExpMod2_Factor,fModExpFac,nBig,pBig,1.0d0,8)
  end Subroutine GetExpMod2Data

	Subroutine updateExpMod2(Dlg,control_name,callbackType)
! callback for ExpMod2Dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum
	  Type (dialog) Dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_ExpMod2_Method0)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D0(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method1)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D1(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method2)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D2(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method3)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D3(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method4)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D4(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method5)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D5(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method6)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D6(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idc_ExpMod2_Method7)
      call GetExpMod2Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call adaptExp2D7(.false.)
      end if
     	call DlgExit(Dlg)
	  Case(idcancel)
      call GetExpMod2Data(Dlg)
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateExpMod2

  Subroutine ExpGen3Dialog(lCheck)
! Dialog for generating expansions along boundaries
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
	  ldum=DlgInit(IDD_ExpGen3,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'ExpGen3 dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetExpGen3Data(Dlg)
      ldum=DlgSetSub(Dlg,idc_ExpGen3_GenExp,updateExpGen3)
      ldum=DlgSetSub(Dlg,idcancel,updateExpGen3)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
  end Subroutine ExpGen3Dialog

  Subroutine SetExpGen3Data(Dlg)
! set dialog boxes containing ExpGen3 data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
    call DlgSetL(dlg,idc_ExpGen3_NoDel,lGenExpDel)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenObj,0,iGenExpObj,-nObj,nObj)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenIns,0,iGenExpIns,-nExp,nExp)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenMinE,0,iGenExpMinE,-32000,32000)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenMaxE,0,iGenExpMaxE,-32000,32000)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenMaxM,0,iGenExpMaxM,-32000,32000)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenCL,0,iGenExpCl,-32000,32000)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenCL2,0,iGenExpCl2,-32000,32000)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenCN,0,iGenExpCn,-32000,32000)
	  call DlgSetI(Dlg,idc_ExpGen3_iGenOB,0,iGenExpOb,-32000,32000)
	  call DlgSetR(Dlg,idc_ExpGen3_GenF,fGenExpObj,8)
  end Subroutine SetExpGen3Data

  Subroutine GetExpGen3Data(Dlg)
! get ExpGen3 data
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) Dlg
		call DlgGetL(dlg,idc_ExpGen3_NoDel,lGenExpDel)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenObj,0,0,iGenExpObj,-nObj,nObj,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenIns,0,0,iGenExpIns,-nExp,nExp,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenMinE,0,0,iGenExpMinE,-32000,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenMaxE,0,0,iGenExpMaxE,-32000,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenMaxM,0,0,iGenExpMaxM,-32000,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenCL,0,0,iGenExpCl,-32000,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenCL2,0,0,iGenExpCl2,-32000,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenCN,0,0,iGenExpCn,-32000,32000,0)
	  call DlgGetI(Dlg,idc_ExpGen3_iGenOB,0,0,iGenExpOb,-32000,32000,0)
    call DlgGetR(Dlg,idc_ExpGen3_GenF,fGenExpObj,nBig,pBig,0.0d0,8)
  end Subroutine GetExpGen3Data

	Subroutine updateExpGen3(Dlg,control_name,callbackType)
! callback for ExpGen3Dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum
	  Type (dialog) Dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_ExpGen3_genExp)
      call GetExpGen3Data(Dlg)
      if(.not.lThreadStarted) then
	      call DlgExit(dlg)
        call GenObjExp(iGenExpObj,iGenExpIns,iGenExpMinE,iGenExpMaxE,iGenExpMaxM,iGenExpCl,iGenExpCl2, &
        & iGenExpCn,iGenExpOb,lGenExpDel,0)
      end if
     	call DlgExit(Dlg)
	  Case(idcancel)
      call GetExpGen3Data(Dlg)
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateExpGen3

  Subroutine FFieldDialog(lCheck)
! dialog for the field formula data
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
	  ldum=DlgInit(IDD_FField,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Field formula dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      lfcFld=lfcFldAll
      call SetFFieldData(dlg)
	    ldum=DlgSetSub(dlg,idc_FField_E,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_H,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_A,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_V,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_IQ,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_IQS,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_Check,updateFField)
	    ldum=DlgSetSub(dlg,idc_FField_Accept,updateFField)
	    ldum=DlgSetSub(dlg,idc_read_flf,updateFField)
	    ldum=DlgSetSub(dlg,idc_write_flf,updateFField)
	    ldum=DlgSetSub(dlg,idcancel,updateFField)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine FFieldDialog

  Subroutine SetFFieldData(dlg)
! set the FField data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    cForm(6:8)=c678(1:3,1)
    call DlgSetR(Dlg,IDC_CFORM6,cForm(6),5)
    call DlgSetR(Dlg,IDC_CFORM7,cForm(7),5)
    call DlgSetR(Dlg,IDC_CFORM8,cForm(8),5)
    call DlgSetI(Dlg,idc_FField_KEXP,0,kExpFFD,-nExp,nExp)
    call DlgSetI(Dlg,idc_FField_KCOL,0,kColFFD,-255,255)
    call DlgSetI(Dlg,idc_FField_KCON,0,kConFFD,-32001,32000)
    call DlgSetI(Dlg,idc_FField_KDOM,0,kDomFFD,-150,150)
    call DlgSetI(Dlg,idc_FField_KPAR,0,kParFFD,-32000,32000)
    call DlgSetI(Dlg,idc_FField_IQ,idc_FField_IQS,IQFFD,1,20)
    if(iFFldE.eq.1) then
      call DlgSetL(dlg,idc_FField_E,.true.)
    else if(iFFldE.eq.2) then
      call DlgSetL(dlg,idc_FField_H,.true.)
    else if(iFFldE.eq.3) then
      call DlgSetL(dlg,idc_FField_A,.true.)
    else
      call DlgSetL(dlg,idc_FField_V,.true.)
    end if
    call DlgSetL(dlg,idc_FField_lExp,lExpFFD)
    call DlgSetL(dlg,idc_FField_lExc,lExcFFD)
    call DlgSetL(dlg,idc_FField_LIQ,LIQ)
    Fld_Def_Form(0,1:3,iFFldE)=Fld_Form(1:3,iFFldE)
    call DlgSetSn(Dlg,idc_FField_X,Fld_Form(1,iFFldE),4,Fld_Def_Form(0:3,1,iFFldE))
    call DlgSetSn(Dlg,idc_FField_Y,Fld_Form(2,iFFldE),4,Fld_Def_Form(0:3,2,iFFldE))
    call DlgSetSn(Dlg,idc_FField_Z,Fld_Form(3,iFFldE),4,Fld_Def_Form(0:3,3,iFFldE))
    Q_Def_Form(0,iQFFD)=Q_Form(iQFFD,1)
    call DlgSetSn(Dlg,idc_FField_Q,Q_Form(iQFFD,1),4,Q_Def_Form(0:3,iQFFD))
  end Subroutine SetFFieldData

  Subroutine GetFFieldData(dlg)
! get the FField data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Logical ldum
	  Type(dialog) dlg
    call DlgGetR(Dlg,IDC_CFORM6,cForm(6),nBig,pBig,0.0d0,5)
    call DlgGetR(Dlg,IDC_CFORM7,cForm(7),nBig,pBig,0.0d0,5)
    call DlgGetR(Dlg,IDC_CFORM8,cForm(8),nBig,pBig,0.0d0,5)
    c678(1:3,1)=cForm(6:8)
    cCForm(6:8)=DCmplx(cForm(6:8),0.0d0)
    call DlgGetI(Dlg,idc_FField_KEXP,0,0,kExpFFD,-nExp,nExp,0)
    call DlgGetI(Dlg,idc_FField_KCOL,0,0,kColFFD,-255,255,0)
    call DlgGetI(Dlg,idc_FField_KCON,0,0,kConFFD,-32001,32000,0)
    call DlgGetI(Dlg,idc_FField_KDOM,0,0,kDomFFD,-150,150,0)
    call DlgGetI(Dlg,idc_FField_KPAR,0,0,kParFFD,-32000,32000,0)
    call DlgGetI(Dlg,idc_FField_IQ,idc_FField_IQS,0,iQFFD,1,20,1)
    call DlgGetL(dlg,idc_FField_E,ldum)
    if(ldum) iFFldE=1
    call DlgGetL(dlg,idc_FField_H,ldum)
    if(ldum) iFFldE=2
    call DlgGetL(dlg,idc_FField_A,ldum)
    if(ldum) iFFldE=3
    call DlgGetL(dlg,idc_FField_V,ldum)
    if(ldum) iFFldE=4
    call DlgGetL(dlg,idc_FField_lExp,lExpFFD)
    call DlgGetL(dlg,idc_FField_lExc,lExcFFD)
    call DlgGetL(dlg,idc_FField_LIQ,LIQ)
    if(lGetAll) then
      call DlgGetS(Dlg,idc_FField_X,Fld_Form(1,iFFldE))
      call DlgGetS(Dlg,idc_FField_Y,Fld_Form(2,iFFldE))
      call DlgGetS(Dlg,idc_FField_Z,Fld_Form(3,iFFldE))
      call DlgGetS(Dlg,idc_FField_Q,Q_Form(iQFFD,1))
    end if
  end Subroutine GetFFieldData

  Subroutine updateFField(dlg,control_name,callbackType)
  ! callback for FFieldDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,ie,idum,i1,lfdum
    Type (dialog) dlg
    idum=callbackType
    lGetAll=.false.
    Select Case(control_name)
    Case(idc_FField_IQ)
      call DlgGetI(Dlg,idc_FField_IQ,idc_FField_IQS,0,iQFFD,1,20,1)
      call SetFFieldData(dlg)
    Case(idc_FField_IQS)
      call DlgGetI(Dlg,idc_FField_IQ,idc_FField_IQS,1,iQFFD,1,20,1)
      call SetFFieldData(dlg)
    Case(idc_FField_E,idc_FField_H,idc_FField_A,idc_FField_V)
      call GetFFieldData(dlg)
      call SetFFieldData(dlg)
    Case(idc_FField_Check)
      lGetAll=.true.
      call GetFFieldData(dlg)
      ie=0
      i1=iQFFD
      cForm(6:8)=c678(1:3,1)
      cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
      do iQFFD=1,20
        if(lfcFld) then
          call checkCFormula(Q_Form(iQFFD,1),lfdum,8,11,300,idum)
        else
          call checkFormula(Q_Form(iQFFD,1),lfdum,8,11,300,idum)
        end if
        if(idum.ne.0) ie=ie+1
      end do
      iQFFD=i1
      i1=iFFldE
      do iFFldE=1,4
        if(lfcFld) then
          call checkCFormula(Fld_Form(1,iFFldE),lfdum,8,10,300,idum)
          if(idum.ne.0) ie=ie+1
          call checkCFormula(Fld_Form(2,iFFldE),lfdum,8,10,300,idum)
          if(idum.ne.0) ie=ie+1
          call checkCFormula(Fld_Form(3,iFFldE),lfdum,8,10,300,idum)
          if(idum.ne.0) ie=ie+1
        else
          call checkFormula(Fld_Form(1,iFFldE),lfdum,8,10,300,idum)
          if(idum.ne.0) ie=ie+1
          call checkFormula(Fld_Form(2,iFFldE),lfdum,8,10,300,idum)
          if(idum.ne.0) ie=ie+1
          call checkFormula(Fld_Form(3,iFFldE),lfdum,8,10,300,idum)
          if(idum.ne.0) ie=ie+1
        end if
      end do
      iFFldE=i1
      call SetFFieldData(dlg)
      if(ie.eq.0) then
        idum=MessageBoxQQ('No errors found!'C,'Field formula dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Errors found!\rCheck formula with leading exclamation mark!'C,&
        &                 'Field formula dialog'C,MB$OK.or.MB$IconExclamation)
      end if
    Case(idc_FField_Accept)
      lGetAll=.true.
      call GetFFieldData(dlg)
      call SetFFieldData(dlg)
    Case(idc_read_flf)
      call GetFFieldData(dlg)
      call OpenFField(.false.)
      call SetFFieldData(dlg)
    Case(idc_write_flf)
      ! lGetAll=.true.
      call GetFFieldData(dlg)
      call SaveFField(.false.)
      call SetFFieldData(dlg)
    Case(idcancel)
      lGetAll=.true.
      call GetFFieldData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateFField

  Subroutine FGridDialog(lCheck)
! dialog for the grid formula data
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
	  ldum=DlgInit(IDD_FGrid,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Grid formula dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetFGridData(dlg)
	    ldum=DlgSetSub(dlg,idc_FGrid_Check,updateFGrid)
	    ldum=DlgSetSub(dlg,idc_read_grf,updateFGrid)
	    ldum=DlgSetSub(dlg,idc_write_grf,updateFGrid)
	    ldum=DlgSetSub(dlg,idcancel,updateFGrid)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine FGridDialog

  Subroutine SetFGridData(dlg)
! set the FGrid data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    cForm(6:8)=c678(1:3,3)
    cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
    call DlgSetR(Dlg,IDC_CFORM6,cForm(6),5)
    call DlgSetR(Dlg,IDC_CFORM7,cForm(7),5)
    call DlgSetR(Dlg,IDC_CFORM8,cForm(8),5)
    Grd_Def_Form(0,1:3)=Grd_Form(1:3)
    call DlgSetSn(Dlg,idc_FGrid_X,Grd_Form(1),4,Grd_Def_Form(0:3,1))
    call DlgSetSn(Dlg,idc_FGrid_Y,Grd_Form(2),4,Grd_Def_Form(0:3,2))
    call DlgSetSn(Dlg,idc_FGrid_Z,Grd_Form(3),4,Grd_Def_Form(0:3,3))
  end Subroutine SetFGridData

  Subroutine GetFGridData(dlg)
! get the FGrid data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    call DlgGetR(Dlg,IDC_CFORM6,cForm(6),nBig,pBig,0.0d0,5)
    call DlgGetR(Dlg,IDC_CFORM7,cForm(7),nBig,pBig,0.0d0,5)
    call DlgGetR(Dlg,IDC_CFORM8,cForm(8),nBig,pBig,0.0d0,5)
    cCForm(6:8)=DCmplx(cForm(6:8),0.0d0)
    c678(1:3,3)=cForm(6:8)
    call DlgGetS(Dlg,idc_FGrid_X,Grd_Form(1))
    call DlgGetS(Dlg,idc_FGrid_Y,Grd_Form(2))
    call DlgGetS(Dlg,idc_FGrid_Z,Grd_Form(3))
  end Subroutine GetFGridData

  Subroutine updateFGrid(dlg,control_name,callbackType)
  ! callback for FGridDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum,ie,lfdum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idc_FGrid_Check)
      call GetFGridData(dlg)
      ie=0
      cForm(6:8)=c678(1:3,3)
      cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
      call checkFormula(Grd_Form(1),lfdum,8,5,3,idum)
      if((idum.ne.0).and.(Grd_Form(1)(1:8).ne.'function')) ie=ie+1
      call checkFormula(Grd_Form(2),lfdum,8,5,3,idum)
      if((idum.ne.0).and.(Grd_Form(2)(1:8).ne.'function')) ie=ie+1
      call checkFormula(Grd_Form(3),lfdum,8,5,3,idum)
      if((idum.ne.0).and.(Grd_Form(3)(1:8).ne.'function')) ie=ie+1
      if(ie.eq.0) then
        idum=MessageBoxQQ('No errors found!'C,'Grid formula dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Errors found!\rCheck formula with leading exclamation mark!'C, &
        &                 'Grid formula dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      call SetFGridData(dlg)
    Case(idc_read_grf)
      call OpenFGrid(.false.)
      call SetFGridData(dlg)
    Case(idc_write_grf)
      call GetFGridData(dlg)
      call SaveFGrid(.false.)
      call SetFGridData(dlg)
    Case(idcancel)
      call GetFGridData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateFGrid

  recursive Subroutine FieldDialog(lCheck)
! dialog for the field data
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical lCheck,ldum
	  Type(dialog) dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Field,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Field dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      if(.not.lgcFld) lGet3DMat=.true.
      call SetFieldData(dlg)
	    ldum=DlgSetSub(dlg,idc_View_Plane,updateField)
	    ldum=DlgSetSub(dlg,idc_cFld_Space,updateField)
	    ldum=DlgSetSub(dlg,idc_GetMinMax,updateField)
	    ldum=DlgSetSub(dlg,idc_GetError,updateField)
	    ldum=DlgSetSub(dlg,idc_DomData,updateField)
	    ldum=DlgSetSub(dlg,idc_FieldFormula,updateField)
	    ldum=DlgSetSub(dlg,idc_GridFormula,updateField)
	    ldum=DlgSetSub(dlg,idc_ClearField,updateField)
	    ldum=DlgSetSub(dlg,idc_ClearField2,updateField)
	    ldum=DlgSetSub(dlg,idc_ClearDomain,updateField)
	    ldum=DlgSetSub(dlg,idc_TransformGrid,updateField)
	    ldum=DlgSetSub(dlg,idc_cFld_Read,updateField)
	    ldum=DlgSetSub(dlg,idc_cFld_Write,updateField)
	    ldum=DlgSetSub(dlg,idcancel,updateField)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine FieldDialog

  Subroutine SetFieldData(dlg)
! set the field data in its dialog
    Implicit none
    Include 'RESOURCE.FD'
    Type(dialog) dlg
    call DlgSetR(Dlg,idc_rMinFld,rMinFld,5)
    call DlgSetR(Dlg,idc_rMaxFld,rMaxFld,5)
    call DlgSetR(Dlg,idc_rSumFld,rSumFld,5)
    call DlgSetR(Dlg,idc_rSum2Fld,rSum2Fld,5)
    call DlgSetR(Dlg,idc_ErrFld,ErrFld,5)
    call DlgSetR(Dlg,idc_Err2Fld,Err2Fld,5)
    if(lfcFld) then
      call DlgSetS0(Dlg,idc_tprFld,'phase'C)
      prFld=360.0d0*Dble(fcFld)*trFld
      call DlgSetR(Dlg,idc_trFld,Dble(prFld),5)
    else
      call DlgSetS0(Dlg,idc_tprFld,'time'C)
      call DlgSetR(Dlg,idc_trFld,trFld,5)
    end if
    call DlgSetR(Dlg,idc_viewDist,viewDist,5)
    call DlgSetR(Dlg,idc_scaleIntensity,scaleIntensity,5)
    call DlgSetR(Dlg,idc_scaleArrow,scaleArrow,5)
    call DlgSetR(Dlg,idc_ArrowLength,ArrowLength,5)
    call DlgSetR(Dlg,idc_rIsoStep,rIsoStep,5)
    call DlgSetR(Dlg,idc_Grid3D,Grid3D,5)
    call DlgSetI(Dlg,idc_nxcFld,0,Int4(nxcFld),1,10000)
    call DlgSetI(Dlg,idc_nycFld,0,Int4(nycFld),1,10000)
    call DlgSetI(Dlg,idc_nzcFld,0,Int4(nzcFld),1,10000)
    call DlgSetI(Dlg,idc_nsFld,0,Int4(nsFld),1,1000)
    call DlgSetI(Dlg,idc_minCIntensity,0,Int4(minCIntensity),0,235)
    call DlgSetI(Dlg,idc_maxCIntensity,0,Int4(maxCIntensity),0,235)
    call DlgSetI(Dlg,idc_itIntensity,0,Int4(itIntensity),0,3)
    call DlgSetI(Dlg,idc_minCArrow,0,Int4(minCArrow),0,235)
    call DlgSetI(Dlg,idc_maxCArrow,0,Int4(maxCArrow),0,235)
    call DlgSetI(Dlg,idc_itArrow,0,Int4(itArrow),0,3)
    if(iPlane.eq.3) then
      call DlgSetL(dlg,idc_lxyPlane,.true.)
      if(levPlane.gt.nzcFld) levPlane=1
      call DlgSetI(Dlg,idc_levPlane,0,Int4(levPlane),1,nzcFld)
    else if(iPlane.eq.2) then
      call DlgSetL(dlg,idc_lxzPlane,.true.)
      if(levPlane.gt.nycFld) levPlane=1
      call DlgSetI(Dlg,idc_levPlane,0,Int4(levPlane),1,nycFld)
    else
      call DlgSetL(dlg,idc_lyzPlane,.true.)
      if(levPlane.gt.nxcFld) levPlane=1
      call DlgSetI(Dlg,idc_levPlane,0,Int4(levPlane),1,nxcFld)
    end if
    call DlgSetL(dlg,idc_larFld,larFld)
    call DlgSetL(dlg,idc_lprFld,lprFld)
    call DlgSetL(dlg,idc_lxrFld,lxrFld)
    call DlgSetL(dlg,idc_lyrFld,lyrFld)
    call DlgSetL(dlg,idc_lzrFld,lzrFld)
    call DlgSetL(dlg,idc_lxcFld,lxcFld)
    call DlgSetL(dlg,idc_lycFld,lycFld)
    call DlgSetL(dlg,idc_lzcFld,lzcFld)
    call DlgSetL(dlg,idc_lEcFld,lEcFld)
    call DlgSetL(dlg,idc_lHcFld,lHcFld)
    call DlgSetL(dlg,idc_lAcFld,lAcFld)
    call DlgSetL(dlg,idc_lVcFld,lVcFld)
    call DlgSetL(dlg,idc_lGrid3D,lGrid3D)
    call DlgSetL(dlg,idc_lIsoFill,lIsoFill)
    call DlgSetL(dlg,idc_lIsoLine,lIsoLine)
    call DlgSetL(dlg,idc_lArrowFill,lArrowFill)
    call DlgSetL(dlg,idc_lrGrd,lrGrd)
    if(itrFld.eq.itE) then
      call DlgSetL(dlg,idc_litE,.true.)
    else if(itrFld.eq.itH) then
      call DlgSetL(dlg,idc_litH,.true.)
    else if(itrFld.eq.itS) then
      call DlgSetL(dlg,idc_litS,.true.)
    else if(itrFld.eq.itD) then
      call DlgSetL(dlg,idc_litD,.true.)
    else if(itrFld.eq.itB) then
      call DlgSetL(dlg,idc_litB,.true.)
    else if(itrFld.eq.itA) then
      call DlgSetL(dlg,idc_litA,.true.)
    else if(itrFld.eq.itJ) then
      call DlgSetL(dlg,idc_litJ,.true.)
    else if(itrFld.eq.itV) then
      call DlgSetL(dlg,idc_litV,.true.)
    else if(itrFld.eq.itWe) then
      call DlgSetL(dlg,idc_litWe,.true.)
    else if(itrFld.eq.itWh) then
      call DlgSetL(dlg,idc_litWh,.true.)
    else if(itrFld.eq.itWt) then
      call DlgSetL(dlg,idc_litWt,.true.)
    else if(itrFld.eq.itPe) then
      call DlgSetL(dlg,idc_litPe,.true.)
    else if(itrFld.eq.itPh) then
      call DlgSetL(dlg,idc_litPh,.true.)
    else if(itrFld.eq.itPt) then
      call DlgSetL(dlg,idc_litPt,.true.)
    end if
  end Subroutine SetFieldData

  Subroutine GetFieldData(dlg)
! get the field data from its dialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) idum
    Logical ldum
    Type(dialog) dlg
    call DlgGetI(Dlg,idc_nxcFld,0,0,nxcFld,1,10000,10)
    call DlgGetI(Dlg,idc_nycFld,0,0,nycFld,1,10000,10)
    call DlgGetI(Dlg,idc_nzcFld,0,0,nzcFld,1,10000,1)
    call DlgGetL(dlg,idc_lxcFld,lxcFld)
    call DlgGetL(dlg,idc_lycFld,lycFld)
    call DlgGetL(dlg,idc_lzcFld,lzcFld)
    call DlgGetL(dlg,idc_lEcFld,lEcFld)
    call DlgGetL(dlg,idc_lHcFld,lHcFld)
    call DlgGetL(dlg,idc_lAcFld,lAcFld)
    call DlgGetL(dlg,idc_lVcFld,lVcFld)
    call DlgGetL(dlg,idc_lrGrd,lrGrd)
    call DlgGetR(Dlg,idc_rMinFld,rMinFld,nBig,pBig,0.0d0,5)
    call DlgGetR(Dlg,idc_rMaxFld,rMaxFld,nBig,pBig,1.0d0,5)
    if(lfcFld) then
      call DlgGetR(Dlg,idc_trFld,prFld,nBig,pBig,0.0d0,5)
      if(cdAbs(fcFld).gt.pSmall) then
        trFld=prFld/(360.0d0*Dble(fcFld))
      else
        trFld=0.0d0
      end if
    else
      call DlgGetR(Dlg,idc_trFld,trFld,nBig,pBig,0.0d0,5)
      prFld=360.0d0*Dble(fcFld)*trFld
    end if
    call DlgGetR(Dlg,idc_viewDist,viewDist,pSmall,pBig,pBig,5)
    call DlgGetR(Dlg,idc_scaleIntensity,scaleIntensity,nBig,pBig,1.0d0,5)
    call DlgGetR(Dlg,idc_scaleArrow,scaleArrow,nBig,pBig,1.0d0,5)
    call DlgGetR(Dlg,idc_ArrowLength,ArrowLength,nBig,pBig,0.05d0,5)
    call DlgGetR(Dlg,idc_rIsoStep,rIsoStep,nBig,pBig,0.1d0,5)
    call DlgGetR(Dlg,idc_Grid3D,Grid3D,nBig,pBig,0.1d0,5)
    call DlgGetI(Dlg,idc_nsFld,0,0,nsFld,1,1000,1)
    call DlgGetI(Dlg,idc_minCIntensity,0,0,idum,0,235,16)
    minCIntensity=idum
    call DlgGetI(Dlg,idc_maxCIntensity,0,0,idum,0,235,115)
    maxCIntensity=idum
    call DlgGetI(Dlg,idc_itIntensity,0,0,idum,0,3,3)
    itIntensity=idum
    call DlgGetI(Dlg,idc_minCArrow,0,0,idum,0,235,216)
    minCArrow=idum
    call DlgGetI(Dlg,idc_maxCArrow,0,0,idum,0,235,235)
    maxCArrow=idum
    call DlgGetI(Dlg,idc_itArrow,0,0,idum,0,3,2)
    itArrow=idum
    call DlgGetL(dlg,idc_lxyPlane,ldum)
    if(ldum) iPlane=3
    call DlgGetL(dlg,idc_lxzPlane,ldum)
    if(ldum) iPlane=2
    call DlgGetL(dlg,idc_lyzPlane,ldum)
    if(ldum) iPlane=1
    if(iPlane.eq.3) then
	    call DlgGetI(Dlg,idc_levPlane,0,0,levPlane,1,nzcFld,1)
    else if(iPlane.eq.2) then
	    call DlgGetI(Dlg,idc_levPlane,0,0,levPlane,1,nycFld,1)
    else
	    call DlgGetI(Dlg,idc_levPlane,0,0,levPlane,1,nxcFld,1)
    end if
    call DlgGetL(dlg,idc_larFld,larFld)
    call DlgGetL(dlg,idc_lprFld,lprFld)
    call DlgGetL(dlg,idc_lxrFld,lxrFld)
    call DlgGetL(dlg,idc_lyrFld,lyrFld)
    call DlgGetL(dlg,idc_lzrFld,lzrFld)
    call DlgGetL(dlg,idc_lGrid3D,lGrid3D)
    call DlgGetL(dlg,idc_lIsoFill,lIsoFill)
    call DlgGetL(dlg,idc_lIsoLine,lIsoLine)
    call DlgGetL(dlg,idc_lArrowFill,lArrowFill)
    call DlgGetL(dlg,idc_litE,ldum)
    if(ldum) itrFld=itE
    call DlgGetL(dlg,idc_litH,ldum)
    if(ldum) itrFld=itH
    call DlgGetL(dlg,idc_litS,ldum)
    if(ldum) itrFld=itS
    call DlgGetL(dlg,idc_litD,ldum)
    if(ldum) itrFld=itD
    call DlgGetL(dlg,idc_litB,ldum)
    if(ldum) itrFld=itB
    call DlgGetL(dlg,idc_litA,ldum)
    if(ldum) itrFld=itA
    call DlgGetL(dlg,idc_litJ,ldum)
    if(ldum) itrFld=itJ
    call DlgGetL(dlg,idc_litV,ldum)
    if(ldum) itrFld=itV
    call DlgGetL(dlg,idc_litWe,ldum)
    if(ldum) itrFld=itWe
    call DlgGetL(dlg,idc_litWh,ldum)
    if(ldum) itrFld=itWh
    call DlgGetL(dlg,idc_litWt,ldum)
    if(ldum) itrFld=itWt
    call DlgGetL(dlg,idc_litPe,ldum)
    if(ldum) itrFld=itPe
    call DlgGetL(dlg,idc_litPh,ldum)
    if(ldum) itrFld=itPh
    call DlgGetL(dlg,idc_litPt,ldum)
    if(ldum) itrFld=itPt
  end Subroutine GetFieldData

  Subroutine updateField(dlg,control_name,callbackType)
  ! callback for FieldDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum,nx,ny,nz
    Logical ldum,lduG,lx,ly,lz,lE,lH,lA,lV
    Type (dialog) dlg
    idum=callbackType
    nx=nxcFld
    ny=nycFld
    nz=nzcFld
    lduG=lrGrd
    lx=lxcFld
    ly=lycFld
    lz=lzcFld
    lE=lEcFld
    lH=lHcFld
    lA=lAcFld
    lV=lVcFld
    call GetFieldData(dlg)
    if(((nx.ne.nxcFld).or.(ny.ne.nycFld).or.(nz.ne.nzcFld).or.(lduG.xor.lrGrd).or.&
    &  (lx.xor.lxcFld).or. (ly.xor.lycFld).or.(lz.xor.lzcFld).or. &
    &  (lE.xor.lEcFld).or.(lH.xor.lHcFld).or.(lA.xor.lAcFld).or.(lV.xor.lVcFld)).and. &
    &  (control_name.ne.idc_ClearField).and.(control_name.ne.idc_ClearField2)) then
      idum=MessageBoxQQ('Essential Fiel data modified!\rField, domain, and grid should be reset!\rReset now?'C, &
      &                 'Field dialog'C,MB$YESNO.or.MB$IconQuestion)
      if(idum.eq.MB$IDYES) then
        call Cursor(.true.,IDC_WAIT)
        call AllocateGrd(ldum)
        if(.not.ldum) return
        call AllocateIFld(ldum)
        if(.not.ldum) return
        call AllocateFld(ldum)
        if(.not.ldum) return
     	  call DlgExit(Dlg)
        call TClrAll(.true.)
        call FieldDialog(.false.)
        call Cursor(.false.,IDC_WAIT)
      end if
    end if
    Select Case(control_name)
    Case(idc_View_Plane)
      space=viewPlane
      SpaceText='Plane for viewing the field'C
      iSpaceRead=2
      call SpaceDialog(.true.)
      viewPlane=space
      idum=MessageBoxQQ('Orthonormalize vectors?'C,'Field dialog'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDYES) call Ortho3DSpace(viewPlane)
    Case(idc_cFld_Space)
      space=spacecFld
      SpaceText='Space of the defined field'C
      iSpaceRead=3
      call SpaceDialog(.true.)
      spacecFld=space
    Case(idc_GetMinMax)
      call GetrField(.true.)
      call SetFieldData(dlg)
    Case(idc_GetError)
      call GetrError(.false.)
      call GetrField(.true.)
      call SetFieldData(dlg)
    Case(idc_GridFormula)
      call FGridDialog(.true.)
      call SetFieldData(dlg)
    Case(idc_DomData)
      call DomainDialog(.true.)
      call SetFieldData(dlg)
    Case(idc_FieldFormula)
      call FFieldDialog(.true.)
      call SetFieldData(dlg)
    Case(idc_TransformGrid)
	    call DlgExit(dlg)
      call TGridDialog(.true.)
    Case(idc_ClearField)
     	call DlgExit(Dlg)
      call AllocateGrd(ldum)
      if(.not.ldum) return
      call AllocateIFld(ldum)
      if(.not.ldum) return
      call AllocateFld(ldum)
      if(.not.ldum) return
      call TClrAll(.true.)
    Case(idc_ClearField2)
      call AllocateGrd(ldum)
      if(.not.ldum) return
      call AllocateIFld(ldum)
      if(.not.ldum) return
      call AllocateFld(ldum)
      if(.not.ldum) return
	    call ClearGrid(ldum)
	    call ClearDomain(ldum)
	    call ClearField(ldum)
      call GetrField(.true.)
      call SetFieldData(dlg)
    Case(idc_ClearDomain)
     	call DlgExit(Dlg)
      iBound=0_2 ! reset splines !!??
      call TClrDom(.true.)
    Case(idc_cFld_Read)
      call GetFieldData(dlg)
      lDiffField=.false.
      lDiffRel=.false.
      lAskFld=.true.
      lSkipFld=.false.
      lSkipDerFld=.false.
      lSkipVecFld=.false.
      lSkipScaFld=.false.
      lSkipFldHead=.false.
      call openField(.false.)
      call SetFieldData(dlg)
      if(lSkipDerFld) call GetrField(.false.)
    Case(idc_cFld_Write)
      call GetFieldData(dlg)
      lAskFld=.true.
      lSkipFld=.false.
      lSkipDerFld=.false.
      lSkipVecFld=.false.
      lSkipScaFld=.false.
      lSkipFldHead=.false.
      call saveField(.false.)
      call SetFieldData(dlg)
    Case(idcancel)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateField

  Subroutine FunctionDialog(lCheck)
! dialog for the function data
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical lCheck,ldum
	  Type(dialog) dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Function,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Function dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetFunctionData(dlg)
	    ldum=DlgSetSub(dlg,idc_fun_a1,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_a2,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_style_width,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_style_step,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_style_color,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_mark_size,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_mark_step,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_mark_color,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_a1s,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_a2s,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_style_widths,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_style_steps,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_style_colors,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_mark_sizes,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_mark_steps,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_mark_colors,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_delete,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_read,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_write,updateFunction)
	    ldum=DlgSetSub(dlg,idc_fun_default,updateFunction)
	    ldum=DlgSetSub(dlg,IDC_fun_POLAR,updateFunction)
	    ldum=DlgSetSub(dlg,IDC_fun_POLAR_DB,updateFunction)
	    ldum=DlgSetSub(dlg,IDC_fun_POLAR_2,updateFunction)
	    ldum=DlgSetSub(dlg,idcancel,updateFunction)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine FunctionDialog

  Subroutine SetFunctionData(dlg)
! set the function data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) rmi,rma
	  Logical ldum,lpolar,lclip,lsquare,ldb
	  Type(dialog) dlg
    if((iFunA1.lt.1_2).or.(iFunA1.gt.nFunA)) then
      rmi=1.0_8
      rma=dble(nFun)
    else
      call MinMax(Fun,mFun,mFunA,nFun,iFunA1,rmi,rma)
    end if
    call DlgSetR(Dlg,idc_fun_xmin,rmi,7)
    call DlgSetR(Dlg,idc_fun_xmax,rma,7)
    if((iFunA2.lt.1_2).or.(iFunA2.gt.nFunA)) then
      rmi=1.0_8
      rma=dble(nFun)
    else
      call MinMax(Fun,mFun,mFunA,nFun,iFunA2,rmi,rma)
    end if
    call DlgSetR(Dlg,idc_fun_ymin,rmi,7)
    call DlgSetR(Dlg,idc_fun_ymax,rma,7)
    rmi=rFunSca*rmi
    rma=rFunSca*rma
    call getFunctionPolar(lpolar,lclip,lsquare,ldb)
    call preprocessFun(2.0d0*pBig,rmi,lPolar,lclip,lsquare,ldb)
    call preprocessFun(2.0d0*pBig,rma,lPolar,lclip,lsquare,ldb)
    call DlgSetR(Dlg,idc_fun_rmin2,rmi,7)
    call DlgSetR(Dlg,idc_fun_rmax2,rma,7)
    call DlgSetR(Dlg,idc_fun_offset,rFunOff,7)
    call DlgSetR(Dlg,idc_fun_scale,rFunSca,7)
    call DlgSetR(Dlg,idc_fun_rmin2,rmi,7)
    call DlgSetR(Dlg,idc_fun_rmax2,rma,7)
    call DlgSetI(Dlg,idc_fun_m,0,Int4(nFun),0,Int4(mFun))
    call DlgSetI(Dlg,idc_fun_ma,0,Int4(nFunA),0,Int4(mFunA))
    call DlgSetI(Dlg,idc_fun_a1,idc_fun_a1S,Int4(iFunA1),0,Int4(nFunA))
    call DlgSetI(Dlg,idc_fun_a2,idc_fun_a2S,Int4(iFunA2),0,Int4(nFunA))
    call DlgSetI(Dlg,idc_fun_a3,0,Int4(iFunA3),0,Int4(nFunA))
    call DlgSetI(Dlg,idc_fun_a4,0,Int4(iFunA4),0,Int4(nFunA))
    call DlgSetI(Dlg,idc_fun_1,0,Int4(iFun1),1,Int4(nFun))
    call DlgSetI(Dlg,idc_fun_2,0,Int4(iFun2),1,Int4(nFun))
    call DlgSetL(dlg,idc_fun_POLAR,lpolar)
    call DlgSetL(dlg,idc_fun_POLAR_DB,ldb)
    call DlgSetL(dlg,idc_fun_POLAR_2,lsquare)
    call DlgSetL(dlg,idc_fun_POLAR_CLIP,lclip)
    ldum=.false.
    if(iFunMarkQ.gt.0_2) ldum=.true.
    call DlgSetL(dlg,idc_fun_mark_Q,ldum)
    ldum=.false.
    if(iFunMarkP.gt.0_2) ldum=.true.
    call DlgSetL(dlg,idc_fun_mark_P,ldum)
    ldum=.false.
    if(iFunMarkX.gt.0_2) ldum=.true.
    call DlgSetL(dlg,idc_fun_mark_X,ldum)
    call DlgSetI(Dlg,idc_fun_mark_Size,idc_fun_mark_SizeS,Int4(iFunMarkSize),0,20)
    call DlgSetI(Dlg,idc_fun_mark_Step,idc_fun_mark_StepS,Int4(iFunMarkStep),1,1000)
    call DlgSetI(Dlg,idc_fun_mark_Color,idc_fun_mark_ColorS,Int4(iFunMarkColor),-1,235)
    ldum=.false.
    if(iFunStyleP.gt.0_2) ldum=.true.
    call DlgSetL(dlg,idc_fun_style_P,ldum)
    ldum=.false.
    if(iFunStyleL.gt.0_2) ldum=.true.
    call DlgSetL(dlg,idc_fun_style_L,ldum)
    ldum=.false.
    if(iFunStyleD.gt.0_2) ldum=.true.
    call DlgSetL(dlg,idc_fun_style_D,ldum)
    call DlgSetI(Dlg,idc_fun_style_Width,idc_fun_style_WidthS,Int4(iFunStyleWidth),1,1000)
    call DlgSetI(Dlg,idc_fun_style_Step,idc_fun_style_StepS,Int4(iFunStyleStep),1,1000)
    call DlgSetI(Dlg,idc_fun_style_Color,idc_fun_style_ColorS,Int4(iFunStyleColor),-1,235)
	  call DlgSetS1(dlg,idc_fun_Atitle1,FunATitle(iFunA1),FunATitle(iFunA1))
	  call DlgSetS1(dlg,idc_fun_Atitle2,FunATitle(iFunA2),FunATitle(iFunA2))
  end Subroutine SetFunctionData

  Subroutine GetFunctionData(dlg)
! get the function data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical check,lpolar,lclip,lsquare,ldb
    Integer(4) idum
	  Type(dialog) dlg
    call DlgGetR(Dlg,idc_fun_offset,rFunOff,nBig,pBig,0.0d0,7)
    call DlgGetR(Dlg,idc_fun_scale,rFunSca,nBig,pBig,1.0d0,7)
	  call DlgGetI(Dlg,idc_fun_A1,idc_fun_A1S,0,idum,0,Int4(nFunA),0)
    iFunA1=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_A2,idc_fun_A2S,0,idum,0,Int4(nFunA),0)
    iFunA2=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_A3,0,0,idum,0,Int4(nFunA),0)
    iFunA3=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_A4,0,0,idum,0,Int4(nFunA),0)
    iFunA4=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_1,0,0,iFun1,1,Int4(nFun),0)
	  call DlgGetI(Dlg,idc_fun_2,0,0,iFun2,1,Int4(nFun),0)
    iFun2=max(iFun2,iFun1)
		call DlgGetL(dlg,idc_fun_Polar,lpolar)
		call DlgGetL(dlg,idc_fun_Polar_dB,ldb)
		call DlgGetL(dlg,idc_fun_Polar_2,lsquare)
		call DlgGetL(dlg,idc_fun_Polar_clip,lclip)
    call setFunctionPolar(lpolar,lclip,lsquare,ldb)
		call DlgGetL(dlg,idc_fun_Mark_Q,check)
    iFunMarkQ=0_2
		if(check) iFunMarkQ=1_2
		call DlgGetL(dlg,idc_fun_Mark_P,check)
    iFunMarkP=0_2
		if(check) iFunMarkP=1_2
		call DlgGetL(dlg,idc_fun_Mark_X,check)
    iFunMarkX=0_2
		if(check) iFunMarkX=1_2
	  call DlgGetI(Dlg,idc_fun_Mark_Size,idc_fun_Mark_SizeS,0,idum,0,20,7)
    iFunMarkSize=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_Mark_Step,idc_fun_Mark_StepS,0,idum,1,1000,10)
    iFunMarkStep=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_Mark_Color,idc_fun_Mark_ColorS,0,idum,-1,235,-1)
    iFunMarkColor=Int2(idum)
		call DlgGetL(dlg,idc_fun_Style_P,check)
    iFunStyleP=0_2
		if(check) iFunStyleP=1_2
		call DlgGetL(dlg,idc_fun_Style_L,check)
    iFunStyleL=0_2
		if(check) iFunStyleL=1_2
		call DlgGetL(dlg,idc_fun_Style_D,check)
    iFunStyleD=0_2
		if(check) iFunStyleD=1_2
	  call DlgGetI(Dlg,idc_fun_Style_Width,idc_fun_Style_WidthS,0,idum,1,10,1)
    iFunStyleWidth=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_Style_Step,idc_fun_Style_StepS,0,idum,1,1000,1)
    iFunStyleStep=Int2(idum)
	  call DlgGetI(Dlg,idc_fun_Style_Color,idc_fun_Style_ColorS,0,idum,-1,235,1)
    iFunStyleColor=Int2(idum)
    if((iFunA1.gt.0_2).and.(iFunA1.le.nFunA)) then
		  call DlgGetS(dlg,idc_fun_ATitle1,FunATitle(iFunA1))
      idum=min(GetSLength(FunATitle(iFunA1))+1,64)
      FunATitle(iFunA1)(idum:idum)=Char(0)
    end if
    if((iFunA2.gt.0_2).and.(iFunA2.le.nFunA)) then
		  call DlgGetS(dlg,idc_fun_ATitle2,FunATitle(iFunA2))
      idum=min(GetSLength(FunATitle(iFunA2))+1,64)
      FunATitle(iFunA2)(idum:idum)=Char(0)
    end if
  end Subroutine GetFunctionData

	Subroutine updateFunction(dlg,control_name,callbackType)
! callback for FunctionDialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum,iWarn
    Logical ldum
	  Type (dialog) dlg
	  idum=callbackType
	  Select Case(control_name)
    Case(idc_fun_a1,idc_fun_a1s,idc_fun_a2,idc_fun_a2s)
      if((iFunA1.gt.0_2).and.(iFunA1.le.nFunA)) then
		    call DlgGetS(dlg,idc_fun_ATitle1,FunATitle(iFunA1))
        idum=min(GetSLength(FunATitle(iFunA1))+1,64)
        FunATitle(iFunA1)(idum:idum)=Char(0)
      end if
      if((iFunA2.gt.0_2).and.(iFunA2.le.nFunA)) then
		    call DlgGetS(dlg,idc_fun_ATitle2,FunATitle(iFunA2))
        idum=min(GetSLength(FunATitle(iFunA2))+1,64)
        FunATitle(iFunA2)(idum:idum)=Char(0)
      end if
	    Select Case(control_name)
      Case(idc_fun_a1)
	      call DlgGetI(Dlg,idc_fun_A1,idc_fun_A1S,0,idum,0,Int4(nFunA),0)
        iFunA1=Int2(idum)
      Case(idc_fun_a1s)
	      call DlgGetI(Dlg,idc_fun_A1,idc_fun_A1S,1,idum,0,Int4(nFunA),0)
        iFunA1=Int2(idum)
      Case(idc_fun_a2)
	      call DlgGetI(Dlg,idc_fun_A2,idc_fun_A2S,0,idum,0,Int4(nFunA),0)
        iFunA2=Int2(idum)
      Case(idc_fun_a2s)
	      call DlgGetI(Dlg,idc_fun_A2,idc_fun_A2S,1,idum,0,Int4(nFunA),0)
        iFunA2=Int2(idum)
	    end Select
      call SetFunctionData(dlg)
    Case(idc_fun_style_width)
	    call DlgGetI(Dlg,idc_fun_Style_Width,idc_fun_Style_WidthS,0,idum,1,10,1)
      iFunStyleWidth=Int2(idum)
    Case(idc_fun_style_widthS)
	    call DlgGetI(Dlg,idc_fun_Style_Width,idc_fun_Style_WidthS,1,idum,1,10,1)
      iFunStyleWidth=Int2(idum)
    Case(idc_fun_style_step)
	    call DlgGetI(Dlg,idc_fun_Style_Step,idc_fun_Style_StepS,0,idum,1,1000,1)
      iFunStyleStep=Int2(idum)
    Case(idc_fun_style_stepS)
	    call DlgGetI(Dlg,idc_fun_Style_Step,idc_fun_Style_StepS,1,idum,1,1000,1)
      iFunStyleStep=Int2(idum)
    Case(idc_fun_style_color)
	    call DlgGetI(Dlg,idc_fun_Style_Color,idc_fun_Style_ColorS,0,idum,-1,235,1)
      iFunStyleColor=Int2(idum)
    Case(idc_fun_style_colorS)
	    call DlgGetI(Dlg,idc_fun_Style_Color,idc_fun_Style_ColorS,1,idum,-1,235,1)
      iFunStyleColor=Int2(idum)
    Case(idc_fun_mark_size)
	    call DlgGetI(Dlg,idc_fun_Mark_Size,idc_fun_Mark_SizeS,0,idum,0,20,7)
      iFunMarkSize=Int2(idum)
    Case(idc_fun_mark_sizeS)
	    call DlgGetI(Dlg,idc_fun_Mark_Size,idc_fun_Mark_SizeS,1,idum,0,20,7)
      iFunMarkSize=Int2(idum)
    Case(idc_fun_mark_step)
	    call DlgGetI(Dlg,idc_fun_Mark_Step,idc_fun_Mark_StepS,0,idum,1,1000,10)
      iFunMarkStep=Int2(idum)
    Case(idc_fun_mark_stepS)
	    call DlgGetI(Dlg,idc_fun_Mark_Step,idc_fun_Mark_StepS,1,idum,1,1000,10)
      iFunMarkStep=Int2(idum)
    Case(idc_fun_mark_color)
	    call DlgGetI(Dlg,idc_fun_Mark_Color,idc_fun_Mark_ColorS,0,idum,-1,235,-1)
      iFunMarkColor=Int2(idum)
    Case(idc_fun_mark_colorS)
	    call DlgGetI(Dlg,idc_fun_Mark_Color,idc_fun_Mark_ColorS,1,idum,-1,235,-1)
      iFunMarkColor=Int2(idum)
	  Case(idc_fun_delete)
      call GetFunctionData(dlg)
      if((iFunA2.gt.0).and.(mFunA.gt.1)) then
        do idum=iFunA2,mFunA-1
          FunATitle(idum)=FunATitle(idum+1)
          Fun(idum,1:mFun)=Fun(idum+1,1:mFun)
        end do
        call IncreaseFun(0,-1,ldum)
        iFunA2=min(iFunA2,mFunA)
        call SetFunctionData(dlg)
      end if
	  Case(idc_fun_read)
      call GetFunctionData(dlg)
      lAskFun=.true.
      lSkipFun=.false.
      lSkipFunHead=.false.
      iWarn=1
      call openFunction(.false.,iWarn)
      call SetFunctionData(dlg)
	  Case(idc_fun_write)
      call GetFunctionData(dlg)
      lAskFun=.true.
      lSkipFun=.false.
      lSkipFunHead=.false.
      iSaveFunction=0
      call saveFunction(.false.,.false.)
      call SetFunctionData(dlg)
	  Case(idc_fun_default)
      call Functions_Defaults(.true.)
      call SetFunctionData(dlg)
	  Case(IDC_fun_POLAR,IDC_fun_POLAR_DB,IDC_fun_POLAR_2)
      call GetFunctionData(dlg)
      call SetFunctionData(dlg)
	  Case(idcancel)
      call GetFunctionData(dlg)
     	call DlgExit(dlg)
	  end Select
	end Subroutine updateFunction

  recursive Subroutine IntegralDialog(lCheck)
! Dialog for setting parameters of the various boundaries
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical ldum,lCheck
	  Type(dialog) Dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Integral,Dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Integral dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetIntegralData(Dlg)
      ldum=DlgSetSub(Dlg,idc_int_kBnd,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_kBndS,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_kObj,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_kObjS,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_BndInt,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_RecInt,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_ObjInt,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_Location,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_read,updateIntegral)
      ldum=DlgSetSub(Dlg,idc_int_write,updateIntegral)
      ldum=DlgSetSub(Dlg,idcancel,updateIntegral)
	  end if
    ldum=DlgModal(Dlg)
	  call DlgUnInit(Dlg)
    IsOpen=0
    select case(iDlgExit)
    case(1)
      call TBndInt(.false.)
    case(2)
      call TRecInt(.false.)
    case(3)
      call TObjInt(.false.)
    end select
  end Subroutine IntegralDialog

  Subroutine SetIntegralData(Dlg)
! set dialog boxes containing boundary data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Type(dialog) Dlg
    call DlgSetI(Dlg,idc_int_kBnd,idc_int_kBndS,iBndInt,-nBnd,nBnd)
    call DlgSetI(Dlg,idc_int_kObj,idc_int_kObjS,iObjInt,-nObj,nObj)
    call DlgSetI(Dlg,idc_int_iter,0,MaxIntIter,1,1000000)
    call DlgSetI(Dlg,idc_int_iord,0,IntOrd,0,10)
    call DlgSetI(Dlg,idc_int_nx,0,IntNx,1,10000)
    call DlgSetI(Dlg,idc_int_ny,0,IntNy,1,10000)
    call DlgSetI(Dlg,idc_int_nBnd,0,nBnd,1,1000000)
    call DlgSetI(Dlg,idc_int_nObj,0,nObj,1,1000000)
    call DlgSetI(Dlg,idc_int_iErr,0,IntErr,-10000,10000)
    call DlgSetI(Dlg,idc_int_icall,0,IntCall,0,1000000)
    idum=Int4(tBnd(Max(1,iBndInt))%iLDom)
    call DlgSetI(Dlg,idc_int_LDom,0,idum,-200,200)
    idum=Int4(tBnd(Max(1,iBndInt))%iRDom)
    call DlgSetI(Dlg,idc_int_RDom,0,idum,-200,200)
	  call DlgSetR(Dlg,idc_int_xmin,XminInt,7)
	  call DlgSetR(Dlg,idc_int_xmax,XmaxInt,7)
	  call DlgSetR(Dlg,idc_int_ymin,YminInt,7)
	  call DlgSetR(Dlg,idc_int_ymax,YmaxInt,7)
	  call DlgSetR(Dlg,idc_int_r,rSphInt,7)
	  call DlgSetR(Dlg,idc_int_acc,AccIntegral,5)
	  call DlgSetR(Dlg,idc_int_result,currentIntegral,7)
	  call DlgSetR(Dlg,idc_int_resultmax,currentIntegralMax,7)
	  call DlgSetR(Dlg,idc_int_resultmin,currentIntegralMin,7)
	  call DlgSetR(Dlg,idc_int_resultmaxx,currentIntegralMaxL(1),7)
	  call DlgSetR(Dlg,idc_int_resultmaxy,currentIntegralMaxL(2),7)
	  call DlgSetR(Dlg,idc_int_resultmaxz,currentIntegralMaxL(3),7)
	  call DlgSetR(Dlg,idc_int_resultminx,currentIntegralMinL(1),7)
	  call DlgSetR(Dlg,idc_int_resultminy,currentIntegralMinL(2),7)
	  call DlgSetR(Dlg,idc_int_resultminz,currentIntegralMinL(3),7)
    if(IntWhat.eq.-1) then
      call DlgSetL(dlg,idc_int_fx,.true.)
    else if(IntWhat.eq.-2) then
      call DlgSetL(dlg,idc_int_fy,.true.)
    else if(IntWhat.eq.-3) then
      call DlgSetL(dlg,idc_int_fz,.true.)
    else if(IntWhat.eq.1) then
      call DlgSetL(dlg,idc_int_fdl,.true.)
    else if(IntWhat.eq.2) then
      call DlgSetL(dlg,idc_int_fdn,.true.)
    else
      call DlgSetL(dlg,idc_int_1,.true.)
    end if
    if(IntType.eq.0) then
      call DlgSetL(dlg,idc_int_sum,.true.)
    else if(IntType.eq.1) then
      call DlgSetL(dlg,idc_int_gl,.true.)
    else if(IntType.eq.2) then
      call DlgSetL(dlg,idc_int_gk,.true.)
    else if(IntType.eq.3) then
      call DlgSetL(dlg,idc_int_hp,.true.)
    end if
    if(IntInter.eq.0) then
      call DlgSetL(dlg,idc_int_inter0,.true.)
    else if(IntInter.eq.1) then
      call DlgSetL(dlg,idc_int_inter1,.true.)
    else if(IntInter.eq.2) then
      call DlgSetL(dlg,idc_int_inter2,.true.)
    else if(IntInter.eq.3) then
      call DlgSetL(dlg,idc_int_inter3,.true.)
    end if
    if(IntField.eq.0) then
      call DlgSetL(dlg,idc_int_field0,.true.)
    else if(iabs(IntField).eq.1) then
      call DlgSetL(dlg,idc_int_fieldE,.true.)
    else if(iabs(IntField).eq.2) then
      call DlgSetL(dlg,idc_int_fieldH,.true.)
    else if(iabs(IntField).eq.3) then
      call DlgSetL(dlg,idc_int_fieldS,.true.)
    else if(iabs(IntField).eq.4) then
      call DlgSetL(dlg,idc_int_fieldF,.true.)
    end if
    if(IntField.lt.0) then
      call DlgSetL(dlg,idc_int_field_aver,.true.)
    else
      call DlgSetL(dlg,idc_int_field_aver,.false.)
    end if
    call DlgSetL(dlg,idc_int_field_abs,lAbsInt)
    call DlgSetL(dlg,idc_int_field_eb,leBInt)
  end Subroutine SetIntegralData

  Subroutine GetIntegralData(Dlg)
! get Integral data
    Implicit none
	  Include 'RESOURCE.FD'
    Logical ldum
	  Type(dialog) Dlg
	  call DlgGetI(Dlg,idc_int_kBnd,idc_int_kBndS,0,iBndInt,-nBnd,nBnd,1)
	  call DlgGetI(Dlg,idc_int_kObj,idc_int_kObjS,0,iObjInt,-nObj,nObj,1)
	  call DlgGetI(Dlg,idc_int_iter,0,0,MaxIntIter,1,1000000,1000)
	  call DlgGetI(Dlg,idc_int_iord,0,0,IntOrd,0,10,1)
	  call DlgGetI(Dlg,idc_int_nx,0,0,IntNx,0,10000,10)
	  call DlgGetI(Dlg,idc_int_ny,0,0,IntNy,0,10000,10)
	  call DlgGetR(Dlg,idc_int_xmin,XminInt,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_int_xmax,XmaxInt,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_int_ymin,YminInt,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_int_ymax,YmaxInt,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_int_r,rSphInt,nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_int_acc,AccIntegral,0.0d0,1.0d0,1.0d-2,5)
    call DlgGetL(dlg,idc_int_fx,ldum)
    if(ldum) IntWhat=-1
    call DlgGetL(dlg,idc_int_fy,ldum)
    if(ldum) IntWhat=-2
    call DlgGetL(dlg,idc_int_fz,ldum)
    if(ldum) IntWhat=-3
    call DlgGetL(dlg,idc_int_1,ldum)
    if(ldum) IntWhat=0
    call DlgGetL(dlg,idc_int_fdl,ldum)
    if(ldum) IntWhat=1
    call DlgGetL(dlg,idc_int_fdn,ldum)
    if(ldum) IntWhat=2
    call DlgGetL(dlg,idc_int_sum,ldum)
    if(ldum) IntType=0
    call DlgGetL(dlg,idc_int_gl,ldum)
    if(ldum) IntType=1
    call DlgGetL(dlg,idc_int_gk,ldum)
    if(ldum) IntType=2
    call DlgGetL(dlg,idc_int_hp,ldum)
    if(ldum) IntType=3
    call DlgGetL(dlg,idc_int_inter0,ldum)
    if(ldum) IntInter=0
    call DlgGetL(dlg,idc_int_inter1,ldum)
    if(ldum) IntInter=1
    call DlgGetL(dlg,idc_int_inter2,ldum)
    if(ldum) IntInter=2
    call DlgGetL(dlg,idc_int_inter3,ldum)
    if(ldum) IntInter=3
    call DlgGetL(dlg,idc_int_field0,ldum)
    if(ldum) IntField=0
    call DlgGetL(dlg,idc_int_fieldE,ldum)
    if(ldum) IntField=1
    call DlgGetL(dlg,idc_int_fieldH,ldum)
    if(ldum) IntField=2
    call DlgGetL(dlg,idc_int_fieldS,ldum)
    if(ldum) IntField=3
    call DlgGetL(dlg,idc_int_fieldF,ldum)
    if(ldum) IntField=4
    call DlgGetL(dlg,idc_int_field_aver,ldum)
    if(ldum) IntField=-IntField
    call DlgGetL(dlg,idc_int_field_abs,lAbsInt)
    call DlgGetL(dlg,idc_int_field_eb,lebInt)
  end Subroutine GetIntegralData

	Subroutine updateIntegral(Dlg,control_name,callbackType)
! callback for IntegralDialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum
	  Type (dialog) Dlg
	  idum=callbackType
    iGetData=0
	  Select Case(control_name)
	  Case(idc_int_kBnd)
	    call DlgGetI(Dlg,idc_int_kBnd,idc_int_kBndS,0,iBndInt,-nBnd,nBnd,1)
      call SetIntegralData(Dlg)
	  Case(idc_int_kBndS)
	    call DlgGetI(Dlg,idc_int_kBnd,idc_int_kBndS,1,iBndInt,-nBnd,nBnd,1)
      call SetIntegralData(Dlg)
	  Case(idc_int_kObj)
	    call DlgGetI(Dlg,idc_int_kObj,idc_int_kObjS,0,iObjInt,-nObj,nObj,1)
      call SetIntegralData(Dlg)
	  Case(idc_int_kObjS)
	    call DlgGetI(Dlg,idc_int_kObj,idc_int_kObjS,1,iObjInt,-nObj,nObj,1)
      call SetIntegralData(Dlg)
	  Case(idc_int_BndInt)
      call GetIntegralData(Dlg)
      iDlgExit=1
     	call DlgExit(Dlg)
	  Case(idc_int_RecInt)
      call GetIntegralData(Dlg)
      iDlgExit=2
     	call DlgExit(Dlg)
	  Case(idc_int_ObjInt)
      call GetIntegralData(Dlg)
      iDlgExit=3
     	call DlgExit(Dlg)
	  Case(idc_int_Location)
      space=spaceInt
      SpaceText='Space for integration'C
      iSpaceRead=3
      call SpaceDialog(.true.)
      spaceInt=space
      idum=MessageBoxQQ('Orthonormalize vectors?'C,'Integral dialog'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDYES) call Ortho3DSpace(spaceInt)
	  Case(idc_int_read)
      call GetIntegralData(Dlg)
     	call openIntegral(.false.)
      call SetIntegralData(Dlg)
	  Case(idc_int_write)
      call GetIntegralData(Dlg)
     	call saveIntegral(.false.)
      call SetIntegralData(Dlg)
	  Case(idcancel)
      call GetIntegralData(Dlg)
      iDlgExit=0
     	call DlgExit(Dlg)
	  end Select
	end Subroutine updateIntegral

END MODULE CHDIA
