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
MODULE CHDIC

! Objects

  USE CHMOV

  SAVE

  CONTAINS

  Subroutine ProjectDialog(lCheck)
! dialog for the project data
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical lCheck,ldum
	  Type(dialog) dlg
    Integer(4) idum
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
	  ldum=DlgInit(IDD_Project,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Project dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      if(.not.lgcFld) lGet3DMat=.true.
      call SetProjectData(dlg)
	    ldum=DlgSetSub(dlg,idc_Pro_lzPer,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_ixySymm,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_ixySymmS,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_ixzSymm,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_ixzSymmS,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_iyzSymm,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_iyzSymmS,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_read,updateProject)
	    ldum=DlgSetSub(dlg,idc_Pro_write,updateProject)
	    ldum=DlgSetSub(dlg,idcancel,updateProject)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine ProjectDialog

  Subroutine SetProjectData(dlg)
! set the Project data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Complex(8) dumC
	  Type(dialog) dlg
    call DlgSetL(dlg,idc_Pro_lfcFld,lfcFld)
    call DlgSetL(dlg,idc_Pro_lgcFld,lgcFld)
    call DlgSetL(dlg,idc_Pro_lrPeriod,lregularPeriod)
    call DlgSetL(dlg,idc_Pro_lxPeriod,lxPeriod)
    call DlgSetL(dlg,idc_Pro_lyPeriod,lyPeriod)
    call DlgSetL(dlg,idc_Pro_lzPeriod,lzPeriod)
    call DlgSetL(dlg,idc_Pro_lzPer,lzPer)
    call DlgSetL(dlg,idc_Pro_lEigen,lEigen)
    call DlgSetL(dlg,idc_Pro_lEigenSave,lEigenSave)
    call DlgSetL(dlg,idc_Pro_lEigenSaveFld,lWriteRoughFld)
    if(iEigen.lt.1) then
      call DlgSetL(dlg,idc_Pro_lfEigen,.true.)
      call DlgSetI(Dlg,idc_Pro_iEigen,0,0,0,10000)
    end if
    if(iEigen.eq.1) then
      call DlgSetL(dlg,idc_Pro_lgEigen,.true.)
      call DlgSetI(Dlg,idc_Pro_iEigen,0,0,0,10000)
    end if
    if(iEigen.gt.1) then
      call DlgSetL(dlg,idc_Pro_lcEigen,.true.)
      call DlgSetI(Dlg,idc_Pro_iEigen,0,Int4(iEigen)-1,1,7)
    end if
    if(iHEGlobal.eq.0_2) then
      call DlgSetL(dlg,idc_Pro_lE,.true.)
    else if(iHEGlobal.eq.1_2) then
      call DlgSetL(dlg,idc_Pro_lH,.true.)
    else if(iHEGlobal.eq.2_2) then
      call DlgSetL(dlg,idc_Pro_lHE,.true.)
    end if
    call DlgSetI(Dlg,idc_Pro_iWriteDigits,0,Int4(iWriteDigits),1,11)
    call DlgSetI(Dlg,idc_Pro_ixySymm,idc_Pro_ixySymmS,Int4(ixySymm),0,2)
    call DlgSetI(Dlg,idc_Pro_ixzSymm,idc_Pro_ixzSymmS,Int4(ixzSymm),0,2)
    call DlgSetI(Dlg,idc_Pro_iyzSymm,idc_Pro_iyzSymmS,Int4(iyzSymm),0,2)
    call DlgSetI(Dlg,idc_Pro_nrEigen,0,Int4(nrEigen),0,10000)
    call DlgSetI(Dlg,idc_Pro_niEigen,0,Int4(niEigen),0,10000)
    call DlgSetI(Dlg,idc_Pro_imEigen,0,Int4(imEigen),1,100)
    call DlgSetI(Dlg,idc_Pro_mmEigen,0,Int4(mmEigen),1,100)
    call DlgSetI(Dlg,idc_Pro_itmEigen,0,Int4(itmEigen),-10000,10000)
    call DlgSetR(Dlg,idc_Pro_aEigen,aEigen,7)
    call DlgSetR(Dlg,idc_Pro_fEigen,fEigen,7)
    call DlgSetR(Dlg,idc_Pro_xPeriod,xPeriod,7)
    call DlgSetR(Dlg,idc_Pro_yPeriodx,yPeriodVector(1),7)
    call DlgSetR(Dlg,idc_Pro_yPeriody,yPeriodVector(2),7)
    call DlgSetR(Dlg,idc_Pro_zPeriodx,zPeriodVector(1),7)
    call DlgSetR(Dlg,idc_Pro_zPeriody,zPeriodVector(2),7)
    call DlgSetR(Dlg,idc_Pro_zPeriodz,zPeriodVector(3),7)
    call setupPeriod()
    call DlgSetR(Dlg,idc_Pro_cxrPeriod,Dble(cxPeriodDlg),7)
    call DlgSetR(Dlg,idc_Pro_cxiPeriod,DImag(cxPeriodDlg),7)
    call DlgSetR(Dlg,idc_Pro_cyrPeriod,Dble(cyPeriodDlg),7)
    call DlgSetR(Dlg,idc_Pro_cyiPeriod,DImag(cyPeriodDlg),7)
    call DlgSetR(Dlg,idc_Pro_czrPeriod,Dble(czPeriodDlg),7)
    call DlgSetR(Dlg,idc_Pro_cziPeriod,DImag(czPeriodDlg),7)
    call DlgSetR(Dlg,idc_Pro_frFld,Dble(fcFld),10)
    call DlgSetR(Dlg,idc_Pro_fiFld,Dimag(fcFld),10)
    if(lzPer) then
      dumC=dcFld
    else
      dumC=gcFld
    end if
    call DlgSetR(Dlg,idc_Pro_grFld,Dble(dumC),10)
    call DlgSetR(Dlg,idc_Pro_giFld,Dimag(dumC),10)
    call DlgSetR(Dlg,idc_Pro_c1rEigen,Dble(c1Eigen),10)
    call DlgSetR(Dlg,idc_Pro_c1iEigen,Dimag(c1Eigen),10)
    call DlgSetR(Dlg,idc_Pro_c2rEigen,Dble(c2Eigen),10)
    call DlgSetR(Dlg,idc_Pro_c2iEigen,Dimag(c2Eigen),10)
    call DlgSetR(Dlg,idc_Pro_c1rEigen2,Dble(c1EigenC),10)
    call DlgSetR(Dlg,idc_Pro_c1iEigen2,Dimag(c1EigenC),10)
    call DlgSetR(Dlg,idc_Pro_c2rEigen2,Dble(c2EigenC),10)
    call DlgSetR(Dlg,idc_Pro_c2iEigen2,Dimag(c2EigenC),10)
  end Subroutine SetProjectData

  Subroutine GetProjectData(dlg)
! get the Project data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) dumR,dumI,dumR1,dumI1
    Integer(4) idum
    Logical ldum
	  Type(dialog) dlg
    call DlgGetL(dlg,idc_Pro_lfcFld,lfcFld)
    call DlgGetL(dlg,idc_Pro_lgcFld,lgcFld)
    call DlgGetL(dlg,idc_Pro_lrPeriod,lregularPeriod)
    call DlgGetL(dlg,idc_Pro_lxPeriod,lxPeriod)
    call DlgGetL(dlg,idc_Pro_lyPeriod,lyPeriod)
    call DlgGetL(dlg,idc_Pro_lzPeriod,lzPeriod)
    call DlgGetL(dlg,idc_Pro_lzPer,lzPer)
    call DlgGetL(dlg,idc_Pro_lEigen,lEigen)
    call DlgGetL(dlg,idc_Pro_lEigenSave,lEigenSave)
    call DlgGetL(dlg,idc_Pro_lEigenSaveFld,lWriteRoughFld)
    call DlgGetL(dlg,idc_Pro_lfEigen,ldum)
    if(ldum) iEigen=0
    call DlgGetL(dlg,idc_Pro_lgEigen,ldum)
    if(ldum) iEigen=1
    call DlgGetL(dlg,idc_Pro_lcEigen,ldum)
    if(ldum) then
      iEigen=2
  	  call DlgGetI(Dlg,idc_Pro_iEigen,0,0,idum,0,7,1)
      iEigen=max(iEigen+idum-1,2)
    end if
    call DlgGetL(dlg,idc_Pro_lE,ldum)
    if(ldum) then
      iHEGlobal=0_2
    else
      call DlgGetL(dlg,idc_Pro_lH,ldum)
      if(ldum) then
        iHEGlobal=1_2
      else
        iHEGlobal=2_2
      end if
    end if
    call DlgGetI(Dlg,idc_Pro_iWriteDigits,0,0,idum,1,11,7)
    iWriteDigits=Int2(idum)
	  call DlgGetI(Dlg,idc_Pro_ixySymm,idc_Pro_ixySymmS,0,ixySymm,0,2,0)
	  call DlgGetI(Dlg,idc_Pro_ixzSymm,idc_Pro_ixzSymmS,0,ixzSymm,0,2,0)
	  call DlgGetI(Dlg,idc_Pro_iyzSymm,idc_Pro_iyzSymmS,0,iyzSymm,0,2,0)
	  call DlgGetI(Dlg,idc_Pro_nrEigen,0,0,nrEigen,0,10000,10)
	  call DlgGetI(Dlg,idc_Pro_niEigen,0,0,niEigen,0,10000,10)
	  call DlgGetI(Dlg,idc_Pro_imEigen,0,0,imEigen,1,100,1)
	  call DlgGetI(Dlg,idc_Pro_itmEigen,0,0,itmEigen,-10000,10000,100)
	  call DlgGetR(Dlg,idc_Pro_aEigen,aEigen,0.0d0,1.0d0,0.01d0,7)
	  call DlgGetR(Dlg,idc_Pro_fEigen,fEigen,0.0d0,1.0d0,0.01d0,7)
	  call DlgGetR(Dlg,idc_Pro_xPeriod,xPeriod,0.0d0,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_yPeriodx,yPeriodVector(1),0.0d0,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_yPeriody,yPeriodVector(2),0.0d0,pBig,1.0d0,7)
    yPeriodVector(3)=0.0d0
	  call DlgGetR(Dlg,idc_Pro_zPeriodx,zPeriodVector(1),0.0d0,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_zPeriody,zPeriodVector(2),0.0d0,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_zPeriodz,zPeriodVector(3),0.0d0,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_cxrPeriod,dumR,nBig,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_cxiPeriod,dumI,nBig,pBig,0.0d0,7)
    cxPeriodDlg=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Pro_cyrPeriod,dumR,nBig,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_cyiPeriod,dumI,nBig,pBig,0.0d0,7)
    cyPeriodDlg=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Pro_czrPeriod,dumR,nBig,pBig,1.0d0,7)
	  call DlgGetR(Dlg,idc_Pro_cziPeriod,dumI,nBig,pBig,0.0d0,7)
    czPeriodDlg=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Pro_frFld,dumR,nBig,pBig,1.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_fiFld,dumI,nBig,pBig,0.0d0,10)
    fcFld=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Pro_grFld,dumR,nBig,pBig,1.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_giFld,dumI,nBig,pBig,0.0d0,10)
    if(lzPer) then
      dcFld=DCmplx(dumR,0.0d0)
      gcFld=0.5d0/(dcFld*fcFld*kw0)
    else
      gcFld=DCmplx(dumR,dumI)
      dcFld=DCmplx(Dble(0.5d0/(gcFld*fcFld*kw0)),0.0d0)
    end if
	  call DlgGetR(Dlg,idc_Pro_c1rEigen,dumR,nBig,pBig,1.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_c1iEigen,dumI,nBig,pBig,0.0d0,10)
    c1Eigen=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Pro_c2rEigen,dumR,nBig,pBig,1.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_c2iEigen,dumI,nBig,pBig,0.0d0,10)
    c2Eigen=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Pro_c1rEigen2,dumR,nBig,pBig,1.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_c1iEigen2,dumI,nBig,pBig,0.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_c2rEigen2,dumR1,nBig,pBig,1.0d0,10)
	  call DlgGetR(Dlg,idc_Pro_c2iEigen2,dumI1,nBig,pBig,0.0d0,10)
    c1EigenC=DCmplx(min(dumR,dumR1),min(dumI,dumI1))
    c2EigenC=DCmplx(max(dumR,dumR1),max(dumI,dumI1))
    if(lfcFld.xor.lfcFldAll) then
      idum=MessageBoxQQ('Changing time/frequency dependent fields \rrequires clearing the original field!&
      &\rThis might take some time!'C, &
      &'OK to clear the original field?'C,MB$YESNO.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) then
        lfcFld=lfcFldAll
        call DlgSetL(dlg,idc_Pro_lfcFld,lfcFld)
      else
        call ClearField(ldum)
       ! call TClrFld(.true.)
      end if
    end if
    lMMPstat=.false.
    if((cdAbs(fcFld).lt.pSmall).or.(.not.lfcFld)) lMMPstat=.true.
  end Subroutine GetProjectData

  Subroutine updateProject(dlg,control_name,callbackType)
  ! callback for ProjectDialog
    Implicit none
    Include 'RESOURCE.FD'
    Real(8) dumR,dumI
    Integer(4) control_name,callbackType,idum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idc_Pro_lzPer)
      call DlgGetL(dlg,idc_Pro_lzPer,lzPer)
	    call DlgGetR(Dlg,idc_Pro_grFld,dumR,nBig,pBig,1.0d0,5)
	    call DlgGetR(Dlg,idc_Pro_giFld,dumI,nBig,pBig,0.0d0,5)
      if(.not.lzPer) then
        dcFld=DCmplx(dumR,0.0d0)
        gcFld=0.5d0/(dcFld*fcFld*kw0)
        call DlgSetR(Dlg,idc_Pro_grFld,Dble(gcFld),5)
        call DlgSetR(Dlg,idc_Pro_giFld,Dimag(gcFld),5)
      else
        gcFld=DCmplx(dumR,dumI)
        dcFld=DCmplx(Dble(0.5d0/(gcFld*fcFld*kw0)),0.0d0)
        call DlgSetR(Dlg,idc_Pro_grFld,Dble(dcFld),5)
        call DlgSetR(Dlg,idc_Pro_giFld,Dimag(dcFld),5)
      end if
    Case(idc_Pro_ixySymm)
	    call DlgGetI(Dlg,idc_Pro_ixySymm,idc_Pro_ixySymmS,0,ixySymm,0,2,0)
    Case(idc_Pro_ixySymmS)
	    call DlgGetI(Dlg,idc_Pro_ixySymm,idc_Pro_ixySymmS,1,ixySymm,0,2,0)
    Case(idc_Pro_ixzSymm)
	    call DlgGetI(Dlg,idc_Pro_ixzSymm,idc_Pro_ixzSymmS,0,ixzSymm,0,2,0)
    Case(idc_Pro_ixzSymmS)
	    call DlgGetI(Dlg,idc_Pro_ixzSymm,idc_Pro_ixzSymmS,1,ixzSymm,0,2,0)
    Case(idc_Pro_iyzSymm)
	    call DlgGetI(Dlg,idc_Pro_iyzSymm,idc_Pro_iyzSymmS,0,iyzSymm,0,2,0)
    Case(idc_Pro_iyzSymmS)
	    call DlgGetI(Dlg,idc_Pro_iyzSymm,idc_Pro_iyzSymmS,1,iyzSymm,0,2,0)
    Case(idc_Pro_read)
      call GetProjectData(dlg)
      call openProAll(.false.)
      call SetProjectData(dlg)
    Case(idc_Pro_write)
      call GetProjectData(dlg)
      call SaveProAll(.false.)
      call SetProjectData(dlg)
    Case(idcancel)
      call GetProjectData(dlg)
      call getEUST()
      call setupPeriod()
      call CorrExpPar(1000_4)
		  call DlgExit(dlg)
    end Select
  end Subroutine updateProject

  Subroutine PFDDialog(lCheck)
! dialog for  PFD
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
	  ldum=DlgInit(IDD_PFD,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'PFD dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetPFDData(dlg)
	    ldum=DlgSetSub(dlg,idok,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_ADD,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_DEL,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_MOD,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_ADDs,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_DELs,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_MODs,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_kPFDsens,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_kPFDsensS,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_kPFDsource,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_kPFDsourceS,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_PFD_nPFDf,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_APPLY_PFD,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_READ_PFD,updatePFD)
	    ldum=DlgSetSub(dlg,IDC_WRITE_PFD,updatePFD)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine PFDDialog

  Subroutine SetPFDData(dlg)
! set the PFD data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Logical lh,le,lx,ly,lz,lp,lm,ls,lha,lpw
	  Type(dialog) dlg
    call DlgSetR(Dlg,IDC_PFD_DT,dtrFld,5)
    call DlgSetI(Dlg,IDC_PFD_N,0,nIterPFD,0,1000000)
	  call DlgSetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,kPFDsource,1,nPFDsource)
	  call DlgSetI(Dlg,IDC_PFD_nPFDsource,0,nPFDsource,1,32000)
	  call DlgSetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,kPFDsens,1,nPFDsens)
	  call DlgSetI(Dlg,IDC_PFD_nPFDsens,0,nPFDsens,1,32000)
	  call DlgSetI(Dlg,IDC_PFD_nPFDf,0,Int4(nPFDf),1,32000)
    call getPFDl(iPFDt,lh,le,lx,ly,lz,lp,lm,ls,lha,lpw)
    call DlgSetL(Dlg,IDC_PFD_Hard,lha)
    call DlgSetL(Dlg,IDC_PFD_Soft,ls)
    call DlgSetL(Dlg,IDC_PFD_lP,lp)
    call DlgSetL(Dlg,IDC_PFD_lMMP,lm)
    call DlgSetL(Dlg,IDC_PFD_lPW,lpw)
    call DlgSetL(Dlg,IDC_PFD_Efield,le)
    call DlgSetL(Dlg,IDC_PFD_Hfield,lh)
    call DlgSetL(Dlg,IDC_PFD_Xcomp,lx)
    call DlgSetL(Dlg,IDC_PFD_Ycomp,ly)
    call DlgSetL(Dlg,IDC_PFD_Zcomp,lz)
    call DlgSetL(Dlg,IDC_PFD_tExp,.false.)
    call DlgSetL(Dlg,IDC_PFD_tCos,.false.)
    call DlgSetL(Dlg,IDC_PFD_tPulse,.false.)
    call DlgSetL(Dlg,IDC_PFD_tRamp,.false.)
    call DlgSetL(Dlg,IDC_PFD_t0,.false.)
    Select case (abs(iPFDft))
    Case(0)
      call DlgSetL(Dlg,IDC_PFD_t0,.true.)
    Case(1)
      call DlgSetL(Dlg,IDC_PFD_tCos,.true.)
      call DlgSetL(Dlg,IDC_PFD_tPulse,.true.)
    Case(2)
      call DlgSetL(Dlg,IDC_PFD_tExp,.true.)
      call DlgSetL(Dlg,IDC_PFD_tRamp,.true.)
    Case(3)
      call DlgSetL(Dlg,IDC_PFD_tCos,.true.)
      call DlgSetL(Dlg,IDC_PFD_tRamp,.true.)
    Case Default
      call DlgSetL(Dlg,IDC_PFD_tExp,.true.)
      call DlgSetL(Dlg,IDC_PFD_tPulse,.true.)
    End Select
    call DlgSetL(Dlg,IDC_PFD_Tharm,.true.)
    if(iPFDft.lt.0) call DlgSetL(Dlg,IDC_PFD_Tharm,.false.)
	  call DlgSetI(Dlg,IDC_PFD_nx,0,Int4(nPFDi),1,32000)
	  call DlgSetI(Dlg,IDC_PFD_ny,0,Int4(nPFDj),1,32000)
	  call DlgSetI(Dlg,IDC_PFD_nz,0,Int4(nPFDk),1,32000)
	  call DlgSetI(Dlg,IDC_PFD_nLeft,0,Int4(nPFDil),-2,100)
	  call DlgSetI(Dlg,IDC_PFD_nRight,0,Int4(nPFDih),-2,100)
	  call DlgSetI(Dlg,IDC_PFD_nBottom,0,Int4(nPFDjl),-2,100)
	  call DlgSetI(Dlg,IDC_PFD_nTop,0,Int4(nPFDjh),-2,100)
	  call DlgSetI(Dlg,IDC_PFD_nBack,0,Int4(nPFDkl),-2,100)
	  call DlgSetI(Dlg,IDC_PFD_nFront,0,Int4(nPFDkh),-2,100)
	  call DlgSetI(Dlg,IDC_PFD_iEff,0,Int4(nPFDiEff),0,4)
	  call DlgSetI(Dlg,IDC_PFD_nScatt,0,Int4(nPFDsLayers),-2,100)
    call DlgSetR(Dlg,idc_Transform_P3,pForm(3),5)
	  call DlgSetR(Dlg,IDC_PFD_xmin,PFDxmin,5)
	  call DlgSetR(Dlg,IDC_PFD_xmax,PFDxmax,5)
	  call DlgSetR(Dlg,IDC_PFD_ymin,PFDymin,5)
	  call DlgSetR(Dlg,IDC_PFD_ymax,PFDymax,5)
	  call DlgSetR(Dlg,IDC_PFD_zmin,PFDzmin,5)
	  call DlgSetR(Dlg,IDC_PFD_zmax,PFDzmax,5)
	  call DlgSetR(Dlg,IDC_PFD_dx,PFDdx,5)
	  call DlgSetR(Dlg,IDC_PFD_dy,PFDdy,5)
	  call DlgSetR(Dlg,IDC_PFD_dz,PFDdz,5)
	  call DlgSetR(Dlg,IDC_PFD_Decay,PFDpml,5)
	  call DlgSetR(Dlg,IDC_PFD_Tmax,PFDfTmax,5)
	  call DlgSetR(Dlg,IDC_PFD_Tau,PFDfTau,5)
	  call DlgSetI(Dlg,IDC_PFD_iPFDs,0,Int4(iPFDs(kPFDsource)),1,Int4(nPFDi))
	  call DlgSetI(Dlg,IDC_PFD_jPFDs,0,Int4(jPFDs(kPFDsource)),1,Int4(nPFDj))
	  call DlgSetI(Dlg,IDC_PFD_kPFDs,0,Int4(kPFDs(kPFDsource)),1,Int4(nPFDk))
	  call DlgSetR(Dlg,IDC_PFD_SourceAr,Dble(PFDsourceA(kPFDsource)),5)
	  call DlgSetR(Dlg,IDC_PFD_SourceAi,Dimag(PFDsourceA(kPFDsource)),5)
	  if(kPFDsens.gt.0) then
      call DlgSetR(Dlg,IDC_PFD_PFDsensX,PFDsensX(kPFDsens),5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensY,PFDsensY(kPFDsens),5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensZ,PFDsensZ(kPFDsens),5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensT,PFDsensT(kPFDsens),5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensD,PFDsensD(kPFDsens),5)
    else
      call DlgSetR(Dlg,IDC_PFD_PFDsensX,0.0d0,5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensY,0.0d0,5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensZ,0.0d0,5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensT,0.0d0,5)
	    call DlgSetR(Dlg,IDC_PFD_PFDsensD,0.0d0,5)
    end if
	  call DlgSetR(Dlg,IDC_PFD_fmin,PFDfmin,5)
	  call DlgSetR(Dlg,IDC_PFD_fmax,PFDfmax,5)
  end Subroutine SetPFDData

  Subroutine GetPFDData(dlg)
! get the PFD data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) dumr,dumi
    Integer(4) idum
    logical lh,le,lx,ly,lz,lp,lm,ls,lpw
	  Type(dialog) dlg
    call DlgGetR(Dlg,IDC_PFD_DT,dtrFld,nBig,pBig,0.0d0,5)
    call DlgGetI(Dlg,IDC_PFD_N,0,0,nIterPFD,0,1000000,10)
	  call DlgGetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,0,kPFDsource,1,nPFDsource,1)
	  call DlgGetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,0,kPFDsens,1,nPFDsens,1)
	  call DlgGetI(Dlg,IDC_PFD_nPFDf,0,0,idum,1,32000,1)
    nPFDf=Int2(max(1,idum))
    call DlgGetL(Dlg,IDC_PFD_Xcomp,lx)
    call DlgGetL(Dlg,IDC_PFD_Ycomp,ly)
    call DlgGetL(Dlg,IDC_PFD_Zcomp,lz)
    call DlgGetL(Dlg,IDC_PFD_Efield,le)
    call DlgGetL(Dlg,IDC_PFD_Hfield,lh)
    call DlgGetL(Dlg,IDC_PFD_lP,lp)
    call DlgGetL(Dlg,IDC_PFD_lMMP,lm)
    call DlgGetL(Dlg,IDC_PFD_lPW,lpw)
    call DlgGetL(Dlg,IDC_PFD_Soft,ls)
    call getPFDi(lh,le,lx,ly,lz,lp,lm,ls,lpw,iPFDt)
    iPFDft=0
    call DlgGetL(Dlg,IDC_PFD_tCos,lx)
    if(lx) iPFDft=iPFDft+1
    call DlgGetL(Dlg,IDC_PFD_tRamp,lx)
    if(lx) iPFDft=iPFDft+2
    if(iPFDft.lt.1) iPFDft=4
    call DlgGetL(Dlg,IDC_PFD_tHarm,lx)
    if(.not.lx) iPFDft=-iPFDft
    call DlgGetL(Dlg,IDC_PFD_t0,lx)
    if(lx) iPFDft=0
	  call DlgGetI(Dlg,IDC_PFD_nx,0,0,idum,1,32000,41)
    nPFDi=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_ny,0,0,idum,1,32000,41)
    nPFDj=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nz,0,0,idum,1,32000,41)
    nPFDk=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nLeft,0,0,idum,-2,100,5)
    nPFDil=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nRight,0,0,idum,-2,100,5)
    nPFDih=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nBottom,0,0,idum,-2,100,5)
    nPFDjl=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nTop,0,0,idum,-2,100,5)
    nPFDjh=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nBack,0,0,idum,-2,100,5)
    nPFDkl=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nFront,0,0,idum,-2,100,5)
    nPFDkh=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_iEff,0,0,idum,0,4,1)
    nPFDiEff=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_nScatt,0,0,idum,-2,100,0)
    nPFDsLayers=Int2(idum)
	  call DlgGetR(Dlg,IDC_PFD_xmin,PFDxmin,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_xmax,PFDxmax,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_ymin,PFDymin,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_ymax,PFDymax,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_zmin,PFDzmin,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_zmax,PFDzmax,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_Decay,PFDpml,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_Tmax,PFDfTmax,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_Tau,PFDfTau,nBig,pBig,1.0d0,5)
    PFDdx=(PFDxmax-PFDxmin)/Dble(max(1,nPFDi-1))
    PFDdy=(PFDymax-PFDymin)/Dble(max(1,nPFDj-1))
    PFDdz=(PFDzmax-PFDzmin)/Dble(max(1,nPFDk-1))
	  call DlgGetI(Dlg,IDC_PFD_iPFDs,0,0,idum,1,Int4(nPFDi),Int4((nPFDi+1)/2))
    iPFDs(kPFDsource)=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_jPFDs,0,0,idum,1,Int4(nPFDj),Int4((nPFDj+1)/2))
    jPFDs(kPFDsource)=Int2(idum)
	  call DlgGetI(Dlg,IDC_PFD_kPFDs,0,0,idum,1,Int4(nPFDk),Int4((nPFDk+1)/2))
    kPFDs(kPFDsource)=Int2(idum)
	  call DlgGetR(Dlg,IDC_PFD_SourceAr,dumr,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_SourceAi,dumi,nBig,pBig,0.0d0,5)
    PFDsourceA(kPFDsource)=Dcmplx(dumr,dumi)
    if(kPFDsens.gt.0) then
	    call DlgGetR(Dlg,IDC_PFD_PFDsensX,PFDsensX(kPFDsens),nBig,pBig,0.0d0,5)
	    call DlgGetR(Dlg,IDC_PFD_PFDsensY,PFDsensY(kPFDsens),nBig,pBig,0.0d0,5)
	    call DlgGetR(Dlg,IDC_PFD_PFDsensZ,PFDsensZ(kPFDsens),nBig,pBig,0.0d0,5)
	    call DlgGetR(Dlg,IDC_PFD_PFDsensT,PFDsensT(kPFDsens),0.0d0,pBig,0.0d0,5)
	    call DlgGetR(Dlg,IDC_PFD_PFDsensD,PFDsensD(kPFDsens),0.0d0,pBig,0.0d0,5)
    end if
	  call DlgGetR(Dlg,IDC_PFD_fmin,PFDfmin,0.0d0,pBig,0.0d0,5)
	  call DlgGetR(Dlg,IDC_PFD_fmax,PFDfmax,PFDfmin,pBig,0.0d0,5)
  end Subroutine GetPFDData

  Subroutine updatePFD(dlg,control_name,callbackType)
  ! callback for TGridDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum
    Logical ldum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(IDC_PFD_kPFDsens)
	    call DlgGetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,0,kPFDsens,1,nPFDsens,1)
      call SetPFDData(Dlg)
    Case(IDC_PFD_kPFDsensS)
	    call DlgGetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,1,kPFDsens,1,nPFDsens,1)
      call SetPFDData(Dlg)
    Case(IDC_PFD_kPFDsource)
	    call DlgGetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,0,kPFDsource,1,nPFDsource,1)
      call SetPFDData(Dlg)
    Case(IDC_PFD_kPFDsourceS)
	    call DlgGetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,1,kPFDsource,1,nPFDsource,1)
      call SetPFDData(Dlg)
    Case(IDC_PFD_nPFDf)
	    call DlgGetI(Dlg,IDC_PFD_nPFDf,0,0,idum,1,32000,1)
      nPFDf=Int2(max(1,idum))
      nPFDsens=Max(1,nPFDsens)
      DeAllocate(cPFDsens,stat=idum)
      Allocate(cPFDsens(0:6,1:nPFDf,1:nPFDsens),stat=idum)
      nPFDsFld=0
      cPFDsens=(0.0d0,0.0d0)
    Case(IDC_PFD_ADD)
	    call DlgGetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,0,kPFDsens,1,nPFDsens,1)
      call InsertPFDsens(kPFDsens,1,ldum)
	    call DlgSetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,kPFDsens,1,nPFDsens)
      call GetPFDData(dlg)
      call SetPFDData(Dlg)
    Case(IDC_PFD_DEL)
      if(nPFDsens.gt.1) then
	      call DlgGetI(Dlg,IDC_PFD_kPFDsens,IDC_PFD_kPFDsensS,0,kPFDsens,1,nPFDsens,1)
        call InsertPFDsens(kPFDsens,-1,ldum)
        call SetPFDData(Dlg)
      end if
    Case(IDC_PFD_MOD,IDC_PFD_MODs)
      call GetPFDData(dlg)
      call SetPFDData(dlg)
    Case(IDC_PFD_ADDs)
	    call DlgGetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,0,kPFDsource,1,nPFDsource,1)
      call InsertPFDsource(kPFDsource,1,ldum)
	    call DlgSetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,kPFDsource,1,nPFDsource)
      call GetPFDData(dlg)
      call SetPFDData(Dlg)
    Case(IDC_PFD_DELs)
      if(nPFDsource.gt.1) then
	      call DlgGetI(Dlg,IDC_PFD_kPFDsource,IDC_PFD_kPFDsourceS,0,kPFDsource,1,nPFDsource,1)
        call InsertPFDsource(kPFDsource,-1,ldum)
        call SetPFDData(Dlg)
      end if
    Case(IDC_APPLY_PFD)
      call GetPFDData(dlg)
	    call DlgExit(dlg)
      call TTransFld(.false.)
    Case(IDC_READ_PFD)
      call GetPFDData(dlg)
      call OpenPFD(.false.)
      call GetPFDData(dlg)
    Case(IDC_WRITE_PFD)
      call GetPFDData(dlg)
      call SavePFD(.false.)
      call SetPFDData(dlg)
    Case(idok)
      call GetPFDData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updatePFD

  Subroutine TGridDialog(lCheck)
! dialog for the grid transform formula
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
	  ldum=DlgInit(IDD_TGRID,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Grid transform dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetTGridData(dlg)
	    ldum=DlgSetSub(dlg,idc_Conform_Check,updateTGrid)
	    ldum=DlgSetSub(dlg,idc_Conform_Apply,updateTGrid)
	    ldum=DlgSetSub(dlg,idc_Transform_Check,updateTGrid)
	    ldum=DlgSetSub(dlg,idc_Transform_Apply,updateTGrid)
	    ldum=DlgSetSub(dlg,idc_read_grt,updateTGrid)
	    ldum=DlgSetSub(dlg,idc_write_grt,updateTGrid)
	    ldum=DlgSetSub(dlg,idcancel,updateTGrid)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine TGridDialog

  Subroutine SetTGridData(dlg)
! set the TGrid data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    Con_Def_Form(0)=Con_Form
    call DlgSetSn(Dlg,idc_Conform_F,Con_Form,4,Con_Def_Form(0:3))
    call DlgSetR(Dlg,idc_Conform_P0R,Dble(pCForm(0)),5)
    call DlgSetR(Dlg,idc_Conform_P0I,DImag(pCForm(0)),5)
    call DlgSetR(Dlg,idc_Conform_P1R,Dble(pCForm(1)),5)
    call DlgSetR(Dlg,idc_Conform_P1I,DImag(pCForm(1)),5)
    call DlgSetR(Dlg,idc_Conform_P2R,Dble(pCForm(2)),5)
    call DlgSetR(Dlg,idc_Conform_P2I,DImag(pCForm(2)),5)
    call DlgSetR(Dlg,idc_Conform_P3R,Dble(pCForm(3)),5)
    call DlgSetR(Dlg,idc_Conform_P3I,DImag(pCForm(3)),5)
    call DlgSetSn(Dlg,idc_Transform_X,Trns_Form(1),4,Trns_Def_Form(0:3,1))
    call DlgSetSn(Dlg,idc_Transform_Y,Trns_Form(2),4,Trns_Def_Form(0:3,2))
    call DlgSetSn(Dlg,idc_Transform_Z,Trns_Form(3),4,Trns_Def_Form(0:3,3))
    call DlgSetR(Dlg,idc_Transform_P0,pForm(0),5)
    call DlgSetR(Dlg,idc_Transform_P1,pForm(1),5)
    call DlgSetR(Dlg,idc_Transform_P2,pForm(2),5)
    call DlgSetR(Dlg,idc_Transform_P3,pForm(3),5)
  end Subroutine SetTGridData

  Subroutine GetTGridData(dlg)
! get the TGrid data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
    Real(8) dumR,dumI
	  Type(dialog) dlg
    call DlgGetS(Dlg,idc_Conform_F,Con_Form)
	  call DlgGetR(Dlg,idc_Conform_P0R,dumR,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Conform_P0I,dumI,nBig,pBig,0.0d0,5)
    pCForm(0)=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Conform_P1R,dumR,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Conform_P1I,dumI,nBig,pBig,0.0d0,5)
    pCForm(1)=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Conform_P2R,dumR,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Conform_P2I,dumI,nBig,pBig,0.0d0,5)
    pCForm(2)=DCmplx(dumR,dumI)
	  call DlgGetR(Dlg,idc_Conform_P3R,dumR,nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Conform_P3I,dumI,nBig,pBig,0.0d0,5)
    pCForm(3)=DCmplx(dumR,dumI)
    call DlgGetS(Dlg,idc_Transform_X,Trns_Form(1))
    call DlgGetS(Dlg,idc_Transform_Y,Trns_Form(2))
    call DlgGetS(Dlg,idc_Transform_Z,Trns_Form(3))
	  call DlgGetR(Dlg,idc_Transform_P0,pForm(0),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Transform_P1,pForm(1),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Transform_P2,pForm(2),nBig,pBig,1.0d0,5)
	  call DlgGetR(Dlg,idc_Transform_P3,pForm(3),nBig,pBig,1.0d0,5)
  end Subroutine GetTGridData

  Subroutine updateTGrid(dlg,control_name,callbackType)
  ! callback for TGridDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) control_name,callbackType,idum,ie,lfdum
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idc_Conform_Check)
      call GetTGridData(dlg)
      cForm(6:8)=c678(1:3,3)
      cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
      call checkCFormula(Con_Form,lfdum,8,3,0,idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('ERROR in Formula!'C,'Grid transformation dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('No errors found!'C,'Grid transformation dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      end if
      call SetTGridData(dlg)
    Case(idc_Conform_Apply)
      call GetTGridData(dlg)
	    call DlgExit(dlg)
      call TTransGrd(.true.)
    Case(idc_Transform_Check)
      call GetTGridData(dlg)
      ie=0
      cForm(6:8)=c678(1:3,3)
      cCForm(6:8)=Dcmplx(cForm(6:8),0.0d0)
      call checkFormula(Trns_Form(1),lfdum,8,3,3,idum)
      if(idum.ne.0) ie=ie+1
      call checkFormula(Trns_Form(2),lfdum,8,3,3,idum)
      if(idum.ne.0) ie=ie+1
      call checkFormula(Trns_Form(3),lfdum,8,3,3,idum)
      if(idum.ne.0) ie=ie+1
      if(ie.eq.0) then
        idum=MessageBoxQQ('No errors found!'C,'Grid transformation dialog'C, &
                          MB$OK.or.MB$IconExclamation)
      else
        idum=MessageBoxQQ('Errors found!\rCheck formula with leading exclamation mark!'C, &
        &                 'Grid transformation dialog'C,MB$OK.or.MB$IconExclamation)
      end if
      call SetTGridData(dlg)
    Case(idc_Transform_Apply)
      call GetTGridData(dlg)
	    call DlgExit(dlg)
      call TTransGrd(.false.)
    Case(idc_read_grt)
      call GetTGridData(dlg)
      call OpenTGrid(.false.)
      call SetTGridData(dlg)
    Case(idc_write_grt)
      call GetTGridData(dlg)
      call SaveTGrid(.false.)
      call SetTGridData(dlg)
    Case(idcancel)
      call GetTGridData(dlg)
	    call DlgExit(dlg)
    end Select
  end Subroutine updateTGrid

  Subroutine TransDialog(lCheck)
! dialog for the Trans data
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
	  ldum=DlgInit(IDD_Trans,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Transform dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetTransData(dlg)
	    ldum=DlgSetSub(dlg,idOK,updateTrans)
	    ldum=DlgSetSub(dlg,idcancel,updateTrans)
	  end if
    ldum=DlgModal(dlg)
    call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine TransDialog

  Subroutine SetTransData(dlg)
! set the Trans data in its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
    call DlgSetS0(Dlg,idc_Trans_T1,TransText(1))
    call DlgSetS0(Dlg,idc_Trans_T2,TransText(2))
    call DlgSetS0(Dlg,idc_Trans_T3,TransText(3))
    call DlgSetR(Dlg,idc_Trans_R1,Trans(1),7)
    call DlgSetR(Dlg,idc_Trans_R2,Trans(2),7)
    call DlgSetR(Dlg,idc_Trans_R3,Trans(3),7)
  end Subroutine SetTransData

  Subroutine GetTransData(dlg)
! get the Trans data from its dialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Type(dialog) dlg
	  call DlgGetR(Dlg,idc_Trans_R1,Trans(1),nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_Trans_R2,Trans(2),nBig,pBig,0.0d0,7)
	  call DlgGetR(Dlg,idc_Trans_R3,Trans(3),nBig,pBig,0.0d0,7)
  end Subroutine GetTransData

  Subroutine updateTrans(dlg,control_name,callbackType)
  ! callback for TransDialog
    Implicit none
    Include 'RESOURCE.FD'
    Integer(4) idum,control_name,callbackType
    Type (dialog) dlg
    idum=callbackType
    Select Case(control_name)
    Case(idok)
      call GetTransData(dlg)
      lTrans=.true.
	    call DlgExit(dlg)
    Case(idcancel)
	    call DlgExit(dlg)
      lTrans=.false.
    end Select
  end Subroutine updateTrans

  Subroutine WindowDialog(lCheck)
! Dialog for the data of the graphic windows
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical lCheck,ldum
	  Type(dialog) dlg
    Integer(4), save:: IsOpen
    Data IsOpen/0/
    if(IsOpen.ne.0) return
	  ldum=lCheck
    call GetKWin(.true.)
	  ldum=DlgInit(IDD_WINDOW,dlg)
	  if(.not.ldum) then
      idum=MessageBoxQQ('Dialog initialization failed!'C,'Window dialog'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
	  else
      IsOpen=1
      call SetWindowData(dlg)
	    ldum=DlgSetSub(dlg,idc_win_set_title,updateCHWindow)
	    ldum=DlgSetSub(dlg,idc_win_read_pal,updateCHWindow)
	    ldum=DlgSetSub(dlg,idc_win_read,updateCHWindow)
	    ldum=DlgSetSub(dlg,idc_win_write,updateCHWindow)
	    ldum=DlgSetSub(dlg,idc_win_defaults,updateCHWindow)
	    ldum=DlgSetSub(dlg,idcancel,updateCHWindow)
	  end if
    ldum=DlgModal(dlg)
	  call DlgUnInit(dlg)
    IsOpen=0
  end Subroutine WindowDialog

  Subroutine SetWindowData(dlg)
! set dialog boxes containing graphic window data
    Implicit none
	  Include 'RESOURCE.FD'
	  Logical ldum
	  Type(dialog) dlg
    call DlgSetI(Dlg,idc_win_kwin,0,Int4(kWin),1,Int4(mWin))
    call DlgSetI(Dlg,idc_win_width,0,Int4(iWinWidth(kWin)),10,10000)
    call DlgSetI(Dlg,idc_win_height,0,Int4(iWinHeight(kWin)),10,10000)
    call DlgSetI(Dlg,idc_win_left,0,Int4(iWinLeft(kWin)),0,100)
    call DlgSetI(Dlg,idc_win_right,0,Int4(iWinRight(kWin)),0,100)
    call DlgSetI(Dlg,idc_win_top,0,Int4(iWinTop(kWin)),0,100)
    call DlgSetI(Dlg,idc_win_bottom,0,Int4(iWinBottom(kWin)),0,100)
    call DlgSetI(Dlg,idc_win_xLabel,0,Int4(iabs(iWinXLab(kWin))),0,100)
    call DlgSetI(Dlg,idc_win_yLabel,0,Int4(iabs(iWinYLab(kWin))),0,100)
    call DlgSetI(Dlg,idc_win_xGrid,0,Int4(iabs(iWinXGrid(kWin))),0,100)
    call DlgSetI(Dlg,idc_win_yGrid,0,Int4(iabs(iWinYGrid(kWin))),0,100)
    call DlgSetI(Dlg,idc_win_cgrid,0,Int4(iWinCGrid(kWin)),0,255)
	  call DlgSetR(Dlg,idc_win_xmin,WinXmin(kWin),7)
	  call DlgSetR(Dlg,idc_win_xmax,WinXmax(kWin),7)
	  call DlgSetR(Dlg,idc_win_ymin,WinYmin(kWin),7)
	  call DlgSetR(Dlg,idc_win_ymax,WinYmax(kWin),7)
    ldum=.false.
    if(iWinXLab(kWin).gt.0) ldum=.true.
    call DlgSetL(dlg,idc_win_xLabel_on,ldum)
    ldum=.false.
    if(iWinYLab(kWin).gt.0) ldum=.true.
    call DlgSetL(dlg,idc_win_yLabel_on,ldum)
    ldum=.false.
    if(iWinXGrid(kWin).gt.0) ldum=.true.
    call DlgSetL(dlg,idc_win_xGrid_on,ldum)
    ldum=.false.
    if(iWinYGrid(kWin).gt.0) ldum=.true.
    call DlgSetL(dlg,idc_win_yGrid_on,ldum)
    ldum=.false.
    if(iabs(iWinXLog(kWin)).gt.1) ldum=.true.
    call DlgSetL(dlg,idc_win_xlog,ldum)
    ldum=.false.
    if(iabs(iWinYLog(kWin)).gt.1) ldum=.true.
    call DlgSetL(dlg,idc_win_ylog,ldum)
    ldum=.false.
    if(iWinXLog(kWin).gt.0) ldum=.true.
    call DlgSetL(dlg,idc_win_xaxis_on,ldum)
    ldum=.false.
    if(iWinYLog(kWin).gt.0) ldum=.true.
    call DlgSetL(dlg,idc_win_yaxis_on,ldum)
    if(iWinPal(kWin).eq.1_2) then
      call DlgSetL(dlg,idc_win_pal_color,.true.)
    else if(iWinPal(kWin).eq.2_2) then
      call DlgSetL(dlg,idc_win_pal_gray,.true.)
    else
      call DlgSetL(dlg,idc_win_pal_variable,.true.)
    end if
    if(lWinFld(kWin)) then
      call DlgSetL(dlg,idc_win_fie,.true.)
    else
      call DlgSetL(dlg,idc_win_fun,.true.)
    end if
	  call DlgSetS0(dlg,idc_win_title,WinTitle(kWin))
  end Subroutine SetWindowData

  Subroutine GetWindowData(dlg)
! get graphic window data
    Implicit none
	  Include 'RESOURCE.FD'
    Integer(4) idum
	  Logical lcheck
	  Type(dialog) dlg
	  call DlgGetI(Dlg,idc_Win_kWin,0,0,kWinDisp,1,Int4(mWin),1)
	  call DlgGetI(Dlg,idc_win_Width,0,0,idum,10,10000,Int4(iWinWidthD))
    iWinWidth(kWinDisp)=Int2(idum)
	  call DlgGetI(Dlg,idc_win_Height,0,0,idum,10,10000,Int4(iWinHeightD))
    iWinHeight(kWinDisp)=Int2(idum)
	  call DlgGetI(Dlg,idc_win_Left,0,0,idum,0,100,Int4(iWinLeftD))
    iWinLeft(kWinDisp)=Int2(idum)
	  call DlgGetI(Dlg,idc_win_Right,0,0,idum,0,100,Int4(iWinRightD))
    iWinRight(kWinDisp)=Int2(idum)
	  call DlgGetI(Dlg,idc_win_Top,0,0,idum,0,100,Int4(iWinTopD))
    iWinTop(kWinDisp)=Int2(idum)
	  call DlgGetI(Dlg,idc_win_Bottom,0,0,idum,0,100,Int4(iWinBottomD))
    iWinBottom(kWinDisp)=Int2(idum)
	  call DlgGetI(Dlg,idc_win_XLabel,0,0,idum,0,100,Int4(iWinXLabD))
    iWinXLab(kWinDisp)=sign(Int2(idum),iWinXLab(kWinDisp))
	  call DlgGetI(Dlg,idc_win_YLabel,0,0,idum,0,100,Int4(iWinXLabD))
    iWinYLab(kWinDisp)=sign(Int2(idum),iWinYLab(kWinDisp))
	  call DlgGetI(Dlg,idc_win_XGrid,0,0,idum,0,100,Int4(iWinXGridD))
    iWinXGrid(kWinDisp)=sign(Int2(idum),iWinXGrid(kWinDisp))
	  call DlgGetI(Dlg,idc_win_YGrid,0,0,idum,0,100,Int4(iWinYGridD))
    iWinYGrid(kWinDisp)=sign(Int2(idum),iWinYGrid(kWinDisp))
	  call DlgGetI(Dlg,idc_win_CGrid,0,0,iWinCGrid(kWinDisp),0,255,Int4(iWinCGridD))
	  call DlgGetR(Dlg,idc_win_xmin,WinXmin(kWinDisp),nBig,pBig,WinXminD,7)
	  call DlgGetR(Dlg,idc_win_xmax,WinXmax(kWinDisp),nBig,pBig,WinXmaxD,7)
	  call DlgGetR(Dlg,idc_win_ymin,WinYmin(kWinDisp),nBig,pBig,WinYminD,7)
	  call DlgGetR(Dlg,idc_win_ymax,WinYmax(kWinDisp),nBig,pBig,WinYmaxD,7)
		call DlgGetL(dlg,idc_win_XLabel_On,lCheck)
    iWinXLab(kWinDisp)=iabs(iWinXLab(kWinDisp))
    if(.not.lCheck) iWinXLab(kWinDisp)=-iWinXLab(kWinDisp)
		call DlgGetL(dlg,idc_win_YLabel_On,lCheck)
    iWinYLab(kWinDisp)=iabs(iWinYLab(kWinDisp))
    if(.not.lCheck) iWinYLab(kWinDisp)=-iWinYLab(kWinDisp)
		call DlgGetL(dlg,idc_win_XGrid_On,lCheck)
    iWinXGrid(kWinDisp)=iabs(iWinXGrid(kWinDisp))
    if(.not.lCheck) iWinXGrid(kWinDisp)=-iWinXGrid(kWinDisp)
		call DlgGetL(dlg,idc_win_YGrid_On,lCheck)
    iWinYGrid(kWinDisp)=iabs(iWinYGrid(kWinDisp))
    if(.not.lCheck) iWinYGrid(kWinDisp)=-iWinYGrid(kWinDisp)
		call DlgGetL(dlg,idc_win_XLog,lCheck)
    iWinXLog(kWinDisp)=sign(1_2,iWinXLog(kWinDisp))
    if(lCheck) iWinXLog(kWinDisp)=2_2*iWinXLog(kWinDisp)
		call DlgGetL(dlg,idc_win_YLog,lCheck)
    iWinYLog(kWinDisp)=sign(1_2,iWinYLog(kWinDisp))
    if(lCheck) iWinYLog(kWinDisp)=2_2*iWinYLog(kWinDisp)
		call DlgGetL(dlg,idc_win_XAxis_On,lCheck)
    iWinXLog(kWinDisp)=iabs(iWinXLog(kWinDisp))
    if(.not.lCheck) iWinXLog(kWinDisp)=-iWinXLog(kWinDisp)
		call DlgGetL(dlg,idc_win_YAxis_On,lCheck)
    iWinYLog(kWinDisp)=iabs(iWinYLog(kWinDisp))
    if(.not.lCheck) iWinYLog(kWinDisp)=-iWinYLog(kWinDisp)
		call DlgGetL(dlg,idc_Win_Pal_Variable,lCheck)
    if(lCheck) iWinPal(kWinDisp)=0_2
		call DlgGetL(dlg,idc_Win_Pal_Color,lCheck)
    if(lCheck) then
      iWinPal(kWinDisp)=1_2
      call ColorPalette(.true.)
    end if
		call DlgGetL(dlg,idc_Win_Pal_Gray,lCheck)
    if(lCheck) then
      iWinPal(kWinDisp)=2_2
      call GrayPalette(.true.)
    end if
		call DlgGetL(dlg,idc_Win_FIE,lCheck)
    lWinFld(kWin)=lCheck
		call DlgGetS(dlg,idc_win_Title,WinTitle(kWinDisp))
    lWinInit=.true.
  end Subroutine GetWindowData

	Subroutine updateCHWindow(dlg,control_name,callbackType)
! callback for WindowDialog
    Implicit none
	  Include 'RESOURCE.FD'
	  Integer(4) control_name,callbackType,idum,lf
	  Type (dialog) dlg
	  idum=callbackType
	  Select Case(control_name)
	  Case(idc_win_defaults)
      call Window_Defaults(.true.)
      call SetWindowData(dlg)
	  Case(idc_win_set_title)
      lf=min(GetSLength(ProFileName),200)
      WinTitle(kWinDisp)=ProFileName(1:lf)//char(0)
      call SetWindowData(dlg)
	  Case(idc_win_read_pal)
      call OpenPalette(.false.)
      iWinPal(kWinDisp)=0_2
      call SetWindowData(dlg)
	  Case(idc_win_read)
      call GetWindowData(dlg)
      call OpenWindow(.false.)
      call SetWindowData(dlg)
	  Case(idc_win_write)
      call GetWindowData(dlg)
      call SaveWindow(.false.)
      call SetWindowData(dlg)
	  Case(idcancel)
      call GetWindowData(dlg)
     	call DlgExit(dlg)
	  end Select
	end Subroutine updateCHWindow

END MODULE CHDIC
