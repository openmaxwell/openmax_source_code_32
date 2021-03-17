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
MODULE CHDAT

! Data of boundaries, domains, expansions

  USE CHSPL

  SAVE

  Logical lInverseConn

! external optimizer

  Integer(4) jOptP,iOptProc,nOptPar,iOptFile,nOptFit,iOptFit
  Real(8) OptFit(100)
  Logical lOptBin

! Demo version

  Integer(4) maxXGrd,maxYGrd,maxZGrd,maxCol,maxPts,maxStr

! Formula interpreter

  Integer(4), Parameter :: mUserForm=10
  Integer(4) kUserForm,nUserForm,lUserForm(0:mUserForm),lUserFor
  Real(8) cForm(0:8),pForm(0:11),vForm(-10:999),dFormu(mForA),xFormu(mForA)
  Complex(8) cCForm(0:8),pCForm(0:11),vCForm(-10:999),cFormu(mForA),zFormu(mForA)
  Logical lForWarn
  Character(1151) sUserForm(0:mUserForm),sUserFor
  Data LForWarn/.true./

! Graphics

  Integer(4), Parameter :: mGraPoly=100
  Integer(2) ixPoly(mGraPoly),iyPoly(mGraPoly)
  Real(8) xPoly(mGraPoly),yPoly(mGraPoly)
  Real(8) GRFtime,xMinBas,xMaxBas,xMinExt,xMaxExt
  Integer(2) iColorCnt
  Logical lLimits
  Logical lGRF_Mark,lGRF_PolygonClosed
  Character(128) OTitle,TTitle,XTitle,YTitle
  Type(xycoord) Poly(mGraPoly)

  Real(8) GRChmax,GRChmin,GRCcolorFMin,GRCcolorFMax
  Integer(4) nGRC,nxGRC,nyGRC
  Integer(2) iGRCLineEndStyle,iGRCxmin,iGRCxmax,iGRCymin,iGRCymax, &
  & iGRCbackground,iGRCcolor,iGRCcolorB,iGRCcolorMin,iGRCcolorMax
  Logical lGRCused,lGRCallocated,lGRCfill
  Integer(2), Allocatable :: iGRCx(:,:),iGRCy(:,:),iGRCc(:,:)
  Real(8), Allocatable :: GRCh(:,:)

! OpenGL

  Real(8) CHGLxGridLineW,CHGLyGridLineW,CHGLzGridLineW,CHGLxAxisW,CHGLyAxisW,CHGLzAxisW, &
   & CHGLxAxisL,CHGLyAxisL,CHGLzAxisL,CHGLVectorScale,CHGLVectorMaxLength,CHGLIsoMinF,CHGLIsoMaxF,CHGLIsoStepF, &
   & CHGLExpLen,CHGLSurfaceMinF,CHGLSurfaceMaxF,CHGLSurfaceStepF,rCHGLMovie
  Real(8) CHGLlookAt(3),CHGLlookFrom(3),CHGLViewAngle,CHGLViewAspect,CHGLViewNear,CHGLViewFar
  Integer(2) iCHGLxGridColor,iCHGLyGridColor,iCHGLzGridColor,iCHGLxVectorColor,iCHGLyVectorColor, &
   & iCHGLxAxisColor,iCHGLyAxisColor,iCHGLzAxisColor,iCHGLxVectorStep,iCHGLyVectorStep,iCHGLrColorMin,iCHGLrColorMax, &
   & iCHGLwidth,iCHGLheight,iCHGLwidthCurrent,iCHGLheightCurrent
  Integer(4) nCHGLlists,iCHGLMovie,nCHGLTube,kCHGLTube,mCHGLTube
  Real(8), Allocatable:: CHGLTubeStart(:,:),CHGLTubeR(:),CHGLTubeD(:)
  Integer(4), Allocatable:: iCHGLTubeR(:),iCHGLTubeD(:)
  Integer(2), Allocatable:: iCHGLTubeColor(:,:)
  Logical lCHGLlist(9),lCHGLdoubleSide,lDrawOGL,lOGLavi,lSaveOGLavi,lSaveOGLbmp
  Character(20), Parameter :: CHOGLIdent=' CHOGL Version 1.1  '
  Character(256) OGLFileName

! boundary data

  type CBnd
    Real(8) Val(2),Start,Length,sLength,fAmpl,Weight,Weight2
    Integer(4) iEdgeOffset,nEdge,kEdge,iSplineOffset,nSpline,iMatOffset,nMat,nMatPts
    Integer(2) iCol,iCond,iConn
    Integer(2) iLDom,iRDom,iTypB,nBC
    Logical lBC(10)
    Character(1151) Formula
  end type
  type CBndEdg
    Integer(2) iOrient
    Real(8) x,y,r,xa,ya,xb,yb,xo,yo,d
  end type
  Character(20), Parameter :: CHBndIdent=' CHBND Version 3.0  '
  Character(32) TransText(3)
  Character(256) BndFileName,ConFileName,MatFileName,GeoFileName,MshFileName
  Real(8) sBndPt(0:10000)
  Real(8), Allocatable :: wBndPt(:),eBndPt(:),fBndPt(:), &
  & eBndPt3D(:),fBndPt3D(:),wBndPt3D(:),BndPt3D(:,:,:),BndPt3DF(:),BndPt3DV(:,:),rpGMSH(:,:)
  Real(8), Allocatable :: sSpline(:),xSpline(:),ySpline(:),xsSpline(:),ysSpline(:)
  Real(8), Allocatable :: sSpl(:),xSpl(:),ySpl(:),xsSpl(:),ysSpl(:)
  Integer(2), Allocatable :: iBndPt(:),jBndPt(:),iBndPt3D(:),iObjBndPt3D(:),iEquBndPt3D(:)
  Real(8) BndLtot,BndDMax,BndPpW,BndOver,trans(3),errorM,errorMA,errorA,AerrorM,AerrorA,ErrorScale,Condition,vBndNorm(3), &
  &       vErrStat(6),smallMatDist
  Integer(4) kBnd,nBnd,mBnd,kBndEdg,nBndEdg,mBndEdg,kBndSpl,nBndSpl,mBndSpl,kVal,iGetData,nBndEq,nBndEq3D, &
  &          kBndPt,nBndPt,mBndPt,mBndPtAlloc,nBndPt3D,mBndPt3D,mBndPt3DAlloc,nSegPt,iDraBnd,iFunWeight,nInhBndPt3D,nInhEqu3D, &
  &          iGMSHbnd1,iGMSHbnd2,iGMSHDom,iGMSHCol
  Integer(2) iMMPCon,iDomBnd,iColBnd,iConBnd,iBound,iWhatiBndPt3D
  Integer(1), allocatable:: ieGMSH(:),ipGMSH(:)
  Logical lInsertBnd,lAskBnd,lCloseNow,lMoveInsert,lTrans,lGet3DMat,lSmoothMat
  Type(CBnd), Allocatable :: tBnd(:),tBndAux(:)
  Type(CBndEdg), Allocatable :: tBndEdg(:),tBndEdgAux(:)

! domain data

  Character(20), Parameter :: CHDomIdent=' CHDOM Version 2.0  '
  Character(20), Parameter :: CHProIdent=' CHPRO Version 3.1  '
  Complex(8), Allocatable :: eDom(:),uDom(:),sDom(:),tDom(:)
  Real(8), Allocatable :: dDom(:),gDom(:),aDom(:,:),bDom(:,:)
  Integer(2), Allocatable :: iDom(:),idDom(:,:)
  Character(1151), Allocatable :: Dom_Form(:,:),hDom_Form(:,:)
  Complex(8) fcFld,gcFld,dcFld,kcFld,kapcFld,cZw,c1Eigen,c2Eigen,c1EigenC,c2EigenC,cxPeriod,cyPeriod,czPeriod, &
  &          cxPeriodDlg,cyPeriodDlg,czPeriodDlg
  Real(8) xPeriod,aEigen,fEigen,Zw0,Kw0,yPeriodVector(3),zPeriodVector(3)
  Integer(4) kDom,nDom,mDom,iEigen,nrEigen,niEigen,mmEigen,imEigen,itmEigen,itEigen,nEigenSave, &
  &          ixySymm,ixzSymm,iyzSymm,nxPeriod,nyPeriod,nzPeriod
  Integer(2) iHEGlobal,lMaxFileName,lMaxFileDir
  Logical lfcFld,lgcFld,lzPer,lxPeriod,lyPeriod,lzPeriod,lregularPeriod,lEigen,lEigenSave,lWriteRoughFld,lExpFFD,lExcFFD
  Character(256) DomFileName,ProFileName,MaxFileName,InpFileName,MaxFileDir
  Real(8) Eps01,Mue01

! expansion data

  type Expansion
    Complex(8) gc
    Real(8) Plane(3,0:3),rE(5),O(3),e(3),xO,yO,dmin,depend
    Integer(2) nPar,iOff,iConn,iCol,iObj
    Integer(2) iDom,iTypE,iHE,iE(6),iS
  ! iTypE            rE1...5
  ! 0     Connection angle(2D), min x,min y, min z, -
  ! 1     2D Hankel  angle(2D), min r, im x, im y, cut angle
  ! 2     2D Bessel  angle(2D), max r, im x, im y, cut angle
  ! 3     Plane wave angle(2D), ----
  ! 4     Rayleigh   -----
  ! 5     Harmonic   angle(2D), x-per or re kx, min x, min y, im kx
  ! 6     3D Hankel  min r, im x, im y, im z, cut angle
  ! 7     3D Bessel  max r, im x, im y, im z, cut angle
  ! 8     Ring       r, min angle, sector angle, degree factor, min r
  ! 9     Line       min z, length, degree factor, min r, -
  ! 10    Spiral     dr, dphi, dz, degree factor, min r
  ! 11    Gauss      r, ----
  end type
  Character(20), Parameter :: CHExpIdent=' CHEXP Version 3.3  '
  Complex(8), Allocatable :: ParExp(:,:),ParExpAux(:,:)
  Integer(2), Allocatable :: iParExp(:,:),iParExpAux(:,:)
  Type(Expansion), Allocatable :: tExp(:),tExpAux(:)
  Complex(8) FldExp(10),FldAExp(10)
  Real(8) FldPln(3,0:3)
  Real(8) dep_delExp,fmin_genExp,fmax_genExp,fact_genExp,finn_genExp,fout_genExp,dinn_genExp,dout_genExp,ainn_genExp, &
  & aout_genExp,df_genExp(2),dl_genExp(2),fModExpFac,ExpSumAcc
  Integer(4) kB_genExp,nE_genExp,iE_genExp,nS_genExp,iCl_genExp,iCn_genExp,iDm_genExp,iWf_genExp,iWe_genExp, &
  & iModExpMin,iModExpMax,iModExpBnd,iModExpDom,iModExpObj,iModExpCol,iModExpCon,iModExpLoop,iModExpWFA,iModExpWFE
  Integer(4) kExp,mExp,nExp,kExc,nExc,nRHS,kPar,mPar,nPar,nParN,iGetExpData,kiExp,krExp,iDraExp,nExcit
  Integer(2) iDomExp,iColExp,iConExp
  Logical lInsertExp,lAskExp,lMMPstat,lMMPBndVal,lDispWarn,lExpTest,lInitSurfWaves
  Character(32) siExp(6,0:20),srExp(5,0:20),siObj(5,0:10),srObj(5,0:10)
  Character(256) ExpFileName,ParFileName

! object data

  type Object
    Real(8) Plane(3,0:3),Par(5),GrfRes,O(3),e(3)
    Integer(2) iTypO,iCol,iColMin,iColMax,iPar(5)
    Integer(4) nGrf,mGrf,iGrf,iMatOffset,nMat,nInhibited
  end type
  Character(20), Parameter :: CHObjIdent=' CHOBJ Version 1.0  '
  Type(Object), Allocatable :: tObj(:),tObjAux(:)
  Real(8), Allocatable :: ObjWeb(:,:),ObjWebX(:,:),ObjWebY(:,:),ObjWebZ(:,:),ObjWebV(:,:),ObjWebF(:)
  Integer(2), Allocatable :: iObjWeb(:),nxObjWeb(:),nyObjWeb(:),iCObjWeb(:)
  Real(8) fGenExpObj
  Integer(2) nObjWeb
  Integer(4) kObj,mObj,nObj,krObj,kiObj,iDraObj,iObjDra,iGenExpObj,iGenExpIns,iGenExpMinE,iGenExpMaxE,iGenExpMaxM, &
  & iGenExpCl,iGenExpCl2,iGenExpCn,iGenExpOb,kInhibit,nInhibit,mInhibit
  Logical lInsertObj,lAskObj,lObjMat,lObjMatW0,lObjFlg,lObjHid,lGenExpDel
  Character(256) ObjFileName,M3DFileName,D3DFileName
  Character(32), Allocatable:: sInhibit(:),sInhibitAlt(:)

! modify object data

  Integer(2) icolMobj,iconMobj
  Real(8) xminMobj,xmaxMobj,yminMobj,ymaxMobj

! field data

  Character(20), Parameter :: CHFldIdent=' CHFLD Version 1.0  '
  Character(20), Parameter :: CHFlfIdent=' CHFLF Version 2.0  '
  Character(20), Parameter :: CHFltIdent=' CHFLT Version 2.0  '
  Character(20), Parameter :: CHPFDIdent=' CHPFD Version 1.0  '
  Character(20), Parameter :: CHGrfIdent=' CHGRF Version 1.0  '
  Character(20), Parameter :: CHGrtIdent=' CHGRT Version 1.0  '
  Complex(8), Parameter :: Ci=(0.0d0,1.0d0)
  Integer(2), Parameter :: itE=1,itH=2,itS=3,itD=4,itB=5,itA=6,itV=7,itWe=8,itWh=9,itWt=10,itPe=11,itPh=12,itPt=13,&
  & itJ=14,itF=15
  Complex(8), Allocatable :: cFld(:,:,:,:),cAuxFld(:,:,:,:)
  Real(8), Allocatable :: dFld(:,:,:,:),dAuxFld(:,:,:,:),rFld(:,:,:),drFld(:),rGrd(:,:,:,:)
  Integer(2), Allocatable :: iFld(:,:,:)
  Integer(4), Allocatable :: irFld(:),jrFld(:),krFld(:)
  Complex(8) cSingleFldPoint(10)
  Real(8) c678(1:3,1:3),spacecFld(3,0:3), &
  &       trFld,prFld,dtrFld, &
  &       rMinFld,rMaxFld,rSumFld,rSum2Fld,ErrFld,Err2Fld, &
  &       scaleIntensity,scaleArrow,rIsoStep,Grid3D,ArrowLength,PowerFld,FactorFld, &
  &       rSingleFldPoint(3),dSingleFldPoint(10),fSingleFldPoint(3)
  Integer(4) nxcFld,nycFld,nzcFld,ixcFld,iycFld,izcFld,jxcFld,jycFld,jzcFld,ncFld, &
  &          nxrFld,nyrFld,ixrFld,iyrFld,nsFld,iFFldE,nIterPFD,iQFFD, &
  &          levPlane,kExpFFD,kColFFD,kConFFD,kDomFFD,kParFFD,idFld(10)
  Integer(2) minCIntensity,maxCIntensity,itIntensity, &
  &          minCArrow,maxCArrow,itArrow
  Integer(2) iEx,iEy,iEz,iHx,iHy,iHz,iAx,iAy,iAz,iV, &
  &          itrFld,iPlane,iSingleFldPoint,iQ_Form
  Logical lFLDset0,lxcFld,lycFld,lzcFld,lEcFld,lHcFld,lAcFld,lVcFld,lfcFldAll, &
  &       lxrFld,lyrFld,lzrFld,larFld,lprFld,lGrid3D,lArrowFill,lIsoFill,lIsoLine, &
  &       lrGrd,lGetAll,LIQ,literE,&
  &       lDiffField,lDiffRel,lSingleFldPoint,lSkipFld,lSkipDerFld,lSkipVecFld,lSkipScaFld,lSkipFldHead,lAskFld
  Character(256) FldFileName,FlfFileName,FltFileName,PFDFileName,GrfFileName,GrtFileName
  Character(1151) Fld_Form(3,4),Fld_Def_Form(0:3,3,4), &
  &        Grd_Form(3),Grd_Def_Form(0:3,3), &
  &        Con_Form,Con_Def_Form(0:3),Trns_Form(3),Trns_Def_Form(0:3,3), &
  &        Q_Form(20,2),Q_Def_Form(0:3,20)

! PFD data

  Logical lPFDalloc,lPFDc
  Integer(2) iPFDt,iPFDft,nPFDi,nPFDj,nPFDk,nPFDil,nPFDih,nPFDjl,nPFDjh,nPFDkl,nPFDkh,nPFDf, &
  & nPFDiEff,nPFDsLayers
  Integer(4) nPFDcFld,nPFDsens,kPFDsens,nPFDsource,kPFDsource
  Real(8) PFDxmin,PFDxmax,PFDymin,PFDymax,PFDzmin,PFDzmax,PFDdx,PFDdy,PFDdz,PFDfTau,PFDfTmax,PFDpml,PFDfmin,PFDfmax, &
  &       PFDwmax,PFDwfact,PFDdfact
  Integer(2), Allocatable:: iPFDs(:),jPFDs(:),kPFDs(:),iPFDsa(:),jPFDsa(:),kPFDsa(:)
  Integer(4), Allocatable:: nPFDsFld(:),nPFDsFlda(:)
  Complex(8), Allocatable:: PFDsourceA(:),PFDsourceAa(:)
  Real(8), Allocatable:: PFDsensX(:),PFDsensY(:),PFDsensZ(:),PFDsensT(:),PFDsensD(:),dPFDsens(:,:)
  Real(8), Allocatable:: PFDsensXa(:),PFDsensYa(:),PFDsensZa(:),PFDsensTa(:),PFDsensDa(:),dPFDsensa(:,:)
  Complex(8), Allocatable :: cPFDsens(:,:,:),cPFDsensa(:,:,:)

! integration data

  Character(20), Parameter :: CHIntIdent=' CHINT Version 2.0  '
  Real(8) currentIntegral,currentIntegralMax,currentIntegralMin,currentIntegralMaxL(3),currentIntegralMinL(3), &
  &       currentIntegralMax1,currentIntegralMin1,currentIntegralMax1S,currentIntegralMin1S, &
  &       AccIntegral,XminInt,XmaxInt,YminInt,YmaxInt,rSphInt,PowerInt,FactorInt
  Real(8) spaceInt(3,0:3)
  Integer(4) IntType,IntField,IntInter,MaxIntIter,IntWhat,IntNx,IntNy,IntOrd,IntErr,IntCall,iBndInt,iObjInt,IntSide
  Integer(2) iIntgEc,iIntgHc
  Logical lIntgSave,lAbsInt,lebInt,lBndIntgX
  Character(256) IntFileName

! PET data

  Character(20), Parameter :: CHPETIdent=' CHBAS Version 1.0  '
  Complex(8), Allocatable :: pPET(:,:)
  Integer(4), Parameter :: mPET=10
  Complex(8) ePET(2,mPET),dPET
  Real(8) fPET,xPET(mPET),wPET(mPET)
  Integer(4) kPETD,kPET,nPET,npPET
  Logical lPET
  Character(256) PETFileName
  Character(1151) sPET(mPET)

! MMP data

  Logical lMMPDeterminant
  Integer(4) iWorkSpace,iMMPlast
  Real(8) rMMPResV(9)
  Complex(8) cMMPeigenPhase,cMMPeigenVector(3)

! particle data

	type Particle2D
    Real(8) r,sMass,Mass,Position(3),Velocity(3),Acceleration(3),Force(3),Friction(2),RandomForce
    Integer(2) iBnd,iColPart,iColMirror,iColSurface
	end type
	type Particle3D
    Real(8) r,sMass,Mass,Position(3),Velocity(3),Acceleration(3),Force(3),Friction(2),RandomForce
    Integer(2) iObj,iColPart,iColMirror,iColSurface
	end type
  Integer(4) nParticle2D,nParticle3D
  Real(8) ParticleTimeStep,ParticleStepLength,Particle2DMirror(4,3),Particle3DMirror(4,3)
  Character(256) PrtFileName
  Type(Particle2D), Allocatable :: tParticle2D(:),tParticle2DAux(:)
  Type(Particle3D), Allocatable :: tParticle3D(:),tParticle3DAux(:)

! Insert dialog

  Integer(4) kInsObj,nInsObj,iAVIdlg
  Logical(4) lInsObj


  CONTAINS

! Defaults

  Subroutine Boundary_Defaults(lCheck)
! set default values for the boundaries
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
	  ldum=lCheck
    call setNameExt(ProFileName,'BND',BndFileName)
    call setNameExt(ProFileName,'MAT',MatFileName)
    call setNameExt(ProFileName,'GEO',GeoFileName)
    call setNameExt(ProFileName,'MSH',MshFileName)
    ConFileName='Contour.DAT'C
    mBndPtAlloc=-1 ! no memory allocated for the matching points
    mBndPt3DAlloc=-1 ! no memory allocated for the 3D matching points
    lGRF_Mark=.true.
    lGRF_PolygonClosed=.true.
    lInsertBnd=.false.
    lAskBnd=.true.
    lCloseNow=.false.
    lMoveInsert=.false.
    iBound=0_2
    iDomBnd=0_2
    iColBnd=0_2
    iConBnd=0_2
    iDraBnd=0
    kVal=1
    trans=0.0d0
    lTrans=.true.
    iWhatiBndPt3D=-1_2
    lGet3DMat=.true.
    nInhBndPt3D=0
    nInhEqu3D=0
    vBndNorm(1)=1.0d0
    vBndNorm(2:3)=0.0d0
! test: 2 circles
    mBnd=2
    nBnd=2
    kBnd=1
    mBndEdg=2
    nBndEdg=2
    kBndEdg=1
    mBndSpl=0
    nBndSpl=0
    kBndSpl=0
    call AllocateBnd(ldum)
    if(.not.ldum) then
      kBndEdg=0
      nBndEdg=0
      mBndEdg=0
      mBndSpl=0
      nBndSpl=0
      kBndSpl=0
      return
    end if
    call AllocateBndEdg(ldum)
    if(.not.ldum) return
    call AllocateBndSpl(ldum)
    if(.not.ldum) then
      iBound=-9_2
      return
    end if
    tBnd(1)%iTypB=1_2
    tBnd(1)%nEdge=1
    tBnd(1)%kEdge=1
    tBnd(1)%iEdgeOffset=0
    tBnd(1)%iSplineOffset=0
    tBnd(1)%iMatOffset=0
    tBnd(1)%iLDom=1_2
    tBnd(1)%iRDom=0_2
    tBnd(1)%iCond=0_2
    tBnd(1)%iConn=0_2
    tBnd(1)%iCol=2_2
    tBnd(1)%Weight=1.0d0
    tBnd(1)%Weight2=1.0d0
    tBnd(1)%val(1)=0.0d0
    tBnd(1)%val(2)=0.0d0
    tBnd(1)%nSpline=0
    tBnd(1)%nMatPts=0
    tBnd(1)%nMat=0
    tBnd(1)%fAmpl=0.0d0
    tBnd(1)%Formula='0'C
    tBnd(2)%iTypB=1_2
    tBnd(2)%nEdge=1
    tBnd(2)%kEdge=1
    tBnd(2)%iEdgeOffset=1
    tBnd(2)%iSplineOffset=0
    tBnd(2)%iMatOffset=0
    tBnd(2)%iLDom=0_2
    tBnd(2)%iRDom=1_2
    tBnd(2)%iCond=0_2
    tBnd(2)%iConn=0_2
    tBnd(2)%iCol=3_2
    tBnd(2)%Weight=1.0d0
    tBnd(2)%Weight2=1.0d0
    tBnd(2)%val(1)=1.0d0
    tBnd(2)%val(2)=0.0d0
    tBnd(2)%nSpline=0
    tBnd(2)%nMatPts=0
    tBnd(2)%nMat=0
    tBnd(2)%fAmpl=0.0d0
    tBnd(2)%Formula='0'C
    tBndEdg(1)%x=0.0d0
    tBndEdg(1)%y=0.0d0
    tBndEdg(1)%r=0.95d0
    tBndEdg(1)%d=0.0d0
    tBndEdg(2)%x=0.0d0
    tBndEdg(2)%y=0.0d0
    tBndEdg(2)%r=0.25d0
    tBndEdg(2)%d=0.0d0
    iGMSHbnd1=1
    iGMSHbnd2=2
    iGMSHDom=0
    iGMSHCol=0
    lInitSurfWaves=.true.
	end Subroutine Boundary_Defaults

  Subroutine Domain_Defaults(lCheck)
! set default domain + project data
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    call setNameExt(ProFileName,'DOM',DomFileName)
    iSpaceRead=3
    iPoints2DRead=0
    iPoints3DRead=0
! global data
    Eps01=1.0d0/Eps0
    Mue01=1.0d0/Mue0
    Kw0=dsqrt(Mue0*Eps0)  ! without omega!
    Zw0=dsqrt(Mue0/Eps0)
    fcFld=(0.0d0,0.0d0)   ! frequency
    gcFld=(0.0d0,0.0d0)   ! normed propagation constant
    dcFld=(1.0d0,0.0d0)   ! half period in z direction
    c1Eigen=(0.0d0,0.0d0) ! first Eigenvalue search point in the complex plane
    c2Eigen=(0.0d0,0.0d0) ! second Eigenvalue search point in the complex plane
    c1EigenC=c1Eigen      ! clip window for fine search same as for rough search
    c2EigenC=c2Eigen
    xPeriod=1.0d0         ! cell length for periodic problems
    yPeriodVector(1)=0.0d0
    yPeriodVector(2)=1.0d0
    yPeriodVector(3)=0.0d0
    zPeriodVector(1)=0.0d0
    zPeriodVector(2)=0.0d0
    zPeriodVector(3)=1.0d0 
    cxPeriod=(0.0d0,0.0d0)! complex constant for periodic problems
    cyPeriod=(0.0d0,0.0d0)! complex constant for periodic problems
    czPeriod=(0.0d0,0.0d0)! complex constant for periodic problems
    CxPeriodDlg=CxPeriod
    CyPeriodDlg=CyPeriod
    CzPeriodDlg=CzPeriod
    aEigen=0.01           ! Eigenvalue stopping criterium 1 (accuracy)
    fEigen=0.01           ! Eigenvalue stopping criterium 1 (flatness)
    nrEigen=10            ! Eigenvalue points in r direction for the rough search
    niEigen=10            ! Eigenvalue points in i direction for the rough search
    imEigen=1             ! fine search of 1st minimum only
    mmEigen=0             ! no minima found in rough search
    itmEigen=100          ! max. Eigenvalue iterations for the fine search
    itEigen=0             ! Eigenvalue iterations performed
    nEigenSave=0          ! never write eigenvalue search data when lEigenSave is false
    ixySymm=0             ! no symmetry with respect to xy plane
    ixzSymm=0             ! no symmetry with respect to xz plane
    iyzSymm=0             ! no symmetry with respect to yz plane
    lfcFld=.false.        ! field is not frequency dependent (time dependent)
    lgcFld=.true.         ! field is 2D (not 3D)
    lzPer=.false.         ! z-per/2 is not characteristic for z dependence (gamma is used)
    lxPeriod=.false.      ! geometry is not periodic
    lyPeriod=.false.      ! geometry is not periodic
    lzPeriod=.false.      ! geometry is not periodic
    lregularPeriod=.true. ! regular periodic cell
    lEigen=.false.        ! not an Eigenvalue problem
    lEigenSave=.true.     ! save Eigenvalue search data on file
    lWriteRoughFld=.true. ! write rough search data on FLD file if possible
    iEigen=1              ! frequency is Eigenvalue
    iHEGlobal=0_2         ! Ez modes
! domain data
    mDom=1
    nDom=1
    kDom=1
    call AllocateDom(ldum)
    if(.not.ldum) return
    iDom(0)=0_2
  end Subroutine Domain_Defaults

  Subroutine Expansion_Defaults(lCheck)
! set default expansion data
    Implicit none
    Integer(4) k
    Logical, intent(in) :: lCheck
    Logical ldum
	  ldum=lCheck
    lExpTest=.false.
    call setNameExt(ProFileName,'EXP',ExpFileName)
    call setNameExt(ProFileName,'PAR',ParFileName)
    siExp='Unused'C
    srExp='Unused'C
    siExp(1,0)='ID number'C        ! Connection
    siExp(2,0)='XY symmetry'C
    siExp(3,0)='XZ symmetry'C
    siExp(4,0)='YZ symmetry'C
    siExp(5,0)='Max. order'C
    siExp(6,0)='Basis type'C
    srExp(1,0)='Angle (XY-plane)'C
    srExp(2,0)='Min x / Max |x|'C
    srExp(3,0)='Min y / Max |y|'C
    srExp(4,0)='Min z / Max |z|'C
    srExp(5,0)='Fourier factor'C
    siExp(1,1)='Minimum order'C    ! 2D Hankel
    siExp(2,1)='Maximum order n'C
    siExp(3,1)='3D->Min. deg. m'C
    siExp(4,1)='3D->Max. deg. m'C
    siExp(5,1)='Step of order'C
    siExp(6,1)='Period for 3D'C
    srExp(1,1)='Angle (XY-plane)'C
    srExp(2,1)='Minimum radius'C
    srExp(3,1)='Imaginary x'C
    srExp(4,1)='Imaginary y'C
    srExp(5,1)='Cut angle'C
    siExp(1,2)='Minimum order'C    ! 2D Bessel
    siExp(2,2)='Maximum order n'C
    siExp(3,2)='3D->Min. deg. m'C
    siExp(4,2)='3D->Max. deg. m'C
    siExp(5,2)='Step of order'C
    siExp(6,2)='3D: Period type'C
    srExp(1,2)='Angle (XY-plane)'C
    srExp(2,2)='Maximum radius'C
    srExp(3,2)='Imaginary x'C
    srExp(4,2)='Imaginary y'C
    srExp(1,3)='Angle (XY-plane)'C ! Plane wave
    srExp(2,3)='Main angle (3D)'C
    srExp(3,3)='Sect. angle (3D)'C
    siExp(2,3)='Superpos. n (3D)'C
    siExp(3,3)='Superpos.ord.(3D)'C
    siExp(4,3)='Scaling (<1:no)'C
    siExp(1,4)='Trans(0)/Refl(1)'C ! Rayleigh
    siExp(2,4)='Maximum x order'C
    siExp(3,4)='Minimum x order'C
    siExp(4,4)='Scaling (<0:no)'C
    srExp(1,4)='Angle must be 0'C
    srExp(2,4)='Min y(2D)/z(3D)'C
    srExp(3,4)='Real(CxPeriod)'C
    srExp(4,4)='Imag(CxPeriod)'C
    siExp(1,5)='x type'C           ! Harmonic
    siExp(2,5)='y type'C
    siExp(3,5)='Minimum order'C
    siExp(4,5)='Maximum order'C
    siExp(5,5)='Step of order'C
    siExp(6,5)='z type'C
    srExp(1,5)='Angle (XY-plane)'C
    srExp(2,5)='x-per./2 | kx/k0'C
    srExp(3,5)='Min x / Max |x|'C
    srExp(4,5)='Min y / Max |y|'C
    srExp(5,5)='Imag(kx/k0)'C
    siExp(1,6)='Minimum order n'C  ! 3D Hankel
    siExp(2,6)='Maximum order n'C
    siExp(3,6)='Minimum degree m'C
    siExp(4,6)='Maximum degree m'C
    srExp(1,6)='Minimum radius'C
    srExp(2,6)='Imaginary x'C
    srExp(3,6)='Imaginary y'C
    srExp(4,6)='Imaginary z'C
    srExp(5,6)='Cut angle'C
    siExp(1,7)='Minimum order n'C  ! 3D Bessel
    siExp(2,7)='Maximum order n'C
    siExp(3,7)='Minimum degree m'C
    siExp(4,7)='Maximum degree m'C
    srExp(1,7)='Maximum radius'C
    srExp(2,7)='Imaginary x'C
    srExp(3,7)='Imaginary y'C
    srExp(4,7)='Imaginary z'C
    siExp(1,8:10)='Minimum order n'C  ! Line multipoles
    siExp(2,8:10)='Maximum order n'C
    siExp(3,8:10)='Minimum degree m'C
    siExp(4,8:10)='Maximum degree m'C
    siExp(5,8)='Step of degree'C
    siExp(5,9:10)='Step of order'C
    siExp(6,8:10)='N multipoles'C
    srExp(1,8)='Ring radius'C  ! Ring Hankel
    srExp(2,8)='Minimum angle'C
    srExp(3,8)='Sector angle'C
    srExp(4,8)='Degree factor'C
    srExp(5,8)='Minimum radius'C
    srExp(1,9)='Minimum z'C  ! Line Hankel
    srExp(2,9)='Length'C
    srExp(3,9)='Degree factor'C
    srExp(4,9)='Minimum radius'C
    srExp(1,10)='Delta r'C  ! Spiral Hankel
    srExp(2,10)='Delta phi'C
    srExp(3,10)='Delta z'C
    srExp(4,10)='Degree factor'C
    srExp(5,10)='Minimum radius'C
    siExp(1,11)='Order n or -1'C  ! 3D Gaussian beam
    srExp(1,11)='Radius or v/c'C
    srExp(2,11)='EBeam min.dist.'C
    siExp(1,12)='N layers'C  ! 2D monopole for multilayer
    siExp(2,12)='Bottom layer type'C 
    siExp(3,12)='Top layer type'C 
    srExp(1,12)='Sommerfeld k'C
    srExp(2,12)='Integration acc.'C
    srExp(3,12)='Imaginary x'C
    srExp(4,12)='Imaginary y'C
    siExp(1,13)='N layers'C  ! 3D dipole for multilayer
    siExp(2,13)='Bottom layer type'C 
    siExp(3,13)='Top layer type'C 
    siExp(4,13)='Hor/vert/both:0/1/2'C 
    srExp(1,13)='Sommerfeld k'C
    srExp(2,13)='Integration acc.'C
    srExp(3,13)='Imaginary x'C
    srExp(4,13)='Imaginary y'C
    srExp(5,13)='Imaginary z'C
    kiExp=1
    krExp=1
    lInsertExp=.true.
    lAskExp=.true.
    lDispWarn=.true.
    iDomExp=0_2
    iColExp=0_2
    iConExp=0_2
    iDraExp=0
    kExc=1
    nExc=1
    nRHS=1
    kExp=1
    nExp=3
    mExp=100
    call AllocateExp(ldum)
    if(.not.ldum) return
    do kExp=1,nExp
      tExp(kExp)%iTypE=1
      tExp(kExp)%iDom=1
      tExp(kExp)%iConn=0
      tExp(kExp)%iCol=1
      tExp(kExp)%iObj=0
      tExp(kExp)%nPar=1
      tExp(kExp)%iHE=2
      tExp(kExp)%iS=0
      tExp(kExp)%iE(1:6)=0
      tExp(kExp)%iE(2)=0
      tExp(kExp)%iE(5)=1
      tExp(kExp)%rE(1:5)=0.0d0
      tExp(kExp)%depend=1.0d100
      tExp(kExp)%gc=gcFld
      tExp(kExp)%Plane(1:3,0:3)=0.0d0
      tExp(kExp)%Plane(1,1)=1.0d0
      tExp(kExp)%Plane(2,2)=1.0d0
      tExp(kExp)%Plane(3,3)=1.0d0
      if(tExp(kExp)%iTypE.ne.10) then
        tExp(kExp)%xo=tExp(kExp)%Plane(1,0)
        tExp(kExp)%yo=tExp(kExp)%Plane(2,0)
      else
        tExp(kExp)%xo=1.0d0
        tExp(kExp)%yo=0.0d0
      end if
      tExp(kExp)%O=0.0d0
      tExp(kExp)%e=0.0d0
      tExp(kExp)%e(1)=1.0d0
    end do
    kExp=1
    tExp(2)%iTypE=2
    tExp(1)%iOff=0
    nPar=tExp(1)%nPar
    do k=2,nExp
      tExp(k)%iOff=tExp(k-1)%iOff+tExp(k-1)%nPar
      nPar=nPar+tExp(k)%nPar
    end do
    mPar=100
    kPar=1
    call AllocatePar(ldum)
    if(.not.ldum) return
    dep_delExp=1.0d0
    fmin_genExp=0.0d0
    fmax_genExp=0.0d0
    fact_genExp=0.0d0
    finn_genExp=0.0d0
    fout_genExp=0.0d0
    dinn_genExp=1.0d0
    dout_genExp=1.0d0
    ainn_genExp=15.0d0
    aout_genExp=15.0d0
    df_genExp(1:2)=0.0d0
    dl_genExp(1:2)=0.0d0
    kB_genExp=1
    nE_genExp=0
    iE_genExp=1
    nS_genExp=1000
    iCl_genExp=-1
    iCn_genExp=-32001
    iDm_genExp=0
    iWf_genExp=0
    iWe_genExp=3
    fModExpFac=1.0d0
    ExpSumAcc=4.0d0
    iModExpMin=1
    iModExpMax=nExp-nExc
    iModExpBnd=0
    iModExpDom=0
    iModExpObj=0
    iModExpCol=0
    iModExpCon=0
    iModExpLoop=1
    iModExpWFA=1
    iModExpWFE=1
    lInitSurfWaves=.true.
  end Subroutine Expansion_Defaults

  Subroutine Object_Defaults(lCheck)
! set default object data
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    iObjDra=0
    lCHGLdoubleSide=.true.
    if((iObjDra.eq.3).or.(iObjDra.eq.4)) lCHGLdoubleSide=.false.
    lObjMat=.false.
    lObjMatW0=.false.
    lObjFlg=.false.
    lObjHid=.true.
    lGRCused=.not.lObjHid
    lGet3DMat=.true.
    call setNameExt(ProFileName,'3DO',ObjFileName)
    call setNameExt(ProFileName,'3DM',M3DFileName)
    call setNameExt(ProFileName,'3DD',D3DFileName)
    srObj='Unused'C
    siObj='Unused'C
    srObj(1,0)='Minimum angle'C
    srObj(2,0)='Sector angle'C
    srObj(4,0)='Mat.Pt.aspect ratio'C
    srObj(1,1)='Minimum z'C
    srObj(2,1)='Length'C
    srObj(4,1)='Mat.Pt.aspect ratio'C
    srObj(1,2)='Delta r'C
    srObj(2,2)='Delta phi'C
    srObj(3,2)='Delta z'C
    srObj(4,2)='Mat.Pt.aspect ratio'C
    srObj(1,3)='Match.pt.side length'C
    srObj(2,3)='Multipole density'C
    srObj(3,3)='Multipole distance'C
    srObj(1,4)='X side length'C
    srObj(2,4)='Y side length'C
    srObj(3,4)='Match.pt.side length'C
    srObj(4,4)='Multipole density'C
    srObj(5,4)='Multipole distance'C
    srObj(1,5)='Minimum z'C
    srObj(2,5)='Delta z'C
    srObj(3,5)='Multipole distance'C
    srObj(4,5)='Mat.Pt.aspect ratio'C
    siObj(1,0:2)='Min.boundary'C
    siObj(2,0:3)='Max.boundary'C
    siObj(3,0:2)='Min.expansion'C
    siObj(4,0:2)='Max.expansion'C
    siObj(5,0:2)='Max.multipoles'C
    siObj(1,3:4)='Use data of boundary'C
    siObj(2,3:4)='Max. multipoles per side'C
    siObj(3,3:4)='Max. multipole order and degree'C
    siObj(1,5)='Min.boundary'C
    siObj(2,5)='Max.boundary'C
    siObj(3,5)='Min.expansion'C
    siObj(4,5)='Max.expansion'C
    siObj(5,5)='Max.multipoles'C
    krObj=1
    kiObj=1
    lInsertObj=.true.
    lAskObj=.true.
    fGenExpObj=0.0d0
    iGenExpObj=0
    iGenExpIns=-1
    iGenExpMinE=0
    iGenExpMaxE=-1
    iGenExpMaxM=-32000
    iGenExpCl=0
    iGenExpCl2=-1
    iGenExpCn=0
    iGenExpOb=0
    lGenExpDel=.true.
    kObj=1
    nObj=1
    mObj=10
    iDraObj=kObj
    call AllocateObj(ldum)
    if(.not.ldum) return
    do kObj=1,nObj
      tObj(kObj)%iTypO=1
      tObj(kObj)%nInhibited=0
      tObj(kObj)%iCol=1
      tObj(kObj)%iColMin=30
      tObj(kObj)%iColMax=90
      tObj(kObj)%iPar(1:2)=1
      tObj(kObj)%iPar(3:5)=0
      tObj(kObj)%GrfRes=0.1d0
      tObj(kObj)%Par(1)=0.0d0
      tObj(kObj)%Par(2)=1.0d0
      tObj(kObj)%Par(3)=0.0d0
      tObj(kObj)%Par(4)=1.0d0
      tObj(kObj)%Par(5)=0.0d0
      tObj(kObj)%O(1:3)=0.0d0
      tObj(kObj)%e(1:3)=0.0d0
      tObj(kObj)%e(1)=1.0d0
      tObj(kObj)%Plane(1:3,0:3)=0.0d0
      tObj(kObj)%Plane(1,1)=1.0d0
      tObj(kObj)%Plane(2,2)=1.0d0
      tObj(kObj)%Plane(3,3)=1.0d0
    end do
    kObj=1
    icolMobj=2_2
    iconMobj=-1_2
    xminMobj=-1.0d0
    xmaxMobj=1.0d0
    yminMobj=-1.0d0
    ymaxMobj=1.0d0
    mInhibit=0
    nInhibit=0
    kInhibit=0
  end Subroutine Object_Defaults

  Subroutine Particle_Defaults(lCheck)
! set default particle data
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
	  ldum=lCheck
    call setNameExt(ProFileName,'PRT',PrtFileName)
    nParticle2D=0_4
    nParticle3D=0_4
    ParticleTimeStep=1.0d0
    ParticleStepLength=2.0d100
    Particle2DMirror(1:3,1:2)=0.0d0 ! mirror origin, tangent, normal vectors in the xy plane
    Particle2DMirror(2,1)=1.0d0
    Particle2DMirror(3,2)=1.0d0
    Particle3DMirror(1:4,1:3)=0.0d0 ! mirror origin, tangent1, tangent2, normal vectors in the xyz space
    Particle3DMirror(2,1)=1.0d0
    Particle3DMirror(3,2)=1.0d0
    Particle3DMirror(4,3)=1.0d0
  end Subroutine Particle_Defaults 

! Allocations

  Subroutine AddInhibit(loc,s,ldum)
    Implicit none
    Logical ldum
    Integer(4) loc,idum
    Character(32) s
    lGet3DMat=.true.
    ldum=.false.
    if(mInhibit.lt.1) then
      if(Allocated(sInhibit)) DeAllocate(sInhibit)
      if(Allocated(sInhibitAlt)) DeAllocate(sInhibitAlt)
      nInhibit=0
      kInhibit=0
    end if
    if((loc.lt.1).or.(loc.gt.nInhibit+1)) return
    if(nInhibit.gt.0) then
      if(Allocated(sInhibitAlt)) DeAllocate(sInhibitAlt)
      Allocate(sInhibitAlt(nInhibit),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for sInhibitAlt failed!'C,'Allocate inhibit'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(sInhibit)) DeAllocate(sInhibit)
        if(Allocated(sInhibitAlt)) DeAllocate(sInhibitAlt)
        nInhibit=0
        mInhibit=0
        return
      end if
      sInhibitAlt=sInhibit
    end if
    nInhibit=nInhibit+1
    if(nInhibit.gt.mInhibit) then
      if(Allocated(sInhibit)) DeAllocate(sInhibit)
      Allocate(sInhibit(nInhibit),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for sInhibit failed!'C,'Allocate inhibit'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(sInhibit)) DeAllocate(sInhibit)
        if(Allocated(sInhibitAlt)) DeAllocate(sInhibitAlt)
        nInhibit=0
        mInhibit=0
        return
      end if
      mInhibit=max(mInhibit,nInhibit)
    end if
    if(loc.gt.1) sInhibit(1:loc-1)=sInhibitAlt(1:loc-1)
    sInhibit(loc)=s
    if(loc.lt.nInhibit) sInhibit(loc+1:nInhibit)=sInhibitAlt(loc:nInhibit-1)
    if(Allocated(sInhibitAlt)) DeAllocate(sInhibitAlt)
    ldum=.true.
	end Subroutine AddInhibit

  Subroutine DelInhibit(loc)
    Implicit none
    Integer(4) loc
    lGet3DMat=.true.
    if(loc.gt.nInhibit) return
    if((loc.eq.0).or.(-loc.ge.nInhibit)) then
      nInhibit=0
    else if(loc.lt.0) then
      sInhibit(1:nInhibit+loc)=sInhibit(-loc+1:nInhibit)
      nInhibit=nInhibit+loc
    else
      sInhibit(loc:nInhibit-1)=sInhibit(loc+1:nInhibit)
      nInhibit=nInhibit-1
    end if
  end Subroutine DelInhibit

  Subroutine AllocateBnd(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(tBnd)) DeAllocate(tBnd)
    mBnd=max(mBnd,0)
    Allocate(tBnd(0:mBnd),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for tBnd failed!'C,'Allocate boundary'C, &
                        MB$OK.or.MB$IconExclamation)
      kBnd=0
      nBnd=0
      mBnd=0
      return
    end if
    ldum=.true.
	end Subroutine AllocateBnd

  Subroutine AllocateBndSpl(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(sSpline)) DeAllocate(sSpline)
    if(Allocated(xSpline)) DeAllocate(xSpline)
    if(Allocated(ySpline)) DeAllocate(ySpline)
    if(Allocated(xsSpline)) DeAllocate(xsSpline)
    if(Allocated(ysSpline)) DeAllocate(ysSpline)
    mBndSpl=max(mBndSpl,0)
    Allocate(sSpline(0:mBndSpl),xSpline(0:mBndSpl),ySpline(0:mBndSpl),xsSpline(0:mBndSpl),ysSpline(0:mBndSpl),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for splines failed!'C,'Allocate boundary'C, &
                        MB$OK.or.MB$IconExclamation)
      kBndSpl=0
      nBndSpl=0
      mBndSpl=0
      return
    end if
    nBndSpl=mBndSpl
    ldum=.true.
	end Subroutine AllocateBndSpl

  Subroutine AllocateBndEdg(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(tBndEdg)) DeAllocate(tBndEdg)
    mBndEdg=max(mBndEdg,0)
    Allocate(tBndEdg(0:mBndEdg),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for tBnd failed!'C,'Allocate boundary'C, &
                        MB$OK.or.MB$IconExclamation)
      kBndEdg=0
      nBndEdg=0
      mBndEdg=0
      return
    end if
    ldum=.true.
	end Subroutine 

  Subroutine AllocateBndPt(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(mBndPt.gt.10000) then
      idum=MessageBoxQQ('Memory allocation for BndPt failed!'C,'Too many matching points!'C, &
                        MB$OK.or.MB$IconExclamation)
      return
    end if
    ldum=.true.
    if(mBndPt.eq.mBndPtAlloc) return ! the requested amount of memory is already allocated
    mBndPt=max(mBndPt,0)
    if(Allocated(wBndPt)) DeAllocate(wBndPt)
    if(Allocated(eBndPt)) DeAllocate(eBndPt)
    if(Allocated(fBndPt)) DeAllocate(fBndPt)
    if(Allocated(iBndPt)) DeAllocate(iBndPt)
    if(Allocated(jBndPt)) DeAllocate(jBndPt)
    Allocate(wBndPt(0:mBndPt),eBndPt(0:mBndPt),fBndPt(0:mBndPt),iBndPt(0:mBndPt),jBndPt(0:mBndPt),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for BndPt failed!'C,'Allocate boundary points'C, &
                        MB$OK.or.MB$IconExclamation)
      if(Allocated(wBndPt)) DeAllocate(wBndPt)
      if(Allocated(eBndPt)) DeAllocate(eBndPt)
      if(Allocated(fBndPt)) DeAllocate(fBndPt)
      if(Allocated(iBndPt)) DeAllocate(iBndPt)
      if(Allocated(jBndPt)) DeAllocate(jBndPt)
      kBndPt=0
      nBndPt=0
      mBndPt=0
      mBndPtAlloc=-1
      ldum=.false.
      return
    end if
    wBndPt=1.0d0
    eBndPt=1.0d0
    fBndPt=1.0d0
    iBndPt=0
    jBndPt=0
    AerrorA=0.0e0
    AerrorM=0.0e0
    Condition=-1.0d0
    mBndPtAlloc=mBndPt
	end Subroutine AllocateBndPt

  Subroutine InsertBnd(iP,nP,ldum)
! if nP>0: insert nP new boundaries after boundary iP
! if nP<0: delete -nP boundaries iP,iP+1...
    Implicit none
    Integer(4) iP,nP,k,idum
    Logical ldum
    ldum=.false.
    if(Allocated(tBndAux)) DeAllocate(tBndAux)
    if(nP.lt.0) then ! delete polygon iP up to iP+(-nP-1), keep at least 1 polygon
      if((iP.lt.1).or.(iP.gt.nBnd).or.(nBnd.lt.1)) return
      do k=1,Min(-nP,nBnd-iP+1)
        call InsertBndEdg(iP,1,-tBnd(iP)%nEdge,ldum)
        if(.not.ldum) return
        if(iP.le.(nBnd-1)) tBnd(iP:nBnd-1)=tBnd(iP+1:nBnd)
        nBnd=nBnd-1
        kBnd=nBnd
      end do
    else             ! insert nP polygons
      if((iP.lt.0).or.(iP.gt.nBnd).or.(nP.eq.0)) return
      if(nBnd+nP.gt.mBnd) then ! allocate more memory
        Allocate(tBndAux(nBnd),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tBndAux failed!'C,'Insert boundary'C, &
                            MB$OK.or.MB$IconExclamation)
          return
        end if
        tBndAux(1:nBnd)=tBnd(1:nBnd)
        if(Allocated(tBnd)) DeAllocate(tBnd)
        Allocate(tBnd(0:nBnd+nP),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tBnd failed!'C,'Insert boundary'C, &
                            MB$OK.or.MB$IconExclamation)
          Allocate(tBnd(0:nBnd),stat=idum) ! try to allocate original memory
          if(idum.eq.0) then
            tBnd=tBndAux
          else                             ! severe allocation problem
            idum=MessageBoxQQ('All boundary data lost!'C,'Insert boundary'C, &
                              MB$OK.or.MB$IconExclamation)
            kBndEdg=0
            nBndEdg=0
            mBndEdg=0
            mBndSpl=0
            nBndSpl=0
            kBndSpl=0
            kBnd=0
            nBnd=0
            mBnd=0
            if(Allocated(tBndEdg)) DeAllocate(tBndEdg)
            if(Allocated(tBndEdgAux)) DeAllocate(tBndEdgAux)
            if(Allocated(tBnd)) DeAllocate(tBnd)
            if(Allocated(tBndAux)) DeAllocate(tBndAux)
            if(Allocated(sSpline)) DeAllocate(sSpline)
            if(Allocated(xSpline)) DeAllocate(xSpline)
            if(Allocated(ySpline)) DeAllocate(ySpline)
            if(Allocated(xsSpline)) DeAllocate(xsSpline)
            if(Allocated(ysSpline)) DeAllocate(ysSpline)
            if(Allocated(sSpl)) DeAllocate(sSpl)
            if(Allocated(xSpl)) DeAllocate(xSpl)
            if(Allocated(ySpl)) DeAllocate(ySpl)
            if(Allocated(xsSpl)) DeAllocate(xsSpl)
            if(Allocated(ysSpl)) DeAllocate(ysSpl)
          end if
          return
        end if
        mBnd=nBnd+nP
        tBnd(1:nBnd)=tBndAux(1:nBnd)
        if((iP+1).le.nBnd) tBnd(iP+1+nP:nBnd+nP)=tBndAux(iP+1:nBnd)
      else ! insert without allocating more memory
        do k=nBnd,iP,-1
          tBnd(k+nP)=tBnd(k)
        end do
      end if
      do k=iP+1,iP+nP ! insert defaults
        tBnd(k)%iTypB=1_2
        tBnd(k)%nMatPts=0
        tBnd(k)%nEdge=0
        tBnd(k)%kEdge=0
        if(k.gt.1) then
          tBnd(k)%iEdgeOffset=tBnd(k-1)%iEdgeOffset+tBnd(k-1)%nEdge
        else
          tBnd(k)%iEdgeOffset=0
        end if
        tBnd(k)%iLDom=1_2
        tBnd(k)%iRDom=0_2
        tBnd(k)%iCond=0_2
        tBnd(k)%iConn=0_2
        tBnd(k)%iCol=1_2
        tBnd(k)%Weight=1.0d0
        tBnd(k)%Weight2=1.0d0
        tBnd(k)%val(1)=0.0d0
        tBnd(k)%val(2)=0.0d0
        tBnd(k)%fAmpl=0.0d0
        tBnd(k)%Formula='0'C
      end do
      nBnd=nBnd+nP
      if(kBnd.gt.nBnd) kBnd=nBnd
      if(Allocated(tBndAux)) DeAllocate(tBndAux)
    end if
    ldum=.true.
  end Subroutine InsertBnd

  Subroutine InsertBndEdg(iP,iE,nE,ldum)
! if nE>0: insert nE corners in the boundary iP after corner iE
! if nE<0: delete -nE corners iE, iE+1, ... of the boundary iP
    Implicit none
    Integer(4) i,iP,iE,nE,n,k,idum
    Logical ldum
    ldum=.false.
    if((iP.lt.1).or.(iP.gt.nBnd).or.(iE.gt.tBnd(iP)%nEdge).or.(iE.lt.0)) return
    if(Allocated(tBndEdgAux)) DeAllocate(tBndEdgAux)
    i=iE+tBnd(iP)%iEdgeOffset
    n=nE
    if(nE.lt.0) then ! delete edge iE up to iE+(-nE-1)
      if(iE.lt.1) return
      n=Min(-nE,tBnd(iP)%nEdge-iE+1)
      if((n.gt.0).and.(i.le.(nBndEdg-n))) tBndEdg(i:nBndEdg-n)=tBndEdg(i+n:nBndEdg)
      n=-n
    else             ! insert nE edges
      if(nBndEdg+n.gt.mBndEdg) then ! allocate more memory
        Allocate(tBndEdgAux(nBndEdg),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tBndEdgAux failed!'C,'Insert boundary corner'C, &
                            MB$OK.or.MB$IconExclamation)
          return
        end if
        tBndEdgAux(1:nBndEdg)=tBndEdg(1:nBndEdg)
        if(Allocated(tBndEdg)) DeAllocate(tBndEdg)
        Allocate(tBndEdg(0:nBndEdg+n),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tBndEdg failed!'C,'Insert boundary corner'C, &
                            MB$OK.or.MB$IconExclamation)
          Allocate(tBndEdg(0:nBndEdg),stat=idum) ! try to allocate original memory
          if(idum.eq.0) then
            tBndEdg=tBndEdgAux
          else                               ! severe allocation problem
            idum=MessageBoxQQ('All boundary data lost!'C,'Insert boundary corner'C, &
                              MB$OK.or.MB$IconExclamation)
            kBndEdg=0
            nBndEdg=0
            mBndEdg=0
            kBnd=0
            nBnd=0
            mBnd=0
            if(Allocated(tBndEdg)) DeAllocate(tBndEdg)
            if(Allocated(tBndEdgAux)) DeAllocate(tBndEdgAux)
            if(Allocated(tBnd)) DeAllocate(tBnd)
            if(Allocated(tBndAux)) DeAllocate(tBndAux)
          end if
          return
        end if
        mBndEdg=nBndEdg+n
        tBndEdg(1:nBndEdg)=tBndEdgAux(1:nBndEdg)
        if((i+1).le.nBndEdg) tBndEdg(i+1+n:nBndEdg+n)=tBndEdgAux(i+1:nBndEdg)
      else ! insert edges without allocating more memory
        if(n.gt.0) then
          do k=nBndEdg,i,-1
            tBndEdg(k+n)=tBndEdg(k)
          end do
        end if
      end if
      do k=i+1,i+n ! insert defaults
        tBndEdg(k)%iOrient=1_2
        tBndEdg(k)%x=0.0d0
        tBndEdg(k)%y=0.0d0
        tBndEdg(k)%r=0.0d0
        tBndEdg(k)%xa=0.0d0
        tBndEdg(k)%ya=0.0d0
        tBndEdg(k)%xb=0.0d0
        tBndEdg(k)%yb=0.0d0
        tBndEdg(k)%xo=0.0d0
        tBndEdg(k)%yo=0.0d0
      end do
    end if
    nBndEdg=nBndEdg+n
    if(kBndEdg.gt.nBndEdg) kBndEdg=nBndEdg
    tBnd(iP)%nEdge=tBnd(iP)%nEdge+n
    tBnd(iP)%kEdge=1
    tBnd(1)%iEdgeOffset=0
    do k=2,nBnd
      tBnd(k)%iEdgeOffset=tBnd(k-1)%iEdgeOffset+tBnd(k-1)%nEdge
    end do
    ldum=.true.
  end Subroutine InsertBndEdg

  Subroutine InsertBndSpl(iP,iE,nE,ldum)
! if nE>0: insert nE spline points in the boundary iP after spline point iE
! if nE<0: delete -nE spline points iE, iE+1, ... of the boundary iP
    Implicit none
    Integer(4) i,iP,iE,nE,n,k,idum
    Logical ldum
    ldum=.false.
    if((iP.lt.1).or.(iP.gt.nBnd).or.(tBnd(iP)%nSpline.lt.1).or.(iE.gt.tBnd(iP)%nSpline).or.(iE.lt.0)) return
    if(Allocated(sSpl)) DeAllocate(sSpl)
    if(Allocated(xSpl)) DeAllocate(xSpl)
    if(Allocated(ySpl)) DeAllocate(ySpl)
    if(Allocated(xsSpl)) DeAllocate(xsSpl)
    if(Allocated(ysSpl)) DeAllocate(ysSpl)
    i=iE+tBnd(iP)%iSplineOffset
    n=nE
    if(nE.lt.0) then ! delete point iE up to iE+(-nE-1)
      if(iE.lt.1) return
      n=Min(-nE,tBnd(iP)%nSpline-iE+1)
      if((n.gt.0).and.(i.le.(nBndSpl-n))) then
        sSpline(i:nBndSpl-n)=sSpline(i+n:nBndSpl)
        xSpline(i:nBndSpl-n)=xSpline(i+n:nBndSpl)
        ySpline(i:nBndSpl-n)=ySpline(i+n:nBndSpl)
        xsSpline(i:nBndSpl-n)=xsSpline(i+n:nBndSpl)
        ysSpline(i:nBndSpl-n)=ysSpline(i+n:nBndSpl)
      end if
      n=-n
    else             ! insert nE points
      if(nBndSpl+n.gt.mBndSpl) then ! allocate more memory
        Allocate(sSpl(nBndSpl),xSpl(nBndSpl),ySpl(nBndSpl),xsSpl(nBndSpl),ysSpl(nBndSpl),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for splines failed!'C,'Insert boundary spline'C, &
                            MB$OK.or.MB$IconExclamation)
          return
        end if
        sSpl(1:nBndSpl)=sSpline(1:nBndSpl)
        xSpl(1:nBndSpl)=xSpline(1:nBndSpl)
        ySpl(1:nBndSpl)=ySpline(1:nBndSpl)
        xsSpl(1:nBndSpl)=xsSpline(1:nBndSpl)
        ysSpl(1:nBndSpl)=ysSpline(1:nBndSpl)
        if(Allocated(sSpline)) DeAllocate(sSpline)
        if(Allocated(xSpline)) DeAllocate(xSpline)
        if(Allocated(ySpline)) DeAllocate(ySpline)
        if(Allocated(xsSpline)) DeAllocate(xsSpline)
        if(Allocated(ysSpline)) DeAllocate(ysSpline)
        Allocate(sSpline(0:nBndSpl+n),xSpline(0:nBndSpl+n),ySpline(0:nBndSpl+n),xsSpline(0:nBndSpl+n), &
        &        ysSpline(0:nBndSpl+n),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for splines failed!'C,'Insert boundary spline'C, &
                            MB$OK.or.MB$IconExclamation)
          Allocate(sSpline(0:nBndSpl),xSpline(0:nBndSpl),ySpline(0:nBndSpl),xsSpline(0:nBndSpl), &
        &        ysSpline(0:nBndSpl),stat=idum) ! try to allocate original memory
          if(idum.eq.0) then
            sSpline=sSpl
            xSpline=xSpl
            ySpline=ySpl
            xsSpline=xsSpl
            ysSpline=ysSpl
          else                               ! severe allocation problem
            idum=MessageBoxQQ('All spline data lost!'C,'Insert boundary spline'C, &
                              MB$OK.or.MB$IconExclamation)
            kBndSpl=0
            nBndSpl=0
            mBndSpl=0
            if(Allocated(sSpline)) DeAllocate(sSpline)
            if(Allocated(xSpline)) DeAllocate(xSpline)
            if(Allocated(ySpline)) DeAllocate(ySpline)
            if(Allocated(xsSpline)) DeAllocate(xsSpline)
            if(Allocated(ysSpline)) DeAllocate(ysSpline)
            if(Allocated(sSpl)) DeAllocate(sSpl)
            if(Allocated(xSpl)) DeAllocate(xSpl)
            if(Allocated(ySpl)) DeAllocate(ySpl)
            if(Allocated(xsSpl)) DeAllocate(xsSpl)
            if(Allocated(ysSpl)) DeAllocate(ysSpl)
          end if
          return
        end if
        mBndSpl=nBndSpl+n
        sSpline(1:nBndSpl)=sSpl(1:nBndSpl)
        xSpline(1:nBndSpl)=xSpl(1:nBndSpl)
        ySpline(1:nBndSpl)=ySpl(1:nBndSpl)
        xsSpline(1:nBndSpl)=xsSpl(1:nBndSpl)
        ysSpline(1:nBndSpl)=ysSpl(1:nBndSpl)
        if((i+1).le.nBndSpl) then
          sSpline(i+1+n:nBndSpl+n)=sSpl(i+1:nBndSpl)
          xSpline(i+1+n:nBndSpl+n)=xSpl(i+1:nBndSpl)
          ySpline(i+1+n:nBndSpl+n)=ySpl(i+1:nBndSpl)
          xsSpline(i+1+n:nBndSpl+n)=xsSpl(i+1:nBndSpl)
          ysSpline(i+1+n:nBndSpl+n)=ysSpl(i+1:nBndSpl)
        end if
      else ! insert Splines without allocating more memory
        if(n.gt.0) then
          do k=nBndSpl,i,-1
            sSpline(k+n)=sSpline(k)
            xSpline(k+n)=xSpline(k)
            ySpline(k+n)=ySpline(k)
            xsSpline(k+n)=xsSpline(k)
            ysSpline(k+n)=ysSpline(k)
          end do
        end if
      end if
      do k=i+1,i+n ! insert defaults
        sSpline(k)=0.0d0
        xSpline(k)=0.0d0
        ySpline(k)=0.0d0
        xsSpline(k)=0.0d0
        ysSpline(k)=0.0d0
      end do
    end if
    nBndSpl=nBndSpl+n
    if(kBndSpl.gt.nBndSpl) kBndSpl=nBndSpl
    tBnd(iP)%nSpline=tBnd(iP)%nSpline+n
    tBnd(1)%iSplineOffset=0
    do k=2,nBnd
      tBnd(k)%iSplineOffset=tBnd(k-1)%iSplineOffset+tBnd(k-1)%nSpline
    end do
    ldum=.true.
	end Subroutine InsertBndSpl

  Subroutine CopyBnd(kB0,iB0,dx,dy,iCnew,ldum)
! Copy the boundary kB and insert it after boundary iB
    Implicit none
    Integer(4) i,kB,iB,k,n,j
    Integer(4), Intent(in):: kB0,iB0,iCnew
    Real(8), Intent(in):: dx,dy
    Logical ldum
    ldum=.false.
    kB=kB0
    iB=iB0
    if((kB.lt.1).or.(kB.gt.nBnd).or.(iB.lt.0).or.(iB.gt.nBnd)) return
    n=tBnd(kB)%nEdge
    call InsertBnd(iB,1,ldum)
    if(.not.ldum) return
    i=iB+1
    call InsertBndEdg(i,0,n,ldum)
    if(.not.ldum) return
    j=kB
    if(iB.lt.kB) j=j+1
    do k=1,n
      tBndEdg(tBnd(i)%iEdgeOffset+k)=tBndEdg(tBnd(j)%iEdgeOffset+k)
      tBndEdg(tBnd(i)%iEdgeOffset+k)%x=tBndEdg(tBnd(i)%iEdgeOffset+k)%x+dx
      tBndEdg(tBnd(i)%iEdgeOffset+k)%y=tBndEdg(tBnd(i)%iEdgeOffset+k)%y+dy
    end do
    k=tBnd(i)%iEdgeOffset
    tBnd(i)=tBnd(j)
    if(iCnew.gt.-2_4) tBnd(i)%iCol=Int2(iCnew)
    tBnd(i)%iEdgeOffset=k
    iBound=0_2
	end Subroutine CopyBnd

  Subroutine AllocateDom(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    DeAllocate(iDom,idDom,eDom,uDom,sDom,tDom,dDom,gDom,aDom,bDom,Dom_Form,stat=idum)
    mDom=max(mDom,0)
    Allocate(eDom(0:mDom),uDom(0:mDom),sDom(0:mDom),iDom(0:mDom),idDom(2,0:mDom),tDom(0:mDom),dDom(0:mDom),gDom(0:mDom), &
    & aDom(6,0:mDom),bDom(6,0:mDom),Dom_Form(4,0:mDom),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for domains failed!'C,'Allocate domain'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(iDom,idDom,eDom,uDom,sDom,tDom,dDom,gDom,aDom,bDom,Dom_Form,stat=idum)
      nDom=0
      return
    end if
    iDom=1_2
    idDom=1_2
    eDom=(1.0d0,0.0d0)
    uDom=(1.0d0,0.0d0)
    sDom=(0.0d0,0.0d0)
    tDom=(0.0d0,0.0d0)
    dDom=0.0d0
    gDom=0.0d0
    aDom=0.0d0
    bDom=0.0d0
    Dom_Form(1:2,0:mDom)='1.0'C
    Dom_Form(3:4,0:mDom)='0.0'C
    iDom(0)=0_2
    ldum=.true.
	end Subroutine AllocateDom

  Subroutine IncreaseDom(inc,ldum)
    Implicit none
    Integer(4) inc,idum
    Logical ldum
    ldum=.false.
    if(Allocated(hDom_Form)) DeAllocate(hDom_Form)
    Allocate(hDom_Form(4,0:mDom),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Increase domains'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(iDom,idDom,eDom,uDom,sDom,tDom,dDom,gDom,aDom,bDom,Dom_Form,hDom_Form,stat=idum)
      nDom=0
      return
    end if
    hDom_Form(1:4,0:mDom)=Dom_Form(1:4,0:mDom)
    if(Allocated(Dom_Form)) DeAllocate(Dom_Form)
    Allocate(Dom_Form(4,0:mDom+inc),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation failed!'C,'Increase domains'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(iDom,idDom,eDom,uDom,sDom,tDom,dDom,gDom,aDom,bDom,Dom_Form,hDom_Form,stat=idum)
      nDom=0
      return
    end if
    Dom_Form(1:4,0:mDom)=hDom_Form(1:4,0:mDom)
    Dom_Form(1:2,mDom+1:mDom+inc)='1.0'C
    Dom_Form(3:4,mDom+1:mDom+inc)='0.0'C
    mDom=mDom+inc
    DeAllocate(iDom,idDom,eDom,uDom,sDom,tDom,dDom,gDom,aDom,bDom,hDom_Form,stat=idum)
    Allocate(eDom(0:mDom),uDom(0:mDom),sDom(0:mDom),iDom(0:mDom),idDom(2,0:mDom),tDom(0:mDom),dDom(0:mDom),gDom(0:mDom), &
    & aDom(6,0:mDom),bDom(6,0:mDom),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for domains failed!'C,'Increase domains'C, &
                        MB$OK.or.MB$IconExclamation)
      DeAllocate(iDom,idDom,eDom,uDom,sDom,tDom,dDom,gDom,aDom,bDom,Dom_Form,stat=idum)
      nDom=0
      return
    end if
    iDom=1_2
    idDom=1_2
    eDom=(1.0d0,0.0d0)
    uDom=(1.0d0,0.0d0)
    sDom=(0.0d0,0.0d0)
    tDom=(0.0d0,0.0d0)
    dDom=0.0d0
    gDom=0.0d0
    aDom=0.0d0
    bDom=0.0d0
    iDom(0)=0_2
    ldum=.true.
	end Subroutine IncreaseDom

  Subroutine AllocateExp(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(tExp)) DeAllocate(tExp)
    mExp=max(mExp,0)
    Allocate(tExp(0:mExp),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for tExp failed!'C,'Allocate expansion'C, &
                        MB$OK.or.MB$IconExclamation)
      kExp=0
      nExp=0
      mExp=0
      return
    end if
    ldum=.true.
	end Subroutine AllocateExp

  Subroutine AllocatePar(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(ParExp)) DeAllocate(ParExp)
    if(Allocated(iParExp)) DeAllocate(iParExp)
    nRHS=max(nRHS,1)
    mPar=max(mPar,0)
    Allocate(ParExp(1:nRHS,0:mPar),iParExp(1:nRHS,0:mPar),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for ParExp failed!'C,'Allocate expansion parameter'C, &
                        MB$OK.or.MB$IconExclamation)
      kPar=0
      nPar=0
      mPar=0
      return
    end if
    ParExp(1:nRHS,0:mPar)=(0.0d0,0.0d0)
    iParExp(1:nRHS,0:mPar)=0_2
    nParN=mPar
    ldum=.true.
	end Subroutine AllocatePar

  Subroutine InsertExp(iP,nP,ldum)
    Implicit none
    Integer(4), Intent(in) :: iP,nP
    Integer(4) k,idum
    Logical ldum
    ldum=.false.
    if(Allocated(tExpAux)) DeAllocate(tExpAux)
    if(nP.lt.0) then ! delete expansion iP up to iP+(-nP-1), keep at least 1 expansion
      if((iP.lt.1).or.(iP.gt.nExp).or.(nExp.lt.1)) return
      do k=1,Min(-nP,nExp-iP+1)
        if(nExp.lt.2) Exit
        call InsertPar(iP,1,Int4(-tExp(iP)%nPar),ldum)
        if(.not.ldum) return
        if(iP.le.(nExp-1)) tExp(iP:nExp-1)=tExp(iP+1:nExp)
        nExp=nExp-1
        kExp=nExp
      end do
    else             ! insert nP Expansions
      if((iP.lt.0).or.(iP.gt.nExp).or.(nP.eq.0)) return
      if(nExp+nP.gt.mExp) then ! allocate more memory
        Allocate(tExpAux(nExp),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tExpAux failed!'C,'Insert expansion'C, &
                            MB$OK.or.MB$IconExclamation)
          return
        end if
        tExpAux(1:nExp)=tExp(1:nExp)
        if(Allocated(tExp)) DeAllocate(tExp)
        Allocate(tExp(0:nExp+nP),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tExp failed!'C,'Insert expansion'C, &
                            MB$OK.or.MB$IconExclamation)
          Allocate(tExp(0:nExp),stat=idum) ! try to allocate original memory
          if(idum.eq.0) then
            tExp=tExpAux
          else                             ! severe allocation problem
            idum=MessageBoxQQ('All boundary data lost!'C,'Insert expansion'C, &
                              MB$OK.or.MB$IconExclamation)
            kPar=0
            nPar=0
            mPar=0
            kExp=0
            nExp=0
            mExp=0
            if(Allocated(ParExp)) DeAllocate(ParExp)
            if(Allocated(iParExp)) DeAllocate(iParExp)
            if(Allocated(ParExpAux)) DeAllocate(ParExpAux)
            if(Allocated(iParExpAux)) DeAllocate(iParExpAux)
            if(Allocated(tExp)) DeAllocate(tExp)
            if(Allocated(tExpAux)) DeAllocate(tExpAux)
          end if
          return
        end if
        mExp=nExp+nP
        tExp(1:nExp)=tExpAux(1:nExp)
        if((iP+1).le.nExp) tExp(iP+1+nP:nExp+nP)=tExpAux(iP+1:nExp)
      else ! insert without allocating more memory
        do k=nExp,iP,-1
          tExp(k+nP)=tExp(k)
        end do
      end if
      do k=1,nP ! copies of the original
        tExp(iP+k)=tExp(iP)
      end do
      do k=iP+1,iP+nP ! insert defaults
        tExp(k)%iTypE=1
        tExp(k)%iDom=1
        tExp(k)%iConn=0
        tExp(k)%iCol=1
        tExp(k)%iObj=0
        tExp(k)%nPar=0
        if(k.gt.1) then
          tExp(k)%iOff=tExp(k-1)%iOff+tExp(k-1)%nPar
        else
          tExp(k)%iOff=0
        end if
        tExp(k)%iHE=2
        tExp(k)%iS=0
        tExp(k)%iE(1:6)=0
        tExp(k)%iE(2)=100
        tExp(k)%iE(5)=1
        tExp(k)%rE(1:5)=0.0d0
        tExp(k)%gc=gcFld
        tExp(k)%depend=1.0d100
        tExp(k)%Plane(1:3,0:3)=0.0d0
        tExp(k)%Plane(1,1)=1.0d0
        tExp(k)%Plane(2,2)=1.0d0
        tExp(k)%Plane(3,3)=1.0d0
        if(tExp(k)%iTypE.ne.10) then
          tExp(k)%xo=tExp(k)%Plane(1,0)
          tExp(k)%yo=tExp(k)%Plane(2,0)
        else
          tExp(k)%xo=1.0d0
          tExp(k)%yo=0.0d0
        end if
        tExp(k)%O=0.0d0
        tExp(k)%e=0.0d0
        tExp(k)%e(2)=1.0d0
      end do
      nExp=nExp+nP
      if(kExp.gt.nExp) kExp=nExp
      if(Allocated(tExpAux)) DeAllocate(tExpAux)
    end if
    ldum=.true.
	end Subroutine InsertExp

  Subroutine InsertPar(iP,iE,nE,ldum)
    Implicit none
    Integer(4) i,iP,iE,nE,n,k,idum
    Logical ldum
    ldum=.false.
    if((iP.lt.1).or.(iP.gt.nExp).or.(iE.gt.tExp(iP)%nPar).or.(iE.lt.0)) return
    if(Allocated(ParExpAux)) DeAllocate(ParExpAux)
    if(Allocated(iParExpAux)) DeAllocate(iParExpAux)
    i=iE+tExp(iP)%iOff
    n=nE
    if(nE.lt.0) then ! delete parameter iE up to iE+(-nE-1)
      n=Min(-nE,tExp(iP)%nPar-iE+1)
      if((n.gt.0).and.(i.le.(nPar-n))) then
        ParExp(1:nRHS,i:nPar-n)=ParExp(1:nRHS,i+n:nPar)
        iParExp(1:nRHS,i:nPar-n)=iParExp(1:nRHS,i+n:nPar)
      end if
      n=-n
    else             ! insert nE parameters
      if(nPar+n.gt.mPar) then ! allocate more memory
        Allocate(ParExpAux(1:nRHS,0:nPar),iParExpAux(1:nRHS,0:nPar),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for ParExpAux failed!'C,'Insert expansion parameter'C, &
                            MB$OK.or.MB$IconExclamation)
          return
        end if
        ParExpAux(1:nRHS,0:nPar)=ParExp(1:nRHS,0:nPar)
        iParExpAux(1:nRHS,0:nPar)=iParExp(1:nRHS,0:nPar)
        if(Allocated(ParExp)) DeAllocate(ParExp)
        if(Allocated(iParExp)) DeAllocate(iParExp)
		    nParN=max(4,nPar+n)
		    Allocate(ParExp(1:nRHS,0:nParN),iParExp(1:nRHS,0:nParN),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for ParExp failed!'C,'Insert expansion parameter'C, &
                            MB$OK.or.MB$IconExclamation)
          Allocate(ParExp(1:nRHS,0:nPar),iParExp(1:nRHS,0:nPar),stat=idum) ! try to allocate original memory
          if(idum.eq.0) then
            ParExp=ParExpAux
            iParExp=iParExpAux
          else                               ! severe allocation problem
            idum=MessageBoxQQ('All boundary data lost!'C,'Insert expansion parameter'C, &
                              MB$OK.or.MB$IconExclamation)
            kPar=0
            nPar=0
            mPar=0
            kExp=0
            nExp=0
            mExp=0
            if(Allocated(ParExp)) DeAllocate(ParExp)
            if(Allocated(iParExp)) DeAllocate(iParExp)
            if(Allocated(ParExpAux)) DeAllocate(ParExpAux)
            if(Allocated(iParExpAux)) DeAllocate(iParExpAux)
            if(Allocated(tExp)) DeAllocate(tExp)
            if(Allocated(tExpAux)) DeAllocate(tExpAux)
          end if
          return
        end if
        mPar=nPar+n
        ParExp(1:nRHS,0:nPar)=ParExpAux(1:nRHS,0:nPar)
        iParExp(1:nRHS,0:nPar)=iParExpAux(1:nRHS,0:nPar)
        if((i+1).le.nPar) ParExp(1:nRHS,i+1+n:nPar+n)=ParExpAux(1:nRHS,i+1:nPar)
        if((i+1).le.nPar) iParExp(1:nRHS,i+1+n:nPar+n)=iParExpAux(1:nRHS,i+1:nPar)
      else ! insert parameters without allocating more memory
        if((n.gt.0).and.(nPar.gt.i)) then
          do k=nPar,i,-1
            ParExp(1:nRHS,k+n)=ParExp(1:nRHS,k)
            iParExp(1:nRHS,k+n)=iParExp(1:nRHS,k)
          end do
        end if
      end if
      do k=i+1,i+n ! insert defaults
        ParExp(1:nRHS,k)=(0.0d0,0.0d0)
        iParExp(1:nRHS,k)=0_2
      end do
    end if
    nPar=nPar+n
    if(kPar.gt.nPar) kPar=nPar
    tExp(iP)%nPar=tExp(iP)%nPar+n
    tExp(1)%iOff=0
    do k=2,nExp
      tExp(k)%iOff=tExp(k-1)%iOff+tExp(k-1)%nPar
    end do
    ldum=.true.
	end Subroutine InsertPar

  Subroutine CopyExp(kB0,iB0,dx,dy,iCnew,ldum)
! Copy the expansion kB and insert it after expansion iB
    Implicit none
    Integer(4) i,kB,iB,k,n,j
    Integer(4), Intent(in):: kB0,iB0,iCnew
    Real(8), Intent(in):: dx,dy
    Logical ldum
    ldum=.false.
    kB=kB0
    iB=iB0
    if((kB.lt.1).or.(kB.gt.nExp).or.(iB.lt.0).or.(iB.gt.nExp)) return
    n=tExp(kB)%nPar
    call InsertExp(iB,1,ldum)
    if(.not.ldum) return
    i=iB+1
    call InsertPar(i,0,n,ldum)
    if(.not.ldum) return
    j=kB
    if(iB.lt.kB) j=j+1
    if(iCnew.gt.-3_4) then
      do k=1,n
        ParExp(1:nRHS,tExp(i)%iOff+k)=ParExp(1:nRHS,tExp(j)%iOff+k)
        iParExp(1:nRHS,tExp(i)%iOff+k)=iParExp(1:nRHS,tExp(j)%iOff+k)
      end do
    else
      do k=1,n
        ParExp(1:nRHS,tExp(i)%iOff+k)=(1.0d0,0.0d0)
        iParExp(1:nRHS,tExp(i)%iOff+k)=0
      end do
    end if
    k=tExp(i)%iOff
    tExp(i)=tExp(j)
    if(iCnew.gt.-2_4) tExp(i)%iCol=Int2(iCnew)
    tExp(i)%Plane(1,0)=tExp(i)%Plane(1,0)+dx
    tExp(i)%Plane(2,0)=tExp(i)%Plane(2,0)+dy
    tExp(i)%xo=tExp(i)%xo+dx
    tExp(i)%yo=tExp(i)%yo+dy
    tExp(i)%O(1)=tExp(i)%O(1)+dx
    tExp(i)%O(2)=tExp(i)%O(2)+dy
    tExp(i)%iOff=k
  end Subroutine CopyExp

  Subroutine SetExp(iB,iDm,iCn,iCl,x,y)
! set the expansion iB at (x,y) in the xy plane and modify its domain, connection, and color numbers
! reset object and dependence flags
    Implicit none
    Integer(4), Intent(in) :: iB
    Integer(2), Intent(in) :: iCl,iCn,iDm
    Real(8), Intent(in):: x,y
    tExp(iB)%iDom=iDm
    tExp(iB)%iConn=iCn
    tExp(iB)%iCol=iCl
    if(iCl.lt.-1) tExp(iB)%iCol=iDom(Max(1_2,Min(Int2(nDom),iDm)))
    if(lgcFld.or.((tExp(iB)%iTypE.gt.5).and.(tExp(iB)%iTypE.ne.12))) then
      tExp(iB)%iObj=0
    else
      tExp(iB)%iObj=-1
    end if
    tExp(iB)%depend=1.0d100
    tExp(iB)%Plane(1:3,0:3)=0.0d0
    if(tExp(iB)%iTypE.ne.8) tExp(iB)%Plane(1,0)=x
    tExp(iB)%Plane(2,0)=y
    tExp(iB)%Plane(1,1)=1.0d0
    tExp(iB)%Plane(2,2)=1.0d0
    tExp(iB)%Plane(3,3)=1.0d0
    if(tExp(iB)%iTypE.ne.10) then
      tExp(iB)%xo=tExp(iB)%Plane(1,0)
      tExp(iB)%yo=tExp(iB)%Plane(2,0)
    else
      tExp(iB)%xo=1.0d0
      tExp(iB)%yo=0.0d0
    end if
    if(tExp(iB)%iTypE.eq.8) tExp(iB)%rE(1)=x
  end Subroutine SetExp

  Subroutine CopyColor(iC,iCnew,iB,iE,dx,dy)
! Copy the boundaries and expansions with color number iC and insert them after boundary iB and expansion iE
! nex color iCnew, new locations = old locations + (dx,dy)
    implicit none
    Integer(4) iC,iCnew,iB,iE,i
    Real(8) dx,dy
    Logical ldum
    i=nBnd
    do while(i.gt.0)
      if((iC.eq.0).or.((iC.gt.0).and.(tBnd(i)%iCol.eq.iC)).or.((iC.lt.0).and.(tBnd(i)%iCol.le.-iC))) then
        call CopyBnd(i,iB,dx,dy,-1_4,ldum)
        if((i.le.iB).or.(i.eq.1)) i=i-1
      else
        i=i-1
      end if
    end do
    do i=1,nBnd
      if(tBnd(i)%iCol.eq.-1_4) tBnd(i)%iCol=Int2(iCnew)
    end do
    i=nExp
    do while(i.gt.0)
      if((iC.eq.0).or.((iC.gt.0).and.(tExp(i)%iCol.eq.iC)).or.((iC.lt.0).and.(tExp(i)%iCol.le.-iC))) then
        call CopyExp(i,iE,dx,dy,-1_4,ldum)
        if((i.le.iE).or.(i.eq.1)) i=i-1
      else
        i=i-1
      end if
    end do
    do i=1,nExp
      if(tExp(i)%iCol.eq.-1_4) tExp(i)%iCol=Int2(iCnew)
    end do
  end Subroutine CopyColor

  Subroutine generateColorRec(iC,iCnew,nx,ny,dx,dy)
! generate copies of the boundaries and expansions with color iC on a rectangular grid
    implicit none
    Integer(4) iC,iCnew,iC1,nx,ny,i,k
    Real(8) dx,dy,x,y
    iC1=iCnew
    if(iC.eq.iCnew) iC1=-1
    y=0.0d0
    do k=1,ny
      x=0.0d0
      do i=1,nx
        if((i.ne.1).or.(k.ne.1)) call copyColor(iC,iC1,0,0,x,y) ! don't copy original position
        x=x+dx
      end do
      y=y+dy
    end do
    if(iC.eq.iCnew) then
      do i=1,nBnd
        if(tBnd(i)%iCol.eq.-1_4) tBnd(i)%iCol=Int2(iCnew)
      end do
      do i=1,nExp
        if(tExp(i)%iCol.eq.-1_4) tExp(i)%iCol=Int2(iCnew)
      end do
    end if
  end Subroutine generateColorRec

  Subroutine generateColorHex(iC,iCnew,nx,ny,dx,dy,angle)
! generate copies of the boundaries and expansions with color iC on a hexagonal grid
    implicit none
    Integer(4) iC,iCnew,iC1,nx,ny,i,k
    Real(8) dx,dy,angle,x,y,dxa
    iC1=iCnew
    if(iC.eq.iCnew) iC1=-1
    dxa=dy/dtan(angle*Pi/180.0d0)
    y=0.0d0
    do k=1,ny
      x=0.0d0
      do i=1,nx
        if((i.ne.1).or.(k.ne.1)) then ! don't copy original position
          if(mod(k,2).eq.1) then
            call copyColor(iC,iC1,0,0,x,y)
          else
            call copyColor(iC,iC1,0,0,x+dxa,y)
          end if
        end if
        x=x+dx
      end do
      y=y+dy
    end do
    if(iC.eq.iCnew) then
      do i=1,nBnd
        if(tBnd(i)%iCol.eq.-1_4) tBnd(i)%iCol=Int2(iCnew)
      end do
      do i=1,nExp
        if(tExp(i)%iCol.eq.-1_4) tExp(i)%iCol=Int2(iCnew)
      end do
    end if
  end Subroutine generateColorHex

  Subroutine AllocateObj(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    lGet3DMat=.true.
    if(Allocated(tObj)) DeAllocate(tObj)
    mObj=max(mObj,0)
    Allocate(tObj(0:mObj),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for tObj failed!'C,'Allocate object'C, &
                        MB$OK.or.MB$IconExclamation)
      kObj=0
      nObj=0
      mObj=0
      return
    end if
    ldum=.true.
	end Subroutine AllocateObj

  Subroutine InsertObj(iP,nP,ldum)
    Implicit none
    Integer(4), Intent(in) :: iP,nP
    Integer(4) k,idum
    Logical ldum
    ldum=.false.
    lGet3DMat=.true.
    if(Allocated(tObjAux)) DeAllocate(tObjAux)
    if(nP.lt.0) then ! delete Object iP up to iP+(-nP-1), keep at least 1 Object
      if((iP.lt.1).or.(iP.gt.nObj).or.(nObj.lt.1)) return
      do k=1,Min(-nP,nObj-iP+1)
        if(iP.le.(nObj-1)) tObj(iP:nObj-1)=tObj(iP+1:nObj)
        nObj=nObj-1
        kObj=nObj
      end do
    else             ! insert nP Objects
      if((iP.lt.0).or.(iP.gt.nObj).or.(nP.eq.0)) return
      if(nObj+nP.gt.mObj) then ! allocate more memory
        Allocate(tObjAux(nObj),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tObjAux failed!'C,'Insert Object'C, &
                            MB$OK.or.MB$IconExclamation)
          return
        end if
        tObjAux(1:nObj)=tObj(1:nObj)
        if(Allocated(tObj)) DeAllocate(tObj)
        Allocate(tObj(0:nObj+nP),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tObj failed!'C,'Insert Object'C, &
                            MB$OK.or.MB$IconExclamation)
          Allocate(tObj(0:nObj),stat=idum) ! try to allocate original memory
          if(idum.eq.0) then
            tObj=tObjAux
          else                             ! severe allocation problem
            idum=MessageBoxQQ('All boundary data lost!'C,'Insert Object'C, &
                              MB$OK.or.MB$IconExclamation)
            kObj=0
            nObj=0
            mObj=0
            if(Allocated(tObj)) DeAllocate(tObj)
            if(Allocated(tObjAux)) DeAllocate(tObjAux)
          end if
          return
        end if
        mObj=nObj+nP
        tObj(1:nObj)=tObjAux(1:nObj)
        if((iP+1).le.nObj) tObj(iP+1+nP:nObj+nP)=tObjAux(iP+1:nObj)
      else ! insert without allocating more memory
        do k=nObj,iP,-1
          tObj(k+nP)=tObj(k)
        end do
      end if
      do k=1,nP ! copies of the original
        tObj(iP+k)=tObj(iP)
      end do
      do k=iP+1,iP+nP ! insert defaults
        tObj(k)%iTypO=1
        tObj(k)%nInhibited=0
        tObj(k)%iCol=1
        tObj(k)%iColMin=30
        tObj(k)%iColMax=90
        tObj(k)%iPar(1:2)=1
        tObj(k)%iPar(3:5)=0
        tObj(k)%GrfRes=0.1d0
        tObj(k)%Par(1)=0.0d0
        tObj(k)%Par(2)=1.0d0
        tObj(k)%Par(3)=0.0d0
        tObj(k)%Par(4)=1.0d0
        tObj(k)%Par(5)=0.0d0
        tObj(k)%O(1:3)=0.0d0
        tObj(k)%e(1:3)=0.0d0
        tObj(k)%e(1)=1.0d0
        tObj(k)%Plane(1:3,0:3)=0.0d0
        tObj(k)%Plane(1,1)=1.0d0
        tObj(k)%Plane(2,2)=1.0d0
        tObj(k)%Plane(3,3)=1.0d0
      end do
      nObj=nObj+nP
      if(kObj.gt.nObj) kObj=nObj
      if(Allocated(tObjAux)) DeAllocate(tObjAux)
    end if
    ldum=.true.
	end Subroutine InsertObj

  Subroutine InsertParticle2D(iP,nP,ldum)
    Implicit none
    Integer(4), Intent(in) :: iP,nP
    Integer(4) k,idum
    Logical ldum
    ldum=.false.
    if(Allocated(tParticle2DAux)) DeAllocate(tParticle2DAux)
    if(nP.lt.0) then ! delete Particle iP up to iP+(-nP-1)
      if((iP.lt.1).or.(iP.gt.nParticle2D).or.(nParticle2D.lt.1)) return
      if(-nP.ge.nParticle2D) then
        if(Allocated(tParticle2D)) DeAllocate(tParticle2D)
        nParticle2D=0_4
      else
        Allocate(tParticle2DAux(nParticle2D+nP),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tParticle2DAux failed!'C,'Insert Particle'C, &
                            MB$OK.or.MB$IconExclamation)
          if(Allocated(tParticle2DAux)) DeAllocate(tParticle2DAux)
          return
        end if
        if(iP.eq.1) then
          tParticle2DAux(iP:nParticle2D+nP)=tParticle2D(iP-nP:nParticle2D)
        else if(iP.ge.nParticle2D+nP+1) then
          tParticle2DAux(1:nParticle2D+nP)=tParticle2D(1:nParticle2D+nP)
        else
          tParticle2DAux(1:iP-1)=tParticle2D(1:iP-1)
          tParticle2DAux(iP:nParticle2D+nP)=tParticle2D(iP-nP:nParticle2D)
        end if
        nParticle2D=nParticle2D+nP
        if(Allocated(tParticle2D)) DeAllocate(tParticle2D)
        Allocate(tParticle2D(nParticle2D),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tParticle2D failed!'C,'Insert Particle'C, &
                            MB$OK.or.MB$IconExclamation)
          if(Allocated(tParticle2D)) DeAllocate(tParticle2D)
          nParticle2D=0_4
        end if
      end if
    else             ! insert nP Particles
      if((iP.lt.0).or.(iP.gt.nParticle2D).or.(nP.le.0)) return
      if(nParticle2D.gt.0) then
        Allocate(tParticle2DAux(nParticle2D),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tParticle2DAux failed!'C,'Insert Particle'C, &
                            MB$OK.or.MB$IconExclamation)
          if(Allocated(tParticle2DAux)) DeAllocate(tParticle2DAux)
          return
        end if
        tParticle2DAux=tParticle2D
      end if
      if(Allocated(tParticle2D)) DeAllocate(tParticle2D)
      Allocate(tParticle2D(nParticle2D+nP),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for tParticle2D failed!'C,'Insert Particle'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(tParticle2DAux)) DeAllocate(tParticle2DAux)
        if(Allocated(tParticle2D)) DeAllocate(tParticle2D)
        nParticle2D=0_4
        return
      end if
      if(nParticle2D.gt.0) then
        tParticle2D(1:nParticle2D)=tParticle2DAux(1:nParticle2D)
        if((iP+1).le.nParticle2D) tParticle2D(iP+1+nP:nParticle2D+nP)=tParticle2DAux(iP+1:nParticle2D)
      end if
      do k=iP+1,iP+nP ! insert defaults
        tParticle2D(k)%iBnd=1_2
        tParticle2D(k)%iColPart=tBnd(tParticle2D(k)%iBnd)%iCol
        tParticle2D(k)%iColMirror=-1_2
        tParticle2D(k)%iColSurface=-1_2
        tParticle2D(k)%sMass=1.0d0
        tParticle2D(k)%Mass=0.0d0
        tParticle2D(k)%r=0.0d0
        tParticle2D(k)%Position(1:2)=0.0d0
        tParticle2D(k)%Velocity(1:2)=0.0d0
        tParticle2D(k)%Acceleration(1:2)=0.0d0
        tParticle2D(k)%Force(1:2)=0.0d0
        tParticle2D(k)%Friction(1:2)=0.0d0
        tParticle2D(k)%RandomForce=0.0d0
      end do
      nParticle2D=nParticle2D+nP
    end if
    if(Allocated(tParticle2DAux)) DeAllocate(tParticle2DAux)
    ldum=.true.
	end Subroutine InsertParticle2D

  Subroutine InsertParticle3D(iP,nP,ldum)
    Implicit none
    Integer(4), Intent(in) :: iP,nP
    Integer(4) k,idum
    Logical ldum
    ldum=.false.
    if(Allocated(tParticle3DAux)) DeAllocate(tParticle3DAux)
    if(nP.lt.0) then ! delete Particle iP up to iP+(-nP-1)
      if((iP.lt.1).or.(iP.gt.nParticle3D).or.(nParticle3D.lt.1)) return
      if(-nP.ge.nParticle3D) then
        if(Allocated(tParticle3D)) DeAllocate(tParticle3D)
        nParticle3D=0_4
      else
        Allocate(tParticle3DAux(nParticle3D+nP),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tParticle3DAux failed!'C,'Insert Particle'C, &
                            MB$OK.or.MB$IconExclamation)
          if(Allocated(tParticle3DAux)) DeAllocate(tParticle3DAux)
          return
        end if
        if(iP.eq.1) then
          tParticle3DAux(iP:nParticle3D+nP)=tParticle3D(iP-nP:nParticle3D)
        else if(iP.ge.nParticle3D+nP+1) then
          tParticle3DAux(1:nParticle3D+nP)=tParticle3D(1:nParticle3D+nP)
        else
          tParticle3DAux(1:iP-1)=tParticle3D(1:iP-1)
          tParticle3DAux(iP:nParticle3D+nP)=tParticle3D(iP-nP:nParticle3D)
        end if
        nParticle3D=nParticle3D+nP
        if(Allocated(tParticle3D)) DeAllocate(tParticle3D)
        Allocate(tParticle3D(nParticle3D),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tParticle3D failed!'C,'Insert Particle'C, &
                            MB$OK.or.MB$IconExclamation)
          if(Allocated(tParticle3D)) DeAllocate(tParticle3D)
          nParticle3D=0_4
        end if
      end if
    else             ! insert nP Particles
      if((iP.lt.0).or.(iP.gt.nParticle3D).or.(nP.le.0)) return
      if(nParticle3D.gt.0) then
        Allocate(tParticle3DAux(nParticle3D),stat=idum)
        if(idum.ne.0) then
          idum=MessageBoxQQ('Memory allocation for tParticle3DAux failed!'C,'Insert Particle'C, &
                            MB$OK.or.MB$IconExclamation)
            if(Allocated(tParticle3DAux)) DeAllocate(tParticle3DAux)
          return
        end if
        tParticle3DAux=tParticle3D
      end if
      if(Allocated(tParticle3D)) DeAllocate(tParticle3D)
      Allocate(tParticle3D(nParticle3D+nP),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for tParticle3D failed!'C,'Insert Particle'C, &
                          MB$OK.or.MB$IconExclamation)
        if(Allocated(tParticle3DAux)) DeAllocate(tParticle3DAux)
        if(Allocated(tParticle3D)) DeAllocate(tParticle3D)
        nParticle3D=0_4
        return
      end if
      if(nParticle3D.gt.0) then
        tParticle3D(1:nParticle3D)=tParticle3DAux(1:nParticle3D)
        if((iP+1).le.nParticle3D) tParticle3D(iP+1+nP:nParticle3D+nP)=tParticle3DAux(iP+1:nParticle3D)
      end if
      do k=iP+1,iP+nP ! insert defaults
        tParticle3D(k)%iObj=1_2
        tParticle3D(k)%iColPart=1_2
        tParticle3D(k)%iColMirror=-1_2
        tParticle3D(k)%iColSurface=-1_2
        tParticle3D(k)%sMass=1.0d0
        tParticle3D(k)%Mass=0.0d0
        tParticle3D(k)%r=0.0d0
        tParticle3D(k)%Position(1:3)=0.0d0
        tParticle3D(k)%Velocity(1:3)=0.0d0
        tParticle3D(k)%Acceleration(1:3)=0.0d0
        tParticle3D(k)%Force(1:3)=0.0d0
        tParticle3D(k)%Friction(1:2)=0.0d0
        tParticle3D(k)%RandomForce=0.0d0
      end do
      nParticle3D=nParticle3D+nP
    end if
    if(Allocated(tParticle3DAux)) DeAllocate(tParticle3DAux)
    ldum=.true.
	end Subroutine InsertParticle3D

  Subroutine AllocateGrd(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(rGrd)) DeAllocate(rGrd)
    nxcFld=max(nxcFld,1)
    nycFld=max(nycFld,1)
    nzcFld=max(nzcFld,1)
    if(.not.lrGrd) then
      Allocate(rGrd(3,nxcFld,nycFld,nzcFld),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for rGrd failed!'C,'Allocate grid'C, &
                          MB$OK.or.MB$IconExclamation)
        nzcFld=0
        return
      end if
    end if
    ldum=.true.
	end Subroutine AllocateGrd

  Subroutine AllocateFld(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    if(Allocated(cFld)) DeAllocate(cFld)
    if(Allocated(dFld)) DeAllocate(dFld)
	  ncFld=GetncFld()
    if(ncFld.lt.1) then
      idum=MessageBoxQQ('No field defined!'C,'Allocate field'C, &
                        MB$OK.or.MB$IconExclamation)
      nzcFld=0
      return
    end if
    ncFld=max(ncFld,1)
    nxcFld=max(nxcFld,1)
    nycFld=max(nycFld,1)
    nzcFld=max(nzcFld,1)
    if(lfcFld) then
      lfcFldAll=.true.
      Allocate(cFld(ncFld,nxcFld,nycFld,nzcFld),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for cFld failed!'C,'Allocate field'C, &
                          MB$OK.or.MB$IconExclamation)
        nzcFld=0
        return
      end if
      cFld=(0.0d0,0.0d0)
    else
      lfcFldAll=.false.
      Allocate(dFld(ncFld,nxcFld,nycFld,nzcFld),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for dFld failed!'C,'Allocate field'C, &
                          MB$OK.or.MB$IconExclamation)
        nzcFld=0
        return
      end if
      dFld=0.0d0
    end if
    call AllocateAuxFld(ldum)
    call SetIDFld()
	end Subroutine AllocateFld

  Subroutine AllocateAuxFld(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    lfcFld=lfcFldAll
    if(Allocated(cAuxFld)) DeAllocate(cAuxFld)
    if(Allocated(dAuxFld)) DeAllocate(dAuxFld)
    if(ncFld.lt.1) then
      idum=MessageBoxQQ('No field defined!'C,'Allocate auxiliary field'C, &
                        MB$OK.or.MB$IconExclamation)
      nzcFld=0
      return
    end if
    ncFld=max(ncFld,1)
    nxcFld=max(nxcFld,1)
    nycFld=max(nycFld,1)
    nzcFld=max(nzcFld,1)
    if(lfcFld) then
      Allocate(cAuxFld(ncFld,nxcFld,nycFld,nzcFld),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for cAuxFld failed!'C,'Allocate auxiliary field'C, &
                          MB$OK.or.MB$IconExclamation)
        nzcFld=0
        return
      end if
    else
      Allocate(dAuxFld(ncFld,nxcFld,nycFld,nzcFld),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for dAuxFld failed!'C,'Allocate auxiliary field'C, &
                          MB$OK.or.MB$IconExclamation)
        nzcFld=0
        return
      end if
    end if
    ldum=.true.
	end Subroutine AllocateAuxFld

  Subroutine AllocateIFld(ldum)
    Logical ldum
    Integer(4) idum
    ldum=.false.
    if(Allocated(iFld)) DeAllocate(iFld)
    nxcFld=max(nxcFld,1)
    nycFld=max(nycFld,1)
    nzcFld=max(nzcFld,1)
    Allocate(iFld(nxcFld,nycFld,nzcFld),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for iFld failed!'C,'Allocate Integer(4) field'C, &
                        MB$OK.or.MB$IconExclamation)
      nzcFld=0
      return
    end if
    iFld=1_2
    ldum=.true.
	end Subroutine AllocateIFld

! Auxiliary

  Function GetncFld()
    Implicit none
    Integer(4) GetncFld,i1
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
    GetncFld=i1
  end Function GetncFld

  Subroutine SetIDFld()
! positions of the field components in the array
    Implicit none
    Integer(4) i1
	  iEx=0
	  iEy=0
	  iEz=0
	  iHx=0
	  iHy=0
	  iHz=0
	  iAx=0
	  iAy=0
	  iAz=0
	  iV=0
		i1=0
    if(lxcFld.and.lEcFld) then
		  i1=i1+1
		  iEx=i1
    end if
    if(lycFld.and.lEcFld) then
		  i1=i1+1
		  iEy=i1
    end if
    if(lzcFld.and.lEcFld) then
		  i1=i1+1
		  iEz=i1
    end if
    if(lxcFld.and.lHcFld) then
		  i1=i1+1
		  iHx=i1
    end if
    if(lycFld.and.lHcFld) then
		  i1=i1+1
		  iHy=i1
    end if
    if(lzcFld.and.lHcFld) then
		  i1=i1+1
		  iHz=i1
    end if
    if(lxcFld.and.lAcFld) then
		  i1=i1+1
		  iAx=i1
    end if
    if(lycFld.and.lAcFld) then
		  i1=i1+1
		  iAy=i1
    end if
    if(lzcFld.and.lAcFld) then
		  i1=i1+1
		  iAz=i1
    end if
    if(lVcFld) then
		  i1=i1+1
		  iV=i1
    end if
    idFld(1)=Int4(iEx)
    idFld(2)=Int4(iEy)
    idFld(3)=Int4(iEz)
    idFld(4)=Int4(iHx)
    idFld(5)=Int4(iHy)
    idFld(6)=Int4(iHz)
    idFld(7)=Int4(iAx)
    idFld(8)=Int4(iAy)
    idFld(9)=Int4(iAz)
    idFld(10)=Int4(iV)
  end Subroutine SetIDFld

  Real(8) Function Wlength(iD)
    Implicit none
    Integer(2) iD
    Wlength=abs(1.0/real(kw0*fcFld*cdsqrt(eDom(iD))*cdsqrt(uDom(iD))))
  end Function Wlength

  Complex(8) Function Wnumber(iD)
    Implicit none
    Integer(2) iD
    Wnumber=(2.0d0*Pi*kw0)*fcFld*cdsqrt(eDom(iD))*cdsqrt(uDom(iD))
  end Function Wnumber

  Complex(8) Function Wimpedance(iD)
    Implicit none
    Integer(2) iD
    Wimpedance=Zw0*cdsqrt(uDom(iD))/cdsqrt(eDom(iD))
  end Function Wimpedance
    
  Subroutine AllocatePFDsource(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
    nPFDsource=max(1,nPFDsource)
    Allocate(PFDsourceA(nPFDsource),iPFDs(nPFDsource),jPFDs(nPFDsource), &
    & kPFDs(nPFDsource),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for PFD sources failed!'C,'Allocate PFD'C, &
                        MB$OK.or.MB$IconExclamation)
      kPFDsource=1
      nPFDsource=1
      DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
      Allocate(PFDsourceA(nPFDsource),iPFDs(nPFDsource),jPFDs(nPFDsource), &
      & kPFDs(nPFDsource),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Serious error:Memory allocation for PFD sources failed!'C,'Allocate PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        kPFDsens=0
        nPFDsens=0
        return
      end if
    end if
    PFDsourceA(1:nPFDsource)=(1.0d0,0.0d0)
    iPFDs(1:nPFDsource)=(nPFDi+1)/2
    jPFDs(1:nPFDsource)=(nPFDj+1)/2
    kPFDs(1:nPFDsource)=(nPFDk+1)/2
    ldum=.true.
	end Subroutine AllocatePFDsource

  Subroutine InsertPFDsource(iP,nP,ldum)
    Implicit none
    Integer(4), Intent(in) :: iP,nP
    Integer(4) k,idum,m
    Logical ldum
    ldum=.false.
    DeAllocate(PFDsourceAa,iPFDsa,jPFDsa,kPFDsa,stat=idum)
    m=max(1,nPFDsource+nP)
    nPFDf=max(1,nPFDf)
    if(nP.lt.0) then ! delete sources iP up to iP+(-nP-1), keep at least 1 source
      if((iP.lt.1).or.(iP.gt.nPFDsource).or.(nPFDsource.lt.1)) return
      do k=1,-nP
        if(iP.le.(nPFDsource-1)) then
          PFDsourceA(iP:nPFDsource-1)=PFDsourceA(iP+1:nPFDsource)
          iPFDs(iP:nPFDsource-1)=iPFDs(iP+1:nPFDsource)
          jPFDs(iP:nPFDsource-1)=jPFDs(iP+1:nPFDsource)
          kPFDs(iP:nPFDsource-1)=kPFDs(iP+1:nPFDsource)
        end if
        nPFDsource=nPFDsource-1
        if(nPFDsource.lt.2) Exit
      end do
      nPFDsource=max(1,nPFDsource)
      Allocate(PFDsourceAa(nPFDsource),iPFDsa(nPFDsource),jPFDsa(nPFDsource), &
      & kPFDsa(nPFDsource),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sources failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsourceAa,iPFDsa,jPFDsa,kPFDsa,stat=idum)
        DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
        nPFDsource=1
        kPFDsource=1
        call AllocatePFDsource(ldum)
        return
      end if
      PFDsourceAa(1:nPFDsource)=PFDsourceA(1:nPFDsource)
      iPFDsa(1:nPFDsource)=iPFDs(1:nPFDsource)
      jPFDsa(1:nPFDsource)=jPFDs(1:nPFDsource)
      kPFDsa(1:nPFDsource)=kPFDs(1:nPFDsource)
      DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
      Allocate(PFDsourceA(nPFDsource),iPFDs(nPFDsource),jPFDs(nPFDsource), &
      & kPFDs(nPFDsource),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sources failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsourceAa,iPFDsa,jPFDsa,kPFDsa,stat=idum)
        DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
        nPFDsource=1
        kPFDsource=1
        call AllocatePFDsource(ldum)
        return
      end if
      PFDsourceA(1:nPFDsource)=PFDsourceAa(1:nPFDsource)
      iPFDs(1:nPFDsource)=iPFDsa(1:nPFDsource)
      jPFDs(1:nPFDsource)=jPFDsa(1:nPFDsource)
      kPFDs(1:nPFDsource)=kPFDsa(1:nPFDsource)
    else ! insert nP Objects
      if((iP.lt.0).or.(iP.gt.nPFDsource).or.(nP.eq.0)) return
      Allocate(PFDsourceAa(nPFDsource),iPFDsa(nPFDsource),jPFDsa(nPFDsource), &
      & kPFDsa(nPFDsource),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sources failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsourceAa,iPFDsa,jPFDsa,kPFDsa,stat=idum)
        nPFDsource=1
        kPFDsource=1
        call AllocatePFDsource(ldum)
        return
      end if
      PFDsourceAa(1:nPFDsource)=PFDsourceA(1:nPFDsource)
      iPFDsa(1:nPFDsource)=iPFDs(1:nPFDsource)
      jPFDsa(1:nPFDsource)=jPFDs(1:nPFDsource)
      kPFDsa(1:nPFDsource)=kPFDs(1:nPFDsource)
      DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
      Allocate(PFDsourceA(m),iPFDs(m),jPFDs(m),kPFDs(m),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sources failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsourceAa,iPFDsa,jPFDsa,kPFDsa,stat=idum)
        DeAllocate(PFDsourceA,iPFDs,jPFDs,kPFDs,stat=idum)
        nPFDsource=1
        kPFDsource=1
        call AllocatePFDsource(ldum)
        return
      end if
      PFDsourceA(1:iP)=PFDsourceAa(1:iP)
      iPFDs(1:iP)=iPFDsa(1:iP)
      jPFDs(1:iP)=jPFDsa(1:iP)
      kPFDs(1:iP)=kPFDsa(1:iP)
      PFDsourceA(iP+1:iP+nP)=(1.0d0,0.0d0)
      iPFDs(iP+1:iP+nP)=(nPFDi+1)/2
      jPFDs(iP+1:iP+nP)=(nPFDj+1)/2
      kPFDs(iP+1:iP+nP)=(nPFDk+1)/2
      PFDsourceA(iP+nP+1:m)=PFDsourceAa(iP+1:nPFDsource)
      iPFDs(iP+nP+1:m)=iPFDsa(iP+1:nPFDsource)
      jPFDs(iP+nP+1:m)=jPFDsa(iP+1:nPFDsource)
      kPFDs(iP+nP+1:m)=kPFDsa(iP+1:nPFDsource)
      nPFDsource=m
      kPFDsource=iP+1
    end if
    kPFDsource=max(1,min(kPFDsource,nPFDsource))
    DeAllocate(PFDsourceAa,iPFDsa,jPFDsa,kPFDsa,stat=idum)
    ldum=.true.
	end Subroutine InsertPFDsource
    
  Subroutine AllocatePFDsens(ldum)
    Implicit none
    Integer(4) idum
    Logical ldum
    ldum=.false.
    DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
    nPFDf=max(1,nPFDf)
    nPFDsens=max(1,nPFDsens)
    Allocate(PFDsensX(nPFDsens),PFDsensY(nPFDsens),PFDsensZ(nPFDsens),PFDsensT(nPFDsens),PFDsensD(nPFDsens), &
    & cPFDsens(0:6,nPFDf,nPFDsens),dPFDsens(6,nPFDsens),nPFDsFld(nPFDsens),stat=idum)
    if(idum.ne.0) then
      idum=MessageBoxQQ('Memory allocation for PFD sensors failed!'C,'Allocate PFD'C, &
                        MB$OK.or.MB$IconExclamation)
      kPFDsens=1
      nPFDsens=1
      DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
      Allocate(PFDsensX(nPFDsens),PFDsensY(nPFDsens),PFDsensZ(nPFDsens),PFDsensT(nPFDsens),PFDsensD(nPFDsens), &
      & cPFDsens(0:6,nPFDf,nPFDsens),dPFDsens(6,nPFDsens),nPFDsFld(nPFDsens),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sensors failed!'C,'Allocate PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        kPFDsens=0
        nPFDsens=0
        return
      end if
    end if
    PFDsensX(1:nPFDsens)=0.0d0
    PFDsensY(1:nPFDsens)=0.0d0
    PFDsensZ(1:nPFDsens)=0.0d0
    PFDsensT(1:nPFDsens)=0.0d0
    PFDsensD(1:nPFDsens)=1.0d100
    nPFDsFld(1:nPFDsens)=0
    dPFDsens(1:6,1:nPFDsens)=0.0d0
    cPFDsens(0:6,1:nPFDf,1:nPFDsens)=(0.0d0,0.0d0)
    ldum=.true.
	end Subroutine AllocatePFDsens

  Subroutine InsertPFDsens(iP,nP,ldum)
    Implicit none
    Integer(4), Intent(in) :: iP,nP
    Integer(4) k,idum,m
    Logical ldum
    ldum=.false.
    DeAllocate(PFDsensXa,PFDsensYa,PFDsensZa,PFDsensTa,PFDsensDa,nPFDsFlda,cPFDsensa,dPFDsensa,stat=idum)
    m=max(1,nPFDsens+nP)
    nPFDf=max(1,nPFDf)
    if(nP.lt.0) then ! delete sensors iP up to iP+(-nP-1), keep at least 1 sensor
      if((iP.lt.1).or.(iP.gt.nPFDsens).or.(nPFDsens.lt.1)) return
      do k=1,-nP
        if(iP.le.(nPFDsens-1)) then
          PFDsensX(iP:nPFDsens-1)=PFDsensX(iP+1:nPFDsens)
          PFDsensY(iP:nPFDsens-1)=PFDsensY(iP+1:nPFDsens)
          PFDsensZ(iP:nPFDsens-1)=PFDsensZ(iP+1:nPFDsens)
          PFDsensT(iP:nPFDsens-1)=PFDsensT(iP+1:nPFDsens)
          PFDsensD(iP:nPFDsens-1)=PFDsensD(iP+1:nPFDsens)
          nPFDsFld(iP:nPFDsens-1)=nPFDsFld(iP+1:nPFDsens)
          dPFDsens(1:6,iP:nPFDsens-1)=dPFDsens(1:6,iP+1:nPFDsens)
          cPFDsens(1:6,1:nPFDf,iP:nPFDsens-1)=cPFDsens(1:6,1:nPFDf,iP+1:nPFDsens)
        end if
        nPFDsens=nPFDsens-1
        if(nPFDsens.lt.2) Exit
      end do
      nPFDsens=max(1,nPFDsens)
      Allocate(PFDsensXa(nPFDsens),PFDsensYa(nPFDsens),PFDsensZa(nPFDsens),PFDsensTa(nPFDsens),PFDsensDa(nPFDsens), &
      & cPFDsensa(6,nPFDf,nPFDsens),dPFDsensa(6,nPFDsens),nPFDsFlda(nPFDsens),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sensors failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsensXa,PFDsensYa,PFDsensZa,PFDsensTa,PFDsensDa,nPFDsFlda,cPFDsensa,dPFDsensa,stat=idum)
        DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
        nPFDsens=1
        kPFDsens=1
        call AllocatePFDsens(ldum)
        return
      end if
      PFDsensXa(1:nPFDsens)=PFDsensX(1:nPFDsens)
      PFDsensYa(1:nPFDsens)=PFDsensY(1:nPFDsens)
      PFDsensZa(1:nPFDsens)=PFDsensZ(1:nPFDsens)
      PFDsensTa(1:nPFDsens)=PFDsensT(1:nPFDsens)
      PFDsensDa(1:nPFDsens)=PFDsensD(1:nPFDsens)
      nPFDsFlda(1:nPFDsens)=nPFDsFld(1:nPFDsens)
      dPFDsensa(1:6,1:nPFDsens)=dPFDsens(1:6,1:nPFDsens)
      cPFDsensa(1:6,1:nPFDf,1:nPFDsens)=cPFDsens(1:6,1:nPFDf,1:nPFDsens)
      DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
      Allocate(PFDsensX(nPFDsens),PFDsensY(nPFDsens),PFDsensZ(nPFDsens),PFDsensT(nPFDsens),PFDsensD(nPFDsens), &
      & cPFDsens(0:6,nPFDf,nPFDsens),dPFDsens(6,nPFDsens),nPFDsFld(nPFDsens),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sensors failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsensXa,PFDsensYa,PFDsensZa,PFDsensTa,PFDsensDa,nPFDsFlda,cPFDsensa,dPFDsensa,stat=idum)
        DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
        nPFDsens=1
        kPFDsens=1
        call AllocatePFDsens(ldum)
        return
      end if
      PFDsensX(1:nPFDsens)=PFDsensXa(1:nPFDsens)
      PFDsensY(1:nPFDsens)=PFDsensYa(1:nPFDsens)
      PFDsensZ(1:nPFDsens)=PFDsensZa(1:nPFDsens)
      PFDsensT(1:nPFDsens)=PFDsensTa(1:nPFDsens)
      PFDsensD(1:nPFDsens)=PFDsensDa(1:nPFDsens)
      nPFDsFld(1:nPFDsens)=nPFDsFlda(1:nPFDsens)
      dPFDsens(1:6,1:nPFDsens)=dPFDsensa(1:6,1:nPFDsens)
      cPFDsens(1:6,1:nPFDf,1:nPFDsens)=cPFDsensa(1:6,1:nPFDf,1:nPFDsens)
    else ! insert nP Objects
      if((iP.lt.0).or.(iP.gt.nPFDsens).or.(nP.eq.0)) return
      Allocate(PFDsensXa(nPFDsens),PFDsensYa(nPFDsens),PFDsensZa(nPFDsens),PFDsensTa(nPFDsens),PFDsensDa(nPFDsens), &
      & cPFDsensa(6,nPFDf,nPFDsens),dPFDsensa(6,nPFDsens),nPFDsFlda(nPFDsens),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sensors failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsensXa,PFDsensYa,PFDsensZa,PFDsensTa,PFDsensDa,nPFDsFlda,cPFDsensa,dPFDsensa,stat=idum)
        return
      end if
      PFDsensXa(1:nPFDsens)=PFDsensX(1:nPFDsens)
      PFDsensYa(1:nPFDsens)=PFDsensY(1:nPFDsens)
      PFDsensZa(1:nPFDsens)=PFDsensZ(1:nPFDsens)
      PFDsensTa(1:nPFDsens)=PFDsensT(1:nPFDsens)
      PFDsensDa(1:nPFDsens)=PFDsensD(1:nPFDsens)
      nPFDsFlda(1:nPFDsens)=nPFDsFld(1:nPFDsens)
      dPFDsensa(1:6,1:nPFDsens)=dPFDsens(1:6,1:nPFDsens)
      cPFDsensa(1:6,1:nPFDf,1:nPFDsens)=cPFDsens(1:6,1:nPFDf,1:nPFDsens)
      DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
      Allocate(PFDsensX(m),PFDsensY(m),PFDsensZ(m),PFDsensT(m),PFDsensD(m),cPFDsens(0:6,nPFDf,m),dPFDsens(6,m), &
      & nPFDsFld(m),stat=idum)
      if(idum.ne.0) then
        idum=MessageBoxQQ('Memory allocation for PFD sensors failed!'C,'Insert PFD'C, &
                          MB$OK.or.MB$IconExclamation)
        DeAllocate(PFDsensXa,PFDsensYa,PFDsensZa,PFDsensTa,PFDsensDa,nPFDsFlda,cPFDsensa,dPFDsensa,stat=idum)
        DeAllocate(PFDsensX,PFDsensY,PFDsensZ,PFDsensT,PFDsensD,nPFDsFld,cPFDsens,dPFDsens,stat=idum)
        nPFDsens=1
        kPFDsens=1
        call AllocatePFDsens(ldum)
        return
      end if
      PFDsensX(1:iP)=PFDsensXa(1:iP)
      PFDsensY(1:iP)=PFDsensYa(1:iP)
      PFDsensZ(1:iP)=PFDsensZa(1:iP)
      PFDsensT(1:iP)=PFDsensTa(1:iP)
      PFDsensD(1:iP)=PFDsensDa(1:iP)
      nPFDsFld(1:iP)=nPFDsFlda(1:iP)
      dPFDsens(1:6,1:iP)=dPFDsensa(1:6,1:iP)
      cPFDsens(1:6,1:nPFDf,1:iP)=cPFDsensa(1:6,1:nPFDf,1:iP)
      PFDsensX(iP+1:iP+nP)=0.0d0
      PFDsensY(iP+1:iP+nP)=0.0d0
      PFDsensZ(iP+1:iP+nP)=0.0d0
      PFDsensT(iP+1:iP+nP)=0.0d0
      PFDsensD(iP+1:iP+nP)=1.0d100
      nPFDsFld(iP+1:iP+nP)=0
      dPFDsens(1:6,iP+1:iP+nP)=(0.0d0,0.0d0)
      cPFDsens(0:6,1:nPFDf,iP+1:iP+nP)=(0.0d0,0.0d0)
      PFDsensX(iP+nP+1:m)=PFDsensXa(iP+1:nPFDsens)
      PFDsensY(iP+nP+1:m)=PFDsensYa(iP+1:nPFDsens)
      PFDsensZ(iP+nP+1:m)=PFDsensZa(iP+1:nPFDsens)
      PFDsensT(iP+nP+1:m)=PFDsensTa(iP+1:nPFDsens)
      PFDsensD(iP+nP+1:m)=PFDsensDa(iP+1:nPFDsens)
      nPFDsFld(iP+nP+1:m)=nPFDsFlda(iP+1:nPFDsens)
      dPFDsens(1:6,iP+nP+1:m)=dPFDsensa(1:6,iP+1:nPFDsens)
      cPFDsens(1:6,1:nPFDf,iP+nP+1:m)=cPFDsensa(1:6,1:nPFDf,iP+1:nPFDsens)
      nPFDsens=m
      kPFDsens=iP+1
    end if
    kPFDsens=max(1,min(kPFDsens,nPFDsens))
    DeAllocate(PFDsensXa,PFDsensYa,PFDsensZa,PFDsensTa,PFDsensDa,nPFDsFlda,cPFDsensa,dPFDsensa,stat=idum)
    ldum=.true.
	end Subroutine InsertPFDsens

END MODULE CHDAT




