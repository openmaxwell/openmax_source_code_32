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
MODULE CHoGL
use CHMOV
use opengl_gl
use opengl_glu
use opengl_glut
implicit none

private
public :: CHGLreset,ChGLviewInit,ChGLviewReset,ChGLdisplay,CHGLColorPalette,CHGLDrawAxes,CHGLDrawSurfaces,CHGLDrawGrids, &
   & CHGLDrawIsoLines,CHGLDrawVectors,CHGLDrawExpansions,CHGLDrawTubes,CHGLDrawPFDsensors,CHGLDrawPFDsources,CHGLclear, &
   & CHGLDefaults,CHGLwindowDialog,CHGLDraw,ChGLsaveBMP
Integer(2), public :: iCHGLmode

private :: mouse,motion,arrows,menu_handler,set_left_button,set_middle_button,set_arrow_keys, &
   & sphere2cart,cart2sphere,cart3D_plus_cart3D,cart3D_minus_cart3D, &
   & CHGLDrawVector,DrawIsoIn3,SetVertex,r3Vec_Prod,r3Vec_Length,Unit3DV,Ortho3DSpace, &
   & SetCHGLwindowData,GetCHGLwindowData,updateCHGLwindow,CHGLMovie, &
   & CHGLxscale_factor,CHGLyscale_factor,CHGLzscale_factor, &
   & ZOOM,PANXY,ROTATE,SCALEX,SCALEY,SCALEZ,ShowAxes,ShowSurface,ShowVectors,RESET,DRAW,MOVIE,BITMAP,PI,&
   & left_button_func,middle_button_func,arrow_key_func,&
   & angle,shift,xscale_factor,yscale_factor,moving_left,moving_middle,begin_left,begin_middle

type,private :: cart2D ! 2D cartesian coordinates
  real(kind=gldouble) :: x,y
end type cart2D
type,private :: cart3D ! 3D cartesian coordinates
  real(kind=gldouble) :: x,y,z
end type cart3D
type,private :: sphere3D ! 3D spherical coordinates
  real(kind=gldouble) :: theta,phi,rho
end type sphere3D

Integer(kind=glcint),parameter :: NOTHING=0,ZOOM=1,PANXY=2,ROTATE=3,SCALEX=4,SCALEY=5,SCALEZ=6,ShowAxes=7, &
      & ShowSurface=8,ShowGrid=9,ShowIso=10,ShowVectors=11,ShowExpansions=12,RESET=13,DRAW=14,MOVIE=15,BITMAP=16, &
      & ShowTubes=17,ShowPointSources=18,ShowXsensors=19
Integer(2), private :: iCR(0:10),iCG(0:10),iCB(0:10)
Real(8), private :: CHGLlookAtCurrent(3),CHGLf

type(cart2D),save :: angle,begin_left,begin_middle
type(cart3D),save :: shift
real(kind=gldouble),save :: xscale_factor,yscale_factor,zscale_factor, &
                CHGLxscale_factor,CHGLyscale_factor,CHGLzscale_factor
logical,save :: moving_left,moving_middle

interface operator(+)
  module procedure cart3D_plus_cart3D
end interface
interface operator(-)
  module procedure cart3D_minus_cart3D
end interface

!initial configuration
integer(4),save :: left_button_func=ROTATE,middle_button_func=PANXY,arrow_key_func=ZOOM

contains

! Initialization

subroutine CHGLDefaults(lini)
  Implicit none
  Real(8) d
  Integer(4) ier,idum
  Logical, intent(in) :: lini
  call setNameExt(ProFileName,'OGL',OGLFileName)
  d=max(abs(WinXmax(kWin)-WinXmin(kWin)),abs(WinYmax(kWin)-WinYmin(kWin)))
  CHGLf=2.0d0*d
  if(lini) then
    rCHGLMovie=10.0d0
    iCHGLMovie=0_2
    iCHGLxAxisColor=2_2
    iCHGLyAxisColor=3_2
    iCHGLzAxisColor=4_2
    iCHGLxGridColor=2_2
    iCHGLyGridColor=3_2
    iCHGLzGridColor=4_2
    CHGLxAxisW=3.0d0
    CHGLyAxisW=3.0d0
    CHGLzAxisW=3.0d0
    CHGLxAxisL=1.5d0*d
    CHGLyAxisL=1.5d0*d
    CHGLzAxisL=1.5d0*d
    CHGLxGridLineW=1.0d0
    CHGLyGridLineW=1.0d0
    CHGLzGridLineW=1.0d0
    CHGLExpLen=0.025d0*d
    space=viewPlane
    CHGLlookAt(1:3)=viewPlane(1:3,0)+0.5d0*(WinXmax(kWin)+WinXmin(kWin))*viewPlane(1:3,1)+ &
    &                                0.5d0*(WinYmax(kWin)+WinYmin(kWin))*viewPlane(1:3,2)
    CHGLlookFrom(1:3)=CHGLlookAt(1:3)+6.0d0*d*viewPlane(1:3,3)
    CHGLlookAtCurrent(1:3)=CHGLlookAt(1:3)
    iCHGLwidth=max(64_2,iWinWidth(kWin))
    iCHGLheight=max(64_2,iWinHeight(kWin))
    iCHGLwidthCurrent=iCHGLwidth
    iCHGLheightCurrent=iCHGLheight
    CHGLViewAngle=10.0d0
    CHGLViewAspect=max(0.1d0,min(10.0d0,Dble(iWinWidth(kWin))/Dble(iWinHeight(kWin))))
    CHGLViewNear=max(pSmall,min(1.0d30,0.1d0*d))
    CHGLViewFar=max(CHGLViewNear,min(1.0d30,1.0d3*d))
    nCHGLTube=1
    kCHGLTube=1
    mCHGLTube=1
    DeAllocate(CHGLTubeStart,CHGLTubeR,CHGLTubeD,iCHGLTubeR,iCHGLTubeD,iCHGLTubeColor,Stat=ier)
    Allocate(CHGLTubeStart(3,nCHGLTube),CHGLTubeR(nCHGLTube),CHGLTubeD(nCHGLTube),iCHGLTubeR(nCHGLTube), &
    & iCHGLTubeD(nCHGLTube),iCHGLTubeColor(3,nCHGLTube),Stat=ier)
    if(ier.ne.0) then
      idum=MessageBoxQQ('Memory allocation error!'C,'Open OpenGL data'C, &
                        MB$OK.or.MB$ICONSTOP)
      mCHGLTube=0
    end if
    CHGLTubeStart(1:3,nCHGLTube)=0.0d0
    CHGLTubeR(nCHGLTube)=0.0d0
    CHGLTubeD(nCHGLTube)=0.0d0
    iCHGLTubeR(nCHGLTube)=0
    iCHGLTubeD(nCHGLTube)=0
    iCHGLTubeColor(1:3,nCHGLTube)=0
  end if
  iCHGLrColorMin=minCIntensity
  iCHGLrColorMax=maxCIntensity
  CHGLIsoMinF=rMinFld
  CHGLIsoMaxF=rMaxFld
  CHGLSurfaceMinF=rMinFld
  CHGLSurfaceMaxF=rMaxFld
  CHGLVectorScale=scaleArrow*ArrowLength/max(dabs(rMinFld),dabs(rMaxFld),1.0d-100)
  CHGLIsoStepF=rIsoStep
  CHGLSurfaceStepF=CHGLIsoStepF
  iCHGLxVectorColor=minCArrow
  iCHGLyVectorColor=maxCArrow
  iCHGLxVectorStep=nsFld
  iCHGLyVectorStep=nsFld
  CHGLVectorMaxLength=ArrowLength    
  CHGLxscale_factor=1.0_gldouble
  CHGLyscale_factor=1.0_gldouble
  CHGLzscale_factor=1.0_gldouble
end subroutine CHGLDefaults

subroutine ChGLviewReset
! resets the view to the current orientation and scale
  Implicit none
  call glMatrixMode(GL_MODELVIEW)
  call glPopMatrix
  call glPushMatrix
  call glTranslated(shift%x,shift%y,shift%z)
  call glRotated(angle%x,0.0_gldouble,0.0_gldouble,1.0_gldouble)
  call glRotated(angle%y,cos(PI*angle%x/180.0_gldouble),&
                 -sin(PI*angle%x/180.0_gldouble),0.0_gldouble)
  call glTranslated(-CHGLlookAtCurrent(1),-CHGLlookAtCurrent(2),-CHGLlookAtCurrent(3))
  call glScaled(xscale_factor,yscale_factor,zscale_factor)
end subroutine ChGLviewReset

function ChGLviewInit() result(menuid)
! initializes the view modifier variables and sets initial view.
! It should be called immediately after glutCreateWindow
  Implicit none
  integer(kind=glcint) :: menuid
  integer(kind=glcint) :: button_left,button_middle,arrow_keys
! set the callback functions
  call glutMouseFunc(mouse)
  call glutMotionFunc(motion)
  call glutSpecialFunc(arrows)
! create the menu
  button_left=glutCreateMenu(set_left_button)
  call glutAddMenuEntry("rotate",ROTATE)
  call glutAddMenuEntry("zoom",ZOOM)
  call glutAddMenuEntry("pan",PANXY)
  call glutAddMenuEntry("scale x",SCALEX)
  call glutAddMenuEntry("scale y",SCALEY)
  call glutAddMenuEntry("scale z",SCALEZ)
  call glutAddMenuEntry("nothing",NOTHING)
  button_middle=glutCreateMenu(set_middle_button)
  call glutAddMenuEntry("rotate",ROTATE)
  call glutAddMenuEntry("zoom",ZOOM)
  call glutAddMenuEntry("pan",PANXY)
  call glutAddMenuEntry("scale x",SCALEX)
  call glutAddMenuEntry("scale y",SCALEY)
  call glutAddMenuEntry("scale z",SCALEZ)
  call glutAddMenuEntry("nothing",NOTHING)
  arrow_keys=glutCreateMenu(set_arrow_keys)
  call glutAddMenuEntry("rotate",ROTATE)
  call glutAddMenuEntry("zoom",ZOOM)
  call glutAddMenuEntry("pan",PANXY)
  call glutAddMenuEntry("scale x",SCALEX)
  call glutAddMenuEntry("scale y",SCALEY)
  call glutAddMenuEntry("scale z",SCALEZ)
  call glutAddMenuEntry("nothing",NOTHING)
  menuid=glutCreateMenu(menu_handler)
  call glutAddMenuEntry("reset to initial view",RESET)
  call glutAddMenuEntry("draw 3D objects",DRAW)
  call glutAddMenuEntry("save bitmap",BITMAP)
  call glutAddMenuEntry("generate movie",MOVIE)
  call glutAddMenuEntry("show/hide axes",ShowAxes)
  call glutAddMenuEntry("show/hide surface",ShowSurface)
  call glutAddMenuEntry("show/hide grid",ShowGrid)
  call glutAddMenuEntry("show/hide iso-lines",ShowIso)
  call glutAddMenuEntry("show/hide vectors",ShowVectors)
  call glutAddMenuEntry("show/hide expansions",ShowExpansions)
  call glutAddMenuEntry("show/hide field-tubes",ShowTubes)
  call glutAddMenuEntry("show/hide PFD sources",ShowPointSources)
  call glutAddMenuEntry("show/hide PFD sensors",ShowXsensors)
  call glutAddSubMenu("left mouse button",button_left)
  call glutAddSubMenu("middle mouse button",button_middle)
  call glutAddSubMenu("arrow keys",arrow_keys)
  call CHGLreset
end function ChGLviewInit

subroutine CHGLclear()
  implicit none
  call glClearColor(1.0_glclampf,1.0_glclampf,1.0_glclampf,0.0_glclampf)
  call glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE)
  call glEnable(GL_LIGHTING)
  call glEnable(GL_DEPTH_TEST)
end subroutine CHGLclear

! Dialogs

Subroutine CHGLwindowDialog(lCheck)
! Dialog for the data of the graphic windows
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
  ldum=DlgInit(IDD_OGL,dlg)
  if(.not.ldum) then
    idum=MessageBoxQQ('Dialog initialization failed!'C,'CHGLwindow dialog'C, &
                      MB$OK.or.MB$ICONSTOP)
    return
  else
    IsOpen=1
    call SetCHGLwindowData(dlg)
    ldum=DlgSetSub(dlg,idok,updateCHGLwindow)
    ldum=DlgSetSub(dlg,idc_ogl_tube,updateCHGLwindow)
    ldum=DlgSetSub(dlg,idc_ogl_read,updateCHGLwindow)
    ldum=DlgSetSub(dlg,idc_ogl_write,updateCHGLwindow)
    ldum=DlgSetSub(dlg,idc_ogl_default,updateCHGLwindow)
  end if
  ldum=DlgModal(dlg)
  call DlgUnInit(dlg)
  IsOpen=0
end Subroutine CHGLwindowDialog

Subroutine SetCHGLwindowData(dlg)
! set dialog boxes containing graphic window data
  Implicit none
  Include 'RESOURCE.FD'
  Type(dialog) dlg
  call DlgSetI(Dlg,idc_OGL_Xcolor,0,Int4(iCHGLxAxisColor),1,236)
  call DlgSetI(Dlg,idc_OGL_Ycolor,0,Int4(iCHGLyAxisColor),1,236)
  call DlgSetI(Dlg,idc_OGL_Zcolor,0,Int4(iCHGLzAxisColor),1,236)
  call DlgSetI(Dlg,idc_OGL_Xcolor2,0,Int4(iCHGLxGridColor),1,236)
  call DlgSetI(Dlg,idc_OGL_Ycolor2,0,Int4(iCHGLyGridColor),1,236)
  call DlgSetI(Dlg,idc_OGL_Zcolor2,0,Int4(iCHGLzGridColor),1,236)
  call DlgSetI(Dlg,idc_OGL_Width,0,Int4(iCHGLwidth),64,10000)
  call DlgSetI(Dlg,idc_OGL_Height,0,Int4(iCHGLheight),64,10000)
  call DlgSetI(Dlg,idc_OGL_iMovie,0,iCHGLMovie,0,1000000)
  call DlgSetR(Dlg,idc_OGL_Xwidth,CHGLxAxisW,2)
  call DlgSetR(Dlg,idc_OGL_Ywidth,CHGLyAxisW,2)
  call DlgSetR(Dlg,idc_OGL_Zwidth,CHGLzAxisW,2)
  call DlgSetR(Dlg,idc_OGL_Xwidth2,CHGLxGridLineW,2)
  call DlgSetR(Dlg,idc_OGL_Ywidth2,CHGLyGridLineW,2)
  call DlgSetR(Dlg,idc_OGL_Zwidth2,CHGLzGridLineW,2)
  call DlgSetR(Dlg,idc_OGL_Xlength,CHGLxAxisL,2)
  call DlgSetR(Dlg,idc_OGL_Ylength,CHGLyAxisL,2)
  call DlgSetR(Dlg,idc_OGL_Zlength,CHGLzAxisL,2)
  call DlgSetR(Dlg,idc_OGL_ExpLen,CHGLExpLen,2)
  call DlgSetR(Dlg,idc_OGL_rMovie,rCHGLMovie,2)
  call DlgSetR(Dlg,idc_OGL_XlookFrom,CHGLlookFrom(1),2)
  call DlgSetR(Dlg,idc_OGL_YlookFrom,CHGLlookFrom(2),2)
  call DlgSetR(Dlg,idc_OGL_ZlookFrom,CHGLlookFrom(3),2)
  call DlgSetR(Dlg,idc_OGL_XlookAt,CHGLlookAt(1),2)
  call DlgSetR(Dlg,idc_OGL_YlookAt,CHGLlookAt(2),2)
  call DlgSetR(Dlg,idc_OGL_ZlookAt,CHGLlookAt(3),2)
  call DlgSetR(Dlg,idc_OGL_ViewAngle,CHGLViewAngle,2)
  call DlgSetR(Dlg,idc_OGL_ViewAspect,CHGLViewAspect,2)
  call DlgSetR(Dlg,idc_OGL_ViewNear,CHGLViewNear,2)
  call DlgSetR(Dlg,idc_OGL_ViewFar,CHGLViewFar,2)
end Subroutine SetCHGLwindowData

Subroutine GetCHGLwindowData(dlg)
! get graphic window data
  Implicit none
  Include 'RESOURCE.FD'
  Integer(4) idum
  Type(dialog) dlg
  call DlgGetI(Dlg,idc_OGL_Xcolor,0,0,idum,0,236,2)
  iCHGLxAxisColor=Int2(idum)
  call DlgGetI(Dlg,idc_OGL_Ycolor,0,0,idum,0,236,2)
  iCHGLyAxisColor=Int2(idum)
  call DlgGetI(Dlg,idc_OGL_Zcolor,0,0,idum,0,236,2)
  iCHGLzAxisColor=Int2(idum)
  call DlgGetI(Dlg,idc_OGL_Xcolor2,0,0,idum,0,236,2)
  iCHGLxGridColor=Int2(idum)
  call DlgGetI(Dlg,idc_OGL_Ycolor2,0,0,idum,0,236,2)
  iCHGLyGridColor=Int2(idum)
  call DlgGetI(Dlg,idc_OGL_Zcolor2,0,0,idum,0,236,2)
  iCHGLzGridColor=Int2(idum)
  call DlgGetI(Dlg,idc_OGL_Width,0,0,idum,64,10000,400)
  iCHGLwidth=max(64_2,Int2(idum))
  call DlgGetI(Dlg,idc_OGL_Height,0,0,idum,64,10000,400)
  iCHGLheight=max(64_2,Int2(idum))
  call DlgGetI(Dlg,idc_OGL_iMovie,0,0,iCHGLMovie,0,1000000,36)
  call DlgGetR(Dlg,idc_OGL_Xwidth,CHGLxAxisW,0.0d0,1.0d3,3.0d0,2)
  call DlgGetR(Dlg,idc_OGL_Ywidth,CHGLyAxisW,0.0d0,1.0d3,3.0d0,2)
  call DlgGetR(Dlg,idc_OGL_Zwidth,CHGLzAxisW,0.0d0,1.0d3,3.0d0,2)
  call DlgGetR(Dlg,idc_OGL_Xwidth2,CHGLxGridLineW,0.0d0,1.0d3,1.0d0,2)
  call DlgGetR(Dlg,idc_OGL_Ywidth2,CHGLyGridLineW,0.0d0,1.0d3,1.0d0,2)
  call DlgGetR(Dlg,idc_OGL_Zwidth2,CHGLzGridLineW,0.0d0,1.0d3,1.0d0,2)
  call DlgGetR(Dlg,idc_OGL_Xlength,CHGLxAxisL,0.0d0,1.0d3,1.5d0,2)
  call DlgGetR(Dlg,idc_OGL_Ylength,CHGLyAxisL,0.0d0,1.0d3,1.5d0,2)
  call DlgGetR(Dlg,idc_OGL_Zlength,CHGLzAxisL,0.0d0,1.0d3,1.5d0,2)
  call DlgGetR(Dlg,idc_OGL_ExpLen,CHGLExpLen,0.0d0,1.0d3,0.05d0,2)
  call DlgGetR(Dlg,idc_OGL_rMovie,rCHGLMovie,nBig,pBig,10.0d0,2)
  call DlgGetR(Dlg,idc_OGL_XlookFrom,CHGLlookFrom(1),-1.0d30,1.0d30,10.0d0,2)
  call DlgGetR(Dlg,idc_OGL_YlookFrom,CHGLlookFrom(2),-1.0d30,1.0d30,10.0d0,2)
  call DlgGetR(Dlg,idc_OGL_ZlookFrom,CHGLlookFrom(3),-1.0d30,1.0d30,10.0d0,2)
  call DlgGetR(Dlg,idc_OGL_XlookAt,CHGLlookAt(1),-1.0d30,1.0d30,0.0d0,2)
  call DlgGetR(Dlg,idc_OGL_YlookAt,CHGLlookAt(2),-1.0d30,1.0d30,0.0d0,2)
  call DlgGetR(Dlg,idc_OGL_ZlookAt,CHGLlookAt(3),-1.0d30,1.0d30,0.0d0,2)
  call DlgGetR(Dlg,idc_OGL_ViewAngle,CHGLViewAngle,1.0d-10,180d0,10.0d0,2)
  call DlgGetR(Dlg,idc_OGL_ViewAspect,CHGLViewAspect,1.0d-1,1.0d1,1.0d0,2)
  call DlgGetR(Dlg,idc_OGL_ViewNear,CHGLViewNear,pSmall,1.0d30,0.01d0,2)
  call DlgGetR(Dlg,idc_OGL_ViewFar,CHGLViewFar,CHGLViewNear,1.0d30,1.0d30,2)
end Subroutine GetCHGLwindowData

Subroutine updateCHGLwindow(dlg,control_name,callbackType)
! callback for CHGLwindowDialog
  Implicit none
  Include 'RESOURCE.FD'
  Integer(4) control_name,callbackType,idum
  Type (dialog) dlg
  idum=callbackType
  Select Case(control_name)
  Case(idok)
    call GetCHGLwindowData(dlg)
    call DlgExit(dlg)
  Case(idc_ogl_tube)
    call TubeDialog(.true.)
  Case(idc_ogl_read)
    call GetCHGLWindowData(dlg)
    call OpenCHGLWindow(.false.)
    call SetCHGLWindowData(dlg)
  Case(idc_ogl_write)
    call GetCHGLWindowData(dlg)
    call SaveCHGLWindow(.false.)
    call SetCHGLWindowData(dlg)
  Case(idc_ogl_default)
    call CHGLDefaults(.true.)
    call SetCHGLWindowData(dlg)
  end Select
end Subroutine updateCHGLwindow

Subroutine TubeDialog(lCheck)
! dialog for the field tube data
  Implicit none
  Include 'RESOURCE.FD'
  Logical, intent(in) :: lCheck
  Logical ldum
  Integer(4) idum
  Type(dialog) dlg
  Integer(4), save:: IsOpen
  Data IsOpen/0/
  if(IsOpen.ne.0) return
  ldum=lCheck
  ldum=DlgInit(IDD_Tube,dlg)
  if(.not.ldum) then
    idum=MessageBoxQQ('Dialog initialization failed!'C,'Tube dialog'C, &
                      MB$OK.or.MB$ICONSTOP)
    return
  else
    IsOpen=1
    call SetTubeData(dlg)
    ldum=DlgSetSub(dlg,idc_kTube,updateTube)
    ldum=DlgSetSub(dlg,idc_kTubeS,updateTube)
    ldum=DlgSetSub(dlg,idc_Modify_Tube,updateTube)
    ldum=DlgSetSub(dlg,idc_Add_Tube,updateTube)
    ldum=DlgSetSub(dlg,idc_Del_Tube,updateTube)
    ldum=DlgSetSub(dlg,idcancel,updateTube)
  end if
  ldum=DlgModal(dlg)
  call DlgUnInit(dlg)
  IsOpen=0
end Subroutine TubeDialog

Subroutine SetTubeData(dlg)
! set the Tube data in its dialog
  Implicit none
  Include 'RESOURCE.FD'
  Type(dialog) dlg
  call DlgSetI(Dlg,idc_kTube,idc_kTubeS,kCHGLTube,1,nCHGLTube)
  call DlgSetI(Dlg,idc_nTube,0,nCHGLTube,1,10000)
  call DlgSetI(Dlg,idc_TubeColorg,0,Int4(iCHGLTubeColor(1,kCHGLTube)),0,235)
  call DlgSetI(Dlg,idc_TubeColori,0,Int4(iCHGLTubeColor(2,kCHGLTube)),0,235)
  call DlgSetI(Dlg,idc_TubeColoro,0,Int4(iCHGLTubeColor(3,kCHGLTube)),0,235)
  call DlgSetI(Dlg,idc_TubeNR,0,iCHGLTubeR(kCHGLTube),0,10000)
  call DlgSetI(Dlg,idc_TubeND,0,iCHGLTubeD(kCHGLTube),0,10000)
  call DlgSetR(Dlg,idc_TubeX,CHGLTubeStart(1,kCHGLTube),7)
  call DlgSetR(Dlg,idc_TubeY,CHGLTubeStart(2,kCHGLTube),7)
  call DlgSetR(Dlg,idc_TubeZ,CHGLTubeStart(3,kCHGLTube),7)
  call DlgSetR(Dlg,idc_TubeR,CHGLTubeR(kCHGLTube),7)
  call DlgSetR(Dlg,idc_TubeD,CHGLTubeD(kCHGLTube),7)
end Subroutine SetTubeData

Subroutine GetTubeData(dlg)
! get the Tube data from its dialog
  Implicit none
  Include 'RESOURCE.FD'
  Integer(4) idum
  Type(dialog) dlg
  call DlgGetI(Dlg,idc_kTube,idc_kTubeS,0,kCHGLTube,1,nCHGLTube,1)
  call DlgGetI(Dlg,idc_TubeColorg,0,0,idum,0,235,1)
  iCHGLTubeColor(1,kCHGLTube)=Int2(idum)
  call DlgGetI(Dlg,idc_TubeColori,0,0,idum,0,235,1)
  iCHGLTubeColor(2,kCHGLTube)=Int2(idum)
  call DlgGetI(Dlg,idc_TubeColoro,0,0,idum,0,235,1)
  iCHGLTubeColor(3,kCHGLTube)=Int2(idum)
  call DlgGetI(Dlg,idc_TubeNR,0,0,iCHGLTubeR(kCHGLTube),0,10000,0)
  call DlgGetI(Dlg,idc_TubeND,0,0,iCHGLTubeD(kCHGLTube),0,10000,0)
  call DlgGetR(Dlg,idc_TubeX,CHGLTubeStart(1,kCHGLTube),nBig,pBig,0.0d0,7)
  call DlgGetR(Dlg,idc_TubeY,CHGLTubeStart(2,kCHGLTube),nBig,pBig,0.0d0,7)
  call DlgGetR(Dlg,idc_TubeZ,CHGLTubeStart(3,kCHGLTube),nBig,pBig,0.0d0,7)
  call DlgGetR(Dlg,idc_TubeR,CHGLTubeR(kCHGLTube),nBig,pBig,0.0d0,7)
  call DlgGetR(Dlg,idc_TubeD,CHGLTubeD(kCHGLTube),nBig,pBig,0.0d0,7)
end Subroutine GetTubeData

Subroutine updateTube(dlg,control_name,callbackType)
! callback for TubeDialog
  Implicit none
  Include 'RESOURCE.FD'
  Integer(4) control_name,callbackType,idum
  Logical ldum
  Type (dialog) dlg
  idum=callbackType
  Select Case(control_name)
  Case(idc_kTube)
    call DlgGetI(Dlg,idc_kTube,idc_kTubeS,0,kCHGLTube,1,nCHGLTube,1)
    call SetTubeData(dlg)
  Case(idc_kTubeS)
    call DlgGetI(Dlg,idc_kTube,idc_kTubeS,1,kCHGLTube,1,nCHGLTube,1)
    call SetTubeData(dlg)
  Case(idc_Modify_Tube)
    call GetTubeData(dlg)
  Case(idc_Add_Tube)
    call InsertTube(kCHGLTube,ldum)
    if(ldum) then
      call DlgSetI(Dlg,idc_nTube,0,nCHGLTube,1,10000)
      call GetTubeData(dlg)
    end if
  Case(idc_Del_Tube)
    call GetTubeData(dlg)
    if(nCHGLTube.gt.1) then
      nCHGLTube=nCHGLTube-1
      do idum=kCHGLTube,nCHGLTube
        CHGLTubeStart(1:3,idum)=CHGLTubeStart(1:3,idum+1)
        CHGLTubeR(idum)=CHGLTubeR(idum+1)
        CHGLTubeD(idum)=CHGLTubeD(idum+1)
        iCHGLTubeR(idum)=iCHGLTubeR(idum+1)
        iCHGLTubeD(idum)=iCHGLTubeD(idum+1)
        iCHGLTubeColor(1:3,idum)=iCHGLTubeColor(1:3,idum+1)
      end do
    end if
    call SetTubeData(dlg)
  Case(idcancel)
    call DlgExit(dlg)
  end Select
end Subroutine updateTube

Subroutine InsertTube(loc,lOK)
  Implicit none
  Real(8), Allocatable:: Start(:,:),R(:),D(:)
  Integer(4), Allocatable:: NR(:),ND(:)
  Integer(2), Allocatable:: IC(:,:)
  Integer(4) loc,idum
  Logical lOK
  lOK=.false.
  DeAllocate(Start,R,D,NR,ND,IC,stat=idum)
  Allocate(Start(3,nCHGLTube+1),R(nCHGLTube+1),D(nCHGLTube+1),NR(nCHGLTube+1),ND(nCHGLTube+1),IC(3,nCHGLTube+1), &
  & stat=idum)
  if(idum.ne.0) then
    idum=MessageBoxQQ('Memory allocation error!'C,'Insert field tube'C, &
                      MB$OK.or.MB$ICONSTOP)
    return
  end if
  do idum=1,loc
    Start(1:3,idum)=CHGLTubeStart(1:3,idum)
    R(idum)=CHGLTubeR(idum)
    D(idum)=CHGLTubeD(idum)
    NR(idum)=iCHGLTubeR(idum)
    ND(idum)=iCHGLTubeD(idum)
    IC(1:3,idum)=iCHGLTubeColor(1:3,idum)
  end do
  do idum=loc,nCHGLTube
    Start(1:3,idum+1)=CHGLTubeStart(1:3,idum)
    R(idum+1)=CHGLTubeR(idum)
    D(idum+1)=CHGLTubeD(idum)
    NR(idum+1)=iCHGLTubeR(idum)
    ND(idum+1)=iCHGLTubeD(idum)
    IC(1:3,idum+1)=iCHGLTubeColor(1:3,idum)
  end do
  DeAllocate(CHGLTubeStart,CHGLTubeR,CHGLTubeD,iCHGLTubeR,iCHGLTubeD,iCHGLTubeColor,Stat=idum)
  nCHGLTube=nCHGLTube+1
  Allocate(CHGLTubeStart(3,nCHGLTube),CHGLTubeR(nCHGLTube),CHGLTubeD(nCHGLTube),iCHGLTubeR(nCHGLTube), &
  & iCHGLTubeD(nCHGLTube),iCHGLTubeColor(3,nCHGLTube),Stat=idum)
  if(idum.ne.0) then
    idum=MessageBoxQQ('Memory allocation error!'C,'Insert field tube'C, &
                      MB$OK.or.MB$ICONSTOP)
    return
  end if
  CHGLTubeStart=Start
  CHGLTubeR=R
  CHGLTubeD=D
  iCHGLTubeR=NR
  iCHGLTubeD=ND
  iCHGLTubeColor=IC
  DeAllocate(Start,R,D,NR,ND,IC,stat=idum)
  lOK=.true.
end Subroutine InsertTube

! public routines

subroutine CHGLreset
! resets the view to the initial configuration
  Implicit none
  Integer(kind=glint) iw,ih
  Real(kind=gldouble) ViewAngle,ViewAspect,ViewNear,ViewFar
  type(Cart3D) :: clookfrom
  type(sphere3D) :: slookfrom
! set the palette
  call CHGLColorPalette()
! set the perspective
  call glMatrixMode(GL_PROJECTION)
  call glLoadIdentity
  ViewAngle=CHGLViewAngle
  ViewAspect=CHGLViewAspect
  ViewNear=CHGLViewNear
  ViewFar=CHGLViewFar
  call gluPerspective(ViewAngle,ViewAspect,ViewNear,ViewFar)
! set the initial view
  call glPushMatrix
  CHGLlookAtCurrent(1:3)=CHGLlookAt(1:3)
  clookfrom%x=CHGLlookFrom(1)-CHGLlookAt(1)
  clookfrom%y=CHGLlookFrom(2)-CHGLlookAt(2)
  clookfrom%z=CHGLlookFrom(3)-CHGLlookAt(3)
  slookfrom=cart2sphere(clookfrom)
  angle%x=-180.0_gldouble*slookfrom%phi/PI-90.0_gldouble
  angle%y=-180.0_gldouble*slookfrom%theta/PI
  shift%x=0.0_gldouble
  shift%y=0.0_gldouble
  shift%z=-slookfrom%rho
  xscale_factor=CHGLxscale_factor
  yscale_factor=CHGLyscale_factor
  zscale_factor=CHGLzscale_factor
  lCHGLlist(1:3)=.true.
  lCHGLlist(4:9)=.false.
  lCHGLdoubleSide=.true.
  if((iObjDra.eq.3).or.(iObjDra.eq.4)) lCHGLdoubleSide=.false.
  iw=iCHGLwidth
  ih=iCHGLheight
  call glutReshapeWindow(max(64,iw),max(64,ih))
  call glutPostRedisplay
end subroutine CHGLreset

subroutine ChGLdisplay
! This gets called when the display needs to be redrawn
  Implicit none
  Integer(4) i
  Integer(kind=glint) iw,ih,id
  call ChGLviewReset
  call glClear(ior(GL_COLOR_BUFFER_BIT,GL_DEPTH_BUFFER_BIT))
  do i=nCHGLlists,1,-1
    if(lCHGLlist(i)) then
      call glCallList(i)
    end if
  end do
  call glutSetWindow(1)
  iw=glutGet(GLUT_WINDOW_WIDTH)
  id=modulo(iw,4_4)
  if(id.ne.0_4) then
    iw=iw+4_4-id
  end if
  ih=glutGet(GLUT_WINDOW_HEIGHT)
  id=modulo(ih,4_4)
  if(id.ne.0_4) then
    ih=ih+4_4-id
  end if
  if((iCHGLWidthCurrent.ne.iw).or.(iCHGLHeightCurrent.ne.ih)) then
    iCHGLWidthCurrent=iw
    iCHGLHeightCurrent=ih
    call glutReshapeWindow(max(64,iw),max(64,ih))
  end if
  call glutShowWindow()
  call glutSwapBuffers
  call glutPostRedisplay
end subroutine ChGLdisplay

Subroutine CHGLColorPalette()
! generate the default color palette
  Implicit none
  Real(kind=glfloat), dimension(4) :: c
  Integer(kind=glint) i
  iCR(0:10)=iWinCR(0:10,kWin)
  iCG(0:10)=iWinCG(0:10,kWin)
  iCB(0:10)=iWinCB(0:10,kWin)
  if(iCHGLmode.gt.0_2) then
    do i=0_glint,235_glint
      call GetColorV(Int2(i),c)
      call glutSetColor(i,c(1),c(2),c(3))
    end do
  end if
end Subroutine CHGLColorPalette

subroutine GetColorV(iCi,c)
  Implicit none
  real(kind=glfloat), dimension(4) :: c
  Integer(2) iCi,iC,ii,ired,igreen,iblue
  c(1:3)=0.0_glfloat
  iC=Max(0_2,Min(235_2,iCi))
  if(iC.lt.16_2) then
    select case (iC)
    case(0,2,5,6)
      c(1)=1.0_glfloat
    case(9,10,13,14)
      c(1)=0.5_glfloat
    end select
    select case (iC)
    case(0,3,5,7)
      c(2)=1.0_glfloat
    case(9,11,13,15)
      c(2)=0.5_glfloat
    end select
    select case (iC)
    case(0,4,6,7)
      c(3)=1.0_glfloat
    case(9,12,14,15)
      c(3)=0.5_glfloat
    end select
  else if(iC.lt.116_2) then
    ii=(iC-16_2)/10_2
    ired=  iCR(ii)+(iCR(ii+1_2)-iCR(ii))*(iC-10_2*ii-16_2)/10_2
    igreen=iCG(ii)+(iCG(ii+1_2)-iCG(ii))*(iC-10_2*ii-16_2)/10_2
    iblue= iCB(ii)+(iCB(ii+1_2)-iCB(ii))*(iC-10_2*ii-16_2)/10_2
    ired=min(255_2,max(0_2,ired))
    igreen=min(255_2,max(0_2,igreen))
    iblue=min(255_2,max(0_2,iblue))
    c(1)=float(ired)/255.0_glfloat
    c(2)=float(igreen)/255.0_glfloat
    c(3)=float(iblue)/255.0_glfloat
  else if(iC.lt.216_2) then
    ii=(iC-116_2)/10_2
    ired=  iCR(ii)+(iCR(ii+1_2)-iCR(ii))*(iC-10_2*ii-116_2)/10_2
    igreen=iCG(ii)+(iCG(ii+1_2)-iCG(ii))*(iC-10_2*ii-116_2)/10_2
    iblue= iCB(ii)+(iCB(ii+1_2)-iCB(ii))*(iC-10_2*ii-116_2)/10_2
    ired=255_2-min(255_2,max(0_2,ired))
    igreen=255_2-min(255_2,max(0_2,igreen))
    iblue=255_2-min(255_2,max(0_2,iblue))
    c(1)=float(ired)/255.0_glfloat
    c(2)=float(igreen)/255.0_glfloat
    c(3)=float(iblue)/255.0_glfloat
  else
    ired=(iC-215_2)*12_2
    ired=min(255_2,max(0_2,ired))
    c(1)=float(ired)/255.0_glfloat
    c(2)=c(1)
    c(3)=c(1)
  end if
  c(4)=1.0_glfloat
end subroutine GetColorV

subroutine CHGLSetColorI(iCi,iCj)
  Implicit none
  Integer(2) iCi
  Integer(2), Optional :: iCj
  Integer(kind=glint) ic(3)
  Real(kind=glfloat), dimension(4) :: c
  call glIndexS(iCi)
  if(iCHGLmode.gt.0_2) then
    ic=0
    ic(1:3)=Int(iCi)
    if(lCHGLdoubleSide) then
      call glMaterialiv(GL_FRONT,GL_COLOR_INDEXES,ic)
      if(Present(iCj)) ic(1:3)=Int(iCj)
      call glMaterialiv(GL_BACK,GL_COLOR_INDEXES,ic)
    else
      call glColor3i(ic(1),ic(2),ic(3))
    end if
  else
    call GetColorV(iCi,c)
    if(lCHGLdoubleSide) then
      call glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,c)
      call glMaterialfv(GL_FRONT,GL_EMISSION,c)
      if(Present(iCj)) call GetColorV(iCj,c)
      call glMaterialfv(GL_BACK,GL_AMBIENT_AND_DIFFUSE,c)
      call glMaterialfv(GL_BACK,GL_EMISSION,c)
    else
      call glColor3f(c(1),c(2),c(3))
    end if
  end if
end subroutine CHGLSetColorI

subroutine CHGLSetColorR(fi,fj)
  Implicit none
  real(8) fi,f
  real(8), Optional :: fj
  Integer(2) iC,jC
  f=Max(0.0d0,Min(1.0d0,fi))
  if(iCHGLrColorMax.ge.iCHGLrColorMin) then
    iC=min(iCHGLrColorMax,iCHGLrColorMin+Int2(f*Dble(iCHGLrColorMax-iCHGLrColorMin)))
  else
    iC=min(iCHGLrColorMin,iCHGLrColorMax+Int2((1.0d0-f)*Dble(iCHGLrColorMin-iCHGLrColorMax)))
  end if
  if(Present(fj)) then
    f=Max(0.0d0,Min(1.0d0,fj))
    if(iCHGLrColorMax.ge.iCHGLrColorMin) then
      jC=min(iCHGLrColorMax,iCHGLrColorMin+Int2(f*Dble(iCHGLrColorMax-iCHGLrColorMin)))
    else
      jC=min(iCHGLrColorMin,iCHGLrColorMax+Int2((1.0d0-f)*Dble(iCHGLrColorMin-iCHGLrColorMax)))
    end if
    call CHGLSetColorI(iC,jC)
  else
    call CHGLSetColorI(iC)
  end if
end subroutine CHGLSetColorR

! draw

subroutine CHGLDrawAxes
! draw x,y,z axes
  implicit none
  real(kind=glfloat) wi
  call OutTxt('t2','OGL Axes'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(1,nCHGLlists)
  call glNewList(1,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  wi=CHGLxAxisW
  call glLineWidth(wi)
  call CHGLSetColorI(iCHGLxAxisColor)
  call glBegin(GL_LINES)
  call SetVertex(CHGLxAxisL,0.0d0,0.0d0,0.0d0)
  call SetVertex(CHGLxAxisL,1.0d0,0.0d0,0.0d0)
  call SetVertex(CHGLxAxisL,1.05d0,-0.05d0,0.05d0)
  call SetVertex(CHGLxAxisL,1.05d0,0.05d0,-0.05d0)
  call SetVertex(CHGLxAxisL,1.05d0,-0.05d0,-0.05d0)
  call SetVertex(CHGLxAxisL,1.05d0,0.05d0,0.05d0)
  call glEnd
  wi=CHGLyAxisW
  call glLineWidth(wi)
  call CHGLSetColorI(iCHGLyAxisColor)
  call glBegin(GL_LINES)
  call SetVertex(CHGLyAxisL,0.0d0,0.0d0,0.0d0)
  call SetVertex(CHGLyAxisL,0.0d0,1.0d0,0.0d0)
  call SetVertex(CHGLyAxisL,-0.05d0,1.05d0,0.05d0)
  call SetVertex(CHGLyAxisL,0.0d0,1.05d0,0.0d0)
  call SetVertex(CHGLyAxisL,0.05d0,1.05d0,0.05d0)
  call SetVertex(CHGLyAxisL,0.0d0,1.05d0,0.0d0)
  call SetVertex(CHGLyAxisL,0.0d0,1.05d0,0.0d0)
  call SetVertex(CHGLyAxisL,0.0d0,1.05d0,-0.05d0)
  call glEnd
  wi=CHGLzAxisW
  call glLineWidth(wi)
  call CHGLSetColorI(iCHGLzAxisColor)
  call glBegin(GL_LINES)
  call SetVertex(CHGLzAxisL,0.0d0,0.0d0,0.0d0)
  call SetVertex(CHGLzAxisL,0.0d0,0.0d0,1.0d0)
  call SetVertex(CHGLzAxisL,-0.05d0,0.05d0,1.05d0)
  call SetVertex(CHGLzAxisL,0.05d0,0.05d0,1.05d0)
  call SetVertex(CHGLzAxisL,0.05d0,0.05d0,1.05d0)
  call SetVertex(CHGLzAxisL,-0.05d0,-0.05d0,1.05d0)
  call SetVertex(CHGLzAxisL,-0.05d0,-0.05d0,1.05d0)
  call SetVertex(CHGLzAxisL,0.05d0,-0.05d0,1.05d0)
  call glEnd
  call glLineWidth(1.0_glFloat)
  call glEndList
  call OutTxt('t2','OGL Axes end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawAxes

subroutine CHGLDrawSurfaces()
! draw surfaces
  Implicit None
  Real(8) fmin,fmax,fl,fb,r4(3,4),dmin,rNmin(3),val(2)
  Integer(4) i,j,k,ik,jk,ikn,lout
  Integer(2) iC,jC,iCmi,iCma
  Logical lold
  call OutTxt('t2','OGL Surf Obj'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','ny'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  if(iObjDra.eq.3) then ! error
    fmin=0.0d0
    fmax=1.0d0
  else
    fmin=CHGLSurfaceMinF
    fmax=CHGLSurfaceMaxF
  end if
  nCHGLlists=max(2,nCHGLlists)
  call glNewList(2,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  jk=1
  lold=lCHGLdoubleSide
  lCHGLdoubleSide=.false. !!! draw web in double side mode does not run under Windows Vista, Windows 7 !!!
  call glDisable(GL_LIGHTING)
  do j=1,nObjWeb
    call IntToStr(j,0,0,SpaceText,lout)
    call OutTxt('n2',SpaceText(1:lout))
    call IntToStr(Int4(nObjWeb),0,0,SpaceText,lout)
    call OutTxt('m2',SpaceText(1:lout))
    if(lObjFlg.and.(j.eq.nObjWeb)) then
      fmin=CHGLSurfaceMinF
      fmax=CHGLSurfaceMaxF
    end if
    ik=jk
    jk=jk+nxObjWeb(j)*nyObjWeb(j)
    if(iCObjWeb(j).eq.-30001_2) Cycle ! transparent
    if(iCObjWeb(j).gt.-30001_2) then ! object colors
      call unPackObjC(1.0d0,iCObjWeb(j),iCmi,iCma) 
      call CHGLSetColorI(iCmi,iCma)
    else if(iCObjWeb(j).eq.-30002_2) then ! domain colors
      call unPackDom(1.0d0,iObjWeb(ik),iCmi,iCma) 
      if(iCmi.lt.0) call DistPtObj(0_2,0_2,BndPt3D(1:3,0,ik),.true.,dmin,rNmin,iCmi,val,.true.)
      if(iCma.lt.0) call DistPtObj(0_2,0_2,BndPt3D(1:3,0,ik),.true.,dmin,rNmin,iCma,val,.true.)
      iC=iDom(max(0_2,iCmi))
      jC=iDom(max(0_2,iCma))
      call CHGLSetColorI(iC,jC)
    end if
    do k=1,nyObjWeb(j)-1
      call IntToStr(k,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nyObjWeb(j)-1,0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      call glBegin(GL_TRIANGLE_STRIP)
      do i=1,nxObjWeb(j)
        if(iCObjWeb(j).eq.-30002_2) then ! domain colors
          call unPackDom(1.0d0,iObjWeb(ik),iCmi,iCma) 
          if(iCmi.lt.0) then
            call DistPtObj(0_2,0_2,ObjWeb(1:3,ik),.true.,dmin,rNmin,iCmi,val,.true.)
            if(iCma.lt.0) iCma=iCmi
          else if(iCma.lt.0) then
            call DistPtObj(0_2,0_2,ObjWeb(1:3,ik),.true.,dmin,rNmin,iCma,val,.true.)
          end if
          iC=iDom(max(0_2,iCmi))
          jC=iDom(max(0_2,iCma))
          call CHGLSetColorI(iC,jC)
        else if(iCObjWeb(j).lt.-30002_2) then ! error or field
          if(rIsoStep.lt.0.0d0) then
            fl=(.log.ObjWebF(ik)-.log.fmin)/(.log.fmax-.log.fmin)
          else
            fl=(ObjWebF(ik)-fmin)/(fmax-fmin)
          end if
          fb=fl
          call CHGLSetColorR(fl,fb)
        end if
        call SetVertex(1.0d0,ObjWeb(1,ik),ObjWeb(2,ik),ObjWeb(3,ik))
        ikn=ik+nxObjWeb(j)
        if(iCObjWeb(j).eq.-30002_2) then ! domain colors
          call unPackDom(1.0d0,iObjWeb(ikn),iCmi,iCma) 
          if(iCmi.lt.0) then
            call DistPtObj(0_2,0_2,ObjWeb(1:3,ikn),.true.,dmin,rNmin,iCmi,val,.true.)
            if(iCma.lt.0) iCma=iCmi
          else if(iCma.lt.0) then
            call DistPtObj(0_2,0_2,ObjWeb(1:3,ikn),.true.,dmin,rNmin,iCma,val,.true.)
          end if
          iC=iDom(max(0_2,iCmi))
          jC=iDom(max(0_2,iCma))
          call CHGLSetColorI(iC,jC)
        else if(iCObjWeb(j).lt.-30002_2) then ! error or field
          if(rIsoStep.lt.0.0d0) then
            fl=(.log.ObjWebF(ikn)-.log.fmin)/(.log.fmax-.log.fmin)
          else
            fl=(ObjWebF(ikn)-fmin)/(fmax-fmin)
          end if
          fb=fl
          call CHGLSetColorR(fl,fb)
        end if
        call SetVertex(1.0d0,ObjWeb(1,ikn),ObjWeb(2,ikn),ObjWeb(3,ikn))
        ik=ik+1
      end do
      call glEnd()
    end do
  end do
  lCHGLdoubleSide=lold !!!
  if(lCHGLdoubleSide) then
    call glEnable(GL_LIGHTING)
  else
    call glDisable(GL_LIGHTING)
  end if
  if(lObjMat.and.(nBndPt3D.gt.0)) then ! matching points
    call OutTxt('t2','OGL Surf'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    call OutTxt('t1','Mat.Point'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    ik=1
    if(iObjDra.eq.2) then ! domain colors
      call unPackDom(1.0d0,iBndPt3D(ik),iCmi,iCma) 
      if(iCmi.lt.0) then
        call DistPtObj(0_2,0_2,ObjWeb(1:3,ikn),.true.,dmin,rNmin,iCmi,val,.true.)
        if(iCma.lt.0) iCma=iCmi
      else if(iCma.lt.0) then
        call DistPtObj(0_2,0_2,ObjWeb(1:3,ikn),.true.,dmin,rNmin,iCma,val,.true.)
      end if
      iC=iDom(max(0_2,iCmi))
      jC=iDom(max(0_2,iCma))
      call CHGLSetColorI(iC,jC)
    else if(iObjDra.eq.0) then ! object colors
      call unPackObjC(1.0d0,iBndPt3D(ik),iC,jC) 
      iC=max(iC,0)
      jC=max(jC,0)
      call CHGLSetColorI(iC,jC)
    else if(iObjDra.eq.3) then ! error
      fmin=0.0d0
      fmax=1.0d0
    end if
    do ik=1,nBndPt3D
      if(iObjDra.eq.1) Cycle ! transparent
      if(.not.lgcFld) then
        if(iObjBndPt3D(ik).lt.0) Cycle
      end if
      call IntToStr(ik,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nBndPt3D,0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      if((iObjDra.eq.3).or.(iObjDra.eq.4)) then ! error or field
        if(rIsoStep.lt.0.0d0) then
          fl=(.log.BndPt3DF(ik)-.log.fmin)/(.log.fmax-.log.fmin)
        else
          fl=(BndPt3DF(ik)-fmin)/(fmax-fmin)
        end if
        fb=fl
        call CHGLSetColorR(fl,fb)
      else if(iObjDra.eq.2) then ! domain colors
        call unPackDom(1.0d0,iBndPt3D(ik),iCmi,iCma) 
        if(iCmi.lt.0) then
          call DistPtObj(0_2,0_2,ObjWeb(1:3,ikn),.true.,dmin,rNmin,iCmi,val,.true.)
          if(iCma.lt.0) iCma=iCmi
        else if(iCma.lt.0) then
          call DistPtObj(0_2,0_2,ObjWeb(1:3,ikn),.true.,dmin,rNmin,iCma,val,.true.)
        end if
        iC=iDom(max(0_2,iCmi))
        jC=iDom(max(0_2,iCma))
        call CHGLSetColorI(iC,jC)
      else if(iObjDra.eq.0) then ! object colors
        call unPackObjC(1.0d0,iBndPt3D(ik),iC,jC) 
        iC=max(iC,0)
        jC=max(jC,0)
        call CHGLSetColorI(iC,jC)
      else if(iObjDra.eq.5) then ! asso.exp. colors
        call unPackObjC(1.0d0,iBndPt3D(ik),iC,jC) 
        iC=max(iC,0)
        jC=max(jC,0)
        call CHGLSetColorI(iC,jC)
      end if
      r4(1:3,1)=BndPt3D(1:3,0,ik)-0.5d0*(BndPt3D(1:3,1,ik)+BndPt3D(1:3,2,ik))
      r4(1:3,2)=r4(1:3,1)+BndPt3D(1:3,1,ik)
      r4(1:3,3)=r4(1:3,2)+BndPt3D(1:3,2,ik)
      r4(1:3,4)=r4(1:3,3)-BndPt3D(1:3,1,ik)
      call glBegin(GL_QUADS)
      call SetVertex(1.0d0,r4(1,1),r4(2,1),r4(3,1))
      call SetVertex(1.0d0,r4(1,2),r4(2,2),r4(3,2))
      call SetVertex(1.0d0,r4(1,3),r4(2,3),r4(3,3))
      call SetVertex(1.0d0,r4(1,4),r4(2,4),r4(3,4))
      call glEnd()
    end do
  end if
  call glLineWidth(1.0_glFloat)
  call glEndList
  call OutTxt('t2','OGL Surf end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawSurfaces

subroutine CHGLDrawGrids
! draw grid
  Implicit None
  Integer(4) i,j,k,ik,jk,lout
  Integer(2) kOb
  Real(8) r4(3,4)
  real(kind=glfloat) wi
  call OutTxt('t2','OGL Grid Obj'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','nx'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(3,nCHGLlists)
  call glNewList(3,GL_COMPILE)
  call glDepthRange(0.00001_glClampd,0.99999_glClampd)
  wi=CHGLyGridLineW
  call glLineWidth(wi)
  jk=1
  do j=1,nObjWeb
    call IntToStr(j,0,0,SpaceText,lout)
    call OutTxt('n2',SpaceText(1:lout))
    call IntToStr(Int4(nObjWeb),0,0,SpaceText,lout)
    call OutTxt('m2',SpaceText(1:lout))
    ik=jk
    if((j.eq.nObjWeb).and.lObjFlg) then
      call CHGLSetColorI(iCHGLyGridColor)
    else
      call CHGLSetColorI(tObj(j)%iCol)
    end if
    do k=1,nyObjWeb(j)
      call IntToStr(k,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(Int4(nyObjWeb(j)),0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      call glBegin(GL_LINE_STRIP)
      do i=1,nxObjWeb(j)
        call SetVertex(1.0d0,ObjWeb(1,ik),ObjWeb(2,ik),ObjWeb(3,ik))
        ik=ik+1
      end do
      call glEnd()
    end do
    wi=CHGLxGridLineW
    call glLineWidth(wi)
    if((j.eq.nObjWeb).and.lObjFlg) call CHGLSetColorI(iCHGLxGridColor)
    do i=1,nxObjWeb(j)
      call IntToStr(i,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(Int4(nxObjWeb(j)),0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      ik=jk+i-1
      call glBegin(GL_LINE_STRIP)
      do k=1,nyObjWeb(j)
        call SetVertex(1.0d0,ObjWeb(1,ik),ObjWeb(2,ik),ObjWeb(3,ik))
        ik=ik+nxObjWeb(j)
      end do
      call glEnd()
    end do
    jk=jk+nxObjWeb(j)*nyObjWeb(j)
  end do
  if(lObjMat) then ! borders of matching points
    call OutTxt('t2','OGL Grid'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    call OutTxt('t1','Mat.Point'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    do ik=1,nBndPt3D
      if(.not.lgcFld) then
        if(iObjBndPt3D(ik).lt.0) Cycle
      end if
      call IntToStr(ik,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nBndPt3D,0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      kOb=min(nObj,iGetObjNr(ik)) ! get object number
      r4(1:3,1)=BndPt3D(1:3,0,ik)-0.5d0*(BndPt3D(1:3,1,ik)+BndPt3D(1:3,2,ik))
      r4(1:3,2)=r4(1:3,1)+BndPt3D(1:3,1,ik)
      r4(1:3,3)=r4(1:3,2)+BndPt3D(1:3,2,ik)
      r4(1:3,4)=r4(1:3,3)-BndPt3D(1:3,1,ik)
      call glBegin(GL_LINE_LOOP)
      call CHGLSetColorI(tObj(kOb)%iCol)
      call SetVertex(1.0d0,r4(1,1),r4(2,1),r4(3,1))
      call SetVertex(1.0d0,r4(1,2),r4(2,2),r4(3,2))
      call SetVertex(1.0d0,r4(1,3),r4(2,3),r4(3,3))
      call SetVertex(1.0d0,r4(1,4),r4(2,4),r4(3,4))
      call glEnd()
    end do
  end if
  call glLineWidth(1.0_glFloat)
  call glEndList
  call OutTxt('t2','OGL Grid end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawGrids

subroutine CHGLDrawIsoLines
! draw grid
  Implicit None
  Real(8) rA(3),rB(3),rC(3),rD(3),fA,fB,fC,fD
  Integer(4) i,j,k,ik,jk,lout
  real(kind=glfloat) wi
  call OutTxt('t2','OGL Iso Obj'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','nx'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(4,nCHGLlists)
  call glNewList(4,GL_COMPILE)
  call glDepthRange(0.00001_glClampd,0.99999_glClampd)
  wi=CHGLzGridLineW
  call glLineWidth(wi)
  call CHGLSetColorI(iCHGLzGridColor)
  call glBegin(GL_LINES)
  jk=1
  call glDepthRange(0.00001_glClampd,0.99999_glClampd)
  call glLineWidth(wi)
  call CHGLSetColorI(iCHGLzGridColor)
  do j=1,nObjWeb
    call IntToStr(j,0,0,SpaceText,lout)
    call OutTxt('n2',SpaceText(1:lout))
    call IntToStr(Int4(nObjWeb),0,0,SpaceText,lout)
    call OutTxt('m2',SpaceText(1:lout))
    do i=1,nxObjWeb(j)-1
      call IntToStr(i,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nxObjWeb(j)-1,0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      ik=jk+i-1
      do k=1,nyObjWeb(j)-1
        rA(1)=ObjWeb(1,ik)
        rA(2)=ObjWeb(2,ik)
        rA(3)=ObjWeb(3,ik)
        fA=ObjWebF(ik)
        rB(1)=ObjWeb(1,ik+nxObjWeb(j))
        rB(2)=ObjWeb(2,ik+nxObjWeb(j))
        rB(3)=ObjWeb(3,ik+nxObjWeb(j))
        fB=ObjWebF(ik+nxObjWeb(j))
        rC(1)=ObjWeb(1,ik+1)
        rC(2)=ObjWeb(2,ik+1)
        rC(3)=ObjWeb(3,ik+1)
        fC=ObjWebF(ik+1)
        rD(1)=ObjWeb(1,ik+nxObjWeb(j)+1)
        rD(2)=ObjWeb(2,ik+nxObjWeb(j)+1)
        rD(3)=ObjWeb(3,ik+nxObjWeb(j)+1)
        fD=ObjWebF(ik+nxObjWeb(j)+1)
        call DrawIsoIn3(rA,rB,rC,fA,fB,fC,CHGLIsoMinF,CHGLIsoMaxF,CHGLIsoStepF)
        call DrawIsoIn3(rB,rC,rD,fB,fC,fD,CHGLIsoMinF,CHGLIsoMaxF,CHGLIsoStepF)
        ik=ik+nxObjWeb(j)
      end do
    end do
    jk=jk+nxObjWeb(j)*nyObjWeb(j)
  end do
  call glEnd()
  call glLineWidth(1.0_glFloat)
  call glEndList
  call OutTxt('t2','OGL Iso end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawIsoLines

subroutine CHGLDrawVectors
! draw vectors
  Implicit None
  Real(8) r(3)
  Integer(4) i,j,k,ik,lout
  call OutTxt('t2','OGL Vect Obj'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','ny'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(5,nCHGLlists)
  call glNewList(5,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  ik=1
  do j=1,nObjWeb
    call IntToStr(j,0,0,SpaceText,lout)
    call OutTxt('n2',SpaceText(1:lout))
    call IntToStr(Int4(nObjWeb),0,0,SpaceText,lout)
    call OutTxt('m2',SpaceText(1:lout))
    do k=1,nyObjWeb(j)
      call IntToStr(k,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(Int4(nyObjWeb(j)),0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      do i=1,nxObjWeb(j)
        if((modulo(i-1_4,Int4(iCHGLxVectorStep)).eq.0_4).and.(modulo(k-1_4,Int4(iCHGLyVectorStep)).eq.0_4)) then
          r(1:3)=ObjWeb(1:3,ik)
          call CHGLDrawVector(r,ObjWebV(1:3,ik),CHGLVectorScale,CHGLVectorMaxLength,iCHGLxVectorColor,iCHGLyVectorColor)
        end if
        ik=ik+1
      end do
    end do
  end do
  if(lObjMat) then ! matching points
    call OutTxt('t2','OGL Vect'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    call OutTxt('t1','Mat.Point'C)
    call OutTxt('n1',' 'C)
    call OutTxt('m1',' 'C)
    do i=1,nBndPt3D
      if(.not.lgcFld) then
        if(iObjBndPt3D(i).lt.0) Cycle
      end if
      call IntToStr(i,0,0,SpaceText,lout)
      call OutTxt('n1',SpaceText(1:lout))
      call IntToStr(nBndPt3D,0,0,SpaceText,lout)
      call OutTxt('m1',SpaceText(1:lout))
      r(1:3)=BndPt3D(1:3,0,i)
      call CHGLDrawVector(r,BndPt3DV(1:3,i),CHGLVectorScale,CHGLVectorMaxLength,iCHGLxVectorColor,iCHGLyVectorColor)
    end do
  end if
  call glEndList
  call OutTxt('t2','OGL Vect end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawVectors

subroutine CHGLDrawExpansions
! draw expansions
  Implicit None
  Real(8) ra(3),rb(3),Pl(3,0:3),r
  Real(8), Allocatable :: Poly(:,:)
  Integer(4) kE,lout,iErr,i,np,idum
  call OutTxt('t2','OGL Expansion'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','n'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(6,nCHGLlists)
  call glNewList(6,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  do kE=1,nExp
    if(LSkipExp(kE)) Cycle
    call IntToStr(kE,0,0,SpaceText,lout)
    call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(Int4(nExp),0,0,SpaceText,lout)
    call OutTxt('m1',SpaceText(1:lout))
    Select Case(tExp(kE)%iTypE)
    Case(0,1,2,3,4,5,11,12,13)
      call CHGLDrawExpansion(tExp(kE)%Plane(1:3,0:3),tExp(kE)%iCol,CHGLExpLen)
    Case(6,7) ! 3d multipole or Bessel
      call CHGLDrawExpansion(tExp(kE)%Plane(1:3,0:3),tExp(kE)%iCol,CHGLExpLen)
      if((abs(tExp(kE)%rE(2))+abs(tExp(kE)%rE(3))+abs(tExp(kE)%rE(4))).gt.pSmall) then ! complex origin multipole
        nP=36
        if(Allocated(Poly)) DeAllocate(Poly)
        Allocate(Poly(3,np),Stat=iErr)
        if(iErr.ne.0) then
          idum=MessageBoxQQ('Cannot allocate buffer!'C,'Draw Spiral'C, &
                            MB$OK.or.MB$ICONSTOP)
        else
          Pl(1:3,0)=0.0d0
          Pl(1:3,2)=tExp(kE)%rE(2:4)
          call Unit3DV(Pl(1:3,2),r)
          ra(1)=0.23417123098d0 ! some random vector
          ra(2)=-0.76327567541d0
          ra(3)=0.51234325468d0
          Pl(1:3,3)=r3Vec_Prod(ra,Pl(1:3,2))
          call Unit3DV(Pl(1:3,3))
          Pl(1:3,1)=r3Vec_Prod(Pl(1:3,2),Pl(1:3,3))
          call sLoc2Glob(Pl,tExp(kE)%Plane(1:3,0:3),Pl)
          ra(1)=r
          ra(2:3)=0.0d0
          call Pt2Torus(ra,0.0d0,360.0d0,np,Pl,Poly)
          call glBegin(GL_LINE_STRIP)
          call CHGLSetColorI(abs(tExp(kE)%iCol))
          do i=1,np
            call SetVertex(1.0d0,Poly(1,i),Poly(2,i),Poly(3,i))
          end do
          call glEnd()
          DeAllocate(Poly)
          ra(1:3)=tExp(kE)%Plane(1:3,0)
          call rvLoc2Glob(tExp(kE)%rE(2:4),tExp(kE)%Plane(1:3,0:3),rb(1:3))
          rb(1:3)=ra(1:3)+rb(1:3)
          call glBegin(GL_LINES)
          call CHGLSetColorI(abs(tExp(kE)%iCol))
          call SetVertex(1.0d0,ra(1),ra(2),ra(3))
          call SetVertex(1.0d0,rb(1),rb(2),rb(3))
          call glEnd()
        end if
      end if
    Case(8) ! 3d Ring
      call CHGLDrawExpansion(tExp(kE)%Plane(1:3,0:3),tExp(kE)%iCol,CHGLExpLen)
      np=Nint(min(1000.0d0,max(3.0d0,(abs(tExp(kE)%rE(3))*0.101d0))),4)
      if(Allocated(Poly)) DeAllocate(Poly)
      Allocate(Poly(3,np),Stat=iErr)
      if(iErr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate buffer!'C,'Draw Spiral'C, &
                          MB$OK.or.MB$ICONSTOP)
      else
        ra(1)=tExp(kE)%rE(1)*tExp(kE)%Plane(1,1)
        ra(2)=tExp(kE)%rE(1)*tExp(kE)%Plane(2,1)
        ra(3)=tExp(kE)%rE(1)*tExp(kE)%Plane(3,1)
        call Pt2Torus(ra,tExp(kE)%rE(2),tExp(kE)%rE(3),np,tExp(kE)%Plane,Poly)
        call glBegin(GL_LINE_STRIP)
        call CHGLSetColorI(abs(tExp(kE)%iCol))
        do i=1,np
          call SetVertex(1.0d0,Poly(1,i),Poly(2,i),Poly(3,i))
        end do
        call glEnd()
        DeAllocate(Poly)
      end if
    Case(9) ! 3d Line
      call CHGLDrawExpansion(tExp(kE)%Plane(1:3,0:3),tExp(kE)%iCol,CHGLExpLen)
      ra(1:3)=tExp(kE)%Plane(1:3,0)+tExp(kE)%rE(1)*tExp(kE)%Plane(1:3,3)
      rb(1:3)=ra(1:3)+tExp(kE)%rE(2)*tExp(kE)%Plane(1:3,3)
      call glBegin(GL_LINES)
      call CHGLSetColorI(abs(tExp(kE)%iCol))
      call SetVertex(1.0d0,ra(1),ra(2),ra(3))
      call SetVertex(1.0d0,rb(1),rb(2),rb(3))
      call glEnd()
    Case(10) ! 3d Spiral
      call CHGLDrawExpansion(tExp(kE)%Plane(1:3,0:3),tExp(kE)%iCol,CHGLExpLen)
      np=Nint(min(1000.0d0,max(3.0d0,(abs(tExp(kE)%rE(2))*0.101d0))),4)
      if(Allocated(Poly)) DeAllocate(Poly)
      Allocate(Poly(3,np),Stat=iErr)
      if(iErr.ne.0) then
        idum=MessageBoxQQ('Cannot allocate buffer!'C,'Draw Spiral'C, &
                          MB$OK.or.MB$ICONSTOP)
      else
        ra(1)=tExp(kE)%xo
        ra(2)=tExp(kE)%yo
        ra(3)=0.0d0
        call Pt2Spiral(ra,tExp(kE)%O,tExp(kE)%e,tExp(kE)%rE(1),tExp(kE)%rE(2),tExp(kE)%rE(3),np, &
        &              tExp(kE)%Plane,Poly)
        call glBegin(GL_LINE_STRIP)
        call CHGLSetColorI(abs(tExp(kE)%iCol))
        do i=1,np
          call SetVertex(1.0d0,Poly(1,i),Poly(2,i),Poly(3,i))
        end do
        call glEnd()
        DeAllocate(Poly)
      end if
    end Select
  end do
  call glEndList
  call OutTxt('t2','OGL Exp end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawExpansions

subroutine CHGLDrawTubes
! draw field lines
  Implicit None
  Real(8) r0(3),r,d
  Real(8), Allocatable:: Tube(:,:,:)
  Integer(4) nx,ny,idum,iErr,lout,k
  Integer(2) iC(3)
  Logical lInvers
  call OutTxt('t2','OGL Tube'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','n'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(7,nCHGLlists)
  call glNewList(7,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  do k=1,nCHGLTube
    call IntToStr(k,0,0,SpaceText,lout)
    call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(nCHGLTube,0,0,SpaceText,lout)
    call OutTxt('m1',SpaceText(1:lout))
    nx=iCHGLTubeR(k)
    ny=iCHGLTubeD(k)
    if((nx.lt.0).or.(ny.lt.1)) Cycle
    iC(1:3)=iCHGLTubeColor(1:3,k)
    r0(1:3)=CHGLTubeStart(1:3,k)
    r=CHGLTubeR(k)
    d=CHGLTubeD(k)
    lInvers=.false.
    if(d.lt.0.0d0) lInvers=.true.
    Allocate(Tube(3,0:ny,nx),stat=iErr)
    if(iErr.ne.0) return
    idum=ny
    call getFieldTube(r0,r,d,nx,idum,Tube,ny)
    if(ny.gt.0) then
      call CHGLDrawTube(Tube,nx,ny,iC,lInvers)
    end if
    DeAllocate(Tube,stat=iErr)
  end do
  call glEndList
  call OutTxt('t2','OGL Tube end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawTubes

subroutine CHGLDrawPFDSources
! draw field lines
  Implicit None
  Real(8) r(3)
  Integer(4) lout,k
  call OutTxt('t2','OGL PFDsource'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','n'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(8,nCHGLlists)
  call glNewList(8,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  call glLineWidth(1.0_glFloat)
  call CHGLSetColorI(2_2)
  do k=1,nPFDsource
    call IntToStr(k,0,0,SpaceText,lout)
    call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(nPFDsource,0,0,SpaceText,lout)
    call OutTxt('m1',SpaceText(1:lout))
    r(1)=PFDxmin+Dble(iPFDs(k)-1)*PFDdx
    r(2)=PFDymin+Dble(jPFDs(k)-1)*PFDdy
    r(3)=PFDzmin+Dble(kPFDs(k)-1)*PFDdz
    call glBegin(GL_LINES)
    call SetVertex(CHGLyAxisL,r(1)-0.5d0*PFDdx,r(2),r(3))
    call SetVertex(CHGLyAxisL,r(1)+0.5d0*PFDdx,r(2),r(3))
    call glEnd
    call glBegin(GL_LINES)
    call SetVertex(CHGLyAxisL,r(1),r(2)-0.5d0*PFDdy,r(3))
    call SetVertex(CHGLyAxisL,r(1),r(2)+0.5d0*PFDdy,r(3))
    call glEnd
    call glBegin(GL_LINES)
    call SetVertex(CHGLyAxisL,r(1),r(2),r(3)-0.5d0*PFDdz)
    call SetVertex(CHGLyAxisL,r(1),r(2),r(3)+0.5d0*PFDdz)
    call glEnd
  end do
  call glLineWidth(1.0_glFloat)
  call glEndList
  call OutTxt('t2','OGL PFDsource end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawPFDSources

subroutine CHGLDrawPFDSensors
! draw field lines
  Implicit None
  Real(8) r(3)
  Integer(4) lout,k
  call OutTxt('t2','OGL PFDsensor'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1','n'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
  nCHGLlists=max(9,nCHGLlists)
  call glNewList(9,GL_COMPILE)
  call glDepthRange(0.0_glClampd,1.0_glClampd)
  call glLineWidth(1.0_glFloat)
  call CHGLSetColorI(3_2)
  do k=1,nPFDsens
    call IntToStr(k,0,0,SpaceText,lout)
    call OutTxt('n1',SpaceText(1:lout))
    call IntToStr(nPFDsens,0,0,SpaceText,lout)
    call OutTxt('m1',SpaceText(1:lout))
    r(1)=PFDsensX(k)
    r(2)=PFDsensY(k)
    r(3)=PFDsensZ(k)
    call glBegin(GL_LINES)
    call SetVertex(CHGLyAxisL,r(1)-0.5d0*PFDdx,r(2),r(3))
    call SetVertex(CHGLyAxisL,r(1)+0.5d0*PFDdx,r(2),r(3))
    call glEnd
    call glBegin(GL_LINES)
    call SetVertex(CHGLyAxisL,r(1),r(2)-0.5d0*PFDdy,r(3))
    call SetVertex(CHGLyAxisL,r(1),r(2)+0.5d0*PFDdy,r(3))
    call glEnd
    call glBegin(GL_LINES)
    call SetVertex(CHGLyAxisL,r(1),r(2),r(3)-0.5d0*PFDdz)
    call SetVertex(CHGLyAxisL,r(1),r(2),r(3)+0.5d0*PFDdz)
    call glEnd
  end do
  call glLineWidth(1.0_glFloat)
  call glEndList
  call OutTxt('t2','OGL PFDsensor end'C)
  call OutTxt('n2',' 'C)
  call OutTxt('m2',' 'C)
  call OutTxt('t1',' 'C)
  call OutTxt('n1',' 'C)
  call OutTxt('m1',' 'C)
end subroutine CHGLDrawPFDSensors

subroutine ChGLsaveBMP(name)
  Implicit none
  Integer(kind=glint) il,iw,ih,ii
  integer(kind=glshort), allocatable :: buff(:)
  integer(kind=glint), allocatable :: ibuff(:)
  integer(1), allocatable :: i1buff(:),i1AVIbuff(:)
  real(kind=glfloat), dimension(4) :: cRGBA
  Real(4) d,dmin,r,g,b
  Integer(2) icRGBA,imin
  Integer(1) iPalette(4)
  character*2 c2
  integer(2) iPlanes,iBitsPerPixel
  integer(4) iFileSize,iReserved,iBmpDataOffset,iBmpHeaderSize,iWidth,iHeight,iCompression,iBitmapDataSize, &
  & iHResolution,iVResolution,iColors,iImportantColors
  integer(4) i,i1,ierr,writeAVIframe,ia
  ! character*256, optional :: name
  character(*), optional :: name
  call glutSetCursor(GLUT_CURSOR_WAIT)
  iw=iCHGLWidthCurrent
  ih=iCHGLHeightCurrent
  if(iCHGLmode.gt.0_2) then
    allocate(ibuff(iw),i1buff(iw))
  else
    allocate(buff(3*4),i1buff(iw)) ! I don't know why this works but not buff(3*iw) - scan row by row - or buff(3) - scan pixels
  end if
  c2='BM'
  iBmpHeaderSize=40
  iBmpDataOffset=1078
  iReserved=0
  iPlanes=1
  iBitsPerPixel=8
  iCompression=0
  iColors=256
  iImportantColors=256
  iw=glutGet(GLUT_WINDOW_WIDTH)
  iWidth=iw
  ih=glutGet(GLUT_WINDOW_HEIGHT)
  iHeight=ih
  iHResolution=iw*10
  iVResolution=ih*10
  iBitmapDataSize=iw*ih
  iFileSize=iBitmapDataSize+iBmpDataOffset
  if(Present(name)) then
    open(2,file=name,Access='Sequential',Form='Binary',Action='Write',Status='Replace',IOStat=iErr)
  else
    open(2,file='dummy.bmp',Access='Sequential',Form='Binary',Action='Write',Status='Replace',IOStat=iErr)
  end if
  write(2,IOStat=iErr) c2
  write(2,IOStat=iErr) iFileSize
  write(2,IOStat=iErr) iReserved
  write(2,IOStat=iErr) iBmpDataOffset
  write(2,IOStat=iErr) iBmpHeaderSize
  write(2,IOStat=iErr) iWidth
  write(2,IOStat=iErr) iHeight
  write(2,IOStat=iErr) iPlanes
  write(2,IOStat=iErr) iBitsPerPixel
  write(2,IOStat=iErr) iCompression
  write(2,IOStat=iErr) iBitmapDataSize
  write(2,IOStat=iErr) iHResolution
  write(2,IOStat=iErr) iVResolution
  write(2,IOStat=iErr) iColors
  write(2,IOStat=iErr) iImportantColors
  ia=55
  do icRGBA=0_2,255_2
    call GetColorV(icRGBA,cRGBA)
    cRGBA(1:3)=255.0*cRGBA(1:3)
    do i=1,3
      if(cRGBA(i).lt.128.0) then
        iPalette(i)=Int1(cRGBA(i))
      else
        iPalette(i)=Int1(cRGBA(i)-256.0)
      end if
    end do
    iPalette(4)=0_1
    write(2,IOStat=iErr) iPalette(3),iPalette(2),iPalette(1),iPalette(4)
  end do
  do il=0_glint,ih-1_glint
    if(iCHGLmode.gt.0_2) then
      call glReadPixels(0_glint,il,iw,1_glint,GL_COLOR_INDEX,GL_INT,ibuff)
      do i1=1,iw
        if(ibuff(i1).lt.128) then
          i1buff(i1)=int1(ibuff(i1))
        else
          i1buff(i1)=int1(ibuff(i1)-256_2)
        end if
      end do
    else
      do ii=0_glint,iw-1_glint
      i1=ii+1
      call glReadPixels(ii,il,2_glint,2_glint,GL_RGB,GL_SHORT,buff)
        i1buff(i1)=0_1
        imin=0_2
        dmin=1.0
        do icRGBA=0_2,235_2
          call GetColorV(icRGBA,cRGBA)
          r=float(buff(1))/32767.0
          g=float(buff(2))/32767.0
          b=float(buff(3))/32767.0
          d=sqrt((r-cRGBA(1))**2+(g-cRGBA(2))**2+(b-cRGBA(3))**2)
          if(d.lt.dmin) then
            dmin=d
            imin=icRGBA
          end if
        end do
        if(imin.lt.128) then
          i1buff(i1)=int1(imin)
        else
          i1buff(i1)=int1(imin-256_2)
        end if
      end do
    end if
    write(2,IOStat=iErr) i1buff
  end do
  close(2)
  if(.not.Present(name)) then
    allocate(i1AVIbuff(iFileSize))
    open(2,file='dummy.bmp',Access='Sequential',Form='Binary',Action='Read',Status='Old',IOStat=iErr)
    read(2,IOStat=iErr) i1AVIbuff
    close(2)
    i=writeAVIframe(i1AVIbuff,icPalette(0),-1_4)
  end if
  if(allocated(i1AVIbuff)) deallocate(i1AVIbuff)
  if(allocated(i1buff)) deallocate(i1buff)
  if(allocated(ibuff)) deallocate(ibuff)
  if(allocated(buff)) deallocate(buff)
  call glutSetCursor(GLUT_CURSOR_RIGHT_ARROW)
end subroutine ChGLsaveBMP

! private routines

subroutine menu_handler(value)
! handles the first level entries in the menu
  Implicit none
  Integer(kind=glcint),intent(in out) :: value
  Integer(kind=glint) iw,ih
  Integer(4) i,openAVIfile,closeAVIfile
  character(12) Aname
  integer(4), save :: iAname
  data iAname/0/
  select case(value)
  case(ShowAxes)
    lCHGLlist(1)=.not.lCHGLlist(1)
    call glutPostRedisplay
  case(ShowSurface)
    lCHGLlist(2)=.not.lCHGLlist(2)
    call glutPostRedisplay
  case(ShowGrid)
    lCHGLlist(3)=.not.lCHGLlist(3)
    call glutPostRedisplay
  case(ShowIso)
    lCHGLlist(4)=.not.lCHGLlist(4)
    call glutPostRedisplay
  case(ShowVectors)
    lCHGLlist(5)=.not.lCHGLlist(5)
    call glutPostRedisplay
  case(ShowExpansions)
    lCHGLlist(6)=.not.lCHGLlist(6)
    call glutPostRedisplay
  case(ShowTubes)
    lCHGLlist(7)=.not.lCHGLlist(7)
    call glutPostRedisplay
  case(ShowPointSources)
    lCHGLlist(8)=.not.lCHGLlist(8)
    call glutPostRedisplay
  case(ShowXsensors)
    lCHGLlist(9)=.not.lCHGLlist(9)
    call glutPostRedisplay
  case(RESET)
    call CHGLreset
  case(DRAW)
    call CHGLDraw()
  case(MOVIE)
    if(iCHGLMovie.gt.0) then
      Aname='OGL0000.AVI'C
      write(Aname(4:7),'(1i4.4)') iAname
      iAname=iAname+1
      if(iAname.gt.9999) iAname=0
      iw=iCHGLWidthCurrent
      ih=iCHGLHeightCurrent
      i=openAVIfile(iAVIdlg,Aname//char(0),Int4(iw),Int4(ih),nAVIframes,nAVIfpsec)
      prFld=0.0d0
      trFld=0.0d0
      do i=1,iCHGLMovie
        if(Dabs(Dble(fcFld)).gt.pSmall) then
          prFld=prFld+rCHGLMovie
          trFld=prFld/(360.0d0*Dble(fcFld))
        else
          trFld=trFld+rCHGLMovie
        end if
        call CHGLDraw()
        call ChGLsaveBMP()
      end do
      i=closeAVIfile()
    else
      call CHGLMovie()
    end if
  case(BITMAP)
    call incName(BmpFileName,'MAX','BMP',1)
    call CHGLsaveBMP(BmpFileName)
  end select
end subroutine menu_handler

subroutine mouse(button,state,x,y)
! called when a mouse button changes
  Implicit none
  integer(kind=glcint),intent(in out) :: button,state,x,y
  if(button.eq.GLUT_LEFT_BUTTON) then
    if(state.eq.GLUT_DOWN) then
      moving_left=.true.
      begin_left=cart2D(x,y)
    else if(state.eq.GLUT_UP) then
      moving_left=.false.
    end if
  else if(button.eq.GLUT_MIDDLE_BUTTON) then
    if(state.eq.GLUT_DOWN) then
      moving_middle=.true.
      begin_middle=cart2D(x,y)
    else if(state.eq.GLUT_UP) then
      moving_middle=.false.
    end if
  end if
end subroutine mouse

subroutine motion(x,y)
! called when the mouse moves
  Implicit none
  integer(kind=glcint),intent(in out) :: x,y
  Integer(4) :: button_function
  type(cart2D) :: begin
  real(kind=gldouble) :: factor
! Determine and apply the button function
  if(moving_left) then
    button_function=left_button_func
    begin=begin_left
  else if(moving_middle) then
    button_function=middle_button_func
    begin=begin_middle
  else
    return
  end if
  select case(button_function)
  case(ZOOM)
    if(y<begin%y) then
      factor=1.0_gldouble/(1.0_gldouble+.002_gldouble*(begin%y-y))
    else if(y>begin%y) then
      factor=1.0_gldouble+.002_gldouble*(y-begin%y)
    else
      factor=1.0_gldouble
    end if
    shift%z=factor*shift%z
  case(PANXY)
    shift%x=shift%x+.01*(x-begin%x)*CHGLf
    shift%y=shift%y-.01*(y-begin%y)*CHGLf
  case(ROTATE)
    angle%x=angle%x+(x-begin%x)
    angle%y=angle%y+(y-begin%y)
  case(SCALEX)
    if(y<begin%y) then
      factor=1.0_gldouble+.002_gldouble*(begin%y-y)
    else if(y>begin%y) then
      factor=1.0_gldouble/(1.0_gldouble+.002_gldouble*(y-begin%y))
    else
      factor=1.0_gldouble
    end if
    xscale_factor=xscale_factor*factor
  case(SCALEY)
    if(y<begin%y) then
      factor=1.0_gldouble+.002_gldouble*(begin%y-y)
    else if(y>begin%y) then
      factor=1.0_gldouble/(1.0_gldouble+.002_gldouble*(y-begin%y))
    else
      factor=1.0_gldouble
    end if
    yscale_factor=yscale_factor*factor
  case(SCALEZ)
    if(y<begin%y) then
      factor=1.0_gldouble+.002_gldouble*(begin%y-y)
    else if(y>begin%y) then
      factor=1.0_gldouble/(1.0_gldouble+.002_gldouble*(y-begin%y))
    else
      factor=1.0_gldouble
    end if
    zscale_factor=zscale_factor*factor
  end select
! update private variables and redisplay
  if(moving_left) then
    begin_left=cart2D(x,y)
  else if(moving_middle) then
    begin_middle=cart2D(x,y)
  endif
  if(moving_left.or.moving_middle) call glutPostRedisplay
end subroutine motion

subroutine arrows(key,x,y)
! handles the arrow key operations
  USE CHDLG
  Implicit none
  integer(glcint),intent(in out) :: key,x,y
  real(kind=gldouble) :: factor
  character(12) name
  integer(4), save :: iname
  data iname/0/
  if(key.eq.GLUT_KEY_F10) then
    name='OGL0000.BMP'C
    write(name(4:7),'(1i4.4)') iname
    iname=iname+1
    if(iname.gt.9999) iname=0
    call CHGLsaveBMP(name)
  else
    factor=1.0_gldouble
    select case(arrow_key_func)
    case(ZOOM)
      select case(key)
      case(GLUT_KEY_DOWN)
        factor=1.0_gldouble+.02_gldouble
      case(GLUT_KEY_UP)
        factor=1.0_gldouble/(1.0_gldouble+.02_gldouble)
      end select
      shift%z=factor*shift%z
    case(PANXY)
      select case(key)
      case(GLUT_KEY_LEFT)
        shift%x=shift%x-.02*CHGLf
      case(GLUT_KEY_RIGHT)
        shift%x=shift%x+.02*CHGLf
      case(GLUT_KEY_DOWN)
        shift%y=shift%y-.02*CHGLf
      case(GLUT_KEY_UP)
        shift%y=shift%y+.02*CHGLf
      end select
    case(ROTATE)
      select case(key)
      case(GLUT_KEY_LEFT)
        angle%x=angle%x-1.0_gldouble
      case(GLUT_KEY_RIGHT)
        angle%x=angle%x+1.0_gldouble
      case(GLUT_KEY_DOWN)
        angle%y=angle%y+1.0_gldouble
      case(GLUT_KEY_UP)
        angle%y=angle%y-1.0_gldouble
      end select
    case(SCALEX)
      select case(key)
      case(GLUT_KEY_DOWN)
        factor=1.0_gldouble/(1.0_gldouble+.02_gldouble)
      case(GLUT_KEY_UP)
        factor=1.0_gldouble+.02_gldouble
      end select
      xscale_factor=xscale_factor*factor
    case(SCALEY)
      select case(key)
      case(GLUT_KEY_DOWN)
        factor=1.0_gldouble/(1.0_gldouble+.02_gldouble)
      case(GLUT_KEY_UP)
        factor=1.0_gldouble+.02_gldouble
      end select
      yscale_factor=yscale_factor*factor
    case(SCALEZ)
      select case(key)
      case(GLUT_KEY_DOWN)
        factor=1.0_gldouble/(1.0_gldouble+.02_gldouble)
      case(GLUT_KEY_UP)
        factor=1.0_gldouble+.02_gldouble
      end select
      zscale_factor=zscale_factor*factor
    end select
  end if
  call glutPostRedisplay
  if(x.eq.y) return ! dummy statement (otherwise x and y would be unused)
end subroutine arrows

subroutine set_left_button(value)
! sets the function of the left button as given by menu selection
  Implicit none
  integer(kind=glcint),intent(in out) :: value
  left_button_func=value
end subroutine set_left_button

subroutine set_middle_button(value)
! sets the function of the middle button as given by menu selection
  Implicit none
  integer(kind=glcint),intent(in out) :: value
  middle_button_func=value
end subroutine set_middle_button

subroutine set_arrow_keys(value)
! This routine sets the function of the arrow keys as given by menu selection
  Implicit none
  integer(kind=glcint),intent(in out) :: value
  arrow_key_func=value
end subroutine set_arrow_keys

! auxiliary for coordinate handling

function sphere2cart(spoint) result(cpoint)
! converts a 3D point from spherical to cartesean coordinates
  Implicit none
  type(sphere3D),intent(in) :: spoint
  type(cart3D) :: cpoint
  real(kind=gldouble) :: t,p,r
  t=spoint%theta
  p=spoint%phi
  r=spoint%rho
  cpoint%x=r*cos(t)*sin(p)
  cpoint%y=r*sin(t)*sin(p)
  cpoint%z=r*cos(p)
  return
end function sphere2cart

function cart2sphere(cpoint) result(spoint)
! converts a 3D point from cartesean to spherical coordinates
  Implicit none
  type(cart3D),intent(in) :: cpoint
  type(sphere3D) :: spoint
  real(kind=gldouble) :: x,y,z
  x=cpoint%x
  y=cpoint%y
  z=cpoint%z
  spoint%rho=sqrt(x*x+y*y+z*z)
  if(x==0.0_gldouble.and.y==0.0_gldouble) then
    spoint%phi=-0.5d0*PI !0.0_gldouble
  else
    spoint%phi=atan2(y,x)
  end if
  if(dabs(spoint%rho).lt.pSmall) then
    spoint%theta=0.0_gldouble
  else
    spoint%theta=dacos(z.div.spoint%rho)
  endif
end function cart2sphere

function cart3D_plus_cart3D(cart1,cart2) result(cart3)
! Compute the sum of two 3D cartesean points
  Implicit none
  type(cart3D),intent(in) :: cart1,cart2
  type(cart3D) :: cart3
  cart3%x=cart1%x+cart2%x
  cart3%y=cart1%y+cart2%y
  cart3%z=cart1%z+cart2%z
end function cart3D_plus_cart3D

function cart3D_minus_cart3D(cart1,cart2) result(cart3)
! Compute the difference of two 3D cartesean points
  Implicit none
  type(cart3D),intent(in) :: cart1,cart2
  type(cart3D) :: cart3
  cart3%x=cart1%x-cart2%x
  cart3%y=cart1%y-cart2%y
  cart3%z=cart1%z-cart2%z
end function cart3D_minus_cart3D

! auxiliary for drawing

subroutine CHGLDrawVector(r,rt,scal,dmax,iCx,iCy)
! draw vectors
  Implicit None
  Real(8) r(3),rt(3),r1(3),r2(3),d,dmax,scal
  Real(8) spac(3,0:3)
  Integer(2) iCx,iCy
  d=min(dmax,scal*sqrt(1.0e-30+rt(1)**2+rt(2)**2+rt(3)**2))
  if(d.gt.sqrt(1.0e-30)) then
    spac=0.0d0
    spac(1:3,1)=rt(1:3)
    spac(1,2)=1.0d0
    spac(2,3)=1.0d0
    call Ortho3DSpace(spac)
    r1(1:3)=d*spac(1:3,1)
    call CHGLSetColorI(iCx)
    call glBegin(GL_TRIANGLES)
    call SetVertex(1.0d0,r(1)+r1(1),r(2)+r1(2),r(3)+r1(3))
    r2(1:3)=0.25*d*spac(1:3,2)
    call SetVertex(1.0d0,r(1)+r2(1),r(2)+r2(2),r(3)+r2(3))
    call SetVertex(1.0d0,r(1)-r2(1),r(2)-r2(2),r(3)-r2(3))
    call glEnd()
    call CHGLSetColorI(iCy)
    call glBegin(GL_TRIANGLES)
    call SetVertex(1.0d0,r(1)+r1(1),r(2)+r1(2),r(3)+r1(3))
    r2(1:3)=0.25*d*spac(1:3,3)
    call SetVertex(1.0d0,r(1)+r2(1),r(2)+r2(2),r(3)+r2(3))
    call SetVertex(1.0d0,r(1)-r2(1),r(2)-r2(2),r(3)-r2(3))
    call glEnd()
  end if
end subroutine CHGLDrawVector

subroutine CHGLDrawExpansion(spac,iC,d)
! draw vectors
  Implicit None
  Real(8) spac(3,0:3),d
  Integer(2) iC
  if(d.gt.1.0d-100) then
    call glLineWidth(1.0_glFloat)
    if(iC.lt.0) call glLineWidth(2.0_glFloat)
    if(iC.ne.0) call CHGLSetColorI(abs(iC))
    if(iC.eq.0) call CHGLSetColorI(iCHGLxAxisColor)
    call glBegin(GL_LINES)
    call SetVertex(1.0d0,spac(1,0),spac(2,0),spac(3,0))
    call SetVertex(1.0d0,spac(1,0)+d*spac(1,1),spac(2,0)+d*spac(2,1),spac(3,0)+d*spac(3,1))
    call glEnd()
    if(iC.eq.0) call CHGLSetColorI(iCHGLyAxisColor)
    call glBegin(GL_LINES)
    call SetVertex(1.0d0,spac(1,0),spac(2,0),spac(3,0))
    call SetVertex(1.0d0,spac(1,0)+d*spac(1,2),spac(2,0)+d*spac(2,2),spac(3,0)+d*spac(3,2))
    call glEnd()
    if(iC.eq.0) call CHGLSetColorI(iCHGLzAxisColor)
    call glBegin(GL_LINES)
    call SetVertex(1.0d0,spac(1,0),spac(2,0),spac(3,0))
    call SetVertex(1.0d0,spac(1,0)+d*spac(1,3),spac(2,0)+d*spac(2,3),spac(3,0)+d*spac(3,3))
    call glEnd()
    call glLineWidth(1.0_glFloat)
  end if
end subroutine CHGLDrawExpansion

subroutine CHGLDrawTube(tube,nx,ny,iC,lInvers)
! draw Tube
  Implicit None
  Integer(4) nx,ny,i,k
  Real(8) Tube(3,0:ny,nx)
  Integer(2) iC(3)
  Logical lInvers
  do k=1,nx
    call CHGLSetColorI(iC(1))
    call glBegin(GL_LINE_STRIP)
    do i=0,ny
      call SetVertex(1.0d0,Tube(1,i,k),Tube(2,i,k),Tube(3,i,k))
    end do
    call glEnd()
  end do
  do i=0,ny
    call CHGLSetColorI(iC(1))
    call glBegin(GL_LINE_LOOP)
    do k=1,nx
      call SetVertex(1.0d0,Tube(1,i,k),Tube(2,i,k),Tube(3,i,k))
    end do
    call glEnd()
  end do
  call CHGLDrawTubeA(tube,nx,ny,iC,lInvers)
end subroutine CHGLDrawTube

subroutine CHGLDrawTubeA(tube,nx,ny,iC,lInvers)
! draw Tube
  Implicit None
  Integer(4) nx,ny,i,k
  Integer(2) iC(3)
  Logical lInvers
  Real(8) Tube(3,0:ny,nx)
  do k=1,nx-1
    if(lInvers) then
      call CHGLSetColorI(iC(3),iC(2))
    else
      call CHGLSetColorI(iC(2),iC(3))
    end if
    call glBegin(GL_TRIANGLE_STRIP)
      do i=0,ny
        call SetVertex(1.0d0,Tube(1,i,k),Tube(2,i,k),Tube(3,i,k))
        call SetVertex(1.0d0,Tube(1,i,k+1),Tube(2,i,k+1),Tube(3,i,k+1))
      end do
    call glEnd()
  end do
  if(lInvers) then
    call CHGLSetColorI(iC(3),iC(2))
  else
    call CHGLSetColorI(iC(2),iC(3))
  end if
  call glBegin(GL_TRIANGLE_STRIP)
  do i=0,ny
    call SetVertex(1.0d0,Tube(1,i,nx),Tube(2,i,nx),Tube(3,i,nx))
    call SetVertex(1.0d0,Tube(1,i,1),Tube(2,i,1),Tube(3,i,1))
  end do
  call glEnd()
end subroutine CHGLDrawTubeA

subroutine DrawIsoIn3(rA0,rB0,rC0,fA0,fB0,fC0,fmin0,fmax0,df)
! draw iso lines in a triangle
  Implicit none
  Real(8) rA0(3),rB0(3),rC0(3),fA0,fB0,fC0,rA(3),rB(3),rC(3),fA,fB,fC,fmin0,fmax0,fmin,fmax,df,f,r(3),dfIso
  Integer(4) i,i1,i2
  Logical lLogScale
  !!! if(lStopThread) return
  fMin=fMin0
  fMax=fMax0
  if(fMin.gt.fMax) then
    fMin=fMax0
    fMax=fMin0
  end if
  dfIso=dabs(df)
  lLogScale=.false.
  if((df.lt.0.0d0).and.(fMin.gt.0).and.(fMax.gt.0)) then
    lLogScale=.true.
    if(dfIso.lt.1.0d0) dfIso=1.0d0/dfIso
    f=fMin/dfIso
    i1=0
    i2=min(1000,idInt(dlog(fMax/fMin)/dlog(dfIso))+1)
    fMax=fMin*(dfIso**i2)
  end if
  if((fA0.ge.fB0).and.(fA0.ge.fC0)) then
    rA=rA0
    fA=fA0
    if(fB0.ge.fC0) then
      rB=rB0
      fB=fB0
      rC=rC0
      fC=fC0
    else
      rB=rC0
      fB=fC0
      rC=rB0
      fC=fB0
    end if
  else if((fB0.ge.fA0).and.(fB0.ge.fC0)) then
    rA=rB0
    fA=fB0
    if(fA0.ge.fC0) then
      rB=rA0
      fB=fA0
      rC=rC0
      fC=fC0
    else
      rB=rC0
      fB=fC0
      rC=rA0
      fC=fA0
    end if
  else
    rA=rC0
    fA=fC0
    if(fA0.ge.fB0) then
      rB=rA0
      fB=fA0
      rC=rB0
      fC=fB0
    else
      rB=rB0
      fB=fB0
      rC=rA0
      fC=fA0
    end if
  end if
  if(lLogScale) then
    do i=i1,i2
      !!! if(lStopThread) Exit
      f=f*dfIso
      if(f.gt.fA) Exit
      if(f.lt.fC) Cycle
      r=((fA-f)*rC+(f-fC)*rA)/(fA-fC)
      call SetVertex(1.0d0,r(1),r(2),r(3))
      if(f.ge.fB) then
        r=((fA-f)*rB+(f-fB)*rA)/(fA-fB)
      else
        r=((fB-f)*rC+(f-fC)*rB)/(fB-fC)
      end if
      call SetVertex(1.0d0,r(1),r(2),r(3))
    end do
  else
    do while ((fmax-fmin).gt.1000.0d0*df)
      df=10.0d0*df
    end do
    do f=fmin,fmax,df
      !!! if(lStopThread) Exit
      if(f.gt.fA) Exit
      if(f.lt.fC) Cycle
      r=((fA-f)*rC+(f-fC)*rA)/(fA-fC)
      call SetVertex(1.0d0,r(1),r(2),r(3))
      if(f.ge.fB) then
        r=((fA-f)*rB+(f-fB)*rA)/(fA-fB)
      else
        r=((fB-f)*rC+(f-fC)*rB)/(fB-fC)
      end if
      call SetVertex(1.0d0,r(1),r(2),r(3))
    end do
  end if
end subroutine DrawIsoIn3

subroutine SetVertex(s,x,y,z)
  Implicit none
  real(kind=gldouble), dimension(3) :: r
  real(8) s,x,y,z
  r(1)=s*x
  r(2)=s*y
  r(3)=s*z
  call glVertex3dv(r)
end subroutine SetVertex

! movies

  Subroutine CHGLMovie
! generate OpenGL movie
  Implicit none
    Include 'resource.fd'
    Integer(4) k,iErr2,idum,lout,kM
    Logical ldum
    call glutSetCursor(GLUT_CURSOR_WAIT)
    call OutTxt('t3','OpenGL movie'C)
    lOGLavi=.true.
    lRestart=.true.
    idum=GetDlgItemText(outputdlg%hwnd,IDC_OUT_TEXT4,Mov_Commands,lsMovCommands)
    ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.false.)
    do while(lRestart)
      lRestart=.false.
! check directives and find the end mark positions
      call chkDir(iErr2)
      if(iErr2.ne.0) return
! generate the movie
      idum=0
      kM=kMovCommand
      kMovCommand=0
      do
        kMovCommand=kMovCommand+1
        if(kMovCommand.gt.nMovCommand) Exit
        call GetStrInStr(Mov_Commands,lsMovCommands-1,kMovCommand,Mov_Command,lsMovComm)
        if(lsMovComm.lt.1) exit
        call OutTxt('t3','A:'//Mov_Command(1:lsMovComm)//Char(0))
        call IntToStr(Int4(kMovCommand),0,0,SpaceText,lout)
        call OutTxt('n3',SpaceText(1:lout))
        call IntToStr(nMovCommand,0,0,SpaceText,lout)
        call OutTxt('m3',SpaceText(1:lout))
        k=kMovCommand
        call MovieCommand(.true.,k,idum)
        if(k.ne.kMovCommand) kMovCommand=k-1 ! jump to kMovCommand=k (+1 will be added)
        if(lRestart.or.lStopThread.or.(idum.gt.0).or.(idum.eq.-99)) Exit
        if(lDrawOGL) call CHGLDraw()
        if(lSaveOGLavi) call ChGLsaveBMP()
        if(lSaveOGLbmp) call ChGLsaveBMP(BmpFileName)
      end do
    end do
    kMovCommand=kM
    call OutTxt('t3','End OpenGL movie'C)
    ldum=EnableDlgItem(outputdlg%hwnd,IDC_OUT_TEXT4,.true.)
    lOGLavi=.false.
    call glutSetCursor(GLUT_CURSOR_RIGHT_ARROW)
  end Subroutine CHGLMovie

  Subroutine CHGLDraw()
    Logical ldo
    call glutSetCursor(GLUT_CURSOR_WAIT)
    if(lCHGLdoubleSide) then ! this was the usual way that does not work for field and error plots since Windows Vista...
      call glEnable(GL_LIGHTING)
    else
      call glDisable(GL_LIGHTING)
    end if
    call OutTxt('t2','OGL real field'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    call GetrField(.false.)
    call OutTxt('t2','OGL draw objects'C)
    call OutTxt('n2',' 'C)
    call OutTxt('m2',' 'C)
    !!! call SleepQQ(1_4)
    ldo=lDrawOGL
    lDrawOGL=.true.
    call DrawObject(iDraOBJ)
    call CHGLDefaults(.false.)
    call CHGLDrawAxes()
    !!! call SleepQQ(1_4)
    call CHGLDrawSurfaces()
    call CHGLDrawGrids()
    call CHGLDrawIsoLines()
    call CHGLDrawVectors()
    call CHGLDrawExpansions()
    call CHGLDrawTubes()
    call CHGLDrawPFDsensors()
    call CHGLDrawPFDsources()
    call ChGLdisplay
    lDrawOGL=ldo
    call glutSetCursor(GLUT_CURSOR_RIGHT_ARROW)
  end Subroutine CHGLDraw

END MODULE CHoGL
