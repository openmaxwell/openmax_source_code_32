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
MODULE CHGRA

! Graphics

  USE CHBMP

  SAVE

  CONTAINS

! 3D objects in the xy plane

  Subroutine Draw23DX(x,y,d)
    Implicit none
    Real(8) x,y,d
    call Draw23DLine(x-d,y-d,x+d,y+d)
    call Draw23DLine(x-d,y+d,x+d,y-d)
  end Subroutine Draw23DX

  Subroutine Draw23DP(x,y,d)
    Implicit none
    Real(8) x,y,d
    call Draw23DLine(x-d,y,x+d,y)
    call Draw23DLine(x,y+d,x,y-d)
  end Subroutine Draw23DP

  Subroutine Draw23DQ(x,y,d)
    Implicit none
    Real(8) x,y,d
    call Draw23DLine(x-d,y-d,x-d,y+d)
    call Draw23DLine(x-d,y+d,x+d,y+d)
    call Draw23DLine(x+d,y+d,x+d,y-d)
    call Draw23DLine(x+d,y-d,x-d,y-d)
  end Subroutine Draw23DQ

  Subroutine Draw23DO(x0,y0,r)
    Implicit none
    Real(8) rp(3),x0,y0,x,y,r,f
    Integer(2) i,idum
    f=0.0d0
    do i=1,8
      rp(1)=x0+r*dcos(f)
      rp(2)=y0+r*dsin(f)
      rp(3)=0.0d0
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(8)
    end do
    idum=polygon($GBorder,Poly,8)
  end Subroutine Draw23DO

  Subroutine Draw23DV(x,y,tx,ty)
    Implicit none
    Real(8) x,y,tx,ty
    call Draw23DLine(x,y,x-tx+0.5d0*ty,y-ty-0.5d0*tx)
    call Draw23DLine(x,y,x-tx-0.5d0*ty,y-ty+0.5d0*tx)
  end Subroutine Draw23DV

  Subroutine Draw23DLine(x1,y1,x2,y2)
    Implicit none
    Real(8) x1,y1,x2,y2,ra(3),rb(3)
    ra(1)=x1
    ra(2)=y1
    ra(3)=0.0d0
    rb(1)=x2
    rb(2)=y2
    rb(3)=0.0d0
    call Draw3DLine(ra,rb)
  end Subroutine Draw23DLine

  Subroutine Draw23DLineM(x1,y1,x2,y2,dMark)
    Implicit none
    Real(8) x1,y1,x2,y2,dMark,x,y,v(2)
	  call Draw23DLine(x1,y1,x2,y2)
    if(lGRF_Mark.and.(dMark.gt.pSmall)) then
      call Draw23DQ(x1,y1,dMark)
      call Draw23DQ(x2,y2,dMark)
      x=0.5d0*(x1+x2)
      y=0.5d0*(y1+y2)
      v(1)=(x2-x1)
      v(2)=(y2-y1)
      call Unit2DV(v)
      v=dMark*v
      call Draw23DV(x,y,v(1),v(2))
    end if
  end Subroutine Draw23DLineM

  Subroutine Draw23DCircle(x0,y0,r)
    Implicit none
    Real(8) rp(3),x0,y0,r,f,x,y
    Integer(4) i
    Integer(2) idum
    f=0.0d0
    rp(3)=0.0d0
    do i=1,mGraPoly
      rp(1)=x0+r*dcos(f)
      rp(2)=y0+r*dsin(f)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(mGraPoly)
    end do
    idum=polygon($GBorder,Poly,mGraPoly)
  end Subroutine Draw23DCircle

  Subroutine Draw23DCircleM(x0,y0,r,dMark)
    Implicit none
    Real(8) x0,y0,r,dMark
    call Draw23DCircle(x0,y0,r)
    if(lGRF_Mark.and.(dMark.gt.pSmall)) then
      call Draw23DO(x0,y0,dMark)
      call Draw23DQ(x0+r,y0,dMark)
      call Draw23DLine(x0,y0,x0+r,y0)
      call Draw23DV(x0-r,y0,0.0d0,-dMark)
    end if
  end Subroutine Draw23DCircleM

  Subroutine Draw23DArc(x0,y0,x1,y1,x2,y2)
    Implicit none
    Real(8) rp(3),x0,y0,x1,y1,x2,y2,v(2),r,f,f1,f2,df,x,y
    Integer(4) i
    Integer(2) idum,ix,iy
	  Type(xycoord) xydum
    v(1)=x1-x0
    v(2)=y1-y0
    r=r2Vec_Length(v)
    f1=Protected_Ata(v(2),v(1))
    f2=Protected_Ata(y2-y0,x2-x0)
    if(f2.lt.f1) f2=f2+2.0d0*Pi
    df=(f2-f1)/Dble(mGraPoly-1)
    f=f1
    i=1
    rp(3)=0.0d0
    if(lGRF_Mark) then
      rp(1)=x0
      rp(2)=y0
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      do i=2,mGraPoly-1
        rp(1)=x0+r*dcos(f)
        rp(2)=y0+r*dsin(f)
        call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
        call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
        f=f+df
      end do
      rp(1)=x0+r*dcos(f2)
      rp(2)=y0+r*dsin(f2)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(mGraPoly).xcoord,Poly(mGraPoly).ycoord)
      idum=polygon($GBorder,Poly,mGraPoly)
    else
      rp(1)=x0+r*dcos(f)
      rp(2)=y0+r*dsin(f)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,ix,iy)
      call MoveTo(ix,iy,xydum)
      f=f+df
      do i=2,mGraPoly-1
        rp(1)=x0+r*dcos(f)
        rp(2)=y0+r*dsin(f)
        call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
        call R2I(x,y,ix,iy)
        idum=LineTo(ix,iy)
        f=f+df
      end do
      rp(1)=x0+r*dcos(f2)
      rp(2)=y0+r*dsin(f2)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,ix,iy)
      idum=LineTo(ix,iy)
    end if
  end Subroutine Draw23DArc

  Subroutine Draw23DArcM(x0,y0,x1,y1,x2,y2,dMark)
    Implicit none
    Real(8) x0,y0,x1,y1,x2,y2,dMark
    call Draw23DArc(x0,y0,x1,y1,x2,y2)
    if(lGRF_Mark.and.(dMark.gt.pSmall)) then
      call Draw23DO(x0,y0,dMark)
      call Draw23DQ(x1,y1,dMark)
      call Draw23DQ(x2,y2,dMark)
    end if
  end Subroutine Draw23DArcM

  Subroutine Draw23DRectangle(x1,y1,x2,y2,lFill)
    Implicit none
    Real(8) rp(3),x1,y1,x2,y2,x,y
    Integer(2) ix1,iy1,ix2,iy2
    Logical, intent(in) :: lFill
    rp(3)=0.0d0
    rp(1)=x1
    rp(2)=y1
    call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
    call R2I(x,y,ix1,iy1)
    rp(1)=x2
    rp(2)=y2
    call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
    call R2I(x,y,ix2,iy2)
    call DrawIRectangle(ix1,iy1,ix2,iy2,lFill)
  end Subroutine Draw23DRectangle

  Subroutine Draw23DPolygon(x,y,np,lFill,icB,icI)
    Implicit none
    Real(8) rp(3),xi,yi,x(*),y(*)
    Integer(2) n,np,i,icB,icI
    Logical, intent(in) :: lFill
    n=Min(mGraPoly,np)
    rp(3)=0.0d0
    do i=1,n
      rp(1)=x(i)
      rp(2)=y(i)
      call Proj3D(rp,ViewPlane,rEye,viewDist,xi,yi)
      call R2I(xi,yi,ixPoly(i),iyPoly(i))
    end do
    call DrawIPolygon(ixPoly,iyPoly,n,lFill,icB,icI)
  end Subroutine Draw23DPolygon

  Subroutine Draw23DArrow(x1,y1,x2,y2,w,iType,lFill,icB,icI)
    Implicit none
    Real(8) rp(3),x1p,y1p,x2p,y2p,x1,y1,x2,y2,w
    Integer(2) icB,icI,iType
    Logical, intent(in) :: lFill
    rp(1)=x1
    rp(2)=y1
    rp(3)=0.0d0
    call Proj3D(rp,ViewPlane,rEye,viewDist,x1p,y1p)
    rp(1)=x2
    rp(2)=y2
    call Proj3D(rp,ViewPlane,rEye,viewDist,x2p,y2p)
    call DrawArrow(x1p,y1p,x2p,y2p,w,iType,lFill,icB,icI)
  end Subroutine Draw23DArrow

! pure 3D objects

  Subroutine Draw3DC(r,d)
! draw lines of a 3D cube of side length d around point r
    Implicit none
    Real(8) r(3),d,ra(3),rb(3)
! lower square
    ra(1)=r(1)-0.5d0*d
    ra(2)=r(2)-0.5d0*d
    ra(3)=r(3)-0.5d0*d
    rb=ra
    rb(1)=rb(1)+d
    call Draw3DLine(ra,rb)
    ra(1)=rb(1)
    rb(2)=rb(2)+d
    call Draw3DLine(ra,rb)
    ra(2)=rb(2)
    rb(1)=rb(1)-d
    call Draw3DLine(ra,rb)
    ra(1)=rb(1)
    rb(2)=rb(2)-d
    call Draw3DLine(ra,rb)
    ra(2)=rb(2)
! upper square
    ra(3)=ra(3)+d
    rb=ra
    rb(1)=rb(1)+d
    call Draw3DLine(ra,rb)
    ra(1)=rb(1)
    rb(2)=rb(2)+d
    call Draw3DLine(ra,rb)
    ra(2)=rb(2)
    rb(1)=rb(1)-d
    call Draw3DLine(ra,rb)
    ra(1)=rb(1)
    rb(2)=rb(2)-d
    call Draw3DLine(ra,rb)
    ra(2)=rb(2)
! vertical lines
    rb(3)=ra(3)-d
    call Draw3DLine(ra,rb)
    ra(1)=ra(1)+d
    rb(1)=rb(1)+d
    call Draw3DLine(ra,rb)
    ra(2)=ra(2)+d
    rb(2)=rb(2)+d
    call Draw3DLine(ra,rb)
    ra(1)=ra(1)-d
    rb(1)=rb(1)-d
    call Draw3DLine(ra,rb)
  end Subroutine Draw3DC

  Subroutine Draw3DX(r,d)
! draw lines - a 3D X at point r
    Implicit none
    Real(8) r(3),d,ra(3),rb(3)
    ra=r
    rb=r
    ra(1)=ra(1)-d
    ra(2)=ra(2)-d
    ra(3)=ra(3)-d
    rb(1)=rb(1)+d
    rb(2)=rb(2)+d
    rb(3)=rb(3)+d
    call Draw3DLine(ra,rb)
    ra=r
    rb=r
    ra(1)=ra(1)-d
    ra(2)=ra(2)+d
    ra(3)=ra(3)-d
    rb(1)=rb(1)+d
    rb(2)=rb(2)-d
    rb(3)=rb(3)+d
    call Draw3DLine(ra,rb)
    ra=r
    rb=r
    ra(1)=ra(1)+d
    ra(2)=ra(2)-d
    ra(3)=ra(3)-d
    rb(1)=rb(1)-d
    rb(2)=rb(2)+d
    rb(3)=rb(3)+d
    call Draw3DLine(ra,rb)
    ra=r
    rb=r
    ra(1)=ra(1)+d
    ra(2)=ra(2)+d
    ra(3)=ra(3)-d
    rb(1)=rb(1)-d
    rb(2)=rb(2)-d
    rb(3)=rb(3)+d
    call Draw3DLine(ra,rb)
  end Subroutine Draw3DX

  Subroutine Draw3DP(r,d)
! draw 3 lines - an alternative 3D X at point r
    Implicit none
    Real(8) r(3),d,ra(3),rb(3)
    ra=r
    rb=r
    ra(1)=ra(1)-d
    rb(1)=rb(1)+d
    call Draw3DLine(ra,rb)
    ra=r
    rb=r
    ra(2)=ra(2)-d
    rb(2)=rb(2)+d
    call Draw3DLine(ra,rb)
    ra=r
    rb=r
    ra(3)=ra(3)-d
    rb(3)=rb(3)+d
    call Draw3DLine(ra,rb)
  end Subroutine Draw3DP

  Subroutine Draw3DQ(r,d)
! draw a cube with center point r, side length d
    Implicit none
    Real(8) r(3),d,ra(3),rb(3),d2
    d2=2.0d0*d
! square in xy direction at z=-d
    ra(1)=r(1)-d 
    ra(2)=r(2)-d
    ra(3)=r(3)-d
    rb=ra
    rb(1)=rb(1)+d2
    call Draw3DLine(ra,rb)
    ra=rb
    rb(2)=rb(2)+d2
    call Draw3DLine(ra,rb)
    ra=rb
    rb(1)=rb(1)-d2
    call Draw3DLine(ra,rb)
    ra=rb
    rb(2)=rb(2)-d2
    call Draw3DLine(ra,rb)
! line in z direction at x=-d, y=-d
    ra=rb
    rb(3)=rb(3)+d2
    call Draw3DLine(ra,rb)
! square in xy direction at z=+d
    ra=rb
    rb(1)=rb(1)+d2
    call Draw3DLine(ra,rb)
    ra=rb
    rb(2)=rb(2)+d2
    call Draw3DLine(ra,rb)
    ra=rb
    rb(1)=rb(1)-d2
    call Draw3DLine(ra,rb)
    ra=rb
    rb(2)=rb(2)-d2
    call Draw3DLine(ra,rb)
! remaining lines in z direction
    rb=ra
    rb(3)=rb(3)-d2
    call Draw3DLine(ra,rb)
    ra(1)=ra(1)+d2
    rb(1)=rb(1)+d2
    call Draw3DLine(ra,rb)
    ra(2)=ra(2)-d2
    rb(2)=rb(2)-d2
    call Draw3DLine(ra,rb)
  end Subroutine Draw3DQ

  Subroutine Draw3DO(ro,r)
! draw 3 circles rith radius r around ro, approximate with 8 corners
    Implicit none
    Real(8) ro(3),rp(3),x,y,r,f
    Integer(2) i,idum
! O in xy plane
    f=0.0d0
    do i=1,8
      rp(1)=ro(1)+r*dcos(f)
      rp(2)=ro(2)+r*dsin(f)
      rp(3)=ro(3)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(8)
    end do
    idum=polygon($GBorder,Poly,8)
! O in xz plane
    f=0.0d0
    do i=1,8
      rp(1)=ro(1)+r*dcos(f)
      rp(2)=ro(2)
      rp(3)=ro(3)+r*dsin(f)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(8)
    end do
    idum=polygon($GBorder,Poly,8)
! O in yz plane
    f=0.0d0
    do i=1,8
      rp(1)=ro(1)
      rp(2)=ro(2)+r*dcos(f)
      rp(3)=ro(3)+r*dsin(f)
      call Proj3D(rp,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(8)
    end do
    idum=polygon($GBorder,Poly,8)
  end Subroutine Draw3DO

  Subroutine Blind3DPoint(r,ix,iy)
    Implicit none
    Real(8) r(3),xp,yp
    Integer(2) ix,iy
    call Proj3D(r,ViewPlane,rEye,viewDist,xp,yp)
    call R2I(xp,yp,ix,iy)
  end Subroutine Blind3DPoint

  Subroutine Draw3DLine(ra,rb)
    Implicit none
    Real(8) ra(3),rb(3),x,y
    Integer(4) idum
    Integer(2) ix,iy
	  Type(xycoord) xydum
    call Proj3D(ra,ViewPlane,rEye,viewDist,x,y)
    call R2I(x,y,ix,iy)
    call MoveTo(ix,iy,xydum)
    call Proj3D(rb,ViewPlane,rEye,viewDist,x,y)
    call R2I(x,y,ix,iy)
    idum=LineTo(ix,iy)
  end Subroutine Draw3DLine

  Subroutine Draw3DPolyLine(Line,n,icl)
    Implicit none
    Integer(4) n,idum,i
    Integer(2) icl,ic,ix,iy
    Real(8) Line(3,n),ra(3),rb(3),x,y
	  Type(xycoord) xydum
    if(n.lt.2) return
    ic=SetColor(icl)
    ra(1:3)=Line(1:3,1)
    call Proj3D(ra,ViewPlane,rEye,viewDist,x,y)
    call R2I(x,y,ix,iy)
    call MoveTo(ix,iy,xydum)
    do i=2,n
      rb(1:3)=Line(1:3,i)
      call Proj3D(rb,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,ix,iy)
      idum=LineTo(ix,iy)
      ra=rb
    end do
    ic=SetColor(ic)
  end Subroutine Draw3DPolyLine

  Subroutine Draw3DPolyLineC(Line,n,icl)
    Implicit none
    Integer(4) n,i,idum
    Integer(2) icl,ic,ix,iy,ix0,iy0
    Real(8) Line(3,n),ra(3),rb(3),x,y
	  Type(xycoord) xydum
    if(n.lt.2) return
    ic=SetColor(icl)
    ra(1:3)=Line(1:3,1)
    call Proj3D(ra,ViewPlane,rEye,viewDist,x,y)
    call R2I(x,y,ix0,iy0)
    call MoveTo(ix0,iy0,xydum)
    do i=2,n
      rb(1:3)=Line(1:3,i)
      call Proj3D(rb,ViewPlane,rEye,viewDist,x,y)
      call R2I(x,y,ix,iy)
      idum=LineTo(ix,iy)
      ra=rb
    end do
    idum=LineTo(ix0,iy0)
    ic=SetColor(ic)
  end Subroutine Draw3DPolyLineC

  Subroutine Draw3DPolygon(r,np,lFill,icB,icI)
    Implicit none
    Integer(2) n,np,i,icB,icI
    Real(8) r(3,*)
    Logical, intent(in) :: lFill
    n=Min(mGraPoly,np)
    do i=1,n
      call Proj3D(r(1:3,i),ViewPlane,rEye,viewDist,xPoly(i),yPoly(i))
    end do
    call DrawPolygon(xPoly,yPoly,n,lFill,icB,icI)
  end Subroutine Draw3DPolygon

  Subroutine Draw3DArc(p,d,f1,f2,np,lFill,icB,icI)
    Implicit none
    Integer(2) np,n,icB,icI,i
    Real(8) p(1:3,0:3),r(3),d,f1,f2,f,df
    Logical, intent(in) :: lFill
    Logical ldum
    n=Max(2,Min(mGraPoly,np))
    df=(f2-f1)*Pi/(Dble(n-1)*180.0d0)
    f=f1*Pi/180.0d0
    do i=1,n
      r(1:3)=p(1:3,0)+d*dcos(f)*p(1:3,1)+d*dsin(f)*p(1:3,2)
      call Proj3D(r,ViewPlane,rEye,viewDist,xPoly(i),yPoly(i))
      f=f+df
    end do
    ldum=lGRF_PolygonClosed
    lGRF_PolygonClosed=.false.
    call DrawPolygon(xPoly,yPoly,n,lFill,icB,icI)
    lGRF_PolygonClosed=ldum
  end Subroutine Draw3DArc

  Subroutine Draw3DSpiral(Plane,O,e,P,drm,dfm,dzm,np,lFill,icB,icI)
    Implicit none
    Real(8) Plane(1:3,0:3),O(3),e(3),P(3),drm,dfm,dzm
    Real(8), Allocatable :: Poly(:,:)
    Integer(2) np,n,i,icB,icI
    Integer(4) iErr,idum
    Logical, intent(in) :: lFill
    Logical ldum
    n=Max(2_2,Min(mGraPoly,np))
    if(Allocated(Poly)) DeAllocate(Poly)
    Allocate(Poly(3,n),Stat=iErr)
    if(iErr.ne.0) then
      idum=MessageBoxQQ('Cannot allocate buffer!'C,'Draw Spiral'C, &
                        MB$OK.or.MB$ICONSTOP)
		else
      call Pt2Spiral(P,O,e,drm,dfm,dzm,Int4(n),Plane,Poly)
      do i=1,n
        call Proj3D(Poly(1:3,i),ViewPlane,rEye,viewDist,xPoly(i),yPoly(i))
      end do
      DeAllocate(Poly)
      ldum=lGRF_PolygonClosed
      lGRF_PolygonClosed=.false.
      call DrawPolygon(xPoly,yPoly,n,lFill,icB,icI)
      lGRF_PolygonClosed=ldum
    end if
  end Subroutine Draw3DSpiral

  Subroutine Draw3DArrow(r1,r2,w,iType,icB,icI,lFill)
    Implicit none
    Real(8) r1(3),r2(3),x1p,y1p,x2p,y2p,w,A(3),B(3),C(3),D(3),E(3),F(3)
    Integer(2) icB,icI,iType
    Logical, intent(in) :: lFill
    if(iType.lt.1_2) return
    if(lGRCallocated) then
      iGRCcolor=abs(icI)
      iGRCcolorB=icB
      lGRCfill=lFill
      if((iType.lt.2_2).or.(abs(w).lt.pSmall)) then
        call GRCDraw3DLine(r1,r2)
      else
        call GRCDraw3DLine(r1,r2)
        C=Unit3DVec(r2-r1)
        E=Unit3DVec(rEye(1:3)-ViewPlane(1:3,0))
        F=r3Vec_Prod(E,C)
        E=Unit3DVec(F)
        if(iType.lt.3_2) then
          A=r1+0.5d0*w*E
          B=r1-0.5d0*w*E
        else
          D=0.5d0*(r1+r2)
          A=D+0.5d0*w*E
          B=D-0.5d0*w*E
        end if
        call GRCDraw3DTriangle(A,B,r2,.true.,.true.,.true.)
        F=r3Vec_Prod(E,C)
        E=Unit3DVec(F)
        if(iType.lt.3_2) then
          A=r1+0.5d0*w*E
          B=r1-0.5d0*w*E
        else
          A=D+0.5d0*w*E
          B=D-0.5d0*w*E
        end if
        call GRCDraw3DTriangle(A,B,r2,.true.,.true.,.true.)
      end if
    else
      call Proj3D(r1,ViewPlane,rEye,viewDist,x1p,y1p)
      call Proj3D(r2,ViewPlane,rEye,viewDist,x2p,y2p)
      call DrawArrow(x1p,y1p,x2p,y2p,w,iType,lFill,icB,icI)
    end if
  end Subroutine Draw3DArrow

  Subroutine Draw3DIsoCell(r,f,dfIsoStep,fMin0,fMax0,MinC,MaxC,lFill,lIso,lBord,iCBord)
    Implicit none
    Real(8) r(3,4),r0(3,4),rp(3,5),rt1(3,3),rt2(3,3),ft1(3),ft2(3),dt1,dt2, &
    &       f(4),f0(4),f0Min,f0Max,dfIsoStep,dfIso,fIso,fMin,fMax,fMin0,fMax0,w
    Integer(4) i,i1,i2
    Integer(2) MinC,MaxC,ic,iMin,iMax,iCBord
    Logical, intent(in) :: lFill,lIso,lBord
    Logical lLogScale,lt
! avoid illegal input
    fMin=fMin0
    fMax=fMax0
    if(fMin.gt.fMax) then
      fMin=fMax0
      fMax=fMin0
    end if
    dfIso=dabs(dfIsoStep)
    lLogScale=.false.
    if((dfIsoStep.lt.0.0d0).and.(fMin.gt.0).and.(fMax.gt.0)) then
      lLogScale=.true.
      if(dfIso.lt.1.0d0) dfIso=1.0d0/dfIso
      fIso=fMin/dfIso
      i1=0
      i2=idInt(dlog(fMax/fMin)/dlog(dfIso))+1
      fMax=fMin*(dfIso**i2)
    end if
! locate corners with min. and max. values
    iMin=1
    iMax=1
    f0Min=f(1)
    f0Max=f(1)
    do i=2,4
      if(f(i).gt.f0Max) then
        iMax=i
        f0Max=f(i)
      else if(f(i).lt.f0Min) then
        iMin=i
        f0Min=f(i)
      end if
    end do
! draw immediately if entire cell has either color minC or maxC
    if(f0Max.le.fMin) then
      ic=minC
      if(lBord) ic=iCBord
      if(lFill) call Draw3DPolygon(r(1:3,1:4),4_2,.true.,ic,minC)
      return
    end if
    if(f0Min.ge.fMax) then
      ic=maxC
      if(lBord) ic=iCBord
      if(lFill) call Draw3DPolygon(r(1:3,1:4),4_2,.true.,ic,maxC)
      return
    end if
    f0Max=Min(fMax,f0Max)
    f0Min=Max(fMin,f0Min)
! sort first corner: min. value
    r0(1:3,1)=r(1:3,iMin)
    f0(1)=f(iMin)
    i=Mod(iMin+1,4)
    if(i.lt.1) i=4
    r0(1:3,2)=r(1:3,i)
    f0(2)=f(i)
    i=Mod(iMin+2,4)
    if(i.lt.1) i=4
    r0(1:3,3)=r(1:3,i)
    f0(3)=f(i)
    i=Mod(iMin+3,4)
    if(i.lt.1) i=4
    r0(1:3,4)=r(1:3,i)
    f0(4)=f(i)
    iMax=Mod(iMax-iMin+5,4)
    if(iMax.lt.1) iMax=4
    lt=.false.
    if((f0(3).lt.f0(2)).and.(f0(3).lt.f0(4))) then ! draw two triangles
      lt=.true.
      dt1=r3Vec_Length((r0(1:3,2)-rEye(1:3)))
      dt2=r3Vec_Length((r0(1:3,4)-rEye(1:3)))
      if(dt2.gt.dt1) then
        rt1(1:3,1)=r0(1:3,1)
        rt1(1:3,2)=r0(1:3,2)
        rt1(1:3,3)=r0(1:3,3)
        ft1(1)=f0(1)
        ft1(2)=f0(2)
        ft1(3)=f0(3)
        rt2(1:3,1)=r0(1:3,1)
        rt2(1:3,2)=r0(1:3,3)
        rt2(1:3,3)=r0(1:3,4)
        ft2(1)=f0(1)
        ft2(2)=f0(3)
        ft2(3)=f0(4)
      else
        rt2(1:3,1)=r0(1:3,1)
        rt2(1:3,2)=r0(1:3,2)
        rt2(1:3,3)=r0(1:3,3)
        ft2(1)=f0(1)
        ft2(2)=f0(2)
        ft2(3)=f0(3)
        rt1(1:3,1)=r0(1:3,1)
        rt1(1:3,2)=r0(1:3,3)
        rt1(1:3,3)=r0(1:3,4)
        ft1(1)=f0(1)
        ft1(2)=f0(3)
        ft1(3)=f0(4)
      end if
    end if
! prepare iso-line loop
    if(.not.lLogScale) then
      w=f0Min/dfIso
      if(dabs(w).gt.1000000000) return
      i1=idInt(w)
      if(w.lt.0.0d0) i1=i1-1
      w=f0Max/dfIso
      if(dabs(w).gt.1000000000) return
      i2=idInt(w)
      if(w.lt.0.0d0) i2=i2-1
      if(i2.lt.i1) then
        i=i1
        i1=i2
        i2=i
      end if
      f0Min=dble(i1)*dfIso
      f0Max=dble(i2)*dfIso
      fIso=f0Min-dfIso
    end if
! iso-line loop
    if(lt) then
      call Draw3DPolygon(rt1(1:3,1:3),3_2,.true.,minC,minC) ! 1st triangle
      call Draw3DPolygon(rt2(1:3,1:3),3_2,.true.,minC,minC) ! 2nd triangle
    else
      call Draw3DPolygon(r0(1:3,1:4),4_2,.true.,minC,minC)
    end if
    do i=i1,i2
      if(lLogScale) then
        fIso=fIso*dfIso
        if(fIso.gt.(f0Max*dfIso)) Exit
        ic=Int2(in_Rinterval(fIso,fmin,fmax,Int4(minC),Int4(maxC),.true.))
      else
        fIso=fIso+dfIso
        ic=Int2(in_Rinterval(fIso,fmin,fmax,Int4(minC),Int4(maxC),.false.))
      end if
      if((f0(1).ge.fIso).and.lFill) then                  ! draw entire square
        if(lt) then
          call Draw3DPolygon(rt1(1:3,1:3),3_2,.true.,ic,ic) ! 1st triangle
          call Draw3DPolygon(rt2(1:3,1:3),3_2,.true.,ic,ic) ! 2nd triangle
        else
          call Draw3DPolygon(r0(1:3,1:4),4_2,.true.,ic,ic)
        end if
        Cycle
      end if
      if(lt) then
        if(ft1(1).lt.fIso) then ! 1st triangle
          if(ft1(2).ge.fIso) then
            call getXYZ_in_Interval(rt1(1:3,1),rt1(1:3,2),ft1(1),ft1(2),fIso,rp(1:3,1))
            if(ft1(3).ge.fIso) then
              rp(1:3,2)=rt1(1:3,2)
              rp(1:3,3)=rt1(1:3,3)
              call getXYZ_in_Interval(rt1(1:3,1),rt1(1:3,3),ft1(1),ft1(3),fIso,rp(1:3,4))
              if(lFill) call Draw3DPolygon(rp(1:3,1:4),4_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,1),rp(1:3,4),0.0d0,1_2,1_2,1_2,.false.)
            else
              rp(1:3,2)=rt1(1:3,2)
              call getXYZ_in_Interval(rt1(1:3,2),rt1(1:3,3),ft1(2),ft1(3),fIso,rp(1:3,3))
              if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,1),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
            end if
          else
            if(ft1(3).ge.fIso) then
              call getXYZ_in_Interval(rt1(1:3,2),rt1(1:3,3),ft1(2),ft1(3),fIso,rp(1:3,1))
              rp(1:3,2)=rt1(1:3,3)
              call getXYZ_in_Interval(rt1(1:3,1),rt1(1:3,3),ft1(1),ft1(3),fIso,rp(1:3,3))
              if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,1),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
            end if
          end if
        end if
        if(ft2(1).lt.fIso) then ! 2nd triangle
          if(ft2(2).ge.fIso) then
            call getXYZ_in_Interval(rt2(1:3,1),rt2(1:3,2),ft2(1),ft2(2),fIso,rp(1:3,1))
            if(ft2(3).ge.fIso) then
              rp(1:3,2)=rt2(1:3,2)
              rp(1:3,3)=rt2(1:3,3)
              call getXYZ_in_Interval(rt2(1:3,1),rt2(1:3,3),ft2(1),ft2(3),fIso,rp(1:3,4))
              if(lFill) call Draw3DPolygon(rp(1:3,1:4),4_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,1),rp(1:3,4),0.0d0,1_2,1_2,1_2,.false.)
            else
              rp(1:3,2)=rt2(1:3,2)
              call getXYZ_in_Interval(rt2(1:3,2),rt2(1:3,3),ft2(2),ft2(3),fIso,rp(1:3,3))
              if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,1),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
            end if
          else
            if(ft2(3).ge.fIso) then
              call getXYZ_in_Interval(rt2(1:3,2),rt2(1:3,3),ft2(2),ft2(3),fIso,rp(1:3,1))
              rp(1:3,2)=rt2(1:3,3)
              call getXYZ_in_Interval(rt2(1:3,1),rt2(1:3,3),ft2(1),ft2(3),fIso,rp(1:3,3))
              if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,1),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
            end if
          end if
        end if
      else
        if(((f0(1).lt.fIso).and.(f0(2).ge.fIso)).or.((f0(1).ge.fIso).and.(f0(2).lt.fIso))) then        ! intersect line 1-2
          if(((f0(2).lt.fIso).and.(f0(3).ge.fIso)).or.((f0(2).ge.fIso).and.(f0(3).lt.fIso))) then      ! intersect line 2-3
            rp(1:3,1)=r0(1:3,2)                             ! 2. corner triangle
            call getXYZ_in_Interval(r0(1:3,1),r0(1:3,2),f0(1),f0(2),fIso,rp(1:3,2))
            call getXYZ_in_Interval(r0(1:3,2),r0(1:3,3),f0(2),f0(3),fIso,rp(1:3,3))
            if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
            if(lIso) call Draw3DArrow(rp(1:3,2),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
            if(((f0(3).lt.fIso).and.(f0(4).ge.fIso)).or.((f0(3).ge.fIso).and.(f0(4).lt.fIso))) then    ! intersect line 3-4
              rp(1:3,1)=r0(1:3,4)                           ! 4. corner triangle
              call getXYZ_in_Interval(r0(1:3,1),r0(1:3,4),f0(1),f0(4),fIso,rp(1:3,2))
              call getXYZ_in_Interval(r0(1:3,4),r0(1:3,3),f0(4),f0(3),fIso,rp(1:3,3))
              if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
              if(lIso) call Draw3DArrow(rp(1:3,2),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
            end if
          else if(((f0(3).lt.fIso).and.(f0(4).ge.fIso)).or.((f0(3).ge.fIso).and.(f0(4).lt.fIso))) then ! intersect line 3-4
            rp(1:3,1)=r0(1:3,2)                             ! 2.+3. corner quadrangle
            rp(1:3,2)=r0(1:3,3)
            call getXYZ_in_Interval(r0(1:3,4),r0(1:3,3),f0(4),f0(3),fIso,rp(1:3,3))
            call getXYZ_in_Interval(r0(1:3,1),r0(1:3,2),f0(1),f0(2),fIso,rp(1:3,4))
            if(lFill) call Draw3DPolygon(rp(1:3,1:4),4_2,.true.,ic,ic)
            if(lIso) call Draw3DArrow(rp(1:3,3),rp(1:3,4),0.0d0,1_2,1_2,1_2,.false.)
          else
            rp(1:3,1)=r0(1:3,2)                             ! 2.+3.+4. corner quintangle
            rp(1:3,2)=r0(1:3,3)
            rp(1:3,3)=r0(1:3,4)
            call getXYZ_in_Interval(r0(1:3,1),r0(1:3,4),f0(1),f0(4),fIso,rp(1:3,4))
            call getXYZ_in_Interval(r0(1:3,1),r0(1:3,2),f0(1),f0(2),fIso,rp(1:3,5))
            if(lFill) call Draw3DPolygon(rp(1:3,1:5),5_2,.true.,ic,ic)
            if(lIso) call Draw3DArrow(rp(1:3,4),rp(1:3,5),0.0d0,1_2,1_2,1_2,.false.)
          end if
        else if(((f0(2).lt.fIso).and.(f0(3).ge.fIso)).or.((f0(2).ge.fIso).and.(f0(3).lt.fIso))) then   ! intersect line 2-3
          if(((f0(3).lt.fIso).and.(f0(4).ge.fIso)).or.((f0(3).ge.fIso).and.(f0(4).lt.fIso))) then      ! intersect line 3-4
            rp(1:3,1)=r0(1:3,3)                             ! 3. corner triangle
            call getXYZ_in_Interval(r0(1:3,2),r0(1:3,3),f0(2),f0(3),fIso,rp(1:3,2))
            call getXYZ_in_Interval(r0(1:3,4),r0(1:3,3),f0(4),f0(3),fIso,rp(1:3,3))
            if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
            if(lIso) call Draw3DArrow(rp(1:3,2),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
          else
            rp(1:3,1)=r0(1:3,3)                             ! 3.+4. corner quadrangle
            rp(1:3,2)=r0(1:3,4)
            call getXYZ_in_Interval(r0(1:3,4),r0(1:3,1),f0(4),f0(1),fIso,rp(1:3,3))
            call getXYZ_in_Interval(r0(1:3,2),r0(1:3,3),f0(2),f0(3),fIso,rp(1:3,4))
            if(lFill) call Draw3DPolygon(rp(1:3,1:4),4_2,.true.,ic,ic)
            if(lIso) call Draw3DArrow(rp(1:3,3),rp(1:3,4),0.0d0,1_2,1_2,1_2,.false.)
          end if
        else if(((f0(3).lt.fIso).and.(f0(4).ge.fIso)).or.((f0(3).ge.fIso).and.(f0(4).lt.fIso))) then   ! intersect line 3-4
          rp(1:3,1)=r0(1:3,4)                               ! 4. corner triangle
          call getXYZ_in_Interval(r0(1:3,1),r0(1:3,4),f0(1),f0(4),fIso,rp(1:3,2))
          call getXYZ_in_Interval(r0(1:3,4),r0(1:3,3),f0(4),f0(3),fIso,rp(1:3,3))
          if(lFill) call Draw3DPolygon(rp(1:3,1:3),3_2,.true.,ic,ic)
          if(lIso) call Draw3DArrow(rp(1:3,2),rp(1:3,3),0.0d0,1_2,1_2,1_2,.false.)
        end if
      end if
    end do
! draw border
    if(lBord) call Draw3DPolygon(r0(1:3,1:4),4_2,.false.,iCBord,1_2)
  end Subroutine Draw3DIsoCell

  Subroutine Draw3DCell(r4,f4,iCBord,iCFill)
! draw a cell with 4 corners stored in r4 and derived field values stored in f4
! use border color iCBord
! if iCFill is present: iCFill>=0: fill cell with this color, ignore f4
!                       iCFill=-1: don't fill cell, ignore f4
    Implicit None
    Real(8) r4(3,4),f4(4),f(4),dum
    Integer(2) ic,icB,iCBord
    Integer(2), Optional :: iCFill
    Logical ldum,llog
    ic=-2_2
    if(Present(iCFill)) ic=iCFill
    llog=.false.
    if(rIsoStep.lt.0.0d0) llog=.true.
    lGRF_PolygonClosed=.true.
    if((ic.gt.-2_2).or.(abs(itIntensity).lt.3)) then
      if(abs(itIntensity).lt.2) then
        dum=scaleIntensity*f4(1)
      else
        dum=scaleIntensity*0.25d0*(f4(1)+f4(2)+f4(3)+f4(4))
      end if
      dum=Min(Max(dum,rMinFld),rMaxFld)
      icB=iCBord
      if(ic.lt.-1_2) ic=Int2(in_Rinterval(dum,rMinFld,rMaxFld,Int4(minCIntensity),Int4(maxCIntensity),llog))
      if(.not.lGrid3D) icB=ic
      if(icB.lt.0_2) return
      if(lGRCallocated) then
        iGRCcolor=abs(ic)
        iGRCcolorB=icB
        lGRCfill=.true.
        if(ic.lt.0_2) lGRCfill=.false.
        call GRCDraw3DTriangle(r4(1:3,1),r4(1:3,2),r4(1:3,3),.true.,.true.,.false.)
        call GRCDraw3DTriangle(r4(1:3,3),r4(1:3,4),r4(1:3,1),.true.,.true.,.false.)
      else
        ldum=lIsoFill
        if(ic.lt.0_2) ldum=.false.
        call Draw3DPolygon(r4(1:3,1:4),4_2,ldum,icB,abs(ic))
      end if
    else
      f(1:4)=scaleIntensity*f4(1:4)
      if(lGRCallocated.and.lGRCused) then
        iGRCcolor=abs(ic)
        iGRCcolorB=iCBord
        lGRCfill=lIsoFill
        call GRCDraw3DTriangle(r4(1:3,1),r4(1:3,2),r4(1:3,3),lGrid3D,lGrid3D,.false.,f(1),f(2),f(3),llog)
        call GRCDraw3DTriangle(r4(1:3,3),r4(1:3,4),r4(1:3,1),lGrid3D,lGrid3D,.false.,f(3),f(4),f(1),llog)
      else
        call Draw3DIsoCell(r4,f,rIsoStep,rMinFld,rMaxFld, &
        &                  minCIntensity,maxCIntensity,lIsoFill,lIsoLine,lGrid3D,iCBord)
      end if
    end if
  end Subroutine Draw3DCell

END MODULE CHGRA




