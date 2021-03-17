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
MODULE CHMAT

! mathematics and auxiliary things

  USE CHDLG
 
  Interface Operator (.inv.)
    Module Procedure Protected_Inv
    Module Procedure Protected_CInv
  end Interface

  Interface Operator (.int.)
    Module Procedure Protected_Int
    Module Procedure Protected_CInt
  end Interface

  Interface Operator (.sqr.)
    Module Procedure Protected_Sqr
    Module Procedure Protected_CSqr
  end Interface

  Interface Operator (.srt.)
    Module Procedure Protected_Srt
    Module Procedure Protected_CSrt
  end Interface

  Interface Operator (.log.)
    Module Procedure Protected_Log
    Module Procedure Protected_CLog
  end Interface

  Interface Operator (.exp.)
    Module Procedure Protected_Exp
    Module Procedure Protected_CExp
  end Interface

  Interface Operator (.cos.)
    Module Procedure Protected_Cos
    Module Procedure Protected_CCos
  end Interface

  Interface Operator (.sin.)
    Module Procedure Protected_Sin
    Module Procedure Protected_CSin
  end Interface

  Interface Operator (.add.)
    Module Procedure Protected_Add
    Module Procedure Protected_CAdd
  end Interface

  Interface Operator (.sub.)
    Module Procedure Protected_Sub
    Module Procedure Protected_CSub
  end Interface

  Interface Operator (.mul.)
    Module Procedure Protected_Mul
    Module Procedure Protected_CMul
  end Interface

  Interface Operator (.div.)
    Module Procedure Protected_Div
    Module Procedure Protected_CDiv
  end Interface

  Interface Operator (.pow.)
    Module Procedure Protected_Pow
    Module Procedure Protected_CPow
  end Interface

  Interface Operator (.ata.)
    Module Procedure Protected_Ata
  end Interface

  SAVE

  Real(8), Parameter :: Pi=3.1415926535898d0,Eps0=8.85418781762038985d-12,Mue0=1.25663706143591730d-6,Qe=1.60217656535d-19
  Integer(4) nzeile

! Formula

!DEC$ IF DEFINED (DEMO)
  Integer(4), Parameter :: mForA=10
!DEC$ ELSE 
  Integer(4), Parameter :: mForA=50
!DEC$ ENDIF 

  CONTAINS

! protected operations and functions

  Function Protected_Inv(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Inv
    if(dabs(a1).lt.pSmall) then
      Protected_Inv=dsign(pBig,a1)
    else
      Protected_Inv=1.0d0/a1
    end if
  end Function Protected_Inv

  Function Protected_Int(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Int
    if(dabs(a1).gt.2.0d9) then
      Protected_Int=dsign(2.0d9,a1)
    else
      Protected_Int=dble(nint(a1,4))
    end if
  end Function Protected_Int

  Function Protected_Sqr(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Sqr
    if(dabs(a1).gt.1.0d150) then
      Protected_Sqr=pBig
    else
      Protected_Sqr=a1*a1
    end if
  end Function Protected_Sqr

  Function Protected_Srt(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Srt
    if(a1.gt.pSmall) then
      Protected_Srt=dsqrt(a1)
    else
      Protected_Srt=0.0d0
    end if
  end Function Protected_Srt

  Function Protected_Log(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Log
    if(a1.gt.pSmall) then
      Protected_Log=dlog(a1)
    else
      Protected_Log=dlog(pSmall)
    end if
  end Function Protected_Log

  Function Protected_Exp(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Exp
    if(a1.lt.700.0d0) then
      if(a1.gt.-700.0d0) then
        Protected_Exp=dexp(a1)
      else
        Protected_Exp=dexp(-700.0d0)
      end if
    else
      Protected_Exp=dexp(700.0d0)
    end if
  end Function Protected_Exp

  Function Protected_Cos(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Cos
    Real(8) dum,p2
    p2=2.0_8*Pi
    dum=dmod(dabs(a1),p2)
    Protected_Cos=dcos(dum)
  end Function Protected_Cos

  Function Protected_Sin(a1)
    Real(8), Intent(In) :: a1
    Real(8) :: Protected_Sin
    Real(8) dum,p2
    p2=2.0_8*Pi
    dum=dmod(dabs(a1),p2)
    if(a1.lt.0.0d0) then
      Protected_Sin=-dsin(dum)
    else
      Protected_Sin=dsin(dum)
    end if
  end Function Protected_Sin

  Function Protected_Add(a1,a2)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: Protected_Add
    Real(8) dum
    if((dabs(a1).gt.1.0d299).and.(dabs(a2).gt.1.0d299)) then
      dum=(a1*1.0d-150)+(a2*1.0d-150)
      if(dabs(dum).gt.1.0d150) then
        if(dum.gt.0.0d0) then
          Protected_Add=pBig
        else
          Protected_Add=nBig
        end if
      else
        Protected_Add=dum*1.0d150
      end if
    else
      Protected_Add=a1+a2
    end if
  end Function Protected_Add

  Function Protected_Sub(a1,a2)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: Protected_Sub
    Real(8) dum
    dum=-a2
    Protected_Sub=Protected_Add(a1,dum)
  end Function Protected_Sub

  Function Protected_Mul(a1,a2)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: Protected_Mul
    Real(8) aa1,aa2,dum
    aa1=dabs(a1)
    aa2=dabs(a2)
    if((aa1.lt.1.0d150).and.(aa2.lt.1.0d150)) then
      Protected_Mul=a1*a2
    else if(aa1.gt.aa2) then
      dum=(a1*pSmall)*a2
      if(dabs(dum).lt.1.0d0) then
        Protected_Mul=pBig*dum
      else
        Protected_Mul=dsign(pBig,dum)
      end if
    else
      dum=(a2*pSmall)*a1
      if(dabs(dum).lt.1.0d0) then
        Protected_Mul=pBig*dum
      else
        Protected_Mul=dsign(pBig,dum)
      end if
    end if
  end Function Protected_Mul

  Function Protected_Div(a1,a2)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: Protected_Div
    Real(8) dum
    if(dabs(a1).lt.pSmall) then
      if(dabs(a2).lt.pSmall) then
        Protected_Div=dsign(1.0d0,a1)*dsign(1.0d0,a2)
      else
        dum=1.0d0/a2
        Protected_Div=Protected_Mul(a1,dum)
      end if
    else
      dum=Protected_Inv(a2)
      Protected_Div=Protected_Mul(a1,dum)
    end if
  end Function Protected_Div

  Function Protected_Pow(a1,a2)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: Protected_Pow
    Real(8) dum
    dum=Protected_Mul(Protected_Log(a1),a2)
    Protected_Pow=Protected_Exp(dum)
  end Function Protected_Pow

  Function Protected_Ata(a1,a2)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: Protected_Ata
    if((dabs(a1).lt.pSmall).and.(dabs(a2).lt.pSmall)) then
      Protected_Ata=0.0d0
    else
      Protected_Ata=datan2(a1,a2)
    end if
  end Function Protected_Ata

  Function Protected_CInv(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CInv
    if(cdabs(a1).lt.pSmall) then
      Protected_CInv=DCmplx(pBig,pBig)
    else
      Protected_CInv=1.0d0/a1
    end if
  end Function Protected_CInv

  Function Protected_CInt(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CInt
    if(cdabs(a1).gt.2.0d9) then
      Protected_CInt=DCmplx(2.0d9,2.0d9)
    else
      Protected_CInt=DCmplx(nint(Dble(a1),4),nint(DImag(a1),4))
    end if
  end Function Protected_CInt

  Function Protected_CSqr(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CSqr
    if(cdabs(a1).gt.1.0d150) then
      Protected_CSqr=pBig
    else
      Protected_CSqr=a1*a1
    end if
  end Function Protected_CSqr

  Function Protected_CSrt(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CSrt
    if(cdabs(a1).gt.pSmall) then
      Protected_CSrt=cdsqrt(a1)
    else
      Protected_CSrt=DCmplx(0.0d0,0.0d0)
    end if
  end Function Protected_CSrt

  Function Protected_CLog(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CLog
    if(cdabs(a1).gt.pSmall) then
      Protected_CLog=cdlog(a1)
    else
      Protected_CLog=cdlog(dCmplx(pSmall,0.0))
    end if
  end Function Protected_CLog

  Function Protected_CExp(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CExp
    if(cdabs(a1).lt.700.0d0) then
      if(cdabs(a1).gt.-700.0d0) then
        Protected_CExp=cdexp(a1)
      else
        Protected_CExp=DCmplx(0.0d0,0.0d0)
      end if
    else
      Protected_CExp=DCmplx(dexp(700.0d0),0.0d0)
    end if
  end Function Protected_CExp

  Function Protected_CCos(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CCos
    Protected_CCos=cdcos(a1)
  end Function Protected_CCos

  Function Protected_CSin(a1)
    Complex(8), Intent(In) :: a1
    Complex(8) :: Protected_CSin
    Protected_CSin=cdsin(a1)
  end Function Protected_CSin

  Function Protected_CAdd(a1,a2)
    Complex(8), Intent(In) :: a1,a2
    Complex(8) :: Protected_CAdd
    Complex(8) dum
    if((cdabs(a1).gt.1.0d299).and.(cdabs(a2).gt.1.0d299)) then
      dum=(a1*1.0d-150)+(a2*1.0d-150)
      if(cdabs(dum).gt.1.0d150) then
        Protected_CAdd=DCmplx(pBig,0.0d0)
      else
        Protected_CAdd=dum*1.0d150
      end if
    else
      Protected_CAdd=a1+a2
    end if
  end Function Protected_CAdd

  Function Protected_CSub(a1,a2)
    Complex(8), Intent(In) :: a1,a2
    Complex(8) :: Protected_CSub
    Complex(8) dum
    dum=-a2
    Protected_CSub=Protected_CAdd(a1,dum)
  end Function Protected_CSub

  Function Protected_CMul(a1,a2)
    Complex(8), Intent(In) :: a1,a2
    Complex(8) :: Protected_CMul
    Complex(8) dum
    Real(8) aa1,aa2
    aa1=cdabs(a1)
    aa2=cdabs(a2)
    if((aa1.lt.1.0d150).and.(aa2.lt.1.0d150)) then
      Protected_CMul=a1*a2
    else if(aa1.gt.aa2) then
      dum=(a1*pSmall)*a2
      if(cdabs(dum).lt.1.0d0) then
        Protected_CMul=pBig*dum
      else
        Protected_CMul=DCmplx(pBig,0.0d0)
      end if
    else
      dum=(a2*pSmall)*a1
      if(cdabs(dum).lt.1.0d0) then
        Protected_CMul=pBig*dum
      else
        Protected_CMul=DCmplx(pBig,0.0d0)
      end if
    end if
  end Function Protected_CMul

  Function Protected_CDiv(a1,a2)
    Complex(8), Intent(In) :: a1,a2
    Complex(8) :: Protected_CDiv
    Complex(8) dum
    if(cdabs(a1).lt.pSmall) then
      if(cdabs(a2).lt.pSmall) then
        Protected_CDiv=DCmplx(1.0d0,0.0d0)
      else
        dum=1.0d0/a2
        Protected_CDiv=Protected_CMul(a1,dum)
      end if
    else
      dum=Protected_CInv(a2)
      Protected_CDiv=Protected_CMul(a1,dum)
    end if
  end Function Protected_CDiv

  Function Protected_CPow(a1,a2)
    Complex(8), Intent(In) :: a1,a2
    Complex(8) :: Protected_CPow
    Complex(8) dum
    dum=Protected_CMul(Protected_CLog(a1),a2)
    Protected_CPow=Protected_CExp(dum)
  end Function Protected_CPow

! random numbers

  Function CHRnd(a1,a2,iseed)
    Real(8), Intent(In) :: a1,a2
    Real(8) :: CHRnd
    Real(4), Save :: dum
    Integer(4), Optional :: iseed
    if(Present(iseed)) then
      if(iseed.gt.0) then
        call random_seed(iseed)
      else
        call random_seed()
      end if
    end if
    call random_number(dum)
    CHRnd=a1+dble(dum)*(a2-a1)
  end Function CHRnd

! 2D geometric objects

  Subroutine Cart2Pol(x,y,r,f)
    Real(8) x,y,r,f,v(2)
    v(1)=x
    v(2)=y
    r=r2Vec_Length(v)
    f=Protected_Ata(y,x)
  end Subroutine Cart2Pol

  Subroutine Pol2Cart(r,f,x,y)
    Real(8) x,y,r,f
    x=r*dcos(f)
    y=r*dsin(f)
  end Subroutine Pol2Cart

  Subroutine DistPtPt(xA,yA,xB,yB,d)
    Real(8) xA,yA,xB,yB,d,v(2)
    v(1)=xB-xA
    v(2)=yB-yA
    d=r2Vec_Length(v)
  end Subroutine DistPtPt

  Real(8) Function DPtPt(xA,yA,xB,yB)
    Real(8) xA,yA,xB,yB,v(2)
    v(1)=xB-xA
    v(2)=yB-yA
    dPtPt=r2Vec_Length(v)
  end Function DPtPt

  Subroutine DistPtCircle(xP,yP,xO,yO,r,d,xN,yN,sN,lRSide)
    Real(8) xP,yP,xO,yO,xN,yN,sN,r,rP,fP,d,v(2)
    Logical lRSide
    v(1)=xP-xO
    v(2)=yP-yO
    call Cart2Pol(v(1),v(2),rP,fP)
    if(fP.lt.0.0d0) fP=fP+2.0d0*Pi
    d=rP-r
    xN=xO+r*dcos(fP)
    yN=yO+r*dsin(fP)
    sN=r*fP
    lRSide=.true.
    if(d.lt.0.0d0) then
      lRSide=.false.
      d=-d
    end if
  end Subroutine DistPtCircle

  Subroutine Dist3DPtFiniteLine(P,A,B,Pn,aA,d)
! get distance d of point P from the nearest point Pn on the finite line from A to B and the distance aA of A from Pn
    Real(8) P(3),A(3),B(3),Pn(3),e(3),AB,aA,d
    Pn=B-A ! abuse Pn as B-A
    AB=r3Vec_Length(Pn)
    if(AB.lt.pSmall) then ! safety if zero length
      e(1)=1.0d0
      e(2)=0.0d0
      e(3)=0.0d0
    else
      e=PN/AB
    end if
    Pn=P-A
    aA=dot_product(Pn,e)
    if(aA.lt.0.0d0) then
      Pn=A
    else if(aA.gt.AB) then
      Pn=B
    else
      Pn=A+aA*e
    end if
    d=r3Vec_Length(P-Pn)
  end Subroutine Dist3DPtFiniteLine

  Subroutine Dist3DPtInfiniteLine(P,O,e,Pn,aO,d)
! get distance d of point P from the nearest point Pn on the infinite line with origin O and unit direction e
    Real(8) P(3),O(3),Pn(3),e(3),aO,d
    Pn=P-O
    aO=dot_product(Pn,e)
    Pn=O+aO*e
    d=r3Vec_Length(P-Pn)
  end Subroutine Dist3DPtInfiniteLine

  Subroutine DistPtLine3D(P,z0,dz,Pn,dsPn,d)
! get distance d of point P from the nearest point Pn on the 3D line on the z axis from z0 to z0+dz
    Real(8) P(3),Pn(3),z0,dz,d
    Pn(1:2)=0.0d0
    if(P(3).lt.z0) then
      Pn(3)=z0
    else if(P(3).gt.z0+dz) then
      Pn(3)=z0+dz
    else
      Pn(3)=P(3)
    end if
    dsPn=Pn(3)
    d=r3Vec_Length(P-Pn)
  end Subroutine DistPtLine3D

  Subroutine DistPtRing3D(P,spac,r,f0,da,Pn,dsPn,d)
! get distance d of point P from the nearest point Pn on the 3D ring with coordinate system "spac"
  Real(8) P(3),spac(3,0:3),r,f0,da,Ps(3),Pn(3),dsPn,d
  call vGlob2Loc(P,spac,Ps)
  call DistPtRing(Ps,r,f0,da,Pn,dsPn,d)
  Ps=Pn
  call vGlob2Loc(Ps,spac,Pn)
  end Subroutine DistPtRing3D

  Subroutine DistPtRing(P,r,f0,da,Pn,dsPn,d)
! get distance d of point P from the nearest point Pn on the 3D ring around the z axis in the xy plane
! f0 in radians
    Real(8) P(3),Pn(3),r,f0,da,dsPn,d
    Integer(2) iOrient
    Logical lRSide
    Pn(3)=0.0d0
    dsPn=0.0d0
    if(dabs(da).lt.1.0d-100) then ! point in xy plane
      Pn(1)=r*dcos(f0)
      Pn(2)=r*dsin(f0)
    else if(dabs(r).lt.1.0d-100) then ! point in origin
      Pn(1:2)=0.0d0
    else
      iOrient=1_2
      if(da.lt.0.0d0) iOrient=-1_2
      call DistPtArc(P(1),P(2),0.0d0,0.0d0,r*dcos(f0),r*dsin(f0),da,2.0d300,iOrient,d,Pn(1),Pn(2),dsPn,lRSide)
    end if
    d=r3Vec_Length(P-Pn)
  end Subroutine DistPtRing

  Subroutine DistPtSpiral3D(P,spac,x0,y0,dr,da,dz,dmx,Pn,d)
! get distance d of point P from the nearest point Pn on the 3D spiral with coordinate system "spac"
  Real(8) P(3),spac(3,0:3),x0,y0,dr,da,dz,dmx,d,Ps(3),Pn(3)
  call vGlob2Loc(P,spac,Ps)
  call DistPtSpiral(Ps,x0,y0,dr,da,dz,dmx,Pn,d)
  Ps=Pn
  call vGlob2Loc(Ps,spac,Pn)
  end Subroutine DistPtSpiral3D

  Subroutine DistPtSpiral(P,x0,y0,dr,da,dz,dmx,Pn,d)
! get distance d of point P from the nearest point Pn on the 3D spiral around the z axis, starting in the xy plane
! not very accurate, if d>dmx: rough estimate
! angles: radians!
    Real(8) P(3),Pn(3),x0,y0,z0,dr,da,dz,dsPn,d,rP,fP,r0,f0,zO,rN,zN,sN,A(3),B(3),f1,f2,da12,dr12,dz12,r,f,z,Pl(3),x, &
    & y1,y2,y3,al,sN2,d1,x1,x2,x3,P1(3),P2(3),P3(3),ri,fi,zi,ri1,fi1,zi1,Pi2,dmx
    Integer(4) i,n,n2,il
    Logical lRSide,lZero
    if(dabs(da).lt.1.0d-100) then ! straight line in 3D space
      A(1)=x0
      A(2)=y0
      A(3)=0.0d0
      fP=datan2(y0,x0)
      r0=dsqrt(x0**2+y0**2)+dr
      B(1)=r0*dcos(fP)
      B(2)=r0*dsin(fP)
      B(3)=dz
      call Dist3DPtFiniteLine(P,A,B,Pn,dsPn,d)
      return
    end if
    if(dabs(dz).lt.1.0d-100) then ! in xy plane
      Pn(3)=0.0d0
      if(dabs(dr).lt.1.0d-100) then ! simple arc in xy plane
        r0=dsqrt(x0**2+y0**2)
        fP=datan2(y0,x0)
        call DistPtRing(P,r0,fP,da,Pn,dsPn,d)
        return
      end if
    end if
    r0=sqrt(x0**2+y0**2) ! spiral start radius in xy plane
    f0=Protected_Ata(y0,x0) ! spiral start angle
    z0=0.0d0
    rP=sqrt(P(2)**2+P(1)**2)
    fP=0.0d0
    if(rP.gt.1.0d-300) fP=datan2(P(2),P(1))
    if(dabs(dr).lt.1.0d-100) then ! cone is cylinder (dz=0 already tested)
      zO=-1.0d300
      call DistPtLine(rP,P(3),r0,0.0d0,r0,dz,0.0d0,d,rN,zN,sN2,lRSide,lZero,al,sN) ! find point on cylinder line in the section plane spanned by P and z axis
    else ! real cone
      zO=-r0*dz/dr
      call DistPtLine(rP,P(3),r0,0.0d0,r0+dr,dz,0.0d0,d,rN,zN,sN2,lRSide,lZero,al,sN) ! find point on cone line in the section plane spanned by P and z axis
    end if
    if(d.gt.dmx) then ! rough estimate
      Pn(1)=rN*dcos(fP)
      Pn(2)=rN*dsin(fP)
      Pn(3)=zN
    end if
    if(dabs(da).lt.2.0d0*Pi) then ! short spiral
      n=max(10_4,Int4(40.0d0*Pi/dabs(da)))
      dr12=dr/Dble(n)
      da12=da/Dble(n)
      dz12=dz/Dble(n)
      call NextMinDistPtSpiral(P,r0,f0,z0,0.5d0*dr,0.5d0*da,0.5d0*dz,n,Pn,d,i,ri,fi,zi) ! upwards
      call NextMinDistPtSpiral(P,r0+dr,f0+da,z0+dz,-0.5d0*dr,-0.5d0*da,-0.5d0*dz,n,Pl,d1,i1,ri1,fi1,zi1) ! downwards
      if(d1.lt.d) then ! use smaller distance
        d=d1
        Pn=Pl
        ri=ri1
        fi=fi1
        zi=zi1
      end if
    else ! long spiral: find first nearest point on circular cone that contains the spiral
  ! define start angle f1 and end angle f2 for search
      Pi2=Pi*2.0d0
      if(da.lt.0.0d0) Pi2=-Pi2
      if(sN.le.0.0d0) then ! "below": search first interval 0...2Pi
        N=60
        da12=Pi2/Dble(n)
        dr12=dr*da12/da
        dz12=dz*da12/da
        call NextMinDistPtSpiral(P,r0,f0,z0,dr*Pi2/da,Pi2,dz*Pi2/da,n,Pn,d,i,ri,fi,zi)
      else if(sN.ge.al) then ! "above"
        N=60
        da12=2.0d0*Pi/Dble(n)
        dr12=dr*da12/da
        dz12=dz*da12/da
        call NextMinDistPtSpiral(P,r0+dr,f0+da,z0+dz,-dr*Pi2/da,-Pi2,-dz*Pi2/da,n,Pn,d,i,ri,fi,zi)
      else ! "in between"
        if(dabs(dz).gt.dabs(dr)) then
          fPs=zN*da/dz ! angle of nearest point Pn from z value
        else
          fPs=(rN-r0)*da/dr ! angle of nearest point Pn from r value
        end if
        sN=dsqrt((Pi2*dr/da)**2+(Pi2*dz/da)**2)
        i=Int4(fPs/(2.0d0*Pi))
        d1=1.0d300
        fPs=fP+Dble(i)*2.0d0*Pi
        do il=-5,5
          f=fP+Dble(2*i+il)*Pi
          if(((da.gt.0.0d0).and.(f.lt.f0)).or.((da.lt.0.0d0).and.(f.gt.f0))) Cycle
          if(((da.gt.0.0d0).and.(f.gt.f0+da)).or.((da.lt.0.0d0).and.(f.lt.f0+da))) Cycle
          r=r0+dr*(f-f0)/da
          z=dz*(f-f0)/da
          Pl(1)=r*dcos(f)
          Pl(2)=r*dsin(f)
          Pl(3)=z
          d=r3Vec_Length(P-Pl)
          if(d.lt.d1) then
            d1=d
            fPs=f
            Pn=Pl
          end if
        end do
! check end points
        Pl(1)=r0*dcos(f0)
        Pl(2)=r0*dsin(f0)
        Pl(3)=0.0d0
        d=r3Vec_Length(P-Pl)
        if(d.lt.d1) then
          d1=d
          fPs=f0
          Pn=Pl
        end if
        Pl(1)=(r0+dr)*dcos(f0+da)
        Pl(2)=(r0+dr)*dsin(f0+da)
        Pl(3)=dz
        d=r3Vec_Length(P-Pl)
        if(d.lt.d1) then
          d1=d
          fPs=f0+da
          Pn=Pl
        end if
        n=60
        n2=n
        d=d1
        if(dabs(d/sN).lt.1.0d0) then ! point P is close: search in angular distance sN depending on d and start radius
          sN=d/(5.0d0*dsqrt(Pn(1)**2+Pn(2)**2))
        else ! not close, search a bit more than +-2Pi
          sN=2.2d0*Pi/Dble(n)
        end if
        f1=fPs+dble(n)*sN ! search intervals
        f2=fPs-dble(n2)*sN
        sN2=sN
        if(da.gt.0.0d0) then ! reduce n if search intervals too big and adapt sN,sN2
          if(f1.gt.f0+da) then
            n=max(0_4,Int4((f0+da-fPs)/sN))
            if(n.gt.0) sN=(f0+da-fPs)/Dble(n)
          end if
          if(f2.lt.f0) then
            n2=max(0_4,Int4((fPs-f0)/sN2))
            if(n2.gt.0) sN2=(fPs-f0)/Dble(n2)
          end if
        else
          if(f1.gt.f0) then
            n=max(0_4,Int4((f0-fPs)/sN))
            if(n.gt.0) sN=(f0-fPs)/Dble(n)
          end if
          if(f2.lt.f0+da) then
            n2=max(0_4,Int4((fPs-f0-da)/sN2))
            if(n2.gt.0) sN2=(fPs-f0-da)/Dble(n2)
          end if
        end if
        f=fPs ! start search starting from f, r, z
        r=r0+dr*(fPs-f0)/da
        z=dz*(fPs-f0)/da
        da12=sN
        dr12=dr*da12/da
        dz12=dz*da12/da
        call NextMinDistPtSpiral(P,r,f,z,dr12*Dble(n),da12*Dble(n),dz12*Dble(n),n,Pn,d,i,ri,fi,zi) ! upwards
        da12=sN2
        dr12=dr*da12/da
        dz12=dz*da12/da
        call NextMinDistPtSpiral(P,r,f,z,-dr12*Dble(n2),-da12*Dble(n2),-dz12*Dble(n2),-n2,Pl,d1,i1,ri1,fi1,zi1) ! downwards
        if(d1.lt.d) then ! use smaller distance
          d=d1
          Pn=Pl
          ri=ri1
          fi=fi1
          zi=zi1
        end if
      end if
    end if
! refine search using parabolic interpolation
    da12=min(dabs(0.5d0*da12),dabs(1.999999d0*da))
    x1=fi-da12
    x3=fi+da12
    if(da.gt.0.0d0) then
      if(x1.lt.f0) then
        x1=f0
      else if(x3.gt.f0+da) then
        x3=f0+da
      end if
    else
      if(x3.gt.f0) then
        x3=f0
      else if(x1.lt.f0+da) then
        x1=f0+da
      end if
    end if
    da12=0.5d0*dabs(x3-x1)
    x2=0.5d0*(x1+x3)
    f=x1
    r=ri+dr*(f-fi)/da
    z=zi+dz*(f-fi)/da
    P1(1)=r*dcos(f)
    P1(2)=r*dsin(f)
    P1(3)=z
    y1=r3Vec_Length(P-P1)
    f=x2
    r=ri+dr*(f-fi)/da
    z=zi+dz*(f-fi)/da
    P2(1)=r*dcos(f)
    P2(2)=r*dsin(f)
    P2(3)=z
    y2=r3Vec_Length(P-P2)
    f=x3
    r=ri+dr*(f-fi)/da
    z=zi+dz*(f-fi)/da
    P3(1)=r*dcos(f)
    P3(2)=r*dsin(f)
    P3(3)=z
    y3=r3Vec_Length(P-P3)
    x=ParabolaOpt(x1,x2,x3,y1,y2,y3) ! parabolic interpolation
    if(((da.gt.0.0d0).and.(x.ge.f0).and.(x.le.f0+da)).or.((da.lt.0.0d0).and.(x.le.f0).and.(x.ge.f0+da))) then ! f is insinde
      r=ri+dr*(x-fi)/da
      z=zi+dz*(x-fi)/da
      Pl(1)=r*dcos(x)
      Pl(2)=r*dsin(x)
      Pl(3)=z
      d1=r3Vec_Length(P-Pl)
    else
      d1=2.0d300
    end if
    if((d1.lt.d).and.(d1.lt.y1).and.(d1.lt.y2).and.(d1.lt.y3)) then ! take shortest distance out of d,d1,y1,y2,y3
      Pn=Pl
      d=d1
    else if((y1.lt.d).and.(y1.lt.d1).and.(y1.lt.y2).and.(y1.lt.y3)) then
      Pn=P1
      d=y1
      x=x1
    else if((y2.lt.d).and.(y2.lt.d1).and.(y2.lt.y1).and.(y2.lt.y3)) then
      Pn=P2
      d=y2
      x=x2
    else if((y3.lt.d).and.(y3.lt.d1).and.(y3.lt.y1).and.(y3.lt.y2)) then
      Pn=P3
      d=y3
      x=x3
    else
      x=fi
    end if
    if((dabs((y1-y2).div.y2).gt.1.0d-3).or.(dabs((y3-y2).div.y2).gt.1.0d-3)) then ! not flat: try second refinement
      fi=x
      da12=min(2.0d0*dabs(d/ri),0.5d0*dabs(da12))
      x1=fi-dabs(da12)
      x3=fi+dabs(da12)
      if(da.gt.0.0d0) then
        if(x1.lt.f0) then
          x1=f0
        else if(x3.gt.f0+da) then
          x3=f0+da
        end if
      else
        if(x3.gt.f0) then
          x3=f0
        else if(x1.lt.f0+da) then
          x1=f0+da
        end if
      end if
      da12=0.5d0*dabs(x3-x1)
      x2=0.5d0*(x1+x3)
      f=x1
      r=ri+dr*(f-fi)/da
      z=zi+dz*(f-fi)/da
      P1(1)=r*dcos(f)
      P1(2)=r*dsin(f)
      P1(3)=z
      y1=r3Vec_Length(P-P1)
      f=x2
      r=ri+dr*(f-fi)/da
      z=zi+dz*(f-fi)/da
      P2(1)=r*dcos(f)
      P2(2)=r*dsin(f)
      P2(3)=z
      y2=r3Vec_Length(P-P2)
      f=x3
      r=ri+dr*(f-fi)/da
      z=zi+dz*(f-fi)/da
      P3(1)=r*dcos(f)
      P3(2)=r*dsin(f)
      P3(3)=z
      y3=r3Vec_Length(P-P3)
      x=ParabolaOpt(x1,x2,x3,y1,y2,y3) ! parabolic interpolation
      if(((da.gt.0.0d0).and.(x.ge.f0).and.(x.le.f0+da)).or.((da.lt.0.0d0).and.(x.le.f0).and.(x.ge.f0+da))) then ! f is insinde
        r=ri+dr*(x-fi)/da
        z=zi+dz*(x-fi)/da
        Pl(1)=r*dcos(x)
        Pl(2)=r*dsin(x)
        Pl(3)=z
        d1=r3Vec_Length(P-Pl)
      else
        d1=2.0d300
      end if
      if((d1.lt.d).and.(d1.lt.y1).and.(d1.lt.y2).and.(d1.lt.y3)) then ! take shortest distance out of d,d1,y1,y2,y3
        Pn=Pl
        d=d1
      else if((y1.lt.d).and.(y1.lt.d1).and.(y1.lt.y2).and.(y1.lt.y3)) then
        Pn=P1
        d=y1
        x=x1
      else if((y2.lt.d).and.(y2.lt.d1).and.(y2.lt.y1).and.(y2.lt.y3)) then
        Pn=P2
        d=y2
        x=x2
      else if((y3.lt.d).and.(y3.lt.d1).and.(y3.lt.y1).and.(y3.lt.y2)) then
        Pn=P3
        d=y3
        x=x3
      else
        x=fi
      end if
    end if
  end Subroutine DistPtSpiral
  
  Subroutine NextMinDistPtSpiral(P,r0,a0,z0,dr,da,dz,ni,Pn,dmin,imin,ri,fi,zi)
! search first local minimum distance dmin of Point from spiral
! if ni<0: do not start upwards
! angles a0,da in radians!
    Real(8) P(3),r0,a0,z0,dr,da,dz,Pn(3),Pl(3),P1(3),dmin,dmax,d,r,a,z,drN,daN,dzN,ri,fi,zi,d1,r1,a1,z1
    Integer(4) ni,n,i,imax,imin
    n=abs(ni)
    drN=dr/Dble(n)
    daN=da/Dble(n)
    dzN=dz/Dble(n)
    r1=r0
    a1=a0
    z1=z0
    P1(1)=r1*dcos(a1)
    P1(2)=r1*dsin(a1)
    P1(3)=z1
    d1=r3Vec_Length(P-P1)
    dmin=d1
    imin=0
    Pn=P1
    ri=r1
    fi=a1
    zi=z1
    r=r1+drN
    a=a1+daN
    z=z1+dzn
    Pl(1)=r*dcos(a)
    Pl(2)=r*dsin(a)
    Pl(3)=z
    d=r3Vec_Length(P-Pl)
    if(d.gt.dmin) then ! start upwards (find maximum before oing down to minmum
      if(ni.lt.0) return
      dmax=d
      imax=1
      do i=2,n
        r=r+drN
        a=a+daN
        z=z+dzn
        Pl(1)=r*dcos(a)
        Pl(2)=r*dsin(a)
        Pl(3)=z
        d=r3Vec_Length(P-Pl)
        if(d.gt.dmax) then
          dmax=d
          imax=i
        else
          exit
        end if
      end do
      dmin=dmax
    else ! start downwards
      dmin=d
      imin=1
      Pn=Pl
      imax=1
      ri=r
      fi=a
      zi=z
    end if
    do i=imax+1,n
      r=r+drN
      a=a+daN
      z=z+dzn
      Pl(1)=r*dcos(a)
      Pl(2)=r*dsin(a)
      Pl(3)=z
      d=r3Vec_Length(P-Pl)
      if(d.lt.dmin) then
        dmin=d
        imin=i
        Pn=Pl
        ri=r
        fi=a
        zi=z
      else
        exit
      end if
    end do
    if(d1.lt.dmin) then
      dmin=d1
      imin=0
      Pn=P1
      ri=r1
      fi=a1
      zi=z1
    end if
  end Subroutine NextMinDistPtSpiral

  Real(8) Function ParabolaOpt01(x2s,y1,y2,y3)
  Real(8) y1,y2,y3,x2s,xo,b,c
    c=((y3-y1)*x2s+y1-y2)/(x2s*(1.0d0-x2s))
    b=y3-y1-c
    xo=-0.5d0*b/c
    ParabolaOpt01=xo
  end Function ParabolaOpt01

  Real(8) Function ParabolaOpt(x1,x2,x3,y1,y2,y3)
  Real(8) x1,x2,x3,y1,y2,y3,x2s,xo
    x2s=(x2-x1)/(x3-x1)
    xo=ParabolaOpt01(x2s,y1,y2,y3)
    ParabolaOpt=x1+xo*(x3-x1)
  end Function ParabolaOpt

  Subroutine Dist3DPtLine(P,A,B,Pn,aA,d)
! get distance d of point P from the nearest point Pn on the line from A to B and the distance aA of A from Pn
    Real(8) P(3),A(3),B(3),Pn(3),e(3),aA,d
    e=Unit3DVec(B-A)
    Pn=P-A
    aA=dot_product(Pn,e)
    Pn=A+aA*e
    d=r3Vec_Length(P-Pn)
  end Subroutine Dist3DPtLine

  Subroutine DistPtLine(xP,yP,xA,yA,xB,yB,r,d,xN,yN,sN,lRSide,lZero,lLine,sLine)
! get distance d of point P from finite line with end points A, B and neares point N (coordinates and parameter s along line)
    Real(8) xP,yP,xA,yA,xB,yB,r,d,xN,yN,sN,AB(2),AP(2),vn(2),dAB,dAP,s
    Real(8), optional:: lLine,sLine
    Logical lRSide,lZero
    lRSide=.true.
    lZero=.true.
    AB(1)=xB-xA
    AB(2)=yB-yA
    dAB=r2Vec_Length(AB)
    if(Present(lLine)) lLine=dAB
    if(Present(sLine)) sLine=0.0d0
    AP(1)=xP-xA
    AP(2)=yP-yA
    dAP=r2Vec_Length(AP)
    sN=0.0d0
    if(dAbs(dAB).lt.max(1.0d-100,1.0d-10*dAbs(r))) then
      d=dAP
      xN=xA
      yN=yA
      return
    end if
    lZero=.false.
    s=r2Scl_Prod(AP,AB)/dAB
    if(Present(sLine)) sLine=s
    if(s.lt.0.0d0) then
      d=dAP
      xN=xA
      yN=yA
    else if(s.gt.dAB) then
      AP(1)=xP-xB
      AP(2)=yP-yB
      d=r2Vec_Length(AP)
      xN=xB
      yN=yB
      sN=dAB
    else
      sN=s
      s=s/dAB
      xN=xA+s*AB(1)
      yN=yA+s*AB(2)
      AP(1)=xP-xN
      AP(2)=yP-yN
      d=r2Vec_Length(AP)
    end if
    vn(1)=-AB(2)
    vn(2)=AB(1)
    s=r2Scl_Prod(AP,vn)
    if(s.gt.0.0d0) lRSide=.false.
  end Subroutine DistPtLine
  
  Subroutine DistPtTriangle(xP,yP,xA,yA,xB,yB,xC,yC,d,xN,yN,linside)
! get the distance d of point (xP,yP) from a triangle with corners (xA,yA),(xB,yB),(xC,yC)
! and return the nearest point (xN,yN), set linside=true when (xP,yP) is in the triangle 
    Real(8) xP,yP,xA,yA,xB,yB,xC,yC,d,dA,dB,dC,xN,yN,xNA,yNA,xNB,yNB,xNC,yNC,sN
    Logical linside,lRSideA,lRSideB,lRSideC,lZero
    call DistPtLine(xP,yP,xA,yA,xB,yB,0.0d0,dC,xNC,yNC,sN,lRSideC,lZero)
    call DistPtLine(xP,yP,xB,yB,xC,yC,0.0d0,dA,xNA,yNA,sN,lRSideA,lZero)
    call DistPtLine(xP,yP,xC,yC,xA,yA,0.0d0,dB,xNB,yNB,sN,lRSideB,lZero)
    linside=.false.
    if(lRSideA) then
      if(lRSideB) then
        d=dA
        xN=xC
        yN=yC
      else if(lRSideC) then
        d=dC
        xN=xB
        yN=yB
      else
        d=dA
        xN=xNA
        yN=yNA
      end if
    else if(lRSideB) then
      if(lRSideC) then
        d=dB
        xN=xA
        yN=yA
      else
        d=dB
        xN=xNB
        yN=yNB
      end if
    else if(lRSideC) then
      d=dC
      xN=xNC
      yN=yNC
    else
      linside=.true.
      d=0.0d0
      xN=xP
      yN=yP
    end if
  end Subroutine DistPtTriangle
  
  Subroutine DistPtRectangle(xP,yP,A,B,d,xN,yN,linside)
! get the distance d of point (xP,yP) from a rectangle with corners (0,0),(A,0),(A,B),(0,B)
! and return the nearest point (xN,yN), set linside=true when (xP,yP) is in the Rectangle 
    Real(8) xP,yP,A,B,d,xN,yN
    Logical linside
    linside=.false.
    if(xP.lt.0.0d0) then
      if(yP.lt.0.0d0) then
        d=dsqrt(xP**2+yP**2)
        xN=0.0d0
        yN=0.0d0
      else if(yP.gt.B) then
        d=dsqrt(xP**2+(yP-B)**2)
        xN=0.0d0
        yN=B
      else
        d=-xP
        xN=0.0d0
        yN=yP
      end if
    else if(xP.gt.A) then
      if(yP.lt.0.0d0) then
        d=dsqrt((xP-A)**2+yP**2)
        xN=A
        yN=0.0d0
      else if(yP.gt.B) then
        d=dsqrt((xP-A)**2+(yP-B)**2)
        xN=A
        yN=B
      else
        d=xP-A
        xN=A
        yN=yP
      end if
    else
      if(yP.lt.0.0d0) then
        d=-yP
        xN=xP
        yN=0.0d0
      else if(yP.gt.B) then
        d=yP-B
        xN=xP
        yN=B
      else
        linside=.true.
        d=0.0d0
        xN=xP
        yN=yP
      end if
    end if
  end Subroutine DistPtRectangle

  Subroutine DistPtArc(xP,yP,xO,yO,xA,yA,xBi,yBi,iOr,d,xN,yN,sN,lRSide,lArc)
! get distance d and nearest point N (coordinates and parameter s along arc) of a point P from arc with origin O, start point A, end point B
    Real(8) xP,yP,xA,yA,xB,yB,xBi,yBi,xO,yO,d,xN,yN,sN,fP,fA,fB,r,rP,v(2)
    Real(8), optional:: lArc
    Integer(2) iOr
    Logical lRSide
    lRSide=.true.
    sN=0.0d0
    v(1)=xA-xO
    v(2)=yA-yO
    r=r2Vec_Length(v)
    fA=Protected_Ata(v(2),v(1))
    if(yBi.ge.1.0d300) then ! point B given by angle, not by coordinates
      fB=fA+xBi
      xB=r*dcos(fB)
      yB=r*dsin(fB)
    else
      xB=xBi
      yB=yBi
      v(1)=xB-xO
      v(2)=yB-yO
      fB=Protected_Ata(v(2),v(1))
    end if
    if(iOr.eq.1_2) then
      if(fB.le.fA) fB=fB+2.0d0*Pi
      if(dabs(fB-fA).lt.1.0d-10) fB=fA+2.0d0*Pi
    else
      if(fB.ge.fA) fB=fB-2.0d0*Pi
      if(dabs(fB-fA).lt.1.0d-10) fB=fA-2.0d0*Pi
    end if
    if(Present(lArc)) lArc=r*abs(fB-fA)
    v(1)=xP-xO
    v(2)=yP-yO
    call Cart2Pol(v(1),v(2),rP,fP)
    if(iOr.eq.1_2) then
      if(fP.lt.fA) fP=fP+2.0d0*Pi
    else
      if(fP.gt.fA) fP=fP-2.0d0*Pi
    end if
    d=rP-r
    if(d.lt.0.0d0) then
      lRSide=.false.
      d=-d
    end if
    if((iOr.eq.1_2).and.(fP.ge.fA).and.(fP.le.fB)) then
      xN=xO+r*dcos(fP)
      yN=yO+r*dsin(fP)
      sN=r*(fP-fA)
    else if((iOr.ne.1_2).and.(fP.le.fA).and.(fP.ge.fB)) then
      xN=xO+r*dcos(fP)
      yN=yO+r*dsin(fP)
      sN=r*(fA-fP)
    else
      call DistPtPt(xA,yA,xP,yP,fA)
      call DistPtPt(xB,yB,xP,yP,fB)
      if(fA.lt.fB) then
        d=fA
        xN=xA
        yN=yA
      else
        d=fB
        xN=xB
        yN=yB
        sN=r*abs(fB-fA)
      end if
    end if
    if(iOr.ne.1_2) lRSide=.not.lRSide
  end Subroutine DistPtArc

  Subroutine PtOnCircle(sPt,xO,yO,r,xP,yP,xtP,ytP,slen,ns)
    Real(8) sPt,xO,yO,r,xP,yP,xtP,ytP,slen,f
    Integer(4) ns
    if(r.lt.pSmall) then
      xP=xO
      yP=yO
      xtP=1.0d0
      ytP=0.0d0
      slen=0.0d0
      ns=-2000000000
    else
      slen=2.0d0*Pi*r
      if((dabs(slen)-dabs(sPt)).gt.nSmall) then
        ns=0_4
      else
        f=min(dabs(sPt/slen),2.1d9)
        ns=idInt(f)
      end if
      if(sPt.lt.0.0d0) ns=-ns-1
      f=(sPt-dble(ns)*slen)/r
      xtP=-dsin(f)
      ytP=dcos(f)
      xP=xO+r*dcos(f)
      yP=yO+r*dsin(f)
    end if
  end Subroutine PtOnCircle

  Subroutine PtOnLine(sPt,xA,yA,xB,yB,xP,yP,xtP,ytP,slen,ns)
    Real(8) sPt,xA,yA,xB,yB,xP,yP,xtP,ytP,slen,v(2),f
    Integer(4) ns
    call DistPtPt(xA,yA,xB,yB,slen)
    v(1)=xB-xA
    v(2)=yB-yA
    call unit2DV(v)
    xtP=v(1)
    ytP=v(2)
    if(slen.lt.pSmall) then
      if(sPt.gt.slen) then
        ns=2000000000
      else if(sPt.lt.0.0d0) then
        ns=-2000000000
      else
        ns=0
      end if
      xP=0.5d0*(xA+xB)
      yP=0.5d0*(yA+yB)
    else
      if(slen.ge.sPt) then
        ns=0_4
      else if((dabs(slen)-dabs(sPt)).gt.(-1.0d-14*dabs(slen))) then
        ns=0_4
      else
        f=min(dabs(sPt/slen),2.1d9)
        ns=idInt(f)
      end if
      if(sPt.lt.0.0d0) ns=-ns-1
      xP=xA+sPt*v(1)
      yP=yA+sPt*v(2)
    end if
  end Subroutine PtOnLine

  Subroutine PtOnArc(sPt,xO,yO,xA,yA,xB,yB,iOr,xP,yP,xtP,ytP,slen,ns)
    Real(8) sPt,xO,yO,xA,yA,xB,yB,xP,yP,xtP,ytP,slen,r,fA,fB,f,v(2)
    Integer(4) ns
    Integer(2) iOr
    v(1)=xA-xO
    v(2)=yA-yO
    call Cart2Pol(v(1),v(2),r,fA)
    v(1)=xB-xO
    v(2)=yB-yO
    fB=Protected_Ata(v(2),v(1))
    if(iOr.eq.1_2) then
      if(fB.lt.fA) fB=fB+2.0d0*Pi
    else
      if(fB.gt.fA) fB=fB-2.0d0*Pi
    end if
    slen=r*Dabs(fB-fA)
    if(sPt.lt.0.0d0) then
      ns=-1
      f=fA
    else if(sPt.gt.slen) then
      ns=1
      f=fB
    else
      ns=0
      if(r.lt.pSmall) then
        f=0.5d0*(fA+fB)
      elseif(iOr.eq.1_2) then
        f=fA+sPt/r
      else
        f=fA-sPt/r
      end if
    end if
    xtP=-dsin(f)
    ytP=dcos(f)
    xP=xO+r*ytP
    yP=yO-r*xtP
    if(iOr.ne.1_2) then
      xtP=-xtP
      ytP=-ytP
    end if
  end Subroutine PtOnArc

  Real(8) Function ArcLength(xO,yO,xA,yA,xB,yB)
    Real(8) xO,yO,xA,yA,xB,yB,r,fA,fB,v(2)
    v(1)=xA-xO
    v(2)=yA-yO
    call Cart2Pol(v(1),v(2),r,fA)
    v(1)=xB-xO
    v(2)=yB-yO
    fB=Protected_Ata(v(2),v(1))
    if(fB.lt.fA) fB=fB+2.0d0*Pi
    ArcLength=r*(fB-fA)
  end Function ArcLength

  Subroutine Line2DSec(A,B,C,D,ab,cd,S)
! section of line AB with CD
    Real(8) A(2),B(2),C(2),D(2),ab,cd,F(2),G(2),E(2)
    Real(8), Optional :: S(2)
    F=B-A
    G=C-D
    E=C-A
    ab= (E(1)*G(2)-E(2)*G(1)).div.(F(1)*G(2)-F(2)*G(1))
    cd=-(E(1)*F(2)-E(2)*F(1)).div.(F(1)*G(2)-F(2)*G(1))
    if(Present(S)) S=A+ab*F
  end Subroutine Line2DSec

  Real(8) Function cdAbs1(c)
  ! try to avoid problem with compliler
    Complex(8), Intent(in):: c
    Real(8) v(2)
    v(1)=Dble(c)
    v(2)=Dimag(c)
    cdAbs1=r2Vec_Length(v)
  end Function cdAbs1

  Complex(8) Function cdAbs2(c)
  ! try to avoid problem with compliler
    Complex(8), Intent(in):: c
    Real(8) r,i
    r=Dble(c)
    i=Dimag(c)
    cdAbs2=r*r+i*i
  end Function cdAbs2

! vector and matrix manipulations

  Function r2Scl_Prod(u,v) Result(w)
  ! product w=u.v of 2D real vectors
    Real(8) u(2),v(2),w
    w=Dot_Product(u,v)
  end Function r2Scl_Prod

  Function c2Scl_Prod(u,v) Result(w)
  ! product w=u.v of 2D complex vectors
    Complex(8) u(2),v(2),w
    w=Dot_Product(Conjg(u),v)
  end Function c2Scl_Prod

  Function r3Scl_Prod(u,v) Result(w)
  ! product w=u.v of 3D real vectors
    Real(8) u(3),v(3),w
    w=Dot_Product(u,v)
  end Function r3Scl_Prod

  Function c3Scl_Prod(u,v) Result(w)
  ! product w=u.v of 3D complex vectors
    Complex(8) u(3),v(3),w
    w=Dot_Product(Conjg(u),v)
  end Function c3Scl_Prod

  Function AbsVec2c(u) Result(w)
  ! length of 2D complex vector
    Complex(8) u(2)
    Real(8) w
    w=Dble(Dot_Product(Conjg(u),u))
  end Function AbsVec2c

  Function AbsVec3c(u) Result(w)
  ! length of 3D complex vector
    Complex(8) u(3)
    Real(8) w
    w=Dble(Dot_Product(Conjg(u),u))
  end Function AbsVec3c

  Function r2Vec_Ortho(u) Result(w)
  ! rotataion w=uorthogonal (2D real vectors)
    Real(8) u(2),w(2)
    w(1)=-u(2)
    w(2)=u(1)
  end Function r2Vec_Ortho

  Function c2Vec_Ortho(u) Result(w)
  ! rotataion w=uorthogonal (2D complexrs)
    Complex(8) u(2),w(2)
    w(1)=-u(2)
    w(2)=u(1)
  end Function c2Vec_Ortho

  Function r3Vec_Prod(u,v) Result(w)
  ! product w=uxv of 3D real vectors
    Real(8) u(3),v(3),w(3)
    w(1)=u(2)*v(3)-u(3)*v(2)
    w(2)=u(3)*v(1)-u(1)*v(3)
    w(3)=u(1)*v(2)-u(2)*v(1)
  end Function r3Vec_Prod

  Function c3Vec_Prod(u,v) Result(w)
  ! product w=uxv of 3D complex vectors
    Complex(8) u(3),v(3),w(3)
    w(1)=u(2)*v(3)-u(3)*v(2)
    w(2)=u(3)*v(1)-u(1)*v(3)
    w(3)=u(1)*v(2)-u(2)*v(1)
  end Function c3Vec_Prod

  Function r2Vec_Length(v) Result(w)
  ! length of 2D vector v
    Real(8) v(2),vm,w
    vm=dabs(v(1))
    if(dabs(v(2)).gt.vm) vm=dabs(v(2))
    if(vm.lt.pSmall) then
      w=0.0d0
    else
      w=vm*dsqrt((v(1)/vm)**2+(v(2)/vm)**2)
    end if
  end Function r2Vec_Length

  Function r3Vec_Length(v) Result(w)
  ! length of 3D vector v
    Real(8) v(3),vm,w
    vm=dabs(v(1))
    if(dabs(v(2)).gt.vm) then
      vm=dabs(v(2))
      if(dabs(v(3)).gt.vm) vm=dabs(v(3))
    else
      if(dabs(v(3)).gt.vm) vm=dabs(v(3))
    end if
    if(vm.lt.pSmall) then
      w=0.0d0
    else
      w=vm*dsqrt((v(1)/vm)**2+(v(2)/vm)**2+(v(3)/vm)**2)
    end if
  end Function r3Vec_Length

  Subroutine Proj3D(r0,plane,rEye,dist,xp,yp,d)
    Real(8) r0(3),plane(3,0:3),xp,yp,r(3),rEye(3),dist,dP(3),dE(3),dz,dzE
    Real(8), Optional :: d
    dP=r0-plane(1:3,0)
    dz=r3Scl_Prod(dP,plane(1:3,3))
    if(Present(d)) d=dz
    if(dist.lt.1.0d100) then
      dE=rEye(1:3)-plane(1:3,0)
      dzE=r3Scl_Prod(dE,plane(1:3,3))
      r(1)=(dzE*dP(1)-dz*dE(1)).div.(dzE-dz)
      r(2)=(dzE*dP(2)-dz*dE(2)).div.(dzE-dz)
      r(3)=(dzE*dP(3)-dz*dE(3)).div.(dzE-dz)
    else
      r=dP-dz*plane(1:3,3)
    end if
    xp=r3Scl_Prod(r,plane(1:3,1))
    yp=r3Scl_Prod(r,plane(1:3,2))
  end Subroutine Proj3D

  Subroutine getProj3Dxy(xv,yv,plane,rEye,dist,xp,yp)
! The point xv,yv is in the view plane. Get the global coordinates xp,yp
    Real(8) plane(3,0:3),xv,yv,xp,yp,r(3),rEye(3),dist,dP(3),dz,dz0
    r(1:3)=xv*plane(1:3,1)+yv*plane(1:3,2)+plane(1:3,0)
    if(dist.lt.1.0d100) then
      dz0=rEye(3)
      dP=r-rEye
      dz=-dP(3)
      dP=(dz0.div.dz)*dP
      r=rEye+dP
    else
      r(3)=0.0d0
    end if
    xp=r(1)
    yp=r(2)
  end Subroutine getProj3Dxy

  Subroutine Unit2DV(v,vl)
    Real(8) v(2),a
    Real(8), Optional :: vl
    a=r2Vec_Length(v)
    if(a.lt.pSmall) then
      v(1)=1.0d0
      v(2)=0.0d0
    else
      v=v/a
    end if
    if(Present(vl)) vl=a
  end Subroutine Unit2DV

  Subroutine Unit3DV(v,vl)
    Real(8) v(3),a
    Real(8), Optional :: vl
    a=r3Vec_Length(v)
    if(a.lt.pSmall) then
      v(1)=1.0d0
      v(2)=0.0d0
      v(3)=0.0d0
    else
      v=v/a
    end if
    if(Present(vl)) vl=a
  end Subroutine Unit3DV

  Function Unit2DVec(v) Result(w)
    Real(8) v(2),a,w(2)
    a=r2Vec_Length(v)
    if(a.lt.pSmall) then
      w(1)=1.0d0
      w(2)=0.0d0
    else
      w=v/a
    end if
  end Function Unit2DVec

  Function Unit3DVec(v) Result(w)
    Real(8) v(3),a,w(3)
    a=r3Vec_Length(v)
    if(a.lt.pSmall) then
      w(1)=1.0d0
      w(2)=0.0d0
      w(3)=0.0d0
    else
      w=v/a
    end if
  end Function Unit3DVec

  Function Rot3DVecX(v,angle) Result(w)
    Real(8) v(3),angle,w(3),f,r
    f=datan2(v(3),v(2))+angle*Pi/180.0d0
    r=r2Vec_Length(v(2:3))
    w(2)=r*dcos(f)
    w(3)=r*dsin(f)
    w(1)=v(1)
  end Function Rot3DVecX

  Function Rot3DVecY(v,angle) Result(w)
    Real(8) v(3),angle,w(3),f,r,vh(2)
    f=datan2(v(1),v(3))+angle*Pi/180.0d0
    vh(1)=v(3)
    vh(2)=v(1)
    r=r2Vec_Length(vh(1:2))
    w(3)=r*dcos(f)
    w(1)=r*dsin(f)
    w(2)=v(2)
  end Function Rot3DVecY

  Function Rot3DVecZ(v,angle) Result(w)
    Real(8) v(3),angle,w(3),f,r
    f=datan2(v(2),v(1))+angle*Pi/180.0d0
    r=r2Vec_Length(v(1:2))
    w(1)=r*dcos(f)
    w(2)=r*dsin(f)
    w(3)=v(3)
  end Function Rot3DVecZ

  Function Rot3DVecAxis0(eAxis,ex,ey,v,angle) Result(w)
! rotate vector v around axis defined by the unit vector eAxis
! ex and ey are auxiliary unit vectors that must be perpendicular to eAxis
    Real(8) eAxis(3),ex(3),ey(3),v(3),angle,w(3),vloc(3),f,r
    call Unit3DV(eAxis)
    ex=0.0d0
    ex(1)=1.0d0
    f=Dot_Product(eAxis,ex)
    if(f.gt.9.0d-1) then
      ex(1)=0.0d0
      ex(2)=1.0d0
    end if
    ey=r3Vec_Prod(eAxis,ex)
    call Unit3DV(ey)
    ex=r3Vec_Prod(ey,eAxis)
    call Unit3DV(ex)
    vloc(1)=Dot_Product(v,ex)
    vloc(2)=Dot_Product(v,ey)
    vloc(3)=Dot_Product(v,eAxis)
    f=datan2(vloc(2),vloc(1))+angle*Pi/180.0d0
    r=r2Vec_Length(vloc(1:2))
    w=r*dcos(f)*ex+r*dsin(f)*ey+vloc(3)*eAxis
  end Function Rot3DVecAxis0

  Function Rot3DVecAxis(eAxis,ex,ey,v,angle) Result(w)
! rotate vector v around axis defined by the unit vector eAxis
! ex and ey are auxiliary unit vectors that must be perpendicular to eAxis
    Real(8) eAxis(3),ex(3),ey(3),v(3),angle,w(3),vloc(3),f,r
    vloc(1)=Dot_Product(v,ex)
    vloc(2)=Dot_Product(v,ey)
    vloc(3)=Dot_Product(v,eAxis)
    f=datan2(vloc(2),vloc(1))+angle*Pi/180.0d0
    r=r2Vec_Length(vloc(1:2))
    w=r*dcos(f)*ex+r*dsin(f)*ey+vloc(3)*eAxis
  end Function Rot3DVecAxis

  Function Rot3DVecXa(v,angle) Result(w)
    Real(8) v(3),angle,w(3),f,r
    f=datan2(v(3),v(2))+angle
    r=r2Vec_Length(v(2:3))
    w(2)=r*dcos(f)
    w(3)=r*dsin(f)
    w(1)=v(1)
  end Function Rot3DVecXa

  Function Rot3DVecYa(v,angle) Result(w)
    Real(8) v(3),angle,w(3),f,r,vh(2)
    f=datan2(v(1),v(3))+angle
    vh(1)=v(3)
    vh(2)=v(1)
    r=r2Vec_Length(vh(1:2))
    w(3)=r*dcos(f)
    w(1)=r*dsin(f)
    w(2)=v(2)
  end Function Rot3DVecYa

  Function Rot3DVecZa(v,angle) Result(w)
    Real(8) v(3),angle,w(3),f,r
    f=datan2(v(2),v(1))+angle
    r=r2Vec_Length(v(1:2))
    w(1)=r*dcos(f)
    w(2)=r*dsin(f)
    w(3)=v(3)
  end Function Rot3DVecZa

  Function Rot3DVecAxisa(eAxis,ex,ey,v,angle) Result(w)
! rotate vector v around axis defined by the unit vector eAxis
! ex and ey are auxiliary unit vectors that must be perpendicular to eAxis
    Real(8) eAxis(3),ex(3),ey(3),v(3),angle,w(3),vloc(3),f,r
    vloc(1)=Dot_Product(v,ex)
    vloc(2)=Dot_Product(v,ey)
    vloc(3)=Dot_Product(v,eAxis)
    f=datan2(vloc(2),vloc(1))+angle
    r=r2Vec_Length(vloc(1:2))
    w=r*dcos(f)*ex+r*dsin(f)*ey+vloc(3)*eAxis
  end Function Rot3DVecAxisa

  Function Rot3DSpaceX(v,angle) Result(w)
    Real(8) v(3,0:3),angle,w(3,0:3)
    Integer(4) i
    do i=0,3
      w(1:3,i)=Rot3DVecX(v(1:3,i),angle)
    end do
  end Function Rot3DSpaceX

  Function Rot3DSpaceY(v,angle) Result(w)
    Real(8) v(3,0:3),angle,w(3,0:3)
    Integer(4) i
    do i=0,3
      w(1:3,i)=Rot3DVecY(v(1:3,i),angle)
    end do
  end Function Rot3DSpaceY

  Function Rot3DSpaceZ(v,angle) Result(w)
    Real(8) v(3,0:3),angle,w(3,0:3)
    Integer(4) i
    do i=0,3
      w(1:3,i)=Rot3DVecZ(v(1:3,i),angle)
    end do
  end Function Rot3DSpaceZ

  Function Rot3DSpaceXa(v,angle) Result(w)
    Real(8) v(3,0:3),angle,w(3,0:3)
    Integer(4) i
    do i=0,3
      w(1:3,i)=Rot3DVecXa(v(1:3,i),angle)
    end do
  end Function Rot3DSpaceXa

  Function Rot3DSpaceYa(v,angle) Result(w)
    Real(8) v(3,0:3),angle,w(3,0:3)
    Integer(4) i
    do i=0,3
      w(1:3,i)=Rot3DVecYa(v(1:3,i),angle)
    end do
  end Function Rot3DSpaceYa

  Function Rot3DSpaceZa(v,angle) Result(w)
    Real(8) v(3,0:3),angle,w(3,0:3)
    Integer(4) i
    do i=0,3
      w(1:3,i)=Rot3DVecZa(v(1:3,i),angle)
    end do
  end Function Rot3DSpaceZa

  Function Dist3DPtPt(P,O) Result(d)
! distance of a point P from a point O
    Real(8) P(3),O(3),d,v(3)
    v=P-O
    d=r3Vec_Length(v)
  end Function Dist3DPtPt

  Subroutine Set3DPtOnAxis(P,O,e)
! set the point P on the nearest point on the axis through point O in direction e (unit vector!)
    Real(8) P(3),O(3),e(3),v(3),x
    v=P-O
    x=Dot_Product(e,v)
    P=O+x*e
  end Subroutine Set3DPtOnAxis

  Function Dist3DPtAxis(P,O,e) Result(d)
! distance of a point P from an axis through point O in direction e (unit vector!)
    Real(8) P(3),O(3),e(3),d,v(3),r,x
    v=P-O
    r=r3Vec_Length(v)
    x=Dot_Product(e,v)
    d=dsqrt(abs(r**2-x**2))
  end Function Dist3DPtAxis

  Function Dist3DPtPlane(P,plane) Result(d)
! distance of a point P from a plane (defined with three unit direction vectors!)
    Real(8) P(3),plane(3,0:3),d,v(3)
    v=P-plane(1:3,0)
    d=r3Scl_Prod(v,plane(1:3,3))
  end Function Dist3DPtPlane

  Subroutine Ortho3DSpace2(space,n)
    Real(8) space(3,0:3),s(3,0:3)
    Integer(2) n
    if(n.eq.3_2) then
      s(1:3,0)=space(1:3,0)
      s(1:3,1)=space(1:3,3)
      s(1:3,2)=space(1:3,1)
      s(1:3,3)=space(1:3,2)
      call Ortho3DSpace(s)
      space(1:3,3)=s(1:3,1)
      space(1:3,1)=s(1:3,2)
      space(1:3,2)=s(1:3,3)
    else if(n.eq.2_2) then
      s(1:3,0)=space(1:3,0)
      s(1:3,1)=space(1:3,2)
      s(1:3,2)=space(1:3,3)
      s(1:3,3)=space(1:3,1)
      call Ortho3DSpace(s)
      space(1:3,2)=s(1:3,1)
      space(1:3,3)=s(1:3,2)
      space(1:3,1)=s(1:3,3)
    else
      call Ortho3DSpace(space)
    end if
  end Subroutine Ortho3DSpace2

  Subroutine Ortho3DSpace(space)
! the array space contains four vectors: location and three direction vectors
! this routine replaces the direction vectors by three orthogonal unit vectors
    Real(8) space(3,0:3),v(3)
    call Unit3DV(space(1:3,1))
    call Unit3DV(space(1:3,2))
    call Unit3DV(space(1:3,3))
    v=r3Vec_Prod(space(1:3,1),space(1:3,2))
    if(r3Vec_Length(v).lt.1.0d-14) then ! first two vectors have same direction
      v=r3Vec_Prod(space(1:3,1),space(1:3,3))
      if(r3Vec_Length(v).lt.1.0d-14) then ! all vectors have same direction
        space(1:3,2)=0.0d0 ! set second vector=ex
        space(1,2)=1.0d0
        v=r3Vec_Prod(space(1:3,1),space(1:3,2))
        if(r3Vec_Length(v).lt.1.0d-14) then
          space(1:3,2)=0.0d0 ! set second vector=ey
          space(2,2)=1.0d0
          v=r3Vec_Prod(space(1:3,1),space(1:3,2))
        end if
      else
        space(1:3,3)=r3Vec_Prod(v,space(1:3,1)) ! use third vector, ignore socond vector
        call Unit3DV(space(1:3,3))
        space(1:3,2)=r3Vec_Prod(space(1:3,3),space(1:3,1))
        call Unit3DV(space(1:3,2))
        return
      end if
    end if
    space(1:3,2)=r3Vec_Prod(v,space(1:3,1))
    call Unit3DV(space(1:3,2))
    space(1:3,3)=r3Vec_Prod(space(1:3,1),space(1:3,2))
    call Unit3DV(space(1:3,3))
  end Subroutine Ortho3DSpace

  Subroutine Ortho3DSpace3(v1,v2,v3)
! three direction vectors v1,v2,v3 are replaced by an orthonormal set of vectors
    Real(8) v1(3),v2(3),v3(3),v(3)
    call Unit3DV(v1)
    call Unit3DV(v2)
    call Unit3DV(v3)
    v=r3Vec_Prod(v1,v2)
    if(r3Vec_Length(v).lt.1.0d-14) then ! first two vectors have same direction
      v=r3Vec_Prod(v1,v3)
      if(r3Vec_Length(v).lt.1.0d-14) then ! all vectors have same direction
        v2=0.0d0 ! set second vector=ex
        v2(1)=1.0d0
        v=r3Vec_Prod(v1,v2)
        if(r3Vec_Length(v).lt.1.0d-14) then
          v2=0.0d0 ! set second vector=ey
          v2(2)=1.0d0
          v=r3Vec_Prod(v1,v2)
        end if
      else
        v3=r3Vec_Prod(v,v1) ! use third vector, ignore socond vector
        call Unit3DV(v3)
        v2=r3Vec_Prod(v3,v1)
        call Unit3DV(v2)
        return
      end if
    end if
    v2=r3Vec_Prod(v,v1)
    call Unit3DV(v2)
    v3=r3Vec_Prod(v1,v2)
    call Unit3DV(v3)
  end Subroutine Ortho3DSpace3

  Subroutine RotTran3D(axi0,axit,r0,dr,da,dz,r,t)
! perform following transformations on point r0:
! 1) rotation around axis (origin axi0, tangent vector axit), angle da
! 2) translation in direction axit, length dz
! 3) radial translation (perpendicular to axis), length dr
! new position r
! if t present: compute unit tangent vector t in point r
    Real(8) axi0(3),axit(3),r0(3),dr,da,dz,r(3),ro(3),roe(3),roeo(3),v(3),z(3),zs,ros,co,si
    Real(8), optional :: t(3)
    v=r0-axi0
    zs=r3Scl_Prod(v,axit)
    ro=v-zs*axit
    ros=r3Vec_Length(ro)
    if((ros.lt.1.0D-100).or.((abs(dr).lt.1.0D-100).and.(abs(da).lt.1.0D-100))) then
! translation in direction axit only
      r=r0+dz*axit
      if(Present(t)) then
        t=axit
      end if
    else
      zs=zs+dz
      z=zs*axit
      roe=ro/ros
      roeo=r3Vec_Prod(axit,roe)
      ros=ros+dr
      co=ros*dcos(da)
      si=ros*dsin(da)
      roe=co*roe
      roeo=si*roeo
      ro=roe+roeo
      v=ro+axi0
      r=v+z
      if(Present(t)) then
        ros=r3Vec_Length(ro)
        roe=dr*ro/ros
        roeo=da*r3Vec_Prod(axit,ro)
        t=dz*axit+roe+roeo
        call Unit3DV(t)
      end if
    end if
  end Subroutine RotTran3D

  Real(8) Function SpiralLength(r0,dr,da,dz)
    Real(8) r0,dr,da,dz,a0,b0,c0,s0,u0,c1,u1,a2,c2
    if(dabs(dr).lt.1.0d-100) then ! helix, no radial increment
      SpiralLength=dsqrt(dz**2+((r0*Pi*da/180.0d0)**2))
      return
    end if
    if(dabs(da).lt.1.0d-100) then ! cone, no angular increment
      SpiralLength=dsqrt(dz**2+dr**2)
      return
    end if
    a0=dr+r0
    b0=(a0/dr)*(Pi*da/180.0d0)
    c0=(a0/dr)*dz
    a2=a0**2
    c2=a2*(b0**2)
    a2=a2+(c0**2)
    if(c2.gt.1.0d-100) then ! special case
      c1=dsqrt(c2)
      u1=dsqrt(a2+c2)
      s0=r0/a0
      u0=dsqrt(a2+c2*(s0**2))
      SpiralLength=0.5d0*((u1+a2*dlog(c1+u1)/c1)-(s0*u0+a2*dlog(s0*c1+u0)/c1))
    else ! special case
      SpiralLength=dsqrt(dr**2+dz**2)
    end if
  end Function SpiralLength

  Subroutine getTriangle2DData(PA,PB,PC,a,b,c,s,F,ha,hb,hc)
! data od a 2D triangle with the points PA,PB,PC
    Implicit none
    Real(8) PA(2),PB(2),PC(2),a,b,c
    Real(8), Optional :: s,F,ha,hb,hc
    a=r2Vec_Length(PC-PB)
    b=r2Vec_Length(PA-PC)
    c=r2Vec_Length(PB-PA)
    if(Present(s)) s=0.5d0*(a+b+c)
    if(Present(F)) F=dsqrt(s*(s-a)*(s-b)*(s-c))
    if(Present(ha)) ha=2.0d0*F/a
    if(Present(hb)) hb=2.0d0*F/b
    if(Present(hc)) hc=2.0d0*F/c
  end Subroutine getTriangle2DData

  Subroutine getMPforTriangle2D(PA,PB,PC,N,fO,Oi,ri)
! construct N origins Oi and radii of circles on the winkelhalbierenden of a 2D triangle with
! the corners PA,PB,PC (positive orientation!) In such a way that the circles do not intersect
! each other (provided that the factor fO is set to 1 or >1). Note that each circle touches 
! at least two sides of the triangle
! These circles may be used for the 3D multipole setting for a triangular boundary element.
    Implicit none
    Integer(4) N,i
    Real(8) PA(2),PB(2),PC(2),fO,Oi(2,N),ri(N),a,b,c,s,sa2,sb2,sc2,va(2),vb(2),vc(2),fa,fb,fc,ra,rb,rc
    if(n.lt.1) return
    call getTriangle2DData(PA,PB,PC,a,b,c,s)
    ri(1)=dsqrt(((s-a)*(s-b)*(s-c))/s)
    sa2=ri(1)/dsqrt(ri(1)**2+(s-a)**2)
    vb=Unit2DVec(PB-PA)
    vc(1)=-vb(2)
    vc(2)=vb(1)
    va=(s-a)*vb+ri(1)*vc
    Oi(1:2,1)=PA(1:2)+va(1:2)
    if(n.lt.2) return
    vb(1:2)=Oi(1:2,1)-PB(1:2)
    vc(1:2)=Oi(1:2,1)-PC(1:2)
    sb2=ri(1)/dsqrt(ri(1)**2+(s-b)**2)
    sc2=ri(1)/dsqrt(ri(1)**2+(s-c)**2)
    fa=(1.0d0-sa2)/(1.0d0+sa2)
    fb=(1.0d0-sb2)/(1.0d0+sb2)
    fc=(1.0d0-sc2)/(1.0d0+sc2)
    if(fO.gt.0.0d0) then
      fa=fa**fO
      fb=fb**fO
      fc=fc**fO
    end if
    ra=fa*ri(1)
    rb=fb*ri(1)
    rc=fc*ri(1)
    do i=2,N
      if((ra.ge.rb).and.(ra.ge.rc)) then
        ri(i)=ra
        va=fa*va
        Oi(1:2,i)=PA(1:2)+va(1:2)
        ra=fa*ra
      else if(rb.ge.rc) then
        ri(i)=rb
        vb=fb*vb
        Oi(1:2,i)=PB(1:2)+vb(1:2)
        rb=fb*rb
      else
        ri(i)=rc
        vc=fc*vc
        Oi(1:2,i)=PC(1:2)+vc(1:2)
        rc=fc*rc
      end if
    end do
  end Subroutine getMPforTriangle2D

  Subroutine getMPforRectangle2D(A,B,N,Oi,ri)
! construct N origins Oi and radii of circles in a 2D Rectangle with
! the corners (0,0),(A,0),(A,B),(0,B) In such a way that the circles do not intersect
! each other.
! These circles may be used for the 3D multipole setting for a rectangular boundary element.
    Implicit none
    Integer(4) N,i
    Real(8) A,B,Oi(2,N),ri(N),d,x,y
    if(n.lt.1) return
    if(A.gt.B) then
      N=min(N,nint(A/B,4))
      if(N.lt.2) then
        Oi(1,1)=0.5d0*A
        Oi(2,1)=0.5d0*B
        ri(1)=0.5d0*B
      else
        d=A/Dble(N)
        x=0.5d0*d
        do i=1,N
          Oi(1,i)=x
          Oi(2,i)=0.5d0*B
          ri(i)=0.5d0*B
          x=x+d
        end do
      end if
    else
      N=min(N,nint(B/A,4))
      if(N.lt.2) then
        Oi(1,1)=0.5d0*A
        Oi(2,1)=0.5d0*B
        ri(1)=0.5d0*A
      else
        d=B/Dble(N)
        y=0.5d0*d
        do i=1,N
          Oi(1,i)=0.5d0*A
          Oi(2,i)=y
          ri(i)=0.5d0*A
          y=y+d
        end do
      end if
    end if
  end Subroutine getMPforRectangle2D

  Subroutine genArc0(alpha,xA,yA,N)
! generate corners xA,yA of a Cpoly defining an arc with radius 1, angle alpha, around (0,0), starting at (1,0)
    Implicit none
    Integer(4) N
    Real(8) alpha,xA(6),yA(6)
    N=0
    xA(1)=1.0d0
    yA(1)=0.0d0
    if((alpha.lt.0.0d0).or.(alpha.gt.360.0d0)) return
    if(alpha.le.90.0d0) then
      N=3
      xA(2)=1.0d0
      yA(2)=dtand(0.5d0*alpha)
      xA(3)=dcosd(alpha)
      yA(3)=dsind(alpha)
    else if(alpha.le.180.0d0) then
      N=4
      xA(2)=1.0d0
      yA(2)=1.0d0
      xA(3)=-dtand(0.5d0*(alpha-90.0d0))
      yA(3)=1.0d0
      xA(4)=-dsind(alpha-90.0d0)
      yA(4)=dcosd(alpha-90.0d0)
    else if(alpha.le.270.0d0) then
      N=5
      xA(2)=1.0d0
      yA(2)=1.0d0
      xA(3)=-1.0d0
      yA(3)=1.0d0
      xA(4)=-1.0d0
      yA(4)=-dtand(0.5d0*(alpha-180.0d0))
      xA(5)=-dcosd(alpha-180.0d0)
      yA(5)=-dsind(alpha-180.0d0)
    else
      N=6
      xA(2)=1.0d0
      yA(2)=1.0d0
      xA(3)=-1.0d0
      yA(3)=1.0d0
      xA(4)=-1.0d0
      yA(4)=-1.0d0
      xA(5)=dtand(0.5d0*(alpha-270.0d0))
      yA(5)=-1.0d0
      xA(6)=dsind(alpha-270.0d0)
      yA(6)=-dcosd(alpha-270.0d0)
    end if
  end Subroutine genArc0

  Subroutine genArc1(xO,yO,r,alpha0,alpha,xA,yA,N)
! generate corners xA,yA of a Cpoly defining an arc with radius r, angle alpha, around (xO,yO), starting at angle alpha0
    Implicit none
    Integer(4) N,i
    Real(8) xO,yO,r,alpha0,alpha,xA(6),yA(6),rl,fl,f0
! generate arc with radius 1, alpha0=0, xO=yO=0
    call genArc0(alpha,xA,yA,N)
    if(N.lt.3) return
! rotate with angle alpha0, stretch with R, move to xO,yO
    f0=Pi*alpha0/180.0d0
    do i=1,N
      call Cart2Pol(xA(i),yA(i),rl,fl)
      fl=fl+f0
      call Pol2Cart(rl,fl,xA(i),yA(i))
      xA(i)=r*xA(i)+xO
      yA(i)=r*yA(i)+yO
    end do
  end Subroutine genArc1

  Subroutine genArc2(xO,yO,xS,yS,alpha,xA,yA,N)
! generate corners xA,yA of a Cpoly defining an arc with angle alpha, around (xO,yO), starting at (xS,yS)
    Implicit none
    Integer(4) N
    Real(8) xO,yO,xS,yS,alpha0,r,alpha,xA(6),yA(6)
    call DistPtPt(xO,yO,xS,yS,r)
    alpha0=datan2d(yS-yO,xS-xO)
    call genArc1(xO,yO,r,alpha0,alpha,xA,yA,N)
  end Subroutine genArc2

  Subroutine RTR(r,n,m,t,mt)
! product t = rT r, where r is a rectangular matrix, t symmetric, only upper triangle stored
    Integer(4) n,i
    Real(8) r(n,m),t(mt)
    ind=0
    do k=1,m
      do i=1,k
        ind=ind+1
        t(ind)=sum(r(1:n,i)*r(1:n,k))
      end do
    end do
  end Subroutine RTR

! sorting

  Recursive Subroutine QuickSort(X,L,R)
! quick sort the elements L up to R of the array X
    Real(8), Dimension(:) :: X
    Integer(4) L,R,L1,R1
    Real(8) rdum
    if(L<R) then
      L1=L; R1=R
      do
        do while(L1<R.and.X(L1)<=X(L))  ! shift L1 right
          L1=L1+1
        end do
        do while(L<R1.and.X(R1)>=X(L))  ! shift R1 left
          R1=R1-1
        end do
        if(L1<R1) then
          rdum=X(L1); X(L1)=X(R1); X(R1)=rdum  ! swop
        else
          Exit                                 ! crossover partition
        end if
      end do
      rdum=X(L); X(L)=X(R1); X(R1)=rdum        ! partition with X(L) at R1
      call QuickSort(X,L,R1-1)            ! now attack left subproblem
      call QuickSort(X,R1+1,R)            ! don't forget right subproblem
    end if
  end Subroutine QuickSort

  Recursive Subroutine QuickIndex(X,L,R,I,Linit)
! quick sort the elements L up to R of the array X and generate the index array I
! initialize the index if Linit=true
    Real(8), Dimension(:) :: X
    Integer(4), Dimension(:) :: I
    Integer(4) L,R,L1,R1,j,idum
    Real(8) rdum
    Logical, intent(in) :: Linit
    if(Linit) then
      do j=1,R-L+1
        I(j)=j
      end do
    end if
    if(L<R) then
      L1=L; R1=R
      do
        do while(L1<R.and.X(L1)<=X(L))
          L1=L1+1
        end do
        do while(L<R1.and.X(R1)>=X(L))
          R1=R1-1
        end do
        if(L1<R1) then
          rdum=X(L1); X(L1)=X(R1); X(R1)=rdum; idum=I(L1); I(L1)=I(R1); I(R1)=idum
        else
          Exit
        end if
      end do
      rdum=X(L); X(L)=X(R1); X(R1)=rdum; idum=I(L); I(L)=I(R1); I(R1)=idum
      call QuickIndex(X,L,R1-1,I,.false.)
      call QuickIndex(X,R1+1,R,I,.false.)
    end if
  end Subroutine QuickIndex

  Subroutine RIndexSort(X,N,I,iOK)
! sort the array x with n elements according to the index array i
    Real(8), Dimension(:) :: X
    Integer(4), Dimension(:) :: I
    Integer(4) N,j,iOK
    Real(8), Allocatable :: Y(:)
    if(Allocated(Y)) Deallocate(Y)
    Allocate(Y(1:N),Stat=j)
    if(j.ne.0) then
      iOK=2
    else
      iOK=0
      Y(1:N)=X(1:N)
      X(1:N)=Y(I(1:N))
    end if
    DeAllocate(Y,Stat=j)
    if(j.ne.0) iOK=iOK+1
  end Subroutine RIndexSort

  Subroutine IIndexSort(K,N,I,iOK)
! sort the array K with n elements according to the index array i
    Integer(4), Dimension(:) :: I,K
    Integer(4) N,j,iOK
    Integer(4), Allocatable :: L(:)
    if(Allocated(L)) Deallocate(L)
    Allocate(L(1:N),Stat=j)
    if(j.ne.0) then
      iOK=2
    else
      iOK=0
      L(1:N)=K(1:N)
      K(1:N)=L(I(1:N))
    end if
    DeAllocate(L,Stat=j)
    if(j.ne.0) iOK=iOK+1
  end Subroutine IIndexSort

  Subroutine SIndexSort(S,L,N,I,iOK)
! sort the array S with n elements according to the index array i
! L=max.length of the character strings in S
    Character(*), Dimension(:) :: S
    Integer(4), Dimension(:) :: I
    Integer(4) L,N,j,iOK
    Character(1), Allocatable :: T(:)
    iOK=1
    if(Allocated(T)) Deallocate(T)
    Allocate(T(1:N),Stat=j)
    if(j.ne.0) then
      iOK=2
    else
      iOK=0
      do j=1,L
        T(1:N)=S(1:N)(j:j)
        S(1:N)(j:j)=T(I(1:N))
      end do
    end if
    DeAllocate(T,Stat=j)
    if(j.ne.0) iOK=iOK+1
  end Subroutine SIndexSort

  Subroutine RInvArr(X,N)
! invert the ordering of the array X with n elements
    Real(8), Dimension(:) :: X
    Real(8) Y
    Integer(4) N,k
    do k=1,n/2
      Y=X(k)
      X(k)=X(N-k+1)
      X(N-k+1)=Y
    end do
  end Subroutine RInvArr

  Subroutine IInvArr(I,N)
! invert the ordering of the array I with n elements
    Integer(4), Dimension(:) :: I
    Integer(4) N,k,J
    do k=1,n/2
      J=I(k)
      I(k)=I(N-k+1)
      I(N-k+1)=J
    end do
  end Subroutine IInvArr

! auxiliary

  Subroutine MinMax(arr,mval,marg,nval,iarg,amin,amax)
    Implicit Real(8) (a-h,o-z)
    Implicit Integer(4) (i-m)
    Dimension arr(marg,mval)
    amin=0.0d0
    amax=0.0d0
    if((iarg.gt.marg).or.(iarg.lt.1).or.(nval.lt.1)) return
    amin=arr(iarg,1)
    amax=arr(iarg,1)
    do i=2,nval
      if(arr(iarg,i).lt.amin) then
        amin=arr(iarg,i)
      elseif(arr(iarg,i).gt.amax) then
        amax=arr(iarg,i)
      end if
    end do
  end Subroutine MinMax

  Integer(4) Function in_Rinterval(r,rMin,rMax,iMin,iMax,lLog) Result(i)
    Implicit Real(8) (a-h,o-z)
    Implicit Integer(4) (i-m)
    Logical, Optional, intent(in) :: lLog
    Logical llLog
    llLog=.false.
    if(Present(lLog)) then
      if(lLog.and.(r.gt.pSmall).and.(rmin.gt.pSmall).and.(rmax.gt.pSmall)) llLog=.true.
    end if
    if(r.le.rMin) then
      i=iMin
    else if(r.ge.rMax) then
      i=iMax
    else if(llLog) then
      i=nint(Dble(iMin)+Dble(iMax-iMin)*((log(r)-log(rMin))/(log(rMax)-log(rMin))),4)
    else
      i=nint(Dble(iMin)+Dble(iMax-iMin)*((r-rMin)/(rMax-rMin)),4)
    end if
  end Function in_Rinterval

  Function cdphi(c)
    Complex(8), Intent(In) :: c
    Real(8) :: a1,a2
    Real(8) :: cdphi
    a1=Dble(c)
    a2=DImag(c)
    if((dabs(a1).lt.pSmall).and.(dabs(a2).lt.pSmall)) then
      cdphi=0.0d0
    else
      cdphi=datan2d(a2,a1)
    end if
  end Function cdphi

  Function cdPhiAbs(l,c)
    Complex(8), Intent(In) :: c
    Real(8) :: cdPhiAbs
    Logical l
    if(l) then
      cdPhiAbs=cdPhi(c)
    else
      cdphiAbs=cdAbs(c)
    end if
  end Function cdPhiAbs

  Function cdPhiRea(l,c)
    Complex(8), Intent(In) :: c
    Real(8) :: cdPhiRea
    Logical l
    if(l) then
      cdPhiRea=cdPhi(c)
    else
      cdPhiRea=Dble(c)
    end if
  end Function cdPhiRea

  Subroutine R2Color(r,iCmin,iCmax,iC)
! get color number iC from real parameter -1<r<1 
! resulting value is within range iCmin...iCmax for r>0
! Note: standard colors      0... 15
!       color palette       16...115
!       inv. color palette 116...215
!       gray palette       216...235
    Implicit None
    Real(8) r
    Integer(2) iCmin,iCmax,iC
    if(r.gt.0.0d0) then
      iC=iCmin+nint(Dble(iCmax-iCmin)*abs(r),2)
    else
      iC=100_2+iCmax-nint(Dble(iCmax-iCmin)*abs(r),2)
    end if
  end Subroutine R2Color

  Subroutine Getik(ik,Ni,i,k)
! get the two indices i and k of a 2D grid from the (1D) index ik=k+(i-1)*Ni
! Ni is the number of grid lines in i direction
    Integer(4) ik,Ni,i,k
    i=1+(ik-1_4)/Ni
    k=ik-Ni*(i-1)
  end Subroutine Getik

  Logical Function Is2DPtOnLine(P,A,B,w,d)
    Implicit None
    Real(8) P(2),A(2),B(2),w,E(2),V(2),s,t,aa
    Real(8), Optional :: d
    Is2DPtOnLine=.false. ! first check: along line
    E=B-A
    call Unit2DV(E,aa)
    V=P-A
    s=r2Scl_Prod(V,E)
    if(Present(d)) d=s.div.aa
    if((s.lt.(-w)).or.(s.gt.(aa+w))) return
    E=r2Vec_Ortho(E) ! second check: perpendicular to line
    t=r2Scl_Prod(V,E)
    if(abs(t).gt.w) return
    Is2DPtOnLine=.true.
    if((s.ge.0.0d0).and.(s.le.aa)) return
    if(s.gt.0.0d0) V=P-B ! third check: line end points (style: circles)
    s=r2Vec_Length(V)
    if(s.gt.w) Is2DPtOnLine=.false.
  end Function Is2DPtOnLine

  Logical Function Is2DPtInTriangle(P,A,B,C)
    Implicit None
    Real(8) P(2),A(2),B(2),C(2),s,sc
    Is2DPtInTriangle=.false.
    sc=Dist2DPtLine(C,A,B)
    if(sc.le.0.0d0) then
      s=Dist2DPtLine(P,A,B)
      if(s.gt.0.0d0) return
      s=Dist2DPtLine(P,B,C)
      if(s.gt.0.0d0) return
      s=Dist2DPtLine(P,C,A)
      if(s.gt.0.0d0) return
    else
      s=Dist2DPtLine(P,A,B)
      if(s.lt.0.0d0) return
      s=Dist2DPtLine(P,B,C)
      if(s.lt.0.0d0) return
      s=Dist2DPtLine(P,C,A)
      if(s.lt.0.0d0) return
    end if
    Is2DPtInTriangle=.true.
  end Function Is2DPtInTriangle

  Real(8) Function Dist2DPtLine(P,A,B)
    Implicit None
    Real(8) P(2),A(2),B(2),E(2)
    E=r2Vec_Ortho(B-A)
    call Unit2DV(E)
    Dist2DPtLine=r2Scl_Prod(P-A,E)
  end Function Dist2DPtLine

  Function Inter22Scal(r,r1,r2,f1,f2) Result(f)
    Real(8) r(2),r1(2),r2(2),f,f1,f2,d1,d2
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r2-r)
    f=(d1*f2+d2*f1)/(d1+d2)
  end Function Inter22Scal

  Function Inter32Scal(r,r1,r2,r3,f1,f2,f3) Result(f)
    Real(8) r(2),r1(2),r2(2),r3(2),f,f1,f2,f3,d1,d2,d3,a1,a2,a3
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    d3=r2Vec_Length(r-r3)
    a1=d2*d3
    a2=d1*d3
    a3=d1*d2
    f=(a1*f1+a2*f2+a3*f3)/(a1+a2+a3)
  end Function Inter32Scal

  Function Inter42Scal(r,r1,r2,r3,r4,f1,f2,f3,f4) Result(f)
    Real(8) r(2),r1(2),r2(2),r3(2),r4(2),f,f1,f2,f3,f4,d1,d2,d3,d4,a1,a2,a3,a4
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    d3=r2Vec_Length(r-r3)
    d4=r2Vec_Length(r-r4)
    a1=d2*d3*d4
    a2=d1*d3*d4
    a3=d1*d2*d4
    a4=d1*d2*d3
    f=(a1*f1+a2*f2+a3*f3+a4*f4)/(a1+a2+a3+a4)
  end Function Inter42Scal

  Function Inter23Scal(r,r1,r2,f1,f2) Result(f)
    Real(8) r(3),r1(3),r2(3),f,f1,f2,d1,d2
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r2-r)
    f=(d1*f2+d2*f1)/(d1+d2)
  end Function Inter23Scal

  Function Inter33Scal(r,r1,r2,r3,f1,f2,f3) Result(f)
    Real(8) r(3),r1(3),r2(3),r3(3),f,f1,f2,f3,d1,d2,d3,a1,a2,a3
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r-r2)
    d3=r3Vec_Length(r-r3)
    a1=d2*d3
    a2=d1*d3
    a3=d1*d2
    f=(a1*f1+a2*f2+a3*f3)/(a1+a2+a3)
  end Function Inter33Scal

  Function Inter43Scal(r,r1,r2,r3,r4,f1,f2,f3,f4) Result(f)
    Real(8) r(3),r1(3),r2(3),r3(3),r4(3),f,f1,f2,f3,f4,d1,d2,d3,d4,a1,a2,a3,a4
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r-r2)
    d3=r3Vec_Length(r-r3)
    d4=r3Vec_Length(r-r4)
    a1=d2*d3*d4
    a2=d1*d3*d4
    a3=d1*d2*d4
    a4=d1*d2*d3
    f=(a1*f1+a2*f2+a3*f3+a4*f4)/(a1+a2+a3+a4)
  end Function Inter43Scal

  Function Inter22Fld(r,r1,r2,f1,f2) Result(f)
    Real(8) r(2),r1(2),r2(2),f(3),f1(3),f2(3),d1,d2
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    f=(d1*f2+d2*f1)/(d1+d2)
  end Function Inter22Fld

  Function Inter32Fld(r,r1,r2,r3,f1,f2,f3) Result(f)
    Real(8) r(2),r1(2),r2(2),r3(2),f(3),f1(3),f2(3),f3(3),d1,d2,d3,a1,a2,a3
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    d3=r2Vec_Length(r-r3)
    a1=d2*d3
    a2=d1*d3
    a3=d1*d2
    f=(a1*f1+a2*f2+a3*f3)/(a1+a2+a3)
  end Function Inter32Fld

  Function Inter42Fld(r,r1,r2,r3,r4,f1,f2,f3,f4) Result(f)
    Real(8) r(2),r1(2),r2(2),r3(2),r4(2),f(3),f1(3),f2(3),f3(3),f4(3),d1,d2,d3,d4,a1,a2,a3,a4
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    d3=r2Vec_Length(r-r3)
    d4=r2Vec_Length(r-r4)
    a1=d2*d3*d4
    a2=d1*d3*d4
    a3=d1*d2*d4
    a4=d1*d2*d3
    f=(a1*f1+a2*f2+a3*f3+a4*f4)/(a1+a2+a3+a4)
  end Function Inter42Fld

  Function Inter23Fld(r,r1,r2,f1,f2) Result(f)
    Real(8) r(3),r1(3),r2(3),f(3),f1(3),f2(3),d1,d2
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r2-r)
    f=(d1*f2+d2*f1)/(d1+d2)
  end Function Inter23Fld

  Function Inter33Fld(r,r1,r2,r3,f1,f2,f3) Result(f)
    Real(8) r(3),r1(3),r2(3),r3(3),f(3),f1(3),f2(3),f3(3),d1,d2,d3,a1,a2,a3
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r-r2)
    d3=r3Vec_Length(r-r3)
    a1=d2*d3
    a2=d1*d3
    a3=d1*d2
    f=(a1*f1+a2*f2+a3*f3)/(a1+a2+a3)
  end Function Inter33Fld

  Function Inter43Fld(r,r1,r2,r3,r4,f1,f2,f3,f4) Result(f)
    Real(8) r(3),r1(3),r2(3),r3(3),r4(3),f(3),f1(3),f2(3),f3(3),f4(3),d1,d2,d3,d4,a1,a2,a3,a4
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r-r2)
    d3=r3Vec_Length(r-r3)
    d4=r3Vec_Length(r-r4)
    a1=d2*d3*d4
    a2=d1*d3*d4
    a3=d1*d2*d4
    a4=d1*d2*d3
    f=(a1*f1+a2*f2+a3*f3+a4*f4)/(a1+a2+a3+a4)
  end Function Inter43Fld

  Function Inter22cFld(r,r1,r2,f1,f2) Result(f)
    Complex(8) f1(10),f2(10),f(10)
    Real(8) r(2),r1(2),r2(2),d1,d2
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r2-r)
    f=(d1*f2+d2*f1)/(d1+d2)
  end Function Inter22cFld

  Function Inter32cFld(r,r1,r2,r3,f1,f2,f3) Result(f)
    Complex(8) f1(10),f2(10),f3(10),f(10)
    Real(8) r(2),r1(2),r2(2),r3(2),d1,d2,d3,a1,a2,a3
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    d3=r2Vec_Length(r-r3)
    a1=d2*d3
    a2=d1*d3
    a3=d1*d2
    f=(a1*f1+a2*f2+a3*f3)/(a1+a2+a3)
  end Function Inter32cFld

  Function Inter42cFld(r,r1,r2,r3,r4,f1,f2,f3,f4) Result(f)
    Complex(8) f1(10),f2(10),f3(10),f4(10),f(10)
    Real(8) r(2),r1(2),r2(2),r3(2),r4(2),d1,d2,d3,d4,a1,a2,a3,a4
    d1=r2Vec_Length(r-r1)
    d2=r2Vec_Length(r-r2)
    d3=r2Vec_Length(r-r3)
    d4=r2Vec_Length(r-r4)
    a1=d2*d3*d4
    a2=d1*d3*d4
    a3=d1*d2*d4
    a4=d1*d2*d3
    f=(a1*f1+a2*f2+a3*f3+a4*f4)/(a1+a2+a3+a4)
  end Function Inter42cFld

  Function Inter23cFld(r,r1,r2,f1,f2) Result(f)
    Complex(8) f1(10),f2(10),f(10)
    Real(8) r(3),r1(3),r2(3),d1,d2
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r2-r)
    f=(d1*f2+d2*f1)/(d1+d2)
  end Function Inter23cFld

  Function Inter33cFld(r,r1,r2,r3,f1,f2,f3) Result(f)
    Complex(8) f1(10),f2(10),f3(10),f(10)
    Real(8) r(3),r1(3),r2(3),r3(3),d1,d2,d3,a1,a2,a3
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r-r2)
    d3=r3Vec_Length(r-r3)
    a1=d2*d3
    a2=d1*d3
    a3=d1*d2
    f=(a1*f1+a2*f2+a3*f3)/(a1+a2+a3)
  end Function Inter33cFld

  Function Inter43cFld(r,r1,r2,r3,r4,f1,f2,f3,f4) Result(f)
    Complex(8) f1(10),f2(10),f3(10),f4(10),f(10)
    Real(8) r(3),r1(3),r2(3),r3(3),r4(3),d1,d2,d3,d4,a1,a2,a3,a4
    d1=r3Vec_Length(r-r1)
    d2=r3Vec_Length(r-r2)
    d3=r3Vec_Length(r-r3)
    d4=r3Vec_Length(r-r4)
    a1=d2*d3*d4
    a2=d1*d3*d4
    a3=d1*d2*d4
    a4=d1*d2*d3
    f=(a1*f1+a2*f2+a3*f3+a4*f4)/(a1+a2+a3+a4)
  end Function Inter43cFld

! generation of 3D lines

	Subroutine Pt2Torus(P,fmin,da0,n0,Location,Poly,iObjDra,nDom,iDL,iDR,iC,PolyX,PolyY,PolyZ,vt1)
! rotate the point P around the y axis, min.angle=fmin, max.angle=fmax -> Poly with abs(n0) points
! place Poly at Location, set color number for the points in the vector iC (only if iDL.ne.-257)
! if iObjDra>1: set iC = iDL,iDR (packed)
! if PolyX,PolyY,PolyZ,vt1 are present, compute the tangential and normal vectors and save them in PolyX,PolyY,PolyZ
    Real(8) P(3),Location(3,0:3),Poly(3,*),r(3),vt2(3),vn(3),fmin,f,da,da0,f0,r0,y
    Real(8), Optional:: PolyX(3,*),PolyY(3,*),PolyZ(3,*),vt1(3)
    Integer(4) n0,n,i
    Integer(4), Optional:: iObjDra,nDom
    Integer(2), Optional:: iDL,iDR,iC(*)
    r=P
    f0=datan2(r(3),r(1))
    r0=dsqrt(r(1)**2+r(3)**2)
    y=r(2)
    n=max(1_4,abs(n0))
    if(n0.lt.2) then
      da=-Pi*abs(da0)/(180.0d0*Dble(n))
      f=f0-Pi*fmin/180.0d0+da*0.5d0
    else
      da=-Pi*abs(da0)/(180.0d0*Dble(n-1))
      f=f0-Pi*fmin/180.0d0
    end if
    if(Present(vt1)) then
      if(dabs(r0).gt.pSmall) then
        vt1=Rot3DVecYa(vt1,-f0)
        vt2(1)=0.0d0
        vt2(2)=0.0d0
        vt2(3)=-r0*da
        vn=r3Vec_Prod(vt1,vt2)
        vt1=Rot3DVecYa(vt1,f)
        vt2=Rot3DVecYa(vt2,f)
        vn=Rot3DVecYa(vn,f)
      else
        vt1=0.0d0
        vt2=0.0d0
        vn=0.0d0
        vt1(1)=1.0d0
        vt2(3)=1.0d0
        vn(2)=-1.0d0
      end if
    end if
    do i=1,n
      r(1)=r0*dcos(f)
      r(2)=y
      r(3)=-r0*dsin(f)
      call vLoc2Glob(r,Location,Poly(1:3,i))
      if(Present(iC)) then
        if(iDL.ne.-257_2) then
          if(iObjDra.gt.1) then ! set domain numbers
            iC(i)=iPackDom(nDom,iDL,iDR)
          else
            iC(i)=-30001_2
          end if
        end if
        if(Present(vt1)) then
          call rvLoc2Glob(vt1,Location,PolyX(1:3,i))
          call rvLoc2Glob(vt2,Location,PolyY(1:3,i))
          call rvLoc2Glob(vn, Location,PolyZ(1:3,i))
          vt1=Rot3DVecYa(vt1,da)
          vt2=Rot3DVecYa(vt2,da)
          vn=Rot3DVecYa(vn,da)
        end if
      end if
      f=f+da
    end do
	end Subroutine Pt2Torus

	Subroutine Pt2Cylinder(P,zmin,dz0,n0,Location,Poly,iObjDra,nDom,iDL,iDR,iC,PolyX,PolyY,PolyZ,vt1)
! move the point P along the z axis -> Poly with abs(n0) points
! place Poly at Location, set color number for the points in the vector iC (only if iDL.ne.-257)
! if iObjDra>1: set iC = iDL,iDR (packed)
! if PolyX,PolyY,PolyZ,vt1 are present, compute the tangential and normal vectors and save them in PolyX,PolyY,PolyZ
    Real(8) P(3),Location(3,0:3),Poly(3,*),r(3),vt2(3),vn(3),zmin,z,dz,dz0
    Real(8), Optional:: PolyX(3,*),PolyY(3,*),PolyZ(3,*),vt1(3)
    Integer(4) n0,n,i
    Integer(4), Optional:: iObjDra,nDom
    Integer(2), Optional:: iDL,iDR,iC(*)
    r=P
    n=max(1_4,abs(n0))
    if(n0.lt.2) then
      dz=abs(dz0)/Dble(n)
      z=zmin+r(3)+dz*0.5d0
    else
      dz=abs(dz0)/Dble(n-1)
      z=zmin+r(3)
    end if
    if(Present(vt1)) then
      vt2(1)=0.0d0
      vt2(2)=0.0d0
      vt2(3)=dz
      vn=r3Vec_Prod(vt1,vt2)
    end if
    do i=1,n
      r(3)=z
      call vLoc2Glob(r,Location,Poly(1:3,i))
      if(Present(iC)) then
        if(iDL.ne.-257_2) then
          if(iObjDra.gt.1) then ! use domain colors
            iC(i)=iPackDom(nDom,iDL,iDR)
          else
            iC(i)=-30001_2
          end if
        end if
        if(Present(vt1)) then
          call rvLoc2Glob(vt1,Location,PolyX(1:3,i))
          call rvLoc2Glob(vt2,Location,PolyY(1:3,i))
          call rvLoc2Glob(vn, Location,PolyZ(1:3,i))
        end if
      end if
      z=z+dz
    end do
	end Subroutine Pt2Cylinder

	Subroutine Pt2Spiral(P,O,e,drm,dam0,dzm,n0,Location,Poly,iObjDra,nDom,iDL,iDR,iC,PolyX,PolyY,PolyZ,vt1)
! rotate the point P around the axis with origin O and direction e, 
! radial increment drm, angular increment dam0 (degree), longitudinal increment dzm -> Poly with abs(n0) points
! place Poly at Location, set color number for the points in the vector iC (only if iDL.ne.-257)
! if iObjDra>1: set iC = iDL,iDR (packed)
! if PolyX,PolyY,PolyZ,vt1 are present, compute the tangential and normal vectors and save them in PolyX,PolyY,PolyZ
    Real(8) P(3),O(3),e(3),drm,dam,dam0,dzm,Location(3,0:3),Poly(3,*),f,da,r,dr,z,dz, &
    & r0,z0,ex(3),ey(3),a,v(3),vt2(3),vt(3),vn(3),s
    Real(8), Optional:: PolyX(3,*),PolyY(3,*),PolyZ(3,*),vt1(3)
    Integer(4) n0,n,i,is
    Integer(4), Optional:: iObjDra,nDom
    Integer(2), Optional:: iDL,iDR,iC(*)
    Logical lp,lo
    call Unit3DV(e)
    dam=Pi*dam0/180.0d0
    v=P-O
    z=Dot_Product(e,v)
    r=r3Vec_Length(v)
    r=dsqrt(abs(r**2-z**2))
    f=0.0d0
    r0=r
    z0=z
    ex=v-z*e ! generate unit vectors ex, ey orthogonal to e
    a=r3Vec_Length(ex)
    if(a.lt.1.0d-100) then ! point is on axis -> ex can be arbitrary, but different from e
      ex=0.0d0
      ex(1)=1.0d0
      a=Dot_Product(e,ex)
      if(a.lt.1.0d-100) then
        ex=0.0d0
        ex(2)=1.0d0
      end if
    else
      ex=ex/a
    end if
    ey=r3Vec_Prod(e,ex)
    call Unit3DV(ey)
    ex=r3Vec_Prod(ey,e)
    call Unit3DV(ex)
    n=max(1_4,abs(n0)) ! compute number of points and increments
    if(n0.lt.2) then
      dr=drm/Dble(n)
      da=dam/Dble(n)
      dz=dzm/Dble(n)
      r=r+dr*0.5d0
      f=f+da*0.5d0
      z=z+dz*0.5d0
    else
      dr=drm/Dble(n-1)
      da=dam/Dble(n-1)
      dz=dzm/Dble(n-1)
    end if
    if((dabs(dzm).gt.dabs(drm)).and.(dabs(dzm).gt.dabs(r*dam))) then !
      is=1
      s=(z-z0)/dzm
    else if((dabs(drm).gt.dabs(dzm)).and.(dabs(drm).gt.dabs(r*dam))) then
      is=2
      s=(r-r0)/drm
    else
      is=3
      s=f/dam
    end if
    if(Present(vt1)) vt1=Rot3DVecAxisa(e,ex,ey,vt1,f) ! tangential vectors at start point
    vt2=(drm*dcos(dam*s)-(r0+drm*s)*dam*dsin(dam*s))*ex
    vt2=vt2+(drm*dsin(dam*s)+(r0+drm*s)*dam*dcos(dam*s))*ey
    vt2=vt2+dzm*e
    call Unit3DV(vt2)
    lp=.true. ! positive orientation of spiral (+z direction)
    if(vt2(3).lt.0.0d0) lp=.false.
    lo=.true. ! exchange tangential vectors if negative orientation
    if(Present(iC)) then
      if(iDL.ne.-257_2) lo=.false.
    end if
    do i=1,n
      v=O+r*dcos(f)*ex+r*dsin(f)*ey+z*e ! position on spiral
      call vLoc2Glob(v,Location,Poly(1:3,i))
      if(Present(iC)) then ! compute additional info
        if(iDL.ne.-257_2) then ! compute Integer(4) info iC
          if(iObjDra.gt.1) then
            if(lp) then
              iC(i)=iPackDom(nDom,iDL,iDR)
            else
              iC(i)=iPackDom(nDom,iDR,iDL)
            end if
          else
            if(lp) then
              iC(i)=-30001
            else
              iC(i)=-30003
            end if
          end if
        end if
        if(Present(vt1)) then ! compute tangential and normal vectors
          if(is.eq.1) then
            s=(z-z0)/dzm
          else if(is.eq.2) then
            s=(r-r0)/drm
          else
            s=f/dam
          end if
          vt2=(drm*dcos(dam*s)-(r0+drm*s)*dam*dsin(dam*s))*ex
          vt2=vt2+(drm*dsin(dam*s)+(r0+drm*s)*dam*dcos(dam*s))*ey
          vt2=vt2+dzm*e
          call Unit3DV(vt2)
          vn=r3Vec_Prod(vt2,vt1)
          vt=r3Vec_Prod(vn,vt2)
          a=SpiralLength(r,dr,180.0d0*da/Pi,dz)
          vt2=a*vt2
          vn=-a*vn
          if((.not.lo).or.lp) then
            call rvLoc2Glob(vt,Location,PolyX(1:3,i))
            call rvLoc2Glob(vt2,Location,PolyY(1:3,i))
            call rvLoc2Glob(vn, Location,PolyZ(1:3,i))
          else
            call rvLoc2Glob(vt2,Location,PolyX(1:3,i))
            call rvLoc2Glob(vt,Location,PolyY(1:3,i))
            vn=-vn
            call rvLoc2Glob(vn, Location,PolyZ(1:3,i))
          end if
          vt1=Rot3DVecAxisa(e,ex,ey,vt1,da)
        end if
      end if
      r=r+dr
      f=f+da
      z=z+dz
    end do
	end Subroutine Pt2Spiral

	Subroutine Pt2Cone(P,O,zmin,dz0,n0,Location,Poly,iObjDra,nDom,iDL,iDR,iC,PolyX,PolyY,PolyZ,vt1)
! move the point P along the z axis, towards cone origin O -> Poly with abs(n0) points
! place Poly at Location, set color number for the points in the vector iC (only if iDL.ne.-257)
! if iObjDra>1: set iC = iDL,iDR (packed)
! if PolyX,PolyY,PolyZ,vt1 are present, compute the tangential and normal vectors and save them in PolyX,PolyY,PolyZ
    Real(8) P(3),O(3),Location(3,0:3),Poly(3,*),r(3),vt0(3),vt2(3),vn(3),zmin,z,dz,dz0,an
    Real(8), Optional:: PolyX(3,*),PolyY(3,*),PolyZ(3,*),vt1(3)
    Integer(4) n0,n,i
    Integer(4), Optional:: iObjDra,nDom
    Integer(2), Optional:: iDL,iDR,iC(*)
    n=max(1_4,abs(n0))
    r=O-P
    if(n0.lt.2) then ! matching points (centers shifted by dz/2)
      dz=abs(dz0)/Dble(n)
      z=zmin+P(3)+dz*0.5d0
      vt0=r*(dz.div.O(3)) ! increment vector
      r=P+r*(zmin.div.O(3)) ! move point P(z=0) to r(z=zmin)
      r=r+0.5d0*vt0 ! start point in center of first matching point
    else ! grid lines
      dz=abs(dz0)/Dble(n-1)
      z=zmin+P(3)
      vt0=r*(dz.div.O(3))
      r=P+r*(zmin.div.O(3))
    end if
    if(Present(vt1)) then ! tangential and normal vectors required
      vn=r3Vec_Prod(vt1,vt0)
     ! an=r3Vec_Length(vn)
     ! vt2=r3Vec_Prod(vn,vt1)
     ! ann=r3Vec_Length(r3Vec_Prod(vt1,vt2))
     ! vt2=vt2*(an/ann)
      vt2=vt0
    end if
    do i=1,n
      call vLoc2Glob(r,Location,Poly(1:3,i))
      if(Present(iC)) then
        if(iDL.ne.-257_2) then
          if(iObjDra.gt.1) then ! use domain colors
            iC(i)=iPackDom(nDom,iDL,iDR)
          else
            iC(i)=-30001_2
          end if
        end if
        if(Present(vt1)) then
          an=(z-O(3)).div.O(3)
          call rvLoc2Glob(an*vt1,Location,PolyX(1:3,i))
          call rvLoc2Glob(vt2,Location,PolyY(1:3,i))
          call rvLoc2Glob(an*vn, Location,PolyZ(1:3,i))
        end if
      end if
      r=r+vt0
      z=z+dz
    end do
	end Subroutine Pt2Cone
  
  Integer(2) Function iPackDom(nDom,iDL0,iDR0)
! pack domain numbers in iC
    Implicit None
    Integer(2) iDL,iDR,iDL0,iDR0
    Integer(4) nDom
    iDL=iDL0
    iDR=iDR0
    if(iDL.lt.0) then
      iDL=min(235_2,226_2-iDL)
    else
      iDL=min(226_2,nDom,iDL)
    end if
    if(iDR.lt.0) then
      iDR=min(235_2,226_2-iDR)
    else
      iDR=min(226_2,nDom,iDR)
    end if
    iPackDom=iPackObjC(iDL,iDR)
  end Function iPackDom

  Subroutine unPackDom(r,iC,iD1,iD2)
! unpack domain numbers contained in iC
    Implicit None
    Real(8) r
    Integer(2) iC,iD1,iD2
    call unPackObjC(r,iC,iD1,iD2)
    if(iD1.gt.226_2) iD1=226_2-iD1
    if(iD2.gt.226_2) iD2=226_2-iD2
  end Subroutine unPackDom
  
  Integer(2) Function iPackObjC(iDL0,iDR0)
! iC = iDL + 236 * iDR - 23165; range iDL,iDR:0...236, iC:-23165...32767
    Implicit None
    Integer(2) iDL0,iDR0
    Integer(4) i
    i=Int4(iDL0)+236_4*Int4(iDR0)-23165_4
    iPackObjC=Int2(i)
  end Function iPackObjC

  Subroutine unPackObjC(r,iC,iD1,iD2)
! iC = iDL + 236 * iDR - 23165
    Implicit None
    Real(8) r
    Integer(2) iC,iD1,iD2
    Integer(4) i,i1,i2
    i=Int4(iC)+23165_4
    if(r.gt.0.0d0) then
      i1=i/236_4
      i2=i-236_4*i1
    else
      i2=i/236_4
      i1=i-236_4*i2
    end if
    iD1=Int2(i1)
    iD2=Int2(i2)
  end Subroutine unPackObjC

  Subroutine GetRange(kB,nB,k1,k2,kB2)
! kB is an Integer(4) number defining a range for a loop from k1 to k2 (1<=k1<=k2<=nB)
! kB<0: k1=1, k2=min(-kB,nB)
! kB=0: k1=1, k2=nB
! kB>0: k1=min(kB,nB)
!       if kB2 is present: k2=max(1,min(nB,kB2)): else: k2=k1
! if k1>k2: exchange k1 and k2
    Implicit none
    Integer(4) kB,nB,k0,k1,k2
    Integer(4), Optional:: kB2
    if(kB.lt.1) then
      if(kB.eq.0) then
        k1=1
        k2=nB
      else
        k1=1
        k2=Min(nB,-kB)
      end if
    else
      k1=Min(nB,kB)
      if(Present(kB2)) then
        k2=Max(1,Min(nB,kB2))
        if(k1.gt.k2) then
          k0=k1
          k1=k2
          k2=k0
        end if
      else
        k2=k1
      end if
    end if
  end Subroutine GetRange

  Real(8) Function getCondition(MMPMtr,nCol,nRow)
  ! get condition number of MMPMtr using SVD analysis
    Complex(8) MMPMtr(nCol*nRow)
    Complex(8), Allocatable :: A(:,:),B(:,:),W(:)
    Real(8), Allocatable :: S(:),R(:)
    Real(8) RCOND
    Integer(4) nCol,nRow,ios,M,N,INFO,IRANK,NM,i,k,ik,lW
    N=nCol-1
    if(N.lt.2) then
      getCondition=1.0d0
    end if
    M=nRow
    NM=min(N,M)
    lW=66*N
    Allocate(A(M,N),B(M,1),W(lW),S(NM),R(5*N),stat=ios)
    if(ios.ne.0) then
      getCondition=-1.0d0
    end if
    ik=0
    do i=1,M
      do k=1,N
        ik=ik+1
        A(i,k)=MMPMtr(ik)
      end do
      ik=ik+1 ! the last column of MMPMtr that contains the right hand side
      B(i,1)=-MMPMtr(ik)
    end do
    call ZGELSS(M,N,1,A,M,B,M,S,RCOND,IRANK,W,lW,R,INFO)
    getCondition=abs(S(1)/S(N))
    DeAllocate(A,B,W,S,R,stat=ios)
  end Function getCondition

  Real(8) Function CircleDeform0(f,p,a,n,sn)
    Implicit none
    Integer(4) n,i
    Real(8) f,p,a,sn(n),sum
    sum=0.0d0
    do i=1,n
      sum=sum+Protected_Pow((dabs(dcos(Pi*(a*f+Dble(i-1)/Dble(n)))).div.sn(i)),p)
    end do
    CircleDeform0=Protected_Pow(sum,-1.0d0.div.p)
  end Function CircleDeform0

END MODULE CHMAT
