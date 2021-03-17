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
MODULE CHGRF

! Graphics

  USE CHGRC

  SAVE

  CONTAINS

! threads

  Subroutine TDrawFunction(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=1
    call StartGRFThread(lCheck)
  end Subroutine TDrawFunction

  Subroutine TDrawWindow(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    call WaitEndThread()
    iThreadAction=3
    call StartGRFThread(lCheck)
  end Subroutine TDrawWindow

  Subroutine StartGRFThread(ldi)
! start a new thread
    Implicit none
    Include 'resource.fd'
    Integer(INT_PTR_KIND()) iThread
    Integer(4), Save:: iArgument
    Integer(4) iStack,iCreation,idum
    Logical, intent(in) :: ldi
    Logical(4) ldum
    if(lThreadStarted) return
		call OutTxt('t3','start graphic thread')
		call OutTxt('n3',' ')
		call OutTxt('m3',' ')
    lThreadStarted=.true.
    iStack=0
    iArgument=0
    iCreation=0
    iThread=0
    if(ldi) iArgument=1_4
		if(iThreadHandle.ne.0) then
		  call OutTxt('t3','close thread handle')
      ldum=CloseHandle(iThreadHandle)
			if(.not.ldum) then
        idum=MessageBoxQQ('Cannot close thread handle'C,'Start graphic thread'C, &
                          MB$OK.or.MB$ICONEXCLAMATION)
        lThreadStarted=.false.
        return
      end if
			iThreadHandle=0
		endif
		call OutTxt('t3','Start graphic thread')
    iThreadHandle=CreateThread(NULL,iStack,Loc(GRFThread),Loc(iArgument),iCreation,iThread)
		if(iThreadHandle.eq.0) then
      idum=MessageBoxQQ('Cannot create thread'C,'Start graphic thread'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      lThreadStarted=.false.
    end if
  end Subroutine StartGRFThread

  Integer(4) Function GRFThread(iWhat)
! tread calls.....
    Implicit none
    Integer(4) iWhat
    Logical ldum
    Include 'resource.fd'
    lStopThread=.false.
    ldum=.false.
    if(iWhat.ne.0) ldum=.true.
    GRFThread=0_4
    if(iThreadAction.eq.1) then
      call MTDrawFunction(ldum)
    else if(iThreadAction.eq.2) then ! NO palette anymore
    else if(iThreadAction.eq.3) then
      call MTDrawWindow(ldum)
    end if
    call endThread()
  end Function GRFThread

! elementary graphics

  Subroutine drawTxt(ix,iy,ic,txt)
! draw text
    Implicit none
    Integer(2) ix,iy,ic,ic0
    Integer(4) len,idum
    Character(*) txt
    Type(xycoord) xy
    call GetKWin(.false.)
    idum=SetActiveQQ(10+kWin)
    call SetClipRgn(0_2,0_2,iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)-1_2, &
    &                       iWinHeight(kWin)+iWinTop(kWin)+iWinBottom(kWin)-1_2)
    call MoveTo(ix,iy,xy)
    len=getSLength(txt)
    ic0=setcolor(ic)
    call OutGText(txt(1:len))
    ic0=setcolor(ic0)
    call SetClipRgn(iWinLeft(kWin)+1_2,iWinTop(kWin)+1_2,iWinWidth(kWin)+iWinLeft(kWin)-1_2,iWinHeight(kWin)+iWinTop(kWin)-1_2)
  end Subroutine drawTxt

  Subroutine DrawILine(ix,iy,kx,ky)
    Implicit none
    Integer(2) ix,iy,kx,ky,idum
	  Type(xycoord) xydum
    call MoveTo(ix,iy,xydum)
    idum=LineTo(kx,ky)
  end Subroutine DrawILine

  Subroutine DrawLine(x1,y1,x2,y2)
    Implicit none
    Real(8) x1,y1,x2,y2
    Integer(2) ix,iy,kx,ky,idum
	  Type(xycoord) xydum
    call R2I(x1,y1,ix,iy)
    call MoveTo(ix,iy,xydum)
    call R2I(x2,y2,kx,ky)
    idum=LineTo(kx,ky)
  end Subroutine DrawLine

  Subroutine DrawLineM(x1,y1,x2,y2,dMark)
    Implicit none
    Real(8) x1,y1,x2,y2,dMark,x,y,v(2)
	  call DrawLine(x1,y1,x2,y2)
    if(lGRF_Mark.and.(dMark.gt.pSmall)) then
      call DrawQ(x1,y1,dMark)
      call DrawQ(x2,y2,dMark)
      x=0.5d0*(x1+x2)
      y=0.5d0*(y1+y2)
      v(1)=(x2-x1)
      v(2)=(y2-y1)
      call Unit2DV(v)
      v=dMark*v
      call DrawV(x,y,v(1),v(2))
    end if
  end Subroutine DrawLineM

  Subroutine DrawICircle(ix,iy,jx,jy)
    Implicit none
    Integer(2) ix,iy,jx,jy,nr,idum,i
    Real(8) r,v(2),f
    v(1)=dble(jx-ix)
    v(2)=dble(jy-iy)
    r=r2Vec_Length(v)
    nr=Min(mGraPoly,2*(2**int(min(r,5.0d0))))
    v(1)=dble(ix)
    v(2)=dble(iy)
    f=0.0d0
    do i=1,nr
      Poly(i).xcoord=nint(v(1)+r*dcos(f),2)
      Poly(i).ycoord=nint(v(2)+r*dsin(f),2)
      f=f+2.0d0*Pi/Dble(nr)
    end do
    idum=polygon($GBorder,Poly,nr)
  end Subroutine DrawICircle

  Subroutine DrawCircle(x0,y0,r)
    Implicit none
    Real(8) x0,y0,r,f,x,y
    Integer(4) i
    Integer(2) idum
    f=0.0d0
    do i=1,mGraPoly
      x=x0+r*dcos(f)
      y=y0+r*dsin(f)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(mGraPoly)
    end do
    idum=polygon($GBorder,Poly,mGraPoly)
  end Subroutine DrawCircle

  Subroutine DrawCircleM(x0,y0,r,dMark)
    Implicit none
    Real(8) x0,y0,r,dMark
    call DrawCircle(x0,y0,r)
    if(lGRF_Mark.and.(dMark.gt.pSmall)) then
      call DrawO(x0,y0,dMark)
      call DrawQ(x0+r,y0,dMark)
      call DrawLine(x0,y0,x0+r,y0)
      call DrawV(x0-r,y0,0.0d0,-dMark)
    end if
  end Subroutine DrawCircleM

  Subroutine DrawIArc(ix,iy,jx,jy,kx,ky)
    Implicit none
    Integer(2) ix,iy,jx,jy,kx,ky
    Real(8) x0,y0,x1,y1,x2,y2
    call I2R(ix,iy,x0,y0)
    call I2R(jx,jy,x1,y1)
    call I2R(kx,ky,x2,y2)
    call DrawArc(x0,y0,x1,y1,x2,y2)
  end Subroutine DrawIArc

  Subroutine DrawArc(x0,y0,x1,y1,x2,y2)
    Implicit none
    Real(8) x0,y0,x1,y1,x2,y2,v(2),r,f,f1,f2,df,x,y
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
    x=x0
    y=y0
    if(lGRF_Mark) then
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      do i=2,mGraPoly-1
        x=x0+r*dcos(f)
        y=y0+r*dsin(f)
        call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
        f=f+df
      end do
      x=x0+r*dcos(f2)
      y=y0+r*dsin(f2)
      call R2I(x,y,Poly(mGraPoly).xcoord,Poly(mGraPoly).ycoord)
      idum=polygon($GBorder,Poly,mGraPoly)
    else
      x=x0+r*dcos(f)
      y=y0+r*dsin(f)
      call R2I(x,y,ix,iy)
      call MoveTo(ix,iy,xydum)
      f=f+df
      do i=2,mGraPoly-1
        x=x0+r*dcos(f)
        y=y0+r*dsin(f)
        call R2I(x,y,ix,iy)
        idum=LineTo(ix,iy)
        f=f+df
      end do
      x=x0+r*dcos(f2)
      y=y0+r*dsin(f2)
      call R2I(x,y,ix,iy)
      idum=LineTo(ix,iy)
    end if
  end Subroutine DrawArc

  Subroutine DrawArcM(x0,y0,x1,y1,x2,y2,dMark)
    Implicit none
    Real(8) x0,y0,x1,y1,x2,y2,dMark
    call DrawArc(x0,y0,x1,y1,x2,y2)
    if(lGRF_Mark.and.(dMark.gt.pSmall)) then
      call DrawO(x0,y0,dMark)
      call DrawQ(x1,y1,dMark)
      call DrawQ(x2,y2,dMark)
    end if
  end Subroutine DrawArcM

  Subroutine DrawIX(ix,iy,id)
    Implicit none
    Integer(2) ix,iy,id
    call DrawILine(ix-id,iy-id,ix+id,iy+id)
    call DrawILine(ix-id,iy+id,ix+id,iy-id)
  end Subroutine DrawIX

  Subroutine DrawX(x,y,d)
    Implicit none
    Real(8) x,y,d
    call DrawLine(x-d,y-d,x+d,y+d)
    call DrawLine(x-d,y+d,x+d,y-d)
  end Subroutine DrawX

  Subroutine DrawIP(ix,iy,id)
    Implicit none
    Integer(2) ix,iy,id
    call DrawILine(ix-id,iy,ix+id,iy)
    call DrawILine(ix,iy+id,ix,iy-id)
  end Subroutine DrawIP

  Subroutine DrawP(x,y,d)
    Implicit none
    Real(8) x,y,d
    call DrawLine(x-d,y,x+d,y)
    call DrawLine(x,y+d,x,y-d)
  end Subroutine DrawP

  Subroutine DrawIQ(ix,iy,id)
    Implicit none
    Integer(2) ix,iy,id
    call DrawILine(ix-id,iy-id,ix-id,iy+id)
    call DrawILine(ix-id,iy+id,ix+id,iy+id)
    call DrawILine(ix+id,iy+id,ix+id,iy-id)
    call DrawILine(ix+id,iy-id,ix-id,iy-id)
  end Subroutine DrawIQ

  Subroutine DrawQ(x,y,d)
    Implicit none
    Real(8) x,y,d
    call DrawLine(x-d,y-d,x-d,y+d)
    call DrawLine(x-d,y+d,x+d,y+d)
    call DrawLine(x+d,y+d,x+d,y-d)
    call DrawLine(x+d,y-d,x-d,y-d)
  end Subroutine DrawQ

  Subroutine DrawO(x0,y0,r)
    Implicit none
    Real(8) x0,y0,x,y,r,f
    Integer(2) i,idum
    f=0.0d0
    do i=1,8
      x=x0+r*dcos(f)
      y=y0+r*dsin(f)
      call R2I(x,y,Poly(i).xcoord,Poly(i).ycoord)
      f=f+2.0d0*Pi/Dble(8)
    end do
    idum=polygon($GBorder,Poly,8)
  end Subroutine DrawO

  Subroutine DrawV(x,y,tx,ty)
    Implicit none
    Real(8) x,y,tx,ty
    call DrawLine(x,y,x-tx+0.5d0*ty,y-ty-0.5d0*tx)
    call DrawLine(x,y,x-tx-0.5d0*ty,y-ty+0.5d0*tx)
  end Subroutine DrawV

  Subroutine DrawIRectangle(ix1,iy1,ix2,iy2,lFill)
    Implicit none
    Integer(2) ix1,iy1,ix2,iy2,idu
    Logical, intent(in) :: lFill
    if(lFill) then
      idu=Rectangle($GFillInterior,ix1,iy1,ix2,iy2)
    else
      idu=Rectangle($GBorder,ix1,iy1,ix2,iy2)
    end if
  end Subroutine DrawIRectangle

  Subroutine DrawRectangle(x1,y1,x2,y2,lFill)
    Implicit none
    Real(8) x1,y1,x2,y2
    Integer(2) ix1,iy1,ix2,iy2
    Logical, intent(in) :: lFill
    call R2I(x1,y1,ix1,iy1)
    call R2I(x2,y2,ix2,iy2)
    call DrawIRectangle(ix1,iy1,ix2,iy2,lFill)
  end Subroutine DrawRectangle

  Subroutine DrawIPolygon(ix,iy,np,lFill,icB,icI)
    Implicit none
    Integer(2) ix(*),iy(*),n,np,icB,icI,ic,idu,i
    Logical, intent(in) :: lFill
	  Type(xycoord) xydum
    n=Min(mGraPoly,np)
    if(lFill) then
      Poly(1:n).xcoord=ix(1:n)
      Poly(1:n).ycoord=iy(1:n)
      ic=SetColor(icI)
      idu=Polygon($GFillInterior,Poly,n)
      idu=SetColor(icB)
      idu=Polygon($GBorder,Poly,n)
    else
      ic=SetColor(icB)
      if(lGRF_PolygonClosed) then
        Poly(1:n).xcoord=ix(1:n)
        Poly(1:n).ycoord=iy(1:n)
        idu=Polygon($GBorder,Poly,n)
      else
        call MoveTo(ix(1),iy(1),xydum)
        do i=2,n
          idu=LineTo(ix(i),iy(i))
        end do
      end if
    end if
    ic=SetColor(ic)
  end Subroutine DrawIPolygon

  Subroutine DrawPolygon(x,y,np,lFill,icB,icI)
    Implicit none
    Real(8) x(*),y(*)
    Integer(2) n,np,i,icB,icI
    Logical, intent(in) :: lFill
    n=Min(mGraPoly,np)
    do i=1,n
      call R2I(x(i),y(i),ixPoly(i),iyPoly(i))
    end do
    call DrawIPolygon(ixPoly,iyPoly,n,lFill,icB,icI)
  end Subroutine DrawPolygon

  Subroutine DrawArrow(x1,y1,x2,y2,w,iType,lFill,icB,icI)
    Implicit none
    Real(8) x1,y1,x2,y2,w,e(2),dx,dy,x(8),y(8)
    Integer(2) icB,icI,iType,ic
    Logical, intent(in) :: lFill
    if(iType.eq.0) return
    if((iType.eq.1).or.(abs(w).lt.pSmall)) then    ! simple line
      ic=SetColor(icI)
      call DrawLine(x1,y1,x2,y2)
      ic=SetColor(ic)
      return
    end if
    dx=x2-x1
    dy=y2-y1
    if(iabs(iType).gt.2) then ! standard arrow
      x(4)=x1+dx
      y(4)=y1+dy
      x(8)=x1+0.7d0*dx
      y(8)=y1+0.7d0*dy
      if(w.lt.0.0d0) then ! constant base line of the arrow
        x(1)=x1+0.08d0*dy
        y(1)=y1-0.08d0*dx
        x(7)=x1-0.08d0*dy
        y(7)=y1+0.08d0*dx
        x(2)=x(8)+0.08d0*dy
        y(2)=y(8)-0.08d0*dx
        x(6)=x(8)-0.08d0*dy
        y(6)=y(8)+0.08d0*dx
      else                ! base line length of the arrow = w
        e(1)=-dy
        e(2)=dx
        call Unit2DV(e)
        e=0.5d0*w*e
        x(1)=x1-e(1)
        y(1)=y1-e(2)
        x(7)=x1+e(1)
        y(7)=y1+e(2)
        e=0.3d0*e
        x(2)=x(8)-e(1)
        y(2)=y(8)-e(2)
        x(6)=x(8)+e(1)
        y(6)=y(8)+e(2)
      end if
      x(3)=x(2)+0.1d0*dy
      y(3)=y(2)-0.1d0*dx
      x(5)=x(6)-0.1d0*dy
      y(5)=y(6)+0.1d0*dx
      call DrawPolygon(x,y,7_2,lFill,icB,icI)
    else                  ! triangular shape
      x(2)=x1+dx
      y(2)=y1+dy
      if(w.lt.0.0d0) then ! constant base line of the arrow
        x(1)=x1+0.15d0*dy
        y(1)=y1-0.15d0*dx
        x(3)=x1-0.15d0*dy
        y(3)=y1+0.15d0*dx
      else                ! base line length of the arrow = w
        e(1)=-dy
        e(2)=dx
        call Unit2DV(e)
        e=0.5d0*w*e
        x(1)=x1-e(1)
        y(1)=y1-e(2)
        x(3)=x1+e(1)
        y(3)=y1+e(2)
      end if
      call DrawPolygon(x,y,3_2,lFill,icB,icI)
    end if
  end Subroutine DrawArrow

  Subroutine getXYZ_in_Interval(r1,r2,f1,f2,f,r)
    Implicit none
    Real(8) r1(3),r2(3),r(3),d(3),f1,f2,f
    d=r2-r1
    d=((f-f1).div.(f2-f1))*d
    r=r1+d
  end Subroutine getXYZ_in_Interval

! window, grid and labels

  Subroutine DrawGrid(lcheck)
! draw grid in horizontal or vertical direction
    Implicit none
    Integer(2) idu,ix,iy,ic,is,ist
    Logical, intent(in) :: lcheck
    Type(xycoord) xy
    if(.not.lcheck) return
    ic=SetColor(int2(iWinCGrid(kWin)))
    if(iWinXGrid(kWin).gt.0_2) then
      is=-1_2
      do ix=0_2,iWinWidth(kWin),iWinXGrid(kWin)
        is=is+1_2
        if(is.gt.9_2) is=0_2
        if(is.eq.0) then
          ist=-1
        else if(is.eq.5) then
          ist=#FFF ! 4095
        else
          ist=#F ! 15
        end if
        call SetLineStyle(ist)
        call MoveTo(ix,0_2,xy)
        idu=LineTo(ix,iWinHeight(kWin))
      end do
    end if
    if(iWinYGrid(kWin).gt.0_2) then
      is=-1_2
      do iy=0_2,iWinHeight(kWin),iWinYGrid(kWin)
        is=is+1_2
        if(is.gt.9_2) is=0_2
        if(is.eq.0) then
          ist=-1
        else if(is.eq.5) then
          ist=#FFF ! 4095
        else
          ist=#F ! 15
        end if
        call SetLineStyle(ist)
        call MoveTo(0_2,iy,xy)
        idu=LineTo(iWinWidth(kWin),iy)
      end do
    end if
    ic=SetColor(ic)
    call SetLineStyle(#FFFF)
  end Subroutine drawGrid

  Subroutine drawLabels(labhor)
! draw labels along horizontal or vertical axis
    Implicit none
    Real(8) a,al0,b,d,d0,d10,xl,yl,an,bn
    Integer(4) height,lout
    Integer(2) k,k10,k10a,k10b,k10s,llab,na,nb,ix,iy,idu
    Integer(2) len,ndigit,nlab,nlabel,iw
    Logical, intent(in) :: labhor
    Logical labwri,llog
    Character(20) rs
    Type(xycoord) xy
    Type(fontinfo) font
    idu=GetFontInfo(font)
    height=font.pixheight
    llog=.false.
    labwri=.true.
    if(labhor) then
      if(iabs(iWinXLog(kWin)).gt.1) llog=.true.
      nlabel=iWinXLab(kWin)
      a=WinXmin(kWin)
      b=WinXmax(kWin)
    else
      if(iabs(iWinYLog(kWin)).gt.1) llog=.true.
      nlabel=iWinYLab(kWin)
      a=WinYmin(kWin)
      b=WinYmax(kWin)
    end if
    if(nlabel.lt.1_2) return
    if(b.lt.a) then
      d=b
      b=a
      a=d
    end if
    if(llog) then
      if((a.lt.1.0d-299).or.(b.lt.1.0d-299)) return
      k10a=0_2
      k10b=8_2
      k10s=1_2
      if(nlabel.eq.1) k10s=10_2
      if(nlabel.eq.2) then
        k10s=4_2
        k10b=5_2
      end if
      na=nint(log10(a),2)-1_2
      nb=nint(log10(b),2)+1_2
      al0=10.0_8**(na-1_2)
      ndigit=1_2
    else
      if(dabs(a-b).lt.pSmall) return
      k10a=0_2
      k10b=9_2
      k10s=1_2
      d0=dabs(b-a)/dble(nlabel)
      call RealToStr(d0,0,0,rs,lout)
      d=GetSValue(rs,Int2(lout))
      an=min(dabs(a.div.d)+1.0d-5,1.0d3)
      bn=min(dabs(b.div.d)+1.0d-5,1.0d3)
      na=int2(an)
      nb=int2(bn)
      if(a.lt.0.0_8) na=-na
      if(b.lt.0.0_8) nb=-nb
      d10=d*0.1_8
      ndigit=5_2
    end if
    nlab=0_2
    if(labhor) then
      yl=WinYmin(kWin)
      do k=na-1_2,nb
        if(llog) then
          al0=al0*10.0_8
        else
          xl=d*dble(k)-d10
        end if
        do k10=k10a,k10b,k10s
          llab=4_2
          if(k10.eq.0_2) llab=10_2
          if(llog) then
            if(k10.eq.4_2) llab=8_2
            xl=al0*dble(k10+1_2)
          else
            if(k10.eq.5_2) llab=8_2
            xl=xl+d10
            if(dabs(xl).lt.(0.000001_8*d)) xl=0.0_8
          end if
          if(xl.lt.a) Cycle
          if(xl.gt.b) Exit
          call R2I(xl,yl,ix,iy)
          call MoveTo(ix,iy,xy)
          idu=LineTo(ix,iy+llab)
          if(labwri.and.(k10.eq.0)) then
            call rswrit2(xl,ndigit,rs,len)
            iw=GetGTextExtent(rs(1:len))
            iw=iw/2_2
            call MoveTo(ix-iw,iy+llab,xy)
            call OutGText(rs(1:len))
            nlab=nlab+1
          end if
        end do
      end do
      if(nlab.lt.1_2) then
        llab=10_2
        ndigit=6_2
        xl=a
        call R2I(xl,yl,ix,iy)
        call MoveTo(ix,iy,xy)
        idu=LineTo(ix,iy+llab)
        call rswrit2(xl,ndigit,rs,len)
        iw=GetGTextExtent(rs(1:len))
        iw=iw/2_2
        call MoveTo(ix-iw,iy+llab,xy)
        call OutGText(rs(1:len))
        xl=b
        call R2I(xl,yl,ix,iy)
        call MoveTo(ix,iy,xy)
        idu=LineTo(ix,iy+llab)
        call rswrit2(xl,ndigit,rs,len)
        iw=GetGTextExtent(rs(1:len))
        iw=iw/2_2
        call MoveTo(ix-iw,iy+llab,xy)
        call OutGText(rs(1:len))
      end if
    else
      xl=WinXmin(kWin)
      do k=na-1_2,nb
        if(llog) then
          al0=al0*10.0_8
        else
          yl=d*dble(k)-d10
        end if
        do k10=k10a,k10b,k10s
          llab=4_2
          if(k10.eq.0_2) llab=10_2
          if(llog) then
            if(k10.eq.4_2) llab=8_2
            yl=al0*dble(k10+1_2)
          else
            if(k10.eq.5_2) llab=8_2
            yl=yl+d10
            if(dabs(yl).lt.(0.000001_8*d)) yl=0.0_8
          end if
          if(yl.lt.a) Cycle
          if(yl.gt.b) Exit
          call R2I(xl,yl,ix,iy)
          call MoveTo(ix,iy,xy)
          idu=LineTo(ix-llab,iy)
          if(labwri.and.(k10.eq.0)) then
            call rswrit2(yl,ndigit,rs,len)
            iw=GetGTextExtent(rs(1:len))
            call MoveTo(ix-llab-iw,iy-Int2(height)/2_2,xy)
            call OutGText(rs(1:len))
            nlab=nlab+1
          end if
        end do
      end do
      if(nlab.lt.1_2) then
        llab=10_2
        ndigit=6_2
        yl=a
        call R2I(xl,yl,ix,iy)
        call MoveTo(ix,iy,xy)
        idu=LineTo(ix-llab,iy)
        call rswrit2(yl,ndigit,rs,len)
        iw=GetGTextExtent(rs(1:len))
        call MoveTo(ix-llab-iw,iy-Int2(height)/2_2,xy)
        call OutGText(rs(1:len))
        yl=b
        call R2I(xl,yl,ix,iy)
        call MoveTo(ix,iy,xy)
        idu=LineTo(ix-llab,iy)
        call rswrit2(yl,ndigit,rs,len)
        iw=GetGTextExtent(rs(1:len))
        call MoveTo(ix-llab-iw,iy-Int2(height)/2_2,xy)
        call OutGText(rs(1:len))
      end if
    end if
  end Subroutine drawLabels

  Subroutine DrawWindow(lCheck)
    Implicit none
    Integer(4) idum,height
    Integer(2) idu,iw
    Integer(2) iOK,lss,ls1
    Logical, intent(in) :: lCheck
    Logical(4) ldum
    Character(20) rss
    Type(windowconfig) drwScreen
    Type(xycoord) xy
    Type(fontinfo) font
    if(lWinInit) then
      call InitWindow(.true.)
      lWinInit=.false.
    end if
    call GetKWin(.false.)
    idum=SetActiveQQ(10+kWin)
    idum=FocusQQ(10+kWin)
    idum=MB$IDNO
    if((.not.lCheck).and.(.not.lWinFld(kWin))) then
      idum=MessageBoxQQ('Automatically set the window limits first?'C,'Draw window'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) return
    end if
    if(idum.eq.MB$IDYES) then
      if(.not.lWinFld(kWin)) then
        call GetLimits(iWinXLog(kWin),iWinYLog(kWin),WinXmin(kWin), &
                       WinYmin(kWin),WinXmax(kWin),WinYmax(kWin),1_2,iOK)
      end if
    end if
    ldum=GetWindowConfig(drwScreen)
    if(.not.ldum) then
      idum=MessageBoxQQ('Cannot get Window configuration!'C,'Draw window'C, &
                        MB$OK.or.MB$ICONSTOP)
      return
    end if
    iColorCnt=0_2 ! reset color counter for GGP
  ! set colors, clear, set viewport
    idum=SetBKColor(0_4)
    idum=SetColor(1_4)
    call GRCclear()
    idum=SetBKColor(0_4)
    call ClearScreen($GCLEARSCREEN)
    call SetViewPort(0_2,0_2,drwScreen.numxpixels-1_2,drwScreen.numypixels-1_2)
    call SetViewOrg(iWinLeft(kWin),iWinTop(kWin),xy)
    call SetClipRgn(0_2,0_2,drwScreen.numxpixels-1_2,drwScreen.numypixels-1_2)
  ! draw texts
    if(lWinFld(kWin)) then
      OTitle(1:3)='O=('
      call rswrit2(viewPlane(1,0),2_2,rss,lss)
      OTitle(4:lss+3)=rss(1:lss)
      ls1=lss+4
      OTitle(ls1:ls1)=','
      call rswrit2(viewPlane(2,0),2_2,rss,lss)
      OTitle(ls1+1:ls1+lss)=rss(1:lss)
      ls1=ls1+lss+1
      OTitle(ls1:ls1)=','
      call rswrit2(viewPlane(3,0),2_2,rss,lss)
      OTitle(ls1+1:ls1+lss)=rss(1:lss)
      ls1=ls1+lss+1
      OTitle(ls1:ls1)=')'
      OTitle(ls1+1:ls1+1)=Char(0)
      TTitle(1:3)='t='
      call rswrit2(GRFtime,2_2,rss,lss)
      TTitle(4:lss+3)=rss(1:lss)
      ls1=lss+4
      TTitle(ls1:ls1)=Char(0)
      XTitle(1:3)='X=('
      call rswrit2(viewPlane(1,1),2_2,rss,lss)
      XTitle(4:lss+3)=rss(1:lss)
      ls1=lss+4
      XTitle(ls1:ls1)=','
      call rswrit2(viewPlane(2,1),2_2,rss,lss)
      XTitle(ls1+1:ls1+lss)=rss(1:lss)
      ls1=ls1+lss+1
      XTitle(ls1:ls1)=','
      call rswrit2(viewPlane(3,1),2_2,rss,lss)
      XTitle(ls1+1:ls1+lss)=rss(1:lss)
      ls1=ls1+lss+1
      XTitle(ls1:ls1)=')'
      XTitle(ls1+1:ls1+1)=Char(0)
      YTitle(1:3)='Y=('
      call rswrit2(viewPlane(1,2),2_2,rss,lss)
      YTitle(4:lss+3)=rss(1:lss)
      ls1=lss+4
      YTitle(ls1:ls1)=','
      call rswrit2(viewPlane(2,2),2_2,rss,lss)
      YTitle(ls1+1:ls1+lss)=rss(1:lss)
      ls1=ls1+lss+1
      YTitle(ls1:ls1)=','
      call rswrit2(viewPlane(3,2),2_2,rss,lss)
      YTitle(ls1+1:ls1+lss)=rss(1:lss)
      ls1=ls1+lss+1
      YTitle(ls1:ls1)=')'
      YTitle(ls1+1:ls1+1)=Char(0)
    else
      OTitle=' 'C
      TTitle=' 'C
      XTitle=FunATitle(iFunA1)
      YTitle=FunATitle(iFunA2)
    end if
	  idu=InitializeFonts()
	  idu=SetFont('t''Arial''h15eb')
    idu=GetFontInfo(font)
    height=font.pixheight
    iw=2_2-iWinLeft(kWin)
    call MoveTo(iw,iWinHeight(kWin)+iWinBottom(kWin)-int2(height)-2_2,xy)
    call OutGText(OTitle)
    iw=GetGTextExtent(TTitle)
    iw=iWinWidth(kWin)-iw-2_2
    call MoveTo(iw,iWinHeight(kWin)+iWinBottom(kWin)-int2(height)-2_2,xy)
    call OutGText(TTitle)
    iw=GetGTextExtent(XTitle)
    iw=(iWinWidth(kWin)-iw)/2_2
    call MoveTo(iw,iWinHeight(kWin)+iWinBottom(kWin)-int2(height)-2_2,xy)
    call OutGText(XTitle)
    iw=GetGTextExtent(YTitle)
    iw=iWinHeight(kWin)-(iWinHeight(kWin)-iw)/2_2
    call MoveTo(2_2-iWinLeft(kWin),iw,xy)
    call SetGTextRotation(900_4)
    call OutGText(YTitle)
    call SetGTextRotation(0_4)
  ! draw borders
    call DrawRectangle(WinXmin(kWin),WinYmin(kWin),WinXmax(kWin),WinYmax(kWin),.false.)
    call DrawLabels(.true.)
    call DrawLabels(.false.)
  ! set clip region, draw axes, grid lines
    call SetClipRgn(iWinLeft(kWin)+1_2,iWinTop(kWin)+1_2,iWinWidth(kWin)+iWinLeft(kWin)-1_2,iWinHeight(kWin)+iWinTop(kWin)-1_2)
    if((iWinXLog(kWin).gt.0_2).and.(iabs(iWinYLog(kWin)).lt.2)) call DrawLine(WinXmin(kWin),0.0_8,WinXmax(kWin),0.0_8)
    if((iWinYLog(kWin).gt.0_2).and.(iabs(iWinXLog(kWin)).lt.2)) call DrawLine(0.0_8,WinYmin(kWin),0.0_8,WinYmax(kWin))
    call DrawGrid(.true.)
  end Subroutine DrawWindow

  Subroutine MTDrawWindow(lCheck)
! draw the graphic window
    Implicit none
    Logical, intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
    call DrawWindow(lCheck)
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTDrawWindow

  Subroutine MTModifyNothing(lCheck)
! draw the current palette (allow modification)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    ldum=lCheck
    iWinAction=0_2
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuUnChecked) !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuChecked)   !nothing
  end Subroutine MTModifyNothing

! function

  Subroutine preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
    Implicit none
    Real(8) x,y,r,f
	  Logical lPolar,lclip,lsquare,ldb
    y=rFunSca*y
    if(lSquare) y=y**2
    if(ldb) y=10.0d0*log10(max(pSmall,y))
    if(lclip) y=rFunOff+y
    if(lPolar.and.(x.lt.pBig)) then
      r=y
      f=x
      x=r*cosd(f)
      y=r*sind(f)
    end if
  end Subroutine preprocessFun

  Subroutine DrawFunction(lCheck,xScale,yScale)
! set the title and draw the current function
    Implicit none
    Include 'resource.fd'
    Real(8), Optional:: xScale,yScale
 	  Real(8) x,y,a
    Integer(4) istat,i,idum,i1,i2
    Integer(2) idu,ix,iy,ic
    Logical(4), intent(in) :: lCheck
	  Logical lpolar,lclip,lsquare,ldb
    Character(32) sch
    Type(xycoord) xy
    lWinFld(kWin)=.false.
    i1=iFun1
    i2=iFun2
    if(Present(xScale)) then
      if(xScale.lt.-1.0d-100) then ! scale such that xmax=-xScale
        a=-1.0d-300
        do i=i1,i2
          call GetFun(iFunA1,i,x)
          if(dabs(x).gt.a) a=dabs(x)
        end do
        xScale=-xScale.div.a
        call GetKWin(.false.)
        idu=9_2
        sch(1:idu)='x scaled:'
        call drawTxt(0_2,iWinHeight(kWin)+30_2,2,sch(1:idu))
        call rswrit2(xScale,10_2,sch,idu)
        call drawTxt(55_2,iWinHeight(kWin)+30_2,2,sch(1:idu))
      end if
    end if
    if(Present(yScale)) then
      if(yScale.lt.-1.0d-100) then ! scale such that ymax=-yScale
        a=-1.0d-300
        do i=i1,i2
          call GetFun(iFunA2,i,y)
          if(dabs(y).gt.a) a=dabs(y)
        end do
        yScale=-yScale.div.a
        call GetKWin(.false.)
        idu=9_2
        sch(1:idu)='y scaled:'
        call drawTxt((iWinWidth(kWin)/2_2)+30_2,iWinHeight(kWin)+30_2,2_2,sch(1:idu))
        call rswrit2(yScale,10_2,sch,idu)
        call drawTxt((iWinWidth(kWin)/2_2)+85_2,iWinHeight(kWin)+30_2,2_2,sch(1:idu))
      end if
    end if
! set window, auto determine borders
    call GetKWin(.false.)
    istat=SetActiveQQ(10+kWin)
    if(.not.lCheck) then
      idum=MessageBoxQQ('Clear graphic window first?'C,'Draw function'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) return
      if(idum.eq.MB$IDYES) call DrawWindow(lCheck)
    end if
    call getFunctionPolar(lpolar,lclip,lsquare,ldb)
! draw function, line style
    if(iFunStyleL.ne.0_2) then
      ic=SetColor(iFunStyleColor)
      do i=i1,i2,iFunStyleStep
        if(lPolar) then
          x=0.0_8
        else
          call GetFun(iFunA1,i,x)
        end if
	      y=0.0_8
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        call MoveTo(ix,iy,xy)
        if(lPolar) call GetFun(iFunA1,i,x)
        call GetFun(iFunA2,i,y)
        call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        idu=LineTo(ix,iy)
      end do
      ic=SetColor(ic)
    end if
! draw function, polygon style
    if(iFunStyleP.ne.0_2) then
      ic=SetColor(iFunStyleColor)
      call GetFun(iFunA1,i1,x)
      call GetFun(iFunA2,i1,y)
      call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
      call R2I(x,y,ix,iy)
      call MoveTo(ix,iy,xy)
      do i=i1+iFunStyleStep,i2,iFunStyleStep
        call GetFun(iFunA1,i,x)
        call GetFun(iFunA2,i,y)
        call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        idu=LineTo(ix,iy)
      end do
      ic=SetColor(ic)
    end if
! draw function, dot style
    if(iFunStyleD.ne.0_2) then
      ic=SetColor(iFunStyleColor)
      do i=i1,i2,iFunStyleStep
        call GetFun(iFunA1,i,x)
        call GetFun(iFunA2,i,y)
        call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        call DrawIX(ix,iy,Int2(iFunStyleWidth))
      end do
      ic=SetColor(ic)
    end if
! draw function, mark square
    if(iFunMarkQ.ne.0_2) then
      if(iFunMarkColor.ge.0) then
        ic=SetColor(iFunMarkColor)
      else
        ic=SetColor(iFunStyleColor)
      end if
      do i=i1,i2,iFunMarkStep
        call GetFun(iFunA1,i,x)
        call GetFun(iFunA2,i,y)
        call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        call DrawIQ(ix,iy,Int2(iFunMarkSize))
      end do
      ic=SetColor(ic)
    end if
! draw function, mark plus
    if(iFunMarkP.ne.0_2) then
      if(iFunMarkColor.ge.0) then
        ic=SetColor(iFunMarkColor)
      else
        ic=SetColor(iFunStyleColor)
      end if
      do i=i1,i2,iFunMarkStep
        call GetFun(iFunA1,i,x)
        call GetFun(iFunA2,i,y)
        call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        call DrawIP(ix,iy,Int2(iFunMarkSize))
      end do
      ic=SetColor(ic)
    end if
! draw function, mark X
    if(iFunMarkX.ne.0_2) then
      if(iFunMarkColor.ge.0) then
        ic=SetColor(iFunMarkColor)
      else
        ic=SetColor(iFunStyleColor)
      end if
      do i=i1,i2,iFunMarkStep
        call GetFun(iFunA1,i,x)
        call GetFun(iFunA2,i,y)
        call preprocessFun(x,y,lPolar,lclip,lsquare,ldb)
        if(Present(yScale)) then
          x=x*xScale
          y=y*yScale
        end if
        call R2I(x,y,ix,iy)
        call DrawIX(ix,iy,Int2(iFunMarkSize))
      end do
      ic=SetColor(ic)
    end if
  end Subroutine DrawFunction

  Subroutine MTDrawFunction(lCheck)
    Implicit none
    Include 'resource.fd'
    Integer(4) idum
    Logical(4), intent(in) :: lCheck
    call EnterCriticalSection(Loc(DrawLock))
    lWinFld(kWin)=.false.
    if(.not.lCheck) then
      idum=MessageBoxQQ('Clear graphic window first?'C,'Draw function'C, &
                        MB$YESNOCANCEL.or.MB$ICONQUESTION)
      if(idum.eq.MB$IDCANCEL) then
        call LeaveCriticalSection(Loc(DrawLock))
        return
      end if
      if(idum.eq.MB$IDYES) call DrawWindow(lCheck)
    end if
    call DrawFunction(.true.)
    call LeaveCriticalSection(Loc(DrawLock))
  end Subroutine MTDrawFunction

  Subroutine MTModifyFunction(lCheck)
    Implicit none
    Include 'resource.fd'
    Logical(4), intent(in) :: lCheck
    Logical(4) ldum
    call MTDrawFunction(lCheck)
    iWinAction=3_2
    ldum=ModifyMenuFlagsQQ(3,1,$MenuUnChecked) !boundary
    ldum=ModifyMenuFlagsQQ(3,2,$MenuUnChecked) !expansion
    ldum=ModifyMenuFlagsQQ(3,3,$MenuUnChecked) !field
    ldum=ModifyMenuFlagsQQ(3,4,$MenuChecked)   !function
    ldum=ModifyMenuFlagsQQ(3,5,$MenuUnChecked) !object
    ldum=ModifyMenuFlagsQQ(3,6,$MenuUnChecked) !nothing
		call OutTxt('t1','Modify function!')
  end Subroutine MTModifyFunction

  Subroutine DrawRange(lCheck)
! draw the approximation and extrapolation range
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    Integer(2) idu,idu0,iw,i0,i1,i2,ix2,iy1,iy2,ih
    Type(xycoord) xy
    TYPE (FONTINFO) info
    idu=GETFONTINFO(info)
    ih=Int2(info.pixheight)/2_2
    ldum=lCheck
! extrapolation range
    idu=SetColor(2_2)
    call R2I(xMinExt,WinYmin(kWin),i1,iy1)
    if((abs(i1).gt.32000).or.(abs(iy1).gt.32000)) return
    call R2I(xMaxExt,WinYmax(kWin),i2,iy2)
    if((abs(i2).gt.32000).or.(abs(iy2).gt.32000)) return
    iy2=iy2+2_2
    iw=GetGTextExtent('Extrapolation'C)
    if(iw.lt.(i2-i1)) then
      i0=(i1+i2-iw)/2_2
      call MoveTo(i0,iy2,xy)
      call OutGText('Extrapolation'C)
    else
      iw=GetGTextExtent('Ext.'C)
      i0=(i1+i2-iw)/2_2
      call MoveTo(i0,iy2,xy)
      call OutGText('Ext.'C)
    end if
    if((i0-2_2).gt.i1) call DrawILine(i1,iy2+ih,i0-2_2,iy2+ih)
    if((i0+iw+2_2).lt.i2) call DrawILine(i0+iw+2_2,iy2+ih,i2,iy2+ih)
    call DrawILine(i1,iy1,i1,iy2)
    call DrawILine(i2,iy1,i2,iy2)
! approximation range
    idu0=SetColor(3_2)
    call R2I(xMinBas,WinYmin(kWin),i1,iy1)
    if((abs(i1).gt.32000).or.(abs(iy1).gt.32000)) return
    call R2I(xMaxBas,WinYmax(kWin),i2,iy2)
    if((abs(i2).gt.32000).or.(abs(iy2).gt.32000)) return
    iy2=iy2+2_2
    iw=GetGTextExtent('Approximation'C)
    if(iw.lt.(i2-i1)) then
      i0=(i1+i2-iw)/2_2
      call MoveTo(i0,iy2,xy)
      call OutGText('Approximation'C)
    else
      iw=GetGTextExtent('App.'C)
      i0=(i1+i2-iw)/2_2
      call MoveTo(i0,1_2,xy)
      call OutGText('App.'C)
    end if
    if((i0-2_2).gt.i1) call DrawILine(i1,iy2+ih,i0-2_2,iy2+ih)
    if((i0+iw+2_2).lt.i2) call DrawILine(i0+iw+2_2,iy2+ih,i2,iy2+ih)
    call DrawILine(i1,iy1,i1,iy2)
    call DrawILine(i2,iy1,i2,iy2)
! prediction range
    idu0=SetColor(4_2)
    iw=GetGTextExtent('Pre.'C)
    call R2I(WinXmax(kWin),WinYmax(kWin),ix2,iy2)
    if((abs(ix2).gt.32000).or.(abs(iy2).gt.32000)) return
    if((xMaxBas.gt.xMaxExt).and.(ix2.gt.(i2+iw))) then
      i1=i2
      i2=ix2
      iw=GetGTextExtent('Prediction'C)
      if(iw.lt.(i2-i1)) then
        i0=(i1+i2-iw)/2_2
        call MoveTo(i0,iy2,xy)
        call OutGText('Prediction'C)
      else
        iw=GetGTextExtent('Pre.'C)
        i0=(i1+i2-iw)/2_2
        call MoveTo(i0,iy2,xy)
        call OutGText('Pre.'C)
      end if
      if((i0-2_2).gt.i1) call DrawILine(i1,iy2+ih,i0-2_2,iy2+ih)
      if((i0+iw+2_2).lt.i2) call DrawILine(i0+iw+2_2,iy2+ih,i2,iy2+ih)
      call DrawILine(i1,iy1,i1,iy2)
      call DrawILine(i2,iy1,i2,iy2)
    end if
    idu=SetColor(idu0)
  end Subroutine DrawRange

END MODULE CHGRF




