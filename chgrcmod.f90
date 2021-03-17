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
MODULE CHGRC

! Graphics

  USE CHFUN

  SAVE

  CONTAINS

  Subroutine GRC_Defaults(lCheck)
    Implicit none
    Logical, intent(in) :: lCheck
    Logical ldum
    ldum=lCheck
    GRChmin=nBig
    GRChmax=pBig
    iGRCLineEndStyle=0_2
    iGRCbackground=0_2
    iGRCcolor=1_2
    iGRCcolorB=1_2
    iGRCcolorMin=17_2
    iGRCcolorMax=116_2
    GRCcolorFMin=-1.0d0
    GRCcolorFMax=1.0d0
    lGRCfill=.true.
  end Subroutine GRC_Defaults

  Subroutine GRCclear()
    Implicit none
    Integer(2) i,k
    if(lGRCused) then
      iGRCxmin=1_2
      iGRCxmax=iWinWidth(kWin)-1_2
      iGRCymin=1_2
      iGRCymax=iWinHeight(kWin)-1_2
      nxGRC=iGRCxmax-iGRCxmin+1_2
      nyGRC=iGRCymax-iGRCymin+1_2
      call GRCallocate()
      if(lGRCallocated) then
        iGRCc=iGRCbackground
        GRCh=nBig
        do k=iGRCymin,iGRCymax
          do i=iGRCxmin,iGRCxmax
            iGRCx(i,k)=i
            iGRCy(i,k)=k
          end do
        end do
        lGRCallocated=.true.
      else
        lGRCused=.false.
      end if
    else
      if(Allocated(iGRCx)) DeAllocate(iGRCx)
      if(Allocated(iGRCy)) DeAllocate(iGRCy)
      if(Allocated(iGRCc)) DeAllocate(iGRCc)
      if(Allocated(GRCh)) DeAllocate(GRCh)
			nGRC=0_4
      lGRCallocated=.false.
    end if
  end Subroutine GRCclear

  Subroutine GRCallocate()
    Implicit none
    Integer(4) ier
    if(Allocated(iGRCx)) DeAllocate(iGRCx)
    if(Allocated(iGRCy)) DeAllocate(iGRCy)
    if(Allocated(iGRCc)) DeAllocate(iGRCc)
    if(Allocated(GRCh)) DeAllocate(GRCh)
    Allocate(iGRCx(nxGRC,nyGRC),iGRCy(nxGRC,nyGRC),iGRCc(nxGRC,nyGRC),GRCh(nxGRC,nyGRC),stat=ier)
		if(ier.ne.0) then
      ier=MessageBoxQQ('Memory alloction for graphic output failed!'C,'GRCallocate'C, &
                        MB$OK.or.MB$ICONEXCLAMATION)
      if(Allocated(iGRCx)) DeAllocate(iGRCx)
      if(Allocated(iGRCy)) DeAllocate(iGRCy)
      if(Allocated(iGRCc)) DeAllocate(iGRCc)
      if(Allocated(GRCh)) DeAllocate(GRCh)
			nGRC=0_4
      lGRCallocated=.false.
			return
		end if
    nGRC=Int4(nxGRC)*Int4(nyGRC)
    lGRCallocated=.true.
  end Subroutine GRCallocate

  Subroutine GRCflush()
    Implicit none
    call SetPixels(nGRC,iGRCx,iGRCy,iGRCc)
  end Subroutine GRCflush

  Subroutine GRCDraw3DLine(A,B)
    Implicit None
    Real(8) A(3),B(3),A2(2),B2(2),qx,qy,x0,y0,dA,dB,x,dx,y,dy,h,dh,dhc
    Integer(2) iA2(2),iB2(2),i,nx,ny,ix,iy,is,idu
    qx=dble(iWinWidth(kWin))/(WinXmax(kWin)-WinXmin(kWin))
    x0=WinXmin(kWin)
    qy=dble(iWinHeight(kWin))/(WinYmax(kWin)-WinYmin(kWin))
    y0=WinYmax(kWin)
    dhc=2.0d0*((.inv.abs(qx))+(.inv.abs(qy)))
    call Proj3D(A,ViewPlane,rEye,viewDist,A2(1),A2(2),dA)
    call Proj3D(B,ViewPlane,rEye,viewDist,B2(1),B2(2),dB)
    iA2(1)=nint(max(min((A2(1)-x0)*qx,16382.0d0),-16382.0d0),2)
    iA2(2)=nint(max(min((y0-A2(2))*qy,16382.0d0),-16382.0d0),2)
    iB2(1)=nint(max(min((B2(1)-x0)*qx,16382.0d0),-16382.0d0),2)
    iB2(2)=nint(max(min((y0-B2(2))*qy,16382.0d0),-16382.0d0),2)
    nx=abs(iB2(1)-iA2(1))+1_2
    ny=abs(iB2(2)-iA2(2))+1_2
    ix=iA2(1)
    iy=iA2(2)
    x=A2(1)
    y=A2(2)
    h=dA
    if(nx.gt.ny) then
      if(iA2(1).lt.iB2(1)) then
        is=1_2
      else
        is=-1_2
      end if
      dy=(B2(2)-A2(2))/Dble(max(0_2,nx-1_2))
      dh=(dB-dA)/Dble(max(0_2,nx-1_2))
      do i=1,nx
        iy=nint(max(min((y0-y)*qy,16382.0d0),-16382.0d0),2)
        if((iGRCxmin.le.ix).and.(iGRCxmax.ge.ix).and.(iGRCymin.le.iy).and.(iGRCymax.ge.iy)) then
          if((h.gt.GRChmax).or.(h.lt.GRChmin)) Cycle
          if((h+dhc).ge.GRCh(ix,iy)) then
            GRCh(ix,iy)=h+dhc
            if(lGRCallocated) then
              iGRCc(ix,iy)=iGRCcolor
            else
              idu=setColor(iGRCcolor)
              idu=setPixel(ix,iy)
            end if
          end if
        end if
        ix=ix+is
        y=y+dy
        h=h+dh
      end do
    else
      if(iA2(2).lt.iB2(2)) then
        is=1_2
      else
        is=-1_2
      end if
      dx=(B2(1)-A2(1))/Dble(max(0_2,ny-1_2))
      dh=(dB-dA)/Dble(max(0_2,ny-1_2))
      do i=1,ny
        ix=nint(max(min((x-x0)*qx,16382.0d0),-16382.0d0),2)
        if((iGRCxmin.le.ix).and.(iGRCxmax.ge.ix).and.(iGRCymin.le.iy).and.(iGRCymax.ge.iy)) then
          if((h.gt.GRChmax).or.(h.lt.GRChmin)) Cycle
          if((h+dhc).ge.GRCh(ix,iy)) then
            GRCh(ix,iy)=h+dhc
            if(lGRCallocated) then
              iGRCc(ix,iy)=iGRCcolor
            else
              idu=setColor(iGRCcolor)
              idu=setPixel(ix,iy)
            end if
          end if
        end if
        iy=iy+is
        x=x+dx
        h=h+dh
      end do
    end if
  end Subroutine GRCDraw3DLine

  Subroutine GRCDraw3DTriangle(A,B,C,lAB,lBC,lCA,fA0,fB0,fC0,lLog)
    Implicit None
    Real(8) A(3),B(3),C(3),fA,fB,fC,A2(2),B2(2),C2(2),P2(2),dA,dB,dC,qx,qy,x0,x1,x2,dx,y0,y,dy,h,h1,h2,dh,dxmax,q, &
    &       f,f1,f2,df,qa,qb
    Real(8), Optional :: fA0,fB0,fC0
    Logical, Optional :: lLog
    Integer(2) iA2(2),iB2(2),iC2(2),ix,ix1,ix2,iy,isx,isy,iC,idu
    Logical, intent(in) :: lAB,lBC,lCA
    Logical lf,llLog
    llLog=.false.
    if(Present(lLog)) then
      llLog=lLog
    end if
    lf=.false.
    if(Present(fC0)) then
      fA=fA0
      fB=fB0
      fC=fC0
      lf=.true.
    end if
    qx=dble(iWinWidth(kWin))/(WinXmax(kWin)-WinXmin(kWin))
    x0=WinXmin(kWin)
    qy=dble(iWinHeight(kWin))/(WinYmax(kWin)-WinYmin(kWin))
    y0=WinYmax(kWin)
    call Proj3D(A,ViewPlane,rEye,viewDist,A2(1),A2(2),dA)
    call Proj3D(B,ViewPlane,rEye,viewDist,B2(1),B2(2),dB)
    call Proj3D(C,ViewPlane,rEye,viewDist,C2(1),C2(2),dC)
    if(B2(2).lt.A2(2)) then ! sort points according to y components
      P2=A2
      A2=B2
      B2=P2
      dh=dA
      dA=dB
      dB=dh
      if(lf) then
        dh=fA
        fA=fB
        fB=dh
      end if
    end if
    if(C2(2).lt.A2(2)) then
      P2=A2
      A2=C2
      C2=P2
      dh=dA
      dA=dC
      dC=dh
      if(lf) then
        dh=fA
        fA=fC
        fC=dh
      end if
    end if
    if(C2(2).lt.B2(2)) then
      P2=B2
      B2=C2
      C2=P2
      dh=dB
      dB=dC
      dC=dh
      if(lf) then
        dh=fB
        fB=fC
        fC=dh
      end if
    end if
    iA2(1)=nint(max(min((A2(1)-x0)*qx,16382.0d0),-16382.0d0),2)
    iA2(2)=nint(max(min((y0-A2(2))*qy,16382.0d0),-16382.0d0),2)
    iB2(1)=nint(max(min((B2(1)-x0)*qx,16382.0d0),-16382.0d0),2)
    iB2(2)=nint(max(min((y0-B2(2))*qy,16382.0d0),-16382.0d0),2)
    iC2(1)=nint(max(min((C2(1)-x0)*qx,16382.0d0),-16382.0d0),2)
    iC2(2)=nint(max(min((y0-C2(2))*qy,16382.0d0),-16382.0d0),2)
    if(iC2(2).ne.iA2(2)) then
      q=(C2(1)-A2(1))/(C2(2)-A2(2))
      dxmax=B2(1)-A2(1)-(B2(2)-A2(2))*q
      y=A2(2)
      dy=(C2(2)-A2(2))/Dble(max(1_2,abs(iC2(2)-iA2(2))))
      if(iA2(2).lt.iB2(2)) then
        isy=1_2
      else
        isy=-1_2
      end if
      do iy=iA2(2),iC2(2),isy
        h1=(dA*(C2(2)-y)+dC*(y-A2(2)))/(C2(2)-A2(2))
        if(lf) f1=(fA*(C2(2)-y)+fC*(y-A2(2)))/(C2(2)-A2(2))
        if(iy.eq.iB2(2)) then
          dx=dxmax
          h2=dB
          if(lf) f2=fB
          x2=B2(1)
          x1=x2-dx
        else
          if((y.le.B2(2)).and.(B2(2).ne.A2(2))) then
            qa=max(-1.0d0,min(1.0d0,(B2(2)-y).div.(B2(2)-A2(2))))
            qb=max(-1.0d0,min(1.0d0,(y-A2(2)).div.(B2(2)-A2(2))))
            dx=dxmax*qb
            h2=dA*qa+dB*qb
            if(lf) f2=fA*qa+fB*qb
          else if(B2(2).ne.C2(2)) then
            qa=max(-1.0d0,min(1.0d0,(B2(2)-y).div.(B2(2)-C2(2))))
            qb=max(-1.0d0,min(1.0d0,(y-C2(2)).div.(B2(2)-C2(2))))
            dx=dxmax*qb
            h2=dB*qb+dC*qa
            if(lf) f2=fC*qa+fB*qb
          end if
          x1=A2(1)+(y-A2(2))*q
          x2=x1+dx
        end if
        ix1=nint(max(min((x1-x0)*qx,16382.0d0),-16382.0d0),2)
        ix2=nint(max(min((x2-x0)*qx,16382.0d0),-16382.0d0),2)
        if(ix2.ge.ix1) then
          isx=1_2
        else
          isx=-1_2
        end if
        h=h1
        dh=(h2-h1)/Dble(max(1_2,abs(ix2-ix1)-1_2))
        if(lf) f=f1
        if(lf) df=(f2-f1)/Dble(max(1_2,abs(ix2-ix1)-1_2))
        do ix=ix1,ix2,isx
          if((iGRCxmin.le.ix).and.(iGRCxmax.ge.ix).and.(iGRCymin.le.iy).and.(iGRCymax.ge.iy)) then
            if((h.gt.GRChmax).or.(h.lt.GRChmin)) Cycle
            if(h.ge.GRCh(ix,iy)) then
              if(lGRCfill) then ! fill color
                GRCh(ix,iy)=h
                if(lf) then
                  iC=Int2(in_Rinterval(f,GRCcolorFMin,GRCcolorFMax,Int4(iGRCcolorMin),Int4(iGRCcolorMax),llLog))
                else
                  iC=iGRCcolor
                end if
                if(lGRCallocated) then
                  iGRCc(ix,iy)=iC
                else
                  idu=setColor(iC)
                  idu=setPixel(ix,iy)
                end if
              end if
            end if
          end if
          h=h+dh
          if(lf) f=f+df
        end do
        y=y+dy
      end do
    end if
    iC=iGRCcolor
    if(iGRCcolorB.ge.0_2) iGRCcolor=iGRCcolorB
    if(lAB) call GRCDraw3DLine(A,B)
    if(lBC) call GRCDraw3DLine(B,C)
    if(lCA) call GRCDraw3DLine(C,A)
    iGRCcolor=iC
  end Subroutine GRCDraw3DTriangle

END MODULE CHGRC




