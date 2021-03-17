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
MODULE CHBMP

! bitmaps

  USE CHGRF

  SAVE

  Real(8) BitcXMin,BitcXMax,BitcYMin,BitcYMax
  Integer(2) iBitcMinC,iBitcMaxC,iBitcXMin,iBitcXMax,iBitcYMin,iBitcYMax,iBitcXArg,iBitcYArg(3)
  Integer(1), Allocatable :: buffer(:)
  Integer(4) LAviFileName,LBmpFileName,kAVIframes,nAVIframes,nAVIfpsec
  Logical(4) lAVI,lAVIopen,lEntireWin,lBitcBar,lBitcScale
  Character(256) AviFileName,BmpFileName
  Character(256) FileName1,FileName2,FileName3

CONTAINS

! Default setting

  Subroutine BitConv_Defaults(lCheck)
! set the default data for the current graphic window
    Implicit none
    Logical, intent(in) :: lCheck
    iBitcXMin=1_2
    iBitcXMax=iWinWidth(kWin)
    iBitcYMin=1_2
    iBitcYMax=iWinHeight(kWin)
	  BitcXMin=WinXmin(kWin)
	  BitcYMin=WinYmin(kWin)
	  BitcXMax=WinXmax(kWin)
	  BitcYMax=WinYmax(kWin)
    if(lCheck) return
    lBitcBar=.false.
    lBitcScale=.true.
    iBitcXArg=1_2
    iBitcYArg(1)=0_2
    iBitcYArg(2)=2_2
    iBitcYArg(3)=0_2
    iBitcMinC=1_2
    iBitcMaxC=1_2
  end Subroutine BitConv_Defaults

! I/O

  Subroutine SaveBitmap(lCheck)
! save Bitmap data in a file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) ios,idum
    lEntireWin=.true.
    if(.not.lCheck) then
      if(lAVI) then
! open the dialog for the AVI file name
        call Open2write(-1,'Select AVI file to be written!','AVI file ',AviFileName,'AVI',ios)
        if(ios.gt.0) return
        LAviFileName=GetSLength(AviFileName)
      else
! open the dialog for the Bmp file name
        call Open2write(-1,'Select bitmap file to be written!','Bitmap file ',BmpFileName,'BMP',ios)
        if(ios.gt.0) return
        LBmpFileName=GetSLength(BmpFileName)
      end if
      idum=MessageBoxQQ('Map entire window on the bitmap?'C,'Save bitmap'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) lEntireWin=.false.
    end if
! save the image on the Bmp file
    call SaveImg(ios)
  end Subroutine SaveBitmap

  Subroutine OpenBitmap(lCheck)
! read Bitmap data from file
    Implicit none
    Logical, intent(in) :: lCheck
	  Integer(4) ios,idum
    lAVI=.false.
    lEntireWin=.true.
    if(.not.lCheck) then
! open the dialog for the Bmp file name
      call Open2read(-1,'Select bitmap file to be read!','Bitmap file ',BmpFileName,'BMP',ios)
      if(ios.gt.0) return
      LBmpFileName=GetSLength(BmpFileName)
      idum=MessageBoxQQ('Map bitmap on entire window?'C,'Open bitmap'C, &
                        MB$YesNo.or.MB$IconQuestion)
      if(idum.eq.MB$IDNO) lEntireWin=.false.
    end if
! load the image from the Bmp file
    call LoadImg(ios)
  end Subroutine OpenBitmap

  Subroutine SaveImg(ios)
! replaces SaveImage (which produces memory leaks)
    Implicit none
	Integer(4) ios,imsize,idum,WriteAviFrame,iErr,ier
    Integer(2) igrstat,iMovXlr,iMovYlr
    Logical(4) ldum
    Type(xycoord) xy
! get the graphic window
    call GetKWin(.false.)
    idum=SetActiveQQ(10+kWin)
! set the clip region to the entire window
    call SetViewOrg(0_2,0_2,xy)
    call SetClipRgn(0_2,0_2,iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)-1_2, &
    &                       iWinHeight(kWin)+iWinTop(kWin)+iWinBottom(kWin)-1_2)
    if(lEntireWin) then
      iMovXlr=iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)-1_2
      iMovYlr=iWinHeight(kWin)+iWinBottom(kWin)+iWinTop(kWin)-1_2
      imsize=ImageSize(0_2,0_2,iMovXlr,iMovYlr)
    else
      iMovXlr=iWinLeft(kWin)+iWinWidth(kWin)-1_2
      iMovYlr=iWinTop(kWin)+iWinHeight(kWin)-1_2
      imsize=ImageSize(iWinLeft(kWin),iWinTop(kWin),iMovXlr,iMovYlr)
    end if
! allocate a buffer for the bitmap and get the image into the buffer
    iErr=0
		if(allocated(buffer)) DeAllocate(Buffer)
    Allocate(buffer(0:imsize-1),Stat=iErr)
    if(iErr.ne.0) then
      idum=MessageBoxQQ('Cannot allocate buffer for the bitmap!'C,'Save image'C, &
                        MB$OK.or.MB$ICONSTOP)
		else
      call EnterCriticalSection(Loc(DrawLock))
      if(lEntireWin) then
        call GetImage(0_2,0_2,iMovXlr,iMovYlr,buffer)
      else
        call GetImage(iWinLeft(kWin),iWinTop(kWin),iMovXlr,iMovYlr,buffer)
      end if
      igrstat=grstatus()
      call LeaveCriticalSection(Loc(DrawLock))
      if(igrstat.ne.0) then
        idum=MessageBoxQQ('Cannot get the image!'C,'Save image'C,MB$OK.or.MB$ICONSTOP)
      else
        if(lAVI.and.(kAVIframes.lt.nAVIframes)) then
          idum=writeAVIframe(buffer,icPalette(0),235_4)
          if(idum.ne.0) then
            idum=MessageBoxQQ('Cannot write the AVI frame!'C,'Save image'C,MB$OK.or.MB$ICONSTOP)
          end if
          kAVIframes=kAVIframes+1_4
        else
! save the image in the bitmap file and deallocate the buffer
          open(2,file=BmpFileName,Access='Sequential',Form='Binary',Action='Write',Status='Replace',IOStat=iErr)
          if(iErr.ne.0) then
            idum=MessageBoxQQ('Error reading output file!'C,'Save image'C, &
                              MB$OK.or.MB$ICONSTOP)
          else
		        write(2,IOStat=iErr) buffer
	        end if
	      end if
	    end if
    end if
    ios=Int4(iErr)
    inquire(unit=2,opened=ldum)
    if(ldum) then
      ! EndFile(2)
		  close(2)
    end if
		DeAllocate(Buffer,Stat=ier)
! reset the clip region
    call SetViewOrg(iWinLeft(kWin),iWinTop(kWin),xy)
    call SetClipRgn(iWinLeft(kWin)+1_2,iWinTop(kWin)+1_2,iWinWidth(kWin)+iWinLeft(kWin)-1_2,iWinHeight(kWin)+iWinTop(kWin)-1_2)
  end Subroutine SaveImg

  Subroutine LoadImg(ios)
! replaces LoadImage
    USE CHLOA
    Implicit none
	  Integer(4) ios,idum,ierr
    Integer(2) Buff(0:11)
    Type(xycoord) xy
! get the graphic window and make it active
    call GetKWin(.false.)
    idum=SetActiveQQ(10+kWin)
! read BMP file header -> width and height of bitmap
    open(2,file=BmpFileName,Access='Sequential',Form='Binary',Action='Read',IOStat=iErr)
	  read(2,IOStat=iErr) buff
    close(2)
    if(lEntireWin) then
! adapt window data
      iWinWidth(kWin)=buff(9)-iWinLeft(kWin)-iWinRight(kWin)
      iWinHeight(kWin)=buff(11)-iWinTop(kWin)-iWinBottom(kWin)
      call InitWindow(.false.)
      call BitConv_Defaults(.true.) !!??
! set clip region to entire window
      call SetViewOrg(0_2,0_2,xy)
      call SetClipRgn(0_2,0_2,iWinWidth(kWin)+iWinLeft(kWin)+iWinRight(kWin)-1_2, &
      &                       iWinHeight(kWin)+iWinTop(kWin)+iWinBottom(kWin)-1_2)
! load the image from the bitmap file
      ios=LoadImag(BmpFileName,0,0)
    else
! adapt window data
      iWinWidth(kWin)=buff(9)
      iWinHeight(kWin)=buff(11)
      lWinInit=.true.
      call DrawWindow(.true.)
      call BitConv_Defaults(.true.) !!??
! load the image from the bitmap file
      ios=LoadImag(BmpFileName,0,0)
    end if
    if(ios.ne.0) then
      idum=MessageBoxQQ('LoadImage returned error!'C,'Load image'C, &
                        MB$OK.or.MB$ICONSTOP)
	  end if
! reset the clip region
    call SetViewOrg(iWinLeft(kWin),iWinTop(kWin),xy)
    call SetClipRgn(iWinLeft(kWin)+1_2,iWinTop(kWin)+1_2,iWinWidth(kWin)+iWinLeft(kWin)-1_2,iWinHeight(kWin)+iWinTop(kWin)-1_2)
  end Subroutine LoadImg

! Bitmap conversion

  Subroutine ImgToFun()
! convert actual bitmap info into a function f(x)
    Implicit none
    Real(8) x,y,qx,qy,xmin,xmax,ymin,ymax
	  Integer(4) idum,n
    Integer(2) icpos,i,k,iy,ky,i1
    Logical ldum
! get the graphic window and set the clip region to the entire window
    call GetKWin(.false.)
    idum=SetActiveQQ(10+kWin)
    n=0
    if((mFunA.lt.1).or.(mFun.lt.1)) return
    call Cursor(.true.,IDC_WAIT)
! add function arguments if required
    iBitcYArg(1)=max(0_2,iBitcYArg(1))
    iBitcYArg(2)=max(0_2,iBitcYArg(2))
    iBitcYArg(3)=max(0_2,iBitcYArg(3))
    iBitcXArg=max(0_2,iBitcXArg)
    i1=max(iBitcXArg,iBitcYArg(1),iBitcYArg(2),iBitcYArg(3))-Int2(mFunA)
    if(i1.gt.0_2) then
      call IncreaseFun(0,Int4(i1),ldum)
      nFunA=mFunA
    end if
! x and y arguments should be different
    if((iBitcXArg.eq.iBitcYArg(1)).or.(iBitcXArg.eq.iBitcYArg(2)).or.(iBitcXArg.eq.iBitcYArg(3))) iBitcXArg=0_2
! search pixels within color range
    qx=(BitcXMax-BitcXMin)/(Dble(iBitcXMax)-Dble(iBitcXMin))
    qy=(BitcYMax-BitcYMin)/(Dble(iBitcYMin)-Dble(iBitcYMax))
    do i=iBitcXMin,iBitcXMax
      iy=iBitcYMax+1_2
      ky=iy
      do k=iBitcYMin,iBitcYMax
        icpos=GetPixel(i,k)
        if((icpos.ge.iBitcMinC).and.(icpos.le.iBitcMaxC)) then
          i1=icpos
          if(lBitcBar) i1=GetPixel(i,k+1_2)
          if(icpos.eq.i1) then
            iy=k
            Exit
          end if
        end if
      end do
      do k=iBitcYMax,iBitcYMin,-1
        icpos=GetPixel(i,k)
        if((icpos.ge.iBitcMinC).and.(icpos.le.iBitcMaxC)) then
          i1=icpos
          if(lBitcBar) i1=GetPixel(i,k-1_2)
          if(icpos.eq.i1) then
            ky=k
            Exit
          end if
        end if
      end do
      if((iy.le.iBitcYMax).and.(ky.le.iBitcYMax)) then
        n=n+1
        if(n.ge.mFun) then
          call IncreaseFun(1,0,ldum)
          if(.not.ldum) Exit
          nFun=mFun
        end if
        if(iBitcXArg.gt.0) then
          x=BitcXMin+(Dble(i)-Dble(iBitcXMin))*qx
          Fun(iBitcXArg,n)=x
        end if
        if(iBitcYArg(1).gt.0) then
          y=BitcYMin+(Dble(ky)-Dble(iBitcYMax))*qy
          Fun(iBitcYArg(1),n)=y
        end if
        if(iBitcYArg(2).gt.0) then
          y=BitcYMin+(0.5d0*(Dble(iy)+Dble(ky))-Dble(iBitcYMax))*qy
          Fun(iBitcYArg(2),n)=y
        end if
        if(iBitcYArg(3).gt.0) then
          y=BitcYMin+(Dble(iy)-Dble(iBitcYMax))*qy
          Fun(iBitcYArg(3),n)=y
        end if
      end if
    end do
    nFun=n
! x and y scale values
    if(lBitcScale) then 
      xmin=pBig 
      xmax=nBig
      ymin=pBig
      ymax=nBig
! find min and max values
      do i=1,nFun
        if(iBitcXArg.gt.0) then
          x=Fun(iBitcXArg,i)
          if(x.gt.xmax) xmax=x
          if(x.lt.xmin) xmin=x
        end if
        if(iBitcYArg(1).gt.0) then
          y=Fun(iBitcYArg(1),i)
          if(y.gt.ymax) ymax=y
          if(y.lt.ymin) ymin=y
        end if
        if(iBitcYArg(2).gt.0) then
          y=Fun(iBitcYArg(2),i)
          if(y.gt.ymax) ymax=y
          if(y.lt.ymin) ymin=y
        end if
        if(iBitcYArg(3).gt.0) then
          y=Fun(iBitcYArg(3),i)
          if(y.gt.ymax) ymax=y
          if(y.lt.ymin) ymin=y
        end if
      end do
! scale
      qx=(BitcXMax-BitcXMin)/(xmax-xmin) 
      qy=(BitcYMax-BitcYMin)/(ymax-ymin)
      do i=1,nFun
        if(iBitcXArg.gt.0) then
          x=BitcXMin+(Fun(iBitcXArg,i)-xmin)*qx
          Fun(iBitcXArg,i)=x
        end if
        if(iBitcYArg(1).gt.0) then
          y=BitcYMin+(Fun(iBitcYArg(1),i)-ymin)*qy
          Fun(iBitcYArg(1),i)=y
        end if
        if(iBitcYArg(2).gt.0) then
          y=BitcYMin+(Fun(iBitcYArg(2),i)-ymin)*qy
          Fun(iBitcYArg(2),i)=y
        end if
        if(iBitcYArg(3).gt.0) then
          y=BitcYMin+(Fun(iBitcYArg(3),i)-ymin)*qy
          Fun(iBitcYArg(3),i)=y
        end if
      end do
    end if
    call Cursor(.false.,IDC_WAIT)
  end Subroutine ImgToFun

END MODULE CHBMP




