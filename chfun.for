c Copyright 2017, Christian Hafner
c
c This file is part of OpenMaXwell.
c
c OpenMaXwell is free software: you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation, either version 3 of the License, or
c (at your option) any later version.
c OpenMaXwell is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c You should have received a copy of the GNU General Public License
c along with OpenMaXwell.  If not, see <http://www.gnu.org/licenses/>.
c LEGENDRE FUNCTIONS

      subroutine legenm(cost,sint,p,l,m)
c legendre functions of degree m and order l=m..l
      implicit none
      Integer(4) l,m,i,iord
      Real(8) cost,sint,p(0:l),p0
      if(m.eq.0) then
c compute p(0,l)
        if(l.eq.0) then
c l=0
          p(0)=1.0d0
          return
        endif
        p(0)=1.0d0
        p(1)=cost
        do i=1,l-1
          p(i+1)=1.0d0/dble(i+1)*(dble(2*i+1)*cost*p(i)-dble(i)*p(i-1))
        end do
      else
c l>0
        p0=-1.0d0
        do i=2,m
          p0=-dble(2*i-1)*sint*p0
        end do
        p(0)=p0
        if(l.gt.m) then
          p(1)=dble(2*m+1)*cost*p(0)
          iord=m+1
          do i=1,l-m-1
            p(i+1)=1.0d0/dble(iord-m+1)*
     1             (dble(2*iord+1)*cost*p(i)-dble(m+iord)*p(i-1))
            iord=iord+1
          end do
        end if
      end if
      end

      subroutine legenmC(cost,sint,p,l,m)
c legendre functions of degree m and order l=m..l
      implicit none
      Integer(4) l,m,i,iord
      Complex(8) cost,sint,p(0:l),p0
      if(m.eq.0) then
c compute p(0,l)
        if(l.eq.0) then
c l=0
          p(0)=1.0d0
          return
        endif
        p(0)=1.0d0
        p(1)=cost
        do i=1,l-1
          p(i+1)=1.0d0/dble(i+1)*(dble(2*i+1)*cost*p(i)-dble(i)*p(i-1))
        end do
      else
c m>0
        p0=-1.0d0
        do i=2,m
          p0=-dble(2*i-1)*sint*p0
        end do
        p(0)=p0
        if(l.gt.m) then
          p(1)=dble(2*m+1)*cost*p(0)
          iord=m+1
          do i=1,l-m-1
            p(i+1)=1.0d0/dble(iord-m+1)*
     1             (dble(2*iord+1)*cost*p(i)-dble(m+iord)*p(i-1))
            iord=iord+1
          end do
        end if
      end if
      end


c SPHERICAL BESSEL FUNCTIONS

      subroutine vkfnor(r,ll,mm)
c scaling factors for vector spherical harmonics for degree m and order l=m..l
c values: r(m,m)..r(l,m) stored in r(1)..r(l-m+1)
c r: scaling factors
c l: order
c m: degree
      implicit none
      Integer(4) ll,mm,l,m,i,j
      Real(8) r(ll-mm+1),pi,fak
	Real(8), external:: dsqrtc
	Data pi/3.1415926535898d0/
	l=ll
	m=mm
c l>=!m
      if(l.lt.m) l=m
      if(l.le.0) l=1
c start mit r(m,m)
c fakultaet
      fak=1.0d0
      do i=2,2*m
        fak=fak*dble(i)
      end do
      if(m.eq.0) fak=fak*2.0d0
      fak=1.0d0/fak
c faktoren
      j=1
      do i=m,l
        if(i.ne.0) then
          r(j)=dsqrtc(dble(2*i+1)/dble(2*i*(i+1))/pi*fak)
          fak=fak*dble(i+1-m)/dble(i+1+m)
        else
          r(j)=0.0d0
        endif
        j=j+1
      end do
      end

      subroutine Bessel(n,z,cb,icode)
c  spherical bessel functions
c  icode=1     bessel     j
c  icode=2     neumann    y
c  icode=3     hankel 1   h1
c  icode=4     hankel 2   h2
c  icode=0     hankel 1   h1 with cut on the negative imaginary axis
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      complex(8) cb(0:n),z,cw(101),zj,zs,c
      Data zwpi,wpi2/0.63661977236758d0,1.2533141373155d0/
      ic=iabs(icode)
      fnu=0.5d0
      n1=n+1
	if(cdabs(z).lt.1.0d-300) then
        zs=wpi2/dsqrt(1.0d-300)
	else
        zs=wpi2/cdsqrt(z)
	end if
      if(ic.eq.1) then
        call zjr(fnu,z,n1,.false.,cb(0),nz,ier)
      else if(ic.eq.2) then
        call zyr(fnu,z,n1,.false.,cb(0),nz,cw(1),ier)
      else if(ic.eq.3) then
        call zhr(1,fnu,z,n1,.false.,cb(0),nz,ier)
      else if(ic.eq.4) then
        call zhr(2,fnu,z,n1,.false.,cb(0),nz,ier)
	else
        zj=(0.0d0,1.0d0)*z
        call zkr(fnu,zj,n1,.false.,cb(0),nz,ier)
        c=(0.0d0,-1.0d0)
        do i=0,n
          zj=c*cb(i)
          cb(i)=zwpi*zj
          c=(0.0d0,-1.0d0)*c
        end do
	end if
      do i=0,n
        c=cb(i)
        cb(i)=zs*c
      end do
      end

c BESSEL FUNCTIONS OF INTEGER ORDERS

      subroutine cj(z,cb,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cb(nmax)
	idum=ns
      fnu=0.0d0
      call zjr(fnu,z,n,.false.,cb(1),nz,ier)
      nmax=n-nz
      end

      subroutine cy(z,cb,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cb(nmax),cw(100)
	idum=ns
      nn=n
      if(nn.gt.100) nn=100
      fnu=0.0d0
      call zyr(fnu,z,n,.false.,cb(1),nz,cw(1),ier)
      nmax=nn-nz
      if((nn.gt.100).and.(ier.eq.0)) ier=-1
      end

      subroutine ch1(z,hn,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,zj,jn,hn(nmax)
      Data zwpi/0.63661977236758d0/
      zj=(0.0d0,-1.0d0)*z
      call ck(zj,hn,n,nmax,ns,ier)
      jn=(0.0d0,-1.0d0)
      do i=1,n
        zj=jn*hn(i)
        hn(i)=zwpi*zj
        jn=(0.0d0,-1.0d0)*jn
      end do
      end

      subroutine ch2(z,hn,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,z0,hn(nmax)
      z0=DConjg(z)
      call ch1(z0,hn,n,nmax,ns,ier)
      do i=1,n
        hn(i)=DConjg(hn(i))
      end do
      end

      subroutine ch1a(z,cb,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cb(nmax)
	idum=ns
      fnu=0.0d0
      call zhr(1_4,fnu,z,n,.false.,cb(1),nz,ier)
      nmax=n-nz
      end

      subroutine ci(z,cb,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cb(nmax)
	idum=ns
      fnu=0.0d0
      call zir(fnu,z,n,.false.,cb(1),nz,ier)
      nmax=n-nz
      end

      subroutine ck(z,cb,n,nmax,ns,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cb(nmax)
	idum=ns
      fnu=0.0d0
      call zkr(fnu,z,n,.false.,cb,nz,ier)
      nmax=n-nz
      end

c BESSEL FUNCTIONS OF REAL ORDERS

      subroutine zjr(fnuu,z,nn,scale,cy,nz,ier)
c bessel functions cy(i)=j(fnu+i-1,z) (scale=false) or scaled bessel
c functions cy(i)=exp(-abs(y))*j(fnu+i-1,z) with complez argument z
c for real, nonnegative orders fnu+i-1,i=1,...,n
c nz: zero terms due to underflow, ier: error flag
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Integer(4), intent(in) :: nn
      Integer(4), intent(out) :: nz,ier
      Real(8), intent(in) :: fnuu
      Complex(8) z,cy(nn),ci,csgn,zn
      Logical scale
      Data hpi/1.57079632679489662d0/
	fnu=fnuu
      if(fnu.lt.0.0d0) fnu=-fnu
	n=nn
      if(n.lt.1) n=1
      ier=0_4
      nz=0_4
      aa=1073741823.0d0
      ci=dcmplx(0.0d0,1.0d0)
      yy=dimag(z)
      az=abs(z)
      fn=fnu+dble(n-1)
      if(az.le.aa) then
        if(fn.le.aa) then
          aa=sqrt(aa)
          if((az.gt.aa).or.(fn.gt.aa)) ier=-1
          inu=int(fnu)
          inuh=inu/2
          ir=inu-2*inuh
          arg=(fnu-inu+ir)*hpi
          r1=cos(arg)
          r2=sin(arg)
          csgn=dcmplx(r1,r2)
          if(mod(inuh,2).eq.1) csgn=-csgn
          zn=-z*ci
          if(yy.lt.0.0d0) then
            zn=-zn
            csgn=dconjg(csgn)
            ci=dconjg(ci)
          endif
          call zir0(zn,fnu,scale,n,cy,nz)
          if(nz.ge.0) then
            nl=n-nz
            if(nl.ne.0) then
              rtol=1.0d16
              ascle=dexp(-700.0d0)*rtol
              do 1 i=1,nl
              zn=cy(i)
              aa=dble(zn)
              bb=dimag(zn)
              atol=1.0d0
              if(max(abs(aa),abs(bb)).le.ascle) then
                zn=zn*rtol
                atol=1.0d-16
              endif
              zn=zn*csgn
              cy(i)=zn*atol
              csgn=csgn*ci
    1         continue
            endif
          else
            nz=0
            ier=1
          endif
        else
          nz=0
          ier=1
        endif
      endif
      end


      subroutine zyr(fnuu,zz,nn,scale,cy,nz,cwrk,ier)
c neumann functions cy(i)=y(fnu+i-1,z) (scale=false) or scaled neumann
c functions cy(i)=exp(-abs(y))*y(fnu+i-1,z) with complez argument z
c for real, nonnegative orders fnu+i-1,i=1,...,n
c nz: zero terms due to underflow, ier: error flag
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Integer(4), intent(in) :: nn
      Integer(4), intent(out) :: nz,ier
      Complex(8) zz,z,cwrk(nn),cy(nn),c1,c2,ex,hci,zu,zv
      Logical scale
      ier=0_4
      nz=0_4
	z=zz
      xx=dble(z)
      yy=dimag(z)
      if(abs(xx).lt.1.0d-307.and.abs(yy).lt.1.0d-307) then
	  z=(1.0d-307,0.0d0)
        xx=dble(z)
        yy=dimag(z)
	end if
	fnu=fnuu
      if(fnu.lt.0.0d0) fnu=-fnu
	n=nn
      if(n.lt.1) n=1
      hci=dcmplx(0.0d0,0.5d0)
      call zhr(1_4,fnu,z,n,scale,cy,nz1,ier)
      if(ier.lt.1) then
        call zhr(2_4,fnu,z,n,scale,cwrk,nz2,ier)
        if(ier.lt.1) then
          nz=min(nz1,nz2)
          if(scale) then
            r1=cos(xx)
            r2=sin(xx)
            ex=dcmplx(r1,r2)
            ey=0.0d0
            tay=abs(yy+yy)
            if(tay.lt.700.0d0) ey=dexp(-tay)
            if(yy.lt.0.0d0) then
              c1=ex
              c2=dconjg(ex)*dcmplx(ey,0.0d0)
            else
              c1=ex*dcmplx(ey,0.0d0)
              c2=dconjg(ex)
            endif
            nz=0
            rtol=1.0d16
            ascle=dexp(-700.0d0)*rtol
            do 1 i=1,n
            zv=cwrk(i)
            aa=dble(zv)
            bb=dimag(zv)
            atol=1.0d0
            if(max(abs(aa),abs(bb)).le.ascle) then
              zv=zv*rtol
              atol=1.0d-16
            endif
            zv=zv*c2*hci
            zv=zv*atol
            zu=cy(i)
            aa=dble(zu)
            bb=dimag(zu)
            atol=1.0d0
            if(max(abs(aa),abs(bb)).le.ascle) then
              zu=zu*rtol
              atol=1.0d-16
            endif
            zu=zu*c1*hci
            zu=zu*atol
            cy(i)=zv-zu
            if(cy(i).eq.dcmplx(0.0d0,0.0d0).and.ey.eq.0.0d0) nz=nz+1
    1       continue
          else
            do 2 i=1,n
            cy(i)=hci*(cwrk(i)-cy(i))
    2       continue
          endif
        endif
      endif
      end


      subroutine zhr(mm,fnuu,zz,nn,scale,cy,nz,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Integer(4), intent(in) :: nn,mm
      Integer(4), intent(out) :: nz,ier
      Complex(8) z,zz,cy(nn),csgn,zn,zt
      Logical scale
      Data hpi/1.57079632679489662d0/
      ier=0_4
      nz=0_4
	z=zz
      xx=dble(z)
      yy=dimag(z)
      if(abs(xx).lt.1.0d-307.and.abs(yy).lt.1.0d-307) then
	  z=(1.0d-307,0.0d0)
        xx=dble(z)
        yy=dimag(z)
	end if
	fnu=fnuu
      if(fnu.lt.0.0d0) fnu=-fnu
	n=nn
      if(n.lt.1) n=1
	m=2
      if(mm.lt.2) m=1
      nnn=n
      aa=1073741823.0d0
      fn=fnu+dble(nnn-1)
      mmm=3-2*m
      fmm=dble(mmm)
      zn=z*dcmplx(0.0d0,-fmm)
      xn=dble(zn)
      yn=dimag(zn)
      az=abs(z)
      if(az.le.aa) then
        if(fn.le.aa) then
          aa=sqrt(aa)
          if((az.gt.aa).or.(fn.gt.aa)) ier=-1
          ufl=dexp(-700.0d0)
          if(az.ge.ufl) then
            if(fnu.gt.82.0d0) then
              mr=0
              if((xn.lt.0.0d0).or.(xn.eq.0.0d0.and.yn.lt.0.0d0
     *         .and.m.eq.2)) then
                mr=-m
                if(xn.eq.0.0d0.and.yn.lt.0.0d0) zn=-zn
              endif
              call zkrua(zn,fnu,scale,mr,nnn,cy,nw)
              if(nw.lt.0) then
                goto 9
              else
                nz=nz+nw
              endif
            else
              if(fn.gt.1.0d0) then
                if(fn.gt.2.0d0) then
                  call zikrp1(zn,fnu,scale,2_4,nnn,cy,nuf)
                  if(nuf.lt.0) goto 9
                  nz=nz+nuf
                  nnn=nnn-nuf
                  if(nnn.eq.0) then
                    if(xn.lt.0.0d0) goto 9
                    return
                  endif
                elseif(az.le.1.0d-16) then
                  arg=0.5d0*az
                  aln=-fn*dlog(arg)
                  if(aln.gt.700.0d0) goto 9
                endif
              endif
              if((xn.lt.0.0d0).or.(xn.eq.0.0d0.and.yn.lt.0.0d0
     *         .and.m.eq.2)) then
                mr=-mmm
                call zkrc(zn,fnu,scale,mr,nnn,cy,nw)
                if(nw.lt.0) then
                  goto 9
                else
                  nz=nw
                endif
              else
                call zkr0(zn,fnu,scale,nnn,cy,nz)
              endif
            endif
            sgn=sign(hpi,-fmm)
            inu=int(fnu)
            inuh=inu/2
            ir=inu-2*inuh
            arg =(fnu-inu+ir)*sgn
            rhpi=1.0d0/sgn
            cpn=rhpi*cos(arg)
            spn=rhpi*sin(arg)
            csgn=dcmplx(-spn,cpn)
            if(mod(inuh,2).eq.1) csgn=-csgn
            zt=dcmplx(0.0d0,-fmm)
            rtol=1.0d16
            ascle=ufl*rtol
            do 1 i=1,nnn
            zn=cy(i)
            aa=dble(zn)
            bb=dimag(zn)
            atol=1.0d0
            if(max(abs(aa),abs(bb)).le.ascle) then
              zn=zn*rtol
              atol=1.0d-16
            endif
            zn=zn*csgn
            cy(i)=zn*atol
            csgn=csgn*zt
    1       continue
            return
          endif
        endif
      endif
    9 nz=0
      ier=1
      end


      subroutine zir(fnuu,z,nn,scale,cy,nz,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Integer(4), intent(in) :: nn
      Integer(4), intent(out) :: nz,ier
      Complex(8) z,cy(nn),cone,csgn,zn
      Logical scale
      Data cone/(1.0d0,0.0d0)/
      Data pi/3.14159265358979324d0/
      ier=0
      nz=0
	fnu=fnuu
      if(fnu.lt.0.0d0) fnu=-fnu
	n=nn
      if(n.lt.1) n=1
      xx=dble(z)
      yy=dimag(z)
      aa=1073741823.0d0
      az=abs(z)
      if(az.le.aa) then
        fn=fnu+dble(n-1)
        if(fn.le.aa) then
          aa=sqrt(aa)
          if((az.gt.aa).or.(fn.gt.aa)) ier=-1
          zn=z
          csgn=cone
          if(xx.lt.0.0d0) then
            zn=-z
            inu=int(fnu)
            arg=(fnu-inu)*pi
            if(yy.lt.0.0d0) arg=-arg
            s1=cos(arg)
            s2=sin(arg)
            csgn=dcmplx(s1,s2)
            if(mod(inu,2).eq.1) csgn=-csgn
          endif
          call zir0(zn,fnu,scale,n,cy,nz)
          if(nz.lt.0) then
            if(nz.eq.(-3)) goto 9
          elseif(xx.lt.0.0d0) then
            nnn=n-nz
            if(nnn.ne.0) then
              rtol=1.0d16
              ascle=dexp(-700.0d0)*rtol
              do 1 i=1,nnn
              zn=cy(i)
              aa=dble(zn)
              bb=dimag(zn)
              atol=1.0d0
              if(max(abs(aa),abs(bb)).le.ascle) then
                zn=zn*rtol
                atol=1.0d-16
              endif
              zn=zn*csgn
              cy(i)=zn*atol
              csgn=-csgn
    1         continue
            endif
          endif
          return
        endif
      endif
    9 nz=0
      ier=1
      end


      subroutine zkr(fnuu,zz,nn,scale,cy,nz,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Integer(4), intent(in) :: nn
      Integer(4), intent(out) :: nz,ier
      Complex(8) z,zz,cy(nn)
      Logical scale
      ier=0_4
      nz=0_4
	z=zz
      xx=dble(z)
      yy=dimag(z)
      if(abs(xx).lt.1.0d-307.and.abs(yy).lt.1.0d-307) then
	  z=(1.0d-307,0.0d0)
        xx=dble(z)
        yy=dimag(z)
	end if
	fnu=fnuu
      if(fnu.lt.0.0d0) fnu=-fnu
	n=nn
      if(n.lt.1) n=1
      nnn=n
      aa=1073741823.0d0
      az=abs(z)
      fn=fnu+dble(nnn-1)
      if(az.le.aa) then
        if(fn.le.aa) then
          aa=sqrt(aa)
          if((az.gt.aa).or.(fn.gt.aa)) ier=-1
          ufl=dexp(-700.0d0)
          if(az.ge.ufl) then
            if(fnu.gt.82.0d0) then
              mr=0
              if(xx.lt.0.0d0) then
                mr=1
                if(yy.lt.0.0d0) mr=-1
              endif
              call zkrua(z,fnu,scale,mr,nnn,cy,nw)
              if(nw.ge.0) then
                nz=nz+nw
                return
              endif
            else
              if(fn.gt.1.0d0) then
                if(fn.gt.2.0d0) then
                  call zikrp1(z,fnu,scale,2_4,nnn,cy,nuf)
                  if(nuf.lt.0) goto 9
                  nz=nz+nuf
                  nnn=nnn-nuf
                  if(nnn.eq.0) then
                    if(xx.lt.0.0d0) goto 9
                    return
                  endif
                elseif(az.le.1.0d-16) then
                  arg=0.5d0*az
                  aln=-fn*dlog(arg)
                  if(aln.gt.700.0d0) goto 9
                endif
              endif
              if(xx.lt.0.0d0) then
                if(nz.ne.0) goto 9
                mr=1
                if(yy.lt.0.0d0) mr=-1
                call zkrc(z,fnu,scale,mr,nnn,cy,nw)
                if(nw.ge.0) then
                  nz=nw
                  return
                endif
              else
                call zkr0(z,fnu,scale,nnn,cy,nw)
                if(nw.ge.0) then
                  nz=nw
                  return
                endif
              endif
            endif
          endif
        endif
      endif
    9 nz=0
      ier=1
      end


      subroutine zairy(deriv,z,scale,ai,nz,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) ai,z,cy(1),cone,csq,s1,s2,trm1,trm2,z3,zta,zexp
      Logical, intent(in) :: scale,deriv
      External zexp
      Data cone/(1.0d0,0.0d0)/
      Data tth,c1,c2,coef/6.66666666666666667d-01,
     *                    3.55028053887817240d-01,
     *                    2.58819403792806799d-01,
     *                    1.83776298473930683d-01/
      ier=0
      nz=0
      az=abs(z)
      if(az.gt.1.0d0) then
        fnu=1.0d0/3.0d0
        if(deriv) fnu=2.0d0*fnu
        aa=1073741823.0d0
        alaz=dlog(az)
        aa=aa**tth
        if(az.gt.aa) goto 9
        aa=sqrt(aa)
        if(az.gt.aa) ier=-1
        csq=sqrt(z)
        zta=z*csq*dcmplx(tth,0.0d0)
        iflag=0
        sfac=1.0d0
        zi=dimag(z)
        zr=dble(z)
        ak=dimag(zta)
        if(zr.lt.0.0d0) then
          bk=dble(zta)
          ck=-abs(bk)
          zta=dcmplx(ck,ak)
        endif
        if((zi.eq.0.0d0).and.(zr.le.0.0d0)) zta=dcmplx(0.0d0,ak)
        aa=dble(zta)
        if(aa.ge.0.0d0.and.zr.gt.0.0d0) then
          if(scale) then
            if(aa.ge.664.0d0) then
              aa=-aa-0.25d0*alaz
              iflag=2
              sfac=1.0d16
              if(aa.lt.(-700.0d0)) then
                nz=1
                ai=dcmplx(0.0d0,0.0d0)
                return
              endif
            endif
          endif
          call zkr0(zta,fnu,scale,1_4,cy,nz)
        else
          if(scale) then
            if(aa.le.(-664.0d0)) then
              aa=-aa+0.25d0*alaz
              iflag=1
              sfac=1.0d-16
              if(aa.gt.700.0d0) goto 9
            endif
          endif
          mr=1
          if(zi.lt.0.0d0) mr=-1
          call zkrca(zta,fnu,scale,mr,1_4,cy,nnn)
          if(nnn.ge.0) then
            nz=nz+nnn
            goto 1
          else
            goto 9
          endif
    1     s1=cy(1)*dcmplx(coef,0.0d0)
          if(iflag.ne.0) then
            s1=s1*dcmplx(sfac,0.0d0)
            if(deriv) then
              s1=-s1*z
            else
              s1=s1*csq
            endif
            ai=s1*dcmplx(1.0d0/sfac,0.0d0)
          elseif(deriv) then
            ai=-z*s1
          else
            ai=csq*s1
          endif
        endif
      else
        s1=cone
        s2=cone
        if(az.lt.1.0d-16) then
          aa=1.0d-304
          s1=dcmplx(0.0d0,0.0d0)
          if(deriv) then
            ai=-dcmplx(c2,0.0d0)
            aa=sqrt(aa)
            if(az.gt.aa) s1=z*z*dcmplx(0.5d0,0.0d0)
            ai=ai+s1*dcmplx(c1,0.0d0)
          else
            if(az.gt.aa) s1=dcmplx(c2,0.0d0)*z
            ai=dcmplx(c1,0.0d0)-s1
          endif
        else
          aa=az*az
          if(aa.ge.1.0d-16/az) then
            trm1=cone
            trm2=cone
            atrm=1.0d0
            z3=z*z*z
            az3=az*aa
            if(deriv) then
              ak=3.0d0
              bk=1.0d0
              ck=3.0d0
              d1=15.0d0
              d2=3.0d0
              ad=3.0d0
              ak=33.0d0
              bk=21.0d0
            else
              ak=2.0d0
              bk=3.0d0
              ck=4.0d0
              d1=6.0d0
              d2=12.0d0
              ad=6.0d0
              ak=24.0d0
              bk=30.0d0
            endif
            z3r=dble(z3)
            z3i=dimag(z3)
            do 2 k=1,25
            trm1=trm1*dcmplx(z3r/d1,z3i/d1)
            s1=s1+trm1
            trm2=trm2*dcmplx(z3r/d2,z3i/d2)
            s2=s2+trm2
            atrm=atrm*az3/ad
            d1=d1+ak
            d2=d2+bk
            ad=min(d1,d2)
            if(atrm.lt.1.0d-16*ad) then
              goto 3
            else
              ak=ak+18.0d0
              bk=bk+18.0d0
            endif
    2       continue
          endif
    3     if(deriv) then
            ai=-s2*dcmplx(c2,0.0d0)
            if(az.gt.1.0d-16) ai=ai+z*z*s1*dcmplx(c1/2.0d0,0.0d0)
          else
            ai=s1*dcmplx(c1,0.0d0)-z*s2*dcmplx(c2,0.0d0)
          endif
          if(scale) then
            zta=z*sqrt(z)*dcmplx(tth,0.0d0)
            ifl=1
            ai=ai*zexp(zta,ifl)
          endif
        endif
      endif
    9 nz=0
      ier=1
      end


      subroutine zir0(z,fnu,scale,n,cy,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cy(n),czero,cw(2)
      Logical scale
      Data czero/(0.0d0,0.0d0)/
      nz=0
      az=abs(z)
      nn=n
      dfnu=fnu+dble(n-1)
      if((az.gt.2.0d0).and.(az*az*0.25d0.gt.dfnu+1.0d0)) goto 20
      call zirp(z,fnu,scale,nn,cy,nw)
      inw=abs(nw)
      nz=nz+inw
      nn=nn-inw
      if(nn.eq.0) return
      if(nw.ge.0) return
      dfnu=fnu+dble(nn-1)
   20 if(az.ge.21.0d0) then
        if((dfnu.gt.1.0d0).and.(az+az.lt.dfnu*dfnu)) goto 40
        call zirua0(z,fnu,scale,nn,cy,nw)
        if(nw.lt.0) goto 120
        return
      else
        if(dfnu.le.1.0d0) goto 100
      endif
   40 call zikrp1(z,fnu,scale,1_4,nn,cy,nw)
      if(nw.lt.0) then
        goto 120
      else
        nz=nz+nw
        nn=nn-nw
        if(nn.eq.0) return
        dfnu=fnu+dble(nn-1)
        if((dfnu.le.82.0d0).and.(az.le.82.0d0)) goto 60
        nui=int(82.0d0-dfnu)+1
        nui=max(nui,0)
        call zirua(z,fnu,scale,nn,cy,nw,nui,nlast)
        if(nw.lt.0) goto 120
        nz=nz+nw
        if(nlast.eq.0) return
        nn=nlast
   60   if(az.gt.21.0d0) then
          call zikrp1(z,fnu,scale,2_4,2_4,cw,nw)
          if(nw.lt.0) then
            nz=nn
            do 80 i=1,nn
            cy(i)=czero
   80       continue
            return
          elseif(nw.gt.0) then
            goto 120
          else
            call zirw(z,fnu,scale,nn,cy,nw,cw)
            if(nw.lt.0) goto 120
            return
          endif
        endif
      endif
  100 call zirm(z,fnu,scale,nn,cy,nw)
      if(nw.ge.0) return
  120 nz=-1
      if(nw.eq.(-2)) nz=-2
      if(nw.eq.(-3)) nz=-3
      end


      subroutine zirw(zr,fnu,scale,n,y,nz,cw)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) zr,cw(2),y(n),c1,c2,cinu,cscl,ct,rct,st
      Logical scale
      nz=0
      call zkr0(zr,fnu,scale,2_4,cw,nw)
      if(nw.ne.0) then
        nz=-1
        if(nw.eq.(-2)) nz=-2
        if(nw.eq.(-3)) nz=-3
      else
        call zirbr(zr,fnu,n,y)
        cinu=dcmplx(1.0d0,0.0d0)
        if(scale) then
          yy=dimag(zr)
          s1=cos(yy)
          s2=sin(yy)
          cinu=dcmplx(s1,s2)
        endif
        acw=abs(cw(2))
        ascle=1.0d-288
        cscl=dcmplx(1.0d0,0.0d0)
        if(acw.gt.ascle) then
          ascle=1.0d0/ascle
          if(acw.ge.ascle) cscl=dcmplx(1.0d-16,0.0d0)
        else
          cscl=dcmplx(1.0d16,0.0d0)
        endif
        c1=cw(1)*cscl
        c2=cw(2)*cscl
        st=y(1)
        ct=zr*(c2+st*c1)
        act=abs(ct)
        rct=dcmplx(1.0d0/act,0.0d0)
        ct=dconjg(ct)*rct
        cinu=cinu*rct*ct
        y(1)=cinu*cscl
        if(n.ne.1) then
          do 1 i=2,n
          cinu=st*cinu
          st=y(i)
          y(i)=cinu*cscl
    1     continue
        endif
      endif
      end


      subroutine zikrp1(z,fnu,scale,ikflg,n,y,nuf)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),arg,asum,bsum,cz,czero,phi,sum,zb,zeta1,zeta2,
     * zn,zr,cwrk(16)
      Logical scale
      Data czero/(0.0d0,0.0d0)/
      Data aic/1.265512123484645396d+00/
      nuf=0
      nn=n
      x=dble(z)
      zr=z
      if(x.lt.0.0d0) zr=-z
      zb=zr
      yy=dimag(zr)
      ax=abs(x)*1.7321d0
      ay=abs(yy)
      iform=1
      if(ay.gt.ax) iform=2
      gnu=max(fnu,1.0d0)
      if(ikflg.ne.1) then
        fnn=dble(nn)
        gnn=fnu+fnn-1.0d0
        gnu=max(gnn,fnn)
      endif
      if(iform.eq.2) then
        zn=-zr*dcmplx(0.0d0,1.0d0)
        if(yy.le.0.0d0) zn=dconjg(-zn)
        call zjhrua(zn,gnu,1_4,phi,arg,zeta1,zeta2,asum,bsum)
        cz=-zeta1+zeta2
        aarg=abs(arg)
      else
        init=0
        call zikrp2(zr,gnu,ikflg,1_4,init,phi,zeta1,zeta2,sum,cwrk)
        cz=-zeta1+zeta2
      endif
      if(scale) cz=cz-zb
      if(ikflg.eq.2) cz=-cz
      aphi=abs(phi)
      rcz=dble(cz)
      if(rcz.le.700.0d0) then
        if(rcz.lt.664.0d0) then
          if(rcz.ge.(-700.0d0)) then
            if(rcz.gt.(-664.0d0)) goto 40
            rcz=rcz+dlog(aphi)
            if(iform.eq.2) rcz=rcz-0.25d0*dlog(aarg)-aic
            if(rcz.gt.(-700.0d0)) then
              ascle=1.0d-288
              cz=cz+cdlog(phi)
              if(iform.ne.1) cz=cz-dcmplx(0.25d0,0.0d0)
     *                       *cdlog(arg)-dcmplx(aic,0.0d0)
              ax=dexp(rcz)/1.0d-16
              ay=dimag(cz)
              cz=dcmplx(ax,0.0d0)*dcmplx(cos(ay),sin(ay))
              call zpt(cz,nw,ascle)
              if(nw.ne.1) goto 40
            endif
          endif
          do 20 i=1,nn
          y(i)=czero
   20     continue
          nuf=nn
          return
        else
          rcz=rcz+dlog(aphi)
          if(iform.eq.2) rcz=rcz-0.25d0*dlog(aarg)-aic
          if(rcz.gt.700.0d0) goto 80
        endif
   40   if(ikflg.ne.2) then
          if(n.ne.1) then
   60       continue
            gnu=fnu+dble(nn-1)
            if(iform.eq.2) then
              call zjhrua(zn,gnu,1_4,phi,arg,zeta1,zeta2,asum,
     *                    bsum)
              cz=-zeta1+zeta2
              aarg=abs(arg)
            else
              init=0
              call zikrp2(zr,gnu,ikflg,1_4,init,phi,zeta1,zeta2,
     *                   sum,cwrk)
              cz=-zeta1+zeta2
            endif
            if(scale) cz=cz-zb
            aphi=abs(phi)
            rcz=dble(cz)
            if(rcz.ge.(-700.0d0)) then
              if(rcz.gt.(-664.0d0)) return
              rcz=rcz+dlog(aphi)
              if(iform.eq.2) rcz=rcz-0.25d0*dlog(aarg)-aic
              if(rcz.gt.(-700.0d0)) then
                ascle=1.0d-288
                cz=cz+cdlog(phi)
                if(iform.ne.1) cz=cz-dcmplx(0.25d0,0.0d0)*cdlog(arg)-
     *                         dcmplx(aic,0.0d0)
                ax=dexp(rcz)/1.0d-16
                ay=dimag(cz)
                cz=dcmplx(ax,0.0d0)*dcmplx(cos(ay),sin(ay))
                call zpt(cz,nw,ascle)
                if(nw.ne.1) return
              endif
            endif
            y(nn)=czero
            nn=nn-1
            nuf=nuf+1
            if(nn.ne.0) goto 60
          endif
        endif
        return
      endif
   80 nuf=-1
      end


      subroutine zirua(z,fnu,scale,n,y,nz,nui,nlast)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),cscl,cscr,rz,s1,s2,st,cy(2)
      Dimension bry(3)
      Logical scale
      nz=0
      xx=dble(z)
      yy=dimag(z)
      ax=abs(xx)*1.7321d0
      ay=abs(yy)
      iform=1
      if(ay.gt.ax) iform=2
      if(nui.eq.0) then
        if(iform.eq.2) then
          call zirua1(z,fnu,scale,n,y,nw,nlast)
        else
          call zirua2(z,fnu,scale,n,y,nw,nlast)
        endif
        if(nw.ge.0) then
          nz=nw
          return
        endif
      else
        fnui=dble(nui)
        dfnu=fnu+dble(n-1)
        gnu=dfnu+fnui
        if(iform.eq.2) then
          call zirua1(z,gnu,scale,2_4,cy,nw,nlast)
        else
          call zirua2(z,gnu,scale,2_4,cy,nw,nlast)
        endif
        if(nw.ge.0) then
          if(nw.ne.0) then
            nlast=n
          else
            ay=abs(cy(1))
            bry(1)=1.0d-288
            bry(2)=1.0d0/bry(1)
            bry(3)=bry(2)
            iflag=2
            ascle=bry(2)
            ax=1.0d0
            cscl=dcmplx(ax,0.0d0)
            if(ay.le.bry(1)) then
              iflag=1
              ascle=bry(1)
              ax=1.0d16
              cscl=dcmplx(ax,0.0d0)
            elseif(ay.ge.bry(2)) then
              iflag=3
              ascle=bry(3)
              ax=1.0d-16
              cscl=dcmplx(ax,0.0d0)
            endif
            ay=1.0d0/ax
            cscr=dcmplx(ay,0.0d0)
            s1=cy(2)*cscl
            s2=cy(1)*cscl
            rz=dcmplx(2.0d0,0.0d0)/z
            do 20 i=1,nui
            st=s2
            s2=dcmplx(dfnu+fnui,0.0d0)*rz*s2+s1
            s1=st
            fnui=fnui-1.0d0
            if(iflag.lt.3) then
              st=s2*cscr
              str=dble(st)
              sti=dimag(st)
              str=abs(str)
              sti=abs(sti)
              stm=max(str,sti)
              if(stm.gt.ascle) then
                iflag=iflag+1
                ascle=bry(iflag)
                s1=s1*cscr
                s2=st
                ax=ax*1.0d-16
                ay=1.0d0/ax
                cscl=dcmplx(ax,0.0d0)
                cscr=dcmplx(ay,0.0d0)
                s1=s1*cscl
                s2=s2*cscl
              endif
            endif
   20       continue
            y(n)=s2*cscr
            if(n.ne.1) then
              nl=n-1
              fnui=nl
              k=nl
              do 40 i=1,nl
              st=s2
              s2=dcmplx(fnu+fnui,0.0d0)*rz*s2+s1
              s1=st
              st=s2*cscr
              y(k)=st
              fnui=fnui-1.0d0
              k=k-1
              if(iflag.lt.3) then
                str=dble(st)
                sti=dimag(st)
                str=abs(str)
                sti=abs(sti)
                stm=max(str,sti)
                if(stm.gt.ascle) then
                  iflag=iflag+1
                  ascle=bry(iflag)
                  s1=s1*cscr
                  s2=st
                  ax=ax*1.0d-16
                  ay=1.0d0/ax
                  cscl=dcmplx(ax,0.0d0)
                  cscr=dcmplx(ay,0.0d0)
                  s1=s1*cscl
                  s2=s2*cscl
                endif
              endif
   40         continue
            endif
          endif
          return
        endif
      endif
      nz=-1
      if(nw.eq.(-2)) nz=-2
      end


      subroutine zirp(z,fnu,scale,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),ak1,ck,coef,cone,crsc,cz,czero,hz,rz,s1,s2,w(2)
      Real(8) zlgam
      Logical scale
      External zlgam
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      nz=0
      az=abs(z)
      if(az.ne.0.0d0) then
        x=dble(z)
        arm=1.0d-304
        rtr1=sqrt(arm)
        crsc=dcmplx(1.0d0,0.0d0)
        iflag=0
        if(az.lt.arm) then
          nz=n
          if(fnu.eq.0.0d0) nz=nz-1
        else
          hz=z*dcmplx(0.5d0,0.0d0)
          cz=czero
          if(az.gt.rtr1) cz=hz*hz
          acz=abs(cz)
          nn=n
          ck=cdlog(hz)
   20     continue
          dfnu=fnu+dble(nn-1)
          fnup=dfnu+1.0d0
          ak1=ck*dcmplx(dfnu,0.0d0)
          idum=0
          ak=zlgam(fnup)
          ak1=ak1-dcmplx(ak,0.0d0)
          if(scale) ak1=ak1-dcmplx(x,0.0d0)
          rak1=dble(ak1)
          if(rak1.gt.(-700.0d0)) then
            if(rak1.le.(-664.0d0)) then
              iflag=1
              ss=1.0d16
              crsc=dcmplx(1.0d-16,0.0d0)
              ascle=arm*ss
            endif
            ak=dimag(ak1)
            aa=dexp(rak1)
            if(iflag.eq.1) aa=aa*ss
            coef=dcmplx(aa,0.0d0)*dcmplx(cos(ak),sin(ak))
            atol=1.0d-16*acz/fnup
            il=min(2,nn)
            do 60 i=1,il
            dfnu=fnu+nn-i
            fnup=dfnu+1.0d0
            s1=cone
            if(acz.ge.1.0d-16*fnup) then
              ak1=cone
              ak=fnup+2.0d0
              s=fnup
              aa=2.0d0
   40         continue
              rs=1.0d0/s
              ak1=ak1*cz*dcmplx(rs,0.0d0)
              s1=s1+ak1
              s=s+ak
              ak=ak+2.0d0
              aa=aa*acz*rs
              if(aa.gt.atol) goto 40
            endif
            m=nn-i+1
            s2=s1*coef
            w(i)=s2
            if(iflag.ne.0) then
              call zpt(s2,nw,ascle)
              if(nw.ne.0) goto 80
            endif
            y(m)=s2*crsc
            if(i.ne.il) coef=coef*dcmplx(dfnu,0.0d0)/hz
   60       continue
            goto 100
          endif
   80     nz=nz+1
          y(nn)=czero
          if(acz.gt.dfnu) goto 180
          nn=nn-1
          if(nn.eq.0) return
          goto 20
  100     if(nn.gt.2) then
            k=nn-2
            ak=k
            rz=(cone+cone)/z
            if(iflag.eq.1) then
              s1=w(1)
              s2=w(2)
              do 120 l=3,nn
              ck=s2
              s2=s1+dcmplx(ak+fnu,0.0d0)*rz*s2
              s1=ck
              ck=s2*crsc
              y(k)=ck
              ak=ak-1.0d0
              k=k-1
              if(abs(ck).gt.ascle) goto 140
  120         continue
              return
  140         ib=l+1
              if(ib.gt.nn) return
            else
              ib=3
            endif
            do 160 i=ib,nn
            y(k)=dcmplx(ak+fnu,0.0d0)*rz*y(k+1)+y(k+2)
            ak=ak-1.0d0
            k=k-1
  160       continue
          endif
          return
  180     continue
          nz=-nz
          return
        endif
      endif
      y(1)=czero
      if(fnu.eq.0.0d0) y(1)=cone
      if(n.ne.1) then
        do 200 i=2,n
        y(i)=czero
  200   continue
      endif
      end


      subroutine zirm(z,fnu,scale,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),ck,cnorm,cone,ctwo,czero,p1,p2,pt,rz,sum,zexp
      Real(8) zlgam
      Logical scale
      External zlgam,zexp
      Data czero,cone,ctwo/(0.0d0,0.0d0),(1.0d0,0.0d0),(2.0d0,0.0d0)/
      scle=1.0d-288
      nz=0
      az=abs(z)
      x=dble(z)
      iaz=int(az)
      ifnu=int(fnu)
      inu=ifnu+n-1
      at=iaz+1.0d0
      ck=dcmplx(at,0.0d0)/z
      rz=ctwo/z
      p1=czero
      p2=cone
      ack=(at+1.0d0)/az
      rho=ack+sqrt(ack*ack-1.0d0)
      rho2=rho*rho
      tst=(rho2+rho2)/((rho2-1.0d0)*(rho-1.0d0))
      tst=tst*1.0d16
      ak=at
      do 20 i=1,80
      pt=p2
      p2=p1-ck*p2
      p1=pt
      ck=ck+rz
      ap=abs(p2)
      if(ap.gt.tst*ak*ak) goto 40
      ak=ak+1.0d0
   20 continue
      goto 180
   40 i=i+1
      k=0
      if(inu.ge.iaz) then
        p1=czero
        p2=cone
        at=inu+1.0d0
        ck=dcmplx(at,0.0d0)/z
        ack=at/az
        tst=sqrt(ack*1.0d16)
        itime=1
        do 60 k=1,80
        pt=p2
        p2=p1-ck*p2
        p1=pt
        ck=ck+rz
        ap=abs(p2)
        if(ap.ge.tst) then
          if(itime.eq.2) goto 80
          ack=abs(ck)
          flam=ack+sqrt(ack*ack-1.0d0)
          fkap=ap/abs(p1)
          rho=min(flam,fkap)
          tst=tst*sqrt(rho/(rho*rho-1.0d0))
          itime=2
        endif
   60   continue
        goto 180
      endif
   80 k=k+1
      kk=max(i+iaz,k+inu)
      fkk=kk
      p1=czero
      p2=dcmplx(scle,0.0d0)
      fnf=fnu-ifnu
      tfnf=fnf+fnf
      idum=0
      bk=zlgam(fkk+tfnf+1.0d0)-zlgam(fkk+1.0d0)-zlgam(tfnf+1.0d0)
      bk=dexp(bk)
      sum=czero
      km=kk-inu
      do 100 i=1,km
      pt=p2
      p2=p1+dcmplx(fkk+fnf,0.0d0)*rz*p2
      p1=pt
      ak=1.0d0-tfnf/(fkk+tfnf)
      ack=bk*ak
      sum=sum+dcmplx(ack+bk,0.0d0)*p1
      bk=ack
      fkk=fkk-1.0d0
  100 continue
      y(n)=p2
      if(n.ne.1) then
        do 120 i=2,n
        pt=p2
        p2=p1+dcmplx(fkk+fnf,0.0d0)*rz*p2
        p1=pt
        ak=1.0d0-tfnf/(fkk+tfnf)
        ack=bk*ak
        sum=sum+dcmplx(ack+bk,0.0d0)*p1
        bk=ack
        fkk=fkk-1.0d0
        m=n-i+1
        y(m)=p2
  120   continue
      endif
      if(ifnu.gt.0) then
        do 140 i=1,ifnu
        pt=p2
        p2=p1+dcmplx(fkk+fnf,0.0d0)*rz*p2
        p1=pt
        ak=1.0d0-tfnf/(fkk+tfnf)
        ack=bk*ak
        sum=sum+dcmplx(ack+bk,0.0d0)*p1
        bk=ack
        fkk=fkk-1.0d0
  140   continue
      endif
      pt=z
      if(scale) pt=pt-dcmplx(x,0.0d0)
      p1=-dcmplx(fnf,0.0d0)*cdlog(rz)+pt
      idum=0
      ap=zlgam(1.0d0+fnf)
      pt=p1-dcmplx(ap,0.0d0)
      p2=p2+sum
      ap=abs(p2)
      p1=dcmplx(1.0d0/ap,0.0d0)
      ifl=1
      ck=zexp(pt,ifl)*p1
      if(ifl.gt.0) goto 200
      pt=dconjg(p2)*p1
      cnorm=ck*pt
      do 160 i=1,n
      y(i)=y(i)*cnorm
  160 continue
      return
  180 nz=-2
      return
  200 nz=-3
      return
      end


      subroutine zkr0(z,fnu,scale,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),cch,celm,ck,coef,cone,crsc,cs,cscl,csh,ctwo,cz,
     * czero,f,fmu,p,p1,p2,pt,q,rz,s1,s2,smu,st,zd,csr(3),css(3),cy(2)
      Dimension bry(3),cc(8)
      Logical scale,scaled
      Complex(8) zexp
      Real(8) zlgam
      External zlgam,zexp
      Data kmax/30/
      Data r1/2.0d0/
      Data czero,cone,ctwo/(0.0d0,0.0d0),(1.0d0,0.0d0),(2.0d0,0.0d0)/
      Data pi,rthpi,spi,hpi,fpi,tth/3.14159265358979324d0,
     * 1.25331413731550025d0,1.90985931710274403d0,
     * 1.57079632679489662d0,1.89769999331517738d0,
     * 6.66666666666666666d-01/
      Data cc(1),cc(2),cc(3),cc(4),cc(5),cc(6),cc(7),cc(8)/
     * 5.77215664901532861d-01,-4.20026350340952355d-02,
     * -4.21977345555443367d-02,7.21894324666309954d-03,
     * -2.15241674114950973d-04,-2.01348547807882387d-05,
     * 1.13302723198169588d-06,6.11609510448141582d-09/
      xx=dble(z)
      yy=dimag(z)
      caz=abs(z)
      cscl=dcmplx(1.0d16,0.0d0)
      crsc=dcmplx(1.0d-16,0.0d0)
      css(1)=cscl
      css(2)=cone
      css(3)=crsc
      csr(1)=crsc
      csr(2)=cone
      csr(3)=cscl
      bry(1)=1.0d-288
      bry(2)=1.0d0/bry(1)
      bry(3)=1.0d308
      nz=0
      iflag=0
      scaled=scale
      rz=ctwo/z
      inu=int(fnu+0.5d0)
      dnu=fnu-inu
      if(abs(dnu).ne.0.5d0) then
        dnu2=0.0d0
        if(abs(dnu).gt.1.0d-16) dnu2=dnu*dnu
        if(caz.le.r1) then
        fc=1.0d0
        smu=cdlog(rz)
        fmu=smu*dcmplx(dnu,0.0d0)
        call zcsh(fmu,csh,cch)
        if(dnu.ne.0.0d0) then
           fc=dnu*pi
           fc=fc/sin(fc)
           smu=csh*dcmplx(1.0d0/dnu,0.0d0)
        endif
        a2=1.0d0+dnu
        idum=0
        t2=dexp(-zlgam(a2))
        t1=1.0d0/(t2*fc)
        if(abs(dnu).gt.0.1d0) then
           g1 =(t1-t2)/(dnu+dnu)
        else
          ak=1.0d0
           s=cc(1)
           do 20 k=2,8
             ak=ak*dnu2
             tm=cc(k)*ak
             s=s+tm
             if(abs(tm).lt.1.0d-16) goto 40
   20        continue
   40        g1=-s
        endif
        g2=0.5d0*(t1+t2)*fc
        g1=g1*fc
        f=dcmplx(g1,0.0d0)*cch+smu*dcmplx(g2,0.0d0)
        ifl=1
        pt=zexp(fmu,ifl)
        if(ifl.gt.0) goto 320
        p=dcmplx(0.5d0/t2,0.0d0)*pt
        q=dcmplx(0.5d0/t1,0.0d0)/pt
        s1=f
        s2=p
        ak=1.0d0
        a1=1.0d0
        ck=cone
        bk=1.0d0-dnu2
        if(inu.gt.0.or.n.gt.1) then
           if(caz.ge.1.0d-16) then
             cz=z*z*dcmplx(0.25d0,0.0d0)
             t1=0.25d0*caz*caz
   60         continue
             f =(f*dcmplx(ak,0.0d0)+p+q)*dcmplx(1.0d0/bk,0.0d0)
             p=p*dcmplx(1.0d0/(ak-dnu),0.0d0)
             q=q*dcmplx(1.0d0/(ak+dnu),0.0d0)
             rk=1.0d0/ak
             ck=ck*cz*dcmplx(rk,0.0d0)
             s1=s1+ck*f
             s2=s2+ck*(p-f*dcmplx(ak,0.0d0))
             a1=a1*t1*rk
             bk=bk+ak+ak+1.0d0
             ak=ak+1.0d0
             if(a1.gt.1.0d-16) goto 60
           endif
           kflag=2
           bk=dble(smu)
           a1=fnu+1.0d0
           ak=a1*abs(bk)
           if(ak.gt.664.0d0) kflag=3
           p2=s2*css(kflag)
           s2=p2*rz
           s1=s1*css(kflag)
           if(scaled) then
c             f=dexp(z)
             ifl=1
             f=zexp(z,ifl)
             if(ifl.gt.0) goto 320
             s1=s1*f
             s2=s2*f
           endif
           goto 160
        else
           if(caz.ge.1.0d-16) then
             cz=z*z*dcmplx(0.25d0,0.0d0)
             t1=0.25d0*caz*caz
   80         continue
             f =(f*dcmplx(ak,0.0d0)+p+q)*dcmplx(1.0d0/bk,0.0d0)
             p=p*dcmplx(1.0d0/(ak-dnu),0.0d0)
             q=q*dcmplx(1.0d0/(ak+dnu),0.0d0)
             rk=1.0d0/ak
             ck=ck*cz*dcmplx(rk,0.0d0)
             s1=s1+ck*f
             a1=a1*t1*rk
             bk=bk+ak+ak+1.0d0
             ak=ak+1.0d0
             if(a1.gt.1.0d-16) goto 80
           endif
           y(1)=s1
c           if(scaled) y(1)=s1*dexp(z)
           if(scaled) then
             ifl=1
             y(1)=zexp(z,ifl)
             if(ifl.gt.0) goto 320
             y(1)=s1*y(1)
           endif
           return
        endif
        endif
      endif
      coef=dcmplx(rthpi,0.0d0)/sqrt(z)
      kflag=2
      if(.not.scaled) then
        if(xx.gt.664.0d0) then
        scaled=.true.
        iflag=1
        kflag=2
        else
        ifl=1
        pt=zexp(dcmplx(-xx,-yy),ifl)
        if(ifl.gt.0) goto 320
        pt=pt*dble(css(kflag))
        coef=coef*pt
        endif
      endif
      if(abs(dnu).ne.0.5d0) then
        ak=cos(pi*dnu)
        ak=abs(ak)
        if(ak.ne.0.0d0) then
        fhs=abs(0.25d0-dnu2)
        if(fhs.ne.0.0d0) then
           t1=52.0d0*dlog10(2.0d0)*3.321928094d0
           t1=max(t1,12.0d0)
           t1=min(t1,60.0d0)
           t2=tth*t1-6.0d0
           if(xx.ne.0.0d0) then
             t1=atan(yy/xx)
             t1=abs(t1)
           else
             t1=hpi
           endif
           if(t2.gt.caz) then
             a2=sqrt(caz)
             ak=fpi*ak/(1.0d-16*sqrt(a2))
             aa=3.0d0*t1/(1.0d0+caz)
             bb=14.7d0*t1/(28.0d0+caz)
             ak =(dlog(ak)+caz*cos(aa)/(1.0d0+0.008d0*caz))/cos(bb)
             fk=0.12125d0*ak*ak/caz+1.5d0
           else
             etest=ak/(pi*caz*1.0d-16)
             fk=1.0d0
             if(etest.ge.1.0d0) then
               fks=2.0d0
               rk=caz+caz+2.0d0
               a1=0.0d0
               a2=1.0d0
               do 100 i=1,kmax
                ak=fhs/fks
                bk=rk/(fk+1.0d0)
                tm=a2
                a2=bk*a2-ak*a1
                a1=tm
                rk=rk+2.0d0
                fks=fks+fk+fk+2.0d0
                fhs=fhs+fk+fk
                fk=fk+1.0d0
                tm=abs(a2)*fk
                if(etest.lt.tm) goto 120
  100            continue
               nz=-2
               return
  120            fk=fk+spi*t1*sqrt(t2/caz)
               fhs=abs(0.25d0-dnu2)
             endif
           endif
           k=int(fk)
           fk=k
           fks=fk*fk
           p1=czero
           p2=dcmplx(1.0d-16,0.0d0)
           cs=p2
           do 140 i=1,k
             a1=fks-fk
             a2 =(fks+fk)/(a1+fhs)
             rk=2.0d0/(fk+1.0d0)
             t1 =(fk+xx)*rk
             t2=yy*rk
             pt=p2
             p2 =(p2*dcmplx(t1,t2)-p1)*dcmplx(a2,0.0d0)
             p1=pt
             cs=cs+p2
             fks=a1-fk+1.0d0
             fk=fk-1.0d0
  140        continue
           tm=abs(cs)
           pt=dcmplx(1.0d0/tm,0.0d0)
           s1=pt*p2
           cs=dconjg(cs)*pt
           s1=coef*s1*cs
           if(inu.gt.0.or.n.gt.1) then
             tm=abs(p2)
             pt=dcmplx(1.0d0/tm,0.0d0)
             p1=pt*p1
             p2=dconjg(p2)*pt
             pt=p1*p2
             s2=s1*(cone+(dcmplx(dnu+0.5d0,0.0d0)-pt)/z)
             goto 160
           else
             zd=z
             if(iflag.eq.1) then
               goto 240
             else
               goto 260
             endif
           endif
        endif
        endif
      endif
      s1=coef
      s2=coef
  160 continue
      ck=dcmplx(dnu+1.0d0,0.0d0)*rz
      if(n.eq.1) inu=inu-1
      if(inu.gt.0) then
        inub=1
        if(iflag.eq.1) then
        elm=dexp(-700.0d0)
        celm=dcmplx(elm,0.0d0)
        ascle=bry(1)
        zd=z
        xd=xx
        yd=yy
        ic=-1
        j=2
        do 180 i=1,inu
           st=s2
           s2=ck*s2+s1
           s1=st
           ck=ck+rz
           as=abs(s2)
           alas=dlog(as)
           p2r=-xd+alas
           if(p2r.ge.(-700.0d0)) then
             p2=-zd+cdlog(s2)
             p2r=dble(p2)
             p2i=dimag(p2)
             p2m=dexp(p2r)/1.0d-16
             p1=dcmplx(p2m,0.0d0)*dcmplx(cos(p2i),sin(p2i))
             call zpt(p1,nw,ascle)
             if(nw.eq.0) then
               j=3-j
               cy(j)=p1
               if(ic.eq.(i-1)) then
                goto 200
               else
                ic=i
                goto 180
               endif
             endif
           endif
           if(alas.ge.350.0d0) then
             xd=xd-700.0d0
             s1=s1*celm
             s2=s2*celm
             zd=dcmplx(xd,yd)
           endif
  180       continue
        if(n.eq.1) s1=s2
        goto 240
  200       kflag=1
        inub=i+1
        s2=cy(j)
        j=3-j
        s1=cy(j)
        if(inub.gt.inu) then
           if(n.eq.1) s1=s2
           goto 260
        endif
        endif
        p1=csr(kflag)
        ascle=bry(kflag)
        do 220 i=inub,inu
        st=s2
        s2=ck*s2+s1
        s1=st
        ck=ck+rz
        if(kflag.lt.3) then
           p2=s2*p1
           p2r=dble(p2)
           p2i=dimag(p2)
           p2r=abs(p2r)
           p2i=abs(p2i)
           p2m=max(p2r,p2i)
           if(p2m.gt.ascle) then
             kflag=kflag+1
             ascle=bry(kflag)
             s1=s1*p1
             s2=p2
             s1=s1*css(kflag)
             s2=s2*css(kflag)
             p1=csr(kflag)
           endif
        endif
  220    continue
        if(n.eq.1) s1=s2
        goto 260
      else
        if(n.eq.1) s1=s2
        zd=z
        if(iflag.ne.1) goto 260
      endif
  240 y(1)=s1
      if(n.ne.1) y(2)=s2
      ascle=bry(1)
      call zkre0(zd,fnu,n,y,nz,rz,ascle)
      inu=n-nz
      if(inu.le.0) then
        return
      else
        kk=nz+1
        s1=y(kk)
        y(kk)=s1*csr(1)
        if(inu.eq.1) then
        return
        else
        kk=nz+2
        s2=y(kk)
        y(kk)=s2*csr(1)
        if(inu.eq.2) then
           return
        else
           t2=fnu+dble(kk-1)
           ck=dcmplx(t2,0.0d0)*rz
           kflag=1
           goto 280
        endif
        endif
      endif
  260 y(1)=s1*csr(kflag)
      if(n.eq.1) then
        return
      else
        y(2)=s2*csr(kflag)
        if(n.eq.2) then
        return
        else
        kk=2
        endif
      endif
  280 kk=kk+1
      if(kk.le.n) then
        p1=csr(kflag)
        ascle=bry(kflag)
        do 300 i=kk,n
        p2=s2
        s2=ck*s2+s1
        s1=p2
        ck=ck+rz
        p2=s2*p1
        y(i)=p2
        if(kflag.lt.3) then
           p2r=dble(p2)
           p2i=dimag(p2)
           p2r=abs(p2r)
           p2i=abs(p2i)
           p2m=max(p2r,p2i)
           if(p2m.gt.ascle) then
             kflag=kflag+1
             ascle=bry(kflag)
             s1=s1*p1
             s2=p2
             s1=s1*css(kflag)
             s2=s2*css(kflag)
             p1=csr(kflag)
           endif
        endif
  300    continue
      endif
      return
  320 nz=-3
      end


      subroutine zirua0(z,fnu,scale,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),ak1,ck,cone,cs1,cs2,cz,czero,dk,ez,p1,rz,s2
      Logical scale,scaled
      Complex(8) zexp
      External zexp
      Data pi,rtpi/3.14159265358979324d0,0.159154943091895336d0/
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      nz=0
      az=abs(z)
      x=dble(z)
      arm=1.0d-304
      rtr1=sqrt(arm)
      il=min(2,n)
      dfnu=fnu+dble(n-il)
      ak1=dcmplx(rtpi,0.0d0)/z
      ak1=sqrt(ak1)
      cz=z
      if(scale) cz=z-dcmplx(x,0.0d0)
      acz=dble(cz)
      if(abs(acz).gt.700.0d0) then
        nz=-1
      else
        dnu2=dfnu+dfnu
        scaled=.true.
        if((abs(acz).le.664.0d0) .or.(n.le.2)) then
        scaled=.false.
        ier1=1
        ak1=ak1*zexp(cz,ier1)
        if(ier1.gt.0) goto 140
        endif
        fdn=0.0d0
        if(dnu2.gt.rtr1) fdn=dnu2*dnu2
        ez=z*dcmplx(8.0d0,0.0d0)
        aez=8.0d0*az
        s=1.0d-16/aez
        yy=dimag(z)
        p1=czero
        if(yy.ne.0.0d0) then
        inu=int(fnu)
        arg =(fnu-inu)*pi
        inu=inu+n-il
        ak=-sin(arg)
        bk=cos(arg)
        if(yy.lt.0.0d0) bk=-bk
        p1=dcmplx(ak,bk)
        if(mod(inu,2).eq.1) p1=-p1
        endif
        do 60 k=1,il
        sqk=fdn-1.0d0
        atol=s*abs(sqk)
        sgn=1.0d0
        cs1=cone
        cs2=cone
        ck=cone
        ak=0.0d0
        aa=1.0d0
        bb=aez
        dk=ez
        do 20 j=1,46
           ck=ck*dcmplx(sqk,0.0d0)/dk
           cs2=cs2+ck
           sgn=-sgn
           cs1=cs1+ck*dcmplx(sgn,0.0d0)
           dk=dk+ez
           aa=aa*abs(sqk)/bb
           bb=bb+aez
           ak=ak+8.0d0
           sqk=sqk-ak
           if(aa.le.atol) goto 40
   20       continue
        goto 120
   40       s2=cs1
        if(x+x.lt.700.0d0) then
           ier1=1
           s2=s2+p1*cs2*zexp(-z-z,ier1)
           if(ier1.gt.0) goto 140
        endif
        fdn=fdn+8.0d0*dfnu+4.0d0
        p1=-p1
        m=n-il+k
        y(m)=s2*ak1
   60    continue
        if(n.gt.2) then
        nn=n
        k=nn-2
        ak=k
        rz =(cone+cone)/z
        ib=3
        do 80 i=ib,nn
           y(k)=dcmplx(ak+fnu,0.0d0)*rz*y(k+1)+y(k+2)
           ak=ak-1.0d0
           k=k-1
   80       continue
        if(scaled) then
           ier1=1
           ck=zexp(cz,ier1)
           if(ier1.gt.0) goto 140
           do 100 i=1,nn
             y(i)=y(i)*ck
  100        continue
        endif
        endif
        return
  120    nz=-2
        return
  140    nz=-3
      endif
      end


      subroutine zkrua(z,fnu,scale,mr,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n)
      Logical scale
      nz=0
      xx=dble(z)
      yy=dimag(z)
      ax=abs(xx)*1.7321d0
      ay=abs(yy)
      if(ay.gt.ax) then
        call zkrua1(z,fnu,scale,mr,n,y,nz)
      else
        call zkrua2(z,fnu,scale,mr,n,y,nz)
      endif
      end


      subroutine zkrc(z,fnu,scale,mr,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),c1,c2,ck,cone,cs,cscl,cscr,csgn,cspn,rz,s1,s2,
     * sc1,sc2,st,zn,csr(3),css(3),cy(2)
      Dimension bry(3)
      Logical scale
      Data pi/3.14159265358979324d0/
      Data cone/(1.0d0,0.0d0)/
      nz=0
      zn=-z
      nn=n
      call zir0(zn,fnu,scale,nn,y,nw)
      if(nw.ge.0) then
        nn=min(2,n)
        call zkr0(zn,fnu,scale,nn,cy,nw)
        if(nw.eq.0) then
        s1=cy(1)
        fmr=mr
        sgn=-sign(pi,fmr)
        csgn=dcmplx(0.0d0,sgn)
        if(scale) then
           yy=-dimag(zn)
           cpn=cos(yy)
           spn=sin(yy)
           csgn=csgn*dcmplx(cpn,spn)
        endif
        inu=int(fnu)
        arg =(fnu-inu)*sgn
        cpn=cos(arg)
        spn=sin(arg)
        cspn=dcmplx(cpn,spn)
        if(mod(inu,2).eq.1) cspn=-cspn
        iuf=0
        c1=s1
        c2=y(1)
        ascle=1.0d-288
        if(scale) then
           call zirt(zn,c1,c2,nw,ascle,iuf)
           nz=nz+nw
           sc1=c1
        endif
        y(1)=cspn*c1+csgn*c2
        if(n.ne.1) then
           cspn=-cspn
           s2=cy(2)
           c1=s2
           c2=y(2)
           if(scale) then
             call zirt(zn,c1,c2,nw,ascle,iuf)
             nz=nz+nw
             sc2=c1
           endif
           y(2)=cspn*c1+csgn*c2
           if(n.ne.2) then
             cspn=-cspn
             rz=dcmplx(2.0d0,0.0d0)/zn
             ck=dcmplx(fnu+1.0d0,0.0d0)*rz
             cscl=dcmplx(1.0d16,0.0d0)
             cscr=dcmplx(1.0d-16,0.0d0)
             css(1)=cscl
             css(2)=cone
             css(3)=cscr
             csr(1)=cscr
             csr(2)=cone
             csr(3)=cscl
             bry(1)=ascle
             bry(2)=1.0d0/ascle
             bry(3)=1.0d308
             as2=abs(s2)
             kflag=2
             if(as2.le.bry(1)) then
               kflag=1
             elseif(as2.ge.bry(2)) then
               kflag=3
             endif
             bscle=bry(kflag)
             s1=s1*css(kflag)
             s2=s2*css(kflag)
             cs=csr(kflag)
             do 20 i=3,n
               st=s2
               s2=ck*s2+s1
               s1=st
               c1=s2*cs
               st=c1
               c2=y(i)
               if(scale) then
                if(iuf.ge.0) then
                  call zirt(zn,c1,c2,nw,ascle,iuf)
                  nz=nz+nw
                  sc1=sc2
                  sc2=c1
                  if(iuf.eq.3) then
                    iuf=-4
                    s1=sc1*css(kflag)
                    s2=sc2*css(kflag)
                    st=sc2
                  endif
                endif
               endif
               y(i)=cspn*c1+csgn*c2
               ck=ck+rz
               cspn=-cspn
               if(kflag.lt.3) then
                c1r=dble(c1)
                c1i=dimag(c1)
                c1r=abs(c1r)
                c1i=abs(c1i)
                c1m=max(c1r,c1i)
                if(c1m.gt.bscle) then
                  kflag=kflag+1
                  bscle=bry(kflag)
                  s1=s1*cs
                  s2=st
                  s1=s1*css(kflag)
                  s2=s2*css(kflag)
                  cs=csr(kflag)
                endif
               endif
   20         continue
           endif
        endif
        return
        endif
      endif
      nz=-1
      if(nw.eq.(-2)) nz=-2
      if(nw.eq.(-3)) nz=-3
      return
      end

      subroutine zirt(zr,s1,s2,nz,ascle,iuf)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) s1,s2,zr,c1,czero,s1d,zexp
      External zexp
      Data czero/(0.0d0,0.0d0)/
      nz=0
      as1=abs(s1)
      as2=abs(s2)
      aa=dble(s1)
      aln=dimag(s1)
      if(aa.ne.0.0d0.or.aln.ne.0.0d0) then
        if(as1.ne.0.0d0) then
        xx=dble(zr)
        aln=-xx-xx+dlog(as1)
        s1d=s1
        s1=czero
        as1=0.0d0
        if(aln.ge.(-664.0d0)) then
           c1=cdlog(s1d)-zr-zr
c           s1=dexp(c1)
           if1=1
           s1=zexp(c1,if1)
           as1=abs(s1)
           iuf=iuf+1
        endif
        endif
      endif
      aa=max(as1,as2)
      if(aa.le.ascle) then
        s1=czero
        s2=czero
        nz=1
        iuf=0
      endif
      return
      end


      subroutine zcsh(z,csh,cch)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) cch,csh,z
      x=dble(z)
      y=dimag(z)
      sh=sinh(x)
      ch=cosh(x)
      sn=sin(y)
      cn=cos(y)
      cshr=sh*cn
      cshi=ch*sn
      csh=dcmplx(cshr,cshi)
      cchr=ch*cn
      cchi=sh*sn
      cch=dcmplx(cchr,cchi)
      end


      subroutine zpt(y,nz,ascle)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) y
      nz=0
      yr=dble(y)
      yi=dimag(y)
      yr=abs(yr)
      yi=abs(yi)
      st=min(yr,yi)
      if(st.le.ascle) then
        ss=max(yr,yi)
        st=st/1.0d-16
        if(ss.lt.st) nz=1
      endif
      end


      subroutine zkre0(zr,fnu,n,y,nz,rz,ascle)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) rz,zr,y(n),celm,ck,cs,czero,s1,s2,zd,cy(2)
      Data czero/(0.0d0,0.0d0)/
      nz=0
      ic=0
      xx=dble(zr)
      nn=min(2,n)
      do 20 i=1,nn
        s1=y(i)
        cy(i)=s1
        as=abs(s1)
        acs=-xx+dlog(as)
        nz=nz+1
        y(i)=czero
        if(acs.ge.(-700.0d0)) then
        cs=-zr+cdlog(s1)
        csr=dble(cs)
        csi=dimag(cs)
        aa=dexp(csr)/1.0d-16
        cs=dcmplx(aa,0.0d0)*dcmplx(cos(csi),sin(csi))
        call zpt(cs,nw,ascle)
        if(nw.eq.0) then
           y(i)=cs
           nz=nz-1
           ic=i
        endif
        endif
   20 continue
      if(n.ne.1) then
        if(ic.le.1) then
        y(1)=czero
        nz=2
        endif
        if(n.ne.2) then
        if(nz.ne.0) then
           fn=fnu+1.0d0
           ck=dcmplx(fn,0.0d0)*rz
           s1=cy(1)
           s2=cy(2)
           elm=dexp(-700.0d0)
           celm=dcmplx(elm,0.0d0)
           zri=dimag(zr)
           zd=zr
           do 40 i=3,n
             kk=i
             cs=s2
             s2=ck*s2+s1
             s1=cs
             ck=ck+rz
             as=abs(s2)
             alas=dlog(as)
             acs=-xx+alas
             nz=nz+1
             y(i)=czero
             if(acs.ge.(-700.0d0)) then
               cs=-zd+cdlog(s2)
               csr=dble(cs)
               csi=dimag(cs)
               aa=dexp(csr)/1.0d-16
               cs=dcmplx(aa,0.0d0)*dcmplx(cos(csi),sin(csi))
               call zpt(cs,nw,ascle)
               if(nw.eq.0) then
                y(i)=cs
                nz=nz-1
                if(ic.eq.(kk-1)) then
                  goto 60
                else
                  ic=kk
                  goto 40
                endif
               endif
             endif
             if(alas.ge.350.0d0) then
               xx=xx-700.0d0
               s1=s1*celm
               s2=s2*celm
               zd=dcmplx(xx,zri)
             endif
   40        continue
           nz=n
           if(ic.eq.n) nz=n-1
           goto 80
   60        nz=kk-2
   80        do 100 k=1,nz
             y(k)=czero
  100        continue
        endif
        endif
      endif
      return
      end


      subroutine zirbr(z,fnu,n,cy)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,cy(n),cdfnu,cone,czero,p1,p2,pt,rz,t1
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      az=abs(z)
      inu=int(fnu)
      idnu=inu+n-1
      fdnu=idnu
      magz=int(az)
      amagz=magz+1
      fnup=max(amagz,fdnu)
      id=idnu-magz-1
      itime=1
      k=1
      rz =(cone+cone)/z
      t1=dcmplx(fnup,0.0d0)*rz
      p2=-t1
      p1=cone
      t1=t1+rz
      if(id.gt.0) id=0
      ap2=abs(p2)
      ap1=abs(p1)
      arg =(ap2+ap2)/(ap1*1.0d-16)
      test1=sqrt(arg)
      test=test1
      rap1=1.0d0/ap1
      p1=p1*dcmplx(rap1,0.0d0)
      p2=p2*dcmplx(rap1,0.0d0)
      ap2=ap2*rap1
   20 continue
      k=k+1
      ap1=ap2
      pt=p2
      p2=p1-t1*p2
      p1=pt
      t1=t1+rz
      ap2=abs(p2)
      if(ap1.le.test) then
        goto 20
      elseif(itime.ne.2) then
        ak=abs(t1)*0.5d0
        flam=ak+sqrt(ak*ak-1.0d0)
        rho=min(ap2/ap1,flam)
        test=test1*sqrt(rho/(rho*rho-1.0d0))
        itime=2
        goto 20
      endif
      kk=k+1-id
      ak=kk
      dfnu=fnu+dble(n-1)
      cdfnu=dcmplx(dfnu,0.0d0)
      t1=dcmplx(ak,0.0d0)
      p1=dcmplx(1.0d0/ap2,0.0d0)
      p2=czero
      do 40 i=1,kk
        pt=p1
        p1=rz*(cdfnu+t1)*p1+p2
        p2=pt
        t1=t1-cone
   40 continue
      if(dble(p1).eq.0.0d0.and.dimag(p1).eq.0.0d0) p1=dcmplx(1.0d-16,
     *    1.0d-16)
      cy(n)=p2/p1
      if(n.ne.1) then
        k=n-1
        ak=k
        t1=dcmplx(ak,0.0d0)
        cdfnu=dcmplx(fnu,0.0d0)*rz
        do 60 i=2,n
        pt=cdfnu+t1*rz+cy(k+1)
        if(dble(pt).eq.0.0d0.and.dimag(pt).eq.0.0d0)
     *        pt=dcmplx(1.0d-16,1.0d-16)
        cy(k)=cone/pt
        t1=t1-cone
        k=k-1
   60    continue
      endif
      return
      end

      subroutine zjhrua(z,fnu,ipmtr,phi,arg,zeta1,zeta2,asum,bsum)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) arg,asum,bsum,phi,z,zeta1,zeta2,cfnu,cone,czero,przth,
     * ptfn,rfn13,rtzta,rzth,suma,sumb,t2,tfn,w,w2,za,zb,zc,zeta,zth,
     * cr(14),dr(14),p(30),up(14)
      Dimension alfa(180),ap(30),ar(14),beta(210),br(14),c(105),
     * gama(30)
      Data ar(1),ar(2),ar(3),ar(4),ar(5),ar(6),ar(7),ar(8),ar(9),
     * ar(10),ar(11),ar(12),ar(13),ar(14)/1.00000000000000000d+00,
     * 1.04166666666666667d-01,8.35503472222222222d-02,
     * 1.28226574556327160d-01,2.91849026464140464d-01,
     * 8.81627267443757652d-01,3.32140828186276754d+00,
     * 1.49957629868625547d+01,7.89230130115865181d+01,
     * 4.74451538868264323d+02,3.20749009089066193d+03,
     * 2.40865496408740049d+04,1.98923119169509794d+05,
     * 1.79190200777534383d+06/
      Data br(1),br(2),br(3),br(4),br(5),br(6),br(7),br(8),br(9),
     * br(10),br(11),br(12),br(13),br(14)/1.00000000000000000d+00,
     * -1.45833333333333333d-01,
     * -9.87413194444444444d-02,
     * -1.43312053915895062d-01,
     * -3.17227202678413548d-01,
     * -9.42429147957120249d-01,
     * -3.51120304082635426d+00,
     * -1.57272636203680451d+01,
     * -8.22814390971859444d+01,
     * -4.92355370523670524d+02,
     * -3.31621856854797251d+03,
     * -2.48276742452085896d+04,
     * -2.04526587315129788d+05,
     * -1.83844491706820990d+06/
      Data c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10),c(11),
     * c(12),c(13),c(14),c(15),c(16)/1.00000000000000000d+00,
     * -2.08333333333333333d-01,
     * 1.25000000000000000d-01,
     * 3.34201388888888889d-01,
     * -4.01041666666666667d-01,
     * 7.03125000000000000d-02,
     * -1.02581259645061728d+00,
     * 1.84646267361111111d+00,
     * -8.91210937500000000d-01,
     * 7.32421875000000000d-02,
     * 4.66958442342624743d+00,
     * -1.12070026162229938d+01,
     * 8.78912353515625000d+00,
     * -2.36408691406250000d+00,
     * 1.12152099609375000d-01,
     * -2.82120725582002449d+01/
      Data c(17),c(18),c(19),c(20),c(21),c(22),c(23),c(24)/
     * 8.46362176746007346d+01,
     * -9.18182415432400174d+01,
     * 4.25349987453884549d+01,
     * -7.36879435947963170d+00,
     * 2.27108001708984375d-01,
     * 2.12570130039217123d+02,
     * -7.65252468141181642d+02,
     * 1.05999045252799988d+03/
      Data c(25),c(26),c(27),c(28),c(29),c(30),c(31),c(32),c(33),c(34),
     * c(35),c(36),c(37),c(38),c(39),c(40)/-6.99579627376132541d+02,
     * 2.18190511744211590d+02,
     * -2.64914304869515555d+01,
     * 5.72501420974731445d-01,
     * -1.91945766231840700d+03,
     * 8.06172218173730938d+03,
     * -1.35865500064341374d+04,
     * 1.16553933368645332d+04,
     * -5.30564697861340311d+03,
     * 1.20090291321635246d+03,
     * -1.08090919788394656d+02,
     * 1.72772750258445740d+00,
     * 2.02042913309661486d+04,
     * -9.69805983886375135d+04,
     * 1.92547001232531532d+05,
     * -2.03400177280415534d+05/
      Data c(41),c(42),c(43),c(44),c(45),c(46),c(47),c(48)/
     * 1.22200464983017460d+05,
     * -4.11926549688975513d+04,
     * 7.10951430248936372d+03,
     * -4.93915304773088012d+02,
     * 6.07404200127348304d+00,
     * -2.42919187900551333d+05,
     * 1.31176361466297720d+06,
     * -2.99801591853810675d+06/
      Data c(49),c(50),c(51),c(52),c(53),c(54),c(55),c(56),c(57),c(58),
     * c(59),c(60),c(61),c(62),c(63),c(64)/3.76327129765640400d+06,
     * -2.81356322658653411d+06,
     * 1.26836527332162478d+06,
     * -3.31645172484563578d+05,
     * 4.52187689813627263d+04,
     * -2.49983048181120962d+03,
     * 2.43805296995560639d+01,
     * 3.28446985307203782d+06,
     * -1.97068191184322269d+07,
     * 5.09526024926646422d+07,
     * -7.41051482115326577d+07,
     * 6.63445122747290267d+07,
     * -3.75671766607633513d+07,
     * 1.32887671664218183d+07,
     * -2.78561812808645469d+06,
     * 3.08186404612662398d+05/
      Data c(65),c(66),c(67),c(68),c(69),c(70),c(71),c(72)/
     * -1.38860897537170405d+04,
     * 1.10017140269246738d+02,
     * -4.93292536645099620d+07,
     * 3.25573074185765749d+08,
     * -9.39462359681578403d+08,
     * 1.55359689957058006d+09,
     * -1.62108055210833708d+09,
     * 1.10684281682301447d+09/
      Data c(73),c(74),c(75),c(76),c(77),c(78),c(79),c(80),c(81),c(82),
     * c(83),c(84),c(85),c(86),c(87),c(88)/-4.95889784275030309d+08,
     * 1.42062907797533095d+08,
     * -2.44740627257387285d+07,
     * 2.24376817792244943d+06,
     * -8.40054336030240853d+04,
     * 5.51335896122020586d+02,
     * 8.14789096118312115d+08,
     * -5.86648149205184723d+09,
     * 1.86882075092958249d+10,
     * -3.46320433881587779d+10,
     * 4.12801855797539740d+10,
     * -3.30265997498007231d+10,
     * 1.79542137311556001d+10,
     * -6.56329379261928433d+09,
     * 1.55927986487925751d+09,
     * -2.25105661889415278d+08/
      Data c(89),c(90),c(91),c(92),c(93),c(94),c(95),c(96)/
     * 1.73951075539781645d+07,
     * -5.49842327572288687d+05,
     * 3.03809051092238427d+03,
     * -1.46792612476956167d+10,
     * 1.14498237732025810d+11,
     * -3.99096175224466498d+11,
     * 8.19218669548577329d+11,
     * -1.09837515608122331d+12/
      Data c(97),c(98),c(99),c(100),c(101),c(102),c(103),c(104),c(105)/
     * 1.00815810686538209d+12,
     * -6.45364869245376503d+11,
     * 2.87900649906150589d+11,
     * -8.78670721780232657d+10,
     * 1.76347306068349694d+10,
     * -2.16716498322379509d+09,
     * 1.43157876718888981d+08,
     * -3.87183344257261262d+06,
     * 1.82577554742931747d+04/
      Data alfa(1),alfa(2),alfa(3),alfa(4),alfa(5),alfa(6),alfa(7),
     * alfa(8),alfa(9),alfa(10),alfa(11),alfa(12),alfa(13),alfa(14)/
     * -4.44444444444444444d-03,
     * -9.22077922077922078d-04,
     * -8.84892884892884893d-05,
     * 1.65927687832449737d-04,
     * 2.46691372741792910d-04,
     * 2.65995589346254780d-04,
     * 2.61824297061500945d-04,
     * 2.48730437344655609d-04,
     * 2.32721040083232098d-04,
     * 2.16362485712365082d-04,
     * 2.00738858762752355d-04,
     * 1.86267636637545172d-04,
     * 1.73060775917876493d-04,
     * 1.61091705929015752d-04/
      Data alfa(15),alfa(16),alfa(17),alfa(18),alfa(19),alfa(20),
     * alfa(21),alfa(22)/1.50274774160908134d-04,
     * 1.40503497391269794d-04,
     * 1.31668816545922806d-04,
     * 1.23667445598253261d-04,
     * 1.16405271474737902d-04,
     * 1.09798298372713369d-04,
     * 1.03772410422992823d-04,
     * 9.82626078369363448d-05/
      Data alfa(23),alfa(24),alfa(25),alfa(26),alfa(27),alfa(28),
     * alfa(29),alfa(30),alfa(31),alfa(32),alfa(33),alfa(34),
     * alfa(35),alfa(36)/9.32120517249503256d-05,
     * 8.85710852478711718d-05,
     * 8.42963105715700223d-05,
     * 8.03497548407791151d-05,
     * 7.66981345359207388d-05,
     * 7.33122157481777809d-05,
     * 7.01662625163141333d-05,
     * 6.72375633790160292d-05,
     * 6.93735541354588974d-04,
     * 2.32241745182921654d-04,
     * -1.41986273556691197d-05,
     * -1.16444931672048640d-04,
     * -1.50803558053048762d-04,
     * -1.55121924918096223d-04/
      Data alfa(37),alfa(38),alfa(39),alfa(40),alfa(41),alfa(42),
     * alfa(43),alfa(44)/-1.46809756646465549d-04,
     * -1.33815503867491367d-04,
     * -1.19744975684254051d-04,
     * -1.06184319207974020d-04,
     * -9.37699549891194492d-05,
     * -8.26923045588193274d-05,
     * -7.29374348155221211d-05,
     * -6.44042357721016283d-05/
      Data alfa(45),alfa(46),alfa(47),alfa(48),alfa(49),alfa(50),
     * alfa(51),alfa(52),alfa(53),alfa(54),alfa(55),alfa(56),
     * alfa(57),alfa(58)/-5.69611566009369048d-05,
     * -5.04731044303561628d-05,
     * -4.48134868008882786d-05,
     * -3.98688727717598864d-05,
     * -3.55400532972042498d-05,
     * -3.17414256609022480d-05,
     * -2.83996793904174811d-05,
     * -2.54522720634870566d-05,
     * -2.28459297164724555d-05,
     * -2.05352753106480604d-05,
     * -1.84816217627666085d-05,
     * -1.66519330021393806d-05,
     * -1.50179412980119482d-05,
     * -1.35554031379040526d-05/
      Data alfa(59),alfa(60),alfa(61),alfa(62),alfa(63),alfa(64),
     * alfa(65),alfa(66)/-1.22434746473858131d-05,
     * -1.10641884811308169d-05,
     * -3.54211971457743841d-04,
     * -1.56161263945159416d-04,
     * 3.04465503594936410d-05,
     * 1.30198655773242693d-04,
     * 1.67471106699712269d-04,
     * 1.70222587683592569d-04/
      Data alfa(67),alfa(68),alfa(69),alfa(70),alfa(71),alfa(72),
     * alfa(73),alfa(74),alfa(75),alfa(76),alfa(77),alfa(78),
     * alfa(79),alfa(80)/1.56501427608594704d-04,
     * 1.36339170977445120d-04,
     * 1.14886692029825128d-04,
     * 9.45869093034688111d-05,
     * 7.64498419250898258d-05,
     * 6.07570334965197354d-05,
     * 4.74394299290508799d-05,
     * 3.62757512005344297d-05,
     * 2.69939714979224901d-05,
     * 1.93210938247939253d-05,
     * 1.30056674793963203d-05,
     * 7.82620866744496661d-06,
     * 3.59257485819351583d-06,
     * 1.44040049814251817d-07/
      Data alfa(81),alfa(82),alfa(83),alfa(84),alfa(85),alfa(86),
     * alfa(87),alfa(88)/-2.65396769697939116d-06,
     * -4.91346867098485910d-06,
     * -6.72739296091248287d-06,
     * -8.17269379678657923d-06,
     * -9.31304715093561232d-06,
     * -1.02011418798016441d-05,
     * -1.08805962510592880d-05,
     * -1.13875481509603555d-05/
      Data alfa(89),alfa(90),alfa(91),alfa(92),alfa(93),alfa(94),
     * alfa(95),alfa(96),alfa(97),alfa(98),alfa(99),alfa(100),
     * alfa(101),alfa(102)/-1.17519675674556414d-05,
     * -1.19987364870944141d-05,
     * 3.78194199201772914d-04,
     * 2.02471952761816167d-04,
     * -6.37938506318862408d-05,
     * -2.38598230603005903d-04,
     * -3.10916256027361568d-04,
     * -3.13680115247576316d-04,
     * -2.78950273791323387d-04,
     * -2.28564082619141374d-04,
     * -1.75245280340846749d-04,
     * -1.25544063060690348d-04,
     * -8.22982872820208365d-05,
     * -4.62860730588116458d-05/
      Data alfa(103),alfa(104),alfa(105),alfa(106),alfa(107),alfa(108),
     * alfa(109),alfa(110)/-1.72334302366962267d-05,
     * 5.60690482304602267d-06,
     * 2.31395443148286800d-05,
     * 3.62642745856793957d-05,
     * 4.58006124490188752d-05,
     * 5.24595294959114050d-05,
     * 5.68396208545815266d-05,
     * 5.94349820393104052d-05/
      Data alfa(111),alfa(112),alfa(113),alfa(114),alfa(115),alfa(116),
     * alfa(117),alfa(118),alfa(119),alfa(120),alfa(121),alfa(122)/
     * 6.06478527578421742d-05,
     * 6.08023907788436497d-05,
     * 6.01577894539460388d-05,
     * 5.89199657344698500d-05,
     * 5.72515823777593053d-05,
     * 5.52804375585852577d-05,
     * 5.31063773802880170d-05,
     * 5.08069302012325706d-05,
     * 4.84418647620094842d-05,
     * 4.60568581607475370d-05,
     * -6.91141397288294174d-04,
     * -4.29976633058871912d-04/
      Data alfa(123),alfa(124),alfa(125),alfa(126),alfa(127),alfa(128),
     * alfa(129),alfa(130)/1.83067735980039018d-04,
     * 6.60088147542014144d-04,
     * 8.75964969951185931d-04,
     * 8.77335235958235514d-04,
     * 7.49369585378990637d-04,
     * 5.63832329756980918d-04,
     * 3.68059319971443156d-04,
     * 1.88464535514455599d-04/
      Data alfa(131),alfa(132),alfa(133),alfa(134),alfa(135),alfa(136),
     * alfa(137),alfa(138),alfa(139),alfa(140),alfa(141),
     * alfa(142)/3.70663057664904149d-05,
     * -8.28520220232137023d-05,
     * -1.72751952869172998d-04,
     * -2.36314873605872983d-04,
     * -2.77966150694906658d-04,
     * -3.02079514155456919d-04,
     * -3.12594712643820127d-04,
     * -3.12872558758067163d-04,
     * -3.05678038466324377d-04,
     * -2.93226470614557331d-04,
     * -2.77255655582934777d-04,
     * -2.59103928467031709d-04/
      Data alfa(143),alfa(144),alfa(145),alfa(146),alfa(147),alfa(148),
     * alfa(149),alfa(150)/-2.39784014396480342d-04,
     * -2.20048260045422848d-04,
     * -2.00443911094971498d-04,
     * -1.81358692210970687d-04,
     * -1.63057674478657464d-04,
     * -1.45712672175205844d-04,
     * -1.29425421983924587d-04,
     * -1.14245691942445952d-04/
      Data alfa(151),alfa(152),alfa(153),alfa(154),alfa(155),alfa(156),
     * alfa(157),alfa(158),alfa(159),alfa(160),alfa(161),alfa(162)/
     * 1.92821964248775885d-03,
     * 1.35592576302022234d-03,
     * -7.17858090421302995d-04,
     * -2.58084802575270346d-03,
     * -3.49271130826168475d-03,
     * -3.46986299340960628d-03,
     * -2.82285233351310182d-03,
     * -1.88103076404891354d-03,
     * -8.89531718383947600d-04,
     * 3.87912102631035228d-06,
     * 7.28688540119691412d-04,
     * 1.26566373053457758d-03/
      Data alfa(163),alfa(164),alfa(165),alfa(166),alfa(167),alfa(168),
     * alfa(169),alfa(170)/1.62518158372674427d-03,
     * 1.83203153216373172d-03,
     * 1.91588388990527909d-03,
     * 1.90588846755546138d-03,
     * 1.82798982421825727d-03,
     * 1.70389506421121530d-03,
     * 1.55097127171097686d-03,
     * 1.38261421852276159d-03/
      Data alfa(171),alfa(172),alfa(173),alfa(174),alfa(175),alfa(176),
     * alfa(177),alfa(178),alfa(179),alfa(180)/1.20881424230064774d-03,
     * 1.03676532638344962d-03,
     * 8.71437918068619115d-04,
     * 7.16080155297701002d-04,
     * 5.72637002558129372d-04,
     * 4.42089819465802277d-04,
     * 3.24724948503090564d-04,
     * 2.20342042730246599d-04,
     * 1.28412898401353882d-04,
     * 4.82005924552095464d-05/
      Data beta(1),beta(2),beta(3),beta(4),beta(5),
     * beta(6),beta(7),beta(8),beta(9),beta(10),
     * beta(11),beta(12),beta(13),
     * beta(14)/1.79988721413553309d-02,
     * 5.59964911064388073d-03,
     * 2.88501402231132779d-03,
     * 1.80096606761053941d-03,
     * 1.24753110589199202d-03,
     * 9.22878876572938311d-04,
     * 7.14430421727287357d-04,
     * 5.71787281789704872d-04,
     * 4.69431007606481533d-04,
     * 3.93232835462916638d-04,
     * 3.34818889318297664d-04,
     * 2.88952148495751517d-04,
     * 2.52211615549573284d-04,
     * 2.22280580798883327d-04/
      Data beta(15),beta(16),beta(17),beta(18),
     * beta(19),beta(20),beta(21),
     * beta(22)/1.97541838033062524d-04,
     * 1.76836855019718004d-04,
     * 1.59316899661821081d-04,
     * 1.44347930197333986d-04,
     * 1.31448068119965379d-04,
     * 1.20245444949302884d-04,
     * 1.10449144504599392d-04,
     * 1.01828770740567258d-04/
      Data beta(23),beta(24),beta(25),beta(26),
     * beta(27),beta(28),beta(29),beta(30),
     * beta(31),beta(32),beta(33),beta(34),
     * beta(35),beta(36)/9.41998224204237509d-05,
     * 8.74130545753834437d-05,
     * 8.13466262162801467d-05,
     * 7.59002269646219339d-05,
     * 7.09906300634153481d-05,
     * 6.65482874842468183d-05,
     * 6.25146958969275078d-05,
     * 5.88403394426251749d-05,
     * -1.49282953213429172d-03,
     * -8.78204709546389328d-04,
     * -5.02916549572034614d-04,
     * -2.94822138512746025d-04,
     * -1.75463996970782828d-04,
     * -1.04008550460816434d-04/
      Data beta(37),beta(38),beta(39),beta(40),
     * beta(41),beta(42),beta(43),
     * beta(44)/-5.96141953046457895d-05,
     * -3.12038929076098340d-05,
     * -1.26089735980230047d-05,
     * -2.42892608575730389d-07,
     * 8.05996165414273571d-06,
     * 1.36507009262147391d-05,
     * 1.73964125472926261d-05,
     * 1.98672978842133780d-05/
      Data beta(45),beta(46),beta(47),beta(48),
     * beta(49),beta(50),beta(51),beta(52),
     * beta(53),beta(54),beta(55),beta(56),
     * beta(57),beta(58)/2.14463263790822639d-05,
     * 2.23954659232456514d-05,
     * 2.28967783814712629d-05,
     * 2.30785389811177817d-05,
     * 2.30321976080909144d-05,
     * 2.28236073720348722d-05,
     * 2.25005881105292418d-05,
     * 2.20981015361991429d-05,
     * 2.16418427448103905d-05,
     * 2.11507649256220843d-05,
     * 2.06388749782170737d-05,
     * 2.01165241997081666d-05,
     * 1.95913450141179244d-05,
     * 1.90689367910436740d-05/
      Data beta(59),beta(60),beta(61),beta(62),
     * beta(63),beta(64),beta(65),
     * beta(66)/1.85533719641636667d-05,
     * 1.80475722259674218d-05,
     * 5.52213076721292790d-04,
     * 4.47932581552384646d-04,
     * 2.79520653992020589d-04,
     * 1.52468156198446602d-04,
     * 6.93271105657043598d-05,
     * 1.76258683069991397d-05/
      Data beta(67),beta(68),beta(69),beta(70),
     * beta(71),beta(72),beta(73),beta(74),
     * beta(75),beta(76),beta(77),beta(78),
     * beta(79),beta(80)/-1.35744996343269136d-05,
     * -3.17972413350427135d-05,
     * -4.18861861696693365d-05,
     * -4.69004889379141029d-05,
     * -4.87665447413787352d-05,
     * -4.87010031186735069d-05,
     * -4.74755620890086638d-05,
     * -4.55813058138628452d-05,
     * -4.33309644511266036d-05,
     * -4.09230193157750364d-05,
     * -3.84822638603221274d-05,
     * -3.60857167535410501d-05,
     * -3.37793306123367417d-05,
     * -3.15888560772109621d-05/
      Data beta(81),beta(82),beta(83),beta(84),
     * beta(85),beta(86),beta(87),
     * beta(88)/-2.95269561750807315d-05,
     * -2.75978914828335759d-05,
     * -2.58006174666883713d-05,
     * -2.41308356761280200d-05,
     * -2.25823509518346033d-05,
     * -2.11479656768912971d-05,
     * -1.98200638885294927d-05,
     * -1.85909870801065077d-05/
      Data beta(89),beta(90),beta(91),beta(92),
     * beta(93),beta(94),beta(95),beta(96),
     * beta(97),beta(98),beta(99),beta(100),
     * beta(101),beta(102)/-1.74532699844210224d-05,
     * -1.63997823854497997d-05,
     * -4.74617796559959808d-04,
     * -4.77864567147321487d-04,
     * -3.20390228067037603d-04,
     * -1.61105016119962282d-04,
     * -4.25778101285435204d-05,
     * 3.44571294294967503d-05,
     * 7.97092684075674924d-05,
     * 1.03138236708272200d-04,
     * 1.12466775262204158d-04,
     * 1.13103642108481389d-04,
     * 1.08651634848774268d-04,
     * 1.01437951597661973d-04/
      Data beta(103),beta(104),beta(105),beta(106),
     * beta(107),beta(108),beta(109),
     * beta(110)/9.29298396593363896d-05,
     * 8.40293133016089978d-05,
     * 7.52727991349134062d-05,
     * 6.69632521975730872d-05,
     * 5.92564547323194704d-05,
     * 5.22169308826975567d-05,
     * 4.58539485165360646d-05,
     * 4.01445513891486808d-05/
      Data beta(111),beta(112),beta(113),beta(114),
     * beta(115),beta(116),beta(117),beta(118),
     * beta(119),beta(120),beta(121),
     * beta(122)/3.50481730031328081d-05,
     * 3.05157995034346659d-05,
     * 2.64956119950516039d-05,
     * 2.29363633690998152d-05,
     * 1.97893056664021636d-05,
     * 1.70091984636412623d-05,
     * 1.45547428261524004d-05,
     * 1.23886640995878413d-05,
     * 1.04775876076583236d-05,
     * 8.79179954978479373d-06,
     * 7.36465810572578444d-04,
     * 8.72790805146193976d-04/
      Data beta(123),beta(124),beta(125),beta(126),
     * beta(127),beta(128),beta(129),
     * beta(130)/6.22614862573135066d-04,
     * 2.85998154194304147d-04,
     * 3.84737672879366102d-06,
     * -1.87906003636971558d-04,
     * -2.97603646594554535d-04,
     * -3.45998126832656348d-04,
     * -3.53382470916037712d-04,
     * -3.35715635775048757d-04/
      Data beta(131),beta(132),beta(133),beta(134),
     * beta(135),beta(136),beta(137),beta(138),
     * beta(139),beta(140),beta(141),
     * beta(142)/-3.04321124789039809d-04,
     * -2.66722723047612821d-04,
     * -2.27654214122819527d-04,
     * -1.89922611854562356d-04,
     * -1.55058918599093870d-04,
     * -1.23778240761873630d-04,
     * -9.62926147717644187d-05,
     * -7.25178327714425337d-05,
     * -5.22070028895633801d-05,
     * -3.50347750511900522d-05,
     * -2.06489761035551757d-05,
     * -8.70106096849767054d-06/
      Data beta(143),beta(144),beta(145),beta(146),
     * beta(147),beta(148),beta(149),
     * beta(150)/1.13698686675100290d-06,
     * 9.16426474122778849d-06,
     * 1.56477785428872620d-05,
     * 2.08223629482466847d-05,
     * 2.48923381004595156d-05,
     * 2.80340509574146325d-05,
     * 3.03987774629861915d-05,
     * 3.21156731406700616d-05/
      Data beta(151),beta(152),beta(153),beta(154),
     * beta(155),beta(156),beta(157),beta(158),
     * beta(159),beta(160),beta(161),
     * beta(162)/-1.80182191963885708d-03,
     * -2.43402962938042533d-03,
     * -1.83422663549856802d-03,
     * -7.62204596354009765d-04,
     * 2.39079475256927218d-04,
     * 9.49266117176881141d-04,
     * 1.34467449701540359d-03,
     * 1.48457495259449178d-03,
     * 1.44732339830617591d-03,
     * 1.30268261285657186d-03,
     * 1.10351597375642682d-03,
     * 8.86047440419791759d-04/
      Data beta(163),beta(164),beta(165),beta(166),
     * beta(167),beta(168),beta(169),
     * beta(170)/6.73073208165665473d-04,
     * 4.77603872856582378d-04,
     * 3.05991926358789362d-04,
     * 1.60315694594721630d-04,
     * 4.00749555270613286d-05,
     * -5.66607461635251611d-05,
     * -1.32506186772982638d-04,
     * -1.90296187989614057d-04/
      Data beta(171),beta(172),beta(173),beta(174),
     * beta(175),beta(176),beta(177),beta(178),
     * beta(179),beta(180),beta(181),
     * beta(182)/-2.32811450376937408d-04,
     * -2.62628811464668841d-04,
     * -2.82050469867598672d-04,
     * -2.93081563192861167d-04,
     * -2.97435962176316616d-04,
     * -2.96557334239348078d-04,
     * -2.91647363312090861d-04,
     * -2.83696203837734166d-04,
     * -2.73512317095673346d-04,
     * -2.61750155806768580d-04,
     * 6.38585891212050914d-03,
     * 9.62374215806377941d-03/
      Data beta(183),beta(184),beta(185),beta(186),
     * beta(187),beta(188),beta(189),
     * beta(190)/7.61878061207001043d-03,
     * 2.83219055545628054d-03,
     * -2.09841352012720090d-03,
     * -5.73826764216626498d-03,
     * -7.70804244495414620d-03,
     * -8.21011692264844401d-03,
     * -7.65824520346905413d-03,
     * -6.47209729391045177d-03/
      Data beta(191),beta(192),beta(193),beta(194),
     * beta(195),beta(196),beta(197),beta(198),
     * beta(199),beta(200),beta(201),
     * beta(202)/-4.99132412004966473d-03,
     * -3.45612289713133280d-03,
     * -2.01785580014170775d-03,
     * -7.59430686781961401d-04,
     * 2.84173631523859138d-04,
     * 1.10891667586337403d-03,
     * 1.72901493872728771d-03,
     * 2.16812590802684701d-03,
     * 2.45357710494539735d-03,
     * 2.61281821058334862d-03,
     * 2.67141039656276912d-03,
     * 2.65203073395980430d-03/
      Data beta(203),beta(204),beta(205),beta(206),
     * beta(207),beta(208),beta(209),
     * beta(210)/2.57411652877287315d-03,
     * 2.45389126236094427d-03,
     * 2.30460058071795494d-03,
     * 2.13684837686712662d-03,
     * 1.95896528478870911d-03,
     * 1.77737008679454412d-03,
     * 1.59690280765839059d-03,
     * 1.42111975664438546d-03/
      Data gama(1),gama(2),gama(3),gama(4),gama(5),
     * gama(6),gama(7),gama(8),gama(9),gama(10),
     * gama(11),gama(12),gama(13),
     * gama(14)/6.29960524947436582d-01,
     * 2.51984209978974633d-01,
     * 1.54790300415655846d-01,
     * 1.10713062416159013d-01,
     * 8.57309395527394825d-02,
     * 6.97161316958684292d-02,
     * 5.86085671893713576d-02,
     * 5.04698873536310685d-02,
     * 4.42600580689154809d-02,
     * 3.93720661543509966d-02,
     * 3.54283195924455368d-02,
     * 3.21818857502098231d-02,
     * 2.94646240791157679d-02,
     * 2.71581677112934479d-02/
      Data gama(15),gama(16),gama(17),gama(18),
     * gama(19),gama(20),gama(21),
     * gama(22)/2.51768272973861779d-02,
     * 2.34570755306078891d-02,
     * 2.19508390134907203d-02,
     * 2.06210828235646240d-02,
     * 1.94388240897880846d-02,
     * 1.83810633800683158d-02,
     * 1.74293213231963172d-02,
     * 1.65685837786612353d-02/
      Data gama(23),gama(24),gama(25),gama(26),
     * gama(27),gama(28),gama(29),
     * gama(30)/1.57865285987918445d-02,
     * 1.50729501494095594d-02,
     * 1.44193250839954639d-02,
     * 1.38184805735341786d-02,
     * 1.32643378994276568d-02,
     * 1.27517121970498651d-02,
     * 1.22761545318762767d-02,
     * 1.18338262398482403d-02/
      Data ex1,ex2,hpi,pi,thpi/3.33333333333333333d-01,
     * 6.66666666666666667d-01,
     * 1.57079632679489662d+00,
     * 3.14159265358979324d+00,
     * 4.71238898038468986d+00/
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      rfnu=1.0d0/fnu
      tstr=dble(z)
      tsti=dimag(z)
      test=fnu*dexp(-700.0d0)
      if(abs(tstr).lt.test) tstr=0.0d0
      if(abs(tsti).lt.test) tsti=0.0d0
      if(tstr.eq.0.0d0.and.tsti.eq.0.0d0) then
        zeta1=dcmplx(1400.0d0+fnu,0.0d0)
        zeta2=dcmplx(fnu,0.0d0)
        phi=cone
        arg=cone
        return
      endif
      zb=dcmplx(tstr,tsti)*dcmplx(rfnu,0.0d0)
      rfnu2=rfnu*rfnu
      fn13=fnu**ex1
      fn23=fn13*fn13
      rfn13=dcmplx(1.0d0/fn13,0.0d0)
      w2=cone-zb*zb
      aw2=abs(w2)
      if(aw2.gt.0.25d0) then
        w=sqrt(w2)
        wr=dble(w)
        wi=dimag(w)
        if(wr.lt.0.0d0) wr=0.0d0
        if(wi.lt.0.0d0) wi=0.0d0
        w=dcmplx(wr,wi)
        za =(cone+w)/zb
        zc=cdlog(za)
        zcr=dble(zc)
        zci=dimag(zc)
        if(zci.lt.0.0d0) zci=0.0d0
        if(zci.gt.hpi) zci=hpi
        if(zcr.lt.0.0d0) zcr=0.0d0
        zc=dcmplx(zcr,zci)
        zth =(zc-w)*dcmplx(1.5d0,0.0d0)
        cfnu=dcmplx(fnu,0.0d0)
        zeta1=zc*cfnu
        zeta2=w*cfnu
        azth=abs(zth)
        zthr=dble(zth)
        zthi=dimag(zth)
        ang=thpi
        if(zthr.lt.0.0d0.or.zthi.ge.0.0d0) then
        ang=hpi
        if(zthr.ne.0.0d0) then
           ang=atan(zthi/zthr)
           if(zthr.lt.0.0d0) ang=ang+pi
        endif
        endif
        pp=azth**ex2
        ang=ang*ex2
        zetar=pp*cos(ang)
        zetai=pp*sin(ang)
        if(zetai.lt.0.0d0) zetai=0.0d0
        zeta=dcmplx(zetar,zetai)
        arg=zeta*dcmplx(fn23,0.0d0)
        rtzta=zth/zeta
        za=rtzta/w
        phi=sqrt(za+za)*rfn13
        if(ipmtr.ne.1) then
        tfn=dcmplx(rfnu,0.0d0)/w
        rzth=dcmplx(rfnu,0.0d0)/zth
        zc=rzth*dcmplx(ar(2),0.0d0)
        t2=cone/w2
        up(2) =(t2*dcmplx(c(2),0.0d0)+dcmplx(c(3),0.0d0))*tfn
        bsum=up(2)+zc
        asum=czero
        if(rfnu.ge.1.0d-16) then
           przth=rzth
           ptfn=tfn
           up(1)=cone
           pp=1.0d0
           bsumr=dble(bsum)
           bsumi=dimag(bsum)
           btol=1.0d-16*(abs(bsumr)+abs(bsumi))
           ks=0
           kp1=2
           l=3
           ias=0
           ibs=0
           do 100 lr=2,12,2
             lrp1=lr+1
             do 40 k=lr,lrp1
               ks=ks+1
               kp1=kp1+1
               l=l+1
               za=dcmplx(c(l),0.0d0)
               do 20 j=2,kp1
                l=l+1
                za=za*t2+dcmplx(c(l),0.0d0)
   20            continue
               ptfn=ptfn*tfn
               up(kp1)=ptfn*za
               cr(ks)=przth*dcmplx(br(ks+1),0.0d0)
               przth=przth*rzth
               dr(ks)=przth*dcmplx(ar(ks+2),0.0d0)
   40         continue
             pp=pp*rfnu2
             if(ias.ne.1) then
               suma=up(lrp1)
               ju=lrp1
               do 60 jr=1,lr
                ju=ju-1
                suma=suma+cr(jr)*up(ju)
   60            continue
               asum=asum+suma
               asumr=dble(asum)
               asumi=dimag(asum)
               test=abs(asumr)+abs(asumi)
               if(pp.lt.1.0d-16.and.test.lt.1.0d-16) ias=1
             endif
             if(ibs.ne.1) then
               sumb=up(lr+2)+up(lrp1)*zc
               ju=lrp1
               do 80 jr=1,lr
                ju=ju-1
                sumb=sumb+dr(jr)*up(ju)
   80            continue
               bsum=bsum+sumb
               bsumr=dble(bsum)
               bsumi=dimag(bsum)
               test=abs(bsumr)+abs(bsumi)
               if(pp.lt.btol.and.test.lt.1.0d-16) ibs=1
             endif
             if(ias.eq.1.and.ibs.eq.1) goto 120
  100        continue
        endif
  120       asum=asum+cone
        bsum=-bsum*rfn13/rtzta
        endif
      else
        k=1
        p(1)=cone
        suma=dcmplx(gama(1),0.0d0)
        ap(1)=1.0d0
        if(aw2.ge.1.0d-16) then
        do 140 k=2,30
           p(k)=p(k-1)*w2
           suma=suma+p(k)*dcmplx(gama(k),0.0d0)
           ap(k)=ap(k-1)*aw2
           if(ap(k).lt.1.0d-16) goto 160
  140       continue
        k=30
        endif
  160    kmax=k
        zeta=w2*suma
        arg=zeta*dcmplx(fn23,0.0d0)
        za=sqrt(suma)
        zeta2=sqrt(w2)*dcmplx(fnu,0.0d0)
        zeta1=zeta2*(cone+zeta*za*dcmplx(ex2,0.0d0))
        za=za+za
        phi=sqrt(za)*rfn13
        if(ipmtr.ne.1) then
        sumb=czero
        do 180 k=1,kmax
           sumb=sumb+p(k)*dcmplx(beta(k),0.0d0)
  180       continue
        asum=czero
        bsum=sumb
        l1=0
        l2=30
        btol=1.0d-16*abs(bsum)
        atol=1.0d-16
        pp=1.0d0
        ias=0
        ibs=0
        if(rfnu2.ge.1.0d-16) then
           do 280 is=2,7
             atol=atol/rfnu2
             pp=pp*rfnu2
             if(ias.ne.1) then
               suma=czero
               do 200 k=1,kmax
                m=l1+k
                suma=suma+p(k)*dcmplx(alfa(m),0.0d0)
                if(ap(k).lt.atol) goto 220
  200            continue
  220            asum=asum+suma*dcmplx(pp,0.0d0)
               if(pp.lt.1.0d-16) ias=1
             endif
             if(ibs.ne.1) then
               sumb=czero
               do 240 k=1,kmax
                m=l2+k
                sumb=sumb+p(k)*dcmplx(beta(m),0.0d0)
                if(ap(k).lt.atol) goto 260
  240            continue
  260            bsum=bsum+sumb*dcmplx(pp,0.0d0)
               if(pp.lt.btol) ibs=1
             endif
             if(ias.eq.1.and.ibs.eq.1) then
               goto 300
             else
               l1=l1+30
               l2=l2+30
             endif

  280        continue
        endif
  300       asum=asum+cone
        pp=rfnu*dble(rfn13)
        bsum=bsum*dcmplx(pp,0.0d0)
        endif
      endif
      return
      end


      subroutine zikrp2(zr,fnu,ikflg,ipmtr,init,phi,zeta1,zeta2,sum,
     * cwrk)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) phi,sum,zeta1,zeta2,zr,cwrk(16),cfn,cone,crfn,czero,s,
     * sr,t,t2,zn,con(2)
      Dimension c(120)
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      Data con(1),con(2)/(3.98942280401432678d-01,0.0d0),
     * (1.25331413731550025d+00,0.0d0)/
      Data c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),
     * c(9),c(10),c(11),c(12),c(13),c(14),c(15),
     * c(16)/1.00000000000000000d+00,
     * -2.08333333333333333d-01,
     * 1.25000000000000000d-01,
     * 3.34201388888888889d-01,
     * -4.01041666666666667d-01,
     * 7.03125000000000000d-02,
     * -1.02581259645061728d+00,
     * 1.84646267361111111d+00,
     * -8.91210937500000000d-01,
     * 7.32421875000000000d-02,
     * 4.66958442342624743d+00,
     * -1.12070026162229938d+01,
     * 8.78912353515625000d+00,
     * -2.36408691406250000d+00,
     * 1.12152099609375000d-01,
     * -2.82120725582002449d+01/
      Data c(17),c(18),c(19),c(20),c(21),c(22),c(23),
     * c(24)/8.46362176746007346d+01,
     * -9.18182415432400174d+01,
     * 4.25349987453884549d+01,
     * -7.36879435947963170d+00,
     * 2.27108001708984375d-01,
     * 2.12570130039217123d+02,
     * -7.65252468141181642d+02,
     * 1.05999045252799988d+03/
      Data c(25),c(26),c(27),c(28),c(29),c(30),c(31),
     * c(32),c(33),c(34),c(35),c(36),c(37),c(38),
     * c(39),c(40)/-6.99579627376132541d+02,
     * 2.18190511744211590d+02,
     * -2.64914304869515555d+01,
     * 5.72501420974731445d-01,
     * -1.91945766231840700d+03,
     * 8.06172218173730938d+03,
     * -1.35865500064341374d+04,
     * 1.16553933368645332d+04,
     * -5.30564697861340311d+03,
     * 1.20090291321635246d+03,
     * -1.08090919788394656d+02,
     * 1.72772750258445740d+00,
     * 2.02042913309661486d+04,
     * -9.69805983886375135d+04,
     * 1.92547001232531532d+05,
     * -2.03400177280415534d+05/
      Data c(41),c(42),c(43),c(44),c(45),c(46),c(47),
     * c(48)/1.22200464983017460d+05,
     * -4.11926549688975513d+04,
     * 7.10951430248936372d+03,
     * -4.93915304773088012d+02,
     * 6.07404200127348304d+00,
     * -2.42919187900551333d+05,
     * 1.31176361466297720d+06,
     * -2.99801591853810675d+06/
      Data c(49),c(50),c(51),c(52),c(53),c(54),c(55),
     * c(56),c(57),c(58),c(59),c(60),c(61),c(62),
     * c(63),c(64)/3.76327129765640400d+06,
     * -2.81356322658653411d+06,
     * 1.26836527332162478d+06,
     * -3.31645172484563578d+05,
     * 4.52187689813627263d+04,
     * -2.49983048181120962d+03,
     * 2.43805296995560639d+01,
     * 3.28446985307203782d+06,
     * -1.97068191184322269d+07,
     * 5.09526024926646422d+07,
     * -7.41051482115326577d+07,
     * 6.63445122747290267d+07,
     * -3.75671766607633513d+07,
     * 1.32887671664218183d+07,
     * -2.78561812808645469d+06,
     * 3.08186404612662398d+05/
      Data c(65),c(66),c(67),c(68),c(69),c(70),c(71),
     * c(72)/-1.38860897537170405d+04,
     * 1.10017140269246738d+02,
     * -4.93292536645099620d+07,
     * 3.25573074185765749d+08,
     * -9.39462359681578403d+08,
     * 1.55359689957058006d+09,
     * -1.62108055210833708d+09,
     * 1.10684281682301447d+09/
      Data c(73),c(74),c(75),c(76),c(77),c(78),c(79),
     * c(80),c(81),c(82),c(83),c(84),c(85),c(86),
     * c(87),c(88)/-4.95889784275030309d+08,
     * 1.42062907797533095d+08,
     * -2.44740627257387285d+07,
     * 2.24376817792244943d+06,
     * -8.40054336030240853d+04,
     * 5.51335896122020586d+02,
     * 8.14789096118312115d+08,
     * -5.86648149205184723d+09,
     * 1.86882075092958249d+10,
     * -3.46320433881587779d+10,
     * 4.12801855797539740d+10,
     * -3.30265997498007231d+10,
     * 1.79542137311556001d+10,
     * -6.56329379261928433d+09,
     * 1.55927986487925751d+09,
     * -2.25105661889415278d+08/
      Data c(89),c(90),c(91),c(92),c(93),c(94),c(95),
     * c(96)/1.73951075539781645d+07,
     * -5.49842327572288687d+05,
     * 3.03809051092238427d+03,
     * -1.46792612476956167d+10,
     * 1.14498237732025810d+11,
     * -3.99096175224466498d+11,
     * 8.19218669548577329d+11,
     * -1.09837515608122331d+12/
      Data c(97),c(98),c(99),c(100),c(101),c(102),
     * c(103),c(104),c(105),c(106),c(107),c(108),
     * c(109),c(110)/1.00815810686538209d+12,
     * -6.45364869245376503d+11,
     * 2.87900649906150589d+11,
     * -8.78670721780232657d+10,
     * 1.76347306068349694d+10,
     * -2.16716498322379509d+09,
     * 1.43157876718888981d+08,
     * -3.87183344257261262d+06,
     * 1.82577554742931747d+04,
     * 2.86464035717679043d+11,
     * -2.40629790002850396d+12,
     * 9.10934118523989896d+12,
     * -2.05168994109344374d+13,
     * 3.05651255199353206d+13/
      Data c(111),c(112),c(113),c(114),c(115),c(116),
     * c(117),c(118),c(119),
     * c(120)/-3.16670885847851584d+13,
     * 2.33483640445818409d+13,
     * -1.23204913055982872d+13,
     * 4.61272578084913197d+12,
     * -1.19655288019618160d+12,
     * 2.05914503232410016d+11,
     * -2.18229277575292237d+10,
     * 1.24700929351271032d+09,
     * -2.91883881222208134d+07,
     * 1.18838426256783253d+05/
      if(init.eq.0) then
        rfn=1.0d0/fnu
        crfn=dcmplx(rfn,0.0d0)
        tstr=dble(zr)
        tsti=dimag(zr)
        test=fnu*dexp(-700.0d0)
        if(abs(tstr).lt.test) tstr=0.0d0
        if(abs(tsti).lt.test) tsti=0.0d0
        if(tstr.eq.0.0d0.and.tsti.eq.0.0d0) then
        zeta1=dcmplx(1400.0d0+fnu,0.0d0)
        zeta2=dcmplx(fnu,0.0d0)
        phi=cone
        return
        endif
        t=dcmplx(tstr,tsti)*crfn
        s=cone+t*t
        sr=sqrt(s)
        cfn=dcmplx(fnu,0.0d0)
        zn =(cone+sr)/t
        zeta1=cfn*cdlog(zn)
        zeta2=cfn*sr
        t=cone/sr
        sr=t*crfn
        cwrk(16)=sqrt(sr)
        phi=cwrk(16)*con(ikflg)
        if(ipmtr.ne.0) then
        return
        else
        t2=cone/s
        cwrk(1)=cone
        crfn=cone
        ac=1.0d0
        l=1
        do 40 k=2,15
           s=czero
           do 20 j=1,k
             l=l+1
             s=s*t2+dcmplx(c(l),0.0d0)
   20        continue
           crfn=crfn*sr
           cwrk(k)=crfn*s
           ac=ac*rfn
           tstr=dble(cwrk(k))
           tsti=dimag(cwrk(k))
           test=abs(tstr)+abs(tsti)
           if(ac.lt.1.0d-16.and.test.lt.1.0d-16) goto 60
   40       continue
        k=15
   60       init=k
        endif
      endif
      if(ikflg.eq.2) then
        s=czero
        t=cone
        do 80 i=1,init
        s=s+t*cwrk(i)
        t=-t
   80    continue
        sum=s
        phi=cwrk(16)*con(2)
      else
        s=czero
        do 100 i=1,init
        s=s+cwrk(i)
  100    continue
        sum=s
        phi=cwrk(16)*con(1)
      endif
      return
      end


      subroutine zirua1(z,fnu,scale,n,y,nz,nlast)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),ai,arg,asum,bsum,c1,c2,cfn,ci,cid,cone,crsc,
     * cscl,czero,dai,phi,rz,s1,s2,zb,zeta1,zeta2,zn,cip(4),csr(3),
     * css(3),cy(2)
      Dimension bry(3)
      Logical scale
      Data czero,cone,ci/(0.0d0,0.0d0),(1.0d0,0.0d0),
     * (0.0d0,1.0d0)/
      Data cip(1),cip(2),cip(3),cip(4)/(1.0d0,0.0d0),
     * (0.0d0,1.0d0),(-1.0d0,0.0d0),(0.0d0,-1.0d0)/
      Data hpi,aic/1.57079632679489662d+00,
     * 1.265512123484645396d+00/
      nz=0
      nd=n
      nlast=0
      cscl=dcmplx(1.0d16,0.0d0)
      crsc=dcmplx(1.0d-16,0.0d0)
      css(1)=cscl
      css(2)=cone
      css(3)=crsc
      csr(1)=crsc
      csr(2)=cone
      csr(3)=cscl
      bry(1)=1.0d-288
      yy=dimag(z)
      zn=-z*ci
      zb=z
      cid=-ci
      inu=int(fnu)
      ang=hpi*(fnu-inu)
      car=cos(ang)
      sar=sin(ang)
      c2=dcmplx(car,sar)
      in=inu+n-1
      in=mod(in,4)
      c2=c2*cip(in+1)
      if(yy.le.0.0d0) then
        zn=dconjg(-zn)
        zb=dconjg(zb)
        cid=-cid
        c2=dconjg(c2)
      endif
      fn=max(fnu,1.0d0)
      call zjhrua(zn,fn,1_4,phi,arg,zeta1,zeta2,asum,bsum)
      if(.not.scale) then
        s1=-zeta1+zeta2
      else
        cfn=dcmplx(fnu,0.0d0)
        s1=-zeta1+cfn*(cfn/(zb+zeta2))
      endif
      rs1=dble(s1)
      if(abs(rs1).le.700.0d0) then
   20    continue
        nn=min(2,nd)
        do 40 i=1,nn
        fn=fnu+nd-i
        call zjhrua(zn,fn,0_4,phi,arg,zeta1,zeta2,asum,bsum)
        if(.not.scale) then
           s1=-zeta1+zeta2
        else
           cfn=dcmplx(fn,0.0d0)
           ay=abs(yy)
           s1=-zeta1+cfn*(cfn/(zb+zeta2))+dcmplx(0.0d0,ay)
        endif
        rs1=dble(s1)
        if(abs(rs1).gt.700.0d0) then
           goto 60
        else
           if(i.eq.1) iflag=2
           if(abs(rs1).ge.664.0d0) then
             aphi=abs(phi)
             aarg=abs(arg)
             rs1=rs1+dlog(aphi)-0.25d0*dlog(aarg)-aic
             if(abs(rs1).gt.700.0d0) then
               goto 60
             else
               if(i.eq.1) iflag=1
               if(rs1.ge.0.0d0) then
                if(i.eq.1) iflag=3
               endif
             endif
           endif
           idum=1
           call zairy(.false.,arg,.true.,ai,nai,idum)
           idum=1
           call zairy(.true.,arg,.true.,dai,ndai,idum)
           s2=phi*(ai*asum+dai*bsum)
           c2r=dble(s1)
           c2i=dimag(s1)
           c2m=dexp(c2r)*dble(css(iflag))
           s1=dcmplx(c2m,0.0d0)*dcmplx(cos(c2i),sin(c2i))
           s2=s2*s1
           if(iflag.eq.1) then
             call zpt(s2,nw,bry(1))
             if(nw.ne.0) goto 60
           endif
           if(yy.le.0.0d0) s2=dconjg(s2)
           j=nd-i+1
           s2=s2*c2
           cy(i)=s2
           y(j)=s2*csr(iflag)
           c2=c2*cid
        endif
   40    continue
        goto 80
   60    if(rs1.gt.0.0d0) then
        goto 160
        else
        y(nd)=czero
        nz=nz+1
        nd=nd-1
        if(nd.eq.0) then
           return
        else
           call zikrp1(z,fnu,scale,1_4,nd,y,nuf)
           if(nuf.lt.0) then
             goto 160
           else
             nd=nd-nuf
             nz=nz+nuf
             if(nd.eq.0) then
               return
             else
               fn=fnu+nd-1
               if(fn.lt.82.0d0) then
                goto 120
               else
                c2=dcmplx(car,sar)
                in=inu+nd-1
                in=mod(in,4)+1
                c2=c2*cip(in)
                if(yy.le.0.0d0) c2=dconjg(c2)
                goto 20
               endif
             endif
           endif
        endif
        endif
   80    if(nd.gt.2) then
        rz=dcmplx(2.0d0,0.0d0)/z
        bry(2)=1.0d0/bry(1)
        bry(3)=1.0d308
        s1=cy(1)
        s2=cy(2)
        c1=csr(iflag)
        ascle=bry(iflag)
        k=nd-2
        fn=k
        do 100 i=3,nd
           c2=s2
           s2=s1+dcmplx(fnu+fn,0.0d0)*rz*s2
           s1=c2
           c2=s2*c1
           y(k)=c2
           k=k-1
           fn=fn-1.0d0
           if(iflag.lt.3) then
             c2r=dble(c2)
             c2i=dimag(c2)
             c2r=abs(c2r)
             c2i=abs(c2i)
             c2m=max(c2r,c2i)
             if(c2m.gt.ascle) then
               iflag=iflag+1
               ascle=bry(iflag)
               s1=s1*c1
               s2=c2
               s1=s1*css(iflag)
               s2=s2*css(iflag)
               c1=csr(iflag)
             endif
           endif
  100       continue
        endif
        return
  120    nlast=nd
        return
      elseif(rs1.le.0.0d0) then
        nz=n
        do 140 i=1,n
        y(i)=czero
  140    continue
        return
      endif
  160 nz=-1
      return
      end


      subroutine zirua2(z,fnu,scale,n,y,nz,nlast)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),c1,c2,cfn,cone,crsc,cscl,czero,phi,rz,s1,s2,
     * sum,zeta1,zeta2,csr(3),css(3),cwrk(16),cy(2)
      Dimension bry(3)
      Logical scale
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      nz=0
      nd=n
      nlast=0
      cscl=dcmplx(1.0d16,0.0d0)
      crsc=dcmplx(1.0d-16,0.0d0)
      css(1)=cscl
      css(2)=cone
      css(3)=crsc
      csr(1)=crsc
      csr(2)=cone
      csr(3)=cscl
      bry(1)=1.0d-288
      fn=max(fnu,1.0d0)
      init=0
      call zikrp2(z,fn,1_4,1_4,init,phi,zeta1,zeta2,sum,cwrk)
      if(.not.scale) then
        s1=-zeta1+zeta2
      else
        cfn=dcmplx(fn,0.0d0)
        s1=-zeta1+cfn*(cfn/(z+zeta2))
      endif
      rs1=dble(s1)
      if(abs(rs1).le.700.0d0) then
   20    continue
        nn=min(2,nd)
        do 40 i=1,nn
        fn=fnu+nd-i
        init=0
        call zikrp2(z,fn,1_4,0_4,init,phi,zeta1,zeta2,sum,cwrk)
        if(.not.scale) then
           s1=-zeta1+zeta2
        else
           cfn=dcmplx(fn,0.0d0)
           yy=dimag(z)
           s1=-zeta1+cfn*(cfn/(z+zeta2))+dcmplx(0.0d0,yy)
        endif
        rs1=dble(s1)
        if(abs(rs1).gt.700.0d0) then
           goto 60
        else
           if(i.eq.1) iflag=2
           if(abs(rs1).ge.664.0d0) then
             aphi=abs(phi)
             rs1=rs1+dlog(aphi)
             if(abs(rs1).gt.700.0d0) then
               goto 60
             else
               if(i.eq.1) iflag=1
               if(rs1.ge.0.0d0) then
                if(i.eq.1) iflag=3
               endif
             endif
           endif
           s2=phi*sum
           c2r=dble(s1)
           c2i=dimag(s1)
           c2m=dexp(c2r)*dble(css(iflag))
           s1=dcmplx(c2m,0.0d0)*dcmplx(cos(c2i),sin(c2i))
           s2=s2*s1
           if(iflag.eq.1) then
             call zpt(s2,nw,bry(1))
             if(nw.ne.0) goto 60
           endif
           m=nd-i+1
           cy(i)=s2
           y(m)=s2*csr(iflag)
        endif
   40    continue
        goto 80
   60    continue
        if(rs1.gt.0.0d0) then
        goto 160
        else
        y(nd)=czero
        nz=nz+1
        nd=nd-1
        if(nd.eq.0) then
           return
        else
           call zikrp1(z,fnu,scale,1_4,nd,y,nuf)
           if(nuf.lt.0) then
             goto 160
           else
             nd=nd-nuf
             nz=nz+nuf
             if(nd.eq.0) then
               return
             else
               fn=fnu+nd-1
               if(fn.ge.82.0d0) then
                goto 20
               else
                goto 120
               endif
             endif
           endif
        endif
        endif
   80    if(nd.gt.2) then
        rz=dcmplx(2.0d0,0.0d0)/z
        bry(2)=1.0d0/bry(1)
        bry(3)=1.0d308
        s1=cy(1)
        s2=cy(2)
        c1=csr(iflag)
        ascle=bry(iflag)
        k=nd-2
        fn=k
        do 100 i=3,nd
           c2=s2
           s2=s1+dcmplx(fnu+fn,0.0d0)*rz*s2
           s1=c2
           c2=s2*c1
           y(k)=c2
           k=k-1
           fn=fn-1.0d0
           if(iflag.lt.3) then
             c2r=dble(c2)
             c2i=dimag(c2)
             c2r=abs(c2r)
             c2i=abs(c2i)
             c2m=max(c2r,c2i)
             if(c2m.gt.ascle) then
               iflag=iflag+1
               ascle=bry(iflag)
               s1=s1*c1
               s2=c2
               s1=s1*css(iflag)
               s2=s2*css(iflag)
               c1=csr(iflag)
             endif
           endif
  100       continue
        endif
        return
  120    nlast=nd
        return
      elseif(rs1.le.0.0d0) then
        nz=n
        do 140 i=1,n
        y(i)=czero
  140    continue
        return
      endif
  160 nz=-1
      return
      end

      subroutine zkrca(z,fnu,scale,mr,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),c1,c2,csgn,cspn,zn,cy(2)
      Logical scale
      Data pi/3.14159265358979324d0/
      nz=0
      zn=-z
      az=abs(z)
      nn=n
      dfnu=fnu+dble(n-1)
      if(az.gt.2.0d0) then
        if(az*az*0.25d0.gt.dfnu+1.0d0) then
        if(az.lt.21.0d0) then
           call zirm(zn,fnu,scale,nn,y,nw)
           if(nw.lt.0) then
             goto 40
           else
             goto 20
           endif
        else
           call zirua0(zn,fnu,scale,nn,y,nw)
           if(nw.lt.0) then
             goto 40
           else
             goto 20
           endif
        endif
        endif
      endif
      call zirp(zn,fnu,scale,nn,y,nw)
   20 call zkr0(zn,fnu,scale,1_4,cy,nw)
      if(nw.eq.0) then
        fmr=mr
        sgn=-sign(pi,fmr)
        csgn=dcmplx(0.0d0,sgn)
        if(scale) then
        yy=-dimag(zn)
        cpn=cos(yy)
        spn=sin(yy)
        csgn=csgn*dcmplx(cpn,spn)
        endif
        inu=int(fnu)
        arg =(fnu-inu)*sgn
        cpn=cos(arg)
        spn=sin(arg)
        cspn=dcmplx(cpn,spn)
        if(mod(inu,2).eq.1) cspn=-cspn
        c1=cy(1)
        c2=y(1)
        if(scale) then
        iuf=0
        ascle=1.0d-288
        call zirt(zn,c1,c2,nw,ascle,iuf)
        nz=nz+nw
        endif
        y(1)=cspn*c1+csgn*c2
        return
      endif
   40 nz=-1
      if(nw.eq.(-2)) nz=-2
      if(nw.eq.(-3)) nz=-3
      return
      end


      subroutine zkrua1(z,fnu,scale,mr,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),ai,argd,asumd,bsumd,c1,c2,cfn,ci,ck,cone,cr1,
     * cr2,crsc,cs,cscl,csgn,cspn,czero,dai,phid,rz,s1,s2,zb,zeta1d,
     * zeta2d,zn,zr,arg(2),asum(2),bsum(2),cip(4),csr(3),css(3),cy(2),
     * phi(2),zeta1(2),zeta2(2)
      Dimension bry(3)
      Logical scale
      Data czero,cone,ci,cr1,cr2/(0.0d0,0.0d0),
     * (1.0d0,0.0d0),(0.0d0,1.0d0),
     * (1.0d0,1.73205080756887729d0),
     * (-0.5d0,-8.66025403784438647d-01)/
      Data hpi,pi,aic/1.57079632679489662d+00,
     * 3.14159265358979324d+00,
     * 1.26551212348464539d+00/
      Data cip(1),cip(2),cip(3),cip(4)/(1.0d0,0.0d0),
     * (0.0d0,-1.0d0),(-1.0d0,0.0d0),(0.0d0,1.0d0)/
      kdflg=1
      nz=0
      cscl=dcmplx(1.0d16,0.0d0)
      crsc=dcmplx(1.0d-16,0.0d0)
      css(1)=cscl
      css(2)=cone
      css(3)=crsc
      csr(1)=crsc
      csr(2)=cone
      csr(3)=cscl
      bry(1)=1.0d-288
      bry(2)=1.0d0/bry(1)
      bry(3)=1.0d308
      x=dble(z)
      zr=z
      if(x.lt.0.0d0) zr=-z
      yy=dimag(zr)
      zn=-zr*ci
      zb=zr
      inu=int(fnu)
      fnf=fnu-inu
      ang=-hpi*fnf
      car=cos(ang)
      sar=sin(ang)
      cpn=-hpi*car
      spn=-hpi*sar
      c2=dcmplx(-spn,cpn)
      kk=mod(inu,4)+1
      cs=cr1*c2*cip(kk)
      if(yy.le.0.0d0) then
        zn=dconjg(-zn)
        zb=dconjg(zb)
      endif
      j=2
      do 40 i=1,n
        j=3-j
        fn=fnu+dble(i-1)
        call zjhrua(zn,fn,0_4,phi(j),arg(j),zeta1(j),zeta2(j),asum(j)
     *          ,bsum(j))
        if(.not.scale) then
        s1=zeta1(j)-zeta2(j)
        else
        cfn=dcmplx(fn,0.0d0)
        s1=zeta1(j)-cfn*(cfn/(zb+zeta2(j)))
        endif
        rs1=dble(s1)
        if(abs(rs1).le.700.0d0) then
        if(kdflg.eq.1) kflag=2
        if(abs(rs1).ge.664.0d0) then
           aphi=abs(phi(j))
           aarg=abs(arg(j))
           rs1=rs1+dlog(aphi)-0.25d0*dlog(aarg)-aic
           if(abs(rs1).gt.700.0d0) then
             goto 20
           else
             if(kdflg.eq.1) kflag=1
             if(rs1.ge.0.0d0) then
               if(kdflg.eq.1) kflag=3
             endif
           endif
        endif
        c2=arg(j)*cr2
        idum=1
        call zairy(.false.,c2,.true.,ai,nai,idum)
        idum=1
        call zairy(.true.,c2,.true.,dai,ndai,idum)
        s2=cs*phi(j)*(ai*asum(j)+cr2*dai*bsum(j))
        c2r=dble(s1)
        c2i=dimag(s1)
        c2m=dexp(c2r)*dble(css(kflag))
        s1=dcmplx(c2m,0.0d0)*dcmplx(cos(c2i),sin(c2i))
        s2=s2*s1
        if(kflag.eq.1) then
           call zpt(s2,nw,bry(1))
           if(nw.ne.0) goto 20
        endif
        if(yy.le.0.0d0) s2=dconjg(s2)
        cy(kdflg)=s2
        y(i)=s2*csr(kflag)
        cs=-ci*cs
        if(kdflg.eq.2) then
           goto 60
        else
           kdflg=2
           goto 40
        endif
        endif
   20    if(rs1.gt.0.0d0) then
        goto 280
        elseif(x.lt.0.0d0) then
        goto 280
        else
        kdflg=1
        y(i)=czero
        cs=-ci*cs
        nz=nz+1
        if(i.ne.1) then
           if(y(i-1).ne.czero) then
             y(i-1)=czero
             nz=nz+1
           endif
        endif
        endif
   40 continue
      i=n
   60 rz=dcmplx(2.0d0,0.0d0)/zr
      ck=dcmplx(fn,0.0d0)*rz
      ib=i+1
      if(n.ge.ib) then
        fn=fnu+dble(n-1)
        ipard=1
        if(mr.ne.0) ipard=0
        call zjhrua(zn,fn,ipard,phid,argd,zeta1d,zeta2d,asumd,
     *           bsumd)
        if(.not.scale) then
        s1=zeta1d-zeta2d
        else
        cfn=dcmplx(fn,0.0d0)
        s1=zeta1d-cfn*(cfn/(zb+zeta2d))
        endif
        rs1=dble(s1)
        if(abs(rs1).le.700.0d0) then
        if(abs(rs1).ge.664.0d0) then
           aphi=abs(phid)
           aarg=abs(argd)
           rs1=rs1+dlog(aphi)-0.25d0*dlog(aarg)-aic
           if(abs(rs1).ge.700.0d0) goto 100
        endif
        s1=cy(1)
        s2=cy(2)
        c1=csr(kflag)
        ascle=bry(kflag)
        do 80 i=ib,n
           c2=s2
           s2=ck*s2+s1
           s1=c2
           ck=ck+rz
           c2=s2*c1
           y(i)=c2
           if(kflag.lt.3) then
             c2r=dble(c2)
             c2i=dimag(c2)
             c2r=abs(c2r)
             c2i=abs(c2i)
             c2m=max(c2r,c2i)
             if(c2m.gt.ascle) then
               kflag=kflag+1
               ascle=bry(kflag)
               s1=s1*c1
               s2=c2
               s1=s1*css(kflag)
               s2=s2*css(kflag)
               c1=csr(kflag)
             endif
           endif
   80       continue
        goto 140
        endif
  100    if(rs1.gt.0.0d0) then
        goto 280
        elseif(x.lt.0.0d0) then
        goto 280
        else
        nz=n
        do 120 i=1,n
           y(i)=czero
  120       continue
        return
        endif
      endif
  140 if(mr.eq.0) then
        return
      else
        nz=0
        fmr=mr
        sgn=-sign(pi,fmr)
        csgn=dcmplx(0.0d0,sgn)
        if(yy.le.0.0d0) csgn=dconjg(csgn)
        ifn=inu+n-1
        ang=fnf*sgn
        cpn=cos(ang)
        spn=sin(ang)
        cspn=dcmplx(cpn,spn)
        if(mod(ifn,2).eq.1) cspn=-cspn
        cs=dcmplx(car,-sar)*csgn
        in=mod(ifn,4)+1
        c2=cip(in)
        cs=cs*dconjg(c2)
        asc=bry(1)
        kk=n
        kdflg=1
        ib=ib-1
        ic=ib-1
        iuf=0
        do 220 k=1,n
        fn=fnu+dble(kk-1)
        if(n.gt.2) then
           if((kk.eq.n) .and.(ib.lt.n)) then
             goto 160
           elseif((kk.ne.ib) .and.(kk.ne.ic)) then
             call zjhrua(zn,fn,0,phid,argd,zeta1d,zeta2d,asumd,
     *    bsumd)
             goto 160
           endif
        endif
        phid=phi(j)
        argd=arg(j)
        zeta1d=zeta1(j)
        zeta2d=zeta2(j)
        asumd=asum(j)
        bsumd=bsum(j)
        j=3-j
  160       if(.not.scale) then
           s1=-zeta1d+zeta2d
        else
           cfn=dcmplx(fn,0.0d0)
           s1=-zeta1d+cfn*(cfn/(zb+zeta2d))
        endif
        rs1=dble(s1)
        if(abs(rs1).le.700.0d0) then
           if(kdflg.eq.1) iflag=2
           if(abs(rs1).ge.664.0d0) then
             aphi=abs(phid)
             aarg=abs(argd)
             rs1=rs1+dlog(aphi)-0.25d0*dlog(aarg)-aic
             if(abs(rs1).gt.700.0d0) then
               goto 180
             else
               if(kdflg.eq.1) iflag=1
               if(rs1.ge.0.0d0) then
                if(kdflg.eq.1) iflag=3
               endif
             endif
           endif
           idum=1
           call zairy(.false.,argd,.true.,ai,nai,idum)
           idum=1
           call zairy(.true.,argd,.true.,dai,ndai,idum)
           s2=cs*phid*(ai*asumd+dai*bsumd)
           c2r=dble(s1)
           c2i=dimag(s1)
           c2m=dexp(c2r)*dble(css(iflag))
           s1=dcmplx(c2m,0.0d0)*dcmplx(cos(c2i),sin(c2i))
           s2=s2*s1
           if(iflag.eq.1) then
             call zpt(s2,nw,bry(1))
             if(nw.ne.0) s2=dcmplx(0.0d0,0.0d0)
           endif
           goto 200
        endif
  180       if(rs1.gt.0.0d0) then
           goto 280
        else
           s2=czero
        endif
  200       if(yy.le.0.0d0) s2=dconjg(s2)
        cy(kdflg)=s2
        c2=s2
        s2=s2*csr(iflag)
        s1=y(kk)
        if(scale) then
           call zirt(zr,s1,s2,nw,asc,iuf)
           nz=nz+nw
        endif
        y(kk)=s1*cspn+s2
        kk=kk-1
        cspn=-cspn
        cs=-cs*ci
        if(c2.eq.czero) then
           kdflg=1
        elseif(kdflg.eq.2) then
           goto 240
        else
           kdflg=2
        endif
  220    continue
        k=n
  240    il=n-k
        if(il.ne.0) then
        s1=cy(1)
        s2=cy(2)
        cs=csr(iflag)
        ascle=bry(iflag)
        fn=inu+il
        do 260 i=1,il
           c2=s2
           s2=s1+dcmplx(fn+fnf,0.0d0)*rz*s2
           s1=c2
           fn=fn-1.0d0
           c2=s2*cs
           ck=c2
           c1=y(kk)
           if(scale) then
             call zirt(zr,c1,c2,nw,asc,iuf)
             nz=nz+nw
           endif
           y(kk)=c1*cspn+c2
           kk=kk-1
           cspn=-cspn
           if(iflag.lt.3) then
             c2r=dble(ck)
             c2i=dimag(ck)
             c2r=abs(c2r)
             c2i=abs(c2i)
             c2m=max(c2r,c2i)
             if(c2m.gt.ascle) then
               iflag=iflag+1
               ascle=bry(iflag)
               s1=s1*cs
               s2=ck
               s1=s1*css(iflag)
               s2=s2*css(iflag)
               cs=csr(iflag)
             endif
           endif
  260       continue
        endif
        return
      endif
  280 nz=-1
      return
      end

      subroutine zkrua2(z,fnu,scale,mr,n,y,nz)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z,y(n),c1,c2,cfn,ck,cone,crsc,cs,cscl,csgn,cspn,czero,
     * phid,rz,s1,s2,sumd,zeta1d,zeta2d,zr,csr(3),css(3),cwrk(16,3),
     * cy(2),phi(2),sum(2),zeta1(2),zeta2(2)
      Dimension bry(3)
      Dimension init(2)
      Logical scale
      Data czero,cone/(0.0d0,0.0d0),(1.0d0,0.0d0)/
      Data pi/3.14159265358979324d0/
      kdflg=1
      nz=0
      cscl=dcmplx(1.0d16,0.0d0)
      crsc=dcmplx(1.0d-16,0.0d0)
      css(1)=cscl
      css(2)=cone
      css(3)=crsc
      csr(1)=crsc
      csr(2)=cone
      csr(3)=cscl
      bry(1)=1.0d-288
      bry(2)=1.0d0/bry(1)
      bry(3)=1.0d308
      x=dble(z)
      zr=z
      if(x.lt.0.0d0) zr=-z
      j=2
      do 40 i=1,n
        j=3-j
        fn=fnu+i-1
        init(j)=0
        call zikrp2(zr,fn,2_4,0_4,init(j),phi(j),zeta1(j),zeta2(j),
     *           sum(j),cwrk(1,j))
        if(.not.scale) then
        s1=zeta1(j)-zeta2(j)
        else
        cfn=dcmplx(fn,0.0d0)
        s1=zeta1(j)-cfn*(cfn/(zr+zeta2(j)))
        endif
        rs1=dble(s1)
        if(abs(rs1).le.700.0d0) then
        if(kdflg.eq.1) kflag=2
        if(abs(rs1).ge.664.0d0) then
           aphi=abs(phi(j))
           rs1=rs1+dlog(aphi)
           if(abs(rs1).gt.700.0d0) then
             goto 20
           else
             if(kdflg.eq.1) kflag=1
             if(rs1.ge.0.0d0) then
               if(kdflg.eq.1) kflag=3
             endif
           endif
        endif
        s2=phi(j)*sum(j)
        c2r=dble(s1)
        c2i=dimag(s1)
        c2m=dexp(c2r)*dble(css(kflag))
        s1=dcmplx(c2m,0.0d0)*dcmplx(cos(c2i),sin(c2i))
        s2=s2*s1
        if(kflag.eq.1) then
           call zpt(s2,nw,bry(1))
           if(nw.ne.0) goto 20
        endif
        cy(kdflg)=s2
        y(i)=s2*csr(kflag)
        if(kdflg.eq.2) then
           goto 60
        else
           kdflg=2
           goto 40
        endif
        endif
   20    if(rs1.gt.0.0d0) then
        goto 280
        elseif(x.lt.0.0d0) then
        goto 280
        else
        kdflg=1
        y(i)=czero
        nz=nz+1
        if(i.ne.1) then
           if(y(i-1).ne.czero) then
             y(i-1)=czero
             nz=nz+1
           endif
        endif
        endif
   40 continue
      i=n
   60 rz=dcmplx(2.0d0,0.0d0)/zr
      ck=dcmplx(fn,0.0d0)*rz
      ib=i+1
      if(n.ge.ib) then
        fn=fnu+n-1
        ipard=1
        if(mr.ne.0) ipard=0
        initd=0
        call zikrp2(zr,fn,2_4,ipard,initd,phid,zeta1d,zeta2d,sumd,
     *           cwrk(1,3))
        if(.not.scale) then
        s1=zeta1d-zeta2d
        else
        cfn=dcmplx(fn,0.0d0)
        s1=zeta1d-cfn*(cfn/(zr+zeta2d))
        endif
        rs1=dble(s1)
        if(abs(rs1).le.700.0d0) then
        if(abs(rs1).ge.664.0d0) then
           aphi=abs(phid)
           rs1=rs1+dlog(aphi)
           if(abs(rs1).ge.700.0d0) goto 100
        endif
        s1=cy(1)
        s2=cy(2)
        c1=csr(kflag)
        ascle=bry(kflag)
        do 80 i=ib,n
           c2=s2
           s2=ck*s2+s1
           s1=c2
           ck=ck+rz
           c2=s2*c1
           y(i)=c2
           if(kflag.lt.3) then
             c2r=dble(c2)
             c2i=dimag(c2)
             c2r=abs(c2r)
             c2i=abs(c2i)
             c2m=max(c2r,c2i)
             if(c2m.gt.ascle) then
               kflag=kflag+1
               ascle=bry(kflag)
               s1=s1*c1
               s2=c2
               s1=s1*css(kflag)
               s2=s2*css(kflag)
               c1=csr(kflag)
             endif
           endif
   80       continue
        goto 140
        endif
  100    if(rs1.gt.0.0d0) then
        goto 280
        elseif(x.lt.0.0d0) then
        goto 280
        else
        nz=n
        do 120 i=1,n
           y(i)=czero
  120       continue
        return
        endif
      endif
  140 if(mr.eq.0) then
        return
      else
        nz=0
        fmr=mr
        sgn=-sign(pi,fmr)
        csgn=dcmplx(0.0d0,sgn)
        inu=int(fnu)
        fnf=fnu-inu
        ifn=inu+n-1
        ang=fnf*sgn
        cpn=cos(ang)
        spn=sin(ang)
        cspn=dcmplx(cpn,spn)
        if(mod(ifn,2).eq.1) cspn=-cspn
        asc=bry(1)
        kk=n
        iuf=0
        kdflg=1
        ib=ib-1
        ic=ib-1
        do 220 k=1,n
        fn=fnu+kk-1
        m=3
        if(n.gt.2) then
           if((kk.eq.n) .and.(ib.lt.n)) then
             goto 160
           elseif((kk.ne.ib) .and.(kk.ne.ic)) then
             initd=0
             goto 160
           endif
        endif
        initd=init(j)
        phid=phi(j)
        zeta1d=zeta1(j)
        zeta2d=zeta2(j)
        sumd=sum(j)
        m=j
        j=3-j
  160       call zikrp2(zr,fn,1_4,0_4,initd,phid,zeta1d,zeta2d,sumd,
     * cwrk(1,m))
        if(.not.scale) then
           s1=-zeta1d+zeta2d
        else
           cfn=dcmplx(fn,0.0d0)
           s1=-zeta1d+cfn*(cfn/(zr+zeta2d))
        endif
        rs1=dble(s1)
        if(abs(rs1).le.700.0d0) then
           if(kdflg.eq.1) iflag=2
           if(abs(rs1).ge.664.0d0) then
             aphi=abs(phid)
             rs1=rs1+dlog(aphi)
             if(abs(rs1).gt.700.0d0) then
               goto 180
             else
               if(kdflg.eq.1) iflag=1
               if(rs1.ge.0.0d0) then
                if(kdflg.eq.1) iflag=3
               endif
             endif
           endif
           s2=csgn*phid*sumd
           c2r=dble(s1)
           c2i=dimag(s1)
           c2m=dexp(c2r)*dble(css(iflag))
           s1=dcmplx(c2m,0.0d0)*dcmplx(cos(c2i),sin(c2i))
           s2=s2*s1
           if(iflag.eq.1) then
             call zpt(s2,nw,bry(1))
             if(nw.ne.0) s2=dcmplx(0.0d0,0.0d0)
           endif
           goto 200
        endif
  180       if(rs1.gt.0.0d0) then
           goto 280
        else
           s2=czero
        endif
  200       cy(kdflg)=s2
        c2=s2
        s2=s2*csr(iflag)
        s1=y(kk)
        if(scale) then
           call zirt(zr,s1,s2,nw,asc,iuf)
           nz=nz+nw
        endif
        y(kk)=s1*cspn+s2
        kk=kk-1
        cspn=-cspn
        if(c2.eq.czero) then
           kdflg=1
        elseif(kdflg.eq.2) then
           goto 240
        else
           kdflg=2
        endif
  220    continue
        k=n
  240    il=n-k
        if(il.ne.0) then
        s1=cy(1)
        s2=cy(2)
        cs=csr(iflag)
        ascle=bry(iflag)
        fn=inu+il
        do 260 i=1,il
           c2=s2
           s2=s1+dcmplx(fn+fnf,0.0d0)*rz*s2
           s1=c2
           fn=fn-1.0d0
           c2=s2*cs
           ck=c2
           c1=y(kk)
           if(scale) then
             call zirt(zr,c1,c2,nw,asc,iuf)
             nz=nz+nw
           endif
           y(kk)=c1*cspn+c2
           kk=kk-1
           cspn=-cspn
           if(iflag.lt.3) then
             c2r=dble(ck)
             c2i=dimag(ck)
             c2r=abs(c2r)
             c2i=abs(c2i)
             c2m=max(c2r,c2i)
             if(c2m.gt.ascle) then
               iflag=iflag+1
               ascle=bry(iflag)
               s1=s1*cs
               s2=ck
               s1=s1*css(iflag)
               s2=s2*css(iflag)
               cs=csr(iflag)
             endif
           endif
  260       continue
        endif
        return
      endif
  280 nz=-1
      return
      end


      Real(8) function zlgam(x)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Real(8) lnr2pi
      Data lnr2pi/9.18938533204672742d-1/
      zlgam=0.0d0
      if(x.le.0.0d0) return
      if(x.le.1.0d-17) then
        zlgam=-dlog(x)
        return
      endif
      if(x.gt.15.0d0) then
        if(x.gt.7.7d+7) then
          if(x.gt.2.54d+305) then
            zlgam=1.79d+308
          else
            zlgam=(x-0.5d0)*dlog(x)-x+lnr2pi
          endif
        else
          t=450.0d0/(x*x)-1.0d0
          y=(((+2.002019273379824d-14*t-6.451144077929628d-12)*t+
     *      3.899788998764847d-9)*t-6.165020494506090d-6)*t+
     *      8.332716440657866d-2
          zlgam=(x-0.5d0)*dlog(x)-x+lnr2pi+y/x
        endif
        return
      endif
      m=int(x)
      t=x-dble(m)
      m=m-1
      g=1.0d0
      if(m) 40,100,60
   40 g=g/x
      goto 100
   60 do 80 i=1,m
      g=(x-dble(i))*g
   80 continue
  100 t=2.0d0*t-1.0d0
      y=(((((((((((((((-1.243191705600000d-10*t+
     *  3.622882508800000d-10)*t-4.030909644800000d-10)*t+
     *  1.265236705280000d-9)*t-5.419466096640000d-9)*t+
     *  1.613133578240000d-8)*t-4.620920340480000d-8)*t+
     *  1.387603440435200d-7)*t-4.179652784537600d-7)*t+
     *  1.253148247777280d-6)*t-3.754930502328320d-6)*t+
     *  1.125234962812416d-5)*t-3.363759801664768d-5)*t+
     *  1.009281733953869d-4)*t-2.968901194293069d-4)*t+
     *  9.157859942174304d-4)*t-2.422595384546340d-3
      y=((((y*t+9.040334940477911d-3)*t-1.341185057058971d-2)*t+
     *  1.037033634220705d-1)*t+1.616919872444243d-2)*t+
     *  8.862269254527580d-1
      zlgam=dlog(y*g)
      end


      Complex(8) function zexp(z,ier)
      implicit Real(8) (a-h,o-z)
      implicit Integer(4) (i-n)
      Complex(8) z
      ier=0
      x=dble(z)
      y=dimag(z)
      if(abs(y).gt.8.333d15) then
        ier=1
        zexp=dcmplx(0.0d0,0.0d0)
      else
        cosy=cos(y)
        siny=sin(y)
        if(x.gt.708.0d0) then
          if(cosy.eq.0.0d0) then
            resr=0.0d0
          else
            xplncy=x+dlog(abs(cosy))
            if(xplncy.gt.708.0d0) then
              ier=1
              resr=sign(3.333D307,cosy)
            else
              resr=sign(dexp(xplncy),cosy)
            endif
          endif
          if(siny.eq.0.0d0) then
            resi=0.0d0
          else
            xplnsy=x+dlog(abs(siny))
            if(xplnsy.gt.708.0d0) then
              ier=1
              resi=sign(3.333D307,siny)
            else
              resi=sign(dexp(xplnsy),siny)
            endif
          endif
        else
          expx=dexp(x)
          resr=expx*cosy
          resi=expx*siny
        endif
        zexp=dcmplx(resr,resi)
        if(abs(y).gt.9.128D7) ier=-1
      endif
      end


