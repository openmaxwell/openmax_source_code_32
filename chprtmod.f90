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
MODULE CHPRT

! Data of boundaries, domains, expansions

  USE CHFLG

  SAVE

  CONTAINS

  Subroutine PRTinitialize2D(kP)
! initialize particles
    Implicit none
    Integer, Optional:: kP
    Integer(4) k,k1,k2
    Integer(2) iB
    Real(8) r
    k1=1
    k2=nParticle2D
    if(Present(kP)) then
      if(kP.lt.0) then
        k1=1
        k2=min(-kP,nParticle2D)
      else if(kP.gt.0) then
        k1=min(kP,nParticle2D)
        k2=k1
      end if
    end if
    do k=k1,k2
      tParticle2D(k)%Mass=0.0d0
      iB=tParticle2D(k)%iBnd
      if((iB.lt.1).or.(iB.gt.nBnd)) cycle
      if(tBnd(iB)%nEdge.ne.1) cycle ! only circular particles
      tParticle2D(k)%iColPart=tBnd(tParticle2D(k)%iBnd)%iCol
      tParticle2D(k)%Position(1)=tBndEdg(tBnd(iB)%iEdgeOffset+1)%x
      tParticle2D(k)%Position(2)=tBndEdg(tBnd(iB)%iEdgeOffset+1)%y
      r=tBndEdg(tBnd(iB)%iEdgeOffset+1)%r
      tParticle2D(k)%r=r
      tParticle2D(k)%Mass=tParticle2D(k)%sMass*Pi*r*r
    end do
	end Subroutine PRTinitialize2D

  Subroutine PRTinitialize3D(kP)
! initialize particles
    Implicit none
    Integer, Optional:: kP
    Integer(4) k,k1,k2
    Integer(2) iO
    Real(8) r
    k1=1
    k2=nParticle3D
    if(Present(kP)) then
      if(kP.lt.0) then
        k1=1
        k2=min(-kP,nParticle3D)
      else if(kP.gt.0) then
        k1=min(kP,nParticle3D)
        k2=k1
      end if
    end if
    do k=k1,k2
      tParticle3D(k)%Mass=0.0d0
      iO=tParticle3D(k)%iObj
      if((iO.lt.1).or.(iO.gt.nObj)) cycle
      if(tObj(iO)%iTypO.ne.0_2) cycle ! only torus type (spherical) particles
      tParticle3D(k)%iColPart=tBnd(tOBJ(k)%iPar(1))%iCol
      tParticle3D(k)%Position(1:3)=tObj(k)%Plane(1:3,0)
      r=tBndEdg(tBnd(tOBJ(k)%iPar(1))%iEdgeOffset+1)%r
      tParticle3D(k)%r=r
      tParticle3D(k)%Mass=4.0d0*tParticle3D(k)%sMass*Pi*r*r*r/3.0d0
    end do
	end Subroutine PRTinitialize3D

  Subroutine PRTgetForce2D()
! get force on particles
    Implicit none
    Integer(4) k,i1,i2,i3
    Real(8) f(2),vl,a1,a2
    Logical l
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    call PRTinitialize2D()
    lAskQ=.false.
    do k=1,nParticle2D
      if(abs(tParticle2D(k)%Mass).lt.pSmall) cycle
! prepare boundary integration
      i1=iBndInt
      i2=IntWhat
      i3=IntField
      l=lIntgSave
! integrate fx
      iBndInt=tParticle2D(k)%iBnd
      IntWhat=-1
      IntField=4
      lIntgSave=.false.
      call BndIntg(.true.)
      f(1)=currentIntegral
! integrate fy
      iBndInt=tParticle2D(k)%iBnd
      IntWhat=-2
      IntField=4
      lIntgSave=.false.
      call BndIntg(.true.)
      f(2)=currentIntegral
! reset integration parameters
      iBndInt=i1
      IntWhat=i2
      IntField=i3
      lIntgSave=l
! compute force, including friction
      tParticle2D(k)%Force(1:2)=f(1:2)
      vl=r2Vec_Length(tParticle2D(k)%Velocity(1:2))
      a1=tParticle2D(k)%Friction(1)*vl
      a2=tParticle2D(k)%Friction(2)*vl*vl
      tParticle2D(k)%Force(1:2)=tParticle2D(k)%Force(1:2)-a1*tParticle2D(k)%Velocity(1:2)-a2*tParticle2D(k)%Velocity(1:2)
      a1=CHrnd(-1.0d0,1.0d0,0_4)*tParticle2D(k)%RandomForce
      a2=CHrnd(-1.0d0,1.0d0)*tParticle2D(k)%RandomForce
      tParticle2D(k)%Force(1)=tParticle2D(k)%Force(1)+a1
      tParticle2D(k)%Force(2)=tParticle2D(k)%Force(2)+a2
    end do
    lAskQ=.true.
	end Subroutine PRTgetForce2D

  Subroutine PRTgetForce3D()
! get force on particles
    Implicit none
    Integer(4) k,i1,i2,i3
    Real(8) f(3),vl,a1,a2,a3
    Logical l
    Logical lStopInt,lAskQ
    Common/StopInt/lStopInt,lAskQ
    call PRTinitialize3D()
    lAskQ=.false.
    do k=1,nParticle3D
      if(abs(tParticle3D(k)%Mass).lt.pSmall) cycle
! prepare object integration
      i1=iObjInt
      i2=IntWhat
      i3=IntField
      l=lIntgSave
! integrate fx
      iObjInt=tParticle3D(k)%iObj
      IntWhat=-1
      IntField=4
      lIntgSave=.false.
      iIntgEc=0_2
      iIntgHc=0_2
      iIntgEc=0_2
      iIntgHc=0_2
      call ObjIntg(.true.)
      f(1)=currentIntegral
! integrate fy
      iObjInt=tParticle3D(k)%iObj
      IntWhat=-2
      IntField=4
      lIntgSave=.false.
      call ObjIntg(.true.)
      f(2)=currentIntegral
! integrate fz
      iObjInt=tParticle3D(k)%iObj
      IntWhat=-3
      IntField=4
      lIntgSave=.false.
      iIntgEc=0_2
      iIntgHc=0_2
      call ObjIntg(.true.)
      f(3)=currentIntegral
! reset integration parameters
      iObjInt=i1
      IntWhat=i2
      IntField=i3
      lIntgSave=l
! compute force, including friction
      tParticle3D(k)%Force(1:3)=f(1:3)
      vl=r2Vec_Length(tParticle3D(k)%Velocity(1:3))
      a1=tParticle3D(k)%Friction(1)*vl
      a2=tParticle3D(k)%Friction(2)*vl*vl
      tParticle3D(k)%Force(1:3)=tParticle3D(k)%Force(1:3)-a1*tParticle3D(k)%Velocity(1:3)-a2*tParticle3D(k)%Velocity(1:3)
      a1=CHrnd(-1.0d0,1.0d0,0_4)*tParticle3D(k)%RandomForce
      a2=CHrnd(-1.0d0,1.0d0)*tParticle3D(k)%RandomForce
      a3=CHrnd(-1.0d0,1.0d0)*tParticle3D(k)%RandomForce
      tParticle3D(k)%Force(1)=tParticle3D(k)%Force(1)+a1
      tParticle3D(k)%Force(2)=tParticle3D(k)%Force(2)+a2
      tParticle3D(k)%Force(3)=tParticle3D(k)%Force(3)+a3
    end do
    lAskQ=.true.
	end Subroutine PRTgetForce3D

  Subroutine PRTgetAcceleration2Dmax(amax)
! max. acceleration of particles after 1 time step
    Implicit none
    Integer(4) k
    Real(8) amax
    call PRTgetForce2D()
    amax=0.0d0
    do k=1,nParticle2D
      tParticle2D(k)%Acceleration(1:2)=tParticle2D(k)%Force(1:2)/tParticle2D(k)%Mass
      amax=max(amax,r2Vec_Length(tParticle2D(k)%Acceleration(1:2)))
    end do
	end Subroutine PRTgetAcceleration2Dmax

  Subroutine PRTgetAcceleration3Dmax(amax)
! max. acceleration of particles after 1 time step
    Implicit none
    Integer(4) k
    Real(8) amax
    call PRTgetForce3D()
    amax=0.0d0
    do k=1,nParticle3D
      tParticle3D(k)%Acceleration(1:3)=tParticle3D(k)%Force(1:3)/tParticle3D(k)%Mass
      amax=max(amax,r3Vec_Length(tParticle3D(k)%Acceleration(1:3)))
    end do
	end Subroutine PRTgetAcceleration3Dmax

  Subroutine PRTgetVelocity2D()
! velocity of particles
    Implicit none
    Integer(4) k
    call PRTgetForce2D()
    do k=1,nParticle2D
      tParticle2D(k)%Acceleration(1:2)=tParticle2D(k)%Force(1:2)/tParticle2D(k)%Mass
      tParticle2D(k)%Velocity(1:2)=tParticle2D(k)%Velocity(1:2)+ParticleTimeStep*tParticle2D(k)%Acceleration(1:2)
    end do
	end Subroutine PRTgetVelocity2D

  Subroutine PRTgetVelocity3D()
! velocity of particles
    Implicit none
    Integer(4) k
    call PRTgetForce3D()
    do k=1,nParticle3D
      tParticle3D(k)%Acceleration(1:3)=tParticle3D(k)%Force(1:3)/tParticle3D(k)%Mass
      tParticle3D(k)%Velocity(1:3)=tParticle3D(k)%Velocity(1:3)+ParticleTimeStep*tParticle3D(k)%Acceleration(1:3)
    end do
	end Subroutine PRTgetVelocity3D

  Subroutine PRTgetPosition2D(ix)
! new position of particles
! when ix>0: keep x coordinate of particle ix and move other particles with respect to this
    Implicit none
    Integer(4) ix,k,i,ncoll,kmin,imin
    Real(8) vt,vn,t,tmin,tremin,PTS
    PTS=ParticleTimeStep
    if(ParticleStepLength.lt.1.0d100) then ! set time step from step length
      if(ParticleStepLength.gt.0.0d0) then
        tParticle2D(1:nParticle2D)%Velocity(1)=0.0d0
        tParticle2D(1:nParticle2D)%Velocity(2)=0.0d0
      end if
      call PRTgetAcceleration2Dmax(vt)
      ParticleTimeStep=dsqrt(ParticleStepLength/vt)
    end if
    call PRTgetVelocity2D()
! check particle collisions and move
    tremin=ParticleTimeStep
    do
      ncoll=0
      tmin=2.0d0*pBig
      kmin=0_4
      imin=0_4
      do k=1,nParticle2D ! collisions with mirror
        call PRTgetMCollisionTime2D(k,t)
        if(tremin.lt.t) Cycle
        ncoll=ncoll+1
        if(t.lt.tmin) then
          tmin=t
          kmin=k
        end if
      end do
      do k=1,nParticle2D-1 ! collisions of 2 particles
        do i=k+1,nParticle2D
          call PRTgetCollisionTime2D(k,i,t)
          if(tremin.lt.t) Cycle
          ncoll=ncoll+1
          if(t.lt.tmin) then
            tmin=t
            kmin=k
            imin=i
          end if
        end do
      end do
      if(ncoll.gt.0) then ! handle first collision
        call PRTmove2D(tmin,ix) ! move to collision point and adapt velocities
        if(imin.eq.0) then ! collision of particle kmin with mirror -> invert vn
          vt=dot_product(tParticle2D(kmin)%Velocity(1:2),Particle2DMirror(2,1:2))
          vn=-dot_product(tParticle2D(kmin)%Velocity(1:2),Particle2DMirror(3,1:2))
          tParticle2D(kmin)%Velocity(1:2)=vt*Particle2DMirror(2,1:2)+vn*Particle2DMirror(3,1:2)
        else
          call PRTCollision2D(kmin,imin)
        end if
        tremin=tremin-tmin
      else
        call PRTmove2D(tremin,ix) ! move to end of time interval and exit
        Exit
      end if
    end do
    ParticleTimeStep=PTS
	end Subroutine PRTgetPosition2D

  Subroutine PRTgetPosition3D(ix)
! new position of particles
! when ix>0: keep x coordinate of particle ix and move other particles with respect to this
    Implicit none
    Integer(4) ix,k,i,ncoll,kmin,imin
    Real(8) vt,vu,vn,t,tmin,tremin,PTS
    call PRTgetVelocity3D()
! reduce time step when too long space steps would be encountered
    PTS=ParticleTimeStep
    if(ParticleStepLength.lt.1.0d100) then ! set time step from step length
      if(ParticleStepLength.gt.0.0d0) then
        tParticle3D(1:nParticle3D)%Velocity(1)=0.0d0
        tParticle3D(1:nParticle3D)%Velocity(2)=0.0d0
        tParticle3D(1:nParticle3D)%Velocity(3)=0.0d0
      end if
      call PRTgetAcceleration3Dmax(vt)
      ParticleTimeStep=dsqrt(ParticleStepLength/vt)
    end if
    call PRTgetVelocity3D()
! check particle collisions and move
    tremin=ParticleTimeStep
    do
      ncoll=0
      tmin=2.0d0*pBig
      kmin=0_4
      imin=0_4
      do k=1,nParticle3D ! collisions with mirror
        call PRTgetMCollisionTime3D(k,t)
        if(tremin.lt.t) Cycle
        ncoll=ncoll+1
        if(t.lt.tmin) then
          tmin=t
          kmin=k
        end if
      end do
      do k=1,nParticle3D-1 ! collisions of 2 particles
        do i=k+1,nParticle3D
          call PRTgetCollisionTime3D(k,i,t)
          if(tremin.lt.t) Cycle
          ncoll=ncoll+1
          if(t.lt.tmin) then
            tmin=t
            kmin=k
            imin=i
          end if
        end do
      end do
      if(ncoll.gt.0) then ! handle first collision
        call PRTmove3D(tmin,ix) ! move to collision point and adapt velocities
        if(imin.eq.0) then ! collision of particle kmin with mirror -> invert vn
          vt=dot_product(tParticle3D(kmin)%Velocity(1:3),Particle3DMirror(2,1:3))
          vu=dot_product(tParticle3D(kmin)%Velocity(1:3),Particle3DMirror(3,1:3))
          vn=-dot_product(tParticle3D(kmin)%Velocity(1:3),Particle3DMirror(4,1:3))
          tParticle3D(kmin)%Velocity(1:3)=vt*Particle3DMirror(2,1:3)+vu*Particle3DMirror(3,1:3)+vn*Particle3DMirror(4,1:3)
        else
          call PRTCollision3D(kmin,imin)
        end if
        tremin=tremin-tmin
      else
        call PRTmove3D(tremin,ix) ! move to end of time interval and exit
        Exit
      end if
    end do
    ParticleTimeStep=PTS
	end Subroutine PRTgetPosition3D

  Subroutine PRTgetCollisionTime2D(k1,k2,t)
! collision of 2 particles: get collision time
    Implicit none
    Integer(4) k1,k2
    Real(8) d,dp(2),dv(2),a,b,c,t1,t2,t
    t=2.0d0*pBig
    dv(1:2)=tParticle2D(k1)%Velocity(1:2)-tParticle2D(k2)%Velocity(1:2)
    a=dv(1)**2+dv(2)**2
    if(a.lt.1.0d-100) return
    d=tParticle2D(k1)%r+tParticle2D(k2)%r
    dp(1:2)=tParticle2D(k1)%Position(1:2)-tParticle2D(k2)%Position(1:2)
    b=2.0d0*(dp(1)*dv(1)+dp(2)*dv(2))
    c=dp(1)**2+dp(2)**2-d**2
    d=b**2-4.0d0*a*c
    if(d.lt.0.0d0) return
    t1=(-b+dsqrt(d))/(2.0d0*a)
    t2=(-b-dsqrt(d))/(2.0d0*a)
    if((t1.lt.0.0d0).and.(t2.lt.0.0d0)) return ! no collision
    if(t1.lt.0.0d0) then
      t=t2
    else if(t2.lt.0.0d0) then
      t=t1
    else
      t=min(t1,t2)
    end if
    if(abs(t).lt.1.0d-100) t=2.0d0*pBig
	end Subroutine PRTgetCollisionTime2D

  Subroutine PRTgetCollisionTime3D(k1,k2,t)
! collision of 2 particles: get collision time
    Implicit none
    Integer(4) k1,k2
    Real(8) d,dp(3),dv(3),a,b,c,t1,t2,t
    t=2.0d0*pBig
    dv(1:3)=tParticle3D(k1)%Velocity(1:3)-tParticle3D(k2)%Velocity(1:3)
    a=dv(1)**2+dv(2)**2+dv(3)**2
    if(a.lt.1.0d-100) return
    d=tParticle3D(k1)%r+tParticle3D(k2)%r
    dp(1:3)=tParticle3D(k1)%Position(1:3)-tParticle3D(k2)%Position(1:3)
    b=2.0d0*(dp(1)*dv(1)+dp(2)*dv(2)+dp(3)*dv(3))
    c=dp(1)**2+dp(2)**2+dp(3)**2-d**2
    d=b**2-4.0d0*a*c
    if(d.lt.0.0d0) return
    t1=(-b+dsqrt(d))/(2.0d0*a)
    t2=(-b-dsqrt(d))/(2.0d0*a)
    if((t1.lt.0.0d0).and.(t2.lt.0.0d0)) return ! no collision
    if(t1.lt.0.0d0) then
      t=t2
    else if(t2.lt.0.0d0) then
      t=t1
    else
      t=min(t1,t2)
    end if
    if(abs(t).lt.1.0d-100) t=2.0d0*pBig
	end Subroutine PRTgetCollisionTime3D

  Subroutine PRTgetMCollisionTime2D(k1,t)
! collision of particle with mirror: get collision time
    Implicit none
    Integer(4) k1
    Real(8) dn,vn,r,t
    dn=dot_product(tParticle2D(k1)%Position(1:2),Particle2DMirror(3,1:2))
    dn=dn-dot_product(Particle2DMirror(1,1:2),Particle2DMirror(3,1:2))
    vn=dot_product(tParticle2D(k1)%Velocity(1:2),Particle2DMirror(3,1:2))
    r=tParticle2D(k1)%r
    if(abs(vn).lt.1.0d-100) then
      t=pBig
    else if((dn.gt.r).and.(vn.lt.0.0d0)) then
      t=(r-dn)/vn
    else if((dn.lt.-r).and.(vn.gt.0.0d0)) then
      t=(r-dn)/vn
    else
      t=2.0d0*pBig
    end if
	end Subroutine PRTgetMCollisionTime2D

  Subroutine PRTgetMCollisionTime3D(k1,t)
! collision of particle with mirror: get collision time
    Implicit none
    Integer(4) k1
    Real(8) dn,vn,r,t
    dn=dot_product(tParticle3D(k1)%Position(1:3),Particle3DMirror(4,1:3))
    dn=dn-dot_product(Particle3DMirror(1,1:3),Particle3DMirror(4,1:3))
    vn=dot_product(tParticle3D(k1)%Velocity(1:3),Particle3DMirror(4,1:3))
    r=tParticle3D(k1)%r
    if(abs(vn).lt.1.0d-100) then
      t=pBig
    else if((dn.gt.r).and.(vn.lt.0.0d0)) then
      t=(r-dn)/vn
    else if((dn.lt.-r).and.(vn.gt.0.0d0)) then
      t=(r-dn)/vn
    else
      t=2.0d0*pBig
    end if
	end Subroutine PRTgetMCollisionTime3D

  Subroutine PRTCollision2D(k1,k2)
! collision of 2 particles: get new velocities
    Implicit none
    Integer(4) k1,k2
    Real(8) et(2),en(2),v1,v2,v1n,v2n,m1,m2
    en(1:2)=Unit2DVec(tParticle2D(k2)%Position(1:2)-tParticle2D(k1)%Position(1:2))
    et(1)=en(2)
    et(2)=-en(1)
    v1=dot_product(tParticle2D(k1)%Velocity(1:2),en(1:2))
    v2=dot_product(tParticle2D(k2)%Velocity(1:2),en(1:2))
    if((v1-v2).lt.0.0d0) return
    m1=tParticle2D(k1)%Mass
    m2=tParticle2D(k2)%Mass
    v1n=((m1-m2)*v1+2.0d0*m2*v2)/(m1+m2)
    v2n=((m2-m1)*v2+2.0d0*m1*v1)/(m1+m2)
    v1=dot_product(tParticle2D(k1)%Velocity(1:2),et(1:2))
    v2=dot_product(tParticle2D(k2)%Velocity(1:2),et(1:2))
    tParticle2D(k1)%Velocity(1:2)=v1*et(1:2)+v1n*en(1:2)
    tParticle2D(k2)%Velocity(1:2)=v2*et(1:2)+v2n*en(1:2)
	end Subroutine PRTCollision2D

  Subroutine PRTCollision3D(k1,k2)
! collision of 2 particles: get new velocities
    Implicit none
    Integer(4) k1,k2
    Real(8) et(3),eu(3),en(3),u1,u2,v1,v2,v1n,v2n,m1,m2
    en(1:3)=Unit3DVec(tParticle3D(k2)%Position(1:3)-tParticle3D(k1)%Position(1:3))
    et(1)=CHRnd(-1.0d0,1.0d0,0_4)
    et(2)=CHRnd(-1.0d0,1.0d0)
    et(3)=CHRnd(-1.0d0,1.0d0)
    eu(1)=CHRnd(-1.0d0,1.0d0)
    eu(2)=CHRnd(-1.0d0,1.0d0)
    eu(3)=CHRnd(-1.0d0,1.0d0)
    call Ortho3DSpace3(en,et,eu)
    v1=dot_product(tParticle3D(k1)%Velocity(1:3),en(1:3))
    v2=dot_product(tParticle3D(k2)%Velocity(1:3),en(1:3))
    if((v1-v2).lt.0.0d0) return
    m1=tParticle3D(k1)%Mass
    m2=tParticle3D(k2)%Mass
    v1n=((m1-m2)*v1+2.0d0*m2*v2)/(m1+m2)
    v2n=((m2-m1)*v2+2.0d0*m1*v1)/(m1+m2)
    v1=dot_product(tParticle3D(k1)%Velocity(1:3),et(1:3))
    v2=dot_product(tParticle3D(k2)%Velocity(1:3),et(1:3))
    u1=dot_product(tParticle3D(k1)%Velocity(1:3),eu(1:3))
    u2=dot_product(tParticle3D(k2)%Velocity(1:3),eu(1:3))
    tParticle3D(k1)%Velocity(1:3)=v1*et(1:3)+u1*eu(1:3)+v1n*en(1:3)
    tParticle3D(k2)%Velocity(1:3)=v2*et(1:3)+u2*eu(1:3)+v2n*en(1:3)
	end Subroutine PRTCollision3D

  Subroutine PRTmove2D(t,ix)
! move particles and mirror objects
    Implicit none
    Integer(4) ix,k
    Real(8) t,v(2),vt,vn,vix(2)
    vix(1:2)=0.0d0
    if(ix.gt.0) then
      vix(1)=t*tParticle2D(ix)%Velocity(1)
    end if
    do k=1,nParticle2D
      v(1:2)=t*tParticle2D(k)%Velocity(1:2)-vix(1:2)
      tParticle2D(k)%Position(1:2)=tParticle2D(k)%Position(1:2)+v(1:2)
      if(tParticle2D(k)%iColPart.gt.-1_2) call MoveColor(Int4(tParticle2D(k)%iColPart),v(1),v(2))
      vt=r2Scl_Prod(v,Particle2DMirror(2,1:2))
      vn=r2Scl_Prod(v,Particle2DMirror(3,1:2))
      v=vt*Particle2DMirror(2,1:2)-vn*Particle2DMirror(3,1:2)
      if(tParticle2D(k)%iColMirror.gt.-1_2) call MoveColor(Int4(tParticle2D(k)%iColMirror),v(1),v(2))
      v=vt*Particle2DMirror(2,1:2)
      if(tParticle2D(k)%iColSurface.gt.-1_2) call MoveColor(Int4(tParticle2D(k)%iColSurface),v(1),v(2))
    end do
  end Subroutine PRTmove2D

  Subroutine PRTmove3D(t,ix)
! move particles and mirror objects !!!!!!!!!!! to be corrected !!!!!!!!!!!!!
    Implicit none
    Integer(4) ix,k,i
    Real(8) t,v(3),vt,vu,vn,vix(3)
    vix(1:3)=0.0d0
    if(ix.gt.0) then
      vix(1)=t*tParticle3D(ix)%Velocity(1)
    end if
    do k=1,nParticle3D
      v(1:3)=t*tParticle3D(k)%Velocity(1:3)-vix(1:3)
      tParticle3D(k)%Position(1:3)=tParticle3D(k)%Position(1:3)+v(1:3)
      tObj(tParticle3D(k)%iObj)%Plane(1:3,0)=tObj(tParticle3D(k)%iObj)%Plane(1:3,0)+v(1:3)
      if(tParticle3D(k)%iColPart.gt.-1_2) then
        do i=1,nExp
          if(tParticle3D(k)%iColPart.ne.tExp(i)%iCol) Cycle
          tExp(i)%Plane(1:3,0)=tExp(i)%Plane(1:3,0)+v(1:3)
        end do
      end if
      vt=r3Scl_Prod(v,Particle3DMirror(2,1:3))
      vu=r3Scl_Prod(v,Particle3DMirror(3,1:3))
      vn=r3Scl_Prod(v,Particle3DMirror(4,1:3))
      v=vt*Particle3DMirror(2,1:3)+vu*Particle3DMirror(3,1:3)-vn*Particle3DMirror(4,1:3)
      if(tParticle3D(k)%iColMirror.gt.-1_2) then 
        do i=1,nExp
          if(tParticle3D(k)%iColPart.ne.tExp(i)%iCol) Cycle
          tExp(i)%Plane(1:3,0)=tExp(i)%Plane(1:3,0)+v(1:3)
        end do
      end if
      v=vt*Particle3DMirror(2,1:3)+vu*Particle3DMirror(3,1:3)
      if(tParticle3D(k)%iColSurface.gt.-1_2) then
        do i=1,nExp
          if(tParticle3D(k)%iColPart.ne.tExp(i)%iCol) Cycle
          tExp(i)%Plane(1:3,0)=tExp(i)%Plane(1:3,0)+v(1:3)
        end do
        do i=1,nObj
          if(tParticle3D(k)%iColPart.ne.tObj(i)%iCol) Cycle
          tObj(i)%Plane(1:3,0)=tObj(i)%Plane(1:3,0)+v(1:3)
        end do
      end if
    end do
  end Subroutine PRTmove3D

END MODULE CHPRT




