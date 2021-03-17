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
MODULE CHFOR

  USE CHWIN

  SAVE

  CONTAINS

  Subroutine GetFormulaParts(s,s1,s2,l1,l2)
    Implicit none
	  Integer(4) lf,l,l1,l2
    Character(*) s
    Character(1151) s1,s2
    lf=-1
    call DelBlanks(s,lf)
    l1=1
    l2=1
    do l=2,lf
      if(s(l:l).eq.'/') Exit
      s1(l1:l1)=s(l:l)
      l1=l1+1
    end do
    s1(l1:l1)=char(0)
    do l=l1+2,lf
      if(s.eq.'/') Exit
      s2(l2:l2)=s(l:l)
      l2=l2+1
    end do
    s2(l2:l2)=char(0)
    l1=l1-1
    l2=l2-1
  end Subroutine GetFormulaParts

  Subroutine CheckFormula(s,lf,nc,np,nv,iErr)
! check the formula string s
    Implicit none
	  Integer(4) nc,np,nv,iErr,lf,l1,l2,i1,i2
    Character(*) s
    Character(1151) s1,s2
    pForm=1.0d0
    vForm=1.0d0
    if(s(1:1).eq.'/') then ! two formulae packed in one string with "/" delimiters!
      iErr=0
      call GetFormulaParts(s,s1,s2,l1,l2)
      lf=l1+l2
      call checkFormula1(s1,l1,nc,np,nv,i1)
      if(i1.ne.0) then
        iErr=1
        s(1:1)='!'
      end if
      call checkFormula1(s2,l2,nc,np,nv,i2)
      if(i1.ne.0) then
        iErr=iErr+2
        s(l1+2:l1+2)='!'
      end if
    else
      lf=-1
      dFormu=Formula(s,lf,cForm,pForm,vForm,nc,np,nv,1,1,1,iErr)
    end if
	end Subroutine CheckFormula

  Subroutine CheckFormula1(s,lf,nc,np,nv,iErr)
! check the formula string s
    Implicit none
	  Integer(4) nc,np,nv,iErr,lf
    Character(*) s
    pForm=1.0d0
    vForm=1.0d0
    if(s(1:1).eq.'/') return ! more than two formulae packed in one string with "/" delimiters!
    lf=-1
    dFormu=Formula(s,lf,cForm,pForm,vForm,nc,np,nv,1,1,1,iErr)
	end Subroutine CheckFormula1

  recursive Subroutine CheckCFormula(s,lf,nc,np,nv,iErr)
! check the formula string s of a complex formula
    Implicit none
	  Integer(4) nc,np,nv,iErr,lf,l1,l2,i1,i2
    Character(*) s
    Character(1151) s1,s2
    vCForm=DCmplx(1.0d0,0.0d0)
    if(s(1:1).eq.'/') then ! two formulae packed in one string with "/" delimiters!
      iErr=0
      call GetFormulaParts(s,s1,s2,l1,l2)
      lf=l1+l2
      call checkCFormula1(s1,l1,nc,np,nv,i1)
      if(i1.ne.0) then
        iErr=1
        s(1:1)='!'
      end if
      call checkCFormula1(s2,l2,nc,np,nv,i2)
      if(i1.ne.0) then
        iErr=iErr+2
        s(l1+2:l1+2)='!'
      end if
    else
      lf=-1
      cFormu=CFormula(s,lf,cCForm,pCForm,vCForm,nc,np,nv,1,1,1,iErr)
    end if
	end Subroutine CheckCFormula
	
  Subroutine CheckCFormula1(s,lf,nc,np,nv,iErr)
! check the formula string s of a complex formula
    Implicit none
	  Integer(4) nc,np,nv,iErr,lf
    Character(*) s
    vCForm=DCmplx(1.0d0,0.0d0)
    if(s(1:1).eq.'/') return ! more than two formulae packed in one string with "/" delimiters!
    lf=-1
    cFormu=CFormula(s,lf,cCForm,pCForm,vCForm,nc,np,nv,1,1,1,iErr)
	end Subroutine CheckCFormula1

  Function Formula(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArrOi,iArrVarOi,nArrVi) Result(aRes)
    Implicit none
    Integer(4) lFor,mC,mP,mV,nx,ny,nz,iErr,lf,nArr,nArrV,iArrVar,i
    Integer(4), Optional :: nArrOi,nArrVi,iArrVarOi
    Real(8) Cns(0:mC),Par(0:mP),Var(-10:mV)
    Real(8), Optional :: vArr(mForA)
    Real(8) aRes(mForA)
    Logical l4in
    Character(*) sFor
    nArrV=1
    nArr=1
    iArrVar=1
    if(Present(nArrVi)) nArrV=nArrVi
    if(Present(nArrOi)) nArr=max(min(mForA/nArrV,nArrOi),1)
    if(Present(iArrVarOi)) iArrVar=max(min(min(iArrVarOi,nArr),mV),-10)
    if(sFor(1:1).eq.'!') then
      aRes(1:nArr)=0.0d0
      iErr=0
    else
      l4in=l4
      l4=.false.
      if(Present(vArr)) then
        aRes(1:nArr)=Formul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
      else
        aRes(1:nArr)=Formul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
      end if
      l4=l4in
      if(iErr.ne.0) then
        lf=-1
        call DelBlanks(sFor,lf)
        if(lf.ne.lFor) then
          if(l4) write(*,*) 'String length error lf, lFor=',lf,lFor
        end if
        if(l4) write(*,*) 'Error in string X',sFor(1:lFor),'X'
        if(l4) write(*,*) ' '
        lFor=-1
        if(Present(vArr)) then
          aRes(1:nArr)=Formul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
        else
          aRes(1:nArr)=Formul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
        end if
      end if
      if(iErr.ne.0) then
        aRes(1:nArr)=0.0d0
        lFor=Min(lFor+1,1150)
        do i=lFor,2,-1
          sFor(i:i)=sFor(i-1:i-1)
        end do
        sFor(1:1)='!'
      end if
    end if
  end Function Formula

  Function CFormula(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArrOi,iArrVarOi,nArrVi) Result(aRes)
    Implicit none
    Integer(4) lFor,mC,mP,mV,nx,ny,nz,iErr,nArr,nArrV,iArrVar,i
    Integer(4), Optional :: nArrOi,nArrVi,iArrVarOi
    Complex(8) Cns(0:mC),Par(0:mP),Var(-10:mV)
    Complex(8), Optional :: vArr(mForA)
    Complex(8) aRes(mForA)
    Character(*) sFor
    nArrV=1
    nArr=1
    iArrVar=1
    if(Present(nArrVi)) nArrV=nArrVi
    if(Present(nArrOi)) nArr=max(min(mForA/nArrV,nArrOi),1)
    if(Present(iArrVarOi)) iArrVar=max(min(min(iArrVarOi,nArr),mV),-10)
    if(sFor(1:1).eq.'!') then
      aRes(1:nArr)=(0.0d0,0.0d0)
      iErr=0
    else
      if(Present(vArr)) then
        aRes(1:nArr)=CFormul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
      else
        aRes(1:nArr)=CFormul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
      end if
      if(iErr.ne.0) then
        aRes(1:nArr)=(0.0d0,0.0d0)
        lFor=Min(lFor+1,1150)
        do i=lFor,2,-1
          sFor(i:i)=sFor(i-1:i-1)
        end do
        sFor(1:1)='!'
      end if
    end if
  end Function CFormula

  Recursive Function Formul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArrOi,iArrVarOi,nArrVi) Result(aRes)
    Implicit none
    Integer(4) mC,mP,mV,nx,ny,nz,iErr,i,k,nn1,lArg,lo,lArgs,lFor,nArrV,nArr,iArrVar,ls,li,nForA,nnz,iEr,ir,k0,ixyz, &
    & ia,ib,ic
    Complex(8) cwrk(1),cb(1)
    Real(8) Val(mForA,mForA),Cns(0:mC),Par(0:mP),Var(-10:mV),aRes(mForA),r,r1,r2
    Real(8), Optional :: vArr(mForA)
    Real(8) Rando
    Integer(4), Optional :: nArrOi,nArrVi,iArrVarOi
    Logical lArr
    Character(*) sFor
    Character(1151) s,sArgs,sArg
    Character(5) sPot
    Character(4) sFkt
    Character(1) sB
    Integer(4) i1Rnd
    Data i1Rnd/0_4/
    nArrV=1
    nArr=1
    iArrVar=1
    if(Present(nArrVi)) nArrV=nArrVi
    if(Present(nArrOi)) nArr=max(min(mForA/nArrV,nArrOi),1)
    if(Present(iArrVarOi)) iArrVar=max(min(min(iArrVarOi,nArr),mV),-10)
    lArr=.false.
    if(Present(vArr)) lArr=.true.
! default result
    iErr=0
    aRes(1:nArr)=0.0_8
    if(lFor.gt.1150) then
      if(lForWarn) then
        if(l4) write(*,*) 'ERROR in Formula: string too long! max.length=',1150
        if(l4) write(*,*) 'original string=',sFor
        if(l4) write(*,*) 'lFor=',lFor
      end if
      iErr=1
      return
    end if
! prepare string
    if(lFor.lt.1) then
      call DelBlanks(sFor,lFor)
      call ToLower(sFor,lFor)
    end if
    if(lFor.lt.1) return
    ls=lFor
    s(1:lFor)=sFor(1:lFor)
    s(lFor+1:lFor+1)=Char(0)
! check if function or value
    if(lFor.gt.4) then
      sB(1:1)=sFor(4:4)
    else
      sB(1:1)='v'
    end if
    if(sB(1:1).eq.'(') then
! functions: determine Type and argument string, compute
      sFkt(1:3)=sFor(1:3)
      lArgs=lFor-5
      sArgs(1:lArgs)=sFor(5:lFor-1)
      sArgs(lArgs+1:lArgs+1)=char(0)
      if(lArgs.lt.1) then
        if(lForWarn) then
          if(l4) write(*,*) 'ERROR in Formula: No argument found!'
          if(l4) write(*,*) 'argument string=',sArgs(1:lArgs)
        end if
        iErr=1
        return
      end if
! user-defined operators
      if(sFkt(1:1).eq.'u') then
        call StrToInt(sFkt(2:3),kUserForm,iErr) ! get number of operator
        if((kUserForm.gt.nUserForm).or.(kUserForm.lt.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: user-defined formula not defined!'
            if(l4) write(*,*) 'formula number=',kUserForm
          end if
          iErr=3
          return
        end if
        if(lUserForm(kUserForm).lt.1) then
          call DelBlanks(sUserForm(kUserForm),lUserForm(kUserForm))
          call ToLower(sUserForm(kUserForm),lUserForm(kUserForm))
        end if
        lo=0 ! insert argument strings in the operator string
        do li=1,lUserForm(kUserForm)
          if(sUserForm(kUserForm)(li:li).eq.'?') then ! insert argument string
            call GetArg1(sArgs,lArgs,sArg,lArg,iErr)
            if(iErr.ne.0) then
              if(lForWarn) then
                if(l4) write(*,*) 'ERROR in Formula: GetArg1 returned error!'
                if(l4) write(*,*) 'current argument string=',lArgs,sArgs(1:lArgs)
                if(l4) write(*,*) 'original string=',lFor,'X',sFor(1:lFor),'X'
              end if
              iErr=1
              return
            end if
            lo=lo+lArg
            if(lo.gt.1150) then
              if(lForWarn) then
                if(l4) write(*,*) 'ERROR in Formula: user-defined formula too long!'
                if(l4) write(*,*) 'formula string=',sUserForm(kUserForm)(1:lUserForm(kUserForm))
              end if
              iErr=3
              return
            end if
            sUserFor(lo-lArg+1:lo)=sArg(1:lArg)
          else
            lo=lo+1 ! copy character
            if(lo.gt.1150) then
              if(lForWarn) then
                if(l4) write(*,*) 'ERROR in Formula: user-defined formula too long!'
                if(l4) write(*,*) 'formula string=',sUserForm(kUserForm)(1:lUserForm(kUserForm))
              end if
              iErr=3
              return
            end if
            sUserFor(lo:lo)=sUserForm(kUserForm)(li:li)
          end if
        end do
        lUserFor=lo ! evaluate operator
        if(lArr) then
          aRes=Formul(sUserFor,lUserFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
        else
          aRes=Formul(sUserFor,lUserFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
        end if
        return
      end if
! get all arguments of the formula and evaluate them -> Val
      do i=1,mForA
        Val(1:nArr,i)=0.0d0
        call GetArg1(sArgs,lArgs,sArg,lArg,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: GetArg1 returned error!'
            if(l4) write(*,*) 'current argument string=',lArgs,sArgs(1:lArgs)
            if(l4) write(*,*) 'current argument number=',i
            if(l4) write(*,*) 'original string=',lFor,'X',sFor(1:lFor),'X'
          end if
          iErr=1
          return
        end if
        if(lArr) then
          Val(1:nArr,i)=Formul(sArg,lArg,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
        else
          Val(1:nArr,i)=Formul(sArg,lArg,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
        end if
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Formula returned error!'
            if(l4) write(*,*) 'current argument string=',lArgs,sArgs(1:lArgs)
            if(l4) write(*,*) 'current argument number=',i
            if(l4) write(*,*) 'original string=',lFor,sFor(1:lFor)
          end if
          iErr=1
          return
        end if
        if(lArgs.eq.0) then
          nForA=i
          Exit
        end if
      end do
      nn1=1_4
      Select Case(sFkt(1:3))
! Cases with one argument
      Case('abs')
        do k=1,nArr
          aRes(k)=dabs(Val(k,1))
        end do
      Case('acs')
        do k=1,nArr
          aRes(k)=dacos(Val(k,1))
        end do
      Case('asn')
        do k=1,nArr
          aRes(k)=dasin(Val(k,1))
        end do
      Case('atg')
        do k=1,nArr
          aRes(k)=datan(Val(k,1))
        end do
      Case('neg')
        do k=1,nArr
          aRes(k)=-Val(k,1)
        end do
      Case('inv')
        do k=1,nArr
          aRes(k)=.inv.Val(k,1)
        end do
      Case('sgn')
        do k=1,nArr
          aRes(k)=1.0_8
          if(Val(k,1).lt.0.0_8) aRes(k)=-1.0_8
        end do
      Case('int')
        do k=1,nArr
          aRes(k)=.int.Val(k,1)
        end do
      Case('sqr')
        do k=1,nArr
          aRes(k)=Val(k,1).mul.Val(k,1)
        end do
      Case('srt')
        do k=1,nArr
          aRes(k)=.srt.Val(k,1)
        end do
      Case('log')
        do k=1,nArr
          aRes(k)=.log.Val(k,1)
        end do
      Case('exp')
        do k=1,nArr
          aRes(k)=.exp.Val(k,1)
        end do
      Case('sin')
        do k=1,nArr
          aRes(k)=.sin.Val(k,1)
        end do
      Case('cos')
        do k=1,nArr
          aRes(k)=.cos.Val(k,1)
        end do
      Case('tan')
        do k=1,nArr
          aRes(k)=dtan(Val(k,1))
        end do
      Case('ctg')
        do k=1,nArr
          aRes(k)=dcotan(Val(k,1))
        end do
      Case('csh')
        do k=1,nArr
          aRes(k)=dcosh(Val(k,1))
        end do
      Case('snh')
        do k=1,nArr
          aRes(k)=dsinh(Val(k,1))
        end do
      Case('tnh')
        do k=1,nArr
          aRes(k)=dtanh(Val(k,1))
        end do
! Cases with two arguments
      Case('at2')
        do k=1,nArr
          aRes(k)=datan2(Val(k,1),Val(k,2))
        end do
      Case('bit')
        do k=1,nArr
          if(BTEST(nint(Val(k,1),4),min(31,max(0,nint(Val(k,2),4)-1_4)))) then
            aRes(k)=1.0d0
          else
            aRes(k)=0.0d0
          end if
        end do
      Case('ran')
        Rando=CHrnd(0.0d0,1.0d0,0_4)
        do k=1,nArr
          Rando=CHrnd(Val(k,1),Val(k,2))
          aRes(k)=Rando
        end do
      Case('rnd')
        if(i1Rnd.eq.0) then
          Rando=CHrnd(0.0d0,1.0d0,0_4)
          i1Rnd=1_4
        end if
        do k=1,nArr
          Rando=CHrnd(Val(k,1),Val(k,2))
          aRes(k)=Rando
        end do
      Case('sub')
        do k=1,nArr
          aRes(k)=Val(k,1).sub.Val(k,2)
        end do
      Case('div')
        do k=1,nArr
          aRes(k)=Val(k,1).div.Val(k,2)
        end do
      Case('pow')
        do k=1,nArr
          aRes(k)=Val(k,1).pow.Val(k,2)
        end do
      Case('lga')
        do k=1,nArr
          aRes(k)=(.log.Val(k,2)).div.(.log.Val(k,1))
        end do
      Case('ta1')
        do k=1,nArr
          aRes(k)=Val(k,1)
        end do
      Case('ta2')
        do k=1,nArr
          aRes(k)=Val(k,2)
        end do
      Case('zii')
        do k=1,nArr
          call zir(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dimag(cb(1))
        end do
      Case('zji')
        do k=1,nArr
          call zjr(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dimag(cb(1))
        end do
      Case('zki')
        do k=1,nArr
          call zkr(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dimag(cb(1))
        end do
      Case('zhi')
        do k=1,nArr
          call zhr(nn1,abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dimag(cb(1))
        end do
      Case('zyi')
        do k=1,nArr
          call zyr(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,cwrk(1),ier)
          aRes(k)=Dimag(cb(1))
        end do
      Case('zir')
        do k=1,nArr
          call zir(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dble(cb(1))
        end do
      Case('zjr')
        do k=1,nArr
          call zjr(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dble(cb(1))
        end do
      Case('zkr')
        do k=1,nArr
          call zkr(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dble(cb(1))
        end do
      Case('zhr')
        do k=1,nArr
          call zhr(nn1,abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=Dble(cb(1))
        end do
      Case('zyr')
        do k=1,nArr
          call zyr(abs(Val(k,1)),DCmplx(Val(k,2),0.0d0),nn1,.false.,cb(1),nnz,cwrk(1),ier)
          aRes(k)=Dble(cb(1))
        end do
! Cases with three arguments
      Case('eps')
        do k=1,nArr
          aRes(k)=Dble(getcEps(Val(k,1),nx,ny,nz))
        end do
      Case('mue')
        do k=1,nArr
          aRes(k)=Dble(getcMue(Val(k,1),nx,ny,nz))
        end do
      Case('sig')
        do k=1,nArr
          aRes(k)=Dble(getcSig(Val(k,1),nx,ny,nz))
        end do
      Case('tau')
        do k=1,nArr
          aRes(k)=Dble(getcTau(Val(k,1),nx,ny,nz))
        end do
      Case('kap')
        do k=1,nArr
          aRes(k)=Dble(getcKap(Val(k,1),nx,ny,nz))
        end do
      Case('kkk')
        do k=1,nArr
          aRes(k)=Dble(getcK(Val(k,1),nx,ny,nz))
        end do
      Case('fex')
        do k=1,nArr
          aRes(k)=getdAuxFld(1,Val,nx,ny,nz)
        end do
      Case('fey')
        do k=1,nArr
          aRes(k)=getdAuxFld(2,Val,nx,ny,nz)
        end do
      Case('fez')
        do k=1,nArr
          aRes(k)=getdAuxFld(3,Val,nx,ny,nz)
        end do
      Case('fhx')
        do k=1,nArr
          aRes(k)=getdAuxFld(4,Val,nx,ny,nz)
        end do
      Case('fhy')
        do k=1,nArr
          aRes(k)=getdAuxFld(5,Val,nx,ny,nz)
        end do
      Case('fhz')
        do k=1,nArr
          aRes(k)=getdAuxFld(6,Val,nx,ny,nz)
        end do
      Case('fax')
        do k=1,nArr
          aRes(k)=getdAuxFld(7,Val,nx,ny,nz)
        end do
      Case('fay')
        do k=1,nArr
          aRes(k)=getdAuxFld(8,Val,nx,ny,nz)
        end do
      Case('faz')
        do k=1,nArr
          aRes(k)=getdAuxFld(9,Val,nx,ny,nz)
        end do
      Case('fvx')
        do k=1,nArr
          aRes(k)=getdAuxFld(10,Val,nx,ny,nz)
        end do
      Case('gex')
        do k=1,nArr
          aRes(k)=getdFld(1,Val,nx,ny,nz)
        end do
      Case('gey')
        do k=1,nArr
          aRes(k)=getdFld(2,Val,nx,ny,nz)
        end do
      Case('gez')
        do k=1,nArr
          aRes(k)=getdFld(3,Val,nx,ny,nz)
        end do
      Case('ghx')
        do k=1,nArr
          aRes(k)=getdFld(4,Val,nx,ny,nz)
        end do
      Case('ghy')
        do k=1,nArr
          aRes(k)=getdFld(5,Val,nx,ny,nz)
        end do
      Case('ghz')
        do k=1,nArr
          aRes(k)=getdFld(6,Val,nx,ny,nz)
        end do
      Case('gax')
        do k=1,nArr
          aRes(k)=getdFld(7,Val,nx,ny,nz)
        end do
      Case('gay')
        do k=1,nArr
          aRes(k)=getdFld(8,Val,nx,ny,nz)
        end do
      Case('gaz')
        do k=1,nArr
          aRes(k)=getdFld(9,Val,nx,ny,nz)
        end do
      Case('gvx')
        do k=1,nArr
          aRes(k)=getdFld(10,Val,nx,ny,nz)
        end do
      Case('lox')
        do k=1,nArr
          aRes(k)=getGrd(1,Val,nx,ny,nz)
        end do
      Case('loy')
        do k=1,nArr
          aRes(k)=getGrd(2,Val,nx,ny,nz)
        end do
      Case('loz')
        do k=1,nArr
          aRes(k)=getGrd(3,Val,nx,ny,nz)
        end do
! Cases with four arguments
      Case('if>')
        do k=1,nArr
          if(Val(k,1).gt.Val(k,2)) then
            aRes(k)=Val(k,3)
          else
            aRes(k)=Val(k,4)
          end if
        end do
      Case('if<')
        do k=1,nArr
          if(Val(k,1).lt.Val(k,2)) then
            aRes(k)=Val(k,3)
          else
            aRes(k)=Val(k,4)
          end if
        end do
      Case('if=')
        do k=1,nArr
          if(dabs(Val(k,1)-Val(k,2)).lt.pSmall) then
            aRes(k)=Val(k,3)
          else
            aRes(k)=Val(k,4)
          end if
        end do
! cases with one or more arguments
      Case('add')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.add.Val(k,i)
          end do
          aRes(k)=r
        end do
      Case('cir') ! Ralf's function for deformend circles
        do k=1,nArr
          r=Val(k,1)
          if(nForA.gt.3) then
            aRes(k)=1.0d0-CircleDeform0(Val(k,1),Val(k,2),Val(k,3),nForA-3,Val(k,4:nForA))
          else
            aRes(k)=0.0d0
          end if
        end do
      Case('com')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.mul.Val(k,i)
          end do
          aRes(k)=.cos.r
        end do
      Case('fco') ! Fourier, only cosine terms
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=0.0d0
          do i=2,nForA
            aRes(k)=aRes(k)+Val(k,i)*cos(dble(i-2)*r)
          end do
        end do
      Case('fsi') ! Fourier, only sine terms
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=0.0d0
          do i=2,nForA
            aRes(k)=aRes(k)+Val(k,i)*sin(dble(i-1)*r)
          end do
        end do
      Case('fou') ! Fourier
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=0.0d0
          if(nForA.gt.1) aRes(k)=aRes(k)+Val(k,2)
          do i=3,nForA,2
            aRes(k)=aRes(k)+Val(k,i)*cos(dble((i-1)/2)*r)
            aRes(k)=aRes(k)+Val(k,i+1)*sin(dble((i-1)/2)*r)
          end do
          if((nForA-(2*(nForA/2))).gt.0) aRes(k)=aRes(k)+Val(k,nForA)*cos(dble((nForA-1)/2)*r)
        end do
      Case('max')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=Max(r,Val(k,i))
          end do
          aRes(k)=r
        end do
      Case('min')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=Min(r,Val(k,i))
          end do
          aRes(k)=r
        end do
      Case('mul')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.mul.Val(k,i)
          end do
          aRes(k)=r
        end do
      Case('sim')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.mul.Val(k,i)
          end do
          aRes(k)=.sin.r
        end do
! cases with four or more arguments
      Case('pro')
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=1.0
          if(nForA.gt.1) aRes(k)=Val(k,2)
          do i=4,nForA,2
            r1=Val(k,i-1)
            r2=Val(k,i)
            if(r.gt.r1) aRes(k)=r2
          end do
        end do
      Case Default
! unknown cases
        if(lForWarn) then
          if(l4) write(*,*) 'ERROR in Formula: unknown function!'
          if(l4) write(*,*) 'function=',sFkt(1:3)
          if(l4) write(*,*) 'original string=',s(1:ls)
        end if
        iErr=2
        return
      end Select
    else
! value: check if constant, parameter, or variable and compute
      if(lFor.eq.1) then
        ir=0
      else
        call StrToInt(sFor(2:lFor),ir,iErr)
        if((iErr.ne.0).and.(lFor.eq.3)) then
          select case(sFor(1:3))
          case('fre')
            do k=1,nArr
              aRes(k)=Dble(fcFld)
            end do
            iErr=0
            return
          case('gam')
            do k=1,nArr
              if(lzPer) then
                aRes(k)=Dble(dcFld)
              else
                aRes(k)=Dble(gcFld)
              end if
            end do
            iErr=0
            return
          case('ome')
            do k=1,nArr
              aRes(k)=Dble((2.0d0*Pi)*fcFld)
            end do
            iErr=0
            return
          end select
        end if
      end if
      Select Case(sFor(1:1))
      Case('c')
! constant
        if((ir.lt.0).or.(ir.gt.mC).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Constant out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mC=',iErr,ir,mC
          end if
          iErr=1
          return
        end if
        do k=1,nArr
          aRes(k)=Cns(ir)
        end do
      Case('p')
! parameter
        if((ir.lt.0).or.(ir.gt.mP).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Parameter out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mP=',iErr,ir,mP
          end if
          iErr=1
          return
        end if
        do k=1,nArr
          aRes(k)=Par(ir)
        end do
      Case('v')
! variable
        if((ir.lt.0).or.(ir.gt.mV).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable V out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mV=',iErr,ir,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.(ir.ge.iArrVar).and.(ir.lt.(iArrVar+nArrV))) then
          k0=(ir-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir)
          end do
        end if
      Case('q')
! abbreviation
        if((ir.lt.1).or.(ir+279.gt.mV).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable Q out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mV=',iErr,ir,mV
          end if
          iErr=1
          return
        end if
        do k=1,nArr
          aRes(k)=Var(ir+279)
        end do
      Case('t')
        if((ir.lt.0).or.(ir.gt.mV).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable T out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.(ir.ge.iArrVar).and.(ir.lt.(iArrVar+nArrV))) then
          k0=(ir-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir)
          end do
        end if
      Case('x')
        if((ir.lt.0).or.(ir.gt.mV-1).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable X out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.((ir+1).ge.iArrVar).and.((ir+1).lt.(iArrVar+nArrV))) then
          k0=(ir+1-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir+1)
          end do
        end if
      Case('y')
        if((ir.lt.0).or.(ir.gt.mV-2).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable Y out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.((ir+2).ge.iArrVar).and.((ir+2).lt.(iArrVar+nArrV))) then
          k0=(ir+2-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir+2)
          end do
        end if
      Case('z')
        if((ir.lt.0).or.(ir.gt.mV-3).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable Z out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.((ir+3).ge.iArrVar).and.((ir+3).lt.(iArrVar+nArrV))) then
          k0=(ir+3-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir+3)
          end do
        end if
      Case('d')
        call getID(sFor(1:Min(3,Max(1,lFor))),lFor,mV,ixyz,ia,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable D.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getRD(Var,mV,ixyz,ia)
        end do
      Case('e')
        call getIF(sFor(1:Min(5,Max(1,lFor))),lFor,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable E.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getRF(Var,mV,0,ixyz,ia,ib,ic)
        end do
      Case('h')
        call getIF(sFor(1:Min(5,Max(1,lFor))),lFor,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable H.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getRF(Var,mV,1,ixyz,ia,ib,ic)
        end do
      Case('a')
        call getIF(sFor(1:Min(5,Max(1,lFor))),lFor,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable A.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getRF(Var,mV,2,ixyz,ia,ib,ic)
        end do
      Case('s')
        sPot(1:5)='sx000'
        if(lFor.gt.1) sPot(3:1+lFor)=sFor(2:lFor)
        call getIF(sPot,5,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Variable S.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getRF(Var,mV,3,ixyz,ia,ib,ic)
        end do
      Case Default
! numeric constant
        r=GetSValue(sFor,Int2(lFor))
        do k=1,nArr
          aRes(k)=r
        end do
        iErr=0
        if(lMatErr) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: Numeric constant unreadable!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mC=',iErr,ir,mC
          end if
          iErr=1
        end if
      end select
    end if
  end Function Formul

  Recursive Function CFormul(sFor,lFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArrOi,iArrVarOi,nArrVi) Result(aRes)
    Implicit none
    Integer(4) mC,mP,mV,nx,ny,nz,iErr,i,k,nn1,lArg,lo,lArgs,lFor,nArrV,nArr,iArrVar,ls,li,nForA,nnz,iEr,ir,k0,ixyz, &
    & ia,ib,ic
    Complex(8) Val(mForA,mForA),Cns(0:mC),Par(0:mP),Var(-10:mV),cwrk(1),cb(1),aRes(mForA),r,r1,r2,om
    Complex(8), Optional :: vArr(mForA)
    Real(8) Rand1,Rand2
    Integer(4), Optional :: nArrOi,nArrVi,iArrVarOi
    Logical lArr
    Character(*) sFor
    Character(1151) s,sArgs,sArg
    Character(5) sPot
    Character(4) sFkt
    Character(1) sB
    Integer(4) i1Rnd
    Data i1Rnd/0_4/
    nArrV=1
    nArr=1
    iArrVar=1
    if(Present(nArrVi)) nArrV=nArrVi
    if(Present(nArrOi)) nArr=max(min(mForA/nArrV,nArrOi),1)
    if(Present(iArrVarOi)) iArrVar=max(min(min(iArrVarOi,nArr),mV),-10)
    lArr=.false.
    if(Present(vArr)) lArr=.true.
! default result
    iErr=0
    aRes(1:nArr)=(0.0d0,0.0d0)
    if(lFor.gt.1150) then
      if(lForWarn) then
        if(l4) write(*,*) 'ERROR in CFormula: string too long! max.length=',1150
        if(l4) write(*,*) 'original string=',sFor
        if(l4) write(*,*) 'lFor=',lFor
      end if
      iErr=1
      return
    end if
! default result
    iErr=0
    aRes(1:nArr)=(0.0d0,0.0d0)
! prepare string
    if(lFor.lt.1) then
      call DelBlanks(sFor,lFor)
      call ToLower(sFor,lFor)
    end if
    if(lFor.lt.1) return
    ls=lFor
    s(1:lFor)=sFor(1:lFor)
    s(lFor+1:lFor+1)=Char(0)
! check if function or value
    if(lFor.gt.4) then
      sB(1:1)=sFor(4:4)
    else
      sB(1:1)='v'
    end if
    if(sB(1:1).eq.'(') then
! functions: determine Type and argument string, compute
      sFkt(1:3)=sFor(1:3)
      lArgs=lFor-5
      sArgs(1:lArgs)=sFor(5:lFor-1)
      sArgs(lArgs+1:lArgs+1)=char(0)
      if(lArgs.lt.1) then
        if(lForWarn) then
          if(l4) write(*,*) 'ERROR in CFormula: No argument found!'
          if(l4) write(*,*) 'argument string=',sArgs(1:lArgs)
        end if
        iErr=1
        return
      end if
! user-defined operators
      if(sFkt(1:1).eq.'u') then
        call StrToInt(sFkt(2:3),kUserForm,iErr) ! get number of operator
        if((kUserForm.gt.nUserForm).or.(kUserForm.lt.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in Formula: user-defined formula not defined!'
            if(l4) write(*,*) 'formula number=',kUserForm
          end if
          iErr=3
          return
        end if
        if(lUserForm(kUserForm).lt.1) then
          call DelBlanks(sUserForm(kUserForm),lUserForm(kUserForm))
          call ToLower(sUserForm(kUserForm),lUserForm(kUserForm))
        end if
        lo=0 ! insert argument strings in the operator string
        do li=1,lUserForm(kUserForm)
          if(sUserForm(kUserForm)(li:li).eq.'?') then ! insert argument string
            call GetArg1(sArgs,lArgs,sArg,lArg,iErr)
            if(iErr.ne.0) then
              if(lForWarn) then
                if(l4) write(*,*) 'ERROR in Formula: GetArg1 returned error!'
                if(l4) write(*,*) 'current argument string=',lArgs,sArgs(1:lArgs)
                if(l4) write(*,*) 'original string=',lFor,'X',sFor(1:lFor),'X'
              end if
              iErr=1
              return
            end if
            lo=lo+lArg
            if(lo.gt.1150) then
              if(lForWarn) then
                if(l4) write(*,*) 'ERROR in Formula: user-defined formula too long!'
                if(l4) write(*,*) 'formula string=',sUserForm(kUserForm)(1:lUserForm(kUserForm))
              end if
              iErr=3
              return
            end if
            sUserFor(lo-lArg+1:lo)=sArg(1:lArg)
          else
            lo=lo+1 ! copy character
            if(lo.gt.1150) then
              if(lForWarn) then
                if(l4) write(*,*) 'ERROR in Formula: user-defined formula too long!'
                if(l4) write(*,*) 'formula string=',sUserForm(kUserForm)(1:lUserForm(kUserForm))
              end if
              iErr=3
              return
            end if
            sUserFor(lo:lo)=sUserForm(kUserForm)(li:li)
          end if
        end do
        lUserFor=lo ! evaluate operator
        if(lArr) then
          aRes=CFormul(sUserFor,lUserFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
        else
          aRes=CFormul(sUserFor,lUserFor,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
        end if
        return
      end if
! get all arguments of the formula and evaluate them -> Val
      do i=1,mForA
        Val(1:nArr,i)=(0.0d0,0.0d0)
        call GetArg1(sArgs,lArgs,sArg,lArg,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: GetArg1 returned error!'
            if(l4) write(*,*) 'current argument string=',lArgs,sArgs(1:lArgs)
            if(l4) write(*,*) 'current argument number=',i
            if(l4) write(*,*) 'original string=',lFor,sFor(1:lFor)
          end if
          iErr=1
          return
        end if
        if(lArr) then
          Val(1:nArr,i)=CFormul(sArg,lArg,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr,vArr,nArr,iArrVar,nArrV)
        else
          Val(1:nArr,i)=CFormul(sArg,lArg,Cns,Par,Var,mC,mP,mV,nx,ny,nz,iErr)
        end if
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: CFormula returned error!'
            if(l4) write(*,*) 'current argument string=',lArgs,sArgs(1:lArgs)
            if(l4) write(*,*) 'current argument number=',i
            if(l4) write(*,*) 'original string=',lFor,sFor(1:lFor)
          end if
          iErr=1
          return
        end if
        if(lArgs.eq.0) then
          nForA=i
          Exit
        end if
      end do
      nn1=1_4
      Select Case(sFkt(1:3))
! Cases with one argument
      Case('rea')
        do k=1,nArr
          aRes(k)=DCmplx(Dble(Val(k,1)),0.0d0)
        end do
      Case('ima')
        do k=1,nArr
          aRes(k)=DCmplx(DImag(Val(k,1)),0.0d0)
        end do
      Case('cnj')
        do k=1,nArr
          aRes(k)=DConjg(Val(k,1))
        end do
      Case('neg')
        do k=1,nArr
          aRes(k)=-Val(k,1)
        end do
      Case('inv')
        do k=1,nArr
          aRes(k)=.inv.Val(k,1)
        end do
      Case('abs')
        do k=1,nArr
          aRes(k)=cdabs(Val(k,1))
        end do
      Case('phi')
        do k=1,nArr
          aRes(k)=DCmplx(Protected_Ata(DImag(Val(k,1)),Dble(Val(k,1))),0.0d0)
        end do
      Case('int')
        do k=1,nArr
          aRes(k)=.int.Val(k,1)
        end do
      Case('sqr')
        do k=1,nArr
          aRes(k)=Val(k,1).mul.Val(k,1)
        end do
      Case('srt')
        do k=1,nArr
          aRes(k)=.srt.Val(k,1)
        end do
      Case('log')
        do k=1,nArr
          aRes(k)=.log.Val(k,1)
        end do
      Case('exp')
        do k=1,nArr
          aRes(k)=.exp.Val(k,1)
        end do
      Case('sin')
        do k=1,nArr
          aRes(k)=.sin.Val(k,1)
        end do
      Case('cos')
        do k=1,nArr
          aRes(k)=.cos.Val(k,1)
        end do
! Cases with two arguments
      Case('ran')
        Rand1=CHrnd(0.0d0,1.0d0,0_4)
        do k=1,nArr
          Rand1=CHrnd(Dble(Val(k,1)),Dble(Val(k,2)))
          Rand2=CHrnd(DImag(Val(k,1)),DImag(Val(k,2)))
          aRes(k)=DCmplx(Rand1,Rand2)
        end do
      Case('rnd')
        if(i1Rnd.eq.0) then
          Rand1=CHrnd(0.0d0,1.0d0,0_4)
          i1Rnd=1_4
        end if
        do k=1,nArr
          Rand1=CHrnd(Dble(Val(k,1)),Dble(Val(k,2)))
          Rand2=CHrnd(DImag(Val(k,1)),DImag(Val(k,2)))
          aRes(k)=DCmplx(Rand1,Rand2)
        end do
      Case('bit')
        do k=1,nArr
          if(BTEST(nint(dble(Val(k,1)),4),min(31,max(0,nint(dble(Val(k,2)),4)-1_4)))) then
            aRes(k)=(1.0d0,0.0d0)
          else
            aRes(k)=(0.0d0,0.0d0)
          end if
        end do
      Case('sub')
        do k=1,nArr
          aRes(k)=Val(k,1).sub.Val(k,2)
        end do
      Case('div')
        do k=1,nArr
          aRes(k)=Val(k,1).div.Val(k,2)
        end do
      Case('pow')
        do k=1,nArr
          aRes(k)=Val(k,1).pow.Val(k,2)
        end do
      Case('lga')
        do k=1,nArr
          aRes(k)=(.log.Val(k,2)).div.(.log.Val(k,1))
        end do
      Case('ta1')
        do k=1,nArr
          aRes(k)=Val(k,1)
        end do
      Case('ta2')
        do k=1,nArr
          aRes(k)=Val(k,2)
        end do
      Case('zic')
        do k=1,nArr
          call zir(abs(Dble(Val(k,1))),Val(k,2),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=cb(1)
        end do
      Case('zjc')
        do k=1,nArr
          call zjr(abs(Dble(Val(k,1))),Val(k,2),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=cb(1)
        end do
      Case('zkc')
        do k=1,nArr
          call zkr(abs(Dble(Val(k,1))),Val(k,2),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=cb(1)
        end do
      Case('zhc')
        do k=1,nArr
          call zhr(nn1,abs(Dble(Val(k,1))),Val(k,2),nn1,.false.,cb(1),nnz,ier)
          aRes(k)=cb(1)
        end do
      Case('zyc')
        do k=1,nArr
          call zyr(abs(Dble(Val(k,1))),Val(k,2),nn1,.false.,cb(1),nnz,cwrk(1),ier)
          aRes(k)=cb(1)
        end do
! Cases with three arguments
      Case('eps')
        do k=1,nArr
          aRes(k)=getcEps(Dble(Val),nx,ny,nz)
        end do
      Case('mue')
        do k=1,nArr
          aRes(k)=getcMue(Dble(Val),nx,ny,nz)
        end do
      Case('sig')
        do k=1,nArr
          aRes(k)=getcSig(Dble(Val),nx,ny,nz)
        end do
      Case('tau')
        do k=1,nArr
          aRes(k)=getcTau(Dble(Val),nx,ny,nz)
        end do
      Case('kap')
        do k=1,nArr
          aRes(k)=getcKap(Dble(Val),nx,ny,nz)
        end do
      Case('kkk')
        do k=1,nArr
          aRes(k)=getcK(Dble(Val),nx,ny,nz)
        end do
      Case('fex')
        do k=1,nArr
          aRes(k)=getcAuxFld(1,Val,nx,ny,nz)
        end do
      Case('fey')
        do k=1,nArr
          aRes(k)=getcAuxFld(2,Val,nx,ny,nz)
        end do
      Case('fez')
        do k=1,nArr
          aRes(k)=getcAuxFld(3,Val,nx,ny,nz)
        end do
      Case('fhx')
        do k=1,nArr
          aRes(k)=getcAuxFld(4,Val,nx,ny,nz)
        end do
      Case('fhy')
        do k=1,nArr
          aRes(k)=getcAuxFld(5,Val,nx,ny,nz)
        end do
      Case('fhz')
        do k=1,nArr
          aRes(k)=getcAuxFld(6,Val,nx,ny,nz)
        end do
      Case('fax')
        do k=1,nArr
          aRes(k)=getcAuxFld(7,Val,nx,ny,nz)
        end do
      Case('fay')
        do k=1,nArr
          aRes(k)=getcAuxFld(8,Val,nx,ny,nz)
        end do
      Case('faz')
        do k=1,nArr
          aRes(k)=getcAuxFld(9,Val,nx,ny,nz)
        end do
      Case('fvx')
        do k=1,nArr
          aRes(k)=getcAuxFld(10,Val,nx,ny,nz)
        end do
      Case('gex')
        do k=1,nArr
          aRes(k)=getcFld(1,Val,nx,ny,nz)
        end do
      Case('gey')
        do k=1,nArr
          aRes(k)=getcFld(2,Val,nx,ny,nz)
        end do
      Case('gez')
        do k=1,nArr
          aRes(k)=getcFld(3,Val,nx,ny,nz)
        end do
      Case('ghx')
        do k=1,nArr
          aRes(k)=getcFld(4,Val,nx,ny,nz)
        end do
      Case('ghy')
        do k=1,nArr
          aRes(k)=getcFld(5,Val,nx,ny,nz)
        end do
      Case('ghz')
        do k=1,nArr
          aRes(k)=getcFld(6,Val,nx,ny,nz)
        end do
      Case('gax')
        do k=1,nArr
          aRes(k)=getcFld(7,Val,nx,ny,nz)
        end do
      Case('gay')
        do k=1,nArr
          aRes(k)=getcFld(8,Val,nx,ny,nz)
        end do
      Case('gaz')
        do k=1,nArr
          aRes(k)=getcFld(9,Val,nx,ny,nz)
        end do
      Case('gvx')
        do k=1,nArr
          aRes(k)=getcFld(10,Val,nx,ny,nz)
        end do
      Case('lox')
        do k=1,nArr
          aRes(k)=DCmplx(getGrd(1,Dble(Val),nx,ny,nz),0.0d0)
        end do
      Case('loy')
        do k=1,nArr
          aRes(k)=DCmplx(getGrd(2,Dble(Val),nx,ny,nz),0.0d0)
        end do
      Case('loz')
        do k=1,nArr
          aRes(k)=DCmplx(getGrd(3,Dble(Val),nx,ny,nz),0.0d0)
        end do
! Cases with four arguments
      Case('if>')
        do k=1,nArr
          if(Dble(Val(k,1)).gt.Dble(Val(k,2))) then
            aRes(k)=Val(k,3)
          else
            aRes(k)=Val(k,4)
          end if
        end do
      Case('if<')
        do k=1,nArr
          if(Dble(Val(k,1)).lt.Dble(Val(k,2))) then
            aRes(k)=Val(k,3)
          else
            aRes(k)=Val(k,4)
          end if
        end do
      Case('if=')
        do k=1,nArr
          if(cdabs(Val(k,1)-Val(k,2)).lt.pSmall) then
            aRes(k)=Val(k,3)
          else
            aRes(k)=Val(k,4)
          end if
        end do
      Case('drf') ! Drude, frequency dependence
        do k=1,nArr
          om=2.0d0*Pi*Val(k,1)
          aRes(k)=Val(k,2)-((Val(k,3)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,4)))))
        end do
      Case('dro') ! Drude, omega dependence
        do k=1,nArr
          om=Val(k,1)
          aRes(k)=Val(k,2)-((Val(k,3)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,4)))))
        end do
      Case('drw') ! Drude, wavelength dependence
        do k=1,nArr
          om=DCmplx(2.0d0*Pi,0.0d0).div.(dsqrt(Mue0*Eps0)*Val(k,1))
          aRes(k)=Val(k,2)-((Val(k,3)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,4)))))
        end do
      Case('lmf') ! lossy material, frequency dependence
        do k=1,nArr
          om=2.0d0*Pi*Val(k,1)
          aRes(k)=Val(k,3)+((0.0d0,1.0d0)*Val(k,4).div.(om*Val(k,2)))
        end do
      Case('lmo') ! lossy material, omega dependence
        do k=1,nArr
          om=Val(k,1)
          aRes(k)=Val(k,3)+((0.0d0,1.0d0)*Val(k,4).div.(om*Val(k,2)))
        end do
      Case('lmw') ! lossy material, wavelength dependence
        do k=1,nArr
          om=DCmplx(2.0d0*Pi,0.0d0).div.(dsqrt(Mue0*Eps0)*Val(k,1))
          aRes(k)=Val(k,3)+((0.0d0,1.0d0)*Val(k,4).div.(om*Val(k,2)))
        end do
! Cases with six arguments
      Case('dsf') ! Drude + sigma, frequency dependence
        do k=1,nArr
          om=2.0d0*Pi*Val(k,1)
          aRes(k)=Val(k,3)-((Val(k,4)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,5)))))
          aRes(k)=aRes(k)+((0.0d0,1.0d0)*Val(k,6).div.(om*Val(k,2)))
        end do
      Case('dso') ! Drude + sigma, omega dependence
        do k=1,nArr
          om=Val(k,1)
          aRes(k)=Val(k,3)-((Val(k,4)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,5)))))
          aRes(k)=aRes(k)+((0.0d0,1.0d0)*Val(k,6).div.(om*Val(k,2)))
        end do
      Case('dsw') ! Drude + sigma, wavelength dependence
        do k=1,nArr
          om=DCmplx(2.0d0*Pi,0.0d0).div.(dsqrt(Mue0*Eps0)*Val(k,1))
          aRes(k)=Val(k,3)-((Val(k,4)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,5)))))
          aRes(k)=aRes(k)+((0.0d0,1.0d0)*Val(k,6).div.(om*Val(k,2)))
        end do
! Cases with seven arguments
      Case('dlf') ! Drude + Lorentz, frequency dependence
        do k=1,nArr
          om=2.0d0*Pi*Val(k,1)
          aRes(k)=Val(k,2)
          if(abs(Val(k,3)).gt.1.0d-200) aRes(k)=aRes(k)-((Val(k,3)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,4)))))
          aRes(k)=aRes(k)-Val(k,5)*((Val(k,6)**2).div.(om**2-Val(k,6)**2+(0.0d0,2.0d0)*om*Val(k,6)*Val(k,7)))
        end do
      Case('dlo') ! Drude + Lorentz, omega dependence: epsilon1-omegaD**2/(omega*(omega+i*gamma)-epsilonL*omegaL**2/(omega**2-omegaL**2+2*i*omega*omegaL)
        do k=1,nArr
          om=2.0d0*Pi*Val(k,1)
          aRes(k)=Val(k,2)
          if(abs(Val(k,3)).gt.1.0d-200) aRes(k)=aRes(k)-((Val(k,3)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,4)))))
          aRes(k)=aRes(k)-Val(k,5)*((Val(k,6)**2).div.(om**2-Val(k,6)**2+(0.0d0,2.0d0)*om*Val(k,6)*Val(k,7)))
        end do
      Case('dlw') ! Drude + Lorentz, wavelength dependence
        do k=1,nArr
          om=DCmplx(2.0d0*Pi,0.0d0).div.(dsqrt(Mue0*Eps0)*Val(k,1))
          aRes(k)=Val(k,2)
          aRes(k)=Val(k,2)
          if(abs(Val(k,3)).gt.1.0d-200) aRes(k)=aRes(k)-((Val(k,3)**2).div.(om*(om+((0.0d0,1.0d0)*Val(k,4)))))
          aRes(k)=aRes(k)-Val(k,5)*((Val(k,6)**2).div.(om**2-Val(k,6)**2+(0.0d0,2.0d0)*om*Val(k,6)*Val(k,7)))
        end do
! cases with 1 or more arguments
      Case('add')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.add.Val(k,i)
          end do
          aRes(k)=r
        end do
      Case('com')
        do k=1,nArr
        r=Val(k,1)
        do i=2,nForA
          r=r.mul.Val(k,i)
        end do
        aRes(k)=.cos.r
        end do
      Case('max')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            if(cdabs(r).lt.cdabs(Val(k,i))) r=Val(k,i)
          end do
          aRes(k)=r
        end do
      Case('min')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            if(cdabs(r).gt.cdabs(Val(k,i))) r=Val(k,i)
          end do
          aRes(k)=r
        end do
      Case('mul')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.mul.Val(k,i)
          end do
          aRes(k)=r
        end do
      Case('fco') ! Fourier, only cosine terms
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=0.0d0
          do i=2,nForA
            aRes(k)=aRes(k)+Val(k,i)*cos(dble(i-2)*r)
          end do
        end do
      Case('fsi') ! Fourier, only sine terms
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=0.0d0
          do i=2,nForA
            aRes(k)=aRes(k)+Val(k,i)*sin(dble(i-1)*r)
          end do
        end do
      Case('fou') ! Fourier
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=0.0d0
          if(nForA.gt.1) aRes(k)=aRes(k)+Val(k,2)
          do i=3,nForA,2
            aRes(k)=aRes(k)+Val(k,i)*cos(dble((i-1)/2)*r)
            aRes(k)=aRes(k)+Val(k,i+1)*sin(dble((i-1)/2)*r)
          end do
          if((nForA-(2*(nForA/2))).gt.0) aRes(k)=aRes(k)+Val(k,nForA)*cos(dble((nForA-1)/2)*r)
        end do
      Case('sim')
        do k=1,nArr
          r=Val(k,1)
          do i=2,nForA
            r=r.mul.Val(k,i)
          end do
          aRes(k)=.sin.r
        end do
! cases with four or more arguments
      Case('pro')
        do k=1,nArr
          r=Val(k,1)
          aRes(k)=1.0
          if(nForA.gt.1) aRes(k)=Val(k,2)
          do i=4,nForA,2
            r1=Val(k,i-1)
            r2=Val(k,i)
            if(cdabs(r).gt.cdabs(r1)) aRes(k)=r2
          end do
        end do
      Case Default
! unknown cases
        if(lForWarn) then
          if(l4) write(*,*) 'ERROR in CFormula: unknown function!'
          if(l4) write(*,*) 'function=',sFkt(1:3)
          if(l4) write(*,*) 'original string=',s(1:ls)
        end if
        iErr=2
        return
      end Select
    else
! value: check if constant, parameter, or variable and compute
      if(lFor.eq.1) then
        ir=0
      else
        call StrToInt(sFor(2:lFor),ir,iErr)
        if((iErr.ne.0).and.(lFor.eq.3)) then
          select case(sFor(1:3))
          case('fre')
            do k=1,nArr
              aRes(k)=fcFld
            end do
            iErr=0
            return
          case('gam')
            do k=1,nArr
              if(lzPer) then
                aRes(k)=dcFld
              else
                aRes(k)=gcFld
              end if
            end do
            iErr=0
            return
          case('ome')
            do k=1,nArr
              aRes(k)=(2.0d0*Pi)*fcFld
            end do
            iErr=0
            return
          end select
        end if
      end if
      Select Case(sFor(1:1))
      Case('c')
! constant
        if((ir.lt.0).or.(ir.gt.mC).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Constant out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mC=',iErr,ir,mC
          end if
          iErr=1
          return
        end if
        do k=1,nArr
          aRes(k)=Cns(ir)
        end do
      Case('p')
! parameter
        if((ir.lt.0).or.(ir.gt.mP).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Parameter out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mP=',iErr,ir,mP
          end if
          iErr=1
          return
        end if
        do k=1,nArr
          aRes(k)=Par(ir)
        end do
      Case('v')
! variable
        if((ir.lt.0).or.(ir.gt.mV).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable V out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mV=',iErr,ir,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.(ir.ge.iArrVar).and.(ir.lt.(iArrVar+nArrV))) then
          k0=(ir-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir)
          end do
        end if
      Case('q')
! abbreviation
        if((ir.lt.1).or.(ir+279.gt.mV).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable Q out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mV=',iErr,ir,mV
          end if
          iErr=1
          return
        end if
        do k=1,nArr
          aRes(k)=Var(ir+279)
        end do
      Case('t')
        if((ir.lt.0).or.(ir.gt.mV).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable T out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.(ir.ge.iArrVar).and.(ir.lt.(iArrVar+nArrV))) then
          k0=(ir-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir)
          end do
        end if
      Case('x')
        if((ir.lt.0).or.(ir.gt.mV-1).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable X out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.((ir+1).ge.iArrVar).and.((ir+1).lt.(iArrVar+nArrV))) then
          k0=(ir+1-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir+1)
          end do
        end if
      Case('y')
        if((ir.lt.0).or.(ir.gt.mV-2).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable Y out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.((ir+2).ge.iArrVar).and.((ir+2).lt.(iArrVar+nArrV))) then
          k0=(ir+2-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir+2)
          end do
        end if
      Case('z')
        if((ir.lt.0).or.(ir.gt.mV-3).or.(iErr.ne.0)) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable Z out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          iErr=1
          return
        end if
        if(lArr.and.((ir+3).ge.iArrVar).and.((ir+3).lt.(iArrVar+nArrV))) then
          k0=(ir+3-iArrVar)*nArr
          do k=1,nArr
            k0=k0+1
            aRes(k)=vArr(k0)
          end do
        else
          do k=1,nArr
            aRes(k)=Var(ir+3)
          end do
        end if
      Case('d')
        call getID(sFor(1:Min(3,Max(1,lFor))),lFor,mV,ixyz,ia,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable D.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getcRD(Var,mV,ixyz,ia)
        end do
      Case('e')
        call getIF(sFor(1:Min(5,Max(1,lFor))),lFor,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable E.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getcRF(Var,mV,0,ixyz,ia,ib,ic)
        end do
      Case('h')
        call getIF(sFor(1:Min(5,Max(1,lFor))),lFor,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable H.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getcRF(Var,mV,1,ixyz,ia,ib,ic)
        end do
      Case('a')
        call getIF(sFor(1:Min(5,Max(1,lFor))),lFor,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable A.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getcRF(Var,mV,2,ixyz,ia,ib,ic)
        end do
      Case('s')
        sPot(1:5)='sx000'
        if(lFor.gt.1) sPot(3:1+lFor)=sFor(2:lFor)
        call getIF(sPot,5,mV,ixyz,ia,ib,ic,iErr)
        if(iErr.ne.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Variable S.. out of range!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,mV=',iErr,mV
          end if
          return
        end if
        do k=1,nArr
          if(lArr) then
            do k0=0,nArrV-1
              Var(iArrVar+k0)=vArr(k+k0*nArr)
            end do
          end if
          aRes(k)=getcRF(Var,mV,3,ixyz,ia,ib,ic)
        end do
      Case Default
! numeric constant
        r=GetCSValue(sFor,Int2(lFor))
        do k=1,nArr
          aRes(k)=r
        end do
        iErr=0
        if(lMatErr) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in CFormula: Numeric constant unreadable!'
            if(l4) write(*,*) 'original string=',s(1:ls)
            if(l4) write(*,*) 'sFor=',sFor(1:lFor)
            if(l4) write(*,*) 'iErr,ir,mC=',iErr,ir,mC
          end if
          iErr=1
        end if
      end select
    end if
  end Function CFormul

  Subroutine getID(s0,l,mV,ixyz,id,iErr)
    Implicit none
    Integer(4) l,mV,id,ixyz,iErr,ie
    Character(3) s,s0
    id=0
    ie=0
    ixyz=0
    iErr=1
    if((l.gt.3).or.(l.lt.2)) return
    s='000'
    s(1:l)=s0(1:l)
    Select Case(s(2:2))
    Case('t')
      ixyz=-1
    Case('x')
      ixyz=0
    Case('y')
      ixyz=1
    Case('z')
      ixyz=2
    Case Default
      return
    end Select
    Select Case(s(3:3))
    Case('+')
      id=0
    Case('-')
      id=1
    Case('0')
      id=-1
    Case Default
      return
    end Select
    iErr=0
    if(mV.lt.9) iErr=2
  end Subroutine getID

  Subroutine getIF(s0,l,mV,ixyz,ia,ib,ic,iErr)
    Implicit none
    Integer(4) l,ia,ib,ic,ixyz,iErr,mV
    Character(5) s,s0
    ia=0
    ib=0
    ic=0
    ixyz=0
    iErr=1
    if((l.gt.5).or.(l.lt.2)) return
    s='00000'
    s(1:l)=s0(1:l)
    Select Case(s(2:2))
    Case('x')
      ixyz=0
    Case('y')
      ixyz=1
    Case('z')
      ixyz=2
    Case Default
      return
    end Select
    Select Case(s(3:3))
    Case('0')
      ia=0
    Case('+')
      ia=1
    Case('-')
      ia=2
    Case Default
      return
    end Select
    Select Case(s(4:4))
    Case('0')
      ib=0
    Case('+')
      ib=1
    Case('-')
      ib=2
    Case Default
      return
    end Select
    Select Case(s(5:5))
    Case('0')
      ic=0
    Case('+')
      ic=1
    Case('-')
      ic=2
    Case Default
      return
    end Select
    iErr=0
    if(mV.lt.280) iErr=2
  end Subroutine getIF

  Real(8) Function getRD(v,mV,ixyz,id)
    Implicit none
    Integer(4) mV,n,id,ixyz
    Real(8) v(-10:mV)
    if(ixyz.ge.0) then
      n=4+ixyz*2
      if(id.ge.0) then
        getRD=v(n+id)
      else
        getRD=0.5d0*(v(n)+v(n+1))
      end if
    else
      if(id.ge.0) then
        getRD=v(-1-id)
      else
        getRD=0.5d0*(v(-1)+v(-2))
      end if
    end if
  end Function getRD

  Complex(8) Function getcRD(v,mV,ixyz,id)
    Implicit none
    Integer(4) mV,n,id,ixyz
    Complex(8) v(-10:mV)
    if(ixyz.gt.0) then
      n=4+ixyz*2
      if(id.ge.0) then
        getcRD=v(n+id)
      else
        getcRD=0.5d0*(v(n)+v(n+1))
      end if
    else
      getcRD=sum(v(4:9))/6.0d0
    end if
  end Function getcRD

  Real(8) Function getRF(v,mV,ie,ixyz,ia,ib,ic)
    Implicit none
    Integer(4) mV,n,ie,ia,ib,ic,ixyz
    Real(8) v(-10:mV)
    n=ie*3+ixyz+10*(ia+ib*3+ic*9+1)
    getRF=v(n)
  end Function getRF

  Complex(8) Function getcRF(v,mV,ie,ixyz,ia,ib,ic)
    Implicit none
    Integer(4) mV,n,ie,ia,ib,ic,ixyz
    Complex(8) v(-10:mV)
    n=ie*3+ixyz+10*(ia+ib*3+ic*9+1)
    getcRF=v(n)
  end Function getcRF

  Subroutine GetArg1(sArgs,lArgs,sArg1,lArg1,iErr)
    Implicit none
    Integer(4) lArgs,lArg1,iErr,nB,i
    Character(*) sArgs
    Character(1151) sArg1
    Character(1) c
    nB=0
    sArg1=' '
    lArg1=0
    iErr=0
    do i=1,lArgs
      c(1:1)=sArgs(i:i)
      if(c(1:1).eq.'(') then
        nB=nB+1
      else if(c(1:1).eq.')') then
        nB=nB-1
        if(nB.lt.0) then
          if(lForWarn) then
            if(l4) write(*,*) 'ERROR in GetArg1: Too many )s!'
            if(l4) write(*,*) 'lArgs,sArgs=',lArgs,sArgs(1:lArgs)
            if(l4) write(*,*) 'current position i=',i
          end if
          iErr=1
          return
        end if
      else if((c(1:1).eq.',').and.(nB.eq.0)) then
        lArg1=i-1
        sArg1(1:lArg1)=sArgs(1:lArg1)
        sArg1(lArg1+1:lArg1+1)=char(0)
        lArgs=lArgs-lArg1-1
        sArgs(1:lArgs)=sArgs(lArg1+2:lArg1+lArgs+1)
        sArgs(lArgs+1:lArgs+1)=char(0)
        return
      end if
    end do
    if(nB.gt.0) then
      if(lForWarn) then
        if(l4) write(*,*) 'ERROR in GetArg1: Too many (s!'
        if(l4) write(*,*) 'lArgs,sArgs=',lArgs,sArgs(1:lArgs)
      end if
      iErr=2
    else
      lArg1=lArgs
      sArg1(1:lArg1)=sArgs(1:lArg1)
      sArg1(lArg1+1:lArg1+1)=char(0)
      lArgs=0
    end if
  end Subroutine GetArg1

  Integer(4) Function nArgInString(s,l)
! count number of arguments (terminals) in the string s of length l
    Implicit none
    Integer(4) l,nv,i
    Character(*) s
    nv=1
    do i=2,l-1
      if(s(i:i).eq.',') nv=nv+1
    end do
    nArgInString=nv
  end Function nArgInString

  Integer(4) Function nVarInString(s,l)
! count number of variables in the string s of length l
    Implicit none
    Integer(4) l,nv,i
    Character(*) s
    nv=0
    do i=2,l-1
      if((s(i:i).eq.'x').or.(s(i:i).eq.'v')) then
        if(((s(i+1:i+1).eq.'0').or.(s(i+1:i+1).eq.',').or.(s(i+1:i+1).eq.')')).and. &
           ((s(i-1:i-1).eq.',').or.(s(i-1:i-1).eq.'('))) nv=nv+1
      end if
    end do
    nVarInString=nv
  end Function nVarInString

  Integer(4) Function nParInString(s,l)
! count number of Parameters in the string s of length l
    Implicit none
    Integer(4) l,nv,i
    Character(*) s
    nv=0
    do i=2,l-1
      if(s(i:i).eq.'p') then
        if(((s(i+1:i+1).eq.'0').or.(s(i+1:i+1).eq.',').or.(s(i+1:i+1).eq.')')).and. &
           ((s(i-1:i-1).eq.',').or.(s(i-1:i-1).eq.'('))) nv=nv+1
      end if
    end do
    nParInString=nv
  end Function nParInString

  Integer(4) Function nConInString(s,l)
! count number of formal constants in the string s of length l
    Implicit none
    Integer(4) l,nv,i
    Character(*) s
    nv=0
    do i=2,l-2
      if(s(i:i).eq.'c') then
        if(((s(i+2:i+2).eq.',').or.(s(i+2:i+2).eq.')')).and. &
           ((s(i-1:i-1).eq.',').or.(s(i-1:i-1).eq.'('))) nv=nv+1
      end if
    end do
    nConInString=nv
  end Function nConInString

  Integer(4) Function nCnsInString(s,l)
! count number of explicit constants in the string s of length l
    Implicit none
    Integer(4) l,nv,i
    Character(*) s
    nv=0
    i=0
    do while(i.lt.l-2)
      i=i+1
      if((s(i:i).ne.'(').and.(s(i:i).ne.',')) Cycle
      i=i+1
      if((s(i:i).eq.'c').or.(s(i:i).eq.'p').or.(s(i:i).eq.'v').or.(s(i:i).eq.'x')) Cycle
      if(i.lt.l-2) then
        if(s(i+3:i+3).eq.'(') Cycle
      end if
      nv=nv+1
    end do
    nCnsInString=nv
  end Function nCnsInString

  Integer(4) Function nOprInString(s,l)
! count number of operators in the string s of length l
    Implicit none
    Integer(4) l,nv,i
    Character(*) s
    nv=0
    do i=2,l-1
      if(s(i:i).eq.'(') nv=nv+1
    end do
    nOprInString=nv
  end Function nOprInString

  Integer(4) Function iVarInString(s,l,n)
! find position of the n-th variable in the string s of length l
    Implicit none
    Integer(4) l,n,nv,i
    Character(*) s
    nv=0
    do i=2,l-1
      if((s(i:i).eq.'x').or.(s(i:i).eq.'v')) then
        if(((s(i+1:i+1).eq.'0').or.(s(i+1:i+1).eq.',').or.(s(i+1:i+1).eq.')')).and. &
           ((s(i-1:i-1).eq.',').or.(s(i-1:i-1).eq.'('))) then
          nv=nv+1
          if(nv.ge.n) Exit
        end if
      end if
    end do
    iVarInString=i
  end Function iVarInString

  Integer(4) Function iParInString(s,l,n)
! find position of the n-th parameter in the string s of length l
    Implicit none
    Integer(4) l,n,nv,i
    Character(*) s
    nv=0
    do i=2,l-1
      if(s(i:i).eq.'p') then
        if(((s(i+1:i+1).eq.'0').or.(s(i+1:i+1).eq.',').or.(s(i+1:i+1).eq.')')).and. &
           ((s(i-1:i-1).eq.',').or.(s(i-1:i-1).eq.'('))) then
          nv=nv+1
          if(nv.ge.n) Exit
        end if
      end if
    end do
    iParInString=i
  end Function iParInString

  Integer(4) Function iConInString(s,l,n)
! find position of the n-th formal constant in the string s of length l
    Implicit none
    Integer(4) l,n,nv,i
    Character(*) s
    nv=0
    do i=2,l-2
      if(s(i:i).eq.'c') then
        if(((s(i+2:i+2).eq.',').or.(s(i+2:i+2).eq.')')).and. &
           ((s(i-1:i-1).eq.',').or.(s(i-1:i-1).eq.'('))) then
          nv=nv+1
          if(nv.ge.n) Exit
        end if
      end if
    end do
    iConInString=i
  end Function iConInString

  Integer(4) Function iCnsInString(s,l,n,lCns)
! find position of the n-th explicit constant in the string s of length l
    Implicit none
    Integer(4) l,n,lCns,nv,i
    Character(*) s
    nv=0
    i=0
    do while(i.lt.l-2)
      i=i+1
      if((s(i:i).ne.'(').and.(s(i:i).ne.',')) Cycle
      i=i+1
      if((s(i:i).eq.'c').or.(s(i:i).eq.'p').or.(s(i:i).eq.'v').or.(s(i:i).eq.'x')) Cycle
      if(i.lt.l-2) then
        if(s(i+3:i+3).eq.'(') Cycle
      end if
      nv=nv+1
      if(nv.ge.n) Exit
    end do
    iCnsInString=i
    lCns=1
    i=i+1
    do while((s(i:i).ne.')').and.(s(i:i).ne.','))
      lCns=lCns+1
      i=i+1
    end do
  end Function iCnsInString

  Integer(4) Function iOprInString(s,l,n)
! find position of the n-th operation in the string s of length l
    Implicit none
    Integer(4) l,n,nv,i
    Character(*) s
    nv=0
    do i=2,l-1
      if(s(i:i).eq.'(') then
          nv=nv+1
          if(nv.ge.n) Exit
        end if
    end do
    iOprInString=i-3
  end Function iOprInString

  Real(8) Function getGrd(id,Val,nx,ny,nz)
    Implicit none
    Integer(4) id,kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    if((id.lt.1).or.(id.gt.3)) then
      getGrd=0.0d0
    else
      call getkxyz(Val,nx,ny,nz,kx,ky,kz)
      getGrd=rGrd(id,kx,ky,kz)
    end if
  end Function getGrd

  Real(8) Function getdFld(id,Val,nx,ny,nz)
    Implicit none
    Integer(4) id,kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    if((idFld(id).lt.1).or.(idFld(id).gt.ncFld)) then
      getdFld=0.0d0
    else
      call getkxyz(Val,nx,ny,nz,kx,ky,kz)
      getdFld=dFld(idFld(id),kx,ky,kz)
    end if
  end Function getdFld

  Real(8) Function getdAuxFld(id,Val,nx,ny,nz)
    Implicit none
    Integer(4) id,kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    if((idFld(id).lt.1).or.(idFld(id).gt.ncFld)) then
      getdAuxFld=0.0d0
    else
      call getkxyz(Val,nx,ny,nz,kx,ky,kz)
      getdAuxFld=dAuxFld(idFld(id),kx,ky,kz)
    end if
  end Function getdAuxFld

  Complex(8) Function getcFld(id,Val,nx,ny,nz)
    Implicit none
    Integer(4) id,kx,ky,kz,nx,ny,nz
    Complex(8) Val(3)
    if((idFld(id).lt.1).or.(idFld(id).gt.ncFld)) then
      getcFld=(0.0d0,0.0d0)
    else
      call getkxyz(Dble(Val),nx,ny,nz,kx,ky,kz)
      getcFld=cFld(idFld(id),kx,ky,kz)
    end if
  end Function getcFld

  Complex(8) Function getcAuxFld(id,Val,nx,ny,nz)
    Implicit none
    Integer(4) id,kx,ky,kz,nx,ny,nz
    Complex(8) Val(3)
    if((idFld(id).lt.1).or.(idFld(id).gt.ncFld)) then
      getcAuxFld=(0.0d0,0.0d0)
    else
      call getkxyz(Dble(Val),nx,ny,nz,kx,ky,kz)
      getcAuxFld=cAuxFld(idFld(id),kx,ky,kz)
    end if
  end Function getcAuxFld

  Complex(8) Function getcEps(Val,nx,ny,nz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    call getkxyz(Val,nx,ny,nz,kx,ky,kz)
    getcEps=eDom(Max(1_2,Min(Int2(nDom),iFld(kx,ky,kz))))*Eps0
  end Function getcEps

  Complex(8) Function getcMue(Val,nx,ny,nz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    call getkxyz(Val,nx,ny,nz,kx,ky,kz)
    getcMue=uDom(Max(1_2,Min(Int2(nDom),iFld(kx,ky,kz))))*Mue0
  end Function getcMue

  Complex(8) Function getcSig(Val,nx,ny,nz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    call getkxyz(Val,nx,ny,nz,kx,ky,kz)
    getcSig=sDom(Max(1_2,Min(Int2(nDom),iFld(kx,ky,kz))))
  end Function getcSig

  Complex(8) Function getcTau(Val,nx,ny,nz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz
    Real(8) Val(3)
    call getkxyz(Val,nx,ny,nz,kx,ky,kz)
    getcTau=tDom(Max(1_2,Min(Int2(nDom),iFld(kx,ky,kz))))
  end Function getcTau

  Complex(8) Function getcKap(Val,nx,ny,nz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz
    Integer(2) lf
    Real(8) Val(3)
    call getkxyz(Val,nx,ny,nz,kx,ky,kz)
    lf=Max(1_2,Min(Int2(nDom),iFld(kx,ky,kz)))
    kcFld=Wnumber(lf)
    if(lzPer) then
      kapcFld=cdsqrt(kcFld**2-(Pi/dcFld)**2)
    else
      kapcFld=cdsqrt(kcFld**2-((2.0d0*Pi*kw0)*fcFld*gcFld)**2)
    end if
    if(DImag(kapcFld).lt.0.0d0) kapcFld=-kapcFld
    getcKap=kapcFld
  end Function getcKap

  Complex(8) Function getcK(Val,nx,ny,nz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz
    Integer(2) lf
    Real(8) Val(3)
    call getkxyz(Dble(Val),nx,ny,nz,kx,ky,kz)
    lf=Max(1_2,Min(Int2(nDom),iFld(kx,ky,kz)))
    kcFld=Wnumber(lf)
    getcK=kcFld
  end Function getcK

  Subroutine getkxyz(Val,nx,ny,nz,kx,ky,kz)
    Implicit none
    Integer(4) kx,ky,kz,nx,ny,nz,ncx,ncy,ncz
    Real(8) Val(3)
    ncx=max(1_4,nxcFld)
    ncy=max(1_4,nycFld)
    ncz=max(1_4,nzcFld)
    kx=Modulo(nx+idnint(Val(1))-1_4,ncx)+1_4
    ky=Modulo(ny+idnint(Val(2))-1_4,ncy)+1_4
    kz=Modulo(nz+idnint(Val(3))-1_4,ncz)+1_4
  end Subroutine getkxyz

  Subroutine getABdom(sFor,lf,a,la)
! read constants from formula strings xxx(v1,const1,const2,...) and save the values const1 in a(1), const2 in a(2)...
! used for Drude-Lorentz formula strings
    Implicit none
    Integer lf,i,l1,l2,l,idum,la
    Real(8) a(la)
    Character(*) sFor
    a(1:la)=0.0d0
    if(lf.gt.1151) return
    if(lf.lt.1) then
      call DelBlanks(sFor,lf)
      call ToLower(sFor,lf)
    end if
    if(lf.lt.6) return
    l1=lf
    l2=0
    do l=5,lf-1
      if(sFor(l:l).eq.',') then
        l1=l+1
        Exit
      end if
    end do
    if(l1.gt.lf-1) return
    do i=1,la-1
      do l=l1+1,lf-1
        if((sFor(l:l).ne.',').and.(sFor(l:l).ne.')')) Cycle
        l2=l-1
        Exit
      end do
      if(l2.lt.l1) Exit
		  call StrToRea(sFor(l1:l2),a(i),idum)
      if(idum.ne.0) then
        do l=l1+1,lf-1
          if(sFor(l:l).ne.')') Cycle
          l2=l
          Exit
        end do
        if(l2.lt.l1) Exit
        cCForm(0)=(0.0d0,1.0d0)
        cCForm(1)=DCmplx(Pi,0.0d0)
        cCForm(2)=DCmplx(Eps0,0.0d0)
        cCForm(3)=DCmplx(Mue0,0.0d0)
        cCForm(4)=DCmplx(Kw0,0.0d0)
        cCForm(5)=DCmplx(Zw0,0.0d0)
        pCForm(0)=(1.0d0,0.0d0)
        vCForm(0)=DCmplx(trFld,0.0d0)
        vCForm(1)=fcFld
        vCForm(2)=2.0d0*Pi*fcFld
        cFormu=cFormula(sFor(l1:l2),l2-l1+1,cCForm,pCForm,vCForm,5,0,2,1,1,1,idum)
        a(i)=Dble(cFormu(1))
      end if
      l1=l2+2
    end do
    if(l1.ge.lf) return
		call StrToRea(sFor(l1:lf-1),a(la),idum)
      if(idum.ne.0) then
        cCForm(0)=(0.0d0,1.0d0)
        cCForm(1)=DCmplx(Pi,0.0d0)
        cCForm(2)=DCmplx(Eps0,0.0d0)
        cCForm(3)=DCmplx(Mue0,0.0d0)
        cCForm(4)=DCmplx(Kw0,0.0d0)
        cCForm(5)=DCmplx(Zw0,0.0d0)
        pCForm(0)=(1.0d0,0.0d0)
        vCForm(0)=DCmplx(trFld,0.0d0)
        vCForm(1)=fcFld
        vCForm(2)=2.0d0*Pi*fcFld
        cFormu=cFormula(sFor(l1:lf-1),lf-l1,cCForm,pCForm,vCForm,5,0,2,1,1,1,idum)
        a(la)=Dble(cFormu(1))
      end if
  end Subroutine getABdom

END MODULE CHFOR




