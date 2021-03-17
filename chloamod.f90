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
MODULE CHLOA

Contains

Integer(4) Function LoadImag(BmpFileName,ix,iy)
! call LoadImage from the appropriate CVF or IVF library (DFLIB or IFQWIN)
  USE IFQWIN !IVF uses this libraray
!CVF  USE DFLIB !CVF uses this libraray
  Implicit none
  Character*(*) BmpFileName
  Integer(4) ix,iy
  LoadImag=LoadImage(BmpFileName,ix,iy)
end Function LoadImag

END MODULE CHLOA