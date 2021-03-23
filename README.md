OpenMaXwell, a general MaXwell solver, Version 2021A\
Copyright 2021, Christian Hafner

Requirements for compiling the source code:
1) Install Microsoft Visual Studio 2019 with the MFC support included (Community version is used)
2) Install Intel OneAPI Base Toolkit (https://software.intel.com/content/www/us/en/develop/tools/oneapi/base-toolkit/download.html)
3) Install Intel OneAPI HPC Toolkit (https://software.intel.com/content/www/us/en/develop/tools/oneapi/hpc-toolkit/download.html)
5) Clone the files from this repository by using the address: https://github.com/openmaxwell/openmax_source_code_32/
6) The locations of the MKL and Compiler libraries should be updated in MaX.sln (if they are installed to a non-default location) under:\
  a) Properties -- Linker -- Additional Library Directories\
  b) Properties -- Fortran -- Additional Library Directories
7) Build the source code by using Debug-Win32 or Release-Win32 Configurations.

  Original WWW home page: http://OpenMaX.ethz.ch - now in a repository of the ETH library at\
  https://rossa-prod-ap21.ethz.ch:8443/delivery/DeliveryManagerServlet?dps_pid=IE13673599 (Accessed 23.03.2021)\
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either  version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.\
  If you publish results obtained with this software you must mention which of the results were obtained with OpenMaXwell.
