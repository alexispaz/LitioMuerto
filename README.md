# Compilation 

Instead, if you want to clone from GitHub then:

    git clone git@github.com:alexispaz/LitioMuerto.git
    cd LitioMuerto
    autoreconf -fi
    ./configure
    make

Further compiling options are handle
in a standar way. For instance:

    export PATH=/share/apps/gcc/6.2.0/bin/:$PATH
    export LD_LIBRARY_PATH=/share/apps/gcc/6.2.0/lib64:$LD_LIBRARY_PATH
    export FC=gfortran
    export FCFLAGS=-fno-use-linker-plugin
	./configure --disable-openmp FCFLAGS='-Ofast'

See `./configure --help` for more information

To create a tar ball to run in other computers without autotools use:
  
    make dist

then you only need to configure and make (i.e. `./configure; make`).

# About

LitioMuerto code is hosted in [github](https://github.com/alexispaz/LitioMuerto). 
It use several lines of code from GEMS. Licenses and copyright notices for GEMS
can be found in its github [repo](https://github.com/alexispaz/GEMS). 

## License

Copyright (C) 2020  Sergio Alexis Paz

LitioMuerto is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
   either version 3 of the License, or (at your option) any later version.

LitioMuerto is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
LitioMuerto. If not, see <https://www.gnu.org/licenses/>.
