program main
use constants, only: dp
use types, only: readxyz

implicit none
 
integer               :: n,narg,carg,larg !number of arguments, counter and length
character(len=1000)   :: arg            !argument


!Check if any arguments are found
narg=command_argument_count()

!Check for optional arguments
if(narg/=1) then
  print *, 'ERROR: Input file should be given as argument'
  stop
endif
  
call get_command_argument(carg,arg,larg)
                 

call readentrada(trim(arg))


! Loop until delete all lithium atoms  
n=0
do while (n<natoms)
  
  i=rand
  compute i CN
  acept or reject
  if acept
    delete atom
  endif
  write
enddo


contains

subroutine readentrada(archivo)
use constants, only: findio
use types, only: readxyz
character(*),intent(in)   :: archivo
character(100)            :: xyz
integer                   :: i

 
! Open the file
u = find_io(30)
open(u,action='read',file=archivo)
                
read(u,*) archivo
read(u,*) i
call readxyz(trim(xyz),i)


close(30)

end subroutine

end program
