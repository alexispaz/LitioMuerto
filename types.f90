module types
use constants, only:dp, dm

implicit none
public

type :: atom

  ! ----- Identidad
  real(dp)          :: mass
  character(2)      :: sym

  ! ----- Propiedades mecanicas
  real(dp)  :: pos(dm),  &
               force(dm) &
               acel(dm)  &
               vel(dm)

  ! ----- Propiedades de movimiento
  logical                :: pbc(dm)=.false.

  ! ----- Lista de vecinos
  real(dp),dimension(dm) :: pos_old =1.e8_dp
  real(dp)               :: maxdisp2=0 !desplazamiento maximo a un determinada T de grupo

end type atom


type(atom),allocatable   :: a(:)
integer                  :: natoms


contains


subroutine readxyz(archivo,frame, a)
! Read frame `frame` of xyz file `archivo` and save coordinates and symbol into `a` atom array
use constants, only: findio
character(*),intent(in)           :: archivo
integer,intent(in),optional       :: frame
integer                           :: i,j,u,io
type(atom),allocatable,intent(in) :: a(:)
character(2)                      :: sym

! WARNING: it deallocate `a` if is already allocated
if(allocated(a)) deallocate(a)

! Open the file
u = find_io(30)
open(u,action='read',file=archivo)

! In case a specific frame wants to be loaded
if(present(frame)) then
  do i=1,frame-1
    read(u,*) j
    read(u,*)
    do j = 1, j
      read(u,*)
    enddo
  enddo
endif

! Reading the frame
read(u,*) natoms
allocate(a(natoms))
read(u,*)
do i = 1, j
  read(u,*) a%sym, a%pos(:)

  select case(a%sym)
  case('CG')
  case('Li')
  case default
    print *, 'WARNING: atom type unknown'
  end select

enddo

close(u)

end subroutine 

end module
 
