module types
use constants, only:dp, dm

implicit none
public

type :: atom

  ! ----- Identidad
  real(dp)          :: mass
  character(2)      :: sym
  logical           :: existe=.true.

  ! ----- Propiedades mecanicas
  real(dp)  :: pos(dm),  &
               force(dm),&
               acel(dm), &
               vel(dm)
  ! ----- Propiedades de movimiento
  logical                :: pbc(3)=[.true.,.true.,.false.]

  ! ----- Lista de vecinos
  real(dp),dimension(dm) :: pos_old=1.e8_dp
  real(dp)               :: maxdisp2=0 !desplazamiento maximo a un determinada T de grupo


  ! ----- Otras
  real(dp)               :: bo ! Bond Order


end type atom

type(atom),allocatable   :: a(:)
integer                  :: natoms

contains


subroutine readxyz(archivo,frame, a)
! Read frame `frame` of xyz file `archivo` and save coordinates and symbol into `a` atom array
use gems_constants, only: find_io
character(*),intent(in)            :: archivo
integer,intent(in),optional        :: frame
integer                            :: i,j,u,io
type(atom),allocatable,intent(out) :: a(:)
character(2)                       :: sym

! WARNING: it deallocate `a` if is already allocated
if(allocated(a)) deallocate(a)

! Open the file
u = find_io(30)
open(u,action='read',file=archivo)

! In case a specific frame wants to be loaded
if(present(frame)) then
  do i=1,frame-1
    read(u,*) natoms
    read(u,*)
    do j = 1, natoms
      read(u,*)
    enddo
  enddo
endif

! Reading the frame
read(u,*) natoms
allocate(a(natoms))
read(u,*)
do i = 1, natoms
  read(u,*) a(i)%sym, a(i)%pos(:)

  select case(a(i)%sym)
  case('CG')
  case('Li')
    a(i)%existe=.false.
    a(i)%pos(:)=[0,0,0]
  case default
    print *, 'WARNING: atom type unknown'
  end select

enddo

close(u)

end subroutine 

subroutine writexyz(a,u)
! Read frame `frame` of xyz file `archivo` and save coordinates and symbol into `a` atom array
use constants, only: find_io
integer                            :: i,j,u
type(atom),allocatable,intent(in)  :: a(:)
character(2)                       :: sym

! WARNING: it deallocate `a` if is already allocated
if(.not.allocated(a)) then
  print *, 'error'
  stop
endif

write(u,*) natoms
write(u,*)
do i = 1, natoms

  if(a(i)%existe) then
    write(u,*) a(i)%sym, a(i)%pos(:)
  else
    write(u,*) a(i)%sym, 0,0,0
  endif

enddo

end subroutine 
 
end module
 
