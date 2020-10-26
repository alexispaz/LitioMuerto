program main
use gems_constants, only: dp,dm
use types, only: writexyz, a, natoms, atom
use gems_random, only: std_init, init_ran, free_ran, ranu

implicit none
 
! Input
integer                  :: nsteps,each
real(dp)                 :: r1,r2
real(dp)                 :: box(dm)
real(dp)                 :: bcut
 
integer               :: i,n,narg,carg,larg,ns !number of arguments, counter and length
character(len=1000)   :: arg            !argument


! Init default random seed
call std_init()
call init_ran()
 
!Check if any arguments are found
narg=command_argument_count()

!Check for optional arguments
if(narg/=1) then
  print *, 'ERROR: Input file should be given as argument'
  stop
endif
  
call get_command_argument(1,arg,larg)
                 
! Input
call readentrada(trim(arg))

! Compute Bond Order
call all_bond_order(r1,r2)

! Open out
open(13,action='write',file='out.xyz')
                    
! Loop until delete all lithium atoms  
n=0
do ns=1,nsteps
  
  i=int(ranu()*natoms)+1
  
  if (a(i)%bo<bcut) then
    a(i)%existe=.false.
  endif

  if(mod(ns,each)==0) call writexyz(a,13)
  
enddo

close(13)


! Free default random seed
call free_ran()

contains

subroutine readentrada(archivo)
use gems_constants, only: find_io
use types, only: readxyz
character(*),intent(in)   :: archivo
character(100)            :: xyz
integer                   :: i,u
 
! Open the file
u = find_io(30)
open(u,action='read',file=archivo)
                
read(u,*) xyz
read(u,*) i
read(u,*) box(dm)
read(u,*) nsteps, each
read(u,*) r1
read(u,*) r2
read(u,*) bcut
call readxyz(trim(xyz),i,a)

close(30)

end subroutine

function bond_order(r,r1,r2)
use gems_constants, only: pi
real(dp),intent(in)  :: r,r1,r2
real(dp)             :: aux
real(dp)             :: bond_order

if(r<r1) then
  bond_order=1.0_dp
elseif(r>=r2) then
  bond_order=0.0_dp
else
  aux=0.5_dp*(r1+r2)
  aux=pi*(r-aux)/(r2-r1) 
  bond_order = 0.5_dp-sin(aux)*0.5_dp
endif      
end function 
 
subroutine all_bond_order(r1,r2)
real(dp),intent(in)          :: r1,r2
real(dp)                     :: vd(dm),rd
integer                      :: i,j

do i=1,natoms
  do j=i+1,natoms

    vd = vdistance(a(i),a(j))
    rd = sqrt(dot_product(vd,vd))
    rd = bond_order(rd,r1,r2)

    a(i)%bo = a(i)%bo + rd
    a(j)%bo = a(j)%bo + rd

  enddo
enddo

end subroutine all_bond_order
                          

function vdistance(i,j) result(vd)
!calculates the distance of two atoms with or without minimum image convention
real(dp),dimension(dm)  :: vd
type(atom),intent(in)   :: i,j
logical                 :: pbc(dm)
integer                 :: l
  
! Distancia
vd(:)=i%pos(:)-j%pos(:)

! Mas rapido usar idnint que un if
pbc(:)=i%pbc(:).or.j%pbc(:)
do l = 1,dm
  if (pbc(l)) vd(l)=vd(l)-box(l)*idnint(vd(l)/box(l))
enddo

end function vdistance


end program
