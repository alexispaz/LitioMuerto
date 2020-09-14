module constants

implicit none
public

integer, parameter    :: sp = kind(1.0)
integer, parameter    :: dp = kind(1.0d0)

integer, parameter    :: dm = 3
 
contains

integer function find_io(start)

!  find an unused unit number for input or output. unit n=start is used
!  if available; otherwise n is incremented until an unused unit is found.
!  unit numbers are limited to the range 1-100; if n reaches 100 the
!  search starts again at 1.

integer, intent(in) :: start
logical :: in_use, exists
integer :: n, n0
integer, parameter :: max_unit=99

n0=start
if (n0 <= 1 .or. n0 > max_unit) n0=1
n=n0
in_use=.true.
do while (in_use)
  inquire(n,opened=in_use,exist=exists)
  if (exists) then
    if (.not. in_use) exit
  else
    !FIXME write (unit=string,fmt="(a,i3,a)") "unit number", n, " out of range"
    !call report (string)
  endif
  n=n+1
  if (n > max_unit) n=1
  if (n == n0) then
    !FIXME call report ("no i/o unit available")
  end if
end do
find_io=n

end function find_io
     
end module
 
