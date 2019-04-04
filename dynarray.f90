module dynarray_mod
implicit none

type dynarray
	integer :: capacity, length
	integer, allocatable :: store(:)
contains
	procedure :: initialize
	procedure :: append
	procedure :: last
	procedure :: increment_last
	procedure :: print
end type dynarray

private :: initialize, append, last, increment_last, print

contains

subroutine initialize(a)
	class(dynarray) :: a
	a%length = 0
	a%capacity = 128
	allocate(a%store(a%capacity))
end subroutine initialize

subroutine append(a, val)
	class(dynarray) :: a
	integer, intent(in) :: val

	integer, allocatable :: buf(:)

	if (a%length == a%capacity) then
		return
		a%capacity = a%capacity * 2
		allocate(buf(a%capacity))
		buf = a%store
		call move_alloc(buf, a%store)
	end if
	a%length = a%length + 1
	a%store(a%length) = val
end subroutine append

function last(a) result(out)
	class(dynarray) :: a
	integer :: out

	out = a%store(a%length)
end function last

subroutine increment_last(a)
	class(dynarray) :: a

	a%store(a%length) = a%store(a%length) + 1
end subroutine increment_last

subroutine print(a)
	class(dynarray) :: a

	integer :: i

	do i = 1, a%length
		write(*, fmt="(I8)", advance="no") a%store(i)
	end do
	print *, ''
end subroutine print

end module dynarray_mod
