module dynarray_mod
implicit none

type dynarray
	integer :: capacity, length
	integer, allocatable :: store(:)
contains
	procedure :: initialize
	procedure :: initialize_length
	procedure :: append
	procedure :: last
	procedure :: increment_last
	procedure :: print
	procedure :: write
	procedure :: read
	procedure :: set_length
end type dynarray

private :: initialize, initialize_length, append, last, increment_last, print, write, read, set_length

contains

subroutine initialize(a)
	class(dynarray) :: a
	a%length = 0
	a%capacity = 128
	allocate(a%store(a%capacity))
end subroutine initialize

subroutine initialize_length(a, length)
	class(dynarray) :: a
	integer :: length
	a%length = 0
	a%capacity = length
	allocate(a%store(a%capacity))
end subroutine initialize_length

subroutine append(a, val)
	class(dynarray) :: a
	integer, intent(in) :: val

	integer, allocatable :: buf(:)

	if (a%length == a%capacity) then
		call move_alloc(a%store, buf)
		a%capacity = a%capacity * 2
		allocate(a%store(a%capacity))
		a%store(1:a%length) = buf
		deallocate(buf)
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

subroutine write(a)
	class(dynarray) :: a

	write(10) a%length
	write(10) a%store(:a%length)
end subroutine write

subroutine read(a)
	class(dynarray) :: a

	read(10) a%length
	a%capacity = a%length
	allocate(a%store(a%capacity))
	read(10) a%store
end subroutine read

subroutine set_length(a, length)
	class(dynarray) :: a
	integer :: length

	a%length = min(a%length, length)
end subroutine set_length

end module dynarray_mod
