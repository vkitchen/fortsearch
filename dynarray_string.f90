module dynarray_string_mod
implicit none

type dynarray_string
	integer :: capacity, length
	character(len=14), allocatable :: store(:)
contains
	procedure :: initialize
	procedure :: append
	procedure :: last
	procedure :: print
	procedure :: write
	procedure :: read
end type dynarray_string

private :: initialize, append, last, print, write, read

contains

subroutine initialize(a)
	class(dynarray_string) :: a

	a%length = 0
	a%capacity = 128
	allocate(character(14) :: a%store(a%capacity))
end subroutine initialize

subroutine append(a, val)
	class(dynarray_string) :: a
	character(len=*), intent(in) :: val

	character(len=:), allocatable :: buf(:)

	if (a%length == a%capacity) then
		call move_alloc(a%store, buf)
		a%capacity = a%capacity * 2
		allocate(character(14) :: a%store(a%capacity))
		a%store(1:a%length) = buf
		deallocate(buf)
	end if
	a%length = a%length + 1
	a%store(a%length) = val
end subroutine append

function last(a) result(out)
	class(dynarray_string) :: a
	character(len=:), allocatable :: out

	out = a%store(a%length)
end function last

subroutine print(a)
	class(dynarray_string) :: a

	integer :: i

	do i = 1, a%length
		print *, a%store(i)
	end do
	print *, ''
end subroutine print

subroutine write(a)
	class(dynarray_string) :: a

	write(10) a%length
	write(10) a%store(:a%length)
end subroutine write

subroutine read(a)
	class(dynarray_string) :: a

	read(10) a%length
	a%capacity = a%length
	allocate(character(14) :: a%store(a%capacity))
	read(10) a%store
end subroutine read

end module dynarray_string_mod
