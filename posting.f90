module posting_mod
use, intrinsic :: iso_fortran_env
use dynarray_mod
use dynarray_string_mod
implicit none

type posting
	type(dynarray) :: id_store, count_store
	real(REAL64), allocatable :: rsv_store(:)
contains
	procedure :: initialize
	procedure :: initialize_length
	procedure :: append
	procedure :: print
	procedure :: write
	procedure :: read
	procedure :: compute_rsv
	procedure :: set_length
	procedure :: merge_with
	procedure :: sort
	procedure :: results_print
end type posting

private :: initialize, initialize_length, append, print, write, read, compute_rsv, set_length, merge_with, sort, results_print

contains

subroutine initialize(p)
	class(posting) :: p
	call p%id_store%initialize()
	call p%count_store%initialize()
end subroutine initialize

subroutine initialize_length(p, length)
	class(posting) :: p
	integer :: length
	call p%id_store%initialize_length(length)
	call p%count_store%initialize_length(length)
end subroutine initialize_length

subroutine append(p, docid)
	class(posting) :: p
	integer, intent(in) :: docid

	if (p%id_store%length /= 0 .AND. p%id_store%last() == docid) then
		call p%count_store%increment_last()
	else
		call p%id_store%append(docid)
		call p%count_store%append(1)
	end if
end subroutine append

subroutine print(p)
	class(posting) :: p

	call p%id_store%print()
	call p%count_store%print()
end subroutine print

subroutine write(p)
	class(posting) :: p

	call p%id_store%write()
	call p%count_store%write()
end subroutine write

subroutine read(p)
	class(posting) :: p

	call p%id_store%read()
	call p%count_store%read()
end subroutine read

subroutine compute_rsv(p, count)
	class(posting) :: p
	integer, intent(in) :: count

	integer :: i
	real(REAL64) :: length, doccount

	if (allocated(p%rsv_store)) then
		return
	end if

	length = p%count_store%length
	doccount = count
	allocate(p%rsv_store(p%count_store%length))

	do i = 1, p%count_store%length
		p%rsv_store(i) = p%count_store%store(i) / (length / doccount)
	end do
end subroutine compute_rsv

subroutine set_length(p, length)
	class(posting) :: p
	integer :: length
	call p%id_store%set_length(length)
	call p%count_store%set_length(length)
end subroutine set_length

function merge_with(p, with) result(out)
	class(posting) :: p
	type(posting) :: with
	type(posting) :: out

	integer :: size, left, right

	if (.NOT. allocated(p%id_store%store)) then
		out = with
		return
	end if

	size = max(p%id_store%length, with%id_store%length)
	call out%initialize_length(size)
	allocate(out%rsv_store(size))

	left = 1
	right = 1
	do while (left < p%id_store%length + 1 .AND. right < with%id_store%length + 1)
		if (p%id_store%store(left) < with%id_store%store(right)) then
			left = left + 1
		else if (p%id_store%store(left) > with%id_store%store(right)) then
			right = right + 1
		else
			out%id_store%length = out%id_store%length + 1
			out%id_store%store(out%id_store%length) = p%id_store%store(left)

			out%rsv_store(out%id_store%length) = p%rsv_store(left) + with%rsv_store(right)

			left = left + 1
			right = right + 1
		end if
	end do

end function merge_with

subroutine sort(p)
	class(posting) :: p

	integer :: i, j, id_temp
	real(REAL64) :: rsv_temp
	do i = 2, p%id_store%length
		id_temp = p%id_store%store(i)
		rsv_temp = p%rsv_store(i)
		j = i - 1
		do while (j >= 1 .AND. p%rsv_store(j) < rsv_temp)
			p%id_store%store(j+1) = p%id_store%store(j)
			p%rsv_store(j+1) = p%rsv_store(j)
			j = j - 1
		end do
		p%id_store%store(j+1) = id_temp
		p%rsv_store(j+1) = rsv_temp
	end do
end subroutine sort

subroutine results_print(p, docnos)
	class(posting) :: p
	type(dynarray_string), intent(in) :: docnos

	integer :: i

	do i = 1, p%id_store%length
		print "(A,F10.2)", docnos%store(p%id_store%store(i)), p%rsv_store(i)
	end do
end subroutine results_print

end module posting_mod
