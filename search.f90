program search
use dynarray_string_mod
use posting_mod
use parser_mod
implicit none

	type postptr
		type(posting), pointer :: ptr
	end type postptr


	type(parser) :: prs
	type(token) :: tok

	type(dynarray_string) :: docnos
	character(len=:), allocatable :: dictionary(:)
	type(postptr), allocatable :: postings(:)
	type(posting), pointer :: posttemp
	character(len=:), allocatable :: readword
	integer :: i, numrecs, recsize, result

	character(len=1024) :: query

	open(unit=10, file="postings.dat", action="read", access="stream")
	call docnos%read()
	read(10) numrecs
	allocate(character(128) :: dictionary(numrecs))
	do i = 1, numrecs + 1
		read(10) recsize
		allocate(character(len=recsize) :: readword)
		read(10) readword
		dictionary(i) = readword
		deallocate(readword)
	end do

	allocate(postings(numrecs))
	do i = 1, numrecs + 1
		allocate(posttemp)
		call posttemp%read()
		postings(i)%ptr => posttemp
	end do

	close(10)

! END READ

	print *, size(dictionary)

	read "(A)", query
	print *, "*******"
	call prs%initialize(query)
	tok%what = word
	do while (tok%what /= end)
		tok = prs%next()

		select case (tok%what)
			case (word)
				print *, tok%value
				result = binary_search(dictionary, tok%value)
				print *, result
		end select
	end do
	print *, "*******"

contains

	function binary_search(dictionary, key) result(out)
		character(len=*), intent(in) :: dictionary(:)
		character(len=*), intent(in) :: key
		integer :: low, mid, high, out

		low = 0
		high = size(dictionary) - 1
		do while (low <= high)
			mid = (low + high) / 2
			if (key < dictionary(mid+1)) then
				high = mid - 1
			else if (key > dictionary(mid+1)) then
				low = mid + 1
			else
				out = mid+1
				return
			end if
		end do
		out = -1
	end function binary_search


end program search
