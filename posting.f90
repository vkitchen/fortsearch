module posting_mod
use dynarray_mod
implicit none

type posting
	type(dynarray) :: id_store, count_store
contains
	procedure :: initialize
	procedure :: append
	procedure :: print
	procedure :: write
	procedure :: read
end type posting

private :: initialize, append, print, write, read

contains

subroutine initialize(p)
	class(posting) :: p
	call p%id_store%initialize()
	call p%count_store%initialize()
end subroutine initialize

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

end module posting_mod
