program index
use file
use parser_mod
implicit none
	
	character(len=:), allocatable :: document_collection
	type(parser) :: prs
	type(token) :: tok
	integer :: i

	document_collection = file_slurp("wsj.xml")

	call prs%initialize(document_collection)

	do i = 1, 10
		tok = prs%next()

		select case (tok%what)
			case (docno)
				print *, "*DOCNO*"
				print *, tok%value
			case (word)
				print *, "*WORD*"
				print *, tok%value
				print *, len(tok%value)
			case (end)
				print *, "*END*"
			case default
				print *, "*UNKNOWN*"
		end select
	end do

end program index
