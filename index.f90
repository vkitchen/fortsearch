program index
use file_mod
use parser_mod
use bst_mod
use dynarray_string_mod
implicit none
	
	character(len=:), allocatable :: document_collection
	type(parser) :: prs
	type(token) :: tok
	type(dynarray_string) :: docnos
	type(bst) :: postings
	integer :: docid

	document_collection = file_slurp("wsj.xml")

	call docnos%initialize()
	call prs%initialize(document_collection)
	call postings%initialize()

	tok%what = word
	docid = 0
	do while (tok%what /= end)
		tok = prs%next()

		select case (tok%what)
			case (docno)
				docid = docid + 1
				call docnos%append(tok%value)
			case (word)
				if (len(tok%value) == 0) then
					cycle
				end if
				call postings%insert(tok%value, docid)
		end select
	end do

	open(unit=10, file="postings.dat", action="write", status="replace", &
		access="stream")
	call docnos%write()
	call postings%write()
	close(unit=10)

!	call postings%print()

end program index
