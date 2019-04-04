program index
use file_mod
use parser_mod
use bst_mod
implicit none
	
	character(len=:), allocatable :: document_collection
	type(parser) :: prs
	type(token) :: tok
	type(bst) :: postings
	integer :: docid

	document_collection = file_slurp("wsj.xml")

	call prs%initialize(document_collection)
	call postings%initialize()

	tok%what = word
	docid = 0
	do while (tok%what /= end)
		tok = prs%next()

		select case (tok%what)
			case (docno)
				docid = docid + 1
			case (word)
				if (len(tok%value) == 0) then
					cycle
				end if
				call postings%insert(tok%value, docid)
		end select
	end do

	call postings%print()

end program index
