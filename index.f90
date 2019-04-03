program index
use file
use parser_mod
implicit none
	
	character(len=:), allocatable :: document_collection
	type(parser) :: prs

	document_collection = file_slurp("wsj.xml")

	call prs%initialize(document_collection)
	call prs%next()

	print *, len(document_collection)

end program index
