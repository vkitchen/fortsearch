program index
use file
implicit none
	
	character(len=:), allocatable :: document_collection
	document_collection = file_slurp("wsj.xml")

	print *, len(document_collection)

end program index
