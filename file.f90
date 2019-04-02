module file
implicit none

contains
	function file_slurp(filename) result(file)
		character(len=*), intent(in) :: filename
		integer :: filesize
		character(len=:), allocatable :: file

		open(unit=10, file=filename, action="read", &
			form="unformatted", access="stream")
		inquire(unit=10, size=filesize)
		allocate(character(len=filesize) :: file)
		read(10) file
		close(10)
	end function
end module file
