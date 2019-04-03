module string_mod
implicit none

contains

!> @brief finds whether a string is the prefix of another
function prefix(pre, str) result(out)
	character(len=*), intent(in) :: pre
	character(len=*), intent(in) :: str
	logical :: out

	integer :: length
	integer :: i

	length = len(pre)
	out = .TRUE.

	if (length > len(str)) then
		out = .FALSE.
		return
	end if

	do i = 1, length
		if (pre(i:i) /= str(i:i)) then
			out = .FALSE.
			exit
		end if
	end do
end function prefix

!> @brief checks if character is whitespace
function isspace(c) result(out)
	character(len=1), intent(in) :: c
	logical :: out

	select case (ichar(c))
		case (9:13) ! horizontal tab, newline, vertical tab, form-feed, carriage return
			out = .TRUE.
		case (32) ! space
			out = .TRUE.
		case default
			out = .FALSE.
	end select
end function isspace

end module string_mod
