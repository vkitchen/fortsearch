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

!> @brief checks if character is alphanumeric
function isalnum(c) result(out)
	character(len=1), intent(in) :: c
	logical :: out

	select case (ichar(c))
		case (48:57) ! numeric
			out = .TRUE.
		case (65:90) ! capital letters
			out = .TRUE.
		case (97:122) ! lowercase letters
			out = .TRUE.
		case default
			out = .FALSE.
	end select
end function isalnum

!> @brief returns new string containing only alphanumeric characters of original string
function clean(str) result(out)
	character(len=*), intent(in) :: str
	character(len=:), allocatable :: out

	integer :: i, j

	allocate(character(len=len(str)) :: out)

	j = 1
	do i = 1, len(str)
		if (isalnum(str(i:i))) then
			out(j:j) = str(i:i)
			j = j + 1
		end if
	end do
	out = out(:j-1)
end function clean

!> @brief converts a string to lowercase
subroutine lowercase(str)
	character(len=*), intent(inout) :: str

	integer :: i

	do i = 1, len(str)
		if ('A' <= str(i:i) .AND. str(i:i) <= 'Z') then
			str(i:i) = char(ichar(str(i:i)) + 32)
		end if
	end do
end subroutine lowercase

end module string_mod
