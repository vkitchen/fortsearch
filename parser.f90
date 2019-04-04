module parser_mod
use string_mod
implicit none

enum, bind(c)
	enumerator :: token_type = 0
	enumerator :: docno, word, end
end enum

type token
	character(len=:), allocatable :: value
	integer(kind(token_type)) :: what
end type token

type parser
	character(len=:), allocatable :: str
	integer :: index
contains
	procedure :: initialize
	procedure :: advance
	procedure :: next
end type parser

contains

subroutine initialize(p, str)
	class(parser) :: p
	character(len=*) :: str

	p%str = str
	p%index = 1
end subroutine initialize

subroutine advance(p)
	class(parser) :: p

	integer :: length
	length = len(p%str)

	do
		if (p%index == length) then
			return
		end if

		do while (p%index < length .AND. isspace(p%str(p%index:p%index)))
			p%index = p%index + 1
		end do

		if (prefix("<DOCNO>", p%str(p%index:))) then
			return
		else if (p%str(p%index:p%index) == '<') then
			p%index = p%index + index(p%str(p%index:), '>')
		else
			return
		end if
			
			
	end do
end subroutine advance

function next(p) result(out)
	class(parser) :: p
	type(token) :: out

	integer :: length
	integer :: slice

	length = len(p%str)

	call p%advance()

	if (p%index == length) then
		out%what = end
		return
	end if

	if (prefix("<DOCNO>", p%str(p%index:))) then
		p%index = p%index + len("<DOCNO>")

		call p%advance()

		slice = index(p%str(p%index:), "</DOCNO>")

		out%what = docno
		out%value = trim(p%str(p%index:p%index+slice-2))

		p%index = p%index + slice-1 + len("</DOCNO>")
		return
	end if

	slice = 0
	do while (p%index+slice < length .AND. .NOT. isspace(p%str(p%index+slice:p%index+slice)))
		slice = slice + 1
	end do

	out%what = word
	out%value = p%str(p%index:p%index+slice-1)
	out%value = clean(out%value)
	call lowercase(out%value)

	p%index = p%index + slice

end function next

end module parser_mod
