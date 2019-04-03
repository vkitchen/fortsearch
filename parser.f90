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
	procedure :: next
end type parser

contains

subroutine initialize(p, str)
	class(parser) :: p
	character(len=*) :: str

	p%str = str
	p%index = 1
end subroutine initialize

subroutine next(p)
	class(parser) :: p

	logical :: t

	print *, p%index
	print *, p%str(:7)
	t = prefix("cat", "catacomb")
	print *, t
	t = prefix("catd", "catacomb")
	print *, t
	t = prefix("catcat", "cat")
	print *, t
	t = prefix("", "cat")
	print *, t
	t = isspace(' ')
	print *, t
	t = isspace('c')
	print *, t
end subroutine next

end module parser_mod
