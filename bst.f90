module bst_mod
use posting_mod
implicit none

type bst_node
	type(bst_node), pointer :: left, right
	character(len=:), allocatable :: key
	type(posting) :: post
end type bst_node

type bst
	type(bst_node), pointer :: root
	integer :: size
contains
	procedure :: initialize
	procedure :: insert
	procedure :: find
	procedure :: print
end type bst

private :: initialize, insert, find, print

contains

subroutine initialize(tree)
	class(bst) :: tree
	tree%size = 0
	nullify(tree%root)
end subroutine initialize

subroutine insert(tree, key, docid)
	class(bst) :: tree
	character(len=*), intent(in) :: key
	integer :: docid

	type(bst_node), pointer :: p

	tree%size = tree%size + 1

	if (.NOT. associated(tree%root)) then
		call make_node(tree%root, key, docid)
	end if

	p => tree%root

	do
		if (key < p%key) then
			if (.NOT. associated(p%left)) then
				call make_node(p%left, key, docid)
				return
			end if
			p => p%left
		else if (key > p%key) then
			if (.NOT. associated(p%right)) then
				call make_node(p%right, key, docid)
				return
			end if
			p => p%right
		else
			call p%post%append(docid)
			tree%size = tree%size - 1
			return
		end if
	end do
end subroutine insert

subroutine make_node(node, key, docid)
	type(bst_node), pointer, intent(inout) :: node
	character(len=*), intent(in) :: key
	integer, intent(in) :: docid

	allocate(node)
	nullify(node%left)
	nullify(node%right)
	node%key = key

	call node%post%initialize()
	call node%post%append(docid)
end subroutine make_node

function find(tree) result(out)
	class(bst) :: tree
	type(bst_node) :: out
end function find

subroutine print(tree)
	class(bst) :: tree
	call print_inorder(tree%root)
end subroutine print

recursive subroutine print_inorder(node)
	type(bst_node), pointer :: node
	if (.NOT. associated(node)) then
		return
	end if
	call print_inorder(node%left)
	print *, node%key
	call node%post%print()
	call print_inorder(node%right)
end subroutine print_inorder

end module bst_mod
