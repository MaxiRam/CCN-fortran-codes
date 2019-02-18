module diag_dom_utility
contains
    subroutine to_diag_dom(n,m,A,out_status)
    !This routime checks if a given matriz A can be converted to diagonally
    !dominant form, and in case that is true, it makes the conversion and returns
    !the diagonally dominant matrix.
    !
    !For the matrix to be convertible, two conditions must be satisfied:
    ! #1 The largest elements (in abs value) of each row must all belong to
    !    different columns, so the can be placed at the main diagonal by
    !    changing rows or columns.
    ! #2 The largest element of each row must be 'dominant', that is, must be
    !    grater than the sum of the abs value of all other elements in the
    !    same row.
    !
    !If both conditions are met the routine returns out_status=.TRUE. and the
    !modified matrix in array A.
    !If any of the conditions is not met, the routine returns out_status=.FALSE.
    !and the original matriz in array A.
        implicit none
        !Declaration of arguments
        integer(2), intent(in) :: n,m
        integer(2), intent(inout) :: A(n,m)
        logical, intent(out) :: out_status

        !Declaration of internal variables
        integer(2) :: i
        integer(2) :: m_index(n), sum_row(n)
        logical :: max_are_dominant, max_in_dif_col


        !Find the position of the largest element (in abs value)
        !for each row.
        !Read about MAXLOC function at: https://gcc.gnu.org/onlinedocs/gcc-4.4.3/gfortran/MAXLOC.html
        m_index=maxloc(abs(A),dim=2)

        !Check if the largest element of each row is in a different column
        !then, after rearranging rows it will be possible to place the those
        !elements in the main diagonal of the matrix.
        max_in_dif_col=all_different(m_index)

        !Compute the summation of all elements in each row.
        !Substracting the value of the row's maximum we get the sum of all other elements
        !Substracting the value of the row's maximum again, if the maximums are dominant
        !we get an array with all negative values.
        sum_row=0
        do i=1,n
            sum_row(i)=sum(abs(A(i,:)))-2*abs(A(i,m_index(i)))
        enddo

        !Check if all values are negative. If that is the case we set
        !max_are_dominant to true because the matriz mets the first conditions
        !to be convertible to diagonally dominant form
        if (all(sum_row<0)) max_are_dominant=.true.

        if ((max_are_dominant).and.(max_in_dif_col)) then
            call convert_to_diag_dom(A, m_index)
            out_status=.true.
        else
            out_status=.false.
        endif
    end subroutine

    logical function all_different(arr)
        !This function returns TRUE if all elements of array 'arr'
        !are different to each other. If not, it returns FALSE.
        implicit none
        !Declaration of arguments
        integer(2), intent(in) :: arr(:)  !<----This is an automatic array not
                                          !a dynamic array (allocatable)
                                          !Ask Google about it or go to consulta

        !Declaration of internal variables
        integer(2) :: i

        all_different=.true.

        !Read about intrisinc function SIZE at:
        !https://gcc.gnu.org/onlinedocs/gcc-4.4.3/gfortran/SIZE.html#SIZE
        do i=1,size(arr)-1
          !Read about intrisinc function ANY at:
          !https://gcc.gnu.org/onlinedocs/gcc-4.4.3/gfortran/ANY.html#ANY
          if (any(arr(i)==arr(i+1:size(arr)))) then
            all_different=.false.
            exit
          endif
        enddo

    end function

    subroutine convert_to_diag_dom(M,m_index)
      !This routine places each row in the correspondig place.
        implicit none
        integer(2), intent(in) :: m_index(:)
        integer(2), intent(inout) :: M(:,:)

        integer(2) :: i
        integer(2) :: aux(size(M,1),size(M,2))

        do i=1,size(M,1)
            aux(m_index(i),:)=M(i,:)
        enddo

        M=aux

    end subroutine convert_to_diag_dom
end module diag_dom_utility

program test_ddomin
      use diag_dom_utility
      implicit none
      integer(2) :: i,n,m
      integer(2), allocatable :: A(:,:)   !This is a dynamic array (allocatable)
      logical :: ddiag_status

      open(unit=10,file='datos.in',status='old')

      read(10,*) n,m
      allocate(A(n,m))

      do i=1,n
        read(10,*) A(i,:)
        write(*,*) A(i,:)
      enddo
      close(10)

      call to_diag_dom(n,m,A,ddiag_status)
      print *, "---------------------------------"

      if (ddiag_status) then
        do i=1,size(A,1)
          print *, A(i,:)
        enddo
      else
        print *, " "
        print *, "The matrix cannot be converted to diagonally dominant form"
      endif

end program
