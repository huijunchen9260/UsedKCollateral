module io

    use iso_Fortran_env, only: rk => real64, ik => int32
    implicit none
    private
    public operator(.f.), operator(.d.), pprint, head, tail

    interface operator(.f.)
        module procedure file_exists
    end interface

    interface operator(.d.)
        module procedure directory_exists
    end interface

    ! print the whole matrix
    interface pprint
        module procedure pprint_1d_int
        module procedure pprint_1d_real
        module procedure pprint_2d_int
        module procedure pprint_2d_real
        module procedure pprint_2d_logical
        module procedure pprint_3d_int
        module procedure pprint_3d_real
        module procedure pprint_3d_logical
    end interface

    ! print the head of matrix, default 10 rows
    interface head
        module procedure head_int
        module procedure head_real
    end interface head

    ! print the tail of matrix, default 10 rows
    interface tail
        module procedure tail_int
        module procedure tail_real
    end interface tail

contains

    function file_exists(filename) result(res)
        implicit none
        character(len=*),intent(in) :: filename
        logical                     :: res
        ! Check if the file exists
        inquire( file=trim(filename), exist=res )
    end function file_exists

    function directory_exists(dirname) result(res)
        implicit none
        character(len=*),intent(in) :: dirname
        logical                     :: res
        ! Check if the file exists
        inquire( file=trim(dirname // '.' ), exist=res )
    end function directory_exists

    subroutine pprint_3d_logical(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        logical, intent(in) :: mat(:, :, :)
        integer(ik), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(ik) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
        character(len=30) :: f, str

        res = shape(mat)
        one_b = 1; if (present(one_begin)) one_b = one_begin
        one_e = res(1); if (present(one_end)) one_e = one_end
        two_b = 1; if (present(two_begin)) two_b = two_begin
        two_e = res(2); if (present(two_end)) two_e = two_end
        three_b = 1; if (present(three_begin)) three_b = three_begin
        three_e = res(3); if (present(one_begin)) three_e = three_end

        do i = three_b, three_e, 1
            write(*, '((a, i1, a))') '[:, :, ', i, '] = '
            call pprint_2d_logical(mat(:, :, i), one_b, one_e, two_b, two_e)
            write(*, *) ''
        enddo
    end subroutine pprint_3d_logical

    subroutine pprint_3d_real(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        real(rk), intent(in) :: mat(:, :, :)
        integer(ik), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(ik) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
        character(len=30) :: f, str

        res = shape(mat)
        one_b = 1; if (present(one_begin)) one_b = one_begin
        one_e = res(1); if (present(one_end)) one_e = one_end
        two_b = 1; if (present(two_begin)) two_b = two_begin
        two_e = res(2); if (present(two_end)) two_e = two_end
        three_b = 1; if (present(three_begin)) three_b = three_begin
        three_e = res(3); if (present(one_begin)) three_e = three_end

        do i = three_b, three_e, 1
            write(*, '((a, i1, a))') '[:, :, ', i, '] = '
            call pprint_2d_real(mat(:, :, i), one_b, one_e, two_b, two_e)
            write(*, *) ''
        enddo
    end subroutine pprint_3d_real

    subroutine pprint_3d_int(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        integer(ik), intent(in) :: mat(:, :, :)
        integer(ik), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(ik) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
        character(len=30) :: f, str

        res = shape(mat)
        one_b = 1; if (present(one_begin)) one_b = one_begin
        one_e = res(1); if (present(one_end)) one_e = one_end
        two_b = 1; if (present(two_begin)) two_b = two_begin
        two_e = res(2); if (present(two_end)) two_e = two_end
        three_b = 1; if (present(three_begin)) three_b = three_begin
        three_e = res(3); if (present(one_begin)) three_e = three_end

        do i = three_b, three_e, 1
            write(*, '((a, i1, a))') '[:, :, ', i, '] = '
            call pprint_2d_int(mat(:, :, i), one_b, one_e, two_b, two_e)
            write(*, *) ''
        enddo
    end subroutine pprint_3d_int

    subroutine pprint_2d_logical(mat, rowbegin, rowend, colbegin, colend)
        logical, intent(in) :: mat(:, :)
        integer(ik), optional :: rowbegin, rowend, colbegin, colend
        integer(ik) :: res(2), i, j, rb, re, cb, ce
        character(len=30) :: f, str(2)
        res = shape(mat)

        rb = 1; if (present(rowbegin)) rb = rowbegin
        re = res(1); if (present(rowend)) re = rowend
        cb = 1; if (present(colbegin)) cb = colbegin
        ce = res(2); if (present(colend)) ce = colend

        write(unit=str, fmt='(I0)') res
        f = trim('(' // trim(str(2)) // 'L2)')
        do i = rb, re, 1
            write(*, f) ( mat(i,j), j = cb, ce, 1 )
        enddo
    end subroutine pprint_2d_logical

    subroutine pprint_2d_real(mat, rowbegin, rowend, colbegin, colend)
        real(rk), intent(in) :: mat(:, :)
        integer(ik), optional :: rowbegin, rowend, colbegin, colend
        integer(ik) :: res(2), i, j, rb, re, cb, ce
        character(len=30) :: f, str(2)
        res = shape(mat)

        rb = 1; if (present(rowbegin)) rb = rowbegin
        re = res(1); if (present(rowend)) re = rowend
        cb = 1; if (present(colbegin)) cb = colbegin
        ce = res(2); if (present(colend)) ce = colend

        write(unit=str, fmt='(I0)') res
        f = trim('(' // trim(str(2)) // 'E13.5)')
        do i = rb, re, 1
            write(*, f) ( mat(i,j), j = cb, ce, 1 )
        enddo
    end subroutine pprint_2d_real

    subroutine pprint_2d_int(mat, rowbegin, rowend, colbegin, colend)
        integer(ik), intent(in) :: mat(:, :)
        integer(ik), optional :: rowbegin, rowend, colbegin, colend
        integer(ik) :: res(2), i, j, rb, re, cb, ce
        character(len=30) :: f, s, str(2)
        res = shape(mat)

        rb = 1; if (present(rowbegin)) rb = rowbegin
        re = res(1); if (present(rowend)) re = rowend
        cb = 1; if (present(colbegin)) cb = colbegin
        ce = res(2); if (present(colend)) ce = colend

        write(unit=str, fmt='(I0)') res
        write(unit=s, fmt='(I0)') maxval(mat)
        write(unit=s, fmt='(I0)') len_trim(s) + 1
        f = trim('(' // trim(str(2)) // 'i' // trim(s) // ')')
        do i = rb, re, 1
            write(*, f) ( mat(i,j), j = cb, ce, 1 )
        enddo
    end subroutine pprint_2d_int

    subroutine pprint_1d_real(mat, rowbegin, rowend)
        real(rk), intent(in) :: mat(:)
        integer(ik), optional :: rowbegin, rowend
        integer(ik) :: res, i, rb, re
        character(len=30) :: f, s, str

        res = size(mat)
        rb = 1; if (present(rowbegin)) rb = rowbegin
        re = res; if (present(rowend)) re = rowend

        write(unit=str, fmt='(I0)') res
        f = trim('(' // trim(str) // 'E13.5)')
        do i = rb, re, 1
            write(*, f) ( mat(i) )
        enddo

    end subroutine pprint_1d_real

    subroutine pprint_1d_int(mat, rowbegin, rowend)
        integer(ik), intent(in) :: mat(:)
        integer(ik), optional :: rowbegin, rowend
        integer(ik) :: res, i, rb, re
        character(len=30) :: f, s, str

        res = size(mat)
        rb = 1; if (present(rowbegin)) rb = rowbegin
        re = res; if (present(rowend)) re = rowend

        write(unit=str, fmt='(I0)') res
        write(unit=s, fmt='(I0)') maxval(mat)
        write(unit=s, fmt='(I0)') len_trim(s) + 1
        f = trim('(' // trim(str) // 'i' // trim(s) // ')')
        do i = rb, re, 1
            write(*, f) ( mat(i) )
        enddo

    end subroutine pprint_1d_int

    subroutine head_real(mat, num)
        real(rk), intent(in) :: mat(:, :)
        integer(ik), optional :: num
        integer(ik) :: n, res(2), i, j
        character(len=30) :: f, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        n = 10_ik
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'g13.5)')
        do i = 1, n, 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine head_real

    subroutine head_int(mat, num)
        integer(ik), intent(in) :: mat(:, :)
        integer(ik), optional :: num
        integer(ik) :: n, res(2), i, j
        character(len=30) :: f, s, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        write(unit=s, fmt='(I0)') maxval(mat)
        write(unit=s, fmt='(I0)') len_trim(s) + 1
        n = 10_ik
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'i' // trim(s) // ')')
        do i = 1, n, 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine head_int

    subroutine tail_real(mat, num)
        real(rk), intent(in) :: mat(:, :)
        integer(ik), optional :: num
        integer(ik) :: n, res(2), i, j
        character(len=30) :: f, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        n = 10_ik
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'g13.5)')
        do i = res(1) - n + 1, res(1), 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine tail_real

    subroutine tail_int(mat, num)
        integer(ik), intent(in) :: mat(:, :)
        integer(ik), optional :: num
        integer(ik) :: n, res(2), i, j
        character(len=30) :: f, s, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        write(unit=s, fmt='(I0)') maxval(mat)
        write(unit=s, fmt='(I0)') len_trim(s) + 1
        n = 10_ik
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'i' // trim(s) // ')')
        do i = res(1) - n + 1, res(1), 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine tail_int

end module io
