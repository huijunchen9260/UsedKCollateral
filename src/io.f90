module io

    use iso_Fortran_env, only: real64, real32, int64, int32
    implicit none
    private
    public checkdir, pprint, head, tail, num2str

    interface operator(.f.)
        module procedure file_exists
    end interface

    interface operator(.d.)
        module procedure directory_exists
    end interface

    ! print the whole matrix
    interface pprint
        module procedure pprint_1d_int32
        module procedure pprint_1d_int64
        module procedure pprint_1d_real64
        module procedure pprint_2d_int32
        module procedure pprint_2d_int64
        module procedure pprint_2d_real64
        module procedure pprint_2d_logical
        module procedure pprint_3d_int32
        module procedure pprint_3d_int64
        module procedure pprint_3d_real64
        module procedure pprint_3d_logical
    end interface

    ! print the head of matrix, default 10 rows
    interface head
        module procedure head_int32
        module procedure head_real64
    end interface head

    ! print the tail of matrix, default 10 rows
    interface tail
        module procedure tail_int32
        module procedure tail_real64
    end interface tail

    ! convert int32, int64, real32, real64 into string
    interface num2str
        module procedure num2str_int32
        module procedure num2str_int64
        module procedure num2str_real32
        module procedure num2str_real64
    end interface num2str

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

    subroutine checkdir(dir)
            implicit none
            character(len=*), intent(in) :: dir
            integer :: unitno

            ! Test whether the directory exists
            open(newunit=unitno, file=trim(dir)//'deleteme.txt', status='replace', err=1234)
            close (unitno, status = 'delete')
            return

            ! If doesn't exist, end gracefully
    1234    write(*,*) 'Data directory, '//trim(dir)//' does not exist or could not write there!'
            STOP
    end subroutine

    subroutine pprint_3d_logical(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        logical, intent(in) :: mat(:, :, :)
        integer(int32), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(int32) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
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

    subroutine pprint_3d_real64(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        real(real64), intent(in) :: mat(:, :, :)
        integer(int32), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(int32) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
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
            call pprint_2d_real64(mat(:, :, i), one_b, one_e, two_b, two_e)
            write(*, *) ''
        enddo
    end subroutine pprint_3d_real64

    subroutine pprint_3d_int32(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        integer(int32), intent(in) :: mat(:, :, :)
        integer(int32), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(int32) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
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
            call pprint_2d_int32(mat(:, :, i), one_b, one_e, two_b, two_e)
            write(*, *) ''
        enddo
    end subroutine pprint_3d_int32

    subroutine pprint_3d_int64(mat, one_begin, one_end, two_begin, two_end, three_begin, three_end)
        integer(int64), intent(in) :: mat(:, :, :)
        integer(int64), optional :: one_begin, one_end, two_begin, two_end, three_begin, three_end
        integer(int64) :: res(3), i, one_b, one_e, two_b, two_e, three_b, three_e
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
            call pprint_2d_int64(mat(:, :, i), one_b, one_e, two_b, two_e)
            write(*, *) ''
        enddo
    end subroutine pprint_3d_int64

    subroutine pprint_2d_logical(mat, rowbegin, rowend, colbegin, colend)
        logical, intent(in) :: mat(:, :)
        integer(int32), optional :: rowbegin, rowend, colbegin, colend
        integer(int32) :: res(2), i, j, rb, re, cb, ce
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

    subroutine pprint_2d_real64(mat, rowbegin, rowend, colbegin, colend)
        real(real64), intent(in) :: mat(:, :)
        integer(int32), optional :: rowbegin, rowend, colbegin, colend
        integer(int32) :: res(2), i, j, rb, re, cb, ce
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
    end subroutine pprint_2d_real64

    subroutine pprint_2d_int32(mat, rowbegin, rowend, colbegin, colend)
        integer(int32), intent(in) :: mat(:, :)
        integer(int32), optional :: rowbegin, rowend, colbegin, colend
        integer(int32) :: res(2), i, j, rb, re, cb, ce
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
    end subroutine pprint_2d_int32

    subroutine pprint_2d_int64(mat, rowbegin, rowend, colbegin, colend)
        integer(int64), intent(in) :: mat(:, :)
        integer(int64), optional :: rowbegin, rowend, colbegin, colend
        integer(int64) :: res(2), i, j, rb, re, cb, ce
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
    end subroutine pprint_2d_int64

    subroutine pprint_1d_real64(mat, rowbegin, rowend)
        real(real64), intent(in) :: mat(:)
        integer(int32), optional :: rowbegin, rowend
        integer(int32) :: res, i, rb, re
        character(len=30) :: f, s, str

        res = size(mat)
        rb = 1; if (present(rowbegin)) rb = rowbegin
        re = res; if (present(rowend)) re = rowend

        write(unit=str, fmt='(I0)') res
        f = trim('(' // trim(str) // 'E13.5)')
        do i = rb, re, 1
            write(*, f) ( mat(i) )
        enddo

    end subroutine pprint_1d_real64

    subroutine pprint_1d_int32(mat, rowbegin, rowend)
        integer(int32), intent(in) :: mat(:)
        integer(int32), optional :: rowbegin, rowend
        integer(int32) :: res, i, rb, re
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

    end subroutine pprint_1d_int32

    subroutine pprint_1d_int64(mat, rowbegin, rowend)
        integer(int64), intent(in) :: mat(:)
        integer(int64), optional :: rowbegin, rowend
        integer(int64) :: res, i, rb, re
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

    end subroutine pprint_1d_int64

    subroutine head_real64(mat, num)
        real(real64), intent(in) :: mat(:, :)
        integer(int32), optional :: num
        integer(int32) :: n, res(2), i, j
        character(len=30) :: f, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        n = 10_int32
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'g13.5)')
        do i = 1, n, 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine head_real64

    subroutine head_int32(mat, num)
        integer(int32), intent(in) :: mat(:, :)
        integer(int32), optional :: num
        integer(int32) :: n, res(2), i, j
        character(len=30) :: f, s, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        write(unit=s, fmt='(I0)') maxval(mat)
        write(unit=s, fmt='(I0)') len_trim(s) + 1
        n = 10_int32
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'i' // trim(s) // ')')
        do i = 1, n, 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine head_int32

    subroutine tail_real64(mat, num)
        real(real64), intent(in) :: mat(:, :)
        integer(int32), optional :: num
        integer(int32) :: n, res(2), i, j
        character(len=30) :: f, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        n = 10_int32
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'g13.5)')
        do i = res(1) - n + 1, res(1), 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine tail_real64

    subroutine tail_int32(mat, num)
        integer(int32), intent(in) :: mat(:, :)
        integer(int32), optional :: num
        integer(int32) :: n, res(2), i, j
        character(len=30) :: f, s, str(2)
        res = shape(mat)
        write(unit=str, fmt='(I0)') res
        write(unit=s, fmt='(I0)') maxval(mat)
        write(unit=s, fmt='(I0)') len_trim(s) + 1
        n = 10_int32
        if (present(num)) n = num
        f = trim('(' // trim(str(2)) // 'i' // trim(s) // ')')
        do i = res(1) - n + 1, res(1), 1
            write(*, f) ( mat(i,j), j=1, res(2), 1 )
        enddo
    end subroutine tail_int32

    function num2str_int32(num)
        integer(int32), intent(in)      :: num
        character(len=:), allocatable   :: num2str_int32
        character(len=range(num))       :: str

        write(unit=str, fmt='(I0)') num
        num2str_int32 = trim(str)
    end function num2str_int32

    function num2str_int64(num)
        integer(int64), intent(in)      :: num
        character(len=:), allocatable   :: num2str_int64
        character(len=range(num))       :: str

        write(unit=str, fmt='(I0)') num
        num2str_int64 = trim(str)
    end function num2str_int64

    function num2str_real32(num, strfmt)
        real(real32), intent(in)        :: num
        character(len=*), optional      :: strfmt
        character(len=:), allocatable   :: num2str_real32
        character(len=range(num))       :: str

        if (present(strfmt)) then
            write(unit=str, fmt= '('//trim(strfmt)//')' ) num
        else
            write(unit=str, fmt='(G0)') num
        end if

        num2str_real32 = trim(str)
    end function num2str_real32

    function num2str_real64(num, strfmt)
        real(real64), intent(in)        :: num
        character(len=*), optional      :: strfmt
        character(len=:), allocatable   :: num2str_real64
        character(len=range(num))       :: str

        if (present(strfmt)) then
            write(unit=str, fmt= '('//trim(strfmt)//')' ) num
        else
            write(unit=str, fmt='(G0)') num
        end if

        num2str_real64 = trim(str)
    end function num2str_real64

end module io
