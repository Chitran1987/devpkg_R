program lin_reg_test
    use iso_fortran_env
    use vec_utils
    implicit none
    real(real64), allocatable :: M(:,:)
    real(real64), allocatable :: C(:)
    M = reshape(source = [0, 1, 2, 0, 2, 4], shape=[3,2])
    call lin_reg(dat = M, Coeff = C)
    print *, "The data matrix"
    call print_mat(M)
    print *, "The coefficient vector"
    print *, C
end program lin_reg_test
