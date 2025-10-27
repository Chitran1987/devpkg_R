program check
    use vec_utils
    implicit none
    real(real64) :: X(10)
    real(real64), allocatable :: Y(:,:)
    call seqn(1.0_real64, 10.0_real64, 10, X)
    Y = reshape(X, shape = [2,5])
    call lineval(Y, 2, 5, 0.0_real64, 1.0_real64)
    call print_mat(Y)
end program check
