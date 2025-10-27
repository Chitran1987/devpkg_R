program check
    use vec_utils
    implicit none
    real(real64) :: S(10), D(9), MP(9)
    real(real64) :: avg, vr, sd
    !real(real64) :: a, b
    !integer :: n
    call seqn(st=1.0_real64, en=10.0_real64, len=10, X=S)
    call diff(X=S, Y = D)
    call mean(X=S, y=avg)
    call var(X = S, res = vr)
    call sdev(X = S, res = sd)
    call mdpnt_vec(X = S, res = MP)
    print *, "The sequence S is", S
    print *,"The differential sequence D is ", D
    print *,"The average avg is ", avg
    print *, "The variance vr of S is ", vr
    print *, "The standard deviation sd of S is ", sd
    print *, "The midpoint vector MP of S is ", MP
end program check
