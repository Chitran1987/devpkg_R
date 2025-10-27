module devpkg_R
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, devpkg_R!"
  end subroutine say_hello
end module devpkg_R
