program rec_bisec
    implicit none

    interface
        function bisec_r(x_l, x_u, tol)
            real(8) :: bisec_r
            real(8), intent(in) :: x_l, x_u, tol
        end function bisec_r
    end interface

    integer, parameter :: r8=SELECTED_REAL_KIND(15)
    real(r8) :: x_r, a, b, tol=1.0e-12
    real(r8) :: func

    a=0.0
    b=30.0

    x_r=bisec_r(a, b, tol)

    print *, "raiz=", x_r
    print *, "f(x)=", func(x_r)

end program rec_bisec

recursive function bisec_r(x_l, x_u, tol) result(root)
    implicit none
    !Declaro argumentos
    real(8), intent(in) :: x_l, x_u, tol
    real(8) :: root

    !Declaro variables internas
    real(8) :: x_m, func

    x_m = (x_l+x_u)/2._8
    print *, x_m, func(x_m)

    if ((abs(x_u-x_m)/2._8 < tol).AND.(func(x_m)<tol)) then
        root=x_m
    elseif (sign(1._8,func(x_l))== sign(1._8,func(x_m))) then
        root=bisec_r(x_m, x_u, tol)
    else
        root=bisec_r(x_l, x_m, tol)
    endif
    
end function bisec_r

real(8) function func(x)
    implicit none
    real(8), intent(in) :: x 

    func=x**2-4._8
end function func