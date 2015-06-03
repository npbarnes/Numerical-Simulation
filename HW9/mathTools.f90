module MathTools
    implicit none
    public

    type vector
        real :: x,y,z
    end type vector

    interface operator (+)
        module procedure vecadd
    end interface

    ! scalar multiplication
    interface operator (*)
        module procedure vecscale
    end interface

    interface operator (.dot.)
        module procedure vecdot
    end interface

    interface operator (.cross.)
        module procedure veccross
    end interface

contains
    type(vector) elemental function vecadd(a,b)
        type(vector), intent(in) :: a,b
        vecadd = vector(a%x + b%x, a%y + b%y, a%z + b%z)
    end function vecadd

    real elemental function vecdot(a,b)
        type(vector), intent(in) :: a,b
        vecdot = a%x*b%x + a%y*b%y + a%z*b%z
    end function vecdot

    type(vector) elemental function veccross(a,b)
        type(vector), intent(in) :: a,b
        veccross = vector(  a%y*b%z - a%z*b%y,&
                            a%z*b%x - a%x*b%z,&
                            a%x*b%y - a%y*b%x)
    end function veccross

    type(vector) elemental function vecscale(s,v)
        real, intent(in) :: s
        type(vector), intent(in) :: v
        vecscale = vector(s*v%x, s*v%y, s*v%z)
    end function vecscale

end module MathTools
