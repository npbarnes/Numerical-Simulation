module Math
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

end module Math

module Electromagnetism
    use Math
    implicit none
    public :: lorentzForce

contains
    type(vector) elemental function lorentzForce(E,B,q,velocity) result(f)
        type(vector), intent(in) :: E, B
        real, intent(in) :: q
        type(vector), intent(in) :: velocity

        f = q*(E + (velocity .cross. B))
    end function lorentzForce

    ! E and B in normalized units, i.e. qE/m -> E, and qB/m -> B
    ! This will have units of acceleration, not force.
    type(vector) elemental function normalizedLorentz(E,B,velocity) result(f)
        type(vector), intent(in) :: E, B
        type(vector), intent(in) :: velocity

        f = (E + (velocity .cross. B))
    end function normalizedLorentz

end module Electromagnetism

program euler
    use Electromagnetism
    implicit none

end program euler
