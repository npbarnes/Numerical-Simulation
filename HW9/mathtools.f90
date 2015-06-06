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
        module procedure vecscalel
        module procedure vecscaler
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

    type(vector) elemental function vecscalel(s,v)
        real, intent(in) :: s
        type(vector), intent(in) :: v
        vecscalel = vector(s*v%x, s*v%y, s*v%z)
    end function vecscalel

    type(vector) elemental function vecscaler(vv,ss)
        real, intent(in) :: ss
        type(vector), intent(in) :: vv
        vecscaler = vector(ss*vv%x, ss*vv%y, ss*vv%z)
    end function vecscaler

    ! Get the next position using Euler method.
    pure subroutine eulerStep(derivitive, step, currx, currt)
        interface
            pure function derivitive(x, t)
                import :: vector
                type(vector) :: derivitive
                type(vector), intent(in) :: x
                real, intent(in) :: t
            end function derivitive
        end interface
        real, intent(in) :: step
        type(vector), intent(inout) :: currx
        real, intent(inout) :: currt

        currx = currx + step*derivitive(currx,currt)
        currt = currt+step
    end subroutine eulerStep
end module MathTools
