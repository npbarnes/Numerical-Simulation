!!!!
! Given the equation
! d/dt x(t) = F(x,t)
! simulate as
! x_n+1 = x_n + dt*F_n
!!!!
program euler
    use MathTools
    use Electromagnetism
    implicit none
    ! E and B in normalized units, i.e. qE/m -> E, and qB/m -> B
    type(vector), parameter :: E=vector(0,0,0), B=vector(0,0,-1)
    type(vector) :: ionPosition
    type(vector) :: ionVelocity
    real :: time

    integer :: un
    integer :: i

    ! Initialize to the right of the origin moving up. (2-d)
    ionPosition = vector(1,0,0)
    ionVelocity = vector(0,1,0)

    open(newunit=un,file="velocities")

    do i=0,500
        write(un,*) time, ionVelocity%x, ionVelocity%y, ionVelocity%z
        call getNext(accel, .1, ionVelocity, time)
    end do

contains
    ! Get the next position using Euler method.
    pure subroutine getNext(derivitive, step, currx, currt)
        interface
           pure function derivitive(x, t)
                use MathTools
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
    end subroutine getNext

    type(vector) pure function accel(x, t)
        type(vector), intent(in) :: x
        real, intent(in) :: t

        accel = normalizedLorentz(E,B,x)
    end function accel

end program euler
