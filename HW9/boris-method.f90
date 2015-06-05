!!!!
! Given the equation
! d/dt x(t) = F(x,t)
! simulate as
! x_n+1 = x_n + dt*F_n
!!!!
program boris
    use MathTools
    use Electromagnetism
    implicit none
    ! E and B in normalized units, i.e. qE/m -> E, and qB/m -> B
    type(vector), parameter :: E=vector(0,0,0), B=vector(0,0,-1)
    type(vector) :: ionPosition
    type(vector) :: ionVelocity
    real :: time = 0
    real :: step = 0.1

    integer :: velocity_un
    integer :: position_un
    integer :: i
    integer, parameter :: iterations = 50000

    type(vector), dimension(iterations) :: positions
    type(vector), dimension(iterations) :: velocities


    ! Initialize to the right of the origin moving up. (2-d)
    ionPosition = vector(1,0,0)
    ionVelocity = vector(0,1,0)

    open(newunit=position_un,file="positions")
    open(newunit=velocity_un,file="velocities")

    do i=1,iterations
        positions(i) = ionPosition
        velocities(i) = ionVelocity

        write(position_un,*) time, ionPosition%x, ionPosition%y, ionPosition%z
        write(velocity_un,*) time, ionVelocity%x, ionVelocity%y, ionVelocity%z

        call getNext(ionVelocity, E, B, step)
        ionPosition = ionPosition + step*ionVelocity
        time = time+step
    end do

contains
    ! Get the next position using Boris method.
    pure subroutine getNext(velocity, E, B, step)
        use mathTools
        type(vector), intent(inout) :: velocity
        type(vector), intent(in) :: E, B
        real, intent(in) :: step

        type(vector) :: vMinus, vPlus

        vMinus = velocity + (step/2)*E

        vPlus = (&
            (1 - ((B.dot.B)*step**2)/4) * vMinus &
            + step*(vMinus .cross. B) + (((step**2)/2)*(vMinus.dot.B))*B)&
            *(1/(1 + ((B.dot.B)*step**2)/4))

        velocity = vPlus + (step/2)*E
    end subroutine getNext
end program boris
