module Electromagnetism
    use MathTools
    implicit none

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

    ! Get the next velocity using Boris method.
    pure subroutine borisStep(velocity, E, B, step)
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
    end subroutine borisStep
end module Electromagnetism
