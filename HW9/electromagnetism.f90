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

end module Electromagnetism
