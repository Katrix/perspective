package perspective

/** An identity type constructor. */
type Id[A] = A

type IdFC[A] = [F0[_]] =>> F0[A]
