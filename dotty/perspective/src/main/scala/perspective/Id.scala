package perspective

type Id[A]    = A
type IdF      = [F[_], A] =>> F[A]
type IdFC[A]  = [F[_], _] =>> F[A]