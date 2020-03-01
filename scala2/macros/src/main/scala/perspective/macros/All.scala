package perspective.macros

import perspective.{ApplicativeK, DistributiveK, TraverseK}

private[macros] trait All[F[_[_], _]] extends ApplicativeK[F] with TraverseK[F] with DistributiveK[F]
