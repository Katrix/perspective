package perspective.macros

import perspective.{RepresentableK, TraverseK}

private[macros] trait All[F[_[_], _]] extends TraverseK[F] with RepresentableK[F]
