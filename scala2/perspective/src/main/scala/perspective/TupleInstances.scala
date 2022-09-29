package perspective

import cats.{Applicative, Functor}

object TupleInstances {

  implicit def tuple1PerspectiveAllInstances[T1]
      : ApplicativeKC[Tuple1F[T1]#λ] with TraverseKC[Tuple1F[T1]#λ] with DistributiveKC[Tuple1F[T1]#λ] =
    new ApplicativeKC[Tuple1F[T1]#λ] with TraverseKC[Tuple1F[T1]#λ] with DistributiveKC[Tuple1F[T1]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple1[A[T1]] = Tuple1(a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple1[A[T1]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple1[B[T1]]] =
        G.map(f(fa._1))(Tuple1.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple1[A[T1]],
          fb: Tuple1[B[T1]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple1[Z[T1]] =
        Tuple1(f(fa._1, fb._1))

      override def foldLeftK[A[_], B, C](fa: Tuple1[A[T1]], b: B)(f: B => A ~>#: B): B =
        f(b)(fa._1)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple1[A[T1]]]
      )(implicit G: Functor[G]): Tuple1[Compose2[G, A, T1]] =
        Tuple1(G.map(gfa)(_._1))
    }

  implicit def tuple2PerspectiveAllInstances[T1, T2]
      : ApplicativeKC[Tuple2F[T1, T2]#λ] with TraverseKC[Tuple2F[T1, T2]#λ] with DistributiveKC[Tuple2F[T1, T2]#λ] =
    new ApplicativeKC[Tuple2F[T1, T2]#λ] with TraverseKC[Tuple2F[T1, T2]#λ] with DistributiveKC[Tuple2F[T1, T2]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple2[A[T1], A[T2]] = Tuple2(a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple2[A[T1], A[T2]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple2[B[T1], B[T2]]] =
        G.map2(f(fa._1), f(fa._2))(Tuple2.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple2[A[T1], A[T2]],
          fb: Tuple2[B[T1], B[T2]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple2[Z[T1], Z[T2]] =
        Tuple2(f(fa._1, fb._1), f(fa._2, fb._2))

      override def foldLeftK[A[_], B, C](fa: Tuple2[A[T1], A[T2]], b: B)(f: B => A ~>#: B): B =
        f(f(b)(fa._1))(fa._2)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple2[A[T1], A[T2]]]
      )(implicit G: Functor[G]): Tuple2[Compose2[G, A, T1], Compose2[G, A, T2]] =
        Tuple2(G.map(gfa)(_._1), G.map(gfa)(_._2))
    }

  implicit def tuple3PerspectiveAllInstances[T1, T2, T3]: ApplicativeKC[Tuple3F[T1, T2, T3]#λ]
    with TraverseKC[Tuple3F[T1, T2, T3]#λ]
    with DistributiveKC[Tuple3F[T1, T2, T3]#λ] =
    new ApplicativeKC[Tuple3F[T1, T2, T3]#λ]
      with TraverseKC[Tuple3F[T1, T2, T3]#λ]
      with DistributiveKC[Tuple3F[T1, T2, T3]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple3[A[T1], A[T2], A[T3]] = Tuple3(a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple3[A[T1], A[T2], A[T3]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple3[B[T1], B[T2], B[T3]]] =
        G.map3(f(fa._1), f(fa._2), f(fa._3))(Tuple3.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple3[A[T1], A[T2], A[T3]],
          fb: Tuple3[B[T1], B[T2], B[T3]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple3[Z[T1], Z[T2], Z[T3]] =
        Tuple3(f(fa._1, fb._1), f(fa._2, fb._2), f(fa._3, fb._3))

      override def foldLeftK[A[_], B, C](fa: Tuple3[A[T1], A[T2], A[T3]], b: B)(f: B => A ~>#: B): B =
        f(f(f(b)(fa._1))(fa._2))(fa._3)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple3[A[T1], A[T2], A[T3]]]
      )(implicit G: Functor[G]): Tuple3[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3]] =
        Tuple3(G.map(gfa)(_._1), G.map(gfa)(_._2), G.map(gfa)(_._3))
    }

  implicit def tuple4PerspectiveAllInstances[T1, T2, T3, T4]: ApplicativeKC[Tuple4F[T1, T2, T3, T4]#λ]
    with TraverseKC[Tuple4F[T1, T2, T3, T4]#λ]
    with DistributiveKC[Tuple4F[T1, T2, T3, T4]#λ] =
    new ApplicativeKC[Tuple4F[T1, T2, T3, T4]#λ]
      with TraverseKC[Tuple4F[T1, T2, T3, T4]#λ]
      with DistributiveKC[Tuple4F[T1, T2, T3, T4]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple4[A[T1], A[T2], A[T3], A[T4]] =
        Tuple4(a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple4[A[T1], A[T2], A[T3], A[T4]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple4[B[T1], B[T2], B[T3], B[T4]]] =
        G.map4(f(fa._1), f(fa._2), f(fa._3), f(fa._4))(Tuple4.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple4[A[T1], A[T2], A[T3], A[T4]],
          fb: Tuple4[B[T1], B[T2], B[T3], B[T4]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple4[Z[T1], Z[T2], Z[T3], Z[T4]] =
        Tuple4(f(fa._1, fb._1), f(fa._2, fb._2), f(fa._3, fb._3), f(fa._4, fb._4))

      override def foldLeftK[A[_], B, C](fa: Tuple4[A[T1], A[T2], A[T3], A[T4]], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple4[A[T1], A[T2], A[T3], A[T4]]]
      )(
          implicit G: Functor[G]
      ): Tuple4[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4]] =
        Tuple4(G.map(gfa)(_._1), G.map(gfa)(_._2), G.map(gfa)(_._3), G.map(gfa)(_._4))
    }

  implicit def tuple5PerspectiveAllInstances[T1, T2, T3, T4, T5]: ApplicativeKC[Tuple5F[T1, T2, T3, T4, T5]#λ]
    with TraverseKC[Tuple5F[T1, T2, T3, T4, T5]#λ]
    with DistributiveKC[Tuple5F[T1, T2, T3, T4, T5]#λ] =
    new ApplicativeKC[Tuple5F[T1, T2, T3, T4, T5]#λ]
      with TraverseKC[Tuple5F[T1, T2, T3, T4, T5]#λ]
      with DistributiveKC[Tuple5F[T1, T2, T3, T4, T5]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple5[A[T1], A[T2], A[T3], A[T4], A[T5]] =
        Tuple5(a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple5[A[T1], A[T2], A[T3], A[T4], A[T5]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple5[B[T1], B[T2], B[T3], B[T4], B[T5]]] =
        G.map5(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5))(Tuple5.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple5[A[T1], A[T2], A[T3], A[T4], A[T5]],
          fb: Tuple5[B[T1], B[T2], B[T3], B[T4], B[T5]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple5[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5]] =
        Tuple5(f(fa._1, fb._1), f(fa._2, fb._2), f(fa._3, fb._3), f(fa._4, fb._4), f(fa._5, fb._5))

      override def foldLeftK[A[_], B, C](fa: Tuple5[A[T1], A[T2], A[T3], A[T4], A[T5]], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple5[A[T1], A[T2], A[T3], A[T4], A[T5]]]
      )(
          implicit G: Functor[G]
      ): Tuple5[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[G, A, T5]] =
        Tuple5(G.map(gfa)(_._1), G.map(gfa)(_._2), G.map(gfa)(_._3), G.map(gfa)(_._4), G.map(gfa)(_._5))
    }

  implicit def tuple6PerspectiveAllInstances[T1, T2, T3, T4, T5, T6]: ApplicativeKC[Tuple6F[T1, T2, T3, T4, T5, T6]#λ]
    with TraverseKC[Tuple6F[T1, T2, T3, T4, T5, T6]#λ]
    with DistributiveKC[Tuple6F[T1, T2, T3, T4, T5, T6]#λ] =
    new ApplicativeKC[Tuple6F[T1, T2, T3, T4, T5, T6]#λ]
      with TraverseKC[Tuple6F[T1, T2, T3, T4, T5, T6]#λ]
      with DistributiveKC[Tuple6F[T1, T2, T3, T4, T5, T6]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple6[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6]] =
        Tuple6(a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple6[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple6[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6]]] =
        G.map6(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6))(Tuple6.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple6[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6]],
          fb: Tuple6[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple6[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6]] =
        Tuple6(f(fa._1, fb._1), f(fa._2, fb._2), f(fa._3, fb._3), f(fa._4, fb._4), f(fa._5, fb._5), f(fa._6, fb._6))

      override def foldLeftK[A[_], B, C](fa: Tuple6[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6]], b: B)(
          f: B => A ~>#: B
      ): B =
        f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple6[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6]]]
      )(
          implicit G: Functor[G]
      ): Tuple6[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ]] =
        Tuple6(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6)
        )
    }

  implicit def tuple7PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7]
      : ApplicativeKC[Tuple7F[T1, T2, T3, T4, T5, T6, T7]#λ]
        with TraverseKC[Tuple7F[T1, T2, T3, T4, T5, T6, T7]#λ]
        with DistributiveKC[Tuple7F[T1, T2, T3, T4, T5, T6, T7]#λ] =
    new ApplicativeKC[Tuple7F[T1, T2, T3, T4, T5, T6, T7]#λ]
      with TraverseKC[Tuple7F[T1, T2, T3, T4, T5, T6, T7]#λ]
      with DistributiveKC[Tuple7F[T1, T2, T3, T4, T5, T6, T7]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple7[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7]] =
        Tuple7(a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple7[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple7[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7]]] =
        G.map7(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7))(Tuple7.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple7[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7]],
          fb: Tuple7[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple7[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7]] =
        Tuple7(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7)
        )

      override def foldLeftK[A[_], B, C](fa: Tuple7[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7]], b: B)(
          f: B => A ~>#: B
      ): B =
        f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple7[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7]]]
      )(
          implicit G: Functor[G]
      ): Tuple7[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7]] =
        Tuple7(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7)
        )
    }

  implicit def tuple8PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8]
      : ApplicativeKC[Tuple8F[T1, T2, T3, T4, T5, T6, T7, T8]#λ]
        with TraverseKC[Tuple8F[T1, T2, T3, T4, T5, T6, T7, T8]#λ]
        with DistributiveKC[Tuple8F[T1, T2, T3, T4, T5, T6, T7, T8]#λ] =
    new ApplicativeKC[Tuple8F[T1, T2, T3, T4, T5, T6, T7, T8]#λ]
      with TraverseKC[Tuple8F[T1, T2, T3, T4, T5, T6, T7, T8]#λ]
      with DistributiveKC[Tuple8F[T1, T2, T3, T4, T5, T6, T7, T8]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple8[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8]] =
        Tuple8(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](fa: Tuple8[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8]])(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple8[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8]]] =
        G.map8(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8))(Tuple8.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple8[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8]],
          fb: Tuple8[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple8[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8]] =
        Tuple8(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8)
        )

      override def foldLeftK[A[_], B, C](fa: Tuple8[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8]], b: B)(
          f: B => A ~>#: B
      ): B =
        f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple8[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8]]]
      )(
          implicit G: Functor[G]
      ): Tuple8[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8]] =
        Tuple8(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8)
        )
    }

  implicit def tuple9PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9]
      : ApplicativeKC[Tuple9F[T1, T2, T3, T4, T5, T6, T7, T8, T9]#λ]
        with TraverseKC[Tuple9F[T1, T2, T3, T4, T5, T6, T7, T8, T9]#λ]
        with DistributiveKC[Tuple9F[T1, T2, T3, T4, T5, T6, T7, T8, T9]#λ] =
    new ApplicativeKC[Tuple9F[T1, T2, T3, T4, T5, T6, T7, T8, T9]#λ]
      with TraverseKC[Tuple9F[T1, T2, T3, T4, T5, T6, T7, T8, T9]#λ]
      with DistributiveKC[Tuple9F[T1, T2, T3, T4, T5, T6, T7, T8, T9]#λ] {
      override def pureK[A[_], C](
          a: Unit #~>: A
      ): Tuple9[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9]] =
        Tuple9(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple9[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple9[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9]]] =
        G.map9(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9))(Tuple9.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple9[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9]],
          fb: Tuple9[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple9[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9]] =
        Tuple9(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple9[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9]],
          b: B
      )(f: B => A ~>#: B): B =
        f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple9[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9]]]
      )(
          implicit G: Functor[G]
      ): Tuple9[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9]] =
        Tuple9(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9)
        )
    }

  implicit def tuple10PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
      : ApplicativeKC[Tuple10F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]#λ]
        with TraverseKC[Tuple10F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]#λ]
        with DistributiveKC[Tuple10F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]#λ] =
    new ApplicativeKC[Tuple10F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]#λ]
      with TraverseKC[Tuple10F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]#λ]
      with DistributiveKC[Tuple10F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]#λ] {
      override def pureK[A[_], C](
          a: Unit #~>: A
      ): Tuple10[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10]] =
        Tuple10(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple10[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple10[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10]]] =
        G.map10(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10))(
          Tuple10.apply
        )

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple10[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10]],
          fb: Tuple10[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple10[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10]] =
        Tuple10(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple10[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10]],
          b: B
      )(f: B => A ~>#: B): B =
        f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple10[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10]]]
      )(
          implicit G: Functor[G]
      ): Tuple10[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10]] =
        Tuple10(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10)
        )
    }

  implicit def tuple11PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
      : ApplicativeKC[Tuple11F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]#λ]
        with TraverseKC[Tuple11F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]#λ]
        with DistributiveKC[Tuple11F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]#λ] =
    new ApplicativeKC[Tuple11F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]#λ]
      with TraverseKC[Tuple11F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]#λ]
      with DistributiveKC[Tuple11F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]#λ] {
      override def pureK[A[_], C](
          a: Unit #~>: A
      ): Tuple11[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11]] =
        Tuple11(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple11[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(
          implicit G: Applicative[G]
      ): G[Tuple11[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11]]] =
        G.map11(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11)
        )(Tuple11.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple11[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11]],
          fb: Tuple11[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11]]
      )(
          f: Tuple2K[A, B, *] ~>: Z
      ): Tuple11[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[T11]] =
        Tuple11(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple11[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11]],
          b: B
      )(f: B => A ~>#: B): B =
        f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(
          fa._11
        )

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple11[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11]]]
      )(
          implicit G: Functor[G]
      ): Tuple11[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11]] =
        Tuple11(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11)
        )
    }

  implicit def tuple12PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
      : ApplicativeKC[Tuple12F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]#λ]
        with TraverseKC[Tuple12F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]#λ]
        with DistributiveKC[Tuple12F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]#λ] =
    new ApplicativeKC[Tuple12F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]#λ]
      with TraverseKC[Tuple12F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]#λ]
      with DistributiveKC[Tuple12F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]#λ] {
      override def pureK[A[_], C](
          a: Unit #~>: A
      ): Tuple12[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12]] =
        Tuple12(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple12[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(
          implicit G: Applicative[G]
      ): G[Tuple12[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12]]] =
        G.map12(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12)
        )(Tuple12.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple12[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12]],
          fb: Tuple12[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12]]
      )(
          f: Tuple2K[A, B, *] ~>: Z
      ): Tuple12[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[T11], Z[T12]] =
        Tuple12(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple12[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(
            fa._11
          )
        )(fa._12)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple12[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12]]]
      )(
          implicit G: Functor[G]
      ): Tuple12[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ]] =
        Tuple12(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12)
        )
    }

  implicit def tuple13PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
      : ApplicativeKC[Tuple13F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]#λ]
        with TraverseKC[Tuple13F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]#λ]
        with DistributiveKC[Tuple13F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]#λ] =
    new ApplicativeKC[Tuple13F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]#λ]
      with TraverseKC[Tuple13F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]#λ]
      with DistributiveKC[Tuple13F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]#λ] {
      override def pureK[A[_], C](
          a: Unit #~>: A
      ): Tuple13[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13]] =
        Tuple13(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple13[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(
          implicit G: Applicative[G]
      ): G[Tuple13[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13]]] =
        G.map13(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13)
        )(Tuple13.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple13[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13]],
          fb: Tuple13[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13]]
      )(
          f: Tuple2K[A, B, *] ~>: Z
      ): Tuple13[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[T11], Z[T12], Z[T13]] =
        Tuple13(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple13[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(
              fa._11
            )
          )(fa._12)
        )(fa._13)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[Tuple13[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13]]]
      )(
          implicit G: Functor[G]
      ): Tuple13[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13]] =
        Tuple13(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13)
        )
    }

  implicit def tuple14PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
      : ApplicativeKC[Tuple14F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]#λ]
        with TraverseKC[Tuple14F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]#λ]
        with DistributiveKC[Tuple14F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]#λ] =
    new ApplicativeKC[Tuple14F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]#λ]
      with TraverseKC[Tuple14F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]#λ]
      with DistributiveKC[Tuple14F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple14[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14]] =
        Tuple14(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple14[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[
        Tuple14[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[T14]]
      ] =
        G.map14(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14)
        )(Tuple14.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple14[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ]],
          fb: Tuple14[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple14[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14]] =
        Tuple14(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple14[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(
                fa._11
              )
            )(fa._12)
          )(fa._13)
        )(fa._14)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple14[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple14[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14]] =
        Tuple14(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14)
        )
    }

  implicit def tuple15PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
      : ApplicativeKC[Tuple15F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]#λ]
        with TraverseKC[Tuple15F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]#λ]
        with DistributiveKC[Tuple15F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]#λ] =
    new ApplicativeKC[Tuple15F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]#λ]
      with TraverseKC[Tuple15F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]#λ]
      with DistributiveKC[Tuple15F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple15[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15]] =
        Tuple15(a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()), a(()))

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple15[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple15[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15]]] =
        G.map15(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15)
        )(Tuple15.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple15[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15]],
          fb: Tuple15[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple15[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15]] =
        Tuple15(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple15[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10)
                )(fa._11)
              )(fa._12)
            )(fa._13)
          )(fa._14)
        )(fa._15)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple15[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple15[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15]] =
        Tuple15(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15)
        )
    }

  implicit def tuple16PerspectiveAllInstances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
      : ApplicativeKC[Tuple16F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]#λ]
        with TraverseKC[Tuple16F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]#λ]
        with DistributiveKC[Tuple16F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]#λ] =
    new ApplicativeKC[Tuple16F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]#λ]
      with TraverseKC[Tuple16F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]#λ]
      with DistributiveKC[Tuple16F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple16[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16]] =
        Tuple16(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple16[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple16[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16]]] =
        G.map16(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16)
        )(Tuple16.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple16[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16]],
          fb: Tuple16[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple16[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16]] =
        Tuple16(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple16[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(
                      fa._10
                    )
                  )(fa._11)
                )(fa._12)
              )(fa._13)
            )(fa._14)
          )(fa._15)
        )(fa._16)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple16[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple16[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16]] =
        Tuple16(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16)
        )
    }

  implicit def tuple17PerspectiveAllInstances[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17
  ]: ApplicativeKC[Tuple17F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]#λ]
    with TraverseKC[Tuple17F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]#λ]
    with DistributiveKC[Tuple17F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]#λ] =
    new ApplicativeKC[Tuple17F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]#λ]
      with TraverseKC[Tuple17F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]#λ]
      with DistributiveKC[Tuple17F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple17[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16], A[T17]] =
        Tuple17(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple17[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple17[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16], B[T17]]] =
        G.map17(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16),
          f(fa._17)
        )(Tuple17.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple17[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17]],
          fb: Tuple17[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16], B[T17]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple17[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16], Z[T17]] =
        Tuple17(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16),
          f(fa._17, fb._17)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple17[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(
                        fa._10
                      )
                    )(fa._11)
                  )(fa._12)
                )(fa._13)
              )(fa._14)
            )(fa._15)
          )(fa._16)
        )(fa._17)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple17[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16], A[T17]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple17[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16], Compose2[G, A, T17]] =
        Tuple17(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16),
          G.map(gfa)(_._17)
        )
    }

  implicit def tuple18PerspectiveAllInstances[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18
  ]: ApplicativeKC[Tuple18F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]#λ]
    with TraverseKC[Tuple18F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]#λ]
    with DistributiveKC[Tuple18F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]#λ] =
    new ApplicativeKC[Tuple18F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]#λ]
      with TraverseKC[Tuple18F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]#λ]
      with DistributiveKC[Tuple18F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]#λ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple18[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16], A[T17], A[T18]] =
        Tuple18(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple18[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple18[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16], B[T17], B[T18]]] =
        G.map18(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16),
          f(fa._17),
          f(fa._18)
        )(Tuple18.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple18[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18]],
          fb: Tuple18[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16], B[T17], B[T18]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple18[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16], Z[T17], Z[T18]] =
        Tuple18(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16),
          f(fa._17, fb._17),
          f(fa._18, fb._18)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple18[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(
                          fa._10
                        )
                      )(fa._11)
                    )(fa._12)
                  )(fa._13)
                )(fa._14)
              )(fa._15)
            )(fa._16)
          )(fa._17)
        )(fa._18)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple18[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16], A[T17], A[T18]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple18[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16], Compose2[
        G,
        A,
        T17
      ], Compose2[
        G,
        A,
        T18
      ]] =
        Tuple18(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16),
          G.map(gfa)(_._17),
          G.map(gfa)(_._18)
        )
    }

  implicit def tuple19PerspectiveAllInstances[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19
  ]: ApplicativeKC[Tuple19F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]#λ]
    with TraverseKC[Tuple19F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]#λ]
    with DistributiveKC[
      Tuple19F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]#λ
    ] =
    new ApplicativeKC[Tuple19F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]#λ]
      with TraverseKC[Tuple19F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]#λ]
      with DistributiveKC[
        Tuple19F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]#λ
      ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple19[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16], A[T17], A[T18], A[T19]] =
        Tuple19(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple19[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple19[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16], B[T17], B[T18], B[T19]]] =
        G.map19(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16),
          f(fa._17),
          f(fa._18),
          f(fa._19)
        )(Tuple19.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple19[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19]],
          fb: Tuple19[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16], B[T17], B[T18], B[T19]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple19[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16], Z[T17], Z[T18], Z[T19]] =
        Tuple19(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16),
          f(fa._17, fb._17),
          f(fa._18, fb._18),
          f(fa._19, fb._19)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple19[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9)
                          )(fa._10)
                        )(fa._11)
                      )(fa._12)
                    )(fa._13)
                  )(fa._14)
                )(fa._15)
              )(fa._16)
            )(fa._17)
          )(fa._18)
        )(fa._19)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple19[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16], A[T17], A[T18], A[T19]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple19[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16], Compose2[
        G,
        A,
        T17
      ], Compose2[
        G,
        A,
        T18
      ], Compose2[G, A, T19]] =
        Tuple19(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16),
          G.map(gfa)(_._17),
          G.map(gfa)(_._18),
          G.map(gfa)(_._19)
        )
    }

  implicit def tuple20PerspectiveAllInstances[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20
  ]: ApplicativeKC[
    Tuple20F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]#λ
  ] with TraverseKC[
      Tuple20F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]#λ
    ]
    with DistributiveKC[
      Tuple20F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]#λ
    ] =
    new ApplicativeKC[
      Tuple20F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]#λ
    ] with TraverseKC[
        Tuple20F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]#λ
      ]
      with DistributiveKC[
        Tuple20F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]#λ
      ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple20[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20]] =
        Tuple20(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple20[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple20[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16], B[T17], B[T18], B[T19], B[T20]]] =
        G.map20(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16),
          f(fa._17),
          f(fa._18),
          f(fa._19),
          f(fa._20)
        )(Tuple20.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple20[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20]],
          fb: Tuple20[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16], B[T17], B[T18], B[T19], B[T20]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple20[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16], Z[T17], Z[T18], Z[T19], Z[T20]] =
        Tuple20(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16),
          f(fa._17, fb._17),
          f(fa._18, fb._18),
          f(fa._19, fb._19),
          f(fa._20, fb._20)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple20[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(
                              f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(
                                fa._9
                              )
                            )(fa._10)
                          )(fa._11)
                        )(fa._12)
                      )(fa._13)
                    )(fa._14)
                  )(fa._15)
                )(fa._16)
              )(fa._17)
            )(fa._18)
          )(fa._19)
        )(fa._20)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple20[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple20[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16], Compose2[
        G,
        A,
        T17
      ], Compose2[
        G,
        A,
        T18
      ], Compose2[G, A, T19], Compose2[G, A, T20]] =
        Tuple20(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16),
          G.map(gfa)(_._17),
          G.map(gfa)(_._18),
          G.map(gfa)(_._19),
          G.map(gfa)(_._20)
        )
    }

  implicit def tuple21PerspectiveAllInstances[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21
  ]: ApplicativeKC[
    Tuple21F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]#λ
  ] with TraverseKC[
      Tuple21F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]#λ
    ]
    with DistributiveKC[
      Tuple21F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]#λ
    ] =
    new ApplicativeKC[
      Tuple21F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]#λ
    ] with TraverseKC[
        Tuple21F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]#λ
      ]
      with DistributiveKC[
        Tuple21F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]#λ
      ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple21[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21]] =
        Tuple21(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple21[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple21[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16], B[T17], B[T18], B[T19], B[T20], B[T21]]] =
        G.map21(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16),
          f(fa._17),
          f(fa._18),
          f(fa._19),
          f(fa._20),
          f(fa._21)
        )(Tuple21.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple21[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21]],
          fb: Tuple21[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16], B[T17], B[T18], B[T19], B[T20], B[T21]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple21[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16], Z[T17], Z[T18], Z[T19], Z[T20], Z[T21]] =
        Tuple21(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16),
          f(fa._17, fb._17),
          f(fa._18, fb._18),
          f(fa._19, fb._19),
          f(fa._20, fb._20),
          f(fa._21, fb._21)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple21[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(
                              f(
                                f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(
                                  fa._9
                                )
                              )(fa._10)
                            )(fa._11)
                          )(fa._12)
                        )(fa._13)
                      )(fa._14)
                    )(fa._15)
                  )(fa._16)
                )(fa._17)
              )(fa._18)
            )(fa._19)
          )(fa._20)
        )(fa._21)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple21[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple21[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16], Compose2[
        G,
        A,
        T17
      ], Compose2[
        G,
        A,
        T18
      ], Compose2[G, A, T19], Compose2[G, A, T20], Compose2[G, A, T21]] =
        Tuple21(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16),
          G.map(gfa)(_._17),
          G.map(gfa)(_._18),
          G.map(gfa)(_._19),
          G.map(gfa)(_._20),
          G.map(gfa)(_._21)
        )
    }

  implicit def tuple22PerspectiveAllInstances[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
  ]: ApplicativeKC[
    Tuple22F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]#λ
  ] with TraverseKC[
      Tuple22F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]#λ
    ]
    with DistributiveKC[
      Tuple22F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]#λ
    ] =
    new ApplicativeKC[
      Tuple22F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]#λ
    ] with TraverseKC[
        Tuple22F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]#λ
      ]
      with DistributiveKC[
        Tuple22F[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]#λ
      ] {
      override def pureK[A[_], C](a: Unit #~>: A): Tuple22[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[
        T9
      ], A[T10], A[T11], A[T12], A[T13], A[T14], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21], A[T22]] =
        Tuple22(
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(()),
          a(())
        )

      override def traverseK[G[_], A[_], B[_], C](
          fa: Tuple22[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21], A[T22]]
      )(
          f: A ~>: Compose2[G, B, *]
      )(implicit G: Applicative[G]): G[Tuple22[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[
        T11
      ], B[T12], B[T13], B[T14], B[T15], B[T16], B[T17], B[T18], B[T19], B[T20], B[T21], B[T22]]] =
        G.map22(
          f(fa._1),
          f(fa._2),
          f(fa._3),
          f(fa._4),
          f(fa._5),
          f(fa._6),
          f(fa._7),
          f(fa._8),
          f(fa._9),
          f(fa._10),
          f(fa._11),
          f(fa._12),
          f(fa._13),
          f(fa._14),
          f(fa._15),
          f(fa._16),
          f(fa._17),
          f(fa._18),
          f(fa._19),
          f(fa._20),
          f(fa._21),
          f(fa._22)
        )(Tuple22.apply)

      override def map2K[A[_], B[_], Z[_], C](
          fa: Tuple22[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21], A[T22]],
          fb: Tuple22[B[T1], B[T2], B[T3], B[T4], B[T5], B[T6], B[T7], B[T8], B[T9], B[T10], B[T11], B[T12], B[T13], B[
            T14
          ], B[T15], B[T16], B[T17], B[T18], B[T19], B[T20], B[T21], B[T22]]
      )(f: Tuple2K[A, B, *] ~>: Z): Tuple22[Z[T1], Z[T2], Z[T3], Z[T4], Z[T5], Z[T6], Z[T7], Z[T8], Z[T9], Z[T10], Z[
        T11
      ], Z[T12], Z[T13], Z[T14], Z[T15], Z[T16], Z[T17], Z[T18], Z[T19], Z[T20], Z[T21], Z[T22]] =
        Tuple22(
          f(fa._1, fb._1),
          f(fa._2, fb._2),
          f(fa._3, fb._3),
          f(fa._4, fb._4),
          f(fa._5, fb._5),
          f(fa._6, fb._6),
          f(fa._7, fb._7),
          f(fa._8, fb._8),
          f(fa._9, fb._9),
          f(fa._10, fb._10),
          f(fa._11, fb._11),
          f(fa._12, fb._12),
          f(fa._13, fb._13),
          f(fa._14, fb._14),
          f(fa._15, fb._15),
          f(fa._16, fb._16),
          f(fa._17, fb._17),
          f(fa._18, fb._18),
          f(fa._19, fb._19),
          f(fa._20, fb._20),
          f(fa._21, fb._21),
          f(fa._22, fb._22)
        )

      override def foldLeftK[A[_], B, C](
          fa: Tuple22[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
            T14
          ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21], A[T22]],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(
                              f(
                                f(
                                  f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(
                                    fa._9
                                  )
                                )(fa._10)
                              )(fa._11)
                            )(fa._12)
                          )(fa._13)
                        )(fa._14)
                      )(fa._15)
                    )(fa._16)
                  )(fa._17)
                )(fa._18)
              )(fa._19)
            )(fa._20)
          )(fa._21)
        )(fa._22)

      override def cosequenceK[G[_], A[_], C](
          gfa: G[
            Tuple22[A[T1], A[T2], A[T3], A[T4], A[T5], A[T6], A[T7], A[T8], A[T9], A[T10], A[T11], A[T12], A[T13], A[
              T14
            ], A[T15], A[T16], A[T17], A[T18], A[T19], A[T20], A[T21], A[T22]]
          ]
      )(
          implicit G: Functor[G]
      ): Tuple22[Compose2[G, A, T1], Compose2[G, A, T2], Compose2[G, A, T3], Compose2[G, A, T4], Compose2[
        G,
        A,
        T5
      ], Compose2[
        G,
        A,
        T6
      ], Compose2[G, A, T7], Compose2[G, A, T8], Compose2[G, A, T9], Compose2[G, A, T10], Compose2[G, A, T11], Compose2[
        G,
        A,
        T12
      ], Compose2[G, A, T13], Compose2[G, A, T14], Compose2[G, A, T15], Compose2[G, A, T16], Compose2[
        G,
        A,
        T17
      ], Compose2[
        G,
        A,
        T18
      ], Compose2[G, A, T19], Compose2[G, A, T20], Compose2[G, A, T21], Compose2[G, A, T22]] =
        Tuple22(
          G.map(gfa)(_._1),
          G.map(gfa)(_._2),
          G.map(gfa)(_._3),
          G.map(gfa)(_._4),
          G.map(gfa)(_._5),
          G.map(gfa)(_._6),
          G.map(gfa)(_._7),
          G.map(gfa)(_._8),
          G.map(gfa)(_._9),
          G.map(gfa)(_._10),
          G.map(gfa)(_._11),
          G.map(gfa)(_._12),
          G.map(gfa)(_._13),
          G.map(gfa)(_._14),
          G.map(gfa)(_._15),
          G.map(gfa)(_._16),
          G.map(gfa)(_._17),
          G.map(gfa)(_._18),
          G.map(gfa)(_._19),
          G.map(gfa)(_._20),
          G.map(gfa)(_._21),
          G.map(gfa)(_._22)
        )
    }

  /*
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 22) {
      val tArgs = (1 to i).map(i => s"T$i")

      def mapTupleType(t: String => String): String = s"Tuple$i[${tArgs.map(t).mkString(", ")}]"

      def applyTupleType(a: String): String = mapTupleType(t => s"$a[$t]")

      def tuples(ident: String): Seq[String] = (1 to i).map(i => s"$ident._$i")

      def mapTuple(ident: String, f: String => String): String = tuples(ident).map(f).mkString(", ")

      val tupleF = s"Tuple${i}F[${tArgs.mkString(", ")}]#λ"

      val tpe = s"ApplicativeKC[$tupleF] with TraverseKC[$tupleF] with DistributiveKC[$tupleF]"

      val instance =
        s"""|implicit def tuple${i}PerspectiveAllInstances[${tArgs.mkString(", ")}]: $tpe = new $tpe {
            |  override def pureK[A[_], C](a: Unit #~>: A): ${applyTupleType("A")} = Tuple$i(${mapTuple("", _ => "a(())")})
            |
            |  override def traverseK[G[_], A[_], B[_], C](fa: ${applyTupleType("A")})(
            |      f: A ~>: Compose2[G, B, *]
            |  )(implicit G: Applicative[G]): G[${applyTupleType("B")}] =
            |    G.map$i(${mapTuple("fa", t => s"f($t)")})(Tuple$i.apply)
            |
            |  override def map2K[A[_], B[_], Z[_], C](
            |      fa: ${applyTupleType("A")},
            |      fb: ${applyTupleType("B")}
            |  )(f: Tuple2K[A, B, *] ~>: Z): ${applyTupleType("Z")} =
            |    Tuple$i(${mapTuple("", t => s"f(fa$t, fb$t)")})
            |
            |  override def foldLeftK[A[_], B, C](fa: ${applyTupleType("A")}, b: B)(f: B => A ~>#: B): B =
            |    ${tuples("fa").foldLeft("b")((acc, t) => s"f($acc)($t)")}
            |
            |  override def cosequenceK[G[_], A[_], C](
            |      gfa: G[${applyTupleType("A")}]
            |  )(implicit G: Functor[G]): ${mapTupleType(t => s"Compose2[G, A, $t]")} =
            |    Tuple$i(${mapTuple("_", t => s"G.map(gfa)($t)")})
            |}""".stripMargin

      println(instance.linesIterator.map(s => s"  $s").mkString("\n"))
      println()
    }
  }
   */
}
