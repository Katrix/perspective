---
title: Typeclasses 
---

# {{page.title}}

perspective offers many higher kinded versions of typical functional typeclasses. `perspective`
offers these higher kinded typeclasses under the same name as the normal typeclass, with a `K`
suffix. These typeclasses have the kind `trait K[F[_[_], _]]`. The kind `trait KC[F[_[_]]]` is
exposed under the suffix `KC`.

Some functions within these typeclasses are also specialized to certain types to help type
inference. These functions often have suffixes like `Id` and `Const` instead of a `K` suffix.

## Type aliases

perspective uses a few type aliases to make function signatures simpler, and more like their non
higher kinded versions.

```scala 3 sc:nocompile
/** A higher kinded type that ignores its second type parameter. */
type Const[A] = [_] =>> A

type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]

object FunctionK:
  def identity[A[_]]: A ~>: A = [Z] => (a: A[Z]) => a

infix type ~>:[A[_], B[_]] = FunctionK[A, B]

/** A FunctionK returning a [[Const]] type. */
infix type ~>#:[F[_], R] = F ~>: Const[R]

/** A FunctionK taking a [[Const]] type. */
infix type #~>:[T, F[_]] = Const[T] ~>: F

/** A FunctionK taking and returning [[Const]] types. */
infix type #~>#:[T, R] = Const[T] ~>: Const[R]
```

## `FunctorK`

```scala 3 sc:nocompile
trait FunctorK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    def mapK[B[_]](f: A ~>: B): F[B, C]
```

`FunctorK` exposes functions to convert `F[A]` to `F[B]` using a function to convert from `A`
to `B`.

## `ApplyK`

```scala 3 sc:nocompile
trait ApplyK[F[_[_], _]] extends FunctorK[F]:
  extension [A[_], C](fa: F[A, C])
    def map2K[B[_], Z[_]](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C]
```

The `ApplyK` typeclass allows combining several different values in a context into a single
value of that context. Apply is a useful tool for many generic programming tasks.

`Apply` is less common typeclass, a simplification of the typeclass called `Applicative`. `Apply` is
an `Applicative` without the function `pure`. `Apply` is seperated from `Applicative` and given more
light in `perspective` because `pure` seems like useful in higher kinded contexts.

## `FoldableK`

```scala 3 sc:nocompile
trait FoldableK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    def foldLeftK[B](b: B)(f: B => A ~>#: B): 
```

`FoldableK` exposes functions to convert a context `F[A]` to a value `B` by applying a function on
all the values of the container together the result of the previous application.

For generic programming, `FoldableK` is useful when each value can be encoded to a non higher kinded
type, and combined together.

## `TraverseK`

```scala 3 sc:nocompile
/** The composition of two higher kinded types. */
type Compose2[A[_], B[_]] = [Z] =>> A[B[Z]]

trait TraverseK[F[_[_], _]] extends FunctorK[F], FoldableK[F]:
  extension [A[_], C](fa: F[A, C])
    def traverseK[G[_] : Applicative, B[_]](f: A ~>: Compose2[G, B]): G[F[B, C]]
```

As used typically, `TraverseK` allows one to convert from `F[G[A]]` to `G[F[A]]`.

For generic programming, `TraverseK` is useful when doing some operation that can fail, and one
wants to collect all these failures.

## `RepresentableK`

```scala 3 sc:nocompile
trait RepresentableK[F[_[_], _]] extends MonadK[F] with DistributiveK[F]:
  type RepresentationK[_]

  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]

  def indicesK[C]: F[RepresentationK, C] = tabulateK(FunctionK.identity)

  extension [A[_], C](fa: F[A, C])
    def indexK[Z](i: RepresentationK[Z]): A[Z]
```

`Representable` and it's higher kinded variants `RepresentableK` (and `RepresentableKC`) is
generally a less used typeclass, but plays a central piece for generic programming. This typeclass
offers the functions `indexK` and `tabulateK` to index and tabulate over a type, in similar ways
to `List.tabulate` and `List#apply`. The index type used by `Representable` is not `Int`, but
some other  ``