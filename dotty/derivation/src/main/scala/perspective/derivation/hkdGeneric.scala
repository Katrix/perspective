package perspective.derivation

import scala.language.implicitConversions

import scala.deriving._
import scala.compiletime._
import scala.quoted._

import perspective._

sealed trait HKDGeneric[A]:
  type Gen[_[_]]
  type Index[A]
  type TupleRep <: Tuple

  def typeName: String

  def names: Gen[Const[String]]
  
  def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F]
  def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F]

  val representable: RepresentableKC.Aux[Gen, Index]
  val traverse: TraverseKC[Gen]

  given RepresentableKC.Aux[Gen, Index] = representable
  given TraverseKC[Gen] = traverse

object HKDGeneric:
  type Aux[A, Gen0[_[_]]] = HKDGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
  
  transparent inline given derived[A](using m: Mirror.Of[A])(
    using ValueOf[Tuple.Size[m.MirroredElemTypes]],
    Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDGeneric[A] = inline m match
    case m: Mirror.ProductOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDProductGeneric.derived[A](using m)
      
    case m: Mirror.SumOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDSumGeneric.derived[A](using m)

trait HKDProductGeneric[A] extends HKDGeneric[A]:
  def to(a: A): Gen[Id]
  def from(gen: Gen[Id]): A

object HKDProductGeneric:
  def apply[A](using gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.ProductOf[A])(
    using ValueOf[Tuple.Size[m.MirroredElemTypes]],
    Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDProductGeneric[A] = derivedImpl(
    constValue[m.MirroredLabel],
    constValueTuple[m.MirroredElemLabels].asInstanceOf[Tuple.Map[m.MirroredElemTypes, Const[String]]]
  )

  def derivedImpl[A, ElemTypes <: Tuple](label: String, namesTuple: Tuple.Map[ElemTypes, Const[String]])(
    using m: Mirror.ProductOf[A] { type MirroredElemTypes = ElemTypes }
  )(
    using ValueOf[Tuple.Size[m.MirroredElemTypes]], 
    Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDProductGeneric[A] {
    type Gen[F[_]] = ProductK[F, m.MirroredElemTypes]
    type Index[_] = Finite[Tuple.Size[m.MirroredElemTypes]]
    type TupleRep = m.MirroredElemTypes
  } =
    new HKDProductGeneric[A]:
      type Gen[F[_]] = ProductK[F, m.MirroredElemTypes]
      type Index[_] = Finite[Tuple.Size[m.MirroredElemTypes]]
      type TupleRep = m.MirroredElemTypes

      override def typeName: String = label

      override def names: Gen[Const[String]] = 
        ProductK.of[Const[String], m.MirroredElemTypes](namesTuple)

      override def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F] = gen.tuple
      override def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] = ProductK.of(tuple)

      override def to(a: A): Gen[Id] = 
        ProductK.of[Id, m.MirroredElemTypes](Tuple.fromProduct(a.asInstanceOf[Product]).asInstanceOf[Tuple.Map[m.MirroredElemTypes, Id]])
      override def from(a: Gen[Id]): A = 
        m.fromProduct(a.tuple)

      private val instance: RepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] = 
        ProductK.productKInstance[m.MirroredElemTypes]

      override val representable: RepresentableKC.Aux[Gen, Index] = instance
      override val traverse: TraverseKC[Gen] = instance

trait HKDSumGeneric[A] extends HKDGeneric[A]:
  class IdxWrapper[X](val idx: Index[X])
  
  given [X]: Conversion[IdxWrapper[X], Index[X]] = _.idx
  given [X]: Conversion[Index[X], IdxWrapper[X]] = new IdxWrapper(_)
  
  //Preferably we would say type Index[X <: A], but we can't
  def upcastIndexed[X](idx: Index[X], x: X): A  = x.asInstanceOf[A]
  def upcastIndex[X](idx: Index[X]): IdxWrapper[_ <: A] = new IdxWrapper(idx).asInstanceOf[IdxWrapper[_ <: A]]

  def nameToIndexMap: Map[String, IdxWrapper[_ <: A]]
  def indexToNameMap: Map[IdxWrapper[_ <: A], String]

  def indexOf[X <: A](x: X): Index[X]

  def to(a: A): Gen[Option] =
    val index = indexOf(a)
    //This cast is safe as we know A = Z
    representable.tabulateK([Z] => (i: Index[Z]) => if i == index then Some(a.asInstanceOf[Z]) else None)
  
  def from(a: Gen[Option]): Option[A] =
    //This is safe. We can't use the widen method as it can't know about the contents of Gen, we do
    val aWiden = a.asInstanceOf[Gen[Const[Option[A]]]]
    val asList = traverse.toListK[Option[A], Nothing](aWiden).flatten
    asList match
      case Nil      => None    //No values present
      case a :: Nil => Some(a) //One value present
      case _        => None    //More than one value present

object HKDSumGeneric:
  def apply[A](using gen: HKDSumGeneric[A]): HKDSumGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDSumGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.SumOf[A])(
    using ValueOf[Tuple.Size[m.MirroredElemTypes]],
    Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDSumGeneric[A] = derivedImpl(
    constValue[m.MirroredLabel],
    constValueTuple[m.MirroredElemLabels].asInstanceOf[Tuple.Map[m.MirroredElemTypes, Const[String]]],
    constValue[Tuple.Size[m.MirroredElemTypes]]
  )

  def derivedImpl[A, ElemTypes <: Tuple](label: String, namesTuple: Tuple.Map[ElemTypes, Const[String]], size: Tuple.Size[ElemTypes])(
    using m: Mirror.SumOf[A] { type MirroredElemTypes = ElemTypes }
  )(
    using ValueOf[Tuple.Size[m.MirroredElemTypes]],
    Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDSumGeneric[A] {
    type Gen[F[_]] = ProductK[F, m.MirroredElemTypes]
    type Index[_] = Finite[Tuple.Size[m.MirroredElemTypes]]
    type TupleRep = m.MirroredElemTypes
  } = 
    new HKDSumGeneric[A]:
      type Gen[F[_]] = ProductK[F, m.MirroredElemTypes]
      type Index[_] = Finite[Tuple.Size[m.MirroredElemTypes]]
      type TupleRep = m.MirroredElemTypes
    
      override def typeName: String = label
    
      override def names: Gen[Const[String]] =
        ProductK.of[Const[String], m.MirroredElemTypes](namesTuple)
        
      override def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F] = gen.tuple
      override def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] = ProductK.of(tuple)
    
      private val instance: RepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[m.MirroredElemTypes]
  
      override val representable: RepresentableKC.Aux[Gen, Index] = instance
      override val traverse: TraverseKC[Gen] = instance
      
      def nameToIndexMap: Map[String, IdxWrapper[_ <: A]] = 
        names.map2K[Index, Const[(String, IdxWrapper[_ <: A])]](representable.indicesK)(
          [Z] => (name: String, idx: Index[Z]) => (name, upcastIndex(idx))
        ).toListK.toMap
      
      def indexToNameMap: Map[IdxWrapper[_ <: A], String] = 
        nameToIndexMap.map(_.swap)
  
      def indexOf[X <: A](x: X): Index[X] = 
        Finite(size, m.ordinal(x))
