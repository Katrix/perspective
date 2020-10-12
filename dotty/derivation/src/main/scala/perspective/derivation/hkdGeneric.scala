package perspective.derivation

import scala.deriving._
import scala.compiletime._
import scala.quoted._

import perspective._

sealed trait HKDGeneric[A]:
  type Gen[_[_]]
  type Index[A]

  def typeName: String

  def names: Gen[Const[String]]

  val representable: RepresentableKC.Aux[Gen, Index]
  val traverse: TraverseKC[Gen]

  given RepresentableKC.Aux[Gen, Index] = representable
  given TraverseKC[Gen] = traverse

object HKDGeneric:
  type Aux[A, Gen0[_[_]]] = HKDGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.Of[A], qctx: QuoteContext, tpe: Type[A]) as HKDGeneric[A] = m match 
    case m: Mirror.ProductOf[A] => ??? //HKDProductGeneric.derived
    case m: Mirror.SumOf[A] => ???

trait HKDProductGeneric[A] extends HKDGeneric[A]:
  def to(a: A): Gen[Id]
  def from(gen: Gen[Id]): A

object HKDProductGeneric:
  def apply[A](using gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  inline def make[A]: HKDProductGeneric[A] = ${ test }

  def test[A: Type](using qctx: QuoteContext): Expr[HKDProductGeneric[A]] = {
    //import qctx.tasty._

    Expr.summon(using '[Mirror.Of[A]]).get match
      case '{ $m: Mirror.ProductOf[A] { type MirroredElemTypes = $elementTypes }} =>
        println(elementTypes)

        /*
        val size = constValue[Tuple.Size[m.MirroredElemTypes]]

        def genType[F[_]: Type] = size match 
          case 0 => '[Product0K[F]]
          case 1 =>
        */ 

    /*
    '{
      new HKDProductGeneric[A]:

        type Gen[F[_]] = ${
          size match
            case 0 => '[Product0K[F]]
            case 1 => '[Product1K[Int, F]]
        }

        type Index[Z] = ${
          size match
            case 0 => '[Product0K.Index[Z]]
            case 1 => '[Product1K.Index[Z]]
        }

        override def typeName: String = constValue[m.MirroredLabel]

        override def names: Gen[Const[String]] = ${
          size match
            case 0 => '{Product0K.fromTuple()}
            case 1 => '{Product1K.fromTuple(constValueTuple[m.MirroredLabel])}
        }

        override def to(a: A): Gen[Id] = ${
          size match
            case 0 => '{Product0K.fromTuple()}
            case 1 => '{Product1K.fromTuple(Tuple.fromProductTyped(a))}
        }

        override def from(gen: Gen[Id]) = m.fromProduct(gen.toTuple)

        override val representable: RepresentableKC.Aux[Gen, Index] = ${
          size match
            case 0 => '{Product0K.representable)}
            case 1 => '{Product1K.representable}
        }
        override val traverse: TraverseKC[Gen] = ${
          size match
            case 0 => '{Product0K.traverse)}
            case 1 => '{Product1K.traverse}
        }
    }
    */

    '{???}
  }

trait HKDSumGeneric[A] extends HKDGeneric[A]:
  type Index2 <: {
    type X <: A
  }
  type Index[Z] = Index2 { type X = Z }

  def indexNameMap: Map[String, Index2]
  def indexOf[X <: A](x: X): Index[X]

  def to(a: A): Gen[Option]
  def from(a: Gen[Option]): Option[A]

object HKDSumGeneric:
  def apply[A](using gen: HKDSumGeneric[A]): HKDSumGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDSumGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
