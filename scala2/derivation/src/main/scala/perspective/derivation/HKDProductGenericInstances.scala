package perspective.derivation

import cats.Id

import shapeless.{Const => _, _}
import shapeless.labelled._
import shapeless.nat._
import perspective._

//TODO: Going through shapeless for this is SLOW. We should implement a macro for this instead.
trait HKDProductGenericInstances extends LowPriorityHKDProductGenericInstances {

  implicit def product1FromShapeless[Prod, L <: HList, K1 <: Symbol, T1](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: HNil),
      k1: Witness.Aux[K1]
  ): HKDProductGeneric.Aux[Prod, Product1K[*[_], T1]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product1K[A, T1]

    override def names: Product1K[Const[String, *], T1] = Product1K[Const[String, *], T1](
      k1.value.name
    )

    override def to(a: Prod): Product1K[Id, T1] = {
      val hlist = gen.to(a)
      val list1 = hlist
      Product1K[Id, T1](
        list1.head
      )
    }

    override def from(prod: Product1K[Id, T1]): Prod =
      gen.from(
        ev2(field[K1](prod.p1) :: HNil)
      )

    override def representable: RepresentableKC[Product1K[*[_], T1]] =
      Product1K.product1KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product1K[*[_], T1]] =
      Product1K.product1KRepresentableTraverseInstance
  }

  implicit def product2FromShapeless[Prod, L <: HList, K1 <: Symbol, K2 <: Symbol, T1, T2](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2]
  ): HKDProductGeneric.Aux[Prod, Product2K[*[_], T1, T2]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product2K[A, T1, T2]

    override def names: Product2K[Const[String, *], T1, T2] = Product2K[Const[String, *], T1, T2](
      k1.value.name,
      k2.value.name
    )

    override def to(a: Prod): Product2K[Id, T1, T2] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      Product2K[Id, T1, T2](
        list1.head,
        list2.head
      )
    }

    override def from(prod: Product2K[Id, T1, T2]): Prod =
      gen.from(
        ev2(field[K1](prod.p1) :: field[K2](prod.p2) :: HNil)
      )

    override def representable: RepresentableKC[Product2K[*[_], T1, T2]] =
      Product2K.product2KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product2K[*[_], T1, T2]] =
      Product2K.product2KRepresentableTraverseInstance
  }

  implicit def product3FromShapeless[Prod, L <: HList, K1 <: Symbol, K2 <: Symbol, K3 <: Symbol, T1, T2, T3](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3]
  ): HKDProductGeneric.Aux[Prod, Product3K[*[_], T1, T2, T3]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product3K[A, T1, T2, T3]

    override def names: Product3K[Const[String, *], T1, T2, T3] = Product3K[Const[String, *], T1, T2, T3](
      k1.value.name,
      k2.value.name,
      k3.value.name
    )

    override def to(a: Prod): Product3K[Id, T1, T2, T3] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      Product3K[Id, T1, T2, T3](
        list1.head,
        list2.head,
        list3.head
      )
    }

    override def from(prod: Product3K[Id, T1, T2, T3]): Prod =
      gen.from(
        ev2(field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: HNil)
      )

    override def representable: RepresentableKC[Product3K[*[_], T1, T2, T3]] =
      Product3K.product3KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product3K[*[_], T1, T2, T3]] =
      Product3K.product3KRepresentableTraverseInstance
  }

  implicit def product4FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      T1,
      T2,
      T3,
      T4
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4]
  ): HKDProductGeneric.Aux[Prod, Product4K[*[_], T1, T2, T3, T4]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product4K[A, T1, T2, T3, T4]

    override def names: Product4K[Const[String, *], T1, T2, T3, T4] = Product4K[Const[String, *], T1, T2, T3, T4](
      k1.value.name,
      k2.value.name,
      k3.value.name,
      k4.value.name
    )

    override def to(a: Prod): Product4K[Id, T1, T2, T3, T4] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      val list4 = list3.tail
      Product4K[Id, T1, T2, T3, T4](
        list1.head,
        list2.head,
        list3.head,
        list4.head
      )
    }

    override def from(prod: Product4K[Id, T1, T2, T3, T4]): Prod =
      gen.from(
        ev2(field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: HNil)
      )

    override def representable: RepresentableKC[Product4K[*[_], T1, T2, T3, T4]] =
      Product4K.product4KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product4K[*[_], T1, T2, T3, T4]] =
      Product4K.product4KRepresentableTraverseInstance
  }

  implicit def product5FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      T1,
      T2,
      T3,
      T4,
      T5
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5]
  ): HKDProductGeneric.Aux[Prod, Product5K[*[_], T1, T2, T3, T4, T5]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product5K[A, T1, T2, T3, T4, T5]

    override def names: Product5K[Const[String, *], T1, T2, T3, T4, T5] =
      Product5K[Const[String, *], T1, T2, T3, T4, T5](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name
      )

    override def to(a: Prod): Product5K[Id, T1, T2, T3, T4, T5] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      val list4 = list3.tail
      val list5 = list4.tail
      Product5K[Id, T1, T2, T3, T4, T5](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head
      )
    }

    override def from(prod: Product5K[Id, T1, T2, T3, T4, T5]): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: HNil
        )
      )

    override def representable: RepresentableKC[Product5K[*[_], T1, T2, T3, T4, T5]] =
      Product5K.product5KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product5K[*[_], T1, T2, T3, T4, T5]] =
      Product5K.product5KRepresentableTraverseInstance
  }

  implicit def product6FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6]
  ): HKDProductGeneric.Aux[Prod, Product6K[*[_], T1, T2, T3, T4, T5, T6]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product6K[A, T1, T2, T3, T4, T5, T6]

    override def names: Product6K[Const[String, *], T1, T2, T3, T4, T5, T6] =
      Product6K[Const[String, *], T1, T2, T3, T4, T5, T6](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name
      )

    override def to(a: Prod): Product6K[Id, T1, T2, T3, T4, T5, T6] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      val list4 = list3.tail
      val list5 = list4.tail
      val list6 = list5.tail
      Product6K[Id, T1, T2, T3, T4, T5, T6](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head
      )
    }

    override def from(prod: Product6K[Id, T1, T2, T3, T4, T5, T6]): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: HNil
        )
      )

    override def representable: RepresentableKC[Product6K[*[_], T1, T2, T3, T4, T5, T6]] =
      Product6K.product6KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product6K[*[_], T1, T2, T3, T4, T5, T6]] =
      Product6K.product6KRepresentableTraverseInstance
  }

  implicit def product7FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7]
  ): HKDProductGeneric.Aux[Prod, Product7K[*[_], T1, T2, T3, T4, T5, T6, T7]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product7K[A, T1, T2, T3, T4, T5, T6, T7]

    override def names: Product7K[Const[String, *], T1, T2, T3, T4, T5, T6, T7] =
      Product7K[Const[String, *], T1, T2, T3, T4, T5, T6, T7](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name
      )

    override def to(a: Prod): Product7K[Id, T1, T2, T3, T4, T5, T6, T7] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      val list4 = list3.tail
      val list5 = list4.tail
      val list6 = list5.tail
      val list7 = list6.tail
      Product7K[Id, T1, T2, T3, T4, T5, T6, T7](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head
      )
    }

    override def from(prod: Product7K[Id, T1, T2, T3, T4, T5, T6, T7]): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: HNil
        )
      )

    override def representable: RepresentableKC[Product7K[*[_], T1, T2, T3, T4, T5, T6, T7]] =
      Product7K.product7KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product7K[*[_], T1, T2, T3, T4, T5, T6, T7]] =
      Product7K.product7KRepresentableTraverseInstance
  }

  implicit def product8FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8]
  ): HKDProductGeneric.Aux[Prod, Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product8K[A, T1, T2, T3, T4, T5, T6, T7, T8]

    override def names: Product8K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8] =
      Product8K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name
      )

    override def to(a: Prod): Product8K[Id, T1, T2, T3, T4, T5, T6, T7, T8] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      val list4 = list3.tail
      val list5 = list4.tail
      val list6 = list5.tail
      val list7 = list6.tail
      val list8 = list7.tail
      Product8K[Id, T1, T2, T3, T4, T5, T6, T7, T8](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head
      )
    }

    override def from(prod: Product8K[Id, T1, T2, T3, T4, T5, T6, T7, T8]): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: HNil
        )
      )

    override def representable: RepresentableKC[Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8]] =
      Product8K.product8KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8]] =
      Product8K.product8KRepresentableTraverseInstance
  }

  implicit def product9FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9]
  ): HKDProductGeneric.Aux[Prod, Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product9K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9]

    override def names: Product9K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9] =
      Product9K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name
      )

    override def to(a: Prod): Product9K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9] = {
      val hlist = gen.to(a)
      val list1 = hlist
      val list2 = list1.tail
      val list3 = list2.tail
      val list4 = list3.tail
      val list5 = list4.tail
      val list6 = list5.tail
      val list7 = list6.tail
      val list8 = list7.tail
      val list9 = list8.tail
      Product9K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head
      )
    }

    override def from(prod: Product9K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9]): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: HNil
        )
      )

    override def representable: RepresentableKC[Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
      Product9K.product9KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
      Product9K.product9KRepresentableTraverseInstance
  }

  implicit def product10FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10]
  ): HKDProductGeneric.Aux[Prod, Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
    new HKDProductGeneric[Prod] {
      private val ev2 = ev.flip

      override type Gen[A[_]] = Product10K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]

      override def names: Product10K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
        Product10K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
          k1.value.name,
          k2.value.name,
          k3.value.name,
          k4.value.name,
          k5.value.name,
          k6.value.name,
          k7.value.name,
          k8.value.name,
          k9.value.name,
          k10.value.name
        )

      override def to(a: Prod): Product10K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = {
        val hlist  = gen.to(a)
        val list1  = hlist
        val list2  = list1.tail
        val list3  = list2.tail
        val list4  = list3.tail
        val list5  = list4.tail
        val list6  = list5.tail
        val list7  = list6.tail
        val list8  = list7.tail
        val list9  = list8.tail
        val list10 = list9.tail
        Product10K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
          list1.head,
          list2.head,
          list3.head,
          list4.head,
          list5.head,
          list6.head,
          list7.head,
          list8.head,
          list9.head,
          list10.head
        )
      }

      override def from(prod: Product10K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): Prod =
        gen.from(
          ev2(
            field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
              K6
            ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: HNil
          )
        )

      override def representable: RepresentableKC[Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
        Product10K.product10KRepresentableTraverseInstance

      override def traverse: TraverseKC[Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
        Product10K.product10KRepresentableTraverseInstance
    }

  implicit def product11FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
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
      T11
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11]
  ): HKDProductGeneric.Aux[Prod, Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
    new HKDProductGeneric[Prod] {
      private val ev2 = ev.flip

      override type Gen[A[_]] = Product11K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]

      override def names: Product11K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
        Product11K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
          k1.value.name,
          k2.value.name,
          k3.value.name,
          k4.value.name,
          k5.value.name,
          k6.value.name,
          k7.value.name,
          k8.value.name,
          k9.value.name,
          k10.value.name,
          k11.value.name
        )

      override def to(a: Prod): Product11K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = {
        val hlist  = gen.to(a)
        val list1  = hlist
        val list2  = list1.tail
        val list3  = list2.tail
        val list4  = list3.tail
        val list5  = list4.tail
        val list6  = list5.tail
        val list7  = list6.tail
        val list8  = list7.tail
        val list9  = list8.tail
        val list10 = list9.tail
        val list11 = list10.tail
        Product11K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
          list1.head,
          list2.head,
          list3.head,
          list4.head,
          list5.head,
          list6.head,
          list7.head,
          list8.head,
          list9.head,
          list10.head,
          list11.head
        )
      }

      override def from(prod: Product11K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): Prod =
        gen.from(
          ev2(
            field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
              K6
            ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
              K11
            ](prod.p11) :: HNil
          )
        )

      override def representable: RepresentableKC[Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
        Product11K.product11KRepresentableTraverseInstance

      override def traverse: TraverseKC[Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
        Product11K.product11KRepresentableTraverseInstance
    }

  implicit def product12FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
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
      T12
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12]
  ): HKDProductGeneric.Aux[Prod, Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
    new HKDProductGeneric[Prod] {
      private val ev2 = ev.flip

      override type Gen[A[_]] = Product12K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]

      override def names: Product12K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
        Product12K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
          k1.value.name,
          k2.value.name,
          k3.value.name,
          k4.value.name,
          k5.value.name,
          k6.value.name,
          k7.value.name,
          k8.value.name,
          k9.value.name,
          k10.value.name,
          k11.value.name,
          k12.value.name
        )

      override def to(a: Prod): Product12K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = {
        val hlist  = gen.to(a)
        val list1  = hlist
        val list2  = list1.tail
        val list3  = list2.tail
        val list4  = list3.tail
        val list5  = list4.tail
        val list6  = list5.tail
        val list7  = list6.tail
        val list8  = list7.tail
        val list9  = list8.tail
        val list10 = list9.tail
        val list11 = list10.tail
        val list12 = list11.tail
        Product12K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
          list1.head,
          list2.head,
          list3.head,
          list4.head,
          list5.head,
          list6.head,
          list7.head,
          list8.head,
          list9.head,
          list10.head,
          list11.head,
          list12.head
        )
      }

      override def from(prod: Product12K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): Prod =
        gen.from(
          ev2(
            field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
              K6
            ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
              K11
            ](prod.p11) :: field[K12](prod.p12) :: HNil
          )
        )

      override def representable: RepresentableKC[Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
        Product12K.product12KRepresentableTraverseInstance

      override def traverse: TraverseKC[Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
        Product12K.product12KRepresentableTraverseInstance
    }

  implicit def product13FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
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
      T13
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13]
  ): HKDProductGeneric.Aux[Prod, Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
    new HKDProductGeneric[Prod] {
      private val ev2 = ev.flip

      override type Gen[A[_]] = Product13K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]

      override def names: Product13K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
        Product13K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
          k1.value.name,
          k2.value.name,
          k3.value.name,
          k4.value.name,
          k5.value.name,
          k6.value.name,
          k7.value.name,
          k8.value.name,
          k9.value.name,
          k10.value.name,
          k11.value.name,
          k12.value.name,
          k13.value.name
        )

      override def to(a: Prod): Product13K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = {
        val hlist  = gen.to(a)
        val list1  = hlist
        val list2  = list1.tail
        val list3  = list2.tail
        val list4  = list3.tail
        val list5  = list4.tail
        val list6  = list5.tail
        val list7  = list6.tail
        val list8  = list7.tail
        val list9  = list8.tail
        val list10 = list9.tail
        val list11 = list10.tail
        val list12 = list11.tail
        val list13 = list12.tail
        Product13K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
          list1.head,
          list2.head,
          list3.head,
          list4.head,
          list5.head,
          list6.head,
          list7.head,
          list8.head,
          list9.head,
          list10.head,
          list11.head,
          list12.head,
          list13.head
        )
      }

      override def from(prod: Product13K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): Prod =
        gen.from(
          ev2(
            field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
              K6
            ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
              K11
            ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: HNil
          )
        )

      override def representable
          : RepresentableKC[Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
        Product13K.product13KRepresentableTraverseInstance

      override def traverse: TraverseKC[Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
        Product13K.product13KRepresentableTraverseInstance
    }

  implicit def product14FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
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
      T14
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14]
  ): HKDProductGeneric.Aux[Prod, Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
    new HKDProductGeneric[Prod] {
      private val ev2 = ev.flip

      override type Gen[A[_]] = Product14K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]

      override def names: Product14K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
        Product14K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
          k1.value.name,
          k2.value.name,
          k3.value.name,
          k4.value.name,
          k5.value.name,
          k6.value.name,
          k7.value.name,
          k8.value.name,
          k9.value.name,
          k10.value.name,
          k11.value.name,
          k12.value.name,
          k13.value.name,
          k14.value.name
        )

      override def to(a: Prod): Product14K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = {
        val hlist  = gen.to(a)
        val list1  = hlist
        val list2  = list1.tail
        val list3  = list2.tail
        val list4  = list3.tail
        val list5  = list4.tail
        val list6  = list5.tail
        val list7  = list6.tail
        val list8  = list7.tail
        val list9  = list8.tail
        val list10 = list9.tail
        val list11 = list10.tail
        val list12 = list11.tail
        val list13 = list12.tail
        val list14 = list13.tail
        Product14K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
          list1.head,
          list2.head,
          list3.head,
          list4.head,
          list5.head,
          list6.head,
          list7.head,
          list8.head,
          list9.head,
          list10.head,
          list11.head,
          list12.head,
          list13.head,
          list14.head
        )
      }

      override def from(prod: Product14K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): Prod =
        gen.from(
          ev2(
            field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
              K6
            ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
              K11
            ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: HNil
          )
        )

      override def representable
          : RepresentableKC[Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
        Product14K.product14KRepresentableTraverseInstance

      override def traverse: TraverseKC[Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
        Product14K.product14KRepresentableTraverseInstance
    }

  implicit def product15FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
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
      T15
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15]
  ): HKDProductGeneric.Aux[Prod, Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
    new HKDProductGeneric[Prod] {
      private val ev2 = ev.flip

      override type Gen[A[_]] = Product15K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]

      override def names
          : Product15K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
        Product15K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
          k1.value.name,
          k2.value.name,
          k3.value.name,
          k4.value.name,
          k5.value.name,
          k6.value.name,
          k7.value.name,
          k8.value.name,
          k9.value.name,
          k10.value.name,
          k11.value.name,
          k12.value.name,
          k13.value.name,
          k14.value.name,
          k15.value.name
        )

      override def to(a: Prod): Product15K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = {
        val hlist  = gen.to(a)
        val list1  = hlist
        val list2  = list1.tail
        val list3  = list2.tail
        val list4  = list3.tail
        val list5  = list4.tail
        val list6  = list5.tail
        val list7  = list6.tail
        val list8  = list7.tail
        val list9  = list8.tail
        val list10 = list9.tail
        val list11 = list10.tail
        val list12 = list11.tail
        val list13 = list12.tail
        val list14 = list13.tail
        val list15 = list14.tail
        Product15K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
          list1.head,
          list2.head,
          list3.head,
          list4.head,
          list5.head,
          list6.head,
          list7.head,
          list8.head,
          list9.head,
          list10.head,
          list11.head,
          list12.head,
          list13.head,
          list14.head,
          list15.head
        )
      }

      override def from(prod: Product15K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): Prod =
        gen.from(
          ev2(
            field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
              K6
            ](
              prod.p6
            ) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[K11](
              prod.p11
            ) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: HNil
          )
        )

      override def representable
          : RepresentableKC[Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
        Product15K.product15KRepresentableTraverseInstance

      override def traverse
          : TraverseKC[Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
        Product15K.product15KRepresentableTraverseInstance
    }

  implicit def product16FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
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
      T16
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16]
  ): HKDProductGeneric.Aux[Prod, Product16K[
    *[_],
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
    T16
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product16K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]

    override def names
        : Product16K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
      Product16K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name
      )

    override def to(a: Prod): Product16K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      Product16K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head
      )
    }

    override def from(
        prod: Product16K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: HNil
        )
      )

    override def representable
        : RepresentableKC[Product16K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] =
      Product16K.product16KRepresentableTraverseInstance

    override def traverse
        : TraverseKC[Product16K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] =
      Product16K.product16KRepresentableTraverseInstance
  }

  implicit def product17FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
      K17 <: Symbol,
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
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: FieldType[
        K17,
        T17
      ] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16],
      k17: Witness.Aux[K17]
  ): HKDProductGeneric.Aux[Prod, Product17K[
    *[_],
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
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] = Product17K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]

    override def names
        : Product17K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
      Product17K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name,
        k17.value.name
      )

    override def to(
        a: Prod
    ): Product17K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      val list17 = list16.tail
      Product17K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head,
        list17.head
      )
    }

    override def from(
        prod: Product17K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: field[K17](prod.p17) :: HNil
        )
      )

    override def representable: RepresentableKC[
      Product17K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    ] =
      Product17K.product17KRepresentableTraverseInstance

    override def traverse
        : TraverseKC[Product17K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] =
      Product17K.product17KRepresentableTraverseInstance
  }

  implicit def product18FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
      K17 <: Symbol,
      K18 <: Symbol,
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
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: FieldType[
        K17,
        T17
      ] :: FieldType[K18, T18] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16],
      k17: Witness.Aux[K17],
      k18: Witness.Aux[K18]
  ): HKDProductGeneric.Aux[Prod, Product18K[
    *[_],
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
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] =
      Product18K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]

    override def names: Product18K[
      Const[String, *],
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
    ] =
      Product18K[Const[String, *], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name,
        k17.value.name,
        k18.value.name
      )

    override def to(
        a: Prod
    ): Product18K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      val list17 = list16.tail
      val list18 = list17.tail
      Product18K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head,
        list17.head,
        list18.head
      )
    }

    override def from(
        prod: Product18K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: field[K17](prod.p17) :: field[K18](prod.p18) :: HNil
        )
      )

    override def representable: RepresentableKC[
      Product18K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    ] =
      Product18K.product18KRepresentableTraverseInstance

    override def traverse: TraverseKC[
      Product18K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    ] =
      Product18K.product18KRepresentableTraverseInstance
  }

  implicit def product19FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
      K17 <: Symbol,
      K18 <: Symbol,
      K19 <: Symbol,
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
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: FieldType[
        K17,
        T17
      ] :: FieldType[K18, T18] :: FieldType[K19, T19] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16],
      k17: Witness.Aux[K17],
      k18: Witness.Aux[K18],
      k19: Witness.Aux[K19]
  ): HKDProductGeneric.Aux[Prod, Product19K[
    *[_],
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
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] =
      Product19K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]

    override def names: Product19K[
      Const[String, *],
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
    ] =
      Product19K[
        Const[String, *],
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
      ](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name,
        k17.value.name,
        k18.value.name,
        k19.value.name
      )

    override def to(
        a: Prod
    ): Product19K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      val list17 = list16.tail
      val list18 = list17.tail
      val list19 = list18.tail
      Product19K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head,
        list17.head,
        list18.head,
        list19.head
      )
    }

    override def from(
        prod: Product19K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: field[K17](prod.p17) :: field[K18](prod.p18) :: field[K19](prod.p19) :: HNil
        )
      )

    override def representable: RepresentableKC[
      Product19K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    ] =
      Product19K.product19KRepresentableTraverseInstance

    override def traverse: TraverseKC[
      Product19K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    ] =
      Product19K.product19KRepresentableTraverseInstance
  }

  implicit def product20FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
      K17 <: Symbol,
      K18 <: Symbol,
      K19 <: Symbol,
      K20 <: Symbol,
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
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: FieldType[
        K17,
        T17
      ] :: FieldType[K18, T18] :: FieldType[K19, T19] :: FieldType[K20, T20] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16],
      k17: Witness.Aux[K17],
      k18: Witness.Aux[K18],
      k19: Witness.Aux[K19],
      k20: Witness.Aux[K20]
  ): HKDProductGeneric.Aux[Prod, Product20K[
    *[_],
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
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] =
      Product20K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]

    override def names: Product20K[
      Const[String, *],
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
    ] =
      Product20K[
        Const[String, *],
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
      ](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name,
        k17.value.name,
        k18.value.name,
        k19.value.name,
        k20.value.name
      )

    override def to(
        a: Prod
    ): Product20K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      val list17 = list16.tail
      val list18 = list17.tail
      val list19 = list18.tail
      val list20 = list19.tail
      Product20K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head,
        list17.head,
        list18.head,
        list19.head,
        list20.head
      )
    }

    override def from(
        prod: Product20K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: field[K17](prod.p17) :: field[K18](prod.p18) :: field[K19](prod.p19) :: field[K20](prod.p20) :: HNil
        )
      )

    override def representable: RepresentableKC[
      Product20K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    ] =
      Product20K.product20KRepresentableTraverseInstance

    override def traverse: TraverseKC[
      Product20K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    ] =
      Product20K.product20KRepresentableTraverseInstance
  }

  implicit def product21FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
      K17 <: Symbol,
      K18 <: Symbol,
      K19 <: Symbol,
      K20 <: Symbol,
      K21 <: Symbol,
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
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: FieldType[
        K17,
        T17
      ] :: FieldType[K18, T18] :: FieldType[K19, T19] :: FieldType[K20, T20] :: FieldType[K21, T21] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16],
      k17: Witness.Aux[K17],
      k18: Witness.Aux[K18],
      k19: Witness.Aux[K19],
      k20: Witness.Aux[K20],
      k21: Witness.Aux[K21]
  ): HKDProductGeneric.Aux[Prod, Product21K[
    *[_],
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
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] =
      Product21K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]

    override def names: Product21K[
      Const[String, *],
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
    ] =
      Product21K[
        Const[String, *],
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
      ](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name,
        k17.value.name,
        k18.value.name,
        k19.value.name,
        k20.value.name,
        k21.value.name
      )

    override def to(a: Prod): Product21K[
      Id,
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
    ] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      val list17 = list16.tail
      val list18 = list17.tail
      val list19 = list18.tail
      val list20 = list19.tail
      val list21 = list20.tail
      Product21K[Id, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head,
        list17.head,
        list18.head,
        list19.head,
        list20.head,
        list21.head
      )
    }

    override def from(
        prod: Product21K[
          Id,
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
        ]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: field[K17](prod.p17) :: field[K18](prod.p18) :: field[K19](prod.p19) :: field[K20](prod.p20) :: field[
            K21
          ](prod.p21) :: HNil
        )
      )

    override def representable: RepresentableKC[
      Product21K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    ] =
      Product21K.product21KRepresentableTraverseInstance

    override def traverse: TraverseKC[
      Product21K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    ] =
      Product21K.product21KRepresentableTraverseInstance
  }

  implicit def product22FromShapeless[
      Prod,
      L <: HList,
      K1 <: Symbol,
      K2 <: Symbol,
      K3 <: Symbol,
      K4 <: Symbol,
      K5 <: Symbol,
      K6 <: Symbol,
      K7 <: Symbol,
      K8 <: Symbol,
      K9 <: Symbol,
      K10 <: Symbol,
      K11 <: Symbol,
      K12 <: Symbol,
      K13 <: Symbol,
      K14 <: Symbol,
      K15 <: Symbol,
      K16 <: Symbol,
      K17 <: Symbol,
      K18 <: Symbol,
      K19 <: Symbol,
      K20 <: Symbol,
      K21 <: Symbol,
      K22 <: Symbol,
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
  ](
      implicit gen: LabelledGeneric.Aux[Prod, L],
      ev: L =:= (FieldType[K1, T1] :: FieldType[K2, T2] :: FieldType[K3, T3] :: FieldType[K4, T4] :: FieldType[K5, T5] :: FieldType[
        K6,
        T6
      ] :: FieldType[K7, T7] :: FieldType[K8, T8] :: FieldType[K9, T9] :: FieldType[K10, T10] :: FieldType[K11, T11] :: FieldType[
        K12,
        T12
      ] :: FieldType[K13, T13] :: FieldType[K14, T14] :: FieldType[K15, T15] :: FieldType[K16, T16] :: FieldType[
        K17,
        T17
      ] :: FieldType[K18, T18] :: FieldType[K19, T19] :: FieldType[K20, T20] :: FieldType[K21, T21] :: FieldType[
        K22,
        T22
      ] :: HNil),
      k1: Witness.Aux[K1],
      k2: Witness.Aux[K2],
      k3: Witness.Aux[K3],
      k4: Witness.Aux[K4],
      k5: Witness.Aux[K5],
      k6: Witness.Aux[K6],
      k7: Witness.Aux[K7],
      k8: Witness.Aux[K8],
      k9: Witness.Aux[K9],
      k10: Witness.Aux[K10],
      k11: Witness.Aux[K11],
      k12: Witness.Aux[K12],
      k13: Witness.Aux[K13],
      k14: Witness.Aux[K14],
      k15: Witness.Aux[K15],
      k16: Witness.Aux[K16],
      k17: Witness.Aux[K17],
      k18: Witness.Aux[K18],
      k19: Witness.Aux[K19],
      k20: Witness.Aux[K20],
      k21: Witness.Aux[K21],
      k22: Witness.Aux[K22]
  ): HKDProductGeneric.Aux[Prod, Product22K[
    *[_],
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
  ]] = new HKDProductGeneric[Prod] {
    private val ev2 = ev.flip

    override type Gen[A[_]] =
      Product22K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]

    override def names: Product22K[
      Const[String, *],
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
    ] =
      Product22K[
        Const[String, *],
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
      ](
        k1.value.name,
        k2.value.name,
        k3.value.name,
        k4.value.name,
        k5.value.name,
        k6.value.name,
        k7.value.name,
        k8.value.name,
        k9.value.name,
        k10.value.name,
        k11.value.name,
        k12.value.name,
        k13.value.name,
        k14.value.name,
        k15.value.name,
        k16.value.name,
        k17.value.name,
        k18.value.name,
        k19.value.name,
        k20.value.name,
        k21.value.name,
        k22.value.name
      )

    override def to(a: Prod): Product22K[
      Id,
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
    ] = {
      val hlist  = gen.to(a)
      val list1  = hlist
      val list2  = list1.tail
      val list3  = list2.tail
      val list4  = list3.tail
      val list5  = list4.tail
      val list6  = list5.tail
      val list7  = list6.tail
      val list8  = list7.tail
      val list9  = list8.tail
      val list10 = list9.tail
      val list11 = list10.tail
      val list12 = list11.tail
      val list13 = list12.tail
      val list14 = list13.tail
      val list15 = list14.tail
      val list16 = list15.tail
      val list17 = list16.tail
      val list18 = list17.tail
      val list19 = list18.tail
      val list20 = list19.tail
      val list21 = list20.tail
      val list22 = list21.tail
      Product22K[
        Id,
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
      ](
        list1.head,
        list2.head,
        list3.head,
        list4.head,
        list5.head,
        list6.head,
        list7.head,
        list8.head,
        list9.head,
        list10.head,
        list11.head,
        list12.head,
        list13.head,
        list14.head,
        list15.head,
        list16.head,
        list17.head,
        list18.head,
        list19.head,
        list20.head,
        list21.head,
        list22.head
      )
    }

    override def from(
        prod: Product22K[
          Id,
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
        ]
    ): Prod =
      gen.from(
        ev2(
          field[K1](prod.p1) :: field[K2](prod.p2) :: field[K3](prod.p3) :: field[K4](prod.p4) :: field[K5](prod.p5) :: field[
            K6
          ](prod.p6) :: field[K7](prod.p7) :: field[K8](prod.p8) :: field[K9](prod.p9) :: field[K10](prod.p10) :: field[
            K11
          ](prod.p11) :: field[K12](prod.p12) :: field[K13](prod.p13) :: field[K14](prod.p14) :: field[K15](prod.p15) :: field[
            K16
          ](prod.p16) :: field[K17](prod.p17) :: field[K18](prod.p18) :: field[K19](prod.p19) :: field[K20](prod.p20) :: field[
            K21
          ](prod.p21) :: field[K22](prod.p22) :: HNil
        )
      )

    override def representable: RepresentableKC[Product22K[
      *[_],
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
    ]] =
      Product22K.product22KRepresentableTraverseInstance

    override def traverse: TraverseKC[Product22K[
      *[_],
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
    ]] =
      Product22K.product22KRepresentableTraverseInstance
  }
}

object Generate extends App {

  (1 to 22).foreach { i =>
    val keyTypes = (1 to i).map(i => s"K$i")
    val types    = (1 to i).map(i => s"T$i")
    val typesStr = types.mkString(", ")

    println(
      s"""|  implicit def product${i}FromShapeless[Prod, L <: HList, ${keyTypes
           .map(k => s"$k <: Symbol")
           .mkString(", ")}, $typesStr](
          |    implicit gen: LabelledGeneric.Aux[Prod, L],
          |    ev: L =:= (${keyTypes.zip(types).map { case (k, t) => s"FieldType[$k, $t]" }.mkString(" :: ")} :: HNil),
          |    ${keyTypes.map(k => s"${k.toLowerCase}: Witness.Aux[$k]").mkString(", ")}
          |  ): HKDProductGeneric.Aux[Prod, Product${i}K[*[_], $typesStr]] = new HKDProductGeneric[Prod] {
          |    private val ev2 = ev.flip
          |
          |    override type Gen[A[_]] = Product${i}K[A, $typesStr]
          |
          |    override def names: Product${i}K[Const[String, *], $typesStr] = Product${i}K[Const[String, *], $typesStr](
          |      ${keyTypes.map(k => s"${k.toLowerCase}.value.name").mkString(", ")}
          |    )
          |
          |    override def to(a: Prod): Product${i}K[Id, $typesStr] = {
          |      val hlist = gen.to(a)
          |      ${(1 to i)
           .map(i => s"val list$i = ${if (i == 1) "hlist" else s"list${i - 1}.tail"}")
           .mkString("\n      ")}
          |      Product${i}K[Id, $typesStr](
          |        ${(1 to i).map(i => s"list$i.head").mkString(", ")}
          |      )
          |    }
          |
          |    override def from(prod: Product${i}K[Id, $typesStr]): Prod =
          |      gen.from(
          |        ev2(${keyTypes.zip(1 to i).map { case (k, i) => s"field[$k](prod.p$i)" }.mkString(" :: ")} :: HNil)
          |      )
          |
          |    override def representable: RepresentableKC[Product${i}K[*[_], $typesStr]] =
          |      Product${i}K.product${i}KRepresentableTraverseInstance
          |
          |    override def traverse: TraverseKC[Product${i}K[*[_], $typesStr]] =
          |      Product${i}K.product${i}KRepresentableTraverseInstance
          |  }
          |
          |""".stripMargin
    )
  }
}
