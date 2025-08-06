package com.github.attributes

import zio.schema.DeriveSchema
import zio.schema.Schema

case class AttributeKey[T](key: String, typ: AttributeType)

object AttributeKey {

  def apply[T](key: String)(
    implicit evidence: ForAttributeType[T]
  ): AttributeKey[T] = AttributeKey(key, evidence.attributeType)

  implicit def schema[T](
    implicit schemaT: Schema[T]
  ): Schema[AttributeKey[T]] = DeriveSchema.gen

  implicit val ord: Ordering[AttributeKey[?]] = Ordering.by(_.key)

  trait ForAttributeType[T] {
    def attributeType: AttributeType
  }

  object ForAttributeType {

    implicit val stringType: ForAttributeType[String] =
      new ForAttributeType[String] {
        def attributeType: AttributeType = AttributeType.STRING
      }

    implicit val booleanType: ForAttributeType[Boolean] =
      new ForAttributeType[Boolean] {
        def attributeType: AttributeType = AttributeType.BOOLEAN
      }

    implicit val longType: ForAttributeType[Long] =
      new ForAttributeType[Long] {
        def attributeType: AttributeType = AttributeType.LONG
      }

    implicit val doubleType: ForAttributeType[Double] =
      new ForAttributeType[Double] {
        def attributeType: AttributeType = AttributeType.DOUBLE
      }

    implicit val stringArrayType: ForAttributeType[List[String]] =
      new ForAttributeType[List[String]] {
        def attributeType: AttributeType = AttributeType.STRING_ARRAY
      }

    implicit val booleanArrayType: ForAttributeType[List[Boolean]] =
      new ForAttributeType[List[Boolean]] {
        def attributeType: AttributeType = AttributeType.BOOLEAN_ARRAY
      }

    implicit val longArrayType: ForAttributeType[List[Long]] =
      new ForAttributeType[List[Long]] {
        def attributeType: AttributeType = AttributeType.LONG_ARRAY
      }

    implicit val doubleArrayType: ForAttributeType[List[Double]] =
      new ForAttributeType[List[Double]] {
        def attributeType: AttributeType = AttributeType.DOUBLE_ARRAY
      }

  }

}
