package com.github.attributes

trait Attributes {
  def get[T](key: AttributeKey[T]): Option[T]
  def size: Int
  def isEmpty: Boolean
  def asMap: Map[AttributeKey[?], Any]

  def add[T](key: String, value: T)(
    implicit evidence: AttributeKey.ForAttributeType[T]
  ): Attributes

}

object Attributes {

  private class AttributesImpl private[Attributes] (data: Array[Any]) extends Attributes {
    private val underlying = ImmutableKeyValuePairs[AttributeKey[?]](data)

    def get[T](key: AttributeKey[T]): Option[T] = underlying.get(key)
    def size: Int                               = underlying.size
    def isEmpty: Boolean                        = underlying.isEmpty
    def asMap: Map[AttributeKey[?], Any]        = underlying.asMap

    def add[T](key: String, value: T)(
      implicit evidence: AttributeKey.ForAttributeType[T]
    ): Attributes = {
      val attributeKey = AttributeKey[T](key)
      val data         = underlying.getData
      val newData      = new Array[Any](data.length + 2)
      System.arraycopy(data, 0, newData, 0, data.length)
      newData(data.length) = attributeKey
      newData(data.length + 1) = value
      new AttributesImpl(newData)
    }

  }

  def empty: Attributes = new AttributesImpl(Array.empty[Any])

  def of[T](key: String, value: T)(
    implicit evidence: AttributeKey.ForAttributeType[T]
  ): Attributes = empty.add(key, value)

  def apply(pairs: (AttributeKey[?], Any)*): Attributes = {
    if (pairs.isEmpty) {
      empty
    } else {
      val data = new Array[Any](pairs.length * 2)
      var i    = 0
      for ((key, value) <- pairs) {
        data(i) = key
        data(i + 1) = value
        i += 2
      }
      new AttributesImpl(data)
    }
  }

  def toBytes(attributes: Attributes): Array[Byte] = {
    val serializable = Attributes.Serialization.toSerializable(attributes)
    Attributes.Serialization.protobufCodec.encode(serializable).toArray
  }

  def fromBytes(bytes: Array[Byte]): Either[String, Attributes] = {
    Attributes
      .Serialization
      .protobufCodec
      .decode(zio.Chunk.fromArray(bytes))
      .map(Attributes.Serialization.fromSerializable)
      .left
      .map(_.getMessage)
  }

  private object Serialization {
    import zio.schema.DeriveSchema
    import zio.schema.Schema
    import zio.schema.codec.BinaryCodec
    import zio.schema.codec.JsonCodec
    import zio.schema.codec.ProtobufCodec

    case class SerializableAttribute(key: String, attributeType: AttributeType, value: AttributeValue)

    sealed trait AttributeValue
    case class StringValue(value: String)              extends AttributeValue
    case class BooleanValue(value: Boolean)            extends AttributeValue
    case class LongValue(value: Long)                  extends AttributeValue
    case class DoubleValue(value: Double)              extends AttributeValue
    case class StringArrayValue(value: List[String])   extends AttributeValue
    case class BooleanArrayValue(value: List[Boolean]) extends AttributeValue
    case class LongArrayValue(value: List[Long])       extends AttributeValue
    case class DoubleArrayValue(value: List[Double])   extends AttributeValue

    implicit val attributeValueSchema: Schema[AttributeValue]               = DeriveSchema.gen
    implicit val serializableAttributeSchema: Schema[SerializableAttribute] = DeriveSchema.gen
    implicit val attributesSchema: Schema[List[SerializableAttribute]]      = Schema.list[SerializableAttribute]

    private def valueToAttributeValue(value: Any, attributeType: AttributeType): AttributeValue =
      attributeType match {
        case AttributeType.STRING        => StringValue(value.asInstanceOf[String])
        case AttributeType.BOOLEAN       => BooleanValue(value.asInstanceOf[Boolean])
        case AttributeType.LONG          => LongValue(value.asInstanceOf[Long])
        case AttributeType.DOUBLE        => DoubleValue(value.asInstanceOf[Double])
        case AttributeType.STRING_ARRAY  => StringArrayValue(value.asInstanceOf[List[String]])
        case AttributeType.BOOLEAN_ARRAY => BooleanArrayValue(value.asInstanceOf[List[Boolean]])
        case AttributeType.LONG_ARRAY    => LongArrayValue(value.asInstanceOf[List[Long]])
        case AttributeType.DOUBLE_ARRAY  => DoubleArrayValue(value.asInstanceOf[List[Double]])
      }

    private def attributeValueToValue(attributeValue: AttributeValue): Any =
      attributeValue match {
        case StringValue(value)       => value
        case BooleanValue(value)      => value
        case LongValue(value)         => value
        case DoubleValue(value)       => value
        case StringArrayValue(value)  => value
        case BooleanArrayValue(value) => value
        case LongArrayValue(value)    => value
        case DoubleArrayValue(value)  => value
      }

    def toSerializable(attributes: Attributes): List[SerializableAttribute] = {
      attributes
        .asMap
        .map { case (key, value) => SerializableAttribute(key.key, key.typ, valueToAttributeValue(value, key.typ)) }
        .toList
    }

    def fromSerializable(serializable: List[SerializableAttribute]): Attributes = {
      val pairs = serializable.map { attr =>
        val key   = AttributeKey[Any](attr.key, attr.attributeType)
        val value = attributeValueToValue(attr.value)
        (key, value)
      }
      Attributes(pairs*)
    }

    val protobufCodec: BinaryCodec[List[SerializableAttribute]] = ProtobufCodec.protobufCodec(attributesSchema)
    val jsonCodec                                               = JsonCodec.schemaBasedBinaryCodec(attributesSchema)

  }

}
