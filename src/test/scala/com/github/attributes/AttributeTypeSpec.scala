package com.github.attributes

import zio.test._
import zio.schema.codec.JsonCodec

object AttributeTypeSpec extends ZIOSpecDefault {

  def spec = suite("AttributeType")(
    test("should have all expected enum values") {
      val allTypes = AttributeType.values.toSet
      val expectedTypes = Set(
        AttributeType.STRING,
        AttributeType.BOOLEAN,
        AttributeType.LONG,
        AttributeType.DOUBLE,
        AttributeType.STRING_ARRAY,
        AttributeType.BOOLEAN_ARRAY,
        AttributeType.LONG_ARRAY,
        AttributeType.DOUBLE_ARRAY
      )
      assertTrue(allTypes == expectedTypes)
    },

    test("should serialize and deserialize correctly with schema") {
      check(Gen.fromIterable(AttributeType.values)) { attrType =>
        val encoded = JsonCodec.schemaBasedBinaryCodec[AttributeType].encode(attrType)
        val decoded = JsonCodec.schemaBasedBinaryCodec[AttributeType].decode(encoded)
        assertTrue(decoded.isRight && decoded.right.get == attrType)
      }
    },

    test("should maintain enum ordering") {
      val types = AttributeType.values
      assertTrue(
        types.indexOf(AttributeType.STRING) < types.indexOf(AttributeType.BOOLEAN) &&
        types.indexOf(AttributeType.BOOLEAN) < types.indexOf(AttributeType.LONG) &&
        types.indexOf(AttributeType.LONG) < types.indexOf(AttributeType.DOUBLE)
      )
    }
  )
}
