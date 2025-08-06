package com.github.attributes

import zio.schema.codec.JsonCodec
import zio.test.*

object AttributeKeySpec extends ZIOSpecDefault {

  def spec =
    suite("AttributeKey")(
      suite("type inference and implicit resolution")(
        test("should infer String type correctly") {
          val key = AttributeKey[String]("test-string")
          assertTrue(key.typ == AttributeType.STRING)
        },
        test("should infer Boolean type correctly") {
          val key = AttributeKey[Boolean]("test-boolean")
          assertTrue(key.typ == AttributeType.BOOLEAN)
        },
        test("should infer Long type correctly") {
          val key = AttributeKey[Long]("test-long")
          assertTrue(key.typ == AttributeType.LONG)
        },
        test("should infer Double type correctly") {
          val key = AttributeKey[Double]("test-double")
          assertTrue(key.typ == AttributeType.DOUBLE)
        },
        test("should infer List[String] type correctly") {
          val key = AttributeKey[List[String]]("test-string-array")
          assertTrue(key.typ == AttributeType.STRING_ARRAY)
        },
        test("should infer List[Boolean] type correctly") {
          val key = AttributeKey[List[Boolean]]("test-boolean-array")
          assertTrue(key.typ == AttributeType.BOOLEAN_ARRAY)
        },
        test("should infer List[Long] type correctly") {
          val key = AttributeKey[List[Long]]("test-long-array")
          assertTrue(key.typ == AttributeType.LONG_ARRAY)
        },
        test("should infer List[Double] type correctly") {
          val key = AttributeKey[List[Double]]("test-double-array")
          assertTrue(key.typ == AttributeType.DOUBLE_ARRAY)
        }
      ),
      suite("ForAttributeType instances")(
        test("should have correct ForAttributeType instances") {
          import AttributeKey.ForAttributeType.*
          assertTrue(
            stringType.attributeType == AttributeType.STRING &&
              booleanType.attributeType == AttributeType.BOOLEAN &&
              longType.attributeType == AttributeType.LONG &&
              doubleType.attributeType == AttributeType.DOUBLE &&
              stringArrayType.attributeType == AttributeType.STRING_ARRAY &&
              booleanArrayType.attributeType == AttributeType.BOOLEAN_ARRAY &&
              longArrayType.attributeType == AttributeType.LONG_ARRAY &&
              doubleArrayType.attributeType == AttributeType.DOUBLE_ARRAY
          )
        }
      ),
      suite("ordering")(
        test("should order keys by key string") {
          val key1 = AttributeKey[String]("a")
          val key2 = AttributeKey[String]("b")
          val key3 = AttributeKey[Long]("c")

          val sorted = List(key3, key1, key2).sorted
          assertTrue(sorted == List(key1, key2, key3))
        },
        test("should handle ordering with property-based testing") {
          check(Gen.listOfN(10)(Gen.alphaNumericString)) { strings =>
            val keys          = strings.map(s => AttributeKey[String](s))
            val sorted        = keys.sorted
            val expectedOrder = strings.sorted
            assertTrue(sorted.map(_.key) == expectedOrder)
          }
        }
      ),
      suite("schema generation")(
        test("should generate valid schema for AttributeKey[String]") {
          val key     = AttributeKey[String]("test")
          val encoded = JsonCodec.schemaBasedBinaryCodec[AttributeKey[String]].encode(key)
          val decoded = JsonCodec.schemaBasedBinaryCodec[AttributeKey[String]].decode(encoded)
          assertTrue(decoded.isRight && decoded.right.get == key)
        },
        test("should generate valid schema for different types") {
          check(Gen.alphaNumericString) { keyName =>
            val stringKey  = AttributeKey[String](keyName)
            val longKey    = AttributeKey[Long](keyName)
            val booleanKey = AttributeKey[Boolean](keyName)

            val stringEncoded  = JsonCodec.schemaBasedBinaryCodec[AttributeKey[String]].encode(stringKey)
            val longEncoded    = JsonCodec.schemaBasedBinaryCodec[AttributeKey[Long]].encode(longKey)
            val booleanEncoded = JsonCodec.schemaBasedBinaryCodec[AttributeKey[Boolean]].encode(booleanKey)

            val stringDecoded  = JsonCodec.schemaBasedBinaryCodec[AttributeKey[String]].decode(stringEncoded)
            val longDecoded    = JsonCodec.schemaBasedBinaryCodec[AttributeKey[Long]].decode(longEncoded)
            val booleanDecoded = JsonCodec.schemaBasedBinaryCodec[AttributeKey[Boolean]].decode(booleanEncoded)

            assertTrue(
              stringDecoded.isRight && stringDecoded.right.get == stringKey &&
                longDecoded.isRight && longDecoded.right.get == longKey &&
                booleanDecoded.isRight && booleanDecoded.right.get == booleanKey
            )
          }
        }
      ),
      suite("equality and hashcode")(
        test("should be equal if key and type are equal") {
          val key1 = AttributeKey[String]("test")
          val key2 = AttributeKey[String]("test")
          assertTrue(key1 == key2 && key1.hashCode == key2.hashCode)
        },
        test("should not be equal if key strings differ") {
          val key1 = AttributeKey[String]("test1")
          val key2 = AttributeKey[String]("test2")
          assertTrue(key1 != key2)
        },
        test("should not be equal if types differ") {
          val key1 = AttributeKey[String]("test")
          val key2 = AttributeKey[Long]("test")
          assertTrue(key1 != key2)
        }
      )
    )

}
