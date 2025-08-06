package com.github.attributes

import zio.test.*

object AttributesSpec extends ZIOSpecDefault {

  def spec =
    suite("Attributes")(
      suite("creation")(
        test("should create empty attributes") {
          val attrs = Attributes.empty
          assertTrue(attrs.isEmpty && attrs.size == 0)
        },
        test("should create single attribute with of()") {
          val attrs = Attributes.of("key1", "value1")
          assertTrue(
            attrs.size == 1 &&
              attrs.get(AttributeKey[String]("key1")).contains("value1")
          )
        },
        test("should create attributes with apply()") {
          val key1  = AttributeKey[String]("key1")
          val key2  = AttributeKey[Long]("key2")
          val attrs = Attributes(key1 -> "value1", key2 -> 42L)
          assertTrue(
            attrs.size == 2 &&
              attrs.get(key1).contains("value1") &&
              attrs.get(key2).contains(42L)
          )
        },
        test("should handle empty apply()") {
          val attrs = Attributes()
          assertTrue(attrs.isEmpty && attrs.size == 0)
        }
      ),
      suite("retrieval")(
        test("should get values by correct keys") {
          val stringKey = AttributeKey[String]("string")
          val longKey   = AttributeKey[Long]("long")
          val boolKey   = AttributeKey[Boolean]("bool")
          val doubleKey = AttributeKey[Double]("double")

          val attrs = Attributes(
            stringKey -> "test",
            longKey   -> 123L,
            boolKey   -> true,
            doubleKey -> 3.14
          )

          assertTrue(
            attrs.get(stringKey).contains("test") &&
              attrs.get(longKey).contains(123L) &&
              attrs.get(boolKey).contains(true) &&
              attrs.get(doubleKey).contains(3.14)
          )
        },
        test("should return None for non-existent keys") {
          val attrs          = Attributes.of("existing", "value")
          val nonExistentKey = AttributeKey[String]("nonexistent")
          assertTrue(attrs.get(nonExistentKey).isEmpty)
        },
        test("should handle array types") {
          val stringArrayKey = AttributeKey[List[String]]("strings")
          val longArrayKey   = AttributeKey[List[Long]]("longs")

          val attrs = Attributes(
            stringArrayKey -> List("a", "b", "c"),
            longArrayKey   -> List(1L, 2L, 3L)
          )

          assertTrue(
            attrs.get(stringArrayKey).contains(List("a", "b", "c")) &&
              attrs.get(longArrayKey).contains(List(1L, 2L, 3L))
          )
        }
      ),
      suite("immutability and add")(
        test("should return new instance when adding") {
          val original = Attributes.of("key1", "value1")
          val modified = original.add("key2", "value2")

          assertTrue(
            original.size == 1 &&
              modified.size == 2 &&
              original.get(AttributeKey[String]("key2")).isEmpty &&
              modified.get(AttributeKey[String]("key2")).contains("value2")
          )
        },
        test("should preserve existing values when adding") {
          val original = Attributes.of("existing", "value")
          val modified = original.add("new", "newvalue")

          assertTrue(
            modified.get(AttributeKey[String]("existing")).contains("value") &&
              modified.get(AttributeKey[String]("new")).contains("newvalue")
          )
        },
        test("should handle adding different types") {
          val attrs = Attributes.empty.add("string", "text").add("long", 42L).add("boolean", true).add("double", 3.14)

          assertTrue(
            attrs.size == 4 &&
              attrs.get(AttributeKey[String]("string")).contains("text") &&
              attrs.get(AttributeKey[Long]("long")).contains(42L) &&
              attrs.get(AttributeKey[Boolean]("boolean")).contains(true) &&
              attrs.get(AttributeKey[Double]("double")).contains(3.14)
          )
        }
      ),
      suite("asMap functionality")(
        test("should convert to map correctly") {
          val stringKey = AttributeKey[String]("string")
          val longKey   = AttributeKey[Long]("long")
          val attrs     = Attributes(stringKey -> "value", longKey -> 42L)

          val map = attrs.asMap
          assertTrue(
            map.size == 2 &&
              map(stringKey) == "value" &&
              map(longKey) == 42L
          )
        },
        test("should return empty map for empty attributes") {
          val attrs = Attributes.empty
          assertTrue(attrs.asMap.isEmpty)
        }
      ),
      suite("serialization and deserialization")(
        test("should serialize and deserialize simple attributes") {
          val original     = Attributes.of("test", "value")
          val bytes        = Attributes.toBytes(original)
          val deserialized = Attributes.fromBytes(bytes)

          assertTrue(
            deserialized.isRight &&
              deserialized.right.get.get(AttributeKey[String]("test")).contains("value")
          )
        },
        test("should serialize and deserialize complex attributes") {
          val stringKey      = AttributeKey[String]("string")
          val longKey        = AttributeKey[Long]("long")
          val boolKey        = AttributeKey[Boolean]("bool")
          val doubleKey      = AttributeKey[Double]("double")
          val stringArrayKey = AttributeKey[List[String]]("strings")

          val original = Attributes(
            stringKey      -> "test",
            longKey        -> 123L,
            boolKey        -> true,
            doubleKey      -> 3.14,
            stringArrayKey -> List("a", "b", "c")
          )

          val bytes        = Attributes.toBytes(original)
          val deserialized = Attributes.fromBytes(bytes)

          assertTrue(
            deserialized.isRight &&
              deserialized.right.get.size == 5 &&
              deserialized.right.get.get(stringKey).contains("test") &&
              deserialized.right.get.get(longKey).contains(123L) &&
              deserialized.right.get.get(boolKey).contains(true) &&
              deserialized.right.get.get(doubleKey).contains(3.14) &&
              deserialized.right.get.get(stringArrayKey).contains(List("a", "b", "c"))
          )
        },
        test("should handle empty attributes serialization") {
          val original     = Attributes.empty
          val bytes        = Attributes.toBytes(original)
          val deserialized = Attributes.fromBytes(bytes)

          assertTrue(
            deserialized.isRight &&
              deserialized.right.get.isEmpty
          )
        },
        test("should handle invalid bytes") {
          val invalidBytes = Array[Byte](1, 2, 3, 4, 5)
          val result       = Attributes.fromBytes(invalidBytes)
          assertTrue(result.isLeft)
        }
      ),
      suite("property-based testing")(
        test("size should be consistent with number of unique keys") {
          check(Gen.listOfN(20)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { pairs =>
            val distinctPairs = pairs.distinctBy(_._1)
            var attrs         = Attributes.empty
            distinctPairs.foreach { case (k, v) => attrs = attrs.add(k, v) }
            assertTrue(attrs.size == distinctPairs.length)
          }
        },
        test("get should retrieve added values") {
          check(Gen.listOfN(10)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { pairs =>
            var attrs = Attributes.empty
            pairs.foreach { case (k, v) => attrs = attrs.add(k, v) }

            pairs.forall { case (k, v) => attrs.get(AttributeKey[String](k)).contains(v) }
          }
        },
        test("serialization round-trip should preserve data") {
          check(Gen.listOfN(15)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { pairs =>
            var attrs = Attributes.empty
            pairs.foreach { case (k, v) => attrs = attrs.add(k, v) }

            val bytes        = Attributes.toBytes(attrs)
            val deserialized = Attributes.fromBytes(bytes)

            deserialized.isRight && {
              val deserializedAttrs = deserialized.right.get
              pairs.forall { case (k, v) => deserializedAttrs.get(AttributeKey[String](k)).contains(v) }
            }
          }
        },
        test("asMap should be consistent with get") {
          check(Gen.listOfN(10)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { pairs =>
            var attrs = Attributes.empty
            pairs.foreach { case (k, v) => attrs = attrs.add(k, v) }

            val map = attrs.asMap
            pairs.forall { case (k, v) =>
              val key = AttributeKey[String](k)
              attrs.get(key) == map.get(key)
            }
          }
        },
        test("adding should maintain immutability") {
          check(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString)) { case (k, v) =>
            val original     = Attributes.of("existing", "value")
            val originalSize = original.size
            val modified     = original.add(k, v)

            // Original should be unchanged
            original.size == originalSize &&
            original.get(AttributeKey[String](k)).isEmpty &&
            // Modified should have the new value
            modified.get(AttributeKey[String](k)).contains(v)
          }
        }
      ),
      suite("edge cases")(
        test("should handle empty string keys") {
          val attrs = Attributes.of("", "empty-key-value")
          assertTrue(attrs.get(AttributeKey[String]("")).contains("empty-key-value"))
        },
        test("should handle special characters in keys") {
          val specialKey = "key-with-special-chars!@#$%^&*()"
          val attrs      = Attributes.of(specialKey, "special-value")
          assertTrue(attrs.get(AttributeKey[String](specialKey)).contains("special-value"))
        },
        test("should handle large number of attributes") {
          val largeAttrs =
            (1 to 1000).foldLeft(Attributes.empty) { (acc, i) =>
              acc.add(s"key$i", s"value$i")
            }

          assertTrue(
            largeAttrs.size == 1000 &&
              largeAttrs.get(AttributeKey[String]("key500")).contains("value500") &&
              largeAttrs.get(AttributeKey[String]("key1")).contains("value1") &&
              largeAttrs.get(AttributeKey[String]("key1000")).contains("value1000")
          )
        },
        test("should handle all supported array types") {
          val attrs = Attributes
            .empty
            .add("strings", List("a", "b", "c"))
            .add("booleans", List(true, false, true))
            .add("longs", List(1L, 2L, 3L))
            .add("doubles", List(1.1, 2.2, 3.3))

          assertTrue(
            attrs.size == 4 &&
              attrs.get(AttributeKey[List[String]]("strings")).contains(List("a", "b", "c")) &&
              attrs.get(AttributeKey[List[Boolean]]("booleans")).contains(List(true, false, true)) &&
              attrs.get(AttributeKey[List[Long]]("longs")).contains(List(1L, 2L, 3L)) &&
              attrs.get(AttributeKey[List[Double]]("doubles")).contains(List(1.1, 2.2, 3.3))
          )
        }
      )
    )

}
