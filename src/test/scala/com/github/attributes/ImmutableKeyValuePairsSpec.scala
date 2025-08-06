package com.github.attributes

import zio.test.*

object ImmutableKeyValuePairsSpec extends ZIOSpecDefault {

  private case class TestKey(name: String)
  private implicit val testKeyOrdering: Ordering[TestKey] = Ordering.by(_.name)

  private class TestKeyValuePairs(data: Array[Any]) extends ImmutableKeyValuePairs[TestKey](data)

  def spec =
    suite("ImmutableKeyValuePairs")(
      suite("basic functionality")(
        test("empty pairs should have size 0") {
          val pairs = new TestKeyValuePairs(Array.empty)
          assertTrue(pairs.size == 0 && pairs.isEmpty)
        },
        test("should return correct size for non-empty pairs") {
          val pairs = new TestKeyValuePairs(Array(TestKey("a"), "value1", TestKey("b"), "value2"))
          assertTrue(pairs.size == 2 && !pairs.isEmpty)
        },
        test("should get values by key") {
          val pairs = new TestKeyValuePairs(Array(TestKey("a"), "value1", TestKey("b"), "value2"))
          assertTrue(
            pairs.get[String](TestKey("a")).contains("value1") &&
              pairs.get[String](TestKey("b")).contains("value2") &&
              pairs.get[String](TestKey("c")).isEmpty
          )
        },
        test("should convert to map correctly") {
          val key1  = TestKey("a")
          val key2  = TestKey("b")
          val pairs = new TestKeyValuePairs(Array(key1, "value1", key2, "value2"))
          val map   = pairs.asMap
          assertTrue(
            map.size == 2 &&
              map(key1) == "value1" &&
              map(key2) == "value2"
          )
        }
      ),
      suite("forEach functionality")(
        test("should iterate through all pairs") {
          val key1  = TestKey("a")
          val key2  = TestKey("b")
          val pairs = new TestKeyValuePairs(Array(key1, "value1", key2, "value2"))

          var collected = List.empty[(TestKey, Any)]
          pairs.forEach { (k, v) => collected = (k, v) :: collected }

          assertTrue(collected.reverse == List((key1, "value1"), (key2, "value2")))
        },
        test("should handle empty pairs in forEach") {
          val pairs = new TestKeyValuePairs(Array.empty)
          var count = 0
          pairs.forEach((_, _) => count += 1)
          assertTrue(count == 0)
        }
      ),
      suite("equality and hashCode")(
        test("should be equal if data arrays are equal") {
          val data   = Array(TestKey("a"), "value1", TestKey("b"), "value2")
          val pairs1 = new TestKeyValuePairs(data.clone())
          val pairs2 = new TestKeyValuePairs(data.clone())
          assertTrue(pairs1 == pairs2 && pairs1.hashCode == pairs2.hashCode)
        },
        test("should not be equal if data differs") {
          val pairs1 = new TestKeyValuePairs(Array(TestKey("a"), "value1"))
          val pairs2 = new TestKeyValuePairs(Array(TestKey("a"), "value2"))
          assertTrue(pairs1 != pairs2)
        },
        test("should have consistent hashCode") {
          val pairs = new TestKeyValuePairs(Array(TestKey("a"), "value1"))
          val hash1 = pairs.hashCode
          val hash2 = pairs.hashCode
          assertTrue(hash1 == hash2)
        }
      ),
      suite("toString functionality")(
        test("should format empty pairs correctly") {
          val pairs = new TestKeyValuePairs(Array.empty)
          assertTrue(pairs.toString == "{}")
        },
        test("should format single pair correctly") {
          val pairs = new TestKeyValuePairs(Array(TestKey("a"), "value1"))
          assertTrue(pairs.toString == "{TestKey(a)=\"value1\"}")
        },
        test("should format multiple pairs correctly") {
          val pairs = new TestKeyValuePairs(Array(TestKey("a"), "value1", TestKey("b"), 42))
          assertTrue(pairs.toString == "{TestKey(a)=\"value1\", TestKey(b)=42}")
        },
        test("should handle non-string values in toString") {
          val pairs = new TestKeyValuePairs(Array(TestKey("num"), 42, TestKey("bool"), true))
          assertTrue(pairs.toString.contains("TestKey(num)=42") && pairs.toString.contains("TestKey(bool)=true"))
        }
      ),
      suite("sortAndFilter functionality")(
        test("should handle empty array") {
          val result = ImmutableKeyValuePairs.sortAndFilter[TestKey](Array.empty)
          assertTrue(result.length == 0)
        },
        test("should sort pairs by key") {
          val unsorted = Array(TestKey("c"), "value3", TestKey("a"), "value1", TestKey("b"), "value2")
          val sorted   = ImmutableKeyValuePairs.sortAndFilter[TestKey](unsorted)
          val pairs    = new TestKeyValuePairs(sorted)
          val keys     = pairs.asMap.keys.toList.sorted
          assertTrue(keys == List(TestKey("a"), TestKey("b"), TestKey("c")))
        },
        test("should deduplicate keys, keeping last value") {
          val withDupes = Array(TestKey("a"), "first", TestKey("b"), "value2", TestKey("a"), "last")
          val deduped   = ImmutableKeyValuePairs.sortAndFilter[TestKey](withDupes)
          val pairs     = new TestKeyValuePairs(deduped)
          assertTrue(
            pairs.size == 2 &&
              pairs.get[String](TestKey("a")).contains("last") &&
              pairs.get[String](TestKey("b")).contains("value2")
          )
        },
        test("should require even number of arguments") {
          val oddArray = Array(TestKey("a"))
          val result = TestUtils.unsafeRun(
            ZIO.attempt(ImmutableKeyValuePairs.sortAndFilter[TestKey](oddArray)).exit
          )
          assertTrue(result.isFailure)
        }
      ),
      suite("property-based testing")(
        test("get should be consistent with asMap") {
          check(Gen.listOfN(20)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { keyValueList =>
            val data  = keyValueList.flatMap { case (k, v) => List(TestKey(k), v) }.toArray
            val pairs = new TestKeyValuePairs(ImmutableKeyValuePairs.sortAndFilter(data))
            val map   = pairs.asMap

            keyValueList.forall { case (k, _) => pairs.get[String](TestKey(k)) == map.get(TestKey(k)) }
          }
        },
        test("size should equal half of data length after sortAndFilter") {
          check(Gen.listOfN(10)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { keyValueList =>
            val data   = keyValueList.flatMap { case (k, v) => List(TestKey(k), v) }.toArray
            val sorted = ImmutableKeyValuePairs.sortAndFilter(data)
            val pairs  = new TestKeyValuePairs(sorted)
            assertTrue(pairs.size * 2 == sorted.length)
          }
        },
        test("forEach should visit all key-value pairs") {
          check(Gen.listOfN(15)(Gen.zip(Gen.alphaNumericString, Gen.alphaNumericString))) { keyValueList =>
            val data  = keyValueList.flatMap { case (k, v) => List(TestKey(k), v) }.toArray
            val pairs = new TestKeyValuePairs(ImmutableKeyValuePairs.sortAndFilter(data))

            var visited = Set.empty[(TestKey, String)]
            pairs.forEach((k, v) => visited += ((k, v.asInstanceOf[String])))

            assertTrue(visited.size == pairs.size)
          }
        }
      ),
      suite("edge cases")(
        test("should handle duplicate consecutive keys correctly") {
          val data   = Array(TestKey("a"), "first", TestKey("a"), "second", TestKey("a"), "third")
          val sorted = ImmutableKeyValuePairs.sortAndFilter(data)
          val pairs  = new TestKeyValuePairs(sorted)
          assertTrue(pairs.size == 1 && pairs.get[String](TestKey("a")).contains("third"))
        },
        test("should handle large datasets") {
          val largeData = (1 to 1000).flatMap(i => List(TestKey(s"key$i"), s"value$i")).toArray
          val pairs     = new TestKeyValuePairs(ImmutableKeyValuePairs.sortAndFilter(largeData))
          assertTrue(
            pairs.size == 1000 &&
              pairs.get[String](TestKey("key500")).contains("value500") &&
              pairs.get[String](TestKey("key1")).contains("value1") &&
              pairs.get[String](TestKey("key1000")).contains("value1000")
          )
        }
      )
    )

}
