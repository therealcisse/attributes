package com.github.attributes

import java.util.Arrays
import scala.collection.mutable.Map as MutableMap

abstract class ImmutableKeyValuePairs[K](private val data: Array[Any]) {
  private var hashcode: Int = 0

  final def size: Int = data.length / 2

  final def isEmpty: Boolean = data.length == 0

  final def asMap: Map[K, Any] = {
    val result = MutableMap.empty[K, Any]
    var i      = 0
    while (i < data.length) {
      result += (data(i).asInstanceOf[K] -> data(i + 1))
      i += 2
    }
    result.toMap
  }

  final def get[V](key: K): Option[V] = {
    var i = 0
    while (i < data.length) {
      if (key == data(i)) {
        return Some(data(i + 1).asInstanceOf[V])
      }
      i += 2
    }
    None
  }

  final def forEach(consumer: (K, Any) => Unit): Unit = {
    var i = 0
    while (i < data.length) {
      consumer(data(i).asInstanceOf[K], data(i + 1))
      i += 2
    }
  }

  override def equals(o: Any): Boolean = {
    if (this eq o.asInstanceOf[AnyRef]) {
      true
    } else if (!o.isInstanceOf[ImmutableKeyValuePairs[?]]) {
      false
    } else {
      val that = o.asInstanceOf[ImmutableKeyValuePairs[?]]
      Arrays.equals(this.data, that.data)
    }
  }

  override def hashCode(): Int = {
    var result = hashcode
    if (result == 0) {
      result = 1
      result *= 1000003
      result ^= Arrays.hashCode(data)
      hashcode = result
    }
    result
  }

  override def toString: String = {
    val sb = new StringBuilder("{")
    var i  = 0
    while (i < data.length) {
      val value = data(i + 1)
      val valueStr =
        value match {
          case s: String => "\"" + s + "\""
          case _         => value.toString
        }
      sb.append(data(i)).append("=").append(valueStr).append(", ")
      i += 2
    }
    if (sb.length > 1) {
      sb.setLength(sb.length - 2)
    }
    sb.append("}")
    sb.toString
  }

  def getData: Array[Any] = data
}

object ImmutableKeyValuePairs {

  def sortAndFilter[K](data: Array[Any])(
    implicit ord: Ordering[K]
  ): Array[Any] = {
    require(data.length % 2 == 0, "You must provide an even number of key/value pair arguments.")

    if (data.length == 0) {
      data
    } else {
      mergeSort(data, ord)
      dedupe(data, ord)
    }
  }

  private def mergeSort[K](data: Array[Any], ord: Ordering[K]): Unit = {
    val workArray = new Array[Any](data.length)
    System.arraycopy(data, 0, workArray, 0, data.length)
    splitAndMerge(workArray, 0, data.length, data, ord)
  }

  private def splitAndMerge[K](
    workArray: Array[Any],
    beginIndex: Int,
    endIndex: Int,
    targetArray: Array[Any],
    ord: Ordering[K]
  ): Unit = {
    if (endIndex - beginIndex <= 2) {
      return
    }
    val midpoint = ((endIndex + beginIndex) / 4) * 2
    splitAndMerge(targetArray, beginIndex, midpoint, workArray, ord)
    splitAndMerge(targetArray, midpoint, endIndex, workArray, ord)
    merge(workArray, beginIndex, midpoint, endIndex, targetArray, ord)
  }

  private def merge[K](
    sourceArray: Array[Any],
    beginIndex: Int,
    middleIndex: Int,
    endIndex: Int,
    targetArray: Array[Any],
    ord: Ordering[K]
  ): Unit = {
    var leftKeyIndex  = beginIndex
    var rightKeyIndex = middleIndex

    var k = beginIndex
    while (k < endIndex) {
      if (
        leftKeyIndex < middleIndex - 1 &&
        (rightKeyIndex >= endIndex - 1 ||
          ord.compare(sourceArray(leftKeyIndex).asInstanceOf[K], sourceArray(rightKeyIndex).asInstanceOf[K]) <= 0)
      ) {
        targetArray(k) = sourceArray(leftKeyIndex)
        targetArray(k + 1) = sourceArray(leftKeyIndex + 1)
        leftKeyIndex = leftKeyIndex + 2
      } else {
        targetArray(k) = sourceArray(rightKeyIndex)
        targetArray(k + 1) = sourceArray(rightKeyIndex + 1)
        rightKeyIndex = rightKeyIndex + 2
      }
      k += 2
    }
  }

  private def dedupe[K](data: Array[Any], ord: Ordering[K]): Array[Any] = {
    var previousKey: Any = null
    var size             = 0

    var i = 0
    while (i < data.length) {
      val key   = data(i)
      val value = data(i + 1)

      if (previousKey != null && ord.compare(key.asInstanceOf[K], previousKey.asInstanceOf[K]) == 0) {
        size -= 2
      }

      previousKey = key
      data(size) = key
      data(size + 1) = value
      size += 2

      i += 2
    }

    if (data.length != size) {
      val result = new Array[Any](size)
      System.arraycopy(data, 0, result, 0, size)
      result
    } else {
      data
    }
  }

  def apply[K](data: Array[Any])(
    implicit ord: Ordering[K]
  ) = new ImmutableKeyValuePairs[K](ImmutableKeyValuePairs.sortAndFilter(data)) {}

}
