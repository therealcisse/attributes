package com.github.attributes

import zio.schema.{DeriveSchema, Schema}

enum AttributeType {
  case STRING
  case BOOLEAN
  case LONG
  case DOUBLE
  case STRING_ARRAY
  case BOOLEAN_ARRAY
  case LONG_ARRAY
  case DOUBLE_ARRAY
}

object AttributeType {
  implicit val schema: Schema[AttributeType] = DeriveSchema.gen
}
