package com.iab.gdpr


object EncodingType {
  case object BitFieldStyle extends EncodingType(0)
  case object RangeEntryStyle extends EncodingType(1)
}

sealed abstract class EncodingType(val value: Int)
