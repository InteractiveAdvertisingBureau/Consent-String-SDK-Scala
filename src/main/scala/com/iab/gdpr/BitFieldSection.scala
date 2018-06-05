package com.iab.gdpr

import com.iab.gdpr.EncodingType.BitFieldStyle
import com.iab.gdpr.util.ConsentUtils
import com.iab.gdpr.util.GdprConstants.{BITS_PER_BYTE, VENDOR_BITFIELD_OFFSET}

import scala.collection.mutable.ListBuffer


case class BitFieldSection(bitField: Array[Boolean]) extends VendorConsentSection {

  override def encodingType: EncodingType = BitFieldStyle

  override def isVendorAllowed(vendorId: Int): Boolean = {
    (vendorId > 0 && vendorId <= bitField.length) && bitField(vendorId - 1)
  }

  override def toString: String = {
    val bitFieldSb = new StringBuilder()
    for (index <- bitField.indices) {
      if (index != bitField.length - 1) {
        bitFieldSb.append(s"${index + 1} -> ${bitField(index)}, ")
      } else {
        bitFieldSb.append(s"${index + 1} -> ${bitField(index)}")
      }
    }
    bitFieldSb.toString()
  }
}


object BitFieldSection {
  def apply(bytes: Array[Byte], encodingType: EncodingType): BitFieldSection = {
    val bitField: ListBuffer[Boolean] = new ListBuffer[Boolean]()
    for (i <- VENDOR_BITFIELD_OFFSET until bytes.length * BITS_PER_BYTE) {
      bitField += ConsentUtils.getBit(bytes, i) == 1
    }
    BitFieldSection(bitField.toArray)
  }
}
