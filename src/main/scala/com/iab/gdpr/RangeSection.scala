package com.iab.gdpr

import com.iab.gdpr.util.GdprConstants._
import com.iab.gdpr.EncodingType.RangeEntryStyle
import com.iab.gdpr.util.ConsentUtils

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class RangeSection(defaultConsent: Boolean, rangeEntries: List[RangeEntry]) extends VendorConsentSection {

  private val lookupMap: Map[Int, Boolean] = getLookupMap

  override def encodingType: EncodingType = RangeEntryStyle

  override def isVendorAllowed(vendorId: Int): Boolean = {
    lookupMap.getOrElse(vendorId, defaultConsent)
  }

  override def toString: String = {
    s"$rangeEntries"
  }

  private def getLookupMap: Map[Int, Boolean] = {
    val lookupMap: mutable.Map[Int, Boolean] = mutable.Map()
    rangeEntries.foreach { entry =>
      Range.inclusive(entry.minVendorId, entry.maxVendorId).foreach(lookupMap += _ -> !defaultConsent)
    }
    lookupMap.toMap
  }
}


object RangeSection {
  def apply(bytes: Array[Byte], encodingType: EncodingType): RangeSection = {

    val defaultConsent = ConsentUtils.getBit(bytes, DEFAULT_CONSENT_OFFSET) == 1

    val numEntries = ConsentUtils.getInt(bytes, NUM_ENTRIES_OFFSET, NUM_ENTRIES_SIZE)

    val rangeEntries: ListBuffer[RangeEntry] = new ListBuffer[RangeEntry]()

    var currentEntryOffset = RANGE_ENTRY_OFFSET

    for (_ <- 0 until numEntries) {
      val isSingleVendorId = ConsentUtils.getBit(bytes, currentEntryOffset) == 0
      currentEntryOffset += 1

      if (isSingleVendorId) {
        val vendorId = ConsentUtils.getInt(bytes, currentEntryOffset, VENDOR_ID_SIZE)
        rangeEntries += RangeEntry(vendorId, vendorId)
        currentEntryOffset += VENDOR_ID_SIZE
      } else {
        val startVendorId = ConsentUtils.getInt(bytes, currentEntryOffset, VENDOR_ID_SIZE)
        currentEntryOffset += VENDOR_ID_SIZE
        val endVendorId = ConsentUtils.getInt(bytes, currentEntryOffset, VENDOR_ID_SIZE)
        currentEntryOffset += VENDOR_ID_SIZE
        rangeEntries += RangeEntry(startVendorId, endVendorId)
      }
    }
    RangeSection(defaultConsent, rangeEntries.toList)
  }
}

case class RangeEntry(minVendorId: Int, maxVendorId: Int)
