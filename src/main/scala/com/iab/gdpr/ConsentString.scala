package com.iab.gdpr

import com.iab.gdpr.EncodingType.{BitFieldStyle, RangeEntryStyle}
import com.iab.gdpr.util.GdprConstants._
import com.iab.gdpr.util.ConsentUtils
import com.iab.gdpr.util.Implicits.Base64Ops

import scala.collection.mutable.ListBuffer


/**
  * GDPR Consent String
  *
  * Spec: https://github.com/InteractiveAdvertisingBureau/GDPR-Transparency-and-Consent-Framework
  *      (Consent string and vendor list formats v1.1 Final.md)
  */
case class ConsentString(rawConsent: String,
                         version: Int,
                         created: Long,
                         lastUpdated: Long,
                         cmpId: Int,
                         cmpVersion: Int,
                         consentScreen: Int,
                         consentLang: String,
                         vendorListVersion: Int,
                         purposesAllowed: List[Int],
                         maxVendorId: Int,
                         encodingType: EncodingType,
                         vendorConsentSection: VendorConsentSection) {

  /**
    * Check if the purpose is allowed
    *
    * @param purpose integer value, start from 1
    * @return true if it's allowed, otherwise false
    */
  def isPurposeAllowed(purpose: Int): Boolean = {
    purposesAllowed.contains(purpose)
  }

  /**
    * Check if the vendor has consent
    *
    * @param vendorId integer value
    * @return true if it's has consent, otherwise false
    *
    * Note: Available vendor list can be found in https://vendorlist.consensu.org/vendorlist.json
    */
  def isVendorAllowed(vendorId: Int): Boolean = {
    vendorConsentSection.isVendorAllowed(vendorId)
  }

  /**
    * Raw GDPR consent string in binary string format
    *
    * @return GDPR consent string in binary string format, e.g. "00000100110010...."
    */
  def getBinaryString: String = {
    val bytes = rawConsent.decodeBase64
    val size = bytes.length * BITS_PER_BYTE
    val sb = new StringBuilder(size)
    for (i <- 0 until size) {
      sb.append(ConsentUtils.getBit(bytes, i).toString)
    }
    sb.toString()
  }
}


object ConsentString {

  def apply(consent: String): ConsentString = {
    val bytes: Array[Byte] = consent.decodeBase64
    val encodingType = getEncodingType(bytes, ENCODING_TYPE_OFFSET, ENCODING_TYPE_SIZE)

    new ConsentString(
      rawConsent = consent,
      version = ConsentUtils.getInt(bytes, VERSION_BIT_OFFSET, VERSION_BIT_SIZE),
      created = ConsentUtils.getLong(bytes, CREATED_BIT_OFFSET, CREATED_BIT_SIZE),
      lastUpdated = ConsentUtils.getLong(bytes, UPDATED_BIT_OFFSET, UPDATED_BIT_SIZE),
      cmpId = ConsentUtils.getInt(bytes, CMP_ID_OFFSET, CMP_ID_SIZE),
      cmpVersion = ConsentUtils.getInt(bytes, CMP_VERSION_OFFSET, CMP_VERSION_SIZE),
      consentScreen = ConsentUtils.getInt(bytes, CONSENT_SCREEN_OFFSET, CONSENT_SCREEN_SIZE),
      consentLang = ConsentUtils.getConsentLanguage(bytes, CONSENT_LANGUAGE_OFFSET, CONSENT_LANGUAGE_SIZE),
      vendorListVersion = ConsentUtils.getInt(bytes, VENDOR_LIST_VERSION_OFFSET, VENDOR_LIST_VERSION_SIZE),
      purposesAllowed = getPurposesAllowed(bytes, PURPOSES_OFFSET, PURPOSES_SIZE),
      maxVendorId = ConsentUtils.getInt(bytes, MAX_VENDOR_ID_OFFSET, MAX_VENDOR_ID_SIZE),
      encodingType = encodingType,
      vendorConsentSection = getVendorConsentSection(bytes, encodingType)
    )
  }

  private def getPurposesAllowed(bytes: Array[Byte], offset: Int, size: Int): List[Int] = {
    val purposes: ListBuffer[Int] = new ListBuffer[Int]()
    for (i <- offset until offset + size) {
      if (ConsentUtils.getBit(bytes, i) == 1) {
        purposes += (i - offset + 1)
      }
    }
    purposes.toList
  }

  private def getEncodingType(bytes: Array[Byte], offset: Int, size: Int): EncodingType = {
    if (ConsentUtils.getInt(bytes, offset, size) == 0) BitFieldStyle else RangeEntryStyle
  }

  private def getVendorConsentSection(bytes: Array[Byte], encodingType: EncodingType): VendorConsentSection = {
    if (encodingType == EncodingType.BitFieldStyle) {
      BitFieldSection(bytes, encodingType)
    } else {
      RangeSection(bytes, encodingType)
    }
  }
}
