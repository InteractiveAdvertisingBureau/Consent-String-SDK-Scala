package com.iab.gdpr


trait VendorConsentSection {

  def encodingType: EncodingType

  def isVendorAllowed(vendorId: Int): Boolean

}
