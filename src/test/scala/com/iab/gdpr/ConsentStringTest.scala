package com.iab.gdpr

import org.scalatest.FunSuite


class ConsentStringTest extends FunSuite {

  test("[BitField]") {

    val consentString = "BN5lERiOMYEdiAOAWeFRAAYAAaAAptQ"
    val consent: ConsentString = ConsentString(consentString)

    assert(consent.rawConsent === consentString)

    assert(consent.cmpId === 14)
    assert(consent.cmpVersion === 22)
    assert(consent.consentLang === "FR")
    assert(consent.created === 14924661858L)
    assert(consent.lastUpdated === 15240021858L)

    assert(consent.purposesAllowed.size === 5)
    assert(consent.isPurposeAllowed(2))
    assert(!consent.isPurposeAllowed(1))
    assert(consent.isPurposeAllowed(21))

    assert(consent.isVendorAllowed(1))
    assert(consent.isVendorAllowed(5))
    assert(consent.isVendorAllowed(7))
    assert(consent.isVendorAllowed(9))
    assert(!consent.isVendorAllowed(0))
    assert(!consent.isVendorAllowed(10))
  }

  test("[RangeEntry]") {
    val consentString = "BN5lERiOMYEdiAKAWXEND1HoSBE6CAFAApAMgBkIDIgM0AgOJxAnQA=="
    val consent: ConsentString = ConsentString(consentString)

    assert(consent.rawConsent === consentString)

    assert(consent.cmpId === 10)
    assert(consent.cmpVersion === 22)
    assert(consent.consentLang === "EN")
    assert(consent.created === 14924661858L)
    assert(consent.lastUpdated === 15240021858L)

    assert(consent.purposesAllowed.size === 8)
    assert(consent.isPurposeAllowed(4))
    assert(!consent.isPurposeAllowed(1))
    assert(consent.isPurposeAllowed(24))
    assert(!consent.isPurposeAllowed(25))
    assert(!consent.isPurposeAllowed(0))

    assert(!consent.isVendorAllowed(1))
    assert(!consent.isVendorAllowed(3))
    assert(consent.isVendorAllowed(225))
    assert(consent.isVendorAllowed(5000))
    assert(consent.isVendorAllowed(515))
    assert(!consent.isVendorAllowed(0))
    assert(!consent.isVendorAllowed(3244))
  }

  test("[RangeEntry 2] Parse GDPR consent string into ConsentString") {
    val consentString = "BONZt-1ONZt-1AHABBENAO-AAAAHCAEAASABmADYAOAAeA"
    val consent: ConsentString = ConsentString(consentString)

    assert(consent.purposesAllowed.size === 5)
    assert(consent.isPurposeAllowed(1))
    assert(consent.isPurposeAllowed(3))

    assert(consent.isVendorAllowed(28))
    assert(!consent.isVendorAllowed(1))
    assert(!consent.isVendorAllowed(3))
    assert(consent.isVendorAllowed(27))
  }

  test("[RangeEntry 3] Parse GDPR consent") {
    val consent: ConsentString = ConsentString("BONxUu4ONxi5LAOABCENARuAAAAHpAMAeQiQQoTBcFRlVABIIg")

    assert(consent.version === 1)

    assert(consent.purposesAllowed.size === 4)
    assert(consent.isPurposeAllowed(1))
    assert(!consent.isPurposeAllowed(2))
    assert(consent.isPurposeAllowed(3))

    assert(!consent.isVendorAllowed(51))
    assert(consent.isVendorAllowed(70))
    assert(!consent.isVendorAllowed(2))
    assert(consent.isVendorAllowed(1))
  }

  test("[Invalid consent]") {
    val consentString = "INVALID"
    intercept[java.text.ParseException] {
      ConsentString(consentString)
    }
  }

  test("[Parsing performance]") {
    val executionTimes = 10000

    val start = System.nanoTime
    for (_ <- Range(0, executionTimes / 2)) {
      ConsentString("BONZt-1ONZt-1AHABBENAO-AAAAHCAEAASABmADYAOAAeA")
      ConsentString("BN5lERiOMYEdiAKAWXEND1HoSBE6CAFAApAMgBkIDIgM0AgOJxAnQA==")
    }
    val end = System.nanoTime

    assert((end - start) / executionTimes < 1000000,
      "Average time to parse 1 GDPR consent string should take less than 1ms.")
  }
}
