package com.iab.gdpr.util

import java.util.Base64


object Implicits {
  /**
    * Base64 support for strings
    * @param str string
    */
  implicit class Base64Ops(val str: String) extends AnyVal {
    def isUrlSafe: Boolean = !str.contains("+") && !str.contains("/")

    def decodeBase64: Array[Byte] = if (isUrlSafe) Base64.getUrlDecoder.decode(str) else Base64.getDecoder.decode(str)

    def decodeBase64AsString: String = new String(decodeBase64, "UTF-8")
  }
}
