package com.iab.gdpr.util

import java.text.ParseException
import GdprConstants.{SIX_BITS, BITS_PER_BYTE}


object ConsentUtils {

  private val bytePows = Array(128.toByte, 64.toByte, 32.toByte, 16.toByte, 8.toByte, 4.toByte, 2.toByte, 1.toByte)

  @throws[java.text.ParseException]
  private[gdpr] def getBit(bytes: Array[Byte], index: Int): Int = {
    val byteIndex = index / BITS_PER_BYTE
    if (byteIndex < 0 || byteIndex >= bytes.length) {
      throw new ParseException(s"Index is out of range!", index)
    } else {
      if ((bytes(byteIndex) & bytePows(index % BITS_PER_BYTE)) != 0) 1 else 0
    }
  }

  @throws[java.text.ParseException]
  private[gdpr] def getInt(bytes: Array[Byte], offset: Int, size: Int): Int = {
    if (size > java.lang.Integer.SIZE) {
      throw new ParseException("Can't fit bit range in int.", offset)
    } else {
      var value = 0
      for (i <- 0 until size) {
        if (getBit(bytes, offset + i) == 1) {
          value += (1 << (size - (i + 1)))
        }
      }
      value
    }
  }

  @throws[java.text.ParseException]
  private[gdpr] def getLong(bytes: Array[Byte], offset: Int, size: Int): Long = {
    if (size > java.lang.Long.SIZE) {
      throw new ParseException("Can't fit bit range in long.", offset)
    } else {
      var value: Long = 0L
      for (i <- 0 until size) {
        if (getBit(bytes, offset + i) == 1) {
          value += (1L << (size - (i + 1)))
        }
      }
      value
    }
  }

  @throws[java.text.ParseException]
  private[gdpr] def getConsentLanguage(bytes: Array[Byte], offset: Int, size: Int): String = {
    if (size % SIX_BITS != 0) {
      throw new ParseException("String bit length must be multiple of six", offset)
    } else {
      val charNum = size / SIX_BITS
      val sb = new StringBuilder(charNum)
      for (i <- 0 until charNum) {
        sb.append((getInt(bytes, offset + (i * SIX_BITS), SIX_BITS) + 65).asInstanceOf[Char])
      }
      sb.toString().toUpperCase
    }
  }
}
