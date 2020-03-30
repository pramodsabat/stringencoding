package view

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

/**
 * An object containing various implicit functions
 */
object Implicit {
  /**
   * Encodes a character
   * @param char
   * @return
   */
  implicit def encodeChar(char: Char) = EncodeChar(char)

  /**
   * Encodes a string
   * @param value
   * @return
   */
  implicit def encodeString(value: String) = EncodeString(value)
}

/**
 * This object contains all attributes of a Vowels
 */
object Vowels{
  val A = ('a' -> '1')
  val E = ('e' -> '2')
  val I = ('i' -> '3')
  val O = ('o' -> '4')
  val U = ('u' -> '5')

  /**
   * This function encodes a vowel
   * @param value
   * @return
   */
  def encode(value: Char): Char = {
    value match {
      case A._1 => A._2
      case E._1 => E._2
      case I._1 => I._2
      case O._1 => O._2
      case U._1 => U._2
      case _ => value
    }
  }

  /**
   * This function accepts an character and checks if it is vowel or not
   */
  val isVowel = (value: Char) => vowels.contains(value)

  /**
   * This function return all vowel
   */
  val vowels = collection.immutable.List(A._1, E._1, I._1, O._1, U._1)
}

/**
 * This class encodes a Character
 * @param value
 */
case class EncodeChar(value: Char) {
  val ALPHABET_REGX = "[a-zA-Z]"
  val Y_REGX = "[yY]"
  val Y = 'y'
  val SPACE = ' '

  /**
   * This function encodes a character with previous letter. For example : b -> a, c -> b, d -> c
   */
  val encodeAlphabet = (value: Char) => if (value.toString.matches(ALPHABET_REGX)) (value - 1).toChar else value

  /**
   * This function encodes all characters of a string except number with more then one digit
   * 1. vowels are replaced with number: a -> 1, e -> 2, i -> 3, o -> 4, and u -> 5
   * 2. consonants are replaced with previous letter: b -> a, c -> b, d -> c, etc.
   * 3. y is replaced with space
   * 4. space is replaced with y
   * @return
   */
  def encode: Char = {
    value match {
      case char if Vowels.isVowel(char) => Vowels.encode(char)
      case ' ' => Y
      case Y => SPACE
      case _ => encodeAlphabet(value)
    }
  }
}

/**
 * This object encodes the input string
 */
object Encode {
  /**
   * This function checks if the input is a string or Number
   */
  val isNumber = (value: String) => scala.util.Try(value.toInt) match { case scala.util.Success(x) => true case _ => false}

  /**
   * This functional reference returns the first number from the input string
   */
  val number = (value: String) => ("""\d+""".r findFirstIn value).mkString("")

  /**
   * This functional reference lowercase the input string
   */
  val lowercase = (value: String) => value.toLowerCase()

  /**
   * In this function, for a input string, numbers are replaced with their reverse: 1 -> 1, 23 -> 32, 1234 -> 4321
   * @param value
   * @return
   */
  def replaceNumbers(value: String): String = {
    isNumber(value) match {
      case true => value.reverse
      case false =>
        def replace(updatedValue: String, value: String, numberToReplace: String, length: Int): String = {
          if(length <= 1 || numberToReplace.isEmpty){
            updatedValue
          } else {
            val indexOfNumber = (value.indexOf(numberToReplace) + numberToReplace.length)
            val source = value.substring(0, indexOfNumber)
            val target = value.substring(0, value.indexOf(numberToReplace)) + numberToReplace.reverse
            val subString = value.substring(indexOfNumber, length)
            replace(updatedValue.replace(source, target), subString, number(subString), subString.length)
          }
        }
        replace(value, value, number(value), value.length)
    }
  }

  /**
   * This function accepts a string and return an encoded output
   * @param value
   * @return
   */
  def apply(value: String): String = {
    import Implicit.encodeChar
    replaceNumbers(lowercase(value)).map{ char => char.encode}
  }
}

/**
 * A class to encode the input string
 * @param value
 */
case class EncodeString(value: String) {
  def encode: String = {
    Encode.apply(value)
  }
}

import Implicit.encodeString
object Result {
  /*
   * Complete the 'encode' function below.
   *
   * The function is expected to return a STRING.
   * The function accepts STRING stringToEncode as parameter.
   */

  def encode(stringToEncode: String): String = {

    // Write your code here
    stringToEncode.encode
  }
}

object Solution {
  def main(args: Array[String]) {
    val stringToEncode = "Hello World"
    //val result = Result.encode(stringToEncode)
    val result = Result.encode(stringToEncode)
    println("Input - "+stringToEncode)
    println("Output - "+result)

  }
}
