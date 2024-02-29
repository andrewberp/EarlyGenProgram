object JDoodle extends App {

  // Function to convert a string to uppercase
  def toUpper(str: String): String = str.toUpperCase

  // Function to encrypt a string using Caesar Cipher
  def encrypt(str: String, shiftAmount: Int): String = {
    toUpper(str).map {
      case c if c.isLetter => ('A' + (c - 'A' + shiftAmount + 26) % 26).toChar
      case c => c
    }.mkString
  }

  // Function to decrypt a string using Caesar Cipher
  def decrypt(str: String, shiftAmount: Int): String = {
    toUpper(str).map {
      case c if c.isLetter => ('A' + (c - 'A' - shiftAmount + 26) % 26).toChar
      case c => c
    }.mkString
  }

  // Function to solve (brute-force) the Caesar Cipher
  def solve(str: String, maxShiftValue: Int): Unit = {
    (1 to maxShiftValue).foreach { shift =>
      println(s"Caesar $shift: ${decrypt(str, shift)}")
    }
  }

  // Example usage
  val originalText = "MINIME PLEASE DONT ENCRYPT ME"
  val encryptedText = encrypt(originalText, 3)
  println(s"Encrypted Text: $encryptedText")

  val decryptedText = decrypt(encryptedText, 3)
  println(s"Decrypted Text: $decryptedText")

  println("Solving Cipher...")
  solve(encryptedText, 26)
}
