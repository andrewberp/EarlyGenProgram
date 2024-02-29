PROGRAM CaesarCipher
  IMPLICIT NONE

  CHARACTER(LEN=100) :: originalText, encryptedText, decryptedText
  INTEGER :: shiftAmount

  ! Set original text and shift amount
  originalText = 'MINIME PLEASE DONT ENCRYPT ME'
  shiftAmount = 3

  ! Encrypt the text
  CALL Encrypt(originalText, shiftAmount, encryptedText)
  PRINT *, 'Encrypted Text: ', encryptedText

  ! Decrypt the text
  CALL Decrypt(encryptedText, shiftAmount, decryptedText)
  PRINT *, 'Decrypted Text: ', decryptedText

  ! Solve the cipher
  PRINT *, 'Solving Cipher...'
  CALL Solve(encryptedText, 26)

CONTAINS

  SUBROUTINE Encrypt(text, shift, encrypted)
    CHARACTER(LEN=*), INTENT(IN) :: text
    INTEGER, INTENT(IN) :: shift
    CHARACTER(LEN=100), INTENT(OUT) :: encrypted
    INTEGER :: i

    DO i = 1, LEN_TRIM(text)
      IF (text(i:i) >= 'a' .AND. text(i:i) <= 'z') THEN
        encrypted(i:i) = CHAR(MOD(ICHAR(UPCASE(text(i:i))) - ICHAR('A') + shift, 26) + ICHAR('A'))
      ELSE IF (text(i:i) >= 'A' .AND. text(i:i) <= 'Z') THEN
        encrypted(i:i) = CHAR(MOD(ICHAR(text(i:i)) - ICHAR('A') + shift, 26) + ICHAR('A'))
      ELSE
        encrypted(i:i) = text(i:i)
      END IF
    END DO
  END SUBROUTINE Encrypt

  SUBROUTINE Decrypt(encrypted, shift, decrypted)
    CHARACTER(LEN=*), INTENT(IN) :: encrypted
    INTEGER, INTENT(IN) :: shift
    CHARACTER(LEN=100), INTENT(OUT) :: decrypted
    INTEGER :: i

    DO i = 1, LEN_TRIM(encrypted)
      IF (encrypted(i:i) >= 'A' .AND. encrypted(i:i) <= 'Z') THEN
        decrypted(i:i) = CHAR(MOD(ICHAR(encrypted(i:i)) - ICHAR('A') - shift + 26, 26) + ICHAR('A'))
      ELSE
        decrypted(i:i) = encrypted(i:i)
      END IF
    END DO
  END SUBROUTINE Decrypt

  SUBROUTINE Solve(encrypted, maxShift)
    CHARACTER(LEN=*), INTENT(IN) :: encrypted
    INTEGER, INTENT(IN) :: maxShift
    CHARACTER(LEN=100) :: decrypted
    INTEGER :: shift

    DO shift = 1, maxShift
      CALL Decrypt(encrypted, shift, decrypted)
      PRINT *, 'Caesar ', shift, ': ', decrypted
    END DO
  END SUBROUTINE Solve

  FUNCTION UPCASE(ch) RESULT(upperCh)
    CHARACTER, INTENT(IN) :: ch
    CHARACTER :: upperCh

    IF (ch >= 'a' .AND. ch <= 'z') THEN
      upperCh = CHAR(ICHAR(ch) - 32)
    ELSE
      upperCh = ch
    END IF
  END FUNCTION UPCASE

END PROGRAM CaesarCipher
