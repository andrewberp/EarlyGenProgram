IDENTIFICATION DIVISION.
PROGRAM-ID. caesar-encrypt-decrypt.

ENVIRONMENT DIVISION.

DATA DIVISION.

WORKING-STORAGE SECTION.
01 inputString PIC X(99) VALUE "Eoh no Mini Me has zapped me".
01 encryptedString PIC X(99).
01 decryptedString PIC X(99).
01 solvedString PIC X(99).
01 numShift PIC 99 VALUE 3.
01 i PIC 9(3) VALUE 1.
01 c PIC X(1).
01 asciiVal PIC 9(3).
01 shiftedAsciiVal PIC 9(3).
01 j PIC 99.

PROCEDURE DIVISION.
    PERFORM EncryptString
    PERFORM DecryptString
    PERFORM SolveCaesarCipher
    STOP RUN.

EncryptString.
    MOVE FUNCTION UPPER-CASE(inputString) TO inputString
    
    IF numShift IS GREATER THAN OR EQUAL TO 26
        MOVE FUNCTION MOD(numShift, 26) TO numShift
    END-IF
    
    PERFORM VARYING i FROM 1 BY 1 UNTIL i IS GREATER THAN FUNCTION LENGTH(inputString)
        IF inputString(i:1) IS NOT EQUAL TO SPACE
            MOVE inputString(i:1) TO c
            COMPUTE asciiVal = FUNCTION ORD(c)
            COMPUTE shiftedAsciiVal = asciiVal + numShift
            IF shiftedAsciiVal IS LESS THAN OR EQUAL TO FUNCTION ORD("Z")
                MOVE FUNCTION CHAR(shiftedAsciiVal) TO encryptedString(i:1)
            ELSE
                COMPUTE shiftedAsciiVal = FUNCTION MOD(shiftedAsciiVal - FUNCTION ORD("A"), 26) + FUNCTION ORD("A")
                MOVE FUNCTION CHAR(shiftedAsciiVal) TO encryptedString(i:1)
            END-IF
        ELSE
            MOVE SPACE TO encryptedString(i:1)
        END-IF
    END-PERFORM
    DISPLAY "Encrypted String: " encryptedString.

DecryptString.
    MOVE encryptedString TO inputString
    COMPUTE numShift = 26 - numShift
    
    PERFORM VARYING i FROM 1 BY 1 UNTIL i IS GREATER THAN FUNCTION LENGTH(inputString)
        IF inputString(i:1) IS NOT EQUAL TO SPACE
            MOVE inputString(i:1) TO c
            COMPUTE asciiVal = FUNCTION ORD(c)
            COMPUTE shiftedAsciiVal = asciiVal + numShift
            IF shiftedAsciiVal IS GREATER THAN FUNCTION ORD("Z")
                COMPUTE shiftedAsciiVal = FUNCTION MOD(shiftedAsciiVal - FUNCTION ORD("A"), 26) + FUNCTION ORD("A")
            END-IF
            MOVE FUNCTION CHAR(shiftedAsciiVal) TO decryptedString(i:1)
        ELSE
            MOVE SPACE TO decryptedString(i:1)
        END-IF
    END-PERFORM
    DISPLAY "Decrypted String: " decryptedString.
    
SolveCaesarCipher.
    MOVE encryptedString TO inputString
    
    PERFORM VARYING j FROM 1 BY 1 UNTIL j IS GREATER THAN 25
        PERFORM VARYING i FROM 1 BY 1 UNTIL i IS GREATER THAN FUNCTION LENGTH(inputString)
            IF inputString(i:1) IS NOT EQUAL TO SPACE
                MOVE inputString(i:1) TO c
                COMPUTE asciiVal = FUNCTION ORD(c)
                COMPUTE shiftedAsciiVal = asciiVal - j
                IF shiftedAsciiVal IS LESS THAN FUNCTION ORD("A")
                    COMPUTE shiftedAsciiVal = shiftedAsciiVal + 26
                END-IF
                MOVE FUNCTION CHAR(shiftedAsciiVal) TO solvedString(i:1)
            ELSE
                MOVE SPACE TO solvedString(i:1)
            END-IF
        END-PERFORM
        DISPLAY "Shift " j " : " solvedString
    END-PERFORM.
