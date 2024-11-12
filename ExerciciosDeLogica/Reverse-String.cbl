      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. reverse-string.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-STRING PIC X(64).
       PROCEDURE DIVISION.
       REVERSE-STRING.
         MOVE 'COBOL' TO WS-STRING.
         MOVE FUNCTION REVERSE(FUNCTION TRIM (WS-STRING) ) TO WS-STRING.
         DISPLAY WS-STRING.
