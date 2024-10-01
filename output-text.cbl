      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OUTPUT-TEXT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-NOME      PIC A(20).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'ESCREVA SEU NOME'.
            ACCEPT WS-NOME.
            DISPLAY WS-NOME.
            DISPLAY "Hello world".
            STOP RUN.
       END PROGRAM OUTPUT-TEXT.
