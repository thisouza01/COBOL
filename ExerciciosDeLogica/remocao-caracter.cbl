      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMOCAO-CARACTER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUMERO-TEL          PIC X(13).
       01  NUMERO-LIMPO        PIC X(13).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE '99-99999-9999' TO NUMERO-TEL.
           PERFORM REMOVE-CARACTER.
           DISPLAY NUMERO-LIMPO.

            STOP RUN.

       REMOVE-CARACTER.
           INSPECT NUMERO-TEL REPLACING ALL '-' BY ' '.
           MOVE NUMERO-TEL TO NUMERO-LIMPO.

       END PROGRAM REMOCAO-CARACTER.
