      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERTE-MAIUSCULA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NOME                PIC X(14) VALUE 'cobol_programa'.
       01  NOME-MAIUSCULO      PIC X(14) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM CONVERTE-MAIUSCULA.
           DISPLAY NOME-MAIUSCULO.
            STOP RUN.

       CONVERTE-MAIUSCULA.
           MOVE FUNCTION UPPER-CASE(NOME) TO NOME-MAIUSCULO.

       END PROGRAM CONVERTE-MAIUSCULA.
