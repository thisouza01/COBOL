      ******************************************************************
      * Author:
      * Date:
      * Purpose: Utilize a instrução INSPECT para contar a quantidade
      *  de ocorrências de um caractere específico em uma string
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTAGEM-SIMPLES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTE-STRING         PIC A(11) VALUE 'COBOL e TOP'.
       01 RESULT               PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM CONTA-CARACTER.
           DISPLAY "TEM "RESULT" LETRAS 'O'".

           STOP RUN.

       CONTA-CARACTER.
           INSPECT TESTE-STRING TALLYING RESULT FOR ALL 'O'.

       END PROGRAM CONTAGEM-SIMPLES.
