      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTAGEM-CARACTER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  AUX.
           05 PALAVRA              PIC A(30) VALUE SPACES.
           05 TAMANHO-PALAVRA      PIC 9(03) VALUE ZEROS.
           05 RESULTADO-CARACTER   PIC 9(03) VALUE ZEROS.
           05 RESULTADO-SEM-ESPACO PIC 9(03) VALUE ZEROS.
           05 RESULTADO-FINAL      PIC 9(03) VALUE ZEROS.
           05 CONTADOR             PIC 9(01) VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ACEITA-PALAVRA
           PERFORM CONTA-CARACTER
            STOP RUN.

       ACEITA-PALAVRA.
           DISPLAY 'Digite uma palavra'.
           ACCEPT  PALAVRA.

       CONTA-CARACTER.
      * RETIRO APENAS A PALVRA SEM OS ESPAÇOS EM BRANCO
           INSPECT PALAVRA TALLYING RESULTADO-CARACTER
               FOR CHARACTERS BEFORE INITIAL ' '.
           DISPLAY 'Tem 'RESULTADO-CARACTER' caracteres'.

       END PROGRAM CONTAGEM-CARACTER.
