      ******************************************************************
      * Author:
      * Date:
      * Purpose: Criar um programa que valida a entrada de dados do
      *  usuário.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA-DADOS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM                  PIC 9(03).
       01 FLAG-VALIDA          PIC 9(01) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY 'DIGITE UM NUMERO DE 1 A 100'
           PERFORM ACEITA-DADO
           PERFORM VALIDA-DADO UNTIL FLAG-VALIDA = 1
            STOP RUN.

       ACEITA-DADO.
           ACCEPT NUM.

       VALIDA-DADO.
           IF NUM >= 1 AND <=100
               DISPLAY 'NUMERO ESTA ENTRE 1 E 100'
               MOVE 1 TO FLAG-VALIDA
           ELSE
               MOVE 0 TO FLAG-VALIDA
               DISPLAY 'NUMERO NAO ESTA NA LISTA'
               DISPLAY 'DIGITE NOVAMENTE UM NUMERO DE 1 A 100'
               PERFORM ACEITA-DADO
           END-IF.

       END PROGRAM VALIDA-DADOS.
