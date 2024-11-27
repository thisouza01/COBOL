      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REDEFINES_TIPO_COMB.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  DATA-ATUAL          PIC 9(08).

       01  DATA-REDEFINIDA REDEFINES DATA-ATUAL.
           05 ANO              PIC 9(04).
           05 MES              PIC 9(02).
           05 DIA              PIC 9(02).
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM PEGA-DIA-ATUAL.
           PERFORM MOSTRA-DATA-REDEFINIDA.

           STOP RUN.

       PEGA-DIA-ATUAL.
           MOVE FUNCTION CURRENT-DATE(1:8) TO DATA-ATUAL.

       MOSTRA-DATA-REDEFINIDA.
           DISPLAY DATA-REDEFINIDA.
           DISPLAY '***********'.
           DISPLAY 'ANO: 'ANO.
           DISPLAY '------'
           DISPLAY 'MES: 'MES.
           DISPLAY '------'
           DISPLAY 'DIA: 'DIA.

       END PROGRAM REDEFINES_TIPO_COMB.
