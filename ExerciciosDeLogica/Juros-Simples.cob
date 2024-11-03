      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Juros-Simples.
      *================================================================*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WS-VALOR       PIC 9(05)V99 VALUE ZEROS.
       01 WS-TAXA-JUROS  PIC 9(03)    VALUE ZEROS.
       01 WS-TEMPO       PIC 9(02)    VALUE ZEROS.
       01 WS-RESULT      PIC 9(06)    VALUE ZEROS.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.
           PERFORM 1-INICIO.
           PERFORM 2-PROCESSAR.
           PERFORM 3-FINALIZAR.
           STOP RUN.

       1-INICIO.
           DISPLAY 'DIGITE O VALOR: '
           ACCEPT WS-VALOR.

           DISPLAY 'A TAXA DE JUROS: (%) '
           ACCEPT WS-TAXA-JUROS.

           DISPLAY 'QUANTO TEMPO (MESES): '
           ACCEPT WS-TEMPO.

       2-PROCESSAR.

           COMPUTE WS-RESULT =
               WS-VALOR * (WS-TAXA-JUROS/100) * WS-TEMPO.

       3-FINALIZAR.
           DISPLAY 'O JUROS SERA: '
           DISPLAY WS-RESULT.

      *================================================================*
