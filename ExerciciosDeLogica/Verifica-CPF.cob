      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Verifica-CPF.
      *================================================================*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WS-CPF.
           03 WS-CPF1        PIC 9(03) VALUE ZEROS.
           03 WS-CPF2        PIC 9(03) VALUE ZEROS.
           03 WS-CPF3        PIC 9(03) VALUE ZEROS.
           03 WS-DIGITOS     PIC 9(02) VALUE ZEROS.

       01 WS-CPF-MASK.
           03 WS-CPF1       PIC 9(03).
           03 FILLER         PIC X VALUE '.'.
           03 WS-CPF2       PIC 9(03).
           03 FILLER         PIC X VALUE '.'.
           03 WS-CPF3       PIC 9(03).
           03 FILLER         PIC X VALUE '-'.
           03 WS-DIGITOS    PIC 9(02).
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIAPAL.
           PERFORM 1-INICIAR.
           PERFORM 2-PROCESSAR.
           PERFORM 3-FINALIZAR.
           STOP RUN.
      *----------------------------------------------------------------*

       1-INICIAR.
           DISPLAY 'DIGITE O CPF (SOMENTE EM NUMEROS): '
           ACCEPT WS-CPF.
           IF WS-CPF NOT NUMERIC
               DISPLAY 'CPF INVALIDO!'
               DISPLAY 'DIGITE NOVAMENTE O CPF (SOMENTE EM NUMEROS): '
               ACCEPT WS-CPF
           END-IF.
      *----------------------------------------------------------------*

       2-PROCESSAR.
           PERFORM 21-CONVERTE-CPF.
      *----------------------------------------------------------------*

       3-FINALIZAR.
           DISPLAY 'CPF: ' WS-CPF-MASK.
      *----------------------------------------------------------------*

       21-CONVERTE-CPF.
           MOVE CORRESPONDING WS-CPF TO WS-CPF-MASK.

      *================================================================*
