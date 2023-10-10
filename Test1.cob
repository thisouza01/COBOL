      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST1.
      *----------------------------------------------------------------*
      *OBJETIVO: CALCULAR A ÁREA DO RETANGULO
      *================================================================*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WR-BASE    PIC 9(03)   VALUE ZEROS.
       01 WR-ALTURA  PIC 9(03)   VALUE ZEROS.
       01 WR-AREA    PIC 9(04)   VALUE ZEROS.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.
           PERFORM 1-INICIA.
           PERFORM 2-PROCESSA.
           PERFORM 3-FINALIZA.

       1-INICIA.
           DISPLAY 'Coloque a base do retangulo'
           ACCEPT WR-BASE.

           DISPLAY 'Coloque a altura do retangulo'
           ACCEPT WR-ALTURA.

       2-PROCESSA.
           COMPUTE WR-AREA = WR-BASE * WR-ALTURA.

       3-FINALIZA.
           DISPLAY 'AREA DO RETANGULO: ' WR-AREA.
           STOP RUN.
      *================================================================*
