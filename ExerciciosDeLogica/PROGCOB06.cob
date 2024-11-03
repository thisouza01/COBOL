       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB06.
      **************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTOR = THIAGO(SOUZA) - THIAGOS
      * OBJETIVO: OPERADORES ARITIMETICOS
      * DATA = XX/XX/XXXX
      **************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NUM1    PIC 9(2) VALUE ZEROS.
       77 WRK-NUM2    PIC 9(2) VALUE ZEROS.
       77 WRK-RESULT  PIC 9(4) VALUE ZEROS.
       77 WRK-RESTO   PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
           ACCEPT WRK-NUM1 FROM CONSOLE.
           ACCEPT WRK-NUM2 FROM CONSOLE.
           DISPLAY 'NUMERO 1: ' WRK-NUM1.
           DISPLAY 'NUMERO 2: ' WRK-NUM2.

      **********         SOMA            *********

           ADD WRK-NUM1 WRK-NUM2 TO WRK-RESULT.
            DISPLAY 'SOMA: ' WRK-RESULT.

      **********       SUBTRACAO         *********

           SUBTRACT WRK-NUM1 FROM WRK-NUM2 GIVING WRK-RESULT.
            DISPLAY 'SUBTRACAO: ' WRK-RESULT.

      **********       DIVISAO         *********

           DIVIDE WRK-NUM1 BY WRK-NUM2 GIVING WRK-RESULT
             REMAINDER WRK-RESTO.
            DISPLAY 'DIVISAO: ' WRK-RESULT
            DISPLAY 'RESTO: '   WRK-RESTO.

      **********       MULTIPLICACAO         *********

           MULTIPLY WRK-NUM1 BY WRK-NUM2 GIVING WRK-RESULT.
            DISPLAY 'MULTIPLICACAO: ' WRK-RESULT.

      **********         MEDIA              ************

           COMPUTE WRK-RESULT = (WRK-NUM1 + WRK-NUM2) / 2
           DISPLAY 'MEDIA: ' WRK-RESULT.

           STOP RUN.
