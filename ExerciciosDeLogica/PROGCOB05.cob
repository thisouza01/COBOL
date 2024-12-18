       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB05.
      **************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTOR = THIAGO(SOUZA) - THIAGOS
      * OBJETIVO: RECEBER CPF
      * FORMATAR E IMPRIMIR
      * DATA = XX/XX/XXXX
      **************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NAME    PIC X(20) VALUE SPACES.
       77 WRK-CPF     PIC X(11) VALUE SPACES.
       77 WRK-CPF-ED  PIC ZZZ.ZZZ.ZZ9/99 VALUE ZEROS.
       PROCEDURE DIVISION.
           ACCEPT WRK-NAME FROM CONSOLE.
           ACCEPT WRK-CPF FROM CONSOLE.
      ********** MOSTRA DADOS **********
           DISPLAY WRK-NAME.
           MOVE WRK-CPF TO WRK-CPF-ED.
           DISPLAY WRK-CPF-ED.
