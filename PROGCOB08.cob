       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB07.
      **************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTOR = THIAGO(SOUZA) - THIAGOS
      * OBJETIVO: RECEBER LARGURA E COMPRIMENTO
      * DATA = XX/XX/XXXX
      **************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-LARGURA        PIC 9(03)V99   VALUE ZEROS.
       77 WRK-COMPRIMENTO    PIC 9(03)V99   VALUE ZEROS.
       77 WRK-AREA           PIC 9(03)V99   VALUE ZEROS.
       PROCEDURE DIVISION.
           DISPLAY 'LARGURA.. '
           ACCEPT WRK-LARGURA.

           DISPLAY 'COMPRIMENTO.. '
           ACCEPT WRK-COMPRIMENTO.

           IF WRK-LARGURA > 0 AND WRK-COMPRIMENTO > 0
               COMPUTE WRK-AREA = (WRK-LARGURA * WRK-COMPRIMENTO)
               DISPLAY 'AREA.. ' WRK-AREA
           ELSE
               DISPLAY 'ALGUMA INFORMACAO ERRADA'
           END-IF.
           STOP RUN.
