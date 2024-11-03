       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGOCOB09.
      **************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTOR = THIAGO(SOUZA) - THIAGOS
      * OBJETIVO:
      *
      * DATA = 13/09/2023
      **************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOTA1   PIC 9(02)     VALUE ZEROS.
       77 WRK-NOTA2   PIC 9(02)     VALUE ZEROS.
       77 WRK-MEDIA   PIC 9(02)V9   VALUE ZEROS.
       PROCEDURE DIVISION.
       0001-PRINCIPAL.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR.
           ACCEPT WRK-NOTA1.
           ACCEPT WRK-NOTA2.

       0200-PROCESSAR.
           COMPUTE WRK-MEDIA = (WRK-NOTA1 + WRK-NOTA2) / 2.
           DISPLAY 'MEDIA.. ' WRK-MEDIA.
               IF WRK-MEDIA >= 6
                   DISPLAY 'APROVADO'
               ELSE
                   DISPLAY 'REPROVADO'
               END-IF.

       0300-FINALIZAR.
           DISPLAY '=========================='.
           DISPLAY 'FINAL DE PROCESSAMENTO'.
