      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calculo-VPL.
      *================================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 TABELA-FLUXO-CAIXA.
           03 FLUXO-CAIXA OCCURS 5 TIMES.
               05 QNT-DINHEIRO PIC 9(10)V99 VALUE ZEROS.

       01 PERIODO              PIC 9(02) VALUE 1.
       01 TAXA-DESCONTO        PIC 9(05)V99.
       01 VPL-RESULT           PIC 9(10)V99 VALUE 0.

      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.
           PERFORM 1-INICIAR.
           PERFORM 2-PROCESSAR.
           PERFORM 3-FINALIZAR.
           STOP RUN.


       1-INICIAR.
           DISPLAY 'QUAL SUA TAXA DE DESCONTO POR ANO?'
           ACCEPT TAXA-DESCONTO.

       2-PROCESSAR.
           PERFORM 21-LER-FLUXO-CAIXA.
           PERFORM 22-CALCULA-VPL.

       3-FINALIZAR.
           DISPLAY 'O VPL E: ' VPL-RESULT.


       21-LER-FLUXO-CAIXA.
           DISPLAY 'DIGITE O VALOR DO FLUXO DE CAIXA NO PERIODO: '
               PERIODO.
           ACCEPT QNT-DINHEIRO(PERIODO).
           ADD 1 TO PERIODO.
           IF PERIODO <= 5
               PERFORM 21-LER-FLUXO-CAIXA
           END-IF.

       22-CALCULA-VPL.
           PERFORM VARYING PERIODO FROM 1 BY 1 UNTIL PERIODO > 5
               COMPUTE VPL-RESULT = VPL-RESULT + (QNT-DINHEIRO(PERIODO)
                   / (1 + TAXA-DESCONTO) ** PERIODO )
           END-PERFORM.
      *================================================================*
