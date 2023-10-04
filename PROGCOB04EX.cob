       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCOB04EX.
      **************************************
      * AREA DE COMENTARIOS - REMARKS
      * AUTOR = THIAGO(SOUZA) - THIAGOS
      * OBJETIVO:
      *
      * DATA = 13/09/2023
      **************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME              PIC X(20)     VALUE SPACES.
       77 WRK-ANOS_TRABALHADOS  PIC 9(02)     VALUE ZEROS.
       77 WRK-SALARIO           PIC 9(04)V99  VALUE ZEROS.
       77 WRK-SALARIO-ED        PIC $ZZZZ,99  VALUE ZEROS.
       PROCEDURE DIVISION.
       0001-PRINCIPAL.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR.
           DISPLAY 'NOME: '.
           ACCEPT WRK-NOME.
           DISPLAY 'ANOS TRABALHADOS: '.
           ACCEPT WRK-ANOS_TRABALHADOS.
           DISPLAY 'SALARIO: '.
           ACCEPT WRK-SALARIO.
           DISPLAY '---------------------------'.

       0200-PROCESSAR.
           EVALUATE WRK-ANOS_TRABALHADOS
               WHEN 00 THRU 01
                   DISPLAY 'SEM AUMENTO DEVIDO A TEMPO DE EMPRESA'
               WHEN 02 THRU 05
                   DISPLAY 'AUMENTO DE 5% NO SALARIO '
                   COMPUTE WRK-SALARIO = (WRK-SALARIO * 0,05
                       + WRK-SALARIO)
                   MOVE WRK-SALARIO TO WRK-SALARIO-ED
                   DISPLAY 'NOVO SALARIO: ' WRK-SALARIO-ED
               WHEN 06 THRU 15
                   DISPLAY 'AUMENTO DE 10% NO SALARIO '
                   COMPUTE WRK-SALARIO = (WRK-SALARIO * 0,10
                       + WRK-SALARIO)
                   MOVE WRK-SALARIO TO WRK-SALARIO-ED
                   DISPLAY 'NOVO SALARIO: ' WRK-SALARIO-ED
               WHEN OTHER
                   DISPLAY 'AUMENTO DE 15% NO SALARIO '
                   COMPUTE WRK-SALARIO = (WRK-SALARIO * 0,15
                       + WRK-SALARIO)
                   MOVE WRK-SALARIO TO WRK-SALARIO-ED
                   DISPLAY 'NOVO SALARIO: ' WRK-SALARIO-ED.

       0300-FINALIZAR.
           DISPLAY ' -------------------------------- '.
           DISPLAY 'PROCESSAMENTO TERMINADO'.
