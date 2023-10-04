       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCOBEX01.
      *********************************
      * REMARKS
      * NOME = THIAGO SOUZA
      * OBJETIVO: RECEBER NOME, ANO_ENTRADA, SALARIO_FUNCIONARIO
      *********************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME                 PIC X(20)     VALUE SPACES.
       77 WRK-TEMPO_TRABALHADO     PIC 9(02)     VALUE ZEROS.
       77 WRK-SALARIO              PIC 9(05)V99  VALUE ZEROS.
       77 WRK-SALARIO-ED           PIC $ZZZZ,99  VALUE ZEROS.
       PROCEDURE DIVISION.
           DISPLAY 'NOME.. '
           ACCEPT WRK-NOME.

           DISPLAY 'TEMPO TRABALHADO.. '
           ACCEPT WRK-TEMPO_TRABALHADO.

           DISPLAY 'SALARIO ATUAL.. '
           ACCEPT WRK-SALARIO.

           EVALUATE WRK-TEMPO_TRABALHADO
               WHEN 0 THRU 01
                   DISPLAY 'NAO TERA AUMENTO'

               WHEN 02 THRU 05
                   COMPUTE WRK-SALARIO-ED = (WRK-SALARIO * 0,05) +
                    WRK-SALARIO
                   DISPLAY 'NOVO SALARIO.. ' WRK-SALARIO-ED

               WHEN 06 THRU 15
                   COMPUTE WRK-SALARIO-ED = (WRK-SALARIO * 0,10) +
                    WRK-SALARIO
                   DISPLAY 'NOVO SALARIO.. ' WRK-SALARIO-ED

               WHEN OTHER
                   COMPUTE WRK-SALARIO-ED = (WRK-SALARIO * 0,15) +
                    WRK-SALARIO
                   DISPLAY 'NOVO SALARIO.. ' WRK-SALARIO-ED

           STOP RUN.
