      ******************************************************************
      * Author:
      * Date:
      * Purpose: Leia um arquivo de funcionários com campos como nome,
      *  departamento e salário. Gere um relatório que mostre o total de
      *  salários pagos por departamento. Para cada departamento,
      *  calcule também o salário médio, o número de funcionários e
      *  inclua um totalizador
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNCIONARIO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

               SELECT FUNCIONARIOS ASSIGN TO
                "C:\exe-cobol\funcionario.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-FUNCIONARIOS.

       DATA DIVISION.
       FILE SECTION.

           FD FUNCIONARIOS.
           01 REGISTRO                     PIC X(25).

       WORKING-STORAGE SECTION.

           01 WS-FUNCIONARIOS.
               05 WS-NOME                  PIC A(10).
               05 WS-DEPARTAMENTO          PIC A(10).
               05 WS-SALARIO               PIC 9(05)V99.

           01 SALARIO-DEPT.
               05 WS-SALARIO-TI            PIC 9(07)V99.
               05 WS-SALARIO-BIO           PIC 9(07)V99.
               05 WS-SALARIO-ELT           PIC 9(07)V99.

           01 MEDIA-SALARIO-DEPT.
               05 WS-SALARIO-TI-MEDIO      PIC 9(05)V99.
               05 WS-SALARIO-BIO-MEDIO     PIC 9(05)V99.
               05 WS-SALARIO-ELT-MEDIO     PIC 9(05)V99.

           01 AUX.
               05 EOF                      PIC X(01) VALUE 'N'.
               05 WS-CNT-TI                PIC 9(02) VALUE ZEROS.
               05 WS-CNT-BIO               PIC 9(02) VALUE ZEROS.
               05 WS-CNT-ELT               PIC 9(02) VALUE ZEROS.
               05 WS-TOTAL-SALARIO         PIC 9(08)V99 VALUE ZEROS.
               05 WS-TOTAL-SAL-EDIT   PIC ZZZ,ZZ9.9(02) BLANK WHEN ZERO.

           01 STATS.
               05 WS-FS-FUNCIONARIOS       PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO THRU 0100-ABRE-ARQUIVO-EXIT.
           PERFORM 0200-LE-ARQUIVO UNTIL EOF = 'Y'.
           PERFORM 1000-FECHA-ARQUIVO THRU 1000-FECHA-ARQUIVO-EXIT.
            STOP RUN.

       0100-ABRE-ARQUIVO.
           OPEN INPUT FUNCIONARIOS.
       0100-ABRE-ARQUIVO-EXIT. EXIT.

       0200-LE-ARQUIVO.
           IF WS-FS-FUNCIONARIOS = 00
               READ FUNCIONARIOS INTO WS-FUNCIONARIOS
               AT END MOVE 'Y' TO EOF
               PERFORM 0260-CALCULA-MEDIA-DPT THRU
                   0260-CALCULA-MEDIA-DPT-EXIT
               PERFORM 0270-CALCULA-SALARIO-TOTAL THRU
                   0270-CALCULA-SALARIO-TOTAL-EXIT
               PERFORM 0300-MOSTRA-DEPARTAMENTO THRU
                   0300-MOSTRA-DEPARTAMENTO-EXIT
               NOT AT END
               UNSTRING REGISTRO
               DELIMITED BY ','
               INTO
                   WS-NOME
                   WS-DEPARTAMENTO
                   WS-SALARIO
               END-UNSTRING
               END-READ
           END-IF.

           IF EOF NOT = 'Y'
             PERFORM 0250-CALCULA-SALDEPT THRU 0250-CALCULA-SALDEPT-EXIT
           END-IF.
       0200-LE-ARQUIVO-EXIT. EXIT.

       0250-CALCULA-SALDEPT.
           EVALUATE WS-DEPARTAMENTO
               WHEN = 'TI'
                   ADD WS-SALARIO TO WS-SALARIO-TI
                   ADD 1 TO WS-CNT-TI
               WHEN = 'Biologia'
                   ADD WS-SALARIO TO WS-SALARIO-BIO
                   ADD 1 TO WS-CNT-BIO
               WHEN = 'Eletronico'
                   ADD WS-SALARIO TO WS-SALARIO-ELT
                   ADD 1 TO WS-CNT-ELT
               WHEN OTHER
                   DISPLAY 'DEPARTAMENTO INVALIDO'
           END-EVALUATE.
       0250-CALCULA-SALDEPT-EXIT. EXIT.

       0260-CALCULA-MEDIA-DPT.
           COMPUTE WS-SALARIO-TI-MEDIO =
           WS-SALARIO-TI / WS-CNT-TI.

           COMPUTE WS-SALARIO-BIO-MEDIO =
           WS-SALARIO-BIO / WS-CNT-BIO.

           COMPUTE WS-SALARIO-ELT-MEDIO =
           WS-SALARIO-ELT / WS-CNT-ELT.
       0260-CALCULA-MEDIA-DPT-EXIT. EXIT.

       0270-CALCULA-SALARIO-TOTAL.
           COMPUTE WS-TOTAL-SALARIO =
               WS-SALARIO-TI + WS-SALARIO-BIO + WS-SALARIO-ELT.
           MOVE WS-TOTAL-SALARIO TO WS-TOTAL-SAL-EDIT.
       0270-CALCULA-SALARIO-TOTAL-EXIT. EXIT.

       0300-MOSTRA-DEPARTAMENTO.
           DISPLAY 'TI'
           DISPLAY 'SALARIO: 'WS-SALARIO-TI.
           DISPLAY 'SALARIO MEDIO: 'WS-SALARIO-TI-MEDIO.
           DISPLAY 'NUMERO DE FUNCIONARIOS: 'WS-CNT-TI.
           DISPLAY '------------------'.
           DISPLAY 'BIOLOGIA'
           DISPLAY 'SALARIO: 'WS-SALARIO-BIO.
           DISPLAY 'SALARIO MEDIO: 'WS-SALARIO-BIO-MEDIO.
           DISPLAY 'NUMERO DE FUNCIONARIOS: 'WS-CNT-BIO.
           DISPLAY '------------------'.
           DISPLAY 'ELETRONICA'
           DISPLAY 'SALARIO: 'WS-SALARIO-ELT.
           DISPLAY 'SALARIO MEDIO: 'WS-SALARIO-ELT-MEDIO.
           DISPLAY 'NUMERO DE FUNCIONARIOS: 'WS-CNT-ELT .
           DISPLAY '------------------'.
           DISPLAY 'TOTAL SALARIOS'.
           DISPLAY '-=-=-=-=-=-==-='
           DISPLAY WS-TOTAL-SAL-EDIT.
           0300-MOSTRA-DEPARTAMENTO-EXIT. EXIT.

       1000-FECHA-ARQUIVO.
           CLOSE FUNCIONARIOS.
       1000-FECHA-ARQUIVO-EXIT. EXIT.

       END PROGRAM FUNCIONARIO.
