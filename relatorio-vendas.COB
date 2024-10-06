      ******************************************************************
      * Author:
      * Date:
      * Purpose: Gerar Relatório de Vendas Simples
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RLT-VENDAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

               SELECT ARQ-RLT
                ASSIGN TO "C:\COBOL-exercicios\arq-relatorio.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-RLT.

               SELECT ARQ-RLT-OUT
                ASSIGN TO "C:\COBOL-exercicios\arq-relatorio-output.txt"
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS WS-FS-RLT-OUT.

       DATA DIVISION.
       FILE SECTION.

           FD ARQ-RLT.
           01 ARQ-RELATORIO        PIC X(30).

           FD ARQ-RLT-OUT.
           01 ARQ-RELATORIO-OUT    PIC X(50).


       WORKING-STORAGE SECTION.

           01 AUX.
               05 WS-FS-RLT        PIC 9(02).
               05 WS-FS-RLT-OUT    PIC 9(02).
               05 WS-EOF           PIC X(01) VALUE 'N'.

           01 WS-ARQ-RELATORIO.
               05 WS-ARQ-ID        PIC 9(04).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-DATA.
                   07 WS-DAY       PIC 9(02).
                   07 FILLER       PIC X(01) VALUE '/'.
                   07 WS-MONTH     PIC 9(02).
                   07 FILLER       PIC X(01) VALUE '/'.
                   07 WS-YEAR      PIC 9(04).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-CD-PROD       PIC 9(04).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-VALOR         PIC 9(03)V99.
               05 FILLER           PIC X(06) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT ARQ-RLT.
           OPEN OUTPUT ARQ-RLT-OUT.

           IF WS-FS-RLT = '00'

               PERFORM UNTIL WS-EOF = 'Y'
                   READ ARQ-RLT INTO WS-ARQ-RELATORIO
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END
                           MOVE WS-ARQ-RELATORIO TO ARQ-RELATORIO-OUT
                           WRITE ARQ-RELATORIO-OUT
               END-PERFORM

           ELSE

               DISPLAY 'ERRO ABERTURA DE ARQUIVO: 'WS-FS-RLT

           END-IF.

           CLOSE ARQ-RLT.
           CLOSE ARQ-RLT-OUT.

            STOP RUN.
       END PROGRAM RLT-VENDAS.
