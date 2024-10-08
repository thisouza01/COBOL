      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MERGE-ARQ.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

           FILE-CONTROL.

               SELECT ARQUIVO1 ASSIGN TO
                "C:\exe-cobol\arquivo1-merge.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ARQ1.

               SELECT ARQUIVO2 ASSIGN TO
                "C:\exe-cobol\arquivo2-merge.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ARQ2.

               SELECT ARQ-OUT ASSIGN TO
                "C:\exe-cobol\arquivo-out.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-OUT.

               SELECT OUTPUT-MERGE ASSIGN TO 'MERGE-O'.

       DATA DIVISION.
       FILE SECTION.

           FD ARQUIVO1.
           01 ARQ1-INPUT           PIC X(30).

           FD ARQUIVO2.
           01 ARQ2-INPUT           PIC X(30).

           FD ARQ-OUT.
           01 ARQ-OUTPUT           PIC X(30).

           SD OUTPUT-MERGE.
           01 MERGE-O.
               05 SD-CPF           PIC X(11).
               05 SD-NOME          PIC A(10).
               05 SD-CARGO         PIC X(11).
               05 SD-SALARIO       PIC 9(05)V99.
               05 SD-DATA-PAG      PIC X(10).


       WORKING-STORAGE SECTION.

           01 STAT.
               05 WS-FS-ARQ1       PIC 9(02).
               05 WS-FS-ARQ2       PIC 9(02).
               05 WS-FS-OUT        PIC 9(02).

           01 AUX.
               05 WS-EOF           PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT ARQUIVO1, ARQUIVO2.
           OPEN OUTPUT ARQ-OUT.

           IF WS-FS-ARQ1 AND WS-FS-ARQ2 = '00'

               MERGE OUTPUT-MERGE
               ON ASCENDING KEY SD-CPF
               WITH DUPLICATES IN ORDER
               USING ARQUIVO1, ARQUIVO2
               GIVING ARQ-OUT

               DISPLAY 'MERGE COMPLETO'

           END-IF.

           CLOSE ARQUIVO1.
           CLOSE ARQUIVO2.
           CLOSE ARQ-OUT.

            STOP RUN.
       END PROGRAM MERGE-ARQ.
