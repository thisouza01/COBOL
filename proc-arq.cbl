      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. proc-arq.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT ARQUIVO
            ASSIGN TO "C:\Users\WIN 11\OneDrive\Desktop\filecobol.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-FS-ARQUIVO.

           SELECT ARQUIVO-O
           ASSIGN TO "C:\Users\WIN 11\OneDrive\Desktop\filecobol-o.txt".

       DATA DIVISION.
       FILE SECTION.
           FD ARQUIVO.
           01 ARQ-REC.
               05 FD-NOME       PIC A(15).
               05 FD-IDADE      PIC 9(02).

           FD ARQUIVO-O.
           01 ARQ-REC-O.
               05 FD-NOME-O     PIC A(15).
               05 FD-IDADE-O    PIC 9(02).
               05 FD-CAT-O      PIC A(10).

       WORKING-STORAGE SECTION.
           01 WS-REC-IN.
               05 WS-NOME       PIC A(15).
               05 WS-IDADE      PIC 9(02).

           01 WS-REC-O.
               05 WS-NOME-O       PIC A(15).
               05 WS-IDADE-O      PIC 9(02).
               05 WS-CAT-O        PIC X(10).

           01 AUX.
               05 WS-FS-ARQUIVO PIC 9(02).
               05 WS-LAST-REC   PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT ARQUIVO
           OPEN OUTPUT ARQUIVO-O

           PERFORM UNTIL WS-LAST-REC = 'Y'
               READ ARQUIVO INTO WS-REC-IN
               AT END MOVE 'Y' TO WS-LAST-REC
               NOT AT END
                   MOVE WS-NOME TO WS-NOME-O
                   MOVE WS-IDADE TO WS-IDADE-O
                   EVALUATE TRUE
                   WHEN WS-IDADE <= 18
                       MOVE 'JOVEM' TO WS-CAT-O
                   WHEN WS-IDADE > 18 AND  WS-IDADE < 60
                       MOVE 'ADULTO' TO WS-CAT-O
                   WHEN WS-IDADE >= 60
                       MOVE 'IDOSO' TO WS-CAT-O
                   WHEN OTHER
                       DISPLAY 'ERRO'
                   END-EVALUATE

                   MOVE WS-NOME-O TO ARQ-REC-O
                   MOVE WS-IDADE-O TO ARQ-REC-O(16:2)
                   MOVE WS-CAT-O TO ARQ-REC-O(18:10)

                   WRITE ARQ-REC-O
                   END-WRITE

               END-READ
           END-PERFORM.



           CLOSE ARQUIVO
           CLOSE ARQUIVO-O
            STOP RUN.
       END PROGRAM proc-arq.
