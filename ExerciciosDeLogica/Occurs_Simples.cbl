      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_SIMPLES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  CONTADOR.
           05 CONT-NOME    PIC 9(02) VALUE 1.

       01  TABELA-NOMES.
           05 NOME         PIC A(10) OCCURS 10 TIMES.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ADICIONA_NOME.
           PERFORM MOSTRA-NOME.
            STOP RUN.

       ADICIONA_NOME.
           MOVE 'AAAAA' TO NOME(1)
           MOVE 'BBBBB' TO NOME(2)
           MOVE 'CCCCC' TO NOME(3)
           MOVE 'DDDDD' TO NOME(4)
           MOVE 'EEEEE' TO NOME(5)
           MOVE 'FFFFF' TO NOME(6)
           MOVE 'GGGGG' TO NOME(7)
           MOVE 'HHHHH' TO NOME(8)
           MOVE 'IIIII' TO NOME(9)
           MOVE 'JJJJJ' TO NOME(10).

       MOSTRA-NOME.
           PERFORM VARYING CONT-NOME FROM 1 BY 1 UNTIL CONT-NOME > 10
               DISPLAY NOME(CONT-NOME)
           END-PERFORM.

       END PROGRAM OCCURS_SIMPLES.
