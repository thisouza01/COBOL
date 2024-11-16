      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REDEFINES_COM_OCCURS.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  TABELA.
           05 TABELA-NUMERO        PIC 9(04) OCCURS 10 TIMES.

       01  TABELA-COMPLETA REDEFINES TABELA PIC X(40).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM POPULA-TABELA.
           PERFORM MOSTRA-TABELA.
           PERFORM POPULA-TABELA-DENOVO.
           PERFORM MOSTRA-TABELA-DENOVO.
            STOP RUN.

       POPULA-TABELA.
           MOVE 1324 TO TABELA-NUMERO(1).
           MOVE 1111 TO TABELA-NUMERO(2).
           MOVE 2222 TO TABELA-NUMERO(3).
           MOVE 3333 TO TABELA-NUMERO(4).
           MOVE 4444 TO TABELA-NUMERO(5).
           MOVE 5555 TO TABELA-NUMERO(6).
           MOVE 6666 TO TABELA-NUMERO(7).
           MOVE 7777 TO TABELA-NUMERO(8).
           MOVE 8888 TO TABELA-NUMERO(9).
           MOVE 9999 TO TABELA-NUMERO(10).

       MOSTRA-TABELA.
           DISPLAY TABELA.

       POPULA-TABELA-DENOVO.
           MOVE 'ASDZXC123456ASD456QWE789ASD456ZXC123456A'
                                            TO TABELA-COMPLETA.

       MOSTRA-TABELA-DENOVO.
           DISPLAY 'TABELA ALTERADA'
           DISPLAY TABELA-COMPLETA.


       END PROGRAM REDEFINES_COM_OCCURS.
