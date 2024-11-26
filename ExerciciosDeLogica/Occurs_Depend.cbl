      ******************************************************************
      * Author:
      * Date:
      * Purpose:Crie uma tabela com OCCURS que usa uma variável para
      *  definir o número de ocorrências (usando DEPENDING ON). Teste
      *  atribuindo valores diferentes à variável dependente e exiba os
      *  resultados.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_DEPEND.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  TABELA.
           05 CAMPO1   OCCURS 1 TO 10 TIMES DEPENDING ON WS-CONTADOR.
               10 A    PIC X(05).
               10 B    PIC X(05).

       01  CONTADORES.
           05 WS-CONTADOR      PIC 9(02).

       01  INDICE.
           05 I                PIC 9(02).
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICILIZANDO-CONT.
           PERFORM MOVENDO-DADOS.
           PERFORM MOSTRA-OCORRENCIA.

           STOP RUN.


       INICILIZANDO-CONT.
      *    MOVE 10 TO WS-CONTADOR.
      *    TEM QUE PREENCHER TODAS AS OCCORENCIAS

           MOVE 3 TO WS-CONTADOR.


       MOVENDO-DADOS.
           MOVE 'HELLO' TO A(1).
           MOVE 'WORLD' TO B(1).

           MOVE 'PRATO' TO A(2).
           MOVE 'JARRO' TO B(2).

           MOVE 'CRAVO' TO A(3).
           MOVE 'TREVO' TO B(3).

       MOSTRA-OCORRENCIA.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-CONTADOR
               DISPLAY 'A(' I '): 'A(I)
               DISPLAY 'B(' I '): 'B(I)
           END-PERFORM.

       END PROGRAM OCCURS_DEPEND.
