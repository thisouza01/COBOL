      ******************************************************************
      * Author:
      * Date:
      * Purpose: Crie uma Tabela com OCCURS em duas camadas: uma
      *  matriz de 5 linhas e 3 colunas. Preencha os valores e exiba
      *  cada elemento com loop aninhado.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COLUNA_EM_CAMADAS.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  TABELA.
           05 LINHA           OCCURS 5 TIMES.
               10 COLUNA          PIC X(10) OCCURS 3 TIMES.

       01  CONTADOR.
           05 I           PIC 9(02) VALUE 1.
           05 J           PIC 9(02) VALUE 1.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      * Populando as colunas da Linha 1
           MOVE 'JOAO' TO COLUNA(1, 1).
           MOVE 'MARIA' TO COLUNA(1, 2).
           MOVE 'PAULO' TO COLUNA(1, 3).

      * Populando as colunas da Linha 2
           MOVE 'ANA' TO COLUNA(2, 1).
           MOVE 'CARLOS' TO COLUNA(2, 2).
           MOVE 'LUCAS' TO COLUNA(2, 3).

      * Populando as colunas da Linha 3
           MOVE 'DIEGO' TO COLUNA(3, 1).
           MOVE 'SOFIA' TO COLUNA(3, 2).
           MOVE 'BIA' TO COLUNA(3, 3).


           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY 'LINHA: 'I
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   DISPLAY 'COLUNA: 'J ' | ' ' VALOR: ' TABELA
               END-PERFORM
           END-PERFORM.
            STOP RUN.
       END PROGRAM COLUNA_EM_CAMADAS.
