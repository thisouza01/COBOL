      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Array-Sorting.
      *
      *================================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 CONTADOR.
           03 WR-I                 PIC 9(02) VALUE ZEROS.

       01 TABELA.
           03 WR-ARRAY             PIC 9(02) OCCURS 15.
           03 WR-LENGTH-OF-TABELA  PIC 9(02) VALUE 15.

       01 WR-TEMP                  PIC 9(02) VALUE ZEROS.

       PROCEDURE DIVISION.
       0-MAIN.
           PERFORM 1-INICIAR
           PERFORM 2-PROCESSAR
           PERFORM 3-FINALIZAR
           STOP RUN.

       1-INICIAR.
           INITIALIZE WR-I
           INITIALIZE WR-TEMP.

       2-PROCESSAR.
           INITIALIZE WR-I
           PERFORM VARYING WR-I FROM 1 BY 1
               UNTIL WR-I > WR-LENGTH-OF-TABELA
               MOVE WR-I TO WR-ARRAY(WR-I)
           END-PERFORM.

      *    PERFORM 21-BUBBLE-SORT UNTIL WR-I >= WR-LENGTH-OF-TABELA - 1.

       3-FINALIZAR.
           DISPLAY 'TABELA ATUALIZADA: '
           PERFORM VARYING WR-I FROM 1 BY 1
               UNTIL WR-I > WR-LENGTH-OF-TABELA
               DISPLAY WR-ARRAY(WR-I)
           END-PERFORM.

       21-BUBBLE-SORT.
           INITIALIZE WR-I
           PERFORM VARYING WR-I FROM 0 BY 1
               UNTIL WR-I >= WR-LENGTH-OF-TABELA - 1
               IF WR-ARRAY(WR-I) > WR-ARRAY(WR-I + 1)
                   MOVE WR-ARRAY(WR-I + 1) TO WR-TEMP
                   MOVE WR-ARRAY(WR-I) TO WR-ARRAY(WR-I + 1)
                   MOVE WR-TEMP TO WR-ARRAY(WR-I)
               END-IF
           END-PERFORM.
      *================================================================*
