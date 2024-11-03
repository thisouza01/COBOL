      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA.
      *================================================================*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WR-TABELA.
           03 WR-TBL   PIC 9(05)  OCCURS 10 TIMES.


       01 WR-I         PIC 9(03)  VALUE ZEROS.
       01 WR-RND       PIC 9(03)  VALUE ZEROS.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0000-PRINCIPAL.
           PERFORM 0001-INICIAR.
           PERFORM 0002-PROCESSAR.
           PERFORM 0003-FINALIZAR.
           STOP RUN.

       0001-INICIAR.
           DISPLAY '--------------------------------------------------'.

       0002-PROCESSAR.
           PERFORM VARYING WR-I FROM 1 BY 1
               UNTIL WR-I > 10
               PERFORM 0100-GERA-NUMERO
           END-PERFORM.

       0003-FINALIZAR.
           PERFORM VARYING WR-I FROM 1 BY 1
               UNTIL WR-I > 10
               DISPLAY WR-TBL(WR-I)
           END-PERFORM.


       0100-GERA-NUMERO.
           COMPUTE WR-RND = FUNCTION RANDOM * 10000.
           MOVE WR-RND TO WR-TBL(WR-I).



      *================================================================*
