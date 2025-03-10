      ******************************************************************
      * Author: Thiago Souza
      * Date: 10/03/2025
      * Purpose: cria, insere e le dados em arquivo indexado
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARQUIVO-INDEXADO.
      *
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
            FILE-CONTROL.
             SELECT  ARQ-INDX
              ASSIGN TO 'C:\Nova pasta\produtos.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-ID
               FILE STATUS IS WS-FILE-STATUS.
      *
       DATA DIVISION.
           FILE SECTION.
               FD ARQ-INDX.
               01 ARQ-REGISTRO.
                   03 CHAVE-REGISTRO.
                       05 PROD-ID      PIC 9(03) VALUE ZEROS.
                   03 NOME             PIC X(30) VALUE SPACES.

       WORKING-STORAGE SECTION.
           01 WS-STATUS.
               03 WS-FILE-STATUS   PIC 9(02) VALUE ZEROS.

           01 WS-EOF.
               03 EOF              PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       0000-MAIN.
           PERFORM 0100-ABRIR-ARQUIVO.
           PERFORM 0200-MOVER-DADOS.
           PERFORM UNTIL WS-EOF EQUAL "S"
               PERFORM 0300-LE-ARQUIVO
           END-PERFORM.
           PERFORM 1000-FECHA-ARQUIVO.

       0100-ABRIR-ARQUIVO.
            OPEN I-O ARQ-INDX.

       0200-MOVER-DADOS.
            MOVE 001 TO PROD-ID.
            MOVE "THIAGO" TO NOME.
            WRITE ARQ-REGISTRO.

            MOVE 002 TO PROD-ID.
            MOVE "MARIA" TO NOME.
            WRITE ARQ-REGISTRO.

            MOVE 003 TO PROD-ID.
            MOVE "JOAO" TO NOME.
            WRITE ARQ-REGISTRO.

       0300-LE-ARQUIVO.
            IF WS-FILE-STATUS = '00'
               READ ARQ-INDX
                   AT END
                       MOVE "S" TO EOF
                   NOT AT END
                       DISPLAY "ID PRODUTO: "PROD-ID, "NOME: " NOME
           END-IF.

       1000-FECHA-ARQUIVO.
            CLOSE ARQ-INDX.
            STOP RUN.
       END PROGRAM ARQUIVO-INDEXADO.
