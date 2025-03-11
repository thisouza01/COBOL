      ******************************************************************
      * Author:Thiago Souza
      * Date:11/03/2025
      * Purpose:Atualizar registro com base no indice
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATUALIZAR-ARQ-IDX.
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

           01 WS-DADOS-ATUALIZAR.
               03 WS-ID            PIC 9(03) VALUE ZEROS.
               03 WS-NOME          PIC X(30) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       0000-MAIN.
           PERFORM 0100-ABRIR-ARQUIVO.
           PERFORM 0200-MOVER-DADOS.
           PERFORM 0300-PERGUNTA-ID.
           PERFORM 0400-LER-ARQUIVO.
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

       0300-PERGUNTA-ID.
           DISPLAY "QUAL ID DESEJA ALTERAR: ".
           ACCEPT WS-ID.

       0400-LER-ARQUIVO.

           MOVE WS-ID TO PROD-ID.

           READ ARQ-INDX KEY IS PROD-ID
               INVALID KEY
                   DISPLAY "REGISTRO NÃO ENCONTRADO!"
               NOT INVALID KEY
                   DISPLAY "DADOS: " NOME

                   DISPLAY "QUAL NOVO NOME: "
                   ACCEPT WS-NOME

                   PERFORM 0450-ATUALIZAR-DADO

                   DISPLAY "NOVOS DADOS: " NOME
           END-READ.

       0450-ATUALIZAR-DADO.
           MOVE WS-NOME TO NOME.
           REWRITE ARQ-REGISTRO.

       1000-FECHA-ARQUIVO.
            CLOSE ARQ-INDX.
            STOP RUN.
       END PROGRAM ATUALIZAR-ARQ-IDX.
