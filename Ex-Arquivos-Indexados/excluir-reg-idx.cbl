      ******************************************************************
      * Author:Thiago Souza
      * Date:11/03/2025
      * Purpose:Excluir registro com base no ID
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXCLUIR-REG-IDX.
      *
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT ARQ-INDX
                       ASSIGN TO "C:\archive\produtos.dat"
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS DYNAMIC
                       RECORD KEY IS PROD-ID
                       FILE STATUS IS WS-FS-ARQ.

      *
       DATA DIVISION.
       FILE SECTION.
           FD ARQ-INDX.
           01 REG-INDX.
               03 CHAVE-PRIMARIA.
                   05 PROD-ID      PIC 9(03) VALUE ZEROS.
               03 NOME             PIC X(30) VALUE SPACES.
      *
       WORKING-STORAGE SECTION.
       01 WS-CHAVE-PRIMARIA.
           03 WS-PROD-ID           PIC 9(03) VALUE ZEROS.

       01 STATUS-ARQ.
           03 WS-FS-ARQ            PIC 9(02) VALUE ZEROS.

       01 UTIL.
           03 ESCOLHA              PIC A(01) VALUE 'N'.
      *
       PROCEDURE DIVISION.
      *
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO.
           PERFORM 0200-RECEBE-ID.
           PERFORM 0250-MOVE-CHAVE.
           PERFORM 0300-PROCESSA-ARQUIVO.
           PERFORM 1000-FECHA-ARQUIVO.



       0100-ABRE-ARQUIVO.
           OPEN I-O ARQ-INDX.
           IF WS-FS-ARQ NOT EQUAL "00"
               DISPLAY "ERRO AO ABRIR O ARQUIVO! STATUS: " WS-FS-ARQ
           END-IF.

       0200-RECEBE-ID.
           DISPLAY "QUAL ID PARA EXCLUSAO: "
           ACCEPT WS-CHAVE-PRIMARIA.

       0250-MOVE-CHAVE.
           MOVE WS-CHAVE-PRIMARIA TO PROD-ID.

       0300-PROCESSA-ARQUIVO.
           IF WS-FS-ARQ EQUAL "00"
               READ ARQ-INDX KEY IS PROD-ID
                   INVALID KEY
                       DISPLAY "REGISTRO NAO ENCONTRADO!"

                   NOT INVALID KEY
                       DISPLAY "DADOS: "NOME

                       PERFORM 0310-VERIFICA-EXCLUSAO

                       IF ESCOLHA EQUAL "S"

                           PERFORM 0400-EXCLUI-ARQUIVO

                       END-IF

               END-READ

           END-IF.

       0310-VERIFICA-EXCLUSAO.
           DISPLAY "TEM CERTEZA QUE QUER EXCLUIR [S/N]: "
           ACCEPT ESCOLHA.

       0400-EXCLUI-ARQUIVO.
           DELETE ARQ-INDX RECORD
               INVALID KEY
                   DISPLAY "ERRO AO EXCLUIR O REGISTRO!"

               NOT INVALID KEY
                   DISPLAY "REGISTRO EXCLUIDO!"

           END-DELETE.


       1000-FECHA-ARQUIVO.
           CLOSE ARQ-INDX
           STOP RUN.

       END PROGRAM EXCLUIR-REG-IDX.
