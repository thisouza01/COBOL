      ******************************************************************
      * Author:
      * Date:
      * Purpose:
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
                   03 NOME         PIC X(30) VALUE SPACES.

       WORKING-STORAGE SECTION.
           01 WS-STATUS.
               03 WS-FILE-STATUS   PIC 9(02) VALUE ZEROS.

           01 RECORD-KEY.
               03 WS-PROD-ID          PIC 9(03) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN OUTPUT ARQ-INDX.



            CLOSE ARQ-INDX.
            STOP RUN.
       END PROGRAM ARQUIVO-INDEXADO.
