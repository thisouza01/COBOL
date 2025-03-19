      ******************************************************************
      * Author:Thiago Souza
      * Date:19/03/2025
      * Purpose: LER REGISTRO A PARTIR DE UM DETERMINADO PONTO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. BUSCA-REGISTRO.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT PRODUTOS ASSIGN TO "C:\Nova pasta\produtos.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-ID
               FILE STATUS IS WS-FS-PROD.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
           FD PRODUTOS.
           01 PROD-REG.
               03 CHAVE.
                   05 PROD-ID      PIC 9(03) VALUE ZEROS.
               03 NOME-PROD        PIC X(30) VALUE SPACES.
      *
       WORKING-STORAGE SECTION.
           01 WS-STATUS.
               03 WS-FS-PROD       PIC 9(02) VALUE ZEROS.

           01 END-OF-FILE.
               03 WS-EOF           PIC X(01) VALUE 'N'.
      ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0001-PRINCIPAL.
           PERFORM 0101-INICIAR
           IF WS-FS-PROD EQUAL "00"
               PERFORM 0201-PROCESSAR
           END-IF.
           PERFORM 1001-FINALIZA.
      ******************************************************************
       0100-INICIAR SECTION.
       0101-INICIAR.
           OPEN INPUT PRODUTOS.
      ******************************************************************
       0200-PROCESSAR SECTION.
       0201-PROCESSAR.
           MOVE 002        TO PROD-ID.
           START PRODUTOS
               KEY IS EQUAL PROD-ID
                   INVALID KEY DISPLAY "ARQUIVO NÃO ENCONTRADO!"
               NOT INVALID KEY PERFORM 0300-LER-ARQUIVO THRU
                                       0301-EXIT.

      ******************************************************************
       0300-LER-ARQUIVO.
           PERFORM UNTIL WS-EOF EQUAL "S"
               READ PRODUTOS
                   NEXT RECORD
                       AT END MOVE "S" TO WS-EOF
                   NOT AT END DISPLAY PROD-REG
               END-READ
           END-PERFORM.
      ******************************************************************
       0301-EXIT.
           EXIT.
      ******************************************************************
       1000-FINZALIZA SECTION.
       1001-FINALIZA.
           CLOSE PRODUTOS.
           STOP RUN.

       END PROGRAM BUSCA-REGISTRO.
