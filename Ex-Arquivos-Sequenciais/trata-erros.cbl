      ******************************************************************
      * Author:
      * Date:
      * Purpose:Criar um programa que faz a leitura de um arquivo e
      *  implementa tratamento de erros em caso de falhas.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRATA-ERROS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ARQ1 ASSIGN TO
                "C:\exe-cobol\inventario.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ARQ1.

       DATA DIVISION.
       FILE SECTION.
           FD ARQ1.
           01 REGISTRO             PIC X(34).

       WORKING-STORAGE SECTION.
       01  WS-REGISTRO.
           05 ID-PROD              PIC X(04).
           05 NOME-PROD            PIC X(18).
           05 QNT-ATUAL            PIC 9(03).
           05 QNT-MIN              PIC 9(03).
           05 QNT-MAX              PIC 9(03).

       01  AUX.
           05 EOF                  PIC X(01) VALUE 'N'.
           05 FLAG-VERIFICA-REG    PIC 9(01) VALUE 0.

       01  WS-STATUS.
           05 FS-ARQ1              PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ABRE-ARQUIVO.
           PERFORM UNTIL EOF = 'Y'
               PERFORM LE-ARQUIVO
      * VERIFICA SE O ARQUIVO NAO CHEGOU AO FIM
               IF EOF = 'N'
                   PERFORM UNSTRING-ARQUIVO
                   PERFORM MOSTRA-ARQUIVO
               END-IF
           END-PERFORM.
           PERFORM FECHA-ARQUIVO.
            STOP RUN.

       ABRE-ARQUIVO.
           OPEN INPUT ARQ1.

       LE-ARQUIVO.
      * VERIFICA SE O ARQUIVO FOI ABERTO COM SUCESSO
       IF FS-ARQ1 = 00
           READ ARQ1 INTO WS-REGISTRO
           AT END MOVE 'Y' TO EOF
       ELSE
           PERFORM FECHA-ARQUIVO
       END-IF.

       UNSTRING-ARQUIVO.
           UNSTRING REGISTRO
               DELIMITED BY ','
               INTO
                   ID-PROD
                   NOME-PROD
                   QNT-ATUAL
                   QNT-MIN
                   QNT-MAX
           END-UNSTRING.

       VERIFICA-REGISTRO.
           IF ID-PROD = SPACES OR NOME-PROD = SPACES
               DISPLAY 'REGISTRO INCOMPLETO'
               PERFORM FECHA-ARQUIVO
               STOP RUN
           ELSE
               MOVE 1 TO FLAG-VERIFICA-REG
           END-IF.

       MOSTRA-ARQUIVO.
           DISPLAY 'ID-PROD: 'ID-PROD.
           DISPLAY 'NOME-PROD: 'NOME-PROD.
           DISPLAY 'QNT-ATUAL: 'QNT-ATUAL.
           DISPLAY 'QNT-MIN: 'QNT-MIN.
           DISPLAY 'QNT-MAX: 'QNT-MAX.
           DISPLAY '--------------------------'.

       FECHA-ARQUIVO.
           CLOSE ARQ1.

       END PROGRAM TRATA-ERROS.
