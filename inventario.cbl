      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTARIO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT INVENTARIO ASSIGN TO
                "C:\exe-cobol\inventario.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-INVENT.

       DATA DIVISION.
       FILE SECTION.
           FD INVENTARIO.
           01 REGISTRO                 PIC X(45).

       WORKING-STORAGE SECTION.
           01 WS-REGISTRO.
               05 PROD_ID              PIC X(04).
               05 PROD_NOME            PIC X(18).
               05 QNT_ATUAL            PIC 9(03).
               05 STQ_MIN              PIC 9(03).
               05 STQ_MAX              PIC 9(03).

           01 UNS-REGISTRO.
               05 UNS-PROD_ID          PIC X(04).
               05 UNS-PROD_NOME        PIC X(18).
               05 UNS-QNT_ATUAL        PIC 9(03).
               05 UNS-STQ_MIN          PIC 9(03).
               05 UNS-STQ_MAX          PIC 9(03).

           01 AUX.
               05 EOF                  PIC X(01) VALUE 'N'.

           01 WS-STATUS.
               05 FS-INVENT            PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO.
           PERFORM UNTIL EOF = 'Y'
               PERFORM 0200-LE-ARQUIVO
               IF EOF = 'N'
                   PERFORM 0300-UNSTRING-ARQUIVO
                   PERFORM 0400-IDENTIFICA-STQ-MIN
               END-IF
           END-PERFORM.
           PERFORM 1000-FECHA-ARQUIVO.
           STOP RUN.

       0100-ABRE-ARQUIVO.
           OPEN INPUT INVENTARIO.

       0200-LE-ARQUIVO.
           IF FS-INVENT = 00
               READ INVENTARIO INTO WS-REGISTRO
                AT END MOVE 'Y' TO EOF
               END-READ
           END-IF.

       0300-UNSTRING-ARQUIVO.
           UNSTRING REGISTRO
           DELIMITED BY ','
           INTO
               UNS-PROD_ID
               UNS-PROD_NOME
               UNS-QNT_ATUAL
               UNS-STQ_MIN
               UNS-STQ_MAX
           END-UNSTRING.

       0310-MOVE-REGISTRO.
           MOVE UNS-PROD_ID TO PROD_ID.
           MOVE UNS-PROD_NOME TO PROD_NOME.
           MOVE UNS-QNT_ATUAL TO QNT_ATUAL.
           MOVE UNS-STQ_MIN TO STQ_MIN.
           MOVE UNS-STQ_MAX TO STQ_MAX.
           DISPLAY WS-REGISTRO.

       0400-IDENTIFICA-STQ-MIN.
           IF UNS-QNT_ATUAL < UNS-STQ_MIN
               PERFORM 0310-MOVE-REGISTRO
           END-IF.

       0450-MOSTRA-REGISTRO.
           DISPLAY WS-REGISTRO.

       1000-FECHA-ARQUIVO.
           CLOSE INVENTARIO.


       END PROGRAM INVENTARIO.
