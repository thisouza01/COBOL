      ******************************************************************
      * Author:
      * Date:
      * Purpose: Ler um arquivo sequencial de inventário, processar os
      *  dados e gerar um relatório com produtos que estão abaixo do 
      *  estoque mínimo e também listar produtos que precisam ser 
      *  repostos com urgência.
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
               05 FLAG-STQ-MIN         PIC 9(01) VALUE 0.

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
                   PERFORM 0410-IDENTIFICA-URGENCIA
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
           PERFORM 0450-MOSTRA-REGISTRO.

       0400-IDENTIFICA-STQ-MIN.
           IF UNS-QNT_ATUAL < UNS-STQ_MIN
               PERFORM 0310-MOVE-REGISTRO
               MOVE 1 TO FLAG-STQ-MIN
           ELSE
               MOVE 0 TO FLAG-STQ-MIN
           END-IF.
               
       0410-IDENTIFICA-URGENCIA.
           IF FLAG-STQ-MIN = 1
      *QUANDO FOR MENOR QUE A METADE DO ESTOQUE MAXIMO
      *OU MAIOR QUE 20% DO 0 
               IF UNS-QNT_ATUAL > (UNS-STQ_MAX * 0.5)
                   DISPLAY 'CONTEM PRODUTOS ACIMA DA METADE DO ESTOQUE'
               ELSE IF UNS-QNT_ATUAL > 20 AND < (UNS-STQ_MAX * 0.5)
                   DISPLAY 'PRECISA DE REPOSICAO'
                   DISPLAY '--/-/-/--'
               ELSE
                   DISPLAY 'REPOSICAO URGENTE'
                   DISPLAY '--/-/-/--'                   
               END-IF
           ELSE
               DISPLAY 'PROD_ID: 'UNS-PROD_ID
               DISPLAY 'PROD_NOME: 'UNS-PROD_NOME
               DISPLAY 'PRODUTO DENTRO DO ESTOQUE NORMAL'
               DISPLAY '-/-/-/-/-'
           END-IF.    

       0450-MOSTRA-REGISTRO.
           DISPLAY 'PROD_ID: 'PROD_ID.
           DISPLAY 'PROD_NOME: 'PROD_NOME.
           DISPLAY 'QNT_ATUAL: 'QNT_ATUAL.
           DISPLAY 'STQ_MIN: 'STQ_MIN.
           DISPLAY 'STQ_MAX: 'STQ_MAX.
           
       1000-FECHA-ARQUIVO.
           CLOSE INVENTARIO.


       END PROGRAM INVENTARIO.
