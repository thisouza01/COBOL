      ******************************************************************
      * Author:
      * Date:
      * Purpose:Leia um arquivo de produtos contendo informações de
      * estoque como código do produto, nome, quantidade em estoque,
      *  e quantidade mínima necessária. Gere um relatório que exiba
      *  todos os produtos, destacando os que estão abaixo da quantidade
      *  mínima. O relatório deve incluir o valor total de estoque e o
      * valor total necessário para repor os produtos abaixo do nível
      * mínimo.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESTOQUE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ESTOQUE ASSIGN TO
                "C:\exe-cobol\estoque.csv"
               ORGANISATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ESTOQUE.

       DATA DIVISION.
       FILE SECTION.

           FD ESTOQUE.
           01 REGISTRO             PIC X(24).

       WORKING-STORAGE SECTION.

           01 WS-ESTOQUE.
               05 WS-CODIGO        PIC 9(04).
               05 WS-NOME-PROD     PIC A(11).
               05 WS-QNT-PROD      PIC 9(03).
               05 WS-QNT-MIN       PIC 9(03).

           01 WS-STATUS.
               05 WS-FS-ESTOQUE    PIC 9(02).

           01 AUX.
               05 EOF              PIC X(01) VALUE 'N'.
               05 WS-QNT-FALTA     PIC 9(03) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO THRU 0100-ABRE-ARQUIVO-EXIT.
           PERFORM 0200-LE-ARQUIVO UNTIL EOF = 'Y'
           PERFORM 0400-FECHA-ARQUIVO THRU 0400-FECHA-ARQUIVO-EXIT.
            STOP RUN.

       0100-ABRE-ARQUIVO.
           OPEN INPUT ESTOQUE.
       0100-ABRE-ARQUIVO-EXIT. EXIT.

       0200-LE-ARQUIVO.
           IF WS-FS-ESTOQUE = 00
               READ ESTOQUE INTO WS-ESTOQUE
               AT END MOVE 'Y' TO EOF
               NOT AT END
                   PERFORM 0210-UNSTRING-REGISTRO
                   PERFORM 0220-CALCULA-QNT-FALTA
                   PERFORM 0300-MOSTRA-REGISTRO
                   PERFORM 0310-INICIALIZA-VAR
           END-IF.
       0200-LE-ARQUIVO-EXIT. EXIT.

       0210-UNSTRING-REGISTRO.
           UNSTRING REGISTRO
               DELIMITED BY ','
               INTO
                   WS-CODIGO
                   WS-NOME-PROD
                   WS-QNT-PROD
                   WS-QNT-MIN
               END-UNSTRING.
       0210-UNSTRING-REGISTRO-EXIT. EXIT.

       0220-CALCULA-QNT-FALTA.
           IF WS-QNT-PROD < WS-QNT-MIN
               COMPUTE WS-QNT-FALTA = WS-QNT-MIN - WS-QNT-PROD
           END-IF.
       0220-CALCULA-QNT-FALTA-EXIT. EXIT.

       0300-MOSTRA-REGISTRO.
           DISPLAY 'CD: 'WS-CODIGO.
           DISPLAY 'NOME-PROD: 'WS-NOME-PROD.
           DISPLAY 'QNT-PROD: 'WS-QNT-PROD.
           DISPLAY 'QNT-MIN: 'WS-QNT-MIN.
           DISPLAY 'QNT NECESSARIA PARA REPOR ESTOQUE: 'WS-QNT-FALTA.
           DISPLAY '-=-=-=-=-=-=-=-=-=-='.
       0300-MOSTRA-REGISTRO-EXIT. EXIT.

       0310-INICIALIZA-VAR.
           MOVE ZEROS TO WS-QNT-FALTA.
       0310-INICIALIZA-VAR-EXIT. EXIT.

       0400-FECHA-ARQUIVO.
           CLOSE ESTOQUE.
       0400-FECHA-ARQUIVO-EXIT. EXIT.

       END PROGRAM ESTOQUE.
