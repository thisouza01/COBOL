      ******************************************************************
      * Author:
      * Date:
      * Purpose: Ler um arquivo sequencial contendo informações de
      *  vendas de diferentes lojas, consolidar os dados por região
      *  e gerar um relatório com o total de vendas, média e número de
      *  transações por região.
      * Inclua um critério que permita gerar o relatório apenas para
      *  regiões com mais de um determinado valor em vendas ou número
      *  de transações
      * Marque regiões onde o total de vendas ultrapassa um certo
      *  limite como “Regiões de Alta Venda”.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VENDAS-REGIAO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT VENDAS-REGIAO ASSIGN TO
                "C:\exe-cobol\vendas-regiao.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-VENDAS.

       DATA DIVISION.
       FILE SECTION.
           FD VENDAS-REGIAO.
           01 REGISTRO                PIC X(29).

       WORKING-STORAGE SECTION.
           01 WS-REGISTRO.
               05 WS-LOJA             PIC X(06).
               05 WS-REGIAO           PIC A(05).
               05 WS-DATA             PIC X(10).
               05 WS-VALOR_VENDA      PIC 9(08)V99.

           01 VALORES-REGIAO.
               05 VALOR-NORTE         PIC 9(10)V99.
               05 VALOR-SUL           PIC 9(10)V99.
               05 VALOR-LESTE         PIC 9(10)V99.
               05 VALOR-OESTE         PIC 9(10)V99.

           01 CONTADOR-REGIAO.
               05 CNT-NORTE           PIC 9(02) VALUE ZEROS.
               05 CNT-SUL             PIC 9(02) VALUE ZEROS.
               05 CNT-LESTE           PIC 9(02) VALUE ZEROS.
               05 CNT-OESTE           PIC 9(02) VALUE ZEROS.

           01 MEDIA-REGIAO.
               05 MEDIA-NORTE         PIC 9(06)V99 VALUE ZEROS.
               05 MEDIA-SUL           PIC 9(06)V99 VALUE ZEROS.
               05 MEDIA-LESTE         PIC 9(06)V99 VALUE ZEROS.
               05 MEDIA-OESTE         PIC 9(06)V99 VALUE ZEROS.

           01 AUX.
               05 EOF                 PIC X(01) VALUE 'N'.

           01 WS-STATUS.
               05 FS-VENDAS           PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO.
           PERFORM UNTIL EOF = 'Y'
               PERFORM 0200-LE-ARQUIVO
               IF EOF = 'N'
                   PERFORM 0300-UNSTRING-REGISTRO
                   PERFORM 0400-MOSTRA-REGISTRO
                   PERFORM 0500-COMPARA-REGIAO
               END-IF
           END-PERFORM.
           PERFORM 0550-MEDIA-REGIAO.
           PERFORM 0600-MOSTRA-VALOR-REGIAO.
           PERFORM 1000-FECHA-REGISTRO.
            STOP RUN.

       0100-ABRE-ARQUIVO.
           OPEN INPUT VENDAS-REGIAO.

       0200-LE-ARQUIVO.
           READ VENDAS-REGIAO INTO WS-REGISTRO
               AT END MOVE 'Y' TO EOF
           END-READ.

       0300-UNSTRING-REGISTRO.
           UNSTRING REGISTRO
           DELIMITED BY '|'
           INTO
               WS-LOJA
               WS-REGIAO
               WS-DATA
               WS-VALOR_VENDA
           END-UNSTRING.

       0400-MOSTRA-REGISTRO.
           DISPLAY 'LOJA: 'WS-LOJA.
           DISPLAY 'REGIAO: 'WS-REGIAO.
           DISPLAY 'DATA: 'WS-DATA.
           DISPLAY 'VALOR VENDA: 'WS-VALOR_VENDA.
           DISPLAY '-*-*-*-*-*-*'.

       0500-COMPARA-REGIAO.
           EVALUATE WS-REGIAO
               WHEN = 'Norte'
                   ADD WS-VALOR_VENDA TO VALOR-NORTE
                   ADD 1 TO CNT-NORTE
               WHEN = 'Sul'
                   ADD WS-VALOR_VENDA TO VALOR-SUL
                   ADD 1 TO CNT-SUL
               WHEN = 'Leste'
                   ADD WS-VALOR_VENDA TO VALOR-LESTE
                   ADD 1 TO CNT-LESTE
               WHEN = 'Oeste'
                   ADD WS-VALOR_VENDA TO VALOR-OESTE
                   ADD 1 TO CNT-OESTE
               WHEN OTHER
                   DISPLAY 'REGIAO INEXISTENTE'
           END-EVALUATE.

       0550-MEDIA-REGIAO.
           COMPUTE MEDIA-NORTE = VALOR-NORTE / CNT-NORTE.
           COMPUTE MEDIA-SUL = VALOR-SUL / CNT-SUL.
           COMPUTE MEDIA-LESTE = VALOR-LESTE / CNT-LESTE.
           COMPUTE MEDIA-OESTE = VALOR-OESTE / CNT-OESTE.

       0600-MOSTRA-VALOR-REGIAO.
           DISPLAY 'REGIAO NORTE'
           DISPLAY 'QUANTIDADE DE VENDAS: 'CNT-NORTE.
           DISPLAY 'VALOR DAS VENDAS: 'VALOR-NORTE.
           DISPLAY 'MEDIA: 'MEDIA-NORTE.
           DISPLAY '----------------------------------------'.
           DISPLAY 'REGIAO SUL'
           DISPLAY 'QUANTIDADE DE VENDAS: 'CNT-SUL.
           DISPLAY 'VALOR DAS VENDAS: 'VALOR-SUL.
           DISPLAY 'MEDIA: 'MEDIA-SUL.
           DISPLAY '----------------------------------------'.
           DISPLAY 'REGIAO LESTE'
           DISPLAY 'QUANTIDADE DE VENDAS: 'CNT-LESTE.
           DISPLAY 'VALOR DAS VENDAS: 'VALOR-LESTE.
           DISPLAY 'MEDIA: 'MEDIA-LESTE.
           DISPLAY '----------------------------------------'.
           DISPLAY 'REGIAO OESTE'
           DISPLAY 'QUANTIDADE DE VENDAS: 'CNT-OESTE.
           DISPLAY 'VALOR DAS VENDAS: 'VALOR-OESTE.
           DISPLAY 'MEDIA: 'MEDIA-OESTE.
           DISPLAY '----------------------------------------'.

       1000-FECHA-REGISTRO.
           CLOSE VENDAS-REGIAO.

       END PROGRAM VENDAS-REGIAO.
