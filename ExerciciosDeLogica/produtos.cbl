      ******************************************************************
      * Author:
      * Date:
      * Purpose:Leia um arquivo de produtos com informações como
      *  código, nome, preço e categoria.
      *  Gere um relatório que mostre quantos produtos pertencem a
      *  cada categoria. No final do relatório, exiba o total de
      *  produtos por categoria e o valor total de estoque para cada
      *  categoria.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUTOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

           FILE-CONTROL.

               SELECT PRODUTOS ASSIGN TO
                "C:\exe-cobol\produtos.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-PROD.

       DATA DIVISION.
       FILE SECTION.

           FD PRODUTOS.
           01 REGISTRO-I               PIC X(38).

       WORKING-STORAGE SECTION.

           01 WS-PRODUTOS.
               05 WS-COD               PIC 9(03).
               05 WS-NOME              PIC X(15).
               05 WS-PRECO             PIC 9(05)V99.
               05 WS-CATEGORIA         PIC A(15).

           01 WS-PRODUTOS-O.
               05 WS-COD-O             PIC 9(03).
               05 WS-NOME-O            PIC X(15).
               05 WS-PRECO-O           PIC 9(05)V99.
               05 WS-CATEGORIA-O       PIC A(15).

           01 STAT.
               05 WS-FS-PROD           PIC 9(02).

           01 AUX.
               05 WS-EOF               PIC X(01) VALUE 'N'.

           01 CONTADOR.
               05 WS-CONT-ALIMENTO     PIC 9(02) VALUE ZEROS.
               05 WS-CONT-ELETRONICO   PIC 9(02) VALUE ZEROS.
               05 WS-CONT-HIGIENE      PIC 9(02) VALUE ZEROS.

           01 QNT-ESTOQUE.
               05 WS-QNT-ALIMENTO     PIC 9(08)V99 VALUE ZERO.
               05 WS-QNT-ELETRONICO   PIC 9(08)V99 VALUE ZERO.
               05 WS-QNT-HIGIENE      PIC 9(08)V99 VALUE ZERO.

           01 QNT-ESTOQUE-EDITED.
               05 EDIT-QNT-ALIMENTO   PIC ZZZ,ZZ9.9(02) BLANK WHEN ZERO.
               05 EDIT-QNT-ELETRONICO PIC ZZZ,ZZ9.9(02) BLANK WHEN ZERO.
               05 EDIT-QNT-HIGIENE    PIC ZZZ,ZZ9.9(02) BLANK WHEN ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            OPEN INPUT PRODUTOS.

            IF WS-FS-PROD = 00

               PERFORM UNTIL WS-EOF = 'Y'

                   READ PRODUTOS INTO WS-PRODUTOS
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END

                       UNSTRING REGISTRO-I
                       DELIMITED BY ','
                       INTO
                           WS-COD
                           WS-NOME
                           WS-PRECO
                           WS-CATEGORIA
                       END-UNSTRING

                       EVALUATE WS-CATEGORIA
                       WHEN = 'Alimentos'
                           ADD 1 TO WS-CONT-ALIMENTO
                           ADD WS-PRECO TO WS-QNT-ALIMENTO
                       WHEN = 'Eletronicos'
                           ADD 1 TO WS-CONT-ELETRONICO
                           ADD WS-PRECO TO WS-QNT-ELETRONICO
                       WHEN = 'Higiene'
                           ADD 1 TO WS-CONT-HIGIENE
                           ADD WS-PRECO TO WS-QNT-HIGIENE
                       WHEN OTHER
                           DISPLAY 'ERRO, NAO EXISTE'
                       END-EVALUATE

                       DISPLAY 'Codigo: ' WS-COD
                       DISPLAY 'Nome:   ' WS-NOME
                       DISPLAY 'Preco:  ' WS-PRECO
                       DISPLAY 'Categoria: ' WS-CATEGORIA
                       DISPLAY '------------------------'


               END-PERFORM

            END-IF

            MOVE WS-QNT-ALIMENTO TO EDIT-QNT-ALIMENTO.
            MOVE WS-QNT-ELETRONICO TO EDIT-QNT-ELETRONICO.
            MOVE WS-QNT-HIGIENE TO EDIT-QNT-HIGIENE.
       
            DISPLAY 'QUANTIDADE POR CATEGORIA E VALOR TOTAL ESTOQUE'
            DISPLAY 'Alimentos: 'WS-CONT-ALIMENTO
            DISPLAY 'Valor estoque Alimentos: 'EDIT-QNT-ALIMENTO
            DISPLAY 'Eletronicos: 'WS-CONT-ELETRONICO
            DISPLAY 'Valor estoque Eletronicos: 'EDIT-QNT-ELETRONICO
            DISPLAY 'Higiene: 'WS-CONT-HIGIENE
            DISPLAY 'Valor estoque Higiene: 'EDIT-QNT-HIGIENE

            CLOSE PRODUTOS.

            STOP RUN.
       END PROGRAM PRODUTOS.
