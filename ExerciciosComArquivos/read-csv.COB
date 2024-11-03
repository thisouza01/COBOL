      ******************************************************************
      * Author:
      * Date:
      * Purpose: Crie um programa que leia um arquivo CSV contendo dados
      *  de produtos (nome, preço, quantidade). Exiba os dados na tela
      *  e calcule o valor total do estoque para cada
      *  produto (preço * quantidade).
      *   Adicione um campo de desconto e aplique uma redução no preço
      *  para produtos cujo valor total do estoque ultrapasse um
      *  determinado limite.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-CSV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT ARQ-CSV
            ASSIGN TO "C:\Users\WIN 11\OneDrive\Desktop\arq-csv.csv"
            ORGANIZATION IS LINE SEQUENTIAL
            FILE STATUS IS WS-FS-ARQ.

       DATA DIVISION.
       FILE SECTION.
           FD ARQ-CSV.
           01 ARQ-REC              PIC X(25).

       WORKING-STORAGE SECTION.

           01 WS-ARQ-REC.
               05 WS-NOME          PIC A(20) VALUE SPACES.
               05 WS-PRECO         PIC 9(03)V99 VALUE ZEROS.
               05 WS-QUANTIDADE    PIC 9(03) VALUE ZEROS.

           01 AUX.
               05 WS-FS-ARQ        PIC 9(02).
               05 WS-EOF           PIC A(01) VALUE 'N'.
               05 WS-TOTAL-STQ     PIC 9(05) VALUE ZEROS.
               05 WS-DESCONTO      PIC 9(05) VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT ARQ-CSV.

           PERFORM UNTIL WS-EOF = 'Y'
               IF WS-FS-ARQ EQUAL '00'
                   READ ARQ-CSV INTO WS-ARQ-REC
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       UNSTRING ARQ-REC DELIMITED BY ','
                       INTO WS-NOME,
                           WS-PRECO,
                           WS-QUANTIDADE
                       END-UNSTRING

                       COMPUTE WS-TOTAL-STQ = WS-PRECO * WS-QUANTIDADE

                       EVALUATE WS-TOTAL-STQ
                           WHEN < 50
                               DISPLAY 'SEM DESCONTO'
                               MOVE 0 TO WS-DESCONTO
                           WHEN >= 50 AND <= 100
                               COMPUTE WS-DESCONTO = 0.5 * WS-PRECO
                               COMPUTE WS-TOTAL-STQ =
                                   WS-DESCONTO * WS-QUANTIDADE
                           WHEN > 100
                               COMPUTE WS-DESCONTO = 0.9 * WS-PRECO
                               COMPUTE WS-TOTAL-STQ =
                                   WS-DESCONTO * WS-QUANTIDADE
                           WHEN OTHER
                               DISPLAY 'ERRO NO CALCULO'
                       END-EVALUATE

                       DISPLAY 'NOME: 'WS-NOME
                       DISPLAY 'PRECO: 'WS-PRECO
                       DISPLAY 'QUANTIDADE: 'WS-QUANTIDADE
                       DISPLAY 'VALOR COM DESCONTO: 'WS-DESCONTO
                       DISPLAY 'VALOR TOTAL QUANTIDADE: 'WS-TOTAL-STQ
                       DISPLAY '-----------------'
               ELSE
                   DISPLAY 'ERRO NA ABERTURA DO ARQUIVO'
               END-IF
           END-PERFORM.

           CLOSE ARQ-CSV.
            STOP RUN.
       END PROGRAM READ-CSV.
