      ******************************************************************
      * Author:
      * Date:
      * Purpose:Você tem dois arquivos de produtos: o primeiro contém
      *  os preços antigos, e o segundo contém os novos preços.
      *  Leia ambos os arquivos, compare os preços e gere um arquivo
      *  de saída contendo apenas os produtos que tiveram seu preço
      *  alterado. Inclua no arquivo o código do produto, o preço
      *  antigo, o novo preço e a variação percentual.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISTA-PRODUTO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

           FILE-CONTROL.

               SELECT PRECO-ANTIGO ASSIGN TO
                "C:\COBOL-exercicios\preco-antigo.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ANTIGO.

               SELECT PRECO-NOVO ASSIGN TO
                "C:\COBOL-exercicios\preco-novo.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-NOVO.

               SELECT RLT-PRECO ASSIGN TO
                "C:\COBOL-exercicios\rlt-preco.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-RLT.

       DATA DIVISION.
       FILE SECTION.

           FD PRECO-ANTIGO.
           01 REG-ANTIGO                   PIC X(17).

           FD PRECO-NOVO.
           01 REG-NOVO                     PIC X(17).

           FD RLT-PRECO.
           01 RELATORIO                    PIC X(30).

       WORKING-STORAGE SECTION.

           01 WS-REG-ANTIGO.
               05 WS-PRODUTO-A             PIC A(09).
               05 WS-PRECO-A               PIC 9(02)V99.

           01 WS-REG-NOVO.
               05 WS-PRODUTO-N             PIC A(09).
               05 WS-PRECO-N               PIC 9(02)V99.

           01 WS-RLT-PRECO.
               05 WS-CODIGO-O              PIC 9(04).
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-PRECO-ANT-O           PIC 9(02)V99.
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-PRECO-NOV-O           PIC 9(02)V99.
               05 FILLER                   PIC X(01) VALUE SPACES.
               05 WS-VAR-PERC-O            PIC 9(03)V99.

           01 WS-STAT.
               05 WS-FS-ANTIGO             PIC 9(02).
               05 WS-FS-NOVO               PIC 9(02).
               05 WS-FS-RLT                PIC 9(02).

           01 AUX.
               05 EOF-ANTIGO               PIC A(01) VALUE 'N'.
               05 EOF-NOVO                 PIC A(01) VALUE 'N'.
               05 VAR-PERCENTUAL           PIC 9(03)V99 VALUE ZEROS.
               05 WS-CODIGO                PIC 9(04) VALUE 0001.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       OPEN INPUT PRECO-ANTIGO.
       OPEN INPUT PRECO-NOVO.
       OPEN OUTPUT RLT-PRECO.

       IF WS-FS-ANTIGO = 00 AND WS-FS-NOVO = 00

           PERFORM UNTIL EOF-ANTIGO = 'Y' AND EOF-NOVO = 'Y'

               IF EOF-ANTIGO NOT = 'Y'

                   READ PRECO-ANTIGO INTO WS-REG-ANTIGO
                   AT END MOVE 'Y' TO EOF-ANTIGO
                   NOT AT END
                   UNSTRING REG-ANTIGO
                   DELIMITED BY ','
                   INTO
                       WS-PRODUTO-A
                       WS-PRECO-A
                   END-UNSTRING
                   END-READ

               END-IF

               IF EOF-NOVO NOT = 'Y'
                   READ PRECO-NOVO INTO WS-REG-NOVO
                   AT END MOVE 'Y' TO EOF-NOVO
                   NOT AT END
                   UNSTRING REG-NOVO
                   DELIMITED BY ','
                   INTO
                       WS-PRODUTO-N
                       WS-PRECO-N
                   END-UNSTRING
                   END-READ

               IF EOF-ANTIGO NOT = 'Y' AND EOF-NOVO NOT = 'Y'
                   IF WS-PRECO-A NOT = WS-PRECO-N

                       COMPUTE VAR-PERCENTUAL =
                         ((WS-PRECO-A - WS-PRECO-N) / WS-PRECO-A) * 100

                       MOVE WS-CODIGO TO WS-CODIGO-O
                       MOVE WS-PRECO-A TO WS-PRECO-ANT-O
                       MOVE WS-PRECO-N TO WS-PRECO-NOV-O
                       MOVE VAR-PERCENTUAL TO WS-VAR-PERC-O

                       IF WS-FS-RLT = 00
                           MOVE WS-RLT-PRECO TO RELATORIO
                           WRITE RELATORIO
                       END-IF

                       IF WS-CODIGO = WS-CODIGO-O
                           ADD 1 TO WS-CODIGO
                        END-IF

                   END-IF
               END-IF

           END-PERFORM

       END-IF.

       CLOSE PRECO-ANTIGO.
       CLOSE PRECO-NOVO.
       CLOSE RLT-PRECO.

            STOP RUN.
       END PROGRAM LISTA-PRODUTO.
