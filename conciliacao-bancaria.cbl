      ******************************************************************
      * Author:
      * Date:
      * Purpose: Dado dois arquivos, um com transações bancárias e
      *  outro com o extrato de um banco, leia ambos e gere um relatório
      *  de conciliação que liste as transações não conciliadas. Exiba
      *  a diferença encontrada entre o saldo esperado (calculado a
      *  partir das transações) e o saldo real informado no extrato
      *  bancário. Ao final, gere um relatório com as transações
      *  divergentes.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCILIACAO-BANCARIA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT TRANS-BANCO ASSIGN TO
                "C:\exe-cobol\trans-bancaria.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-TRANS.

               SELECT EXTRATO-BANCO ASSIGN TO
                "C:\exe-cobol\extrato-bancario.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-EXTRATO.

       DATA DIVISION.
       FILE SECTION.
           FD TRANS-BANCO.
           01 REGISTRO1                PIC X(33).

           FD EXTRATO-BANCO.
           01 REGISTRO2                PIC X(42).

       WORKING-STORAGE SECTION.
           01 WS-REGISTRO1.
               05 WS-TRANSID           PIC X(04).
               05 WS-DATE1             PIC X(10).
               05 WS-VALOR1            PIC 9(04)V99.
               05 WS-TIPO1             PIC A(01).
               05 WS-CNTCORRENTE1      PIC 9(06).

           01 WS-REGISTRO2.
               05 WS-CNTCORRENTE2      PIC 9(06).
               05 WS-DATE2             PIC X(10).
               05 WS-VALOR2            PIC 9(04)V99.
               05 WS-TIPO2             PIC A(01).
               05 WS-IDBANCO           PIC 9(03).

           01 AUX.
               05 EOF1                 PIC X(01) VALUE 'N'.
               05 EOF2                 PIC X(01) VALUE 'N'.
               05 CABECALHO1-LIDO      PIC X(05) VALUE 'FALSE'.
               05 CABECALHO2-LIDO      PIC X(05) VALUE 'FALSE'.

           01 WS-STATUS.
               05 WS-FS-TRANS          PIC 9(02).
               05 WS-FS-EXTRATO        PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO THRU 0100-ABRE-ARQUIVO-EXIT.
           PERFORM 0200-LE-ARQUIVO UNTIL EOF2 = 'Y'.
           PERFORM 1000-FECHA-ARQUIVO THRU 1000-FECHA-ARQUIVO-EXIT.

           STOP RUN.
       0100-ABRE-ARQUIVO.
           OPEN INPUT TRANS-BANCO, EXTRATO-BANCO.
       0100-ABRE-ARQUIVO-EXIT. EXIT.

       0200-LE-ARQUIVO.

           IF WS-FS-TRANS = 00 AND WS-FS-EXTRATO = 00
               READ TRANS-BANCO INTO WS-REGISTRO1
                   AT END MOVE 'Y' TO EOF1
                   NOT AT END
                   IF CABECALHO1-LIDO = 'FALSE'
                       MOVE 'Y' TO CABECALHO1-LIDO
                   ELSE
                       PERFORM 0300-UNSTRING-ARQUIVO1
                       DISPLAY WS-TRANSID
                       DISPLAY WS-DATE1
                       DISPLAY WS-VALOR1
                       DISPLAY WS-TIPO1
                       DISPLAY WS-CNTCORRENTE1
                   END-IF
               END-READ


      * AMBOS OS JEITOS FUNCIONARAM, INTERESSANTE
               READ EXTRATO-BANCO INTO WS-REGISTRO2
                   AT END MOVE 'Y' TO EOF2
               END-READ

               PERFORM 0310-UNSTRING-ARQUIVO2
               IF CABECALHO2-LIDO = 'FALSE'
                   MOVE 'Y' TO CABECALHO2-LIDO
               ELSE
                   DISPLAY WS-REGISTRO2
                   DISPLAY '-----------------'
               END-IF
           END-IF.
       0200-LE-ARQUIVO-EXIT. EXIT.

       0300-UNSTRING-ARQUIVO1.
           UNSTRING REGISTRO1
               DELIMITED BY ','
               INTO
                   WS-TRANSID
                   WS-DATE1
                   WS-VALOR1
                   WS-TIPO1
                   WS-CNTCORRENTE1
           END-UNSTRING.
       0300-UNSTRING-ARQUIVO1-EXIT. EXIT.

       0310-UNSTRING-ARQUIVO2.
           UNSTRING REGISTRO2
               DELIMITED BY ','
               INTO
                   WS-CNTCORRENTE2
                   WS-DATE2
                   WS-VALOR2
                   WS-TIPO2
                   WS-IDBANCO
           END-UNSTRING.
       0300-UNSTRING-ARQUIVO2-EXIT. EXIT.

       1000-FECHA-ARQUIVO.
           CLOSE TRANS-BANCO, EXTRATO-BANCO.
       1000-FECHA-ARQUIVO-EXIT. EXIT.


       END PROGRAM CONCILIACAO-BANCARIA.
