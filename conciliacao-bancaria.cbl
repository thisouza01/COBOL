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
               05 COMP-TRAN            PIC X(01) VALUE 'N'.
               05 SALDO-ESPERADO       PIC 9(05)V99 VALUE ZEROS.
               05 SALDO-REAL           PIC 9(05)V99 VALUE ZEROS.
               05 WS-TRANSID-O         PIC X(04).

           01 WS-STATUS.
               05 WS-FS-TRANS          PIC 9(02).
               05 WS-FS-EXTRATO        PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 0100-ABRE-ARQUIVO THRU 0100-ABRE-ARQUIVO-EXIT.
      * LENDO O ARQUIVO PARA LER O CABEÇALHO ANTES, PRECISO ENTENDER
      *  COMO TIRAR O CABEÇALHO SEM FAZER ISSO
           PERFORM 0200-LE-ARQUIVO1 THRU 0210-LE-ARQUIVO2-EXIT 2 TIMES.
      * VERIFICA SE CHEGOU AO FINAL DOS DOIS ARQUIVOS
           PERFORM UNTIL EOF1 = 'Y' AND EOF2 = 'Y'
               IF EOF1 = 'N'
                   PERFORM 0300-UNSTRING-ARQUIVO1
               END-IF
               IF EOF2 = 'N'
                   PERFORM 0310-UNSTRING-ARQUIVO2
               END-IF

               PERFORM 0400-COMPARA-TRANSACAO
               PERFORM 0500-CALCULA-SALDO-ESP

               IF EOF1 = 'N'
                   PERFORM 0200-LE-ARQUIVO1
               END-IF
               IF EOF2 = 'N'
                   PERFORM 0210-LE-ARQUIVO2
               END-IF
           END-PERFORM.
           PERFORM 0600-MOSTRA-DIVERGENTE.    
           PERFORM 1000-FECHA-ARQUIVO THRU 1000-FECHA-ARQUIVO-EXIT.

           STOP RUN.
       0100-ABRE-ARQUIVO.
           OPEN INPUT TRANS-BANCO, EXTRATO-BANCO.
       0100-ABRE-ARQUIVO-EXIT. EXIT.

       0200-LE-ARQUIVO1.
           IF WS-FS-TRANS = 00
               READ TRANS-BANCO INTO WS-REGISTRO1
                   AT END
                   MOVE 'Y' TO EOF1
               END-READ
           END-IF.
       0200-LE-ARQUIVO1-EXIT. EXIT.

       0210-LE-ARQUIVO2.
           IF WS-FS-EXTRATO = 00
               READ EXTRATO-BANCO INTO WS-REGISTRO2
                   AT END MOVE 'Y' TO EOF2
               END-READ
           END-IF.
       0210-LE-ARQUIVO2-EXIT. EXIT.

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

       0400-COMPARA-TRANSACAO.
      * WS-TRANSID(2:3) POR QUE ELE COMECA COM UM T. EX: T001 E A
      *  VERIFICAÇÃO DO ID É APENAS OS NUMEROS
           EVALUATE WS-TRANSID(2:3)
           WHEN = WS-IDBANCO
               DISPLAY 'TRANSACAO: 'WS-TRANSID
               ADD WS-VALOR2 TO SALDO-REAL
               DISPLAY 'SALDO REAL DA 'WS-TRANSID' E: 'SALDO-REAL
               MOVE ZEROS TO SALDO-REAL
           WHEN NOT = WS-IDBANCO
               DISPLAY 'TRASACAO INEXISTENTE'
               DISPLAY 'WS-TRANSID: 'WS-TRANSID
               MOVE 'Y' TO COMP-TRAN
               MOVE WS-TRANSID TO WS-TRANSID-O
           END-EVALUATE.
       0400-COMPARA-TRANSACAO-EXIT. EXIT.

       0500-CALCULA-SALDO-ESP.
           IF COMP-TRAN = 'Y'        
               ADD WS-VALOR1 TO SALDO-ESPERADO
               DISPLAY 'SALDO-ESPERADO: 'SALDO-ESPERADO
               MOVE 'N' TO COMP-TRAN
           END-IF.
       0500-CALCULA-SALDO-ESP-EXIT. EXIT.
       
       0600-MOSTRA-DIVERGENTE.
           DISPLAY 'Transacoes Divergentes'.
           DISPLAY 'Numero da transacao: 'WS-TRANSID-O.
           DISPLAY 'Valor esperado da transacao: 'SALDO-ESPERADO.
       0600-MOSTRA-DIVERGENTE-EXIT. EXIT.    

       1000-FECHA-ARQUIVO.
           CLOSE TRANS-BANCO, EXTRATO-BANCO.
       1000-FECHA-ARQUIVO-EXIT. EXIT.


       END PROGRAM CONCILIACAO-BANCARIA.
