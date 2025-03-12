      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TRANSACOES.
      *Dado um arquivo sequencial de transações financeiras contendo
      * informações como código da transação, data, valor e tipo de
      * transação (débito/crédito), processe os registros em blocos de
      * 5. Para cada bloco, calcule o saldo total (débito - crédito)
      * e exiba o saldo no final de cada bloco de 5 registros
      * processados
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
           SELECT TRANSACOES ASSIGN TO
            "C:\teste-vscode-cobol\trans-client.csv"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS-TRANS.

       DATA DIVISION.
       FILE SECTION.
         FD TRANSACOES.
         01 REGISTRO             PIC X(31).

       WORKING-STORAGE SECTION.
         01 WS-REGISTRO.
            05 WS-TRANSID        PIC X(04).
            05 WS-DATA           PIC X(11).
            05 WS-VALOR          PIC 9(03)V99.
            05 WS-TRANSTP        PIC A(07).

         01 AUX.
            05 EOF               PIC X(01) VALUE 'N'.
            05 VALOR-CREDITO     PIC S9(05)V99 VALUE ZEROS.
            05 VALOR-DEBITO      PIC S9(05)V99 VALUE ZEROS.
            05 SALDO-BLOCO       PIC S9(06)V99 VALUE ZEROS.

         01 STATS.
            05 WS-FS-TRANS       PIC X(02).

       PROCEDURE DIVISION.
       0001-MAIN.
           PERFORM 0100-ABRE-ARQUIVO THRU 0100-ABRE-ARQUIVO-EXIT.
           PERFORM 0200-LE-ARQUIVO THRU 0200-LE-ARQUIVO-EXIT 5 TIMES.
           PERFORM 0290-MOSTRA-SALDO THRU 0290-MOSTRA-SALDO-EXIT.
           PERFORM 0200-LE-ARQUIVO THRU 0200-LE-ARQUIVO-EXIT 5 TIMES.
           PERFORM 0290-MOSTRA-SALDO THRU 0290-MOSTRA-SALDO-EXIT.
           PERFORM 0300-FECHA-ARQUIVO THRU 0300-FECHA-ARQUIVO-EXIT.
           STOP RUN.

       0100-ABRE-ARQUIVO.
           OPEN INPUT TRANSACOES.
       0100-ABRE-ARQUIVO-EXIT. EXIT.

      *LER 5 VEZES O ARQUIVO

       0200-LE-ARQUIVO.
           IF WS-FS-TRANS = 00
               READ TRANSACOES INTO WS-REGISTRO
               AT END MOVE 'Y' TO EOF
               NOT AT END
                  UNSTRING REGISTRO
                  DELIMITED BY ','
                  INTO
                       WS-TRANSID
                       WS-DATA
                       WS-VALOR
                       WS-TRANSTP
               END-READ
           END-IF.

           DISPLAY 'WS-TRANSID: 'WS-TRANSID.
           DISPLAY 'WS-DATA: 'WS-DATA.
           DISPLAY 'WS-VALOR: 'WS-VALOR.
           DISPLAY 'WS-TRANSTP: 'WS-TRANSTP.
           DISPLAY '-----------------------'.

           PERFORM 0250-PROCESSA-BLOCO.
       0200-LE-ARQUIVO-EXIT. EXIT.

       0250-PROCESSA-BLOCO.
           EVALUATE WS-TRANSTP
           WHEN = 'CREDITO'
            ADD WS-VALOR TO VALOR-CREDITO
           WHEN = 'DEBITO'
            ADD WS-VALOR TO VALOR-DEBITO
           WHEN OTHER
            DISPLAY 'INVALIDO'
           END-EVALUATE.

           COMPUTE SALDO-BLOCO = VALOR-DEBITO - VALOR-CREDITO.
       0250-PROCESSA-BLOCO-EXIT. EXIT.

       0290-MOSTRA-SALDO.
           DISPLAY 'SALDO BLOCO: 'SALDO-BLOCO.
           DISPLAY '======================='
           MOVE ZEROS TO VALOR-CREDITO.
           MOVE ZEROS TO VALOR-DEBITO.
           MOVE ZEROS TO SALDO-BLOCO.
       0290-MOSTRA-SALDO-EXIT. EXIT.

       0300-FECHA-ARQUIVO.
           CLOSE TRANSACOES.
       0300-FECHA-ARQUIVO-EXIT. EXIT.

       END PROGRAM TRANSACOES.
