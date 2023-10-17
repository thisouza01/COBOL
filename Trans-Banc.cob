      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Trans-Banc.
      *================================================================*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WS-CONTA1.
           03 WS-BALANCE1  PIC 9(05) VALUE 1000.

       01 WS-CONTA2.
           03 WS-BALANCE2  PIC 9(05) VALUE 100.

       01 WS-OPCAO         PIC 9(02) VALUE ZEROS.
       01 WS-VALOR         PIC 9(05) VALUE ZEROS.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.
           PERFORM 1-INICIAR.
           PERFORM 2-PROCESSAR.
           PERFORM 3-FINALIZAR.
           STOP RUN.

       1-INICIAR.
           DISPLAY 'BEM VINDO AO BANCO'
           DISPLAY 'ESCOLHA A OPCAO DESEJADA: '
           DISPLAY '1 - TRANSFERENCIA'
           DISPLAY '2 - DEPOSITO'
           DISPLAY '3 - SAIR'
           ACCEPT WS-OPCAO.

       2-PROCESSAR.
           IF WS-OPCAO = 3
               DISPLAY 'ADEUS'
               STOP RUN
           ELSE
               EVALUATE WS-OPCAO
                   WHEN 1
                       DISPLAY 'TRANSFERENCIA'
                       PERFORM 11-VERIFICA-SALDO
                       PERFORM 12-TRANSFERE-VALOR
                   WHEN 2
                       DISPLAY 'DEPOSITO'.

       3-FINALIZAR.
           DISPLAY 'SALDO CONTA 1: ' WS-BALANCE1.
           DISPLAY 'SALDO CONTA 2: ' WS-BALANCE2.

       11-VERIFICA-SALDO.
           DISPLAY 'QUAL O VALOR:'
           ACCEPT WS-VALOR.

           IF WS-VALOR <= 0
               DISPLAY 'VALOR INVÁLIDO, DIGITE NOVAMENTE: '
               ACCEPT WS-VALOR
           END-IF.

           IF WS-VALOR > WS-BALANCE1
               DISPLAY 'VALOR PARA TRANSFERENCIA INSUFICIENTE'
               PERFORM 11-VERIFICA-SALDO
           END-IF.

       12-TRANSFERE-VALOR.
           SUBTRACT WS-VALOR FROM WS-BALANCE1
           ADD WS-VALOR TO WS-BALANCE2.
      *================================================================*
