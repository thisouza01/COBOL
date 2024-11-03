      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Caixa-Eletronico.
      *================================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WS-SALDO-CONTA     PIC 9(05)V99 VALUE 1000,00.

       01 WS-ESCOLHA         PIC 9(02)    VALUE ZEROS.
       01 WS-VALOR           PIC 9(05)V99 VALUE ZEROS.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.
           PERFORM UNTIL WS-ESCOLHA = 4
               PERFORM 1-INICIAR
               PERFORM 2-PROCESSAR
               PERFORM 3-FINALIZAR
           END-PERFORM.
           STOP RUN.

       1-INICIAR.
               DISPLAY 'CAIXA ELETRONICO '
               DISPLAY 'ESCOLHA UMA OPCAO: '
               DISPLAY '1 - SAQUE'
               DISPLAY '2 - DEPOSITO'
               DISPLAY '3 - CONSULTA AO SALDO'
               DISPLAY '4 - SAIR'
               ACCEPT WS-ESCOLHA.

       2-PROCESSAR.
           EVALUATE WS-ESCOLHA
               WHEN 1
                   DISPLAY 'SAQUE'
                   DISPLAY 'QUAL O VALOR DO SAQUE?'
                   ACCEPT WS-VALOR
                   PERFORM 21-VALIDA-SALDO
                   PERFORM 211-SAQUE

               WHEN 2
                   DISPLAY 'DEPOSITO'
                   DISPLAY 'QUAL O VALOR DO DEPOSITO?'
                   ACCEPT WS-VALOR
                   PERFORM 22-DEPOSITO

               WHEN 3
                   DISPLAY 'CONSULTA AO SALDO'
                   DISPLAY 'SEU SALDO E: ' WS-SALDO-CONTA

               WHEN 4
                   DISPLAY 'OBRIGADO'

               WHEN OTHER
                   DISPLAY 'OPCAO INVALIDA, TENTE OUTRA'
           END-EVALUATE.

       3-FINALIZAR.
           DISPLAY 'SEU NOVO SALDO E: ' WS-SALDO-CONTA.

       21-VALIDA-SALDO.
           IF WS-VALOR <= 0
               DISPLAY 'VALOR INVALIDO'
               ACCEPT WS-VALOR
               PERFORM 21-VALIDA-SALDO
           END-IF.

           IF WS-VALOR > WS-SALDO-CONTA
               DISPLAY 'SALDO INSUFICIENTE.'
               ACCEPT WS-VALOR
               PERFORM 21-VALIDA-SALDO
           END-IF.

       211-SAQUE.
           SUBTRACT WS-VALOR FROM WS-SALDO-CONTA.
           DISPLAY 'SAQUE REALIZADO'.

       22-DEPOSITO.
           ADD WS-VALOR TO WS-SALDO-CONTA.
           DISPLAY 'DEPOSITO REALIZADO'.
      *================================================================*
