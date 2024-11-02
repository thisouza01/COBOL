      ******************************************************************
      * Author:
      * Date:
      * Purpose: Processar dois arquivos de transações para um mesmo
      *  cliente, sendo um arquivo de transações realizadas pelo cliente
      *  e outro de transações lançadas no sistema. O exercício envolve
      *  identificar discrepâncias entre os arquivos e conciliar as
      * transações.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCILIA-TRANS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT TRANS-REG ASSIGN TO
                "C:\COBOL-exercicios\trans-reg.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-REG.

               SELECT TRANS-CLI ASSIGN TO
                "C:\COBOL-exercicios\transa-cli.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-CLI.

       DATA DIVISION.
       FILE SECTION.
       FD TRANS-REG.
       01 REGISTRO1                PIC X(32).

       FD TRANS-CLI.
       01 REGISTRO2                PIC X(35).

       WORKING-STORAGE SECTION.
       01 WS-REGISTRO1.
           05 ID-CLIENTE1          PIC 9(05).
           05 DATA-TRANS1          PIC X(10).
           05 VALOR-TRANS1         PIC S9(04)V99.
           05 STATUS1              PIC A(10).

       01 WS-REGISTRO2.
           05 ID-CLIENTE2          PIC 9(05).
           05 DATA-TRANS2          PIC X(10).
           05 TIPO-TRANS           PIC A(08).
           05 VALOR-TRANS2         PIC S9(04)V99.

       01 AUX.
           05 EOF1                 PIC X(01) VALUE 'N'.
           05 EOF2                 PIC X(01) VALUE 'N'.
           05 FLAG-DATA            PIC X(01) VALUE 'N'.
           05 FLAG-VALOR           PIC X(01) VALUE 'N'.
           05 FLAG-STATUS          PIC X(01) VALUE 'N'.
           05 FLAG-VALIDA          PIC X(01) VALUE 'N'.
           05 CNT-CONCILIADO       PIC 9(02) VALUE 0.
           05 CNT-NAO-CONCILIADO   PIC 9(02) VALUE 0.
           05 VALOR-TOTAL          PIC 9(05)V99 VALUE ZEROS.

       01 WS-STATUS.
           05 FS-REG               PIC 9(02).
           05 FS-CLI               PIC 9(02).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       PERFORM 0100-ABRE-ARQUIVIO.
       PERFORM UNTIL EOF1 = 'Y' OR EOF2 = 'Y'
           PERFORM 0200-LE-ARQUIVO1
           PERFORM 0210-LE-ARQUIVO2
           IF EOF1 = 'N' OR EOF2 = 'N'
               PERFORM 0300-UNSTRING-REG1
               PERFORM 0310-UNSTRING-REG2
               PERFORM 0400-VALIDA-TRANSACAO
               PERFORM 0500-CALCULA-VALOR-TOTAL
               IF FLAG-VALIDA = 'Y'
                   PERFORM 0600-MOSTRA-RLT-CONCI
               END-IF
           END-IF
       END-PERFORM.
       PERFORM 0700-MOSTRA-TOTAL.
       PERFORM 1000-FECHA-ARQUIVO.

            STOP RUN.

       0100-ABRE-ARQUIVIO.
           OPEN INPUT TRANS-REG, TRANS-CLI.

       0200-LE-ARQUIVO1.
           IF FS-REG = 00
               READ TRANS-REG INTO WS-REGISTRO1
               AT END MOVE 'Y' TO EOF1
           END-IF.

       0210-LE-ARQUIVO2.
           IF FS-CLI = 00
               READ TRANS-CLI INTO WS-REGISTRO2
               AT END MOVE 'Y' TO EOF2
           END-IF.

       0300-UNSTRING-REG1.
           UNSTRING REGISTRO1
               DELIMITED BY ','
               INTO
                   ID-CLIENTE1
                   DATA-TRANS1
                   TIPO-TRANS
                   VALOR-TRANS1
           END-UNSTRING.

       0310-UNSTRING-REG2.
           UNSTRING REGISTRO2
               DELIMITED BY ','
               INTO
                   ID-CLIENTE2
                   DATA-TRANS2
                   VALOR-TRANS2
                   STATUS1
           END-UNSTRING.

      * 0320-MOSTRA-REGISTRO1.
      *     DISPLAY 'REGISTRO 1'.
      *     DISPLAY 'ID CLIENTE: 'ID-CLIENTE1.
      *     DISPLAY 'DATA DA TRANSACAO: 'DATA-TRANS1.
      *     DISPLAY 'VALOR DA TRANSACAO: 'VALOR-TRANS1.
      *     DISPLAY 'TIPO DA TRANSACAO: 'TIPO-TRANS.
      *     DISPLAY '--------------------------------'.

      * 0330-MOSTRA-REGISTRO2.
      *     DISPLAY 'REGISTRO 2'.
      *     DISPLAY 'ID CLIENTE: 'ID-CLIENTE2.
      *     DISPLAY 'DATA DA TRANSACAO: 'DATA-TRANS2.
      *     DISPLAY 'VALOR DA TRANSACAO: 'VALOR-TRANS2.
      *     DISPLAY 'STATUS DA TRANSACAO: 'STATUS1.
      *     DISPLAY '///////////////////////////////////'.

       0400-VALIDA-TRANSACAO.
           MOVE 'N' TO FLAG-VALIDA
           IF ID-CLIENTE1 = ID-CLIENTE2
               PERFORM 0410-VALIDA-DATA-TRAN
               IF FLAG-DATA = 'Y'
                   PERFORM 0420-VALIDA-VALOR
                   IF FLAG-VALOR = 'Y'
                       PERFORM 0430-VALIDA-STATUS
                       IF FLAG-STATUS = 'Y'
                           MOVE 'Y' TO FLAG-VALIDA
                       END-IF
                   END-IF
               END-IF
           END-IF.

       0410-VALIDA-DATA-TRAN.
           IF DATA-TRANS1 = DATA-TRANS2
               MOVE 'Y' TO FLAG-DATA
           ELSE
               MOVE 'N' TO FLAG-DATA
           END-IF.

       0420-VALIDA-VALOR.
           IF VALOR-TRANS1 = VALOR-TRANS2
               MOVE 'Y' TO FLAG-VALOR
           ELSE
               MOVE 'N' TO FLAG-VALOR
           END-IF.

       0430-VALIDA-STATUS.
           IF STATUS1 = 'Confirmado'
               MOVE 'Y' TO FLAG-STATUS
           ELSE
               MOVE 'N' TO FLAG-STATUS
           END-IF.

       0500-CALCULA-VALOR-TOTAL.
           EVALUATE FLAG-VALIDA
               WHEN = 'Y'
                   ADD VALOR-TRANS1 TO VALOR-TOTAL
                   ADD 1 TO CNT-CONCILIADO
               WHEN = 'N'
                   ADD 1 TO CNT-NAO-CONCILIADO
               WHEN OTHER
                   DISPLAY 'ERRO'
           END-EVALUATE.

       0600-MOSTRA-RLT-CONCI.
           DISPLAY '-----------------------------------------'.
           DISPLAY 'ID CLIENTE: 'ID-CLIENTE1
           DISPLAY 'DATA DA TRANSACAO: 'DATA-TRANS1.
           DISPLAY 'VALOR DA TRANSACAO: 'VALOR-TRANS1.
           DISPLAY 'TIPO DA TRANSACAO: 'TIPO-TRANS.
           DISPLAY 'STATUS DA TRANSACAO: 'STATUS1.

       0700-MOSTRA-TOTAL.
           DISPLAY '============================================'.
           DISPLAY 'VALOR TOTAL DE TRANSACOES CONCILIADAS: 'VALOR-TOTAL.
           DISPLAY 'TRANSACOES CONCILIADAS: 'CNT-CONCILIADO.
           DISPLAY 'TRANSACOES NAO CONCILIADAS: 'CNT-NAO-CONCILIADO.

       1000-FECHA-ARQUIVO.
           CLOSE TRANS-REG, TRANS-CLI.

       END PROGRAM CONCILIA-TRANS.
