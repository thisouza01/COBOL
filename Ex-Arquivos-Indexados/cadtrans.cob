      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADTRANS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD-TRANS ASSIGN TO "C:\Financas\cad-trans.txt"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WK-FS-TRANS.

           SELECT CADTRANS ASSIGN TO "C:\Financas\cadtrans.dat"
           ORGANIZATION IS INDEXED
           RECORD KEY IS FD-IDCONTA
           FILE STATUS IS WK-FS-CADTRANS.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
           FD CADTRANS.
           01 REG-CADTRANS.
               03 FD-DATA              PIC 9(08).
               03 FD-TIPO              PIC 9.
               03 FD-VALOR             PIC 9(5)V99.
               03 FD-DESCR             PIC X(5).
               03 FD-IDCONTA           PIC 9(04).

           FD CAD-TRANS.
           01 REG-CAD-TRANS.
               03 FD-DATA              PIC 9(08).
               03 FD-TIPO              PIC 9.
               03 FD-VALOR             PIC 9(5)V99.
               03 FD-DESCR             PIC X(5).
               03 FD-IDCONTA-SEQ       PIC 9(04).

       WORKING-STORAGE SECTION.
       77  WK-FS-TRANS             PIC X(2)      VALUE SPACES.
       77  WK-FS-CADTRANS          PIC X(2)      VALUE SPACES.
      ******************************************************************
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0010-PRINCIPAL.
           PERFORM 0110-OPEN-DATA.
           PERFORM 0210-VALIDATE-CAD-TRANS-OPEN  THRU
                   0220-VALIDATE-CADTRANS-OPEN.
           PERFORM 0310-PROCESS-DATA UNTIL WK-FS-TRANS  EQUAL "10".
           PERFORM 0410-CLOSE-DATA.

      ******************************************************************
       0100-OPEN-DATA SECTION.
       0110-OPEN-DATA.
           OPEN INPUT  CAD-TRANS.
           OPEN I-O    CADTRANS.
           IF WK-FS-CADTRANS = "35"
               OPEN OUTPUT CADTRANS.

       0200-VALIDATE-DATA SECTION.
       0210-VALIDATE-CAD-TRANS-OPEN.
           EVALUATE WK-FS-TRANS
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-TRANS
                           " NA ABERTURA DO ARQUIVO CAD-TRANS"
                   STOP RUN
           END-EVALUATE.
       0220-VALIDATE-CADTRANS-OPEN.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA ABERTURA DO ARQUIVO CADTRANS"
                   STOP RUN
           END-EVALUATE.
       0230-VALIDATE-CAD-TRANS-READ.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-TRANS
                           " NA LEITURA DO ARQUIVO CAD-TRANS"
                   STOP RUN
           END-EVALUATE.
       0240-VALIDATE-CADTRANS-WRITE.
           EVALUATE WK-FS-CADTRANS
               WHEN "00"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WK-FS-CADTRANS
                           " NA ESCRITA DO ARQUIVO CADTRANS"
                   STOP RUN
           END-EVALUATE.
       0300-PROCESS-DATA SECTION.
       0310-PROCESS-DATA.
           READ CAD-TRANS.
           PERFORM 0230-VALIDATE-CAD-TRANS-READ.
           DISPLAY REG-CAD-TRANS.
           MOVE REG-CAD-TRANS TO REG-CADTRANS.
           DISPLAY REG-CADTRANS.
           IF FD-IDCONTA = SPACES OR FD-IDCONTA = ZERO
                DISPLAY "CHAVE INV�LIDA: " FD-IDCONTA
                STOP RUN
           END-IF
           WRITE REG-CADTRANS.
           PERFORM 0240-VALIDATE-CADTRANS-WRITE.
           DISPLAY 'GRAVADO COM SUCESSO'.
       0400-CLOSE-DATA SECTION.
       0410-CLOSE-DATA.
           CLOSE CAD-TRANS.
           CLOSE CADTRANS.
           IF WK-FS-CADTRANS <> "00" THEN
               DISPLAY "ERRO AO FECHAR O ARQUIVO:" WK-FS-CADTRANS
           ELSE
               DISPLAY "ARQUIVO FECHADO COM SUCESSO".
       0500-END-PROGRAM SECTION.
       0510-END-PROGRAM.
           END PROGRAM CADTRANS.
