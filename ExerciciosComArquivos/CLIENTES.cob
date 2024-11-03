      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.
      *----------------------------------------------------------------*
      * OBJETIVO: SISTEMA DE GESTAO DE CLIENTES
      * AUTHOR: THIAGO
      *----------------------------------------------------------------*

      *================================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CLIENTES ASSIGN TO 'C:\COBOL\CLIENTES.DAT'
             ORGANIZATION INDEXED
             ACCESS MODE IS DYNAMIC
             FILE STATUS IS CLIENTES-STATUS
             RECORD KEY CLIENTES-KEY.

           SELECT RELATO ASSIGN TO 'C:\COBOL\RELATOS.TXT'
           ORGANIZATION IS SEQUENTIAL.    
           
      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTES-REG.
           03 CLIENTES-KEY.
               05 CLIENTES-FONE PIC 9(09).
           03 CLIENTES-NAME     PIC X(30).
           03 CLIENTES-EMAIL    PIC X(40).
           
       FD RELATO.
       01 RELATO-REG.
           03 RELATO-DADOS      PIC X(79).

       WORKING-STORAGE SECTION.
       01 OPCAO            PIC X(01).
       01 MODULO           PIC X(25).
       01 TECLA            PIC X(02).
       01 OPCAO-RELATORIO  PIC X(01).
       01 CLIENTES-STATUS  PIC 9(02).
       01 ERRO             PIC X(30).
       01 CONTA-LINHA      PIC 9(03) VALUE ZEROS.
       01 QT-REGISTROS     PIC 9(04) VALUE ZEROS.
         
       SCREEN SECTION.
       01 TELA.
           03 LIMPA-TELA.
               05 BLANK SCREEN.
               05 LINE 01 COLUMN 01 PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 2.
               05 LINE 01 COLUMN 25 PIC X(20)
                    BACKGROUND-COLOR 2 FROM 'SISTEMA DE CLIENTES'.
               05 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                   BACKGROUND-COLOR 1 FROM MODULO.

       01 MENU.
           03 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
           03 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
           03 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
           03 LINE 10 COLUMN 15 VALUE '4 - EXCUIR'.
           03 LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
           03 LINE 12 COLUMN 15 VALUE 'X - SAIDA'.
           03 LINE 13 COLUMN 15 VALUE 'OPCAO: '.
           03 LINE 13 COLUMN 22 USING OPCAO.

       01 TELA-REGISTRO.
           03 CHAVE FOREGROUND-COLOR 2.
               05 LINE 10 COLUMN 10 VALUE 'TELEFONE: '.
               05 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.

           03 SS-DADOS.
               05 LINE 11 COLUMN 10 VALUE 'NOME: '.
               05 COLUMN PLUS 2 PIC X(30) USING CLIENTES-NAME.
               05 LINE 12 COLUMN 10 VALUE 'EMAIL: '.
               05 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.

       01 MOSTRA-ERRO.
           03 MSGN-ERRO.
               05 LINE 16 COLUMN 01 ERASE EOL BACKGROUND-COLOR 3.
               05 LINE 16 COLUMN 10 PIC X(40) BACKGROUND-COLOR 3
                   FROM ERRO.
               05 COLUMN PLUS 2     PIC X(01) BACKGROUND-COLOR 3
                   USING TECLA.



       01 MENU-RELATORIO.
           03 LINE 12 COLUMN 55 VALUE '1 - EM TELA'.
           03 LINE 13 COLUMN 55 VALUE '2 - EM DISCO'.
           03 LINE 14 COLUMN 55 VALUE 'OPCAO: '.
           03 LINE 14 COLUMN 62 USING OPCAO-RELATORIO.

      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
           PERFORM 1000-INICIAR THRU 1100-MONTA-TELA.
           PERFORM 2000-PROCESSAR UNTIL OPCAO EQUAL 'X'.
           PERFORM 3000-FINALIZAR.
           STOP RUN.

       1000-INICIAR.
           OPEN I-O CLIENTES
               IF CLIENTES-STATUS EQUAL 35 THEN
                   OPEN OUTPUT CLIENTES
                   CLOSE CLIENTES
                   OPEN I-O CLIENTES
               END-IF.

       1100-MONTA-TELA.
           MOVE 0 TO CONTADOR.
           DISPLAY TELA.
           ACCEPT MENU.

       2000-PROCESSAR.
           MOVE SPACES TO ERRO.
           EVALUATE OPCAO
               WHEN 1
                   PERFORM 0100-INCLUIR

               WHEN 2
                   PERFORM 0200-CONSULTAR
                   
               WHEN 3
                   PERFORM 0300-ALTERAR

               WHEN 4
                   PERFORM 0400-EXCLUIR

               WHEN 5
                   ACCEPT MENU-RELATORIO
                   IF OPCAO-RELATORIO EQUAL 1
                       PERFORM 0500-RELATORIO-TELA
                   ELSE
                       PERFORM 0510-RELATORIO-DISCO
                   END-IF

               WHEN OTHER
                   IF OPCAO NOT EQUAL 'X'
                       DISPLAY 'ENTRE COM A OPCAO CORRETA ' AT 1330
                       ACCEPT TECLA
                   END-IF

           END-EVALUATE.
           MOVE SPACES TO OPCAO
           PERFORM 1100-MONTA-TELA.
      *================================================================*
       3000-FINALIZAR.
           CLOSE CLIENTES.

      *================================================================*

      *----------------------------------------------------------------*
       0100-INCLUIR.
           MOVE 'MODULO - INCLUIR' TO MODULO.
           DISPLAY TELA.
           ACCEPT TELA-REGISTRO.
               WRITE CLIENTES-REG
                   INVALID KEY
                   MOVE 'JA EXISTE! NOVO REGISTRO?' TO ERRO
                   ACCEPT MOSTRA-ERRO
                   IF TECLA = 'n' OR TECLA = 'N'
                       MOVE ZEROS TO CLIENTES-FONE
                       PERFORM 0100-INCLUIR
                   END-IF
               END-WRITE.
               PERFORM 1100-MONTA-TELA.

      *----------------------------------------------------------------*
       0200-CONSULTAR.
           MOVE 'MODULO - CONSULTAR' TO MODULO.
           DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CHAVE.
             READ CLIENTES
               INVALID KEY
               MOVE 'NAO ENCONTRADO' TO ERRO
               NOT INVALID KEY
               MOVE 'ENCONTRADO' TO ERRO
               DISPLAY SS-DADOS
             END-READ.
             MOVE SPACES TO CLIENTES-NAME, CLIENTES-EMAIL.
             ACCEPT MOSTRA-ERRO.

      *----------------------------------------------------------------*
       0300-ALTERAR.
           MOVE 'MODULO - EXCLUSAO' TO MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
               READ CLIENTES
               IF CLIENTES-STATUS = 0
                   ACCEPT SS-DADOS
                   REWRITE CLIENTES-REG
                       IF CLIENTES-STATUS = 0
                           MOVE 'REGISTRO ALTERADO' TO ERRO
                           ACCEPT MOSTRA-ERRO
                       ELSE
                           MOVE 'REGISTRO NAO ALTERADO ' TO ERRO
                           ACCEPT MOSTRA-ERRO
                       END-IF   
               ELSE
                   MOVE 'REGISTRO NAO ENCONTRADO ' TO ERRO
                   ACCEPT MOSTRA-ERRO
               END-IF.    
                   


      *----------------------------------------------------------------*
       0400-EXCLUIR.
           MOVE 'MODULO - EXCLUSAO' TO MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
            ACCEPT CHAVE.
             READ CLIENTES
               INVALID KEY
                MOVE 'NAO ENCONTRADO' TO ERRO
               NOT INVALID KEY
                MOVE 'ENCONTRDO (S/N) ? ' TO ERRO
                DISPLAY SS-DADOS
             END-READ.
             ACCEPT MOSTRA-ERRO.
             IF TECLA = 'S' AND CLIENTES-STATUS = 0
                 DELETE CLIENTES
                 INVALID KEY
                  MOVE 'NAO EXLUIDO ' TO ERRO
                  ACCEPT MOSTRA-ERRO
                 END-DELETE
             END-IF.
                 
      *----------------------------------------------------------------*
       0500-RELATORIO-TELA.
           MOVE 'MODULO - RELATORIO' TO MODULO.
           ACCEPT CHAVE.
           DISPLAY TELA.
           START CLIENTES KEY EQUAL CLIENTES-FONE.
           READ CLIENTES
               INVALID KEY
                   MOVE 'NENHUM REGISTRO ENCONTRADO ' TO ERRO
               NOT INVALID KEY 
                   DISPLAY '         RELATORIO DE CLIENTES          '
                   DISPLAY '----------------------------------------'
                   
                   PERFORM UNTIL CLIENTES-STATUS = 10
                       ADD 1 TO QT-REGISTROS
                       DISPLAY CLIENTES-FONE ' ' 
                               CLIENTES-NAME ' '  
                               CLIENTES-EMAIL    
                       READ CLIENTES NEXT
                           ADD 1 TO CONTA-LINHA
                       IF CONTA-LINHA = 5
                           ACCEPT MOSTRA-ERRO
                           MOVE 'PRESSIONE ALGUMA TECLA' TO ERRO
                           ACCEPT MOSTRA-ERRO
                           MOVE 'MODULO - RELATORIO' TO MODULO
                           DISPLAY TELA
                           DISPLAY '      RELATORIO DE CLIENTES     '
                           DISPLAY '-----------------------------------'
                           MOVE 0 TO CONTA-LINHA
                           
                       END-IF
                   END-PERFORM
                   
           END-READ.
           MOVE 'REGISTROS LIDOS ' TO ERRO.
           MOVE QT-REGISTROS TO ERRO(17:05).
           ACCEPT MOSTRA-ERRO.    
           
              
      *----------------------------------------------------------------*
       0510-RELATORIO-DISCO.
           MOVE 'MODULO - RELATORIO' TO MODULO.
           ACCEPT CHAVE.
           DISPLAY TELA.
           START CLIENTES KEY EQUAL CLIENTES-FONE.
           READ CLIENTES
           
               INVALID KEY
                   MOVE 'NENHUM REGISTRO ENCONTRADO ' TO ERRO
              
               NOT INVALID KEY
               OPEN OUTPUT RELATO
                   PERFORM UNTIL CLIENTES-STATUS = 10
                       ADD 1 TO QT-REGISTROS
                       MOVE CLIENTES-REG TO RELATO-REG
                       WRITE RELATO-REG
                       READ CLIENTES NEXT
                   END-PERFORM
                   MOVE 'REGISTROS LIDOS ' TO RELATO-REG
                   MOVE QT-REGISTROS       TO RELATO-REG(18:05) 
                   WRITE RELATO-REG
                   CLOSE RELATO
           END-READ.
           ACCEPT MOSTRA-ERRO. 
      *================================================================*
