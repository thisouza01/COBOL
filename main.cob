      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
      ****************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT ARQ-CLIENTES ASSIGN TO
               "C:\Users\WIN 11\OneDrive\Desktop\COBOL\clientes.txt"
           FILE STATUS IS WS-FS-CLIENTES.
      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION. 
      * ARQUIVO COM TAMANHO DE 26 
       FD  ARQ-CLIENTES.
       01  REG-CLIENTE.
           03 FD-CODIGO         PIC 9(04).
           03 FD-NOME           PIC A(15).
           03 FD-SALDO          PIC 9(5)V99.           
      *     
       WORKING-STORAGE SECTION.
       77  WS-FS-CLIENTES       PIC XX      VALUE SPACES.

       01  WS-CLIENTE.
           03 WS-CODIGO         PIC 9(04).
           03 WS-NOME           PIC A(15).
           03 WS-SALDO          PIC 9(5)V99.
      ****************************************************************** 
       PROCEDURE DIVISION.
       0000-PRINCIPAL SECTION.
       0001-PRINCIPAL.
           PERFORM 0101-INICIAR.
           PERFORM 0201-PROCESSAR UNTIL WS-FS-CLIENTES = "10".
           PERFORM 0901-FINALIZAR.
           STOP RUN.
      ****************************************************************** 
      * ABRIR ARQ-CLIENTES EM MODO DE LEITURA 
       0101-INICIAR.
           OPEN INPUT ARQ-CLIENTES.
           EVALUATE WS-FS-CLIENTES
               WHEN "00"
                   CONTINUE
               WHEN "35"
                   DISPLAY "ARQUIVO NAO ENCONTRADO"
                   MOVE 12 TO RETURN-CODE 
                   STOP RUN
               WHEN OTHER
                   DISPLAY "ERRO: " WS-FS-CLIENTES
                           " NO COMANDO OPEN VENDAS" 
                   MOVE 12 TO RETURN-CODE 
                   STOP RUN                
           END-EVALUATE.   
      ****************************************************************** 
       0200-PROCESSAR SECTION.
       0201-PROCESSAR.
           PERFORM 0301-LER-CLIENTE.           
      ****************************************************************** 
       0300-LER-CLIENTE SECTION.
       0301-LER-CLIENTE.       
           READ ARQ-CLIENTES.
           EVALUATE WS-FS-CLIENTES 
               WHEN "00"
                   MOVE REG-CLIENTE TO WS-CLIENTE 
                   DISPLAY WS-CLIENTE
                   INITIALIZE WS-CLIENTE
               WHEN "10"
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERRO: " WS-FS-CLIENTES
                           " NO COMANDO READ CLIENTES"
                   MOVE 12 TO RETURN-CODE
                   STOP RUN                                      
           END-EVALUATE.
           
   
      ****************************************************************** 
       0901-FINALIZAR.
           CLOSE ARQ-CLIENTES.
