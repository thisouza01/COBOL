      ******************************************************************
      * Author:
      * Date:
      * Purpose: Filtrar Registros de Clientes
      *  Crie um programa que leia um arquivo de clientes e filtre
      *  apenas aqueles que estão ativos (Status = 'A'). O arquivo deve
      *  conter campos como código do cliente, nome, e-mail, telefone e
      *  status. Gere um novo arquivo com apenas os clientes ativos.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-CLIENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

           FILE-CONTROL.

               SELECT ARQ-CLIENTE ASSIGN TO
                "C:\COBOL-exercicios\arq-cliente.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-CLIENT.

               SELECT CLIENTE-RLT ASSIGN TO
                "C:\COBOL-exercicios\arq-cliente-output.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-RELATORIO.


       DATA DIVISION.
       FILE SECTION.

           FD ARQ-CLIENTE.
           01 FD-CLIENTES          PIC X(38).

           FD CLIENTE-RLT.
           01 FD-RELATORIO         PIC X(50).

       WORKING-STORAGE SECTION.

           01 WS-CLIENTE           PIC X(38).

           01 WS-CLIENTE-OUT.
               05 WS-ID-O          PIC 9(04).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-NOME-O        PIC A(10).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-EMAIL-O       PIC X(20).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-TELEFONE-O    PIC 9(11).
               05 FILLER           PIC X(01) VALUE SPACES.
               05 WS-STATUS-O      PIC A(01).

           01 AUX.
               05 WS-FS-CLIENT     PIC 9(02).
               05 WS-FS-RELATORIO  PIC 9(02).
               05 WS-EOF           PIC A(01) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT ARQ-CLIENTE.
           OPEN OUTPUT CLIENTE-RLT.

           IF WS-FS-CLIENT = 00

               PERFORM UNTIL WS-EOF = 'Y'

                   READ ARQ-CLIENTE INTO WS-CLIENTE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END

                       UNSTRING
                               WS-CLIENTE DELIMITED BY ','
                       INTO
                               WS-ID-O
                               WS-NOME-O
                               WS-EMAIL-O
                               WS-TELEFONE-O
                               WS-STATUS-O
                       END-UNSTRING

                       DISPLAY WS-CLIENTE-OUT
                       DISPLAY '---------------'

                       IF WS-FS-RELATORIO = '00'
                           IF WS-STATUS-O = 'A'

                               MOVE WS-CLIENTE-OUT TO FD-RELATORIO
                               WRITE FD-RELATORIO
                           END-IF

                       END-IF

               END-PERFORM

           END-IF

           CLOSE ARQ-CLIENTE.
           CLOSE CLIENTE-RLT.

            STOP RUN.
       END PROGRAM FILTER-CLIENT.
