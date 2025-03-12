 ******************************************************************
 * Author:
 * Date:
 * Purpose:Leia dois arquivos diferentes, um contendo os dados 
 *  pessoais dos funcionários (nome, CPF, cargo) e outro contendo 
 *  os dados de pagamento (CPF, salário, data de pagamento). 
 *  O programa deve consolidar os dois arquivos em um único 
 *  arquivo com todas as informações de cada funcionário. 
 *  Tarefa prática: Fazer o "merge" de arquivos baseados em 
 *  chaves comuns (neste caso, CPF).
 *    ** PARA ARQUIVOS JA ORDENADOS **   
 * Tectonics: cobc
 ******************************************************************
  IDENTIFICATION DIVISION.
  PROGRAM-ID. MERGE-ARQ.

  ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.

      FILE-CONTROL.

          SELECT ARQUIVO1 ASSIGN TO
           "C:\exe-cobol\arquivo1-merge.txt"
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS WS-FS-ARQ1.

          SELECT ARQUIVO2 ASSIGN TO
           "C:\exe-cobol\arquivo2-merge.txt"
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS WS-FS-ARQ2.

          SELECT ARQ-OUT ASSIGN TO
           "C:\exe-cobol\arquivo-out.txt"
          ORGANIZATION IS LINE SEQUENTIAL
          FILE STATUS IS WS-FS-OUT.

          SELECT OUTPUT-MERGE ASSIGN TO 'MERGE-O'.

  DATA DIVISION.
  FILE SECTION.

      FD ARQUIVO1.
      01 ARQ1-REGISTRO.
          05 ARQ1-CPF         PIC X(11).
          05 ARQ1-NOME        PIC A(30).
          05 ARQ1-CARGO       PIC X(20).

      FD ARQUIVO2.
      01 ARQ2-REGISTRO.
          05 ARQ2-CPF         PIC X(11).
          05 ARQ2-SALARIO     PIC 9(07)V99.
          05 ARQ2-PAGAMENTO   PIC X(10).

      SD OUTPUT-MERGE.
      01 MERGE-O.
          05 MERGE-CPF        PIC X(11).
          05 MERGE-NOME       PIC A(10).
          05 MERGE-CARGO      PIC X(11).
          05 MERGE-SALARIO    PIC 9(05)V99.
          05 MERGE-DATA-PAG   PIC X(10).


  WORKING-STORAGE SECTION.

      01 STAT.
          05 WS-FS-ARQ1       PIC 9(02).
          05 WS-FS-ARQ2       PIC 9(02).
          05 WS-FS-OUT        PIC 9(02).

      01 AUX.
          05 WS-EOF           PIC X(01) VALUE 'N'.

  PROCEDURE DIVISION.
  MAIN-PROCEDURE.

      OPEN INPUT ARQUIVO1, ARQUIVO2.
      OPEN OUTPUT ARQ-OUT.

      IF WS-FS-ARQ1 = '00' AND WS-FS-ARQ2 = '00'

          MERGE OUTPUT-MERGE
          ON ASCENDING KEY MERGE-CPF
          USING ARQUIVO1, ARQUIVO2
          GIVING ARQ-OUT

      ELSE
          
          DISPLAY 'ERRO AO ABRIR O ARQUIVO'

      END-IF.

      CLOSE ARQUIVO1.
      CLOSE ARQUIVO2.
      CLOSE ARQ-OUT.

       STOP RUN.
  END PROGRAM MERGE-ARQ.
