      ******************************************************************
      * Author:
      * Date:
      * Purpose: Crie um programa que leia um arquivo de registros de
      *  alunos. Cada registro contém o nome do aluno e suas três notas.
      *  O programa deve calcular a média de cada aluno e gravar um
      *  novo arquivo que contenha o nome do aluno, suas notas, a média
      *  e o status ("Aprovado" ou "Reprovado" com base na média).
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIA-ALUNOS.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.

           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

           FILE-CONTROL.

               SELECT NOTAS-ALUNO ASSIGN TO
                "C:\exe-cobol\aluno-nota.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-NOTAS.

               SELECT MEDIA-OUTPUT ASSIGN TO
                "C:\exe-cobol\aluno-media-out.txt"
               ORGANISATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUT.

       DATA DIVISION.
       FILE SECTION.

           FD NOTAS-ALUNO.
           01 ARQUIVO-I                PIC X(30).

           FD MEDIA-OUTPUT.
           01 ARQUIVO-O.
               05 NOME-O               PIC A(15).
               05 FILLER               PIC X(01) VALUE SPACE.
               05 NOTA1-O              PIC 9(02).
               05 FILLER               PIC X(01) VALUE SPACE.
               05 NOTA2-O              PIC 9(02).
               05 FILLER               PIC X(01) VALUE SPACE.
               05 NOTA3-O              PIC 9(02).
               05 FILLER               PIC X(01) VALUE SPACE.
               05 MEDIA-O              PIC ZZ,99.
               05 FILLER               PIC X(01) VALUE SPACE.
               05 STATUS-O             PIC A(09).

       WORKING-STORAGE SECTION.

           01 WS-NOTAS.
               05 WS-NOME              PIC A(15).
               05 WS-NOTA1             PIC 9(02).
               05 WS-NOTA2             PIC 9(02).
               05 WS-NOTA3             PIC 9(02).
               05 WS-MEDIA             PIC 9(02)V99.
               05 WS-MEDIA-EDIT        PIC ZZ,99.
               05 WS-STATUS            PIC A(09).

           01 TEMP.
               05 WS-NOME-TEMP         PIC X(15).
               05 WS-NOTA1-TEMP        PIC X(02).
               05 WS-NOTA2-TEMP        PIC X(02).
               05 WS-NOTA3-TEMP        PIC X(02).

           01 STAT.
               05 WS-FS-NOTAS          PIC X(02).
               05 WS-FS-OUT            PIC X(02).

           01 AUX.
               05 WS-EOF               PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT NOTAS-ALUNO.
           OPEN OUTPUT MEDIA-OUTPUT.

           IF WS-FS-NOTAS = '00' AND WS-FS-OUT = '00'

               PERFORM UNTIL WS-EOF = 'Y'

                   READ NOTAS-ALUNO INTO WS-NOTAS
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       UNSTRING ARQUIVO-I
                       DELIMITED BY SPACES
                       INTO
                           WS-NOME,
                           WS-NOTA1,
                           WS-NOTA2,
                           WS-NOTA3
                       END-UNSTRING

                       COMPUTE
                        WS-MEDIA = (WS-NOTA1 + WS-NOTA2 + WS-NOTA3) / 3
                       MOVE WS-MEDIA TO WS-MEDIA-EDIT

                       IF WS-MEDIA >= 70
                            MOVE "Aprovado" TO WS-STATUS
                       ELSE
                           MOVE "Reprovado" TO WS-STATUS
                       END-IF

                       MOVE WS-NOME TO NOME-O
                       MOVE WS-NOTA1 TO NOTA1-O
                       MOVE WS-NOTA2 TO NOTA2-O
                       MOVE WS-NOTA3 TO NOTA3-O
                       MOVE WS-MEDIA-EDIT TO MEDIA-O
                       MOVE WS-STATUS TO STATUS-O

                       WRITE ARQUIVO-O
                       AFTER ADVANCING 1 LINE

               END-PERFORM

           END-IF.

           CLOSE NOTAS-ALUNO.
           CLOSE MEDIA-OUTPUT.

            STOP RUN.
       END PROGRAM MEDIA-ALUNOS.
