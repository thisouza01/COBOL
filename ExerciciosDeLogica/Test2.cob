      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA.
      *----------------------------------------------------------------*
      * OBJETIVO: UM PROGRAMA PARA LER AS NOTAS DOS ESTUDANTES E MOSTRAR
      * UMA MENSAGEM BASEADA NA SUA NOTA
      * EX: >90 = EXCELLENT / >70 & <90 = GOOD /
      * OTHERS = NEEDS IMPROVEMENT
      *================================================================*

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 WR-ALUNOS   PIC 9(03)   VALUE ZEROS.
       01 WR-NOTA     PIC 9(02)   VALUE ZEROS.

       01 WR-AUXILIAR.
           03 WR-CONT PIC 9(02)   VALUE ZEROS.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.
           PERFORM 1-INICIAR.
           PERFORM 2-PROCESSAR.
           PERFORM 3-FINALIZAR.
           STOP RUN.

       1-INICIAR.
           DISPLAY 'QUANTOS ALUNOS TEM NA CLASSE?'
           ACCEPT WR-ALUNOS.

       2-PROCESSAR.

           PERFORM VARYING WR-CONT FROM 1 BY 1
               UNTIL WR-CONT > WR-ALUNOS

               DISPLAY 'QUAL A NOTA?'
               ACCEPT WR-NOTA

               EVALUATE WR-NOTA

                   WHEN >90
                       DISPLAY 'EXCELLENT'

                   WHEN 70 THRU  90
                       DISPLAY 'GOOD'

                   WHEN OTHER
                       DISPLAY 'NEEDS IMPROVEMNT'

               END-EVALUATE

           END-PERFORM.

       3-FINALIZAR.
           DISPLAY 'TERMINADO'.
      *================================================================*
