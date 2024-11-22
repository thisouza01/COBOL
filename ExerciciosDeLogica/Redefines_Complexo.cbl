      ******************************************************************
      * Author:
      * Date:
      * Purpose:Crie uma estrutura de dados com uma área de 20 bytes
      *  que contém 4 variáveis diferentes. Use REDEFINES para mapear
      *  essas variáveis de diferentes formas e observe o impacto de
      *  manipulações de dados
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REDEFINES_COMPLEXO.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  DADOS.
      * Valor que vai entrar na variavel
           05 VALOR-BRUTO      PIC X(40).
      * Valores alfabeticos serão mostrados
           05 NOMES            REDEFINES VALOR-BRUTO.
               10 NOME         PIC X(10) OCCURS 4 TIMES.
      * Valores numericos serao mostrados
           05 NUMERICOS        REDEFINES VALOR-BRUTO.
               10 NUMERO       PIC 9(20) OCCURS 2 TIMES.

       01  INDICE.
           05 I                PIC 9(02) VALUE 1.
           05 J                PIC 9(02) VALUE 1.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM MOVE-DADOS.
           PERFORM PERCORRE-NOMES.
           PERFORM PERCORRE-VALORES.
           STOP RUN.

       MOVE-DADOS.
           MOVE 'MARIA     JOAO     ANA      LUCAS' TO VALOR-BRUTO.
           MOVE '1234567890' TO VALOR-BRUTO(31:).

       PERCORRE-NOMES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               DISPLAY 'Nome ' I ': ' NOME(I)
           END-PERFORM.

       PERCORRE-VALORES.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
               DISPLAY 'Valor ' J ': ' NUMERO(J)
           END-PERFORM.

       END PROGRAM REDEFINES_COMPLEXO.
