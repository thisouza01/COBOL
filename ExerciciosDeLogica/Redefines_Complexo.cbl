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
      * Anotações
      * Considerei o exemplo 
      * A  pic x(01).
      *  B  REDEFINES a
      *    B1 OCCURS tantas vezes
      *     
      *  C  REDEFINES a  
      *    C1 OCCURS tantas vezes
      * 
      *
       01  DADOS.
      * Valor que vai entrar na variavel
           05 VALOR-BRUTO      PIC X(40).
      * Valores alfabeticos serão mostrados
           05 NOMES            REDEFINES VALOR-BRUTO.
               10 NOME         PIC X(10) OCCURS 4 TIMES.
      * Valores numericos serao mostrados
           05 NUMERICOS        REDEFINES VALOR-BRUTO.
               10 NUMERO       PIC 9(10) OCCURS 2 TIMES.

       01  INDICE.
           05 I                PIC 9(02) VALUE 1.
           05 J                PIC 9(02) VALUE 1.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM MOVE-DADOS1.
           PERFORM PERCORRE-NOMES.
           PERFORM MOVE-DADOS2.
           PERFORM PERCORRE-VALORES.
           STOP RUN.

       MOVE-DADOS1.
           MOVE 'MARIA     JOAO     ANA      LUCAS' TO VALOR-BRUTO.
          
       PERCORRE-NOMES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               DISPLAY 'Nome ' I ': ' NOME(I)
           END-PERFORM.

       MOVE-DADOS2.
           MOVE '12345678901234567890' TO VALOR-BRUTO.

       PERCORRE-VALORES.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 2
               DISPLAY 'Valor ' J ': ' NUMERO(J)
           END-PERFORM.

       END PROGRAM REDEFINES_COMPLEXO.
