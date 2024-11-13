      ******************************************************************
      * Author:
      * Date:
      * Purpose: Crie uma estrutura de dados com uma vari�vel inteira
      *  de 4 d�gitos e uma vari�vel de caracteres de 4 bytes. Use
      *  REDEFINES para compartilhar o mesmo espa�o de mem�ria entre
      *  essas vari�veis. Teste a altera��o de um valor e observe o
      *  efeito no outro campo.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASICO_REDEFINES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  TESTE.
           05  NUMERO              PIC 9(04) VALUE ZEROS.
           05  CARACTER            REDEFINES NUMERO PIC A(04).
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM TESTE-NUMERICO.
           PERFORM REINICIA-VAR.
           PERFORM TESTE-CARACTER.
           STOP RUN.

       TESTE-NUMERICO.
      * TESTE MOVENDO NUMEROS PARA VARIAVEL NUMERICA
      *  E MOSTRAR NA VARIAVEL ALFABETICA
           MOVE 1234 TO NUMERO.
           DISPLAY CARACTER.

       TESTE-CARACTER.
      * TESTE MOVENDO LETRAS PARA VARIAVEL ALFABETICA
      *  E MOSTRAR NA VARIAVEL NUMERICA
           MOVE 'ABCD' TO CARACTER.
           DISPLAY NUMERO.

       REINICIA-VAR.
           INITIALIZE NUMERO, CARACTER.

       END PROGRAM BASICO_REDEFINES.
