      *================================================================*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.    CRP0206.
      *----------------------------------------------------------------*
      * SISTEMA:      CR - CONTAS A RECEBER
      * JOB:          02 - GERACAO DE FLUXO DE CAIXA
      * PROGRAMA:     06 - SELECIONA DUPLICATAS ATIVAS
      *
      * OBJETIVO:     GERAR ARQUIVO DE SAIDA QUE CONTEM APENAS AS DU-
      *               PLICATAS QUE NAO FORAM CANCELADAS (SITUACAO DA
      *               DUPLICATA DEVE SER DIFERENTE DE "CNC")
      *
      * VERSOES:      DATA    DESCRICAO
      *               ------  ---------------------------------------
      *               XXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *
      *----------------------------------------------------------------*

      *================================================================*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
      * UNLOAD DA TABELA DE DUPLICATAS
      *----------------------------------------------------------------*
           SELECT CRA0205 ASSIGN TO "../dat/cra0205.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WT-ST-CRA0205.

      *----------------------------------------------------------------*
      * DUPLICATAS ATIVAS EMITIDAS NO PERIODO
      *----------------------------------------------------------------*
           SELECT CRA0206 ASSIGN TO "../dat/cra0206.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WT-ST-CRA0206.

      *================================================================*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------------*
      * UNLOAD DA TABELA DE DUPLICATAS
      *----------------------------------------------------------------*
       FD CRA0205.
       01 CRA0205-REGISTRO.
           03 CRA0205-NR-FATURA        PIC  9(006).
           03 CRA0205-NR-DUPLICATA     PIC  9(002).
           03 CRA0205-CD-CLIENTE       PIC  X(006).
           03 CRA0205-DT-EMISSAO       PIC  9(008).
           03 CRA0205-DT-VENCIMENTO    PIC  9(008).
           03 CRA0205-VL-FATURA        PIC S9(013)V9(002).
           03 CRA0205-CD-CATEGORIA     PIC  X(003).
           03 CRA0205-ST-DUPLICATA     PIC  X(003).

      *----------------------------------------------------------------*
      * DUPLICATAS ATIVAS EMITIDAS NO PERIODO
      *----------------------------------------------------------------*
       FD CRA0206.
       01 CRA0206-REGISTRO.
           03 CRA0206-NR-FATURA        PIC  9(006).
           03 CRA0206-NR-DUPLICATA     PIC  9(002).
           03 CRA0206-CD-CLIENTE       PIC  X(006).
           03 CRA0206-DT-EMISSAO       PIC  9(008).
           03 CRA0206-DT-VENCIMENTO    PIC  9(008).
           03 CRA0206-VL-FATURA        PIC S9(013)V9(002).
           03 CRA0206-CD-CATEGORIA     PIC  X(003).
           03 CRA0206-ST-DUPLICATA     PIC  X(003).

      *================================================================*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
       01 WT-CONTADORES.
           03 WT-CT-LIDOS              PIC  9(006) VALUE ZEROS.
           03 WT-CT-GRAVADOS           PIC  9(006) VALUE ZEROS.

       01 WT-FILE-STATUS.
           03 WT-ST-CRA0205  PIC X(002) VALUE SPACES.
           03 WT-ST-CRA0206  PIC X(002) VALUE SPACES.
      *================================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       0-PRINCIPAL.

           PERFORM 1-INICIO.

           PERFORM 2-PROCESSO UNTIL WT-ST-CRA0205 NOT = '00'.

           PERFORM 3-TERMINO.

           STOP RUN.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVOS E PRIMEIRA LEITURA DO ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
       1-INICIO.

           OPEN INPUT CRA0205 OUTPUT CRA0206.

           READ CRA0205.

      *----------------------------------------------------------------*
      * VERIFICA SE A DUPLICATA E’ ATIVA. SE FOR, GRAVA O REGISTRO NO
      * ARQUIVO DE SAIDA E LE O PROXIMO REGISTRO DO ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
       2-PROCESSO.

           ADD 1 TO WT-CT-LIDOS

           IF CRA0205-ST-DUPLICATA NOT = 'CNC'
               PERFORM 21-GRAVA-SAIDA THRU 21-FIM
           END-IF.

      *----------------------------------------------------------------*
      * FECHA ARQUIVOS E EXIBE CONTADORES
      *----------------------------------------------------------------*
       3-TERMINO.

           CLOSE CRA0205 CRA0206

           DISPLAY 'LIDOS= ' WT-CT-LIDOS ' GRAVADOS= ' WT-CT-GRAVADOS.

      *----------------------------------------------------------------*
      * GRAVA REGISTRO NO ARQUIVO DE SAIDA
      *----------------------------------------------------------------*
       21-GRAVA-SAIDA.

           MOVE CRA0205-NR-FATURA TO CRA0206-NR-FATURA
           MOVE CRA0205-NR-DUPLICATA TO CRA0206-NR-DUPLICATA
           MOVE CRA0205-CD-CLIENTE TO CRA0206-CD-CLIENTE
           MOVE CRA0205-DT-EMISSAO TO CRA0206-DT-EMISSAO
           MOVE CRA0205-DT-VENCIMENTO TO CRA0206-DT-VENCIMENTO
           MOVE CRA0205-VL-FATURA TO CRA0206-VL-FATURA
           MOVE CRA0205-CD-CATEGORIA TO CRA0206-CD-CATEGORIA
           MOVE CRA0205-ST-DUPLICATA TO CRA0206-ST-DUPLICATA
           WRITE CRA0206-REGISTRO
           ADD 1 TO WT-CT-GRAVADOS.

       21-FIM.
