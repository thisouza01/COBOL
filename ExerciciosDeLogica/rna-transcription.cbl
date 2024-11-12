      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RNA-TRANSCRIPTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMPLEMENT PIC X(64).

       PROCEDURE DIVISION.

       RNA-TRANSCRIPTION.

       MOVE 'ACGTGGTCTTAA' TO WS-COMPLEMENT.
       INSPECT WS-COMPLEMENT REPLACING
                        ALL 'G' BY 'C',
                            'C' BY 'G',
                            'T' BY 'A',
                            'A' BY 'U'.
       DISPLAY WS-COMPLEMENT.
