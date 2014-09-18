************************************************************************
* Program Name      : ZRRF_BARCODE_PERNR
* Author            : Bongsoo, Kim
* Creation Date     : 2004.07.22.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K911210
* Addl Documentation:
* Description       : Personnel number bar code print
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRRF_BARCODE_PERNR
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMRF.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZTRF_RECIPIENT.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_RECP OCCURS 0,
        PERNR TYPE ZTRF_RECIPIENT-PERNR,
        WEMPF TYPE ZTRF_RECIPIENT-WEMPF,
      END OF IT_RECP.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* DECLARATION FOR SEARCH HELP
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE CONTROLS
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_PERNR TYPE ZTRF_RECIPIENT-PERNR  OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM READ_PROCESS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT PERNR
         WEMPF
       FROM ZTRF_RECIPIENT
       INTO TABLE IT_RECP
       WHERE PERNR EQ P_PERNR.
  IF SY-SUBRC EQ 0.
    PERFORM BARCODE_PRINTING.
  ELSE.
    WRITE: / 'NO DATA'.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BARCODE_PRINTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BARCODE_PRINTING.
  DATA: L_PERNR(10),
        L_WEMPF(16).
  DATA: LEN  TYPE I,
        LEN1 TYPE I,
        LEN2 TYPE I.

  LOOP AT IT_RECP.
    NEW-PAGE PRINT ON
       NO-TITLE
       NO-HEADING
       LINE-SIZE 255
       DESTINATION 'RFL'
       IMMEDIATELY 'X'
       KEEP IN SPOOL 'X'
       NEW LIST IDENTIFICATION 'X'
       NO DIALOG.
    CONCATENATE '*' IT_RECP-PERNR '*' INTO L_PERNR .
    LEN  = STRLEN( L_PERNR ).

    LEN1 = STRLEN( IT_RECP-WEMPF ).
    L_WEMPF = IT_RECP-WEMPF.
    LEN2 = 16 - LEN1.
    CLEAR LEN1.
    IF LEN2 NE 0.
      LEN1 = LEN2 DIV 2 .
    ENDIF.
    DO LEN1 TIMES.
      CONCATENATE ' ' L_WEMPF INTO
                      L_WEMPF SEPARATED BY SPACE.
    ENDDO.

    WRITE: / '^XA' NO-GAP,
             '^FO' NO-GAP,
               '440' NO-GAP,  " X
               ',' NO-GAP,
               '80' NO-GAP,   " Y

             '^BY' NO-GAP,
               '2.5' NO-GAP,  "
               ',' NO-GAP,
               '1.5' NO-GAP,  "

               ',' NO-GAP,
               '100' NO-GAP,  "BAR

              '^AB' NO-GAP,
                'N' NO-GAP,
                ',' NO-GAP,
                '10' NO-GAP,
                ',' NO-GAP,
                '10' NO-GAP,

              '^B3N' NO-GAP,  " N"
                ',' NO-GAP,
                'N' NO-GAP,   "MODULE 43 CHECK DIGIT
                ',' NO-GAP,
                '80' NO-GAP,  "BAR
                ',' NO-GAP,
                'N' NO-GAP,    "BAR
                ',' NO-GAP,
                'N' NO-GAP,    "BAR
              '^FD' NO-GAP,
                L_PERNR(LEN) NO-GAP,
              '^FS' NO-GAP,
* DESCRIPTION WRITE
           /  '^FO' NO-GAP,
                '440' NO-GAP,
                ',' NO-GAP,
                '180' NO-GAP,

              '^AE' NO-GAP,
                'N' NO-GAP,
                ',' NO-GAP,
                '8' NO-GAP,
                ',' NO-GAP,
                '8' NO-GAP,

               '^FD' NO-GAP,
                 L_WEMPF,
               '^FS' NO-GAP,

             / '^XZ' NO-GAP.
    NEW-PAGE PRINT OFF.
  ENDLOOP.
ENDFORM.                    " BARCODE_PRINTING
