*&---------------------------------------------------------------------*
*& Include ZMRF_RECIPIENT_BARCODETOP                                   *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZMRF_RECIPIENT_BARCODE MESSAGE-ID ZMRF.

TABLES: ZTRF_RECIPIENT.

DATA: IT_RECP TYPE ZTRF_RECIPIENT OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_FCODE OCCURS 0,
      FCODE LIKE RSMPE-FUNC,
      END   OF IT_FCODE.

DATA: OK_CODE TYPE SY-UCOMM,
      OKCODE  TYPE SY-UCOMM.
DATA: CHANGE_MODE.
*&spwizard: type for the data of tablecontrol 'TC_RECP'
TYPES: BEGIN OF T_TC_RECP,
         PERNR LIKE ZTRF_RECIPIENT-PERNR,
         WEMPF LIKE ZTRF_RECIPIENT-WEMPF,
         FLAG,       "flag for mark column
       END OF T_TC_RECP.

*&spwizard: internal table for tablecontrol 'TC_RECP'
DATA:     G_TC_RECP_ITAB   TYPE T_TC_RECP OCCURS 0,
          G_TC_RECP_WA     TYPE T_TC_RECP. "work area
DATA:     G_TC_RECP_COPIED.           "copy flag

*&spwizard: declaration of tablecontrol 'TC_RECP' itself
CONTROLS: TC_RECP TYPE TABLEVIEW USING SCREEN 9900.

*&spwizard: lines of tablecontrol 'TC_RECP'
DATA:     G_TC_RECP_LINES  LIKE SY-LOOPC.
