*----------------------------------------------------------------------*
*   INCLUDE ZTEST_SD_DEL_TOP                                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: VBAK,
        ZTPP_WOSUM,CABN.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        BSTKD LIKE VBKD-BSTKD,
        VBELN(10),
      END   OF IT_EXCL.
DATA : VAL_TABLE LIKE ZSPP_VIN_VALUE OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
  DATA: BEGIN OF WA_OPT OCCURS 0.
          INCLUDE STRUCTURE CTU_PARAMS.
  DATA: END OF WA_OPT.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1 DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_MODE DEFAULT 'N'.
SELECTION-SCREEN END   OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_a1 RADIOBUTTON GROUP R2 USER-COMMAND UCOM,
            P_a2 RADIOBUTTON GROUP R2 DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK B3.
