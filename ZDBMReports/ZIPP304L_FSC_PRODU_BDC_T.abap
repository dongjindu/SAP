*----------------------------------------------------------------------*
*   INCLUDE ZIPP304U_FSC_PRODU_BDC_T                                   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXLFPDT.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: IT_ALFP TYPE ZTBM_ABXLFPDT OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_ROUT OCCURS 0.
       INCLUDE STRUCTURE ZTBM_ABXLFPDT.
DATA   PLNNR TYPE MAPL-PLNNR.
DATA  END   OF IT_ROUT.

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MATNR TYPE MARA-MATNR,  "MATERIAL
        WERKS TYPE MARC-WERKS,  "Plant
        VERID TYPE MKAL-VERID,  "COLOR LESS FSC VERSION
        TEXT1 TYPE MKAL-TEXT1, "TEXT PRO.VERS
        ADATU TYPE MKAL-ADATU,  "DATE BEG.OF.VALIDITY
        BDATU TYPE MKAL-BDATU,  "VALIDIT ENDS
        STLAL TYPE MKAL-STLAL,  "ALTERNATIVE BOM
        STLAN TYPE MKAL-STLAN,  "BOM USAGE
        MSGTY TYPE SY-MSGTY,
        MESSG LIKE CFGNL-MSGLIN,
      END   OF IT_EXCL.

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
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-100.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 USER-COMMAND UCOM DEFAULT 'X',
            P_RDO2 RADIOBUTTON GROUP R1.
SELECTION-SCREEN END   OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-200.
PARAMETERS:
  P_ZEDAT LIKE ZTBM_ABXLFPDT-ZEDAT DEFAULT SY-DATUM,
  P_ZBTIM LIKE ZTBM_ABXLFPDT-ZBTIM.

PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM02'.
SELECTION-SCREEN END   OF BLOCK B1.
