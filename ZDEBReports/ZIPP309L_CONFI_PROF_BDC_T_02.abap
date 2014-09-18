*----------------------------------------------------------------------*
*   INCLUDE ZIPP309L_CONFI_PROF_BDC_T                                  *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTBM_ABYCFIDT,
        RCUCO.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: IT_ACFI TYPE ZTBM_ABYCFIDT OCCURS 0 WITH HEADER LINE.
* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MTNO(18),
        PLNT(04),   "
        CLID(18),
        EONO(12),
        PRIT(02),
        PROF(30),
        CLTY(03),
        STAT(01),
        CLMI(10), "TYPE ZTBM_ABYCFIDT-CLMI,
        CLO1(04),  " TYPE ZTBM_ABYCFIDT-CLO1,
        CLO2(04),  " TYPE ZTBM_ABYCFIDT-CLO2,
        CLO3(04),  " TYPE ZTBM_ABYCFIDT-CLO3,
        CLO4(04),  " TYPE ZTBM_ABYCFIDT-CLO4,
        GEFT(04),  " TYPE ZTBM_ABYCFIDT-GEFT,
        ZRESULT TYPE ZTBM_ABYCFIDT-ZRESULT,
        ZMSG TYPE ZTBM_ABYCFIDT-ZMSG,
      END   OF IT_EXCL.

DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      PARAM_CUQ LIKE TPARA-PARAMID VALUE 'CUQ',
      L_NAMEEXT LIKE RCTMS-MNAME VALUE 'COL_EXT',
      L_NAMEINT LIKE RCTMS-MNAME VALUE 'COL_INT'.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
*PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 DEFAULT 'X'
*                                   USER-COMMAND UCOM,
*            P_RDO2 RADIOBUTTON GROUP R1.
SELECTION-SCREEN END   OF BLOCK B2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_ZEDAT LIKE ZTBM_ABYCFIDT-ZEDAT DEFAULT SY-DATUM,
  P_ZBTIM LIKE ZTBM_ABYCFIDT-ZBTIM.

* EXCEL DATA UPLOAD
PARAMETERS:
**  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
*  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
*  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'CU41'.
SELECTION-SCREEN END   OF BLOCK B1.
