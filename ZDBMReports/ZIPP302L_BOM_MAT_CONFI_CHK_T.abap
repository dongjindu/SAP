*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_MAT_CONFI_CHK_T                               *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXEBMDT,  "ECM BOM STRUCTURE
        MARA.           "Material Master

*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: IT_AEBM TYPE ZTBM_ABXEBMDT OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF IT_BOM_EXPLODED OCCURS 0,
        MATNR      LIKE   STPOV-MATNR,   "Material
        WERKS      LIKE   STPOV-WERKS,   "Plant
        KZKFG      TYPE   MARA-KZKFG,    "Configurable Material
        ZRESULT    TYPE   SY-MSGTY,
        ZMSG       LIKE   CFGNL-MSGLIN,
       END   OF IT_BOM_EXPLODED.
*------> BOM EXPLOSION

DATA : IT_MC29S   LIKE MC29S   OCCURS 0 WITH HEADER LINE,
       IT_STPOV   LIKE STPOV   OCCURS 0 WITH HEADER LINE,
       IT_CSCEQUI LIKE CSCEQUI OCCURS 0 WITH HEADER LINE,
       IT_CSCKND  LIKE CSCKND  OCCURS 0 WITH HEADER LINE,
       IT_CSCMAT  LIKE CSCMAT  OCCURS 0 WITH HEADER LINE,
       IT_CSCSTD  LIKE CSCSTD  OCCURS 0 WITH HEADER LINE,
       IT_CSCTPL  LIKE CSCTPL  OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_LAST.

*----------------------------------------------------------------------*
* BDC VARIABLE DECLARATION
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.

DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.

DATA: BEGIN OF WA_OPT.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT,
            P_ZBTIM LIKE ZTBM_ABXEBMDT-ZBTIM,
            P_CLPT  LIKE ZTBM_ABXEBMDT-CLPT DEFAULT 'C', " COLOR PART
            P_UPCT  LIKE ZTBM_ABXEBMDT-UPCT DEFAULT '1'. " CONTROL TYPE
SELECTION-SCREEN END   OF BLOCK B1.
