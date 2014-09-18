*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_BDC_T                                         *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXEBMDT,  "ECM BOM STRUCTURE
        MAST,           "Material to BOM Link
        STKO,           "BOM Header
        STPO,           "BOM Item
        MARC.           "Plant Data for Material

*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: IT_AEBM TYPE ZTBM_ABXEBMDT OCCURS 0 WITH HEADER LINE.
* Material number does exist check
DATA: BEGIN OF IT_MARC OCCURS 0,
        MATNR TYPE MARC-MATNR,
        WERKS TYPE MARC-WERKS,
        MTART TYPE MARA-MTART,
      END OF IT_MARC.
* BOM does exist check
DATA: BEGIN OF IT_MAST OCCURS 0,
        MATNR TYPE MAST-MATNR,  "MATNRIAL NO
        WERKS TYPE MAST-WERKS,  "PLANT
        STLAN TYPE MAST-STLAN,  "BOM USAGE
        STLAL TYPE MAST-STLAL,  "ALTERNATIVE BOM
        POSNR TYPE STPO-POSNR,  "BOM item number(PREFIX)
        IDNRK TYPE STPO-IDNRK,  "BOM component
        POTX1 TYPE STPO-POTX1,  "BOM item text1(SUFFIX)
        STLNR TYPE MAST-STLNR,  "Bill of material
        STLKN TYPE STPO-STLKN,  "BOM item node number
      END OF IT_MAST.
*----------------------------------------------------------------------*
* BOM EXPLOSION
*----------------------------------------------------------------------*
DATA : BEGIN OF IT_BOM_EXPLODED OCCURS 0,
        MATNR      LIKE   STPOV-MATNR,   "Material
        WERKS      LIKE   STPOV-WERKS,   "Plant
        KZKFG      TYPE   MARA-KZKFG,    "Configurable Material
       END   OF IT_BOM_EXPLODED.

DATA : IT_MC29S   LIKE MC29S   OCCURS 0 WITH HEADER LINE,
       IT_STPOV   LIKE STPOV   OCCURS 0 WITH HEADER LINE,
       IT_CSCEQUI LIKE CSCEQUI OCCURS 0 WITH HEADER LINE,
       IT_CSCKND  LIKE CSCKND  OCCURS 0 WITH HEADER LINE,
       IT_CSCMAT  LIKE CSCMAT  OCCURS 0 WITH HEADER LINE,
       IT_CSCSTD  LIKE CSCSTD  OCCURS 0 WITH HEADER LINE,
       IT_CSCTPL  LIKE CSCTPL  OCCURS 0 WITH HEADER LINE.
DATA:  WA_LAST.
*----------------------------------------------------------------------*
* GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_CHECK.

*----------------------------------------------------------------------*
* BDC VARIABLE DECLARATION
*----------------------------------------------------------------------*

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
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT,
            P_ZBTIM LIKE ZTBM_ABXEBMDT-ZBTIM.
SELECTION-SCREEN END   OF BLOCK B1.
