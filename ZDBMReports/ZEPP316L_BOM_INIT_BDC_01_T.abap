*----------------------------------------------------------------------*
*   INCLUDE ZEPP316L_BOM_INIT_BDC_01_T                                 *
*----------------------------------------------------------------------*
TABLES: CUKB.
* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MATNR TYPE MARA-MATNR,
        WERKS TYPE T001W-WERKS,
        STLAN TYPE RC29N-STLAN,
        STLAL TYPE RC29N-STLAL,
        POSNR TYPE RC29P-POSNR,
        IDNRK TYPE RC29P-IDNRK,
        ZSUFF TYPE ZSUFF,
        ZSEQU(04),
        AENNR TYPE RC29N-AENNR,
        BMENG(20),
        BMEIN TYPE RC29K-BMEIN,
        STLST TYPE RC29K-STLST,
        POSTP TYPE RC29P-POSTP,
        MENGE(20),
          ZSTGB(20),
        MEINS TYPE RC29P-MEINS,
        ITSOB TYPE RC29P-ITSOB,
        ZEITM TYPE ZEITM,
        CLPT(01),
        DPID  TYPE RCUKD-KNNAM,
        UPCT(01),
        ZUPGN TYPE ZUPGN,
        ZINFO TYPE ZINFO,
        ZRESULT LIKE SY-MSGTY,
        ZMSG LIKE CFGNL-MSGLIN,
      END   OF IT_EXCL.
DATA: BEGIN OF IT_MAST OCCURS 0,
        MATNR TYPE MAST-MATNR,  "MATNRIAL NO
        WERKS TYPE MAST-WERKS,  "PLANT
        STLAN TYPE MAST-STLAN,  "BOM USAGE
        STLAL TYPE MAST-STLAL,  "ALTERNATIVE BOM
        POSNR TYPE STPO-POSNR,  "BOM item number(PREFIX)
        IDNRK TYPE STPO-IDNRK,  "BOM component
        ZSUFF TYPE STPO-SUFF ,  "BOM item text1(SUFFIX)
        ZSEQU TYPE STPO-SEQU,
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
* BDC-DATA
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
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_CHECK.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT
             'C:\    .txt' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'CS01 & CS02'.
SELECTION-SCREEN END   OF BLOCK B1.
