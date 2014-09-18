*----------------------------------------------------------------------*
*   INCLUDE ZIPP301L_ECM_BDC_T                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA (???? ?? TABLE? ??)
*----------------------------------------------------------------------*
TABLES: RC29A,
        T100.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: IT_ECM TYPE ZTBM_ABXEHDDT OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_ECM1 OCCURS 0,
        AENNR TYPE RC29A-AENNR,
        DATUV(8),  "TYPE RC29A-DATUV,
        EOKD  TYPE AENR-AENST,
        AENST TYPE RC29A-AENST,
        AETXT TYPE RC29A-AETXT,
      END OF IT_ECM1.

DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.

DATA: IT_MSG LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_ZEDAT LIKE ZTBM_ABXEHDDT-ZEDAT DEFAULT SY-DATUM,
  P_ZBTIM LIKE ZTBM_ABXEHDDT-ZBTIM.

SELECTION-SCREEN END   OF BLOCK B1.
