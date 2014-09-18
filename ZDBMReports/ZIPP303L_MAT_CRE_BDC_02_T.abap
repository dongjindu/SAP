*----------------------------------------------------------------------*
*   INCLUDE ZIPP303L_MAT_CRE_BDC_01_T                                  *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA
*----------------------------------------------------------------------*
TABLES: ZTBM_ABYMMCDT,
        MARC,MARA,ZTBM_FSC_CRE_INF.


*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA IT_AMMC TYPE ZTBM_ABYMMCDT OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS1 OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS1.
DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I,
      WA_CHK.  "ERROR CHECK

*----------------------------------------------------------------------*
* BAPI ( INTERNAL-TABLE AREA )
*----------------------------------------------------------------------*
* BAPI IMPORT STRUCTURE
DATA: WA_HEAD  TYPE BAPIMATHEAD,  "Header
      WA_MARA  TYPE BAPI_MARA,    "Client-specific material data
      WA_MARAX TYPE BAPI_MARAX,   "Information on update for CLIENTDATA
      WA_MARC  TYPE BAPI_MARC,    "Plant-specific material data
      WA_MARCX TYPE BAPI_MARCX,   "Information on update for PLANTDATA
      WA_MPOP  TYPE BAPI_MPOP,    "Forecast parameters
      WA_MPOPX TYPE BAPI_MPOPX,   "Information on updatefor FORECASTDATA
      WA_MPGD  TYPE BAPI_MPGD,    "Planning data
      WA_MPGDX TYPE BAPI_MPGDX,   "Information on update forPLANNINGDATA
      WA_MBEW  TYPE BAPI_MBEW,    "Valuation data
      WA_MBEWX TYPE BAPI_MBEWX,   "Information onupdatefor VALUATIONDATA
      WA_MVKE  TYPE BAPI_MVKE,    "Sales data
      WA_MVKEX TYPE BAPI_MVKEX.   "Information on update for SALESDATA
* EXPORT
DATA: WA_RETURN TYPE BAPIRET2.
* BAPI TABLES
DATA: IT_MAKT TYPE BAPI_MAKT OCCURS 0 WITH HEADER LINE,
      IT_MLAN TYPE BAPI_MLAN OCCURS 0 WITH HEADER LINE,
      IT_MESS TYPE BAPI_MATRETURN2 OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-200.
PARAMETERS: P_ZEDAT LIKE ZTBM_ABYMMCDT-ZEDAT,
            P_ZBTIM LIKE ZTBM_ABYMMCDT-ZBTIM.
SELECTION-SCREEN END   OF BLOCK B1.
