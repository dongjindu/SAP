*----------------------------------------------------------------------*
*   INCLUDE ZRPP301L_CS12_T                                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES : CABN,
         ZTPP_WOSUM.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_TOTAL_LINE TYPE I.
*----------------------------------------------------------------------*
* ALV DECLARATION
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.
DATA: BEGIN OF STB OCCURS 1000.
        INCLUDE STRUCTURE STPOX.
DATA: END OF STB.

DATA: MAX_NUM(7)  TYPE P DECIMALS 3 VALUE '9999999999.999',
      MIN_NUM(7)  TYPE P DECIMALS 3 VALUE '9999999999.999-'.
DATA: UEBERL_KZ(1) TYPE C VALUE '*'.
DATA: B_FLAG(1) TYPE C VALUE 'X'.
DATA:
   OTYP_MAT(1) TYPE C VALUE '1',
   OOTYP_MAT(1) TYPE C VALUE 'M',                           "HGB099459
*  Objekttyp 'kein Objekt'
   OTYP_NOO(1) TYPE C VALUE '2',                            "YHG133914
   OTYP_DOC(1) TYPE C VALUE '3',
   OTYP_KLA(1) TYPE C VALUE '4',
*  Objekttyp 'Intramaterial'
   OTYP_NTM(1) TYPE C VALUE '5'.                            "YHG133914
* ---------------------------------
*     langes leeres Feld
DATA: ECFLD(250) TYPE C.                                    "YHG133914
DATA: ANZ_STUFE(11)  TYPE C.
DATA: IT_STPOX TYPE  STPOX OCCURS 0 WITH HEADER LINE.
DATA: WA_STB TYPE STPOX.
DATA: BEGIN OF ALV_STB OCCURS 0.
        INCLUDE STRUCTURE STPOX_ALV.
DATA:   INFO(3)   TYPE C,
      END OF ALV_STB.
DATA: BEGIN OF IT_MULT OCCURS 0.
        INCLUDE STRUCTURE ZSBM_STPOX_ALV.
DATA:   INFO(3)   TYPE C,
      END OF IT_MULT.
DATA: BEGIN OF FTAB OCCURS 200.
        INCLUDE STRUCTURE DFIES.
DATA: END   OF FTAB.
DATA: BEGIN OF STB_ORIG.                                    "
        INCLUDE STRUCTURE STPOX.                            "
DATA: END OF STB_ORIG.                                      "
*     Uebergabestruktur Typ STPOL_ADD
DATA: BEGIN OF STB_ADD.                                     "
        INCLUDE STRUCTURE STPOL_ADD.                        "
DATA: END OF STB_ADD.                                       "

DATA:
   REPORT_NAME      LIKE SY-REPID,
   ALVLO_STB        TYPE SLIS_LAYOUT_ALV,
   ALVVR            LIKE DISVARIANT,
   ALVVR_SAV        TYPE C,
   EXIT_BY_CALLER   TYPE C,
   EXIT_BY_USER     TYPE SLIS_EXIT_BY_USER.
DATA:
   ALVVR_SAV_ALL    TYPE C VALUE 'A',
   ALVVR_SAV_NO_USR TYPE C VALUE 'X'.
DATA:
*  ALV Events complete
   ALV_EVNT_TB_CMPL TYPE SLIS_T_EVENT,
*  ALV Events pf exit only
   ALV_EVNT_TB_PFXT TYPE SLIS_T_EVENT,
*  ALV Top of page table
   ALV_TOP_TB    TYPE SLIS_T_LISTHEADER,
*  field display properties  stb tab
   STB_FIELDS_TB TYPE SLIS_T_FIELDCAT_ALV.
DATA: WA_STB_FIELDS_TB TYPE SLIS_FIELDCAT_ALV.
DATA: BEGIN OF SELPOOL OCCURS 0.
*{ 09/28/11 Paul Change : CSTMAT -> CSCMAT
*        INCLUDE STRUCTURE CSTMAT.
        INCLUDE STRUCTURE CScMAT.
*}
DATA: END OF SELPOOL.

*     Materialkatalog
DATA: BEGIN OF MATCAT OCCURS 50.                            "YHG133914
        INCLUDE STRUCTURE CSCMAT.                           "YHG133914
DATA: END OF MATCAT.                                        "YHG133914

DATA: BEGIN OF CSIN.
        INCLUDE STRUCTURE CSIN.
DATA: END OF CSIN.

DATA:
   TYP_DOC   LIKE STZU-STLTY VALUE 'D',                     "YHG137469
   TYP_EQUI  LIKE STZU-STLTY VALUE 'E',
   TYP_KND   LIKE STZU-STLTY VALUE 'K',                     "YHG137469
   TYP_MAT   LIKE STZU-STLTY VALUE 'M',
   TYP_PRJ   LIKE STZU-STLTY VALUE 'P',                     "HGA046836
   TYP_STD   LIKE STZU-STLTY VALUE 'S',                     "YHG137469
   TYP_TPL   LIKE STZU-STLTY VALUE 'T'.                     "YHG137469
DATA: WA_CHK(01).
*----------------------------------------------------------------------*
* DECLARATION FOR SEARCH HELP
*----------------------------------------------------------------------*
DATA DYNPREAD LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF VALUETAB OCCURS 0,
          VALUE(80).
DATA: END OF VALUETAB.

DATA: BEGIN OF FIELDS OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF FIELDS.

DATA: BEGIN OF DYNPFIELDS  OCCURS 0.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA  SELECT_INDEX LIKE SY-TABIX.

DATA: BEGIN OF SELECT_VALUES OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA: END OF SELECT_VALUES.

DATA: BEGIN OF IT_IBSYMBOL OCCURS 0,
        ATWRT TYPE IBSYMBOL-ATWRT,
        ATNAM TYPE CABN-ATNAM,
      END OF IT_IBSYMBOL.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_WKOD(14).
PARAMETERS: P_MATNR TYPE ZBM_MATNR  "OBLIGATORY
                                     MEMORY ID MAT MODIF ID GR1,
            P_WERKS LIKE RC29L-WERKS OBLIGATORY
                                     DEFAULT 'P001' MODIF ID GR1,
            P_STLAN LIKE MAST-STLAN  OBLIGATORY
                                     DEFAULT '1' MODIF ID GR1,
            P_STLAL LIKE MAST-STLAL  MODIF ID GR1,
            P_CAPID LIKE RC29L-CAPID OBLIGATORY MEMORY ID CSA
                                     DEFAULT 'PP01' MODIF ID GR1.
SELECTION-SCREEN END   OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_ATWRE LIKE  CAWN-ATWRT,    "MODIF ID GR1,
            P_ATWRI LIKE  CAWN-ATWRT.    "MODIF ID GR1.
SELECTION-SCREEN END   OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_DATUV LIKE RC29L-DATUV DEFAULT SY-DATUM,
            P_AENNR LIKE AENR-AENNR,
*            P_REVLV LIKE RC29L-REVLV MODIF ID GR1,
            P_EMENG LIKE RC29L-EMENG OBLIGATORY
                                     DEFAULT '1' MODIF ID GR1.
SELECTION-SCREEN END   OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1,
            P_RDO2 RADIOBUTTON GROUP R1,
            P_RDO3 RADIOBUTTON GROUP R1.
SELECTION-SCREEN END   OF BLOCK B4.
