************************************************************************
* Program Name      : ZEPP309U_HPCS_REPORT
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.07.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K903741
* Addl Documentation:
* Description       : HPCS Hysteresis administration
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZEPP309U_HPCS_REPORT
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES : ZTBM_ABXHPCDT,
         ZTBM_ABXHPCDT01.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_HPCS OCCURS 0,
        FSCC  TYPE ZTBM_ABXHPCDT-FSCC,
*        PQBG  TYPE ZTBM_ABXHPCDT-PQBG,
*        CEXT  TYPE ZTBM_ABXHPCDT-CEXT,
*        CINT  TYPE ZTBM_ABXHPCDT-CINT,
        ZEDAT TYPE ZTBM_ABXHPCDT-ZEDAT,
*        ZMODE TYPE ZTBM_ABXHPCDT-ZMODE,
*        MATNR TYPE MATNR,
        ZMODT(10),
      END   OF IT_HPCS.
DATA: BEGIN OF IT_MARA OCCURS 0,
        MATNR TYPE MARA-MATNR,
        MAKTX TYPE MAKT-MAKTX,
      END   OF IT_MARA.
DATA: BEGIN OF IT_AUSP OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATINN TYPE AUSP-ATINN,
        ATFLV TYPE AUSP-ATFLV,
      END   OF IT_AUSP.

DATA: BEGIN OF IT_LIST OCCURS 0,
        ZEDAT TYPE ZTBM_ABXHPCDT-ZEDAT,
        MATNR TYPE MARA-MATNR,
        MAKTX TYPE MAKT-MAKTX,
        MODE(20),
      END   OF IT_LIST.
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
DATA: BEGIN OF FTAB OCCURS 200.
        INCLUDE STRUCTURE DFIES.
DATA: END   OF FTAB.
DATA: BEGIN OF STB_ORIG.                                    "YHG133914
        INCLUDE STRUCTURE STPOX.                            "YHG133914
DATA: END OF STB_ORIG.                                      "YHG133914
*     Uebergabestruktur Typ STPOL_ADD
DATA: BEGIN OF STB_ADD.                                     "YHG133914
        INCLUDE STRUCTURE STPOL_ADD.                        "YHG133914
DATA: END OF STB_ADD.                                       "YHG133914

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
        INCLUDE STRUCTURE CSTMAT.
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
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_CRE TYPE I,
      WA_LINE_UPD TYPE I.
DATA: WA_ATINN TYPE AUSP-ATINN,
      WA_ATINM TYPE AUSP-ATINN,
      WA_CHECK.
*----------------------------------------------------------------------*
* RANGES
*----------------------------------------------------------------------*
RANGES: R_ATFLV FOR AUSP-ATFLV.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  S_ZEDAT FOR  ZTBM_ABXHPCDT-ZEDAT,
  S_FSCC  FOR  ZTBM_ABXHPCDT-FSCC.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  IF WA_CHECK NE 'X'.
    PERFORM DATA_PROCESS.

    PERFORM WRITE_PROCESS.
  ENDIF.

END-OF-SELECTION.

*TOP-OF-PAGE.
*  PERFORM TOP_OF_PAGE.
*
*END-OF-PAGE.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  S_ZEDAT-LOW = SY-DATUM.
  S_ZEDAT-HIGH = SY-DATUM.
  S_ZEDAT-SIGN = 'I'.
  S_ZEDAT-OPTION = 'BT'.
  APPEND S_ZEDAT.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.

  PERFORM S_ZEDAT_TO_R_ATFLV.

  PERFORM READ_ZTBM_ABXHPCDT.
  IF WA_CHECK NE 'X'.
    PERFORM READ_MAKT.
    PERFORM READ_CLASSIFICATION.
    PERFORM READ_AUSP.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_DATUM(8) TYPE N.
  SORT IT_MARA BY MATNR.
  SORT IT_HPCS BY FSCC.
  LOOP AT IT_AUSP.
    IT_LIST-MATNR = IT_AUSP-OBJEK.
    READ TABLE IT_MARA WITH KEY MATNR = IT_LIST-MATNR
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_LIST-MAKTX = IT_MARA-MAKTX.
      READ TABLE IT_HPCS WITH KEY FSCC = IT_LIST-MAKTX
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        L_DATUM = IT_AUSP-ATFLV.
        IT_LIST-ZEDAT = L_DATUM.
*        IT_LIST-ZEDAT = IT_HPCS-ZEDAT.
      ENDIF.
    ENDIF.

    IF WA_ATINN EQ IT_AUSP-ATINN.
      WA_LINE_CRE = WA_LINE_CRE + 1.
      IT_LIST-MODE = 'Created'.
    ENDIF.
    IF WA_ATINM EQ IT_AUSP-ATINN.
      WA_LINE_UPD = WA_LINE_UPD + 1.
      IT_LIST-MODE = 'Updated'.
    ENDIF.
    APPEND IT_LIST.
    CLEAR: IT_LIST, IT_HPCS, IT_MARA, IT_AUSP.
  ENDLOOP.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA: L_TEXT(10).
  SORT IT_LIST BY ZEDAT MATNR.
  ALVVR = SY-REPID.
  ALVVR_SAV_ALL = 'A'.
  REPORT_NAME = SY-REPID.
  ALVLO_STB-INFO_FIELDNAME = 'INFO'.
  PERFORM ALV_EVNT_TB_PREP USING 'A'
                                 ALV_EVNT_TB_CMPL.
  PERFORM STB_FIELDS_TB_PREP.

  PERFORM ALV_TOP_TB_PREP USING ALV_TOP_TB.
* ALV DISPLAY
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = REPORT_NAME
            I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS_SET'
            I_STRUCTURE_NAME         = 'IT_LIST'
            IS_LAYOUT                = ALVLO_STB
            I_SAVE                   = ALVVR_SAV_ALL
            IS_VARIANT               = ALVVR
            IT_EVENTS                = ALV_EVNT_TB_CMPL
            IT_FIELDCAT              = STB_FIELDS_TB
       IMPORTING
            E_EXIT_CAUSED_BY_CALLER  = EXIT_BY_CALLER
            ES_EXIT_CAUSED_BY_USER   = EXIT_BY_USER
       TABLES
            T_OUTTAB                 = IT_LIST
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
  IF SY-SUBRC = 0.
    IF EXIT_BY_CALLER = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    ELSE.
*     User left list via F3, F12 or F15
      IF EXIT_BY_USER-BACK = 'X'.       "F3
*       <do_something>.
      ELSE.
        IF EXIT_BY_USER-EXIT = 'X'.     "F15
*         <do_something>.
        ELSE.
          IF EXIT_BY_USER-CANCEL = 'X'. "F12
*           <do_something>.
          ELSE.
*           should not occur!
*           <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  ENDIF.
*  PERFORM ALV_TOP_TB_PREP USING ALV_TOP_TB.
*  SORT IT_HPCS BY ZEDAT MATNR.
*  LOOP AT IT_HPCS.
*    IF IT_HPCS-ZMODE EQ 'C'.
*      CLEAR L_TEXT.
*      L_TEXT = 'CREATED'.
*    ELSEIF IT_HPCS-ZMODE EQ 'U'.
*      CLEAR L_TEXT.
*      L_TEXT = 'UPDATED'.
*    ENDIF.
*    WRITE: /(10) IT_HPCS-ZEDAT,
*            (18) IT_HPCS-MATNR,
*            (40) IT_HPCS-FSCC,
*            (20) L_TEXT.
*  ENDLOOP.
ENDFORM.                    " WRITE_PROCESS
*---------------------------------------------------------------------*
*       FORM alv_top_of_page                                          *
*---------------------------------------------------------------------*
FORM ALV_TOP_OF_PAGE.
*.....................................

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = ALV_TOP_TB.
ENDFORM. "alv_top_of_page
*---------------------------------------------------------------------*
*       FORM alv_top_tb_prep                                          *
*---------------------------------------------------------------------*
FORM ALV_TOP_TB_PREP USING    TOP_TB TYPE SLIS_T_LISTHEADER.
*......................................

  DATA: WA_TOP_TB TYPE SLIS_LISTHEADER.

  DATA: L_MAKTX LIKE MAKT-MAKTX,
        L_DATE1(10),
        L_DATE2(10).
*......................................

  CLEAR WA_TOP_TB.
  IF WA_TOP_TB IS INITIAL.
    WA_TOP_TB-TYP  = 'S'.
*  WA_TOP_TB-KEY  = 'Created W/O  : '.
    ECFLD = 'Created W/O  : '.
    WA_TOP_TB-KEY  = ECFLD(20).
*    CONDENSE WA_TOP_TB-KEY NO-GAPS.
    WA_TOP_TB-INFO = WA_LINE_CRE.
    APPEND WA_TOP_TB TO TOP_TB.
  ENDIF.

  CLEAR WA_TOP_TB.
  WA_TOP_TB-TYP  = 'S'.
  ECFLD = 'Updated W/O  : '.
  WA_TOP_TB-KEY  = ECFLD(20).
*  WA_TOP_TB-KEY  = 'Updated W/O  : '.
*  CONDENSE WA_TOP_TB-KEY NO-GAPS.
  WA_TOP_TB-INFO = WA_LINE_UPD.
  APPEND WA_TOP_TB TO TOP_TB.

  CLEAR WA_TOP_TB.
  IF WA_TOP_TB IS INITIAL.
    WA_TOP_TB-TYP  = 'S'.
    ECFLD = 'SAP INTERFACE DATE'.
    WA_TOP_TB-KEY  = ECFLD(20).
    CLEAR: ECFLD.
    WRITE: S_ZEDAT-LOW  TO L_DATE1,
           S_ZEDAT-HIGH TO L_DATE2.

    CONCATENATE L_DATE1 '~' L_DATE2 INTO WA_TOP_TB-INFO
                                    SEPARATED BY SPACE.
    APPEND WA_TOP_TB TO TOP_TB.
  ENDIF.
  CLEAR WA_TOP_TB.



  WA_TOP_TB-TYP  = 'S'.
  ECFLD = 'FSC'.
  WA_TOP_TB-KEY  = ECFLD(20).
  CLEAR: ECFLD.
  WRITE: S_FSCC-LOW  TO L_DATE1,
         S_FSCC-HIGH TO L_DATE2.
  IF NOT S_FSCC-LOW IS INITIAL.
    CONCATENATE L_DATE1 '~' L_DATE2 INTO WA_TOP_TB-INFO
                                    SEPARATED BY SPACE.
  ENDIF.
  APPEND WA_TOP_TB TO TOP_TB.
ENDFORM. "alv_top_tb_prep
*&---------------------------------------------------------------------
*         Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
FORM ALV_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'S121_ALV' EXCLUDING RT_EXTAB.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  WRITE: / 'Created W/O  : ', WA_LINE_CRE,
         / 'Updated W/O  : ', WA_LINE_UPD.
  WRITE: /(88) SY-ULINE.
  WRITE: /(10) 'DATE',
          (18) 'Work order',
          (40) 'Description',
          (20) 'Created/Updated'.
  WRITE: /(88) SY-ULINE.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  ALV_EVNT_TB_PREP
*&---------------------------------------------------------------------*
FORM ALV_EVNT_TB_PREP USING    EVENT_SPEC TYPE C
                               EVENT_TB TYPE SLIS_T_EVENT.

  DATA: WA_EVENT_TB TYPE SLIS_ALV_EVENT.
  CHECK EVENT_TB[] IS INITIAL.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = EVENT_TB.
  READ TABLE EVENT_TB
    WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
    INTO WA_EVENT_TB.

  IF SY-SUBRC = 0.
    WA_EVENT_TB-FORM = 'ALV_TOP_OF_PAGE'.
    APPEND WA_EVENT_TB TO EVENT_TB.
  ENDIF.


*  READ TABLE EVENT_TB
*    WITH KEY NAME = SLIS_EV_USER_COMMAND
*    INTO WA_EVENT_TB.
*
*  IF SY-SUBRC = 0.
*    WA_EVENT_TB-FORM = 'ALV_USER_COMMAND'.
*    APPEND WA_EVENT_TB TO EVENT_TB.
*  ENDIF.


  READ TABLE EVENT_TB
    WITH KEY NAME = SLIS_EV_PF_STATUS_SET
    INTO WA_EVENT_TB.

  IF SY-SUBRC = 0.
    WA_EVENT_TB-FORM = 'ALV_PF_STATUS_SET'.
    APPEND WA_EVENT_TB TO EVENT_TB.
  ENDIF.

ENDFORM.                    " ALV_EVNT_TB_PREP
*&---------------------------------------------------------------------*
*&      Form  STB_FIELDS_TB_PREP
*&---------------------------------------------------------------------*
FORM STB_FIELDS_TB_PREP.
*  CALL FUNCTION 'GET_FIELDTAB'
*       EXPORTING
*            LANGU    = SY-LANGU
*            TABNAME  = 'IT_HPCS'
*            WITHTEXT = ' '
*            ONLY     = 'T'
*       TABLES
*            FIELDTAB = FTAB
*       EXCEPTIONS
*            OTHERS   = 1.

*  LOOP AT FTAB.
*    CLEAR: WA_STB_FIELDS_TB.

*    CASE FTAB-FIELDNAME.
*      WHEN 'ZEDAT'.
  WA_STB_FIELDS_TB-FIELDNAME = 'ZEDAT'.
  WA_STB_FIELDS_TB-SELTEXT_L = 'SAP INTERFACE DATE'.
  WA_STB_FIELDS_TB-COL_POS   =  1.
  WA_STB_FIELDS_TB-FIX_COLUMN = 'X' .
  WA_STB_FIELDS_TB-OUTPUTLEN = 20.
  WA_STB_FIELDS_TB-JUST      = 'L' .
  APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

*      WHEN 'MATNR'.
  WA_STB_FIELDS_TB-FIELDNAME = 'MATNR'.
  WA_STB_FIELDS_TB-SELTEXT_L = 'WORK ORDER'.
  WA_STB_FIELDS_TB-COL_POS   =  2.
  WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
  WA_STB_FIELDS_TB-OUTPUTLEN = 25.
  APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

*      WHEN 'FSCC'.
  WA_STB_FIELDS_TB-FIELDNAME = 'MAKTX'.
  WA_STB_FIELDS_TB-SELTEXT_L = 'DESCRIPTION'.
  WA_STB_FIELDS_TB-COL_POS   =  3.
  WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
  WA_STB_FIELDS_TB-OUTPUTLEN = 40.
  WA_STB_FIELDS_TB-ICON       =  'X' .
  WA_STB_FIELDS_TB-JUST      = 'L' .
  APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

*      WHEN 'ZMODE'.
  WA_STB_FIELDS_TB-FIELDNAME = 'MODE'.
  WA_STB_FIELDS_TB-SELTEXT_L = 'Created/Updated'.
  WA_STB_FIELDS_TB-COL_POS   =  4.
  WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
  WA_STB_FIELDS_TB-OUTPUTLEN =  20.
  APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

*    ENDCASE.
*  ENDLOOP.
ENDFORM.                    " STB_FIELDS_TB_PREP
*&---------------------------------------------------------------------*
*&      Form  S_ZEDAT_TO_R_ATFLV
*&---------------------------------------------------------------------*
FORM S_ZEDAT_TO_R_ATFLV.
  DATA: L_LOW(8) TYPE N,
        L_HIGH(8) TYPE N,
        L_I_LOW TYPE I,
        L_I_HIGH TYPE I.
  LOOP AT S_ZEDAT.
    MOVE-CORRESPONDING S_ZEDAT TO R_ATFLV.
    L_LOW  = S_ZEDAT-LOW.
    L_HIGH = S_ZEDAT-HIGH.
    L_I_LOW  = L_LOW.
    L_I_HIGH = L_HIGH.
    R_ATFLV-LOW = L_I_LOW.
    IF NOT L_I_HIGH IS INITIAL.
      CLEAR R_ATFLV-HIGH.
      R_ATFLV-HIGH = L_I_HIGH.
    ENDIF.
    APPEND R_ATFLV.
    CLEAR R_ATFLV.
  ENDLOOP.
ENDFORM.                    " S_ZEDAT_TO_R_ATFLV
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXHPCDT
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABXHPCDT.
  SELECT FSCC
*         PQBG
*         CEXT
*         CINT
         ZEDAT
*         ZMODE
       FROM ZTBM_ABXHPCDT
       INTO TABLE IT_HPCS
       WHERE ZEDAT IN S_ZEDAT
       AND   FSCC  IN S_FSCC.
  IF SY-SUBRC EQ 0.
    SORT IT_HPCS.
    DELETE ADJACENT DUPLICATES FROM IT_HPCS COMPARING FSCC.
  ELSE.
    WRITE: 'NO DATA'.
    WA_CHECK = 'X'.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABXHPCDT
*&---------------------------------------------------------------------*
*&      Form  READ_MAKT
*&---------------------------------------------------------------------*
FORM READ_MAKT.
  IF NOT IT_HPCS[] IS INITIAL.
    LOOP AT IT_HPCS.
      SELECT MATNR
             MAKTX
           FROM MAKT
           APPENDING TABLE IT_MARA
           WHERE SPRAS EQ SY-LANGU
           AND   MAKTX EQ IT_HPCS-FSCC.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " READ_MAKT
*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM READ_CLASSIFICATION.

  PERFORM SELECT_CLASSIFICATION USING 'P_H_GEN_DATE' WA_ATINN.
  PERFORM SELECT_CLASSIFICATION USING 'P_H_MOD_DATE' WA_ATINM.

ENDFORM.                    " READ_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM SELECT_CLASSIFICATION USING P_ATNAM P_ATINN.
  DATA : L_ATINN   TYPE   AUSP-ATINN,
         L_ATWRT   TYPE   AUSP-ATWRT.
*  CLEAR P_ATWRT.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_ATNAM
       IMPORTING
            OUTPUT = P_ATINN.

ENDFORM.                    " SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  READ_AUSP
*&---------------------------------------------------------------------*
FORM READ_AUSP.
  IF NOT IT_MARA[] IS INITIAL.
    LOOP AT IT_MARA.
      SELECT OBJEK
             ATINN
             ATFLV
           FROM AUSP
           APPENDING TABLE IT_AUSP
*         FOR ALL ENTRIES IN IT_MARA
           WHERE OBJEK EQ IT_MARA-MATNR
           AND   ATINN IN (WA_ATINN, WA_ATINM)
           AND   ATFLV IN R_ATFLV.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " READ_AUSP
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
*  LOOP AT SCREEN.
*    CASE SCREEN-NAME.
*      WHEN 'S_FSCC-LOW' OR 'S_FSCC-HIGH'.
*        SCREEN-LENGTH = 23.
*
*        MODIFY SCREEN.
*      WHEN OTHERS.
*    ENDCASE.
*  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
