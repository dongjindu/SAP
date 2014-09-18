*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 06/10/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-2
*& Development Request No : UD1K904466
*& Addl documentation     :
*& Description  : [FI-IM] IM Progress Report
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 10.18.2006  Michelle J  Andy Choi       Add %Progress
*&--------------------------------------------------------------------
* Bug:
* - downpayment;
* - original budget; standard ledger, z-table.

REPORT ZRFII07 MESSAGE-ID  ZMFI.
TYPE-POOLS: SLIS, VRM.
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
CLASS CL_GUI_RESOURCES DEFINITION LOAD.

CONSTANTS C_F2CODE LIKE SY-UCOMM VALUE '&ETA'.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORTS    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GS_PRNT     TYPE SLIS_PRINT_ALV.

DATA: WA_REPID LIKE SY-REPID,
      WA_VAR_SAVE(1) TYPE C             VALUE  'A',
      WA_DEFAULT(1)  TYPE C,
      WA_EXIT(1) TYPE C,
      WA_VARIANT LIKE DISVARIANT,
      WA_VAR LIKE DISVARIANT,
      WA_ALV_FUNCTION_NAME(30) TYPE C VALUE 'REUSE_ALV_GRID_LIST',
      WA_ALV_GET_INFO_NAME(40) TYPE C.

*--- ALV
DATA: W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
      W_SELFIELD TYPE SLIS_SELFIELD,
      W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      W_COL_POS  TYPE I,
      W_PROGRAM  LIKE SY-REPID,
      W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
      W_LINE1 TYPE SLIS_LISTHEADER.

*----------------------------------------------------------------------
* define tables and internal structure
*----------------------------------------------------------------------
TABLES: AUFK, IMPR, FMFCTR, RIPASW, CODIA, BKPF, ZTFI_IMFM, IMAK.

TYPES: BEGIN OF TY_ITAB,
         AUFNR TYPE AUFNR,
         POSNR TYPE IM_POSNR,
*         GJAHR TYPE IM_GNJHR,
         POSID TYPE IM_POSID,
         PRNAM TYPE IM_PRNAM,
         OBJNR TYPE IM_OBJNR,
         KOSTL type kostl,
       END OF TY_ITAB.

TYPES: BEGIN OF TY_IMAK,
         POSID TYPE IMA_POSID,
       END OF TY_IMAK.

DATA: BEGIN OF IT_IMPR OCCURS 0,
         POSID LIKE IMPR-POSID,
         OBJNR LIKE IMPR-OBJNR,
         GJAHR LIKE IMPR-GJAHR,
         ARNO  LIKE IMAK-POSID,
         KOSTL type KOSTL,
      END OF IT_IMPR.

DATA: IT_OUT       TYPE TABLE OF IMPR WITH HEADER LINE,
*      IT_IMZO      TYPE TABLE OF IMZO WITH HEADER LINE,
*     IT_IMPR      TYPE TABLE OF IMPR WITH HEADER LINE,
      IT_IMFM      TYPE TABLE OF ZTFI_IMFM WITH HEADER LINE,
      IT_AUFK      TYPE TABLE OF AUFK WITH HEADER LINE,
      IT_IO_BUDGET TYPE TABLE OF ZFI_IO_BUDGET WITH HEADER LINE,
      IT_IO_ACTUAL TYPE TABLE OF ZFI_IO_ACTUAL WITH HEADER LINE,
      IT_BUDGET    TYPE TABLE OF ZFI_PI_BUDGET WITH HEADER LINE,
      IT_ACTUAL    TYPE TABLE OF ZFI_PI_ACTUAL_ACT WITH HEADER LINE,
      IT_IMAK      TYPE TABLE OF TY_IMAK WITH HEADER LINE,
      ITAB         TYPE TABLE OF TY_ITAB WITH HEADER LINE.

DATA: BEGIN OF GT_OUT OCCURS 0,
        POSID    LIKE IMPR-POSID,
        KOSTL    TYPE KOSTL,
        TXT50    LIKE IMAKT-TXT50,
*        GJAHR    LIKE IMPR-GJAHR,
        AUFNR    LIKE AUFK-AUFNR,
        OBJNR    LIKE IMPR-OBJNR,
        PRNAM    LIKE ZTFI_IMFM-PRNAM,
        ARPLAN   LIKE ZTFI_IMFM-TOT,
        PLAN     LIKE ZTFI_IMFM-TOT,
        ORG_AMT  LIKE ZTFI_IMFM-TOT,
        SUP_AMT  LIKE ZTFI_IMFM-TOT,
        RET_AMT  LIKE ZTFI_IMFM-TOT,
        CUR_AMT  LIKE ZTFI_IMFM-TOT,
        IO_AMT   LIKE ZTFI_IMFM-TOT,
        PR_AMT   LIKE ZTFI_IMFM-TOT,
        PO_AMT   LIKE ZTFI_IMFM-TOT,
        ACT_AMT  LIKE ZTFI_IMFM-TOT,
        ASS_AMT  LIKE ZTFI_IMFM-TOT,
        RES_AMT  LIKE ZTFI_IMFM-TOT,

*Issue Number : FI-20041118-005, Requested by YCYOON
*Changed on 2004/12/10, by WSKIM
*---Start
        DOW_AMT  LIKE ZTFI_IMFM-TOT,
*---End
        DIFA_AMT LIKE ZTFI_IMFM-TOT,
        DIFB_AMT LIKE ZTFI_IMFM-TOT,
        DIF_AMT  LIKE ZTFI_IMFM-TOT,
        DPCR_AMT LIKE ZTFI_IMFM-TOT,     " Balance for DP
        AR_RATE  LIKE ZTFI_IMFM-TOT,     " AR %Progress
        PI_RATE  LIKE ZTFI_IMFM-TOT,     " PI %Progress
        STAT,    " Status: 1 Created 2 Rel 3 Locked
        CRTD,
        REL,
        LKD,
        ARVRNT   TYPE IMA_VARNT,         " AR Variant
        CHK,
        LIGHT    TYPE C,
        ICON     TYPE ICON_D,
        TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV,
    END OF GT_OUT.

*--summary by activity
DATA : BEGIN OF IT_SUM OCCURS 0,
        POSID LIKE IMPR-POSID,
        GJAHR LIKE IMPR-GJAHR,
        PRNAM LIKE ZTFI_IMFM-PRNAM,
        OBJNR LIKE IMPR-OBJNR,
        KOSTL TYPE KOSTL,
        GUBUN LIKE ZTFI_IMFM-GUBUN,
        TOT   LIKE ZTFI_IMFM-TOT,
        STAT,     " Status: 1 Created 2 Rel 3 Locked
       END OF IT_SUM.
*---Temp
*----for combox
DATA: IT_VAL TYPE VRM_VALUES,
      W_LINE LIKE LINE OF IT_VAL.
*---WORK AREA
DATA : WA_T_CNT      TYPE I,
*      WA_T_CNT1     TYPE I,
       WA_PRNAM      LIKE  ZTFI_IMFM-PRNAM,
       WA_BEFORE     LIKE  IMZO-GJAHR,
       WA_LAST       LIKE  IMZO-GJAHR,
       WA_YEAR       LIKE  IMZO-GJAHR,
       WA_YEAR1      LIKE  IMZO-GJAHR,
       WA_YEAR2      LIKE  IMZO-GJAHR,
       WA_YEAR3      LIKE  IMZO-GJAHR,
       WA_YEAR4      LIKE  IMZO-GJAHR,
       WA_YEAR5      LIKE  IMZO-GJAHR,
       WA_YEAR6      LIKE  IMZO-GJAHR,
       WA_AFTER      LIKE  IMZO-GJAHR,
       WA_BEFORE_TXT(10),
       WA_AFTER_TXT(10),
       WA_ORG_AMT    LIKE  ZTFI_IMFM-TOT,
       WA_SUP_AMT    LIKE  ZTFI_IMFM-TOT,
       WA_RET_AMT    LIKE  ZTFI_IMFM-TOT,
       WA_CUR_AMT    LIKE  ZTFI_IMFM-TOT,
       WA_BEFORE_AMT LIKE  COSP-WTG001,
       WA_AFTER_AMT  LIKE  COSP-WTG001,
*---Currenty
        WA_N_TOT     LIKE  COSP-WTG001,
        WA_N_BEFORE  LIKE  COSP-WTG001,
        WA_N_LAST    LIKE  COSP-WTG001,
        WA_N_YEAR    LIKE  COSP-WTG001,
        WA_N_YEAR1   LIKE  COSP-WTG001,
        WA_N_YEAR2   LIKE  COSP-WTG001,
        WA_N_YEAR3   LIKE  COSP-WTG001,
        WA_N_YEAR4   LIKE  COSP-WTG001,
        WA_N_YEAR5   LIKE  COSP-WTG001,
        WA_N_AFTER   LIKE  COSP-WTG001.
*---Actual
DATA :  WA_A_TOT     LIKE  COSP-WTG001,
        WA_A_BEFORE  LIKE  COSP-WTG001,
        WA_A_LAST    LIKE  COSP-WTG001,
        WA_A_YEAR    LIKE  COSP-WTG001,
        WA_A_YEAR1   LIKE  COSP-WTG001,
        WA_A_YEAR2   LIKE  COSP-WTG001,
        WA_A_YEAR3   LIKE  COSP-WTG001,
        WA_A_YEAR4   LIKE  COSP-WTG001,
        WA_A_YEAR5   LIKE  COSP-WTG001,
        WA_A_AFTER   LIKE  COSP-WTG001.
*---Commitment
DATA :  WA_C_TOT     LIKE  COSP-WTG001,
        WA_C_BEFORE  LIKE  COSP-WTG001,
        WA_C_LAST    LIKE  COSP-WTG001,
        WA_C_YEAR    LIKE  COSP-WTG001,
        WA_C_YEAR1   LIKE  COSP-WTG001,
        WA_C_YEAR2   LIKE  COSP-WTG001,
        WA_C_YEAR3   LIKE  COSP-WTG001,
        WA_C_YEAR4   LIKE  COSP-WTG001,
        WA_C_YEAR5   LIKE  COSP-WTG001,
        WA_C_AFTER   LIKE  COSP-WTG001.
*---Downpayment
DATA :  WA_D_TOT     LIKE  COSP-WTG001,
        WA_D_BEFORE  LIKE  COSP-WTG001,
        WA_D_LAST    LIKE  COSP-WTG001,
        WA_D_YEAR    LIKE  COSP-WTG001,
        WA_D_YEAR1   LIKE  COSP-WTG001,
        WA_D_YEAR2   LIKE  COSP-WTG001,
        WA_D_YEAR3   LIKE  COSP-WTG001,
        WA_D_YEAR4   LIKE  COSP-WTG001,
        WA_D_YEAR5   LIKE  COSP-WTG001,
        WA_D_AFTER   LIKE  COSP-WTG001.

* for BDC
DATA: GT_BDC TYPE TABLE OF BDCDATA    WITH HEADER LINE,
      GT_MSG TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      GS_OPT LIKE CTU_PARAMS.

*----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
* General selections
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-010.
SELECT-OPTIONS S_PRNAM FOR IMPR-PRNAM MEMORY ID IMT OBLIGATORY.
* ...Position ID
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) C_PI.
PARAMETER R_1 RADIOBUTTON GROUP R1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS S_POSID   FOR   IMPR-POSID.
SELECTION-SCREEN END OF LINE.

* ...Internal Order
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) C_OR.
PARAMETER R_2 RADIOBUTTON GROUP R1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS S_AUFNR  FOR   AUFK-AUFNR.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B0.

* Run Parameter
SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE C010.
PARAMETER P_AYEAR LIKE IMPR-GJAHR   MEMORY ID GJR OBLIGATORY
                                      DEFAULT SY-DATUM+0(4).
SELECT-OPTIONS S_GJAHR FOR IMPR-GJAHR .
PARAMETERS: P_MON(2)  TYPE N,
            P_AUTH(1) TYPE C DEFAULT 'X' NO-DISPLAY,
            P_ARVER   LIKE IMAVZ-VERSI DEFAULT 'IM'.
SELECTION-SCREEN END OF BLOCK S1.

* Select option
SELECTION-SCREEN BEGIN OF BLOCK S3 WITH FRAME TITLE C030.
SELECT-OPTIONS: S_KOSTL    FOR IMPR-KOSTL,
                S_ERGSO    FOR IMPR-ERGSO.
* AR Selection
SELECT-OPTIONS: S_VKOKRS  FOR IMAK-VKOKRS,
                S_USR03   FOR IMAK-USR03,
                S_AGJAHR  FOR IMAK-GJAHR,
                S_IVART   FOR IMAK-IVART.
SELECTION-SCREEN END OF BLOCK S3.

* Status
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_CRTD AS CHECKBOX DEFAULT 'X',
            P_REL  AS CHECKBOX DEFAULT 'X',
            P_LKD  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.

* Depreciation Selection
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_S AS CHECKBOX,           " Simulation
            P_DARVER LIKE IMAVZ-VERSI.
SELECTION-SCREEN END OF BLOCK B4.

* Select layout
SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-004.
PARAMETER P_LAYOUT LIKE DISVARIANT-VARIANT.   " LAYOUT
SELECTION-SCREEN END OF BLOCK B5.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = 'P_ACT'
            VALUES = IT_VAL.
*----------------------------------------------------------------------
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LAYOUT.
  PERFORM F4_VARIANT CHANGING P_LAYOUT.

*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
* ==> Change Variant saving type
  WA_VAR_SAVE = 'A'.
* ==> Change first mode   GRID or LIST
  WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
*  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : GT_FIELDCAT.
  CLEAR   : GS_LAYOUT.
*--title set
  C010 = 'Run Parameter'.
*  c020 = 'Select option'.
  C030 = 'Select option'.
  WA_REPID = SY-REPID.
  C_PI = 'Position ID'.
  C_OR = 'Internal Order'.

* for combo box
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
END-OF-SELECTION.
* ==> 5. build field category
  IF R_1 = 'X'.
    PERFORM BUILD_FIELD_CATEGORY
    USING :
     'POSID'     'Position ID'     '16' 'X' 'L',
     'TXT50'     'Description'     '25' 'X' 'L',
     'KOSTL'     'Cost Ctr'        '10' ' ' 'L',
     'CRTD'      'Created'         '1'  ' ' 'C',
     'REL'       'Released'        '1'  ' ' 'C',
     'LKD'       'Locked'          '1'  ' ' 'C',
     'ARPLAN'    'AR Plan'         '16' ' ' 'R',
     'PLAN'      'PI Plan'         '16' ' ' 'R',
     'ORG_AMT'   'A.Original'      '16' ' ' 'R',
     'SUP_AMT'   'B.Supplement'    '16' ' ' 'R',
     'RET_AMT'   'C.Return'        '16' ' ' 'R',
     'CUR_AMT'   'D.Cur(A+B+C)'    '16' ' ' 'R',
     'IO_AMT'    'E.IO Budget'     '16' ' ' 'R',
     'PR_AMT'    'F.Commit-PR'     '16' ' ' 'R' ,
     'PO_AMT'    'G.Cmmit-PO'      '16' ' ' 'R',
     'ACT_AMT'   'H.Actual'        '16' ' ' 'R',
     'ASS_AMT'   'I.Assign(F+G+H)' '16' ' ' 'R',
     'RES_AMT'   'J.Avail.(D-I)'   '16' ' ' 'R',
     'DOW_AMT'   'Downpayment'     '16' ' ' 'R',
     'DIFA_AMT'  'Org-Act'         '16' ' ' 'R',
     'DIFB_AMT'  'Cur-Act'         '16' ' ' 'R',
     'DPCR_AMT'  'Balance for DP'  '16' ' ' 'R',
     'AR_RATE'   'AR %Progress'    '16' ' ' 'R',
     'PI_RATE'   'PI %Progress'    '16' ' ' 'R',
     'ICON'      'Created Depr.'   '6'  ' ' 'C'.

  ELSE.
    PERFORM BUILD_FIELD_CATEGORY
    USING :
     'POSID'     'Position ID'     '16' 'X' 'L',
     'TXT50'     'Description'     '25' 'X' 'L',
     'KOSTL'     'Cost Ctr'        '10' ' ' 'L',
     'CRTD'      'Created'         '1'  ' ' 'C',
     'REL'       'Released'        '1'  ' ' 'C',
     'LKD'       'Locked'          '1'  ' ' 'C',
     'AUFNR'     'I.Order'         '10' 'X' 'L',
     'PLAN'      'Plan'            '16' ' ' 'R',
     'ORG_AMT'   'A.Original'      '16' ' ' 'R',
     'SUP_AMT'   'B.Supplement'    '16' ' ' 'R',
     'RET_AMT'   'C.Return'        '16' ' ' 'R',
     'CUR_AMT'   'D.Cur(A+B+C)'    '16' ' ' 'R',
     'IO_AMT'    'E.IO Budget'     '16' ' ' 'R',
     'PR_AMT'    'F.Commit-PR'     '16' ' ' 'R',
     'PO_AMT'    'G.Cmmit-PO'      '16' ' ' 'R',
     'ACT_AMT'   'H.Actual'        '16' ' ' 'R',
     'ASS_AMT'   'I.Assign(F+G+H)' '16' ' ' 'R',
     'RES_AMT'   'J.Avail.(D-I)'   '16' ' ' 'R',
     'DOW_AMT'   'Downpayment'     '16' ' ' 'R',
     'DIFA_AMT'  'Org-Act'         '16' ' ' 'R',
     'DIFB_AMT'  'Cur-Act'         '16' ' ' 'R',
     'DPCR_AMT'  'Balance for DP'  '16' ' ' 'R',
     'RATE'      '%Progress'       '16' ' ' 'R',
     'ICON'      'Created Depr.'   '6'  ' ' 'C'.
  ENDIF.

* ==> 1. select data from db
  PERFORM SELECT_DATA.

  IF GT_OUT[] IS INITIAL.
    MESSAGE S000(ZMFI) WITH 'No found data '.
    EXIT.
  ENDIF.

  PERFORM MAKE_OUT.

* ==> 2. set variant default
  PERFORM SET_VARIANT CHANGING WA_VAR.
* ==> 3. set layout for alv style
  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.
* ==> 4. set events for alv
  PERFORM SET_EVENTS CHANGING GT_EVENTS.
* ==> 5. call function display alv.

  CALL FUNCTION WA_ALV_FUNCTION_NAME
       EXPORTING
            I_CALLBACK_PROGRAM       = WA_REPID
            I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
            I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
            IS_LAYOUT                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_SPECIAL_GROUPS        = GT_SP_GROUP[]
            IT_SORT                  = GT_SORTS[]
            I_DEFAULT                = WA_DEFAULT
            I_SAVE                   = WA_VAR_SAVE
            IS_VARIANT               = WA_VAR
            IT_EVENTS                = W_EVENTCAT[]
            IS_PRINT                 = GS_PRNT
       TABLES
            T_OUTTAB                 = GT_OUT.

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM SELECT_DATA.
  REFRESH : IT_IMFM, IT_SUM, GT_OUT.
  CLEAR   : IT_IMFM, IT_SUM, GT_OUT.

* Position ID
  IF R_1 = 'X'.
    PERFORM CBO_PI_PROCESS.
* Order
  ELSE.
    PERFORM ORDER_RPOCESS.
  ENDIF.

ENDFORM.                    " select_data

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM ALV_EVENT_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  IF WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID'.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.
  ENDIF.
  SET TITLEBAR  'STANDARD'.

ENDFORM.                    "alv_event_pf_status_set
*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM ALV_EVENT_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                                  RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE GT_OUT INDEX RS_SELFIELD-TABINDEX.
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'POSID'.
          SET PARAMETER ID 'IMT' FIELD S_PRNAM-LOW.
          SET PARAMETER ID 'IMP' FIELD GT_OUT-POSID.
          SET PARAMETER ID 'GJR' FIELD P_AYEAR.
          CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.
      ENDCASE.
*   ---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM SWITCH_LIST_OR_GRID USING R_UCOMM.

    WHEN 'PRE' OR 'NEXT'.
      PERFORM CHANGE_STATUS USING R_UCOMM.        " Change status
      RS_SELFIELD-REFRESH = 'X'.

    WHEN 'DPCR'.
      PERFORM CREATE_DPCR.             " Create depreciation data
      RS_SELFIELD-REFRESH = 'X'.

    WHEN 'CAL1' OR 'CAL2' OR 'CAL3'.
      PERFORM CAL_DPCR_AMT USING R_UCOMM.
      RS_SELFIELD-REFRESH = 'X'.

  ENDCASE.

  CHECK R_UCOMM EQ 'LIST' OR
        R_UCOMM EQ 'GRID'.

  RS_SELFIELD-EXIT = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
FORM SET_VARIANT CHANGING CS_VARI TYPE DISVARIANT.
  CHECK P_LAYOUT NE SPACE.

  CS_VARI-REPORT      = SY-REPID.
  CS_VARI-HANDLE      = SPACE.
  CS_VARI-LOG_GROUP   = SPACE.
  CS_VARI-USERNAME    = SPACE.
  CS_VARI-VARIANT     = P_LAYOUT.
  CS_VARI-TEXT        = SPACE.
  CS_VARI-DEPENDVARS  = SPACE.

ENDFORM.                    " set_variant
*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
FORM SET_EVENTS CHANGING CT_EVENTS TYPE SLIS_T_EVENT.

  FIELD-SYMBOLS: <LS_EVENT> TYPE SLIS_ALV_EVENT.

  DATA: L_EVENT TYPE LVC_FNAME.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE     = 0
       IMPORTING
            ET_EVENTS       = CT_EVENTS
       EXCEPTIONS
            LIST_TYPE_WRONG = 1
            OTHERS          = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    DELETE CT_EVENTS WHERE NAME NE 'END_OF_PAGE'
                       AND NAME NE 'TOP_OF_PAGE'
                       AND NAME NE 'TOP_OF_LIST'
                       AND NAME NE 'END_OF_LIST'.
    LOOP AT CT_EVENTS ASSIGNING <LS_EVENT>.
      CONCATENATE 'ALV_EVENT_'
                  <LS_EVENT>-NAME
                  INTO <LS_EVENT>-FORM.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts
*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM SET_LAYOUT CHANGING CS_LAYO TYPE SLIS_LAYOUT_ALV.
  CS_LAYO-NUMC_SUM               = 'X'.
  CS_LAYO-GROUP_BUTTONS          = 'X'.
  CS_LAYO-GROUP_CHANGE_EDIT      = 'X'.
  CS_LAYO-DETAIL_POPUP           = 'X'.
  CS_LAYO-COLTAB_FIELDNAME       = 'TABCOLOR'.
  CS_LAYO-LIST_APPEND            = SPACE.
  CS_LAYO-COLWIDTH_OPTIMIZE      = 'X'.
  CS_LAYO-BOX_FIELDNAME          = 'CHK'.

ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM DISPLAY_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = W_TOP_OF_PAGE.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM SWITCH_LIST_OR_GRID USING R_UCOMM.
  DATA: LS_VARI      TYPE DISVARIANT,
        LS_SLIS_LAYO TYPE SLIS_LAYOUT_ALV,
        LT_SLIS_FCAT TYPE SLIS_T_FIELDCAT_ALV,
        LT_SLIS_SORT TYPE SLIS_T_SORTINFO_ALV,
        LT_SLIS_FILT TYPE SLIS_T_FILTER_ALV,
        LS_SLIS_PRNT TYPE SLIS_PRINT_ALV.

  IF R_UCOMM = 'LIST' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.

  IF R_UCOMM = 'GRID' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.

  CASE WA_ALV_FUNCTION_NAME.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.
  ENDCASE.

  CALL FUNCTION WA_ALV_GET_INFO_NAME
       IMPORTING
            ES_LAYOUT     = LS_SLIS_LAYO
            ET_FIELDCAT   = LT_SLIS_FCAT
            ET_SORT       = LT_SLIS_SORT
            ET_FILTER     = LT_SLIS_FILT
            ES_VARIANT    = LS_VARI
       EXCEPTIONS
            NO_INFOS      = 1
            PROGRAM_ERROR = 2
            OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF R_UCOMM = 'LIST'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
              IT_EVENTS                = GT_EVENTS[]
         TABLES
              T_OUTTAB                 = GT_OUT
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  ENDIF.

  IF R_UCOMM = 'GRID'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
         TABLES
              T_OUTTAB                 = GT_OUT
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM BUILD_FIELD_CATEGORY USING   P_FIELDNAME       " field name
                                  P_TITLE           " field title
                                  P_OUTPUTLEN       " length
                                  P_KEY
                                  P_JUST.

  DATA LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.

  IF P_FIELDNAME = 'ICON'.
    LS_FIELDCAT-ICON = 'X'.
  ENDIF.

  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM F4_VARIANT CHANGING C_VARIANT TYPE DISVARIANT-VARIANT.
  DATA: LS_VARIANT TYPE DISVARIANT,
        L_EXIT     TYPE CHAR1.

  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = LS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            E_EXIT     = L_EXIT
            ES_VARIANT = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND  = 2.

  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF L_EXIT EQ SPACE.
      C_VARIANT = LS_VARIANT-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM BUILD_SORT_TABLE USING  P_SPOS
                             P_FIELDNAME
                             P_UP
                             P_SUBTOT
                             P_GROUP.
  DATA LS_SORT TYPE SLIS_SORTINFO_ALV.

  LS_SORT-SPOS      = P_SPOS.
  LS_SORT-FIELDNAME = P_FIELDNAME.
  LS_SORT-UP        = P_UP.
  LS_SORT-SUBTOT    = P_SUBTOT.
  LS_SORT-GROUP     = P_GROUP.
  APPEND LS_SORT TO GT_SORTS.

ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM SET_LINE_COLOR USING    P_COLOR.
  DATA: LS_FIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        LT_COLOR      TYPE SLIS_T_SPECIALCOL_ALV,
        LS_COLOR      TYPE SLIS_SPECIALCOL_ALV.

  REFRESH LT_COLOR.
  CLEAR   LT_COLOR.

  LOOP AT GT_FIELDCAT INTO LS_FIELDCAT.
    LS_COLOR-FIELDNAME = LS_FIELDCAT-FIELDNAME.
    LS_COLOR-COLOR-COL = P_COLOR.
    LS_COLOR-COLOR-INT = CL_GUI_RESOURCES=>LIST_INTENSIFIED.
    LS_COLOR-COLOR-INV = 0.
    LS_COLOR-NOKEYCOL  = 'X'.
    APPEND LS_COLOR TO LT_COLOR.
    GT_OUT-TABCOLOR = LT_COLOR.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  build_field_category1
*&---------------------------------------------------------------------*
FORM BUILD_FIELD_CATEGORY1 USING
                                  P_FIELDNAME       " field name
                                  P_TITLE           " field title
                                  P_OUTPUTLEN       " length
                                  P_KEY             "
                                  P_JUST            "
                                  P_NOOUT           "
                                  P_EDIT            "
                                  P_CFIELD          " currency field nam
                                  P_QFIELD          " quantity field nam
                                  .

  DATA LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.

  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.
ENDFORM.                    " build_field_category1
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
FORM COMMENT_BUILD USING  LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER,
          L_MANAGER(50),
          L_DATE(50),
          L_LIST(50),
          L_DSNAM LIKE T024D-DSNAM,
          L_H_DSNAM LIKE T024D-DSNAM,
          L_LDATE(10),
          L_HDATE(10).
*-------------- HEADER
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = TEXT-H01.     "HEADER TITLE (H001)
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Investment program : '.
  LS_LINE-INFO = S_PRNAM-LOW.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*--
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Approval Year : '.
  LS_LINE-INFO = P_AYEAR.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Fiscal Year : '.
*  ls_line-info = S_GJAHR-LOW.
  CONCATENATE   S_GJAHR-LOW  ' ~'  S_GJAHR-HIGH INTO L_LIST.
  LS_LINE-INFO = L_LIST.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*--
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Position ID : '.
  CONCATENATE   S_POSID-LOW  ' ~'  S_POSID-HIGH INTO L_LIST.
  LS_LINE-INFO = L_LIST.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
FORM SET_BUILD_EVENT.
  W_EVENTCAT-NAME = 'TOP_OF_PAGE'.
  W_EVENTCAT-FORM = 'DISPLAY_HEADER'.
  APPEND W_EVENTCAT.

ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  get_pi_plan
*&---------------------------------------------------------------------*
FORM GET_PI_PLAN USING    U_POSID
                          U_AYEAR
                          U_PRNAM.
  REFRESH IT_BUDGET.
  CLEAR   IT_BUDGET.

  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
       EXPORTING
            POSID = U_POSID
            PRNAM = U_PRNAM
            GJAHR = U_AYEAR
       TABLES
            OUT   = IT_BUDGET.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
  IF  S_GJAHR-LOW = SPACE.
    READ TABLE IT_BUDGET WITH KEY POSID = U_POSID
                                 GJAHR = '1111'.
    IF SY-SUBRC = 0.
      MOVE IT_BUDGET-PLAN TO GT_OUT-PLAN.
    ENDIF.
  ELSE.
    READ TABLE IT_BUDGET WITH KEY POSID = U_POSID
                                  GJAHR = S_GJAHR-LOW.
    IF SY-SUBRC = 0.
      MOVE IT_BUDGET-PLAN TO GT_OUT-PLAN.
    ENDIF.
  ENDIF.
*---End
ENDFORM.                    " get_pi_plan
*&---------------------------------------------------------------------*
*&      Form  get_pi_ACTUAL
*&---------------------------------------------------------------------*
FORM GET_PI_ACTUAL USING    U_POSID
                            U_AYEAR
                            U_PRNAM.
  DATA: L_NAME(20),
        L_TOT TYPE WTGXXX,
        N(2) TYPE N.

  FIELD-SYMBOLS <FS> TYPE ANY.

  REFRESH IT_ACTUAL.
  CLEAR   IT_ACTUAL.

  CALL FUNCTION 'Z_FFI_GET_PI_ACTUAL_ACT'
       EXPORTING
            POSID = U_POSID
            GJAHR = U_AYEAR
            PRNAM = U_PRNAM
       TABLES
            OUT   = IT_ACTUAL.

  IF P_MON IS INITIAL.
*---2004/03/23
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*  LOOP AT it_actual WHERE gjahr IN s_gjahr
*                    AND   ippos = ' '.
    LOOP AT IT_ACTUAL WHERE IPPOS <>  0.
      IF S_GJAHR-LOW <> ' '.
        CHECK IT_ACTUAL-GJAHR = S_GJAHR-LOW.
      ENDIF.
*---End
      CASE IT_ACTUAL-WRTTP.
        WHEN '21'. "PR
          ADD IT_ACTUAL-TOT TO GT_OUT-PR_AMT.
        WHEN '22'. "PO
          ADD IT_ACTUAL-TOT TO GT_OUT-PO_AMT.
        WHEN '04' OR '11'. " ACTUAL
          ADD IT_ACTUAL-TOT TO GT_OUT-ACT_AMT.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
        WHEN '12'.  "downpayment
          ADD IT_ACTUAL-TOT TO GT_OUT-DOW_AMT.
*---End
      ENDCASE.
    ENDLOOP.

  ELSE.
    N = P_MON.

    LOOP AT IT_ACTUAL WHERE IPPOS <> 0.
      IF S_GJAHR-LOW <> ' '.
        CHECK IT_ACTUAL-GJAHR = S_GJAHR-LOW.
      ENDIF.

      CLEAR L_TOT.
      DO P_MON TIMES.
        CLEAR L_NAME.
        CONCATENATE 'IT_ACTUAL-WTG0' N INTO L_NAME.
        ASSIGN (L_NAME) TO <FS>.
        L_TOT = L_TOT + <FS>.

        N = N + 1.
      ENDDO.

      CASE IT_IO_ACTUAL-WRTTP.
        WHEN '21'. "PR
          ADD L_TOT TO GT_OUT-PR_AMT.
        WHEN '22'. "P0
          ADD L_TOT TO GT_OUT-PO_AMT.
        WHEN '04' OR '11'.
          ADD L_TOT TO GT_OUT-ACT_AMT.
        WHEN '12' .
          ADD L_TOT TO GT_OUT-DOW_AMT.
      ENDCASE.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " get_pi_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  get_io_plan
*&---------------------------------------------------------------------*
FORM GET_IO_PLAN USING    U_POSID
                          U_AYEAR
                          U_PRNAM.

  REFRESH IT_BUDGET.
  CLEAR   IT_BUDGET.

  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET_IO'
       EXPORTING
            POSID = U_POSID
            PRNAM = U_PRNAM
            GJAHR = U_AYEAR
       TABLES
            OUT   = IT_BUDGET.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
  IF  S_GJAHR-LOW = SPACE.
    READ TABLE IT_BUDGET WITH KEY POSID = U_POSID
                                 GJAHR = '1111'.
    IF SY-SUBRC = 0.
      MOVE IT_BUDGET-WTJHR TO GT_OUT-IO_AMT.
    ENDIF.
  ELSE.
    READ TABLE IT_BUDGET WITH KEY POSID = U_POSID
                                  GJAHR = S_GJAHR-LOW.
    IF SY-SUBRC = 0.
      MOVE IT_BUDGET-WTJHR TO GT_OUT-IO_AMT.
    ENDIF.
  ENDIF.
*---End
ENDFORM.                    " get_io_plan
*&---------------------------------------------------------------------*
*&      Form  order_rpocess
*&---------------------------------------------------------------------*
FORM ORDER_RPOCESS.
*  SELECT * INTO TABLE IT_AUFK FROM AUFK
*    WHERE AUFNR IN S_AUFNR
*    AND   KOSTL IN S_KOSTL.
*
**--GET IMZO
*  CLEAR WA_T_CNT.
*  DESCRIBE TABLE IT_AUFK LINES WA_T_CNT.
*  IF WA_T_CNT > 0.
*    SELECT * INTO TABLE IT_IMZO FROM IMZO
*    FOR ALL ENTRIES IN IT_AUFK
*    WHERE OBJNR = IT_AUFK-OBJNR.
*  ENDIF.
*
**---GET IMPR
*  CLEAR WA_T_CNT.
*  DESCRIBE TABLE IT_IMZO LINES WA_T_CNT.
*  IF WA_T_CNT > 0.
*    SELECT * INTO TABLE IT_IMPR FROM IMPR
*    FOR ALL ENTRIES IN IT_IMZO
*    WHERE POSNR = IT_IMZO-POSNR
*    AND   POSID IN S_POSID
*    AND   GJAHR = IT_IMZO-GJAHR
*    AND   PRNAM IN S_PRNAM.
*  ENDIF.
**====*
*
*  CLEAR GT_OUT.
*  LOOP AT IT_AUFK.
*    MOVE-CORRESPONDING IT_AUFK TO GT_OUT.
*    READ TABLE IT_IMZO WITH KEY OBJNR = IT_AUFK-OBJNR
*                                GJAHR = P_AYEAR.
*    IF SY-SUBRC = 0.
*      READ TABLE IT_IMPR WITH KEY POSNR = IT_IMZO-POSNR
*                                  GJAHR = IT_IMZO-GJAHR.
*      IF SY-SUBRC = 0.
*        MOVE IT_IMPR-POSID TO GT_OUT-POSID.
*        PERFORM GET_IO_BUDGET USING IT_AUFK-AUFNR.
*      ENDIF.
*    ENDIF.
*
**----2004/04/07
*    PERFORM GET_IO_ACTUAL USING IT_AUFK-AUFNR.
*
**<< Start of addition on 10.18.2006 by Michelle
*    IF GT_OUT-ASS_AMT <> 0 AND GT_OUT-CUR_AMT <> 0.
*      GT_OUT-AR_RATE = GT_OUT-ASS_AMT / GT_OUT-CUR_AMT.
*    ENDIF.
** End of addition on 10.18.2006 by Michelle >>
*
*    APPEND GT_OUT.
*    CLEAR  GT_OUT.
*  ENDLOOP.
  DATA: L_INDX  TYPE I,
        L_POSNR TYPE IMA_POSNR,
        L_CHK VALUE 'X',
        L_STAT,
        L_CRTD,
        L_REL,
        L_LKD,
        L_AYEAR TYPE GJAHR.

  CLEAR: ITAB, GT_OUT, L_AYEAR.
  REFRESH: ITAB, GT_OUT.

  IF P_S = 'X'.
    L_AYEAR = P_AYEAR - 1.
  ELSE.
    L_AYEAR = P_AYEAR.
  ENDIF.

  SELECT A~AUFNR C~POSNR C~POSID C~PRNAM C~OBJNR A~KOSTL
    INTO TABLE ITAB
    FROM AUFK AS A
    JOIN IMZO AS B
      ON B~OBJNR = A~OBJNR
    JOIN IMPR AS C
      ON C~POSNR = B~POSNR
     AND C~GJAHR = B~GJAHR
   WHERE A~AUFNR IN S_AUFNR
     AND A~KOSTL IN S_KOSTL
     AND C~POSID IN S_POSID
     AND C~PRNAM IN S_PRNAM
     AND C~GJAHR = L_AYEAR.
  CLEAR L_INDX.

  LOOP AT ITAB.
    L_INDX = L_INDX + 1.

    CLEAR L_POSNR.
    SELECT SINGLE POSNR INTO L_POSNR
      FROM IMAK
     WHERE POSID = ITAB-POSID
       AND IVART  IN S_IVART
       AND VKOKRS IN S_VKOKRS
       AND GJAHR  IN S_AGJAHR
       AND USR03  IN S_USR03.

    IF SY-SUBRC <> 0.
      DELETE ITAB INDEX L_INDX.
      CONTINUE.
    ELSE.
      CLEAR: L_STAT, L_CRTD, L_REL, L_LKD.

      PERFORM CHK_STATUS USING    ITAB-OBJNR
                         CHANGING L_STAT
                                  L_CRTD
                                  L_REL
                                  L_LKD
                                  L_CHK.

      IF L_CHK IS INITIAL.
        GT_OUT-POSID = ITAB-POSID.
        GT_OUT-AUFNR = ITAB-AUFNR.
        GT_OUT-OBJNR = ITAB-OBJNR.
        GT_OUT-KOSTL = ITAB-KOSTL.
        GT_OUT-PRNAM = ITAB-PRNAM.

        PERFORM GET_IO_BUDGET USING ITAB-AUFNR.
        PERFORM GET_IO_ACTUAL USING ITAB-AUFNR.

        GT_OUT-STAT = L_STAT.
        GT_OUT-CRTD = L_CRTD.
        GT_OUT-REL = L_REL.
        GT_OUT-LKD = L_LKD.

        APPEND GT_OUT.
        CLEAR  GT_OUT.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " order_rpocess
*&---------------------------------------------------------------------*
*&      Form  cbo_pi_process
*&---------------------------------------------------------------------*
FORM CBO_PI_PROCESS.
  DATA L_AYEAR TYPE IM_GNJHR.

  CLEAR L_AYEAR.

  IF P_S = 'X'.
    L_AYEAR = P_AYEAR - 1.
  ELSE.
    L_AYEAR = P_AYEAR.
  ENDIF.

  CLEAR IT_IMPR.
  REFRESH IT_IMPR.

  SELECT A~POSID A~OBJNR A~GJAHR B~POSID A~KOSTL
     INTO TABLE IT_IMPR
     FROM IMPR AS A
     INNER JOIN IMAK AS B
        ON B~POSID = A~POSID
     WHERE A~GJAHR = L_AYEAR
       AND A~POSID IN S_POSID
       AND A~PRNAM IN S_PRNAM
       AND A~KOSTL IN S_KOSTL
       AND A~ERGSO IN S_ERGSO
*     AND B~POSNR  IN S_POSNR   "ERROR
       AND B~IVART  IN S_IVART
       AND B~VKOKRS IN S_VKOKRS
       AND B~GJAHR  IN S_AGJAHR
       AND B~USR03  IN S_USR03.

  CLEAR WA_T_CNT.
  DESCRIBE TABLE IT_IMPR LINES WA_T_CNT.

  IF WA_T_CNT > 0.
    SELECT * INTO TABLE IT_IMFM
      FROM ZTFI_IMFM
       FOR ALL ENTRIES IN IT_IMPR
     WHERE POSID = IT_IMPR-POSID
       AND POSID IN S_POSID
       AND AYEAR = L_AYEAR
*Issue Number : FI-20041118-005, Requested by YCYOON
*Changed on 2004/12/09, by WSKIM
*---Start
       AND STATUS = 'A'
*---End
       AND GJAHR IN S_GJAHR
       AND PRNAM IN S_PRNAM
       AND KOSTL IN S_KOSTL.
  ENDIF.

  IF NOT IT_IMFM[] IS INITIAL.
    PERFORM GET_IT_SUM.
  ENDIF.

  IF NOT IT_SUM[] IS INITIAL.
    DATA: L_CHK VALUE 'X',
          L_STAT,
          L_CRTD,
          L_REL,
          L_LKD.

    CLEAR: WA_ORG_AMT, WA_SUP_AMT, WA_RET_AMT, WA_PRNAM.

    LOOP AT IT_SUM.
      GT_OUT-POSID = IT_SUM-POSID.
      GT_OUT-PRNAM = IT_SUM-PRNAM.
      GT_OUT-OBJNR = IT_SUM-OBJNR.
      WA_PRNAM = IT_SUM-PRNAM.

      CASE IT_SUM-GUBUN.
        WHEN '1'.
          MOVE IT_SUM-TOT TO GT_OUT-ORG_AMT.
        WHEN '2'.
          MOVE IT_SUM-TOT TO GT_OUT-SUP_AMT.
        WHEN '3'.
          MOVE IT_SUM-TOT TO GT_OUT-RET_AMT.
      ENDCASE.

      GT_OUT-CUR_AMT
        = GT_OUT-ORG_AMT + GT_OUT-SUP_AMT + GT_OUT-RET_AMT.

      AT END OF POSID.
        CLEAR: L_STAT, L_CRTD, L_REL, L_LKD.

        PERFORM CHK_STATUS USING    GT_OUT-OBJNR
                           CHANGING L_STAT
                                    L_CRTD
                                    L_REL
                                    L_LKD
                                    L_CHK.

        IF L_CHK IS INITIAL.
          IF P_S = 'X'.
            CLEAR L_AYEAR.
            L_AYEAR = P_AYEAR - 1.
          ENDIF.

          PERFORM GET_AR_PLAN USING IT_SUM-POSID P_AYEAR.
          PERFORM GET_PI_PLAN USING IT_SUM-POSID L_AYEAR WA_PRNAM.
          PERFORM GET_IO_PLAN USING IT_SUM-POSID L_AYEAR WA_PRNAM.
          PERFORM GET_PI_ACTUAL USING IT_SUM-POSID L_AYEAR WA_PRNAM.

          GT_OUT-DIFA_AMT = GT_OUT-ACT_AMT - GT_OUT-ORG_AMT.
          GT_OUT-DIFB_AMT = GT_OUT-ACT_AMT - GT_OUT-CUR_AMT.

          IF GT_OUT-DPCR_AMT < 0.
            GT_OUT-DPCR_AMT = 0.
          ENDIF.

          GT_OUT-STAT = L_STAT.
          GT_OUT-CRTD = L_CRTD.
          GT_OUT-REL = L_REL.
          GT_OUT-LKD = L_LKD.

          APPEND GT_OUT.
          CLEAR: GT_OUT, WA_PRNAM.
        ENDIF.

      ENDAT.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " cbo_pi_process
*&---------------------------------------------------------------------*
*&      Form  GET_IO_BUDGET
*&---------------------------------------------------------------------*
FORM GET_IO_BUDGET USING    U_AUFNR.
  REFRESH : IT_IO_BUDGET.
  CLEAR   : IT_IO_BUDGET.
  CALL FUNCTION 'Z_FFI_GET_IO_BUDGET'
       EXPORTING
            AUFNR = U_AUFNR
       TABLES
            OUT   = IT_IO_BUDGET.
  READ TABLE IT_IO_BUDGET WITH KEY AUFNR = U_AUFNR
                                   GJAHR = '1111'.
  IF SY-SUBRC = 0.
    MOVE IT_IO_BUDGET-PLAN TO GT_OUT-PLAN.
    MOVE IT_IO_BUDGET-ORG  TO GT_OUT-ORG_AMT.
    MOVE IT_IO_BUDGET-SUPP TO GT_OUT-SUP_AMT.

    GT_OUT-CUR_AMT =  GT_OUT-ORG_AMT
                   +  GT_OUT-SUP_AMT - GT_OUT-RET_AMT.
    GT_OUT-IO_AMT =  GT_OUT-ORG_AMT
                   +  GT_OUT-SUP_AMT - GT_OUT-RET_AMT.

  ENDIF.

ENDFORM.                    " GET_IO_BUDGET
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
FORM GET_IO_ACTUAL USING    U_AUFNR.
  DATA: L_NAME(20),
        L_TOT TYPE WTGXXX,
        N(2) TYPE N.

  FIELD-SYMBOLS <FS> TYPE ANY.

  REFRESH IT_IO_ACTUAL.
  CLEAR   IT_IO_ACTUAL.

  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
       EXPORTING
            AUFNR = U_AUFNR
       TABLES
            OUT   = IT_IO_ACTUAL.

  IF P_MON IS INITIAL.
    CASE IT_IO_ACTUAL-WRTTP.
      WHEN '21'. "PR
        ADD IT_IO_ACTUAL-TOT TO GT_OUT-PR_AMT.
      WHEN '22'. "P0
        ADD IT_IO_ACTUAL-TOT TO GT_OUT-PO_AMT.
      WHEN '04' OR '11'.
        ADD IT_IO_ACTUAL-TOT TO GT_OUT-ACT_AMT.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
      WHEN '12' .
        ADD IT_IO_ACTUAL-TOT TO GT_OUT-DOW_AMT.
*---End
    ENDCASE.

  ELSE.
    N = P_MON.

    LOOP AT IT_IO_ACTUAL WHERE GJAHR IN S_GJAHR.
      CLEAR L_TOT.
      DO P_MON TIMES.
        CLEAR L_NAME.
        CONCATENATE 'IT_IO_ACTUAL-WTG0' N INTO L_NAME.
        ASSIGN (L_NAME) TO <FS>.
        L_TOT = L_TOT + <FS>.

        N = N + 1.
      ENDDO.

      CASE IT_IO_ACTUAL-WRTTP.
        WHEN '21'. "PR
          ADD L_TOT TO GT_OUT-PR_AMT.
        WHEN '22'. "P0
          ADD L_TOT TO GT_OUT-PO_AMT.
        WHEN '04' OR '11'.
          ADD L_TOT TO GT_OUT-ACT_AMT.
        WHEN '12' .
          ADD L_TOT TO GT_OUT-DOW_AMT.
      ENDCASE.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  MAKE_OUT
*&---------------------------------------------------------------------*
FORM MAKE_OUT.
  LOOP AT GT_OUT.
*Issue Number : FI-20041118-005, Requested by YCYOON
*Changed on 2004/12/09, by WSKIM
*---Start
* Available...
    GT_OUT-RES_AMT = GT_OUT-CUR_AMT - GT_OUT-ACT_AMT - GT_OUT-PO_AMT
                     - GT_OUT-PR_AMT.
*---End
    GT_OUT-ASS_AMT = GT_OUT-ACT_AMT + GT_OUT-PO_AMT + GT_OUT-PR_AMT.

    IF GT_OUT-ORG_AMT = 0.
      GT_OUT-DIFA_AMT = 0.
    ELSE.
      GT_OUT-DIFA_AMT = ( GT_OUT-ACT_AMT + GT_OUT-DOW_AMT )
                          / GT_OUT-ORG_AMT.
    ENDIF.

    IF GT_OUT-CUR_AMT = 0.
      GT_OUT-DIFB_AMT = 0.
    ELSE.
      GT_OUT-DIFB_AMT = ( GT_OUT-ACT_AMT + GT_OUT-DOW_AMT )
                          / GT_OUT-CUR_AMT.
    ENDIF.

    SELECT SINGLE TXT50 INTO GT_OUT-TXT50
        FROM IMAKT
        WHERE SPRAS = SY-LANGU
          AND POSNR = GT_OUT-POSID.

    IF GT_OUT-ASS_AMT <> 0 AND GT_OUT-ARPLAN <> 0.
      GT_OUT-AR_RATE = 100 * GT_OUT-ASS_AMT / GT_OUT-ARPLAN.
    ENDIF.

    IF GT_OUT-ASS_AMT <> 0 AND GT_OUT-CUR_AMT <> 0.
      GT_OUT-PI_RATE = 100 * GT_OUT-ASS_AMT / GT_OUT-CUR_AMT.
    ENDIF.

    MODIFY GT_OUT.
  ENDLOOP.

ENDFORM.                    " MAKE_OUT
*&---------------------------------------------------------------------*
*&      Form  get_ar_plan
*&---------------------------------------------------------------------*
FORM GET_AR_PLAN USING    U_POSID
                          U_AYEAR.

  DATA: BEGIN OF I_PLAN_TOT OCCURS 0.
          INCLUDE STRUCTURE BAPIAPPREQPLANTOTALMULTI.
  DATA: END OF I_PLAN_TOT.

  DATA IT_VARIANT LIKE BAPIAPPREQVARNTASSIGNMULTI OCCURS 0
                                                  WITH HEADER LINE.

  CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
       EXPORTING
            EXTERNALNUMBER     = U_POSID
       TABLES
            PLAN_TOTAL         = I_PLAN_TOT
            VARIANT_TO_VERSION = IT_VARIANT.

  READ TABLE IT_VARIANT WITH KEY APPR_YEAR    = U_AYEAR
                                 PLAN_VERSION = P_ARVER.
  IF SY-SUBRC = 0.
    GT_OUT-ARVRNT = IT_VARIANT-APPREQVRNT.
    READ TABLE I_PLAN_TOT WITH KEY APPREQVRNT = IT_VARIANT-APPREQVRNT.

    MOVE I_PLAN_TOT-INVESTMENT_COSTS TO GT_OUT-ARPLAN.
  ENDIF.

ENDFORM.                    " get_ar_plan

* For BDC
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING P_DYNBEGIN P_NAME P_VALUE.
  CLEAR GT_BDC.

  IF P_DYNBEGIN = 'X'.
    GT_BDC-PROGRAM = P_NAME.
    GT_BDC-DYNPRO = P_VALUE.
    GT_BDC-DYNBEGIN = P_DYNBEGIN.
  ELSE.
    GT_BDC-FNAM = P_NAME.
    GT_BDC-FVAL = P_VALUE.
  ENDIF.

  APPEND GT_BDC.

ENDFORM.                    " DYNPRO
*---------------------------------------------------------------------*
*       Form GET_OPT                                                   *
*---------------------------------------------------------------------*
FORM GET_OPT.
  CLEAR GS_OPT.

  GS_OPT-DISMODE  = 'N'.
  GS_OPT-UPDMODE  = 'X'.
  GS_OPT-RACOMMIT = 'X'.
  GS_OPT-NOBINPT  = 'X'.

ENDFORM.                    " GET_OPT
*---------------------------------------------------------------------*
*       Form GET_MSG                                                   *
*---------------------------------------------------------------------*
FORM GET_MSG CHANGING P_MSG.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.

ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  CHANGE_STATUS
*&---------------------------------------------------------------------*
*       Change to status
*----------------------------------------------------------------------*
FORM CHANGE_STATUS USING P_UCOMM TYPE SYUCOMM.
  DATA: L_OKCODE(5),
        L_CHK VALUE 'X',
        L_STAT.

  PERFORM GET_OPT.

  LOOP AT GT_OUT WHERE CHK = 'X'.
    CLEAR L_OKCODE.

    IF P_UCOMM = 'PRE'.
      PERFORM PRE_STEP CHANGING L_OKCODE.
    ELSEIF P_UCOMM = 'NEXT'.
      PERFORM NEXT_STEP CHANGING L_OKCODE.
    ENDIF.

    IF NOT L_OKCODE IS INITIAL.
      PERFORM BDC_IM12 USING L_OKCODE.
    ENDIF.

    READ TABLE GT_MSG WITH KEY MSGTYP = 'S'
                               MSGID = 'AP'
                               MSGNR = '010'.

    IF SY-SUBRC = 0.
      WAIT UP TO 1 SECONDS.
      CLEAR: GT_OUT-STAT, GT_OUT-CRTD, GT_OUT-REL, GT_OUT-LKD.

      PERFORM CHK_STATUS USING    GT_OUT-OBJNR
                         CHANGING GT_OUT-STAT
                                  GT_OUT-CRTD
                                  GT_OUT-REL
                                  GT_OUT-LKD
                                  L_CHK.
      MODIFY GT_OUT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHANGE_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_IT_SUM
*&---------------------------------------------------------------------*
FORM GET_IT_SUM.
  LOOP AT IT_IMFM.
    READ TABLE IT_IMPR WITH KEY POSID = IT_IMFM-POSID.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING IT_IMFM TO IT_SUM.
    IT_SUM-GJAHR = IT_IMPR-GJAHR.
    IT_SUM-OBJNR = IT_IMPR-OBJNR.
    IT_SUM-KOSTL = IT_IMPR-KOSTL.

    COLLECT IT_SUM.
    CLEAR   IT_SUM.
  ENDLOOP.

*Refer ZRFII02 - IM-PI Budget/Actual Report
*FIXME

  SORT IT_SUM ASCENDING BY POSID.

ENDFORM.                    " GET_IT_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_IT_IMAK
*&---------------------------------------------------------------------*
*       AR
*----------------------------------------------------------------------*
*FORM GET_IT_IMAK.
*  CLEAR IT_IMAK.
*  REFRESH IT_IMAK.
*
*  SELECT POSID INTO TABLE IT_IMAK
*    FROM IMAK
*     FOR ALL ENTRIES IN IT_IMPR
*   WHERE POSID = IT_IMPR-POSID
*     AND POSNR  IN S_POSNR
*     AND IVART  IN S_IVART
*     AND VKOKRS IN S_VKOKRS
*     AND VPRCTR IN S_VPRCTR
*     AND GJAHR  IN S_AGJAHR
*     AND USR03  IN S_USR03.
*
*  CLEAR WA_T_CNT1.
*  DESCRIBE TABLE IT_IMAK LINES WA_T_CNT1.
*
*ENDFORM.                    " GET_IT_IMAK
*&---------------------------------------------------------------------*
*&      Form  CHK_STATUS
*&---------------------------------------------------------------------*
*       Check Status
*----------------------------------------------------------------------*
FORM CHK_STATUS USING    P_OBJNR TYPE IM_OBJNR
                CHANGING P_STAT
                         L_CRTD
                         L_REL
                         L_LKD
                         P_CHK.

  TYPES: BEGIN OF TY_STATUS,
           STAT TYPE J_STATUS,
         END OF TY_STATUS.

  DATA: LT_STATUS TYPE TABLE OF TY_STATUS WITH HEADER LINE.

* Check Status
*   : I0001	CRTD	Created
*     I0002	REL	Released
*     I0043	LKD	Locked
  CLEAR LT_STATUS.
  REFRESH LT_STATUS.

  SELECT STAT INTO TABLE LT_STATUS
    FROM JEST
   WHERE OBJNR = P_OBJNR
     AND ( STAT = 'I0001' OR STAT = 'I0002' OR STAT = 'I0043' )
     AND INACT <> 'X'.

  IF SY-SUBRC = 0.
    SORT LT_STATUS BY STAT.

    READ TABLE LT_STATUS WITH KEY STAT = 'I0001' BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_CRTD = 'X'.
      P_STAT = 1.
    ENDIF.

    READ TABLE LT_STATUS WITH KEY STAT = 'I0002' BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_CRTD = 'X'.
      L_REL = 'X'.
      P_STAT = 2.
    ENDIF.

    READ TABLE LT_STATUS WITH KEY STAT = 'I0043' BINARY SEARCH.
    IF SY-SUBRC = 0.
      L_CRTD = 'X'.
      L_LKD = 'X'.
      P_STAT = 3.
    ENDIF.
  ENDIF.

* Created
  IF P_CRTD = 'X'.
    IF L_CRTD = 'X'.
      CLEAR P_CHK.
    ENDIF.
  ENDIF.

* Released
  IF P_REL = 'X'.
    IF L_REL = 'X'.
      CLEAR P_CHK.
    ENDIF.
  ENDIF.

* Locked
  IF P_LKD = 'X'.
    IF L_LKD = 'X'.
      CLEAR P_CHK.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHK_STATUS
*&---------------------------------------------------------------------*
*&      Form  BDC_IM12
*&---------------------------------------------------------------------*
*       Change IM
*----------------------------------------------------------------------*
FORM BDC_IM12 USING P_OKCODE.
  REFRESH: GT_BDC, GT_MSG.
  CLEAR  : GT_BDC, GT_MSG.

  PERFORM DYNPRO USING: 'X'  'SAPLAIP2'          '0500',
                        ' '  'BDC_OKCODE'        '/00',
                        ' '  'IMPR-PRNAM'        GT_OUT-PRNAM,
                        ' '  'IMPR-POSID'        GT_OUT-POSID,
                        ' '  'IMPR-GJAHR'        IT_IMPR-GJAHR,

                        'X'  'SAPLAIP2'          '0600',
                        ' '  'BDC_OKCODE'        P_OKCODE,

                        'X'  'SAPLAIP2'          '0600',
                        ' '  'BDC_OKCODE'        '=UPD'.

  CALL TRANSACTION 'IM12'  USING         GT_BDC
                           OPTIONS FROM  GS_OPT
                           MESSAGES INTO GT_MSG.


ENDFORM.                                                    " BDC_IM12
*&---------------------------------------------------------------------*
*&      Form  PRE_STEP
*&---------------------------------------------------------------------*
*  Change to previous status
*----------------------------------------------------------------------*
FORM PRE_STEP CHANGING P_OKCODE.
  CASE GT_OUT-STAT.
    WHEN 2.
      P_OKCODE = '=BFRZ'.    " Cancel Release
    WHEN 3.
      P_OKCODE = '=BUNL'.    " Unlock
  ENDCASE.

ENDFORM.                    " PRE_STEP
*&---------------------------------------------------------------------*
*&      Form  NEXT_STEP
*&---------------------------------------------------------------------*
*  Change to next status
*----------------------------------------------------------------------*
FORM NEXT_STEP CHANGING P_OKCODE.
  CASE GT_OUT-STAT.
    WHEN 1.
      P_OKCODE = '=BFRE'.    " Release
    WHEN 2.
      P_OKCODE = '=BLOC'.    " Block
  ENDCASE.

ENDFORM.                    " NEXT_STEP
*&---------------------------------------------------------------------*
*&      Form  CREATE_DPCR
*&---------------------------------------------------------------------*
*       Create depreciation data
*----------------------------------------------------------------------*
FORM CREATE_DPCR.
  DATA: LT_VARIANT    TYPE TABLE OF BAPIAPPREQVARNTASSIGNMULTI
                                    WITH HEADER LINE,
        LT_PLAN_TOT   TYPE TABLE OF BAPIAPPREQPLANTOTALMULTI
                                    WITH HEADER LINE,
        LT_VERSION TYPE TABLE OF BAPIAPPREQVARNTASSIGN
                                 WITH HEADER LINE,
        LT_PLAN_YEAR  TYPE TABLE OF BAPIAPPREQPLANYEAR WITH HEADER LINE,
        LT_RETURN     TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
        WA_PLAN_TOTAL LIKE BAPIAPPREQPLANTOTAL,
        L_INDEX       TYPE SYTABIX,
        L_ARVRNT      TYPE IMA_VARNT.

  CLEAR: L_INDEX, L_ARVRNT.

  LOOP AT GT_OUT WHERE CHK = 'X'.
    IF GT_OUT-DPCR_AMT IS INITIAL.
      MESSAGE S000 WITH 'Check depreciation amount!'.
      EXIT.
    ENDIF.

    L_INDEX = SY-TABIX.

*   Check AR Variant
    CLEAR: LT_VARIANT, LT_PLAN_TOT.
    REFRESH: LT_VARIANT, LT_PLAN_TOT.


    CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
         EXPORTING
              EXTERNALNUMBER     = GT_OUT-POSID
              LANGUAGE           = SY-LANGU
         TABLES
              VARIANT_TO_VERSION = LT_VARIANT
              PLAN_TOT           = LT_PLAN_TOT.

    READ TABLE LT_VARIANT WITH KEY APPR_YEAR = P_AYEAR
                                   PLAN_VERSION = P_ARVER.

    IF SY-SUBRC = 0.
      L_ARVRNT = LT_VARIANT-APPREQVRNT.
    ENDIF.

    WA_PLAN_TOTAL-INVESTMENT_COSTS = GT_OUT-DPCR_AMT.

    IF L_ARVRNT IS INITIAL.
*     Create Appropriation Request Variant
      CLEAR: LT_VERSION, LT_PLAN_YEAR, LT_RETURN.
      REFRESH: LT_VERSION, LT_PLAN_YEAR, LT_RETURN.

      LT_VERSION-APPR_YEAR    = P_AYEAR.
      LT_VERSION-PLAN_VERSION = P_ARVER.
      APPEND LT_VERSION.


      CALL FUNCTION 'BAPI_APPREQUEST_ADDVARIANT'
           EXPORTING
                EXTERNALNUMBER     = GT_OUT-POSID
                PLAN_TOTAL         = WA_PLAN_TOTAL
           TABLES
                VARIANT_TO_VERSION = LT_VERSION
                PLAN_YEAR          = LT_PLAN_YEAR
                RETURN             = LT_RETURN.

    ELSE.
*     Change Plan Values of Appropriation Request Variant
      CALL FUNCTION 'BAPI_APPREQUEST_SETPLANVALUES'
           EXPORTING
                EXTERNALNUMBER              = GT_OUT-POSID
                APPROPRIATIONREQUESTVARIANT = L_ARVRNT
                PLAN_TOTAL                  = WA_PLAN_TOTAL
           TABLES
                PLAN_YEAR                   = LT_PLAN_YEAR
                RETURN                      = LT_RETURN.
    ENDIF.

*   Get result
    READ TABLE LT_RETURN WITH KEY TYPE = 'E'.

    IF SY-SUBRC <> 0.
      GT_OUT-ICON = ICON_LED_GREEN.
*     Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                WAIT = 'X'.

    ELSE.
      GT_OUT-ICON = ICON_LED_RED.
*     Rollback
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    MODIFY GT_OUT INDEX L_INDEX TRANSPORTING ICON.
  ENDLOOP.

ENDFORM.                    " CREATE_DPCR
*&---------------------------------------------------------------------*
*&      Form  CAL_DPCR_AMT
*&---------------------------------------------------------------------*
*       Calculate Depreciation amount
*----------------------------------------------------------------------*
FORM CAL_DPCR_AMT USING P_UCOMM TYPE SYUCOMM.
  DATA: L_INDEX TYPE SYTABIX,
        L_AMT   TYPE BP_WPT.

  CLEAR L_INDEX.

  LOOP AT GT_OUT WHERE CHK = 'X'.
    L_INDEX = SY-TABIX.
    CLEAR L_AMT.

    CASE P_UCOMM.
      WHEN 'CAL1'.
        L_AMT = GT_OUT-IO_AMT.
      WHEN 'CAL2'.
        L_AMT = GT_OUT-CUR_AMT.
      WHEN 'CAL3'.
        L_AMT = GT_OUT-PLAN.
    ENDCASE.

    GT_OUT-DPCR_AMT =  L_AMT - GT_OUT-ACT_AMT - GT_OUT-DOW_AMT.

    IF GT_OUT-DPCR_AMT < 0.
      GT_OUT-DPCR_AMT = 0.
    ENDIF.

    MODIFY GT_OUT  INDEX L_INDEX TRANSPORTING DPCR_AMT.
  ENDLOOP.

ENDFORM.                    " CAL_DPCR_AMT
