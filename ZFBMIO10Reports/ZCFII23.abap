*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 19/03/2004
*& Specification By       : hs.jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K902603
*& Addl documentation     :
*& Description            : Management Order status
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT ZCFII02 MESSAGE-ID  ZMFI.
TYPE-POOLS: SLIS, VRM.
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
CLASS CL_GUI_RESOURCES DEFINITION LOAD.


CONSTANTS:
  C_F2CODE               LIKE SY-UCOMM                    VALUE '&ETA'.


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
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE1 TYPE SLIS_LISTHEADER.


*DATA: gt_list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
*
TABLES: AUFK, IMPR, FMFCTR, RIPASW, CODIA, IMAK,
        ANLI, ANLC.

DATA: IT_OUT  TYPE TABLE OF AUFK WITH HEADER LINE.
DATA: IT_COAS TYPE TABLE OF COAS WITH HEADER LINE.
*DATA: gt_out  TYPE TABLE OF aufk WITH HEADER LINE.
DATA: IT_IMZO TYPE TABLE OF IMZO WITH HEADER LINE.
DATA: IT_IMPR TYPE TABLE OF IMPR WITH HEADER LINE.

DATA: BEGIN OF GT_OUT OCCURS 0.
        INCLUDE STRUCTURE AUFK.
DATA :
*Requested by yc yoon,changed by wskim,on 20041105
*user request : add asset,class,capitalization date
*-----Start
        ANLN1    LIKE ANLI-ANLN1,     " Asset
        ANLKL    LIKE ANIA-ANLKL,     " Asset class
        AKTIV    LIKE ANIA-AKTIV,     " capitalization date
        ANSWL    LIKE ANLC-ANSWL,     " asset value
*-----End
        ACT(16)  TYPE P DECIMALS 2,
        AFABE    TYPE AFABE_D,
        DPCR,                         " Depreciation
        CHKBOX   TYPE C,
        LIGHT    TYPE C,
        TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV,
      END OF GT_OUT.

DATA : IT_SUCC LIKE GT_OUT OCCURS 0 WITH HEADER LINE.
*---WORK AREA
DATA : WA_T_CNT TYPE I,
       WA_CNT   TYPE I,
       WA_BEFORE  LIKE  IMZO-GJAHR,
       WA_LAST    LIKE  IMZO-GJAHR,
       WA_YEAR    LIKE  IMZO-GJAHR,
       WA_YEAR1   LIKE  IMZO-GJAHR,
       WA_YEAR2   LIKE  IMZO-GJAHR,
       WA_YEAR3   LIKE  IMZO-GJAHR,
       WA_YEAR4   LIKE  IMZO-GJAHR,
       WA_YEAR5   LIKE  IMZO-GJAHR,
       WA_YEAR6   LIKE  IMZO-GJAHR,
       WA_AFTER   LIKE  IMZO-GJAHR,
       WA_BEFORE_TXT(10),
       WA_AFTER_TXT(10).

*---WORK

DATA : WA_OKCODE(4),
       OK_CODE(4),
       WA_CURSOR(14),
       WA_LINE TYPE I,
       WA_ACT  LIKE COSP-WTG001,
       WA_PICK_LINE TYPE I,
       WA_CHK.
*----
DATA : WA_PHAS0 LIKE AUFK-PHAS0,
       WA_PHAS1 LIKE AUFK-PHAS1,
       WA_PHAS2 LIKE AUFK-PHAS2,
       WA_PHAS3 LIKE AUFK-PHAS3.
*====FOR BDC
DATA : IT_BDC      LIKE BDCDATA OCCURS 0 WITH HEADER LINE.
DATA : IT_ACT      LIKE ZFI_IO_ACTUAL OCCURS 0 WITH HEADER LINE.
DATA:  IT_MESSTAB  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA : TCODE LIKE TSTC-TCODE.
DATA : WA_PAR(5).
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B00 WITH FRAME TITLE TEXT-010.
PARAMETERS : P_KOKRS LIKE AUFK-KOKRS OBLIGATORY MEMORY ID CAC.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) C_OR.
PARAMETERS : R_1  RADIOBUTTON GROUP R1.
SELECTION-SCREEN POSITION 25.
SELECTION-SCREEN COMMENT (15) C_PI.
PARAMETERS : R_2  RADIOBUTTON GROUP R1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B00.
PARAMETERS : P_DEPR AS CHECKBOX.

*---------------------------------------------
***********************
SELECTION-SCREEN BEGIN OF BLOCK S4 WITH FRAME TITLE TEXT-030.
PARAMETERS : P_CHK0 AS CHECKBOX DEFAULT 'X',
             P_CHK1 AS CHECKBOX DEFAULT 'X',
             P_CHK2 AS CHECKBOX,
             P_CHK3 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK S4.

PARAMETERS : P_PRNAM LIKE IMPR-PRNAM.
SELECT-OPTIONS:
  S_POSID   FOR   IMPR-POSID.

SELECT-OPTIONS:
  S_AUFNR FOR   AUFK-AUFNR,
  S_AUART FOR  AUFK-AUART,
  S_USR02 FOR   IMAK-USR02,
  S_AKSTL FOR  AUFK-AKSTL.

PARAMETERS P_MODE LIKE CTU_PARAMS-UPDMODE DEFAULT 'E' NO-DISPLAY.

PARAMETERS CUPDATE LIKE CTU_PARAMS-UPDMODE DEFAULT 'S' NO-DISPLAY.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETERS :
  P_LAYOUT LIKE DISVARIANT-VARIANT.   "LAYOUT
SELECTION-SCREEN END OF BLOCK B2.
*----------------------------------------------------------------------
*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*
*----------------------------------------------------------------------
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LAYOUT.
  PERFORM F4_VARIANT CHANGING P_LAYOUT.
*----------------------------------------------------------------------
*
* INITIALIZATION
*
*----------------------------------------------------------------------
*
INITIALIZATION.
* ==> Change Variant saving type
  WA_VAR_SAVE = 'A'.
* ==> Change first mode   GRID or LIST
  WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
* wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : GT_FIELDCAT.
  CLEAR   : GS_LAYOUT.
  WA_REPID = SY-REPID.
  C_PI = 'Position ID'.
  C_OR = 'Internal Order'.
*  c010 = 'Internal Order Option'.
*  c020 = 'Position ID Option'.
*  c030 = 'Status'.

*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
END-OF-SELECTION.
* ==> 5. build field category
  PERFORM BUILD_FIELD_CATEGORY
  USING :
   'AUFNR'     'Order'       '12' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'AUART'     'Ord Type'    '8' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'KTEXT'     'Description' '18' 'X' 'L'  ' '  ' '  '  ' '  ' ,
*Requested by yc yoon,changed by wskim,on 20041105
*user request : add asset,class,capitalization date
*-----Start
   'ANLN1'     'Asset'        '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ANLKL'     'Class'        '8'  ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'AKTIV'     'Cap.date'     '8'  ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ANSWL'     'Balance'      '16' ' ' 'R'  ' '  ' '  '  ' '  ' ,
*-----End
   'KOSTV'     'Resp.Cctr'   '9' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'AKSTL'     'Req.Cctr'    '8' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'USER0'     'Applicant'   '15' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'USER4'     'Cost Estimated' '17' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'ACT'       'Actual Cost ' '17' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'PHAS0'     'Created'      '7' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PHAS1'     'Released'     '8' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PHAS2'     'Tech Com'     '8' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PHAS3'     'Close'        '5' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'DPRC'      'Depreciation' '1' ' ' 'C'  ' '  ' '  '  ' '  ',
   'KOKRS'     'Con Ar'       '4' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'WERKS'     'Plant'        '4' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'STORT'     'Loca'         '4' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'KAPPL'     'Appli'        '4' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'FUNC_AREA'  'Func Area'   '4' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'KDAUF'  'Sales Ord'       '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'AUFEX'  'Ex ord'          '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'SIZECL'  'Scale'          '2' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'IZWEK'  'Reason'          '2' ' ' 'L'  ' '  ' '  '  ' '  ' .

* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'AUFNR'   'X'   'X'   '*'.
* ==> 1. select data from db
  PERFORM SELECT_DATA.

  IF GT_OUT[] IS INITIAL.
    MESSAGE S000(ZMFI) WITH 'No found data '.
    EXIT.
  ENDIF.
* ==> 2. set variant default
  PERFORM SET_VARIANT CHANGING WA_VAR.
* ==> 3. set layout for alv style
  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.
* ==> 4. set events for alv
  PERFORM SET_EVENTS CHANGING GT_EVENTS.
*===> 5. set event for top-of-page grid.
  PERFORM SET_BUILD_EVENT.
*===>
  PERFORM COMMENT_BUILD USING  W_TOP_OF_PAGE[].

* ==> 7. call function display alv.

  CALL FUNCTION WA_ALV_FUNCTION_NAME
    EXPORTING
         I_CALLBACK_PROGRAM      = WA_REPID
         I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
         I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
         IS_LAYOUT               = GS_LAYOUT
         IT_FIELDCAT             = GT_FIELDCAT[]
         IT_SPECIAL_GROUPS       = GT_SP_GROUP[]
         IT_SORT                 = GT_SORTS[]
*         IT_FILTER               =
         I_DEFAULT               = WA_DEFAULT
         I_SAVE                  = WA_VAR_SAVE
         IS_VARIANT              = WA_VAR
*         it_events               = gt_events[]
         IT_EVENTS               =  W_EVENTCAT[]
         IS_PRINT                = GS_PRNT
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         T_OUTTAB                = GT_OUT.
***********************************************************************
*

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM SELECT_DATA.

  IF R_1 = 'X'.
    PERFORM GET_IT_ORDER.
  ELSE.
    PERFORM GET_PI_ORDER.
  ENDIF.

  PERFORM PROCESS_STATUS.

  PERFORM GET_ASSET_INFO.

  SORT GT_OUT BY AUFNR.
ENDFORM.                    " select_data

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM ALV_EVENT_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  IF WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' EXCLUDING RT_EXTAB.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.
  ENDIF.
  SET TITLEBAR  'STANDARD'.

ENDFORM.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM ALV_EVENT_USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                                      RS_SELFIELD TYPE SLIS_SELFIELD.
                                                            "#EC *


  CASE R_UCOMM.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE GT_OUT INDEX RS_SELFIELD-TABINDEX.
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'AUFNR'.
          SET PARAMETER ID 'ANR' FIELD GT_OUT-AUFNR.
          CALL TRANSACTION 'KO02' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
          SET PARAMETER ID 'ANR' FIELD GT_OUT-AUFNR.
          CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
      ENDCASE.
*
    WHEN '&PRE'.
      WA_PAR = '=VAR2'.

      LOOP AT GT_OUT WHERE CHKBOX = 'X'.
        PERFORM PREVIOUS_PROCESS USING GT_OUT-AUFNR WA_PAR.
      ENDLOOP.
*--refresh.
      PERFORM REFRESH_PROCESS.
      RS_SELFIELD-REFRESH = 'X'.

    WHEN '&NEXT'.
      WA_PAR = '=VARI'.
      REFRESH : IT_SUCC.
      CLEAR   : IT_SUCC.

      LOOP AT GT_OUT WHERE CHKBOX = 'X'.
        PERFORM PREVIOUS_PROCESS USING GT_OUT-AUFNR WA_PAR.
      ENDLOOP.

*--refresh.
      PERFORM REFRESH_PROCESS.
      RS_SELFIELD-REFRESH = 'X'.

    WHEN '&REF'.
*---------------------------------- switching view type grid or list

    WHEN 'LIST' OR 'GRID'.
      PERFORM SWITCH_LIST_OR_GRID USING R_UCOMM.

    WHEN 'DPCR' OR 'CSPCR'.
      DATA L_RESET.

      CLEAR L_RESET.
      IF R_UCOMM = 'DPCR'.
        CLEAR L_RESET.
      ELSEIF R_UCOMM = 'CDPCR'.
        L_RESET = 'X'.
      ENDIF.

      PERFORM SUBMIT_ZCFII12 USING L_RESET.

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

*... Display options
  CS_LAYO-COLWIDTH_OPTIMIZE      = SPACE.
  "?????
  CS_LAYO-NO_COLHEAD             = SPACE.
  CS_LAYO-NO_HOTSPOT             = SPACE.
  CS_LAYO-ZEBRA                  = ' '.
  CS_LAYO-NO_VLINE               = SPACE.
  CS_LAYO-CELL_MERGE             = SPACE.
  CS_LAYO-NO_MIN_LINESIZE        = SPACE.
  CS_LAYO-MIN_LINESIZE           = SPACE.
  CS_LAYO-MAX_LINESIZE           = SPACE.
  CS_LAYO-WINDOW_TITLEBAR        = SPACE.
  CS_LAYO-NO_ULINE_HS            = SPACE.
*... Edit
  CS_LAYO-EDIT                   = ' '."space.
  CS_LAYO-EDIT_MODE              = ' '."space.
*... Exceptions
  CS_LAYO-LIGHTS_FIELDNAME       = ' '.
  "=> ??? ??? ???
  CS_LAYO-LIGHTS_TABNAME         = SPACE.
  CS_LAYO-LIGHTS_ROLLNAME        = SPACE.
  CS_LAYO-LIGHTS_CONDENSE        = SPACE.
*... Sums
  CS_LAYO-NO_SUMCHOICE           = SPACE.
  CS_LAYO-NO_TOTALLINE           = SPACE.
  CS_LAYO-TOTALS_BEFORE_ITEMS    = SPACE.
  CS_LAYO-TOTALS_ONLY            = SPACE.
  CS_LAYO-TOTALS_TEXT            = SPACE.
  CS_LAYO-NO_SUBCHOICE           = SPACE.
  CS_LAYO-NO_SUBTOTALS           = SPACE.
  CS_LAYO-SUBTOTALS_TEXT         = SPACE.
  CS_LAYO-NUMC_SUM               = 'X'.
  CS_LAYO-NO_UNIT_SPLITTING      = SPACE.
*... Interaction
  CS_LAYO-BOX_FIELDNAME          = 'CHKBOX'.
  CS_LAYO-BOX_TABNAME            = SPACE.
  CS_LAYO-BOX_ROLLNAME           = SPACE.
  CS_LAYO-EXPAND_FIELDNAME       = SPACE.
  CS_LAYO-HOTSPOT_FIELDNAME      = SPACE.
  CS_LAYO-NO_INPUT               = ' '.
  CS_LAYO-F2CODE                 = SPACE.
  CS_LAYO-CONFIRMATION_PROMPT    = SPACE.
  CS_LAYO-KEY_HOTSPOT            = SPACE.
  CS_LAYO-FLEXIBLE_KEY           = SPACE.
  CS_LAYO-REPREP                 = SPACE.
  CS_LAYO-GROUP_BUTTONS          = 'X'.
  CS_LAYO-NO_KEYFIX              = SPACE.
  CS_LAYO-GET_SELINFOS           = SPACE.
  CS_LAYO-GROUP_CHANGE_EDIT      = 'X'.
  CS_LAYO-NO_SCROLLING           = SPACE.
  CS_LAYO-EXPAND_ALL             = SPACE.
  CS_LAYO-NO_AUTHOR              = SPACE.
*... Detailed screen
  CS_LAYO-DETAIL_POPUP           = 'X'.
  CS_LAYO-DETAIL_INITIAL_LINES   = SPACE.
  CS_LAYO-DETAIL_TITLEBAR        = SPACE.
*... PF-status
  CS_LAYO-DEF_STATUS             = SPACE.
*... Display variants
  CS_LAYO-HEADER_TEXT            = SPACE.
  CS_LAYO-ITEM_TEXT              = SPACE.
  CS_LAYO-DEFAULT_ITEM           = SPACE.
*... colour
  CS_LAYO-INFO_FIELDNAME         = SPACE.
  CS_LAYO-COLTAB_FIELDNAME       = 'TABCOLOR'.
*... others
  CS_LAYO-LIST_APPEND            = SPACE.

ENDFORM.                    " set_layout


*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_PAGE.                                 "#EC CALLED
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_LIST.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_PAGE.
*  NEW-LINE.
*  ULINE.
*  DATA: l_page(10).
*  WRITE : sy-pagno TO l_page.
*  WRITE: /(120) l_page CENTERED.
*
ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_LIST.


ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM DISPLAY_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
         EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
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
*                it_events               = gt_events[]
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
FORM BUILD_FIELD_CATEGORY USING
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

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
  IF P_FIELDNAME = 'TOT'.
    LS_FIELDCAT-EMPHASIZE = 'C700'.
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
            IS_VARIANT          = LS_VARIANT
            I_SAVE              = 'A'
*           it_default_fieldcat =
       IMPORTING
            E_EXIT              = L_EXIT
            ES_VARIANT          = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND = 2.
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
  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.

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
*    "cl_gui_resources=>list_col_positive.
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
*       text
*----------------------------------------------------------------------*
*      -->P_0634   text
*      -->P_0635   text
*      -->P_0636   text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*      -->P_0640   text
*      -->P_0641   text
*      -->P_0642   text
*----------------------------------------------------------------------*
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

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
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
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.
ENDFORM.                    " build_field_category1
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
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
*  CLEAR ls_line.
*  ls_line-typ  = 'H'.
*  ls_line-info = text-h01.     "HEADER TITLE (H001)
*  APPEND ls_line TO lt_top_of_page.
*
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Investment program : '.
*  ls_line-info = p_prnam.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Approval Year : '.
**  ls_line-info = p_ayear.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Order no : '.
*  CONCATENATE   s_aufnr-low  ' ~'  s_aufnr-high INTO l_list.
*  ls_line-info = l_list.
*  APPEND ls_line TO lt_top_of_page.
*
*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_BUILD_EVENT.
  W_EVENTCAT-NAME = 'TOP_OF_PAGE'.
  W_EVENTCAT-FORM = 'DISPLAY_HEADER'.
  APPEND W_EVENTCAT.
ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  PREVIOUS_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
FORM PREVIOUS_PROCESS USING    U_AUFNR U_PAR.
  REFRESH : IT_BDC, IT_MESSTAB.
  CLEAR   : IT_BDC.
  TCODE = 'KO02'.
  PERFORM MAKE_BDC_RTN USING :
                      'X'  'SAPMKAUF'        '0110',
                      ' '  'COAS-AUFNR'      U_AUFNR,
                     ' '  'BDC_OKCODE'      '/00'.

  PERFORM MAKE_BDC_RTN USING :
                      'X'  'SAPMKAUF'        '0600',
                     ' '  'BDC_OKCODE'      '=BUT2'.
  PERFORM MAKE_BDC_RTN USING :
                      'X'  'SAPMKAUF'        '0600',
                     ' '  'BDC_OKCODE'      U_PAR.          "'=VAR2'.
  PERFORM MAKE_BDC_RTN USING :
                      'X'  'SAPMKAUF'        '0600',
                     ' '  'BDC_OKCODE'      '=SICH'.

  CALL TRANSACTION TCODE USING IT_BDC
                   MODE   P_MODE        "error/all/no disp
                   UPDATE CUPDATE    "sync/async/local
                   MESSAGES INTO IT_MESSTAB.

  READ TABLE IT_MESSTAB WITH KEY MSGTYP = 'S'
                                 MSGNR = '109'.
  IF SY-SUBRC = 0.
    MOVE-CORRESPONDING GT_OUT TO IT_SUCC.
    APPEND IT_SUCC.
    CLEAR  IT_SUCC.
  ENDIF.

ENDFORM.                    " PREVIOUS_PROCESS
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1652   text
*      -->P_1653   text
*      -->P_1654   text
*----------------------------------------------------------------------*
FORM MAKE_BDC_RTN USING   DYNBEGIN PROGRAM DYNPRO.
  CLEAR IT_BDC.

  IF DYNBEGIN = 'X'.
    IT_BDC-PROGRAM  = PROGRAM.
    IT_BDC-DYNPRO   = DYNPRO.
    IT_BDC-DYNBEGIN = 'X'.
  ELSE.
    IT_BDC-FNAM     = PROGRAM.
    IT_BDC-FVAL     = DYNPRO.
  ENDIF.

  APPEND IT_BDC.
ENDFORM.                    " make_bdc_rtn
*&---------------------------------------------------------------------*
*&      Form  get_update_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
FORM GET_UPDATE_DATA USING    U_AUFNR.

  CLEAR : WA_PHAS0, WA_PHAS1, WA_PHAS2, WA_PHAS3.

  SELECT SINGLE PHAS0 PHAS1 PHAS2 PHAS3 INTO
        (WA_PHAS0,  WA_PHAS1, WA_PHAS2, WA_PHAS3)
  FROM AUFK
  WHERE AUFNR = U_AUFNR.
ENDFORM.                    " get_update_data
*&---------------------------------------------------------------------*
*&      Form  get_it_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_IT_ORDER.
  REFRESH : IT_OUT, GT_OUT, IT_COAS.
  CLEAR   : IT_OUT, GT_OUT, IT_COAS.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OUT
    FROM AUFK AS A
    INNER JOIN T003O AS B
       ON B~AUART = A~AUART
    WHERE  B~AUTYP = '01'
      AND  B~SCOPE = 'IV'     "Investment
      AND  A~AUART IN S_AUART
      AND  A~AUFNR IN S_AUFNR
      AND  A~AKSTL IN S_AKSTL
      AND  A~USER0  IN S_USR02.

ENDFORM.                    " get_it_order
*&---------------------------------------------------------------------*
*&      Form  get_pi_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PI_ORDER.
  REFRESH : IT_IMPR, IT_IMZO.
  CLEAR   : IT_IMPR, IT_IMZO.
  IF P_PRNAM <> ' '.
    SELECT * INTO TABLE IT_IMPR FROM IMPR
    WHERE POSID IN S_POSID
    AND   PRNAM =  P_PRNAM
    AND   KOSTL IN S_AKSTL.
  ELSE.
    SELECT * INTO TABLE IT_IMPR FROM IMPR
    WHERE POSID IN S_POSID
    AND   KOSTL IN S_AKSTL.
  ENDIF.

  CLEAR WA_CNT.
  DESCRIBE TABLE IT_IMPR LINES WA_CNT.
  IF WA_CNT > 0.
    SELECT * INTO TABLE IT_IMZO FROM IMZO
    FOR ALL ENTRIES IN IT_IMPR
    WHERE POSNR = IT_IMPR-POSNR.
  ENDIF.
  CLEAR WA_CNT.
  DESCRIBE TABLE IT_IMZO LINES WA_CNT.
  IF WA_CNT > 0.
    SELECT * INTO TABLE IT_OUT  FROM AUFK
    FOR ALL ENTRIES IN IT_IMZO
    WHERE OBJNR = IT_IMZO-OBJNR.
  ENDIF.

ENDFORM.                    " get_pi_order
*&---------------------------------------------------------------------*
*&      Form  refresh_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_PROCESS.
  REFRESH : GT_OUT.
  LOOP AT GT_OUT.
    READ TABLE IT_SUCC WITH KEY AUFNR = GT_OUT-AUFNR.
    IF SY-SUBRC = 0.
      DELETE GT_OUT.
    ENDIF.
  ENDLOOP.
  IF R_1 = 'X'.
    PERFORM GET_IT_ORDER.
    LOOP AT IT_OUT.
      MOVE-CORRESPONDING IT_OUT TO GT_OUT.
      APPEND GT_OUT.
      CLEAR GT_OUT.
    ENDLOOP.
  ENDIF.
  IF R_2 = 'X'.
    PERFORM GET_PI_ORDER.
    LOOP AT IT_OUT.
      MOVE-CORRESPONDING IT_OUT TO GT_OUT.
      APPEND GT_OUT.
      CLEAR GT_OUT.
    ENDLOOP.
  ENDIF.
  SORT GT_OUT BY AUFNR.
ENDFORM.                    " refresh_process
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_OUT_AUFNR  text
*----------------------------------------------------------------------*
FORM GET_IO_ACTUAL USING    U_AUFNR
                   CHANGING L_AMT.
  REFRESH : IT_ACT.
  CLEAR   : IT_ACT.
  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      AUFNR         = U_AUFNR
* IMPORTING
*   AMT           = wa_act
    TABLES
      OUT           = IT_ACT
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR : L_AMT.
  LOOP AT IT_ACT WHERE WRTTP = '04' OR WRTTP = '11'.
    L_AMT = L_AMT + IT_ACT-TOT.
  ENDLOOP.
ENDFORM.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_ZCFII12
*&---------------------------------------------------------------------*
*       Execute Create Depreciation Simulation Data in AuC Asset
*----------------------------------------------------------------------*
FORM SUBMIT_ZCFII12 USING P_RESET.
  RANGES R_ANLN1 FOR ANLA-ANLN1.

  CLEAR R_ANLN1.
  REFRESH R_ANLN1.

  R_ANLN1-SIGN = 'I'.
  R_ANLN1-OPTION = 'EQ'.

  LOOP AT GT_OUT WHERE CHKBOX = 'X'.
    R_ANLN1 = 'IEQ'.
    R_ANLN1-LOW = GT_OUT-ANLN1.
    APPEND R_ANLN1.
    CLEAR R_ANLN1.
  ENDLOOP.

  SUBMIT ZCFII12 WITH P_AFABE = GT_OUT-AFABE
                 WITH P_RESET = P_RESET
                 WITH P_MODE = 'N'
                 WITH P_TEST = SPACE
                 WITH S_ANLN1 IN R_ANLN1     AND RETURN .


ENDFORM.                    " SUBMIT_ZCFII12
*&---------------------------------------------------------------------*
*&      Form  process_status
*&---------------------------------------------------------------------*
FORM PROCESS_STATUS.
  DATA : WA_CHK.

  LOOP AT IT_OUT.
    CLEAR WA_CHK.
    IF P_CHK0 = 'X'.
      IF IT_OUT-PHAS0 = 'X'.
        WA_CHK = 'Q'.
      ENDIF.
    ENDIF.
    IF P_CHK1 = 'X'.
      IF IT_OUT-PHAS1 = 'X'.
        WA_CHK = 'Q'.
      ENDIF.
    ENDIF.
    IF P_CHK2 = 'X'.
      IF IT_OUT-PHAS2 = 'X'.
        WA_CHK = 'Q'.
      ENDIF.
    ENDIF.
    IF P_CHK3 = 'X'.
      IF IT_OUT-PHAS3 = 'X'.
        WA_CHK = 'Q'.
      ENDIF.
    ENDIF.

    IF WA_CHK <> 'Q'.
      DELETE IT_OUT INDEX SY-TABIX.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " process_status
*&---------------------------------------------------------------------*
*&      Form  get_asset_info
*&---------------------------------------------------------------------*
FORM GET_ASSET_INFO.
  DATA: L_NDJAR TYPE NDJAR.

*====*
  CLEAR GT_OUT.
  LOOP AT IT_OUT.
    MOVE-CORRESPONDING IT_OUT TO GT_OUT.

*---get io actual.
    PERFORM GET_IO_ACTUAL USING    GT_OUT-AUFNR
                          CHANGING GT_OUT-ACT.

*-----Start
    DATA : L_OBJNR LIKE ANLI-OBJNR.
    CLEAR L_OBJNR.
*Asset
    L_OBJNR = IT_OUT-OBJNR.

*class and capitalization date
    SELECT SINGLE ANLN1 ANLKL AKTIV
    INTO (GT_OUT-ANLN1, GT_OUT-ANLKL, GT_OUT-AKTIV)
     FROM ANLI AS A
     INNER JOIN ANIA AS B
        ON A~OBJNR = B~OBJNR
      WHERE A~OBJNR EQ L_OBJNR.

*get asset value
    SELECT SUM( ANSWL ) INTO GT_OUT-ANSWL
       FROM ANLC
       WHERE BUKRS = IT_OUT-BUKRS
         AND ANLN1 = GT_OUT-ANLN1
         AND AFABE = '01'.
    IF P_DEPR = 'X' AND GT_OUT-ANSWL = 0.
      CONTINUE.
    ENDIF.

* Depreciation
    CLEAR L_NDJAR.
    SELECT SINGLE AFABE NDJAR
      INTO (GT_OUT-AFABE, L_NDJAR)
      FROM ANLB
     WHERE BUKRS = IT_OUT-BUKRS
       AND ANLN1 = GT_OUT-ANLN1
       AND AFABE = '20'.

    IF NOT L_NDJAR IS INITIAL.
      GT_OUT-DPCR = 'X'.
    ENDIF.

*-----End
    APPEND GT_OUT.
    CLEAR  GT_OUT.
  ENDLOOP.

ENDFORM.                    " get_asset_info
