*&---------------------------------------------------------------------*
*& Report  ZMMR_VAATZ_IF001_LOG
*
*&                                                                     *
*&---------------------------------------------------------------------*
*& program copy from ZMMR_IF001
*
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZMMR_VAATZ_IF001_LOG   MESSAGE-ID ZMM_IF
                     NO STANDARD PAGE HEADING.

TABLES : ZTMM_VAZ_IF001,
*         ztmm_if006,
         ZSMM_VAZ_IF001,
         EBAN,
         EBKN.

*-------------------------------------------------------------*
*  Globale Type(ALV)
*-------------------------------------------------------------*
TYPE-POOLS: SLIS.

************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Select data checking field
DATA COUNT   TYPE I.

*-------------------------------------------------------------*
*  RANGES
*-------------------------------------------------------------*
RANGES : R_TYPE FOR ZTMM_VAZ_IF001-TYPE,
         R_FLAG FOR ZTMM_VAZ_IF001-FLAG.

DATA: BEGIN OF IT_TAB OCCURS 0,
        ICON(4).
DATA: CHK.
        INCLUDE STRUCTURE ZTMM_VAZ_IF001.
DATA : END OF IT_TAB.

DATA IT_HEAD LIKE TABLE OF ZTMM_VAZ_IF001 WITH HEADER LINE.
DATA IT_ITEM LIKE TABLE OF ZTMM_IF006 WITH HEADER LINE.

DATA : IT_REP LIKE IT_TAB OCCURS 0 WITH HEADER LINE.
DATA : W_REP_INDEX TYPE I,
       W_REP_LINES TYPE I.


*-------------------------------------------------------------*
*  ALV : Function
*-------------------------------------------------------------*
* General Work fields
DATA : G_EXIT_CAUSED_BY_CALLER  TYPE C,
       G_REPID                  TYPE SY-REPID,
       G_SAVE                   TYPE C,
       G_PROGRAM_NAME           LIKE SY-REPID,
       G_INCLNAME               LIKE TRDIR-NAME.

* Structures
DATA : G_LAYOUT_S               TYPE SLIS_LAYOUT_ALV,
       GS_FIELDCAT              TYPE SLIS_FIELDCAT_ALV,
       G_EXIT_CAUSED_BY_USER_S  TYPE SLIS_EXIT_BY_USER,
       G_VARIANT_S              TYPE DISVARIANT.

* Internal tables
DATA : G_EVENTS_T               TYPE SLIS_T_EVENT,
       G_LIST_TOP_OF_PAGE_T     TYPE SLIS_T_LISTHEADER,
       GT_FIELDCAT              TYPE SLIS_T_FIELDCAT_ALV,
       GT_SORT                  TYPE SLIS_T_SORTINFO_ALV.

DATA : ALV_PRINT                TYPE SLIS_PRINT_ALV.
DATA : ALV_REPID        LIKE SY-REPID,
       ALV_VARIANT      LIKE DISVARIANT.
DATA: ALV_DETAIL_FUNC(30).

*--------------------------------------------------------------*
*  CONSTANTS:
*--------------------------------------------------------------*
CONSTANTS : C_STATUS_SET   TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
            C_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
            C_TOP_OF_PAGE  TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.


*--------------------------------------------------------------*
*  CONTROL
*--------------------------------------------------------------*
DATA : G_CUSTOM_CONTAINER                    ">Display Screen
       TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       ALV_GRID
       TYPE REF TO CL_GUI_ALV_GRID.

DATA: GS_LAYOUT TYPE LVC_S_LAYO,
      GS_FDCAT  TYPE LVC_S_FCAT,
      GT_FDCAT  TYPE LVC_T_FCAT.

DATA IT_OUT LIKE TABLE OF ZTMM_IF006 WITH HEADER LINE.

DATA : OK_CODE LIKE SY-UCOMM.


************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  S_BANFN  FOR ZTMM_VAZ_IF001-BANFN       MODIF ID IOG,
  "P/R Number.
  S_SERNO  FOR ZTMM_VAZ_IF001-SERNO       NO-EXTENSION NO INTERVALS,
  S_MATNR  FOR ZTMM_VAZ_IF001-MATNR,
  S_DATE   FOR SY-DATUM.
"Date on which the record was created.
*  S_TIME   FOR SY-UZEIT               NO-EXTENSION.      "Entry time.
SELECTION-SCREEN END OF BLOCK BOX1.

*SELECTION-SCREEN ULINE.

*Division
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_CREATE    RADIOBUTTON GROUP AB1,
*            P_CHANGE    RADIOBUTTON GROUP AB1,
            P_OUTBND    RADIOBUTTON GROUP AB1,
            P_INDEL     RADIOBUTTON GROUP AB1,
            P_ALL       RADIOBUTTON GROUP AB1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BOX2.

*SELECTION-SCREEN ULINE.

*Result
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_SUCESS    RADIOBUTTON GROUP AB2,
            P_ERROR     RADIOBUTTON GROUP AB2,
            P_REP       RADIOBUTTON GROUP AB2,     " re-processing
            P_SCHALL    RADIOBUTTON GROUP AB2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BOX3.

************************************************************************
* INITIALIZATION Event                                                 *
************************************************************************
INITIALIZATION.
* g_repid = sy-repid.
  PERFORM LAYOUT_INIT USING G_LAYOUT_S.
  PERFORM EVENTTAB_BUILD USING G_EVENTS_T[].
  G_REPID                = SY-REPID.


************************************************************************
* START-OF-SELECTION Event                                             *
************************************************************************
START-OF-SELECTION.
  PERFORM RANGE_CONVERSION.

  PERFORM GET_LOG_DATA.

  IF SY-SUBRC NE 0.
    MESSAGE S001.
    EXIT.
  ENDIF.

END-OF-SELECTION.
  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_SORTCAT.
  PERFORM ALV_DISPLAY.






*&---------------------------------------------------------------------*
*&      Form  RANGE_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RANGE_CONVERSION .
*--FLAG & TYPE RANGE
  CLEAR : R_FLAG, R_TYPE.
  REFRESH : R_FLAG, R_TYPE.

  IF     P_CREATE  = 'X'.
    R_FLAG-SIGN    = 'I'.
    R_FLAG-OPTION  = 'EQ'.
    R_FLAG-LOW     = '1'.
    APPEND R_FLAG. CLEAR R_FLAG.

*  ELSEIF P_CHANGE = 'X'.
*    R_FLAG-SIGN   = 'I'.
*    R_FLAG-OPTION = 'EQ'.
*    R_FLAG-LOW    = '2'.
*    APPEND R_FLAG. CLEAR R_FLAG.
*
  ELSEIF P_INDEL  = 'X'.
    R_FLAG-SIGN   = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_FLAG-LOW    = '4'.
    APPEND R_FLAG. CLEAR R_FLAG.

  ELSEIF P_OUTBND = 'X'.
    R_FLAG-SIGN   = 'I'.
    R_FLAG-OPTION = 'EQ'.
    R_FLAG-LOW    = '3'.
    APPEND R_FLAG. CLEAR R_FLAG.
  ENDIF.

ENDFORM.                    " RANGE_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY .
*> ALV reuse function call..
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = G_REPID
*            i_background_id         = 'ALV_BACKGROUND'
            I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
            IT_EVENTS               = G_EVENTS_T[]
            IT_FIELDCAT             = GT_FIELDCAT[]
            IT_SORT                 = GT_SORT[]
            I_SAVE                  = 'X'
            IS_LAYOUT               = G_LAYOUT_S
       TABLES
            T_OUTTAB                = IT_TAB[].
ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT .
  PERFORM FILL_FIELD_CATEGORY USING :
            'S' 'FIELDNAME'        'ICON',
            'E' 'REPTEXT_DDIC'     'Status',

            'S' 'FIELDNAME'        'SERNO',
            'E' 'REPTEXT_DDIC'     'Serial No',

            'S' 'FIELDNAME'        'FLAG',
            'E' 'REPTEXT_DDIC'     'Flag No',

            'S' 'FIELDNAME'        'TYPE',
            'E' 'REPTEXT_DDIC'     'Type',

*            'S' 'FIELDNAME'        'MESSAGE',
*            'E' 'REPTEXT_DDIC'     'MESSAGE',

            'S' 'FIELDNAME'        'TRAN_DATE',
            'E' 'REPTEXT_DDIC'     'Date',

            'S' 'FIELDNAME'        'TRAN_TIME',
            'E' 'REPTEXT_DDIC'     'Time',

            'S' 'FIELDNAME'        'ZR2PRO',
            'E' 'REPTEXT_DDIC'      'Re-Processing Ind',

            'S' 'FIELDNAME'        'ZREDOC',
            'E' 'REPTEXT_DDIC'     'Re-Processing Doc',

            'S' 'FIELDNAME'        'BANFN',
            ' ' 'EMPHASIZE'        'C100',
            ' ' 'HOTSPOT'          'X',
            'E' 'REPTEXT_DDIC'     'P/R Number',

            'S' 'FIELDNAME'        'BNFPO',
            'E' 'REPTEXT_DDIC'     'Item Number',

            'S' 'FIELDNAME'        'BSART',
            'E' 'REPTEXT_DDIC'     'P/R Doc Type',

            'S' 'FIELDNAME'        'KNTTP',
            'E' 'REPTEXT_DDIC'     'Account Assignment Category',

            'S' 'FIELDNAME'        'PSTYP',
            'E' 'REPTEXT_DDIC'     'Item Category',

            'S' 'FIELDNAME'        'MATNR',
            'E' 'REPTEXT_DDIC'     'Material Number',

            'S' 'FIELDNAME'        'TXZ01',
            'E' 'REPTEXT_DDIC'     'Short Text',

            'S' 'FIELDNAME'        'WERKS',
            'E' 'REPTEXT_DDIC'     'Plant',

            'S' 'FIELDNAME'        'LGORT',
            ' ' 'REPTEXT_DDIC'     'Storage Location',

            'S' 'FIELDNAME'        'MATKL',
            'E' 'REPTEXT_DDIC'     'Material Group',

            'S' 'FIELDNAME'        'MENGE',
            ' ' 'REPTEXT_DDIC'     'P/R Quantity',

            'S' 'FIELDNAME'        'MEINS',
            'E' 'REPTEXT_DDIC'     'P/R Unit of Measure',

            'S' 'FIELDNAME'        'LPEIN',
            'E' 'REPTEXT_DDIC'     'Category of Delivery Date',

            'S' 'FIELDNAME'        'LFDAT',
            'E' 'REPTEXT_DDIC'     'Item Delivery Date',

            'S' 'FIELDNAME'        'PREIS',
            'E' 'REPTEXT_DDIC'     'P/R Price',

            'S' 'FIELDNAME'        'PEINH',
            'E' 'REPTEXT_DDIC'     'Price Unit',

            'S' 'FIELDNAME'        'EKGRP',
            'E' 'REPTEXT_DDIC'     'P/R Group',

            'S' 'FIELDNAME'        'EKORG',
            'E' 'REPTEXT_DDIC'     'Purchasing Organization',

            'S' 'FIELDNAME'        'AFNAM',
            'E' 'REPTEXT_DDIC'     'Name of Requisitioner/Requester',

            'S' 'FIELDNAME'        'BEDNR',
            'E' 'REPTEXT_DDIC'     'Requirement Tracking Number',

            'S' 'FIELDNAME'        'LIFNR',
            'E' 'REPTEXT_DDIC'     'Desired Vendor',

*            'S' 'FIELDNAME'        'DISPO',
*            'E' 'REPTEXT_DDIC'     'MRP Controller',

            'S' 'FIELDNAME'        'FLIEF',
            'E' 'REPTEXT_DDIC'     'Fixed Vendor',

            'S' 'FIELDNAME'        'WAERS',
            'E' 'REPTEXT_DDIC'     'Currency',

            'S' 'FIELDNAME'        'KOSTL',
            'E' 'REPTEXT_DDIC'     'Cost Center',

            'S' 'FIELDNAME'        'AUFNR',
            'E' 'REPTEXT_DDIC'     'Order Number',

*            'S' 'FIELDNAME'        'ANLN1',
*            'E' 'REPTEXT_DDIC'     'Asset',

            'S' 'FIELDNAME'        'SAKTO',
            'E' 'REPTEXT_DDIC'     'G/L Account Number'.

*            'S' 'FIELDNAME'        'KOKRS',
*            'E' 'REPTEXT_DDIC'     'Controlling Area',
*
*            'S' 'FIELDNAME'        'GEBER',
*            'E' 'REPTEXT_DDIC'     'Fund',
*
*            'S' 'FIELDNAME'        'FISTL',
*            'E' 'REPTEXT_DDIC'     'Funds Center',
*
*            'S' 'FIELDNAME'        'FIPOS',
*            'E' 'REPTEXT_DDIC'     'Commitment Item'.
*---// Modification 2006.01.24
*--In case is outbound, check as do not display field on screen--------*
  IF P_OUTBND NE 'X'.
    PERFORM FILL_FIELD_CATEGORY USING :
      'S' 'FIELDNAME'        'ZZVZ_PR',
      'E' 'REPTEXT_DDIC'     'VAATZ P/R Unique number',

      'S' 'FIELDNAME'        'ZZVZ_ITEM',
      'E' 'REPTEXT_DDIC'     'VZZTZ P/R ITEM Unique number'.
  ENDIF.
*----------------------------------------------------------------------*
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0302   text
*      -->P_0303   text
*      -->P_0304   text
*----------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY  USING  P_GUB P_FNAME P_CON.
  IF P_GUB = 'S'.
    CLEAR GS_FIELDCAT.
  ENDIF.
* Field symbol MOVE
  DATA L_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'GS_FIELDCAT-' P_FNAME  INTO L_COL.
  ASSIGN      (L_COL)         TO       <FS>.
  MOVE         P_CON          TO       <FS>.

  IF P_GUB = 'E'.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  ENDIF.
ENDFORM.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  layout_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_LAYOUT_S  text
*----------------------------------------------------------------------*
FORM LAYOUT_INIT USING P_LAYOUT_S TYPE SLIS_LAYOUT_ALV.
  P_LAYOUT_S-COLWIDTH_OPTIMIZE = 'X'.
  P_LAYOUT_S-ZEBRA             = 'X'.
  P_LAYOUT_S-BOX_FIELDNAME     = 'CHK'.

* PRINTING SETTINGS
  P_LAYOUT_S-GET_SELINFOS = 'X'.
  P_LAYOUT_S-GROUP_CHANGE_EDIT = 'X'.

  ALV_PRINT-NO_PRINT_SELINFOS = 'X'.
  ALV_PRINT-NO_COVERPAGE = 'X'.
  ALV_PRINT-NO_PRINT_LISTINFOS = 'X'.
ENDFORM.                    " layout_init
*&------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&------------------------------------------------------------------*
*       ??(REUSE_ALV_EVENTS_GET)?? CALL? FORM ???.
*-------------------------------------------------------------------*
FORM  PF_STATUS_SET USING P_RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR  'LIST'.
ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_EVENTS_T[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD USING P_EVENTS_T TYPE SLIS_T_EVENT.

  DATA : L_EVENT_S     TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = G_EVENTS_T.

** REUSE_ALV_EVENTS_GET ?? ???? - FORM PF_STATUS_SET
  READ TABLE G_EVENTS_T WITH KEY NAME = SLIS_EV_PF_STATUS_SET
                        INTO L_EVENT_S.

  IF SY-SUBRC EQ 0.

    MOVE  C_STATUS_SET  TO L_EVENT_S-FORM.
    APPEND L_EVENT_S    TO G_EVENTS_T.

  ENDIF.
** REUSE_ALV_EVENTS_GET ?? ???? - FORM USER_COMMAND
  READ TABLE G_EVENTS_T WITH KEY NAME = SLIS_EV_USER_COMMAND
                        INTO L_EVENT_S.

  IF SY-SUBRC EQ 0.

    MOVE   C_USER_COMMAND TO L_EVENT_S-FORM.
    APPEND L_EVENT_S      TO G_EVENTS_T.

  ENDIF.

** REUSE_ALV_EVENTS_GET 에서 수행된다 - FORM TOP_OF_PAGE
  READ TABLE G_EVENTS_T WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                        INTO L_EVENT_S.

  IF SY-SUBRC EQ 0.

    MOVE   C_TOP_OF_PAGE TO L_EVENT_S-FORM.
    APPEND L_EVENT_S     TO G_EVENTS_T.

  ENDIF.
ENDFORM.                    " eventtab_build
*&---------------------------------------------------------------------*
*&      Form  GET_LOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LOG_DATA .
*---
  CLEAR IT_TAB. REFRESH IT_TAB.

  CASE 'X'.
    WHEN P_SUCESS.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
               FROM ZTMM_VAZ_IF001
              WHERE FLAG IN R_FLAG
                AND BANFN IN S_BANFN
                AND SERNO IN S_SERNO
                AND TRAN_DATE IN S_DATE
                AND TYPE IN ('S', 'R').
    WHEN P_ERROR.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
               FROM ZTMM_VAZ_IF001
              WHERE FLAG IN R_FLAG
                AND BANFN IN S_BANFN
                AND SERNO IN S_SERNO
                AND TRAN_DATE IN S_DATE
                AND TYPE EQ 'E'.
    WHEN P_REP.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
               FROM ZTMM_VAZ_IF001
              WHERE FLAG IN R_FLAG
                AND BANFN IN S_BANFN
                AND SERNO IN S_SERNO
                AND TRAN_DATE IN S_DATE
                AND ( TYPE EQ 'R' OR ZR2PRO EQ 'S' ).
    WHEN P_SCHALL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
               FROM ZTMM_VAZ_IF001
              WHERE FLAG IN R_FLAG
                AND BANFN IN S_BANFN
                AND SERNO IN S_SERNO
                AND TRAN_DATE IN S_DATE.
  ENDCASE.

  DATA : L_TABIX LIKE SY-TABIX.

  LOOP AT IT_TAB.
    MOVE : SY-TABIX TO L_TABIX.
    CASE IT_TAB-TYPE.
      WHEN 'S'.
        MOVE : '@08@' TO IT_TAB-ICON.
      WHEN 'E'.
        IF IT_TAB-ZR2PRO EQ 'S'.
          MOVE : '@09@' TO IT_TAB-ICON.
        ELSE.
          MOVE : '@0A@' TO IT_TAB-ICON.
        ENDIF.
      WHEN 'R'.
        MOVE : '@09@' TO IT_TAB-ICON.
    ENDCASE.
    MODIFY IT_TAB INDEX L_TABIX.
  ENDLOOP.

  SORT IT_TAB BY SERNO DESCENDING
                 CUNT ASCENDING
                 BANFN ASCENDING
                 BNFPO ASCENDING.
ENDFORM.                    " GET_LOG_DATA
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND USING   R_UCOMM      LIKE SY-UCOMM
                          RS_SELFIELD  TYPE SLIS_SELFIELD.
  DATA L_ANS.
  RS_SELFIELD-REFRESH = 'X'.

  CASE R_UCOMM.

*---// Refresh button click
    WHEN 'REFR'.
      PERFORM GET_LOG_DATA.
*---// P/R No click -> ME53N displsy
    WHEN '&IC1'.
      READ TABLE IT_TAB INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        IF RS_SELFIELD-FIELDNAME = 'BANFN'.
          SET PARAMETER ID 'BAN' FIELD IT_TAB-BANFN.
          CALL TRANSACTION 'ME53N'.
        ENDIF.
      ENDIF.
*---// Detail button click -> Header & item detail display
    WHEN 'REPV'.
      CLEAR:   IT_OUT, COUNT.
*---// multi selection search checking
      LOOP AT IT_TAB.
        IF IT_TAB-CHK = 'X'.
          COUNT = COUNT + 1.
        ENDIF.
      ENDLOOP.
*---// searching display multi select error
      IF COUNT > 1.
        MESSAGE I002.

*        MESSAGE 'Selected several case.Please check your select is
*possible inquiry of only one case.' TYPE 'I'.

      ELSE.
        DATA : L_SERNO LIKE IT_TAB-SERNO,
               L_BANFN LIKE IT_TAB-BANFN,
               L_FLAG  LIKE IT_TAB-FLAG.
        READ TABLE IT_TAB WITH KEY CHK = 'X'.
        IF SY-SUBRC = 0.
          CLEAR : IT_REP, IT_REP[], L_SERNO, L_BANFN, L_FLAG.
          MOVE : IT_TAB-SERNO TO L_SERNO,
                 IT_TAB-BANFN TO L_BANFN,
                 IT_TAB-FLAG  TO L_FLAG.
          LOOP AT IT_TAB WHERE SERNO EQ L_SERNO
                           AND BANFN EQ L_BANFN.
            MOVE-CORRESPONDING IT_TAB TO IT_REP.
            APPEND IT_REP.
          ENDLOOP.

          PERFORM READ_ERROR_LOG USING L_SERNO L_FLAG.
          DESCRIBE TABLE IT_REP LINES W_REP_LINES.
          MOVE : 1 TO W_REP_INDEX.
          READ TABLE IT_REP INDEX W_REP_INDEX.
          MOVE-CORRESPONDING IT_REP TO ZTMM_VAZ_IF001.
          CALL SCREEN '0100'.
        ELSE.
          MESSAGE I001.
*          MESSAGE'No data with search conditions are existed.' TYPE 'I'
          .
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*---
  DATA : ANSWER,
         SAVE_OKCODE LIKE OK_CODE.

  MOVE : OK_CODE TO SAVE_OKCODE.

  CLEAR : OK_CODE.

  CASE SAVE_OKCODE.
    WHEN '&F03'.
      CLEAR : SAVE_OKCODE.
      CLEAR : IT_OUT, IT_OUT[].
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      CLEAR : SAVE_OKCODE.
      PERFORM POPUP_TO_CONFIRM USING    TEXT-P01
                                        TEXT-P02
                               CHANGING ANSWER.
      CHECK ANSWER = 'J'.

      PERFORM RE_PROCESS_MATERIAL.
    WHEN 'PREV'.
      CLEAR : SAVE_OKCODE.
      IF W_REP_INDEX EQ 1.
        MESSAGE S000 WITH 'The First Item'.
      ELSE.
        W_REP_INDEX = W_REP_INDEX - 1.
        IF W_REP_INDEX LE 0.
          MOVE : 1 TO W_REP_INDEX.
        ENDIF.
        CLEAR : IT_REP.
        READ TABLE IT_REP INDEX W_REP_INDEX.
        MOVE-CORRESPONDING IT_REP TO ZTMM_VAZ_IF001.
      ENDIF.
    WHEN 'NEXT'.
      CLEAR : SAVE_OKCODE.
      IF W_REP_INDEX EQ W_REP_LINES.
        MESSAGE S000 WITH 'The Last Item'.
      ELSE.
        W_REP_INDEX = W_REP_INDEX + 1.
        IF W_REP_INDEX GE W_REP_LINES.
          MOVE : W_REP_LINES TO W_REP_INDEX.
        ENDIF.
        CLEAR : IT_REP.
        READ TABLE IT_REP INDEX W_REP_INDEX.
        MOVE-CORRESPONDING IT_REP TO ZTMM_VAZ_IF001.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  IF IT_REP-ICON NE '@0A@' OR IT_REP-FLAG EQ '3'.
    SET PF-STATUS 'DETAIL' EXCLUDING 'REP'.
  ELSE.
    SET PF-STATUS 'DETAIL'.
  ENDIF.

*  SET TITLEBAR  'Detail List'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.
  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING CONTAINER_NAME = 'CONTROL_AREA'.
    CREATE OBJECT ALV_GRID
      EXPORTING       I_PARENT = G_CUSTOM_CONTAINER.
  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRANSFER_DATA OUTPUT.
* Set a layout for the grid control
  PERFORM LAYOUT.
  PERFORM FIELD_CAT.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = GS_LAYOUT
    CHANGING
      IT_FIELDCATALOG = GT_FDCAT
      IT_OUTTAB       = IT_OUT[].
ENDMODULE.                 " TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LAYOUT .
  CLEAR GS_LAYOUT.
  GS_LAYOUT-CWIDTH_OPT = 'X'.  "??? ???
  GS_LAYOUT-ZEBRA      = 'X'.
ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CAT .
  REFRESH GT_FDCAT.
  PERFORM FIELD_CAT_BUILD USING :

    'S' 'FIELDNAME'       'SERNOI',
    'E' 'COLTEXT'         'Item Sequential No',

    'S' 'FIELDNAME'       'FLAG',
    'E' 'COLTEXT'         'FLAG',

    'S' 'FIELDNAME'       'TYPE',
    'E' 'COLTEXT'         'TYPE',

    'S' 'FIELDNAME'       'MESSAGE',
    'E' 'COLTEXT'         'MESSAGE'.
*---// Modification 2006.01.24
*--In case is outbound, check as do not display field on screen--------*
  IF P_OUTBND NE 'X'.
    PERFORM FIELD_CAT_BUILD USING :
      'S' 'FIELDNAME'       'ZZVZ_PR',
      'E' 'COLTEXT'         'VAATZ P/R Unique number',

      'S' 'FIELDNAME'       'ZZVZ_ITEM',
      'E' 'COLTEXT'         'VAATZ P/R ITEM Unique number'.
  ENDIF.
*----------------------------------------------------------------------*
ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1286   text
*      -->P_1287   text
*      -->P_1288   text
*----------------------------------------------------------------------*
FORM FIELD_CAT_BUILD  USING    VALUE(P_GUB)
                               VALUE(P_FNAME)
                               VALUE(P_CON).
  IF P_GUB = 'S'.
    CLEAR GS_FDCAT.
  ENDIF.
* ?? MOVE
  DATA L_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'GS_FDCAT-' P_FNAME  INTO L_COL.
  ASSIGN      (L_COL)         TO       <FS>.
  MOVE         P_CON          TO       <FS>.

  IF P_GUB = 'E'.
    APPEND GS_FDCAT TO GT_FDCAT.
  ENDIF.
ENDFORM.                    " FIELD_CAT_BUILD

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DATA: LV_MODE TYPE I.
  DATA: HEADER TYPE SLIS_LISTHEADER OCCURS 0 WITH HEADER LINE.

  REFRESH HEADER.

  HEADER-TYP = 'H'.

  HEADER-INFO = 'Purchase Requisition log List'(T20).
  CLEAR: HEADER-KEY.
  APPEND HEADER.

  HEADER-TYP = 'S'.
  HEADER-KEY = 'Print date'(106).
  WRITE: SY-DATLO TO HEADER-INFO.
  APPEND HEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            I_LOGO             = 'KMS_ALV_LOGO'
            IT_LIST_COMMENTARY = HEADER[].
ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
*       MODULE screen_control OUTPUT                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE SCREEN_CONTROL OUTPUT.
  CHECK IT_REP-ICON EQ '@0A@'.  " error
  CHECK IT_REP-ZFLAG NE 'D'.    " delete
  CHECK IT_REP-FLAG NE '3'.     " outbound

  LOOP AT SCREEN.
    SCREEN-INPUT = 1.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " screen_control  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_TEXT_P02  text
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING    P_TEXT_P01
                               P_TEXT_P02
                      CHANGING P_ANSWER.
*----------------------------------------------------------------------*
*  MESSAGE POPUP
*----------------------------------------------------------------------*
  DATA: BEGIN OF POP,
        TITEL     LIKE SPOP-TITEL,
        DIAGNOSE1 LIKE SPOP-DIAGNOSE1,
        DIAGNOSE2 LIKE SPOP-DIAGNOSE2,
        DIAGNOSE3 LIKE SPOP-DIAGNOSE3,
        TEXTLINE1 LIKE SPOP-TEXTLINE1,
        TEXTLINE2 LIKE SPOP-TEXTLINE2,
        TEXTLINE3 LIKE SPOP-TEXTLINE3,
        OPTION1   LIKE SPOP-VAROPTION1,
        OPTION2   LIKE SPOP-VAROPTION2,
        DEFAULT,
        ANSWER,
        END OF POP.

  DATA: CANCEL_DISPLAY.

  MOVE: P_TEXT_P01 TO POP-TEXTLINE1,
        P_TEXT_P02 TO POP-TITEL.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
*     DEFAULTOPTION        = 'Y'
      DIAGNOSETEXT1        = POP-DIAGNOSE1
*     DIAGNOSETEXT2        = ' '
*     DIAGNOSETEXT3        = ' '
      TEXTLINE1            = POP-TEXTLINE1
      TEXTLINE2            = POP-TEXTLINE2
      TITEL                = POP-TITEL
*     START_COLUMN         = 25
*     START_ROW            = 6
      CANCEL_DISPLAY       = CANCEL_DISPLAY
    IMPORTING
      ANSWER               = POP-ANSWER
    EXCEPTIONS
      OTHSERS              = 1.

  P_ANSWER = POP-ANSWER.
ENDFORM.                    " popup_to_confirm

*&---------------------------------------------------------------------*
*&      Form  re_process_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RE_PROCESS_MATERIAL.
*---
 DATA : IT_ZSMM_VAZ_IF081 LIKE ZSMM_VAZ_IF081 OCCURS 0 WITH HEADER LINE,
        IT_ZSMM_VAZ_IF001 LIKE ZSMM_VAZ_IF001 OCCURS 0 WITH HEADER LINE,
          IT_ZSMM_IF017 LIKE ZSMM_IF017 OCCURS 0 WITH HEADER LINE.
  DATA : IT_XMSG LIKE ZTISMESSAGE OCCURS 0 WITH HEADER LINE.
  DATA : L_RESULT(1).


  CLEAR : IT_ZSMM_VAZ_IF081, IT_ZSMM_VAZ_IF081[],
          IT_ZSMM_VAZ_IF001, IT_ZSMM_VAZ_IF001[],
          IT_ZSMM_IF017, IT_ZSMM_IF017[].

  LOOP AT IT_REP.
    MOVE-CORRESPONDING IT_REP TO IT_ZSMM_VAZ_IF081.
    APPEND IT_ZSMM_VAZ_IF081.
    MOVE-CORRESPONDING IT_REP TO IT_ZSMM_VAZ_IF001.
    APPEND IT_ZSMM_VAZ_IF001.
  ENDLOOP.

*  READ TABLE it_tab WITH KEY chk = 'X'.
*
*  MOVE-CORRESPONDING it_tab TO it_zsmm_if001.
*  APPEND it_zsmm_if001.
  CLEAR: L_RESULT.
  REFRESH: IT_ZSMM_IF017.
*  CALL FUNCTION 'ZMMF_VAZ_IF_PR_INBOUND'
*       TABLES
*            IT_ZSMM_IF081 = IT_ZSMM_VAZ_IF001
*            E_RETURN      = IT_ZSMM_IF017.
  CALL FUNCTION 'ZMMF_VAZ_IF_PR_INBOUND'
*       IMPORTING
*            O_RESULT      = L_RESULT
       TABLES
            IT_ZSMM_IF081 = IT_ZSMM_VAZ_IF081
            E_RETURN      = IT_ZSMM_IF017.

*---
  DATA : L_SERNO LIKE ZTMM_VAZ_IF001-SERNO.

  CLEAR : L_SERNO, IT_REP.

  READ TABLE IT_REP INDEX 1.

  SELECT MAX( SERNO ) INTO L_SERNO
                     FROM ZTMM_VAZ_IF001
                    WHERE BANFN EQ IT_REP-BANFN.

*  IF IT_ZSMM_IF017[] IS INITIAL.     " Success
  READ TABLE IT_ZSMM_IF017 WITH KEY TYPE = 'E'." not success
  IF SY-SUBRC <> 0.
    UPDATE ZTMM_VAZ_IF001 SET ZR2PRO = 'S'
                          ZREDOC = L_SERNO
                    WHERE SERNO EQ IT_REP-SERNO
                      AND BANFN EQ IT_REP-BANFN.
    COMMIT WORK AND WAIT.
    UPDATE ZTMM_VAZ_IF001 SET TYPE = 'R'
                          ZREDOC = IT_REP-SERNO
                    WHERE SERNO EQ L_SERNO.
    COMMIT WORK AND WAIT.
    MESSAGE I009(ZMMM) WITH 'Success!!'.
  ELSE.                              " Error
    DELETE FROM ZTMM_VAZ_IF001 WHERE SERNO EQ L_SERNO.
    DELETE FROM ZTMM_IF006 WHERE SERNO EQ L_SERNO.
    COMMIT WORK AND WAIT.

    LOOP AT IT_ZSMM_IF017.
      MOVE: IT_ZSMM_IF017-TYPE    TO IT_XMSG-MSGTY,
            IT_ZSMM_IF017-MESSAGE TO IT_XMSG-MSGTX.
      APPEND IT_XMSG.
    ENDLOOP.
    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
         EXPORTING
              XDOCNO_SHOW = 'X'
         TABLES
              XMSG        = IT_XMSG.
  ENDIF.
*  IF L_RESULT = 'S'.
*    UPDATE ZTMM_VAZ_IF001 SET ZR2PRO = 'S'
*                          ZREDOC = L_SERNO
*                    WHERE SERNO EQ IT_REP-SERNO
*                      AND BANFN EQ IT_REP-BANFN.
*    COMMIT WORK AND WAIT.
*    UPDATE ZTMM_VAZ_IF001 SET TYPE = 'R'
*                          ZREDOC = IT_REP-SERNO
*                    WHERE SERNO EQ L_SERNO.
*    COMMIT WORK AND WAIT.
*    MESSAGE I000(ZMMM) WITH 'Success!!'.
*  ELSE.                              " Error
*    DELETE FROM ZTMM_VAZ_IF001 WHERE SERNO EQ L_SERNO.
*    DELETE FROM ZTMM_IF006 WHERE SERNO EQ L_SERNO.
*    COMMIT WORK AND WAIT.
*    MOVE: 'E' TO IT_XMSG-MSGTY,
*          'Error in re_process'  TO IT_XMSG-MSGTX.
*
*    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
*         EXPORTING
*              XDOCNO_SHOW = 'X'
*         TABLES
*              XMSG        = IT_XMSG.
*  ENDIF.


*---
  PERFORM RANGE_CONVERSION.
  PERFORM GET_LOG_DATA.

  LEAVE TO SCREEN 0.
ENDFORM.                    " re_process_material

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT.
*---
  DATA : L_SORT LIKE LINE OF GT_SORT.

  CLEAR : L_SORT, GT_SORT[].

  L_SORT-SPOS = 1.
  L_SORT-FIELDNAME = 'SERNO'.
  L_SORT-DOWN = 'X'.
  APPEND L_SORT TO GT_SORT.

  CLEAR : L_SORT.

  L_SORT-SPOS = 2.
  L_SORT-FIELDNAME = 'BANFN'.
  L_SORT-UP = 'X'.
  APPEND L_SORT TO GT_SORT.

*  CLEAR : l_sort.
*
*  l_sort-spos = 3.
*  l_sort-fieldname = 'BNFPO'.
*  l_sort-up = 'X'.
*  APPEND l_sort TO gt_sort.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  read_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ERROR_LOG USING P_SERNO P_FLAG.
*---
  CLEAR : IT_OUT, IT_OUT[].

  SELECT * FROM ZTMM_IF006
           INTO CORRESPONDING FIELDS OF TABLE IT_OUT
          WHERE SERNO = P_SERNO
            AND FLAG  = P_FLAG.
ENDFORM.                    " read_error_log

*&---------------------------------------------------------------------*
*&      Module  modify_rep_tab  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_REP_TAB INPUT.
*---
  MOVE-CORRESPONDING ZTMM_VAZ_IF001 TO IT_REP.

  MODIFY IT_REP INDEX W_REP_INDEX.
ENDMODULE.                 " modify_rep_tab  INPUT
