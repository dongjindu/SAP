*&---------------------------------------------------------------------*
*& Report  ZMMR_IF005                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZMMR_IF005 MESSAGE-ID ZMM_IF
                   NO STANDARD PAGE HEADING.

*-------------------------------------------------------------*
*  SAP TABLES
*-------------------------------------------------------------*
TABLES: EKPO,
        MARA,
        MARD,
        MBEW,
        MKPF,
        ZTMM_IF018.
*-------------------------------------------------------------*
*  Globale Type(ALV)
*-------------------------------------------------------------*
TYPE-POOLS: SLIS.

*-------------------------------------------------------------*
*  Internal TABLE
*-------------------------------------------------------------*
DATA: BEGIN OF IT_TAB OCCURS 0.
DATA: CHK.
        INCLUDE STRUCTURE ZTMM_IF018.
DATA: END OF IT_TAB.

DATA: IT_HEAD LIKE TABLE OF ZTMM_IF018 WITH HEADER LINE.

*-------------------------------------------------------------*
*  RANGES
*-------------------------------------------------------------*
RANGES : R_TYPE FOR ZTMM_IF018-TYPE.

************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Select data checking field
DATA COUNT   TYPE I.

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
DATA : ALV_REPID                LIKE SY-REPID,
       ALV_VARIANT              LIKE DISVARIANT.
DATA : ALV_DETAIL_FUNC(30).

*--------------------------------------------------------------*
*  CONSTANTS:
*--------------------------------------------------------------*
CONSTANTS : C_STATUS_SET   TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
            C_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.

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

*---// table display
*DATA IT_OUT LIKE TABLE OF ZTMM_IF018 WITH HEADER LINE.

DATA: BEGIN OF IT_OUT OCCURS 0,
        CUNT    LIKE ZTMM_IF018-CUNT,
        ZCNT    LIKE ZTMM_IF018-ZCNT,
        TYPE    LIKE ZTMM_IF018-TYPE,
        MESSAGE LIKE ZTMM_IF018-MESSAGE,
      END OF IT_OUT.

DATA : G_MBLNR LIKE EKBE-BELNR,
       G_MJAHR LIKE EKBE-GJAHR,
       G_ZEILE LIKE EKBE-BUZEI,
       G_BUKRS LIKE EKKO-BUKRS.

************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
    S_EBELN FOR ZTMM_IF018-EBELN MODIF ID IOG,
    S_EBELP FOR ZTMM_IF018-EBELP MODIF ID IOG,
    S_SERNO FOR ZTMM_IF018-SERNO NO-EXTENSION NO INTERVALS,
    S_DATE  FOR SY-DATUM         MODIF ID IOG.
SELECTION-SCREEN END OF BLOCK BOX1.

*SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
*PARAMETER : P_GROUT AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETER :
    P_SUCCES RADIOBUTTON GROUP AB2,
    P_ERROR  RADIOBUTTON GROUP AB2,
    P_ALL    RADIOBUTTON GROUP AB2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK BOX3.

************************************************************************
* INITIALIZATION Event                                                 *
************************************************************************
INITIALIZATION.

  PERFORM LAYOUT_INIT USING G_LAYOUT_S.
  PERFORM EVENTTAB_BUILD USING G_EVENTS_T[].
  G_REPID = SY-REPID.

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

************************************************************************
* END-OF-SELECTION Event                                             *
************************************************************************
END-OF-SELECTION.

  PERFORM BUILD_FIELDCAT.
  PERFORM ALV_DISPLAY.

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
* ?? MOVE
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
        IF RS_SELFIELD-FIELDNAME = 'EBELN' OR
           RS_SELFIELD-FIELDNAME = 'EBELP'.

          SET PARAMETER ID 'BES' FIELD IT_TAB-EBELN.
          CALL TRANSACTION 'ME23N'.

        ELSEIF RS_SELFIELD-FIELDNAME = 'BELNR'.

          CLEAR : G_MBLNR, G_MJAHR, G_ZEILE,
                  G_BUKRS.

          SELECT SINGLE BUKRS
                   FROM EKKO
                   INTO G_BUKRS
                  WHERE EBELN = IT_TAB-EBELN.

          SELECT SINGLE BELNR GJAHR BUZEI
                   FROM EKBE
                   INTO (G_MBLNR, G_MJAHR, G_ZEILE)
                  WHERE EBELN = IT_TAB-EBELN
                    AND EBELP = IT_TAB-EBELP
                    AND BELNR = IT_TAB-BELNR.

          CHECK NOT G_MBLNR IS INITIAL.
          SET PARAMETER ID 'MBN' FIELD G_MBLNR.
          SET PARAMETER ID 'MJA' FIELD G_MJAHR.
          SET PARAMETER ID 'BUK' FIELD G_BUKRS.
*---// MIGO transaction call
          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
                I_ACTION = 'A04'
                I_REFDOC = 'R02'
                I_NOTREE = 'X'
*         I_NO_AUTH_CHECK
                I_SKIP_FIRST_SCREEN = 'X'
                I_DEADEND           = 'X'
                I_OKCODE            = 'OK_GO'
*         I_LEAVE_AFTER_POST    =
*         I_NEW_ROLLAREA        = 'X'
*         I_SYTCODE             =
*         I_EBELN               =
*         I_EBELP               =
                I_MBLNR             = G_MBLNR
                I_MJAHR             = G_MJAHR
                I_ZEILE             = G_ZEILE
*         I_TRANSPORT           =
*         I_ORDER_NUMBER        =
*         I_ORDER_ITEM          =
*         I_TRANSPORT_MEANS     =
*         I_TRANSPORTIDENT      =
*         I_INBOUND_DELIV       =
*         I_OUTBOUND_DELIV      =
*         I_RESERVATION_NUMB    =
*         I_RESERVATION_ITEM    =
            EXCEPTIONS
              ILLEGAL_COMBINATION = 1
              OTHER               = 2.
          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
        ENDIF.
      ENDIF.
*---// Detail button click -> Header & item detail display
    WHEN 'DETA'.
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

*        MESSAGE
*        'Selected several case. Please check your select is possible
*inquiry of only one case.'
*        TYPE 'I'.
      ELSE.
        READ TABLE IT_TAB WITH KEY CHK = 'X'.
        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING IT_TAB TO ZTMM_IF018.

*---// table display
*          SELECT * FROM ZTMM_IF018
*                   INTO CORRESPONDING FIELDS OF TABLE IT_OUT
*                  WHERE SERNO = IT_TAB-SERNO.

          SELECT SINGLE CUNT ZCNT TYPE MESSAGE
                   FROM ZTMM_IF018
                   INTO (IT_OUT-CUNT, IT_OUT-ZCNT,
                         IT_OUT-TYPE, IT_OUT-MESSAGE)
                  WHERE SERNO = IT_TAB-SERNO.
          APPEND IT_OUT.
          CALL SCREEN '0100'.
        ELSE.
          MESSAGE I001.
*         MESSAGE 'No data with search conditions are existed.' TYPE 'I'
.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " USER_COMMAND

*&------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&------------------------------------------------------------------*
*       ??(REUSE_ALV_EVENTS_GET)?? CALL? FORM ???.
*-------------------------------------------------------------------*
FORM  PF_STATUS_SET USING P_RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR  'Search List'.
ENDFORM.                    "PF_STATUS_SET

*&---------------------------------------------------------------------*
*&      Form  RANGE_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RANGE_CONVERSION.

*--- FLAG & TYPE RANGE

  CLEAR:   R_TYPE.
  REFRESH: R_TYPE.

  IF      P_SUCCES = 'X'.
    R_TYPE-SIGN    = 'I'.
    R_TYPE-OPTION  = 'EQ'.
    R_TYPE-LOW     = 'S'.

    APPEND R_TYPE.
    CLEAR  R_TYPE.

  ELSEIF  P_ERROR  = 'X'.
    R_TYPE-SIGN    = 'I'.
    R_TYPE-OPTION  = 'EQ'.
    R_TYPE-LOW     = 'E'.

    APPEND R_TYPE.
    CLEAR  R_TYPE.
  ENDIF.

ENDFORM.                    " RANGE_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  GET_LOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LOG_DATA .

  CLEAR   IT_TAB.
  REFRESH IT_TAB.

  SELECT * FROM ZTMM_IF018
           INTO CORRESPONDING FIELDS OF TABLE IT_TAB
          WHERE EBELN     IN S_EBELN
            AND EBELP     IN S_EBELP
            AND TYPE      IN R_TYPE
            AND SERNO     IN S_SERNO
            AND TRAN_DATE IN S_DATE.

ENDFORM.                    " GET_LOG_DATA
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_INIT
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

ENDFORM.                    " LAYOUT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
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

    MOVE  C_USER_COMMAND TO L_EVENT_S-FORM.
    APPEND L_EVENT_S     TO G_EVENTS_T.

  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT .
  PERFORM FILL_FIELD_CATEGORY USING :

            'S' 'FIELDNAME'        'SERNO',
            'E' 'REPTEXT_DDIC'     'Serial No',

            'S' 'FIELDNAME'        'EBELN',
            ' ' 'EMPHASIZE'        'C100',
            ' ' 'HOTSPOT'          'X',
            'E' 'REPTEXT_DDIC'     'Purchasing Doc',

            'S' 'FIELDNAME'        'EBELP',
*            ' ' 'EMPHASIZE'        'C200',
*            ' ' 'HOTSPOT'          'X',
            'E' 'REPTEXT_DDIC'     'PO Item',

            'S' 'FIELDNAME'        'BELNR',
            ' ' 'EMPHASIZE'        'C300',
            ' ' 'HOTSPOT'          'X',
            'E' 'REPTEXT_DDIC'     'Material Doc',

            'S' 'FIELDNAME'        'ZCNT',
            'E' 'REPTEXT_DDIC'     'Return count',

            'S' 'FIELDNAME'        'CUNT',
            'E' 'REPTEXT_DDIC'     'Item count',

            'S' 'FIELDNAME'        'TYPE',
            'E' 'REPTEXT_DDIC'     'TYPE',

*            'S' 'FIELDNAME'        'MESSAGE',
*            'E' 'REPTEXT_DDIC'     'MESSAGE',

            'S' 'FIELDNAME'        'BURKS',
            'E' 'REPTEXT_DDIC'     'Company Code',

            'S' 'FIELDNAME'        'WERKS',
            'E' 'REPTEXT_DDIC'     'Plant',

            'S' 'FIELDNAME'        'BUZEI',
            'E' 'REPTEXT_DDIC'     'Material Doc Item',

            'S' 'FIELDNAME'        'LIFNR',
            'E' 'REPTEXT_DDIC'     'Vendor',

            'S' 'FIELDNAME'        'MATNR',
            'E' 'REPTEXT_DDIC'     'Material',

            'S' 'FIELDNAME'        'TXZ01',
            'E' 'REPTEXT_DDIC'     'Material Description',

            'S' 'FIELDNAME'        'WRKST',
            ' ' 'REPTEXT_DDIC'     'Basic Material',

            'S' 'FIELDNAME'        'MEINS',
            'E' 'REPTEXT_DDIC'     'G/R UNIT OF MEASURE',

            'S' 'FIELDNAME'        'MENGE',
            ' ' 'REPTEXT_DDIC'     'P/O Quantity',

            'S' 'FIELDNAME'        'ZMENGE',
            'E' 'REPTEXT_DDIC'     'Pre-GR Quantity',

            'S' 'FIELDNAME'        'ZGRQTY',
            'E' 'REPTEXT_DDIC'     'GR Quantity',

            'S' 'FIELDNAME'        'NET_PRICE',
            'E' 'REPTEXT_DDIC'     'GR Quantity divide GR Amount price',

            'S' 'FIELDNAME'        'DMBTR',
            'E' 'REPTEXT_DDIC'     'Amount in local currency',

            'S' 'FIELDNAME'        'ZDMBTR',
            'E' 'REPTEXT_DDIC'     'Stock Update Amount',

            'S' 'FIELDNAME'        'STPRS',
            'E' 'REPTEXT_DDIC'     'Standard Price',

            'S' 'FIELDNAME'        'WAERS',
            'E' 'REPTEXT_DDIC'     'Currency Key',

            'S' 'FIELDNAME'        'BWART',
            'E' 'REPTEXT_DDIC'     'Movement Type',

            'S' 'FIELDNAME'        'LGPBE',
            'E' 'REPTEXT_DDIC'     'Storage bin',

            'S' 'FIELDNAME'        'CPUDT',
            'E' 'REPTEXT_DDIC'     'DESIRED VENDOR',

            'S' 'FIELDNAME'        'CPUTM',
            'E' 'REPTEXT_DDIC'     'MRP CONTROLLER',

            'S' 'FIELDNAME'        'USNAM',
            'E' 'REPTEXT_DDIC'     'User Serial no',

            'S' 'FIELDNAME'        'ZNAME',
            'E' 'REPTEXT_DDIC'     'User name'.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
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
        I_CALLBACK_PROGRAM       = G_REPID
**     I_STRUCTURE_NAME               =
**     IT_EXCLUDING                   =
**     IT_SPECIAL_GROUPS              =
**     IT_FILTER                      =
**     IS_SEL_HIDE                    =
**     I_DEFAULT                      = 'X'
**     I_SAVE                         = ' '
        IT_EVENTS                = G_EVENTS_T[]
        IT_FIELDCAT              = GT_FIELDCAT[]
*        it_sort                  = gt_sort
**        I_DEFAULT                = 'X'
**        IS_VARIANT               = VARIANT_DETAIL
*        is_variant              = alv_variant
*        i_save                  = g_save
        IS_LAYOUT               = G_LAYOUT_S
*        is_print                = alv_print
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
      TABLES
        T_OUTTAB                 = IT_TAB[].

ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'DETAIL'.
*  SET TITLEBAR 'Detail list'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*  IF SY-UCOMM = '&F03'.
*    LEAVE TO SCREEN 0.
*  ENDIF.

  IF SY-UCOMM = '&F03'.
    CLEAR   IT_OUT.
    REFRESH IT_OUT.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 OUTPUT.
  IF SY-UCOMM = '&F03'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0100  OUTPUT
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
*&      Form  FIELD_CAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1258   text
*      -->P_1259   text
*      -->P_1260   text
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

    'S' 'FIELDNAME'       'CUNT',
    'E' 'COLTEXT'         'G/R Outbound  item number',

    'S' 'FIELDNAME'       'ZCNT',
    'E' 'COLTEXT'         'Transmission Serial No.',

    'S' 'FIELDNAME'       'TYPE',
    'E' 'COLTEXT'         'TYPE',

    'S' 'FIELDNAME'       'MESSAGE',
    'E' 'COLTEXT'         'MESSAGE'.
ENDFORM.                    " FIELD_CAT
