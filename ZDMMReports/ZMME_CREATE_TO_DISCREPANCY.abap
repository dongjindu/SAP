************************************************************************
* Program Name      : ZMME_CREATE_TO_DISCREPANCY
* Author            : Furong Wang
* Creation Date     : 10/28/2010
* Specifications By :
* Development Request No : UD1K909855
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZMME_CREATE_TO_DISCREPANCY NO STANDARD PAGE HEADING
                        LINE-SIZE 400
                        MESSAGE-ID ZMMM.

TABLES: LQUA.

CONSTANTS: C_YELL(4)  VALUE 'C310',
    C_GREEN(4) VALUE 'C510',
    C_blue(4) VALUE 'C210',
    C_RED(4)   VALUE 'C610'.

DATA : W_SUBRC LIKE SY-SUBRC,
       WA_DATUM LIKE SY-DATUM.

DATA : IT_BDCMSGCOLL LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       WA_BDCMSGCOLL LIKE LINE OF IT_BDCMSGCOLL.

DATA : BEGIN OF IT_ITAB OCCURS 0,
       MATNR LIKE LQUA-MATNR,
       LGORT LIKE LQUA-LGORT,
       SRC_LGTYP LIKE LAGP-LGTYP,
       DES_LGTYP LIKE LAGP-LGTYP,
       SRC_LGPLA LIKE LAGP-LGPLA,
       DES_LGPLA LIKE LAGP-LGPLA,
       GESME LIKE LQUA-GESME,
       MSGTY LIKE ZTMM_STL_LOG-MSGTY,
       LINECOLOR(4),     " ALV Color
       MESSA(80),
       END OF IT_ITAB.

* ALV
DATA: WC_SPLITTER_0100     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      WC_SP_CONTAINER_0100 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WC_CONTROL_0100      TYPE        SCRFNAME VALUE 'CC_0100'.

DATA: WC_CONTAINER_ITAB TYPE REF TO CL_GUI_CONTAINER,
      WC_GRID_ITAB      TYPE REF TO CL_GUI_ALV_GRID,
      V_LAYOUT_ITAB     TYPE LVC_S_LAYO,
      V_VARIANT_ITAB    TYPE DISVARIANT,
      IT_SORT_ITAB      TYPE LVC_T_SORT WITH HEADER LINE,
      IT_FILTER_ITAB    TYPE LVC_T_FILT WITH HEADER LINE.

DATA : IT_ROWS         TYPE LVC_T_ROW  WITH HEADER LINE,
       IT_ROW_NO       TYPE LVC_T_ROID WITH HEADER LINE,
       IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

DATA : V_FIELDNAME   TYPE LINE OF SLIS_T_FIELDCAT_ALV,
       V_REPID       LIKE SY-REPID,
       V_CNT         TYPE I,                   "Field count
       V_SCROLL      TYPE LVC_S_STBL,
       V_SAVE        TYPE C   VALUE 'A'.       "for Parameter I_SAVE

DATA: V_CONTAINER(100),
      V_CONTROL(100),
      V_SPLITTER(100),
      V_GRID(100),
      V_ITAB(100),
      V_STRUCTURE LIKE DD02L-TABNAME.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID.

FIELD-SYMBOLS: <FS_CONTAINER> TYPE REF TO   CL_GUI_CUSTOM_CONTAINER,
               <FS_CONTROL>   TYPE          SCRFNAME,
               <FS_SPLITTER>  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
               <FS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID,
               <FS_ITAB>      TYPE STANDARD TABLE,
               <FS_ITAB_OLD>  TYPE STANDARD TABLE.

CONSTANTS: C_STRUCTURE(100) VALUE 'IT_'.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW
                      E_COLUMN
                      ES_ROW_NO.

    METHODS:
    HANDLE_HOTSPOT_CLICK
        FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID
                      E_COLUMN_ID
                      ES_ROW_NO.

    METHODS:
    HANDLE_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DBL_CLICK_0100 USING E_COLUMN-FIELDNAME
                                 ES_ROW_NO-ROW_ID.
  ENDMETHOD.                           "handle_double_click

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HOTSPOT_CLICK      USING   E_ROW_ID
                                       E_COLUMN_ID
                                       ES_ROW_NO.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD HANDLE_TOOLBAR.

  ENDMETHOD.                    "handle_toolbar
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_MATNR FOR LQUA-MATNR,
                 S_DATE FOR SY-DATUM OBLIGATORY,     "Current date
                 S_TIME FOR SY-UZEIT.
SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM GET_DATA.


END-OF-SELECTION.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999(ZMMM) WITH 'There is no data!'(001).
    EXIT.
  ELSE.
    PERFORM CREATE_TRANSFER_ORDER.
    CALL SCREEN 0100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT B~MATNR GESME A~LGTYP AS SRC_LGTYP C~LGTYP AS DES_LGTYP
   A~LGPLA AS SRC_LGPLA C~LGPLA AS DES_LGPLA
   INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
   FROM LAGP AS A
   INNER JOIN LQUA AS B
   ON A~LGTYP = B~LGTYP
   AND A~LGPLA = B~LGPLA
   INNER JOIN MLGT AS C
   ON B~MATNR = C~MATNR
   WHERE A~LGNUM = 'P01'
     AND A~LGTYP = '999'
     AND A~KZLER = ' '
     AND A~KZDYN = 'X'
     AND GESME > 0
     AND A~BDATU IN S_DATE
     AND A~BZEIT IN S_TIME
     AND B~MATNR IN S_MATNR
     AND C~LGTYP = '422'
     AND LVORM = ' '.

  SELECT B~MATNR GESME A~LGTYP AS DES_LGTYP C~LGTYP AS SRC_LGTYP
   A~LGPLA AS DES_LGPLA C~LGPLA AS SRC_LGPLA
   APPENDING CORRESPONDING FIELDS OF TABLE IT_ITAB
   FROM LAGP AS A
   INNER JOIN LQUA AS B
   ON A~LGTYP = B~LGTYP
   AND A~LGPLA = B~LGPLA
   INNER JOIN MLGT AS C
   ON B~MATNR = C~MATNR
   WHERE A~LGNUM = 'P01'
     AND A~LGTYP = '999'
     AND A~KZLER = ' '
     AND A~KZDYN = 'X'
     AND GESME < 0
     AND A~BDATU IN S_DATE
     AND A~BZEIT IN S_TIME
     AND B~MATNR IN S_MATNR
     AND C~LGTYP = '422'
     AND LVORM = ' '.

ENDFORM.                    " get_des_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM BDC_PROCESSING_LT01 TABLES   EXT_BDCMSGCOLL STRUCTURE BDCMSGCOLL
                         CHANGING VALUE(P_SUBRC).

  CLEAR : EXT_BDCMSGCOLL, EXT_BDCMSGCOLL[], P_SUBRC.

  DATA : LV_BWLVS_002     TYPE BDCDATA-FVAL,   "Movement type
         LV_MATNR_003     TYPE BDCDATA-FVAL,
         LV_ANFME_004     TYPE BDCDATA-FVAL,
         LV_LGORT_006    TYPE BDCDATA-FVAL,
         LV_ANFME_007     TYPE BDCDATA-FVAL,
         LV_ALTME_008     TYPE BDCDATA-FVAL,
         LV_VLTYP_009     TYPE BDCDATA-FVAL,
         LV_VLPLA_010     TYPE BDCDATA-FVAL,
         LV_NLTYP_011     TYPE BDCDATA-FVAL,
         LV_NLPLA_012     TYPE BDCDATA-FVAL,
         LV_REFNR_013     TYPE BDCDATA-FVAL.   "Group(Feeder)

  LV_BWLVS_002 = '977'.
  LV_REFNR_013 = 'DIFF'. "Group(Feeder)
  LV_MATNR_003  = IT_ITAB-MATNR.
  IF IT_ITAB-GESME < 0.
    LV_ANFME_004  = - IT_ITAB-GESME.
  ELSE.
    LV_ANFME_004  = IT_ITAB-GESME.
  ENDIF.
  LV_ANFME_007  = IT_ITAB-GESME.
*  lv_altme_008  = it_toline-meins.
  LV_VLTYP_009  = IT_ITAB-SRC_LGTYP.
  LV_VLPLA_010  = IT_ITAB-SRC_LGPLA.
  LV_NLTYP_011  = IT_ITAB-DES_LGTYP.
  LV_NLPLA_012  = IT_ITAB-DES_LGPLA.
  LV_LGORT_006  = 'P400'.  "IT_ITAB-LGORT.

  CONDENSE : LV_BWLVS_002,  "Movement type
             LV_MATNR_003,
             LV_ANFME_004,
             LV_ANFME_007,
             LV_ALTME_008,
             LV_VLTYP_009,
             LV_VLPLA_010,
             LV_NLTYP_011,
             LV_NLPLA_012,
             LV_REFNR_013.

*--- BDC for LT01(Create TO)
  CALL FUNCTION 'Z_FMM_6012_01'
       EXPORTING
            LGNUM_001 = 'P01'  "Warehouse number
            REFNR_013 = LV_REFNR_013  "Group(Feeder)
            BWLVS_002 = LV_BWLVS_002  "Movement type
            MATNR_003 = LV_MATNR_003  "Material
            ANFME_004 = LV_ANFME_004
            WERKS_005 = 'P001'  "Plant
            LGORT_006 = LV_LGORT_006  "Storage Location
            ANFME_007 = LV_ANFME_007
            ALTME_008 = LV_ALTME_008
            VLTYP_009 = LV_VLTYP_009  "Src Storage Type
            VLPLA_010 = LV_VLPLA_010  "Src Storage Bin
            NLTYP_011 = LV_NLTYP_011  "Des Storage Type'
            NLPLA_012 = LV_NLPLA_012  "Des Storage Bin '
       IMPORTING
            SUBRC     = P_SUBRC
       TABLES
            MESSTAB   = EXT_BDCMSGCOLL[].
ENDFORM.                    " bdc_processing_lt01

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_TRANSFER_ORDER.

  DATA : L_TABIX LIKE SY-TABIX,
         L_MESSA(80).
  DATA : LW_BEGZT(14).
  DATA : L_DATUM TYPE D.
  DATA : L_STATS LIKE ZTMM_STL_LOG-STATS.

  LOOP AT IT_ITAB.
    CLEAR : L_STATS.
    MOVE : SY-TABIX TO L_TABIX.

*--- BDC Processing of LT01
    PERFORM BDC_PROCESSING_LT01 TABLES   IT_BDCMSGCOLL
                                  CHANGING W_SUBRC.

    IF W_SUBRC EQ 0.
      MOVE : C_blue TO IT_ITAB-LINECOLOR,
             'S'     TO IT_ITAB-MSGTY.
      CLEAR : IT_BDCMSGCOLL.
      READ TABLE IT_BDCMSGCOLL with key MSGTYP = 'S'.
      MOVE : IT_BDCMSGCOLL-MSGV1 TO IT_ITAB-MESSA.
    ELSE.
      MOVE : C_RED TO IT_ITAB-LINECOLOR,
             'E'   TO IT_ITAB-MSGTY.

      CLEAR : IT_BDCMSGCOLL, L_MESSA.
      READ TABLE IT_BDCMSGCOLL WITH KEY MSGTYP = 'E'.
      IF SY-SUBRC EQ 0.
        PERFORM GET_MESSAGE USING IT_BDCMSGCOLL-MSGID
                                  IT_BDCMSGCOLL-MSGNR
                                  IT_BDCMSGCOLL-MSGV1
                                  IT_BDCMSGCOLL-MSGV2
                                  IT_BDCMSGCOLL-MSGV3
                                  IT_BDCMSGCOLL-MSGV4
                         CHANGING L_MESSA.

        MOVE : L_MESSA TO IT_ITAB-MESSA.
      ENDIF.
    ENDIF.
    MODIFY IT_ITAB INDEX L_TABIX.
    CLEAR : IT_BDCMSGCOLL, IT_BDCMSGCOLL[].
    CLEAR : IT_ITAB.
  ENDLOOP.
ENDFORM.                    " create_transfer_order
*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL_MSGID  text
*      -->P_IT_BDCMSGCOLL_MSGNR  text
*      -->P_IT_BDCMSGCOLL_MSGV1  text
*      -->P_IT_BDCMSGCOLL_MSGV2  text
*      -->P_IT_BDCMSGCOLL_MSGV3  text
*      -->P_IT_BDCMSGCOLL_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE USING P_MSGID
                       P_MSGNR
                       P_MSGV1
                       P_MSGV2
                       P_MSGV3
                       P_MSGV4
              CHANGING P_L_MESSA.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = P_MSGID
            MSGNR               = P_MSGNR
            MSGV1               = P_MSGV1
            MSGV2               = P_MSGV2
            MSGV3               = P_MSGV3
            MSGV4               = P_MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = P_L_MESSA.
ENDFORM.                    " get_message
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR  '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CONTROL OUTPUT.
  PERFORM CREATE_SPLITTER USING SY-DYNNR.
  PERFORM CREATE_GRID USING 'ITAB'   '1' '1'.

ENDMODULE.                 " CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_SPLITTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_SPLITTER USING PV_DYNNR.
  CONCATENATE: 'WC_SP_CONTAINER_' PV_DYNNR INTO V_CONTAINER.
  ASSIGN:      (V_CONTAINER)               TO   <FS_CONTAINER>.

  CONCATENATE: 'WC_CONTROL_' PV_DYNNR INTO V_CONTROL.
  ASSIGN:      (V_CONTROL)            TO   <FS_CONTROL>.

  CONCATENATE: 'WC_SPLITTER_' PV_DYNNR INTO V_SPLITTER.
  ASSIGN:      (V_SPLITTER)            TO   <FS_SPLITTER>.

  IF <FS_CONTAINER> IS INITIAL.
    CREATE OBJECT <FS_CONTAINER>
      EXPORTING
        CONTAINER_NAME              = <FS_CONTROL>
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT <FS_SPLITTER>
      EXPORTING
        PARENT            = <FS_CONTAINER>
        ROWS              = 1
        COLUMNS           = 1
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1136   text
*----------------------------------------------------------------------*
FORM CREATE_GRID USING PV_GRID PV_ROW PV_COLUMN.
  DATA: LV_CONTAINER(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO CL_GUI_CONTAINER.

  CONCATENATE: 'WC_CONTAINER_' PV_GRID INTO LV_CONTAINER.
  ASSIGN:      (LV_CONTAINER)          TO   <LFS_CONTAINER>.

  IF <LFS_CONTAINER> IS INITIAL.
    PERFORM CREATE_GRID_CONTAINER USING PV_GRID PV_ROW PV_COLUMN.
    PERFORM BUILD_FIELD_CATALOG   USING PV_GRID.
    PERFORM SET_ATTRIBUTE         USING PV_GRID.
    PERFORM ASSIGN_ITAB_TO_ALV    USING PV_GRID.
    PERFORM SSSIGN_EVENT          USING PV_GRID.
  ELSE.
    PERFORM BUILD_FIELD_CATALOG   USING PV_GRID.
    PERFORM SET_ATTRIBUTE         USING PV_GRID.
    PERFORM REFRESH_GRID USING PV_GRID.
  ENDIF.
ENDFORM.                    " CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM REFRESH_GRID USING PV_GRID.
  DATA: LV_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  V_SCROLL-ROW = 'X'.
  V_SCROLL-COL = 'X'.

  CALL METHOD <LFS_GRID>->REFRESH_TABLE_DISPLAY
    EXPORTING
*      i_soft_refresh = 'X'
      IS_STABLE      = V_SCROLL.
ENDFORM.                    " REFRESH_GRID

*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM CREATE_GRID_CONTAINER USING PV_GRID PV_ROW PV_COLUMN.
  DATA: LV_CONTAINER(100),
        LV_GRID(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO   CL_GUI_CONTAINER,
                 <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_CONTAINER_' PV_GRID INTO LV_CONTAINER.
  ASSIGN:      (LV_CONTAINER)          TO   <LFS_CONTAINER>.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CALL METHOD <FS_SPLITTER>->GET_CONTAINER
    EXPORTING
      ROW       = PV_ROW
      COLUMN    = PV_COLUMN
    RECEIVING
      CONTAINER = <LFS_CONTAINER>.

  CREATE OBJECT <LFS_GRID>
    EXPORTING
      I_PARENT          = <LFS_CONTAINER>
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CREATE_GRID_CONTAINER
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING PV_GRID.
*--- adjust field catalog to suppress the output of already
*  displayed key fields of structure

  DATA: LV_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

*  CALL METHOD <alv>->get_frontend_fieldcatalog
*    IMPORTING
*      et_fieldcatalog = it_fieldcat[].

  PERFORM SET_FIELDNAME USING PV_GRID.
  PERFORM SET_SCREEN_FIELDS USING PV_GRID.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = IT_FIELDCAT[].
ENDFORM.                    "build_field_catalog
*----------------------------------------------------------------------*
FORM SET_FIELDNAME USING PV_GRID.

  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  MOVE: SY-REPID TO V_REPID.
  CONCATENATE C_STRUCTURE PV_GRID INTO LW_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = V_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = V_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME[].


ENDFORM.                    "set_fieldname
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS USING PV_GRID.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'MATNR'        ' ',
                                  ' ' 'COLTEXT'      'Material',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100'.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                 'S' 'SRC_LGTYP'        ' ',
                                 ' ' 'COLTEXT'      'Src Type',
                                 ' ' 'FIX_COLUMN'   'X',
                                 'E' 'EMPHASIZE'      'C250',

                                 'S' 'DES_LGTYP'        ' ',
                                 ' ' 'COLTEXT'      'Des Type',
                                 ' ' 'FIX_COLUMN'   'X',
                                 'E' 'EMPHASIZE'      'C250',

                                'S' 'SRC_LGPLA'        ' ',
                                 ' ' 'COLTEXT'      'Src Bin',
                                 ' ' 'FIX_COLUMN'   'X',
                                 'E' 'EMPHASIZE'      'C250',

                                'S' 'DES_LGPLA'        ' ',
                                 ' ' 'COLTEXT'      'Des Bin',
                                 ' ' 'FIX_COLUMN'   'X',
                                 'E' 'EMPHASIZE'      'C250',

                                'S' 'GESME'        ' ',
                                ' ' 'COLTEXT'      'QTY',
                                ' ' 'JUST'           'R',
*                                ' ' 'DO_SUM'         'X',
                                ' ' 'NO_ZERO'        'X',
                                ' ' 'FIX_COLUMN'   'X',
                                 'E' 'EMPHASIZE'      'C250',

                              'S' 'MESSA'        ' ',
                                 ' ' 'COLTEXT'      'Remarks',
                                 ' ' 'FIX_COLUMN'   'X',
                                 'E' 'EMPHASIZE'      'C250'.



ENDFORM.                    "set_screen_fields_0101
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV USING PV_GRID.
  DATA: LV_GRID(100),
        LV_ITAB(100),
        LV_LAYOUT(100),
        LV_VARIANT(100),
        LV_SORT(100),
        LV_FILTER(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_ITAB>  TYPE STANDARD TABLE,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO,
                 <LFS_VARIANT> TYPE DISVARIANT,
                 <LFS_SORT> TYPE LVC_T_SORT,
                 <LFS_FILTER> TYPE LVC_T_FILT.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'V_LAYOUT_' PV_GRID INTO LV_LAYOUT.
  ASSIGN:      (LV_LAYOUT)         TO   <LFS_LAYOUT>.

  CONCATENATE: 'V_VARIANT_' PV_GRID INTO LV_VARIANT.
  ASSIGN:      (LV_VARIANT)         TO   <LFS_VARIANT>.

  CONCATENATE: 'IT_'      PV_GRID '[]' INTO LV_ITAB.
  ASSIGN:      (LV_ITAB)               TO   <LFS_ITAB>.

  CONCATENATE: 'IT_SORT_' PV_GRID '[]' INTO LV_SORT.
  ASSIGN:      (LV_SORT)               TO   <LFS_SORT>.

  CONCATENATE: 'IT_FILTER_' PV_GRID '[]' INTO LV_FILTER.
  ASSIGN:      (LV_FILTER)               TO   <LFS_FILTER>.

  CONCATENATE: C_STRUCTURE PV_GRID INTO V_STRUCTURE.

  CALL METHOD <LFS_GRID>->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME              = V_STRUCTURE
      IS_LAYOUT                     = <LFS_LAYOUT>
      I_SAVE                        = V_SAVE
      IS_VARIANT                    = <LFS_VARIANT>
      I_DEFAULT                     = SPACE
    CHANGING
      IT_FIELDCATALOG               = IT_FIELDCAT[]
      IT_FILTER                     = <LFS_FILTER>
      IT_SORT                       = <LFS_SORT>
      IT_OUTTAB                     = <LFS_ITAB>
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "assign_itab_to_alv
*----------------------------------------------------------------------*
FORM SSSIGN_EVENT USING PV_GRID.
  DATA: LV_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

*--  Regist event for Edit
  CALL METHOD <LFS_GRID>->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.

  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR <LFS_GRID>.
  SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR FOR <LFS_GRID>.


ENDFORM.                    " sssign_event
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT  STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.

  DATA : LV_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO V_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check filed catalog:' P_FIELD.
    ENDIF.

    MOVE: V_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO LV_COL.
  ASSIGN (LV_COL) TO <FS>.
  MOVE   P_VALUE  TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    IF P_FIELDCAT-COL_POS IS INITIAL.
      ADD 1 TO V_CNT.
      P_FIELDCAT-COL_POS = V_CNT.
    ENDIF.
    APPEND P_FIELDCAT.
  ENDIF.

ENDFORM.                    "setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTE USING PV_GRID.
  PERFORM SET_LAYOUT                USING PV_GRID.
  PERFORM SET_VARIANT               USING PV_GRID.

ENDFORM.                    " SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT USING PV_GRID.
  DATA: LV_GRID(100),
        LV_LAYOUT(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'V_LAYOUT_' PV_GRID INTO LV_LAYOUT.
  ASSIGN:      (LV_LAYOUT)         TO   <LFS_LAYOUT>.

*  CALL METHOD <lfs_grid>->get_frontend_layout
*    IMPORTING
*      es_layout = <lfs_layout>.

*  IF <lfs_layout> IS INITIAL.
  <LFS_LAYOUT>-EDIT       = ' '.         " Edit Mode Enable
  <LFS_LAYOUT>-SEL_MODE   = 'A'.         " mode for select col and row
  <LFS_LAYOUT>-LANGUAGE   = SY-LANGU.    " Language Key
  <LFS_LAYOUT>-TOTALS_BEF = 'X'.        " Upper Total Line

*  <LFS_LAYOUT>-no_totline = 'X'.        " Disable Total Line
*    <lfs_layout>-cwidth_opt = 'X'.         " optimizes the column width
*  <LFS_LAYOUT>-no_merging = 'X'.        " Disable cell merging
  <LFS_LAYOUT>-ZEBRA      = 'X'.         " Emphasize C250
  <LFS_LAYOUT>-INFO_FNAME = 'LINECOLOR'. " Line color field
*  ENDIF.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = <LFS_LAYOUT>.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM SET_VARIANT USING PV_GRID.
  DATA: LV_GRID(100),
        LV_VARIANT(100).

  FIELD-SYMBOLS: <LFS_GRID>    TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_VARIANT> TYPE DISVARIANT.

  CONCATENATE: 'WC_GRID_' PV_GRID INTO LV_GRID.
  ASSIGN:      (LV_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'V_VARIANT_' PV_GRID INTO LV_VARIANT.
  ASSIGN:      (LV_VARIANT)         TO   <LFS_VARIANT>.

  CALL METHOD <LFS_GRID>->GET_VARIANT
    IMPORTING
      ES_VARIANT = <LFS_VARIANT>.

  IF <LFS_VARIANT> IS INITIAL.
    <LFS_VARIANT>-REPORT      = SY-REPID.
    <LFS_VARIANT>-HANDLE      = SPACE.
    <LFS_VARIANT>-LOG_GROUP   = SPACE.
    <LFS_VARIANT>-USERNAME    = SPACE.
    <LFS_VARIANT>-VARIANT     = SPACE.
    <LFS_VARIANT>-TEXT        = SPACE.
    <LFS_VARIANT>-DEPENDVARS  = SPACE.
  ENDIF.
ENDFORM.                    " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM HOTSPOT_CLICK  USING  PV_ROW_ID STRUCTURE LVC_S_ROW
                           PV_FIELD
                           PS_ROW_NO.

  CHECK PV_ROW_ID-INDEX >= 1.

  READ TABLE IT_ITAB INDEX PV_ROW_ID.
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M01.
    LEAVE TO SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM DBL_CLICK_0100 USING    P_COLUMN_FIELDNAME
                             P_ROW_NO_ROW_ID.

ENDFORM.                    " DBL_CLICK_0100
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
