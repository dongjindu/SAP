************************************************************************
* Program Name      : ZMMR_MATERIAL_MASTER_OVERVIEW
* Creation Date     : 12/2010
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZMMR_MATERIAL_MASTER_OVERVIEW NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TABLES:
  T134,                       "Material Types
  T001W,                      "Plant Data.
  MARA,                       "General Material Data.
  MARC,                       "Plant Data for material.
  MARD,                       "Storage Location Data for Material.
  MAKT,                       "Material Descriptions.
  MBEW,                       "Material Valuation.
  T460A,                      "Special procurement key.
  LFA1,                       "Vendor Master
*  MLGT,
  EORD,                       "Source List,
  STKO.

TYPE-POOLS SLIS.
* Constants.
CONSTANTS:
  CO_CLIENT_LEVEL  TYPE MARA-PSTAT  VALUE 'K',
  CO_PLANT_LEVEL   TYPE MARC-PSTAT  VALUE 'ABDEGLPQSV',
  CO_STATUS_SPACE  TYPE ICONSHORT   VALUE '@5F\QNot Relevant@',
  CO_STATUS_RED    TYPE ICONSHORT   VALUE '@5C\QNot yet processed@',
  CO_STATUS_YELLOW TYPE ICONSHORT   VALUE '@5D\QNot yet,no difficult@',
  CO_STATUS_GREEN  TYPE ICONSHORT   VALUE '@5B\QCompletely Processed@',
  CO_BASIC         TYPE T133A-AUSWG VALUE '07',   "K
  CO_SALES         TYPE T133A-AUSWG VALUE '09',   "V
  CO_PURCHASING    TYPE T133A-AUSWG VALUE '14',   "E
  CO_EXPORT        TYPE T133A-AUSWG VALUE '12',   "E
  CO_IMPORT        TYPE T133A-AUSWG VALUE '15',   "V
  CO_MRP1          TYPE T133A-AUSWG VALUE '26',   "D
  CO_MRP2          TYPE T133A-AUSWG VALUE '27',   "D
  CO_MRP3          TYPE T133A-AUSWG VALUE '28',   "D
  CO_MRP4          TYPE T133A-AUSWG VALUE '29',   "D
  CO_QUALITY       TYPE T133A-AUSWG VALUE '20',   "Q
  CO_ACCOUNTING1   TYPE T133A-AUSWG VALUE '21',   "B
  CO_ACCOUNTING2   TYPE T133A-AUSWG VALUE '22',   "B
  CO_COSTING1      TYPE T133A-AUSWG VALUE '34',   "G
  CO_COSTING2      TYPE T133A-AUSWG VALUE '35',   "G
  CO_STORAGE       TYPE T133A-AUSWG VALUE '18',   "L
  CO_STORAGE2      TYPE T133A-AUSWG VALUE '19',   "L
  CO_CLASS         TYPE T133A-AUSWG VALUE '23',   "C
  CO_FORECAST      TYPE T133A-AUSWG VALUE '30',   "P
  CO_TOOLS         TYPE T133A-AUSWG VALUE '17',   "F
  CO_WORK          TYPE T133A-AUSWG VALUE '31',   "A
  CO_WH            TYPE T133A-AUSWG VALUE '32',   "S
  CO_BACKGRND      TYPE I VALUE 0,
  CO_HEADING       TYPE I VALUE 1,
  CO_NORMAL        TYPE I VALUE 2,
  CO_TOTAL         TYPE I VALUE 3,
  CO_KEY           TYPE I VALUE 4,
  CO_POSITIVE      TYPE I VALUE 5,
  CO_NEGATIVE      TYPE I VALUE 6,
  CO_GROUP         TYPE I VALUE 7.

DATA: GT_COLOR  TYPE LVC_T_SCOL.
DATA: FIRST_ACT VALUE 'X',
      ACT_BUTT  VALUE 'X'.

DATA: X001W LIKE TABLE OF T001W WITH HEADER LINE,
      X159L LIKE TABLE OF T159L WITH HEADER LINE,
      X134  LIKE TABLE OF T134  WITH HEADER LINE,
      XLFA1 TYPE TABLE OF LFA1  WITH HEADER LINE,
      XMAST TYPE HASHED TABLE OF MAST
            WITH UNIQUE KEY MATNR WERKS STLAN STLNR STLAL
            WITH HEADER LINE,
      XMARD TYPE HASHED TABLE OF MARD
            WITH UNIQUE KEY MATNR WERKS LGORT
            WITH HEADER LINE,
      XEORD TYPE HASHED TABLE OF EORD
            WITH UNIQUE KEY MATNR WERKS ZEORD
            WITH HEADER LINE,
      XPKHD TYPE HASHED TABLE OF PKHD
*      Paul Change 07/06/11
*            WITH UNIQUE KEY MATNR WERKS PRVBE
            WITH UNIQUE KEY PKNUM MATNR WERKS PRVBE
*
            WITH HEADER LINE,
      XMLGN TYPE HASHED TABLE OF MLGN
            WITH UNIQUE KEY MATNR LGNUM
            WITH HEADER LINE.

DATA: BEGIN OF XINFO OCCURS 0,
      MATNR TYPE EINA-MATNR,
      ESOKZ TYPE EINE-ESOKZ,
      INFNR TYPE EINA-INFNR,
      LIFNR TYPE EINA-LIFNR,
      LOEKZ TYPE EINA-LOEKZ,
      PRDAT TYPE EINE-PRDAT,
      NETPR TYPE EINE-NETPR,
      BSTAE TYPE EINE-BSTAE,
      MWSKZ TYPE EINE-MWSKZ,
      WEBRE TYPE EINE-WEBRE,
      XERSN TYPE EINE-XERSN,
      END OF XINFO.
DATA: BEGIN OF XSA OCCURS 0,
      EBELN LIKE EKKO-EBELN,
      EBELP LIKE EKPO-EBELP,
      LIFNR LIKE EKKO-LIFNR,
      WERKS LIKE EKPO-WERKS,
      LGORT LIKE EKPO-LGORT,
      MATNR LIKE EKPO-MATNR,
      KTMNG LIKE EKPO-KTMNG,
      UEBTK LIKE EKPO-UEBTK,
      ABUEB LIKE EKPO-ABUEB,
      FABKZ LIKE EKPO-FABKZ,
      BSTAE LIKE EKPO-BSTAE,
      BSTYP LIKE EKKO-BSTYP,
      BSART LIKE EKKO-BSART,
      LOEKZ LIKE EKKO-LOEKZ,
      KDATB LIKE EKKO-KDATB,
      KDATE LIKE EKKO-KDATE,
      END OF XSA.

DATA: BEGIN OF XLIST OCCURS 0.
        INCLUDE STRUCTURE ZMMS_MASTER_OVERVIEW.
*DATA: KALKZ TYPE MBEW-KALKZ,
*      KALKL TYPE MBEW-KALKL,
*      KALKV TYPE MBEW-KALKV,
DATA: MARK,
      LIGHT,
*      COLTAB  TYPE LVC_T_SCOL,
      END OF XLIST.

DATA : BEGIN OF IT_MARC OCCURS 0,
         WERKS LIKE MARC-WERKS,
         MATNR LIKE MARC-MATNR,
         SOBSL LIKE MARC-SOBSL,
       END OF IT_MARC.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT       TYPE   I,
      W_TYPE(1),
      CHANGE_M(1).

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
*       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
*       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
*      it_color type LVC_T_SCOL,
*      wa_color like line of it_color,
      W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : ERROR_IN_DATA   TYPE LVC_S_STBL.

FIELD-SYMBOLS : <FS01>, <FS02>, <FS-QTY>.

** CLASS
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.

    METHODS : HANDLE_DATA_CHANGED
              FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
              IMPORTING ER_DATA_CHANGED.

    METHODS : HANDLE_HOTSPOT_CLICK
                 FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
                 IMPORTING E_ROW_ID
                           E_COLUMN_ID.
*
ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_DATA_CHANGED.
*    PERFORM HANDLE_DATA_CHANGED  USING ER_DATA_CHANGED.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-B01.
PARAMETERS : P_WERKS LIKE T001W-WERKS MEMORY ID WRK.
SELECT-OPTIONS:
*  S_WERKS FOR  T001W-WERKS MEMORY ID WRK NO INTERVALS,
  S_MATNR FOR  MARA-MATNR,
  S_MTART FOR  MARA-MTART  OBLIGATORY DEFAULT 'ROH',
*  so_ferth FOR  mara-ferth,
*  so_formt FOR  mara-formt,
  S_BKLAS FOR  MBEW-BKLAS,
  S_PROFL FOR MARA-PROFL,
*  so_labor FOR  mara-labor,
  S_MATKL FOR  MARA-MATKL,
*  so_kzkri FOR  marc-kzkri,
  S_BESKZ FOR  MARC-BESKZ,
*  so_sobsl FOR  marc-sobsl,
  S_ERSDA FOR  MARA-ERSDA,
  S_LAEDA FOR  MARA-LAEDA,
  S_LVORM FOR  MARC-LVORM,
*  so_fabkz FOR  marc-fabkz,
*  so_tempb FOR  mara-tempb,
  S_DISPO FOR  MARC-DISPO,
*  so_ekgrp FOR  marc-ekgrp,
*  so_aeszn FOR  mara-aeszn,
  S_MSTAE FOR  MARA-MSTAE,
  S_LIFNR FOR  EORD-LIFNR MODIF ID NOI,
**      Paul Change 07/06/11
  S_LGFSB FOR  MARC-LGFSB ,
  S_LGPRO FOR  MARC-LGPRO ,
  S_TEMPB FOR  MARA-TEMPB.
**
PARAMETERS: P_CHK AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B01.

*SELECTION-SCREEN BEGIN OF BLOCK bom WITH FRAME TITLE text-bom.
*PARAMETERS:
*  pa_bom   AS CHECKBOX,
*  pa_datuv LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK bom.

SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-LIS.
PARAMETERS:
  PA_ALL   RADIOBUTTON GROUP REND,
  PA_ERROR RADIOBUTTON GROUP REND,
  PA_NORM  RADIOBUTTON GROUP REND.
SELECTION-SCREEN END OF BLOCK BLK.
*
SELECTION-SCREEN BEGIN OF BLOCK BLKCD WITH FRAME TITLE TEXT-CDM.
PARAMETERS:
  PREF_D   RADIOBUTTON GROUP CD23,
  PREF_C   RADIOBUTTON GROUP CD23.
SELECTION-SCREEN END OF BLOCK BLKCD.

INITIALIZATION.
  PERFORM CHECK_TCODE_AUTHORITY USING SY-TCODE.
  PERFORM SET_DEFAULT_VALUE.


START-OF-SELECTION.
  IF PREF_C = 'X'.
    CHANGE_M = 'X'.
  ELSE.
    CLEAR: CHANGE_M.
  ENDIF.

  PERFORM PROCESS_DATA.
  IF XLIST[] IS INITIAL.
    EXIT.
  ENDIF.
  PERFORM DISPLAY_DATA.


*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE.

** added by furong check s_matnr
  IF S_MATNR-LOW IS INITIAL AND S_MATNR-HIGH IS INITIAL.
    S_MATNR-HIGH = 'ZZZZZZZ'.
  ELSEIF S_MATNR-HIGH IS INITIAL.
    S_MATNR-HIGH = S_MATNR-LOW.
  ENDIF.

  IF S_DISPO-LOW IS INITIAL AND S_DISPO-HIGH IS INITIAL.
    S_DISPO-HIGH = 'ZZZ'.
  ELSEIF S_DISPO-HIGH IS INITIAL.
    S_DISPO-HIGH = S_DISPO-LOW.
  ENDIF.
** end of addition
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  PERFORM GET_MATERIAL_MASTER.
  PERFORM READ_MATERIAL_TYPES.
  IF XLIST[] IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM READ_PLANT_DATA.
  PERFORM READ_BOM.
  PERFORM READ_DEFAULT_VALUES_FOR_IM.
  PERFORM READ_STORAGE_DATA .
  PERFORM READ_CONTROL_CYCLES.
  PERFORM READ_INFO_RECORDS.
  PERFORM READ_SA.
  PERFORM READ_SOURCE_LIST.
  PERFORM READ_MLGN.
  PERFORM CHECK_MATERIAL_STATUS.
  PERFORM FILTER_MATERIAL_STATUS.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  call_COGI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_COGI.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE XLIST INDEX LT_ROWS-INDEX.
  IF XLIST-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.

*  LOOP AT LT_ROWS.
*    READ TABLE IT_data INDEX LT_ROWS-INDEX.
*    IF NOT IT_data-MATNR IS INITIAL.
*      S_MATNR-SIGN = 'I'.
*      S_MATNR-OPTION = 'EQ'.
*      S_MATNR-LOW = IT_OUTPUT-MATNR.
*      APPEND S_MATNR.
*    ENDIF.
*    CLEAR: S_MATNR.
*  ENDLOOP.
*
*
*  SUBMIT CORUAFFW WITH S_WERKS EQ P_WERKS
*                  WITH S_MATNR IN S_MATNR
*                  AND RETURN.

ENDFORM.                    " call_COGI
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
  DATA: LS_STABLE TYPE LVC_S_STBL.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'XLIST'.
    PERFORM EVENT_HANDLER_REGISTER.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    LS_STABLE-ROW = 'X'.
    LS_STABLE-COL = 'X'.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
     EXPORTING
         IS_STABLE    = LS_STABLE.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA: L_COUNT(5),
        L_CT_F(5),
        L_CT_S(5).

  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  DESCRIBE TABLE XLIST LINES L_COUNT.
  LOOP AT XLIST.
    IF  XLIST-DOWNLOAD = 'S'.
      L_CT_S = L_CT_S + 1.
    ELSE.
      L_CT_F = L_CT_F + 1.
    ENDIF.
  ENDLOOP.
  CONDENSE L_COUNT.
  CONDENSE L_CT_S.
  CONDENSE L_CT_F.
  IF L_CT_S IS INITIAL.
    L_CT_S = '0'.
  ENDIF.
  IF L_CT_F IS INITIAL.
    L_CT_F = '0'.
  ENDIF.
  IF L_COUNT IS INITIAL.
    L_COUNT = '0'.
  ENDIF.

  CONCATENATE 'Total records: ' L_COUNT INTO WA_IS_LAYOUT-GRID_TITLE
   SEPARATED BY SPACE.
  MOVE: ' S = ' TO WA_IS_LAYOUT-GRID_TITLE+30(5),
        L_CT_S TO WA_IS_LAYOUT-GRID_TITLE+35(5),
        ' F = ' TO WA_IS_LAYOUT-GRID_TITLE+42(5),
        L_CT_F TO WA_IS_LAYOUT-GRID_TITLE+47(5).

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'MATNR'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = ' '.
  APPEND IT_SORT.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'DISPO'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.


  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                               'S' 'MATNR'       ' ',
                               ' ' 'KEY'         'X',
                               ' ' 'COLTEXT'     'Material',
                               ' ' 'HOTSPOT'     'X',
                               ' ' 'FIX_COLUMN'  'X',
                                  'E' 'OUTPUTLEN'   '18',

                               'S' 'MAKTX'       ' ',
                               ' ' 'COLTEXT'     'Material Description',
                               'E' 'OUTPUTLEN'   '25',

                                  'S' 'MATKL'       ' ',
                                  ' ' 'COLTEXT'     'Mat. Grp',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '9',

                               'S' 'MSTAE'       ' ',
                                ' ' 'COLTEXT'     'X-Pl',
                                  'E' 'OUTPUTLEN'   '5',

                                'S' 'BRGEW'       ' ',
                                  ' ' 'COLTEXT'     'Gross Weight',
*                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'NTGEW'       ' ',
                                 ' ' 'COLTEXT'     'Net Weight',
*                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'GEWEI'       ' ',
                                  ' ' 'COLTEXT'     'Unit',
                                  'E' 'OUTPUTLEN'   '5',


                                  'S' 'PROFL'       ' ',
                                  ' ' 'COLTEXT'     'LP/KD/MIP',
*                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '9',

                               'S' 'EKGRP'       ' ',
                                ' ' 'COLTEXT'     'Pur Grp',
                               ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '7',

                                'S' 'GEWEI'       ' ',
                                  ' ' 'COLTEXT'     'Unit',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'KORDB'       ' ',
                                  ' ' 'COLTEXT'     'Src list',
                                 ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'STAWN'       ' ',
                                  ' ' 'COLTEXT'     'Comm/Imp Code No.',
                                  'E' 'OUTPUTLEN'   '17',

                               'S' 'HERKL'       ' ',
                                ' ' 'COLTEXT'     'Country',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'DISPO'        ' ',
                                  ' ' 'COLTEXT'     'MRP',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'DISLS'        ' ',
                                  ' ' 'COLTEXT'     'Lot Sz',
                                   'E' 'OUTPUTLEN'   '6',

                                 'S' 'BSTRF'        ' ',
                                  ' ' 'COLTEXT'     'R Vaule',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'BESKZ'        ' ',
                                  ' ' 'COLTEXT'     'Proc T',
                                   'E' 'OUTPUTLEN'   '6',

*
*                                  'S' 'LABST'       ' ',
*                                  ' ' 'COLTEXT'     'Inventory',
*                                  ' ' 'DECIMALS_O'  '0',
*                                  ' ' 'NO_ZERO'     'X',
*                                  'E' 'OUTPUTLEN'   '13',
*

                                  'S' 'RGEKZ'         ' ',
                                  ' ' 'COLTEXT'     'B/F',
                                  'E' 'OUTPUTLEN'   '3',


                                 'S' 'FABKZ'        ' ',
                                  ' ' 'COLTEXT'     'JIT Del Sched',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'LGPRO'        ' ',
                                  ' ' 'COLTEXT'     'Iss Stor',
                                   'E' 'OUTPUTLEN'   '8',

                                  'S' 'VSPVB'         ' ',
                                  ' ' 'COLTEXT'     'Def. Suppl Area',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'LGFSB'        ' ',
                                  ' ' 'COLTEXT'     'Stor(EP)',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'MRPPP'        ' ',
                                  ' ' 'COLTEXT'     'Pl Calendar',
*                                   ' ' 'HOTSPOT'     'X',
                                   'E' 'OUTPUTLEN'   '8',

                                  'S' 'PLIFZ'       ' ',
                                  ' ' 'COLTEXT'     'Pl Del Time',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'LTKZA'        ' ',
                                  ' ' 'COLTEXT'     'S Removal',
                                   'E' 'OUTPUTLEN'   '10',

                                  'S' 'LTKZE'         ' ',
                                  ' ' 'COLTEXT'     'S Place',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'RAUBE'        ' ',
                                  ' ' 'COLTEXT'     'Shop',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'TEMPB'        ' ',
                                  ' ' 'COLTEXT'     'B/F Cyc',
                                   'E' 'OUTPUTLEN'   '8',

                                 'S' 'XMCNG'        ' ',
                                  ' ' 'COLTEXT'     'Neg Stock',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'BKLAS'        ' ',
                                  ' ' 'COLTEXT'     'Val Class',
                                  ' ' 'HOTSPOT'     'X',
                   'E' 'OUTPUTLEN'   '8',

                                  'S' 'VDATU'       ' ',
                                  ' ' 'COLTEXT'     'Valid From',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'BDATU'        ' ',
                                  ' ' 'COLTEXT'     'Valid To',
                                   'E' 'OUTPUTLEN'   '10',

                                  'S' 'VRTYP'         ' ',
                                  ' ' 'COLTEXT'     'Doc Cat',
                                  'E' 'OUTPUTLEN'   '6',

                                 'S' 'EBELN_SOURCE'        ' ',
                                  ' ' 'COLTEXT'     'SA No',
                                  ' ' 'HOTSPOT'     'X',

                    'E' 'OUTPUTLEN'   '10',

*                                  'S' 'EBELP'        ' ',
*                                  ' ' 'COLTEXT'     'SA Item',
*                                   'E' 'OUTPUTLEN'   '6',

                                 'S' 'FEBEL_SOURCE'        ' ',
                                  ' ' 'COLTEXT'     'Fix',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'AUTET'        ' ',
                                  ' ' 'COLTEXT'     'S Usage',
                                   'E' 'OUTPUTLEN'   '6',

                                  'S' 'LIFNR_SOURCE'       ' ',
                                  ' ' 'COLTEXT'     'Vendor(Src)',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '10',

                                'S' 'FLIFN_SOURCE'        ' ',
                                  ' ' 'COLTEXT'     'Fix V',
                                   'E' 'OUTPUTLEN'   '5',

                                  'S' 'LOEKZ_INFO'         ' ',
                                  ' ' 'COLTEXT'     'Del',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'LIFNR_INFO'        ' ',
                                  ' ' 'COLTEXT'     'Vendor(SA)',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PRDAT_INFO'        ' ',
                                  ' ' 'COLTEXT'     'Valid to',
                                   'E' 'OUTPUTLEN'   '10',


                                 'S' 'ESOKZ_INFO'        ' ',
                                  ' ' 'COLTEXT'     'item Cat',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'NETPR'        ' ',
                                  ' ' 'COLTEXT'     'Net Price',
                                   'E' 'OUTPUTLEN'   '13',

                                  'S' 'BSTAE_INFO'       ' ',
                                  ' ' 'COLTEXT'     'Conf Ctl',
                                  'E' 'OUTPUTLEN'   '8',

                                'S' 'MWSKZ_INFO'        ' ',
                                  ' ' 'COLTEXT'     'Tax',
                                   'E' 'OUTPUTLEN'   '3',

                                  'S' 'WEBRE_INFO'         ' ',
                                  ' ' 'COLTEXT'     'GR-b Inv',
                                  'E' 'OUTPUTLEN'   '6',

                                 'S' 'XERSN_INFO'        ' ',
                                  ' ' 'COLTEXT'     'ERS',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'KTMNG_SA'        ' ',
                                  ' ' 'COLTEXT'     'Target QTY',
                                   'E' 'OUTPUTLEN'   '13',


                                'S' 'UEBTK_SA'        ' ',
                                  ' ' 'COLTEXT'     'O Del',
                                   'E' 'OUTPUTLEN'   '5',

                                  'S' 'ABUEB_SA'         ' ',
                                  ' ' 'COLTEXT'     'Cr profile',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'FABKZ_SA'        ' ',
                                  ' ' 'COLTEXT'     'JIT',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'BSTAE_SA'        ' ',
                                  ' ' 'COLTEXT'     'Conf Ctl',
                                   'E' 'OUTPUTLEN'   '13',

                             'S' 'BSTYP_SA'        ' ',
                                  ' ' 'COLTEXT'     'Doc Cat',
                                   'E' 'OUTPUTLEN'   '7',

                                  'S' 'BSART_SA'         ' ',
                                  ' ' 'COLTEXT'     'Doc Type',
                                  'E' 'OUTPUTLEN'   '5',

**                                 'S' 'LOEKZ_SA'        ' ',
**                                  ' ' 'COLTEXT'     'Del',
**                                  'E' 'OUTPUTLEN'   '3',
**
**                                  'S' 'LGNUM'        ' ',
**                                  ' ' 'COLTEXT'     'Warehouse',
**                                   'E' 'OUTPUTLEN'   '4',
**
**
**                             'S' 'LGTYP'        ' ',
**                                  ' ' 'COLTEXT'     'Stor Type',
**                                   'E' 'OUTPUTLEN'   '8',
**
**                                  'S' 'LGPLA'         ' ',
**                                  ' ' 'COLTEXT'     'Stor Bin',
**                                  'E' 'OUTPUTLEN'   '5',
**
                                 'S' 'STLAN'        ' ',
                                  ' ' 'COLTEXT'     'BOM Usage',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STLNR'        ' ',
                                  ' ' 'COLTEXT'     'BOM No',
                                   'E' 'OUTPUTLEN'   '10',

                             'S' 'STATUS_BASIC'        ' ',
                                  ' ' 'COLTEXT'     'Basic',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_PLANT_ST1'         ' ',
                                  ' ' 'COLTEXT'     'Pl/1',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '5',


                                  'S' 'STATUS_PLANT_ST2'         ' ',
                                  ' ' 'COLTEXT'     'Pl/2',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_PURCHASE'         ' ',
                                  ' ' 'COLTEXT'     'Purchase',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_IMPORT'         ' ',
                                  ' ' 'COLTEXT'     'Import',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_MRP1'         ' ',
                                  ' ' 'COLTEXT'     'MRP1',
                                   ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_MRP2'         ' ',
                                  ' ' 'COLTEXT'     'MRP2',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_MRP4'         ' ',
                                  ' ' 'COLTEXT'     'MRP4',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',
*
*                                  'S' 'STATUS_WH'         ' ',
*                                  ' ' 'COLTEXT'     'WH',
*                                  ' ' 'HOTSPOT'     'X',
*                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_ACCOUNT'         ' ',
                                  ' ' 'COLTEXT'     'Account',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_COST'         ' ',
                                  ' ' 'COLTEXT'     'Cost',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_SOURCE'         ' ',
                                  ' ' 'COLTEXT'     'Src L',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_INFOREC'         ' ',
                                  ' ' 'COLTEXT'     'InfoRec',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'STATUS_SA'         ' ',
                                  ' ' 'COLTEXT'     'SA',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'STATUS_CC'         ' ',
                                  ' ' 'COLTEXT'     'C/C',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',


                                 'S' 'STATUS_BOM'         ' ',
                                  ' ' 'COLTEXT'     'BOM',
                                  ' ' 'HOTSPOT'     'X',
                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'DOWNLOAD'   ' ',
                                  ' ' 'COLTEXT'     'DW',
                  'E' 'OUTPUTLEN'   '1'.

ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = XLIST[].
*               IT_SORT          = IT_SORT[].

ENDFORM.                    " assign_itab_to_alv


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      PERFORM PROCESS_DATA.
    WHEN 'EXCEL'.
      PERFORM DOWNLOAD.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  call_MM02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_MM02.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE XLIST INDEX LT_ROWS-INDEX.
  IF XLIST-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD XLIST-MATNR.
  CALL TRANSACTION 'MM02' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " call_MM02
*&---------------------------------------------------------------------*
*&      Form  call_MM03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_MM03.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE XLIST INDEX LT_ROWS-INDEX.
  IF XLIST-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD XLIST-MATNR.
  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
ENDFORM.                                                    " call_MM03
*&---------------------------------------------------------------------*
*&      Form  call_ME01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ME01.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
         LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE XLIST INDEX LT_ROWS-INDEX.
  IF XLIST-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD XLIST-MATNR.
  SET PARAMETER ID 'WRK' FIELD XLIST-WERKS.
  CALL TRANSACTION 'ME01' AND SKIP FIRST SCREEN.

ENDFORM.                                                    " call_ME01
**&---------------------------------------------------------------------
*
*&      Form  CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*       Check TCode Authority.
*----------------------------------------------------------------------*
FORM CHECK_TCODE_AUTHORITY USING U_TCODE .
* Authority Check
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
       EXPORTING
            TCODE  = U_TCODE
       EXCEPTIONS
            OK     = 0
            NOT_OK = 1
            OTHERS = 2.
  IF SY-SUBRC NE 0.
    MESSAGE E172(00) WITH U_TCODE RAISING NO_AUTHORITY.
  ENDIF.
ENDFORM.                    " CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*&      Form  SET_DEFAULT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DEFAULT_VALUE.

ENDFORM.                    " SET_DEFAULT_VALUE
*&---------------------------------------------------------------------*
*&      Form  get_master_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MATERIAL_MASTER.
  REFRESH: XLIST.
  IF S_LIFNR[] IS INITIAL.
    SELECT C~MATNR C~WERKS K~MAKTX M~MTART M~MATKL M~MSTAE
           MTPOS_MARA M~BRGEW
           M~NTGEW M~GEWEI M~PROFL C~EKGRP  C~KORDB   "MARD_KORDB
           C~STAWN C~HERKL C~DISPO C~DISLS C~BSTRF C~BESKZ
           C~RGEKZ C~FABKZ C~LGPRO C~VSPVB C~LGFSB C~MRPPP
           C~PLIFZ W~BKLAS  C~PRCTR C~NCOST KORDB  M~VPSTA
            M~PSTAT
           M~BISMT M~LABOR M~MSTAE M~RMATP
           M~BRGEW  M~MBRSH M~ZEINR
           M~LAEDA  M~TEMPB M~RAUBE
           M~FERTH M~FORMT M~NORMT M~AESZN
           C~KZKRI
           C~EKGRP C~KAUTB C~KORDB C~WEBAZ
           C~FABKZ C~DISGR C~MAABC C~DISMM
           C~MINBE C~MRPPP C~EPRIO AS INSLC
           C~DISLS C~BSTRF C~BESKZ C~SOBSL
           C~LGPRO C~LGFSB C~PLIFZ C~FHORI
           C~SHZET C~EISBE C~XMCNG C~RDPRF C~SHFLG
           C~MTVFP C~SAUFT C~SBDKZ C~KZBED C~LOGGR
           C~PRCTR M~ERSDA
           C~LVORM W~KALKZ W~KALKL W~KALKV
      INTO CORRESPONDING FIELDS OF TABLE XLIST
      FROM MARC AS C LEFT JOIN MBEW AS W
                            ON W~MATNR = C~MATNR AND
                               W~BWKEY = C~WERKS
                    INNER JOIN MARA AS M
                            ON C~MATNR = M~MATNR
                    INNER JOIN MAKT AS K
                            ON M~MATNR = K~MATNR AND
                               K~SPRAS = SY-LANGU
     WHERE C~WERKS EQ P_WERKS
       AND C~MATNR IN S_MATNR
       AND M~MTART IN S_MTART
       AND M~MATKL IN S_MATKL
       AND M~ERSDA IN S_ERSDA
       AND M~PROFL IN S_PROFL
       AND M~LAEDA IN S_LAEDA
**Paul insert Control Type 07/06/11
       AND M~TEMPB IN S_TEMPB
       AND C~LGFSB IN S_LGFSB
       AND C~LGPRO IN S_LGPRO
**
       AND C~BESKZ IN S_BESKZ
       AND C~LVORM IN S_LVORM
       AND C~DISPO IN S_DISPO
       AND M~MSTAE IN S_MSTAE.
  ELSE.
    SELECT C~MATNR C~WERKS K~MAKTX M~MTART M~MATKL M~MSTAE
          MTPOS_MARA M~BRGEW
          M~NTGEW M~GEWEI M~PROFL C~EKGRP  C~KORDB   "MARD_KORDB
          C~STAWN C~HERKL C~DISPO C~DISLS C~BSTRF C~BESKZ
          C~RGEKZ C~FABKZ C~LGPRO C~VSPVB C~LGFSB C~MRPPP
          C~PLIFZ W~BKLAS  C~PRCTR C~NCOST KORDB  M~VPSTA
           M~PSTAT
          M~BISMT M~LABOR M~MSTAE M~RMATP
          M~BRGEW  M~MBRSH M~ZEINR
          M~LAEDA  M~TEMPB M~RAUBE
          M~FERTH M~FORMT M~NORMT M~AESZN
          C~KZKRI
          C~EKGRP C~KAUTB C~KORDB C~WEBAZ
          C~FABKZ C~DISGR C~MAABC C~DISMM
          C~MINBE C~MRPPP C~EPRIO AS INSLC
          C~DISLS C~BSTRF C~BESKZ C~SOBSL
          C~LGPRO C~LGFSB C~PLIFZ C~FHORI
          C~SHZET C~EISBE C~XMCNG C~RDPRF C~SHFLG
          C~MTVFP C~SAUFT C~SBDKZ C~KZBED C~LOGGR
          C~PRCTR M~ERSDA
          C~LVORM W~KALKZ W~KALKL W~KALKV
       INTO CORRESPONDING FIELDS OF TABLE XLIST
       FROM MARC AS C LEFT JOIN MBEW AS W
                             ON W~MATNR = C~MATNR AND
                                W~BWKEY = C~WERKS
                     INNER JOIN MARA AS M
                             ON C~MATNR = M~MATNR
                     INNER JOIN MAKT AS K
                             ON M~MATNR = K~MATNR AND
                                K~SPRAS = SY-LANGU
       WHERE C~WERKS EQ P_WERKS
        AND C~MATNR IN S_MATNR
        AND M~MTART IN S_MTART
        AND M~MATKL IN S_MATKL
        AND M~ERSDA IN S_ERSDA
        AND M~LAEDA IN S_LAEDA
        AND M~PROFL IN S_PROFL
**Paul insert Control Type 07/06/11
        AND M~TEMPB IN S_TEMPB
        AND C~LGFSB IN S_LGFSB
        AND C~LGPRO IN S_LGPRO
**
        AND C~BESKZ IN S_BESKZ
        AND C~LVORM IN S_LVORM
        AND C~DISPO IN S_DISPO
        AND M~MSTAE IN S_MSTAE
        AND EXISTS ( SELECT *
                       FROM EORD AS D
                   WHERE D~MATNR  EQ C~MATNR
                     AND D~WERKS  EQ C~WERKS
                     AND D~LIFNR  IN S_LIFNR
                     AND D~VDATU  <= SY-DATUM
                     AND D~BDATU  >= SY-DATUM
                     AND D~AUTET  NE SPACE ).
  ENDIF.

  DELETE XLIST WHERE NOT BKLAS IN S_BKLAS.

ENDFORM.                    " get_master_master
*&---------------------------------------------------------------------*
*&      Form  read_material_types
*&---------------------------------------------------------------------*
*       Read Material Types.
*----------------------------------------------------------------------*
FORM READ_MATERIAL_TYPES.
  REFRESH: X134.
  REFRESH X134.
  SELECT *
    INTO TABLE X134
    FROM T134
   WHERE MTART IN S_MTART.
ENDFORM.                    " read_material_types
*&---------------------------------------------------------------------*
*&      Form  GET_MTART_PSTAT
*&---------------------------------------------------------------------*
*       Get Maintenance Status for Material Type.
*----------------------------------------------------------------------*
*      -->I_MTART  Material Type.
*      <--E_PSTAT  Maintenance Status.
*----------------------------------------------------------------------*
FORM GET_MTART_PSTAT USING    I_MTART
                     CHANGING E_PSTAT.
  CLEAR E_PSTAT.
  READ TABLE X134 WITH KEY MTART = I_MTART
                  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    E_PSTAT = X134-PSTAT.
  ENDIF.
ENDFORM.                    " get_mtart_pstat
*&---------------------------------------------------------------------*
*&      Form  READ_DEFAULT_VALUES_FOR_IM
*&---------------------------------------------------------------------*
*       Read Default values for Inventory Management.
*----------------------------------------------------------------------*
FORM READ_DEFAULT_VALUES_FOR_IM .
  REFRESH: X159L.
  SELECT *
    INTO TABLE X159L
    FROM T159L
   WHERE WERKS EQ P_WERKS.
ENDFORM.                    " READ_DEFAULT_VALUES_FOR_IM
*&---------------------------------------------------------------------*
*&      Form  READ_PLANT_DATA
*&---------------------------------------------------------------------*
*       Read Plant Data.
*----------------------------------------------------------------------*
FORM READ_PLANT_DATA .
  SELECT *
    INTO TABLE X001W
    FROM T001W
   WHERE WERKS EQ P_WERKS.
ENDFORM.                    " READ_PLANT_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_CONTROL_CYCLES
*&---------------------------------------------------------------------*
*       Read Control Cycles.
*----------------------------------------------------------------------*
FORM READ_CONTROL_CYCLES.
  REFRESH XPKHD.
  SELECT *
    INTO TABLE XPKHD
    FROM PKHD AS P
   WHERE P~WERKS EQ P_WERKS
     AND P~MATNR IN S_MATNR
     AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ P~MATNR
                     AND C~WERKS EQ P~WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).
ENDFORM.                    " READ_CONTROL_CYCLES
*&---------------------------------------------------------------------*
*&      Form  READ_STORAGE_DATA
*&---------------------------------------------------------------------*
*       Read Storage Data
*----------------------------------------------------------------------*

FORM READ_STORAGE_DATA .
  REFRESH XMARD.
  SELECT *
    INTO TABLE XMARD
    FROM MARD AS D
   WHERE D~WERKS EQ P_WERKS
     AND D~MATNR IN S_MATNR
       AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ D~MATNR
                     AND C~WERKS EQ P_WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).

ENDFORM.                    " READ_STORAGE_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_INFO_RECORDS
*&---------------------------------------------------------------------*
*       Read Info.Records
*----------------------------------------------------------------------*
FORM READ_INFO_RECORDS.
  REFRESH XINFO.
  SELECT A~MATNR E~ESOKZ A~INFNR A~LIFNR
         A~LOEKZ E~PRDAT E~NETPR E~BSTAE
         E~MWSKZ E~WEBRE XERSN
    INTO CORRESPONDING FIELDS OF TABLE XINFO
    FROM EINA AS A INNER JOIN EINE AS E
      ON A~INFNR = E~INFNR
   WHERE A~MATNR IN S_MATNR
*     AND A~LOEKZ EQ SPACE
*     AND E~LOEKZ EQ SPACE
     AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ A~MATNR
                     AND C~WERKS EQ P_WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).

ENDFORM.                    " READ_INFO_RECORDS
*&---------------------------------------------------------------------*
*&      Form  READ_SOURCE_LIST
*&---------------------------------------------------------------------*
*       Read Source List
*----------------------------------------------------------------------*
FORM READ_SOURCE_LIST .
  REFRESH XEORD.
  SELECT *
    INTO TABLE XEORD
    FROM EORD AS E
   WHERE E~MATNR IN S_MATNR
     AND E~WERKS EQ P_WERKS
     AND E~VDATU <= SY-DATUM
     AND E~BDATU >= SY-DATUM
     AND E~AUTET IN ('1','2')
     AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ E~MATNR
                     AND C~WERKS EQ E~WERKS
                     AND C~WERKS EQ P_WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).

ENDFORM.                    " READ_SOURCE_LIST
*&---------------------------------------------------------------------*
*&      Form  read_mlgn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MLGN.
  REFRESH XMLGN.
  SELECT *
    INTO TABLE XMLGN
    FROM MLGN AS P
   WHERE MATNR IN S_MATNR
     AND LGNUM = 'P01'
     AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ P~MATNR
                     AND C~WERKS EQ P_WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).

ENDFORM.                    " read_mlgn

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_STATUS
*&---------------------------------------------------------------------*
*       Check Material Status.
*----------------------------------------------------------------------*
FORM CHECK_MATERIAL_STATUS.
  DATA: LV_LEN    TYPE I,
        LV_POS    TYPE I,
        LV_VIEW,
        L_INDEX LIKE SY-TABIX.

  DATA: LV_MTART_VIEW TYPE T134-PSTAT.
  DATA: BEGIN OF LT_MDSM OCCURS 0,
        MATNR LIKE MARA-MATNR,
        END OF LT_MDSM.

  IF P_CHK = 'X'.
    SELECT MATNR INTO TABLE LT_MDSM
      FROM MDSM
      FOR ALL ENTRIES IN XLIST
      WHERE PLSCN = '900'
       AND MATNR = XLIST-MATNR
       AND WERKS = XLIST-WERKS.
    LOOP AT XLIST.
      READ TABLE LT_MDSM WITH KEY MATNR = XLIST-MATNR.
      IF SY-SUBRC = 0.
      ELSE.
        DELETE XLIST.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT XLIST.
    L_INDEX = SY-TABIX.
    CLEAR: W_TYPE.
    CASE XLIST-DISPO.
      WHEN 'M01'.
        W_TYPE = 'M'.
      WHEN OTHERS.
        CASE XLIST-BKLAS.
          WHEN '3001'.
            W_TYPE = 'T'.
          WHEN '3005'.
            W_TYPE = 'S'.
          WHEN '3000'.
            W_TYPE = 'C'.
        ENDCASE.
    ENDCASE.

    PERFORM GET_MTART_PSTAT USING    XLIST-MTART
                            CHANGING LV_MTART_VIEW.
    LV_LEN = STRLEN( LV_MTART_VIEW ).
    CLEAR LV_POS.
    DO.
      IF LV_LEN = LV_POS. EXIT. ENDIF.
      LV_POS = SY-INDEX - 1.
      LV_VIEW = LV_MTART_VIEW+LV_POS(1).
      IF LV_VIEW CA XLIST-VPSTA.             "Maintenance status.
        IF LV_VIEW CA CO_CLIENT_LEVEL.
          PERFORM UPDATE_MAINTAINED_VIEW USING LV_VIEW.
        ELSE.
          IF LV_VIEW CA CO_PLANT_LEVEL AND
             LV_VIEW CA XLIST-PSTAT.
            PERFORM UPDATE_MAINTAINED_VIEW USING LV_VIEW.
          ELSE.
            PERFORM UPDATE_OMITTED_VIEW USING LV_VIEW.
          ENDIF.
        ENDIF.
      ELSE.
        PERFORM UPDATE_OMITTED_VIEW USING LV_VIEW.
      ENDIF.
    ENDDO.

    PERFORM CHECK_BASIC_VIEW USING W_TYPE.

    PERFORM CHECK_PURCHASE_VIEW USING W_TYPE.

    IF W_TYPE = 'C'.
      PERFORM CHECK_FOREIGN_TRADE.
    ELSE.
      XLIST-STATUS_IMPORT = CO_STATUS_SPACE.
    ENDIF.

    PERFORM CHECK_MRP1_VIEW USING W_TYPE.
    PERFORM CHECK_MRP2_VIEW USING W_TYPE.
    PERFORM CHECK_MRP4_VIEW USING W_TYPE.

    PERFORM CHECK_PLANT_ST1_VIEW USING W_TYPE.
    PERFORM CHECK_PLANT_ST2_VIEW USING W_TYPE.

    IF W_TYPE = 'T' OR W_TYPE = 'C'.
      PERFORM CHECK_WAREHOUSE_VIEW USING W_TYPE.
    ENDIF.

    PERFORM CHECK_ACCOUNT_VIEW USING W_TYPE.

    PERFORM CHECK_COST_VIEW USING W_TYPE.

    PERFORM CHECK_SOURCE_LIST USING W_TYPE.

    PERFORM CHECK_INFORECO USING W_TYPE.

    IF W_TYPE = 'C'.
      XLIST-STATUS_CC = CO_STATUS_SPACE.
    ELSE.
      PERFORM CHECK_SA USING W_TYPE.
    ENDIF.

    IF W_TYPE = 'T' OR W_TYPE = 'C'.
      PERFORM CHECK_CONTROL_CYCLE USING W_TYPE.
    ELSE.
      XLIST-STATUS_CC = CO_STATUS_SPACE.
    ENDIF.

    IF W_TYPE = 'M'.
      PERFORM CHECK_BOM USING W_TYPE.
    ELSE.
      XLIST-STATUS_BOM = CO_STATUS_SPACE.
    ENDIF.
*    PERFORM CHECK_SPECIFIC_DATA.
*
    PERFORM UPDATE_ALL_VIEW_STATUS.

    MODIFY XLIST INDEX L_INDEX.
  ENDLOOP.
ENDFORM.                    " CHECK_MATERIAL_STATUS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MAINTAINED_VIEW
*&---------------------------------------------------------------------*
*       Update Maintained View.
*----------------------------------------------------------------------*
*      -->I_VIEW  View.
*----------------------------------------------------------------------*
FORM UPDATE_MAINTAINED_VIEW USING    I_VIEW.
  DATA : L_LAND1 TYPE LFA1-LAND1,
         L_CNT   TYPE I,
         L_LIFNR TYPE LFA1-LIFNR.

  CASE I_VIEW.
*    WHEN 'A'.   "Work Scheduling
*      XLIST-STATUS_WORK     = CO_STATUS_GREEN.
    WHEN 'B'.   "Accounting
      XLIST-STATUS_ACCOUNT  = CO_STATUS_GREEN.
*    WHEN 'C'.   "Classification
*      XLIST-STATUS_CLASS    = CO_STATUS_GREEN.
    WHEN 'D'.   "MRP
      XLIST-STATUS_MRP1     = CO_STATUS_GREEN.
      XLIST-STATUS_MRP2     = CO_STATUS_GREEN.
      XLIST-STATUS_MRP4     = CO_STATUS_GREEN.
    WHEN 'E'.   "Purchasing
      XLIST-STATUS_PURCHASE = CO_STATUS_GREEN.
*    WHEN 'F'.   "Production resources/tools
*      XLIST-STATUS_TOOL     = CO_STATUS_GREEN.
    WHEN 'G'.   "Costing
*      IF XLIST-PRCTR IS INITIAL.
*        XLIST-STATUS_COST = CO_STATUS_RED.
*      ELSE.
      XLIST-STATUS_COST = CO_STATUS_GREEN.
*      ENDIF.
    WHEN 'K'.   "Basic
      XLIST-STATUS_BASIC    = CO_STATUS_GREEN.
    WHEN 'L'.   "Storage
      XLIST-STATUS_PLANT_ST1     = CO_STATUS_GREEN.
      XLIST-STATUS_PLANT_ST1     = CO_STATUS_GREEN.
*    WHEN 'P'.   "Forecasting
*      XLIST-STATUS_FORECAST = CO_STATUS_GREEN.
*    WHEN 'Q'.   "Quality Management
*      XLIST-STATUS_QM       = CO_STATUS_GREEN.
*    WHEN 'V'.   "Sales
*      XLIST-STATUS_SALES    = CO_STATUS_GREEN.
    WHEN 'S'.   "Warehouse
      IF W_TYPE = 'T' OR W_TYPE = 'C'.
        XLIST-STATUS_WH    = CO_STATUS_GREEN.
      ELSE.
        XLIST-STATUS_WH    = CO_STATUS_SPACE.
      ENDIF.
  ENDCASE.
ENDFORM.                    " UPDATE_MAINTAINED_VIEW
*&---------------------------------------------------------------------*
*&      Form  UPDATE_OMITTED_VIEW
*&---------------------------------------------------------------------*
*       Update Omitted View.
*----------------------------------------------------------------------*
*      -->I_VIEW  View.
*----------------------------------------------------------------------*
FORM UPDATE_OMITTED_VIEW USING    I_VIEW.
  CASE I_VIEW.
*    WHEN 'A'.   "Work Scheduling
*      XLIST-STATUS_WORK     = CO_STATUS_YELLOW.
    WHEN 'B'.   "Accounting
*      IF XLIST-SOBSL(1) EQ '5'.
*        XLIST-STATUS_ACCOUNT  = CO_STATUS_YELLOW.
*      ELSE.
      XLIST-STATUS_ACCOUNT  = CO_STATUS_RED.
*      ENDIF.
*    WHEN 'C'.   "Classification
*      XLIST-STATUS_CLASS    = CO_STATUS_YELLOW.
    WHEN 'D'.   "MRP
*      IF XLIST-SOBSL(1) EQ '5'.
*        XLIST-STATUS_MRP1     = CO_STATUS_YELLOW.
*        XLIST-STATUS_MRP2     = CO_STATUS_YELLOW.
*        XLIST-STATUS_MRP3     = CO_STATUS_YELLOW.
*        XLIST-STATUS_MRP4     = CO_STATUS_YELLOW.
*      ELSE.
      XLIST-STATUS_MRP1     = CO_STATUS_RED.
      XLIST-STATUS_MRP2     = CO_STATUS_RED.
*      XLIST-STATUS_MRP3     = CO_STATUS_RED.
      XLIST-STATUS_MRP4     = CO_STATUS_RED.
*      ENDIF.
    WHEN 'E'.   "Purchasing
      XLIST-STATUS_PURCHASE = CO_STATUS_RED.
*      IF XLIST-MTART EQ 'SEMI'.
*        XLIST-STATUS_PURCHASE = CO_STATUS_GREEN.
*      ELSE.
*        CASE XLIST-BESKZ.
*          WHEN 'F'.
*            IF XLIST-SOBSL(1) CA '5'.
*              XLIST-STATUS_PURCHASE = CO_STATUS_YELLOW.
*            ELSE.
*              XLIST-STATUS_PURCHASE = CO_STATUS_RED.
*            ENDIF.
*          WHEN 'X'.
*            CASE XLIST-SOBSL(1).
*              WHEN '1' OR '2' OR '3' OR '4'.
*                XLIST-STATUS_PURCHASE = CO_STATUS_RED.
*              WHEN OTHERS.
*                XLIST-STATUS_PURCHASE = CO_STATUS_YELLOW.
*            ENDCASE.
*          WHEN 'E'.
*            XLIST-STATUS_PURCHASE = CO_STATUS_SPACE.
*          WHEN OTHERS.
*            XLIST-STATUS_PURCHASE = CO_STATUS_YELLOW.
*        ENDCASE.
*      ENDIF.
*    WHEN 'F'.   "Production resources/tools
*      XLIST-STATUS_TOOL     = CO_STATUS_YELLOW.
    WHEN 'G'.   "Costing
      XLIST-STATUS_COST     = CO_STATUS_RED.
    WHEN 'K'.   "Basic
      XLIST-STATUS_BASIC    = CO_STATUS_RED.
    WHEN 'L'.   "Storage
      XLIST-STATUS_PLANT_ST1  = CO_STATUS_SPACE.
      XLIST-STATUS_PLANT_ST1  = CO_STATUS_SPACE.
    WHEN 'S'.   "Warehouse
      IF W_TYPE = 'T' OR W_TYPE = 'C'.
        XLIST-STATUS_WH    = CO_STATUS_RED.
      ELSE.
        XLIST-STATUS_WH    = CO_STATUS_SPACE.
      ENDIF.

*    WHEN 'P'.   "Forecasting
*      XLIST-STATUS_FORECAST = CO_STATUS_YELLOW.
*    WHEN 'Q'.   "Quality Management
*      XLIST-STATUS_QM       = CO_STATUS_YELLOW.
*    WHEN 'V'.   "Sales
*      CASE XLIST-MTART.
*        WHEN 'FSC'.
*          XLIST-STATUS_SALES    = CO_STATUS_RED.
*        WHEN OTHERS.
*          IF XLIST-BEIKZ EQ 'C'.
*            XLIST-STATUS_SALES    = CO_STATUS_RED.
*          ELSE.
*            XLIST-STATUS_SALES    = CO_STATUS_YELLOW.
*          ENDIF.
*      ENDCASE.
  ENDCASE.
ENDFORM.                    " UPDATE_OMITTED_VIEW
*&---------------------------------------------------------------------*
*&      Form  check_basic_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BASIC_VIEW USING P_TYPE.
  CASE P_TYPE.
    WHEN 'T' OR 'S' OR 'M'.
      IF XLIST-MATKL CS 'LP'.
      ELSE.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-MSTAE = '11'.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF NOT XLIST-MTPOS_MARA IS INITIAL.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-PROFL <> 'V'.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
    WHEN 'C'.
      IF XLIST-MATKL CS 'KD'.
      ELSE.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-MSTAE = '11'.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF NOT XLIST-MTPOS_MARA IS INITIAL.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-BRGEW IS INITIAL.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-NTGEW IS INITIAL.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-GEWEI <> 'KG'.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
      IF XLIST-PROFL <> 'K'.
        XLIST-STATUS_BASIC    = CO_STATUS_RED.
      ENDIF.
  ENDCASE.
ENDFORM.                    " check_basic_view
*&---------------------------------------------------------------------*
*&      Form  check_purchase_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_PURCHASE_VIEW USING P_TYPE.

  IF XLIST-EKGRP IS INITIAL.
    XLIST-STATUS_PURCHASE = CO_STATUS_RED.
  ENDIF.
  IF XLIST-KORDB IS INITIAL.
    XLIST-STATUS_PURCHASE = CO_STATUS_RED.
  ENDIF.
ENDFORM.                    " check_purchase_view
*&---------------------------------------------------------------------*
*&      Form  check_Foreign_Trade
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_FOREIGN_TRADE.
  XLIST-STATUS_IMPORT  = CO_STATUS_GREEN.
  IF XLIST-STAWN IS INITIAL.
    XLIST-STATUS_IMPORT    = CO_STATUS_RED.
  ENDIF.
  IF XLIST-HERKL IS INITIAL.
    XLIST-STATUS_IMPORT    = CO_STATUS_RED.
  ENDIF.
ENDFORM.                    " check_Foreign_Trade
*&---------------------------------------------------------------------*
*&      Form  check_MRP_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_MRP1_VIEW USING P_TYPE.
  CASE P_TYPE.
    WHEN 'T'.
      IF XLIST-DISPO = 'C01' OR XLIST-DISPO = '001' OR
         XLIST-DISPO = '   '.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISMM <> 'PD'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISLS IS INITIAL.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
    WHEN 'S'.
      IF XLIST-DISPO = 'C01' OR XLIST-DISPO = '001' OR
         XLIST-DISPO = '   '.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISMM <> 'PD'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISLS <> 'PK'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.

    WHEN 'M'.
      IF XLIST-DISPO <> 'M01'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISMM <> 'PD'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISLS <> 'PK'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.

    WHEN 'C'.
      IF XLIST-DISPO = 'C01' OR XLIST-DISPO = '001' OR
         XLIST-DISPO = '   '.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISMM <> 'PD'.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-DISLS IS INITIAL.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-BSTRF IS INITIAL.
        XLIST-STATUS_MRP1 = CO_STATUS_RED.
      ENDIF.
  ENDCASE.
ENDFORM.                    " check_MRP_view
*&---------------------------------------------------------------------*
*&      Form  check_MRP2_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_MRP2_VIEW USING P_TYPE.
  DATA: L_PRVBE LIKE PVBE-PRVBE.

  RANGES: S_VSPVB FOR MARC-VSPVB.

  S_VSPVB-OPTION = 'EQ'.
  S_VSPVB-SIGN = 'I'.
  S_VSPVB-LOW = 'B0'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'B1'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'P1'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'P2'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'P3'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'PBI'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'PBO'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'T1'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'T1S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'T2'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'T2S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'T3'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'T3S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'C1'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'C1S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'ENG'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'C2'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'C2S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F1'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F1S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F2'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F2S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F3'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F3S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F4'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F4S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F5'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'F5S'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'OK'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'SOFF'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'VLCBULK'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'BASRS260'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'BASRS500'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'BBULK010'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'BBULK250'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'BBULK300'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'BBULK310'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'PBULK010'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'PBULK300'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'PBULK310'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'TBULK010'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'TBULK300'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'TBULK310'.
  APPEND S_VSPVB.
  S_VSPVB-LOW = 'TBULK450'.
  APPEND S_VSPVB.

  CASE P_TYPE.
    WHEN 'T'.
      IF XLIST-BESKZ <> 'F'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-SOBSL = '50'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-RGEKZ <> '1'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-FABKZ <> '1'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
** Changed on 06/29/11
**Paul add E210 also Green when Valuation Class T(3001).
      IF XLIST-WERKS = 'E001'.
        IF XLIST-LGPRO = 'E100' OR
           XLIST-LGPRO = 'E200' OR
           XLIST-LGPRO = 'E210' OR
           XLIST-LGPRO = 'E300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
* by igmoon - Engine Plan Split {
      ELSEIF XLIST-WERKS = 'E002'.
        IF XLIST-LGPRO = 'N100' OR
           XLIST-LGPRO = 'N200' OR
           XLIST-LGPRO = 'N210' OR
           XLIST-LGPRO = 'N300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
* }
      ELSE.
        IF XLIST-LGPRO <> 'P400'.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
** End on 06/29/11

** Changed on 06/29/11
      IF XLIST-WERKS = 'E001' or XLIST-WERKS = 'E002'.
**        IF XLIST-VSPVB IS INITIAL.
****Paul change 07/07/2011 : while werks = e001 initial value is not
**error
****          XLIST-STATUS_MRP2 = CO_STATUS_RED.
**        ELSE.
**          SELECT SINGLE PRVBE INTO L_PRVBE
**           FROM PVBE
**           WHERE WERKS = XLIST-WERKS
**            AND PRVBE = XLIST-VSPVB.
**          IF SY-SUBRC = 0.
**          ELSE.
**            XLIST-STATUS_MRP2 = CO_STATUS_RED.
**          ENDIF.
**        ENDIF.
        XLIST-STATUS_MRP2 = CO_STATUS_GREEN.
      ELSE.
** End on 06/29/11
        IF XLIST-VSPVB IN S_VSPVB.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
** Changed on 06/15/11
**Paul add E210 also Green when Valuation Class T(3001).
      IF XLIST-WERKS = 'E001'.
        IF XLIST-LGFSB = 'E100' OR
            XLIST-LGFSB = 'E200' OR
            XLIST-LGFSB = 'E210' OR
            XLIST-LGFSB = 'E300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
* by ig.moon - Engine Plant Split {
      ELSEIF XLIST-WERKS = 'E002'.
        IF XLIST-LGFSB = 'N100' OR
            XLIST-LGFSB = 'N200' OR
            XLIST-LGFSB = 'N210' OR
            XLIST-LGFSB = 'N300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
* }
      ELSE.
** End on 06/29/11
** Changed on 06/29/11
        IF XLIST-TEMPB = '2'
         OR  XLIST-TEMPB = '3'
          OR  XLIST-TEMPB = '9'.
          CASE XLIST-TEMPB.
            WHEN '3' OR '2'.
              IF XLIST-LGFSB = 'G100'.
              ELSE.
                XLIST-STATUS_MRP2 = CO_STATUS_RED.
              ENDIF.
            WHEN '9'.
              IF XLIST-LGFSB = 'P400'.
              ELSE.
                XLIST-STATUS_MRP2 = CO_STATUS_RED.
              ENDIF.
          ENDCASE.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
*      IF XLIST-LGFSB <> 'P400'.
*        XLIST-STATUS_MRP2 = CO_STATUS_RED.
*      ENDIF.
** end of change

      IF XLIST-MRPPP IS INITIAL AND XLIST-DISLS = 'PK'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.

    WHEN 'S'.
      IF XLIST-BESKZ <> 'F'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-SOBSL = '50'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-RGEKZ <> '1'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF NOT XLIST-FABKZ IS INITIAL.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-LGPRO <> 'P500'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-VSPVB IN S_VSPVB.
      ELSE.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-LGFSB <> 'P500'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-MRPPP <> 'M11'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
    WHEN 'C'.
      IF XLIST-BESKZ <> 'F'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-SOBSL = '50'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-RGEKZ <> '1'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF NOT XLIST-FABKZ IS INITIAL.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
** Changed on 06/29/11
      IF XLIST-WERKS = 'E001'.
        IF XLIST-LGPRO = 'E100' OR
           XLIST-LGPRO = 'E200' OR
           XLIST-LGPRO = 'E300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ELSE.
** End on 06/29/11
        IF XLIST-LGPRO <> 'P400'.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
* by ig.moon - Engine Plant Split {
      IF XLIST-WERKS = 'E002'.
        IF XLIST-LGPRO = 'N100' OR
           XLIST-LGPRO = 'N200' OR
           XLIST-LGPRO = 'N300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ELSE.
** End on 06/29/11
        IF XLIST-LGPRO <> 'P400'.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
* }

** Changed on 06/29/11
      IF XLIST-WERKS = 'E001' or XLIST-WERKS = 'E002'.
**Paul change 07/07/2011 : while werks = e001 initial value is not error
**        IF XLIST-VSPVB IS INITIAL.
**          XLIST-STATUS_MRP2 = CO_STATUS_RED.
**        ELSE.
**          SELECT SINGLE PRVBE INTO L_PRVBE
**           FROM PVBE
**           WHERE WERKS = XLIST-WERKS
**            AND PRVBE = XLIST-VSPVB.
**          IF SY-SUBRC = 0.
**          ELSE.
**            XLIST-STATUS_MRP2 = CO_STATUS_RED.
**          ENDIF.
**        ENDIF.
      ELSE.
** End on 06/29/11
        IF XLIST-VSPVB IN S_VSPVB.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
** Changed on 06/29/11
      IF XLIST-WERKS = 'E001'.
        IF XLIST-LGFSB = 'E100' OR
            XLIST-LGFSB = 'E200' OR
            XLIST-LGFSB = 'E300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
* by ig.moon - Engine Plant Split {
      ELSEIF XLIST-WERKS = 'E002'.
        IF XLIST-LGFSB = 'N100' OR
            XLIST-LGFSB = 'N200' OR
            XLIST-LGFSB = 'N300'.
        ELSE.
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
* }
      ELSE.
** end of 06/29/11
** Changed on 06/15/11
        IF XLIST-LGFSB <> 'G100'.
*      IF XLIST-LGFSB <> 'P400'.
** End of change
          XLIST-STATUS_MRP2 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
      IF XLIST-PLIFZ IS INITIAL.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
    WHEN 'M  '.
      IF XLIST-BESKZ <> 'F'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-SOBSL = '50'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-RGEKZ <> '1'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
*  IF NOT XLIST-FABKZ IS INITIAL.
*    XLIST-STATUS_MRP2 = CO_STATUS_RED.
*  ENDIF.
      IF XLIST-LGPRO <> 'P500'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-VSPVB IN S_VSPVB.
      ELSE.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-LGFSB <> 'P500'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
      IF XLIST-MRPPP <> 'M11'.
        XLIST-STATUS_MRP2 = CO_STATUS_RED.
      ENDIF.
  ENDCASE.
ENDFORM.                    " check_MRP2_view
*&---------------------------------------------------------------------*
*&      Form  check_MRP4_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_MRP4_VIEW USING P_TYPE.

  IF XLIST-SBDKZ <> '2'.
    XLIST-STATUS_MRP4 = CO_STATUS_RED.
  ENDIF.

ENDFORM.                    " check_MRP4_view
*&---------------------------------------------------------------------*
*&      Form  check_plant_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_PLANT_ST1_VIEW USING P_TYPE.

  CASE P_TYPE.
    WHEN 'T' OR 'C'.
      IF XLIST-RAUBE IS INITIAL.
        XLIST-STATUS_PLANT_ST1 = CO_STATUS_RED.
      ELSE.
        XLIST-STATUS_PLANT_ST1 = CO_STATUS_GREEN.
      ENDIF.
** Changed on 06/29/11
      IF XLIST-WERKS = 'E001' or XLIST-WERKS = 'E002'.
        IF XLIST-TEMPB = '01' OR
        XLIST-TEMPB = '02' OR
        XLIST-TEMPB = '03' OR
        XLIST-TEMPB = '4' OR
        XLIST-TEMPB = '9'.
          XLIST-STATUS_PLANT_ST1 = CO_STATUS_GREEN.
        ELSE.
          XLIST-STATUS_PLANT_ST1 = CO_STATUS_RED.
        ENDIF.
      ENDIF.
** End on 06/29/11
    WHEN 'S'.
** Changed by Furong on 07/11/11
*      IF XLIST-TEMPB <> '11'.
      IF XLIST-WERKS = 'P001'.
        IF XLIST-TEMPB <> '1'.
** End on 07/11/11
          XLIST-STATUS_PLANT_ST1 = CO_STATUS_RED.
        ELSE.
          XLIST-STATUS_PLANT_ST1 = CO_STATUS_GREEN.
        ENDIF.
      ENDIF.
    WHEN 'M'.
** Changed by Furong on 07/11/11
*      IF XLIST-TEMPB <> '11' OR
      IF XLIST-TEMPB <> '1' OR
** End
        XLIST-RAUBE <> '13'.
        XLIST-STATUS_PLANT_ST1 = CO_STATUS_RED.
      ELSE.
        XLIST-STATUS_PLANT_ST1 = CO_STATUS_GREEN.
      ENDIF.

  ENDCASE.
ENDFORM.                    " check_plant_view
*&---------------------------------------------------------------------*
*&      Form  check_plant_ST2_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_PLANT_ST2_VIEW USING P_TYPE.

  CASE P_TYPE.
    WHEN 'T'.
      IF XLIST-PRCTR IS INITIAL.
        XLIST-STATUS_PLANT_ST2 = CO_STATUS_RED.
      ELSE.
        XLIST-STATUS_PLANT_ST2 = CO_STATUS_GREEN.
      ENDIF.
    WHEN 'S' OR 'M'.
** Changed by Furong on 06/29/11
*      IF XLIST-PRCTR IS INITIAL OR
*        XLIST-XMCNG IS INITIAL.
*        XLIST-STATUS_PLANT_ST2 = CO_STATUS_RED.
*      ELSE.
*        XLIST-STATUS_PLANT_ST2 = CO_STATUS_GREEN.
*      ENDIF.
** End of change
      IF XLIST-PRCTR IS INITIAL.
        XLIST-STATUS_PLANT_ST2 = CO_STATUS_RED.
      ELSE.
        XLIST-STATUS_PLANT_ST2 = CO_STATUS_GREEN.
      ENDIF.

    WHEN 'C'.
      IF XLIST-PRCTR IS INITIAL OR
           XLIST-BRGEW IS INITIAL OR
           XLIST-NTGEW IS INITIAL OR
           XLIST-GEWEI IS INITIAL.
        XLIST-STATUS_PLANT_ST2 = CO_STATUS_RED.
      ELSE.
        XLIST-STATUS_PLANT_ST2 = CO_STATUS_GREEN.
      ENDIF.
  ENDCASE.
  IF XLIST-XMCNG IS INITIAL.
    XLIST-STATUS_PLANT_ST2 = CO_STATUS_RED.
  ELSE.
    XLIST-STATUS_PLANT_ST2 = CO_STATUS_GREEN.
  ENDIF.

ENDFORM.                    " check_plant_ST2_view
*&---------------------------------------------------------------------*
*&      Form  check_WAREHOUSE_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_WAREHOUSE_VIEW USING P_TYPE.

  CLEAR: XMLGN.
  CASE P_TYPE.
    WHEN 'T'.
      READ TABLE XMLGN WITH KEY MATNR = XLIST-MATNR
                                LGNUM = 'P01'.
      IF SY-SUBRC = 0.
        XLIST-LTKZA = XMLGN-LTKZA.
        XLIST-LTKZE = XMLGN-LTKZE.

        IF XLIST-LTKZA IS INITIAL OR
           XLIST-LTKZE IS INITIAL.
          XLIST-STATUS_WH = CO_STATUS_RED.
        ELSE.
          XLIST-STATUS_WH = CO_STATUS_GREEN.
        ENDIF.

      ELSE.
        XLIST-STATUS_WH = CO_STATUS_RED.
      ENDIF.
    WHEN 'C'.
      READ TABLE XMLGN WITH KEY MATNR = XLIST-MATNR
                                LGNUM = 'P01'.
      IF SY-SUBRC = 0.
        XLIST-LTKZA = XMLGN-LTKZA.
        XLIST-LTKZE = XMLGN-LTKZE.

        IF XLIST-LTKZA = '003' AND
           XLIST-LTKZE = '003'.
          XLIST-STATUS_WH = CO_STATUS_GREEN.
        ELSE.
          XLIST-STATUS_WH = CO_STATUS_RED.
        ENDIF.
      ELSE.
        XLIST-STATUS_WH = CO_STATUS_RED.
      ENDIF.
  ENDCASE.
ENDFORM.                    " check_WAREHOUSE_view
*&---------------------------------------------------------------------*
*&      Form  check_ACCOUNT_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_ACCOUNT_VIEW USING P_TYPE.

  CASE P_TYPE.
    WHEN 'T'.
      IF XLIST-BKLAS <> '3001'.
        XLIST-STATUS_ACCOUNT = CO_STATUS_RED.
      ENDIF.
    WHEN 'S' OR 'M'.
      IF XLIST-BKLAS <> '3005'.
        XLIST-STATUS_ACCOUNT = CO_STATUS_RED.
      ENDIF.
    WHEN 'C'.
      IF XLIST-BKLAS <> '3000'.
        XLIST-STATUS_ACCOUNT = CO_STATUS_RED.
      ENDIF.
  ENDCASE.
ENDFORM.                    " check_ACCOUNT_view
*&---------------------------------------------------------------------*
*&      Form  check_COST_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_COST_VIEW USING P_TYPE.

  IF NOT XLIST-NCOST IS INITIAL.
    XLIST-STATUS_COST = CO_STATUS_RED.
  ENDIF.
ENDFORM.                    " check_COST_view
*&---------------------------------------------------------------------*
*&      Form  check_source_list_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_SOURCE_LIST USING P_TYPE.
  DATA : L_CHK(1).
  CLEAR L_CHK.

  READ TABLE XEORD WITH KEY MATNR = XLIST-MATNR
                            WERKS = XLIST-WERKS
** Paul 07/08/11 ADD : only fix value
                            FEBEL = 'X'.
  IF SY-SUBRC = 0.
    L_CHK = 'X'.
  ELSE.
    READ TABLE XEORD WITH KEY MATNR = XLIST-MATNR
                              WERKS = XLIST-WERKS
                              FLIFN = 'X'.
    IF SY-SUBRC = 0.
      L_CHK = 'X'.
    ENDIF.
  ENDIF.
  IF NOT L_CHK IS INITIAL.
*  IF SY-SUBRC = 0.
    XLIST-STATUS_SOURCE = CO_STATUS_GREEN.
    XLIST-VDATU = XEORD-VDATU.
    XLIST-BDATU = XEORD-BDATU.
    XLIST-VRTYP = XEORD-VRTYP.
    XLIST-EBELN_SOURCE = XEORD-EBELN.
    XLIST-EBELP_SOURCE = XEORD-EBELP.
    XLIST-FEBEL_SOURCE = XEORD-FEBEL.
    XLIST-AUTET = XEORD-AUTET.
    XLIST-LIFNR_SOURCE = XEORD-LIFNR.
    XLIST-FLIFN_SOURCE = XEORD-FLIFN.
    CASE P_TYPE.
      WHEN 'T' OR 'S' OR 'M'.
        IF XEORD-VRTYP <> 'L'.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
        IF  XLIST-EBELN_SOURCE IS INITIAL.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
        IF XLIST-FEBEL_SOURCE IS INITIAL.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
        IF XLIST-AUTET <> '2'.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.

      WHEN 'C'.
        IF XLIST-VRTYP  <> ' '.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
        IF XLIST-LIFNR_SOURCE IS INITIAL.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
        IF XLIST-FLIFN_SOURCE IS INITIAL.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
        IF XLIST-AUTET <> '1'.
          XLIST-STATUS_SOURCE = CO_STATUS_RED.
        ENDIF.
    ENDCASE.
  ELSE.
    XLIST-STATUS_SOURCE = CO_STATUS_RED.
  ENDIF.

ENDFORM.                    " check_source_list_view
*&---------------------------------------------------------------------*
*&      Form  check_INFORECO_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_INFORECO USING P_TYPE.
  DATA: L_FLAG(1),
        L_COUNT TYPE I,
        L_FLAG_PRDAT(1),
        L_FLAG_ERROR(1),
        L_LIFNR LIKE EINA-LIFNR.

  DATA: BEGIN OF LT_EINE OCCURS 0,
        INFNR LIKE EINE-INFNR,
        ESOKZ LIKE EINE-ESOKZ,
        PRDAT LIKE EINE-PRDAT,
        NETPR LIKE EINE-NETPR,
        BSTAE LIKE EINE-BSTAE,
        MWSKZ LIKE EINE-MWSKZ,
        WEBRE LIKE EINE-WEBRE,
        XERSN LIKE EINE-XERSN,

        END OF LT_EINE.

  DATA: BEGIN OF LT_EINA OCCURS 0,
       INFNR LIKE EINA-INFNR,
       MATNR LIKE EINA-MATNR,
       LIFNR LIKE EINA-LIFNR,
       LOEKZ LIKE EINA-LOEKZ,
       END OF LT_EINA.

  SELECT INFNR MATNR LIFNR LOEKZ
   INTO TABLE LT_EINA
   FROM EINA
   WHERE MATNR = XLIST-MATNR.
  IF SY-SUBRC = 0.
    DELETE LT_EINA WHERE LOEKZ = 'X'.
    DESCRIBE TABLE LT_EINA LINES L_COUNT.
    IF L_COUNT = 0.
      XLIST-STATUS_INFOREC = CO_STATUS_RED.
    ELSE.

      CLEAR: L_FLAG.
      LOOP AT LT_EINA.
        IF LT_EINA-LIFNR = 'SEF9' OR
           LT_EINA-LIFNR = 'SSTX' OR
           LT_EINA-LIFNR = 'SBC3'.
          L_FLAG = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      CLEAR: L_FLAG_ERROR.
      IF W_TYPE = 'C'.
        IF L_FLAG <> 'X'.
          L_FLAG_ERROR = 'X'.
        ENDIF.
*      ELSE.
*        IF l_flag = 'X'.
*          l_flag_error = 'X'.
*        ENDIF.
      ENDIF.
      IF L_FLAG_ERROR = 'X'.
        XLIST-STATUS_INFOREC = CO_STATUS_RED.
      ELSE.

*        SELECT infnr esokz prdat netpr bstae mwskz webre xersn
*          INTO TABLE lt_eine
*          FROM eine
*          FOR ALL ENTRIES IN lt_eina
*          WHERE infnr = lt_eina-infnr
*           AND ekorg = 'PU01'
*           AND loekz = ' '.
**           AND WERKS = XLIST-WERKS.
*        IF sy-subrc = 0.
        CLEAR: L_FLAG.
        CLEAR: L_COUNT.
        LOOP AT LT_EINA.
          SELECT SINGLE LIFNR INTO L_LIFNR
            FROM A018
           WHERE KAPPL =  'M'
             AND KSCHL =  'PB00'
             AND MATNR =  XLIST-MATNR
             AND LIFNR =  LT_EINA-LIFNR
             AND EKORG =  'PU01'
             AND ESOKZ =  '0'
             AND DATAB <= SY-DATUM
             AND DATBI >= SY-DATUM.
          IF SY-SUBRC = 0.
            L_COUNT = L_COUNT + 1.
          ELSE.
            DELETE LT_EINA.
          ENDIF.
        ENDLOOP.
        IF L_COUNT > 1.
          LOOP AT LT_EINA.
            READ TABLE XEORD WITH KEY MATNR = XLIST-MATNR
                              WERKS = XLIST-WERKS
                              LIFNR = LT_EINA-LIFNR
** Paul 07/08/11 ADD : only fix value
                              FLIFN = 'X'.
            IF SY-SUBRC = 0.
            ELSE.
              READ TABLE XEORD WITH KEY MATNR = XLIST-MATNR
                               WERKS = XLIST-WERKS
                               LIFNR = LT_EINA-LIFNR
** Paul 07/08/11 ADD : only fix value
                               FEBEL = 'X'.
              IF SY-SUBRC = 0.
              ELSE.
                DELETE LT_EINA.
              ENDIF.
            ENDIF.
          ENDLOOP.
          DESCRIBE TABLE LT_EINA LINES L_COUNT.
          IF L_COUNT <> 1.
            XLIST-STATUS_INFOREC = CO_STATUS_RED.
            L_FLAG_ERROR = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        IF L_FLAG_ERROR IS INITIAL.
          SELECT INFNR ESOKZ PRDAT NETPR BSTAE MWSKZ WEBRE XERSN
          INTO TABLE LT_EINE
          FROM EINE
          FOR ALL ENTRIES IN LT_EINA
          WHERE INFNR = LT_EINA-INFNR
           AND EKORG = 'PU01'
           AND LOEKZ = ' '.
*           AND WERKS = XLIST-WERKS.
          IF SY-SUBRC = 0.

            XLIST-STATUS_INFOREC = CO_STATUS_GREEN.
            READ TABLE LT_EINE INDEX 1.

            XLIST-PRDAT_INFO = LT_EINE-PRDAT.
            XLIST-ESOKZ_INFO = LT_EINE-ESOKZ.
            XLIST-NETPR_INFO = LT_EINE-NETPR.
            XLIST-BSTAE_INFO = LT_EINE-BSTAE.
            XLIST-MWSKZ_INFO = LT_EINE-MWSKZ.
            XLIST-WEBRE_INFO = LT_EINE-WEBRE.
            XLIST-XERSN_INFO = LT_EINE-XERSN.

            READ TABLE LT_EINA WITH KEY INFNR = LT_EINE-INFNR.
            IF SY-SUBRC = 0.
              XLIST-LOEKZ_INFO = LT_EINA-LOEKZ.
              XLIST-LIFNR_INFO = LT_EINA-LIFNR.
            ENDIF.

            SELECT SINGLE LIFNR INTO L_LIFNR
              FROM A018
             WHERE KAPPL =  'M'
               AND KSCHL =  'PB00'
               AND MATNR =  XLIST-MATNR
               AND LIFNR =  XLIST-LIFNR_INFO
               AND EKORG =  'PU01'
               AND ESOKZ =  '0'
               AND DATAB <= SY-DATUM
               AND DATBI >= SY-DATUM.
            IF SY-SUBRC NE 0.
              XLIST-STATUS_INFOREC = CO_STATUS_RED.
              L_FLAG_ERROR = 'X'.
              EXIT.
            ENDIF.

            IF W_TYPE <> 'C'.
              IF   XLIST-LIFNR_INFO = 'SEF9' OR
                   XLIST-LIFNR_INFO = 'SSTX' OR
                   XLIST-LIFNR_INFO = 'SBC3'.
                XLIST-STATUS_INFOREC = CO_STATUS_RED.
                L_FLAG_ERROR = 'X'.
                EXIT.
              ENDIF.
            ENDIF.
            IF LT_EINE-ESOKZ <> '0'.
              XLIST-STATUS_INFOREC = CO_STATUS_RED.
              L_FLAG_ERROR = 'X'.
              EXIT.
            ENDIF.
            IF  LT_EINE-NETPR IS INITIAL.
              XLIST-STATUS_INFOREC = CO_STATUS_RED.
              L_FLAG_ERROR = 'X'.
              EXIT.
            ENDIF.
            IF  LT_EINE-BSTAE IS INITIAL.
              IF W_TYPE = 'C' OR W_TYPE = 'T'.
                XLIST-STATUS_INFOREC = CO_STATUS_RED.
                L_FLAG_ERROR = 'X'.
                EXIT.
              ENDIF.
            ENDIF.
            IF  LT_EINE-MWSKZ <> 'U0'.
              XLIST-STATUS_INFOREC = CO_STATUS_RED.
              L_FLAG_ERROR = 'X'.
              EXIT.
            ENDIF.
            IF  NOT LT_EINE-WEBRE IS INITIAL.
              IF W_TYPE = 'C'.
                XLIST-STATUS_INFOREC = CO_STATUS_RED.
                L_FLAG_ERROR = 'X'.
                EXIT.
              ENDIF.
            ELSE.
              IF W_TYPE <> 'C'.
                XLIST-STATUS_INFOREC = CO_STATUS_RED.
                L_FLAG_ERROR = 'X'.
                EXIT.
              ENDIF.
            ENDIF.
            IF  NOT LT_EINE-XERSN IS INITIAL.
              XLIST-STATUS_INFOREC = CO_STATUS_RED.
              L_FLAG_ERROR = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
*          ENDLOOP.
        ELSE.
** No record in EINE
          XLIST-STATUS_INFOREC = CO_STATUS_RED.
          L_FLAG_ERROR = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    XLIST-STATUS_INFOREC = CO_STATUS_RED.
  ENDIF.

ENDFORM.                    " check_INFORECO_view
*&---------------------------------------------------------------------*
*&      Form  READ_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_SA.
  REFRESH XSA.
  SELECT A~EBELN B~EBELP LIFNR WERKS LGORT MATNR B~KTMNG UEBTK ABUEB
         FABKZ BSTAE A~BSTYP BSART A~LOEKZ KDATB KDATE
    INTO CORRESPONDING FIELDS OF TABLE XSA
    FROM EKKO AS A INNER JOIN EKPO AS B
      ON A~EBELN = B~EBELN
   WHERE A~BSTYP = 'L'
     AND B~MATNR IN S_MATNR
     AND KDATB <= SY-DATUM
     AND KDATE >= SY-DATUM
     AND B~WERKS EQ P_WERKS
*     AND A~LOEKZ EQ SPACE
*     AND E~LOEKZ EQ SPACE
     AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ B~MATNR
                     AND C~WERKS EQ P_WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).

ENDFORM.                    " READ_SA
*&---------------------------------------------------------------------*
*&      Form  CHECK_SA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_SA USING P_TYPE.
  DATA: L_FLAG(1),
        LC_EBELP LIKE EKPO-EBELP VALUE '00001',
        L_INDEX LIKE SY-TABIX.
**Paul Change 07/08/11 : If w_type = T
  READ TABLE XSA WITH KEY MATNR = XLIST-MATNR.
  IF SY-SUBRC = 0.
    XLIST-STATUS_SA = CO_STATUS_GREEN.
    XLIST-LIFNR_SA = XSA-LIFNR.
    XLIST-KTMNG_SA = XSA-KTMNG.
    XLIST-UEBTK_SA = XSA-UEBTK.
    XLIST-ABUEB_SA = XSA-ABUEB.
    XLIST-FABKZ_SA = XSA-FABKZ.
    XLIST-BSTAE_SA = XSA-BSTAE.
    XLIST-BSTYP_SA = XSA-BSTYP.
    XLIST-BSART_SA = XSA-BSART.
    XLIST-LOEKZ_SA = XSA-LOEKZ.
    XLIST-EBELN_SA = XSA-EBELN.
    CLEAR L_FLAG.
    LOOP AT XSA WHERE MATNR = XLIST-MATNR.
      L_INDEX = SY-TABIX.
      IF W_TYPE = 'T'.
        IF ( XSA-WERKS = 'P001' AND XSA-LGORT = 'P400' AND
             XSA-LOEKZ = ' '    AND XSA-EBELP = LC_EBELP )
        OR ( XSA-WERKS = 'P001' AND XSA-LGORT = 'G100' AND
             XSA-LOEKZ = ' '    AND XSA-EBELP = LC_EBELP )
        OR ( XSA-WERKS = 'E001' AND XSA-LGORT = 'E210' AND
             XSA-LOEKZ = ' '    AND XSA-EBELP = LC_EBELP )
* by ig.moon - Engine plant split {
        OR ( XSA-WERKS = 'E002' AND XSA-LGORT = 'N210' AND
             XSA-LOEKZ = ' '    AND XSA-EBELP = LC_EBELP ).
* }
          IF XSA-LGORT = XLIST-LGFSB.
            L_FLAG = 'X'.
            EXIT.
          ENDIF.
*        ELSE.
*          L_FLAG = 'X'.
*          EXIT.
        ENDIF.
      ELSE.
        IF XSA-WERKS = 'P001'  AND
           XSA-LGORT = 'P500'  AND
           XSA-LOEKZ = ' '  AND
           XSA-EBELP = LC_EBELP.
          L_FLAG = 'X'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF L_FLAG = 'X'.
      READ TABLE XSA INDEX L_INDEX.
      XLIST-STATUS_SA = CO_STATUS_GREEN.
      XLIST-LIFNR_SA = XSA-LIFNR.
      XLIST-KTMNG_SA = XSA-KTMNG.
      XLIST-UEBTK_SA = XSA-UEBTK.
      XLIST-ABUEB_SA = XSA-ABUEB.
      XLIST-FABKZ_SA = XSA-FABKZ.
      XLIST-BSTAE_SA = XSA-BSTAE.
      XLIST-BSTYP_SA = XSA-BSTYP.
      XLIST-BSART_SA = XSA-BSART.
      XLIST-LOEKZ_SA = XSA-LOEKZ.
      XLIST-EBELN_SA = XSA-EBELN.

*      XLIST-KTMNG = XSA-KTMNG.
*      XLIST-KTMNG_SA = XSA-KTMNG.
*      XLIST-UEBTK_SA = XSA-UEBTK.
*      XLIST-ABUEB_SA = XSA-ABUEB.
*      XLIST-FABKZ_SA = XSA-FABKZ.
*      XLIST-BSTAE_SA = XSA-BSTAE.
*      XLIST-BSTYP_SA = XSA-BSTYP.
*      XLIST-BSART_SA = XSA-BSART.
*      XLIST-LOEKZ_SA = XSA-LOEKZ.

      IF XLIST-KTMNG_SA <> 9999999 AND
          XLIST-KTMNG_SA <> 999999.
        XLIST-STATUS_SA = CO_STATUS_RED.
      ENDIF.
**Paul Change 07/08/11 : =''
*      IF XLIST-ABUEB_SA <> 'Z001'.
*        XLIST-STATUS_SA = CO_STATUS_RED.
*      ENDIF.
      IF XLIST-ABUEB_SA = ' '.
        XLIST-STATUS_SA = CO_STATUS_RED.
      ENDIF.
**
      IF NOT XLIST-LOEKZ_SA IS INITIAL.
        XLIST-STATUS_SA = CO_STATUS_RED.
      ENDIF.

      IF XSA-KDATB > SY-DATUM.
        XLIST-STATUS_SA = CO_STATUS_RED.
      ENDIF.
      IF XSA-KDATE < SY-DATUM.
        XLIST-STATUS_SA = CO_STATUS_RED.
      ENDIF.

      CASE P_TYPE.
        WHEN 'T'.
          IF NOT XLIST-UEBTK_SA IS INITIAL.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF XLIST-FABKZ_SA <> '1'.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF XLIST-BSART_SA <> 'JIT'.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
        WHEN 'S'.
          IF XLIST-UEBTK_SA IS INITIAL.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF XLIST-FABKZ_SA = '1'.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF NOT XLIST-BSTAE_SA IS INITIAL.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF XLIST-BSART_SA <> 'JIS'.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
        WHEN 'M'.
          IF XLIST-UEBTK_SA IS INITIAL.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF NOT XLIST-FABKZ_SA  IS INITIAL.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF NOT XLIST-BSTAE_SA IS INITIAL.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
          IF XLIST-BSART_SA <> 'JIS'.
            XLIST-STATUS_SA = CO_STATUS_RED.
          ENDIF.
      ENDCASE.
    ELSE.
      XLIST-STATUS_SA = CO_STATUS_RED.
    ENDIF.
  ELSE.
    IF W_TYPE = 'T'.
      IF XLIST-LGFSB <> 'P400'.
        IF XLIST-LGFSB <> 'G100'.
*          IF XLIST-LGFSB <> 'E210'.
          IF XLIST-LGFSB <> 'E210' and XLIST-LGFSB <> 'N210'.
            XLIST-STATUS_SA = CO_STATUS_GREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      XLIST-STATUS_SA = CO_STATUS_RED.
    ENDIF.
  ENDIF.
  IF W_TYPE = 'T' AND XLIST-WERKS = 'P001'.
    IF XLIST-TEMPB = '2' OR
       XLIST-TEMPB = '3' OR
       XLIST-TEMPB = '4'.
      IF XLIST-LGORT <> 'G100'.
        XLIST-STATUS_SA = CO_STATUS_RED.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_SA
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONTROL_CYCLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_CONTROL_CYCLE USING P_TYPE.
*DATA: L_LGPLA LIKE MLGT-LGPLA.

*  IF W_TYPE = 'T'.
  IF XLIST-TEMPB = '3' OR
     XLIST-TEMPB = '4'.
    READ TABLE XPKHD WITH KEY MATNR = XLIST-MATNR
                              WERKS = P_WERKS.
**Paul Change 07072011 : 'P001' -> P_WERKS
**     Change 07082011 : PRVBE Comment
*                             WERKS = 'P001'
*                             PRVBE = XLIST-VSPVB.
    IF SY-SUBRC = 0.
      IF ( XLIST-TEMPB = '3' AND XPKHD-RKSTA = 'I' )
         OR ( XLIST-TEMPB = '4' AND XPKHD-RKSTA = 'K' ).
        XLIST-STATUS_CC = CO_STATUS_GREEN.
      ELSE.
        XLIST-STATUS_CC = CO_STATUS_RED.
      ENDIF.
    ELSE.
      XLIST-STATUS_CC = CO_STATUS_RED.
    ENDIF.
  ELSE.
**  Paul change red -> Green : 07/05/11
*    XLIST-STATUS_CC = CO_STATUS_RED.
    XLIST-STATUS_CC = CO_STATUS_GREEN.
  ENDIF.
*  ENDIF.


*  READ TABLE XPKHD WITH KEY MATNR = XLIST-MATNR
*                                 WERKS = 'P001'
*                                 PRVBE = XLIST-VSPVB.
*  IF SY-SUBRC = 0.
*    XLIST-LGNUM = XPKHD-LGNUM.
*    XLIST-LGTYP = XPKHD-LGTYP.
*    XLIST-LGPLA = XPKHD-LGPLA.
*    IF XLIST-LGNUM <> 'P01' OR
*      XLIST-LGTYP IS INITIAL.
*      XLIST-STATUS_CC = CO_STATUS_RED.
*    ELSE.
*      CLEAR: MLGT.
*      SELECT SINGLE LGPLA INTO MLGT-LGPLA
*       FROM MLGT
*       WHERE MATNR = XLIST-MATNR
*         AND LGNUM = XLIST-LGNUM
*         AND LGTYP = XLIST-LGTYP.
*      IF XLIST-LGPLA = ' '.
*        XLIST-STATUS_CC = CO_STATUS_RED.
*      ELSE.
*        XLIST-STATUS_CC = CO_STATUS_GREEN.
*      ENDIF.
*    ENDIF.
*  ELSE.
*    XLIST-STATUS_CC = CO_STATUS_RED.
*  ENDIF.
ENDFORM.                    " CHECK_CONTROL_CYCLE
*&---------------------------------------------------------------------*
*&      Form  EVENT_HANDLER_REGISTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EVENT_HANDLER_REGISTER.

  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
   EXPORTING
     I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.
  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.

ENDFORM.                    " EVENT_HANDLER_REGISTER
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED USING  U_CHANGED
                          TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA: LS_GOOD TYPE LVC_S_MODI,
         LV_VALUE TYPE LVC_VALUE,
         LVC_T_ROW TYPE LVC_T_ROW.

  ERROR_IN_DATA = SPACE.
  LOOP AT U_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.
    CASE LS_GOOD-FIELDNAME.
* check if column Name1 of this row was changed
      WHEN 'CHECK'.

        CALL METHOD U_CHANGED->GET_CELL_VALUE
                   EXPORTING
                      I_ROW_ID  = LS_GOOD-ROW_ID
                      I_FIELDNAME = LS_GOOD-FIELDNAME
                   IMPORTING
                      E_VALUE =   LV_VALUE.

        IF  ERROR_IN_DATA = ' '.
          CALL METHOD U_CHANGED->MODIFY_CELL
                  EXPORTING
                       I_ROW_ID = LS_GOOD-ROW_ID
                       I_FIELDNAME = LS_GOOD-FIELDNAME
                       I_VALUE     = LV_VALUE.
        ELSE.
          CALL METHOD U_CHANGED->ADD_PROTOCOL_ENTRY
           EXPORTING
           I_MSGID     = 'ZMMM'
           I_MSGNO     = '000'
           I_MSGTY     = 'E'
           I_MSGV1     = 'Reason Code not found - '
           I_MSGV2     = LV_VALUE
*           I_MSGV3     = LV_VALUE
           I_FIELDNAME = LS_GOOD-FIELDNAME
           I_ROW_ID    = LS_GOOD-ROW_ID.

        ENDIF.
    ENDCASE.
  ENDLOOP.

*§7.Display application log if an error has occured.
  IF ERROR_IN_DATA EQ 'X'.

    CALL METHOD U_CHANGED->DISPLAY_PROTOCOL.
  ENDIF.

ENDFORM.                    " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID.

  READ TABLE XLIST INDEX E_ROW_ID.

  CASE E_COLUMN_ID.
    WHEN 'STATUS_BASIC' OR 'MATKL' OR 'RMATP' OR 'MSTAE' OR
               'LABOR' OR 'MEINS' OR 'MATNR' OR 'MAKTX' OR
               'FERTH' OR 'FORMT'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_BASIC
                                     'MM02' 'MM03'.
    WHEN 'STATUS_PURCHASE' OR 'EKGRP' OR 'KORDB'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                          XLIST-MATNR
                                          XLIST-WERKS
                                          CO_PURCHASING
                                          'MM02' 'MM03'.
    WHEN 'STATUS_IMPORT'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                    XLIST-MATNR
                                    XLIST-WERKS
                                     CO_IMPORT
                                     'MM02' 'MM03'.
    WHEN 'STATUS_MRP1' OR 'DISGR' OR 'MAABC' OR 'DISMM' OR
          'DISPO' OR 'DISLS' OR 'BSTRF'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                  XLIST-MATNR
                                    XLIST-WERKS
                                     CO_MRP1
                                     'MM02' 'MM03'.
    WHEN 'STATUS_MRP2' OR 'BESKZ' OR 'SOBSL' OR 'LGPRO' OR
                 'LGFSB' OR 'PLIFZ' OR 'FHORI' OR 'VSPVB' OR
                 'SHZET' OR 'EISBE' OR 'INSLC'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                             XLIST-MATNR
                            XLIST-WERKS
                                     CO_MRP2
                                     'MM02' 'MM03'.
    WHEN 'STATUS_MRP4' OR 'SAUFT' OR 'SBDKZ' OR 'KZBED'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_MRP4
                                     'MM02' 'MM03'.
    WHEN 'STATUS_PLANT_ST1'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_STORAGE
                                     'MM02' 'MM03'.
    WHEN 'STATUS_PLANT_ST2'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_STORAGE2
                                      'MM02' 'MM03'.

    WHEN 'STATUS_WH'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_WH
                                     'MM02' 'MM03'.

    WHEN 'STATUS_ACCOUNT' OR 'BKLAS'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_ACCOUNTING1
                                     'MM02' 'MM03'.

    WHEN 'STATUS_COST' OR 'NCOST'.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                     XLIST-WERKS
                                     CO_COSTING1
                                     'MM02' 'MM03'.
    WHEN 'STATUS_SOURCE'.    " OR 'KORDB'.

      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                   XLIST-WERKS
                                     ''
                                     'ME01' 'ME03'.
    WHEN 'STATUS_INFOREC'.

      READ TABLE X001W WITH KEY WERKS = XLIST-WERKS.
      IF SY-SUBRC EQ 0.
        SET PARAMETER ID 'EKO' FIELD X001W-EKORG.
      ENDIF.
*      CASE XLIST-STATUS_INFOREC.
*        WHEN CO_STATUS_RED.
      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                   XLIST-WERKS
                                     ''
                                     'ME12' 'ME13'.

*        WHEN OTHERS.
*          PERFORM MAINTAIN_DISPLAY USING CHANGE_M
*                                           XLIST-MATNR
*                                       XLIST-WERKS
*                                         ''
*                                         'ME1M' 'ME1M'.

    WHEN 'STATUS_SA' OR 'EBELN_SOURCE' OR 'EBELP_SOURCE'.
*    if XLIST-EBELN_SOURCe is initial.

*      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
*                                       XLIST-MATNR
*                                       XLIST-WERKS
*                                       ''
*                                    'ME32L' 'ME33L'.
*      ELSE.
      IF CHANGE_M IS INITIAL.
        CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
             EXPORTING
                  I_EBELN              = XLIST-EBELN_SOURCE
                  I_EBELP              = XLIST-EBELP_SOURCE
                  I_EDIT               = CHANGE_M
             EXCEPTIONS
                  NOT_FOUND            = 1
                  NO_AUTHORITY         = 2
                  INVALID_CALL         = 3
                  PREVIEW_NOT_POSSIBLE = 4
                  OTHERS               = 5.
      ELSE.
        CALL FUNCTION 'ME_CHANGE_PURCHASE_DOCUMENT'
          EXPORTING
             I_EBELN              = XLIST-EBELN_SOURCE
             I_EBELP              = XLIST-EBELP_SOURCE
*      I_ENJOY            = ' '
         EXCEPTIONS
             NOT_FOUND          = 1
             NO_AUTHORITY       = 2
             INVALID_CALL       = 3
             OTHERS             = 4
                  .
        IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.
    WHEN 'STATUS_CC'.    "

      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                   XLIST-WERKS
                                     ''
*                                     'LPK2' 'LPK3'
                                     'PK02' 'PK03'.
    WHEN 'STATUS_BOM'.    "

      PERFORM MAINTAIN_DISPLAY USING CHANGE_M
                                     XLIST-MATNR
                                   XLIST-WERKS
                                     ''
                                     'CS02' 'CS03'.


  ENDCASE.


ENDFORM.                    " hotspot_click
*&---------------------------------------------------------------------*
*&      Form  maintain_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MAINTAIN_DISPLAY  USING  CHANGE_M
                              IV_MATNR
                              IV_WERKS
                              IV_AUSWG
                              IV_TCODE_M
                              IV_TCODE_D.
  DATA:TCOD LIKE SY-TCODE,
       L_PRVBE LIKE PKHD-PRVBE,
       L_TCODE LIKE SY-TCODE. " VALUE 'ME12'.

  IF CHANGE_M  = 'X'.
    PERFORM CHECK_TCODE_AUTH_NW USING IV_TCODE_M CHANGING TCOD.
    CASE TCOD.
      WHEN 'MM01' OR 'MM02' OR 'MM03'.
        IF TCOD = 'MM02'.
          CLEAR FIRST_ACT.
        ENDIF.
        PERFORM MAINTAIN_MATERIAL_VIEW USING IV_MATNR
                                             IV_WERKS
                                             IV_AUSWG
                                             TCOD.
      WHEN 'ME11' OR 'ME12' OR 'ME13'.
        SET PARAMETER ID 'LIF' FIELD XLIST-LIFNR_INFO.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD SPACE.
        IF IV_TCODE_M = 'ME13'.
          CALL TRANSACTION TCOD AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION TCOD.
        ENDIF.
      WHEN 'ME01' OR 'ME02' OR 'ME03'.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
        CALL TRANSACTION TCOD AND SKIP FIRST SCREEN.
      WHEN 'ME1M'.
        SUBMIT RM06IM00 WITH IF_MATNR EQ IV_MATNR AND RETURN.
      WHEN 'ME31L' OR 'ME32L'.
        SET PARAMETER ID 'VRT' FIELD XLIST-EBELN_SA.
        CALL TRANSACTION 'ME32L'.
      WHEN 'POP1'.
        CALL TRANSACTION 'POP1'.
*      WHEN 'LPK2' OR 'LPK3' .
      WHEN 'PK02' OR 'PK03' .
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
**        Paul Changed 07/08/11 : Case plant is E001.
        IF P_WERKS = 'E001'.
          CLEAR : L_PRVBE.
          SELECT SINGLE PRVBE
            INTO L_PRVBE
            FROM PKHD
           WHERE WERKS EQ P_WERKS
             AND MATNR EQ IV_MATNR.

          SET PARAMETER ID 'PVB' FIELD L_PRVBE.
        ELSE.
          SET PARAMETER ID 'PVB' FIELD XLIST-VSPVB.
        ENDIF.

**        SET PARAMETER ID 'PVB' FIELD XLIST-VSPVB.
        CALL TRANSACTION 'PK02'  AND SKIP FIRST SCREEN..
      WHEN 'CS02' OR 'CS03'.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
        SET PARAMETER ID 'CSV' FIELD '2'.
        IF IV_TCODE_M = 'CS03'.
          CALL TRANSACTION TCOD AND SKIP FIRST SCREEN.
        ELSE.
          CALL TRANSACTION TCOD.
        ENDIF.
    ENDCASE.
  ELSE.
    CASE IV_TCODE_D.
      WHEN 'MM03'.
        PERFORM MAINTAIN_MATERIAL_VIEW USING IV_MATNR
                                             IV_WERKS
                                             IV_AUSWG
                                             IV_TCODE_D.
      WHEN 'ME03'.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
        CALL TRANSACTION IV_TCODE_D AND SKIP FIRST SCREEN.
      WHEN 'ME1M'.
        SUBMIT RM06IM00 WITH IF_MATNR EQ IV_MATNR AND RETURN.
      WHEN 'ME13'.
        SET PARAMETER ID 'LIF' FIELD XLIST-LIFNR_INFO.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD SPACE.
        CALL TRANSACTION IV_TCODE_D AND SKIP FIRST SCREEN.
      WHEN 'ME33L'.
        SET PARAMETER ID 'VRT' FIELD XLIST-EBELN_SA.
        CALL TRANSACTION IV_TCODE_D.  " AND SKIP FIRST SCREEN.
      WHEN 'POP1'.
      WHEN 'LPK3'.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
        SET PARAMETER ID 'PVB' FIELD XLIST-VSPVB.
        CALL TRANSACTION 'LPK3' AND SKIP FIRST SCREEN..
      WHEN 'CS03'.
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
        SET PARAMETER ID 'CSV' FIELD '2'.
        CALL TRANSACTION 'CS03' AND SKIP FIRST SCREEN.
      WHEN 'PK03' .
        SET PARAMETER ID 'MAT' FIELD IV_MATNR.
        SET PARAMETER ID 'WRK' FIELD IV_WERKS.
**        Paul Changed 07/08/11 : Case plant is E001.
        IF P_WERKS = 'E001'.
          CLEAR : L_PRVBE.
          SELECT SINGLE PRVBE
            INTO L_PRVBE
            FROM PKHD
           WHERE WERKS EQ P_WERKS
             AND MATNR EQ IV_MATNR.

          SET PARAMETER ID 'PVB' FIELD L_PRVBE.
        ELSE.
          SET PARAMETER ID 'PVB' FIELD XLIST-VSPVB.
        ENDIF.
        CALL TRANSACTION 'PK03'  AND SKIP FIRST SCREEN.

    ENDCASE.
  ENDIF.
ENDFORM.                    " maintain_display
*&---------------------------------------------------------------------*
*&      Form  check_tcode_auth_nw
*&---------------------------------------------------------------------*
FORM CHECK_TCODE_AUTH_NW USING T_CODE CHANGING TC.
* Authority Check
  TC = T_CODE.
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
       EXPORTING
            TCODE  = T_CODE
       EXCEPTIONS
            OK     = 0
            NOT_OK = 1
            OTHERS = 2.
  IF SY-SUBRC NE 0.
    MESSAGE S172(00) WITH T_CODE." RAISING no_authority.
    CASE T_CODE.
      WHEN 'MM01' OR 'MM02' OR 'MMSC' OR 'ME03'.
        TC = 'MM03'.
      WHEN 'ME01' OR 'ME1M'.
        TC = 'ME03'.
      WHEN 'POP1' OR 'POP2'.
        TC = 'POP3'.
    ENDCASE.
  ENDIF.
ENDFORM.                    " check_tcode_auth_nw
*&      Form  MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*       Maintain Material View
*----------------------------------------------------------------------*
*      -->IV_MATNR  Material
*      -->IV_WERKS  Plant
*      -->IV_AUSWG  Screen Sequence
*      -->IV_TCODE  Transaction Code
*----------------------------------------------------------------------*
FORM MAINTAIN_MATERIAL_VIEW  USING    IV_MATNR
                                      IV_WERKS
                                      IV_AUSWG
                                      IV_TCODE.
  DATA LF_VIEW(15).

  CASE IV_AUSWG.
    WHEN CO_BASIC.
      LF_VIEW = 'K'.
    WHEN CO_SALES OR CO_EXPORT.
      LF_VIEW = 'V'.
    WHEN CO_PURCHASING OR CO_IMPORT.
      LF_VIEW = 'E'.
    WHEN CO_MRP1 OR CO_MRP2 OR CO_MRP3 OR CO_MRP4.
      LF_VIEW = 'D'.
    WHEN CO_QUALITY.
      LF_VIEW = 'Q'.
    WHEN CO_ACCOUNTING1 OR CO_ACCOUNTING2.
      LF_VIEW = 'B'.
    WHEN CO_COSTING1 OR CO_COSTING2.
      LF_VIEW = 'G'.
    WHEN CO_STORAGE  OR CO_STORAGE2.
      LF_VIEW = 'L'.
    WHEN CO_CLASS.
      LF_VIEW = 'C'.
    WHEN CO_FORECAST.
      LF_VIEW = 'P'.
    WHEN CO_TOOLS.
      LF_VIEW = 'F'.
    WHEN CO_WORK.
      LF_VIEW = 'A'.
    WHEN CO_WH.
      LF_VIEW = 'S'.

  ENDCASE.

  SET PARAMETER ID 'MXX' FIELD LF_VIEW.
  SET PARAMETER ID 'MAT' FIELD IV_MATNR.
  SET PARAMETER ID 'WRK' FIELD IV_WERKS.
  SET PARAMETER ID 'VZW' FIELD IV_WERKS.
*  get parameter id 'LAG' field lv_lgort.
*  set parameter id 'LAG' field l_lgort_leer.

  CASE IV_TCODE.
    WHEN 'MM01'.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
               ID 'TCD' FIELD 'MM01'.
      IF SY-SUBRC NE 0.
        MESSAGE E172(00) WITH 'MM01'.
      ENDIF.
      CALL TRANSACTION 'MM01' AND SKIP FIRST SCREEN.
    WHEN 'MM02'.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
               ID 'TCD' FIELD 'MM02'.
      IF SY-SUBRC NE 0.
        MESSAGE E172(00) WITH 'MM02'.
      ENDIF.
      CALL TRANSACTION 'MM02' AND SKIP FIRST SCREEN.
    WHEN 'MM03'.
      AUTHORITY-CHECK OBJECT 'S_TCODE'
             ID 'TCD' FIELD 'MM03'.
      IF SY-SUBRC NE 0.
        MESSAGE E172(00) WITH 'MM03'.
      ENDIF.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " MAINTAIN_MATERIAL_VIEW
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ALL_VIEW_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ALL_VIEW_STATUS.

  IF XLIST-STATUS_BASIC    EQ CO_STATUS_RED OR
*     XLIST-STATUS_SALES    EQ CO_STATUS_RED OR
*     XLIST-STATUS_EXPORT   EQ CO_STATUS_RED OR
     XLIST-STATUS_PURCHASE EQ CO_STATUS_RED OR
     XLIST-STATUS_IMPORT   EQ CO_STATUS_RED OR
     XLIST-STATUS_MRP1     EQ CO_STATUS_RED OR
     XLIST-STATUS_MRP2     EQ CO_STATUS_RED OR
*     XLIST-STATUS_MRP3     EQ CO_STATUS_RED OR
     XLIST-STATUS_MRP4     EQ CO_STATUS_RED OR
*     XLIST-STATUS_STORAGE  EQ CO_STATUS_RED OR
*     XLIST-STATUS_QM       EQ CO_STATUS_RED OR
     XLIST-STATUS_PLANT_ST1 EQ CO_STATUS_RED OR
     XLIST-STATUS_PLANT_ST2 EQ CO_STATUS_RED OR
     XLIST-STATUS_ACCOUNT  EQ CO_STATUS_RED OR
     XLIST-STATUS_COST     EQ CO_STATUS_RED OR
     XLIST-STATUS_INFOREC  EQ CO_STATUS_RED OR
     XLIST-STATUS_SOURCE   EQ CO_STATUS_RED OR
     XLIST-STATUS_SA       EQ CO_STATUS_RED OR
     XLIST-STATUS_BOM       EQ CO_STATUS_RED OR
     XLIST-STATUS_CC       EQ CO_STATUS_RED.
    XLIST-LIGHT = '1'.
    XLIST-DOWNLOAD = 'F'.
  ELSE.
    XLIST-LIGHT = '3'.
    XLIST-DOWNLOAD = 'S'.
  ENDIF.

ENDFORM.                    " UPDATE_ALL_VIEW_STATUS
*&---------------------------------------------------------------------*
*&      Form  filter_material_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILTER_MATERIAL_STATUS.
  IF  PA_ERROR EQ 'X'.
    DELETE XLIST WHERE LIGHT NE '1'.
  ELSEIF PA_NORM  EQ 'X'.
    DELETE XLIST WHERE LIGHT EQ '1'.
  ENDIF.

ENDFORM.                    " filter_material_status
*&---------------------------------------------------------------------*
*&      Form  CHECK_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TYPE  text
*----------------------------------------------------------------------*
FORM CHECK_BOM USING P_TYPE.
  DATA: L_STLST LIKE STKO-STLST.

  SELECT SINGLE A~STLNR A~STLAN B~STLST
      INTO (XLIST-STLNR, XLIST-STLAN, L_STLST)
*       INTO LW_LIST
        FROM MAST AS A
        INNER JOIN STKO AS B
        ON A~STLNR = B~STLNR
        WHERE A~MATNR = XLIST-MATNR
        AND A~WERKS = XLIST-WERKS
        AND A~STLAN = '2'
        AND B~STLTY = 'M'.

*  READ TABLE XMAST WITH KEY MATNR = XLIST-MATNR
*                             WERKS = XLIST-WERKS
*                             STLAN = '2'.
  IF SY-SUBRC EQ 0.
*    XLIST-STLAN = XMAST-STLAN.
*    XLIST-STLNR = XMAST-STLNR.


    IF XLIST-STLNR IS INITIAL OR
       L_STLST <> '01'.
      XLIST-STATUS_BOM = CO_STATUS_RED.
    ELSE.
      XLIST-STATUS_BOM = CO_STATUS_GREEN.
    ENDIF.
  ELSE.
    XLIST-STATUS_BOM = CO_STATUS_RED.
  ENDIF.

ENDFORM.                    " CHECK_BOM
*&---------------------------------------------------------------------*
*&      Form  read_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BOM.
  REFRESH XMAST.
  SELECT *
    INTO TABLE XMAST
    FROM MAST AS B
   WHERE B~WERKS EQ P_WERKS
     AND B~MATNR IN S_MATNR
*     AND b~stlan IN ('1','M')
        AND EXISTS ( SELECT *
                    FROM MARA AS M INNER JOIN MARC AS C
                                      ON M~MATNR = C~MATNR
                   WHERE M~MATNR EQ B~MATNR
                     AND C~WERKS EQ P_WERKS
                     AND C~MATNR IN S_MATNR
**Paul insert Control Type 07/06/11
                     AND M~TEMPB IN S_TEMPB
                     AND C~LGFSB IN S_LGFSB
                     AND C~LGPRO IN S_LGPRO
**
                     AND M~MTART IN S_MTART
*                     AND M~LABOR IN SO_LABOR
                     AND M~MATKL IN S_MATKL
                     AND M~ERSDA IN S_ERSDA
                     AND M~LAEDA IN S_LAEDA
*                     AND C~KZKRI IN SO_KZKRI
                     AND C~BESKZ IN S_BESKZ
*                     AND C~SOBSL IN SO_SOBSL
                     AND C~LVORM IN S_LVORM
*                     AND C~FABKZ IN SO_FABKZ
                     AND C~DISPO IN S_DISPO
*                     AND C~EKGRP IN SO_EKGRP
*                     AND M~TEMPB IN SO_TEMPB
                     AND M~MSTAE IN S_MSTAE ).
ENDFORM.                    " READ_BOM
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD.
  DATA: L_COUNT               TYPE I,
        WA_FILENAME       LIKE RLGRAP-FILENAME,
        BEGIN OF IT_EXCEL OCCURS 0,
        MATNR(18),
*        WERKS(4),
        MAKTX(40),
*        mtart(4),
        MATKL(9),
        MSTAE(2),
*        mtpos_mara(4),
        BRGEW(13),
        NTGEW(13),
        GEWEI(4),
        PROFL(10),
        EKGRP(8),
        STAWN(17),
        HERKL(3),
        DISPO(3),
        DISLS(3),
        BSTRF(13),
        BESKZ(6),
        RGEKZ(3),
        FABKZ(10),
        LGPRO(8),
        VSPVB(10),
        LGFSB(6),
        MRPPP(10),
        PLIFZ(10),
        LTKZA(10),
        LTKZE(8),
        RAUBE(4),
        TEMPB(8),
        XMCNG(5),
        BKLAS(10),
        KORDB(8),
        VDATU(10),
        BDATU(10),
        VRTYP(7),
        EBELN_SOURCE(10),
        FEBEL_SOURCE(3),
        AUTET(6),
        LIFNR_SOURCE(10),
        FLIFN_SOURCE(5),
        LOEKZ_INFO(3),
        LIFNR_INFO(10),
        PRDAT_INFO(10),
        ESOKZ_INFO(8),
        NETPR(13),
        BSTAE_INFO(8),
        MWSKZ_INFO(3),
        WEBRE_INFO(8),
        XERSN_INFO(3),
        KTMNG_SA(13),
        UEBTK_SA(5),
        ABUEB_SA(8),
        FABKZ_SA(3),
        BSTAE_SA(13),
        BSTYP_SA(7),
        BSART_SA(5),
        LOEKZ_SA(3),
        LGNUM(4),
        LGTYP(8),
        LGPLA(6),
        STLAN(10),
        STLNR(10),
        STATUS_BASIC(5),
        STATUS_PLANT_ST1(5),
        STATUS_PLANT_ST2(5),
        STATUS_PURCHASE(5),
        STATUS_IMPORT(5),
        STATUS_MRP1(5),
        STATUS_MRP2(5),
        STATUS_MRP4(5),
        STATUS_WH(5),
        STATUS_ACCOUNT(5),
        STATUS_COST(5),
        STATUS_SOURCE(5),
        STATUS_INFOREC(5),
        STATUS_SA(5),
        STATUS_CC(5),
        STATUS_BOM(5),
        DOWNLOAD(2),
  END OF IT_EXCEL.

  LOOP AT XLIST.
    MOVE-CORRESPONDING XLIST TO IT_EXCEL.
    CASE IT_EXCEL-STATUS_BASIC.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_BASIC = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_BASIC = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_BASIC.
    ENDCASE.
    CASE IT_EXCEL-STATUS_PLANT_ST1.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_PLANT_ST1 = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_PLANT_ST1 = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_PLANT_ST1.
    ENDCASE.
    CASE IT_EXCEL-STATUS_PLANT_ST2.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_PLANT_ST2 = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_PLANT_ST2 = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_PLANT_ST2.
    ENDCASE.
    CASE IT_EXCEL-STATUS_PURCHASE.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_PURCHASE = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_PURCHASE = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_PURCHASE.
    ENDCASE.
    CASE IT_EXCEL-STATUS_IMPORT.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_IMPORT = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_IMPORT = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_IMPORT.
    ENDCASE.
    CASE IT_EXCEL-STATUS_MRP1.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_MRP1 = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_MRP1 = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_MRP1.
    ENDCASE.
    CASE IT_EXCEL-STATUS_MRP2.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_MRP2 = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_MRP2 = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_MRP2.
    ENDCASE.
    CASE IT_EXCEL-STATUS_MRP4.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_MRP4 = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_MRP4 = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_MRP4.
    ENDCASE.
    CASE IT_EXCEL-STATUS_WH.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_WH = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_WH = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_WH.
    ENDCASE.
    CASE IT_EXCEL-STATUS_ACCOUNT.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_ACCOUNT = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_ACCOUNT = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_ACCOUNT.
    ENDCASE.
    CASE IT_EXCEL-STATUS_COST.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_COST = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_COST = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_COST.
    ENDCASE.
    CASE IT_EXCEL-STATUS_SOURCE.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_SOURCE = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_SOURCE = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_SOURCE.
    ENDCASE.
    CASE IT_EXCEL-STATUS_INFOREC.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_INFOREC = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_INFOREC = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_INFOREC.
    ENDCASE.
    CASE IT_EXCEL-STATUS_SA.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_SA = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_SA = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_SA.
    ENDCASE.
    CASE IT_EXCEL-STATUS_CC.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_CC = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_CC = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_CC.
    ENDCASE.
    CASE IT_EXCEL-STATUS_BOM.
      WHEN '@5B\Q'.     " co_status_green.
        IT_EXCEL-STATUS_BOM = 'S'.
      WHEN '@5C\Q'.    " co_status_red.
        IT_EXCEL-STATUS_BOM = 'F'.
      WHEN OTHERS.
        CLEAR: IT_EXCEL-STATUS_BOM.
    ENDCASE.

    APPEND IT_EXCEL.
  ENDLOOP.
  DESCRIBE TABLE XLIST LINES L_COUNT.
  CLEAR: IT_EXCEL.
  INSERT IT_EXCEL INDEX 1 .
  IT_EXCEL-MATNR = 'Material No.'.
  IT_EXCEL-MAKTX = 'Material Description'.
  IT_EXCEL-MATKL = 'Mat. Grp'.
  IT_EXCEL-MSTAE = 'X-Pl'.
  IT_EXCEL-BRGEW = 'Gross Weight'.
  IT_EXCEL-NTGEW = 'Net Weight'.
  IT_EXCEL-GEWEI = 'Unit'.
  IT_EXCEL-PROFL = 'LP/KD/MIP'.
  IT_EXCEL-EKGRP = 'Pur Grp'.
  IT_EXCEL-STAWN = 'Comm/Imp Code No.'.
  IT_EXCEL-HERKL = 'Cnt'.
  IT_EXCEL-DISPO = 'MRP'.
  IT_EXCEL-DISLS = 'Lot'.
  IT_EXCEL-BSTRF = 'R Vaule'.
  IT_EXCEL-BESKZ = 'Proc T'.
  IT_EXCEL-RGEKZ = 'B/F'.
  IT_EXCEL-FABKZ =  'JIT D Sch'.
  IT_EXCEL-LGPRO = 'Iss Stor'.
  IT_EXCEL-VSPVB = 'Supl Area'.
  IT_EXCEL-LGFSB = 'Stor(EP)'.
  IT_EXCEL-MRPPP = 'Pl Calendar'.
  IT_EXCEL-PLIFZ = 'Pl Del T'.
  IT_EXCEL-LTKZA = 'S Removal'.
  IT_EXCEL-LTKZE = 'S Place'.
  IT_EXCEL-RAUBE = 'Shop'.
  IT_EXCEL-TEMPB = 'B/F Cyc'.
  IT_EXCEL-XMCNG = 'Neg S'.
  IT_EXCEL-BKLAS = 'Val Class'.
  IT_EXCEL-KORDB = 'Src list'.
  IT_EXCEL-VDATU = 'Valid From'.
  IT_EXCEL-BDATU = 'Valid To'.
  IT_EXCEL-VRTYP = 'Doc Cat'.
  IT_EXCEL-EBELN_SOURCE = 'SA No'.
  IT_EXCEL-FEBEL_SOURCE = 'Fix'.
  IT_EXCEL-AUTET = 'S Usage'.
  IT_EXCEL-LIFNR_SOURCE = 'Vendor(S)'.
  IT_EXCEL-FLIFN_SOURCE = 'Fix V'.
  IT_EXCEL-LOEKZ_INFO = 'Del'.
  IT_EXCEL-LIFNR_INFO = 'Vendor(SA)'.
  IT_EXCEL-PRDAT_INFO = 'Valid to'.
  IT_EXCEL-ESOKZ_INFO = 'item Cat'.
  IT_EXCEL-NETPR =  'Net Price'.
  IT_EXCEL-BSTAE_INFO = 'Conf Ctl'.
  IT_EXCEL-MWSKZ_INFO = 'Tax'.
  IT_EXCEL-WEBRE_INFO = 'GR-b Inv'.
  IT_EXCEL-XERSN_INFO = 'ERS'.
  IT_EXCEL-KTMNG_SA = 'Target QTY'.
  IT_EXCEL-UEBTK_SA = 'O Del'.
  IT_EXCEL-ABUEB_SA = 'Cr profile'.
  IT_EXCEL-FABKZ_SA = 'JIT'.
  IT_EXCEL-BSTAE_SA = 'Conf Ctl'.
  IT_EXCEL-BSTYP_SA = 'Doc Cat'.
  IT_EXCEL-BSART_SA = 'Doc T'.
  IT_EXCEL-LOEKZ_SA = 'Del'.
  IT_EXCEL-LGNUM = 'Warehouse'.
  IT_EXCEL-LGTYP = 'Stor Type'.
  IT_EXCEL-LGPLA = 'Stor B'.
  IT_EXCEL-STLAN = 'BOM Usage'.
  IT_EXCEL-STLNR = 'BOM No'.
  IT_EXCEL-STATUS_BASIC = 'Basic'.
  IT_EXCEL-STATUS_PLANT_ST1 = 'Pl/1'.
  IT_EXCEL-STATUS_PLANT_ST2 = 'Pl/2'.
  IT_EXCEL-STATUS_PURCHASE = 'Purch'.
  IT_EXCEL-STATUS_IMPORT = 'Imp'.
  IT_EXCEL-STATUS_MRP1 = 'MRP1'.
  IT_EXCEL-STATUS_MRP2 = 'MRP2'.
  IT_EXCEL-STATUS_MRP4 = 'MRP4'.
  IT_EXCEL-STATUS_WH = 'WH'.
  IT_EXCEL-STATUS_ACCOUNT = 'Acc'.
  IT_EXCEL-STATUS_COST = 'Cost'.
  IT_EXCEL-STATUS_SOURCE = 'Src L'.
  IT_EXCEL-STATUS_INFOREC = 'InfoR'.
  IT_EXCEL-STATUS_SA = 'SA'.
  IT_EXCEL-STATUS_CC = 'CC'.
  IT_EXCEL-STATUS_BOM = 'BOM'.
  IT_EXCEL-DOWNLOAD = 'DW'.
  INSERT  IT_EXCEL INDEX 2     .
  CLEAR: IT_EXCEL.
  INSERT       IT_EXCEL INDEX 3    .
  PERFORM GET_WINDOWS_CLIFILE USING    ',*.xls.'
                              CHANGING WA_FILENAME.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = WA_FILENAME
            FILETYPE = 'DAT'
       TABLES
            DATA_TAB = IT_EXCEL.
  IF SY-SUBRC = 0.
    MESSAGE S009 WITH 'Successfully Downloaded'.
  ELSE.
    MESSAGE E009 WITH 'Unsuccessfully Downloaded'.
  ENDIF.
ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  GET_WINDOWS_CLIFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8668   text
*      <--P_WA_FILENAME  text
*----------------------------------------------------------------------*
FORM GET_WINDOWS_CLIFILE USING MASK CHANGING
                         CLIFILE    LIKE RLGRAP-FILENAME .
  DATA WINSYS(3).
  DATA TMP_CLIFILE    LIKE RLGRAP-FILENAME .

  IF CLIFILE IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD TMP_CLIFILE.
    IF SY-SUBRC NE 0.CLEAR  TMP_CLIFILE.ENDIF.
  ELSE.
    TMP_CLIFILE =  CLIFILE.
  ENDIF.
  CALL FUNCTION 'WS_QUERY'
       EXPORTING
            QUERY  = 'WS'
       IMPORTING
            RETURN = WINSYS.

  IF WINSYS(2) NE 'WN'.
    MESSAGE E016(14).
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
       DEF_FILENAME    = TMP_CLIFILE
       DEF_PATH         = TMP_CLIFILE
       MASK             = MASK
       MODE             = 'S'
       TITLE            = SY-TITLE
    IMPORTING
*ESO 11.04.01 d?ut de correction
       FILENAME         = TMP_CLIFILE
*       CLIFILE         = TMP_CLIFILE
*ESO 11.04.01 fin de correction de correction
*       RC               = RC
      EXCEPTIONS
         INV_WINSYS       = 1
         NO_BATCH         = 2
         SELECTION_CANCEL = 3
         SELECTION_ERROR  = 4
         OTHERS           = 5.

  IF SY-SUBRC EQ 0.
    CLIFILE = TMP_CLIFILE.
  ENDIF.
ENDFORM.                               " GET_WINDOWS_CLIFILE
