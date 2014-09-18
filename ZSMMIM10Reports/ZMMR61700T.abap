*&------------------------------------------------------------------
*& Program ID     : ZMMR61700
*& Profram Name   : Create Control Cycles automatically
*& Created by     : KKS
*& Created on     : 20.02.2008
*& Development ID : MM-047
*& Reference Pgm. : ZMMR60200
*& Description    : Create Control Cycles automatically
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID   Description
*& 20.02.2008   KKS                        First development
*& 19.04.2011   PAUL                       COPY
*&--------------------------------------------------------------------

REPORT  ZMMR61700T                                .

*===============================================================*
* Data definition                                               *
*===============================================================*
*-----------------------------------------*
* Include                                 *
*-----------------------------------------*
INCLUDE ZMMITOP01.                             "Global TOP
INCLUDE ZMMR61700_CLS.
*INCLUDE ZMMR61700_CLS2.
INCLUDE <ICON>.
TYPE-POOLS: PDT.
*-----------------------------------------*
* Local class                             *
*-----------------------------------------*
DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
*DATA : event_receiver2 TYPE REF TO lcl_event_receiver2.
*-----------------------------------------*
* Table definition                        *
*-----------------------------------------*
TABLES:
    MARA,
    MARC,
    MARD,
    MAKT,
    PKHD,
    PVKT,
    RMPKR.

*-----------------------------------------*
* data processing                         *
*-----------------------------------------*
* temp
TYPES : BEGIN OF TY_MARA,
         MATNR  TYPE  MATNR,
         WERKS  TYPE  WERKS_D,
         LGFSB  TYPE  MARC-LGFSB,
*         inslc  TYPE  oig_inslc, TYPE OIG_INSLC IS NOT EXIST
         INSLC  TYPE  BF_GROUP,
         LGPRO  TYPE  MARC-LGPRO,
         DISPO  TYPE  DISPO,
         PRVBE  TYPE  PRVBE,
         MATKL  TYPE  MATKL,
         MEINS  TYPE  MEINS,
         NORMT  TYPE  NORMT,
         TEMPB  TYPE  TEMPB,
         RMATP  TYPE  PL_RMATP.
TYPES : END OF TY_MARA.

DATA : GS_MARA TYPE TY_MARA.
DATA : GT_MARA TYPE TABLE OF TY_MARA.

* temp
DATA : BEGIN OF GT_PKHD OCCURS 0.
        INCLUDE STRUCTURE PKHD.
DATA :  NORMT      TYPE NORMT,
        MAKTX      TYPE MAKT-MAKTX,
        MATKL      TYPE MARA-MATKL,
        DISPO      TYPE MARC-DISPO,
        LGORT      TYPE LGORT_D,
        PVBTX      TYPE PVKT-PVBTX,    "supply area text
        ZFEEDNM    TYPE ZMMT0087-ZFEEDNM, "Feeder Name
        PIID       TYPE PL_PIID,
        LINE,
        MARK,
        ICON       TYPE ICON-ID,
        MSG(100),
       END OF GT_PKHD.

* main list
DATA : BEGIN OF GT_LIST OCCURS 0.
        INCLUDE STRUCTURE GT_PKHD.
DATA :  CELLTAB    TYPE LVC_T_STYL,
       END OF GT_LIST.
DATA : GS_LIST LIKE GT_LIST.

** copy line
*DATA : it_copy LIKE TABLE OF gt_pkhd WITH HEADER LINE.


RANGES: R_TEMPB FOR MARA-TEMPB,
        R_DISLS FOR MARC-DISLS,
        R_EPRIO FOR MARC-EPRIO.

* Macro
DEFINE SET_RANGES.
  &1-SIGN   = &2.
  &1-OPTION = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  APPEND &1.
END-OF-DEFINITION.

* f4 entry variant
TABLES : CUVTAB, CUVTAB_VALC, CABN, CABNT, CAWN, CAWNT, TSRTB.
DATA : TABLE_FIELDS     LIKE TABLE OF CUVTAB_FLD  WITH HEADER LINE.
DATA : ENTRIES_CHAR     LIKE TABLE OF CUVTAB_VALC WITH HEADER LINE.
DATA : ENTRIES_NON_CHAR LIKE TABLE OF CUVTAB_VALN.
DATA : FIELD_TAB        LIKE TABLE OF DFIES WITH HEADER LINE.
DATA : DYNPFLD_MAPPING  LIKE TABLE OF DSELC WITH HEADER LINE.
DATA : RETURN_TAB       LIKE TABLE OF DDSHRETVAL WITH HEADER LINE.

DATA : BEGIN OF F4_TAB OCCURS 0,
        F001   TYPE CHAR20,
        F002   TYPE CHAR20,
        F003   TYPE CHAR20,
        F004   TYPE CHAR20,
        F005   TYPE CHAR20,
        F006   TYPE CHAR20,
        F007   TYPE CHAR20,
        F008   TYPE CHAR20,
        F009   TYPE CHAR20,
        F010   TYPE CHAR20,
        F011   TYPE CHAR20,
        F012   TYPE CHAR20,
        F013   TYPE CHAR20,
        F014   TYPE CHAR20,
        F015   TYPE CHAR20,
       END OF F4_TAB.

DATA: F4_ATWRT LIKE TABLE OF F4_TAB WITH HEADER LINE.
DATA: F4_KCPRF LIKE TABLE OF TPKPT  WITH HEADER LINE.

* upload file info
DATA : FILENAME         TYPE RLGRAP-FILENAME,
       FILEFILTER       TYPE STRING,
       PATH             TYPE STRING,
       FULLPATH         TYPE STRING,
       USER_ACTION      TYPE I.
*       TYPE 'ABAP_ENCODING IS NOT USE 4.6 4/19/11
*       file_encoding    TYPE abap_encoding.
DATA : XLSDATA LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF IT_EXCEL OCCURS 0,
         MATNR      TYPE  PKHD-MATNR,      "Material
         WERKS      TYPE  PKHD-WERKS,      "Plant
         PRVBE      TYPE  PKHD-PRVBE,      "Supply Area
         RKSTA      TYPE  PKHD-RKSTA,      "Control Cycle Category
         BEHAZ      TYPE  PKHD-BEHAZ,      "Number of kanban containers
         ABLAD      TYPE  PKHD-ABLAD,      "Storing position
         SIGAZ      TYPE  PKHD-SIGAZ,      "maximum empty
         UMLGO      TYPE  PKHD-UMLGO,      "Storage Location
         KWBZM      TYPE  PKHD-KWBZM,      "Replenishment Lead Time
         ZZEISBE    TYPE  PKHD-ZZEISBE,    "Safety Stock
         ZZTIM      TYPE  PKHD-ZZTIM,      "Safety Time
        END OF IT_EXCEL.

DATA : BEGIN OF EXCEL_DOWN OCCURS 0,
         MATNR(20),     "TYPE  pkhd-matnr,       "Material
         WERKS(20),     "TYPE  pkhd-werks,       "Plant
         PRVBE(20),     "TYPE  pkhd-prvbe,       "Supply Area
         RKSTA(20),     "TYPE  pkhd-rksta,       "Control Cycle Category
         BEHAZ(20),     "TYPE  pkhd-behaz,       "Number of kanban
"containers
         ABLAD(20),     "TYPE  pkhd-ablad,       "Storing position
         SIGAZ(20),     "TYPE  pkhd-sigaz,       "maximum empty
         UMLGO(20),     "TYPE  pkhd-umlgo,       "Storage Location
         KWBZM(20),     "type  pkhd-kwbzm,       "Replenishment Lead
"Time
         ZZEISBE(20),   "type  pkhd-zzeisbe,     "Safety Stock
         ZZTIM(20),     "type  pkhd-zztim,       "Safety Time
        END OF EXCEL_DOWN.

*** message in screen
DATA : GS_CLASS_MSG    TYPE LVC_S_MSG1.
DATA : ERROR_IN_DATA ,
      L_SCROLL     TYPE LVC_S_STBL.

* constants
DATA : ICON_GRE TYPE ICON-ID VALUE '@5B@',
       ICON_RED TYPE ICON-ID VALUE '@5C@',
       ICON_YEL TYPE ICON-ID VALUE '@5D@',
       ICON_DEL TYPE ICON-ID VALUE '@11@',  "dummy icon
       ICON_POS TYPE ICON-ID VALUE '@P8@'.  "ICON_RATING_POSITIVE

CONSTANTS :
    C_DATUM  TYPE DATUM VALUE '99991231'.

*===============================================================*
* Selection screen                                              *
*===============================================================*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-CAT.
PARAMETERS:
  P_KANBAN RADIOBUTTON GROUP CAT,
  P_CLASS  RADIOBUTTON GROUP CAT.
**  P_SUMJC  RADIOBUTTON GROUP CAT,
**  P_RESERV RADIOBUTTON GROUP CAT.
SELECTION-SCREEN END OF BLOCK BLOCK1.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-B01.
PARAMETERS :
*    p_werks  TYPE pkhd-werks OBLIGATORY MEMORY ID wrk DEFAULT 'KVA1'.
    P_WERKS  TYPE PKHD-WERKS OBLIGATORY MEMORY ID WRK DEFAULT 'P001'.
SELECT-OPTIONS :
    S_MTART  FOR MARA-MTART DEFAULT 'ROH',
    S_MATNR  FOR PKHD-MATNR,
    S_FERTH  FOR MARA-FERTH,
    S_MATKL  FOR MARA-MATKL,
    S_DISPO  FOR MARC-DISPO.

PARAMETERS :
    P_MODE  TYPE CTU_MODE DEFAULT 'N'.

SELECTION-SCREEN END OF BLOCK BLOCK2.

AT SELECTION-SCREEN OUTPUT.

*===============================================================*
* Events                                                        *
*===============================================================*
INITIALIZATION.
  G_REPID = SY-REPID.
  G_MODE  = 'D'.        "display

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM RUN.

END-OF-SELECTION.
  IF GT_LIST[] IS INITIAL.
    MESSAGE S429(MO). "No table entries found for specified key
  ELSE.
    CALL SCREEN 100.
  ENDIF.

*===============================================================*
* Subroutine                                                    *
*===============================================================*
*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       program run~
*----------------------------------------------------------------------*
FORM RUN .

  PERFORM READ_MATERIAL_MASTER.

  PERFORM READ_CONTROL_CYCLES.

* Compare matrial master with control cycle
  SORT GT_PKHD BY WERKS MATNR.
  SORT GT_MARA BY WERKS MATNR.
  LOOP AT GT_PKHD.
    READ TABLE GT_MARA INTO GS_MARA WITH KEY WERKS = GT_PKHD-WERKS
                                             MATNR = GT_PKHD-MATNR
                                             PRVBE = GT_PKHD-PRVBE
                                             BINARY SEARCH.
    CHECK SY-SUBRC EQ 0.
    DELETE GT_MARA INDEX SY-TABIX.
  ENDLOOP.

*  SORT GT_PKHD BY WERKS NORMT MATNR PKNUM.
*  SORT GT_MARA BY WERKS NORMT MATNR.
  SORT GT_PKHD BY WERKS MATNR .
  SORT GT_MARA BY WERKS MATNR .

*-- Read first PGN, first Control cycle number.
  LOOP AT GT_MARA INTO GS_MARA.
    CLEAR GS_LIST.
*    IF gs_mara-normt IS INITIAL.
*      PERFORM create_item.
*    ELSE.
    READ TABLE GT_PKHD WITH KEY WERKS    = GS_MARA-WERKS
                                MATNR(5) = GS_MARA-MATNR(5)
*                                PRVBE    = GS_MARA-PRVBE
*                                  normt = gs_mara-normt
                                BINARY SEARCH.
    IF  SY-SUBRC EQ 0.
**S__ PAUL : IF already exist same material No. in pkhd pass it.
      SELECT SINGLE *
        FROM PKHD
       WHERE MATNR EQ GS_MARA-MATNR
         AND WERKS EQ GS_MARA-WERKS.

      IF SY-SUBRC = 0.
        CONTINUE.
      ENDIF.
**E__< 06/06/11
      MOVE-CORRESPONDING GT_PKHD TO GS_LIST.
      GS_LIST-MATNR = GS_MARA-MATNR.
      MOVE SPACE         TO GS_LIST-PKNUM.
      GS_LIST-LGORT = GS_LIST-UMLGO.
*   Set Control Cycle Category
      IF GS_LIST-RKSTA  = 'K'.  "Classic KANBAN
        GS_LIST-BEHAZ  = '3'.  "Number of kanban containers
      ENDIF.
*   Get Feeder Name
      PERFORM GET_FEEDER_NAME.
    ELSE.
      PERFORM CREATE_ITEM.
    ENDIF.
*  ENDIF.
*   Check Packing Instruction.
    PERFORM FIND_PACKOBJ USING    GS_MARA-MATNR GS_MARA-RMATP
                         CHANGING GS_LIST-PACKV
                                  GS_LIST-PIID.
    PERFORM GET_PI_INFO USING    GS_LIST-PACKV
                        CHANGING GS_LIST-BEHMG
                                 GS_LIST-PKBHT.
    IF GS_LIST-PIID IS INITIAL.
      GS_LIST-ICON = ICON_RED.
**Paul Delete Message about "Packing" 06/23/11
**      MESSAGE ID 'VHU01' TYPE 'I' NUMBER '098'
**               INTO  GS_LIST-MSG.
    ENDIF.
*   Find Scheduling Agreement
**    IF P_SUMJC EQ 'X'.
**      PERFORM FIND_SA USING    GS_LIST-MATNR
**                               GS_LIST-WERKS
**                      CHANGING GS_LIST-EBELN
**                               GS_LIST-EBELP.
**      IF GS_LIST-EBELP IS INITIAL.
**        GS_LIST-ICON = ICON_RED.
**        MESSAGE ID 'PK' TYPE 'I' NUMBER '071'
**                 INTO  GS_LIST-MSG.
**      ENDIF.
**    ENDIF.
*  text
    PERFORM READ_TEXT USING     'MAKTX'
                                GS_LIST-MATNR
                      CHANGING  GS_LIST-MAKTX.
    PERFORM READ_TEXT USING     'PVBTX'
                                GS_LIST-PRVBE
                      CHANGING  GS_LIST-PVBTX.
    IF GS_LIST-ICON IS INITIAL.
      GS_LIST-ICON = ICON_YEL.
    ENDIF.
*S__ by Paul
    IF GS_LIST-WERKS NE 'P001'.
      SELECT SINGLE LGFSB
        INTO GS_LIST-LGORT
        FROM MARC
       WHERE MATNR EQ GS_LIST-MATNR
         AND WERKS EQ 'P001'.
    ENDIF.
*
    APPEND GS_LIST TO GT_LIST.

  ENDLOOP.

ENDFORM.                    " run
*&---------------------------------------------------------------------*
*&      Form  read_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM READ_TEXT  USING    U_ID
                         U_VAL
                CHANGING C_TXT.

  CASE U_ID.
    WHEN 'MAKTX'.
      SELECT SINGLE MAKTX
        FROM MAKT
        INTO C_TXT
       WHERE MATNR = U_VAL
         AND SPRAS = SY-LANGU.

    WHEN 'PVBTX'.
      SELECT SINGLE PVBTX
        FROM PVKT
        INTO C_TXT
       WHERE WERKS = P_WERKS
         AND PRVBE = U_VAL
         AND SPRAS = SY-LANGU.

    WHEN 'CABNT'.
      SELECT SINGLE ATBEZ
        FROM CABNT
        INTO C_TXT
       WHERE ATINN = U_VAL
         AND SPRAS = SY-LANGU.

  ENDCASE.

ENDFORM.                    " read_text
*&---------------------------------------------------------------------*
*&      Form  handle_data_changed
*&---------------------------------------------------------------------*
*       check changed cell
*----------------------------------------------------------------------*
FORM HANDLE_DATA_CHANGED  USING  U_CHANGED
                          TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  DATA : LS_MOD_CELLS TYPE LVC_S_MODI,
         LS_CELLS     TYPE LVC_S_MODI,
         L_LINE       TYPE SY-TFILL.

  LOOP AT U_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
    PERFORM ASSIGN_CHANGED_FIELD USING U_CHANGED
                                       LS_MOD_CELLS-FIELDNAME
                                       LS_MOD_CELLS-ROW_ID.
    CLEAR : GS_CLASS_MSG, ERROR_IN_DATA .

    CASE LS_MOD_CELLS-FIELDNAME.
*      WHEN 'MATNR'.
*        PERFORM input_check_material.
      WHEN 'PRVBE'.
        PERFORM INPUT_CHECK_SUPPLY_AREA.

      WHEN 'LGORT'.  "'UMLGO'.
        PERFORM INPUT_CHECK_LOCATION.

      WHEN 'RKSTA'.
        PERFORM INPUT_CHECK_STATUS.

      WHEN 'KWBZM'.      "Replenishment Lead Time in Hours:Minutes

      WHEN 'LOGGR'.

    ENDCASE.
    IF ERROR_IN_DATA EQ 'X'.
      CALL METHOD U_CHANGED->ADD_PROTOCOL_ENTRY
        EXPORTING
          I_MSGID     = GS_CLASS_MSG-MSGID
          I_MSGNO     = GS_CLASS_MSG-MSGNO
          I_MSGTY     = 'E'
          I_MSGV1     = GS_CLASS_MSG-MSGV1
          I_MSGV2     = GS_CLASS_MSG-MSGV2
          I_MSGV3     = GS_CLASS_MSG-MSGV3
          I_FIELDNAME = LS_MOD_CELLS-FIELDNAME
          I_ROW_ID    = LS_MOD_CELLS-ROW_ID.
    ELSE.
*      IF gt_list-line = 'C'.
*        gt_list-line = 'S'.
*      ENDIF.
      MODIFY GT_LIST INDEX LS_MOD_CELLS-ROW_ID.
    ENDIF.
  ENDLOOP.
  IF ERROR_IN_DATA EQ 'X'.
    CALL METHOD U_CHANGED->DISPLAY_PROTOCOL.
  ELSE.
    PERFORM GUI_ALV_CELL_CONTROL.
    PERFORM GUI_ALV_REFRESH.
*  call method u_changed->MODIFY_PROTOCOL_ENTRY
*    EXPORTING
*      it_msg = u_changed->mt_protocol.
  ENDIF.


ENDFORM.                    " handle_data_changed
*&---------------------------------------------------------------------*
*&      Form  handle_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_USER_COMMAND  USING    P_E_UCOMM.

ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  handle_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HANDLE_TOOLBAR  USING    U_OBJECT
                              U_INTERACTIVE.


ENDFORM.                    " handle_toolbar
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HOTSPOT_CLICK  USING    U_ROW
                             U_COLUMN.
  READ TABLE GT_LIST INDEX U_ROW.
  CASE U_COLUMN.
    WHEN 'PKNUM'.
*      CHECK gt_list-pknum IS NOT INITIAL.
      CHECK GT_LIST-PKNUM <> ''.
      PERFORM LINK_CONTROL_CYCLE USING GT_LIST-PKNUM.
  ENDCASE.
ENDFORM.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA : L_TFILL(5).

  DESCRIBE TABLE GT_LIST LINES L_TFILL.
  IF     P_KANBAN EQ 'X'.
    SET TITLEBAR '100' WITH 'KANBAN' L_TFILL.
*  ELSEIF P_SUMJC  EQ 'X'.
*    SET TITLEBAR '100' WITH 'JIT Call' L_TFILL.
*  ELSEIF P_RESERV EQ 'X'.
*    SET TITLEBAR '100' WITH 'Replenishment Order' L_TFILL.
  else.
    SET TITLEBAR '100' WITH 'CLASSIC KANBAN' L_TFILL.
  ENDIF.
  SET PF-STATUS '100'.
ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_SCREEN OUTPUT.
  IF G_CUSTOM_CONTAINER1 IS INITIAL.
    PERFORM GUI_ALV_CREATE.
    PERFORM GUI_ALV_CONTENT.
    PERFORM GUI_ALV_SORT.
    PERFORM GUI_ALV_CELL_CONTROL.
    PERFORM GUI_ALV_DISPLAY.
    PERFORM GUI_ALV_EVENT.
  ELSE.
    PERFORM GUI_ALV_REFRESH.
  ENDIF.
  PERFORM SET_FOCUS.

ENDMODULE.                 " init_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  gui_alv_create
*&---------------------------------------------------------------------*
*       create screen object
*----------------------------------------------------------------------*
FORM GUI_ALV_CREATE .
  CREATE OBJECT G_CUSTOM_CONTAINER1
    EXPORTING
      CONTAINER_NAME = G_CONTAINER1.

  CREATE OBJECT G_GRID1
    EXPORTING
      I_PARENT = G_CUSTOM_CONTAINER1.
ENDFORM.                    " gui_alv_create
*&---------------------------------------------------------------------*
*&      Form  gui_alv_content
*&---------------------------------------------------------------------*
*       fill screen fieldcatalog
*
*  Selection modes for SEL_MODE
* 'A' : Column and row selection
* 'B' : Simple selection, list box
* 'C' : Multiple selection, list box
* 'D' : Cell selection
*----------------------------------------------------------------------*
FORM GUI_ALV_CONTENT .
**** layout ****
  CLEAR GS_FCATLAYO.
  GS_FCATLAYO-ZEBRA                = 'X'.
  GS_FCATLAYO-BOX_FNAME            = 'MARK'.
  GS_FCATLAYO-SEL_MODE             = 'A'.
  GS_FCATLAYO-NO_TOOLBAR           = 'X'.
  GS_FCATLAYO-STYLEFNAME           = 'CELLTAB'.
  GS_FCATLAYO-EDIT                 = 'X'.

**** FIELD CATALOG ****
  DATA : COL_POS TYPE I.
  DATA : L_FIELDCATALOG  TYPE LVC_S_FCAT.
  DATA : L_TXT(20).
  REFRESH IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ICON'.
  L_FIELDCATALOG-ICON              = 'X'.
  L_FIELDCATALOG-REPTEXT           = 'St.'.
  L_FIELDCATALOG-OUTPUTLEN         = 2.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MATNR'.
  L_FIELDCATALOG-REF_TABLE         = 'MARD'.
  L_FIELDCATALOG-OUTPUTLEN         = 16.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'WERKS'.
  L_FIELDCATALOG-REF_TABLE         = 'MARD'.
  L_FIELDCATALOG-KEY               = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.
*  IF P_KANBAN EQ 'X'.
*     OR P_RESERV EQ 'X'.
  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'LGORT'.
  L_FIELDCATALOG-REF_TABLE         = 'MARD'.
  L_FIELDCATALOG-KEY               = 'X'.
  L_FIELDCATALOG-CHECKTABLE        = '!'.  "do not check foreign keys
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.
*  ENDIF.
  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MAKTX'.
  L_FIELDCATALOG-REF_TABLE         = 'MAKT'.
  L_FIELDCATALOG-OUTPUTLEN         = 26.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

*  CLEAR L_FIELDCATALOG.
*  COL_POS = COL_POS + 1.
*  L_FIELDCATALOG-COL_POS           = COL_POS.
*  L_FIELDCATALOG-FIELDNAME         = 'NORMT'.
*  L_FIELDCATALOG-REF_TABLE         = 'MARA'.
*  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MATKL'.
  L_FIELDCATALOG-REF_TABLE         = 'MARA'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'DISPO'.
  L_FIELDCATALOG-REF_TABLE         = 'MARC'.
  L_FIELDCATALOG-NO_OUT            = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'PRVBE'.
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-CHECKTABLE        = '!'.  "do not check foreign keys
  L_FIELDCATALOG-OUTPUTLEN         = 5.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'PVBTX'.
  L_FIELDCATALOG-REF_TABLE         = 'PVKT'.
  L_FIELDCATALOG-OUTPUTLEN         = 16.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'PKNUM'.
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
* l_fieldcatalog-key               = 'X'.
  L_FIELDCATALOG-HOTSPOT           = 'X'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'RKSTA'.
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
* l_fieldcatalog-key               = 'X'.
  L_FIELDCATALOG-OUTPUTLEN         = 2.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ABLAD'.   "Storing position
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 16.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

**  IF P_SUMJC EQ 'X'.
**    CLEAR L_FIELDCATALOG.
**    COL_POS = COL_POS + 1.
**    L_FIELDCATALOG-COL_POS           = COL_POS.
**    L_FIELDCATALOG-FIELDNAME         = 'EBELN'.   "Agreement
**    L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
**    APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.
**    CLEAR L_FIELDCATALOG.
**    COL_POS = COL_POS + 1.
**    L_FIELDCATALOG-COL_POS           = COL_POS.
**    L_FIELDCATALOG-FIELDNAME         = 'EBELP'.   "Agreement Item
**    L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
**    APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.
**  ELSE.
  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ZFEEDER'.   "Feeder
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 5.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ZFEEDNM'.   "Feeder NM
  L_FIELDCATALOG-REF_TABLE         = 'ZMMT0087'.
  L_FIELDCATALOG-OUTPUTLEN         = 16.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ZRHLH'.   "RH/LH
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 4.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ZZFSTP'.   "STOP
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 3.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'BEHAZ'.   "Number of kanban
  "containers
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 3.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'SIGAZ'.      "Maximum Number
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.       "Empty Containers
  L_FIELDCATALOG-OUTPUTLEN         = 3.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.
**  ENDIF.
  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'KWBZM'.      "RLT
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 8.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ZZTIM'.      "Safety Time
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 8.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'ZZEISBE'.
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-QFIELDNAME        = 'MEINS'.
  L_FIELDCATALOG-NO_ZERO           = 'X'.
  L_FIELDCATALOG-OUTPUTLEN         = 10.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MEINS'.
  L_FIELDCATALOG-REF_TABLE         = 'MARA'.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'PKBHT'.
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-OUTPUTLEN         = 5.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'BEHMG'.
  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
  L_FIELDCATALOG-QFIELDNAME        = 'MEINS'.
  L_FIELDCATALOG-NO_ZERO           = 'X'.
  L_FIELDCATALOG-OUTPUTLEN         = 10.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

*  CLEAR L_FIELDCATALOG.
*  COL_POS = COL_POS + 1.
*  L_FIELDCATALOG-COL_POS           = COL_POS.
*  L_FIELDCATALOG-FIELDNAME         = 'PIID'.
*  L_FIELDCATALOG-REF_FIELD         = 'PACKV'.
*  L_FIELDCATALOG-REF_TABLE         = 'PKHD'.
*  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

  CLEAR L_FIELDCATALOG.
  COL_POS = COL_POS + 1.
  L_FIELDCATALOG-COL_POS           = COL_POS.
  L_FIELDCATALOG-FIELDNAME         = 'MSG'.
  L_FIELDCATALOG-REPTEXT           = 'Result'.
  L_FIELDCATALOG-OUTPUTLEN         = 45.
  APPEND L_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.                    " gui_alv_content
*&---------------------------------------------------------------------*
*&      Form  gui_alv_display
*&---------------------------------------------------------------------*
*       screen display
*----------------------------------------------------------------------*
FORM GUI_ALV_DISPLAY .
  CALL METHOD G_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = GS_FCATLAYO
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE
    CHANGING
      IT_OUTTAB            = GT_LIST[]
      IT_FIELDCATALOG      = IT_FIELDCATALOG
      IT_SORT              = GT_SORT.

ENDFORM.                    " gui_alv_display
*&---------------------------------------------------------------------*
*&      Form  gui_alv_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GUI_ALV_EVENT .
  CALL METHOD G_GRID1->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.
  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR G_GRID1.
  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR G_GRID1.
*  CREATE OBJECT event_receiver2.
*  SET HANDLER event_receiver2->handle_f4            FOR g_grid1.

*  PERFORM f4_field_assign.
ENDFORM.                    " gui_alv_event
*&---------------------------------------------------------------------*
*&      Form  gui_alv_refresh
*&---------------------------------------------------------------------*
*       screen data refresh
*----------------------------------------------------------------------*
FORM GUI_ALV_REFRESH .
  DATA : ROW_NO      TYPE LVC_S_ROID,
         ROW_INFO    TYPE LVC_S_ROW,
         COL_INFO    TYPE LVC_S_COL.

  CHECK NOT G_GRID1 IS INITIAL.

  L_SCROLL-ROW = 'X'.
  L_SCROLL-COL = 'X'.

  CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      I_SOFT_REFRESH = 'X'
      IS_STABLE      = L_SCROLL.     "?? ??? ??? refresh


*  CALL METHOD g_grid1->get_scroll_info_via_id
*    IMPORTING
*      es_row_no   = row_no
*      es_row_info = row_info
*      es_col_info = col_info.
*
*  CALL METHOD g_grid1->refresh_table_display.
*
*  CALL METHOD g_grid1->set_scroll_info_via_id
*    EXPORTING
*      is_row_info = row_info
*      is_col_info = col_info
*      is_row_no   = row_no.

ENDFORM.                    " gui_alv_refresh
*&---------------------------------------------------------------------*
*&      Form  set_focus
*&---------------------------------------------------------------------*
*       screen focus
*----------------------------------------------------------------------*
FORM SET_FOCUS .

  CHECK NOT G_GRID1 IS INITIAL.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
    EXPORTING
      CONTROL = G_GRID1.
ENDFORM.                    " set_focus
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND INPUT.
  PERFORM USER_COMMAND.
ENDMODULE.                 " USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND .
  DATA : L_CODE TYPE SY-UCOMM.
  L_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE L_CODE.
    WHEN 'SAVE'.
      CALL METHOD G_GRID1->CHECK_CHANGED_DATA.  "changed data
      PERFORM GET_SELECTED_ROWS.
      PERFORM USER_COMMAND_SAVE.
*    WHEN 'REFR'.
*      g_mode = 'D'.
*      PERFORM run.
*      PERFORM gui_alv_cell_control.
**      PERFORM gui_alv_refresh.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
*    WHEN 'DELE'.              "delete line
*      PERFORM user_command_delete.
*    WHEN 'COPY'.              "copy line
*      PERFORM user_command_copy.
*      PERFORM remove_mark_in_screen.
    WHEN 'TOGL'.              "display<->change
      PERFORM USER_COMMAND_TOGGLE.
      PERFORM GUI_ALV_CELL_CONTROL.
*    WHEN 'NEW'.               "new line
*      PERFORM user_command_new.
*      PERFORM gui_alv_cell_control.
*    WHEN 'PASTE'.
*      PERFORM user_command_paste.
*    WHEN 'ENTR'.
*      PERFORM user_command_enter.
*      LEAVE TO SCREEN 0.
    WHEN 'STUP'.              "sort up
      PERFORM USER_COMMAND_SORT USING 'U'.
    WHEN 'STDN'.              "sort down
      PERFORM USER_COMMAND_SORT USING 'D'.
*    WHEN 'UPLOAD'.
*      PERFORM user_command_upload.
*      PERFORM gui_alv_cell_control.
*    WHEN 'DOWN'.               "templete download
*      PERFORM user_command_download USING 'T'.
*    WHEN 'DNLOAD'.             "screen data download
*      PERFORM user_command_download USING 'S'.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " user_command
*&---------------------------------------------------------------------*
*&      Module  exit_commnad  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMNAD INPUT.
  CASE OK_CODE.
    WHEN 'EXIT' OR 'CANC'.
*      IF sy-dynnr = '0110'.
*        LEAVE TO SCREEN 0.
*      ELSE.
      LEAVE PROGRAM.
*      ENDIF.
  ENDCASE.
ENDMODULE.                 " exit_commnad  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_SELECTED_ROWS.
  DATA : LT_ROWS TYPE LVC_T_ROW.
  DATA : L_ROW   TYPE LVC_S_ROW.

* Get Selected Rows
  CALL METHOD G_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS.

  DESCRIBE TABLE LT_ROWS.
  IF SY-TFILL = 0.
    MESSAGE E800(13) .
  ENDIF.


  LOOP AT LT_ROWS INTO L_ROW.
    READ TABLE GT_LIST INDEX L_ROW-INDEX.
    GT_LIST-MARK = 'X'.
    MODIFY GT_LIST INDEX L_ROW-INDEX.
  ENDLOOP.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  user_command_delete
*&---------------------------------------------------------------------*
*       delete selected line
*----------------------------------------------------------------------*
FORM USER_COMMAND_DELETE .
*  DATA : lt_list LIKE TABLE OF gt_pkhd WITH HEADER LINE.
*  DATA : l_answer.
*  PERFORM get_selected_rows.
*  PERFORM func_confirm_data_loss CHANGING l_answer.
*  CHECK l_answer = 'J'.
*
*  LOOP AT gt_list WHERE mark = 'X'.
*    IF gt_list-pknum IS INITIAL.          "delete from itab.
*      DELETE gt_list.
*    ELSE.                                 "delete from DB
*      REFRESH : bdcdata, it_message.
*      CLEAR : bdcdata, it_message.
*
*      PERFORM bdc_delete USING gt_list-werks
*                               gt_list-pknum.
*      PERFORM bdc_return CHANGING gt_list-icon
*                                  gt_list-msg.
*      IF gt_list-icon = icon_red.
*        gt_list-icon = gt_list-icon.
*        gt_list-msg  = gt_list-msg.
*      ELSE.
**        gt_list-line = 'D'.
**        gt_list-icon = icon_del.
*      ENDIF.
*      MODIFY gt_list .
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " user_command_delete
*&---------------------------------------------------------------------*
*&      Form  func_confirm_data_loss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FUNC_CONFIRM_DATA_LOSS  CHANGING C_ANSWER.

*  CALL FUNCTION 'POPUP_TO_CONFIRM_DATA_LOSS'
*    EXPORTING
*      defaultoption = 'J'
*      titel         = text-101
*    IMPORTING
*      answer        = c_answer.
ENDFORM.                    " func_confirm_data_loss
*&---------------------------------------------------------------------*
*&      Form  gui_alv_cell_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GUI_ALV_CELL_CONTROL .
  LOOP AT GT_LIST.
    CLEAR GT_LIST-CELLTAB.
    PERFORM FILL_CELLTAB USING    GT_LIST-LINE
                                  GT_LIST-RKSTA
                         CHANGING GT_LIST-CELLTAB[].
    MODIFY GT_LIST.
  ENDLOOP.

ENDFORM.                    " gui_alv_cell_control
*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FILL_CELLTAB  USING    U_LINE
                            U_RKSTA
                   CHANGING C_CELLTAB  TYPE LVC_T_STYL.

  DATA : L_FIELDCATALOG  TYPE LVC_S_FCAT.
  DATA : L_CELLTAB       TYPE LVC_S_STYL.

  LOOP AT IT_FIELDCATALOG INTO L_FIELDCATALOG.
    L_CELLTAB-FIELDNAME = L_FIELDCATALOG-FIELDNAME.
    CASE U_LINE.
*      WHEN 'N'.                       "New line
*        CASE l_celltab-fieldname.
*          WHEN 'MATNR' OR 'WERKS' OR 'PRVBE' OR 'ABLAD' OR
**              'UMLGO' OR
*               'KWBZM'.
*            l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
*          WHEN 'SIGAZ' OR 'BEHAZ'.
*            IF u_rksta = 'I'.         "event-driven
*              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*            ELSEIF u_rksta = 'K'.     "classical
*              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
*            ENDIF.
*          WHEN 'ZZEISBE' OR 'ZZTIM'.
*            IF u_rksta = 'I'.         "event-driven
*              l_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
*            ELSEIF u_rksta = 'K'.     "classical
*              l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*            ENDIF.
*          WHEN OTHERS.
*            l_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*        ENDCASE.
      WHEN 'C'.                       "change line
        CASE L_CELLTAB-FIELDNAME.
          WHEN 'PRVBE' OR 'LGORT'  OR  "'UMLGO' OR
               'ABLAD' OR 'KWBZM'  OR  "'PIID'   or
               'RKSTA' OR 'ZFEEDER' OR
               'BEHMG' OR 'PKBHT'  OR 'ZRHLH' OR 'ZZFSTP'.
            L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          WHEN 'SIGAZ' OR 'BEHAZ'.
            IF U_RKSTA = 'I'.         "event-driven
              L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            ELSEIF U_RKSTA = 'K'.     "classical
              L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
            ENDIF.
          WHEN 'ZZEISBE' OR 'ZZTIM'.
            IF U_RKSTA = 'I'.         "event-driven
              L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
            ELSEIF U_RKSTA = 'K'.     "classical
              L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
            ENDIF.
          WHEN OTHERS.
            L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDCASE.

      WHEN OTHERS.
        L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDCASE.
    INSERT L_CELLTAB INTO TABLE C_CELLTAB.
  ENDLOOP.
ENDFORM.                    " fill_celltab
*&---------------------------------------------------------------------*
*&      Form  user_command_toggle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_TOGGLE .

  IF G_MODE = 'D'.   "Display
    G_MODE = 'M'.    "Modify
    GT_LIST-LINE = 'C'.
    MODIFY GT_LIST TRANSPORTING LINE WHERE PKNUM EQ SPACE.
    GT_LIST-LINE = ' '.
    MODIFY GT_LIST TRANSPORTING LINE WHERE PKNUM NE SPACE.
  ELSEIF G_MODE = 'M'.
    G_MODE = 'D'.
    CLEAR GT_LIST-LINE.
    MODIFY GT_LIST TRANSPORTING LINE WHERE LINE = 'C'.
  ENDIF.

ENDFORM.                    " user_command_toggle
*&---------------------------------------------------------------------*
*&      Form  assign_changed_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ASSIGN_CHANGED_FIELD  USING  U_CHANGED
                              TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                  U_FIELDNAME
                                  U_ROW_ID.
  DATA : L_FIELD(20),
         L_VAL(20).
  FIELD-SYMBOLS : <FS1> .

  CONCATENATE 'GT_LIST-' U_FIELDNAME INTO L_FIELD.
  ASSIGN (L_FIELD)  TO <FS1>.

  READ TABLE GT_LIST INDEX U_ROW_ID.

* get cell input value
  CALL METHOD U_CHANGED->GET_CELL_VALUE
    EXPORTING
      I_ROW_ID    = U_ROW_ID
      I_FIELDNAME = U_FIELDNAME
    IMPORTING
      E_VALUE     = <FS1>.

ENDFORM.                    " assign_changed_field
*&---------------------------------------------------------------------*
*&      Form  user_command_save
*&---------------------------------------------------------------------*
*       save changed data
*****  mode (GT_LIST-line)
* S = changed control cycle
* N = new control cycle
* D = delete control cycle
* A = add ALC at existed KANBAN
*----------------------------------------------------------------------*
FORM USER_COMMAND_SAVE .
** changed data / inserted data
*  LOOP AT gt_list WHERE LINE NE 'C'
*                    AND LINE NE space.
  LOOP AT GT_LIST WHERE MARK EQ 'X'.
    REFRESH : BDCDATA, IT_MESSAGE.
    CLEAR : BDCDATA, IT_MESSAGE, GT_LIST-ICON, GT_LIST-MSG.
*    CASE gt_list-line.
*      WHEN 'S'.                   "changed data
*        PERFORM bdc_change.
*      WHEN 'N'.                   "inserted data
*        PERFORM bdc_insert_fieldcheck.
*        PERFORM bdc_insert.
*      WHEN 'A'.                   "add ALC data
*    ENDCASE.

    PERFORM BDC_INSERT_FIELDCHECK.
    PERFORM BDC_INSERT.

    PERFORM BDC_RETURN CHANGING GT_LIST-ICON
                                GT_LIST-MSG.
    IF GT_LIST-ICON = ICON_GRE.
      CLEAR : GT_LIST-LINE, GT_LIST-MARK.
    ENDIF.
    MODIFY GT_LIST.
  ENDLOOP.

  IF SY-SUBRC NE 0.
    MESSAGE S314(SLS).
  ELSE.
    G_MODE = 'D'.
*    PERFORM clear_table_after_save.
    PERFORM GUI_ALV_CELL_CONTROL.
  ENDIF.
ENDFORM.                    " user_command_save
*&---------------------------------------------------------------------*
*&      Form  bdc_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BDC_DELETE USING U_WERKS
                      U_PKNUM.
*# initial screen
  PERFORM BDC_DYNPRO_PROCESSING USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=GETD' ,
                      ' ' 'RMPKR-WERKS'              U_WERKS,
                      ' ' 'RMPKR-PRVBE'              '',
                      ' ' 'RMPKR-RGVER'              '',
                      ' ' 'RMPKR-PKNUM'              U_PKNUM.
*# toggle
  PERFORM BDC_DYNPRO_PROCESSING USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=TOGG' .

*# delete
  PERFORM BDC_DYNPRO_PROCESSING USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=DELE' .
*#. confirm
  PERFORM BDC_DYNPRO_PROCESSING USING:
                      'X' 'SAPLSPO1'                 '0300',
                      ' ' 'BDC_OKCODE'               '=YES' .
*#. save
  PERFORM BDC_DYNPRO_PROCESSING USING:
                      'X' 'SAPLMPK_CCY_UI'           '0110',
                      ' ' 'BDC_OKCODE'               '=SAVE' .

  CALL TRANSACTION 'PKMC' USING BDCDATA MODE BDC_MODE
                   MESSAGES INTO IT_MESSAGE.

ENDFORM.                    " bdc_delete
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO_PROCESSING  USING   DY_BEGIN  PG_NAME   SC_NO.
  IF DY_BEGIN = 'X'.
    CLEAR BDCDATA.
    MOVE  PG_NAME  TO BDCDATA-PROGRAM.
    MOVE  SC_NO    TO BDCDATA-DYNPRO.
    MOVE  'X'      TO BDCDATA-DYNBEGIN.
    APPEND BDCDATA.
  ELSE.
    CLEAR BDCDATA.
    MOVE  PG_NAME  TO BDCDATA-FNAM.
    MOVE  SC_NO    TO BDCDATA-FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.                    " bdc_dynpro_processing
*&---------------------------------------------------------------------*
*&      Form  bdc_return
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BDC_RETURN CHANGING C_ICON
                         C_MSG.
  READ TABLE IT_MESSAGE WITH KEY MSGTYP = 'E'.  "error
  IF SY-SUBRC = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = IT_MESSAGE-MSGID
              MSGNR               = IT_MESSAGE-MSGNR
              MSGV1               = IT_MESSAGE-MSGV1
              MSGV2               = IT_MESSAGE-MSGV2
              MSGV3               = IT_MESSAGE-MSGV3
              MSGV4               = IT_MESSAGE-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = C_MSG.

    C_ICON  = ICON_RED.

  ELSE.
    SELECT SINGLE PKNUM
      FROM PKHD
      INTO GT_LIST-PKNUM
     WHERE MATNR = GT_LIST-MATNR
       AND WERKS = GT_LIST-WERKS
       AND PRVBE = GT_LIST-PRVBE.
    IF SY-SUBRC = 0.
      C_ICON  = ICON_GRE.
    ELSE.
      C_ICON  = ICON_RED.
    ENDIF.
  ENDIF.
ENDFORM.                    " bdc_return
*&---------------------------------------------------------------------*
*&      Form  bdc_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BDC_CHANGE .
  DATA : L_KWBZM(20),
         L_BEHAZ(20),
         L_SAFETY(10),
         L_SAFETYTIME(20).
  WRITE: GT_LIST-KWBZM   TO L_KWBZM NO-ZERO,
         GT_LIST-BEHAZ   TO L_BEHAZ NO-ZERO,
         GT_LIST-ZZEISBE TO L_SAFETY,
         GT_LIST-ZZTIM   TO L_SAFETYTIME NO-ZERO.

*# initial screen
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=TOGG',
           ' ' 'RMPKR-WERKS'          GT_LIST-WERKS,
           ' ' 'RMPKR-PRVBE'          ' ',
           ' ' 'RMPKR-RGVER'          ' ',
           ' ' 'RMPKR-PKNUM'          GT_LIST-PKNUM.
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=GETD'.
*# change data
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=OK',
           ' ' 'PKHD-PRVBE'           GT_LIST-PRVBE,
           ' ' 'PKHD-ABLAD'           GT_LIST-ABLAD,
           ' ' 'PKHD-UMLGO'           GT_LIST-UMLGO,
           ' ' 'PKHD-KWBZM'           L_KWBZM. "Replenishment Lead Time
  IF GT_LIST-RKSTA = 'K'.
    PERFORM BDC_DYNPRO_PROCESSING USING:
             ' ' 'PKHD-BEHAZ'         L_BEHAZ,         "Number of KANBAN
             ' ' 'PKHD-SIGAZ'         GT_LIST-SIGAZ,   "Maximum Empty
             ' ' 'PKHD-KWBZM'         L_KWBZM.         "Replenishment
    "Lead Time
  ELSE.
    PERFORM BDC_DYNPRO_PROCESSING USING:
             ' ' 'PKHD-ZZEISBE'       L_SAFETY,        "Safety Stock
             ' ' 'PKHD-ZZTIM'         L_SAFETYTIME.    "Safety Time
  ENDIF.

*  IF gt_list-rksta NE 'M'.
  PERFORM BDC_DYNPRO_PROCESSING USING:
         ' ' 'PKHD-ZFEEDER'         GT_LIST-ZFEEDER.  "Feeder
*  ENDIF.
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=SAVE'.
  CALL TRANSACTION 'PKMC' USING BDCDATA MODE P_MODE
                   MESSAGES INTO IT_MESSAGE.
ENDFORM.                    " bdc_change
*&---------------------------------------------------------------------*
*&      Form  user_command_copy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_COPY .

*  PERFORM get_selected_rows.
*  REFRESH it_copy.
*  LOOP AT gt_list WHERE mark = 'X'.
*    CLEAR it_copy.
*    MOVE-CORRESPONDING gt_list TO it_copy.
** Control Cycle Category
*    IF gt_list-rksta   = 'K'.      "Classic KANBAN
*      it_copy-pkkla = 'X'.
*    ELSEIF gt_list-rksta   = 'I'.  "Event-driven KANBAN
*      it_copy-pkimp = 'X'.
*    ENDIF.
*
*    APPEND it_copy.
*  ENDLOOP.
*  IF sy-subrc = 0.
*    MESSAGE s060(e2).
*  ENDIF.

ENDFORM.                    " user_command_copy
*&---------------------------------------------------------------------*
*&      Form  bdc_insert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BDC_INSERT .
  DATA: L_PKASM,
        L_PKSUM,
        L_PKIMP,
        L_PKKLA.
  DATA: L_KWBZM(20),
        L_BEHAZ(3),
        L_SAFETY(10),
        L_BEHMG(16),
        L_SAFETYTIME(20).

  WRITE: GT_LIST-KWBZM   TO L_KWBZM NO-ZERO,
         GT_LIST-BEHAZ   TO L_BEHAZ NO-ZERO,
         GT_LIST-ZZEISBE TO L_SAFETY,
         GT_LIST-ZZTIM   TO L_SAFETYTIME NO-ZERO.
  CASE GT_LIST-RKSTA.
    WHEN 'A'.
      L_PKASM = 'X'.
    WHEN 'M'.
      L_PKSUM = 'X'.
    WHEN 'I'.
      L_PKIMP = 'X'.
    WHEN 'K'.
      L_PKKLA = 'X'.
  ENDCASE.

  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=TOGG',
           ' ' 'RMPKR-WERKS'          GT_LIST-WERKS.
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=CCY_CRE'.
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0120',
           ' ' 'BDC_OKCODE'           '=WEIT',
           ' ' 'RMPKR-MATNR'          GT_LIST-MATNR,
           ' ' 'RMPKR-PRVBE'          GT_LIST-PRVBE,
           ' ' 'RMPKR-PKKLA'          L_PKKLA,  "classic
           ' ' 'RMPKR-PKIMP'          L_PKIMP,  "event
           ' ' 'RMPKR-PKSUM'          L_PKSUM,  "Manual SumJC
           ' ' 'RMPKR-PKASM'          L_PKASM.  "Auto. SumJC
  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '/00',
           ' ' 'PKHD-ABLAD'           GT_LIST-ABLAD.   "Storing position
  CASE GT_LIST-RKSTA.
    WHEN 'A'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
               ' ' 'RMPKR-STUML'          'X',
"stock transport
               ' ' 'PKHD-SUMRST3'         '0001',
"Replenishment Strategy
               ' ' 'PKHD-PABPRF'          'JC01',
"JIT Call Profile
               ' ' 'PKHD-JITSCPRF'        'SP01'.          .
      "Sched Profile
    WHEN 'M'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
               ' ' 'RMPKR-STFRD'          'X',
"External Procurement
               ' ' 'PKHD-SUMRST2'         '0001',
"Replenishment Strategy
               ' ' 'PKHD-PABPRF'          'JC01',
"JIT Call Profile
               ' ' 'PKHD-JITSCPRF'        'SP01'.
      "Sched.Profile
    WHEN OTHERS.
*S__PAUL 04.05.11
      DATA : LV_PKSTU(4).

      IF GT_LIST-WERKS = 'P001'.
        LV_PKSTU = '0001'.
      ELSE.
        LV_PKSTU = '0007'.
      ENDIF.
      PERFORM BDC_DYNPRO_PROCESSING USING:
*"stock transport
               ' ' 'RMPKR-STUML'          'X',
*"Replenishment Strategy
*               ' ' 'PKHD-PKSTU'           '0001'.
               ' ' 'PKHD-PKSTU'           LV_PKSTU.
*E__
  ENDCASE.
  IF GT_LIST-RKSTA EQ 'K'.
    L_BEHMG = GT_LIST-BEHMG.
    CONDENSE L_BEHMG.
    PERFORM BDC_DYNPRO_PROCESSING USING:
*           ' ' 'PKHD-BEHAZ'           '2', "Number of kanban containers
           ' ' 'PKHD-BEHAZ'           GT_LIST-BEHAZ, "Number of kanban
           ' ' 'PKHD-SIGAZ'           GT_LIST-SIGAZ,   "Maximum Empty
           ' ' 'PKHD-BEHMG'           L_BEHMG,         "KANBAN QTY
           ' ' 'PKHD-PKBHT'           GT_LIST-PKBHT,   "CARREIER LOAD
           ' ' 'PKHD-ZFEEDER'         GT_LIST-ZFEEDER. "Feeder
  ENDIF.

*S__PAUL ADD ABOUT 'I'.
  IF GT_LIST-RKSTA EQ 'I'.
    L_BEHMG = GT_LIST-BEHMG.
    CONDENSE L_BEHMG.
    PERFORM BDC_DYNPRO_PROCESSING USING:
           ' ' 'PKHD-ZFEEDER'         GT_LIST-ZFEEDER,  "Feeder
*           ' ' 'PKHD-SIGAZ'           GT_LIST-SIGAZ,   "Maximum Empty
           ' ' 'PKHD-PKBHT'           GT_LIST-PKBHT,   "CARREIER LOAD
           ' ' 'PKHD-BEHMG'           L_BEHMG.         "KANBAN QTY
  ENDIF.

  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '/00',
           ' ' 'RMPKR-PACKV'          GT_LIST-PIID.    "Packing Object
  if p_kanban = ' '.
    PERFORM BDC_DYNPRO_PROCESSING USING:
        ' ' 'PKHD-KWBZM'           L_KWBZM.
  ENDIF.
*S_paul
  IF GT_LIST-WERKS = 'E001' or GT_LIST-WERKS = 'E002'.
    PERFORM BDC_DYNPRO_PROCESSING USING:
             ' ' 'PKHD-PKUMW'           'P001'.
  ENDIF.
*E__
  "Replenishment Lead Time
  CASE GT_LIST-RKSTA.
    WHEN 'M'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
               ' ' 'PKHD-EBELN'           GT_LIST-EBELN,   "Agreement
               ' ' 'PKHD-EBELP'           GT_LIST-EBELP.
      "Agreement Item
    WHEN 'A'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
               ' ' 'PKHD-UMLGO'           GT_LIST-LGORT.
*S__Paul
    WHEN 'I'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
               ' ' 'PKHD-UMLGO'           GT_LIST-LGORT.
*               ' ' 'PKHD-KCPRF'           '0001'.
      IF L_BEHMG > 0.
        PERFORM BDC_DYNPRO_PROCESSING USING:
                 ' ' 'PKHD-PKRMG'           L_BEHMG,
                 ' ' 'PKHD-PKFMG'           L_BEHMG.
      ENDIF.
*E__
    WHEN OTHERS.
      PERFORM BDC_DYNPRO_PROCESSING USING:
               ' ' 'PKHD-UMLGO'           GT_LIST-LGORT,
  "StorageLocation
               ' ' 'PKHD-KCPRF'           '0001'.
      "Kanban Calculation Profile
  ENDCASE.

  PERFORM BDC_DYNPRO_PROCESSING USING:
           'X' 'SAPLMPK_CCY_UI'       '0110',
           ' ' 'BDC_OKCODE'           '=SAVE'.

  CASE GT_LIST-RKSTA.
    WHEN 'K'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
             ' ' 'PKHD-BEHAZ'           L_BEHAZ,
  "Number of kanban containers
             ' ' 'PKHD-SIGAZ'           GT_LIST-SIGAZ,   "Maximum Empty
             ' ' 'PKHD-PKSFG'         '0001'.
    WHEN 'I'.
      PERFORM BDC_DYNPRO_PROCESSING USING:
           ' ' 'PKHD-PKSFG'         '0001'.
    WHEN OTHERS.
  ENDCASE.

  if p_kanban = 'X'.
    PERFORM BDC_DYNPRO_PROCESSING USING:
           ' ' 'PKHD-ZZEISBE'         L_SAFETY,       "Safety Stock
           ' ' 'PKHD-ZZTIM'           L_SAFETYTIME.   "Safety Time
  endif.
  PERFORM BDC_DYNPRO_PROCESSING USING:
         ' ' 'PKHD-ZRHLH'           Gt_list-zrhlh,  "RH/LH
         ' ' 'PKHD-ZZFSTP'          Gt_list-ZZFSTP. "STOP

  CALL TRANSACTION 'PKMC'
                   USING BDCDATA
                   MODE  P_MODE
                   UPDATE 'S'
                   MESSAGES INTO IT_MESSAGE.
ENDFORM.                    " bdc_insert
*&---------------------------------------------------------------------*
*&      Form  find_packobj
*&---------------------------------------------------------------------*
*       Find Packing Instruction
*----------------------------------------------------------------------*
FORM FIND_PACKOBJ  USING    IF_MATNR  LF_RMATP
                   CHANGING EF_PACKNR
                            EF_POBJID.

*  DATA: lf_rmatp         TYPE mara-rmatp.
  DATA: LT_PINST         TYPE PDT_T_PACKOBJ_FOUND,
        LS_PINST         TYPE PDT_S_PACKOBJ_FOUND,
        LS_PAITEMTYPE    TYPE PDT_RA_PAITEMTYPE,
        LT_RA_PAITEMTYPE TYPE PDT_T_RA_PAITEMTYPE.

  CLEAR: EF_PACKNR, EF_POBJID.
  CLEAR LS_PAITEMTYPE.
  LS_PAITEMTYPE-SIGN   = 'I'.
  LS_PAITEMTYPE-OPTION = 'EQ'.
  LS_PAITEMTYPE-LOW    = 'R'.         "reference material
  APPEND LS_PAITEMTYPE TO LT_RA_PAITEMTYPE.

*  SELECT SINGLE rmatp
*    INTO lf_rmatp
*    FROM mara
*   WHERE matnr EQ if_matnr.

* Find Packing Object
  CHECK NOT LF_RMATP IS INITIAL.

  CALL FUNCTION 'VHUPODB_PACKOBJ_FIND'
       EXPORTING
            PACKTYP_IMP          = 'P'
            RANGE_PAITEMTYPE_IMP = LT_RA_PAITEMTYPE
            MATNR_IMP            = LF_RMATP
            MAXRECORDS_IMP       = 1
       CHANGING
            PACKOBJ_TAB          = LT_PINST
       EXCEPTIONS
            POS_WITHOUT_HEAD     = 1
            OTHERS               = 2.
  READ TABLE LT_PINST INTO LS_PINST INDEX 1.
  IF SY-SUBRC EQ 0.
    EF_PACKNR = LS_PINST-PACKNR.
    EF_POBJID = LS_PINST-POBJID.
  ENDIF.
ENDFORM.                    " find_packobj
*&---------------------------------------------------------------------*
*&      Form  get_pi_info
*&---------------------------------------------------------------------*
*       Get Packing Instruction Info.
*----------------------------------------------------------------------*
FORM GET_PI_INFO USING    U_PACKV
                 CHANGING C_BEHMG
                          C_PKBHT.
  DATA: LS_PIBASEDATA LIKE PIBASEDATA.

  CALL FUNCTION 'VHUPIAP_GET_PI_INFO'
       EXPORTING
            IP_PACKNR           = U_PACKV
       IMPORTING
            ES_BASEDATA         = LS_PIBASEDATA
       EXCEPTIONS
            PACKINSTR_NOT_FOUND = 1
            PACKINSTR_DELETED   = 2
            OTHERS              = 3.

  C_BEHMG  = LS_PIBASEDATA-T_TRGQTY.
  C_PKBHT  = LS_PIBASEDATA-LOADCARR.

ENDFORM.                    " get_pi_info
*&---------------------------------------------------------------------*
*&      Form  read_packobj_ident
*&---------------------------------------------------------------------*
*       Read Packing Object Identification
*----------------------------------------------------------------------*
FORM READ_PACKOBJ_IDENT  USING    IF_PACKNR
                         CHANGING EF_PIID.
*  DATA : pobjid_exp   LIKE  packkp-pobjid,
*         packtyp_exp  LIKE  packkp-packtyp,
*         content_exp  LIKE  packkps-content.
*
*  CALL FUNCTION 'VHUPODB_PACKOBJ_READ_IDENT'
*    EXPORTING
*      packnr_imp  = if_packnr
*    IMPORTING
*      pobjid_exp  = pobjid_exp
*      packtyp_exp = packtyp_exp
*      content_exp = content_exp.
*
*  IF packtyp_exp = 'P'.
*    ef_piid = pobjid_exp.
*  ENDIF.
ENDFORM.                    " read_packobj_ident
*&---------------------------------------------------------------------*
*&      Form  user_command_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_NEW .
*  CLEAR rmpkr.
*  rmpkr-werks = p_werks.
*  CALL SCREEN 110 STARTING AT 5 5.
ENDFORM.                    " user_command_new
*&---------------------------------------------------------------------*
*&      Form  handle_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM handle_f4  USING    u_fieldname
*                         u_fieldvalue
*                         u_row_no
*                         u_event_data
*                         u_bad_cells
*                         u_display.
*  CASE u_fieldname.
*    WHEN 'KCPRF'.
*      PERFORM f4_kcprf.
*  ENDCASE.
*ENDFORM.                                                    " handle_f4
*&---------------------------------------------------------------------*
*&      Form  func_conversion_exit_matn1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FUNC_CONVERSION_EXIT_MATN1  USING    U_IN
                                 CHANGING C_OUT.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
       EXPORTING
            INPUT  = U_IN
       IMPORTING
            OUTPUT = C_OUT.

ENDFORM.                    " func_conversion_exit_matn1
*&---------------------------------------------------------------------*
*&      Form  f4_field_assign
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f4_field_assign .
*  CLEAR gs_f4.
*  gs_f4-fieldname  = 'KCPRF'.
*  gs_f4-register   = 'X'.
*  APPEND gs_f4 TO gt_f4.
*
*  CALL METHOD g_grid1->register_f4_for_fields
*    EXPORTING
*      it_f4 = gt_f4.
*ENDFORM.                    " f4_field_assign
*&---------------------------------------------------------------------*
*&      Form  user_command_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_ENTER .
*  CLEAR gt_list.
*  gt_list-werks   = rmpkr-werks.
*  gt_list-matnr   = rmpkr-matnr.
*  gt_list-maktx   = rmpkr-maktx.
*  gt_list-prvbe   = rmpkr-prvbe.
*  gt_list-pvbtx   = rmpkr-pvbtx.
*  gt_list-pkstu   = '0001'.
*  gt_list-line    = 'N'.
*
**  PERFORM find_packobj USING    gt_list-matnr
**                       CHANGING gt_list-packv
**                                gt_list-piid.
**  PERFORM get_pi_info USING    gt_list-packv
**                      CHANGING gt_list-behmg
**                               gt_list-pkbht.
*
*  SELECT SINGLE meins
*    FROM mara
*    INTO gt_list-meins
*   WHERE matnr = gt_list-matnr.
*
** Control Cycle Category
*  CASE 'X'.
*    WHEN rmpkr-pkkla.
*      gt_list-rksta   = 'K'.  "Classic KANBAN
*      gt_list-pkkla   = 'X'.
*    WHEN rmpkr-pkimp.
*      gt_list-rksta   = 'I'.  "Event-driven KANBAN
*      gt_list-pkimp   = 'X'.
*  ENDCASE.
*
*  INSERT gt_list INDEX 1.
ENDFORM.                    " user_command_enter
*&---------------------------------------------------------------------*
*&      Form  input_check_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INPUT_CHECK_MATERIAL .
*  SELECT SINGLE meins
*    FROM mara
*    INTO gt_list-meins
*   WHERE matnr = gt_list-matnr.
*  IF sy-subrc = 0.
*    PERFORM read_text USING    'MAKTX'
*                                gt_list-matnr
*                      CHANGING  gt_list-maktx.
**    PERFORM find_packobj USING    gt_list-matnr
**                         CHANGING gt_list-packv
**                                  gt_list-piid.
*    PERFORM get_pi_info USING    gt_list-packv
*                        CHANGING gt_list-behmg
*                                 gt_list-pkbht.
*  ELSE.
**  MESSAGE e002(pk) WITH GT_LIST-matnr GT_LIST-werks.
*    gs_class_msg-msgid = 'PK'.
*    gs_class_msg-msgno = '002'.
*    gs_class_msg-msgv1 = gt_list-matnr .
*    gs_class_msg-msgv2 = gt_list-werks.
*    error_in_data = 'X'.
*  ENDIF.
ENDFORM.                    " input_check_material
*&---------------------------------------------------------------------*
*&      Form  input_check_supply_area
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INPUT_CHECK_SUPPLY_AREA .
  PERFORM READ_TEXT USING    'PVBTX'
                             GT_LIST-PRVBE
                    CHANGING GT_LIST-PVBTX.
  CHECK GT_LIST-PVBTX IS INITIAL.
*  MESSAGE e003(pk) WITH GT_LIST-prvbe.
  GS_CLASS_MSG-MSGID = 'PK'.
  GS_CLASS_MSG-MSGNO = '003'.
  GS_CLASS_MSG-MSGV1 = GT_LIST-PRVBE .
  ERROR_IN_DATA = 'X'.
ENDFORM.                    " input_check_supply_area
*&---------------------------------------------------------------------*
*&      Form  input_check_location
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM INPUT_CHECK_LOCATION .

  DATA : L_LGORT TYPE LGORT_D.

*   Storage Location
*  CLEAR l_lgort.
*  SELECT p~lgort INTO l_lgort UP TO 1 ROWS
*    FROM eord AS d INNER JOIN ekpo AS p
*         ON d~ebeln EQ p~ebeln
*        AND d~ebelp EQ p~ebelp
*         WHERE d~matnr EQ gt_list-matnr
*           AND d~werks EQ gt_list-werks
*           AND d~ebeln NE space
*           AND p~lgort EQ gt_list-lgpro.
*  ENDSELECT.
  SELECT SINGLE *
    FROM MARD
   WHERE MATNR = GT_LIST-MATNR
     AND WERKS = GT_LIST-WERKS
     AND LGORT = GT_LIST-LGORT.   " GT_LIST-umlgo.

  CHECK SY-SUBRC NE 0.
*  MESSAGE e040(pk) WITH GT_LIST-matnr GT_LIST-umlgo.
  GS_CLASS_MSG-MSGID = 'PK'.
  GS_CLASS_MSG-MSGNO = '040'.
  GS_CLASS_MSG-MSGV1 = GT_LIST-MATNR  .
  GS_CLASS_MSG-MSGV2 = GT_LIST-LGORT.   " GT_LIST-umlgo  .
  ERROR_IN_DATA = 'X'.

ENDFORM.                    " input_check_location
*&---------------------------------------------------------------------*
*&      Form  bdc_insert_fieldcheck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BDC_INSERT_FIELDCHECK .

  IF GT_LIST-MATNR = SPACE OR
     GT_LIST-PRVBE = SPACE.     "Supply Area
    IT_MESSAGE-MSGTYP  = 'E'.
    IT_MESSAGE-MSGID   = 'PK'.
    IT_MESSAGE-MSGNR   = '274'.
    APPEND IT_MESSAGE.
  ENDIF.
ENDFORM.                    " bdc_insert_fieldcheck
*&---------------------------------------------------------------------*
*&      Form  user_command_paste
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_PASTE .

*  DATA : lt_rows TYPE lvc_t_row,
*         l_row   TYPE lvc_s_row,
*         l_index     TYPE sy-tabix.
*
** get select line
*  CALL METHOD g_grid1->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_rows.
*
*  READ TABLE lt_rows INTO l_row INDEX 1.
*  l_index = l_row-index.
*  LOOP AT it_copy.
*    l_index = l_index + 1.
*    CLEAR gt_list.
*    MOVE-CORRESPONDING it_copy TO gt_list.
*    CLEAR gt_list-pknum.
*    gt_list-line = 'N'.
*    CLEAR gt_list-celltab.
*    PERFORM fill_celltab USING    gt_list-line
*                                  gt_list-rksta
*                         CHANGING gt_list-celltab[].
*    INSERT gt_list INDEX l_index.
*  ENDLOOP.

ENDFORM.                    " user_command_paste
*&---------------------------------------------------------------------*
*&      Form  user_command_sortup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_SORT USING U_UPDOWN.
  DATA : ET_INDEX_COLUMNS  TYPE  LVC_T_COL,
         L_COLUMNS         TYPE  LVC_S_COL.


  CALL METHOD G_GRID1->GET_SELECTED_COLUMNS
    IMPORTING
      ET_INDEX_COLUMNS = ET_INDEX_COLUMNS.

  READ TABLE ET_INDEX_COLUMNS INDEX 1 INTO L_COLUMNS.
  CHECK SY-SUBRC = 0.
  IF U_UPDOWN = 'U'.
    SORT GT_LIST BY (L_COLUMNS-FIELDNAME) ASCENDING.
  ELSEIF U_UPDOWN = 'D'.
    SORT GT_LIST BY (L_COLUMNS-FIELDNAME) DESCENDING.
  ENDIF.

ENDFORM.                    " user_command_sortup
*&---------------------------------------------------------------------*
*&      Form  user_command_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM USER_COMMAND_UPLOAD .

*  PERFORM get_file_path.
*  CHECK filename NE space.
*  PERFORM upload_local_file.
*  CHECK it_excel[] IS NOT INITIAL.
*  PERFORM uploaded_data_input_check.

ENDFORM.                    " user_command_upload
*&---------------------------------------------------------------------*
*&      Form  get_file_path
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_FILE_PATH .
*  CLEAR : filename, filefilter, path , fullpath , user_action,
*          file_encoding .

*  CALL FUNCTION 'F4_FILENAME'
*   EXPORTING
*     program_name        = sy-cprog
*     dynpro_number       = sy-dynnr
**   FIELD_NAME          = ' '
*   IMPORTING
*     file_name           = filename .

ENDFORM.                    " get_upload_file
*&---------------------------------------------------------------------*
*&      Form  upload_local_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM UPLOAD_LOCAL_FILE .
*  REFRESH it_excel.
*  CLEAR it_excel.
*
*  CALL FUNCTION 'Z_MM_EXCEL_UPLOAD'
*    EXPORTING
*      filename   = filename
*      itab       = 'IT_EXCEL'
*      begin_line = 2
*    TABLES
*      outab      = it_excel.

ENDFORM.                    " upload_local_file
*&---------------------------------------------------------------------*
*&      Form  uploaded_data_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM UPLOADED_DATA_INPUT_CHECK .
*  DATA : wa_list LIKE gt_list .
*  DATA : l_index  TYPE sy-tabix,
*         l_kwbzm(11)  TYPE n .
*
*  LOOP AT it_excel.
*    CLEAR : gt_list, wa_list.
*    MOVE-CORRESPONDING it_excel TO wa_list.
*
*
**--> check control cycle category
*    PERFORM check_category USING    wa_list-rksta
*                           CHANGING wa_list-icon
*                                    wa_list-msg.
*
*    IF wa_list-icon = space.
*
*      PERFORM func_conv_matn1 USING    it_excel-matnr
*                              CHANGING wa_list-matnr.
*
**--> check existing KANBAN master.
*      SELECT SINGLE pknum
*        FROM pkhd
*        INTO wa_list-pknum
*       WHERE matnr = wa_list-matnr
*         AND werks = wa_list-werks
*         AND prvbe = wa_list-prvbe.
*
*      IF sy-subrc = 0.      "========duplicated KANBAN
*        wa_list-icon   = icon_red.
*        CLEAR wa_list-line.
*        PERFORM func_message_text_build USING    'PK'
*                                                 '009'
*                                                 wa_list-matnr
*                                                 wa_list-werks
*                                                 wa_list-prvbe
*                                                 ''
*                                        CHANGING wa_list-msg.
*
*      ELSE.       "===================New KANBAN
*        READ TABLE gt_list WITH KEY matnr = wa_list-matnr
*                                    werks = wa_list-werks
*                                    prvbe = wa_list-prvbe
*                                    line  = 'N'.
*        IF sy-subrc NE 0.   "==============Insert KANBAN
** Control Cycle Category
*          IF it_excel-rksta = 'I'.   "event
*            wa_list-pkimp   = 'X'.  "event
*          ELSEIF it_excel-rksta = 'K'.   "classic
*            wa_list-pkkla   = 'X'.  "classic
*          ENDIF.
*
*          wa_list-icon = icon_pos.
*          wa_list-line = 'N'.
*        ELSE.   "===================== duplication KANBAN
*          wa_list-icon   = icon_red.
*          CLEAR wa_list-line.
*          PERFORM func_message_text_build USING    'PK'
*                                                    '009'
*                                                    wa_list-matnr
*                                                    wa_list-werks
*                                                    wa_list-prvbe
*                                                    ''
*                                           CHANGING wa_list-msg.
*        ENDIF.
*      ENDIF.
*
*      IF wa_list-line NE space.
**        PERFORM find_packobj USING    wa_list-matnr
**                             CHANGING wa_list-packv
**                                      wa_list-piid.
*        PERFORM get_pi_info USING    wa_list-packv
*                            CHANGING wa_list-behmg
*                                     wa_list-pkbht.
*
*        SELECT SINGLE meins
*          FROM mara
*          INTO wa_list-meins
*         WHERE matnr = wa_list-matnr.
*
** text
*        PERFORM read_text USING     'MAKTX'
*                                    wa_list-matnr
*                          CHANGING  wa_list-maktx.
*        PERFORM read_text USING     'PVBTX'
*                                    wa_list-prvbe
*                          CHANGING  wa_list-pvbtx.
*
** Replenishment Lead Time in Hours:Minutes
*        IF it_excel-kwbzm IS NOT INITIAL.
*          CLEAR wa_list-kwbzm.
*          PERFORM func_conversion_tstrn_input USING    it_excel-kwbzm
*                                              CHANGING wa_list-kwbzm.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    wa_list-pkstu = '0001'.
*    wa_list-kcprf = '0001'.
*    APPEND wa_list TO gt_list.
*  ENDLOOP.

ENDFORM.                    " uploaded_data_input
*&---------------------------------------------------------------------*
*&      Form  func_MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FUNC_MESSAGE_TEXT_BUILD  USING    U_MSGID
                                       U_MSGNR
                                       U_MSGV1
                                       U_MSGV2
                                       U_MSGV3
                                       U_MSGV4
                              CHANGING C_MSG.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = U_MSGID
            MSGNR               = U_MSGNR
            MSGV1               = U_MSGV1
            MSGV2               = U_MSGV2
            MSGV3               = U_MSGV3
            MSGV4               = U_MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = C_MSG.

ENDFORM.                    " func_MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*&      Form  func_conv_matn1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FUNC_CONV_MATN1  USING    U_MATNR
                      CHANGING C_MATNR.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
            INPUT  = U_MATNR
       IMPORTING
            OUTPUT = C_MATNR.

ENDFORM.                    " func_conv_matn1
*&---------------------------------------------------------------------*
*&      Form  link_control_cycle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LINK_CONTROL_CYCLE USING U_PKNUM.
  CALL FUNCTION 'PK_DISPLAY_CCY'
       EXPORTING
            PKNUM_IV = U_PKNUM.

ENDFORM.                    " link_control_cycle
*&---------------------------------------------------------------------*
*&      Form  clear_table_after_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CLEAR_TABLE_AFTER_SAVE .
**  CLEAR : GT_LIST-mark, GT_LIST-line.
**  MODIFY GT_LIST TRANSPORTING mark LINE where mark = 'X'
**                                           OR LINE ne space.
*  REFRESH : it_copy, it_excel.
*  CLEAR   : it_copy, it_excel.

ENDFORM.                    " clear_table_after_save
*&---------------------------------------------------------------------*
*&      Form  f4_KCPRF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM f4_kcprf .
*  DATA : l_line TYPE sy-tabix.
*  DATA : ct_cells    TYPE lvc_t_cell,
*          l_cell     TYPE lvc_s_cell.
*
*  REFRESH : table_fields, entries_char, f4_kcprf, field_tab.
*
**--> Read Dynpro Value from Screen.
*  CALL METHOD g_grid1->get_selected_cells
*    IMPORTING
*      et_cell = ct_cells.
*
*  READ TABLE ct_cells INTO l_cell INDEX 1.
*  READ TABLE GT_LIST INDEX l_cell-row_id-index.
*
*  SELECT *
*    FROM tpkpt
*    INTO TABLE f4_kcprf
*   WHERE spras = sy-langu
*     AND werks = GT_LIST-werks.
*
*  IF sy-subrc = 0.
**--> pop up
*    PERFORM f4_kcprf_fieldcat.
*    PERFORM f4_kcprf_popup USING l_cell-row_id-index.
*
*  ELSE.
**    MESSAGE i000(zz) WITH text-001.
*  ENDIF.

*ENDFORM.                                                    " f4_KCPRF
*&---------------------------------------------------------------------*
*&      Form  f4_kcprf_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F4_KCPRF_POPUP  USING   I_ROW_ID.
*  DATA : es_selfield   TYPE slis_selfield,
*         data_protocol TYPE REF TO cl_alv_changed_data_protocol.
*
*  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
*    EXPORTING
*      i_selection = 'X'
*      i_tabname   = 'F4_KCPRF'
*      it_fieldcat = it_fieldcat2
*    IMPORTING
*      es_selfield = es_selfield
*    TABLES
*      t_outtab    = f4_kcprf.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*** selecting value in pop-up screen
*    READ TABLE f4_kcprf INDEX es_selfield-tabindex.
*
** modify cell
*    CHECK f4_kcprf-kcprf IS NOT INITIAL.
*    CHECK gt_list-line NE space .
*    gt_list-kcprf = f4_kcprf-kcprf.
*    IF gt_list-line = 'C'.
*      gt_list-line  = 'S'.
*    ENDIF.
*    CHECK sy-subrc = 0.
*    MODIFY gt_list INDEX i_row_id.
*    PERFORM gui_alv_refresh.
*  ENDIF.
ENDFORM.                    " f4_kcprf_popup
*&---------------------------------------------------------------------*
*&      Form  f4_kcprf_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F4_KCPRF_FIELDCAT .
*  DATA : l_fieldcat TYPE slis_fieldcat_alv.
*
*  REFRESH it_fieldcat2.
*
*  l_fieldcat-fieldname             = 'KCPRF'.
*  l_fieldcat-ref_tabname           = 'TPKPT'.
*  APPEND l_fieldcat TO it_fieldcat2.
*
*  l_fieldcat-fieldname             = 'KCPRT'.
*  l_fieldcat-ref_tabname           = 'TPKPT'.
*  APPEND l_fieldcat TO it_fieldcat2.

ENDFORM.                    " f4_kcprf_fieldcat
*&---------------------------------------------------------------------*
*&      Form  user_command_download
*&---------------------------------------------------------------------*
*       case u_table
*       'T' = template
*       'S' = screen data
*----------------------------------------------------------------------*
FORM USER_COMMAND_DOWNLOAD USING U_TABLE.
*  PERFORM get_file_path.
*  CHECK filename NE space.
*  PERFORM make_excel_head.
*  IF u_table = 'S'.
*    PERFORM make_excel_item.
*  ENDIF.
*  PERFORM download_excel.
ENDFORM.                    " user_command_download
*&---------------------------------------------------------------------*
*&      Form  make_excel_head
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MAKE_EXCEL_HEAD  .
*  REFRESH excel_down.
*  CLEAR excel_down.
*
*  excel_down-matnr      = 'Material'.
*  excel_down-werks      = 'Plant'.
*  excel_down-prvbe      = 'Supply Area'.
*  excel_down-rksta      = 'Category'.
*  excel_down-behaz      = 'Number of kanban'.
*  excel_down-ablad      = 'Storing position'.
*  excel_down-sigaz      = 'Maximum empty'.
*  excel_down-umlgo      = 'Storage Location'.
*  excel_down-kwbzm      = 'Rep. Lead Time'.
*  excel_down-zzeisbe    = 'Safety Stock'.
*  APPEND excel_down.

ENDFORM.                    " make_excel_head
*&---------------------------------------------------------------------*
*&      Form  download_excel
*&---------------------------------------------------------------------*
*       download excel
*----------------------------------------------------------------------*
FORM DOWNLOAD_EXCEL .
*  DATA : l_filename   TYPE  string.
*  l_filename = filename.
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filename                = l_filename
*      filetype                = 'DAT'
*    TABLES
*      data_tab                = excel_down
*    EXCEPTIONS
*      file_write_error        = 1
*      no_batch                = 2
*      gui_refuse_filetransfer = 3
*      invalid_type            = 4
*      no_authority            = 5
*      unknown_error           = 6
*      header_not_allowed      = 7
*      separator_not_allowed   = 8
*      filesize_not_allowed    = 9
*      header_too_long         = 10
*      dp_error_create         = 11
*      dp_error_send           = 12
*      dp_error_write          = 13
*      unknown_dp_error        = 14
*      access_denied           = 15
*      dp_out_of_memory        = 16
*      disk_full               = 17
*      dp_timeout              = 18
*      file_not_found          = 19
*      dataprovider_exception  = 20
*      control_flush_error     = 21
*      OTHERS                  = 22.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " download_excel
*&---------------------------------------------------------------------*
*&      Form  func_conversion_tstrn_input
*&---------------------------------------------------------------------*
*       CALL FUNCTION 'CONVERSION_EXIT_TSTRN_INPUT'
*----------------------------------------------------------------------*
FORM FUNC_CONVERSION_TSTRN_INPUT  USING    U_KWBZM
                                  CHANGING C_KWBZM.
*  CALL FUNCTION 'CONVERSION_EXIT_TSTRN_INPUT'
*    EXPORTING
*      input          = u_kwbzm
*    IMPORTING
*      output         = c_kwbzm
*    EXCEPTIONS
*      invalid_format = 1
*      OTHERS         = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " func_conversion_tstrn_input
*&---------------------------------------------------------------------*
*&      Form  handle_after_user_command
*&---------------------------------------------------------------------*
*       Not used in this program
*----------------------------------------------------------------------*
FORM HANDLE_AFTER_USER_COMMAND  USING    P_E_UCOMM
                                         P_E_NOT_PROCESSED.

ENDFORM.                    " handle_after_user_command
*&---------------------------------------------------------------------*
*&      Form  make_excel_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MAKE_EXCEL_ITEM .
*  CLEAR excel_down.
*
*  LOOP AT gt_list.
*    CLEAR excel_down.
*    MOVE-CORRESPONDING gt_list TO excel_down.
*    WRITE gt_list-kwbzm TO excel_down-kwbzm NO-ZERO.
*    APPEND excel_down.
*  ENDLOOP.

ENDFORM.                    " make_excel_item
*&---------------------------------------------------------------------*
*&      Form  gui_alv_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GUI_ALV_SORT .
  DATA : L_SORT TYPE LVC_S_SORT.

  REFRESH GT_SORT.
  CLEAR L_SORT.
  L_SORT-SPOS       = 1.
  L_SORT-FIELDNAME  = 'NORMT'.
  L_SORT-UP         = 'X'.
*  l_sort-group      = 'X'.
  APPEND L_SORT TO GT_SORT.

  REFRESH GT_SORT.
  CLEAR L_SORT.
  L_SORT-SPOS       = 2.
  L_SORT-FIELDNAME  = 'MATNR'.
  L_SORT-UP         = 'X'.
*  l_sort-group      = 'X'.
  APPEND L_SORT TO GT_SORT.

  REFRESH GT_SORT.
  CLEAR L_SORT.
  L_SORT-SPOS       = 3.
  L_SORT-FIELDNAME  = 'WERKS'.
  L_SORT-UP         = 'X'.
*  l_sort-group      = 'X'.
  APPEND L_SORT TO GT_SORT.

  REFRESH GT_SORT.
  CLEAR L_SORT.
  L_SORT-SPOS       = 4.
  L_SORT-FIELDNAME  = 'PRVBE'.
  L_SORT-UP         = 'X'.
*  l_sort-group      = 'X'.
  APPEND L_SORT TO GT_SORT.

ENDFORM.                    " gui_alv_sort
*&---------------------------------------------------------------------*
*&      Form  remove_mark_in_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REMOVE_MARK_IN_SCREEN .
*  LOOP AT gt_list WHERE mark = 'X'.
*    CLEAR gt_list-mark.
*    MODIFY gt_list.
*  ENDLOOP.
ENDFORM.                    " remove_mark_in_screen
*&---------------------------------------------------------------------*
*&      Form  check_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECK_CATEGORY  USING    U_RKSTA
                     CHANGING C_ICON
                              C_MSG.
*  DATA : i_domname    LIKE  dd07v-domname,
*         i_domvalue   LIKE  dd07v-domvalue_l.
*
*  i_domname   = 'RKSTA'.
*  i_domvalue  = u_rksta.
*
*  CALL FUNCTION 'DOMAIN_VALUE_GET'
*    EXPORTING
*      i_domname        = i_domname
*      i_domvalue       = i_domvalue
** IMPORTING
**   E_DDTEXT         =
*   EXCEPTIONS
*     not_exist        = 1
*     OTHERS           = 2
*            .
*  IF sy-subrc <> 0.
*    c_icon = icon_red.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*            INTO c_msg.
*  ENDIF.


ENDFORM.                    " check_category
*&---------------------------------------------------------------------*
*&      Form  READ_MARD_TABLE
*&---------------------------------------------------------------------*
FORM READ_MARD_TABLE.
  DATA : L_LGORT TYPE LGORT_D.

  SELECT SINGLE LGORT INTO L_LGORT
         FROM  PVBE
         WHERE WERKS EQ GS_LIST-WERKS
           AND PRVBE EQ GS_LIST-PRVBE.
  SELECT SINGLE LGPBE INTO GS_LIST-ABLAD
         FROM MARD
         WHERE MATNR EQ GS_LIST-MATNR
           AND WERKS EQ GS_LIST-WERKS
           AND LGORT EQ L_LGORT.

ENDFORM.                    " READ_MARD_TABLE
*&---------------------------------------------------------------------*
*&      Form  READ_MATERIAL_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MATERIAL_MASTER .
  RANGES : R_MATKL FOR MARA-MATKL.

  CLEAR   : GT_MARA[], GT_LIST[],
            R_TEMPB[], R_DISLS[], R_EPRIO[].

  IF     P_KANBAN EQ 'X'.
*-- Set Temperature conditions indicator
    SET_RANGES R_TEMPB 'I' 'EQ' '3'  ' '.   "e-KANBAN
*    SET_RANGES R_TEMPB 'I' 'EQ' '4'  ' '.   "Manual KANBAN
**  ELSEIF P_SUMJC EQ  'X'.
**    SET_RANGES R_DISLS 'I' 'EQ' 'RO' ' '.
**    "Lot size (materials planning)
**  ELSEIF P_RESERV EQ 'X'.
**    SET_RANGES R_EPRIO 'I' 'EQ' 'P401' ' '. "Stock Determination Group
  ELSE.
    SET_RANGES R_TEMPB 'I' 'EQ' '4'  ' '.   "Classic KANBAN
  ENDIF.

*-- Read Material
  SELECT C~MATNR  C~WERKS  C~DISPO C~LGFSB C~LGPRO
         C~VSPVB  AS PRVBE C~EPRIO AS INSLC
         A~MATKL  A~MEINS A~NORMT A~TEMPB A~RMATP
    INTO CORRESPONDING FIELDS OF TABLE GT_MARA
         FROM MARC AS C INNER JOIN MARA AS A
          ON C~MATNR EQ A~MATNR
         WHERE C~WERKS EQ P_WERKS
           AND C~DISPO IN S_DISPO
           AND A~MATNR IN S_MATNR
           AND A~MTART IN S_MTART
           AND A~MATKL IN S_MATKL
           AND A~FERTH IN S_FERTH
           AND A~TEMPB IN R_TEMPB
           AND C~DISLS IN R_DISLS
           AND C~EPRIO IN R_EPRIO.

  IF P_CLASS = 'X' AND P_WERKS = 'P001'.
    DELETE GT_MARA WHERE MATKL CP '*EN*'.
  ENDIF.

ENDFORM.                    " READ_MATERIAL_MASTER
*&---------------------------------------------------------------------*
*&      Form  READ_CONTROL_CYCLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CONTROL_CYCLES .

  RANGES : R_RKSTA FOR PKHD-RKSTA.

  CLEAR : R_RKSTA[].

*-- Material which Control Cycle exists
  DATA : LT_TEMP TYPE TABLE OF TY_MARA WITH HEADER LINE.

  DATA : BEGIN OF LT_TEMP01 OCCURS 0,
          MATNR(5),
          WERKS LIKE MARC-WERKS,
         END OF LT_TEMP01.

  LT_TEMP[] = GT_MARA[].

  LOOP AT LT_TEMP.
    MOVE-CORRESPONDING LT_TEMP TO LT_TEMP01.
    APPEND LT_TEMP01.
  ENDLOOP.


*S__PAUL : NO MORE USE PGN IN HMMA (NORMT)..

*  SORT LT_TEMP BY WERKS NORMT.
*
*  DELETE ADJACENT DUPLICATES FROM LT_TEMP
*                  COMPARING WERKS NORMT.

  SORT LT_TEMP01 BY WERKS MATNR.

  DELETE ADJACENT DUPLICATES FROM LT_TEMP01
                  COMPARING WERKS MATNR.

  CHECK NOT LT_TEMP[] IS INITIAL.

  IF     P_KANBAN EQ 'X'.
    SET_RANGES R_RKSTA 'I' 'EQ' 'I' ' '.  "Control cycle status
**    SET_RANGES R_RKSTA 'I' 'EQ' 'K' ' '.  "Control cycle status
**  ELSEIF P_SUMJC  EQ 'X'.
**    SET_RANGES R_RKSTA 'I' 'EQ' 'M' ' '.  "Control cycle status
**  ELSEIF P_RESERV EQ 'X'.
**    SET_RANGES R_RKSTA 'I' 'EQ' 'A' ' '.  "Control cycle status
  ELSE.
    SET_RANGES R_RKSTA 'I' 'EQ' 'K' ' '.  "Classic Kanban
  ENDIF.

*-- Read Material and Control Cycle using PGN
*  SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE GT_PKHD
*         FROM MARC AS C INNER JOIN MARA AS A
*          ON C~MATNR EQ A~MATNR
*         INNER JOIN PKHD AS P
*          ON C~MATNR EQ P~MATNR
*         AND C~WERKS EQ P~WERKS
*         FOR ALL ENTRIES IN LT_TEMP
*         WHERE C~WERKS EQ LT_TEMP-WERKS
**           AND A~NORMT EQ LT_TEMP-NORMT
*           AND P~MATNR EQ LT_TEMP-MATNR
*           AND P~RKSTA IN R_RKSTA.

  DATA : LV_MAT(6).

  LOOP AT LT_TEMP01.

    CONCATENATE LT_TEMP01-MATNR(5) '%' INTO LV_MAT.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF GT_PKHD
      FROM PKHD
     WHERE WERKS EQ LT_TEMP01-WERKS
       AND RKSTA IN R_RKSTA
       AND PKNUM EQ ( SELECT MAX( PKNUM )
                        FROM PKHD
                       WHERE WERKS EQ LT_TEMP01-WERKS
                         AND RKSTA IN R_RKSTA
                         AND MATNR LIKE LV_MAT ).

    SELECT SINGLE MATKL
      FROM MARA
      INTO GT_PKHD-MATKL
     WHERE MATNR EQ GT_PKHD-MATNR.

    SELECT SINGLE DISPO
      FROM MARC
      INTO GT_PKHD-DISPO
     WHERE MATNR EQ GT_PKHD-MATNR
       AND WERKS EQ GT_PKHD-WERKS.

    APPEND GT_PKHD.

    CLEAR : GT_PKHD, LV_MAT, LT_TEMP01.

  ENDLOOP.


*E__PAUL : NO MORE USE PGN IN HMMA (NORMT)..
ENDFORM.                    " READ_CONTROL_CYCLES
*&---------------------------------------------------------------------*
*&      Form  CREATE_ITEM
*&---------------------------------------------------------------------*
FORM CREATE_ITEM .

  MOVE-CORRESPONDING GS_MARA TO GS_LIST.
*  IF P_KANBAN EQ 'X'.
  CASE  GS_MARA-TEMPB.
    WHEN '3'.               "Event-driven KANBAN
*   Set Control Cycle Category
      GS_LIST-RKSTA  = 'I'.
    WHEN '4'.
      GS_LIST-RKSTA  = 'K'.  "Classic KANBAN
      GS_LIST-BEHAZ  = '3'.  "Number of kanban containers
  ENDCASE.
  IF GS_MARA-INSLC IS INITIAL.
    GS_LIST-LGORT = GS_MARA-LGFSB.
  ELSE.
    GS_LIST-LGORT = GS_MARA-INSLC.
  ENDIF.
  GS_LIST-KWBZM = '20000'.
**  ELSEIF P_SUMJC EQ 'X'.
**    GS_LIST-RKSTA  = 'M'.      "Manual SumJC
**    GS_LIST-PRVBE  = GS_MARA-LGFSB.
**    GS_LIST-KWBZM = '50000'.
**  ELSEIF P_RESERV EQ 'X'.
**    GS_LIST-RKSTA  = 'A'.      "Automatic SumJC
**    GS_LIST-LGORT = GS_MARA-LGFSB.
**    GS_LIST-PRVBE = GS_MARA-INSLC.
**    GS_LIST-KWBZM = '20000'.
*  ENDIF.
*   Storing position
  PERFORM READ_MARD_TABLE.

ENDFORM.                    " CREATE_ITEM
*&---------------------------------------------------------------------*
*&      Form  GET_FEEDER_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FEEDER_NAME .

  SELECT SINGLE ZFEEDNM INTO GS_LIST-ZFEEDNM
         FROM ZMMT0087
         WHERE ZFEEDER EQ GS_LIST-ZFEEDER.

ENDFORM.                    " GET_FEEDER_NAME
*&---------------------------------------------------------------------*
*&      Form  INPUT_CHECK_STATUS
*&---------------------------------------------------------------------*
FORM INPUT_CHECK_STATUS .
*   Set Control Cycle Category
  IF GT_LIST-RKSTA  = 'I'.
    GT_LIST-BEHAZ  = ' '.  "Number of kanban containers
  ELSEIF GT_LIST-RKSTA  = 'K'.  "Classic KANBAN
    GT_LIST-BEHAZ  = '3'.  "Number of kanban containers
  ENDIF.
ENDFORM.                    " INPUT_CHECK_STATUS
*&---------------------------------------------------------------------*
*&      Form  FIND_SA
*&---------------------------------------------------------------------*
*       Find Scheduling Agreement
*----------------------------------------------------------------------*
*      -->IF_MATNR  Material
*      -->IF_WERKS  Plant
*      <--EF_EBELN  Agreement
*      <--EF_EBELP  Agreement Item
*----------------------------------------------------------------------*
FORM FIND_SA  USING    IF_MATNR
                       IF_WERKS
              CHANGING EF_EBELN
                       EF_EBELP.
  CLEAR: EF_EBELN, EF_EBELP.
  SELECT SINGLE P~EBELN P~EBELP
    INTO (EF_EBELN,EF_EBELP)
    FROM EKKO AS K INNER JOIN EKPO AS P
                      ON K~EBELN = P~EBELN
   WHERE K~BSTYP EQ 'L'
     AND K~LOEKZ EQ SPACE
     AND P~LOEKZ EQ SPACE
     AND P~KANBA EQ 'Z'
     AND P~MATNR EQ IF_MATNR
     AND P~WERKS EQ IF_WERKS.
ENDFORM.                    " FIND_SA
