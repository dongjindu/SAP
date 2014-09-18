*&--------------------------------------------------------------------
*& Program ID     : ZMMR60100T
*& Profram Name   : Mass KANBAN Correction
*& Created by     : hmoon
*& Created on     : 04.29.2009
*& Development ID : MM-067
*& Reference Pgm. : ZMMR1770
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date        Developer      Request ID   Description
*& 04.29.2009   Yang                       First development
*&--------------------------------------------------------------------

REPORT  ZMMR60100T.
* Includes
INCLUDE <LIST>.
* ALV
TYPE-POOLS: SLIS.

*"Callback
DATA: GT_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
      G_STATUS            TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
      G_USER_COMMAND      TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
      G_TOP_OF_PAGE       TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
      G_TOP_OF_LIST       TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST',
      G_END_OF_LIST       TYPE SLIS_FORMNAME VALUE 'END_OF_LIST'.
DATA: GT_FIELDCAT         TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT           TYPE SLIS_LAYOUT_ALV,
      GS_GRID_SETTINGS    TYPE LVC_S_GLAY,
      GS_LIGHT            TYPE LVC_S_LAYO,
      GS_PRINT            TYPE SLIS_PRINT_ALV,
      GT_SORT             TYPE SLIS_T_SORTINFO_ALV,
      GT_SP_GROUP         TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS           TYPE SLIS_T_EVENT,
      G_SAVE                                 VALUE 'A',
      GX_VARIANT          LIKE DISVARIANT,
      G_VARIANT           LIKE DISVARIANT.
DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.
* Data to be displayed.
DATA: G_REPID             LIKE SY-REPID.
*======================================================================*
*    Global Data Declaration.
*======================================================================*
TABLES:
  MARA,
  PKHD,
  PKPS,
  PKER,
  T001W,
  PKEK.
DATA:
  BEGIN OF XKANBAN OCCURS 0.
        INCLUDE STRUCTURE ZMMS0002.
DATA: LIGHTS,
      SELECTED,
  END OF XKANBAN.
DATA: LS_RETURN LIKE  BAPIRET2,
      LT_RESULT TYPE TABLE OF BAPI1075_3 WITH HEADER LINE.

INCLUDE ZMMRCOMMON.

DATA: P_WERKS  TYPE PKHD-WERKS.

*======================================================================*
*    Selection Screen.
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK SEL WITH FRAME TITLE TEXT-SEL.
PARAMETERS:
*  PA_WERKS TYPE T001W-WERKS OBLIGATORY MEMORY ID WRK.
  P_P001 RADIOBUTTON GROUP PLNT DEFAULT 'X',
  P_E001 RADIOBUTTON GROUP PLNT,

* by ig.moon - Engine Plant Split {
  P_E002 RADIOBUTTON GROUP PLNT.
* }

SELECT-OPTIONS:
*  SO_WERKS FOR T001W-WERKS no-display,
  SO_RKSTA FOR PKHD-RKSTA,
  SO_MATNR FOR PKHD-MATNR,
  SO_PRVBE FOR PKHD-PRVBE,
  SO_PKBST FOR PKPS-PKBST OBLIGATORY,
  SO_RSNUM FOR PKPS-RSNUM,
**Paul Add 2 Fields : 07/11/11
  SO_SAEDT FOR PKPS-SAEDT MODIF ID XYZ,
  SO_SAEUZ FOR PKPS-SAEUZ MODIF ID XYZ.
**E
*  SO_FERTH FOR MARA-FERTH,
*  SO_FORMT FOR MARA-FORMT.
SELECTION-SCREEN END OF BLOCK SEL.
SELECTION-SCREEN BEGIN OF BLOCK OPT WITH FRAME TITLE TEXT-OPT.
PARAMETERS:
  P_REPEAT RADIOBUTTON GROUP BG DEFAULT 'X',
  P_EMPTY  RADIOBUTTON GROUP BG,
  P_CANCEL RADIOBUTTON GROUP BG.
SELECTION-SCREEN END OF BLOCK OPT.
*======================================================================*
*    Initialization.
*======================================================================*
INITIALIZATION.
  PERFORM INITIALIZE.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'XYZ' AND P_CANCEL = 'X'.
      SCREEN-INPUT = '0'.
      SCREEN-INVISIBLE = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*  so_rksta-option = 'EQ'. so_rksta-sign = 'I'.
*  so_rksta-low = 'I'.   append so_rksta.
*  so_rksta-low = 'K'.   append so_rksta.

*======================================================================*
*    Start of Selection.
*======================================================================*
START-OF-SELECTION.
*  PERFORM get_working_time  TABLES xwtime
*                            USING sy-datum '20'  pa_werks space.
  IF P_P001 = 'X'.
    P_WERKS = 'P001'.
* by ig.moon - Engine Plant Split {
*  ELSE.
   ELSEIF P_E001 = 'X'.
    P_WERKS = 'E001'.
   ELSE.
    P_WERKS = 'E002'.
* }
  ENDIF.

  PERFORM READ_KANBAN_STATUS.

*======================================================================*
*    End of Selection.
*======================================================================*
END-OF-SELECTION.

  IF SY-BATCH EQ 'X'.
    IF P_REPEAT EQ 'X'.
*   Only Repeat Status Change
      PERFORM CHANGE_STATUS_IN_BACKGROUND.
    ELSEIF P_EMPTY EQ 'X'.
      PERFORM EMPTY_KANBAN_IN_BACKGROUND.
** Furong on 10/14/11
    ELSEIF P_CANCEL EQ 'X'.
      PERFORM OLD_BUCKET_CANCEL_BACKGROUND.
** End on 10/14/11
    ENDIF.
  ELSE.
    PERFORM SHOW_LIST.
  ENDIF.

*======================================================================*
*    Subroutines.
*======================================================================*
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Variant Initialize.
*----------------------------------------------------------------------*
FORM VARIANT_INIT.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = G_REPID.
ENDFORM.                               " VARIANT_INIT
*-----------------------------------------------------------------------
*       FORM PF_STATUS_SET
*-----------------------------------------------------------------------
*       Set Status.
*----------------------------------------------------------------------*
FORM PF_STATUS_SET USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'MAIN'  EXCLUDING EXTAB.
ENDFORM.                               " PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: LS_KANBAN LIKE LINE OF XKANBAN.
  CASE R_UCOMM.
    WHEN '&IC1'.                       "doubleclick
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'CTRLCYC_NO'.
          READ TABLE XKANBAN INTO LS_KANBAN INDEX RS_SELFIELD-TABINDEX.
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'PK_DISPLAY_CCY'
                 EXPORTING
                      PKNUM_IV = LS_KANBAN-CTRLCYC_NO.
          ENDIF.
      ENDCASE.
    WHEN 'SLSE'.
      PERFORM SET_KANBAN_NEXT_STATUS.
      RS_SELFIELD-REFRESH = 'X'.
    WHEN 'K_SM'.
      PERFORM CHANGE_STATUS_QUANTITY.
      RS_SELFIELD-REFRESH = 'X'.
    WHEN 'K_FK'.
      PERFORM REPEAT_STATUS_CHANGE.
      RS_SELFIELD-REFRESH = 'X'.
    WHEN 'STRN'.
      PERFORM REVERSE_KANBAN.
      RS_SELFIELD-REFRESH = 'X'.
    WHEN '&NTE'.   "refresh
      RS_SELFIELD-REFRESH = 'X'.
  ENDCASE.
  CLEAR R_UCOMM.
ENDFORM.                               " USER_COMMAND
*---------------------------------------------------------------------*
*       FORM LAYOUT_INIT                                              *
*---------------------------------------------------------------------*
*       Layout Initialize.                                             *
*---------------------------------------------------------------------*
*  -->  RS_LAYOUT                                                     *
*---------------------------------------------------------------------*
FORM LAYOUT_INIT USING RS_LAYOUT TYPE SLIS_LAYOUT_ALV.
*"Build layout for list display
*  rs_layout-detail_popup      = 'X'.
  RS_LAYOUT-LIGHTS_FIELDNAME  = G_LIGHTS_FIELDNAME.
  RS_LAYOUT-BOX_FIELDNAME     = 'SELECTED'.
  RS_LAYOUT-ZEBRA             = 'X'.
  RS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_GRID_SETTINGS-EDT_CLL_CB = 'X'.
ENDFORM.                               " LAYOUT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       Registration of events to happen during list display.
*----------------------------------------------------------------------*
*      -->RT_EVENTS[]  Events.
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD USING RT_EVENTS TYPE SLIS_T_EVENT.
*"Registration of events to happen during list display
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = RT_EVENTS.
  READ TABLE RT_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE G_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO RT_EVENTS.
  ENDIF.
ENDFORM.                               " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_SORT
*&---------------------------------------------------------------------*
*       Sort.
*----------------------------------------------------------------------*
*      -->RT_SORT  Sort .
*----------------------------------------------------------------------*
FORM INITIALIZE_SORT USING    RT_SORT TYPE SLIS_T_SORTINFO_ALV.
  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.
  REFRESH RT_SORT.
*---- SORT
  CLEAR LS_SORT.
  LS_SORT-SPOS          =  1.
  LS_SORT-FIELDNAME     =  'MATERIAL'.
  LS_SORT-UP            =  'X'.
  APPEND LS_SORT TO RT_SORT.
  CLEAR LS_SORT.
  LS_SORT-SPOS          =  2.
  LS_SORT-FIELDNAME     =  'SUPPLYAREA'.
  LS_SORT-UP            =  'X'.
  APPEND LS_SORT TO RT_SORT.

ENDFORM.                    " INITIALIZE_SORT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG_BUILD
*&---------------------------------------------------------------------*
*       Build Field Catalog.
*----------------------------------------------------------------------*
*      -->T_FIELDCAT  Field Catalog Table.
*----------------------------------------------------------------------*
FORM FIELD_CATALOG_BUILD TABLES   ET_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
*-- Fieldcatalog create automatically --------------------------------*
  DATA : LS_FIELDCAT  TYPE SLIS_FIELDCAT_ALV,
         LT_FIELDCAT  TYPE SLIS_T_FIELDCAT_ALV.
  DATA : COL_POS(2) TYPE N.
  REFRESH ET_FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_INTERNAL_TABNAME     = 'XKANBAN'
            I_STRUCTURE_NAME       = 'ZMMS0002'
       CHANGING
            CT_FIELDCAT            = LT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LOOP AT LT_FIELDCAT INTO LS_FIELDCAT.
      CASE LS_FIELDCAT-FIELDNAME.
        WHEN 'KANBAN_ID'.
          CLEAR ET_FIELDCAT.
          MOVE-CORRESPONDING LS_FIELDCAT TO ET_FIELDCAT.
          ET_FIELDCAT-KEY                   = 'X'.
          COL_POS = COL_POS + 1.
          ET_FIELDCAT-COL_POS = COL_POS .
          APPEND ET_FIELDCAT.
        WHEN 'CTRLCYC_NO'.
          CLEAR ET_FIELDCAT.
          MOVE-CORRESPONDING LS_FIELDCAT TO ET_FIELDCAT.
          ET_FIELDCAT-HOTSPOT               = 'X'.
          COL_POS = COL_POS + 1.
          ET_FIELDCAT-COL_POS = COL_POS .
          APPEND ET_FIELDCAT.
        WHEN 'NEW_STATUS' OR 'ACTUAL_QTY'.
          CLEAR ET_FIELDCAT.
          MOVE-CORRESPONDING LS_FIELDCAT TO ET_FIELDCAT.
          COL_POS = COL_POS + 1.
          ET_FIELDCAT-EDIT                 = 'X'.
          ET_FIELDCAT-COL_POS = COL_POS .
          APPEND ET_FIELDCAT.
** PAUL
        WHEN 'BEHMG'.
          CLEAR ET_FIELDCAT.
          MOVE-CORRESPONDING LS_FIELDCAT TO ET_FIELDCAT.
          COL_POS = COL_POS + 1.
          ET_FIELDCAT-SELTEXT_M = 'Kanban Quantity'.
          ET_FIELDCAT-COL_POS = COL_POS .
          ET_FIELDCAT-COL_POS = COL_POS .
          APPEND ET_FIELDCAT.
        WHEN 'PART_GROUP_N0' OR 'PART_ASSEMBLY_CD' OR
             'MESSAGE_V1' OR 'MESSAGE_V2' OR 'MESSAGE_V3' OR
             'MESSAGE_V4'.
          CLEAR ET_FIELDCAT.
          MOVE-CORRESPONDING LS_FIELDCAT TO ET_FIELDCAT.
          COL_POS = COL_POS + 1.
          ET_FIELDCAT-NO_OUT = 'X'.
          ET_FIELDCAT-COL_POS = COL_POS .
          ET_FIELDCAT-COL_POS = COL_POS .
          APPEND ET_FIELDCAT.
        WHEN OTHERS.
          CLEAR ET_FIELDCAT.
          MOVE-CORRESPONDING LS_FIELDCAT TO ET_FIELDCAT.
          COL_POS = COL_POS + 1.
          ET_FIELDCAT-COL_POS = COL_POS .
          APPEND ET_FIELDCAT.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " FIELD_CATALOG_BUILD
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       Initialize
*----------------------------------------------------------------------*
FORM INITIALIZE .
  G_REPID = SY-REPID.
  PERFORM VARIANT_INIT.
* Get default variant
  GX_VARIANT = G_VARIANT.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            I_SAVE     = G_SAVE
       CHANGING
            CS_VARIANT = GX_VARIANT
       EXCEPTIONS
            NOT_FOUND  = 2.
  IF SY-SUBRC = 0.

  ENDIF.
  PERFORM LAYOUT_INIT     USING GS_LAYOUT.
  PERFORM EVENTTAB_BUILD  USING GT_EVENTS[].
ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  show_list
*&---------------------------------------------------------------------*
*       Show List
*----------------------------------------------------------------------*
FORM SHOW_LIST .
  PERFORM FIELD_CATALOG_BUILD TABLES GT_FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      I_CALLBACK_PF_STATUS_SET = G_STATUS
      I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
      I_GRID_SETTINGS          = GS_GRID_SETTINGS
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
      IT_SORT                  = GT_SORT[]
*      i_save                   = g_save
      IS_VARIANT               = G_VARIANT
    TABLES
      T_OUTTAB                 = XKANBAN.
ENDFORM.                    " show_list

*&---------------------------------------------------------------------*
*&      Form  change_status_quantity
*&---------------------------------------------------------------------*
*       Change Status/Quantity
*----------------------------------------------------------------------*
FORM CHANGE_STATUS_QUANTITY.
  DATA: LS_ACTUAL    LIKE BAPI1075_ACTUAL_QTY,
        LF_PROCESSED LIKE SY-TABIX,
        LF_LOCKED,
        EPKPS        LIKE PKPS,
        SPKPS        LIKE PKPS,
        PKERX        TYPE TABLE OF PKEK WITH HEADER LINE.

  LOOP AT XKANBAN WHERE SELECTED = 'X'.
    PERFORM LOCK_KANBAN_ID USING    XKANBAN-KANBAN_ID
                           CHANGING LF_LOCKED.
    IF LF_LOCKED IS INITIAL.
      IF XKANBAN-NEW_STATUS IS INITIAL.
        XKANBAN-TYPE   = 'E'.
        XKANBAN-LIGHTS = '1'.
      ELSEIF XKANBAN-NEW_STATUS EQ '2'.
        XKANBAN-TYPE   = 'E'.
        XKANBAN-LIGHTS = '1'.
        XKANBAN-ID     = 'PK'.
        XKANBAN-NUMBER = '172'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = XKANBAN-ID
                  MSGNR               = XKANBAN-NUMBER
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = XKANBAN-MESSAGE.
      ELSE.
        CLEAR: LS_ACTUAL,
               PKHD,
               SPKPS,
               EPKPS,
               PKER.
        SELECT SINGLE *
          FROM PKHD
         WHERE PKNUM EQ XKANBAN-CTRLCYC_NO.
        SELECT SINGLE *
          INTO SPKPS
          FROM PKPS
         WHERE PKKEY EQ XKANBAN-KANBAN_ID
**Paul add Date & Time : 071111
           AND SAEDT IN SO_SAEDT
           AND SAEUZ IN SO_SAEUZ.
**END

        EPKPS = SPKPS.
        EPKPS-PKBST = XKANBAN-NEW_STATUS.
        EPKPS-PKIMG = XKANBAN-ACTUAL_QTY.
        EPKPS-SAEDT = SY-DATLO.
        EPKPS-SAEUZ = SY-TIMLO.
        IF PKHD-RKSTA  EQ 'I' AND
           EPKPS-PKBST EQ '1'.
          EPKPS-PKLKZ = 'X'.
        ELSE.
          EPKPS-PKLKZ = SPACE.
        ENDIF.
        IF EPKPS-PKBST EQ '2'.
          EPKPS-SADT2 = SY-DATLO.
          EPKPS-SAUZ2 = SY-TIMLO.
        ENDIF.
*--> Wenn der Status manuell geändert wird ist kein Storno mehr möglich
        IF EPKPS-PKBST NE SPKPS-PKBST.
          CLEAR EPKPS-PKBSA.
        ENDIF.

        CALL FUNCTION 'PK_REGELKREISPOSITION_AENDERN'
             EXPORTING
                  EPKPS    = EPKPS
                  E_S_PKPS = SPKPS.
        COMMIT WORK AND WAIT.
        LF_PROCESSED = LF_PROCESSED + 1.
        XKANBAN-OLD_STATUS = XKANBAN-NEW_STATUS.
        CLEAR XKANBAN-NEW_STATUS.
      ENDIF.
    ENDIF.
    CASE XKANBAN-TYPE.
      WHEN 'E'.
        XKANBAN-LIGHTS = '1'.
      WHEN 'S'.
        XKANBAN-LIGHTS = '3'.
      WHEN 'W'.
        XKANBAN-LIGHTS = '2'.
      WHEN SPACE.
        XKANBAN-LIGHTS = '3'.
      WHEN OTHERS.
        XKANBAN-LIGHTS = '1'.
    ENDCASE.
    MODIFY XKANBAN.
    PERFORM UNLOCK_KANBAN_ID USING XKANBAN-KANBAN_ID.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    MESSAGE S152(PK) WITH LF_PROCESSED.
  ELSE.
    MESSAGE S010(OH).
  ENDIF.
ENDFORM.                    " change_status_quantity
*&---------------------------------------------------------------------*
*&      Form  repeat_status_change
*&---------------------------------------------------------------------*
*       repeat_status_change.
*----------------------------------------------------------------------*
FORM REPEAT_STATUS_CHANGE.
  DATA: LS_ACTUAL    LIKE BAPI1075_ACTUAL_QTY,
        LF_PROCESSED LIKE SY-TABIX,
        LF_LOCKED,
        PKPSK        LIKE PKPS,
        SPKPS        LIKE PKPS,
        PKERX        TYPE TABLE OF PKEK WITH HEADER LINE.
* Capacity ID
  DATA: LF_CAPACITYID   TYPE KAKO-KAPID,
        LF_SHOP         TYPE CRHD-ARBPL,
        LF_DELIVERYTIME TYPE PKPS-PKGPZG.
  LOOP AT XKANBAN WHERE SELECTED = 'X'.
* Only Error Status
    CHECK XKANBAN-OLD_STATUS EQ '9'.
    PERFORM LOCK_KANBAN_ID USING    XKANBAN-KANBAN_ID
                           CHANGING LF_LOCKED.
    IF LF_LOCKED IS INITIAL.
      CLEAR: LS_ACTUAL,
             PKHD,
             SPKPS,
             PKPSK,
             PKER,
             PKEK.

      SELECT SINGLE *
            FROM PKHD
            WHERE PKNUM EQ XKANBAN-CTRLCYC_NO.
      SELECT SINGLE *
            INTO SPKPS
            FROM PKPS
            WHERE PKKEY EQ XKANBAN-KANBAN_ID
**Paul add Date & Time : 071111
           AND SAEDT IN SO_SAEDT
           AND SAEUZ IN SO_SAEUZ.
**END
* Calculate Delivery Time using Replenishment Lead time
      IF PKHD-RKSTA EQ 'I'.
        CASE XKANBAN-SUPPLYAREA(1).
          WHEN 'B'.
            LF_SHOP = 'B'.
          WHEN 'P'.
            LF_SHOP = 'P'.
          WHEN OTHERS.
            LF_SHOP = 'T'.
        ENDCASE.
* Calculate Delivery Time using Replenishment Lead time
        IF SPKPS-SAEDT < SY-DATUM.
          CONVERT DATE SPKPS-SAEDT TIME SPKPS-SAEUZ
                  INTO TIME STAMP LF_DELIVERYTIME
                  TIME ZONE SY-ZONLO.
*        ELSE.
*          PERFORM set_requirements_time USING    lf_shop
*                                                 spkps-saedt
*                                                 spkps-saeuz
*                                                 pkhd-kwbzm
*                                        CHANGING lf_deliverytime.
        ENDIF.
      ENDIF.
      PKPSK = SPKPS.
      IF PKHD-RKSTA EQ 'I'.
        SELECT SINGLE *
          FROM PKEK
         WHERE PKKEY EQ XKANBAN-KANBAN_ID.
        MOVE-CORRESPONDING PKEK TO PKER.
        PKPSK-PKBST = PKER-SFGSN.
        PKPSK-PKBSA = PKER-SFGSV.
        PKHD-BEHMG  = SPKPS-PKBMG.
      ELSE.
        SELECT SINGLE *
          FROM PKER
         WHERE PKNUM EQ XKANBAN-CTRLCYC_NO
           AND PKPOS EQ XKANBAN-KANBAN_NO.
        PKPSK-PKBST = PKER-SFGSN.
        PKPSK-PKBSA = PKER-SFGSV.
      ENDIF.
*      CASE pkpsk-pkbst.
*        WHEN '2'.
**--> Bei Impulsgesteuertem Kanban mit der Abrufmenge
*          IF  pkhd-rksta EQ 'I'
*          AND rmpkb-pkimg IS INITIAL.
*            MOVE pkpsk-pkbmg TO rmpkb-pkimg.
*            IF rmpkb-pkimg IS INITIAL.
*              EXIT.
*            ENDIF.
*          ENDIF.
*          IF rmpkb-pkimg GT 0.
**   Es kann auch eine vom letzten Nachschubanstoss abweichende Menge
**   bestellt werden sowohl ereignisgesteuert als auch klassisches
*Kanban
*            pkhd-behmg = rmpkb-pkimg.
*          ENDIF.
*      ENDCASE.
      DATA: PKPSW_LS LIKE PKPS,
            SUBRC_LV LIKE SY-SUBRC.

      CLEAR SUBRC_LV.
*--> Prufen versetztes Leersetzen
      REFRESH PKERX.

      CALL FUNCTION 'PK_CHECK_VERSETZTES_LEERSETZEN'
           EXPORTING
                I_PKHD        = PKHD
                I_PKPS        = PKPSK
                I_QNT_NEW     = 0
           IMPORTING
                E_PKPS        = PKPSW_LS
           TABLES
                E_PKER        = PKERX
           EXCEPTIONS
                ERROR_MESSAGE = 1.
      IF SY-SUBRC EQ 1.
        XKANBAN-TYPE       = SY-MSGTY.
        XKANBAN-ID         = SY-MSGID.
        XKANBAN-NUMBER     = SY-MSGNO.
        XKANBAN-MESSAGE_V1 = SY-MSGV1.
        XKANBAN-MESSAGE_V2 = SY-MSGV2.
        XKANBAN-MESSAGE_V3 = SY-MSGV3.
        XKANBAN-MESSAGE_V4 = SY-MSGV4.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = SY-MSGID
                  MSGNR               = SY-MSGNO
                  MSGV1               = SY-MSGV1
                  MSGV2               = SY-MSGV2
                  MSGV3               = SY-MSGV3
                  MSGV4               = SY-MSGV4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = XKANBAN-MESSAGE.
      ELSE.
        CALL FUNCTION 'PK_ITEM_CHANGE_STATUS'
             EXPORTING
                  EPKPS         = PKPSK
                  PKTIM         = LF_DELIVERYTIME
                  EPKHD         = PKHD
                  E_S_PKPS      = SPKPS
                  ECMFLG        = 'W'
                  EKBKOR        = 'K'
             IMPORTING
                  IPKPS         = PKPSK
             TABLES
                  IPKERX        = PKERX
             EXCEPTIONS
                  ERROR_MESSAGE = 1.
        IF SY-SUBRC EQ 1.
          XKANBAN-TYPE       = SY-MSGTY.
          XKANBAN-ID         = SY-MSGID.
          XKANBAN-NUMBER     = SY-MSGNO.
          XKANBAN-MESSAGE_V1 = SY-MSGV1.
          XKANBAN-MESSAGE_V2 = SY-MSGV2.
          XKANBAN-MESSAGE_V3 = SY-MSGV3.
          XKANBAN-MESSAGE_V4 = SY-MSGV4.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
               EXPORTING
                    MSGID               = SY-MSGID
                    MSGNR               = SY-MSGNO
                    MSGV1               = SY-MSGV1
                    MSGV2               = SY-MSGV2
                    MSGV3               = SY-MSGV3
                    MSGV4               = SY-MSGV4
               IMPORTING
                    MESSAGE_TEXT_OUTPUT = XKANBAN-MESSAGE.
        ELSE.
          XKANBAN-OLD_STATUS = PKER-SFGSN.
          XKANBAN-ACTUAL_QTY = PKPSK-PKIMG.
          CLEAR XKANBAN-NEW_STATUS.
          LF_PROCESSED = LF_PROCESSED + 1.
        ENDIF.
      ENDIF.
    ENDIF.
    CASE XKANBAN-TYPE.
      WHEN 'E'.
        XKANBAN-LIGHTS = '1'.
      WHEN 'S'.
        XKANBAN-LIGHTS = '3'.
      WHEN 'W'.
        XKANBAN-LIGHTS = '2'.
      WHEN SPACE.
        XKANBAN-LIGHTS = '3'.
      WHEN OTHERS.
        XKANBAN-LIGHTS = '1'.
    ENDCASE.
    MODIFY XKANBAN.
    PERFORM UNLOCK_KANBAN_ID USING XKANBAN-KANBAN_ID.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    MESSAGE S152(PK) WITH LF_PROCESSED.
  ELSE.
    MESSAGE S010(OH).
  ENDIF.
ENDFORM.                    " repeat_status_change
*&---------------------------------------------------------------------*
*&      Form  reverse_kanban
*&---------------------------------------------------------------------*
*       Cancel KANBAN
*----------------------------------------------------------------------*
FORM REVERSE_KANBAN.
  DATA: LF_PROCESSED LIKE SY-TABIX,
        LF_LOCKED,
        PKPSK        LIKE PKPS.

  LOOP AT XKANBAN WHERE SELECTED = 'X'.
    WAIT UP TO 3 SECONDS.
    PERFORM LOCK_KANBAN_ID USING    XKANBAN-KANBAN_ID
                           CHANGING LF_LOCKED.
    IF LF_LOCKED IS INITIAL.
      CLEAR: PKHD,
             PKEK,
             PKPSK.
      SELECT SINGLE *
        FROM PKHD
       WHERE PKNUM EQ XKANBAN-CTRLCYC_NO.
      SELECT SINGLE *
        INTO PKPSK
        FROM PKPS
       WHERE PKKEY EQ XKANBAN-KANBAN_ID
**Paul add Date & Time : 071111
         AND SAEDT IN SO_SAEDT
         AND SAEUZ IN SO_SAEUZ.
**END

      CALL FUNCTION 'PK_STORNO'
           EXPORTING
                I_PKPS        = PKPSK
                I_PKHD        = PKHD
           IMPORTING
                E_PKPS        = PKPSK
                E_PKEK        = PKEK
           EXCEPTIONS
                ERROR_MESSAGE = 1
                OTHERS        = 2.
      IF SY-SUBRC = 1.
        XKANBAN-TYPE       = SY-MSGTY.
        XKANBAN-ID         = SY-MSGID.
        XKANBAN-NUMBER     = SY-MSGNO.
        XKANBAN-MESSAGE_V1 = SY-MSGV1.
        XKANBAN-MESSAGE_V2 = SY-MSGV2.
        XKANBAN-MESSAGE_V3 = SY-MSGV3.
        XKANBAN-MESSAGE_V4 = SY-MSGV4.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = SY-MSGID
                  MSGNR               = SY-MSGNO
                  MSGV1               = SY-MSGV1
                  MSGV2               = SY-MSGV2
                  MSGV3               = SY-MSGV3
                  MSGV4               = SY-MSGV4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = XKANBAN-MESSAGE.
      ELSE.
        XKANBAN-OLD_STATUS = PKPSK-PKBST.
        XKANBAN-ACTUAL_QTY = PKPSK-PKIMG.
        CLEAR XKANBAN-NEW_STATUS.
        LF_PROCESSED = LF_PROCESSED + 1.
      ENDIF.
    ENDIF.
    CASE XKANBAN-TYPE.
      WHEN 'E'.
        XKANBAN-LIGHTS = '1'.
      WHEN 'S'.
        XKANBAN-LIGHTS = '3'.
      WHEN 'W'.
        XKANBAN-LIGHTS = '2'.
      WHEN SPACE.
        XKANBAN-LIGHTS = '3'.
      WHEN OTHERS.
        XKANBAN-LIGHTS = '1'.
    ENDCASE.
    MODIFY XKANBAN.
    PERFORM UNLOCK_KANBAN_ID USING XKANBAN-KANBAN_ID.
  ENDLOOP.
  IF SY-SUBRC EQ 0.
    MESSAGE S152(PK) WITH LF_PROCESSED.
  ELSE.
    MESSAGE S010(OH).
  ENDIF.
ENDFORM.                    " reverse_kanban

*&---------------------------------------------------------------------*
*&      Form  read_kanban_status
*&---------------------------------------------------------------------*
*       Read KANBAN Status.
*----------------------------------------------------------------------*
FORM READ_KANBAN_STATUS .
**C__Paul change logic 06/08/11
  DATA     L_SAEDT LIKE SY-DATUM.

  L_SAEDT = SY-DATUM - 7.
  IF P_CANCEL EQ 'X'.

    SELECT P~PKKEY AS KANBAN_ID
           H~PKNUM AS CTRLCYC_NO
           H~RKSTA AS CONTROLCYCLESTATUS
           P~PKPOS AS KANBAN_NO
           H~MATNR AS MATERIAL
           H~WERKS AS PLANT
           H~PRVBE AS SUPPLYAREA
           P~PKBST AS OLD_STATUS
           P~PKIMG AS ACTUAL_QTY
           H~MEINS AS BASE_UOM
           P~RSNUM AS RESERV_NO
           H~ZFEEDER
           H~BEHMG
           P~SAEDT
           P~SAEUZ

        INTO CORRESPONDING FIELDS OF TABLE XKANBAN
        FROM PKHD AS H
       INNER JOIN PKPS AS P
          ON H~PKNUM = P~PKNUM
       INNER JOIN MARA AS A
          ON H~MATNR = A~MATNR
       WHERE H~MATNR IN SO_MATNR
         AND H~WERKS EQ P_WERKS
         AND H~PRVBE IN SO_PRVBE
         AND H~RKSTA IN SO_RKSTA
         AND P~PKBST IN SO_PKBST
         AND P~RSNUM IN SO_RSNUM
         AND SAEDT < L_SAEDT.

  ELSE.
    SELECT P~PKKEY AS KANBAN_ID
           H~PKNUM AS CTRLCYC_NO
           H~RKSTA AS CONTROLCYCLESTATUS
           P~PKPOS AS KANBAN_NO
           H~MATNR AS MATERIAL
           H~WERKS AS PLANT
           H~PRVBE AS SUPPLYAREA
           P~PKBST AS OLD_STATUS
           P~PKIMG AS ACTUAL_QTY
           H~MEINS AS BASE_UOM
           P~RSNUM AS RESERV_NO
           H~ZFEEDER
           H~BEHMG
           P~SAEDT
           P~SAEUZ

        INTO CORRESPONDING FIELDS OF TABLE XKANBAN
        FROM PKHD AS H
       INNER JOIN PKPS AS P
          ON H~PKNUM = P~PKNUM
       INNER JOIN MARA AS A
          ON H~MATNR = A~MATNR
       WHERE H~MATNR IN SO_MATNR
         AND H~WERKS EQ P_WERKS
*       AND H~WERKS IN SO_WERKS
         AND H~PRVBE IN SO_PRVBE
         AND H~RKSTA IN SO_RKSTA
         AND P~PKBST IN SO_PKBST
**S__Paul ADD : RSNUM condition
         AND P~RSNUM IN SO_RSNUM
**E__<06/21/11
**Paul add Date & Time : 071111
         AND SAEDT IN SO_SAEDT
         AND SAEUZ IN SO_SAEUZ.
**END
  ENDIF.
*  SELECT p~pkkey AS kanban_id
*         h~pknum AS ctrlcyc_no
*         h~rksta AS controlcyclestatus
*         p~pkpos AS kanban_no
*         h~matnr AS material
*         h~werks AS plant
*         h~prvbe AS supplyarea
*         p~pkbst AS old_status
*         p~pkimg AS actual_qty
*         h~meins AS base_uom
*         p~rsnum AS reserv_no
*         a~ferth AS part_group_n0
*         a~formt AS part_assembly_cd
*         h~zfeeder
*    INTO CORRESPONDING FIELDS OF TABLE xkanban
*    FROM pkhd AS h INNER JOIN pkps AS p
*                      ON h~pknum = p~pknum
*                   INNER JOIN mara AS a
*                      ON h~matnr = a~matnr
*   WHERE h~matnr IN so_matnr
*     AND h~werks EQ pa_werks
*     AND h~prvbe IN so_prvbe
*     AND h~rksta IN so_rksta
*     AND p~pkbst IN so_pkbst
*     AND a~ferth IN so_ferth
*     AND a~formt IN so_formt.
**E__
  LOOP AT XKANBAN.
    PERFORM READ_TEXT USING XKANBAN-ZFEEDER
                   CHANGING XKANBAN-ZFEEDNM.
    MODIFY XKANBAN.
  ENDLOOP.

ENDFORM.                    " read_kanban_status
*&---------------------------------------------------------------------*
*&      Form  lock_kanban_id
*&---------------------------------------------------------------------*
*       Lock KANBAN ID
*----------------------------------------------------------------------*
*      -->IF_PKKEY  KANBAN ID
*      -->EF_LOCKED Locked
*----------------------------------------------------------------------*
FORM LOCK_KANBAN_ID  USING    IF_PKKEY
                     CHANGING EF_LOCKED.

  CALL FUNCTION 'ENQUEUE_E_PKPS_E'
       EXPORTING
            MODE_PKPS      = 'E'
            MANDT          = SY-MANDT
            PKKEY          = IF_PKKEY
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
  IF SY-SUBRC EQ 0.
    CLEAR EF_LOCKED.
  ELSE.
    EF_LOCKED = 'X'.
    XKANBAN-TYPE       = SY-MSGTY.
    XKANBAN-ID         = SY-MSGID.
    XKANBAN-NUMBER     = SY-MSGNO.
    XKANBAN-MESSAGE_V1 = SY-MSGV1.
    XKANBAN-MESSAGE_V2 = SY-MSGV2.
    XKANBAN-MESSAGE_V3 = SY-MSGV3.
    XKANBAN-MESSAGE_V4 = SY-MSGV4.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = XKANBAN-MESSAGE.
  ENDIF.
ENDFORM.                    " lock_kanban_id
*&---------------------------------------------------------------------*
*&      Form  UNLOCK_KANBAN_ID
*&---------------------------------------------------------------------*
*       Unlock KANBAN ID
*----------------------------------------------------------------------*
*      -->IF_PKKEY KANBAN ID
*----------------------------------------------------------------------*
FORM UNLOCK_KANBAN_ID  USING    IF_PKKEY.
  CALL FUNCTION 'DEQUEUE_E_PKPS_E'
       EXPORTING
            MANDT  = SY-MANDT
            PKKEY  = IF_PKKEY
            _SCOPE = '3'.
ENDFORM.                    " UNLOCK_KANBAN_ID
*&---------------------------------------------------------------------*
*&      Form  CHANGE_STATUS_IN_BACKGROUND
*&---------------------------------------------------------------------*
*       Change Status in the background.
*----------------------------------------------------------------------*
FORM CHANGE_STATUS_IN_BACKGROUND .
  XKANBAN-SELECTED = 'X'.
  MODIFY XKANBAN TRANSPORTING SELECTED
                        WHERE SELECTED   EQ SPACE
                          AND OLD_STATUS EQ '9'.
  PERFORM REPEAT_STATUS_CHANGE.
ENDFORM.                    " CHANGE_STATUS_IN_BACKGROUND
*&---------------------------------------------------------------------*
*&      Form  set_kanban_next_status
*&---------------------------------------------------------------------*
*       Set KANBAN next status
*----------------------------------------------------------------------*
FORM SET_KANBAN_NEXT_STATUS .
  DATA: LS_RETURN    LIKE BAPIRET2,
        LF_PROCESSED TYPE SY-TABIX,
        LT_CHANGE    LIKE TABLE OF BAPI1075_3 WITH HEADER LINE.
**C__Paul 06/08/11
  LOOP AT XKANBAN WHERE SELECTED = 'X'.
    CLEAR LS_RETURN.
    REFRESH LT_CHANGE.
    CALL FUNCTION 'BAPI_KANBAN_CHANGESTATUS'
         EXPORTING
              KANBANIDNUMBER     = XKANBAN-KANBAN_ID
              NEXTSTATUS         = XKANBAN-NEW_STATUS
         IMPORTING
              RETURN             = LS_RETURN
         TABLES
              STATUSCHANGERESULT = LT_CHANGE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    IF LS_RETURN IS INITIAL.
      READ TABLE LT_CHANGE INDEX 1.
      IF SY-SUBRC EQ 0.
        XKANBAN-LIGHTS     = '3'.
        XKANBAN-OLD_STATUS = LT_CHANGE-STATUS.
        XKANBAN-NEW_STATUS = SPACE.
        XKANBAN-ACTUAL_QTY = LT_CHANGE-KANBAN_QTY.
        XKANBAN-RESERV_NO  = LT_CHANGE-RESERV_NO.
        LF_PROCESSED = LF_PROCESSED + 1.
      ENDIF.
    ELSE.
      XKANBAN-LIGHTS     = '1'.
      MOVE-CORRESPONDING LS_RETURN TO XKANBAN.
    ENDIF.
    MODIFY XKANBAN.
  ENDLOOP.

*  LOOP AT XKANBAN WHERE SELECTED = 'X'.
*    CLEAR LS_RETURN.
*    REFRESH LT_CHANGE.
*    IF XKANBAN-NEW_STATUS IS INITIAL AND
*       ( XKANBAN-OLD_STATUS EQ '5' OR
*         XKANBAN-OLD_STATUS EQ '6' ).
*      XKANBAN-NEW_STATUS = '1'.
*    ENDIF.
*    CALL FUNCTION 'BAPI_KANBAN_CHANGESTATUS'
*         EXPORTING
*              KANBANIDNUMBER     = XKANBAN-KANBAN_ID
*              NEXTSTATUS         = XKANBAN-NEW_STATUS
*         IMPORTING
*              RETURN             = LS_RETURN
*         TABLES
*              STATUSCHANGERESULT = LT_CHANGE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*         EXPORTING
*              WAIT = 'X'.
*    IF LS_RETURN IS INITIAL.
*      READ TABLE LT_CHANGE INDEX 1.
*      IF SY-SUBRC EQ 0.
*        XKANBAN-LIGHTS     = '3'.
*        XKANBAN-OLD_STATUS = LT_CHANGE-STATUS.
*        XKANBAN-NEW_STATUS = SPACE.
*        XKANBAN-ACTUAL_QTY = LT_CHANGE-KANBAN_QTY.
*        XKANBAN-RESERV_NO  = LT_CHANGE-RESERV_NO.
*        LF_PROCESSED = LF_PROCESSED + 1.
*      ENDIF.
*    ELSE.
*      XKANBAN-LIGHTS     = '1'.
*      MOVE-CORRESPONDING LS_RETURN TO XKANBAN.
*    ENDIF.
*    MODIFY XKANBAN.
*  ENDLOOP.
  MESSAGE S152(PK) WITH LF_PROCESSED.
ENDFORM.                    " set_kanban_next_status
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    P_ZFEEDER
                CHANGING P_ZFEEDNM.

  SELECT SINGLE ZFEEDNM
         INTO P_ZFEEDNM
        FROM ZMMT0087
        WHERE ZFEEDER = P_ZFEEDER.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  EMPTY_KANBAN_IN_BACKGROUND
*&---------------------------------------------------------------------*
*       Empty KANBAN in Background
*----------------------------------------------------------------------*
FORM EMPTY_KANBAN_IN_BACKGROUND .
  XKANBAN-SELECTED = 'X'.
  MODIFY XKANBAN TRANSPORTING SELECTED
                        WHERE SELECTED   EQ SPACE
                          AND ( OLD_STATUS EQ '5' OR
                                OLD_STATUS EQ '6' ).
  PERFORM SET_KANBAN_NEXT_STATUS.
ENDFORM.                    " EMPTY_KANBAN_IN_BACKGROUND
*&---------------------------------------------------------------------*
*&      Form  OLD_BUCKET_CANCEL_BACKGROUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OLD_BUCKET_CANCEL_BACKGROUND.

  XKANBAN-SELECTED = 'X'.
  MODIFY XKANBAN TRANSPORTING SELECTED
                        WHERE SELECTED EQ SPACE.
*                          AND old_status EQ '2'.

  PERFORM REVERSE_KANBAN.

ENDFORM.                    " OLD_BUCKET_CANCEL_BACKGROUND
