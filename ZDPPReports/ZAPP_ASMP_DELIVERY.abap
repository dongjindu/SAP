
*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZAPP_ASMP_DELIVERY
*& Program Name   : AS/MP Delivery Management
*& Created by     : Victor Park
*& Created on     : 07.08.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.
*&
*&----------------------------------------------------------------------
REPORT  ZAPP_ASMP_DELIVERY   NO STANDARD PAGE HEADING LINE-SIZE 132
                                                      MESSAGE-ID ZMPP.
INCLUDE ZAPP_ASMP_DELIVERY_CLASS.

TABLES : ZTPP_ASMPGI, T001W, DD07T, MARC, AFKO, MSEG, LIKP, agr_users.


DATA : BEGIN OF WA_CONDITION,
        WERKS    LIKE ZTPP_ASMPGI-WERKS,
        GITYP    LIKE ZTPP_ASMPGI-GITYP,
        ERROR_ONLY(1),
      END OF WA_CONDITION.

DATA : BEGIN OF IT_MAIN OCCURS 0.
        INCLUDE STRUCTURE ZTPP_ASMPGI.
DATA :  MAKTX LIKE MAKT-MAKTX,
        STAT_ICON LIKE ICON-ID,
        DIR_ICON  LIKE ICON-ID.
DATA : END OF IT_MAIN.

DATA : WA_MAIN LIKE IT_MAIN.

*-Screen Input fields
DATA : BEGIN OF WA_SCREEN.
        INCLUDE STRUCTURE ZTPP_ASMPGI.
DATA :  MAKTX LIKE MAKT-MAKTX,
        ASPART_NM(60),
      END OF WA_SCREEN.

DATA : IT_ASMPGI LIKE ZTPP_ASMPGI OCCURS 0 WITH HEADER LINE,
       WA_ASMPGI LIKE ZTPP_ASMPGI.

DATA : V_PLANT TYPE STRING,
       V_GITYPE TYPE STRING,
       V_ERROR  TYPE STRING,
       L_CONDITION TYPE STRING,
       L_LGORT  TYPE MSEG-LGORT,
       G_INDEX  TYPE SY-INDEX,
       L_INPUT_ERROR(1).

RANGES : R_BPART  FOR ZTPP_ASMPGI-BPART,
         R_APART  FOR ZTPP_ASMPGI-MATNR,
         R_BUDAT  FOR ZTPP_ASMPGI-BUDAT.

DATA: BTN_BPART_EXT(4)  VALUE ICON_ENTER_MORE,
      BTN_APART_EXT(4) VALUE ICON_ENTER_MORE.     "AS/MP Part

data : l_mvpart type i,   "Scrap digit check
       l_mppart type i.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INIT.


*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CALL SCREEN 0100.




*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CLOSE'.
      CLEAR : WA_SCREEN.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'  EXCLUDING extab_main.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CLEAR : EXTAB[], EXTAB.

  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.
    WHEN 'INQUIRY'.

      PERFORM INQUIRY_RTN.
    WHEN 'DELIVERY'.
      CALL SCREEN 0110 STARTING AT 10  5
                       ENDING   AT 120  10.
    WHEN 'SCRAP'.
      CALL SCREEN 0120 STARTING AT 10  5
                       ENDING   AT 120  10.
    WHEN 'RETURN'.
      CALL SCREEN 0130 STARTING AT 10 5
                       ENDING   AT 120 8.

    WHEN 'REPROC'.    "Reprocessing
      PERFORM REPROCESSING.

    WHEN 'CANCEL'.    "Cancel
      PERFORM CANCEL_PROCESS.

    WHEN 'EXT_BPART'.
      PERFORM CLICK_EXTENSION_BPART.

    WHEN 'EXT_APART'.
      PERFORM CLICK_EXTENSION_APART.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
MODULE LISTBOX OUTPUT.
  PERFORM SET_LISTBOX_PLANT.
  PERFORM SET_LISTBOX_GITYPE.
ENDMODULE.                 " LISTBOX  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_OBJECT OUTPUT.

  IF  GK_GRID  IS INITIAL.
    PERFORM SET_ALV_GRID.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = GK_GRID.
  ELSE.

    CALL METHOD GK_GRID->REFRESH_TABLE_DISPLAY.
*      EXPORTING
**        i_soft_refresh = 'X'
*        is_stable      = gs_stbl.
  ENDIF.

ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_GRID
*&---------------------------------------------------------------------*
FORM SET_ALV_GRID .

  G_REPID  =  SY-REPID.
  GS_VARIANT-REPORT = G_REPID.

  PERFORM  PRO_CREATE_GRID.
*  PERFORM  create_html.
  PERFORM  SET_LAYOUT.
  PERFORM  PRO_SET_HANDLER.
  PERFORM  BUILD_FIELDCAT TABLES GT_FIELDCAT  USING 'IT_MAIN'.
  PERFORM  PRO_ADJUST_FIELDCAT  USING GT_FIELDCAT.
  PERFORM  BUILD_SORTINFO.
*  PERFORM  set_exclude_toolbar.
*  PERFORM set_f4_field.

  PERFORM  DISPLAY_ALV_GRID.

ENDFORM.                    " SET_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  PRO_CREATE_GRID
*&---------------------------------------------------------------------*
FORM PRO_CREATE_GRID .

  CREATE OBJECT GK_CON
    EXPORTING
      CONTAINER_NAME              = CONTAINER_NAME
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  CREATE OBJECT GK_GRID
    EXPORTING
      I_PARENT      = GK_CON
      I_APPL_EVENTS = GC_TRUE.

ENDFORM.                    " PRO_CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT .

  CLEAR GS_LAYOUT.

  GS_LAYOUT-DETAILINIT = GC_TRUE.              "
  GS_LAYOUT-SMALLTITLE = GC_TRUE.               "
  GS_LAYOUT-ZEBRA       = 'X'.                  "
*  GS_LAYOUT-CWIDTH_OPT  = 'X'.            "automatic column adjustment.
*  gs_layout-excp_fname = 'LIGHT'.               "Signal
*  gs_layout-box_fname   = 'CHK'.               "using alv func
*  gs_layout-no_rowmark = gc_true.
*  gs_layout-info_fieldname    = 'COLOR'.
*  gs_layout-stylefname  = 'CELLTAB'.
  GS_LAYOUT-SEL_MODE    = 'B'. "D
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  PRO_SET_HANDLER
*&---------------------------------------------------------------------*
FORM PRO_SET_HANDLER .

*-. EVENT RECIEVER creation
  CREATE OBJECT G_EVENT_RECEIVER.

  CALL METHOD GK_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

*  CALL METHOD gk_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  SET HANDLER:  event_receiver->hndl_data_changed          FOR gk_grid.
*  SET HANDLER:  event_receiver->handle_onf4                FOR gk_grid.
*                event_receiver->hndl_data_changed_finished FOR gk_grid,
*                event_receiver->hndl_context_menu          FOR gk_grid.
*
*  SET HANDLER:  event_receiver->hndl_user_command          FOR gk_grid,
*                event_receiver->hndl_toolbar               FOR gk_grid.

*  SET HANDLER: event_receiver->hndl_double_click           FOR gk_grid.

ENDFORM.                    " PRO_SET_HANDLER
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT  TABLES PT_FIELDCAT TYPE LVC_T_FCAT
                     USING    P_STRUCT TYPE DD02L-TABNAME.
  DATA: L_DATUM(08).

  CLEAR : FUNC_FIELDCAT[].

  SY-DATUM = SY-DATUM + 1.
  MOVE SY-DATUM TO L_DATUM.
  SET PARAMETER ID 'ALVBUFFER' FIELD L_DATUM.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = G_REPID
*     i_structure_name   = ''
      I_INTERNAL_TABNAME = P_STRUCT
      I_INCLNAME         = G_REPID
      I_BYPASSING_BUFFER = 'X'
    CHANGING
      CT_FIELDCAT        = FUNC_FIELDCAT[].

  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      IT_FIELDCAT_ALV = FUNC_FIELDCAT[]
    IMPORTING
      ET_FIELDCAT_LVC = PT_FIELDCAT[]
    TABLES
      IT_DATA         = IT_MAIN
    EXCEPTIONS
      IT_DATA_MISSING = 1
      OTHERS          = 2.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  PRO_ADJUST_FIELDCAT
*&---------------------------------------------------------------------*
FORM PRO_ADJUST_FIELDCAT  USING PT_FIELDCAT TYPE LVC_T_FCAT.

  LOOP AT PT_FIELDCAT INTO GS_FIELDCAT .
    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'BPART'.
        GS_FIELDCAT-COL_POS = 1.
        GS_FIELDCAT-SCRTEXT_L = 'MV Part'.
        GS_FIELDCAT-OUTPUTLEN = 13.
      WHEN 'GITYP'.
        GS_FIELDCAT-COL_POS = 2.
        GS_FIELDCAT-SCRTEXT_L = 'Type'.
        GS_FIELDCAT-OUTPUTLEN = 4.
      WHEN 'BUDAT'.
        GS_FIELDCAT-COL_POS = 3.
        GS_FIELDCAT-OUTPUTLEN = 10.
      WHEN 'SEQNO'.
        GS_FIELDCAT-COL_POS = 4.
        GS_FIELDCAT-OUTPUTLEN = 3.
      WHEN 'STAT_ICON'.
        GS_FIELDCAT-COL_POS = 5.
        GS_FIELDCAT-SCRTEXT_L = 'Sts'.
        GS_FIELDCAT-ICON      = 'X'.
        GS_FIELDCAT-OUTPUTLEN = 3.
      WHEN 'DIR_ICON'.
        GS_FIELDCAT-COL_POS = 6.
        GS_FIELDCAT-SCRTEXT_L = 'Dir'.
        GS_FIELDCAT-ICON      = 'X'.
        GS_FIELDCAT-OUTPUTLEN = 3.
      WHEN 'MAKTX'.
        GS_FIELDCAT-COL_POS = 7.
        GS_FIELDCAT-OUTPUTLEN = 20.
      WHEN 'MATNR'.
        GS_FIELDCAT-COL_POS = 8.
        GS_FIELDCAT-OUTPUTLEN = 13.
      WHEN 'MENGE'.
        GS_FIELDCAT-COL_POS = 9.
        GS_FIELDCAT-OUTPUTLEN = 8.
      WHEN 'MEINS'.
        GS_FIELDCAT-COL_POS = 10.
        GS_FIELDCAT-SCRTEXT_L = 'UoM'.
        GS_FIELDCAT-OUTPUTLEN = 3.
      WHEN 'MPDST'.
        GS_FIELDCAT-COL_POS = 11.
        GS_FIELDCAT-SCRTEXT_L = 'Dest'.
        GS_FIELDCAT-OUTPUTLEN = 4.
      WHEN 'BUDAT_C'.
        GS_FIELDCAT-COL_POS = 12.
        GS_FIELDCAT-SCRTEXT_L = 'CancelDate'.
      WHEN 'REMAK'.
        GS_FIELDCAT-COL_POS = 13.
        GS_FIELDCAT-OUTPUTLEN = 20.
      WHEN 'M2MDOC'.
        GS_FIELDCAT-COL_POS = 14.
      WHEN 'MBLNR'.
        GS_FIELDCAT-COL_POS = 15.

      WHEN 'VBELN'.
        GS_FIELDCAT-COL_POS = 16.
      WHEN 'DLDOC'.
        GS_FIELDCAT-COL_POS = 17.
      WHEN 'AUFNR'.
        GS_FIELDCAT-COL_POS = 18.
        GS_FIELDCAT-SCRTEXT_L = 'Prod.Order'.
      WHEN 'ZMSG'.
        GS_FIELDCAT-COL_POS = 19.
        GS_FIELDCAT-OUTPUTLEN = 30.
      WHEN 'AENAM'.
        GS_FIELDCAT-COL_POS = 20.
      WHEN 'AEDAT'.
        GS_FIELDCAT-COL_POS = 21.
      WHEN 'AEZET'.
        GS_FIELDCAT-COL_POS = 22.

      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = 'X'.

    ENDCASE.

    GS_FIELDCAT-REPTEXT   =
    GS_FIELDCAT-SCRTEXT_M =
    GS_FIELDCAT-SCRTEXT_S =
    GS_FIELDCAT-SCRTEXT_L .

    MODIFY PT_FIELDCAT FROM GS_FIELDCAT.
    CLEAR : GS_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " PRO_ADJUST_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORTINFO
*&---------------------------------------------------------------------*
FORM BUILD_SORTINFO .
  CALL METHOD GK_GRID->GET_SORT_CRITERIA
    IMPORTING
      ET_SORT = GT_SORT[].

  IF GT_SORT[] IS INITIAL.
    REFRESH GT_SORT.

    CLEAR : GT_SORT.
    GT_SORT-FIELDNAME = 'BPART'.
    APPEND GT_SORT.
    CLEAR : GT_SORT.
    GT_SORT-FIELDNAME = 'MAKTX'.
    APPEND GT_SORT.
    CLEAR : GT_SORT.
    GT_SORT-FIELDNAME = 'GITYP'.
    APPEND GT_SORT.
    CLEAR : GT_SORT.
    GT_SORT-FIELDNAME = 'BUDAT'.
    APPEND GT_SORT.
*    CLEAR : GT_SORT.
*    GT_SORT-FIELDNAME = 'MATNR'.
*    APPEND GT_SORT.
    CLEAR : GT_SORT.
    GT_SORT-FIELDNAME = 'SEQNO'.
    APPEND GT_SORT.

  ENDIF.

ENDFORM.                    " BUILD_SORTINFO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID .
  CALL METHOD GK_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = GS_LAYOUT
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = G_SAVE
*     I_CONSISTENCY_CHECK           = 'X'
*     i_default                     = 'X'
      I_DEFAULT                     = SPACE
    CHANGING
      IT_OUTTAB                     = IT_MAIN[]
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.
ENDFORM.                    " DISPLAY_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_PLANT
*&---------------------------------------------------------------------*
FORM SET_LISTBOX_PLANT .
  CLEAR : LIST[], LIST,  VALUE.
  SELECT * FROM T001W WHERE WERKS LIKE 'P%'.
    VALUE-KEY    =  T001W-WERKS.
    VALUE-TEXT   =  T001W-WERKS.
    APPEND VALUE TO LIST.
  ENDSELECT.

  IF SY-DYNNR <> '0100'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID     = 'WA_SCREEN-WERKS'
        VALUES = LIST.

    IF WA_SCREEN-WERKS IS INITIAL.
      READ TABLE LIST INTO VALUE INDEX 1.
      WA_SCREEN-WERKS = VALUE-KEY.
    ENDIF.

  ELSE.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        ID     = 'WA_CONDITION-WERKS'
        VALUES = LIST.

*    IF WA_CONDITION-WERKS IS INITIAL.
*      READ TABLE LIST INTO VALUE INDEX 1.
*      WA_CONDITION-WERKS = VALUE-KEY.
*    ENDIF.
  ENDIF.
ENDFORM.                    " SET_LISTBOX_PLANT
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX-GITYPE
*&---------------------------------------------------------------------*
FORM SET_LISTBOX_GITYPE .
  CLEAR : LIST[], LIST,  VALUE.

  SELECT * FROM DD07T WHERE DOMNAME = 'ZGITYP'
                        AND DDLANGUAGE = SY-LANGU.
    VALUE-KEY  = DD07T-DOMVALUE_L.
    VALUE-TEXT = DD07T-DDTEXT.
    APPEND VALUE TO LIST .
  ENDSELECT.

  VALUE-KEY  = '*'.
  VALUE-TEXT = 'All'.
  APPEND : VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = 'WA_CONDITION-GITYP'
      VALUES = LIST.

  IF WA_CONDITION-GITYP IS INITIAL.
    WA_CONDITION-GITYP = '*'.
  ENDIF.
ENDFORM.                    " SET_LISTBOX-GITYPE
*&---------------------------------------------------------------------*
*&      Module  SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SELECTION_CONDITION INPUT.
  PERFORM SET_RANGES_FIRST_LINE USING 'R_BPART'   R_BPART.
  PERFORM SET_RANGES_FIRST_LINE USING 'R_APART'   R_APART.
ENDMODULE.                 " SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
FORM SET_RANGES_FIRST_LINE  USING PV_RANGE PV_RANGE_VAL.
  DATA: L_RANGE        TYPE STRING,
        L_RANGE_SIGN   TYPE STRING,
        L_RANGE_OPTION TYPE STRING,
        L_RANGE_LOW    TYPE STRING,
        L_RANGE_HIGH   TYPE STRING,
        L_BUTTON       TYPE STRING.

  FIELD-SYMBOLS: <LFS_RANGE_IT>     TYPE STANDARD TABLE,
                 <LFS_RANGE_SIGN>,
                 <LFS_RANGE_OPTION>,
                 <LFS_RANGE_LOW>,
                 <LFS_RANGE_HIGH>,
                 <LFS_BUTTON>.

  CONCATENATE: PV_RANGE '[]'          INTO L_RANGE,
               PV_RANGE '-SIGN'       INTO L_RANGE_SIGN,
               PV_RANGE '-OPTION'     INTO L_RANGE_OPTION,
               PV_RANGE '-LOW'        INTO L_RANGE_LOW,
               PV_RANGE '-HIGH'       INTO L_RANGE_HIGH,
               'BTN_' PV_RANGE+2(5) '_EXT' INTO L_BUTTON.

  ASSIGN: (L_RANGE)        TO <LFS_RANGE_IT>,
          (L_RANGE_SIGN)   TO <LFS_RANGE_SIGN>,
          (L_RANGE_OPTION) TO <LFS_RANGE_OPTION>,
          (L_RANGE_LOW)    TO <LFS_RANGE_LOW>,
          (L_RANGE_HIGH)   TO <LFS_RANGE_HIGH>,
          (L_BUTTON)       TO <LFS_BUTTON>.

  READ TABLE <LFS_RANGE_IT> INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    IF <LFS_RANGE_HIGH> IS INITIAL.
      IF <LFS_RANGE_LOW> IS INITIAL.
        DELETE <LFS_RANGE_IT> INDEX 1.
      ELSEIF <LFS_RANGE_LOW> CA '*+'.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'CP' TO <LFS_RANGE_OPTION>.
        MODIFY <LFS_RANGE_IT> INDEX 1 FROM PV_RANGE_VAL.
      ELSE.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'EQ' TO <LFS_RANGE_OPTION>.
        MODIFY <LFS_RANGE_IT> INDEX 1 FROM PV_RANGE_VAL.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <LFS_RANGE_SIGN>, 'BT' TO <LFS_RANGE_OPTION>.
      MODIFY <LFS_RANGE_IT> INDEX 1 FROM PV_RANGE_VAL.
    ENDIF.
  ELSE.
    IF <LFS_RANGE_HIGH> IS INITIAL.
      IF <LFS_RANGE_LOW> IS INITIAL.
        " N/A
      ELSEIF <LFS_RANGE_LOW> CA '*+'.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'CP' TO <LFS_RANGE_OPTION>.
        APPEND PV_RANGE_VAL TO <LFS_RANGE_IT>.
      ELSE.
        MOVE: 'I' TO <LFS_RANGE_SIGN>, 'EQ' TO <LFS_RANGE_OPTION>.
        APPEND PV_RANGE_VAL TO <LFS_RANGE_IT>.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <LFS_RANGE_SIGN>, 'BT' TO <LFS_RANGE_OPTION>.
      APPEND PV_RANGE_VAL TO <LFS_RANGE_IT>.
    ENDIF.
  ENDIF.

  READ TABLE <LFS_RANGE_IT> INDEX 1 TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    MOVE: ICON_DISPLAY_MORE TO <LFS_BUTTON>.
  ELSE.
    MOVE: ICON_ENTER_MORE TO <LFS_BUTTON>.
  ENDIF.

ENDFORM.                    " SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*&      Form  CLICK_EXTENSION_BPART
*&---------------------------------------------------------------------*
FORM CLICK_EXTENSION_BPART .

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      HELP_FIELD = 'BTN_BPART_EXT'
    TABLES
      RANGE      = R_BPART
    EXCEPTIONS
      CANCELLED  = 1
      OTHERS     = 2.

  READ TABLE R_BPART INDEX 1.
  IF SY-SUBRC EQ 0.
    MOVE: ICON_DISPLAY_MORE TO BTN_BPART_EXT.
  ELSE.
    MOVE: ICON_ENTER_MORE TO BTN_BPART_EXT.
  ENDIF.

ENDFORM.                    " CLICK_EXTENSION_BPART
*&---------------------------------------------------------------------*
*&      Form  CLICK_EXTENSION_APART
*&---------------------------------------------------------------------*
FORM CLICK_EXTENSION_APART .
  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      HELP_FIELD = 'BTN_APART_EXT'
    TABLES
      RANGE      = R_APART
    EXCEPTIONS
      CANCELLED  = 1
      OTHERS     = 2.

  READ TABLE R_APART INDEX 1.
  IF SY-SUBRC EQ 0.
    MOVE: ICON_DISPLAY_MORE TO BTN_APART_EXT.
  ELSE.
    MOVE: ICON_ENTER_MORE TO BTN_APART_EXT.
  ENDIF.
ENDFORM.                    " CLICK_EXTENSION_APART
*&---------------------------------------------------------------------*
*&      Form  INQUIRY_RTN
*&---------------------------------------------------------------------*
FORM INQUIRY_RTN .
  PERFORM SET_CONDITION.

  TRY.
      SELECT A~WERKS  A~BPART A~GITYP A~BUDAT A~SEQNO A~MATNR
             A~MPDST  A~PRPID A~MENGE A~MEINS A~REMAK A~DIRCT
             A~M2MDOC A~MBLNR A~GRUND A~VBELN A~POSNR A~DLDOC
             A~AUFNR  A~BUDAT_C A~ZRESULT     A~ZMSG  A~ERNAM
             A~ERDAT  A~ERZET A~AENAM A~AEDAT A~AEZET B~MAKTX
        INTO CORRESPONDING FIELDS OF TABLE IT_MAIN
      FROM ZTPP_ASMPGI AS A  INNER JOIN MAKT AS B
            ON A~BPART  = B~MATNR
      WHERE (V_PLANT)
        AND (V_GITYPE)
        AND (V_ERROR)
        AND A~BUDAT IN R_BUDAT
        AND A~BPART IN R_BPART
        AND A~MATNR IN R_APART
        AND B~SPRAS = SY-LANGU.

    CATCH CX_SY_DYNAMIC_OSQL_SYNTAX.
      MESSAGE S000 WITH TEXT-M01.
      EXIT.
  ENDTRY.


*-For Display
  LOOP AT IT_MAIN.
    IF IT_MAIN-DIRCT = '1'.
      IT_MAIN-DIR_ICON = ICON_ARROW_RIGHT.
    ELSE.
      IT_MAIN-DIR_ICON = ICON_ARROW_LEFT.
    ENDIF.

    IF IT_MAIN-ZRESULT = 'S'.
      IT_MAIN-STAT_ICON = ICON_LED_GREEN.
    ELSEIF IT_MAIN-ZRESULT = 'P' OR IT_MAIN-ZRESULT = 'E'.
      IT_MAIN-STAT_ICON = ICON_LED_YELLOW.
    ENDIF.

    MODIFY IT_MAIN.
  ENDLOOP.
ENDFORM.                    " INQUIRY_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_CONDITION
*&---------------------------------------------------------------------*
FORM SET_CONDITION .
  PERFORM SET_PLANT.
  PERFORM SET_GITYPE.
  PERFORM SET_ERROR.
  PERFORM SET_DATE.
  PERFORM SET_BPART.
  PERFORM SET_APART.


ENDFORM.                    " SET_CONDITION
*&---------------------------------------------------------------------*
*&      Form  SET_PLANT
*&---------------------------------------------------------------------*
FORM SET_PLANT .
  CLEAR V_PLANT.

  V_PLANT  = 'A~WERKS = WA_CONDITION-WERKS'.
ENDFORM.                    " SET_PLANT
*&---------------------------------------------------------------------*
*&      Form  SET_GITYPE
*&---------------------------------------------------------------------*
FORM SET_GITYPE .
  CLEAR V_GITYPE.

  IF WA_CONDITION-GITYP <> '*'.
    V_GITYPE  = 'A~GITYP = WA_CONDITION-GITYP'.
  ENDIF.
ENDFORM.                    " SET_GITYPE
*&---------------------------------------------------------------------*
*&      Form  SET_ERROR
*&---------------------------------------------------------------------*

FORM SET_ERROR .
  CLEAR : V_ERROR, L_CONDITION.

  IF WA_CONDITION-ERROR_ONLY = 'X'.
    CONCATENATE '(`'`E`'`, `'`P`'`)' INTO L_CONDITION.
    CONCATENATE 'A~ZRESULT IN'  L_CONDITION INTO V_ERROR
                                      SEPARATED BY SPACE.
  ENDIF.
ENDFORM.                    " SET_ERROR
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
FORM SET_DATE .
  CLEAR: R_BUDAT[].

  IF R_BUDAT-LOW > R_BUDAT-HIGH.
    MESSAGE E000 WITH 'Posting date input error.'.
  ENDIF.

  R_BUDAT-SIGN = 'I'.
  R_BUDAT-OPTION = 'BT'.
  APPEND R_BUDAT.
ENDFORM.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM INIT .
  clear : extab_main[], extab_main.

*-Posting date
  CONCATENATE sy-datum+0(6) '01' INTO   R_BUDAT-LOW.
  R_BUDAT-HIGH  = sy-datum.

  get PARAMETER ID 'WRK' field  wa_condition-werks.
  if wa_condition-werks is INITIAL.
    SELECT single werks into wa_condition-werks
    FROM T001W WHERE WERKS LIKE 'P%'.
  endif.

  select single * from agr_users
  where agr_name = 'Z:PP_ASMP_PC'
    and uname    = sy-uname
    and to_dat   = '99991231'.

  if sy-subrc <> 0.
    append 'DELIVERY' to extab_main.
    append 'SCRAP'    to extab_main.
    append 'RETURN'   to extab_main.
    append 'REPROC'   to extab_main.
    append 'CANCEL'   TO extab_main.
  endif.
ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  SET_BPART
*&---------------------------------------------------------------------*
FORM SET_BPART .

  IF R_BPART-LOW IS INITIAL  AND R_BPART-HIGH IS INITIAL.
    EXIT.
  ELSEIF  R_BPART-LOW IS INITIAL  AND R_BPART-HIGH IS NOT INITIAL.
    R_BPART-SIGN = 'I'.
    R_BPART-OPTION = 'BT'.
    APPEND R_BPART.
  ELSEIF R_BPART-LOW IS NOT INITIAL  AND R_BPART-HIGH IS INITIAL.
    R_BPART-SIGN = 'I'.
    R_BPART-OPTION = 'EQ'.
    APPEND R_BPART.
  ELSEIF R_BPART-LOW IS NOT INITIAL AND R_BPART-HIGH IS NOT INITIAL.
    R_BPART-SIGN = 'I'.
    R_BPART-OPTION = 'BT'.
    APPEND R_BPART.
  ENDIF.

  SORT r_BPART BY sign option low high.
  DELETE ADJACENT DUPLICATES FROM r_BPART COMPARING ALL FIELDS.
ENDFORM.                    " SET_BPART
*&---------------------------------------------------------------------*
*&      Form  SET_APART
*&---------------------------------------------------------------------*
FORM SET_APART .
  IF R_APART-LOW IS INITIAL  AND R_APART-HIGH IS INITIAL.
    EXIT.
  ELSEIF  R_APART-LOW IS INITIAL  AND R_APART-HIGH IS NOT INITIAL.
    R_APART-SIGN = 'I'.
    R_APART-OPTION = 'BT'.
    APPEND R_APART.
  ELSEIF R_APART-LOW IS NOT INITIAL  AND R_APART-HIGH IS INITIAL.
    R_APART-SIGN = 'I'.
    R_APART-OPTION = 'EQ'.
    APPEND R_APART.
  ELSEIF R_APART-LOW IS NOT INITIAL AND R_APART-HIGH IS NOT INITIAL.
    R_APART-SIGN = 'I'.
    R_APART-OPTION = 'BT'.
    APPEND R_APART.
  ENDIF.

  SORT r_APART BY sign option low high.
  DELETE ADJACENT DUPLICATES FROM r_APART COMPARING ALL FIELDS.
ENDFORM.                    " SET_APART
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.
  SET PF-STATUS '0110' EXCLUDING extab.
  SET TITLEBAR '0110'.

ENDMODULE.                 " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'CREATE'.
      PERFORM check_screen_0110.
      PERFORM process_0110.
      PERFORM refresh_grid.


  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0110_INIT  OUTPUT
*&---------------------------------------------------------------------*
MODULE SCREEN_0110_INIT OUTPUT.
  PERFORM set_listbox_plant.
  IF wa_screen-bpart IS NOT INITIAL.
    SELECT SINGLE maktx INTO wa_screen-maktx  FROM makt
    WHERE matnr =  wa_screen-bpart AND SPRAS = sy-langu.
    IF SY-SUBRC = 0.
      PERFORM GET_ASPART.
    ENDIF.
  ENDIF.

  IF wa_screen-budat IS INITIAL.
    wa_screen-budat = sy-datum.
  ENDIF.
  wa_screen-meins = 'EA'.
ENDMODULE.                 " SCREEN_0110_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  UPDATE_0100_BPART_NM  INPUT
*&---------------------------------------------------------------------*
MODULE UPDATE_0100_BPART_NM INPUT.
  CLEAR : dynpfields[], dynpfields.

  IF wa_screen-bpart IS NOT INITIAL.
    SELECT SINGLE maktx INTO wa_screen-maktx
    FROM makt
    WHERE matnr =  wa_screen-bpart AND SPRAS = sy-langu.


    MOVE : 'WA_SCREEN-BPART' TO dynpfields-fieldname,
            wa_screen-maktx  TO dynpfields-fieldvalue.
    APPEND dynpfields. CLEAR : dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname              = sy-repid
        dynumb              = sy-dynnr
      TABLES
        dynpfields          = dynpfields
      EXCEPTIONS
        invalid_dynprofield = 1
        OTHERS              = 2.

  ENDIF.
ENDMODULE.                 " UPDATE_0100_BPART_NM  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_ASPART
*&---------------------------------------------------------------------*
FORM GET_ASPART .
  DATA : wultb   TYPE TABLE OF stpov   WITH HEADER LINE,
      equicat TYPE TABLE OF cscequi WITH HEADER LINE,
      kndcat  TYPE TABLE OF cscknd  WITH HEADER LINE,
      matcat  TYPE TABLE OF cscmat  WITH HEADER LINE,
      stdcat  TYPE TABLE OF cscstd  WITH HEADER LINE,
      tplcat  TYPE TABLE OF csctpl  WITH HEADER LINE.

  CLEAR : list[], list,  value.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
    EXPORTING
      DATUB                      = SY-DATUM
      DATUV                      = SY-DATUM
      MATNR                      = WA_SCREEN-BPART
      STLAN                      = '1'
      WERKS                      = WA_SCREEN-WERKS
    TABLES
      wultb                      = wultb
      equicat                    = equicat
      kndcat                     = kndcat
      matcat                     = matcat
      stdcat                     = stdcat
      tplcat                     = tplcat
    EXCEPTIONS
      call_invalid               = 1
      material_not_found         = 2
      no_where_used_rec_found    = 3
      no_where_used_rec_selected = 4
      no_where_used_rec_valid    = 5
      OTHERS                     = 6.

  CHECK WULTB[] IS NOT INITIAL.

  LOOP AT WULTB.
    IF sy-dynnr = '0110'.
      CHECK WULTB-MATNR+0(2) = 'AS'.
    ELSEIF sy-dynnr = '0120'.
      CHECK WULTB-MATNR+0(2) = 'MP'.
    ENDIF.

    value-key    =  WULTB-MATNR.
    value-text   =  WULTB-OJTXB.
    APPEND value TO list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'WA_SCREEN-ASPART_NM'
      values = list.

  IF WA_SCREEN-ASPART_NM IS INITIAL.
    READ TABLE list INTO value INDEX 1.
    WA_SCREEN-ASPART_NM = value-key.
  ENDIF.
ENDFORM.                    " GET_ASPART
*&---------------------------------------------------------------------*
*&      Module  GET_SALES_ORDER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_SALES_ORDER INPUT.

  DATA : IT_VBAP LIKE VBAP OCCURS 0 WITH HEADER LINE.
  DATA : BEGIN OF it_sorder OCCURS 0,
           VBELN LIKE vbap-vbeln,
           POSNR LIKE vbap-posnr,
         END OF it_sorder.

  DATA : BEGIN OF it_order_qty OCCURS 0,
*          matnr LIKE mara-matnr,
          vbeln LIKE vapma-vbeln,
          posnr LIKE vapma-posnr,
          AUDAT LIKE VAPMA-AUDAT,
          wmeng LIKE vbep-wmeng,
          remqty TYPE ZREMQTY,
*          edatu LIKE vbep-edatu,
        END OF it_order_qty.

  DATA : BEGIN OF it_gi_qty OCCURS 0,
*          matnr LIKE mara-matnr,
          vbelv LIKE vbfa-vbelv,
          posnv LIKE vbfa-posnv,
          vbeln LIKE vbfa-vbeln,
          posnn LIKE mseg-zeile, "for Join
          mjahr LIKE mseg-mjahr,
          bwart LIKE mseg-bwart,
          erfmg LIKE mseg-erfmg,
*          zbudat LIKE mseg-zbudat,
         END OF it_gi_qty.

  DATA : it_gi_qty_tmp LIKE it_gi_qty OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF it_help_value OCCURS 0.
          INCLUDE STRUCTURE help_value.
  DATA: END OF it_help_value.

  DATA: BEGIN OF it_valuetab OCCURS 0,
          value LIKE help_vtab-value,
       END OF it_valuetab.

  DATA: BEGIN OF it_help_vtab OCCURS 0.
          INCLUDE STRUCTURE help_vtab.
  DATA: END OF it_help_vtab.

  DATA: lv_index LIKE sy-tabix.

  CLEAR : dynpfields[], dynpfields, it_sorder[], it_sorder.



  CHECK WA_SCREEN-ASPART_NM IS NOT INITIAL.

*-Sales Order Qty
  SELECT a~vbeln a~posnr A~AUDAT SUM( c~wmeng ) AS  wmeng
    INTO CORRESPONDING FIELDS OF TABLE it_order_qty
  FROM vapma AS a INNER JOIN vbup AS b
             ON a~vbeln = b~vbeln
            AND a~posnr = b~posnr
             INNER JOIN vbep AS c
             ON a~vbeln = c~vbeln
            AND a~posnr = c~posnr
   WHERE a~matnr = WA_SCREEN-ASPART_NM
     AND a~vkorg = 'D100'
     AND a~vtweg = '30'
     AND a~spart = '30'
     AND a~kunnr = 'MOBIS'
*     AND b~lfsta IN ('A', 'B')
     and b~lfgsa in ('A', 'B')
*     AND B~BESTA NE 'C'
     AND c~wmeng > 0
   GROUP BY a~vbeln a~posnr A~AUDAT.

  CHECK it_order_qty[] IS NOT INITIAL.

  SELECT a~vbelv a~posnv a~vbeln a~posnn  a~mjahr
    INTO CORRESPONDING FIELDS OF TABLE it_gi_qty_tmp
  FROM vbfa AS a
    FOR ALL ENTRIES IN it_order_qty
  WHERE vbelv = it_order_qty-vbeln
    AND posnv = it_order_qty-posnr
    AND a~vbtyp_n = 'R'.

  IF IT_GI_QTY_TMP[] IS NOT INITIAL.
    SELECT b~mblnr AS vbeln b~mjahr  b~zeile AS posnn
           b~bwart b~erfmg
      INTO CORRESPONDING FIELDS OF TABLE it_gi_qty
    FROM  mseg AS b
      FOR ALL ENTRIES IN it_gi_qty_tmp
    WHERE b~mblnr = it_gi_qty_tmp-vbeln
      AND b~zeile = it_gi_qty_tmp-posnn
      AND b~mjahr = it_gi_qty_tmp-mjahr
      AND b~bwart = '601'
      AND NOT EXISTS ( SELECT * FROM mseg AS c
                       WHERE c~smbln  = b~mblnr
                         AND c~smblp  = b~zeile
                         AND c~sjahr  = b~mjahr ).
  ENDIF.

  SORT it_gi_qty BY vbeln posnn mjahr.
  SORT it_gi_qty_tmp BY vbelv posnv.

  LOOP AT it_gi_qty_tmp.
    READ TABLE it_gi_qty WITH KEY vbeln = it_gi_qty_tmp-vbeln
                                  posnn = it_gi_qty_tmp-posnn
                                  mjahr = it_gi_qty_tmp-mjahr
                                  BINARY SEARCH.
    IF sy-subrc = 0.
      it_gi_qty_tmp-erfmg = it_gi_qty-erfmg.
      MODIFY it_gi_qty_tmp.
    ENDIF.
  ENDLOOP.

  LOOP AT it_order_qty .
    LOOP AT it_gi_qty_tmp WHERE vbelv = it_order_qty-vbeln
                            AND posnv = it_order_qty-posnr.

      it_order_qty-remqty = it_order_qty-remqty + it_gi_qty_tmp-erfmg.
    ENDLOOP.
    it_order_qty-remqty = it_order_qty-wmeng - it_order_qty-remqty.

    IF IT_ORDER_QTY-REMQTY EQ 0.
      DELETE IT_order_qty. CONTINUE.
    ENDIF.

    MODIFY it_order_qty.
  ENDLOOP.


*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_VBAP
*  FROM VBAP
*  WHERE MATNR = WA_SCREEN-ASPART_NM.
*
*  LOOP AT it_vbap.
**-Remaining QTY check logic ..--> SHOULD BE ADDED LATER
*
*    it_sorder-vbeln = it_vbap-vbeln.
*    it_sorder-posnr = it_vbap-posnr.
*    APPEND  it_sorder.
*  ENDLOOP.

*  CHECK it_sorder[] IS NOT INITIAL.
  CHECK it_order_qty[] IS NOT INITIAL.

  SORT IT_ORDER_QTY BY AUDAT DESCENDING VBELN DESCENDING POSNR.
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield    = 'VBELN'
*      dynpprog    = sy-repid
*      dynpnr      = sy-dynnr
*      dynprofield = 'WA_SCREEN-VBELN'
*      value_org   = 'S'
*    TABLES
*      value_tab   = it_order_qty
*      return_tab  = it_value.
*
*  READ TABLE it_value INDEX 1.
*  WA_SCREEN-VBELN = it_value-fieldval.
*
*  READ TABLE it_order_qty WITH KEY VBELN = it_value-fieldval.
*  IF SY-SUBRC = 0.
*
*    dynpfields-fieldname = 'WA_SCREEN-POSNR'.
*    dynpfields-fieldvalue = it_order_qty-POSNR.
*    APPEND dynpfields.
*
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname     = sy-cprog
*        dynumb     = sy-dynnr
*      TABLES
*        dynpfields = dynpfields.
*  ENDIF.




  CLEAR: IT_HELP_VALUE, IT_HELP_VALUE[].
  it_help_value-tabname    = 'VBAP'.
  it_help_value-fieldname  = 'VBELN'.
  it_help_value-selectflag = 'X'.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'VBAP'.
  it_help_value-fieldname  = 'POSNR'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'VAPMA'.
  it_help_value-fieldname  = 'AUDAT'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'ZSPP_COMMON'.
  it_help_value-fieldname  = 'ORQTY'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'ZSPP_COMMON'.
  it_help_value-fieldname  = 'REMQTY'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.

  DATA: LV_VALUE(30).

  CLEAR: IT_VALUETAB, IT_VALUETAB[].
  LOOP AT it_order_qty.
    it_valuetab-value = it_order_qty-vbeln.
    APPEND it_valuetab.
    it_valuetab-value = it_order_qty-posnr.
    APPEND it_valuetab.
    it_valuetab-value = it_order_qty-audat.
    APPEND it_valuetab.
    WRITE IT_ORDER_QTY-WMENG TO it_valuetab-value(10)
          UNIT 'EA' RIGHT-JUSTIFIED.
    APPEND it_valuetab.
    WRITE IT_ORDER_QTY-REMQTY TO it_valuetab-value(10)
          UNIT 'EA' RIGHT-JUSTIFIED.
    APPEND it_valuetab.
  ENDLOOP.

  CLEAR: lv_index.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      show_all_values_at_first_time = 'X'
    IMPORTING
      index                         = lv_index
    TABLES
      fields                        = it_help_value
      select_values                 = it_help_vtab
      valuetab                      = it_valuetab
    EXCEPTIONS
      field_not_in_ddic             = 1
      more_then_one_selectfield     = 2
      no_selectfield                = 3
      OTHERS                        = 4.

  READ TABLE it_order_qty INDEX lv_index.
  IF sy-subrc NE 0. EXIT. ENDIF.

  MOVE: it_order_qty-vbeln TO WA_SCREEN-VBELN,
        IT_ORDER_QTY-POSNR TO WA_SCREEN-POSNR.

  LEAVE TO SCREEN SY-DYNNR.
ENDMODULE.                 " GET_SALES_ORDER  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0120 OUTPUT.
  SET PF-STATUS '0110' EXCLUDING extab.
  SET TITLEBAR '0120'.

ENDMODULE.                 " STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0120_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_0120_INIT OUTPUT.
  PERFORM set_listbox_plant.

  IF wa_screen-bpart IS NOT INITIAL.
    SELECT SINGLE maktx INTO wa_screen-maktx  FROM makt
    WHERE matnr =  wa_screen-bpart AND SPRAS = sy-langu.
    IF SY-SUBRC = 0.
      PERFORM GET_ASPART.
    ENDIF.
  ENDIF.

  IF wa_screen-budat IS INITIAL.
    wa_screen-budat = sy-datum.
  ENDIF.
  wa_screen-meins = 'EA'.
ENDMODULE.                 " SCREEN_0120_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0120 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'CREATE'.
      PERFORM check_screen_0120.
      PERFORM process_0120.
      PERFORM refresh_grid.



  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0130 OUTPUT.

  SET PF-STATUS '0110' EXCLUDING extab.
  SET TITLEBAR '0130'.

ENDMODULE.                 " STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0130_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_0130_INIT OUTPUT.
  PERFORM set_listbox_plant.

  IF wa_screen-bpart IS NOT INITIAL.
    SELECT SINGLE maktx INTO wa_screen-maktx  FROM makt
    WHERE matnr =  wa_screen-bpart AND SPRAS = sy-langu.
  ENDIF.

  IF wa_screen-budat IS INITIAL.
    wa_screen-budat = sy-datum.
  ENDIF.
  wa_screen-meins = 'EA'.
ENDMODULE.                 " SCREEN_0130_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0130  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0130 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'CREATE'.
      PERFORM check_screen_0130.
      PERFORM process_0130.
      PERFORM refresh_grid.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0130  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PROD_ORDER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_PROD_ORDER INPUT.
  DATA: BEGIN OF it_poqty OCCURS 0,
          aufnr   LIKE   afko-aufnr,
          FTRMI   LIKE   afko-FTRMI,
          psmng   LIKE   afpo-psmng,
          bwart   LIKE   mseg-bwart,
          erfmg   LIKE   mseg-erfmg,
        END   OF it_poqty.

  DATA : BEGIN OF it_order OCCURS 0,
          aufnr   LIKE   afko-aufnr,
          FTRMI   LIKE   afko-FTRMI,
          psmng   LIKE   afpo-psmng,
          bfqty   LIKE   mseg-menge,
          remqty  TYPE   zremqty,
         END OF it_ORDER.

  DATA : BEGIN OF IT_MSEG OCCURS 0,
           AUFNR  LIKE ZTPP_ASMPGI-AUFNR,
           ZBUDAT LIKE MSEG-ZBUDAT,
           BWART  LIKE MSEG-BWART,
           MENGE  LIKE MSEG-MENGE,
           ORDER_QTY LIKE MSEG-MENGE,
           BF_QTY LIKE mseg-menge,
        END OF IT_MSEG.

  DATA: lv_tabix LIKE sy-tabix.

  CLEAR : dynpfields[], dynpfields, it_order[], it_order.

*--- COGI quantity should be appended in future.

  IF WA_SCREEN-BPART IS INITIAL.
    MESSAGE S000 WITH 'Input B-PART'.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  SELECT a~aufnr a~FTRMI b~psmng b~WEMNG AS bfqty
      UP TO 100 ROWS
    INTO CORRESPONDING FIELDS OF TABLE it_order
    FROM afko AS a INNER JOIN afpo AS b
                      ON b~aufnr = a~aufnr
                     AND b~posnr = 1
   WHERE a~FTRMI <= sy-datum
     AND a~PLNBEZ = wa_screen-bpart.

  LOOP AT it_order.
    it_order-remqty = it_order-psmng - it_order-bfqty.

    IF it_order-remqty <= 0.
      DELETE it_order. CONTINUE.
    ENDIF.

    MODIFY it_order.
  ENDLOOP.

  LOOP AT it_order FROM 41.
    DELETE it_order.
  ENDLOOP.

*  select a~aufnr a~FTRMI b~psmng c~bwart c~erfmg
*      up to 100 rows
*    into CORRESPONDING FIELDS OF TABLE it_poqty
*    from afko as a inner join afpo as b
*                      on b~aufnr = a~aufnr
*                     and b~posnr = 1
*                   left outer join mseg as c
*                      on c~aufnr = a~aufnr
*                     and c~bwart = '101'
*   where a~FTRMI <= sy-datum
*     and a~PLNBEZ = wa_screen-bpart.
**     and c~bwart in ('101','102').
*  if sy-subrc ne 0.
*    message s000 with 'Available Prod. Order not found.'.
*    leave to screen sy-dynnr.
*  endif.

*  select a~aufnr a~FTRMI b~psmng c~bwart c~erfmg
*      up to 100 rows
*    appending CORRESPONDING FIELDS OF TABLE it_poqty
*    from afko as a inner join afpo as b
*                      on b~aufnr = a~aufnr
*                     and b~posnr = 1
*                   left outer join mseg as c
*                      on c~aufnr = a~aufnr
*                     and c~bwart = '102'
*   where a~FTRMI <= sy-datum
*     and a~PLNBEZ = wa_screen-bpart.
**     and c~bwart in ('101','102').
*  if sy-subrc ne 0.
*    message s000 with 'Available Prod. Order not found.'.
*    leave to screen sy-dynnr.
*  endif.
*
*  loop at it_poqty.
*    clear: it_order.
*    read table it_order with key aufnr = it_poqty-aufnr.
*    if sy-subrc ne 0.
*      move: it_poqty-aufnr to it_order-aufnr,
*            it_poqty-FTRMI to it_order-FTRMI,
*            it_poqty-psmng to it_order-psmng.
*      case it_poqty-bwart.
*        when '101'.
*          it_order-bfqty = it_poqty-erfmg.
*        when '102'.
*          it_order-bfqty = it_poqty-erfmg * -1.
*      endcase.
*
*      append it_order.
*    else.
*      move: sy-tabix to lv_tabix.
*
*      case it_poqty-bwart.
*        when '101'.
*          it_order-bfqty = it_order-bfqty + it_poqty-erfmg.
*        when '102'.
*          it_order-bfqty = it_order-bfqty - it_poqty-erfmg.
*      endcase.
*
*      it_order-remqty = it_order-psmng - it_order-bfqty.
*
*      modify it_order index lv_tabix.
*    endif.
*  endloop.
*
*  delete it_order where remqty <= 0.

  IF it_order[] IS INITIAL.
    MESSAGE s000 WITH 'Available Prod. Order not found.'.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  SORT IT_ORDER BY FTRMI DESCENDING AUFNR DESCENDING.
  CLEAR: IT_HELP_VALUE, IT_HELP_VALUE[].
  it_help_value-tabname    = 'AFKO'.
  it_help_value-fieldname  = 'AUFNR'.
  it_help_value-selectflag = 'X'.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'AFKO'.
  it_help_value-fieldname  = 'FTRMI'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'ZSPP_COMMON'.
  it_help_value-fieldname  = 'ORQTY'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.
  it_help_value-tabname    = 'ZSPP_COMMON'.
  it_help_value-fieldname  = 'REMQTY'.
  it_help_value-selectflag = ''.    "<=Selected value
  APPEND it_help_value.

  CLEAR: IT_VALUETAB, IT_VALUETAB[].
  LOOP AT it_order.
    it_valuetab-value = it_order-AUFNR.
    APPEND it_valuetab.
    it_valuetab-value = it_order-FTRMI.
    APPEND it_valuetab.
    WRITE IT_ORDER-PSMNG TO it_valuetab-value(10)
          UNIT 'EA' RIGHT-JUSTIFIED.
    APPEND it_valuetab.
    WRITE IT_ORDER-REMQTY TO it_valuetab-value(10)
          UNIT 'EA' RIGHT-JUSTIFIED.
    APPEND it_valuetab.
  ENDLOOP.

  CLEAR: lv_index.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      show_all_values_at_first_time = 'X'
    IMPORTING
      index                         = lv_index
    TABLES
      fields                        = it_help_value
      select_values                 = it_help_vtab
      valuetab                      = it_valuetab
    EXCEPTIONS
      field_not_in_ddic             = 1
      more_then_one_selectfield     = 2
      no_selectfield                = 3
      OTHERS                        = 4.

  READ TABLE it_order INDEX lv_index.
  IF sy-subrc NE 0. EXIT. ENDIF.

  MOVE: it_order-AUFNR TO WA_SCREEN-AUFNR.

  LEAVE TO SCREEN SY-DYNNR.

ENDMODULE.                 " GET_PROD_ORDER  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_0130
*&---------------------------------------------------------------------*
FORM CHECK_SCREEN_0130 .
  CLEAR : l_input_error.

  PERFORM check_screen_common.
  PERFORM call_alpha_conversion CHANGING wa_screen-aufnr.

  IF wa_screen-aufnr IS INITIAL.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'INPUT Prod.Order'.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM afko
  WHERE aufnr = wa_screen-aufnr
    AND PLNBEZ  = wa_screen-bpart.
  IF sy-subrc <> 0.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'Prod.Order material is diffrent from MV Part.'.
    EXIT.
  ENDIF.

  READ TABLE it_ORDER WITH KEY wa_screen-aufnr.
  IF wa_screen-menge > it_order-remqty.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'Iinput Quantity is bigger than Prod. Order'
                      'remaining quantity'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_SCREEN_0130
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_COMMON
*&---------------------------------------------------------------------*
FORM CHECK_SCREEN_COMMON .
  SELECT SINGLE * FROM marc
  WHERE werks = wa_screen-werks
    AND matnr = wa_screen-bpart.
  IF sy-subrc <> 0.
    l_input_error = 'X'.
    IF sy-dynnr = '0110'.
      MESSAGE s000 WITH 'AS PART is not available in' wa_screen-werks.
    ELSE.
      MESSAGE s000 WITH 'MV PART is not available in' wa_screen-werks.
    ENDIF.
    EXIT.
  ENDIF.

  IF wa_screen-MENGE IS INITIAL.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'Please, input Quantity.'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_SCREEN_COMMON
*&---------------------------------------------------------------------*
*&      Form  PROCESS_0130
*&---------------------------------------------------------------------*
FORM PROCESS_0130 .
  DATA: it_Header LIKE BAPI_PP_HDRLEVEL      OCCURS 0 WITH HEADER LINE,
        it_item LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        IT_RETURN LIKE BAPI_CORU_RETURN      OCCURS 0 WITH HEADER LINE,
        IT_LINK LIKE BAPI_LINK_CONF_GOODSMOV OCCURS 0 WITH HEADER LINE,
        st_return1    LIKE bapiret1.

  DATA : l_rsnum LIKE afko-rsnum,
         it_resb LIKE resb OCCURS 0 WITH HEADER LINE.

  CLEAR : it_header[], it_header, it_item[], it_item, it_link[],
          it_link, it_resb[].

  CHECK l_input_error IS INITIAL.

*-Header
  it_header-orderid         = wa_screen-aufnr.
  it_header-FIN_CONF        = 'X'.
  it_header-POSTG_DATE      = wa_screen-budat.
  it_header-CONF_QUAN_UNIT  = wa_screen-meins.
  it_header-YIELD           = 0.
  it_header-EXEC_START_DATE = SY-DATUM.
  it_header-EXEC_START_TIME = SY-UZEIT.
  it_header-EXEC_FIN_DATE   = SY-DATUM.
  it_header-EXEC_FIN_TIME   = SY-UZEIT.
  it_header-CONF_TEXT       = 'RETURN-TO-LINE'.
  APPEND IT_HEADER.

*-Item
  SELECT SINGLE LGPRO INTO  L_LGORT
  FROM MARC
  WHERE MATNR = wa_screen-bpart
    AND WERKS = wa_screen-werks.

  SELECT SINGLE rsnum INTO l_rsnum
  FROM afko
  WHERE AUFNR = wa_screen-aufnr.

  SELECT * INTO TABLE it_resb
  FROM resb
  WHERE rsnum =   l_rsnum
    AND dumps <> 'X'.
  IF sy-subrc <> 0.
    MESSAGE E000 WITH 'There is no confirmation material.'.
  ENDIF.

  CLEAR: IT_ITEM, IT_LINK.
  IT_ITEM-MATERIAL   = wa_screen-bpart.
  IT_ITEM-PLANT      = wa_screen-WERKS.
  IT_ITEM-STGE_LOC   = L_LGORT.
  IT_ITEM-MOVE_TYPE  = '102'.
  IT_ITEM-ENTRY_QNT  = WA_SCREEN-MENGE.
  IT_ITEM-ENTRY_UOM  = WA_SCREEN-MEINS.
  it_item-mvt_ind    = 'F'.
  it_item-NO_MORE_GR = 'X'.
  it_item-line_id    = 1.

  APPEND IT_ITEM.

  IT_LINK-INDEX_CONFIRM = 1.
  IT_LINK-INDEX_GOODSMOV = 1.
  APPEND IT_LINK.

  LOOP AT IT_RESB.

    CLEAR: IT_ITEM, IT_LINK.
    IT_ITEM-MATERIAL   = it_resb-MATNR.
    IT_ITEM-PLANT      = it_resb-WERKS.
    IT_ITEM-STGE_LOC   = it_resb-LGORT.
    IT_ITEM-MOVE_TYPE  = '262'.

    IT_ITEM-ENTRY_QNT  = WA_SCREEN-MENGE * AFKO-GAMNG
                        / IT_RESB-ERFMG.
    IT_ITEM-ENTRY_UOM  = it_resb-MEINS.
    it_item-NO_MORE_GR = 'X'.
*    it_item-mvt_ind    = 'F'.
    it_item-mvt_ind    = ''.
    it_item-line_id    = SY-TABIX + 1.

    APPEND IT_ITEM.

    IT_LINK-INDEX_CONFIRM = 1.
    IT_LINK-INDEX_GOODSMOV = it_item-line_id.
    APPEND IT_LINK.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_HDR'
    IMPORTING
      RETURN             = st_return1
    TABLES
      ATHDRLEVELS        = it_header
      GOODSMOVEMENTS     = it_item
      LINK_CONF_GOODSMOV = IT_LINK
      DETAIL_RETURN      = IT_RETURN.

  READ TABLE IT_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    l_input_error = 'X'.
    MESSAGE S000 WITH IT_RETURN-MESSAGE.
    EXIT.
  ELSE.
    PERFORM update_asmpgi_0130.
    message s000 with 'Order Confirmation has been processed'
                      'successfully'.
  ENDIF.


*  data : l_date type sy-datum,
*         l_budat type sy-datum,
*         l_menge(10), l_menge_int type i.
*
*  clear : mess_tab[], it_bdc[], mess_tab, it_bdc.
*
*  check l_input_error is INITIAL.
*
*  write : sy-datum to l_date, "Change Date format
*          wa_screen-budat to l_budat.
*
*  l_menge_int = wa_screen-menge.
*  l_menge  = l_menge_int. CONDENSE l_menge.
*
*
*  perform bdc_dynpro using :
*          'X' 'SAPLCORU'             '0400',
*          ' ' 'BDC_CURSOR'           'CORUF-AUFNR',
*          ' ' 'BDC_OKCODE'           '/00',
*          ' ' 'CORUF-AUFNR'           wa_screen-aufnr.
*
*  perform bdc_dynpro using :
*          'X' 'SAPLCORU'             '0410',
*          ' ' 'BDC_OKCODE'           '=MB03',
*          ' ' 'BDC_CURSOR'           'AFRUD-BUDAT',
*          ' ' 'CORUF-ENDRU'          'X',
*          ' ' 'CORUF-AUTER'           '',
**          ' ' 'AFRUD-LMNGA'           wa_screen-menge,
*          ' ' 'AFRUD-LMNGA'           l_menge,
*          ' ' 'AFRUD-MEINH'           wa_screen-meins,
*
*          ' ' 'AFRUD-ISDD'            l_date,
*          ' ' 'AFRUD-ISDZ'            sy-uzeit,
*          ' ' 'AFRUD-IEDD'            l_date,
*          ' ' 'AFRUD-IEDZ'            sy-uzeit,
*          ' ' 'AFRUD-BUDAT'           l_budat.
*
*  perform bdc_dynpro using :
*          'X' 'SAPLCOWB'             '0130',
*          ' ' 'BDC_OKCODE'           '=WEIT',
*          ' ' 'BDC_CURSOR'           'COWB_COMP-BWART(02)',
*          ' ' 'COWB_COMP-BWART(01)'  '102',
*          ' ' 'COWB_COMP-BWART(02)'  '262'.
*
*  call transaction 'CO15' using it_bdc mode disp_mode
*                                update 'S'
*                                messages into mess_tab.

*  perform read_message_0130.


ENDFORM.                    " PROCESS_0130
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO  USING dynbegin name value.

  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: dynbegin TO it_bdc-dynbegin,
          name TO it_bdc-program,
          value TO it_bdc-dynpro.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.


ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0130 .
  DATA : l_mblnr LIKE mseg-mblnr,
         l_time  LIKE sy-uzeit.

  l_time  = sy-uzeit - 10.
  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgnr  = '110'
                               msgv1  = '2'.
  IF sy-subrc = 0.
    DO 100 TIMES.
      SELECT SINGLE b~mblnr INTO l_mblnr
      FROM mkpf AS a INNER JOIN mseg AS b
                  ON a~mblnr  = b~mblnr
                 AND a~MJAHR  = b~MJAHR
      WHERE b~aufnr = wa_screen-aufnr
        AND b~bwart = '102'
        AND b~matnr = wa_screen-bpart
        AND b~zbudat = wa_screen-budat
        AND b~werks  = wa_screen-werks
        AND a~cpudt  = sy-datum
        AND a~cputm  BETWEEN l_time AND sy-uzeit
        AND a~usnam  = sy-uname.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDDO.

*-Success
    IF l_mblnr IS NOT INITIAL.
      wa_screen-mblnr = l_mblnr.
      PERFORM update_asmpgi_0130.
    ELSE.

*-  Error
    ENDIF.

  ELSE.
    READ TABLE mess_tab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = lv_msg.
    ENDIF.

    MESSAGE e000 WITH lv_msg.

  ENDIF.
ENDFORM.                    " READ_MESSAGE_0130
*&---------------------------------------------------------------------*
*&      Form  CALL_ALPHA_CONVERSION
*&---------------------------------------------------------------------*
FORM CALL_ALPHA_CONVERSION  CHANGING P_para.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_para
    IMPORTING
      OUTPUT = P_para.

ENDFORM.                    " CALL_ALPHA_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ASMPGI_0130
*&---------------------------------------------------------------------*

FORM UPDATE_ASMPGI_0130 .
  DATA : l_time TYPE t.

  CLEAR : wa_asmpgi.

  l_time = sy-uzeit - 10.


  wa_screen-GITYP = 'RT'.
  wa_screen-ZRESULT = 'S'.
  wa_screen-ERNAM = sy-uname.
  wa_screen-ERDAT = sy-datum.
  wa_screen-ERZET = sy-uzeit.
  wa_screen-AENAM = sy-uname.
  wa_screen-AEDAT = sy-datum.
  wa_screen-AEZET = sy-uzeit.
  wa_screen-DIRCT = '1'.

  SELECT SEQNO INTO wa_screen-SEQNO FROM ztpp_asmpgi
  UP TO 1 ROWS
  WHERE WERKS = wa_screen-werks
    AND BPART = wa_screen-bpart
    AND GITYP = wa_screen-gityp
    AND BUDAT = wa_screen-budat
  ORDER BY seqno DESCENDING.
  ENDSELECT.

  wa_screen-SEQNO = wa_screen-SEQNO  + 1.

  MOVE-CORRESPONDING wa_screen TO wa_asmpgi.
  INSERT ztpp_asmpgi FROM wa_asmpgi.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    DO 10 TIMES.
      SELECT SINGLE b~mblnr INTO wa_screen-mblnr
      FROM mkpf AS a INNER JOIN mseg AS b
                  ON a~mblnr  = b~mblnr
                 AND a~MJAHR  = b~MJAHR
      WHERE b~aufnr = wa_screen-aufnr
        AND b~bwart = '102'
        AND b~matnr = wa_screen-bpart
        AND b~zbudat = wa_screen-budat
        AND b~werks  = wa_screen-werks
        AND a~cpudt  = sy-datum
        AND a~cputm  BETWEEN l_time AND sy-uzeit
        AND a~usnam  = sy-uname.
      IF sy-subrc = 0.
        wa_asmpgi-mblnr = wa_screen-mblnr.

        UPDATE ztpp_asmpgi
          SET: mblnr = wa_asmpgi-mblnr
         WHERE werks = wa_asmpgi-werks
           AND bpart = wa_asmpgi-bpart
           AND gityp = wa_asmpgi-gityp
           AND budat = wa_asmpgi-budat
           AND seqno = wa_asmpgi-seqno.

        COMMIT WORK AND WAIT.
        EXIT.
      ENDIF.
    ENDDO.

    PERFORM append_it_main.
  ELSE.
    l_input_error = 'X'.
    MESSAGE S000 WITH 'TABLE UPDATE FAILED ZTPP_ASMPGI'.
    EXIT.
  ENDIF.
ENDFORM.                    " UPDATE_ASMPGI_0130
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_MAIN
*&---------------------------------------------------------------------*
FORM APPEND_IT_MAIN .
  CLEAR : it_main.

  MOVE-CORRESPONDING wa_asmpgi TO it_main.
  it_main-maktx = wa_screen-maktx.

  IF it_MAIN-DIRCT = '1'.
    it_MAIN-DIR_ICON = ICON_ARROW_RIGHT.
  ELSE.
    it_MAIN-DIR_ICON = ICON_ARROW_LEFT.
  ENDIF.

  IF it_MAIN-ZRESULT = 'S'.
    it_MAIN-STAT_ICON = ICON_LED_GREEN.
  ELSEIF it_MAIN-ZRESULT = 'P' OR it_MAIN-ZRESULT = 'E'.
    it_MAIN-STAT_ICON = ICON_LED_YELLOW.
  ENDIF.

  APPEND it_main.
  G_INDEX = sy-tabix.  "Update row

  APPEND 'CREATE' TO extab.  "Hide Create Icon
ENDFORM.                    " APPEND_IT_MAIN
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
FORM REFRESH_GRID .
  CHECK l_input_error IS INITIAL.

  gs_stbl-row = 'X'.
  gs_stbl-col = 'X'.

  CALL METHOD GK_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
*     i_soft_refresh = 'X'
      is_stable      = gs_stbl.
ENDFORM.                    " REFRESH_GRID
*&---------------------------------------------------------------------*
*&      Module  GET_REASON_CODE  INPUT
*&---------------------------------------------------------------------*
MODULE GET_REASON_CODE INPUT.

  DATA : BEGIN OF it_reason OCCURS 0,
           GRUND LIKE t157E-grund,
           GRTXT LIKE t157E-grtxt,
         END OF it_reason.

  CLEAR : dynpfields[], dynpfields, it_reason[], it_reason.

  SELECT *
      INTO CORRESPONDING FIELDS OF TABLE IT_reason
  FROM T157E AS A
   WHERE A~SPRAS = SY-LANGU
     AND A~bwart = '551'.

  CHECK it_REASON[] IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'GRUND'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'WA_SCREEN-GRUND'
      value_org   = 'S'
    TABLES
      value_tab   = it_REASON
      return_tab  = it_value.

  READ TABLE it_value INDEX 1.
  WA_SCREEN-GRUND = it_value-fieldval.

ENDMODULE.                 " GET_REASON_CODE  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_0120
*&---------------------------------------------------------------------*
FORM CHECK_SCREEN_0120 .

  CLEAR : l_input_error .

  PERFORM check_screen_common.
  PERFORM check_part_bom.

*  perform call_alpha_conversion CHANGING wa_screen-aufnr.

  SELECT SINGLE * FROM marc
  WHERE werks = wa_screen-werks
    AND matnr =  WA_SCREEN-ASPART_NM.
  IF sy-subrc <> 0.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'MP Part is not abailable in' wa_screen-werks.
    EXIT.
  ENDIF.

  IF wa_screen-MPDST IS INITIAL.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'Input Destination'.
    EXIT.
  ENDIF.

  IF WA_SCREEN-GRUND IS INITIAL.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'Input Reason Code'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_SCREEN_0120
*&---------------------------------------------------------------------*
*&      Form  PROCESS_0120
*&---------------------------------------------------------------------*
FORM PROCESS_0120 .

  l_mvpart = strlen( wa_screen-bpart ).
  l_mppart = strlen( wa_screen-ASPART_NM ).

  if l_mvpart = l_mppart.  "Same Digit
    perform process_0120_1.
  else.
    perform process_0120_2.
  endif.

ENDFORM.                    " PROCESS_0120
*&---------------------------------------------------------------------*
*&      Form  CHECK_PART_BOM
*&---------------------------------------------------------------------*
FORM CHECK_PART_BOM .
  DATA : l_idnrk TYPE stpo-idnrk.

  SELECT   SINGLE idnrk INTO l_idnrk
  FROM mast AS a INNER JOIN stpo AS b
           ON a~stlnr = b~stlnr
  WHERE a~matnr = WA_SCREEN-ASPART_NM
    AND b~idnrk = wa_screen-bpart
    AND a~werks = wa_screen-werks
    AND a~stlan = '1'
    AND b~stlty = 'M'.
  IF sy-subrc <> 0.
    l_input_error = 'X'.
    IF sy-dynnr = '0110'.
      MESSAGE S000 WITH 'B-PART CAN NOT BE CHANGED WITH AS PART'
                        'CHECK AS PART BOM'.
    ELSE.
      MESSAGE S000 WITH 'B-PART CAN NOT BE CHANGED WITH MP PART'
                        'CHECK MP PART BOM'.
    ENDIF.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_PART_BOM
*&---------------------------------------------------------------------*
*&      Form  INSERT_ASMPGI_0120
*&---------------------------------------------------------------------*
FORM Insert_ASMPGI_0120 .
  CLEAR : wa_asmpgi.

  wa_screen-GITYP = 'MP'.
  if l_mvpart = l_mppart.  "Same Digit
    wa_screen-ZRESULT = 'S'.
  else.
    wa_screen-ZRESULT = 'P'.
  endif.
  wa_screen-ERNAM = sy-uname.
  wa_screen-ERDAT = sy-datum.
  wa_screen-ERZET = sy-uzeit.
  wa_screen-AENAM = sy-uname.
  wa_screen-AEDAT = sy-datum.
  wa_screen-AEZET = sy-uzeit.
  wa_screen-DIRCT = '1'.

  SELECT SEQNO INTO wa_screen-SEQNO FROM ztpp_asmpgi
  UP TO 1 ROWS
  WHERE WERKS = wa_screen-werks
    AND BPART = wa_screen-bpart
    AND GITYP = wa_screen-gityp
    AND BUDAT = wa_screen-budat
  ORDER BY seqno DESCENDING.
  ENDSELECT.

  wa_screen-SEQNO = wa_screen-SEQNO  + 1.

  MOVE-CORRESPONDING wa_screen TO wa_asmpgi.
  INSERT ztpp_asmpgi FROM wa_asmpgi.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM append_it_main.
  ELSE.
    ROLLBACK WORK.
    l_input_error = 'X'.
    MESSAGE S000 WITH 'TABLE UPDATE FAILED ZTPP_ASMPGI'.
    EXIT.
*    message e000 with 'Table Update failed ZTPP_ASMPGI'.
  ENDIF.
ENDFORM.                    " INSERT_ASMPGI_0120
*&---------------------------------------------------------------------*
*&      Form  CREATE_MB1A_0120
*&---------------------------------------------------------------------*
FORM CREATE_MB1A_0120 .
  DATA : l_date TYPE sy-datum,
         l_budat TYPE sy-datum,
         l_menge(10), l_menge_int TYPE i.

  CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

  CHECK l_input_error IS INITIAL.

  WRITE : sy-datum TO l_date, "Change Date format
          wa_screen-budat TO l_budat.

  l_menge_int = wa_screen-menge.
  l_menge  = l_menge_int. CONDENSE l_menge.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMM07M'             '0400',
          ' ' 'BDC_CURSOR'           'RM07M-GRUND',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'MKPF-BLDAT'           l_date,
          ' ' 'MKPF-BUDAT'           l_budat,
          ' ' 'MKPF-BKTXT'           'MOVING PART SCRAP',
          ' ' 'RM07M-BWARTWA'        '551',
          ' ' 'RM07M-WERKS'          wa_screen-werks,
          ' ' 'RM07M-GRUND'          wa_screen-grund,
          ' ' 'RM07M-LGORT'          l_lgort,
          ' ' 'XFULL'                'X',
          ' ' 'RM07M-WVERS2'         'X'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMM07M'             '0421',
          ' ' 'BDC_CURSOR'           'MSEG-ERFME(01)',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'MSEG-MATNR(01)'       wa_screen-MATNR,
          ' ' 'MSEG-ERFMG(01)'       l_menge,
          ' ' 'MSEG-ERFME(01)'       wa_screen-meins .

  PERFORM bdc_dynpro USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_CURSOR'           'COBL-AUFNR',
          ' ' 'BDC_OKCODE'           '=ENTE',
          ' ' 'COBL-AUFNR'           'CP001'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_CURSOR'           'COBL-AUFNR',
          ' ' 'BDC_OKCODE'           '=ENTE',
          ' ' 'COBL-AUFNR'           'CP001'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMM07M'             '0421',
          ' ' 'BDC_CURSOR'           'MSEG-ERFMG(01)',
          ' ' 'BDC_OKCODE'           '=BU',
          ' ' 'DKACB-FMORE'           'X'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE',
          ' ' 'BDC_CURSOR'           'COBL-AUFNR'.

  CALL TRANSACTION 'MB1A' USING it_bdc MODE disp_mode
                                UPDATE 'S'
                                MESSAGES INTO mess_tab.

  PERFORM read_message_0120.
ENDFORM.                    " CREATE_MB1A_0120
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0120
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0120 .

  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgnr  = '060'.
  IF sy-subrc = 0.
    WA_SCREEN-MBLNR = MESS_TAB-MSGV1.
    WA_SCREEN-ZRESULT = 'S'.
    WA_SCREEN-ZMSG    = ''.
    PERFORM update_asmpgi_0120.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = lv_msg.

      WA_SCREEN-ZRESULT = 'E'.
      WA_SCREEN-ZMSG    = lv_msg.
      l_input_error = 'X'.

      PERFORM update_asmpgi_0120.
      MESSAGE s000 WITH lv_msg.
      EXIT.

    ENDIF.

  ENDIF.

ENDFORM.                    " READ_MESSAGE_0120
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ASMPGI_0120
*&---------------------------------------------------------------------*
FORM UPDATE_ASMPGI_0120 .

  wa_screen-AENAM = sy-uname.
  wa_screen-AEDAT = sy-datum.
  wa_screen-AEZET = sy-uzeit.

  UPDATE ztpp_asmpgi
    SET MBLNR = wa_screen-MBLNR
        AENAM = wa_screen-AENAM
        AEDAT = wa_screen-AEDAT
        AEZET = wa_screen-AEZET
        ZRESULT = wa_screen-ZRESULT
        ZMSG  =  wa_screen-ZMSG
  WHERE WERKS = wa_screen-WERKS
    AND BPART = wa_screen-BPART
    AND GITYP = wa_screen-GITYP
    AND BUDAT = wa_screen-BUDAT
    AND SEQNO = wa_screen-SEQNO.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM UPDATE_it_main.
  ELSE.
    ROLLBACK WORK.
    l_input_error = 'X'.
    MESSAGE S000 WITH 'TABLE UPDATE FAILED ZTPP_ASMPGI'.
    EXIT.
  ENDIF.

ENDFORM.                    " UPDATE_ASMPGI_0120
*&---------------------------------------------------------------------*
*&      Form  UPDATE_IT_MAIN
*&---------------------------------------------------------------------*
FORM UPDATE_IT_MAIN .

  IF G_INDEX IS INITIAL.
    G_INDEX =  1.
  ENDIF.

*  IF OK_CODE = 'CREATE'.
  IF save_ok = 'CREATE' OR
     save_ok = 'REPROC'.
    MOVE-CORRESPONDING wa_SCREEN TO wa_main.
  ENDIF.

*  it_main-maktx = wa_screen-maktx.

  IF wa_MAIN-DIRCT = '1'.
    wa_MAIN-DIR_ICON = ICON_ARROW_RIGHT.
  ELSE.
    wa_MAIN-DIR_ICON = ICON_ARROW_LEFT.
  ENDIF.

  IF wa_MAIN-ZRESULT = 'S'.
    wa_MAIN-STAT_ICON = ICON_LED_GREEN.
  ELSEIF wa_MAIN-ZRESULT = 'P' OR wa_MAIN-ZRESULT = 'E'.
    wa_MAIN-STAT_ICON = ICON_LED_YELLOW.
  ENDIF.

  MODIFY it_main INDEX G_INDEX  FROM wa_MAIN.

*  IF save_ok = 'CREATE'.
*    append 'CREATE' to extab.  "Hide Create Icon
*  ENDIF.
ENDFORM.                    " UPDATE_IT_MAIN
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_0110
*&---------------------------------------------------------------------*
FORM CHECK_SCREEN_0110 .
  CLEAR : l_input_error .

  PERFORM check_screen_common.
  PERFORM check_part_bom.

  SELECT SINGLE * FROM marc
  WHERE werks = wa_screen-werks
    AND matnr =  WA_SCREEN-ASPART_NM.
  IF sy-subrc <> 0.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'AS Part is not available' wa_screen-werks.
    EXIT.
  ENDIF.

  IF WA_SCREEN-VBELN IS INITIAL.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'Select REF.S/O'.
    EXIT.
  ENDIF.

  READ TABLE it_order_qty WITH KEY vbeln  = wa_screen-vbeln
                                   posnr  = wa_screen-posnr.
  IF wa_screen-MENGE > it_order_qty-remqty.
    l_input_error = 'X'.
    MESSAGE s000 WITH 'REF.S/O Remain quantity is' it_order_qty-remqty.
    EXIT.
  ENDIF.

  IF WA_SCREEN-VBELN IS INITIAL.
    l_input_error = 'X'.
    MESSAGE s000 WITH ''.
    EXIT.

  ENDIF.

ENDFORM.                    " CHECK_SCREEN_0110
*&---------------------------------------------------------------------*
*&      Form  PROCESS_0110
*&---------------------------------------------------------------------*
FORM PROCESS_0110 .
  DATA: wa_header LIKE BAPI2017_GM_HEAD_01,
        it_item LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        it_return LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
        l_mat_doc LIKE BAPI2017_GM_HEAD_RET-MAT_DOC,
        l_doc_year LIKE BAPI2017_GM_HEAD_RET-DOC_YEAR.

  CLEAR : wa_header, it_item[], it_item, it_return[], it_return.

  CHECK     l_input_error  IS INITIAL.

  wa_HEADER-PSTNG_DATE = wa_screen-budat.
  wa_HEADER-DOC_DATE = sy-datum.
  wa_HEADER-HEADER_TXT = 'AS/MP DELIVERY M-TO-M'.

  SELECT SINGLE LGPRO INTO  it_ITEM-STGE_LOC
  FROM MARC
  WHERE MATNR = wa_screen-bpart
    AND WERKS = wa_screen-werks.

  SELECT SINGLE LGPRO INTO  it_ITEM-MOVE_STLOC
  FROM MARC
  WHERE MATNR = wa_screen-aspart_NM
    AND WERKS = wa_screen-werks.

  L_LGORT = it_ITEM-STGE_LOC. "B-part storage location

  it_ITEM-MATERIAL = wa_screen-bpart.
  it_ITEM-PLANT    = wa_screen-werks.
  it_ITEM-MOVE_MAT = wa_screen-aspart_NM.
  it_ITEM-MOVE_PLANT = wa_screen-werks.
  it_ITEM-MOVE_TYPE = '309'.
  it_ITEM-ENTRY_QNT = wa_screen-MENGE.
  it_ITEM-ENTRY_UOM = wa_screen-MEINS.
  APPEND IT_ITEM.

*-M-to-M
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER  = wa_header
      GOODSMVT_CODE    = '04'
    IMPORTING
      MATERIALDOCUMENT = l_mat_doc
      MATDOCUMENTYEAR  = l_doc_year
    TABLES
      GOODSMVT_ITEM    = it_item
      RETURN           = it_return.

  READ TABLE it_return WITH KEY TYPE = 'E'.

  IF l_mat_doc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    l_input_error = 'X'.
    MESSAGE S000 WITH it_return-MESSAGE.
    EXIT.
*    message e000 with it_return-MESSAGE.

  ELSE.
    wa_screen-M2MDOC = l_mat_doc.
    wa_screen-matnr = wa_screen-aspart_NM.
    PERFORM INSERT_asmpgi_0110.
  ENDIF.

*-Create Delivery and Good Issue
  PERFORM create_vl01n_0110.
ENDFORM.                    " PROCESS_0110
*&---------------------------------------------------------------------*
*&      Form  INSERT_ASMPGI_0110
*&---------------------------------------------------------------------*
FORM INSERT_ASMPGI_0110 .
  CLEAR : wa_asmpgi.

  wa_screen-GITYP = 'DL'.
  wa_screen-ZRESULT = 'P'.
  wa_screen-ERNAM = sy-uname.
  wa_screen-ERDAT = sy-datum.
  wa_screen-ERZET = sy-uzeit.
  wa_screen-AENAM = sy-uname.
  wa_screen-AEDAT = sy-datum.
  wa_screen-AEZET = sy-uzeit.
  wa_screen-DIRCT = '1'.

  SELECT SEQNO INTO wa_screen-SEQNO FROM ztpp_asmpgi
  UP TO 1 ROWS
  WHERE WERKS = wa_screen-werks
    AND BPART = wa_screen-bpart
    AND GITYP = wa_screen-gityp
    AND BUDAT = wa_screen-budat
  ORDER BY seqno DESCENDING.
  ENDSELECT.

  wa_screen-SEQNO = wa_screen-SEQNO  + 1.

  MOVE-CORRESPONDING wa_screen TO wa_asmpgi.
  INSERT ztpp_asmpgi FROM wa_asmpgi.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM append_it_main.
  ELSE.
*    message e000 with 'Table Update failed ZTPP_ASMPGI'.
    l_input_error = 'X'.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'TABLE UPDATE FAILED ZTPP_ASMPGI'.
    EXIT.
  ENDIF.
ENDFORM.                    " INSERT_ASMPGI_0110
*&---------------------------------------------------------------------*
*&      Form  CREATE_VL01N_0110
*&---------------------------------------------------------------------*
FORM CREATE_VL01N_0110 .
  DATA : l_date TYPE sy-datum,
         l_today TYPE sy-datum,
         l_budat TYPE sy-datum,
         l_menge(10), l_menge_int TYPE i.

  CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

  CHECK l_input_error IS INITIAL.

  l_date = SY-DATUM + 365.

  WRITE : L_DATE   TO l_date, "Change Date format
          sy-datum TO l_today,
          wa_screen-budat TO l_budat.

  l_menge_int = wa_screen-menge.
  l_menge  = l_menge_int. CONDENSE l_menge.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMV50A'             '4001',
          ' ' 'BDC_CURSOR'           'LV50C-BIPOS',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'LIKP-VSTEL'           'P200',
          ' ' 'LV50C-DATBI'           l_date,
          ' ' 'LV50C-VBELN'           wa_screen-vbeln,
          ' ' 'LV50C-ABPOS'           wa_screen-POSNR,
          ' ' 'LV50C-BIPOS'           wa_screen-posnr.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMV50A'             '1000',
          ' ' 'BDC_CURSOR'           'LIPSD-G_LFIMG(01)',
          ' ' 'BDC_OKCODE'           '=WABU_T',
          ' ' 'LIKP-BLDAT'           l_today,
          ' ' 'LIKP-WADAT'            l_budat,
          ' ' 'LIKP-WAUHR'            sy-uzeit,
*          ' ' 'LIKP-BTGEW'           l_menge,
*          ' ' 'LIKP-GEWEI'           wa_screen-meins,
          ' ' 'LIKP-WADAT_IST'       l_budat,
          ' ' 'LIPSD-G_LFIMG(01)'    l_menge.

  CALL TRANSACTION 'VL01N' USING it_bdc MODE disp_mode
                                 UPDATE 'S'
                                 MESSAGES INTO mess_tab.

  PERFORM read_message_0110.
ENDFORM.                    " CREATE_VL01N_0110
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0110
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0110 .

  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgnr  = '311'.
  IF sy-subrc = 0.
    WA_SCREEN-DLDOC = MESS_TAB-MSGV2.
    WA_SCREEN-ZRESULT = 'S'.
    WA_SCREEN-ZMSG    = ''.

    PERFORM call_alpha_conversion CHANGING WA_SCREEN-DLDOC.

    DO 10 TIMES.
      SELECT SINGLE MBLNR INTO WA_SCREEN-MBLNR
      FROM MKPF
      WHERE BUDAT = WA_SCREEN-BUDAT AND XBLNR =  WA_SCREEN-DLDOC.
      IF WA_SCREEN-MBLNR IS NOT INITIAL.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ENDDO.

    PERFORM update_asmpgi_0110.
    message s000 with 'Delivery AS Part has been processed'
                      'successfully'.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = lv_msg.

      WA_SCREEN-ZRESULT = 'E'.
      WA_SCREEN-ZMSG    = lv_msg.

      PERFORM update_asmpgi_0110.

*      message e000 with lv_msg.
      l_input_error = 'X'.
      ROLLBACK WORK.
      MESSAGE S000 WITH lv_msg.
      EXIT.
    ENDIF.


  ENDIF.
ENDFORM.                    " READ_MESSAGE_0110
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ASMPGI_0110
*&---------------------------------------------------------------------*
FORM UPDATE_ASMPGI_0110 .
  wa_screen-AENAM = sy-uname.
  wa_screen-AEDAT = sy-datum.
  wa_screen-AEZET = sy-uzeit.

  UPDATE ztpp_asmpgi
    SET MBLNR = wa_screen-MBLNR
        DLDOC = wa_screen-DLDOC
        AENAM = wa_screen-AENAM
        AEDAT = wa_screen-AEDAT
        AEZET = wa_screen-AEZET
        ZRESULT = wa_screen-ZRESULT
        ZMSG  =  wa_screen-ZMSG
  WHERE WERKS = wa_screen-WERKS
    AND BPART = wa_screen-BPART
    AND GITYP = wa_screen-GITYP
    AND BUDAT = wa_screen-BUDAT
    AND SEQNO = wa_screen-SEQNO.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM UPDATE_it_main.
  ELSE.
    l_input_error = 'X'.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'TABLE UPDATE FAILED ZTPP_ASMPGI'.
    EXIT.
  ENDIF.
ENDFORM.                    " UPDATE_ASMPGI_0110
*&---------------------------------------------------------------------*
*&      Form  REPROCESSING
*&---------------------------------------------------------------------*
FORM REPROCESSING .
  DATA : it_row  TYPE lvc_t_row,
         is_row  TYPE lvc_s_row,
         it_roid TYPE lvc_t_roid.

  CLEAR : wa_main, l_input_error.

  CALL METHOD gk_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_row
      et_row_no     = it_roid.

  IF it_row[] IS INITIAL.
    MESSAGE e000 WITH 'Select an item'.
*    exit.
  ELSE.
    READ TABLE it_row INDEX 1 INTO is_row.
    READ TABLE it_main INDEX is_row-index INTO wa_main.
  ENDIF.

  IF wa_main-zresult = 'S'.
    MESSAGE e000 WITH 'Selected item has already processed.'.
*    exit.
  ENDIF.

  IF wa_main-dirct = '2'.
    MESSAGE e000 WITH 'Selected item has been cancelling.'.
*    exit.
  ENDIF.

  G_INDEX = is_row-index.  "Update row

  CASE wa_main-GITYP.
    WHEN 'DL'.   "Delivery
      CHECK wa_main-dldoc IS INITIAL.
      MOVE-CORRESPONDING wa_main TO wa_screen.

      PERFORM create_vl01n_0110.

    WHEN 'MP'.  "Scrap - Only process in case MP&MV lengths different
      CHECK WA_MAIN-AUFNR IS NOT INITIAL.
      MOVE-CORRESPONDING wa_main TO wa_screen.

      l_mvpart = strlen( wa_screen-bpart ).
      l_mppart = strlen( wa_screen-matnr ).

      check l_mvpart <> l_mppart.  "Different Digit

      perform create_confirmation_0120.

*      PERFORM get_stor_location.
*      PERFORM create_mb1a_0120.

  ENDCASE.

  PERFORM refresh_grid.
ENDFORM.                    " REPROCESSING
*&---------------------------------------------------------------------*
*&      Form  GET_STOR_LOCATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_STOR_LOCATION
  .
  SELECT SINGLE LGPRO INTO L_LGORT
  FROM MARC
  WHERE MATNR = wa_screen-bpart
    AND WERKS = wa_screen-werks.

ENDFORM.                    " GET_STOR_LOCATION
*&---------------------------------------------------------------------*
*&      Form  CANCEL_PROCESS
*&---------------------------------------------------------------------*
FORM CANCEL_PROCESS .
  DATA : it_row  TYPE lvc_t_row,
         is_row  TYPE lvc_s_row,
         it_roid TYPE lvc_t_roid.

  DATA : l_confirm(1).

  CLEAR : wa_main,  l_input_error.

  CALL METHOD gk_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_row
      et_row_no     = it_roid.

  IF it_row[] IS INITIAL.
    MESSAGE e000 WITH 'Select an item.'.
*    exit.
  ELSE.
    READ TABLE it_row INDEX 1 INTO is_row.
    READ TABLE it_main INDEX is_row-index INTO wa_main.
  ENDIF.

  IF wa_main-dirct = '1'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR              = 'CONFIRMATION'
        TEXT_QUESTION         = text-t01
        TEXT_BUTTON_1         = 'YES'(001)
        TEXT_BUTTON_2         = 'NO'(002)
        display_cancel_button = space
        DEFAULT_BUTTON        = '2'
      IMPORTING
        ANSWER                = l_confirm.

    CHECK l_confirm = '1'.
  ENDIF.

  IF wa_main-zresult = 'S' AND wa_main-dirct = '2'.
    MESSAGE e000 WITH 'Selected item has already cancelled.'.
*    exit.
  ENDIF.

  IF wa_main-BUDAT_C IS INITIAL.  "Select Posting date for cancel
    wa_main-BUDAT_C = wa_main-budat.

    CALL SCREEN 0102 STARTING AT 10 5
                     ENDING   AT 50 6.
    IF save_ok = 'CANC_C'.
      CLEAR : wa_main-BUDAT_C.
      EXIT.
    ENDIF.
  ENDIF.
  G_INDEX = is_row-index.  "Update row


  CASE wa_main-GITYP.
    WHEN 'DL'.   "Delivery
      PERFORM cancel_0110.

    WHEN 'MP'.  "Scrap
      PERFORM CANCEL_0120.

    WHEN 'RT'.  "Return to Line
      PERFORM cancel_0130.

  ENDCASE.

  PERFORM refresh_grid.
ENDFORM.                    " CANCEL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0110
*&---------------------------------------------------------------------*
FORM CANCEL_0110 .
  PERFORM cancel_0110_gi.
  PERFORM cancel_0110_delivery.
  PERFORM CANCEL_0110_M2M.

ENDFORM.                    " CANCEL_0110
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0110_GI
*&---------------------------------------------------------------------*
FORM CANCEL_0110_GI .
  DATA : WA_EMKPF LIKE EMKPF,
         IT_MESG LIKE MESG OCCURS 0 WITH HEADER LINE,
         L_VBTYP LIKE LIKP-VBTYP.

  CHECK wa_main-MBLNR IS NOT INITIAL.
  CHECK l_input_error IS INITIAL.

*-Cancelled Document exist?
  SELECT SINGLE *
  FROM mseg AS a
  WHERE sjahr = wa_main-budat+0(4)
    AND smbln = wa_main-mblnr
    AND smblp = '1'.
  IF sy-subrc = 0.
    EXIT.     "Go to Next Cancel Process
  ENDIF.

  SELECT SINGLE VBTYP INTO L_VBTYP
  FROM LIKP WHERE VBELN = wa_main-dldoc.

  CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
    EXPORTING
      I_VBELN                   = wa_main-dldoc
      I_BUDAT                   = wa_main-budat_c
*     I_COUNT                   =
*     I_MBLNR                   =
      I_TCODE                   = 'VL09'
      I_VBTYP                   = L_VBTYP
    IMPORTING
      ES_EMKPF                  = WA_EMKPF
    TABLES
      T_MESG                    = IT_MESG
    EXCEPTIONS
      ERROR_REVERSE_GOODS_ISSUE = 1
      OTHERS                    = 2.
  IF SY-SUBRC = 0 AND WA_EMKPF-MBLNR IS NOT INITIAL.
    WA_MAIN-DIRCT   = '2'.
    WA_MAIN-ZRESULT = 'P'.
    WA_MAIN-ZMSG    = ''.

    PERFORM update_asmpgi_0110_CANC.
  ELSE.
    l_input_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE S000 WITH 'G/I Document cancel failed.'.
    EXIT.
  ENDIF.


ENDFORM.                    " CANCEL_0110_GI
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0102 OUTPUT.
  SET PF-STATUS '0102'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'SELECT' OR 'CANC_C'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ASMPGI_0110_CANC
*&---------------------------------------------------------------------*
FORM UPDATE_ASMPGI_0110_CANC .
  wa_MAIN-AENAM = sy-uname.
  wa_MAIN-AEDAT = sy-datum.
  wa_MAIN-AEZET = sy-uzeit.

  UPDATE ztpp_asmpgi
    SET DIRCT   = wa_MAIN-DIRCT
        BUDAT_C = wa_MAIN-BUDAT_C
        AENAM = wa_MAIN-AENAM
        AEDAT = wa_MAIN-AEDAT
        AEZET = wa_MAIN-AEZET
        ZRESULT = wa_MAIN-ZRESULT
        ZMSG    =  wa_MAIN-ZMSG
  WHERE WERKS = wa_MAIN-WERKS
    AND BPART = wa_MAIN-BPART
    AND GITYP = wa_MAIN-GITYP
    AND BUDAT = wa_MAIN-BUDAT
    AND SEQNO = wa_MAIN-SEQNO.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    PERFORM UPDATE_it_main.
  ELSE.
    l_input_error = 'X'.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'TABLE UPDATE FAILED ZTPP_ASMPGI'.
    EXIT.
  ENDIF.
ENDFORM.                    " UPDATE_ASMPGI_0110_CANC
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0110_DELIVERY
*&---------------------------------------------------------------------*
FORM CANCEL_0110_DELIVERY .
  CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

  CHECK wa_main-dldoc IS NOT INITIAL.
  CHECK l_input_error IS INITIAL.

  SELECT SINGLE * FROM likp
  WHERE vbeln = wa_main-dldoc.
  IF sy-subrc <> 0.
    EXIT.       ""Deleted ->Go to Next Cancel Process
  ENDIF.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMV50A'             '4004',
          ' ' 'BDC_CURSOR'           'LIKP-VBELN',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'LIKP-VBELN'           wa_main-dldoc.

  PERFORM bdc_dynpro USING :
          'X' 'SAPMV50A'             '1000',
          ' ' 'BDC_OKCODE'           '/ELOES_T'.

  CALL TRANSACTION 'VL02N' USING it_bdc MODE disp_mode
                                 UPDATE 'S'
                                 MESSAGES INTO mess_tab.

  PERFORM read_message_0110_vl02n.
ENDFORM.                    " CANCEL_0110_DELIVERY
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0110_VL02N
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0110_VL02N .
  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgnr  = '310'.
  IF sy-subrc = 0.
    WA_SCREEN-ZRESULT = 'P'.
    WA_SCREEN-ZMSG    = ''.
    PERFORM update_asmpgi_0110_CANC.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = lv_msg.

      WA_SCREEN-ZRESULT = 'E'.
      CONCATENATE '[DL]' lv_msg INTO WA_SCREEN-ZMSG.

      PERFORM update_asmpgi_0110_CANC.

      l_input_error = 'X'.
      MESSAGE S000 WITH lv_msg.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_MESSAGE_0110_VL02N
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0110_M2M
*&---------------------------------------------------------------------*
FORM CANCEL_0110_M2M .
  DATA : l_budat TYPE sy-datum.
  CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

  CHECK wa_main-M2MDOC IS NOT INITIAL.
  CHECK l_input_error IS INITIAL.

*-Cancelled Document exist?
  SELECT SINGLE *
  FROM mseg AS a
  WHERE sjahr = wa_main-budat+0(4)
    AND smbln = wa_main-m2MDOC
    AND smblp = '1'.
  IF sy-subrc = 0.
    EXIT.     "Don'T EXECUTE THIS PROCESS
  ENDIF.

  WRITE : WA_MAIN-BUDAT_C TO L_BUDAT.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPMM07M'             '0460',
          ' ' 'BDC_CURSOR'           'MKPF-BUDAT',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'MKPF-BUDAT'           L_BUDAT,
          ' ' 'RM07M-MBLNR'          WA_MAIN-M2MDOC,
          ' ' 'RM07M-MJAHR'          WA_MAIN-BUDAT_C+0(4),
          ' ' 'XFULL'                'X',
          ' ' 'RM07M-WVERS2'         'X'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPMM07M'             '0421',
          ' ' 'BDC_CURSOR'           'RM07M-XSELK(01)',
          ' ' 'BDC_OKCODE'           '=BU',
          ' ' 'DKACB-FMORE'          'X'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE'.

  CALL TRANSACTION 'MBST' USING IT_BDC MODE DISP_MODE
                                 UPDATE 'S'
                                 MESSAGES INTO MESS_TAB.

  PERFORM READ_MESSAGE_0110_MBST.
ENDFORM.                    " CANCEL_0110_M2M
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0110_MBST
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0110_MBST .
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                               MSGNR  = '060'.
  IF SY-SUBRC = 0.
    WA_MAIN-DIRCT   = '2'.
    WA_MAIN-ZRESULT = 'S'.
    WA_MAIN-ZMSG    = ''.
    PERFORM UPDATE_ASMPGI_0110_CANC.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = MESS_TAB-MSGID
          MSGNR               = MESS_TAB-MSGNR
          MSGV1               = MESS_TAB-MSGV1
          MSGV2               = MESS_TAB-MSGV2
          MSGV3               = MESS_TAB-MSGV3
          MSGV4               = MESS_TAB-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LV_MSG.

      WA_SCREEN-ZRESULT = 'E'.
      CONCATENATE '[M2M]' LV_MSG INTO WA_SCREEN-ZMSG.

      PERFORM UPDATE_ASMPGI_0110_CANC.

      L_INPUT_ERROR = 'X'.
      MESSAGE S000 WITH LV_MSG.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_MESSAGE_0110_MBST
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0120
*&---------------------------------------------------------------------*
FORM CANCEL_0120 .
  l_mvpart = strlen( wa_main-bpart ).
  l_mppart = strlen( wa_main-matnr ).

  if l_mvpart = l_mppart.  "Same Digit
*  PERFORM CANCEL_0120_SCRAP.
    PERFORM CANCEL_0120_M2M.
  else.
    if wa_main-zresult = 'S'.
      perform cancel_confirmation_0120.
    else.
      perform cancel_0120_co02.
    endif.

  endif.
ENDFORM.                    " CANCEL_0120
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0120_SCRAP
*&---------------------------------------------------------------------*
FORM CANCEL_0120_SCRAP .
  DATA : L_BUDAT TYPE SY-DATUM.
  CLEAR : MESS_TAB[], IT_BDC[], MESS_TAB, IT_BDC.

  CHECK WA_MAIN-MBLNR IS NOT INITIAL.
  CHECK L_INPUT_ERROR IS INITIAL.

*-Cancelled Document exist?
  SELECT SINGLE *
  FROM MSEG AS A
  WHERE SJAHR = WA_MAIN-BUDAT+0(4)
    AND SMBLN = WA_MAIN-MBLNR
    AND SMBLP = '1'.
  IF SY-SUBRC = 0.
    EXIT.     "Go to Next Cancel Process
  ENDIF.

  WRITE : WA_MAIN-BUDAT_C TO L_BUDAT.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPMM07M'             '0460',
          ' ' 'BDC_CURSOR'           'MKPF-BUDAT',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'MKPF-BUDAT'           L_BUDAT,
          ' ' 'RM07M-MBLNR'          WA_MAIN-MBLNR,
          ' ' 'RM07M-MJAHR'          WA_MAIN-BUDAT_C+0(4),
          ' ' 'XFULL'                'X',
          ' ' 'RM07M-WVERS2'         'X'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPMM07M'             '0421',
          ' ' 'BDC_CURSOR'           'RM07M-XSELK(01)',
          ' ' 'BDC_OKCODE'           '=BU',
          ' ' 'DKACB-FMORE'          'X'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE'.

  CALL TRANSACTION 'MBST' USING IT_BDC MODE DISP_MODE
                                 UPDATE 'S'
                                 MESSAGES INTO MESS_TAB.

  PERFORM READ_MESSAGE_0120_MBST.

ENDFORM.                    " CANCEL_0120_SCRAP
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0120_MBST
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0120_MBST .
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                               MSGNR  = '060'.
  IF SY-SUBRC = 0.
    WA_MAIN-DIRCT   = '2'.
    WA_MAIN-ZRESULT = 'P'.
    WA_MAIN-ZMSG    = ''.
    PERFORM UPDATE_ASMPGI_0110_CANC.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = MESS_TAB-MSGID
          MSGNR               = MESS_TAB-MSGNR
          MSGV1               = MESS_TAB-MSGV1
          MSGV2               = MESS_TAB-MSGV2
          MSGV3               = MESS_TAB-MSGV3
          MSGV4               = MESS_TAB-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LV_MSG.

      WA_SCREEN-ZRESULT = 'E'.
      WA_SCREEN-ZMSG    = LV_MSG.

      PERFORM UPDATE_ASMPGI_0110_CANC.

      L_INPUT_ERROR = 'X'.
      MESSAGE S000 WITH LV_MSG.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_MESSAGE_0120_MBST
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0120_M2M
*&---------------------------------------------------------------------*
FORM CANCEL_0120_M2M .
  DATA : L_BUDAT TYPE SY-DATUM.
  CLEAR : MESS_TAB[], IT_BDC[], MESS_TAB, IT_BDC.

  CHECK WA_MAIN-M2MDOC IS NOT INITIAL.
  CHECK L_INPUT_ERROR IS INITIAL.

*-Cancelled Document exist?
  SELECT SINGLE *
  FROM MSEG AS A
  WHERE SJAHR = WA_MAIN-BUDAT+0(4)
    AND SMBLN = WA_MAIN-M2MDOC
    AND SMBLP = '1'.
  IF SY-SUBRC = 0.
    EXIT.     "Don't execute this process
  ENDIF.

  WRITE : WA_MAIN-BUDAT_C TO L_BUDAT.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPMM07M'             '0460',
          ' ' 'BDC_CURSOR'           'MKPF-BUDAT',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'MKPF-BUDAT'           L_BUDAT,
          ' ' 'RM07M-MBLNR'          WA_MAIN-M2MDOC,
          ' ' 'RM07M-MJAHR'          WA_MAIN-BUDAT_C+0(4),
          ' ' 'XFULL'                'X',
          ' ' 'RM07M-WVERS2'         'X'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPMM07M'             '0421',
          ' ' 'BDC_CURSOR'           'RM07M-XSELK(01)',
          ' ' 'BDC_OKCODE'           '=BU',
          ' ' 'DKACB-FMORE'          'X'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE'.

  PERFORM BDC_DYNPRO USING :
          'X' 'SAPLKACB'             '0002',
          ' ' 'BDC_OKCODE'           '=ENTE'.

  CALL TRANSACTION 'MBST' USING IT_BDC MODE DISP_MODE
                                 UPDATE 'S'
                                 MESSAGES INTO MESS_TAB.

  PERFORM READ_MESSAGE_0110_MBST.
ENDFORM.                    " CANCEL_0120_M2M
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0130
*&---------------------------------------------------------------------*
FORM CANCEL_0130 .
  DATA: IT_HEADER LIKE BAPI_PP_HDRLEVEL      OCCURS 0 WITH HEADER LINE,
        IT_ITEM LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        IT_RETURN LIKE BAPI_CORU_RETURN      OCCURS 0 WITH HEADER LINE,
        IT_LINK LIKE BAPI_LINK_CONF_GOODSMOV OCCURS 0 WITH HEADER LINE,
        ST_RETURN1    LIKE BAPIRET1.

  DATA : L_RSNUM LIKE AFKO-RSNUM,
         IT_RESB LIKE RESB OCCURS 0 WITH HEADER LINE.

  CLEAR : IT_HEADER[], IT_HEADER, IT_ITEM[], IT_ITEM, IT_LINK[],
          IT_LINK, IT_RESB[].

  CHECK L_INPUT_ERROR IS INITIAL.

*-Cancelled Document exist?


*-Header
  IT_HEADER-ORDERID         = WA_MAIN-AUFNR.
  IT_HEADER-FIN_CONF        = 'X'.
  IT_HEADER-POSTG_DATE      = WA_MAIN-BUDAT_C.
  IT_HEADER-CONF_QUAN_UNIT  = WA_MAIN-MEINS.
  IT_HEADER-YIELD           = 0.
  IT_HEADER-EXEC_START_DATE = SY-DATUM.
  IT_HEADER-EXEC_START_TIME = SY-UZEIT.
  IT_HEADER-EXEC_FIN_DATE   = SY-DATUM.
  IT_HEADER-EXEC_FIN_TIME   = SY-UZEIT.
  IT_HEADER-CONF_TEXT       = 'Return-to-Line'.
  APPEND IT_HEADER.

*-Item
  SELECT SINGLE LGPRO INTO  L_LGORT
  FROM MARC
  WHERE MATNR = WA_MAIN-BPART
    AND WERKS = WA_MAIN-WERKS.

  SELECT SINGLE *
  FROM AFKO
  WHERE AUFNR = WA_MAIN-AUFNR.

  SELECT * INTO TABLE IT_RESB
  FROM RESB
  WHERE RSNUM =   afko-rsnum
    AND DUMPS <> 'X'.
  IF SY-SUBRC <> 0.
    L_INPUT_ERROR = 'X'.
    MESSAGE S000 WITH 'There is No Confirmation Material'.
    EXIT.
  ENDIF.

  CLEAR: IT_ITEM, IT_LINK.
  IT_ITEM-MATERIAL   = WA_MAIN-BPART.
  IT_ITEM-PLANT      = WA_MAIN-WERKS.
  IT_ITEM-STGE_LOC   = L_LGORT.
  IT_ITEM-MOVE_TYPE  = '101'.
  IT_ITEM-ENTRY_QNT  = WA_MAIN-MENGE.
  IT_ITEM-ENTRY_UOM  = WA_MAIN-MEINS.
  IT_ITEM-MVT_IND    = 'F'.
  it_item-NO_MORE_GR = 'X'.
  IT_ITEM-LINE_ID    = 1.

  APPEND IT_ITEM.

  IT_LINK-INDEX_CONFIRM = 1.
  IT_LINK-INDEX_GOODSMOV = 1.
  APPEND IT_LINK.

  LOOP AT IT_RESB.

    CLEAR: IT_ITEM, IT_LINK.
    IT_ITEM-MATERIAL   = IT_RESB-MATNR.
    IT_ITEM-PLANT      = IT_RESB-WERKS.
    IT_ITEM-STGE_LOC   = IT_RESB-LGORT.
    IT_ITEM-MOVE_TYPE  = '261'.

    IT_ITEM-ENTRY_QNT  = WA_MAIN-MENGE * AFKO-GAMNG
                        / IT_RESB-ERFMG.
    IT_ITEM-ENTRY_UOM  = IT_RESB-MEINS.
    IT_ITEM-MVT_IND    = ''.
    it_item-NO_MORE_GR = 'X'.
    IT_ITEM-LINE_ID    = SY-TABIX + 1.

    APPEND IT_ITEM.

    IT_LINK-INDEX_CONFIRM = 1.
    IT_LINK-INDEX_GOODSMOV = IT_ITEM-LINE_ID.
    APPEND IT_LINK.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_HDR'
    IMPORTING
      RETURN             = ST_RETURN1
    TABLES
      ATHDRLEVELS        = IT_HEADER
      GOODSMOVEMENTS     = IT_ITEM
      LINK_CONF_GOODSMOV = IT_LINK
      DETAIL_RETURN      = IT_RETURN.

  READ TABLE IT_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    WA_MAIN-ZRESULT = 'E'.
    WA_MAIN-ZMSG    = IT_RETURN-MESSAGE.
    WA_MAIN-DIRCT   = '2'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM UPDATE_ASMPGI_0110_CANC.

    L_INPUT_ERROR = 'X'.
    MESSAGE S000 WITH IT_RETURN-MESSAGE.
    EXIT.
  ELSE.
    WA_MAIN-ZRESULT = 'S'.
    WA_MAIN-ZMSG    = ''.
    WA_MAIN-DIRCT   = '2'.
    PERFORM UPDATE_ASMPGI_0110_CANC.
  ENDIF.
ENDFORM.                    " CANCEL_0130
*&---------------------------------------------------------------------*
*&      Form  PROCESS_0120_1
*&---------------------------------------------------------------------*
FORM PROCESS_0120_1 .
  DATA: wa_header LIKE BAPI2017_GM_HEAD_01,
        it_item LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        it_return LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
        l_mat_doc LIKE BAPI2017_GM_HEAD_RET-MAT_DOC,
        l_doc_year LIKE BAPI2017_GM_HEAD_RET-DOC_YEAR.

  CLEAR : wa_header, it_item[], it_item, it_return[], it_return.

  CHECK     l_input_error  IS INITIAL.

  wa_HEADER-PSTNG_DATE = wa_screen-budat.
  wa_HEADER-DOC_DATE = sy-datum.
  wa_HEADER-HEADER_TXT = 'AS/MP DELIVERY M-TO-M'.

  SELECT SINGLE LGPRO INTO  it_ITEM-STGE_LOC
  FROM MARC
  WHERE MATNR = wa_screen-bpart
    AND WERKS = wa_screen-werks.

*  SELECT SINGLE LGPRO INTO  it_ITEM-MOVE_STLOC
*  FROM MARC
*  WHERE MATNR = wa_screen-aspart_NM
*    AND WERKS = wa_screen-werks.

  it_ITEM-MOVE_STLOC    = 'X551'. "Hard Coding 07.29.2014
  L_LGORT = it_ITEM-STGE_LOC. "MV part storage location

  it_ITEM-MATERIAL = wa_screen-bpart.
  it_ITEM-PLANT    = wa_screen-werks.
  it_ITEM-MOVE_MAT = wa_screen-aspart_NM.
  it_ITEM-MOVE_PLANT = wa_screen-werks.
  it_ITEM-MOVE_TYPE = '309'.
  it_ITEM-ENTRY_QNT = wa_screen-MENGE.
  it_ITEM-ENTRY_UOM = wa_screen-MEINS.
*  IT_ITEM-MOVE_REAS = wa_screen-grund.
  APPEND IT_ITEM.

*-M-to-M
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      GOODSMVT_HEADER  = wa_header
      GOODSMVT_CODE    = '04'
    IMPORTING
*     GOODSMVT_HEADRET =
      MATERIALDOCUMENT = l_mat_doc
      MATDOCUMENTYEAR  = l_doc_year
    TABLES
      GOODSMVT_ITEM    = it_item
      RETURN           = it_return.

  READ TABLE it_return WITH KEY TYPE = 'E'.

  IF l_mat_doc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    l_input_error = 'X'.
    MESSAGE S000 WITH it_return-MESSAGE.
    EXIT.
*    message e000 with it_return-MESSAGE.
  ELSE.
    wa_screen-M2MDOC = l_mat_doc.
    wa_screen-matnr = wa_screen-aspart_NM.
    PERFORM INSERT_asmpgi_0120.
    message s000 with 'Mat. Document' wa_screen-M2MDOC
                      'has been created'.
  ENDIF.


*-Create Scrap : commented on 7.29.2014 requeted by BS BAE
*  PERFORM create_mb1a_0120.
ENDFORM.                    " PROCESS_0120_1
*&---------------------------------------------------------------------*
*&      Form  PROCESS_0120_2
*&---------------------------------------------------------------------*
FORM PROCESS_0120_2 .
  DATA : l_budat TYPE sy-datum,
         l_menge(10), l_menge_int TYPE i.

  CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

  CHECK l_input_error IS INITIAL.

  WRITE : wa_screen-budat TO l_budat.
  l_menge_int = wa_screen-menge.
  l_menge  = l_menge_int. CONDENSE l_menge.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLCOKO1'             '0100',
          ' ' 'BDC_CURSOR'           'CAUFVD-MATNR',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'CAUFVD-MATNR'         wa_screen-ASPART_NM,
          ' ' 'CAUFVD-WERKS'         wa_screen-werks,
          ' ' 'AUFPAR-PP_AUFART'     'PP01'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLCOKO1'             '0115',
          ' ' 'BDC_CURSOR'           'CAUFVD-GSTRP',
          ' ' 'BDC_OKCODE'           '=FREI',
          ' ' 'CAUFVD-GAMNG'         l_menge,
          ' ' 'CAUFVD-GMEIN'         'EA',
          ' ' 'CAUFVD-GLTRP'          l_budat,
          ' ' 'CAUFVD-GSTRP'          l_budat,
          ' ' 'CAUFVD-TERKZ'          '3',
          ' ' 'CAUFVD-FHORI'         '000'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLCOKO1'             '0115',
          ' ' 'BDC_CURSOR'           'CAUFVD-GAMNG',
          ' ' 'BDC_OKCODE'           '=BU',
          ' ' 'CAUFVD-GAMNG'         l_menge,
*            ' ' 'CAUFVD-GMEIN'         'EA',
          ' ' 'CAUFVD-GLTRP'          l_budat,
          ' ' 'CAUFVD-GSTRP'          l_budat,
          ' ' 'CAUFVD-TERKZ'          '3',
          ' ' 'CAUFVD-FHORI'         '000'.

  CALL TRANSACTION 'CO01' USING it_bdc MODE disp_mode
                                UPDATE 'S'
                                MESSAGES INTO mess_tab.

  PERFORM read_message_0120_2.

  perform create_confirmation_0120.
ENDFORM.                    " PROCESS_0120_2
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0120_2
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0120_2 .
  READ TABLE mess_tab WITH KEY msgtyp = 'S'
                               msgnr  = '100'.
  IF sy-subrc = 0.
    wa_screen-aufnr = mess_tab-msgv1.
    PERFORM call_alpha_conversion CHANGING wa_screen-aufnr.

    DO 1000 TIMES.
      SELECT SINGLE * FROM afko WHERE aufnr = wa_screen-aufnr.
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    ENDDO.

    wa_screen-matnr = wa_screen-aspart_NM.
    PERFORM INSERT_asmpgi_0120.
*    message s000 with 'Production Order' wa_screen-aufnr
*                      'has been created'.

  else.
    READ TABLE mess_tab WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = mess_tab-msgid
          msgnr               = mess_tab-msgnr
          msgv1               = mess_tab-msgv1
          msgv2               = mess_tab-msgv2
          msgv3               = mess_tab-msgv3
          msgv4               = mess_tab-msgv4
        IMPORTING
          message_text_output = lv_msg.

      l_input_error = 'X'.
      message s000 with lv_msg.
    ENDIF.

  endif.
ENDFORM.                    " READ_MESSAGE_0120_2
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONFIRMATION_0120
*&---------------------------------------------------------------------*
FORM CREATE_CONFIRMATION_0120 .
  DATA: it_Header LIKE BAPI_PP_HDRLEVEL      OCCURS 0 WITH HEADER LINE,
        it_item LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        IT_RETURN LIKE BAPI_CORU_RETURN      OCCURS 0 WITH HEADER LINE,
        IT_LINK LIKE BAPI_LINK_CONF_GOODSMOV OCCURS 0 WITH HEADER LINE,
        st_return1    LIKE bapiret1.

  DATA : l_rsnum LIKE afko-rsnum,
         it_resb LIKE resb OCCURS 0 WITH HEADER LINE.

  CLEAR : it_header[], it_header, it_item[], it_item, it_link[],
          it_link, it_resb[].

  check l_input_error is INITIAL and wa_screen-aufnr is not INITIAL.

  it_header-orderid         = wa_screen-aufnr.
  it_header-fin_conf        = 'X'.
  it_header-postg_date      = wa_screen-budat.
  it_header-conf_quan_unit  = wa_screen-meins.
  it_header-yield           = wa_screen-menge.
  it_header-exec_start_date = sy-datum.
  it_header-exec_start_time = sy-uzeit.
  it_header-exec_fin_date   = sy-datum.
  it_header-exec_fin_time   = sy-uzeit.
  it_header-CONF_TEXT       = 'Scrap for Moving Part'.
  APPEND it_header.

*-Item
*  SELECT SINGLE LGPRO INTO  L_LGORT
*  FROM MARC
*  WHERE MATNR = wa_screen-ASPART_NM
*    AND WERKS = wa_screen-werks.

*  SELECT SINGLE rsnum INTO l_rsnum
*  FROM afko
*  WHERE AUFNR = wa_screen-aufnr.
*
*  SELECT * INTO TABLE it_resb
*  FROM resb
*  WHERE rsnum =   l_rsnum

  SELECT SINGLE * " INTO l_rsnum
  FROM afko
  WHERE AUFNR = wa_screen-aufnr.

  SELECT * INTO TABLE it_resb
  FROM resb
  WHERE rsnum =   afko-rsnum
    AND dumps <> 'X'.
  IF sy-subrc <> 0.
    MESSAGE E000 WITH 'There is no confirmation material.'.
  ENDIF.

  CLEAR: IT_ITEM, IT_LINK.
  IT_ITEM-MATERIAL   = wa_screen-ASPART_NM.
  IT_ITEM-PLANT      = wa_screen-WERKS.
*  IT_ITEM-STGE_LOC   = L_LGORT.
  IT_ITEM-STGE_LOC   = 'X551'.
  IT_ITEM-MOVE_TYPE  = '101'.
  IT_ITEM-ENTRY_QNT  = WA_SCREEN-MENGE.
  IT_ITEM-ENTRY_UOM  = WA_SCREEN-MEINS.
  it_item-mvt_ind    = 'F'.
  it_item-NO_MORE_GR = 'X'.
  it_item-line_id    = 1.

  APPEND IT_ITEM.

  IT_LINK-INDEX_CONFIRM = 1.
  IT_LINK-INDEX_GOODSMOV = 1.
  APPEND IT_LINK.


  LOOP AT IT_RESB.

    CLEAR: IT_ITEM, IT_LINK.
    IT_ITEM-MATERIAL   = it_resb-MATNR.
    IT_ITEM-PLANT      = it_resb-WERKS.
    IT_ITEM-STGE_LOC   = it_resb-LGORT.
    IT_ITEM-MOVE_TYPE  = '261'.

    IT_ITEM-ENTRY_QNT  = WA_SCREEN-MENGE * AFKO-GAMNG
                        / IT_RESB-ERFMG.
    IT_ITEM-ENTRY_UOM  = it_resb-MEINS.
    it_item-NO_MORE_GR = 'X'.
*    it_item-mvt_ind    = 'F'.
    it_item-mvt_ind    = ''.
    it_item-line_id    = SY-TABIX + 1.

    APPEND IT_ITEM.

    IT_LINK-INDEX_CONFIRM = 1.
    IT_LINK-INDEX_GOODSMOV = it_item-line_id.
    APPEND IT_LINK.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_HDR'
    IMPORTING
      return             = st_return1
    TABLES
      athdrlevels        = it_header
      GOODSMOVEMENTS     = it_item
      LINK_CONF_GOODSMOV = IT_LINK
      detail_return      = it_return.

  READ TABLE it_return WITH KEY type = 'E'.

  IF sy-subrc <> 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*-< added on 09.11.2014  Victor
    DO 10 TIMES.
      SELECT single mblnr into wa_screen-mblnr
      FROM mseg
      WHERE werks   = wa_screen-werks
        AND matnr   = wa_screen-matnr
        AND bwart   = '101'
        AND aufnr = wa_screen-aufnr.
      IF sy-subrc eq 0.
        EXIT.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ENDDO.
*->

    WA_SCREEN-ZRESULT = 'S'.
    WA_SCREEN-ZMSG    = ''.
    PERFORM update_asmpgi_0120.
    message s000 with 'Scrap for Moving Part has been processed'
                      'successfully'.

  else.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    wa_screen-zresult = 'E'.
    wa_screen-zmsg    = it_return-message.
    l_input_error = 'X'.

    PERFORM update_asmpgi_0120.
    MESSAGE s000 WITH it_return-message.
    EXIT.
  endif.
ENDFORM.                    " CREATE_CONFIRMATION_0120
*&---------------------------------------------------------------------*
*&      Form  CANCEL_CONFIRMATION_0120
*&---------------------------------------------------------------------*
FORM CANCEL_CONFIRMATION_0120 .
  DATA: it_Header LIKE BAPI_PP_HDRLEVEL      OCCURS 0 WITH HEADER LINE,
        it_item LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        IT_RETURN LIKE BAPI_CORU_RETURN      OCCURS 0 WITH HEADER LINE,
        IT_LINK LIKE BAPI_LINK_CONF_GOODSMOV OCCURS 0 WITH HEADER LINE,
        st_return1    LIKE bapiret1.

  DATA : l_rsnum LIKE afko-rsnum,
         it_resb LIKE resb OCCURS 0 WITH HEADER LINE.

  CLEAR : it_header[], it_header, it_item[], it_item, it_link[],
          it_link, it_resb[].

  check l_input_error is INITIAL and wa_main-aufnr is not INITIAL.

  it_header-orderid         = wa_main-aufnr.
  it_header-fin_conf        = 'X'.
  it_header-postg_date      = wa_main-budat.
  it_header-conf_quan_unit  = wa_main-meins.
*  it_header-yield           = wa_main-menge.
  it_header-yield           = 0.     "??
  it_header-exec_start_date = sy-datum.
  it_header-exec_start_time = sy-uzeit.
  it_header-exec_fin_date   = sy-datum.
  it_header-exec_fin_time   = sy-uzeit.
  it_header-CONF_TEXT       = 'Scrap for Moving Part'.
  APPEND it_header.

*-Item
*  SELECT SINGLE LGPRO INTO  L_LGORT
*  FROM MARC
*  WHERE MATNR = wa_main-ASPART_NM
*    AND WERKS = wa_main-werks.

  SELECT SINGLE *
  FROM afko
  WHERE AUFNR = wa_main-aufnr.

  SELECT * INTO TABLE it_resb
  FROM resb
  WHERE rsnum =   afko-rsnum
    AND dumps <> 'X'.
  IF sy-subrc <> 0.
    MESSAGE E000 WITH 'There is no confirmation material.'.
  ENDIF.

  CLEAR: IT_ITEM, IT_LINK.
  IT_ITEM-MATERIAL   = wa_main-matnr.
  IT_ITEM-PLANT      = wa_main-WERKS.
*  IT_ITEM-STGE_LOC   = L_LGORT.
  IT_ITEM-STGE_LOC   = 'X551'.
  IT_ITEM-MOVE_TYPE  = '102'.
  IT_ITEM-ENTRY_QNT  = wa_main-MENGE.
  IT_ITEM-ENTRY_UOM  = wa_main-MEINS.
  it_item-mvt_ind    = 'F'.
  it_item-NO_MORE_GR = 'X'.
  it_item-line_id    = 1.

  APPEND IT_ITEM.

  IT_LINK-INDEX_CONFIRM = 1.
  IT_LINK-INDEX_GOODSMOV = 1.
  APPEND IT_LINK.


  LOOP AT IT_RESB.

    CLEAR: IT_ITEM, IT_LINK.
    IT_ITEM-MATERIAL   = it_resb-MATNR.
    IT_ITEM-PLANT      = it_resb-WERKS.
    IT_ITEM-STGE_LOC   = it_resb-LGORT.
    IT_ITEM-MOVE_TYPE  = '262'.

    IT_ITEM-ENTRY_QNT  = wa_main-MENGE * AFKO-GAMNG
                        / IT_RESB-ERFMG.
    IT_ITEM-ENTRY_UOM  = it_resb-MEINS.
    it_item-mvt_ind    = ''.
    it_item-NO_MORE_GR = 'X'.
    it_item-line_id    = SY-TABIX + 1.

    APPEND IT_ITEM.

    IT_LINK-INDEX_CONFIRM = 1.
    IT_LINK-INDEX_GOODSMOV = it_item-line_id.
    APPEND IT_LINK.

  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_HDR'
    IMPORTING
      return             = st_return1
    TABLES
      athdrlevels        = it_header
      GOODSMOVEMENTS     = it_item
      LINK_CONF_GOODSMOV = IT_LINK
      detail_return      = it_return.

  READ TABLE it_return WITH KEY type = 'E'.

  IF sy-subrc <> 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    wa_main-ZRESULT = 'S'.
    WA_MAIN-DIRCT   = '2'.
    wa_main-zmsg    = ''.

    PERFORM UPDATE_ASMPGI_0110_CANC.
    message s000 with 'Scrap for Moving Part has been Cancelled'
                      'successfully'.

  else.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    wa_main-zresult = 'E'.
    WA_MAIN-DIRCT   = '2'.
    wa_main-zmsg    = it_return-message.
    l_input_error = 'X'.

    PERFORM UPDATE_ASMPGI_0110_CANC.
    MESSAGE s000 WITH it_return-message.
    EXIT.
  endif.
ENDFORM.                    " CANCEL_CONFIRMATION_0120
*&---------------------------------------------------------------------*
*&      Form  CANCEL_0120_CO02
*&---------------------------------------------------------------------*
FORM CANCEL_0120_CO02 .
  DATA : l_budat TYPE sy-datum,
         l_menge(10), l_menge_int TYPE i.

  CLEAR : mess_tab[], it_bdc[], mess_tab, it_bdc.

  CHECK l_input_error IS INITIAL.

  WRITE : wa_MAIN-budat_C TO l_budat.
  l_menge_int = wa_MAIN-menge.
  l_menge  = l_menge_int. CONDENSE l_menge.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLCOKO1'            '0110',
          ' ' 'BDC_CURSOR'           'CAUFVD-AUFNR',
          ' ' 'BDC_OKCODE'           '/00',
          ' ' 'CAUFVD-AUFNR'         wa_main-aufnr,
          ' ' 'R62CLORD-FLG_OVIEW'   'X'.

  PERFORM bdc_dynpro USING :
          'X' 'SAPLCOKO1'            '0115',
          ' ' 'BDC_CURSOR'           'CAUFVD-GAMNG',
          ' ' 'BDC_OKCODE'           '=KOWE',
          ' ' 'CAUFVD-GAMNG'         l_menge,
          ' ' 'CAUFVD-GLTRP'         L_BUDAT,
          ' ' 'CAUFVD-GSTRP'         L_BUDAT,
          ' ' 'CAUFVD-TERKZ'         '3',   "Scheduling type
          ' ' 'CAUFVD-FHORI'         '000'.  "Scheduling Margin Key

  PERFORM bdc_dynpro USING :
          'X' 'SAPLCOKO1'             '0115',
          ' ' 'BDC_CURSOR'           'AFPOD-ELIKZ',
          ' ' 'BDC_OKCODE'           '=BU',
          ' ' 'AFPOD-ELIKZ'          'X'.
*          ' ' 'AFPOD-LGORT'         ''.  "Storage

  CALL TRANSACTION 'CO02' USING it_bdc MODE disp_mode
                                 UPDATE 'S'
                                 MESSAGES INTO mess_tab.

  PERFORM READ_MESSAGE_0120_co02.

ENDFORM.                    " CANCEL_0120_CO02
*&---------------------------------------------------------------------*
*&      Form  READ_MESSAGE_0120_CO02
*&---------------------------------------------------------------------*
FORM READ_MESSAGE_0120_CO02 .
  READ TABLE MESS_TAB WITH KEY MSGTYP = 'S'
                               MSGNR  = '100'.
  IF SY-SUBRC = 0.
    WA_MAIN-DIRCT   = '2'.
    WA_MAIN-ZRESULT = 'S'.
    WA_MAIN-ZMSG    = ''.
    PERFORM UPDATE_ASMPGI_0110_CANC.
  ELSE.
    READ TABLE MESS_TAB WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = MESS_TAB-MSGID
          MSGNR               = MESS_TAB-MSGNR
          MSGV1               = MESS_TAB-MSGV1
          MSGV2               = MESS_TAB-MSGV2
          MSGV3               = MESS_TAB-MSGV3
          MSGV4               = MESS_TAB-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = LV_MSG.

      WA_SCREEN-ZRESULT = 'E'.
      WA_SCREEN-ZMSG    = LV_MSG.

      PERFORM UPDATE_ASMPGI_0110_CANC.

      L_INPUT_ERROR = 'X'.
      MESSAGE S000 WITH LV_MSG.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_MESSAGE_0120_CO02
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*

MODULE MODIFY_SCREEN OUTPUT.
  check extab[] is not INITIAL.

  LOOP AT SCREEN.
    IF SCREEN-GROUP2 = 'G2'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
