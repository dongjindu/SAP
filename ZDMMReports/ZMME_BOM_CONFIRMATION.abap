************************************************************************
* Program Name      : ZMME_BOM_CONFIRMATION
* Creation Date     : 10/2010
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date            Developer        RequestNo      Description
*
*
*
************************************************************************
REPORT ZMME_BOM_CONFIRMATION  NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.
TABLES: MAST, EKKO, MARC, ZTMM_ASSY_COST1, MARA,T024.
TYPE-POOLS: VRM, SLIS.
*INCLUDE <ICON>.
*INCLUDE <SYMBOL>.

*INCLUDE ZMME_BOM_ACTIVATE_FORM.


DATA : BEGIN OF IT_LIST OCCURS 0,
      CHECK(1),
      SEQ(3) TYPE N,
      STLNR LIKE MAST-STLNR,
      STLAN LIKE MAST-STLAN,  "BOM Usage
      STLAL LIKE STKO-STLAL,  "Alternative
      STLST LIKE STKO-STLST,  "BOM Status
      LABOR LIKE STKO-LABOR,
      MATNR LIKE MAST-MATNR,
      WERKS LIKE MAST-WERKS,
      MAKTX LIKE MAKT-MAKTX,
      ANDAT LIKE STKO-ANDAT,
      AEDAT LIKE STKO-AEDAT,
      ACTNAM LIKE EKKO-ERNAM,
      DATUV LIKE STKO-DATUV,
      CONFIRM LIKE MAST-CSLTY,
      CONF_DATE LIKE STKO-ANDAT,
      CELLTAB    TYPE LVC_T_STYL,
      END OF IT_LIST.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: G_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I,
        L_DYNAME LIKE SY-REPID.

DATA: WA_VTYPE_F   LIKE   ZTMM_ASSY_COST1-VTYPE,
      WA_VTYPE_T   LIKE   ZTMM_ASSY_COST1-VTYPE,
      WA_MCODE_F   LIKE   ZTMM_ASSY_COST1-MCODE,
      WA_MCODE_T   LIKE   ZTMM_ASSY_COST1-MCODE,
      WA_LIFNR_F   LIKE   ZTMM_ASSY_COST1-LIFNR,
      WA_LIFNR_T   LIKE   ZTMM_ASSY_COST1-LIFNR,
      WA_MATNR_F   LIKE   MARA-MATNR,
      WA_MATNR_T   LIKE   MARA-MATNR.

DATA:  W_FLAG,
       W_REFRESH.

DATA : ERROR_IN_DATA   TYPE LVC_S_STBL.
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF LW_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF LW_OPT.
DATA:  CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
       CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'A',
       MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

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
    PERFORM HANDLE_DATA_CHANGED  USING ER_DATA_CHANGED.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED

  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.


SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
PARAMETERS:  P_WERKS LIKE MARC-WERKS DEFAULT 'P001'.
SELECT-OPTIONS: S_VTYPE FOR ZTMM_ASSY_COST1-VTYPE
                            NO-EXTENSION NO INTERVALS.
PARAMETERS:     P_MCODE LIKE ZTMM_ASSY_COST1-MCODE OBLIGATORY,
                P_MATNR LIKE MARA-MATNR.
SELECT-OPTIONS: S_EKGRP FOR ZTMM_ASSY_COST1-EKGRP,
                S_DISPO FOR MARC-DISPO.
SELECTION-SCREEN END   OF BLOCK BL1.
*SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
*
*SELECT-OPTIONS : S_MATNR FOR MAST-MATNR,
*                 S_WERKS FOR MARC-WERKS DEFAULT 'P001',
*                 S_EKGRP FOR MARC-EKGRP,
*                 S_DISPO FOR MARC-DISPO.
*
*SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.
  PERFORM INI_DATA.

AT SELECTION-SCREEN.
* F4 Values for MCODE
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  P_MCODE.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            MCODE  LIKE ZTMM_ASSY_COST1-MCODE,
           END OF VALUE_TAB.

* Select
  SELECT DISTINCT MCODE  FROM ZTMM_ASSY_COST1
             INTO TABLE VALUE_TAB.

  L_DYNAME = SY-REPID.

* Set F4 values for Module code
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'P_MCODE'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'MCODE'
            WINDOW_TITLE    = 'Module Codes'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

* F4 Values for Vechicle type
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_VTYPE-LOW.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            VTYPE  LIKE ZTMM_ASSY_COST1-VTYPE,
           END OF VALUE_TAB.

* Select Vehicle TYpe
  SELECT DISTINCT VTYPE  FROM ZTMM_ASSY_COST1
             INTO TABLE VALUE_TAB.

  L_DYNAME = SY-REPID.

*
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'S_VTYPE'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'VTYPE'
            WINDOW_TITLE    = 'Vehicle Type'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
       EXCEPTIONS
            PARAMETER_ERROR = 1.

* F4 Values for Purchasing group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_EKGRP-LOW.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            EKGRP LIKE T024-EKGRP,
            EKNAM LIKE T024-EKNAM,
           END OF VALUE_TAB.
* Select
  SELECT  EKGRP EKNAM  FROM T024
             INTO TABLE VALUE_TAB.
  L_DYNAME = SY-REPID.


* Set  Purchasing Group
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'EKGRP'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'P_EKGRP'
            WINDOW_TITLE    = 'Purchasing Group'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
*              RETURN_TAB      = T_RETURN
       EXCEPTIONS
            PARAMETER_ERROR = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  S_EKGRP-HIGH.

  DATA : BEGIN OF VALUE_TAB OCCURS 0,
            EKGRP LIKE T024-EKGRP,
            EKNAM LIKE T024-EKNAM,
           END OF VALUE_TAB.
* Select
  SELECT  EKGRP EKNAM  FROM T024
             INTO TABLE VALUE_TAB.
  L_DYNAME = SY-REPID.


* Set  Purchasing Group
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'EKGRP'
            DYNPPROG        = L_DYNAME
            DYNPNR          = '1000'
            DYNPROFIELD     = 'P_EKGRP'
            WINDOW_TITLE    = 'Purchasing Group'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_TAB
*              RETURN_TAB      = T_RETURN
       EXCEPTIONS
            PARAMETER_ERROR = 1.

START-OF-SELECTION.
  PERFORM CHECK_INPUT_VALUE.

  PERFORM GET_DATA.
  IF IT_LIST[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM DISPLAY_DATA.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LT_COST1 LIKE TABLE OF ZTMM_ASSY_COST1 WITH HEADER LINE,
        L_MATNR LIKE MARA-MATNR,
         L_TIME LIKE SY-UZEIT,
         L_DATE LIKE SY-DATUM,
         L_VERSION LIKE ZTPP_MOD_BOM_HIS-VERSON.

  DATA : BEGIN OF LT_TEMP OCCURS 0,
      CHECK(1),
      SEQ(3) TYPE N,
      STLNR LIKE MAST-STLNR,
      STLAN LIKE MAST-STLAN,  "BOM Usage
      STLAL LIKE STKO-STLAL,  "Alternative
      STLST LIKE STKO-STLST,  "BOM Status
                  LABOR LIKE STKO-LABOR,
      MATNR LIKE MAST-MATNR,
      WERKS LIKE MAST-WERKS,
      MAKTX LIKE MAKT-MAKTX,
      ANDAT LIKE STKO-ANDAT,
      AEDAT LIKE STKO-AEDAT,
      ACTNAM LIKE EKKO-ERNAM,
      DATUV LIKE STKO-DATUV,
      CONFIRM LIKE MAST-CSLTY,
      CONF_DATE LIKE STKO-ANDAT,
      END OF LT_TEMP.

  REFRESH IT_LIST.
  CLEAR: IT_LIST.
  IF P_MATNR IS INITIAL.
    SELECT * INTO TABLE LT_COST1
       FROM ZTMM_ASSY_COST1
       WHERE  VTYPE BETWEEN WA_VTYPE_F AND WA_VTYPE_T
          AND MCODE = P_MCODE
          AND EKGRP  IN S_EKGRP
          AND DATAB <= SY-DATUM
          AND DATBI >= SY-DATUM .
    LOOP AT LT_COST1.

      CONCATENATE '%' LT_COST1-VTYPE P_MCODE '%M1%' INTO L_MATNR.

      SELECT A~STLNR A~STLAN B~STLAL B~STLST B~LABOR
          A~MATNR A~WERKS MAKTX B~ANDAT B~AEDAT B~DATUV
          APPENDING CORRESPONDING FIELDS OF TABLE LT_TEMP
           FROM MAST AS A
           INNER JOIN STKO AS B
           ON A~STLNR = B~STLNR
           INNER JOIN MARA AS M
           ON A~MATNR = M~MATNR
           INNER JOIN MARC AS C
           ON M~MATNR = C~MATNR
           AND A~WERKS = C~WERKS
           INNER JOIN MAKT AS D
           ON A~MATNR = D~MATNR
           WHERE A~MATNR  LIKE L_MATNR
           AND A~WERKS = P_WERKS
           AND A~STLAN = '2'
           AND B~STLTY = 'M'
*           AND B~STLST = '2'
           AND M~LVORM = ''
           AND M~MSTAE <> '14'
           AND C~EKGRP IN S_EKGRP
           AND C~DISPO IN S_DISPO
          AND D~SPRAS = SY-LANGU.
    ENDLOOP.

  ELSE.
    IF P_MATNR+0(3) <> S_VTYPE-LOW AND S_VTYPE-LOW <> ' ' OR
        P_MATNR+3(2) <> P_MCODE AND P_MCODE <> ' '.
      MESSAGE I009 WITH
        'Material Number does not match with Vehicle Type/Model Code'.
    ELSE.
      SELECT A~STLNR A~STLAN B~STLAL B~STLST B~LABOR
          A~MATNR A~WERKS MAKTX B~ANDAT B~AEDAT B~DATUV
          INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
           FROM MAST AS A
           INNER JOIN STKO AS B
           ON A~STLNR = B~STLNR
           INNER JOIN MARA AS M
           ON A~MATNR = M~MATNR
           INNER JOIN MARC AS C
           ON M~MATNR = C~MATNR
           AND A~WERKS = C~WERKS
           INNER JOIN MAKT AS D
           ON A~MATNR = D~MATNR
           WHERE A~MATNR = P_MATNR
           AND A~WERKS = P_WERKS
           AND A~STLAN = '2'
           AND B~STLTY = 'M'
*         AND B~STLST = '2'
           AND M~LVORM = ''
           AND M~MSTAE <> '14'
            AND C~EKGRP IN S_EKGRP
           AND C~DISPO IN S_DISPO
          AND D~SPRAS = SY-LANGU.
    ENDIF.
  ENDIF.

*  SELECT A~STLNR A~STLAN B~STLAL B~STLST
*    A~MATNR A~WERKS MAKTX B~ANDAT B~AEDAT B~DATUV
*    INTO CORRESPONDING FIELDS OF TABLE IT_LIST
*     FROM MAST AS A
*     INNER JOIN STKO AS B
*     ON A~STLNR = B~STLNR
*     INNER JOIN MARC AS C
*     ON A~MATNR = C~MATNR
*     AND A~WERKS = C~WERKS
*     INNER JOIN MAKT AS D
*     ON A~MATNR = D~MATNR
*     WHERE A~MATNR IN S_MATNR
*     AND A~WERKS IN S_WERKS
*     AND A~STLAN = '2'
*     AND B~STLTY = 'M'
*     AND B~STLST = '2'
*     AND C~EKGRP IN S_EKGRP
*     AND C~DISPO IN S_DISPO.


  SORT LT_TEMP BY MATNR STLNR STLAL DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING MATNR.

  LOOP AT LT_TEMP.
    MOVE-CORRESPONDING LT_TEMP TO IT_LIST.

*    SELECT SINGLE CREATE_BY MAX( CONF_DATE ) INTO
*    (IT_LIST-ACTNAM, IT_LIST-CONF_DATE )
*     FROM ZTPP_MOD_BOM_HIS
*     WHERE MATNR = IT_LIST-MATNR
*     GROUP BY CREATE_BY.

*    SELECT SINGLE MAX( VERSON ) CONF_DATE CREATE_BY
*      INTO (L_VERSION, IT_LIST-CONF_DATE, IT_LIST-ACTNAM)
*       FROM ZTPP_MOD_BOM_HIS
*       WHERE MATNR = IT_LIST-MATNR
*       GROUP BY VERSON CONF_DATE CREATE_BY.

    SELECT SINGLE MAX( VERSON ) INTO L_VERSION
       FROM ZTPP_MOD_BOM_HIS
       WHERE MATNR = IT_LIST-MATNR.

    IF SY-SUBRC = 0.
      SELECT SINGLE CONF_DATE CREATE_BY MAX( CREATE_TIME )
        INTO (IT_LIST-CONF_DATE, IT_LIST-ACTNAM, L_TIME)
         FROM ZTPP_MOD_BOM_HIS
         WHERE MATNR = IT_LIST-MATNR
          AND VERSON = L_VERSION
         GROUP BY CONF_DATE CREATE_BY.
    ENDIF.

    IF NOT IT_LIST-CONF_DATE IS INITIAL.
      IT_LIST-CONFIRM = 'Y'.
    ENDIF.
    IT_LIST-SEQ = SY-TABIX.

    APPEND IT_LIST.
    CLEAR: IT_LIST.
  ENDLOOP.

ENDFORM.
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

*---------------------------------------------------------------------*
*       FORM display_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 200.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  DATA: LS_STABLE TYPE LVC_S_STBL.
  IF G_DOCKING_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_LIST'.
    PERFORM GUI_ALV_CELL_CONTROL.
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
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'ACTIVATE'.
      PERFORM ACTIVATE_BOM.
      IF W_REFRESH = 'X'.
        PERFORM GET_DATA.
        PERFORM GUI_ALV_CELL_CONTROL.
*      SUBMIT ZMME_BOM_CONFIRMATION
*          WITH P_WERKS = P_WERKS
*          WITH S_VTYPE IN S_VTYPE
*          WITH P_MCODE = P_MCODE
*            WITH P_MATNR = P_MATNR
*            WITH S_EKGRP IN S_EKGRP
*            WITH S_DISPO IN S_DISPO.
*            CLEAR: OK_CODE.
*            CLEAR: W_FLAG.
      ENDIF.
      CLEAR: W_REFRESH.
    WHEN 'COMPARE'.
*      PERFORM CALL_CS14.
*    WHEN 'COMP2'.
      PERFORM CALL_COMP2.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

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
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_LIST[]
               IT_SORT          = IT_SORT[].

ENDFORM.                    " assign_itab_to_alv

*---------------------------------------------------------------------*
*       FORM CREATE_CONTAINER_N_OBJECT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
*  CREATE OBJECT GRID_CONTAINER
*          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
*          EXCEPTIONS
*           CNTL_ERROR = 1
*           CNTL_SYSTEM_ERROR = 2
*           CREATE_ERROR = 3
*           LIFETIME_ERROR = 4
*           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  W_REPID = SY-REPID.
*  IF SY-SUBRC NE 0.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = W_REPID
*              TXT2  = SY-SUBRC
*              TXT1  = 'The control can not be created'.
*  ENDIF.
*  CREATE OBJECT ALV_GRID
*         EXPORTING I_PARENT = GRID_CONTAINER
*                   I_APPL_EVENTS = 'X'.

  W_REPID = SY-REPID.

  CREATE OBJECT G_DOCKING_CONTAINER
    EXPORTING
      REPID     = W_REPID
      DYNNR     = '0200'
      SIDE      = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_BOTTOM
*        RATIO     = 90
      EXTENSION = 2000.

  CREATE OBJECT ALV_GRID
     EXPORTING
       I_PARENT = G_DOCKING_CONTAINER.


ENDFORM.                    " create_container_n_object

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
**S> 08/10/11 PAUL : ECC6.0
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
**E<
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'KONNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

*  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
  CLEAR: IT_FIELDCAT, IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

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

                                  'S' 'SEQ'       ' ',
                                  ' ' 'COLTEXT'     'Seq',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'CHECK'       ' ',
                                  ' ' 'COLTEXT'     'Confirm',
                                  ' ' 'JUST'        'C',
                                  ' ' 'CHECKBOX'    'X',
*                                  ' ' 'EDIT'        'X',
*                                ' ' 'KEY'         'X',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material',
*                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MAKTX'        ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'ANDAT'       ' ',
                                  ' ' 'COLTEXT'     'Created on',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'AEDAT'       ' ',
                                  ' ' 'COLTEXT'     'Changed on',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ACTNAM'       ' ',
                                  ' ' 'COLTEXT'     'Confirmed by',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'CONFIRM'       ' ',
                                  ' ' 'COLTEXT'     'Confirm',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'CONF_DATE'       ' ',
                                  ' ' 'COLTEXT'     'Confirmed Date',
                                  'E' 'OUTPUTLEN'   '12'.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*&      Form  event_handler_register
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EVENT_HANDLER_REGISTER .

  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
   EXPORTING
     I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.
  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
ENDFORM.                    " event_handler_register

*&---------------------------------------------------------------------*
*&      Form  hotspot_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM HOTSPOT_CLICK  USING    E_ROW_ID
      E_COLUMN_ID.

  READ TABLE IT_LIST INDEX E_ROW_ID.

  CASE E_COLUMN_ID.
    WHEN 'MATNR'.
      PERFORM CALL_CS14.
*    WHEN 'RBLNR'.
*      SET PARAMETER ID 'MBN'  FIELD gt_display-rblnr.
*      SET PARAMETER ID 'MJA'  FIELD gt_display-rjahr.
*      CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

  ENDCASE.


ENDFORM.                    " hotspot_click

*&---------------------------------------------------------------------*
*&      Form  ACTIVATE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ACTIVATE_BOM.
  DATA: L_MATNR LIKE MAST-MATNR.
  CLEAR:  W_FLAG, W_REFRESH.
  LOOP AT IT_LIST WHERE CHECK = 'X'.
    SELECT SINGLE A~MATNR INTO L_MATNR
        FROM MAST AS A
        INNER JOIN STKO AS B
        ON A~STLNR = B~STLNR
        WHERE A~MATNR = IT_LIST-MATNR
        AND A~WERKS = IT_LIST-WERKS
        AND A~STLAN = '2'
        AND B~STLTY = 'M'
*        AND B~STLST = '2'.
        AND B~LABOR = '002'.
    IF SY-SUBRC = 0.
      PERFORM UPDATE_BOM_STATUS_TO_01.
*      PERFORM DELETE_BOM_WITH_USAGE_2.
*      PERFORM CREATE_BOM_WITH_USAGE_2.
      PERFORM COPY_TO_ZTABLE.
    ELSE.
      MESSAGE S000 WITH 'Confirmation already processed:' IT_LIST-MATNR.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ACTIVATE_BOM
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
*&      Form  CALL_CS14
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPARE_BOM.

  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
      LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  DATA: L_DATUM(10).
  DATA: BEGIN OF LW_LIST,
        STLNR LIKE MAST-STLNR,
        STLAN LIKE MAST-STLAN,
        STLAL LIKE STKO-STLAL,
        STLST LIKE STKO-STLST,
        END OF LW_LIST.

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
  READ TABLE IT_LIST INDEX LT_ROWS-INDEX.
  IF IT_LIST-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  PERFORM CALL_CS14.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CALL_CS14                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CALL_CS14.
  DATA: L_DATUM(10).
  DATA: BEGIN OF LW_LIST,
        STLNR LIKE MAST-STLNR,
        STLAN LIKE MAST-STLAN,
        STLAL LIKE STKO-STLAL,
        STLST LIKE STKO-STLST,
        END OF LW_LIST.

  SELECT SINGLE A~STLNR A~STLAN B~STLAL B~STLST
       INTO LW_LIST
       FROM MAST AS A
       INNER JOIN STKO AS B
       ON A~STLNR = B~STLNR
       WHERE A~MATNR = IT_LIST-MATNR
       AND A~WERKS = IT_LIST-WERKS
       AND A~STLAN = '2'
       AND B~STLTY = 'M'
       AND B~STLST = '1'.

  IF SY-SUBRC = 0.
    REFRESH: IT_BDC.

    WRITE: IT_LIST-DATUV TO L_DATUM.

    PERFORM DYNPRO USING:
    'X' 'SAPMC29V' '0115',
*  ' ' 'BDC_OKCODE' '=DIFF',
  ' '  'MATNR1'  IT_LIST-MATNR,
  ' '  'WERKS1'  IT_LIST-WERKS,
  ' '  'STLAL1'  IT_LIST-STLAL,
  ' '  'STLAN1'  IT_LIST-STLAN,
*  ' ' 'DATUV1'  L_DATUM,

    ' ' 'BDC_CURSOR' 'STLAN2',
    ' '  'MATNR2'  IT_LIST-MATNR,
  ' '  'WERKS2'  IT_LIST-WERKS,
  ' '  'STLAL2'  LW_LIST-STLAL,
  ' '  'STLAN2'  LW_LIST-STLAN.
*  ' ' 'DATUV2'  L_DATUM,

*    'X' 'SAPMC29V' '0115',
*    ' ' 'BDC_OKCODE' '/EBACK'.

    CALL TRANSACTION 'CS14'  USING IT_BDC OPTIONS FROM LW_OPT.
  ELSE.
    MESSAGE S009 WITH 'No BOM of Usage = 3'.
  ENDIF.
ENDFORM.                                                    " CALL_CS14

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1598   text
*      -->P_1599   text
*      -->P_1600   text
*----------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  ini_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INI_DATA.

*  LW_OPT-DEFSIZE = 'X'.
  LW_OPT-DISMODE = 'A'.
  LW_OPT-UPDMODE = 'S'.
ENDFORM.                    " ini_data
*&---------------------------------------------------------------------*
*&      Form  update_bom_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_BOM_STATUS_TO_01.
  DATA: MSG(255).

  REFRESH: IT_BDC, MESSTAB.
** Changed on 11/18/10
*  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0100'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RC29N-STLAL'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=KALL'.
*  PERFORM BDC_FIELD       USING 'RC29N-MATNR'
*                                IT_LIST-MATNR.
*  PERFORM BDC_FIELD       USING 'RC29N-WERKS'
*                                IT_LIST-WERKS.
*  PERFORM BDC_FIELD       USING 'RC29N-STLAN'
*                                IT_LIST-STLAN.
*  PERFORM BDC_FIELD       USING 'RC29N-STLAL'
*                                IT_LIST-STLAL.
**  PERFORM BDC_FIELD       USING 'RC29N-DATUV'
**                                RECORD-DATUV_005.
*  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '2110'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=FCBU'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RC29K-STLST'.
**  PERFORM BDC_FIELD       USING 'RC29K-BMENG'
**                                RECORD-BMENG_006.
*  PERFORM BDC_FIELD       USING 'RC29K-STLST'
*                                '01'.

  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29N-STLAN'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=KALL'.
  PERFORM BDC_FIELD       USING 'RC29N-MATNR'
                                IT_LIST-MATNR.
  PERFORM BDC_FIELD       USING 'RC29N-WERKS'
                                IT_LIST-WERKS.
  PERFORM BDC_FIELD       USING 'RC29N-STLAN'
                                IT_LIST-STLAN.
*perform bdc_field       using 'RC29N-DATUV'
*                              record-DATUV_004.

  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '2110'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=KDAT'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29K-EXSTL'.
*perform bdc_field       using 'RC29K-BMENG'
*                              record-BMENG_005.
*perform bdc_field       using 'RC29K-STLST'
*                              record-STLST_006.

  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '2110'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29K-LABOR'.
  PERFORM BDC_FIELD       USING 'RC29K-LABOR'
                                '001'.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '2110'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=FCBU'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29K-EXSTL'.
*perform bdc_field       using 'RC29K-LABOR'
*                              record-LABOR_008.

** end of change
  CALL TRANSACTION 'CS02' USING IT_BDC
                    MODE   CTUMODE
                    UPDATE CUPDATE
                    MESSAGES INTO MESSTAB.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = MSG.

    REFRESH IT_BDC.
    MESSAGE I000 WITH 'Error in change BOM status:' MSG.
  ELSE.
    READ TABLE MESSTAB WITH KEY MSGTYP = 'A'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = MSG.
      REFRESH IT_BDC.
      MESSAGE I000 WITH 'Error in change BOM status:' MSG.
      W_FLAG = 'X'.
    ELSE.
      MESSAGE S000 WITH 'BOM changed successfully for' IT_LIST-MATNR.
    ENDIF.
  ENDIF.

ENDFORM.                    " update_bom_status
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING    PROGRAM DYNPRO.
  CLEAR IT_BDC.
  IT_BDC-PROGRAM  = PROGRAM.
  IT_BDC-DYNPRO   = DYNPRO.
  IT_BDC-DYNBEGIN = 'X'.
  APPEND IT_BDC.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0305   text
*      -->P_0306   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING    FNAM FVAL.
  CLEAR IT_BDC.
  IT_BDC-FNAM = FNAM.
  IT_BDC-FVAL = FVAL.
  APPEND IT_BDC.
ENDFORM.                    " bdc_field
*&---------------------------------------------------------------------*
*&      Form  create_bom_with_usage_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_BOM_WITH_USAGE_2.
  DATA: MSG(255).

  CHECK W_FLAG IS INITIAL.
  REFRESH: IT_BDC, MESSTAB.

  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29N-STLAN'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RC29N-MATNR'
                                IT_LIST-MATNR.
  PERFORM BDC_FIELD       USING 'RC29N-WERKS'
                                IT_LIST-WERKS.
  PERFORM BDC_FIELD       USING 'RC29N-STLAN'
                                '2'.
  PERFORM BDC_FIELD       USING 'RC29N-STLAL'
                                  '01'.
* perform bdc_field       using 'RC29N-DATUV'
*                              record-DATUV_004.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0110'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
*perform bdc_field       using 'RC29K-BMENG'
*                              record-BMENG_005.
  PERFORM BDC_FIELD       USING 'RC29K-STLST'
                                '1'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29K-EXSTL'.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0111'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29K-LABOR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0140'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29P-POSNR(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=FCEW'.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0105'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29N-STLAL'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=CLWI'.
  PERFORM BDC_FIELD       USING 'RC29N-MATNR'
                                IT_LIST-MATNR.
  PERFORM BDC_FIELD       USING 'RC29N-WERKS'
                                IT_LIST-WERKS.
  PERFORM BDC_FIELD       USING 'RC29N-STLAN'
                                IT_LIST-STLAN.
  PERFORM BDC_FIELD       USING 'RC29N-STLAL'
                                IT_LIST-STLAL.
*perform bdc_field       using 'RC29N-DATUV'
*                              record-DATUV_011.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0156'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=MALL'.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0156'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=FCUE'.
  PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0150'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RC29P-POSNR(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=FCBU'.

  CALL TRANSACTION 'CS01' USING IT_BDC
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = MSG.

    REFRESH IT_BDC.
    MESSAGE I000 WITH 'Error in creation of BOM (2):' MSG.
    W_FLAG = 'X'.
  ELSE.
    READ TABLE MESSTAB WITH KEY MSGTYP = 'A'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = MSG.
      REFRESH IT_BDC.
      MESSAGE I000 WITH 'Error in creation of BOM (2):' MSG.
      W_FLAG = 'X'.
    ELSE.
      MESSAGE S000 WITH 'BOM copied successfully for' IT_LIST-MATNR.
    ENDIF.
  ENDIF.

ENDFORM.                    " create_bom_with_usage_2
*&---------------------------------------------------------------------*
*&      Form  CALL_comp2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_COMP2.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
       LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  CALL METHOD CL_GUI_CFW=>FLUSH.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_LIST INDEX LT_ROWS-INDEX.
  IF IT_LIST-MATNR = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
*  SET PARAMETER ID 'MAT' FIELD IT_LIST-MATNR.
*  SET PARAMETER ID 'WRK' FIELD IT_LIST-WERKS.
*  CALL TRANSACTION 'ZMMR00004' AND SKIP FIRST SCREEN.
  SUBMIT ZMMR_BOM_COMPARE WITH P_MATNR = IT_LIST-MATNR
                          WITH P_WERKS = IT_LIST-WERKS
                          WITH P_DATUM = SY-DATUM
                          WITH P_STLST = IT_LIST-STLST AND RETURN.

ENDFORM.                    " CALL_comp2
*&---------------------------------------------------------------------*
*&      Form  DELETE_BOM_WITH_USAGE_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_BOM_WITH_USAGE_2.
  DATA: MSG(255),
  L_MATNR LIKE MAST-MATNR.

  CHECK W_FLAG IS INITIAL.

  SELECT SINGLE A~MATNR INTO L_MATNR
      FROM MAST AS A
      INNER JOIN STKO AS B
      ON A~STLNR = B~STLNR
      WHERE A~MATNR = IT_LIST-MATNR
      AND A~WERKS = IT_LIST-WERKS
      AND A~STLAN = '2'
      AND B~STLTY = 'M'.
  IF SY-SUBRC = 0.

    REFRESH: IT_BDC, MESSTAB.

    PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RC29N-STLAL'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RC29N-MATNR'
                                    IT_LIST-MATNR.
    PERFORM BDC_FIELD       USING 'RC29N-WERKS'
                                    IT_LIST-WERKS.
    PERFORM BDC_FIELD       USING 'RC29N-STLAN'
                                    '2'.
    PERFORM BDC_FIELD       USING 'RC29N-STLAL'
                                    '01'.
*perform bdc_field       using 'RC29N-DATUV'
*                              record-DATUV_005.
    PERFORM BDC_DYNPRO      USING 'SAPLCSDI' '0150'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RC29P-POSNR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=FCLO'.

    CALL TRANSACTION 'CS02' USING IT_BDC
                        MODE   CTUMODE
                        UPDATE CUPDATE
                        MESSAGES INTO MESSTAB.

    READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = MSG.

      REFRESH IT_BDC.
      MESSAGE S000 WITH 'Error in delete BOM with status 2:' MSG.
      W_FLAG = 'X'.
    ELSE.
      READ TABLE MESSTAB WITH KEY MSGTYP = 'A'.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = SY-MSGID
                  MSGNR               = SY-MSGNO
                  MSGV1               = SY-MSGV1
                  MSGV2               = SY-MSGV2
                  MSGV3               = SY-MSGV3
                  MSGV4               = SY-MSGV4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = MSG.
        REFRESH IT_BDC.
        MESSAGE S000 WITH 'Error in delete BOM with status 2:' MSG.
        W_FLAG = 'X'.
      ELSE.
        WAIT UP TO 3 SECONDS.
*      MESSAGE S000 WITH 'BOM changed successfully for' IT_LIST-MATNR.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " DELETE_BOM_WITH_USAGE_2
*&---------------------------------------------------------------------*
*&      Form  copy_to_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPY_TO_ZTABLE.
  DATA: LT_DATA LIKE TABLE OF ZTPP_MOD_BOM_HIS WITH HEADER LINE.
*        LT_VPP_BOM LIKE TABLE OF ZVPP_BOM WITH HEADER LINE.
  DATA: L_VER LIKE ZTPP_MOD_BOM_HIS-VERSON,
            L_STLNR LIKE MAST-STLNR,
            L_TIME LIKE SY-UZEIT.

  DATA : BEGIN OF LT_STB OCCURS 0.
          INCLUDE STRUCTURE STPOB.
  DATA : END OF LT_STB.

  DATA : BEGIN OF ADD_WA OCCURS 0.
          INCLUDE STRUCTURE CSZALT.
  DATA : END OF ADD_WA.

  CHECK W_FLAG IS INITIAL.


  SELECT SINGLE STLNR INTO L_STLNR
            FROM MAST
           WHERE MATNR = IT_LIST-MATNR
            AND WERKS = IT_LIST-WERKS
            AND STLAN = '2'.

  LT_STB-STLTY = 'M'.
  LT_STB-STLNR = L_STLNR.
  APPEND LT_STB.

  CALL FUNCTION 'GET_STPO'
   EXPORTING
     ALL                    = 'X'
     ALTER                  = '01'
     DATUB                  = SY-DATUM
     DATUV                  = SY-DATUM
*     NO_BUFFER              = ' '
     SET                    = 'X'
*     VALID                  = 'X'
*     VIEWNAME               =
    TABLES
      ADD_WA                 = ADD_WA
      WA                     = LT_STB
   EXCEPTIONS
     CALL_INVALID           = 1
     END_OF_TABLE           = 2
     GET_WITHOUT_SET        = 3
     KEY_INCOMPLETE         = 4
     KEY_INVALID            = 5
     NO_RECORD_FOUND        = 6
     VIEWNAME_INVALID       = 7
     OTHERS                 = 8
            .

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_VPP_BOM
*            FROM ZVPP_BOM
*           WHERE MATNR = IT_LIST-MATNR
*            AND WERKS = IT_LIST-WERKS
*            AND STLAN = '2'
*            AND STLTY = 'M'
*            AND DATUV <= SY-DATUM
*            AND LOEKZ = ' '.
  SELECT SINGLE MAX( VERSON ) INTO L_VER
    FROM ZTPP_MOD_BOM_HIS
    WHERE MATNR = IT_LIST-MATNR.
  L_VER = L_VER + 1.
  L_TIME = SY-UZEIT.
  LOOP AT LT_STB.
    LT_DATA-MATNR = IT_LIST-MATNR.
    LT_DATA-UPGN = LT_STB-UPGN.
    LT_DATA-IDNRK = LT_STB-IDNRK.
    LT_DATA-VERSON = L_VER.
    LT_DATA-MENGE = LT_STB-MENGE.
    LT_DATA-MEINS = LT_STB-MEINS.

    LT_DATA-EITM = LT_STB-EITM.

    SELECT SINGLE MAKTX INTO LT_DATA-MAKTX_M
       FROM MAKT
       WHERE MATNR = IT_LIST-MATNR
         AND SPRAS = 'EN'.
    SELECT SINGLE MAKTX INTO LT_DATA-MAKTX_C
        FROM MAKT
        WHERE MATNR = LT_DATA-IDNRK
          AND SPRAS = 'EN'.

    LT_DATA-CREATE_BY = SY-UNAME.
    LT_DATA-CREATE_ON = SY-DATUM.
    LT_DATA-CREATE_TIME = L_TIME.
    LT_DATA-CONF_DATE = SY-DATUM.

    LT_DATA-DATUV = LT_STB-DATUV.
    LT_DATA-DATUB = LT_STB-DATUB.

    LT_DATA-AENNR = LT_STB-AENNR.
    LT_DATA-AENNR_TO = LT_STB-AENRA.
    APPEND LT_DATA.
  ENDLOOP.

*  LOOP AT LT_VPP_BOM.
*    LT_DATA-MATNR = LT_VPP_BOM-MATNR.
*    LT_DATA-UPGN = LT_VPP_BOM-UPGN.
*    LT_DATA-IDNRK = LT_VPP_BOM-IDNRK.
*    LT_DATA-VERSON = L_VER.
*    LT_DATA-MENGE = LT_VPP_BOM-MENGE.
*    LT_DATA-MEINS = LT_VPP_BOM-MEINS.
*    LT_DATA-DATUV = LT_VPP_BOM-DATUV.
*    LT_DATA-EITM = LT_VPP_BOM-EITM.
*    LT_DATA-AENNR = LT_VPP_BOM-AENNR.
*    SELECT SINGLE MAKTX INTO LT_DATA-MAKTX_M
*       FROM MAKT
*       WHERE MATNR = LT_VPP_BOM-MATNR
*         AND SPRAS = 'EN'.
*    SELECT SINGLE MAKTX INTO LT_DATA-MAKTX_C
*        FROM MAKT
*        WHERE MATNR = LT_DATA-IDNRK
*          AND SPRAS = 'EN'.
*
*    LT_DATA-CREATE_BY = SY-UNAME.
*    LT_DATA-CREATE_ON = SY-DATUM.
*    LT_DATA-CREATE_TIME = SY-UZEIT.
*    LT_DATA-conf_date = SY-DATUM..
*    APPEND LT_DATA.
*  ENDLOOP.
  INSERT ZTPP_MOD_BOM_HIS FROM TABLE LT_DATA.
  IF SY-SUBRC = 0.
    COMMIT WORK.
    W_REFRESH = 'X'.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " copy_to_ztable
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE.
  PERFORM CHECK_VTYPE.
  PERFORM CHECK_MCODE.
  PERFORM CHECK_MATNR.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_EKGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CHECK_EKGRP.
*  IF P_EKGRP EQ ' '.
*    WA_EKGRP_T = 'ZZZ'.
*  ELSE.
*    SELECT SINGLE * FROM T024 WHERE EKGRP = P_EKGRP.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZZ) WITH TEXT-M02.
*    ENDIF.
*    WA_EKGRP_T = WA_EKGRP_F = P_EKGRP.
*  ENDIF.
*ENDFORM.                    " CHECK_EKGRP
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MATNR.
  IF P_MATNR EQ ' '.
    WA_MATNR_T = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSE.
    SELECT SINGLE * FROM MARA WHERE MATNR = P_MATNR.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M04.
    ENDIF.
    WA_MATNR_T = WA_MATNR_F = P_MATNR.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_vtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VTYPE.
  LOOP AT S_VTYPE.
    TRANSLATE S_VTYPE-LOW  TO UPPER CASE.
    TRANSLATE S_VTYPE-HIGH TO UPPER CASE.
    MODIFY S_VTYPE.
  ENDLOOP.

  IF S_VTYPE-LOW EQ ' '.
    WA_VTYPE_T = 'ZZZ'.
  ELSE.
    WA_VTYPE_F = WA_VTYPE_T = S_VTYPE-LOW.
  ENDIF.
ENDFORM.                    " check_vtype
*&---------------------------------------------------------------------*
*&      Form  check_mcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MCODE.
  WA_MCODE_F = WA_MCODE_T = P_MCODE.
ENDFORM.                    " check_mcode
*&---------------------------------------------------------------------*
*&      Form  GUI_ALV_CELL_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GUI_ALV_CELL_CONTROL.
  LOOP AT IT_LIST.
    CLEAR IT_LIST-CELLTAB.
** Changed By Furong on 11/18/10
*    PERFORM FILL_CELLTAB USING    IT_LIST-STLST
*                         CHANGING IT_LIST-CELLTAB[].
    PERFORM FILL_CELLTAB USING    IT_LIST-LABOR
                          CHANGING IT_LIST-CELLTAB[].
** end of change
    MODIFY IT_LIST.
  ENDLOOP.

ENDFORM.                    " GUI_ALV_CELL_CONTROL
*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*FORM FILL_CELLTAB USING P_STLST CHANGING C_CELLTAB  TYPE LVC_T_STYL.
FORM FILL_CELLTAB USING P_LABOR CHANGING C_CELLTAB  TYPE LVC_T_STYL.
  DATA : L_FIELDCATALOG  TYPE LVC_S_FCAT.
  DATA : L_CELLTAB       TYPE LVC_S_STYL.

  LOOP AT IT_FIELDCAT INTO L_FIELDCATALOG.
    L_CELLTAB-FIELDNAME = L_FIELDCATALOG-FIELDNAME.

    IF L_CELLTAB-FIELDNAME = 'CHECK'.
      IF P_LABOR = '002'.
        L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
      ELSE.
        L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDIF.
    ELSE.
      L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDIF.
    INSERT L_CELLTAB INTO TABLE C_CELLTAB.
  ENDLOOP.
ENDFORM.                    " fill_celltab
