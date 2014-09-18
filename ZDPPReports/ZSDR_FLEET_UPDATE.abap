************************************************************************
* Program Name      : ZPPE_FLEET_UPDATE
* Author            : Furong, Wang
* Creation Date     : 07/2012
* Specifications By : Pascal Lee
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Module Cost Update
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 08/20/12   Furong                 Change new logic by MIT
*
*
************************************************************************

REPORT ZSDR_FLEET_UPDATE
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID ZMMM.

TABLES: ZTSD_UM.
DATA: IT_DATA LIKE TABLE OF ZTSD_UM WITH HEADER LINE,
      IT_UM LIKE TABLE OF IT_DATA WITH HEADER LINE.

DATA: OK_CODE  LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT    TYPE I.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDNAME.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-101.
SELECT-OPTIONS: S_SDATE FOR SY-DATUM NO-EXTENSION OBLIGATORY.
SELECT-OPTIONS: S_NATION FOR ZTSD_UM-WO_NATION.
SELECT-OPTIONS: S_WO FOR ZTSD_UM-WO_SERIAL.
SELECT-OPTIONS: S_ZVIN FOR ZTSD_UM-ZVIN.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-M01 FOR FIELD P_BATCH.
PARAMETERS: P_BATCH RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN COMMENT 30(12) TEXT-M02 FOR FIELD P_ONLINE.
PARAMETERS: P_ONLINE RADIOBUTTON GROUP GRP1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK B2.

START-OF-SELECTION.
  PERFORM READ_DATA.
  IF IT_UM[] IS not INITIAL.
    IF P_BATCH IS INITIAL.
      CALL SCREEN 0800.
    ELSE.
      IT_DATA[] = IT_UM[].
      PERFORM PROCESS_DATA.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: L_EQUNR LIKE EQUI-EQUNR.
  DATA: L_VARTABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  LOOP AT IT_DATA.
    CONCATENATE IT_DATA-MODEL_CODE IT_DATA-BODY_NO
         INTO L_EQUNR.

    REFRESH: L_VARTABLE.
    L_VARTABLE-ATNAM = 'P_FLEET'.
    L_VARTABLE-ATWRT =  'Y'.
    APPEND L_VARTABLE.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        OBJECT       = L_EQUNR
        MODE         = 'W'
      TABLES
        VAL_TABLE    = L_VARTABLE
      EXCEPTIONS
        NO_DATA      = 1
        ERROR_MODE   = 2
        ERROR_OBJECT = 3
        ERROR_VALUE  = 4
        OTHERS       = 5.

    IF SY-SUBRC <> 0.
      MESSAGE S997 WITH 'Error:' L_EQUNR.
      ROLLBACK WORK.
    ELSE.
*      IT_UM-FLET_VM_UDATE = SY-DATUM.
*      IT_UM-FLET_VM_UTIME = SY-UZEIT.
*      MODIFY IT_UM.
      UPDATE ZTSD_UM SET: FLET_VM_UDATE = SY-DATUM
                          FLET_VM_UTIME = SY-UZEIT
           WHERE MODEL_CODE = IT_DATA-MODEL_CODE
             AND BODY_NO = IT_DATA-BODY_NO.
      IF SY-SUBRC = 0.
        COMMIT WORK.
        MESSAGE S997 WITH 'Successfully update:' L_EQUNR.
      ELSE.
        ROLLBACK WORK.
        MESSAGE S997 WITH 'Error:' L_EQUNR.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MESS  text
*----------------------------------------------------------------------*
FORM DISPLAY_PROGRESS_BAR USING    P_TEXT.
  DATA: LW_TEXT(50).

  MOVE: P_TEXT TO LW_TEXT.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = LW_TEXT.

ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: L_NAME TYPE CABN-ATNAM,  "SHOP. Date
        L_ATFLV_FROM TYPE AUSP-ATFLV,
        L_ATFLV_TO TYPE AUSP-ATFLV,
        L_NUM(8) TYPE N.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        MODEL_CODE LIKE ZTSD_UM-MODEL_CODE,
        BODY_NO LIKE ZTSD_UM-BODY_NO,
    END OF LT_OBJEK.
*    update ztsd_um set FLET_VM_UDATE = '00000000'
*     where FLET_VM_UDATE is null.
*  commit work.

  REFRESH: IT_UM, IT_DATA.

  L_NAME = 'P_RP23_SHOP_DATE'.
  L_ATFLV_FROM = L_NUM = S_SDATE-LOW.
  L_ATFLV_TO = L_NUM = S_SDATE-HIGH.

  SELECT OBJEK INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
    FROM AUSP AS A
    INNER JOIN CABN AS B
    ON A~ATINN = B~ATINN
    WHERE KLART = '002'
      AND ATNAM = L_NAME
      AND ATFLV BETWEEN L_ATFLV_FROM AND L_ATFLV_TO.

  IF SY-SUBRC NE 0.
    WRITE: ' No data in vehicle master'.
    EXIT.
  ENDIF.

  LOOP AT LT_OBJEK.
    LT_OBJEK-MODEL_CODE = LT_OBJEK-OBJEK+0(3).
    LT_OBJEK-BODY_NO = LT_OBJEK-OBJEK+3(6).
    MODIFY LT_OBJEK.
  ENDLOOP.

  SELECT * FROM ZTSD_UM INTO TABLE IT_UM
    FOR ALL ENTRIES IN LT_OBJEK
  WHERE MODEL_CODE = LT_OBJEK-MODEL_CODE
    AND BODY_NO = LT_OBJEK-BODY_NO
    AND WO_SERIAL IN S_WO
    AND WO_NATION IN S_NATION
    AND ZVIN IN S_ZVIN
    AND ( FLET_VM_UDATE = '00000000' OR FLET_VM_UDATE IS NULL )
*   AND  BODY_NO <> '000000'
    AND FLET = 'Y'.

  IF SY-SUBRC NE 0.
    WRITE: ' No data in ztsd_um table'.
    EXIT.
  ENDIF.
*LOOP AT IT_UM.
*  IF NOT IT_UM-FLET_VM_UDATE IS INITIAL.
*     DELETE IT_UM.
*  ENDIF.
*  ENDLOOP.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'ST800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_UM'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
    EXPORTING
      CONTAINER_NAME              = WA_CUSTOM_CONTROL
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = W_REPID
        TXT2  = SY-SUBRC
        TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
    EXPORTING
      I_PARENT      = GRID_CONTAINER
      I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
*  WA_VARIANT-REPORT       = SY-REPID.
*  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MATNR'.
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

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
*     I_INTERNAL_TABNAME = LW_ITAB
      I_STRUCTURE_NAME   = 'ZTSD_UM'
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'WO_NATION'       ' ',
                                  ' ' 'KEY'         '',
                                  ' ' 'COLTEXT'     'Nation',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MODEL_CODE'       ' ',
                                  ' ' 'COLTEXT'     'Model No',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'BODY_NO'       ' ',
                                  ' ' 'COLTEXT'     'Body No',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZVIN'       ' ',
                                  ' ' 'COLTEXT'     'ZVIN',
                                  'E' 'OUTPUTLEN'   '30'.

ENDFORM.                    "build_field_catalog
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
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_UM[]
      IT_SORT              = IT_SORT[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0800 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'POST'.
      PERFORM PROCESS_DATA_ONLINE.
    WHEN 'REFRESH'.
      PERFORM READ_DATA.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA_ONLINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA_ONLINE .
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_ROWS[]
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

  LOOP AT LT_ROWS.
    READ TABLE IT_UM INDEX LT_ROWS-INDEX.
    IF SY-SUBRC = 0.
      IT_DATA = IT_UM.
      APPEND IT_DATA.
    ENDIF.
  ENDLOOP.
  PERFORM  PROCESS_DATA.
  PERFORM READ_DATA.
ENDFORM.                    " PROCESS_DATA_ONLINE
