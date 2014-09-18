************************************************************************
* Program Name      : ZIMR_REASON_CODE_ZIM23
* Creation Date     : 08/26/2010
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMR_REASON_CODE_ZIM23 NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

INCLUDE ZIMR_REASON_CODE_ZIM23_CLS.
INCLUDE ZIMR_REASON_CODE_ZIM23_CLS2.

TABLES: USR21,V_USR_NAME,  ZTBL, ZTBLIT, ZTCIVHD, EKKO.

TYPE-POOLS SLIS .
DATA : BEGIN OF IT_DATA OCCURS 0,
        ZFBLNO LIKE ZTBL-ZFBLNO,
        ZFHBLNO LIKE ZTBL-ZFHBLNO,
        LIFNR LIKE ZTBL-LIFNR,
        ZFETD LIKE ZTBL-ZFETD,
        ZFETA LIKE ZTBL-ZFETA,
        CDAT LIKE ZTBL-CDAT,
        ERNAM LIKE EKKO-ERNAM,
        ZFCIVNO LIKE ZTCIVHD-ZFCIVNO,
        END OF IT_DATA.

DATA: BEGIN OF IT_BLIT OCCURS 0.
        INCLUDE STRUCTURE ZTBLIT.
DATA: CELLTAB TYPE LVC_T_STYL,
      END OF IT_BLIT.

DATA: F4_FINRSN LIKE TABLE OF ZTIM_RSN_CODE WITH HEADER LINE.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL1 TYPE        SCRFNAME VALUE 'ALV_CONTAINER1',
      ALV_GRID1          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER1    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : IT_FIELDCAT1     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME1    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT1         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT1 TYPE LVC_S_LAYO, "/The Layout Structure
*       W_FIELDNAME1    LIKE LINE OF IT_FIELDCAT,
       WA_VARIANT1 TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA : IT_FIELDCAT2 TYPE SLIS_T_FIELDCAT_ALV.
DATA: WA_STBL  TYPE LVC_S_STBL.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
DATA : EVENT_RECEIVER2 TYPE REF TO LCL_EVENT_RECEIVER2.

DATA : GS_F4              TYPE LVC_S_F4.        "F4
DATA : GT_F4              TYPE LVC_T_F4.

DATA : ERROR_IN_DATA   TYPE LVC_S_STBL.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_ZFBLNO FOR ZTBL-ZFBLNO,
                S_INVNO FOR ZTCIVHD-ZFCIVNO,
                S_MATNR FOR ZTBLIT-MATNR,
                S_HBLNO FOR ZTBL-ZFHBLNO,
                S_LIFNR FOR ZTBL-LIFNR,
                S_ZFETD FOR ZTBL-ZFETD,
                S_ZFETA FOR ZTBL-ZFETA,
                S_CDAT FOR ZTBL-CDAT.
PARAMETERS:     P_USER TYPE ZPERSNO.
*SELECT-OPTIONS: S_USER type ZPERSNO. " FOR EKKO-ERNAM.
SELECTION-SCREEN END OF BLOCK BLOCK1.

*AT SELECTION-SCREEN OUTPUT.

*AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
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

  DATA: BEGIN OF LT_INVNO OCCURS 0,
        ZFBLNO LIKE ZTBLIT-ZFBLNO,
        ZFCIVNO LIKE ZTCIVHD-ZFCIVNO,
        END OF LT_INVNO.

  DATA: L_INVNO LIKE ZTCIVHD-ZFCIVNO,
        L_ERNAM LIKE EKKO-ERNAM,
        L_ZERO(1) VALUE '0'.

  IF P_USER IS INITIAL.
    SELECT A~ZFBLNO ZFHBLNO A~LIFNR ZFETD ZFETA A~CDAT C~ERNAM
     INTO CORRESPONDING FIELDS OF TABLE IT_DATA
     FROM ZTBL AS A
     INNER JOIN ZTBLIT AS B
     ON A~ZFBLNO = B~ZFBLNO
     INNER JOIN EKKO AS C
     ON B~EBELN = C~EBELN
      WHERE A~ZFBLNO IN S_ZFBLNO
        AND ZFHBLNO IN S_HBLNO
        AND A~LIFNR IN S_LIFNR
        AND ZFETD IN S_ZFETD
        AND ZFETA IN S_ZFETA
*      AND ZFVIA = 'AIR'
        AND A~CDAT IN S_CDAT
        AND MATNR IN S_MATNR.
  ELSE.
    L_ERNAM = P_USER.
    SHIFT L_ERNAM LEFT DELETING LEADING L_ZERO.
    SELECT A~ZFBLNO ZFHBLNO A~LIFNR ZFETD ZFETA A~CDAT C~ERNAM
      INTO CORRESPONDING FIELDS OF TABLE IT_DATA
      FROM ZTBL AS A
      INNER JOIN ZTBLIT AS B
      ON A~ZFBLNO = B~ZFBLNO
      INNER JOIN EKKO AS C
      ON B~EBELN = C~EBELN
       WHERE A~ZFBLNO IN S_ZFBLNO
         AND ZFHBLNO IN S_HBLNO
         AND A~LIFNR IN S_LIFNR
         AND ZFETD IN S_ZFETD
         AND ZFETA IN S_ZFETA
         AND A~CDAT IN S_CDAT
         AND MATNR IN S_MATNR
*      AND ZFVIA = 'AIR'
         AND C~ERNAM = L_ERNAM.

  ENDIF.

  SELECT ZFBLNO ZFCIVNO INTO TABLE LT_INVNO
    FROM ZTCIVHD AS A
    INNER JOIN ZTCIVIT AS B
    ON A~ZFCIVRN = B~ZFCIVRN
    FOR ALL ENTRIES IN IT_DATA
    WHERE ZFCIVNO IN S_INVNO
      AND ZFBLNO = IT_DATA-ZFBLNO.

  IF S_INVNO IS INITIAL.
    LOOP AT IT_DATA.
      READ TABLE LT_INVNO WITH KEY ZFBLNO = IT_DATA-ZFBLNO.
      IT_DATA-ZFCIVNO = LT_INVNO-ZFCIVNO.
      MODIFY IT_DATA.
      CLEAR: LT_INVNO, IT_DATA.
    ENDLOOP.
  ELSE.
    LOOP AT IT_DATA.
*        SELECT SINGLE ZFCIVNO INTO L_INVNO
*         FROM ZTCIVHD AS A
*         INNER JOIN ZTCIVIT AS B
*         ON A~ZFCIVRN = B~ZFCIVRN
*         WHERE ZFCIVNO IN S_INVNO
*          AND ZFBLNO = IT_DATA-ZFBLNO.
      READ TABLE LT_INVNO WITH KEY ZFBLNO = IT_DATA-ZFBLNO.
      IF SY-SUBRC = 0.
        IT_DATA-ZFCIVNO = LT_INVNO-ZFCIVNO.
        MODIFY IT_DATA.
      ELSE.
        DELETE IT_DATA.
      ENDIF.
      CLEAR: LT_INVNO, IT_DATA.
    ENDLOOP.
  ENDIF.


  SORT IT_DATA BY ZFBLNO.
  DELETE ADJACENT DUPLICATES FROM IT_DATA.

ENDFORM.

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
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
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
    WHEN 'RSN_CODE'.
      PERFORM GET_DETAIL_DATA.
      CALL SCREEN 300.
    WHEN 'CHANGE'.
      PERFORM CHANGE_ZIM22.
    WHEN 'DISPLAY'.
      PERFORM DISPLAY_ZIM23.
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
               IT_OUTTAB        = IT_DATA[]
               IT_SORT          = IT_SORT[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
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
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
*  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'ZFBLNO'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

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

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
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

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT IT_FIELDNAME USING :

                                  'S' 'ZFBLNO'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'B/L No',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZFHBLNO'       ' ',
                                  ' ' 'COLTEXT'     'House of B/L',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZFCIVNO'       ' ',
                                  ' ' 'COLTEXT'     'Invoice No',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZFETD'       ' ',
                                  ' ' 'COLTEXT'     'E.T.D',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZFETA'     ' ',
                                  ' ' 'COLTEXT'     'E.T.A',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'CDAT'        ' ',
                                  ' ' 'COLTEXT'     'Created on',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ERNAM'       ' ',
                                  ' ' 'COLTEXT'     'User',
                                  'E' 'OUTPUTLEN'   '10'.

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
                               P_FIELDNAME TYPE SLIS_T_FIELDCAT_ALV
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE P_FIELDNAME INTO W_FIELDNAME
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
*&      Form  GET_DETAIL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DETAIL_DATA.
  DATA: LT_TEMP LIKE TABLE OF ZTBLIT WITH HEADER LINE.

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
  READ TABLE IT_DATA INDEX LT_ROWS-INDEX.
  IF IT_DATA-ZFBLNO = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.

  REFRESH IT_BLIT.
  SELECT * INTO TABLE LT_TEMP
    FROM ZTBLIT
    WHERE ZFBLNO = IT_DATA-ZFBLNO.
  LOOP AT LT_TEMP.
    MOVE-CORRESPONDING LT_TEMP TO IT_BLIT.
    IT_BLIT-ERNAM = IT_DATA-ERNAM.
    APPEND IT_BLIT.
  ENDLOOP.

  SORT IT_BLIT BY ZFBLNO EBELN EBELP.
ENDFORM.                    " GET_DETAIL_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_ZIM22
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_ZIM22.
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
  READ TABLE IT_DATA INDEX LT_ROWS-INDEX.
  IF IT_DATA-ZFBLNO = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'ZPHBLNO' FIELD IT_DATA-ZFHBLNO.
  CALL TRANSACTION 'ZIM22' AND SKIP FIRST SCREEN.
ENDFORM.                    " CHANGE_ZIM22
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZIM23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ZIM23.
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
  READ TABLE IT_DATA INDEX LT_ROWS-INDEX.
  IF IT_DATA-ZFBLNO = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  SET PARAMETER ID 'ZPHBLNO' FIELD IT_DATA-ZFHBLNO.
  CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.

ENDFORM.                    " DISPLAY_ZIM23
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'ST300'.
  SET TITLEBAR 'ST300'.
ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_DETAIL_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DETAIL_ALV OUTPUT.
  IF GRID_CONTAINER1 IS INITIAL.
    PERFORM CREATE_OBJECT1.
    PERFORM SET_ATTRIBUTES_ALV_GRID1.
*    PERFORM BUILD_SORTCAT_DISPLAY1.
    PERFORM BUILD_FIELD_CATALOG1 USING 'IT_BLIT'.
    PERFORM ALV_CELL_CONTROL.
    PERFORM ALV_EVENT.
    PERFORM ASSIGN_ITAB_TO_ALV1.
*    PERFORM sssign_event_9000.
  ELSE.
    WA_STBL-ROW = 'X'.
    WA_STBL-COL = 'X'.

    CALL METHOD ALV_GRID1->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " display_DETAIL_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_OBJECT1.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER1
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL1
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
  CREATE OBJECT ALV_GRID1
         EXPORTING I_PARENT = GRID_CONTAINER1
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_OBJECT1
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID1.

  CLEAR : WA_IS_LAYOUT1, WA_VARIANT1.

*//-- Set Layout Structure
  WA_IS_LAYOUT1-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT1-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT1-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT1-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT1-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT1-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT1-REPORT       = SY-REPID.
  WA_VARIANT1-USERNAME     = SY-UNAME.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID1
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1180   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG1 USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT1,  IT_FIELDCAT1[],
         IT_FIELDNAME1, IT_FIELDNAME1[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME1.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT1 IT_FIELDNAME1 USING :

                                  'S' 'ZFBLNO'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'B/L No',
                                  'E' 'OUTPUTLEN'   '12',

*                                  'S' 'ZFHBLNO'       ' ',
*                                  ' ' 'COLTEXT'     'House of B/L',
*                                  'E' 'OUTPUTLEN'   '15',
*
*                                  'S' 'LIFNR'       ' ',
*                                  ' ' 'COLTEXT'     'Vendor',
*                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELN'     ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELP'        ' ',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'TXZ01'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'BLMENGE'       ' ',
                                  ' ' 'COLTEXT'     'B/L Qty',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'NETPR'       ' ',
                                  ' ' 'COLTEXT'     'Net Price',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ERNAM'       ' ',
                                  ' ' 'COLTEXT'     'Created by',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'INIRSN'       ' ',
                                  ' ' 'COLTEXT'     'Init. Rsn',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'HMMARSN'       ' ',
                                  ' ' 'COLTEXT'     'HMMA Rsn',
                                  ' ' 'F4AVAILABL'  'X',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'FINRSN'       ' ',
                                  ' ' 'COLTEXT'     'Final Rsn',
                                  ' ' 'F4AVAILABL'  'X',
                                  'E' 'OUTPUTLEN'   '10'.

ENDFORM.                    " BUILD_FIELD_CATALOG1
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV1.
  CALL METHOD ALV_GRID1->SET_TABLE_FOR_FIRST_DISPLAY

    EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT1
                I_SAVE           = WA_SAVE
                IS_VARIANT       = WA_VARIANT1
                I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
      CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT1[]
                IT_OUTTAB        = IT_BLIT[]
                IT_SORT          = IT_SORT1[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV1
*&---------------------------------------------------------------------*
*&      Form  alv_cell_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_CELL_CONTROL.
  DATA : L_FIELDCAT1 TYPE LVC_S_FCAT.
  DATA : L_CELLTAB TYPE LVC_S_STYL.
  DATA: L_INDEX LIKE SY-TABIX.

  LOOP AT IT_BLIT.
    L_INDEX = SY-TABIX.
    LOOP AT IT_FIELDCAT1 INTO L_FIELDCAT1.
      L_CELLTAB-FIELDNAME = L_FIELDCAT1-FIELDNAME.
      CASE L_CELLTAB-FIELDNAME.
        WHEN 'FINRSN'.
          L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        WHEN 'HMMARSN'.
          L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        WHEN OTHERS.
          L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
      ENDCASE.
      INSERT L_CELLTAB INTO TABLE IT_BLIT-CELLTAB.
    ENDLOOP.
    CLEAR: L_CELLTAB, L_FIELDCAT1.
    MODIFY IT_BLIT INDEX L_INDEX.
  ENDLOOP.

ENDFORM.                    " alv_cell_control
*&---------------------------------------------------------------------*
*&      Form  ALV_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_EVENT.
  CALL METHOD ALV_GRID1->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.
*  SET HANDLER event_receiver->handle_hotspot_click FOR ALV_grid1.
  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID1.
  CREATE OBJECT EVENT_RECEIVER2.
  SET HANDLER EVENT_RECEIVER2->HANDLE_F4            FOR ALV_GRID1.

  PERFORM F4_FIELD_ASSIGN.

ENDFORM.                    " ALV_EVENT
*&---------------------------------------------------------------------*
*&      Form  f4_field_assign
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FIELD_ASSIGN.
  CLEAR GS_F4.
*  GS_F4-FIELDNAME  = 'INIRSN'.
*  GS_F4-REGISTER   = 'X'.
*  APPEND GS_F4 TO GT_F4.
*
*  CLEAR GS_F4.
*  GS_F4-FIELDNAME  = 'FINRSN'.
*  GS_F4-REGISTER   = 'X'.
*  APPEND GS_F4 TO GT_F4.

  GS_F4-FIELDNAME  = 'INIRSN'.
  GS_F4-REGISTER   = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.

  GS_F4-FIELDNAME  = 'HMMARSN'.
  GS_F4-REGISTER   = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.

  CLEAR GS_F4.
  GS_F4-FIELDNAME  = 'FINRSN'.
  GS_F4-REGISTER   = 'X'.
  INSERT GS_F4 INTO TABLE GT_F4.


  CALL METHOD ALV_GRID1->REGISTER_F4_FOR_FIELDS
    EXPORTING
      IT_F4 = GT_F4.

ENDFORM.                    " f4_field_assign
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
      WHEN 'HMMARSN' OR 'FINRSN'.

        CALL METHOD U_CHANGED->GET_CELL_VALUE
                   EXPORTING
                      I_ROW_ID  = LS_GOOD-ROW_ID
                      I_FIELDNAME = LS_GOOD-FIELDNAME
                   IMPORTING
                      E_VALUE =   LV_VALUE.
        PERFORM CHECK_REASON_CODE USING LV_VALUE.
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
*&      Form  HANDLE_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
FORM HANDLE_F4  USING    U_FIELDNAME
                         U_FIELDVALUE
                         U_ROW_NO
                         U_EVENT_DATA
                         U_BAD_CELLS
                         U_DISPLAY.
  CASE U_FIELDNAME.
    WHEN 'INIRSN'.
*      PERFORM F4_INIRSN.
    WHEN 'HMMARSN'.
      PERFORM F4_FINRSN USING '1'.
    WHEN 'FINRSN'.
      PERFORM F4_FINRSN USING '2'.

  ENDCASE.

ENDFORM.                                                    " HANDLE_F4
*&---------------------------------------------------------------------*
*&      Form  F4_FINRSN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FINRSN USING P_RSN.
  DATA : L_LINE TYPE SY-TABIX.
  DATA : CT_CELLS    TYPE LVC_T_CELL,
         L_CELL     TYPE LVC_S_CELL.

*  REFRESH : TABLE_FIELDS, ENTRIES_CHAR, F4_COLOR, FIELD_TAB.

*--> Read Dynpro Value from Screen.
  CALL METHOD ALV_GRID1->GET_SELECTED_CELLS
    IMPORTING
      ET_CELL = CT_CELLS.

  READ TABLE CT_CELLS INTO L_CELL INDEX 1.
  READ TABLE IT_BLIT INDEX L_CELL-ROW_ID-INDEX.

  SELECT *
    FROM ZTIM_RSN_CODE
    INTO TABLE F4_FINRSN.

*--> pop up
  PERFORM F4_FINRSN_FILEDCAT.
  PERFORM F4_FINRSN_POPUP USING L_CELL-ROW_ID-INDEX P_RSN.

ENDFORM.                                                    " F4_FINRSN
*&---------------------------------------------------------------------*
*&      Form  F4_COMM_FILEDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FINRSN_FILEDCAT.
  DATA : L_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  REFRESH IT_FIELDCAT2.

  L_FIELDCAT-FIELDNAME             = 'CODE'.
  L_FIELDCAT-SELTEXT_S             = 'Reason Code'.
  L_FIELDCAT-REF_TABNAME           = 'ZTIM_RSN_CODE'.
  APPEND L_FIELDCAT TO IT_FIELDCAT2.
  L_FIELDCAT-FIELDNAME             = 'CODE_DESC'.
  L_FIELDCAT-SELTEXT_S             = 'Description'.
  L_FIELDCAT-REF_TABNAME           = 'ZTIM_RSN_CODE'.
  APPEND L_FIELDCAT TO IT_FIELDCAT2.
  L_FIELDCAT-FIELDNAME             = 'CODE_DESC_LONG'.
  L_FIELDCAT-SELTEXT_S             = 'Description'.
  L_FIELDCAT-REF_TABNAME           = 'ZTIM_RSN_CODE'.
  APPEND L_FIELDCAT TO IT_FIELDCAT2.

ENDFORM.                    " F4_finsn_FILEDCAT
*&---------------------------------------------------------------------*
*&      Form  F4_FINRSN_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CELL_ROW_ID_INDEX  text
*----------------------------------------------------------------------*
FORM F4_FINRSN_POPUP  USING   I_ROW_ID P_RSN.
  DATA : ES_SELFIELD   TYPE SLIS_SELFIELD,
         DATA_PROTOCOL TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            I_SELECTION = 'X'
            I_TABNAME   = 'F4_FINRSN'
            IT_FIELDCAT = IT_FIELDCAT2
       IMPORTING
            ES_SELFIELD = ES_SELFIELD
       TABLES
            T_OUTTAB    = F4_FINRSN.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
** selecting value in pop-up screen
    READ TABLE F4_FINRSN INDEX ES_SELFIELD-TABINDEX.

* modify cell
    CHECK NOT F4_FINRSN-CODE IS INITIAL.
    IF P_RSN = 1.
      IT_BLIT-HMMARSN = F4_FINRSN-CODE.
    ELSE.
      IT_BLIT-FINRSN = F4_FINRSN-CODE.
    ENDIF.
    MODIFY IT_BLIT INDEX I_ROW_ID.
    CALL METHOD ALV_GRID1->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    " F4_FINRSN_POPUP
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM SAVE_ZTBIT.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_ztbit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_ZTBIT.
  LOOP AT IT_BLIT.
    UPDATE ZTBLIT SET FINRSN = IT_BLIT-FINRSN
                   HMMARSN = IT_BLIT-HMMARSN
                  WHERE ZFBLNO = IT_BLIT-ZFBLNO
                  AND ZFBLIT = IT_BLIT-ZFBLIT.
  ENDLOOP.
*  MODIFY ZTBLIT FROM TABLE IT_BLIT. " transporting finrsn.
*  IF SY-SUBRC = 0.
  COMMIT WORK.
  MESSAGE S000 WITH  'Data Saved'.
*  ELSE.
*    ROLLBACK WORK.
*    MESSAGE E000 WITH  'SAVE ERROR'.
*  ENDIF.
ENDFORM.                    " save_ztbit
*&---------------------------------------------------------------------*
*&      Form  check_reason_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_REASON_CODE USING P_RSN_CODE.
  DATA: L_CODE LIKE ZTIM_RSN_CODE-CODE.
  SELECT SINGLE CODE INTO L_CODE
     FROM ZTIM_RSN_CODE
     WHERE CODE = P_RSN_CODE.
  IF SY-SUBRC = 0.
  ELSE.
    ERROR_IN_DATA = 'X'.
  ENDIF.
ENDFORM.                    " check_reason_code
