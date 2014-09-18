************************************************************************
* Program Name      : ZIMM_PILOT_MATERIAL_MASTER
* Creation Date     : 09/14/09
* Development Request No :
* Addl Documentation:
* Description       : Send Materail Master to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMM_PILOT_MATERIAL_MASTER NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS, VRM.
TABLES: ZTMM_PILOT_MATL.
*DATA: IT_DATA LIKE TABLE OF ZTMM_PILOT_MATL WITH HEADER LINE.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTMM_PILOT_MATL.
*DATA: UDATE LIKE SY-DATUM.
DATA: END OF IT_DATA.

CONSTANTS: C_DEST(10) VALUE 'WMPM01'.

*DATA: W_FILENAME LIKE RLGRAP-FILENAME.

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

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM DEFAULT SY-DATUM.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: P_BATCH AS CHECKBOX  USER-COMMAND CHAL.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-U01 FOR FIELD P_SD.
PARAMETERS: P_SD RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 20(8) TEXT-U02 FOR FIELD P_EO.
PARAMETERS: P_EO RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 40(15) TEXT-U03 FOR FIELD P_ES.
PARAMETERS: P_ES RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN  END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.
*  IF SY-UCOMM = 'CHAL'.
  PERFORM MODIFY_SCREEN_ALL.
*  ENDIF.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
    MESSAGE I009 WITH 'No data'.
  ELSE.
    IF P_BATCH = 'X'.
      PERFORM SAVE_SEND_DATA.
    ELSE.
      IF P_ES = 'X'.
        PERFORM SAVE_SEND_DATA.
      ENDIF.
      PERFORM DISPLAY_DATA.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LT_CDHDR LIKE TABLE OF CDHDR WITH HEADER LINE.
  DATA: BEGIN OF LT_CDPOS OCCURS 0.
          INCLUDE STRUCTURE CDPOS.
  DATA: UDATE LIKE SY-DATUM.
  DATA: END OF LT_CDPOS.

  DATA : BEGIN OF LT_MATNR OCCURS 0,
         MATNR LIKE ZTMM_PILOT_MATL-MATNR,
         WERKS LIKE ZTMM_PILOT_MATL-WERKS,
         MAKTX LIKE ZTMM_PILOT_MATL-MAKTX,
         EKGRP LIKE ZTMM_PILOT_MATL-EKGRP,
         PROFL LIKE ZTMM_PILOT_MATL-PROFL,
         MMSTA LIKE ZTMM_PILOT_MATL-MMSTA,
         FABKZ LIKE ZTMM_PILOT_MATL-FABKZ,
         ERSDA LIKE ZTMM_PILOT_MATL-ERSDA,
         LAEDA LIKE ZTMM_PILOT_MATL-LAEDA,
         END OF LT_MATNR.

  IF P_SD = 'X' AND P_BATCH IS INITIAL.
    SELECT * INTO TABLE IT_DATA
      FROM ZTMM_PILOT_MATL
      WHERE UDATE IN S_DATUM.
  ELSE.
    SELECT * INTO TABLE LT_CDHDR
      FROM CDHDR
      WHERE OBJECTCLAS = 'MATERIAL'
        AND ( TCODE = 'MM01' OR TCODE = 'MM02' )
        AND UDATE IN S_DATUM.

    SELECT OBJECTID CHANGENR TABNAME TABKEY FNAME VALUE_NEW
      INTO CORRESPONDING FIELDS OF TABLE LT_CDPOS
      FROM CDPOS
      FOR ALL ENTRIES IN LT_CDHDR
      WHERE OBJECTCLAS = LT_CDHDR-OBJECTCLAS
        AND OBJECTID = LT_CDHDR-OBJECTID
        AND CHANGENR = LT_CDHDR-CHANGENR
        AND ( FNAME = 'MATNR' OR FNAME = 'WERKS' OR FNAME = 'MAKTX'
              OR FNAME = 'EKGRP' OR FNAME = 'PROFL'
              OR FNAME = 'MMSTA' OR FNAME = 'FABKZ' ).

    LOOP AT LT_CDHDR.
      LOOP AT LT_CDPOS WHERE OBJECTID = LT_CDHDR-OBJECTID
                               AND  CHANGENR = LT_CDHDR-CHANGENR.
        LT_CDPOS-UDATE = LT_CDHDR-UDATE.
        MODIFY LT_CDPOS.
      ENDLOOP.
    ENDLOOP.

    SORT LT_CDPOS BY OBJECTID  DESCENDING UDATE TABNAME FNAME.
    DELETE ADJACENT DUPLICATES FROM LT_CDPOS COMPARING OBJECTID UDATE
           TABNAME FNAME.

    SORT LT_CDPOS BY OBJECTID UDATE TABNAME FNAME.
    SORT LT_CDHDR BY OBJECTID UDATE.
    DELETE ADJACENT DUPLICATES FROM LT_CDHDR COMPARING OBJECTID UDATE.

    LOOP AT LT_CDHDR.

      IT_DATA-MATNR = LT_CDHDR-OBJECTID.
      CASE LT_CDHDR-TCODE.
        WHEN 'MM02'.
          LOOP AT LT_CDPOS WHERE OBJECTID = LT_CDHDR-OBJECTID
                            AND UDATE = LT_CDHDR-UDATE.
            CASE LT_CDPOS-FNAME.
              WHEN 'WERKS'.
                IT_DATA-WERKS = LT_CDPOS-VALUE_NEW.
              WHEN 'MAKTX'.
                IT_DATA-MAKTX = LT_CDPOS-VALUE_NEW.
              WHEN 'EKGRP'.
                IT_DATA-EKGRP = LT_CDPOS-VALUE_NEW.
                IT_DATA-WERKS = LT_CDPOS-TABKEY+21(4).
              WHEN 'PROFL'.
                IT_DATA-PROFL = LT_CDPOS-VALUE_NEW.
              WHEN 'MMSTA'.
                IT_DATA-MMSTA = LT_CDPOS-VALUE_NEW.
                IT_DATA-WERKS = LT_CDPOS-TABKEY+21(4).
              WHEN 'FABKZ'.
                IT_DATA-FABKZ = LT_CDPOS-VALUE_NEW.
                IT_DATA-WERKS = LT_CDPOS-TABKEY+21(4).
            ENDCASE.
          ENDLOOP.
          if IT_DATA-WERKS is initial.
             IT_DATA-WERKS = 'P001'.
          ENDIF.
          IT_DATA-BUKRS = 'H201'.
          SELECT SINGLE ERSDA INTO IT_DATA-ERSDA
             FROM MARA
             WHERE MATNR = LT_CDHDR-OBJECTID.
          IT_DATA-LAEDA = LT_CDHDR-UDATE.
          IF IT_DATA-WERKS IS INITIAL AND IT_DATA-MAKTX IS INITIAL
            AND IT_DATA-EKGRP IS INITIAL AND IT_DATA-PROFL IS INITIAL
            AND IT_DATA-MMSTA IS INITIAL AND IT_DATA-FABKZ IS INITIAL.
          ELSE.
            IT_DATA-UDATE = LT_CDHDR-UDATE.
            IT_DATA-UTIME = LT_CDHDR-UTIME.
            APPEND IT_DATA.
          ENDIF.
        WHEN 'MM01'.
*          SELECT SINGLE WERKS MAKTX EKGRP PROFL MMSTA FABKZ
*           INTO (IT_DATA-WERKS, IT_DATA-MAKTX,  IT_DATA-EKGRP,
*                 IT_DATA-PROFL, IT_DATA-MMSTA, IT_DATA-FABKZ)
          SELECT A~MATNR WERKS MAKTX EKGRP PROFL MMSTA FABKZ
                 ERSDA LAEDA
            INTO TABLE LT_MATNR
            FROM MARA AS A
            INNER JOIN MARC AS B
            ON A~MATNR = B~MATNR
            INNER JOIN MAKT AS C
            ON A~MATNR = C~MATNR
            WHERE A~MATNR = IT_DATA-MATNR.
          IF SY-SUBRC = 0.
            LOOP AT LT_MATNR.
              IT_DATA-WERKS = LT_MATNR-WERKS.
              IT_DATA-MAKTX = LT_MATNR-MAKTX.
              IT_DATA-EKGRP = LT_MATNR-EKGRP.
              IT_DATA-PROFL = LT_MATNR-PROFL.
              IT_DATA-MMSTA = LT_MATNR-MMSTA.
              IT_DATA-FABKZ = LT_MATNR-FABKZ.
              IT_DATA-ERSDA = LT_MATNR-ERSDA.
              IT_DATA-LAEDA = LT_CDHDR-UDATE.
              IT_DATA-UDATE = LT_CDHDR-UDATE.
              IT_DATA-UTIME = LT_CDHDR-UTIME.
              IT_DATA-BUKRS = 'H201'.
              APPEND IT_DATA.
            ENDLOOP.
          ENDIF.
      ENDCASE.
      CLEAR: IT_DATA.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_SEND_DATA.
  DATA: L_RESULT(1),
        L_TOTREC TYPE I,
        L_MSGTXT(60).

  DATA: LT_OUTPUT LIKE TABLE OF ZSMM_PILOT_MATL WITH HEADER LINE.
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO LT_OUTPUT.
    APPEND LT_OUTPUT.
  ENDLOOP.

  DESCRIBE TABLE IT_DATA LINES L_TOTREC.

  CALL FUNCTION 'Z_FMM_PILOT_MATL'
     DESTINATION C_DEST
     IMPORTING
       FLAG          = L_RESULT
     TABLES
       I_PILOT_MATL  = LT_OUTPUT
     EXCEPTIONS
            COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
            SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.
  IF SY-SUBRC = 0.
    WRITE: / 'Total record number(s) are : ', L_TOTREC,
           'were sent successfully'.
    LOOP AT IT_DATA.
      IT_DATA-ERSDA = SY-DATUM.
      IT_DATA-IF_DATE = SY-DATUM.
      IT_DATA-IF_TIME = SY-UZEIT.

      IT_DATA-FLAG = 'S'.
      MODIFY IT_DATA.
    ENDLOOP.

  ELSE.
    WRITE: / 'EAI Fail, Total records are: ', L_TOTREC.
    LOOP AT IT_DATA.
      IT_DATA-IF_DATE = SY-DATUM.
      IT_DATA-IF_TIME = SY-UZEIT.
      IT_DATA-FLAG = 'E'.
      MODIFY IT_DATA.
    ENDLOOP.
  ENDIF.
*  DELETE FROM ZTMM_PILOT_MATL WHERE ERSDA IN S_DATUM.
*  INSERT ZTMM_PILOT_MATL FROM TABLE IT_DATA.
  MODIFY ZTMM_PILOT_MATL FROM TABLE IT_DATA.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_ALL.

*  LOOP AT SCREEN .
*    IF P_BATCH = 'X'.  " AND SCREEN-GROUP1 EQ 'ABC'.
*      P_SD = ' '.
*      P_EO = ' '.
*      P_ES = ' '.
*      SCREEN-INVISIBLE = 1.
*      SCREEN-ACTIVE    = 0.
*      SCREEN-INPUT     = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

  LOOP AT SCREEN.
    IF P_BATCH = 'X' AND SCREEN-GROUP1 EQ 'ABC'.

      SCREEN-INVISIBLE = 1.
      SCREEN-ACTIVE    = 0.
      SCREEN-INPUT     = 0.
      MODIFY SCREEN.
*    ELSE.
*      SCREEN-INVISIBLE = 0.
*      SCREEN-ACTIVE    = 1.
*      SCREEN-INPUT     = 1.
*      MODIFY SCREEN.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 800.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'T800'.

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
    PERFORM BUILD_FIELD_CATALOG USING 'IT_DATA'.
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
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
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
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MAKTX'        ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '45',

                                  'S' 'UDATE'       ' ',
                                  ' ' 'COLTEXT'     'Ch. Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'WERKS'       ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EKGRP'       ' ',
                                  ' ' 'COLTEXT'     'Pur Grp',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PROFL'       ' ',
                                  ' ' 'COLTEXT'     'Profile',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MMSTA'       ' ',
                                  ' ' 'COLTEXT'     'Matl Status',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'FABKZ'       ' ',
                                  ' ' 'COLTEXT'     'JIT Ind',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ERSDA'       ' ',
                                  ' ' 'COLTEXT'     'Cr Date',
                                  'E' 'OUTPUTLEN'   '10',

*                                  'S' 'UDATE'       ' ',
*                                  ' ' 'COLTEXT'     'Ch. Date',
*                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'FLAG'        ' ',
                                  ' ' 'COLTEXT'     'EAI Interface',
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

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_DATA[]
               IT_SORT          = IT_SORT[].

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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
