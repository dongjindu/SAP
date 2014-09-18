************************************************************************
* Program Name      : ZIMM_PILOT_FSC
* Creation Date     : 11/05/09
* Development Request No :
* Addl Documentation:
* Description       : Send FSC to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMM_PILOT_FSC NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS, VRM.
TABLES: ZTMM_PILOT_FSC.
*DATA: IT_DATA LIKE TABLE OF ZTMM_PILOT_MATL WITH HEADER LINE.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTMM_PILOT_FSC.
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
PARAMETERS: P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM.
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
  DATA: L_ATFLV_FR TYPE AUSP-ATFLV,
  L_ATFLV_TO TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N,
        L_DATE  TYPE I,
        L_NATN(3),
*        L_NATN_NEW(1),
        L_DEALER(2),
        L_DATE_8(8),
        L_DATUM LIKE SY-DATUM,
        L_MATNR LIKE MARA-MATNR,
        L_WORDER LIKE ZTPP_WOSUM-WO_SER,
        L_NAME TYPE CABN-ATNAM.

  DATA: BEGIN OF LT_OBJEK OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATFLV LIKE AUSP-ATFLV,
        END OF LT_OBJEK.
  DATA: L_VARIABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  L_DATUM = P_DATUM - 6.
  IF P_SD = 'X' AND P_BATCH IS INITIAL.
    SELECT * INTO TABLE IT_DATA
      FROM ZTMM_PILOT_FSC
      WHERE SEQ_DATE BETWEEN L_DATUM AND P_DATUM.
  ELSE.

    L_DATUM = P_DATUM - 6.
    L_ATFLV_FR = L_NUM = L_DATUM.
    L_ATFLV_TO = L_NUM = P_DATUM.
    L_NAME = 'P_SEQUENCE_DATE'.
    SELECT DISTINCT OBJEK ATFLV INTO TABLE LT_OBJEK
    FROM AUSP AS AU
    INNER JOIN CABN AS CA
    ON AU~ATINN = CA~ATINN
    WHERE KLART = '002'
     AND AU~ATFLV BETWEEN L_ATFLV_FR AND L_ATFLV_TO
     AND CA~ATNAM = L_NAME.

    IF SY-SUBRC = 0.

      DELETE ADJACENT DUPLICATES FROM LT_OBJEK.

      L_VARIABLE-ATNAM = 'P_MODEL'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_MODEL_YEAR'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_DESTINATION_CODE'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_MI'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_OCN'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_VERSION'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_EXT_COLOR'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_INT_COLOR'.
      APPEND L_VARIABLE .
      L_VARIABLE-ATNAM = 'P_WORK_ORDER'.
      APPEND L_VARIABLE .

      LOOP AT LT_OBJEK.
        L_MATNR = LT_OBJEK-OBJEK.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  OBJECT       = L_MATNR
                  MODE         = 'R'
                  CTYPE        = '002'
             TABLES
                  VAL_TABLE    = L_VARIABLE
             EXCEPTIONS
                  NO_DATA      = 1
                  ERROR_MODE   = 2
                  ERROR_OBJECT = 3.

        IF SY-SUBRC = 0.
          LOOP AT L_VARIABLE.
            CASE L_VARIABLE-ATNAM.
              WHEN 'P_MODEL'.
                IT_DATA-MODEL = L_VARIABLE-ATWRT.
              WHEN 'P_MODEL_YEAR'.
                IT_DATA-MODEL_YEAR = L_VARIABLE-ATWRT.
              WHEN 'P_DESTINATION_CODE'.
                IT_DATA-DEST = L_VARIABLE-ATWRT.
              WHEN 'P_MI'.
                IT_DATA-MI = L_VARIABLE-ATWRT.
              WHEN 'P_OCN'.
                IT_DATA-OCN = L_VARIABLE-ATWRT.
              WHEN 'P_VERSION'.
                IT_DATA-VERSION = L_VARIABLE-ATWRT.
              WHEN 'P_EXT_COLOR'.
                IT_DATA-EXTC = L_VARIABLE-ATWRT.
              WHEN 'P_INT_COLOR'.
                IT_DATA-INTC = L_VARIABLE-ATWRT.
              WHEN 'P_WORK_ORDER'.
                L_WORDER = L_VARIABLE-ATWRT.

            ENDCASE.
          ENDLOOP.
          L_DATE = LT_OBJEK-ATFLV.
          IT_DATA-SEQ_DATE = L_DATE_8 = L_DATE.
          L_DEALER = IT_DATA-DEST+3(2).
          L_NATN = IT_DATA-DEST+0(3).

*          CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
*               EXPORTING
*                    OLD_DEALER = L_NATN_OLD
*               IMPORTING
*                    NEW_DEALER = L_NATN_NEW.

          SELECT SINGLE FSC INTO IT_DATA-FSC
            FROM ZTPP_WOSUM
            WHERE WO_SER = L_WORDER
              AND NATION = L_NATN
              AND DEALER = L_DEALER
              AND EXTC = IT_DATA-EXTC
              AND INTC = IT_DATA-INTC.

*          CONCATENATE IT_DATA-MODEL_YEAR L_NATN_NEW IT_DATA-MI
*              IT_DATA-OCN INTO IT_DATA-FSC.
          IF NOT IT_DATA-FSC IS INITIAL.
            SELECT SINGLE ERSDA INTO IT_DATA-ERSDA
            FROM MARA
            WHERE MATNR = IT_DATA-FSC.
          ENDIF.
          IT_DATA-BUKRS = 'H201'.
*          APPEND IT_DATA.
          COLLECT IT_DATA.
          CLEAR: IT_DATA.
        ENDIF.
        LOOP AT L_VARIABLE.
          CLEAR: L_VARIABLE-ATWRT.
          MODIFY L_VARIABLE.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
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

  DATA: LT_OUTPUT LIKE TABLE OF ZSMM_PILOT_FSC WITH HEADER LINE.
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO LT_OUTPUT.
    APPEND LT_OUTPUT.
  ENDLOOP.

  DESCRIBE TABLE IT_DATA LINES L_TOTREC.

  CALL FUNCTION 'Z_FMM_PILOT_FSC'
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
*  delete from ZTMM_PILOT_FSC where BUKRS = 'H201'.
  MODIFY ZTMM_PILOT_FSC FROM TABLE IT_DATA.
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

                                  'S' 'FSC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'FSC',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MODEL'        ' ',
                                  ' ' 'COLTEXT'     'Model',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'MODEL_YEAR'       ' ',
                                  ' ' 'COLTEXT'     'M Year',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'DEST'       ' ',
                                  ' ' 'COLTEXT'     'Nation',
                                  'E' 'OUTPUTLEN'   '7',

                                  'S' 'MI'          ' ',
                                  ' ' 'COLTEXT'     'MI',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'OCN'       ' ',
                                  ' ' 'COLTEXT'     'OCN',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'VERSION'       ' ',
                                  ' ' 'COLTEXT'     'Ver.',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'EXTC'       ' ',
                                  ' ' 'COLTEXT'     'Ext C',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'INTC'       ' ',
                                  ' ' 'COLTEXT'     'Int C',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SEQ_DATE'       ' ',
                                  ' ' 'COLTEXT'     'Seq Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ERSDA'       ' ',
                                  ' ' 'COLTEXT'     'Cr Date',
                                  'E' 'OUTPUTLEN'   '10',

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
