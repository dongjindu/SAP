************************************************************************
* Program Name      : ZIMM_ROH1_INVENTORY
* Creation Date     : 11/09/09
* Development Request No :
* Addl Documentation:
* Description       : Send ROH1 Inventory to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMM_ROH1_INVENTORY NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS, VRM.
TABLES: ZTMM_ROH1_INV, MARA.
*DATA: IT_DATA LIKE TABLE OF ZTMM_PILOT_MATL WITH HEADER LINE.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTMM_ROH1_INV.
DATA: END OF IT_DATA.
DATA: IT_ERROR LIKE TABLE OF IT_DATA WITH HEADER LINE.

*CONSTANTS: C_DEST(10) VALUE 'WMPM01'.
DATA: W_DEST(10).
*DATA: W_FILENAME LIKE RLGRAP-FILENAME.

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

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I,
      W_MTART LIKE MARA-MTART.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_DATE LIKE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.
SELECT-OPTIONS: S_TIME FOR SY-UZEIT NO-EXTENSION OBLIGATORY,
                S_MATKL FOR MARA-MATKL.
PARAMETERS: P_BUKRS LIKE T001-BUKRS DEFAULT 'H201' OBLIGATORY.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: P_BATCH AS CHECKBOX  USER-COMMAND CHAL.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-U01 FOR FIELD P_SD.
PARAMETERS: P_SD RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 20(8) TEXT-U12 FOR FIELD P_EO.
PARAMETERS: P_EO RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 36(15) TEXT-U13 FOR FIELD P_ES.
PARAMETERS: P_ES RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 60(11) TEXT-U14 FOR FIELD P_RP.
PARAMETERS: P_RP RADIOBUTTON GROUP GRP1 MODIF ID ABC.

SELECTION-SCREEN  END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.
PARAMETERS: P_RVER like SOMLRECI1-RECEIVER OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN_ALL.

START-OF-SELECTION.

  PERFORM GET_DATA_MSEG.

** Added by Furong on 02/09/10
*  PERFORM GET_DATA_OTHERS.
** End of change

  IF IT_DATA[] IS INITIAL.
    MESSAGE I009 WITH 'No data found'.
  ELSE.
    IF P_BATCH = 'X'.
      PERFORM SAVE_SEND_DATA.
    ELSE.
      IF P_ES = 'X' OR P_RP = 'X'.
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
FORM GET_DATA_MSEG.

  DATA: L_VARIABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.
*  DATA: L_QTY_1 LIKE EKBE-MENGE,
*        L_QTY_2 LIKE EKBE-MENGE,
  DATA: L_MATNR LIKE MSEG-MATNR,
        L_CHARG LIKE MSEG-CHARG,
         L_EBELN LIKE EKPO-EBELN,
         L_EBELP LIKE EKPO-EBELP.

  DATA: LT_MCHB LIKE TABLE OF MCHB WITH HEADER LINE.

  DATA: BEGIN OF LT_MSEG OCCURS 0,
        MATNR LIKE MSEG-MATNR,
        BUDAT LIKE MKPF-BUDAT,
        WERKS LIKE MSEG-WERKS,
        LGORT LIKE MSEG-LGORT,
        CHARG LIKE MSEG-CHARG,
        BWART LIKE MSEG-BWART,
        EBELN LIKE MSEG-EBELN,
        EBELP LIKE MSEG-EBELP,
        LIFNR LIKE MSEG-LIFNR,
*        erfmg LIKE mseg-erfmg,
        ERFMG LIKE MARD-LABST,
        END OF LT_MSEG.

  RANGES: S_BWART FOR MSEG-BWART.

  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '101'.
  APPEND   S_BWART.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '102'.
  APPEND   S_BWART.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '122'.
  APPEND   S_BWART.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '123'.
  APPEND   S_BWART.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '261'.
  APPEND   S_BWART.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '262'.
  APPEND   S_BWART.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'EQ'.
  S_BWART-LOW = '551'.
  APPEND   S_BWART.

  IF P_BUKRS = 'H201'.
    W_MTART = 'ROH1'.
  ELSEIF P_BUKRS = 'K201'.
    W_MTART = 'RAW1'.
  ENDIF.

  CASE 'X'.
    WHEN P_SD.  "AND P_BATCH IS INITIAL.
      SELECT * INTO TABLE IT_DATA
        FROM ZTMM_ROH1_INV
        WHERE BUDAT = P_DATE.

    WHEN P_RP.
      SELECT * INTO TABLE IT_DATA
        FROM ZTMM_ROH1_INV
        WHERE BUDAT = P_DATE
          AND RFLAG <> 'Z'.

      LOOP AT IT_DATA.
        CLEAR: IT_DATA-IF_DATE_CHANGE, IT_DATA-IF_TIME_CHANGE,
               IT_DATA-RFLAG, IT_DATA-MESSAGE.
        MODIFY IT_DATA.
      ENDLOOP.

    WHEN OTHERS.
*    SELECT A~MATNR BUDAT WERKS LGORT CHARG BWART
*           EBELN EBELP LIFNR ERFMG
*      INTO TABLE LT_MSEG
*      FROM MARA AS A
*      INNER JOIN MSEG AS B
*      ON A~MATNR = B~MATNR
*      INNER JOIN MKPF AS C
*      ON B~MBLNR = C~MBLNR
*      AND B~MJAHR = C~MJAHR
*      WHERE C~BUDAT = P_DATE
*       AND C~CPUTM IN S_TIME
*       AND A~MTART = W_MTART
**       AND MATKL = 'AM'
*       AND MATKL IN S_MATKL
*       AND B~BWART IN S_BWART.

      SELECT A~MATNR WERKS LGORT CHARG CLABS
        INTO CORRESPONDING FIELDS OF TABLE LT_MCHB
        FROM MARA AS A
        INNER JOIN MCHB AS B
        ON A~MATNR = B~MATNR
        WHERE A~MTART = W_MTART
          AND MATKL IN S_MATKL
          AND CLABS > 0.

      IF SY-SUBRC = 0.
** Fuorng on 05/18/12 for SAP Tuning

*        SORT LT_MCHB BY MATNR CHARG.
*        SELECT matnr cpudt as budat werks lgort charg bwart
*               ebeln ebelp lifnr erfmg
*          INTO TABLE lt_mseg
*          FROM mkpf AS a
*          INNER JOIN mseg AS b
*           ON a~mblnr = b~mblnr
*           AND a~mjahr = b~mjahr
*           FOR ALL ENTRIES IN lt_mchb
*          WHERE cpudt = p_date
*           AND a~cputm IN s_time
*           AND matnr = lt_mchb-matnr
*           AND werks = lt_mchb-werks
*           AND lgort = lt_mchb-lgort
*           AND charg = lt_mchb-charg
*           AND b~bwart IN s_bwart.

        SELECT MATNR CPUDT AS BUDAT WERKS LGORT CHARG BWART
                      EBELN EBELP LIFNR ERFMG
                 INTO TABLE LT_MSEG
                 FROM MKPF AS A
                 INNER JOIN MSEG AS B
                  ON A~MBLNR = B~MBLNR
                  AND A~MJAHR = B~MJAHR
                 WHERE CPUDT = P_DATE
                  AND A~CPUTM IN S_TIME
                  AND B~BWART IN S_BWART
    %_HINTS ORACLE 'ORDERED USE_NL (T_00 T_01) INDEX (T_00 "MKPF~Z02")'.

        SORT LT_MCHB BY MATNR WERKS LGORT CHARG.

        LOOP AT LT_MSEG.
          READ TABLE LT_MCHB WITH KEY MATNR = LT_MSEG-MATNR
                                      WERKS = LT_MSEG-WERKS
                                      LGORT = LT_MSEG-LGORT
                                      CHARG = LT_MSEG-CHARG
                                      BINARY SEARCH.
          IF SY-SUBRC NE 0.
            DELETE LT_MSEG.
          ENDIF.
        ENDLOOP.
** End on 05/18/12

        SORT LT_MSEG BY MATNR CHARG.

        LOOP AT LT_MCHB.
          MOVE-CORRESPONDING LT_MCHB TO IT_DATA.
          IT_DATA-PLANT = LT_MCHB-WERKS.
          LOOP AT LT_MSEG WHERE MATNR = LT_MCHB-MATNR
                            AND CHARG = LT_MCHB-CHARG.
            MOVE-CORRESPONDING LT_MSEG TO IT_DATA.
            CASE LT_MSEG-BWART.
              WHEN '101'.
                IT_DATA-GRQTY = IT_DATA-GRQTY + LT_MSEG-ERFMG.
              WHEN '123'.
                IT_DATA-GRQTY = IT_DATA-GRQTY + LT_MSEG-ERFMG.
              WHEN '102'.
                IT_DATA-GRQTY = IT_DATA-GRQTY - LT_MSEG-ERFMG.
              WHEN '122'.
                IT_DATA-GRQTY = IT_DATA-GRQTY - LT_MSEG-ERFMG.
              WHEN '261'.
                IT_DATA-GIQTY = IT_DATA-GIQTY + LT_MSEG-ERFMG.
              WHEN '551'.
                IT_DATA-GIQTY = IT_DATA-GIQTY + LT_MSEG-ERFMG.
              WHEN '262'.
                IT_DATA-GIQTY = IT_DATA-GIQTY - LT_MSEG-ERFMG.
            ENDCASE.
          ENDLOOP.
          APPEND IT_DATA.
          CLEAR: IT_DATA, LT_MSEG.
        ENDLOOP.

        L_VARIABLE-ATNAM = 'ZSTEEL_MATPROPERTY'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZFRONT_FINISHING_THICKNESS'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZSPEC_THICK'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZSPEC_WIDTH'.
        APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZSPEC_LENGTH'.
*      APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZKIND_OF_STEEL'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZIN_OR_OUT'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZEDGE'.
        APPEND L_VARIABLE .

        LOOP AT IT_DATA.

          CALL FUNCTION 'Z_ALL_CLASS_CHARC'   " 'Z_FPP_HANDLING_MASTER'
                EXPORTING
                     OBJECT       = IT_DATA-MATNR
*                  MODE         = 'R'
                     CTYPE        = '001'
                TABLES
                     VAL_TABLE    = L_VARIABLE
                EXCEPTIONS
                     NO_DATA      = 1
                     ERROR_MODE   = 2
                     ERROR_OBJECT = 3.

          IF SY-SUBRC = 0.
            LOOP AT L_VARIABLE.
              CASE L_VARIABLE-ATNAM.
                WHEN 'ZSTEEL_MATPROPERTY'.
                  IT_DATA-STEEL_MAT = L_VARIABLE-ATWRT.
                WHEN 'ZFRONT_FINISHING_THICKNESS'.
                  IT_DATA-COATING = L_VARIABLE-ATWRT.
                WHEN 'ZSPEC_THICK'.
                  IT_DATA-THICK = L_VARIABLE-ATWRT.
                WHEN 'ZSPEC_WIDTH'.
                  IT_DATA-WIDTH = L_VARIABLE-ATWRT.
*              WHEN 'ZSPEC_LENGTH'.
*                IT_DATA-LENGTH = L_VARIABLE-ATWRT.
                WHEN 'ZKIND_OF_STEEL'.
                  IT_DATA-KIND = L_VARIABLE-ATWRT.
              ENDCASE.
            ENDLOOP.
          ENDIF.

          IT_DATA-LENGTH = '0'.
          SELECT SINGLE STPRS INTO IT_DATA-STPRS
             FROM MBEW
             WHERE MATNR = IT_DATA-MATNR
               AND BWKEY = IT_DATA-PLANT.
*
*        SELECT SINGLE LABST INTO IT_DATA-LABST
*           FROM MARD
*           WHERE MATNR = IT_DATA-MATNR
*             AND WERKS = IT_DATA-PLANT
*             AND LGORT = IT_DATA-LGORT.

          READ TABLE LT_MCHB WITH KEY MATNR = IT_DATA-MATNR
                                      WERKS = IT_DATA-PLANT
                                      LGORT = IT_DATA-LGORT
                                      CHARG = IT_DATA-CHARG.
          IT_DATA-LABST = LT_MCHB-CLABS.

          IF IT_DATA-EBELN IS INITIAL.
            SELECT SINGLE EBELN EBELP INTO (L_EBELN, L_EBELP)
              FROM MSEG
              WHERE MATNR = IT_DATA-MATNR
                AND CHARG = IT_DATA-CHARG
                AND BWART = '101'.
            IF SY-SUBRC = 0.
              SELECT SINGLE NETPR PEINH MEINS WAERS INTO
         (IT_DATA-NETPR, IT_DATA-PEINH, IT_DATA-MEINS, IT_DATA-WAERS)
              FROM EKKO AS A
              INNER JOIN EKPO AS B
              ON A~EBELN = B~EBELN
              WHERE A~EBELN = L_EBELN
              AND B~EBELP = L_EBELP.
            ENDIF.
          ELSE.
            SELECT SINGLE NETPR PEINH MEINS WAERS INTO
         (IT_DATA-NETPR, IT_DATA-PEINH, IT_DATA-MEINS, IT_DATA-WAERS)
              FROM EKKO AS A
              INNER JOIN EKPO AS B
              ON A~EBELN = B~EBELN
              WHERE A~EBELN = IT_DATA-EBELN
                AND B~EBELP = IT_DATA-EBELP.
          ENDIF.

*        SELECT SUM( MENGE ) INTO L_QTY_1
*          FROM MSEG
*          WHERE WERKS = IT_DATA-PLANT
*            AND MATNR = IT_DATA-MATNR
*            AND CHARG = IT_DATA-CHARG
*            AND BWART IN ('261', '551').
*
*        SELECT SUM( MENGE ) INTO L_QTY_2
*          FROM MSEG
*          WHERE WERKS = IT_DATA-PLANT
*            AND MATNR = IT_DATA-MATNR
*            AND CHARG = IT_DATA-CHARG
*            AND BWART IN ('262').
*
*        IT_DATA-GIQTY = L_QTY_1 - L_QTY_2.

          MODIFY IT_DATA.

          LOOP AT L_VARIABLE.
            CLEAR: L_VARIABLE-ATWRT.
            MODIFY L_VARIABLE.
          ENDLOOP.
          CLEAR: IT_DATA.
        ENDLOOP.
      ENDIF.
  ENDCASE.

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
        L_SREC TYPE I,
        L_FREC TYPE I,
        L_MSGTXT(60).

*  DATA: LT_OUTPUT LIKE TABLE OF ZTMM_ROH1_inv WITH HEADER LINE.
*
*  LOOP AT IT_DATA.
*    MOVE-CORRESPONDING IT_DATA TO LT_OUTPUT.
*    APPEND LT_OUTPUT.
*  ENDLOOP.

  IF P_BUKRS = 'H201'.
    W_DEST = 'WMPM01'.
  ELSE.

  ENDIF.

  DESCRIBE TABLE IT_DATA LINES L_TOTREC.

  CALL FUNCTION 'Z_FMM_ROH1_INV'
    DESTINATION W_DEST
    IMPORTING
      FLAG                  = L_RESULT
    TABLES
      I_PILOT_MATL          = IT_DATA
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.
*  IF SY-SUBRC = 0.
*  IF L_RESULT = 'S' OR L_RESULT = 's'.
*    WRITE: / 'Total record number(s) are : ', L_TOTREC,
*           'were sent successfully'.
*    LOOP AT IT_DATA.
*      IT_DATA-BUDAT = SY-DATUM.
*      IT_DATA-IF_DATE = SY-DATUM.
*      IT_DATA-IF_TIME = SY-UZEIT.
*      IT_DATA-FLAG = 'S'.
*      MODIFY IT_DATA.
*    ENDLOOP.
*
*  ELSE.
*    WRITE: / 'EAI Failed, ', l_msgtxt.
*    LOOP AT IT_DATA.
*      IT_DATA-BUDAT = SY-DATUM.
*      IT_DATA-IF_DATE = SY-DATUM.
*      IT_DATA-IF_TIME = SY-UZEIT.
*      IT_DATA-FLAG = 'E'.
*      MODIFY IT_DATA.
*    ENDLOOP.
*  ENDIF.
  IF L_RESULT = 'S' OR L_RESULT = 's'.

    LOOP AT IT_DATA.
      IF IT_DATA-RFLAG = 'F'.
        L_FREC = L_FREC + 1.
        IT_ERROR = IT_DATA.
        APPEND IT_ERROR.
      ELSE.
        L_SREC = L_SREC + 1.
      ENDIF.
      IF P_RP = 'X'.
        IT_DATA-IF_DATE_CHANGE = SY-DATUM.
        IT_DATA-IF_TIME_CHANGE = SY-UZEIT.
      ELSE.
*        it_data-budat = sy-datum.
        IT_DATA-BUDAT = P_DATE.
        IT_DATA-IF_DATE = SY-DATUM.
        IT_DATA-IF_TIME = SY-UZEIT.
      ENDIF.
      IT_DATA-FLAG = 'S'.
      MODIFY IT_DATA.
    ENDLOOP.
    WRITE: / 'Total records are : ', L_TOTREC.
    WRITE: / 'Successfully sent records are : ', L_SREC.
    WRITE: / 'Unsuccessfully sent records are : ', L_FREC.
    IF L_TOTREC = L_SREC.
      IT_DATA-FLAG = 'S'.
      MODIFY TABLE IT_DATA TRANSPORTING FLAG.
    ELSE.
      IT_DATA-FLAG = 'E'.
      MODIFY TABLE IT_DATA TRANSPORTING FLAG.
    ENDIF.
  ELSE.
    WRITE: / 'EAI Failed, Total records are: ', L_TOTREC.
    LOOP AT IT_DATA.
      IF P_RP = 'X'.
        IT_DATA-IF_DATE_CHANGE = SY-DATUM.
        IT_DATA-IF_TIME_CHANGE = SY-UZEIT.
      ELSE.
*        it_data-budat = sy-datum.
        IT_DATA-BUDAT = P_DATE.
        IT_DATA-IF_DATE = SY-DATUM.
        IT_DATA-IF_TIME = SY-UZEIT.
      ENDIF.
      IT_DATA-FLAG = 'E'.
      MODIFY IT_DATA.
    ENDLOOP.
    IT_ERROR[] = IT_DATA[].
  ENDIF.

  MODIFY ZTMM_ROH1_INV FROM TABLE IT_DATA.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  IF NOT IT_ERROR[] IS INITIAL.
    PERFORM SEND_EMAIL.
  ENDIF.

ENDFORM.                    "save_send_data
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_ALL.

  LOOP AT SCREEN.
    IF P_BATCH = 'X' AND SCREEN-GROUP1 EQ 'ABC'.
      SCREEN-INVISIBLE = 1.
      SCREEN-ACTIVE    = 0.
      SCREEN-INPUT     = 0.
      MODIFY SCREEN.
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

                                  'S' 'PLANT'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'CHARG'       ' ',
                                  ' ' 'COLTEXT'     'Batch',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'LGORT'       ' ',
                                  ' ' 'COLTEXT'     'Storage',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'BUDAT'       ' ',
                                  ' ' 'COLTEXT'     'Post Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'STEEL_MAT'       ' ',
                                  ' ' 'COLTEXT'     'Steel Material',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'COATING'       ' ',
                                  ' ' 'COLTEXT'     'Coating',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'THICK'       ' ',
                                  ' ' 'COLTEXT'     'Thickness',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'WIDTH'       ' ',
                                  ' ' 'COLTEXT'     'Width',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'LENGTH'       ' ',
                                  ' ' 'COLTEXT'     'Length',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'KIND'       ' ',
                                  ' ' 'COLTEXT'     'Kind of Steel',
                                  'E' 'OUTPUTLEN'   '30',

                                  'S' 'GRQTY'       ' ',
                                  ' ' 'COLTEXT'     'GR QTY',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'GIQTY'       ' ',
                                  ' ' 'COLTEXT'     'GI QTY',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'STPRS'       ' ',
                                  ' ' 'COLTEXT'     'Std Price',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'NETPR'       ' ',
                                  ' ' 'COLTEXT'     'Net Price',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'LABST'       ' ',
                                  ' ' 'COLTEXT'     'Stock',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'WAERS'       ' ',
                                  ' ' 'COLTEXT'     'Currency',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'PEINH'       ' ',
                                  ' ' 'COLTEXT'     'PR Unit',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'MEINS'       ' ',
                                  ' ' 'COLTEXT'     'UOM',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'FLAG'        ' ',
                                  ' ' 'COLTEXT'     'EAI',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'RFLAG'        ' ',
                                  ' ' 'COLTEXT'     'HMC',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MESSAGE'       ' ',
                                  ' ' 'COLTEXT'     'HMC Message',
                                  'E' 'OUTPUTLEN'   '80'.

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
      IT_OUTTAB            = IT_DATA[]
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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      -->P_<FS_ZTMM_6019_01>_UDATE  text
*      <--P_<FS_ZTMM_6019_01>_LIFNR  text
*----------------------------------------------------------------------*
FORM GET_LIFNR_FR_SOURCELIST
               USING    VALUE(IM_MATNR)
                        VALUE(IM_DATE)
               CHANGING VALUE(EX_LIFNR).

  SELECT SINGLE LIFNR
   INTO EX_LIFNR
   FROM EORD
   WHERE MATNR = IM_MATNR AND
         VDATU =< IM_DATE AND
            "Source list record valid from
         BDATU => IM_DATE.
  "Source list record valid to
ENDFORM.                    "get_lifnr_fr_sourcelist
*&---------------------------------------------------------------------*
*&      Form  get_inforecord_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      -->P_<FS_ZTMM_6019_01>_LIFNR  text
*      <--P_<FS_ZTMM_6019_01>_NETPR  text
*      <--P_<FS_ZTMM_6019_01>_EFFPR  text
*      <--P_<FS_ZTMM_6019_01>_WAERS  text
*      <--P_<FS_ZTMM_6019_01>_BPUMZ  text
*      <--P_<FS_ZTMM_6019_01>_BPUMN  text
*----------------------------------------------------------------------*
FORM GET_INFORECORD_DATA USING VALUE(IM_MATNR)
                               VALUE(IM_LIFNR)
                      CHANGING VALUE(EX_KBETR)
                               VALUE(EX_KONWA)
                               VALUE(EX_KPEIN)
                               VALUE(EX_KMEIN).

  DATA: BEGIN OF LT_DATAB OCCURS 0,
          DATAB LIKE A018-DATAB,
          KNUMH LIKE KONP-KNUMH,
          END OF LT_DATAB.
  DATA: L_KNUMH LIKE LT_DATAB-KNUMH.

  SELECT DATAB KNUMH INTO TABLE LT_DATAB
    FROM A018
    WHERE KSCHL = 'PB00'
      AND MATNR = IM_MATNR
      AND LIFNR = IM_LIFNR.

  SORT LT_DATAB BY DATAB DESCENDING.
  READ TABLE LT_DATAB INDEX 1.
  L_KNUMH = LT_DATAB-KNUMH.

  SELECT SINGLE KBETR KONWA KPEIN KMEIN
    INTO (EX_KBETR, EX_KONWA, EX_KPEIN, EX_KMEIN)
    FROM KONP
    WHERE KNUMH = L_KNUMH
     AND KSCHL = 'PB00'.
ENDFORM.                    "get_inforecord_data
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_OTHERS.

  DATA: BEGIN OF LT_MARA OCCURS 0,
         MATNR LIKE MARD-MATNR,
         WERKS LIKE MARD-WERKS,
         LABST LIKE MARD-LABST,
       END OF LT_MARA.
  DATA: L_VARIABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  SELECT A~MATNR WERKS SUM( LABST ) AS LABST
    INTO TABLE LT_MARA
    FROM MARA AS A
    INNER JOIN MARD AS B
     ON A~MATNR = B~MATNR
     WHERE A~MTART = 'ROH1'
      AND WERKS = 'P001'
     AND MATKL IN S_MATKL
     AND A~LVORM = ' '
     AND B~LVORM = ' '
     AND LABST > 0
     GROUP BY A~MATNR WERKS.


  L_VARIABLE-ATNAM = 'ZSTEEL_MATPROPERTY'.
  APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'ZFRONT_FINISHING_THICKNESS'.
  APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'ZSPEC_THICK'.
  APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'ZSPEC_WIDTH'.
  APPEND L_VARIABLE .
*  L_VARIABLE-ATNAM = 'ZSPEC_LENGTH'.
*  APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'ZKIND_OF_STEEL'.
  APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'ZIN_OR_OUT'.
  APPEND L_VARIABLE .
  L_VARIABLE-ATNAM = 'ZEDGE'.
  APPEND L_VARIABLE .

  LOOP AT LT_MARA.
    CLEAR: IT_DATA.
    READ TABLE IT_DATA WITH KEY MATNR = LT_MARA-MATNR.
    IF SY-SUBRC = 0.
    ELSE.
      IT_DATA-MATNR = LT_MARA-MATNR.
      IT_DATA-PLANT = 'P001'.
      IT_DATA-CHARG = 'NO GR/GI'.
      IT_DATA-BUDAT = P_DATE.
      IT_DATA-LABST = LT_MARA-LABST.

      CALL FUNCTION 'Z_ALL_CLASS_CHARC'   " 'Z_FPP_HANDLING_MASTER'
              EXPORTING
                   OBJECT       = LT_MARA-MATNR
*                  MODE         = 'R'
                   CTYPE        = '001'
              TABLES
                   VAL_TABLE    = L_VARIABLE
              EXCEPTIONS
                   NO_DATA      = 1
                   ERROR_MODE   = 2
                   ERROR_OBJECT = 3.

      IF SY-SUBRC = 0.
        LOOP AT L_VARIABLE.
          CASE L_VARIABLE-ATNAM.
            WHEN 'ZSTEEL_MATPROPERTY'.
              IT_DATA-STEEL_MAT = L_VARIABLE-ATWRT.
            WHEN 'ZFRONT_FINISHING_THICKNESS'.
              IT_DATA-COATING = L_VARIABLE-ATWRT.
            WHEN 'ZSPEC_THICK'.
              IT_DATA-THICK = L_VARIABLE-ATWRT.
            WHEN 'ZSPEC_WIDTH'.
              IT_DATA-WIDTH = L_VARIABLE-ATWRT.
*            WHEN 'ZSPEC_LENGTH'.
*              IT_DATA-LENGTH = L_VARIABLE-ATWRT.
            WHEN 'ZKIND_OF_STEEL'.
              IT_DATA-KIND = L_VARIABLE-ATWRT.
          ENDCASE.
        ENDLOOP.
      ENDIF.
      SELECT SINGLE STPRS INTO IT_DATA-STPRS
                FROM MBEW
                WHERE MATNR = IT_DATA-MATNR
                  AND BWKEY = IT_DATA-PLANT.

      APPEND IT_DATA.
      LOOP AT L_VARIABLE.
        CLEAR: L_VARIABLE-ATWRT.
        MODIFY L_VARIABLE.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_DATA_others
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EMAIL.
  DATA: LT_BODY LIKE TABLE OF SOLISTI1 WITH HEADER LINE.

  DATA: L_SUBJECT TYPE P15_TEXT150,
        L_P_REC_TYPE  LIKE  SOMLRECI1-REC_TYPE.

  MOVE 'Following items with EAI errors:' TO LT_BODY.
  APPEND LT_BODY.
  CLEAR: LT_BODY.
  MOVE '================================' TO LT_BODY.
  APPEND LT_BODY.
  CLEAR: LT_BODY.

  MOVE: 'Material No' TO LT_BODY+0(20),
        'Batch No' TO LT_BODY+20(15).

  APPEND LT_BODY.
  CLEAR: LT_BODY.

  MOVE: '--------------------' TO  LT_BODY+0(20),
        '---------------' TO  LT_BODY+20(15).
  APPEND LT_BODY.
  CLEAR: LT_BODY.

  LOOP AT IT_DATA.
    MOVE: IT_DATA-MATNR TO LT_BODY+0(20),
          IT_DATA-CHARG TO LT_BODY+20(15).
    APPEND LT_BODY.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      P_SUBJECT  = 'V-Steel interface error - BLANK'
      P_REC_TYPE = 'C'
      P_RECEIVER = P_RVER
    TABLES
      PT_BODY    = LT_BODY.

ENDFORM.                    " SEND_EMAIL
