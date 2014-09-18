************************************************************************
* Program Name      : ZIMM_ROH1_BLANK
* Creation Date     : 04/30/10
* Development Request No :
* Addl Documentation:
* Description       : Send ROH1 Blanks to HMC
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZIMM_ROH1_BLANK NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS, VRM.
TABLES: ZTMM_ROH1_BLANK, MARA, T001.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTMM_ROH1_BLANK.
DATA: END OF IT_DATA.
DATA: IT_ERROR LIKE TABLE OF IT_DATA WITH HEADER LINE.

DATA: W_DEST(10).
*DATA: W_FILENAME LIKE RLGRAP-FILENAME.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDNAME.  "IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE I,
      W_MTART LIKE MARA-MTART.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM DEFAULT SY-DATUM.

*                S_MATKL FOR MARA-MATKL.
PARAMETERS: P_BUKRS LIKE T001-BUKRS DEFAULT 'H201' OBLIGATORY.
*PARAMETERS: p_rver LIKE somlreci1-receiver OBLIGATORY.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: P_BATCH AS CHECKBOX  USER-COMMAND CHAL.

SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-U01 FOR FIELD P_SD.
PARAMETERS: P_SD RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 20(8) TEXT-U12 FOR FIELD P_EO.
PARAMETERS: P_EO RADIOBUTTON GROUP GRP1 MODIF ID ABC.
SELECTION-SCREEN COMMENT 38(14) TEXT-U13 FOR FIELD P_ES.
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

  PERFORM GET_DATA.
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
FORM GET_DATA.

  DATA: L_VARIABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        L_INDEX LIKE SY-TABIX,
        L_QTY LIKE MSEG-MENGE.

  DATA: BEGIN OF IT_MSEG OCCURS 0,
    MATNR LIKE MSEG-MATNR,
    AUFNR LIKE MSEG-AUFNR,
    MBLNR LIKE MSEG-MBLNR,
    MENGE LIKE MSEG-MENGE,
    CHARG LIKE MSEG-CHARG,
    MEINS LIKE MSEG-MEINS,
    ZBUDAT LIKE MSEG-ZBUDAT,
    NTGEW LIKE MARA-NTGEW,
    ZEILE LIKE MSEG-ZEILE,
    END OF IT_MSEG.

*  DATA: BEGIN OF IT_TEMP OCCURS 0.
*          INCLUDE STRUCTURE ZTMM_ROH1_BLANK.
*  DATA: PPRDNO LIKE ZTPPPR-PPRDNO,
*  END OF IT_TEMP.

  DATA: IT_TEMP LIKE TABLE OF ZTMM_ROH1_BLANK WITH HEADER LINE.

  IF P_BUKRS = 'H201'.
    W_MTART = 'ROH1'.
  ELSEIF P_BUKRS = 'K201'.
    W_MTART = 'RAW1'.
  ENDIF.

  CASE 'X'.
    WHEN P_SD.  " =  AND p_batch IS INITIAL.
      SELECT * INTO TABLE IT_DATA
        FROM ZTMM_ROH1_BLANK
        WHERE ERSDA IN S_DATUM.

    WHEN P_RP.
      SELECT * INTO TABLE IT_DATA
        FROM ZTMM_ROH1_BLANK
        WHERE ERSDA IN S_DATUM
          AND RFLAG <> 'Z'.

      LOOP AT IT_DATA.
        CLEAR: IT_DATA-IF_DATE_CHANGE, IT_DATA-IF_TIME_CHANGE,
               IT_DATA-RFLAG, IT_DATA-MESSAGE.
        MODIFY IT_DATA.
      ENDLOOP.

    WHEN OTHERS.

** Changed on 03/10/11
*      SELECT PRS_BLK_MCOLN PRS_BLK_COLQ
*        PRS_BLK_COLPF PRS_BLK_COLT PRS_BLK_COLW PRS_BLK_COLL
**      PRS_BLK_CUSG1
*        PNLNO PPRDNO
*        INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
*        FROM ZTPPPR AS A
*        INNER JOIN ZTPPPS_BLK AS B
*        ON A~PNLNO = B~MATNR
*        WHERE PRPID = 'R03'.
**        AND RDATE IN S_DATUM.

      SELECT IDNRK AS PRS_BLK_MCOLN   " PRS_BLK_COLQ
*        PRS_BLK_COLPF PRS_BLK_COLT PRS_BLK_COLW PRS_BLK_COLL
*      PRS_BLK_CUSG1
             PNLNO PPRDNO
             INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
             FROM ZTPPPR AS A
             INNER JOIN MAST AS B
             ON A~PNLNO = B~MATNR
             INNER JOIN STPO AS C
              ON B~STLNR = C~STLNR
             WHERE PRPID = 'R03'
               AND RDATE IN S_DATUM
               AND B~WERKS = 'P001'
               AND B~STLAN = '1'
               AND C~STLTY IN ('M', 'E', 'S'). "TuningProject05.11.2012
** End of change

      IF SY-SUBRC = 0.

        SORT IT_TEMP BY PRS_BLK_MCOLN PNLNO PPRDNO.
        DELETE ADJACENT DUPLICATES FROM IT_TEMP
            COMPARING PRS_BLK_MCOLN PNLNO PPRDNO.

        SELECT A~MATNR AUFNR A~MBLNR MENGE CHARG A~MEINS
                CPUDT AS ZBUDAT NTGEW ZEILE
                                    INTO TABLE IT_MSEG
                                    FROM MSEG AS A
                                    INNER JOIN MKPF AS C
                                    ON A~MBLNR = C~MBLNR
                                    INNER JOIN MARA AS B
                                    ON A~MATNR = B~MATNR
                                    FOR ALL ENTRIES IN IT_TEMP
                                    WHERE WERKS = 'P001'
                                      AND BWART = '261'
*                                      AND CPUDT IN S_DATUM
*        AND MATNR = IT_TEMP-PRS_BLK_MCOLN
                                      AND AUFNR = IT_TEMP-PPRDNO.

        SORT IT_MSEG BY AUFNR MATNR.
        LOOP AT IT_TEMP.
          MOVE-CORRESPONDING IT_TEMP TO IT_DATA.
          LOOP AT IT_MSEG WHERE AUFNR = IT_TEMP-PPRDNO
                           AND  MATNR = IT_TEMP-PRS_BLK_MCOLN.
            IT_DATA-MBLNR = IT_MSEG-MBLNR.
            CONCATENATE IT_DATA-MBLNR IT_MSEG-ZEILE INTO IT_DATA-MBLNR.
*        L_QTY = IT_MSEG-MENGE / IT_DATA-PRS_BLK_CUSG1.
            IF IT_MSEG-NTGEW IS INITIAL.
              L_QTY = 1.
            ELSE.
              L_QTY = IT_MSEG-MENGE / IT_MSEG-NTGEW.
            ENDIF.
            IF L_QTY > 50.
              IT_DATA-NTGEW = IT_MSEG-MENGE.
            ELSE.
              IT_DATA-PSQTY = IT_MSEG-MENGE.
            ENDIF.
*            IT_DATA-PRS_BLK_MCOLN = IT_MSEG-MATNR.
            IT_DATA-CHARG = IT_MSEG-CHARG.
            IT_DATA-MEINS = IT_MSEG-MEINS.
            IT_DATA-ZBUDAT = IT_MSEG-ZBUDAT.
            APPEND IT_DATA.
            CLEAR: L_QTY, IT_DATA-NTGEW, IT_DATA-PSQTY.
          ENDLOOP.
          CLEAR: IT_DATA, IT_TEMP.
        ENDLOOP.

        SORT IT_DATA BY MBLNR CHARG PRS_BLK_MCOLN.

** Change on 03/10/11
*        LOOP AT IT_DATA.
*          L_INDEX = SY-TABIX.
**      IT_DATA-PSQTY = IT_DATA-PSQTY * IT_DATA-PRS_BLK_CUSG1.
**      IT_DATA-NTGEW = IT_DATA-NTGEW * IT_DATA-PRS_BLK_CUSG1.
*          CASE P_BUKRS.
*            WHEN 'H201'.
*              IT_DATA-PLANT = 'P001'.
*            WHEN 'K201'.
*              IT_DATA-PLANT = 'KVA1'.
*          ENDCASE.
*          IT_DATA-ERSDA = SY-DATUM.
**      IT_DATA-IF_TIME = SY-UZEIT.
*          MODIFY IT_DATA INDEX L_INDEX.
*        ENDLOOP.

        L_VARIABLE-ATNAM = 'ZSTEEL_MATPROPERTY'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZFRONT_FINISHING_THICKNESS'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZSPEC_THICK'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZSPEC_WIDTH'.
        APPEND L_VARIABLE .
        L_VARIABLE-ATNAM = 'ZSPEC_LENGTH'.
        APPEND L_VARIABLE .

        LOOP AT IT_DATA.
          L_INDEX = SY-TABIX.
          LOOP AT L_VARIABLE.
            CLEAR: L_VARIABLE-ATWRT.
            MODIFY L_VARIABLE.
          ENDLOOP.

          CALL FUNCTION 'Z_ALL_CLASS_CHARC'
                EXPORTING
                     OBJECT       = IT_DATA-PRS_BLK_MCOLN
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
                  IT_DATA-PRS_BLK_COLQ = L_VARIABLE-ATWRT.
                WHEN 'ZFRONT_FINISHING_THICKNESS'.
                  IT_DATA-PRS_BLK_COLPF = L_VARIABLE-ATWRT.
                WHEN 'ZSPEC_THICK'.
                  IT_DATA-PRS_BLK_COLT = L_VARIABLE-ATWRT.
                WHEN 'ZSPEC_WIDTH'.
                  IT_DATA-PRS_BLK_COLW = L_VARIABLE-ATWRT.
                WHEN 'ZSPEC_LENGTH'.
                  IT_DATA-PRS_BLK_COLL = L_VARIABLE-ATWRT.
              ENDCASE.
            ENDLOOP.
          ENDIF.

          CASE P_BUKRS.
            WHEN 'H201'.
              IT_DATA-PLANT = 'P001'.
            WHEN 'K201'.
              IT_DATA-PLANT = 'KVA1'.
          ENDCASE.
          IT_DATA-ERSDA = SY-DATUM.
          MODIFY IT_DATA INDEX L_INDEX.
        ENDLOOP.

** End of change on 03/10/11

*    SELECT MATNR ERSDA
*      INTO CORRESPONDING FIELDS OF TABLE IT_DATA
*      FROM MARA AS A
*      WHERE MTART = W_MTART
**        AND MATKL = 'AM'
*        AND MATKL IN S_MATKL
*        AND MSTAE = '12'
*        AND ERSDA IN S_DATUM.
*    IF SY-SUBRC = 0.
*      L_VARIABLE-ATNAM = 'ZSTEEL_MATPROPERTY'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZFRONT_FINISHING_THICKNESS'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZSPEC_THICK'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZSPEC_WIDTH'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZSPEC_LENGTH'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZKIND_OF_STEEL'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZIN_OR_OUT'.
*      APPEND L_VARIABLE .
*      L_VARIABLE-ATNAM = 'ZEDGE'.
*      APPEND L_VARIABLE .
*
*      LOOP AT IT_DATA.
*        LOOP AT L_VARIABLE.
*          CLEAR: L_VARIABLE-ATWRT.
*          MODIFY        L_VARIABLE.
*        ENDLOOP.
*
*        CALL FUNCTION 'Z_ALL_CLASS_CHARC'   " 'Z_FPP_HANDLING_MASTER'
*             EXPORTING
*                  OBJECT       = IT_DATA-MATNR
**                  MODE         = 'R'
*                  CTYPE        = '001'
*             TABLES
*                  VAL_TABLE    = L_VARIABLE
*             EXCEPTIONS
*                  NO_DATA      = 1
*                  ERROR_MODE   = 2
*                  ERROR_OBJECT = 3.
*
*        IF SY-SUBRC = 0.
*          LOOP AT L_VARIABLE.
*            CASE L_VARIABLE-ATNAM.
*              WHEN 'ZSTEEL_MATPROPERTY'.
*                IT_DATA-STEEL_MAT = L_VARIABLE-ATWRT.
*              WHEN 'ZFRONT_FINISHING_THICKNESS'.
*                IT_DATA-COATING = L_VARIABLE-ATWRT.
*              WHEN 'ZSPEC_THICK'.
*                IT_DATA-THICK = L_VARIABLE-ATWRT.
*              WHEN 'ZSPEC_WIDTH'.
*                IT_DATA-WIDTH = L_VARIABLE-ATWRT.
*              WHEN 'ZSPEC_LENGTH'.
*                IT_DATA-LENGTH = L_VARIABLE-ATWRT.
*              WHEN 'ZKIND_OF_STEEL'.
*                IT_DATA-KIND = L_VARIABLE-ATWRT.
*              WHEN 'ZIN_OR_OUT'.
*                IT_DATA-IN_OUT = L_VARIABLE-ATWRT.
*              WHEN 'ZEDGE'.
*                IT_DATA-EDGE = L_VARIABLE-ATWRT.
*            ENDCASE.
*          ENDLOOP.
*          CASE P_BUKRS.
*            WHEN 'H201'.
*              IT_DATA-PLANT = 'P001'.
*            WHEN 'K201'.
*              IT_DATA-PLANT = 'KVA1'.
*          ENDCASE.
*          MODIFY IT_DATA.
*        ENDIF.
*
*      ENDLOOP.
*    ENDIF.
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

  IF P_BUKRS = 'H201'.
    W_DEST = 'WMPM01'.
  ELSE.

  ENDIF.

  DESCRIBE TABLE IT_DATA LINES L_TOTREC.

  CALL FUNCTION 'Z_FMM_ROH1_BLANK'
     DESTINATION W_DEST
     IMPORTING
       FLAG          = L_RESULT
     TABLES
       I_PILOT_BLANK  = IT_DATA
     EXCEPTIONS
            COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
            SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

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
        IT_DATA-ERSDA = SY-DATUM.
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
        IT_DATA-IF_DATE = SY-DATUM.
        IT_DATA-IF_TIME = SY-UZEIT.
      ENDIF.
      IT_DATA-FLAG = 'E'.
      MODIFY IT_DATA.
    ENDLOOP.
    IT_ERROR[] = IT_DATA[].
  ENDIF.

*  DELETE FROM ZTMM_PILOT_MATL WHERE ERSDA IN S_DATUM.
*  INSERT ZTMM_PILOT_MATL FROM TABLE IT_DATA.
  MODIFY ZTMM_ROH1_BLANK FROM TABLE IT_DATA.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  IF NOT IT_ERROR[] IS INITIAL.
    PERFORM SEND_EMAIL.
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
*
*                                  'S' 'MATNR'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  ' ' 'COLTEXT'     'Material',
*                                  'E' 'OUTPUTLEN'   '18',

*
                                  'S' 'MBLNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'MatL Doc',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PLANT'       ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'CHARG'       ' ',
                                  ' ' 'COLTEXT'     'Batch',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PRS_BLK_MCOLN'       ' ',
                                  ' ' 'COLTEXT'     'ROH1 Matl',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'PRS_BLK_COLQ'       ' ',
                                  ' ' 'COLTEXT'     'Quatity',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'PRS_BLK_COLPF'       ' ',
                                  ' ' 'COLTEXT'     'Coating',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'PRS_BLK_COLT'       ' ',
                                  ' ' 'COLTEXT'     'Thickness',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PRS_BLK_COLW'       ' ',
                                  ' ' 'COLTEXT'     'Width',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'PRS_BLK_COLL'       ' ',
                                  ' ' 'COLTEXT'     'Length',
                                  'E' 'OUTPUTLEN'   '5',


                                  'S' 'PNLNO'       ' ',
                                  ' ' 'COLTEXT'     'Matl (Blank)',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'NTGEW'       ' ',
                                  ' ' 'COLTEXT'     'Net Weight',
                                  'E' 'OUTPUTLEN'   '13',

                                  'S' 'PSQTY'       ' ',
                                  ' ' 'COLTEXT'     'Scrap QTY',
                                  'E' 'OUTPUTLEN'   '13',


                                  'S' 'ZBUDAT'       ' ',
                                  ' ' 'COLTEXT'     'Doc Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'COLTEXT'     'UOM',
                                  'E' 'OUTPUTLEN'   '13',


                                  'S' 'ERSDA'       ' ',
                                  ' ' 'COLTEXT'     'Cr Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'FLAG'        ' ',
                                  ' ' 'COLTEXT'     'EAI',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'RFLAG'        ' ',
                                  ' ' 'COLTEXT'     'HMC',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MESSAGE'       ' ',
                                  ' ' 'COLTEXT'     'HMC Message',
                                  'E' 'OUTPUTLEN'   '80'.


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
        L_P_REC_TYPE  LIKE  SOMLRECI1-REC_TYPE,
        P_RVER  LIKE  SOMLRECI1-RECEIVER.

  MOVE 'Following items with EAI errors:' TO LT_BODY.
  APPEND LT_BODY.
  CLEAR: LT_BODY.
  MOVE '================================' TO LT_BODY.
  APPEND LT_BODY.
  CLEAR: LT_BODY.

  MOVE: 'Material Doc' TO LT_BODY+0(15),
        'Batch No' TO LT_BODY+15(11),
       'Main Coil #' TO  LT_BODY+26(19).
  APPEND LT_BODY.
  CLEAR: LT_BODY.

  MOVE: '-------------------' TO  LT_BODY+0(15),
        '-----------' TO  LT_BODY+15(11),
        '-------------------' TO  LT_BODY+26(19).
  APPEND LT_BODY.
  CLEAR: LT_BODY.

  LOOP AT IT_DATA.
    CONCATENATE IT_DATA-MBLNR IT_DATA-CHARG IT_DATA-PRS_BLK_MCOLN
    INTO LT_BODY SEPARATED BY SPACE.
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
