************************************************************************
* Program Name      : ZPP_PILOT_VEHICLE
* Author            : Haseeb Mohammad
* Creation Date     : 2008.12.19.
* Specifications By : Daniel Kim
* ABAP Guidence     : IG Moon
* Version           : 1.0
* Development Request No :UD1K945205
* Addl Documentation:
* Description       : Transfer of Production Spec and Prod Data From PP
*                     to Pilot DB
*Modificaication Logs
* Date       Developer    RequestNo    Description
************************************************************************

REPORT ZPP_PILOT_VEHICLE NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID zdpp.

************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES:
        ztppvs_pilot, ztppvp_pilot, sscrfields.


DATA:   BEGIN OF it_spec1 OCCURS 0 ,
          MANDT TYPE ZTPPVS_PILOT-MANDT,
          FLG TYPE ZTPPVS_PILOT-FLG,
          MATNR TYPE ZTPPVS_PILOT-MATNR,
          BMDL TYPE ZTPPVS_PILOT-BMDL,
          OCNN TYPE ZTPPVS_PILOT-OCNN,
          PVER TYPE ZTPPVS_PILOT-PVER,
          T219_019 TYPE ZTPPVS_PILOT-T219_019,
          T219_002 TYPE ZTPPVS_PILOT-T219_002,
          T219_004 TYPE ZTPPVS_PILOT-T219_004,
          T219_003 TYPE ZTPPVS_PILOT-T219_003,
          T219_005 TYPE ZTPPVS_PILOT-T219_005,
          PACK     TYPE ZTPPVS_PILOT-PACK,
          T219_219 TYPE ZTPPVS_PILOT-T219_219,
        END   OF  it_spec1,

        it_ztppvs    LIKE ztppvs_pilot OCCURS 0 WITH HEADER LINE,
        it_Spec_Excl LIKE ztppvs_pilot OCCURS 0 WITH HEADER LINE,
        it_spec2 LIKE ZSPPVS_219_PILOT OCCURS 0 WITH HEADER LINE,
        it_spec3 LIKE ZSPPVS_ALCU_PILOT OCCURS 0 WITH HEADER LINE,
        it_spec4 LIKE ZSPPVS_ALCC_PILOT OCCURS 0 WITH HEADER LINE,
        it_zsppTECH  LIKE TABLE OF zsppTECH  WITH HEADER LINE,
        it_ztppvp    LIKE ztppvp_pilot OCCURS 0 WITH HEADER LINE,
        it_Plan_Excl LIKE ztppvp_pilot OCCURS 0 WITH HEADER LINE,
        l_msgtxt(100),
        COUNT TYPE i VALUE 0,
        COUNTER TYPE I VALUE 0,
        Z_RES(1), Z_MSG(255), ZEDAT LIKE SY-DATUM, ZETIM LIKE SY-UZEIT.


CONSTANTS: c_mode    VALUE  'A',
           c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination


************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE UPLOAD.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-001.

PARAMETERS P_CHK1 AS CHECKBOX USER-COMMAND uco1 .

POSITION 8.
PARAMETERS:
  S_FILE1  LIKE RLGRAP-FILENAME DEFAULT '' MODIF ID GR1,
  S_FTY1 LIKE RLGRAP-FILETYPE DEFAULT 'DAT' MODIF ID GR1,
  S_FILE2  LIKE RLGRAP-FILENAME DEFAULT '' MODIF ID GR1,
  S_FTY2 LIKE RLGRAP-FILETYPE DEFAULT 'DAT' MODIF ID GR1 ,
  S_FILE3  LIKE RLGRAP-FILENAME DEFAULT '' MODIF ID GR1,
  S_FTY3 LIKE RLGRAP-FILETYPE DEFAULT 'DAT' MODIF ID GR1,
  S_FILE4  LIKE RLGRAP-FILENAME DEFAULT '' MODIF ID GR1,
  S_FTY4 LIKE RLGRAP-FILETYPE DEFAULT 'DAT' MODIF ID GR1.
SELECTION-SCREEN END OF BLOCK b4.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS P_CHK2 AS CHECKBOX  USER-COMMAND uco2.
PARAMETERS :
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT '' MODIF ID GR2,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT' MODIF ID GR2.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
POSITION 8.

PARAMETERS:  c_VPDS AS CHECKBOX DEFAULT '' MODIF ID GR1.
PARAMETERS : c_VPLO AS CHECKBOX DEFAULT '' MODIF ID GR2.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'UCO1' OR 'UCO2'.
      PERFORM modify_screen.
  ENDCASE.

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE .
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O' 'P'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FILE1 .
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING S_FILE1 'O' 'S1'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FILE2 .
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING S_FILE2 'O' 'S2'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FILE3 .
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING S_FILE3 'O' 'S3'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FILE4 .
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING S_FILE4 'O' 'S4'.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.

  IF P_CHK1 = 'X'.
    PERFORM PROCESS_SPEC.
  ENDIF.
  IF P_CHK2 = 'X'.
    PERFORM PROCESS_PLAN.
  ENDIF.
*PERFORM progress_bar USING 'TRANSFER sPEC AND DATA TO HMC pilot DB'.


*  SET PF-STATUS 'PILOT DB (HMC)'.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  SEND_VPDS
*&---------------------------------------------------------------------*
FORM SEND_VPDS.
* VehicleProd Spec.
*  PERFORM progress_bar USING 'TRANSFER sPEC AND DATA TO HMC pilot DB'.
*SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztppvs FROM
*ztppvs_pilot
  .

  IF NOT IT_SPEC_EXCL[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_SET_ZTPPVS_PILOT'
      DESTINATION C_DEST
      IMPORTING
        Z_RES         = Z_RES
        Z_MSG         = Z_MSG
        ZEDAT         = ZEDAT
        ZETIM         = ZETIM
      TABLES
        i_zsppvs      = IT_SPEC_EXCL
        I_ZSPPTECH    = IT_ZSPPTECH
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

    IF Z_RES = 'E' OR SY-SUBRC <> 0.
      DATA L_TEXT(132).
      MESSAGE S000(ZDCO) WITH Z_MSG L_MSGTXT.
      CONCATENATE '' 'ERROR WHILE TRANSFERING PILOT Specification'
                  INTO L_TEXT.
      WRITE: / L_TEXT. CLEAR L_TEXT.
      CONCATENATE Z_MSG L_MSGTXT INTO L_TEXT.
      WRITE: / L_TEXT.

    ELSE .
      CONCATENATE '' 'PILOT Specification SENT TO HMC SUCCESSFULLY'
                    INTO L_TEXT.
      WRITE: / L_TEXT.
    ENDIF.

  ENDIF.

ENDFORM.           "SEND_VPDS

*&---------------------------------------------------------------------*
*&      Form  SEND_VPLO
*&---------------------------------------------------------------------*
FORM SEND_VPLO.
* Vehicle planned order
*  PERFORM progress_bar USING 'TRANSFER sPEC AND DATA TO HMC pilot DB'.

*SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztppvp FROM
*ztppvp_pilot
  .
  IF NOT IT_PLAN_EXCL[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_SET_ZTPPVP_PILOT'
          DESTINATION C_DEST
          IMPORTING
               Z_RES                 = Z_RES
               Z_MSG                 = Z_MSG
               ZEDAT                 = ZEDAT
               ZETIM                 = ZETIM
          TABLES
               I_ZSPPVP              = IT_PLAN_EXCL
          EXCEPTIONS
               COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
               SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

    IF Z_RES = 'E' OR SY-SUBRC <> 0.
      DATA L_TEXT(132).
      MESSAGE S000(ZDCO) WITH Z_MSG L_MSGTXT.
      CONCATENATE '' 'ERROR WHILE TRANSFERING PILOT Planned Order'
                  INTO L_TEXT.
      WRITE: / L_TEXT. CLEAR L_TEXT.
      CONCATENATE Z_MSG L_MSGTXT INTO L_TEXT.
      WRITE: / L_TEXT.

    ELSE .
      CONCATENATE '' 'PILOT Planned Order SENT TO HMC SUCCESSFULLY'
                    INTO L_TEXT.
      WRITE: / L_TEXT.
    ENDIF.

  ENDIF.
ENDFORM.           "SEND_VPLO

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*

FORM progress_bar USING   p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = p_text.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PLAN
*&---------------------------------------------------------------------*
FORM UPLOAD_PLAN.

  IF NOT P_FILE IS INITIAL.
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              CODEPAGE                = ' '
              FILENAME                = P_FILE
              FILETYPE                = P_FILETY
         TABLES
              DATA_TAB                = IT_PLAN_EXCL
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.
  ENDIF.
  CASE SY-SUBRC.
    WHEN 0.
      DATA L_TEXT(132).
      CONCATENATE '' 'PLANNED ORDER FILE UPLOADED SUCCESSFULLY'
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000(ZDCO) WITH TEXT-002.
    WHEN 3.
      MESSAGE E000 WITH TEXT-003.
    WHEN OTHERS.
      MESSAGE E000 WITH TEXT-004.
  ENDCASE.

ENDFORM.                               " UPLOAD_PLAN
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_SPEC
*&---------------------------------------------------------------------*
FORM UPLOAD_SPEC.

* Load File 1
  IF NOT S_FILE1 IS INITIAL.
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              CODEPAGE                = ' '
              FILENAME                = S_FILE1
              FILETYPE                = S_FTY1
         TABLES
              DATA_TAB                = it_spec1
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.
  ENDIF.
* Load File 2
  IF NOT S_FILE2 IS INITIAL.
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              CODEPAGE                = ' '
              FILENAME                = S_FILE2
              FILETYPE                = S_FTY2
         TABLES
              DATA_TAB                = it_spec2
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.
  ENDIF.
* Load File 3.
  IF NOT S_FILE3 IS INITIAL.

    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              CODEPAGE                = ' '
              FILENAME                = S_FILE3
              FILETYPE                = S_FTY3
         TABLES
              DATA_TAB                = it_spec3
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.
  ENDIF.
* Load File 4.
  IF NOT S_FILE4 IS INITIAL.

    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              CODEPAGE                = ' '
              FILENAME                = S_FILE4
              FILETYPE                = S_FTY4
         TABLES
              DATA_TAB                = it_spec4
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.
  ENDIF.
  CASE SY-SUBRC.
    WHEN 0.
      DATA L_TEXT(132).
      CONCATENATE '' 'SPECIFICATION FILE UPLOADED SUCCESSFULLY'
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000(ZDCO) WITH TEXT-002.
    WHEN 3.
      MESSAGE E000 WITH TEXT-003.
    WHEN OTHERS.
      MESSAGE E000 WITH TEXT-004.
  ENDCASE.

ENDFORM.                               " UPLOAD_SPEC.

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C
                                          ZFILE TYPE C.


  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  TMP_MASK = ',*.*,*.*.'.
  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING

            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
*           MASK             = ',*.*,*.*.'
            MASK             = TMP_MASK
            MODE             = MODE
*           TITLE            = ' '
       IMPORTING
            FILENAME         = TMP_FILENAME
*         RC               =
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    CASE ZFILE.
      WHEN 'P'.
        P_FILE = TMP_FILENAME.
      WHEN 'S1'.
        S_FILE1 = TMP_FILENAME.
      WHEN 'S2'.
        S_FILE2 = TMP_FILENAME.
      WHEN 'S3'.
        S_FILE3 = TMP_FILENAME.
      WHEN 'S4'.
        S_FILE4 = TMP_FILENAME.

    ENDCASE.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PLAN
*&---------------------------------------------------------------------*
FORM PROCESS_PLAN.


  PERFORM UPLOAD_PLAN.
  DESCRIBE TABLE IT_PLAN_EXCL LINES COUNT.
  IF COUNT > 0.
*    LOOP AT IT_PLAN_EXCL.
**       MOVE-CORRESPONDING IT_PLAN_EXCL TO it_ztppvp.
**       APPEND it_ztppvp.
*      MODIFY ztppvp_pilot FROM IT_PLAN_EXCL.
*    ENDLOOP.

    IF c_VPLO = 'X' .
      PERFORM SEND_VPLO.
    ENDIF.

    LOOP AT IT_PLAN_EXCL.
      MOVE SY-UNAME TO IT_PLAN_EXCL-ZUSER.
      MOVE SY-DATUM TO IT_PLAN_EXCL-ZSDAT.
      MOVE SY-UZEIT TO IT_PLAN_EXCL-ZSTIM.
      MOVE ZEDAT TO IT_PLAN_EXCL-ZEDAT.
      MOVE ZETIM TO IT_PLAN_EXCL-ZETIM.
      MOVE Z_RES TO IT_PLAN_EXCL-ZRESULT.
      MOVE Z_MSG TO IT_PLAN_EXCL-ZMSG.
      MODIFY IT_PLAN_EXCL.
      MODIFY ztppvp_pilot FROM IT_PLAN_EXCL.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " PROCESS_PLAN
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SPEC
*&---------------------------------------------------------------------*
FORM PROCESS_SPEC.
  PERFORM UPLOAD_SPEC.
  COUNT = 0.
  COUNTER = 0.
  DESCRIBE TABLE IT_SPEC1 LINES COUNT.
  IF COUNT > 0.

    WHILE COUNTER < COUNT.
*   LOOP AT IT_SPEC1.
*     MOVE-CORRESPONDING IT_SPEC1 TO IT_PLAN_EXCL.
*   ENDLOOP.
      READ TABLE IT_SPEC1 INDEX COUNTER.
      MOVE-CORRESPONDING IT_SPEC1 TO IT_SPEC_EXCL.
      READ TABLE IT_SPEC2 INDEX COUNTER.
      MOVE-CORRESPONDING IT_SPEC2 TO IT_SPEC_EXCL.
      READ TABLE IT_SPEC3 INDEX COUNTER.
      MOVE-CORRESPONDING IT_SPEC3 TO IT_SPEC_EXCL.
      READ TABLE IT_SPEC4 INDEX COUNTER.
      MOVE-CORRESPONDING IT_SPEC4 TO IT_SPEC_EXCL.
      COUNTER = COUNTER + 1.
      APPEND IT_SPEC_EXCL.
    ENDWHILE.

    IF c_VPDS = 'X' .
      PERFORM SEND_VPDS.
    ENDIF.

    LOOP AT IT_SPEC_EXCL.
      MOVE SY-UNAME TO IT_SPEC_EXCL-ZUSER.
      MOVE SY-DATUM TO IT_SPEC_EXCL-ZSDAT.
      MOVE SY-UZEIT TO IT_SPEC_EXCL-ZSTIM.
      MOVE ZEDAT TO IT_SPEC_EXCL-ZEDAT.
      MOVE ZETIM TO IT_SPEC_EXCL-ZETIM.
      MOVE Z_RES TO IT_SPEC_EXCL-ZRESULT.
      MOVE Z_MSG TO IT_SPEC_EXCL-ZMSG.
      MODIFY IT_SPEC_EXCL.
      MODIFY ztppvs_pilot FROM IT_SPEC_EXCL.
    ENDLOOP.


  ENDIF.
ENDFORM.                    " PROCESS_SPEC
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
FORM modify_screen.
  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      IF P_CHK1 EQ 'X'.
        screen-input = 1.
        c_VPDS = 'X'.

*        screen-invisible = 0.
      ELSE.
        screen-input = 0.
        c_VPDS = ''.
*        screen-invisible = 1.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'GR2'.
      IF P_CHK2 EQ 'X'.
        screen-input = 1.
        c_VPLO = 'X'.
      ELSE.
        screen-input = 0.
        c_VPLO = ''.
      ENDIF.
*      screen-invisible = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " modify_screen
