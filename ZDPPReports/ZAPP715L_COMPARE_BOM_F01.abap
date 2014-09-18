*----------------------------------------------------------------------*
*   INCLUDE ZAPP715L_COMPARE_BOM_F01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_ICON_9000
*&---------------------------------------------------------------------*
FORM SET_ICON_9000.
  DATA : L_LINES_IX   LIKE  SY-TABIX.

*-----> SET R_DATUM
  CLEAR R_DATUM.
  DESCRIBE TABLE R_DATUM LINES L_LINES_IX.
  IF L_LINES_IX GT 0.
    READ TABLE R_DATUM INDEX 1.
  ENDIF.
  PERFORM:
    SET_ICON_EXCL USING WA_DATE_EXCL R_DATUM-SIGN R_DATUM-OPTION,
    SET_ICON_MORE USING WA_DATE_MORE L_LINES_IX.

*-----> SET R_MATNR
  CLEAR R_MATNR.
  DESCRIBE TABLE R_MATNR LINES L_LINES_IX.
  IF L_LINES_IX GT 0.
    READ TABLE R_MATNR INDEX 1.
  ENDIF.
  PERFORM:
    SET_ICON_EXCL USING WA_MATNR_EXCL R_MATNR-SIGN R_MATNR-OPTION,
    SET_ICON_MORE USING WA_MATNR_MORE L_LINES_IX.

*-----> SET R_PSTTR
  CLEAR R_PSTTR.
  DESCRIBE TABLE R_PSTTR LINES L_LINES_IX.
  IF L_LINES_IX GT 0.
    READ TABLE R_PSTTR INDEX 1.
  ENDIF.
  PERFORM:
    SET_ICON_EXCL USING WA_PSTTR_EXCL R_PSTTR-SIGN R_PSTTR-OPTION,
    SET_ICON_MORE USING WA_PSTTR_MORE L_LINES_IX.

ENDFORM.                    " SET_ICON_9000
*&---------------------------------------------------------------------*
*&      Form  SET_ICON_EXCL
*&---------------------------------------------------------------------*
FORM SET_ICON_EXCL USING ICON
                   VALUE(SIGNUM) LIKE TVARV-SIGN
                   VALUE(OPTION) LIKE TVARV-OPTI.
  DATA: UP_ICON LIKE RSCSEL-SOP_I.

  IF SIGNUM EQ 'E' OR 'EQ/BT' NS OPTION. "Icon not necessary
    CALL FUNCTION 'SELSCREEN_ICONS_SUPPLY'
         EXPORTING
              SIGN        = SIGNUM
              OPTION      = OPTION
         IMPORTING
              ICON_RESULT = UP_ICON.
  ENDIF.
  ICON = UP_ICON.
ENDFORM.                    " SET_ICON_EXCL
*&---------------------------------------------------------------------*
*&      Form  SET_ICON_MORE
*&---------------------------------------------------------------------*
FORM SET_ICON_MORE USING ICON VALUE(LINES).

  IF LINES LE 1.                       "up to one line
*    IF icon_enter_more IS INITIAL.     "fill icon-string first
*      PERFORM set_icon USING icon_enter_more 'ICON_ENTER_MORE'.
*    ENDIF.
    ICON = ICON_ENTER_MORE.
  ELSE.
*    IF icon_display_more IS INITIAL.   "fill icon-string first
*      PERFORM set_icon USING icon_display_more 'ICON_DISPLAY_MORE'.
*    endif.
    ICON = ICON_DISPLAY_MORE.
  ENDIF.
ENDFORM.                    " SET_ICON_MORE

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM DISPLAY_DATA.

  CLEAR : ZTPP_COMPPARTVIN, WA_CHANGED, WA_PARTNEW.
  CASE SY-DYNNR.
    WHEN '9000'.
      MOVE IT_SCREEN-CHK           TO CHK.
      MOVE-CORRESPONDING IT_SCREEN TO ZTPP_COMPPARTVIN.
      IF IT_SCREEN-APGRP EQ 'S'.
        MOVE ZTPP_COMPPARTVIN-PARTOLD TO WA_PARTNEW.
        CLEAR ZTPP_COMPPARTVIN-PARTOLD.
      ENDIF.

    WHEN '9100'.
      MOVE-CORRESPONDING IT_SCREEN9100 TO ZTPP_COMPPARTVIN.
      IF IT_SCREEN9100-APGRP EQ 'S'.
        MOVE ZTPP_COMPPARTVIN-PARTOLD TO WA_PARTNEW.
        CLEAR ZTPP_COMPPARTVIN-PARTOLD.
      ELSE.
        MOVE IT_SCREEN9100-CHANGED TO WA_CHANGED.
      ENDIF.

  ENDCASE.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  FILL_RANGES
*&---------------------------------------------------------------------*
FORM FILL_RANGES.

*----> Fill Ranges for R_DATUM
  IF R_DATUM-SIGN EQ 'I' AND 'EQ/BT' CS R_DATUM-OPTION OR
     R_DATUM-SIGN EQ 'E' AND 'NE/NB' CS R_DATUM-OPTION.
    CLEAR: R_DATUM-OPTION,
           R_DATUM-SIGN.
  ENDIF.
  CALL FUNCTION 'SELOPTS_INPUT_ADJUST'
       CHANGING
            SIGN   = R_DATUM-SIGN
            OPTION = R_DATUM-OPTION
            LOW    = R_DATUM-LOW
            HIGH   = R_DATUM-HIGH
       EXCEPTIONS
            OTHERS = 4.
  IF SY-SUBRC NE 0.
    SET CURSOR FIELD 'R_DATUM-LOW'.
    MESSAGE E650(DB).
  ENDIF.
  IF R_DATUM-LOW  IS INITIAL AND
     R_DATUM-HIGH IS INITIAL.
*AND 'EQ/BT/NE/NB'  CS R_DATUM-OPTION.
    DELETE R_DATUM INDEX 1.          "if existing, delete old value
  ELSE.
    MODIFY R_DATUM INDEX 1.
    IF SY-SUBRC NE 0.
      APPEND R_DATUM.
    ENDIF.
  ENDIF.


*----> Fill Ranges for R_MATNR
  IF R_MATNR-SIGN EQ 'I' AND 'EQ/BT' CS R_MATNR-OPTION OR
     R_MATNR-SIGN EQ 'E' AND 'NE/NB' CS R_MATNR-OPTION.
    CLEAR: R_MATNR-OPTION,
           R_MATNR-SIGN.
  ENDIF.
  CALL FUNCTION 'SELOPTS_INPUT_ADJUST'
       CHANGING
            SIGN   = R_MATNR-SIGN
            OPTION = R_MATNR-OPTION
            LOW    = R_MATNR-LOW
            HIGH   = R_MATNR-HIGH
       EXCEPTIONS
            OTHERS = 4.
  IF SY-SUBRC NE 0.
    SET CURSOR FIELD 'R_MATNR-LOW'.
    MESSAGE E650(DB).
  ENDIF.
  IF R_MATNR-LOW  IS INITIAL AND
     R_MATNR-HIGH IS INITIAL.
*AND 'EQ/BT/NE/NB'  CS R_MATNR-OPTION.
    DELETE R_MATNR INDEX 1.          "if existing, delete old value
  ELSE.
    MODIFY R_MATNR INDEX 1.
    IF SY-SUBRC NE 0.
      APPEND R_MATNR.
    ENDIF.
  ENDIF.

*----> Fill Ranges for R_PSTTR
*  IF R_PSTTR-SIGN EQ 'I' AND 'EQ/BT' CS R_PSTTR-OPTION OR
*     R_PSTTR-SIGN EQ 'E' AND 'NE/NB' CS R_PSTTR-OPTION.
*    CLEAR: R_PSTTR-OPTION,
*           R_PSTTR-SIGN.
*  ENDIF.
*  CALL FUNCTION 'SELOPTS_INPUT_ADJUST'
*       CHANGING
*            SIGN   = R_PSTTR-SIGN
*            OPTION = R_PSTTR-OPTION
*            LOW    = R_PSTTR-LOW
*            HIGH   = R_PSTTR-HIGH
*       EXCEPTIONS
*            OTHERS = 4.
*  IF SY-SUBRC NE 0.
*    SET CURSOR FIELD 'R_PSTTR-LOW'.
*    MESSAGE E650(DB).
*  ENDIF.
*  IF R_PSTTR-LOW  IS INITIAL AND
*     R_PSTTR-HIGH IS INITIAL.
**AND 'EQ/BT/NE/NB'  CS R_PSTTR-OPTION.
*    DELETE R_PSTTR INDEX 1.          "if existing, delete old value
*  ELSE.
*    MODIFY R_PSTTR INDEX 1.
*    IF SY-SUBRC NE 0.
*      APPEND R_PSTTR.
*    ENDIF.
*  ENDIF.
  DATA L_NUM   TYPE  I.

  IF R_PSTTR-HIGH IS INITIAL.
    REFRESH R_PSTTR.
    R_PSTTR-SIGN   = 'I'.
    R_PSTTR-OPTION = 'EQ'.
    APPEND R_PSTTR.
  ELSE.
    REFRESH R_PSTTR.
    R_PSTTR-SIGN   = 'I'.
    R_PSTTR-OPTION = 'BT'.
    L_NUM = R_PSTTR-HIGH - R_PSTTR-LOW.
    IF L_NUM GE 3.
      MESSAGE E001 WITH TEXT-201.
    ELSEIF L_NUM LT 0.
      MESSAGE E650(DB).
    ELSE.
      APPEND R_PSTTR.
    ENDIF.
  ENDIF.
ENDFORM.                    " FILL_RANGES
*&---------------------------------------------------------------------*
*&      Form  MORE_ENTRIES
*&---------------------------------------------------------------------*
FORM MORE_ENTRIES USING P_IND.

  DATA: L_TEXT LIKE RSSELINT-TEXT.

  CASE P_IND.
    WHEN 'SDAT'.
      L_TEXT = TEXT-101.
      WA_HELP_FIELD = 'DATUM'.
      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
           EXPORTING
                TEXT       = L_TEXT
                HELP_FIELD = WA_HELP_FIELD
           TABLES
                RANGE      = R_DATUM
           EXCEPTIONS
                CANCELLED  = 1
                OTHERS     = 2.
    WHEN 'SMAT'.
      L_TEXT = TEXT-102.
      WA_HELP_FIELD = 'MATNR'.
      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
           EXPORTING
                TEXT       = L_TEXT
                HELP_FIELD = WA_HELP_FIELD
           TABLES
                RANGE      = R_MATNR
           EXCEPTIONS
                CANCELLED  = 1
                OTHERS     = 2.

    WHEN 'PSTA'.
*      L_TEXT = TEXT-101.
*      WA_HELP_FIELD = 'DATUM'.
*      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
*           EXPORTING
*                TEXT       = L_TEXT
*                HELP_FIELD = WA_HELP_FIELD
*           TABLES
*                RANGE      = R_PSTTR
*           EXCEPTIONS
*                CANCELLED  = 1
*                OTHERS     = 2.
  ENDCASE.
  IF SY-SUBRC EQ 0.
    PERFORM CHECK_RANGE_VALUES USING P_IND.
  ELSE.
    MESSAGE S781(DB).
  ENDIF.

ENDFORM.                    " MORE_ENTRIES
*&---------------------------------------------------------------------*
*&      Form  CHECK_RANGE_VALUES
*&---------------------------------------------------------------------*
FORM CHECK_RANGE_VALUES USING P_IND.
  CASE P_IND.
*-- R_DATUM ---------------------------
    WHEN 'SDAT'.
      LOOP AT R_DATUM.
        IF R_DATUM-LOW  IS INITIAL AND
           R_DATUM-HIGH IS INITIAL.
*     AND  'EQ/BT/NE/NB'  CS R_DATUM-OPTION.
          DELETE R_DATUM.            "if existing, delete old value
        ENDIF.
      ENDLOOP.

*-- R_MATNR ---------------------------
    WHEN 'SMAT'.
      LOOP AT R_MATNR.
        IF R_MATNR-LOW  IS INITIAL AND
           R_MATNR-HIGH IS INITIAL.
* AND      'EQ/BT/NE/NB'  CS R_MATNR-OPTION.
          DELETE R_MATNR.            "if existing, delete old value
        ENDIF.
      ENDLOOP.

*-- R_PSTTR ---------------------------
    WHEN 'PSTA'.
      LOOP AT R_PSTTR.
        IF R_PSTTR-LOW  IS INITIAL AND
           R_PSTTR-HIGH IS INITIAL.
*     AND  'EQ/BT/NE/NB'  CS R_PSTTR-OPTION.
          DELETE R_PSTTR.            "if existing, delete old value
        ENDIF.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " CHECK_RANGE_VALUES
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  DATA : L_TABIX  TYPE  SY-TABIX.
*-----> Selected compared data in condition of input field
  CLEAR : IT_SCREEN, IT_SCREEN[].

*  IF R_MATNR[] IS INITIAL AND R_DATUM[] IS INITIAL.
*
*  ELSEIF NOT R_MATNR[] IS INITIAL AND R_DATUM[] IS INITIAL.
*
*  ELSEIF R_MATNR[] IS INITIAL AND NOT R_DATUM[] IS INITIAL.
*
*  ELSE.
*
*  ENDIF.

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_SCREEN
         FROM ZTPP_COMPPARTVIN
         WHERE PARTOLD IN R_MATNR
            OR PSTTR   IN R_DATUM.
  SORT IT_SCREEN BY MATNR VERID EQUNR BAUST BAUWG POSNR APGRP.
  TC_9000-TOP_LINE = 1.
  DESCRIBE TABLE IT_SCREEN LINES TC_9000-LINES.

*----> RP_STATUS.
  LOOP AT IT_SCREEN.
    MOVE SY-TABIX TO L_TABIX.
    AT NEW EQUNR.
      MOVE IT_SCREEN-EQUNR TO WA_OBJEK.
*----> Check P_RP_STATUS
      PERFORM SELECT_CLASSIFICATION USING 'P_RP_STATUS'
                                    CHANGING WA_ATWRT.
    ENDAT.
    MOVE WA_ATWRT  TO  IT_SCREEN-RP_POINT.

    IF NOT IT_SCREEN-PARTNEW IS INITIAL.
      MOVE 'Y'  TO  IT_SCREEN-CHANGED.
    ENDIF.
    MODIFY IT_SCREEN INDEX L_TABIX.
    CLEAR IT_SCREEN.
  ENDLOOP.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  BATCH_JOB
*&---------------------------------------------------------------------*
FORM BATCH_JOB.
  DATA: L_DATES     TYPE  I.

  MOVE :  TEXT-901    TO  WA_JOBNAME,
          WA_JOBNAME  TO  WA_REPORT.
  IF R_PSTTR[] IS INITIAL.
    MESSAGE I001 WITH TEXT-202.
  ELSE.
    READ TABLE R_PSTTR INDEX 1.
    IF R_PSTTR-OPTION EQ 'BT'.
      L_DATES = R_PSTTR-HIGH - R_PSTTR-LOW.
    ENDIF.

    IF L_DATES GE 3.
      MESSAGE E001 WITH TEXT-201.
    ELSE.
*-----> JOB OPEN
      PERFORM CALL_JOB_OPEN USING WA_JOBNAME WA_JOBCOUNT.

*-----> JOB SUBMIT
      SUBMIT (WA_REPORT) WITH S_PSTTR IN R_PSTTR
                       USER SY-UNAME
                       VIA JOB WA_JOBNAME
                       NUMBER WA_JOBCOUNT
                       AND RETURN.

*    PERFORM CALL_JOB_SUBMIT USING WA_JOBNAME
*                                  WA_REPORT
*                                  WA_JOBCOUNT.

*-----> JOB CLOSE
      PERFORM CALL_JOB_CLOSE USING WA_JOBNAME
                                   WA_JOBCOUNT.
    ENDIF.
  ENDIF.
ENDFORM.                    " BATCH_JOB
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_OPEN
*&---------------------------------------------------------------------*
FORM CALL_JOB_OPEN USING P_JOBNAME P_JOBCOUNT.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*      DELANFREP              = ' '
*      JOBGROUP               = ' '
      JOBNAME                = P_JOBNAME
*      SDLSTRTDT              = NO_DATE
*      SDLSTRTTM              = NO_TIME
    IMPORTING
      JOBCOUNT               = P_JOBCOUNT
    EXCEPTIONS
      CANT_CREATE_JOB        = 1
      INVALID_JOB_DATA       = 2
      JOBNAME_MISSING        = 3
      OTHERS                 = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDFORM.                    " CALL_JOB_OPEN

*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
FORM CALL_JOB_SUBMIT USING P_JOBNAME
                           P_REPORT
                           P_JOBCOUNT.

  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
*      ARCPARAMS                         =
      AUTHCKNAM                         = SY-UNAME
*      COMMANDNAME                       = ' '
*      OPERATINGSYSTEM                   = ' '
*      EXTPGM_NAME                       = ' '
*      EXTPGM_PARAM                      = ' '
*      EXTPGM_SET_TRACE_ON               = ' '
*      EXTPGM_STDERR_IN_JOBLOG           = 'X'
*      EXTPGM_STDOUT_IN_JOBLOG           = 'X'
*      EXTPGM_SYSTEM                     = ' '
*      EXTPGM_RFCDEST                    = ' '
*      EXTPGM_WAIT_FOR_TERMINATION       = 'X'
      JOBCOUNT                          = P_JOBCOUNT
      JOBNAME                           = P_JOBNAME
*      LANGUAGE                          = SY-LANGU
*      PRIPARAMS                         = ' '
      REPORT                            = P_REPORT
*      VARIANT                           = ' '
*    IMPORTING
*      STEP_NUMBER                       =
      EXCEPTIONS
      BAD_PRIPARAMS                     = 1
      BAD_XPGFLAGS                      = 2
      INVALID_JOBDATA                   = 3
      JOBNAME_MISSING                   = 4
      JOB_NOTEX                         = 5
      JOB_SUBMIT_FAILED                 = 6
      LOCK_FAILED                       = 7
      PROGRAM_MISSING                   = 8
      PROG_ABAP_AND_EXTPG_SET           = 9
      OTHERS                            = 10.

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " CALL_JOB_SUBMIT

*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
FORM CALL_JOB_CLOSE USING P_JOBNAME P_JOBCOUNT.
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
*   AT_OPMODE                         = ' '
*   AT_OPMODE_PERIODIC                = ' '
*   CALENDAR_ID                       = ' '
*   EVENT_ID                          = ' '
*   EVENT_PARAM                       = ' '
*   EVENT_PERIODIC                    = ' '
      JOBCOUNT                          = P_JOBCOUNT
      JOBNAME                           = P_JOBNAME
*   LASTSTRTDT                        = NO_DATE
*   LASTSTRTTM                        = NO_TIME
*   PRDDAYS                           = 0
*   PRDHOURS                          = 0
*   PRDMINS                           = 0
*   PRDMONTHS                         = 0
*   PRDWEEKS                          = 0
*   PREDJOB_CHECKSTAT                 = ' '
*   PRED_JOBCOUNT                     = ' '
*   PRED_JOBNAME                      = ' '
*   SDLSTRTDT                         = NO_DATE
*   SDLSTRTTM                         = NO_TIME
*   STARTDATE_RESTRICTION             = BTC_PROCESS_ALWAYS
     STRTIMMED                         = 'X'  "IMMEDIATE
*   TARGETSYSTEM                      = ' '
*   START_ON_WORKDAY_NOT_BEFORE       = SY-DATUM
*   START_ON_WORKDAY_NR               = 0
*   WORKDAY_COUNT_DIRECTION           = 0
*   RECIPIENT_OBJ                     =
*   TARGETSERVER                      = ' '
*   DONT_RELEASE                      = ' '
* IMPORTING
*   JOB_WAS_RELEASED                  =
   EXCEPTIONS
     CANT_START_IMMEDIATE              = 1
     INVALID_STARTDATE                 = 2
     JOBNAME_MISSING                   = 3
     JOB_CLOSE_FAILED                  = 4
     JOB_NOSTEPS                       = 5
     JOB_NOTEX                         = 6
     LOCK_FAILED                       = 7
     OTHERS                            = 8.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*&      Form  DETAILED_DISPLAY
*&---------------------------------------------------------------------*
FORM DETAILED_DISPLAY.
  DATA : L_ATINN   TYPE  AUSP-ATINN.

  CLEAR : IT_TEMP, IT_TEMP[],
          IT_SCREEN9100, IT_SCREEN9100[].

*-----> DATA Selected CheckBOX
  LOOP AT IT_SCREEN WHERE CHK EQ C_MARK.
    MOVE-CORRESPONDING IT_SCREEN TO IT_TEMP.
    APPEND IT_TEMP.
  ENDLOOP.

*-> Internal characteristic of P_RP_STATUS
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = C_RP_STATUS
       IMPORTING
            OUTPUT = L_ATINN.

*-----> Detailed DATA Selected FSC + Version
  LOOP AT IT_TEMP.
    LOOP AT IT_SCREEN WHERE MATNR EQ IT_TEMP-MATNR
                        AND VERID EQ IT_TEMP-VERID.
      CLEAR IT_SCREEN9100.
      MOVE-CORRESPONDING IT_SCREEN TO IT_SCREEN9100.

      SELECT SINGLE *
                   FROM ZTPP_PARTHISTORY
                   WHERE EQUNR EQ IT_SCREEN-EQUNR
                     AND BAUST EQ IT_SCREEN-BAUST
                     AND BAUWG EQ IT_SCREEN-BAUWG
                     AND POSNR EQ IT_SCREEN-POSNR
                     AND PARTOLD EQ IT_SCREEN-PARTOLD
                     AND BAUGR EQ IT_SCREEN-BAUGR.
      IF SY-SUBRC EQ 0.
        IT_SCREEN9100-CHANGED = 'Y'.
        IT_SCREEN9100-PARTNEW = ZTPP_PARTHISTORY-PARTNEW.
      ENDIF.
*--> RP VALUE
      SELECT SINGLE ATWRT
             INTO IT_SCREEN9100-RP_POINT
             FROM AUSP
             WHERE OBJEK EQ IT_SCREEN9100-EQUNR
               AND ATINN EQ L_ATINN
               AND KLART EQ '002'.
      IF SY-SUBRC NE 0.
        MOVE '00' TO IT_SCREEN9100-RP_POINT.
      ENDIF.
      APPEND IT_SCREEN9100.
    ENDLOOP.
  ENDLOOP.
  TC_9100-TOP_LINE  = 1.
  DESCRIBE TABLE IT_SCREEN9100 LINES TC_9100-LINES.
  CALL SCREEN 9100.
ENDFORM.                    " DETAILED_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DISPLAY
*&---------------------------------------------------------------------*
FORM CHANGE_DISPLAY.
  IF SY-TCODE NE 'ZPPA7150'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'SEL'.
        SCREEN-INVISIBLE = 1.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHANGE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM CHANGE_DISPLAY_DATA.
  DATA : L_BEFORE_LINE   TYPE  SY-TABIX.

  IF IT_SCREEN[] IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'CHK'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF SY-DYNNR EQ '9000'.
      L_BEFORE_LINE = TC_9000-CURRENT_LINE - 1.
      IF L_BEFORE_LINE NE 0.
        CLEAR WA_SCREEN_DS.
        READ TABLE IT_SCREEN INTO WA_SCREEN_DS
                             INDEX L_BEFORE_LINE.
        IF IT_SCREEN-MATNR EQ WA_SCREEN_DS-MATNR AND
           IT_SCREEN-VERID EQ WA_SCREEN_DS-VERID.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 EQ 'CHK'.
              SCREEN-INPUT = 0.
              MODIFY SCREEN.
            ENDIF.
            IF SCREEN-GROUP1 EQ 'DIS'.
              SCREEN-ACTIVE = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSEIF SY-DYNNR EQ '9100'.
      L_BEFORE_LINE = TC_9100-CURRENT_LINE - 1.
      IF L_BEFORE_LINE NE 0.
        CLEAR WA_SCREEN_DS.
        READ TABLE IT_SCREEN9100 INTO WA_SCREEN_DS
                             INDEX L_BEFORE_LINE.
        IF IT_SCREEN9100-MATNR EQ WA_SCREEN_DS-MATNR AND
           IT_SCREEN9100-VERID EQ WA_SCREEN_DS-VERID.
          LOOP AT SCREEN.
*          IF SCREEN-GROUP1 EQ 'CHK'.
*            SCREEN-INPUT = 0.
*            MODIFY SCREEN.
*          ENDIF.
            IF SCREEN-GROUP1 EQ 'DIS'.
              SCREEN-ACTIVE = 0.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM MODIFY_SCREEN.
  CASE SY-DYNNR.
    WHEN '9000'.
      MOVE :  CHK  TO  IT_SCREEN-CHK.
*      MOVE-CORRESPONDING ZTPP_COMPPARTVIN TO IT_SCREEN.
      MODIFY IT_SCREEN INDEX TC_9000-CURRENT_LINE.
    WHEN '9100'.
*      MOVE-CORRESPONDING ZTPP_COMPPARTVIN TO IT_SCREEN9100.
*      IF IT_SCREEN9100-APGRP EQ 'S'.
*        CLEAR : IT_SCREEN9100-PARTOLD, IT_SCREEN9100-PARTNEW.
*        MOVE ZTPP_COMPPARTVIN-PARTNEW TO IT_SCREEN9100-PARTOLD.
*      ENDIF.
*      MODIFY IT_SCREEN9100 INDEX TC_9100-CURRENT_LINE.
  ENDCASE.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CHANGE_HISTORY
*&---------------------------------------------------------------------*
FORM DISPLAY_CHANGE_HISTORY.
  DATA : L_LINE LIKE SY-INDEX,
         L_READ_LINE LIKE SY-INDEX.

*----> Display BOM Change History for Change No
  GET CURSOR LINE L_LINE.
  L_READ_LINE = L_LINE + TC_9000-TOP_LINE - 1.

  READ TABLE IT_SCREEN INDEX L_READ_LINE.
  IF SY-SUBRC EQ 0.
    LEAVE TO LIST-PROCESSING.
    WRITE: / IT_SCREEN-AENNR.
  ENDIF.
ENDFORM.                    " DISPLAY_CHANGE_HISTORY
*&---------------------------------------------------------------------*
*&      Form  SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM SELECT_CLASSIFICATION USING P_ATNAM
                           CHANGING P_ATWRT.
  DATA : L_ATINN   TYPE  CABN-ATINN,
         L_ATFOR   TYPE  CABN-ATFOR.

  PERFORM SELECT_CABN USING P_ATNAM
                    CHANGING L_ATINN L_ATFOR.
  PERFORM SELECT_AUSP USING WA_OBJEK L_ATINN '002' L_ATFOR
                    CHANGING P_ATWRT.
ENDFORM.                    " SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_CABN
*&---------------------------------------------------------------------*
FORM SELECT_CABN USING P_ATNAM TYPE CABN-ATNAM
                 CHANGING P_ATINN TYPE CABN-ATINN
                          P_ATFOR TYPE CABN-ATFOR.
  SELECT SINGLE ATINN
                ATFOR
              INTO (P_ATINN, P_ATFOR)
              FROM CABN
              WHERE ATNAM EQ P_ATNAM.

ENDFORM.                    " SELECT_CABN
*&---------------------------------------------------------------------*
*&      Form  SELECT_AUSP
*&---------------------------------------------------------------------*
FORM SELECT_AUSP USING    P_OBJEK P_ATINN P_KLART P_ATFOR
                 CHANGING P_ATWRT.
  DATA : L_NUM(10)  TYPE  N,
         L_INT      TYPE  I,
         L_ATFLV    TYPE  AUSP-ATFLV,
         L_ATWRT    TYPE  AUSP-ATWRT.

  SELECT SINGLE ATWRT
                ATFLV
               INTO (L_ATWRT, L_ATFLV)
               FROM AUSP
               WHERE OBJEK EQ P_OBJEK
                 AND ATINN EQ P_ATINN
                 AND KLART EQ P_KLART.
  IF P_ATFOR EQ 'CHAR'.
    P_ATWRT = L_ATWRT.
  ELSE.
    L_NUM = L_ATFLV.
    L_INT = L_NUM.
    WRITE L_INT TO P_ATWRT LEFT-JUSTIFIED NO-GROUPING.
  ENDIF.


ENDFORM.                    " SELECT_AUSP
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
FORM CHECK_LOCK_OBJECT.
  GET PARAMETER ID 'ZBAT' FIELD WA_PSTTR_FLG.
  IF WA_PSTTR_FLG EQ 'Y'.
    MESSAGE E001 WITH TEXT-203.
  ENDIF.

  IF OK_CODE EQ 'ZRUN'.
*----> ENQUEUE
    CALL FUNCTION 'ENQUEUE_EZPP_COMPPARTVIN'
         EXPORTING
              MODE_ZTPP_COMPPARTVIN = 'E'
              MANDT                 = SY-MANDT.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
*----> DEQUEUE
      CALL FUNCTION 'DEQUEUE_EZPP_COMPPARTVIN'
       EXPORTING
         MODE_ZTPP_COMPPARTVIN       = 'E'
         MANDT                       = SY-MANDT.
    ENDIF.

  ENDIF.
ENDFORM.                    " CHECK_LOCK_OBJECT
