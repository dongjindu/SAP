*----------------------------------------------------------------------
* Program ID        : ZM_IF_YM_1006_DEMAND
* Title             : [MM] GCCS Interface - Part Demand
* Created on        : 10/22/2007
* Created by        : Rakesh Gandhi
* Specifications By : Crossley, Ron
* Description       : GCCS Interface - Part Demand
* Modification Logs
* Date            Developer        RequestNo      Description
* 03/14/08        Furong Wang       UD1K943095    21 wks data from MDSM
*
*----------------------------------------------------------------------
REPORT ZM_IF_YM_1006_DEMAND MESSAGE-ID ZMCO.

TYPE-POOLS : SLIS,
             ICON.

TABLES: MARA,
        MDSB.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
TYPES: BEGIN OF TY_ALV.
INCLUDE  TYPE ZTMM_EAI_DEMAND.
TYPES : CHKBOX(1),
        ICON TYPE ICON_D,
        TABCOLOR  TYPE SLIS_T_SPECIALCOL_ALV.
TYPES: END OF TY_ALV.

DATA: BEGIN OF IT_RESB_TMP OCCURS 0,
        MATNR LIKE RESB-MATNR  ,
        BDTER LIKE RESB-BDTER  ,
        BDMNG LIKE RESB-BDMNG  ,
        LIFNR LIKE LFA1-LIFNR  ,
      END   OF IT_RESB_TMP     ,

      BEGIN OF IT_DAY OCCURS 21,
        SEQ     TYPE I         ,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_DAY          ,

      BEGIN OF IT_WEEK OCCURS 21,
        SEQ(2)  TYPE N          ,
        DATUM   LIKE SY-DATUM   ,
      END   OF IT_WEEK          .

DATA:  IT_RESB_TMP1 LIKE IT_RESB_TMP  OCCURS 0 WITH HEADER LINE,
       IT_RESB      LIKE IT_RESB_TMP  OCCURS 0 WITH HEADER LINE,
       IT_RESB1     LIKE IT_RESB_TMP  OCCURS 0 WITH HEADER LINE,
       IT_21DAY  LIKE ZTMM_EAI_DEMAND OCCURS 0 WITH HEADER LINE,
       IT_21WEEK LIKE ZTMM_EAI_DEMAND OCCURS 0 WITH HEADER LINE,
       GV_KALID    LIKE KAKO-KALID ,
       GV_LASTDATE LIKE SY-DATUM   ,
       GV_LASTWEEK LIKE SY-DATUM   ,
       GV_TEXT(20) TYPE C          .

DATA: GT_ALV        TYPE TABLE OF TY_ALV     WITH HEADER LINE   ,
      GT_FIELDCAT   TYPE SLIS_T_FIELDCAT_ALV                    .

DATA : GS_LAYOUT   TYPE SLIS_LAYOUT_ALV  ,
       GS_VARIANT  TYPE DISVARIANT       ,
       GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
       GV_REPID    LIKE SY-REPID         ,
       GV_LINES TYPE I                   .
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR MARA-MATNR.
PARAMETERS: P_DATUM LIKE SY-DATUM DEFAULT SY-DATUM OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_D21 RADIOBUTTON GROUP G2 DEFAULT 'X',
             P_W21 RADIOBUTTON GROUP G2            ,
             P_BOTH RADIOBUTTON GROUP G2           .

SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-002.
PARAMETER: P_OPT1 RADIOBUTTON GROUP G1 DEFAULT 'X',
           P_OPT2 RADIOBUTTON GROUP G1,
           P_OPT3 RADIOBUTTON GROUP G1,
           P_OPT4 RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B5.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME.
PARAMETERS P_DEST LIKE RFCDES-RFCDEST
                       DEFAULT 'WMRM01'.
SELECTION-SCREEN END OF BLOCK B4.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = '[MM] GCCS Interface - Part Demand'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF P_D21 = 'X' OR P_BOTH = 'X'.
    PERFORM SET_DAYS.
  ENDIF.
  IF P_W21 = 'X' OR P_BOTH = 'X'.
    PERFORM SET_WEEK.
  ENDIF.
  IF P_OPT1 = 'X'.      " Display current data.
    PERFORM GET_DATA_FROM_ZTABLE.
  ELSEIF P_OPT2 = 'X'.  " Refresh Data
    PERFORM SAVE_DATA.
  ELSEIF P_OPT3 = 'X'.  " Refresh and send data
    PERFORM SAVE_DATA.
    PERFORM SEND_DATA_TO_EAI.
  ELSEIF P_OPT4 = 'X'.  " Send existing data
    PERFORM SEND_DATA_TO_EAI.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  alv_variant_f4
*&---------------------------------------------------------------------*
*       F4 help for ALV Variant
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM ALV_VARIANT_F4 CHANGING P_VARI.
  DATA: RS_VARIANT LIKE DISVARIANT.

  CLEAR RS_VARIANT.
  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = RS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = RS_VARIANT
       EXCEPTIONS
            OTHERS     = 1.

  IF SY-SUBRC = 0.
    P_VARI = RS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " alv_variant_f4
*&---------------------------------------------------------------------*
*&      Form  set_days
*&---------------------------------------------------------------------*
*       Subroutine to calculate number of days
*----------------------------------------------------------------------*
FORM SET_DAYS.
  DATA: L_COUNT TYPE I.
  DATA: L_DATE LIKE SY-DATUM.

*-reading working calendar
  PERFORM READ_SHOP_CALID  USING GV_KALID.
*-first is current input date
  IT_DAY-SEQ = 0.
  IT_DAY-DATUM = P_DATUM.
  APPEND IT_DAY.
  CLEAR L_COUNT.
  L_DATE = P_DATUM .
  DO 21 TIMES.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+'  GV_KALID  L_DATE.
    IT_DAY-SEQ     = L_COUNT.
    IT_DAY-DATUM   = L_DATE .
    APPEND IT_DAY.  CLEAR: IT_DAY.
  ENDDO.
  SORT IT_DAY BY SEQ DATUM.
  GV_LASTDATE = L_DATE .
ENDFORM.                    " set_days
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gv_kalid  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE KALID INTO PA_KALID
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'T'   .
ENDFORM.                    " read_shop_calid
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  get_data_from_ztable
*&---------------------------------------------------------------------*
*       Subroutine to get existing data from Z table
*----------------------------------------------------------------------*
FORM GET_DATA_FROM_ZTABLE.
*  DATA: w_datfm(1) TYPE c.

  REFRESH: IT_21DAY.
  CLEAR  : IT_21DAY.
  IF P_D21 = 'X'.
    SELECT * FROM ZTMM_EAI_DEMAND
             INTO TABLE IT_21DAY
             WHERE EPART_NO IN S_MATNR AND
                   EDMD_TYPE = 'D'     AND
                   EDATE = P_DATUM.
  ELSEIF P_W21 = 'X'.
    SELECT * FROM ZTMM_EAI_DEMAND
             INTO TABLE IT_21DAY
             WHERE EPART_NO IN S_MATNR AND
                   EDMD_TYPE = 'W'     AND
                   EDATE = P_DATUM.
  ELSEIF P_BOTH = 'X'.
    SELECT * FROM ZTMM_EAI_DEMAND
             INTO TABLE IT_21DAY
             WHERE EPART_NO IN S_MATNR AND
                   EDATE = P_DATUM.

  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH TEXT-005.
    EXIT.
  ENDIF.

  GV_TEXT = 'Preparing output...'.
  PERFORM SHOW_PROGRESS USING GV_TEXT.

  REFRESH GT_ALV.
  CLEAR   GT_ALV.

*  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
*       EXPORTING
*            bname = sy-uname
*       IMPORTING
*            datfm = w_datfm.

  LOOP AT IT_21DAY.
    MOVE-CORRESPONDING IT_21DAY TO GT_ALV.
*    CASE w_datfm.
*      WHEN 1.
*        CONCATENATE it_21day-edate+2(2) it_21day-edate+0(2)
*                    it_21day-edate+4(4) INTO gt_alv-edate
*                    SEPARATED BY '.'.
*
*      WHEN 2.
*        CONCATENATE it_21day-edate+0(2) it_21day-edate+2(2)
*                    it_21day-edate+4(4) INTO gt_alv-edate
*                    SEPARATED BY '/'.
*
*      WHEN 3.
*        CONCATENATE it_21day-edate+0(2) it_21day-edate+2(2)
*                    it_21day-edate+4(4) INTO gt_alv-edate
*                    SEPARATED BY '-'.
*
*      WHEN 4.
*        CONCATENATE it_21day-edate+4(4) it_21day-edate+0(2)
*                    it_21day-edate+2(2) INTO gt_alv-edate
*                    SEPARATED BY '.'.
*
*      WHEN 5.
*        CONCATENATE it_21day-edate+4(4) it_21day-edate+0(2)
*                    it_21day-edate+2(2) INTO gt_alv-edate
*                    SEPARATED BY '/'.
*
*      WHEN 6.
*        CONCATENATE it_21day-edate+4(4) it_21day-edate+0(2)
*                    it_21day-edate+2(2) INTO gt_alv-edate
*                    SEPARATED BY '-'.
*
*    ENDCASE.

    CASE IT_21DAY-TAIT_TARG_RSLT.
      WHEN SPACE.
      WHEN 'S'.
        GT_ALV-ICON = ICON_LED_GREEN.
      WHEN 'F'.
        GT_ALV-ICON = ICON_LED_RED.
    ENDCASE.
    APPEND GT_ALV.
    CLEAR  GT_ALV.
  ENDLOOP.

  CLEAR GV_LINES.
  DESCRIBE TABLE  GT_ALV LINES GV_LINES.
  IF GV_LINES = 1.
    MESSAGE S000 WITH GV_LINES TEXT-012.
  ELSEIF GV_LINES > 1.
    MESSAGE S000 WITH GV_LINES TEXT-011.
  ENDIF.

  PERFORM DISPLAY_ALV_DATA.

ENDFORM.                    " get_data_from_ztable
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       Subroutine to delete existing data and Save new data
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: L_FDATE_21D LIKE SY-DATUM.
  DATA: L_LDATE_21D LIKE SY-DATUM.

  CLEAR GV_TEXT.
  GV_TEXT = 'Refreshing data...'.
  PERFORM SHOW_PROGRESS USING GV_TEXT.
  IF P_D21 = 'X'.
    DELETE FROM ZTMM_EAI_DEMAND
      WHERE EPART_NO IN S_MATNR AND
            EDMD_TYPE = 'D'     AND
            EDATE = P_DATUM.
  ELSEIF P_W21 = 'X'.
    DELETE FROM ZTMM_EAI_DEMAND
      WHERE EPART_NO IN S_MATNR AND
            EDMD_TYPE = 'W'     AND
            EDATE = P_DATUM.
  ELSEIF P_BOTH = 'X'.
    DELETE FROM ZTMM_EAI_DEMAND
      WHERE EPART_NO IN S_MATNR AND
            EDATE = P_DATUM.
  ENDIF.

  IF P_D21 = 'X'.
*-If 21 days data is selected, get data from first day
    SELECT A~MATNR BDTER BDMNG B~LIFNR
           INTO TABLE IT_RESB_TMP
           FROM MDSB AS A
           INNER JOIN EORD AS B
           ON A~MATNR = B~MATNR
           WHERE BDTER >= P_DATUM AND
                 A~MATNR IN S_MATNR AND
                 XLOEK = ' '
                AND VDATU <= SY-DATUM AND
                 BDATU >= SY-DATUM.
*
    SORT IT_RESB_TMP BY MATNR BDTER.
    LOOP AT IT_RESB_TMP.
      MOVE-CORRESPONDING IT_RESB_TMP TO IT_RESB.
      COLLECT IT_RESB.
    ENDLOOP.
    SORT IT_RESB BY MATNR BDTER.
    DELETE ADJACENT DUPLICATES FROM IT_RESB COMPARING MATNR BDTER.

    IF IT_RESB[] IS INITIAL.
      MESSAGE S000 WITH TEXT-005.
      EXIT.
    ENDIF.

  ELSEIF P_W21 = 'X'.
*-If 21 days data is selected, get data from first day of first week
    SORT IT_WEEK.

** Changed by Furong on 03/14/08   "UD1K943095
*    READ TABLE it_week INDEX 1.
*    SELECT matnr bdter bdmng
*           INTO TABLE it_resb_tmp1
*           FROM mdsb
*           WHERE bdter >= it_week-datum AND
*                 matnr IN s_matnr       AND
*                 xloek = ' '.

    READ TABLE IT_WEEK INDEX 4.
    L_FDATE_21D = IT_WEEK-DATUM - 1.
    READ TABLE IT_WEEK INDEX 22.
    L_LDATE_21D = IT_WEEK-DATUM.


    SELECT A~MATNR BDTER BDMNG B~LIFNR
           INTO TABLE IT_RESB_TMP1
           FROM MDSB AS A
           INNER JOIN EORD AS B
           ON A~MATNR = B~MATNR
           WHERE A~MATNR IN S_MATNR AND
                 BDTER BETWEEN P_DATUM AND  L_FDATE_21D AND
                 XLOEK = ' '
                AND VDATU <= SY-DATUM AND
                 BDATU >= SY-DATUM.
    .

    SELECT A~MATNR BDTER BDMNG B~LIFNR
           APPENDING TABLE IT_RESB_TMP1
           FROM MDSM AS A
           INNER JOIN EORD AS B
           ON A~MATNR = B~MATNR
            WHERE A~MATNR IN S_MATNR   AND
                  PLSCN = '900'  AND
                  BDTER BETWEEN L_FDATE_21D AND L_LDATE_21D
                  AND VDATU <= SY-DATUM AND
                 BDATU >= SY-DATUM.

** End of change
    SORT IT_RESB_TMP1 BY MATNR BDTER.
    LOOP AT IT_RESB_TMP1.
      MOVE-CORRESPONDING IT_RESB_TMP1 TO IT_RESB1.
      COLLECT IT_RESB1.
    ENDLOOP.
    SORT IT_RESB1 BY MATNR BDTER.
    DELETE ADJACENT DUPLICATES FROM IT_RESB1 COMPARING MATNR BDTER.

    IF IT_RESB1[] IS INITIAL.
      MESSAGE S000 WITH TEXT-005.
      EXIT.
    ENDIF.

  ELSEIF P_BOTH ='X'.
*-If both 21 days and 21 weeks data is selected, for 21 days, get data
*-from first day and for 21 weeks get data from first day of first week
    SELECT A~MATNR BDTER BDMNG B~LIFNR
           INTO TABLE IT_RESB_TMP
           FROM MDSB AS A
            INNER JOIN EORD AS B
           ON A~MATNR = B~MATNR
           WHERE BDTER >= P_DATUM AND
                 A~MATNR IN S_MATNR AND
                 XLOEK = ' '
                 AND VDATU <= SY-DATUM AND
                 BDATU >= SY-DATUM.


    SORT IT_WEEK.
** Changed by Furong on 03/14/08   "UD1K943095
*    READ TABLE it_week INDEX 1.
*    SELECT matnr bdter bdmng
*           INTO TABLE it_resb_tmp1
*           FROM mdsb
*           WHERE bdter >= it_week-datum AND
*                 matnr IN s_matnr       AND
*                 xloek = ' '.


    READ TABLE IT_WEEK INDEX 4.
    L_FDATE_21D = IT_WEEK-DATUM - 1.
    READ TABLE IT_WEEK INDEX 22.
    L_LDATE_21D = IT_WEEK-DATUM.


    SELECT A~MATNR BDTER BDMNG B~LIFNR
           INTO TABLE IT_RESB_TMP1
           FROM MDSB AS A
                 INNER JOIN EORD AS B
           ON A~MATNR = B~MATNR
           WHERE A~MATNR IN S_MATNR AND
                 BDTER BETWEEN P_DATUM AND  L_FDATE_21D AND
                 XLOEK = ' '
                 AND VDATU <= SY-DATUM AND
                 BDATU >= SY-DATUM.


    SELECT A~MATNR BDTER BDMNG B~LIFNR
           APPENDING TABLE IT_RESB_TMP1
           FROM MDSM AS A
                 INNER JOIN EORD AS B
           ON A~MATNR = B~MATNR
            WHERE A~MATNR IN S_MATNR   AND
                  PLSCN = '900'  AND
                  BDTER BETWEEN L_FDATE_21D AND L_LDATE_21D
                                 AND VDATU <= SY-DATUM AND
                 BDATU >= SY-DATUM.

** End of change
    SORT IT_RESB_TMP BY MATNR BDTER.
    LOOP AT IT_RESB_TMP.
      MOVE-CORRESPONDING IT_RESB_TMP TO IT_RESB.
      COLLECT IT_RESB.
    ENDLOOP.
    SORT IT_RESB BY MATNR BDTER.
    DELETE ADJACENT DUPLICATES FROM IT_RESB COMPARING MATNR BDTER.

    SORT IT_RESB_TMP1 BY MATNR BDTER.
    LOOP AT IT_RESB_TMP1.
      MOVE-CORRESPONDING IT_RESB_TMP1 TO IT_RESB1.
      COLLECT IT_RESB1.
    ENDLOOP.
    SORT IT_RESB1 BY MATNR BDTER.
    DELETE ADJACENT DUPLICATES FROM IT_RESB1 COMPARING MATNR BDTER.

    IF IT_RESB[] IS INITIAL AND IT_RESB1[] IS INITIAL.
      MESSAGE S000 WITH TEXT-005.
      EXIT.
    ENDIF.

  ENDIF.    " IF p_d21 = 'X'.

  IF P_D21 = 'X'.
    PERFORM SET_21DAYS_DATA.
  ELSEIF P_W21 = 'X'.
    PERFORM SET_21WEEKS_DATA.
  ELSEIF P_BOTH = 'X'.
    PERFORM SET_21DAYS_DATA.
    PERFORM SET_21WEEKS_DATA.
  ENDIF.

  LOOP AT IT_21WEEK.
    MOVE-CORRESPONDING IT_21WEEK TO IT_21DAY.
    APPEND IT_21DAY.
    CLEAR  IT_21DAY.
  ENDLOOP.

  SORT IT_21DAY BY EPART_NO EDATE EDMD_TYPE.
  DELETE ADJACENT DUPLICATES FROM IT_21DAY COMPARING
  EPART_NO EDATE EDMD_TYPE.
  PERFORM UPDATE_TABLE.

ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  send_data_to_eai
*&---------------------------------------------------------------------*
*       Subroutine to call RFC to pass data to WebMethods
*----------------------------------------------------------------------*
FORM SEND_DATA_TO_EAI.
  IF P_OPT4 = 'X'.
    REFRESH: IT_21DAY.
    CLEAR  : IT_21DAY.
    IF P_D21 = 'X'.
      SELECT * FROM ZTMM_EAI_DEMAND
               INTO TABLE IT_21DAY
               WHERE EPART_NO IN S_MATNR AND
                     EDMD_TYPE = 'D'     AND
                     EDATE    = P_DATUM.
    ELSEIF P_W21 = 'X'.
      SELECT * FROM ZTMM_EAI_DEMAND
               INTO TABLE IT_21DAY
               WHERE EPART_NO IN S_MATNR AND
                     EDMD_TYPE = 'W'     AND
                     EDATE    = P_DATUM.
    ELSEIF P_BOTH = 'X'.
      SELECT * FROM ZTMM_EAI_DEMAND
               INTO TABLE IT_21DAY
               WHERE EPART_NO IN S_MATNR AND
                     EDATE    = P_DATUM.
    ENDIF.

    IF SY-SUBRC NE 0.
      MESSAGE S000 WITH TEXT-005.
      EXIT.
    ENDIF.
  ENDIF.        " IF p_opt4 = 'X'.

  IF NOT IT_21DAY[] IS INITIAL.
    DATA: L_MSGTXT(100) TYPE C   ,
          L_SIZE        TYPE NUM9.


    CALL FUNCTION 'Z_GCS_EAI_DEMAND'
        DESTINATION P_DEST
     TABLES
       EAI_DEMAND          = IT_21DAY
     EXCEPTIONS
       NO_DATA_FOUND       = 1
       OTHERS              = 2
              .
    IF SY-SUBRC <> 0.
      LOOP AT IT_21DAY.
        IT_21DAY-TAIT_TARG_RSLT = 'F'.
        MODIFY IT_21DAY INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
      ENDLOOP.
      MESSAGE S000 WITH TEXT-008.
    ELSE.
      LOOP AT IT_21DAY.
        IT_21DAY-TAIT_TARG_RSLT = 'S'.
        MODIFY IT_21DAY INDEX SY-TABIX TRANSPORTING TAIT_TARG_RSLT.
      ENDLOOP.
      DESCRIBE TABLE IT_21DAY LINES GV_LINES.
      MESSAGE S000 WITH GV_LINES TEXT-009.
    ENDIF.
    UPDATE ZTMM_EAI_DEMAND FROM TABLE IT_21DAY.
  ENDIF.    " IF NOT it_21day[] IS INITIAL

ENDFORM.                    " send_data_to_eai
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       Subroutine to show progress
*----------------------------------------------------------------------*
*      -->P_gv_text  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    P_TEXT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
*            PERCENTAGE =
            TEXT       = P_TEXT.

ENDFORM.                    " show_progress
*&---------------------------------------------------------------------*
*&      Form  set_21days_data
*&---------------------------------------------------------------------*
*       Subroutine to distribute data day wise
*----------------------------------------------------------------------*
FORM SET_21DAYS_DATA.
  DATA: L_INDEX LIKE SY-TABIX.
  DATA: L_MATNR LIKE MARA-MATNR.

  LOOP AT IT_RESB.
    CLEAR: IT_21DAY.
    IF IT_RESB-BDTER GT GV_LASTDATE.
      CONTINUE.
    ENDIF.
    READ TABLE IT_DAY WITH KEY DATUM = IT_RESB-BDTER
                      BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE IT_21DAY WITH KEY EPART_NO = IT_RESB-MATNR.

    IF SY-SUBRC EQ 0.
      MOVE: SY-TABIX TO L_INDEX.

      PERFORM APPEND_QUANTITY.

      MODIFY IT_21DAY INDEX L_INDEX.

    ELSE.
      IT_21DAY-EPART_NO = IT_RESB-MATNR.
      IT_21DAY-EDATE = P_DATUM.
      IT_21DAY-vendor = IT_RESB-lifnr.
      IT_21DAY-EDMD_TYPE = 'D'.
      IT_21DAY-TAIT_TARG_D = SY-DATUM.
      IT_21DAY-TAIT_TARG_T = SY-UZEIT.
      IT_21DAY-TAIT_TARG_RSLT = ' '.
      IT_21DAY-TAIT_TARG_DESC = ' '.
      IT_21DAY-TAIT_EVENT_C = 'I'.

      PERFORM APPEND_QUANTITY.
      APPEND IT_21DAY.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " set_21days_data
*&---------------------------------------------------------------------*
*&      Form  append_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM APPEND_QUANTITY.
  DATA: L_QUANTITY(50),
        L_DAY(2) TYPE N,
        L_BDMNG  LIKE RESB-BDMNG.

  FIELD-SYMBOLS: <QUANTITY>.

  READ TABLE IT_DAY WITH KEY DATUM = IT_RESB-BDTER BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE: IT_RESB-BDMNG TO L_BDMNG.
  ELSE.
    MOVE: 0             TO L_BDMNG.
  ENDIF.

  L_DAY = IT_DAY-SEQ.

  CONCATENATE 'IT_21DAY-ED' L_DAY '_DMD' INTO L_QUANTITY.
  ASSIGN (L_QUANTITY) TO <QUANTITY>.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  <QUANTITY> = <QUANTITY> + L_BDMNG.

ENDFORM.                    " append_quantity
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       Subroutine to update Z table
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.
  CLEAR GV_LINES.

  DESCRIBE TABLE IT_21DAY LINES GV_LINES.
  INSERT ZTMM_EAI_DEMAND FROM TABLE IT_21DAY ACCEPTING DUPLICATE KEYS.

  IF SY-SUBRC = 0.
    MESSAGE S000 WITH GV_LINES TEXT-003.
    IF P_OPT2 = 'X'.
      PERFORM DISPLAY_ALV_DATA.
    ENDIF.
  ELSE.
    MESSAGE S000 WITH TEXT-004.
  ENDIF.

ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  display_alv_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_DATA.

  IF P_OPT2 = 'X'.
    REFRESH GT_ALV.
    CLEAR   GT_ALV.

    LOOP AT IT_21DAY.
      MOVE-CORRESPONDING IT_21DAY TO GT_ALV.

      CASE IT_21DAY-TAIT_TARG_RSLT.
        WHEN SPACE.
        WHEN 'S'.
          GT_ALV-ICON = ICON_LED_GREEN.
        WHEN 'F'.
          GT_ALV-ICON = ICON_LED_RED.
      ENDCASE.
      APPEND GT_ALV.
      CLEAR  GT_ALV.

    ENDLOOP.
  ENDIF.

  PERFORM SET_LAYOUT.
  CLEAR GS_VARIANT.

  GV_REPID = SY-REPID.
  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

  PERFORM ALV_FIELDCAT.

  PERFORM ALV_GRID_DISPLAY.

ENDFORM.                    " display_alv_data
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       Set Layout
*----------------------------------------------------------------------*
FORM SET_LAYOUT.
  CLEAR GS_LAYOUT.
  GS_LAYOUT-COLWIDTH_OPTIMIZE      = 'X'.
  GS_LAYOUT-BOX_FIELDNAME          = 'CHKBOX'.
  GS_LAYOUT-COLTAB_FIELDNAME       = 'TABCOLOR'.
ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       Prepare ALV fieldcatalogue
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT.
  DATA: L_CN(2)     TYPE N,
        L_COLPOS    TYPE I,
        L_DATUM(8)        ,
        L_DATUM1(8)       ,
        L_RQTY(9)         ,
        L_TEXT(25)  TYPE C.

  CLEAR:  GS_FIELDCAT,
          GT_FIELDCAT.
  REFRESH GT_FIELDCAT.

  PERFORM BUILD_FIELD_CAT USING: '1' 'X'  'EPART_NO' 'Material'
                                                      15 'CHAR',
                                 '2' 'X'  'EDATE' 'Reqs Date'
                                                      10 'CHAR',
                                 '3' 'X'  'EDMD_TYPE'  'Demand Ident'
                                                      12 'CHAR',
                                 '4' 'X'  'VENDOR'  'Supplier'
                                                      6 'CHAR',
                                 '27' ' '  'ICON'       'flg'
                                                       3 'ICON'.
  L_COLPOS = 4.
  L_CN = '00'.
  SORT IT_WEEK.
  SORT IT_DAY.
  DO 22 TIMES.
    CLEAR GS_FIELDCAT.
    L_COLPOS = L_COLPOS + 1.
    IF P_D21 = 'X'.
      READ TABLE IT_DAY WITH KEY SEQ = L_CN.
    ELSEIF P_W21 = 'X'.
      READ TABLE IT_WEEK WITH KEY SEQ = L_CN.
    ELSEIF P_BOTH = 'X'.
      READ TABLE IT_DAY WITH KEY SEQ  = L_CN.
      READ TABLE IT_WEEK WITH KEY SEQ = L_CN.
    ENDIF.

    CONCATENATE 'ED' L_CN '_DMD' INTO L_RQTY.
    IF P_D21 = 'X'.
      WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.
    ELSEIF P_W21 = 'X'.
      WRITE IT_WEEK-DATUM TO L_DATUM MM/DD/YY.
    ELSEIF P_BOTH = 'X'.
      WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.
      WRITE IT_WEEK-DATUM TO L_DATUM1 MM/DD/YY.
      CONCATENATE L_DATUM '(D)/' L_DATUM1 '(W)' INTO L_TEXT.
    ENDIF.

    GS_FIELDCAT-COL_POS       = L_COLPOS.  " Column position
    GS_FIELDCAT-FIELDNAME     = L_RQTY.    " Field Name
    IF P_D21 = 'X' OR P_W21 = 'X'.
      GS_FIELDCAT-SELTEXT_M     = L_DATUM.   " Column heading
    ELSEIF P_BOTH = 'X'.
      GS_FIELDCAT-SELTEXT_L     = L_TEXT.    " Column heading
    ENDIF.
    GS_FIELDCAT-OUTPUTLEN     = '10'.      " Column width
    GS_FIELDCAT-DATATYPE      = 'NUMC'.    " Data type
    APPEND GS_FIELDCAT TO GT_FIELDCAT.

    CLEAR: L_RQTY.
    L_CN = L_CN + 1.

  ENDDO.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
*       Subroutine to display data in ALV Grid form
*----------------------------------------------------------------------*
FORM ALV_GRID_DISPLAY.
  DATA: LV_SAVE VALUE 'A'.
*  SORT gt_alv BY epart_no.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
*            i_callback_pf_status_set = ' '
*            i_callback_user_command  = ' '
            IS_LAYOUT                = GS_LAYOUT
*            it_excluding             =
            IT_FIELDCAT              = GT_FIELDCAT
*            it_special_groups        =
*            it_sort                  =
            I_SAVE                   = LV_SAVE
            IS_VARIANT               = GS_VARIANT
*            it_events                =
       TABLES
            T_OUTTAB                 = GT_ALV
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

ENDFORM.                    " alv_grid_display
*&---------------------------------------------------------------------*
*&      Form  build_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1141   text
*      -->P_1142   text
*      -->P_1143   text
*      -->P_1144   text
*      -->P_15     text
*      -->P_1146   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CAT USING    COL_POS KEY FIELD_NAME
                              SHORT_TXT LENGHT DATATYPE.
  GS_FIELDCAT-COL_POS       = COL_POS.
  GS_FIELDCAT-KEY           = KEY.
  GS_FIELDCAT-FIELDNAME     = FIELD_NAME.
  GS_FIELDCAT-SELTEXT_M     = SHORT_TXT.   " Column heading
  GS_FIELDCAT-OUTPUTLEN     = LENGHT.      " Column width
  GS_FIELDCAT-DATATYPE      = DATATYPE.    " Data type
*  gs_fieldcat-qfieldname    = &6.
  APPEND GS_FIELDCAT TO GT_FIELDCAT.
ENDFORM.                    " build_field_cat
*&---------------------------------------------------------------------*
*&      Form  set_week
*&---------------------------------------------------------------------*
*       Subroutine to calculate weeks
*----------------------------------------------------------------------*
FORM SET_WEEK.
  DATA: L_DATE LIKE SY-DATUM  ,
        L_DATE_1 LIKE SY-DATUM,
        L_CN(2) TYPE N.

  CLEAR: IT_WEEK, IT_WEEK[].

  CLEAR GV_KALID.
*-reading working calendar
  PERFORM READ_SHOP_CALID  USING GV_KALID.

  L_DATE = P_DATUM .
  CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
    EXPORTING
      P_PDATE             = L_DATE
   IMPORTING
     P_SUNDAY            = L_DATE
*   P_SATURDAY          =
*   P_DAY_IN_WEEK       =
*   P_WEEK_NO           =
             .
  L_DATE = L_DATE + 1.

  L_CN = '00'.

  DO 22 TIMES.
    IT_WEEK-SEQ = L_CN.
    L_DATE_1 = L_DATE.
    PERFORM READ_WORKING_DATE USING '+'  GV_KALID  L_DATE_1.
    L_DATE = L_DATE + 7.
    IF L_DATE_1 > L_DATE.
      L_DATE = L_DATE + 7.
    ELSE.
      IF L_DATE = L_DATE_1.
        L_DATE_1 = L_DATE_1 - 7.
      ENDIF.
    ENDIF.
    IT_WEEK-DATUM = L_DATE_1.
    APPEND IT_WEEK.
    CLEAR IT_WEEK.
    L_CN = L_CN + 1.
  ENDDO.

  L_DATE_1 = L_DATE.
  PERFORM READ_WORKING_DATE USING '+'  GV_KALID  L_DATE_1.
  L_DATE = L_DATE + 7.
  IF L_DATE_1 > L_DATE.
    L_DATE = L_DATE + 7.
  ELSE.
    IF L_DATE = L_DATE_1.
      L_DATE_1 = L_DATE_1 - 7.
    ENDIF.
  ENDIF.
  GV_LASTWEEK = L_DATE_1.

ENDFORM.                    " set_week
*&---------------------------------------------------------------------*
*&      Form  set_21weeks_data
*&---------------------------------------------------------------------*
*       Subroutine to distribute data week wise
*----------------------------------------------------------------------*
FORM SET_21WEEKS_DATA.
  DATA: BEGIN OF IT_MATNR OCCURS 0,
          MATNR LIKE MARA-MATNR   ,
        END OF IT_MATNR           .
  DATA: L_WEEK_F LIKE SY-DATUM    ,
        L_SEQ(2) TYPE N           ,
        L_IDX    TYPE SY-INDEX    ,
        L_TEXT_21T(30) TYPE C     ,
        L_TMP LIKE ZTMM_EAI_DEMAND-ED00_DMD.

  FIELD-SYMBOLS : <FS01>, <FS-QTY>.

  LOOP AT IT_RESB1.
    MOVE-CORRESPONDING IT_RESB1 TO IT_MATNR.
    COLLECT IT_MATNR.
  ENDLOOP.
  SORT IT_MATNR.
  SORT IT_WEEK.
  L_SEQ = '00'.

  LOOP AT IT_MATNR.
    CLEAR IT_WEEK.
    CLEAR L_IDX.
    L_SEQ = '00'.
    L_IDX = L_IDX + 1.
    READ TABLE IT_WEEK INDEX L_IDX.
    L_WEEK_F = IT_WEEK-DATUM.
    L_IDX = L_IDX + 1.
    READ TABLE IT_WEEK INDEX L_IDX.

    DO 22 TIMES.
      CLEAR L_TMP.
      LOOP AT IT_RESB1 WHERE MATNR = IT_MATNR-MATNR AND
                             BDTER >= L_WEEK_F      AND
                             BDTER < IT_WEEK-DATUM.

        CLEAR L_TEXT_21T.
        CONCATENATE 'IT_21WEEK-ED' L_SEQ '_DMD' INTO L_TEXT_21T.
        ASSIGN (L_TEXT_21T) TO <FS-QTY>.
        L_TMP = L_TMP + IT_RESB1-BDMNG.
        <FS-QTY> = L_TMP..
      ENDLOOP.  " LOOP AT it_resb1 WHERE matnr = it_matnr-matnr.
      UNASSIGN <FS-QTY>.
      L_WEEK_F = IT_WEEK-DATUM.
      L_IDX = L_IDX + 1.
      IF L_IDX = 23.
        IT_WEEK-DATUM = GV_LASTWEEK.
        L_SEQ = L_SEQ + 1.
      ELSEIF L_IDX > 23.
        EXIT.
      ELSE.
        READ TABLE IT_WEEK INDEX L_IDX.
        L_SEQ = L_SEQ + 1.
      ENDIF.
    ENDDO.

    IT_21WEEK-EPART_NO = IT_RESB1-MATNR.
    IT_21WEEK-EDATE = P_DATUM.
    IT_21WEEK-EDMD_TYPE = 'W'.
          IT_21week-vendor = IT_RESB1-lifnr.
    IT_21WEEK-TAIT_TARG_D = SY-DATUM.
    IT_21WEEK-TAIT_TARG_T = SY-UZEIT.
    IT_21WEEK-TAIT_TARG_RSLT = ' '.
    IT_21WEEK-TAIT_TARG_DESC = ' '.
    IT_21WEEK-TAIT_EVENT_C = 'I'.
    APPEND IT_21WEEK.
  ENDLOOP.

ENDFORM.                    " set_21weeks_data
