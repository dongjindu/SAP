************************************************************************
* Program Name      : ZRMMPM10R_STL_LOG
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.03.17.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K907802
* Addl Documentation:
* Description       : Supply to Line Log Display
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.03.17.     Sung-Tae Lim     UD1K907802     Initial Coding
* 2011.07.25.     Matthew Cupples  UD1K952571     Modified for Remove WM
*
*
************************************************************************

REPORT ZRMMPM10R_STL_LOG  NO STANDARD PAGE HEADING
                          LINE-SIZE 400
*                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMMM.

**---
INCLUDE : ZRMMPMXXR_INCL.


**---
DATA : BEGIN OF IT_ITAB OCCURS 0.
        INCLUDE STRUCTURE ZTMM_STL_LOG.
DATA :   MAKTX LIKE MAKT-MAKTX,
         LINECOLOR(4),
         MARK(1),
       END OF IT_ITAB.

* Log Date
DATA: FR_LOGDATE  LIKE SY-DATUM.
DATA: TO_LOGDATE  LIKE SY-DATUM.

RANGES : R_MSGTY FOR ZTMM_STL_LOG-MSGTY.

*----- BDC
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.

DATA : BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESS.

DATA : IT_MESSAGE LIKE IT_MESS OCCURS 0 WITH HEADER LINE.

DATA : W_MODE LIKE CTU_PARAMS-DISMODE VALUE 'N'.

**--- Macro
DEFINE APPEND_FIELDCAT.
  &1 = &1 + 1.
  W_FIELDCAT-COL_POS    = &1.
  W_FIELDCAT-FIELDNAME  = &2.
  W_FIELDCAT-OUTPUTLEN  = &3.
  W_FIELDCAT-SELTEXT_L  = &4.
  W_FIELDCAT-SELTEXT_M  = &4.
  W_FIELDCAT-SELTEXT_S  = &4.
  W_FIELDCAT-DATATYPE   = &5.
  W_FIELDCAT-KEY        = &6.
  W_FIELDCAT-QFIELDNAME = &7.
  W_FIELDCAT-CFIELDNAME = &8.
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
END-OF-DEFINITION.

DEFINE APPEND_TOP.
  CLEAR : W_LINE.
  IF NOT &3 IS INITIAL OR NOT &4 IS INITIAL.
    W_LINE-TYP   = &1.
    W_LINE-KEY   = &2.
    CONCATENATE &3 '~' &4 INTO W_LINE-INFO SEPARATED BY SPACE.
    APPEND W_LINE TO W_TOP_OF_PAGE.
  ENDIF.
END-OF-DEFINITION.

DEFINE APPEND_SORTCAT.
  W_SORTCAT-SPOS      = &1.
  W_SORTCAT-FIELDNAME = &2.
  W_SORTCAT-TABNAME   = &3.
  W_SORTCAT-UP        = &4.
  W_SORTCAT-SUBTOT    = &5.
  APPEND W_SORTCAT.
  CLEAR : W_SORTCAT.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT (12) TEXT-010.
SELECTION-SCREEN POSITION 33.
PARAMETERS P_SUCCE AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) TEXT-011 FOR FIELD P_SUCCE.
SELECTION-SCREEN POSITION 46.
PARAMETERS P_ERROR AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) TEXT-012 FOR FIELD P_ERROR.
SELECTION-SCREEN POSITION 58.
PARAMETERS P_SPACE AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) TEXT-013 FOR FIELD P_SPACE.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

SELECT-OPTIONS : S_SDATE   FOR ZTMM_STL_LOG-SDATE,
                 S_FEEDT   FOR ZTMM_STL_LOG-FEEDING_TIME,
                 S_DISPO   FOR ZTMM_STL_LOG-DISPO,
                 S_FEEDR   FOR ZTMM_STL_LOG-FEEDR,
                 S_MATNR   FOR ZTMM_STL_LOG-MATNR,
*                 s_tanum   FOR ztmm_stl_log-tanum,
                 S_RSNUM   FOR ZTMM_STL_LOG-RSNUM,
                 S_ERNAM   FOR ZTMM_STL_LOG-ERNAM,
                 S_ERDAT   FOR ZTMM_STL_LOG-ERDAT,
                 S_ERZET   FOR ZTMM_STL_LOG-ERZET.

SELECTION-SCREEN ULINE.

*  PARAMETERS : p_kquit AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK BLOCK1.


**---
INITIALIZATION.
  PERFORM INIT.
  PERFORM EVENT_BUILD USING W_EVENTCAT[].


**---
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


**---
START-OF-SELECTION.
  PERFORM GET_DATA.


**---
END-OF-SELECTION.
  IF IT_ITAB[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    PERFORM COMMENT_BUILD.
    PERFORM MAKE_ALV_GRID.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT.
*--- For User
  MOVE: 'I'      TO S_ERNAM-SIGN,
        'EQ'     TO S_ERNAM-OPTION,
        SY-UNAME TO S_ERNAM-LOW.
  APPEND S_ERNAM.

*--- For Log Date
  FR_LOGDATE = SY-DATUM - 1.
  TO_LOGDATE = SY-DATUM.
  MOVE: 'I'        TO S_ERDAT-SIGN,
        'BT'       TO S_ERDAT-OPTION,
        FR_LOGDATE TO S_ERDAT-LOW,
        TO_LOGDATE TO S_ERDAT-HIGH.
  APPEND S_ERDAT.
ENDFORM.                    " init

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD.
*---
  CLEAR : W_LINE.
  W_LINE-TYP  = 'H'.
  W_LINE-INFO = TEXT-002.
  APPEND W_LINE TO W_TOP_OF_PAGE.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_ALV_GRID.
*---
  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_SORTCAT.

  CLEAR : W_PROGRAM.

  MOVE : SY-REPID TO W_PROGRAM.

  MOVE : 'LINECOLOR' TO W_LAYOUT-INFO_FIELDNAME,
         'X'         TO W_LAYOUT-COLWIDTH_OPTIMIZE,
         'MARK'      TO W_LAYOUT-BOX_FIELDNAME.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = W_PROGRAM
            I_CALLBACK_PF_STATUS_SET = 'SET_STATUS'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            IS_LAYOUT                = W_LAYOUT
            IT_FIELDCAT              = W_FIELDCAT[]
            IT_EVENTS                = W_EVENTCAT[]
            IT_SORT                  = W_SORTCAT[]
            I_SAVE                   = 'A'
       TABLES
            T_OUTTAB                 = IT_ITAB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
**  append_fieldcat :
**    w_col_pos 'LOGNO_H'   10 'Log Number'     'CHAR' 'X' ''      '',
**    w_col_pos 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
**    w_col_pos 'MAKTX'     30 'Material Desc'  'CHAR' 'X' ''      '',
**    w_col_pos 'SDATE'     10 'TO S/Date'      'DATS' ''  ''      '',
**    w_col_pos 'FEEDING_TIME'  08 'TO S/Time'  'TIMS' ''  ''      '',
**    w_col_pos 'EDATE'     10 'TO E/Date'      'DATS' ''  ''      '',
**    w_col_pos 'ETIME'     08 'TO E/Time'      'TIMS' ''  ''      '',
**    w_col_pos 'BDMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
**    w_col_pos 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
**    w_col_pos 'WORKS'     05 'Workstation'    'CHAR' ''  ''      '',
**    w_col_pos 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
**    w_col_pos 'STOCK_CHECK'  01 'Stock Check' 'CHAR' ''  ''      '',
**    w_col_pos 'FEED_CYCLE'   04 'Feed Cycle'  'NUMC' ''  'MEINS' '',
**    w_col_pos 'ZTIME'     03 'Time for STL'   'NUMC' ''  ''      '',
**    w_col_pos 'VERME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
**    w_col_pos 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
**    w_col_pos 'OPEN_TO'   12 'Open TO'        'QUAN' ''  'MEINS' '',
**    w_col_pos 'BFERRORQTY' 12 'B/F Error'     'QUAN' ''  'MEINS' '',
**    w_col_pos 'BF_TO_QTY' 12 'to B/F Qty'     'QUAN' ''  'MEINS' '',
**    w_col_pos 'NSOLA'     12 'Prev Qty'       'QUAN' ''  'MEINS' '',
**    w_col_pos 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
**    w_col_pos 'TQTY'      12 'TO Qty'         'QUAN' ''  'MEINS' '',
**    w_col_pos 'FEEDR'     05 'Feeder'         'CHAR' ''  ''      '',
**    w_col_pos 'TANUM'     10 'TO Number'      'CHAR' ''  ''      '',
**    w_col_pos 'SRC_LGTYP' 03 'Src S/Type'     'CHAR' ''  ''      '',
**    w_col_pos 'SRC_LGPLA' 10 'Src S/Bin'      'CHAR' ''  ''      '',
**    w_col_pos 'DES_LGTYP' 03 'Des S/Type'     'CHAR' ''  ''      '',
**    w_col_pos 'DES_LGPLA' 10 'Des S/Bin'      'CHAR' ''  ''      '',
**    w_col_pos 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
***    w_col_pos 'ERDAT'     10 'Date'           'DATS' ''  ''      '',
***    w_col_pos 'ERZET'     10 'Time'           'TIMS' ''  ''      '',
***    w_col_pos 'ERNAM'     10 'User'           'CHAR' ''  ''      ''.

**S__PAUL FROM ZMME_S2L_HOURLY : 06/20/11.
  APPEND_FIELDCAT :
    W_COL_POS 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    W_COL_POS 'BDMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
    W_COL_POS 'DES_LGPLA' 05 'Workstation'    'CHAR' ''  ''      '',
**Changed by Matthew Cupples 07/25/2011
*    W_COL_POS 'ZRHLH'     02 'RH/LH'          'CHAR' ''  ''      '',
    W_COL_POS 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
    W_COL_POS 'SDATE'     10 'TO S/Date'      'DATS' ''  ''      '',
    W_COL_POS 'FEEDING_TIME' 10 'Starting Time'
                                              'TIMS' ''  ''      '',
    W_COL_POS 'EDATE'     10 'TO E/Date'      'DATS' ''  ''      '',
**Changed by Matthew Cupples 07/25/2011
*    W_COL_POS 'LABST'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    W_COL_POS 'VERME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    W_COL_POS 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    W_COL_POS 'BFERRORQTY' 12 'B/F Error'     'QUAN' ''  'MEINS' '',
    W_COL_POS 'BF_TO_QTY' 12 'to B/F Qty'     'QUAN' ''  'MEINS' '',
    W_COL_POS 'NSOLA'     12 'Prev Qty'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
**Changed by Matthew Cupples 07/25/2011
*    W_COL_POS 'ZFEEDER'   05 'Feeder'         'CHAR' ''  ''      '',
    W_COL_POS 'FEEDR'   05 'Feeder'         'CHAR' ''  ''      '',
**Changed by Matthew Cupples 07/25/2011
*    W_COL_POS 'OPEN_QTY'   12 'Open Res'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'OPEN_TO'   12 'Open Res'       'QUAN' ''  'MEINS' '',
    W_COL_POS 'TQTY'       12 'Res Qty'        'QUAN' ''  'MEINS' '',
    W_COL_POS 'RSNUM'      10 'Res. Number'    'CHAR' ''  ''      '',
**Changed by Matthew Cupples 07/25/2011
*    W_COL_POS 'SRC_SLOC'   04 'Src S/Loc'      'CHAR' ''  ''      '',
    W_COL_POS 'SRC_LGORT'   04 'Src S/Loc'      'CHAR' ''  ''      '',
    W_COL_POS 'DES_LGORT'  04 'Des S/Loc'      'CHAR' ''  ''      '',
    W_COL_POS 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
**E__PAUL

ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  APPEND_SORTCAT : '1' 'LOGNO_H' 'IT_ITAB' 'X' '',
                   '2' 'MATNR'   'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
*---
  PERFORM MAKE_MESSAGE_TYPE_RANGES.

  CLEAR : IT_ITAB, IT_ITAB[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
           FROM ZTMM_STL_LOG
          WHERE ERNAM            IN S_ERNAM
            AND ERDAT            IN S_ERDAT
            AND ERZET            IN S_ERZET
            AND MSGTY            IN R_MSGTY
            AND MATNR            IN S_MATNR
**s__paul Change remove p_kquit
*            AND tanum            IN s_tanum
            AND RSNUM            IN S_RSNUM
**e__06/20/11
            AND SDATE            IN S_SDATE
            AND FEEDING_TIME     IN S_FEEDT
            AND DISPO            IN S_DISPO
            AND FEEDR            IN S_FEEDR.

*---
**s__paul Change remove p_kquit
**  IF p_kquit NE space.
**S_PAUL change : 071111
**    DELETE it_itab WHERE rsnum EQ space.
**    LOOP AT it_itab.
**      CLEAR : ltak.
**      SELECT SINGLE kquit INTO ltak-kquit
**                          FROM ltak
**                         WHERE tanum EQ it_itab-tanum
**                           AND kquit NE space.
**      IF sy-subrc NE 0.
**        DELETE it_itab.
**      ENDIF.
**    ENDLOOP.
**  ENDIF.
**e__06/20/11
*---
  DATA : L_TABIX LIKE SY-TABIX.

  LOOP AT IT_ITAB.     " WHERE msgty NE space.
    MOVE : SY-TABIX TO L_TABIX.
    IF IT_ITAB-MSGTY EQ 'E'.
      MOVE : C_RED               TO IT_ITAB-LINECOLOR.
    ELSEIF IT_ITAB-MSGTY EQ 'S'.
      MOVE : C_GREEN             TO IT_ITAB-LINECOLOR.
    ENDIF.
    IF IT_ITAB-CANCL EQ 'X'.
      MOVE : C_YELL              TO IT_ITAB-LINECOLOR.
    ENDIF.
    PERFORM GET_MATERIAL_DESC USING IT_ITAB-MATNR.
    MOVE : MAKT-MAKTX TO IT_ITAB-MAKTX.
    MODIFY IT_ITAB INDEX L_TABIX.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  make_message_type_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_MESSAGE_TYPE_RANGES.
*---
  CLEAR : R_MSGTY, R_MSGTY[].

  IF P_SUCCE NE SPACE.
    MOVE : 'I'     TO R_MSGTY-SIGN,
           'EQ'    TO R_MSGTY-OPTION,
           'S'     TO R_MSGTY-LOW.
    APPEND R_MSGTY.
  ENDIF.

  IF P_ERROR NE SPACE.
    MOVE : 'I'     TO R_MSGTY-SIGN,
           'EQ'    TO R_MSGTY-OPTION,
           'E'     TO R_MSGTY-LOW.
    APPEND R_MSGTY.
  ENDIF.

  IF P_SPACE NE SPACE.
    MOVE : 'I'     TO R_MSGTY-SIGN,
           'EQ'    TO R_MSGTY-OPTION,
           ' '     TO R_MSGTY-LOW.
    APPEND R_MSGTY.
  ENDIF.
ENDFORM.                    " make_message_type_ranges

*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM SET_STATUS USING EXTAB TYPE SLIS_T_EXTAB.
*---
  SET PF-STATUS 'BASE'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM
                       SELFIELD TYPE SLIS_SELFIELD.
*---
  DATA : L_TANUM LIKE LTAK-TANUM,
         L_RSNUM LIKE PKPS-RSNUM,
         L_ANSWER(1).

  READ TABLE IT_ITAB INDEX SELFIELD-TABINDEX.

  CASE UCOMM.
    WHEN 'EXEC'.
      READ TABLE IT_ITAB WITH KEY MARK = 'X'.
      IF SY-SUBRC EQ 0.
        PERFORM CONFIRM_STEP USING L_ANSWER TEXT-004.
        IF L_ANSWER EQ 'J'.
          MOVE : 'X' TO SELFIELD-REFRESH.
*          PERFORM create_transfer_order.
*           PERFORM CREATE_S2L_ORDER.
        ENDIF.
      ELSE.
        MESSAGE E999 WITH TEXT-M03.
      ENDIF.
    WHEN 'CANCEL'.
      READ TABLE IT_ITAB WITH KEY MARK = 'X'.
      IF SY-SUBRC EQ 0.
        PERFORM CONFIRM_STEP USING L_ANSWER TEXT-005.
        IF L_ANSWER EQ 'J'.
          MOVE : 'X' TO SELFIELD-REFRESH.
*          PERFORM CANCEL_TRANSFER_ORDER.
          PERFORM CANCEL_S2L_ORDER.
        ENDIF.
      ELSE.
        MESSAGE E999 WITH TEXT-M03.
      ENDIF.
    WHEN '&IC1'.
      CHECK SY-SUBRC EQ 0.
**c__ PAUL CHANGE 06/20/11
      CLEAR : L_RSNUM.
      MOVE : IT_ITAB-RSNUM TO L_RSNUM.
      SET PARAMETER ID 'RES' FIELD L_RSNUM.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.
      CLEAR : IT_ITAB.
*      CLEAR : l_tanum.
*      MOVE : it_itab-tanum TO l_tanum.
*      SET PARAMETER ID 'TAN' FIELD l_tanum.
*      SET PARAMETER ID 'LGN' FIELD 'P01'.
*      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.
*      CLEAR : it_itab.
  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  confirm_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM CONFIRM_STEP USING    P_L_ANSWER
                           P_TEXT.
*---
  CLEAR : P_L_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      DEFAULTOPTION        = 'Y'
      TEXTLINE1            = P_TEXT
*     TEXTLINE2            = ' '
      TITEL                = TEXT-003
      START_COLUMN         = 25
      START_ROW            = 6
      CANCEL_DISPLAY       = 'X'
    IMPORTING
      ANSWER               = P_L_ANSWER.
ENDFORM.                    " confirm_step

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
**----------------------------------------------------------------------
***
**FORM CREATE_TRANSFER_ORDER.
*---
**  DATA : LV_BWLVS_002     TYPE BDCDATA-FVAL,   "Movement type
**         LV_MATNR_003     TYPE BDCDATA-FVAL,
**         LV_ANFME_004     TYPE BDCDATA-FVAL,
**         LV_ANFME_007     TYPE BDCDATA-FVAL,
**         LV_ALTME_008     TYPE BDCDATA-FVAL,
**         LV_VLTYP_009     TYPE BDCDATA-FVAL,
**         LV_VLPLA_010     TYPE BDCDATA-FVAL,
**         LV_NLTYP_011     TYPE BDCDATA-FVAL,
**         LV_NLPLA_012     TYPE BDCDATA-FVAL,
**         LV_REFNR_013     TYPE BDCDATA-FVAL.   "Group(Feeder)
**
**  DATA : LV_TANUM_001     TYPE BDCDATA-FVAL,  "TO number
**         LV_LGNUM_002     TYPE BDCDATA-FVAL,  "Warehouse number
**         LV_STDAT_003     TYPE BDCDATA-FVAL,  "Start date
**         LV_STUZT_004     TYPE BDCDATA-FVAL,  "Start time
**         LV_ENDAT_005     TYPE BDCDATA-FVAL,  "End date
**         LV_ENUZT_006     TYPE BDCDATA-FVAL.  "End time
**
**  DATA : L_SUBRC LIKE SY-SUBRC,
**         L_TABIX LIKE SY-TABIX,
**         L_MESSA(80).
**
**  CLEAR : IT_MESSAGE, IT_MESSAGE[].
**
***---
**  LOOP AT IT_ITAB WHERE MARK NE SPACE
**                    AND MSGTY EQ 'E'
**                    AND ( TANUM IS INITIAL AND CANCL EQ SPACE
**                       OR NOT TANUM IS INITIAL AND CANCL NE SPACE ).
**    MOVE : SY-TABIX TO L_TABIX.
**
**    CLEAR : IT_MESS, IT_MESS[].
**
**    IF IT_ITAB-STATS NE 'H'.     " except header change error item
**
**      LV_BWLVS_002 = '850'.
**      LV_REFNR_013 =  IT_ITAB-FEEDR. "Group(Feeder)
**      LV_MATNR_003  = IT_ITAB-MATNR. "Material '327003K100'
**      LV_ANFME_004  = IT_ITAB-TQTY.
**      LV_ANFME_007  = IT_ITAB-TQTY.
**      LV_ALTME_008  = IT_ITAB-MEINS.
**      LV_VLTYP_009  = IT_ITAB-SRC_LGTYP. "Src Storage Type
**      LV_VLPLA_010  = IT_ITAB-SRC_LGPLA. "Src Storage Bin
**      LV_NLTYP_011  = IT_ITAB-DES_LGTYP. "Des Storage Type
**      LV_NLPLA_012  = IT_ITAB-DES_LGPLA. "Des Storage Bin
**
**      CONDENSE : LV_BWLVS_002,  "Movement type
**                 LV_MATNR_003,
**                 LV_ANFME_004,
**                 LV_ANFME_007,
**                 LV_ALTME_008,
**                 LV_VLTYP_009,
**                 LV_VLPLA_010,
**                 LV_NLTYP_011,
**                 LV_NLPLA_012,
**                 LV_REFNR_013.
**
***--- BDC for LT01(Create TO)
**      CALL FUNCTION 'Z_FMM_6012_01'
**           EXPORTING
**                LGNUM_001 = 'P01'  "Warehouse number
**                REFNR_013 = LV_REFNR_013  "Group(Feeder)
**                BWLVS_002 = LV_BWLVS_002  "Movement type '999'
**                MATNR_003 = LV_MATNR_003  "Material '327003K100'
**                ANFME_004 = LV_ANFME_004
**                WERKS_005 = 'P001'  "Plant
**                LGORT_006 = 'P400'  "Storage Location
**                ANFME_007 = LV_ANFME_007
**                ALTME_008 = LV_ALTME_008
**                VLTYP_009 = LV_VLTYP_009  "Src Storage Type '434'
**                VLPLA_010 = LV_VLPLA_010  "Src Storage Bin 'AA-01-11'
**                NLTYP_011 = LV_NLTYP_011  "Des Storage Type '443'
**                NLPLA_012 = LV_NLPLA_012  "Des Storage Bin 'TS-01'
**           IMPORTING
**                SUBRC     = L_SUBRC
**           TABLES
**                MESSTAB   = IT_MESS[].
**
**      APPEND LINES OF IT_MESS TO IT_MESSAGE.
**
**    ENDIF.
**
**    IF L_SUBRC EQ 0.
**      CLEAR : IT_MESS, L_SUBRC.
**      READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.
**      IF SY-SUBRC EQ 0.
**        LV_TANUM_001 = IT_MESS-MSGV1.
**        LV_LGNUM_002 = 'P01'.
**        WRITE : IT_ITAB-SDATE TO LV_STDAT_003,
**                IT_ITAB-EDATE TO LV_ENDAT_005.
**        LV_STUZT_004 = IT_ITAB-FEEDING_TIME.
**        LV_ENUZT_006 = IT_ITAB-ETIME.
**
**        CONDENSE : LV_TANUM_001,
**                   LV_LGNUM_002,
**                   LV_STDAT_003,
**                   LV_STUZT_004,
**                   LV_ENDAT_005,
**                   LV_ENUZT_006.
**
***--- BDC for LTA1(Change TO Header)
**        CALL FUNCTION 'Z_FMM_6012_02'
**             EXPORTING
**                  TANUM_001 = LV_TANUM_001
**                  LGNUM_002 = LV_LGNUM_002
**                  STDAT_003 = LV_STDAT_003
**                  STUZT_004 = LV_STUZT_004
**                  ENDAT_005 = LV_ENDAT_005
**                  ENUZT_006 = LV_ENUZT_006
**             IMPORTING
**                  SUBRC     = L_SUBRC
**             TABLES
**                  MESSTAB   = IT_MESS[].
**
**        IF IT_MESS[] IS INITIAL.     " success
**          CLEAR : IT_MESS.
**          IT_MESS-TCODE   = 'LT1A'.
**          IT_MESS-MSGTYP  = 'S'.  "SUCCESS
**          IT_MESS-MSGSPRA = 'E'.
**          IT_MESS-MSGID   = 'ZMMM'.
**          IT_MESS-MSGNR   = '999'.
**          IT_MESS-MSGV1   = 'Transfer order'.
**          IT_MESS-MSGV2   = LV_TANUM_001.
**          IT_MESS-MSGV3   = 'Start/End Date/Time'.
**          IT_MESS-MSGV4   = 'is changed.'.
**          APPEND IT_MESS.
**        ENDIF.
**
**        IF L_SUBRC EQ 0.
**          CLEAR : IT_MESS.
**          READ TABLE IT_MESS INDEX 1.
**          MOVE : IT_MESS-MSGV2 TO IT_ITAB-TANUM.
**          MOVE : 'S'           TO IT_ITAB-MSGTY,
**                 IT_MESS-MSGV1 TO IT_ITAB-TANUM.
**          MOVE : 'C'           TO IT_ITAB-STATS,
**                 SPACE         TO IT_ITAB-CANCL.
**        ELSE.
**        ENDIF.
**      ENDIF.
**
**      CLEAR : IT_MESS, L_MESSA.
**      READ TABLE IT_MESS WITH KEY MSGTYP = 'E'.
**      IF SY-SUBRC EQ 0.
**        PERFORM GET_MESSAGE USING    IT_MESS-MSGID
**                                     IT_MESS-MSGNR
**                                     IT_MESS-MSGV1
**                                     IT_MESS-MSGV2
**                                     IT_MESS-MSGV3
**                                     IT_MESS-MSGV4
**                            CHANGING L_MESSA.
**      ELSE.
**        READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.
**        IF SY-SUBRC EQ 0.
**          PERFORM GET_MESSAGE USING    IT_MESS-MSGID
**                                       IT_MESS-MSGNR
**                                       IT_MESS-MSGV1
**                                       IT_MESS-MSGV2
**                                       IT_MESS-MSGV3
**                                       IT_MESS-MSGV4
**                              CHANGING L_MESSA.
**        ENDIF.
**      ENDIF.
**
**    ELSE.
**      CLEAR : IT_MESS, L_MESSA.
**      READ TABLE IT_MESS WITH KEY MSGTYP = 'E'.
**      IF SY-SUBRC EQ 0.
**        PERFORM GET_MESSAGE USING    IT_MESS-MSGID
**                                     IT_MESS-MSGNR
**                                     IT_MESS-MSGV1
**                                     IT_MESS-MSGV2
**                                     IT_MESS-MSGV3
**                                     IT_MESS-MSGV4
**                            CHANGING L_MESSA.
**      ELSE.
**        READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.
**        IF SY-SUBRC EQ 0.
**          PERFORM GET_MESSAGE USING    IT_MESS-MSGID
**                                       IT_MESS-MSGNR
**                                       IT_MESS-MSGV1
**                                       IT_MESS-MSGV2
**                                       IT_MESS-MSGV3
**                                       IT_MESS-MSGV4
**                              CHANGING L_MESSA.
**        ENDIF.
**      ENDIF.
**    ENDIF.
**
**    MOVE : L_MESSA       TO IT_ITAB-MESSA,
**           IT_MESS-MSGID TO IT_ITAB-MSGID,
**           IT_MESS-MSGNR TO IT_ITAB-MSGNR.
**
**    IT_ITAB-AENAM = SY-UNAME.
**    IT_ITAB-AEDAT = SY-DATUM.
**    IT_ITAB-AEZET = SY-UZEIT.
**
**    MODIFY IT_ITAB INDEX L_TABIX.
**
**    CLEAR : ZTMM_STL_LOG.
**    MOVE-CORRESPONDING IT_ITAB TO ZTMM_STL_LOG.
**    MODIFY ZTMM_STL_LOG.
**    MESSAGE S999 WITH L_MESSA.
**  ENDLOOP.
**ENDFORM.                    " create_transfer_order

*&---------------------------------------------------------------------*
*&      Form  cancel_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
**FORM CANCEL_TRANSFER_ORDER.
*---
**  DATA : L_MESSA(100),
**         L_TABIX LIKE SY-TABIX.
**
**  LOOP AT IT_ITAB WHERE MARK NE SPACE
**                    AND MSGTY EQ 'S'
**                    AND TANUM NE SPACE.
**
**    MOVE : SY-TABIX TO L_TABIX.
**
**    CLEAR : IT_BDC, IT_BDC[], L_MESSA, LTAK.
**
***    SELECT SINGLE kquit INTO ltak-kquit
***                        FROM ltak
***                       WHERE tanum EQ it_itab-tanum
***                         AND kquit NE space.
***
***    IF sy-subrc EQ 0.
***
***      MESSAGE s999 WITH text-m04 it_itab-tanum.
***
***    ELSE.
**
**    PERFORM DYNPRO USING : 'X'  'SAPML03T'          '0118',
**                           ' '  'LTAK-TANUM'        IT_ITAB-TANUM,
**                           ' '  'LTAK-LGNUM'        'P01',
**                           ' '  'RL03T-RHELL'       SPACE,
**                           ' '  'RL03T-RDNKL'       'X',
**                           ' '  'BDC_OKCODE'        '/00'.
**
**    CALL TRANSACTION 'LT15' USING IT_BDC
**                            MODE W_MODE
**                            UPDATE 'S'
**                            MESSAGES INTO IT_MESS.
**
**    APPEND LINES OF IT_MESS TO IT_MESSAGE.
**
**    READ TABLE IT_MESS INDEX 1.
**
**    PERFORM GET_MESSAGE USING    IT_MESS-MSGID
**                                 IT_MESS-MSGNR
**                                 IT_MESS-MSGV1
**                                 IT_MESS-MSGV2
**                                 IT_MESS-MSGV3
**                                 IT_MESS-MSGV4
**                        CHANGING L_MESSA.
**
**    READ TABLE IT_MESS WITH KEY MSGTYP = 'S'.
**    IF SY-SUBRC EQ 0.
**      MOVE : 'E'           TO IT_ITAB-MSGTY,
**             SPACE         TO IT_ITAB-STATS,
**             'X'           TO IT_ITAB-CANCL.
**      MOVE : L_MESSA       TO IT_ITAB-MESSA,
**             IT_MESS-MSGID TO IT_ITAB-MSGID,
**             IT_MESS-MSGNR TO IT_ITAB-MSGNR.
**      IT_ITAB-AENAM = SY-UNAME.
**      IT_ITAB-AEDAT = SY-DATUM.
**      IT_ITAB-AEZET = SY-UZEIT.
**    ENDIF.
**
**    MODIFY IT_ITAB INDEX L_TABIX.
**
**    CLEAR : ZTMM_STL_LOG.
**
**    MOVE-CORRESPONDING IT_ITAB TO ZTMM_STL_LOG.
**    MODIFY ZTMM_STL_LOG.
**
**    MESSAGE S999 WITH L_MESSA.
**
***    ENDIF.
**  ENDLOOP.
**ENDFORM.                    " cancel_transfer_order

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1117   text
*      -->P_1118   text
*      -->P_1119   text
*----------------------------------------------------------------------*
FORM DYNPRO USING    DYNBEGIN
                     NAME
                     VALUE.
*---
  IF DYNBEGIN = 'X'.
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-PROGRAM,
           VALUE TO IT_BDC-DYNPRO,
           'X'   TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE .
    CLEAR : IT_BDC.
    MOVE : NAME  TO IT_BDC-FNAM,
           VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE USING    P_MSGID
                          P_MSGNR
                          P_MSGV1
                          P_MSGV2
                          P_MSGV3
                          P_MSGV4
                 CHANGING P_L_MESSA.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = P_MSGID
            MSGNR               = P_MSGNR
            MSGV1               = P_MSGV1
            MSGV2               = P_MSGV2
            MSGV3               = P_MSGV3
            MSGV4               = P_MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = P_L_MESSA.
ENDFORM.                    " get_message
*&---------------------------------------------------------------------*
*&      Form  CREATE_S2L_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_S2L_ORDER.
****---
***  DATA : L_TABIX LIKE SY-TABIX,
***         L_MESSA(80).
***  DATA : LW_BEGZT(14).
***  DATA : L_DATUM TYPE D,
***         LV_DATE(8).
***  DATA : L_STATS LIKE ZTMM_STL_LOG-STATS.
****BAPI
***  DATA: LS_REQUESTED LIKE BAPI1172_REQUESTED_QTY,
***        LS_DELIVERYTIME LIKE BAPI1172_DELVRYTIME,
***        LT_RETURN  TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
***        LT_RESULT LIKE BAPI1075_3 OCCURS 0 WITH HEADER LINE,
***        LS_RETURN LIKE BAPIRET2.
***
***  DATA : IT_M039 LIKE ZMMT0039 OCCURS 0 WITH HEADER LINE.
***
***  CLEAR : IT_ITAB, IT_ITAB[], IT_DISPO, IT_DISPO[],
***          IT_M039, IT_M039[].
***
***  LOOP AT IT_TOLINE.
***    CLEAR : L_STATS.
***    MOVE : SY-TABIX TO L_TABIX.
***
****--- TO GET Start Date(Feeding Date) and Start Time
***    MOVE : W_SCREEN_DATE   TO IT_TOLINE-SDATE,
***           W_STARTING_TIME TO IT_TOLINE-FEEDING_TIME,
***           W_ENDING_DATE   TO IT_TOLINE-EDATE,
***           W_ENDING_TIME   TO IT_TOLINE-ETIME.
***    CLEAR : W_MINUS_STOCK_CHECK.
***
***    IF W_MINUS_STOCK_CHECK EQ SPACE.
***
***      CLEAR : IT_CC.
***      READ TABLE IT_CC WITH KEY MATNR = IT_TOLINE-MATNR.
***   							
***      LS_REQUESTED-REQUESTED_QTY = IT_TOLINE-TQTY.
***      LS_REQUESTED-BASE_UOM      = IT_TOLINE-MEINS.
***      LS_DELIVERYTIME-DELVRYDATE = SY-DATUM.
***      LS_DELIVERYTIME-DELVRYTIME = P_DTIME.
***
***      CALL FUNCTION 'BAPI_KANBANCC_ADDEVENTDRKANBAN'
***     	EXPORTING
***       	KANBANCONTROLCYCLE = IT_CC-PKNUM
***            REQUESTED_QTY      = LS_REQUESTED
***            DELIVERYTIME       = LS_DELIVERYTIME
***       IMPORTING
***            RETURN             = LS_RETURN
***       TABLES
***            STATUSCHANGERESULT = LT_RESULT
***       EXCEPTIONS
***             OTHERS             = 1.
***
***      IF LS_RETURN-TYPE EQ 'E'.
***        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
***      ELSE.
***        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
***             EXPORTING
***                  	WAIT = 'X'.
***
***        LOOP AT LT_RESULT.
***
****          IT_M039-PKKEY  = LT_RESULT-KANBAN_ID.
****          IT_M039-RSNUM  = LT_RESULT-RESERV_NO.
****          IT_M039-RSPOS  = '0001'.                 "Reservation Item
****          IT_M039-BUDAT  = IT_TOLINE-EDATE.        "Posting Date
****          IT_M039-PKTIM  = IT_TOLINE-FEEDING_TIME. "Request Time
****          IT_M039-MATNR  = LT_RESULT-MATERIAL.     "Material Number
****          IT_M039-PRVBE  = LT_RESULT-SUPPLYAREA.   "Supply Area
****          IT_M039-ABLAD  = IT_CC-ABLAD.            "Workstation
****          IT_M039-PKBMG  = LT_RESULT-KANBAN_QTY.  "Requirements
****          IT_M039-MEINS  = LT_RESULT-BASE_UOM.     "Unit of Measure
****          IT_M039-WERKS  = LT_RESULT-PLANT.        "Plant
****          IT_M039-REVERSED = "".                   "Reversed
****          IT_M039-LGORT  = LS_HEADER-MOVE_STLOC.   "Storage Location
****          IT_M039-LGPRO  = IT_DEST-LGORT.       "Issue Storage
***Location
****          IT_M039-ZFEEDER  = IT_CC-ZFEEDER.          "Goods
*Recipient
***
****          APPEND IT_M039.
***          MOVE-CORRESPONDING IT_TOLINE TO IT_ITAB.
***
***          IT_ITAB-TQTY  = LT_RESULT-KANBAN_QTY.
***          IT_ITAB-RDMNG = LT_RESULT-KANBAN_QTY.
***          IT_ITAB-ZRHLH = IT_TOLINE-ZRHLH.
***
***          PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09     "NRO
***Interval
***                                           W_NRO_OBJECT    "NRO
*Object
***                                  CHANGING W_ZDOCNO.     "App Doc No
***
***          MOVE : IT_DEST-LGORT         TO IT_ITAB-DES_LGORT.
***          MOVE : IT_CC-UMLGO           TO IT_ITAB-SRC_SLOC.
***          MOVE : W_ZDOCNO              TO IT_ITAB-W_DOCNO.
***          MOVE : W_NSOLA               TO IT_ITAB-NSOLA.
***          MOVE : L_STATS               TO IT_ITAB-STATS.
***
***          CASE LS_RETURN-TYPE.
***            WHEN 'E'.
***              	MOVE : C_RED TO IT_ITAB-LINECOLOR,
***              		'E'    TO IT_ITAB-MSGTY.
***              MOVE : LS_RETURN-MESSAGE TO IT_ITAB-MESSA.
***              MOVE : LS_RETURN-ID      TO IT_ITAB-MSGID,
***            	   LS_RETURN-NUMBER  TO IT_ITAB-MSGNR.
***            WHEN 'S' OR ''.
***              	MOVE : C_GREEN  TO IT_ITAB-LINECOLOR,
***              		 'S'      TO IT_ITAB-MSGTY.
***              	MOVE : LT_RESULT-RESERV_NO TO IT_ITAB-RSNUM.
***            WHEN OTHERS.
***
***          ENDCASE.
***
***          APPEND IT_ITAB.
***          CLEAR : IT_ITAB, IT_M039.
***
***        ENDLOOP.
***      ENDIF.
***
***    ELSE.
***
***      MODIFY IT_TOLINE INDEX L_TABIX.
***
***      MOVE-CORRESPONDING IT_TOLINE TO IT_ITAB.
***
***      PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09     "NRO Interval
***                                       W_NRO_OBJECT    "NRO Object
***                              CHANGING W_ZDOCNO.     "App Doc No
***
***      MOVE : IT_DEST-LGORT         TO IT_ITAB-DES_LGORT.
***      MOVE : IT_CC-UMLGO           TO IT_ITAB-SRC_SLOC.
***      MOVE : W_ZDOCNO              TO IT_ITAB-W_DOCNO.
***      MOVE : W_NSOLA               TO IT_ITAB-NSOLA.
***      MOVE : L_STATS               TO IT_ITAB-STATS.
***
***      APPEND IT_ITAB.
***    ENDIF.
***    MOVE : IT_TOLINE-DISPO TO IT_DISPO-DISPO.
***    COLLECT IT_DISPO.
***    CLEAR : IT_TOLINE, IT_ITAB, IT_DISPO, IT_M039,
***            LS_REQUESTED, LS_DELIVERYTIME, LS_RETURN,
***            LT_RETURN[], LT_RETURN, LT_RESULT, LT_RESULT[].
***  ENDLOOP.

ENDFORM.                    " CREATE_S2L_ORDER
*&---------------------------------------------------------------------*
*&      Form  CANCEL_S2L_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CANCEL_S2L_ORDER.
  DATA : ZRESB LIKE RESB OCCURS 0 WITH HEADER LINE,
         L_TABIX LIKE SY-TABIX.

  LOOP AT IT_ITAB WHERE MARK NE SPACE
                        AND MSGTY EQ 'S'
                        AND RSNUM NE SPACE.

    L_TABIX = SY-TABIX.

    CLEAR : ZRESB, ZRESB[].

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ZRESB
      FROM RESB
     WHERE RSNUM EQ IT_ITAB-RSNUM
       AND RSPOS EQ '0001'.

    IF SY-SUBRC NE 0.
      MESSAGE W013 WITH 'S2L Order'.
      CONTINUE.
    ELSE.
      ZRESB-XLOEK = 'X'.
      APPEND ZRESB.

      CALL FUNCTION 'MB_CHANGE_RESERVATION'
           EXPORTING
                CHANGE_RESB = 'X'
                CHANGE_RKPF = ''
                NEW_RESB    = ''
           TABLES
                XRESB       = ZRESB.

      IF SY-SUBRC EQ 0.
        MOVE : 'E'           TO IT_ITAB-MSGTY,
               SPACE         TO IT_ITAB-STATS,
               'X'           TO IT_ITAB-CANCL.
        MOVE : 'Canceled'    TO IT_ITAB-MESSA.
*               IT_MESS-MSGID TO IT_ITAB-MSGID,
*               IT_MESS-MSGNR TO IT_ITAB-MSGNR.
        IT_ITAB-AENAM = SY-UNAME.
        IT_ITAB-AEDAT = SY-DATUM.
        IT_ITAB-AEZET = SY-UZEIT.
      ENDIF.

      MODIFY IT_ITAB INDEX L_TABIX.

      CLEAR : ZTMM_STL_LOG.

      MOVE-CORRESPONDING IT_ITAB TO ZTMM_STL_LOG.
      MODIFY ZTMM_STL_LOG.

    ENDIF.

  ENDLOOP.


ENDFORM.                    " CANCEL_S2L_ORDER
