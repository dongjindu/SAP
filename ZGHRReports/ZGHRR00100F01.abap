*&---------------------------------------------------------------------*
*&  Include           ZGHRR00100F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INITIAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INITIAL_VALUE .

  DATA: L_BEGDA(10),
            L_ENDDA(10).



  CONCATENATE '/1PAPAXX/HDR_' SY-MANDT  '60A                     0100SUBSCR_HEADER' INTO G_FVAL.


  CONCATENATE SY-DATUM(4) SY-DATUM+4(2) '01'
   INTO P_BEGDA.

*  CALL FUNCTION 'SLS_MISC_CONVERT_TO_DATE'
*    EXPORTING
*      p_date                        = p_begda
*    IMPORTING
*      p_date_string                 = l_begda
*    EXCEPTIONS
*      error_selecting_user_defaults = 1
*      OTHERS                        = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = P_BEGDA
    IMPORTING
      LAST_DAY_OF_MONTH = P_ENDDA
    EXCEPTIONS
      DAY_IN_NO_DATE    = 1
      OTHERS            = 2.


*  CALL FUNCTION 'SLS_MISC_CONVERT_TO_DATE'
*    EXPORTING
*      p_date                        = p_endda
*    IMPORTING
*      p_date_string                 = l_endda
*    EXCEPTIONS
*      error_selecting_user_defaults = 1
*      OTHERS                        = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  REPLACE ALL OCCURRENCES OF '.' IN l_begda WITH ''.
*  REPLACE ALL OCCURRENCES OF '-' IN l_begda WITH ''.
*  REPLACE ALL OCCURRENCES OF '/' IN l_begda WITH ''.
*  CONDENSE l_begda NO-GAPS.
*
*  REPLACE ALL OCCURRENCES OF '.' IN l_endda WITH ''.
*  REPLACE ALL OCCURRENCES OF '-' IN l_endda WITH ''.
*  REPLACE ALL OCCURRENCES OF '/' IN l_endda WITH ''.
*  CONDENSE l_endda NO-GAPS.

*  p_begda = l_begda.
*  p_endda = l_endda.

ENDFORM.                    " SET_INITIAL_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  DATA:
            LT_PA0000     TYPE TABLE OF PA0000 WITH HEADER LINE,
            LT_PA9883     TYPE TABLE OF PA9883 WITH HEADER LINE,
            LT_DATA         LIKE TABLE OF GS_DATA WITH HEADER LINE,
            L_ERROR       TYPE FLAG,
            L_SUBRC.

  RANGES R_BUKRS FOR P_BUKRS.

  CLEAR: GT_DATA[],GT_DATA.

  IF P_BUKRS IS NOT INITIAL.

    R_BUKRS-SIGN = 'I'.
    R_BUKRS-OPTION = 'EQ'.
    R_BUKRS-LOW = P_BUKRS.
    APPEND R_BUKRS.
  ENDIF.

  SELECT DISTINCT * FROM PA0000 AS M
    INNER JOIN PA0001 AS R
      ON M~PERNR = R~PERNR
    INTO CORRESPONDING FIELDS OF TABLE LT_DATA
    WHERE   M~PERNR IN P_PERNR
          AND   M~STAT2 <> 0
          AND   M~BEGDA <= P_ENDDA
          AND   M~ENDDA >= P_BEGDA
          AND   R~BEGDA <= P_ENDDA
          AND   R~ENDDA >= P_BEGDA
          AND   R~BUKRS IN R_BUKRS.

  IF SY-SUBRC EQ 0.

    SORT LT_DATA.
    DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING ALL FIELDS.

    SELECT DISTINCT * FROM PA9883
      INTO TABLE LT_PA9883
      FOR ALL ENTRIES IN LT_DATA
      WHERE   PERNR = LT_DATA-PERNR
            AND   BEGDA <= P_ENDDA
            AND   ENDDA >= P_BEGDA.

    SORT LT_PA9883 BY PERNR.

    LOOP AT LT_DATA.

      CLEAR: L_ERROR.
      LOOP AT LT_PA9883 WHERE PERNR = LT_DATA-PERNR AND BEGDA <= LT_DATA-ENDDA AND ENDDA => LT_DATA-BEGDA..

         L_SUBRC = 'X'.
        EXIT.
      ENDLOOP.

      IF P_INFO EQ 'X'.

         IF L_SUBRC NE 'X' OR LT_PA9883-ZZCGSJBGRP IS INITIAL
            OR  LT_PA9883-ZZCGSJIKUN IS INITIAL OR LT_PA9883-ZZCGSJIKUB IS INITIAL.

          L_ERROR = 'X'.

          IF L_SUBRC EQ 'X'.

            LT_DATA-ZZCGSJBGRP = LT_PA9883-ZZCGSJBGRP.
            LT_DATA-ZZCGSJIKUN = LT_PA9883-ZZCGSJIKUN.
            LT_DATA-ZZCGSJIKUB = LT_PA9883-ZZCGSJIKUB.

            LT_DATA-MODE = 'U'.
            LT_DATA-BEGDA = LT_PA9883-BEGDA.
            LT_DATA-ENDDA = LT_PA9883-ENDDA.
          ELSE.
            LT_DATA-MODE = 'C'.
          ENDIF.

        ENDIF.
      ELSEIF P_AGREE EQ 'X'.

        IF  L_SUBRC NE 'X' OR LT_PA9883-ZZCGSAGREE IS INITIAL.

          L_ERROR = 'X'.

         IF L_SUBRC EQ 'X'.

            LT_DATA-ZZCGSAGREE = LT_PA9883-ZZCGSAGREE.

            LT_DATA-MODE = 'U'.
            LT_DATA-BEGDA = LT_PA9883-BEGDA.
            LT_DATA-ENDDA = LT_PA9883-ENDDA.
          ELSE.
            LT_DATA-MODE = 'C'.
          ENDIF.

        ENDIF.
      ENDIF.

      CASE LT_DATA-MODE.
        WHEN 'U'.
          LT_DATA-ICONNAME = GC_YELLOW.
        WHEN 'C'.
          LT_DATA-ICONNAME = GC_RED.
      ENDCASE.

      IF L_ERROR  EQ 'X'.

        SELECT SINGLE PTEXT INTO LT_DATA-PGTXT
          FROM T501T
          WHERE SPRSL = SY-LANGU
            AND PERSG = LT_DATA-PERSG.

        SELECT SINGLE PTEXT INTO LT_DATA-PKTXT
          FROM T503T
          WHERE SPRSL = SY-LANGU
            AND PERSK = LT_DATA-PERSK.

        SELECT SINGLE STEXT INTO LT_DATA-ORGTX
          FROM HRP1000
          WHERE OBJID = LT_DATA-ORGEH
            AND OTYPE = 'O'
            AND ISTAT = '1'
            AND PLVAR = '01'
            AND BEGDA <= P_ENDDA
            AND ENDDA >= P_BEGDA.

        APPEND LT_DATA TO GT_DATA.

      ENDIF.

    ENDLOOP.

  ENDIF.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_ALV .

  IF G_R_CONT IS INITIAL.
*&--- Custom container
    CREATE OBJECT G_R_CONT
      EXPORTING
        CONTAINER_NAME              = 'C_CONT'
        STYLE                       = 1
        REPID                       = SY-REPID
        DYNNR                       = SY-DYNNR
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

*&--- Create ALV.
    CREATE OBJECT G_R_GRID
      EXPORTING
        I_PARENT          = G_R_CONT
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

*&--- Variant.
    GS_VARIANT-REPORT = SY-REPID.
    GS_VARIANT-USERNAME = SY-UNAME.

*&--- Layoyt
    GS_LAYOUT-SEL_MODE               = 'B'.
    GS_LAYOUT-NO_TOOLBAR             = 'X'.
    GS_LAYOUT-ZEBRA                  = 'X'.
*    gs_layout-cwidth_opt             = 'X'.

*&--- Field catalog.
    PERFORM SET_FCAT.


    CALL METHOD G_R_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_BYPASSING_BUFFER            = 'X'
        IS_VARIANT                    = GS_VARIANT
        I_SAVE                        = 'A'
        IS_LAYOUT                     = GS_LAYOUT
*       it_toolbar_excluding          = gt_ex_tool
      CHANGING
        IT_OUTTAB                     = GT_DATA[]
        IT_FIELDCATALOG               = GT_FCAT[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      RETURN.
    ENDIF.

  ELSE.
    PERFORM REFRESH.

  ENDIF.
ENDFORM.                    " INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FCAT .


  DATA: L_POS          TYPE I.
  CONSTANTS: C_TAB     TYPE STRING VALUE 'GT_DATA'.

  DEFINE _ALV_FACT.
    CLEAR GT_FCAT.

    ADD 1 TO L_POS.

    GT_FCAT-COL_POS     = L_POS.
    GT_FCAT-TABNAME     = C_TAB.
    GT_FCAT-FIELDNAME   = &1.
    GT_FCAT-COLTEXT     = &2.
    GT_FCAT-SCRTEXT_S   = &2.
    GT_FCAT-SCRTEXT_M   = &2.
    GT_FCAT-SCRTEXT_L   = &2.
    GT_FCAT-REPTEXT     = &2.
    GT_FCAT-KEY         = &3.
    GT_FCAT-JUST        = &4.
    GT_FCAT-FIX_COLUMN  = ' '.
    GT_FCAT-COL_OPT     = ' '.
    GT_FCAT-LZERO       = 'X'.
    GT_FCAT-OUTPUTLEN   = &5.

    IF GT_FCAT-FIELDNAME = 'BUKRS' OR GT_FCAT-FIELDNAME = 'PERNR'.
      GT_FCAT-CONVEXIT  = 'ALPHA'.
    ENDIF.

    IF GT_FCAT-FIELDNAME = 'ICONNAME'.
      GT_FCAT-ICON = 'X'.
    ENDIF.

    APPEND GT_FCAT.
  END-OF-DEFINITION.

  _ALV_FACT: 'ICONNAME'       'Status'              'X' 'C' 8,
                 'BUKRS'              'CoCd'              'X' 'C' 8,
                 'PERNR'              'Pers.No'          'X' 'C' 10,
                 'ENAME'              'NAME'            ' ' 'L' 15,
                 'PGTXT'                'EEG'             ' ' 'L' 10,
                 'PKTXT'                'EESG'           ' ' 'L' 10,
                 'ORGTX'              'Org.Unit'         ' ' 'L' 10,
                 'BEGDA'              'BEGDA'         ' ' 'C' 10,
                 'ENDDA'              'ENDDA'         ' ' 'C' 10.

  IF P_INFO EQ 'X'.
    _ALV_FACT:  'ZZCGSJBGRP'    'GHRS Group' ' ' 'L' 10,
                   'ZZCGSJIKUN'     'GHRS Track'   ' ' 'L' 10,
                  'ZZCGSJIKUB'     'GHRS Grade'   ' ' 'L' 10.

  ELSEIF P_AGREE EQ 'X'.
    _ALV_FACT:  'ZZCGSAGREE'    'Agree check'  ' '   'c' 10.
  ENDIF.


ENDFORM.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH .
  G_R_GRID->REFRESH_TABLE_DISPLAY( ).
ENDFORM.                    " REFRESH
*&---------------------------------------------------------------------*
*&      Form  GET_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SELECT_DATA .

  DATA: LT_INDEX          TYPE LVC_T_ROID WITH HEADER LINE.

  CLEAR: GT_DATA,G_SELIDX,LT_INDEX.

  G_R_GRID->GET_SELECTED_ROWS(
    IMPORTING
      ET_ROW_NO = LT_INDEX[]
  ).

  CLEAR LT_INDEX.
  READ TABLE LT_INDEX INDEX 1.

  IF SY-SUBRC <> 0.
    MESSAGE 'Select line' TYPE  'I'.
    RETURN.
  ENDIF.

  G_SELIDX  = LT_INDEX-ROW_ID.

  READ TABLE GT_DATA INDEX LT_INDEX-ROW_ID.

ENDFORM.                    " GET_SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION .

  DATA:
            LT_BDC  TYPE    BDCDATA_TAB WITH HEADER LINE,
            LS_PA9883   TYPE PA9883.

  DEFINE _BDC.
    &6-PROGRAM = &1.
    &6-DYNPRO = &2.
    &6-DYNBEGIN = &3.
    &6-FNAM = &4.
    &6-FVAL = &5.

    APPEND &6 TO &7.

  END-OF-DEFINITION.

  CASE GT_DATA-MODE.
    WHEN 'C'.
      PERFORM SET_BDC_CREATE CHANGING LT_BDC[].
    WHEN 'U'.
      PERFORM SET_BDC_UPDATE CHANGING LT_BDC[].
  ENDCASE.

*  call
  CALL  TRANSACTION 'PA30' USING LT_BDC[] MODE 'E' .
  IF SY-SUBRC EQ 0.

    IF GT_DATA-MODE EQ 'U'.
      SELECT SINGLE * FROM PA9883
        INTO LS_PA9883
        WHERE PERNR = GT_DATA-PERNR
              AND BEGDA = GT_DATA-BEGDA
              AND ENDDA = GT_DATA-ENDDA.
    ELSE.
      SELECT SINGLE * FROM PA9883
        INTO LS_PA9883
        WHERE PERNR = GT_DATA-PERNR
            AND   BEGDA <= P_ENDDA
            AND   ENDDA >= P_BEGDA.
    ENDIF.

    IF SY-SUBRC EQ 0.

      CASE ABAP_TRUE.
        WHEN P_INFO.
          IF LS_PA9883-ZZCGSJBGRP IS NOT INITIAL
            AND   LS_PA9883-ZZCGSJIKUN IS NOT  INITIAL
            AND LS_PA9883-ZZCGSJIKUB IS NOT INITIAL.
            DELETE GT_DATA[] INDEX G_SELIDX.

          ELSE.
            GT_DATA-ICONNAME = GC_YELLOW.
            MODIFY GT_DATA INDEX G_SELIDX.
          ENDIF.
        WHEN P_AGREE.
          IF LS_PA9883-ZZCGSAGREE EQ 'X'.
            DELETE GT_DATA[] INDEX G_SELIDX.
          ELSE.
            GT_DATA-ICONNAME = GC_YELLOW.
            MODIFY GT_DATA INDEX G_SELIDX.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  SET_BDC_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_BDC[]  text
*----------------------------------------------------------------------*
FORM SET_BDC_CREATE  CHANGING PT_BDC TYPE    BDCDATA_TAB.

  DATA: LS_BDC TYPE BDCDATA.

  _BDC 'SAPMP50A' '1000' 'X' '' '' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_OKCODE'  '/00'  LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-PERNR'  GT_DATA-PERNR LS_BDC PT_BDC.
*  _BDC '' '' '' 'BDC_SUBSCR'  '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  G_FVAL  LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0320SUBSCR_ITMENU' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0330SUBSCR_TIME' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-TIMR6'  'X' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0350SUBSCR_ITKEYS' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-CHOIC'  '9883' LS_BDC PT_BDC.

  _BDC 'SAPMP50A' '1000' 'X' '' '' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_CURSOR'  'RP50G-PERNR' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_OKCODE'  '=INS' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-PERNR'	GT_DATA-PERNR LS_BDC PT_BDC.
*  _BDC '' '' '' 'BDC_SUBSCR'  '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  G_FVAL  LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0320SUBSCR_ITMENU' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0330SUBSCR_TIME' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-TIMR6'	'X' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0350SUBSCR_ITKEYS' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-CHOIC'	'GHRS Information' LS_BDC PT_BDC.

  _BDC 'MP988300' '2000'  'X' '' '' LS_BDC PT_BDC.

ENDFORM.                    " SET_BDC_CREATE
*&---------------------------------------------------------------------*
*&      Form  SET_BDC_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_BDC[]  text
*----------------------------------------------------------------------*
FORM SET_BDC_UPDATE  CHANGING PT_BDC TYPE  BDCDATA_TAB.

  DATA:
            LS_BDC TYPE BDCDATA,
            L_BEGDA(10),
            L_ENDDA(10).

  CALL FUNCTION 'SLS_MISC_CONVERT_TO_DATE'
    EXPORTING
      P_DATE                        = GT_DATA-BEGDA
    IMPORTING
      P_DATE_STRING                 = L_BEGDA
    EXCEPTIONS
      ERROR_SELECTING_USER_DEFAULTS = 1
      OTHERS                        = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'SLS_MISC_CONVERT_TO_DATE'
    EXPORTING
      P_DATE                        = GT_DATA-ENDDA
    IMPORTING
      P_DATE_STRING                 = L_ENDDA
    EXCEPTIONS
      ERROR_SELECTING_USER_DEFAULTS = 1
      OTHERS                        = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  REPLACE ALL OCCURRENCES OF '.' IN l_begda WITH ''.
*  REPLACE ALL OCCURRENCES OF '-' IN l_begda WITH ''.
*  REPLACE ALL OCCURRENCES OF '/' IN l_begda WITH ''.
*  CONDENSE l_begda NO-GAPS.
*
*  REPLACE ALL OCCURRENCES OF '.' IN l_endda WITH ''.
*  REPLACE ALL OCCURRENCES OF '-' IN l_endda WITH ''.
*  REPLACE ALL OCCURRENCES OF '/' IN l_endda WITH ''.
*  CONDENSE l_endda NO-GAPS.

  _BDC 'SAPMP50A' '1000' 'X' '' '' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_OKCODE'  '/00'  LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-PERNR'  GT_DATA-PERNR LS_BDC PT_BDC.
*  _BDC '' '' '' 'BDC_SUBSCR'  '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  G_FVAL  LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0320SUBSCR_ITMENU' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0330SUBSCR_TIME' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-TIMR6'  'X' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0350SUBSCR_ITKEYS' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-CHOIC'  '9883' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-BEGDA'  L_BEGDA LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-ENDDA'  L_ENDDA LS_BDC PT_BDC.

  _BDC 'SAPMP50A' '1000' 'X' '' '' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_CURSOR'  'RP50G-PERNR' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_OKCODE'  '=MOD' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-PERNR'	GT_DATA-PERNR LS_BDC PT_BDC.
*  _BDC '' '' '' 'BDC_SUBSCR'  '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  G_FVAL  LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0320SUBSCR_ITMENU' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0330SUBSCR_TIME' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-TIMR6'	'X' LS_BDC PT_BDC.
  _BDC '' '' '' 'BDC_SUBSCR'  'SAPMP50A                                0350SUBSCR_ITKEYS' LS_BDC PT_BDC.
  _BDC '' '' '' 'RP50G-CHOIC'	'GHRS Information' LS_BDC PT_BDC.

  _BDC 'MP988300' '2000'  'X' '' '' LS_BDC PT_BDC.

ENDFORM.                    " SET_BDC_UPDATE
