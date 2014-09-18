REPORT ZQM_SCARP_PQRE NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMMM.

TABLES: QMEL, VIQMEL, TJ30T, QMFE.
TYPE-POOLS: VRM.

DATA: BEGIN OF IT_ITAB1 OCCURS 0,
         QMNUM LIKE QMEL-QMNUM,
      MATNR LIKE QMEL-MATNR,
      MAKTX LIKE MAKT-MAKTX,
      FECOD LIKE VIQMFE-FECOD,
      FECOD_TX(20),
      FEGRP LIKE VIQMFE-FEGRP,
      FEGRP_TX(20),
      CRCH TYPE CHAR1,
      LIFNUM LIKE QMEL-LIFNUM,
      QWRNUM LIKE QMEL-QWRNUM,
      TXT04 LIKE TJ30T-TXT04,
      INACT LIKE JEST-INACT,
      ESTAT LIKE TJ30-ESTAT,
      ERDAT LIKE QMEL-ERDAT,
      RKMNG LIKE QMEL-RKMNG,
      ERZEIT LIKE QMEL-ERZEIT,
      REFUTE(1),
      CKD(1),
      END OF IT_ITAB1.

DATA: WA_ITAB LIKE IT_ITAB1.
DATA:  CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
       CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'S',
       BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
       MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       IT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.

DATA: XNAME    TYPE VRM_ID,
      NAME     TYPE VRM_ID    ,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      BEGIN OF YLIST     OCCURS 0,
         KEY(40) TYPE C,
         TEXT(80) TYPE C,
      END OF YLIST      .

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_QMNUM FOR QMEL-QMNUM,
                 S_ERDAT FOR QMEL-ERDAT,
                 S_TXT04 FOR TJ30T-TXT04 OBLIGATORY,
                 S_LIFNUM FOR QMEL-LIFNUM.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-001.
SELECTION-SCREEN POSITION 33.
PARAMETERS: P_QMGRP LIKE VIQMEL-QMGRP,
            P_QMCOD LIKE VIQMEL-QMCOD MODIF ID A.
SELECTION-SCREEN END OF LINE.
*PARAMETERS: P_FEGRPN(8) AS LISTBOX VISIBLE LENGTH 15 OBLIGATORY.


SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.
  PERFORM INIT_DATA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_QMGRP.
  PERFORM DISPLAY_SEARCH_HELP USING 'X' 'P_QMGRP' 'P_QMCOD'.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_QMCOD'    " OR SCREEN-NAME = 'P_FECOD'.
       OR SCREEN-NAME = 'S_FECOD-LOW' OR
       SCREEN-NAME = 'S_FECOD-HIGH'.
      SCREEN-INPUT = 0.
      SCREEN-OUTPUT = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*  PERFORM LIST_BOX_FEGRPN.

START-OF-SELECTION.

  IF S_QMNUM[] IS INITIAL AND S_TXT04[] IS INITIAL AND
     S_LIFNUM[] IS INITIAL.
    MESSAGE S009 WITH TEXT-M01.
  ELSE.
    PERFORM GET_DATA.
    IF IT_ITAB1[] IS INITIAL.
      MESSAGE S009 WITH TEXT-M01.
    ELSE.
      PERFORM UPDATE_DATA.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA: BEGIN OF LT_TEMP OCCURS 0,
      QMNUM LIKE QMEL-QMNUM,
      MATNR LIKE QMEL-MATNR,
      MAKTX LIKE MAKT-MAKTX,
      FECOD LIKE VIQMFE-FECOD,
      FECOD_TX(20),
      FEGRP LIKE VIQMFE-FEGRP,
      FEGRP_TX(20),
      CRCH TYPE CHAR1,
      LIFNUM LIKE QMEL-LIFNUM,
      QWRNUM LIKE QMEL-QWRNUM,
      TXT04 LIKE TJ30T-TXT04,
      INACT LIKE JEST-INACT,
      ESTAT LIKE TJ30-ESTAT,
      ERDAT LIKE QMEL-ERDAT,
      RKMNG LIKE QMEL-RKMNG,
      ERZEIT LIKE QMEL-ERZEIT,
      REFUTE(1),
      MAWERK LIKE QMEL-MAWERK,
      END OF LT_TEMP.

  DATA: BEGIN OF LT_QMNUM OCCURS 0,
        QMNUM LIKE QMEL-QMNUM,
        END OF LT_QMNUM.
*  DATA: L_FLAG_D(1),
*        L_FLAG_SAVE(1),
  DATA: L_OBJNR LIKE QMEL-OBJNR,
       L_STAT LIKE JEST-STAT,
       L_DATE_REFU LIKE JCDS-UDATE,
       L_TIME_REFU LIKE JCDS-UTIME,
       L_INT TYPE I.

  DATA: L_CURR_DATE LIKE SY-DATUM,
        L_CURR_TIME LIKE SY-UZEIT,
        L_DATEDIFF TYPE P,
        L_TIMEDIFF TYPE P,
        L_PROFL LIKE MARA-PROFL,
        L_KALID LIKE KAKO-KALID,
        L_DATE_BE LIKE SY-DATUM,
        L_TIME_BE LIKE SY-UZEIT,
        L_DATE LIKE SY-DATUM.

  CONSTANTS: C_STAT LIKE JEST-STAT VALUE '10068'.

  RANGES: S_QMGRP FOR QMEL-QMGRP,
          S_QMCOD FOR QMEL-QMCOD.

  IF NOT P_QMGRP IS INITIAL.
    S_QMGRP-SIGN = 'I'.
    S_QMGRP-OPTION = 'EQ'.
    S_QMGRP-LOW = P_QMGRP.
    APPEND S_QMGRP.
    S_QMCOD-SIGN = 'I'.
    S_QMCOD-OPTION = 'EQ'.
    S_QMCOD-LOW = P_QMCOD.
    APPEND S_QMCOD.
  ENDIF.

  SELECT A~QMNUM A~MATNR A~LIFNUM A~QWRNUM FECOD FEGRP
      E~ESTAT TXT04 INACT
      A~ERDAT A~RKMNG A~ERZEIT A~MAWERK
      INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
      FROM QMEL AS A
      INNER JOIN QMFE AS B
      ON A~QMNUM = B~QMNUM
      INNER JOIN VIQMEL AS C
      ON A~QMNUM = C~QMNUM
      INNER JOIN JEST AS D
      ON C~OBJNR = D~OBJNR
      INNER JOIN TJ30 AS E
      ON D~STAT = E~ESTAT
      INNER JOIN TJ30T AS F
      ON E~STSMA = F~STSMA
      AND E~ESTAT = F~ESTAT
      WHERE A~QMNUM IN S_QMNUM
        AND A~ERDAT IN S_ERDAT
        AND C~QMGRP IN S_QMGRP
        AND C~QMCOD IN S_QMCOD
        AND A~LIFNUM IN S_LIFNUM
         AND FEGRP = '0'
        AND F~STSMA = 'ZQNSCRP1'
        AND F~SPRAS = 'EN'.

*  LOOP AT LT_TEMP.
*    LT_QMNUM-QMNUM = LT_TEMP-QMNUM.
*    COLLECT LT_QMNUM.
*  ENDLOOP.

  SORT LT_TEMP BY QMNUM MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_TEMP COMPARING QMNUM MATNR.

  L_CURR_DATE = SY-DATUM.
  L_CURR_TIME = SY-UZEIT.

  LOOP AT LT_TEMP.
    CHECK LT_TEMP-TXT04 IN S_TXT04 AND LT_TEMP-INACT = ' '.
    CLEAR: L_DATEDIFF, L_TIMEDIFF, L_DATE_REFU, L_TIME_REFU.
*    CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
*      EXPORTING
*        DATE1            = L_CURR_DATE
*        TIME1            = L_CURR_TIME
*        DATE2            = LT_TEMP-ERDAT
*        TIME2            = LT_TEMP-ERZEIT
*      IMPORTING
*        DATEDIFF         = L_DATEDIFF
*        TIMEDIFF         = L_TIMEDIFF
**       EARLIEST         =
*      EXCEPTIONS
*        INVALID_DATETIME = 1
*        OTHERS           = 2.
*    IF SY-SUBRC <> 0.
*      CONTINUE.
*    ENDIF.


*    IF LT_TEMP-ESTAT <> 'E0001' AND
*       LT_TEMP-ESTAT <> 'E0015' AND
*       LT_TEMP-ESTAT <> 'E0008' AND
*       LT_TEMP-ESTAT <> 'E0009'.
*      CONTINUE.
*    ENDIF.
*
    SELECT SINGLE OBJNR INTO L_OBJNR
       FROM QMEL
       WHERE QMNUM = LT_TEMP-QMNUM.

*      SELECT SINGLE STAT INTO L_STAT
*        FROM JEST AS A
*        INNER JOIN TJ02T AS B
*        ON A~STAT = B~ISTAT
*        WHERE OBJNR = L_OBJNR
*          AND TXT04 = 'OSNO'
*          AND INACT = ' '.

    SELECT SINGLE STAT INTO L_STAT
         FROM JEST AS A
         INNER JOIN TJ02T AS B
         ON A~STAT = B~ISTAT
         WHERE OBJNR = L_OBJNR
           AND TXT04 = 'OSNO'
           AND INACT = ' '.

    IF SY-SUBRC = 0.

      IT_ITAB1 = LT_TEMP.

      CLEAR: L_DATE_REFU, L_TIME_REFU.
      SELECT SINGLE UDATE UTIME INTO (L_DATE_REFU, L_TIME_REFU)
       FROM JCDS AS A
       WHERE OBJNR = L_OBJNR
         AND STAT = 'E0015'           " Refute
         AND INACT = ' '.

      IF SY-SUBRC = 0.
        L_DATE_BE = L_DATE_REFU.
        L_TIME_BE = L_TIME_REFU.
      ELSE.
        L_DATE_BE = LT_TEMP-ERDAT.
        L_TIME_BE = LT_TEMP-ERZEIT.
      ENDIF.

      CASE LT_TEMP-MAWERK.
        WHEN 'P001'.
          L_KALID = 'HM'.
        WHEN 'E001' OR 'E002'.
          L_KALID = 'HE'.
      ENDCASE.

** Furong on 08/16/12 for no-working check
      L_DATE = L_DATE_BE.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          CORRECT_OPTION               = '+'
          DATE                         = L_DATE
          FACTORY_CALENDAR_ID          = L_KALID
        IMPORTING
          DATE                         = L_DATE
        EXCEPTIONS
          CALENDAR_BUFFER_NOT_LOADABLE = 1
          CORRECT_OPTION_INVALID       = 2
          DATE_AFTER_RANGE             = 3
          DATE_BEFORE_RANGE            = 4
          DATE_INVALID                 = 5
          FACTORY_CALENDAR_NOT_FOUND   = 6
          OTHERS                       = 7.
      IF L_DATE <> L_DATE_BE.
        L_DATE_BE = L_DATE.
        L_TIME_BE = '000630'.
      ENDIF.
** End
      IF L_CURR_DATE = L_DATE_BE.
        L_DATEDIFF = 0.
        L_TIMEDIFF =  L_CURR_TIME - L_TIME_BE.
        IF L_TIMEDIFF < 0.
          L_TIMEDIFF =  L_TIMEDIFF MOD 86400 .
          L_DATEDIFF = L_DATEDIFF - 1.
        ENDIF.
        L_TIMEDIFF = L_TIMEDIFF / 3600.
      ELSEIF L_CURR_DATE > L_DATE_BE.

        WHILE L_CURR_DATE > L_DATE_BE.

          L_DATE_BE = L_DATE_BE + 1.
          CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
            EXPORTING
              CORRECT_OPTION               = '+'
              DATE                         = L_DATE_BE
              FACTORY_CALENDAR_ID          = L_KALID
            IMPORTING
              DATE                         = L_DATE_BE
            EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 1
              CORRECT_OPTION_INVALID       = 2
              DATE_AFTER_RANGE             = 3
              DATE_BEFORE_RANGE            = 4
              DATE_INVALID                 = 5
              FACTORY_CALENDAR_NOT_FOUND   = 6
              OTHERS                       = 7.

          IF L_DATE_BE <= L_CURR_DATE.
            L_DATEDIFF = L_DATEDIFF + 1.
          ENDIF.

        ENDWHILE.

        L_TIMEDIFF =  L_CURR_TIME - L_TIME_BE.
        IF L_TIMEDIFF < 0.
          L_TIMEDIFF =  L_TIMEDIFF MOD 86400 .
          L_DATEDIFF = L_DATEDIFF - 1.
        ENDIF.
        L_TIMEDIFF = L_TIMEDIFF / 3600.
      ENDIF.

      L_TIMEDIFF = L_DATEDIFF * 24 + L_TIMEDIFF.

      clear: L_PROFL.
      IF NOT L_DATE_REFU IS INITIAL.
        IT_ITAB1-REFUTE = 'X'.
        SELECT SINGLE PROFL INTO L_PROFL
        FROM MARA
        WHERE MATNR = IT_ITAB1-MATNR.
        IF L_PROFL = 'K'.
          IT_ITAB1-CKD = 'X'.
          IF L_TIMEDIFF < 120.
            CLEAR: IT_ITAB1.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR: IT_ITAB1-CKD.
          IF L_TIMEDIFF < 72.
            CLEAR: IT_ITAB1.
            CONTINUE.
          ENDIF.
        ENDIF.
** Changed on 03/05/13: change to < 72 for CKD
      else.
        SELECT SINGLE PROFL INTO L_PROFL
        FROM MARA
        WHERE MATNR = IT_ITAB1-MATNR.
        case L_PROFL.
          when 'K'.
            if L_TIMEDIFF < 72.
              CLEAR: IT_ITAB1.
              CONTINUE.
            ELSE.
              CLEAR: IT_ITAB1-REFUTE.
            endif.
          when others.
            if L_TIMEDIFF < 24.
              CLEAR: IT_ITAB1.
              CONTINUE.
            ELSE.
              CLEAR: IT_ITAB1-REFUTE.
            ENDIF.
        endcase.
*      ELSEIF L_TIMEDIFF < 24.
*        CLEAR: IT_ITAB1.
*        CONTINUE.
*      ELSE.
*       CLEAR: IT_ITAB1-REFUTE.
** End on 03/05/13
      ENDIF.

      IT_ITAB1-RKMNG = L_INT =  LT_TEMP-RKMNG.
      WRITE: LT_TEMP-ERDAT TO IT_ITAB1-ERDAT MM/DD/YY.

      SELECT SINGLE KURZTEXT INTO IT_ITAB1-FEGRP_TX
        FROM QPGT
        WHERE CODEGRUPPE = IT_ITAB1-FEGRP
          AND SPRACHE = 'EN'.

      SELECT SINGLE KURZTEXT INTO IT_ITAB1-FECOD_TX
        FROM QPCT
        WHERE CODEGRUPPE = IT_ITAB1-FEGRP
          AND CODE = IT_ITAB1-FECOD
          AND SPRACHE = 'EN'.

      SELECT SINGLE MAKTX INTO IT_ITAB1-MAKTX
        FROM MAKT
        WHERE MATNR = IT_ITAB1-MATNR
          AND SPRAS = 'E'.
      APPEND IT_ITAB1.

      CLEAR: LT_QMNUM, IT_ITAB1, LT_TEMP.
    ENDIF.
  ENDLOOP.
  SORT IT_ITAB1 BY QMNUM MATNR FEGRP LIFNUM.
ENDFORM.                    " GET_DATA
.
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  S_TXT04-SIGN = 'I'.
  S_TXT04-OPTION = 'EQ'.
  S_TXT04-LOW = 'CRTD'.
  APPEND S_TXT04.
  S_TXT04-LOW = 'NPPM'.
  APPEND S_TXT04.

*  LOOP AT SCREEN.
*    IF SCREEN-NAME = 'S_FECOD-LOW' OR
*       SCREEN-NAME = 'S_FECOD-HIGH'.
*      SCREEN-INPUT = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_DATA.
  CLEAR: IT_TLINE[].
  LOOP AT IT_ITAB1.
    PERFORM UPDATE_STATUS TABLES IT_TLINE.
    READ TABLE IT_TLINE WITH KEY TDFORMAT = 'E'.
    IF SY-SUBRC = 0.
      MESSAGE I009 WITH IT_TLINE-TDLINE.
    ENDIF.
    REFRESH IT_TLINE.
    CLEAR: IT_TLINE.
  ENDLOOP.

ENDFORM.                    " UPDATE_data
*&---------------------------------------------------------------------*
*&      Form  update_system_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_SYSTEM_STATUS.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                IT_ITAB1-QMNUM.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=COWO'.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM BDC_TRANSACTION TABLES IT_TLINE
                          USING 'QM02'.

ENDFORM.                    " update_system_status
*&---------------------------------------------------------------------*
*&      Form  release_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB1_QMNUM  text
*----------------------------------------------------------------------*
FORM RELEASE_LOCK USING P_QMNUM.
  IF NOT P_QMNUM IS INITIAL.
    WAIT UP TO 2 SECONDS.
    CALL FUNCTION 'DEQUEUE_EIQMEL'
      EXPORTING
*       MODE_QMEL = 'E'
*       MANDT     = SY-MANDT
        QMNUM     = P_QMNUM
*       X_QMNUM   = ' '
*       _SCOPE    = '3'
*       _SYNCHRON = ' '
*       _COLLECT  = ' '
      .
  ENDIF.
ENDFORM.                    " release_lock

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING    PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0305   text
*      -->P_0306   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING    FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    " bdc_field

*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0315   text
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION TABLES  P_IT_TLINE STRUCTURE TLINE
                      USING TCODE.

  DATA: L_SUBRC LIKE SY-SUBRC,
        MSG(255).

* call transaction using

*  REFRESH: messtab.
  CLEAR : MSG.
  CALL TRANSACTION TCODE USING BDCDATA
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.
  L_SUBRC = SY-SUBRC.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
*    LOOP AT MESSTAB.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
*                 MESSTAB-MSGV4.
*
*      CONCATENATE MSG MESSTAB-MSGV1 INTO MSG.
*    ENDLOOP.
*    CONCATENATE 'System Status' MSG INTO P_IT_TLINE-TDLINE
*              SEPARATED BY SPACE.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        MSGID               = SY-MSGID
        MSGNR               = SY-MSGNO
        MSGV1               = SY-MSGV1
        MSGV2               = SY-MSGV2
        MSGV3               = SY-MSGV3
        MSGV4               = SY-MSGV4
      IMPORTING
        MESSAGE_TEXT_OUTPUT = MSG.
    CONCATENATE 'STATUS' MSG INTO P_IT_TLINE-TDLINE
                SEPARATED BY SPACE.
    REFRESH BDCDATA.
    P_IT_TLINE-TDFORMAT = 'E' .
    P_IT_TLINE-TDLINE = MSG.
  ELSE.
    READ TABLE MESSTAB WITH KEY MSGTYP = 'A'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = SY-MSGID
          MSGNR               = SY-MSGNO
          MSGV1               = SY-MSGV1
          MSGV2               = SY-MSGV2
          MSGV3               = SY-MSGV3
          MSGV4               = SY-MSGV4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = MSG.
      CONCATENATE 'STATUS' MSG INTO P_IT_TLINE-TDLINE
                  SEPARATED BY SPACE.
      REFRESH BDCDATA.
      P_IT_TLINE-TDFORMAT = 'E' .
      P_IT_TLINE-TDLINE = MSG.
    ELSE.
*    MESSAGE I000 WITH TEXT-M03.
      CONCATENATE  TEXT-M04 IT_ITAB1-QMNUM INTO MSG
              SEPARATED BY SPACE.
      REFRESH BDCDATA.
      P_IT_TLINE-TDFORMAT = 'S' .
      P_IT_TLINE-TDLINE = MSG.
    ENDIF.
  ENDIF.
  APPEND P_IT_TLINE.
  CLEAR P_IT_TLINE.
ENDFORM.                    " bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB1_QMNUM  text
*----------------------------------------------------------------------*
FORM UPDATE_STATUS TABLES P_IT_TLINE STRUCTURE TLINE.
  DATA: L_TXT04 LIKE IT_ITAB1-TXT04,
        L_STAT LIKE JEST-STAT,
        MSG(255).
  DATA: L_STATUS(2).

  L_STATUS = '04'.
  L_TXT04 = 'VEND'.
  L_STAT = 'E0004'.

  PERFORM UPDATE_SYS_USER_STATUS TABLES IT_TLINE USING L_STATUS.

ENDFORM.                    " update
*&---------------------------------------------------------------------*
*&      Form  display_search_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0189   text
*      -->P_0190   text
*      -->P_0191   text
*----------------------------------------------------------------------*
FORM DISPLAY_SEARCH_HELP USING    P_KATALOGART
                                  P_FIELDNAME
                                  P_FIELDNAME1.

  DATA:L_REPID                 LIKE D020S-PROG,
       L_DYNNR                 LIKE SY-DYNNR,
       L_QMGRP                 LIKE VIQMEL-QMGRP.

  DATA: I_KATALOGART TYPE QPGR-KATALOGART,
        L_QPK1CD          LIKE QPK1CD,
        I_CODEGRUPPE LIKE  QPGR-CODEGRUPPE,
        I_CODE LIKE  QPCD-CODE VALUE '*' .
  DATA : T_CODEGRPTAB LIKE QPK1CODEGRP OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF L_DYNFIELDTAB OCCURS 10.
          INCLUDE STRUCTURE DYNPREAD.
  DATA : END   OF L_DYNFIELDTAB.

  MOVE : SY-REPID TO L_REPID,
         SY-DYNNR TO L_DYNNR.

  IF P_FIELDNAME = 'S_FEGRP-LOW' OR
     P_FIELDNAME = 'S_FEGRP-HIGH'.

    MOVE P_FIELDNAME TO L_DYNFIELDTAB-FIELDNAME.
    APPEND L_DYNFIELDTAB.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME                         = L_REPID
        DYNUMB                         = L_DYNNR
*       TRANSLATE_TO_UPPER             = ' '
*       REQUEST                        = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX           = ' '
      TABLES
        DYNPFIELDS                     = L_DYNFIELDTAB
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA           = 1
*       INVALID_DYNPROFIELD            = 2
*       INVALID_DYNPRONAME             = 3
*       INVALID_DYNPRONUMMER           = 4
*       INVALID_REQUEST                = 5
*       NO_FIELDDESCRIPTION            = 6
*       INVALID_PARAMETER              = 7
*       UNDEFIND_ERROR                 = 8
*       DOUBLE_CONVERSION              = 9
*       STEPL_NOT_FOUND                = 10
*       OTHERS                         = 11
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    READ TABLE L_DYNFIELDTAB INDEX 1.
    L_QMGRP = L_DYNFIELDTAB-FIELDVALUE.

    CLEAR: L_DYNFIELDTAB, L_DYNFIELDTAB[].


    IF L_QMGRP = 'MXTX10' OR L_QMGRP = 'MXTX11' OR
       L_QMGRP = 'MXTX12' OR L_QMGRP = 'MXTX13' OR
       L_QMGRP = 'MXTX15' OR L_QMGRP = 'MXTX19' OR
       L_QMGRP = 'MXTX51' OR L_QMGRP = 'MXTX53'.

      T_CODEGRPTAB = '0'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '4'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '9'.
      APPEND T_CODEGRPTAB.
    ELSE.

      T_CODEGRPTAB = '*'.
      APPEND T_CODEGRPTAB.

    ENDIF.

    CLEAR : L_QMGRP.

  ELSE.

    T_CODEGRPTAB = '*'.
    APPEND T_CODEGRPTAB.

  ENDIF.

  I_KATALOGART = P_KATALOGART.

  CALL FUNCTION 'QPK1_GP_CODE_PICKUP'
    EXPORTING
      I_KATALOGART                 = I_KATALOGART
      I_CODEGRUPPE                 = I_CODEGRUPPE
      I_CODE                       = I_CODE
      I_SPRACHE                    = SY-LANGU
      I_WINX1                      = 10
      I_WINX2                      = 68
      I_WINY1                      = 5
      I_WINY2                      = 27
*   I_DISPLAY_MODE               =
*   I_RETURN_IF_ONE              = 'X'
*   I_RETURN_IF_MANY             =
*   I_NO_USAGEINDICATION         =
*   I_NO_AUTHORITY_CHECK         =
    IMPORTING
      E_QPK1CD                     = L_QPK1CD
    TABLES
      T_CODEGRPTAB                 = T_CODEGRPTAB
* EXCEPTIONS
*   NO_MATCH_IN_RANGE            = 1
*   NO_USER_SELECTION            = 2
*   NO_AUTHORIZATION             = 3
*   NO_SELECTION_SPECIFIED       = 4
*   OBJECT_LOCKED                = 5
*   LOCK_ERROR                   = 6
*   OBJECT_MISSING               = 7
*   OTHERS                       = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  MOVE P_FIELDNAME TO L_DYNFIELDTAB-FIELDNAME.
  MOVE  L_QPK1CD-CODEGRUPPE TO L_DYNFIELDTAB-FIELDVALUE.
  APPEND L_DYNFIELDTAB.

  MOVE P_FIELDNAME1 TO L_DYNFIELDTAB-FIELDNAME.
  MOVE  L_QPK1CD-CODE TO L_DYNFIELDTAB-FIELDVALUE.
  APPEND L_DYNFIELDTAB.

*  IF p_fieldname = 'QMGRP'.
*
*    MOVE  'OTGRP' TO l_dynfieldtab-fieldname.
*    MOVE  l_qpk1cd-codegruppe TO l_dynfieldtab-fieldvalue.
*    APPEND l_dynfieldtab.
*
*    MOVE  'OTEIL' TO l_dynfieldtab-fieldname.
*    MOVE  l_qpk1cd-code TO l_dynfieldtab-fieldvalue.
*    APPEND l_dynfieldtab.
*
*  ENDIF.

  IF P_FIELDNAME = 'P_FEGRP' AND L_QPK1CD-CODEGRUPPE = '0'.

    MOVE 'URGRP' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  'CAUS' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    MOVE 'URCOD' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  '04' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF P_FIELDNAME = 'P_FEGRP' AND L_QPK1CD-CODEGRUPPE = '4'.

    MOVE 'URGRP' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  'CAUS' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    MOVE 'URCOD' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  '02' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME               = L_REPID
      DYNUMB               = L_DYNNR
    TABLES
      DYNPFIELDS           = L_DYNFIELDTAB
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 01
      INVALID_DYNPROFIELD  = 02
      INVALID_DYNPRONAME   = 03
      INVALID_DYNPRONUMMER = 04
      INVALID_REQUEST      = 05
      NO_FIELDDESCRIPTION  = 06
      UNDEFIND_ERROR       = 07.

  IF SY-SUBRC <> 0.
*    MESSAGE I009.
    EXIT.
  ENDIF.
ENDFORM.                    " display_search_help
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA.
  CLEAR: IT_ITAB1[], BDCDATA[], MESSTAB[], IT_TLINE[].
  CLEAR: WA_ITAB, P_QMGRP,P_QMCOD.
*  clear:  P_FEGRP,  P_FECOD.
ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST.
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'IT_ITAB1-FECOD'.
  PERFORM SET_FIELD_PROG.
  PERFORM CALL_FUNCTION_VRM USING XLIST.
ENDFORM.                    " make_dropdown_list
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_PROG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_PROG.
  XVALUE-KEY  = '24'.
  XVALUE-TEXT = '(24,26) SHIP/IN'.
  APPEND XVALUE TO XLIST.
* SHIP/OUT(25)
  XVALUE-KEY  = '25'.
  XVALUE-TEXT = '(25,27) SHIP/OUT'.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " SET_FIELD_PROG
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_VRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XLIST  text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_VRM USING P_XLIST.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = P_XLIST.
ENDFORM.                    " CALL_FUNCTION_VRM
.
*&---------------------------------------------------------------------*
*&      Form  update_sys_user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_SYS_USER_STATUS TABLES P_IT_TLINE STRUCTURE TLINE
                            USING P_USER_STATUS.

  DATA:  L_PROFL LIKE MARA-PROFL.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                               IT_ITAB1-QMNUM.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=AWST'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0201'.

  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                               'J_STMAINT-ANWS(04)'.
  PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(04)'
                            'X'.

** UPDATE pa24
  IF IT_ITAB1-REFUTE IS INITIAL.

    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'J_STMAINT-ANWSO(04)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=OKAY'.
    PERFORM BDC_FIELD       USING 'J_STMAINT-ANWSO(04)'
                                    'X'.
  ELSE.

    IF IT_ITAB1-CKD = 'X'.

      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'J_STMAINT-ANWSO(01)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=OKAY'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWSO(01)'
                                      'X'.
    ELSE.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                   'J_STMAINT-ANWSO(02)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=OKAY'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWSO(02)'
                                      'X'.

    ENDIF.


  ENDIF.

  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=OKAY'.

*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RIWO00-QMNUM'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
*                                IT_ITAB1-QMNUM.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=COWO'.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM BDC_TRANSACTION TABLES IT_TLINE
                          USING 'QM02'.

ENDFORM.                    " update_sys_user_status
