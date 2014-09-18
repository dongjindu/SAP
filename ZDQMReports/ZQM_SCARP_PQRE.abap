REPORT ZQM_SCARP_PQRE NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMMM.

TABLES: QMEL, VIQMEL, TJ30T, QMFE.
TYPE-POOLS: VRM.

DATA: BEGIN OF IT_ITAB1 OCCURS 0,
      SEL TYPE CHAR1,
      QMNUM LIKE QMEL-QMNUM,
      MATNR LIKE QMEL-MATNR,
      MAKTX LIKE MAKT-MAKTX,
      APPROVE TYPE CHAR1,
      REFUTE TYPE CHAR1,
      VENDER TYPE CHAR1,
      HMMA  TYPE CHAR1,
      STCK TYPE CHAR1,
      FECOD LIKE VIQMFE-FECOD,
      FECOD_TX(20),
      FEGRP LIKE VIQMFE-FEGRP,
      FEGRP_TX(20),
      CRCH TYPE CHAR1,
      LIFNUM LIKE QMEL-LIFNUM,
      QWRNUM LIKE QMEL-QWRNUM,
      CHANGE(4),
      COMMENTS(50),
      TXT04 LIKE TJ30T-TXT04,
      INACT LIKE JEST-INACT,
      ESTAT LIKE TJ30-ESTAT,
      ERDAT(8),  "LIKE QMEL-ERDAT,
      RKMNG(5), "LIKE QMEL-RKMNG,
      END OF IT_ITAB1.

DATA: WA_ITAB LIKE IT_ITAB1.
DATA:  CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
       CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'A',
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
*                 S_QMGRP FOR VIQMEL-QMGRP,
                 S_TXT04 FOR TJ30T-TXT04 OBLIGATORY,
                 S_LIFNUM FOR QMEL-LIFNUM.
*                 S_FEGRP FOR QMFE-FEGRP,
*                 S_FECOD FOR QMFE-FECOD.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-001.
SELECTION-SCREEN POSITION 33.
PARAMETERS: P_QMGRP LIKE VIQMEL-QMGRP,
            P_QMCOD LIKE VIQMEL-QMCOD MODIF ID A.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(12) TEXT-002.
*SELECTION-SCREEN POSITION 33.
**PARAMETERS: P_FEGRP type FEGRP,
**            P_FECOD type FECOD MODIF ID A.

PARAMETERS: P_FEGRPN(8) AS LISTBOX VISIBLE LENGTH 15 OBLIGATORY.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.
  PERFORM INIT_DATA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_QMGRP.
  PERFORM DISPLAY_SEARCH_HELP USING 'X' 'P_QMGRP' 'P_QMCOD'.


*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_QMGRPN.


*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FEGRP-LOW.
*  PERFORM DISPLAY_SEARCH_HELP USING 'W' 'S_FEGRP-LOW' 'S_FECOD-LOW'.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FEGRP-HIGH.
*  PERFORM DISPLAY_SEARCH_HELP USING 'W' 'S_FEGRP-HIGH' 'S_FECOD-HIGH'.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_QMCOD'    " OR SCREEN-NAME = 'P_FECOD'.
       OR SCREEN-NAME = 'S_FECOD-LOW' OR
       SCREEN-NAME = 'S_FECOD-HIGH'.
*       SCREEN-ACTIVE = 1.
      SCREEN-INPUT = 0.
      SCREEN-OUTPUT = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  PERFORM LIST_BOX_FEGRPN.

START-OF-SELECTION.
*  PERFORM CLEAR_DATA.
  IF S_QMNUM[] IS INITIAL AND S_TXT04[] IS INITIAL AND
     S_LIFNUM[] IS INITIAL.
    MESSAGE S009 WITH TEXT-M01.
  ELSE.
*    READ TABLE S_TXT04 INDEX 1.
*    IF S_TXT04-LOW = 'CRTD'.
    PERFORM GET_DATA.
    IF IT_ITAB1[] IS INITIAL.
      MESSAGE S009 WITH TEXT-M01.
    ELSE.
      CALL SCREEN 9000.
    ENDIF.
*    ELSE.
*      MESSAGE I009 WITH 'Incorrect Status, must be CRTD'.
*    ENDIF.
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
*  DATA: LT_TEMP LIKE TABLE OF IT_ITAB1 WITH HEADER LINE.

  DATA: BEGIN OF LT_TEMP OCCURS 0,
      SEL TYPE CHAR1,
      QMNUM LIKE QMEL-QMNUM,
      MATNR LIKE QMEL-MATNR,
      MAKTX LIKE MAKT-MAKTX,
      APPROVE TYPE CHAR1,
      REFUTE TYPE CHAR1,
      VENDER TYPE CHAR1,
      HMMA  TYPE CHAR1,
      STCK TYPE CHAR1,
      FECOD LIKE VIQMFE-FECOD,
      FECOD_TX(20),
      FEGRP LIKE VIQMFE-FEGRP,
      FEGRP_TX(20),
      CRCH TYPE CHAR1,
      LIFNUM LIKE QMEL-LIFNUM,
      QWRNUM LIKE QMEL-QWRNUM,
      CHANGE(4),
      COMMENTS(50),
      TXT04 LIKE TJ30T-TXT04,
      INACT LIKE JEST-INACT,
      ESTAT LIKE TJ30-ESTAT,
      ERDAT LIKE QMEL-ERDAT,
      RKMNG LIKE QMEL-RKMNG,
      END OF LT_TEMP.

  DATA: BEGIN OF LT_QMNUM OCCURS 0,
        QMNUM LIKE QMEL-QMNUM,
        END OF LT_QMNUM.
  DATA: L_FLAG_D(1),
        L_FLAG_SAVE(1),
        L_OBJNR LIKE QMEL-OBJNR,
        L_STAT LIKE JEST-STAT,
        L_INT TYPE I,
        L_REFUTE(1).

  CONSTANTS: C_STAT LIKE JEST-STAT VALUE '10068'.

  RANGES: S_QMGRP FOR QMEL-QMGRP,
*          S_FEGRP FOR QMFE-FEGRP,
          S_QMCOD FOR QMEL-QMCOD.
*          S_FECOD FOR QMFE-FECOD.

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

*  IF NOT P_FEGRP IS INITIAL.
*    S_FEGRP-SIGN = 'I'.
*    S_FEGRP-OPTION = 'EQ'.
*    S_FEGRP-LOW = P_FEGRP.
*    APPEND S_FEGRP.
*    S_FECOD-SIGN = 'I'.
*    S_FECOD-OPTION = 'EQ'.
*    S_FECOD-LOW = P_FECOD.
*    APPEND S_FECOD.
*  ENDIF.

  IF P_FEGRPN = 'ALL'.
    SELECT A~QMNUM A~MATNR A~LIFNUM A~QWRNUM FECOD FEGRP
      E~ESTAT TXT04 INACT
** Added by Furong on 05/26/09
      A~ERDAT A~RKMNG
** End of addition
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
*       AND TXT04 IN S_TXT04
        AND A~LIFNUM IN S_LIFNUM
*     AND FEGRP IN S_FEGRP
*     AND FECOD IN S_FECOD
        AND F~STSMA = 'ZQNSCRP1'
        AND F~SPRAS = 'EN'.
  ELSE.
    SELECT A~QMNUM A~MATNR A~LIFNUM A~QWRNUM FECOD FEGRP
       E~ESTAT TXT04 INACT
** Added by Furong on 05/26/09
       A~ERDAT A~RKMNG
** End of addition
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
*       AND TXT04 IN S_TXT04
         AND A~LIFNUM IN S_LIFNUM
          AND FEGRP = P_FEGRPN
*       AND FEGRP IN S_FEGRP
*       AND FECOD IN S_FECOD
         AND F~STSMA = 'ZQNSCRP1'
         AND F~SPRAS = 'EN'.
  ENDIF.
  LOOP AT LT_TEMP.
    LT_QMNUM-QMNUM = LT_TEMP-QMNUM.
    COLLECT LT_QMNUM.
  ENDLOOP.

  SORT LT_TEMP BY QMNUM ESTAT.

  LOOP AT LT_QMNUM.
** Furong on 05/15/12 for SAP tuning
*   LOOP AT LT_TEMP WHERE QMNUM = LT_QMNUM-QMNUM.
    READ TABLE LT_TEMP WITH KEY QMNUM = LT_QMNUM-QMNUM
           BINARY SEARCH.

    IF SY-SUBRC = 0.
      LOOP AT LT_TEMP FROM SY-TABIX.
        IF LT_TEMP-QMNUM <> LT_QMNUM-QMNUM.
          EXIT.
        ENDIF.
** End on 05/15/12
        IF LT_TEMP-ESTAT > 'E0002' AND LT_TEMP-ESTAT <> 'E0009'
                                   AND LT_TEMP-ESTAT <> 'E0008'.
** Furong on 06/05/12
          IF LT_TEMP-ESTAT = 'E0015'.
            IF LT_TEMP-INACT = ' '.
              L_REFUTE = 'X'.
            ENDIF.
          ELSE.
** End on 06/05/12
            L_FLAG_D = 'X'.
            EXIT.
          ENDIF.
        ELSE.
          IF LT_TEMP-TXT04 IN S_TXT04 AND LT_TEMP-INACT = ' '.
            SELECT SINGLE OBJNR INTO L_OBJNR
             FROM QMEL
             WHERE QMNUM = LT_QMNUM-QMNUM.

            SELECT SINGLE STAT INTO L_STAT
              FROM JEST AS A
              INNER JOIN TJ02T AS B
              ON A~STAT = B~ISTAT
              WHERE OBJNR = L_OBJNR
                AND TXT04 = 'OSNO'
                AND INACT = ' '.

*          SELECT SINGLE OBJNR INTO L_OBJNR
*            FROM QMEL AS A
*            INNER JOIN JEST AS B
*            ON A~OBJNR = B~OBJNR
**            INNER JOIN TJ30 AS c
**            ON b~STAT = c~ESTAT
*            WHERE QMNUM = LT_QMNUM-QMNUM
*              AND STAT = C_STAT
*              AND INACT = ' '.

            IF SY-SUBRC = 0.
*        IF LT_TEMP-ESTAT = 'E0001'.
              IT_ITAB1 = LT_TEMP.
              IT_ITAB1-RKMNG = L_INT =  LT_TEMP-RKMNG.
              WRITE: LT_TEMP-ERDAT TO IT_ITAB1-ERDAT MM/DD/YY.
              IF LT_TEMP-FEGRP = '0'.
                IT_ITAB1-VENDER = 'X'.
              ELSE.
                IF LT_TEMP-FEGRP = '99'.
                  IT_ITAB1-STCK = 'X'.
                ELSE.
                  IT_ITAB1-HMMA = 'X'.
                ENDIF.
              ENDIF.
              L_FLAG_SAVE = 'X'.
              SELECT SINGLE KURZTEXT INTO IT_ITAB1-FEGRP_TX
                FROM QPGT
                WHERE CODEGRUPPE = IT_ITAB1-FEGRP
                  AND SPRACHE = 'EN'.

              SELECT SINGLE KURZTEXT INTO IT_ITAB1-FECOD_TX
                FROM QPCT
                WHERE CODEGRUPPE = IT_ITAB1-FEGRP
                  AND CODE = IT_ITAB1-FECOD
                  AND SPRACHE = 'EN'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
** Furong on 05/15/12 for SAP tuning
    ENDIF.     "Addition
** End on 05/15/12
    IF L_FLAG_D IS INITIAL AND L_FLAG_SAVE = 'X'.
      SELECT SINGLE MAKTX INTO IT_ITAB1-MAKTX
        FROM MAKT
        WHERE MATNR = IT_ITAB1-MATNR
          AND SPRAS = 'E'.
      IF L_REFUTE = 'X'.
        IT_ITAB1-REFUTE = 'X'.
      ENDIF.
      APPEND IT_ITAB1.
    ENDIF.
    CLEAR: L_FLAG_D, L_FLAG_SAVE, LT_QMNUM, IT_ITAB1, LT_TEMP, L_REFUTE.
  ENDLOOP.
  SORT IT_ITAB1 BY QMNUM MATNR FEGRP LIFNUM.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'ST9000'.
  SET TITLEBAR 'ST9000'.
*  if it_itab1[] is initial.
*   perform make_dropdown_list.
*  endif.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  DATA: L_COMM LIKE SY-UCOMM.
  L_COMM = SY-UCOMM.
  CASE L_COMM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN  'POST'.
      PERFORM UPDATE_DATA.
      LEAVE TO SCREEN 0.
    WHEN 'QM03'.
      PERFORM DISPALY_QM03.
      CLEAR: SY-UCOMM.
    WHEN 'CHQN'.
      PERFORM CHANGE_QM02.
      CLEAR: SY-UCOMM.
  ENDCASE.
  CLEAR: L_COMM.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&spwizard: declaration of tablecontrol 'TC_9000' itself
CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*&spwizard: output module for tc 'TC_9000'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE TC_9000_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ITAB1 LINES TC_9000-LINES.
ENDMODULE.                    "TC_9000_CHANGE_TC_ATTR OUTPUT

*&spwizard: input module for tc 'TC_9000'. do not change this line!
*&spwizard: modify table
MODULE TC_9000_MODIFY INPUT.
  MODIFY IT_ITAB1
    INDEX TC_9000-CURRENT_LINE.
ENDMODULE.                    "TC_9000_MODIFY INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPALY_QM03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPALY_QM03.
  DATA: L_LINE TYPE I.
  GET CURSOR LINE L_LINE.
  L_LINE =  TC_9000-TOP_LINE + L_LINE - 1.
  READ TABLE IT_ITAB1 INDEX L_LINE.
*  READ TABLE IT_ITAB1 INDEX TC_9000-CURRENT_LINE.
  IF SY-SUBRC = 0.
    SET PARAMETER ID 'IQM' FIELD IT_ITAB1-QMNUM.
    CALL TRANSACTION 'QM03' AND SKIP FIRST SCREEN.
  ELSE.
    MESSAGE S000 WITH 'No data selected'.
  ENDIF.
ENDFORM.                    " DISPALY_QM03
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

    IF IT_ITAB1-APPROVE = 'X'.
      PERFORM UPDATE_STATUS TABLES IT_TLINE.
      READ TABLE IT_TLINE WITH KEY TDFORMAT = 'E'.
      IF SY-SUBRC = 0.
        MESSAGE I009 WITH IT_TLINE-TDLINE.
      ENDIF.
      REFRESH IT_TLINE.
      CLEAR: IT_TLINE.
** Furong on 06/05/12 for refute
    ELSEIF IT_ITAB1-REFUTE = 'X'.
      PERFORM UPDATE_STATUS_REFUTE TABLES IT_TLINE.
      READ TABLE IT_TLINE WITH KEY TDFORMAT = 'E'.
      IF SY-SUBRC = 0.
        MESSAGE I000 WITH IT_TLINE-TDLINE.
      ENDIF.
      REFRESH IT_TLINE.
      CLEAR: IT_TLINE.
** End on 06/05/12
    ENDIF.
  ENDLOOP.
*  LOOP AT IT_TLINE.
*    MESSAGE I000 WITH IT_TLINE-TDLINE.
*  ENDLOOP.

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
    CONCATENATE 'Status' MSG INTO P_IT_TLINE-TDLINE
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
      CONCATENATE 'Status' MSG INTO P_IT_TLINE-TDLINE
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

  IF NOT IT_ITAB1-VENDER IS INITIAL.
    L_STATUS = '04'.
    L_TXT04 = 'VEND'.
    L_STAT = 'E0004'.
  ENDIF.
  IF NOT IT_ITAB1-HMMA IS INITIAL.
    L_STATUS = '05'.
    L_TXT04 = 'HMMA'.
    L_STAT = 'E0005'.
  ENDIF.
  IF NOT IT_ITAB1-STCK IS INITIAL.
    L_STATUS = '06'.
    L_TXT04 = 'STCK'.
    L_STAT = 'E0006'.
  ENDIF.

** Changed by Furong on 03/04/09
  PERFORM UPDATE_SYS_USER_STATUS TABLES IT_TLINE USING L_STATUS.
  .
** End of change on 03/04/09

*  CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
*     EXPORTING
*         I_QMNUM                    = IT_ITAB1-QMNUM
*         I_USER_STAT_INTERN         = L_STAT
*         I_USER_STAT_EXTERN         = L_TXT04
*         I_LANGU                    = SY-LANGU
** IMPORTING
**   E_USER_STAT_INTERN         =
**   E_USER_STAT_EXTERN         =
*     EXCEPTIONS
*         FOREIGN_LOCK               = 1
**   SYST_FAILURE               = 2
*         INVALID_NOTIFICATION       = 3
**   OBJECT_NOT_FOUND           = 4
*         STATUS_INCONSISTANT        = 5
*         STATUS_NOT_ALLOWED         = 6
**   OTHERS                     = 7
*        .
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    CONCATENATE 'User Status:' IT_ITAB1-QMNUM SY-MSGV1 INTO MSG
*           SEPARATED BY SPACE.
*  ELSE.
**    MESSAGE I000 WITH TEXT-M02.
*    COMMIT WORK AND WAIT.
*    CONCATENATE  TEXT-M02 IT_ITAB1-QMNUM INTO MSG
*           SEPARATED BY SPACE.
*    PERFORM RELEASE_LOCK USING IT_ITAB1-QMNUM.
*    PERFORM UPDATE_SYSTEM_STATUS.
*  ENDIF.
*  P_IT_TLINE-TDFORMAT = 'U' .
*  P_IT_TLINE-TDLINE = MSG.
*  APPEND P_IT_TLINE.
*  CLEAR P_IT_TLINE.
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
  XVALUE-TEXT = '(24,26) Ship/In'.
  APPEND XVALUE TO XLIST.
* SHIP/OUT(25)
  XVALUE-KEY  = '25'.
  XVALUE-TEXT = '(25,27) Ship/Out'.
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
*&---------------------------------------------------------------------*
*&      Form  CHANGE_QM02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_QM02.
  DATA: L_LINE TYPE I.
  DATA: L_QMNUM LIKE QMFE-QMNUM,
        L_FEGRP LIKE QMFE-FEGRP,
        L_FECOD LIKE QMFE-FECOD.
  DATA: L_NAME LIKE	THEAD-TDNAME,
        L_OBJECT LIKE THEAD-TDOBJECT,
        L_URTXT LIKE QMUR-URTXT.

  DATA: LT_TLINE LIKE TABLE OF TLINE WITH HEADER LINE.

  GET CURSOR LINE L_LINE.
  L_LINE =  TC_9000-TOP_LINE + L_LINE - 1.
  READ TABLE IT_ITAB1 INDEX L_LINE.
  L_QMNUM =  IT_ITAB1-QMNUM.
*  READ TABLE IT_ITAB1 INDEX TC_9000-CURRENT_LINE.
  IF SY-SUBRC = 0.
    SET PARAMETER ID 'IQM' FIELD IT_ITAB1-QMNUM.
    CALL TRANSACTION 'QM02' AND SKIP FIRST SCREEN.
  ELSE.
    MESSAGE S009 WITH 'No Data Selected'.
  ENDIF.

  WAIT UP TO 1 SECONDS.

** Changed by Furong on 06/01/09

*  SELECT SINGLE FEGRP FECOD INTO (IT_ITAB1-FEGRP, IT_ITAB1-FECOD)
*   FROM QMFE
*   WHERE QMNUM = L_QMNUM.


  CONCATENATE IT_ITAB1-QMNUM '00010001' INTO L_NAME.
  DO.
    CLEAR: L_URTXT, LT_TLINE[].

    SELECT SINGLE FEGRP FECOD INTO (L_FEGRP, L_FECOD)
     FROM QMFE
     WHERE QMNUM = L_QMNUM.
    IF L_FEGRP <> IT_ITAB1-FEGRP AND IT_ITAB1-FEGRP = '0'.
      SELECT SINGLE URTXT INTO L_URTXT
     FROM QMUR
     WHERE QMNUM = L_QMNUM.
      IF NOT L_URTXT IS INITIAL.
        EXIT.
      ENDIF.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*        CLIENT                        = SY-MANDT
          ID                            = 'LTQM'
          LANGUAGE                      = SY-LANGU
          NAME                          =  L_NAME
          OBJECT                        = 'QMUR'
*        ARCHIVE_HANDLE                = 0
*        LOCAL_CAT                     = ' '
*      IMPORTING
*        HEADER                        =
        TABLES
          LINES                         =  LT_TLINE
      EXCEPTIONS
        ID                            = 1
        LANGUAGE                      = 2
        NAME                          = 3
        NOT_FOUND                     = 4
        OBJECT                        = 5
        REFERENCE_CHECK               = 6
        WRONG_ACCESS_TO_ARCHIVE       = 7
        OTHERS                        = 8
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        MESSAGE I009 WITH ' Please enter Cause Long Text'.
        CALL TRANSACTION 'QM02' AND SKIP FIRST SCREEN.
        WAIT UP TO 1 SECONDS.
        CONTINUE.
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  IT_ITAB1-FEGRP = L_FEGRP.
  IT_ITAB1-FECOD = L_FECOD.
** End of change

  IF IT_ITAB1-FEGRP = '0'.
    IT_ITAB1-VENDER = 'X'.
    CLEAR: IT_ITAB1-HMMA, IT_ITAB1-STCK.
  ELSE.
    IF IT_ITAB1-FEGRP = '99'.
      IT_ITAB1-STCK = 'X'.
      CLEAR: IT_ITAB1-VENDER, IT_ITAB1-HMMA.
    ELSE.

      IT_ITAB1-HMMA = 'X'.
      CLEAR: IT_ITAB1-VENDER, IT_ITAB1-STCK.
    ENDIF.
  ENDIF.
  SELECT SINGLE KURZTEXT INTO IT_ITAB1-FEGRP_TX
    FROM QPGT
    WHERE CODEGRUPPE = IT_ITAB1-FEGRP
      AND SPRACHE = 'EN'.

  SELECT SINGLE KURZTEXT INTO IT_ITAB1-FECOD_TX
    FROM QPCT
    WHERE CODEGRUPPE = IT_ITAB1-FEGRP
      AND CODE = IT_ITAB1-FECOD
      AND SPRACHE = 'EN'.

  MODIFY IT_ITAB1 INDEX L_LINE TRANSPORTING FEGRP FECOD
         FEGRP_TX FECOD_TX VENDER HMMA STCK.


ENDFORM.                    " CHANGE_QM02
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_QMGRPN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_BOX_FEGRPN.
  DATA: NAME TYPE VRM_ID,   " Field Name
        XLIST TYPE VRM_VALUES,
        XVALUE LIKE LINE OF XLIST.


  MOVE 'ALL' TO XVALUE-KEY.
  MOVE 'For all' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '0' TO XVALUE-KEY.
  MOVE 'Supplier Defect' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '1' TO XVALUE-KEY.
  MOVE ' Stamping' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '2' TO XVALUE-KEY.
  MOVE 'Welding' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '3' TO XVALUE-KEY.
  MOVE 'Paint' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '4' TO XVALUE-KEY.
  MOVE 'Assembly' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '5' TO XVALUE-KEY.
  MOVE 'Engine' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '6' TO XVALUE-KEY.
  MOVE 'Quality' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '9' TO XVALUE-KEY.
  MOVE 'Administration' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  CLEAR XVALUE.
  MOVE '99' TO XVALUE-KEY.
  MOVE 'No Defect Found' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.

  NAME = 'P_FEGRPN'.

* LIST BOX SETTING
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = NAME  " list box
      VALUES          = XLIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.

  IF P_FEGRPN IS INITIAL.
    READ TABLE XLIST INTO XVALUE  INDEX 1.
    P_FEGRPN = XVALUE-KEY.
  ENDIF.

ENDFORM.                    " LIST_BOX_QMGRPN
*&---------------------------------------------------------------------*
*&      Module  tc_9000_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9000_CHANGE_FIELD_ATTR OUTPUT.
* if IT_ITAB1-hmma = 'X'.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'IT_ITAB1-HMMA'.
      IF IT_ITAB1-HMMA <> 'X'.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    IF SCREEN-NAME = 'IT_ITAB1-VENDER'.
      IF IT_ITAB1-VENDER <> 'X'.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    IF SCREEN-NAME = 'IT_ITAB1-STCK'.
      IF IT_ITAB1-STCK <> 'X'.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    IF SCREEN-NAME = 'IT_ITAB1-REFUTE'.
      IF IT_ITAB1-REFUTE = 'X'.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " tc_9000_change_field_attr  OUTPUT
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
  CASE P_USER_STATUS.

    WHEN '04'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                   'J_STMAINT-ANWS(04)'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(04)'
                                'X'.
    WHEN '05'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'J_STMAINT-ANWS(05)'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(05)'
                                 'X'.
    WHEN '06'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'ANWS_STONR(01)'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=A+'.

      PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0201'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'J_STMAINT-ANWS(04)'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(04)'
                                 'X'.
    WHEN OTHERS.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'J_STMAINT-ANWS(02)'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(02)'
                                 'X'.
  ENDCASE.
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
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STATUS_REFUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*----------------------------------------------------------------------*
FORM UPDATE_STATUS_REFUTE TABLES P_IT_TLINE STRUCTURE TLINE.
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
                                'J_STMAINT-ANWSO(05)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=OKAY'.
  PERFORM BDC_FIELD       USING 'J_STMAINT-ANWSO(05)'
                                 'X'.

  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=OKAY'.


  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM BDC_TRANSACTION TABLES IT_TLINE
                          USING 'QM02'.

ENDFORM.                    " UPDATE_STATUS_REFUTE
