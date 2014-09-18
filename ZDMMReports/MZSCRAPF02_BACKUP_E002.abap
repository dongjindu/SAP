*---------------------------------------------------------------------*
*       FORM DISPLAY_VALUES                                           *
*---------------------------------------------------------------------*
*                                                          *
*---------------------------------------------------------------------*
FORM DISPLAY_VALUES.
  DATA: IT_REPTAG LIKE TABLE OF ZSCRAP_REPTAG WITH HEADER LINE.
  DATA:  L_STAT LIKE JEST-STAT,
           L_TXT30 LIKE TJ30T-TXT30,
           L_STSMA LIKE JSTO-STSMA,
           L_DATE LIKE SY-DATUM.
  DATA : DYNAME LIKE D020S-PROG , DYNUMB LIKE D020S-DNUM .
  DATA:  RET_TAB LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE,
         IT_DSEL LIKE DSELC OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF IT_DYNPFIELDS OCCURS 3.
          INCLUDE STRUCTURE DYNPREAD.
  DATA: END OF IT_DYNPFIELDS.

  DYNAME = 'SAPMZSCRAP'.
  DYNUMB = '1801'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_QMNUM
       IMPORTING
            OUTPUT = P_QMNUM.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
       EXPORTING
            INPUT  = QMEL-QMDAT
       IMPORTING
            OUTPUT = L_DATE.

  IF P_QMNUM IS INITIAL.
    IF P_MATNR IS INITIAL.
      IF P_NUSER IS INITIAL AND QMEL-QMDAT IS INITIAL.
        MESSAGE I026 WITH 'No data selected'.
        EXIT.
      ENDIF.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
      FROM QMEL.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
*      FROM QMEL
*      WHERE MATNR = P_MATNR
*      AND AEDAT = L_DATE
*      AND AENAM = P_NUSER.
*      IF SY-SUBRC <> 0.
*        EXIT.
*      ENDIF.
    ELSE.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
               FROM QMEL
              WHERE MATNR = P_MATNR.
    ENDIF.
    IF NOT P_NUSER IS INITIAL.
      DELETE IT_REPTAG WHERE AENAM <> P_NUSER.
    ENDIF.
    IF NOT QMEL-QMDAT IS INITIAL.
      DELETE IT_REPTAG WHERE AEDAT <> QMEL-QMDAT.
    ENDIF.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
               FROM QMEL
              WHERE QMNUM = P_QMNUM.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ENDIF.

  LOOP AT IT_REPTAG.
    SELECT SINGLE STAT FROM JEST INTO L_STAT
                       WHERE OBJNR = IT_REPTAG-OBJNR
                         AND INACT = ' '.
    SELECT SINGLE STSMA FROM JSTO INTO L_STSMA
                       WHERE OBJNR = JEST-OBJNR.
    SELECT SINGLE TXT30 FROM TJ30T INTO L_TXT30
                       WHERE STSMA = L_STSMA
                         AND ESTAT = L_STAT.
    IT_REPTAG-TXT30 = L_TXT30.
    MODIFY IT_REPTAG.
    CLEAR: IT_REPTAG, L_STAT, L_TXT30, L_STSMA.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM IT_REPTAG.
  IT_DSEL-FLDNAME = 'QMNUM'.
  IT_DSEL-DYFLDNAME = 'P_QMNUM'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MATNR'.
  IT_DSEL-DYFLDNAME = 'P_MATNR'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'AENAM'.
  IT_DSEL-DYFLDNAME = 'P_USER'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'AEDAT'.
  IT_DSEL-DYFLDNAME = 'QMEL-QMDAT'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MZEIT'.
  IT_DSEL-DYFLDNAME = 'P_NTIME'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'TXT30'.
  IT_DSEL-DYFLDNAME = 'P_STATUS'.
  APPEND IT_DSEL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    DDIC_STRUCTURE         = 'ZSCRAP_REPTAG'
       RETFIELD               = 'QMEL_QMNUM'
*   PVALKEY                = ' '
     DYNPPROG               = DYNAME
     DYNPNR                 = DYNUMB
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     VALUE_TAB              = IT_REPTAG
*    field_tab             = fld_tab
     RETURN_TAB             = RET_TAB
     DYNPFLD_MAPPING        = IT_DSEL
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT RET_TAB.
    CASE RET_TAB-FIELDNAME.
      WHEN 'QMNUM'.
        P_QMNUM = RET_TAB-FIELDVAL.
      WHEN 'MATNR'.
        P_MATNR = RET_TAB-FIELDVAL.
      WHEN 'AENAM'.
        P_NUSER = RET_TAB-FIELDVAL.
      WHEN 'AEDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
             EXPORTING
                  INPUT  = RET_TAB-FIELDVAL
             IMPORTING
                  OUTPUT = QMEL-QMDAT.
      WHEN 'MZEIT'.
        P_NTIME = RET_TAB-FIELDVAL.
      WHEN 'TXT30'.
        P_STATUS = RET_TAB-FIELDVAL.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  find_QMNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_QMNUM.
  DATA: IT_REPTAG LIKE TABLE OF ZSCRAP_REPTAG WITH HEADER LINE.
  DATA:  L_STAT LIKE JEST-STAT,
         L_TXT30 LIKE TJ30T-TXT30,
         L_STSMA LIKE JSTO-STSMA,
         L_DATE LIKE SY-DATUM.
  DATA : DYNAME LIKE D020S-PROG , DYNUMB LIKE D020S-DNUM .
  DATA:  RET_TAB LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE,
         IT_DSEL LIKE DSELC OCCURS 0 WITH HEADER LINE.

  DYNAME = 'SAPMZSCRAP'.
  DYNUMB = '1801'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_QMNUM
       IMPORTING
            OUTPUT = P_QMNUM.

  IF P_QMNUM IS INITIAL.
    IF P_MATNR IS INITIAL.
      IF P_NUSER IS INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
        FROM QMEL.
      ELSE.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
           FROM QMEL
           WHERE AENAM = P_NUSER.
      ENDIF.
      IF NOT QMEL-QMDAT IS INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*             EXPORTING
*                  INPUT  = QMEL-QMDAT
*             IMPORTING
*                  OUTPUT = L_DATE.
        DELETE IT_REPTAG WHERE AEDAT <> QMEL-QMDAT.
      ENDIF.
    ELSE.
      DO.
        REPLACE '*' WITH '%' INTO P_MATNR.
        IF SY-SUBRC <> 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF  ( QMEL-QMDAT = '00000000' OR QMEL-QMDAT = ' ' )
          AND P_NUSER IS INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
        FROM QMEL
        WHERE MATNR LIKE P_MATNR.
      ELSE.
        IF  QMEL-QMDAT = '00000000' OR QMEL-QMDAT = ' '.
          DO.
            REPLACE '*' WITH '%' INTO P_NUSER.
            IF SY-SUBRC <> 0.
              EXIT.
            ENDIF.
          ENDDO.
          SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
            FROM QMEL
           WHERE MATNR LIKE P_MATNR
             AND AENAM LIKE P_NUSER.
        ELSE.
          IF P_NUSER IS INITIAL.
*            CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                 EXPORTING
*                      INPUT  = QMEL-QMDAT
*                 IMPORTING
*                      OUTPUT = L_DATE.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
             FROM QMEL
            WHERE MATNR LIKE P_MATNR
              AND AEDAT = QMEL-QMDAT.
          ELSE.
            DO.
              REPLACE '*' WITH '%' INTO P_NUSER.
              IF SY-SUBRC <> 0.
                EXIT.
              ENDIF.
            ENDDO.
*            CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                 EXPORTING
*                      INPUT  = QMEL-QMDAT
*                 IMPORTING
*                      OUTPUT = L_DATE.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
              FROM QMEL
             WHERE MATNR LIKE P_MATNR
               AND AEDAT = QMEL-QMDAT
               AND AENAM LIKE P_NUSER.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    DO.
      REPLACE '*' WITH '%' INTO P_QMNUM.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
    ENDDO.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
               FROM QMEL
              WHERE QMNUM LIKE P_QMNUM.
  ENDIF.

  LOOP AT IT_REPTAG.
    SELECT SINGLE STAT FROM JEST INTO L_STAT
                       WHERE OBJNR = IT_REPTAG-OBJNR
                         AND INACT = ' '.
    SELECT SINGLE STSMA FROM JSTO INTO L_STSMA
                       WHERE OBJNR = JEST-OBJNR.
    SELECT SINGLE TXT30 FROM TJ30T INTO L_TXT30
                       WHERE STSMA = L_STSMA
                         AND ESTAT = L_STAT.
    IT_REPTAG-TXT30 = L_TXT30.
    MODIFY IT_REPTAG.
    CLEAR: IT_REPTAG, L_STAT, L_TXT30, L_STSMA.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM IT_REPTAG.

  IT_DSEL-FLDNAME = 'QMNUM'.
  IT_DSEL-DYFLDNAME = 'P_QMNUM'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MATNR'.
  IT_DSEL-DYFLDNAME = 'P_MATNR'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'AENAM'.
  IT_DSEL-DYFLDNAME = 'P_USER'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'AEDAT'.
  IT_DSEL-DYFLDNAME = 'QMEL-QMDAT'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MZEIT'.
  IT_DSEL-DYFLDNAME = 'P_NTIME'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'TXT30'.
  IT_DSEL-DYFLDNAME = 'P_STATUS'.
  APPEND IT_DSEL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    DDIC_STRUCTURE         = 'ZSCRAP_REPTAG'
       RETFIELD               = 'QMEL_QMNUM'
*   PVALKEY                = ' '
     DYNPPROG               = DYNAME
     DYNPNR                 = DYNUMB
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     VALUE_TAB              = IT_REPTAG
*    field_tab             = fld_tab
     RETURN_TAB             = RET_TAB
     DYNPFLD_MAPPING        = IT_DSEL
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT RET_TAB.
    CASE RET_TAB-FIELDNAME.
      WHEN 'QMNUM'.
        P_QMNUM = RET_TAB-FIELDVAL.
      WHEN 'MATNR'.
        P_MATNR = RET_TAB-FIELDVAL.
      WHEN 'AENAM'.
        P_NUSER = RET_TAB-FIELDVAL.
      WHEN 'AEDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
             EXPORTING
                  INPUT  = RET_TAB-FIELDVAL
             IMPORTING
                  OUTPUT = QMEL-QMDAT.
      WHEN 'MZEIT'.
        P_NTIME = RET_TAB-FIELDVAL.
      WHEN 'TXT30'.
        P_STATUS = RET_TAB-FIELDVAL.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " find_QMNUM
*&---------------------------------------------------------------------*
*&      Form  rePRINT_LABEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REPRINT_LABEL.
*DATA: BEGIN OF LT_MESS OCCURS 5,
*      MESS(255),
*      END OF LT_MESS.
*DATA: L_MESS LIKE LT_MESS.
  DATA: L_MESS(255).
  IF P_QMNUM IS INITIAL AND
     P_MATNR IS INITIAL.
    EXIT.
  ELSEIF P_QMNUM IS INITIAL OR P_MATNR IS INITIAL.
    PERFORM DISPLAY_VALUES.
  ENDIF.
  SET PARAMETER ID 'IQM' FIELD P_QMNUM.
  SET PARAMETER ID 'MAT' FIELD P_MATNR.
  CLEAR: IT_QMNUM, IT_QMNUM[].
  IT_QMNUM-QMNUM = P_QMNUM.
  IT_QMNUM-MATNR = P_MATNR.
  APPEND IT_QMNUM.
  EXPORT IT_QMNUM TO MEMORY ID 'M1'.
  DO P_COPY TIMES.
    SUBMIT ZQMSCRAP_LABEL AND RETURN.
    IMPORT L_MESS FROM MEMORY ID 'ME'.
    IF NOT L_MESS IS INITIAL.
      MESSAGE I026 WITH L_MESS.
    ELSE.
      MESSAGE S026 WITH 'Reprint label complete'.
    ENDIF.
    FREE MEMORY ID 'ME'.
  ENDDO.
ENDFORM.                    " rePRINT_LABEL
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  IF P_COPY IS INITIAL.
    P_COPY = 1.
  ENDIF.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VALUE.
  CLEAR: P_QMNUM, P_MATNR, P_NUSER, P_NDATE, P_NTIME, P_STATUS,
         QMEL-QMDAT.
ENDFORM.                    " CLEAR_VALUE
*&---------------------------------------------------------------------*
*&      Form  release_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM RELEASE_LOCK USING    P_QMNUM.
  WAIT UP TO 1 SECONDS.
  IF NOT P_QMNUM IS INITIAL.
    CALL FUNCTION 'DEQUEUE_EIQMEL'
     EXPORTING
*      MODE_QMEL       = 'E'
*      MANDT           = SY-MANDT
        QMNUM           = P_QMNUM
*      X_QMNUM         = ' '
*      _SCOPE          = '3'
*      _SYNCHRON       = ' '
*      _COLLECT        = ' '
              .

  ENDIF.

ENDFORM.                    " release_lock
*&---------------------------------------------------------------------*
*&      Form  code_check1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CODE_CHECK1.
  IF QMNUM IS INITIAL.
    MESSAGE E024.
  ELSE.
    SELECT SINGLE * FROM QMEL WHERE QMNUM = QMNUM.
    IF SY-SUBRC <> 0.
      MESSAGE E025.
    ENDIF.
  ENDIF.
ENDFORM.                    " code_check1
*&---------------------------------------------------------------------*
*&      Form  GET_CURSOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CURSOR.
  GET CURSOR FIELD  P_CURSOR.
  IF  P_CURSOR = 'RKMNG'.
    P_CURSOR = 'RKMNG'.
  ENDIF.
ENDFORM.                    " GET_CURSOR
*&---------------------------------------------------------------------*
*&      Form  update_nopr_hmma
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_NOPR_HMMA TABLES IT_TLINE STRUCTURE TLINE
                      USING P_V_QMNUM.
  DATA: MSG(60).

*    L_TXT04 = 'HMMA'.
*    L_STAT = 'E0005'.

  CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
     EXPORTING
         I_QMNUM                    = P_V_QMNUM
         I_USER_STAT_INTERN         = 'E0005'
         I_USER_STAT_EXTERN         = 'HMMA'
         I_LANGU                    = SY-LANGU
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
     EXCEPTIONS
         FOREIGN_LOCK               = 1
*   SYST_FAILURE               = 2
         INVALID_NOTIFICATION       = 3
*   OBJECT_NOT_FOUND           = 4
         STATUS_INCONSISTANT        = 5
         STATUS_NOT_ALLOWED         = 6
*   OTHERS                     = 7
        .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CONCATENATE 'User Status:' P_V_QMNUM SY-MSGV1 INTO MSG
           SEPARATED BY SPACE.
  ELSE.
*    MESSAGE I000 WITH TEXT-M02.
    COMMIT WORK AND WAIT.
    CONCATENATE 'User status HMMA sucessfully updated' P_V_QMNUM
           INTO MSG SEPARATED BY SPACE.
    PERFORM RELEASE_LOCK USING P_V_QMNUM.
    PERFORM UPDATE_SYSTEM_STATUS TABLES IT_TLINE
                                USING P_V_QMNUM.
  ENDIF.
  IT_TLINE-TDFORMAT = 'U' .
  IT_TLINE-TDLINE = MSG.
  APPEND IT_TLINE.
  CLEAR IT_TLINE.
ENDFORM.                    " update_nopr_hmma
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SYSTEM_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_SYSTEM_STATUS TABLES IT_TLINE  STRUCTURE  TLINE
                          USING P_V_QMNUM.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                P_V_QMNUM.
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

ENDFORM.                    " UPDATE_SYSTEM_STATUS
*&---------------------------------------------------------------------*
*&      Form  clear_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_SCREEN_1003.
  CLEAR: QMNUM,VN_MATNR,ERNAM,MZEIT,TJ30T-TXT30, IT_QMNUM..
  REFRESH: IT_QMNUM.
ENDFORM.                    " clear_screen
*&---------------------------------------------------------------------*
*&      Form  clear_screen_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_SCREEN_1001.
  CLEAR: MATNR,MAKT-MAKTG,MARD-LGPBE,LFA1-LIFNR,MARA-MFRPN,MARC-VSPVB,
LFA1-NAME1,LGORT, MDOC.
  CLEAR: RMMG1-LGNUM,T001L-LGORT,RMMG1-LGORT,QMGRP,QMCOD,RKMNG,
  BWART,RM07M-GRUND,RIWO00-QMART, IT_QMNUM,ITOBATTR-EQUNR,
  OTGRP,OTEIL,FEGRP,FECOD,URGRP,URCOD, P_UCOMM, CUASETXT,QMEL-ERDAT,C3,
  QMNUM, LIFNR.
  REFRESH: IT_QMNUM.
** LTAK-BWLVS,COBL-AUFNR,
ENDFORM.                    " clear_screen_1001
*&---------------------------------------------------------------------*
*&      Form  PROCESS_1004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_1004.
  CLEAR EFLAG.

  DATA: V_LGTYP LIKE PKHD-LGTYP,
        V_LGPLA LIKE PKHD-LGPLA,
        V_VSPVB LIKE MARC-VSPVB,
        V_LGBER LIKE LAGP-LGBER,
        V_VERME LIKE LQUA-VERME,
        V_NEGAT TYPE T331-NEGAT,
        V_QMNUM LIKE QMEL-QMNUM,
        V_LTKZE LIKE MLGN-LTKZE.

  DATA: V_SUB(20) TYPE C,
  L_ATWRT LIKE AUSP-ATWRT.

  DATA: IT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.

  CLEAR: IT_TLINE, IT_TLINE[].
  IF RKMNG IS INITIAL.
    MESSAGE E026 WITH 'Quantity is required'.
  ENDIF.
  IF ITOBATTR-EQUNR IS INITIAL.
    MESSAGE E026 WITH 'Body # is required'.
  ENDIF.

  IF NOT ITOBATTR-EQUNR IS INITIAL.
    L_EQUNR = ITOBATTR-EQUNR.
    IF L_EQUNR+3(1) = ' '.
      CONCATENATE L_EQUNR+0(3) L_EQUNR+4(14) INTO L_EQUNR.
      ITOBATTR-EQUNR = L_EQUNR.
    ENDIF.
    SELECT SINGLE EQUNR INTO L_EQUNR
    FROM EQUI
      WHERE EQUNR = L_EQUNR.
    IF SY-SUBRC = 0.
    ELSE.
      MESSAGE E026 WITH 'Invalid body number entered'.
    ENDIF.
  ENDIF.

** Added on 09/15/09 by furong; Check RP>=19
  IF W_CALL = 'VPC'.
    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP AS A
      INNER JOIN CABN AS B
      ON A~ATINN = B~ATINN
      WHERE A~OBJEK = ITOBATTR-EQUNR
        AND KLART = '002'
        AND ATNAM = 'P_RP_STATUS'.

    IF L_ATWRT < 19.
      MESSAGE E026 WITH 'RP status is invalid for scrapping'.
    ENDIF.
  ENDIF.
*  PERFORM CODE_CHECK.

*  IT_TLINE-TDFORMAT = '*' .
*  CONCATENATE 'Plant' MARC-WERKS INTO IT_TLINE-TDLINE.
*  APPEND IT_TLINE.
*  CLEAR IT_TLINE.
*
*  IT_TLINE-TDFORMAT = '*' .
*  CONCATENATE 'Storage Location' T001L-LGORT INTO IT_TLINE-TDLINE.
*  APPEND IT_TLINE.
*  CLEAR IT_TLINE.

  IF T001L-LGORT = 'P400'.

    PERFORM CREAT_MVT_201 TABLES IT_TLINE   " creation of movement
                          USING ITOBATTR-EQUNR.

    SELECT SINGLE VSPVB INTO V_VSPVB FROM MARC
                                     WHERE MATNR = MATNR
                                       AND WERKS = MARC-WERKS.

    SELECT SINGLE LGTYP LGPLA  INTO (V_LGTYP, V_LGPLA) FROM PKHD
                               WHERE MATNR = MATNR
                                 AND WERKS = MARC-WERKS
                                 AND PRVBE = V_VSPVB.

    IF ( V_LGTYP IS INITIAL AND V_LGPLA IS INITIAL ).

*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO V_LGPLA.
      MOVE '999' TO V_LGTYP.

    ELSE.

      IF RMMG1-LGNUM IS INITIAL.
        RMMG1-LGNUM = 'P01'.
      ENDIF.
      SELECT SINGLE LGBER INTO V_LGBER FROM LAGP
                                       WHERE LGNUM = RMMG1-LGNUM
                                         AND LGTYP = V_LGTYP
                                         AND LGPLA = V_LGPLA.


      SELECT SINGLE VERME INTO V_VERME FROM LQUA
                            WHERE LGNUM = RMMG1-LGNUM
                              AND MATNR = MATNR
                              AND LGTYP = V_LGTYP
                              AND LGPLA = V_LGPLA.

      IF V_VERME < RKMNG.

        SELECT SINGLE NEGAT FROM T331 INTO V_NEGAT
                    WHERE LGNUM = RMMG1-LGNUM AND LGTYP = V_LGTYP.

        IF V_NEGAT = 'X'.

        ELSE.

*          MOVE 'INV_COUNT' TO V_LGPLA.
          MOVE 'HMMA ADJ' TO V_LGPLA.
          MOVE '999' TO V_LGTYP.
        ENDIF.

      ENDIF.

    ENDIF.

** Furong on 07/19/11
*    IF EFLAG IS INITIAL.
*
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                            USING V_VSPVB V_LGTYP V_LGPLA V_LGBER.
*
*    ENDIF.
** End on 07/19/11

*  ELSEIF T001L-LGORT = 'P500'.
  ELSE.

    PERFORM CREAT_MVT_201 TABLES IT_TLINE     " creation of movement
                                 USING ITOBATTR-EQUNR.
  ENDIF.

  CLEAR EFLAG.

  READ TABLE IT_TLINE INDEX 1.
  MESSAGE S026 WITH IT_TLINE-TDLINE.

*  IF NOT LFA1-LIFNR IS INITIAL.
*
*    PERFORM CREAT_NOTIF.                  " creation of notification
*
*  ELSE.
*
*    IT_TLINE-TDLINE = 'Vendor Not available'.
*    APPEND IT_TLINE.
*    CLEAR IT_TLINE.
*
*    PERFORM CREAT_NOTIF1.                " creation of notification
*
*  ENDIF.

*  READ TABLE IT_QMNUM INDEX 1.
*  V_QMNUM = IT_QMNUM-QMNUM.
*
*  PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE    " updating QN# long text
*                            USING V_QMNUM.
*  READ TABLE IT_TLINE INDEX 3.
*
*  IF IT_TLINE-TDLINE+0(5) = 'ERROR'.
*    PERFORM UPDATE_NOTIF_STATUS USING V_QMNUM. " updating QN# status
*  ELSE.
*    READ TABLE IT_TLINE INDEX 4.
*    IF IT_TLINE-TDLINE+0(5) = 'ERROR'.
*      PERFORM UPDATE_NOTIF_STATUS USING V_QMNUM.  " updating QN# status
*    ELSE.
*** Changed by Furong on 03/03/09
*      IF FEGRP = '4' AND URGRP <> '04'.
*
*        CLEAR: IT_TLINE, IT_TLINE[].
*        PERFORM UPDATE_NOPR_HMMA TABLES IT_TLINE
*                                 USING V_QMNUM.
*        WAIT UP TO 2 SECONDS.
*     PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE   "updating QN# long
*text
*                                                      USING V_QMNUM.
*      ENDIF.
*** End of change
*    ENDIF.
*ENDIF.

********  Printing Label ********

*  PERFORM PRINT_LABEL.

*****   Send mail logic  ********
*  IF T001L-LGORT = 'P400'.
*
*    IF V_LGTYP = '422'.
*
*      SELECT SINGLE LTKZE FROM MLGN INTO V_LTKZE
*                        WHERE MATNR = MATNR AND LGNUM = RMMG1-LGNUM.
*      IF V_LTKZE = '003'.
*
*        IF QMGRP = 'MXTX53'.
*
*          V_SUB = 'PDI reorder'.
*
*          PERFORM SEND_MAIL USING V_SUB.
*
*        ELSE.
*
*          V_SUB = 'Line side Reorder'.
*
*          PERFORM SEND_MAIL USING V_SUB.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*  ELSEIF T001L-LGORT = 'P500'.
*
*    IF QMGRP = 'MXTX53'.
*
*      V_SUB = 'PDI reorder'.
*
*      PERFORM SEND_MAIL USING V_SUB.
*
*    ELSE.
*
*      V_SUB = 'Line side Reorder'.
*
*      PERFORM SEND_MAIL USING V_SUB.
*    ENDIF.
*
*  ENDIF.

  IF T001L-LGORT = 'P500'.
    V_SUB = 'Manager Vehicle Repair Reorder'.
    PERFORM SEND_MAIL_1004 USING V_SUB.
  ENDIF.

  CLEAR: V_LGTYP, V_LGPLA, V_VSPVB, V_LGBER, V_SUB, V_VERME,
         V_QMNUM,V_LTKZE,V_QMNUM,V_NEGAT,IT_TLINE, IT_TLINE[].
ENDFORM.                    " PROCESS_1004
*&---------------------------------------------------------------------*
*&      Form  CREAT_MVT_201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*----------------------------------------------------------------------*
FORM CREAT_MVT_201 TABLES P_IT_TLINE STRUCTURE TLINE
                   USING P_EQUNR.


  DATA : IT_HEADER LIKE BAPI2017_GM_HEAD_01,
           CODE LIKE BAPI2017_GM_CODE VALUE '03',
           V_UOM LIKE MARA-MEINS.

  DATA: IT_ITEM LIKE BAPI2017_GM_ITEM_CREATE OCCURS 0 WITH HEADER LINE,
        IT_RET LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.


  SELECT SINGLE MEINS FROM MARA INTO V_UOM WHERE MATNR = MATNR.

  IT_HEADER-PSTNG_DATE  = SY-DATUM.
  IT_HEADER-DOC_DATE	   = SY-DATUM.
  IF W_CALL = 'SHD' OR W_CALL = 'STPT'.
    IT_HEADER-HEADER_TXT = C3.
  ELSE.
    IT_HEADER-HEADER_TXT = P_EQUNR.
  ENDIF.

  IT_ITEM-MATERIAL      = MATNR.
  IT_ITEM-PLANT         = MARC-WERKS.
  IT_ITEM-STGE_LOC      = T001L-LGORT.
  IT_ITEM-MOVE_TYPE     = BWART.
  IT_ITEM-ENTRY_QNT     = RKMNG.
  IT_ITEM-ENTRY_UOM     = V_UOM.
  IT_ITEM-MOVE_PLANT    = MARC-WERKS.
  IT_ITEM-MOVE_STLOC    = RMMG1-LGORT.

** Changed by Furong on 05/19/10
  CASE W_CALL.
    WHEN 'VPC'.
      IT_ITEM-GL_ACCOUNT = '0000531350'.
    WHEN 'SHD'.
      IT_ITEM-GL_ACCOUNT = '0000123205'.
      IT_ITEM-MOVE_REAS = RM07M-GRUND.
    WHEN 'STPT'.
      IT_ITEM-GL_ACCOUNT = '0000608310'.
      IT_ITEM-MOVE_REAS = RM07M-GRUND.
    WHEN OTHERS.
      IT_ITEM-GL_ACCOUNT = '0000603020'.
  ENDCASE.

*  IF W_CALL = 'VPC'.
*    IT_ITEM-GL_ACCOUNT = '0000531350'.
*  ELSE.
*    IT_ITEM-GL_ACCOUNT = '0000603020'.
*  ENDIF.
** End of change

  IF  W_CALL = 'SHD' OR  W_CALL = 'STPT'.
    CASE IT_ITEM-MOVE_REAS.
      WHEN '9006'.
        IT_ITEM-COSTCENTER = '0000055001'.
      WHEN '9007'.
        IT_ITEM-COSTCENTER = '0000055002'.
      WHEN '9008'.
        IT_ITEM-COSTCENTER = '0000055003'.
      WHEN '9009'.
        IT_ITEM-COSTCENTER = '0000055004'.
      WHEN '9010'.
        IT_ITEM-COSTCENTER = '0000055005'.
      WHEN '9011'.
        IT_ITEM-COSTCENTER = '0000055015'.
      WHEN '9012'.
        IT_ITEM-COSTCENTER = '0000055101'.
      WHEN '9013'.
        IT_ITEM-COSTCENTER = '0000055102'.
      WHEN '9014'.
        IT_ITEM-COSTCENTER = '0000055103'.
      WHEN OTHERS.
        IT_ITEM-COSTCENTER = '0000033003'.
    ENDCASE.
  ELSE.
    IT_ITEM-COSTCENTER = '0000033003'.
  ENDIF.
  IT_ITEM-MVT_IND       = ' '.
  APPEND IT_ITEM.
  CLEAR IT_ITEM.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
   EXPORTING
   GOODSMVT_HEADER             =  IT_HEADER
   GOODSMVT_CODE               =  CODE
*   TESTRUN                     = ' '
IMPORTING
*   GOODSMVT_HEADRET            =
   MATERIALDOCUMENT            = MDOC
   MATDOCUMENTYEAR             = MYEAR
  TABLES
  GOODSMVT_ITEM               =  IT_ITEM
*   GOODSMVT_SERIALNUMBER       =
  RETURN                      =  IT_RET
      .


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             WAIT          = 'X'
*           IMPORTING
*             RETURN        =
            .

  IF MDOC IS INITIAL.
    P_IT_TLINE-TDFORMAT = '*' .
    CONCATENATE 'ERROR: ' IT_RET-MESSAGE INTO P_IT_TLINE-TDLINE.
    APPEND P_IT_TLINE.
    CLEAR P_IT_TLINE.
    EFLAG = 'X'.
  ELSE.
    P_IT_TLINE-TDFORMAT = '*' .
    CONCATENATE '@1 Material DOC:' MDOC MYEAR INTO P_IT_TLINE-TDLINE
           SEPARATED BY SPACE.
*    CONCATENATE '@1 Material DOC:' MDOC INTO P_IT_TLINE-TDLINE.
    APPEND P_IT_TLINE.
    CLEAR P_IT_TLINE.
  ENDIF.

  CLEAR V_UOM.

ENDFORM.                    " CREAT_MVT_201
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CAUSE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_CAUSE_TEXT.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                QMNUM.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BUCH'.
*perform bdc_field       using 'VIQMEL-QMGRP'
*                              record-QMGRP_002.
*perform bdc_field       using 'VIQMEL-QMCOD'
*                              record-QMCOD_003.
*perform bdc_field       using 'RQM00-MATNR'
*                              record-MATNR_004.
*perform bdc_field       using 'RQM00-MAWERK'
*                              record-MAWERK_005.
*perform bdc_field       using 'VIQMEL-RKMNG'
*                              record-RKMNG_006.
*perform bdc_field       using 'VIQMEL-BZMNG'
*                              record-BZMNG_007.
*perform bdc_field       using 'BDC_CURSOR'
*                              'VIQMUR-URTXT'.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              record-OTGRP_008.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              record-OTEIL_009.
*perform bdc_field       using 'VIQMFE-FEGRP'
*                              record-FEGRP_010.
*perform bdc_field       using 'VIQMFE-FECOD'
*                              record-FECOD_011.
*perform bdc_field       using 'VIQMUR-URCOD'
*                              record-URCOD_012.
*perform bdc_field       using 'VIQMUR-URGRP'
*                              record-URGRP_013.
  PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'
                                CUASETXT.
  PERFORM BDC_TRANSACTION1 USING 'QM02'.

ENDFORM.                    " UPDATE_CAUSE_TEXT
*&---------------------------------------------------------------------*
*&      Form  SET_REORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_REORDER USING P_QMNUM.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                P_QMNUM.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=VWML'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '8040'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMNAM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=WEIT'.
  PERFORM BDC_FIELD       USING 'VIQMEL-QMNAM'
                                'REORDER'.
*perform bdc_field       using 'VIQMEL-QMDAT'
*                              record-QMDAT_011.
*perform bdc_field       using 'VIQMEL-MZEIT'
*                              record-MZEIT_012.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.
*  PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'
*                                  CUASETXT.

  PERFORM BDC_TRANSACTION1 USING 'QM02'.

ENDFORM.                    " SET_REORDER
*&---------------------------------------------------------------------*
*&      Form  PROCESS_1005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_1005.
  CLEAR EFLAG.

  DATA: V_LGTYP LIKE PKHD-LGTYP,
        V_LGPLA LIKE PKHD-LGPLA,
        V_VSPVB LIKE MARC-VSPVB,
        V_LGBER LIKE LAGP-LGBER,
        V_VERME LIKE LQUA-VERME,
        V_NEGAT TYPE T331-NEGAT,
        V_QMNUM LIKE QMEL-QMNUM,
        V_LTKZE LIKE MLGN-LTKZE.

  DATA: V_SUB(20) TYPE C,
  L_ATWRT LIKE AUSP-ATWRT,
  L_MDOC_MATL LIKE BAPI2017_GM_HEAD_RET-MAT_DOC.

  DATA: IT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.

  CLEAR: IT_TLINE, IT_TLINE[].
  IF RKMNG IS INITIAL.
    MESSAGE E026 WITH 'Quantity is required'.
  ENDIF.
  IF QMGRP IS INITIAL.
    MESSAGE E026 WITH 'Work Center is required'.
  ENDIF.
  IF C3 IS INITIAL.
    MESSAGE E026 WITH 'Comment field is required'.
  ENDIF.
  IF RM07M-GRUND IS INITIAL.
    MESSAGE E026 WITH 'Cost Center is required'.
  ENDIF.

  IF NOT ITOBATTR-EQUNR IS INITIAL.
    L_EQUNR = ITOBATTR-EQUNR.
    IF L_EQUNR+3(1) = ' '.
      CONCATENATE L_EQUNR+0(3) L_EQUNR+4(14) INTO L_EQUNR.
      ITOBATTR-EQUNR = L_EQUNR.
    ENDIF.
    SELECT SINGLE EQUNR INTO L_EQUNR
    FROM EQUI
      WHERE EQUNR = L_EQUNR.
    IF SY-SUBRC = 0.
    ELSE.
      MESSAGE E026 WITH 'Invalid body number entered'.
    ENDIF.
  ENDIF.

** Added on 09/15/09 by furong; Check RP>=19
  IF W_CALL = 'VPC'.
    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP AS A
      INNER JOIN CABN AS B
      ON A~ATINN = B~ATINN
      WHERE A~OBJEK = ITOBATTR-EQUNR
        AND KLART = '002'
        AND ATNAM = 'P_RP_STATUS'.

    IF L_ATWRT < 19.
      MESSAGE E026 WITH 'RP status is invalid for scrapping'.
    ENDIF.
  ENDIF.

  MESSAGE I026 WITH 'Material will be consumed  from HMMA inventory'.

  IF T001L-LGORT = 'P400'.

    PERFORM CREAT_MVT_201 TABLES IT_TLINE   " creation of movement
                          USING ITOBATTR-EQUNR.

    SELECT SINGLE VSPVB INTO V_VSPVB FROM MARC
                                     WHERE MATNR = MATNR
                                       AND WERKS = MARC-WERKS.

    SELECT SINGLE LGTYP LGPLA  INTO (V_LGTYP, V_LGPLA) FROM PKHD
                               WHERE MATNR = MATNR
                                 AND WERKS = MARC-WERKS
                                 AND PRVBE = V_VSPVB.

    IF ( V_LGTYP IS INITIAL AND V_LGPLA IS INITIAL ).

*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO V_LGPLA.
      MOVE '999' TO V_LGTYP.
    ELSE.
      IF RMMG1-LGNUM IS INITIAL.
        RMMG1-LGNUM = 'P01'.
      ENDIF.
      SELECT SINGLE LGBER INTO V_LGBER FROM LAGP
                                       WHERE LGNUM = RMMG1-LGNUM
                                         AND LGTYP = V_LGTYP
                                         AND LGPLA = V_LGPLA.

      SELECT SINGLE VERME INTO V_VERME FROM LQUA
                            WHERE LGNUM = RMMG1-LGNUM
                              AND MATNR = MATNR
                              AND LGTYP = V_LGTYP
                              AND LGPLA = V_LGPLA.
      IF V_VERME < RKMNG.
        SELECT SINGLE NEGAT FROM T331 INTO V_NEGAT
                    WHERE LGNUM = RMMG1-LGNUM AND LGTYP = V_LGTYP.
        IF V_NEGAT = 'X'.
        ELSE.
*          MOVE 'INV_COUNT' TO V_LGPLA.
          MOVE 'HMMA ADJ' TO V_LGPLA.
          MOVE '999' TO V_LGTYP.
        ENDIF.
      ENDIF.
    ENDIF.

    L_MDOC_MATL = MDOC.

** Furong on 07/19/11
*    IF EFLAG IS INITIAL.
*
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                            USING V_VSPVB V_LGTYP V_LGPLA V_LGBER.
*    ENDIF.
** End on 07/19/11

*  ELSEIF T001L-LGORT = 'P500'.
  ELSE.
    PERFORM CREAT_MVT_201 TABLES IT_TLINE     " creation of movement
                                 USING ITOBATTR-EQUNR.
    L_MDOC_MATL = MDOC.
  ENDIF.

  CLEAR EFLAG.

  READ TABLE IT_TLINE INDEX 1.
  MESSAGE S026 WITH IT_TLINE-TDLINE.


********  Printing Label ********
  MDOC = L_MDOC_MATL.

  IF MDOC IS INITIAL.
  ELSE.
    PERFORM PRINT_LABEL_1.
*    REFRESH: IT_QMNUM.
*    CLEAR: IT_QMNUM.
*    IT_QMNUM-QMNUM = MDOC.
*    IT_QMNUM-MATNR = MATNR.
*    IT_QMNUM-MAKTX = QMCOD.
*    IT_QMNUM-RKMNG = RKMNG.
*    IT_QMNUM-ZERDAT = SY-DATUM.
*    IT_QMNUM-ZREASON = RM07M-GRUND.
*    IT_QMNUM-OTGRP = W_CALL.
*    IT_QMNUM-TXTCDUR = QMGRP.  " QPCT-KURZTEXT.
**    IT_QMNUM-ZROOTWC LIKE QPCT-KURZTEXT.
*    APPEND IT_QMNUM.
**    PERFORM PRINT_LABEL.
*    PERFORM PRINT_LABEL_OTHERS.
  ENDIF.
*****   Send mail logic  ********

*  IF T001L-LGORT = 'P500'.
*    V_SUB = 'Supplier Handling Damage at HMMA'.
*    PERFORM SEND_MAIL_1004 USING V_SUB.
*  ENDIF.

  CLEAR: V_LGTYP, V_LGPLA, V_VSPVB, V_LGBER, V_SUB, V_VERME,
         V_QMNUM,V_LTKZE,V_QMNUM,V_NEGAT,IT_TLINE, IT_TLINE[].

ENDFORM.                    " PROCESS_1005
*&---------------------------------------------------------------------*
*&      Form  PRINT_LABEL_other
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LABEL_OTHER.

  SET PARAMETER ID 'MBN' FIELD MDOC.   "IT_QMNUM-QMNUM.
  SET PARAMETER ID 'MAT' FIELD IT_QMNUM-MATNR.
*it_qmnum-QMNUM = QMNUM.
*append it_qmnum.
  REFRESH IT_QMNUM.
  IT_QMNUM-MATNR = MATNR.
  IT_QMNUM-QMNUM = MDOC.
  IT_QMNUM-RKMNG = RKMNG.

  SELECT SINGLE MEINS INTO IT_QMNUM-ZMEINS
  FROM MARA WHERE MATNR = MATNR.


*   it_QMNUM-MAKTX
*  IT_QMNUM-MATKL
*IT_QMNUM-PDFBC
*IT_QMNUM-TXTCDUR
*IT_QMNUM-ZERDAT
*
*iT_QMNUM-ZREASON
*IT_QMNUM-ZROOTWC

  EXPORT IT_QMNUM TO MEMORY ID 'M1'.
  SUBMIT ZQMSCRAP_LABEL AND RETURN.

ENDFORM.                    " PRINT_LABEL_other
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_BOX.
  DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      L_CN LIKE XVALUE-KEY.

  DATA: LT_TEMP LIKE TABLE OF T157E WITH HEADER LINE.

  CLEAR : XLIST[] , XVALUE.

  SELECT GRUND INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
    FROM T157E
    WHERE SPRAS ='EN'
      AND BWART ='201'
      AND GRUND > '9000'.

  L_CN = '1'.
  LOOP AT LT_TEMP.
    XVALUE-TEXT = LT_TEMP-GRUND.
    XVALUE-KEY  = L_CN.
    APPEND XVALUE TO XLIST.
    L_CN = L_CN + 1.
  ENDLOOP.

  XVALUE-TEXT = 'ALL'.
  XVALUE-KEY  = '4'.
  APPEND XVALUE TO XLIST .


  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = 'RM07M-GRUND'
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.


  READ TABLE XLIST INTO XVALUE  INDEX 1.
  RM07M-GRUND = XVALUE-TEXT.

ENDFORM.                    " LIST_BOX
*&---------------------------------------------------------------------*
*&      Form  PRINT_LABEL_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LABEL_OTHERS.
* SET PARAMETER ID 'IQM' FIELD IT_QMNUM-QMNUM.
  SET PARAMETER ID 'MAT' FIELD IT_QMNUM-MATNR.
*it_qmnum-QMNUM = QMNUM.
*append it_qmnum.
*  SORT IT_QMNUM BY QMNUM.
  DELETE ADJACENT DUPLICATES FROM IT_QMNUM COMPARING QMNUM.
  EXPORT IT_QMNUM TO MEMORY ID 'M2'.
  SUBMIT ZQMSCRAP_LABEL_OTHERS AND RETURN.
*****end code to pass variables

ENDFORM.                    " PRINT_LABEL_OTHERS
*&---------------------------------------------------------------------*
*&      Form  print_label_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LABEL_1.
  REFRESH: IT_QMNUM.
  CLEAR: IT_QMNUM.
  IT_QMNUM-QMNUM = MDOC.
  IT_QMNUM-MATNR = MATNR.
  IT_QMNUM-MAKTX = QMCOD.
  IT_QMNUM-RKMNG = RKMNG.
  IT_QMNUM-ZERDAT = SY-DATUM.
  IT_QMNUM-ZREASON = RM07M-GRUND.
  IT_QMNUM-OTGRP = W_CALL.
  IT_QMNUM-TXTCDUR = QMGRP.  " QPCT-KURZTEXT.
*    IT_QMNUM-ZROOTWC LIKE QPCT-KURZTEXT.
  APPEND IT_QMNUM.
*    PERFORM PRINT_LABEL.
  PERFORM PRINT_LABEL_OTHERS.
ENDFORM.                    " print_label_1
*&---------------------------------------------------------------------*
*&      Form  clear_valuse_1005_1006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VALUSE_1005_1006.
  CLEAR: QMGRP, QMCOD, RKMNG,RM07M-GRUND.
ENDFORM.                    " clear_valuse_1005_1006
*&---------------------------------------------------------------------*
*&      Form  FIND_MDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_MDOC.
  DATA: IT_REPTAG LIKE TABLE OF ZSCRAP_REPRINTO WITH HEADER LINE.
*  DATA:  L_STAT LIKE JEST-STAT,
*         L_TXT30 LIKE TJ30T-TXT30,
*         L_STSMA LIKE JSTO-STSMA,
  DATA:  L_DATE LIKE SY-DATUM,
         L_GLACC LIKE MSEG-SAKTO.

  DATA : DYNAME LIKE D020S-PROG , DYNUMB LIKE D020S-DNUM .
  DATA:  RET_TAB LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE,
         IT_DSEL LIKE DSELC OCCURS 0 WITH HEADER LINE.

  RANGES: R_GRUND FOR MSEG-GRUND.

  DYNAME = 'SAPMZSCRAP'.
  DYNUMB = '1802'.
  IF W_CALL = 'SHD'.
    L_GLACC = '123205'.
  ELSE.
    L_GLACC = '608310'.
  ENDIF.

  R_GRUND-OPTION = 'BT'.
  R_GRUND-SIGN = 'I'.
  R_GRUND-LOW = '9006'.
  R_GRUND-HIGH = '9014'.
  APPEND  R_GRUND.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = MBLNR
       IMPORTING
            OUTPUT = MBLNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_GLACC
       IMPORTING
            OUTPUT = L_GLACC.

  IF MBLNR IS INITIAL.
    IF P_MATNR IS INITIAL.
      IF P_NUSER IS INITIAL.
        SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
         INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
           FROM MSEG AS A
           INNER JOIN MKPF AS B
           ON A~MBLNR = B~MBLNR
          WHERE BWART = '201'
            AND GRUND IN R_GRUND
            AND SAKTO = L_GLACC.
      ELSE.
        SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
        INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
          FROM MSEG AS A
          INNER JOIN MKPF AS B
          ON A~MBLNR = B~MBLNR
           WHERE USNAM = P_NUSER
               AND BWART = '201'
            AND GRUND IN R_GRUND
            AND SAKTO = L_GLACC.
      ENDIF.
      IF NOT MSEG-ZBUDAT IS INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*             EXPORTING
*                  INPUT  = QMEL-QMDAT
*             IMPORTING
*                  OUTPUT = L_DATE.
        DELETE IT_REPTAG WHERE ZBUDAT <> MSEG-ZBUDAT.
      ENDIF.
    ELSE.
      DO.
        REPLACE '*' WITH '%' INTO P_MATNR.
        IF SY-SUBRC <> 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF  ( MSEG-ZBUDAT = '00000000' OR MSEG-ZBUDAT = ' ' )
          AND P_NUSER IS INITIAL.
        SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
          INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
            FROM MSEG AS A
            INNER JOIN MKPF AS B
            ON A~MBLNR = B~MBLNR
            WHERE BWART = '201'
           AND GRUND IN R_GRUND
           AND SAKTO = L_GLACC
           AND MATNR LIKE P_MATNR.
      ELSE.
        IF  MSEG-ZBUDAT = '00000000' OR MSEG-ZBUDAT = ' '.
          DO.
            REPLACE '*' WITH '%' INTO P_NUSER.
            IF SY-SUBRC <> 0.
              EXIT.
            ENDIF.
          ENDDO.

          SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
          INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
            FROM MSEG AS A
            INNER JOIN MKPF AS B
            ON A~MBLNR = B~MBLNR
             WHERE USNAM = P_NUSER
                 AND BWART = '201'
              AND GRUND IN R_GRUND
              AND SAKTO = L_GLACC
              AND MATNR LIKE P_MATNR.

        ELSE.
          IF P_NUSER IS INITIAL.
            SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
            INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
            FROM MSEG AS A
            INNER JOIN MKPF AS B
            ON A~MBLNR = B~MBLNR
                WHERE BWART = '201'
               AND GRUND IN R_GRUND
               AND SAKTO = L_GLACC
                AND  MATNR LIKE P_MATNR
                  AND ZBUDAT = MSEG-ZBUDAT.
          ELSE.
            DO.
              REPLACE '*' WITH '%' INTO P_NUSER.
              IF SY-SUBRC <> 0.
                EXIT.
              ENDIF.
            ENDDO.
*            CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                 EXPORTING
*                      INPUT  = QMEL-QMDAT
*                 IMPORTING
*                      OUTPUT = L_DATE.
            SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
              INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
              FROM MSEG AS A
              INNER JOIN MKPF AS B
              ON A~MBLNR = B~MBLNR
              WHERE USNAM = P_NUSER
                AND BWART = '201'
                AND GRUND IN R_GRUND
                AND SAKTO = L_GLACC
                AND MATNR LIKE P_MATNR
                AND ZBUDAT = MSEG-ZBUDAT.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    DO.
      REPLACE '*' WITH '%' INTO P_QMNUM.
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.
    ENDDO.
    SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
       INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
         FROM MSEG AS A
         INNER JOIN MKPF AS B
         ON A~MBLNR = B~MBLNR
       WHERE BWART = '201'
         AND GRUND IN R_GRUND
         AND SAKTO = L_GLACC
         AND A~MBLNR LIKE MBLNR.
  ENDIF.

*  LOOP AT IT_REPTAG.
*    SELECT SINGLE STAT FROM JEST INTO L_STAT
*                       WHERE OBJNR = IT_REPTAG-OBJNR
*                         AND INACT = ' '.
*    SELECT SINGLE STSMA FROM JSTO INTO L_STSMA
*                       WHERE OBJNR = JEST-OBJNR.
*    SELECT SINGLE TXT30 FROM TJ30T INTO L_TXT30
*                       WHERE STSMA = L_STSMA
*                         AND ESTAT = L_STAT.
*    IT_REPTAG-TXT30 = L_TXT30.
*    MODIFY IT_REPTAG.
*    CLEAR: IT_REPTAG, L_STAT, L_TXT30, L_STSMA.
*  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM IT_REPTAG.

  IT_DSEL-FLDNAME = 'MBLNR'.
  IT_DSEL-DYFLDNAME = 'MBLNR'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MATNR'.
  IT_DSEL-DYFLDNAME = 'P_MATNR'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'GRUND'.
  IT_DSEL-DYFLDNAME = 'MSEG-GRUND'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'ZBUDAT'.
  IT_DSEL-DYFLDNAME = 'MSEG-ZBUDAT'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'USNAM'.
  IT_DSEL-DYFLDNAME = 'P_NUSER'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MENGE'.
  IT_DSEL-DYFLDNAME = 'MSEG-MENGE'.
  APPEND IT_DSEL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    DDIC_STRUCTURE         = 'ZSCRAP_REPRINTO'
       RETFIELD               = 'MBLNR'
*   PVALKEY                = ' '
     DYNPPROG               = DYNAME
     DYNPNR                 = DYNUMB
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     VALUE_TAB              = IT_REPTAG
*    field_tab             = fld_tab
     RETURN_TAB             = RET_TAB
     DYNPFLD_MAPPING        = IT_DSEL
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT RET_TAB.
    CASE RET_TAB-FIELDNAME.
      WHEN 'MBLNR'.
        MBLNR = RET_TAB-FIELDVAL.
      WHEN 'MATNR'.
        P_MATNR = RET_TAB-FIELDVAL.
      WHEN 'USNAM'.
        P_NUSER = RET_TAB-FIELDVAL.
      WHEN 'ZBUDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
             EXPORTING
                  INPUT  = RET_TAB-FIELDVAL
             IMPORTING
                  OUTPUT = MSEG-ZBUDAT.
      WHEN 'MENGE'.
        MSEG-MENGE = RET_TAB-FIELDVAL.
      WHEN 'GRUND'.
        MSEG-GRUND = RET_TAB-FIELDVAL.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FIND_MDOC
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MDOC.
  DATA: IT_REPTAG LIKE TABLE OF ZSCRAP_REPRINTO WITH HEADER LINE.
*  DATA:  L_STAT LIKE JEST-STAT,
*         L_TXT30 LIKE TJ30T-TXT30,
*         L_STSMA LIKE JSTO-STSMA,
  DATA:  L_DATE LIKE SY-DATUM,
         L_GLACC LIKE MSEG-SAKTO.

  DATA : DYNAME LIKE D020S-PROG , DYNUMB LIKE D020S-DNUM .
  DATA:  RET_TAB LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE,
         IT_DSEL LIKE DSELC OCCURS 0 WITH HEADER LINE.

  RANGES: R_GRUND FOR MSEG-GRUND.

  DYNAME = 'SAPMZSCRAP'.
  DYNUMB = '1802'.
  IF W_CALL = 'SHD'.
    L_GLACC = '123205'.
  ELSE.
    L_GLACC = '608310'.
  ENDIF.

  R_GRUND-OPTION = 'BT'.
  R_GRUND-SIGN = 'I'.
  R_GRUND-LOW = '9006'.
  R_GRUND-HIGH = '9014'.
  APPEND  R_GRUND.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = MBLNR
       IMPORTING
            OUTPUT = MBLNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = L_GLACC
       IMPORTING
            OUTPUT = L_GLACC.

  IF MBLNR IS INITIAL.
    IF P_MATNR IS INITIAL.
      IF P_NUSER IS INITIAL AND MSEG-ZBUDAT IS INITIAL.
        MESSAGE I026 WITH 'No data selected'.
        EXIT.
      ENDIF.
      SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
        INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
          FROM MSEG AS A
          INNER JOIN MKPF AS B
          ON A~MBLNR = B~MBLNR
        WHERE BWART = '201'
          AND GRUND IN R_GRUND
          AND SAKTO = L_GLACC.
    ELSE.
      SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
       INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
         FROM MSEG AS A
         INNER JOIN MKPF AS B
         ON A~MBLNR = B~MBLNR
     WHERE BWART = '201'
       AND MATNR = P_MATNR
       AND GRUND IN R_GRUND
       AND SAKTO = L_GLACC.
    ENDIF.
    IF NOT P_NUSER IS INITIAL.
      DELETE IT_REPTAG WHERE USNAM <> P_NUSER.
    ENDIF.
    IF NOT MSEG-ZBUDAT IS INITIAL.
      DELETE IT_REPTAG WHERE ZBUDAT <> MSEG-ZBUDAT.
    ENDIF.
  ELSE.
    SELECT A~MBLNR MATNR ZBUDAT MENGE USNAM GRUND
       INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
         FROM MSEG AS A
         INNER JOIN MKPF AS B
         ON A~MBLNR = B~MBLNR
         WHERE B~MBLNR = MBLNR.

    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ENDIF.

*  LOOP AT IT_REPTAG.
*    SELECT SINGLE STAT FROM JEST INTO L_STAT
*                       WHERE OBJNR = IT_REPTAG-OBJNR
*                         AND INACT = ' '.
*    SELECT SINGLE STSMA FROM JSTO INTO L_STSMA
*                       WHERE OBJNR = JEST-OBJNR.
*    SELECT SINGLE TXT30 FROM TJ30T INTO L_TXT30
*                       WHERE STSMA = L_STSMA
*                         AND ESTAT = L_STAT.
*    IT_REPTAG-TXT30 = L_TXT30.
*    MODIFY IT_REPTAG.
*    CLEAR: IT_REPTAG, L_STAT, L_TXT30, L_STSMA.
*  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM IT_REPTAG.

  IT_DSEL-FLDNAME = 'MBLNR'.
  IT_DSEL-DYFLDNAME = 'MBLNR'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MATNR'.
  IT_DSEL-DYFLDNAME = 'P_MATNR'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'GRUND'.
  IT_DSEL-DYFLDNAME = 'MSEG-GRUND'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'ZBUDAT'.
  IT_DSEL-DYFLDNAME = 'MSEG-ZBUDAT'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'USNAM'.
  IT_DSEL-DYFLDNAME = 'P_NUSER'.
  APPEND IT_DSEL.

  IT_DSEL-FLDNAME = 'MENGE'.
  IT_DSEL-DYFLDNAME = 'MSEG-MENGE'.
  APPEND IT_DSEL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    DDIC_STRUCTURE         = 'ZSCRAP_REPRINTO'
       RETFIELD               = 'MBLNR'
*   PVALKEY                = ' '
     DYNPPROG               = DYNAME
     DYNPNR                 = DYNUMB
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     VALUE_TAB              = IT_REPTAG
*    field_tab             = fld_tab
     RETURN_TAB             = RET_TAB
     DYNPFLD_MAPPING        = IT_DSEL
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT RET_TAB.
    CASE RET_TAB-FIELDNAME.
      WHEN 'MBLNR'.
        MBLNR = RET_TAB-FIELDVAL.
      WHEN 'MATNR'.
        P_MATNR = RET_TAB-FIELDVAL.
      WHEN 'USNAM'.
        P_NUSER = RET_TAB-FIELDVAL.
      WHEN 'ZBUDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
             EXPORTING
                  INPUT  = RET_TAB-FIELDVAL
             IMPORTING
                  OUTPUT = MSEG-ZBUDAT.
      WHEN 'MENGE'.
        MSEG-MENGE = RET_TAB-FIELDVAL.
      WHEN 'GRUND'.
        MSEG-GRUND = RET_TAB-FIELDVAL.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " DISPLAY_MDOC
*&---------------------------------------------------------------------*
*&      Form  CLEAR_1802
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_1802.
  CLEAR: MBLNR, P_MATNR, P_NUSER, QMGRP, QMCOD,
        MSEG-ZBUDAT, MSEG-GRUND, MSEG-MENGE.

ENDFORM.                    " CLEAR_1802
*&---------------------------------------------------------------------*
*&      Form  REPRINT_LABEL_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REPRINT_LABEL_OTHERS.
  DATA: L_MESS(255).
  IF QMGRP IS INITIAL.
    MESSAGE E026 WITH 'Please input work center'.
  ENDIF.
  IF MBLNR IS INITIAL AND
     P_MATNR IS INITIAL.
    EXIT.
  ELSEIF MBLNR IS INITIAL OR P_MATNR IS INITIAL.
    PERFORM DISPLAY_MDOC.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD P_MATNR.

  REFRESH: IT_QMNUM.
  CLEAR: IT_QMNUM.
  IT_QMNUM-QMNUM = MBLNR.
  IT_QMNUM-MATNR = P_MATNR.
  IT_QMNUM-MAKTX = QMCOD.
  IT_QMNUM-RKMNG = MSEG-MENGE.
  IT_QMNUM-ZERDAT = SY-DATUM.
  IT_QMNUM-ZREASON = MSEG-GRUND.
  IT_QMNUM-OTGRP = W_CALL.
  IT_QMNUM-TXTCDUR = QMGRP.  " QPCT-KURZTEXT.

  APPEND IT_QMNUM.


  SET PARAMETER ID 'MAT' FIELD IT_QMNUM-MATNR.

  DELETE ADJACENT DUPLICATES FROM IT_QMNUM COMPARING QMNUM.
  EXPORT IT_QMNUM TO MEMORY ID 'M2'.

  DO P_COPY TIMES.
    SUBMIT ZQMSCRAP_LABEL_OTHERS AND RETURN.
    IMPORT L_MESS FROM MEMORY ID 'ME'.
    IF NOT L_MESS IS INITIAL.
      MESSAGE I026 WITH L_MESS.
    ELSE.
      MESSAGE S026 WITH 'Reprint label complete'.
    ENDIF.
    FREE MEMORY ID 'ME'.
  ENDDO.

*  CLEAR: IT_QMNUM, IT_QMNUM[].
*  IT_QMNUM-QMNUM = P_QMNUM.
*  IT_QMNUM-MATNR = P_MATNR.
*  APPEND IT_QMNUM.
*  EXPORT IT_QMNUM TO MEMORY ID 'M1'.
*  DO P_COPY TIMES.
*    SUBMIT ZQMSCRAP_LABEL AND RETURN.
*    IMPORT L_MESS FROM MEMORY ID 'ME'.
*    IF NOT L_MESS IS INITIAL.
*      MESSAGE I026 WITH L_MESS.
*    ELSE.
*      MESSAGE S026 WITH 'Reprint label complete'.
*    ENDIF.
*    FREE MEMORY ID 'ME'.
*  ENDDO.

ENDFORM.                    " REPRINT_LABEL_others
*&---------------------------------------------------------------------*
*&      Form  CALL_1802
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_1202.
  DATA: L_TXT04 LIKE TJ02T-TXT04,
        L_QMART LIKE QMEL-QMART,
         L_STAT LIKE TJ30T-ESTAT.

  SELECT SINGLE LIFNUM QMART STAT INTO (LIFNR, L_QMART, L_STAT)
         FROM QMEL AS A
         INNER JOIN JEST AS D
         ON A~OBJNR = D~OBJNR
         WHERE A~QMNUM = QMNUM
           AND INACT = ' '.


*  SELECT SINGLE LIFNUM TXT04 QMART INTO (LIFNR, L_TXT04, L_QMART)
*         FROM QMEL AS A
*         INNER JOIN JEST AS D
*         ON A~OBJNR = D~OBJNR
*         INNER JOIN TJ30T AS E
*         ON D~STAT = E~ESTAT
*         WHERE A~QMNUM = QMNUM
*           AND E~STSMA = 'ZQNSCRP1'
*           AND INACT = ' '.

  IF L_QMART = 'Q4'.
    IF LIFNR IS INITIAL.
      MESSAGE I026 WITH 'Venodr not available'.
      EXIT.
    ENDIF.
  ELSE.
    SELECT SINGLE TXT04 INTO L_TXT04
        FROM TJ30T
         WHERE STSMA = 'ZQNSCRP1'
             AND ESTAT = L_STAT.

    IF L_TXT04 <> 'VEND'.
      MESSAGE I026 WITH 'User status must be VEND'.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE TXT04 INTO L_TXT04
        FROM QMEL AS A
        INNER JOIN JEST AS D
        ON A~OBJNR = D~OBJNR
        INNER JOIN TJ02T AS E
        ON D~STAT = E~ISTAT
        WHERE A~QMNUM = QMNUM
           AND SPRAS = 'EN'
          AND INACT = ' '
          AND TXT04 = 'DLFL'.

  IF SY-SUBRC = 0.
    MESSAGE I026 WITH 'System status CANNOT be DLFL'.
    EXIT.
  ENDIF.

  LEAVE TO SCREEN '1202'.
ENDFORM.                                                    " CALL_1802
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PRIMARY_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_PRIMARY_VALUE.
  SELECT SINGLE *  FROM QMEL
   WHERE QMNUM = QMNUM.
  IF SY-SUBRC  = 0.
    LFA1-LIFNR = QMEL-LIFNUM.
    QMGRP = QMEL-QMGRP.
    QMCOD = QMEL-QMCOD.
    ITOBATTR-EQUNR = QMEL-KDMAT.

    SELECT SINGLE OTGRP OTEIL INTO (OTGRP, OTEIL)
     FROM QMFE
     WHERE QMNUM = QMNUM.

    FEGRP = '0'.
    FECOD = '0015'.
    URGRP = 'CAUS'.
    URCOD = '04'.
  ENDIF.
ENDFORM.                    " UPDATE_PRIMARY_VALUE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_1203.
  CLEAR EFLAG.

  DATA: V_LGTYP LIKE PKHD-LGTYP,
        V_LGPLA LIKE PKHD-LGPLA,
        V_VSPVB LIKE MARC-VSPVB,
        V_LGBER LIKE LAGP-LGBER,
        V_VERME LIKE LQUA-VERME,
        V_NEGAT TYPE T331-NEGAT,
        V_QMNUM LIKE QMEL-QMNUM,
        V_LTKZE LIKE MLGN-LTKZE.

  DATA: V_SUB(20) TYPE C.

  DATA: IT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.

  CLEAR: IT_TLINE, IT_TLINE[].

  PERFORM CODE_CHECK.

  IT_TLINE-TDFORMAT = '*' .
  CONCATENATE 'Plant' MARC-WERKS INTO IT_TLINE-TDLINE.
  APPEND IT_TLINE.
  CLEAR IT_TLINE.

  IT_TLINE-TDFORMAT = '*' .
  CONCATENATE 'Storage Location' T001L-LGORT INTO IT_TLINE-TDLINE.
  APPEND IT_TLINE.
  CLEAR IT_TLINE.

  IF T001L-LGORT = 'P400'.

    PERFORM CREAT_MVT TABLES IT_TLINE.   " creation of movement


    SELECT SINGLE VSPVB INTO V_VSPVB FROM MARC
                                     WHERE MATNR = MATNR
                                       AND WERKS = MARC-WERKS.

    SELECT SINGLE LGTYP LGPLA  INTO (V_LGTYP, V_LGPLA) FROM PKHD
                               WHERE MATNR = MATNR
                                 AND WERKS = MARC-WERKS
                                 AND PRVBE = V_VSPVB.

    IF ( V_LGTYP IS INITIAL AND V_LGPLA IS INITIAL ).
** Changed by Furong on 06/21/10
*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO V_LGPLA.
** End of change
      MOVE '999' TO V_LGTYP.

    ELSE.

      SELECT SINGLE LGBER INTO V_LGBER FROM LAGP
                                       WHERE LGNUM = RMMG1-LGNUM
                                         AND LGTYP = V_LGTYP
                                         AND LGPLA = V_LGPLA.


      SELECT SINGLE VERME INTO V_VERME FROM LQUA
                            WHERE LGNUM = RMMG1-LGNUM
                              AND MATNR = MATNR
                              AND LGTYP = V_LGTYP
                              AND LGPLA = V_LGPLA.

      IF V_VERME < RKMNG.

        SELECT SINGLE NEGAT FROM T331 INTO V_NEGAT
                    WHERE LGNUM = RMMG1-LGNUM AND LGTYP = V_LGTYP.

        IF V_NEGAT = 'X'.

        ELSE.

*          MOVE 'INV_COUNT' TO V_LGPLA.
          MOVE 'HMMA ADJ' TO V_LGPLA.
          MOVE '999' TO V_LGTYP.
        ENDIF.

      ENDIF.

    ENDIF.

*S__BY PAUL
*    IF EFLAG IS INITIAL.
*
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                        USING V_VSPVB V_LGTYP V_LGPLA V_LGBER.
*
*    ENDIF.

** Changed by Furong on 10/19/09
*  ELSEIF T001L-LGORT = 'P500'.
  ELSE.
** End of change on 10/19/09

    PERFORM CREAT_MVT TABLES IT_TLINE.     " creation of movement

  ENDIF.

  CLEAR EFLAG.

  PERFORM CREAT_NOTIF_1203.                " creation of notification


  READ TABLE IT_QMNUM INDEX 1.
  V_QMNUM = IT_QMNUM-QMNUM.
  PERFORM DELETE_LONGTEXT USING V_QMNUM.

  PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE    " updating QN# long text
                            USING V_QMNUM.
  READ TABLE IT_TLINE INDEX 3.

  IF IT_TLINE-TDLINE+0(5) = 'ERROR'.
    PERFORM UPDATE_NOTIF_STATUS USING V_QMNUM. " updating QN# status
     READ TABLE IT_QMNUM INDEX 1.
      IT_QMNUM-FLAG = 'X'.
      MODIFY IT_QMNUM INDEX 1.

  ELSE.
    READ TABLE IT_TLINE INDEX 4.
    IF IT_TLINE-TDLINE+0(5) = 'ERROR'.
      PERFORM UPDATE_NOTIF_STATUS USING V_QMNUM.  " updating QN# status
      READ TABLE IT_QMNUM INDEX 1.
      IT_QMNUM-FLAG = 'X'.
      MODIFY IT_QMNUM INDEX 1.

    ELSE.
** Changed by Furong on 03/03/09
      IF FEGRP = '4' AND URGRP <> '04'.

        CLEAR: IT_TLINE, IT_TLINE[].
        PERFORM UPDATE_NOPR_HMMA TABLES IT_TLINE
                                 USING V_QMNUM.
        WAIT UP TO 2 SECONDS.
     PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE   "updating QN# long text
                                                          USING V_QMNUM.
      ENDIF.
** End of change
    ENDIF.
  ENDIF.

** Changed by Furong on 09/16/09
  IF W_ANS = '1'.
    PERFORM SET_REORDER USING V_QMNUM.
    CLEAR: W_ANS.
  ENDIF.
** Changed by Furong on 11/04/09
*  QMNUM = V_QMNUM.
*  PERFORM UPDATE_CAUSE_TEXT.
** End of change on 11/04/09
** end of change

********  Printing Label ********

  PERFORM PRINT_LABEL.

  CLEAR: V_LGTYP, V_LGPLA, V_VSPVB, V_LGBER, V_SUB, V_VERME,
         V_QMNUM,V_LTKZE,V_QMNUM,V_NEGAT,IT_TLINE, IT_TLINE[].

*  IF SY-UNAME <> '101457'  or SY-UNAME <> '100794'.
*    CALL 'SYST_LOGOFF'.
*  ELSE.
*    LEAVE PROGRAM.
*  ENDIF.

ENDFORM.                    " PROCESS_1203
*&---------------------------------------------------------------------*
*&      Form  CREAT_NOTIF_1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREAT_NOTIF_1203.
  DATA: L_DATE_C(8).
  REFRESH: BDCDATA.

***************  Initial screen **************
*
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RIWO00-QMART'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM BDC_FIELD       USING 'RIWO00-QMART'
*                                 RIWO00-QMART.
*
*****   Second screen   ***********************
*
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=10\TAB02'.
*  PERFORM BDC_FIELD       USING 'VIQMEL-QMGRP'
*                                 QMGRP.           " Author work centre
*  PERFORM BDC_FIELD       USING 'VIQMEL-QMCOD'
*                                 QMCOD.
*
*  PERFORM BDC_FIELD       USING 'RQM00-MATNR'     " material
*                                 MATNR.
*  PERFORM BDC_FIELD       USING 'RQM00-MAWERK'
*                                 MARC-WERKS.      " Plant
*  PERFORM BDC_FIELD       USING 'VIQMEL-RKMNG'
*                                 RKMNG.           " quantity
*
*
*** Changed by Furong on 02/10/10
*  IF NOT ITOBATTR-EQUNR IS INITIAL.
*    PERFORM BDC_FIELD       USING 'VIQMEL-KDMAT'
*                                   ITOBATTR-EQUNR.    " BODY NO
*  ENDIF.
*** End of change
*  PERFORM BDC_FIELD       USING 'VIQMFE-OTGRP'
*                                 OTGRP.           " root cause
*  PERFORM BDC_FIELD       USING 'VIQMFE-OTEIL'
*                                 OTEIL.
*  PERFORM BDC_FIELD       USING 'VIQMFE-FEGRP'
*                                 FEGRP.           " Defect type
*  PERFORM BDC_FIELD       USING 'VIQMFE-FECOD'
*                                 FECOD.
*  PERFORM BDC_FIELD       USING 'VIQMUR-URCOD'    " cause code
*                                 URCOD.
*** Changed by Furong on 11/04/09
*  PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'    " Cause text
*                                CUASETXT.
*** End of change
*  PERFORM BDC_FIELD       USING 'VIQMUR-URGRP'
*                                 URGRP.
*  IF MARC-XCHAR = 'X'.
*    PERFORM BDC_FIELD       USING 'VIQMEL-CHARG'
*                                   MSEG-CHARG.
*  ENDIF.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'IHPA-PARNR(02)'.
*  PERFORM BDC_FIELD       USING 'IHPA-PARVW(02)'
*                                'Z5'.
*  PERFORM BDC_FIELD       USING 'IHPA-PARNR(02)'
*                                 LFA1-LIFNR.
*** Changed by Furong on 02/10/10
*  IF NOT QMEL-ERDAT IS INITIAL.
*    WRITE: QMEL-ERDAT TO L_DATE_C MMDDYY.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                  'VIQMEL-AUSVN'.
*    PERFORM BDC_FIELD       USING 'VIQMEL-AUSVN'
*                                  L_DATE_C.
*  ENDIF.
*** End of change
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=BUCH'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'IHPA-PARVW(03)'.
*

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIWO00-QWRNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMART'
                                'Q3'.           "record-QMART_001.
  PERFORM BDC_FIELD       USING 'RIWO00-QWRNUM'
                                QMNUM.          "record-QWRNUM_002.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=AWST'.
  PERFORM BDC_FIELD       USING 'VIQMEL-QMGRP'
                                QMGRP.
  PERFORM BDC_FIELD       USING 'VIQMEL-QMCOD'
                                QMCOD.
  PERFORM BDC_FIELD       USING 'RQM00-MATNR'
                                MATNR.
  PERFORM BDC_FIELD       USING 'RQM00-MAWERK'
                                MARC-WERKS.
  IF NOT ITOBATTR-EQUNR IS INITIAL.
    PERFORM BDC_FIELD       USING 'VIQMEL-KDMAT'
                                  ITOBATTR-EQUNR.
  ENDIF.
  PERFORM BDC_FIELD       USING 'VIQMEL-RKMNG'
                                RKMNG.
  PERFORM BDC_FIELD       USING 'VIQMEL-BZMNG'
                                '0'.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              OTGRP.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              OTEIL.
  PERFORM BDC_FIELD       USING 'VIQMFE-FEGRP'
                                FEGRP.
  PERFORM BDC_FIELD       USING 'VIQMFE-FECOD'
                                FECOD.
  PERFORM BDC_FIELD       USING 'VIQMUR-URCOD'
                                URCOD.
  PERFORM BDC_FIELD       USING 'VIQMUR-URGRP'
                                URGRP.
  PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'
                                CUASETXT.

  PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0201'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'J_STMAINT-ANWSO(02)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=OKAY'.
  PERFORM BDC_FIELD       USING 'J_STMAINT-ANWSO(02)'
                                'X'.  " record-ANWSO_02_017.

  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=10\TAB02'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'VIQMEL-PRIOK'.
*perform bdc_field       using 'VIQMEL-STRMN'
*                              record-STRMN_032.
*perform bdc_field       using 'VIQMEL-STRUR'
*                              record-STRUR_033.
*perform bdc_field       using 'VIQMEL-LTRUR'
*                              record-LTRUR_034.
*perform bdc_field       using 'VIQMEL-AUSVN'
*                              record-AUSVN_035.
*perform bdc_field       using 'VIQMEL-AUZTV'
*                              record-AUZTV_036.

  PERFORM BDC_TRANSACTION1 USING 'QM01'.

ENDFORM.                    " CREAT_NOTIF_1203
*&---------------------------------------------------------------------*
*&      Form  DELETE_LONGTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_LONGTEXT USING P_QMNUM.
  DATA: L_QMNUM LIKE THEAD-TDNAME.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_QMNUM
       IMPORTING
            OUTPUT = P_QMNUM.

  L_QMNUM = P_QMNUM.
  CALL FUNCTION 'DELETE_TEXT'
       EXPORTING
            CLIENT          = SY-MANDT
            ID              = 'LTQM'
            LANGUAGE        = SY-LANGU
            NAME            = L_QMNUM
            OBJECT          = 'QMEL'
            SAVEMODE_DIRECT = 'X'
            TEXTMEMORY_ONLY = ' '
            LOCAL_CAT       = ' '
       EXCEPTIONS
            NOT_FOUND       = 1
            OTHERS          = 2.

ENDFORM. " DELETE_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SEARCH_HELP_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_KATALOGART  text
*      -->P_P_FIELDNAME  text
*      -->P_P_FIELDNAME1  text
*----------------------------------------------------------------------*
FORM DISPLAY_SEARCH_HELP_READ USING    P_KATALOGART
                                  P_FIELDNAME
                                  P_FIELDNAME1.

  DATA:
       L_TQ15T                 LIKE TQ15T,
       L_QPK1GR                LIKE QPK1GR,
       L_REPID                 LIKE D020S-PROG,
       L_DYNNR                 LIKE SY-DYNNR,
       L_QMGRP                 LIKE QMGRP.

  DATA: I_KATALOGART TYPE QPGR-KATALOGART,
        L_QPK1CD          LIKE QPK1CD,
        I_CODEGRUPPE LIKE  QPGR-CODEGRUPPE,
        I_CODE LIKE  QPCD-CODE VALUE '*',
        W_ANS(1) TYPE C.

  DATA : T_CODEGRPTAB LIKE QPK1CODEGRP OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF L_DYNFIELDTAB OCCURS 10.
          INCLUDE STRUCTURE DYNPREAD.
  DATA : END   OF L_DYNFIELDTAB.

  MOVE : SY-REPID TO L_REPID,
         SY-DYNNR TO L_DYNNR.



  IF P_FIELDNAME = 'FEGRP'.


    MOVE 'QMGRP' TO L_DYNFIELDTAB-FIELDNAME.
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
** Chnaged by Furong on 10/19/09
    ELSEIF L_QMGRP+0(4) = 'MXPX'.
      T_CODEGRPTAB = '0'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '1'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '2'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '3'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '9'.
      APPEND T_CODEGRPTAB.
** End of change
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

  IF P_FIELDNAME = 'QMGRP'.

    MOVE  'OTGRP' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  L_QPK1CD-CODEGRUPPE TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    MOVE  'OTEIL' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  L_QPK1CD-CODE TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

  ENDIF.

  IF P_FIELDNAME = 'FEGRP' AND L_QPK1CD-CODEGRUPPE = '0'.

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

  ELSEIF P_FIELDNAME = 'FEGRP' AND L_QPK1CD-CODEGRUPPE = '4'.

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

*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*       EXPORTING
*            DYNAME               = L_REPID
*            DYNUMB               = L_DYNNR
*       TABLES
*            DYNPFIELDS           = L_DYNFIELDTAB
*       EXCEPTIONS
*            INVALID_ABAPWORKAREA = 01
*            INVALID_DYNPROFIELD  = 02
*            INVALID_DYNPRONAME   = 03
*            INVALID_DYNPRONUMMER = 04
*            INVALID_REQUEST      = 05
*            NO_FIELDDESCRIPTION  = 06
*            UNDEFIND_ERROR       = 07.

*  IF SY-SUBRC <> 0.
*    MESSAGE S099(Q3) .
*    EXIT.
*  ENDIF.

ENDFORM.                    " DISPLAY_SEARCH_HELP_READ
*&---------------------------------------------------------------------*
*&      Form  get_var1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VAR1203.
  DATA: VAL_TAB2 LIKE ZSCRAP_VAR. " OCCURS 0 WITH HEADER LINE.


*  SELECT * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF TABLE VAL_TAB2

* IF SY-TCODE = 'ZSCRAP1_ENG'.
  IF MARC-WERKS = 'E001'.
  SELECT SINGLE * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF VAL_TAB2
               WHERE LGORT = 'E499'.
** Changed on 12/13/11
  ELSEIF MARC-WERKS = 'E002'.
  SELECT SINGLE * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF VAL_TAB2
               WHERE LGORT = 'N499'.
** End on 12/13/11
  else.
  SELECT SINGLE * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF VAL_TAB2
              WHERE LGORT = 'P499'.
  ENDIF.

*  SELECT SINGLE * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF VAL_TAB2
*.

*read table VAL_TAB2 index 1.

  RMMG1-LGNUM = VAL_TAB2-LGNUM.

  BWART = VAL_TAB2-BWART.

  RMMG1-LGORT = VAL_TAB2-LGORT.


ENDFORM.                    " get_var1203
