  REPORT ZQM_SCARP_RCST NO STANDARD PAGE HEADING
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
        VENDER TYPE CHAR1,
        HMMA  TYPE CHAR1,
        STCK TYPE CHAR1,
        FECOD LIKE VIQMFE-FECOD,
        FECOD_TX(20),
        FEGRP LIKE VIQMFE-FEGRP,
        FEGRP_TX(20),
        CRCH TYPE CHAR1,
        LIFNUM LIKE QMEL-LIFNUM,
        CHANGE(4),
        RKMNG LIKE QMEL-RKMNG,
        TXT04 LIKE TJ30T-TXT04,
        INACT LIKE JEST-INACT,
        ESTAT LIKE TJ30-ESTAT,
        END OF IT_ITAB1.

  DATA: BEGIN OF IT_DATA OCCURS 0,
        SEL TYPE CHAR1,
        QMNUM LIKE QMEL-QMNUM,
        MATNR LIKE QMEL-MATNR,
        MAKTX LIKE MAKT-MAKTX,
        LIFNUM LIKE QMEL-LIFNUM,
        CHANGE(4),
*      RKMNG LIKE QMEL-RKMNG,
*      QN_VEND LIKE QMEL-QMNUM,
*      QN_HMMA LIKE QMEL-QMNUM,
        RKMNG(10),
        QN_VEND LIKE QMEL-QMNUM,
        QN_HMMA LIKE QMEL-QMNUM,
        MBLNR LIKE MKPF-MBLNR,
        MJAHR(4),  " LIKE MKPF-MJAHR,
        QTY_VEND(10),
        QTY_HMMA(10),
        TEXT(40),
        END OF IT_DATA.

  DATA:  CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
         CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'L',
         BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
         MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
         IT_TLINE TYPE TLINE OCCURS 0 WITH HEADER LINE.

  DATA: FEGRP TYPE FEGRP,
        FECOD TYPE FECOD,
        URCOD TYPE URCOD,
        URGRP TYPE URGRP.

  DATA : P_KATALOGART TYPE QPGR-KATALOGART,
           P_FIELDNAME  TYPE DYNPREAD-FIELDNAME,
           P_FIELDNAME1 TYPE DYNPREAD-FIELDNAME.


  DATA: XNAME    TYPE VRM_ID,
        NAME     TYPE VRM_ID    ,
        XLIST    TYPE VRM_VALUES,
        XVALUE   LIKE LINE OF XLIST,
        BEGIN OF YLIST     OCCURS 0,
           KEY(40) TYPE C,
           TEXT(80) TYPE C,
        END OF YLIST      .

  DATA: W_ERROR(1),
        W_DLFL(1),
        W_MATNR LIKE QMEL-MATNR,
        W_LIFNUM LIKE QMEL-LIFNUM,
        W_RKMNG LIKE QMEL-RKMNG,
        W_REVERSAL(1),
        W_QN_VEND(1),
        W_QN_HMMA(1),
        W_QN_NEW LIKE QMEL-QMNUM,
        W_UCOMM LIKE SY-UCOMM,
        W_MODE(4),
        W_NEW_R_HMMA LIKE QMEL-QMNUM,
        W_NEW_R_VEND LIKE QMEL-QMNUM.

  SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS : S_QMNUM FOR QMEL-QMNUM,
*                 S_ERDAT FOR QMEL-ERDAT,
*                 S_TXT04 FOR TJ30T-TXT04 OBLIGATORY,
*                 S_LIFNUM FOR QMEL-LIFNUM.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(12) TEXT-001.
*SELECTION-SCREEN POSITION 33.
*PARAMETERS: P_QMGRP LIKE VIQMEL-QMGRP,
*            P_QMCOD LIKE VIQMEL-QMCOD MODIF ID A.
*SELECTION-SCREEN END OF LINE.
*
*PARAMETERS: P_FEGRPN(8) AS LISTBOX VISIBLE LENGTH 15 OBLIGATORY.
  PARAMETERS: P_QMNUM LIKE QMEL-QMNUM OBLIGATORY.

  SELECTION-SCREEN END OF BLOCK BLOCK1.

  INITIALIZATION.

  START-OF-SELECTION.

    IF SY-UNAME <> '101457'.
      AUTHORITY-CHECK OBJECT 'ZQM_REV' ID 'ZQM_REV' FIELD 'X'.
      IF SY-SUBRC <> 0.
        MESSAGE I009 WITH 'No Authorization to access the function'.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR: W_QN_VEND, W_QN_HMMA.
    PERFORM GET_DATA.
    IF IT_DATA[] IS INITIAL.
      MESSAGE S009 WITH TEXT-M01.
    ELSE.
      CALL SCREEN 9000.
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
    DATA: L_TXT04 LIKE TJ02T-TXT04,
          L_COUNT TYPE I,
          L_NEW_QTY LIKE QMEL-RKMNG,
          LT_CHILD LIKE TABLE OF QMEL WITH HEADER LINE.

    DATA: LT_TEMP LIKE TABLE OF IT_ITAB1 WITH HEADER LINE,
          LT_QN_NEW LIKE TABLE OF QMEL WITH HEADER LINE.

    DATA: BEGIN OF LT_REV OCCURS 10,
          QMNUM LIKE QMEL-QMNUM,
          RKMNG LIKE QMEL-RKMNG,
          END OF LT_REV.

    CLEAR: W_REVERSAL.

    SELECT SINGLE TXT04 INTO L_TXT04
          FROM QMEL AS A
          INNER JOIN JEST AS D
          ON A~OBJNR = D~OBJNR
          INNER JOIN TJ30T AS E
          ON D~STAT = E~ESTAT
          WHERE A~QMNUM = P_QMNUM
            AND E~STSMA = 'ZQNSCRP1'
            AND INACT = ' '.

    IF L_TXT04 = 'VEND' OR L_TXT04 = 'RVSL'.
      SELECT A~QMNUM A~MATNR LIFNUM RKMNG E~ISTAT TXT04 INACT
         INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
         FROM QMEL AS A
         INNER JOIN JEST AS D
         ON A~OBJNR = D~OBJNR
         INNER JOIN TJ02T AS E
         ON D~STAT = E~ISTAT
         WHERE A~QMNUM = P_QMNUM
            AND SPRAS = 'EN'
           AND INACT = ' '.

      READ TABLE LT_TEMP WITH KEY TXT04 = 'NOCO'.
      IF SY-SUBRC = 0.
        W_MATNR = LT_TEMP-MATNR.
        W_LIFNUM = LT_TEMP-LIFNUM.
        W_RKMNG = LT_TEMP-RKMNG.
        READ TABLE LT_TEMP WITH KEY TXT04 = 'DLFL'.
        IF SY-SUBRC = 0.
          W_REVERSAL = 'X'.
          W_DLFL = 'X'.
        ELSE.
          CLEAR: W_REVERSAL.
          W_DLFL = ' '.
        ENDIF.
      ELSE.
        MESSAGE I009 WITH 'System status must be NOCO'.
        W_ERROR = 'X'.
        EXIT.
      ENDIF.
    ELSE.
   MESSAGE I009 WITH 'User status must be VEND/RVSL or QN is not valid'.
      W_ERROR = 'X'.
      EXIT.
    ENDIF.

    READ TABLE LT_TEMP INDEX 1.

    IT_DATA-QMNUM = LT_TEMP-QMNUM.
    IT_DATA-MATNR = LT_TEMP-MATNR.
    IT_DATA-LIFNUM = LT_TEMP-LIFNUM.
    IT_DATA-RKMNG = LT_TEMP-RKMNG.
    SELECT SINGLE MAKTX INTO IT_DATA-MAKTX
         FROM MAKT
         WHERE MATNR = IT_DATA-MATNR
         AND SPRAS = 'E'.

** Getting new QNs

    SELECT QMNUM RKMNG INTO TABLE LT_REV
       FROM QMEL AS A
       INNER JOIN JEST AS D
       ON A~OBJNR = D~OBJNR
       INNER JOIN TJ30T AS E
       ON D~STAT = E~ESTAT
       WHERE A~QWRNUM = P_QMNUM
         AND E~STSMA = 'ZQNSCRP1'
         AND INACT = ' '
         AND TXT04 = 'VEND'.

    IF SY-SUBRC = 0.
      READ TABLE LT_REV INDEX 1.
      IT_DATA-QN_VEND = LT_REV-QMNUM.
      L_NEW_QTY = LT_REV-RKMNG.
      W_NEW_R_VEND = LT_REV-RKMNG.
    ELSE.
      CLEAR: W_NEW_R_VEND.
    ENDIF.

    REFRESH LT_REV.

    SELECT QMNUM RKMNG INTO TABLE LT_REV
       FROM QMEL AS A
       INNER JOIN JEST AS D
       ON A~OBJNR = D~OBJNR
       INNER JOIN TJ30T AS E
       ON D~STAT = E~ESTAT
       WHERE A~QWRNUM = P_QMNUM
         AND E~STSMA = 'ZQNSCRP1'
         AND INACT = ' '
         AND TXT04 = 'HMMA'.

    IF SY-SUBRC = 0.
      READ TABLE LT_REV INDEX 1.
      IT_DATA-QN_HMMA = LT_REV-QMNUM.
      L_NEW_QTY = L_NEW_QTY + LT_REV-RKMNG.
      W_NEW_R_HMMA = LT_REV-RKMNG.
    ELSE.
      CLEAR: W_NEW_R_HMMA.
    ENDIF.

    IF IT_DATA-RKMNG <= L_NEW_QTY.
*    MESSAGE I009 WITH 'No balance QTY for R_VEND and R_HMMA'.
      W_ERROR = 'X'.
*    EXIT.
    ENDIF.

    IF IT_DATA-RKMNG = 1.
      IF W_NEW_R_HMMA IS INITIAL.
        IT_DATA-QTY_HMMA = 1.
        CLEAR: IT_DATA-QTY_VEND.
      ELSE.
        W_ERROR = 'X'.
      ENDIF.
    ELSE.
      IF W_NEW_R_HMMA = 0 AND W_NEW_R_VEND > 0.
        IT_DATA-QTY_HMMA = IT_DATA-RKMNG - W_NEW_R_VEND.
        IT_DATA-QTY_VEND = W_NEW_R_VEND - 0.
      ELSE.
        IF W_NEW_R_HMMA > 0 AND W_NEW_R_VEND = 0.
          IT_DATA-QTY_VEND = IT_DATA-RKMNG - W_NEW_R_HMMA.
          IT_DATA-QTY_VEND = W_NEW_R_VEND - 0.
        ELSE.
          IF W_NEW_R_HMMA > 0 AND W_NEW_R_VEND > 0.
            IT_DATA-QTY_HMMA = W_NEW_R_HMMA.
            IT_DATA-QTY_VEND = W_NEW_R_VEND.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND IT_DATA.

  ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE STATUS_9000 OUTPUT.
    DATA: BEGIN OF TAB_FUNC OCCURS 5,
          FCODE LIKE RSMPE-FUNC,
        END OF TAB_FUNC.

    IF W_ERROR = 'X' OR
       ( W_NEW_R_HMMA > 0  AND W_NEW_R_VEND > 0 ).
      CLEAR TAB_FUNC.
      MOVE 'REVERSAL' TO TAB_FUNC.
      APPEND TAB_FUNC.
      MOVE 'CR_QN_VEND' TO TAB_FUNC.
      APPEND TAB_FUNC.
      MOVE 'CR_QN_HMMA' TO TAB_FUNC.
      APPEND TAB_FUNC.
      SET PF-STATUS 'ST9000' EXCLUDING TAB_FUNC.
    ELSE.
      IF W_REVERSAL = 'X'.
        CLEAR TAB_FUNC.
        MOVE 'REVERSAL' TO TAB_FUNC.
        APPEND TAB_FUNC.
        IF W_QN_VEND = 'X' OR W_NEW_R_VEND > 0.
          MOVE 'CR_QN_VEND' TO TAB_FUNC.
          APPEND TAB_FUNC.
        ENDIF.
        IF W_QN_HMMA = 'X' OR W_NEW_R_HMMA > 0.
          MOVE 'CR_QN_HMMA' TO TAB_FUNC.
          APPEND TAB_FUNC.
        ENDIF.
        SET PF-STATUS 'ST9000' EXCLUDING TAB_FUNC.
      ELSE.
        IF W_QN_VEND IS INITIAL AND W_QN_HMMA IS INITIAL.
          IF IT_DATA-RKMNG = 1.
            MOVE 'CR_QN_VEND' TO TAB_FUNC.
            APPEND TAB_FUNC.
            SET PF-STATUS 'ST9000' EXCLUDING TAB_FUNC.
          ELSE.
            SET PF-STATUS 'ST9000'.
          ENDIF.
        ELSE.
          IF W_QN_VEND = 'X'.
            MOVE 'CR_QN_VEND' TO TAB_FUNC.
            APPEND TAB_FUNC.
          ENDIF.
          IF W_QN_HMMA = 'X'.
            MOVE 'CR_QN_HMMA' TO TAB_FUNC.
            APPEND TAB_FUNC.
          ENDIF.
          SET PF-STATUS 'ST9000' EXCLUDING TAB_FUNC.
        ENDIF.
      ENDIF.
    ENDIF.
    SET TITLEBAR 'ST9000'.
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
      WHEN  'REVERSAL'.
        PERFORM CLEAR_DATA.
        PERFORM PROCESS_REVERSAL  TABLES IT_TLINE.
        PERFORM CLEAR_DATA.
        PERFORM CLEAR_LONG_TEXT.
      WHEN  'CR_QN_VEND'.
        PERFORM CLEAR_DATA.
        W_UCOMM = 'CR_QN_VEND'.
        W_MODE = 'VEND'.
        PERFORM PERFORM_CREATE_QN TABLES IT_TLINE.
        PERFORM CLEAR_DATA.
        PERFORM CLEAR_LONG_TEXT.
      WHEN  'CR_QN_HMMA'.
        PERFORM CLEAR_DATA.
        W_UCOMM = 'CR_QN_HMMA'.
        W_MODE = 'HMMA'.
        PERFORM PERFORM_CREATE_QN TABLES IT_TLINE.
        PERFORM CLEAR_DATA.
        PERFORM CLEAR_LONG_TEXT.
      WHEN 'QM03'.
        PERFORM DISPALY_QM03.
        CLEAR: SY-UCOMM.
    ENDCASE.
    CLEAR: L_COMM.
  ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&spwizard: declaration of tablecontrol 'TC_9000' itself
  CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*&spwizard: output module for tc 'TC_9000'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
  MODULE TC_9000_CHANGE_TC_ATTR OUTPUT.
    DESCRIBE TABLE IT_DATA LINES TC_9000-LINES.
  ENDMODULE.

*&spwizard: input module for tc 'TC_9000'. do not change this line!
*&spwizard: modify table
  MODULE TC_9000_MODIFY INPUT.
    MODIFY IT_DATA
      INDEX TC_9000-CURRENT_LINE.
  ENDMODULE.
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
    LOOP AT IT_DATA.
      PERFORM UPDATE_STATUS TABLES IT_TLINE.
      READ TABLE IT_TLINE WITH KEY TDFORMAT = 'E'.
      IF SY-SUBRC = 0.
        MESSAGE I000 WITH IT_TLINE-TDLINE.
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
                                  IT_DATA-QMNUM.
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
*      -->P_IT_DATA_QMNUM  text
*----------------------------------------------------------------------*
  FORM RELEASE_LOCK USING P_QMNUM.
    IF NOT P_QMNUM IS INITIAL.
      WAIT UP TO 2 SECONDS.
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
      W_ERROR = 'X'.
      MESSAGE I009 WITH P_IT_TLINE-TDLINE.
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
        W_ERROR = 'X'.
        MESSAGE I009 WITH P_IT_TLINE-TDLINE.
      ELSE.
*    MESSAGE I000 WITH TEXT-M03.
        CONCATENATE 'Status was updated successfully: '
                IT_DATA-QMNUM INTO MSG SEPARATED BY SPACE.
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
*      -->P_IT_DATA_QMNUM  text
*----------------------------------------------------------------------*
  FORM UPDATE_STATUS TABLES P_IT_TLINE STRUCTURE TLINE.
    DATA: L_TXT04 LIKE IT_ITAB1-TXT04,
          L_STAT LIKE JEST-STAT,
          MSG(255).
    DATA: L_STATUS(2).

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

    IF P_FIELDNAME = 'FEGRP'.

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

    ELSEIF P_FIELDNAME = 'FEGRP'  AND L_QPK1CD-CODEGRUPPE = '4'.

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
    CLEAR: BDCDATA[], MESSTAB[], IT_TLINE[].
    CLEAR: IT_TLINE, W_ERROR,W_UCOMM.
*    LOOP AT IT_DATA.
*      CLEAR: IT_DATA-TEXT.
*      MODIFY IT_DATA TRANSPORTING TEXT.
*    ENDLOOP.
  ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Module  tc_9000_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE TC_9000_CHANGE_FIELD_ATTR OUTPUT.

    LOOP AT SCREEN.
      IF W_ERROR = 'X'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF SCREEN-GROUP1 = 'A2'.
        IF W_REVERSAL = 'X'.
          SCREEN-INPUT = 0.
*          SCREEN-ACTIVE = 0.
        ELSE.
          SCREEN-INPUT = 1.
          SCREEN-ACTIVE = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'A3'.
        IF W_QN_VEND = 'X' OR  W_NEW_R_VEND > 0 OR IT_DATA-RKMNG = 1.
          SCREEN-INPUT = 0.
*          SCREEN-ACTIVE = 0.
        ELSE.
          SCREEN-INPUT = 1.
          SCREEN-ACTIVE = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'A4'.
        IF W_QN_HMMA = 'X' OR W_NEW_R_HMMA > 0.
          SCREEN-INPUT = 0.
*          SCREEN-ACTIVE = 0.
        ELSE.
          SCREEN-INPUT = 1.
          SCREEN-ACTIVE = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
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
                                 IT_DATA-QMNUM.
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
                                        'J_STMAINT-ANWS(05)'.
        PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(05)'
                                   'X'.
      WHEN OTHERS.
        PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                      'J_STMAINT-ANWS(02)'.
        PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(02)'
                                   'X'.
    ENDCASE.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=OKAY'.

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
*&      Form  process_REVERSAL_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM  PROCESS_REVERSAL TABLES P_IT_TLINE STRUCTURE TLINE.

    DATA: L_MAT_DOC LIKE BAPI2017_GM_HEAD_02-MAT_DOC,
          L_DOC_YEAR LIKE BAPI2017_GM_HEAD_02-DOC_YEAR,
          L_PSTNG_DATE LIKE BAPI2017_GM_HEAD_02-PSTNG_DATE,
*        l_uname like GOODSMVT_PR_UNAME,
          L_DATE LIKE SY-DATUM,
          L_MATNR LIKE MSEG-MATNR,
          L_BWART LIKE MSEG-BWART,
          L_QMNUM LIKE QMEL-QMNUM,
          L_FROM_DATE LIKE SY-DATUM,
          L_TO_DATE LIKE SY-DATUM,
          L_LFGJA LIKE MARV-LFGJA,
          L_LFMON LIKE MARV-LFMON,
          L_VMGJA LIKE MARV-VMGJA,
          L_VMMON LIKE  MARV-VMMON.


    DATA: LT_RET LIKE TABLE OF BAPIRET2 WITH HEADER LINE,
          L_REV_SET LIKE TABLE OF BAPI2017_GM_HEAD_RET WITH HEADER LINE.

    READ TABLE IT_DATA INDEX 1.
    L_MAT_DOC = IT_DATA-MBLNR.


    SELECT SINGLE BUDAT INTO L_DATE
      FROM MKPF
      WHERE MBLNR = IT_DATA-MBLNR
        AND MJAHR = IT_DATA-MJAHR.

** checking posting date within current period.

*  SELECT SINGLE FRYE1 FRPE1 TOYE1  TOPE1
*     INTO (L_FRYE, L_FRPE, L_TOYE, L_TOPE)
*     FROM T001B
*     WHERE BUKRS = 'H201'
*       AND MKOAR = '+'.

    SELECT SINGLE  LFGJA LFMON VMGJA VMMON
         INTO (L_LFGJA, L_LFMON, L_VMGJA, L_VMMON)
         FROM MARV
         WHERE BUKRS = 'H201'.

    CONCATENATE L_LFGJA L_LFMON '01' INTO L_TO_DATE.

** CHANGED BY FURONG ON 07/16/09
*    CONCATENATE L_VMGJA L_VMMON '01' INTO L_FROM_DATE.
    CONCATENATE L_LFGJA L_LFMON '01' INTO L_FROM_DATE.
** END OF CHANGE

    CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
         EXPORTING
              DAY_IN            = L_TO_DATE
         IMPORTING
              LAST_DAY_OF_MONTH = L_TO_DATE
         EXCEPTIONS
              DAY_IN_NO_DATE    = 1
              OTHERS            = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF L_DATE >= L_FROM_DATE AND L_DATE  <= L_TO_DATE.

      L_DOC_YEAR = IT_DATA-MJAHR.
      L_PSTNG_DATE = L_DATE.
    ELSE.
      L_DOC_YEAR = L_FROM_DATE+0(4).
      L_PSTNG_DATE = L_FROM_DATE.
    ENDIF.

** Check Material No

    SELECT SINGLE MATNR BWART INTO (L_MATNR, L_BWART)
      FROM MSEG
      WHERE MBLNR = IT_DATA-MBLNR
        AND MJAHR = IT_DATA-MJAHR.

    IF L_MATNR <> IT_DATA-MATNR.
      W_ERROR = 'X'.
      MESSAGE I009 WITH 'Material Document is invalid'.
      EXIT.
    ENDIF.

    IF L_BWART <> '122' AND L_BWART <> '201'.
      W_ERROR = 'X'.
      MESSAGE I009 WITH 'Movement type on material document is invalid'.
      EXIT.
    ENDIF.

** Reversal material document

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        MATERIALDOCUMENT          = L_MAT_DOC
        MATDOCUMENTYEAR           = L_DOC_YEAR
       GOODSMVT_PSTNG_DATE       = L_PSTNG_DATE
*      GOODSMVT_PR_UNAME         = l_uname
     IMPORTING
       GOODSMVT_HEADRET          =  L_REV_SET
      TABLES
        RETURN                    = LT_RET
*      GOODSMVT_MATDOCITEM       =
              .
    READ TABLE L_REV_SET INDEX 1.

    IF L_REV_SET-MAT_DOC IS INITIAL.
      P_IT_TLINE-TDFORMAT = '*' .
      CONCATENATE 'Error: Reversal Material Doc:'
       LT_RET-MESSAGE INTO P_IT_TLINE-TDLINE.
      APPEND P_IT_TLINE.
      W_ERROR = 'X'.
      MESSAGE I009 WITH P_IT_TLINE-TDLINE.
      CLEAR P_IT_TLINE.
      EXIT.
    ELSE.
      P_IT_TLINE-TDFORMAT = '*' .
      CONCATENATE 'Material Document Reversal Successfully: '
            L_REV_SET-MAT_DOC L_DOC_YEAR INTO P_IT_TLINE-TDLINE
            SEPARATED BY SPACE.
      APPEND P_IT_TLINE.
      CLEAR P_IT_TLINE.
      W_REVERSAL = 'X'.
    ENDIF.

** Change NOCO for QN
    CLEAR: W_ERROR .

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RIWO00-QMNUM'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                  IT_DATA-QMNUM.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=WIAR'.

    PERFORM BDC_TRANSACTION1 TABLES IT_TLINE
                              USING 'QM02'.
    COMMIT WORK.
    IF W_ERROR = 'X'.
      EXIT.
    ENDIF.


** Update long text

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = IT_DATA-QMNUM
         IMPORTING
              OUTPUT = L_QMNUM.


    CALL FUNCTION 'IQS0_ADD_NOTIFICATION_LONGTEXT'
      EXPORTING
        I_QMNUM             = L_QMNUM
        I_POST              = 'X'
*   I_RESET             =
      TABLES
        T_INLINES           = P_IT_TLINE
* EXCEPTIONS
*   SHOW_MESSAGES       = 1
*   OTHERS              = 2
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      COMMIT WORK AND WAIT.
    ENDIF.

** Update CAUSE TEXT

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RIWO00-QMNUM'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                  L_QMNUM.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=AWST'.
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
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'VIQMUR-URTXT'.
    PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'
                                  IT_DATA-TEXT.
    PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'ANWS_STONR(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=A+'.

    PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0201'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'J_STMAINT-ANWS(05)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=OKAY'.
    PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(02)'
                                  ' '.
    PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(05)'
                                  'X'.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=LVMS'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'VIQMEL-QMGRP'.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BUCH'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'VIQMEL-QMGRP'.

    PERFORM BDC_TRANSACTION TABLES IT_TLINE
                                USING 'QM02'.
    IF W_ERROR = 'X'.
      CLEAR: W_REVERSAL.
    ELSE.
      W_REVERSAL = 'X'.
    ENDIF.

  ENDFORM.                    " process_REVERSAL_
*&---------------------------------------------------------------------*
*&      Form  CREATE_QN_VEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*----------------------------------------------------------------------*
  FORM PERFORM_CREATE_QN TABLES P_IT_TLINE STRUCTURE TLINE.
    DATA: L_QTY LIKE IT_DATA-QTY_VEND.
*        L_ERROR(1).

    IF  W_REVERSAL <> 'X'.
      MESSAGE I009 WITH 'Status of QN must be NOCO, DLFL & RVSL'.
      W_ERROR = 'X'.
      EXIT.
    ENDIF.

    IF ( W_MODE = 'VEND' AND IT_DATA-QTY_VEND IS INITIAL )
        OR ( W_MODE = 'HMMA' AND IT_DATA-QTY_HMMA IS INITIAL )
   .
      MESSAGE I009 WITH 'Error: QTY is zero'.
      W_ERROR = 'X'.
      EXIT.
    ENDIF.

    PERFORM CHECK_PARENT_QN USING W_ERROR.
    IF W_ERROR = 'X'.
      EXIT.
    ENDIF.

    L_QTY = IT_DATA-QTY_VEND + IT_DATA-QTY_HMMA.

    IF IT_DATA-RKMNG < L_QTY.
 MESSAGE I009 WITH 'The total of R_VEND and R_HMMA is more than QN QTY'
                                                                       .
      W_ERROR = 'X'.
      EXIT.
    ENDIF.

    SELECT SINGLE FEGRP FECOD INTO (FEGRP, FECOD)
      FROM QMFE
      WHERE QMNUM = IT_DATA-QMNUM.

    SELECT SINGLE URGRP URCOD INTO (URGRP, URCOD)
      FROM QMUR
      WHERE QMNUM = IT_DATA-QMNUM.

    CALL SCREEN '9002'.

  ENDFORM.                    " CREATE_QN_VEND
*&---------------------------------------------------------------------*
*&      Module  status_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE STATUS_9002 OUTPUT.

    SET PF-STATUS 'ST9002'.
    SET TITLEBAR 'ST9002'.
  ENDMODULE.                 " status_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_defect  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE DISPLAY_VALUES_DEFECT INPUT.

    CLEAR: P_KATALOGART,
           P_FIELDNAME,
           P_FIELDNAME1.


    P_KATALOGART = 'W'.
    P_FIELDNAME  = 'FEGRP'.
    P_FIELDNAME1 = 'FECOD'.
    PERFORM DISPLAY_SEARCH_HELP USING P_KATALOGART
                                      P_FIELDNAME
                                      P_FIELDNAME1.

  ENDMODULE.                 " display_values_defect  INPUT
*&---------------------------------------------------------------------*
*&      Module  display_values_causecode  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE DISPLAY_VALUES_CAUSECODE INPUT.

    CLEAR: P_KATALOGART,
           P_FIELDNAME,
           P_FIELDNAME1.

    P_KATALOGART = 'Y'.
    P_FIELDNAME  = 'URGRP'.
    P_FIELDNAME1 = 'URCOD'.
    PERFORM DISPLAY_SEARCH_HELP USING P_KATALOGART
                                      P_FIELDNAME
                                      P_FIELDNAME1.

  ENDMODULE.                 " display_values_causecode  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE USER_COMMAND_9002 INPUT.
    IF SY-UCOMM = 'EXIT'.
      LEAVE PROGRAM .
    ELSEIF SY-UCOMM = 'PROCESS'.
      PERFORM CREATE_QN.
      SET SCREEN 9000.
      LEAVE TO SCREEN 0.
    ELSEIF SY-UCOMM = 'BACK'.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDMODULE.                 " user_command_9002  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_QN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM CREATE_QN.
    DATA: L_QMNUM LIKE THEAD-TDNAME,
          L_QTY LIKE QMEL-RKMNG,
          L_RESULT TYPE CHAR1,
          L_MESSAGE TYPE CHAR255.

    CASE W_MODE.
      WHEN 'VEND'.
        IF FEGRP = '0'.
        ELSE.
          MESSAGE I009 WITH 'Resposibility must be ZERO for VEND'.
          EXIT.
        ENDIF.
      WHEN 'HMMA'.
        IF FEGRP = '0'.
          MESSAGE I009 WITH 'Resposibility cannot be ZERO for HMMA'.
          EXIT.
        ELSE.
        ENDIF.
    ENDCASE.

    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RIWO00-QWRNUM'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'RIWO00-QMART'
                                  'Q3'.
    PERFORM BDC_FIELD       USING 'RIWO00-QWRNUM'
                                  IT_DATA-QMNUM.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=BUCH'.
                                '=ANST'.
*perform bdc_field       using 'VIQMEL-QMGRP'
*                              record-QMGRP_003.
*perform bdc_field       using 'VIQMEL-QMCOD'
*                              record-QMCOD_004.
*perform bdc_field       using 'RQM00-MATNR'
*                              record-MATNR_005.
*perform bdc_field       using 'RQM00-MAWERK'
*                              record-MAWERK_006.
    IF W_MODE = 'VEND'.
      PERFORM BDC_FIELD       USING 'VIQMEL-RKMNG'
                                    IT_DATA-QTY_VEND.
    ELSE.
      PERFORM BDC_FIELD       USING 'VIQMEL-RKMNG'
                                   IT_DATA-QTY_HMMA.
    ENDIF.
*perform bdc_field       using 'VIQMEL-BZMNG'
*                              record-BZMNG_008.
*  PERFORM BDC_FIELD       USING 'VIQMFE-OTGRP'
*                                RECORD-OTGRP_009.
*  PERFORM BDC_FIELD       USING 'VIQMFE-OTEIL'
*                                RECORD-OTEIL_010.
    PERFORM BDC_FIELD       USING 'VIQMFE-FEGRP'
                                  FEGRP.
    PERFORM BDC_FIELD       USING 'VIQMFE-FECOD'
                                  FECOD.
    PERFORM BDC_FIELD       USING 'VIQMUR-URCOD'
                                  URCOD.
    PERFORM BDC_FIELD       USING 'VIQMUR-URGRP'
                                  URGRP.

    PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0300'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BACK'.
    IF W_MODE = 'VEND'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'J_STMAINT-ANWS(04)'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(01)'
                                    ' '.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(04)'
                                    'X'.
    ELSE.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                     'J_STMAINT-ANWS(05)'.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(01)'
                                    ' '.
      PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(05)'
                                    'X'.
    ENDIF.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=COWO'.
*perform bdc_field       using 'VIQMEL-QMGRP'
*                              record-QMGRP_018.
*perform bdc_field       using 'VIQMEL-QMCOD'
*                              record-QMCOD_019.
*perform bdc_field       using 'RQM00-MATNR'
*                              record-MATNR_020.
*perform bdc_field       using 'RQM00-MAWERK'
*                              record-MAWERK_021.
*perform bdc_field       using 'VIQMEL-RKMNG'
*                              record-RKMNG_022.
*perform bdc_field       using 'VIQMEL-BZMNG'
*                              record-BZMNG_023.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'VIQMUR-URTXT'.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              record-OTGRP_024.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              record-OTEIL_025.
*perform bdc_field       using 'VIQMFE-FEGRP'
*                              record-FEGRP_026.
*perform bdc_field       using 'VIQMFE-FECOD'
*                              record-FECOD_027.
*perform bdc_field       using 'VIQMUR-URCOD'
*                              record-URCOD_028.
*perform bdc_field       using 'VIQMUR-URGRP'
*                              record-URGRP_029.
*perform bdc_field       using 'VIQMUR-URTXT'
*                              record-URTXT_030.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BUCH'.
*perform bdc_field       using 'VIQMEL-QMGRP'
*                              record-QMGRP_031.
*perform bdc_field       using 'VIQMEL-QMCOD'
*                              record-QMCOD_032.
*perform bdc_field       using 'RQM00-MATNR'
*                              record-MATNR_033.
*perform bdc_field       using 'RQM00-MAWERK'
*                              record-MAWERK_034.
*perform bdc_field       using 'VIQMEL-RKMNG'
*                              record-RKMNG_035.
*perform bdc_field       using 'VIQMEL-BZMNG'
*                              record-BZMNG_036.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'VIQMUR-URTXT'.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              record-OTGRP_037.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              record-OTEIL_038.
*perform bdc_field       using 'VIQMFE-FEGRP'
*                              record-FEGRP_039.
*perform bdc_field       using 'VIQMFE-FECOD'
*                              record-FECOD_040.
*perform bdc_field       using 'VIQMUR-URCOD'
*                              record-URCOD_041.
*perform bdc_field       using 'VIQMUR-URGRP'
*                              record-URGRP_042.
    PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'
                                  IT_DATA-TEXT.

    PERFORM BDC_TRANSACTION1 TABLES IT_TLINE
                                 USING 'QM01'.
    IF NOT W_QN_NEW IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                INPUT  = W_QN_NEW
           IMPORTING
                OUTPUT = W_QN_NEW.
      L_QMNUM = W_QN_NEW.

** DELELTE LONG TEXT

      CALL FUNCTION 'DELETE_TEXT'
        EXPORTING
*        CLIENT                = SY-MANDT
          ID                    = 'LTQM'
          LANGUAGE              =  SY-LANGU
          NAME                  = L_QMNUM
          OBJECT                = 'QMEL'
          SAVEMODE_DIRECT       = 'X'
*        TEXTMEMORY_ONLY       = ' '
*        LOCAL_CAT             = ' '
       EXCEPTIONS
         NOT_FOUND             = 1
         OTHERS                = 2
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF W_MODE = 'VEND'.
        SELECT SINGLE QMNUM INTO L_QMNUM
               FROM VIQMEL AS C
               INNER JOIN JEST AS D
               ON C~OBJNR = D~OBJNR
                 INNER JOIN TJ30 AS E
                 ON D~STAT = E~ESTAT
                WHERE C~QMNUM = IT_DATA-QMNUM
                AND  STSMA = 'ZQNSCRP1'
                 AND ESTAT = 'E0009'
                 AND INACT = ' '.

        IF SY-SUBRC = 0.

          PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'RIWO00-QMNUM'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'RIWO00-QMNUM'
                                        W_QN_NEW.
          PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=ANST'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'VIQMEL-QMGRP'.

          PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0300'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'J_STMAINT-ANWSO(02)'.
          PERFORM BDC_FIELD       USING 'J_STMAINT-ANWSO(02)'
                                        'X'.
          PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0300'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BACK'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'VIQMEL-QMNUM'.
          PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
          PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                        '=BUCH'.
          PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                        'VIQMEL-QMGRP'.

          PERFORM BDC_TRANSACTION TABLES IT_TLINE
                                         USING 'QM02'.

        ENDIF.
      ENDIF.
      L_QTY = IT_DATA-QTY_VEND.

      CALL FUNCTION 'ZQM_SCRAP_DISPOSE'
           EXPORTING
                I_QMNUM = W_QN_NEW
                I_TYPE  = 'P'
                I_RKMNG = L_QTY
      IMPORTING
*     O_WERKS         =
*     O_MATNR         =
*     O_QMGRP         =
*     O_QMCOD         =
*     O_RKMNG         =
*     O_OTGRP         =
*     O_OTEIL         =
*     O_FEGRP         =
*     O_FECOD         =
*     O_URGRP         =
*     O_URCOD         =
*     O_TXT04         =
*     O_STTXT         =
*     O_TOLOCA        =
*     O_LIFNUM        =
       O_RESULT        = L_RESULT
       O_MESSAGE       = L_MESSAGE
      .

      MESSAGE I026(ZS) WITH L_MESSAGE.

    ENDIF.
    IF W_ERROR IS INITIAL.
      LOOP AT IT_DATA.
        IF W_UCOMM = 'CR_QN_VEND'.
          IT_DATA-QN_VEND = W_QN_NEW.
          W_QN_VEND = 'X'.
          W_NEW_R_VEND = L_QTY.
        ELSE.
          IT_DATA-QN_HMMA = W_QN_NEW.
          W_NEW_R_HMMA = L_QTY.
          W_QN_HMMA = 'X'.
        ENDIF.
        MODIFY IT_DATA.
      ENDLOOP.
    ENDIF.
  ENDFORM.                    " CREATE_QN
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*      -->P_2349   text
*----------------------------------------------------------------------*
  FORM BDC_TRANSACTION1 TABLES  P_IT_TLINE STRUCTURE TLINE
                        USING TCODE.

    DATA: L_SUBRC LIKE SY-SUBRC,
          MSG(255).

* call transaction using

    REFRESH: MESSTAB.

    CALL TRANSACTION TCODE USING BDCDATA
                     MODE   CTUMODE
                     UPDATE CUPDATE
                     MESSAGES INTO MESSTAB.
    L_SUBRC = SY-SUBRC.

    READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.

    IF SY-SUBRC = 0.
      LOOP AT MESSTAB.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
                   MESSTAB-MSGV4.

        CONCATENATE MSG MESSTAB-MSGV1 INTO MSG.
      ENDLOOP.
      W_ERROR = 'X'.
    ELSE.
      LOOP AT MESSTAB.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH MESSTAB-MSGV1 MESSTAB-MSGV2 MESSTAB-MSGV3
                   MESSTAB-MSGV4.

        CONCATENATE MSG MESSTAB-MSGV1 INTO MSG.
      ENDLOOP.

      REFRESH BDCDATA.
      IF TCODE = 'QM01'.
        W_QN_NEW = MESSTAB-MSGV1.
        MESSAGE I003(ZS) WITH MSG.
      ENDIF.
    ENDIF.
  ENDFORM.                    " BDC_TRANSACTION1
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
    READ TABLE IT_DATA INDEX L_LINE.
*  READ TABLE IT_DATA INDEX TC_9000-CURRENT_LINE.
    IF SY-SUBRC = 0.
      SET PARAMETER ID 'IQM' FIELD IT_DATA-QMNUM.
      CALL TRANSACTION 'QM03' AND SKIP FIRST SCREEN.
    ELSE.
      MESSAGE S009 WITH 'No data selected'.
    ENDIF.

  ENDFORM.                    " DISPALY_QM03
*&---------------------------------------------------------------------*
*&      Form  CHECK_PARENT_QN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM CHECK_PARENT_QN USING P_ERROR.
    DATA: LT_CHILD LIKE TABLE OF QMEL WITH HEADER LINE.
    DATA: L_LINE TYPE I,
          L_QTY LIKE QMEL-RKMNG.

*    SELECT * INTO TABLE LT_CHILD
*      FROM QMEL
*      WHERE QWRNUM = IT_DATA-QMNUM.
*    DESCRIBE TABLE LT_CHILD LINES L_LINE.
*    IF L_LINE > 2.
*      P_ERROR = 'X'.
*      MESSAGE I009 WITH 'Two QNs have been created'.
*    ENDIF.
*    LOOP AT LT_CHILD.
*      L_QTY = L_QTY + LT_CHILD-RKMNG.
*    ENDLOOP.
*
*    L_QTY = IT_DATA-RKMNG - L_QTY.
*    IF W_MODE = 'VEND' AND L_QTY < IT_DATA-QTY_VEND.
*      P_ERROR = 'X'.
*      MESSAGE I009 WITH 'Quantity entered exceeds original Quantity'.
*    ENDIF.
*    IF W_MODE = 'HMMA' AND L_QTY < IT_DATA-QTY_HMMA.
*      P_ERROR = 'X'.
*      MESSAGE I009 WITH 'Quantity entered exceeds original Quantity'.
*    ENDIF.
*
*    IF W_NEW_R_VEND > 0 AND IT_DATA-QTY_VEND > 0.
*      P_ERROR = 'X'.
*      MESSAGE I009 WITH 'Q_VEND entered exceeds allowws Quantity'.
*    ENDIF.
*
*    IF W_NEW_R_HMMA > 0 AND IT_DATA-QTY_HMMA > 0.
*      P_ERROR = 'X'.
*      MESSAGE I009 WITH 'Total quantity entered exceeds original
*Quantity'.
*    ENDIF.

    CLEAR: L_QTY.
    L_QTY = IT_DATA-QTY_HMMA + IT_DATA-QTY_VEND.
    IF  W_NEW_R_VEND = 0 AND  W_NEW_R_HMMA = 0.
      IF L_QTY > IT_DATA-RKMNG.
        P_ERROR = 'X'.
        MESSAGE I009 WITH 'Total QTY entered exceeds original QTY'.
      ENDIF.
    ELSE.
      IF IT_DATA-RKMNG <> L_QTY.
        P_ERROR = 'X'.
    MESSAGE I009 WITH 'Total scrap QTY has to be equal to original QTY'.
      ENDIF.
    ENDIF.
  ENDFORM.                    " CHECK_PARENT_QN
*&---------------------------------------------------------------------*
*&      Form  CLEAR_LONG_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
  FORM CLEAR_LONG_TEXT.
    LOOP AT IT_DATA.
      CLEAR: IT_DATA-TEXT.
      MODIFY IT_DATA TRANSPORTING TEXT.
    ENDLOOP.
  ENDFORM.                    " CLEAR_LONG_TEXT
