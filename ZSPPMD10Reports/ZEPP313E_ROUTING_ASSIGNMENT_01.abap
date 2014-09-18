************************************************************************
* Program Name      : ZEPP313E_ROUTING_ASSIGNMENT
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.21.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K905477
* Addl Documentation:
* Description       : Reference Rate Routing Assignment
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZEPP313E_ROUTING_ASSIGNMENT_01
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: MAPL,
        PLKO,
        PLPO,
        PLAS,
        ZTBM_ABXOPVDT,
        ZTBM_ABYLFPDT.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_LFPD OCCURS 0,
        MATNR TYPE ZTBM_ABYLFPDT-MTNO,
        WERKS TYPE ZTBM_ABYLFPDT-PLNT,
        VERS  TYPE ZTBM_ABYLFPDT-VERS,
        TEXT  TYPE ZTBM_ABYLFPDT-TEXT,
        FVLD  TYPE ZTBM_ABYLFPDT-FVLD,
        TVLD  TYPE ZTBM_ABYLFPDT-TVLD,
        STLAL TYPE ZTBM_ABYLFPDT-ALTN,
        STLAN TYPE ZTBM_ABYLFPDT-USAG,
       $MATNR TYPE ZTBM_ABYLFPDT-MTNO,
      END OF IT_LFPD.

DATA: BEGIN OF IT_ROUT OCCURS 0,
        MATNR TYPE ZTBM_ABYLFPDT-MTNO,
        WERKS TYPE ZTBM_ABYLFPDT-PLNT,
        PLNNR TYPE PLAS-PLNNR,
        VERS  TYPE ZTBM_ABYLFPDT-VERS,
        TEXT  TYPE ZTBM_ABYLFPDT-TEXT,
        FVLD  TYPE ZTBM_ABYLFPDT-FVLD,
        TVLD  TYPE ZTBM_ABYLFPDT-TVLD,
        STLAL TYPE ZTBM_ABYLFPDT-ALTN,
        STLAN TYPE ZTBM_ABYLFPDT-USAG,
        MESSA LIKE CFGNL-MSGLIN,
        MSGTY TYPE  SY-MSGTY,
      END OF IT_ROUT.

*----------------------------------------------------------------------*
* BDC DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
DATA: BEGIN OF IT_MESSA OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESSA.
DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
*SELECT-OPTIONS: S_MTNO FOR ZTBM_ABYLFPDT-MTNO.
PARAMETERS:
  P_ZBDAT LIKE ZTBM_ABYLFPDT-ZBDAT DEFAULT SY-DATUM,
*  P_ZBTIM LIKE ZTBM_ABYLFPDT-ZBTIM,

  P_TCODE LIKE TSTC-TCODE DEFAULT 'CA21 & MM02'.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
*INITIALIZATION.
*  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

*AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
  PERFORM WRITE_PROCESS.
*  PERFORM MESSAGE_IT_MESSAGE.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'P_TCODE'.
      SCREEN-INPUT = 0.
    ENDIF.
    MODIFY SCREEN.
    CLEAR SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  PERFORM READ_ZTBM_ABYLFPDT.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABYLFPDT
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABYLFPDT.
  SELECT MTNO
         PLNT
         VERS
         TEXT
         FVLD
         TVLD
         ALTN
         USAG
       FROM ZTBM_ABYLFPDT
       INTO TABLE IT_LFPD
       WHERE ZEDAT EQ P_ZBDAT
       AND   ZRESULT NE 'E'.

  IF SY-SUBRC EQ 0.
    SORT IT_LFPD BY MATNR WERKS VERS.
  ELSE.
    WRITE: / TEXT-001.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABYLFPDT
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_PLNNR TYPE MKAL-PLNNR,
        M_PLNNR TYPE PLKO-PLNNR,
        N_PLNNR TYPE PLKO-PLNNR,
        Q_PLNNR TYPE PLKO-PLNNR,
        L_LINES TYPE SY-TABIX.

  DATA $MATNR LIKE MARA-MATNR.

  DATA: BEGIN OF LT_PLKO OCCURS 0,
          PLNNR TYPE PLKO-PLNNR,
          COUNT TYPE I,
        END   OF LT_PLKO.

  DATA: BEGIN OF LT_MAPL OCCURS 0,
          PLNTY TYPE MAPL-PLNTY,
          PLNNR TYPE MAPL-PLNNR,
          PLNAL TYPE MAPL-PLNAL,
        END   OF LT_MAPL.
  DATA $IX LIKE SY-TABIX.

* BDC OPTIONS
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
*  WA_OPT-RACOMMIT = 'X'.
  SORT LT_PLKO BY PLNNR.
*   REFERENCE RATE ROUTION CREATE
  CLEAR: IT_LFPD, L_PLNNR, M_PLNNR, LT_MAPL, LT_PLKO, L_TABIX,
         N_PLNNR, Q_PLNNR.
  REFRESH: LT_PLKO, LT_MAPL.
  LOOP AT IT_LFPD.
    $IX = SY-TABIX.
    CLEAR $MATNR.
    SELECT SINGLE * FROM ZTBM_ABXOPVDT
                   WHERE CARX EQ IT_LFPD-MATNR+5(2)
                            AND CLNO EQ '002'.
    IF SY-SUBRC EQ 0.

      IF ZTBM_ABXOPVDT-VALU EQ SPACE.
        MESSAGE E000 WITH 'Model 219 can not be blank!'.
        EXIT.
      ENDIF.

      CONCATENATE IT_LFPD-MATNR(5)
                  ZTBM_ABXOPVDT-VALU IT_LFPD-MATNR+7 INTO $MATNR.
    ENDIF.

    IT_LFPD-$MATNR = $MATNR+5(8).
    MODIFY IT_LFPD INDEX $IX TRANSPORTING $MATNR.

*   READ PLKO
    SELECT SINGLE PLNNR
                FROM PLKO
                INTO M_PLNNR
                WHERE PLNTY EQ 'M'
                AND   PLNNR EQ $MATNR+5(8)
                AND   LOEKZ NE 'X'.

    IF SY-SUBRC NE 0.
      REFRESH LT_PLKO. CLEAR LT_PLKO.
      SELECT PLNNR
       FROM PLKO
       INTO TABLE LT_PLKO
       WHERE PLNTY EQ 'M'
       AND   LOEKZ NE 'X'.

      LOOP AT LT_PLKO.
        L_TABIX = SY-TABIX.
        IF LT_PLKO-PLNNR+6(1) IS INITIAL.
          DELETE LT_PLKO INDEX L_TABIX.
          CLEAR LT_PLKO.
          CONTINUE.
        ELSE.

*////////////////////////////////////////*
*      01234 5 67 89 0 1 2 345678
*      ==========================
*      8B28AEM S4 J1 6 1 D 0076
*            M S4 J1 6 1 D
*            M B  E  E F T
*            0 12 34 5 6 7
*////////////////////////////////////////*

          DO 6 TIMES.
            CASE SY-INDEX.
              WHEN '1'.
                IF LT_PLKO-PLNNR(1) EQ $MATNR+5(1).
                  LT_PLKO-COUNT = LT_PLKO-COUNT + 200.
                ENDIF.
              WHEN '2'.
                IF LT_PLKO-PLNNR+1(2) EQ $MATNR+6(2).
                  LT_PLKO-COUNT = LT_PLKO-COUNT + 100.
                ENDIF.
              WHEN '3'.
                IF LT_PLKO-PLNNR+3(2) EQ $MATNR+8(2).
                  LT_PLKO-COUNT = LT_PLKO-COUNT + 50.
                ENDIF.
              WHEN '4'.
                IF LT_PLKO-PLNNR+5(1) EQ IT_LFPD-MATNR+10(1).
                  LT_PLKO-COUNT = LT_PLKO-COUNT + 20.
                ENDIF.
              WHEN '5'.
                IF LT_PLKO-PLNNR+6(1) EQ IT_LFPD-MATNR+11(1).
                  LT_PLKO-COUNT = LT_PLKO-COUNT + 10.
                ENDIF.
              WHEN '6'.
                IF LT_PLKO-PLNNR+7(1) EQ IT_LFPD-MATNR+12(1).
                  LT_PLKO-COUNT = LT_PLKO-COUNT + 1.
                ENDIF.
            ENDCASE.
          ENDDO.
          MODIFY LT_PLKO INDEX L_TABIX TRANSPORTING COUNT.
          CLEAR LT_PLKO.
        ENDIF.
      ENDLOOP.
*     SORTING
      SORT LT_PLKO BY COUNT DESCENDING.

      CLEAR LT_PLKO.
      READ TABLE LT_PLKO INDEX '1'.
      IF SY-SUBRC EQ 0.
        M_PLNNR = LT_PLKO-PLNNR.
      ENDIF.
*     REFERENCE RATE ROUTION CREATE
      PERFORM REFERENCE_RATE_ROUTION_CREATE
                             USING   $MATNR+5(8)
                                     M_PLNNR.
    ENDIF.
    CLEAR: IT_LFPD, M_PLNNR.
  ENDLOOP.
* WAIT UP TO 15 SECONDS
*  WAIT UP TO 15 SECONDS .
  CLEAR: IT_LFPD, L_PLNNR, M_PLNNR, LT_MAPL, LT_PLKO, L_TABIX,
         N_PLNNR, Q_PLNNR.
  REFRESH: LT_PLKO, LT_MAPL.

* FSC RATE ROUTION
  LOOP AT IT_LFPD.
    SELECT SINGLE PLNNR
                FROM MAPL
                INTO L_PLNNR
                WHERE MATNR EQ IT_LFPD-MATNR
                AND   WERKS EQ IT_LFPD-WERKS
                AND   PLNTY EQ 'R'
                AND   PLNNR LIKE '10%'
                AND   LOEKZ NE 'X'.
    IF SY-SUBRC NE 0.
      REFRESH LT_MAPL. CLEAR LT_MAPL.
*     READ MAPL
      SELECT  PLNTY PLNNR PLNAL
           FROM MAPL
           INTO TABLE LT_MAPL
           WHERE PLNTY EQ 'R'
           AND   PLNNR LIKE '10%'.
      IF SY-SUBRC EQ 0.
        SORT LT_MAPL BY PLNNR DESCENDING.
        READ TABLE LT_MAPL INDEX 1.
        L_PLNNR = LT_MAPL-PLNNR + 1.
*       FSC RATE ROUTION
        IF NOT L_PLNNR IS INITIAL.
          PERFORM FSC_RATE_ROUTION USING    IT_LFPD-MATNR
                                            IT_LFPD-WERKS
                                            L_PLNNR
                                            IT_LFPD-$MATNR.
          COMMIT WORK.
* WAIT UP TO 15 SECONDS
*          WAIT UP TO 3 SECONDS .
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: L_PLNNR, M_PLNNR, IT_LFPD.
  ENDLOOP.

* WAIT UP TO 5 SECONDS
*  WAIT UP TO 15 SECONDS .
  CLEAR: IT_LFPD, L_PLNNR, M_PLNNR, LT_MAPL, LT_PLKO, L_TABIX,
         N_PLNNR, Q_PLNNR.
  REFRESH: LT_PLKO, LT_MAPL.
* FSC ROUTING MM02 ASSIGN
  LOOP AT IT_LFPD.
    PERFORM READ_MAPL_CHECK USING    IT_LFPD-MATNR
                                     IT_LFPD-WERKS
                            CHANGING L_PLNNR.
    PERFORM FSC_ROUTING_MM02_EXECUTION USING L_PLNNR.
    CLEAR: L_PLNNR, IT_LFPD.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_MAPL_CHECK
*&---------------------------------------------------------------------*
FORM READ_MAPL_CHECK USING    P_MATNR
                              P_WERKS
                     CHANGING P_PLNNR.
  SELECT SINGLE PLNNR
              FROM MAPL
              INTO P_PLNNR
              WHERE MATNR EQ P_MATNR
              AND   WERKS EQ P_WERKS
              AND   PLNTY EQ 'R'
              AND   PLNNR LIKE '10%'
              AND   LOEKZ NE 'X'.
  IF SY-SUBRC NE 0.
    CLEAR P_PLNNR.
  ENDIF.

ENDFORM.                    " READ_MAPL_CHECK
*&---------------------------------------------------------------------*
*&      Form  REFERENCE_RATE_ROUTION_CREATE
*&---------------------------------------------------------------------*
FORM REFERENCE_RATE_ROUTION_CREATE USING    P_PLNNR
                                            Q_PLNNR.

  DATA: L_DATUM(10).
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
  WRITE: SY-DATUM TO L_DATUM.
  PERFORM DYNPRO USING:
     'X' 'SAPLCPDI'    '1001',
     ' ' 'RC271-PLNNR' P_PLNNR,   "
     ' ' 'RC271-STTAG' L_DATUM,  "
     ' ' 'BDC_OKCODE'  '=COPY',

     'X' 'SAPLCPCO'    '0101',
     ' ' 'TYP(01)'     'X',   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPCO'    '1020',
     ' ' 'RC271-PLNNR' Q_PLNNR,   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPDA'    '1200',
     ' ' 'PLKOD-STATU' '4',   "
     ' ' 'PLKOD-KTEXT' P_PLNNR,   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=MAAL',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=BU'.
  CALL TRANSACTION 'CA31'  USING IT_BDC
                  OPTIONS FROM WA_OPT
                  MESSAGES INTO IT_MESS.
  PERFORM APPEND_IT_MESSA.
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " REFERENCE_RATE_ROUTION_CREATE
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  FSC_RATE_ROUTION
*&---------------------------------------------------------------------*
FORM FSC_RATE_ROUTION USING    P_MATNR
                               P_WERKS
                               P_PLNNR
                               Q_PLNNR.
  DATA: L_DATUM(10).
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
  WRITE: SY-DATUM TO L_DATUM.
  PERFORM DYNPRO USING:
     'X' 'SAPLCPDI'    '1010',
     ' ' 'RC27M-MATNR' P_MATNR,   "
     ' ' 'RC27M-WERKS' P_WERKS,   "
     ' ' 'RC271-PLNNR' P_PLNNR,  "
     ' ' 'RC271-STTAG' L_DATUM, "
     ' ' 'BDC_OKCODE'  '=COPY',

     'X' 'SAPLCPCO'    '0101',
     ' ' 'TYP(02)'     'X',   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPCO'    '1020',
     ' ' 'RC271-PLNNR' Q_PLNNR,   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPDA'    '1200',
     ' ' 'PLKOD-STATU' '4',   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=MAAL',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=BU'.
  CALL TRANSACTION 'CA21'  USING IT_BDC
                  OPTIONS FROM WA_OPT
                  MESSAGES INTO IT_MESS.

  PERFORM APPEND_IT_MESSA.
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " FSC_RATE_ROUTION
*&---------------------------------------------------------------------*
*&      Form  FSC_ROUTING_MM02_EXECUTION
*&---------------------------------------------------------------------*
FORM FSC_ROUTING_MM02_EXECUTION USING    P_PLNNR.
  DATA: L_MSG  LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE  SY-MSGTY,
        L_CURSO(15) TYPE C.
  PERFORM READ_MAKL_COUNT CHANGING L_CURSO.
  PERFORM DYNPRO USING:
     'X' 'SAPLMGMM'    '0060',
     ' ' 'RMMG1-MATNR' IT_LFPD-MATNR,   "
     ' ' 'BDC_OKCODE'  '=AUSW',

     'X' 'SAPLMGMM'    '0070',
*       2003/12/21 SCREEN NUMBER CHANGE
     ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*       ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
     ' ' 'BDC_OKCODE'  '=ENTR',

     'X' 'SAPLMGMM'    '5004',
     ' ' 'BDC_OKCODE'  '=SP15',

     'X' 'SAPLMGMM'    '0081',
     ' ' 'RMMG1-WERKS' IT_LFPD-WERKS,  "
     ' ' 'BDC_OKCODE'  '=ENTR',
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'BDC_OKCODE'  '=PB03',

*     'X' 'SAPLMDIA'    '0100',
*     ' ' 'BDC_OKCODE'  '=NEWM',

     'X' 'SAPLMDIA'    '0100',
     ' ' 'BDC_CURSOR'  L_CURSO,
*     ' ' 'BDC_CURSOR' 'MKAL-VERID(01)',
     ' ' 'BDC_OKCODE'  '=DETA',

     'X' 'SAPLMDIA'    '0200',
*      RATE ROUTING ADDTIONAL
     ' ' 'MKAL-PLTYG'  'R',   "
     ' ' 'MKAL-PLNNG'  P_PLNNR,   "
     ' ' 'MKAL-ALNAG'  '1',   "
     ' ' 'MKAL-SERKZ'  'X',   "
     ' ' 'MKAL-MDV01'  '1',   "
     ' ' 'BDC_OKCODE'  '=ENTR',

     'X' 'SAPLMDIA'    '0100',
     ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5004',
     ' ' 'BDC_OKCODE'  '=BU'.

  CALL TRANSACTION 'MM02'  USING         IT_BDC
                           OPTIONS  FROM WA_OPT
                           MESSAGES INTO IT_MESS.
  L_MSGTY = SY-MSGTY.
  PERFORM RKC_MSG_STRING CHANGING L_MSG.
  MOVE-CORRESPONDING IT_LFPD TO IT_ROUT.
  IT_ROUT-MSGTY = L_MSGTY.
  IT_ROUT-MESSA = L_MSG.
  APPEND IT_ROUT. CLEAR IT_ROUT.
  CLEAR: IT_LFPD.
  PERFORM APPEND_IT_MESSA.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " FSC_ROUTING_MM02_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM RKC_MSG_STRING CHANGING P_MSG.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DESCRIBE TABLE IT_ROUT LINES WA_LINE_IDX.
  LOOP AT IT_ROUT WHERE MSGTY EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-013, WA_LINE_IDX.
  WRITE: / TEXT-014, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-014.
    FORMAT COLOR OFF.
    WRITE: /(18)  TEXT-002,
            (05)  TEXT-003,
            (18)  TEXT-012,
            (07)  TEXT-004,
            (40)  TEXT-005,
            (10)  TEXT-006,
            (10)  TEXT-007,
            (10)  TEXT-008,
            (10)  TEXT-009,
            (10)  TEXT-010,
                  TEXT-011.
    LOOP AT IT_ROUT WHERE MSGTY EQ 'E'.
      WRITE: /(18) IT_ROUT-MATNR,
              (05) IT_ROUT-WERKS,
              (18) IT_ROUT-PLNNR,
              (07) IT_ROUT-VERS,
              (40) IT_ROUT-TEXT,
              (10) IT_ROUT-FVLD,
              (10) IT_ROUT-TVLD,
              (10) IT_ROUT-STLAL,
              (10) IT_ROUT-STLAN,
              (10) IT_ROUT-MSGTY,
                   IT_ROUT-MESSA.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_MESSA
*&---------------------------------------------------------------------*
FORM APPEND_IT_MESSA.
  LOOP AT IT_MESS.
    IT_MESSA = IT_MESS.
    APPEND IT_MESSA.
    CLEAR IT_MESSA.
  ENDLOOP.
ENDFORM.                    " APPEND_IT_MESSA
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_IT_MESSAGE
*&---------------------------------------------------------------------*
FORM MESSAGE_IT_MESSAGE.
  WRITE: /(20) 'BDC Transaction code',
  (40) 'Batch input module name',
  (10) 'number',
  (10) 'type',
  (10) 'message',
  (20) 'Batch input message ID',
  (10) 'message NO',
  (40) 'Variable part of a message',
  (40) 'Variable part of a message',
  (40) 'Variable part of a message',
  (40) 'Variable part of a message',
  (40) 'Batch input monitoring activity',
  (132) 'Field name'.

  LOOP AT IT_MESSA.
    WRITE: /(20) IT_MESSA-TCODE,
(40) IT_MESSA-DYNAME,
(10) IT_MESSA-DYNUMB,
(10) IT_MESSA-MSGTYP,
(10) IT_MESSA-MSGSPRA,
(20) IT_MESSA-MSGID,
(10) IT_MESSA-MSGNR,
(40) IT_MESSA-MSGV1,
(40) IT_MESSA-MSGV2,
(40) IT_MESSA-MSGV3,
(40) IT_MESSA-MSGV4,
(40) IT_MESSA-ENV,
(132) IT_MESSA-FLDNAME.
  ENDLOOP.
ENDFORM.                    " MESSAGE_IT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  READ_MAKL_COUNT
*&---------------------------------------------------------------------*
FORM READ_MAKL_COUNT CHANGING P_CURSOR.
  DATA: BEGIN OF LT_MKAL OCCURS 0,
          MATNR TYPE MKAL-MATNR,
          WERKS TYPE MKAL-WERKS,
          VERID TYPE MKAL-VERID,
        END OF LT_MKAL.
  DATA: L_COUNT(2) TYPE N.
  SELECT MATNR
         WERKS
         VERID
       FROM MKAL
       INTO TABLE LT_MKAL
       WHERE MATNR EQ IT_LFPD-MATNR
       AND   WERKS EQ IT_LFPD-WERKS.
  IF SY-SUBRC EQ 0.
    SORT LT_MKAL BY MATNR WERKS VERID.
    READ TABLE LT_MKAL WITH KEY VERID = IT_LFPD-VERS
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      L_COUNT = SY-TABIX.
      CONCATENATE 'MKAL-VERID(' L_COUNT ')' INTO P_CURSOR.
    ENDIF.
  ENDIF.


ENDFORM.                    " READ_MAKL_COUNT
