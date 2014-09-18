************************************************************************
* Program Name      : ZAPP_DISC_PLANORDER
* Creation Date     : 09/2009
* Development Request No :
* Addl Documentation:
* Description       : Material Discontinuation in Planned Orders
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZAPP_DISC_PLANORDER NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.


*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS: P_DATUM LIKE SY-DATUM.
*SELECTION-SCREEN END OF BLOCK B1.

DATA: IT_DCM_LIST LIKE TABLE OF ZTPP_DCM_LIST WITH HEADER LINE,
      IT_BDCDATA LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
      IT_NPLAN_LIST LIKE TABLE OF ZTPP_NPLAN_LIST WITH HEADER LINE.

DATA: BEGIN OF IT_PORDER OCCURS 0,
      PLNUM LIKE RESB-PLNUM,
      OBJEK LIKE MARA-MATNR,
      RP06_SHOP_DATE LIKE SY-DATUM,
      SERIAL LIKE AUSP-ATWRT,
      RP18_SHOP_DATE LIKE SY-DATUM,
      STATUS LIKE AUSP-ATWRT,
      END OF IT_PORDER..

DATA: W_PLNUM LIKE RESB-PLNUM,
      W_NEW_QTY(13),
      W_OLD_MATNR LIKE RESB-MATNR,
      W_NEW_MATNR  LIKE RESB-MATNR,
      W_MAX_SORTF(2) TYPE N.

DATA: CTUMODE LIKE CTU_PARAMS-DISMODE VALUE 'N',
      CUPDATE LIKE CTU_PARAMS-UPDMODE VALUE 'A'.

START-OF-SELECTION.

  PERFORM PROCESS_DATA.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  SELECT * INTO TABLE IT_DCM_LIST
   FROM ZTPP_DCM_LIST.

  CLEAR: W_MAX_SORTF.
  LOOP AT IT_DCM_LIST.
    IF W_MAX_SORTF < IT_DCM_LIST-SORTF.
      W_MAX_SORTF = IT_DCM_LIST-SORTF.
    ENDIF.
  ENDLOOP.

  W_MAX_SORTF = W_MAX_SORTF - 1.

  PERFORM GET_VEHICLES.

  IF IT_PORDER[] IS INITIAL.
    WRITE: 'No data was selected'.
  ELSE.
    LOOP AT IT_DCM_LIST.
      PERFORM CHECK_UPDATE_PLAN_ORDER.
    ENDLOOP.
    IF NOT IT_NPLAN_LIST[] IS INITIAL.
      INSERT ZTPP_NPLAN_LIST FROM TABLE IT_NPLAN_LIST.
      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
        WRITE: / 'ZTPP_NPLAN_LIST update error '.
      ENDIF.
    ENDIF.
    WRITE: / 'End of Process'.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_vehicles
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VEHICLES.

  DATA:  L_SUBRC    TYPE SY-SUBRC ,
         L_ATINN    TYPE AUSP-ATINN,
         L_NUM(08) TYPE N.

  DATA: BEGIN OF LT_OBJEK OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
        END OF LT_OBJEK .

  RANGES: R_PROG FOR AUSP-ATWRT.

  R_PROG-SIGN   = 'I'.
  R_PROG-OPTION = 'BT'.
  R_PROG-LOW    = '06'.
  R_PROG-HIGH   = W_MAX_SORTF.      "IT_DOC_LIST-SORTF.
  APPEND R_PROG.

  SELECT SINGLE ATINN INTO L_ATINN
  FROM CABN
 WHERE ATNAM = 'P_RP_STATUS'.

  SELECT OBJEK ATWRT INTO TABLE LT_OBJEK
     FROM AUSP
    WHERE ATINN = L_ATINN
      AND KLART = '002'
       AND ATWRT IN R_PROG.

  SELECT SINGLE ATINN INTO L_ATINN
  FROM CABN
  WHERE ATNAM = 'P_USAGE_CAR'.

  LOOP AT LT_OBJEK.
    IT_PORDER-OBJEK = LT_OBJEK-OBJEK.
    IT_PORDER-STATUS = LT_OBJEK-ATWRT.

    SELECT SINGLE OBJEK INTO LT_OBJEK-OBJEK
      FROM AUSP
     WHERE OBJEK = LT_OBJEK-OBJEK
       AND ATINN = L_ATINN
       AND KLART = '002'
       AND ATWRT IN ('S', 'D').

    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    PERFORM READ_NORMAL_CLASSIFICATION USING IT_PORDER-OBJEK
                                             'P_PLAN_ORDER'
                                    CHANGING IT_PORDER-PLNUM.

    PERFORM READ_SHOP_DATE USING    IT_PORDER-OBJEK
                                    'P_RP06_SHOP_DATE'
                           CHANGING IT_PORDER-RP06_SHOP_DATE.

    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_PORDER-OBJEK
                                                'P_RP06_SERIAL'
                                       CHANGING IT_PORDER-SERIAL .

*    PERFORM READ_SHOP_DATE USING    IT_PORDER-OBJEK
*                                    'P_RP18_SHOP_DATE'
*                           CHANGING IT_PORDER-RP18_SHOP_DATE.

    APPEND IT_PORDER.
  ENDLOOP.

  SORT IT_PORDER BY STATUS RP06_SHOP_DATE SERIAL RP18_SHOP_DATE.

ENDFORM.                    " get_vehicles
*----------------------------------------------------------------------*
*      -->P_IT_APP246_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP246_MI  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASSIFICATION USING    P_VMNO
                                         P_CHAR
                                CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .

ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  read_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_APP246_OBJEK  text
*      -->P_L_ATNAM  text
*      <--P_IT_TEMP_APP246_PIN  text
*----------------------------------------------------------------------*
FORM READ_SHOP_DATE USING    P_OBJEK
                             P_ATNAM
                    CHANGING P_DATE.
  DATA: L_ATFLV TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.

  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM       .

  P_DATE = L_NUM = L_ATFLV.

ENDFORM.                    " read_shop_date
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PLAN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_UPDATE_PLAN_ORDER.

  DATA: BEGIN OF LT_RESB OCCURS 0,
         PLNUM LIKE RESB-PLNUM,
         RSNUM LIKE RESB-RSNUM,
         RP06_SHOP_DATE LIKE SY-DATUM,
         SERIAL LIKE AUSP-ATWRT,
         MATNR LIKE RESB-MATNR,
         ERFMG LIKE RESB-ERFMG,
         BODYNO LIKE AUSP-ATWRT,
        END OF LT_RESB.

  DATA: LT_TEMP LIKE TABLE OF IT_PORDER WITH HEADER LINE.
  DATA: L_BAL LIKE IT_DCM_LIST-BALQTY,
        L_FLAG(1),
        L_FLAG_NEW(1).

  LT_TEMP[] = IT_PORDER[].

  DELETE LT_TEMP WHERE STATUS > IT_DCM_LIST-SORTF.
  SORT LT_TEMP BY PLNUM.

  SELECT PLNUM RSNUM MATNR ERFMG
    INTO CORRESPONDING FIELDS OF TABLE LT_RESB
    FROM RESB
    FOR ALL ENTRIES IN LT_TEMP
  WHERE PLNUM = LT_TEMP-PLNUM
   AND ( MATNR = IT_DCM_LIST-FRMATNR OR MATNR = IT_DCM_LIST-TOMATNR )
   AND XLOEK = ' '.

  IF SY-SUBRC = 0.

    L_BAL = IT_DCM_LIST-BALQTY.

    LOOP AT LT_RESB.
      READ TABLE LT_TEMP WITH KEY PLNUM = LT_RESB-PLNUM
                     BINARY SEARCH.
      LT_RESB-RP06_SHOP_DATE = LT_TEMP-RP06_SHOP_DATE.
      LT_RESB-SERIAL = LT_TEMP-SERIAL.
      LT_RESB-BODYNO = LT_TEMP-OBJEK.
      MODIFY LT_RESB.
    ENDLOOP.

    SORT LT_RESB BY RP06_SHOP_DATE SERIAL PLNUM MATNR.

    CALL FUNCTION 'ENQUEUE_EZ_DCM_LIST'
     EXPORTING
       MODE_ZTPP_DCM_LIST       = 'E'
       MANDT                    = SY-MANDT
       FRMATNR                  = IT_DCM_LIST-FRMATNR
*   TOMATNR                  =
     EXCEPTIONS
       FOREIGN_LOCK             = 1
       SYSTEM_FAILURE           = 2
       OTHERS                   = 3
              .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: L_FLAG_NEW, IT_NPLAN_LIST.

    LOOP AT LT_RESB.
      IF L_BAL >= LT_RESB-ERFMG.
        IF LT_RESB-MATNR = IT_DCM_LIST-TOMATNR.
          W_PLNUM  = LT_RESB-PLNUM.
          W_OLD_MATNR = IT_DCM_LIST-TOMATNR.
          W_NEW_MATNR = IT_DCM_LIST-FRMATNR.

          PERFORM CHANGE_PLAN_ORDER USING LT_RESB-ERFMG L_FLAG.
          IF L_FLAG IS INITIAL.
            L_BAL = L_BAL - LT_RESB-ERFMG.
          ENDIF.
        ELSE.
          L_BAL = L_BAL - LT_RESB-ERFMG.
        ENDIF.

      ELSE.
        IF L_FLAG_NEW IS INITIAL.
          L_FLAG_NEW = 'X'.
          IT_NPLAN_LIST-CRDATE = SY-DATUM.
          IT_NPLAN_LIST-CRTIME = SY-UZEIT.
          IT_NPLAN_LIST-MODEL = LT_RESB-BODYNO+0(3).
          IT_NPLAN_LIST-BODYNO = LT_RESB-BODYNO+3(6).
          IT_NPLAN_LIST-PLNUM = LT_RESB-PLNUM.
          IT_NPLAN_LIST-RP06_DATE = LT_RESB-RP06_SHOP_DATE.
          IT_NPLAN_LIST-RP06_SER =  LT_RESB-SERIAL.
          IT_NPLAN_LIST-OLD_MATNR =  IT_DCM_LIST-FRMATNR.
          IT_NPLAN_LIST-NEW_MATNR = IT_DCM_LIST-TOMATNR.
          APPEND IT_NPLAN_LIST.
        ENDIF.
        IF LT_RESB-MATNR = IT_DCM_LIST-FRMATNR.
          W_PLNUM  = LT_RESB-PLNUM.
          W_OLD_MATNR = IT_DCM_LIST-FRMATNR.
          W_NEW_MATNR = IT_DCM_LIST-TOMATNR.

          PERFORM CHANGE_PLAN_ORDER USING LT_RESB-ERFMG L_FLAG.
        ENDIF.
      ENDIF.
*      CASE LT_RESB-MATNR.
*        WHEN IT_DCM_LIST-FRMATNR.
*
*          IF L_BAL >= LT_RESB-ERFMG.
*            L_BAL = L_BAL - LT_RESB-ERFMG.
*          ELSE.
*            IF L_BAL > 0.
*              L_BAL = 0.
*            ENDIF.
*          ENDIF.
*        WHEN IT_DCM_LIST-TOMATNR.
*          IF L_BAL >= LT_RESB-ERFMG.
*            W_PLNUM  = LT_RESB-PLNUM.
*            W_OLD_MATNR = IT_DCM_LIST-TOMATNR.
*            W_NEW_MATNR = IT_DCM_LIST-FRMATNR.
*
*            PERFORM CHANGE_PLAN_ORDER USING LT_RESB-ERFMG L_FLAG.
*            IF L_FLAG IS INITIAL.
*              L_BAL = L_BAL - LT_RESB-ERFMG.
*            ENDIF.
*          ELSE.
*            IF L_BAL > 0.
*              W_PLNUM  = LT_RESB-PLNUM.
*              W_NEW_QTY = LT_RESB-ERFMG - L_BAL.
*              W_OLD_MATNR = IT_DCM_LIST-TOMATNR.
*              W_NEW_MATNR = IT_DCM_LIST-TOMATNR.
*              PERFORM CHANGE_PLAN_ORDER USING W_NEW_QTY L_FLAG.
*              IF L_FLAG IS INITIAL.
*
**** INSERT A NEW LINE WITH OLD MATNR
*                W_PLNUM  = LT_RESB-PLNUM.
*                W_NEW_MATNR = IT_DCM_LIST-FRMATNR.
*                PERFORM ADD_PLAN_ORDER USING L_BAL L_FLAG.
*                IF L_FLAG IS INITIAL.
*                  L_BAL = 0.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*      ENDCASE.
    ENDLOOP.
    UPDATE ZTPP_DCM_LIST SET BALQTY = L_BAL
             WHERE FRMATNR = IT_DCM_LIST-FRMATNR
               AND TOMATNR = IT_DCM_LIST-TOMATNR.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EZ_DCM_LIST'
         EXPORTING
              MODE_ZTPP_DCM_LIST = 'E'
              MANDT              = SY-MANDT
              FRMATNR            = IT_DCM_LIST-FRMATNR.
  ELSE.
    WRITE: /(35) 'No planned order was selectd for',
           (22) IT_DCM_LIST-FRMATNR,
           (22) IT_DCM_LIST-TOMATNR.
  ENDIF.
ENDFORM.                    " UPDATE_PLAN_ORDER
*&---------------------------------------------------------------------*
*&      Form  change_PLAN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_PLAN_ORDER USING P_QTY P_FLAG.

  PERFORM GENERATE_BDC_DATA_NEW USING P_QTY  .

  PERFORM GENERATE_BDC_DATA_END.

  PERFORM CALL_TRANSACTION USING P_FLAG.

ENDFORM.                    " CHANGE_PLAN_ORDER

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_NEW
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_NEW  USING PA_QTY .
  DATA: L_QTY(40).

  L_QTY = PA_QTY.  CONDENSE L_QTY.

*----> PLANNED NUMBER
  PERFORM BDC_DYNPRO USING  'SAPMM61P'          '0101'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'        'RM61P-PLNUM',
                            'BDC_OKCODE'        '/00',
                            'RM61P-PLNUM'       W_PLNUM.

*----> COMPONENT
  PERFORM BDC_DYNPRO USING  'SAPLM61O'          '0110'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'        'PLAF-MATNR',
                            'BDC_OKCODE'        '=POSU'.
*----> Search Material..
  PERFORM BDC_DYNPRO USING  'SAPLM61Q'          '0115'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'        'MDPM-MATNR(01)' ,
                            'BDC_OKCODE'        '=SUCH'.
*----> Search POP-UP Screen..
  PERFORM BDC_DYNPRO USING  'SAPLM61Q'          '0300'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'        'SUCHL-MATNR',
                            'SUCHL-MATNR'       W_OLD_MATNR,
                            'BDC_OKCODE'        '=SUCH'.

*----> Change the Material and Quantity..
  PERFORM BDC_DYNPRO USING  'SAPLM61Q'          '0115'.
  PERFORM BDC_FIELD USING : 'MDPM-ERFMG(01)'    L_QTY,
                            'MDPM-MATNR(01)'    W_NEW_MATNR.
ENDFORM.                    " GENERATE_BDC_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_END
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_END.
*---> Remain Function Code..
  PERFORM BDC_FIELD USING   'BDC_OKCODE'        '=BACK'.

*---> SAVE
  PERFORM BDC_DYNPRO USING  'SAPLM61O'          '0110'.
  PERFORM BDC_FIELD USING   'BDC_OKCODE'        '=HZPL'.
ENDFORM.                    " GENERATE_BDC_DATA_END
*&---------------------------------------------------------------------*
*&      Form  call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION USING P_FLAG.

  DATA : L_MSG LIKE CFGNL-MSGLIN,
         MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

  CALL TRANSACTION 'MD12' USING IT_BDCDATA
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.

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
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.

  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      CONCATENATE TEXT-801 L_MSG ')'  INTO L_MSG.
      P_FLAG = 'X'.
      WRITE: /(5) 'From:',
              (22) W_OLD_MATNR,
              (4) 'To:',
              (22) W_NEW_MATNR,
              (100) L_MSG.
    WHEN OTHERS.     " 'I', 'S' :SUCCESS
      CLEAR: P_FLAG.
      WRITE: /(5) 'From:',
            (22) W_OLD_MATNR,
            (4) 'To:',
            (22) W_NEW_MATNR,
            (100) TEXT-802.
  ENDCASE.

  CLEAR IT_BDCDATA.
  REFRESH IT_BDCDATA.

ENDFORM.                    " call_transaction

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING  P_PROGRAM
                       P_DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = P_PROGRAM.
  IT_BDCDATA-DYNPRO   = P_DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM  P_FVAL.
*  IF P_FVAL <> Nodata.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = P_FNAM.
  IT_BDCDATA-FVAL = P_FVAL.
  APPEND IT_BDCDATA.
*  ENDIF.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  add_PLAN_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_BAL  text
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
FORM ADD_PLAN_ORDER USING  P_QTY P_FLAG.

  PERFORM BDC_DYNPRO      USING 'SAPMM61P' '0101'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM61P-PLNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RM61P-PLNUM'
                                W_PLNUM.

  PERFORM BDC_DYNPRO      USING 'SAPLM61O' '0110'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'PLAF-MATNR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=POSU'.

  PERFORM BDC_DYNPRO      USING 'SAPLM61Q' '0115'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=POS1'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'MDPA-MATNR'.
  PERFORM BDC_DYNPRO      USING 'SAPLM61Q' '0110'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'MDPM-ERFMG(02)'.
  PERFORM BDC_FIELD       USING 'MDPM-MATNR(02)'
                                W_NEW_MATNR.
  PERFORM BDC_FIELD       USING 'MDPM-ERFMG(02)'
                                P_QTY.

  PERFORM BDC_DYNPRO      USING 'SAPLM61Q' '0110'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BACK'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'MDPM-MATNR(03)'.
  PERFORM BDC_DYNPRO      USING 'SAPLM61O' '0110'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'PLAF-MATNR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=HZPL'.

  PERFORM CALL_TRANSACTION USING P_FLAG.

ENDFORM.                    " ADD_PLAN_ORDER
