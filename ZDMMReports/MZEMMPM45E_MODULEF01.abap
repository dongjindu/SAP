*----------------------------------------------------------------------*
***INCLUDE MZEMMPM45E_MODULEF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  date_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATE_SELECTION.

  DATA : WA_MATNR LIKE MARA-MATNR.
  DATA : WA_CHANGE(2) TYPE C VALUE '* ' .

  CLEAR : IT_MODULE , IT_MODULE[], IT_200, IT_200[].

  LOOP AT IT_100.
    MOVE IT_100-MATNR TO WA_MATNR .

    TRANSLATE WA_MATNR USING WA_CHANGE.
    CONDENSE WA_MATNR .
    CONCATENATE '___' WA_MATNR(2) '%' INTO WA_MATNR.

    SELECT DISTINCT  MTNO AS MATNR
                     PLNT AS WERKS
          APPENDING CORRESPONDING FIELDS OF TABLE IT_MODULE
          FROM  ZTBM_ABXDULDT
          WHERE USAG = '2'
            AND MTNO LIKE WA_MATNR

            AND DATUB = '99991231'
            .

    IF SY-SUBRC NE 0.
      MESSAGE E015(ZMMM) .
    ENDIF.
    IT_MODULE-KLVAR           = IT_100-KLVAR .
    IT_MODULE-WERKS           = IT_100-WERKS .
    IT_MODULE-LOSGR           = IT_100-LOSGR.
    IT_MODULE-TVERS           = IT_100-TVERS.
    IT_MODULE-KADAT           = IT_100-KADAT.
    IT_MODULE-BIDAT           = IT_100-BIDAT.
    IT_MODULE-RAW_MATERIAL    = IT_100-COMP.
    IT_MODULE-TYPPS           = IT_100-TYPPS.
    IT_MODULE-KSTAR           = IT_100-KSTAR.
    IT_MODULE-MENGE           = IT_100-MENGE.
    IT_MODULE-LPREIS          = IT_100-LPREIS.
    IT_MODULE-LPREIFX         = IT_100-LPREIFX.
    IT_MODULE-LPEINH          = IT_100-LPEINH.
    IT_MODULE-LIFNR           = IT_100-LIFNR.
    IT_MODULE-MEINS           = IT_100-MEINS.


    MODIFY IT_MODULE TRANSPORTING KLVAR
                               WERKS
                               LOSGR
                               TVERS
                               KADAT
                               BIDAT
                               RAW_MATERIAL
                               TYPPS
                               KSTAR
                               MENGE
                               LPREIS
                               LPREIFX
                               LPEINH
                               LIFNR
                               MEINS
                               WHERE KLVAR = ' '.
  ENDLOOP.


  LOOP AT IT_MODULE.
    MOVE-CORRESPONDING  IT_MODULE TO IT_200.
    WRITE IT_MODULE-MENGE  UNIT IT_MODULE-MEINS TO IT_200-MENGE.
    APPEND IT_200. CLEAR IT_200.
  ENDLOOP.

  CTU_PARAMS-DISMODE = 'N'.
* TABLE CONTROL
  SORT IT_200 BY MATNR.
  REFRESH CONTROL 'TC_200' FROM SCREEN 200.
  DESCRIBE TABLE IT_200 LINES TC_200-LINES.
  CALL SCREEN 200 .

ENDFORM.                    " date_selection
*&---------------------------------------------------------------------*
*&      Form  SAVE_AND_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_AND_POSTING.
  PERFORM SAVE_DATA.
  PERFORM POSTING_DATE.



ENDFORM.                    " SAVE_AND_POSTING
*&---------------------------------------------------------------------*
*&      Form  lock_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOCK_OBJECT.

  CALL FUNCTION 'DEQUEUE_EZ_ZTMM_MODULE'
       EXPORTING
            MODE_ZTMM_MODULE = 'X'
            MANDT            = SY-MANDT.
  CALL FUNCTION 'ENQUEUE_EZ_ZTMM_MODULE'
       EXPORTING
            MODE_ZTMM_MODULE = 'X'
            MANDT            = SY-MANDT.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " lock_object
*&---------------------------------------------------------------------*
*&      Form  posting_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POSTING_DATE.
** Type-Pools
  TYPE-POOLS : KKPI , KCDE .

* For Posting
  DATA : IT_TRS_DATA TYPE KKPI_TRANSFER_DATA
                     WITH HEADER LINE .

  DATA : WA_MODE TYPE C VALUE 'N',
         WA_UPDATE TYPE C VALUE 'A'.

*KKPI_BDC_ADD_ON_COSTS_CREATE
*
  DATA : IT_L_POS LIKE IT_TRS_DATA-POSITION
                  WITH HEADER LINE .

  CLEAR : IT_TRS_DATA, IT_TRS_DATA[].

  SORT IT_200 BY KEY.

  LOOP AT IT_200.
    AT NEW KEY.
      CLEAR : IT_L_POS, IT_L_POS[].
      CLEAR IT_TRS_DATA.
      MOVE-CORRESPONDING IT_200 TO IT_TRS_DATA.
    ENDAT.

    MOVE-CORRESPONDING IT_200 TO IT_L_POS.
    APPEND IT_L_POS.
    CLEAR  IT_L_POS.

    AT END OF KEY.
      IT_TRS_DATA-POSITION[] = IT_L_POS[].
      CLEAR : IT_L_POS, IT_L_POS[].
      APPEND IT_TRS_DATA.
      CLEAR  IT_TRS_DATA.
    ENDAT.
  ENDLOOP.

  CLEAR  IT_TRS_DATA.


  CALL FUNCTION 'KKPI_BDC_ADD_ON_COSTS_CREATE'
       EXPORTING
            USER                   = SY-UNAME
            MODE                   = CTU_PARAMS-DISMODE
            UPDATE                 = WA_UPDATE
            IT_TRANSFER_DATA       = IT_TRS_DATA[]
       EXCEPTIONS
            CALL_TRANSACTION_ERROR = 1
            NO_TRANSFER_DATA       = 2
            WRONG_MODE_PARAMETER   = 3
            OTHERS                 = 4.

  IF SY-SUBRC <> 0.
    CASE SY-SUBRC .
      WHEN '1'.
        MESSAGE E009(ZMMM) WITH 'CALL_TRANSACTION_ERROR'.
      WHEN '2'.
        MESSAGE E009(ZMMM) WITH 'NO_TRANSFER_DATA'.
      WHEN '3'.
        MESSAGE E009(ZMMM) WITH 'WRONG_MODE_PARAMETER'.
      WHEN '4'.
        MESSAGE E009(ZMMM) WITH 'WRONG_MODE_PARAMETER'.
      WHEN OTHERS .
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDCASE.
  ELSE.
    COMMIT WORK .
    MESSAGE S009(ZMMM) WITH 'Cost estimate Complited AND Data saved .'.

  ENDIF.

ENDFORM.                    " posting_date
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.

  IT_MODULE-ANDAT     = SY-DATUM.
  IT_MODULE-ANNAM     = SY-UNAME.

  MODIFY IT_MODULE TRANSPORTING ANDAT
                                ANNAM
                               WHERE ANNAM  =  '' .

  IT_MODULE-AEDAT     = SY-DATUM.
  IT_MODULE-AENAM     = SY-UNAME.

  MODIFY IT_MODULE TRANSPORTING AEDAT
                                AENAM
                               WHERE MATNR <>  '' .


  MODIFY ZTMM_MODULE FROM TABLE IT_MODULE.

  IF SY-SUBRC EQ 0.
  ELSE.
    ROLLBACK WORK.
    MESSAGE E009(ZMMM) WITH 'Save Error! . call IT member '.
  ENDIF.

ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_DATA.
*  READ TABLE IT_MODULE INDEX 1.
*
*  DELETE FROM  ZTMM_MODULE WHERE XGJAHR = IT_MODULE-XGJAHR
*                             AND XVERSION = IT_MODULE-XVERSION .
*
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  DELETE_TC_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_TC_LINE.

  LOOP AT IT_100 WHERE MARK = 'X'.
    DELETE IT_100.
    DELETE FROM ZTMM_MOINPUT WHERE KLVAR = IT_100-KLVAR
                          AND MATNR = IT_100-MATNR
                          AND WERKS = IT_100-WERKS.
  ENDLOOP.


ENDFORM.                    " DELETE_TC_LINE
*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0133   text
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME TYPE DYNFNAM
                                      P_OK_CODE.
*>>>>>>BEGIN OF LOCAL DATA<<<<<<<<
  DATA L_TC_NEW_TOP_LINE     TYPE I.
  DATA L_TC_NAME             LIKE FELD-NAME.
  DATA L_TC_LINES_NAME       LIKE FELD-NAME.
  DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
  FIELD-SYMBOLS <LINES>      TYPE I.
*>>>>>>>>END OF LOCAL DATA<<<<<<<<

  ASSIGN (P_TC_NAME) TO <TC>.
* get looplines of TableControl
*   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
*   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


* is no line filled?
*
  IF <TC>-LINES = 0.
*   yes, ...
*
    L_TC_NEW_TOP_LINE = 1.
  ELSE.
*   no, ...
*
    CALL FUNCTION 'SCROLLING_IN_TABLE'
         EXPORTING
              ENTRY_ACT             = <TC>-TOP_LINE
              ENTRY_FROM            = 1
              ENTRY_TO              = <TC>-LINES
*               LAST_PAGE_FULL        = 'X'
              LOOPS                 = WA_LINE
              OK_CODE               = P_OK_CODE
*               OVERLAPPING           = 'X'
         IMPORTING
              ENTRY_NEW             = L_TC_NEW_TOP_LINE
         EXCEPTIONS
              NO_ENTRY_OR_PAGE_ACT  = 01
              NO_ENTRY_TO           = 02
              NO_OK_CODE_OR_PAGE_GO = 03
              OTHERS                = 99.
  ENDIF.


  <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.

** get actual tc and column
**
*   GET CURSOR FIELD L_TC_FIELD_NAME
*              AREA  L_TC_NAME.
*
*   IF SYST-SUBRC = 0.
*     IF L_TC_NAME = P_TC_NAME.
**     set actual column
**
*       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
*     ENDIF.
*   ENDIF.
*
** set the new top line
**

ENDFORM.                    " COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_MODULE_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CHANGED_MODULE_PRICE.
  CLEAR : IT_400[], IT_400, TCOUNT, ECOUNT.
  CTU_PARAMS-DISMODE = 'N'.
  PERFORM GET_MODULE_AND_INFO_RECORD.
  IF IT_400[] IS INITIAL.
    MESSAGE E015(ZMMM) .
  ENDIF.
* TABLE CONTROL
  SORT IT_400 BY MATNR.
  REFRESH CONTROL 'TC_400' FROM SCREEN 400.
  DESCRIBE TABLE IT_400 LINES TC_400-LINES.

ENDFORM.                    " GET_CHANGED_MODULE_PRICE
*&---------------------------------------------------------------------*
*&      Form  GET_MODULE_AND_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MODULE_AND_INFO_RECORD.
  DATA : WA_LINE TYPE I,
         WA_CONT TYPE I.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = 10
            TEXT       = 'DATA READING'(150).

  SELECT A~MATNR A~WERKS A~LIFNR C~HWGES AS KBETR
         A~KADAT A~BIDAT
     INTO CORRESPONDING FIELDS OF  TABLE IT_400
     FROM  ( ( KEKO AS B INNER JOIN CKHS AS C
           ON B~BZOBJ = C~BZOBJ AND
              B~KALNR = C~KALNR AND
              B~KALKA = C~KALKA AND
              B~KADKY = C~KADKY AND
              B~TVERS = C~TVERS AND
              B~BWVAR = C~BWVAR AND
              B~KKZMA = C~KKZMA )
             INNER JOIN    ZTMM_MODULE AS A
          ON A~MATNR = B~MATNR  AND
             A~WERKS = B~WERKS  AND
             A~KADAT = B~KADKY  AND
*             A~KADAT = B~KADAT  AND
*             A~BIDAT = B~BIDAT  AND
             A~KLVAR = B~KLVAR  AND
             A~TVERS = B~TVERS  )
             WHERE B~KKZMA = ' '  .
*          WHERE A~KLVAR = 'ZMOD'.
  DESCRIBE TABLE IT_400 LINES WA_LINE.

* N : not defind price
* U : upper price
* D : down price

  LOOP AT IT_400.
    WA_CONT = WA_CONT + 1.
    IF WA_CONT = 50 .
      CLEAR WA_CONT.
      PERFORM  PROGRESS_BAR USING WA_LINE
                                  SY-TABIX.
    ENDIF.

    PERFORM GET_INFORECORD .

    READ TABLE IT_BAPIEINE INDEX 1.
    IF SY-SUBRC EQ 0.
      IT_400-EKGRP = IT_BAPIEINE-PUR_GROUP.

      IF IT_BAPIEINE-NET_PRICE  = IT_400-KBETR .
        DELETE IT_400.
        CONTINUE.
      ELSEIF IT_BAPIEINE-NET_PRICE  < IT_400-KBETR .
        IT_400-FLAG = 'U'.
        PERFORM CONDITION_COMPONENT_CHECK   .
      ELSEIF IT_BAPIEINE-NET_PRICE  > IT_400-KBETR .
        IT_400-FLAG = 'D'.
        PERFORM CONDITION_COMPONENT_CHECK  .
      ELSE . " unkown
        IT_400-FLAG = 'Z'.
      ENDIF.
    ELSE.
      IT_400-FLAG = 'N'.
    ENDIF.

    PERFORM GET_TEXT_REASON_CODE.

    MODIFY IT_400.

  ENDLOOP.


ENDFORM.                    " GET_MODULE_AND_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  condition_component_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONDITION_COMPONENT_CHECK .


  DATA  : BEGIN OF IT_KNUMH OCCURS 0,
          KNUMH LIKE KONH-KNUMH,
          END OF IT_KNUMH.

  DATA : BEGIN OF IT_UPG OCCURS 0,
         COMP LIKE MARA-MATNR,
         END OF IT_UPG.

  DATA : BEGIN OF IT_COM OCCURS 0,
         COMP LIKE MARA-MATNR,
         END OF IT_COM.

  DATA : WA_COUNT TYPE I.

  CLEAR : IT_COM[], IT_COM ,
          IT_KNUMH[], IT_KNUMH,
          IT_UPG[],  IT_UPG.

* model code
  SELECT COMP  INTO TABLE IT_UPG
         FROM ZTBM_ABXDULDT
         WHERE USAG = '2'
           AND MTNO = IT_400-MATNR   .

* part list
  IF NOT IT_UPG[] IS INITIAL.
    SELECT COMP INTO TABLE IT_COM
           FROM ZTBM_ABXDULDT
           FOR ALL ENTRIES IN IT_UPG
        WHERE USAG = '2'
             AND MTNO = IT_UPG-COMP.
  ENDIF.

*  CHECK SY-SUBRC EQ 0.
  IF NOT IT_COM[] IS INITIAL.

    SELECT  KNUMH INTO TABLE IT_KNUMH
                FROM A018
                FOR ALL ENTRIES IN IT_COM
               WHERE KAPPL EQ C_KAPPL
                 AND KSCHL EQ C_PB00
                 AND MATNR EQ IT_COM-COMP
                 AND EKORG EQ C_EKORG
                 AND DATBI EQ '99991231'.

    IF NOT  IT_KNUMH[] IS INITIAL.
      SELECT COUNT(*) INTO WA_COUNT FROM KONH
             FOR ALL ENTRIES IN IT_KNUMH
             WHERE KNUMH = IT_KNUMH-KNUMH
               AND KZUST LIKE 'X%'.
    ENDIF.
  ENDIF.

  IF WA_COUNT <> 0 .
    IT_400-KZUST = 'ADX'.
  ELSE.
    IT_400-KZUST = 'AD1'.
  ENDIF.

  IF IT_400-FLAG = 'D'.
    IT_400-KZUST+1(1) = 'D'.
  ELSEIF IT_400-FLAG = 'U'.
    IT_400-KZUST+1(1) = 'U'.
  ENDIF.

ENDFORM.                    " condition_component_check
*&---------------------------------------------------------------------*
*&      Form  INFOR_UPDATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INFOR_UPDATE_BDC.

  CLEAR : TCOUNT , ECOUNT.
  PERFORM  CHECK_ENTRY .
  PERFORM  BDC_EXCUTION.

ENDFORM.                    " INFOR_UPDATE_BDC
*&---------------------------------------------------------------------*
*&      Form  bdc_me12_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_ME12_CHANGE.
  DATA : FNAME(60) TYPE C,
         FNAME2(60) TYPE C,
         L_SUBRC LIKE SY-SUBRC .
  DATA : NETPR(14) TYPE C,
         BDATE(10) TYPE C,
         EDATE(10) TYPE C.


*
*  PERFORM  SELECT_INFOR_FOR_BDC CHANGING L_SUBRC  .
*  CHECK L_SUBRC  EQ 0.

  REFRESH: BDC_TAB, MSGTAB.
  CLEAR  : BDC_TAB, MSGTAB.
*  W_MODE = ''.
  WRITE: IT_400-KBETR TO NETPR ,
         IT_400-KADAT TO BDATE ,
         IT_400-BIDAT TO EDATE .


  PERFORM DYNPRO USING:
          'X' 'SAPMM06I'               '0100',
          ' ' 'EINA-LIFNR'             IT_400-LIFNR,
          ' ' 'EINA-MATNR'             IT_400-MATNR,
          ' ' 'EINE-EKORG'             C_EKORG,
          ' ' 'EINE-WERKS'             ' ',

          'X' 'SAPMM06I'               '0101',
          ' ' 'BDC_OKCODE'             '/8',

          'X' 'SAPLV14A'               '0102',
          ' ' 'BDC_OKCODE'             '/7',
          'X' 'SAPMV13A'               '0201',
          ' ' 'RV13A-DATAB'            BDATE,
          ' ' 'RV13A-DATBI'            EDATE,
          ' ' 'KONP-KBETR(1)'           NETPR,
          'X' 'SAPMV13A'               '0201',
          ' ' 'BDC_OKCODE'             'KDAT',
          'X' 'SAPMV13A'               '0200',
          ' ' 'KONH-KZUST'           IT_400-KZUST,
          'X' 'SAPMV13A'               '0200',
          ' ' 'BDC_OKCODE'             '/11'.

  PERFORM DYNPRO USING:
      'X' 'SAPMV13A'               '0201',
      ' ' 'BDC_OKCODE'             'KDAT',
      'X' 'SAPMV13A'               '0200',
      ' ' 'KONH-KZUST'           IT_400-KZUST.


  CALL  TRANSACTION    'ME12'
               USING   BDC_TAB
               MODE    CTU_PARAMS-DISMODE      "'E'
               UPDATE  W_UPDATE    "'S'
            MESSAGES  INTO  MSGTAB.

  IF SY-SUBRC NE 0.
    IT_400-MODE = 'E'.
    MODIFY IT_400.
    ECOUNT = ECOUNT + 1.
  ENDIF.

  PERFORM MESSAGE_CONTROL.

ENDFORM.                    " bdc_me12_change
*&---------------------------------------------------------------------*
*&      Form  bdc_me11_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_ME11_CREATE.
  DATA : FNAME(60) TYPE C,
         FNAME2(60) TYPE C,
         L_SUBRC LIKE SY-SUBRC .
  DATA : NETPR(14) TYPE C,
         BDATE(10) TYPE C,
         EDATE(10) TYPE C.

  REFRESH MSGTAB.
  REFRESH BDC_TAB.

*  W_MODE = ''.
  WRITE: IT_400-KBETR TO NETPR ,
         IT_400-KADAT TO BDATE ,
         IT_400-BIDAT TO EDATE .

  PERFORM DYNPRO USING:
    'X' 'SAPMM06I'               '0100',
    ' ' 'EINA-LIFNR'             IT_400-LIFNR,
    ' ' 'EINA-MATNR'             IT_400-MATNR,
    ' ' 'EINE-EKORG'             C_EKORG,
    ' ' 'EINE-WERKS'             ' ',

    'X' 'SAPMM06I'               '0101',
    ' ' 'BDC_OKCODE'             '=EINE',

    'X' 'SAPMM06I'               '0102',
*          ' ' 'EINE-APLFZ'             '72',
    ' ' 'EINE-EKGRP'             IT_400-EKGRP,
    ' ' 'EINE-NORBM'             '1',
    ' ' 'EINE-NETPR'             NETPR,

    ' ' 'BDC_OKCODE'             '=KO',
    'X' 'SAPMV13A'               '0201',
    ' ' 'RV13A-DATAB'            BDATE,
    ' ' 'RV13A-DATBI'            EDATE,
    ' ' 'KONP-KBETR(1)'          NETPR,
    ' ' 'BDC_OKCODE'             '/11'.



  CALL  TRANSACTION    'ME11'
               USING   BDC_TAB
               MODE    CTU_PARAMS-DISMODE      "'E'
               UPDATE  W_UPDATE    "'S'
            MESSAGES  INTO  MSGTAB.

  IF SY-SUBRC NE 0.
    IT_400-MODE = 'E'.
    MODIFY IT_400.
    ECOUNT = ECOUNT + 1.
  ENDIF.

  PERFORM MESSAGE_CONTROL.

ENDFORM.                    " bdc_me11_create
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0957   text
*      -->P_0958   text
*      -->P_0959   text
*----------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE: NAME  TO BDC_TAB-PROGRAM,
          VALUE TO BDC_TAB-DYNPRO,
          'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE: NAME  TO BDC_TAB-FNAM,
          VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  select_infor_for_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_INFOR_FOR_BDC CHANGING P_SUBRC LIKE SY-SUBRC.

  SELECT SINGLE KNUMH INTO KONP-KNUMH
    FROM A018
    WHERE KAPPL = C_KAPPL
*      and KSCHL = c_KSCHL
      AND LIFNR = IT_400-LIFNR
      AND MATNR = IT_400-MATNR
      AND EKORG = C_EKORG
      AND DATBI = '99991231'.
  SELECT KOPOS KSCHL KBETR
         INTO CORRESPONDING FIELDS OF TABLE IT_KONP
         FROM KONP
         WHERE KNUMH = KONP-KNUMH.

  P_SUBRC = SY-SUBRC .

ENDFORM.                    " select_infor_for_bdc
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MESSAGE_LOG.

  READ TABLE IT_400  WITH KEY MARK = 'X'.
  IF SY-SUBRC EQ 0 .
    READ TABLE IT_ER WITH KEY MATNR = IT_400-MATNR.
    IF SY-SUBRC EQ 0.


      CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
           EXPORTING
                TITEL        = 'Module Cost'
                TEXTLINE1    = IT_400-MATNR
                TEXTLINE2    = IT_ER-TEXT
                START_COLUMN = 25
                START_ROW    = 6.
    ELSE .
      MESSAGE I009(ZMMM) WITH 'Dont Error '.
    ENDIF .
  ELSE.
    MESSAGE I001(ZMMM).
  ENDIF.
ENDFORM.                    " MESSAGE_LOG
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MESSAGE_CONTROL.
  DATA:  L_MSTRING(480).

  LOOP AT MSGTAB WHERE MSGTYP = 'E'.
    SELECT SINGLE * FROM T100 WHERE SPRSL = MSGTAB-MSGSPRA
                              AND   ARBGB = MSGTAB-MSGID
                              AND   MSGNR = MSGTAB-MSGNR.
    IF SY-SUBRC = 0.
      L_MSTRING = T100-TEXT.
      IF L_MSTRING CS '&1'.
        REPLACE '&1' WITH MSGTAB-MSGV1 INTO L_MSTRING.
        REPLACE '&2' WITH MSGTAB-MSGV2 INTO L_MSTRING.
        REPLACE '&3' WITH MSGTAB-MSGV3 INTO L_MSTRING.
        REPLACE '&4' WITH MSGTAB-MSGV4 INTO L_MSTRING.
      ELSE.
        REPLACE '&' WITH MSGTAB-MSGV1 INTO L_MSTRING.
        REPLACE '&' WITH MSGTAB-MSGV2 INTO L_MSTRING.
        REPLACE '&' WITH MSGTAB-MSGV3 INTO L_MSTRING.
        REPLACE '&' WITH MSGTAB-MSGV4 INTO L_MSTRING.
      ENDIF.
      CONDENSE L_MSTRING.
*      WRITE: / MSGTAB-MSGTYP, L_MSTRING(250).

      MOVE: IT_400-MATNR TO IT_ER-MATNR ,
            L_MSTRING    TO IT_ER-TEXT.

    ELSE.
      MOVE: IT_400-MATNR TO IT_ER-MATNR ,
            'ERROR'      TO IT_ER-TEXT.

    ENDIF.
    APPEND IT_ER .
  ENDLOOP.

ENDFORM.                    " MESSAGE_CONTROL
*&---------------------------------------------------------------------*
*&      Form  get_text_reason_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TEXT_REASON_CODE.
  SELECT SINGLE VTEXT INTO IT_400-VTEXT
       FROM T686D
       WHERE SPRAS = SY-LANGU
         AND KZUST = IT_400-KZUST.

ENDFORM.                    " get_text_reason_code
*&---------------------------------------------------------------------*
*&      Form  get_inforecord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_INFORECORD.
  CLEAR : IT_BAPIEINE[], IT_RETURN[].
  CLEAR : IT_BAPIEINE, IT_RETURN.

  CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
       EXPORTING
            VENDOR              = IT_400-LIFNR
            MATERIAL            = IT_400-MATNR
            PURCH_ORG           = 'PU01'
            PLANT               = 'P001'  " it_400-werks
            PURCHORG_DATA       = 'X'
            GENERAL_DATA        = ''
       TABLES
            INFORECORD_PURCHORG = IT_BAPIEINE
            RETURN              = IT_RETURN.

ENDFORM.                    " get_inforecord
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ENTRY.

  READ TABLE IT_400 WITH KEY EKGRP = ' '
                             FLAG  = 'N'.
  IF SY-SUBRC EQ 0.
    MESSAGE E009(ZMMM) WITH 'Input Purchasing group '.
  ENDIF.


ENDFORM.                    " CHECK_ENTRY
*&---------------------------------------------------------------------*
*&      Form  BDC_EXCUTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_EXCUTION.
  LOOP AT IT_400.
    IF IT_400-FLAG = 'N'.
      PERFORM BDC_ME11_CREATE .
    ELSE.
      PERFORM BDC_ME12_CHANGE .
    ENDIF.
    TCOUNT = TCOUNT + 1.
  ENDLOOP.

ENDFORM.                    " BDC_EXCUTION
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROGRESS_BAR USING P_LINE TYPE I
                        P_CLINE TYPE I .
  DATA : WA_PERCENT TYPE I,
         WA_T       TYPE P,
         WA_C       TYPE P.
  WA_T = P_LINE.
  WA_C = P_CLINE.

  WA_PERCENT =   WA_C / WA_T * 100 .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = WA_PERCENT
            TEXT       = 'Inforecord reading'(150).

ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  DATA_SAVE_SCREEN_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SAVE_SCREEN_100.

  DATA : IT_INPUT LIKE ZTMM_MOINPUT OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_100.
    MOVE-CORRESPONDING IT_100 TO IT_INPUT.
    APPEND IT_INPUT . CLEAR IT_INPUT.

  ENDLOOP.

  MODIFY ZTMM_MOINPUT FROM TABLE IT_INPUT.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE S009(ZMMM) WITH 'DATA SAVED'.
  ENDIF.



ENDFORM.                    " DATA_SAVE_SCREEN_100
*&---------------------------------------------------------------------*
*&      Form  INITAL_VALUE_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITAL_VALUE_100.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_100
    FROM  ZTMM_MOINPUT.

ENDFORM.                    " INITAL_VALUE_100
*&---------------------------------------------------------------------*
*&      Form  DOWN_LOAD_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWN_LOAD_EXCEL.
  DATA : WA_FILENAME    LIKE RLGRAP-FILENAME .
  DATA : BEGIN OF IT_DOWN200 OCCURS 0 ,
              KLVAR(20),
              MATNR(20),
              WERKS(20),
              LOSGR(20),
              TVERS(20),
              KADAT(20),
              BIDAT(20),
              RAW_MATERIAL(20),
              TYPPS(20),
              KSTAR(20),
              MENGE(20),
              LPREIS(20),
              LPREIFX(20),
              LPEINH(20),
         END OF IT_DOWN200.

  CLEAR: IT_DOWN200, IT_DOWN200[].

  LOOP AT IT_200.
    MOVE-CORRESPONDING IT_200 TO IT_DOWN200.
    APPEND IT_DOWN200 . CLEAR IT_DOWN200.

  ENDLOOP.

  IT_DOWN200-KLVAR = 'Costing Variant'.
  IT_DOWN200-MATNR = 'Material number'.
  IT_DOWN200-WERKS = 'Plant'.
  IT_DOWN200-LOSGR = 'Costing lot size'.
  IT_DOWN200-TVERS = 'Costing version'.
  IT_DOWN200-KADAT = 'Costing date from'.
  IT_DOWN200-BIDAT = 'Costing date to'.
  IT_DOWN200-RAW_MATERIAL = 'Material component'.
  IT_DOWN200-TYPPS = 'Item category'.
  IT_DOWN200-KSTAR = 'Cost element'.
  IT_DOWN200-MENGE = 'Qty'.
  IT_DOWN200-LPREIS = 'Price in Entry Currency'.
  IT_DOWN200-LPREIFX = 'Fixed price in Entry Currency'.
  IT_DOWN200-LPEINH = 'Price unit'.

  INSERT IT_DOWN200 INDEX 1.

  IF WA_FILENAME IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD WA_FILENAME.
    IF SY-SUBRC NE 0.CLEAR  WA_FILENAME.ENDIF.
  ENDIF.

* file path
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME = WA_FILENAME
            DEF_PATH     = WA_FILENAME
            MASK         = ',*.xls.'
            MODE         = 'S'
            TITLE        = SY-TITLE
       IMPORTING
            FILENAME     = WA_FILENAME.

* file down load
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = WA_FILENAME
            FILETYPE = 'DAT'
       TABLES
            DATA_TAB = IT_DOWN200.


* excel run
  CALL FUNCTION 'WS_EXCEL'
       TABLES
            DATA = IT_DOWN200.




ENDFORM.                    " DOWN_LOAD_EXCEL
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DOWNLOAD_400
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCEL_DOWNLOAD_400.
  DATA : WA_FILENAME    LIKE RLGRAP-FILENAME .
  DATA : BEGIN OF IT_DOWN400 OCCURS 0 ,
         MATNR(40),
         LIFNR(40),
         KBETR(40),
         KZUST(40),
         VTEXT(40),
         KADAT(40),
         BIDAT(40),
         EKGRP(40),
         END OF IT_DOWN400.

  CLEAR: IT_DOWN400, IT_DOWN400[].

  LOOP AT IT_400.
    MOVE-CORRESPONDING IT_400 TO IT_DOWN400.
    APPEND IT_DOWN400 . CLEAR IT_DOWN400.

  ENDLOOP.

  IT_DOWN400-MATNR = 'Material number'.
  IT_DOWN400-LIFNR = 'Suplier'.
  IT_DOWN400-KBETR = 'Module Price'.
  IT_DOWN400-KZUST = 'Reason Code'.
  IT_DOWN400-KADAT = 'Costing date from'.
  IT_DOWN400-BIDAT = 'Costing date to'.
  IT_DOWN400-VTEXT = 'Text'.
  IT_DOWN400-EKGRP = 'Purch. group'.

  INSERT IT_DOWN400 INDEX 1.

  IF WA_FILENAME IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD WA_FILENAME.
    IF SY-SUBRC NE 0.CLEAR  WA_FILENAME.ENDIF.
  ENDIF.

* file path
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME = WA_FILENAME
            DEF_PATH     = WA_FILENAME
            MASK         = ',*.xls.'
            MODE         = 'S'
            TITLE        = SY-TITLE
       IMPORTING
            FILENAME     = WA_FILENAME.

* file down load
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = WA_FILENAME
            FILETYPE = 'DAT'
       TABLES
            DATA_TAB = IT_DOWN400.


* excel run
  CALL FUNCTION 'WS_EXCEL'
       TABLES
            DATA = IT_DOWN400.

ENDFORM.                    " EXCEL_DOWNLOAD_400
