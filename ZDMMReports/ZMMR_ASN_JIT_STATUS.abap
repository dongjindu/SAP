************************************************************************
* Program Name      : ZMMR_ASN_JIT_STATUS
* Author            : Furong Wang
* Creation Date     : 12/2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZMMR_ASN_JIT_STATUS MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TABLES: MARA, EKPO, EKET, EDIDC, EKES.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

CONSTANTS: C_GREEN  TYPE ICONSHORT VALUE '@5B@',
           C_RED    TYPE ICONSHORT VALUE '@5C@',
           C_YELLOW TYPE ICONSHORT VALUE '@5D@'.

*DATA: BEGIN OF it_data OCCURS 0,
*      ebeln LIKE ekko-ebeln,
*      ebelp LIKE ekpo-ebelp,
*      lifnr LIKE ekko-lifnr,
*      sortl LIKE lfa1-sortl,
*      matnr LIKE ekpo-matnr,
*      werks LIKE ekpo-werks,
*      lgort LIKE ekpo-lgort,
*      lgbzo LIKE ekpo-lgbzo,
*      dispo LIKE marc-dispo,
*      menge LIKE eket-menge,
*      wemng LIKE eket-wemng,
*      eindt LIKE eket-eindt,
*      uzeit LIKE sy-uzeit,
*      wemng_862 LIKE eket-wemng,
*      wemng_856 LIKE eket-wemng,
*      open_qty LIKE eket-wemng,
*      docnum LIKE edidc-docnum,
*      message(40),
*      light_jit(10),
*      light_asn(10),
*      color_line(4) TYPE c,
*      color TYPE slis_t_specialcol_alv,
*      END OF it_data.

DATA: BEGIN OF IT_DATA OCCURS 0.
        INCLUDE STRUCTURE ZTMM_DMS.
DATA: LIGHT_JIT(10),
      LIGHT_ASN(10),
      COLOR_LINE(4) TYPE C,
      COLOR TYPE SLIS_T_SPECIALCOL_ALV,
      END OF IT_DATA.

** ALV
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      IT_EXCLUDE TYPE UI_FUNCTIONS.

DATA: G_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER.

DATA: ALV_GRID         TYPE REF TO CL_GUI_ALV_GRID.

DATA: G_LIGHTS_FIELDNAME  TYPE SLIS_FIELDNAME VALUE 'LIGHTS'.

DATA: OK_CODE LIKE SY-UCOMM,
      W_CODE LIKE SY-UCOMM,
      W_OLD_CODE LIKE SY-UCOMM,
      W_CNT   TYPE   I,
      W_REPID LIKE SY-REPID,
      W_DYNNR LIKE SY-DYNNR,
*      W_TIME LIKE SY-UZEIT,
      W_NEXT_DATE LIKE SY-DATUM,
      W_DATE LIKE SY-DATUM.

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER DEFINITION INHERITING FROM CL_GUI_CONTROL.
  PUBLIC SECTION.
    CONSTANTS:  EVENTID_FINISHED TYPE I VALUE 1 .
    CLASS-DATA: INTERVAL TYPE I VALUE '0'.
    EVENTS:     FINISHED .
    METHODS:
*             show_alv,
             CANCEL
                  EXCEPTIONS
                     ERROR,
             CONSTRUCTOR
                 IMPORTING
                     LIFETIME TYPE I OPTIONAL
                     VALUE(SHELLSTYLE) TYPE I OPTIONAL
                     VALUE(PARENT) TYPE REF TO CL_GUI_CONTAINER OPTIONAL
                 EXCEPTIONS
                     ERROR,
             RUN
                 EXCEPTIONS
                     ERROR,
             DISPATCH REDEFINITION.
ENDCLASS.                    "lcl_gui_timer DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
                ON_FINISHED
                       FOR EVENT FINISHED OF LCL_GUI_TIMER.

ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: TIMER_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GUI_TIMER TYPE REF TO LCL_GUI_TIMER,
      EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER,
      TIMEOUT_INTERVAL TYPE I.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD ON_FINISHED.
* Start Timer again
    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.
* cause PAI
    CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
      EXPORTING
        NEW_CODE = 'REFR'.
  ENDMETHOD.                    "on_finished
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_GUI_TIMER IMPLEMENTATION.
  METHOD CONSTRUCTOR.
    TYPE-POOLS: SFES.
    DATA CLSID(80).
    DATA EVENT_TAB TYPE CNTL_SIMPLE_EVENTS.
    DATA EVENT_TAB_LINE TYPE CNTL_SIMPLE_EVENT.

    IF CLSID IS INITIAL.
      DATA: RETURN,
            GUITYPE TYPE I.
      GUITYPE = 0.
      CALL FUNCTION 'GUI_HAS_OBJECTS'
        EXPORTING
          OBJECT_MODEL = SFES_OBJ_ACTIVEX
        IMPORTING
          RETURN       = RETURN
        EXCEPTIONS
          OTHERS       = 1.
      IF SY-SUBRC NE 0.
        RAISE ERROR.
      ENDIF.

      IF RETURN = 'X'.
        GUITYPE = 1.
      ENDIF.
      IF GUITYPE = 0.
        CALL FUNCTION 'GUI_HAS_OBJECTS'
          EXPORTING
            OBJECT_MODEL = SFES_OBJ_JAVABEANS
          IMPORTING
            RETURN       = RETURN
          EXCEPTIONS
            OTHERS       = 1.
        IF SY-SUBRC NE 0.
          RAISE ERROR.
        ENDIF.

        IF RETURN = 'X'.
          GUITYPE = 2.
        ENDIF.
      ENDIF.

      CASE GUITYPE.
        WHEN 1.
          CLSID = 'Sapgui.InfoCtrl.1'.
        WHEN 2.
          CLSID = 'com.sap.components.controls.sapImage.SapImage'.
      ENDCASE.
    ENDIF.

    CALL METHOD SUPER->CONSTRUCTOR
      EXPORTING
        CLSID      = CLSID
        SHELLSTYLE = 0
        PARENT     = CL_GUI_CONTAINER=>DEFAULT_SCREEN
        AUTOALIGN  = SPACE
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

    CALL METHOD CL_GUI_CFW=>SUBSCRIBE
      EXPORTING
        SHELLID = H_CONTROL-SHELLID
        REF     = ME
      EXCEPTIONS
        OTHERS  = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.

* Register the events
    EVENT_TAB_LINE-EVENTID = LCL_GUI_TIMER=>EVENTID_FINISHED.
    APPEND EVENT_TAB_LINE TO EVENT_TAB.

    CALL METHOD SET_REGISTERED_EVENTS
      EXPORTING
        EVENTS = EVENT_TAB.

  ENDMETHOD.                    "constructor

  METHOD CANCEL.
    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = -1
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.
  ENDMETHOD.                    "cancel

  METHOD RUN.
    CALL METHOD CALL_METHOD
      EXPORTING
        METHOD     = 'SetTimer'
        P_COUNT    = 1
        P1         = INTERVAL
        QUEUE_ONLY = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF SY-SUBRC NE 0.
      RAISE ERROR.
    ENDIF.
  ENDMETHOD.                    "run

  METHOD DISPATCH .
    CASE EVENTID.
      WHEN EVENTID_FINISHED.
        RAISE EVENT FINISHED.
    ENDCASE.

    CLEAR TIMER_CONTAINER.
  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_WERKS LIKE MARC-WERKS DEFAULT 'P001',
            P_DATE LIKE EKET-EINDT.   "OBLIGATORY DEFAULT SY-DATUM.
*SELECT-OPTIONS: S_TIME FOR SY-UZEIT,
SELECT-OPTIONS:  S_LGBZO FOR EKPO-LGBZO.
*                 S_DOCNUM FOR EDIDC-DOCNUM NO-DISPLAY.
PARAMETERS: P_RCVPRN LIKE LFA1-LIFNR. " EDIDC-RCVPRN DEFAULT 'UD1300'.
*P_MSTYP LIKE EDIDC-MESTYP OBLIGATORY ,
*             P_DIRECT LIKE EDIDC-DIRECT DEFAULT '2',
*             P_SNDPRN LIKE lfa1-lifnr.  "EDIDC-SNDPRN.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK6 WITH FRAME.
PARAMETERS :
    P_INTRV TYPE I DEFAULT '10'.
SELECTION-SCREEN END OF BLOCK BLOCK6.


*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM INIT_DATA.

AT SELECTION-SCREEN OUTPUT.
*  if sy-tcode = 'ZMMR00010A'
  IF SY-TCODE <> 'ZMMR00010'  AND
     SY-UNAME <> 'HIS20043' AND SY-UNAME <> 'HIS20037'.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'P_DATE'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM GET_DATA.
  IF SY-TCODE = 'ZMMR00010'.
    CALL SCREEN 0200.
  ELSE.
** Furong on 10/28/11: performance {
    check not it_data[] is initial.
** } on 10/28/11
    PERFORM SAVE_DATA_FOR_BW.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog' P_GUBUN P_FIELD.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME .
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*&---------------------------------------------------------------------*
*&      Form  GET_VIN_SCHUDLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  DATA : LT_EDIDC LIKE TABLE OF EDIDC WITH HEADER LINE,
         LT_EDIDD LIKE TABLE OF EDIDD WITH HEADER LINE.

  DATA: BEGIN OF LT_TEMP OCCURS 0.
          INCLUDE STRUCTURE ZTMM_DMS.
  DATA:  LIGHT_JIT(10),
         LIGHT_ASN(10),
         END OF LT_TEMP.

  DATA: BEGIN OF LT_TEMP_0 OCCURS 0,
      EBELN LIKE EKKO-EBELN,
      EBELP LIKE EKPO-EBELP,
      LIFNR LIKE EKKO-LIFNR,
      SORTL LIKE LFA1-SORTL,
      MATNR LIKE EKPO-MATNR,
      WERKS LIKE EKPO-WERKS,
      LGORT LIKE EKPO-LGORT,
      LGBZO LIKE EKPO-LGBZO,
      EINDT LIKE EKET-EINDT,
      MENGE LIKE EKET-MENGE,
      WEMNG LIKE EKET-WEMNG,
      END OF LT_TEMP_0.

  DATA: BEGIN OF LT_EKEK OCCURS 0,
        EBELN LIKE EKKO-EBELN,
        EBELP LIKE EKPO-EBELP,
        INIDT LIKE EKEK-INIDT,
        INITM LIKE EKEK-INITM,
        ABART LIKE EKEK-ABART,
        ABRUF LIKE EKEK-ABRUF,
        DISPO LIKE EKEK-DISPO,
        END OF LT_EKEK.

  DATA: BEGIN OF LT_TIME OCCURS 0,
        EINDT LIKE IT_DATA-EINDT,
        UZEIT LIKE IT_DATA-UZEIT,
        WEMNG_862 LIKE IT_DATA-WEMNG,
        DATE_TIME(14),
        STATUS(20),
        END OF LT_TIME.

  DATA: BEGIN OF LT_LIPS OCCURS 0,
         EBELN LIKE EKPO-EBELN,
         EBELP LIKE EKPO-EBELP,
         ERDAT LIKE LIKP-ERDAT,
         ERZET LIKE LIKP-ERZET,
         LFIMG LIKE LIPS-LFIMG,
         VBELN LIKE MKPF-XBLNR,
         DATE_TIME(14),
         END OF LT_LIPS.

  DATA: BEGIN OF LT_MSEG OCCURS 0,
          EBELN LIKE EKPO-EBELN,
          EBELP LIKE EKPO-EBELP,
          CPUDT LIKE MKPF-CPUDT,
          CPUTM LIKE MKPF-CPUTM,
          MENGE LIKE MSEG-MENGE,
          XBLNR LIKE MKPF-XBLNR,
          DATE_TIME(14),
          PROCESSED(1),
          DELJIT LIKE MSEG-MENGE,
          END OF LT_MSEG.

  DATA: WS_TIME LIKE LT_TIME.

  DATA: WA_E1EDK09 LIKE E1EDK09,
        WA_E1EDP10 LIKE E1EDP10,
        WA_E1EDP16 LIKE E1EDP16.

  DATA: L_EBELN LIKE EKKO-EBELN,
        L_MATNR LIKE EKPO-MATNR,
        L_NUMC LIKE EKEK-ABRUF,
        L_OBJKY LIKE NAST-OBJKY,
*        L_DOCNUM LIKE EDIDC-DOCNUM.
        L_DOCNUM LIKE CMFP-MSGV1,
        L_DATE LIKE SY-DATUM,
        L_CURR_DTIME(14),
*        L_DATE_TIME(14),
        L_MENGE LIKE MSEG-MENGE,
*        L_DIFF LIKE SY-UZEIT,
*        L_DIFF_CURRENT LIKE SY-UZEIT,
        l_current_time(14),
        l_del_time(14),
        L_MINUTES TYPE I,
        L_ASN LIKE MSEG-MENGE,
        L_DELJIT LIKE MSEG-MENGE,
        L_COL(1),
        L_COUNT TYPE I,
        L_EDATUB LIKE E1EDP16-EDATUB,
        L_TRAILER LIKE EKES-XBLNR,
        L_ETENS LIKE EKES-ETENS,
        L_INDEX LIKE SY-TABIX,
        L_DIFF_QTY LIKE MSEG-MENGE,
        L_SUM_QTY LIKE MSEG-MENGE,
        L_TIME LIKE SY-UZEIT,
        L_TODAY_DATE LIKE SY-DATUM.

  RANGES: R_DATE FOR SY-DATUM.

  TIMEOUT_INTERVAL = P_INTRV * 60.
*  if sy-tcode = 'ZMMR00010A'.
  IF SY-TCODE <> 'ZMMR00010'
     AND SY-UNAME <> 'HIS20043'
     AND SY-UNAME <> 'HIS20037'.
    IF SY-UZEIT >= '00:06:30'.
      W_DATE = SY-DATUM.
    ELSE.
      W_DATE = SY-DATUM - 1.
    ENDIF.
  ELSE.
    W_DATE = P_DATE.
  ENDIF.

  L_TIME = SY-UZEIT.
  L_TODAY_DATE = SY-DATUM.

  W_NEXT_DATE = W_DATE + 1.
  CONCATENATE SY-DATUM SY-UZEIT INTO L_CURR_DTIME.

** Get PO & Delivery
  IF P_RCVPRN IS INITIAL.
    SELECT A~EBELN B~EBELP A~LIFNR SORTL MATNR B~WERKS
           LGORT LGBZO EINDT  SUM( D~MENGE ) SUM( D~WEMNG )
*    INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
     INTO TABLE LT_TEMP_0
    FROM EKKO AS A
     INNER JOIN EKPO AS B
      ON A~EBELN = B~EBELN
    INNER JOIN LFA1 AS C
     ON A~LIFNR = C~LIFNR
   INNER JOIN EKET AS D
     ON A~EBELN = D~EBELN
     AND B~EBELP = D~EBELP
   WHERE A~BSTYP = 'L'
     AND BSART = 'JIT'
     AND A~LOEKZ = ' '
     AND B~WERKS = P_WERKS
     AND B~LOEKZ = ' '
     AND LGBZO IN S_LGBZO
    AND D~EINDT = W_DATE
    AND D~MENGE > 0
    GROUP BY A~EBELN B~EBELP A~LIFNR SORTL MATNR B~WERKS
      LGORT LGBZO EINDT.
  ELSE.
    SELECT A~EBELN B~EBELP A~LIFNR SORTL MATNR B~WERKS
           LGORT LGBZO EINDT SUM( D~MENGE ) SUM( D~WEMNG )
      INTO TABLE LT_TEMP_0
       FROM EKKO AS A
       INNER JOIN EKPO AS B
        ON A~EBELN = B~EBELN
       INNER JOIN LFA1 AS C
      ON A~LIFNR = C~LIFNR
     INNER JOIN EKET AS D
       ON A~EBELN = D~EBELN
       AND B~EBELP = D~EBELP
     WHERE A~BSTYP = 'L'
       AND BSART = 'JIT'
       AND A~LOEKZ = ' '
       AND A~LIFNR = P_RCVPRN
       AND B~WERKS = P_WERKS
       AND B~LOEKZ = ' '
       AND LGBZO IN S_LGBZO
      AND D~EINDT = W_DATE
      AND D~MENGE > 0
      GROUP BY A~EBELN B~EBELP A~LIFNR SORTL MATNR B~WERKS
           LGORT LGBZO EINDT.
  ENDIF.
*

* IF P_RCVPRN IS INITIAL.
*    SELECT A~EBELN B~EBELP A~LIFNR SORTL MATNR B~WERKS
*           LGORT LGBZO EINDT D~MENGE D~WEMNG
*    INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
*    FROM EKKO AS A
*     INNER JOIN EKPO AS B
*      ON A~EBELN = B~EBELN
*    INNER JOIN LFA1 AS C
*     ON A~LIFNR = C~LIFNR
*   INNER JOIN EKET AS D
*     ON A~EBELN = D~EBELN
*     AND B~EBELP = D~EBELP
*   WHERE A~BSTYP = 'L'
*     AND BSART = 'JIT'
*     AND A~LOEKZ = ' '
*     AND B~WERKS = P_WERKS
*     AND B~LOEKZ = ' '
*     AND LGBZO IN S_LGBZO
*    AND D~EINDT = P_DATE
*    AND D~MENGE > 0.
*  ELSE.
*    SELECT A~EBELN B~EBELP A~LIFNR SORTL MATNR B~WERKS
*           LGORT LGBZO EINDT D~MENGE D~WEMNG
*      INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
*       FROM EKKO AS A
*       INNER JOIN EKPO AS B
*        ON A~EBELN = B~EBELN
*       INNER JOIN LFA1 AS C
*      ON A~LIFNR = C~LIFNR
*     INNER JOIN EKET AS D
*       ON A~EBELN = D~EBELN
*       AND B~EBELP = D~EBELP
*     WHERE A~BSTYP = 'L'
*       AND BSART = 'JIT'
*       AND A~LOEKZ = ' '
*       AND A~LIFNR = P_RCVPRN
*       AND B~WERKS = P_WERKS
*       AND B~LOEKZ = ' '
*       AND LGBZO IN S_LGBZO
*      AND D~EINDT = P_DATE
*      AND D~MENGE > 0.
*  ENDIF.
*

  LOOP AT LT_TEMP_0.
    MOVE-CORRESPONDING LT_TEMP_0 TO LT_TEMP.
    APPEND LT_TEMP.
  ENDLOOP.

** Furong on 10/28/11: performance
  IF LT_TEMP[] is not initial.
** End on 10/28/11

    SORT LT_TEMP BY EBELN EBELP LIFNR MATNR.
*    delete adjacent duplicates from lt_temp comparing ebeln ebelp lifnr
*  matnr.

    SELECT EBELN EBELP INIDT INITM ABART ABRUF DISPO
      INTO TABLE LT_EKEK
      FROM EKEK
      FOR ALL ENTRIES IN LT_TEMP
      WHERE EBELN = LT_TEMP-EBELN
        AND EBELP = LT_TEMP-EBELP.

    SORT LT_EKEK BY EBELN EBELP INIDT INITM DESCENDING.
  ENDIF.

** getting mrp controller and idoc number
  LOOP AT LT_TEMP.

    READ TABLE LT_EKEK WITH KEY EBELN = LT_TEMP-EBELN
                                EBELP = LT_TEMP-EBELP
                                INIDT = LT_TEMP-EINDT.
    IF SY-SUBRC = 0.
      L_NUMC = LT_EKEK-EBELN.
      IF NOT LT_EKEK-DISPO IS INITIAL.
        LT_TEMP-DISPO = LT_EKEK-DISPO.
      ENDIF.
      CLEAR: L_OBJKY, L_DOCNUM.
      CONCATENATE L_NUMC LT_EKEK-EBELP LT_EKEK-ABART LT_EKEK-ABRUF
              INTO L_OBJKY.
      SELECT SINGLE MSGV1 INTO L_DOCNUM
        FROM NAST AS A
        INNER JOIN CMFP AS B
        ON A~CMFPNR = B~NR
        WHERE KAPPL = 'EL'
          AND OBJKY = L_OBJKY
          AND KSCHL = 'LPJ1'
          AND APLID = 'WFMC'
*         AND ARBGB = 'E0'
         AND MSGNR = '045'.
      IF SY-SUBRC = 0.
        LT_TEMP-DOCNUM = L_DOCNUM.
      ENDIF.
    ENDIF.
    IF LT_TEMP-DISPO IS INITIAL.
      SELECT SINGLE DISPO INTO LT_TEMP-DISPO
        FROM MARC
        WHERE WERKS = 'P001'
         AND MATNR = LT_TEMP-MATNR.
    ENDIF.

    LT_TEMP-OPEN_QTY = LT_TEMP-MENGE - LT_TEMP-WEMNG.

    CLEAR: L_TRAILER, L_ETENS.
    SELECT SINGLE MAX( ETENS ) XBLNR INTO (L_ETENS,
      L_TRAILER)
      FROM EKES
      WHERE EBELN = LT_TEMP-EBELN
       AND EBELP = LT_TEMP-EBELP
** changed on 02/25/11
       AND EINDT = P_DATE
** end of change
       AND DABMG = 0
       GROUP BY EBELN EBELP XBLNR.
    IF SY-SUBRC = 0.
      LT_TEMP-TRAILER = L_TRAILER.
    ENDIF.
    MODIFY LT_TEMP.
    CLEAR: LT_TEMP.
  ENDLOOP.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_EDIDC
*   FROM EDIDC
*   WHERE MESTYP EQ P_MSTYP
*     AND DOCNUM IN S_DOCNUM
*     AND CREDAT IN S_DATE
*     AND RCVPRT EQ 'LS'
*     AND RCVPRN EQ P_RCVPRN.
*    AND SNDPRN EQ P_SNDPRN
*     AND DIRECT EQ P_DIRECT.

  LOOP AT LT_TEMP.

    MOVE-CORRESPONDING LT_TEMP TO IT_DATA.
    REFRESH: LT_EDIDD, LT_TIME.
    CLEAR: LT_EDIDD, WA_E1EDK09, WA_E1EDP10.

    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        DOCUMENT_NUMBER         = LT_TEMP-DOCNUM
      TABLES
        INT_EDIDD               = LT_EDIDD
      EXCEPTIONS
        DOCUMENT_NOT_EXIST      = 1
        DOCUMENT_NUMBER_INVALID = 2
        OTHERS                  = 3.
    IF SY-SUBRC = 0.
*      READ TABLE LT_EDIDD WITH KEY SEGNAM = 'E1EDK09'.
*      WA_E1EDK09 = LT_EDIDD-SDATA.
*      L_EBELN = WA_E1EDK09-VTRNR.
*      READ TABLE LT_EDIDD WITH KEY SEGNAM = 'E1EDP10'.
*      WA_E1EDP10 = LT_EDIDD-SDATA.
*      L_MATNR = WA_E1EDP10-IDNKD.

      REFRESH: R_DATE.
      REFRESH: LT_TIME.
      LOOP AT LT_EDIDD WHERE SEGNAM = 'E1EDP16'.
        WA_E1EDP16 = LT_EDIDD-SDATA.
        LT_TIME-EINDT = WA_E1EDP16-EDATUV.
        L_EDATUB = WA_E1EDP16-EDATUB.
        IF L_EDATUB <> W_DATE.
          CONTINUE.
        ENDIF.
        LT_TIME-UZEIT = WA_E1EDP16-EZEIT.
        LT_TIME-WEMNG_862 = WA_E1EDP16-WMENG.
        CONCATENATE  LT_TIME-EINDT LT_TIME-UZEIT INTO LT_TIME-DATE_TIME.
        APPEND LT_TIME.

        R_DATE-OPTION = 'EQ'.
        R_DATE-SIGN = 'I'.
        R_DATE-LOW = WA_E1EDP16-EDATUV.
        COLLECT R_DATE.
      ENDLOOP.

      DESCRIBE TABLE LT_TIME LINES L_COUNT.

      IF L_COUNT > 0.

** getting ASN
        REFRESH: LT_LIPS, LT_MSEG.
        SELECT VGBEL AS EBELN VGPOS AS EBELP A~ERDAT A~ERZET
               LFIMG A~VBELN INTO TABLE LT_LIPS
          FROM LIKP AS A
          INNER JOIN LIPS AS B
               ON A~VBELN = B~VBELN
          WHERE VGBEL = LT_TEMP-EBELN
            AND VGPOS = LT_TEMP-EBELP
            AND A~ERDAT IN R_DATE.

** Changed by Furong on 03/03/11
*        IF SY-SUBRC = 0.
*          SELECT EBELN EBELP CPUDT CPUTM MENGE XBLNR
*           INTO TABLE LT_MSEG
*           FROM MKPF AS A
*           INNER JOIN MSEG AS B
*           ON A~MBLNR = B~MBLNR
*           FOR ALL ENTRIES IN LT_LIPS
*           WHERE EBELN = LT_LIPS-EBELN
*             AND XBLNR = LT_LIPS-VBELN.
**           AND EBELP = LT_LIPS-EBELP.
*        ENDIF.
*
*        LOOP AT LT_MSEG.
**        L_DATE_C = LT_MSEG-CPUDT.
**        L_TIME_C = LT_MSEG-CPUTM.
**        CONCATENATE L_DATE_C L_TIME_C INTO LT_MSEG-DATE_TIME.
*         CONCATENATE LT_MSEG-CPUDT LT_MSEG-CPUTM INTO LT_MSEG-DATE_TIME
*.
*          MODIFY LT_MSEG.
*        ENDLOOP.
** End of change on 03/03/11

        LOOP AT LT_LIPS.
*        L_DATE_C = LT_LIPS-ERDAT.
*        L_TIME_C = LT_LIPS-ERZET.
*        CONCATENATE L_DATE_C L_TIME_C INTO LT_LIPS-DATE_TIME.
         CONCATENATE LT_LIPS-ERDAT LT_LIPS-ERZET INTO LT_LIPS-DATE_TIME.
          MODIFY LT_LIPS.
        ENDLOOP.

        SORT LT_LIPS BY DATE_TIME.
        SORT LT_MSEG BY DATE_TIME.
        SORT LT_TIME BY DATE_TIME.

        CLEAR: L_ASN, L_DELJIT.
        LOOP AT LT_LIPS.
          L_ASN = L_ASN + LT_LIPS-LFIMG.
        ENDLOOP.

        LOOP AT LT_TIME.  " WHERE DATE_TIME <= L_CURR_DTIME.
          L_DELJIT = L_DELJIT + LT_TIME-WEMNG_862.
        ENDLOOP.

        IF L_ASN >= L_DELJIT.
          IT_DATA-LIGHT_ASN = C_GREEN.
          IT_DATA-ASN_INDICATOR = 'G'.
        ELSE.
          IT_DATA-LIGHT_ASN = C_RED.
          IT_DATA-ASN_INDICATOR = 'R'.
        ENDIF.

        CLEAR: L_DELJIT.
** Changed on 03/03/11
        LOOP AT LT_TIME  INTO WS_TIME.
          L_INDEX = SY-TABIX.
*          L_DELJIT = L_DELJIT + LT_TIME-WEMNG_862.
          concatenate L_TODAY_DATE L_TIME into l_current_TIME.
          concatenate WS_TIME-EINDT WS_TIME-UZEIT into l_del_TIME.
*          IF LT_TIME-EINDT = SY-DATUM.
*            L_DIFF_CURRENT = W_TIME - LT_TIME-UZEIT.
*          ELSE.
*            L_DIFF_CURRENT = '235959' - LT_TIME-UZEIT.
*          ENDIF.

          CLEAR: L_SUM_QTY, L_DIFF_QTY.
          LOOP AT LT_TIME FROM L_INDEX.
            L_SUM_QTY = L_SUM_QTY + LT_TIME-WEMNG_862.
          ENDLOOP.
          L_DIFF_QTY = L_SUM_QTY - LT_TEMP-OPEN_QTY.
          IF l_current_time < l_del_TIME.
            IF L_DIFF_QTY <= 0.
              CLEAR: WS_TIME-STATUS.
            ELSE.
              WS_TIME-STATUS = 'Pre_delivery'.
            ENDIF.
          ELSE.
            CLEAR: L_MINUTES.
            CALL FUNCTION 'DELTA_TIME_DAY_HOUR'
              EXPORTING
                T1      = WS_TIME-UZEIT
                T2      = L_TIME
                D1      = WS_TIME-EINDT
                D2      = L_TODAY_DATE
              IMPORTING
                MINUTES = L_MINUTES.


            IF L_MINUTES > 30.
              IF L_DIFF_QTY <= 0.
                WS_TIME-STATUS = 'Late'.
              ELSE.
                CLEAR: L_SUM_QTY, L_DIFF_QTY.
                L_INDEX = L_INDEX + 1.
                LOOP AT LT_TIME FROM L_INDEX.
                  L_SUM_QTY = L_SUM_QTY + LT_TIME-WEMNG_862.
                ENDLOOP.
                IF  L_SUM_QTY < LT_TEMP-OPEN_QTY.
                  WS_TIME-STATUS = 'Partial Delivery'.
                ELSE.
                  WS_TIME-STATUS = 'Finished'.
                ENDIF.
              ENDIF.
            ELSE.
              IF L_DIFF_QTY <= 0.
                WS_TIME-STATUS = 'Current'.
              ELSE.
                IF  L_SUM_QTY < LT_TEMP-OPEN_QTY.
                  WS_TIME-STATUS = 'Partial Delivery'.
                ELSE.

                  WS_TIME-STATUS = 'Finished'.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*       LOOP AT LT_TIME.  " WHERE DATE_TIME <= L_CURR_DTIME.
*
*          L_DELJIT = L_DELJIT + LT_TIME-WEMNG_862.
*          IF LT_TIME-EINDT = SY-DATUM.
*            L_DIFF_CURRENT = W_TIME - LT_TIME-UZEIT.
*          ELSE.
*            L_DIFF_CURRENT = '235959'.
*          ENDIF.
*
*          CLEAR: L_MENGE, L_DIFF.

*          LOOP AT LT_MSEG WHERE PROCESSED = ' '.
*            L_MENGE = L_MENGE + LT_MSEG-MENGE - LT_MSEG-DELJIT.
*            IF L_MENGE >= LT_TIME-WEMNG_862.
*              LT_MSEG-DELJIT = LT_MSEG-DELJIT + LT_TIME-WEMNG_862.
*              IF LT_MSEG-DELJIT = LT_MSEG-MENGE.
*                LT_MSEG-PROCESSED = 'X'.
*              ENDIF.
*              MODIFY LT_MSEG.
*
*              IF L_DIFF_CURRENT <= '003000'.
*                LT_TIME-STATUS = 'Current'.
*              ELSE.
*                IF LT_TIME-DATE_TIME >= LT_MSEG-DATE_TIME.
*                  LT_TIME-STATUS = 'Finished'.
*                ELSE.
*                  IF LT_MSEG-CPUDT = LT_TIME-EINDT.
*                    L_DIFF = LT_MSEG-CPUTM - LT_TIME-UZEIT.
*                  ELSE.
*                    L_DIFF = '235959'.
*                  ENDIF.
*                  IF L_DIFF > '010000'.
*                    LT_TIME-STATUS = 'Late'.
*                  ELSE.
*                    LT_TIME-STATUS = 'Finished'.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
**              CLEAR: l_trailer.
**              SELECT SINGLE lifex INTO l_trailer
**                FROM vbuk AS a
**                INNER JOIN likp AS b
**                ON a~vbeln = b~vbeln
**                WHERE a~vbeln = lt_mseg-xblnr.
**                  AND a~lvstk <> 'C'.
*              EXIT.
*            ELSE.
*
*            ENDIF.
*          ENDLOOP.

*          IF  LT_TIME-STATUS IS INITIAL AND L_DIFF_CURRENT <= '003000'.
*            LT_TIME-STATUS = 'Current'.
*          ENDIF.
** End of change
          IT_DATA-EINDT = WS_TIME-EINDT.
          IT_DATA-UZEIT = WS_TIME-UZEIT.
          IT_DATA-WEMNG_862 = WS_TIME-WEMNG_862.
          IT_DATA-LIGHT_JIT = C_GREEN.
          IT_DATA-JIT_INDICATOR = 'G'.
          IT_DATA-MESSAGE = WS_TIME-STATUS.
*          it_data-lifex = l_trailer.
          IF IT_DATA-MATNR IS INITIAL.
            MOVE 'C211' TO IT_DATA-COLOR_LINE.
          ELSE.
            MOVE 'C411' TO IT_DATA-COLOR_LINE.
          ENDIF.
          CASE WS_TIME-STATUS.
            WHEN 'Late'.
              L_COL = '6'.
              PERFORM SET_CELL_COLOR USING L_COL
                                          '1'
                                      'MESSAGE'
                                   CHANGING IT_DATA-COLOR[].
              PERFORM SET_CELL_COLOR USING L_COL
                                           '1'
                                       'WEMNG_862'
                                    CHANGING IT_DATA-COLOR[].

              PERFORM SET_CELL_COLOR USING L_COL
                                           '1'
                                       'UZEIT'
                                    CHANGING IT_DATA-COLOR[].

            WHEN 'Current'.
              L_COL = '5'.
              PERFORM SET_CELL_COLOR USING L_COL
                                      '1'
                                  'MESSAGE'
                               CHANGING IT_DATA-COLOR[].
              PERFORM SET_CELL_COLOR USING L_COL
                                           '1'
                                       'WEMNG_862'
                                    CHANGING IT_DATA-COLOR[].

              PERFORM SET_CELL_COLOR USING L_COL
                                           '1'
                                       'UZEIT'
                                    CHANGING IT_DATA-COLOR[].
            WHEN 'Pre_delivery'.
              L_COL = '3'.
              PERFORM SET_CELL_COLOR USING L_COL
                                      '1'
                                  'MESSAGE'
                               CHANGING IT_DATA-COLOR[].
              PERFORM SET_CELL_COLOR USING L_COL
                                           '1'
                                       'WEMNG_862'
                                    CHANGING IT_DATA-COLOR[].

              PERFORM SET_CELL_COLOR USING L_COL
                                           '1'
                                       'UZEIT'
                                    CHANGING IT_DATA-COLOR[].
          ENDCASE.
          APPEND IT_DATA.
          CLEAR: LT_TIME, WS_TIME.
          CLEAR: IT_DATA.
          REFRESH: IT_DATA-COLOR[].
        ENDLOOP.
** end of getting ASN for an SA
      ELSE.
        IF IT_DATA-MATNR IS INITIAL.
          MOVE 'C211' TO IT_DATA-COLOR_LINE.
        ELSE.
          MOVE 'C411' TO IT_DATA-COLOR_LINE.
        ENDIF.
        IT_DATA-LIGHT_JIT = C_RED.
        IT_DATA-JIT_INDICATOR = 'R'.
        APPEND IT_DATA.
      ENDIF.
    ELSE.
      IF IT_DATA-MATNR IS INITIAL.
        MOVE 'C211' TO IT_DATA-COLOR_LINE.
      ELSE.
        MOVE 'C411' TO IT_DATA-COLOR_LINE.
      ENDIF.
      IT_DATA-LIGHT_JIT = C_RED.
      IT_DATA-JIT_INDICATOR = 'R'.
      APPEND IT_DATA.
    ENDIF.
    CLEAR: IT_DATA, LT_TEMP.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_0200 OUTPUT.
*  IF GRID_CONTAINER IS INITIAL.
  IF G_DOCKING_CONTAINER IS INITIAL.
    PERFORM CREATE_CONTAINER_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM SORT_TABLE.
*    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_OBJECT.
  CLEAR: W_REPID.

  W_REPID = SY-REPID.
  CREATE OBJECT G_DOCKING_CONTAINER
    EXPORTING
      REPID     = W_REPID
      DYNNR     = SY-DYNNR
      SIDE      = CL_GUI_DOCKING_CONTAINER=>DOCK_AT_BOTTOM
*     RATIO     = 90
      EXTENSION = 2000.

  CREATE OBJECT ALV_GRID
    EXPORTING
      I_PARENT      = G_DOCKING_CONTAINER
      I_APPL_EVENTS = 'X'.
ENDFORM.                    " CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA: L_DATE_C(10),
        L_TIME(8),
        L_UPH_C(6).
  .

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
  CONCATENATE W_DATE+4(2) '/' W_DATE+6(2) '/' W_DATE+0(4)
                                                         INTO L_DATE_C.
  CONCATENATE SY-UZEIT+0(2) ':' SY-UZEIT+2(2) ':' SY-UZEIT+4(2)
                                                         INTO L_TIME.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
*  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-CTAB_FNAME  = 'COLOR'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
*  WA_IS_LAYOUT-EXCP_FNAME = 'LIGHTS'.
  WA_IS_LAYOUT-INFO_FNAME = 'COLOR_LINE'.

  CONCATENATE 'As of' L_DATE_C L_TIME INTO WA_IS_LAYOUT-GRID_TITLE
  SEPARATED BY SPACE.

*  CONCATENATE WA_IS_LAYOUT-GRID_TITLE W_UPDATING INTO
*  WA_IS_LAYOUT-GRID_TITLE
*  SEPARATED BY SPACE.
*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

  WA_IS_LAYOUT-ZEBRA             = 'X'.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3194   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME.

  DATA: L_CN(2) TYPE N,
  L_RP(30),
  L_HR(10).

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.
  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME     = W_REPID
      I_INTERNAL_TABNAME = LW_ITAB
      I_INCLNAME         = W_REPID
    CHANGING
      CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                 'S' 'LIGHT_JIT'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'ICON'         'X',
                                 ' ' 'COLTEXT'     'JIT',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'LIGHT_ASN'    ' ',
*                                 ' ' 'KEY'         'X',
                                 ' ' 'ICON'         'X',
                                 ' ' 'COLTEXT'     'ASN',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'LIFNR'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'SORTL'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Name',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EBELN'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'PO Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MATNR'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                 'S' 'DISPO'    ' ',
*                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'LGBZO'    ' ',
*                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Dock Point',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'MENGE'    ' ',
*                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Schdl Quanitity',
                                 ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'OPEN_QTY'    ' ',
*                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Open Quanitity',
                                 ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'WEMNG'    ' ',
*                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'GR Quanitity',
                                 ' ' 'NO_ZERO'     'X',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'EINDT'    ' ',
                                 ' ' 'COLTEXT'     'Schdl Date',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'UZEIT'    ' ',
                                 ' ' 'COLTEXT'     'Schdl Time',
                                  'E' 'OUTPUTLEN'   '8',

                                 'S' 'WEMNG_862'    ' ',
                                 ' ' 'COLTEXT'     'Del Quanitity',
                                  'E' 'OUTPUTLEN'   '10',

*                                 'S' 'WEMNG_856'    ' ',
*                                 ' ' 'COLTEXT'     'ASN Quanitity',
*                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'TRAILER'    ' ',
                                  ' ' 'COLTEXT'     'Trailer',
                                  'E' 'OUTPUTLEN'   '15',

                                 'S' 'MESSAGE'    ' ',
                                  ' ' 'COLTEXT'     'Current Status',
                                  'E' 'OUTPUTLEN'   '20'.

ENDFORM.                    " BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WA_IS_LAYOUT
      I_SAVE          = WA_SAVE
      IS_VARIANT      = WA_VARIANT
    CHANGING
      IT_FIELDCATALOG = IT_FIELDCAT[]
      IT_OUTTAB       = IT_DATA[]
      IT_SORT         = IT_SORT[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REFR'.
      SUBMIT ZMMR_ASN_JIT_STATUS
          WITH P_WERKS = P_WERKS
          WITH P_DATE = P_DATE
          WITH S_LGBZO IN  S_LGBZO
          WITH P_RCVPRN = P_RCVPRN
          WITH P_INTRV = P_INTRV.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
FORM SET_CELL_COLOR  USING    U_COL
                              U_INT
                              U_FIELD
                     CHANGING COLOR_TAB
                              TYPE SLIS_T_SPECIALCOL_ALV.
*----------------------------------------------------------------------*
* No  Colour
*  0  COL_BACKGROUND
*  1  COL_HEADING
*  2  COL_NORMAL
*  3  COL_TOTAL
*  4  COL_KEY
*  5  COL_POSITIVE
*  6  COL_NEGATIVE
*  7  COL_GROUP
*----------------------------------------------------------------------*
  DATA : L_COLOR TYPE SLIS_SPECIALCOL_ALV.
  L_COLOR-FIELDNAME = U_FIELD.
  L_COLOR-COLOR-COL = U_COL.
  L_COLOR-COLOR-INT = U_INT.
  APPEND L_COLOR TO COLOR_TAB.
ENDFORM.                    " set_cell_color

*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  W_REPID = SY-REPID.
  W_DYNNR = SY-DYNNR.
  IF SY-TCODE <> 'ZMMR00010'
     AND SY-UNAME = 'HIS20043'
     AND SY-UNAME = 'HIS20037'.
  ELSE.
    P_DATE = SY-DATUM.
  ENDIF.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Module  SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TIMER OUTPUT.
  IF TIMER_CONTAINER IS INITIAL.
    CREATE OBJECT:
       TIMER_CONTAINER
             EXPORTING
                  CONTAINER_NAME = 'TI_CONTAINER',
       GUI_TIMER
             EXPORTING
                  PARENT = TIMER_CONTAINER.

    SET HANDLER EVENT_HANDLER->ON_FINISHED FOR GUI_TIMER.

    GUI_TIMER->INTERVAL = TIMEOUT_INTERVAL.
    CALL METHOD GUI_TIMER->RUN.
  ENDIF.
ENDMODULE.                 " SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  sort_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_TABLE.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIGHT_JIT'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'LIGHT_ASN'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'EBELN'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
* it_sort-spos           = 5.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 6.
*  it_sort-fieldname      = 'DISPO'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
* it_sort-spos           = 6.
*  it_sort-fieldname      = 'LGBZO'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.


*  it_sort-spos           = 7.
*  it_sort-fieldname      = 'MENGE_C'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 8.
*  it_sort-fieldname      = 'OPEN_QTY_C'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
* it_sort-spos           = 9.
*  it_sort-fieldname      = 'WEMNG_C'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " sort_table
*&---------------------------------------------------------------------*
*&      Form  save_data_for_bw
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA_FOR_BW.
  DATA: LT_TEMP LIKE TABLE OF ZTMM_DMS WITH HEADER LINE.

  DELETE FROM ZTMM_DMS CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
  LOOP AT IT_DATA.
    IF IT_DATA-EBELN IS INITIAL.
      LT_TEMP-EINDT = IT_DATA-EINDT.
      LT_TEMP-UZEIT = IT_DATA-UZEIT.
      LT_TEMP-WEMNG_862 = IT_DATA-WEMNG_862.
      LT_TEMP-WEMNG_856 = IT_DATA-WEMNG_856.
      LT_TEMP-OPEN_QTY = IT_DATA-OPEN_QTY.
      LT_TEMP-MESSAGE = IT_DATA-MESSAGE.
    ELSE.
      MOVE-CORRESPONDING IT_DATA TO LT_TEMP.
    ENDIF.
    LT_TEMP-ZSDAT = SY-DATUM.
    LT_TEMP-ZSTIM = SY-UZEIT.
    LT_TEMP-ZUSER = SY-UNAME.
    APPEND LT_TEMP.
  ENDLOOP.
  INSERT ZTMM_DMS FROM TABLE LT_TEMP ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
*    MESSAGE e009 WITH 'Table Update Error'.
  ENDIF.
ENDFORM.                    " save_data_for_bw
