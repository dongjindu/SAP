*----------------------------------------------------------------------*
*   INCLUDE ZRPP_REQUIRMENTS_F01                                       *
*----------------------------------------------------------------------*
*********************************************************************
***  FORMS
*********************************************************************
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       MODIFY SELECTION SCREEN BEFORE OUTPUT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN.
  P_DATE-SIGN = 'I'.
  P_DATE-OPTION = 'EQ'.
  P_DATE-LOW = SY-DATUM.
  P_DATE-HIGH = P_DATE-LOW + 7.
  APPEND P_DATE.
*  LOOP AT SCREEN.
*   IF SCREEN-NAME EQ 'P_DATE-LOW'.
*     SCREEN-INPUT = 0.
*     MODIFY SCREEN.
*   ENDIF.
*  ENDLOOP.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  MAKE_TYPE_LIST
*&---------------------------------------------------------------------*
*       LIST FOR LP OR KD PARTS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_TYPE_LIST.
  DATA : BEGIN OF IT_LPKD OCCURS 0,
          PROFL    LIKE  TDG41-PROFL,
          PROFLD   LIKE  TDG42-PROFLD.
  DATA : END OF IT_LPKD.

   CLEAR VALUETAB. REFRESH VALUETAB.
   CLEAR FIELDS.   REFRESH FIELDS.
   SELECT A~PROFL B~PROFLD INTO TABLE IT_LPKD
    FROM TDG41 AS A INNER JOIN TDG42 AS B ON
         A~PROFL = B~PROFL
    WHERE B~DGSPRAS = 'EN'. "AND
*          A~PROFL NE '001'.
    LOOP AT IT_LPKD.
      VALUETAB-VALUE = IT_LPKD-PROFL.
      APPEND VALUETAB.
      CLEAR VALUETAB.
      VALUETAB-VALUE = IT_LPKD-PROFLD.
      APPEND VALUETAB.
      CLEAR VALUETAB.
    ENDLOOP.

  PERFORM ADD_FIELDS USING: 'TDG41' 'PROFL' 'X',
                            'TDG42' 'PROFLD' ' '.

  PERFORM HELP_VALUES_GET.
  P_LPKD = SELECT_VALUE.

ENDFORM.                    " MAKE_TYPE_LIST

*&---------------------------------------------------------------------*
*&      Form  MAKE_VENDOR_LIST
*&---------------------------------------------------------------------*
*       VALUE-REQUEST LIST FOR VENDORS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_VENDOR_LIST.
  DATA : BEGIN OF LT_VEND OCCURS 0,
          LIFNR    LIKE  LFA1-LIFNR,
          NAME1     LIKE  LFA1-NAME1.
  DATA : END OF LT_VEND.

   CLEAR VALUETAB. REFRESH VALUETAB.
   CLEAR FIELDS.   REFRESH FIELDS.
   SELECT A~LIFNR A~NAME1 INTO TABLE LT_VEND
    FROM LFA1 AS A INNER JOIN EORD AS B ON
         A~LIFNR = B~LIFNR.

    SORT LT_VEND BY LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_VEND COMPARING LIFNR.

    LOOP AT LT_VEND.
      VALUETAB-VALUE = LT_VEND-LIFNR.
      APPEND VALUETAB.
      CLEAR VALUETAB.
      VALUETAB-VALUE = LT_VEND-NAME1.
      APPEND VALUETAB.
      CLEAR VALUETAB.
    ENDLOOP.

  PERFORM ADD_FIELDS USING: 'LFA1' 'LIFNR' 'X',
                            'LFA1' 'NAME1' ' '.

  PERFORM HELP_VALUES_GET.
  P_VEND = SELECT_VALUE.

ENDFORM.                    " MAKE_VENDOR_LIST

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*&      CHECK THE SELECTION SCREEN INPUT
*&---------------------------------------------------------------------*
 FORM CHECK_INPUT.
    IF P_DATE-HIGH IS INITIAL.
      P_DATE-HIGH = P_DATE-LOW.
    ENDIF.
*--->CHECK THE VENDOR IS VALID OR NOT
    IF NOT P_VEND IS INITIAL.
      SELECT COUNT(*) FROM EORD WHERE LIFNR = P_VEND.
      IF SY-SUBRC <> 0.
        MESSAGE E000 WITH 'INVALID VENDOR ! '.
      ENDIF.
    ENDIF.
*--->CHECK THE CONTROLLER
    IF NOT P_CTRL IS INITIAL.
      SELECT COUNT(*) FROM T024D WHERE DISPO = P_CTRL.
      IF SY-SUBRC <> 0.
         MESSAGE E000 WITH 'INVALID CONTROLLER !'.
      ENDIF.
    ENDIF.
*--->CHECK THE PART TYPE
    IF NOT P_LPKD IS INITIAL.
       SELECT COUNT(*) FROM TDG41 WHERE PROFL = P_LPKD.
       IF SY-SUBRC <> 0.
         MESSAGE E000 WITH 'INVALID PART TYPE !'.
       ENDIF.
       WA_LPKD = P_LPKD.
    ENDIF.
*--->CHECK THE REPORT TYPE: 21 DAYS/WEEKS OR USER SPECIFIED PERIOD
    IF P_21 = 'X'.
      LOOP AT SCREEN.
       IF SCREEN-NAME = 'P_DATE-LOW' OR
          SCREEN-NAME = 'P_DATE-HIGH'.
           SCREEN-INPUT = 0.
           MODIFY SCREEN.
       ENDIF.
      ENDLOOP.
    ENDIF.
 ENDFORM.              "CHECK_INUT

*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM HELP_VALUES_GET.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY                   = ' '
       IMPORTING
            INDEX                     = SELECT_INDEX
            SELECT_VALUE              = SELECT_VALUE
       TABLES
            FIELDS                    = FIELDS
            SELECT_VALUES             = SELECT_VALUES
            VALUETAB                  = VALUETAB
       EXCEPTIONS
            FIELD_NOT_IN_DDIC         = 1
            MORE_THEN_ONE_SELECTFIELD = 2
            NO_SELECTFIELD            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM ADD_FIELDS USING  P_TABNAME P_FIELDNAME P_FLAG.
  FIELDS-TABNAME = P_TABNAME.
  FIELDS-FIELDNAME = P_FIELDNAME.
  FIELDS-SELECTFLAG = P_FLAG.
  APPEND FIELDS.      CLEAR FIELDS.
ENDFORM.                    " ADD_FIELDS

*&---------------------------------------------------------------------*
*&      Form  INIT_VARIABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_VARIABLES.



  CLEAR: V_STAT, G_MESSAGE.
  G_LTP_BEGDATE  = SY-DATUM.
  L_MRP_LASTDATE = SY-DATUM + 20.
*  PERFORM GET_LASTMRPDATE.

*  IF P_DATE-HIGH GT L_MRP_LASTDATE
*     OR P_21 = 'X'.
*     G_LTP = 'X'.
*  ENDIF.

*-->CHECK WHICH REPORT 21+21 OR PERIOD SPECIFIED
  IF P_21 = 'X'.
    P_DATE-LOW = SY-DATUM.
    P_DATE-HIGH = SY-DATUM + 168."21DAYS+21WEEKS     UD1K914168
    MODIFY P_DATE FROM P_DATE index 1.
    PERFORM SET_DAYS.
  ENDIF.



ENDFORM.                    " INIT_VARIABLES

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       READ DATA FROM MDKP
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: L_LINES TYPE I.
  RANGES: R_LGORT FOR MARD-LGORT,              "UD1K912703
          R_CTRL FOR MDKP-DISPO,               "UD1K912703
          R_RAUBE FOR MARA-RAUBE.              "UD1K912703
  IF NOT P_LGORT IS INITIAL.
    R_LGORT-SIGN   = 'I'.                      "UD1K912703
    R_LGORT-OPTION = 'EQ'.                     "UD1K912703
    R_LGORT-LOW    = P_LGORT.
    R_LGORT-HIGH   = ''.                       "UD1K912703
    APPEND R_LGORT.
  ENDIF.
  IF NOT P_CTRL IS INITIAL.                    "UD1K912703
    R_CTRL-SIGN   = 'I'.                       "UD1K912703
    R_CTRL-OPTION = 'EQ'.                      "UD1K912703
    R_CTRL-LOW    = P_CTRL.                    "UD1K912703
    R_CTRL-HIGH   = ''.                        "UD1K912703
    APPEND R_CTRL.                             "UD1K912703
  ENDIF.
  IF NOT P_RAUBE IS INITIAL.                   "UD1K912703
    R_RAUBE-SIGN   = 'I'.                      "UD1K912703
    R_RAUBE-OPTION = 'EQ'.                     "UD1K912703
    R_RAUBE-LOW    = P_RAUBE.                  "UD1K912703
    R_RAUBE-HIGH   = ''.                       "UD1K912703
    APPEND R_RAUBE.                            "UD1K912703
  ENDIF.
*-->READING RESERVED MATERIALS FROM RESB
* IF NOT P_CTRL IS INITIAL.
   SELECT A~MATNR A~PLWRK A~PLSCN INTO TABLE IT_MDKP
     FROM ( MDKP AS A INNER JOIN MARC AS B
          ON A~MATNR = B~MATNR AND
             A~PLWRK = B~WERKS )
          INNER JOIN MARA AS C                 "UD1K912703
          ON A~MATNR = C~MATNR                 "UD1K912703
     WHERE ( A~MTART = 'ROH' OR
           A~MTART = 'ROH1' OR
           A~MTART = 'HALB' OR
           A~MTART = 'ERSA' ) AND
           A~SOBSL <> '50'    AND
           A~PLWRK = P_PLANT  AND
           A~MATNR IN P_MAT   AND
           ( A~DTART = 'MD' OR                  "UD1K912703
           ( A~DTART = 'LP' AND                 "UD1K912703
             A~PLSCN = '900' ) ) AND            "UD1K912703
           A~DISPO IN R_CTRL     AND            "UD1K912703
           B~LGPRO IN R_LGORT    AND            "UD1K912703
           C~RAUBE IN R_RAUBE.                  "UD1K912703
*  ELSE.                                        "UD1K912703
*    SELECT MATNR PLWRK PLSCN INTO TABLE IT_MDKP"UD1K912703
*     FROM MDKP                                 "UD1K912703
*     WHERE ( MTART = 'ROH' OR                  "UD1K912703
*           MTART = 'ROH1' OR                   "UD1K912703
*           MTART = 'HALB' OR                   "UD1K912703
*           MTART = 'ERSA' ) AND                "UD1K912703
*           MATNR IN P_MAT   AND                "UD1K912703
*           SOBSL <> '50'.                      "UD1K912703
*  ENDIF.                                       "UD1K912703

   IF SY-SUBRC <> 0.
     V_STAT = 'N'.
     EXIT.
   ENDIF.
   SORT IT_MDKP BY MATNR WERKS.
   DELETE ADJACENT DUPLICATES FROM IT_MDKP COMPARING MATNR WERKS.

*-->SUM AND STORE THE READ DATA INTO IT_REQ_A
*-->CHECK THE PART TYPE
   PERFORM CHECK_LPKD.

*-->CHECK MARERIAL VENDOR
    PERFORM CHECK_VENDOR.
*--->IF NO DATA, SET FLAG
    DESCRIBE TABLE IT_REQ_A LINES L_LINES.
    IF L_LINES = 0.
      V_STAT = 'N'.
    ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_LPKD
*&---------------------------------------------------------------------*
*       CHECK PART TYPE AND REDUCE THE RECORDS
*----------------------------------------------------------------------*
FORM CHECK_LPKD.
  DATA: BEGIN OF LT_LPKD OCCURS 0,
          MATNR LIKE MDKP-MATNR,
          PROFL LIKE MARA-PROFL,
        END OF LT_LPKD.
*-->IF PART TYPE IS SPECIFIED, DELETE MATERIALS WITH OTHER TYPES
  IF NOT P_LPKD IS INITIAL.
  SELECT MATNR PROFL INTO TABLE LT_LPKD
   FROM MARA
   FOR ALL ENTRIES IN IT_MDKP
   WHERE MATNR = IT_MDKP-MATNR AND
         PROFL = WA_LPKD.
  ELSE.
   SELECT MATNR PROFL INTO TABLE LT_LPKD
   FROM MARA
   FOR ALL ENTRIES IN IT_MDKP
   WHERE MATNR = IT_MDKP-MATNR." AND
*         PROFL NE 'K' AND
*         PROFL NE 'V' AND
*         PROFL NE 'M' AND
*         PROFL NE '001'.
  ENDIF.
  IF SY-SUBRC <> 0.
    CLEAR IT_MDKP[]. "NO MATERIALS
    EXIT.
  ENDIF.
  CLEAR: IT_REQ_A, IT_REQ_A[].
  LOOP AT IT_MDKP.
    L_TABIX = SY-TABIX.
    READ TABLE LT_LPKD WITH KEY MATNR = IT_MDKP-MATNR.
    IF SY-SUBRC = 0.
     IT_REQ_A-MATNR = IT_MDKP-MATNR.
     IT_REQ_A-PROFL = LT_LPKD-PROFL.
     IT_REQ_A-WERKS = IT_MDKP-WERKS.
     APPEND IT_REQ_A.
    ELSE.
     DELETE IT_MDKP INDEX L_TABIX.
    ENDIF.
  ENDLOOP.

ENDFORM.         "CHECK_LDKP

*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR
*&---------------------------------------------------------------------*
*       CHECK VENDOR AND REDUCE RECORDS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VENDOR.
  DATA: BEGIN OF LT_VENDOR OCCURS 0,
          MATNR LIKE EINA-MATNR,
          LIFNR LIKE EINA-LIFNR,
        END OF LT_VENDOR.
  DATA: L_TABIX LIKE SY-TABIX.
*-->READING VENDORS FOR MATERIALS
  SELECT MATNR LIFNR INTO TABLE IT_VENDOR
  FROM EORD
  FOR ALL ENTRIES IN IT_REQ_A
  WHERE MATNR = IT_REQ_A-MATNR AND
        VDATU LE SY-DATUM AND
        BDATU GE P_DATE-HIGH.
  CHECK SY-SUBRC = 0.
  SORT IT_VENDOR BY LIFNR MATNR.
  DELETE ADJACENT DUPLICATES FROM IT_VENDOR
   COMPARING LIFNR MATNR.
  IF NOT P_VEND IS INITIAL.
*-->REDUCE RECORDS
    LOOP AT IT_REQ_A.
     L_TABIX = SY-TABIX.
     READ TABLE IT_VENDOR WITH KEY MATNR = IT_REQ_A-MATNR
                                   LIFNR = P_VEND.
     IF SY-SUBRC <> 0.
       DELETE IT_REQ_A INDEX L_TABIX.
     ELSE.
       IT_REQ_A-LIFNR = IT_VENDOR-LIFNR.
       MODIFY IT_REQ_A.
     ENDIF.
    ENDLOOP.
  ELSE.
  ENDIF.
ENDFORM.                    " CHECK_VENDOR



*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       READ DUE-IN QTY AND CALCULATE THE SHORTAGE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
    IF V_STAT = 'N'.
     EXIT.
    ENDIF.
*-->READING THE DESCRIPTION
    PERFORM READ_DES.
*-->RE-SUM THE 24 WEEKS DATA
*--> 1. FIND THE EARLIEST LTP BEGIN DATE FOR ALL MATERIALS
*--> 2. FROM THIS DATE COUNT FORMWORD TO THE END OF 24 WEEKS
*--> 3. IF NO DATA AFTER 21 WEEKS, DO NOT DISPLAY IT.
*--> 4. IF AFTER 21 WEEKS WE HAVE DATA AND IT'S ALSO WITHIN
*-->    21+21WEEKS, THE DATA SHOULD BE DISPLAYED.
    IF P_21 = 'X'.
     PERFORM RE_SUM_24_WEEKS.
    ENDIF.
ENDFORM.                    " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_DES
*&---------------------------------------------------------------------*
*       READING MATERIAL DESCRIPTION
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DES.
  DATA: BEGIN OF LT_DES OCCURS 0,
          MATNR LIKE MAKT-MATNR,
          MAKTX LIKE MAKT-MAKTX,
        END OF LT_DES.
  SELECT MATNR MAKTX INTO TABLE LT_DES
    FROM MAKT
    FOR ALL ENTRIES IN IT_REQ_A
    WHERE MATNR = IT_REQ_A-MATNR.
   CHECK SY-SUBRC = 0.
*-->TRANSFER THE DESCRIPTION
   LOOP AT IT_REQ_A.
    READ TABLE LT_DES WITH KEY MATNR = IT_REQ_A-MATNR.
    IT_REQ_A-MAKTX = LT_DES-MAKTX.
    MODIFY IT_REQ_A.

*-->TRANSFER THE DESCRIPTION TO 21+21 REPORT
    IT_21LAYOUT-MATNR = IT_REQ_A-MATNR.
    IT_21LAYOUT-MAKTX = LT_DES-MAKTX.
    MODIFY IT_21LAYOUT FROM IT_21LAYOUT
          TRANSPORTING MAKTX
          WHERE MATNR = IT_21LAYOUT-MATNR.

   ENDLOOP.
ENDFORM.       "READ_DES

*&---------------------------------------------------------------------*
*&      Form  GET_QUANTITY
*&---------------------------------------------------------------------*
*       GET QUANTITY OF  MATERIALS
*----------------------------------------------------------------------*
FORM GET_QUANTITY.
   DATA: L_PLSCN LIKE MDKP-PLSCN,
         L_MATNR LIKE MARC-MATNR,
         L_WERKS LIKE MARC-WERKS,
         L_REQ LIKE MDSU-MNG02,
         L_DUEIN LIKE MDSU-MNG03,
         L_STOCK LIKE MDSU-MNG04,
         L_AVAIL LIKE MDSU-MNG04,
         P_STATUS.
   DATA: I_TABIX LIKE SY-TABIX.
   DATA: L_COUNT TYPE I.

    IF V_STAT = 'N'.
      G_MESSAGE = 'No Material selected.'.
      EXIT.
    ENDIF.

    CLEAR: IT_21, IT_21[].
*-->LOOP AND GET QTY FOR EACH MATERIAL
    LOOP AT IT_REQ_A.
      I_TABIX = SY-TABIX.
      CLEAR: L_REQ, L_DUEIN, L_AVAIL.
      L_MATNR = IT_REQ_A-MATNR.
      L_WERKS = IT_REQ_A-WERKS.
      PERFORM READ_QUANTITY USING L_MATNR L_WERKS
                         CHANGING L_REQ   L_DUEIN  L_STOCK
                                  L_AVAIL P_STATUS.
      IF P_STATUS IS INITIAL. "NO ERROR FOR FM EXECUTION
*--->IF NO REQUIREMENTS, DELETE IT FROM LIST
*--->STORE THE RESULT
       IF L_REQ <> 0 OR L_DUEIN NE 0 OR L_AVAIL NE 0.
        IT_REQ_A-MENGE = L_DUEIN.
        IT_REQ_A-BDMNG = L_REQ.
        IT_REQ_A-LABST = L_STOCK.
        IT_REQ_A-SHORT = L_AVAIL.
        READ TABLE IT_VENDOR INDEX 1.
        IF SY-SUBRC = 0.
          IT_REQ_A-LIFNR = IT_VENDOR-LIFNR.
        ENDIF.
        MODIFY IT_REQ_A.
*--->   BUILD THE 21+21 DATA
        PERFORM BUILD_21DATA.
       ELSE.
         DELETE IT_REQ_A INDEX I_TABIX.
       ENDIF.
      ELSE.
       MESSAGE I001 WITH TEXT-005 L_MATNR.
      ENDIF.


*--->INDICATE THE PROGRESS
     L_COUNT = L_COUNT + 1.
     PERFORM MAKE_PROGRESS USING L_COUNT.
    ENDLOOP.
*-->CHECK THE RESULT.
    DESCRIBE TABLE IT_REQ_A LINES L_COUNT.
    IF L_COUNT = 0.
     G_MESSAGE = 'No Activities for Specified Period.'.
     V_STAT = 'X'.
    ENDIF.

ENDFORM.         "GET_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  READ_QUANTITY
*&---------------------------------------------------------------------*
*       READ THE QUANTITY OF A MATERIAL
*----------------------------------------------------------------------*
FORM READ_QUANTITY USING P_MATNR LIKE MARC-MATNR
                         P_WERKS LIKE MARC-WERKS
                CHANGING P_REQ   LIKE MDSU-MNG02
                         P_DUEIN LIKE MDSU-MNG03
                         P_STOCK LIKE MDSU-MNG04
                         P_AVAIL LIKE MDSU-MNG04
                         P_STATUS.

  DATA : WA_L_MT61D LIKE MT61D,
         WA_L_MDKP  LIKE MDKP.
  DATA:  L_DUEIN    LIKE MDSU-MNG02,
         L_REQ      LIKE MDSU-MNG02,
         L_REQ_T    LIKE MDSU-MNG02,
         L_DUEIN_T  LIKE MDSU-MNG02,
         L_STOCK    LIKE MDSU-MNG02.
  DATA : LT_MDSU    LIKE STANDARD TABLE OF MDSU
                    WITH HEADER LINE .
  DATA : LT_MDEZ    LIKE STANDARD TABLE OF MDEZ
                    WITH HEADER LINE .
  DATA : LT_PURRQS  LIKE STANDARD TABLE OF MDEZ
                    WITH HEADER LINE.
  DATA: L_PLSCN LIKE PLSC-PLSCN.
  DATA: L_AVAIL LIKE MDSU-MNG02.
  DATA: S_STOCK_SAVED.
  DATA: LT_DETAIL TYPE STANDARD TABLE OF DETAIL.
  DATA: WA_DET LIKE WA_DETAIL.


  CLEAR: P_REQ ,  L_REQ,
         P_DUEIN, L_DUEIN,
         P_STOCK, P_AVAIL,
         S_STOCK_SAVED.

  CLEAR: WA_DETAIL, IT_DETAIL[],
         SC_VENDOR, SC_LPKD ,
         IT_VENDOR, IT_VENDOR[],
         LT_PURRQS, LT_PURRQS[].


*-->MRP
    L_PLSCN = '000'.
    CLEAR: G_LTP.
    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
        PLSCN                          = L_PLSCN
        MATNR                          = P_MATNR
        WERKS                          = P_WERKS
      IMPORTING
        E_MT61D                        = WA_L_MT61D
        E_MDKP                         = WA_L_MDKP
      TABLES
*       MDPSX                          =
        MDEZX                          = LT_MDEZ
        MDSUX                          = LT_MDSU
      EXCEPTIONS
        MATERIAL_PLANT_NOT_FOUND       = 1
        PLANT_NOT_FOUND                = 2
        OTHERS                         = 3.

    IF SY-SUBRC <> 0.
      P_STATUS = 'X'.
      EXIT.
    ENDIF.



*--->GET MRP RESULT
   PERFORM FILTER_RESULT TABLES LT_MDEZ
                      USING L_PLSCN.
   PERFORM SUM_RESULT TABLES   LT_MDEZ
                      USING    L_PLSCN
                      CHANGING L_STOCK
                               L_REQ
                               L_DUEIN
                               L_REQ_T
                               L_DUEIN_T
                               L_AVAIL.

   PERFORM COMPRESS_DETAIL.

* DECIDE IF LTP NEEDED.
  PERFORM GET_LTP_INDICATOR.



*-->LTP
  IF G_LTP = 'X'.
    L_PLSCN = '900'.
    CLEAR: WA_L_MT61D,WA_L_MDKP.

    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
       PLSCN                          = L_PLSCN
       MATNR                          = P_MATNR
       WERKS                          = P_WERKS
      IMPORTING
       E_MT61D                        = WA_L_MT61D
       E_MDKP                         = WA_L_MDKP
      TABLES
*      MDPSX                          =
       MDEZX                          = LT_MDEZ
       MDSUX                          = LT_MDSU
      EXCEPTIONS
       MATERIAL_PLANT_NOT_FOUND       = 1
       PLANT_NOT_FOUND                = 2
       OTHERS                         = 3.

    IF SY-SUBRC <> 0.
      P_STATUS = 'X'.
      EXIT.
    ENDIF.
   PERFORM FILTER_RESULT TABLES LT_MDEZ
                      USING L_PLSCN.

   PERFORM SUM_RESULT TABLES   LT_MDEZ
                      USING    L_PLSCN
                      CHANGING L_STOCK
                               L_REQ
                               L_DUEIN
                               L_REQ_T
                               L_DUEIN_T
                               L_AVAIL.
*-->COMPRESS IT_DETAIL

  PERFORM COMPRESS_DETAIL.


  ENDIF.       "IF G_LTP = 'X'.

  PERFORM SUM_MRP_LTP_DETAIL.



*-->ADD MATNR TO IT_VENDOR
  LOOP AT IT_VENDOR.
    IT_VENDOR-MATNR = P_MATNR.
    MODIFY IT_VENDOR.
  ENDLOOP.
*-->DELETE DUPLICATE VENDOR
  SORT IT_VENDOR BY MATNR LIFNR.
  DELETE ADJACENT DUPLICATES FROM IT_VENDOR
    COMPARING MATNR LIFNR.

*-->TOTALIZE THE RESULT.
  P_STOCK = L_STOCK.
  P_AVAIL = L_STOCK + L_DUEIN_T + L_REQ_T.
  P_REQ   = L_REQ.
  P_DUEIN = L_DUEIN.

ENDFORM.          "READ_QUANTITY

*&---------------------------------------------------------------------*
*&      Form  FILTER_RESULT
*&---------------------------------------------------------------------*
*       READ THE RESULT OF MRP OR LTP
*----------------------------------------------------------------------*
*      -->P_L_PLSCN  text
*      -->P_LT_MDEZ  text
*----------------------------------------------------------------------*
FORM FILTER_RESULT TABLES PT_MDEZ STRUCTURE MDEZ
                USING  P_PLSCN LIKE PLSC-PLSCN.
 DATA: LT_MDEZ LIKE STANDARD TABLE OF MDEZ WITH HEADER LINE.
 DATA: MT_MDEZ LIKE STANDARD TABLE OF MDEZ WITH HEADER LINE.

 LT_MDEZ[] = PT_MDEZ[].
 CLEAR: PT_MDEZ, PT_MDEZ[].

*--->NOTE: FILTER THE QTY IN STORAGE LOCATION '9999' 06/29/2005

 IF P_PLSCN = '000'.   "MRP
    LOOP AT LT_MDEZ WHERE ( DAT00 GE SY-DATUM    AND
                        DAT00 LE P_DATE-HIGH     AND
                        DAT00 LE L_MRP_LASTDATE  AND
                        ( LGORT NE '9999' OR DELKZ NE 'LB' )

                        ) OR
                        DELKZ = 'WB'.
*     DELETE Requisition/PlndOrder due-in QTY without vendor
*     Delete the storage location 9999 stock
      IF ( ( ( LT_MDEZ-DELKZ = 'BA' OR
               LT_MDEZ-DELKZ = 'SA' ) AND
         LT_MDEZ-LIFNR IS INITIAL )   OR
         LT_MDEZ-DELKZ = 'LB' )          AND
         LT_MDEZ-MNG01 GT 0.
      ELSE.
        APPEND LT_MDEZ TO MT_MDEZ.
      ENDIF.
    ENDLOOP.
    PT_MDEZ[] = MT_MDEZ[].
 ENDIF.
 IF P_PLSCN = '900'.   "LTP
    LOOP AT LT_MDEZ WHERE ( DAT00 GE SY-DATUM  AND
                        DAT00 GE G_LTP_BEGDATE AND
                        DAT00 LE P_DATE-HIGH   AND
                        ( LGORT NE '9999' OR DELKZ NE 'LB' )
                        AND  DELKZ NE 'SH'  "SAFTY STOCK
                        ) OR
                        DELKZ = 'WB'.
      IF ( ( ( LT_MDEZ-DELKZ = 'BA' OR
           LT_MDEZ-DELKZ = 'SA' )   AND
         LT_MDEZ-LIFNR IS INITIAL ) OR
         LT_MDEZ-DELKZ = 'LB' )        AND
         LT_MDEZ-MNG01 GT 0. "Requisition due-in without vendor
      ELSE.
        APPEND LT_MDEZ TO MT_MDEZ.
      ENDIF.
    ENDLOOP.
    PT_MDEZ[] = MT_MDEZ[].
 ENDIF.


ENDFORM.                    " FILTER_RESULT

*&---------------------------------------------------------------------*
*&      Form  SUM_RESULT
*&---------------------------------------------------------------------*
*       READ THE RESULT OF MRP OR LTP
*----------------------------------------------------------------------*
*      -->P_L_PLSCN  text
*      -->P_LT_MDEZ  text
*----------------------------------------------------------------------*
FORM SUM_RESULT TABLES PT_MDEZ     STRUCTURE MDEZ
                USING P_PLSCN      LIKE PLSC-PLSCN
                CHANGING P_STOCK   LIKE MDEZ-MNG02
                         P_REQ     LIKE MDEZ-MNG02
                         P_DUEIN   LIKE MDEZ-MNG02
                         P_REQ_T   LIKE MDEZ-MNG02
                         P_DUEIN_T LIKE MDEZ-MNG02
                         P_AVAIL   LIKE MDEZ-MNG02.


   DATA: LT_MDEZ LIKE STANDARD TABLE OF MDEZ WITH HEADER LINE.
   DATA: MT_MDEZ LIKE STANDARD TABLE OF MDEZ WITH HEADER LINE.
   DATA: L_STOCK LIKE MDEZ-MNG02,
         L_REQ   LIKE MDEZ-MNG02,
         L_DUEIN LIKE MDEZ-MNG02,
         L_REQ_T LIKE MDEZ-MNG02,
         L_DUEIN_T LIKE MDEZ-MNG02,
         L_AVAIL LIKE  MDEZ-MNG02.
   DATA: L_DATE LIKE SY-DATUM.

   L_STOCK   = P_STOCK.
   L_REQ     = P_REQ.
   L_DUEIN   = P_DUEIN.
   L_REQ_T   = P_REQ_T.
   L_DUEIN_T = P_DUEIN_T.
   L_AVAIL   = P_AVAIL.

   LT_MDEZ[] = PT_MDEZ[].
   CLEAR: PT_MDEZ, PT_MDEZ[].

   LOOP AT LT_MDEZ .
     IF LT_MDEZ-DELKZ = 'WB' .       "STOCK QTY
*        OR  LT_MDEZ-DELKZ = 'SH'.   "SAFTY STOCK

*      PUT THE STOCK AND SAFTY STOCK TOGETHER.
       IF LT_MDEZ-DELKZ = 'WB'.
        L_STOCK = LT_MDEZ-MNG02.
        WA_DETAIL-MNG04 = LT_MDEZ-MNG02.
       ELSE.
        L_STOCK = L_STOCK + LT_MDEZ-MNG01.
        WA_DETAIL-MNG04 = LT_MDEZ-MNG01.
       ENDIF.
*       READ TABLE IT_DETAIL INTO WA_DETAIL WITH KEY ELEMT = 'STOCK'.
*       IF SY-SUBRC <> 0.  " Stock has not been saved
         WA_DETAIL-DAT00 = LT_MDEZ-DAT00.
         WA_DETAIL-ELEMT = 'STOCK'.
         COLLECT WA_DETAIL INTO IT_DETAIL.
         L_AVAIL = L_STOCK.
*       ENDIF.
       CLEAR WA_DETAIL.
*       CONTINUE.


     ELSEIF LT_MDEZ-DELKZ NE 'LB'. "EXCLUDING STORAGE LOCATION P500
                                   "BLOCK QTY
*-->  AVAILABILITY
      L_AVAIL = L_AVAIL + LT_MDEZ-MNG01.
*-->  STORE TOTAL REQ AND DUE-IN QTY

     IF LT_MDEZ-MNG01 GT 0.                        "DUE-IN
        L_DUEIN_T = L_DUEIN_T + LT_MDEZ-MNG01.

     ENDIF.

     IF LT_MDEZ-MNG01 LT 0.                  "REQUIREMENTS
       L_REQ_T = L_REQ_T + LT_MDEZ-MNG01.
     ENDIF.
*-->SUM FOR SPECIFIED PERIOD AND RECORD DETAIL.
     IF LT_MDEZ-DAT00 GE P_DATE-LOW AND
        LT_MDEZ-DAT00 LE P_DATE-HIGH.

        IF LT_MDEZ-MNG01 GT 0.                     "DUE-IN
          L_DUEIN = L_DUEIN + LT_MDEZ-MNG01.
          WA_DETAIL-DAT00 = LT_MDEZ-DAT00.
*          WA_DETAIL-ELEMT = LT_MDEZ-DELKZ.
          WA_DETAIL-MNG03 = WA_DETAIL-MNG03 + LT_MDEZ-MNG01.
*         CHECK THE VENDOR
          IF NOT LT_MDEZ-LIFNR IS INITIAL.
            IT_VENDOR-LIFNR = LT_MDEZ-LIFNR.
            APPEND IT_VENDOR.
          ENDIF.
        ENDIF.

        IF LT_MDEZ-MNG01 LT 0.               "REQUIREMENTS
          L_REQ = L_REQ + LT_MDEZ-MNG01.
          WA_DETAIL-DAT00 = LT_MDEZ-DAT00.
          WA_DETAIL-MNG02 = WA_DETAIL-MNG02 + LT_MDEZ-MNG01.
          IF LT_MDEZ-DELKZ = 'SH'.  "SAFTY STOCK
             WA_DETAIL-ELEMT = 'SAFTY STOCK'.
          ENDIF.
        ENDIF.

     ENDIF.


*    STORE THE REAULT
     IF NOT WA_DETAIL-DAT00 IS INITIAL.
       IF LT_MDEZ-DELKZ NE 'WB'       AND
          LT_MDEZ-DAT00 GE P_DATE-LOW AND
          LT_MDEZ-DAT00 LE P_DATE-HIGH.
        WA_DETAIL-MNG04 = L_AVAIL.
        APPEND WA_DETAIL TO IT_DETAIL.
        CLEAR WA_DETAIL.
       ENDIF.
     ENDIF.

     ENDIF.                       " IF LT_MDEZ-DELKZ = 'WB'.
    ENDLOOP.
*---> OUTPUT
    P_STOCK   = L_STOCK.
    P_REQ     = L_REQ  .
    P_DUEIN   = L_DUEIN.
    P_REQ_T   = L_REQ_T .
    P_DUEIN_T = L_DUEIN_T.
    P_AVAIL   = L_AVAIL.

ENDFORM.     "SUM_RESULT
*&---------------------------------------------------------------------*
*&      Form  MAKE_PROGRESS
*&---------------------------------------------------------------------*
*       READ MAKE PROGRESS INDICATOR
*----------------------------------------------------------------------*
FORM MAKE_PROGRESS USING P_COUNT TYPE I.
  DATA: L_MOD TYPE I.
  DATA: L_TEXT(50).
  DATA: L_COUNT(10).
  L_MOD = P_COUNT MOD 5.

  IF L_MOD = 0.
    L_COUNT = P_COUNT.
    CONCATENATE 'PROCESSED MATERIALS :' L_COUNT INTO L_TEXT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
         TEXT   = L_TEXT.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SET_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DAYS.
  DATA: L_DATE               TYPE D ,
        WA_KALID             LIKE SCAL-FCALID,
        L_SUBRC              LIKE SY-SUBRC,
        L_COUNT              TYPE I .
  DATA: L_DAY(2).
  DATA: L_TEXT(3).
*-->Get working calendar
  PERFORM READ_SHOP_CALID   USING WA_KALID.

  CLEAR: L_DATE, L_COUNT.
*--> From D+1 Day To D+21 Day....
*    (Only Working Dates in FACTORY-Calendar)
*--> TODAY IS D+0 WHICH MUST BE DISPLAY
  L_DATE = P_DATE-LOW .
  IT_WORKDAY-DSEQ = 'D00'.
  IT_WORKDAY-DATE = L_DATE.
  APPEND IT_WORKDAY.
  DO 20 TIMES.
    L_DATE   = L_DATE  + 1.
    PERFORM CHECK_WORKING_DATE USING L_DATE  WA_KALID  L_SUBRC.
    IF L_SUBRC = 0.
      L_COUNT = L_COUNT + 1.
      L_DAY = L_COUNT.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         input  = L_DAY
       IMPORTING
         OUTPUT = L_DAY.
      CONCATENATE 'D' L_DAY INTO L_TEXT.
      IT_WORKDAY-DSEQ = L_TEXT.
      IT_WORKDAY-DATE = L_DATE.
      APPEND IT_WORKDAY.
    ENDIF.
  ENDDO.

* REQUESTED BY CATHERINE CHNAGED BY CHRIS       "UD1K914168
* BAEAUSE THE LTP DATA IS NOT ROLLED OVER TO MRP EVERY DAY,
* SO SOMETIMES THE LTP HORIZON IS NOT FROM THE DAY+21 DATE. THE
* LTP BEGIN DATE IS CHANGED TIME BY TIME. NOW WE ARE GOING TO
* DECIDE THE LTP HORIZON BEGIN DATE BY LOOKING THE LAST DATE OF
* MRP PLAN DATA. IF THE LAST DATE OF MRP IS DAY+15, THEN THE LTP
* DATA WILL BEGIN FROM DAY+16, AND FROM THIS BEGIN DATE, WE COUNT
* 21 WEEKS DATA. BUT BEFORE WE CHECK THE ACTURAL MRP DATE, WE
* DON'T KNOW IT AND ALSO THE LTP BEGIN DATE COULD VARY FOR DIFFERENT
* MATERIALS. SO INITIALLY, WE BUILD 24 WEEKS WHICH COVER ALL POSSIBLE
* DAYS FOR LTP PLAN.
*
*--->SET 21 WEEKS    "COMMENT BY CHRIS "UD1K914168
*--->BECAUSE 21 WEEKS HOTIZON HAS BEEN CHANGED TO
*--->FROM THE NEXT DAY OF MRP LAST DAY THAT HAS
*--->ACTUAL DATA. SO COMMENT THE FOLLOWING CODE
*--->REQUESTED BY CATHERINE S.

*   L_DATE = G_LTP_BEGDATE - 1.
*   CLEAR : L_COUNT, L_DAY,
*           IT_WEEKS, IT_WEEKS[].
*   DO 21 TIMES.
*      L_COUNT = L_COUNT + 1.
*      L_DAY = L_COUNT.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*       EXPORTING
*         input  = L_DAY
*       IMPORTING
*         OUTPUT = L_DAY.
*      CONCATENATE 'W' L_DAY INTO L_TEXT.
*      IT_WEEKS-WSEQ = L_TEXT.
*      IT_WEEKS-DATE1 = L_DATE + 1.
*      IT_WEEKS-DATE2 = L_DATE + 7.
*      APPEND IT_WEEKS.
*      L_DATE = IT_WEEKS-DATE2.
*   ENDDO.

    PERFORM MAKE_24_WEEKS USING G_LTP_BEGDATE.

ENDFORM.                    " SET_DAYS


*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE KALID INTO PA_KALID
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  CHECK_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATE  P_KALID
*----------------------------------------------------------------------*
FORM CHECK_WORKING_DATE USING  P_DATE  P_KALID  P_SUBRC.
  DATA: L_MSGTY  LIKE SY-MSGTY.
  CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
       EXPORTING
            DATE                       = P_DATE
            FACTORY_CALENDAR_ID        = P_KALID
            MESSAGE_TYPE               = 'E'
       EXCEPTIONS
            DATE_AFTER_RANGE           = 1
            DATE_BEFORE_RANGE          = 2
            DATE_INVALID               = 3
            DATE_NO_WORKINGDAY         = 4
            FACTORY_CALENDAR_NOT_FOUND = 5
            MESSAGE_TYPE_INVALID       = 6
            OTHERS                     = 7.
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " CHECK_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  BUILD_21DATA
*&---------------------------------------------------------------------*
*       CONSTRUCT THE 21+21 SUM DATA
*----------------------------------------------------------------------*
*  -->  TABLE: IT_DETAIL, IT_REQ_A-MATNR
*  <--  TABLE: IT_21LAYOUT
*----------------------------------------------------------------------*
FORM BUILD_21DATA .
   FIELD-SYMBOLS: <FS>.
   DATA: L_TEXT(15).
   DATA: I_COUNT TYPE I.
   DATA: L_COUNT(2).

     CLEAR: IT_21LAYOUT, WA_DETAIL.
     IT_21LAYOUT-MATNR = IT_REQ_A-MATNR.
     IT_21LAYOUT-PROFL = IT_REQ_A-PROFL.

*--->BUILD 21 DAYS DATA
     LOOP AT IT_DETAIL_MRP INTO WA_DETAIL.

        IF WA_DETAIL-ELEMT NE 'STOCK' AND
           WA_DETAIL-DAT00 LE L_MRP_LASTDATE.
           READ TABLE IT_WORKDAY WITH KEY DATE = WA_DETAIL-DAT00.
           IF SY-SUBRC = 0.

             CONCATENATE 'IT_21LAYOUT-' IT_WORKDAY-DSEQ INTO L_TEXT.
             ASSIGN (L_TEXT) TO <FS>.
             <FS> = WA_DETAIL-MNG02.

           ENDIF.
        ENDIF.
     ENDLOOP.

*---->BUILD 21 WEEKS DATA
     LOOP AT IT_DETAIL_LTP INTO WA_DETAIL.
       IF WA_DETAIL-ELEMT NE 'STOCK' AND
           WA_DETAIL-DAT00 GE G_LTP_BEGDATE.
           LOOP AT IT_WEEKS WHERE DATE1 LE WA_DETAIL-DAT00 AND
                                  DATE2 GE WA_DETAIL-DAT00.

             CONCATENATE 'IT_21LAYOUT-' IT_WEEKS-WSEQ INTO L_TEXT.
             ASSIGN (L_TEXT) TO <FS>.
             <FS> = WA_DETAIL-MNG02 + <FS>.

           ENDLOOP.
       ENDIF.
     ENDLOOP.

    APPEND IT_21LAYOUT .

    CLEAR: IT_DETAIL, IT_DETAIL[],
           IT_DETAIL_MRP[],
           IT_DETAIL_LTP[].


ENDFORM.                    " BUILD_21DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_21_WEEKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_LTP_BEGDATE  text
*----------------------------------------------------------------------*
form MAKE_24_WEEKS using    p_begdate.
  DATA: DATE                 LIKE SY-DATUM,
        WEEK                 LIKE SCAL-WEEK,
        L_COUNT              TYPE I ,
        MONDAY               LIKE SY-DATUM,
        SUNDAY               LIKE SY-DATUM,
        L_DAY(2),
        L_TEXT(3).

   DATE = P_BEGDATE .
   CLEAR : L_COUNT,
           IT_WEEKS, IT_WEEKS[].

   DO 24 TIMES.
      L_COUNT = L_COUNT + 1.
      L_DAY   = L_COUNT.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         input  = L_DAY
       IMPORTING
         OUTPUT = L_DAY.
      CONCATENATE 'W' L_DAY INTO L_TEXT.

      IT_WEEKS-WSEQ  = L_TEXT.


*     GET THE WEEK
      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          DATE            =  DATE
        IMPORTING
          WEEK            = WEEK
          MONDAY          = MONDAY
          SUNDAY          = SUNDAY .

      IT_WEEKS-WEEK = WEEK.

      IF L_COUNT = 1.
        IT_WEEKS-DATE1 = DATE .
      ELSE.
        IT_WEEKS-DATE1 = MONDAY.
      ENDIF.

      IT_WEEKS-DATE2   = SUNDAY.
      APPEND IT_WEEKS.
      DATE = DATE + 7.
   ENDDO.

endform.                    " MAKE_21_WEEKS
*&---------------------------------------------------------------------*
*&      Form  MRP_LASTDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MDSU  text
*----------------------------------------------------------------------*
form MRP_LASTDATE TABLES  pt_mdsu STRUCTURE MDSU.
   DATA: L_DATE LIKE MDSU-DAT00.
   DATA: L_MRP_MOST LIKE MDSU-DAT00.

   L_DATE = SY-DATUM.
   L_MRP_MOST = SY-DATUM + 20." 21 DAYS IS THE MAXIMUM MRP DAYS

   LOOP AT PT_MDSU WHERE MNG02 NE 0   AND
                  DAT00 GE SY-DATUM   AND
                  DAT00 LE L_MRP_MOST.
     IF PT_MDSU-DAT00 GT L_DATE.
      L_DATE = PT_MDSU-DAT00.
     ENDIF.
   ENDLOOP.
*  COMPARING THE MRP LAST DATE OF MATERIALS
   IF L_DATE LT L_MRP_LASTDATE.
     L_MRP_LASTDATE = L_DATE. "EARLIEST MRP LAST DATE
   ENDIF.

*  CURRENT MATERIAL LTP BEGIN DATE
   G_LTP_BEGDATE  = L_DATE + 1.
*  SAVE THE CURRENT MATERIAL LTP BEGIN DATE
   IT_LTP_BEG-MATNR = IT_REQ_A-MATNR.
   IT_LTP_BEG-BEGDT = L_DATE.
   APPEND IT_LTP_BEG.
endform.                    " MRP_LASTDATE
*&---------------------------------------------------------------------*
*&      Form  RE_SUM_24_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form RE_SUM_24_WEEKS.
  DATA: L_SOUR TYPE I.
  DATA: L_CC(2).
  DATA: L_STXT(16).
  FIELD-SYMBOLS  <SOUR>.


*
* CHECK HOM MANY WEEKS DATA SHOULD BE DISPLAYED
  G_DISPLAY_END_WEEK = 1.
  G_DISPLAY_BEGIN_WEEK = 24.
  LOOP AT IT_21LAYOUT.

   L_SOUR = 1.

   DO 24 TIMES.
     L_CC = L_SOUR.
     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         INPUT     =  L_CC
       IMPORTING
         OUTPUT    =  L_CC.
     CONCATENATE 'IT_21LAYOUT-W' L_CC INTO L_STXT.
     ASSIGN (L_STXT) TO <SOUR>.

*    DECIDE THE BEGIN WEEK AND END WEEK THAT WILL BE DISPLAYED
     IF <SOUR> NE 0.
      PERFORM GET_BIGGER USING L_SOUR.
     ENDIF.
     L_SOUR = L_SOUR + 1.  " CHECK NEXT WEEK
   ENDDO.

 ENDLOOP.

* CHECK IF NONE ZERO WEEKS IS MORE THAT 21
  IF G_DISPLAY_BEGIN_WEEK GE 4.
     G_DISPLAY_BEGIN_WEEK = 4.
     G_DISPLAY_END_WEEK = 24.
  ENDIF.

  IF G_DISPLAY_BEGIN_WEEK LT 4.
     L_SOUR = G_DISPLAY_BEGIN_WEEK + 20.
     IF G_DISPLAY_END_WEEK LT L_SOUR.
       G_DISPLAY_END_WEEK = L_SOUR.
     ENDIF.
  ENDIF.

endform.                    " RE_SUM_24_WEEK
*&---------------------------------------------------------------------*
*&      Form  GET_BIGGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_BIGGER USING P_SOUR.

  IF G_DISPLAY_END_WEEK LT P_SOUR.
    G_DISPLAY_END_WEEK = P_SOUR.
  ENDIF.
  IF G_DISPLAY_BEGIN_WEEK GT P_SOUR.
    G_DISPLAY_BEGIN_WEEK = P_SOUR.
  ENDIF.

endform.                    " GET_BIGGER
*&---------------------------------------------------------------------*
*&      Form  GET_LTP_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_LTP_INDICATOR.
  IF P_21 = 'X' OR
     P_DATE-HIGH GE G_LTP_BEGDATE.
    G_LTP = 'X'.
  ELSE.
    CLEAR G_LTP.
  ENDIF.
endform.                    " GET_LTP_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  COMPRESS_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form COMPRESS_DETAIL.

  DATA: LT_DETAIL TYPE STANDARD TABLE OF DETAIL.
  DATA: L_TABIX LIKE SY-TABIX.
  DATA: WA_DET  LIKE WA_DETAIL.
  DATA: WA_DET1 LIKE WA_DETAIL.

  CLEAR: WA_DET.

  LOOP AT IT_DETAIL INTO WA_DETAIL.
     L_TABIX = SY-TABIX + 1.
     IF WA_DETAIL-ELEMT = 'STOCK' OR
        WA_DETAIL-ELEMT = 'SAFTY STOCK'.
       APPEND WA_DETAIL TO LT_DETAIL.
       CONTINUE.
     ENDIF.
     WA_DET-DAT00 = WA_DETAIL-DAT00.
     WA_DET-MNG02 = WA_DETAIL-MNG02 + WA_DET-MNG02.
     WA_DET-MNG03 = WA_DETAIL-MNG03 + WA_DET-MNG03.
     WA_DET-MNG04 = WA_DETAIL-MNG04.

     CLEAR: WA_DET1.
     READ TABLE IT_DETAIL INTO WA_DET1 INDEX L_TABIX.
     IF WA_DETAIL-DAT00 = WA_DET1-DAT00.
*       IF NOT WA_DET-DAT00 IS INITIAL.
*          IF WA_DETAIL-DAT00 = WA_DET-DAT00 .
       CONTINUE.
     ELSE.
        APPEND WA_DET TO LT_DETAIL.

        IF P_21 = 'X'.
           MOVE-CORRESPONDING WA_DET TO IT_21.
           IT_21-MATNR = IT_REQ_A-MATNR.
           APPEND IT_21 TO IT_21.  "SAVE FOR 21+21 REPORT
        ENDIF.

        CLEAR WA_DET.

     ENDIF.

  ENDLOOP.

  IF G_LTP = 'X'.
    IT_DETAIL_LTP[] = LT_DETAIL[].
  ELSE.
    IT_DETAIL_MRP[] = LT_DETAIL[].
  ENDIF.
    CLEAR IT_DETAIL[].
endform.                    " COMPRESS_DETAIL
*&---------------------------------------------------------------------*
*&      Form  SUM_MRP_LTP_DETAIL
*&---------------------------------------------------------------------*
*       SUM THE MRP AND LTP DETAIL INTO ONE INTERNAL TABLE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form SUM_MRP_LTP_DETAIL.
  DATA: WA_DET LIKE WA_DETAIL.
  DATA: WA_DET1 LIKE WA_DETAIL.
  DATA: LT_DET TYPE STANDARD TABLE OF DETAIL.
  DATA: L_TABIX LIKE SY-TABIX.
  DATA: L_AVAIL LIKE MDSU-MNG02.

  CLEAR: WA_DETAIL, IT_DETAIL[].

* SAVE STOCK RECORD
  READ TABLE IT_DETAIL_MRP INTO WA_DET WITH KEY ELEMT = 'STOCK'.
  APPEND WA_DET TO IT_DETAIL.
    L_AVAIL = WA_DET-MNG04.
  CLEAR: WA_DET.

* SAVE SAFTY STOCK IF EXIST
  READ TABLE IT_DETAIL_MRP INTO WA_DET WITH KEY ELEMT = 'SAFTY STOCK'.
  IF SY-SUBRC EQ 0.
   APPEND WA_DET TO IT_DETAIL.
   L_AVAIL = L_AVAIL + WA_DET-MNG02.
  ENDIF.
  CLEAR: WA_DET.


  APPEND LINES OF IT_DETAIL_MRP TO LT_DET.
  APPEND LINES OF IT_DETAIL_LTP TO LT_DET.

  DELETE LT_DET WHERE ELEMT = 'STOCK' OR
                      ELEMT = 'SAFTY STOCK'  .

  SORT LT_DET BY DAT00.

  LOOP AT LT_DET INTO WA_DETAIL.
    L_TABIX = SY-TABIX + 1.
    WA_DET1-DAT00 = WA_DETAIL-DAT00.
    WA_DET1-MNG02 = WA_DET1-MNG02 + WA_DETAIL-MNG02.
    WA_DET1-MNG03 = WA_DET1-MNG03 + WA_DETAIL-MNG03.
    L_AVAIL = L_AVAIL + WA_DETAIL-MNG02 + WA_DETAIL-MNG03.

    CLEAR: WA_DET.
    READ TABLE LT_DET INTO WA_DET INDEX L_TABIX.
    IF WA_DETAIL-DAT00 NE WA_DET-DAT00.
      WA_DET1-MNG04 = L_AVAIL.
      APPEND WA_DET1 TO IT_DETAIL.
      CLEAR: WA_DET1.
    ENDIF.

  ENDLOOP.
* IN MRP THE SAFETY STOCK SHOULD BE A REQUIREMENT.
* PUT THE SAFTY STOCK INTO THE SAME DAY REQUIRMENT
  CLEAR: WA_DET.
  READ TABLE IT_DETAIL_MRP INTO WA_DET WITH KEY ELEMT = 'SAFTY STOCK'.
  IF SY-SUBRC EQ 0.
   DELETE IT_DETAIL_MRP WHERE ELEMT = 'SAFTY STOCK'.
   CLEAR WA_DET-ELEMT.
   COLLECT WA_DET INTO IT_DETAIL_MRP.
  ENDIF.

endform.                    " SUM_MRP_LTP_DETAIL
