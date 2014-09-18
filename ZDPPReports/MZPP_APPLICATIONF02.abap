*----------------------------------------------------------------------*
***INCLUDE MZPP_APPLICATIONF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  download_1001
*&---------------------------------------------------------------------*
FORM DOWNLOAD_1001.
  DATA: L_COUNT               TYPE I,
        BEGIN OF IT_EXCEL1001          OCCURS 0,
          NO(50)              TYPE C,
          219CODE(50)         TYPE C,
          219DESC(50)         TYPE C,
          219VALS(50)         TYPE C,
        END OF IT_EXCEL1001 .

  LOOP AT IT_219.
    MOVE-CORRESPONDING IT_219 TO IT_EXCEL1001.
    APPEND IT_EXCEL1001.
  ENDLOOP.
  DESCRIBE TABLE IT_219 LINES L_COUNT.

  CLEAR: IT_EXCEL1001.
  IT_EXCEL1001-NO      = 'FILENMAE   '  .
  IT_EXCEL1001-219CODE = 'HkpsP101.xls' .
  INSERT       IT_EXCEL1001 INDEX 1     .
  IT_EXCEL1001-NO      = 'TITLE      '  .
  IT_EXCEL1001-219CODE = 'ORDER BASICs' .
  INSERT       IT_EXCEL1001 INDEX 2     .
  IT_EXCEL1001-NO      = 'CONDITIONS '  .
  CONCATENATE 'Order-No: ' WA_ORDER          INTO  IT_EXCEL1001-219CODE.
  INSERT       IT_EXCEL1001 INDEX 3     .
  IT_EXCEL1001-NO      = 'DATES     '   .
  CONCATENATE SY-DATUM   SY-UZEIT            INTO  IT_EXCEL1001-219CODE.
  INSERT       IT_EXCEL1001 INDEX 4     .
  IT_EXCEL1001-NO      = 'RECORDS   '   .
  IT_EXCEL1001-219CODE = L_COUNT        .
  INSERT       IT_EXCEL1001 INDEX 5     .
  CLEAR: IT_EXCEL1001.
  INSERT       IT_EXCEL1001 INDEX 6     .
  INSERT       IT_EXCEL1001 INDEX 7     .
  INSERT       IT_EXCEL1001 INDEX 8     .
  INSERT       IT_EXCEL1001 INDEX 9     .
  INSERT       IT_EXCEL1001 INDEX 10    .
  INSERT       IT_EXCEL1001 INDEX 11    .
  INSERT       IT_EXCEL1001 INDEX 12    .
  INSERT       IT_EXCEL1001 INDEX 13    .
  INSERT       IT_EXCEL1001 INDEX 14    .
  INSERT       IT_EXCEL1001 INDEX 15    .
  INSERT       IT_EXCEL1001 INDEX 16    .
  INSERT       IT_EXCEL1001 INDEX 17    .
  CLEAR: IT_EXCEL1001.
  INSERT       IT_EXCEL1001 INDEX 18    .
  INSERT       IT_EXCEL1001 INDEX 19    .
  IT_EXCEL1001-NO      = 'COLUMN'       .
  IT_EXCEL1001-219CODE = 'OPTION'       .
  IT_EXCEL1001-219DESC = 'OPTION  NAME' .
  IT_EXCEL1001-219VALS = 'COLUMN NAME'  .
  INSERT       IT_EXCEL1001 INDEX 20    .
  PERFORM GET_WINDOWS_CLIFILE USING    ',*.xls.'
                              CHANGING WA_FILENAME.

* PERFORM EXCEL_DOWNLOAD USING 'IT_EXCEL1001'.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      FILENAME = WA_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EXCEL1001.
ENDFORM.                    " download_1001

*&---------------------------------------------------------------------*
*&      Form  download_1002
*&---------------------------------------------------------------------*
FORM DOWNLOAD_1002.
  DATA: L_COUNT               TYPE I,
        L_DATE(10)            TYPE C,
        L_TIME(8)             TYPE C,
        BEGIN OF IT_EXCEL1002 OCCURS 0,
          EXTC(50)            TYPE C,
          INTC(50)            TYPE C,
          INITQTY(50)         TYPE C,
          MODQTY(50)          TYPE C,
          FIELD01(50)         TYPE C,
          FIELD02(50)         TYPE C,
          FIELD03(50)         TYPE C,
          SEQQTY(50)          TYPE C,
          MITUQTY(50)         TYPE C,
          RP01TQ(50)          TYPE C,
          RP02TQ(50)          TYPE C,
          RP03TQ(50)          TYPE C,
          RP04TQ(50)          TYPE C,
          RP05TQ(50)          TYPE C,
          RP06TQ(50)          TYPE C,
          RP07TQ(50)          TYPE C,
          RP08TQ(50)          TYPE C,
          RP09TQ(50)          TYPE C,
          RP11TQ(50)          TYPE C,
          T_DATE(50)          TYPE C,
          FLAG1(050)          TYPE C,
        END OF IT_EXCEL1002 .

  LOOP AT IT_WOSUM.
    MOVE-CORRESPONDING IT_WOSUM TO IT_EXCEL1002.
    APPEND IT_EXCEL1002.
  ENDLOOP.
  DESCRIBE TABLE IT_WOSUM LINES L_COUNT.

  CLEAR: IT_EXCEL1002.
  IT_EXCEL1002-EXTC    = 'FILENMAE   '  .
  IT_EXCEL1002-INTC    = 'HkpsP103.xls' .
  INSERT       IT_EXCEL1002 INDEX 1     .
  IT_EXCEL1002-EXTC    = 'TITLE      '  .
  IT_EXCEL1002-INTC    = 'ORDER BASICs' .
  INSERT       IT_EXCEL1002 INDEX 2     .
  IT_EXCEL1002-EXTC    = 'CONDITIONS '  .
  CONCATENATE 'Order-No: ' WA_ORDER          INTO  IT_EXCEL1002-INTC   .
  INSERT       IT_EXCEL1002 INDEX 3     .
  IT_EXCEL1002-EXTC    = 'DATES     '   .
  WRITE SY-DATUM           TO L_DATE    .
  WRITE SY-UZEIT           TO L_TIME    .
  CONCATENATE L_DATE L_TIME INTO IT_EXCEL1002-INTC  SEPARATED BY SPACE.
  INSERT       IT_EXCEL1002 INDEX 4     .
  IT_EXCEL1002-EXTC    = 'RECORDS   '   .
  IT_EXCEL1002-INTC    = L_COUNT        .
  INSERT       IT_EXCEL1002 INDEX 5     .
  CLEAR: IT_EXCEL1002.
  INSERT       IT_EXCEL1002 INDEX 6     .
  IT_EXCEL1002-EXTC    = 'EXTERNAL COLOR'.
  IT_EXCEL1002-INTC    = 'INTERNAL COLOR'.
  IT_EXCEL1002-INITQTY = 'INITIAL QUANTITY'.
  IT_EXCEL1002-MODQTY  = 'MODIFY  QUANTITY'.
  IT_EXCEL1002-FIELD01 = 'C1 '             .
  IT_EXCEL1002-FIELD02 = 'C2 '             .
  IT_EXCEL1002-FIELD03 = 'CONFIRM QUANTIRY'.
  IT_EXCEL1002-SEQQTY  = 'SEQUENCE QUANTIRY'.
  IT_EXCEL1002-MITUQTY = 'MITU    QUANTIRY'.
  IT_EXCEL1002-RP01TQ  = '1 '              .
  IT_EXCEL1002-RP02TQ  = '2 '              .
  IT_EXCEL1002-RP03TQ  = '3 '              .
  IT_EXCEL1002-RP04TQ  = '4 '              .
  IT_EXCEL1002-RP05TQ  = '5 '              .
  IT_EXCEL1002-RP06TQ  = '6 '              .
  IT_EXCEL1002-RP07TQ  = '7 '              .
  IT_EXCEL1002-RP08TQ  = '8 '              .
  IT_EXCEL1002-RP09TQ  = '9 '              .
  IT_EXCEL1002-RP11TQ  = '11'              .
  IT_EXCEL1002-T_DATE  = 'TRANSFER DATE'   .
  IT_EXCEL1002-FLAG1   = 'FLAG'            .

  INSERT       IT_EXCEL1002 INDEX 7     .

  PERFORM GET_WINDOWS_CLIFILE USING    ',*.xls.'
                              CHANGING WA_FILENAME.

* PERFORM EXCEL_DOWNLOAD USING 'IT_EXCEL1001'.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      FILENAME = WA_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EXCEL1002.
ENDFORM.                    " download_1002

*&---------------------------------------------------------------------*
*&      Form  download_1003
*&---------------------------------------------------------------------*
FORM DOWNLOAD_1003.
  DATA: L_COUNT               TYPE I,
        L_NO(2)               TYPE N,
        L_DATE(10)            TYPE C,
        L_TIME(8)             TYPE C,
        L_FNAME(50)           TYPE C,
        BEGIN OF IT_EXCEL1003 OCCURS 0,
          NO(50)              TYPE C,
          VALS(50)            TYPE C,
        END OF IT_EXCEL1003 .

  DO 50 TIMES .
    L_NO = L_NO + 1 .
    CONCATENATE 'WA_1003_VAL' L_NO INTO L_FNAME .
    ASSIGN (L_FNAME) TO <FIELD1> .

    IT_EXCEL1003-NO = L_NO .
    IT_EXCEL1003-VALS = <FIELD1> .
    APPEND IT_EXCEL1003.
  ENDDO.
  DESCRIBE TABLE IT_WOSUM LINES L_COUNT.

  CLEAR: IT_EXCEL1003.
  IT_EXCEL1003-NO      = 'FILENMAE   '  .
  IT_EXCEL1003-VALS    = 'HkpsP104.xls' .
  INSERT       IT_EXCEL1003 INDEX 1     .
  IT_EXCEL1003-NO      = 'TITLE      '  .
  IT_EXCEL1003-VALS    = 'ALC COLOR MANAGEMENT - 1' .
  INSERT       IT_EXCEL1003 INDEX 2     .
  IT_EXCEL1003-NO      = 'CONDITIONS '  .
  CONCATENATE 'Order-No: ' WA_ORDER          INTO  IT_EXCEL1003-VALS   .
  INSERT       IT_EXCEL1003 INDEX 3     .    CLEAR: IT_EXCEL1003       .
  CONCATENATE 'Color: '  WA_ECOLOR WA_ICOLOR INTO  IT_EXCEL1003-VALS   .
  INSERT       IT_EXCEL1003 INDEX 4     .
  IT_EXCEL1003-NO      = 'DATES     '   .
  WRITE SY-DATUM           TO L_DATE    .
  WRITE SY-UZEIT           TO L_TIME    .
  CONCATENATE L_DATE L_TIME INTO IT_EXCEL1003-VALS  SEPARATED BY SPACE.
  INSERT       IT_EXCEL1003 INDEX 5     .
  IT_EXCEL1003-NO      = 'RECORDS   '   .
  IT_EXCEL1003-VALS    = '1'            .
  INSERT       IT_EXCEL1003 INDEX 6     .
  CLEAR: IT_EXCEL1003.
  INSERT       IT_EXCEL1003 INDEX 7     .
  IT_EXCEL1003-NO      = 'CLM'          .
  IT_EXCEL1003-VALS    = 'CODE'         .

  INSERT       IT_EXCEL1003 INDEX 8     .

  PERFORM GET_WINDOWS_CLIFILE USING    ',*.xls.'
                              CHANGING WA_FILENAME.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      FILENAME = WA_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EXCEL1003.
ENDFORM.                    " download_1003

*&---------------------------------------------------------------------*
*&      Form  download_1004
*&---------------------------------------------------------------------*
FORM DOWNLOAD_1004.
  DATA: L_COUNT               TYPE I,
        L_NO(2)               TYPE N,
        L_DATE(10)            TYPE C,
        L_TIME(8)             TYPE C,
        L_FNAME(50)           TYPE C,
        BEGIN OF IT_EXCEL1003 OCCURS 0,
          NO(50)              TYPE C,
          VALS(50)            TYPE C,
        END OF IT_EXCEL1003 .

  DO 50 TIMES .
    L_NO = L_NO + 1 .
    CONCATENATE 'WA_1003_VAL' L_NO INTO L_FNAME .
    ASSIGN (L_FNAME) TO <FIELD1> .

    IT_EXCEL1003-NO = L_NO .
    IT_EXCEL1003-VALS = <FIELD1> .
    APPEND IT_EXCEL1003.
  ENDDO.
  DESCRIBE TABLE IT_WOSUM LINES L_COUNT.

  CLEAR: IT_EXCEL1003.
  IT_EXCEL1003-NO      = 'FILENMAE   '  .
  IT_EXCEL1003-VALS    = 'HkpsP104.xls' .
  INSERT       IT_EXCEL1003 INDEX 1     .
  IT_EXCEL1003-NO      = 'TITLE      '  .
  IT_EXCEL1003-VALS    = 'ALC COLOR MANAGEMENT - 1' .
  INSERT       IT_EXCEL1003 INDEX 2     .
  IT_EXCEL1003-NO      = 'CONDITIONS '  .
  CONCATENATE 'Order-No: ' WA_ORDER          INTO  IT_EXCEL1003-VALS   .
  INSERT       IT_EXCEL1003 INDEX 3     .
  IT_EXCEL1003-NO      = 'DATES     '   .
  WRITE SY-DATUM           TO L_DATE    .
  WRITE SY-UZEIT           TO L_TIME    .
  CONCATENATE L_DATE L_TIME INTO IT_EXCEL1003-VALS  SEPARATED BY SPACE.
  INSERT       IT_EXCEL1003 INDEX 4     .
  IT_EXCEL1003-NO      = 'RECORDS   '   .
  IT_EXCEL1003-VALS    = '1'            .
  INSERT       IT_EXCEL1003 INDEX 5     .
  CLEAR: IT_EXCEL1003.
  INSERT       IT_EXCEL1003 INDEX 6     .
  IT_EXCEL1003-NO      = 'CLM'          .
  IT_EXCEL1003-VALS    = 'CODE'         .

  INSERT       IT_EXCEL1003 INDEX 7     .

  PERFORM GET_WINDOWS_CLIFILE USING    ',*.xls.'
                              CHANGING WA_FILENAME.

* PERFORM EXCEL_DOWNLOAD USING 'IT_EXCEL1001'.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      FILENAME = WA_FILENAME
      FILETYPE = 'DAT'
    TABLES
      DATA_TAB = IT_EXCEL1003.
ENDFORM.                    " download_1004

*&---------------------------------------------------------------------*
*&      Form  get_DATA_ERROR
*&---------------------------------------------------------------------*
FORM GET_DATA_ERROR.
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_MARA
    FROM MARA
   WHERE MTART = 'WOHD'
     AND MBRSH = 'A'
     AND KZKFG = SPACE .

  LOOP AT IT_MARA.
    CLEAR: WA_ATWRT.
    SELECT SINGLE ATWRT INTO WA_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_MARA-MATNR
       AND ATINN = WA_ATINN
       AND KLART = '001'        .

    IF WA_ATWRT(1) = 'N'       .
    ELSE.
      DELETE IT_MARA           .
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE IT_MARA LINES WA_LINES.
  IF WA_LINES = 0. WA_ERR_FLAG = 'X'. ENDIF.
ENDFORM.                    " get_DATA_ERROR

*&---------------------------------------------------------------------*
*&      Form  excel_down_3107
*&---------------------------------------------------------------------*
FORM EXCEL_DOWN_3107.

ENDFORM.                    " excel_down_3107

*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_APP244
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP244.
* Plant
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_PLANT'   .
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT .

* Model
*  CLEAR: xlist, xvalue.
*  name = 'WA_MODEL' .
*  PERFORM set_field_model USING name  WA_MODEL.

* Body Serial "P_BODY_SERIAL
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_BODYNO_APP244'.
*  PERFORM set_field_bodyno .
*  PERFORM call_function_vrm   USING xlist.

* Line
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_LINE_APP244'.
*  PERFORM set_field_line.
*  PERFORM call_function_vrm   USING xlist.
* Progress
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_PROG_APP244'.
  PERFORM SET_FIELD_PROG.
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* Work Order
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_WONO_APP244'.
*  PERFORM set_field_wono_app244.
*  PERFORM call_function_vrm   USING xlist.
** External Color
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_EXTC_APP244'.
*  PERFORM set_field_extc.
*  PERFORM call_function_vrm   USING xlist.
** Internal Color
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_INTC_APP244'.
*  PERFORM set_field_intc.
*  PERFORM call_function_vrm   USING xlist.
* Columnes
  PERFORM SET_COLUMNS   .
ENDFORM.                    " make_dropdown_list_box_APP244

*&---------------------------------------------------------------------*
*&      Form  set_field_PROG_APP244
*&---------------------------------------------------------------------*
FORM SET_FIELD_PROG .
* Sequence(00)
  XVALUE-KEY  = '00'.
  XVALUE-TEXT = '(00)    SEQ' .
  APPEND XVALUE TO XLIST.
* B/IN(01)
  XVALUE-KEY  = '01'.
  XVALUE-TEXT = '(01)    Body'.
  APPEND XVALUE TO XLIST.
* P/IN(02)
  XVALUE-KEY  = '02'.
  XVALUE-TEXT = '(02)    Paint/IN'.
  APPEND XVALUE TO XLIST.
* T/C(03)
  XVALUE-KEY  = '03'.
  XVALUE-TEXT = '(03)    T/Coat'.
  APPEND XVALUE TO XLIST.
* P/OUT(04)
  XVALUE-KEY  = '04'.
  XVALUE-TEXT = '(04)    Paint/Out'.
  APPEND XVALUE TO XLIST.
* PBS/I(05)
  XVALUE-KEY  = '05'.
  XVALUE-TEXT = '(05)    PBS/In'.
  APPEND XVALUE TO XLIST.
* PBS/OUT(06)
  XVALUE-KEY  = '06'.
  XVALUE-TEXT = '(06)    PBS/Out'.
  APPEND XVALUE TO XLIST.
* T/IN 1(07)
  XVALUE-KEY  = '07'.
  XVALUE-TEXT = '(07)    Trim 1'.
  APPEND XVALUE TO XLIST.
* T/IN 2(08)
  XVALUE-KEY  = '08'.
  XVALUE-TEXT = '(08)    Trim 2'.
  APPEND XVALUE TO XLIST.
* T/IN 3(09)
  XVALUE-KEY  = '09'.
  XVALUE-TEXT = '(09)    Trim 3'.
  APPEND XVALUE TO XLIST.
* CHASSIS 1 INPUT(10)
  XVALUE-KEY  = '10'.
  XVALUE-TEXT = '(10)    Chassis 1'.
  APPEND XVALUE TO XLIST.
* CHASSIS 1 INPUT(11)
  XVALUE-KEY  = '11'.
  XVALUE-TEXT = '(11)    Chassis 2'.
  APPEND XVALUE TO XLIST.
* FINAL 1 (12)
  XVALUE-KEY  = '12'.
  XVALUE-TEXT = '(12)    Final 1'.
  APPEND XVALUE TO XLIST.
* CHASSIS 2 INPUT(13)
  XVALUE-KEY  = '13'.
  XVALUE-TEXT = '(13)    Final 2'.
  APPEND XVALUE TO XLIST.
* CHASSIS 3 INPUT(14)
  XVALUE-KEY  = '14'.
  XVALUE-TEXT = '(14)    Final 3'.
  APPEND XVALUE TO XLIST.
* CHASSIS 4 INPUT(15)
  XVALUE-KEY  = '15'.
  XVALUE-TEXT = '(15)    Final 4'.
  APPEND XVALUE TO XLIST.
* CHASSIS 5 INPUT(16)
  XVALUE-KEY  = '16'.
  XVALUE-TEXT = '(16)    Final 5'.
  APPEND XVALUE TO XLIST.
* C/F(17)
  XVALUE-KEY  = '17'.
  XVALUE-TEXT = '(17)    Line Off'.
  APPEND XVALUE TO XLIST.
* S/OFF(18)
  XVALUE-KEY  = '18'.
  XVALUE-TEXT = '(18)    Sign/Off'.
  APPEND XVALUE TO XLIST.
* C/GATE(19)
  XVALUE-KEY  = '19'.
  XVALUE-TEXT = '(19)    C/Gate'.
  APPEND XVALUE TO XLIST.
* Under body coating20)
  XVALUE-KEY  = '20'.
  XVALUE-TEXT = '(20)    Coating'.
  APPEND XVALUE TO XLIST.
* Dealer Alloc
  XVALUE-KEY  = '21'.
  XVALUE-TEXT = '(21)    Dealer Alloc'.
  APPEND XVALUE TO XLIST.

* VPC/I(21)
  XVALUE-KEY  = '22'.
  XVALUE-TEXT = '(22)    VPC/In'.
  APPEND XVALUE TO XLIST.
* VPC/O(22)
  XVALUE-KEY  = '23'.
  XVALUE-TEXT = '(23)    VPC/Out'.
  APPEND XVALUE TO XLIST.
* SHIP/IN(24)
  XVALUE-KEY  = '24'.
  XVALUE-TEXT = '(24,26) Ship/In'.
  APPEND XVALUE TO XLIST.
* SHIP/OUT(25)
  XVALUE-KEY  = '25'.
  XVALUE-TEXT = '(25,27) Ship/Out'.
  APPEND XVALUE TO XLIST.

ENDFORM.                    " set_field_PROG

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP244
*&---------------------------------------------------------------------*
FORM SET_FIELD_WONO_APP244.
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_WORK_ORDER' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " SET_FIELD_WONO_APP244

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INTC_APP244
*&---------------------------------------------------------------------*
FORM SET_FIELD_INTC .
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_INT_COLOR' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " SET_FIELD_INTC

*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
FORM SEARCH_DATA_APP244.
  DATA: L_ERROR ,
        L_TEXT(60) .
  CLEAR L_ERROR.
  PERFORM SET_PARAMETER_FOR_APP244 USING L_ERROR L_TEXT.
  IF L_ERROR <> SPACE.
    CONCATENATE 'Enter The Parameters -' L_TEXT  INTO L_TEXT.
    MESSAGE I000 WITH L_TEXT.
    EXIT.
  ENDIF.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_APP244, IT_APP244[].
* There are Two Selection Options.
  PERFORM GET_VEHICLE_MASTER_NO_APP244 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_APP244 .
ENDFORM.                    " search_data

*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
FORM SET_PARAMETER_FOR_APP244 USING P_ERROR P_TEXT .
* Production Date
  IF P_PROD_DATE_APP244 IS INITIAL .
    P_ERROR = 'X'.
    P_TEXT = 'Prod. Date'.
    EXIT.
  ENDIF.

* Progress
  IF P_PROG_APP244 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'Progress'.
    EXIT.
  ENDIF.
ENDFORM.                    " SET_PARAMETER_FOR_SRCHNG_DATA

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_NO_APP244 TABLES P_IT_OBJEK STRUCTURE IT_OBJEK .
  DATA: L_SUBRC    TYPE SY-SUBRC ,
        L_ATNAM    TYPE CABN-ATNAM,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_NAME     TYPE CABN-ATNAM,  "Prod. Date
        L_NAME1    TYPE CABN-ATNAM,
        L_ATFLV    TYPE AUSP-ATFLV,  "Prod. Date
        L_ATFLV_ST TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08)  TYPE N,
        L_EXRP(02) TYPE N.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK    LIKE AUSP-OBJEK,
          ATWRT    LIKE AUSP-ATWRT,
        END OF LT_OBJEK.

  CONCATENATE 'P_RP' P_PROG_APP244 '_SHOP_DATE'  INTO L_NAME .
  IF P_PROG_APP244 = '24'.
    CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_NAME1 .
    L_EXRP  = '26'.
  ELSEIF P_PROG_APP244 = '25'.
    CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_NAME1 .
    L_EXRP  = '27'.
  ELSE.
    L_NAME1 = L_NAME.
  ENDIF.

  L_ATFLV = L_NUM = P_PROD_DATE_APP244 .

  REFRESH LT_OBJEK.
  SELECT DISTINCT OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE KLART = '002' AND
          AU~ATFLV = L_ATFLV AND
          CA~ATNAM = L_NAME .
* save the reporting point
  LT_OBJEK-ATWRT = P_PROG_APP244.
  MODIFY LT_OBJEK TRANSPORTING ATWRT WHERE ATWRT = SPACE.

  IF P_PROG_APP244 = '24' OR
     P_PROG_APP244 = '25'.
    SELECT DISTINCT OBJEK
      APPENDING TABLE LT_OBJEK
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE KLART = '002' AND
            AU~ATFLV = L_ATFLV AND
            CA~ATNAM = L_NAME1 .

* save the reporting point.
    LT_OBJEK-ATWRT = L_EXRP.
    MODIFY LT_OBJEK TRANSPORTING ATWRT WHERE ATWRT = SPACE.

  ENDIF.

  LOOP AT LT_OBJEK.
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.
    IT_OBJEK-ATWRT = LT_OBJEK-ATWRT.
**    P_WONO_APP244,       "P_WORK_ORDER
    IF P_WONO_APP244 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_WONO_APP244 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP244 USING    IT_OBJEK-OBJEK
                                      'P_WORK_ORDER'
                                      L_ATWRT
                             CHANGING L_SUBRC .
      IF L_SUBRC <> 0 .
        CONTINUE.
      ENDIF.
    ENDIF.
**    P_EXTC_APP244,       "P_EXT_COLOR
    IF P_EXTC_APP244 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_EXTC_APP244 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP244 USING IT_OBJEK-OBJEK
                                     'P_EXT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0 .
        CONTINUE.
      ENDIF.
    ENDIF.
**    P_PLAN_ORDER

**    P_INTC_APP244.       "P_INT_COLOR
    IF P_INTC_APP244 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_INTC_APP244 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP244 USING IT_OBJEK-OBJEK
                                     'P_INT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0 .
        CONTINUE.
      ENDIF.
    ENDIF.
**    p_column01 ~ 10  "P_219_xxx
    PERFORM CHECK_219_CODE        USING    IT_OBJEK-OBJEK
                           CHANGING L_SUBRC .
    IF L_SUBRC <> 0.
      CONTINUE.
    ENDIF.
    APPEND IT_OBJEK.

  ENDLOOP.
  SORT IT_OBJEK BY OBJEK .
ENDFORM.                    " get_vehicle_master_no

*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
FORM CREATE_DATA_APP244.
  DATA: L_RPNO(02) TYPE N          ,
        L_ATNAM    TYPE CABN-ATNAM ,
        L_ATWRT    TYPE AUSP-ATWRT .
  CLEAR: IT_APP244, IT_APP244[].
  LOOP AT IT_OBJEK.
    CLEAR IT_APP244.
*   V/M No.
    MOVE-CORRESPONDING IT_OBJEK TO IT_APP244.
*   Model
    PERFORM READ_NORMAL_CLASS_APP244 USING IT_APP244-OBJEK
                                             'P_MODEL'
                                       CHANGING IT_APP244-MODEL .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM READ_NORMAL_CLASS_APP244 USING IT_APP244-OBJEK
                                             'P_BODY_SERIAL'
                                       CHANGING IT_APP244-BODYNO .
    CONCATENATE IT_APP244-MODEL IT_APP244-BODYNO
      INTO IT_APP244-BODYNO .
*   vin TYPE ausp-atwrt,                                "P_VIN(17)
    PERFORM READ_NORMAL_CLASS_APP244 USING IT_APP244-OBJEK
                                             'P_VIN'
                                       CHANGING IT_APP244-VIN .
*   vendor(10),    "Not Defined
*   mi TYPE ausp-atwrt,                                 "P_MI (07)
    PERFORM READ_NORMAL_CLASS_APP244 USING IT_APP244-OBJEK
                                             'P_MI'
                                       CHANGING IT_APP244-MI .
*   ocn TYPE ausp-atwrt,                                "P_OCN (04)
    PERFORM READ_NORMAL_CLASS_APP244 USING IT_APP244-OBJEK
                                             'P_OCN'
                                       CHANGING IT_APP244-OCN .
*   ver TYPE ausp-atwrt,  "P_VERSION(03)
    PERFORM READ_NORMAL_CLASS_APP244 USING IT_APP244-OBJEK
                                             'P_VERSION'
                                       CHANGING IT_APP244-VER .
*   serial TYPE ausp-atwrt,  "P_RPxx_SERIAL(06)
    L_RPNO = IT_OBJEK-ATWRT .  "p_prog_app244 .
    CONCATENATE 'P_RP' L_RPNO '_SERIAL'
      INTO L_ATNAM.
    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                L_ATNAM
                                       CHANGING IT_APP244-SERIAL .
*   Work Order(Serial)
    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_APP244-WONO.
*   External Color
    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                'P_EXT_COLOR'
                                        CHANGING IT_APP244-EXTC.
*   plan order--changed by Chris 03/04/2005

    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                'P_PLAN_ORDER'
                                        CHANGING IT_APP244-VENDOR.

*   Internal Color
    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_APP244-INTC.
*   prog TYPE ausp-atwrt,      "P_STATUS
    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                'P_RP_STATUS'
                                       CHANGING IT_APP244-PROG .
**  Date : P_RPxx_ACTUAL_DATE.
    L_RPNO = IT_OBJEK-ATWRT .   "p_prog_app244 .
    CONCATENATE 'P_RP' L_RPNO '_ACTUAL_DATE'
      INTO L_ATNAM .
    PERFORM READ_NORMAL_CLASS_APP244 USING    IT_APP244-OBJEK
                                                L_ATNAM
                                       CHANGING L_ATWRT .
    IT_APP244-ACT_DATE = L_ATWRT+00(08).
    IT_APP244-ACT_TIME = L_ATWRT+08(06).
    APPEND IT_APP244.
  ENDLOOP.

  SORT IT_APP244 BY BODYNO .
  DESCRIBE TABLE IT_APP244 LINES P_TOTAL_APP244.
ENDFORM.                    " create_data

*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
FORM READ_NORMAL_CLASS_APP244 USING    P_VMNO  P_CHAR
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
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
FORM CHECK_DATA_OF_VM_APP244 USING    P_VMNO P_CHAR P_VALUE
                      CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO         AND
          KLART = '002'          AND
          AU~ATWRT = P_VALUE     AND
          CA~ATNAM = P_CHAR      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_data_of_vm

*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
FORM DOWNLOAD_DATA_APP244.
  CLEAR: IT_EXCEL_APP244, IT_EXCEL_APP244[].
  PERFORM SET_HEADER_APP244         TABLES IT_EXCEL_APP244.
  PERFORM SET_BODY_APP244           TABLES IT_EXCEL_APP244.
  PERFORM CALL_FUNC_DOWNLOAD_APP244 TABLES IT_EXCEL_APP244.
ENDFORM.                    " download_data

*&---------------------------------------------------------------------*
*&      Form  set_header
*&---------------------------------------------------------------------*
FORM SET_HEADER_APP244 TABLES   P_IT_EXCEL STRUCTURE IT_EXCEL_APP244.
  WRITE:  'Body No.' TO  P_IT_EXCEL-BODYNO  ,
          'V.I.N.' TO  P_IT_EXCEL-VIN  ,
          'Order-No.' TO  P_IT_EXCEL-WONO  ,
          'External Color' TO  P_IT_EXCEL-EXTC  ,
          'Internal Color' TO  P_IT_EXCEL-INTC ,
          'Vendor' TO  P_IT_EXCEL-VENDOR  ,    "Not Defined
          'Model Index' TO  P_IT_EXCEL-MI  ,
          'OCN' TO  P_IT_EXCEL-OCN  ,
          'Version' TO  P_IT_EXCEL-VER  ,
          'Reporting Date' TO  P_IT_EXCEL-ACT_DATE  ,
          'Reporting Time' TO  P_IT_EXCEL-ACT_TIME ,
          'Present Progress' TO  P_IT_EXCEL-PROG  ,
          'Serial' TO  P_IT_EXCEL-SERIAL  .
  APPEND P_IT_EXCEL.
ENDFORM.                    " set_header

*&---------------------------------------------------------------------*
*&      Form  set_body
*&---------------------------------------------------------------------*
FORM SET_BODY_APP244 TABLES   P_IT STRUCTURE IT_EXCEL_APP244 .
  LOOP AT IT_APP244.
    CLEAR P_IT.
    MOVE-CORRESPONDING IT_APP244 TO P_IT.
    APPEND P_IT.
  ENDLOOP.
ENDFORM.                    " set_body

*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
FORM CALL_FUNC_DOWNLOAD_APP244 TABLES   P_IT STRUCTURE IT_EXCEL_APP244.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'VM Spec Per Each Progress.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = P_IT
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.
ENDFORM.                    " call_func_download

*&---------------------------------------------------------------------*
*&      Form  sort_screen_2115
*&---------------------------------------------------------------------*
FORM SORT_SCREEN_2115  USING PA_STYPE .
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP244-COLS  INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP244    ASCENDING  BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP244    DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " sort_screen_2115

*&---------------------------------------------------------------------*
*&      Form  check_shop_date
*&---------------------------------------------------------------------*
FORM CHECK_SHOP_DATE_APP244 USING    P_OBJEK  P_ATNAM  P_ATFLV
                     CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_OBJEK AND
          KLART = '002'      AND
          AU~ATFLV = P_ATFLV AND
          CA~ATNAM = P_ATNAM      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_shop_date

*&---------------------------------------------------------------------*
*&      Form  set_columnes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_COLUMNS .
* Column01
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN01'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* COLUMN02
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN02'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* COLUMN03
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN03'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* COLUMN04
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN04'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* COLUMN05
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN05'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* Column06
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN06'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* Column07
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN07'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* Column08
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN08'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* Column09
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN09'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
* Column10
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN10'       .
  PERFORM SET_FIELD_COLUMN  .
  PERFORM CALL_FUNCTION_VRM   USING XLIST.
ENDFORM.                    " set_columnes

*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP240.
* Plant
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_PLANT'     .
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT .

* Model
  CLEAR: XLIST, XLIST[],  XVALUE.
  NAME = 'WA_MODEL'      .
  PERFORM SET_FIELD_MODEL USING NAME  WA_MODEL .

* Line
*  CLEAR: xlist, xlist[],  xvalue.
*  name = 'P_LINE_APP240'.
*  PERFORM set_field_line.
*  PERFORM call_function_vrm   USING xlist.

* Progress
  CLEAR: XLIST, XLIST[],  XVALUE.
  NAME = 'P_PROG_APP240'.
  PERFORM SET_FIELD_PROG.
  PERFORM CALL_FUNCTION_VRM    USING XLIST.

  NAME = 'P_PROG_APP240_H'.
  PERFORM CALL_FUNCTION_VRM    USING XLIST.

* Part - U or C
  CLEAR: XLIST, XLIST[],  XVALUE.
  NAME = 'P_PART_APP240'.
  PERFORM SET_FIELD_PART.
  PERFORM CALL_FUNCTION_VRM    USING XLIST.
* Column
  CLEAR: XLIST, XLIST[],  XVALUE.
  NAME = 'P_COLUMN_APP240'.
  PERFORM SET_FIELD_COLUMN.
  PERFORM CALL_FUNCTION_VRM    USING XLIST.

* Body No.
*  CLEAR:  xlist, xlist[],  xvalue.
*  name = 'P_BODYNO_APP240'.
*  PERFORM set_field_bodyno       .
*  PERFORM call_function_vrm   USING xlist .

* Work Order
*  CLEAR: xlist, xlist[],  xvalue.
*  name = 'P_WONO_APP240'.
*  PERFORM set_field_wono_app240.
*  PERFORM call_function_vrm   USING xlist.
** External Color
*  CLEAR: xlist, xlist[],  xvalue.
*  name = 'P_EXTC_APP240'.
*  PERFORM set_field_extc.
*  PERFORM call_function_vrm   USING xlist.
** Internal Color
*  CLEAR: xlist, xlist[],  xvalue.
*  name = 'P_INTC_APP240'.
*  PERFORM set_field_intc.
*  PERFORM call_function_vrm   USING xlist.
ENDFORM.                    " make_dropdown_list_box_app240

*&---------------------------------------------------------------------*
*&      Form  set_field_line_app240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_LINE .
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_TRIM_LINE_NO' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " set_field_line

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_COLUMN_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_COLUMN .
  DATA: L_COUNT(03).
  DO 219 TIMES.
    XVALUE-KEY = L_COUNT = L_COUNT + 1.
    WRITE XVALUE-KEY TO XVALUE-KEY LEFT-JUSTIFIED .
    APPEND XVALUE TO XLIST .
  ENDDO.
ENDFORM.                    " SET_FIELD_COLUMN

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_WONO_APP240.
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_WORK_ORDER' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.

ENDFORM.                    " SET_FIELD_WONO_APP240
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_EXTC_APP240
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_EXTC     .
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_EXT_COLOR' .
    CLEAR XLIST.
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " SET_FIELD_EXTC

*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP240.
  DATA: L_ERROR ,
        L_TEXT(40).
  CLEAR L_ERROR.
  CLEAR: R_PLANT_APP240,  R_PLANT_APP240[],
         R_MODEL_APP240,  R_MODEL_APP240[],
         R_LINE_APP240,   R_LINE_APP240[],
         R_PROG_APP240,   R_PROG_APP240[],
         R_PART_APP240,   R_PART_APP240[],
         R_COLUMN_APP240, R_COLUMN_APP240[],
         R_BODYNO_APP240, R_BODYNO_APP240[],
         R_WONO_APP240,   R_WONO_APP240[],
         R_EXTC_APP240,   R_EXTC_APP240[],
         R_INTC_APP240,   R_INTC_APP240[].
  PERFORM SET_PARAMETER_FOR_APP240 USING L_ERROR
                                         L_TEXT.
  IF L_ERROR <> SPACE.
    CONCATENATE 'Set The Parameter -'
                L_TEXT
      INTO L_TEXT SEPARATED BY SPACE.
    MESSAGE I000 WITH L_TEXT.
*    message i000 with 'Enter The Necessary Parameters!!!'.
    EXIT.
  ENDIF.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_APP240, IT_APP240[].
  PERFORM GET_VEHICLE_MASTER_NO_APP240 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_APP240 .
ENDFORM.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_FOR_APP240 USING P_ERROR
                                    P_TEXT.
* Plant
  IF WA_PLANT       <> SPACE.
    R_PLANT_APP240-OPTION = 'EQ'.
    R_PLANT_APP240-SIGN   = 'I'.
    R_PLANT_APP240-LOW    = WA_PLANT      .
    APPEND R_PLANT_APP240.
  ELSE.
    CLEAR: R_PLANT_APP240, R_PLANT_APP240[].
  ENDIF.

* Model
  IF WA_MODEL <> SPACE.
    R_MODEL_APP240-OPTION = 'EQ'.
    R_MODEL_APP240-SIGN   = 'I'.
    R_MODEL_APP240-LOW    = WA_MODEL .
    APPEND R_MODEL_APP240.
  ELSE.
    CLEAR: R_MODEL_APP240, R_MODEL_APP240[].
  ENDIF.

* Line
  IF P_LINE_APP240 <> SPACE.
    R_LINE_APP240-OPTION = 'EQ'.
    R_LINE_APP240-SIGN   = 'I'.
    R_LINE_APP240-LOW    = P_LINE_APP240.
    APPEND R_LINE_APP240.
  ELSE.
    CLEAR: R_LINE_APP240, R_LINE_APP240[].
  ENDIF.

* Progress
* Changed by Furong on 01/07/2006, add RP range
  CLEAR: R_PROG_APP240, R_PROG_APP240[].
  IF P_PROG_APP240 IS INITIAL AND P_PROG_APP240_H IS INITIAL.
  ELSE.
    IF P_PROG_APP240 IS INITIAL.
      R_PROG_APP240-SIGN = 'I'.
      R_PROG_APP240-OPTION = 'BT'.
      R_PROG_APP240-HIGH = P_PROG_APP240_H.
    ELSE.
      IF P_PROG_APP240_H IS INITIAL.
        R_PROG_APP240-SIGN = 'I'.
        R_PROG_APP240-OPTION = 'EQ'.
        R_PROG_APP240-LOW = P_PROG_APP240.
      ELSE.
        R_PROG_APP240-SIGN = 'I'.
        R_PROG_APP240-OPTION = 'BT'.
        R_PROG_APP240-LOW = P_PROG_APP240.
        R_PROG_APP240-HIGH = P_PROG_APP240_H.
      ENDIF.
    ENDIF.
    APPEND R_PROG_APP240.
  ENDIF.
*  IF p_prog_app240 <> space.
*    r_prog_app240-option = 'EQ'.
*    r_prog_app240-sign   = 'I'.
*    r_prog_app240-low    = p_prog_app240.
*    APPEND r_prog_app240.
*  ELSE.
*    CLEAR: r_prog_app240, r_prog_app240[].
*    p_text = 'Progress'.
*    p_error = 'X'.
*    EXIT.
*  ENDIF.
** end of change

* Part
  IF P_PART_APP240 <> SPACE.
    R_PART_APP240-OPTION = 'EQ'.
    R_PART_APP240-SIGN   = 'I'.
    R_PART_APP240-LOW    = P_PART_APP240.
    APPEND R_PART_APP240 .
  ELSE.
    CLEAR: R_PART_APP240, R_PART_APP240[].
    P_TEXT = 'Part'.
    P_ERROR = 'X'.
    EXIT.
  ENDIF.

* Column
  IF P_COLUMN_APP240 <> SPACE.
    R_COLUMN_APP240-OPTION = 'EQ'.
    R_COLUMN_APP240-SIGN   = 'I'.
    R_COLUMN_APP240-LOW    = P_COLUMN_APP240.
    APPEND R_COLUMN_APP240.
  ELSE.
    CLEAR: R_COLUMN_APP240, R_COLUMN_APP240[].
    P_TEXT = 'Column'.
    P_ERROR = 'X'.
    EXIT.
  ENDIF.

* Body Serial
  IF P_BODYNO_APP240 <> SPACE.
    R_BODYNO_APP240-OPTION = 'EQ'.
    R_BODYNO_APP240-SIGN   = 'I'.
    R_BODYNO_APP240-LOW    = P_BODYNO_APP240.
    APPEND R_BODYNO_APP240.
  ELSE.
    CLEAR: R_BODYNO_APP240, R_BODYNO_APP240[].
  ENDIF.

* Work Order
  IF P_WONO_APP240 <> SPACE.
    R_WONO_APP240-OPTION = 'EQ'.
    R_WONO_APP240-SIGN   = 'I'.
    R_WONO_APP240-LOW    = P_WONO_APP240.
    APPEND R_WONO_APP240.
  ELSE.
    CLEAR: R_WONO_APP240, R_WONO_APP240[].
  ENDIF.

* External Color
  IF P_EXTC_APP240 <> SPACE.
    R_EXTC_APP240-OPTION = 'EQ'.
    R_EXTC_APP240-SIGN   = 'I'.
    R_EXTC_APP240-LOW    = P_EXTC_APP240.
    APPEND R_EXTC_APP240.
  ELSE.
    CLEAR: R_EXTC_APP240, R_EXTC_APP240[].
  ENDIF.

* Internal Color
  IF P_INTC_APP240 <> SPACE.
    R_INTC_APP240-OPTION = 'EQ'.
    R_INTC_APP240-SIGN   = 'I'.
    R_INTC_APP240-LOW    = P_INTC_APP240.
  ELSE.
    CLEAR: R_INTC_APP240, R_INTC_APP240[].
  ENDIF.

ENDFORM.                    " SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_NO_APP240 TABLES P_IT_OBJEK STRUCTURE IT_OBJEK .
  DATA: LT_OBJEK   LIKE TABLE OF IT_OBJEK              WITH HEADER LINE,
        L_SUBRC TYPE SY-SUBRC ,
        L_ATINN    TYPE AUSP-ATINN,
        L_ATWRT TYPE AUSP-ATWRT,
        L_ATNAM TYPE CABN-ATNAM.
* Reporting Point's Shop Date
* There are two cases.
* one is that RP = '00', the other is That RP = '01' ~ '18'.

** R_PROG_APP240 FOR P_PROG_APP240,       "P_RP_STATUS
  " Today's Production Result...
  PERFORM READ_ATINN      USING   'P_RP_STATUS'   L_ATINN  .
  SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
    FROM AUSP
   WHERE ATINN = L_ATINN
     AND KLART = '002'
** Changed by Furong on 01/07/2006
     AND ATWRT IN R_PROG_APP240 .
** end of change
  " Appending the Other Point for the Processing...
  CASE P_PROG_APP240 .
    WHEN '07'.
*      SELECT objek atwrt APPENDING TABLE lt_objek
*        FROM ausp
*       WHERE atinn = l_atinn
*         AND klart = '002'
*         AND atwrt IN
*         ('08', '09', '10', '11', '12', '13', '14', '15', '16').
    WHEN '17'.
*      SELECT objek atwrt APPENDING TABLE lt_objek
*        FROM ausp
*       WHERE atinn = l_atinn
*         AND klart = '002'
*         AND atwrt IN ('10', '11', '12', '13', '14', '15', '16').
    WHEN '24'.
      SELECT OBJEK ATWRT APPENDING TABLE LT_OBJEK
        FROM AUSP
       WHERE ATINN = L_ATINN
         AND KLART = '002'
         AND ATWRT = '26'.
    WHEN '25'.
      SELECT OBJEK ATWRT APPENDING TABLE LT_OBJEK
        FROM AUSP
       WHERE ATINN = L_ATINN
         AND KLART = '002'
         AND ATWRT = '27'.

  ENDCASE.

  PERFORM READ_ATINN      USING   'P_USAGE_CAR'   L_ATINN  .
  LOOP AT LT_OBJEK.
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.
    IT_OBJEK-ATWRT = LT_OBJEK-ATWRT.
    IF WA_MODEL   <> SPACE.
      CLEAR L_SUBRC .
      MOVE WA_MODEL  TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP240 USING IT_OBJEK-OBJEK
                                     'P_MODEL'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
*     R_WONO_APP240 FOR P_WONO_APP240,       "P_WORK_ORDER
    IF P_WONO_APP240 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_WONO_APP240 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP240 USING    IT_OBJEK-OBJEK
                                      'P_WORK_ORDER'
                                      L_ATWRT
                             CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
*     R_EXTC_APP240 FOR P_EXTC_APP240,       "P_EXT_COLOR
    IF P_EXTC_APP240 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_EXTC_APP240 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP240 USING IT_OBJEK-OBJEK
                                     'P_EXT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
*     R_INTC_APP240 FOR P_INTC_APP240.       "P_INT_COLOR
    IF P_INTC_APP240 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_INTC_APP240 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP240 USING IT_OBJEK-OBJEK
                                     'P_INT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.

    " Eliminate the Scrap / Disposal Car.
    SELECT SINGLE OBJEK INTO LT_OBJEK-OBJEK
      FROM AUSP
     WHERE OBJEK = LT_OBJEK-OBJEK
       AND ATINN = L_ATINN
       AND KLART = '002'
       AND ATWRT IN ('S', 'D').

    IF SY-SUBRC = 0.   CONTINUE.   ENDIF.
    APPEND IT_OBJEK.
  ENDLOOP.

  SORT IT_OBJEK BY OBJEK .
ENDFORM.                    " get_vehicle_master_no
*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_APP240.
  DATA: L_RPNO(02)         TYPE N,
        L_RPSERIAL(13)     TYPE C,
        L_MODEL   TYPE AUSP-ATWRT,
        L_BODYNO  TYPE AUSP-ATWRT.
  LOOP AT IT_OBJEK.
    CLEAR IT_APP240.
*   V/M No.
    MOVE-CORRESPONDING IT_OBJEK TO IT_APP240.
    IT_APP240-RP = IT_OBJEK-ATWRT.

*   BODYNO (P_MODEL & P_BODY_SERIAL)
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_MODEL'
                                       CHANGING L_MODEL .
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_BODY_SERIAL'
                                       CHANGING L_BODYNO .
    CONCATENATE L_MODEL L_BODYNO
      INTO IT_APP240-BODYNO .
*   Work Order(Serial), Ext.C, Int.C
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_APP240-WONO .
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_EXT_COLOR'
                                       CHANGING IT_APP240-EXTC .
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_APP240-INTC .
*   MI
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_MI'
                                       CHANGING IT_APP240-MI .
*   OCN
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_OCN'
                                       CHANGING IT_APP240-OCN .
*   Version
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_VERSION'
                                       CHANGING IT_APP240-VER .
*   Engine
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_219_9'
                                       CHANGING IT_APP240-ENG .
*   T/M
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_219_7'
                                       CHANGING IT_APP240-TM .
*   T/L
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                'P_219_5'
                                       CHANGING IT_APP240-TL .
*   Serial
    MOVE IT_OBJEK-ATWRT TO L_RPNO.
    CONCATENATE 'P_RP' L_RPNO '_SERIAL'  INTO L_RPSERIAL .
    PERFORM READ_NORMAL_CLASS_APP240 USING    IT_APP240-OBJEK
                                                L_RPSERIAL
                                       CHANGING IT_APP240-SERIAL .
*   ALC
    PERFORM READ_ALC_FROM_WO_APP240 USING    P_PART_APP240
                                      P_COLUMN_APP240
                                      IT_APP240-WONO
                                      IT_APP240-EXTC
                                      IT_APP240-INTC
                             CHANGING IT_APP240-ALC .
    APPEND IT_APP240.
  ENDLOOP.
  SORT IT_APP240 BY SERIAL BODYNO.
  DATA: L_TABIX TYPE SY-TABIX.
  DESCRIBE TABLE IT_APP240 LINES L_TABIX.
  IF L_TABIX <= 0.
    MESSAGE S000 WITH 'No Data!!'.
  ENDIF.
ENDFORM.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP240_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP240_MI  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASS_APP240 USING    P_VMNO
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
*&      Form  read_alc_from_wo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PART  text
*      -->P_P_COLUMN  text
*      -->P_IT_APP240_WONO  text
*      -->P_IT_APP240_EXTC  text
*      -->P_IT_APP240_INTC  text
*      <--P_IT_APP240_ALC  text
*----------------------------------------------------------------------*
FORM READ_ALC_FROM_WO_APP240 USING    P_PART
                               P_COLUMN
                               P_WONO
                               P_EXTC
                               P_INTC
                      CHANGING P_ALC.
  DATA: L_MATNR TYPE AUSP-OBJEK,
        L_ATNAM TYPE CABN-ATNAM,
        L_COLUMN TYPE I.
* alc TYPE ausp-atwrt,     "P_ALC_C_xxx OR P_ALC_U_xxx(05)
  L_COLUMN = P_COLUMN.
  IF P_PART_APP240 = 'U'.
    L_MATNR = P_WONO.
    WRITE L_COLUMN TO L_ATNAM LEFT-JUSTIFIED .
    CONCATENATE 'P_ALC_U_' L_ATNAM
      INTO L_ATNAM.
  ELSE.
    WRITE L_COLUMN TO L_ATNAM LEFT-JUSTIFIED .
    CONCATENATE P_WONO P_EXTC P_INTC
      INTO L_MATNR .
    CONCATENATE 'P_ALC_C_' L_ATNAM
      INTO L_ATNAM.
  ENDIF.

  SELECT SINGLE ATWRT
    INTO P_ALC
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~OBJEK = L_MATNR AND
          KLART    = '001'   AND
          CA~ATNAM = L_ATNAM   .

ENDFORM.                    " read_alc_from_wo
*&---------------------------------------------------------------------*
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_0827   text
*      -->P_P_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_DATA_OF_VM_APP240 USING    P_VMNO
                               P_CHAR
                               P_VALUE
                      CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = IT_OBJEK-OBJEK AND
          KLART = '002'          AND
          AU~ATWRT = P_VALUE     AND
          CA~ATNAM = P_CHAR      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DATA_APP240.
  CLEAR: IT_EXCEL_APP240, IT_EXCEL_APP240[].
  PERFORM SET_HEADER_APP240         TABLES IT_EXCEL_APP240.
  PERFORM SET_BODY_APP240           TABLES IT_EXCEL_APP240.
  PERFORM CALL_FUNC_DOWNLOAD_APP240 TABLES IT_EXCEL_APP240.

ENDFORM.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  set_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP240  text
*----------------------------------------------------------------------*
FORM SET_HEADER_APP240 TABLES   P_IT_EXCEL STRUCTURE IT_EXCEL_APP240.
  WRITE: 'Serial' TO P_IT_EXCEL-SERIAL,
         'Body No.' TO P_IT_EXCEL-BODYNO,
         'Work Order' TO P_IT_EXCEL-WONO,
         'Model Index' TO P_IT_EXCEL-MI,
         'OCN' TO P_IT_EXCEL-OCN,
         'Version' TO P_IT_EXCEL-VER,
         'External Color' TO P_IT_EXCEL-EXTC,
         'Internal Color' TO P_IT_EXCEL-INTC,
         'ALC' TO P_IT_EXCEL-ALC,
         'Engine' TO P_IT_EXCEL-ENG,
         'T/M' TO P_IT_EXCEL-TM,
         'T/L' TO P_IT_EXCEL-TL.
  APPEND P_IT_EXCEL.

ENDFORM.                    " set_header
*&---------------------------------------------------------------------*
*&      Form  set_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP240  text
*----------------------------------------------------------------------*
FORM SET_BODY_APP240 TABLES   P_IT STRUCTURE IT_EXCEL_APP240 .
  LOOP AT IT_APP240.
    CLEAR P_IT.
    MOVE-CORRESPONDING IT_APP240 TO P_IT.
    APPEND P_IT.
  ENDLOOP.
ENDFORM.                    " set_body
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP240  text
*----------------------------------------------------------------------*
FORM CALL_FUNC_DOWNLOAD_APP240 TABLES   P_IT STRUCTURE IT_EXCEL_APP240.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'VM Spec Per Each Progress.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = P_IT
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.
ENDFORM.                    " call_func_download
*&---------------------------------------------------------------------*
*&      Form  sort_SCREEN_2114
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_2114   USING PA_STYPE .
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP240-COLS  INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP240    ASCENDING  BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP240    DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " sort_SCREEN_2114

*&---------------------------------------------------------------------*
*&      Form  set_parameter_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_3107.
* Plant
  CLEAR : XLIST[], XVALUE.
  NAME = 'WA_PLANT'.
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT    .

* Model
  CLEAR : XLIST[], XVALUE.
  NAME = 'WA_MODEL' .
  PERFORM SET_FIELD_MODEL USING NAME  WA_MODEL .

* TABLEA(04), " (Header or Color)
  CLEAR : XLIST[], XVALUE.
  NAME = 'ST_3107_INPUT-TABLEA'.
  PERFORM SET_FIELD_CODE_TABLE1.
  PERFORM CALL_FUNCTION_VRM USING XLIST.
* TABLEB(04), " (Header or Color) and Code(ALC, HPC, 219)
  CLEAR : XLIST[], XVALUE.
  NAME = 'ST_3107_INPUT-TABLEB'.
  PERFORM SET_FIELD_CODE_TABLE2.
  PERFORM CALL_FUNCTION_VRM USING XLIST.
* cola(3)   TYPE c         ,             " column
* colb(3)   TYPE c         ,             " column
ENDFORM.                    " set_parameter_3107

*&---------------------------------------------------------------------*
*&      Form  set_parameter
*&---------------------------------------------------------------------*
*       Setting Parameters to Search Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_APP239.
  CLEAR R_OBJEK_APP239.
  REFRESH R_OBJEK_APP239.
*
  LOOP AT IT_OBJEK_APP239.
    R_OBJEK_APP239-SIGN = 'I'.
    R_OBJEK_APP239-OPTION = 'EQ'.
    R_OBJEK_APP239-LOW = IT_OBJEK_APP239-OBJEK.
    APPEND R_OBJEK_APP239.
  ENDLOOP.

  DATA: L_CHAR_C(20).
*
  CLEAR R_ATINN_APP239.
  REFRESH R_ATINN_APP239.
*
  R_ATINN_APP239-SIGN = 'I'.
  R_ATINN_APP239-OPTION = 'EQ'.
  L_CHAR_C = 'P_MI'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_OCN'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_EXT_COLOR'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_INT_COLOR'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_MODEL'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_BODY_SERIAL'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_MITU'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_MITU_DATE'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_SEQUENCE_SERIAL'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_SEQUENCE_DATE'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .
  APPEND R_ATINN_APP239.
  L_CHAR_C = 'P_WORK_ORDER'.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                         R_ATINN_APP239-LOW .

  APPEND R_ATINN_APP239.

ENDFORM.                    " set_parameter
*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA_APP239.
*
  DATA: L_LINES TYPE I,
        L_NUM08(08) TYPE N.
*
  CLEAR IT_APP239.
  REFRESH IT_APP239.
*
  PERFORM SELECT_OBJEK_APP239.
  DESCRIBE TABLE IT_OBJEK_APP239 LINES L_LINES.
  IF L_LINES < 1.
    MESSAGE S000 WITH 'THERE IS NO DATA'.
    EXIT.
  ENDIF.
*
  PERFORM SET_PARAMETER_APP239.
*
  CLEAR IT_CHAR_APP239.
  REFRESH IT_CHAR_APP239.
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_CHAR_APP239
    FROM AUSP
    WHERE OBJEK IN R_OBJEK_APP239 AND
          ATINN IN R_ATINN_APP239 AND
          KLART = '002' .

  DATA: L_TABIX LIKE SY-TABIX.
*
  LOOP AT IT_CHAR_APP239.
*
    AT NEW OBJEK.
      MOVE IT_CHAR_APP239-OBJEK TO IT_APP239-OBJEK.
      APPEND IT_APP239.
    ENDAT.
*
    SELECT SINGLE ATNAM INTO IT_CHAR_APP239-ATNAM
      FROM CABN
      WHERE ATINN = IT_CHAR_APP239-ATINN.
    MODIFY IT_CHAR_APP239.
*
    READ TABLE IT_APP239 WITH KEY OBJEK = IT_CHAR_APP239-OBJEK.
    L_TABIX = SY-TABIX.
*
    CASE IT_CHAR_APP239-ATNAM.
      WHEN 'P_MI'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-MI.
      WHEN 'P_OCN'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-OCN.
      WHEN 'P_EXT_COLOR'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-EXT_COLOR.
      WHEN 'P_INT_COLOR'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-INT_COLOR.
      WHEN 'P_MODEL'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-MODEL.
      WHEN 'P_BODY_SERIAL'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-BODY_SERIAL.
*      WHEN 'P_MITU'.
*        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-MITO.
      WHEN 'P_MITU_DATE'.
        MOVE: IT_CHAR_APP239-ATFLV TO L_NUM08,
              L_NUM08                 TO IT_APP239-MITU_DATE.
      WHEN 'P_SEQUENCE_SERIAL'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-SEQUENCE_SERIAL.
      WHEN 'P_SEQUENCE_DATE'.
        MOVE: IT_CHAR_APP239-ATFLV TO L_NUM08,
              L_NUM08                 TO IT_APP239-SEQUENCE_DATE.
      WHEN 'P_WORK_ORDER'.
        MOVE IT_CHAR_APP239-ATWRT TO IT_APP239-WORKORDER.
    ENDCASE.
*
    MODIFY IT_APP239 INDEX L_TABIX.
*
  ENDLOOP.
ENDFORM.                    " make_data

*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJEK
*&---------------------------------------------------------------------*
*       Checking If there is a V/M No in The Table - AUSP.
*----------------------------------------------------------------------*
*      -->P_WA_OBJEK_L  text
*      -->P_WA_ATINN_L  text
*      -->P_WA_ATWRT_L  text
*----------------------------------------------------------------------*
FORM CHECK_OBJEK_APP239 USING    P_OBJEK
                          P_ATINN
                          P_ATWRT.
  SELECT SINGLE OBJEK
    INTO P_OBJEK
    FROM AUSP
    WHERE OBJEK = P_OBJEK AND
          ATINN = P_ATINN AND
          ATWRT = P_ATWRT AND
          KLART = '002'   .

ENDFORM.                    " CHECK_OBJEK
*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJEK
*&---------------------------------------------------------------------*
*       Separating V/M No. By Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_OBJEK_APP239.
  DATA: L_OBJEK_C TYPE AUSP-OBJEK,
        L_ATWRT_C TYPE AUSP-ATWRT,
        L_ATINN_N TYPE CABN-ATINN.
  DATA: L_BODYNO_C(10).
  DATA: LG_ATINN_N TYPE AUSP-ATINN,
        LG_CHAR_C(20).
  DATA: L_CHAR_C(20).

  IF P_BODYSER_APP239 <> SPACE.
    CONCATENATE WA_MODEL       P_BODYSER_APP239 INTO P_BODYNO_APP239.
    CONCATENATE P_BODYNO_APP239 '%' INTO L_BODYNO_C.
  ELSE.
    CONCATENATE WA_MODEL       '%' INTO L_BODYNO_C.
  ENDIF.
*
  CLEAR IT_OBJEK_APP239.
  REFRESH IT_OBJEK_APP239.
*
* necessary parameter.
  LG_CHAR_C = 'P_MITU'.
  CLEAR L_ATINN_N.
  PERFORM CALL_FUNC_CONVERSION_APP239 USING LG_CHAR_C
                                         LG_ATINN_N .
  SELECT DISTINCT OBJEK  " CHECK MITO STATUS
    INTO L_OBJEK_C
    FROM AUSP
    WHERE OBJEK LIKE L_BODYNO_C AND
          ATINN = LG_ATINN_N AND
          ATWRT = 'Y' AND
          KLART = '002'.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
*   IF P_MODEL_APP239 is NOT INITIAL ...
    IF WA_MODEL  <> SPACE.
      L_CHAR_C = 'P_MODEL'.
      CLEAR L_ATINN_N.
      PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                             L_ATINN_N .
      L_ATWRT_C = WA_MODEL     .
      PERFORM CHECK_OBJEK_APP239 USING L_OBJEK_C
                                L_ATINN_N
                                L_ATWRT_C.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.  " IF P_MODEL_APP239 <> SPACE ...
*   IF P_BODYSER_APP239 IS NOT INITIAL ...
    IF P_BODYSER_APP239 <> SPACE.
      L_CHAR_C = 'P_BODY_SERIAL'.
      CLEAR L_ATINN_N.
      PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                             L_ATINN_N .
      L_ATWRT_C = P_BODYSER_APP239.
      PERFORM CHECK_OBJEK_APP239 USING L_OBJEK_C
                                L_ATINN_N
                                L_ATWRT_C.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   IF P_ORDERNO_APP239 IS NOT INITIAL ...
    IF P_ORDERNO_APP239 <> SPACE.
      L_CHAR_C = 'P_WORK_ORDER'.
      CLEAR L_ATINN_N.
      PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                             L_ATINN_N .
      L_ATWRT_C = P_ORDERNO_APP239.
      PERFORM CHECK_OBJEK_APP239 USING L_OBJEK_C
                                L_ATINN_N
                                L_ATWRT_C.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   IF P_EXT_COLOR_APP239 IS NOT INITIAL ...
    IF P_EXT_COLOR_APP239 <> SPACE.
      L_CHAR_C = 'P_EXT_COLOR'.
      CLEAR L_ATINN_N.
      PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                             L_ATINN_N .
      L_ATWRT_C = P_EXT_COLOR_APP239.
      PERFORM CHECK_OBJEK_APP239 USING L_OBJEK_C
                                L_ATINN_N
                                L_ATWRT_C.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   IF P_INT_COLOR_APP239 IS NOT INITIAL ...
    IF P_INT_COLOR_APP239 <> SPACE.
      L_CHAR_C = 'P_INT_COLOR'.
      CLEAR L_ATINN_N.
      PERFORM CALL_FUNC_CONVERSION_APP239 USING L_CHAR_C
                                             L_ATINN_N .
      L_ATWRT_C = P_INT_COLOR_APP239.
      PERFORM CHECK_OBJEK_APP239 USING L_OBJEK_C
                                L_ATINN_N
                                L_ATWRT_C.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*
    APPEND L_OBJEK_C TO IT_OBJEK_APP239.
*
  ENDSELECT.  " CHECK MITO STATUS

ENDFORM.                    " SELECT_OBJEK
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       Char's Name Conversion To Char's Value
*----------------------------------------------------------------------*
*      -->P_L_CHAR_C  text
*      -->P_L_MITU_N  text
*----------------------------------------------------------------------*
FORM CALL_FUNC_CONVERSION_APP239 USING    P_CHAR_C
                                       P_NUMB_N.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      INPUT  = P_CHAR_C
    IMPORTING
      OUTPUT = P_NUMB_N.

ENDFORM.                    " call_function_conversion
*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       Setting Internal Table's Header For download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_APP239.
  CLEAR IT_EXCEL_239.
  REFRESH IT_EXCEL_239.
  MOVE 'Body Number' TO IT_EXCEL_239-OBJEK.
  MOVE 'MITU Date'   TO IT_EXCEL_239-MITU_DATE.
  MOVE 'Body Serial' TO IT_EXCEL_239-BODY_SERIAL.
  MOVE 'Model'       TO IT_EXCEL_239-MODEL.
  MOVE 'Spec'        TO IT_EXCEL_239-MI.
  MOVE 'OCN'         TO IT_EXCEL_239-OCN.
  MOVE 'Work Order'  TO IT_EXCEL_239-WORKORDER.
  MOVE 'External Color' TO IT_EXCEL_239-EXT_COLOR.
  MOVE 'Internal Color' TO IT_EXCEL_239-INT_COLOR.
  MOVE 'Sequence Serial' TO IT_EXCEL_239-SEQUENCE_SERIAL.
  MOVE 'Sequence Date' TO IT_EXCEL_239-SEQUENCE_DATE.
  APPEND IT_EXCEL_239.

  LOOP AT IT_APP239.
    CLEAR IT_EXCEL_239.
    MOVE-CORRESPONDING IT_APP239 TO IT_EXCEL_239.
    APPEND IT_EXCEL_239.
  ENDLOOP.

  PERFORM CALL_FUNCTION_DOWNLOAD_APP239.

ENDFORM.                    " download
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_DOWNLOAD
*&---------------------------------------------------------------------*
*       Calling A Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_DOWNLOAD_APP239.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'MITU STATUS.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = IT_EXCEL_239
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_FUNCTION_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  sort_SCREEN_2113
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_2113     USING PA_STYPE.
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP239-COLS INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP239 ASCENDING BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP239 DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " sort_SCREEN_2113

*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX
*&---------------------------------------------------------------------*
FORM SET_LISTBOX_APP272.
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_PLANT'            .
  PERFORM SET_FIELD_PLANT USING NAME  WA_PLANT   .
*
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_MODEL'            .
  PERFORM SET_FIELD_MODEL USING NAME  WA_MODEL   .
*
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'ZTBM_ABXPLIDT-GUBN'  .
  PERFORM LIST_BOX_P_GUBN_APP272  USING NAME ZTBM_ABXPLIDT-GUBN.
ENDFORM.                    " SET_LISTBOX

*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_P_GUBN_APP272
*&---------------------------------------------------------------------*
FORM LIST_BOX_P_GUBN_APP272  USING P_NAME P_PARAMETER .
  CLEAR : XLIST, XLIST[].
  XVALUE-KEY = 'P'. XVALUE-TEXT = 'P TABLE'.  APPEND XVALUE TO XLIST.
  XVALUE-KEY = 'B'. XVALUE-TEXT = 'B TABLE'.  APPEND XVALUE TO XLIST.
  XVALUE-KEY = 'Q'. XVALUE-TEXT = 'Q TABLE'.  APPEND XVALUE TO XLIST.

* LIST BOX SETTING
  PERFORM LIST_BOX_FUNCTION USING P_NAME.
  IF P_PARAMETER IS INITIAL.
    READ TABLE XLIST INTO XVALUE  INDEX 1.
    P_PARAMETER = XVALUE-KEY.
  ENDIF.
ENDFORM.                    " LIST_BOX_P_GUBN_APP272

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS_APP272.
  DATA: L_ORDR           LIKE ZTPP_WOSUM-WO_SER,
        L_NATION LIKE ZTPP_WOSUM-NATION,
        L_DEALER LIKE ZTPP_WOSUM-DEALER,
        L_CHK            TYPE C                 ,
        L_MATNR_CL       LIKE MARA-MATNR        ,
        L_MATNR_HD       LIKE MARA-MATNR        ,
        L_TOTAL          LIKE ZTPP_WOSUM-MODQTY,
        L_RSNUM LIKE ZTPP_INPUT_PLAN-RSNUM,
        LT_WOSUM         LIKE TABLE OF ZTPP_WOSUM  WITH HEADER LINE.

  DATA: L_SEQNO(2) TYPE N,
        L_TEXT(25),
        L_COUNT TYPE I,
        L_NO(2) TYPE N,
        L_DATE LIKE SY-DATUM,
        L_CHECK(1),
        LT_INPUT LIKE TABLE OF ZTPP_INPUT_PLAN
                WITH HEADER LINE.

  DATA: BEGIN OF LT_WORDER OCCURS 0,
        WORK_ORDER LIKE ZTPP_INPUT_PLAN-WORK_ORDER,
        EXTC LIKE ZTPP_INPUT_PLAN-EXTC,
        INTC LIKE ZTPP_INPUT_PLAN-INTC,
*        quantity like it_app272_01-p009,
      END OF LT_WORDER.

  FIELD-SYMBOLS: <FIELD>.

  CLEAR: IT_APP272_01, IT_APP272_01[], IT_APP272_DATE, IT_APP272_01[].
  READ TABLE IT_APP272_DATE INDEX 1.


  SELECT * INTO TABLE LT_INPUT
   FROM ZTPP_INPUT_PLAN
    WHERE MODL = WA_MODEL.
*   where rsnum <> '        ' OR rsnum <> '00000000'.

  DELETE LT_INPUT WHERE RSNUM = '        ' OR RSNUM = '00000000'.
  SORT LT_INPUT BY WORK_ORDER EXTC INTC RSNUM.
  LOOP AT LT_INPUT.
    MOVE-CORRESPONDING LT_INPUT TO LT_WORDER.
    COLLECT LT_WORDER.
  ENDLOOP.

*  delete adjacent duplicates from lt_input comparing
*          work_order extc intc.
*  read table lt_input index 1.
**  l_work_order = lt_input-work_order.
**  l_extc = lt_input-extc.
**  intc = lt_input-intc.
*  l_rsnum = lt_input-rsnum.
*  clear: l_qty.

  LOOP AT LT_WORDER.
    L_RSNUM = '*'..
    IT_APP272_01-P001 = LT_WORDER-WORK_ORDER.
    IT_APP272_01-P002 = LT_WORDER-EXTC.
    IT_APP272_01-P003 = LT_WORDER-INTC.
    L_ORDR = LT_WORDER-WORK_ORDER+0(9).
    L_NATION = LT_WORDER-WORK_ORDER+9(3).
    L_DEALER = LT_WORDER-WORK_ORDER+12(2).

    SELECT SINGLE MODQTY FORECASTQTY PLANQTY SEQQTY
           FSC INTO
         (IT_APP272_01-P005, IT_APP272_01-P006,
          IT_APP272_01-P007, IT_APP272_01-P008,
          IT_APP272_01-P004)
     FROM ZTPP_WOSUM
     WHERE WO_SER = L_ORDR
       AND NATION = L_NATION
       AND DEALER = L_DEALER
       AND EXTC = LT_WORDER-EXTC
       AND INTC = LT_WORDER-INTC.

*      it_app272_01-p005 = lt_wosum-modqty.
*      it_app272_01-p006 = lt_wosum-forecastqty.
*      it_app272_01-p007 = lt_wosum-planqty.

    LOOP AT LT_INPUT WHERE WORK_ORDER = LT_WORDER-WORK_ORDER
                     AND EXTC = LT_WORDER-EXTC
                     AND INTC = LT_WORDER-INTC.
      IF L_RSNUM <> LT_INPUT-RSNUM.
        IF L_RSNUM = '*'.
          L_RSNUM = LT_INPUT-RSNUM.
          L_TOTAL = L_TOTAL + 1.
        ELSE.
          CASE L_RSNUM.
            WHEN IT_APP272_DATE-DAY01.
              IT_APP272_01-P009 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY02.
              IT_APP272_01-P010 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY03.
              IT_APP272_01-P011 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY04.
              IT_APP272_01-P012 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY05.
              IT_APP272_01-P013 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY06.
              IT_APP272_01-P014 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY07.
              IT_APP272_01-P015 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY08.
              IT_APP272_01-P016 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY09.
              IT_APP272_01-P017 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY10.
              IT_APP272_01-P018 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY11.
              IT_APP272_01-P019 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY12.
              IT_APP272_01-P020 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY13.
              IT_APP272_01-P021 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY14.
              IT_APP272_01-P022 = L_TOTAL.
            WHEN IT_APP272_DATE-DAY15.
              IT_APP272_01-P023 = L_TOTAL.
          ENDCASE.
          CLEAR: L_TOTAL.
          L_RSNUM = LT_INPUT-RSNUM.
          L_TOTAL = L_TOTAL + 1.
        ENDIF.
      ELSE.
        L_TOTAL = L_TOTAL + 1.
      ENDIF.
    ENDLOOP.
    CASE L_RSNUM.
      WHEN IT_APP272_DATE-DAY01.
        IT_APP272_01-P009 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY02.
        IT_APP272_01-P010 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY03.
        IT_APP272_01-P011 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY04.
        IT_APP272_01-P012 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY05.
        IT_APP272_01-P013 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY06.
        IT_APP272_01-P014 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY07.
        IT_APP272_01-P015 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY08.
        IT_APP272_01-P016 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY09.
        IT_APP272_01-P017 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY10.
        IT_APP272_01-P018 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY11.
        IT_APP272_01-P019 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY12.
        IT_APP272_01-P020 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY13.
        IT_APP272_01-P021 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY14.
        IT_APP272_01-P022 = L_TOTAL.
      WHEN IT_APP272_DATE-DAY15.
        IT_APP272_01-P023 = L_TOTAL.
    ENDCASE.
    CLEAR: L_TOTAL.
    APPEND IT_APP272_01.
    CLEAR: IT_APP272_01.
  ENDLOOP.


*    select single count( distinct rsnum ) into IT_APP272_01-P009
*    from ztpp_input_plan
*    where work_order = lt_worder-work_order
*                     and extc = lt_worder-extc
*                     and intc = lt_worder-intc
*                     and rsnum = IT_APP272_DATE-DAY01
*       group by work_order extc intc.
*
*    if not IT_APP272_DATE-DAY02 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P010
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY02
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY03 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P011
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY03
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY04 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P012
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY04
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY05 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P013
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY05
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY06 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P014
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY06
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY07 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P015
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY07
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY08 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P016
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY08
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY09 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P017
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY09
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY10 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P018
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY10
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY11 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P019
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY11
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY12 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P020
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY12
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY13 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P021
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY13
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY14 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P022
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY14
*          group by work_order extc intc.
*    endif.
*
*    if not IT_APP272_DATE-DAY15 is initial.
*      select single count( distinct rsnum ) into IT_APP272_01-P023
*       from ztpp_input_plan
*       where work_order = lt_worder-work_order
*                        and extc = lt_worder-extc
*                        and intc = lt_worder-intc
*                        and rsnum = IT_APP272_DATE-DAY15
*          group by work_order extc intc.
*    endif.
*    l_total = IT_APP272_01-P009 + IT_APP272_01-P010 + IT_APP272_01-P011
*     + IT_APP272_01-P012 + IT_APP272_01-P013 + IT_APP272_01-P014
*     + IT_APP272_01-P015 + IT_APP272_01-P016 + IT_APP272_01-P017
*     + IT_APP272_01-P018 + IT_APP272_01-P019 + IT_APP272_01-P020
*     + IT_APP272_01-P021 + IT_APP272_01-P022 + IT_APP272_01-P023.
*
*   if l_total > 0.
*    it_app272_01-p001 = lt_worder-work_order.
*    it_app272_01-p002 = lt_worder-extc.
*    it_app272_01-p003 = lt_worder-intc.
**      it_app272_01-p005 = lt_wosum-modqty.
**      it_app272_01-p006 = lt_wosum-forecastqty.
**      it_app272_01-p007 = lt_wosum-planqty.
*    append  it_app272_01.
*    endif.
*    clear: it_app272_01.
*  endloop.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_wosum
*    FROM ztpp_wosum AS a
*   WHERE a~modqty > a~rp16tq .
*
*  loop at lt_wosum.
*    CONCATENATE lt_wosum-wo_ser lt_wosum-nation lt_wosum-dealer
*           INTO l_ordr.
*
*    select * into table lt_input
*        from ztpp_input_plan
*        where work_order = l_ordr
*          and extc = lt_wosum-extc
*          and intc = lt_wosum-intc.
*
*    if sy-subrc = 0.
*      it_app272_01-p001 = l_ordr         .
*      it_app272_01-p002 = lt_wosum-extc  .
*      it_app272_01-p003 = lt_wosum-intc  .
*      it_app272_01-p005 = lt_wosum-modqty.
*      it_app272_01-p006 = lt_wosum-forecastqty.
*      it_app272_01-p007 = lt_wosum-planqty.
*      sort lt_input by rsnum.
*
*      l_seqno = '08'.
*
*      l_no = '01'.
*      do 15 times.
*        concatenate 'IT_APP272_DATE-DAY' l_no into l_text.
*        assign (l_text) to <field>.
*        l_date = <field>.
*        if l_date is initial.
*           exit.
*        endif.
*        clear: l_count.
*        loop at lt_input where rsnum = l_date.
*          l_count = l_count + 1.
*        endloop.
*        if l_count > 0.
*          concatenate 'IT_APP272_01-P0' l_seqno INTO l_text.
*          assign (l_text) to <field>.
*          <field> = l_count.
*          clear: l_count.
*          l_check = 'X'.
*        endif.
*        l_seqno = l_seqno + 1.
*        l_no = l_no + 1.
*      enddo.
*      if l_check = 'X'.
*        APPEND it_app272_01.
*      endif.
*      clear: it_app272_01, l_check, lt_input, lt_input[].
*    endif.
*  endloop.

*  LOOP AT lt_wosum.
*    CLEAR: l_chk, l_matnr_hd, l_matnr_cl.
*    CONCATENATE lt_wosum-wo_ser lt_wosum-nation lt_wosum-dealer
*           INTO l_ordr  .
*    l_matnr_hd        = l_ordr         .
*    CONCATENATE l_matnr_hd lt_wosum-extc  lt_wosum-intc INTO l_matnr_cl
*.
*    it_app272_01-p001 = l_ordr         .
*    it_app272_01-p002 = lt_wosum-extc  .
*    it_app272_01-p003 = lt_wosum-intc  .
**   IT_APP272_01-P004 = LT_WOSUM-WO_SER.     " Not defined!!
*    it_app272_01-p005 = lt_wosum-modqty.
*    it_app272_01-p006 = lt_wosum-forecastqty.
*    it_app272_01-p007 = lt_wosum-planqty.
*    PERFORM read_6gb  USING l_ordr  lt_wosum-extc  lt_wosum-intc.
*    PERFORM check_input_vals USING l_matnr_hd  l_matnr_cl  l_chk.
*    CHECK l_chk = space  .
*    APPEND it_app272_01  .
*  ENDLOOP.
ENDFORM.                    " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_9002
*&---------------------------------------------------------------------*
FORM DISPLAY_APP272_02.

ENDFORM.                    " DISPLAY_9002
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_9003
*&---------------------------------------------------------------------*
FORM DISPLAY_APP272_03.

ENDFORM.                    " DISPLAY_9003
*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP301.
* Plant
  CLEAR : XLIST[], XVALUE.
  NAME = 'WA_PLANT'.
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT    .

* Model
  CLEAR : XLIST[], XVALUE.
  NAME = 'WA_MODEL' .
  PERFORM SET_FIELD_MODEL USING NAME  WA_MODEL .

* Selection Type
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_SL_APP301'.
  PERFORM SET_FIELD_SL_APP301.
  PERFORM CALL_FUNCTION_VRM        USING XLIST.

* Part
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_PART_APP301'.
  PERFORM SET_FIELD_PART.
  PERFORM CALL_FUNCTION_VRM        USING XLIST.
* Column
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLUMN_APP301'.
  PERFORM SET_FIELD_COLUMN .
  PERFORM CALL_FUNCTION_VRM        USING XLIST.
ENDFORM.                    " make_dropdown_list_box_app301
*&---------------------------------------------------------------------*
*&      Form  build_variant_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_VARIANT_APP301.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " build_variant_app301
*&---------------------------------------------------------------------*
*&      Form  build_layout_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT_APP301.
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " build_layout_app301
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT_APP301.
  CLEAR : GT_FIELDCAT_SLIS[] .
  CASE P_SL_APP301.
    WHEN 'H'. "Hourly
      PERFORM  BUILD_FIELDCATALOG_APP301  TABLES GT_FIELDCAT_SLIS
                                          USING 'IT_HOUR_APP301'.
    WHEN 'D'.
      PERFORM  BUILD_FIELDCATALOG_APP301  TABLES GT_FIELDCAT_SLIS
                                          USING 'IT_DAY_APP301' .
    WHEN 'W'.
      PERFORM  BUILD_FIELDCATALOG_APP301  TABLES GT_FIELDCAT_SLIS
                                          USING 'IT_WEEK_APP301' .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " build_fieldcat_app301
*&---------------------------------------------------------------------*
*&      Form  call_method_hourly_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_HOURLY_APP301.
  DATA: LT_EXCLUDE TYPE UI_FUNCTIONS .
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_VARIANT
      IT_TOOLBAR_EXCLUDING = LT_EXCLUDE
      I_SAVE               = 'A'
    CHANGING
      IT_OUTTAB            = IT_HOUR_APP301[]
      IT_FIELDCATALOG      = GT_FIELDCAT[].
ENDFORM.                    " call_method_hourly_app301

*&---------------------------------------------------------------------*
*&      Form  call_method_daily_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_DAILY_APP301.
  DATA: LT_EXCLUDE TYPE UI_FUNCTIONS .
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_VARIANT
      IT_TOOLBAR_EXCLUDING = LT_EXCLUDE
      I_SAVE               = 'A'
    CHANGING
      IT_OUTTAB            = IT_DAY_APP301[]
      IT_FIELDCATALOG      = GT_FIELDCAT[].
ENDFORM.                    " call_method_daily_app301

*&---------------------------------------------------------------------*
*&      Form  call_method_weekly_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_WEEKLY_APP301.
  DATA: LT_EXCLUDE TYPE UI_FUNCTIONS .
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT            = 'X'
      IS_LAYOUT            = GS_LAYOUT
      IS_VARIANT           = GS_VARIANT
      IT_TOOLBAR_EXCLUDING = LT_EXCLUDE
      I_SAVE               = 'A'
    CHANGING
      IT_OUTTAB            = IT_WEEK_APP301[]
      IT_FIELDCATALOG      = GT_FIELDCAT[].
ENDFORM.                    " call_method_weekly_app301

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT_SLIS  text
*      -->P_0033   text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG_APP301 TABLES   P_FIELDCAT_SLIS TYPE
                                        SLIS_T_FIELDCAT_ALV
                               USING    ITAB_NAME           .
  DATA  : LS_FVAT         TYPE LVC_S_FCAT                          ,
          T_REPID         TYPE SY-REPID                            ,
          L_EXIT          TYPE C                                   ,
          L_COL           LIKE SY-TABIX VALUE 10                   ,
          LT_FIELDCATALOG LIKE LINE OF GT_FIELDCAT                 .

  T_REPID = SY-REPID                                               .
  CLEAR : GT_FIELDCAT[]                                            .
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = T_REPID
      I_INTERNAL_TABNAME     = ITAB_NAME
      I_INCLNAME             = T_REPID
    CHANGING
      CT_FIELDCAT            = P_FIELDCAT_SLIS[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2.

  LOOP AT P_FIELDCAT_SLIS     .
    CLEAR: LT_FIELDCATALOG, L_EXIT.
    MOVE-CORRESPONDING P_FIELDCAT_SLIS TO LT_FIELDCATALOG          .
    MOVE P_FIELDCAT_SLIS-REPTEXT_DDIC TO LT_FIELDCATALOG-REPTEXT.

    LT_FIELDCATALOG-KEY        = ''.
    LT_FIELDCATALOG-CFIELDNAME = ''.
    LT_FIELDCATALOG-NO_ZERO    = ''.

    CASE P_FIELDCAT_SLIS-FIELDNAME.
      WHEN 'SERIAL' OR 'RP' OR 'ALC_CODE' OR 'B_RESULT' OR
           'ZUNAME' OR 'ZDATUM' OR 'ZUZEIT' .
        L_EXIT = 'X'.
      WHEN 'ORDER'.
        LT_FIELDCATALOG-KEY        = 'X'.
        LT_FIELDCATALOG-REPTEXT    = 'Order'.
        LT_FIELDCATALOG-COL_POS    = 1 .
      WHEN 'MODEL'.
        LT_FIELDCATALOG-KEY        = 'X'.
        LT_FIELDCATALOG-REPTEXT    = 'Model'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 2 .
      WHEN 'ALC_VALS'.
        LT_FIELDCATALOG-KEY        = 'X'.
        LT_FIELDCATALOG-REPTEXT    = 'Code'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 3 .
      WHEN 'D_1' OR 'W_1' .
        LT_FIELDCATALOG-KEY        = ' '.
        LT_FIELDCATALOG-REPTEXT    = P_FIELDCAT_SLIS-FIELDNAME.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 10.
      WHEN 'SEQ'.
        LT_FIELDCATALOG-KEY        = ' '.
        LT_FIELDCATALOG-REPTEXT    = 'SEQ'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 4 .
      WHEN 'BODYIN'.
        LT_FIELDCATALOG-KEY        = ' '.
*       lt_fieldcatalog-reptext    = 'Body In'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 5 .
      WHEN 'WBS'.
        LT_FIELDCATALOG-KEY        = ' '.
        LT_FIELDCATALOG-REPTEXT    = 'WBS'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 6 .
      WHEN 'PAINT'.
        LT_FIELDCATALOG-KEY        = ' '.
        LT_FIELDCATALOG-REPTEXT    = 'P/IN' .
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 7 .
      WHEN 'PRJ'.
        LT_FIELDCATALOG-KEY        = ' '.
        LT_FIELDCATALOG-REPTEXT    = 'P/REJ'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 8 .
      WHEN 'PBS'.
        LT_FIELDCATALOG-KEY        = ' '.
        LT_FIELDCATALOG-REPTEXT    = 'PBS'.
        LT_FIELDCATALOG-OUTPUTLEN  = 05.
        LT_FIELDCATALOG-COL_POS    = 09.
      WHEN OTHERS.
        CASE P_SL_APP301.
          WHEN 'H' .
            CASE P_FIELDCAT_SLIS-FIELDNAME.
              WHEN 'MITU'.
                LT_FIELDCATALOG-REPTEXT    = 'MITU'.
              WHEN 'T01'.
                LT_FIELDCATALOG-KEY        = ' '.
                LT_FIELDCATALOG-REPTEXT    = 'Total 1 Day'.
                LT_FIELDCATALOG-OUTPUTLEN  = 05.
                LT_FIELDCATALOG-COL_POS    = 11.
** Furong on 07/30/12 for 3 shift
              WHEN 'H02' OR 'H04' OR 'H06' OR 'H08' OR 'H10' OR
                   'H12' OR 'H14' OR 'H16' OR 'H18' OR 'H20' OR
                   'H22' OR 'H24'.
                LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.
                LT_FIELDCATALOG-COL_POS = LT_FIELDCATALOG-COL_POS - 3 .
              WHEN  'H26' OR 'H28' OR 'H30' OR 'H32' OR 'H34' OR
                    'H36' OR 'H38' OR 'H40' OR 'H42' OR 'H44' OR
                    'H46' OR 'H48' .
                LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.
                LT_FIELDCATALOG-COL_POS = LT_FIELDCATALOG-COL_POS - 2 .
              WHEN  'H50' OR 'H52' OR 'H54' OR 'H56' OR 'H58' OR
                    'H60' OR 'H62' OR 'H64' OR 'H66' OR 'H68' OR
                    'H70' OR 'H72'.
                LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.

*              WHEN 'H02' OR 'H04' OR 'H06' OR 'H08' OR 'H10' OR
*                   'H12' OR 'H14' OR 'H16' OR 'H18' OR 'H20' .
*                LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.
*                LT_FIELDCATALOG-COL_POS = LT_FIELDCATALOG-COL_POS - 3 .
*              WHEN 'H22' OR 'H24' OR 'H26' OR 'H28' OR 'H30' OR
*                   'H32' OR 'H34' OR 'H36' OR 'H38' OR 'H40' .
*                LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.
*                LT_FIELDCATALOG-COL_POS = LT_FIELDCATALOG-COL_POS - 2 .
*              WHEN 'H42' OR 'H44' OR 'H46' OR 'H48' OR 'H50' OR
*                   'H52' OR 'H54' OR 'H56' OR 'H58' OR 'H60' .
*                LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.
** End on 07/30/12

              WHEN 'T02'.
                LT_FIELDCATALOG-KEY        = ' '.
                LT_FIELDCATALOG-REPTEXT    = 'Total 2 Day'.
                LT_FIELDCATALOG-OUTPUTLEN  = 05.
                LT_FIELDCATALOG-COL_POS    = 21.
              WHEN 'GTOT'.
                LT_FIELDCATALOG-REPTEXT    = 'Grand Total'.
              WHEN 'T03'.
                LT_FIELDCATALOG-KEY        = ' '.
                LT_FIELDCATALOG-REPTEXT    = 'Total 3 Day'.
                LT_FIELDCATALOG-OUTPUTLEN  = 05.
                LT_FIELDCATALOG-COL_POS    = 33.
            ENDCASE.
          WHEN 'W' OR 'D'.
            LT_FIELDCATALOG-REPTEXT = P_FIELDCAT_SLIS-FIELDNAME.
        ENDCASE.
    ENDCASE.
    IF L_EXIT = SPACE.
      APPEND LT_FIELDCATALOG TO GT_FIELDCAT.
    ENDIF.
  ENDLOOP.
  SORT GT_FIELDCAT BY COL_POS.
ENDFORM.                    " BUILD_FIELDCATALOG_app301

*&---------------------------------------------------------------------*
*&      Form  set_field_SL_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_SL_APP301.
  XVALUE-KEY  = 'H'.
  XVALUE-TEXT = 'Hourly'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'D'.
  XVALUE-TEXT = 'Daily'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'W'.
  XVALUE-TEXT = 'Weekly'.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " set_field_SL_app301

*&---------------------------------------------------------------------*
*&      Form  search_data_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP301.
  IF NOT ( GS_CUSTOM_CONTAINER IS INITIAL ).
    CALL METHOD GS_CUSTOM_CONTAINER->FREE.
    FREE  GS_CUSTOM_CONTAINER.
  ENDIF.
  DATA: L_ERROR,
        L_TEXT(40).
  PERFORM CHECK_PARAMETERS_APP301 USING L_ERROR  L_TEXT .
  IF L_ERROR <> SPACE.
    CONCATENATE 'Set Parameter -' L_TEXT
      INTO L_TEXT
      SEPARATED BY SPACE.
    MESSAGE S000 WITH L_TEXT .
    EXIT.
  ENDIF.

  CASE P_SL_APP301.
    WHEN 'H'.
      PERFORM READ_HOUR_TABLE_APP301.
    WHEN 'D'.
      PERFORM READ_DAY_TABLE_APP301.
    WHEN 'W'.
      PERFORM READ_WEEK_TABLE_APP301.
  ENDCASE.
ENDFORM.                    " search_data_app301
*&---------------------------------------------------------------------*
*&      Form  read_hour_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_HOUR_TABLE_APP301.
********************************************************************
* Because there is not Field 'PART' in The Table - ZTPP_SEQ_SUM01,
* You have to define The Parameter to search The Table.
* So, It has to be defined .
********************************************************************
  RANGES: LR_RP FOR ZTPP_SEQ_SUM01-RP,         "Reporting Point
          LR_MODEL FOR ZTPP_SEQ_SUM01-MODEL,   "Model
          LR_COL FOR ZTPP_SEQ_SUM01-ALC_CODE,  "Column
          LR_CODE FOR ZTPP_SEQ_SUM01-ALC_VALS. "Code

  DATA: L_C301         LIKE P_COLUMN_APP301 ,
        BEGIN OF LT_HOUR OCCURS 0,
          MODEL TYPE ZTPP_SEQ_SUM01-MODEL,
          ALC_VALS TYPE ZTPP_SEQ_SUM01-ALC_VALS,
          D_1 TYPE ZTPP_SEQ_SUM01-D_1,
          SEQ TYPE ZTPP_SEQ_SUM01-SEQ,
          BODYIN TYPE ZTPP_SEQ_SUM01-BODYIN,
          WBS TYPE ZTPP_SEQ_SUM01-WBS,
          PAINT TYPE ZTPP_SEQ_SUM01-PAINT,
          PRJ TYPE ZTPP_SEQ_SUM01-PRJ,
          PBS TYPE ZTPP_SEQ_SUM01-PBS,
          H02 TYPE ZTPP_SEQ_SUM01-H02,
          H04 TYPE ZTPP_SEQ_SUM01-H04,
          H06 TYPE ZTPP_SEQ_SUM01-H06,
          H08 TYPE ZTPP_SEQ_SUM01-H08,
          H10 TYPE ZTPP_SEQ_SUM01-H10,
          H12 TYPE ZTPP_SEQ_SUM01-H12,
          H14 TYPE ZTPP_SEQ_SUM01-H14,
          H16 TYPE ZTPP_SEQ_SUM01-H16,
          H18 TYPE ZTPP_SEQ_SUM01-H18,
          H20 TYPE ZTPP_SEQ_SUM01-H20,
          H22 TYPE ZTPP_SEQ_SUM01-H22,
          H24 TYPE ZTPP_SEQ_SUM01-H24,
          H26 TYPE ZTPP_SEQ_SUM01-H26,
          H28 TYPE ZTPP_SEQ_SUM01-H28,
          H30 TYPE ZTPP_SEQ_SUM01-H30,
          H32 TYPE ZTPP_SEQ_SUM01-H32,
          H34 TYPE ZTPP_SEQ_SUM01-H34,
          H36 TYPE ZTPP_SEQ_SUM01-H36,
          H38 TYPE ZTPP_SEQ_SUM01-H38,
          H40 TYPE ZTPP_SEQ_SUM01-H40,
          H42 TYPE ZTPP_SEQ_SUM01-H42,
          H44 TYPE ZTPP_SEQ_SUM01-H44,
          H46 TYPE ZTPP_SEQ_SUM01-H46,
          H48 TYPE ZTPP_SEQ_SUM01-H48,
          H50 TYPE ZTPP_SEQ_SUM01-H50,
          H52 TYPE ZTPP_SEQ_SUM01-H52,
          H54 TYPE ZTPP_SEQ_SUM01-H54,
          H56 TYPE ZTPP_SEQ_SUM01-H56,
          H58 TYPE ZTPP_SEQ_SUM01-H58,
          H60 TYPE ZTPP_SEQ_SUM01-H60,
** Furong on 07/30/12 for 3 shift
          H62 TYPE ZTPP_SEQ_SUM01-H62,
          H64 TYPE ZTPP_SEQ_SUM01-H64,
          H66 TYPE ZTPP_SEQ_SUM01-H66,
          H68 TYPE ZTPP_SEQ_SUM01-H68,
          H70 TYPE ZTPP_SEQ_SUM01-H70,
          H72 TYPE ZTPP_SEQ_SUM01-H72,
** End

          MITU TYPE ZTPP_SEQ_SUM01-MITU,
          STOT TYPE ZTPP_SEQ_SUM01-STOT,
          FORE TYPE ZTPP_SEQ_SUM01-FORE,
          T01(5)    TYPE P,  "The Today's Summary
          T02(5)    TYPE P,  "The Today's Summary
          T03(5)    TYPE P,  "The Today's Summary
          GTOT(7) TYPE P,  " Grand Total
        END OF LT_HOUR.

  IF WA_MODEL   <> SPACE.
    LR_MODEL-SIGN = 'I'.
    LR_MODEL-OPTION = 'EQ'.
    LR_MODEL-LOW = WA_MODEL  .
    APPEND LR_MODEL.
  ENDIF.
  IF P_COLUMN_APP301 <> SPACE.
    LR_COL-SIGN = 'I'.
    LR_COL-OPTION = 'EQ'.
    L_C301 = P_COLUMN_APP301.  CONDENSE L_C301.
    CONCATENATE P_PART_APP301 L_C301 INTO LR_COL-LOW.
    APPEND LR_COL.
  ENDIF.
  IF P_CODE_APP301 <> SPACE.
    LR_CODE-SIGN = 'I'.
    LR_CODE-OPTION = 'EQ'.
    LR_CODE-LOW = P_CODE_APP301.
    APPEND LR_CODE.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_HOUR
    FROM ZTPP_SEQ_SUM01
    WHERE MODEL    IN LR_MODEL AND
          ALC_CODE IN LR_COL   AND
          ALC_VALS IN LR_CODE    .
  SORT LT_HOUR BY MODEL ALC_VALS .
  DELETE LT_HOUR WHERE ALC_VALS = 'BIP' OR ALC_VALS = 'BIW'.
  CLEAR: IT_HOUR_APP301, IT_HOUR_APP301[].
  LOOP AT LT_HOUR.
    CLEAR IT_HOUR_APP301.
** Furong on 07/30/12 for shift
    LT_HOUR-T01  = LT_HOUR-H02 + LT_HOUR-H04 + LT_HOUR-H06 +
     LT_HOUR-H08 + LT_HOUR-H10 + LT_HOUR-H12 + LT_HOUR-H14 +
     LT_HOUR-H16 + LT_HOUR-H18 + LT_HOUR-H20 + LT_HOUR-H22 +
     LT_HOUR-H24.

    LT_HOUR-T02  = LT_HOUR-H26 + LT_HOUR-H28 + LT_HOUR-H30 +
     LT_HOUR-H32 + LT_HOUR-H34 + LT_HOUR-H36 + LT_HOUR-H38 +
     LT_HOUR-H40 + LT_HOUR-H42 + LT_HOUR-H44 + LT_HOUR-H46 +
     LT_HOUR-H48.

    LT_HOUR-T03  = LT_HOUR-H50 + LT_HOUR-H52 + LT_HOUR-H54 +
     LT_HOUR-H56 + LT_HOUR-H58 + LT_HOUR-H60 + LT_HOUR-H62 +
     LT_HOUR-H64 + LT_HOUR-H66 + LT_HOUR-H68 + LT_HOUR-H70 +
     LT_HOUR-H72.

*    LT_HOUR-T01  = LT_HOUR-H02 + LT_HOUR-H04 + LT_HOUR-H06 +
*    LT_HOUR-H08  + LT_HOUR-H10 + LT_HOUR-H12 + LT_HOUR-H14 +
*                     LT_HOUR-H16 + LT_HOUR-H18 + LT_HOUR-H20 .
*    LT_HOUR-T02  = LT_HOUR-H22 + LT_HOUR-H24 + LT_HOUR-H26 +
*    LT_HOUR-H28  + LT_HOUR-H30 + LT_HOUR-H32 + LT_HOUR-H34 +
*                     LT_HOUR-H36 + LT_HOUR-H38 + LT_HOUR-H40 .
*    LT_HOUR-T03  = LT_HOUR-H42 + LT_HOUR-H44 + LT_HOUR-H46 +
*    LT_HOUR-H48  + LT_HOUR-H50 + LT_HOUR-H52 + LT_HOUR-H54 +
*                     LT_HOUR-H56 + LT_HOUR-H58 + LT_HOUR-H60 .

    LT_HOUR-GTOT = LT_HOUR-STOT + LT_HOUR-MITU.
    MOVE-CORRESPONDING LT_HOUR TO IT_HOUR_APP301.
    COLLECT IT_HOUR_APP301.
  ENDLOOP.
  LOOP AT IT_HOUR_APP301.
    IT_HOUR_APP301-ORDER = SY-TABIX.
    MODIFY IT_HOUR_APP301.
  ENDLOOP.
ENDFORM.                    " read_hour_table

*&---------------------------------------------------------------------*
*&      Form  read_day_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DAY_TABLE_APP301.
********************************************************************
* Because there is not Field 'PART' in The Table - ZTPP_SEQ_SUM02,
* You have to define The Parameter to search The Table.
* So, It has to be defined .
********************************************************************
  RANGES: LR_RP FOR ZTPP_SEQ_SUM01-RP,         "Reporting Point
          LR_MODEL FOR ZTPP_SEQ_SUM01-MODEL,   "Model
          LR_COL FOR ZTPP_SEQ_SUM01-ALC_CODE,  "Column
          LR_CODE FOR ZTPP_SEQ_SUM01-ALC_VALS. "Code

  DATA:  L_C301         LIKE P_COLUMN_APP301 ,
        BEGIN OF LT_DAY OCCURS 0,
*
          MODEL TYPE ZTPP_SEQ_SUM02-MODEL,
          ALC_VALS TYPE ZTPP_SEQ_SUM02-ALC_VALS,
*
*        serial TYPE ztpp_seq_sum02-serial,
*        alc_code TYPE ztpp_seq_sum02-alc_code,
*        rp TYPE ztpp_seq_sum02-rp,
*        d_1 TYPE ztpp_seq_sum02-d_1,
          SEQ TYPE ZTPP_SEQ_SUM02-SEQ,
          BODYIN TYPE ZTPP_SEQ_SUM02-BODYIN,
          WBS TYPE ZTPP_SEQ_SUM02-WBS,
          PAINT TYPE ZTPP_SEQ_SUM02-PAINT,
          PRJ TYPE ZTPP_SEQ_SUM02-PRJ,
          PBS TYPE ZTPP_SEQ_SUM02-PBS,
          D01 TYPE ZTPP_SEQ_SUM02-D01,
          D02 TYPE ZTPP_SEQ_SUM02-D02,
          D03 TYPE ZTPP_SEQ_SUM02-D03,
          D04 TYPE ZTPP_SEQ_SUM02-D04,
          D05 TYPE ZTPP_SEQ_SUM02-D05,
          D06 TYPE ZTPP_SEQ_SUM02-D06,
          D07 TYPE ZTPP_SEQ_SUM02-D07,
          D08 TYPE ZTPP_SEQ_SUM02-D08,
          D09 TYPE ZTPP_SEQ_SUM02-D09,
          D10 TYPE ZTPP_SEQ_SUM02-D10,
          D11 TYPE ZTPP_SEQ_SUM02-D11,
          D12 TYPE ZTPP_SEQ_SUM02-D12,
          D13 TYPE ZTPP_SEQ_SUM02-D13,
          D14 TYPE ZTPP_SEQ_SUM02-D14,
          D15 TYPE ZTPP_SEQ_SUM02-D15,
          D16 TYPE ZTPP_SEQ_SUM02-D16,
          D17 TYPE ZTPP_SEQ_SUM02-D17,
          D18 TYPE ZTPP_SEQ_SUM02-D18,
          D19 TYPE ZTPP_SEQ_SUM02-D19,
          D20 TYPE ZTPP_SEQ_SUM02-D20,
          D21 TYPE ZTPP_SEQ_SUM02-D21,
          MITU TYPE ZTPP_SEQ_SUM02-MITU,
          STOT TYPE ZTPP_SEQ_SUM02-STOT,
          FORE TYPE ZTPP_SEQ_SUM02-FORE,
        END OF LT_DAY.

  IF WA_MODEL   <> SPACE.
    LR_MODEL-SIGN = 'I'.
    LR_MODEL-OPTION = 'EQ'.
    LR_MODEL-LOW = WA_MODEL  .
    APPEND LR_MODEL.
  ENDIF.
  IF P_COLUMN_APP301 <> SPACE.
    LR_COL-SIGN = 'I'.
    LR_COL-OPTION = 'EQ'.
    L_C301 = P_COLUMN_APP301.  CONDENSE L_C301.
    CONCATENATE P_PART_APP301 L_C301  INTO LR_COL-LOW.
    APPEND LR_COL.
  ENDIF.
  IF P_CODE_APP301 <> SPACE.
    LR_CODE-SIGN = 'I'.
    LR_CODE-OPTION = 'EQ'.
    LR_CODE-LOW = P_CODE_APP301.
    APPEND LR_CODE.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_DAY
    FROM ZTPP_SEQ_SUM02
    WHERE MODEL    IN LR_MODEL AND
          ALC_CODE IN LR_COL   AND
          ALC_VALS IN LR_CODE    .
  SORT LT_DAY BY MODEL ALC_VALS .
  DELETE LT_DAY  WHERE ALC_VALS = 'BIP' OR ALC_VALS = 'BIW'.
  CLEAR: IT_DAY_APP301, IT_DAY_APP301[].
  LOOP AT LT_DAY.
    CLEAR IT_DAY_APP301.
    MOVE-CORRESPONDING LT_DAY TO IT_DAY_APP301.
    COLLECT IT_DAY_APP301.
  ENDLOOP.
  LOOP AT IT_DAY_APP301.
    IT_DAY_APP301-ORDER = SY-TABIX.
    MODIFY IT_DAY_APP301.
  ENDLOOP.
ENDFORM.                    " read_day_table

*&---------------------------------------------------------------------*
*&      Form  read_WEEK_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WEEK_TABLE_APP301.
********************************************************************
* Because there is not Field 'PART' in The Table - ZTPP_SEQ_SUM03,
* You have to define The Parameter to search The Table.
* So, It has to be defined .
********************************************************************
  RANGES: LR_RP FOR ZTPP_SEQ_SUM01-RP,         "Reporting Point
          LR_MODEL FOR ZTPP_SEQ_SUM01-MODEL,   "Model
          LR_COL FOR ZTPP_SEQ_SUM01-ALC_CODE,  "Column
          LR_CODE FOR ZTPP_SEQ_SUM01-ALC_VALS. "Code

  DATA:  L_C301         LIKE P_COLUMN_APP301 ,
        BEGIN OF LT_WEEK OCCURS 0,
          MODEL TYPE ZTPP_SEQ_SUM03-MODEL,
          ALC_VALS TYPE ZTPP_SEQ_SUM03-ALC_VALS,
          W_1 TYPE ZTPP_SEQ_SUM03-W_1,
          SEQ TYPE ZTPP_SEQ_SUM03-SEQ,
          BODYIN TYPE ZTPP_SEQ_SUM03-BODYIN,
          WBS TYPE ZTPP_SEQ_SUM03-WBS,
          PAINT TYPE ZTPP_SEQ_SUM03-PAINT,
          PRJ TYPE ZTPP_SEQ_SUM03-PRJ,
          PBS TYPE ZTPP_SEQ_SUM03-PBS,
          W01 TYPE ZTPP_SEQ_SUM03-W01,
          W02 TYPE ZTPP_SEQ_SUM03-W02,
          W03 TYPE ZTPP_SEQ_SUM03-W03,
          W04 TYPE ZTPP_SEQ_SUM03-W04,
          W05 TYPE ZTPP_SEQ_SUM03-W05,
          W06 TYPE ZTPP_SEQ_SUM03-W06,
          W07 TYPE ZTPP_SEQ_SUM03-W07,
          W08 TYPE ZTPP_SEQ_SUM03-W08,
          W09 TYPE ZTPP_SEQ_SUM03-W09,
          W10 TYPE ZTPP_SEQ_SUM03-W10,
          W11 TYPE ZTPP_SEQ_SUM03-W11,
          W12 TYPE ZTPP_SEQ_SUM03-W12,
          W13 TYPE ZTPP_SEQ_SUM03-W13,
          W14 TYPE ZTPP_SEQ_SUM03-W14,
          W15 TYPE ZTPP_SEQ_SUM03-W15,
          W16 TYPE ZTPP_SEQ_SUM03-W16,
          W17 TYPE ZTPP_SEQ_SUM03-W17,
          W18 TYPE ZTPP_SEQ_SUM03-W18,
          W19 TYPE ZTPP_SEQ_SUM03-W19,
          W20 TYPE ZTPP_SEQ_SUM03-W20,
          W21 TYPE ZTPP_SEQ_SUM03-W21,
          MITU TYPE ZTPP_SEQ_SUM03-MITU,
        END OF LT_WEEK.

  IF WA_MODEL  <> SPACE.
    LR_MODEL-SIGN = 'I'.
    LR_MODEL-OPTION = 'EQ'.
    LR_MODEL-LOW = WA_MODEL  .
    APPEND LR_MODEL.
  ENDIF.
  IF P_COLUMN_APP301 <> SPACE.
    LR_COL-SIGN = 'I'.
    LR_COL-OPTION = 'EQ'.
    L_C301 = P_COLUMN_APP301.  CONDENSE L_C301.
    CONCATENATE P_PART_APP301 L_C301 INTO LR_COL-LOW.
    APPEND LR_COL.
  ENDIF.
  IF P_CODE_APP301 <> SPACE.
    LR_CODE-SIGN = 'I'.
    LR_CODE-OPTION = 'EQ'.
    LR_CODE-LOW = P_CODE_APP301.
    APPEND LR_CODE.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_WEEK
    FROM ZTPP_SEQ_SUM03
    WHERE "rp       IN lr_rp    AND
          MODEL    IN LR_MODEL AND
          ALC_CODE IN LR_COL   AND
          ALC_VALS IN LR_CODE    .
  SORT LT_WEEK BY MODEL ALC_VALS .
  DELETE LT_WEEK WHERE ALC_VALS = 'BIP' OR ALC_VALS = 'BIW'.
  CLEAR: IT_WEEK_APP301, IT_WEEK_APP301[].
  LOOP AT LT_WEEK.
    CLEAR IT_WEEK_APP301.
    MOVE-CORRESPONDING LT_WEEK TO IT_WEEK_APP301.
    COLLECT IT_WEEK_APP301.
  ENDLOOP.
  LOOP AT IT_WEEK_APP301.
    IT_WEEK_APP301-ORDER = SY-TABIX.
    MODIFY IT_WEEK_APP301.
  ENDLOOP.
ENDFORM.                    " read_WEEK_table

*&---------------------------------------------------------------------*
*&      Form  check_parameters_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ERROR  text
*      -->P_L_TEXT  text
*----------------------------------------------------------------------*
FORM CHECK_PARAMETERS_APP301 USING    P_ERROR  P_TEXT.
  IF P_DATE_APP301 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'DATE'.
    EXIT.
  ENDIF.
  IF P_SL_APP301 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'Select Type'.
    EXIT.
  ENDIF.
  IF P_PART_APP301 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'PART'.
    EXIT.
  ENDIF.
  IF P_COLUMN_APP301 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'Column'.
    EXIT.
  ENDIF.
ENDFORM.                    " check_parameters_app301

*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_app301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP302.
* Plant
  CLEAR : XLIST, XLIST[],  XVALUE.
  NAME = 'WA_PLANT'.
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT    .

* Model
  CLEAR : XLIST, XLIST[],  XVALUE.
  NAME = 'WA_MODEL' .
  PERFORM SET_FIELD_MODEL USING NAME  WA_MODEL .

* Part
  CLEAR: XLIST, XLIST[],  XVALUE.
  NAME = 'P_PART_APP302'.
  PERFORM SET_FIELD_PART.
  PERFORM CALL_FUNCTION_VRM        USING XLIST.
* Column
  CLEAR: XLIST, XLIST[],  XVALUE.
  NAME = 'P_COLUMN_APP302'.
  PERFORM SET_FIELD_COLUMN .
  PERFORM CALL_FUNCTION_VRM        USING XLIST.
ENDFORM.                    " make_dropdown_list_box_APP302

*&---------------------------------------------------------------------*
*&      Form  search_data_APP302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP302.
  DATA: LT_APP302       LIKE TABLE OF IT_APP302  WITH HEADER LINE,
        L_ERROR,  L_TEXT(40).

  PERFORM CHECK_PARAMETERS_APP302 USING L_ERROR  L_TEXT .
  IF L_ERROR <> SPACE.
    CONCATENATE 'Set Parameter -' L_TEXT  INTO L_TEXT  SEPARATED BY ' '.
    MESSAGE S000 WITH L_TEXT .
    EXIT.
  ENDIF.

  CLEAR: IT_WO_APP302, IT_WO_APP302[], ST_APP302.
  PERFORM GET_PARAMETRS_VALS.  CONDENSE P_COLUMN_APP302.
* Read W/O Number & ALC Code's Value with Part, Column and Code's Value
  PERFORM GET_WONO_APP302 TABLES IT_WO_APP302
                          USING  WA_MODEL
                                 P_PART_APP302
                                 P_COLUMN_APP302
                                 P_CODE_APP302.
* Read Prod. Plan with Part, Column and Code's Value
* Read The Previous Prod. Result with ALC Code and RP
  PERFORM READ_DAY_PLAN_TABLE_APP302 USING P_PART_APP302
                                           P_COLUMN_APP302
                                           P_CODE_APP302  .
* Read Prod. Result Quantity with W/O Number
  PERFORM READ_RESULT_TABLE_APP302.

* Read today's Vehicle Status(PBS OUT)
  PERFORM READ_TODAY_STATUS       TABLES  LT_APP302       .

* Consolidate the Result for the Display..
  PERFORM CONCATE_RESULT          TABLES  LT_APP302       .
ENDFORM.                    " search_data_APP302

*&---------------------------------------------------------------------*
*&      Form  read_hour_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DAY_PLAN_TABLE_APP302 USING P_PART  P_COL  P_CODE .
  DATA: LT_DAY LIKE IT_APP302 OCCURS 0 WITH HEADER LINE ,
        L_ALC_CODE TYPE ZTPP_SEQ_SUM02-ALC_CODE         .
  RANGES: LR_ALC_VALS FOR ZTPP_SEQ_SUM02-ALC_VALS       .

  CONCATENATE P_PART P_COL  INTO L_ALC_CODE .

  IF P_CODE <> SPACE.
    LR_ALC_VALS-SIGN   = 'I'    .
    LR_ALC_VALS-OPTION = 'EQ'   .
    LR_ALC_VALS-LOW    = P_CODE .
    APPEND LR_ALC_VALS.
  ELSE.
    CLEAR: LR_ALC_VALS, LR_ALC_VALS[].
  ENDIF.

  CLEAR: LT_DAY, LT_DAY[].
  SELECT *
    FROM ZTPP_SEQ_SUM02
    WHERE ALC_CODE =  L_ALC_CODE  AND
          ALC_VALS IN LR_ALC_VALS   .
    CLEAR LT_DAY.
*   Daily Plan Quantity
    MOVE-CORRESPONDING ZTPP_SEQ_SUM02 TO LT_DAY.
*   Sum Plan Quantity
*    lt_day-tot_p = ztpp_seq_sum02-seq    +
*                   ztpp_seq_sum02-bodyin +
*                   ztpp_seq_sum02-wbs    +
*                   ztpp_seq_sum02-paint  +
*                   ztpp_seq_sum02-prj    +
*                   ztpp_seq_sum02-pbs    .
*   The Previous Prod. Result Quantity
    PERFORM PREVIOUS_RESULT_APP302 USING    ZTPP_SEQ_SUM02-ALC_CODE
                                            ZTPP_SEQ_SUM02-ALC_VALS
                                            ZTPP_SEQ_SUM02-RP
                                   CHANGING LT_DAY-B_RESULT       .
    APPEND LT_DAY .
  ENDSELECT.

  SORT LT_DAY BY MODEL ALC_VALS .

  CLEAR: IT_APP302, IT_APP302[].
  LOOP AT LT_DAY.
    CLEAR IT_APP302.
    MOVE-CORRESPONDING LT_DAY TO IT_APP302.
    MOVE WA_MODEL       TO IT_APP302-MODEL.
    COLLECT IT_APP302.
  ENDLOOP.
ENDFORM.                    " read_hour_table

*&---------------------------------------------------------------------*
*&      Form  sort_by_ascending_app302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_BY_ASCENDING_APP302.
  DATA: FIELD_NAME01(40),
        OFFSET01 TYPE I.
*
  GET CURSOR FIELD FIELD_NAME01.
*
  IF FIELD_NAME01(06) = 'IT_APP'.
    SEARCH FIELD_NAME01 FOR '-'.
    OFFSET01 = SY-FDPOS + 1.
    FIELD_NAME01 = FIELD_NAME01+OFFSET01.
    SORT IT_APP302 ASCENDING BY (FIELD_NAME01).
  ENDIF.

ENDFORM.                    " sort_by_ascending_app302
*&---------------------------------------------------------------------*
*&      Form  sort_by_desceding_app302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_4102  USING  PA_STYPE.
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP302-COLS  INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP302    ASCENDING  BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP302    DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " sort_SCREEN_4102

*&---------------------------------------------------------------------*
*&      Form  READ_RESULT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_RESULT_TABLE_APP302.
  DATA: L_ATINN     LIKE AUSP-ATINN,
        L_ATINN2    LIKE AUSP-ATINN,
        L_ATFLV     LIKE AUSP-ATFLV,
        L_NAME(30)  TYPE C         ,
        L_NUM(8)    TYPE N         ,
        LT_APP302_T LIKE IT_APP302 OCCURS 0 WITH HEADER LINE,
        LT_APP302   LIKE IT_APP302 OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_WO_APP302.
    CLEAR LT_APP302_T.
*   Result Quantity
    IF P_PART = 'U'.  "Work Order Header
      SELECT *
        INTO CORRESPONDING FIELDS OF LT_APP302_T
        FROM ZTPP_WOSUM
        WHERE WO_SER = IT_WO_APP302-WONO+00(09) AND
              NATION = IT_WO_APP302-WONO+09(03) AND
              DEALER = IT_WO_APP302-WONO+12(02)   .
*       Model
        MOVE WA_MODEL    TO LT_APP302_T-MODEL.
*       ALC Code's Value
        MOVE-CORRESPONDING IT_WO_APP302 TO LT_APP302_T.
        APPEND LT_APP302_T .
      ENDSELECT.
    ELSE.             "Work Order Color
      SELECT *
        INTO CORRESPONDING FIELDS OF LT_APP302_T
        FROM ZTPP_WOSUM
        WHERE WO_SER = IT_WO_APP302-WONO+00(09) AND
              NATION = IT_WO_APP302-WONO+09(03) AND
              DEALER = IT_WO_APP302-WONO+12(02) AND
              EXTC   = IT_WO_APP302-WONO+14(02) AND
              INTC   = IT_WO_APP302-WONO+16(02)   .
*       Model
        MOVE WA_MODEL  TO LT_APP302_T-MODEL.
*       ALC Code's Value
        MOVE-CORRESPONDING IT_WO_APP302 TO LT_APP302_T.
        APPEND LT_APP302_T.
      ENDSELECT.
    ENDIF.
  ENDLOOP.

  SORT LT_APP302_T BY MODEL ALC_VALS .
  LOOP AT LT_APP302_T.
    CLEAR LT_APP302.
    MOVE-CORRESPONDING LT_APP302_T TO LT_APP302.
    COLLECT LT_APP302.
  ENDLOOP.

  L_ATFLV = L_NUM = SY-DATUM.
  PERFORM READ_ATINN  USING 'P_RP06_SHOP_DATE' L_ATINN .
  PERFORM READ_ATINN  USING 'P_WORK_ORDER'     L_ATINN2.
ENDFORM.                    " READ_RESULT_TABLE

*&---------------------------------------------------------------------*
*&      Form  check_parameters_app302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ERROR  text
*      -->P_L_TEXT  text
*----------------------------------------------------------------------*
FORM CHECK_PARAMETERS_APP302 USING    P_ERROR  P_TEXT.
  IF P_DATE_APP302 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'DATE'.
    EXIT.
  ENDIF.
  IF P_PART_APP302 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'PART'.
    EXIT.
  ENDIF.
  IF P_COLUMN_APP302 = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'Column'.
    EXIT.
  ENDIF.
ENDFORM.                    " check_parameters_app302

*&---------------------------------------------------------------------*
*&      Form  data_select_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_3107.
  DATA: L_TEXT(50),
        L_FLAG,
        L_WO_SER(10), " TYPE ztpp_wosum-wo_ser,
        L_NATION(04), " TYPE ztpp_wosum-nation,
        L_DEALER(03), " TYPE ztpp_wosum-dealer,
        L_EXTC(04), "   TYPE ztpp_wosum-extc,
        L_INTC(04). "   TYPE ztpp_wosum-intc.

  PERFORM CHECK_ESSENTIAL_PRMTR_3107 USING L_TEXT  L_FLAG .

  IF L_FLAG = 'X'.
    CONCATENATE 'Set the essential parameter -'
                L_TEXT
      INTO L_TEXT SEPARATED BY SPACE.
    MESSAGE I000 WITH L_TEXT.
    EXIT.
  ENDIF.
  CLEAR: IT_3107, IT_3107[].

  PERFORM SET_PRMTR_OF_WONO_3107 USING    ST_3107_INPUT-ORDNO
                                          ST_3107_INPUT-EXTC
                                          ST_3107_INPUT-INTC
                                 CHANGING L_WO_SER
                                          L_NATION
                                          L_DEALER
                                          L_EXTC
                                          L_INTC  .

  PERFORM GET_WONO_FROM_WOSUM_3107 TABLES IT_3107
                                   USING  L_WO_SER
                                          L_NATION
                                          L_DEALER
                                          L_EXTC
                                          L_INTC   .

  PERFORM GET_DATA_FROM_MM03_3107.
ENDFORM.                    " data_select_3107

*&---------------------------------------------------------------------*
*&      Form  set_prmtr_of_wono_3107
*&---------------------------------------------------------------------*
FORM SET_PRMTR_OF_WONO_3107 USING    P_ORDER
                                     P_ECOLOR
                                     P_ICOLOR
                            CHANGING P_WO_SER  "09
                                     P_NATION  "03
                                     P_DEALER  "02
                                     P_EXTC    "02
                                     P_INTC.   "02
  CONCATENATE P_ORDER+00(09) '%'
    INTO P_WO_SER.
  CONCATENATE P_ORDER+09(03) '%'
    INTO P_NATION.
  CONCATENATE P_ORDER+12(02) '%'
    INTO P_DEALER.
  CONCATENATE P_ECOLOR '%'
    INTO P_EXTC.
  CONCATENATE P_ICOLOR '%'
    INTO P_INTC.

ENDFORM.                    " set_prmtr_of_wono_3107
*&---------------------------------------------------------------------*
*&      Form  get_wono_from_wosum_3107
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_3107  text
*      -->P_L_WO_SER  text
*      -->P_L_NATION  text
*      -->P_L_DEALER  text
*      -->P_L_EXTC  text
*      -->P_L_INTC  text
*----------------------------------------------------------------------*
FORM GET_WONO_FROM_WOSUM_3107 TABLES   P_IT_3107 STRUCTURE IT_3107
                              USING    P_WO_SER
                                       P_NATION
                                       P_DEALER
                                       P_EXTC
                                       P_INTC.
  SELECT *
    FROM ZTPP_WOSUM
    WHERE WO_SER LIKE P_WO_SER AND
          NATION LIKE P_NATION AND
          DEALER LIKE P_DEALER AND
          EXTC   LIKE P_EXTC   AND
          INTC   LIKE P_INTC      .

    CLEAR P_IT_3107.
    CONCATENATE ZTPP_WOSUM-WO_SER
                ZTPP_WOSUM-NATION
                ZTPP_WOSUM-DEALER
      INTO P_IT_3107-ORDER.
    MOVE ZTPP_WOSUM-EXTC TO P_IT_3107-EXTC.
    MOVE ZTPP_WOSUM-INTC TO P_IT_3107-INTC.
    APPEND P_IT_3107.

  ENDSELECT.
ENDFORM.                    " get_wono_from_wosum_3107

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CODE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_CODE_TABLE.
* Header Parts
  XVALUE-KEY  = 'HALC'.
  XVALUE-TEXT = 'Header-ALC'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'HHPCB'.
  XVALUE-TEXT = 'Header-HPC(B)'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'HHPCP'.
  XVALUE-TEXT = 'Header-HPC(P)'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'H219'.
  XVALUE-TEXT = 'Header-219'.
  APPEND XVALUE TO XLIST.

* Color Parts
  XVALUE-KEY  = 'CALC'.
  XVALUE-TEXT = 'Color-ALC'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'CHPCQ'.
  XVALUE-TEXT = 'Color-HPC(Q)'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'C219'.
  XVALUE-TEXT = 'Color-219'.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " SET_FIELD_CODE_TABLE

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CODE_TABLE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_CODE_TABLE1 .
* Header Parts
  XVALUE-KEY  = 'P_ALC_U_'.
  XVALUE-TEXT = 'HD-ALC(U)'.
  APPEND XVALUE TO XLIST.

* Color Parts
  XVALUE-KEY  = 'P_ALC_C_'.
  XVALUE-TEXT = 'CL-ALC(C)'.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " SET_FIELD_CODE_TABLE1

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CODE_TABLE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_CODE_TABLE2 .
* Header Parts
  XVALUE-KEY  = 'P_WO_HPC_B'   .
  XVALUE-TEXT = 'Header-HPC(B)'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY  = 'P_WO_HPC_P'   .
  XVALUE-TEXT = 'Header-HPC(P)'.
  APPEND XVALUE TO XLIST.

* Color Parts
  XVALUE-KEY  = 'P_WO_HPC_Q'   .
  XVALUE-TEXT = 'Color-HPC(Q)'.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " SET_FIELD_CODE_TABLE2

*&---------------------------------------------------------------------*
*&      Form  get_next_header_1002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_NEXT_HEADER_1002.
* Search The Present Work Order Header.
  READ TABLE IT_WOSUM INDEX 1 .
  READ TABLE WA_WOSUM WITH KEY WO_SER = IT_WOSUM-WO_SER
                               NATION = IT_WOSUM-NATION
                               DEALER = IT_WOSUM-DEALER .
  WA_INDEX = SY-TABIX .
  CASE SY-SUBRC.
    WHEN 0.
      CLEAR: WA_WOSUM-MARK.
      DO.
        WA_INDEX = WA_INDEX + 1.
        READ TABLE WA_WOSUM INDEX WA_INDEX.
        IF SY-SUBRC <> 0.
          EXIT.
        ENDIF.
        IF WA_WOSUM-WO_SER = IT_WOSUM-WO_SER AND
           WA_WOSUM-NATION = IT_WOSUM-NATION AND
           WA_WOSUM-DEALER = IT_WOSUM-DEALER   .
          CONTINUE.
        ELSE.
          CONCATENATE WA_WOSUM-WO_SER WA_WOSUM-NATION WA_WOSUM-DEALER
            INTO WA_ORDER .
          EXIT.
        ENDIF.
      ENDDO.
    WHEN OTHERS.
      READ TABLE WA_WOSUM INDEX 1.
      CONCATENATE WA_WOSUM-WO_SER WA_WOSUM-NATION  WA_WOSUM-DEALER
             INTO WA_ORDER .
  ENDCASE.
  WA_ECOLOR = WA_WOSUM-EXTC .
  WA_ICOLOR = WA_WOSUM-INTC .

ENDFORM.                    " get_next_header_1002

*&---------------------------------------------------------------------*
*&      Form  data_selection_219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECTION_ORDER2.

  DATA: IT_CONDITION LIKE ZSCA_CHARACTERISTIC_VALUE " Read condition
                          OCCURS 0 WITH HEADER LINE,
        IT_VEHICLE   LIKE ZSCA_VEHICLE_CHAR_VALUE
        OCCURS 0 WITH HEADER LINE.                   "Result
  DATA: LT_VALUE LIKE ZSCA_CHAR_VALUE OCCURS 0 WITH HEADER LINE.

  DATA : L_ALC_NAME(20) TYPE C,
         L_COUNT(3)     TYPE C,
         L_ATWRT_S LIKE AUSP-ATWRT,
         L_ATWRT_E LIKE AUSP-ATWRT,
         L_OBJEK   LIKE AUSP-OBJEK,
         L_KLART   LIKE AUSP-KLART.

  CLEAR : L_ATWRT_S, L_ATWRT_E.

  MOVE : ST_0111_INPUT-ORDER2(9) TO L_ATWRT_S ,
         ST_0111_INPUT-ORDER2(9) TO L_ATWRT_E ,
*        st_0111_input-order1    TO l_objek,
         '001'                   TO L_KLART.

  CONCATENATE ST_0111_INPUT-ORDER2
              ST_0111_INPUT-EXCLR2 ST_0111_INPUT-INCLR2
              INTO L_OBJEK.

  IF ST_0111_INPUT-EXCLR2 <> ''.

  ENDIF.

  CLEAR : L_COUNT.
  CLEAR : LT_VALUE, LT_VALUE[].

  DO 219 TIMES .
    L_COUNT = L_COUNT + 1.
* ALC
    CONDENSE L_COUNT.
    IF L_COUNT <=  200.
      CONCATENATE 'P_ALC_U_' L_COUNT INTO L_ALC_NAME.
      LT_VALUE-ATNAM = L_ALC_NAME . APPEND LT_VALUE.
      CLEAR : L_ALC_NAME  .
    ENDIF.

* 219
    CONCATENATE 'P_219_' L_COUNT INTO L_ALC_NAME.
    LT_VALUE-ATNAM = L_ALC_NAME . APPEND LT_VALUE.
    CLEAR : L_ALC_NAME  .
  ENDDO.

  LT_VALUE-ATNAM = 'P_MI' .      APPEND LT_VALUE.
  LT_VALUE-ATNAM = 'P_OCN' .     APPEND LT_VALUE.
  LT_VALUE-ATNAM = 'P_VERSION' . APPEND LT_VALUE.

  CALL FUNCTION 'Z_FCA_GET_WORK_ORDER_MASTER'
    EXPORTING
      I_ATNAM                       = 'P_WO_SER'
      I_ATWRT_S                     = L_ATWRT_S
      I_ATWRT_E                     = L_ATWRT_E
      I_OBJEK                       = L_OBJEK
      I_KLART                       = L_KLART
      I_COUNT                       = 1000000
    TABLES
      T_CONDITION                   = IT_CONDITION
      T_VEHICLE                     = IT_VEHICLE
      T_VALUE                       = LT_VALUE
    EXCEPTIONS
      DATE_OVERFLOW                 = 1
      INVALID_DATE                  = 2
      CONDITION_DOES_NOT_EXIST      = 3
      CHARACTERISTIC_DOES_NOT_EXIST = 4
      OTHERS                        = 5.

  CLEAR : L_COUNT.
* SPEC NO READ

  READ TABLE IT_VEHICLE WITH KEY ATNAM = 'P_MI'.
  IF SY-SUBRC EQ 0.
    MOVE : IT_VEHICLE-ATWRT TO  ST_0111_INPUT-MI2 .
  ENDIF.

  READ TABLE IT_VEHICLE WITH KEY ATNAM = 'P_OCN'.
  IF SY-SUBRC EQ 0.
    MOVE : IT_VEHICLE-ATWRT TO  ST_0111_INPUT-OCN2 .
  ENDIF.

  READ TABLE IT_VEHICLE WITH KEY ATNAM = 'P_VERSION'.
  IF SY-SUBRC EQ 0.
    MOVE : IT_VEHICLE-ATWRT TO  ST_0111_INPUT-VER2 .
  ENDIF.

* ALC

  DO 200 TIMES .
    CLEAR : L_ALC_NAME, IT_0111.
    L_COUNT = L_COUNT + 1.
    READ TABLE IT_0111 INDEX L_COUNT .

    CONDENSE L_COUNT.
    CONCATENATE 'P_ALC_U_' L_COUNT INTO L_ALC_NAME.
    READ TABLE IT_VEHICLE WITH KEY ATNAM = L_ALC_NAME.
    IF SY-SUBRC EQ 0.
      MOVE : IT_VEHICLE-ATWRT TO IT_0111-CODE2 .
    ENDIF.
    MODIFY  IT_0111 INDEX L_COUNT . CLEAR IT_0111.
  ENDDO.

  CLEAR : L_COUNT.

  DO 219 TIMES .
    CLEAR : L_ALC_NAME.
    L_COUNT = L_COUNT + 1.
    READ TABLE IT_0111_C INDEX L_COUNT.
    CONDENSE L_COUNT.
    CONCATENATE 'P_219_' L_COUNT INTO L_ALC_NAME.
    READ TABLE IT_VEHICLE WITH KEY ATNAM = L_ALC_NAME.

    IF SY-SUBRC EQ 0.
      MOVE : IT_VEHICLE-ATWRT TO IT_0111_C-CODE2 .
    ENDIF.
    MODIFY IT_0111_C INDEX L_COUNT . CLEAR IT_0111_C.
  ENDDO.

ENDFORM.                    " data_selection_219

*&---------------------------------------------------------------------*
*&      Form  get_next_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_NEXT_ERROR.
  CHECK WA_ERR_FLAG IS INITIAL .
  READ TABLE IT_MARA WITH KEY ERNAM = 'X'.
  WA_INDEX = SY-TABIX .
  CASE SY-SUBRC.
    WHEN 0.
      CLEAR: IT_MARA-ERNAM.
      MODIFY IT_MARA INDEX WA_INDEX TRANSPORTING ERNAM.
      WA_INDEX = WA_INDEX + 1.
      READ TABLE IT_MARA  INDEX WA_INDEX.
      IT_MARA-ERNAM = 'X'.
      WA_ORDER = IT_MARA-MATNR.
      MODIFY IT_MARA INDEX WA_INDEX TRANSPORTING ERNAM.
    WHEN OTHERS.
      READ TABLE IT_MARA INDEX 1.
      IT_MARA-ERNAM = 'X'.
      WA_ORDER = IT_MARA-MATNR.
      MODIFY IT_MARA INDEX 1 TRANSPORTING ERNAM.
  ENDCASE.
  DESCRIBE TABLE IT_MARA LINES WA_LINES.
  IF WA_LINES = 0 .
    WA_ERR_FLAG = 'X'.
  ENDIF.
ENDFORM.                    " get_next_ERROR

*&---------------------------------------------------------------------*
*&      Form  GET_ALC_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RESULT1001  text
*----------------------------------------------------------------------*
FORM GET_ALC_CONDITION TABLES   PA_VALS STRUCTURE  IT_RESULT1001
                       USING    PA_TYPE.
  DATA: LT_CUKB                 LIKE TABLE OF CUKB  WITH HEADER LINE,
        L_MODEL                 LIKE WA_CAR    ,
        L_LINES                 TYPE I,
        L_CNT                   TYPE I ,
        L_OBJKEY                LIKE CUXREF-OBJKEY,
        L_ERROR                 TYPE C,
        L_KNOBJ                 LIKE CUCO-KNOBJ,
        L_KNNAM                 LIKE CUKB-KNNAM.

*----> ALC Valeu Check for the existed with Error
  L_MODEL = WA_MODEL  .

  CLEAR: IT_ALC, IT_ALC[].

  CASE PA_TYPE.
    WHEN 'U'.
      CONCATENATE L_MODEL  '_WOHD'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
    WHEN 'C'.
      CONCATENATE L_MODEL  '_WOCL'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
  ENDCASE.

  LOOP AT IT_ALC  .
    SELECT SINGLE B~KNNAM T~KNKTX
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB AS B INNER JOIN CUKBT AS T
        ON B~KNNUM = T~KNNUM
     WHERE B~KNNUM = IT_ALC-KNNUM
       AND T~SPRAS = SY-LANGU   .

    IF IT_ALC-KNNAM(10) NE 'D_EMF_ALC_' .
      DELETE IT_ALC .
      CONTINUE .
    ENDIF.
    " Field meaning translate .. LT_CUKB-KNAUS : Characteristic Name..
    "                            LT_CUKB-KNART2: Unique / Head
    CONCATENATE 'P'  IT_ALC-KNNAM+5(10)  INTO IT_ALC-KNAUS    .
    IT_ALC-KNART2 = IT_ALC-KNNAM+10(1) .
    MODIFY IT_ALC  .
  ENDLOOP.

  LOOP AT IT_ALC  .
    READ TABLE PA_VALS WITH KEY ATNAM = IT_ALC-KNAUS .
    IF SY-SUBRC NE 0 OR PA_VALS-ATWRT = SPACE         .
      L_ERROR = 'X'.
      IF PA_TYPE = 'U'.
        IT_ERR-OBJKEY = WA_ORDER.
      ELSE.
        IT_ERR-OBJKEY = WA_COLOR.
      ENDIF.
      CONCATENATE 'ERROR:'  IT_ALC-KNAUS INTO IT_ERR-OBJTYP.
      APPEND IT_ERR.
    ENDIF.
  ENDLOOP.

  SORT IT_ERR BY OBJKEY.   CLEAR: L_LINES, L_CNT.
  READ TABLE IT_ERR INDEX 1.   L_OBJKEY = IT_ERR-OBJKEY.
  LOOP AT IT_ERR.
    IF L_OBJKEY = IT_ERR-OBJKEY..
      L_LINES = L_LINES + 1.
      CONTINUE.
    ELSE.
      L_CNT = L_CNT + 1 .
      IT_ERR-CUCOUNT = L_LINES.
      MODIFY IT_ERR TRANSPORTING CUCOUNT WHERE OBJKEY = L_OBJKEY .
      L_OBJKEY = IT_ERR-OBJKEY.
      L_LINES  = 1.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_ERR LINES WA_TOTQTY.
  IF WA_TOTQTY > 0.
    IT_ERR-CUCOUNT = L_LINES.
    L_CNT = L_CNT + 1 .
    MODIFY IT_ERR TRANSPORTING CUCOUNT WHERE OBJKEY = L_OBJKEY .
  ENDIF.

  WA_TOTQTY = L_CNT.
ENDFORM.                    " GET_ALC_CONDITION

*&---------------------------------------------------------------------*
*&      Form  DATA_NEXT_SELECTION_0111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_NEXT_SELECTION_0111.
  DATA: L_INDEX  TYPE I         ,
        L_OBJEK  LIKE AUSP-OBJEK,
        L_OBJEK2 LIKE AUSP-OBJEK.

  DESCRIBE TABLE WA_WOSUM_KEY LINES L_INDEX .
  READ TABLE WA_WOSUM_KEY WITH KEY WO_SER = ST_0111_INPUT-ORDER1 .
  IF L_INDEX > SY-TABIX.
    L_INDEX = SY-TABIX + 1 .
  ELSE.
    L_INDEX = 1 .
  ENDIF.

  READ TABLE WA_WOSUM_KEY INDEX L_INDEX .
  CONCATENATE WA_WOSUM_KEY-WO_SER   WA_WOSUM_KEY-NATION
              WA_WOSUM_KEY-DEALER   INTO ST_0111_INPUT-ORDER1 .

  PERFORM DATA_SELECT_0111 .
ENDFORM.                    " DATA_NEXT_SELECTION_0111

*&---------------------------------------------------------------------*
*&      Form  data_select_0118
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_0118.
  DATA: BEGIN OF LT_HEADER OCCURS 0,
          MATNR TYPE MARA-MATNR,
        END OF LT_HEADER ,
        L_HEADER TYPE MARA-MATNR,
        L_WO_SER TYPE AUSP-ATWRT,
        L_NATION TYPE AUSP-ATWRT,
        L_DEALER TYPE AUSP-ATWRT,
        L_SUM    TYPE P.
  RANGES: LR_HEADER FOR MARA-MATNR.

  IF ST_0118_INPUT-MODEL = SPACE.
    MESSAGE S000 WITH 'Set Parameter - Model!!'.
    EXIT.
  ENDIF.

  IF ST_0118_INPUT-MPACK = SPACE.
    CLEAR: LR_HEADER, LR_HEADER[].
  ELSE.
    CLEAR: LR_HEADER, LR_HEADER[].
    LR_HEADER-SIGN   = 'I' .
    LR_HEADER-OPTION = 'CP'.
    CONCATENATE ST_0118_INPUT-MPACK '*'
      INTO LR_HEADER-LOW.
    APPEND LR_HEADER.
  ENDIF.

  CLEAR: IT_0118, IT_0118[].
  SELECT DISTINCT MA~MATNR
    INTO CORRESPONDING FIELDS OF TABLE LT_HEADER
    FROM ( ( MARA AS MA
         INNER JOIN AUSP AS AU ON MA~MATNR = AU~OBJEK )
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
   WHERE MA~MTART = 'WOHD'              AND
         MA~MBRSH = 'A'                 AND
         MA~KZKFG = SPACE               AND
         CA~ATNAM = 'P_MODEL'           AND
         AU~ATWRT = ST_0118_INPUT-MODEL AND
         MA~MATNR IN LR_HEADER             .
  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH TEXT-100.
    EXIT.
  ENDIF.
  LOOP AT LT_HEADER .
    CLEAR IT_0118.
*   Model.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_MODEL'
                                   IT_0118-MODEL.
*   Order.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_WO_SER'
                                   L_WO_SER.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_NATION'
                                   L_NATION.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_DEALER'
                                   L_DEALER.
    CONCATENATE L_WO_SER
                L_NATION
                L_DEALER
      INTO IT_0118-ORDER.
*   Init.
    PERFORM READ_NUM_APP218 USING LT_HEADER-MATNR
                                  'P_INIT_QTY'
                                  IT_0118-INIT.
*   Modi.
    PERFORM READ_NUM_APP218 USING LT_HEADER-MATNR
                                  'P_MOD_QTY'
                                  IT_0118-MODI.
*   Sequ.
    PERFORM READ_NUM_APP218 USING LT_HEADER-MATNR
                                  'P_SEQ_QTY'
                                  IT_0118-SEQU.
*   MITU.
    PERFORM READ_NUM_APP218 USING LT_HEADER-MATNR
                                  'P_MITU_QTY'
                                  IT_0118-MITU.
*   Plan.
    PERFORM READ_NUM_APP218 USING LT_HEADER-MATNR
                                  'P_PLAN_QTY'
                                  IT_0118-PLAN.
*   Fore.
    PERFORM READ_NUM_APP218 USING LT_HEADER-MATNR
                                  'P_FORECAST_QTY'
                                  IT_0118-FORE.
*   MI.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_MI'
                                   IT_0118-MI.
*   OCN.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_OCN'
                                   IT_0118-OCN.
*   Vers.
    PERFORM READ_CHAR_APP218 USING LT_HEADER-MATNR
                                   'P_VERSION'
                                   IT_0118-VERS.
*   Message.
    L_SUM = IT_0118-SEQU + IT_0118-MITU .
    IF IT_0118-MODI < L_SUM.
      IT_0118-MESSAGE = 'Quantity Error!!'.
    ENDIF.
*Requested by Hur,20041020,changed by wskim
*-----Start
    SELECT COUNT( * ) INTO IT_0118-PLANW
        FROM ZTPP_PMT07JB_A
         WHERE MODL  EQ IT_0118-MODEL
           AND ORDR  EQ IT_0118-ORDER(9)
           AND DIST  EQ IT_0118-ORDER+9(5)
           AND GUBB  EQ 'A'
           AND GUB1  EQ '2'.
*-----End
    APPEND IT_0118.
  ENDLOOP.
ENDFORM.                    " data_select_0118

*&---------------------------------------------------------------------*
*&      Form  data_select_2479
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_2479.
  DATA : LT_AUSP     LIKE TABLE OF AUSP           WITH HEADER LINE,
         LT_VALUE    LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
         LW_AUSP     LIKE AUSP      ,
         L_DESEL     TYPE C         ,
         L_STATUS(2) TYPE N         ,
         L_DATE      TYPE D         ,
         L_NUM(8)    TYPE N         ,
         L_NAME(30)  TYPE C         ,
         L_OBJEK     LIKE MARA-MATNR,
         L_ATFLV     LIKE AUSP-ATFLV,
         L_ATINN     LIKE AUSP-ATINN,
         L_ATINN2    LIKE AUSP-ATINN.
  RANGES: R_ATWRT    FOR  AUSP-ATWRT.

  CLEAR: R_ATWRT, R_ATWRT[].
  PERFORM READ_ATINN USING 'P_RP_STATUS' L_ATINN .
  PERFORM READ_ATINN USING 'P_MODEL'     L_ATINN2.
  IF WA_MODEL     NE SPACE.
    R_ATWRT-SIGN = 'I'.                   R_ATWRT-OPTION = 'EQ'.
    R_ATWRT-LOW  = WA_MODEL .             APPEND R_ATWRT.
  ENDIF.

* Changed by I.G.MOON on 05/26/2010 {
*  SELECT *
*    FROM ausp INTO TABLE lt_ausp
*   WHERE objek IN ( select OBJEK from AUSP
*                                WHERE atinn = l_atinn
*                                  AND klart = '002'
*                                  AND atwrt = '18'  )
**                                  OR    atwrt = '19' ) )
*     AND atinn = l_atinn2
*     AND klart = '002'
*     AND atwrt IN r_atwrt .

  DATA   L_ATINN3    LIKE AUSP-ATINN.
  PERFORM READ_ATINN USING 'P_USAGE_CAR' L_ATINN3.

  DATA : BEGIN OF TMP_OBJEK OCCURS 0,
       OBJEK TYPE OBJNUM,
         END OF TMP_OBJEK.

  SELECT OBJEK INTO TABLE TMP_OBJEK FROM AUSP
                                  WHERE ATINN = L_ATINN
                                    AND KLART = '002'
                                    AND ATWRT = '18'.

  LOOP AT TMP_OBJEK.

    SELECT SINGLE OBJEK INTO AUSP-OBJEK FROM AUSP
     WHERE OBJEK EQ TMP_OBJEK-OBJEK
       AND ATINN = L_ATINN3
       AND ATWRT = 'D'.

    IF SY-SUBRC EQ 0.
      CONTINUE.
    ENDIF.

* now not disposal car

    SELECT SINGLE * INTO LT_AUSP FROM AUSP
     WHERE OBJEK EQ TMP_OBJEK-OBJEK
       AND ATINN = L_ATINN2
       AND KLART = '002'
       AND ATWRT IN R_ATWRT.
    IF SY-SUBRC EQ 0.
      APPEND LT_AUSP.
    ENDIF.

  ENDLOOP.

* }


  PERFORM READ_ATINN USING 'P_RP18_SHOP_DATE' L_ATINN .
  LOOP AT LT_AUSP.
    " Duration Check for the input condition.
    CLEAR: L_DESEL.
    SELECT SINGLE *
      FROM AUSP INTO LW_AUSP
     WHERE OBJEK = LT_AUSP-OBJEK
       AND ATINN = L_ATINN
       AND KLART = '002'        .
    L_DATE = L_NUM = LW_AUSP-ATFLV .

    CHECK ST_4279_INPUT-DURA > 0   .
    DO ST_4279_INPUT-DURA TIMES.
      L_DATE = L_DATE + 1      .
      PERFORM CALL_WORKDAY  USING L_DATE .
      IF L_DATE > SY-DATUM.
        L_DESEL = 'X'    .
      ENDIF.
    ENDDO.

    IF L_DESEL = 'X'.
      DELETE LT_AUSP.
    ENDIF.
  ENDLOOP.

  CLEAR: IT_4279[], IT_4279.

  LOOP AT LT_AUSP .
    CLEAR: LT_VALUE, LT_VALUE[].
    LT_VALUE-ATNAM = 'P_WORK_ORDER' .        APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_EXT_COLOR'  .        APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_INT_COLOR'  .        APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_MI'  .               APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_OCN'  .              APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_VERSION'  .          APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_RP_STATUS'  .        APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_RP18_SHOP_DATE' .    APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_RP19_SHOP_DATE' .    APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_RP01_SHOP_DATE' .    APPEND LT_VALUE.
    LT_VALUE-ATNAM = 'P_RP23_SHOP_DATE' .    APPEND LT_VALUE.

    L_OBJEK = LT_AUSP-OBJEK.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        OBJECT       = L_OBJEK
        MODE         = 'R'
        CTYPE        = '002'
*       DISPLAY      = 'D'
      TABLES
        VAL_TABLE    = LT_VALUE
      EXCEPTIONS
        NO_DATA      = 1
        ERROR_MODE   = 2
        ERROR_OBJECT = 3
        ERROR_VALUE  = 4
        OTHERS       = 5.

* MODEL &

    READ TABLE LT_VALUE INDEX 1 .

*   Requested by my Hur changed by chris

    IF LT_VALUE-ATWRT CS 'XX' OR
       LT_VALUE-ATWRT CS 'XY' OR
       LT_VALUE-ATWRT CS 'XA'.
      CONTINUE.
    ENDIF.
*   end of change on 06/21/2005

    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-ORDER.
      CLEAR: LT_VALUE .
    ENDIF.


    IT_4279-BODY = L_OBJEK .
    READ TABLE LT_VALUE INDEX 7 .
    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-STATUS.
      CLEAR: LT_VALUE .
    ENDIF.


    READ TABLE LT_VALUE INDEX 2 .
    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-ECLR.
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX 3 .
    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-ICLR.
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX 4 .
    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-MI.
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX 5 .
    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-OCN.
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX 6 .
    IF LT_VALUE-ZFLAG = SPACE   .
      MOVE : LT_VALUE-ATWRT TO IT_4279-VER.
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX  8.
    IF LT_VALUE-ZFLAG = SPACE   .
      IT_4279-C_GATE  = LT_VALUE-ATWRT   .
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX  9.
    IF LT_VALUE-ZFLAG = SPACE   .
      IT_4279-VPC     = LT_VALUE-ATWRT   .
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX 10.
    IF LT_VALUE-ZFLAG = SPACE   .
      IT_4279-PDATE   = LT_VALUE-ATWRT   .
      CLEAR: LT_VALUE .
    ENDIF.

    READ TABLE LT_VALUE INDEX  11.
    IF LT_VALUE-ZFLAG = SPACE   .
      IT_4279-SHIPOUT   = LT_VALUE-ATWRT   .
      CLEAR: LT_VALUE .
    ENDIF.

    APPEND IT_4279. CLEAR IT_4279.
  ENDLOOP.
ENDFORM.                    " data_select_2479

*&---------------------------------------------------------------------*
*&      Form  data_select_5290
*&---------------------------------------------------------------------*
* the all one hour gap was changed by chris on 05/11/2005 to consider
* the lunch(meal) break. the meal break is a separate display record.
* so the gap for break is not one hour. "UD1K915960
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_5290 USING P_SHIFT.
  DATA : LT_5290_T LIKE IT_5290 OCCURS 0 WITH HEADER LINE,
         LT_AUSP   LIKE TABLE OF AUSP    WITH HEADER LINE,
         L_ATINN   LIKE AUSP-ATINN ,
         L_ATINN2  LIKE AUSP-ATINN ,
         L_ATINN3  LIKE AUSP-ATINN ,
         L_ATFLV   LIKE AUSP-ATFLV ,
         L_TIME    TYPE KAPENDZT   ,
         L_GAP     TYPE KAPENDZT   ,
         L_CHECK   TYPE KAPENDZT   ,
         L_NUM(8)  TYPE N          ,
         L_CNT     TYPE I          ,
         L_IDX(2)  TYPE N          ,
         L_ZEIT    LIKE SY-UZEIT   ,
         IT_SUM    LIKE IT_5290 OCCURS 0 WITH HEADER LINE.
  DATA:  L_START_TIME LIKE ST_5290_INPUT-FTIME,
         L_END_TIME LIKE ST_5290_INPUT-TTIME,
         L_TEMP     LIKE ST_5290_INPUT-TTIME.
  DATA: WA_GAP01  TYPE I,
        WA_GAP02  TYPE I,
        WA_GAP03  TYPE I,
        WA_GAP04  TYPE I,
        WA_GAP05  TYPE I,
        WA_GAP06  TYPE I,
        WA_GAP07  TYPE I,
        WA_GAP08  TYPE I,
        WA_GAP09  TYPE I,
        WA_GAP10 TYPE I,
        WA_GAP11 TYPE I,
        WA_GAP12 TYPE I.

  CLEAR :  IT_5290_SHIFT, IT_5290_SHIFT[].
  IF ST_5290_INPUT-DATE IS INITIAL .
    MESSAGE I000  WITH  TEXT-013.
    EXIT.
  ENDIF.
* Making the hour gap
  PERFORM MAKE_GAP USING P_SHIFT                            "UD1K915960
                         WA_GAP01 WA_GAP02 WA_GAP03         "UD1K915960
                         WA_GAP04 WA_GAP05 WA_GAP06         "UD1K915960
                         WA_GAP07 WA_GAP08 WA_GAP09         "UD1K915960
                         WA_GAP10 WA_GAP11 WA_GAP12         "UD1K915960
                         L_CNT.

** Furong on 06/12/12 for 3 shift
*  IF p_shift = 1.                                           "UD1K912931
*    l_start_time = st_5290_input-ftime.                     "UD1K912931
*    l_end_time   = st_5290_input-ttime .                    "UD1K912931
*  ELSEIF p_shift = 2.                                       "UD1K912931
*    l_start_time = st_5290_input-ftime_2.                   "UD1K912931
*    l_end_time   = st_5290_input-ttime_2.                   "UD1K912931
*    wa_gap12 = wa_gap12 + 1800.
**  IF THE SHIFT CROSS THE MIDDLE NIGHT
*    IF l_start_time > l_end_time.
*      l_end_time = l_end_time + 86400.
*    ENDIF.
*  ELSE.                                                     "UD1K912931
*    MESSAGE e001 WITH 'Shift Not EXist :' p_shift.          "UD1K912931
*  ENDIF.

  IF P_SHIFT = 1.                                           "UD1K912931
    L_START_TIME = ST_5290_INPUT-FTIME.                     "UD1K912931
    L_END_TIME   = ST_5290_INPUT-TTIME .                    "UD1K912931
  ELSEIF P_SHIFT = 2.                                       "UD1K912931
    L_START_TIME = ST_5290_INPUT-FTIME_2.
    L_END_TIME   = ST_5290_INPUT-TTIME_2.
  ELSEIF P_SHIFT = 3.                                       "UD1K912931
    L_START_TIME = ST_5290_INPUT-FTIME_3.                   "UD1K912931
    L_END_TIME   = ST_5290_INPUT-TTIME_3.                   "UD1K912931
*    wa_gap12 = wa_gap12 + 1800.
    WA_GAP08 = WA_GAP08 + 900.                              " 15 mins
**  IF THE SHIFT CROSS THE MIDDLE NIGHT
    IF L_START_TIME > L_END_TIME.
      L_END_TIME = L_END_TIME + 86400.
    ENDIF.
  ELSE.                                                     "UD1K912931
    MESSAGE E001 WITH 'Shift Not EXist :' P_SHIFT.          "UD1K912931
  ENDIF.
** End on 06/12/12

* Initial Record for the Internal Table.. (For the basic data display)
* Consider the shift cross middle night

  CLEAR: L_IDX.
*  IF L_START_TIME <= L_END_TIME.
*   l_cnt = ceil( ( L_END_TIME  - L_START_TIME ) / 3600 ) .
*  ELSE.
*   L_CNT = CEIL( ( 86400 - L_END_TIME + L_START_TIME ) / 3600 ) .
*  ENDIF.

*  it_5290time
  DO L_CNT TIMES .
    L_IDX = L_IDX + 1.
    LT_5290_T-TIME = LT_5290_T-TYPE = L_IDX .

    APPEND LT_5290_T.     CLEAR LT_5290_T .
  ENDDO.

*    ADD LOGIC TO ALLOW CARS PRODUCED AFTER THE END TIME TO BE
*    COUNTED. if EXTEND THE SHIFT END TIME At this point, extended
*    time will not display if no cars produces after the shift end time
*    If cars exist, it will be in the the extra hour. For first shift,
*    the extended time must be less than the start time of the
*    second shift.

*  if P_shift = 1.                             "UD1K912931
*    l_temp = l_end_time + 3600.               "UD1K912931
*    if st_5290_input-ftime_2 ne 0.            "UD1K912931
*      if st_5290_input-ftime lt l_temp.       "UD1K912931
*        l_end_time = st_5290_input-ftime.     "UD1K912931
*      else.                                   "UD1K912931
*        l_end_time = l_temp.                  "UD1K912931
*      endif.                                  "UD1K912931
*    else.                                     "UD1K912931
*      l_end_time = l_temp.                    "UD1K912931
*    endif.                                    "UD1K912931
*
*  elseif P_shift = 2.                         "UD1K912931
*    l_end_time = l_end_time + 3600.           "UD1K912931
*  endif.                                      "UD1K912931


  CLEAR: L_IDX.
  PERFORM READ_ATINN USING 'P_RP01_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP01_ACTUAL_DATE' L_ATINN2.
  PERFORM READ_ATINN USING 'P_WORD_ORDER'       L_ATINN3.
  L_ATFLV = L_NUM = ST_5290_INPUT-DATE .
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .
* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-BI.

  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.

      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .       " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP < WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-BI   = LT_5290_T-BI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.                                                   " Over  8
*      LT_5290_T-TIME = LT_5290_T-TYPE = '9'.
*      LT_5290_T-BI   = LT_5290_T-BI + 1     .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-bi   = lt_5290_t-bi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-bi   = lt_5290_t-bi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-bi   = lt_5290_t-bi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-bi   = lt_5290_t-bi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                              " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-bi   = lt_5290_t-bi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP02_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP02_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-PI.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931

    ENDIF.

    " Check the Interval for the SHIFT's time
*    IF st_5290_input-ttime >= l_check AND             "UD1K912931
*       st_5290_input-ftime <= l_check .               "UD1K912931
    IF L_END_TIME >= L_CHECK AND                            "UD1K912931
       L_START_TIME <= L_CHECK .                            "UD1K912931

    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .       " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-PI   = LT_5290_T-PI + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.                                             " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-PI   = LT_5290_T-PI + 1     .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-pi   = lt_5290_t-pi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-pi   = lt_5290_t-pi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-pi   = lt_5290_t-pi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-pi   = lt_5290_t-pi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                             " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-pi   = lt_5290_t-pi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** end on 06/12/12

  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP03_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP03_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-TC.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME   >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-TC   = LT_5290_T-TC + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .

** Furong on 06/12/12 for 3 shift
*    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-TC   = LT_5290_T-TC + 1     .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-tc   = lt_5290_t-tc + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-tc   = lt_5290_t-tc + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-tc   = lt_5290_t-tc + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-tc   = lt_5290_t-tc + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-tc   = lt_5290_t-tc + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP04_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP04_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-PO.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-PO   = LT_5290_T-PO + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-PO   = LT_5290_T-PO + 1     .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-po   = lt_5290_t-po + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-po   = lt_5290_t-po + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-po   = lt_5290_t-po + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-po   = lt_5290_t-po + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-po   = lt_5290_t-po + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12

  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP05_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP05_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-PBSI.

  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-PBSI = LT_5290_T-PBSI + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-pbsi   = lt_5290_t-pbsi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-pbsi   = lt_5290_t-pbsi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-pbsi   = lt_5290_t-pbsi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-pbsi   = lt_5290_t-pbsi + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-pbsi = lt_5290_t-pbsi + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

*  "PBS-OUT means the PBS OUT for the production result...
  PERFORM READ_ATINN USING 'P_RP06_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP06_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-PBSO.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-PBSO = LT_5290_T-PBSO + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-pbso   = lt_5290_t-pbso + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-pbso   = lt_5290_t-pbso + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-pbso   = lt_5290_t-pbso + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-pbso   = lt_5290_t-pbso + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-pbso = lt_5290_t-pbso + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].


  " Trim-In means the PBS OUT for the production result...
  PERFORM READ_ATINN USING 'P_RP07_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP07_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-TRIM.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-TRIM = LT_5290_T-TRIM + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-trim   = lt_5290_t-trim + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-trim   = lt_5290_t-trim + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-trim   = lt_5290_t-trim + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-trim   = lt_5290_t-trim + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-trim = lt_5290_t-trim + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP17_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP17_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-CF.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-CF   = LT_5290_T-CF + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '13'.
*      LT_5290_T-CF   = LT_5290_T-CF + 1     .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-cf   = lt_5290_t-cf + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-cf   = lt_5290_t-cf + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-cf   = lt_5290_t-cf + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-cf   = lt_5290_t-cf + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-cf   = lt_5290_t-cf + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP18_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP18_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-SOFF.

  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '13'.
*      LT_5290_T-SOFF = LT_5290_T-SOFF + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-soff   = lt_5290_t-soff + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-soff   = lt_5290_t-soff + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-soff   = lt_5290_t-soff + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-soff   = lt_5290_t-soff + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-soff = lt_5290_t-soff + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP19_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP19_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-CG.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-CG   = LT_5290_T-CG + 1     .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-CG   = LT_5290_T-CG + 1     .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-cg   = lt_5290_t-cg + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-cg   = lt_5290_t-cg + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-cg   = lt_5290_t-cg + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-cg   = lt_5290_t-cg + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-cg   = lt_5290_t-cg + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

  PERFORM READ_ATINN USING 'P_RP22_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP22_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-VPCI.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '13'.
*      LT_5290_T-VPCI = LT_5290_T-VPCI + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.
*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-vpci   = lt_5290_t-vpci + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-vpci   = lt_5290_t-vpci + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-vpci   = lt_5290_t-vpci + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-vpci   = lt_5290_t-vpci + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-vpci = lt_5290_t-vpci + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

** changed by Furong on 06/18/09

  PERFORM READ_ATINN USING 'P_RP23_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP23_ACTUAL_DATE' L_ATINN2.
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-VPCO.

  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 . " Under  2 Hour

      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-VPCO = LT_5290_T-VPCO + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-vpco   = lt_5290_t-vpco + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-vpco   = lt_5290_t-vpco + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-vpco   = lt_5290_t-vpco + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-vpco   = lt_5290_t-vpco + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-vpco = lt_5290_t-vpco + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

***** added by chris on 06/09/2005 requested by MY HUR

* shipping in
  DATA: L_ATINN_E  LIKE CABN-ATINN,
        L_ATINN2_E LIKE CABN-ATINN.

  PERFORM READ_ATINN USING 'P_RP24_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP24_ACTUAL_DATE' L_ATINN2.
  PERFORM READ_ATINN USING 'P_RP26_SHOP_DATE'   L_ATINN_E .
  PERFORM READ_ATINN USING 'P_RP26_ACTUAL_DATE'   L_ATINN2_E .

  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .

  SELECT * APPENDING TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN_E
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2_E
     AND KLART = '002'   .



* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-SHIPIN.


  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.                                        " Over  12 Hour
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-SHIPIN = LT_5290_T-SHIPIN + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-shipin   = lt_5290_t-shipin + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-shipin   = lt_5290_t-shipin + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-shipin   = lt_5290_t-shipin + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-shipin   = lt_5290_t-shipin + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-shipin = lt_5290_t-shipin + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].

* shipping  out
  PERFORM READ_ATINN USING 'P_RP25_SHOP_DATE'   L_ATINN .
  PERFORM READ_ATINN USING 'P_RP25_ACTUAL_DATE' L_ATINN2.
  PERFORM READ_ATINN USING 'P_RP27_SHOP_DATE'   L_ATINN_E .
  PERFORM READ_ATINN USING 'P_RP27_ACTUAL_DATE'   L_ATINN2_E .

  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2
     AND KLART = '002'   .
  SELECT * APPENDING TABLE LT_AUSP
    FROM AUSP
   WHERE OBJEK IN ( SELECT OBJEK FROM AUSP WHERE ATINN = L_ATINN_E
                                             AND KLART = '002'
                                             AND ATFLV = L_ATFLV )
     AND ATINN = L_ATINN2_E
     AND KLART = '002'   .



* check the cars are 'XX' or 'XY' car
  PERFORM CHECK_XX_XY TABLES LT_AUSP.

* total:
  DESCRIBE TABLE LT_AUSP LINES WA_TOTAL_5290-SHIPOUT.

  LOOP AT LT_AUSP.
    L_TIME = L_ZEIT  = LT_AUSP-ATWRT+8(6).
    IF LT_AUSP-ATWRT(8) = ST_5290_INPUT-DATE .
      L_CHECK = L_TIME .
      L_GAP   = L_TIME - L_START_TIME  .
    ELSE.
      L_CHECK = L_GAP   = L_TIME + ( 86400 - L_START_TIME ) .
      L_CHECK = 86400 + L_TIME.                             "UD1K912931
    ENDIF.

    " Check the Interval for the SHIFT's time
    IF L_END_TIME >= L_CHECK AND
       L_START_TIME <= L_CHECK .
    ELSE.
      DELETE LT_AUSP.
      CONTINUE.
    ENDIF.

    IF     L_GAP >=  0    AND L_GAP <  WA_GAP01 .    " Under  1 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '01'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP01 AND L_GAP <  WA_GAP02 .    " Under  2 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '02'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >=  WA_GAP02 AND L_GAP < WA_GAP03 .    " Under  3 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '03'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP03 AND L_GAP < WA_GAP04 .    " Under  4 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '04'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP04 AND L_GAP < WA_GAP05 .    " Under  5 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '05'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP05 AND L_GAP < WA_GAP06 .    " Under  6 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '06'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP06 AND L_GAP < WA_GAP07 .    " Under  7 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '07'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ELSEIF L_GAP >= WA_GAP07 AND L_GAP < WA_GAP08 .    " Under  8 Hour
      LT_5290_T-TIME = LT_5290_T-TYPE = '08'.
      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
      APPEND LT_5290_T.     CLEAR LT_5290_T .
** Furong on 06/12/12 for 3 shift
*    ELSE.
*      LT_5290_T-TIME = LT_5290_T-TYPE = '09'.
*      LT_5290_T-SHIPOUT = LT_5290_T-SHIPOUT + 1   .
*      APPEND LT_5290_T.     CLEAR LT_5290_T .
    ENDIF.

*    ELSEIF l_gap >= wa_gap08 AND l_gap < wa_gap09 .    " Under  9 Hour
*      lt_5290_t-time = lt_5290_t-type = '09'.
*      lt_5290_t-shipout   = lt_5290_t-shipout + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap09 AND l_gap < wa_gap10 .    " Under 10 Hour
*      lt_5290_t-time = lt_5290_t-type = '10'.
*      lt_5290_t-shipout   = lt_5290_t-shipout + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap10 AND l_gap < wa_gap11 .    " Under 11 Hour
*      lt_5290_t-time = lt_5290_t-type = '11'.
*      lt_5290_t-shipout   = lt_5290_t-shipout + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSEIF l_gap >= wa_gap11 AND l_gap < wa_gap12 .    " Under 12 Hour
*      lt_5290_t-time = lt_5290_t-type = '12'.
*      lt_5290_t-shipout   = lt_5290_t-shipout + 1     .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ELSE.                                        " Over  12 Hour
*      lt_5290_t-time = lt_5290_t-type = '13'.
*      lt_5290_t-shipout = lt_5290_t-shipout + 1   .
*      APPEND lt_5290_t.     CLEAR lt_5290_t .
*    ENDIF.
** End on 06/12/12
  ENDLOOP.
  CLEAR: LT_AUSP, LT_AUSP[].


***** end of add on 06/09/2005
  SORT LT_5290_T BY TYPE .
  LOOP AT LT_5290_T .
    CLEAR IT_5290_SHIFT.
    MOVE-CORRESPONDING LT_5290_T TO IT_5290_SHIFT.
*   REQUESTED BY MY HUR CHANGED BY YONGPING
*   CONCATENATE 'Hour:' it_5290-time INTO it_5290-time SEPARATED BY ' '.
    PERFORM GET_ACTUAL_PERIOD_TIME
                      USING LT_5290_T-TIME
                      IT_5290_SHIFT-TIME
                      P_SHIFT.
*   END OF CHANGE ON 11/10/2004
    CONCATENATE 'H'     IT_5290_SHIFT-TYPE INTO IT_5290_SHIFT-TYPE .
    COLLECT IT_5290_SHIFT.
  ENDLOOP.
* summary shift total
  DATA: L_SHIFT(10).
  IF P_SHIFT = 1.
    L_SHIFT = '1st'.
  ELSEIF P_SHIFT = 2.
    L_SHIFT = '2nd'.
** Furong on 06/12/12 for 3 shift
  ELSEIF P_SHIFT = 3.
    L_SHIFT = '3rd'.
** End on 06/12/12
  ENDIF.
  CONCATENATE L_SHIFT 'total' INTO L_SHIFT
     SEPARATED BY SPACE.
  CLEAR: IT_SUM[], IT_SUM.
  LOOP AT IT_5290_SHIFT.
    CLEAR: IT_5290_SHIFT-TIME.
    CLEAR: IT_5290_SHIFT-TYPE.
    COLLECT IT_5290_SHIFT INTO IT_SUM.
  ENDLOOP.
  READ TABLE IT_SUM INDEX 1.
  IT_SUM-TIME = L_SHIFT.
  APPEND IT_SUM TO IT_5290_SHIFT.

  DATA: L_PLAN    LIKE IT_5290_SHIFT-PLAN.
  CLEAR: IT_SUM[], IT_SUM.
  LOOP AT IT_5290_SHIFT.

** Furong on 06/12/12 for 3 shift
*    IF p_shift = 1.
*      IF it_5290_shift-time = l_shift.
*        l_plan = st_5290_input-lqty .
*      ELSE.
*        l_plan = st_5290_input-uph_l .
*      ENDIF.
*    ELSE.
*      IF it_5290_shift-time = l_shift.
*        l_plan = st_5290_input-hqty .
*      ELSE.
*        l_plan = st_5290_input-uph_h .
*      ENDIF.
*    ENDIF.

    CASE P_SHIFT.
      WHEN 1.
        IF IT_5290_SHIFT-TIME = L_SHIFT.
          L_PLAN = ST_5290_INPUT-LQTY .
        ELSE.
          L_PLAN = ST_5290_INPUT-UPH_L .
        ENDIF.
      WHEN 2.
        IF IT_5290_SHIFT-TIME = L_SHIFT.
          L_PLAN = ST_5290_INPUT-HQTY .
        ELSE.
          L_PLAN = ST_5290_INPUT-UPH_H .
        ENDIF.
      WHEN 3.
        IF IT_5290_SHIFT-TIME = L_SHIFT.
          L_PLAN = ST_5290_INPUT-TQTY .
        ELSE.
          L_PLAN = ST_5290_INPUT-UPH_T .
        ENDIF.
    ENDCASE.
** End on 06/12/12

    IF L_PLAN = 0.
      IT_5290_SHIFT-RATION = 0.
      IT_5290_SHIFT-PLAN   = 0.
    ELSE.
      IT_5290_SHIFT-RATION = IT_5290_SHIFT-SOFF / L_PLAN * 100 .
      IT_5290_SHIFT-PLAN   = IT_5290_SHIFT-TRIM / L_PLAN * 100 .
    ENDIF.
    MODIFY IT_5290_SHIFT  .
*    MOVE it_5290_SHIFT TO it_sum.
*    if it_sum-time ne l_shift.
*      CLEAR: it_sum-time, it_sum-type.
*      COLLECT it_sum.
*    endif.
  ENDLOOP.

*  READ TABLE it_sum INDEX 1.
*  it_sum-time   = 'SUM'.
* CHANGED BY chris LI
* st_5290_input-dayu COULD BE ZERO AND CAUSE RUNTIME ERROR
*  IF ST_5290_INPUT-DAYU = 0.                              "UD1K912931
*    IT_SUM-RATION = 0.                                    "UD1K912931
*    IT_SUM-PLAN   = 0.                                    "UD1K912931
*  ELSE.                                                   "UD1K912931
** END OF CHANGE ON 11/10/2004
*    it_sum-ration = it_sum-soff / st_5290_input-dayu * 100 .
*    it_sum-plan   = it_sum-trim / st_5290_input-dayu * 100 .
*  ENDIF.                                                  "UD1K912931
*  APPEND it_sum TO it_5290_SHIFT .
ENDFORM.                    " data_select_5290

*&---------------------------------------------------------------------*
*&      Form  data_select_5291
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_5291.
  TABLES : ZTPP_ALCLOG1,
           LDLT,
           V_TC37P. " Break schedule

  DATA : WA_BE    LIKE TC37P-PAUBEG,
         WA_EN    LIKE TC37P-PAUBEG,
         WA_SUM   LIKE TC37P-PADAUER,
         WA_TIME  LIKE KAPA-BEGZT.

  DATA :   WA_P TYPE P,
           WA_RATE  LIKE IT_5291-TRIM_P.

  DATA : IT_SUM LIKE IT_5290 OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE  * FROM ZTPP_ALCLOG1
                   WHERE CDATE = '20031125' . " sy-datum

  CLEAR :  IT_5291, IT_5291[].
  MOVE : ZTPP_ALCLOG1-PLANT      TO IT_5291-PLANT,
         ZTPP_ALCLOG1-POINT07    TO IT_5291-TRIM_S,
         ZTPP_ALCLOG1-POINT08    TO IT_5291-CFIN_S,
         ZTPP_ALCLOG1-POINT09    TO IT_5291-SOFF_S.

  APPEND IT_5291.


  LOOP AT IT_5291.
    CALL FUNCTION 'Z_FPP_UPH_TIME'
      EXPORTING
        INPUT_DATE = SY-DATUM
        INPUT_TIME = SY-UZEIT
      IMPORTING
        RATE       = WA_P.

    MOVE WA_P TO WA_RATE.

    MOVE : WA_RATE TO IT_5291-TRIM_P,
           WA_RATE TO IT_5291-CFIN_P,
           WA_RATE TO IT_5291-SOFF_P.

    IF IT_5291-TRIM_P NE 0.
      IT_5291-TRIM_R = IT_5291-TRIM_S / IT_5291-TRIM_P * 100.
    ENDIF.

    IF IT_5291-CFIN_P NE 0.
      IT_5291-CFIN_R = IT_5291-CFIN_S / IT_5291-CFIN_P * 100.
    ENDIF.

    IF IT_5291-SOFF_P NE 0.
      IT_5291-SOFF_R = IT_5291-SOFF_S / IT_5291-SOFF_P * 100.
    ENDIF.
    MODIFY IT_5291.
  ENDLOOP.

ENDFORM.                    " data_select_5291
*&---------------------------------------------------------------------*
*&      Form  data_select_5293
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_5293.
  DATA : IT_SUM LIKE IT_5293 OCCURS 0 WITH HEADER LINE,
         IT_XXX LIKE ZTPP_ALCLOG2 OCCURS 0 WITH HEADER LINE,
         IT_YYY LIKE ZTPP_ALCLOG2 OCCURS 0 WITH HEADER LINE.

  DATA : WA_TIME LIKE SY-UZEIT,
         WA_TMP(6) TYPE C,
         WA_RATE LIKE ST_5293_INPUT-BI.

  RANGES : R_USAGE FOR ZTPP_ALCLOG2-ZUSAGE.
  CLEAR : R_USAGE[], R_USAGE.

  IF  ST_5293_INPUT-DATE IS INITIAL  .
    MESSAGE I000 WITH 'Set Date Parameter!!'.
    EXIT.
  ENDIF.

  IF ST_5293_INPUT-USE <> SPACE.
    R_USAGE-OPTION = 'EQ'.
    R_USAGE-SIGN   = 'I'.
    R_USAGE-LOW    = ST_5293_INPUT-USE.
    APPEND R_USAGE.
  ENDIF.

  SELECT  * INTO TABLE IT_XXX FROM ZTPP_ALCLOG2
                   WHERE CDATE   = ST_5293_INPUT-DATE
                     AND ZUSAGE  IN R_USAGE .

  CLEAR : IT_YYY , IT_YYY[].

  LOOP AT IT_XXX.
    MOVE IT_XXX TO IT_YYY.
    COLLECT IT_YYY. CLEAR IT_YYY.
  ENDLOOP.

  CLEAR :  IT_5293, IT_5293[], ZTPP_ALCLOG2 .
  LOOP AT IT_YYY.
    MOVE IT_YYY TO    ZTPP_ALCLOG2 .
    IF ST_5293_INPUT-DAY <> '2'.
      MOVE :    '06 ~ 08 '              TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_D01  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_D01  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_D01  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_D01  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_D01  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_D01  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_D01  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_D01  TO IT_5293-SOFF,
*            ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_D01  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_D01  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_D01  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '08 ~ 10 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_D02  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_D02  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_D02  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_D02  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_D02  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_D02  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_D02  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_02 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_D02  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_D02  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_D02  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_D02  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '10 ~ 12 '              TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_D03  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_D03  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_D03  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_D03  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_D03  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_D03  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_D03  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_03 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_D03  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_D03  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_D03  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_D03  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '13 ~ 15 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_D04  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_D04  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_D04  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_D04  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_D04  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_D04  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_D04  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_04 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_D04  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_D04  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_D04  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_D04  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '15 ~ 17 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_D05  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_D05  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_D05  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_D05  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_D05  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_D05  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_D05  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_D05  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5293-ration,
                ZTPP_ALCLOG2-QTY10_D05  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_D05  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_D05  TO IT_5293-VPO.
      PERFORM APPEND_5293  .
    ENDIF.
    IF ST_5293_INPUT-DAY <> '1'.

      MOVE :    '18 ~ 20 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_N01  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_N01  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_N01  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_N01  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_N01  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_N01  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_N01  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_N01  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_N01  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_N01  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_N01  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '21 ~ 23 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_N02  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_N02  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_N02  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_N02  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_N02  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_N02  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_N02  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_N02  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_N02  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_N02  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_N02  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '23 ~ 01 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_N03  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_N03  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_N03  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_N03  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_N03  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_N03  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_N03  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_N03  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_N03  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_N03  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_N03  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '02 ~ 04 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_N04  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_N04  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_N04  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_N04  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_N04  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_N04  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_N04  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_N04  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_N04  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_N04  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_N04  TO IT_5293-VPO.
      PERFORM APPEND_5293  .

      MOVE :    '04 ~ 06 '             TO IT_5293-TIME,
                ZTPP_ALCLOG2-QTY01_N05  TO IT_5293-BI,
                ZTPP_ALCLOG2-QTY02_N05  TO IT_5293-PI,
                ZTPP_ALCLOG2-QTY03_N05  TO IT_5293-TC,
                ZTPP_ALCLOG2-QTY04_N05  TO IT_5293-PO,
                ZTPP_ALCLOG2-QTY05_N05  TO IT_5293-PBSI,
                ZTPP_ALCLOG2-QTY07_N05  TO IT_5293-TRIM,
                ZTPP_ALCLOG2-QTY08_N05  TO IT_5293-CF,
                ZTPP_ALCLOG2-PLANQTY_01 TO IT_5293-PLAN,
                ZTPP_ALCLOG2-QTY09_N05  TO IT_5293-SOFF,
*           ztpp_alclog2-QTY01_N05  to it_5290-ration,
                ZTPP_ALCLOG2-QTY10_N05  TO IT_5293-CG,
                ZTPP_ALCLOG2-QTY11_N05  TO IT_5293-VPI,
                ZTPP_ALCLOG2-QTY12_N05  TO IT_5293-VPO.
      PERFORM APPEND_5293  .
    ENDIF.

  ENDLOOP.

  LOOP AT IT_5293.
    IF IT_5293-PLAN > 0.
      IT_5293-RATION = IT_5293-SOFF / IT_5293-PLAN * 100 .
      MODIFY IT_5293.
    ENDIF.

    MOVE IT_5293 TO IT_SUM.
    CLEAR IT_SUM-TIME.
    COLLECT IT_SUM.
  ENDLOOP.

  READ TABLE IT_SUM INDEX 1.
  MOVE IT_SUM TO IT_5293.
  IT_5293-TIME = 'Sum'.

  APPEND IT_5293.


* header
  SELECT SINGLE * FROM CRTX
                  WHERE OBJTY = 'A'
                    AND SPRAS = SY-LANGU
                    AND KTEXT_UP = 'ASSEMBLY LINE 1' .

  SELECT SINGLE * FROM LDLT
                  WHERE LNID  = CRTX-OBJID
                    AND LNSID = CRTX-OBJID
                    AND LD_PERST <= ST_5293_INPUT-DATE
                    AND LD_PERED >= ST_5293_INPUT-DATE .

  MOVE : LDLT-LRATE TO ST_5293_INPUT-UPH.

  IF ST_5293_INPUT-DATE <= SY-DATUM.
    WA_TIME = '240000'.
  ELSE.
    WA_TIME = SY-UZEIT .
  ENDIF.

  CALL FUNCTION 'Z_FPP_UPH_TIME'
    EXPORTING
      INPUT_DATE = ST_5293_INPUT-DATE
      INPUT_TIME = WA_TIME
    IMPORTING
      RATE       = WA_RATE.

  IF WA_RATE <> 0.
    ST_5293_INPUT-BI   =    ( IT_SUM-BI    / WA_RATE ) * 100 .
    ST_5293_INPUT-PI   =    ( IT_SUM-PI    / WA_RATE ) * 100 .
    ST_5293_INPUT-PO   =    ( IT_SUM-PO    / WA_RATE ) * 100 .
    ST_5293_INPUT-TRIM =    ( IT_SUM-TRIM  / WA_RATE ) * 100 .
    ST_5293_INPUT-SOFF =    ( IT_SUM-SOFF  / WA_RATE ) * 100 .
  ENDIF.
ENDFORM.                    " data_select_5293

*&---------------------------------------------------------------------*
*&      Form  data_select_6299
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_6299.
  DATA: L_SER     TYPE I,
        L_NUM(3)  TYPE N.

  CLEAR : IT_6299, IT_6299[], L_SER.
  PERFORM READ_VARIANT_TABLE USING WA_MODEL .
  LOOP AT IT_ALC.
    L_SER = L_SER + 1  .
    IT_6299-SERIAL = L_SER .
    IT_6299-ZCOMMENT = IT_ALC-KNKTX .
    SELECT SINGLE CHNAM_CONT INTO IT_6299-AENAM
      FROM CUVTAB_ADM
     WHERE VTINT = IT_ALC-KNNUM.
    IT_6299-ZPART   = IT_ALC-MODEL .
    IT_6299-ZCOLUMN = IT_ALC-KNNAM .
    APPEND IT_6299 .    CLEAR: IT_6299.
  ENDLOOP.
  SORT IT_6299 BY SERIAL.
ENDFORM.                    " data_select_6299

*&---------------------------------------------------------------------*
*&      Form  SORT_SCREEN_4103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_4103    USING  PA_STYPE.
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_6299-COLS  INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+8        .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_6299      ASCENDING  BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_6299      DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " SORT_SCREEN_4103

*&---------------------------------------------------------------------*
*&      Form  GET_SUM_WOHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDER  text
*----------------------------------------------------------------------*
FORM GET_SUM_WOHD .
  CLEAR WA_TOT.
  LOOP AT WA_WOSUM  WHERE WO_SER = WA_ORDER(9)
                      AND NATION = WA_ORDER+9(3)
                      AND DEALER = WA_ORDER+12(2) .
    ADD : WA_WOSUM-T01DQ       TO  WA_TOT-T01DQ,
          WA_WOSUM-T08DQ       TO  WA_TOT-T08DQ,
          WA_WOSUM-T12DQ       TO  WA_TOT-T12DQ,
          WA_WOSUM-T17DQ       TO  WA_TOT-T17DQ,
          WA_WOSUM-T20DQ       TO  WA_TOT-T20DQ,

          WA_WOSUM-RP01TQ      TO  WA_TOT-RP01TQ,
          WA_WOSUM-RP02TQ      TO  WA_TOT-RP02TQ,
          WA_WOSUM-RP03TQ      TO  WA_TOT-RP03TQ,
          WA_WOSUM-RP04TQ      TO  WA_TOT-RP04TQ,
          WA_WOSUM-RP05TQ      TO  WA_TOT-RP05TQ,
          WA_WOSUM-RP06TQ      TO  WA_TOT-RP06TQ,
          WA_WOSUM-RP07TQ      TO  WA_TOT-RP07TQ,
          WA_WOSUM-RP08TQ      TO  WA_TOT-RP08TQ,
          WA_WOSUM-RP09TQ      TO  WA_TOT-RP09TQ,
          WA_WOSUM-RP10TQ      TO  WA_TOT-RP10TQ,
          WA_WOSUM-RP11TQ      TO  WA_TOT-RP11TQ,
          WA_WOSUM-RP12TQ      TO  WA_TOT-RP12TQ,
          WA_WOSUM-RP13TQ      TO  WA_TOT-RP13TQ,
          WA_WOSUM-RP14TQ      TO  WA_TOT-RP14TQ,
          WA_WOSUM-RP15TQ      TO  WA_TOT-RP15TQ,
          WA_WOSUM-RP16TQ      TO  WA_TOT-RP16TQ,

          WA_WOSUM-PLANQTY     TO  WA_TOT-PLANQTY,
          WA_WOSUM-FORECASTQTY TO  WA_TOT-FORECASTQTY.

  ENDLOOP.

  WA_TOTAL = WA_TOT-T08DQ + WA_TOT-T12DQ + WA_TOT-T17DQ + WA_TOT-T20DQ
           + WA_TOT-T01DQ + WA_TOT-T06DQ .
ENDFORM.                    " GET_SUM_WOHD
*&---------------------------------------------------------------------*
*&      Form  EXTINT_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXTINT_COLOR.
  DATA: L_NO(2)              TYPE   N,
        L_FNAME(40).

  DO 20 TIMES.
    L_NO = SY-INDEX.
    CONCATENATE 'WA_1004_E'    L_NO  INTO  L_FNAME.
    ASSIGN (L_FNAME)   TO   <FIELD1>.
    CLEAR <FIELD1>.
    CONCATENATE 'WA_1004_I'    L_NO  INTO  L_FNAME.
    ASSIGN (L_FNAME)   TO   <FIELD1>.
    CLEAR <FIELD1>.
  ENDDO.
  CLEAR: L_NO.
  LOOP AT WA_WOSUM WHERE WO_SER = WA_ORDER(9)
                     AND NATION = WA_ORDER+9(3)
                     AND DEALER = WA_ORDER+12(2).
    L_NO = L_NO + 1 .
    CONCATENATE 'WA_1004_E'    L_NO  INTO  L_FNAME.
    ASSIGN (L_FNAME)   TO   <FIELD1>.
    MOVE WA_WOSUM-EXTC        TO   <FIELD1>.
    CONCATENATE 'WA_1004_I'    L_NO  INTO  L_FNAME.
    ASSIGN (L_FNAME)   TO   <FIELD1>.
    MOVE WA_WOSUM-INTC        TO   <FIELD1>.
  ENDLOOP.
ENDFORM.                    " EXTINT_COLOR

*&---------------------------------------------------------------------*
*&      Form  CHECK_SAVE_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SAVE_1001.
  DATA: L_ANS             TYPE C.

  " Check the SAVE Flag.
  CHECK WA_CHG_1001_FLG = 'X'.
  " Call the Confirm Function. (YES[D] / NO / CANCEL)

  IF L_ANS = 'J' .
    PERFORM SAVE_1001       .
  ENDIF.
ENDFORM.                    " CHECK_SAVE_1001

*&---------------------------------------------------------------------*
*&      Form  clear_common_val
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_COMMON_VAL.
  " Clear Common Variables and Special Flag..
  " (Ex. Save_Flag, Initial_Flag...)
  CLEAR:   WA_219,       WA_FLAG,     WA_SAVE_FLG,  WA_WO_CREATE_DATE,
           WA_CAR,       WA_MI,       WA_OCN,       WA_MODEL ,
           WA_SCN_FLAG,  WA_ORDER,    WA_LINES,     WA_ERR_FLAG,
           WA_CHANGE,    WA_ORDER,    WA_WOSUM,     WA_WOM_DATE,
           WA_TOTAL,     WA_TOT,      WA_VERSION,   WA_ICOLOR,
           WA_ECOLOR,    WA_LC_NO,    WA_INDEX,     WA_DESTINATION_CODE,
           WA_PERF_YN,   WA_VAL11,    WA_VAL12,     WA_UPDATE_ALC_DATE1,
           WA_INIT_QTY,  WA_PROD,     WA_MITU_QTY,  WA_WO_MODI_DATE,
           WA_MOD_QTY,   WA_WO_PACK,  WA_PLAN_QTY,  WA_FORECAST_QTY,
           WA_WOC_DATE,  WA_TOTQTY,   WA_EDIT,      WA_ALV_CALLED,
           WA_ANSWER,    WA_INSERT,
           WA_PROD_DATE, WA_VIN_SPEC, WA_SEQ_QTY,   WA_TRIM_PLANT_NO.
  CLEAR:   P_COLUMN01,   P_COLUMN02,  P_COLUMN03,   P_COLUMN04,
           P_COLUMN05,   P_COLUMN06,  P_COLUMN07,   P_COLUMN08,
           P_COLUMN09,   P_COLUMN10,  P_VALUE01,    P_VALUE02,
           P_VALUE03,    P_VALUE04,   P_VALUE05,    P_VALUE06,
           P_VALUE07,    P_VALUE08,   P_VALUE09,    P_VALUE10.

  CLEAR:   IT_219, IT_ALCU_A, IT_ALCU_B, IT_ALCU_C, IT_ALCU_D.
  REFRESH: IT_219, IT_ALCU_A, IT_ALCU_B, IT_ALCU_C, IT_ALCU_D,
           WA_WOSUM.
  CLEAR: WA_Y_APP272, WA_T_APP272, WA_P_APP272, WA_M_APP272,
         WA_D_APP272, WA_S_APP272, WA_H_APP272, WA_X_APP272.
ENDFORM.                    " clear_common_val

*&---------------------------------------------------------------------*
*&      Form  get_knobj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNOBJ USING    PA_KNNAM  PA_KNOBJ.
  SELECT SINGLE KNOBJ INTO PA_KNOBJ
    FROM CUCO
   WHERE OBTAB = 'MARA'
     AND OBJEK = PA_KNNAM .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNNUM           USING    PA_KNOBJ.
  SELECT KNNUM APPENDING CORRESPONDING FIELDS OF TABLE IT_ALC
    FROM CUOB
   WHERE KNOBJ = PA_KNOBJ.
ENDFORM.                    " get_KNNUM

*&---------------------------------------------------------------------*
*&      Form  get_CUVTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_CUVTAB          USING    PA_VTNAM.
  SELECT VTINT VTNAM INTO (IT_ALC-KNNUM, IT_ALC-KNNAM)
    FROM CUVTAB
   WHERE VTNAM LIKE PA_VTNAM .
    APPEND IT_ALC.
  ENDSELECT.
ENDFORM.                    " get_CUVTAB

*&---------------------------------------------------------------------*
*&      Form  CALL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COLOR  text
*----------------------------------------------------------------------*
FORM CALL_COLOR TABLES  PA_VALS STRUCTURE  IT_RESULT1001
                USING   PA_ORDER .
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT       = PA_ORDER
      CTYPE        = '001'
    TABLES
      VAL_TABLE    = IT_RESULT1001
    EXCEPTIONS
      NO_DATA      = 1
      ERROR_MODE   = 2
      ERROR_OBJECT = 3
      ERROR_VALUE  = 4
      OTHERS       = 5.
ENDFORM.                    " CALL_COLOR
*&---------------------------------------------------------------------*
*&      Form  set_field
*&---------------------------------------------------------------------*
*       Searching Data To Set a dropdown list box - Bef.Progress
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_BRP.
  SELECT DISTINCT STATUS PROGRESS
    INTO (XVALUE-KEY , XVALUE-TEXT)
    FROM ZTPP_PROCESS
    WHERE VMRP <> ''.
    APPEND XVALUE TO XLIST.

  ENDSELECT.

ENDFORM.                    " set_field
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       Calling a Function For making a dropdown list box
*----------------------------------------------------------------------*
*      -->P_XLIST  text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_VRM USING    P_LIST.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = P_LIST.
ENDFORM.                    " CALL_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD2
*&---------------------------------------------------------------------*
*       Getting Data For a parameter - BODYNO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_BODYNO.
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_BODY_SERIAL' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " SET_FIELD_BODYNO

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       The Process of Data Selection
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA_2200.
  PERFORM SET_PARAMETERS_2200.
  PERFORM MAKE_DATA_2200.
ENDFORM.                    " select_data

*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETERS
*&---------------------------------------------------------------------*
*       Setting Parameters For Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETERS_2200.
  IF NOT WA_CDATE_ST IS INITIAL .
    CLEAR   R_CDATE.
    REFRESH R_CDATE.
    R_CDATE-LOW = WA_CDATE_ST.
    IF WA_CDATE_EN IS INITIAL.
      R_CDATE-OPTION = 'EQ'.
      R_CDATE-SIGN   = 'I'.
    ELSE.
      R_CDATE-HIGH   = WA_CDATE_EN.
      R_CDATE-OPTION = 'BT'.
      R_CDATE-SIGN   = 'I'.
    ENDIF.
    APPEND R_CDATE.
  ELSE.
    CLEAR   R_CDATE.
    REFRESH R_CDATE.
  ENDIF.
ENDFORM.                    " SET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA_2200.
* Model(CarType) = bodyno+0(03).
  DATA: L_MODEL  TYPE ZTPPVR-P_MODEL      ,
        L_SERIAL TYPE ZTPPVR-P_BODY_SERIAL,
        L_CARTYPE(04).

  CONCATENATE WA_MODEL '%' INTO L_CARTYPE.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_APP250
    FROM ZTPP_CHANGE
   WHERE CDATE  IN   R_CDATE   AND
         BODYNO LIKE L_CARTYPE AND
         CFLAG  =    'B'       .

* Order Number = ordno + nation + dealer.
  LOOP AT IT_APP250.
    CONCATENATE IT_APP250-ORDNO
                IT_APP250-NATION
                IT_APP250-DEALER
                INTO IT_APP250-ORDERNO.
*    MOVE it_app250-bodyno+00(03) TO l_model.
*    MOVE it_app250-bodyno+03(06) TO l_serial.
**
*    SELECT SINGLE p_model
*      INTO l_model
*      FROM ztppvr
*      WHERE flag          = 'LP'     AND
*            p_model       = l_model  AND
*            p_body_serial = l_serial   .
*    IF sy-subrc <> 0.
*      DELETE it_app250.
*      CONTINUE.
*    ENDIF.
*
    MODIFY IT_APP250.
  ENDLOOP.
  DATA: L_COUNT TYPE SY-TABIX.
  DESCRIBE TABLE IT_APP250 LINES L_COUNT .
  IF L_COUNT <= 0.
    MESSAGE S000 WITH 'No Data'.
  ENDIF.

* Cdate, ShopDate, BodyNo, OrderNo.
  SORT IT_APP250 BY CDATE B_RP_SHOPDAT BODYNO ORDERNO.
ENDFORM.                    " make_data
*&---------------------------------------------------------------------*
*&      Form  process_download
*&---------------------------------------------------------------------*
*       The Process of download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DOWNLOAD_2200.
  PERFORM SET_EXCEL_DATA_2200.
  PERFORM CALL_FUNCTION_FOR_EXCEL_2200.
ENDFORM.                    " process_download
*&---------------------------------------------------------------------*
*&      Form  SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*       Setting Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_EXCEL_DATA_2200.
*
  CLEAR IT_EXCEL_2200.
  REFRESH IT_EXCEL_2200.
  MOVE 'DATE' TO IT_EXCEL_2200-COL01.
  MOVE 'SERIAL' TO IT_EXCEL_2200-COL02.
  MOVE 'Body No' TO IT_EXCEL_2200-COL03.
  MOVE 'Order No' TO IT_EXCEL_2200-COL04.
  MOVE 'Spec' TO IT_EXCEL_2200-COL05.
  MOVE 'OUT COLOR' TO IT_EXCEL_2200-COL06.
  MOVE 'IN COLOR' TO IT_EXCEL_2200-COL07.
  MOVE 'BEFORE RP' TO IT_EXCEL_2200-COL08.
  MOVE 'Current RP' TO IT_EXCEL_2200-COL09.
  MOVE 'Act. Date' TO IT_EXCEL_2200-COL10.
  APPEND IT_EXCEL_2200.
*
  LOOP AT IT_APP250.
    CLEAR IT_EXCEL_2200.
    APPEND IT_EXCEL_2200.
  ENDLOOP.
*
ENDFORM.                    " SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_FOR_EXCEL
*&---------------------------------------------------------------------*
*       Calling a Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_FOR_EXCEL_2200.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'LINE_BACK.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = IT_EXCEL_2200
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.
ENDFORM.                    " CALL_FUNCTION_FOR_EXCEL

*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_ASCENDING_2200.
  DATA: FIELD_NAME01(40),
        OFFSET01 TYPE I.
*
  GET CURSOR FIELD FIELD_NAME01.
*
  IF FIELD_NAME01(09) = 'IT_APP250'.
    SEARCH FIELD_NAME01 FOR '-'.
    OFFSET01 = SY-FDPOS + 1.
    FIELD_NAME01 = FIELD_NAME01+OFFSET01.
    SORT IT_APP250 ASCENDING BY (FIELD_NAME01).
  ENDIF.
*
ENDFORM.                    " SORT_ASCENDING
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_2200  USING PA_STYPE.
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP250-COLS  INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP250    ASCENDING  BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP250    DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " SORT_SCREEN_2200

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       The Process of Data Selection
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA_2201.
  PERFORM SET_PARAMETERS_2200.   " Common Routine with 2200 Screen..
  PERFORM MAKE_DATA_2201.
ENDFORM.                    " select_data

*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       Making Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA_2201.
* Model(MODEL) = bodyno+0(03).
  DATA: L_SERIAL TYPE ZTPPVR-P_BODY_SERIAL,
        L_TABIX  TYPE SY-TABIX            ,
        L_MODEL(04).

  CONCATENATE WA_MODEL '%' INTO L_MODEL.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_APP252
    FROM ZTPP_CHANGE
    WHERE CDATE  IN   R_CDATE  AND
          BODYNO LIKE L_MODEL  AND
          CFLAG  =    'S'      .

  DESCRIBE TABLE IT_APP252  LINES WA_LINES.
  IF WA_LINES = 0.
    MESSAGE W001 WITH TEXT-100.
    EXIT.
  ENDIF.

* Order Number = ordno + nation + dealer.
  LOOP AT IT_APP252.
    L_TABIX = SY-TABIX .
    CONCATENATE IT_APP252-ORDNO     IT_APP252-NATION
                IT_APP252-DEALER    INTO IT_APP252-ORDERNO.

*    MOVE it_app252-bodyno+00(03) TO l_model.
*    MOVE it_app252-bodyno+03(06) TO l_serial.
**
*    SELECT SINGLE p_model  INTO l_model
*      FROM ztppvr
*      WHERE flag          = 'LS'     AND
*            p_model       = l_model  AND
*            p_body_serial = l_serial   .
*
*    IF sy-subrc <> 0.
*      DELETE it_app252.
*    ENDIF.
*
    MODIFY IT_APP252 INDEX L_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_APP252 LINES WA_LINES.
  IF WA_LINES < 1.
    MESSAGE S000 WITH  TEXT-100.
  ENDIF.

* Body_No, C.Date, Serial.
  SORT IT_APP252 BY BODYNO CDATE SERIAL.
ENDFORM.                    " make_data

*&---------------------------------------------------------------------*
*&      Form  process_download
*&---------------------------------------------------------------------*
*       The Process of Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DOWNLOAD_2201.
  PERFORM SET_EXCEL_DATA_2201.
  PERFORM CALL_FUNCTION_FOR_EXCEL_2201.
ENDFORM.                    " process_download
*&---------------------------------------------------------------------*
*&      Form  SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*       Setting Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_EXCEL_DATA_2201.
*
  CLEAR IT_EXCEL_2201.
  REFRESH IT_EXCEL_2201.
  MOVE 'Serial' TO IT_EXCEL_2201-COL01.
  MOVE 'BODY_NO' TO IT_EXCEL_2201-COL02.
  MOVE 'Current RP' TO IT_EXCEL_2201-COL03.
  MOVE 'VIN' TO IT_EXCEL_2201-COL04.
  MOVE 'ORDER_NO' TO IT_EXCEL_2201-COL05.
  MOVE 'OUT COLOR' TO IT_EXCEL_2201-COL06.
  MOVE 'IN COLOR' TO IT_EXCEL_2201-COL07.
  MOVE 'SPEC' TO IT_EXCEL_2201-COL08.
  MOVE 'OCN' TO IT_EXCEL_2201-COL09.
  MOVE 'Date(CDATE)' TO IT_EXCEL_2201-COL10.
  MOVE 'Bef.OrdNo' TO IT_EXCEL_2201-COL11.
  MOVE 'Bef.Nation' TO IT_EXCEL_2201-COL12.
  MOVE 'Bef.Dealer' TO IT_EXCEL_2201-COL13.
  MOVE 'Bef.Ext.C' TO IT_EXCEL_2201-COL14.
  MOVE 'Bef.Int.C' TO IT_EXCEL_2201-COL15.
  MOVE 'Bef.Spec' TO IT_EXCEL_2201-COL16.
  MOVE 'Bef.OCN' TO IT_EXCEL_2201-COL17.
  MOVE 'Bef.VIN' TO IT_EXCEL_2201-COL18.
  APPEND IT_EXCEL_2201.
*
  LOOP AT IT_APP252.
    CLEAR IT_EXCEL_2201.
    MOVE IT_APP252-SERIAL TO IT_EXCEL_2201-COL01.
    MOVE IT_APP252-BODYNO TO IT_EXCEL_2201-COL02.
    MOVE IT_APP252-CRP TO IT_EXCEL_2201-COL03.
    MOVE IT_APP252-VIN TO IT_EXCEL_2201-COL04.
    MOVE IT_APP252-ORDERNO TO IT_EXCEL_2201-COL05.
    MOVE IT_APP252-EXTC TO IT_EXCEL_2201-COL06.
    MOVE IT_APP252-INTC TO IT_EXCEL_2201-COL07.
    MOVE IT_APP252-MI TO IT_EXCEL_2201-COL08.
    MOVE IT_APP252-OCN TO IT_EXCEL_2201-COL09.
    MOVE IT_APP252-CDATE TO IT_EXCEL_2201-COL10.
    MOVE IT_APP252-B_ORDNO TO IT_EXCEL_2201-COL11.
    MOVE IT_APP252-B_NATION TO IT_EXCEL_2201-COL12.
    MOVE IT_APP252-B_DEALER TO IT_EXCEL_2201-COL13.
    MOVE IT_APP252-B_EXTC TO IT_EXCEL_2201-COL14.
    MOVE IT_APP252-B_INTC TO IT_EXCEL_2201-COL15.
    MOVE IT_APP252-B_MI TO IT_EXCEL_2201-COL16.
    MOVE IT_APP252-B_OCN TO IT_EXCEL_2201-COL17.
    MOVE IT_APP252-B_VIN TO IT_EXCEL_2201-COL18.
    APPEND IT_EXCEL_2201.
  ENDLOOP.
*
ENDFORM.                    " SET_EXCEL_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_FOR_EXCEL
*&---------------------------------------------------------------------*
*       Calling a Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_FOR_EXCEL_2201.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'LINE_BACK.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = IT_EXCEL_2201
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.
ENDFORM.                    " CALL_FUNCTION_FOR_EXCEL

*&---------------------------------------------------------------------*
*&      Form  set_field3
*&---------------------------------------------------------------------*
*       Setting Data To Make A Dropdown List Box - MODEL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_MODEL USING P_NAME  P_PARAMETER .
  DATA: L_ATINN              LIKE CABN-ATINN,
        L_ATNAM              LIKE CABN-ATNAM,
        L_ATWRT              LIKE AUSP-ATWRT,
        L_ATWTB              LIKE CAWNT-ATWTB.

  CLEAR : XLIST[],XVALUE.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    XVALUE-TEXT = L_ATWRT.  " ZTPP_VEH_MODEL-NAME.  l_atwtb
    XVALUE-KEY  = L_ATWRT.  " ZTPP_VEH_MODEL-MODEL.
    APPEND XVALUE TO XLIST .
  ENDSELECT.

* LIST BOX SETTING
  PERFORM LIST_BOX_FUNCTION USING P_NAME.
  IF P_PARAMETER IS INITIAL.
    READ TABLE XLIST INTO XVALUE  INDEX 1.
    P_PARAMETER = XVALUE-KEY.
  ENDIF.
ENDFORM.                    " set_field3

*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_ASCENDING_2201.
  DATA: FIELD_NAME01(40),
        OFFSET01 TYPE I.
*
  GET CURSOR FIELD FIELD_NAME01.
*
  IF FIELD_NAME01(09) = 'IT_APP252'.
    SEARCH FIELD_NAME01 FOR '-'.
    OFFSET01 = SY-FDPOS + 1.
    FIELD_NAME01 = FIELD_NAME01+OFFSET01.
    SORT IT_APP252 ASCENDING BY (FIELD_NAME01).
  ENDIF.
*
ENDFORM.                    " SORT_ASCENDING
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_2201   USING  PA_STYPE .
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40).
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP252-COLS  INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP252    ASCENDING  BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP252    DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " SORT_SCREEN_2201

*&---------------------------------------------------------------------*
*&      Form  set_field02
*&---------------------------------------------------------------------*
*       Setting a Parameter - PART
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_PART.
  CLEAR: XVALUE-KEY, XVALUE-TEXT.
  MOVE 'U' TO XVALUE-KEY.
  MOVE 'Unique Part' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.
*
  CLEAR: XVALUE-KEY, XVALUE-TEXT.
  MOVE 'C' TO XVALUE-KEY.
  MOVE 'Color Part' TO XVALUE-TEXT.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " set_field02

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD03
*&---------------------------------------------------------------------*
*       Setting a Parameter - Part's Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_KEY.
  DATA L_COUNT(03).
  DO 200 TIMES.
    L_COUNT = L_COUNT + 1.
    CLEAR: XVALUE-KEY, XVALUE-TEXT.
    MOVE L_COUNT TO XVALUE-KEY.
    MOVE L_COUNT TO XVALUE-TEXT.
    APPEND XVALUE TO XLIST.
  ENDDO.
*

ENDFORM.                    " SET_FIELD03
*&---------------------------------------------------------------------*
*&      Form  check_essential_condition
*&---------------------------------------------------------------------*
*       Checking Essential Condition & Code Structure
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ESSENTIAL_CONDITION_1205.
  IF WA_MODEL IS INITIAL.
    MESSAGE S000 WITH 'Model needs to be filled'.
    EXIT.
  ENDIF.
  IF P_PART IS INITIAL.
    MESSAGE S000 WITH 'Part needs to be filled'.
    EXIT.
  ENDIF.
  IF P_KEY IS INITIAL.
    MESSAGE S000 WITH 'Key needs to be filled'.
    EXIT.
  ENDIF.
  IF P_PART = 'C' AND P_KEY > 50.
    MESSAGE S000 WITH 'The maxium key value is 50'.
    EXIT.
  ENDIF.
  IF P_PART = 'U' AND P_KEY > 200.
    MESSAGE S000 WITH 'The maxium key value is 200'.
    EXIT.
  ENDIF.

* Full Code(ALC) = p_model+'_ALC_'+p_part+'_'+p_key.
  CONDENSE P_KEY.
  CONCATENATE WA_MODEL '_ALC_' P_PART '_' P_KEY
    INTO P_FULL_CODE.

  CLEAR: WA_DESCRIPTIONS, IT_COLUMN.
  REFRESH: WA_DESCRIPTIONS, IT_COLUMN.

  CALL FUNCTION 'CARD_TABLE_READ_STRUCTURE'
    EXPORTING
      VAR_TAB         = P_FULL_CODE
      LANGUAGE        = 'E'
    TABLES
      DESCRIPTIONS    = WA_DESCRIPTIONS
      CHARACTERISTICS = IT_COLUMN
    EXCEPTIONS
      ERROR           = 1
      OTHERS          = 2.

  MOVE WA_DESCRIPTIONS-DESCRIPT TO P_COL_NAME.

  DATA: L_OFFSET TYPE I.
  DATA: L_COL_NAME(20).

  CLEAR: P_KEY_01, P_KEY_02, P_KEY_03, P_KEY_04, P_KEY_05,
         P_KEY_06, P_KEY_07, P_KEY_08, P_KEY_09, P_KEY_10,
         P_KEY_11, P_KEY_12, P_KEY_13, P_KEY_14, P_KEY_15,
         P_KEY_16, P_KEY_17, P_KEY_18, P_KEY_19, P_KEY_20.

  LOOP AT IT_COLUMN.
    SEARCH IT_COLUMN-CHARACT FOR 'ALC'.
    IF SY-SUBRC = 0.
      CONTINUE.
    ELSE.
      CLEAR: L_OFFSET, L_COL_NAME.
      SEARCH IT_COLUMN-CHARACT FOR '219'.
      IF SY-SUBRC = 0.
        L_OFFSET = SY-FDPOS + 4.
        L_COL_NAME = IT_COLUMN-CHARACT+L_OFFSET.
      ELSE.
        L_COL_NAME = IT_COLUMN-CHARACT+2.
      ENDIF.
      CASE SY-TABIX.
        WHEN 3.
          P_KEY_01 = L_COL_NAME.
        WHEN 4.
          P_KEY_02 = L_COL_NAME.
        WHEN 5.
          P_KEY_03 = L_COL_NAME.
        WHEN 6.
          P_KEY_04 = L_COL_NAME.
        WHEN 7.
          P_KEY_05 = L_COL_NAME.
        WHEN 8.
          P_KEY_06 = L_COL_NAME.
        WHEN 9.
          P_KEY_07 = L_COL_NAME.
        WHEN 10.
          P_KEY_08 = L_COL_NAME.
        WHEN 11.
          P_KEY_09 = L_COL_NAME.
        WHEN 12.
          P_KEY_10 = L_COL_NAME.
        WHEN 13.
          P_KEY_11 = L_COL_NAME.
        WHEN 14.
          P_KEY_12 = L_COL_NAME.
        WHEN 15.
          P_KEY_13 = L_COL_NAME.
        WHEN 16.
          P_KEY_14 = L_COL_NAME.
        WHEN 17.
          P_KEY_15 = L_COL_NAME.
        WHEN 18.
          P_KEY_16 = L_COL_NAME.
        WHEN 19.
          P_KEY_17 = L_COL_NAME.
        WHEN 20.
          P_KEY_18 = L_COL_NAME.
        WHEN 21.
          P_KEY_19 = L_COL_NAME.
        WHEN 22.
          P_KEY_20 = L_COL_NAME.
      ENDCASE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_essential_condition
*&---------------------------------------------------------------------*
*&      Form  GET_ACTURL_PERIOD_TIME      "UD1K912931
*&---------------------------------------------------------------------*
*  this routine was changed by chris on 05/11/2005.
*  the actural working time period are already calculated and stored
*  in table it_5290time by routine MAKE_GAP. The result includes the
*  the considering of break. lunch(meal) break is a separate period.
*  other peiod gap is one hour.
*----------------------------------------------------------------------*
*      -->P_L_IDX  text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_PERIOD_TIME USING  P_IDX P_TIME P_SHIFT.
  DATA: L_START LIKE SY-UZEIT,
        L_END   LIKE SY-UZEIT.
  DATA: L_SHIFT_END LIKE SY-UZEIT.


*  IF P_SHIFT = 1.
*    L_START = ST_5290_INPUT-BTIME.          "UD1K912931
*    L_END   = L_START.
*    L_SHIFT_END = ST_5290_INPUT-ETIME.
*    IF ST_5290_INPUT-BTIME GT ST_5290_INPUT-ETIME.
*       L_SHIFT_END(2) = L_SHIFT_END(2) + 24.
*    ENDIF.
*  ELSEIF P_SHIFT = 2.
*    L_START = ST_5290_INPUT-BTIME_2.
*    L_END = L_START.
*    L_SHIFT_END = ST_5290_INPUT-ETIME_2.
*    IF ST_5290_INPUT-BTIME_2 GT ST_5290_INPUT-ETIME_2.
*       L_SHIFT_END(2) = L_SHIFT_END(2) + 24.
*    ENDIF.
*
*  ENDIF.

*  L_START(2) = L_START(2) + P_IDX - 1.
*  L_END(2) = L_START(2) + 1.
*
*  IF L_END GT L_SHIFT_END.
*     L_END = L_SHIFT_END.
*  ENDIF.
*
*  IF L_START(2) GE 24.
*     L_START(2) = L_START(2) - 24.
*  ENDIF.
*
*  IF L_END(2) GE 24.
*     L_END(2) = L_END(2) - 24.
*  ENDIF.

* changed by chris on 05/11/2005
* considering the meal break period
* read the actural time for eahc period
  READ TABLE IT_5290TIME WITH KEY SEQ = P_IDX.
  IF SY-SUBRC EQ 0.
    L_START = IT_5290TIME-TIME1.
    L_END   = IT_5290TIME-TIME2.
    CONCATENATE L_START(2) ':' L_START+2(2)'~'
              L_END(2) ':' L_END+2(2)
         INTO P_TIME        .
  ELSE.
    P_TIME = '00:00~00:00'.
  ENDIF.
ENDFORM.                    " GET_ACTURL_PERIOD_TIME
*&---------------------------------------------------------------------*
*&      Form  data_make_5290
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_MAKE_5290.
  DATA: L_SHIFT LIKE ST_5290_INPUT-DAY,
         WA_SHIFT LIKE ST_5290_INPUT-DAY.
  DATA: L_START  LIKE SY-UZEIT,
        L_END1   LIKE SY-UZEIT,
        L_END2   LIKE SY-UZEIT.

  CLEAR: IT_5290, IT_5290[].
*  GET THE SHIFT
  L_SHIFT = ST_5290_INPUT-DAY.
*  CHECK SHIFT

** Furong on 06/12/12 for 3 shift
  IF L_SHIFT = 1 .
    WA_SHIFT = 1 .
    CLEAR: IT_5290_SHIFT, IT_5290_SHIFT[].
    PERFORM DATA_SELECT_5290 USING WA_SHIFT.
*  STORE THE RESULT OF FIRST SHIFT.
    PERFORM TOTAL_12_TOTAL USING WA_SHIFT.
  ELSEIF L_SHIFT = 2.
    WA_SHIFT = 2.
    CLEAR: IT_5290_SHIFT, IT_5290_SHIFT[].
    PERFORM DATA_SELECT_5290 USING WA_SHIFT.
*  STORE THE RESULT OF SECOND SHIFT and total
    PERFORM TOTAL_12_TOTAL USING WA_SHIFT.
  ELSEIF L_SHIFT = 3.
    WA_SHIFT = 3.
    CLEAR: IT_5290_SHIFT, IT_5290_SHIFT[].
    PERFORM DATA_SELECT_5290 USING WA_SHIFT.
*  STORE THE RESULT OF SECOND SHIFT and total
    PERFORM TOTAL_12_TOTAL USING WA_SHIFT.

  ELSEIF L_SHIFT = 4.
    WA_SHIFT = 1 .
    CLEAR: IT_5290_SHIFT, IT_5290_SHIFT[].
    PERFORM DATA_SELECT_5290 USING WA_SHIFT.
    MOVE IT_5290_SHIFT[] TO IT_5290_1SHIFT[].
    WA_SHIFT = 2.
    CLEAR: IT_5290_SHIFT, IT_5290_SHIFT[].
    PERFORM DATA_SELECT_5290 USING WA_SHIFT.
    MOVE IT_5290_SHIFT[] TO IT_5290_2SHIFT[].
    WA_SHIFT = 3.
    CLEAR: IT_5290_SHIFT, IT_5290_SHIFT[].
    PERFORM DATA_SELECT_5290 USING WA_SHIFT.
    MOVE IT_5290_SHIFT[] TO IT_5290_3SHIFT[].
*   merge all shifts
    PERFORM MERGE_SHIFTS_RESULT.
  ENDIF.

*  IF l_shift = 1 .
*    wa_shift = 1 .
*    CLEAR: it_5290_shift, it_5290_shift[].
*    PERFORM data_select_5290 USING wa_shift.
**  STORE THE RESULT OF FIRST SHIFT.
*    PERFORM total_12_total USING wa_shift.
*  ELSEIF l_shift = 2.
*    wa_shift = 2.
*    CLEAR: it_5290_shift, it_5290_shift[].
*    PERFORM data_select_5290 USING wa_shift.
**  STORE THE RESULT OF SECOND SHIFT and total
*    PERFORM total_12_total USING wa_shift.
*  ELSEIF l_shift = 3.
*    wa_shift = 1 .
*    CLEAR: it_5290_shift, it_5290_shift[].
*    PERFORM data_select_5290 USING wa_shift.
*    MOVE it_5290_shift[] TO it_5290[].
*    wa_shift = 2.
*    CLEAR: it_5290_shift, it_5290_shift[].
*    PERFORM data_select_5290 USING wa_shift.
**    MERGE SHIFT 1 AND SHIFT 2 RESULT
*    PERFORM merge_shifts_result.
*
*  ENDIF.
** End on 06/12/12

  IF SY-UNAME = '100701'.
    GET TIME.
    L_END1 = SY-UZEIT.
  ENDIF.

*    add the wip data.
  PERFORM ADD_WIP.

ENDFORM.                    " data_make_5290
*&---------------------------------------------------------------------*
*&      Form  MERGE_SHIFTS_RESULT
*&---------------------------------------------------------------------*
*       MERGE THE RESULT OF FIRST AND SECOND SHIFT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MERGE_SHIFTS_RESULT.
  DATA: WA_5290 LIKE IT_5290.
  DATA: L_UPH LIKE ST_5290_INPUT-UPH_L.
*  DATA: l_lines TYPE i.
  DATA: L_LINES_1 TYPE I,
        L_LINES_2 TYPE I,
        L_LINES_3 TYPE I.

** Furong on 06/12/12 for 3 shift
  DESCRIBE TABLE IT_5290_3SHIFT LINES L_LINES_3.
  DESCRIBE TABLE IT_5290_2SHIFT LINES L_LINES_2.
  DESCRIBE TABLE IT_5290_1SHIFT LINES L_LINES_1.
*  only first shift
  IF L_LINES_2 = 1 AND L_LINES_3 = 1. "ONLY SUM RECORD
    IT_5290_SHIFT[] = IT_5290_1SHIFT[].
    PERFORM TOTAL_12_TOTAL USING '1'.
    EXIT.
  ENDIF.
  IF L_LINES_1 = 1 AND L_LINES_3 = 1. "ONLY SUM RECORD
    IT_5290_SHIFT[] = IT_5290_2SHIFT[].
    PERFORM TOTAL_12_TOTAL USING '2'.
    EXIT.
  ENDIF.
  IF L_LINES_1 = 1 AND L_LINES_2 = 1. "ONLY SUM RECORD
    IT_5290_SHIFT[] = IT_5290_1SHIFT[].
    PERFORM TOTAL_12_TOTAL USING '3'.
    EXIT.
  ENDIF.

  IF L_LINES_1 > 1.
    LOOP AT IT_5290_1SHIFT.
      APPEND IT_5290_1SHIFT TO IT_5290.
    ENDLOOP.
  ENDIF.
  IF L_LINES_2 > 1.
    LOOP AT IT_5290_2SHIFT.
      APPEND IT_5290_2SHIFT TO IT_5290.
    ENDLOOP.
  ENDIF.
  IF L_LINES_3 > 1.
    LOOP AT IT_5290_3SHIFT.
      APPEND IT_5290_3SHIFT TO IT_5290.
    ENDLOOP.
  ENDIF.

*  DESCRIBE TABLE it_5290_shift LINES l_lines.
**  IF NOT ANY DATA FOR SECOND SHIFT, MERGE IS NOT NEEDED.
*  IF l_lines = 1. "ONLY SUM RECORD
*    it_5290_shift[] = it_5290[].
*    PERFORM total_12_total USING '1'.
*    EXIT.
*  ENDIF.
** if only second shift.
*  DESCRIBE TABLE it_5290 LINES l_lines.
*  IF l_lines = 1. "ONLY SUM RECORD
*    PERFORM total_12_total USING '2'.
*    EXIT.
*  ENDIF.
*
*  LOOP AT it_5290_shift.
*    APPEND it_5290_shift TO it_5290.
*  ENDLOOP.

** End on 06/12/12

*  calculate the grand total and after shift total
  CLEAR: WA_5290.
  WA_5290 = WA_TOTAL_5290.

  LOOP AT IT_5290 .
** Furong on 06/12/12 for 3 shift
*    IF it_5290-time = '1st total' OR
*       it_5290-time = '2nd total'.
    IF IT_5290-TIME = '1st total' OR
         IT_5290-TIME = '2nd total' OR
         IT_5290-TIME = '3rd total'.
*      wa_5290-time = 'Other Time'.
** furong on 08/08/12 for 3 shift
*      WA_5290-TIME = 'Pre. Date'.
** Furong on 02/28/13
*      WA_5290-TIME = '3rd (~6:45)'.
      wa_5290-time = 'Others'.
** End on 02/28/13
      WA_5290-SHIPIN = WA_5290-SHIPIN - IT_5290-SHIPIN.
      WA_5290-SHIPOUT = WA_5290-SHIPOUT - IT_5290-SHIPOUT.
** End on 08/08/12
      WA_5290-BI   = WA_5290-BI - IT_5290-BI.
      WA_5290-PI   = WA_5290-PI - IT_5290-PI.
      WA_5290-TC   = WA_5290-TC - IT_5290-TC.
      WA_5290-PO   = WA_5290-PO - IT_5290-PO.
      WA_5290-PBSI   = WA_5290-PBSI - IT_5290-PBSI.
      WA_5290-PBSO   = WA_5290-PBSO - IT_5290-PBSO.
      WA_5290-TRIM   = WA_5290-TRIM - IT_5290-TRIM.
      WA_5290-CF   = WA_5290-CF - IT_5290-CF.
      WA_5290-SOFF   = WA_5290-SOFF - IT_5290-SOFF.
      WA_5290-CG   = WA_5290-CG - IT_5290-CG.
      WA_5290-VPCI   = WA_5290-VPCI - IT_5290-VPCI.
      WA_5290-VPCO   = WA_5290-VPCO - IT_5290-VPCO.
    ENDIF.
  ENDLOOP.

  IF ST_5290_INPUT-DAYU = 0.
    WA_5290-RATION = 0.
    WA_5290-PLAN   = 0.
  ELSE.
    WA_5290-RATION = WA_5290-SOFF /
                           ST_5290_INPUT-DAYU * 100 .
    WA_5290-PLAN   = WA_5290-TRIM /
                           ST_5290_INPUT-DAYU * 100 .
  ENDIF.
** Furong on 02/28/13
  APPEND WA_5290 TO IT_5290.
** End on 02/28/13
** furong on 08/08/12 for 3 shift
*  APPEND WA_5290 TO IT_5290.
** End on 09/08/12
* calculate the total ratio
  WA_TOTAL_5290-TIME = 'Total'.
  IF ST_5290_INPUT-DAYU = 0.
    WA_TOTAL_5290-RATION = 0.
    WA_TOTAL_5290-PLAN   = 0.
  ELSE.
    WA_TOTAL_5290-RATION = WA_TOTAL_5290-SOFF /
                           ST_5290_INPUT-DAYU * 100 .
    WA_TOTAL_5290-PLAN   = WA_TOTAL_5290-TRIM /
                           ST_5290_INPUT-DAYU * 100 .
  ENDIF.

  APPEND WA_TOTAL_5290 TO IT_5290.


ENDFORM.                    " MERGE_SHIFTS_RESULT
*&---------------------------------------------------------------------*
*&      Form  CALL_TCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TCODE.
  DATA: CLICKED_LINE TYPE I.
  DATA: CLICKED_FIELD(20).

*  CHECK THE COLUMN CLICKED
  GET CURSOR FIELD CLICKED_FIELD.
  GET CURSOR LINE CLICKED_LINE.
  CLICKED_LINE = TC_APP244-TOP_LINE + CLICKED_LINE.
  CLICKED_LINE = CLICKED_LINE - 1.
  READ TABLE IT_APP244 INDEX CLICKED_LINE.
  IF CLICKED_FIELD = 'IT_APP244-VENDOR'.
    IF NOT IT_APP244-BODYNO IS INITIAL.
      SET PARAMETER ID 'PAF' FIELD IT_APP244-VENDOR.
      CALL TRANSACTION 'MD13' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSEIF CLICKED_FIELD = 'IT_APP244-BODYNO'.
    IF NOT IT_APP244-BODYNO IS INITIAL.
      SET PARAMETER ID 'EQN' FIELD IT_APP244-BODYNO.
      CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN .
    ENDIF.
  ENDIF.


ENDFORM.                    " CALL_TCODE
*&---------------------------------------------------------------------*
*&      Form  total_12_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TOTAL_12_TOTAL USING P_SHIFT.
  DATA: WA_AFTER LIKE IT_5290.
  CLEAR: IT_5290[], IT_5290.
  IF P_SHIFT = 1.
    READ TABLE IT_5290_SHIFT WITH KEY TIME = '1st total'.
  ELSEIF P_SHIFT = 2.
    READ TABLE IT_5290_SHIFT WITH KEY TIME = '2nd total'.
** Furong on 06/12/12
  ELSEIF P_SHIFT = 3.
    READ TABLE IT_5290_SHIFT WITH KEY TIME = '3rd total'.
  ENDIF.
* wa_after-time = 'Other Time'.
*  wa_after-bi   = wa_total_5290-bi - it_5290_shift-bi.
*  wa_after-pi   = wa_total_5290-pi - it_5290_shift-pi.
*  wa_after-tc   = wa_total_5290-tc - it_5290_shift-tc.
*  wa_after-po   = wa_total_5290-po - it_5290_shift-po.
*  wa_after-pbsi   = wa_total_5290-pbsi - it_5290_shift-pbsi.
*  wa_after-pbso   = wa_total_5290-pbso - it_5290_shift-pbso.
*  wa_after-trim   = wa_total_5290-trim - it_5290_shift-trim.
*  wa_after-cf   = wa_total_5290-cf - it_5290_shift-cf.
*  wa_after-soff   = wa_total_5290-soff - it_5290_shift-soff.
*  wa_after-cg   = wa_total_5290-cg - it_5290_shift-cg.
*  wa_after-vpci   = wa_total_5290-vpci - it_5290_shift-vpci.
*  wa_after-vpco   = wa_total_5290-vpco - it_5290_shift-vpco.
*  wa_after-shipin   = wa_total_5290-shipin - it_5290_shift-shipin.
*  wa_after-shipout   = wa_total_5290-shipout - it_5290_shift-shipout.
*  APPEND wa_after TO it_5290_shift.
*  wa_total_5290-time = 'Total'.

  IF ST_5290_INPUT-DAYU = 0.
    WA_TOTAL_5290-RATION = 0.
    WA_TOTAL_5290-PLAN   = 0.
  ELSE.
    WA_TOTAL_5290-RATION = WA_TOTAL_5290-SOFF /
                           ST_5290_INPUT-DAYU * 100 .
    WA_TOTAL_5290-PLAN   = WA_TOTAL_5290-TRIM /
                           ST_5290_INPUT-DAYU * 100 .
  ENDIF.
*  APPEND wa_total_5290 TO it_5290_shift.
  MOVE IT_5290_SHIFT[] TO IT_5290[].

ENDFORM.                    " total_12_total
*&---------------------------------------------------------------------*
*&      Form  ADD_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADD_WIP.
  DATA: I_COUNT  TYPE I.
  DATA: L_ATINN  LIKE AUSP-ATINN.

  CLEAR: IT_5290.
  APPEND IT_5290.
  IT_5290-TIME = 'Current WIP'.

  PERFORM READ_ATINN USING 'P_RP_STATUS' L_ATINN .

  PERFORM GET_STATUS_COUNT USING '01'  I_COUNT L_ATINN.
  IT_5290-BI = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '02'  I_COUNT L_ATINN.
  IT_5290-PI = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '03'  I_COUNT L_ATINN.
  IT_5290-TC = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '04'  I_COUNT L_ATINN.
  IT_5290-PO = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '05'  I_COUNT L_ATINN.
  IT_5290-PBSI = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '06'  I_COUNT L_ATINN.
  IT_5290-PBSO = I_COUNT.
  PERFORM GET_ST_RG_COUNT USING '07' '16' I_COUNT L_ATINN.
  IT_5290-TRIM = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '17'  I_COUNT L_ATINN.
  IT_5290-CF = I_COUNT.
  PERFORM GET_STATUS_COUNT USING '18'  I_COUNT L_ATINN.
  IT_5290-SOFF = I_COUNT.
** CHANGED BY FURONG ON 12/02/05
*  PERFORM GET_STATUS_COUNT USING '19'  I_COUNT L_ATINN.
*  IT_5290-CG = I_COUNT.
*  PERFORM GET_STATUS_COUNT USING '20'  I_COUNT L_ATINN.
*  IT_5290-CG = IT_5290-CG + I_COUNT.
*  PERFORM GET_STATUS_COUNT USING '22'  I_COUNT L_ATINN.
*  IT_5290-VPCI = I_COUNT.
** END OF CHANGE
*  PERFORM GET_STATUS_COUNT USING '21'  I_COUNT L_ATINN.
*  IT_5290-VPCI = IT_5290-VPCI + I_COUNT.
*  PERFORM GET_STATUS_COUNT USING '20'  I_COUNT L_ATINN.
*  IT_5290-VPCI = IT_5290-VPCI + I_COUNT.
*
*  PERFORM GET_STATUS_COUNT USING '23'  I_COUNT L_ATINN.
*  IT_5290-VPCO = I_COUNT.
** CHANGED BY FURONG ON 12/02/05
*  PERFORM GET_STATUS_COUNT USING '24'  I_COUNT L_ATINN.
*  IT_5290-shipin = I_COUNT.
*  PERFORM GET_STATUS_COUNT USING '26'  I_COUNT L_ATINN.
*  IT_5290-shipin = IT_5290-shipin + I_COUNT.
** END OF CHANG
*  PERFORM GET_STATUS_COUNT USING '25'  I_COUNT L_ATINN.
*  IT_5290-shipout = I_COUNT.
*  PERFORM GET_STATUS_COUNT USING '27'  I_COUNT L_ATINN.
*  IT_5290-shipout = IT_5290-shipout + I_COUNT.

  APPEND IT_5290.


ENDFORM.                    " ADD_WIP
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_STATUS_COUNT USING P_STL P_COUNT P_ATINN.
  DATA: L_ATINN LIKE AUSP-ATINN.
  DATA: L_ATINN1 LIKE AUSP-ATINN.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK  LIKE AUSP-OBJEK,
          ATINN  LIKE AUSP-ATINN,
          ATWRT  LIKE AUSP-ATWRT,
        END OF LT_OBJEK.
  DATA: L_CNT_T TYPE I.
  DATA: L_CNT_X TYPE I.
  DATA: L_CNT_S TYPE I.
  DATA: LT_OBJ1 LIKE LT_OBJEK OCCURS 0 WITH HEADER LINE.

  CLEAR: P_COUNT.
  PERFORM READ_ATINN USING 'P_WORK_ORDER'   L_ATINN .
  PERFORM READ_ATINN USING 'P_USAGE_CAR'    L_ATINN1 .

*  read rp status cars
  SELECT OBJEK ATINN ATWRT INTO TABLE LT_OBJEK
    FROM AUSP
    WHERE KLART = '002'
      AND ATINN = P_ATINN
      AND ATWRT = P_STL.
  CHECK SY-SUBRC EQ 0.



* EXCLUDING THE 'S' 'D' CARS FOR P_USAGE_CAR
  SELECT OBJEK ATINN ATWRT INTO TABLE LT_OBJ1
    FROM AUSP
    FOR ALL ENTRIES IN LT_OBJEK
    WHERE OBJEK = LT_OBJEK-OBJEK
     AND  ATINN = L_ATINN1
     AND  KLART = '002'.


  LOOP AT LT_OBJEK.
    CLEAR: LT_OBJ1.
    READ TABLE LT_OBJ1 WITH KEY OBJEK = LT_OBJEK-OBJEK.
    IF SY-SUBRC EQ 0 AND
       ( LT_OBJ1-ATWRT = 'S' OR
         LT_OBJ1-ATWRT = 'D' ).
      DELETE LT_OBJEK.
    ENDIF.
  ENDLOOP.

  CHECK NOT LT_OBJEK IS INITIAL.

* excluding 'XX' 'XY' CARS
  CLEAR: LT_OBJ1, LT_OBJ1[].
  SELECT OBJEK ATINN ATWRT INTO TABLE LT_OBJ1
   FROM AUSP
   FOR ALL ENTRIES IN LT_OBJEK
   WHERE OBJEK = LT_OBJEK-OBJEK
    AND  ATINN = L_ATINN
    AND  KLART = '002'.

* check the work order value
  LOOP AT LT_OBJEK.
    CLEAR: LT_OBJ1.
    READ TABLE LT_OBJ1 WITH KEY OBJEK = LT_OBJEK-OBJEK.
    IF SY-SUBRC NE 0.
*      delete LT_OBJEK.  "
    ELSE.
      IF ST_5290_INPUT-TYP = 1.
        IF LT_OBJ1-ATWRT CS 'XX' OR
           LT_OBJ1-ATWRT CS 'XY'.
          DELETE LT_OBJEK.               "COMPLETE
        ENDIF.
      ELSEIF ST_5290_INPUT-TYP = 2.
        IF NOT ( LT_OBJ1-ATWRT CS 'XX' OR
           LT_OBJ1-ATWRT CS 'XY' ).
          DELETE LT_OBJEK.               "BIW & BIP
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_OBJEK LINES P_COUNT.

ENDFORM.                    " GET_STATUS_COUNT
*&---------------------------------------------------------------------*
*&      Form  GET_ST_RG_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7316   text
*      -->P_7317   text
*      -->P_I_COUNT  text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
FORM GET_ST_RG_COUNT USING    P_STL
                              P_STH
                              P_COUNT
                              P_ATINN.
  DATA: RP(02) TYPE N.
  DATA: LOOP TYPE I.
  RANGES: R_RP FOR RP.
  DATA: L_ATINN LIKE AUSP-ATINN.
  DATA: L_ATINN1 LIKE AUSP-ATINN.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
           OBJEK  LIKE AUSP-OBJEK,
           ATINN  LIKE AUSP-ATINN,
           ATWRT  LIKE AUSP-ATWRT,
        END OF LT_OBJEK.
  DATA: LT_OBJ1 LIKE LT_OBJEK OCCURS 0 WITH HEADER LINE.

  PERFORM READ_ATINN USING 'P_WORK_ORDER'   L_ATINN .
  PERFORM READ_ATINN USING 'P_USAGE_CAR'    L_ATINN1 .

  RP = P_STL.
  LOOP = P_STH - P_STL + 1.
  DO LOOP TIMES.
    R_RP-OPTION = 'EQ'.
    R_RP-SIGN    = 'I'.
    R_RP-LOW     = RP.
    APPEND R_RP.
    RP = RP + 1.
  ENDDO.

*  read rp status cars
  SELECT OBJEK ATINN ATWRT INTO TABLE LT_OBJEK
    FROM AUSP
    WHERE KLART = '002'
      AND ATINN = P_ATINN
      AND ATWRT IN R_RP.
  CHECK SY-SUBRC EQ 0.

* EXCLUDING THE 'S' 'D' CARS FOR P_USAGE_CAR
  SELECT OBJEK ATINN ATWRT INTO TABLE LT_OBJ1
    FROM AUSP
    FOR ALL ENTRIES IN LT_OBJEK
    WHERE OBJEK = LT_OBJEK-OBJEK
     AND  ATINN = L_ATINN1
     AND  KLART = '002'.

  LOOP AT LT_OBJEK.
    CLEAR: LT_OBJ1.
    READ TABLE LT_OBJ1 WITH KEY OBJEK = LT_OBJEK-OBJEK.
    IF SY-SUBRC EQ 0 AND
       ( LT_OBJ1-ATWRT = 'S' OR
         LT_OBJ1-ATWRT = 'D' ).
      DELETE LT_OBJEK.
    ENDIF.
  ENDLOOP.

  CHECK NOT LT_OBJEK IS INITIAL.

* excluding 'XX' 'XY' CARS
  CLEAR: LT_OBJ1, LT_OBJ1[].
  SELECT OBJEK ATINN ATWRT INTO TABLE LT_OBJ1
   FROM AUSP
   FOR ALL ENTRIES IN LT_OBJEK
   WHERE OBJEK = LT_OBJEK-OBJEK
    AND  ATINN = L_ATINN
    AND  KLART = '002'.

* check the work order value
  LOOP AT LT_OBJEK.
    CLEAR: LT_OBJ1.
    READ TABLE LT_OBJ1 WITH KEY OBJEK = LT_OBJEK-OBJEK.
    IF SY-SUBRC NE 0.
*      delete LT_OBJEK.  "
    ELSE.
      IF ST_5290_INPUT-TYP = 1.
        IF LT_OBJ1-ATWRT CS 'XX' OR
           LT_OBJ1-ATWRT CS 'XY'.
          DELETE LT_OBJEK.               "COMPLETE
        ENDIF.
      ELSEIF ST_5290_INPUT-TYP = 2.
        IF NOT ( LT_OBJ1-ATWRT CS 'XX' OR
           LT_OBJ1-ATWRT CS 'XY' ).
          DELETE LT_OBJEK.               "BIW & BIP
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE LT_OBJEK LINES P_COUNT.


ENDFORM.                    " GET_ST_RG_COUNT
*&---------------------------------------------------------------------*
*&      Form  check_xx_xy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_AUSP  text
*----------------------------------------------------------------------*
FORM CHECK_XX_XY TABLES   PT_AUSP STRUCTURE AUSP.
  DATA: L_ATINN  LIKE AUSP-ATINN.
  DATA: L_ATINN1  LIKE AUSP-ATINN.
  DATA: LT_AUSP  LIKE AUSP OCCURS 0 WITH HEADER LINE.
  DATA: L_LINE TYPE I.

  DESCRIBE TABLE PT_AUSP LINES L_LINE.
  IF L_LINE EQ 0.
    EXIT.
  ENDIF.

  PERFORM READ_ATINN USING 'P_WORK_ORDER'   L_ATINN .
  PERFORM READ_ATINN USING 'P_USAGE_CAR'    L_ATINN1 .
* check the P_usage_car  value
  SELECT * INTO TABLE LT_AUSP
   FROM AUSP
   FOR ALL ENTRIES IN PT_AUSP
   WHERE OBJEK = PT_AUSP-OBJEK
    AND  ATINN = L_ATINN1
    AND  KLART = '002'.

  LOOP AT PT_AUSP.
    CLEAR: LT_AUSP.
    READ TABLE LT_AUSP WITH KEY OBJEK = PT_AUSP-OBJEK.
    IF SY-SUBRC EQ 0 AND
       ( LT_AUSP-ATWRT = 'S' OR
         LT_AUSP-ATWRT = 'D' ).
      DELETE PT_AUSP.
    ENDIF.
  ENDLOOP.

* check if 'XX', 'XY' car
  CLEAR: LT_AUSP, LT_AUSP[].
  SELECT * INTO TABLE LT_AUSP
   FROM AUSP
   FOR ALL ENTRIES IN PT_AUSP
   WHERE OBJEK = PT_AUSP-OBJEK
    AND  ATINN = L_ATINN
    AND  KLART = '002'.

* check the work order value
  LOOP AT PT_AUSP.
    CLEAR: LT_AUSP.
    READ TABLE LT_AUSP WITH KEY OBJEK = PT_AUSP-OBJEK.
    IF SY-SUBRC NE 0.
      DELETE PT_AUSP.  "                 usage : 'S' OR 'D'
    ELSE.
      IF ST_5290_INPUT-TYP = 1.
        IF LT_AUSP-ATWRT CS 'XX' OR
           LT_AUSP-ATWRT CS 'XY'.
          DELETE PT_AUSP.               "COMPLETE
        ENDIF.
      ELSEIF ST_5290_INPUT-TYP = 2.
        IF NOT ( LT_AUSP-ATWRT CS 'XX' OR
           LT_AUSP-ATWRT CS 'XY' ).
          DELETE PT_AUSP.               "BIW & BIP
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_xx_xy
*&---------------------------------------------------------------------*
*&      Form  make_gap
*&---------------------------------------------------------------------*
* making the time span for each display record, regular gap is 1 hour
* bur for lunch break record, the gap is lunch break time
* each one is start from shift start time:
* sample: 3600(7:30),7200(8:30), 10800, .... 18000(11:30)
*         20700(12:15) 24300(13:15)....38700(17:15)...
*         the lunch(meal) break is a separate peiod. Other period gap
*         is one hour.
* for perfoamance reason, use 12 varibles to store the result
*----------------------------------------------------------------------*
*      -->P_WA_GAP01  text
*----------------------------------------------------------------------*
FORM MAKE_GAP USING    P_SHIFT
                       P_01 P_02 P_03 P_04 P_05 P_06
                       P_07 P_08 P_09 P_10 P_11 P_12
                       P_CNT.

  DATA: L_BEG_S   LIKE ST_5290_INPUT-FTIME,
        L_END_S   LIKE ST_5290_INPUT-TTIME,
        L_BEG_T   LIKE ST_5290_INPUT-BTIME,
        L_END_T   LIKE ST_5290_INPUT-ETIME,
        L_GAP     TYPE I,             "GAP TIME IN SECONDS BETWEEN TWO
        L_PRE     TYPE I,             "PREVIOUS TIME IN SECONDS

        L_PRE_T   LIKE ST_5290_INPUT-BTIME,
        L_PRE_S   LIKE ST_5290_INPUT-TTIME,
        L_BBEG    LIKE ST_5290_INPUT-BTIME,  "BREAK BEGIN TIME
        L_BEND    LIKE ST_5290_INPUT-ETIME,  "BREAK END TIIME
        L_BBEG_S  LIKE ST_5290_INPUT-FTIME,
        L_BEND_S  LIKE ST_5290_INPUT-FTIME,
        L_CNT(2)  TYPE N,
        L_TEXT(4) TYPE C,
        L_FLAG    TYPE C,
        L_TEMP    LIKE ST_5290_INPUT-ETIME.
  FIELD-SYMBOLS: <PXX>.

  CLEAR: IT_5290TIME, IT_5290TIME[], P_CNT,
         P_01, P_02, P_03, P_04, P_05, P_06,
         P_07, P_08, P_09, P_10, P_11, P_12.
  IF P_SHIFT = 1.
    L_BEG_S  =  ST_5290_INPUT-FTIME.  "START IN SECONDS
    L_END_S  =  ST_5290_INPUT-TTIME.  "END   IN SECONDS
    L_BEG_T  =  ST_5290_INPUT-BTIME.  "START TIME
    L_END_T  =  ST_5290_INPUT-ETIME.  "END   TIME
** Furong on 06/12/12 for 3 shift
  ELSEIF P_SHIFT = 2.
    L_BEG_S  =  ST_5290_INPUT-FTIME_2.  "START IN SECONDS
    L_END_S  =  ST_5290_INPUT-TTIME_2.  "END   IN SECONDS
    L_BEG_T  =  ST_5290_INPUT-BTIME_2.  "START TIME
    L_END_T  =  ST_5290_INPUT-ETIME_2.  "END   TIME
  ELSEIF P_SHIFT = 3.
    L_BEG_S  =  ST_5290_INPUT-FTIME_3.  "START IN SECONDS
    L_END_S  =  ST_5290_INPUT-TTIME_3.  "END   IN SECONDS
    L_BEG_T  =  ST_5290_INPUT-BTIME_3.  "START TIME
    L_END_T  =  ST_5290_INPUT-ETIME_3.  "END   TIME
  ENDIF.
*  ELSE.
*    l_beg_s  =  st_5290_input-ftime_2.  "START IN SECONDS
*    l_end_s  =  st_5290_input-ttime_2.  "END   IN SECONDS
*    l_beg_t  =  st_5290_input-btime_2.  "START TIME
*    l_end_t  =  st_5290_input-etime_2.  "END   TIME
*  ENDIF.
** End on 06/12/12

* if no schedule for the shift, all should be zero
  IF  L_BEG_S = 0 AND
      L_END_S = 0.
    EXIT.
  ENDIF.

* if shift cross middle night
  IF L_END_S LT L_BEG_S.
    L_END_S = L_END_S + 86400.
  ENDIF.
* get the break time
  READ TABLE IT_BREAK WITH KEY PAUNR = P_SHIFT.
  IF SY-SUBRC NE 0.
    L_BBEG = '000000'.
    L_BEND = '000000'.
  ELSE.
    L_BBEG = IT_BREAK-PAUBEG.
    L_BEND = IT_BREAK-PAUEND.
    L_BBEG_S = L_BBEG(02) * 3600 + L_BBEG+2(2) * 60 + L_BBEG+4(2).
    L_BEND_S = L_BEND(02) * 3600 + L_BEND+2(2) * 60 + L_BEND+4(2).
    IF L_BEND LT L_BBEG.               "cross middle night
      L_BEND_S = L_BEND_S + 86400.
    ENDIF.
** CHANGED BY FURONG
*    if l_bend lt l_beg_t.              "break begin after middle night
*      l_bend_s = l_bend_s + 86400.
*      l_bbeg_s = l_bbeg_s + 86400.
*    endif.
** END OF CHANGE.
  ENDIF.

  L_PRE = 0.                     "INITIAL VALUE
  L_PRE_T = L_BEG_T.             "previous gap end time
  L_PRE_S = L_BEG_S.             "previous gap end time in seconds
  L_CNT = '00'.


* make 12 gap values
** Furong on 06/12/12 for 3 shift
*  DO 12 TIMES.
  DO 8 TIMES.
** End on 06/12/12
    L_CNT = L_CNT + 1.
    CONCATENATE 'P_' L_CNT INTO L_TEXT.
    ASSIGN (L_TEXT) TO <PXX>.
    IT_5290TIME-TIME1 = L_PRE_T.
** on 02/28/13
    IF P_SHIFT = 1 and L_PRE_T = '063000'
        and ST_5290_INPUT-DATE >= '20130301'.
      L_PRE_T = L_PRE_T + 900.
      L_PRE = L_PRE + 900.
      L_PRE_S = L_PRE_s + 900.
    endif.
** end on 02/28/13
*      make flag
    IF L_PRE_S LT L_BBEG_S .
      L_FLAG = 'B' .              " BEFORE BREAK
    ELSEIF L_PRE_S = L_BBEG_S.
      L_FLAG = 'X'.               " BREAK TIME
    ELSEIF L_PRE_S GE L_END_S .   " PASS SHIFT END

      L_FLAG = 'E'.
    ELSE.
      L_FLAG = 'A' .              " AFTER BREAK AND BEFORE SHIFT END
    ENDIF.

    CASE L_FLAG.
      WHEN 'B'.
*        preset one hour gap
        L_PRE = L_PRE + 3600.
        L_PRE_T = L_PRE_T + 3600.
        L_PRE_S = L_PRE_S + 3600.
*        comparing with break time
        IF L_BBEG NE '000000' AND    "EXIST BREAK TIME
           L_BEND NE '000000'.
          IF L_PRE_S GT L_BBEG_S.     "PASS BREAK BEGIN TIME
            L_PRE = L_PRE  + L_BBEG_S - L_PRE_S.
            L_PRE_S = L_BBEG_S.
            L_PRE_T = L_BBEG.
          ENDIF.
        ENDIF.
        MOVE L_PRE TO <PXX>.
        P_CNT = P_CNT + 1.
      WHEN 'X'.
        L_PRE   = L_PRE   + L_BEND_S - L_BBEG_S.
        L_PRE_S = L_PRE_S + L_BEND_S - L_BBEG_S.
        L_PRE_T = L_BEND.
        MOVE L_PRE TO <PXX>.
        P_CNT = P_CNT + 1.
      WHEN 'A'.
*       preset one hour gap
        L_PRE   = L_PRE   + 3600.
        L_PRE_S = L_PRE_S + 3600.
        L_PRE_T = L_PRE_T + 3600.
*        comparing with shift end time
        IF L_PRE_S GT L_END_S.     "PASS shift end TIME
          L_PRE   = L_PRE + L_END_S - L_PRE_S.
          L_PRE_S = L_END_S.
          L_PRE_T = L_END_T.
        ENDIF.
        MOVE L_PRE TO <PXX>.
        P_CNT = P_CNT + 1.
      WHEN 'E'.
*       preset one hour gap
        L_PRE   = L_PRE + 3600.
        L_PRE_S = L_PRE_S + 3600.
        L_PRE_T = L_PRE_T + 3600.
*
        MOVE L_PRE TO <PXX>.

    ENDCASE.
*    save the time gap list
    IT_5290TIME-SEQ = L_CNT.
    IF L_BEG_S = 0  AND        "check if shift time exist
** changed by Furong
       L_END_S = 0.
** end of change
      IT_5290TIME-TIME1 = '000000'. "no shift set 000000
      IT_5290TIME-TIME2 = '000000'. "no shift set 000000
    ELSE.
      IT_5290TIME-TIME2 = L_PRE_T.
    ENDIF.
    APPEND IT_5290TIME.
  ENDDO.
** Furong on 06/12/12 for 3 shift
*  IF p_shift = 2.
*    READ TABLE it_5290time INDEX 12.
*    it_5290time-time2 =  it_5290time-time2 + 1800.
*    MODIFY it_5290time INDEX 12.
*  ENDIF.
** End on 06/12/12
ENDFORM.       "make_gap
*&---------------------------------------------------------------------*
*&      Form  download_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_APP237.
  CLEAR IT_EXCEL_2107.
*  REFRESH it_excel_237.
*  MOVE 'Body Number' TO it_excel_239-objek.
*  MOVE 'MITU Date'   TO it_excel_239-mitu_date.
*  MOVE 'Body Serial' TO it_excel_239-body_serial.
*  MOVE 'Model'       TO it_excel_239-model.
*  MOVE 'Spec'        TO it_excel_239-mi.
*  MOVE 'OCN'         TO it_excel_239-ocn.
*  MOVE 'Work Order'  TO it_excel_239-workorder.
*  MOVE 'External Color' TO it_excel_239-ext_color.
*  MOVE 'Internal Color' TO it_excel_239-int_color.
*  MOVE 'Sequence Serial' TO it_excel_239-sequence_serial.
*  MOVE 'Sequence Date' TO it_excel_239-sequence_date.
*  APPEND it_excel_237.

  LOOP AT IT_DLS_2107.
    CLEAR IT_EXCEL_2107.
    MOVE-CORRESPONDING IT_DLS_2107 TO IT_EXCEL_2107.
    APPEND IT_EXCEL_2107.
  ENDLOOP.

  PERFORM CALL_FUNCTION_DOWNLOAD_APP237.

ENDFORM.                    " download_app237
*&---------------------------------------------------------------------*
*&      Form  call_function_download_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION_DOWNLOAD_APP237.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'DELAYVEH.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = IT_EXCEL_2107
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " call_function_download_app237

** Changed by Furong on 03/30/07  >> Help desk: 73V9584258
**                                >> Transport Request: UD1K940212

*&---------------------------------------------------------------------*
*&      Form  download_data_app272
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DATA_APP272.
  CLEAR: IT_EXCEL_APP272, IT_EXCEL_APP272[].
  PERFORM SET_HEADER_APP272         TABLES IT_EXCEL_APP272.
  PERFORM SET_BODY_APP272           TABLES IT_EXCEL_APP272.
  PERFORM CALL_FUNC_DOWNLOAD_APP272 TABLES IT_EXCEL_APP272.
ENDFORM.                    " download_data_app272
*&---------------------------------------------------------------------*
*&      Form  set_header_app272
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP272  text
*----------------------------------------------------------------------*
FORM SET_HEADER_APP272 TABLES   P_IT_EXCEL STRUCTURE IT_EXCEL_APP272.
  WRITE: 'Work Order No' TO P_IT_EXCEL-P001,
         'Exc' TO P_IT_EXCEL-P002,
         'Int' TO P_IT_EXCEL-P003,
         'Y T P M D S H X' TO P_IT_EXCEL-P004,
         'Mod Qty' TO P_IT_EXCEL-P005,
         'Forecast' TO P_IT_EXCEL-P006,
         'Plan' TO P_IT_EXCEL-P007,
         'Sequence' TO P_IT_EXCEL-P008.
  READ TABLE IT_APP272_DATE INDEX 1.
  WRITE: IT_APP272_DATE-DAY01 TO P_IT_EXCEL-P009,
         IT_APP272_DATE-DAY02 TO P_IT_EXCEL-P010,
         IT_APP272_DATE-DAY03 TO P_IT_EXCEL-P011,
         IT_APP272_DATE-DAY04 TO P_IT_EXCEL-P012,
         IT_APP272_DATE-DAY05 TO P_IT_EXCEL-P013,
         IT_APP272_DATE-DAY06 TO P_IT_EXCEL-P014,
         IT_APP272_DATE-DAY07 TO P_IT_EXCEL-P015,
         IT_APP272_DATE-DAY08 TO P_IT_EXCEL-P016,
         IT_APP272_DATE-DAY09 TO P_IT_EXCEL-P017,
         IT_APP272_DATE-DAY10 TO P_IT_EXCEL-P018,
         IT_APP272_DATE-DAY11 TO P_IT_EXCEL-P019,
         IT_APP272_DATE-DAY12 TO P_IT_EXCEL-P020,
         IT_APP272_DATE-DAY13 TO P_IT_EXCEL-P021,
         IT_APP272_DATE-DAY14 TO P_IT_EXCEL-P022,
         IT_APP272_DATE-DAY15 TO P_IT_EXCEL-P023.
  APPEND P_IT_EXCEL.
ENDFORM.                    " set_header_app272
*&---------------------------------------------------------------------*
*&      Form  set_body_app272
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP272  text
*----------------------------------------------------------------------*
FORM SET_BODY_APP272 TABLES P_IT STRUCTURE IT_EXCEL_APP272.
  LOOP AT IT_APP272_01.
    CLEAR P_IT.
    MOVE-CORRESPONDING IT_APP272_01 TO P_IT.
    APPEND P_IT.
  ENDLOOP.

ENDFORM.                    " set_body_app272
*&---------------------------------------------------------------------*
*&      Form  call_func_download_app272
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP272  text
*----------------------------------------------------------------------*
FORM CALL_FUNC_DOWNLOAD_APP272 TABLES   P_IT STRUCTURE IT_EXCEL_APP272.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      FILENAME                = 'Input Plan Per Work Order.XLS'
      FILETYPE                = 'DAT'
      ITEM                    = ' '
      FILETYPE_NO_CHANGE      = 'X'
      FILETYPE_NO_SHOW        = 'X'
    TABLES
      DATA_TAB                = P_IT
    EXCEPTIONS
      INVALID_FILESIZE        = 1
      INVALID_TABLE_WIDTH     = 2
      INVALID_TYPE            = 3
      NO_BATCH                = 4
      UNKNOWN_ERROR           = 5
      GUI_REFUSE_FILETRANSFER = 6
      OTHERS                  = 7.

ENDFORM.                    " call_func_download_app272
** end of change
