*----------------------------------------------------------------------*
*   INCLUDE ZRPP301L_CS12_F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  CLEAR: P_ATWRE, P_ATWRI.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  IF P_AENNR IS INITIAL.
    IF P_DATUV IS INITIAL.
      MESSAGE S000 WITH 'E: Enter valid from date'.
    ENDIF.
  ELSE.
    SELECT SINGLE DATUV
                FROM AENR
                INTO P_DATUV
                WHERE AENNR EQ P_AENNR.
    IF SY-SUBRC EQ 0.

    ELSE.
      MESSAGE I000 WITH 'Change number ' P_AENNR ' does not exist'.
      LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
        IF SCREEN-GROUP1 = 'GR1'.
          SCREEN-INPUT   = 0.
        ENDIF.
        MODIFY SCREEN.
        CLEAR SCREEN.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST
*&---------------------------------------------------------------------*
FORM HELP_REQUEST.
  DATA : BEGIN OF L_MARA OCCURS 0,
           MATNR  LIKE MARA-MATNR,
           WERKS  LIKE MAST-WERKS,
           STLAN  LIKE MAST-STLAN,
           STLAL  LIKE MAST-STLAL,
           MAKTX  LIKE MAKT-MAKTX.
  DATA : END OF L_MARA.
  DATA :   L_MATNR  LIKE MARA-MATNR.
  DATA :   L_ATWRE  LIKE IBSYMBOL-ATWRT,
           L_ATWRI  LIKE IBSYMBOL-ATWRT.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.
  PERFORM VALUE_READ USING: 'P_MATNR'.

  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_MATNR = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.
  IF P_MATNR IS INITIAL.
    CONCATENATE P_MATNR '%' INTO L_MATNR.
    SELECT A~MATNR
           A~WERKS
           A~STLAN
           A~STLAL
           C~MAKTX
           INTO TABLE L_MARA
           FROM MAST AS A INNER JOIN MARA AS B
                            ON A~MATNR EQ B~MATNR
                          INNER JOIN MAKT AS C
                            ON B~MATNR EQ C~MATNR
           WHERE A~MATNR LIKE L_MATNR
           AND   B~MTART EQ   'FERT'
           AND   C~SPRAS EQ   SY-LANGU.
  ELSE.
    REPLACE '*' WITH '%' INTO P_MATNR.
    CONCATENATE P_MATNR '%' INTO L_MATNR.
    SELECT A~MATNR
           A~WERKS
           A~STLAN
           A~STLAL
           C~MAKTX
           INTO TABLE L_MARA
           FROM MAST AS A INNER JOIN MARA AS B
                            ON A~MATNR EQ B~MATNR
                          INNER JOIN MAKT AS C
                            ON B~MATNR EQ C~MATNR
           WHERE A~MATNR LIKE L_MATNR
*         AND   B~MTART EQ   'FERT'
           AND   C~SPRAS EQ   SY-LANGU.
  ENDIF.

  SORT L_MARA BY MATNR WERKS STLAN STLAL.
  LOOP AT L_MARA.
    VALUETAB-VALUE = L_MARA-MATNR.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = L_MARA-WERKS.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = L_MARA-STLAN.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = L_MARA-STLAL.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = L_MARA-MAKTX.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'MARA' 'MATNR' 'X',
                            'MAST' 'WERKS' 'X',
                            'MAST' 'STLAN' 'X',
                            'MAST' 'STLAL' 'X',
                            'MAKT' 'MAKTX' ' '.
  PERFORM HELP_VALUES_GET.

  DATA: BEGIN OF LT_IBSYMBOL OCCURS 0,
          ATWRT TYPE IBSYMBOL-ATWRT,
          ATNAM TYPE CABN-ATNAM,
        END OF LT_IBSYMBOL.

  IF SELECT_INDEX > 0.
    READ TABLE L_MARA   INDEX SELECT_INDEX.
    PERFORM COLOR_SEARCH USING    L_MARA-MATNR.

    READ TABLE IT_IBSYMBOL WITH KEY ATNAM = 'COLOREXT'.
    L_ATWRE = IT_IBSYMBOL-ATWRT.
    READ TABLE IT_IBSYMBOL WITH KEY ATNAM = 'COLORINT'.
    L_ATWRI = IT_IBSYMBOL-ATWRT.

    PERFORM VALUE_UPDATE USING:
            ' '   'P_MATNR' L_MARA-MATNR 0,
            ' '   'P_WERKS' L_MARA-WERKS 0,
            ' '   'P_STLAN' L_MARA-STLAN 0,
            ' '   'P_STLAL' L_MARA-STLAL 0,
            ' '   'P_ATWRE' L_ATWRE 0,
            'X'   'P_ATWRI' L_ATWRI 0.
  ENDIF.

ENDFORM.                    " HELP_REQUEST
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_STLAN
*&---------------------------------------------------------------------*
FORM HELP_REQUEST_STLAN.
  DATA : BEGIN OF LT_MAST OCCURS 0,
           MATNR  LIKE MARA-MATNR,
           WERKS  LIKE MAST-WERKS,
           STLAN  LIKE MAST-STLAN,
           STLAL  LIKE MAST-STLAL.
  DATA : END OF LT_MAST.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_MATNR'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_MATNR = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.
  PERFORM VALUE_READ USING: 'P_WERKS'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_WERKS = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  SELECT MATNR
         WERKS
         STLAN
         STLAL
         INTO TABLE LT_MAST
         FROM MAST
         WHERE MATNR EQ P_MATNR
         AND   WERKS EQ P_WERKS.


  LOOP AT LT_MAST.
    VALUETAB-VALUE = LT_MAST-MATNR.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MAST-WERKS.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MAST-STLAN.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MAST-STLAL.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'MAST' 'MATNR' ' ',
                            'MAST' 'WERKS' ' ',
                            'MAST' 'STLAN' 'X',
                            'MAST' 'STLAL' 'X'.
  PERFORM HELP_VALUES_GET.


  IF SELECT_INDEX > 0.
    READ TABLE LT_MAST   INDEX SELECT_INDEX.
    PERFORM VALUE_UPDATE USING:
            ' '   'P_STLAN' LT_MAST-STLAN 0,
            'X'   'P_STLAL' LT_MAST-STLAL 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_STLAN
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_STLAL
*&---------------------------------------------------------------------*
FORM HELP_REQUEST_STLAL.
  DATA : BEGIN OF LT_MAST OCCURS 0,
           MATNR  LIKE MARA-MATNR,
           WERKS  LIKE MAST-WERKS,
           STLAN  LIKE MAST-STLAN,
           STLAL  LIKE MAST-STLAL.
  DATA : END OF LT_MAST.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_MATNR'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_MATNR = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.
  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.
  PERFORM VALUE_READ USING: 'P_WERKS'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_WERKS = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  SELECT MATNR
         WERKS
         STLAN
         STLAL
         INTO TABLE LT_MAST
         FROM MAST
         WHERE MATNR EQ P_MATNR
         AND   WERKS EQ P_WERKS.


  LOOP AT LT_MAST.
    VALUETAB-VALUE = LT_MAST-MATNR.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MAST-WERKS.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MAST-STLAN.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = LT_MAST-STLAL.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'MAST' 'MATNR' ' ',
                            'MAST' 'WERKS' ' ',
                            'MAST' 'STLAN' ' ',
                            'MAST' 'STLAL' 'X'.
  PERFORM HELP_VALUES_GET.


  IF SELECT_INDEX > 0.
    READ TABLE LT_MAST   INDEX SELECT_INDEX.

    PERFORM VALUE_UPDATE USING:
            ' '   'P_STLAN' LT_MAST-STLAN 0,
            'X'   'P_STLAL' LT_MAST-STLAL 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_STLAL
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_NAME
*&---------------------------------------------------------------------*
FORM HELP_REQUEST_NAME.
*  DATA L_T001W   LIKE T001W   OCCURS 0 WITH HEADER LINE.

  DATA IT_KSML   LIKE TABLE OF KSML  WITH HEADER LINE.
  DATA : BEGIN OF IT_CAWN OCCURS 0,
          ATWRT   LIKE  CAWN-ATWRT,
          ATWTB   LIKE  CAWNT-ATWTB.
  DATA : END OF IT_CAWN.

  DATA :  L_CUOBJ   LIKE  INOB-CUOBJ,
          L_CLINT   LIKE  KLAH-CLINT.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_MATNR'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_MATNR = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE CUOBJ
         INTO L_CUOBJ
         FROM INOB
         WHERE KLART EQ '300'
           AND OBTAB EQ 'MARA'
           AND OBJEK EQ P_MATNR.

  SELECT SINGLE CLINT
         INTO L_CLINT
         FROM KSSK
         WHERE OBJEK EQ L_CUOBJ
           AND MAFID EQ 'O'
           AND KLART EQ '300'.

  SELECT *
         INTO TABLE IT_KSML
         FROM KSML
         WHERE CLINT EQ L_CLINT.

  DATA L_TABIX   LIKE SY-TABIX.
  LOOP AT IT_KSML.
    L_TABIX = SY-TABIX.
    SELECT SINGLE *
              FROM CABN
              WHERE ATINN EQ IT_KSML-IMERK
                AND ATNAM EQ 'COLOREXT'.
    IF SY-SUBRC NE 0.
      DELETE IT_KSML INDEX L_TABIX.
    ENDIF.
  ENDLOOP.

  READ TABLE IT_KSML INDEX 1.
  SELECT A~ATWRT
         B~ATWTB
         INTO TABLE IT_CAWN
         FROM CAWN AS A INNER JOIN CAWNT AS B
                        ON  A~ATINN EQ B~ATINN
                        AND A~ATZHL EQ B~ATZHL
         WHERE A~ATINN EQ IT_KSML-OMERK.
  SORT IT_CAWN.
  IT_CAWN-ATWRT = 'No entry'.
  INSERT IT_CAWN INDEX 1.
  CLEAR: IT_CAWN.
  LOOP AT IT_CAWN.
    VALUETAB-VALUE = IT_CAWN-ATWRT.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = IT_CAWN-ATWTB.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'CAWN' 'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM HELP_VALUES_GET.

  IF SELECT_INDEX > 0.
    READ TABLE IT_CAWN   INDEX SELECT_INDEX.
    PERFORM VALUE_UPDATE USING:
            'X'   'P_ATWRE' IT_CAWN-ATWRT 0.
*            'X'   'P_NAME1' L_T001W-NAME1 0.
  ENDIF.

ENDFORM.                    " HELP_REQUEST_NAME
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_NAME1
*&---------------------------------------------------------------------*
FORM HELP_REQUEST_NAME1.
*  DATA L_T001W   LIKE T001W   OCCURS 0 WITH HEADER LINE.

  DATA IT_KSML   LIKE TABLE OF KSML  WITH HEADER LINE.
  DATA : BEGIN OF IT_CAWN OCCURS 0,
          ATWRT   LIKE  CAWN-ATWRT,
          ATWTB   LIKE  CAWNT-ATWTB.
  DATA : END OF IT_CAWN.

  DATA :  L_CUOBJ   LIKE  INOB-CUOBJ,
          L_CLINT   LIKE  KLAH-CLINT.

  CLEAR DYNPREAD. REFRESH DYNPREAD.
  CLEAR VALUETAB. REFRESH VALUETAB.
  CLEAR FIELDS.   REFRESH FIELDS.

  PERFORM VALUE_READ USING: 'P_MATNR'.
  LOOP AT DYNPREAD.
    CASE SY-TABIX.
      WHEN 1. P_MATNR = DYNPREAD-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE CUOBJ
         INTO L_CUOBJ
         FROM INOB
         WHERE KLART EQ '300'
           AND OBTAB EQ 'MARA'
           AND OBJEK EQ P_MATNR.

  SELECT SINGLE CLINT
         INTO L_CLINT
         FROM KSSK
         WHERE OBJEK EQ L_CUOBJ
           AND MAFID EQ 'O'
           AND KLART EQ '300'.

  SELECT *
         INTO TABLE IT_KSML
         FROM KSML
         WHERE CLINT EQ L_CLINT.

  DATA L_TABIX   LIKE SY-TABIX.
  LOOP AT IT_KSML.
    L_TABIX = SY-TABIX.
    SELECT SINGLE *
              FROM CABN
              WHERE ATINN EQ IT_KSML-IMERK
                AND ATNAM EQ 'COLORINT'.
    IF SY-SUBRC NE 0.
      DELETE IT_KSML INDEX L_TABIX.
    ENDIF.
  ENDLOOP.

  READ TABLE IT_KSML INDEX 1.
  SELECT A~ATWRT
         B~ATWTB
         INTO TABLE IT_CAWN
         FROM CAWN AS A INNER JOIN CAWNT AS B
                        ON  A~ATINN EQ B~ATINN
                        AND A~ATZHL EQ B~ATZHL
         WHERE A~ATINN EQ IT_KSML-OMERK.
  SORT IT_CAWN.
  IT_CAWN-ATWRT = 'No entry'.
  INSERT IT_CAWN INDEX 1.
  CLEAR: IT_CAWN.
  LOOP AT IT_CAWN.
    VALUETAB-VALUE = IT_CAWN-ATWRT.
    APPEND VALUETAB. CLEAR VALUETAB.
    VALUETAB-VALUE = IT_CAWN-ATWTB.
    APPEND VALUETAB. CLEAR VALUETAB.
  ENDLOOP.

  PERFORM ADD_FIELDS USING: 'CAWN' 'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM HELP_VALUES_GET.

  IF SELECT_INDEX > 0.
    READ TABLE IT_CAWN   INDEX SELECT_INDEX.
    PERFORM VALUE_UPDATE USING:
            'X'   'P_ATWRI' IT_CAWN-ATWRT 0.
*            'X'   'P_NAME1' L_T001W-NAME1 0.
  ENDIF.

ENDFORM.                    " HELP_REQUEST_NAME1
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
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM HELP_VALUES_GET.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            DISPLAY                   = ' '
       IMPORTING
            INDEX                     = SELECT_INDEX
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
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
FORM VALUE_READ USING  P_NAME.
  DYNPREAD-FIELDNAME = P_NAME. APPEND DYNPREAD.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME                   = SY-CPROG
            DYNUMB                   = SY-DYNNR
       TABLES
            DYNPFIELDS               = DYNPREAD
*      EXCEPTIONS
*           INVALID_ABAPWORKAREA     = 1
*           INVALID_DYNPROFIELD      = 2
*           INVALID_DYNPRONAME       = 3
*           INVALID_DYNPRONUMMER     = 4
*           INVALID_REQUEST          = 5
*           NO_FIELDDESCRIPTION      = 6
*           INVALID_PARAMETER        = 7
*           UNDEFIND_ERROR           = 8
*           DOUBLE_CONVERSION        = 9
*           OTHERS                   = 10
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " VALUE_READ
*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
FORM VALUE_UPDATE USING  P_PROCESS
                         P_FIELDNAME
                         P_FIELDVALUE
                         P_STEPL.
  CLEAR DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = P_FIELDNAME.
  DYNPFIELDS-FIELDVALUE = P_FIELDVALUE.
  IF P_STEPL > 0.
    DYNPFIELDS-STEPL = P_STEPL.
  ENDIF.
  APPEND DYNPFIELDS.      CLEAR DYNPFIELDS.

  IF P_PROCESS EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME               = SY-CPROG
              DYNUMB               = SY-DYNNR
         TABLES
              DYNPFIELDS           = DYNPFIELDS
         EXCEPTIONS
              INVALID_ABAPWORKAREA = 1
              INVALID_DYNPROFIELD  = 2
              INVALID_DYNPRONAME   = 3
              INVALID_DYNPRONUMMER = 4
              INVALID_REQUEST      = 5
              NO_FIELDDESCRIPTION  = 6
              UNDEFIND_ERROR       = 7
              OTHERS               = 8.
    REFRESH DYNPFIELDS.
  ENDIF.

ENDFORM.                    " VALUE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS CHANGING P_CHK.
  DATA: BEGIN OF LT_MAST OCCURS 0,
          STLAL TYPE MAST-STLAL,
        END OF LT_MAST.

  IF P_STLAL IS INITIAL.
    SELECT STLAL
         FROM MAST
         INTO TABLE LT_MAST
         WHERE MATNR EQ P_MATNR
         AND   WERKS EQ P_WERKS
         AND   STLAN EQ P_STLAN.
    IF SY-SUBRC EQ 0.
      SORT LT_MAST BY STLAL DESCENDING.
      READ TABLE LT_MAST INDEX '1'.
      P_STLAL = LT_MAST-STLAL.
    ELSE.
      P_CHK = 'X'.
    ENDIF.
  ELSE.
    SELECT STLAL
         FROM MAST
         INTO TABLE LT_MAST
         WHERE MATNR EQ P_MATNR
         AND   WERKS EQ P_WERKS
         AND   STLAN EQ P_STLAN
         AND   STLAL EQ P_STLAL.
    IF SY-SUBRC NE 0.
      P_CHK = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  CASE 'X'.
    WHEN P_RDO1 OR P_RDO3.
      PERFORM CS_BOM_EXPL_MAT_V2_FIRST.
    WHEN P_RDO2.
      PERFORM CS_BOM_EXPL_MAT_V2_FIRST1.
  ENDCASE.
  PERFORM ALV_STB_TO_DEPENDENCY_DELETE.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2 USING    PA_STB STRUCTURE WA_STB.

  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        LT_DSTST  LIKE  CSDATA-XFELD.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = P_CAPID
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUV
            EMENG  = P_EMENG
            MKTLS  = 'X'
            MEHRS  = 'X'
            MTNRV  = PA_STB-IDNRK
            STLAN  = PA_STB-STLAN
            STLAL  = PA_STB-STLAL
            STPST  = 0
            SVWVO  = 'X'
            WERKS  = PA_STB-WERKS
            VRSVO  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            STB    = LT_STB
            MATCAT = MATCAT
 EXCEPTIONS
   ALT_NOT_FOUND               = 1
   CALL_INVALID                = 2
   MATERIAL_NOT_FOUND          = 3
   MISSING_AUTHORIZATION       = 4
   NO_BOM_FOUND                = 5
   NO_PLANT_DATA               = 6
   NO_SUITABLE_BOM_FOUND       = 7
   CONVERSION_ERROR            = 8
   OTHERS                      = 9
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  REFRESH STB. CLEAR STB.

  STB[] = LT_STB[].
  LOOP AT STB.
    STB-INDEX = SY-TABIX.
*    STB-MNGKO = STB-MNGKO * PA_STB-MNGKO.
    MODIFY STB.

    PERFORM ALV_STB_PREP USING '1'.

  ENDLOOP.

ENDFORM.                    " CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2_1
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2_1 USING    PA_STB STRUCTURE WA_STB.

  DATA: LT_STB    TYPE STPOX OCCURS 0 WITH HEADER LINE,
        LA_STB    TYPE STPOX,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        L_STLNR   LIKE  MAST-STLNR,
        LT_DSTST  LIKE  CSDATA-XFELD.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = P_CAPID
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUV
            EMENG  = P_EMENG
            MKTLS  = 'X'
*            MEHRS  = 'X'
            MTNRV  = PA_STB-IDNRK
            STLAN  = PA_STB-STLAN
            STLAL  = PA_STB-STLAL
            STPST  = 0
            SVWVO  = 'X'
            WERKS  = PA_STB-WERKS
            VRSVO  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            STB    = LT_STB
            MATCAT = MATCAT
 EXCEPTIONS
   ALT_NOT_FOUND               = 1
   CALL_INVALID                = 2
   MATERIAL_NOT_FOUND          = 3
   MISSING_AUTHORIZATION       = 4
   NO_BOM_FOUND                = 5
   NO_PLANT_DATA               = 6
   NO_SUITABLE_BOM_FOUND       = 7
   CONVERSION_ERROR            = 8
   OTHERS                      = 9
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  REFRESH STB. CLEAR STB.

*  STB[] = LT_STB[].
  LOOP AT LT_STB.
    STB-INDEX = SY-TABIX.
    STB = LA_STB = LT_STB.
    IF LA_STB-DUMPS EQ 'x'.
      PERFORM READ_MAST USING   LA_STB-IDNRK
                                LA_STB-WERKS
                                LA_STB-STLAN
                       CHANGING LA_STB-STLAL
                                L_STLNR.
      IF NOT L_STLNR IS INITIAL.
        PERFORM CS_BOM_EXPL_MAT_V2_1 USING LA_STB.
      ENDIF.
    ELSE.
      PERFORM ALV_STB_PREP USING '1'.

    ENDIF.
    CLEAR: LA_STB, STB, LT_STB.

  ENDLOOP.

ENDFORM.                    " CS_BOM_EXPL_MAT_V2_1
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2_FIRST
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2_FIRST.
  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        L_STLNR   LIKE  MAST-STLNR,
        LT_DSTST  LIKE  CSDATA-XFELD.
  CLEAR: L_CUOBJ.
*  PERFORM READ_MARC USING    P_MATNR
*                             P_WERKS
*                    CHANGING L_CUOBJ.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = P_CAPID
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUV
            EMENG  = P_EMENG
            MKTLS  = 'X'
*            MEHRS  = 'X'
            MTNRV  = P_MATNR
            STPST  = 0
            STLAN  = P_STLAN
            STLAL  = P_STLAL
            SVWVO  = 'X'
            WERKS  = P_WERKS
            VRSVO  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            STB    = LT_STB
            MATCAT = SELPOOL
 EXCEPTIONS
   ALT_NOT_FOUND               = 1
   CALL_INVALID                = 2
   MATERIAL_NOT_FOUND          = 3
   MISSING_AUTHORIZATION       = 4
   NO_BOM_FOUND                = 5
   NO_PLANT_DATA               = 6
   NO_SUITABLE_BOM_FOUND       = 7
   CONVERSION_ERROR            = 8
   OTHERS                      = 9
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_STB.
    WA_STB = LT_STB.
    PERFORM READ_MAST USING   WA_STB-IDNRK
                              WA_STB-WERKS
                              WA_STB-STLAN
                     CHANGING WA_STB-STLAL
                              L_STLNR.
    IF NOT L_STLNR IS INITIAL.
      STB = STB_ORIG = WA_STB.
*      IT_STPOX = LT_STB.
      PERFORM ALV_STB_PREP USING '0'.
      PERFORM CS_BOM_EXPL_MAT_V2 USING WA_STB.
    ELSE.
*      MOVE-CORRESPONDING WA_STB TO ALV_STB.
      STB = STB_ORIG = WA_STB.
*      IT_STPOX = LT_STB.
      PERFORM ALV_STB_PREP USING '0'.
    ENDIF.
    CLEAR: IT_STPOX, LT_STB, L_STLNR, ALV_STB.
  ENDLOOP.
ENDFORM.                    " CS_BOM_EXPL_MAT_V2_FIRST
*&---------------------------------------------------------------------*
*&      Form  CS_BOM_EXPL_MAT_V2_FIRST1
*&---------------------------------------------------------------------*
FORM CS_BOM_EXPL_MAT_V2_FIRST1.
  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE,
        LT_MATCAT TYPE  CSCMAT OCCURS 0 WITH HEADER LINE,
        LT_TOPMAT LIKE  CSTMAT,
        L_CUOBJ   LIKE  MARC-CUOBJ,
        L_STLNR   LIKE  MAST-STLNR,
        LT_DSTST  LIKE  CSDATA-XFELD.
  CLEAR: L_CUOBJ.
*  PERFORM READ_MARC USING    P_MATNR
*                             P_WERKS
*                    CHANGING L_CUOBJ.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            AUMNG  = 0
            CAPID  = P_CAPID
*            CUOBJ  = L_CUOBJ
            CUOVS  = '0'
            DATUV  = P_DATUV
            EMENG  = P_EMENG
            MKTLS  = 'X'
*            MEHRS  = 'X'
            MTNRV  = P_MATNR
            STPST  = 0
            STLAN  = P_STLAN
            STLAL  = P_STLAL
            SVWVO  = 'X'
            WERKS  = P_WERKS
            VRSVO  = 'X'
* IMPORTING
*   TOPMAT                      = LT_TOPMAT
*   DSTST                       = LT_DSTST
 TABLES
            STB    = LT_STB
            MATCAT = SELPOOL
 EXCEPTIONS
   ALT_NOT_FOUND               = 1
   CALL_INVALID                = 2
   MATERIAL_NOT_FOUND          = 3
   MISSING_AUTHORIZATION       = 4
   NO_BOM_FOUND                = 5
   NO_PLANT_DATA               = 6
   NO_SUITABLE_BOM_FOUND       = 7
   CONVERSION_ERROR            = 8
   OTHERS                      = 9
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT LT_STB.
    WA_STB = LT_STB.
    IF WA_STB-DUMPS EQ 'x'.
      PERFORM READ_MAST USING   WA_STB-IDNRK
                                WA_STB-WERKS
                                WA_STB-STLAN
                       CHANGING WA_STB-STLAL
                                L_STLNR.
      IF NOT L_STLNR IS INITIAL.
*      STB = STB_ORIG = WA_STB.
*      IT_STPOX = LT_STB.
*      PERFORM ALV_STB_PREP USING '0'.
        PERFORM CS_BOM_EXPL_MAT_V2_1 USING WA_STB.
      ENDIF.
    ELSE.
*      MOVE-CORRESPONDING WA_STB TO ALV_STB.
      STB = STB_ORIG = WA_STB.
*      IT_STPOX = LT_STB.
      PERFORM ALV_STB_PREP USING '0'.
    ENDIF.
    CLEAR: IT_STPOX, LT_STB, L_STLNR, ALV_STB.
  ENDLOOP.
ENDFORM.                    " CS_BOM_EXPL_MAT_V2_FIRST1
*&---------------------------------------------------------------------*
*&      Form  READ_MARC
*&---------------------------------------------------------------------*
FORM READ_MARC USING    Q_MATNR
                        Q_WERKS
               CHANGING P_CUOBJ.
  SELECT SINGLE CUOBJ
              FROM MARC
              INTO P_CUOBJ
              WHERE MATNR EQ Q_MATNR
              AND   WERKS EQ Q_WERKS.
ENDFORM.                    " READ_MARC
*&---------------------------------------------------------------------*
*&      Form  READ_MAST
*&---------------------------------------------------------------------*
FORM READ_MAST USING    Q_MATNR
                        Q_WERKS
                        Q_STLAN
               CHANGING Q_STLAL
                        P_STLNR.
  SELECT SINGLE STLNR
                STLAL
              FROM MAST
              INTO (P_STLNR, Q_STLAL)
              WHERE MATNR EQ Q_MATNR
              AND   WERKS EQ Q_WERKS
              AND   STLAN EQ Q_STLAN.
ENDFORM.                    " READ_MAST
*&---------------------------------------------------------------------*
*&      Form  ALV_STB_TO_DEPENDENCY_DELETE
*&---------------------------------------------------------------------*
FORM ALV_STB_TO_DEPENDENCY_DELETE.
  DATA: L_TABIX    LIKE SY-TABIX,
        L_KNNUM    LIKE CUOB-KNNUM,
        L_KNNAM    LIKE CUKB-KNNAM,
        L_KNNAE    LIKE RCUKD-KNNAM,
        L_KNNAI    LIKE RCUKD-KNNAM,
        L_EXAIN    LIKE RCUKD-KNNAM.
  DATA: BEGIN OF LT_CUOB OCCURS 0,
          KNNUM TYPE CUOB-KNNUM,
        END OF LT_CUOB.

  DATA: BEGIN OF LT_CUKB OCCURS 0,
          KNNAM TYPE CUKB-KNNAM,
        END OF LT_CUKB.
  CONCATENATE P_ATWRE '_' P_ATWRI INTO L_EXAIN.

  PERFORM DEPENDENCY_CHECK_FIELD CHANGING L_KNNAE
                                          L_KNNAI.

  LOOP AT ALV_STB WHERE NOT KNOBJ IS INITIAL.
    L_TABIX = SY-TABIX.

    REFRESH: LT_CUOB, LT_CUKB. CLEAR: LT_CUOB, LT_CUKB.
    SELECT KNNUM
         FROM CUOB
         INTO TABLE LT_CUOB
         WHERE KNTAB EQ 'STPO'
         AND   KNOBJ EQ ALV_STB-KNOBJ.
    IF NOT LT_CUOB[] IS INITIAL.
      SELECT KNNAM
           FROM CUKB
           INTO TABLE LT_CUKB
           FOR ALL ENTRIES IN LT_CUOB
           WHERE KNNUM EQ LT_CUOB-KNNUM.
      IF SY-SUBRC EQ 0.
        SORT LT_CUKB BY KNNAM.
        IF ALV_STB-MTART EQ 'ROH1'.
          READ TABLE LT_CUKB WITH KEY KNNAM = P_ATWRE
                             BINARY SEARCH
                             TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            ALV_STB-POTX1 = P_ATWRE.
            MODIFY ALV_STB INDEX L_TABIX TRANSPORTING POTX1.
          ELSE.
            READ TABLE LT_CUKB WITH KEY KNNAM = P_ATWRI
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
            IF SY-SUBRC EQ 0.
              ALV_STB-POTX1 = P_ATWRI.
              MODIFY ALV_STB INDEX L_TABIX TRANSPORTING POTX1.
            ELSE.
              DELETE ALV_STB INDEX L_TABIX.
            ENDIF.
          ENDIF.

        ELSE.
          READ TABLE LT_CUKB WITH KEY KNNAM = L_KNNAE
                             BINARY SEARCH
                             TRANSPORTING NO FIELDS.
          IF SY-SUBRC EQ 0.
            ALV_STB-POTX1 = L_KNNAE.
            MODIFY ALV_STB INDEX L_TABIX TRANSPORTING POTX1.
          ELSE.
            READ TABLE LT_CUKB WITH KEY KNNAM = L_KNNAI
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
            IF SY-SUBRC EQ 0.
              ALV_STB-POTX1 = L_KNNAI.
              MODIFY ALV_STB INDEX L_TABIX TRANSPORTING POTX1.
            ELSE.
              READ TABLE LT_CUKB WITH KEY KNNAM = L_EXAIN
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
              IF SY-SUBRC EQ 0.
                ALV_STB-POTX1 = L_EXAIN.
                MODIFY ALV_STB INDEX L_TABIX TRANSPORTING POTX1.
              ELSE.
                DELETE ALV_STB INDEX L_TABIX.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ALV_STB_TO_DEPENDENCY_DELETE
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  IF P_RDO1 EQ 'X'.
    PERFORM REUSE_ALV_GRID_DISPLAY.
  ELSEIF P_RDO2 EQ 'X'.
    PERFORM DELETE_MULTI_LEVEL.
    PERFORM REUSE_ALV_GRID_DISPLAY.
  ELSEIF P_RDO3 EQ 'X'.
    PERFORM MULTI_CULUMN.
    PERFORM LEVEL_ALV_GRID_DISPLAY.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  REUSE_ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
FORM REUSE_ALV_GRID_DISPLAY.
* TOTAL LINES
  DESCRIBE TABLE ALV_STB LINES WA_TOTAL_LINE.
* ALV VARIABLE
  PERFORM ALV_VARIABLE_CREATE.
* ALV DISPLAY
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = REPORT_NAME
            I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS_SET'
            I_STRUCTURE_NAME         = 'STPOX_ALV'
            IS_LAYOUT                = ALVLO_STB
            I_SAVE                   = ALVVR_SAV_ALL
            IS_VARIANT               = ALVVR
            IT_EVENTS                = ALV_EVNT_TB_CMPL
            IT_FIELDCAT              = STB_FIELDS_TB
       IMPORTING
            E_EXIT_CAUSED_BY_CALLER  = EXIT_BY_CALLER
            ES_EXIT_CAUSED_BY_USER   = EXIT_BY_USER
       TABLES
            T_OUTTAB                 = ALV_STB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
  IF SY-SUBRC = 0.
    IF EXIT_BY_CALLER = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    ELSE.
*     User left list via F3, F12 or F15
      IF EXIT_BY_USER-BACK = 'X'.       "F3
*       <do_something>.
      ELSE.
        IF EXIT_BY_USER-EXIT = 'X'.     "F15
*         <do_something>.
        ELSE.
          IF EXIT_BY_USER-CANCEL = 'X'. "F12
*           <do_something>.
          ELSE.
*           should not occur!
*           <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  ENDIF.
ENDFORM.                    " REUSE_ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_EVNT_TB_PREP
*&---------------------------------------------------------------------*
FORM ALV_EVNT_TB_PREP USING    EVENT_SPEC TYPE C
                               EVENT_TB TYPE SLIS_T_EVENT.

  DATA: WA_EVENT_TB TYPE SLIS_ALV_EVENT.
  CHECK EVENT_TB[] IS INITIAL.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = EVENT_TB.
  READ TABLE EVENT_TB
    WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
    INTO WA_EVENT_TB.

  IF SY-SUBRC = 0.
    WA_EVENT_TB-FORM = 'ALV_TOP_OF_PAGE'.
    APPEND WA_EVENT_TB TO EVENT_TB.
  ENDIF.


  READ TABLE EVENT_TB
    WITH KEY NAME = SLIS_EV_USER_COMMAND
    INTO WA_EVENT_TB.

  IF SY-SUBRC = 0.
    WA_EVENT_TB-FORM = 'ALV_USER_COMMAND'.
    APPEND WA_EVENT_TB TO EVENT_TB.
  ENDIF.


  READ TABLE EVENT_TB
    WITH KEY NAME = SLIS_EV_PF_STATUS_SET
    INTO WA_EVENT_TB.

  IF SY-SUBRC = 0.
    WA_EVENT_TB-FORM = 'ALV_PF_STATUS_SET'.
    APPEND WA_EVENT_TB TO EVENT_TB.
  ENDIF.

ENDFORM.                    " ALV_EVNT_TB_PREP
*&---------------------------------------------------------------------*
*&      Form  STB_FIELDS_TB_PREP
*&---------------------------------------------------------------------*
FORM STB_FIELDS_TB_PREP.
  CALL FUNCTION 'GET_FIELDTAB'
       EXPORTING
            LANGU    = SY-LANGU
            TABNAME  = 'STPOX_ALV'
            WITHTEXT = ' '
            ONLY     = 'T'
       TABLES
            FIELDTAB = FTAB
       EXCEPTIONS
            OTHERS   = 1.

  LOOP AT FTAB.
    CLEAR: WA_STB_FIELDS_TB.

    CASE FTAB-FIELDNAME.
      WHEN 'DGLVL'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DGLVL'.
        WA_STB_FIELDS_TB-COL_POS   =  1.
        WA_STB_FIELDS_TB-FIX_COLUMN = 'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  5.
        WA_STB_FIELDS_TB-JUST      = 'L' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'POSNR'.
        WA_STB_FIELDS_TB-FIELDNAME = 'POSNR'.
        WA_STB_FIELDS_TB-COL_POS   =  2.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN = 4 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'IDNRK'.
        WA_STB_FIELDS_TB-FIELDNAME = 'IDNRK'.
        WA_STB_FIELDS_TB-COL_POS   =  3.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN = 18.
        WA_STB_FIELDS_TB-ICON       =  'X' .
        WA_STB_FIELDS_TB-JUST      = 'L' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'OJTXP'.
        WA_STB_FIELDS_TB-FIELDNAME = 'OJTXP'.
        WA_STB_FIELDS_TB-COL_POS   =  4.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  30.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MNGKO'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MNGKO'.
        WA_STB_FIELDS_TB-COL_POS   =  5.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  10.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MEINS'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MEINS'.
        WA_STB_FIELDS_TB-COL_POS   =  6.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  3 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'WERKS'.
        WA_STB_FIELDS_TB-FIELDNAME = 'WERKS'.
        WA_STB_FIELDS_TB-COL_POS   =  7.
        WA_STB_FIELDS_TB-OUTPUTLEN = 4 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'AENNR'.
        WA_STB_FIELDS_TB-FIELDNAME = 'AENNR'.
        WA_STB_FIELDS_TB-COL_POS   = 8.
        WA_STB_FIELDS_TB-OUTPUTLEN = 12.
        WA_STB_FIELDS_TB-JUST      = 'R' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'DATUV'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DATUV'.
        WA_STB_FIELDS_TB-COL_POS   = 9.
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        WA_STB_FIELDS_TB-NO_SUM    = 'X'.
        WA_STB_FIELDS_TB-NO_ZERO   = 'X'.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'DATUB'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DATUB'.
        WA_STB_FIELDS_TB-COL_POS   = 10.
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'DUMPS'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DUMPS'.
        WA_STB_FIELDS_TB-COL_POS   = 11.
        WA_STB_FIELDS_TB-OUTPUTLEN = 3 .
        WA_STB_FIELDS_TB-JUST      = 'C' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'KZKFG'.
        WA_STB_FIELDS_TB-FIELDNAME = 'KZKFG'.
        WA_STB_FIELDS_TB-COL_POS   = 12.
        WA_STB_FIELDS_TB-OUTPUTLEN = 1 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MTART'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MTART'.
        WA_STB_FIELDS_TB-COL_POS   = 13.
        WA_STB_FIELDS_TB-OUTPUTLEN = 4 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'KNOBJ'.
        WA_STB_FIELDS_TB-FIELDNAME = 'KNOBJ'.
        WA_STB_FIELDS_TB-COL_POS   = 14.
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'POTX1'.
        WA_STB_FIELDS_TB-FIELDNAME = 'POTX1'.
        WA_STB_FIELDS_TB-COL_POS   = 15.
        WA_STB_FIELDS_TB-OUTPUTLEN = 18.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'EITM'.
        WA_STB_FIELDS_TB-FIELDNAME = 'EITM'.
        WA_STB_FIELDS_TB-COL_POS   = 16.
        WA_STB_FIELDS_TB-OUTPUTLEN = 5 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN OTHERS.
        WA_STB_FIELDS_TB-FIELDNAME = FTAB-FIELDNAME.
        WA_STB_FIELDS_TB-NO_OUT    = 'X'.
        WA_STB_FIELDS_TB-NO_SUM    = 'X'.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " STB_FIELDS_TB_PREP
*&---------------------------------------------------------------------*
*&      Form  ALV_STB_PREP
*&---------------------------------------------------------------------*
FORM ALV_STB_PREP USING P_LEVEL.
*..................................
  CLEAR:
    ALV_STB,
    STB_ORIG,
    STB_ADD.

  STB_ORIG = STB.

  IF STB-MNGKO >= MAX_NUM.
    STB_ADD-OVFLS = UEBERL_KZ.
  ELSE.
    IF STB-MNGKO <= MIN_NUM.
      STB_ADD-OVFLS = UEBERL_KZ.
    ELSE.
      CLEAR: STB_ADD-OVFLS.
    ENDIF.
  ENDIF.

  IF NOT STB-XTLNR IS INITIAL.
    STB_ADD-BOMFL = B_FLAG.
  ENDIF.

  IF    NOT STB-KNOBJ IS INITIAL
     OR NOT STB-CLASS IS INITIAL
     OR NOT STB-KZCLB IS INITIAL.

    STB_ADD-KNOFL = 'X'.
  ENDIF.

  CLEAR:
    STB_ADD-DOBJT,
    STB_ADD-OBJIC.

  CASE STB-OBJTY.
    WHEN OTYP_MAT.
      WRITE: STB_ORIG-IDNRK TO ECFLD.
      STB_ADD-OBJIC = '@A6@'.

    WHEN 'M'.
      WRITE: STB_ORIG-IDNRK TO ECFLD.
      STB_ADD-OBJIC = '@A6@'.

    WHEN OTYP_NOO.
      WRITE: STB_ORIG-POTX1 TO ECFLD.
      STB_ADD-OBJIC = '@0Q@'.

    WHEN OTYP_DOC.
      WRITE STB_ORIG-DOKNR TO ECFLD.                        "note 489354

      IF ECFLD CP '*# '. ENDIF.                             "note 489354
      SY-FDPOS = SY-FDPOS + 1.                              "note 489354

      CONCATENATE
*d      stb_orig-doknr                                      "note 489354
        STB_ORIG-DOKAR
        STB_ORIG-DOKTL
        STB_ORIG-DOKVR
*d      INTO ecfld                                          "note 489354
        INTO ECFLD+SY-FDPOS                                 "note 489354
        SEPARATED BY SPACE.

      STB_ADD-OBJIC = '@AR@'.

    WHEN OTYP_KLA.
      CONCATENATE
        STB_ORIG-CLASS
        STB_ORIG-KLART
        INTO ECFLD
        SEPARATED BY SPACE.

      STB_ADD-OBJIC = '@7C@'.

    WHEN OTYP_NTM.
      WRITE: STB_ORIG-INTRM TO ECFLD.

    WHEN OTHERS.
  ENDCASE.

*d CONDENSE ecfld.                                          "note 515408
  STB_ADD-DOBJT = ECFLD(40).
  CLEAR: ECFLD.

  WRITE STB_ORIG-STUFE TO STB_ADD-DSTUF NO-SIGN.
  PERFORM STUFE_AUFBEREITEN USING P_LEVEL.
  WRITE ANZ_STUFE TO STB_ADD-DGLVL NO-SIGN.

*d  MOVE-CORRESPONDING stb_orig TO alv_stb.                 "note 331962
  MOVE-CORRESPONDING STB_ADD TO ALV_STB.
  MOVE-CORRESPONDING STB_ORIG TO ALV_STB.                   "note 331962

  APPEND ALV_STB.
ENDFORM.                    " ALV_STB_PREP
*&---------------------------------------------------------------------*
*&      Form  STUFE_AUFBEREITEN
*&---------------------------------------------------------------------*
FORM STUFE_AUFBEREITEN USING P_LEVEL.
  STB-STUFE = STB-STUFE + P_LEVEL.
  ANZ_STUFE = STB-STUFE.
  TRANSLATE ANZ_STUFE USING ' .'.
  ANZ_STUFE+10(1) = ' '.

  IF STB-STUFE < 9.
    STB-STUFE = 9 - STB-STUFE.
    SHIFT ANZ_STUFE BY STB-STUFE PLACES.
    STB-STUFE = 9 - STB-STUFE.
  ENDIF.
ENDFORM.                    " STUFE_AUFBEREITEN
*---------------------------------------------------------------------*
*       FORM alv_top_of_page                                          *
*---------------------------------------------------------------------*
FORM ALV_TOP_OF_PAGE.
*.....................................

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*            I_LOGO             = 'Z_HYUNDAI_LOGO'
*            I_LOGO             = 'ENJOY_LOGO'
            IT_LIST_COMMENTARY = ALV_TOP_TB.
ENDFORM. "alv_top_of_page
*---------------------------------------------------------------------*
*       FORM alv_top_tb_prep                                          *
*---------------------------------------------------------------------*
FORM ALV_TOP_TB_PREP
  USING
    TOP_TB TYPE SLIS_T_LISTHEADER.
*......................................

  DATA:
    WA_TOP_TB TYPE SLIS_LISTHEADER.

  DATA: LKL_MATNR LIKE MARA-MATNR.
  DATA L_MAKTX LIKE MAKT-MAKTX.
  DATA L_LINE(10).
*......................................
  SELECT SINGLE MAKTX
              FROM MAKT
              INTO L_MAKTX
              WHERE MATNR EQ P_MATNR.

  CLEAR WA_TOP_TB.
  IF WA_TOP_TB IS INITIAL.
    WA_TOP_TB-TYP  = 'S'.
    ECFLD = 'Material'.
    WA_TOP_TB-KEY  = ECFLD(30).
    CLEAR: ECFLD.
    WRITE P_MATNR TO LKL_MATNR.
    WRITE WA_TOTAL_LINE TO L_LINE LEFT-JUSTIFIED.
    CONCATENATE LKL_MATNR
    '/          TOTAL LINES'
    L_LINE INTO WA_TOP_TB-INFO
                                 SEPARATED BY ' / '.
    APPEND WA_TOP_TB TO TOP_TB.

*    WA_TOP_TB-INFO = LKL_MATNR.
*    APPEND WA_TOP_TB TO TOP_TB.
  ENDIF.

  CLEAR WA_TOP_TB.
  WA_TOP_TB-TYP  = 'S'.
  ECFLD    = 'PLANT'.
  ECFLD+5  = '/'.
  ECFLD+6  = 'USAGE'.
  ECFLD+11 = '/'.
  ECFLD+12 = 'Alt. <-- 5 characters'.
  WA_TOP_TB-KEY  = ECFLD(17).
  CONDENSE WA_TOP_TB-KEY NO-GAPS.
  CLEAR: ECFLD.

  CONCATENATE
    P_WERKS
    P_STLAN
    P_STLAL
    INTO WA_TOP_TB-INFO
    SEPARATED BY ' / '.
  APPEND WA_TOP_TB TO TOP_TB.

  CLEAR WA_TOP_TB.
  WA_TOP_TB-TYP  = 'S'.
  WA_TOP_TB-KEY = 'Description'.
  CONDENSE WA_TOP_TB-KEY.
  WA_TOP_TB-INFO = L_MAKTX.
  APPEND WA_TOP_TB TO TOP_TB.

  CLEAR WA_TOP_TB.
  WA_TOP_TB-TYP  = 'S'.
  ECFLD = 'Base qty     ; '.
  ECFLD+13 = ' ('.
  ECFLD+15 = 'EA'.
  ECFLD+18 = ')'.
  WA_TOP_TB-KEY  = ECFLD(19).
  CLEAR: ECFLD.
  WRITE P_EMENG TO WA_TOP_TB-INFO .
  CONDENSE WA_TOP_TB-INFO.
  APPEND WA_TOP_TB TO TOP_TB.

  CLEAR WA_TOP_TB.
  WA_TOP_TB-TYP  = 'S'.
* so it looks better
  APPEND WA_TOP_TB TO TOP_TB.

*    WA_TOP_TB-TYP  = 'S'.
*    ECFLD = 'TOTLA LINE '.
*    WA_TOP_TB-KEY  = ECFLD(11).
*    CLEAR: ECFLD.
*    WRITE WA_TOTAL_LINE TO L_LINE LEFT-JUSTIFIED.
*    WA_TOP_TB-INFO = L_LINE.
*    APPEND WA_TOP_TB TO TOP_TB.

ENDFORM. "alv_top_tb_prep
*---------------------------------------------------------------------*
*       FORM alv_user_command                                         *
*---------------------------------------------------------------------*
FORM ALV_USER_COMMAND USING I_UCOMM    LIKE SY-UCOMM
                            I_SELFIELD TYPE SLIS_SELFIELD.
*.......................................

  CASE I_UCOMM.
    WHEN 'ANMS'.                                            "HGA020150
      LEAVE TO TRANSACTION SY-TCODE.                        "HGA020150

    WHEN 'CSAP' OR '&IC1'.
      SORT STB BY INDEX.

      READ TABLE ALV_STB INDEX I_SELFIELD-TABINDEX.
*      CALL TRANSACTION 'CS03' AND SKIP FIRST SCREEN .
      IF NOT ALV_STB-INDEX IS INITIAL.
        READ TABLE STB INDEX ALV_STB-INDEX.
        READ TABLE MATCAT INDEX STB-TTIDX.
      ELSE.
        CLEAR:
           STB,
           MATCAT.
      ENDIF.

*      PERFORM POSITION_ANZEIGEN.

    WHEN 'CSAO'.
      SORT STB BY INDEX.

      READ TABLE ALV_STB INDEX I_SELFIELD-TABINDEX.
      IF NOT ALV_STB-INDEX IS INITIAL.
        READ TABLE STB INDEX ALV_STB-INDEX.
      ELSE.
        CLEAR:
           STB.
      ENDIF.

*      PERFORM OBJEKT_ANZEIGEN.

    WHEN 'CSWU'.
      SORT STB BY INDEX.

      READ TABLE ALV_STB INDEX I_SELFIELD-TABINDEX.
      IF NOT ALV_STB-INDEX IS INITIAL.
        READ TABLE STB INDEX ALV_STB-INDEX.
      ELSE.
        CLEAR:
           STB.
      ENDIF.

*      PERFORM VERWENDUNG_ANZEIGEN.

    WHEN 'CSSL'.
*      PERFORM ALV_DSP_SEL_DSP.

  ENDCASE.
ENDFORM. "alv_user_command
*&---------------------------------------------------------------------
*         Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
FORM ALV_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'S121_ALV' EXCLUDING RT_EXTAB.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIABLE_CREATE
*&---------------------------------------------------------------------*
FORM ALV_VARIABLE_CREATE.
  ALVVR = SY-REPID.
  ALVVR_SAV_ALL = 'A'.
  REPORT_NAME = SY-REPID.
  ALVLO_STB-INFO_FIELDNAME = 'INFO'.
  PERFORM ALV_EVNT_TB_PREP USING 'A'
                                 ALV_EVNT_TB_CMPL.
  PERFORM STB_FIELDS_TB_PREP.
  PERFORM ALV_TOP_TB_PREP USING ALV_TOP_TB.
ENDFORM.                    " ALV_VARIABLE_CREATE
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION_SCREEN.
  DATA :   L_ATWRE  LIKE IBSYMBOL-ATWRT,
           L_ATWRI  LIKE IBSYMBOL-ATWRT,
           L_STLAL  LIKE MAST-STLAL.
  IF NOT P_WKOD IS INITIAL.
    SELECT SINGLE FSC
                FROM ZTPP_WOSUM
                INTO P_MATNR
                WHERE WO_SER EQ P_WKOD(9)
                AND   NATION EQ P_WKOD+9(3)
                AND   DEALER EQ P_WKOD+12(2).
  ENDIF.
  SELECT SINGLE MAX( STLAL )
              FROM MAST
              INTO L_STLAL
              WHERE MATNR EQ P_MATNR
              AND   WERKS EQ P_WERKS
              AND   STLAN EQ P_STLAN.
  IF NOT L_STLAL IS INITIAL.
    P_STLAL = L_STLAL.
  ELSE.
    CLEAR P_STLAL.
  ENDIF.
*  IF SY-UCOMM NE 'ONLI'.
  PERFORM COLOR_SEARCH USING    P_MATNR.

  READ TABLE IT_IBSYMBOL WITH KEY ATNAM = 'COLOREXT'.
  L_ATWRE = IT_IBSYMBOL-ATWRT.
  READ TABLE IT_IBSYMBOL WITH KEY ATNAM = 'COLORINT'.
  L_ATWRI = IT_IBSYMBOL-ATWRT.
  IF P_ATWRE IS INITIAL.
    P_ATWRE = L_ATWRE.
  ENDIF.
  IF P_ATWRI IS INITIAL.
    P_ATWRI = L_ATWRI.
  ENDIF.
  TRANSLATE P_ATWRE TO UPPER CASE.
  TRANSLATE P_ATWRI TO UPPER CASE.
*  ENDIF.
*  PERFORM VALUE_UPDATE USING:
*          'X'   'P_ATWRE' L_ATWRE 0,
*          'X'   'P_ATWRI' L_ATWRI 0.

ENDFORM.                    " AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  COLOR_SEARCH
*&---------------------------------------------------------------------*
FORM COLOR_SEARCH USING    Q_MATNR.
  REFRESH IT_IBSYMBOL. CLEAR IT_IBSYMBOL.

  SELECT C~ATWRT
         D~ATNAM
       INTO TABLE IT_IBSYMBOL
       FROM MARC AS A INNER JOIN IBIN AS B
                        ON A~CUOBJ EQ B~INSTANCE
                      INNER JOIN V_IBIN_SYVAL AS C
                        ON B~IN_RECNO EQ C~IN_RECNO
                      INNER JOIN CABN AS D
                        ON C~ATINN EQ D~ATINN
       WHERE D~ATNAM IN ('COLOREXT', 'COLORINT')
       AND   A~MATNR EQ Q_MATNR.

ENDFORM. " COLOR_SEARCH
*&---------------------------------------------------------------------*
*&      Form  POSITION_ANZEIGEN
*&---------------------------------------------------------------------*
FORM POSITION_ANZEIGEN.
*del IF STB-MATNR IS INITIAL.                       "YHG069035"YHG137469
  IF MATCAT-MATNR IS INITIAL.                               "YHG137469
    MESSAGE S150.                                           "YHG069035
*del  CHECK NOT STB-MATNR IS INITIAL.                         "YHG137469
    CHECK NOT MATCAT-MATNR IS INITIAL.                      "YHG137469
  ENDIF.                                                    "YHG069035

  IF STB-SUMKZ EQ '*'.
    CHECK STB-SUMKZ NE '*'.
  ENDIF.

  CSIN-DATUV = P_DATUV.
  CSIN-DATUB = P_DATUV.
  CSIN-EMENG = 0 .
*d csin-idnrk = stb-idnrk.                                 "note 149116
*del CSIN-MATNR = STB-MATNR.                                  "YHG137469
  CSIN-MATNR = MATCAT-MATNR.                                "YHG137469
  CSIN-STLAL = STB-STLAL.
  CSIN-STLAN = STB-STLAN.
  CSIN-STLKN = STB-STLKN.
*d csin-stlty = typ_mat.                                      "HGA024434
  CSIN-STLTY = STB-STLTY.                                   "HGA024434

*   IF CSIN-STLTY EQ TYP_KND.

*      CSIN-VBELN = PM_VBELN.

*      CSIN-VBPOS = PM_VBPOS.

*   ENDIF.

*
*   IF CSIN-STLTY EQ TYP_PRJ.

*      CSIN-pspnr = PM_pspnr.

*   ENDIF.


  CSIN-TRTYP = 'A'.
*d csin-werks = pm_werks.
  CSIN-WERKS = MATCAT-PRWRK.                                "HGA027225
  CSIN-CMODE = '01'.
  CSIN-STUEZ = STB-STPOZ.                                   "YHG061577

  CALL DIALOG 'CS_BOM_DISPLAY'
       EXPORTING
            CSIN.

  PERFORM CLR_HIDE_AREA.
ENDFORM.                    " POSITION_ANZEIGEN
*&---------------------------------------------------------------------*
*&      Form  CLR_HIDE_AREA
*&---------------------------------------------------------------------*
FORM CLR_HIDE_AREA.
  CLEAR:
     STB-MNGLG,
     STB-IDNRK,
     STB-WERKS,                                             "HGA027225
*      CLO_MATNR,

     STB-DOKAR,                                             "YHG041147
     STB-DOKNR,                                             "YHG041147
     STB-DOKVR,                                             "YHG041147
     STB-DOKTL,                                             "YHG041147
     STB-CLASS,                                             "YHG000109
     STB-KLART,                                             "YHG000109
     STB-OBJTY,                                             "YHG079407
     STB-SUMKZ,
     STB-STPOZ,                                             "YHG063011
*del  STB-MATNR,                                              "YHG137469
     MATCAT-MATNR,                                          "YHG137469
     MATCAT-PRWRK,                                          "HGA027225
     STB-VPSTA,
     STB-STLAL,
     STB-STLAN,
     STB-STLKN,
     STB-STLTY,                                             "HGA024434
     STB-STLNR.
ENDFORM.                    " CLR_HIDE_AREA
*&---------------------------------------------------------------------*
*&      Form  ALV_STB_INDEX_MODIFY
*&---------------------------------------------------------------------*
FORM ALV_STB_INDEX_MODIFY.
  LOOP AT ALV_STB.
    ALV_STB-INDEX = SY-TABIX.
    MODIFY ALV_STB INDEX SY-TABIX TRANSPORTING INDEX.

  ENDLOOP.
ENDFORM.                    " ALV_STB_INDEX_MODIFY
*&---------------------------------------------------------------------*
*&      Form  DELETE_MULTI_LEVEL
*&---------------------------------------------------------------------*
FORM DELETE_MULTI_LEVEL.
  DATA L_TABIX TYPE SY-TABIX.
  DATA LT_STB LIKE ALV_STB OCCURS 0 WITH HEADER LINE.
  LOOP AT ALV_STB.
    L_TABIX = SY-TABIX.
*   2004.08.19 CHANGE
*    IF ALV_STB-EITM EQ 'M'.
    IF ALV_STB-DUMPS NE 'x'.
      ALV_STB-DGLVL = '.1'.
      MODIFY ALV_STB INDEX L_TABIX TRANSPORTING DGLVL.
    ELSE.
      DELETE ALV_STB INDEX L_TABIX.
    ENDIF.
    CLEAR ALV_STB.
  ENDLOOP.
*   2004.08.19 APPENDING
  SORT ALV_STB BY IDNRK.
  CLEAR ALV_STB.
  LT_STB[] = ALV_STB[].
  CLEAR ALV_STB. REFRESH ALV_STB.
  CLEAR L_TABIX. " MNGKO
  LOOP AT LT_STB.
    READ TABLE ALV_STB WITH KEY IDNRK = LT_STB-IDNRK
                       BINARY SEARCH .
    IF SY-SUBRC EQ 0.
      L_TABIX = SY-TABIX.
      ALV_STB-MNGKO =  ALV_STB-MNGKO + LT_STB-MNGKO.
      MODIFY ALV_STB INDEX L_TABIX TRANSPORTING MNGKO.
    ELSE.
      MOVE-CORRESPONDING LT_STB TO ALV_STB.
      APPEND ALV_STB.
    ENDIF.
    CLEAR: ALV_STB, LT_STB.

  ENDLOOP.
***********************************
ENDFORM.                    " DELETE_MULTI_LEVEL
*&---------------------------------------------------------------------*
*&      Form  DEPENDENCY_CHECK_FIELD
*&---------------------------------------------------------------------*
FORM DEPENDENCY_CHECK_FIELD CHANGING P_KNNAE
                                     P_KNNAI.
  DATA: L_CUOBJ    LIKE MARC-CUOBJ,
        L_IN_RECNO LIKE IBIN-IN_RECNO.

  DATA: BEGIN OF LT_CABN OCCURS 7,
          ATWRT TYPE V_IBIN_SYVAL-ATWRT,
          ATNAM TYPE CABN-ATNAM,
        END   OF LT_CABN.

  SELECT SINGLE CUOBJ
              FROM MARC
              INTO L_CUOBJ
              WHERE MATNR EQ P_MATNR
              AND   WERKS EQ P_WERKS.

  SELECT SINGLE IN_RECNO
              FROM IBIN
              INTO L_IN_RECNO
              WHERE INSTANCE EQ L_CUOBJ.

  SELECT A~ATWRT
         B~ATNAM
       INTO TABLE LT_CABN
       FROM V_IBIN_SYVAL AS A INNER JOIN CABN AS B
                              ON A~ATINN EQ B~ATINN
       WHERE A~IN_RECNO EQ L_IN_RECNO.

  SORT LT_CABN BY ATNAM.
  P_KNNAE = P_ATWRE.
  P_KNNAI = P_ATWRI.
  LOOP AT LT_CABN.
    CASE LT_CABN-ATNAM.
      WHEN  'COLOR_MI'.
        CONCATENATE P_KNNAE  LT_CABN-ATWRT INTO P_KNNAE
                                     SEPARATED BY SPACE.
        CONCATENATE P_KNNAI  LT_CABN-ATWRT INTO P_KNNAI
                                     SEPARATED BY SPACE.

      WHEN  'COLOR_OPT1'.
        IF LT_CABN-ATWRT EQ '-'.
          CONCATENATE P_KNNAE '  ' '    ' INTO P_KNNAE
                                       SEPARATED BY SPACE.
          CONCATENATE P_KNNAI '  ' '    ' INTO P_KNNAI
                                       SEPARATED BY SPACE.
        ELSE.
          CONCATENATE P_KNNAE '  ' LT_CABN-ATWRT INTO P_KNNAE
                                       SEPARATED BY SPACE.
          CONCATENATE P_KNNAI '  ' LT_CABN-ATWRT INTO P_KNNAI
                                       SEPARATED BY SPACE.
        ENDIF.
      WHEN  'COLOR_OPT2'.
        IF LT_CABN-ATWRT EQ '-'.
          CONCATENATE P_KNNAE  '    ' INTO P_KNNAE.

          CONCATENATE P_KNNAI  '    ' INTO P_KNNAI.
        ELSE.
          CONCATENATE P_KNNAE  LT_CABN-ATWRT INTO P_KNNAE.

          CONCATENATE P_KNNAI  LT_CABN-ATWRT INTO P_KNNAI.
        ENDIF.
      WHEN  'COLOR_OPT3'.
        IF LT_CABN-ATWRT EQ '-'.
          CONCATENATE P_KNNAE  '    ' INTO P_KNNAE.

          CONCATENATE P_KNNAI  '    ' INTO P_KNNAI.
        ELSE.
          CONCATENATE P_KNNAE  LT_CABN-ATWRT INTO P_KNNAE.

          CONCATENATE P_KNNAI  LT_CABN-ATWRT INTO P_KNNAI.

        ENDIF.
      WHEN  'COLOR_OPT4'.
        IF LT_CABN-ATWRT EQ '-'.
          CONCATENATE P_KNNAE  '    ' INTO P_KNNAE.

          CONCATENATE P_KNNAI  '    ' INTO P_KNNAI.
        ELSE.
          CONCATENATE P_KNNAE  LT_CABN-ATWRT INTO P_KNNAE.

          CONCATENATE P_KNNAI  LT_CABN-ATWRT INTO P_KNNAI.
        ENDIF.

    ENDCASE.

  ENDLOOP.
ENDFORM.                    " DEPENDENCY_CHECK_FIELD
*&---------------------------------------------------------------------*
*&      Form  MULTI_CULUMN
*&---------------------------------------------------------------------*
FORM MULTI_CULUMN.
  DATA: L_TABIX TYPE SY-TABIX,
        L_DGLVL TYPE STPOX_ALV-DGLVL,
        L_MATN1 TYPE MARA-MATNR,
        L_MAKT1 TYPE MAKT-MAKTX,
        L_MATN2 TYPE MARA-MATNR,
        L_MAKT2 TYPE MAKT-MAKTX,
        L_MATN3 TYPE MARA-MATNR,
        L_MAKT3 TYPE MAKT-MAKTX.
  LOOP AT ALV_STB.
    L_TABIX = SY-TABIX.
    MOVE-CORRESPONDING ALV_STB TO IT_MULT.
    CASE ALV_STB-DGLVL.
      WHEN '.1'.
        IT_MULT-MATN1 = L_MATN1 = ALV_STB-IDNRK.
        APPEND IT_MULT.
      WHEN '..2'.
        IT_MULT-MATN1 = L_MATN1.
        IT_MULT-MATN2 = L_MATN2 = ALV_STB-IDNRK.
        APPEND IT_MULT.
      WHEN '...3'.
        IT_MULT-MATN1 = L_MATN1.
        IT_MULT-MATN2 = L_MATN2.
        IT_MULT-MATN3 = L_MATN3 = ALV_STB-IDNRK.
        APPEND IT_MULT.
      WHEN '....4'.
        IT_MULT-MATN1 = L_MATN1.
        IT_MULT-MATN2 = L_MATN2.
        IT_MULT-MATN3 = L_MATN3.
        IT_MULT-MATN4 = ALV_STB-IDNRK.
        APPEND IT_MULT.
    ENDCASE.
    CLEAR: IT_MULT, ALV_STB.
  ENDLOOP.
ENDFORM.                    " MULTI_CULUMN
*&---------------------------------------------------------------------*
*&      Form  LEVEL_ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
FORM LEVEL_ALV_GRID_DISPLAY.
* TOTAL LINES
  DESCRIBE TABLE IT_MULT LINES WA_TOTAL_LINE.
* ALV VARIABLE
  PERFORM ALV_VARIABLE_MULTI_CREATE.
* ALV DISPLAY
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = REPORT_NAME
            I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS_SET'
            I_STRUCTURE_NAME         = 'ZSBM_STPOX_ALV'
            IS_LAYOUT                = ALVLO_STB
            I_SAVE                   = ALVVR_SAV_ALL
            IS_VARIANT               = ALVVR
            IT_EVENTS                = ALV_EVNT_TB_CMPL
            IT_FIELDCAT              = STB_FIELDS_TB
       IMPORTING
            E_EXIT_CAUSED_BY_CALLER  = EXIT_BY_CALLER
            ES_EXIT_CAUSED_BY_USER   = EXIT_BY_USER
       TABLES
            T_OUTTAB                 = IT_MULT
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
  IF SY-SUBRC = 0.
    IF EXIT_BY_CALLER = 'X'.
*     Forced Exit by calling program
*     <do_something>.
    ELSE.
*     User left list via F3, F12 or F15
      IF EXIT_BY_USER-BACK = 'X'.       "F3
*       <do_something>.
      ELSE.
        IF EXIT_BY_USER-EXIT = 'X'.     "F15
*         <do_something>.
        ELSE.
          IF EXIT_BY_USER-CANCEL = 'X'. "F12
*           <do_something>.
          ELSE.
*           should not occur!
*           <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
*   Fatal error callin ALV
*   MESSAGE AXXX(XY) WITH ...
  ENDIF.
ENDFORM.                    " LEVEL_ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIABLE_MULTI_CREATE
*&---------------------------------------------------------------------*
FORM ALV_VARIABLE_MULTI_CREATE.
  ALVVR = SY-REPID.
  ALVVR_SAV_ALL = 'A'.
  REPORT_NAME = SY-REPID.
  ALVLO_STB-INFO_FIELDNAME = 'INFO'.
  PERFORM ALV_EVNT_TB_PREP USING 'A'
                                 ALV_EVNT_TB_CMPL.
  PERFORM STB_FIELDS_TB_PREP_MULTI.
  PERFORM ALV_TOP_TB_PREP USING ALV_TOP_TB.
ENDFORM.                    " ALV_VARIABLE_MULTI_CREATE
*&---------------------------------------------------------------------*
*&      Form  STB_FIELDS_TB_PREP_MULTI
*&---------------------------------------------------------------------*
FORM STB_FIELDS_TB_PREP_MULTI.
  CALL FUNCTION 'GET_FIELDTAB'
       EXPORTING
            LANGU    = SY-LANGU
            TABNAME  = 'ZSBM_STPOX_ALV'
            WITHTEXT = ' '
            ONLY     = 'T'
       TABLES
            FIELDTAB = FTAB
       EXCEPTIONS
            OTHERS   = 1.

  LOOP AT FTAB.
    CLEAR: WA_STB_FIELDS_TB.

    CASE FTAB-FIELDNAME.
      WHEN 'DGLVL'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DGLVL'.
        WA_STB_FIELDS_TB-COL_POS   =  1.
        WA_STB_FIELDS_TB-FIX_COLUMN = 'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  5.
        WA_STB_FIELDS_TB-JUST      = 'L' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.
*
*      WHEN 'POSNR'.
*        WA_STB_FIELDS_TB-FIELDNAME = 'POSNR'.
*        WA_STB_FIELDS_TB-COL_POS   =  2.
*        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
*        WA_STB_FIELDS_TB-OUTPUTLEN = 4 .
*        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MATN1'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MATN1'.
        WA_STB_FIELDS_TB-COL_POS   =  2.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        WA_STB_FIELDS_TB-ICON       =  'X' .
        WA_STB_FIELDS_TB-JUST      = 'L' .
        WA_STB_FIELDS_TB-EMPHASIZE  = 'C100' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MATN2'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MATN2'.
        WA_STB_FIELDS_TB-COL_POS   =  3.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        WA_STB_FIELDS_TB-ICON       =  'X' .
        WA_STB_FIELDS_TB-JUST      = 'L' .
        WA_STB_FIELDS_TB-EMPHASIZE  = 'C110' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MATN3'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MATN3'.
        WA_STB_FIELDS_TB-COL_POS   =  4.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        WA_STB_FIELDS_TB-ICON       =  'X' .
        WA_STB_FIELDS_TB-JUST      = 'L' .
        WA_STB_FIELDS_TB-EMPHASIZE  = 'C300' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MATN4'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MATN4'.
        WA_STB_FIELDS_TB-COL_POS   =  5.
        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        WA_STB_FIELDS_TB-ICON       =  'X' .
        WA_STB_FIELDS_TB-JUST      = 'L' .
        WA_STB_FIELDS_TB-EMPHASIZE  = 'C400' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'OJTXP'.
        WA_STB_FIELDS_TB-FIELDNAME = 'OJTXP'.
        WA_STB_FIELDS_TB-COL_POS   =  6.
*        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  30.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MNGKO'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MNGKO'.
        WA_STB_FIELDS_TB-COL_POS   =  7.
*        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  10.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MEINS'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MEINS'.
        WA_STB_FIELDS_TB-COL_POS   =  8.
*        WA_STB_FIELDS_TB-FIX_COLUMN =  'X' .
        WA_STB_FIELDS_TB-OUTPUTLEN =  3 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'WERKS'.
        WA_STB_FIELDS_TB-FIELDNAME = 'WERKS'.
        WA_STB_FIELDS_TB-COL_POS   =  9.
        WA_STB_FIELDS_TB-OUTPUTLEN = 4 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'AENNR'.
        WA_STB_FIELDS_TB-FIELDNAME = 'AENNR'.
        WA_STB_FIELDS_TB-COL_POS   = 10.
        WA_STB_FIELDS_TB-OUTPUTLEN = 12.
        WA_STB_FIELDS_TB-JUST      = 'R' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'DATUV'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DATUV'.
        WA_STB_FIELDS_TB-COL_POS   = 11.
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        WA_STB_FIELDS_TB-NO_SUM    = 'X'.
        WA_STB_FIELDS_TB-NO_ZERO   = 'X'.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'DATUB'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DATUB'.
        WA_STB_FIELDS_TB-COL_POS   = 12.
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'DUMPS'.
        WA_STB_FIELDS_TB-FIELDNAME = 'DUMPS'.
        WA_STB_FIELDS_TB-COL_POS   = 13.
        WA_STB_FIELDS_TB-OUTPUTLEN = 3 .
        WA_STB_FIELDS_TB-JUST      = 'C' .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'KZKFG'.
        WA_STB_FIELDS_TB-FIELDNAME = 'KZKFG'.
        WA_STB_FIELDS_TB-COL_POS   = 14.
        WA_STB_FIELDS_TB-OUTPUTLEN = 1 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'MTART'.
        WA_STB_FIELDS_TB-FIELDNAME = 'MTART'.
        WA_STB_FIELDS_TB-COL_POS   = 15.
        WA_STB_FIELDS_TB-OUTPUTLEN = 4 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'KNOBJ'.
        WA_STB_FIELDS_TB-FIELDNAME = 'KNOBJ'.
        WA_STB_FIELDS_TB-COL_POS   = 16.
        WA_STB_FIELDS_TB-OUTPUTLEN = 10.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'POTX1'.
        WA_STB_FIELDS_TB-FIELDNAME = 'POTX1'.
        WA_STB_FIELDS_TB-COL_POS   = 17.
        WA_STB_FIELDS_TB-OUTPUTLEN = 18.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN 'EITM'.
        WA_STB_FIELDS_TB-FIELDNAME = 'EITM'.
        WA_STB_FIELDS_TB-COL_POS   = 18.
        WA_STB_FIELDS_TB-OUTPUTLEN = 5 .
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

      WHEN OTHERS.
        WA_STB_FIELDS_TB-FIELDNAME = FTAB-FIELDNAME.
        WA_STB_FIELDS_TB-NO_OUT    = 'X'.
        WA_STB_FIELDS_TB-NO_SUM    = 'X'.
        APPEND WA_STB_FIELDS_TB TO STB_FIELDS_TB.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " STB_FIELDS_TB_PREP_MULTI
