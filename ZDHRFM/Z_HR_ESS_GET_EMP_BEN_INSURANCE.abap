FUNCTION Z_HR_ESS_GET_EMP_BEN_INSURANCE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_BENEFIT_DATA STRUCTURE  ZESS_EMP_BENEFIT_DATA_INSURANC
*"      ZESS_EMP_BEN_DATA_INSUR_ITEM STRUCTURE
*"        ZESS_EMP_BEN_DATA_INSUR_ITEM
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 12/12/2012 VALERIAN   UD1K955925  Exclude Lock Data from retrieval
*-----------------------------------------------------------------------

  DATA : ERROR_TABLE LIKE RPBENERR OCCURS 0,
         EEPER LIKE Q0168-EEPER,
         SUBRC LIKE SY-SUBRC,
         EE_BENEFIT_DATA LIKE RPBENEEDAT,
         H74FA LIKE T74FA,
         H74FB LIKE T74FB,
         H74FC LIKE T74FC,
         BEN_SALARY LIKE P0168-SALOV.

  DATA : DTYXX TYPE BEN_DEPTYP,
         DIDXX TYPE BEN_DEPID,
         CTYXX TYPE BEN_DEPTYP,
         CIDXX TYPE BEN_DEPID,
         BPTXX TYPE BEN_BPRCNT,
         CPTXX TYPE BEN_BPRCNT.

  DATA : BEGIN OF I_MEMORY OCCURS 0,
          PERNR LIKE PA0000-PERNR,
          SEQNO(2) TYPE N,
          SUBTY LIKE PA0021-SUBTY,
          VORNA LIKE PA0002-VORNA, "
          PERID LIKE PA0002-PERID, "
          GBDAT LIKE PA0002-GBDAT, "
          OBJPS LIKE PA0021-OBJPS,
          MIDNM LIKE PA0002-MIDNM,
          NACHN LIKE PA0002-NACHN, "
          BEGDA LIKE PA0168-BEGDA, "
          ENDDA LIKE PA0168-ENDDA, "
          DTYXX LIKE PA0168-DTY01,
          TRDAT LIKE PA0168-ENDDA,
          GENDR(10),
          RELCODE(10),             "
         END OF I_MEMORY.

  __CLS : ZESS_EMP_BENEFIT_DATA, ZESS_EMP_BEN_DATA_INSUR_ITEM,P0168.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR         = EMPLOYEE_NUMBER
            INFTY         = '0168'
            BEGDA         = SY-DATUM
            ENDDA         = '99991231'
            BYPASS_BUFFER = 'X'
       TABLES
            INFTY_TAB     = P0168.

  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Employee Number'.
    APPEND RETURN.
    EXIT.
  ENDIF.

* LOOP AT P0168.                                           "UD1K955925
  LOOP AT P0168 WHERE sprps IS INITIAL.                    "UD1K955925

    CLEAR ZESS_EMP_BENEFIT_DATA.
    SELECT SINGLE LTEXT INTO ZESS_EMP_BENEFIT_DATA-BEBEFIT_PLAN
    FROM T5UCA WHERE BAREA = P0168-BAREA
                 AND BPLAN = P0168-BPLAN
                 AND LANGU = SY-LANGU.

    ZESS_EMP_BENEFIT_DATA-START_DATE = P0168-BEGDA.
    ZESS_EMP_BENEFIT_DATA-END_DATE = P0168-ENDDA.

    SELECT SINGLE LTEXT INTO ZESS_EMP_BENEFIT_DATA-OPTION
    FROM T74GA WHERE LANGU EQ SY-LANGU
                 AND BAREA EQ P0168-BAREA
                 AND BPLAN EQ P0168-BPLAN
                 AND BCOVR EQ P0168-BCOVR.

    CALL FUNCTION 'HR_BEN_READ_EE_BENEFIT_DATA'
         EXPORTING
              PERNR           = P0168-PERNR
              DATUM           = P0168-BEGDA
              REACTION        = NO_MSG
         IMPORTING
              EE_BENEFIT_DATA = EE_BENEFIT_DATA
              SUBRC           = SUBRC
         TABLES
              ERROR_TABLE     = ERROR_TABLE.

    SELECT SINGLE * FROM T74FA WHERE BAREA EQ P0168-BAREA
                                 AND BPLAN EQ P0168-BPLAN
                                 AND BCOVR EQ P0168-BCOVR
                                 AND ENDDA EQ '99991231'.

    PERFORM RE74FB(SAPFBEN0) TABLES ERROR_TABLE
                              USING P0168-BAREA
                                    P0168-BPLAN
                                    T74FA-BCOVE
                                    NO_MSG
                           CHANGING H74FB
                                    SUBRC.

    CALL FUNCTION 'HR_BEN_GET_RULE_COVERAGE'
         EXPORTING
              EE_BENEFIT_DATA  = EE_BENEFIT_DATA
              COVERAGE_FORMULA = H74FB
              DATUM            = SY-DATUM
              DESIRED_CURRE    = 'USD'
              REACTION         = NO_MSG
         IMPORTING
              COVERAGE_RULE    = H74FC
              SUBRC            = SUBRC
         TABLES
              ERROR_TABLE      = ERROR_TABLE.

    MOVE H74FC-ADDUN TO Q0168-ADDUN.
*
* get benefit salary
    IF NOT H74FC-COVFA IS INITIAL OR
       NOT H74FC-MINFA IS INITIAL OR
       NOT H74FC-MAXFA IS INITIAL.
      CALL FUNCTION 'HR_BEN_GET_PLAN_SALARY'
           EXPORTING
                EE_BEN_DATA    = EE_BENEFIT_DATA
                BPLAN          = P0168-BPLAN
                CUTOFF_MONTH   = H74FB-BSALM
                CUTOFF_DAY     = H74FB-BSALD
                CURRENCY       = 'USD'
                DATUM          = SY-DATUM
                REACTION       = NO_MSG
           IMPORTING
                BENEFIT_SALARY = Q0168-BENSL
                SUBRC          = SUBRC
           TABLES
                ERROR_TABLE    = ERROR_TABLE.
    ELSE.
      CLEAR Q0168-BENSL.
    ENDIF.

*
* check to see if the benefits salary was overriden
    IF P0168-SALOV IS INITIAL.
      MOVE Q0168-BENSL TO BEN_SALARY.
    ELSE.
      IF P0168-CURRE <> Q0168-CURR1.
        CALL FUNCTION 'HR_BEN_CONVERT_CURRENCY'
             EXPORTING
                  OLD_AMOUNT  = P0168-SALOV
                  OLD_CURRE   = P0168-CURRE
                  NEW_CURRE   = 'USD'
                  DATUM       = SY-DATUM
                  REACTION    = NO_MSG
             IMPORTING
                  NEW_AMOUNT  = BEN_SALARY
                  SUBRC       = SUBRC
             TABLES
                  ERROR_TABLE = ERROR_TABLE.
      ELSE.
        MOVE P0168-SALOV TO BEN_SALARY.
      ENDIF.
    ENDIF.
*
* get base coverage
    CALL FUNCTION 'HR_BEN_EVAL_AMOUNT'
         EXPORTING
              FLAT_AMOUNT      = H74FC-COVAM
              FCTR_AMOUNT      = BEN_SALARY
              FACTOR           = H74FC-COVFA
              ROUNDING_RULE    = H74FC-COVRL
              ROUNDING_DIVISOR = H74FC-COVRN
              REACTION         = NO_MSG
         IMPORTING
              EVALUATED_AMOUNT = Q0168-BSCOV
              SUBRC            = SUBRC
         TABLES
              ERROR_TABLE      = ERROR_TABLE.
*
* get total coverage amount
    CALL FUNCTION 'HR_BEN_CALC_COVERAGE_AMOUNT'
         EXPORTING
              COVERAGE_RULE    = H74FC
              BENEFIT_SALARY   = BEN_SALARY
              ADDITIONAL_UNITS = P0168-ADDNO
              OUT_CURRE        = 'USD'
              DATUM            = SY-DATUM
              REACTION         = NO_MSG
         IMPORTING
              COVERAGE_AMOUNT  = Q0168-COVAM
              SUBRC            = SUBRC
         TABLES
              ERROR_TABLE      = ERROR_TABLE.
*
* check to see if the coverage amount was overriden
    IF ( P0168-COVOV IS INITIAL ).
      MOVE Q0168-COVAM TO ZESS_EMP_BENEFIT_DATA-COVER_AMOUNT.
    ELSE.
      IF P0168-CURRE <> Q0168-CURR1.
        CALL FUNCTION 'HR_BEN_CONVERT_CURRENCY'
             EXPORTING
                  OLD_AMOUNT  = P0168-COVOV
                  OLD_CURRE   = P0168-CURRE
                  NEW_CURRE   = 'USD'
                  DATUM       = SY-DATUM
                  REACTION    = NO_MSG
             IMPORTING
                  NEW_AMOUNT  = ZESS_EMP_BENEFIT_DATA-COVER_AMOUNT
                  SUBRC       = SUBRC
             TABLES
                  ERROR_TABLE = ERROR_TABLE.
      ELSE.
        MOVE P0168-COVOV TO ZESS_EMP_BENEFIT_DATA-COVER_AMOUNT.
      ENDIF.
    ENDIF.
*

    PERFORM RE74FA(SAPFBEN0) TABLES ERROR_TABLE
                              USING P0168-BAREA
                                    P0168-BPLAN
                                    P0168-BCOVR
                                    P0168-BEGDA
                                    NO_MSG
                           CHANGING H74FA
                                    SUBRC.

*    CALL FUNCTION 'HR_BEN_GET_PLAN_PERIOD'
*         EXPORTING
*              PERNR         = P0168-PERNR
*              BAREA         = P0168-BAREA
*              BPLAN         = P0168-BPLAN
*              VALIDITY_DATE = SY-DATUM
*              REACTION      = 'N'
*         IMPORTING
*              PLAN_PERIOD   = EEPER
*              SUBRC         = SUBRC
*         TABLES
*              ERROR_TABLE   = ERROR_TABLE.

EEPER = '02'.

    CALL FUNCTION 'HR_BEN_GET_PLAN_COST'
         EXPORTING
              EE_BENEFIT_DATA = EE_BENEFIT_DATA
              BPLAN           = P0168-BPLAN
              BCOST           = H74FA-BCOST
              DATUM           = SY-DATUM
              COVER           = ZESS_EMP_BENEFIT_DATA-COVER_AMOUNT
              OUT_PERIOD      = EEPER
              OUT_CURRE       = 'USD'
              REACTION        = NO_MSG
         IMPORTING
              EECST           = Q0168-EECST
              ERCST           = Q0168-ERCST
              ACCST           = Q0168-ACCST
              FLXCR           = Q0168-FLXCR
              SUBRC           = SUBRC
         TABLES
              ERROR_TABLE     = ERROR_TABLE.

    ZESS_EMP_BENEFIT_DATA-EECOST = Q0168-EECST.
    ZESS_EMP_BENEFIT_DATA-ERCOST = Q0168-ERCST.
    ZESS_EMP_BENEFIT_DATA-BPLAN  = P0168-BPLAN.

    CASE  EEPER.
      WHEN '01'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Monthly'.
      WHEN '02'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Semi-monthly'.
      WHEN '03'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Weekly'.
      WHEN '04'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Bi-weekly'.
      WHEN '05'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Every four weeks'.
      WHEN '06'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Annually'.
      WHEN '07'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Quarterly'.
      WHEN '08'.
        ZESS_EMP_BENEFIT_DATA-EEPER_TXT = 'Semi-annually'.
    ENDCASE.

    ZESS_EMP_BENEFIT_DATA-PRETX = P0168-PRETX.
    APPEND ZESS_EMP_BENEFIT_DATA.

    DO 20 TIMES VARYING DTYXX FROM P0168-DTY01
                              NEXT P0168-DTY02
                VARYING DIDXX FROM P0168-DID01
                              NEXT P0168-DID02
                VARYING CTYXX FROM P0168-CTY01
                              NEXT P0168-CTY02
                VARYING CIDXX FROM P0168-CID01
                              NEXT P0168-CID02
                VARYING BPTXX FROM P0168-BPT01
                              NEXT P0168-BPT02
                VARYING CPTXX FROM P0168-CPT01
                              NEXT P0168-CPT02.

      IF NOT DTYXX  EQ '0000' AND NOT DTYXX  EQ SPACE.
        SELECT SINGLE * FROM PA0021
         WHERE PERNR EQ P0168-PERNR
           AND SUBTY EQ DTYXX
           AND OBJPS EQ DIDXX
           AND BEGDA <= SY-DATUM
           AND ENDDA >= SY-DATUM.

        IF SY-SUBRC EQ 0.
          CONCATENATE PA0021-FAVOR PA0021-FANAM PA0021-FNMZU INTO
         ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_NAME SEPARATED BY SPACE.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_VAL_BEGDA = P0168-BEGDA.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_VAL_ENDDA = P0168-ENDDA.

          CASE DTYXX.
            WHEN 'H'.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Subscriber'.
            WHEN '1'.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Spouse'.
            WHEN '2' OR '6' OR '9' OR '13'.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Child'.
            WHEN OTHERS.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Others'.
          ENDCASE.


          ZESS_EMP_BEN_DATA_INSUR_ITEM-BPLAN =
                        ZESS_EMP_BENEFIT_DATA-BPLAN.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_CONTINGENT  = FALSE.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_PERCENTAGE = BPTXX.
          APPEND ZESS_EMP_BEN_DATA_INSUR_ITEM.

        ENDIF.

      ENDIF.

      IF NOT CTYXX  EQ '0000' AND NOT CTYXX  EQ SPACE.

        SELECT SINGLE * FROM PA0021
         WHERE PERNR EQ P0168-PERNR
           AND SUBTY EQ CTYXX
           AND OBJPS EQ CIDXX
           AND BEGDA <= SY-DATUM
           AND ENDDA >= SY-DATUM.

        IF SY-SUBRC EQ 0.
          CONCATENATE PA0021-FAVOR PA0021-FANAM PA0021-FNMZU INTO
         ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_NAME SEPARATED BY SPACE.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_VAL_BEGDA = PA0021-BEGDA.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_VAL_ENDDA = PA0021-ENDDA.

          CASE CTYXX.
            WHEN 'H'.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Subscriber'.
            WHEN '1'.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Spouse'.
            WHEN '2' OR '6' OR '9' OR '13'.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Child'.
            WHEN OTHERS.
              ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_RELATIONS = 'Others'.
          ENDCASE.


          ZESS_EMP_BEN_DATA_INSUR_ITEM-BPLAN =
                        ZESS_EMP_BENEFIT_DATA-BPLAN.

          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_CONTINGENT  = TRUE.
          ZESS_EMP_BEN_DATA_INSUR_ITEM-BEN_PERCENTAGE = CPTXX.
          APPEND ZESS_EMP_BEN_DATA_INSUR_ITEM.

        ENDIF.

      ENDIF.

    ENDDO.

  ENDLOOP.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

ENDFUNCTION.
