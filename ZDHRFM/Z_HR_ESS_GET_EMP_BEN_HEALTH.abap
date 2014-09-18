FUNCTION Z_HR_ESS_GET_EMP_BEN_HEALTH .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_BENEFIT_DATA STRUCTURE  ZESS_EMP_BENEFIT_DATA_HEALTH
*"      ZESS_EMP_BENEFIT_DATA_DEP STRUCTURE  ZESS_EMP_BENEFIT_DATA_DEP
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 12/12/2012 VALERIAN   UD1K955925  Exclude Lock Data from retrieval
*-----------------------------------------------------------------------

  DATA : ERROR_TABLE LIKE RPBENERR OCCURS 0,
         EEPER LIKE Q0167-EEPER,
         SUBRC LIKE SY-SUBRC,
         EE_BENEFIT_DATA LIKE RPBENEEDAT.

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
          BEGDA LIKE PA0167-BEGDA, "
          ENDDA LIKE PA0167-ENDDA, "
          DTYXX LIKE PA0167-DTY01,
          TRDAT LIKE PA0167-ENDDA,
          GENDR(10),
          RELCODE(10),             "
         END OF I_MEMORY.


  __CLS : ZESS_EMP_BENEFIT_DATA, P0167.
  __CLS ZESS_EMP_BENEFIT_DATA_DEP.

  CALL FUNCTION 'HR_READ_INFOTYPE'
       EXPORTING
            PERNR     = EMPLOYEE_NUMBER
            INFTY     = '0167'
            BEGDA     = SY-DATUM
            ENDDA     = '99991231'
       TABLES
            INFTY_TAB = P0167.

  IF SY-SUBRC NE 0.
    RETURN-TYPE = 'E'.
    RETURN-MESSAGE = 'Invalid Employee Number'.
    APPEND RETURN.
    EXIT.
  ENDIF.

* LOOP AT P0167.                                           "UD1K955925
  LOOP AT P0167 WHERE sprps IS INITIAL.                    "UD1K955925
    CLEAR ZESS_EMP_BENEFIT_DATA.
    SELECT SINGLE LTEXT INTO ZESS_EMP_BENEFIT_DATA-BEBEFIT_PLAN
    FROM T5UCA WHERE BAREA = P0167-BAREA
                 AND BPLAN = P0167-BPLAN
                 AND LANGU = SY-LANGU.

    ZESS_EMP_BENEFIT_DATA-START_DATE = P0167-BEGDA.
    ZESS_EMP_BENEFIT_DATA-END_DATE = P0167-ENDDA.

    SELECT SINGLE LTEXT INTO ZESS_EMP_BENEFIT_DATA-OPTION
    FROM T5UCE WHERE LANGU EQ SY-LANGU
                 AND BAREA EQ P0167-BAREA
                 AND BPLAN EQ P0167-BPLAN
                 AND BOPTI EQ P0167-BOPTI.

    SELECT SINGLE LTEXT INTO ZESS_EMP_BENEFIT_DATA-DEP_COVERAGE
    FROM T5UCF WHERE LANGU EQ SY-LANGU
                 AND BAREA EQ P0167-BAREA
                 AND DEPCV EQ P0167-DEPCV.


    SELECT * FROM T5UBB WHERE BAREA  = P0167-BAREA
                          AND BPLAN  = P0167-BPLAN
                          AND BOPTI  = P0167-BOPTI
                          AND DEPCV  = P0167-DEPCV
                          AND ENDDA >= SY-DATUM
                          AND BEGDA <= SY-DATUM
                          ORDER BY PRIMARY KEY.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC EQ 0.

*      CALL FUNCTION 'HR_BEN_GET_PLAN_PERIOD'
*           EXPORTING
*                PERNR         = P0167-PERNR
*                BAREA         = P0167-BAREA
*                BPLAN         = P0167-BPLAN
*                VALIDITY_DATE = SY-DATUM
*                REACTION      = 'N'
*           IMPORTING
*                PLAN_PERIOD   = EEPER
*                SUBRC         = SUBRC
*           TABLES
*                ERROR_TABLE   = ERROR_TABLE.

EEPER = '02'.

      CALL FUNCTION 'HR_BEN_READ_EE_BENEFIT_DATA'
           EXPORTING
                PERNR           = P0167-PERNR
                DATUM           = P0167-BEGDA
                REACTION        = 'N'
           IMPORTING
                EE_BENEFIT_DATA = EE_BENEFIT_DATA
                SUBRC           = SUBRC
           TABLES
                ERROR_TABLE     = ERROR_TABLE.

      CALL FUNCTION 'HR_BEN_GET_PLAN_COST'
           EXPORTING
                EE_BENEFIT_DATA = EE_BENEFIT_DATA
                BPLAN           = P0167-BPLAN
                BCOST           = T5UBB-BCOST
                DATUM           = SY-DATUM
                OUT_PERIOD      = EEPER
                OUT_CURRE       = 'USD'
                REACTION        = 'N'
           IMPORTING
                EECST           = Q0167-EECST
                ERCST           = Q0167-ERCST
                ACCST           = Q0167-ACCST
                FLXCR           = Q0167-FLXCR
                SUBRC           = SUBRC
           TABLES
                ERROR_TABLE     = ERROR_TABLE.
    ENDIF.

    ZESS_EMP_BENEFIT_DATA-EECOST = Q0167-EECST.
    ZESS_EMP_BENEFIT_DATA-ERCOST = Q0167-ERCST.
    ZESS_EMP_BENEFIT_DATA-BPLAN =  P0167-BPLAN.

    RANGES S_PERNR FOR P0167-PERNR OCCURS 1.
    S_PERNR-SIGN = 'I'.
    S_PERNR-OPTION = 'EQ'.
    S_PERNR-LOW = P0167-PERNR.
    APPEND S_PERNR.

    __CLS I_MEMORY.

    CASE P0167-DEPCV.

      WHEN 'EE+F'.
        SUBMIT ZAHRU011 WITH S_PERNR IN S_PERNR
                        WITH P_PLTYP EQ P0167-PLTYP
                        WITH P_EEF EQ TRUE
                        WITH P_CALL EQ TRUE
                         AND RETURN.
        IMPORT I_MEMORY FROM MEMORY ID 'ESS_BENEFIT_INFO'.
      WHEN 'EE+1'.
        SUBMIT ZAHRU011 WITH S_PERNR IN S_PERNR
                        WITH P_PLTYP EQ P0167-PLTYP
                        WITH P_EE1 EQ TRUE
                        WITH P_CALL EQ TRUE
                         AND RETURN.

        IMPORT I_MEMORY FROM MEMORY ID 'ESS_BENEFIT_INFO'.
      WHEN OTHERS.

    ENDCASE.

*    __CLS ZESS_EMP_BENEFIT_DATA_DEP.

    LOOP AT I_MEMORY.
      CHECK NOT I_MEMORY-SUBTY IS INITIAL.
      MOVE-CORRESPONDING I_MEMORY TO ZESS_EMP_BENEFIT_DATA_DEP.

      CONCATENATE I_MEMORY-VORNA I_MEMORY-NACHN INTO
     ZESS_EMP_BENEFIT_DATA_DEP-BEN_NAME SEPARATED BY SPACE.
      ZESS_EMP_BENEFIT_DATA_DEP-BPLAN =  P0167-BPLAN.
      APPEND ZESS_EMP_BENEFIT_DATA_DEP.
    ENDLOOP.

    ZESS_EMP_BENEFIT_DATA-PRETX = P0167-PRETX.


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

    APPEND ZESS_EMP_BENEFIT_DATA.

  ENDLOOP.

  RETURN-TYPE = 'S'.
  RETURN-MESSAGE = 'Success!'.
  APPEND RETURN.

ENDFUNCTION.
