*XCF 15.02.2002 note 495439 CATS notebook
FUNCTION ZCATS_EXTERNAL_WORKLIST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(PROFILE) LIKE  TCATS-VARIANT
*"             VALUE(PERNR_IMP) LIKE  CATSFIELDS-PERNR
*"             VALUE(INPUTDATE_IMP) LIKE  CATSFIELDS-INPUTDATE
*"         OPTIONAL
*"       EXPORTING
*"             VALUE(DATE_FROM) LIKE  BAPIHRITBASE-FROM_DATE
*"             VALUE(DATE_TO) LIKE  BAPIHRITBASE-TO_DATE
*"             VALUE(MEINH) LIKE  CATSFIELDS-MEINH
*"       TABLES
*"              ICATSW_EXP STRUCTURE  CATSW
*"              MSG_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"       EXCEPTIONS
*"              PROFILE_NOT_FOUND
*"----------------------------------------------------------------------
* W o r k l i s t
*should be always called from external                             "LUX
  DATA: IF_EXT_CALL VALUE YX.                               "LUX
  DATA: I_MESSAGES LIKE MESG OCCURS 0 WITH HEADER LINE.
  DATA: BEGDATE  LIKE CATSFIELDS-DATELEFT.                  "YIK
  DATA: BEGWEEK  LIKE CATSFIELDS-CATSWEEK.                  "YIK

  CATSFIELDS-PERNR = PERNR_IMP.
  REFRESH ICATSW.                                           "YIK
* activate message handler
  PERFORM ACTIVATE_MESSAGES TABLES I_MESSAGES.              "lux

* Get unit for hour
  PERFORM GET_UNIT_HOUR USING YX.

* move unit_of_hour to meinh
  CATSFIELDS-MEINH = UNIT_OF_HOUR.
  MEINH            = UNIT_OF_HOUR.                          "YIK
* get profile
  SELECT SINGLE * FROM TCATS WHERE VARIANT = PROFILE.
  IF SY-SUBRC NE 0.
    MESSAGE E161 WITH PROFILE RAISING PROFILE_NOT_FOUND.
  ENDIF.

* get startdate for CATS-data in worklist
  CATSFIELDS-WORKSINCE = SY-DATLO - TCATS-WORKSINCE.

* get inputdate
  IF INPUTDATE_IMP IS INITIAL.
    PERFORM GET_INPUTDATE USING CATSFIELDS-INPUTDATE.
  ELSE.
    CATSFIELDS-INPUTDATE = INPUTDATE_IMP.
  ENDIF.
* get ddic info

  PERFORM GET_DDIC_INFORMATION USING YX.
* set dates
  BEGDATE = CATSFIELDS-INPUTDATE.
  PERFORM SET_WEEK USING BEGDATE BEGWEEK.
  PERFORM GET_BOUNDARIES USING BEGDATE DATE_FROM DATE_TO BEGWEEK
                                                DAYS_ON_SCREEN.
  CATSFIELDS-CATSWEEK = BEGWEEK.                            "YIK
  CATSFIELDS-DATEFROM = DATE_FROM.                          "YIK
  CATSFIELDS-DATETO   = DATE_TO.                            "YIK

  PERFORM FILL_DAYFROM_AND_DAYTO USING CATSFIELDS-DATELEFT
                                       CATSFIELDS-DATERIGHT
                                       YX YX.

* use distribution key with equal distribution
  PERFORM EQUAL_DISTRIBUTION_KEY_SET.

* Create PERTAB for distribution
  CALL FUNCTION 'CATS_PERTAB_FROM_PERIODS_FILL'
       EXPORTING
            DATEFROM     = CATSFIELDS-DATEFROM
            DATETO       = CATSFIELDS-DATETO
            I_CALENDAR   = TCATS-CALENDAR
            I_BEGEND     = TCATS-BEGEND
            I_PERNR      = CATSFIELDS-PERNR
            I_WORKDAY_W  = WORKDAY_W
            I_EXT_CALL   = IF_EXT_CALL
       TABLES
            BLOCKED_DAYS = BLOCKED_DAYS.

* IF NOT TCATS-WORKLIST IS INITIAL AND TCATS-MULTPER IS INITIAL."del xcf
*                                                            note 495439
* Read capacity splits
    IF NOT TCATS-ASSIGNED IS INITIAL.
      PERFORM READ_SPLIT USING IF_EXT_CALL.                 "LUX
    ENDIF.

* Read activities via workcenter
    IF NOT TCATS-WORKCENTRE IS INITIAL.
      PERFORM READ_ACTIVITIES USING IF_EXT_CALL.            "lux
    ENDIF.

* Read confirmation pool
    IF NOT TCATS-FROMPOOL IS INITIAL AND
       NOT CATSFIELDS-POOL IS INITIAL.
      PERFORM READ_POOL USING IF_EXT_CALL.                  "lux
    ENDIF.

* Read CATS-data for worklist
    IF NOT TCATS-WORKING IS INITIAL.
      PERFORM READ_CATS_DATA_WORK USING IF_EXT_CALL.        "lux
    ENDIF.

* Read myObjects for worklist (if PI functionality exists)
    perform read_myobjects using if_ext_call.

* Read worklist from customer exit
    IF NOT TCATS-FROMEXIT IS INITIAL.
      PERFORM READ_WORKLIST_FROM_EXIT.
    ENDIF.

* convert intern to extern for wbs
    LOOP AT ICATSW WHERE NOT RPROJ IS INITIAL.

      PERFORM CONVERT_PROJ_TO_POSID USING ICATSW-RPROJ
                                          ICATSW-POSID.
      MODIFY ICATSW.
    ENDLOOP.
* add customer fields
    LOOP AT ICATSW.
      CALL CUSTOMER-FUNCTION '010'     "new 4.6C YIK
       EXPORTING
            TCATS_IMP     = TCATS
            CATSW_IMP     = ICATSW
            DISPTEXTW1_IMP = ICATSW-DISPTEXTW1
            DISPTEXTW2_IMP = ICATSW-DISPTEXTW2
       IMPORTING
            DISPTEXTW1_EXP = ICATSW-DISPTEXTW1
            DISPTEXTW2_EXP = ICATSW-DISPTEXTW2
       EXCEPTIONS
            OTHERS  = 1.
      MODIFY ICATSW.
    ENDLOOP.

*  ENDIF.                                        "del xcf note 495439

  ICATSW_EXP[] = ICATSW[].

  CALL FUNCTION 'MESSAGES_GIVE'                             "lux
       TABLES                                               "lux
           T_MESG = I_MESSAGES                              "lux
       EXCEPTIONS                                           "lux
           OTHERS = 1.                                      "lux
* only messages from cats should be collected
  LOOP AT I_MESSAGES WHERE ARBGB = 'LR'.                    "YIK
    PERFORM INSERT_MSG_2_BAPIRETRUN USING I_MESSAGES        "lux
                                    CHANGING MSG_RETURN.    "lux
    APPEND MSG_RETURN.                                      "lux
  ENDLOOP.                                                  "lux

ENDFUNCTION.
*eject
