FUNCTION ZCATS_CHECK_FACTORY_DATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(TMP_PERNR) TYPE  PERNR_D
*"             VALUE(TMP_DATE) LIKE  SY-DATUM
*"             VALUE(TMP_EXTERNAL) TYPE  C
*"       CHANGING
*"             VALUE(TMP_IND) TYPE  CIND
*"----------------------------------------------------------------------

* Profile with calendar check ?
* CHECK TCATS-CALENDAR EQ YX.
* Get plant out of infotype 0315
  CALL FUNCTION 'CATS_GET_INFOTYPE_0315'
       EXPORTING
            PERNR           = TMP_PERNR
            DATE            = TMP_DATE
       IMPORTING
            I0315           = CATS_0315
       EXCEPTIONS
            PERNR_NOT_FOUND = 1
            NO_RECORD       = 2
            NO_AUTHORITY    = 3
            OTHERS          = 4.
* Plant given ?
  IF NOT CATS_0315-WERKS IS INITIAL.
* Read customizing table for plants
    IF T001W-WERKS NE CATS_0315-WERKS.
      SELECT SINGLE * FROM T001W
             WHERE WERKS = CATS_0315-WERKS.
    ENDIF.
* Check calendar for working days
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
         EXPORTING
              DATE                         = TMP_DATE
              FACTORY_CALENDAR_ID          = T001W-FABKL
         IMPORTING
              WORKINGDAY_INDICATOR         = TMP_IND
         EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 1
              CORRECT_OPTION_INVALID       = 2
              DATE_AFTER_RANGE             = 3
              DATE_BEFORE_RANGE            = 4
              DATE_INVALID                 = 5
              FACTORY_CALENDAR_NOT_FOUND   = 6
              OTHERS                       = 7.
  ELSE.
    IF PLANT_MSG IS INITIAL.
      IF TMP_EXTERNAL = YX.
        PERFORM ADD_MESSAGE USING ARBGB 'I' '075'
                            SPACE SPACE SPACE SPACE.
      ELSE.
        MESSAGE I075.
      ENDIF.
      PLANT_MSG = YX.
    ENDIF.
  ENDIF.




ENDFUNCTION.
