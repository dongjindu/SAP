FUNCTION Z_FPP_CHANGE_DATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IWERKS) TYPE  T001W-WERKS
*"     VALUE(IDATE) TYPE  SY-DATUM
*"     VALUE(ITIME) TYPE  SY-UZEIT
*"  EXPORTING
*"     VALUE(ODATE) TYPE  SY-DATUM
*"  EXCEPTIONS
*"      FACTORY_CALENDAR_NOT_FOUND
*"      HOLIDAY_CALENDAR_NOT_FOUND
*"      DATE_HAS_INVALID_FORMAT
*"      DATE_INCONSISTENCY
*"      ERROR_TIME
*"----------------------------------------------------------------------
  TABLES T001W .
  DATA : L_LINES       TYPE SY-INDEX ,
         L_DATE        TYPE SY-DATUM .
  DATA : IT_HOLIDAYS   LIKE TABLE OF ISCAL_DAY WITH HEADER LINE .

  IF IWERKS IS INITIAL.
    IWERKS = 'P001' .
  ENDIF.
  CLEAR T001W .

  SELECT SINGLE *
             FROM T001W
             WHERE WERKS EQ IWERKS .
** changed by Furong on 05/05/2006
*  IF ITIME GE '060000' AND ITIME LE '240000' .
  IF ITIME GE '051500' AND ITIME LE '240000' .
** end of change
    L_DATE  =  IDATE .
    DO .
      CLEAR : IT_HOLIDAYS, IT_HOLIDAYS[].
      CALL FUNCTION 'HOLIDAY_GET'
       EXPORTING
*     HOLIDAY_CALENDAR                 = ' '
         FACTORY_CALENDAR                 = T001W-FABKL
         DATE_FROM                        = L_DATE
         DATE_TO                          = L_DATE
*   IMPORTING
*     YEAR_OF_VALID_FROM               =
*     YEAR_OF_VALID_TO                 =
*     RETURNCODE                       =
        TABLES
          HOLIDAYS                         = IT_HOLIDAYS
        EXCEPTIONS
          FACTORY_CALENDAR_NOT_FOUND       = 1
          HOLIDAY_CALENDAR_NOT_FOUND       = 2
          DATE_HAS_INVALID_FORMAT          = 3
          DATE_INCONSISTENCY               = 4
          OTHERS                           = 5  .
      IF SY-SUBRC EQ 0.
        DESCRIBE TABLE IT_HOLIDAYS LINES  L_LINES.
        IF L_LINES IS INITIAL .
          EXIT .
        ELSE.
          L_DATE = L_DATE - 1 .
        ENDIF.
      ELSE .
        CASE SY-SUBRC .
          WHEN 1 .
            RAISE FACTORY_CALENDAR_NOT_FOUND  .
          WHEN 2 .
            RAISE HOLIDAY_CALENDAR_NOT_FOUND  .
          WHEN 3 .
            RAISE DATE_HAS_INVALID_FORMAT     .
          WHEN 4 .
            RAISE DATE_INCONSISTENCY          .
          WHEN 5 .
            RAISE OTHERS                      .
        ENDCASE .
      ENDIF.
    ENDDO .
** changed by Furong on 05/05/2006
*  ELSEIF ITIME GE '000000' AND ITIME LT '060000' .
  ELSEIF ITIME GE '000000' AND ITIME LT '051500' .
** end of change
    L_DATE = IDATE - 1 .
    DO .
      CLEAR : IT_HOLIDAYS, IT_HOLIDAYS[].
      CALL FUNCTION 'HOLIDAY_GET'
       EXPORTING
*     HOLIDAY_CALENDAR                 = ' '
         FACTORY_CALENDAR                 = T001W-FABKL
         DATE_FROM                        = L_DATE
         DATE_TO                          = L_DATE
*   IMPORTING
*     YEAR_OF_VALID_FROM               =
*     YEAR_OF_VALID_TO                 =
*     RETURNCODE                       =
        TABLES
          HOLIDAYS                         = IT_HOLIDAYS
        EXCEPTIONS
          FACTORY_CALENDAR_NOT_FOUND       = 1
          HOLIDAY_CALENDAR_NOT_FOUND       = 2
          DATE_HAS_INVALID_FORMAT          = 3
          DATE_INCONSISTENCY               = 4
          OTHERS                           = 5  .
      IF SY-SUBRC EQ 0.
        DESCRIBE TABLE IT_HOLIDAYS LINES  L_LINES.
        IF L_LINES IS INITIAL .
          EXIT .
        ELSE.
          L_DATE = L_DATE - 1 .
        ENDIF.
      ELSE .
        CASE SY-SUBRC .
          WHEN 1 .
            RAISE FACTORY_CALENDAR_NOT_FOUND  .
          WHEN 2 .
            RAISE HOLIDAY_CALENDAR_NOT_FOUND  .
          WHEN 3 .
            RAISE DATE_HAS_INVALID_FORMAT     .
          WHEN 4 .
            RAISE DATE_INCONSISTENCY          .
          WHEN 5 .
            RAISE OTHERS                      .
        ENDCASE .
      ENDIF.
    ENDDO .
  ELSE.
    RAISE ERROR_TIME .
  ENDIF.

  ODATE = L_DATE .

ENDFUNCTION.
