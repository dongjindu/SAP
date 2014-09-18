FUNCTION ZIM_GET_NEXT_DATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(FACTORY_CALENDAR) TYPE  TSCMA-FACTORY_CALENDAR
*"     VALUE(HOLIDAY_CALENDAR) TYPE  TSCMA-HOLIDAY_CALENDAR OPTIONAL
*"     VALUE(NDATE) TYPE  DATUM
*"     VALUE(NDAY) TYPE  I
*"  EXPORTING
*"     VALUE(RDATE) TYPE  DATUM
*"----------------------------------------------------------------------
DATA : FROM_DATE   LIKE    SY-DATUM,
       TO_DATE     LIKE    SY-DATUM,
       TMP_NDAY    TYPE    I,
       TMP_DAY     TYPE    I,
       TMP_CHK_DAY TYPE    I.

DATA: LS_DAY_ATTRIBUTES      LIKE CASDAYATTR OCCURS 0
                                  WITH HEADER LINE.
DATA: IT_DAY_TEMP            LIKE CASDAYATTR OCCURS 0
                                  WITH HEADER LINE.

   FROM_DATE = NDATE.
   TO_DATE   = NDATE + NDAY + NDAY.
   TMP_NDAY  = NDAY + 1.

*       get attributes of day.
   CALL FUNCTION 'DAY_ATTRIBUTES_GET'
        EXPORTING
              FACTORY_CALENDAR           = FACTORY_CALENDAR
              HOLIDAY_CALENDAR           = HOLIDAY_CALENDAR
              DATE_FROM                  = FROM_DATE
              DATE_TO                    = TO_DATE
              LANGUAGE                   = SY-LANGU
        TABLES
              DAY_ATTRIBUTES             = LS_DAY_ATTRIBUTES
        EXCEPTIONS
              FACTORY_CALENDAR_NOT_FOUND = 1
              HOLIDAY_CALENDAR_NOT_FOUND = 2
              DATE_HAS_INVALID_FORMAT    = 3
              DATE_INCONSISTENCY         = 4
              OTHERS                     = 5.
*       send error messages
   IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

   IT_DAY_TEMP[] = LS_DAY_ATTRIBUTES[].

   CLEAR : TMP_DAY, TMP_CHK_DAY.
*   LOOP AT LS_DAY_ATTRIBUTES.
*      W_TABIX = SY-TABIX.
**      IF ls_day_attributes-FREEDAY EQ 'X'.
**         CONTINUE.
**      ENDIF.
*      ADD 1 TO TMP_DAY.
*      IF TMP_DAY LE TMP_NDAY.
*         IF LS_DAY_ATTRIBUTES-FREEDAY EQ 'X'.
*            IF LS_DAY_ATTRIBUTES-WEEKDAY EQ '6'.
*               W_TABIX = W_TABIX - 1.
*               DO.
*                  IF W_TABIX LE 0.
*                     CLEAR : RDATE.   EXIT.
*                  ENDIF.
*                  READ TABLE IT_DAY_TEMP INDEX W_TABIX.
*                  IF SY-SUBRC EQ 0.
*                     IF IT_DAY_TEMP-FREEDAY EQ 'X'.
*                        W_TABIX = W_TABIX - 1.
*                     ELSE.
*                        RDATE = IT_DAY_TEMP-DATE.
*                        EXIT.
*                     ENDIF.
*                  ENDIF.
*               ENDDO.
*               EXIT.
*            ENDIF.
*            CONTINUE.
*         ENDIF.
*         RDATE = LS_DAY_ATTRIBUTES-DATE.
*         EXIT.
*      ENDIF.
*   ENDLOOP.

*>> NHJ MODIFY
   LOOP AT LS_DAY_ATTRIBUTES.
      W_TABIX = SY-TABIX.
      ADD 1 TO TMP_DAY.
      IF LS_DAY_ATTRIBUTES-FREEDAY EQ 'X'.
         CONTINUE.
      ELSE.
         RDATE  =  LS_DAY_ATTRIBUTES-DATE.
         IF TMP_CHK_DAY EQ  NDAY.  EXIT.  ENDIF.
         TMP_CHK_DAY = TMP_CHK_DAY + 1.
      ENDIF.
   ENDLOOP.

ENDFUNCTION.
