*----------------------------------------------------------------------*
*   INCLUDE MZAHR0002F01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P_ENTRY_PERSONAL_AREA
*&---------------------------------------------------------------------*
FORM P_ENTRY_PERSONAL_AREA.
  CLEAR IT_VALUE. REFRESH IT_VALUE.
*
  CLEAR T500P.
  SELECT PERSA NAME1 INTO (T500P-PERSA, T500P-NAME1)
    FROM T500P WHERE MOLGA = '10'.
    IT_VALUE-PERSA = T500P-PERSA.
    IT_VALUE-NAME1 = T500P-NAME1.
    APPEND IT_VALUE. CLEAR IT_VALUE.
  ENDSELECT.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME   = 'T500P'.
  IT_FIELD-FIELDNAME = 'PERSA'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.

  IT_FIELD-TABNAME   = 'T500P'.
  IT_FIELD-FIELDNAME = 'NAME1'.
  IT_FIELD-SELECTFLAG = ' '.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_VALUE.
ENDFORM.                    " P_ENTRY_PERSONAL_AREA
*&---------------------------------------------------------------------*
*&      Form  GET_PERSONAL_AREA_NAME
*&---------------------------------------------------------------------*
FORM GET_PERSONAL_AREA_NAME.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
*
  READ TABLE IT_VALUE INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME = 'IT_S9000-NAME1'.
  DYNPFIELDS-STEPL     = W_INDEX.
  DYNPFIELDS-FIELDVALUE = IT_VALUE-NAME1.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
  IT_S9000-ZPERA = IT_VALUE-PERSA.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME                     = SY-CPROG
            DYNUMB                     = SY-DYNNR
       TABLES
            DYNPFIELDS                 = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA       = 1
            INVALID_DYNPROFIELD        = 2
            INVALID_DYNPRONAME         = 3
            INVALID_DYNPRONUMMER       = 4
            INVALID_REQUEST            = 5
            NO_FIELDDESCRIPTION        = 6
            UNDEFIND_ERROR             = 7
            OTHERS                     = 8.
ENDFORM.                    " GET_PERSONAL_AREA_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_PERSONNEL_COUNT
*&---------------------------------------------------------------------*
FORM GET_PERSONNEL_COUNT.
  DATA: L_BEGDA LIKE SY-DATUM,
        L_YEARS TYPE I,
        L_ENTRY LIKE HIDA OCCURS 1 WITH HEADER LINE.
*
  CLEAR TP_HCP01. REFRESH TP_HCP01.
*
  SELECT PERNR WERKS PERSG PERSK KOSTL STELL
    INTO (PA0001-PERNR, PA0001-WERKS, PA0001-PERSG,
          PA0001-PERSK, PA0001-KOSTL, PA0001-STELL)
    FROM PA0001 WHERE ENDDA = '99991231'.
    IF PA0001-PERSG <> '2'.
*     TP_HCP01-ZVERS = W_ZVERS.
      TP_HCP01-ZYEAR = SY-DATUM+(4) + 1.
      TP_HCP01-ZPERA = PA0001-WERKS.
      TP_HCP01-ZPERG = PA0001-PERSG.
      TP_HCP01-ZSUBG = PA0001-PERSK.
      TP_HCP01-ZCOST = PA0001-KOSTL.
      TP_HCP01-ZJOBK = PA0001-STELL.
      TP_HCP01-ZCURC = 1.
      TP_HCP01-ZHEDC = 1.
*.. get entry date
      CLEAR: L_ENTRY, L_ENTRY[].
      CALL FUNCTION 'HR_ENTRY_DATE'
           EXPORTING PERSNR      = PA0001-PERNR
           IMPORTING ENTRYDATE   = L_BEGDA
           TABLES    ENTRY_DATES = L_ENTRY.
*.. get working years
      CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
           EXPORTING BEG_DA     = L_BEGDA
                     END_DA     = SY-DATUM
           IMPORTING NO_YEAR    = L_YEARS.
      IF L_YEARS = 0. L_YEARS = 1. ENDIF.
*
      TP_HCP01-ZSENR = L_YEARS + 1.
      COLLECT TP_HCP01. CLEAR TP_HCP01.
    ENDIF.
  ENDSELECT.
*... get job description
  LOOP AT TP_HCP01.
    CLEAR HRP1000.
    SELECT SINGLE SHORT INTO HRP1000-SHORT
      FROM HRP1000 WHERE PLVAR = '01'
                     AND OTYPE = 'C'
                     AND OBJID = TP_HCP01-ZJOBK
                     AND ISTAT = '1'
                     AND ENDDA = '99991231'
                     AND LANGU = SY-LANGU.
    TP_HCP01-ZJOBK = HRP1000-SHORT.
    MODIFY TP_HCP01. CLEAR TP_HCP01.
  ENDLOOP.
*
  SORT TP_HCP01 BY ZPERA ZCOST ZJOBK ZPERG ZSUBG ZSENR.
ENDFORM.                    " GET_PERSONNEL_COUNT
*&---------------------------------------------------------------------*
*&      Form  GET_MONTHLY_COUNT
*&---------------------------------------------------------------------*
FORM GET_MONTHLY_COUNT.
  DATA: L_COUNT(2)    TYPE N.
  CLEAR IT_HCP01. REFRESH IT_HCP01.
*
  L_COUNT = 1.
*
  DO 12 TIMES.
    LOOP AT TP_HCP01.
      MOVE-CORRESPONDING TP_HCP01 TO IT_HCP01.
      IT_HCP01-ZVERS = W_ZVERS.
      IT_HCP01-ZMONS = L_COUNT.
      APPEND IT_HCP01. CLEAR IT_HCP01.
    ENDLOOP.

    L_COUNT = L_COUNT + 1.
  ENDDO.
*
  SORT IT_HCP01 BY ZYEAR ZMONS.
ENDFORM.                    " GET_MONTHLY_COUNT
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_PERSONAL_COUNT
*&---------------------------------------------------------------------*
FORM CALCULATE_PERSONAL_COUNT.
  DATA: L_HEADC   LIKE ZTHR_HCP01-ZHEDC.
*
  LOOP AT IT_S9000 WHERE ZYEAR NE SPACE.
    CLEAR T500P.
    SELECT SINGLE PERSA INTO T500P-PERSA
      FROM T500P WHERE MOLGA = '10'
                   AND NAME1 = IT_S9000-NAME1.
    IT_S9000-ZPERA = T500P-PERSA.
    MODIFY IT_S9000. CLEAR IT_S9000.
  ENDLOOP.
*
  LOOP AT IT_S9000 WHERE ZYEAR NE SPACE.
    READ TABLE IT_HCP01 WITH KEY ZYEAR = IT_S9000-ZYEAR
                                 ZMONS = IT_S9000-ZMONS
                                 ZPERA = IT_S9000-ZPERA
                                 ZCOST = IT_S9000-ZCOST
                                 ZJOBK = IT_S9000-ZJOBK
                                 ZPERG = IT_S9000-ZPERG
                                 ZSUBG = IT_S9000-ZSUBG
                                 ZSENR = IT_S9000-ZSENR.
    IF SY-SUBRC = 0.
      IT_HCP01-ZPLAN = IT_S9000-ZPLAN.
      IT_HCP01-ZHEDC = IT_S9000-ZHEDC.
      MODIFY IT_HCP01 TRANSPORTING ZPLAN ZHEDC
                             WHERE ZYEAR = IT_S9000-ZYEAR
                               AND ZMONS = IT_S9000-ZMONS
                               AND ZPERA = IT_S9000-ZPERA
                               AND ZCOST = IT_S9000-ZCOST
                               AND ZJOBK = IT_S9000-ZJOBK
                               AND ZPERG = IT_S9000-ZPERG
                               AND ZSUBG = IT_S9000-ZSUBG
                               AND ZSENR = IT_S9000-ZSENR.
    ELSE.
      IT_HCP01-ZVERS = W_ZVERS.
      IT_HCP01-ZYEAR = SY-DATUM+(4) + 1.
      IT_HCP01-ZMONS = IT_S9000-ZMONS.
      IT_HCP01-ZPERA = IT_S9000-ZPERA.
      IT_HCP01-ZCOST = IT_S9000-ZCOST.
      IT_HCP01-ZJOBK = IT_S9000-ZJOBK.
      IT_HCP01-ZPERG = IT_S9000-ZPERG.
      IT_HCP01-ZSUBG = IT_S9000-ZSUBG.
      IT_HCP01-ZSENR = IT_S9000-ZSENR.
      IT_HCP01-ZCURC = IT_S9000-ZCURC.
      IT_HCP01-ZPLAN = IT_S9000-ZPLAN.
      IT_HCP01-ZHEDC = IT_S9000-ZHEDC.
      APPEND IT_HCP01. CLEAR IT_HCP01.
    ENDIF.
  ENDLOOP.
*
  MODIFY ZTHR_HCP01 FROM TABLE IT_HCP01.
  IF SY-SUBRC = 0.
    MESSAGE S001 WITH 'DATA SAVED'.
  ENDIF.
ENDFORM.                    " CALCULATE_PERSONAL_COUNT
*&---------------------------------------------------------------------*
*&      Form  P_ENTRY_JOB
*&---------------------------------------------------------------------*
FORM P_ENTRY_JOB.
  CLEAR IT_JOBVL. REFRESH IT_JOBVL.
*
  CLEAR HRP1000.
  SELECT OBJID SHORT STEXT
    INTO (HRP1000-OBJID, HRP1000-SHORT, HRP1000-STEXT)
    FROM HRP1000 WHERE PLVAR = '01'
                   AND OTYPE = 'C'
                   AND ISTAT = '1'
                   AND ENDDA = '99991231'
                   AND LANGU = SY-LANGU.
    IT_JOBVL-OBJID = HRP1000-OBJID.
    IT_JOBVL-SHORT = HRP1000-SHORT.
    IT_JOBVL-STEXT = HRP1000-STEXT.
    APPEND IT_JOBVL. CLEAR IT_JOBVL.
  ENDSELECT.
*
  CLEAR IT_FIELD. REFRESH IT_FIELD.
*
  IT_FIELD-TABNAME   = 'HRP1000'.
  IT_FIELD-FIELDNAME = 'OBJID'.
  IT_FIELD-SELECTFLAG = 'X'.
  APPEND IT_FIELD. CLEAR IT_FIELD.

  IT_FIELD-TABNAME   = 'HRP1000'.
  IT_FIELD-FIELDNAME = 'SHORT'.
  IT_FIELD-SELECTFLAG = ' '.
  APPEND IT_FIELD. CLEAR IT_FIELD.

  IT_FIELD-TABNAME   = 'HRP1000'.
  IT_FIELD-FIELDNAME = 'STEXT'.
  IT_FIELD-SELECTFLAG = ' '.
  APPEND IT_FIELD. CLEAR IT_FIELD.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD      = W_FNAME
       IMPORTING
            IND              = W_TABIX
            SELECT_VALUE     = W_FLDVL
       TABLES
            FIELDS           = IT_FIELD
            FULL_TABLE       = IT_JOBVL.
ENDFORM.                    " P_ENTRY_JOB
*&---------------------------------------------------------------------*
*&      Form  GET_JOB_NAME
*&---------------------------------------------------------------------*
FORM GET_JOB_NAME.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
*
  READ TABLE IT_JOBVL INDEX W_TABIX.
  DYNPFIELDS-FIELDNAME = 'IT_S9000-ZJOBK'.
  DYNPFIELDS-STEPL     = W_INDEX.
  DYNPFIELDS-FIELDVALUE = IT_JOBVL-SHORT.
  APPEND DYNPFIELDS. CLEAR DYNPFIELDS.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME                     = SY-CPROG
            DYNUMB                     = SY-DYNNR
       TABLES
            DYNPFIELDS                 = DYNPFIELDS
       EXCEPTIONS
            INVALID_ABAPWORKAREA       = 1
            INVALID_DYNPROFIELD        = 2
            INVALID_DYNPRONAME         = 3
            INVALID_DYNPRONUMMER       = 4
            INVALID_REQUEST            = 5
            NO_FIELDDESCRIPTION        = 6
            UNDEFIND_ERROR             = 7
            OTHERS                     = 8.
ENDFORM.                    " GET_JOB_NAME
