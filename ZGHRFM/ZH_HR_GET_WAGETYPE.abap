FUNCTION ZH_HR_GET_WAGETYPE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRACD) TYPE  NUMC08 OPTIONAL
*"     VALUE(IV_BRAAV) TYPE  CHAR20 OPTIONAL
*"     VALUE(IV_BUKRS) TYPE  BUKRS OPTIONAL
*"     VALUE(IV_BEGDA) TYPE  BEGDA OPTIONAL
*"     VALUE(IV_ENDDA) TYPE  ENDDA OPTIONAL
*"  TABLES
*"      ET_LIST STRUCTURE  ZGHRSS0018 OPTIONAL
*"----------------------------------------------------------------------

*1. ## + SEQNR# Key# PYXX_READ_PAYROLL_RESULT F/M# ## ###### ##### #####
*     ## # ### ## t500l## ####ID# ## ### ## # (## ZH_HR_IF_PY_MASTER ## ##)
*
* 2. ### ##### ## ### # #### ### ### ##### Filtering## ##### ### ##
*
*   1) /101 (####), /552(#####), /560 (###)# ### #### ####, # ## W/Type(LGART)#
*       Key# T512W-AKLAS# 39~40 ## ### (##### 20#)# #### ## #### ## ###
*
*  (# WT ####  F/S## ### ##)
*
*      # T512W## ##### #####,  ###,  ### ### ## ### ####
*
*   2) ### ### ##### #(CHAR 2) # T52DB## EVCLS=#20## EVCVT (##### ###)# ## ##
*
*3) Amount(##)# ##,           - ##### ($, & # ##)#  #### WT
*                                              - /101, /552, /560 WT
*                                              - ###### # /*  or  #### #### WT
*
*     # ## #### Unique## # ##### ## #,  ####, ##, #####,  WT##, WT###, WT
*        #### ###/### ## ##### ## ### ### ####  #### Export

  DATA: LT_HRPY_RGDIR   LIKE TABLE OF HRPY_RGDIR WITH HEADER LINE,
          LT_TEMP         LIKE TABLE OF HRPY_RGDIR WITH HEADER LINE,
          LT_T512W        TYPE TABLE OF T512W WITH HEADER LINE,
          LT_T512T        TYPE TABLE OF T512T WITH HEADER LINE,
          LT_T52DB        TYPE TABLE OF T52DB WITH HEADER LINE,

          LS_PA0001       TYPE PA0001,
          LS_PA0000       TYPE PA0000,
          LS_T500L        TYPE T500L,
          LS_RT           LIKE LINE OF GS_PAY_RESULT-INTER-RT,

          L_DATUM         TYPE DATUM,
          L_ERROR.

  SELECT * FROM HRPY_RGDIR AS R
    INTO CORRESPONDING FIELDS OF TABLE LT_HRPY_RGDIR
    WHERE R~FPEND = R~IPEND
      AND FPEND => IV_BEGDA
      AND FPEND =< IV_ENDDA.

  IF SY-SUBRC NE 0.
    RETURN.
  ENDIF.

  LOOP AT LT_HRPY_RGDIR.

    L_DATUM = LT_HRPY_RGDIR-FPEND.

*    check ## # ####.
    CLEAR: LS_PA0001,LS_PA0000.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = L_DATUM
      IMPORTING
        LAST_DAY_OF_MONTH = L_DATUM
      EXCEPTIONS
        DAY_IN_NO_DATE    = 1
        OTHERS            = 2.

    SELECT SINGLE * FROM PA0001
    INTO LS_PA0001
    WHERE PERNR   = LT_HRPY_RGDIR-PERNR
      AND BEGDA  <= L_DATUM
      AND ENDDA  => L_DATUM
      AND BUKRS = IV_BUKRS.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM PA0000
    INTO LS_PA0000
    WHERE PERNR   = LT_HRPY_RGDIR-PERNR
      AND BEGDA  <= L_DATUM
      AND ENDDA  => L_DATUM
      AND STAT2 <> '0'.

    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING LT_HRPY_RGDIR TO LT_TEMP.

    APPEND LT_TEMP.

    CLEAR: LT_HRPY_RGDIR.
  ENDLOOP.

  SELECT SINGLE * INTO LS_T500L FROM T500L WHERE MOLGA = G_MOLGA.

  SELECT * FROM T512W
     INTO TABLE LT_T512W
     WHERE MOLGA = G_MOLGA
       AND ENDDA = '99991231'
       AND BEGDA <= SY-DATUM
       AND AKLAS <> ''.

  DELETE LT_T512W WHERE AKLAS+38(2) IS INITIAL.

  SELECT * FROM T52DB
    INTO TABLE LT_T52DB
    WHERE SPRSL = SY-LANGU
      AND MOLGA = G_MOLGA
      AND EVCLS = '20'.

  SORT LT_T52DB BY EVCLV.

  SELECT * FROM T512T
    INTO TABLE LT_T512T
    WHERE SPRSL = 'EN'
      AND MOLGA = G_MOLGA.

  SORT LT_T512T BY LGART.

  LOOP AT LT_TEMP.

    CLEAR : GS_PAY_RESULT.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        CLUSTERID                    = LS_T500L-RELID
        EMPLOYEENUMBER               = LT_TEMP-PERNR
        SEQUENCENUMBER               = LT_TEMP-SEQNR
        CHECK_READ_AUTHORITY         = SPACE
        READ_ONLY_INTERNATIONAL      = 'X'
      CHANGING
        PAYROLL_RESULT               = GS_PAY_RESULT
      EXCEPTIONS
        ILLEGAL_ISOCODE_OR_CLUSTERID = 1
        ERROR_GENERATING_IMPORT      = 2
        IMPORT_MISMATCH_ERROR        = 3
        SUBPOOL_DIR_FULL             = 4
        NO_READ_AUTHORITY            = 5
        NO_RECORD_FOUND              = 6
        VERSIONS_DO_NOT_MATCH        = 7
        OTHERS                       = 8.

**********************************************************************
* ### ## ##.
**********************************************************************
    LOOP AT GS_PAY_RESULT-INTER-RT INTO LS_RT
       WHERE ( LGART EQ '/101' OR LGART EQ '/560'
          OR LGART EQ '/552' ) AND BETRG > 0.

      READ TABLE ET_LIST WITH KEY LGART = LS_RT-LGART.
      IF SY-SUBRC EQ 0.
        CONTINUE.
      ENDIF.

      CLEAR: ET_LIST.
      MOVE-CORRESPONDING LS_RT TO ET_LIST.
      ET_LIST-ZCBRACD  = IV_BRACD.

      SELECT SINGLE BEGDA ENDDA FROM T512W
        INTO (ET_LIST-BEGDA, ET_LIST-ENDDA)
        WHERE MOLGA = G_MOLGA
          AND LGART = ET_LIST-LGART
          AND ENDDA = '99991231'
          AND BEGDA <= SY-DATUM.

      CASE ET_LIST-LGART.
        WHEN '/101'.
          ET_LIST-EVCLV_B = '$T'.
          ET_LIST-EVCVT = 'Gross Payment'.
        WHEN '/560'.
          ET_LIST-EVCLV_B = '$N'.
          ET_LIST-EVCVT = 'Net Payment'.
      ENDCASE.

      READ TABLE LT_T512T WITH KEY LGART = ET_LIST-LGART BINARY SEARCH.

      ET_LIST-LGTXT     = LT_T512T-LGTXT.
      ET_LIST-MOLGA     = G_MOLGA.
      ET_LIST-ZCBRAAV   = IV_BRAAV.
      ET_LIST-BUKRS     = IV_BUKRS.

      APPEND ET_LIST.
      CLEAR: ET_LIST,LS_RT,LT_T512T.
    ENDLOOP.

**********************************************************************
*&      ## ### 20## ### ### ### #### ### ##.    &*
**********************************************************************

    LOOP AT LT_T512W WHERE LGART NE '/101' AND LGART NE  '/552'
               AND LGART NE '/560'.

      READ TABLE LT_T52DB WITH KEY EVCLV = LT_T512W-AKLAS+38(2) BINARY SEARCH.
      IF SY-SUBRC NE 0 .
        CONTINUE.
      ENDIF.

      LOOP AT GS_PAY_RESULT-INTER-RT INTO LS_RT
          WHERE LGART = LT_T512W-LGART
            AND BETRG > 0.

        READ TABLE ET_LIST WITH KEY LGART = LS_RT-LGART.
        IF SY-SUBRC EQ 0.
          CONTINUE.
        ENDIF.

        MOVE-CORRESPONDING LS_RT TO ET_LIST.

        ET_LIST-ZCBRACD = IV_BRACD.
        ET_LIST-EVCLV_B = LT_T512W-AKLAS+38(2).
        ET_LIST-EVCVT   = LT_T52DB-EVCVT.
        ET_LIST-BEGDA   = LT_T512W-BEGDA.
        ET_LIST-ENDDA   = LT_T512W-ENDDA.

        READ TABLE LT_T512T WITH KEY LGART = ET_LIST-LGART BINARY SEARCH.

        ET_LIST-LGTXT     = LT_T512T-LGTXT.
        ET_LIST-MOLGA     = G_MOLGA.
        ET_LIST-ZCBRAAV   = IV_BRAAV.
        ET_LIST-BUKRS     = IV_BUKRS.

        APPEND ET_LIST.
        CLEAR: ET_LIST,LS_RT,LT_T512T.
      ENDLOOP.
      CLEAR: LT_T512W,LT_T52DB.
    ENDLOOP.

**********************************************************************
*&      ## ### W/T  ##.    &*
**********************************************************************
    LOOP AT GS_PAY_RESULT-INTER-RT INTO LS_RT
          WHERE LGART NE '/101' AND LGART NE  '/552'
               AND LGART NE '/560' AND BETRG > 0.

      READ TABLE ET_LIST WITH KEY LGART = LS_RT-LGART.
      IF SY-SUBRC EQ 0.
        CONTINUE.
      ENDIF.

      CLEAR: L_ERROR.

      CALL FUNCTION 'CHECK_FIELDTYPE'
        EXPORTING
          L_FIELD_VALUE = LS_RT-LGART(1)
          L_CLASS       = 'A'
        CHANGING
          L_ERROR_FLAG  = L_ERROR.

      IF  LS_RT-LGART(1) EQ '/' OR L_ERROR NE 'Y'.
        CONTINUE.
      ENDIF.

      CLEAR: ET_LIST.
      MOVE-CORRESPONDING LS_RT TO ET_LIST.
      ET_LIST-ZCBRACD  = IV_BRACD.

      SELECT SINGLE BEGDA ENDDA FROM T512W
        INTO (ET_LIST-BEGDA, ET_LIST-ENDDA)
        WHERE MOLGA = G_MOLGA
          AND LGART = ET_LIST-LGART
          AND ENDDA = '99991231'
          AND BEGDA <= SY-DATUM.

      READ TABLE LT_T512T WITH KEY LGART = ET_LIST-LGART BINARY SEARCH.

      ET_LIST-LGTXT     = LT_T512T-LGTXT.
      ET_LIST-MOLGA     = G_MOLGA.
      ET_LIST-ZCBRAAV   = IV_BRAAV.
      ET_LIST-BUKRS     = IV_BUKRS.

      APPEND ET_LIST.
      CLEAR: ET_LIST,LS_RT,LT_T512T.
    ENDLOOP.

    CLEAR: LT_TEMP.
  ENDLOOP.



ENDFUNCTION.
