FUNCTION z_hr_ess_upd_cafe_enroll.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PERNR) TYPE  PERNR_D
*"     VALUE(BEGDA) TYPE  BEGDA OPTIONAL
*"     VALUE(ENDDA) TYPE  ENDDA OPTIONAL
*"     VALUE(ABGRD) TYPE  ABGRD OPTIONAL
*"     VALUE(UNAME) TYPE  AENAM OPTIONAL
*"     VALUE(TEXT1) TYPE  TEXT100 OPTIONAL
*"     VALUE(EVENT) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------


  DATA : lv_i             TYPE i
       , lv_date          TYPE dats
       , lv_ddat(11)      TYPE c
       , lv_pernr(8)      TYPE n
       .
  DATA: is_9100           TYPE p9100.
  DATA: lt_9100           LIKE TABLE OF p9100 WITH HEADER LINE.
  DATA: ls_return         LIKE bapireturn1,
        lv_key            LIKE bapipakey.
  DATA: s_p9100           LIKE p9100,
        change            LIKE pspar-actio VALUE 'MOD'.
  DATA: w_begda           LIKE p0008-begda,
        w_endda           LIKE p0008-endda.



  CLEAR: is_cafe.
  is_cafe-pernr = lv_pernr = pernr.
  is_cafe-begda = begda.
  is_cafe-endda = endda.
  is_cafe-abgrd = abgrd.
  is_cafe-uname = uname.
  is_cafe-text1 = text1.
  is_cafe-event = event.

* Check Employee
  CLEAR: pa9100, return, return[].

* Event check
  IF is_cafe-event NE 'R' AND is_cafe-event NE 'X'.
    return-type = 'E'.
    return-message = 'Invalid Event'.
    APPEND return.
    EXIT.
  ENDIF.

*
  IF is_cafe-event = 'X'.
**    CLEAR: is_cafe-begda, is_cafe-endda.
  ELSE.
    CLEAR: is_cafe-abgrd.
  ENDIF.

* Check BEGDA
  IF is_cafe-event = 'R'.  "Enroll
    lv_i = is_cafe-begda.
    IF lv_i = 0.
      return-type = 'E'.
      return-message = 'Invalid Begin Date'.
      APPEND return.
      EXIT.
    ENDIF.
* Check ENDDA
    lv_i = is_cafe-endda.
    IF lv_i = 0.
      return-type = 'E'.
      return-message = 'Invalid End Date'.
      APPEND return.
      EXIT.
    ENDIF.
  ENDIF.

* Check ABGRD
  IF is_cafe-event = 'X'.
    lv_i = is_cafe-abgrd.
    IF lv_i = 0.
      return-type = 'E'.
      return-message = 'Invalid ABGRD Date'.
      APPEND return.
      EXIT.
    ENDIF.
    lv_date = sy-datum + 2.
    IF is_cafe-abgrd < lv_date.
      return-type = 'E'.
      return-message = 'Invalid ABGRD Date'.
      APPEND return.
      EXIT.
    ENDIF.
    is_cafe-abgrd = is_cafe-abgrd - 1.
  ENDIF.

* Uname
  is_cafe-uname = 'RFCESS'.

* Event = 'R' (meaning to enroll)
  IF event = 'R'.
    SELECT SINGLE * FROM pa9100
     WHERE pernr = is_cafe-pernr
       AND ( begda <= sy-datum AND endda >= sy-datum ).
    IF sy-subrc = 0.
      return-type = 'E'.
      return-message = 'The employee is Already Enrolled.'.
      APPEND return.
      EXIT.
    ELSE.
      CLEAR *pa9100.
       *pa9100-pernr = is_cafe-pernr.
       *pa9100-begda = is_cafe-begda.
       *pa9100-endda = is_cafe-endda.
       *pa9100-aedtm = sy-datum.
       *pa9100-uname = 'RFCESS'.
      CLEAR: is_cafe-text1.
      WRITE sy-datum USING EDIT MASK '__/__/____' TO lv_ddat.
      CONCATENATE 'Waiver signed online on' lv_ddat '- enrolled.'
             INTO is_cafe-text1 SEPARATED BY space.
       *pa9100-text1 = is_cafe-text1.
       *pa9100-eligi = 'E'.

      "This code is requred and locks the record ready for modification
      CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = lv_pernr.

      CLEAR is_9100.
      is_9100-pernr = lv_pernr.
      is_9100-endda = is_cafe-endda. "'99991231'. "
      is_9100-begda = is_cafe-begda. "sy-datum.   "
      is_9100-text1 = is_cafe-text1.
      is_9100-uname = is_cafe-uname.
      is_9100-seqnr = '000'.
      is_9100-aedtm = *pa9100-aedtm.
      is_9100-eligi = 'E'.

      "plus populate any other fields you need to update
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty            = '9100'
          number           = is_9100-pernr
          subtype          = is_9100-subty
          objectid         = is_9100-objps
*         LOCKINDICATOR    =
          validityend      = is_9100-endda
          validitybegin    = is_9100-begda
*         RECORDNUMBER     =
          record           = is_9100
          operation        = 'INS'
          tclas            = 'A'
*         DIALOG_MODE      = '0'
          nocommit         = 'X'
*         VIEW_IDENTIFIER  =
*         SECONDARY_RECORD =
        IMPORTING
          return           = ls_return
          key              = lv_key.

      IF ls_return IS NOT INITIAL.
        return-type = 'E'.
        CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
        CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
        return-message = ls_return-message.
        APPEND return.
      ELSE.
        COMMIT WORK.
        return-type = 'S'.
        CONCATENATE 'Success :' lv_key INTO return-message
                    SEPARATED BY space.
*        return-message = 'Success!'.
        APPEND return.
      ENDIF.
      "unlock record after modification
      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr.

    ENDIF.
  ENDIF.



* Event = 'X' (meaning to exit)
  IF is_cafe-event = 'X'.
    SELECT SINGLE * FROM pa9100
      INTO CORRESPONDING FIELDS OF s_p9100
     WHERE pernr = is_cafe-pernr
       AND ( begda <= sy-datum AND endda >= sy-datum ).
    IF sy-subrc <> 0.
      return-type = 'E'.
      return-message = 'The employee Status is Enrolled.'.
      APPEND return.
      EXIT.
    ELSE.

      s_p9100-infty = '9100'.
      w_begda = s_p9100-begda.
      w_endda = s_p9100-endda.
      MOVE-CORRESPONDING s_p9100 TO is_9100.
      is_9100-pernr = is_cafe-pernr.
      is_9100-endda = is_cafe-endda. "'99991231'.
      s_p9100-endda = is_cafe-abgrd. " - 1.
      s_p9100-aedtm = sy-datum.
      s_p9100-uname = 'RFCESS'.
      CLEAR: is_cafe-text1.
      WRITE sy-datum USING EDIT MASK '__/__/____' TO lv_ddat.
      CONCATENATE lv_ddat '.' into lv_ddat.
      condense lv_ddat.
      CONCATENATE is_9100-text1 'Submitted termination online on'
                  lv_ddat INTO is_cafe-text1 SEPARATED BY space.
      s_p9100-text1 = is_cafe-text1.


      "This code is requred and locks the record ready for modification
      CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
        EXPORTING
          number = s_p9100-pernr.

      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = '9100'
          number        = s_p9100-pernr
          subtype       = s_p9100-subty
          objectid      = s_p9100-objps
          lockindicator = s_p9100-sprps
          validityend   = W_ENDDA
          validitybegin = W_BEGDA
          recordnumber  = s_p9100-seqnr
          record        = s_p9100
          operation     = change
          nocommit      = ' '
        IMPORTING
          return        = ls_return.

      IF ls_return IS NOT INITIAL.
        return-type = 'E'.
        CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
        CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
        return-message = ls_return-message.
        APPEND return.
      ELSE.
        COMMIT WORK.

        return-type = 'S'.
        CONCATENATE 'Success :' lv_key INTO return-message
                    SEPARATED BY space.
*        return-message = 'Success!'.
        APPEND return.
      ENDIF.

      "unlock record after modification
      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = lv_pernr.

    ENDIF.
  ENDIF.

ENDFUNCTION.
