FUNCTION zfhr_badge_master .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(ZFLAG) LIKE  BAPIRET2-TYPE
*"     VALUE(ZMSG) TYPE  BAPIRET2-MESSAGE_V1
*"  TABLES
*"      T_BADGE STRUCTURE  ZTHR_BADGE OPTIONAL
*"----------------------------------------------------------------------
  DATA: BEGIN OF lt_hmma OCCURS 0,
          empno TYPE zthr_badge-empno,
        END OF lt_hmma.

  DATA: it_badge LIKE zthr_badge OCCURS 0 WITH HEADER LINE.

  SELECT pernr INTO TABLE lt_hmma
      FROM pa0000
     WHERE endda = '99991231'.

  SORT lt_hmma BY empno.

  SORT lt_hmma BY empno.
    LOOP AT lt_hmma.
      SHIFT lt_hmma LEFT DELETING LEADING '0'.
      MODIFY lt_hmma INDEX sy-tabix.
    ENDLOOP.
*
  IF t_badge[] IS INITIAL.
    zflag = 'E'.
    zmsg = 'No data'.
    EXIT.
  ENDIF.

  LOOP AT t_badge.
    it_badge = t_badge.
    SHIFT t_badge-EMPNO LEFT DELETING LEADING '0'.
    READ TABLE lt_hmma WITH KEY empno = t_badge-EMPNO
                                   BINARY SEARCH.
    IF sy-subrc = 0.
      IF it_badge-descr CS 'PERMANENT' OR
         it_badge-descr CS 'HMMA INTERN'.
        it_badge-hmma = 'X'.
      ENDIF.
    ENDIF.
    it_badge-ERdat = sy-datum.
    it_badge-ERZET = sy-uzeit.

    APPEND it_badge.
    CLEAR it_badge.
  ENDLOOP.

  DELETE FROM zthr_badge WHERE badge <> ' '.
  INSERT zthr_badge FROM TABLE it_badge.
  IF sy-subrc = 0.
    zflag = 'S'.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      IMPORTING
        message_text_output = zmsg.

    zflag = 'E'.
  ENDIF.

ENDFUNCTION.
