*----------------------------------------------------------------------*
***INCLUDE MZVEMAILF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAVE_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_EMAIL.
   if ZSQM_VEND_EMAIL-EMAIL1 is initial.
      message s026 with 'Please enter email address'.
      exit.
   endif.
    select single *
      FROM ZTQM_VEND_EMAIL
    WHERE LIFNR = ZSQM_VEND_EMAIL-LIFNR.
    IF SY-SUBRC = 0.
       UPDATE ZTQM_VEND_EMAIL SET: EMAIL1 = ZSQM_VEND_EMAIL-EMAIL1
                                   EMAIL2 = ZSQM_VEND_EMAIL-EMAIL2
                                   EMAIL3 = ZSQM_VEND_EMAIL-EMAIL3
                              WHERE  LIFNR = ZSQM_VEND_EMAIL-LIFNR.
    ELSE.
      INSERT INTO ZTQM_VEND_EMAIL VALUES ZSQM_VEND_EMAIL.
    ENDIF.
    IF SY-SUBRC = 0.
       COMMIT WORK.
       message s026 with 'email was updated'.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
ENDFORM.                    " SAVE_EMAIL
*&---------------------------------------------------------------------*
*&      Form  FIND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_EMAIL.
    PERFORM CLEAR_DATA.
    select single email1 email2 email3 into (ZSQM_VEND_EMAIL-email1,
                                     ZSQM_VEND_EMAIL-email2,
                                     ZSQM_VEND_EMAIL-email3)
      FROM ZTQM_VEND_EMAIL
    WHERE LIFNR = ZSQM_VEND_EMAIL-LIFNR.
ENDFORM.                    " FIND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_DATA.
  CLEAR: ZSQM_VEND_EMAIL-email1,
         ZSQM_VEND_EMAIL-email2,
         ZSQM_VEND_EMAIL-email3.
ENDFORM.                    " CLEAR_DATA
