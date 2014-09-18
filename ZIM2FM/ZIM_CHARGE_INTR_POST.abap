FUNCTION ZIM_CHARGE_INTR_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFPNNO) LIKE  ZTPMTHD-ZFPNNO
*"     VALUE(BLART) LIKE  ZTPMTHD-BLART DEFAULT 'RE'
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      POST_ERROR
*"----------------------------------------------------------------------

  SELECT SINGLE *
           FROM ZTPMTHD
          WHERE ZFPNNO = ZFPNNO.
  IF ZTPMTHD-ZFLCKN EQ '2'.
    SELECT SINGLE *
             FROM ZTIMIMG11
            WHERE BUKRS = ZTPMTHD-BUKRS.
    IF NOT ZTIMIMG11-ZFIOCAC31 IS INITIAL.
*>> 2003.10.29: NSH Inserted..
      PERFORM P3000_INTEREST_BDC_POST.
      IF W_ERR_CHK EQ SPACE.
        MESSAGE S260(M8) WITH INVOICEDOCNUMBER.
        MOVE : SY-MSGTY   TO     RETURN-TYPE,
               SY-MSGID   TO     RETURN-ID,
               SY-MSGNO   TO     RETURN-NUMBER,
               SY-MSGV1   TO     RETURN-MESSAGE_V1,
               SY-MSGV2   TO     RETURN-MESSAGE_V2,
               SY-MSGV3   TO     RETURN-MESSAGE_V3,
               SY-MSGV4   TO     RETURN-MESSAGE_V4.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = RETURN-ID
                  MSGNR               = RETURN-NUMBER
                  MSGV1               = RETURN-MESSAGE_V1
                  MSGV2               = RETURN-MESSAGE_V2
                  MSGV3               = RETURN-MESSAGE_V3
                  MSGV4               = RETURN-MESSAGE_V4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
        APPEND  RETURN.

        MOVE  :  INVOICEDOCNUMBER   TO  ZTPMTHD-BELNR_TR,
                 FISCALYEAR         TO  ZTPMTHD-GJAHR_TR.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZIM_PM_DOC_MODIFY'
       EXPORTING
            ZFPNNO         = ZTPMTHD-ZFPNNO
            ZFSTATUS       = 'U'
            W_ZTPMTHD_OLD  = *ZTPMTHD
            W_ZTPMTHD      = ZTPMTHD
            W_OK_CODE      = 'SAVE'
       TABLES
            IT_ZSPMTIV_OLD = IT_ZSPMTIV
            IT_ZSPMTIV     = IT_ZSPMTIV
       EXCEPTIONS
            OTHERS         = 4.

  IF SY-SUBRC EQ 0.
*>> 2003.10.30 NSH Commented..
*>> 이자전표 생성 이력관리 안하도록 주석처리.
*    CLEAR : ZTPMTHST.
*    MOVE-CORRESPONDING ZTPMTHD  TO   ZTPMTHST.
*    MOVE :  SY-MANDT                   TO  ZTPMTHST-MANDT,
*            SY-UNAME                   TO  ZTPMTHST-ERNAM,
*            SY-DATUM                   TO  ZTPMTHST-CDAT,
*            SY-UZEIT                   TO  ZTPMTHST-CTME,
*            SY-UNAME                   TO  ZTPMTHST-UNAM,
*            SY-DATUM                   TO  ZTPMTHST-UDAT,
*            SY-UZEIT                   TO  ZTPMTHST-UTME.
*
*    INSERT  ZTPMTHST.
*
*    IF SY-SUBRC NE 0.
*      ROLLBACK WORK.
*      RAISE POST_ERROR.
*    ENDIF.

    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    RAISE POST_ERROR.
  ENDIF.

ENDFUNCTION.
