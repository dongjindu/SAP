FUNCTION ZHR_BADGEHISTORYHMMA_SEC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ACTION) LIKE  BAPIRET2-TYPE OPTIONAL
*"  EXPORTING
*"     VALUE(ZRET) LIKE  BAPIRET2-TYPE
*"     VALUE(ZMSG) TYPE  BAPIRET2-MESSAGE_V1
*"  TABLES
*"      ZTBHISTHMMA STRUCTURE  ZTHR_BHISTHMMA
*"----------------------------------------------------------------------
* Developer       date           HPticket        Transport         Desc
* Haseeb         04/24/2007    75EA363637      UD1K940360     This
*  fucntion module is created to insert the data into Security table,
*  Initiated by Mike Z.

  IF ACTION EQ 'M'.
    MODIFY ZTHR_BHISTHMMA FROM TABLE ZTBHISTHMMA.
  ELSE.
    INSERT ZTHR_BHISTHMMA FROM TABLE ZTBHISTHMMA.
  ENDIF.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
*   ZTBHISTHMMA-ZRET = 'E'.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = SY-MSGID
              MSGNR               = SY-MSGNO
              MSGV1               = SY-MSGV1
              MSGV2               = SY-MSGV2
              MSGV3               = SY-MSGV3
              MSGV4               = SY-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = ZMSG.

*   MODIFY ZTBHISTHMMA TRANSPORTING ZRET ZMSG WHERE ID = ZTBHISTHMMA-ID.
    ZRET = 'E'.


*   SELECT TEXT INTO ZTBHISTHMMA-ZMSG FROM T100 WHERE MSGNR = SY-MSGV1.
*   ZTBHISTHMMA-ZMSG = ''.
  ELSE.
    COMMIT WORK.
*   ZTBHISTHMMA-ZRET = 'S'.
    ZRET = 'S'.
*   MODIFY ZTBHISTHMMA TRANSPORTING ZRET ZMSG WHERE ID = ZTBHISTHMMA-ID.
  ENDIF.


ENDFUNCTION.
