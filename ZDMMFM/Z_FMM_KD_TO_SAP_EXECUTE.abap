FUNCTION Z_FMM_KD_TO_SAP_EXECUTE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_COUNT) TYPE  I
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  CHAR1
*"----------------------------------------------------------------------

*---
  DATA : BEGIN OF IT_ITAB OCCURS 0.
          INCLUDE STRUCTURE ZTMM_KD_ASN.
  DATA : END OF IT_ITAB.

  DATA: LT_ERROR LIKE TABLE OF ZTMM_KD_ASN_ERR WITH HEADER LINE.

  DATA : W_TOTAL TYPE I,
         W_JOBNAM LIKE TBTCJOB-JOBNAME,
         W_PROGNAM LIKE SY-REPID,
         W_INT LIKE TBTCJOB-JOBCOUNT,
         W_ZSLNO LIKE ZTMM_KD_ASN_MAIN-ZSLNO,

*---

********  HASEEB Mohammad modifications start  UD1K949694
        v_fin LIKE  TBTCV-FIN,
        v_abrt LIKE TBTCV-ABORT.

********  HASEEB Mohammad modifications complete  UD1K949694
  CLEAR : IT_ITAB, IT_ITAB[], W_TOTAL.

  SELECT COUNT( * ) INTO W_TOTAL
                    FROM ZTMM_KD_ASN.

  IF I_COUNT NE W_TOTAL.
** Changed by Furong on 03/22/10
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ERROR
             FROM ZTMM_KD_ASN
      WHERE TRAID GE SPACE.
    LOOP AT LT_ERROR.
      LT_ERROR-ERR_DATE = SY-DATUM.
      LT_ERROR-ERR_TIME = SY-UZEIT.
      MODIFY LT_ERROR.
    ENDLOOP.
    INSERT ZTMM_KD_ASN_ERR FROM TABLE LT_ERROR.
    COMMIT WORK.
** End of change
    DELETE FROM ZTMM_KD_ASN WHERE TRAID GE SPACE.
    MOVE : 'E' TO E_SUBRC.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
             FROM ZTMM_KD_ASN.

*--- get MM interface serial number
    CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
              NR_RANGE_NR             = '01'
              OBJECT                  = 'ZMM_KD'
              QUANTITY                = '1'
         IMPORTING
              NUMBER                  = W_ZSLNO
         EXCEPTIONS
              INTERVAL_NOT_FOUND      = 1
              NUMBER_RANGE_NOT_INTERN = 2
              OBJECT_NOT_FOUND        = 3
              QUANTITY_IS_0           = 4
              QUANTITY_IS_NOT_1       = 5
              INTERVAL_OVERFLOW       = 6
              BUFFER_OVERFLOW         = 7
              OTHERS                  = 8.

    IF SY-SUBRC EQ 0.
      MOVE : 'ASN KDWEB -> SAP' TO W_JOBNAM,
*           'ZEMMPM36E_IDEL'   TO w_prognam. "UD1K930445
            'ZEMMPM36E_IDEL_CHGPO_ORIG' TO W_PROGNAM.       "UD1K930445
      MOVE : W_ZSLNO TO IT_ITAB-ZSLNO.     " I/F serial number
*--- job open
      PERFORM CALL_JOB_OPEN USING W_JOBNAM
                            CHANGING W_INT.
      MOVE : W_INT TO IT_ITAB-JOBCOUNT.     " Job count
*--- internal table modify
      MODIFY IT_ITAB TRANSPORTING ZSLNO
                                  JOBCOUNT WHERE TRAID GE SPACE.

** Changed by Furong on 07/09/07 seal = invoice if seal is
** blank                             "UD1K940979
      LOOP AT IT_ITAB.
        IF IT_ITAB-BORGR_GRP IS INITIAL.
          IT_ITAB-BORGR_GRP = IT_ITAB-ZINVOICE.
          MODIFY IT_ITAB.
        ENDIF.
      ENDLOOP.
** End of change

*--- interface table insert
      INSERT ZTMM_KD_ASN_MAIN FROM TABLE IT_ITAB.
*      accepting duplicate keys.
*--- interface temp table delete
      DELETE FROM ZTMM_KD_ASN WHERE TRAID GE SPACE.
*--- submit program
      SUBMIT (W_PROGNAM) WITH S_ZSLNO  EQ W_ZSLNO
                         WITH P_UPDATE EQ 'X'
                              "
                              USER SY-UNAME
                              VIA JOB W_JOBNAM
                              NUMBER W_INT
                              AND RETURN.
*--- job close
********  HASEEB Mohammad modifications start  UD1K949694
      PERFORM CALL_JOB_CLOSE USING W_INT
                                   W_JOBNAM.
DO 200 times.
  CALL FUNCTION 'SHOW_JOBSTATE'
    EXPORTING
      jobcount         = W_INT
      jobname          = W_JOBNAM
    IMPORTING
      aborted          = v_abrt
      finished         = v_fin
    EXCEPTIONS
      jobcount_missing = 1
      jobname_missing  = 2
      job_notex        = 3
      OTHERS           = 4.
  IF sy-subrc = 0.
*    CHECK NOT ( v_fin IS INITIAL ) OR  NOT ( v_abrt IS INITIAL ).
     IF v_abrt = 'X'.
        EXIT.
      ELSEIF v_fin = 'X'.
        EXIT.
     ENDIF.

*    EXIT.

  ELSE.
    EXIT.
  ENDIF.
 WAIT UP TO 30 SECONDS.
ENDDO.
******** HASEEB Mohammad modifications  complete UD1K949694


    ENDIF.
  ENDIF.

ENDFUNCTION.
