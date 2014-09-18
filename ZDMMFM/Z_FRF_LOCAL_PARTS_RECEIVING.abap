FUNCTION Z_FRF_LOCAL_PARTS_RECEIVING.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LIFEX) LIKE  LIKP-LIFEX
*"     VALUE(I_LIFNR) LIKE  LIKP-LIFNR
*"     VALUE(I_PERNR) LIKE  LTAK-PERNR OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_RECEIVER STRUCTURE  ZSRF_LOCA_RECEIVER
**
**  08/25/2010  Matthew  UD1K949722  LPR Enhancements
*"----------------------------------------------------------------------
  DATA: BEGIN OF LT_RECEIVER OCCURS 0.
          INCLUDE STRUCTURE T_RECEIVER.
  DATA: WBSTK LIKE VBUK-WBSTK,
        KOSTK LIKE VBUK-KOSTK,
** Added by Furong on 11/13/08
        TRAID LIKE LIKP-TRAID,
        LVSTK LIKE VBUK-LVSTK,
** End of addition
        END OF LT_RECEIVER.

  DATA: IT_LPR LIKE TABLE OF ZTMM_LPR_STATUS WITH HEADER LINE.

** select a~vbeln lifex kdmat lfdat lfuhr
*   into corresponding fields of table T_RECEIVER
*   from likp as a
*   inner join lips as b
*   ON  A~VBELN = B~VBELN
*   inner join vbuk as c
*   ON  A~VBELN = c~VBELN
*   where lifex = i_LIFEX
*     and LIFNR = I_LIFNR
*     and c~wbstk = 'A'
*     and c~KOSTK = 'A'.

** Begin of change by Matthew on 08/26/2010
** Added the Rounding Value from MRP1 view to the query
**  SELECT A~VBELN LIFEX KDMAT LFDAT LFUHR MATNR LFIMG WBSTK KOSTK
*     TRAID LVSTK
*     INTO CORRESPONDING FIELDS OF TABLE LT_RECEIVER
*     FROM LIKP AS A
*     INNER JOIN LIPS AS B
*     ON  A~VBELN = B~VBELN
*     INNER JOIN VBUK AS C
*     ON  A~VBELN = C~VBELN
*     WHERE LIFEX = I_LIFEX
*       AND LIFNR = I_LIFNR.

  SELECT A~VBELN LIFEX KDMAT LFDAT LFUHR B~MATNR LFIMG WBSTK KOSTK
     TRAID LVSTK BSTRF
     INTO CORRESPONDING FIELDS OF TABLE LT_RECEIVER
     FROM LIKP AS A
     INNER JOIN LIPS AS B
     ON  A~VBELN = B~VBELN
     INNER JOIN VBUK AS C
     ON  A~VBELN = C~VBELN
     INNER JOIN MARC AS D
     ON  B~MATNR = D~MATNR
     WHERE LIFEX = I_LIFEX
       AND LIFNR = I_LIFNR.

** End of change by Matthew on 08/26/2010

  IF SY-SUBRC = 0.
** Changed by Furong on 06/21/10
*    SORT LT_RECEIVER BY VBELN.
** Changed by Furong on 08/04/10
*    SORT LT_RECEIVER BY MATNR VBELN.
*    SORT LT_RECEIVER BY VBELN.
** End of change on 08/04/10
** Changed by Matthew on 08/25/10
    SORT LT_RECEIVER BY MATNR VBELN.
** End of change on 08/25/10
** End of change
** Changed by Furong on 07/06/10
*    DELETE ADJACENT DUPLICATES FROM LT_RECEIVER.
** Changed by Furong on 08/04/10
** Changed by Matthew on 08/25/10
*   DELETE ADJACENT DUPLICATES FROM LT_RECEIVER.
** End of change on 08/25/10
** End of change on 08/04/10
** End of change
    LOOP AT LT_RECEIVER.
      IF LT_RECEIVER-WBSTK = 'A' AND ( LT_RECEIVER-KOSTK = 'A'
**A__ BY PAUL
      OR LT_RECEIVER-KOSTK = '' ).
        MOVE-CORRESPONDING LT_RECEIVER TO T_RECEIVER.
        APPEND T_RECEIVER.
        CLEAR: T_RECEIVER.
      ENDIF.
    ENDLOOP.
    IF T_RECEIVER[] IS INITIAL.
      ZRESULT = '1'.
      E_MESS = 'The inbound delivery has already been processed'.
** Added by Furong on 11/13/08
      CLEAR: IT_LPR.
      MOVE: 'P' TO IT_LPR-ZDELF,
            SY-DATUM TO IT_LPR-ZDATE,
            SY-UZEIT TO IT_LPR-ZTIME,
            I_PERNR TO IT_LPR-ZUSER.
      LOOP AT LT_RECEIVER.
        IT_LPR-ZVBELN = LT_RECEIVER-VBELN.
        IT_LPR-ZDEL_DATE = LT_RECEIVER-LFDAT.
        IT_LPR-ZVENDOR = I_LIFNR.
        SELECT SINGLE NAME1 INTO IT_LPR-ZVENDOR_NAME
          FROM LFA1
          WHERE LIFNR = I_LIFNR.
        IT_LPR-ZTRAID = LT_RECEIVER-TRAID.
        IT_LPR-ZLIFEX = I_LIFEX.
        IT_LPR-ZQPS =  LT_RECEIVER-KOSTK.
        IT_LPR-ZWM =  LT_RECEIVER-LVSTK.
        IT_LPR-ZGM =  LT_RECEIVER-WBSTK.
        APPEND IT_LPR.
        CLEAR: IT_LPR-ZVENDOR_NAME.
      ENDLOOP.
      SORT IT_LPR BY ZVENDOR ZLIFEX.
      DELETE ADJACENT DUPLICATES FROM IT_LPR COMPARING ZVENDOR ZLIFEX.
      INSERT ZTMM_LPR_STATUS FROM TABLE IT_LPR
        ACCEPTING DUPLICATE KEYS.
*      IF SY-SUBRC = 0.
      COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
** End of addition on 11/13/08
    ELSE.
      ZRESULT  = '0'.
      E_MESS = 'Success !'.
    ENDIF.
  ELSE.
    ZRESULT  = '1'.
    E_MESS = 'No inbound deliveries found'.
** ADDED BY FURONG ON 11/13/08
    CLEAR: IT_LPR.
    MOVE: 'X' TO IT_LPR-ZDELF,
          SY-DATUM TO IT_LPR-ZDATE,
          SY-UZEIT TO IT_LPR-ZTIME,
          I_PERNR TO IT_LPR-ZUSER.
    IT_LPR-ZVENDOR = I_LIFNR.
    SELECT SINGLE NAME1 INTO IT_LPR-ZVENDOR_NAME
        FROM LFA1
        WHERE LIFNR = I_LIFNR.
    IT_LPR-ZLIFEX =  I_LIFEX.
    APPEND IT_LPR.
    CLEAR: IT_LPR, IT_LPR-ZVENDOR_NAME.
    INSERT ZTMM_LPR_STATUS FROM TABLE IT_LPR
           ACCEPTING DUPLICATE KEYS.
*    IF SY-SUBRC = 0.
    COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
** End of addition on 11/13/08
  ENDIF.

ENDFUNCTION.
