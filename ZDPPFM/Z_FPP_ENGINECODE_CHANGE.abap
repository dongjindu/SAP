FUNCTION Z_FPP_ENGINECODE_CHANGE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_ZTPPVM) LIKE  ZTPPVM STRUCTURE  ZTPPVM
*"  EXPORTING
*"     REFERENCE(SUCCESS)
*"  EXCEPTIONS
*"      NO_ENGINE_LIST
*"      NO_ENGINE_COMPONENT
*"      NO_PO_IN_VM
*"      NO_PLAN_ORDER
*"      UPDATE_FAIL
*"      BACKFLUSH_DONE
*"      WRONG_ENGINE_CODE
*"      MATERIAL_CODE_ERROR
*"----------------------------------------------------------------------
  DATA: L_OBJEK LIKE AUSP-OBJEK.
  DATA: L_FLAG.
  DATA: L_RSNUM LIKE PLAF-RSNUM.
  DATA: W_BFQTY LIKE RESB-FMENG.
  DATA: LT_RESB LIKE RESB OCCURS 0 WITH HEADER LINE.
  DATA: L_MATNR LIKE MARC-MATNR.

* check the data
  IF I_ZTPPVM-P_AIRBAG_NO30 IS INITIAL.
    EXIT.    "do nothing if no engine code
  ENDIF.

* read the engine list

  SELECT ATWRT AS ENGIN
   INTO CORRESPONDING FIELDS OF TABLE IT_ENGINE
   FROM CAWN AS A INNER JOIN CABN AS B
     ON A~ATINN = B~ATINN
   WHERE B~ATNAM = 'EN_ITEM_CODE'.
  IF SY-SUBRC NE 0.
    RAISE NO_ENGINE_LIST.
    EXIT.
  ENDIF.

* Check if the input engine code exist
  READ TABLE IT_ENGINE WITH KEY ENGIN = I_ZTPPVM-P_AIRBAG_NO30.
  IF SY-SUBRC NE 0.
    RAISE WRONG_ENGINE_CODE.
    EXIT.
  ENDIF.

* make the objek
  CONCATENATE I_ZTPPVM-P_MODEL
              I_ZTPPVM-P_BODY_SERIAL
         INTO L_OBJEK.
  CHECK NOT L_OBJEK IS INITIAL.

* read planned order
  SELECT SINGLE ATWRT INTO W_PLNUM
      FROM AUSP AS A INNER JOIN CABN AS B
        ON A~ATINN = B~ATINN
      WHERE KLART  = '002'
       AND  B~ATNAM  = 'P_PLAN_ORDER'
       AND  OBJEK  = L_OBJEK.
  IF SY-SUBRC NE 0.
    RAISE NO_PO_IN_VM.
    EXIT.
  ENDIF.

* read reservation number
  SELECT SINGLE RSNUM INTO L_RSNUM
    FROM PLAF
    WHERE PLNUM  = W_PLNUM.

  IF SY-SUBRC NE 0.
    RAISE NO_PLAN_ORDER.
    EXIT.
  ENDIF.

*  READ THE RESERVED COMPONENT.
  SELECT * INTO TABLE LT_RESB
    FROM RESB
    WHERE RSNUM  = L_RSNUM.

* seach engine part in component.
  L_FLAG = 'X'.
  LOOP AT LT_RESB .
    READ TABLE IT_ENGINE WITH KEY ENGIN  = LT_RESB-MATNR.
    IF SY-SUBRC EQ 0.
      CLEAR L_FLAG.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF L_FLAG = 'X'.
    RAISE NO_ENGINE_COMPONENT.
    EXIT.
  ENDIF.

* found the engine and check
  IF LT_RESB-MATNR EQ I_ZTPPVM-P_AIRBAG_NO30.
    SUCCESS = 'X'.  " If same code, consider as success
    EXIT.
  ENDIF.
* if two engine code is different, update the RESB with actural code

* check if the B/F has been done.

  IF LT_RESB-ENMNG NE 0.
    RAISE BACKFLUSH_DONE.
    EXIT.
  ENDIF.


** Changed on 05/08/12

*  UPDATE resb SET matnr = i_ztppvm-p_airbag_no30
*            WHERE rsnum = l_rsnum
*              AND matnr = lt_resb-matnr.
*
*  IF sy-subrc EQ 0.
*    COMMIT WORK AND WAIT.
*    success = 'X'.
*    EXIT.
*  ELSE.
*    RAISE update_fail.
*  ENDIF.

  SELECT SINGLE MATNR INTO L_MATNR
      FROM MARC
    WHERE MATNR = I_ZTPPVM-P_AIRBAG_NO30
      AND WERKS = LT_RESB-WERKS.

  IF SY-SUBRC = 0.
    UPDATE RESB SET MATNR = I_ZTPPVM-P_AIRBAG_NO30
               WHERE RSNUM = L_RSNUM
                 AND MATNR = LT_RESB-MATNR.

    IF SY-SUBRC EQ 0.
      COMMIT WORK AND WAIT.
      SUCCESS = 'X'.
      EXIT.
    ELSE.
      RAISE UPDATE_FAIL.
    ENDIF.
  ELSE.
    RAISE MATERIAL_CODE_ERROR.
    EXIT.
  ENDIF.

** End

ENDFUNCTION.
