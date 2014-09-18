FUNCTION Z_FQM_EAI_PDMS_EO_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_SE_FLAG) TYPE  CHAR1 DEFAULT ' '
*"  EXPORTING
*"     VALUE(E_RET_STATUS) TYPE  CHAR1
*"  TABLES
*"      T_AXMASTDT STRUCTURE  ZSQM_IQS_EO OPTIONAL
*"----------------------------------------------------------------------

  TABLES : ZTQM_MAT_EO, ZTQM_MAT_EO_R.

  DATA : LT_ZTQM_MAT   LIKE ZTQM_MAT_EO   OCCURS 0 WITH HEADER LINE.
  DATA : LT_ZTQM_MAT_R LIKE ZTQM_MAT_EO_R OCCURS 0 WITH HEADER LINE.

  CHECK NOT T_AXMASTDT[] IS INITIAL.

*-- Replace data to internal table for updationg CBO table
  LOOP  AT T_AXMASTDT.
    CLEAR LT_ZTQM_MAT_R.
    MOVE-CORRESPONDING T_AXMASTDT TO LT_ZTQM_MAT_R.
    APPEND LT_ZTQM_MAT_R.
  ENDLOOP.

*-- Data Delete
*-     I_SE_FLAG = 'S' : Start -> Start of forwarding : Delete All Data
*-                 'E' : Last  -> End of forwarding from PDMS by EAI

  IF I_SE_FLAG = 'S'.
    DELETE FROM ZTQM_MAT_EO_R WHERE PTNO NE ''.
  ENDIF.

*-- Insert data to DB(ZTQM_MAT_EO)
  INSERT ZTQM_MAT_EO_R FROM TABLE LT_ZTQM_MAT_R
                                      ACCEPTING DUPLICATE KEYS.

*-- Return result status.
  IF SY-SUBRC = 0.
    COMMIT WORK.
    MOVE : 'S' TO E_RET_STATUS.
    MOVE : 'S' TO T_AXMASTDT-ZZRET.
    MODIFY T_AXMASTDT   TRANSPORTING ZZRET
                   WHERE PTNO NE ''.

  ELSE.
    ROLLBACK WORK.
    MOVE 'E' TO E_RET_STATUS.
    MOVE : 'E' TO T_AXMASTDT-ZZRET.
    MODIFY T_AXMASTDT   TRANSPORTING ZZRET
                   WHERE PTNO NE ''.
  ENDIF.

*-/ IF last forwarding from EAI(from PDMS)
*-- Fill ZTQM_MAT_EO and Modify ZDELFG field by comparing ZTQM_MAT_EO
*   with ZTQM_MAT_EO_R.

  CHECK I_SE_FLAG = 'E'.

  REFRESH LT_ZTQM_MAT.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ZTQM_MAT
    FROM ZTQM_MAT_EO_R.

  CHECK SY-SUBRC = 0.
**  Clear delete flag field for managing item log
*  MOVE : ' ' TO LT_ZTQM_MAT-ZDELFG.
*  MODIFY LT_ZTQM_MAT  TRANSPORTING ZDELFG
*                   WHERE PTNO NE ''.

*- Insert new entries and modify item  already exist.
  MODIFY ZTQM_MAT_EO FROM TABLE LT_ZTQM_MAT.

  IF  SY-SUBRC NE 0.
    ROLLBACK WORK.
    EXIT.
  ENDIF.

  REFRESH LT_ZTQM_MAT.

*-  Get items not exists in ZTQM_MAT_EO_R
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_ZTQM_MAT
    FROM ZTQM_MAT_EO AS A
      WHERE NOT EXISTS ( SELECT * FROM ZTQM_MAT_EO_R
                           WHERE PTNO = A~PTNO
                             AND EONO = A~EONO
                             AND VEND = A~VEND ).

  CHECK SY-SUBRC = 0.
*-  Mart deletion flag of item that is not exists in PDMS
  MOVE : 'X' TO LT_ZTQM_MAT-ZDELFG.

  MODIFY LT_ZTQM_MAT TRANSPORTING ZDELFG
                 WHERE PTNO NE ''.

  MODIFY ZTQM_MAT_EO FROM TABLE LT_ZTQM_MAT.

  IF  SY-SUBRC NE 0.
    ROLLBACK WORK.
    EXIT.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
