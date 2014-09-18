FUNCTION Z_GCS_EAI_INVENTORY_GET.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_DATUM) LIKE  SY-DATUM
*"  EXPORTING
*"     VALUE(FLAG) TYPE  ZRESULT
*"     VALUE(MESS) TYPE  ZMESS
*"  TABLES
*"      EAI_INVENTORY STRUCTURE  ZSMM_EAI_INV OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------
  TYPES: BEGIN OF TY_INV.
  INCLUDE TYPE ZTMM_EAI_INV.
  TYPES: END OF TY_INV.

  DATA: BEGIN OF IT_INVENTORY OCCURS 0,
          LGTYP LIKE LQUA-LGTYP,  " Storage Type
          LGPLA LIKE LQUA-LGPLA,  " Storage BIN
          MATNR LIKE LQUA-MATNR,  " Material number
          LENUM LIKE LQUA-LENUM,  " Storage unit number
          LGNUM LIKE LQUA-LGNUM,  " Warehouse No/Warehouse Complex
          GESME LIKE LQUA-GESME,  " ON Hand Qty
        END OF IT_INVENTORY.

  DATA: IT_INV      TYPE TABLE OF TY_INV WITH HEADER LINE.
  DATA: GV_LINES TYPE I.

  SELECT A~LGTYP
         A~LGPLA
         A~MATNR
         A~LENUM
         A~LGNUM
         A~GESME
         INTO TABLE IT_INVENTORY
         FROM LQUA AS A
         INNER JOIN MARA AS B
         ON A~MATNR EQ B~MATNR
         WHERE A~LGNUM = 'P01' AND
               A~LGTYP IN ('411', '421', '422', '511', '521', '523',
                '611', '621', '623') AND
*               a~lgpla IN s_lgpla AND
*               a~matnr IN s_matnr AND
*               a~lenum IN s_lenum AND
*               a~lgnum IN s_lgnum AND
               B~MTART = 'ROH' AND
               B~LVORM EQ SPACE   AND
               B~PROFL IN ('V','K','M') AND
               B~TEMPB NE '11'.

  SORT IT_INVENTORY BY LGTYP LGPLA MATNR LENUM.
  DELETE ADJACENT DUPLICATES FROM IT_INVENTORY COMPARING
                             LGTYP LGPLA MATNR LENUM.
  IF NOT IT_INVENTORY[] IS INITIAL.
    LOOP AT IT_INVENTORY.
      IT_INV-TAIT_TARG_D    = SY-DATUM.
      IT_INV-ELOC_TP        = IT_INVENTORY-LGTYP.
      IT_INV-EBIN_NO        = IT_INVENTORY-LGPLA.
      IT_INV-EPART_NO       = IT_INVENTORY-MATNR.
      IT_INV-ESTRG_UT       = IT_INVENTORY-LENUM.
      IT_INV-EWHN           = IT_INVENTORY-LGNUM.
      IT_INV-EONHAND        = IT_INVENTORY-GESME.
      IT_INV-TAIT_TARG_T    = SY-UZEIT.
      IT_INV-TAIT_TARG_RSLT = ' '.
      IT_INV-TAIT_TARG_DESC = ' '.
      IT_INV-TAIT_EVENT_C   = 'I'.
      APPEND IT_INV.
      CLEAR IT_INV.
    ENDLOOP.

    CLEAR GV_LINES.
    DESCRIBE TABLE IT_INV LINES GV_LINES.

    DELETE FROM ZTMM_EAI_INV
*           WHERE epart_no IN s_matnr AND
*                 eloc_tp  IN s_lgtyp AND
*                 ebin_no  IN s_lgpla AND
*                 ewhn     IN s_lgnum AND
*                 estrg_ut IN s_lenum AND
            WHERE TAIT_TARG_D EQ P_DATUM.

    INSERT ZTMM_EAI_INV FROM TABLE IT_INV.

    IF SY-SUBRC = 0.
      FLAG = 'S'.
      EAI_INVENTORY[] = IT_INV[].
    ELSE.
      FLAG = 'E'.
      MESS = TEXT-002.
    ENDIF.
  ELSE.
    FLAG = 'E'.
    MESS = TEXT-003.
  ENDIF.

ENDFUNCTION.
