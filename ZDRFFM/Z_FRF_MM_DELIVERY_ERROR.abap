FUNCTION Z_FRF_MM_DELIVERY_ERROR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_VBELN) LIKE  LIKP-VBELN
*"     VALUE(I_MESSA) LIKE  ZTMM_CT_ERRLOG-TEXT
*"     VALUE(I_EPOSITION) LIKE  ZTMM_CT_ERRLOG-EPOSITION
*"----------------------------------------------------------------------
  TABLES: ZTMM_CT_ERRLOG.
  DATA: BEGIN OF LT_LIKP OCCURS 0,
          VBELN TYPE LIKP-VBELN,  "Delivery
          VERUR TYPE LIKP-VERUR,  "Distribution delivery
          LIFNR TYPE LIKP-LIFNR,  "Vendor's account number
          VGBEL TYPE LIPS-VGBEL,  "Document number
        END OF LT_LIKP.
  DATA: LT_LOG TYPE ZTMM_CT_ERRLOG OCCURS 0 WITH HEADER LINE.

  SELECT A~VBELN
         A~VERUR
         A~LIFNR
         B~VGBEL
       FROM LIKP AS A INNER JOIN LIPS AS B
                      ON A~VBELN EQ B~VBELN
       INTO TABLE LT_LIKP
       WHERE A~VBELN EQ I_VBELN.

  IF SY-SUBRC EQ 0.
    SORT LT_LIKP BY VBELN.
    DELETE ADJACENT DUPLICATES FROM LT_LIKP
                               COMPARING VBELN.
    LOOP AT LT_LIKP.
      LT_LOG-MANDT       = SY-MANDT.
      LT_LOG-ZCONTAINER  = LT_LIKP-VERUR.
      LT_LOG-VBELN       = LT_LIKP-VBELN.
      LT_LOG-EPOSITION   = I_EPOSITION.
      LT_LOG-EBELN       = LT_LIKP-VGBEL.
      LT_LOG-PARKING_TXT = LT_LIKP-LIFNR.
      LT_LOG-TEXT        = I_MESSA.
      LT_LOG-ERDAT       = SY-DATUM.
      LT_LOG-ERZET       = SY-UZEIT.
      LT_LOG-ERNAM       = SY-UNAME.
      LT_LOG-AEDAT       = SY-DATUM.
      LT_LOG-AEZET       = SY-UZEIT.
      LT_LOG-AENAM       = SY-UNAME.
*      LT_LOG-FLAG        = LT_LIKP-VERUR.
      APPEND LT_LOG. CLEAR LT_LOG.
    ENDLOOP.

    IF NOT LT_LOG[] IS INITIAL.
      READ TABLE LT_LOG INDEX 1.
      SELECT SINGLE *
                  FROM ZTMM_CT_ERRLOG
                  WHERE ZCONTAINER EQ LT_LOG-ZCONTAINER
                  AND   VBELN      EQ LT_LOG-VBELN.
      IF SY-SUBRC EQ 0.
        UPDATE ZTMM_CT_ERRLOG FROM TABLE LT_LOG.
        IF SY-SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ELSE.
      INSERT ZTMM_CT_ERRLOG FROM TABLE LT_LOG ACCEPTING DUPLICATE KEYS .
        IF SY-SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
