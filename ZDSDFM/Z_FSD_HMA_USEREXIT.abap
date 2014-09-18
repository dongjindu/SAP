FUNCTION Z_FSD_HMA_USEREXIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(FLAG) TYPE  CHAR1
*"     VALUE(VBELN) LIKE  LIKP-VBELN OPTIONAL
*"     VALUE(LFART) LIKE  LIKP-LFART OPTIONAL
*"     VALUE(TCODE) LIKE  SY-TCODE OPTIONAL
*"     VALUE(KUNNR) LIKE  VBPA-KUNNR OPTIONAL
*"----------------------------------------------------------------------

  CASE FLAG.
    WHEN 'D'.
      DATA : LV_OBJEK LIKE AUSP-OBJEK.
      LV_OBJEK = VBELN.

      CASE TCODE.
        WHEN 'VL01N'. "SIGN OFF
          IF LFART EQ 'ZVLF'.
            CALL FUNCTION 'Z_FSD_HMA_SIGNOFF'
                 EXPORTING
                      BODYNO = LV_OBJEK.
            .

          ENDIF.
        WHEN 'VL02N'. "GI.
          IF LFART EQ 'ZVLF'.

            CALL FUNCTION 'Z_FSD_HMA_DELIVERY'
                 EXPORTING
                      BODYNO = LV_OBJEK.
          ENDIF.
      ENDCASE.
    WHEN 'I'.

      DATA : LV_VBELN LIKE VBRK-VBELN.
      LV_VBELN = VBELN.

      CALL FUNCTION 'Z_FSD_HMA_INVOICE'
           EXPORTING
                VBELN = LV_VBELN.

  ENDCASE.
ENDFUNCTION.
