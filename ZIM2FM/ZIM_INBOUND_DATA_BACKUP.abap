FUNCTION ZIM_INBOUND_DATA_BACKUP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(FILE_NAME)
*"     REFERENCE(BACK_PATH) LIKE  ZTIMIMGTX-ZFRBAK
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NO_DATA
*"----------------------------------------------------------------------
DATA: L_FILENAME(200) TYPE C,
      BACK_FILE(200)  TYPE C,
      L_LEN           TYPE I,
      L_FLEN          TYPE I,
      L_START         TYPE I,
      L_STR_TMP       TYPE I,
*                     VALUE '/usr/sap/trans/int/bak/inbound/',
      W_EDI_RECORD(65535).
FIELD-SYMBOLS <FC>.

*------ EDI
DATA: BEGIN OF IT_FLATDATA OCCURS 0,
      RECORD(65535),
      END OF IT_FLATDATA.

  CLEAR : L_START, L_STR_TMP.
  L_LEN = STRLEN( FILE_NAME ).

  DO.
     IF L_STR_TMP GT L_LEN.
        EXIT.
     ENDIF.
     ASSIGN FILE_NAME+L_STR_TMP(1) TO <FC>.
     IF <FC> EQ '/'.
        L_START = L_STR_TMP.
     ENDIF.
     ADD 1 TO L_STR_TMP.
  ENDDO.

  ADD 1 TO L_START.
  L_FLEN = L_LEN - L_START.

  CONCATENATE BACK_PATH '/' INTO BACK_FILE.
  L_FILENAME = FILE_NAME+L_START(L_FLEN).
  CONCATENATE BACK_FILE L_FILENAME INTO BACK_FILE.

  REFRESH : IT_FLATDATA.

  OPEN DATASET    FILE_NAME     FOR     INPUT   IN  TEXT  MODE.
  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  DO.
     READ    DATASET    FILE_NAME     INTO    W_EDI_RECORD.

     IF SY-SUBRC    EQ    4.
        CLOSE DATASET FILE_NAME.

        IF IT_FLATDATA[] IS INITIAL.
           DELETE  DATASET    FILE_NAME.
           RAISE NO_DATA.
        ELSE.
           OPEN DATASET BACK_FILE FOR OUTPUT IN TEXT MODE.
           IF SY-SUBRC EQ 0.
              LOOP AT IT_FLATDATA.
                 TRANSFER IT_FLATDATA-RECORD  TO  BACK_FILE.
              ENDLOOP.
              CLOSE DATASET    BACK_FILE.
              DELETE  DATASET    FILE_NAME.
           ELSE.

           ENDIF.
           EXIT.
        ENDIF.
     ELSE.
        MOVE : W_EDI_RECORD TO IT_FLATDATA.
        APPEND IT_FLATDATA.
     ENDIF.
  ENDDO.

ENDFUNCTION.
