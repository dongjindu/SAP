FUNCTION Z_FPM_UPLOAD_IMAGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(NODE_KEY) TYPE  CHAR20
*"     VALUE(UP_FLAG) TYPE  CHAR01 DEFAULT 'X'
*"  EXCEPTIONS
*"      CANCEL_PROCESS
*"      FAILURE_PROCESS
*"      DUPLICATE_ERROR
*"      NOT_EXIST
*"----------------------------------------------------------------------

  DATA: LV_SUBRC LIKE SY-SUBRC.

  CLEAR WA_OBJECT_ID.
  CONCATENATE C_PREFIX NODE_KEY INTO WA_OBJECT_ID.

  PERFORM CHECK_EXIST_DATA USING    C_RELID
                                    WA_OBJECT_ID
                           CHANGING LV_SUBRC.
  IF LV_SUBRC NE 0.
    IF UP_FLAG EQ 'X'.
      RAISE DUPLICATE_ERROR.
    ENDIF.
  ELSE.
    IF UP_FLAG EQ ' '.
      RAISE NOT_EXIST.
    ENDIF.
  ENDIF.

  CLEAR: BDC_TAB, MSG_TAB.    REFRESH BDC_TAB.

  IF UP_FLAG EQ 'X'.
    CALL SELECTION-SCREEN 100 STARTING AT 25 5.
    IF SY-SUBRC EQ 0.
      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMWWW0'          '0100',
        ' '    'RADIO_MI'          'X',
        ' '    'BDC_OKCODE'        '=CRO1'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'RSWWWSHW'          '1000',
        ' '    'SO_DEVCL-LOW'      C_DEVCLASS,
        ' '    'BDC_OKCODE'        '=ONLI'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMWWW0'          '0200',
        ' '    'BDC_OKCODE'        '=HCRE'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMWWW0'          '0300',
        ' '    'WWWDATA_NEW-OBJID' wa_OBJECT_ID,
        ' '    'WWWDATA_NEW-TEXT'  PA_TEXT,
        ' '    'BDC_OKCODE'        '=HIMP'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLGRAP'          '0210',
        ' '    'RLGRAP-FILENAME'   PA_FILE,
        ' '    'RLGRAP-FILEFM_UL'  C_FILETYPE,
        ' '    'BDC_OKCODE'        '=SEND'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPLSTRD'          '0100',
        ' '    'KO007-L_DEVCLASS'  C_DEVCLASS,
        ' '    'KO007-L_AUTHOR'    SY-UNAME,
        ' '    'BDC_OKCODE'        '=TEMP'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMWWW0'          '0200',
        ' '    'BDC_OKCODE'        '=BACK'.

      PERFORM GENERATE_BDC_DATA USING:
        'X'    'SAPMWWW0'          '0100',
        ' '    'BDC_OKCODE'        '=ENDE'.

    ELSE.
      RAISE CANCEL_PROCESS.
    ENDIF.
  ELSE.
    PERFORM GENERATE_BDC_DATA USING:
      'X'    'SAPMWWW0'          '0100',
      ' '    'RADIO_MI'          'X',
      ' '    'BDC_OKCODE'        '=CRO1'.

    PERFORM GENERATE_BDC_DATA USING:
      'X'    'RSWWWSHW'          '1000',
      ' '    'SO_DEVCL-LOW'      C_DEVCLASS,
      ' '    'SO_OBJID-LOW'      WA_OBJECT_ID,
      ' '    'BDC_OKCODE'        '=ONLI'.

    PERFORM GENERATE_BDC_DATA USING:
      'X'    'SAPMWWW0'          '0200',
      ' '    'SEL_FIELD(01)'     'X',
      ' '    'BDC_OKCODE'        '=HDEL'.

    PERFORM GENERATE_BDC_DATA USING:
      'X'    'SAPLSPO1'          '0400',
      ' '    'BDC_OKCODE'        '=YES'.

    PERFORM GENERATE_BDC_DATA USING:
      'X'    'SAPMWWW0'          '0200',
      ' '    'BDC_OKCODE'        '=BACK'.

    PERFORM GENERATE_BDC_DATA USING:
      'X'    'SAPMWWW0'          '0100',
      ' '    'BDC_OKCODE'        '=ENDE'.
  ENDIF.

  PERFORM CALL_TRANSACTION USING 'SMW0'
                           CHANGING LV_SUBRC.
  IF LV_SUBRC NE 0.
    RAISE FAILURE_PROCESS.
  ENDIF.

ENDFUNCTION.
