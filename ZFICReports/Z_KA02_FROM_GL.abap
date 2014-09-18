*
*  made by Andy Choi (2003.1)
*  upload commitment item
*
report Z_FS02 no standard page heading line-size 255.

include bdcrecx1.

*   If it is nessesary to change the data section use the rules:
*   1.) Each definition of a field exists of two lines
*   2.) The first line shows exactly the comment
*       '* data element: ' followed with the data element
*       which describes the field.
*       If you don't have a data element use the
*       comment without a data element name
*   3.) The second line shows the fieldname of the
*       structure, the fieldname must consist of
*       a fieldname and optional the character '_' and
*       three numbers and the field length in brackets
*   4.) Each field must be type C.

TABLES: SKA1,
        SKAT,
        SKB1,
        CSKA,
        CSKT,
        T001.

DATA: BEGIN OF iftab OCCURS 0,
        kstar(10)  type c,
        ktext     like CSKBZ-KTEXT,
        ltext     like CSKBZ-LTEXT,
      END OF iftab.

*ELECT-OPTIONS: S_BUKRS FOR SKB1-BUKRS OBLIGATORY memory id BUK.
parameters: p_bukrs like bkpf-bukrs default 'H201',
            p_run   as checkbox.

*** Generated data section with specific formatting - DO NOT CHANGE  ***
*data: begin of record,
*      end of record.

*** End generated data section ***
Initialization.
  ctu = 'X'.            " Call Transaction
  session = ' '.
  GROUP = 'Cost Element'.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
start-of-selection.
  select single * from t001 where bukrs = p_bukrs.
  check sy-subrc = 0.

  select * from CSKA
     where KTOPL = t001-KTOPL.

    select single * from skat
      where spras = sy-langu
        and ktopl = t001-ktopl
        and saknr = cska-kstar.

    check sy-subrc = 0.

    iftab-KSTAR = cska-kstar.
    iftab-KTEXT = skat-TXT20.
    iftab-LTEXT = skat-TXT50.
    append iftab.
  endselect.
* PERFORM UPLOAD_PC_FILE.

*perform open_dataset using dataset.
  perform open_group.

  LOOP AT iftab.
    write:/ iftab-kstar, iftab-ktext.
    if p_run = 'X'.

      perform bdc_dynpro      using 'SAPLKMA4' '0200'.
      perform bdc_field       using 'BDC_CURSOR'   'CSKBZ-KSTAR'.
      perform bdc_field       using 'BDC_OKCODE'   '/00'.
      perform bdc_field       using 'CSKBZ-KOKRS'  p_bukrs.
      perform bdc_field       using 'CSKBZ-KSTAR'  iftab-KSTAR.
      perform bdc_dynpro      using 'SAPLKMA4' '0299'.
      perform bdc_field       using 'BDC_OKCODE'   '=BU'.
      perform bdc_field       using 'BDC_CURSOR'   'CSKBZ-LTEXT'.
      perform bdc_field       using 'CSKBZ-KTEXT'  iftab-KTEXT.
      perform bdc_field       using 'CSKBZ-LTEXT'  iftab-LTEXT.
      perform bdc_transaction using 'KA02'.
      write: '...changed'.
    endif.
  endloop.

  perform close_group.
*perform close_dataset using dataset.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
*FORM UPLOAD_PC_FILE.
*  CALL FUNCTION 'WS_UPLOAD'
*       EXPORTING
*            FILENAME            = p_file
*            FILETYPE            = 'DAT'
*       TABLES
*            DATA_TAB            = iftab
*       EXCEPTIONS
*            CONVERSION_ERROR    = 1
*            INVALID_TABLE_WIDTH = 2
*            INVALID_TYPE        = 3
*            NO_BATCH            = 4
*            UNKNOWN_ERROR       = 5
*            FILE_OPEN_ERROR     = 6
*            FILE_READ_ERROR     = 7
*            OTHERS              = 8.
*
*ENDFORM.                    " UPLOAD_PC_FILE
