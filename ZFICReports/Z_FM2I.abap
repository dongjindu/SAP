*
*  made by Andy Choi (2003.1)
*  upload fund center
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
        T001.

DATA: BEGIN OF iftab OCCURS 0,
        FICTR    like fmfpo-FICTR,     " default fundcenter
        BEZEICH  like ifmfpo-BEZEICH,  " desc.
        parent   like IFMFCTR-PARENT_CTR,
        bossid   like ifmfctr-bossid,
        bossname like ifmfctr-bossname,
        BESCHR   like ifmfpo-BESCHR,   " desc.
        fictrt   like ifmfctr-BEZEICH, " fundctr desc
      END OF iftab.

*ELECT-OPTIONS: S_BUKRS FOR SKB1-BUKRS OBLIGATORY memory id BUK.
parameters: p_FIKRS like fmfpo-FIKRS default 'H201',
            p_upd   as checkbox,
            p_run   as checkbox.
PARAMETERS : p_file LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\fmctr.txt'.


*** Generated data section with specific formatting - DO NOT CHANGE  ***
*data: begin of record,
*      end of record.

*** End generated data section ***
Initialization.
  ctu = 'X'.            " Call Transaction
  session = ' '.
  GROUP = 'FM-Commitment Item'.

*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
start-of-selection.
  PERFORM UPLOAD_PC_FILE.

*perform open_dataset using dataset.
  perform open_group.

  LOOP AT iftab.
    if p_run = 'X'.
      perform bdc_dynpro   using 'SAPLFM22' '0100'.
      perform bdc_field    using 'BDC_OKCODE'       '/00'.
      perform bdc_field    using 'IFMFCTR-FIKRS'  p_fikrs.
      perform bdc_field    using 'IFMFCTR-FICTR'  iftab-fictr.
      perform bdc_dynpro   using 'SAPLFM22' '0200'.
      perform bdc_field    using 'BDC_OKCODE'  '=SAFE'.
      perform bdc_field    using 'IFMFCTR-BEZEICH'     iftab-bezeich.
      perform bdc_field    using 'IFMFCTR-PARENT_CTR'  iftab-parent.
      perform bdc_field    using 'IFMFCTR-BOSSID'      iftab-bossid.
      perform bdc_field    using 'IFMFCTR-BOSSNAME'    iftab-bossname.

      if p_upd = 'X'.
        perform bdc_transaction using 'FM2U'.  "update
      else.
        perform bdc_transaction using 'FM2I'.  "insert
      endif.
    else.
      write:/ iftab-fictr, iftab-bezeich.
    endif.
  endloop.

  perform close_group.
*perform close_dataset using dataset.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_PC_FILE.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME            = p_file
            FILETYPE            = 'DAT'
       TABLES
            DATA_TAB            = iftab
       EXCEPTIONS
            CONVERSION_ERROR    = 1
            INVALID_TABLE_WIDTH = 2
            INVALID_TYPE        = 3
            NO_BATCH            = 4
            UNKNOWN_ERROR       = 5
            FILE_OPEN_ERROR     = 6
            FILE_READ_ERROR     = 7
            OTHERS              = 8.

ENDFORM.                    " UPLOAD_PC_FILE
