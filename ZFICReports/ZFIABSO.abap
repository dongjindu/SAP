report ZFIABSO
       no standard page heading line-size 255.

include bdcrecx1.

*replace source.........................................
*parameters: dataset(132) lower case.
PARAMETERS : LOCFILE LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\abso.txt'.

***    DO NOT CHANGE - the generated data section - DO NOT CHANGE    ***
*
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
*
*** Generated data section with specific formatting - DO NOT CHANGE  ***
*change source.............................
data: begin of record occurs 0,
* data element: BUKRS
        BUKRS_001(004),
* data element: ANLN1
        ANLN1_002(012),
* data element: ANLN2
        ANLN2_003(004),
* data element: BLDAT
        BLDAT_004(010),
* data element: BUDAT
        BUDAT_005(010),
* data element: MONAT
        PERID_006(002),
* data element: BWASL
        BWASL_007(003),
* data element: BTRAB
        DMBTR_008(016),
* data element: BZDAT
        BZDAT_009(010),
      end of record.

*** End generated data section ***

start-of-selection.

*replace source.........................................
*perform open_dataset using dataset.
  PERFORM UPLOAD_PC_FILE.

  perform open_group.

*replace source.........................................
*do.
*read dataset dataset into record.
*if sy-subrc <> 0. exit. endif.
  LOOP AT RECORD.


    perform bdc_dynpro      using 'SAPMA01B' '0100'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANBZ-BWASL'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'ANBZ-BUKRS'
                                  record-BUKRS_001.
    perform bdc_field       using 'ANBZ-ANLN1'
                                  record-ANLN1_002.
    perform bdc_field       using 'ANBZ-ANLN2'
                                  record-ANLN2_003.
    perform bdc_field       using 'ANEK-BLDAT'
                                  record-BLDAT_004.
    perform bdc_field       using 'ANEK-BUDAT'
                                  record-BUDAT_005.
    perform bdc_field       using 'ANBZ-PERID'
                                  record-PERID_006.
    perform bdc_field       using 'ANBZ-BWASL'
                                  record-BWASL_007.
    perform bdc_dynpro      using 'SAPMA01B' '0110'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANBZ-BZDAT'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=UPDA'.
    perform bdc_field       using 'ANBZ-DMBTR'
                                  record-DMBTR_008.
    perform bdc_field       using 'ANBZ-BZDAT'
                                  record-BZDAT_009.
    perform bdc_transaction using 'ABSO'.

*replace source.........................................
*enddo.
*perform close_group.
*perform close_dataset using dataset.
*replace source
  ENDLOOP.

  perform close_group.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_PC_FILE.
  CALL FUNCTION 'UPLOAD'
       EXPORTING
            FILENAME            = LOCFILE
            FILETYPE            = 'DAT'
       TABLES
            DATA_TAB            = RECORD
       EXCEPTIONS
            CONVERSION_ERROR    = 1
            FILE_OPEN_ERROR     = 2
            FILE_READ_ERROR     = 3
            INVALID_TABLE_WIDTH = 4
            INVALID_TYPE        = 5
            NO_BATCH            = 6
            UNKNOWN_ERROR       = 7
            OTHERS              = 8.
ENDFORM.                    " UPLOAD_PC_FILE
