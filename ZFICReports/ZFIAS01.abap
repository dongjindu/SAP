report ZFIAS01
       no standard page heading line-size 255.

include bdcrecx1.

*replace
*parameters: dataset(132) lower case.
PARAMETERS : LOCFILE LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\as01.txt'.

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
data: begin of record occurs 0,
* data element: ANLKL
        ANLKL_001(008),
* data element: BUKRS
        BUKRS_002(004),
* data element: GRUANZ
        NASSETS_003(003),
* data element: ANLN1
        ANLN1_004(012),
* data element: ANLN2
        ANLN2_005(004),
* data element: TXA50_ANLT
        TXT50_006(050),
* data element: MEINS
        MEINS_007(003),
* data element: AFASL
        AFASL_01_008(004),
* data element: NDJAR
        NDJAR_01_009(003),
* data element: AFABG
        AFABG_01_010(010),
      end of record.

*** End generated data section ***

start-of-selection.

*add source
  PERFORM UPLOAD_PC_FILE.

  perform open_group.

* replace
  LOOP AT RECORD.

*original source
    perform bdc_dynpro      using 'SAPLAIST' '0105'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'RA02S-NASSETS'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'ANLA-ANLKL'
                                  record-ANLKL_001.
    perform bdc_field       using 'ANLA-BUKRS'
                                  record-BUKRS_002.
    perform bdc_field       using 'RA02S-NASSETS'
                                  record-NASSETS_003.
    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=TAB08'.
    perform bdc_field       using 'ANLA-ANLN1'
                                  record-ANLN1_004.
    perform bdc_field       using 'ANLA-ANLN2'
                                  record-ANLN2_005.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLA-TXT50'.
    perform bdc_field       using 'ANLA-TXT50'
                                  record-TXT50_006.
    perform bdc_field       using 'ANLA-MEINS'
                                  record-MEINS_007.
    perform bdc_dynpro      using 'SAPLAIST' '1000'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=BUCH'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'ANLB-AFABG(01)'.
    perform bdc_field       using 'ANLB-AFASL(01)'
                                  record-AFASL_01_008.
    perform bdc_field       using 'ANLB-NDJAR(01)'
                                  record-NDJAR_01_009.
    perform bdc_field       using 'ANLB-AFABG(01)'
                                  record-AFABG_01_010.
    perform bdc_transaction using 'AS01'.

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
