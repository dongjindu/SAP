report Z_FK01 no standard page heading line-size 255.

include bdcrecx1.

parameters: dataset(132) lower case.
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
* data element: BUKRS
        BUKRS_001(004),
* data element: KTOKK
        KTOKK_002(004),
* data element: NAME1_GP
        NAME1_003(035),
* data element: SORTL
        SORTL_004(010),
* data element: STRAS_GP
        STRAS_005(035),
* data element: ORT01_GP
        ORT01_006(035),
* data element: PSTLZ
        PSTLZ_007(010),
* data element: LAND1_GP
        LAND1_008(003),
* data element: REGIO
        REGIO_009(003),
* data element: TELF1
        TELF1_010(016),
* data element: TELFX
        TELFX_011(031),
* data element: AKONT
        AKONT_012(010),
* data element: DZUAWA
        ZUAWA_013(003),
* data element: FDGRV
        FDGRV_014(010),
* data element: ALTKN
        ALTKN_015(010),
* data element: DZTERM
        ZTERM_016(004),
* data element: REPRF
        REPRF_017(001),
* data element: DZWELS
        ZWELS_018(010),
        pernr(10),
      end of record.

*** End generated data section ***

PARAMETERS : LOCFILE LIKE RLGRAP-FILENAME DEFAULT
   'c:\temp\vend.txt'.
parameters : p_new as checkbox.
tables: lfb1.

start-of-selection.

*  perform open_dataset using dataset.
  PERFORM UPLOAD_PC_FILE.

  perform open_group.


  LOOP AT RECORD.
* check
    if p_new <> 'X'.
      select single * from lfb1 where bukrs = record-BUKRS_001
                                  and ALTKN = record-ALTKN_015.
      if sy-subrc = 0.
        continue.
      endif.
    endif.

* ok
    perform bdc_dynpro      using 'SAPMF02K' '0105'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'RF02K-KTOKK'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'RF02K-BUKRS'
                                  record-BUKRS_001.
    if record-pernr <> space.
      perform bdc_field       using 'RF02K-LIFNR' record-pernr.
    endif.

    perform bdc_field       using 'RF02K-KTOKK'
                                  record-KTOKK_002.
    perform bdc_dynpro      using 'SAPMF02K' '0110'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'LFA1-SORTL'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'LFA1-NAME1'
                                  record-NAME1_003.
    perform bdc_field       using 'LFA1-SORTL'
                                  record-SORTL_004.
    perform bdc_field       using 'LFA1-STRAS'
                                  record-STRAS_005.
    perform bdc_field       using 'LFA1-ORT01'
                                  record-ORT01_006.
    perform bdc_field       using 'LFA1-PSTLZ'
                                  record-PSTLZ_007.
    perform bdc_field       using 'LFA1-LAND1'
                                  record-LAND1_008.
    perform bdc_field       using 'LFA1-REGIO'
                                  record-REGIO_009.
    perform bdc_field       using 'LFA1-TELF1'
                                  record-TELF1_010.
    perform bdc_field       using 'LFA1-TELFX'
                                  record-TELFX_011.
    perform bdc_dynpro      using 'SAPMF02K' '0120'.
*    perform bdc_field       using 'BDC_CURSOR'
*                                  'LFA1-KUNNR'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_dynpro      using 'SAPMF02K' '0130'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'LFBK-BANKS(01)'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=ENTR'.
    perform bdc_dynpro      using 'SAPMF02K' '0210'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'LFB1-FDGRV'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'LFB1-AKONT'
                                  record-AKONT_012.
    perform bdc_field       using 'LFB1-ZUAWA'
                                  record-ZUAWA_013.
    perform bdc_field       using 'LFB1-FDGRV'
                                  record-FDGRV_014.
    perform bdc_field       using 'LFB1-ALTKN'
                                  record-ALTKN_015.
    perform bdc_dynpro      using 'SAPMF02K' '0215'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'LFB1-REPRF'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=UPDA'.
    perform bdc_field       using 'LFB1-ZTERM'
                                  record-ZTERM_016.
    perform bdc_field       using 'LFB1-REPRF'
                                  record-REPRF_017.
    perform bdc_field       using 'LFB1-ZWELS'
                                  record-ZWELS_018.
    perform bdc_transaction using 'FK01'.

  endloop.

  perform close_group.
*  perform close_dataset using dataset.

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
