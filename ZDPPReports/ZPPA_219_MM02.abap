report ZPPA_219_MM02
       no standard page heading line-size 255.

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
data: begin of record,
* data element: MATNR
        MATNR_001(040),
* data element: XFELD
        KZSEL_02_002(001),
* data element: MAKTX
        MAKTX_003(040),
* data element: ATBEZ
        MNAME_01_004(030),
* data element: ATBEZ
        MNAME_02_005(030),
* data element: ATWRT
        MWERT_01_006(030),
* data element: ATWRT
        MWERT_02_007(030),
* data element: MAKTX
        MAKTX_008(040),
      end of record.

*** End generated data section ***

start-of-selection.

perform open_dataset using dataset.
perform open_group.



do.

read dataset dataset into record.
if sy-subrc <> 0. exit. endif.

perform bdc_dynpro      using 'SAPLMGMM' '0060'.
perform bdc_field       using 'BDC_CURSOR'
                              'RMMG1-MATNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=AUSW'.
perform bdc_field       using 'RMMG1-MATNR'
                              record-MATNR_001.
perform bdc_dynpro      using 'SAPLMGMM' '0070'.
perform bdc_field       using 'BDC_CURSOR'
                              'MSICHTAUSW-DYTXT(02)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTR'.
perform bdc_field       using 'MSICHTAUSW-KZSEL(02)'
                              record-KZSEL_02_002.
perform bdc_dynpro      using 'SAPLMGMM' '5004'.
perform bdc_field       using 'BDC_OKCODE'
                              '=PB21'.
perform bdc_field       using 'BDC_CURSOR'
                              'MAKT-MAKTX'.
perform bdc_field       using 'MAKT-MAKTX'
                              record-MAKTX_003.
perform bdc_dynpro      using 'SAPLCEI0' '0109'.
perform bdc_field       using 'BDC_CURSOR'
                              'RCTMS-MWERT(02)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_field       using 'RCTMS-MNAME(01)'
                              record-MNAME_01_004.
perform bdc_field       using 'RCTMS-MNAME(02)'
                              record-MNAME_02_005.
perform bdc_field       using 'RCTMS-MWERT(01)'
                              record-MWERT_01_006.
perform bdc_field       using 'RCTMS-MWERT(02)'
                              record-MWERT_02_007.
perform bdc_dynpro      using 'SAPLMGMM' '5004'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'MAKT-MAKTX'.
perform bdc_field       using 'MAKT-MAKTX'
                              record-MAKTX_008.
perform bdc_transaction using 'MM02'.

enddo.

perform close_group.
perform close_dataset using dataset.
