report Z_AR31
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

parameters: P_ID(10)  type c,
            p_date(8) type c.

data: g_id type c.
*** End generated data section ***

start-of-selection.

*perform open_dataset using dataset.
perform open_group.
*
*do.

*read dataset dataset into record.
*if sy-subrc <> 0. exit. endif.

perform bdc_dynpro      using 'RAWORK01' '1000'.
perform bdc_field       using 'BDC_CURSOR'
                              'PA_AI_ID'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ONLI'.
perform bdc_field       using 'PA_AI_ID'
                              p_id.
perform bdc_field       using 'BERDATUM'
                              p_date.
perform bdc_field       using 'BEREICH1'
                              '01'.
perform bdc_field       using 'SRTVR'
                              '0001'.
perform bdc_dynpro      using 'SAPMSSY0' '0120'.
perform bdc_field       using 'BDC_OKCODE'
                              '=AIRL'.
perform bdc_dynpro      using 'SAPMSSY0' '0120'.
perform bdc_field       using 'BDC_OKCODE'
                              '=MBCK'.
perform bdc_dynpro      using 'RAWORK01' '1000'.
perform bdc_field       using 'BDC_OKCODE'
                              '/EE'.
perform bdc_field       using 'BDC_CURSOR'
                              'PA_AI_ID'.
perform bdc_transaction using 'AR31'.

*nddo.

perform close_group.
perform close_dataset using dataset.
