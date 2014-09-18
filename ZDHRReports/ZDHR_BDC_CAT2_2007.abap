************************************************************************
* Author                 : Haseeb Mohammad, Hassan siddiqui.
* Creation Date          : 05/22/2007
* Specifications By      : siddiqui
* Development Request No : UD1K940617
* Addl documentation     :
* Description            : to upload July holiday schedule for all
*                          employess. The results will be seen in PA30
**                         transaction with '2002' infotype.
* Modification Log
* Date       Developer    Request ID Description
* Description            : This BDC recording uses CAT2 transaction.
*
************************************************************************

report ZDHR_BDC_CAT2
       no standard page heading line-size 255.

*include bdcrecx1.
include ZDHR_BDCRECX1_2007.

tables bdcmsgcoll.
*parameters: dataset(132) lower case .

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
* data element: CATSVARIAN
        VARIANT_001(008),
* data element: CATSINPDAT
        INPUTDATE_002(010),
* data element: PERNR_D
        PERNR_003(008),
* data element: CATSWEEKEX
        CATSWEEKEX_004(007),
* data element: AWART
        AWART_01_005(004),
* data element: CATSCELL
        DAY1_01_006(020),
* data element: CATSCELL
        DAY2_01_007(020),
* data element: CATSCELL
        DAY3_01_008(020),
* data element: CATSCELL
        DAY4_01_009(020),
* data element: CATSCELL
        DAY5_01_010(020),
* data element: CATSCELL
        DAY6_01_011(020),
* data element: CATSCELL
        DAY7_01_012(020),
      end of record.
*2007 July
DATA: IT_UPFILE  like record occurs 0 with header line.
data: zcat(4) type c value 'CAT2'.
DATA: eno(8) type c.
data: zmsg(100) type c.
data: begin of zmsgtab,
      zmsg(100) type c,
      end of zmsgtab.
DATA: itab like table of zmsgtab .
data: itabbdc type  bdcmsgcoll occurs 0.
DATA: counter type i value  0.
*** End generated data section ***
selection-screen begin of block prosel.
parameters: dataset1 like rlgrap-filename obligatory
               default 'c:\'.
selection-screen end of  block prosel.


at selection-screen on value-request for dataset1.
  call function 'F4_FILENAME'
       IMPORTING
            file_name = dataset1.

** Furong on 08/31/12 for deactivate the unused program
Initialization.
Leave program.
** End on 08/31/12

start-of-selection.
*perform open_dataset using dataset.
call function 'WS_UPLOAD'
       EXPORTING
         FILENAME  = dataset1
*         FILETYPE  = 'TXT'
       TABLES
         DATA_TAB  = it_upfile
       EXCEPTIONS
        CONVERSION_ERROR = 1
        FILE_OPEN_ERROR  = 2
        FILE_READ_ERROR  = 3
        INVALID_TYPE     = 4
        NO_BATCH         = 5
*      BAD_DATA_FORMAT  = 6
        OTHERS           = 9.


 if sy-subrc <> 0.
    write: /01 'Error loading input file, subrc = ', sy-subrc.
    stop.
 ENDIF.

perform open_group.
CLEAR it_upfile.
*do.
LOOP AT it_upfile into record.
*read table it_upfile into record.
*read dataset dataset into record.
if sy-subrc <> 0. exit. endif.
counter = counter + 1.
perform bdc_dynpro      using 'SAPLCATS' '1000'.
perform bdc_field       using 'BDC_CURSOR'
                              'CATSFIELDS-PERNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=TIME'.
perform bdc_field       using 'TCATST-VARIANT'
                              record-VARIANT_001.
perform bdc_field       using 'CATSFIELDS-INPUTDATE'
                              record-INPUTDATE_002.
perform bdc_field       using 'CATSFIELDS-PERNR'
                              record-PERNR_003.
perform bdc_dynpro      using 'SAPLCATS' '2002'.
perform bdc_field       using 'BDC_CURSOR'
                              'CATSD-DAY1(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SAVE'.
perform bdc_field       using 'CATSFIELDS-CATSWEEKEX'
                              record-CATSWEEKEX_004.
perform bdc_field       using 'CATSD-AWART(01)'
                              record-AWART_01_005.
perform bdc_field       using 'CATSD-DAY1(01)'
                              record-DAY1_01_006.
 perform bdc_field       using 'CATSD-DAY2(01)'
                              record-DAY2_01_007.
perform bdc_field       using 'CATSD-DAY3(01)'
                              record-DAY3_01_008.
perform bdc_field       using 'CATSD-DAY4(01)'
                              record-DAY4_01_009.
perform bdc_field       using 'CATSD-DAY5(01)'
                              record-DAY5_01_010.
***********Saturday Working day
perform bdc_field       using 'CATSD-DAY6(01)'
                              record-DAY6_01_011.
***********Sunday working day
perform bdc_field       using 'CATSD-DAY7(01)'
                              record-DAY7_01_012.

**************
perform bdc_dynpro      using 'SAPLCATS' '2002'.
perform bdc_field       using 'BDC_CURSOR'
                              'CATSD-DAY5(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SAVE'.
***********************
eno = record-pernr_003.

perform bdc_transaction using 'CAT2' eno counter.

*if sy-subrc = 0.
**  write: / 'Employee update failed:' ,record-pernr_003.
*   read table bdcmsgcoll  with key bdcmsgcoll-MSGTYP." = 'S'.
*   call function 'FORMAT_MESSAGE' importing msg = zmsg.
*   select * from bdcmsgcoll into itabbdc.
*    if itabbdc-msgtyp = 'E'.
*     call function 'FORMAT_MESSAGE' importing msg = zmsg.
*     zmsgtab-zmsg = zmsg.
*     append zmsgtab to itab.
*
*    endif.
*
*else.
*   loop at bdcmsgcoll version 'ABCD'.
*    if bdcmsgcoll-msgtyp = 'E'.
*     call function 'FORMAT_MESSAGE' importing msg = zmsg.
*     zmsgtab-zmsg = zmsg.
*     append zmsgtab to itab.
*
*    endif.
*  endloop.
*
*endif.
*
*enddo.
ENDLOOP.
perform close_group.
*perform close_dataset using dataset.

*loop at itab into zmsgtab.
* write: / zmsgtab-zmsg.
* endloop.
