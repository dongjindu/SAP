************************************************************************
* Program Name      : ZIMMGM29I_6026
* Author            : Hakchin Kim
* Creation Date     : 2004.02.04.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : IMMGM29
* Addl Documentation: F/S - IMMGM29 FIFOProcessing(Outbound)
* Description       : SAP MM -> External SYSTEM(FTZ)
*
* Modification Logs
* Date        Developer    RequestNo    Description
* 07/22/2004  Shiva        UD1K911573   Stop exploding BOM for IPPC &
*                                       IPIM once again for materials
*                                       already done. Also for XPIM
*                                       don't filter the selection
*                                       with Production scheduler(SEA).
* 08/02/2004  Shiva        UD1K911712
*                       1. If 5th and 6th charc. of FSC material is
*                          'XX' or 'XY' then don't get data for
*                          movement type '261'.
*                       2. Need to sum the Usage qty. of sub components
*                          (all entries)in the BOM explosion.
*                       3. To explode BOM for engine check whether
*                          the material belongs to the plant 'E100'.
*                       4. If mat. type is 'HALB' and profl is 'K' then
*                          group it under trxn. code 'IPPC'.
*                       5. Don't retrive '262' movement type data for
*                          the trxn. code 'INPC' and 'INIM'.
* 08/19/2004  Shiva        UD1K911949   Add txncode for movement type
*                                  '991' as 'XPIM' and '992' as 'XNIM'.
* 09/07/2004  Shiva        UD1K912145    Assign status code by checking
*                                        whether Import Request exists.
* 09/21/2004  Shiva       UD1K912268   For FTZ transaction code 'RPPC' &
*                                     'APPC' get the unit price from the
*                                      condition of the material info
*                                      record for validity date in the
*                                      material info record[EINE-DATLB].
************************************************************************
REPORT zimmgm29i_6026 MESSAGE-ID zmmm NO STANDARD PAGE HEADING
                                      LINE-SIZE 400.

INCLUDE zimmgm29i_6026top.   "Data Declaration
INCLUDE zimmgm29i_6026cla.   "Class Part
INCLUDE zimmgm29i_6026o01.   "PBO Part
INCLUDE zimmgm29i_6026i01.   "PAI Part
INCLUDE zimmgm29i_6026f01.   "Perform Library
*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.
*/General
*  s_budat-low    = sy-datum - 1.
**  s_BUDAT-high   = sy-datum - 1.
*  s_budat-sign   = 'I'.
*  s_budat-option = 'BT'.
*  APPEND s_budat.
  p_budat = sy-datum - 1.

*/Adjusting
  s_budat-low    = sy-datum - 8.
  s_budat-high   = sy-datum - 1.
  s_budat-sign   = 'I'.
  s_budat-option = 'BT'.
  APPEND s_budat.

*/ processed before the selection screen is displayed.
AT SELECTION-SCREEN OUTPUT.
  IF rb1     = 'X'.   "General
    LOOP AT SCREEN.
      CHECK screen-group1 = 'M02'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF rb2 = 'X'.   "Adjusting
    LOOP AT SCREEN.
      CHECK screen-group1 = 'M01'.
      screen-active = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

*/ Check of selection criterion S_BUDAT
AT SELECTION-SCREEN ON s_budat.
* Upper Limit cannot be empty.
  IF s_budat-high IS INITIAL.
    MESSAGE e999(zmmm) WITH 'Upper Limit cannot be empty !'.
  ENDIF.

* Multiple Line Posting Date Not Allowed
  DESCRIBE TABLE s_budat LINES w_lines.
  IF w_lines > 1.
    MESSAGE e999(zmmm) WITH 'Multiple Line Posting Date'
                                         'Not Allowed !'.
  ENDIF.

*Posting Date Option must be 'Between'
  LOOP AT s_budat.
    IF s_budat-option <> 'BT'.
      MESSAGE e999(zmmm) WITH 'Posting Date Option must be'
                                            '''Between'' !'.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  PERFORM get_present_date_time.
  IF rb1     = 'X'.   "General
    PERFORM get_data.    "get data
  ELSEIF rb2 = 'X'.   "Adjusting
    PERFORM get_data_adjusting.  "Get Data for Adjusting
  ENDIF.

  IF it_ztmm_6026_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.

*/Refine Data.
  PERFORM refine_data.

*/Process Data
  PERFORM process_data_by_section.  "Process Data by Section
*  PERFORM z_fca_eai_interface_log. "I commented this for performance.
  IF sy-batch IS INITIAL.   "Not Backgroung Processing
    PERFORM dsp_log.        "Display Data Log
  ELSE.
    MESSAGE s999(zmmm) WITH 'Application Doc. No.'
                                          w_zdocno
                                    'is created !'.
  ENDIF.
*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
*  PERFORM make_col_heading.
