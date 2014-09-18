************************************************************************
* Program Name      : ZESD11R_BILLING_EXPORT
* Author            : jun ho choi
* Creation Date     : 2004.02.05.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No :
* Addl Documentation:
* Description       : BILLING EXPORT
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 04.15.2014 Victor        copied from ZESD11R_BILLING_HAC
*
*
************************************************************************
REPORT zesd11r_billing_export NO STANDARD PAGE HEADING
                           MESSAGE-ID zmsd
                           LINE-SIZE 170.


*
TABLES : vkdfs, likp, lips, vbak, tvrot, vbss,
         cabn, ausp, usr01.


*
DATA : BEGIN OF it_list OCCURS 0,
       lifex LIKE likp-lifex,   "Ext. delivery : RC #
       route LIKE likp-route,   "Route : DESTINATION
       bezei LIKE tvrot-bezei,  "DESTINATION
       vbeln LIKE likp-vbeln,   "Delivery document
       vbeln_b LIKE vbrk-vbeln, "Billing document
       matnr LIKE lips-matnr,   "Material
       arktx LIKE lips-arktx,   "Description
       lfimg LIKE lips-lfimg,   "Delivery qty
       vrkme LIKE lips-vrkme,   "Sales unit
       netwr LIKE vbak-netwr,   "Net value
       waerk LIKE vbak-waerk,   "Doc. currency
       vbelv LIKE lips-vbelv,   "Originating doc
       WADAT_IST like likp-WADAT_IST, "Actual Goods Movement Date
       END OF it_list.

DATA : BEGIN OF it_list_h OCCURS 0,
       flag(1),                 "CHECK BOX
       lifex LIKE likp-lifex,   "Ext. delivery : RC #
       END OF it_list_h.

DATA : BEGIN OF it_list_m OCCURS 0,
       lifex LIKE likp-lifex,   "Ext. delivery : RC #
       route LIKE likp-route,   "Route : DESTINATION
       bezei LIKE tvrot-bezei,  "DESTINATION
       END OF it_list_m.

DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

RANGES : r_kunag FOR likp-kunag.

DATA : ok_code(4),
       save_ok_code(4).

DATA : www(1) VALUE 'N', "BDC MODE
       w_cnt TYPE i,
       w_tabix LIKE sy-tabix,
       w_flag(1),
       w_lifex LIKE likp-lifex,
       w_field(15),
       w_value(10),
       w_result(1),
       w_sammg LIKE vbsk-sammg,
       w_c_8_gi(8).


*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_fkdat FOR vkdfs-fkdat OBLIGATORY
                                         NO-EXTENSION NO INTERVALS
                                         DEFAULT sy-datum.
PARAMETERS : p_nation LIKE ztsd_um-wo_nation OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


*
TOP-OF-PAGE.
  PERFORM top_of_page.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.


*
START-OF-SELECTION.
  SET PF-STATUS 'ESD11R'.
  PERFORM check_input_data.
  PERFORM get_data.


*
  PERFORM display_data.

END-OF-SELECTION.



*
AT USER-COMMAND.
  PERFORM user_command.


*
AT LINE-SELECTION.
  PERFORM display_doc.


*
  INCLUDE zesd11l_billing_export_f01.
