*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM15I_6013_TOP
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
*&----------------shiva declaration.
data: w_inper like mdst-inper.
data: begin of wa_marc,
       matnr like marc-matnr,
       werks like marc-werks,
       meins like mara-meins,
       end of wa_marc.
data: wa_mdsu like mdsu.
data: begin of wa_mat_ltp,
        matnr like marc-matnr,
        meins like mara-meins,
        mng01 like mdsu-mng01,
        mng02 like mdsu-mng01,
        mng03 like mdsu-mng01,
        mng04 like mdsu-mng01,
        mng05 like mdsu-mng01,
        mng06 like mdsu-mng01,
      end of wa_mat_ltp.
data: begin of wa_ausp,
        objek like ausp-objek,
        atwrt like ausp-atwrt,
        atnam like cabn-atnam,
      end of wa_ausp.
data: begin of wa_mdsu1.
        include structure mdsu.
data:   matnr like mara-matnr,
      end of wa_mdsu1.
data: it_mdsu1 like table of wa_mdsu1.

data: begin of wa_mat_info,
        matnr like mara-matnr,
        mprop(20) type c,
        mcoat(6)  type c,
        mthick(8) type c,
        mwidth(5) type c,
        mleng(5)  type c,
        minout    type c,
      end of wa_mat_info.

data: it_marc like table of wa_marc,
      it_mdsu like table of wa_mdsu,
      it_mat_ltp like table of wa_mat_ltp,
      it_mat_ltp1 like table of wa_mat_ltp,
      it_ausp like table of wa_ausp,
      it_mat_info like table of wa_mat_info.
data: wa_fieldcat1 type line of slis_t_fieldcat_alv,
      wa_layout type slis_layout_alv,
      wa_events1 type slis_alv_event,
      it_events type slis_t_event.

data: w_stock like mdsu-mng01,
      w_menge like mdsu-mng01,
      w_1st_date like sy-datum,
      w_2nd_date like sy-datum,
      w_3rd_date like sy-datum,
      w_4th_date like sy-datum,
      w_5th_date like sy-datum,
      w_6th_date like sy-datum,
      w_mon1(6)  type c,
      w_mon2(6)  type c,
      w_mon3(6)  type c,
      w_mon4(6)  type c,
      w_mon5(6)  type c,
      w_mon6(6)  type c,
      w_prev_matnr like marc-matnr.

field-symbols: <fs_mat> like line of it_marc,
               <fs_mdsu> like line of it_mdsu,
               <fs_ltp> like line of it_mat_ltp.

*&-------------Constants.
constants: c_perkz like mdsu-perkz value 'M'.
*&----------------End of Shiva declaration.

* alv Definition
data:  w_fieldcat     TYPE   slis_t_fieldcat_alv,
       wa_fieldcat    LIKE   LINE OF w_fieldcat,
       wa_events     TYPE   slis_t_event,
       it_fieldcat    TYPE   slis_t_fieldcat_alv
                             WITH HEADER LINE,
       it_fieldcat1    TYPE   slis_t_fieldcat_alv,
       it_list_top_of_page
                      TYPE   slis_t_listheader,
       c_formname_top_of_page
                      TYPE   slis_formname VALUE 'TOP_OF_PAGE',
       w_repid        LIKE   sy-repid.
*       col_pos        TYPE   i.

*/ Itab & wa for ZTMM_6013_01
DATA: BEGIN OF wa_ztmm_6013_02,
        zmark   type c.
        INCLUDE STRUCTURE ztmm_6013_01.
DATA: END OF wa_ztmm_6013_02.
data: wa_ztmm_6013_01 like ztmm_6013_01.
data: it_ztmm_6013_01 like table of wa_ztmm_6013_01,
      it_ztmm_6013_02 like table of wa_ztmm_6013_02.

FIELD-SYMBOLS: <fs_ztmm_6013_01> LIKE LINE OF it_ztmm_6013_01.

**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
CONSTANTS: c_dest(10) VALUE 'WMRM01'.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.

*/For interface log
DATA: wa_ztca_if_log LIKE ztca_if_log.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME  TITLE text-bl1.
PARAMETERS: p_date LIKE sy-datum
                   DEFAULT sy-datum
                   OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl_1.
