*&---------------------------------------------------------------------*
*& Include MZPP_PRO_DAILY_SUMMARYTOP                                   *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzpp_pro_daily_summary    MESSAGE-ID zmpp .
*Tables declation
TABLES :  ztpp_pro_sum.
*Types
*Internal table declation
*plan & actual
DATA : BEGIN OF it_act OCCURS 0,
        model LIKE ztpp_pro_sum-model,"Model
        mann  LIKE ztpp_pro_sum-rp01, "Month-plan annual
        mope  LIKE ztpp_pro_sum-rp01, "Month-plan oper
        yplan LIKE ztpp_pro_sum-rp01, "Yesterday plan
        yact  LIKE ztpp_pro_sum-rp01, "Yesterday-actual
        ydiff LIKE ztpp_pro_sum-rp01, "Yesterday-diff
        splan LIKE ztpp_pro_sum-rp01, "Summary Month
        sact  LIKE ztpp_pro_sum-rp01, "Summary actual
        sdiff LIKE ztpp_pro_sum-rp01, "Summary difference
        reqty LIKE ztpp_pro_sum-rp01, "Remain Qty
        tplan LIKE ztpp_pro_sum-rp01, "Today plan
        twip  LIKE ztpp_pro_sum-rp01, "T/R WIP
       END OF it_act.

*Wip
DATA : BEGIN OF it_wip OCCURS 0,
        model LIKE ztpp_pro_sum-model,
        dqty  LIKE ztpp_pro_sum-rp01,   "Delay Prod Qty
        dtime LIKE ztpp_pro_sum-dtime,
        drate(3) TYPE p DECIMALS 2,     "Delay Prod rate
        cretu LIKE ztpp_pro_sum-rp01,   "control gate return
        cpass LIKE ztpp_pro_sum-rp01,   "control gate passed
        vpc   LIKE ztpp_pro_sum-rp01,   "vpc
        truck LIKE ztpp_pro_sum-rp01,   "trucking
        rai   LIKE ztpp_pro_sum-rp01,   "railing
        sqty  LIKE ztpp_pro_sum-rp01,   "sales qty
        shmaw LIKE ztpp_pro_sum-rp01,   "sales hma wip
        sfut  LIKE ztpp_pro_sum-rp01,   "sales future use
        ysum  LIKE ztpp_pro_sum-rp01,   "2004 summary
       END OF it_wip.
* Begin of changes - UD1K919014
   DATA : BEGIN OF it_act1 OCCURS 0,
            opt(2)    type c,
            model LIKE ztpp_pro_sum-model,"Model
            mann  LIKE ztpp_pro_sum-rp01, "Month-plan annual
            mope  LIKE ztpp_pro_sum-rp01, "Month-plan oper
            yplan LIKE ztpp_pro_sum-rp01, "Yesterday plan
            yact  LIKE ztpp_pro_sum-rp01, "Yesterday-actual
            ydiff LIKE ztpp_pro_sum-rp01, "Yesterday-diff
            splan LIKE ztpp_pro_sum-rp01, "Summary Month
            sact  LIKE ztpp_pro_sum-rp01, "Summary actual
            sdiff LIKE ztpp_pro_sum-rp01, "Summary difference
            reqty LIKE ztpp_pro_sum-rp01, "Remain Qty
            tplan LIKE ztpp_pro_sum-rp01, "Today plan
            twip  LIKE ztpp_pro_sum-rp01, "T/R WIP
          END OF it_act1.
* End of changes - UD1K919014

DATA : BEGIN OF it_wtsum OCCURS 0,
        opsec LIKE zsmm_working_time-opsec,
       END OF it_wtsum,
       BEGIN OF it_wdsum OCCURS 0,
        datum LIKE zsmm_working_time-datum,
        opsec LIKE zsmm_working_time-opsec,
       END OF it_wdsum,
        p_char_status LIKE  cabn-atnam ,
        p_i_atwrt_s LIKE ausp-atwrt,
        p_i_atwrt_e LIKE ausp-atwrt.

DATA  :it_condition LIKE zsca_characteristic_value OCCURS 0
                         WITH HEADER LINE,
       it_value  LIKE  zsca_char_value  OCCURS 0
                         WITH HEADER LINE,
       it_vehicle LIKE zsca_vehicle_char_value OCCURS 0
                         WITH HEADER LINE,
       it_vehi LIKE it_vehicle OCCURS 0 WITH HEADER LINE.
*Data declation
DATA : ok_code LIKE sy-ucomm,
       p_plant LIKE marc-werks VALUE 'P001',
       p_date  LIKE sy-datum ,"VALUE sy-datum,
       p_rp TYPE z_prp,
       n_date  LIKE sy-datum,
       n_time  LIKE ztpp_pro_sum-erzet,
       z_atinn LIKE cawn-atinn,
       y_date  LIKE sy-datum,
       w_int TYPE i,
       p_flag .
DATA : z_mann LIKE it_act-mann,
       z_mope LIKE it_act-mope,
       z_yplan LIKE it_act-yplan,
       z_yact  LIKE it_act-yact,
       z_ydiff LIKE it_act-ydiff,
       z_splan LIKE it_act-splan,
       z_sact  LIKE it_act-sact,
       z_sdiff LIKE it_act-sdiff,
       z_reqty LIKE it_act-reqty,
       z_tplan LIKE it_act-tplan,
       z_twip  LIKE it_act-twip,
       t_count TYPE i,
       p_atwrt_s LIKE ausp-atwrt,
       p_atwrt_e LIKE ausp-atwrt,
       z_dtime LIKE ztpp_pro_sum-dtime,
       z_dqty LIKE ztpp_pro_sum-dqty,
       p_dtime(2).

RANGES : s_date FOR sy-datum.

*declaration of tablecontrol 'IT_C101'
CONTROLS: it_c101 TYPE TABLEVIEW USING SCREEN 0100.

*declaration of tablecontrol 'IT_CONT2'
CONTROLS: it_cont2 TYPE TABLEVIEW USING SCREEN 0100.
*Text edotor
CONSTANTS: line_length TYPE i VALUE 500.

DATA : editor TYPE REF TO cl_gui_textedit,
       textedit_custom_container TYPE REF TO cl_gui_custom_container,
       repid LIKE sy-repid,
       relink TYPE c,                   " to manage relinking
       mytable(line_length) TYPE c OCCURS 0,
*            "TYPE STREAM_TABLE,
       mycontainer(30) TYPE c,          " string for the containers
       container_linked TYPE i.                             "#EC NEEDED
* " container to which control is linked
DATA : WA_FORMNAME TYPE TDSFNAME  VALUE 'ZPP_PROD_PLAN',
       WA_FORMNAME1 TYPE TDSFNAME  VALUE 'ZPP_PROD_PLAN01'.


* necessary to flush the automation queue
CLASS cl_gui_cfw DEFINITION LOAD.
DATA : itftext     TYPE tline OCCURS 0.
DATA : name LIKE thead-tdname.
