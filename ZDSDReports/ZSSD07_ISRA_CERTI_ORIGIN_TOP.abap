*----------------------------------------------------------------------*
*   INCLUDE ZSSD06U_HAC_COMMERCIAL_INV_TOP                             *
*----------------------------------------------------------------------*
TABLES : vbrk,    "Billing: Header Data
         vbrp,    "Billing: Item Data
         vbkd,    "Sales Document: Business Data
         likp,    "SD Document: Delivery Header Data
         lips,    "SD document: Delivery: Item data
         vbak,    "Sales Document: Header Data
         vbpa,    "Sales Document: Partner
         tvrot, kna1, t005t, nast,
         ausp, ztbm_abxopvdt, cuvtab_valc, cuvtab.

DATA : wa_isra_certi_origin TYPE zssd_isra_certi_origin.

DATA : BEGIN OF wa_list ,
       fkdat  LIKE vbrk-fkdat,
       vbeln  LIKE vbrk-vbeln,
       berot  LIKE likp-berot,
       year(20),
       make(20),
       body(20),
       btgew TYPE i,                      "Shipping Weight
       gewei LIKE likp-gewei,
       hp(20),
       gvwr   LIKE ztpp_tech-tech_004,
       cyls(4),                            "No. CYLS
       arktx LIKE vbrp-arktx,
       model(100),
       name1  LIKE adrc-name1,
       name2  LIKE adrc-name2,
       city1  LIKE adrc-city1,
       post_code1  LIKE adrc-post_code1,

       END OF wa_list.

DATA : wa_ausp TYPE ausp.

DATA : w_price LIKE vbrp-netwr,
       w_first(1),
       w_cnt(2) TYPE n,
       w_edate(11),
       w_edate2(11),
       w_chk(1),
       w_sign(1),
       h9(10),
       h9_d LIKE sy-datum.


* smart form
DATA : func_mod_name TYPE rs38l_fnam.
DATA : control_parameters TYPE ssfctrlop.
DATA : output_options TYPE ssfcompop.
DATA : job_output_info TYPE ssfcrescl.
DATA : w_spoolid TYPE tsfspoolid WITH HEADER LINE.


DATA : pdf LIKE tline OCCURS 100 WITH HEADER LINE.
DATA : numbytes TYPE i.
DATA : srcspoolid LIKE tsp01-rqident.
DATA : pdfspoolid LIKE tsp01-rqident.
DATA : w_file LIKE rlgrap-filename.
