************************************************************************
* Program Name      : ZISD05U_ACM_UPLOAD_WS_HMC
* Author            : Valerian Utama
* Creation Date     : 11/23/2010
* Specifications By : Jeetendra Kumar Virwani
* Pattern           :
* Dev Request       :
* Addl Documentation:
* Description       : Uploading ACM files and store in SAP Custom
*                     Tables.
*                     Copy from program ZISD05U_ACM_UPLOAD_WS
*                     and modified based on the requirement.
*
************************************************************************
* Modification Logs
* Date       Developer        RequestNo    Description
* 11/23/2010 Valerian Utama   UD1K950319   Initial Program Development
*
************************************************************************
REPORT zisd05u_acm_upload_ws_hmc NO STANDARD PAGE HEADING
                                 MESSAGE-ID zmsd.

TABLES : ztsd_acm_h,
         ztsd_acm_i,
         ztsd_acl_l,
         ztsd_vin_conv,
         cabn,
         ausp,
         knvv.

TYPE-POOLS slis .

DATA : BEGIN OF it_upfile OCCURS 0,
       record(438),
       END OF it_upfile.

DATA : BEGIN OF it_acm_h OCCURS 0.
        INCLUDE STRUCTURE ztsd_acm_h.
DATA : END OF it_acm_h.

DATA : BEGIN OF it_acm_i OCCURS 0.
        INCLUDE STRUCTURE ztsd_acm_i.
DATA : END OF it_acm_i.

DATA : BEGIN OF it_acl_l OCCURS 0.
        INCLUDE STRUCTURE ztsd_acl_l.
DATA : END OF it_acl_l.

DATA : BEGIN OF it_vm OCCURS 0,
       zvin  LIKE it_acm_h-zvin,
       zscod LIKE it_acm_h-zscod,
       END OF it_vm.

DATA : w_acln LIKE ztsd_acm_h-zacln,
       w_cdst LIKE ztsd_acm_h-zcdst,
       w_cdlr LIKE ztsd_acm_h-zcdlr,
       w_cser LIKE ztsd_acm_h-zcser.

DATA : w_cnt TYPE i,
       w_exit(1),
       w_n_8(8) TYPE n.

DATA: BEGIN OF gt_message OCCURS 0,
       msg(100),
      END OF gt_message.
*
DATA : variant LIKE indx-srtfd VALUE 'ISD05_01'.

DATA : BEGIN OF it_list OCCURS 0,
       file LIKE rlgrap-filename,
       END OF it_list.
DATA:  wa_file LIKE rlgrap-filename.
DATA:  p_path  LIKE filenameci-pathintern.

DATA: BEGIN OF it_ov_acm OCCURS 0,
      zcdst LIKE ztsd_acm_h-zcdst,
      zacln LIKE ztsd_acm_h-zacln,
      zcsts LIKE ztsd_acm_h-zcsts,
      zsbss LIKE ztsd_acm_h-zsbss,
      zrmss LIKE ztsd_acm_h-zrmss,
      zsbpp LIKE ztsd_acm_h-zsbpp,
      zrmpp LIKE ztsd_acm_h-zrmpp,
      zsbll LIKE ztsd_acm_h-zsbll,
      zrmll LIKE ztsd_acm_h-zrmll,
      zsbtt LIKE ztsd_acm_h-zsbss,
      zrmtt LIKE ztsd_acm_h-zrmss,
      zsbtt_con LIKE ztsd_acm_h-zsbss,
      zrmtt_con LIKE ztsd_acm_h-zrmss,
      zym   LIKE ztsd_acl_l-zym,
      END OF it_ov_acm.
DATA: w_ov_acm LIKE it_ov_acm.


** For ALV display
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
      w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid.

DATA: w_check(1).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_file LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : p_rate TYPE p DECIMALS 4.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS : p_yymm(6) TYPE n OBLIGATORY .
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.
  PERFORM check_yymm.
  IF w_check IS INITIAL.
    PERFORM upload_file.
    PERFORM storage_acm_tables.
    PERFORM get_overview_data.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM upload_file.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            filename         = p_file
            filetype         = 'DAT'
       TABLES
            data_tab         = it_upfile
       EXCEPTIONS
            conversion_error = 1
            file_open_error  = 2
            file_read_error  = 3
            invalid_type     = 4
            no_batch         = 5
            OTHERS           = 9.

  IF sy-subrc NE 0.
    IF sy-subrc = 1.
      gt_message-msg = 'FILE UPLOAD CONVERSION ERROR'.
      APPEND gt_message.
    ELSEIF sy-subrc = 2.
      gt_message-msg = 'UPLOAD FILE OPEN ERROR'.
      APPEND gt_message.
    ELSEIF sy-subrc = 3.
      gt_message-msg = 'UPLOAD FILE READ ERROR'.
      APPEND gt_message.
    ELSEIF sy-subrc = 4.
      gt_message-msg = 'INVALID DATA TYPE!'.
      APPEND gt_message.
    ENDIF.
    MESSAGE i000 WITH text-m02 gt_message-msg.
    STOP.
  ENDIF.

ENDFORM.                    " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  STORAGE_ACM_TABLES
*&---------------------------------------------------------------------*
FORM storage_acm_tables.
  DESCRIBE TABLE it_upfile LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m03.
    STOP.
  ENDIF.

  REFRESH : it_acm_h, it_acm_i, it_acl_l.
  CLEAR   : it_acm_h, it_acm_i, it_acl_l.

  LOOP AT it_upfile.
*   Warranty, Campaign For PC Format
    CASE it_upfile-record+45(1). "CLAIM TYPE
      WHEN 'C'.
        PERFORM process_cp.
      WHEN OTHERS.
        PERFORM process_wp.
    ENDCASE.
  ENDLOOP.

  PERFORM modify_it.
ENDFORM.                    " STORAGE_ACM_TABLES
*&---------------------------------------------------------------------*
*&      Form  PROCESS_WP
*&---------------------------------------------------------------------*
FORM process_wp. "HEADER & DETAIL
* HEADER
  IF w_acln <> it_upfile-record+14(10) OR
     w_cdst <> it_upfile-record+24(5) OR
     w_cdlr <> it_upfile-record+29(5) OR
     w_cser <> it_upfile-record+34(10).

    it_acm_h-zcsts = it_upfile-record+12(2).   "ACM STATUS
    it_acm_h-zacln = it_upfile-record+14(10).  "ACL NUMBER
    it_acm_h-zcdst = it_upfile-record+24(5).   "DISTRIBUTOR CODE

* Testing by Daniel and Jeetu
*    IF it_upfile-record+24(3) = 'B28'.   "DISTRIBUTOR CODE
*      SELECT SINGLE *
*             FROM knvv
*            WHERE kunnr EQ it_upfile-record+24(5)
*            AND   vkorg EQ 'D100'
*            AND   vtweg EQ '40'
*            AND   spart EQ '99'.
*    ELSE.
*      SELECT SINGLE *
*             FROM knvv
*            WHERE kunnr EQ it_upfile-record+24(5)
*            AND   vkorg EQ 'E100'
*            AND   vtweg EQ '40'
*            AND   spart EQ '99'.
*    ENDIF.

    SELECT SINGLE *
             FROM knvv
            WHERE kunnr EQ it_upfile-record+24(5)
              AND vtweg EQ '40'
              AND spart EQ '99'.
* Testing by Daniel and Jeetu


    it_acm_h-zpycr = knvv-waers.               "PAYMENT CURRENCY
    it_acm_h-zcdlr = it_upfile-record+29(5).   "DEALER CODE
    it_acm_h-zcser = it_upfile-record+34(10).  "SERIAL NUMBER
    it_acm_h-zrsfg = it_upfile-record+44(1).   "PROCESSING FLAG
    it_acm_h-zctyp = it_upfile-record+45(1).   "CLAIM TYPE
    it_acm_h-zvin  = it_upfile-record+46(17).  "VEHICLE NUMBER
**
    SELECT SINGLE *
           FROM cabn
          WHERE atnam = 'P_VIN'.
    SELECT SINGLE *
           FROM ausp
          WHERE klart = '002'
          AND   atinn EQ cabn-atinn
          AND   atwrt EQ it_acm_h-zvin.

    SELECT SINGLE *
           FROM cabn
          WHERE atnam = 'P_RP18_SHOP_DATE'.
    SELECT SINGLE *
           FROM ausp
          WHERE klart = '002'
          AND   objek EQ ausp-objek
          AND   atinn EQ cabn-atinn.

    w_n_8 = ausp-atflv.
    it_acm_h-zprdt = w_n_8.
**
    it_acm_h-zvsfg = it_upfile-record+63(1).   "VISITING OWNER
    IF it_upfile-record+64(8) = '99999999'.
      it_acm_h-zdlvy = '19000101'.             "DELIVERY DATE
    ELSE.
      it_acm_h-zdlvy = it_upfile-record+64(8). "DELIVERY DATE
    ENDIF.
    it_acm_h-zrpdt = it_upfile-record+72(8).   "REPAIR DATE
    it_acm_h-zodrd = it_upfile-record+80(6).   "ODOMETER
    it_acm_h-zronm = it_upfile-record+86(10).  "REPAIR ORDER NUMBER
    it_acm_h-zcptn = it_upfile-record+104(15). "CAUSAL PART NUMBER
    it_acm_h-znatr = it_upfile-record+134(3).  "NATURE CODE
    it_acm_h-zcaus = it_upfile-record+137(3).  "CAUSE CODE
    it_acm_h-zpwt1 = it_upfile-record+140(1).  "PWA TYPE CODE - 1
    it_acm_h-zpwt2 = it_upfile-record+141(1).  "PWA TYPE CODE - 2
    it_acm_h-zpwno = it_upfile-record+142(12). "PWA NUMBER
    it_acm_h-zsbla = it_upfile-record+154(2).  "SUBLET TYPE CODE - 1
    it_acm_h-zsblb = it_upfile-record+156(2).  "SUBLET TYPE CODE - 2
    it_acm_h-zdesc = it_upfile-record+175(30). "CONDITION DECSRIPTION

    IF it_upfile-record+205(2) = '01'.  "LINE OF CLAIM
      it_acm_h-zpcau = it_upfile-record+264(3).  "PAINT CAUSE CODE

    it_acm_h-zsbss = it_upfile-record+295(7). "SUBLET AMOUNT - SUBMITTED
      it_acm_h-zsbss = it_acm_h-zsbss / 100.                "DECIMALS 2
    it_acm_h-zrmss = it_upfile-record+302(7).  "SUBLET AMOUNT - APPROVED
      it_acm_h-zrmss = it_acm_h-zrmss / 100.                "DECIMALS 2
    ENDIF.

    it_acm_h-zpidt = it_upfile-record+309(8).  "PREVIOUS R/O DATE
    it_acm_h-zpodr = it_upfile-record+317(6).  "PREVIOUS ODOMETER
    it_acm_h-zpron = it_upfile-record+323(10). "PREVIOUS R/O NUMBER
    it_acm_h-zadjh = it_upfile-record+333(75). "ERROR MESSAGE - HEADER

    it_acm_h-zplnt = it_upfile-record(5).      "CLAIM# - RECLAIM PLANT
    it_acm_h-zhacl = it_upfile-record+5(7).    "FILLER
    it_acm_h-zexch = it_upfile-record+411(6).  "CLAIM EX. RATE
    it_acm_h-zexch = it_acm_h-zexch / 10000.                "DECIMALS 4
    it_acm_h-zrmpc = it_upfile-record+417(7).  "PART AMT
    it_acm_h-zrmpc = it_acm_h-zrmpc / 100.                  "DECIMALS 2
    it_acm_h-zrmlc = it_upfile-record+424(7).  "LABOR AMT
    it_acm_h-zrmlc = it_acm_h-zrmlc / 100.                  "DECIMALS 2
    it_acm_h-zrmsc = it_upfile-record+431(7).  "SUBLET AMT
    it_acm_h-zrmsc = it_acm_h-zrmsc / 100.                  "DECIMALS 2

    APPEND it_acm_h. CLEAR it_acm_h.

    w_acln = it_upfile-record+14(10).
    w_cdst = it_upfile-record+24(5).
    w_cdlr = it_upfile-record+29(5).
    w_cser = it_upfile-record+34(10).

  ENDIF.

* DETAIL
  it_acm_i-zacln = it_upfile-record+14(10).  "ACL NUMBER
  it_acm_i-zcdst = it_upfile-record+24(5).   "DISTRIBUTOR CODE

  IF it_upfile-record+24(3) = 'B28'.   "DISTRIBUTOR CODE
    SELECT SINGLE *
           FROM knvv
          WHERE kunnr EQ it_upfile-record+24(5)
          AND   vkorg EQ 'D100'
          AND   vtweg EQ '40'
          AND   spart EQ '99'.
  ELSE.
    SELECT SINGLE *
           FROM knvv
          WHERE kunnr EQ it_upfile-record+24(5)
          AND   vkorg EQ 'E100'
          AND   vtweg EQ '40'
          AND   spart EQ '99'.
  ENDIF.

  it_acm_i-zpycr = knvv-waers.               "PAYMENT CURRENCY
  it_acm_i-zcdlr = it_upfile-record+29(5).   "DEALER CODE
  it_acm_i-zcser = it_upfile-record+34(10).  "SERIAL NUMBER
  it_acm_i-zline = it_upfile-record+205(2).  "LINE OF CLAIM
  it_acm_i-zrppn = it_upfile-record+207(15). "RPLCMNT. PART NUMBER
  it_acm_i-zsbpq = it_upfile-record+222(2).  "RPLCMNT. PART QUANTITY-S
  it_acm_i-zrmpq = it_upfile-record+224(2).  "RPLCMNT. PART QUANTITY-A
  it_acm_i-zsbpu = it_upfile-record+226(7).  "RPLCMNT. PART UNIT PRICE-S
  it_acm_i-zsbpu = it_acm_i-zsbpu / 100.                    "DECIMALS 2
  it_acm_i-zrmpu = it_upfile-record+233(7).  "RPLCMNT. PART UNIT PRICE-A
  it_acm_i-zrmpu = it_acm_i-zrmpu / 100.                    "DECIMALS 2
  it_acm_i-zrmup = it_upfile-record+240(3).  "PART MARK UP
  it_acm_i-zoper = it_upfile-record+243(8).  "LABOR OPERATION CODE
  it_acm_i-zsblq = it_upfile-record+251(1).  "LABOR OPERATION QUANTITY-S
  it_acm_i-zrmlq = it_upfile-record+252(1).  "LABOR OPERATION QUANTITY-A
  it_acm_i-zsblt = it_upfile-record+253(3).  "LABOR OPERATION HOUR-SUBM
  it_acm_i-zrmlt = it_upfile-record+256(3).  "LABOR OPERATION HOUR-APPR
  it_acm_i-zlrat = it_upfile-record+259(5).  "LABOR RATE
  it_acm_i-zpcau = it_upfile-record+264(3).  "PAINT CAUSE CODE
  it_acm_i-zsbpp = it_upfile-record+267(7).  "PART AMOUNT - SUBMITTED
  it_acm_i-zsbpp = it_acm_i-zsbpp / 100.                    "DECIMALS 2
  it_acm_i-zrmpp = it_upfile-record+274(7).  "PART AMOUNT - APPROVED
  it_acm_i-zrmpp = it_acm_i-zrmpp / 100.                    "DECIMALS 2
  it_acm_i-zsbll = it_upfile-record+281(7).  "LABOR AMOUNT - SUBMITTED
  it_acm_i-zsbll = it_acm_i-zsbll / 100.                    "DECIMALS 2
  it_acm_i-zrmll = it_upfile-record+288(7).  "LABOR AMOUNT - APPROVED
  it_acm_i-zrmll = it_acm_i-zrmll / 100.                    "DECIMALS 2
  it_acm_i-zadjd = it_upfile-record+333(75). "ERROR MESSAGE - DETAIL

  it_acm_i-zplnt = it_upfile-record(5).      "CLAIM# - RECLAIM PLANT
  it_acm_i-zhacl = it_upfile-record+5(7).    "FILLER
  it_acm_i-zexch = it_upfile-record+411(6).  "CLAIM EX. RATE
  it_acm_i-zexch = it_acm_i-zexch / 10000.                  "DECIMALS 4
  it_acm_i-zrmpc = it_upfile-record+417(7).  "PART AMT
  it_acm_h-zrmpc = it_acm_h-zrmpc / 100.                    "DECIMALS 2
  it_acm_i-zrmlc = it_upfile-record+424(7).  "LABOR AMT
  it_acm_h-zrmlc = it_acm_h-zrmlc / 100.                    "DECIMALS 2
  it_acm_i-zrmsc = it_upfile-record+431(7).  "SUBLET AMT
  it_acm_h-zrmsc = it_acm_h-zrmsc / 100.                    "DECIMALS 2

  APPEND it_acm_i. CLEAR it_acm_i.
ENDFORM.                    " PROCESS_WP
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CP
*&---------------------------------------------------------------------*
FORM process_cp. "HEADER
  it_acm_h-zcsts = it_upfile-record+12(2).   "ACM STATUS
  it_acm_h-zacln = it_upfile-record+14(10).  "ACL NUMBER
  it_acm_h-zcdst = it_upfile-record+24(5).   "DISTRIBUTOR CODE

* Testing by Daniel and Jeetu
*  IF it_upfile-record+24(3) = 'B28'.   "DISTRIBUTOR CODE
*    SELECT SINGLE *
*           FROM knvv
*          WHERE kunnr EQ it_upfile-record+24(5)
*          AND   vkorg EQ 'D100'
*          AND   vtweg EQ '40'
*          AND   spart EQ '99'.
*  ELSE.
*    SELECT SINGLE *
*           FROM knvv
*          WHERE kunnr EQ it_upfile-record+24(5)
*          AND   vkorg EQ 'E100'
*          AND   vtweg EQ '40'
*          AND   spart EQ '99'.
*  ENDIF.

  SELECT SINGLE *
         FROM knvv
        WHERE kunnr EQ it_upfile-record+24(5)
        AND   vtweg EQ '40'
        AND   spart EQ '99'.
* Testing by Daniel and Jeetu

  it_acm_h-zpycr = knvv-waers.               "PAYMENT CURRENCY
  it_acm_h-zcdlr = it_upfile-record+29(5).   "DEALER CODE
  it_acm_h-zcser = it_upfile-record+34(10).  "SERIAL NUMBER
  it_acm_h-zrsfg = it_upfile-record+44(1).   "PROCESSING FLAG
  it_acm_h-zctyp = it_upfile-record+45(1).   "CLAIM TYPE
  it_acm_h-zcseq = it_upfile-record+46(2).   "LINE NUMBER
  it_acm_h-zvin  = it_upfile-record+48(17).  "VEHICLE NUMBER
**
  SELECT SINGLE *
         FROM cabn
        WHERE atnam = 'P_VIN'.
  SELECT SINGLE *
         FROM ausp
        WHERE klart = '002'
        AND   atinn EQ cabn-atinn
        AND   atwrt EQ it_acm_h-zvin.

  SELECT SINGLE *
         FROM cabn
        WHERE atnam = 'P_RP18_SHOP_DATE'.
  SELECT SINGLE *
         FROM ausp
        WHERE klart = '002'
        AND   objek EQ ausp-objek
        AND   atinn EQ cabn-atinn.

  w_n_8 = ausp-atflv.
  it_acm_h-zprdt = w_n_8.
**
  it_acm_h-zvsfg = it_upfile-record+65(1).   "VISITING OWNER
  IF it_upfile-record+66(8) = '99999999'.
    it_acm_h-zdlvy = '19000101'.             "DELIVERY DATE
  ELSE.
    it_acm_h-zdlvy = it_upfile-record+66(8). "DELIVERY DATE
  ENDIF.
  it_acm_h-zrpdt = it_upfile-record+74(8).   "REPAIR DATE
  it_acm_h-zodrd = it_upfile-record+82(6).   "ODOMETER
  it_acm_h-zcpis = it_upfile-record+88(6).   "CAMPAIGN ISSUE NUMBER+0(6)
  it_acm_h-zmnop = it_upfile-record+88(8).   "CAMPAIGN ISSUE NUMBER+0(8)
  it_acm_h-zsbpp = it_upfile-record+96(7).   "PART AMOUNT - SUBMITTED
  it_acm_h-zsbpp = it_acm_h-zsbpp / 100.                    "DECIMALS 2
  it_acm_h-zrmpp = it_upfile-record+103(7).  "PART AMOUNT - APPROVED
  it_acm_h-zrmpp = it_acm_h-zrmpp / 100.                    "DECIMALS 2
  it_acm_h-zsbll = it_upfile-record+110(7).  "LABOR AMOUNT - SUBMITTED
  it_acm_h-zsbll = it_acm_h-zsbll / 100.                    "DECIMALS 2
  it_acm_h-zrmll = it_upfile-record+117(7).  "LABOR AMOUNT - APPROVED
  it_acm_h-zrmll = it_acm_h-zrmll / 100.                    "DECIMALS 2
  it_acm_h-zsbss = it_upfile-record+124(7).  "SUBLET AMOUNT - SUBMITTED
  it_acm_h-zsbss = it_acm_h-zsbss / 100.                    "DECIMALS 2
  it_acm_h-zrmss = it_upfile-record+131(7).  "SUBLET AMOUNT - APPROVED
  it_acm_h-zrmss = it_acm_h-zrmss / 100.                    "DECIMALS 2
  it_acm_h-zadjh = it_upfile-record+238(45). "ERROR MESSAGE

  it_acm_h-zplnt = it_upfile-record(5).      "CLAIM# - RECLAIM PLANT
  it_acm_h-zhacl = it_upfile-record+5(7).    "FILLER
  it_acm_h-zexch = it_upfile-record+186(6).  "CLAIM EX. RATE
  it_acm_h-zexch = it_acm_h-zexch / 10000.                  "DECIMALS 4
  it_acm_h-zrmpc = it_upfile-record+192(7).  "PART AMT
  it_acm_h-zrmpc = it_acm_h-zrmpc / 100.                    "DECIMALS 2
  it_acm_h-zrmlc = it_upfile-record+199(7).  "LABOR AMT
  it_acm_h-zrmlc = it_acm_h-zrmlc / 100.                    "DECIMALS 2
  it_acm_h-zrmsc = it_upfile-record+206(7).  "SUBLET AMT
  it_acm_h-zrmsc = it_acm_h-zrmsc / 100.                    "DECIMALS 2

  APPEND it_acm_h. CLEAR it_acm_h.
ENDFORM.                    " PROCESS_CP
*&---------------------------------------------------------------------*
*&      Form  MODIFY_IT
*&---------------------------------------------------------------------*
FORM modify_it.
  DATA : it_acm_h_idx LIKE sy-tabix,
         it_acl_l_idx LIKE sy-tabix.

* CALCULATE AND ADD UP THE LABOR AND PART AMOUNT
  LOOP AT it_acm_h.
    CHECK it_acm_h-zctyp <> 'C'.
    it_acm_h_idx = sy-tabix.

    LOOP AT it_acm_i WHERE zacln EQ it_acm_h-zacln
                     AND   zcdst EQ it_acm_h-zcdst
                     AND   zcdlr EQ it_acm_h-zcdlr
                     AND   zcser EQ it_acm_h-zcser.
      it_acm_h-zsbpp = it_acm_h-zsbpp + it_acm_i-zsbpp.
      it_acm_h-zrmpp = it_acm_h-zrmpp + it_acm_i-zrmpp.
      it_acm_h-zsbll = it_acm_h-zsbll + it_acm_i-zsbll.
      it_acm_h-zrmll = it_acm_h-zrmll + it_acm_i-zrmll.
    ENDLOOP.

    MODIFY it_acm_h INDEX it_acm_h_idx.
  ENDLOOP.

* DETERMINE THE VEHICLE MODEL CODE USING VIN
  PERFORM make_vm.

  LOOP AT it_acm_h.
    it_acm_h_idx = sy-tabix.

* BY pass VIN number check if ACL number contains character
* 'M' in it.

*   IF it_acm_h-zacln CP '*M*' OR it_acm_h-zacln CP '*Z*'.  "VALERIAN
*   ELSE.                                                   "VALERIAN
    IF it_acm_h-zvin IS INITIAL.
      SELECT SINGLE
             *
             FROM ztsd_vin_conv
            WHERE zvin EQ 'DUMMY'.

      IF sy-subrc = 0.
        it_acm_h-zscod = ztsd_vin_conv-zscod.
        it_acm_h-zmodl = ztsd_vin_conv-zmodl.

        MODIFY it_acm_h INDEX it_acm_h_idx.
      ENDIF.
    ELSE.
      READ TABLE it_vm WITH KEY zvin = it_acm_h-zvin.

      IF sy-subrc = 0.
        SELECT *
               FROM ztsd_vin_conv
              WHERE zscod EQ it_vm-zscod.

          TRANSLATE ztsd_vin_conv-zvin USING '*+'.

          IF it_acm_h-zvin CP ztsd_vin_conv-zvin.
            it_acm_h-zscod = ztsd_vin_conv-zscod.
            it_acm_h-zmodl = ztsd_vin_conv-zmodl.

            MODIFY it_acm_h INDEX it_acm_h_idx.
            EXIT.
          ENDIF.
        ENDSELECT.
      ENDIF.
*     ENDIF.                                                "VALERIAN

      IF it_acm_h-zmodl IS INITIAL AND
         it_acm_h-zcsts EQ 'AA'.
        MESSAGE i000 WITH text-m04
                          '('
                          it_acm_h-zvin
                          ')'.
        w_exit = 'Y'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF w_exit = 'Y'.
    EXIT.
  ENDIF.

* MAIN OPERATION CODE
  LOOP AT it_acm_h.
    CHECK it_acm_h-zctyp <> 'C'.
    it_acm_h_idx = sy-tabix.

    READ TABLE it_acm_i WITH KEY zacln = it_acm_h-zacln
                                 zcdst = it_acm_h-zcdst
                                 zcdlr = it_acm_h-zcdlr
                                 zcser = it_acm_h-zcser
                                 zline = '01'.
    IF sy-subrc = 0.
      it_acm_h-zmnop = it_acm_i-zoper.
    ENDIF.

    MODIFY it_acm_h INDEX it_acm_h_idx.
  ENDLOOP.

* DETERMINE SECOND CLAIM FLAG
  LOOP AT it_acm_h.
    CHECK it_acm_h-zctyp <> 'C' AND
          it_acm_h-zctyp <> 'S' AND
          it_acm_h-zpodr <> '000000'.
    it_acm_h_idx = sy-tabix.

    it_acm_h-zscfg = '2'.

    MODIFY it_acm_h INDEX it_acm_h_idx.
  ENDLOOP.

* STORE ACL LIST TABLE
  LOOP AT it_acm_h.
    it_acl_l-zacln = it_acm_h-zacln.
    it_acl_l-zcdst = it_acm_h-zcdst.
    it_acl_l-zpycr = it_acm_h-zpycr.

    it_acl_l-zplnt = it_acm_h-zplnt.
    it_acl_l-zhacl = it_acm_h-zhacl.

    IF it_acm_h-zctyp = 'C'.
      it_acl_l-zctyp = 'C'.
    ELSE.
      it_acl_l-zctyp = 'W'.
    ENDIF.

    it_acl_l-zacaa = it_acl_l-zacaa
                   + it_acm_h-zrmss
                   + it_acm_h-zrmpp
                   + it_acm_h-zrmll.

    it_acl_l-zym = p_yymm.
    COLLECT it_acl_l. CLEAR it_acl_l.
  ENDLOOP.

  LOOP AT it_acl_l.
    it_acl_l_idx = sy-tabix.
    LOOP AT it_acm_h WHERE zacln EQ it_acl_l-zacln
                     AND   zcdst EQ it_acl_l-zcdst
                     AND   ( zcsts EQ 'AA' OR zcsts EQ 'JJ' ).
      it_acl_l-zacqt = it_acl_l-zacqt + 1.
    ENDLOOP.
    MODIFY it_acl_l INDEX it_acl_l_idx.
  ENDLOOP.
ENDFORM.                    " MODIFY_IT
*&---------------------------------------------------------------------*
*&      Form  MAKE_VM
*&---------------------------------------------------------------------*
FORM make_vm.
  REFRESH it_vm. CLEAR it_vm.

  LOOP AT it_acm_h.
    it_vm-zvin = it_acm_h-zvin.

    SELECT SINGLE *
           FROM cabn
          WHERE atnam = 'P_VIN'.
    SELECT SINGLE *
           FROM ausp
          WHERE klart = '002'
          AND   atinn EQ cabn-atinn
          AND   atwrt EQ it_acm_h-zvin.

** Changed on 02/07/12 - check vehicle exist or not
*    CHECK sy-subrc = 0.
*    SELECT SINGLE *
*           FROM cabn
*          WHERE atnam = 'P_MODEL'.
*    SELECT SINGLE *
*           FROM ausp
*          WHERE klart = '002'
*          AND   objek EQ ausp-objek
*          AND   atinn EQ cabn-atinn.
*    CHECK sy-subrc = 0.
*    it_vm-zscod = ausp-atwrt.
*    APPEND it_vm. CLEAR it_vm.

  IF sy-subrc = 0.
      SELECT SINGLE *
             FROM cabn
            WHERE atnam = 'P_MODEL'.
      SELECT SINGLE *
             FROM ausp
            WHERE klart = '002'
            AND   objek EQ ausp-objek
            AND   atinn EQ cabn-atinn.
      CHECK sy-subrc = 0.
      it_vm-zscod = ausp-atwrt.
      APPEND it_vm. CLEAR it_vm.
    ELSE.
      SELECT SINGLE model_code INTO it_vm-zscod
      FROM ztpp_vm
      WHERE vin = it_acm_h-zvin.
      IF sy-subrc = 0.
        APPEND it_vm. CLEAR it_vm.
      ENDIF.
    ENDIF.
** End on 02/07/12
  ENDLOOP.
ENDFORM.                    " MAKE_VM
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ACM_TABLE
*&---------------------------------------------------------------------*
FORM modify_acm_table.
  CHECK w_exit NE 'Y'.

* CHECK DUPLICATE
  READ TABLE it_acl_l INDEX 1.
  SELECT SINGLE *
         FROM ztsd_acl_l
        WHERE zacln EQ it_acm_h-zacln
        AND   zcdst EQ it_acm_h-zcdst.
  IF sy-subrc = 0.
    MESSAGE i000 WITH text-m05.
    EXIT.
  ENDIF.

  LOOP AT it_acm_h.
    it_acm_h-zerda = sy-datum.
    it_acm_h-erzet = sy-uzeit.
    it_acl_l-ernam = sy-uname.
    MODIFY it_acm_h INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_acm_i.
    it_acm_i-zerda = sy-datum.
    it_acm_i-erzet = sy-uzeit.
    it_acl_l-ernam = sy-uname.
    MODIFY it_acm_i INDEX sy-tabix.
  ENDLOOP.

  LOOP AT it_acl_l.
    it_acl_l-zerda = sy-datum.
    it_acl_l-erzet = sy-uzeit.
    it_acl_l-ernam = sy-uname.
    MODIFY it_acl_l INDEX sy-tabix.
  ENDLOOP.

  MODIFY ztsd_acm_h FROM TABLE it_acm_h.

  IF sy-subrc = 0.
    IF it_acm_h-zctyp <> 'C'.
      MODIFY ztsd_acm_i FROM TABLE it_acm_i.
    ENDIF.
    IF sy-subrc = 0.
      MODIFY ztsd_acl_l FROM TABLE it_acl_l.
    ENDIF.
  ENDIF.

  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE i000 WITH text-m07.
    LEAVE SCREEN.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i000 WITH text-m08.
  ENDIF.
ENDFORM.                    " MODIFY_ACM_TABLE

*---------------------------------------------------------------------*
*       FORM at_sel_screen_on_value_request                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DEF_PATH                                                      *
*  -->  MODE                                                          *
*---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: v_file_table TYPE filetable,
        v_rc TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
  EXPORTING
    window_title            = 'Input File'
    initial_directory       = 'C:\'
  CHANGING
    file_table              = v_file_table
    rc                      = v_rc
  EXCEPTIONS
    file_open_dialog_failed = 1
    cntl_error              = 2
    error_no_gui            = 3
    OTHERS                  = 4.

  READ TABLE v_file_table INDEX 1 INTO p_file.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  get_overview_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_overview_data.

  DATA: l_index LIKE sy-tabix.

  LOOP AT it_acm_h.
    MOVE-CORRESPONDING it_acm_h TO it_ov_acm.
    it_ov_acm-zym = p_yymm.
    COLLECT it_ov_acm.
    CLEAR: it_ov_acm.
  ENDLOOP.

  LOOP AT it_ov_acm.
    l_index = sy-tabix.
    it_ov_acm-zsbtt = it_ov_acm-zsbss + it_ov_acm-zsbpp +
                      it_ov_acm-zsbll.
    it_ov_acm-zrmtt = it_ov_acm-zrmss + it_ov_acm-zrmpp +
                      it_ov_acm-zrmll.
    IF it_ov_acm-zcdst+0(3) = 'B06'.
      it_ov_acm-zsbtt_con = it_ov_acm-zsbtt * p_rate.
      it_ov_acm-zrmtt_con = it_ov_acm-zrmtt * p_rate.
    ELSE.
      it_ov_acm-zsbtt_con = it_ov_acm-zsbtt.
      it_ov_acm-zrmtt_con = it_ov_acm-zrmtt.
    ENDIF.

    MODIFY it_ov_acm INDEX l_index TRANSPORTING zsbtt zrmtt
                                                zsbtt_con zrmtt_con.
    w_ov_acm-zsbss = w_ov_acm-zsbss + it_ov_acm-zsbss.
    w_ov_acm-zrmss = w_ov_acm-zrmss + it_ov_acm-zrmss.
    w_ov_acm-zsbpp = w_ov_acm-zsbpp + it_ov_acm-zsbpp.
    w_ov_acm-zrmpp = w_ov_acm-zrmpp + it_ov_acm-zrmpp.
    w_ov_acm-zsbll = w_ov_acm-zsbll + it_ov_acm-zsbll.
    w_ov_acm-zrmll = w_ov_acm-zrmll + it_ov_acm-zrmll.
    w_ov_acm-zsbtt = w_ov_acm-zsbtt + it_ov_acm-zsbtt.
    w_ov_acm-zrmtt = w_ov_acm-zrmtt + it_ov_acm-zrmtt.
    w_ov_acm-zsbtt_con = w_ov_acm-zsbtt_con + it_ov_acm-zsbtt_con.
    w_ov_acm-zrmtt_con = w_ov_acm-zrmtt_con + it_ov_acm-zrmtt_con.
  ENDLOOP.
  APPEND w_ov_acm TO it_ov_acm.

  CALL SCREEN 200.
ENDFORM.                    " get_overview_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_OV_ACM'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
          EXPORTING container_name = wa_custom_control
          EXCEPTIONS
           cntl_error = 1
           cntl_system_error = 2
           create_error = 3
           lifetime_error = 4
           lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
         EXPORTING i_parent = grid_container
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'CT'.
* wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2039   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'ZCDST'       ' ',
                                  ' ' 'COLTEXT'     'Dist Code',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZACLN'  ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'ACL Number',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZCSTS'       ' ',
                                  ' ' 'COLTEXT'     'Status',
                                  'E' 'OUTPUTLEN'   '8',


                                  'S' 'ZSBSS'       ' ',
                                  ' ' 'COLTEXT'     'Sublet-SB',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZRMSS'         ' ',
                                  ' ' 'COLTEXT'     'Sublet-AP',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZSBPP'       ' ',
                                  ' ' 'COLTEXT'     'Part-SB',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZRMPP'         ' ',
                                  ' ' 'COLTEXT'     'Part-AP',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZSBLL'       ' ',
                                  ' ' 'COLTEXT'     'Labor-SB',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZRMLL'         ' ',
                                  ' ' 'COLTEXT'     'Labor-AP',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZSBTT'       ' ',
                                  ' ' 'COLTEXT'     'Total-SB',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ZRMTT'         ' ',
                                  ' ' 'COLTEXT'     'Total-AP',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ZSBTT_CON'       ' ',
                                  ' ' 'COLTEXT'     'USD Tot-SB',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ZRMTT_CON'         ' ',
                                  ' ' 'COLTEXT'     'USD Tot-AP',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ZYM'         ' ',
                                  ' ' 'COLTEXT'     'ACM Month',
                                  'E' 'OUTPUTLEN'   '8'.


ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_ov_acm[]
               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'UPDATE'.
      PERFORM update.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update.
  PERFORM modify_acm_table.
ENDFORM.                    " UPDATE
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_2172   text
*      -->P_2173   text
*      -->P_2174   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.

ENDFORM.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  check_yymm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_yymm.
  IF p_yymm+4(2) > 13.
    w_check = 'X'.
    MESSAGE i000(zz) WITH 'Wrong Month'.
  ENDIF.
ENDFORM.                    " check_yymm
