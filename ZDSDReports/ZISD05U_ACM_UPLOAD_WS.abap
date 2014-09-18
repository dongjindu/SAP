************************************************************************
* Program Name      : ZISD05U_ACM_UPLOAD_BG
* Author            : jun ho choi
* Creation Date     : 2003.07.08.
* Specifications By : jun ho choi
* Pattern           : 5-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Uploading ACM files and storage within SAP
*                          Costum Tables.
*
* Modification Logs
* Date       Developer        RequestNo    Description
* 08/07/2006 Haseeb Mohammad  UD1K921643    Make the system read file *
*from UNIX not from desktop, as these files are now send by HISNA via *
* FTP.
* 03/21/2007 Manju            UD1K940140    Option to upload Puerto rico
*                                           claim file from desktop
************************************************************************
REPORT zisd05u_acm_upload NO STANDARD PAGE HEADING
                          MESSAGE-ID zmsd.


*
TABLES : ztsd_acm_h,
         ztsd_acm_i,
         ztsd_acl_l,
         ztsd_vin_conv,
         cabn,
         ausp,
         knvv.

TYPE-POOLS slis .

DATA : BEGIN OF it_upfile OCCURS 0,
       record(431),
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

** for overview data
*DATA: BEGIN OF W_OV_ACM,
*      ZACLN LIKE ZTSD_ACM_H-ZACLN,
*      ZCDST LIKE ZTSD_ACM_H-ZCDST,
*      ZCSTS LIKE ZTSD_ACM_H-ZCSTS,
*      ZSBSS LIKE ZTSD_ACM_H-ZSBSS,
*      ZRMSS LIKE ZTSD_ACM_H-ZRMSS,
*      ZSBPP LIKE ZTSD_ACM_H-ZSBPP,
*      ZRMPP LIKE ZTSD_ACM_H-ZRMPP,
*      ZSBLL LIKE ZTSD_ACM_H-ZSBLL,
*      ZRMLL LIKE ZTSD_ACM_H-ZRMLL,
*      ZSBTT LIKE ZTSD_ACM_H-ZSBSS,
*      ZRMTT LIKE ZTSD_ACM_H-ZRMSS,
*      END OF W_OV_ACM.
*DATA: IT_OV_ACM LIKE TABLE OF W_OV_ACM WITH HEADER LINE.

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
PARAMETERS : deskfile AS CHECKBOX DEFAULT 'X',
             p_puerto AS CHECKBOX.                       "UD1K940140
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : p_rate TYPE p DECIMALS 4.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS : p_yymm(6) TYPE n OBLIGATORY .
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  p_file = ''.
  p_path = '/usr/sap/EDI_SAP/'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
* commented by Haseb Mohammad UD1K921643.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'. "UD1K940140

START-OF-SELECTION.
  PERFORM check_yymm.
  IF w_check IS INITIAL.
    PERFORM upload_file.
    PERFORM storage_acm_tables.
** Changed by Furong on 07/18/08 adding ACL overview
    PERFORM get_overview_data.
  ENDIF.
*  PERFORM MODIFY_ACM_TABLE.
*  IF DESKFILE EQ 'X'.
*    PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.
*    PERFORM DOWNLOAD_FILE.
*  ENDIF.
*
** End of change
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM upload_file.


* UD1K921643 Commented by Haseeb Mohammad on 08/07/2006.
  IF  p_puerto  EQ 'X' .                                 "UD1K940140
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
*       BAD_DATA_FORMAT  = 6
        OTHERS           = 9.
*  IF SY-SUBRC NE 0.
*     S_ERROR = 'X'.
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
* ENDIF.
** Change by Haseeb Mohammad is fished here.


*
*  DATA : DSN(30).
*
*  IMPORT IT_LIST FROM DATABASE INDX(ZS) ID VARIANT.
*
*  READ TABLE IT_LIST INDEX 1.
*
*  DSN = IT_LIST-FILE.
*
*  OPEN DATASET DSN IN TEXT MODE FOR INPUT.
*
*  DO.
*    READ DATASET DSN INTO IT_UPFILE-RECORD.
*    IF SY-SUBRC = 0.
*      APPEND IT_UPFILE.
*    ELSE.
**      IF SY-SUBRC = 4.
**        WRITE:/ IT_UPFILE-RECORD.
*        EXIT.
**      ENDIF.
*    ENDIF.
*  ENDDO.
*
*  CLOSE DATASET DSN.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE I000 WITH TEXT-M02 GT_MESSAGE-MSG.
*    STOP.
*  ENDIF.

* Added by Haseeb Mohammad on 08/07/2006 UD1K921643
* Reads the file on UNIX.
  ELSE.                                                   "UD1K940140
    CONCATENATE p_path p_file INTO p_file.
    wa_file = p_file.
    OPEN DATASET p_file IN TEXT MODE.
    IF sy-subrc <> 0.
      WRITE: / 'File Not Found', sy-subrc.
      STOP.
    ENDIF.
    WHILE sy-subrc = 0.
      READ DATASET p_file INTO it_upfile.
      IF sy-subrc = 0.
        APPEND it_upfile.
      ENDIF.
    ENDWHILE.
    CLOSE DATASET p_file.
  ENDIF.                                                    "UD1K940140
** Adding Complete here by Haseeb Mohammad UD1K921643.

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
    CASE it_upfile-record+38(1). "CLAIM TYPE
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
  IF w_acln <> it_upfile-record+14(7) OR
     w_cdst <> it_upfile-record+21(5) OR
     w_cdlr <> it_upfile-record+26(5) OR
     w_cser <> it_upfile-record+31(6).

    it_acm_h-zcsts = it_upfile-record+12(2).   "ACM STATUS
    it_acm_h-zacln = it_upfile-record+14(7).   "ACL NUMBER
    it_acm_h-zcdst = it_upfile-record+21(5).   "DISTRIBUTOR CODE

    IF it_upfile-record+21(3) = 'B28'.   "DISTRIBUTOR CODE
      SELECT SINGLE *
             FROM knvv
            WHERE kunnr EQ it_upfile-record+21(5)
            AND   vkorg EQ 'D100'
            AND   vtweg EQ '40'
            AND   spart EQ '99'.
    ELSE.
      SELECT SINGLE *
             FROM knvv
            WHERE kunnr EQ it_upfile-record+21(5)
            AND   vkorg EQ 'E100'
            AND   vtweg EQ '40'
            AND   spart EQ '99'.
    ENDIF.

    it_acm_h-zpycr = knvv-waers.               "PAYMENT CURRENCY
    it_acm_h-zcdlr = it_upfile-record+26(5).   "DEALER CODE
    it_acm_h-zcser = it_upfile-record+31(6).   "SERIAL NUMBER
    it_acm_h-zrsfg = it_upfile-record+37(1).   "PROCESSING FLAG
    it_acm_h-zctyp = it_upfile-record+38(1).   "CLAIM TYPE
    it_acm_h-zvin  = it_upfile-record+39(17).  "VEHICLE NUMBER
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
    it_acm_h-zvsfg = it_upfile-record+56(1).   "VISITING OWNER
    IF it_upfile-record+57(8) = '99999999'.
      it_acm_h-zdlvy = '19000101'.             "DELIVERY DATE
    ELSE.
      it_acm_h-zdlvy = it_upfile-record+57(8). "DELIVERY DATE
    ENDIF.
    it_acm_h-zrpdt = it_upfile-record+65(8).   "REPAIR DATE
    it_acm_h-zodrd = it_upfile-record+73(6).   "ODOMETER
    it_acm_h-zronm = it_upfile-record+79(10).  "REPAIR ORDER NUMBER
    it_acm_h-zcptn = it_upfile-record+97(15).  "CAUSAL PART NUMBER
    it_acm_h-znatr = it_upfile-record+127(3).  "NATURE CODE
    it_acm_h-zcaus = it_upfile-record+130(3).  "CAUSE CODE
    it_acm_h-zpwt1 = it_upfile-record+133(1).  "PWA TYPE CODE - 1
    it_acm_h-zpwt2 = it_upfile-record+134(1).  "PWA TYPE CODE - 2
    it_acm_h-zpwno = it_upfile-record+135(12). "PWA NUMBER
    it_acm_h-zsbla = it_upfile-record+147(2).  "SUBLET TYPE CODE - 1
    it_acm_h-zsblb = it_upfile-record+149(2).  "SUBLET TYPE CODE - 2
    it_acm_h-zdesc = it_upfile-record+168(30). "CONDITION DECSRIPTION

    IF it_upfile-record+198(2) = '01'.  "LINE OF CLAIM
      it_acm_h-zpcau = it_upfile-record+257(3).  "PAINT CAUSE CODE

      it_acm_h-zsbss = it_upfile-record+288(7). "SUBLET AMOUNT-SUBMITTED
      it_acm_h-zsbss = it_acm_h-zsbss / 100.                "DECIMALS 2
      it_acm_h-zrmss = it_upfile-record+295(7).  "SUBLET AMOUNT-APPROVED
      it_acm_h-zrmss = it_acm_h-zrmss / 100.                "DECIMALS 2
    ENDIF.

    it_acm_h-zpidt = it_upfile-record+302(8).  "PREVIOUS R/O DATE
    it_acm_h-zpodr = it_upfile-record+310(6).  "PREVIOUS ODOMETER
    it_acm_h-zpron = it_upfile-record+316(10). "PREVIOUS R/O NUMBER
    it_acm_h-zadjh = it_upfile-record+326(45). "ERROR MESSAGE - HEADER

    APPEND it_acm_h. CLEAR it_acm_h.

    w_acln = it_upfile-record+14(7).
    w_cdst = it_upfile-record+21(5).
    w_cdlr = it_upfile-record+26(5).
    w_cser = it_upfile-record+31(6).

  ENDIF.

* DETAIL
  it_acm_i-zacln = it_upfile-record+14(7).   "ACL NUMBER
  it_acm_i-zcdst = it_upfile-record+21(5).   "DISTRIBUTOR CODE

  IF it_upfile-record+21(3) = 'B28'.   "DISTRIBUTOR CODE
    SELECT SINGLE *
           FROM knvv
          WHERE kunnr EQ it_upfile-record+21(5)
          AND   vkorg EQ 'D100'
          AND   vtweg EQ '40'
          AND   spart EQ '99'.
  ELSE.
    SELECT SINGLE *
           FROM knvv
          WHERE kunnr EQ it_upfile-record+21(5)
          AND   vkorg EQ 'E100'
          AND   vtweg EQ '40'
          AND   spart EQ '99'.
  ENDIF.

  it_acm_i-zpycr = knvv-waers.               "PAYMENT CURRENCY
  it_acm_i-zcdlr = it_upfile-record+26(5).   "DEALER CODE
  it_acm_i-zcser = it_upfile-record+31(6).   "SERIAL NUMBER
  it_acm_i-zline = it_upfile-record+198(2).  "LINE OF CLAIM
  it_acm_i-zrppn = it_upfile-record+200(15). "RPLCMNT. PART NUMBER
  it_acm_i-zsbpq = it_upfile-record+215(2).  "RPLCMNT. PART QUANTITY-S
  it_acm_i-zrmpq = it_upfile-record+217(2).  "RPLCMNT. PART QUANTITY-A
  it_acm_i-zsbpu = it_upfile-record+219(7).  "RPLCMNT. PART UNIT PRICE-S
  it_acm_i-zsbpu = it_acm_i-zsbpu / 100.                    "DECIMALS 2
  it_acm_i-zrmpu = it_upfile-record+226(7).  "RPLCMNT. PART UNIT PRICE-A
  it_acm_i-zrmpu = it_acm_i-zrmpu / 100.                    "DECIMALS 2
  it_acm_i-zrmup = it_upfile-record+233(3).  "PART MARK UP
  it_acm_i-zoper = it_upfile-record+236(8).  "LABOR OPERATION CODE
  it_acm_i-zsblq = it_upfile-record+244(1).  "LABOR OPERATION QUANTITY-S
  it_acm_i-zrmlq = it_upfile-record+245(1).  "LABOR OPERATION QUANTITY-A
  it_acm_i-zsblt = it_upfile-record+246(3).  "LABOR OPERATION HOUR-SUBM
  it_acm_i-zrmlt = it_upfile-record+249(3).  "LABOR OPERATION HOUR-APPR
  it_acm_i-zlrat = it_upfile-record+252(5).  "LABOR RATE
  it_acm_i-zpcau = it_upfile-record+257(3).  "PAINT CAUSE CODE
  it_acm_i-zsbpp = it_upfile-record+260(7).  "PART AMOUNT - SUBMITTED
  it_acm_i-zsbpp = it_acm_i-zsbpp / 100.                    "DECIMALS 2
  it_acm_i-zrmpp = it_upfile-record+267(7).  "PART AMOUNT - APPROVED
  it_acm_i-zrmpp = it_acm_i-zrmpp / 100.                    "DECIMALS 2
  it_acm_i-zsbll = it_upfile-record+274(7).  "LABOR AMOUNT - SUBMITTED
  it_acm_i-zsbll = it_acm_i-zsbll / 100.                    "DECIMALS 2
  it_acm_i-zrmll = it_upfile-record+281(7).  "LABOR AMOUNT - APPROVED
  it_acm_i-zrmll = it_acm_i-zrmll / 100.                    "DECIMALS 2
  it_acm_i-zadjd = it_upfile-record+371(30). "ERROR MESSAGE - DETAIL

  APPEND it_acm_i. CLEAR it_acm_i.
ENDFORM.                    " PROCESS_WP
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CP
*&---------------------------------------------------------------------*
FORM process_cp. "HEADER
  it_acm_h-zcsts = it_upfile-record+12(2).   "ACM STATUS
  it_acm_h-zacln = it_upfile-record+14(7).   "ACL NUMBER
  it_acm_h-zcdst = it_upfile-record+21(5).   "DISTRIBUTOR CODE

  IF it_upfile-record+21(3) = 'B28'.   "DISTRIBUTOR CODE
    SELECT SINGLE *
           FROM knvv
          WHERE kunnr EQ it_upfile-record+21(5)
          AND   vkorg EQ 'D100'
          AND   vtweg EQ '40'
          AND   spart EQ '99'.
  ELSE.
    SELECT SINGLE *
           FROM knvv
          WHERE kunnr EQ it_upfile-record+21(5)
          AND   vkorg EQ 'E100'
          AND   vtweg EQ '40'
          AND   spart EQ '99'.
  ENDIF.

  it_acm_h-zpycr = knvv-waers.               "PAYMENT CURRENCY
  it_acm_h-zcdlr = it_upfile-record+26(5).   "DEALER CODE
  it_acm_h-zcser = it_upfile-record+31(6).   "SERIAL NUMBER
  it_acm_h-zrsfg = it_upfile-record+37(1).   "PROCESSING FLAG
  it_acm_h-zctyp = it_upfile-record+38(1).   "CLAIM TYPE
  it_acm_h-zcseq = it_upfile-record+39(2).   "LINE NUMBER
  it_acm_h-zvin  = it_upfile-record+41(17).  "VEHICLE NUMBER
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
  it_acm_h-zvsfg = it_upfile-record+58(1).   "VISITING OWNER
  IF it_upfile-record+59(8) = '99999999'.
    it_acm_h-zdlvy = '19000101'.             "DELIVERY DATE
  ELSE.
    it_acm_h-zdlvy = it_upfile-record+59(8). "DELIVERY DATE
  ENDIF.
  it_acm_h-zrpdt = it_upfile-record+67(8).   "REPAIR DATE
  it_acm_h-zodrd = it_upfile-record+75(6).   "ODOMETER
  it_acm_h-zcpis = it_upfile-record+81(6).   "CAMPAIGN ISSUE NUMBER+0(6)
  it_acm_h-zmnop = it_upfile-record+81(8).   "CAMPAIGN ISSUE NUMBER+0(8)
  it_acm_h-zsbpp = it_upfile-record+89(7).   "PART AMOUNT - SUBMITTED
  it_acm_h-zsbpp = it_acm_h-zsbpp / 100.                    "DECIMALS 2
  it_acm_h-zrmpp = it_upfile-record+96(7).   "PART AMOUNT - APPROVED
  it_acm_h-zrmpp = it_acm_h-zrmpp / 100.                    "DECIMALS 2
  it_acm_h-zsbll = it_upfile-record+103(7).  "LABOR AMOUNT - SUBMITTED
  it_acm_h-zsbll = it_acm_h-zsbll / 100.                    "DECIMALS 2
  it_acm_h-zrmll = it_upfile-record+110(7).  "LABOR AMOUNT - APPROVED
  it_acm_h-zrmll = it_acm_h-zrmll / 100.                    "DECIMALS 2
  it_acm_h-zsbss = it_upfile-record+117(7).  "SUBLET AMOUNT - SUBMITTED
  it_acm_h-zsbss = it_acm_h-zsbss / 100.                    "DECIMALS 2
  it_acm_h-zrmss = it_upfile-record+124(7).  "SUBLET AMOUNT - APPROVED
  it_acm_h-zrmss = it_acm_h-zrmss / 100.                    "DECIMALS 2
  it_acm_h-zadjh = it_upfile-record+231(45). "ERROR MESSAGE

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

** Changed by Fuorng on 07/18/08 for 'Z'.
*  check not IT_ACM_H-ZACLN CP '*M*'.
    IF it_acm_h-zacln CP '*M*' OR it_acm_h-zacln CP '*Z*'.
    ELSE.
** End of change
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
      ENDIF.

      IF it_acm_h-zmodl IS INITIAL AND
         it_acm_h-zcsts EQ 'AA'.
        MESSAGE i000 WITH text-m04
                          '('
                          it_acm_h-zvin
                          ')'.
        w_exit = 'Y'.
      ENDIF.
** Added by Furong on 07/18/08
    ENDIF.
** End of addition
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
    IF it_acm_h-zctyp = 'C'.
      it_acl_l-zctyp = 'C'.
    ELSE.
      it_acl_l-zctyp = 'W'.
    ENDIF.

    it_acl_l-zacaa = it_acl_l-zacaa
                   + it_acm_h-zrmss
                   + it_acm_h-zrmpp
                   + it_acm_h-zrmll.
** Changed by Furong on 03/12/10
    it_acl_l-zym = p_yymm.
** End of change
    COLLECT it_acl_l. CLEAR it_acl_l.
  ENDLOOP.
  LOOP AT it_acl_l.
    it_acl_l_idx = sy-tabix.
*UD1K923292
    LOOP AT it_acm_h WHERE zacln EQ it_acl_l-zacln
                     AND   zcdst EQ it_acl_l-zcdst
                     AND   ( zcsts EQ 'AA' OR zcsts EQ 'JJ').
      it_acl_l-zacqt = it_acl_l-zacqt + 1.
    ENDLOOP.
*UD1K923292
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

** Changed on 02/01/12 - check vehicle exist or not
*    CHECK SY-SUBRC = 0.
*    SELECT SINGLE *
*           FROM CABN
*          WHERE ATNAM = 'P_MODEL'.
*    SELECT SINGLE *
*           FROM AUSP
*          WHERE KLART = '002'
*          AND   OBJEK EQ AUSP-OBJEK
*          AND   ATINN EQ CABN-ATINN.
*
*    CHECK SY-SUBRC = 0.
*    IT_VM-ZSCOD = AUSP-ATWRT.
*    APPEND IT_VM. CLEAR IT_VM.
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
* Changed by chris on 06/15/2005
* INSERT ZTSD_ACM_H FROM TABLE IT_ACM_H.
  MODIFY ztsd_acm_h FROM TABLE it_acm_h.
* end of change
  IF sy-subrc = 0.
    IF it_acm_h-zctyp <> 'C'.
* Changed by chris on 06/15/2005
*      INSERT ZTSD_ACM_I FROM TABLE IT_ACM_I.
      MODIFY ztsd_acm_i FROM TABLE it_acm_i.
* end of change
    ENDIF.
    IF sy-subrc = 0.
* Changed by chris on 06/15/2005
*      INSERT ZTSD_ACL_L FROM TABLE IT_ACL_L.
      MODIFY ztsd_acl_l FROM TABLE it_acl_l.
* end of change
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
*  DATA: BEGIN OF DYNPFIELDS OCCURS 3.
*          INCLUDE STRUCTURE DYNPREAD.
*  DATA: END OF DYNPFIELDS.
*
*  DATA : L_DYNAME LIKE SY-REPID,
*         L_DYNUMB LIKE SY-DYNNR.
*
*  DYNPFIELDS-FIELDNAME = 'P_PUERTO'.
*  APPEND DYNPFIELDS.
*  L_DYNAME = SY-REPID.
*  L_DYNUMB = sy-DYNNR.
**  L_DYNUMB = '1000'.
*
*  CALL FUNCTION 'DYNP_VALUES_READ'
*       EXPORTING
*            DYNAME     = L_DYNAME
*            DYNUMB     = L_DYNUMB
*       TABLES
*            DYNPFIELDS = DYNPFIELDS.
*
**read dynpfields index 1.
*  CHECK DYNPFIELDS-FIELDVALUE EQ 'X'.
  CHECK p_puerto EQ 'X'.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = sy-cprog
      dynpro_number = sy-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = tmp_filename.
  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
    MESSAGE e000 WITH 'FILE SELECT WINDOW OPEN ERROR!'.
  ENDIF.
ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
* Fucntion Added by Haseeb Mohammad UD1K921643, to give a back up copy
*of ACM file to USER for comparision from server data.
*----------------------------------------------------------------------*
FORM download_file.


  CALL FUNCTION 'WS_DOWNLOAD'
   EXPORTING
*     BIN_FILESIZE                  = ' '
*     CODEPAGE                      = ' '
      filename                      = p_file
*     FILETYPE                      = 'ASC'
*     MODE                          = ' '
*     WK1_N_FORMAT                  = ' '
*     WK1_N_SIZE                    = ' '
*     WK1_T_FORMAT                  = ' '
*     WK1_T_SIZE                    = ' '
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*    IMPORTING
*     FILELENGTH                    =
    TABLES
      data_tab                      = it_upfile
*     FIELDNAMES                    =
    EXCEPTIONS
      file_open_error               = 1
      file_write_error              = 2
      invalid_filesize              = 3
      invalid_type                  = 4
      no_batch                      = 5
      unknown_error                 = 6
      invalid_table_width           = 7
      gui_refuse_filetransfer       = 8
      customer_error                = 9
      OTHERS                        = 10
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MESSAGE i000 WITH 'FILE SAVED ON LOCAL DESKTOP DRIVE!'.


ENDFORM.                    " DOWNLOAD_FILE
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
*  SORT IT_OV_ACM BY ZCDST ZACLN ZCSTS.
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
*    PERFORM sssign_event_9000.
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
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
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
    EXPORTING
      i_parent      = grid_container
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
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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
*        lw_waers LIKE t001-waers,

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
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_ov_acm[]
      it_sort              = it_sort[].

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
  IF deskfile EQ 'X'.
    PERFORM at_sel_screen_on_value_request USING p_file 'O'.
    PERFORM download_file.
  ENDIF.
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
