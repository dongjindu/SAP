*&--------------------------------------------------------------------
*& Author                 : hs.jeong
*& Creation Date          : 09/04/2003
*& Specification By       : hs.jeong
*& Pattern                : Report 1-1
*& Development Request No : UD1K902152
*& Addl documentation     :
*& Description            : IO-FM Order Commitment Assing
*&
*& Modification Log
*& Date     Developer      Request ID   Description
*& ....      ANDY
*&
* 12/21/05 Manjunath       UD1K918739   Added input parameter
*                                       to FM Z_FFI_GET_IM_ASSIGN
*                                       to fix problem of help request
*                                       5C5C325A34
* 12/22/05 Manjunath       UD1K918704   Spelling correction on selection
*                                       screen
* 08/19/10 Valerian        UD1K949686   Add button to release Internal
*                                       order
* 12/14/11 KDM             UD1K953547   FM Derivation Block
* 03/06/12 Valerian        UD1K954177   Change requesting cost center
*                                       to Resp cost center
*&-------------------------------------------------------------------
REPORT zcfii04           MESSAGE-ID zmfi
                         NO STANDARD PAGE HEADING
                         LINE-SIZE   200   LINE-COUNT   90.


INCLUDE : <icon>,
          <symbol>.

TABLES : impr.
DATA : BEGIN OF it_bpja OCCURS 0.
        INCLUDE STRUCTURE bpja.
DATA : END OF it_bpja.

DATA : BEGIN OF it_bpge OCCURS 0.
        INCLUDE STRUCTURE bpge.
DATA : END OF it_bpge.
*-----IMZO
DATA : BEGIN OF it_imzo OCCURS 0.
        INCLUDE STRUCTURE imzo.
DATA : posid LIKE impr-posid,
       END OF it_imzo.
*----- IMPR
DATA : BEGIN OF it_impr OCCURS 0.
        INCLUDE STRUCTURE impr.
DATA : END OF it_impr.
*---- PI Master
DATA : BEGIN OF it_bpja2 OCCURS 0.
        INCLUDE STRUCTURE bpja.
DATA : END OF it_bpja2.
*-----------------------------------
DATA : BEGIN OF it_bpge2 OCCURS 0.
        INCLUDE STRUCTURE bpge.
DATA : END OF it_bpge2.
*----PI
DATA : BEGIN OF it_pi OCCURS 0,
         posid    LIKE  impr-posid,
         gjahr    LIKE  impr-gjahr,
         prnam    LIKE  impr-prnam,
         amt1     LIKE  bpja-wtjhr,   "Original budget
         amt2     LIKE  bpja-wtjhr,   "Current bud
         amt3     LIKE  bpja-wtjhr,   " Assign
         amt4     LIKE  bpja-wtjhr,   "Available
         amt5     LIKE  bpja-wtjhr,   "I/O Plan
         amt6     LIKE  bpja-wtjhr,   "Order budget after
         amt9     LIKE  bpja-wtjhr,   "IO credit
*         rate     LIKE  imzo-prozu,
        icon(4),
       END OF it_pi.
*-------------------------------------*
DATA : BEGIN OF it_pi1 OCCURS 0,
         posid    LIKE  impr-posid,
         prnam    LIKE  impr-prnam,
       END OF it_pi1.
*--------------------------*
DATA : BEGIN OF it_pi2 OCCURS 0,
         posid    LIKE  impr-posid,
         gjahr    LIKE  impr-gjahr,
         prnam    LIKE  impr-prnam,
         amt1     LIKE  bpja-wtjhr,   "Original budget
         amt2     LIKE  bpja-wtjhr,   "Current bud
         amt3     LIKE  bpja-wtjhr,   " Assign
         amt4     LIKE  bpja-wtjhr,   "Available
         amt5     LIKE  bpja-wtjhr,   "I/O Plan
*         amt6     LIKE  bpja-wtjhr,   "Order budget
         amt9     LIKE  bpja-wtjhr,   "IO credit
         rate     LIKE  imzo-prozu,
        icon(4),
        sign(2),
       END OF it_pi2.
*----------RATE
DATA : BEGIN OF it_rate OCCURS 0,
         posnr    LIKE  impr-posnr,
         posid    LIKE  impr-posid,
         prozu    LIKE  imzo-prozu,
       END OF it_rate.
*----
DATA : BEGIN OF it_assign OCCURS 0.
        INCLUDE STRUCTURE zfi_im_assign.
DATA : END OF it_assign.

DATA : BEGIN OF it_order OCCURS 0.
        INCLUDE STRUCTURE zfi_im_assign.
DATA : END OF it_order.
*--------------------------*
DATA : it_actual LIKE zfi_io_actual OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_act_sum OCCURS 0,
          gjahr LIKE zfi_io_actual-gjahr,
          tot   LIKE zfi_io_actual-tot,
       END OF it_act_sum.

*---------------------------------------*
DATA : wa_vorga LIKE bpja-vorga,
       wa_objnr LIKE aufk-objnr,
       wa_kostv LIKE aufk-kostv,   "resp.CC
       wa_subrc LIKE sy-subrc.
DATA : wa_year    LIKE bpja-gjahr.
DATA : wa_year1   LIKE bpja-gjahr,
       wa_year2   LIKE bpja-gjahr,
       wa_year3   LIKE bpja-gjahr,
       wa_year4   LIKE bpja-gjahr,
       wa_year5   LIKE bpja-gjahr,
       wa_year6   LIKE bpja-gjahr,
       wa_year7   LIKE bpja-gjahr,
       wa_year8   LIKE bpja-gjahr,
       wa_year9   LIKE bpja-gjahr.
*----Plan
DATA :   wa_text_1(20)   TYPE  c VALUE 'Plan',
         wa_wtjhro_1  LIKE  bpja-wtjhr,
         wa_wtjhr1_1  LIKE  bpja-wtjhr,
         wa_wtjhr2_1  LIKE  bpja-wtjhr,
         wa_wtjhr3_1  LIKE  bpja-wtjhr,
         wa_wtjhr4_1  LIKE  bpja-wtjhr,
         wa_wtjhr5_1  LIKE  bpja-wtjhr,
         wa_wtjhr6_1  LIKE  bpja-wtjhr,
         wa_wtjhr7_1  LIKE  bpja-wtjhr,
         wa_wtjhr8_1  LIKE  bpja-wtjhr.
*----Org budget
DATA :   wa_text_2(20)   TYPE  c VALUE 'Ori Budget',
         wa_wtjhro_2  LIKE  bpja-wtjhr,
         wa_wtjhr1_2  LIKE  bpja-wtjhr,
         wa_wtjhr2_2  LIKE  bpja-wtjhr,
         wa_wtjhr3_2  LIKE  bpja-wtjhr,
         wa_wtjhr4_2  LIKE  bpja-wtjhr,
         wa_wtjhr5_2  LIKE  bpja-wtjhr,
         wa_wtjhr6_2  LIKE  bpja-wtjhr,
         wa_wtjhr7_2  LIKE  bpja-wtjhr,
         wa_wtjhr8_2  LIKE  bpja-wtjhr.

*----Supplement
DATA :   wa_text_3(20)   TYPE  c VALUE 'Supplement',
         wa_wtjhro_3  LIKE  bpja-wtjhr,
         wa_wtjhr1_3  LIKE  bpja-wtjhr,
         wa_wtjhr2_3  LIKE  bpja-wtjhr,
         wa_wtjhr3_3  LIKE  bpja-wtjhr,
         wa_wtjhr4_3  LIKE  bpja-wtjhr,
         wa_wtjhr5_3  LIKE  bpja-wtjhr,
         wa_wtjhr6_3  LIKE  bpja-wtjhr,
         wa_wtjhr7_3  LIKE  bpja-wtjhr,
         wa_wtjhr8_3  LIKE  bpja-wtjhr.

*----Retuen
DATA :   wa_text_4(20)   TYPE  c VALUE 'Return',
         wa_wtjhro_4  LIKE  bpja-wtjhr,
         wa_wtjhr1_4  LIKE  bpja-wtjhr,
         wa_wtjhr2_4  LIKE  bpja-wtjhr,
         wa_wtjhr3_4  LIKE  bpja-wtjhr,
         wa_wtjhr4_4  LIKE  bpja-wtjhr,
         wa_wtjhr5_4  LIKE  bpja-wtjhr,
         wa_wtjhr6_4  LIKE  bpja-wtjhr,
         wa_wtjhr7_4  LIKE  bpja-wtjhr,
         wa_wtjhr8_4  LIKE  bpja-wtjhr.

*----Currency Budget
DATA :   wa_text_5(20)   TYPE  c VALUE 'Cur Budget',
         wa_wtjhro_5  LIKE  bpja-wtjhr,
         wa_wtjhr1_5  LIKE  bpja-wtjhr,
         wa_wtjhr2_5  LIKE  bpja-wtjhr,
         wa_wtjhr3_5  LIKE  bpja-wtjhr,
         wa_wtjhr4_5  LIKE  bpja-wtjhr,
         wa_wtjhr5_5  LIKE  bpja-wtjhr,
         wa_wtjhr6_5  LIKE  bpja-wtjhr,
         wa_wtjhr7_5  LIKE  bpja-wtjhr,
         wa_wtjhr8_5  LIKE  bpja-wtjhr.

*----Value
DATA :   wa_text_6(20)   TYPE  c,
*        wa_wtjhro_6  LIKE  bpja-wtjhr,
         wa_wtjhro_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr1_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr2_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr3_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr4_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr5_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr6_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr7_6(15) TYPE p,  "LIKE  bpja-wtjhr,
         wa_wtjhr8_6(15) TYPE p.  "  LIKE  bpja-wtjhr.
DATA :   wa_d_text_6(20)   TYPE  c,
         wa_d_wtjhro_6(20), " LIKE  bpja-wtjhr,
         wa_d_wtjhr1_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr2_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr3_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr4_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr5_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr6_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr7_6(20), "  LIKE  bpja-wtjhr,
         wa_d_wtjhr8_6(20). "  LIKE  bpja-wtjhr.
*----After Budget
DATA :   wa_text_7(20)   TYPE  c,
         wa_wtjhro_7  LIKE  bpja-wtjhr,
         wa_wtjhr1_7  LIKE  bpja-wtjhr,
         wa_wtjhr2_7  LIKE  bpja-wtjhr,
         wa_wtjhr3_7  LIKE  bpja-wtjhr,
         wa_wtjhr4_7  LIKE  bpja-wtjhr,
         wa_wtjhr5_7  LIKE  bpja-wtjhr,
         wa_wtjhr6_7  LIKE  bpja-wtjhr,
         wa_wtjhr7_7  LIKE  bpja-wtjhr,
         wa_wtjhr8_7  LIKE  bpja-wtjhr.
DATA : wa_text(90),
       wa_text1(100),
       wa_width TYPE i VALUE 170. "Paper width

DATA : wa_amt LIKE bpja-wtjhr,
       wa_actual LIKE bpja-wtjhr,
       wa_tot_amt LIKE bpja-wtjhr,
       wa_gjahr(07).


* UD1K941576 - by IG.MOON 9/12/2007 {
DATA : it_act_n LIKE zfi_io_actual OCCURS 0 WITH HEADER LINE.
* }

DATA: it_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.
*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.
DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_result  OCCURS 0,
            text(10),
            year(10), "  LIKE bpja-gjahr,
            msgv1 LIKE bdcmsgcoll-msgv1,
       END OF it_result.
DATA:   messtxt(255) TYPE c.
DATA : tcode LIKE tstc-tcode.

DATA : disp_mode         TYPE   c,
       umode             TYPE   c   VALUE 'S',     " Async, Sync
       w_subrc           LIKE   sy-subrc,
       wa_date           LIKE   bpdy-bldat,
       wa_ok,
       wa_ok1,
       wa_budget,
       wa_posid          LIKE   impr-posid,
       wa_t_cnt          TYPE  i,
       wa_cnt            TYPE  i,
       wa_chk,
       wa_type,
       wa_year_chk.
*----VALUE-REQUEST
DATA : selectfield   LIKE  help_info-fieldname,
       it_fields     LIKE  help_value OCCURS 0 WITH HEADER LINE,
       select_value  LIKE  help_info-fldvalue,
       ld_tabix      LIKE  sy-tabix.

CLEAR: selectfield, it_fields, select_value, ld_tabix.
REFRESH: it_fields.

DATA : BEGIN OF it_value OCCURS 0,
         reson LIKE ztfi_su-reson,
         descr LIKE ztfi_su-descr,
       END OF it_value.
CLEAR: it_value.

REFRESH : it_dynpfields, it_value.
CLEAR   : it_dynpfields, it_value.
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE text-111.
PARAMETER : p_aufnr  LIKE   codia-aufnr MATCHCODE OBJECT orde,
            p_gjahr  LIKE   impr-gjahr DEFAULT sy-datum+0(4).
SELECT-OPTIONS :
            s_year   FOR    impr-gjahr.
PARAMETER : p_date   LIKE   bpdy-bldat DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b11.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS : p_r0 RADIOBUTTON GROUP rad,
             p_r1 RADIOBUTTON GROUP rad,
             p_r2 RADIOBUTTON GROUP rad,
             p_r3 RADIOBUTTON GROUP rad.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-002.
PARAMETERS : p_reson LIKE ztfi_reason-reson.
SELECTION-SCREEN END OF BLOCK b02.
PARAMETERS: p_mode(1) TYPE c DEFAULT 'E' NO-DISPLAY.

*----------------------------------------------------------------------
*
* INITIALIZATION
*
*----------------------------------------------------------------------
*
INITIALIZATION.
*---------------------------------------------------------------------*
  CLEAR : wa_ok.
  wa_ok  = 'Q'.
  wa_ok1 = 'X'.
  s_year-low = sy-datum+0(4) - 1.
  s_year-high = s_year-low + 8.
  APPEND s_year.
*---------------------------------------------------------------------
AT SELECTION-SCREEN  ON RADIOBUTTON GROUP rad.
*---------------------------------------------------------------------*
  IF p_r1 = 'X'.
    wa_text_6 = 'Orig Budget'.
    wa_type = '1'.
  ELSEIF p_r2 = 'X'.
    wa_text_6 = 'Supplement'.
    wa_type = '2'.
  ELSEIF p_r3 = 'X'.
    wa_text_6 = 'Return'.
    wa_type = '3'.
  ENDIF.
  wa_text_7 = 'After Budget'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_reson.
  PERFORM pov_reson.
*
*---------------------------------------------------------------------*
*    M   A   I   N
*
*---------------------------------------------------------------------
START-OF-SELECTION.
**--------------------------------------------------------------------*

*--------------------------------------------------------------------*
  CONCATENATE  p_date+4(2) p_date+6(2) p_date+0(4) INTO wa_date.
  IF p_aufnr IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No data found'.
    EXIT.
  ENDIF.
  IF p_r1 = 'X' OR p_r2 = 'X' OR p_r3 = 'X'.
    IF p_reson = space.
      MESSAGE s000(zmfi) WITH 'Select reason code'.
      EXIT.
    ELSE.
      IF p_r1 = 'X'.
        wa_chk = '1'.
      ELSEIF p_r2 = 'X'.
        wa_chk = '2'.
      ELSEIF p_r3 = 'X'.
        wa_chk = '3'.
      ENDIF.
      CLEAR : wa_cnt.
      PERFORM check_reason_code USING wa_chk wa_cnt.
      IF wa_cnt < 1.
        MESSAGE s000(zmfi) WITH 'Check reason code'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
*--GET Oeder master object
  PERFORM select_data.
*--Check Data if no data --> error message.
  IF wa_subrc <> 0.
    MESSAGE s000(zmfi) WITH 'No data found'.
    EXIT.
  ENDIF.
*---Totals Record for Annual Total FROM table BPJA
*-- Order plan value by annual
  PERFORM get_annual_data.
*----get data  overall
*---Order plan value by Overall
  PERFORM get_overall_data.
*----Make internal table from bpja bpge by org bud/supplement/return
  PERFORM make_it_data.
*---Data Calculation and convert
  PERFORM cal_process.
****PI Process rtn
*---Get PI from IMZO Table   rate
  PERFORM get_imzo.
*---Get PI from IMPR Table
  CLEAR wa_t_cnt.
  DESCRIBE TABLE it_imzo LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    PERFORM get_impr.
  ENDIF.
*---GET PI data from BPJA
*--- pi plan vaiue annual and overall
  CLEAR wa_t_cnt.
  DESCRIBE TABLE it_impr LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    PERFORM get_pi_data.
*---Make pi data   org budget Current Budget
    PERFORM make_ip_data.
*----DATA MOVE
    PERFORM move_data.
  ENDIF.


*====================================================================*
END-OF-SELECTION.
  REFRESH :  it_pi1, it_pi2.
  CLEAR   :  it_pi1, it_pi2.
  CLEAR wa_t_cnt.
  DESCRIBE TABLE it_impr LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    PERFORM write_pro.
  ELSE.
    MESSAGE i000(zmfi) WITH 'No data PI'.
    STOP.
  ENDIF.

  SET PF-STATUS '9000'.

*====================================================================*
AT LINE-SELECTION.
  DATA : wa_field(20),
         wa_value(20),
         wa_posid1 LIKE impr-posid,
         wa_gjahr1(07),
         wa_gjahr11(04),
         wa_prnam LIKE impr-prnam,
         wa_line TYPE i.
  GET CURSOR FIELD  wa_field VALUE wa_value.
  GET CURSOR LINE wa_line.
  wa_line = wa_line - 15.
  wa_posid1 = sy-lisel+12(16).
  wa_gjahr1 = sy-lisel+31(07).
  IF wa_gjahr1 = 'Overall'.
    wa_gjahr11 = '1111'.
  ELSE.
    wa_gjahr11 = wa_gjahr1+0(4).
  ENDIF.
  IF wa_field = 'P_AUFNR'.
    SET PARAMETER ID 'ANR' FIELD p_aufnr.
    CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
  ENDIF.

  PERFORM modify_symbol.

  IF wa_field = 'IT_PI2-POSID'.
    SET PARAMETER ID 'IMT'   FIELD it_pi2-prnam.
    SET PARAMETER ID 'IMP'   FIELD wa_posid1.
    SET PARAMETER ID 'GJR'   FIELD p_gjahr.
    CALL TRANSACTION 'IM13' AND SKIP FIRST SCREEN.
  ENDIF.

  PERFORM write_pro_agin.


*====================================================================*
AT USER-COMMAND.

  CASE sy-ucomm.
*--- data check   after budget = ori budget + supplement - retuen.
    WHEN 'CHEK'.
      CHECK p_r0 <> 'X'.
      wa_ok1 = 'Q'.
      wa_ok  = ' '.
      PERFORM check_data_rtn.
**----after budget > available
      IF p_r2 ='X' OR p_r1 = 'X'.
        IF wa_ok = 'X' AND wa_tot_amt < 0.
          MESSAGE s000(zmfi) WITH 'PI Budget Exceed'.
        ENDIF.
*      else.
*         IF wa_ok = 'X'.
*           MESSAGE s000(zmfi) WITH 'PI Budget Exceed'.
*        ENDIF.
      ENDIF.
*---2004/04/02
      IF wa_wtjhro_7 < 0 OR wa_wtjhr1_7 < 0 OR
         wa_wtjhr2_7 < 0 OR wa_wtjhr3_7 < 0 OR
         wa_wtjhr4_7 < 0 OR wa_wtjhr5_7 < 0 OR
         wa_wtjhr6_7 < 0 OR wa_wtjhr7_7 < 0 OR
         wa_wtjhr8_7 < 0.
        MESSAGE s000(zmfi) WITH 'Check After Budget Nagative'.
      ENDIF.

      IF wa_year_chk = 'X'.
        MESSAGE w000(zmfi) WITH 'PI Budget Check'.
*        EXIT.
      ENDIF.
*---text check
      READ LINE 12 FIELD VALUE wa_text INTO wa_text.
      IF wa_text = ' '.
        MESSAGE s000(zmfi) WITH 'Insert Text'.
        EXIT.
      ENDIF.
    WHEN 'PROD'.              "BDC process FOR BUDGET
      CHECK p_r0 <> 'X'.
*----after budget > available
      IF wa_ok = 'X' AND wa_tot_amt < 0.
        MESSAGE w000(zmfi) WITH 'PI Budget Exceed or Check amount'.
        EXIT.
      ENDIF.
*--clicked CHECK button before click PROD......data error check
      IF wa_ok1 = 'X'.
        MESSAGE s000(zmfi) WITH 'Click check button'.
        EXIT.
      ENDIF.
*---2004/04/02
      IF wa_wtjhro_7 < 0 OR wa_wtjhr1_7 < 0 OR
         wa_wtjhr2_7 < 0 OR wa_wtjhr3_7 < 0 OR
         wa_wtjhr4_7 < 0 OR wa_wtjhr5_7 < 0 OR
         wa_wtjhr6_7 < 0 OR wa_wtjhr7_7 < 0 OR
         wa_wtjhr8_7 < 0.
        MESSAGE s000(zmfi) WITH 'Check After Budget Nagative'.
      ENDIF.
*---required text
*---text check
      READ LINE 12 FIELD VALUE wa_text INTO wa_text.
      IF wa_text = ' '.
        MESSAGE w000(zmfi) WITH 'Insert Text'.
        EXIT.
      ENDIF.
      MOVE 'X' TO wa_budget.          "CHECK BUDGET BDC
*- Execute Transcation by activity ( org bud, sipplement, return)
*--- No exist BAPI Function
      PERFORM process_rtn.
*-- Order plan value by annual
      PERFORM get_annual_data.
*-- Order plan value by overall
      PERFORM get_overall_data.
*----Make internal table from bpja bpge by org bud/supplement/return
      PERFORM make_it_data.
*---Data Calculation and convert
      PERFORM cal_process.
*---Get PI from IMZO Table   rate
      PERFORM get_imzo.
*---Get PI from IMPR Table
      PERFORM get_impr.
*---GET PI data from BPJA
*--- pi plan vaiue annual and overall
      PERFORM get_pi_data.
*---Make pi data   org budget Current Budget
      PERFORM make_ip_data.
*-- redisplay after execute transaction.
      PERFORM check_data_rtn.
      wa_ok1 = 'X'.
    WHEN 'FUND'.
**      IF WA_BUDGET <>  'Q'.
**        MESSAGE s000(zmfi) WITH 'First Update Budget'.
**        EXIT.
**      ENDIF.
**-- pi update fund
**--- No exist BAPI Function
**     check p_r0 <> 'X'.
*** --> Change Logic for ECC6.0(KDM01)
*      perform fund_bdc_process.
      PERFORM insert_fmderive.
*-- error list display.
    WHEN 'RST'.
      CHECK p_r0 <> 'X'.
      LOOP AT it_result.
        WRITE : / it_result-text,
                  it_result-year,
                  it_result-msgv1.
      ENDLOOP.

* BEGIN OF UD1K949686
    WHEN 'RELE'.

*      DATA : l_mandt TYPE mandt.
*      DATA : l_aufnr TYPE aufnr.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT         = p_aufnr
*       IMPORTING
*         OUTPUT        = l_aufnr.
*
*      SELECT SINGLE mandt INTO l_mandt
*        FROM FMFMOAUP23000013
*       WHERE SOUR1_FROM = l_aufnr.
*
*      IF sy-subrc <> 0.
*        MESSAGE i000 WITH 'Assign FM account first!'.
*      ELSE.
*        set parameter id 'ANR' field p_aufnr.
*        call transaction 'KO02' and skip first screen.
*      ENDIF.
** END OF UD1K949686


* BEGIN OF UD1K953547
      SET PARAMETER ID 'ANR' FIELD p_aufnr.
      CALL TRANSACTION 'KO02' AND SKIP FIRST SCREEN.
* END OF UD1K953547

  ENDCASE.
*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM select_data.
*---Get Object no from io
  PERFORM get_object_no USING p_aufnr.
ENDFORM.                    " select_data


*&---------------------------------------------------------------------*
*&      Form  GET_OBJECT_NO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_AUFNR  text
*----------------------------------------------------------------------*
FORM get_object_no USING    p_p_aufnr.
  CLEAR : wa_objnr, wa_subrc.
  SELECT SINGLE objnr kostv INTO (wa_objnr, wa_kostv)
     FROM aufk
     WHERE aufnr EQ p_p_aufnr.
  wa_subrc = sy-subrc.

ENDFORM.                    " GET_OBJECT_NO
*&---------------------------------------------------------------------*
*&      Form  GET_ANNUAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_annual_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja
  FROM bpja
  WHERE lednr EQ '0001'
  AND   objnr EQ wa_objnr
  AND   versn EQ '000'.     " 524A353457


ENDFORM.                    " GET_ANNUAL_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_data.
*-----jhs modify set year
*  wa_year1 = sy-datum+0(4) - 1.
*  wa_year2 = sy-datum+0(4).
*  wa_year3 = sy-datum+0(4) + 1.
*  wa_year4 = sy-datum+0(4) + 2.
*  wa_year5 = sy-datum+0(4) + 3.
*  wa_year6 = sy-datum+0(4) + 4.
*  wa_year7 = sy-datum+0(4) + 5.
*  wa_year8 = sy-datum+0(4) + 6.
*  wa_year9 = sy-datum+0(4) + 7.
  wa_year1 = s_year-low.
  wa_year2 = s_year-low + 1.
  wa_year3 = s_year-low + 2.
  wa_year4 = s_year-low + 3.
  wa_year5 = s_year-low + 4.
  wa_year6 = s_year-low + 5.
  wa_year7 = s_year-low + 6.
  wa_year8 = s_year-low + 7.
  wa_year9 = s_year-low + 8.
*--- Annual data process
  LOOP AT it_bpja.
    CASE it_bpja-vorga.
      WHEN 'KSTP'.        "Plan
        MOVE 'Plan'   TO wa_text_1.
        CASE it_bpja-gjahr.
          WHEN  wa_year1.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr1_1.
          WHEN  wa_year2.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr2_1.
          WHEN  wa_year3.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr3_1.
          WHEN  wa_year4.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr4_1.
          WHEN  wa_year5.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr5_1.
          WHEN  wa_year6.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr6_1.
          WHEN  wa_year7.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr7_1.
          WHEN  wa_year8.
            MOVE it_bpja-wtjhr  TO   wa_wtjhr8_1.
        ENDCASE.
      WHEN 'KBUD'.        "Ori Budget
        MOVE 'Ori Budget'  TO wa_text_2.
        CASE it_bpja-gjahr.
          WHEN  wa_year1.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr1_2.
          WHEN  wa_year2.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr2_2.
          WHEN  wa_year3.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr3_2.
          WHEN  wa_year4.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr4_2.
          WHEN  wa_year5.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr5_2.
          WHEN  wa_year6.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr6_2.
          WHEN  wa_year7.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr7_2.
          WHEN  wa_year8.
            MOVE it_bpja-wtjhr  TO   wa_wtjhr8_2.
        ENDCASE.
      WHEN 'KBN0'.        "Supplement
        MOVE 'Supplement'  TO wa_text_3.
        CASE it_bpja-gjahr.
          WHEN  wa_year1.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr1_3.
          WHEN  wa_year2.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr2_3.
          WHEN  wa_year3.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr3_3.
          WHEN  wa_year4.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr4_3.
          WHEN  wa_year5.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr5_3.
          WHEN  wa_year6.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr6_3.
          WHEN  wa_year7.
            MOVE it_bpja-wtjhr  TO  wa_wtjhr7_3.
          WHEN  wa_year8.
            MOVE it_bpja-wtjhr  TO   wa_wtjhr8_3.
        ENDCASE.
      WHEN 'KBR0'.        "Return
        MOVE 'Return'    TO  wa_text_4.
        CASE it_bpja-gjahr.
          WHEN  wa_year1.
            wa_wtjhr1_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year2.
            wa_wtjhr2_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year3.
            wa_wtjhr3_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year4.
            wa_wtjhr4_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year5.
            wa_wtjhr5_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year6.
            wa_wtjhr6_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year7.
            wa_wtjhr7_4 = it_bpja-wtjhr * -1.
          WHEN  wa_year8.
            wa_wtjhr8_4 = it_bpja-wtjhr * -1.
        ENDCASE.
    ENDCASE.
  ENDLOOP.
*-------------------Overall data process
  LOOP AT it_bpge.
    CASE it_bpge-vorga.
      WHEN 'KSTP'.        "Plan
        MOVE it_bpge-wtges  TO  wa_wtjhro_1.
      WHEN 'KBUD'.        "Ori Budget
        MOVE it_bpge-wtges  TO  wa_wtjhro_2.
      WHEN 'KBN0'.        "Supplement
        MOVE it_bpge-wtges  TO  wa_wtjhro_3.
      WHEN 'KBR0'.        "Return
        wa_wtjhro_4 = it_bpge-wtges * -1.
    ENDCASE.
  ENDLOOP.

  MOVE 'Cur Budget'   TO wa_text_5.
ENDFORM.                    " MAKE_IT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_OVERALL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_overall_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge
  FROM bpge
  WHERE lednr EQ '0001'
  AND   objnr EQ wa_objnr.
ENDFORM.                    " GET_OVERALL_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_process.
  wa_wtjhro_5  = wa_wtjhro_2 + wa_wtjhro_3 - wa_wtjhro_4.
  wa_wtjhr1_5 =  wa_wtjhr1_2 + wa_wtjhr1_3 - wa_wtjhr1_4.
  wa_wtjhr2_5 =  wa_wtjhr2_2 + wa_wtjhr2_3 - wa_wtjhr2_4.
  wa_wtjhr3_5 =  wa_wtjhr3_2 + wa_wtjhr3_3 - wa_wtjhr3_4.
  wa_wtjhr4_5 =  wa_wtjhr4_2 + wa_wtjhr4_3 - wa_wtjhr4_4.
  wa_wtjhr5_5 =  wa_wtjhr5_2 + wa_wtjhr5_3 - wa_wtjhr5_4.
  wa_wtjhr6_5 =  wa_wtjhr6_2 + wa_wtjhr6_3 - wa_wtjhr6_4.
  wa_wtjhr7_5 =  wa_wtjhr7_2 + wa_wtjhr7_3 - wa_wtjhr7_4.
  wa_wtjhr8_5 =  wa_wtjhr8_2 + wa_wtjhr8_3 - wa_wtjhr8_4.
*---CONVERT FOR DISPLAY
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_aufnr
    IMPORTING
      output = p_aufnr.

ENDFORM.                    " CAL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  GET_PI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pi_data.
  REFRESH : it_bpja2.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja2
  FROM bpja
  FOR ALL ENTRIES IN it_impr
  WHERE lednr EQ '0001'
  AND   objnr EQ it_impr-objnr.
*  AND   GJAHR EQ IT_IMPR-GJAHR.
*---
  REFRESH : it_bpge2.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge2
  FROM bpge
  FOR ALL ENTRIES IN it_impr
  WHERE lednr EQ '0001'
  AND   objnr EQ it_impr-objnr.
ENDFORM.                    " GET_PI_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_IMZO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_imzo.
  REFRESH : it_imzo.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_imzo
  FROM imzo
  WHERE objnr EQ wa_objnr.
*---jhs modify 2004/01/30
* AND   xgenj EQ '0'.
ENDFORM.                    " GET_IMZO
*&---------------------------------------------------------------------*
*&      Form  GET_IMPR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_impr.
  REFRESH : it_impr.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_impr
  FROM impr
  FOR ALL ENTRIES IN it_imzo
  WHERE posnr = it_imzo-posnr
  AND   gjahr = p_gjahr.
ENDFORM.                    " GET_IMPR
*&---------------------------------------------------------------------*
*&      Form  WRITE_PRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_pro.
  sy-lsind = sy-lsind - 1.
  ULINE AT (wa_width).
  WRITE : /01 'Order.no'   COLOR COL_KEY,
           12 'Activity'   COLOR COL_KEY,
           30 'Overall'    COLOR COL_KEY,
           49(4) wa_year1  COLOR COL_KEY,
           65(4) wa_year2  COLOR COL_KEY,
           81(4) wa_year3  COLOR COL_KEY,
           97(4) wa_year4  COLOR COL_KEY,
          113(4) wa_year5  COLOR COL_KEY,
          129(4) wa_year6  COLOR COL_KEY,
          145(4) wa_year7  COLOR COL_KEY,
          161(4) wa_year8  COLOR COL_KEY.
  ULINE AT (wa_width).
*---Plan
  WRITE : /01(10)  p_aufnr COLOR COL_GROUP,
           12(10)  wa_text_1,
           23(15)  wa_wtjhro_1 NO-ZERO,
           39(15)  wa_wtjhr1_1 NO-ZERO,
           55(15)  wa_wtjhr2_1 NO-ZERO,
           71(15)  wa_wtjhr3_1 NO-ZERO,
           87(15)  wa_wtjhr4_1 NO-ZERO,
          103(15)  wa_wtjhr5_1 NO-ZERO,
          119(15)  wa_wtjhr6_1 NO-ZERO,
          135(15)  wa_wtjhr7_1 NO-ZERO,
          151(15)  wa_wtjhr8_1 NO-ZERO.

*---Org Budget
  WRITE : /12(10)  wa_text_2,
           23(15)  wa_wtjhro_2 NO-ZERO,
           39(15)  wa_wtjhr1_2 NO-ZERO,
           55(15)  wa_wtjhr2_2 NO-ZERO,
           71(15)  wa_wtjhr3_2 NO-ZERO,
           87(15)  wa_wtjhr4_2 NO-ZERO,
          103(15)  wa_wtjhr5_2 NO-ZERO,
          119(15)  wa_wtjhr6_2 NO-ZERO,
          135(15)  wa_wtjhr7_2 NO-ZERO,
          151(15)  wa_wtjhr8_2 NO-ZERO.

*---Supplement
  WRITE : /12(10)  wa_text_3,
           23(15)  wa_wtjhro_3 NO-ZERO,
           39(15)  wa_wtjhr1_3 NO-ZERO,
           55(15)  wa_wtjhr2_3 NO-ZERO,
           71(15)  wa_wtjhr3_3 NO-ZERO,
           87(15)  wa_wtjhr4_3 NO-ZERO,
          103(15)  wa_wtjhr5_3 NO-ZERO,
          119(15)  wa_wtjhr6_3 NO-ZERO,
          135(15)  wa_wtjhr7_3 NO-ZERO,
          151(15)  wa_wtjhr8_3 NO-ZERO.

*---Return
  WRITE : /12(10)  wa_text_4,
           23(15)  wa_wtjhro_4 NO-ZERO,
           39(15)  wa_wtjhr1_4 NO-ZERO,
           55(15)  wa_wtjhr2_4 NO-ZERO,
           71(15)  wa_wtjhr3_4 NO-ZERO,
           87(15)  wa_wtjhr4_4 NO-ZERO,
          103(15)  wa_wtjhr5_4 NO-ZERO,
          119(15)  wa_wtjhr6_4 NO-ZERO,
          135(15)  wa_wtjhr7_4 NO-ZERO,
          151(15)  wa_wtjhr8_4 NO-ZERO.
*---Curren Budget
  WRITE : /12(10)  wa_text_5,
           23(15)  wa_wtjhro_5 NO-ZERO  COLOR COL_TOTAL,
           39(15)  wa_wtjhr1_5 NO-ZERO  COLOR COL_TOTAL,
           55(15)  wa_wtjhr2_5 NO-ZERO  COLOR COL_TOTAL,
           71(15)  wa_wtjhr3_5 NO-ZERO  COLOR COL_TOTAL,
           87(15)  wa_wtjhr4_5 NO-ZERO  COLOR COL_TOTAL,
          103(15)  wa_wtjhr5_5 NO-ZERO  COLOR COL_TOTAL,
          119(15)  wa_wtjhr6_5 NO-ZERO  COLOR COL_TOTAL,
          135(15)  wa_wtjhr7_5 NO-ZERO  COLOR COL_TOTAL,
          151(15)  wa_wtjhr8_5 NO-ZERO  COLOR COL_TOTAL.
  ULINE AT (wa_width).
*---2004/01/27
  IF p_r0 = 'X'.
    WRITE : /23(15)  wa_wtjhro_6  NO-ZERO, "INPUT ON,
             39(15)  wa_wtjhr1_6  NO-ZERO,
             55(15)  wa_wtjhr2_6  NO-ZERO,
             71(15)  wa_wtjhr3_6  NO-ZERO,
             87(15)  wa_wtjhr4_6  NO-ZERO,
            103(15)  wa_wtjhr5_6  NO-ZERO,
            119(15)  wa_wtjhr6_6  NO-ZERO,
            135(15)  wa_wtjhr7_6  NO-ZERO,
            151(15)  wa_wtjhr8_6  NO-ZERO.
  ELSE.
*---value  input
    WRITE : /12(10)  wa_text_6,
             23(15)  wa_wtjhro_6 , "INPUT ON,
             39(15)  wa_wtjhr1_6 INPUT ON NO-ZERO,
             55(15)  wa_wtjhr2_6 INPUT ON NO-ZERO,
             71(15)  wa_wtjhr3_6 INPUT ON NO-ZERO,
             87(15)  wa_wtjhr4_6 INPUT ON NO-ZERO,
            103(15)  wa_wtjhr5_6 INPUT ON NO-ZERO,
            119(15)  wa_wtjhr6_6 INPUT ON NO-ZERO,
            135(15)  wa_wtjhr7_6 INPUT ON NO-ZERO,
            151(15)  wa_wtjhr8_6 INPUT ON NO-ZERO.
  ENDIF.
* wa_d_wtjhr1_6 = wa_wtjhr1_6.
*---After Budget
  IF p_r0 <> 'X'.
    WRITE : /12(10)  wa_text_7,
             23(15)  wa_wtjhro_7 NO-ZERO,
             39(15)  wa_wtjhr1_7 NO-ZERO,
             55(15)  wa_wtjhr2_7 NO-ZERO,
             71(15)  wa_wtjhr3_7 NO-ZERO,
             87(15)  wa_wtjhr4_7 NO-ZERO,
            103(15)  wa_wtjhr5_7 NO-ZERO,
            119(15)  wa_wtjhr6_7 NO-ZERO,
            135(15)  wa_wtjhr7_7 NO-ZERO,
            151(15)  wa_wtjhr8_7 NO-ZERO.
  ENDIF.
*--Text
  IF p_r0 <> 'X'.                                           "2004/01/27
    WRITE : /12(10) 'Text',
             23(142) wa_text INPUT ON.
    ULINE AT (wa_width).
  ENDIF.
**=======PI
*  WRITE :/1(10) 'Condition' COLOR COL_KEY,
*         13(10) 'Position' COLOR COL_KEY,
*         32(04)  'Year' COLOR COL_KEY,
*         46(15)  'Orig.Bud' COLOR COL_KEY,
*         62(15)  'Curr.Bud' COLOR COL_KEY,
*         80(10)  'Assign' COLOR COL_KEY,
*         94(15)  'Available' COLOR COL_KEY,
*        110(04)  'Rate' COLOR COL_KEY,
*        124(10)  'I/O Plan' COLOR COL_KEY,
*        143(15)  'Ord.Bud' COLOR COL_KEY.
**        146(10)  'Condition' COLOR COL_KEY.
*=======PI
  WRITE : /1(10)  'Condition' COLOR COL_KEY,
         13(15) 'Position Number' COLOR COL_KEY,
         32(04)  'Year' COLOR COL_KEY,
         49(08)   'Orig.Bud' COLOR COL_KEY,
         67(08)  'Curr.Bud' COLOR COL_KEY,
         86(08)  'Assigned' COLOR COL_KEY,
        103(09)  'Available' COLOR COL_KEY,
        119(04)  'Rate' COLOR COL_KEY,
        132(08)  'I/O Plan' COLOR COL_KEY.
*        150(09)  'After.Bud' COLOR COL_KEY.
*       146(10)  'Condition' COLOR COL_KEY.
  ULINE AT (wa_width).
  CLEAR wa_t_cnt.
  DESCRIBE TABLE it_pi LINES wa_t_cnt.
*===================================================================*
  LOOP AT it_pi.
    MOVE it_pi-posid    TO it_pi1-posid.
    MOVE it_impr-prnam  TO it_pi1-prnam.
    COLLECT it_pi1.
    CLEAR   it_pi1.
  ENDLOOP.
*-------------------------------------------------------------------*
  REFRESH : it_rate, it_order.
  CLEAR   : it_rate, it_order.
  LOOP AT it_pi1.
    PERFORM get_pi_assign USING it_pi1-posid
                          CHANGING wa_amt.
    LOOP AT it_assign.
      MOVE-CORRESPONDING it_assign TO it_order.
      APPEND it_order.
      CLEAR  it_order.
*---CONVERT FOR DISPLAY
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = it_assign-aufnr
        IMPORTING
          output = it_assign-aufnr.
*------------------------------*
      MOVE it_pi1-posid     TO it_pi2-posid.
      MOVE it_pi1-prnam     TO it_pi2-prnam.
      MOVE it_assign-gjahr TO it_pi2-gjahr.
*---2004/03/12
* exlclude available budget if it is original distribution.
      IF p_r1 = 'X'.
        IF p_aufnr <>  it_assign-aufnr.
          MOVE it_assign-wtjhr TO it_pi2-amt3.
        ENDIF.
      ELSE.
        MOVE it_assign-wtjhr TO it_pi2-amt3.
      ENDIF.
*-----*
*      move it_assign-wtjhr    to it_pi2-amt6.
*--RATE APPEND
      MOVE it_pi1-posid    TO it_rate-posid.
      MOVE it_assign-prozu TO it_rate-prozu.
      APPEND it_rate.
      CLEAR  it_rate.
*      MOVE it_assign-prozu TO it_pi2-rate.
      COLLECT it_pi2.
      CLEAR  it_pi2.
    ENDLOOP.

  ENDLOOP.
*===================================================*
  SORT it_pi BY posid  ASCENDING.
  LOOP AT it_pi.
    MOVE-CORRESPONDING it_pi TO it_pi2.
    COLLECT it_pi2.
    CLEAR it_pi2.
  ENDLOOP.
*====*GET RATE.
  LOOP AT it_imzo.
    READ TABLE it_impr WITH KEY posnr = it_imzo-posnr.
    IF sy-subrc = 0.
      MOVE it_impr-posid TO it_imzo-posid.
      MODIFY it_imzo.
    ENDIF.
  ENDLOOP.
*
  LOOP AT it_pi2.
    READ TABLE it_imzo WITH KEY posid = it_pi2-posid.
    IF sy-subrc = 0.
      IF it_imzo-prozu = 0.
        MOVE it_imzo-baprz TO it_pi2-rate.
      ELSE.
        MOVE it_imzo-prozu TO it_pi2-rate.
      ENDIF.
      IF it_imzo-prozu = 0 AND it_imzo-baprz = 0.
        MOVE 100           TO it_pi2-rate.
      ENDIF.
      MOVE '4' TO it_pi2-sign.
      MODIFY it_pi2.
    ENDIF.
  ENDLOOP.

*----PI WRITE-----*
  SORT it_pi2 BY posid gjahr ASCENDING.
  wa_year_chk = ' '.
  LOOP AT it_pi2.

*----get i/o plan
    CASE it_pi2-gjahr.
      WHEN '1111'.
        MOVE wa_wtjhro_1 TO   it_pi2-amt5.
      WHEN  wa_year1.
        MOVE wa_wtjhr1_1  TO  it_pi2-amt5.
      WHEN  wa_year2.
        MOVE wa_wtjhr2_1  TO  it_pi2-amt5.
      WHEN  wa_year3.
        MOVE wa_wtjhr3_1  TO  it_pi2-amt5.
      WHEN  wa_year4.
        MOVE wa_wtjhr4_1  TO  it_pi2-amt5.
      WHEN  wa_year5.
        MOVE wa_wtjhr5_1  TO  it_pi2-amt5.
      WHEN  wa_year6.
        MOVE wa_wtjhr6_1  TO  it_pi2-amt5.
      WHEN  wa_year7.
        MOVE wa_wtjhr7_1  TO  it_pi2-amt5.
      WHEN  wa_year8.
        MOVE wa_wtjhr8_1  TO  it_pi2-amt5.
    ENDCASE.
    IF it_pi2-gjahr = '1111'.
      wa_gjahr = 'Overall'.
    ELSE.
      wa_gjahr = it_pi2-gjahr.
    ENDIF.
    it_pi2-amt4 = it_pi2-amt2 - it_pi2-amt3 - it_pi2-amt9.
    IF it_pi2-amt5 <> 0.
      it_pi2-amt5 = it_pi2-amt5 * it_pi2-rate / 100.
    ENDIF.
    MODIFY it_pi2 INDEX sy-tabix.

    WRITE : /1(04) it_pi2-icon AS ICON CENTERED NO-GAP.
    CASE it_pi2-sign.
      WHEN '4'. WRITE: 10(3) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'. WRITE: 10(3) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
    ENDCASE.
*---2004/03/31
    IF wa_gjahr = '0000'.
      MOVE 'X' TO wa_year_chk.
    ENDIF.
    WRITE : 13(16) it_pi2-posid COLOR COL_NORMAL,
            32(07) wa_gjahr, "it_pi-gjahr,
            43(15) it_pi2-amt1,   "ori.bud
            61(15) it_pi2-amt2,
            78(15) it_pi2-amt3,   "assign
            98(15) it_pi2-amt4,   "available
           115(10) it_pi2-rate,
           126(15) it_pi2-amt5.   "i/o plan
*           145(15) it_pi2-amt6.

    AT END OF posid.
      ULINE AT (wa_width).
    ENDAT.
  ENDLOOP.
ENDFORM.                    " WRITE_PRO
*&---------------------------------------------------------------------*
*&      Form  MAKE_IP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_ip_data.

  REFRESH : it_pi.
**---DELETE  annual
*  LOOP AT it_bpja2.
*    IF it_bpja2-posit+7(1) = '2'.
*      DELETE it_bpja2 INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.
**---DELETE  overall
*  LOOP AT it_bpge2.
*    IF it_bpge2-posit+7(1) = '2'.
*      DELETE it_bpge2 INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.
*--------
  DESCRIBE TABLE it_bpja2 LINES wa_t_cnt.
  IF wa_t_cnt < 1.
    LOOP AT it_impr.
      MOVE it_impr-posid  TO it_pi-posid.
      MOVE it_impr-prnam  TO it_pi-prnam.
      APPEND it_pi.
      CLEAR  it_pi.
    ENDLOOP.
  ELSE.
    LOOP AT it_impr.
      MOVE it_impr-posid  TO it_pi-posid.
      MOVE it_impr-prnam  TO it_pi-prnam.
      LOOP AT it_bpja2 WHERE objnr = it_impr-objnr.
        MOVE it_impr-posid  TO it_pi-posid.
        MOVE it_impr-prnam  TO it_pi-prnam.
        MOVE it_bpja2-gjahr  TO it_pi-gjahr.
*-----Activity check---
        CASE it_bpja2-vorga.
          WHEN 'KBUD'.  "Original
            MOVE it_bpja2-wtjhr TO it_pi-amt1.
            MOVE it_bpja2-wtjhr TO it_pi-amt2.
          WHEN 'KBN0'.
            MOVE it_bpja2-wtjhr TO it_pi-amt2.
          WHEN 'KBR0'.
            MOVE it_bpja2-wtjhr TO it_pi-amt2.
        ENDCASE.
        COLLECT it_pi.
        CLEAR   it_pi.
      ENDLOOP.
*
**---overall
      LOOP AT it_bpge2 WHERE objnr = it_impr-objnr.
        MOVE it_impr-posid  TO it_pi-posid.
        MOVE it_impr-prnam  TO it_pi-prnam.
        MOVE '1111'         TO it_pi-gjahr.
*-----Activity check---
        CASE it_bpge2-vorga.
          WHEN 'KBUD'.  "Original
            MOVE it_bpge2-wtges TO it_pi-amt1.
            MOVE it_bpge2-wtges TO it_pi-amt2.
          WHEN 'KBN0'.  "Supplement
            MOVE it_bpge2-wtges TO it_pi-amt2.
          WHEN 'KBR0'.  "Return
            MOVE it_bpge2-wtges TO it_pi-amt2.
        ENDCASE.
        COLLECT it_pi.
        CLEAR   it_pi.
      ENDLOOP.


*ANDY
*Adj. IO budget = 0, IO actual to Return

      DATA: l_wtges LIKE bpge-wtges,
            w_imzo  LIKE imzo,
            w_imzo2 LIKE imzo OCCURS 0 WITH HEADER LINE.
      DATA : io_act LIKE zfi_io_actual OCCURS 0
                                       WITH HEADER LINE.
      REFRESH w_imzo2.
      SELECT * INTO w_imzo FROM imzo
        WHERE posnr = it_impr-posnr.

        SELECT SUM( wtges ) INTO l_wtges  FROM bpge
           WHERE lednr = '0001'
             AND objnr = w_imzo-objnr
             AND wrttp = '41'.

        IF l_wtges = 0.
          w_imzo2-objnr = w_imzo-objnr. APPEND w_imzo2.
        ENDIF.
      ENDSELECT.
      LOOP AT w_imzo2.

        CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
          EXPORTING
            objnr = w_imzo2-objnr
            l_im  = 'X'  "exclude Co reposting
          IMPORTING
            amt   = l_wtges
          TABLES
            out   = io_act.
        IF l_wtges <> 0.
          MOVE it_impr-posid   TO it_pi-posid.
          MOVE it_impr-prnam   TO it_pi-prnam.
          MOVE '1111'          TO it_pi-gjahr.
          MOVE l_wtges         TO it_pi-amt9.
          COLLECT it_pi.
          LOOP AT io_act.
            MOVE io_act-gjahr    TO it_pi-gjahr.
            MOVE io_act-tot      TO it_pi-amt9.
            COLLECT it_pi.
          ENDLOOP.
        ENDIF.
        CLEAR   it_pi.

      ENDLOOP.

    ENDLOOP.
  ENDIF.
*

ENDFORM.                    " MAKE_IP_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PI_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PI_POSID  text
*----------------------------------------------------------------------*
FORM get_pi_assign USING    u_posid
                            u_amt  LIKE bpja-wtjhr.
  REFRESH : it_assign.
  CLEAR   : it_assign.
* Begin of changes - UD1K918739
  CALL FUNCTION 'Z_FFI_GET_IM_ASSIGN'
    EXPORTING
      posid = u_posid
      aufnr = p_aufnr
      gjahr = p_gjahr                                       "UD1K918739
    IMPORTING
      amt   = u_amt
    TABLES
      out   = it_assign.
* End of changes - UD1K918739
ENDFORM.                    " GET_PI_ASSIGN
*&---------------------------------------------------------------------*
*&      Form  PROCESS_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_rtn.
  IF p_r1 = 'X'.
    tcode = 'KO22'.
  ELSEIF p_r2 = 'X'.
    tcode = 'KO24'.
  ELSEIF p_r3 = 'X'.
    tcode = 'KO26'.
  ENDIF.
  DATA : wa_chk VALUE 'Q',
         wa_gjahr LIKE bpja-gjahr.
  wa_gjahr = wa_year1 - 1.
**--FIELD VALUE f1 INTO g1
*  PERFORM get_input_date.
*
  CLEAR wa_wtjhro_6.
**---2004/03/17
*  IF P_R3 = 'X'.
*     if wa_wtjhr1_6 < 0.
*        wa_wtjhr1_6 = wa_wtjhr1_6 * -1.
*     endif.
*     if wa_wtjhr2_6 < 0.
*        wa_wtjhr2_6 = wa_wtjhr2_6 * -1.
*     endif.
*     if wa_wtjhr3_6 < 0.
*        wa_wtjhr3_6 = wa_wtjhr3_6 * -1.
*     endif.
*     if wa_wtjhr4_6 < 0.
*        wa_wtjhr4_6 = wa_wtjhr4_6 * -1.
*     endif.
*     if wa_wtjhr5_6 < 0.
*        wa_wtjhr5_6 = wa_wtjhr5_6 * -1.
*     endif.
*     if wa_wtjhr6_6 < 0.
*        wa_wtjhr6_6 = wa_wtjhr6_6 * -1.
*     endif.
*     if wa_wtjhr7_6 < 0.
*        wa_wtjhr7_6 = wa_wtjhr7_6 * -1.
*     endif.
*     if wa_wtjhr8_6 < 0.
*        wa_wtjhr8_6 = wa_wtjhr8_6 * -1.
*     endif.
*  ENDIF.

* change sign
  IF p_r3 = 'X'.
    wa_wtjhr1_6 = wa_wtjhr1_6 * -1.
    wa_wtjhr2_6 = wa_wtjhr2_6 * -1.
    wa_wtjhr3_6 = wa_wtjhr3_6 * -1.
    wa_wtjhr4_6 = wa_wtjhr4_6 * -1.
    wa_wtjhr5_6 = wa_wtjhr5_6 * -1.
    wa_wtjhr6_6 = wa_wtjhr6_6 * -1.
    wa_wtjhr7_6 = wa_wtjhr7_6 * -1.
    wa_wtjhr8_6 = wa_wtjhr8_6 * -1.
  ENDIF.

*
  wa_wtjhro_6 = wa_wtjhr1_6 + wa_wtjhr2_6 + wa_wtjhr3_6
              + wa_wtjhr4_6 + wa_wtjhr5_6 + wa_wtjhr6_6
              + wa_wtjhr7_6 + wa_wtjhr8_6.
**----2004/03/16
*  if wa_wtjhro_6 < 0.
*     wa_wtjhro_6 = wa_wtjhro_6 * -1.
*  endif.
*---Overall value process
  REFRESH : it_result.
  CLEAR   : it_result.
  CLEAR   : wa_text1.

  CONCATENATE wa_type p_reson '-' wa_text INTO wa_text1.

  PERFORM make_bdc_rtn USING :
                        'X'  'SAPMKBUD'        '0300',
                        ' '  'CODIA-AUFNR'     p_aufnr,
                        ' '  'BDC_OKCODE'      '/00'.

*---Annual value process
  DO 8 TIMES.
    wa_gjahr = wa_gjahr + 1.
    IF wa_year1 = wa_gjahr.
*     IF NOT wa_wtjhr1_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr1_6 wa_year1.
*     ENDIF.
    ELSEIF wa_year2 = wa_gjahr.
*     IF NOT wa_wtjhr2_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr2_6 wa_year2.
*     ENDIF.
    ELSEIF wa_year3 = wa_gjahr.
*     IF NOT wa_wtjhr3_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr3_6 wa_year3.
*     ENDIF.
    ELSEIF wa_year4 = wa_gjahr.
*     IF NOT wa_wtjhr4_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr4_6 wa_year4.
*     ENDIF.
    ELSEIF wa_year5 = wa_gjahr.
*     IF NOT wa_wtjhr5_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr5_6 wa_year5.
*     ENDIF.
    ELSEIF wa_year6 = wa_gjahr.
*     IF NOT wa_wtjhr6_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr6_6 wa_year6.
*     ENDIF.
    ELSEIF wa_year7 = wa_gjahr.
*     IF NOT wa_wtjhr7_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr7_6 wa_year7.
*     ENDIF.
    ELSEIF wa_year8 = wa_gjahr.
*     IF NOT wa_wtjhr8_6 IS INITIAL.
      PERFORM create_bdc USING wa_wtjhr8_6 wa_year8.
*     ENDIF.
    ENDIF.
  ENDDO.
* IF NOT wa_wtjhro_6 IS INITIAL.
  PERFORM create_bdc USING wa_wtjhro_6 '0'.
* ENDIF.
ENDFORM.                    " PROCESS_RTN
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WTJHR1_6  text
*----------------------------------------------------------------------*
FORM create_bdc USING    u_amt u_year.
  DATA : wk_amount(12).
*  WRITE U_AMT TO WK_AMOUNT CURRENCY 'KRW'.
  MOVE u_amt TO wk_amount.
  TRANSLATE  wk_amount  USING ', '.
  CONDENSE   wk_amount NO-GAPS.

*  IF u_year <> '0'.
  PERFORM make_bdc_rtn USING :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     u_year,
                 ' '  'BDC_OKCODE'      '=DROT'.
*  ENDIF.
  PERFORM make_bdc_rtn USING :
                        'X'  'SAPLKBPP'        '0320',
                        ' '  'BPDY-WERT1(01)'  wk_amount,
                        ' '  'BDC_OKCODE'      '/00'.
  IF u_year = '0'.
    IF p_r1 <> 'X'.
      PERFORM make_bdc_rtn USING :
                     'X'  'SAPLKBPP'        '0320',
                    ' '  'BDC_OKCODE'      '=BELE'.
      PERFORM make_bdc_rtn USING :
                     'X'  'SAPLKBPP'        '0702',
                     ' '  'BPDY-BLDAT'  wa_date,
                     ' '  'BPDY-SGTXT'  wa_text1,
                     ' '  'BDC_OKCODE'      '=ENTE'.
    ENDIF.
    PERFORM make_bdc_rtn USING :
                        'X'  'SAPLKBPP'        '0320',
                       ' '  'BDC_OKCODE'      '=POST'.
    CALL TRANSACTION tcode   USING it_bdc
                             MODE  p_mode
                             UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                      MESSAGES INTO it_messtab.

    READ TABLE it_messtab INDEX 1.
    IF it_messtab-msgtyp = 'S'.
      MOVE 'Success' TO it_result-text.
      MOVE u_year    TO it_result-year.
      MOVE it_messtab-msgv1 TO it_result-msgv1.
      MOVE 'O'     TO wa_budget.
    ELSEIF it_messtab-msgtyp = 'E'..
      MOVE 'Error'   TO it_result-text.
      MOVE u_year    TO it_result-year.
      MESSAGE s000(zmfi) WITH it_messtab-msgv1.
    ENDIF.
    IF u_year = '0'.
      MOVE 'Overall' TO it_result-year.
    ENDIF.
    APPEND it_result.
    CLEAR  it_result.
    REFRESH : it_bdc, it_messtab.
    CLEAR   : it_bdc, it_messtab.
  ENDIF.
ENDFORM.                 "CREATE_BDC
*&---------------------------------------------------------------------*
*&      Form  MAKE_BDC_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2288   text
*      -->P_2289   text
*      -->P_2290   text
*----------------------------------------------------------------------*
FORM make_bdc_rtn USING   dynbegin program dynpro.
  CLEAR it_bdc.

  IF dynbegin = 'X'.
    it_bdc-program  = program.
    it_bdc-dynpro   = dynpro.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam     = program.
    it_bdc-fval     = dynpro.
  ENDIF.

  APPEND it_bdc.
ENDFORM.                    " MAKE_BDC_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_rtn.
  PERFORM get_input_date.
  CLEAR wa_wtjhro_6.
  wa_wtjhro_6 = wa_wtjhr1_6 + wa_wtjhr2_6 + wa_wtjhr3_6
              + wa_wtjhr4_6 + wa_wtjhr5_6 + wa_wtjhr6_6
              + wa_wtjhr7_6 + wa_wtjhr8_6.
*----after budget check.
  IF p_r1 = 'X'.       "original
    wa_wtjhro_7 = wa_wtjhro_3 + wa_wtjhro_6 - wa_wtjhro_4.
    wa_wtjhr1_7 = wa_wtjhr1_3 + wa_wtjhr1_6 - wa_wtjhr1_4.
    wa_wtjhr2_7 = wa_wtjhr2_3 + wa_wtjhr2_6 - wa_wtjhr2_4.
    wa_wtjhr3_7 = wa_wtjhr3_3 + wa_wtjhr3_6 - wa_wtjhr3_4.
    wa_wtjhr4_7 = wa_wtjhr4_3 + wa_wtjhr4_6 - wa_wtjhr4_4.
    wa_wtjhr5_7 = wa_wtjhr5_3 + wa_wtjhr5_6 - wa_wtjhr5_4.
    wa_wtjhr6_7 = wa_wtjhr6_3 + wa_wtjhr6_6 - wa_wtjhr6_4.
    wa_wtjhr7_7 = wa_wtjhr7_3 + wa_wtjhr7_6 - wa_wtjhr7_4.
    wa_wtjhr8_7 = wa_wtjhr8_3 + wa_wtjhr8_6 - wa_wtjhr8_4.
  ELSEIF p_r2 = 'X'.  " supplement
    wa_wtjhro_7 = wa_wtjhro_5 + wa_wtjhro_6.
    wa_wtjhr1_7 = wa_wtjhr1_5 + wa_wtjhr1_6.
    wa_wtjhr2_7 = wa_wtjhr2_5 + wa_wtjhr2_6.
    wa_wtjhr3_7 = wa_wtjhr3_5 + wa_wtjhr3_6.
    wa_wtjhr4_7 = wa_wtjhr4_5 + wa_wtjhr4_6.
    wa_wtjhr5_7 = wa_wtjhr5_5 + wa_wtjhr5_6.
    wa_wtjhr6_7 = wa_wtjhr6_5 + wa_wtjhr6_6.
    wa_wtjhr7_7 = wa_wtjhr7_5 + wa_wtjhr7_6.
    wa_wtjhr8_7 = wa_wtjhr8_5 + wa_wtjhr8_6.
  ELSEIF p_r3 = 'X'.  " return
* change sign
*    wa_wtjhro_7 = wa_wtjhro_5 - wa_wtjhro_6.
*    wa_wtjhr1_7 = wa_wtjhr1_5 - wa_wtjhr1_6.
*    wa_wtjhr2_7 = wa_wtjhr2_5 - wa_wtjhr2_6.
*    wa_wtjhr3_7 = wa_wtjhr3_5 - wa_wtjhr3_6.
*    wa_wtjhr4_7 = wa_wtjhr4_5 - wa_wtjhr4_6.
*    wa_wtjhr5_7 = wa_wtjhr5_5 - wa_wtjhr5_6.
*    wa_wtjhr6_7 = wa_wtjhr6_5 - wa_wtjhr6_6.
*    wa_wtjhr7_7 = wa_wtjhr7_5 - wa_wtjhr7_6.
*    wa_wtjhr8_7 = wa_wtjhr8_5 - wa_wtjhr8_6.
    wa_wtjhro_7 = wa_wtjhro_5 + wa_wtjhro_6.
    wa_wtjhr1_7 = wa_wtjhr1_5 + wa_wtjhr1_6.
    wa_wtjhr2_7 = wa_wtjhr2_5 + wa_wtjhr2_6.
    wa_wtjhr3_7 = wa_wtjhr3_5 + wa_wtjhr3_6.
    wa_wtjhr4_7 = wa_wtjhr4_5 + wa_wtjhr4_6.
    wa_wtjhr5_7 = wa_wtjhr5_5 + wa_wtjhr5_6.
    wa_wtjhr6_7 = wa_wtjhr6_5 + wa_wtjhr6_6.
    wa_wtjhr7_7 = wa_wtjhr7_5 + wa_wtjhr7_6.
    wa_wtjhr8_7 = wa_wtjhr8_5 + wa_wtjhr8_6.
  ENDIF.
*========Order budget check
*===Return ----> order actual value
  IF p_r3 = 'X'.
    CLEAR : wa_actual.
    PERFORM get_actual_data USING wa_actual.
  ENDIF.
*---2004/04/01
  CLEAR : wa_tot_amt.
  LOOP AT it_pi2.
    CLEAR : wa_amt.
*======= Supplement ---> available - input amount
    IF p_r2 = 'X'.
      CASE it_pi2-gjahr.
        WHEN  '1111'.
          wa_amt = it_pi2-amt4 - wa_wtjhro_6.
          wa_tot_amt = wa_tot_amt + it_pi2-amt4.
        WHEN  wa_year1.
          wa_amt = it_pi2-amt4 - wa_wtjhr1_6.
        WHEN  wa_year2.
          wa_amt = it_pi2-amt4 - wa_wtjhr2_6.
        WHEN  wa_year3.
          wa_amt = it_pi2-amt4 - wa_wtjhr3_6.
        WHEN  wa_year4.
          wa_amt = it_pi2-amt4 - wa_wtjhr4_6.
        WHEN  wa_year5.
          wa_amt = it_pi2-amt4 - wa_wtjhr5_6.
        WHEN  wa_year6.
          wa_amt = it_pi2-amt4 - wa_wtjhr6_6.
        WHEN  wa_year7.
          wa_amt = it_pi2-amt4 - wa_wtjhr7_6.
        WHEN  wa_year8.
          wa_amt = it_pi2-amt4 - wa_wtjhr8_6.
      ENDCASE.
    ENDIF.
*=======Original---> available - after amount
    IF p_r1 = 'X'.
      CASE it_pi2-gjahr.
        WHEN  '1111'.
          wa_amt = it_pi2-amt4 - wa_wtjhro_7.
          wa_tot_amt = wa_tot_amt + it_pi2-amt4.
        WHEN  wa_year1.
          wa_amt = it_pi2-amt4 - wa_wtjhr1_7.
        WHEN  wa_year2.
          wa_amt = it_pi2-amt4 - wa_wtjhr2_7.
        WHEN  wa_year3.
          wa_amt = it_pi2-amt4 - wa_wtjhr3_7.
        WHEN  wa_year4.
          wa_amt = it_pi2-amt4 - wa_wtjhr4_7.
        WHEN  wa_year5.
          wa_amt = it_pi2-amt4 - wa_wtjhr5_7.
        WHEN  wa_year6.
          wa_amt = it_pi2-amt4 - wa_wtjhr6_7.
        WHEN  wa_year7.
          wa_amt = it_pi2-amt4 - wa_wtjhr7_7.
        WHEN  wa_year8.
          wa_amt = it_pi2-amt4 - wa_wtjhr8_7.
      ENDCASE.
    ENDIF.
*=======Return---> input value <= Current budget - actual
    IF p_r3 = 'X'.
* change sign
*      CASE it_pi2-gjahr.
*        WHEN  '1111'.
*          wa_amt =  ( it_pi2-amt2 - wa_actual ) - wa_wtjhro_6.
*        WHEN  wa_year1.
*          read table it_act_sum with key gjahr = wa_year1.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr1_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr1_6.
*          endif.
*        WHEN  wa_year2.
*          read table it_act_sum with key gjahr = wa_year2.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr2_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr2_6.
*          endif.
*        WHEN  wa_year3.
*          read table it_act_sum with key gjahr = wa_year3.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr3_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr3_6.
*          endif.
*        WHEN  wa_year4.
*          read table it_act_sum with key gjahr = wa_year4.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr4_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr4_6.
*          endif.
*        WHEN  wa_year5.
*          read table it_act_sum with key gjahr = wa_year5.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr5_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr5_6.
*          endif.
*        WHEN  wa_year6.
*          read table it_act_sum with key gjahr = wa_year6.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr6_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr6_6.
*          endif.
*        WHEN  wa_year7.
*          read table it_act_sum with key gjahr = wa_year7.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr7_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr1_7.
*          endif.
*        WHEN  wa_year8.
*          read table it_act_sum with key gjahr = wa_year8.
*          if sy-subrc = 0.
*             wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) - wa_wtjhr8_6.
*          else.
*             wa_amt =  it_pi2-amt2 - wa_wtjhr8_6.
*          endif.
*      ENDCASE.
      CASE it_pi2-gjahr.
        WHEN  '1111'.
          wa_amt =  ( it_pi2-amt2 - wa_actual ) + wa_wtjhro_6.
        WHEN  wa_year1.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year1.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr1_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr1_6.
          ENDIF.
        WHEN  wa_year2.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year2.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr2_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr2_6.
          ENDIF.
        WHEN  wa_year3.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year3.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr3_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr3_6.
          ENDIF.
        WHEN  wa_year4.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year4.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr4_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr4_6.
          ENDIF.
        WHEN  wa_year5.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year5.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr5_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr5_6.
          ENDIF.
        WHEN  wa_year6.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year6.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr6_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr6_6.
          ENDIF.
        WHEN  wa_year7.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year7.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr7_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr1_7.
          ENDIF.
        WHEN  wa_year8.
          READ TABLE it_act_sum WITH KEY gjahr = wa_year8.
          IF sy-subrc = 0.
            wa_amt =  ( it_pi2-amt2 - it_act_sum-tot ) + wa_wtjhr8_6.
          ELSE.
            wa_amt =  it_pi2-amt2 + wa_wtjhr8_6.
          ENDIF.
      ENDCASE.

    ENDIF.
*********************wwwwww 2004/03/16
    IF wa_amt >= 0.
      it_pi2-icon = '@08@'. "ICON_GREEN_LIGHT.
    ELSE.
*      it_pi2-icon = '@0A@'. "ICON_RED_LIGHT.
      it_pi2-icon = '@09@'. "YELLOW_LIGHT.
**=====2004/03/12 jhs modify
      IF it_pi2-gjahr = '1111'.
        MOVE 'X'  TO wa_ok.
      ENDIF.
*========*
    ENDIF.
    MODIFY it_pi2. " INDEX sy-tabix.
    CLEAR wa_amt.

  ENDLOOP.
*----2004/04/01
  IF p_r2 = 'X'.
    wa_tot_amt = wa_tot_amt - wa_wtjhro_6.
  ENDIF.
  IF p_r1 = 'X'.
    wa_tot_amt = wa_tot_amt - wa_wtjhro_7.
  ENDIF.
*------
  PERFORM write_pro_agin.
ENDFORM.                    " CHECK_DATA_RTN
*&---------------------------------------------------------------------*
*&      Form  GET_INPUT_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_input_date.
  CLEAR : wa_d_wtjhr1_6, wa_d_wtjhr2_6, wa_d_wtjhr3_6,
          wa_d_wtjhr4_6, wa_d_wtjhr5_6, wa_d_wtjhr6_6,
          wa_d_wtjhr7_6, wa_d_wtjhr8_6.
  wa_wtjhr1_6 = 0.
  wa_wtjhr2_6 = 0.
  wa_wtjhr3_6 = 0.
  wa_wtjhr4_6 = 0.
  wa_wtjhr5_6 = 0.
  wa_wtjhr6_6 = 0.
  wa_wtjhr7_6 = 0.
  READ  LINE 10 FIELD VALUE   wa_wtjhr1_6  INTO wa_d_wtjhr1_6
                              wa_wtjhr2_6  INTO wa_d_wtjhr2_6
                              wa_wtjhr3_6  INTO wa_d_wtjhr3_6
                              wa_wtjhr4_6  INTO wa_d_wtjhr4_6
                              wa_wtjhr5_6  INTO wa_d_wtjhr5_6
                              wa_wtjhr6_6  INTO wa_d_wtjhr6_6
                              wa_wtjhr7_6  INTO wa_d_wtjhr7_6
                              wa_wtjhr8_6  INTO wa_d_wtjhr8_6.
  CLEAR : wa_wtjhr1_6, wa_wtjhr2_6, wa_wtjhr3_6,
          wa_wtjhr4_6, wa_wtjhr5_6, wa_wtjhr6_6,
          wa_wtjhr7_6, wa_wtjhr8_6.
  wa_wtjhr1_6 = 0.
  wa_wtjhr2_6 = 0.
  wa_wtjhr3_6 = 0.
  wa_wtjhr4_6 = 0.
  wa_wtjhr5_6 = 0.
  wa_wtjhr6_6 = 0.
  wa_wtjhr7_6 = 0.
  TRANSLATE  wa_d_wtjhr1_6  USING ', '.
  CONDENSE   wa_d_wtjhr1_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr2_6  USING ', '.
  CONDENSE   wa_d_wtjhr2_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr3_6  USING ', '.
  CONDENSE   wa_d_wtjhr3_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr4_6  USING ', '.
  CONDENSE   wa_d_wtjhr4_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr5_6  USING ', '.
  CONDENSE   wa_d_wtjhr5_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr6_6  USING ', '.
  CONDENSE   wa_d_wtjhr6_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr7_6  USING ', '.
  CONDENSE   wa_d_wtjhr7_6 NO-GAPS.
  TRANSLATE  wa_d_wtjhr8_6  USING ', '.
  CONDENSE   wa_d_wtjhr8_6 NO-GAPS.


  wa_wtjhr1_6 = wa_d_wtjhr1_6.                              " * 100.
  wa_wtjhr2_6 = wa_d_wtjhr2_6.                              " * 100.
  wa_wtjhr3_6 = wa_d_wtjhr3_6.                              " * 100.
  wa_wtjhr4_6 = wa_d_wtjhr4_6.                              " * 100.
  wa_wtjhr5_6 = wa_d_wtjhr5_6.                              " * 100.
  wa_wtjhr6_6 = wa_d_wtjhr6_6.                              " * 100.
  wa_wtjhr7_6 = wa_d_wtjhr7_6.                              " * 100.
  wa_wtjhr8_6 = wa_d_wtjhr8_6.                              " * 100.

*---GET TEXT
  READ LINE 12 FIELD VALUE wa_text INTO wa_text.

ENDFORM.                    " GET_INPUT_DATE
*&---------------------------------------------------------------------*
*&      Form  create_ALL_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WTJHRO_6  text
*      -->P_2143   text
*----------------------------------------------------------------------*
FORM create_all_bdc USING    u_amt
                             u_year.
  DATA : wk_amount1(12).
*  WRITE U_AMT TO WK_AMOUNT CURRENCY 'KRW'.
  MOVE u_amt TO wk_amount1.
  TRANSLATE  wk_amount1  USING ', '.
  CONDENSE   wk_amount1 NO-GAPS.
  PERFORM make_bdc_rtn USING :
                        'X'  'SAPMKBUD'        '0300',
                        ' '  'CODIA-AUFNR'     p_aufnr,
                        ' '  'BDC_OKCODE'      '/00',
                        'X'  'SAPLKBPP'        '0320',
                        ' '  'BPDY-WERT1(01)'  wk_amount1,
                        ' '  'BDC_OKCODE'      '=POST'.
  CALL TRANSACTION tcode   USING it_bdc
                           MODE  'N'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.
  REFRESH : it_bdc.
  CLEAR   : it_bdc.


ENDFORM.                    " create_ALL_bdc
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_data.
*--Org
  IF p_r1 = 'X'.
    wa_wtjhro_6 = wa_wtjhro_2.
    wa_wtjhr1_6 = wa_wtjhr1_2.
    wa_wtjhr2_6 = wa_wtjhr2_2.
    wa_wtjhr3_6 = wa_wtjhr3_2.
    wa_wtjhr4_6 = wa_wtjhr4_2.
    wa_wtjhr5_6 = wa_wtjhr5_2.
    wa_wtjhr6_6 = wa_wtjhr6_2.
    wa_wtjhr7_6 = wa_wtjhr7_2.
    wa_wtjhr8_6 = wa_wtjhr8_2.
**--Supplement
*  ELSEIF p_r2 = 'X'.
*    wa_wtjhro_6 = wa_wtjhro_3.
*    wa_wtjhr1_6 = wa_wtjhr1_3.
*    wa_wtjhr2_6 = wa_wtjhr2_3.
*    wa_wtjhr3_6 = wa_wtjhr3_3.
*    wa_wtjhr4_6 = wa_wtjhr4_3.
*    wa_wtjhr5_6 = wa_wtjhr5_3.
*    wa_wtjhr6_6 = wa_wtjhr6_3.
*    wa_wtjhr7_6 = wa_wtjhr7_3.
*    wa_wtjhr8_6 = wa_wtjhr8_3.
*
*  ELSEIF p_r3 = 'X'.
*    wa_wtjhro_6 = wa_wtjhro_4.
*    wa_wtjhr1_6 = wa_wtjhr1_4.
*    wa_wtjhr2_6 = wa_wtjhr2_4.
*    wa_wtjhr3_6 = wa_wtjhr3_4.
*    wa_wtjhr4_6 = wa_wtjhr4_4.
*    wa_wtjhr5_6 = wa_wtjhr5_4.
*    wa_wtjhr6_6 = wa_wtjhr6_4.
*    wa_wtjhr7_6 = wa_wtjhr7_4.
*    wa_wtjhr8_6 = wa_wtjhr8_4.
  ENDIF.

ENDFORM.                    " MOVE_DATA
*&---------------------------------------------------------------------*
*&      Form  POV_RESON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pov_reson.

  MOVE  ' ' TO p_r1.
******************************
  it_dynpfields-fieldname = 'P_R1'.
  APPEND it_dynpfields.
  CLEAR it_dynpfields.

  it_dynpfields-fieldname = 'P_R2'.
  APPEND it_dynpfields.
  CLEAR it_dynpfields.

  it_dynpfields-fieldname = 'P_R3'.
  APPEND it_dynpfields.
  CLEAR it_dynpfields.

* ????? ?? ?? call

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
      determine_loop_index = 'X'
    TABLES
      dynpfields           = it_dynpfields.

* ??? ???? ? ?? check..

  READ TABLE it_dynpfields INDEX 1.
  IF NOT it_dynpfields-fieldvalue IS INITIAL.
    p_r1 = it_dynpfields-fieldvalue.
*    WA_ZGBID = 'A%'.
  ENDIF.

  READ TABLE it_dynpfields INDEX 2.
  IF NOT it_dynpfields-fieldvalue IS INITIAL.
    p_r2 = it_dynpfields-fieldvalue.
*    WA_ZGBID = 'B%'.
  ENDIF.

  READ TABLE it_dynpfields INDEX 3.
  IF NOT it_dynpfields-fieldvalue IS INITIAL.
    p_r3 = it_dynpfields-fieldvalue.
*   WA_ZGBID = 'C%'.
  ENDIF.
*****************************************************
  REFRESH : it_value, it_fields.
  CLEAR   : it_value, it_fields.
*---2004/03/16
  IF p_r1 = 'X'.       "Original
    SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_value
  FROM ztfi_reason
  WHERE type = '1'.
    it_fields-tabname = 'ZTFI_REASON'.
    it_fields-fieldname = 'RESON'.
    it_fields-selectflag = 'X'.
    APPEND it_fields.

    it_fields-tabname = 'ZTFI_REASON'.
    it_fields-fieldname = 'DESCR'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    MOVE ' ' TO  p_r1.
  ELSEIF p_r2 = 'X'.       "Supplement
    SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_value
  FROM ztfi_reason
  WHERE type = '2'.
    it_fields-tabname = 'ZTFI_REASON'.
    it_fields-fieldname = 'RESON'.
    it_fields-selectflag = 'X'.
    APPEND it_fields.

    it_fields-tabname = 'ZTFI_REASON'.
    it_fields-fieldname = 'DESCR'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    MOVE ' ' TO  p_r2.

  ELSEIF p_r3 = 'X'.     "Return
    SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_value
    FROM ztfi_reason
    WHERE type = '3'.
    it_fields-tabname = 'ZTFI_REASON'.
    it_fields-fieldname = 'RESON'.
    it_fields-selectflag = 'X'.
    APPEND it_fields.

    it_fields-tabname = 'ZTFI_REASON'.
    it_fields-fieldname = 'DESCR'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    MOVE ' ' TO p_r3.
  ENDIF.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
    EXPORTING
      selectfield                  = selectfield
    IMPORTING
      ind                          = ld_tabix
      select_value                 = select_value
    TABLES
      fields                       = it_fields
      full_table                   = it_value
    EXCEPTIONS
      full_table_empty             = 1
      no_tablestructure_given      = 2
      no_tablefields_in_dictionary = 3
      more_then_one_selectfield    = 4
      no_selectfield               = 5
      OTHERS                       = 6.

  CHECK NOT ld_tabix IS INITIAL.
  READ TABLE it_value INDEX ld_tabix.
  p_reson = select_value.
  CLEAR : p_r1, p_r2, p_r3.
ENDFORM.                    " POV_RESON
*&---------------------------------------------------------------------*
*&      Form  write_pro_AGIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_pro_agin.
  sy-lsind = sy-lsind - 1.
  ULINE AT (wa_width).
  WRITE : /01 'Order.no'   COLOR COL_KEY,
           12 'Activity'   COLOR COL_KEY,
           30 'Overall'    COLOR COL_KEY,
           49(4) wa_year1  COLOR COL_KEY,
           65(4) wa_year2  COLOR COL_KEY,
           81(4) wa_year3  COLOR COL_KEY,
           97(4) wa_year4  COLOR COL_KEY,
          113(4) wa_year5  COLOR COL_KEY,
          129(4) wa_year6  COLOR COL_KEY,
          145(4) wa_year7  COLOR COL_KEY,
          161(4) wa_year8  COLOR COL_KEY.

  ULINE AT (wa_width).
*---Plan
  WRITE : /01(10)  p_aufnr COLOR COL_GROUP,
           12(10)  wa_text_1,
           23(15)  wa_wtjhro_1 NO-ZERO,
           39(15)  wa_wtjhr1_1 NO-ZERO,
           55(15)  wa_wtjhr2_1 NO-ZERO,
           71(15)  wa_wtjhr3_1 NO-ZERO,
           87(15)  wa_wtjhr4_1 NO-ZERO,
          103(15)  wa_wtjhr5_1 NO-ZERO,
          119(15)  wa_wtjhr6_1 NO-ZERO,
          135(15)  wa_wtjhr7_1 NO-ZERO,
          151(15)  wa_wtjhr8_1 NO-ZERO.

*---Org Budget
  WRITE : /12(10)  wa_text_2,
           23(15)  wa_wtjhro_2 NO-ZERO,
           39(15)  wa_wtjhr1_2 NO-ZERO,
           55(15)  wa_wtjhr2_2 NO-ZERO,
           71(15)  wa_wtjhr3_2 NO-ZERO,
           87(15)  wa_wtjhr4_2 NO-ZERO,
          103(15)  wa_wtjhr5_2 NO-ZERO,
          119(15)  wa_wtjhr6_2 NO-ZERO,
          135(15)  wa_wtjhr7_2 NO-ZERO,
          151(15)  wa_wtjhr8_2 NO-ZERO.

*---Supplement
  WRITE : /12(10)  wa_text_3,
           23(15)  wa_wtjhro_3 NO-ZERO,
           39(15)  wa_wtjhr1_3 NO-ZERO,
           55(15)  wa_wtjhr2_3 NO-ZERO,
           71(15)  wa_wtjhr3_3 NO-ZERO,
           87(15)  wa_wtjhr4_3 NO-ZERO,
          103(15)  wa_wtjhr5_3 NO-ZERO,
          119(15)  wa_wtjhr6_3 NO-ZERO,
          135(15)  wa_wtjhr7_3 NO-ZERO,
          151(15)  wa_wtjhr8_3 NO-ZERO.

*---Return
  WRITE : /12(10)  wa_text_4,
           23(15)  wa_wtjhro_4 NO-ZERO,
           39(15)  wa_wtjhr1_4 NO-ZERO,
           55(15)  wa_wtjhr2_4 NO-ZERO,
           71(15)  wa_wtjhr3_4 NO-ZERO,
           87(15)  wa_wtjhr4_4 NO-ZERO,
          103(15)  wa_wtjhr5_4 NO-ZERO,
          119(15)  wa_wtjhr6_4 NO-ZERO,
          135(15)  wa_wtjhr7_4 NO-ZERO,
          151(15)  wa_wtjhr8_4 NO-ZERO.
*---Curren Budget
  WRITE : /12(10)  wa_text_5,
           23(15)  wa_wtjhro_5 NO-ZERO  COLOR COL_TOTAL,
           39(15)  wa_wtjhr1_5 NO-ZERO  COLOR COL_TOTAL,
           55(15)  wa_wtjhr2_5 NO-ZERO  COLOR COL_TOTAL,
           71(15)  wa_wtjhr3_5 NO-ZERO  COLOR COL_TOTAL,
           87(15)  wa_wtjhr4_5 NO-ZERO  COLOR COL_TOTAL,
          103(15)  wa_wtjhr5_5 NO-ZERO  COLOR COL_TOTAL,
          119(15)  wa_wtjhr6_5 NO-ZERO  COLOR COL_TOTAL,
          135(15)  wa_wtjhr7_5 NO-ZERO  COLOR COL_TOTAL,
          151(15)  wa_wtjhr8_5 NO-ZERO  COLOR COL_TOTAL.

  ULINE AT (wa_width).
*---value  input
  WRITE : /12(10)  wa_text_6,
           23(15)  wa_wtjhro_6 , "INPUT ON,
           39(15)  wa_wtjhr1_6 INPUT ON NO-ZERO,
           55(15)  wa_wtjhr2_6 INPUT ON NO-ZERO,
           71(15)  wa_wtjhr3_6 INPUT ON NO-ZERO,
           87(15)  wa_wtjhr4_6 INPUT ON NO-ZERO,
          103(15)  wa_wtjhr5_6 INPUT ON NO-ZERO,
          119(15)  wa_wtjhr6_6 INPUT ON NO-ZERO,
          135(15)  wa_wtjhr7_6 INPUT ON NO-ZERO,
          151(15)  wa_wtjhr8_6 INPUT ON NO-ZERO.
* wa_d_wtjhr1_6 = wa_wtjhr1_6.
*---After Budget
  WRITE : /12(10)  wa_text_7,
           23(15)  wa_wtjhro_7 NO-ZERO,
           39(15)  wa_wtjhr1_7 NO-ZERO,
           55(15)  wa_wtjhr2_7 NO-ZERO,
           71(15)  wa_wtjhr3_7 NO-ZERO,
           87(15)  wa_wtjhr4_7 NO-ZERO,
          103(15)  wa_wtjhr5_7 NO-ZERO,
          119(15)  wa_wtjhr6_7 NO-ZERO,
          135(15)  wa_wtjhr7_7 NO-ZERO,
          151(15)  wa_wtjhr8_7 NO-ZERO.
*--Text
  WRITE : /12(10) 'Text',
           23(142) wa_text INPUT ON.
  ULINE AT (wa_width).
*=======PI
  WRITE : /1(10)  'Condition' COLOR COL_KEY,
         13(15) 'Position Number' COLOR COL_KEY,
         32(04)  'Year' COLOR COL_KEY,
         49(08)   'Orig.Bud' COLOR COL_KEY,
         67(08)  'Curr.Bud' COLOR COL_KEY,
         88(06)  'Assign' COLOR COL_KEY,
        103(09)  'Available' COLOR COL_KEY,
        119(04)  'Rate' COLOR COL_KEY,
        132(08)  'I/O Plan' COLOR COL_KEY.
*        150(07)  'Ord.Bud' COLOR COL_KEY.
*       146(10)  'Condition' COLOR COL_KEY.

  ULINE AT (wa_width).
*====*

*----PI WRITE-----*
  SORT it_pi2 BY posid gjahr ASCENDING.

  LOOP AT it_pi2.

*----get i/o plan
    CASE it_pi2-gjahr.
      WHEN '1111'.
        MOVE wa_wtjhro_1 TO   it_pi2-amt5.
      WHEN  wa_year1.
        MOVE wa_wtjhr1_1  TO  it_pi2-amt5.
      WHEN  wa_year2.
        MOVE wa_wtjhr2_1  TO  it_pi2-amt5.
      WHEN  wa_year3.
        MOVE wa_wtjhr3_1  TO  it_pi2-amt5.
      WHEN  wa_year4.
        MOVE wa_wtjhr4_1  TO  it_pi2-amt5.
      WHEN  wa_year5.
        MOVE wa_wtjhr5_1  TO  it_pi2-amt5.
      WHEN  wa_year6.
        MOVE wa_wtjhr6_1  TO  it_pi2-amt5.
      WHEN  wa_year7.
        MOVE wa_wtjhr7_1  TO  it_pi2-amt5.
      WHEN  wa_year8.
        MOVE wa_wtjhr8_1  TO  it_pi2-amt5.
    ENDCASE.
    IF it_pi2-gjahr = '1111'.
      wa_gjahr = 'Overall'.
    ELSE.
      wa_gjahr = it_pi2-gjahr.
    ENDIF.
    it_pi2-amt4 = it_pi2-amt2 - it_pi2-amt3 - it_pi2-amt9.
    MODIFY it_pi2 INDEX sy-tabix.
    WRITE : /1(04) it_pi2-icon AS ICON CENTERED NO-GAP.
    WRITE : 13(16) it_pi2-posid COLOR COL_NORMAL,
            32(07) wa_gjahr, "it_pi-gjahr,
            43(15) it_pi2-amt1,   "ori.bud
            61(15) it_pi2-amt2,
            80(15) it_pi2-amt3,   "assign
            98(15) it_pi2-amt4,   "available
           115(10) it_pi2-rate,
           126(15) it_pi2-amt5.   "i/o plan
*           143(15) it_pi2-amt6.
    CASE it_pi2-sign.
      WHEN '4'. WRITE: 10(3) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'.
        WRITE: 10(3) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
        SORT it_order  ASCENDING BY posid aufnr.
        LOOP AT it_order WHERE  posid = it_pi2-posid
                          AND   gjahr = it_pi2-gjahr.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = it_order-aufnr
            IMPORTING
              output = it_order-aufnr.

          WRITE : /20(10) it_order-aufnr COLOR COL_TOTAL NO-ZERO,
*                          34(07) it_order-gjahr,
                   43(15) it_order-wtjhr.
        ENDLOOP.
    ENDCASE.

    AT END OF posid.
      ULINE AT :/(wa_width).
    ENDAT.
  ENDLOOP.

ENDFORM.                    " write_pro_AGIN
*&---------------------------------------------------------------------*
*&      Form  FUND_BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fund_bdc_process.
  DATA: l_kostv(10) TYPE c.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_kostv
    IMPORTING
      output = l_kostv.


  SORT it_pi2 DESCENDING BY rate.
  READ TABLE it_pi2 INDEX 1.
  IF sy-subrc = 0.
    wa_posid = it_pi2-posid+1(11).
  ENDIF.
  REFRESH : it_bdc.
  CLEAR   : it_bdc.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLFRC1'        '0030',
                      ' '  'FMII1-AUFNR'     p_aufnr,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLFRC1'        '0032',
                      ' '  'FMII1-FISTL(01)'  l_kostv, "CC
                      ' '  'FMII1-FONDS(01)'  wa_posid, "it_pi2-posid,
                      ' '  'BDC_OKCODE'      '=SAVE'.
  CALL TRANSACTION 'FRC5'   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.


ENDFORM.                    " FUND_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  modify_symbol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_symbol.

  CALL FUNCTION 'CONVERSION_EXIT_POSID_INPUT'
    EXPORTING
      input  = wa_posid1
    IMPORTING
      output = wa_posid1.

  READ TABLE it_pi2 WITH KEY posid = wa_posid1 gjahr = wa_gjahr11.
*  INDEX wa_line.
  IF sy-subrc = 0.
    CASE wa_value.
      WHEN '4'.
        it_pi2-sign = '5'.
      WHEN '5'.
        it_pi2-sign = '4'.
    ENDCASE.
    MODIFY it_pi2 TRANSPORTING sign WHERE gjahr = it_pi2-gjahr.
  ENDIF.
ENDFORM.                    " modify_symbol
*&---------------------------------------------------------------------*
*&      Form  check_reason_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CNT  text
*----------------------------------------------------------------------*
FORM check_reason_code USING    u_chk u_cnt.
  CLEAR : u_cnt.
  SELECT  COUNT(*) INTO wa_cnt
FROM ztfi_reason
WHERE type = u_chk
AND   reson = p_reson.

ENDFORM.                    " check_reason_code
*&---------------------------------------------------------------------*
*&      Form  get_actual_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ACTUAL  text
*----------------------------------------------------------------------*
FORM get_actual_data USING    u_actual.

  REFRESH : it_actual, it_act_sum.
  CLEAR   : it_actual, it_act_sum.
  CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
    EXPORTING
      input  = p_aufnr
    IMPORTING
      output = p_aufnr.

  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      aufnr         = p_aufnr
   IMPORTING
     amt           =  u_actual
    TABLES
      out           = it_actual
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT it_actual.
    MOVE-CORRESPONDING it_actual TO it_act_sum.
    COLLECT it_act_sum.
    CLEAR   it_act_sum.
  ENDLOOP.
ENDFORM.                    " get_actual_data
*&---------------------------------------------------------------------*
*&      Form  get_it_act_n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_IMZO2_OBJNR  text
*----------------------------------------------------------------------*
FORM get_it_act_n USING    p_objnr.
  DATA $it_act_n LIKE it_act_n OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      objnr = p_objnr
    TABLES
      out   = $it_act_n.

  IF sy-subrc EQ 0.
    APPEND LINES OF $it_act_n TO it_act_n.
  ENDIF.

ENDFORM.                    " get_it_act_n
*&---------------------------------------------------------------------*
*&      Form  INSERT_FMDERIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_fmderive .

  DATA : ls_aufk TYPE aufk.
*  DATA : ls_fm37 TYPE FMFMOAUD13000013.

* by ig.moon Feb.22-2012 {
  TYPES: BEGIN OF ty_fm,
          mandt TYPE mandt,
          sour1_from	TYPE aufnr,
          sour1_to TYPE aufnr,
          valid_from	TYPE abaintab,
          target1 TYPE fm_fipex,
          target2 TYPE fistl,
          target3 TYPE bp_geber,
          delete_flg TYPE abadrdelflag,
          added_by TYPE abadrerfasser,
          added_on TYPE erdat,
     END OF ty_fm.

  TYPES ty_fm37 TYPE STANDARD TABLE OF ty_fm.

  DATA  ls_fm37 TYPE ty_fm37 WITH HEADER LINE.
* }

  DATA : l_target TYPE fistl.
  DATA : l_aufnr TYPE aufnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_aufnr
    IMPORTING
      output = l_aufnr.

  SELECT SINGLE * INTO ls_aufk
    FROM aufk
   WHERE aufnr = l_aufnr.

  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'Please check order No.'.
    STOP.
  ENDIF.

* by ig.moon Feb.22-2012 {

  DATA : fm_tab_name7 TYPE abadrparam.
  DATA : fm_tab_name8 TYPE abadrparam.

  SELECT SINGLE param_1 FROM tabadrs INTO fm_tab_name7
  WHERE applclass = 'FM'
  AND subclass = '01'
  AND abadrstratid = 'FMOA'
  AND abadrenv = 'SAP'
  AND step_no = '7'.

  SELECT SINGLE param_1 FROM tabadrs INTO fm_tab_name8
  WHERE applclass = 'FM'
  AND subclass = '01'
  AND abadrstratid = 'FMOA'
  AND abadrenv = 'SAP'
  AND step_no = '8'.
* }

  SELECT SINGLE target2 INTO l_target
    FROM (fm_tab_name7)
   WHERE sour1_from = 'H201'
     AND sour2_from = ls_aufk-kostv                         "UD1K954177
*    AND sour2_from = ls_aufk-akstl                         "UD1K954177
     AND valid_from <= sy-datum.

  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'No matching Funds Center in FMDERIVE!'.
    STOP.
  ENDIF.

  CLEAR : it_impr.

  READ TABLE it_impr INDEX 1.

  ls_fm37-sour1_from = ls_aufk-aufnr.
  ls_fm37-sour1_to = ls_aufk-aufnr.
  ls_fm37-valid_from = '00010101'.
  ls_fm37-target2 = l_target.
  ls_fm37-target3 = it_impr-posid+1(10).
  ls_fm37-added_by = sy-uname.
  ls_fm37-added_on = sy-datum.

  MODIFY (fm_tab_name8) FROM ls_fm37.

  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE i000 WITH 'Order assigned to Fund' it_impr-posid+1(10)
                      ' / Fctr ' l_target.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i000 WITH 'Update error'.
  ENDIF.

ENDFORM.                    " INSERT_FMDERIVE
