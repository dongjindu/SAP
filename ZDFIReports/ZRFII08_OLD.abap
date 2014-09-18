*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 06/10/2003
*& Specification By       : hs.jeong
*& Pattern                : Report 1-2
*& Development Request No : UD1K904466
*& Addl documentation     :
*& Description  : [FI-IM] IM Progress Report
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT zrfii08 MESSAGE-ID  zmfi NO STANDARD PAGE HEADING
                               LINE-SIZE 250.
.
TYPE-POOLS: slis, vrm.
INCLUDE <icon>.
INCLUDE <symbol>.

CONSTANTS : h_item(16)    VALUE   'Item',
            h_txt50(20)   VALUE  'Description',
            h_plan(14)    VALUE  'Plan',
            h_org(14)     VALUE  'Orig. Budget',
            h_sup(14)     VALUE  'Supplement',
            h_ret(14)     VALUE  'Return',
            h_cur(14)     VALUE  'Current',
            h_io(14)      VALUE  'IO.Budget',

            h_act(14)     VALUE  'Actual',
            h_diffa(14)   VALUE  'Different',
            h_commit(14)  VALUE  'Commitment',
            h_post(14)    VALUE  'Postpone',
            h_pur(14)     VALUE  'Pur.saving',
            h_pro(14)     VALUE  'Pro.saving',
            h_difa(15)    VALUE  '(act - Org.bud)',
            h_year(14)    VALUE  'dif. yr Plan',
            h_other(14)   VALUE  'Other rea',
            h_org_d(14)   VALUE  'Original',
            h_supp_d(14)  VALUE  'Supplement',
            h_ret_d(14)   VALUE  'Return'.
*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
*
TABLES: aufk, impr, fmfctr, ripasw, codia, bkpf, ztfi_imfm,bpbk.

DATA: it_out  TYPE TABLE OF impr WITH HEADER LINE.
DATA: it_imzo TYPE TABLE OF imzo WITH HEADER LINE.
*DATA: it_impr TYPE TABLE OF impr WITH HEADER LINE.
DATA : BEGIN OF it_impr OCCURS 0,
          posid LIKE impr-posid,
          posnr LIKE impr-posnr,
          gjahr LIKE impr-gjahr,
          post1 LIKE impu-post1,
          prnam LIKE impr-prnam,
          objnr LIKE impr-objnr,
       END OF it_impr.

DATA: it_imfm TYPE TABLE OF ztfi_imfm WITH HEADER LINE.
DATA: it_aufk TYPE TABLE OF aufk WITH HEADER LINE.

*
DATA : it_io_budget LIKE zfi_io_budget OCCURS 0 WITH HEADER LINE.
DATA : it_io_actual LIKE zfi_io_actual OCCURS 0 WITH HEADER LINE.

DATA : it_budget LIKE zfi_pi_budget OCCURS 0 WITH HEADER LINE.
DATA : it_actual LIKE zfi_pi_actual_act OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_out OCCURS 0,
        posid        LIKE  impr-posid,
        objnr        LIKE  impr-objnr,
*        kostl        LIKE  impr-kostl,
*        gjahr        LIKE  impr-gjahr,
        aufnr        LIKE  aufk-aufnr,
        txt50(30),
        prnam        LIKE  ztfi_imfm-prnam,
        plan         LIKE  ztfi_imfm-tot,
        org_amt      LIKE  ztfi_imfm-tot,
        sup_amt      LIKE  ztfi_imfm-tot,
        ret_amt      LIKE  ztfi_imfm-tot,
        cur_amt      LIKE  ztfi_imfm-tot,
        io_amt       LIKE  ztfi_imfm-tot,
        pr_amt       LIKE  ztfi_imfm-tot,
        po_amt       LIKE  ztfi_imfm-tot,
        act_amt      LIKE  ztfi_imfm-tot,
        ass_amt      LIKE  ztfi_imfm-tot,
        res_amt      LIKE  ztfi_imfm-tot,
        difa_amt     LIKE  ztfi_imfm-tot,
        difb_amt     LIKE  ztfi_imfm-tot,
        dif_amt      LIKE  ztfi_imfm-tot,
        chkbox       TYPE c,
        light        TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
        icon(4),
        sign(2),
      END OF gt_out.

DATA: BEGIN OF gt_det OCCURS 0,
        posid        LIKE  impr-posid,
        aufnr        LIKE  aufk-aufnr,
        prnam        LIKE  ztfi_imfm-prnam,
        item(30),
        reason      LIKE ztfi_reason-reson,
        amt(15) TYPE p DECIMALS 2, "         LIKE  ztfi_imfm-tot,
        co_amt(15) TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        pt_amt(15) TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        ps_amt(15) TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        or_amt(15) TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        su_amt(15) TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        re_amt(15) TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        difa_amt(15) TYPE p DECIMALS 2, "     LIKE  ztfi_imfm-tot,
        difb_amt(15) TYPE p DECIMALS 2, "     LIKE  ztfi_imfm-tot,
      END OF gt_det.
*---reason code
DATA: BEGIN OF gt_reason OCCURS 0,
        objnr        LIKE  impr-posid,
        reason       LIKE ztfi_reason-reson,
        amt(15)      TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        difa_amt(15) TYPE p DECIMALS 2, "     LIKE  ztfi_imfm-tot,
        difb_amt(15) TYPE p DECIMALS 2, "     LIKE  ztfi_imfm-tot,
      END OF gt_reason.

*--summary by activity
DATA : BEGIN OF it_sum OCCURS 0,
        posid       LIKE  impr-posid,
        prnam       LIKE  ztfi_imfm-prnam,
        gubun       LIKE  ztfi_imfm-gubun,
        tot         LIKE  ztfi_imfm-tot,
       END OF it_sum.

DATA: BEGIN OF it_bpeg OCCURS 0,
        objnr    LIKE bpeg-objnr,     " PI, IO, FundCenter
        vorga    LIKE bpeg-vorga,
        belnr    LIKE bpeg-belnr,
        wtges    LIKE bpeg-wtges,      "overall amount
        posit    LIKE bpeg-posit,      "commitment
        lednr    LIKE bpeg-lednr,
        twaer    LIKE bpeg-twaer,
        versn    LIKE bpeg-versn,
        wrttp    LIKE bpeg-wrttp,      "overall amount
        trgkz    LIKE bpeg-trgkz,
        geber    LIKE bpeg-geber,      "fund
        sgtext   LIKE bpbk-sgtext,
        bldat    LIKE bpbk-bldat,      "Document Date
        cpudt    LIKE bpbk-cpudt,      "System Date
        usnam    LIKE bpbk-usnam,      "user
        fipos    LIKE fmfpo-fipos,     "commitment
      END OF it_bpeg.
DATA: BEGIN OF it_bpja OCCURS 0,

         objnr LIKE bpja-objnr,
         wtjhr LIKE bpja-wtjhr,
      END OF it_bpja.
DATA : BEGIN OF it_down OCCURS 0,
        objnr        LIKE  impr-posid,
        reason       LIKE ztfi_reason-reson,
        amt(15)      TYPE p DECIMALS 2, "       LIKE  ztfi_imfm-tot,
        difa_amt(15) TYPE p DECIMALS 2, "     LIKE  ztfi_imfm-tot,
        difb_amt(15) TYPE p DECIMALS 2, "     LIKE  ztfi_imfm-tot,
       END OF it_down.
DATA: BEGIN OF it_head OCCURS 0,
         text(16),
      END OF it_head.

*----for combox
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.
*---WORK AREA
DATA : wa_t_cnt TYPE i,
       wa_prnam   LIKE  ztfi_imfm-prnam,
       wa_org_amt      LIKE  ztfi_imfm-tot,
       wa_sup_amt      LIKE  ztfi_imfm-tot,
       wa_ret_amt      LIKE  ztfi_imfm-tot,
       wa_cur_amt      LIKE  ztfi_imfm-tot,
       wa_sum_amt      LIKE  ztfi_imfm-tot,
       wa_co_amt     LIKE  ztfi_imfm-tot,
       wa_difa_amt     LIKE  ztfi_imfm-tot,
       wa_difb_amt     LIKE  ztfi_imfm-tot.

RANGES : r_objnr FOR  aufk-objnr,
         r_posnr FOR  imzo-posnr.
RANGES : r_vorga FOR bpbk-vorga,
         r_wrttp FOR bpeg-wrttp.
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE c020.
**--------------------------------------
SELECT-OPTIONS: s_prnam FOR impr-prnam MEMORY ID imt OBLIGATORY.
*---pi
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_pi.
PARAMETERS : r_1  RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 32.
SELECT-OPTIONS:
  s_posid   FOR   impr-posid.
SELECTION-SCREEN END OF LINE.

*=====================================*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_or.
PARAMETERS : r_2  RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 32.
SELECT-OPTIONS:
  s_aufnr  FOR   aufk-aufnr.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE c010.
PARAMETERS:  p_ayear LIKE impr-gjahr   MEMORY ID gjr OBLIGATORY
                                      DEFAULT sy-datum+0(4).

SELECT-OPTIONS:  s_gjahr   FOR   impr-gjahr,
                 s_cpudt   FOR   bpbk-cpudt.

PARAMETERS: p_auth(1) TYPE c DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK s1.

*--------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE c030.
SELECT-OPTIONS:
*  s_posid   FOR   impr-posid,
  s_kostl   FOR   impr-kostl.
SELECTION-SCREEN END OF BLOCK s3.
*---------------------------------------*
PARAMETERS : p_file LIKE rlgrap-filename DEFAULT
   'c:\temp\IM anaysis.xls'.
*------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_ACT'
            values = it_val.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------
*
INITIALIZATION.
  c_or = 'Order'.
  c_pi = 'Position ID'.
*  CONCATENATE sy-datum+0(6) '01' INTO s_cpudt-low.
*  s_cpudt-high = sy-datum.
*  APPEND s_cpudt.
*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
START-OF-SELECTION.
* ==> 1. select data from db
  PERFORM select_data.
  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No found data '.
    EXIT.
  ENDIF.
************************************************************************
** TOP-OF-PAGE
************************************************************************
TOP-OF-PAGE.
  PERFORM top_of_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.
*
END-OF-SELECTION.
  PERFORM write_data.
*****************************
  SET PF-STATUS 'PF1000'.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'ALLS'.
      LOOP AT gt_out.
        gt_out-sign = '5'.
        MODIFY gt_out.
      ENDLOOP.
    WHEN 'ALLD'.
      LOOP AT gt_out.
        gt_out-sign = '4'.
        MODIFY gt_out.
      ENDLOOP.
    WHEN 'DOWN'.
      CALL FUNCTION 'DOWNLOAD'
           EXPORTING
                filename   = p_file
                filetype   = 'DAT'
           TABLES
                data_tab   = gt_reason
                fieldnames = it_head.

  ENDCASE.
  PERFORM write_data.
***************************************************
AT LINE-SELECTION.
**************************************************
  DATA : wa_field(20),
         wa_value(20),
         wa_posid       LIKE imak-posid,
         wa_item(20), " LIKE imak-posid,
         wa_aufnr       LIKE aufk-aufnr,
         wa_prnam1 LIKE impr-prnam,
         wa_line TYPE i,
         wa_head_line TYPE i,
         wa_objnr LIKE impr-objnr.

  GET CURSOR FIELD  wa_field VALUE wa_value.
  GET CURSOR LINE wa_line.
  wa_line = wa_line - 3.
  wa_item = sy-lisel+4(12).
  wa_aufnr = sy-lisel+4(12).
  wa_posid = sy-lisel+4(12).
*-----jhs

  IF wa_field  =  'GT_OUT-POSID'.
    wa_posid = sy-lisel+4(12).
  ELSEIF wa_field  =  'GT_OUT-AUFNR'.
    wa_aufnr = sy-lisel+4(12).
  ELSEIF wa_field  =  'GT_REASON-REASON'.
    wa_objnr = sy-lisel+58(15).
  ELSEIF wa_field  = 'SYM_PLUS_FOLDER'.
  ELSEIF wa_field  = 'SYM_MINUS_FOLDER'.
  ELSE.
    EXIT.
  ENDIF.
*====================================*
  IF wa_field = 'GT_OUT-POSID'.
    SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
    SET PARAMETER ID 'IMP' FIELD wa_posid.
    SET PARAMETER ID 'GJR' FIELD p_ayear.
    CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.

  ELSEIF wa_field = 'GT_OUT-AUFNR'.
    SET PARAMETER ID 'ANR' FIELD wa_aufnr.
    CALL TRANSACTION 'KO23' AND SKIP FIRST SCREEN.

  ELSEIF wa_field = 'GT_REASON-REASON'.

    READ TABLE it_bpeg WITH KEY objnr = wa_objnr.
    SUBMIT bpep0001 AND RETURN
           WITH posit    =  it_bpeg-posit
           WITH lednr    =  it_bpeg-lednr
           WITH twaer    =  it_bpeg-twaer
           WITH versn    =  it_bpeg-versn
           WITH wrttp    =  it_bpeg-wrttp
           WITH objnr    =  it_bpeg-objnr
           WITH trgkz    =  it_bpeg-trgkz
           WITH total    = 'X'.
  ENDIF.
  IF r_1 = 'X'.
    PERFORM modify_symbol USING wa_posid.
  ELSE.
    PERFORM modify_symbol USING wa_aufnr.
  ENDIF.
*  PERFORM write_data_again using wa_posid.
  PERFORM write_data.

*******************************************************************



*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM select_data.
  REFRESH : it_imfm, it_sum.
  CLEAR   : it_imfm, it_sum.
  IF r_1 = 'X'.
    PERFORM pi_process.
*-----order
  ELSE.
    PERFORM order_process.
  ENDIF.

  CLEAR wa_t_cnt.
  CLEAR : wa_org_amt, wa_sup_amt, wa_ret_amt.
  CLEAR : wa_prnam.
*-----Doc.history by eason code
  CLEAR : wa_t_cnt.
  DESCRIBE TABLE r_objnr LINES wa_t_cnt.
  PERFORM set_value_type.
  PERFORM get_doc_data.
  PERFORM make_doc_data.
ENDFORM.                    " select_data

*&      Form  get_pi_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_POSID  text
*      -->P_P_AYEAR  text
*----------------------------------------------------------------------*
FORM get_pi_plan USING    u_posid
                          u_ayear
                          u_prnam.
  REFRESH : it_budget.
  CLEAR   : it_budget.

  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = u_ayear
       TABLES
            out   = it_budget.
  LOOP AT it_budget WHERE posid = u_posid
                    AND   gjahr IN s_gjahr
                    AND   gjahr <> '1111'.
    gt_out-plan    = gt_out-plan + it_budget-plan.
    gt_out-cur_amt = gt_out-cur_amt + it_budget-wtjhr.
    gt_out-org_amt = gt_out-org_amt + it_budget-org.
    gt_out-sup_amt = gt_out-sup_amt + it_budget-supp.
    gt_out-ret_amt = gt_out-ret_amt + it_budget-ret.
  ENDLOOP.
ENDFORM.                    " get_pi_plan
*&---------------------------------------------------------------------*
*&      Form  get_pi_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_POSID  text
*      -->P_P_AYEAR  text
*----------------------------------------------------------------------*
FORM get_pi_actual USING    u_posid
                            u_ayear
                            u_prnam.
  REFRESH : it_actual.
  CLEAR   : it_actual.

  CALL FUNCTION 'Z_FFI_GET_PI_ACTUAL_ACT'
    EXPORTING
      posid         = u_posid
      gjahr         = u_ayear
      prnam         = u_prnam
* IMPORTING
*   AMT           =
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
*READ TABLE IT_ACTUAL WITH KEY POSID = U_POSID
*                              GJAHR = P_AYEAR.
*IF SY-SUBRC = 0.
*   MOVE IT_ACTUAL-TOT    TO  GT_OUT-ACT_AMT.
*ENDIF.
*---2004/03/23
  LOOP AT it_actual WHERE gjahr IN s_gjahr
*Issue Number : FI-20041118-006, Requested by YCYOON
*Changed on 2004/12/10, by WSKIM
*---Start
*                     AND  IPPOS = ' '.
                      AND  ippos NE space.
*---End
    CASE it_actual-wrttp.
      WHEN '21'. "PR Commitment
        ADD it_actual-tot TO gt_out-pr_amt.
        ADD it_actual-tot TO wa_co_amt.
      WHEN '22'. "PO Commitment
        ADD it_actual-tot TO gt_out-po_amt.
        ADD it_actual-tot TO wa_co_amt.
*Issue Number : FI-20041118-006, Requested by YCYOON
*Changed on 2004/12/10, by WSKIM
*---Start
*     WHEN '04' .
      WHEN '04' OR '11'.
*---End
        ADD it_actual-tot TO gt_out-act_amt.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " get_pi_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  get_io_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_POSID  text
*      -->P_P_AYEAR  text
*      -->P_WA_PRNAM  text
*----------------------------------------------------------------------*
FORM get_io_plan USING    u_posid
                          u_ayear
                          u_prnam.

  REFRESH : it_budget.
  CLEAR   : it_budget.

  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET_IO'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = u_ayear
       TABLES
            out   = it_budget.

  READ TABLE it_budget WITH KEY posid = u_posid
                                gjahr = '1111'.
  IF sy-subrc = 0.
    MOVE it_budget-wtjhr TO gt_out-io_amt.
  ENDIF.
ENDFORM.                    " get_io_plan
*&---------------------------------------------------------------------*
*&      Form  GET_IO_BUDGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_AUFK_AUFNR  text
*----------------------------------------------------------------------*
FORM get_io_budget USING    u_aufnr.
  REFRESH : it_io_budget.
  CLEAR   : it_io_budget.
  CALL FUNCTION 'Z_FFI_GET_IO_BUDGET'
       EXPORTING
            aufnr = u_aufnr
       TABLES
            out   = it_io_budget.
  READ TABLE it_io_budget WITH KEY aufnr = u_aufnr
                                   gjahr = '1111'.
  IF sy-subrc = 0.
    MOVE it_io_budget-plan TO gt_out-plan.
    MOVE it_io_budget-org  TO gt_out-org_amt.
    MOVE it_io_budget-supp TO gt_out-sup_amt.
  ENDIF.

ENDFORM.                    " GET_IO_BUDGET
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_AUFK_AUFNR  text
*----------------------------------------------------------------------*
FORM get_io_actual USING    u_aufnr.

  REFRESH : it_io_actual.
  CLEAR   : it_io_actual.

  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
    EXPORTING
      aufnr         = u_aufnr
* IMPORTING
*   AMT           =
    TABLES
      out           = it_io_actual
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT it_io_actual WHERE gjahr IN s_gjahr.
    CASE it_io_actual-wrttp.
      WHEN '21'.
        ADD it_io_actual-tot TO gt_out-pr_amt.
      WHEN '22'.
        ADD it_io_actual-tot TO gt_out-po_amt.
      WHEN '04' OR '11'.
        ADD it_io_actual-tot TO gt_out-act_amt.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  pi_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pi_process.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_impr
  FROM impr AS a INNER JOIN impu AS b
  ON a~posnr = b~posnr
  WHERE a~posid IN s_posid
  AND   a~gjahr EQ p_ayear
  AND   a~prnam IN s_prnam
  AND   a~kostl IN s_kostl
  AND   a~pprio <> ' '.

***********************************************
  CLEAR :  wa_difa_amt , wa_difb_amt, wa_co_amt.
  LOOP AT it_impr.
*-----Doc.history by eason code
    r_objnr-sign = 'I'.
    r_objnr-option = 'EQ'.
    r_objnr-low = it_impr-objnr.
    APPEND r_objnr.

    MOVE it_impr-posid TO gt_out-posid.
    MOVE it_impr-prnam TO gt_out-prnam.
    MOVE it_impr-post1 TO gt_out-txt50.
    MOVE it_impr-objnr TO gt_out-objnr.
    wa_prnam = it_impr-prnam.

    AT END OF posid.
      PERFORM get_pi_plan USING   gt_out-posid p_ayear wa_prnam.
      PERFORM get_io_plan USING   gt_out-posid p_ayear wa_prnam.
      PERFORM get_pi_actual USING gt_out-posid p_ayear wa_prnam.

      MOVE '4' TO gt_out-sign.
*Issue Number : FI-20041118-006, Requested by YCYOON
*Changed on 2004/12/10, by WSKIM
*---Start
      gt_out-difa_amt = gt_out-act_amt - gt_out-org_amt.
      gt_out-difb_amt = gt_out-act_amt - gt_out-cur_amt.
      wa_difa_amt = gt_out-difa_amt.
      wa_difb_amt = gt_out-difb_amt.
*---End
      APPEND  gt_out.
*      CLEAR  : gt_out.
      CLEAR  wa_prnam.
      PERFORM make_detail_data.
      CLEAR  : gt_out.

    ENDAT.
    CLEAR :  wa_difa_amt , wa_difb_amt, wa_co_amt.
  ENDLOOP.

ENDFORM.                    " pi_process
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  ULINE AT : (184).
  FORMAT COLOR COL_HEADING.
  WRITE :/ sy-vline NO-GAP, (15) h_item,
           sy-vline NO-GAP, (20) h_txt50,
           sy-vline NO-GAP, (16) h_plan  CENTERED,
           sy-vline NO-GAP, (16) h_org   CENTERED,
           sy-vline NO-GAP, (16) h_sup   CENTERED,
           sy-vline NO-GAP, (16) h_ret   CENTERED,
           sy-vline NO-GAP, (16) h_cur   CENTERED,
           sy-vline NO-GAP, (16) h_io    CENTERED,
           sy-vline NO-GAP, (16) h_act   CENTERED,
           sy-vline NO-GAP, (16) h_diffa CENTERED,
           sy-vline NO-GAP.
  FORMAT COLOR OFF.
  ULINE AT : (184).

ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  modify_symbol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_symbol USING u_item.
  IF r_1 = 'X'.
    READ TABLE gt_out WITH KEY posid = u_item.
  ELSE.
    READ TABLE gt_out WITH KEY aufnr = u_item.
  ENDIF.
*  INDEX wa_line.
  IF sy-subrc = 0.
    CASE wa_value.
      WHEN '4'.
        gt_out-sign = '5'.
      WHEN '5'.
        gt_out-sign = '4'.
    ENDCASE.
    IF r_1 = 'X'.
      MODIFY gt_out TRANSPORTING sign WHERE posid = u_item.
    ELSE.
      MODIFY gt_out TRANSPORTING sign WHERE aufnr = u_item.
    ENDIF.
  ENDIF.

ENDFORM.                    " modify_symbol
*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.
  sy-lsind = sy-lsind - 1.
*  PERFORM top_of_page.
  LOOP AT gt_out.
    WRITE : / sy-vline NO-GAP.
    IF r_1  = 'X'.
      WRITE : 5(12) gt_out-posid.
    ELSE.
      WRITE : 5(12) gt_out-aufnr.
    ENDIF.
    WRITE :  sy-vline NO-GAP, (20) gt_out-txt50.
    SET LEFT SCROLL-BOUNDARY.
    WRITE :  sy-vline NO-GAP, (16) gt_out-plan RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-org_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-sup_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-ret_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-cur_amt RIGHT-JUSTIFIED ,
             sy-vline NO-GAP, (16) gt_out-io_amt  RIGHT-JUSTIFIED ,
             sy-vline NO-GAP, (16) gt_out-act_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-difa_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP.
    CASE gt_out-sign.
      WHEN '4'.
        WRITE: sy-vline NO-GAP,
               2(2) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'.
        WRITE: 2(2) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
        SORT gt_det  ASCENDING BY posid.
        FORMAT INTENSIFIED OFF.
        LOOP AT gt_det WHERE  posid = gt_out-posid.
          WRITE : / sy-vline,
                   (14) ' ',
                    sy-vline,
                   (19) gt_det-item   COLOR 2, "COL_HEADING,
                   sy-vline NO-GAP,
                   (16) gt_det-amt COLOR 3, "COL_TOTAL NO-ZERO,
                   sy-vline NO-GAP,
                   (16) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline.
        ENDLOOP.
        LOOP AT gt_reason WHERE  objnr = gt_out-objnr.
          WRITE : / sy-vline,
                   (14) ' ',
                    sy-vline,
                   (19) gt_reason-reason   COLOR 7, "COL_HEADING,
                   sy-vline NO-GAP,
                   (16) gt_reason-amt COLOR 7, "COL_TOTAL NO-ZERO,
                   sy-vline NO-GAP,
                   (16) gt_reason-objnr,  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline.
        ENDLOOP.
    ENDCASE.
    AT LAST.
      ULINE AT : /(184).
    ENDAT.
  ENDLOOP.

ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  append_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_detail.
  MOVE it_impr-posid TO gt_det-posid.
  MOVE it_impr-prnam TO gt_det-prnam.

  COLLECT : gt_det.
  CLEAR  : gt_det.
ENDFORM.                    " append_detail
*&---------------------------------------------------------------------*
*&      Form  order_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM order_process.

  SELECT * INTO TABLE it_aufk FROM aufk
    WHERE aufnr IN s_aufnr
    AND   kostl IN s_kostl.
*--
  CLEAR wa_t_cnt.
  DESCRIBE TABLE it_aufk LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    LOOP AT it_aufk.
      MOVE 'I' TO r_objnr-sign.
      MOVE 'EQ' TO r_objnr-option.
      MOVE it_aufk-objnr TO r_objnr-low.
      APPEND r_objnr.
    ENDLOOP.
  ENDIF.
*====*
  CLEAR gt_out.
  LOOP AT it_aufk.
    MOVE-CORRESPONDING it_aufk TO gt_out.
    PERFORM convert_aufnr CHANGING it_aufk-aufnr.
    MOVE it_aufk-aufnr TO gt_out-aufnr.
    PERFORM get_io_budget USING it_aufk-aufnr.
*----2004/04/07
    PERFORM get_io_actual USING it_aufk-aufnr.
    MOVE it_aufk-ktext  TO gt_out-txt50.
    MOVE '4' TO gt_out-sign.
    APPEND gt_out.
    CLEAR  gt_out.
    PERFORM make_detail_data.
  ENDLOOP.
  SORT gt_out BY aufnr ASCENDING.
ENDFORM.                    " order_process
*&---------------------------------------------------------------------*
*&      Form  set_value_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_value_type.
  REFRESH : r_vorga,r_wrttp. CLEAR   : r_vorga,r_wrttp.
  r_vorga-sign   = 'I'.  r_vorga-option = 'EQ'.
*--Original
  r_vorga-low = 'KBUD'. APPEND r_vorga.
*---supplement
  r_vorga-low = 'KBN0'. APPEND r_vorga.
*---Return
  r_vorga-low = 'KBR0'. APPEND r_vorga.
*--transfer
  r_vorga-low = 'KBUS'. APPEND r_vorga.
  r_vorga-low = 'KBUE'. APPEND r_vorga.
*--transfer
  r_vorga-low = 'KBFR'. APPEND r_vorga.
*---start wskim : add
  r_vorga-low = 'KSTP'. APPEND r_vorga.
*---end

* Amount Type
  r_wrttp-sign   = 'I'.
** FM org/sup/ret
*  r_wrttp-low = '43'.  APPEND r_wrttp.
** FM release
*  r_wrttp-low = '46'.  APPEND r_wrttp.
* PI budget
  IF r_1 = 'X'.
*-----Start wskim
    r_wrttp-option = 'BT'.
    r_wrttp-low = '47'.
    r_wrttp-high = '48'.
*-----End wskim
    APPEND r_wrttp.
  ELSE.
* ORDER budget
    r_wrttp-option = 'EQ'.
    r_wrttp-low = '41'.  APPEND r_wrttp.
  ENDIF.

ENDFORM.                    " set_value_type
*&---------------------------------------------------------------------*
*&      Form  GET_DOC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_doc_data.
* select overall amount
  SELECT l~objnr l~vorga l~belnr
         l~wtges l~posit l~lednr
         l~twaer l~versn l~wrttp
         l~trgkz
         l~geber h~sgtext h~bldat h~cpudt h~usnam
    INTO TABLE it_bpeg
       FROM bpeg AS l INNER JOIN bpbk AS h
              ON l~belnr  = h~belnr
              WHERE lednr = '0001'
                AND l~objnr IN r_objnr
                AND l~wrttp IN r_wrttp
                AND l~vorga IN r_vorga
                AND h~cpudt IN s_cpudt.
*        AND h~belnr IN s_belnr
*        AND h~usnam IN s_usnam.

ENDFORM.                    " GET_DOC_DATA
*&---------------------------------------------------------------------*
*&      Form  make_doc_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_doc_data.
  LOOP AT it_bpeg.
    gt_reason-objnr = it_bpeg-objnr.
    gt_reason-reason = it_bpeg-sgtext+1(2).
    gt_reason-amt = it_bpeg-wtges.
    COLLECT gt_reason.
    CLEAR   gt_reason.
  ENDLOOP.

ENDFORM.                    " make_doc_data
*&---------------------------------------------------------------------*
*&      Form  write_data_again
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_POSID  text
*----------------------------------------------------------------------*
FORM write_data_again USING    u_posid.
  sy-lsind = sy-lsind - 1.
  PERFORM top_of_page.
  LOOP AT gt_out WHERE posid = u_posid.
    WRITE : / sy-vline NO-GAP,
             5(12) gt_out-posid,
             sy-vline NO-GAP, (20) gt_out-txt50.
    SET LEFT SCROLL-BOUNDARY.
    WRITE :  sy-vline NO-GAP, (16) gt_out-plan RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-org_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-sup_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-ret_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-cur_amt RIGHT-JUSTIFIED ,
             sy-vline NO-GAP, (16) gt_out-io_amt  RIGHT-JUSTIFIED ,
             sy-vline NO-GAP, (16) gt_out-act_amt RIGHT-JUSTIFIED,
             sy-vline NO-GAP, (16) gt_out-difa_amt RIGHT-JUSTIFIED.
*           sy-vline NO-GAP.
    CASE gt_out-sign.
      WHEN '4'.
        WRITE: sy-vline NO-GAP,
               2(2) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'.
        WRITE: 2(2) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
        SORT gt_det  ASCENDING BY posid.
        LOOP AT gt_det WHERE  posid = gt_out-posid.
          WRITE : / sy-vline,
                   (14) ' ',
                    sy-vline,
                   (19) gt_det-item   COLOR 2, "COL_HEADING,
                   sy-vline NO-GAP,
                   (16) gt_det-amt COLOR 3, "COL_TOTAL NO-ZERO,
                   sy-vline NO-GAP,
                   (16) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline.
        ENDLOOP.
        LOOP AT gt_reason WHERE  objnr = gt_out-objnr.
          WRITE : / sy-vline,
                   (14) ' ',
                    sy-vline,
                   (19) gt_reason-reason   COLOR 2, "COL_HEADING,
                   sy-vline NO-GAP,
                   (16) gt_reason-amt COLOR 3, "COL_TOTAL NO-ZERO,
                   sy-vline NO-GAP,
                   (16) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline,
                   (15) ' ',  sy-vline.
        ENDLOOP.
    ENDCASE.
    AT LAST.
      ULINE AT : (184).
    ENDAT.
  ENDLOOP.

ENDFORM.                    " write_data_again
*&---------------------------------------------------------------------*
*&      Form  make_detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_detail_data.
*Issue Number : FI-20041118-006, Requested by YCYOON
*Changed on 2004/12/10, by WSKIM
*---Start
*  gt_out-difa_amt = gt_out-act_amt - gt_out-org_amt.
*  gt_out-difb_amt = gt_out-act_amt - gt_out-cur_amt.
*  wa_difa_amt = gt_out-difa_amt.
*  wa_difb_amt = gt_out-difb_amt.
*---End

*----commitment
  MOVE h_commit TO gt_det-item.
  gt_det-co_amt = wa_co_amt.
  gt_det-amt = wa_co_amt.
  PERFORM append_detail.
*----postpone
  MOVE h_post   TO gt_det-item.
  gt_det-amt = gt_out-cur_amt - gt_out-io_amt.
  PERFORM append_detail.
*-------pur.saving
  MOVE h_pur   TO gt_det-item.
*-----Start wskim
* gt_det-amt = gt_out-act_amt - gt_out-pr_amt .
  gt_det-amt = gt_out-act_amt - gt_out-po_amt .
*-----End
  PERFORM append_detail.
*----difa.(act - o.bud)
  MOVE h_difa  TO gt_det-item.
  gt_det-amt = gt_out-act_amt - gt_out-org_amt.
  PERFORM append_detail.
*-------pro.saving
  MOVE h_pro   TO gt_det-item.
  gt_det-amt = ( gt_out-act_amt - gt_out-org_amt ) +
              wa_co_amt + ( gt_out-cur_amt - gt_out-io_amt )
              + ( gt_out-act_amt - gt_out-pr_amt ) .
  PERFORM append_detail.

ENDFORM.                    " make_detail_data
*&---------------------------------------------------------------------*
*&      Form  CONVERT_AUFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_AUFNR  text
*----------------------------------------------------------------------*
FORM convert_aufnr CHANGING u_aufnr.
  CALL FUNCTION 'CONVERSION_EXIT_AUFNR_OUTPUT'
       EXPORTING
            input  = u_aufnr
       IMPORTING
            output = u_aufnr.
  .

ENDFORM.                    " CONVERT_AUFNR
