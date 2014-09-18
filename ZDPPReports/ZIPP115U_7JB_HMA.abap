************************************************************************
* Program Name      : ZIPP115U_7JB_HMA
* Author            : Bongsoo, Kim
* Creation Date     : 2004-03-19
* Specifications By : Bongsoo, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K908420
* Addl Documentation:
* Description       : ZTPP_PMT07JB_A TO HMA SD MODULE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.03.18  ZDPP         UD1K908440   ZTPP_PMT07JB_A TO HMA SD MODULE
*02/17/2005  CHRIS        UD1K914442   SEND DATA IF ANY TABLE HAS DATA
************************************************************************
REPORT zipp115u_7jb_hma NO STANDARD PAGE HEADING
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_pmt07jb_a,  "SUMMARIZED PMT07JB
        ztpp_pmt07jb_b,  "PMT07JB FOR THE SEQUENCE FIXED DATA
        ztpp_pmt07jb.    "Sequencing Result (D~D+3)

*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF it_7jb_a OCCURS 0 ,
        sqdt TYPE ztpp_pmt07jb_a-sqdt, "SEQ date
        plnt TYPE ztpp_pmt07jb_a-plnt, "Plant
        line TYPE ztpp_pmt07jb_a-line, "Line
        modl TYPE ztpp_pmt07jb_a-modl, "Model Code
        mtgu TYPE ztpp_pmt07jb_a-mtgu, "Re-SEQ
        ssr1 TYPE ztpp_pmt07jb_a-ssr1, "SEQ Serial 1
        ssr2 TYPE ztpp_pmt07jb_a-ssr2, "SEQ Serial 2
        sqcd TYPE ztpp_pmt07jb_a-sqcd, "SEQ.Code
        vhno TYPE ztpp_pmt07jb_a-vhno, "Serial No of VIN
        ordr TYPE ztpp_pmt07jb_a-ordr, "Work Order No
        dist TYPE ztpp_pmt07jb_a-dist, "DISTRIBUTOR
        extc TYPE ztpp_pmt07jb_a-extc, "Exterior Color
        intc TYPE ztpp_pmt07jb_a-intc, "Interior Color
        bmdl TYPE ztpp_pmt07jb_a-bmdl, "Model Index
        ocnn TYPE ztpp_pmt07jb_a-ocnn, "OCN
        vers TYPE ztpp_pmt07jb_a-vers, "Version
        evl1 TYPE ztpp_pmt07jb_a-evl1, "Application Value #
        evl2 TYPE ztpp_pmt07jb_a-evl2, "Application Value #
        evl3 TYPE ztpp_pmt07jb_a-evl3, "Application Value #
        evl4 TYPE ztpp_pmt07jb_a-evl4, "Application Value #
        evl5 TYPE ztpp_pmt07jb_a-evl5, "Application Value #
        pqty TYPE ztpp_pmt07jb_a-pqty, "Quantity
        gubb TYPE ztpp_pmt07jb_a-gubb, "Plan Section
        gub1 TYPE ztpp_pmt07jb_a-gub1,
*                   "Indicator of Confirmed('1') or Only Planned Order
        cdat TYPE ztpp_pmt07jb_a-cdat, "Working Date for the Sequence
        ctim TYPE ztpp_pmt07jb_a-ctim, "Working Time for the Sequence
        moye TYPE ztpp_pmt07jb_a-moye, "Model Year
        pver TYPE ztpp_pmt07jb_a-pver, "Production version
        plnum TYPE ztpp_pmt07jb_a-plnum, "Planned order number
        vinn TYPE ztpp_pmt07jb_a-vinn, "VIN ID
      END OF it_7jb_a.
DATA: BEGIN OF it_7jb_b OCCURS 0 ,
        sqdt TYPE ztpp_pmt07jb_b-sqdt, "SEQ date
        plnt TYPE ztpp_pmt07jb_b-plnt, "Plant
        line TYPE ztpp_pmt07jb_b-line, "Line
        modl TYPE ztpp_pmt07jb_b-modl, "Model Code
        mtgu TYPE ztpp_pmt07jb_b-mtgu, "Re-SEQ
        ssr1 TYPE ztpp_pmt07jb_b-ssr1, "SEQ Serial 1
        ssr2 TYPE ztpp_pmt07jb_b-ssr2, "SEQ Serial 2
        sqcd TYPE ztpp_pmt07jb_b-sqcd, "SEQ.Code
        vhno TYPE ztpp_pmt07jb_b-vhno, "Serial No of VIN
        ordr TYPE ztpp_pmt07jb_b-ordr, "Work Order No
        dist TYPE ztpp_pmt07jb_b-dist, "DISTRIBUTOR
        extc TYPE ztpp_pmt07jb_b-extc, "Exterior Color
        intc TYPE ztpp_pmt07jb_b-intc, "Interior Color
        bmdl TYPE ztpp_pmt07jb_b-bmdl, "Model Index
        ocnn TYPE ztpp_pmt07jb_b-ocnn, "OCN
        vers TYPE ztpp_pmt07jb_b-vers, "Version
        evl1 TYPE ztpp_pmt07jb_b-evl1, "Application Value #
        evl2 TYPE ztpp_pmt07jb_b-evl2, "Application Value #
        evl3 TYPE ztpp_pmt07jb_b-evl3, "Application Value #
        evl4 TYPE ztpp_pmt07jb_b-evl4, "Application Value #
        evl5 TYPE ztpp_pmt07jb_b-evl5, "Application Value #
        pqty TYPE ztpp_pmt07jb_b-pqty, "Quantity
        gubb TYPE ztpp_pmt07jb_b-gubb, "Plan Section
        gub1 TYPE ztpp_pmt07jb_b-gub1,
*                   "Indicator of Confirmed('1') or Only Planned Order
        cdat TYPE ztpp_pmt07jb_b-cdat, "Working Date for the Sequence
        ctim TYPE ztpp_pmt07jb_b-ctim, "Working Time for the Sequence
        moye TYPE ztpp_pmt07jb_b-moye, "Model Year
        pver TYPE ztpp_pmt07jb_b-pver, "Production version
        plnum TYPE ztpp_pmt07jb_b-plnum, "Planned order number
        vinn TYPE ztpp_pmt07jb_b-vinn, "VIN ID
      END OF it_7jb_b.
DATA: BEGIN OF it_7jb OCCURS 0 ,
        sqdt TYPE ztpp_pmt07jb_a-sqdt, "SEQ date
        plnt TYPE ztpp_pmt07jb_a-plnt, "Plant
        line TYPE ztpp_pmt07jb_a-line, "Line
        modl TYPE ztpp_pmt07jb_a-modl, "Model Code
        mtgu TYPE ztpp_pmt07jb_a-mtgu, "Re-SEQ
        ssr1 TYPE ztpp_pmt07jb_a-ssr1, "SEQ Serial 1
        ssr2 TYPE ztpp_pmt07jb_a-ssr2, "SEQ Serial 2
        sqcd TYPE ztpp_pmt07jb_a-sqcd, "SEQ.Code
        vhno TYPE ztpp_pmt07jb_a-vhno, "Serial No of VIN
        ordr TYPE ztpp_pmt07jb_a-ordr, "Work Order No
        dist TYPE ztpp_pmt07jb_a-dist, "DISTRIBUTOR
        extc TYPE ztpp_pmt07jb_a-extc, "Exterior Color
        intc TYPE ztpp_pmt07jb_a-intc, "Interior Color
        bmdl TYPE ztpp_pmt07jb_a-bmdl, "Model Index
        ocnn TYPE ztpp_pmt07jb_a-ocnn, "OCN
        vers TYPE ztpp_pmt07jb_a-vers, "Version
        evl1 TYPE ztpp_pmt07jb_a-evl1, "Application Value #
        evl2 TYPE ztpp_pmt07jb_a-evl2, "Application Value #
        evl3 TYPE ztpp_pmt07jb_a-evl3, "Application Value #
        evl4 TYPE ztpp_pmt07jb_a-evl4, "Application Value #
        evl5 TYPE ztpp_pmt07jb_a-evl5, "Application Value #
        pqty(04),  " TYPE ZTPP_PMT07JB_A-PQTY, "Quantity
        gubb TYPE ztpp_pmt07jb_a-gubb, "Plan Section
        gub1 TYPE ztpp_pmt07jb_a-gub1,
*                   "Indicator of Confirmed('1') or Only Planned Order
        cdat TYPE ztpp_pmt07jb_a-cdat, "Working Date for the Sequence
        ctim TYPE ztpp_pmt07jb_a-ctim, "Working Time for the Sequence
        moye TYPE ztpp_pmt07jb_a-moye, "Model Year
        pver TYPE ztpp_pmt07jb_a-pver, "Production version
        plnum TYPE ztpp_pmt07jb_a-plnum, "Planned order number
        vinn TYPE ztpp_pmt07jb_a-vinn, "VIN ID
      END OF it_7jb.
*----------------------------------------------------------------------*
*  DATA DECLARATION
*----------------------------------------------------------------------*
DATA: w_sapmnt(10) VALUE '/usr/sap',
*     W_SYSID  LIKE SY-SYSID ,
      w_edi(10) VALUE '/EDI_SAP/',
      w_hma_7jb(10) VALUE 'HMA_7JB_' ,
      w_file(90).
DATA: wa_flg .

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: p_chek AS CHECKBOX DEFAULT 'X' .
SELECTION-SCREEN: SKIP.
SELECT-OPTIONS: p_dealer FOR it_7jb_a-dist OBLIGATORY.
*PARAMETERS: P_SQDT LIKE  ZTPP_PMT07JB_A-SQDT.
*SELECT-OPTIONS: S_SSR1 FOR ZTPP_PMT07JB_A-SSR1.
SELECTION-SCREEN END   OF BLOCK b2.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_screen.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  MESSAGE i000 WITH 'DO NOT USE THIS PROGRAM'.
  LEAVE PROGRAM.

  PERFORM read_process.
  IF wa_flg IS INITIAL.
    PERFORM data_process.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
*  S_SSR1-LOW    = '0001'.
*  S_SSR1-HIGH   = '0005'.
*  S_SSR1-SIGN   = 'I'.
*  S_SSR1-OPTION = 'BT'.
*  APPEND S_SSR1.
*  P_SQDT = '20040109'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.

  PERFORM read_ztpp_pmt07jb_a.
  PERFORM read_ztpp_pmt07jb_b.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTPP_PMT07JB_A
*&---------------------------------------------------------------------*
FORM read_ztpp_pmt07jb_a.
  DATA l_tabix LIKE sy-tabix.
  SELECT sqdt plnt line modl mtgu ssr1 ssr2
         sqcd vhno ordr dist extc intc bmdl
         ocnn vers evl1 evl2 evl3 evl4 evl5
         pqty gubb gub1 cdat ctim
         moye
         pver
         plnum
         vinn
       FROM ztpp_pmt07jb_a
       INTO TABLE it_7jb_a
**       WHERE SQDT EQ P_SQDT
**       AND   SSR1 IN S_SSR1
**       AND   GUBB NE '*'.
*       WHERE SSR1 IN S_SSR1
*       AND   GUBB NE '*'.
       WHERE gubb NE '*' AND
         dist IN p_dealer.
  IF sy-subrc EQ 0.
    CLEAR wa_flg .
    LOOP AT it_7jb_a.
      l_tabix = sy-tabix.
      IF it_7jb_a-ordr(1) EQ 'F'.
        DELETE it_7jb_a INDEX l_tabix.
      ENDIF.
      CLEAR it_7jb_a.
    ENDLOOP.
  ELSE.
    FORMAT RESET.
    wa_flg = 'X'.
    WRITE: / text-001.
  ENDIF.

ENDFORM.                    " READ_ZTPP_PMT07JB_A
*&---------------------------------------------------------------------*
*&      Form  READ_ZTPP_PMT07JB_B
*&---------------------------------------------------------------------*
FORM read_ztpp_pmt07jb_b.
  SELECT sqdt plnt line modl mtgu ssr1 ssr2
         sqcd vhno ordr dist extc intc bmdl
         ocnn vers evl1 evl2 evl3 evl4 evl5
         pqty gubb gub1 cdat ctim
         moye
         pver
         plnum
         vinn
       FROM ztpp_pmt07jb_b
       INTO TABLE it_7jb_b
       WHERE dist IN p_dealer.
  IF sy-subrc NE 0.
*    CHANGEED BY CHRIS REQUESTED BY CATHERINE
*    WA_FLG = 'X' .                                     "UD1K914442
*    END OF CHANGE ON 02/17/2005
    FORMAT RESET.
    WRITE: / text-002.
  ELSE.
    CLEAR wa_flg.
  ENDIF.

ENDFORM.                    " READ_ZTPP_PMT07JB_B
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  DATA: l_file  LIKE w_file  ,
        l_flag  TYPE c       .
*  DESCRIBE TABLE IT_7JB LINES W_LINE_COUNT.
* W_SYSID = SY-SYSID.
  CONCATENATE  w_hma_7jb  sy-datum  '.txt'  INTO l_file.
  CONCATENATE  w_sapmnt
*              W_SYSID
               w_edi
               w_hma_7jb
               sy-datum
               '.txt'
               INTO w_file.
* DATA IT_7JB_A & IT_7JB_B TO IT_7JB SUMMED
  PERFORM data_summed.

  LOOP AT it_7jb.
    " Skip the CANADA Order...
    IF it_7jb-dist(3) = 'B06'.
      CONTINUE.
    ENDIF.
*   L_TABIX = SY-TABIX.
    IF l_flag = space.
      OPEN DATASET w_file IN TEXT MODE FOR OUTPUT. "CHECK DUPL.
      IF sy-subrc NE 0.
        MESSAGE w002 WITH text-100 l_file .
        LEAVE PROGRAM .
      ENDIF.
      l_flag = 'X' .
    ELSE.
      OPEN DATASET w_file IN TEXT MODE FOR APPENDING.
    ENDIF.
    TRANSFER it_7jb TO w_file.
    CLEAR it_7jb.
  ENDLOOP.
  CLOSE DATASET w_file.

  MESSAGE s001 WITH text-301 .
ENDFORM.                    " DATA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DATA_SUMMED
*&---------------------------------------------------------------------*
FORM data_summed.

  LOOP AT it_7jb_b.
    MOVE-CORRESPONDING it_7jb_b TO it_7jb.
    WRITE: it_7jb_b-pqty TO it_7jb-pqty USING EDIT MASK 'RR____'.
    APPEND it_7jb.
    CLEAR: it_7jb, it_7jb_b.
  ENDLOOP.

  LOOP AT it_7jb_a.
    MOVE-CORRESPONDING it_7jb_a TO it_7jb.
    WRITE: it_7jb_a-pqty TO it_7jb-pqty USING EDIT MASK 'RR____'.
    APPEND it_7jb.
    CLEAR: it_7jb, it_7jb_a.
  ENDLOOP.

ENDFORM.                    " DATA_SUMMED
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM selection_screen.
  LOOP AT SCREEN .
    IF screen-name  EQ 'P_CHEK' .
      screen-input = 0 .
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SELECTION_SCREEN
