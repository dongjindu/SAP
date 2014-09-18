*&---------------------------------------------------------------------*
*& INCLUDE ZRIM01I01                                                   *
*&---------------------------------------------------------------------*
*&  Program Name  :  B/L PAI MODULE Include                            *
*&  Created By    : INFOLINK Ltd.                                      *
*&  Created On    : 2000.02.12                                         *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF SY-DYNNR EQ '0060'.
    ANTWORT = 'C'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF SY-DYNNR EQ '0070' OR SY-DYNNR EQ '0071'.
    IF NOT G_CUSTOM_CONTAINER IS INITIAL.
      " destroy tree container (detroys contained tree control, too)
      CALL METHOD G_CUSTOM_CONTAINER->FREE
        EXCEPTIONS
          CNTL_SYSTEM_ERROR = 1
          CNTL_ERROR        = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR G_CUSTOM_CONTAINER.
      CLEAR G_TREE.
      CLEAR G_APPLICATION.
    ENDIF.
    ANTWORT = 'C'.
    CLEAR : G_NODE_KEY, G_NODE_KEY_OLD, G_EVENT, G_NODE_KEY.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  IF W_STATUS EQ 'D'.
    IF SY-DYNNR = '9912'.
      PERFORM P1000_READ_MSHD.
      PERFORM P1000_SORT_MSHD.
      SET SCREEN '9911'. LEAVE TO SCREEN '9911'.
    ELSE.
      SET SCREEN 0.   LEAVE SCREEN.
    ENDIF.
  ELSE.
*-----------------------------------------------------------------------
* 각 초기화면 Number 추?
*-----------------------------------------------------------------------
    IF SY-DYNNR EQ '0100' OR SY-DYNNR EQ '0200' OR SY-DYNNR EQ '0300'
    OR SY-DYNNR EQ '0810' OR SY-DYNNR EQ '0820'
    OR SY-DYNNR EQ '1100' OR SY-DYNNR EQ '1200' OR SY-DYNNR EQ '1300'
    OR SY-DYNNR EQ '2600' OR SY-DYNNR EQ '2700' OR SY-DYNNR EQ '2800'
    OR SY-DYNNR EQ '2900'
    OR SY-DYNNR EQ '3100' OR SY-DYNNR EQ '3200' OR SY-DYNNR EQ '3300'
    OR SY-DYNNR EQ '3400'
    OR SY-DYNNR EQ '3101' OR SY-DYNNR EQ '3201' OR SY-DYNNR EQ '3301'
    OR SY-DYNNR EQ '3401'
    OR SY-DYNNR EQ '3500' OR SY-DYNNR EQ '3600' OR SY-DYNNR EQ '3700'
    OR SY-DYNNR EQ '3800'
    OR SY-DYNNR EQ '3501' OR SY-DYNNR EQ '3601' OR SY-DYNNR EQ '3701'
    OR SY-DYNNR EQ '3801'
    OR SY-DYNNR EQ '3512'
    OR SY-DYNNR EQ '5700' OR SY-DYNNR EQ '5800'
    OR SY-DYNNR EQ '6200' OR SY-DYNNR EQ '6300'
    OR SY-DYNNR EQ '6400' OR SY-DYNNR EQ '6500' OR SY-DYNNR EQ '6450'
    OR SY-DYNNR EQ '6600'
    OR SY-DYNNR EQ '6700' OR SY-DYNNR EQ '6800' OR SY-DYNNR EQ '6900'
    OR SY-DYNNR EQ '7100' OR SY-DYNNR EQ '7200' OR SY-DYNNR EQ '7300'
    OR SY-DYNNR EQ '7400' OR SY-DYNNR EQ '7500' OR SY-DYNNR EQ '7600'
    OR SY-DYNNR EQ '8100' OR SY-DYNNR EQ '8110' OR SY-DYNNR EQ '8120'
    OR SY-DYNNR EQ '8300' OR SY-DYNNR EQ '8400'
    OR SY-DYNNR EQ '8500' OR SY-DYNNR EQ '8600'
    OR SY-DYNNR EQ '8700' OR SY-DYNNR EQ '8800'
    OR SY-DYNNR EQ '9200' OR SY-DYNNR EQ '9202'
    OR SY-DYNNR EQ '9203'
    OR SY-DYNNR EQ '9211' OR SY-DYNNR EQ '9212' OR SY-DYNNR EQ '9213'
    OR SY-DYNNR EQ '9221' OR SY-DYNNR EQ '9222' OR SY-DYNNR EQ '9223'
    OR SY-DYNNR EQ '9224'
    OR SY-DYNNR EQ '9226' OR SY-DYNNR EQ '9228'
    OR SY-DYNNR EQ '9910' OR SY-DYNNR EQ '9214'
    OR SY-DYNNR EQ '4100' OR SY-DYNNR EQ '4200'
    OR SY-DYNNR EQ '4300' OR SY-DYNNR EQ '4400'.

      SET SCREEN 0.   LEAVE SCREEN.
    ELSE.
      PERFORM P2000_SET_MESSAGE USING  SY-UCOMM.
      IF SY-DYNNR EQ '9912'.
        PERFORM P2000_SET_LOCK_ZTMSHD USING 'U'.
        SET SCREEN '9911'. LEAVE TO SCREEN '9911'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0100 INPUT.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
  IF ( W_OK_CODE EQ 'COPY' AND ANTWORT EQ 'Y' ) OR
     ( W_OK_CODE EQ 'ENTR' AND ANTWORT EQ 'Y' ).
* CONFIGURATION CHECK.
    SELECT SINGLE * FROM ZTIMIMG00.
*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
    PERFORM   P2000_MOVE_BL_DATA.
* TOTAL CHG.
    PERFORM   P2000_TOTAL_CHG_CALC.

    REFRESH : IT_ZSBLIT_ORG, IT_ZSBLCST_ORG, IT_ZSBLCST1_ORG.
    CLEAR : *ZTBL.

* MKIM 2001.05.14
    SET PARAMETER ID 'ZPAPRTC' FIELD ZTBL-ZFAPPC.
    SET PARAMETER ID 'ZPCARC'  FIELD ZTBL-ZFCARC.

* CALL SCREEN
    W_STATUS = C_REQ_C.
    SET SCREEN 0101.  LEAVE TO SCREEN 0101.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'N'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  IF SY-DYNNR EQ '3515' AND  ANTWORT  EQ  'Y'.
    ZTCIVHD-ZFEXRT = ZSCIVHD-ZFEXRT.
  ENDIF.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0101 INPUT.

  CASE OK-CODE.
    WHEN 'ZIMG'.                    " Import IMG
      CALL TRANSACTION 'ZIMGM'.
    WHEN 'DDLC'.           " Double click
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'COST'.
      IF SY-TCODE(4) NE 'ZIMB'.
        PERFORM  P2000_CALL_COST_SCREEN.
      ELSE.
        PERFORM  P4000_DISPLAY_DOCUMENT.
      ENDIF.
    WHEN 'POST'.     ">B/L Cost Posting
      PERFORM  P3000_BLCOST_POST.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE' OR 'SVCO' OR 'IMPREQ'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP' OR 'ADDC' OR 'OPDC' OR
         'DCSD' OR 'DCRP' OR 'BLCT'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE' OR 'DELR' OR 'REVK' OR 'OPCL' OR 'EDIS' OR
         'SDSD' OR 'REAL'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'REOG'.
      PERFORM  P2000_REOG_DISPLAY.
    WHEN 'ZRIM18'.
      PERFORM  P2000_CON_DISPLAY  USING ZTBL-ZFHBLNO
                                        ZTBL-ZFBLNO.
    WHEN 'DSBL'.
      IF SY-TCODE = 'ZIM62'  OR  SY-TCODE  =  'ZIM63'.
        PERFORM  P2000_BL_DOC_DISPLAY   USING  ZTIDR-ZFBLNO.
      ELSEIF SY-TCODE = 'ZIM64'  OR  SY-TCODE  =  'ZIM65' OR
             SY-TCODE = 'ZIM66'  OR  SY-TCODE  =  'ZIM76' OR
             SY-TCODE = 'ZIM74'  OR  SY-TCODE  =  'ZIM75'.
        PERFORM  P2000_BL_DOC_DISPLAY   USING  ZTIDS-ZFBLNO.
      ELSEIF SY-TCODE = 'ZIM31'  OR  SY-TCODE  =  'ZIM32' OR
             SY-TCODE = 'ZIM33'.
        PERFORM  P2000_BL_DOC_DISPLAY   USING  ZTIV-ZFBLNO.
      ELSEIF SY-TCODE = 'ZIMB1'  OR  SY-TCODE EQ 'ZIMB2' OR
             SY-TCODE = 'ZIMB3'  OR  SY-TCODE EQ 'ZIM84'.
        PERFORM  P2000_BL_DOC_DISPLAY   USING  ZTINSB-ZFBLNO.
      ENDIF.
    WHEN 'GRPT'.                    " G/R
      PERFORM  P2000_EXEC_GOODS_RECEIPT.
    WHEN 'GRRV'.                    " G/R Cancel
      PERFORM  P2000_EXEC_GR_CANCLE.
    WHEN 'MIRO' OR 'MIR1'.          " Miro
      PERFORM  P2000_EXEC_MAT_INVOICE_VERIFY.
    WHEN 'MIR2'.                    " I/V CANCEL
      PERFORM  P2000_EXEC_INV_DOC_CALCEL.
    WHEN 'TRTR'.                   " TR Reflection
      PERFORM  P2000_EXEC_TR_DOCUMENT.
    WHEN 'CHIN'.
      PERFORM  P2000_EXEC_INT_DOCUMENT.
    WHEN 'CKEK'.                   " Check
      PERFORM  P2000_EDI_DOC_CHECK.
    WHEN 'IVCR'.                  " INVOICE CREATE
      PERFORM  P3000_INVOICE_CREATE.
    WHEN 'HIST'.                 " HEADER CHANGE DOCUMENT
      PERFORM  P2000_HEADER_CHANGE_DOC.
    WHEN 'HIIT'.                " ITEM CHANGE DOCUMENT
      PERFORM  P2000_ITEM_CHANGE_DOC.
    WHEN 'HICT'.               " CHANGE DOCUMENT
      PERFORM  P2000_COST_CHANGE_DOC.
    WHEN 'HICT1'.             " B/L Others Cost.
      PERFORM  P2000_COST_CHANGE_DOC_1.
    WHEN 'DSDR'.              " Import License
      PERFORM  P2000_ZTIDR_CALL USING  ZTIDS-ZFBLNO
                                       ZTIDS-ZFCLSEQ.
    WHEN 'ME23'.              " P/O
      PERFORM  P2000_PO_DOC_DISPLAY     USING  ZTBL-ZFREBELN ''.
    WHEN 'ZIM03'.             " L/C
      PERFORM  P2000_LC_DOC_DISPLAY     USING  ''
                                               ZTBL-ZFOPNNO.
    WHEN 'ZIM23'.             " B/L
      PERFORM   P2000_BL_DOC_DISPLAY     USING ZTBL-ZFBLNO.
    WHEN 'BLSD'.              " Send Shipping Document
      PERFORM   P2000_BL_SD_PRINT         USING ZTBL-ZFBLNO.
    WHEN 'ZIM73'.            " B/L
      PERFORM   P2000_ZTBLUG_DOC_DISPLAY     USING ZTBL-ZFBLNO.
    WHEN 'ZIMI3'.            " Carry-in Information
      CASE SY-TCODE.
        WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.
          PERFORM   P2000_BLINOU_DOC_DISPLAY USING  ZTBLINR-ZFBLNO
                                                    ZTBLINR-ZFBTSEQ.
        WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4' OR 'ZIMO6'.
          PERFORM   P2000_BLINOU_DOC_DISPLAY USING  ZTBLOUR-ZFBLNO
                                                    ZTBLOUR-ZFBTSEQ.
      ENDCASE.
    WHEN 'ZIMI8'.
      CASE SY-TCODE.
        WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.
          PERFORM   P2000_BLINR_DOC_DISPLAY USING  ZTBLINOU-ZFBLNO
                                                   ZTBLINOU-ZFBTSEQ.
        WHEN 'ZIMO1' OR 'ZIMO2' OR 'ZIMO3' OR 'ZIMO4'
                     OR 'ZIMO6'.
          PERFORM   P2000_BLINR_DOC_DISPLAY USING  ZTBLOUR-ZFBLNO
                                                   ZTBLOUR-ZFBTSEQ.
      ENDCASE.
    WHEN 'ZIMO3'.
      CASE SY-TCODE.
        WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.
          PERFORM   P2000_BLOUR_DOC_DISPLAY USING  ZTBLINOU-ZFBLNO
                                                   ZTBLINOU-ZFBTSEQ.
        WHEN 'ZIMI7' OR 'ZIMI8' OR 'ZIMI9'.
          PERFORM   P2000_BLOUR_DOC_DISPLAY USING  ZTBLINR-ZFBLNO
                                                   ZTBLINR-ZFBTSEQ.
        WHEN 'ZIMO6'.
          PERFORM   P2000_BLOUR_DOC_DISPLAY USING  ZTBLOUR-ZFBLNO
                                                   ZTBLOUR-ZFBTSEQ.
      ENDCASE.
    WHEN 'ZIM28'.
      PERFORM   P2000_LG_DOC_DISPLAY    USING  ZTBL-ZFBLNO.
    WHEN 'ZIM93'.
      CASE SY-TCODE.
        WHEN 'ZIMI1' OR 'ZIMI2' OR 'ZIMI3'.
          IF ZTBL-ZFREBELN IS INITIAL.
            MESSAGE E167 WITH 'Representative P/O No'.
          ELSE.
            PERFORM  P2000_ZIM93_DISPLAY     USING  ''
                                                    ZTBL-ZFREBELN.
          ENDIF.
        WHEN OTHERS.
          IF ZTBL-ZFREBELN IS INITIAL.
            MESSAGE E167 WITH 'Representative P/O No'.
          ELSE.
            PERFORM  P2000_ZIM93_DISPLAY     USING  ''
                                                    ZTBL-ZFREBELN.
          ENDIF.
      ENDCASE.
    WHEN 'MK03'.           " Vendor
      CASE SY-DYNNR.
        WHEN '0101'.
          PERFORM   P2000_VENDOR_DISPLAY    USING  ZTBL-LIFNR.
        WHEN '3510'.
          PERFORM   P2000_VENDOR_DISPLAY    USING  ZTCIVHD-ZFMAVN.
      ENDCASE.
    WHEN 'BENI'.           " BENIFIARY
      CASE SY-DYNNR.
        WHEN '0101'.
          PERFORM   P2000_VENDOR_DISPLAY    USING  ZTBL-ZFBENI.
      ENDCASE.
    WHEN 'BANK'.           " Open Bank
      CASE SY-DYNNR.
        WHEN '3510'.
          PERFORM   P2000_VENDOR_DISPLAY    USING  ZTCIVHD-ZFOPBN.
      ENDCASE.
    WHEN 'FWDR'.           " Forwarder
      PERFORM   P2000_VENDOR_DISPLAY    USING  ZTBL-ZFFORD.
    WHEN 'TRUC'.           " Trucker
      PERFORM   P2000_VENDOR_DISPLAY    USING  ZTBL-ZFTRCK.
    WHEN 'MM03'.           " Material
      PERFORM P2000_SELECT_MATERIAL.
      PERFORM P2000_MATERIAL_DISPLAY.
    WHEN 'MD04'.           " MRP List
      PERFORM P2000_SELECT_MATERIAL.
      PERFORM P2000_MATERIAL_MD04.
    WHEN 'MMBE'.           " Inventory Information
      PERFORM P2000_SELECT_MATERIAL.
      PERFORM P2000_MATERIAL_MMBE.
    WHEN 'MB51'.           " Material Document
      PERFORM P2000_SELECT_MATERIAL.
      PERFORM P2000_MATERIAL_MB51.
    WHEN 'ME2M'.           " Open P/O
      PERFORM P2000_SELECT_MATERIAL.
      PERFORM P2000_MATERIAL_ME2M.
    WHEN 'ME03'.           " Source List
      PERFORM P2000_SELECT_MATERIAL.
      PERFORM P2000_MATERIAL_ME03.
    WHEN 'LGPT'.           " Letter of Guarantee
      PERFORM P2000_LG_PRINT.
    WHEN 'PRES'.           " Import License Print Out
      PERFORM P2000_IMPRES_PRINT.
    WHEN 'FLAT'.           " FLAT DATA
      PERFORM   P2000_SHOW_FLAT.
    WHEN 'STAT'.
      PERFORM   P2000_STATUS_DISPLAY.
*> 2002.08.27. INSERTED BY SEUNGYEON.
    WHEN 'PAYORD'.
      IF W_STATUS = 'C' AND ZTCIVHD-ZFCIVRN EQ SPACE.
        MESSAGE E977 WITH 'Save invoice Doc first.'.
      ELSE.
        PERFORM   P2000_PAYORD_DISPLAY.
      ENDIF.
    WHEN 'TTREQ'.
      SUBMIT ZRIMTTREQ  WITH P_CIVRN EQ ZTTTHD-ZFCIVRN
                        AND RETURN.
    WHEN 'DTRS'.
      IF ZTBL-ZFVIA = 'AIR'.
        SUBMIT ZRIMBLAIRCST WITH P_ZFBLNO EQ ZTBL-ZFBLNO
                           AND RETURN.
      ELSEIF ZTBL-ZFVIA = 'VSL'.
        SUBMIT ZRIMBLVSLCST WITH P_ZFBLNO EQ ZTBL-ZFBLNO
                           AND RETURN.
      ENDIF.
    WHEN 'DHYK'.
      SUBMIT ZRIMBLDOMCST WITH P_ZFBLNO EQ ZTBL-ZFBLNO
                         AND RETURN.
*>> 2004.05.23 Nashinho Inserted..
    WHEN 'CIPT'.
      SUBMIT ZRIMCIVPRT WITH P_CIVRN EQ ZTCIVHD-ZFCIVRN AND RETURN.
    WHEN 'CIST'.
      SUBMIT ZRIMIVLST  WITH P_CIVRN EQ ZTCIVHD-ZFCIVRN AND RETURN.
    WHEN 'PKST'.
      SUBMIT ZRIMPKLST  WITH P_CIVRN EQ ZTCIVHD-ZFCIVRN AND RETURN.
    WHEN 'CPK'.
      SUBMIT ZRIMCPKLST WITH P_CIVRN EQ ZTCIVHD-ZFCIVRN AND RETURN.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  IV_IT_GET_LINE_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE IV_IT_GET_LINE_SCR0102 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0102-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IV_IT_GET_LINE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0104 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0104-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  HOUSE_BL_DUP_CHK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE HOUSE_BL_DUP_CHK_SCRCOM INPUT.
  OK-CODE = W_OK_CODE.
*-----------------------------------------------------------------------
* OK-CODE Process
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

*-----------------------------------------------------------------------
* Formal House B/L No Dup. error Checking
*-----------------------------------------------------------------------
  IF ZSREQHD-ZFHBLNO IS INITIAL.
    MESSAGE W167 WITH 'House B/L Number'.
  ELSE.
    W_COUNT = 0.
    SELECT ZFHBLNO INTO W_HBLNO FROM ZTBL
                   WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      W_COUNT = W_COUNT + 1.
    ENDSELECT.

    IF W_COUNT NE 0.
      MESSAGE W303 WITH W_COUNT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " HOUSE_BL_DUP_CHK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0105  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0105 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0105-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0105  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0106  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0106 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0106-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCRCOM INPUT.
*-----------------------------------------------------------------------
* journalizing by OK-CODE.
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* Input Value check..
  IF ZSREQHD-ZFHBLNO IS INITIAL AND
     ZSREQHD-ZFBLNO IS INITIAL  AND
     ZSREQHD-EBELN IS INITIAL.
    MESSAGE E304.
  ENDIF.

  IF ZSREQHD-ZFBLNO IS INITIAL.
    IF NOT ZSREQHD-ZFHBLNO  IS INITIAL.    " Display with House B/L.
* B/L No. Count..
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.

    ELSEIF NOT ZSREQHD-EBELN IS INITIAL.   " Display with P/O No..
* B/L No. Count..
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTBL
                        WHERE ZFREBELN EQ ZSREQHD-EBELN.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E140(ZIM1) WITH ZSREQHD-EBELN.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFREBELN EQ ZSREQHD-EBELN.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT3.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.

    ENDIF.
  ENDIF.
* B/L READ PERFORM ?
  ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
  PERFORM   P1000_READ_BL_DOC.

* 반입예정정보.
  PERFORM   P1000_READ_BLINOU_DOC.

*>> B/L DATA MOVE.
  IF SY-TCODE EQ 'ZIM81'.
*---------------------------- 200.04.04 KSB ----------------------------
*>> 양도 B/L일 경우....(하역데이타 막음)
    IF ZTBL-ZFRENT EQ 'X'.
      MESSAGE E602 WITH ZTBL-ZFBLNO.
    ENDIF.
*>> BULK B/L일 경우....(하역데이타 막음)
    IF ZTBL-ZFSHTY NE 'B'.
      MESSAGE E603 WITH ZTBL-ZFBLNO.
    ENDIF.
*>> B/L 납품 완료지시자.
    IF ZTBL-ZFELIKZ EQ 'X'.
      MESSAGE E404 WITH ZTBL-ZFBLNO.
    ENDIF.
*---------------------------- 200.04.04 KSB ----------------------------

    W_COUNT = 0.
    SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO
           FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
           ON     H~ZFCGNO   EQ   I~ZFCGNO
           WHERE  H~ZFCGPT   EQ   ZSCGHD-ZFCGPT
           AND    I~ZFBLNO   EQ   ZTBL-ZFBLNO
           GROUP BY H~ZFCGNO.
      ADD 1 TO W_COUNT.
    ENDSELECT.
    IF W_COUNT GT 0.
      MESSAGE   E389 WITH ZTBL-ZFBLNO ZSCGHD-ZFCGPT.
    ENDIF.

    PERFORM   P2000_CARGO_DATA_MOVE.
    W_COUNT = 1.
  ELSEIF SY-TCODE EQ 'ZIM82' OR SY-TCODE EQ 'ZIM83'.

    W_COUNT = 0.
    SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO
           FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
           ON     H~ZFCGNO   EQ   I~ZFCGNO
           WHERE  H~ZFCGPT   EQ   ZSCGHD-ZFCGPT
           AND    I~ZFBLNO   EQ   ZTBL-ZFBLNO
           GROUP BY H~ZFCGNO.
      ADD 1 TO W_COUNT.
    ENDSELECT.

    CASE W_COUNT.
      WHEN 0.
        MESSAGE E392 WITH ZTBL-ZFBLNO ZSCGHD-ZFCGPT.
      WHEN 1.
        SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO UP TO 1 ROWS
               FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
               ON     H~ZFCGNO   EQ   I~ZFCGNO
               WHERE  H~ZFCGPT   EQ   ZSCGHD-ZFCGPT
               AND    I~ZFBLNO   EQ   ZTBL-ZFBLNO.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_CARGO_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.

    PERFORM   P1000_READ_CARGO_WORK_DOC.
  ENDIF.

* LOCK OBJECT
  IF NOT ( W_STATUS EQ C_REQ_D OR W_STATUS EQ C_ADD_D OR
     W_STATUS EQ C_OPEN_D ).
    IF SY-TCODE EQ 'ZIM22'  OR SY-TCODE EQ 'ZIM26'  OR
       SY-TCODE EQ 'ZIM221' OR SY-TCODE EQ 'ZIM222' OR
       SY-TCODE EQ 'ZIM223' OR
       SY-TCODE EQ 'ZIM81'  OR SY-TCODE EQ 'ZIM82'  OR
       SY-TCODE EQ 'ZIMB1'  OR SY-TCODE EQ 'ZIMB2'  OR
       SY-TCODE EQ 'ZIMB4'.
      PERFORM P2000_SET_BL_REQDOC_LOCK USING    'L'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " READ_BL_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR9200  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR9200 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF NOT ZTBLINOU-ZFBLNO IS INITIAL.
    ZTBL-ZFBLNO = ZTBLINOU-ZFBLNO.
* B/L 문서 조?
    PERFORM   P1000_READ_BL_DOC.
*    PERFORM   P1000_READ_INPUT_INFO.
  ENDIF.

ENDMODULE.                 " READ_BL_DOC_SCR9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR9202  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR9202 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.         " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ELSE.
* B/L READ PERFORM ?
      ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
      PERFORM   P1000_READ_BL_DOC.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
* B/L READ PERFORM ?
    ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
    PERFORM   P1000_READ_BL_DOC.
  ENDIF.
*-----------------------------------------------------------------------
* 반입예정정보 조?
*-----------------------------------------------------------------------
  IF ZSREQHD-ZFBTSEQ IS INITIAL.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBLINOU
                      WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
    CASE W_COUNT.
      WHEN 0.   MESSAGE E181 WITH  ZTBL-ZFBLNO.
      WHEN 1.
        SELECT ZFBTSEQ INTO ZSREQHD-ZFBTSEQ
                       FROM ZTBLINOU UP TO 1 ROWS
                       WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
        ENDSELECT.
      WHEN OTHERS.
        REFRESH IT_ZSBLINOU.
* Table Multi-Select
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLINOU
                 FROM   ZTBLINOU
                 WHERE  ZFBLNO EQ ZTBL-ZFBLNO
                 ORDER  BY ZFBTSEQ.

        DESCRIBE TABLE IT_ZSBLINOU LINES TFILL.
        IF TFILL = 0.
          MESSAGE E406.
        ENDIF.

*         PERFORM   P2000_GET_POSITION.
        W_STATUS_CHK = 'C'.
        INCLUDE = 'BLINOUCG'.                 "
        CALL SCREEN 0014 STARTING AT  07 3
                         ENDING   AT  87 15.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.

    ENDCASE.
  ENDIF.

  SELECT SINGLE * FROM ZTBLINOU  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                 AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E044 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
  ENDIF.
* 후속 작?
  IF W_STATUS NE C_REQ_D.
    IF NOT ZTBLINOU-ZFBINYN IS INITIAL OR
       NOT ZTBLINOU-ZFBOUYN IS INITIAL.
      MESSAGE E187 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
    ENDIF.
  ENDIF.

* 발송?
  PERFORM   P1000_GET_BONDED_NAME1   USING      ZTBLINOU-ZFDBNARC
                                     CHANGING   W_DEL_NM.
* 도착?
  PERFORM   P1000_GET_BONDED_NAME1   USING      ZTBLINOU-ZFABNARC
                                     CHANGING   W_ARR_NM.
* BENIFICIARY
  PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                              CHANGING   W_LFA1-NAME1.
  MOVE : W_LFA1-NAME2     TO     W_LFA1-NAME1.

  IF W_STATUS NE C_REQ_D.
    PERFORM P2000_SET_BLINOU_LOCK USING    'L'.
  ENDIF.

  SELECT SINGLE * FROM ZTBLOUR   WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                 AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.

ENDMODULE.                 " READ_BL_DOC_SCR9202  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9203  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9203 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 9201.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR9203  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR9211  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR9211 INPUT.

*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.
  IF SY-TCODE = 'ZIMI7' OR SY-TCODE = 'ZIMI8'.
*>> 반입예정정보 미사용시 엘지버전.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF ZTIMIMG00-ZFINOU EQ SPACE.
      PERFORM  P2000_NOTUSE_IMG00_ZFINOU.
      EXIT.
    ENDIF.
  ENDIF.
  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.         " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ELSE.
* B/L READ PERFORM ?
      ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
      PERFORM   P1000_READ_BL_DOC.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
* B/L READ PERFORM ?
    ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
    PERFORM   P1000_READ_BL_DOC.
  ENDIF.
*-----------------------------------------------------------------------
* 반입정보 조?
*-----------------------------------------------------------------------
  IF ZSREQHD-ZFBTSEQ IS INITIAL.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBLINR
                      WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
    CASE W_COUNT.
      WHEN 0.   MESSAGE E181 WITH  ZTBL-ZFBLNO.
      WHEN 1.
        SELECT ZFBTSEQ INTO ZSREQHD-ZFBTSEQ
                       FROM ZTBLINR  UP TO 1 ROWS
                       WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
        ENDSELECT.
      WHEN OTHERS.
        REFRESH IT_ZTBLINR.
* Table Multi-Select
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLINR
                 FROM   ZTBLINR
                 WHERE  ZFBLNO EQ ZTBL-ZFBLNO
                 ORDER  BY ZFBTSEQ.

        DESCRIBE TABLE IT_ZTBLINR  LINES TFILL.
        IF TFILL = 0.
          MESSAGE E406.
        ENDIF.

*         PERFORM   P2000_GET_POSITION.
        W_STATUS_CHK = 'C'.
        INCLUDE = 'ZTBLINRD'.                 " 반입 정?
        CALL SCREEN 0014 STARTING AT  13 3
                         ENDING   AT  75 15.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.

    ENDCASE.
  ENDIF.

  SELECT SINGLE * FROM ZTBLINR   WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                 AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E045 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
  ELSE.
     *ZTBLINR  =  ZTBLINR.
  ENDIF.
*-----------------------------------------------------------------------
* 업무별로 문서 상태검증함.
*-----------------------------------------------------------------------
  CASE W_STATUS.
    WHEN C_REQ_U.        " 변?
      PERFORM P2000_INR_CHANGE_CHECK.
    WHEN C_REQ_D.        " 조회....
      PERFORM P2000_INR_DISPLAY_CHECK.
    WHEN C_OPEN_C.       " OPEN
      PERFORM P2000_INR_OPEN_CHECK.
    WHEN OTHERS.
  ENDCASE.
* 반입예정정?
  SELECT SINGLE * FROM ZTBLINOU  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                 AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
* 도착?
  PERFORM   P1000_GET_BONDED_NAME1   USING      ZTBLINR-ZFBNARCD
                                     CHANGING   W_ARR_NM.
* BENIFICIARY
  PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                              CHANGING   W_LFA1-NAME1.
  MOVE : W_LFA1-NAME2     TO     W_LFA1-NAME1.

  IF W_STATUS NE C_REQ_D.
    PERFORM    P2000_SET_ZTBLINR_LOCK_MODE USING 'L'.
  ENDIF.

ENDMODULE.                 " READ_BL_DOC_SCR9211  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9211  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9211 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_U TO W_STATUS.

  SET SCREEN 9210.  LEAVE SCREEN.


ENDMODULE.                 " USER_COMMAND_SCR9211  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9212  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9212 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 9210.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR9212  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9213  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9213 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_OPEN_U TO W_STATUS.

  SET SCREEN 9210.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR9213  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3100 INPUT.

*>> Import IMG Get
  SELECT SINGLE * FROM ZTIMIMG00.

  MOVE : SY-MANDT      TO      ZTIV-MANDT,
         SY-UNAME      TO      ZTIV-ERNAM,
         SY-DATUM      TO      ZTIV-CDAT,
         SY-UNAME      TO      ZTIV-UNAM,
         SY-DATUM      TO      ZTIV-UDAT,
         ZSIV-ZFYSDST  TO      ZTIV-ZFYSDST,
         ZSIV-ZFCLCD   TO      ZTIV-ZFCLCD,
         ZSIV-ZFLGRST  TO      ZTIV-ZFLGRST,
         'N'           TO      ZTIV-ZFPONMA,
         ZSIV-ZFTRIPLE TO      ZTIV-ZFTRIPLE,
         'N'           TO      ZTIV-ZFGRST.

*>> Customs Clearance Status
  IF ZSIV-ZFCLCD EQ 'X'.
    MOVE 'N'          TO      ZTIV-ZFCUST.
  ELSE.
    MOVE '1'          TO      ZTIV-ZFCUST.
  ENDIF.
*>> Cost Distribution Status
  IF ZSIV-ZFCSTYN IS INITIAL.
    MOVE 'X'          TO      ZTIV-ZFCDST.
  ELSE.
    MOVE 'N'          TO      ZTIV-ZFCDST.
  ENDIF.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    MOVE  'X'         TO      ZTIV-ZFCDST.
  ENDIF.
*>> Import Cost I/V Status
  MOVE ZTIV-ZFCDST     TO      ZTIV-ZFCIVST.

*>> Local Purchase admission Process
  IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU'.
    MOVE : 'N'          TO      ZTIV-ZFCUST,
           'N'          TO      ZTIV-ZFGRST,
           'X'          TO      ZTIV-ZFCLCD,
           'Y'          TO      ZTIV-ZFPOYN,
           SPACE        TO      ZTIV-ZFYSDST.
  ENDIF.

*> G/R Status
  IF ZSIV-ZFGRYN EQ 'X'.
    ZTIV-ZFGRST = 'X'.
  ENDIF.

*> Sample => No G/R
  IF ZTBL-ZFPOYN EQ 'N'.
    IF ZTBL-ZFPOTY EQ 'S'.
      ZTIV-ZFGRST = 'N'.
    ENDIF.
  ENDIF.

*>> Vendor
  IF NOT ZTIV-LIFNR IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTIV-LIFNR
                                           CHANGING  W_LIFNR_NM.
  ELSE.
    CLEAR : W_LIFNR_NM.
  ENDIF.

*>> Phantom Vendor
  IF NOT ZTIV-ZFPHVN IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTIV-ZFPHVN
                                           CHANGING  W_ZFOPBN_NM.
  ELSE.
    CLEAR : W_ZFOPBN_NM.
  ENDIF.

  CLEAR : T001.
  IF NOT ZTIV-BUKRS IS INITIAL.
    SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTIV-BUKRS.
  ENDIF.

  CLEAR : ZSIMIMG08.
  IF NOT ZTIV-ZFPONC IS INITIAL.
    SELECT SINGLE ZFCDNM INTO ZSIMIMG08-ZFCDNM
                    FROM ZTIMIMG08
                    WHERE ZFCDTY   EQ   '001'
                    AND   ZFCD     EQ   ZTIV-ZFPONC.
  ENDIF.

*>> COMPANY DESCRIPTION SELECT.
  CLEAR : T001, ZTMSHD.
  IF NOT ZTIV-BUKRS IS INITIAL.
    SELECT SINGLE * FROM T001
                    WHERE BUKRS EQ ZTIV-BUKRS.
  ENDIF.
  IF NOT ZTBL-ZFMSNO IS INITIAL.
    SELECT SINGLE * FROM ZTMSHD
                    WHERE ZFMSNO  EQ  ZTBL-ZFMSNO.
  ENDIF.
  CLEAR : *ZTIV.
  REFRESH : IT_ZSIVIT_ORG.

  MOVE C_REQ_C TO W_STATUS.
  SET SCREEN 3110.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR3100  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR2600  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR2600 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF NOT ZTBLINOU-ZFBLNO IS INITIAL.
    ZTBL-ZFBLNO = ZTBLINOU-ZFBLNO.
* B/L 문서 조?
    PERFORM   P1000_READ_BL_DOC.
* L/G 문서 존재여부 검?
    PERFORM   P1000_READ_LG_DOC.
  ENDIF.

ENDMODULE.                 " READ_BL_DOC_SCR2600  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2610  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2610 INPUT.

  CASE OK-CODE.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'STAT'.
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR2610  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2600  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2600 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_C TO W_STATUS.
  CLEAR : ZTLG.

  SET SCREEN 2601.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR2600  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR2604  INPUT
*&---------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR2604 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_2604-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR2604  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR9221  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR9221 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSREQHD-ZFBLNO IS INITIAL AND ZSREQHD-ZFHBLNO IS INITIAL.
    MESSAGE E043.
  ENDIF.
  IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
      FROM  ZTBL
     WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
      WHEN 1.
        SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                      FROM ZTBL
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_BL_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
  ENDIF.

  IF NOT ZSREQHD-ZFBLNO IS INITIAL.
    ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
* B/L 문서 조?
    PERFORM   P1000_READ_BL_DOC.
* 반입예정정보 조?
    SELECT SINGLE * FROM ZTBLINOU  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                   AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
    IF SY-SUBRC NE 0.
      MESSAGE E044 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
    ENDIF.
* 반입신고 조?
    SELECT SINGLE * FROM ZTBLINR   WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                   AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
    IF SY-SUBRC NE 0.
      MESSAGE E045 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
    ENDIF.

* 반출신고 조?
    SELECT SINGLE * FROM ZTBLOUR   WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                   AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.

    IF SY-TCODE EQ 'ZIMO1'.   " 생성?
      IF SY-SUBRC EQ 0.
        MESSAGE E056 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
      ELSE.
        CLEAR  *ZTBLOUR.
      ENDIF.
    ELSE.                     " 기타..
      IF SY-SUBRC NE 0.
        MESSAGE E055 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
      ELSE.
         *ZTBLOUR  =  ZTBLOUR.
      ENDIF.
    ENDIF.
* 반입정보 LOCK
    PERFORM P2000_SET_BLINR_LOCK    USING    'L'.
  ENDIF.

ENDMODULE.                 " READ_BL_DOC_SCR9221  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9221  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9221 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  CASE SY-TCODE.
    WHEN 'ZIMO1'.
      PERFORM   P2000_DATA_IN_TO_OUT.
      MOVE C_REQ_C TO W_STATUS.
    WHEN 'ZIMO2'.
      MOVE C_REQ_U TO W_STATUS.
    WHEN 'ZIMO3'.
      MOVE C_REQ_D TO W_STATUS.
    WHEN 'ZIMO4'.
      MOVE C_OPEN_C TO W_STATUS.
  ENDCASE.

  SET SCREEN 9220.  LEAVE SCREEN.


ENDMODULE.                 " USER_COMMAND_SCR9221  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9226  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9226 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.
*   IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
*   IF SY-UCOMM EQ 'DISD'.
  MOVE C_REQ_D TO W_STATUS.
*   ELSE.
*      MOVE C_REQ_C TO W_STATUS.
*   ENDIF.

  SET SCREEN 9227.  LEAVE SCREEN.
*   ENDIF.
ENDMODULE.                 " USER_COMMAND_SCR9226  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9228  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9228 INPUT.

  IF SY-UCOMM EQ 'BACK' OR SY-UCOMM EQ 'EXIT'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

  MOVE C_REQ_D TO W_STATUS.

  SET SCREEN 9227.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR9228  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_VENDOR_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_VENDOR_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Benificiary
  PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                                 CHANGING   W_VENDOR_NM.

ENDMODULE.                 " GET_VENDOR_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FWDR_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_FWDR_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFFORD
                              CHANGING   W_FWDR_NM.

* FWDR 변경시 마다 해외비용 Vendor 및 지불처 코드 변?
  IF ZTIMIMG11-ZFVNCT IS INITIAL.
    LOOP AT IT_ZSBLCST.
      IT_ZSBLCST-ZFVEN = ZTBL-ZFFORD.
      IF W_LFA1-LNRZA IS INITIAL.
        IT_ZSBLCST-ZFPAY = ZTBL-ZFFORD.
      ELSE.
        IT_ZSBLCST-ZFPAY = W_LFA1-LNRZA.
      ENDIF.
      MODIFY IT_ZSBLCST INDEX SY-TABIX.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " GET_FWDR_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_HAYEK_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_HAYEK_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTIMIMG11-ZFVNCT EQ 'X'.
    PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFHAYEK
                                CHANGING   W_HAYEK_NM.

    CLEAR : W_LFA1.
    IF NOT ZTBL-ZFHAYEK IS INITIAL.
      SELECT SINGLE * INTO W_LFA1 FROM LFA1
                      WHERE LIFNR EQ ZTBL-ZFHAYEK.
    ENDIF.
* 운임지불업체 변경시 마다 해외,국내비용 Vendor 및 지불처 코드 변경.
    LOOP AT IT_ZSBLCST.
      IT_ZSBLCST-ZFVEN = ZTBL-ZFHAYEK.
      IF W_LFA1-LNRZA IS INITIAL.
        IT_ZSBLCST-ZFPAY = ZTBL-ZFHAYEK.
      ELSE.
        IT_ZSBLCST-ZFPAY = W_LFA1-LNRZA.
      ENDIF.
      MODIFY IT_ZSBLCST INDEX SY-TABIX.
    ENDLOOP.

    LOOP AT IT_ZSBLCST1.
      IT_ZSBLCST1-ZFVEN = ZTBL-ZFHAYEK.
      IF W_LFA1-LNRZA IS INITIAL.
        IT_ZSBLCST1-ZFPAY = ZTBL-ZFHAYEK.
      ELSE.
        IT_ZSBLCST1-ZFPAY = W_LFA1-LNRZA.
      ENDIF.
      MODIFY IT_ZSBLCST1 INDEX SY-TABIX.
    ENDLOOP.

  ENDIF.
ENDMODULE.                 " GET_HAYEK_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TRUC_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_TRUC_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFTRCK
                              CHANGING   W_TRUK_NM.

* TRUCKER 변경시 마다 해외비용 Vendor 및 지불처 코드 변?
  LOOP AT IT_ZSBLCST1.
    IF IT_ZSBLCST1-ZFVEN IS INITIAL.
      IT_ZSBLCST1-ZFVEN =  ZTBL-ZFTRCK.
    ENDIF.
    IF IT_ZSBLCST1-ZFPAY IS INITIAL.
      IF W_LFA1-LNRZA IS INITIAL.
        IT_ZSBLCST1-ZFPAY =  ZTBL-ZFTRCK.
      ELSE.
        IT_ZSBLCST1-ZFPAY =  W_LFA1-LNRZA.
      ENDIF.
    ENDIF.
    MODIFY IT_ZSBLCST1 INDEX SY-TABIX.
  ENDLOOP.

ENDMODULE.                 " GET_TRUC_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BNAR_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_BNAR_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF NOT ZTBL-ZFBNARCD IS INITIAL.
    CLEAR :  ZTIMIMG03.

    SELECT * FROM ZTIMIMG03 WHERE ZFBNARCD EQ ZTBL-ZFBNARCD.
    ENDSELECT.

    W_ZFBNARM = ZTIMIMG03-ZFBNARM.
  ELSE.
    CLEAR : W_ZFBNARM.
  ENDIF.
ENDMODULE.                 " GET_BNAR_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CARGO_TYPE_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_CARGO_TYPE_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF NOT ZTBL-ZFCAGTY IS INITIAL.
    PERFORM   GET_DD07T_SELECT USING      'ZDCAGTY'  ZTBL-ZFCAGTY
                               CHANGING   W_CARGO_TYPE.
  ELSE.
    CLEAR : W_CARGO_TYPE.
  ENDIF.
ENDMODULE.                 " GET_CARGO_TYPE_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_AMOUNT_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_BL_AMOUNT_CHECK_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  CHECK : ZTBL-ZFPOYN EQ 'Y'.

  IF ZTBL-ZFBLAMT IS INITIAL.
    MESSAGE W167 WITH 'B/L Amount'.
  ENDIF.
* IF ZTBL-ZFTRTEC IS INITIAL.
*    ZTBL-ZFTRTEC = ZTBL-ZFBLAMC.
* ENDIF.

ENDMODULE.                 " GET_BL_AMOUNT_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ORIJIN_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ORIJIN_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CLEAR : T005T.
  SELECT SINGLE * FROM T005T WHERE   SPRAS EQ SY-LANGU
                             AND     LAND1 EQ ZTBL-ZFCARC.

  W_ORIGIN_NM = T005T-LANDX.

* GET 선적국가 CURRENCY
  PERFORM P2000_GET_SHIP_PORT_CURRENCY.

ENDMODULE.                 " GET_ORIJIN_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_VIA_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_VIA_CHECK_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*> VIA.
  IF ZTBL-ZFVIA IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFVIA'.
  ENDIF.

*> VIA
  IF ZTBL-INCO1 EQ 'EXW' OR ZTBL-INCO1 EQ 'FCA' OR
     ZTBL-INCO1 EQ 'FAS' OR ZTBL-INCO1 EQ 'FOB'.

    IF ZTBL-ZFVIA EQ 'VSL'.
      IF ZTBL-ZFSHTY IS INITIAL.
        SET CURSOR FIELD ZTBL-ZFSHTY.
        MESSAGE E300 WITH ZTBL-ZFVIA.
      ENDIF.
      IF ZTBL-ZFSHTY EQ 'B' AND        ">BULK 이고.
         ZTBL-ZFMSNO IS INITIAL AND   ">모선 코드가 입력되지 않았을때.
         ZTIMIMG00-ZFMSYN EQ 'X'.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFMSNO'.
      ENDIF.

      IF ZTBL-ZFSHTY NE 'B'.      " 20021008 JSY 한수원 수정.
        IF ZTBL-ZF20FT  IS INITIAL AND
           ZTBL-ZF40FT  IS INITIAL AND
           ZTBL-ZF20FHQ IS INITIAL AND
           ZTBL-ZF40FHQ IS INITIAL AND
           ZTBL-ZFGITA  IS INITIAL.
          MESSAGE E935.
        ENDIF.
      ENDIF.
      ZTBL-ZFTRCUR = 'USD'.

    ELSEIF ZTBL-ZFVIA EQ 'AIR'.
      IF ZTBL-ZFRENT EQ 'X'.
        MESSAGE E539 WITH ZTBL-ZFBLNO.
      ENDIF.
      IF NOT ZTBL-ZFSHTY IS INITIAL.
        SET CURSOR FIELD ZTBL-ZFSHTY.
        MESSAGE E301 WITH ZTBL-ZFVIA.
      ENDIF.
      ZTBL-ZFTRCUR = ZTBL-ZFBLAMC.
    ENDIF.
  ENDIF.

*>> 모선관리번호...
  IF NOT ZTBL-ZFMSNO IS INITIAL AND ZTIMIMG00-ZFMSYN EQ 'X'.
    PERFORM  GET_MATHER_SHIP_NO.
  ENDIF.

* VIA VALUE 변경시 마다 비용 항목 갱신 작업.
  IF ZTIMIMG00-ZFPSMS EQ '1' OR
     ZTIMIMG00-BLCSTMD EQ 'X'.
    IF W_VIA IS INITIAL.
      MOVE : ZTBL-ZFVIA    TO      W_VIA.
      PERFORM    P1000_READ_CHARGE_RECORD  USING  ZTBL-ZFVIA 'A'.
    ELSE.
      IF W_VIA NE ZTBL-ZFVIA.
        PERFORM    P2000_CHARGE_CHANGE_MSG.
        IF ANTWORT EQ 'Y'.
          PERFORM    P1000_READ_CHARGE_RECORD  USING  ZTBL-ZFVIA
                                                      'A'.
          IF ZTBL-ZFVIA = 'AIR'.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
                 EXPORTING
                      INPUT          = 'KG'
                 IMPORTING
                      OUTPUT         = ZTBL-ZFNEWTM
                 EXCEPTIONS
                      UNIT_NOT_FOUND = 1
                      OTHERS         = 2.
            IF SY-SUBRC NE 0.
              CLEAR ZTBL-ZFNEWTM.
            ENDIF.

          ELSEIF ZTBL-ZFVIA = 'VSL'.
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
                 EXPORTING
                      INPUT          = 'TON'
                 IMPORTING
                      OUTPUT         = ZTBL-ZFNEWTM
                 EXCEPTIONS
                      UNIT_NOT_FOUND = 1
                      OTHERS         = 2.
            IF SY-SUBRC NE 0.
              CLEAR ZTBL-ZFNEWTM.
            ENDIF.
          ENDIF.

          MOVE : ZTBL-ZFVIA    TO      W_VIA.
        ELSE.
          ZTBL-ZFVIA    =      W_VIA.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_VIA_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ETD_ETA_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ETD_ETA_CHECK_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTBL-ZFETD > ZTBL-ZFETA.
    MESSAGE E302 WITH ZTBL-ZFETD ZTBL-ZFETA.
  ENDIF.

ENDMODULE.                 " GET_ETD_ETA_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ARR_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ARR_PORT_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*  PERFORM  GET_PORT_NAME     USING      '002'     ZTBL-ZFAPRTC   'E'
*                             CHANGING   W_TMP_TEXT.

*PERFORM   GET_DD07T_SELECT USING      'ZEAPRTC'  ZTBL-ZFAPRTC
*                           CHANGING   W_TMP_TEXT.

  IF ZTBL-ZFAPRT IS INITIAL.
    SELECT SINGLE PORTT INTO ZTBL-ZFAPRT
           FROM   ZTIEPORT
           WHERE  LAND1 EQ ZTBL-ZFAPPC
           AND    PORT  EQ ZTBL-ZFAPRTC.
    IF SY-SUBRC NE 0.
      MESSAGE E421(ZIM1) WITH ZTBL-ZFAPPC ZTBL-ZFAPRTC.
    ENDIF.
*     ZTBL-ZFAPRT = W_TMP_TEXT.
  ENDIF.

ENDMODULE.                 " GET_ARR_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCR0002 INPUT.

  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQ_DATA_SCR0002  INPUT
*&---------------------------------------------------------------------*
MODULE READ_REQ_DATA_SCR0002 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.   EXIT.
    WHEN OTHERS.
  ENDCASE.

  IF ZTBL-ZFHBLNO    IS INITIAL.         " B/L NO를 입력하지 않을 경?
    IF ZTBL-ZFBLNO     IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ENDIF.
  ELSE.
    IF ZTBL-ZFBLNO     IS INITIAL.      " 관리번호가 입력하지 않을 경?
* 해당 House B/L로 Count
      W_COUNT = 0.
      SELECT COUNT( * ) INTO  W_COUNT
                            FROM  ZTBL
                            WHERE ZFHBLNO EQ ZTBL-ZFHBLNO.
      IF W_COUNT EQ 0.
        MESSAGE E305 WITH  ZTBL-ZFHBLNO.
      ELSEIF W_COUNT > 1.
        MESSAGE E303 WITH  W_COUNT.
      ENDIF.
* B/L NO에 MAX인 문서관리번호를 SELECT.
* 00/07/29 김연?
      SELECT SINGLE *
        FROM  ZTBL
       WHERE ZFHBLNO EQ ZTBL-ZFHBLNO.
      EXIT.
*        SELECT SINGLE ZFBLNO INTO  W_BLNO
*                              FROM  ZTBL
*                              WHERE ZFHBLNO EQ ZTBL-ZFHBLNO.
*
*        IF SY-SUBRC NE 0.
*           MESSAGE E305 WITH  ZTBL-ZFHBLNO.
*        ELSE.
*           IF W_HBLNO IS INITIAL.
*              IF ZTBL-ZFBLNO IS INITIAL.  " 관리번호가 입력되지 않?
*                 MESSAGE E305 WITH  ZTBL-ZFHBLNO.
*              ENDIF.
*           ELSE.
*              ZTBL-ZFBLNO = W_BLNO.
*           ENDIF.
*        ENDIF.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
* 기존 문서 READ
*-----------------------------------------------------------------------
* 김연중 막?
  PERFORM   P1000_READ_BL_DOC.

ENDMODULE.                 " READ_REQ_DATA_SCR0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0102_MARK_TC_0102  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0102_MARK_TC_0102 INPUT.

  READ TABLE IT_ZSIV  WITH KEY ZFIVNO = IT_ZSIV-ZFIVNO
                                                    BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX = SY-TABIX.

  IF W_SY_SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIV-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIV-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIV INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0102_MARK_TC_0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0102 INPUT.

  CASE OK-CODE.
    WHEN 'IVDC'.       " INVOICE 조?
      W_SEL_MAT_CNT = 0.
      LOOP AT IT_ZSIV WHERE ZFMARK NE SPACE.
        W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
      ENDLOOP.
      CASE W_SEL_MAT_CNT.
        WHEN 0.
          MESSAGE S951.
        WHEN 1.
          SET PARAMETER ID 'ZPCIVNO' FIELD ''.
          SET PARAMETER ID 'ZPIVNO'  FIELD IT_ZSIV-ZFIVNO.
          CALL TRANSACTION 'ZIM33' AND SKIP  FIRST SCREEN.
        WHEN OTHERS.
          MESSAGE S965.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLCST_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBLCST_UPDATE_SCR0103 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBLCST   INDEX TC_0105-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC EQ 0 AND ZSBLCST IS INITIAL.
    EXIT.
  ENDIF.

  IF ZSBLCST-ZFCSCD IS INITIAL.
    PERFORM P2000_NO_INPUT  USING  'ZSBLCST' 'ZFCSCD'
                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E167 WITH 'Expense code'.
  ELSE.
    IF ZSBLCST-ZFCDNM IS INITIAL.
      SELECT SINGLE ZFCDNM ZFCD2
             INTO (ZSBLCST-ZFCDNM, ZSBLCST-ZFCD2)
             FROM ZTIMIMG08
             WHERE ZFCDTY EQ '004'
             AND   ZFCD   EQ ZSBLCST-ZFCSCD.
      IF ZSBLCST-ZFCSCD IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSBLCST' 'ZFCSCD'
                                DFIES-SCRTEXT_M W_SUBRC.
        MESSAGE E433 WITH 'Overseas freight' ZSBLCST-ZFCSCD.
      ENDIF.
    ENDIF.
  ENDIF.

*>
  MOVE-CORRESPONDING ZSBLCST   TO IT_ZSBLCST.
  MOVE ZTBL-BUKRS              TO IT_ZSBLCST-BUKRS.
*>> 2001.04.11 나현주 추가. 원화인 경우 비용원화금액에 비용금액 SET.
  IT_ZSBLCST-KRW = 'KRW'.
  IF IT_ZSBLCST-WAERS  EQ  'KRW'.
    MOVE  IT_ZSBLCST-ZFCAMT  TO  IT_ZSBLCST-ZFCKAMT.
    MOVE  1                  TO  IT_ZSBLCST-ZFEXRT.
  ENDIF.

  IF IT_ZSBLCST-WAERS IS INITIAL.
    IF ZTBL-ZFVIA EQ 'AIR'.
      IF IT_ZSBLCST-ZFCSCD NE 'AHC'.       " HANDLING CHARGE
        MOVE : ZTBL-ZFTRTEC        TO    IT_ZSBLCST-WAERS.
      ELSE.
        MOVE : 'KRW'               TO    IT_ZSBLCST-WAERS.
      ENDIF.
    ELSEIF ZTBL-ZFVIA EQ 'VSL'.        " OCEAN
      CASE IT_ZSBLCST-ZFCSCD.
* 체선료 / W.F.G / T.H.C / C.F.S / Cont'R Tax / Doc. Fee
        WHEN 'DTC' OR 'WFG' OR 'THC' OR 'CFS' OR 'CTT' OR 'DCF'.
          MOVE : 'KRW'               TO    IT_ZSBLCST-WAERS.
        WHEN OTHERS.
          MOVE : ZTBL-ZFTRTEC        TO    IT_ZSBLCST-WAERS.
      ENDCASE.
    ENDIF.
  ENDIF.

  IF NOT IT_ZSBLCST-KRW IS INITIAL.
    IT_ZSBLCST-KRW = W_KRW.
  ENDIF.
  IF IT_ZSBLCST-WAERS NE ZTBL-ZFTRTEC AND
     IT_ZSBLCST-WAERS NE 'KRW'.
    IT_ZSBLCST-WAERS = ZTBL-ZFTRTEC.
  ENDIF.

*>>> Basic Charge
  IF IT_ZSBLCST-ZFCSCD EQ 'ABC' OR IT_ZSBLCST-ZFCSCD EQ 'OBC'.
    IF ZTBL-ZFTRTPM EQ 'C'.     " 후불.
      IF IT_ZSBLCST-ZFCAMT  IS INITIAL.
        IF OK-CODE NE 'RECL'.
          IF NOT ZTBL-ZFTRTE IS INITIAL AND
             NOT ZTBL-ZFTOWT IS INITIAL AND
             NOT IT_ZSBLCST-ZFCSQ IS INITIAL.
            MESSAGE E364 WITH ZTBL-INCO1 'Basic Charge'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*>>> Other Charge
  IF IT_ZSBLCST-ZFCSCD EQ 'AOC' OR IT_ZSBLCST-ZFCSCD EQ 'OOC'.
    IF ZTBL-ZFOTHPM EQ 'C'.     " 후불.
      IF IT_ZSBLCST-ZFCAMT  IS INITIAL.
        IF OK-CODE NE 'RECL'.
          IF NOT ZTBL-ZFTRTE IS INITIAL AND
             NOT ZTBL-ZFTOWT IS INITIAL AND
             NOT IT_ZSBLCST-ZFCSQ IS INITIAL.
            MESSAGE E364 WITH ZTBL-INCO1 'Other Charge'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*>>>>
  CLEAR LFA1.
  SELECT SINGLE * FROM LFA1
         WHERE LIFNR = IT_ZSBLCST-ZFVEN.
  IF SY-SUBRC NE 0 AND NOT IT_ZSBLCST-ZFCAMT IS INITIAL.
    MESSAGE E025.
  ENDIF.
  IF IT_ZSBLCST-ZFPAY IS INITIAL.
    IF LFA1-LNRZA IS INITIAL.
      MOVE IT_ZSBLCST-ZFVEN  TO IT_ZSBLCST-ZFPAY.   " 지불?
    ELSE.
      MOVE LFA1-LNRZA  TO IT_ZSBLCST-ZFPAY.   " 지불?
    ENDIF.
  ENDIF.

  IF IT_ZSBLCST-ZTERM IS INITIAL.
    SELECT SINGLE ZTERM INTO IT_ZSBLCST-ZTERM   " Payment Term
           FROM LFB1
           WHERE LIFNR = IT_ZSBLCST-ZFVEN
           AND BUKRS   = IT_ZSBLCST-BUKRS.
*     CLEAR : IT_ZSBLCST-ZTERM.
*     SELECT ZTERM INTO  IT_ZSBLCST-ZTERM   " Payment Term
*                  FROM  LFB1 UP TO 1 ROWS
*                  WHERE LIFNR = IT_ZSBLCST-ZFVEN.
*     ENDSELECT.
  ENDIF.
  IF NOT ( IT_ZSBLCST-ZFPAY IS INITIAL ).
    SELECT SINGLE *
      FROM LFA1
     WHERE LIFNR = IT_ZSBLCST-ZFPAY.
    IF SY-SUBRC NE 0 AND NOT IT_ZSBLCST-ZFCAMT IS INITIAL.
      MESSAGE E341.
    ENDIF.
  ENDIF.
  IF IT_ZSBLCST-ZFOCDT IS INITIAL.   " 발생?
    IT_ZSBLCST-ZFOCDT    =    ZTBL-ZFETA.
  ENDIF.
  IF IT_ZSBLCST-MWSKZ IS INITIAL.    " TAX CODE
    IF ZTBL-ZFVIA EQ 'AIR'.
      WZ_VIA  =  'A'.
    ELSEIF ZTBL-ZFVIA EQ 'VSL'.
      WZ_VIA  =  'O'.
    ENDIF.
    SELECT SINGLE ZFCD5 INTO IT_ZSBLCST-MWSKZ FROM ZTIMIMG08
     WHERE ZFCDTY EQ '004'
*        AND ( ZFCD4  EQ WZ_VIA OR
*              ZFCD4  EQ 'B' )
       AND ZFCD   EQ IT_ZSBLCST-ZFCSCD.
    IF IT_ZSBLCST-MWSKZ IS INITIAL.    " TAX CODE
      MESSAGE W167 WITH 'Tax Code'.
    ELSE.
      SELECT SINGLE * FROM T007A
             WHERE KALSM EQ 'TAXKR'
             AND   MWSKZ EQ  IT_ZSBLCST-MWSKZ.
      IF SY-SUBRC NE 0.
        MESSAGE E495 WITH 'TAXKR' IT_ZSBLCST-MWSKZ.
      ENDIF.
    ENDIF.
  ENDIF.
  IF IT_ZSBLCST-ZFWERKS IS INITIAL.    " 대표 PLANT
    IT_ZSBLCST-ZFWERKS  =    ZTBL-ZFWERKS.
  ENDIF.

* ===> TAX RATE
  IF IT_ZSBLCST-MWSKZ IS INITIAL.
    CLEAR : IT_ZSBLCST-KBETR, IT_ZSBLCST-KONWA, IT_ZSBLCST-ZFVAT.
  ELSE.
    PERFORM   P1000_READ_KONP   USING   IT_ZSBLCST-MWSKZ
                                        IT_ZSBLCST-KBETR
                                        IT_ZSBLCST-KONWA.
  ENDIF.
*>>> 금?
  IF IT_ZSBLCST-ZFEXRT IS INITIAL.
    IT_ZSBLCST-ZFCKAMT = 0.
  ELSE.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*      IF IT_ZSBLCST-ZFCKAMT IS INITIAL.
*         IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFCAMT.
*         W_AMOUNT           = IT_ZSBLCST-ZFCAMT.
*
*         PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFCKAMT
*                                                    IT_ZSBLCST-KRW.
*
*         IF IT_ZSBLCST-WAERS NE 'KRW'.
*            IT_ZSBLCST-ZFCKAMT = ZTBL-ZFEXRT * IT_ZSBLCST-ZFCKAMT.
*            W_AMOUNT           = ZTBL-ZFEXRT * W_AMOUNT.
*         ENDIF.
*      ELSE.
*         W_AMOUNT         = IT_ZSBLCST-ZFCKAMT.
*      ENDIF.
* 2000/07/13 김연?
    IF IT_ZSBLCST-ZFCKAMT IS INITIAL.
      IF IT_ZSBLCST-WAERS NE 'KRW'.
        IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFEXRT
                           * IT_ZSBLCST-ZFCAMT.
*            IT_ZSBLCST-ZFCKAMT = ZTBL-ZFEXRT * IT_ZSBLCST-ZFCAMT.

        SELECT SINGLE * FROM  TCURX
               WHERE  CURRKEY     = IT_ZSBLCST-WAERS.
        IF SY-SUBRC NE 0.
          TCURX-CURRDEC = 2.
        ENDIF.

        IF TCURX-CURRDEC NE 0.
          PERFORM SET_CURR_CONV_TO_INTERNAL USING
                 IT_ZSBLCST-ZFCKAMT IT_ZSBLCST-KRW.
        ENDIF.
      ELSE.
        IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFCAMT.
      ENDIF.
    ENDIF.
    W_AMOUNT         = IT_ZSBLCST-ZFCKAMT.
* 2000/07/13 김연?
  ENDIF.
*-----------------------------------------------------------------------
* TAX CODE  ===> 부가세 원단위 절?
* DESC : 유재오 과장 DEFINE (E&Y)
  IF IT_ZSBLCST-ZFVAT IS INITIAL.
    IF NOT IT_ZSBLCST-KBETR IS INITIAL.
      W_AMOUNT = W_AMOUNT * IT_ZSBLCST-KBETR / 10.
      COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
      W_AMOUNT = W_AMOUNT * 10.
      IT_ZSBLCST-ZFVAT = W_AMOUNT.
      PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFVAT
                                                'KRW'.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
*>>> Basic Charge
  IF IT_ZSBLCST-ZFCSCD EQ 'ABC' OR IT_ZSBLCST-ZFCSCD EQ 'OBC'.
    IF ZTBL-ZFTRTPM EQ 'P'.     " 선불.
      IF NOT IT_ZSBLCST-ZFCAMT  IS INITIAL AND
         NOT IT_ZSBLCST-ZFCKAMT IS INITIAL AND
         NOT IT_ZSBLCST-ZFVAT   IS INITIAL.

        IF NOT ZTBL-ZFTRTE IS INITIAL AND
           NOT ZTBL-ZFTOWT IS INITIAL.
          MESSAGE W362 WITH ZTBL-INCO1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*>>> Other Charge
  IF IT_ZSBLCST-ZFCSCD EQ 'AOC' OR IT_ZSBLCST-ZFCSCD EQ 'OOC'.
    IF ZTBL-ZFOTHPM EQ 'P'.     " 선불.
      IF NOT IT_ZSBLCST-ZFCAMT  IS INITIAL AND
         NOT IT_ZSBLCST-ZFCKAMT IS INITIAL AND
         NOT IT_ZSBLCST-ZFVAT   IS INITIAL.

        IF NOT ZTBL-ZFTRTE IS INITIAL AND
           NOT ZTBL-ZFTOWT IS INITIAL.
          MESSAGE W363 WITH ZTBL-INCO1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*>> 부대비 정산 이후 비용 수정시 CHECK!
  CLEAR W_TOT_AMT.
  IF W_SY_SUBRC EQ 0 AND IT_ZSBLCST-ZFCSTYN = 'X'.
    W_TOT_AMT = IT_ZSBLCST-ZFUPCST.
    IF W_TOT_AMT > 0.
      IF W_TOT_AMT > IT_ZSBLCST-ZFCKAMT.
        MESSAGE E471 WITH IT_ZSBLCST-ZFCSQ.
      ELSEIF W_TOT_AMT < IT_ZSBLCST-ZFCKAMT.
        MESSAGE W472 WITH IT_ZSBLCST-ZFCSQ.
      ENDIF.
    ENDIF.
  ENDIF.
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBLCST  INDEX W_TABIX.
  ELSE.
*     IT_ZSBLCST-ZFCSQ   = ( TC_0105-CURRENT_LINE * 10 ) + 10000.
    APPEND IT_ZSBLCST.
  ENDIF.

ENDMODULE.                 " IT_ZSBLCST_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0103_MARK_TC_0105  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0103_MARK_TC_0105 INPUT.

  READ TABLE IT_ZSBLCST  " WITH KEY ZSBLCST(18)  BINARY SEARCH.
                         INDEX TC_0105-CURRENT_LINE.
  IF SY-SUBRC = 0.
**>>> 금?
**      IF ZTBL-ZFEXRT IS INITIAL.
**         IT_ZSBLCST-ZFCKAMT = 0.
**      ELSE.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
**        IT_ZSBLCST-ZFCKAMT = IT_ZSBLCST-ZFCAMT.
**         PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFCKAMT
**                                                    IT_ZSBLCST-KRW.
*
**         IF IT_ZSBLCST-WAERS NE 'KRW'.
**            IF IT_ZSBLCST-ZFCKAMT IS INITIAL.
**               IT_ZSBLCST-ZFCKAMT = ZTBL-ZFEXRT * IT_ZSBLCST-ZFCKAMT.
**            ENDIF.
**         ENDIF.
*      ENDIF.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLCST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLCST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLCST  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0103_MARK_TC_0105  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0103 INPUT.
  CLEAR W_CHK_CNT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.
    CLEAR : W_MARK.
  ENDIF.

  SORT IT_ZSBLCST BY ZFCD2 ZFCSQ.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.
      LOOP AT IT_ZSBLCST   WHERE ZFMARK NE SPACE.
        IF IT_ZSBLCST-ZFCSTYN = 'X'.
          MESSAGE E470 WITH IT_ZSBLCST-ZFCSQ.
        ENDIF.
        IF NOT IT_ZSBLCST-ZFACDO IS INITIAL.
          MESSAGE E469.
        ENDIF.
        DELETE IT_ZSBLCST   INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'INS1'.
      IF LINE GT 0.
        CLEAR : IT_ZSBLCST.
        MOVE : ZTBL-BUKRS   TO   IT_ZSBLCST-BUKRS,
               ZTBL-ZFEXRT  TO   IT_ZSBLCST-ZFEXRT,
               ZTBL-ZFFORD  TO   IT_ZSBLCST-ZFVEN,
               ZTBL-ZFFORD  TO   IT_ZSBLCST-ZFPAY.

        ">> LOCAL CURRENCY GET.
        CLEAR : T001.
        SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTBL-BUKRS.
        MOVE  T001-WAERS   TO  IT_ZSBLCST-KRW.

        INSERT  IT_ZSBLCST INDEX  LINE.
      ELSE.
        MESSAGE S962.
      ENDIF.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSBLCST.
        IT_ZSBLCST-ZFMARK = W_MARK.   MODIFY IT_ZSBLCST.
      ENDLOOP.
    WHEN 'ZIMG17'.
      CALL TRANSACTION 'ZIMG17'.

    WHEN 'RECL'.           " BASIC CHARGE RECALC.
      CLEAR : W_ZSBLCST_ZFCAMT1, W_ZSBLCST_ZFCAMT2, W_ZTBL_ZFTRTE.

      IF ZTBL-ZFTRTPM NE 'C'.
        MESSAGE I362 WITH ZTBL-INCO1.
        EXIT.
      ENDIF.
      ">> TRANSPORTATION
      IF ZTBL-ZFVIA IS INITIAL.
        MESSAGE E167 WITH 'VIA of B/L'.  EXIT.
      ENDIF.
      ">> Loading Country.
      IF ZTBL-ZFCARC IS INITIAL.
        MESSAGE E167 WITH 'Shipping country code of B/L'. EXIT.
      ENDIF.
      ">> Freight Charge Currency
      IF ZTBL-ZFTRTEC IS INITIAL.
        MESSAGE E167 WITH 'Freight rate currency of B/L'.  EXIT.
      ENDIF.
      ">> Chareg Code Set.
      IF ZTBL-ZFVIA EQ 'AIR'.         " AIR
        WZ_VIA = 'A'.   WL_VIA  = 'ABC'.
      ELSEIF ZTBL-ZFVIA EQ 'VSL'.     " OCEAN
        WZ_VIA = 'O'.   WL_VIA  = 'OBC'.
      ENDIF.
      READ TABLE IT_ZSBLCST WITH KEY ZFCSCD =  WL_VIA.
      W_TABIX    =   SY-TABIX.
      IF SY-SUBRC NE 0.   MESSAGE I239 WITH WL_VIA.   EXIT.   ENDIF.
      IF ZTBL-ZFTRTPM IS INITIAL OR ZTBL-ZFTRTPM EQ 'P'.
        MESSAGE E241 WITH ZTBL-INCO1.   EXIT.
      ENDIF.

      W_ZSBLCST_ZFCAMT1 = IT_ZSBLCST-ZFCAMT.

      IF ZTBL-ZFVIA NE 'AIR'.         " AIR
        W_ZSBLCST_ZFCAMT2 = ZTBL-ZFTRTE * ZTBL-ZFTOWT.
        W_ZTBL_ZFTRTE = ZTBL-ZFTRTE.
        ANTWORT = 'Y'.
      ELSE.
        PERFORM   P2000_AIR_BASIC_CHG_CALC.
      ENDIF.

      IF ANTWORT EQ 'Y' AND W_STATUS NE C_REQ_D.   " YES...
        IT_ZSBLCST-ZFCAMT  = W_ZSBLCST_ZFCAMT2.
        PERFORM    SET_CURR_CONV_TO_EXTERNAL USING IT_ZSBLCST-ZFCAMT
                                                   ZTBL-ZFTRTEC
                                                   W_ZSBLCST_ZFCAMT2.
        IF IT_ZSBLCST-ZFEXRT IS INITIAL.
          IF IT_ZSBLCST-WAERS EQ T001-WAERS.
            IT_ZSBLCST-ZFEXRT = 1.
          ELSE.
            IT_ZSBLCST-ZFEXRT = ZTBL-ZFEXRT.
          ENDIF.
        ENDIF.
        IT_ZSBLCST-ZFCKAMT = W_ZSBLCST_ZFCAMT2 * IT_ZSBLCST-ZFEXRT.
        PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFCKAMT
                                                   IT_ZSBLCST-KRW.
        IF NOT IT_ZSBLCST-KBETR IS INITIAL AND
           NOT IT_ZSBLCST-ZFCKAMT IS INITIAL.
          W_AMOUNT = IT_ZSBLCST-ZFCKAMT.
          W_AMOUNT = W_AMOUNT * IT_ZSBLCST-KBETR / 10.
          COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
          W_AMOUNT = W_AMOUNT * 10.
          IT_ZSBLCST-ZFVAT = W_AMOUNT.
          PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST-ZFVAT
                                                     IT_ZSBLCST-KRW.
        ENDIF.
        MODIFY IT_ZSBLCST   INDEX W_TABIX.
        ZTBL-ZFTRTE       = W_ZTBL_ZFTRTE.
        MESSAGE S361.
      ENDIF.

    WHEN 'REF1'.
      LOOP AT IT_ZSBLCST WHERE ZFACDO NE SPACE.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      PERFORM    P2000_CHARGE_REFRESH_MSG.
      IF ANTWORT EQ 'Y'.
        PERFORM    P1000_READ_CHARGE_RECORD  USING  ZTBL-ZFVIA
                                                    'O'.
        MOVE : ZTBL-ZFVIA    TO      W_VIA.
      ELSE.
        ZTBL-ZFVIA    =      W_VIA.
      ENDIF.

    WHEN 'VEN1' OR 'VEN2' OR 'MKPF' OR
         'BKPF'.
      PERFORM   P2000_LINE_SELECT.
      PERFORM   P2000_LINE_CALL_T_CODE.
    WHEN 'SCHG'.
      LOOP AT IT_ZSBLCST WHERE ZFMARK = 'X'.
        W_TABIX   = SY-TABIX.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      MOVE-CORRESPONDING IT_ZSBLCST TO ZSBLCST.
      PERFORM P2000_DOC_CHANGE.
      IF ANTWORT = 'Y'.
        MOVE ' ' TO ZSBLCST-ZFACDO.
        MOVE ' ' TO ZSBLCST-ZFFIYR.
        MOVE-CORRESPONDING ZSBLCST TO IT_ZSBLCST.
        MODIFY IT_ZSBLCST INDEX W_TABIX.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

* TOTAL CHG.
  PERFORM   P2000_TOTAL_CHG_CALC.

ENDMODULE.                 " USER_COMMAND_SCR0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLCON_UPDATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBLCON_UPDATE_SCR0104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSBLCON   WITH KEY ZFCONSEQ = ZSBLCON-ZFCONSEQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSBLCON  TO IT_ZSBLCON.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBLCON  INDEX W_TABIX.
  ELSE.
    IT_ZSBLCON-ZFCONSEQ = TC_0104-CURRENT_LINE * 10.
    APPEND IT_ZSBLCON.
  ENDIF.

ENDMODULE.                 " IT_ZSBLCON_UPDATE_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0104_MARK_TC_0104  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0104_MARK_TC_0104 INPUT.

  READ TABLE IT_ZSBLCON  WITH KEY
                       ZFCONSEQ = ZSBLCON-ZFCONSEQ  BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLCON-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLCON-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLCON INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0104_MARK_TC_0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0104 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 원산지 삭?

      LOOP AT IT_ZSBLCON   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSBLCON  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSBLCON.
        IT_ZSBLCON-ZFMARK = W_MARK.   MODIFY IT_ZSBLCON.
      ENDLOOP.

    WHEN 'REF1'.           " 원산지 Refresh
      REFRESH : IT_ZSBLCON.
      LOOP AT IT_ZSBLCON_ORG.
        MOVE-CORRESPONDING   IT_ZSBLCON_ORG   TO   IT_ZSBLCON.
        APPEND IT_ZSBLCON.
      ENDLOOP.
    WHEN 'VEN3'.           " 운송업?
      W_COUNT = 0.
      LOOP AT IT_ZSBLCON.
        W_COUNT = W_COUNT + 1.
        MOVE-CORRESPONDING IT_ZSBLCON TO ZTBLCON.
      ENDLOOP.
      PERFORM   P2000_LINE_CALL_T_CODE.
    WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_IT_ZSBLCON_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLCST1_UPDATE_SCR0105  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBLCST1_UPDATE_SCR0105 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF W_SY_SUBRC EQ 0 AND ZSBLCST IS INITIAL.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSBLCST1  INDEX TC_0106-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF ZSBLCST-ZFCSCD IS INITIAL.
    PERFORM P2000_NO_INPUT  USING  'ZSBLCST' 'ZFCSCD'
                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E167 WITH 'Expense code'.
  ELSE.
    IF ZSBLCST-ZFCDNM IS INITIAL.
      SELECT SINGLE ZFCDNM ZFCD2
             INTO (ZSBLCST-ZFCDNM, ZSBLCST-ZFCD2)
             FROM ZTIMIMG08
             WHERE ZFCDTY EQ '005'
             AND   ZFCD   EQ ZSBLCST-ZFCSCD.
      IF ZSBLCST-ZFCSCD IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSBLCST' 'ZFCSCD'
                                DFIES-SCRTEXT_M W_SUBRC.
        MESSAGE E433 WITH 'Other freight' ZSBLCST-ZFCSCD.
      ENDIF.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING ZSBLCST   TO IT_ZSBLCST1.
  IT_ZSBLCST1-BUKRS = ZTBL-BUKRS.
  IT_ZSBLCST1-KRW = W_KRW.
  IF IT_ZSBLCST1-WAERS IS INITIAL.
    IT_ZSBLCST1-WAERS = ZTBL-ZFTRTEC.
  ENDIF.

* 필수 입력 검?
  CHECK  NOT ZSBLCST-ZFCKAMT IS INITIAL.
  IF IT_ZSBLCST1-ZFVEN IS INITIAL.
    MESSAGE E025.
  ENDIF.

  CLEAR LFA1.
  SELECT SINGLE *  FROM   LFA1
  WHERE  LIFNR = IT_ZSBLCST1-ZFVEN.
  IF SY-SUBRC NE 0 AND NOT IT_ZSBLCST1-ZFCAMT IS INITIAL.
    MESSAGE E025.
  ENDIF.
  IF IT_ZSBLCST1-ZFPAY IS INITIAL.
    IF LFA1-LNRZA IS INITIAL.
      MOVE IT_ZSBLCST1-ZFVEN  TO IT_ZSBLCST1-ZFPAY.   " 지불?
    ELSE.
      MOVE LFA1-LNRZA  TO IT_ZSBLCST1-ZFPAY.   " 지불?
    ENDIF.
  ENDIF.
  IF IT_ZSBLCST1-ZTERM IS INITIAL.
    SELECT SINGLE ZTERM INTO IT_ZSBLCST1-ZTERM   " Payment Term
    FROM   LFB1
    WHERE  LIFNR = IT_ZSBLCST1-ZFVEN
    AND    BUKRS = IT_ZSBLCST1-BUKRS.
  ENDIF.
  IF NOT ( IT_ZSBLCST1-ZFPAY IS INITIAL ).
    SELECT SINGLE *
    FROM   LFA1
    WHERE  LIFNR = IT_ZSBLCST1-ZFPAY.
    IF SY-SUBRC NE 0 AND NOT IT_ZSBLCST1-ZFCAMT IS INITIAL.
      MESSAGE E341.
    ENDIF.
  ENDIF.
  IF IT_ZSBLCST1-ZFOCDT IS INITIAL.   " 발생?
    IT_ZSBLCST1-ZFOCDT    =    ZTBL-ZFBNDT.
  ENDIF.
  IF IT_ZSBLCST1-MWSKZ IS INITIAL.    " TAX CODE
    IF ZTBL-ZFVIA EQ 'AIR'.
      WZ_VIA  =  'A'.
    ELSEIF ZTBL-ZFVIA EQ 'VSL'.
      WZ_VIA  =  'O'.
    ENDIF.
    SELECT SINGLE ZFCD5 INTO IT_ZSBLCST1-MWSKZ FROM ZTIMIMG08
     WHERE ZFCDTY EQ '005'
*        AND ( ZFCD4  EQ WZ_VIA OR
*              ZFCD4  EQ 'B' )
       AND   ZFCD   EQ IT_ZSBLCST1-ZFCSCD.

    IF IT_ZSBLCST1-MWSKZ IS INITIAL.    " TAX CODE
      MESSAGE W167 WITH 'Tax Code'.
    ELSE.
      SELECT SINGLE * FROM T007A
             WHERE KALSM EQ 'TAXKR'
             AND   MWSKZ EQ  IT_ZSBLCST1-MWSKZ.
      IF SY-SUBRC NE 0.
        MESSAGE E495 WITH 'TAXKR' IT_ZSBLCST1-MWSKZ.
      ENDIF.
    ENDIF.
  ENDIF.
  IF IT_ZSBLCST1-ZFWERKS IS INITIAL.    " 대표 PLANT
    IT_ZSBLCST1-ZFWERKS  =    ZTBL-ZFWERKS.
  ENDIF.

* ===> TAX RATE
  IF IT_ZSBLCST1-MWSKZ IS INITIAL.
    CLEAR : IT_ZSBLCST1-KBETR, IT_ZSBLCST1-KONWA, IT_ZSBLCST1-ZFVAT.
  ELSE.
    PERFORM   P1000_READ_KONP   USING   IT_ZSBLCST1-MWSKZ
                                        IT_ZSBLCST1-KBETR
                                        IT_ZSBLCST1-KONWA.
  ENDIF.

  IF NOT IT_ZSBLCST1-ZFCKAMT IS INITIAL.
    W_AMOUNT = IT_ZSBLCST1-ZFCKAMT.
*-----------------------------------------------------------------------
* TAX CODE  ===> 부가세 원단위 절?

    IF IT_ZSBLCST1-ZFVAT IS INITIAL.
      IF NOT IT_ZSBLCST1-KBETR IS INITIAL.
        W_AMOUNT = W_AMOUNT * IT_ZSBLCST1-KBETR / 10.
        COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
        W_AMOUNT = W_AMOUNT * 10.
        IT_ZSBLCST1-ZFVAT = W_AMOUNT.
        PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSBLCST1-ZFVAT
                                                  'KRW'.
      ENDIF.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
*>> 부대비 정산 이후 금액 수정시 CHECK!
  CLEAR W_TOT_AMT.
  IF W_SY_SUBRC EQ 0 AND IT_ZSBLCST1-ZFCSTYN = 'X'.
    W_TOT_AMT = IT_ZSBLCST1-ZFUPCST.
    IF W_TOT_AMT > 0.
      IF W_TOT_AMT > IT_ZSBLCST1-ZFCKAMT.
        MESSAGE E471 WITH IT_ZSBLCST1-ZFCSQ.
      ELSEIF W_TOT_AMT < IT_ZSBLCST1-ZFCKAMT.
        MESSAGE W472 WITH IT_ZSBLCST1-ZFCSQ.
      ENDIF.
    ENDIF.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBLCST1  INDEX W_TABIX.
  ELSE.
*     IT_ZSBLCST1-ZFCSQ   = ( TC_0106-CURRENT_LINE * 10 ).
    APPEND IT_ZSBLCST1.
  ENDIF.

ENDMODULE.                 " IT_ZSBLCST1_UPDATE_SCR0105  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0105_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0105_MARK_TC_0106 INPUT.

  READ TABLE IT_ZSBLCST1 " WITH KEY ZSBLCST(18)  BINARY SEARCH.
             INDEX TC_0106-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLCST1-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLCST1-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLCST1  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0105_MARK_TC_0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0106  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0106 INPUT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CLEAR W_CHK_CNT.
  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 원산지 삭?
      LOOP AT IT_ZSBLCST1  WHERE ZFMARK NE SPACE.
* 비용배부 후에 삭제불가!
        IF IT_ZSBLCST1-ZFCSTYN = 'X'.
          MESSAGE E470 WITH IT_ZSBLCST-ZFCSQ.
        ENDIF.
        IF NOT IT_ZSBLCST1-ZFACDO IS INITIAL.
          MESSAGE E469.
        ENDIF.
        DELETE IT_ZSBLCST1  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSBLCST1.
        IT_ZSBLCST1-ZFMARK = W_MARK.   MODIFY IT_ZSBLCST1.
      ENDLOOP.
    WHEN 'INS1'.           " 삽입.
      IF LINE GT 0.
        CLEAR : IT_ZSBLCST1.
        MOVE : 'KRW'              TO   IT_ZSBLCST1-KRW,
               'KRW'              TO   IT_ZSBLCST1-WAERS,
               ZTBL-ZFTRCK        TO   IT_ZSBLCST1-ZFVEN,
               ZTBL-ZFTRCK        TO   IT_ZSBLCST1-ZFPAY.
        INSERT  IT_ZSBLCST1 INDEX  LINE.
      ELSE.
        MESSAGE S962.
      ENDIF.

    WHEN 'REF1'.           " 원산지 Refresh
      LOOP AT IT_ZSBLCST1 WHERE ZFACDO NE SPACE.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      PERFORM    P2000_CHARGE_REFRESH_MSG.
      IF ANTWORT EQ 'Y'.
        PERFORM    P1000_READ_CHARGE_RECORD  USING  ZTBL-ZFVIA
                                                    'L'.
        MOVE : ZTBL-ZFVIA    TO      W_VIA.
      ELSE.
        ZTBL-ZFVIA    =      W_VIA.
      ENDIF.
    WHEN 'VEN1' OR 'VEN2' OR 'MKPF'.
      PERFORM   P2000_LINE_SELECT_1.
      PERFORM   P2000_LINE_CALL_T_CODE.
    WHEN 'SCHG'.
      LOOP AT IT_ZSBLCST1 WHERE ZFMARK = 'X'.
        W_TABIX   = SY-TABIX.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      MOVE-CORRESPONDING IT_ZSBLCST1 TO ZSBLCST.
      PERFORM P2000_DOC_CHANGE.
      IF ANTWORT = 'Y'.
        MOVE ' ' TO ZSBLCST-ZFACDO.
        MOVE ' ' TO ZSBLCST-ZFFIYR.
        MOVE-CORRESPONDING ZSBLCST TO IT_ZSBLCST1.
        MODIFY IT_ZSBLCST1 INDEX W_TABIX.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_IT_ZSBLCST1_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0106  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0200  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0200 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
* CONFIGURATION GET.
    SELECT SINGLE * FROM ZTIMIMG00.
    CASE SY-TCODE.
* Covering of Insurance(BL) Display, Change.
      WHEN 'ZIMB2'.
        W_STATUS = C_REQ_U.
        SET SCREEN 4101.  LEAVE TO SCREEN 4101.
      WHEN 'ZIMB3'.
        W_STATUS = C_REQ_D.
        SET SCREEN 4101.  LEAVE TO SCREEN 4101.
      WHEN 'ZIMB4'.
        W_STATUS = C_OPEN_C.
        SET SCREEN 4101.  LEAVE TO SCREEN 4101.
* B/L Change / Display.
      WHEN 'ZIM22'.
        W_STATUS = C_REQ_U.
        PERFORM   P2000_GET_CODE_TEXT.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
      WHEN 'ZIM221'.
        W_STATUS =  C_BL_SEND.
        PERFORM   P2000_GET_CODE_TEXT.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
      WHEN 'ZIM222'.
        W_STATUS =  C_BL_REAL.
        PERFORM   P2000_GET_CODE_TEXT.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
      WHEN 'ZIM223'.
        W_STATUS =  C_BL_COST.
        PERFORM   P2000_INCOTERMS_CHECK.
        PERFORM   P2000_GET_CODE_TEXT.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
      WHEN 'ZIM23'.
        W_STATUS = C_REQ_D.
        PERFORM   P2000_GET_CODE_TEXT.
        SET SCREEN 0101.  LEAVE TO SCREEN 0101.
* L/G Create / Change / Display.
      WHEN 'ZIM26'.
        W_STATUS = C_REQ_C.
        PERFORM   P2000_BL_DATA_REF.
        SET SCREEN 2601.  LEAVE TO SCREEN 2601.
      WHEN 'ZIM27'.
        W_STATUS = C_REQ_U.
        SET SCREEN 2601.  LEAVE TO SCREEN 2601.
      WHEN 'ZIM28'.
        W_STATUS = C_REQ_D.
        SET SCREEN 2601.  LEAVE TO SCREEN 2601.
      WHEN 'ZIM29'.
        W_STATUS = C_OPEN_C.
        SET SCREEN 2601.  LEAVE TO SCREEN 2601.
*>> Cargo management
      WHEN 'ZIM81'.
        W_STATUS = C_REQ_C.
        CLEAR : *ZTCGHD.  REFRESH :IT_ZSCGIT_ORG.
        SET SCREEN 0811.  LEAVE TO SCREEN 0811.
      WHEN 'ZIM82'.
        W_STATUS = C_REQ_U.
        SET SCREEN 0811.  LEAVE TO SCREEN 0811.
      WHEN 'ZIM83'.
        W_STATUS = C_REQ_D.
        SET SCREEN 0811.  LEAVE TO SCREEN 0811.
*>> Expected Carry-in information
      WHEN 'ZIMI1'.
        W_STATUS = C_REQ_C.
        CLEAR : *ZTBLINOU.
        PERFORM   P2000_MOVE_BL_DATA_INOU.
        SET SCREEN 9201.  LEAVE TO SCREEN 9201.
      WHEN 'ZIMI2'.
        W_STATUS = C_REQ_U.
         *ZTBLINOU = ZTBLINOU.
        SET SCREEN 9201.  LEAVE TO SCREEN 9201.
      WHEN 'ZIMI3'.
        W_STATUS = C_REQ_D.
         *ZTBLINOU = ZTBLINOU.
        SET SCREEN 9201.  LEAVE TO SCREEN 9201.
*>> Carry-in information
      WHEN 'ZIMI7'.
        W_STATUS = C_REQ_U.
        SELECT SINGLE * FROM ZTIMIMG00.
        IF ZTIMIMG00-ZFINOU EQ SPACE.
          IF W_ERR_CHK EQ 'N'.
             *ZTBLINR_TMP = ZTBLINR_TMP.
            SET SCREEN 9215.  LEAVE TO SCREEN 9215.
          ENDIF.
        ELSE.
           *ZTBLINR = ZTBLINR.
          SET SCREEN 9210.  LEAVE TO SCREEN 9210.
        ENDIF.
      WHEN 'ZIMI8'.
        W_STATUS = C_REQ_D.
        SELECT SINGLE * FROM ZTIMIMG00.
        IF ZTIMIMG00-ZFINOU EQ SPACE.
          IF W_ERR_CHK EQ 'N'.
             *ZTBLINR_TMP = ZTBLINR_TMP.
            SET SCREEN 9215.  LEAVE TO SCREEN 9215.
          ENDIF.
        ELSE.
           *ZTBLINR = ZTBLINR.
          SET SCREEN 9210.  LEAVE TO SCREEN 9210.
        ENDIF.
      WHEN 'ZIMI9'.
        W_STATUS = C_OPEN_C.
         *ZTBLINR = ZTBLINR.
        SET SCREEN 9210.  LEAVE TO SCREEN 9210.
* Carry-out create/change/display
      WHEN 'ZIMO2'.
        W_STATUS = C_REQ_U.
         *ZTBLOUR = ZTBLOUR.
        SET SCREEN 9210.  LEAVE TO SCREEN 9220.
      WHEN 'ZIMO3'.
        W_STATUS = C_REQ_D.
         *ZTBLOUR = ZTBLOUR.
        SET SCREEN 9210.  LEAVE TO SCREEN 9220.
      WHEN 'ZIMO4'.
        W_STATUS = C_OPEN_C.
         *ZTBLOUR = ZTBLOUR.
        SET SCREEN 9210.  LEAVE TO SCREEN 9220.
      WHEN 'ZIMO6'.
        W_STATUS = C_OPEN_C.
         *ZTBLOUR = ZTBLOUR.
        SET SCREEN 9227.  LEAVE TO SCREEN 9227.
* Customs clearance declaration
      WHEN 'ZIM62'.
        W_STATUS = C_REQ_U.
        MOVE  SY-DATUM   TO   ZTIDR-ZFIDWDT.
        SET SCREEN 6210.  LEAVE TO SCREEN 6210.
      WHEN 'ZIM63'.
        W_STATUS = C_REQ_D.
        SET SCREEN 6210.  LEAVE TO SCREEN 6210.
* Import License
      WHEN 'ZIM74'.
        W_STATUS = C_REQ_C.
        SET SCREEN 7410.  LEAVE TO SCREEN 7410.
      WHEN 'ZIM75'.
        W_STATUS = C_REQ_U.
        SET SCREEN 7410.  LEAVE TO SCREEN 7410.
      WHEN 'ZIM76'.
        W_STATUS = C_REQ_D.
        SET SCREEN 7410.  LEAVE TO SCREEN 7410.
* Payment Notice Create/Change/Display.
      WHEN 'ZIMP2'.
        W_STATUS = C_REQ_C.
        SET SCREEN 8210.  LEAVE TO SCREEN 8210.
      WHEN 'ZIMP3'.
        W_STATUS = C_REQ_U.
        SET SCREEN 8210.  LEAVE TO SCREEN 8210.
      WHEN 'ZIMP4'.
        W_STATUS = C_REQ_D.
        SET SCREEN 8210.  LEAVE TO SCREEN 8210.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0010 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0010-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0010  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0010 INPUT.

  READ TABLE IT_ZSREQHD   WITH KEY ZFBLNO = ZSREQHD-ZFBLNO
                          BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSREQHD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSREQHD-ZFMARK.
    ENDIF.
    MODIFY IT_ZSREQHD INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  CASE OK-CODE.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    W_SEL_MAT_CNT = 0.
    LOOP AT IT_ZSREQHD WHERE ZFMARK EQ 'X'.
      W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
      MOVE-CORRESPONDING  IT_ZSREQHD  TO  W_ZSREQHD.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE I962.
    ELSE.
      IF W_SEL_MAT_CNT NE 1.
        MESSAGE I965.
      ELSE.
        ZSREQHD-ZFBLNO  = W_ZSREQHD-ZFBLNO.
        ZSREQHD-ZFHBLNO = W_ZSREQHD-ZFHBLNO.
        SET SCREEN 0.   LEAVE SCREEN.
      ENDIF.
    ENDIF.
  ELSE.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SHIP_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_SHIP_PORT_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*  PERFORM  GET_PORT_NAME     USING      '002'     ZTBL-ZFSPRTC  'E'
*                             CHANGING   W_TMP_TEXT.

*  CLEAR : ZTIMIMG08.
*  SELECT SINGLE * FROM ZTIMIMG08 WHERE ZFCDTY   EQ   '002'
*                                 AND   ZFCD     EQ   ZTBL-ZFSPRTC
*                                 AND   ZFCD2    EQ   ZTBL-ZFCARC.
*  IF SY-SUBRC NE 0.
*     MESSAGE ID 'ZIM' TYPE 'E' NUMBER 308 WITH ZTBL-ZFSPRTC.
*  ENDIF.
*
* W_TMP_TEXT = ZTIMIMG08-ZFCDNM.
**PERFORM   GET_DD07T_SELECT USING      'ZEAPRTC'  ZTBL-ZFAPRTC
**                           CHANGING   W_TMP_TEXT.

  IF ZTBL-ZFSPRT IS INITIAL.
    SELECT SINGLE PORTT INTO ZTBL-ZFSPRT
           FROM   ZTIEPORT
           WHERE  LAND1 EQ ZTBL-ZFCARC
           AND    PORT  EQ ZTBL-ZFSPRTC.
    IF SY-SUBRC NE 0.
      MESSAGE E421(ZIM1) WITH ZTBL-ZFCARC ZTBL-ZFSPRTC.
    ENDIF.
*     ZTBL-ZFSPRT = W_TMP_TEXT.
  ENDIF.

ENDMODULE.                 " GET_SHIP_PORT_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PONC_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_PONC_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF ZTBL-ZFPONC IS INITIAL.
    EXIT.
  ENDIF.
  PERFORM  GET_PORT_NAME     USING    '001'   ZTBL-ZFPONC 'I'
                             CHANGING   W_TMP_TEXT.

  W_IMGR_NM = W_TMP_TEXT.

ENDMODULE.                 " GET_PONC_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_LG_DOC_SCR2600  INPUT
*&---------------------------------------------------------------------*
MODULE READ_LG_DOC_SCR2600 INPUT.
  IF SY-TCODE NE 'ZIM26'.
    IF ZSREQHD-ZFLGSEQ IS INITIAL.
      REFRESH : IT_ZTLG.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTLG
               FROM   ZTLG
               WHERE  ZFBLNO   EQ ZTBL-ZFBLNO
               ORDER  BY ZFLGSEQ.
      IF SY-SUBRC NE 0.
        MESSAGE E053 WITH ZTBL-ZFBLNO.
      ENDIF.
      W_COUNT = SY-DBCNT.
      IF  SY-DBCNT EQ 1.
        LOOP AT IT_ZTLG.
          ZSREQHD-ZFLGSEQ = IT_ZTLG-ZFLGSEQ.
        ENDLOOP.
      ELSE.
        INCLUDE = 'ZTLG'.
        CALL SCREEN 0014 STARTING AT  08 3
                         ENDING   AT  90 15.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        ZSREQHD-ZFLGSEQ = W_LGSEQ.
      ENDIF.
    ELSE.
      W_COUNT = 1.
    ENDIF.
  ENDIF.
  PERFORM   P1000_READ_LG_DOC.

ENDMODULE.                 " READ_LG_DOC_SCR2600  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSLGGOD_UPDATE_SCR2604  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSLGGOD_UPDATE_SCR2604 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* Internal Table Read
  READ TABLE IT_ZSLGGOD    WITH KEY ZFLGOD = ZSLGGOD-ZFLGOD.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSLGGOD   TO IT_ZSLGGOD.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSLGGOD   INDEX W_TABIX.
  ELSE.
    IT_ZSLGGOD-ZFLGOD = TC_2604-CURRENT_LINE * 10.
    APPEND IT_ZSLGGOD.
  ENDIF.

ENDMODULE.                 " IT_ZSLGGOD_UPDATE_SCR2604  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR2604_MARK_TC_2604  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR2604_MARK_TC_2604 INPUT.

  READ TABLE IT_ZSLGGOD
       WITH KEY ZFLGOD = ZSLGGOD-ZFLGOD BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSLGGOD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSLGGOD-ZFMARK.
    ENDIF.
    MODIFY IT_ZSLGGOD INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR2604_MARK_TC_2604  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2604  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2604 INPUT.
  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 삭제 및 취?
      LOOP AT IT_ZSLGGOD   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSLGGOD   INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSLGGOD.
        IT_ZSLGGOD-ZFMARK = W_MARK.   MODIFY IT_ZSLGGOD.
      ENDLOOP.
    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSLGGOD.
      LOOP AT IT_ZSLGGOD_ORG.
        MOVE-CORRESPONDING   IT_ZSLGGOD_ORG  TO   IT_ZSLGGOD.
        APPEND IT_ZSLGGOD.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
* Index 수정 작?
*-----------------------------------------------------------------------
  PERFORM   P2000_IT_ZSLGGOD_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR2604  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OPEN_BANK_NAME_SCR2602  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OPEN_BANK_NAME_SCR2602 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTLG-ZFISBNC IS INITIAL.
    ZTLG-ZFEDICK = 'X'.    EXIT.
  ENDIF.

* 개설은행 NAME1 GET
  PERFORM  P1000_GET_VENDOR   USING      ZTLG-ZFISBNC
                              CHANGING   W_TEXT18.
  IF ZTLG-ZFISBNM IS INITIAL AND
     ZTLG-ZFISBB  IS INITIAL AND
     ZTLG-ZFISBNCD IS INITIAL.
    MOVE : W_LFA1-NAME1    TO   ZTLG-ZFISBNM,       " 은행?
           W_LFA1-NAME2    TO   ZTLG-ZFISBB,        " 지점?
*           W_LFA1-BAHNS    TO   ZTMLCHD-ZFOPBNCD.   " 수발신식별?
           W_LFA1-KRAUS    TO   ZTLG-ZFISBNCD.      " 한국은행부?
  ENDIF.

ENDMODULE.                 " GET_OPEN_BANK_NAME_SCR2602  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OPEN_BANK_CODE_SCR2602  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OPEN_BANK_CODE_SCR2602 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

ENDMODULE.                 " GET_OPEN_BANK_CODE_SCR2602  INPUT
*&---------------------------------------------------------------------*
*&      Module  OPEN_BANK_NAME_CHK_SCR2602  INPUT
*&---------------------------------------------------------------------*
MODULE OPEN_BANK_NAME_CHK_SCR2602 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTLG-ZFISBNM  IS INITIAL.
    MESSAGE E310.
  ENDIF.

ENDMODULE.                 " OPEN_BANK_NAME_CHK_SCR2602  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR2601  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR2601 INPUT.
  CASE OK-CODE.
    WHEN 'ZIMG'.                    " Import IMG
      CALL TRANSACTION OK-CODE.
    WHEN 'DDLC'.           " Double click
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP' OR 'ADDC' OR 'OPDC'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE' OR 'DELR' OR 'REVK' OR 'OPCL' OR 'EDIS'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'CKEK'.           " 검?
      CASE SY-DYNNR.
        WHEN '2601'.
          PERFORM    P2000_LG_STATUS_CHECK.
          IF  ZTLG-ZFEDICK = 'O'.
            MESSAGE I315 WITH ZTLG-ZFBLNO.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'HIST'.           " HEADER CHANGE DOCUMENT
      PERFORM   P2000_HEADER_CHANGE_DOC.
    WHEN 'FLAT'.           " FLAT DATA
      PERFORM P2000_SHOW_FLAT.
    WHEN 'STAT'.           " STATUS
      PERFORM  P2000_STATUS_DISPLAY.
    WHEN 'ZIM23'.          " B/L DOC.
      PERFORM   P2000_BL_DOC_DISPLAY    USING  ZTLG-ZFBLNO.
    WHEN 'ZIBK'.           " 발급은?
      PERFORM   P2000_VENDOR_DISPLAY    USING  ZTLG-ZFISBNC.
    WHEN 'ZISH'.           " 선박(운송)회?
      PERFORM   P2000_VENDOR_DISPLAY    USING  ZTLG-ZFCARIR.
    WHEN 'MK03'.           " 송화?
      PERFORM   P2000_VENDOR_DISPLAY    USING  ZTLG-ZFGSCD.
    WHEN 'LGPT'.
      PERFORM   P2000_LG_PRINT.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR2601  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_LG_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_LG_DOC_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.
  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.         " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ELSE.
* B/L READ PERFORM ?
      ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
      PERFORM   P1000_READ_BL_DOC.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTLG
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTLG
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          ZSREQHD-ZFBLNO  = W_BLNO.
          ZSREQHD-ZFHBLNO = W_HBLNO.
      ENDCASE.
    ENDIF.
* B/L READ PERFORM ?
    ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
    PERFORM   P1000_READ_BL_DOC.
  ENDIF.

ENDMODULE.                 " READ_BL_LG_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_LC_DOC_DISPLAY USING    P_ZFREQNO
                                   P_ZFOPNNO.

  IF P_ZFREQNO IS INITIAL AND  P_ZFOPNNO IS INITIAL.
    MESSAGE E063.
  ENDIF.

  SET PARAMETER ID 'BES' FIELD ''.
  EXPORT 'BES'  TO MEMORY ID 'BES'.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'ZPREQNO'  TO MEMORY ID 'ZPREQNO'.
  SET PARAMETER ID 'ZPOPNNO' FIELD P_ZFOPNNO.
  EXPORT 'ZPOPNNO'  TO MEMORY ID 'ZPOPNNO'.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  GET_ARRIVE_ORIJIN_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_ARRIVE_ORIJIN_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CLEAR : T005T.
  SELECT SINGLE * FROM T005T WHERE   SPRAS EQ SY-LANGU
                             AND     LAND1 EQ ZTBL-ZFAPPC.

  W_ORIGIN_NM1 = T005T-LANDX.

ENDMODULE.                 " GET_ARRIVE_ORIJIN_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_REPRESENT_DOC_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_REPRESENT_DOC_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CLEAR : W_ZFREQNO.

  IF ZTBL-ZFREBELN IS INITIAL AND     " P/O NO를 입력하지 않을 경?
   ZTBL-ZFOPNNO  IS INITIAL.        " 문서번호가 입력하지 않을 경?
*    ZTBL-ZFREQNO  IS INITIAL.        " 관리번호가 입력하지 않을 경?
    MESSAGE I066.    EXIT.
  ENDIF.
*
  IF NOT ZTBL-ZFOPNNO IS INITIAL.  " 문서번호가 입력된 경?
*    IF ZTBL-ZFREQNO IS INITIAL.
* 문서 승인번?
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTREQST
                      WHERE ZFOPNNO EQ ZTBL-ZFOPNNO
                      AND   ZFREQTY NE 'PU'
                      AND   ZFREQTY NE 'LO'
                      AND   ZFAMDNO EQ '00000'.
*                         AND   ZFDOCST EQ 'O'.
*                         OR    ZFDOCST EQ 'A' ).
    CASE W_COUNT.
      WHEN 0.     MESSAGE E067 WITH ZTBL-ZFOPNNO.
      WHEN 1.
        SELECT * UP TO 1 ROWS
                 FROM ZTREQST
                 WHERE ZFOPNNO EQ ZTBL-ZFOPNNO
                 AND   ZFAMDNO EQ '00000'
                 AND   ZFREQTY NE 'LO'
                 AND   ZFREQTY NE 'PU'.
*                      AND   ZFDOCST EQ 'O'.
*                      OR    ZFDOCST EQ 'A' ).
          EXIT.
        ENDSELECT.
        W_ZFREQNO = ZTREQST-ZFREQNO.
      WHEN OTHERS.
        PERFORM P2000_OPEN_DOC_SELECT    USING ZTBL-ZFOPNNO.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
    ENDCASE.
*    ENDIF.
  ENDIF.

  IF NOT ZTBL-ZFREBELN IS INITIAL AND   " 관리번호가 입력하지 않을 경?
     W_ZFREQNO IS INITIAL.
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE EBELN EQ ZTBL-ZFREBELN
                      AND   ZFAMDNO EQ '00000'
                      AND   ZFREQTY NE 'LO'
                      AND   ZFREQTY NE 'PU'.
*                      AND   ZFDOCST EQ 'O'.
*                      OR    ZFDOCST EQ 'A' ).

    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZTBL-ZFREBELN.
      WHEN 1.
        SELECT * UP TO 1 ROWS FROM ZVREQHD_ST
                         WHERE EBELN EQ ZTBL-ZFREBELN
                          AND   ZFAMDNO EQ '00000'
                          AND   ZFREQTY NE 'LO'
                          AND   ZFREQTY NE 'PU'.
*                             AND   ZFDOCST EQ 'O'.
*                            OR    ZFDOCST EQ 'A' ).
          EXIT.
        ENDSELECT.
        W_ZFREQNO = ZVREQHD_ST-ZFREQNO.
      WHEN OTHERS.
        PERFORM P2000_OPEN_DOC_SELECT1   USING    ZTBL-ZFREBELN.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.

    ENDCASE.
  ENDIF.
* 문서 조?
  CLEAR : ZVREQHD_ST.
  SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO FROM ZTREQST
                        WHERE ZFREQNO EQ W_ZFREQNO.

  SELECT SINGLE * FROM ZVREQHD_ST
                  WHERE ZFREQNO EQ W_ZFREQNO
                  AND   ZFAMDNO EQ W_ZFAMDNO.

  IF ZVREQHD_ST-ZFREQTY EQ 'LO' OR ZVREQHD_ST-ZFREQTY EQ 'PU'.
    MESSAGE E317 WITH ZVREQHD_ST-ZFREQNO ZVREQHD_ST-ZFREQTY.
  ENDIF.

*  IF NOT ( ZVREQHD_ST-ZFDOCST EQ 'O' OR ZVREQHD_ST-ZFDOCST EQ 'A' ).
*     MESSAGE E318 WITH ZVREQHD_ST-ZFREQNO ZVREQHD_ST-ZFDOCST.
*  ENDIF.

  MOVE : ZVREQHD_ST-ZFWERKS      TO     ZTBL-ZFWERKS,
*        ZVREQHD_ST-ZFREQNO      TO     ZTBL-ZFREQNO,
         ZVREQHD_ST-EBELN        TO     ZTBL-ZFREBELN,
         ZVREQHD_ST-ZFOPNNO      TO     ZTBL-ZFOPNNO,
         ZVREQHD_ST-ZFMATGB      TO     ZTBL-ZFMATGB,
         ZVREQHD_ST-LIFNR        TO     ZTBL-LIFNR,
         ZVREQHD_ST-MAKTX        TO     ZTBL-ZFRGDSR,
         ZVREQHD_ST-ZFPRNAM      TO     ZTBL-ZFPRNAM,
         ZVREQHD_ST-INCO1        TO     ZTBL-INCO1.

*  IF ZTBL-ZFSHNO IS INITIAL.
*
*  ENDIF.

ENDMODULE.                 " GET_REPRESENT_DOC_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0011  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0011 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0011-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0011  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR0012  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR0012 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0012-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR0012  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0011  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0011 INPUT.
  CLEAR : W_ZFREQNO.

  CASE OK-CODE.
    WHEN 'DDCL'.
      IF TC_0011-TOP_LINE > LINE.    EXIT.   ENDIF.
      READ TABLE IT_ZSREQHD INDEX LINE.
      IT_ZSREQHD-ZFMARK = 'X'.    MODIFY   IT_ZSREQHD   INDEX  LINE.
      ANTWORT = 'Y'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    W_SEL_MAT_CNT = 0.
    LOOP AT IT_ZSREQHD WHERE ZFMARK EQ 'X'.
      W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
      MOVE-CORRESPONDING  IT_ZSREQHD  TO  W_ZSREQHD.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE I962.
    ELSE.
      IF W_SEL_MAT_CNT NE 1.
        MESSAGE I965.
      ELSE.
        W_ZFREQNO       = W_ZSREQHD-ZFREQNO.
        ZTBL-ZFREBELN   = W_ZSREQHD-EBELN.
        ZTBL-ZFOPNNO    = W_ZSREQHD-ZFOPNNO.
        SET SCREEN 0.   LEAVE SCREEN.
      ENDIF.
    ENDIF.
  ELSE.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0011  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0011  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0011 INPUT.

  READ TABLE IT_ZSREQHD   WITH KEY ZFREQNO = ZSREQHD-ZFREQNO
                          BINARY SEARCH.

  IF SY-SUBRC = 0.
*       MOVE-CORRESPONDING   ZSREQHD   TO IT_ZSREQHD.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSREQHD-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSREQHD-ZFMARK.
    ENDIF.
*-----------------------------------------------------------------------
* 통화키로 자리수 변경..( External ==> Internal )
*-----------------------------------------------------------------------
*>>> 금?
*       PERFORM    SET_CURR_CONV_TO_INTERNAL USING IT_ZSREQHD-ZFLASTAM
*                                                  IT_ZSREQHD-WAERS.

    MODIFY IT_ZSREQHD INDEX SY-TABIX.

  ENDIF.

ENDMODULE.                 " SET_SCR_MARK_TC_0011  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DOC_SCR0100 INPUT.

  CLEAR : ZTBL, ZVREQHD_ST.
  W_OK_CODE = OK-CODE.
  ANTWORT   = 'Y'.
*-----------------------------------------------------------------------
* OK-CODE
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

*>> < 주기기일 경우 P/O 번호로 진행. >------------
  IF ZSREQHD-ZFMAIN EQ 'X'.
    IF ZSREQHD-ZFMAINPO IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSREQHD' 'ZFMAINPO'.
      EXIT.
    ENDIF.
  ENDIF.
*>>---------------------------------------------------------------------

  IF RADIO_NP EQ 'X'.             " Non-Monetary Transaction
    IF ZSREQHD-ZFPOTY IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSREQHD' 'ZFPOTY'.
    ENDIF.
    "------------------------------------------------------
    " For replace, compensation, repair -> P/O Entry
    "------------------------------------------------------
    IF ZSREQHD-ZFPOTY EQ 'P' OR ZSREQHD-ZFPOTY EQ 'B' OR
       ZSREQHD-ZFPOTY EQ 'A'.
      IF ZSBL-ZFREBELN IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBL' 'ZFREBELN'.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF.

  IF RADIO_PO EQ 'X'.                " P/O Reference
    IF ZSREQHD-EBELN IS INITIAL.     " By PO
      MESSAGE E733.
    ENDIF.
    " P/O No. Count.
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE EBELN EQ ZSREQHD-EBELN
                      AND   ZFAMDNO EQ '00000'.

    CASE W_COUNT.
      WHEN 0.     MESSAGE E243 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT * UP TO 1 ROWS FROM ZVREQHD_ST
                         WHERE EBELN EQ ZSREQHD-EBELN
                          AND   ZFAMDNO EQ '00000'.
          EXIT.
        ENDSELECT.
        W_ZFREQNO = ZVREQHD_ST-ZFREQNO.
      WHEN OTHERS.
        PERFORM P2000_OPEN_DOC_SELECT1   USING     ZSREQHD-EBELN.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
  ELSEIF RADIO_RQ EQ 'X'.             " Import Request Input.
    IF ZSREQHD-ZFREQNO IS INITIAL.
      MESSAGE E737.
    ENDIF.
    W_ZFREQNO = ZSREQHD-ZFREQNO.
  ELSEIF RADIO_LC EQ 'X'.             " L/C No. Input
    IF ZSREQHD-ZFOPNNO IS INITIAL.
      MESSAGE E736.
    ENDIF.
    PERFORM   P1000_GET_LC_DOC_NUM   USING   ZSREQHD-ZFOPNNO.
  ENDIF.
  IF NOT ZSREQHD-ZFMAIN IS INITIAL. EXIT. ENDIF.

  IF W_ZFREQNO IS INITIAL .
    MESSAGE E320.  EXIT.
  ENDIF.
  CLEAR : ZVREQHD_ST.

  SELECT SINGLE * FROM ZVREQHD_ST
                  WHERE ZFREQNO EQ W_ZFREQNO
                  AND   ZFAMDNO EQ W_ZFAMDNO.

  IF SY-SUBRC NE 0.
    MESSAGE E018 WITH W_ZFREQNO.
  ENDIF.
  " L/C Close Yes/No...
  IF ZVREQHD_ST-ZFCLOSE EQ 'X'.
    MESSAGE E035 WITH ZVREQHD_ST-ZFREQNO.
  ENDIF.

  IF ZVREQHD_ST-ZFREQTY EQ 'LO' OR ZVREQHD_ST-ZFREQTY EQ 'PU'.
    MESSAGE E317 WITH ZVREQHD_ST-ZFREQNO ZVREQHD_ST-ZFREQTY.
  ENDIF.

*-----------------------------------------------------------------------
*  IF NOT ( ZVREQHD_ST-ZFDOCST EQ 'O' OR ZVREQHD_ST-ZFDOCST EQ 'A' ).
*     MESSAGE E318 WITH ZVREQHD_ST-ZFREQNO ZVREQHD_ST-ZFDOCST.
*  ENDIF.
*-----------------------------------------------------------------------

ENDMODULE.                 " READ_DOC_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0012  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0012 INPUT.
  CLEAR : W_ZFREQNO.

  CASE OK-CODE.
    WHEN 'DDCL'.
      IF TC_0012-TOP_LINE > LINE.    EXIT.   ENDIF.
      READ TABLE IT_ZSREQHD INDEX LINE.
      IT_ZSREQHD-ZFMARK = 'X'.    MODIFY   IT_ZSREQHD   INDEX  LINE.
      ANTWORT = 'Y'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    W_SEL_MAT_CNT = 0.
    LOOP AT IT_ZSREQHD WHERE ZFMARK EQ 'X'.
      W_SEL_MAT_CNT = W_SEL_MAT_CNT + 1.
      MOVE-CORRESPONDING  IT_ZSREQHD  TO  W_ZSREQHD.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE I962.
    ELSE.
      IF W_SEL_MAT_CNT NE 1.
        MESSAGE I965.
      ELSE.
        W_ZFREQNO       = W_ZSREQHD-ZFREQNO.
        ZTBL-ZFREBELN   = W_ZSREQHD-EBELN.
        ZTBL-ZFOPNNO    = W_ZSREQHD-ZFOPNNO.
        SET SCREEN 0.   LEAVE SCREEN.
      ENDIF.
    ENDIF.
  ELSE.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0012  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXCHANGE_RATE_CHANGE  INPUT
*&---------------------------------------------------------------------*
MODULE EXCHANGE_RATE_CHANGE INPUT.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTBL-ZFEXRT IS INITIAL.
    SPOP-TEXTLINE1 = 'No Input Exchange Rate'.
    PERFORM    P2000_GET_EXCHANGE_RATE     USING ZTBL-ZFBLAMC
                                                 ZTBL-ZFBLDT
                                        CHANGING ZTBL-ZFEXRT
                                                 ZTBL-FFACT.
  ENDIF.

ENDMODULE.                 " EXCHANGE_RATE_CHANGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXCHANGE_DATE_CHANGE  INPUT
*&---------------------------------------------------------------------*
MODULE EXCHANGE_DATE_CHANGE INPUT.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  ">> Local Currency Get
  CLEAR : T001.
  SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTBL-BUKRS.

  CHECK NOT ZTBL-ZFEXDTT IS INITIAL.
  PERFORM     P2000_GET_EX_RATE_NODIALOG   USING ZTBL-ZFTRCUR
                                                 T001-WAERS
                                                 ZTBL-ZFEXDTT
                                        CHANGING ZTBL-ZFEXRTT
                                                 ZTBL-ZFFACTT.

  LOOP AT IT_ZSBLCST.
    W_TABIX = SY-TABIX.
    IF IT_ZSBLCST-WAERS  EQ  T001-WAERS.
      MOVE  IT_ZSBLCST-ZFCAMT  TO  IT_ZSBLCST-ZFCKAMT.
      MOVE  1                  TO  IT_ZSBLCST-ZFEXRT.
    ELSE.
      MOVE  ZTBL-ZFEXRTT       TO IT_ZSBLCST-ZFEXRT.
      PERFORM P3000_EXCHAGE_CHARGE.
    ENDIF.
    MODIFY IT_ZSBLCST INDEX W_TABIX.
  ENDLOOP.

ENDMODULE.                 " EXCHANGE_DATE_CHANGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ZFSPRTC_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE SET_ZFSPRTC_SCR0102 INPUT.
*  SET PARAMETER ID 'ZPCD2' FIELD ZTBL-ZFCARC.









ENDMODULE.                 " SET_ZFSPRTC_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE D0014_LIST_CHECK_SCR0014 INPUT.
* list-processing
  LEAVE TO LIST-PROCESSING.
* list Write
  PERFORM P2000_DATA_LISTING.

ENDMODULE.                 " D0014_LIST_CHECK_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_LC_NO_CHECK_SCR2600  INPUT
*&---------------------------------------------------------------------*
MODULE READ_LC_NO_CHECK_SCR2600 INPUT.

  IF ZSREQHD-ZFOPNNO IS INITIAL.
    CLEAR : W_COUNT.
    SELECT DISTINCT ZFREQNO INTO W_ZFREQNO
           FROM  ZTREQIT
           FOR ALL ENTRIES IN IT_ZSBLIT
           WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
           AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO.
      ADD 1 TO W_COUNT.
    ENDSELECT.

    CASE W_COUNT.
      WHEN 0.
      WHEN 1.
        SELECT ZFOPNNO INTO ZSREQHD-ZFOPNNO
               FROM    ZTREQST
               WHERE   ZFREQNO EQ W_ZFREQNO.

        ENDSELECT.
        IF ZSREQHD-ZFOPNNO IS INITIAL.
          MESSAGE E319.
        ENDIF.
        PERFORM   P1000_GET_LC_DOC_NUM   USING   ZSREQHD-ZFOPNNO.
      WHEN OTHERS.
        MESSAGE E208(ZIM1) WITH ZTBL-ZFHBLNO W_COUNT.
*           PERFORM P2000_OPEN_DOC_SELECT1   USING     ZSREQHD-EBELN.
*           IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
*           PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.

*     MESSAGE E319.
  ELSE.

    PERFORM   P1000_GET_LC_DOC_NUM   USING   ZSREQHD-ZFOPNNO.
  ENDIF.

  IF W_ZFREQNO IS INITIAL.
    MESSAGE E320.
  ELSE.
    SELECT SINGLE * FROM ZVREQHD_ST WHERE ZFREQNO EQ W_ZFREQNO
                                    AND   ZFAMDNO EQ W_ZFAMDNO.
  ENDIF.

ENDMODULE.                 " READ_LC_NO_CHECK_SCR2600  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0103_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
MODULE SET_SCR0103_MARK_TC_0103 INPUT.

  READ TABLE IT_ZSIVIT  WITH KEY IT_ZSIVIT(18)  BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSIVIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVIT  INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0103_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_FIELD_NAME_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE GET_FIELD_NAME_SCRCOM INPUT.
  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
ENDMODULE.                 " GET_FIELD_NAME_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  FWDR_READ_IMG03_SCR9201  INPUT
*&---------------------------------------------------------------------*
MODULE FWDR_READ_IMG03_SCR9201 INPUT.

* 발송지 : ZTBLINOU.
  PERFORM   P1000_GET_BONDED_NAME   USING      ZTBLINOU-ZFDBNARC
                                    CHANGING   ZTBLINOU-ZFDBNAR
                                               W_DEL_NM.

ENDMODULE.                 " FWDR_READ_IMG03_SCR9201  INPUT
*&---------------------------------------------------------------------*
*&      Module  ARRIVE_READ_IMG03_SCR9201  INPUT
*&---------------------------------------------------------------------*
MODULE ARRIVE_READ_IMG03_SCR9201 INPUT.
* 도착?
  PERFORM   P1000_GET_BONDED_NAME   USING      ZTBLINOU-ZFABNARC
                                    CHANGING   ZTBLINOU-ZFABNAR
                                               W_ARR_NM.
ENDMODULE.                 " ARRIVE_READ_IMG03_SCR9201  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CARRY_INTO_SCR9210  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CARRY_INTO_SCR9210 INPUT.
  CHECK : W_STATUS NE C_REQ_D.

  IF ZTBLINR-ZFPRIN EQ 'A'.
    IF NOT ZTBLINR-ZFPINS IS INITIAL.
      MESSAGE E210.
    ENDIF.
  ELSE.
    IF ZTBLINR-ZFPINS IS INITIAL.
      MESSAGE E167 WITH 'Partial carry-in balance'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_CARRY_INTO_SCR9210  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_UNIT_SCR9210  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_UNIT_SCR9210 INPUT.
  CHECK : W_STATUS NE C_REQ_D.

  IF NOT ( ZTBLINR-ZFINAQN IS INITIAL AND ZTBLINR-ZFINTQN IS INITIAL ).
    IF ZTBLINR-ZFCT IS INITIAL.
      MESSAGE E167 WITH 'Unit'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_UNIT_SCR9210  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9227  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9227 INPUT.

  CASE OK-CODE.
    WHEN 'EXIT' OR 'BACK'.     " EXIT or BACK
      IF W_STATUS EQ C_REQ_U OR W_STATUS EQ C_REQ_C.
        PERFORM P2000_SAVE_PROCESS.
      ELSE.
        CLEAR OK-CODE.
        CASE SY-TCODE.
          WHEN 'ZIMI1'.    LEAVE TO SCREEN 9200.
          WHEN 'ZIMO6'.    LEAVE TO SCREEN 9226.
        ENDCASE.
      ENDIF.
    WHEN 'NEWL'.               " NEW ENTRY
      W_OLD_STATUS = W_STATUS.
      MOVE C_REQ_C TO W_STATUS.
      PERFORM  P2000_IT_TAB_REFRESH.
    WHEN 'SAVE' OR 'ANZG'.     " SAVE or CHANGE=>DISPLAY
      PERFORM P2000_SAVE_PROCESS.
    WHEN 'DELT'.               " Delete
      PERFORM P2000_DATA_DELETE.
    WHEN 'DELE' OR 'DELC'.     " DELETE mark or unDELETE mark
      PERFORM P2000_SET_DEL_MARK.
    WHEN 'MKAL' OR 'MKLO'.     "  전체 선택 및 선택 해제?
      PERFORM P2000_SET_ROW_MARK.
    WHEN 'AEND'.         " DISPLAY ==> CHANGE
      MOVE C_REQ_U   TO  W_STATUS.
      PERFORM  P1000_DATA_REREAD.
    WHEN OTHERS.
  ENDCASE.





ENDMODULE.                 " USER_COMMAND_SCR9201  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA_SCR0003  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_DATA_SCR0003 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
      EXIT.
  ENDCASE.

*  IF ZTIV-ZFCIVNO    IS INITIAL.         " B/L NO를 입력하지 않을 경?
*        MESSAGE E213.
*  ENDIF.

ENDMODULE.                 " CHECK_DATA_SCR0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_DOC_SCR0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DOC_SCR0003 INPUT.

  IF RADIO_NP EQ 'X'.             " 무환 선택?
    EXIT.
  ENDIF.
  IF RADIO_NPO EQ 'X'.            " 무환 (대체,보상품...)
    EXIT.
  ENDIF.

  IF RADIO_PO EQ 'X'.                 " P/O No. 선택?
    IF ZSREQHD-EBELN IS INITIAL.     " By PO
      MESSAGE E733.
    ENDIF.
* P/O NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZVREQHD_ST
                      WHERE EBELN EQ ZSREQHD-EBELN
                      AND   ZFAMDNO EQ '00000'
                      AND   ZFREQTY NE 'LO'
                      AND   ZFREQTY NE 'PU'.
*                       AND   ZFDOCST EQ 'O'.
*                      OR    ZFDOCST EQ 'A' ).

    CASE W_COUNT.
      WHEN 0.     MESSAGE E064 WITH ZSREQHD-EBELN.
      WHEN 1.
        SELECT * UP TO 1 ROWS FROM ZVREQHD_ST
                         WHERE EBELN EQ ZSREQHD-EBELN
                          AND   ZFAMDNO EQ '00000'
                          AND   ZFREQTY NE 'LO'
                          AND   ZFREQTY NE 'PU'.
*                             AND   ZFDOCST EQ 'O'.
*                            OR    ZFDOCST EQ 'A' ).
          EXIT.
        ENDSELECT.
        W_ZFREQNO = ZVREQHD_ST-ZFREQNO.
        SET PARAMETER ID 'ZPREQNO' FIELD ZVREQHD_ST-ZFREQNO.
      WHEN OTHERS.
        PERFORM P2000_OPEN_DOC_SELECT1   USING     ZSREQHD-EBELN.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
        SET PARAMETER ID 'ZPREQNO' FIELD ZSREQHD-ZFREQNO.
        CLEAR : RADIO_PO, RADIO_RQ, RADIO_LC.
        MOVE 'X' TO RADIO_RQ.
    ENDCASE.
  ELSEIF RADIO_RQ EQ 'X'.             " 수입의뢰 관리번호 선택?
    IF ZSREQHD-ZFREQNO IS INITIAL.
      MESSAGE E737.
    ENDIF.
    W_ZFREQNO = ZSREQHD-ZFREQNO.
  ELSEIF RADIO_LC EQ 'X'.             " L/C No. 선택?
    IF ZSREQHD-ZFOPNNO IS INITIAL.   " By LC
      MESSAGE E736.
    ENDIF.
* 문서 승인번?
    PERFORM   P1000_GET_LC_DOC_NUM   USING   ZSREQHD-ZFOPNNO.
  ENDIF.
* 문서 조?
* PERFORM      P1000_GET_LC_DOCUMENT.
  IF W_ZFREQNO IS INITIAL.
    MESSAGE E320.
  ENDIF.
  CLEAR : ZVREQHD_ST.
* SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO FROM ZTREQST
*                       WHERE ZFREQNO EQ W_ZFREQNO.
*

  SELECT SINGLE * FROM ZVREQHD_ST
                  WHERE ZFREQNO EQ W_ZFREQNO
                  AND   ZFAMDNO EQ W_ZFAMDNO.
  IF SY-SUBRC NE 0.
    MESSAGE E018 WITH W_ZFREQNO.
  ENDIF.

  SET PARAMETER ID 'ZPREQNO' FIELD W_ZFREQNO.

  IF ZVREQHD_ST-ZFREQTY EQ 'LO' OR ZVREQHD_ST-ZFREQTY EQ 'PU'.
    MESSAGE E317 WITH ZVREQHD_ST-ZFREQNO ZVREQHD_ST-ZFREQTY.
  ENDIF.

* IF NOT ( ZVREQHD_ST-ZFDOCST EQ 'O' OR ZVREQHD_ST-ZFDOCST EQ 'A' ).
*    MESSAGE E318 WITH ZVREQHD_ST-ZFREQNO ZVREQHD_ST-ZFDOCST.
* ENDIF.

ENDMODULE.                 " READ_DOC_SCR0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0104 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSBLCST-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSBLCST-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSBLCST-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZSBLCST-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZSBLCST-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0104  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_ZTERM_NAME_SCR0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_ZTERM_NAME_SCR0103 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
*>>> 금액을 입력하지 않았을 경우...
  CHECK  NOT ZSBLCST-ZFCKAMT IS INITIAL.

  IF ZSBLCST-ZTERM IS INITIAL.
    EXIT.
  ENDIF.
  REFRESH : ZBTXT_LINES.

  CALL FUNCTION 'FI_PRINT_ZTERM'                            "80864
       EXPORTING
            I_ZTERM         = ZSBLCST-ZTERM
            I_LANGU         = SY-LANGU
           I_XT052U        = 'X'                            "187501
            I_T052          = T052
       TABLES
            T_ZTEXT         = ZBTXT_LINES
       EXCEPTIONS
            ZTERM_NOT_FOUND = 01.

  IF SY-SUBRC NE 0.
    MESSAGE E410(FH) WITH SY-MSGV1 RAISING ZTERM_NOT_FOUND.
  ENDIF.
ENDMODULE.                 " GET_ZTERM_NAME_SCR0103  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0107 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'RECL'.   " Recalcurate
      W_ZSBLCST_ZFCAMT2 = ZTBL-ZFTOWT * W_ZTBL_ZFTRTE.
      IF ZTIMIMG17-ZFMINPR GT W_ZSBLCST_ZFCAMT2.
        W_ZSBLCST_ZFCAMT2 = ZTIMIMG17-ZFMINPR.
      ENDIF.
      EXIT.
    WHEN 'IMGC'.   "Code Management CALL
      CALL TRANSACTION 'ZIMG17'.
      EXIT.
    WHEN 'ENTR'.
      IF W_STATUS EQ 'D'.
        ANTWORT = 'N'.
      ELSE.
        ANTWORT = 'Y'.
      ENDIF.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.


ENDMODULE.                 " GET_OK_CODE_SCR0107  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXRATE_CHANGE_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE EXRATE_CHANGE_CHECK_SCR0104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* 운임율통화 미입력?
  IF ZTBL-ZFTRTEC IS INITIAL.
    MESSAGE E167 WITH 'Freight rate currency'.
  ENDIF.
* 환율이 바뀔 경우..
  IF ZTBL-ZFTRTEC NE W_ZFTRTEC.

    PERFORM    P2000_GET_EXCHANGE_RATE     USING ZTBL-ZFTRTEC
                                                 ZTBL-ZFBLDT
                                        CHANGING ZTBL-ZFEXRT
                                                 ZTBL-FFACT.

    MOVE : ZTBL-ZFTRTEC TO W_ZFTRTEC,
           ZTBL-ZFEXRT  TO ZTBL-ZFEXRT.
  ENDIF.

  LOOP AT IT_ZSBLCST.
    IF NOT IT_ZSBLCST-WAERS IS INITIAL.
      CONTINUE.
    ENDIF.
    IF ZTBL-ZFVIA EQ 'AIR'.
      IF IT_ZSBLCST-ZFCSCD NE 'AHC' .
        MOVE : ZTBL-ZFTRTEC        TO    IT_ZSBLCST-WAERS.
      ENDIF.
    ELSEIF ZTBL-ZFVIA EQ 'VSL'.        " OCEAN
      CASE IT_ZSBLCST-ZFCSCD.
* 체선료 / W.F.G / T.H.C / C.F.S / Cont'R Tax / Doc. Fee
        WHEN 'DTC' OR 'WFG' OR 'THC' OR 'CFS' OR 'CTT' OR 'DCF'.
          MOVE : 'KRW'               TO    IT_ZSBLCST-WAERS.
        WHEN OTHERS.
          MOVE : ZTBL-ZFTRTEC        TO    IT_ZSBLCST-WAERS.
      ENDCASE.
    ENDIF.
    IT_ZSBLCST-WAERS = ZTBL-ZFTRTEC.
    MODIFY IT_ZSBLCST INDEX SY-TABIX.
  ENDLOOP.

ENDMODULE.                 " EXRATE_CHANGE_CHECK_SCR0104  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHG_WEIGHT_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE CHG_WEIGHT_SCR0104 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTBL-ZFTOWT IS INITIAL.
    MESSAGE E167 WITH 'Chargeable Weight'.
  ENDIF.

ENDMODULE.                 " CHG_WEIGHT_SCR0104  INPUT

*&---------------------------------------------------------------------*
*&      Module  INCOTERMS_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
MODULE INCOTERMS_CHECK_SCR0104 INPUT.
* Display MODE MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTBL-INCO1 IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'INCO1'.
  ELSE.
    PERFORM P2000_CHARGE_PAY_METHOD.
  ENDIF.

ENDMODULE.                 " INCOTERMS_CHECK_SCR0104  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR9222  INPUT
*&---------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR9222 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.         " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ELSE.
* B/L READ PERFORM ?
      ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
      PERFORM   P1000_READ_BL_DOC.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* B/L NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
* B/L READ PERFORM ?
    ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
    PERFORM   P1000_READ_BL_DOC.
  ENDIF.
*-----------------------------------------------------------------------
* 반입정보 조?
*-----------------------------------------------------------------------
  IF ZSREQHD-ZFBTSEQ IS INITIAL.
    SELECT COUNT( * ) INTO W_COUNT FROM ZTBLOUR
                      WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
    CASE W_COUNT.
      WHEN 0.   MESSAGE E181 WITH  ZTBL-ZFBLNO.
      WHEN 1.
        SELECT ZFBTSEQ INTO ZSREQHD-ZFBTSEQ
                       FROM ZTBLOUR  UP TO 1 ROWS
                       WHERE ZFBLNO  EQ ZTBL-ZFBLNO.
        ENDSELECT.
      WHEN OTHERS.
        REFRESH IT_ZTBLOUR.
* Table Multi-Select
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLOUR
                 FROM   ZTBLOUR
                 WHERE  ZFBLNO EQ ZTBL-ZFBLNO
                 ORDER  BY ZFBTSEQ.

        DESCRIBE TABLE IT_ZTBLOUR  LINES TFILL.
        IF TFILL = 0.
          MESSAGE E406.
        ENDIF.

        W_STATUS_CHK = 'C'.
        INCLUDE = 'ZTBLOURD'.                 " 반입 정?
        CALL SCREEN 0014 STARTING AT  13 3
                         ENDING   AT  75 15.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
    ENDCASE.
  ENDIF.

  SELECT SINGLE * FROM ZTBLOUR   WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                 AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
  IF SY-SUBRC NE 0.
    MESSAGE E045 WITH ZTBL-ZFBLNO ZSREQHD-ZFBTSEQ.
  ELSE.
     *ZTBLOUR  =  ZTBLOUR.
  ENDIF.
*-----------------------------------------------------------------------
* 업무별로 문서 상태검증함.
*-----------------------------------------------------------------------
  CASE W_STATUS.
    WHEN C_REQ_U.        " 변?
      PERFORM P2000_OUR_CHANGE_CHECK.
    WHEN C_REQ_D.        " 조회....
      PERFORM P2000_OUR_DISPLAY_CHECK.
    WHEN C_OPEN_C.       " OPEN
      PERFORM P2000_OUR_OPEN_CHECK.
    WHEN OTHERS.
  ENDCASE.
* 반입예정정?
  SELECT SINGLE * FROM ZTBLINOU  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                                 AND   ZFBTSEQ EQ ZSREQHD-ZFBTSEQ.
* 도착?
  PERFORM   P1000_GET_BONDED_NAME1   USING      ZTBLINR-ZFBNARCD
                                     CHANGING   W_ARR_NM.
* BENIFICIARY
  PERFORM  P1000_GET_VENDOR   USING      ZTBL-LIFNR
                              CHANGING   W_LFA1-NAME1.
  MOVE : W_LFA1-NAME2     TO     W_LFA1-NAME1.

  IF W_STATUS NE C_REQ_D.
    PERFORM    P2000_SET_ZTBLOUR_LOCK_MODE USING 'L'.
  ENDIF.

ENDMODULE.                 " READ_BL_DOC_SCR9222  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_ZTIDR_ZTIDS_SCR9226  INPUT
*&---------------------------------------------------------------------*
MODULE READ_ZTIDR_ZTIDS_SCR9226 INPUT.

  CLEAR : ZTIDR, ZTIDS.
  SELECT MAX( ZFCLSEQ ) INTO ZTIDR-ZFCLSEQ FROM ZTIDR
                WHERE ZFBLNO  EQ ZTBL-ZFBLNO.

  IF ZTIDR-ZFCLSEQ IS INITIAL.
    MESSAGE W753.   " WITH ZTBL-ZFBLNO.
  ELSE.
    SELECT SINGLE * FROM ZTIDR
                  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                  AND   ZFCLSEQ EQ ZTIDR-ZFCLSEQ.
    SELECT SINGLE * FROM ZTIDS
                  WHERE ZFBLNO  EQ ZTBL-ZFBLNO
                  AND   ZFCLSEQ EQ ZTIDR-ZFCLSEQ.
    IF SY-SUBRC NE 0.
      MESSAGE W782.   " WITH ZTBL-ZFBLNO.
    ENDIF.
  ENDIF.

ENDMODULE.                 " READ_ZTIDR_ZTIDS_SCR9226  INPUT

*&---------------------------------------------------------------------*
*&      Module  MWSKZ_UPDATE_SCR0103  INPUT
*&---------------------------------------------------------------------*
MODULE MWSKZ_UPDATE_SCR0103 INPUT.

  PERFORM   P1000_READ_KONP   USING   IT_ZSBLCST-MWSKZ
                                      IT_ZSBLCST-KBETR
                                      IT_ZSBLCST-KONWA.

ENDMODULE.                 " MWSKZ_UPDATE_SCR0103  INPUT

*&---------------------------------------------------------------------*
*&      Module  MWSKZ_UPDATE_SCR0106  INPUT
*&---------------------------------------------------------------------*
MODULE MWSKZ_UPDATE_SCR0106 INPUT.
  PERFORM   P1000_READ_KONP   USING   IT_ZSBLCST1-MWSKZ
                                      IT_ZSBLCST1-KBETR
                                      IT_ZSBLCST1-KONWA.

ENDMODULE.                 " MWSKZ_UPDATE_SCR0106  INPUT

*&---------------------------------------------------------------------*
*&      Module  MAT_DOC_HELP_SCR6700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MAT_DOC_HELP_SCR6700 INPUT.
  DATA: FLAG(1).
  FLAG  = 'X'.
  EXPORT FLAG TO MEMORY ID 'MB51_FLAG'.
  CALL TRANSACTION 'MB51'.
  GET PARAMETER ID 'MBN' FIELD MSEG-MBLNR.
  GET PARAMETER ID 'MJA' FIELD MSEG-MJAHR.

ENDMODULE.                 " MAT_DOC_HELP_SCR6700  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_CURRENCY_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_BL_CURRENCY_CHECK_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTBL-ZFBLAMC IS INITIAL.
    MESSAGE E167 WITH 'B/L Currecy'.
  ENDIF.

  ZSREQHD-ZFBLAMC = ZTBL-ZFBLAMC.
  IF ZTBL-ZFTRTEC IS INITIAL.
    ZTBL-ZFTRTEC = ZTBL-ZFBLAMC.
  ENDIF.

  LOOP AT IT_ZSBLCST WHERE WAERS EQ SPACE.
    IT_ZSBLCST-WAERS = ZTBL-ZFTRTEC.
    MODIFY IT_ZSBLCST INDEX SY-TABIX.
  ENDLOOP.

  LOOP AT IT_ZSBLCST1 WHERE WAERS EQ SPACE.
    IT_ZSBLCST1-WAERS = 'USD'.
    MODIFY IT_ZSBLCST1 INDEX SY-TABIX.
  ENDLOOP.

ENDMODULE.                 " GET_BL_CURRENCY_CHECK_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0110 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0110-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0110_MARK_TC_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0110_MARK_TC_0110 INPUT.

*   READ TABLE IT_ZSBLIT  WITH KEY
*                         ZFBLIT = ZSBLIT-ZFBLIT  BINARY SEARCH.
  READ TABLE IT_ZSBLIT   INDEX TC_0110-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0110_MARK_TC_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_UPDATE_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBLIT_UPDATE_SCR0110 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK ZSBLIT-EBELN IS INITIAL.

* Internal Table Read
*  READ TABLE IT_ZSBLIT   WITH KEY ZFBLIT = ZSBLIT-ZFBLIT.
  READ TABLE IT_ZSBLIT   INDEX TC_0110-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.

*>> 수입의뢰 번호가 입력되지 않았을 경우.
  IF ZSBLIT-ZFREQNO IS INITIAL.
    MESSAGE W019.   EXIT.
  ELSE.
*>> 수입의뢰 번호가 입력되었을 경우.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO.
    IF SY-SUBRC NE 0.
      CLEAR : ZSBLIT.
      MESSAGE W018 WITH IT_ZSBLIT-ZFREQNO.
      EXIT.
    ENDIF.
*>> 삼국무역...
    IF ZTREQHD-ZFTRIPLE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSBLIT.  EXIT.
    ENDIF.
*>> 사후관리여부...
    IF ZTREQHD-ZFCLOSE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSBLIT.  EXIT.
    ENDIF.

    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      MESSAGE W317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      CLEAR : ZSBLIT.  EXIT.
    ENDIF.

*>> 통화단위 검증..
    IF ZTBL-ZFBLAMC IS INITIAL.
      ZTBL-ZFBLAMC = ZTREQHD-WAERS.
    ENDIF.
    IF ZTBL-ZFBLAMC NE ZTREQHD-WAERS.
      MESSAGE E379 WITH ZTBL-ZFBLAMC IT_ZSBLIT-ZFREQNO ZTREQHD-WAERS.
    ENDIF.
*>> Vendor 검증.
    IF ZTBL-LIFNR IS INITIAL.
      ZTBL-LIFNR  =  ZTREQHD-LIFNR.
    ENDIF.
    IF ZTBL-LIFNR NE ZTREQHD-LIFNR.
      MESSAGE E380 WITH ZTBL-LIFNR IT_ZSBLIT-ZFREQNO ZTREQHD-LIFNR.
    ENDIF.
*>> Beneficiay 검증.
    IF ZTBL-ZFBENI IS INITIAL.
      ZTBL-ZFBENI  =  ZTREQHD-ZFBENI.
    ENDIF.
    IF ZTBL-ZFBENI NE ZTREQHD-ZFBENI.
      MESSAGE E381 WITH ZTBL-ZFBENI IT_ZSBLIT-ZFREQNO ZTREQHD-ZFBENI.
    ENDIF.
*>> 회사코드 검증.
    IF ZTBL-BUKRS IS INITIAL.
      ZTBL-BUKRS  =  ZTREQHD-BUKRS.
    ENDIF.
    IF ZTBL-BUKRS NE ZTREQHD-BUKRS.
      MESSAGE E382 WITH ZTBL-BUKRS IT_ZSBLIT-ZFREQNO ZTREQHD-BUKRS.
    ENDIF.
*>> 구매그룹/구매조직 검증.
    SELECT SINGLE * FROM ZTREQST
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                    AND   ZFAMDNO EQ '00000'.
    IF ZTBL-EKORG IS INITIAL.
      ZTBL-EKORG  =  ZTREQST-EKORG.
    ENDIF.
    IF ZTBL-EKGRP IS INITIAL.
      ZTBL-EKGRP  =  ZTREQST-EKGRP.
    ENDIF.
*>> 수입의뢰 ITEM번호가 입력되었을 경우.
    IF NOT IT_ZSBLIT-ZFITMNO IS INITIAL.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSBLIT WITH KEY ZFREQNO = ZSBLIT-ZFREQNO
                                     ZFITMNO = ZSBLIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSBLIT.
        MESSAGE S358 WITH ZSBLIT-ZFREQNO ZSBLIT-ZFITMNO
                          IT_ZSBLIT-ZFBLIT.
        EXIT.
      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.
*----> 수입의뢰 ITEM번호가 입력되었을 경우, 발췌하여 메세지.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF IT_ZSBLIT
             FROM   ZTREQIT
             WHERE ZFREQNO EQ ZSBLIT-ZFREQNO
             AND   ZFITMNO EQ ZSBLIT-ZFITMNO.
      IF SY-SUBRC NE 0.
        MESSAGE W357 WITH ZSBLIT-ZFREQNO ZSBLIT-ZFITMNO.
        CLEAR : ZSBLIT.
        EXIT.
      ENDIF.
*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ WERKS LGORT MATKL
             INTO (IT_ZSBLIT-MENGE_PO, IT_ZSBLIT-UEBTO,
                   IT_ZSBLIT-UEBTK,    IT_ZSBLIT-WEPOS,
                   IT_ZSBLIT-ELIKZ,    IT_ZSBLIT-LOEKZ,
                   IT_ZSBLIT-UNTTO,
                   IT_ZSBLIT-BPUMN,    IT_ZSBLIT-BPUMZ,
                   IT_ZSBLIT-WERKS,    IT_ZSBLIT-LGORT,
                   IT_ZSBLIT-MATKL)
             FROM   EKPO
             WHERE  EBELN   EQ   ZTREQHD-EBELN
             AND    EBELP   EQ   ZSBLIT-ZFITMNO.
      IF SY-SUBRC EQ 0.
        IF IT_ZSBLIT-LOEKZ NE SPACE.
          MESSAGE W069 WITH ZTREQHD-EBELN ZSBLIT-ZFITMNO.
          CLEAR : ZSBLIT.
          EXIT.
        ENDIF.
        IF IT_ZSBLIT-ELIKZ EQ 'X'.
          MESSAGE W359 WITH ZTREQHD-EBELN ZSBLIT-ZFITMNO.
          CLEAR : ZSBLIT.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE W071 WITH ZTREQHD-EBELN ZSBLIT-ZFITMNO.
        CLEAR : ZSBLIT.
        EXIT.
      ENDIF.

*----> B/L 자재내역(기입력건)
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
               WHERE ZFBLNO  NE ZTBL-ZFBLNO
               AND   ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ELSE.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ENDIF.

*>>> B/L 기본 수?
      IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT.
      IT_ZSBLIT-EBELN = ZTREQHD-EBELN.
      IT_ZSBLIT-EBELP = IT_ZSBLIT-ZFITMNO.
      IF IT_ZSBLIT-BLMENGE LT 0.
        IT_ZSBLIT-BLMENGE = 0.
      ENDIF.
*----> 기존 데이타 갱신.
      IF W_SY_SUBRC EQ 0.
        IT_ZSBLIT-EBELN = ZTREQHD-EBELN.
        MODIFY IT_ZSBLIT  INDEX W_TABIX.
      ELSE.
*----> 신규 데이타 추가.
*           IT_ZSBLIT-ZFBLIT = TC_0110-CURRENT_LINE * 10.
        IT_ZSBLIT-EBELN = ZTREQHD-EBELN.
        MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
               SY-DATUM    TO   IT_ZSBLIT-CDAT,
               SY-UNAME    TO   IT_ZSBLIT-UNAM,
               SY-DATUM    TO   IT_ZSBLIT-UDAT.

        APPEND  IT_ZSBLIT.
      ENDIF.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.
*---> 기존 데이타가 있었을 경우.
    IF W_SY_SUBRC EQ 0.
      DELETE IT_ZSBLIT  INDEX W_TABIX.
    ELSE.
*        IT_ZSBLIT-ZFBLIT = TC_0110-CURRENT_LINE * 10.
    ENDIF.

*>>수입의뢰 HEADER SELECT.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ ZSBLIT-ZFREQNO.

    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.

*>> 통화단위 검증..
    IF ZTBL-ZFBLAMC IS INITIAL.
      ZTBL-ZFBLAMC = ZTREQHD-WAERS.
    ENDIF.
    IF ZTBL-ZFBLAMC NE ZTREQHD-WAERS.
      MESSAGE E379 WITH ZTBL-ZFBLAMC IT_ZSBLIT-ZFREQNO ZTREQHD-WAERS.
    ENDIF.
*>> Vendor 검증.
    IF ZTBL-LIFNR IS INITIAL.
      ZTBL-LIFNR  =  ZTREQHD-LIFNR.
    ENDIF.
    IF ZTBL-LIFNR NE ZTREQHD-LIFNR.
      MESSAGE E380 WITH ZTBL-LIFNR IT_ZSBLIT-ZFREQNO ZTREQHD-LIFNR.
    ENDIF.
*>> Beneficiay 검증.
    IF ZTBL-ZFBENI IS INITIAL.
      ZTBL-ZFBENI  =  ZTREQHD-ZFBENI.
    ENDIF.
    IF ZTBL-ZFBENI NE ZTREQHD-ZFBENI.
      MESSAGE E381 WITH ZTBL-ZFBENI IT_ZSBLIT-ZFREQNO ZTREQHD-ZFBENI.
    ENDIF.
*>> 회사코드 검증.
    IF ZTBL-BUKRS IS INITIAL.
      ZTBL-BUKRS  =  ZTREQHD-BUKRS.
    ENDIF.
    IF ZTBL-BUKRS NE ZTREQHD-BUKRS.
      MESSAGE E382 WITH ZTBL-BUKRS IT_ZSBLIT-ZFREQNO ZTREQHD-BUKRS.
    ENDIF.
*>> 구매그룹/구매조직 검증.
    SELECT SINGLE * FROM ZTREQST
                    WHERE ZFREQNO EQ ZTREQHD-ZFREQNO
                    AND   ZFAMDNO EQ '00000'.
    IF ZTBL-EKORG IS INITIAL.
      ZTBL-EKORG  =  ZTREQST-EKORG.
    ENDIF.
    IF ZTBL-EKGRP IS INITIAL.
      ZTBL-EKGRP  =  ZTREQST-EKGRP.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBLIT_TMP
             FROM ZTREQIT
             WHERE ZFREQNO EQ ZSBLIT-ZFREQNO.

    LOOP AT IT_ZSBLIT_TMP.
      MOVE-CORRESPONDING  IT_ZSBLIT_TMP   TO  IT_ZSBLIT.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSBLIT WITH KEY ZFREQNO = IT_ZSBLIT-ZFREQNO
                                     ZFITMNO = IT_ZSBLIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSBLIT.
        MESSAGE S358 WITH IT_ZSBLIT-ZFREQNO IT_ZSBLIT-ZFITMNO
                          IT_ZSBLIT-ZFBLIT.
        CONTINUE.
      ENDIF.
*-----------------------------------------------------------------------
*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMZ BPUMN WERKS LGORT MATKL
             INTO (IT_ZSBLIT-MENGE_PO, IT_ZSBLIT-UEBTO,
                   IT_ZSBLIT-UEBTK,    IT_ZSBLIT-WEPOS,
                   IT_ZSBLIT-ELIKZ,    IT_ZSBLIT-LOEKZ,
                   IT_ZSBLIT-UNTTO,
                   IT_ZSBLIT-BPUMZ,    IT_ZSBLIT-BPUMN,
                   IT_ZSBLIT-WERKS,    IT_ZSBLIT-LGORT,
                   IT_ZSBLIT-MATKL)
             FROM   EKPO
             WHERE  EBELN   EQ   ZTREQHD-EBELN
             AND    EBELP   EQ   IT_ZSBLIT-ZFITMNO.

      IF SY-SUBRC EQ 0.
        IF IT_ZSBLIT-LOEKZ NE SPACE.
          CLEAR : ZSBLIT.
          MESSAGE W069 WITH ZTREQHD-EBELN IT_ZSBLIT-ZFITMNO.
          CONTINUE.
        ENDIF.
        IF IT_ZSBLIT-ELIKZ EQ 'X'.
          CLEAR : ZSBLIT.
          MESSAGE W359 WITH ZTREQHD-EBELN IT_ZSBLIT-ZFITMNO.
          CONTINUE.
        ENDIF.
      ELSE.
        CLEAR : ZSBLIT.
        MESSAGE W071 WITH ZTREQHD-EBELN IT_ZSBLIT-ZFITMNO.
        CONTINUE.
      ENDIF.
*----> B/L 자재내역(기입력건)
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
               WHERE ZFBLNO  NE ZTBL-ZFBLNO
               AND   ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ELSE.
        SELECT SUM( BLMENGE ) INTO IT_ZSBLIT-ZFBLTOT
               FROM  ZTBLIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO
               AND   BLOEKZ  NE 'X'.
      ENDIF.

*>>> B/L 기본 수?
      IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE - IT_ZSBLIT-ZFBLTOT.
      IT_ZSBLIT-EBELN = ZTREQHD-EBELN.
      IT_ZSBLIT-EBELP = IT_ZSBLIT-ZFITMNO.

      IF IT_ZSBLIT-BLMENGE LT 0.
        IT_ZSBLIT-BLMENGE = 0.
      ENDIF.

*----> 변경....
      IF W_SY_SUBRC EQ 0.
        MODIFY IT_ZSBLIT  INDEX W_TABIX.
      ELSE.
*           IT_ZSBLIT-ZFBLIT = W_TABIX * 10.
*           INSERT IT_ZSBLIT  INDEX W_TABIX.
*           ADD 1 TO W_TABIX.
*        ELSE.
*----> 마지막에 추가.
        MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
               SY-DATUM    TO   IT_ZSBLIT-CDAT,
               SY-UNAME    TO   IT_ZSBLIT-UNAM,
               SY-DATUM    TO   IT_ZSBLIT-UDAT.

        APPEND IT_ZSBLIT.
*           IT_ZSBLIT-ZFBLIT = IT_ZSBLIT-ZFBLIT + 10.
      ENDIF.
*     ENDSELECT.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " IT_ZSBLIT_UPDATE_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0110 INPUT.

  W_OK_CODE = SY-UCOMM.

  IF SY-UCOMM EQ 'MKA1'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' .
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'UND1'.
      LOOP AT IT_ZSBLIT WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        MOVE : SPACE    TO   IT_ZSBLIT-BLOEKZ.
        MODIFY IT_ZSBLIT INDEX W_TABIX.
      ENDLOOP.
    WHEN 'DEL1'.
      LOOP AT IT_ZSBLIT WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        IF IT_ZSBLIT-ZFBLIT IS INITIAL.
          DELETE IT_ZSBLIT INDEX W_TABIX.
          CONTINUE.
        ENDIF.

        CLEAR : W_COUNT.
        SELECT COUNT( * ) INTO W_COUNT
               FROM   ZTCIVIT
               WHERE  ZFBLNO   EQ   IT_ZSBLIT-ZFBLNO
               AND    ZFBLIT   EQ   IT_ZSBLIT-ZFBLIT.

        IF W_COUNT EQ 0.
          MOVE : 'X'          TO   IT_ZSBLIT-BLOEKZ.
        ENDIF.
        MODIFY IT_ZSBLIT  INDEX  W_TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSBLIT.
        IT_ZSBLIT-ZFMARK = W_MARK.   MODIFY IT_ZSBLIT INDEX SY-TABIX.
      ENDLOOP.
      EXIT.
    WHEN 'PGUP'.
      TC_0112-TOP_LINE = TC_0112-TOP_LINE - LOOPLINES.
      IF TC_0112-TOP_LINE < 1.
        TC_0112-TOP_LINE = 1.
      ENDIF.
      EXIT.
    WHEN 'PGDN'.
      TC_0112-TOP_LINE = TC_0112-TOP_LINE + LOOPLINES.
      EXIT.
    WHEN 'SU01' OR 'PODP' OR 'IMDP' OR 'VL31'.
      PERFORM   P2000_LINE_SELECT_BL_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_BL_ITEM.
      EXIT.
    WHEN 'PORE'.
      PERFORM   P1000_GET_IMPORT_DATA_REFRESH.
    WHEN 'APOD_T'.
      CLEAR   IT_CONTLST.
      REFRESH IT_CONTLST.
      LOOP AT IT_ZSBLIT WHERE DSC NE SPACE.
        PERFORM P1000_GET_CONTAINER_DATA.
      ENDLOOP.
      EXIT.
    WHEN 'CALC'.
      PERFORM P1000_GET_BL_AMOUNT.
    WHEN OTHERS.
  ENDCASE.

  IF SY-BINPT NE 'X'.
    PERFORM P1000_GET_BL_AMOUNT.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0112.
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0112 INPUT.
  CASE OK-CODE.
    WHEN 'APOD_T'.
      CLEAR   IT_CONTLST.
      REFRESH IT_CONTLST.
      SELECT *
        FROM LIKP
       WHERE BOLNR = ZTBL-ZFHBLNO.

        IF SY-SUBRC EQ 0.
          SELECT *
            FROM LIPS
           WHERE VBELN = LIKP-VBELN
             AND VGBEL = IT_ZSBLIT-EBELN
             AND VGPOS = IT_ZSBLIT-EBELP.
            ADD LIPS-LFIMG TO IT_CONTLST-LFIMG.
          ENDSELECT.
          IF SY-SUBRC EQ 0.
            MOVE ZTBL-ZFBLNO      TO IT_CONTLST-ZFBLNO.
            MOVE IT_ZSBLIT-ZFBLIT TO IT_CONTLST-ZFBLIT.
            MOVE IT_ZSBLIT-EBELN  TO IT_CONTLST-EBELN.
            MOVE IT_ZSBLIT-EBELP  TO IT_CONTLST-EBELP.
            MOVE IT_ZSBLIT-MATNR  TO IT_CONTLST-MATNR.
            MOVE IT_ZSBLIT-TXZ01  TO IT_CONTLST-TXZ01.
            MOVE LIPS-VRKME       TO IT_CONTLST-VRKME.
            MOVE LIKP-TRAID       TO IT_CONTLST-TRAID.
            MOVE LIPS-KDMAT       TO IT_CONTLST-KDMAT.
            MOVE LIPS-VBELN       TO IT_CONTLST-VBELN.
            APPEND IT_CONTLST.
            CLEAR IT_CONTLST-LFIMG.
          ENDIF.
        ENDIF.
      ENDSELECT.
      DESCRIBE  TABLE IT_CONTLST  LINES  W_LINE.
      IF W_LINE GT 0.
        INCLUDE = 'CONTDISP'.
        CALL SCREEN 0015 STARTING AT  10   3
                         ENDING   AT  135 12.
        CLEAR : INCLUDE.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_SCR0112 INPUT.
*&---------------------------------------------------------------------*
*&      Module  TC_9911_UPDATE_SCR9911  INPUT
*&---------------------------------------------------------------------*
MODULE TC_9911_UPDATE_SCR9911 INPUT.

*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSMSHD INDEX TC_9911-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.
  CLEAR W_DEL_MARK.

  MOVE-CORRESPONDING ZSMSHD TO IT_ZSMSHD.
  MOVE : W_ROW_MARK         TO IT_ZSMSHD-ZFMARK.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSMSHD   INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " TC_9911_UPDATE_SCR9911  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_MENGE_CHECK_SCR0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSBLIT_MENGE_CHECK_SCR0110 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSBLIT-EBELN IS INITIAL.  " P/O 가 입력되지 않았을 경우.

*  READ TABLE IT_ZSBLIT   WITH KEY ZFBLIT = ZSBLIT-ZFBLIT.
  READ TABLE IT_ZSBLIT   INDEX TC_0110-CURRENT_LINE.
  W_TABIX = SY-TABIX.

*미달납품 허용치 (%).
  IF NOT IT_ZSBLIT-UNTTO IS INITIAL.
    W_MENGE1 = ( ( ZSBLIT-MENGE * ZSBLIT-UEBTO ) / 100 ). " 미달납품.
    W_MENGE  = ZSBLIT-BLMENGE - W_MENGE1.   " B/L 수량 - 미달납품허용.
    W_MENGE2 = ZSBLIT-MENGE - W_MENGE1.     " 수입수량 - 미달납품허용.

    CLEAR : W_OLD_MENGE.
    SELECT SINGLE BLMENGE INTO W_OLD_MENGE
           FROM   ZTBLIT
           WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
           AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
           AND    BLOEKZ   NE   'X'.
*>> 수입의뢰 - OPEN 수량 + 저장된 B/L수량(현재) - 입력B/L수량.
    W_OLD_MENGE = W_MENGE2 - ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).

    IF ZSBLIT-BLMENGE LT W_OLD_MENGE.
      MESSAGE W365 WITH ZSBLIT-ZFBLIT.
    ENDIF.
  ENDIF.

  IF ZSBLIT-UEBTK EQ 'X'.          " 지시자: 허용된 무제한초과납품.
    MOVE ZSBLIT-BLMENGE  TO     IT_ZSBLIT-BLMENGE.
    MODIFY  IT_ZSBLIT  INDEX W_TABIX.
    EXIT.
  ENDIF.

*초과납품 허용치 (%).
  IF ZSBLIT-UEBTO IS INITIAL.      " 초과납품 허용치 (%)가 미입력시.
    CLEAR : W_OLD_MENGE.
    SELECT SINGLE BLMENGE INTO W_OLD_MENGE
           FROM   ZTBLIT
           WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
           AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
           AND    BLOEKZ   NE   'X'.
*>> 수입의뢰 - OPEN 수량 + 저장된 B/L수량(현재) - 입력B/L수량.
    W_OLD_MENGE = IT_ZSBLIT-MENGE -
                ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).
    IF  ZSBLIT-BLMENGE GT W_OLD_MENGE.
      MESSAGE E360 WITH ZSBLIT-ZFBLIT.
    ENDIF.
  ELSE.                            " 초과납품 허용치 (%)가 입력시.
    W_MENGE1 = ( ( ZSBLIT-MENGE * ZSBLIT-UEBTO ) / 100 ).
    W_MENGE  = W_MENGE1 + ZSBLIT-BLMENGE.   " B/L수량  + 초과납품허용.
    W_MENGE2 = W_MENGE1 + ZSBLIT-MENGE.     " 수입수량 + 초과납품허용.

    CLEAR : W_OLD_MENGE.
    SELECT SINGLE BLMENGE INTO W_OLD_MENGE
           FROM   ZTBLIT
           WHERE  ZFBLNO   EQ   ZSBLIT-ZFBLNO
           AND    ZFBLIT   EQ   ZSBLIT-ZFBLIT
           AND    BLOEKZ   NE   'X'.

    W_OLD_MENGE = W_MENGE2 - ( IT_ZSBLIT-ZFBLTOT - W_OLD_MENGE ).
    IF ZSBLIT-BLMENGE GT W_OLD_MENGE.
      MESSAGE E360 WITH ZSBLIT-ZFBLIT.
    ENDIF.
  ENDIF.

  MOVE ZSBLIT-BLMENGE  TO     IT_ZSBLIT-BLMENGE.
  MODIFY  IT_ZSBLIT  INDEX W_TABIX.

ENDMODULE.                 " IT_ZSBLIT_MENGE_CHECK_SCR0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_COMMERCIAL_IV_SCR3500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_COMMERCIAL_IV_SCR3500 INPUT.
*-----------------------------------------------------------------------
* OK-CODE
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  CLEAR : W_COUNT.
  IF SY-UCOMM NE 'LOPU'.
    IF ZSCIVHD-ZFCIVNO IS INITIAL.
      IF ZSCIVHD-ZFPRPYN EQ 'N'.
        MESSAGE W213.
      ENDIF.
    ELSE.
*>> Commercial Invoice No. Check.
      SELECT COUNT( * ) INTO W_COUNT
             FROM ZTCIVHD
             WHERE ZFCIVNO   EQ   ZSCIVHD-ZFCIVNO.
      IF W_COUNT GT 0.
        MESSAGE W740 WITH ZSCIVHD-ZFCIVNO.
      ENDIF.
    ENDIF.

*>> Monetary/Nonmonetary Check
    IF ZSCIVHD-ZFPOYN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCIVHD' 'ZFPOYN'.
    ENDIF.
*>> Prepaid Yes/No
    IF ZSCIVHD-ZFPRPYN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCIVHD' 'ZFPRPYN'.
    ENDIF.
  ENDIF.

  IF OK-CODE EQ 'LOPU'.
    ZSCIVHD-ZFPOYN = 'Y'.
    ZSCIVHD-ZFPRPYN = 'N'.
  ENDIF.

  W_OK_CODE = OK-CODE.
  REFRESH : IT_ZSCIVIT, IT_ZSCIVIT_ORG.
  CLEAR : IT_ZSCIVIT, IT_ZSCIVIT_ORG, ZTCIVIT, ZTCIVHD, *ZTCIVHD.

  IF OK-CODE EQ 'LOPU'.
    PERFORM  P2000_REF_REQ_DOC_NO_INPUT.
  ELSE.
    IF ZSCIVHD-ZFPRPYN EQ 'Y'.
      PERFORM  P2000_REF_REQ_DOC_NO_INPUT.
    ELSE.
      PERFORM  P2000_REF_BL_DOC_NO_INPUT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_COMMERCIAL_IV_SCR3500  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3500  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3500 INPUT.

  IF ANTWORT NE 'Y'.  EXIT.   ENDIF.
  IF SY-TCODE EQ 'ZIM35'.
    MOVE : ZSCIVHD-ZFCIVNO     TO     ZTCIVHD-ZFCIVNO,
           ZSCIVHD-ZFTRIPLE    TO     ZTCIVHD-ZFTRIPLE,
           ZSCIVHD-ZFPOYN      TO     ZTCIVHD-ZFPOYN,
*             'Y'                 TO     ZTCIVHD-ZFPOYN,
           ZSCIVHD-ZFPRPYN     TO     ZTCIVHD-ZFPRPYN.
  ELSE.
    CLEAR: ZTCIVHD-ZFCIVNO, ZTCIVHD-ZFTRIPLE.
    MOVE : 'Y'                 TO     ZTCIVHD-ZFPOYN,
           ZSCIVHD-ZFPRPYN     TO     ZTCIVHD-ZFPRPYN.
  ENDIF.

*          'N'                 TO     ZTCIVHD-ZFIVST,
  MOVE:  SY-UNAME            TO     ZTCIVHD-ERNAM,
         SY-DATUM            TO     ZTCIVHD-CDAT,
         SY-UNAME            TO     ZTCIVHD-UNAM,
         SY-DATUM            TO     ZTCIVHD-UDAT.

  IF ZTCIVHD-ZFPOYN EQ 'N'.
    ZTCIVHD-ZFIVST = 'X'.
  ELSE.
    ZTCIVHD-ZFIVST = 'N'.
  ENDIF.

  IF NOT ZTCIVHD-ZFMAVN IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTCIVHD-ZFMAVN
                                           CHANGING  W_LIFNR_NM.
  ENDIF.

  IF NOT ZTCIVHD-ZFOPBN IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ZTCIVHD-ZFOPBN
                                           CHANGING  W_ZFOPBN_NM.
  ENDIF.

  IF NOT ZTCIVHD-BUKRS IS INITIAL.
    SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTCIVHD-BUKRS.
    MOVE  T001-WAERS  TO   ZTCIVHD-ZFKRW.
  ENDIF.

  PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.

  CLEAR : *ZTCIVHD.
  REFRESH : IT_ZSCIVIT_ORG.

  MOVE C_REQ_C TO W_STATUS.
  SET SCREEN 3510.  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_SCR3500  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR3512  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR3512 INPUT.
* CLEAR : ZSCIVHD-ZFBLNO.

  CASE OK-CODE.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    ZSCIVHD-ZFBLNO   = ZSREQHD-ZFBLNO.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
*    CLEAR : ZSCIVHD-ZFBLNO.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR3512  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_DOCUMENT_SCR3512  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_BL_DOCUMENT_SCR3512 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.   EXIT.
    WHEN OTHERS.
  ENDCASE.

  IF ZSREQHD-ZFBLNO IS INITIAL.
    IF ZSREQHD-ZFHBLNO IS INITIAL.
      MESSAGE E304.
    ELSE.
      ">> House B/L No Count.
      SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZTBL
                       WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
  ELSE.
    IF NOT ZSCIVHD-ZFHBLNO IS INITIAL.
* B/L NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                       FROM  ZTBL
                       WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.
        WHEN 1.
          SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM ZTBL
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          PERFORM P2000_SEARCH_FIELD_MOVE.
      ENDCASE.
    ENDIF.
  ENDIF.
* B/L READ PERFORM ?
  ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
  PERFORM   P1000_READ_BL_DOC.

  REFRESH : IT_ZFREQNO, IT_LOCKED, IT_ZSCIVIT.
  PERFORM  P2000_BL_DATA_MOVE.            " B/L DATA MOVE.

ENDMODULE.                 " GET_BL_DOCUMENT_SCR3512  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3511  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3511 INPUT.

  W_OK_CODE = SY-UCOMM.

  IF SY-UCOMM EQ 'MKA1'.   " 전체 선택.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1'.  " 선택 해제.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'INST'.       " 삽입.
      CHECK LINE GT 0.
      CLEAR : IT_ZSCIVIT.
      IT_ZSCIVIT-ZFCIVSQ =  LINE * 10.
      INSERT IT_ZSCIVIT INDEX  LINE.
      PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'DEL1'.       " 삭제.
*>> 후속작업 체크...
      DELETE IT_ZSCIVIT  WHERE ZFMARK NE SPACE.
      LOOP AT IT_LOCKED.
        W_TABIX = SY-TABIX.
        IF ZTCIVHD-ZFPRPYN EQ 'Y'.           ">선급금.
          READ TABLE  IT_ZSCIVIT WITH KEY ZFREQNO = IT_LOCKED-ZFREQNO.
        ELSEIF ZTCIVHD-ZFPRPYN EQ 'N'.       ">일반.
          READ TABLE  IT_ZSCIVIT WITH KEY ZFBLNO = IT_LOCKED-ZFBLNO.
        ENDIF.
        IF SY-SUBRC NE 0.
          IF ZTCIVHD-ZFPRPYN EQ 'Y'.           ">선급금.
            CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
                 EXPORTING
                      ZFREQNO = IT_LOCKED-ZFREQNO.
            DELETE IT_LOCKED INDEX W_TABIX.
            DELETE IT_ZFREQNO WHERE ZFREQNO EQ IT_LOCKED-ZFREQNO.
          ELSEIF ZTCIVHD-ZFPRPYN EQ 'N'.       ">일반.
            CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
                 EXPORTING
                      ZFBLNO = IT_LOCKED-ZFBLNO.
            DELETE IT_LOCKED INDEX W_TABIX.
            DELETE IT_ZFREQNO WHERE ZFBLNO EQ IT_LOCKED-ZFBLNO.
          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'PGUP'.
      TC_3511-TOP_LINE = TC_3511-TOP_LINE - LOOPLINES.
      IF TC_3511-TOP_LINE < 1.
        TC_3511-TOP_LINE = 1.
      ENDIF.
    WHEN 'PGDN'.
      TC_3511-TOP_LINE = TC_3511-TOP_LINE + LOOPLINES.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해제.
      LOOP AT IT_ZSCIVIT.
        IT_ZSCIVIT-ZFMARK = W_MARK.  MODIFY IT_ZSCIVIT INDEX SY-TABIX.
      ENDLOOP.
      PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'REF1'.           " 비용코드 Refresh
      REFRESH : IT_ZSCIVIT.
      IT_ZSCIVIT[] = IT_ZSCIVIT_ORG[].
      PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'PODP' OR 'IMDP' OR 'BLDP'.   " P/O, 수입의뢰 조회.
      PERFORM   P2000_LINE_SELECT_CIV_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_CIV_ITEM.
      PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN 'CALC'.            " 금액 자동계산.
      PERFORM   P2000_IT_ZSCIVIT_RECALC.
      PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      EXIT.
    WHEN OTHERS.
      IF SY-BINPT NE 'X'.
        PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR3511  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR3511  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR3511 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_3511-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR3511  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3511_MARK_TC_3511  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3511_MARK_TC_3511 INPUT.

  READ TABLE IT_ZSCIVIT  WITH KEY
                         ZFCIVSQ = ZSCIVIT-ZFCIVSQ  BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSCIVIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSCIVIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSCIVIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3511_MARK_TC_3511  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCIVIT_UPDATE_SCR3511  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSCIVIT_UPDATE_SCR3511 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK ZSCIVIT-EBELN IS INITIAL.

* Internal Table Read
  READ TABLE IT_ZSCIVIT   WITH KEY ZFCIVSQ = ZSCIVIT-ZFCIVSQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.

*>> B/L 번호가 입력되지 않았을 경우.
  IF ZSCIVIT-ZFBLNO IS INITIAL.
    MESSAGE W039.    EXIT.
  ELSE.
*>> B/L 번호가 입력되었을 경우.
    SELECT SINGLE * FROM ZTBL
                    WHERE ZFBLNO EQ IT_ZSCIVIT-ZFBLNO.
    IF SY-SUBRC NE 0.
      CLEAR : ZSCIVIT.
      MESSAGE W038 WITH IT_ZSCIVIT-ZFBLNO.
      EXIT.
    ENDIF.

*>> B/L ITEM번호가 입력되었을 경우.
    IF NOT IT_ZSCIVIT-ZFBLIT IS INITIAL.
*-----------------------------------------------------------------------
* 기존에 해당 B/L 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSCIVIT WITH KEY ZFBLNO  = ZSCIVIT-ZFBLNO
                                      ZFBLIT  = ZSCIVIT-ZFBLIT.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE S369 WITH IT_ZSCIVIT-ZFBLNO IT_ZSCIVIT-ZFBLIT
                          IT_ZSCIVIT-ZFCIVSQ.
        EXIT.
      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.
*----> B/L ITEM번호가 입력되었을 경우, 발췌하여 메세지.
      SELECT SINGLE *
             FROM   ZTBLIT
             WHERE ZFBLNO EQ IT_ZSCIVIT-ZFBLNO
             AND   ZFBLIT EQ IT_ZSCIVIT-ZFBLIT
             AND   BLOEKZ NE 'X'.

      IF SY-SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE W370 WITH IT_ZSCIVIT-ZFBLNO IT_ZSCIVIT-ZFBLIT.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING  ZTBLIT   TO    IT_ZSCIVIT.
      MOVE : ZTBLIT-BLMENGE        TO    IT_ZSCIVIT-MENGE_BL.

*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ
          INTO (IT_ZSCIVIT-MENGE_PO, IT_ZSCIVIT-UEBTO,
                IT_ZSCIVIT-UEBTK,    IT_ZSCIVIT-WEPOS,
                IT_ZSCIVIT-ELIKZ,    IT_ZSCIVIT-LOEKZ,
                IT_ZSCIVIT-UNTTO,
                IT_ZSCIVIT-BPUMN,    IT_ZSCIVIT-BPUMZ)
          FROM   EKPO
          WHERE  EBELN   EQ   IT_ZSCIVIT-EBELN
          AND    EBELP   EQ   IT_ZSCIVIT-EBELP.

      IF SY-SUBRC EQ 0.
        IF IT_ZSCIVIT-LOEKZ NE SPACE.
          CLEAR : ZSCIVIT.
          MESSAGE W069 WITH IT_ZSCIVIT-EBELN IT_ZSCIVIT-EBELP.
          EXIT.
        ENDIF.
      ELSE.
        CLEAR : ZSCIVIT.
        MESSAGE W071 WITH IT_ZSCIVIT-EBELN IT_ZSCIVIT-EBELP.
        EXIT.
      ENDIF.

*+++++> 수입의뢰 수량.
      SELECT SINGLE MENGE KBETR KWERT KPEIN KMEIN
          INTO (IT_ZSCIVIT-MENGE, IT_ZSCIVIT-KBETR,
                IT_ZSCIVIT-KWERT, IT_ZSCIVIT-KPEIN,
                IT_ZSCIVIT-KMEIN)
          FROM  ZTREQIT
          WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
          AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

*>>>> B/L 수량.
      SELECT SINGLE BLMENGE INTO IT_ZSCIVIT-MENGE_BL
             FROM ZTBLIT
             WHERE ZFBLNO   EQ    IT_ZSCIVIT-ZFBLNO
             AND   ZFBLIT   EQ    IT_ZSCIVIT-ZFBLIT
             AND   BLOEKZ   NE    'X'.

*----> INVOICE 자재내역(기입력건)
      IF NOT IT_ZSCIVIT-ZFBLNO IS INITIAL.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFBLNO  EQ IT_ZSCIVIT-ZFBLNO
               AND   ZFBLIT  EQ IT_ZSCIVIT-ZFBLIT.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
               WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFBLNO  EQ IT_ZSCIVIT-ZFBLNO
               AND   ZFBLIT  EQ IT_ZSCIVIT-ZFBLIT
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ELSE.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
               WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ENDIF.

*>>> INVOICE 기본 수?
      IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE_BL - IT_ZSCIVIT-CIVTOT.
      IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE_BL - IT_ZSCIVIT-CIVTOT1.
      IT_ZSCIVIT-EBELN = IT_ZSCIVIT-EBELN.
      IT_ZSCIVIT-EBELP = IT_ZSCIVIT-EBELP.
      IF IT_ZSCIVIT-ZFPRQN LT 0.
        IT_ZSCIVIT-ZFPRQN = 0.
      ENDIF.
      IF IT_ZSCIVIT-CMENGE  LT 0.
        IT_ZSCIVIT-CMENGE = 0.
      ENDIF.

*> 수입의뢰 SELECT
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      ENDIF.

*>> 통화단위 검증..
      IF ZTCIVHD-ZFIVAMC IS INITIAL.
        ZTCIVHD-ZFIVAMC = ZTREQHD-WAERS.
      ENDIF.
      IF ZTCIVHD-ZFIVAMC NE ZTREQHD-WAERS.
        MESSAGE E379
        WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO ZTREQHD-WAERS.
      ENDIF.
*>> Beneficiay 검증.
      IF ZTCIVHD-ZFMAVN IS INITIAL.
        ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
      ENDIF.
      IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
        MESSAGE E381
        WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
      ENDIF.
*>> 회사코드 검증.
      IF ZTCIVHD-BUKRS IS INITIAL.
        ZTCIVHD-BUKRS  =  ZTREQHD-BUKRS.
      ENDIF.
      IF ZTCIVHD-BUKRS NE ZTREQHD-BUKRS.
        MESSAGE E382
           WITH ZTCIVHD-BUKRS IT_ZSCIVIT-ZFREQNO ZTREQHD-BUKRS.
      ENDIF.

      SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTCIVHD-BUKRS.

      MOVE : T001-WAERS        TO    IT_ZSCIVIT-ZFKRW,
             ZTCIVHD-ZFIVAMC   TO    IT_ZSCIVIT-ZFIVAMC.

*>> ITEM LOCKED...
      PERFORM P2000_SET_CIVIT_LOCK_ITEM.

*----> 기존 데이타 갱신.
      IF W_SY_SUBRC EQ 0.
        MODIFY IT_ZSCIVIT  INDEX W_TABIX.
      ELSE.
*----> 신규 데이타 추가.
        IT_ZSCIVIT-ZFCIVSQ = TC_3511-CURRENT_LINE * 10.
        APPEND  IT_ZSCIVIT.
      ENDIF.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.
*---> 기존 데이타가 있었을 경우.
    IF W_SY_SUBRC EQ 0.
      DELETE IT_ZSCIVIT  INDEX W_TABIX.
    ELSE.
      IT_ZSCIVIT-ZFCIVSQ = TC_3511-CURRENT_LINE * 10.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVIT_TMP
             FROM ZTBLIT
             WHERE ZFBLNO EQ ZSCIVIT-ZFBLNO
             AND   BLOEKZ NE 'X'.

    LOOP AT IT_ZSCIVIT_TMP.
      MOVE-CORRESPONDING   IT_ZSCIVIT_TMP  TO  IT_ZSCIVIT.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSCIVIT WITH KEY ZFBLNO = IT_ZSCIVIT-ZFBLNO
                                      ZFBLIT = IT_ZSCIVIT-ZFBLIT.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE S369 WITH IT_ZSBLIT-ZFBLNO IT_ZSCIVIT-ZFBLIT
                          IT_ZSCIVIT-ZFCIVSQ.
        CONTINUE.
      ENDIF.
*-----------------------------------------------------------------------

*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ
             INTO (IT_ZSCIVIT-MENGE_PO, IT_ZSCIVIT-UEBTO,
                   IT_ZSCIVIT-UEBTK,    IT_ZSCIVIT-WEPOS,
                   IT_ZSCIVIT-ELIKZ,    IT_ZSCIVIT-LOEKZ,
                   IT_ZSCIVIT-UNTTO,
                   IT_ZSCIVIT-BPUMN,    IT_ZSCIVIT-BPUMZ)
             FROM   EKPO
             WHERE  EBELN   EQ   IT_ZSCIVIT-EBELN
             AND    EBELP   EQ   IT_ZSCIVIT-EBELP.

      IF SY-SUBRC EQ 0.
        IF IT_ZSCIVIT-LOEKZ NE SPACE.
          CLEAR : ZSBLIT.
          MESSAGE W069 WITH IT_ZSCIVIT-EBELN IT_ZSCIVIT-EBELP.
          CONTINUE.
        ENDIF.
      ELSE.
        CLEAR : ZSCIVIT.
        MESSAGE W071 WITH IT_ZSCIVIT-EBELN IT_ZSCIVIT-EBELP.
        CONTINUE.
      ENDIF.
*+++++> 수입의뢰 수량.
      SELECT SINGLE MENGE KBETR KWERT KPEIN KMEIN
          INTO (IT_ZSCIVIT-MENGE, IT_ZSCIVIT-KBETR,
                IT_ZSCIVIT-KWERT, IT_ZSCIVIT-KPEIN,
                IT_ZSCIVIT-KMEIN)
          FROM  ZTREQIT
          WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
          AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

*>>>>> B/L TOTAL 수량(수입의뢰별).
*        SELECT SUM( BLMENGE ) INTO IT_ZSCIVIT-ZFBLTOT
*            FROM  ZTBLIT
*            WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
*            AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
*            AND   BLOEKZ   NE 'X'.

*>>>> B/L 수량.
      SELECT SINGLE BLMENGE INTO IT_ZSCIVIT-MENGE_BL
             FROM ZTBLIT
             WHERE ZFBLNO   EQ    IT_ZSCIVIT-ZFBLNO
             AND   ZFBLIT   EQ    IT_ZSCIVIT-ZFBLIT
             AND   BLOEKZ NE 'X'.

*----> INVOICE 자재내역(기입력건)
      IF NOT IT_ZSCIVIT-ZFBLNO IS INITIAL.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFBLNO  EQ IT_ZSCIVIT-ZFBLNO
               AND   ZFBLIT  EQ IT_ZSCIVIT-ZFBLIT.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
               WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFBLNO   EQ IT_ZSCIVIT-ZFBLNO
               AND   ZFBLIT   EQ IT_ZSCIVIT-ZFBLIT
               AND   ZFPRPYN  NE 'Y'.        " 선급금이 아닌 것.
      ELSE.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
               WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ENDIF.

*>>> INVOICE 기본 수?
      IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE_BL - IT_ZSCIVIT-CIVTOT.
      IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE_BL - IT_ZSCIVIT-CIVTOT1.

      IF IT_ZSCIVIT-ZFPRQN LT 0.
        IT_ZSCIVIT-ZFPRQN = 0.
      ENDIF.
      IF IT_ZSCIVIT-CMENGE  LT 0.
        IT_ZSCIVIT-CMENGE = 0.
      ENDIF.
*> 수입의뢰 SELECT
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      ENDIF.

*>> 통화단위 검증..
      IF ZTCIVHD-ZFIVAMC IS INITIAL.
        ZTCIVHD-ZFIVAMC = ZTREQHD-WAERS.
      ENDIF.
      IF ZTCIVHD-ZFIVAMC NE ZTREQHD-WAERS.
        MESSAGE E379
        WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO ZTREQHD-WAERS.
      ENDIF.
*>> Beneficiay 검증.
      IF ZTCIVHD-ZFMAVN IS INITIAL.
        ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
      ENDIF.
      IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
        MESSAGE E381
        WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
      ENDIF.
*>> 회사코드 검증.
      IF ZTCIVHD-BUKRS IS INITIAL.
        ZTCIVHD-BUKRS  =  ZTREQHD-BUKRS.
      ENDIF.
      IF ZTCIVHD-BUKRS NE ZTREQHD-BUKRS.
        MESSAGE E382
           WITH ZTCIVHD-BUKRS IT_ZSCIVIT-ZFREQNO ZTREQHD-BUKRS.
      ENDIF.

*>> ITEM LOCKED.
      PERFORM P2000_SET_CIVIT_LOCK_ITEM.

      MOVE : 'KRW'             TO    IT_ZSCIVIT-ZFKRW,
             ZTCIVHD-ZFIVAMC   TO    IT_ZSCIVIT-ZFIVAMC.
*----> 중간에 삽입.
      IF W_SY_SUBRC EQ 0.
        IT_ZSCIVIT-ZFCIVSQ = W_TABIX * 10.
        INSERT IT_ZSCIVIT  INDEX W_TABIX.
        ADD 1 TO W_TABIX.
      ELSE.
*----> 마지막에 추가.
        APPEND IT_ZSCIVIT.
        IT_ZSCIVIT-ZFCIVSQ = IT_ZSCIVIT-ZFCIVSQ + 10.
      ENDIF.
*     ENDSELECT.
    ENDLOOP.

    IF SY-SUBRC NE 0.
      CLEAR : ZSCIVIT.
      MESSAGE W384 WITH IT_ZSCIVIT-ZFBLNO.
      EXIT.
    ENDIF.

  ENDIF.

ENDMODULE.                 " IT_ZSCIVIT_UPDATE_SCR3511  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCIVIT_MENGE_CHECK_SCR3511  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSCIVIT_MENGE_CHECK_SCR3511 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSCIVIT-ZFBLNO IS INITIAL. "B/L NO 입력되지 않았을 경우.
  CLEAR : IT_ZSCIVIT.
  READ TABLE IT_ZSCIVIT   WITH KEY ZFBLNO  = ZSCIVIT-ZFBLNO
                                   ZFBLIT  = ZSCIVIT-ZFBLIT.
  W_OK_CODE = SY-SUBRC.
  W_TABIX = SY-TABIX.
  CHECK SY-SUBRC EQ 0.

  IF NOT ZSCIVIT-EBELN IS INITIAL.
    MOVE-CORRESPONDING ZSCIVIT TO IT_ZSCIVIT.
  ENDIF.
  CLEAR : W_OLD_MENGE, W_MENGE1, W_MENGE2.
  IF NOT ZTCIVHD-ZFCIVRN IS INITIAL.
    SELECT SINGLE CMENGE ZFPRQN INTO (W_MENGE1, W_MENGE2)
                 FROM   ZTCIVIT
                 WHERE  ZFCIVRN   EQ   ZTCIVHD-ZFCIVRN
                 AND    ZFBLNO    EQ   IT_ZSCIVIT-ZFBLNO
                 AND    ZFBLIT    EQ   IT_ZSCIVIT-ZFBLIT.
  ENDIF.

*>> B/L 수량 - OPEN 수량  - 기존B/L수량.
  W_OLD_MENGE = IT_ZSCIVIT-MENGE_BL -
              ( IT_ZSCIVIT-CIVTOT1 - W_MENGE1 ).
  IF IT_ZSCIVIT-CMENGE GT W_OLD_MENGE.
    PERFORM P2000_NO_INPUT  USING  'ZSCIVIT' 'CMENGE'
                                   DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E371 WITH IT_ZSCIVIT-ZFCIVSQ.
  ENDIF.
*>> B/L 수량 - OPEN 수량  - 기존B/L수량.
  W_OLD_MENGE = IT_ZSCIVIT-MENGE_BL -
              ( IT_ZSCIVIT-CIVTOT - W_MENGE2 ).
  IF IT_ZSCIVIT-ZFPRQN GT W_OLD_MENGE.
    PERFORM P2000_NO_INPUT  USING  'ZSCIVIT' 'ZFPRQN'
                                   DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E372 WITH IT_ZSCIVIT-ZFCIVSQ.
  ENDIF.

*>> INVOICE 자재별 금액.
  IF IT_ZSCIVIT-ZFIVAMT IS INITIAL.
    IT_ZSCIVIT-ZFIVAMT = ( IT_ZSCIVIT-CMENGE *
                         ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                         ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
  ENDIF.

  IF IT_ZSCIVIT-ZFIVAMP IS INITIAL.
    IT_ZSCIVIT-ZFIVAMP = ( IT_ZSCIVIT-ZFPRQN *
                      ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                      ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
  ENDIF.

*>> 처리 원화 금액 계산...
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSCIVIT-ZFIVAMP
                                          IT_ZSCIVIT-ZFIVAMC
                                          IT_ZSCIVIT-ZFIVAMK.
  IF ZTCIVHD-FFACT IS INITIAL.
    ZTCIVHD-FFACT  =  1.
  ENDIF.
   *BAPICURR-BAPICURR  = ( ZTCIVHD-ZFEXRT / ZTCIVHD-FFACT )
                         * IT_ZSCIVIT-ZFIVAMK.
  PERFORM SET_CURR_CONV_TO_INTERNAL USING
                         *BAPICURR-BAPICURR ZTCIVHD-ZFKRW.
  IF *BAPICURR-BAPICURR GT 9999999999999.
    MESSAGE W923 WITH *BAPICURR-BAPICURR.
    IT_ZSCIVIT-ZFIVAMK = 0.
  ELSE.
    IT_ZSCIVIT-ZFIVAMK = *BAPICURR-BAPICURR.
  ENDIF.

  IF W_OK_CODE EQ 0.
    MODIFY  IT_ZSCIVIT  INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSCIVIT_MENGE_CHECK_SCR3511  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_HELP_MSNO_SCR9910  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_HELP_MSNO_SCR9910 INPUT.

  REFRESH IT_MSNM.
* 모선명 SELECT.
  SELECT ZFMSNM
  INTO   CORRESPONDING FIELDS OF TABLE IT_MSNM
  FROM   ZTMSHD
  GROUP BY
         ZFMSNM.

  DESCRIBE TABLE IT_MSNM      LINES W_COUNTER.

  IF W_COUNTER EQ 0.
    MESSAGE I464.
  ELSE.
    CLEAR W_NAME.
    W_STATUS_CHK = 'C'.
    INCLUDE = 'MSNMFIND'.                 "모선명 FIND.
    CALL SCREEN 0014 STARTING AT  40 3
                     ENDING   AT  70 10.

    IF NOT ( W_NAME IS INITIAL ).
      ZTMSHD-ZFMSNM = W_NAME.
    ENDIF.

  ENDIF.

ENDMODULE.                 " GET_HELP_MSNO_SCR9910  INPUT
*&---------------------------------------------------------------------*
*&      Module  VENDOR_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
MODULE VENDOR_CHECK_SCR3510 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTCIVHD-ZFMAVN IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHD' 'ZFMAVN'.
  ELSE.
    PERFORM  P1000_GET_VENDOR   USING      ZTCIVHD-ZFMAVN
                                           CHANGING  W_LIFNR_NM.
  ENDIF.

ENDMODULE.                 " VENDOR_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*&      Module  CURRENCY_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
MODULE CURRENCY_CHECK_SCR3510 INPUT.

  SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTCIVHD-BUKRS.

  IF ZTCIVHD-ZFIVAMC IS INITIAL.
    MESSAGE E167 WITH 'Invoice Amount Currency'.
  ENDIF.

*>> 통화단위가 다른 경우.
  W_COUNT1 = 0.
  LOOP AT IT_ZSCIVIT WHERE ZFIVAMC <> ZTCIVHD-ZFIVAMC.
    MESSAGE E379
         WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO IT_ZSCIVIT-ZFIVAMC.
  ENDLOOP.

  MOVE ZTCIVHD-ZFIVAMC  TO  ZTCIVHD-ZFPKCUR.
  MOVE ZTCIVHD-ZFIVAMC  TO  ZTCIVHD-ZFHDCUR.

  IF ZTCIVHD-ZFIVAMC EQ T001-WAERS.
    ZTCIVHD-ZFEXRT = 1.
  ENDIF.

ENDMODULE.                 " CURRENCY_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*&      Module  AMOUNT_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE AMOUNT_CHECK_SCR3510 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK W_LINE GT 0.

*  IF ZTCIVHD-ZFIVAMT IS INITIAL.
*     MESSAGE W167 WITH 'Invoice Amount'.
*     ZTCIVHD-ZFIVAMT = W_TOT_AMOUNT.
*  ELSE.
  IF OK-CODE EQ 'CALC'.
    ZTCIVHD-ZFIVAMT = W_TOT_AMOUNT.
  ELSE.
    IF ZTCIVHD-ZFIVAMT NE W_TOT_AMOUNT.
      MESSAGE W784.
    ENDIF.
  ENDIF.
*  ENDIF.

*>> 처리금액.
  ZTCIVHD-ZFIVAMP = W_TOT_AMOUNT.
  ZTCIVHD-ZFIVAMK =  W_TOT_AMOUNT1.

ENDMODULE.                 " AMOUNT_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXCHANGE_RATE_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
MODULE EXCHANGE_RATE_CHECK_SCR3510 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  DATA : W_DATE_OUT(10)    TYPE C,               "NCW 추가
         W_DATE_OUT2(10)   TYPE C,
         W_DATE_OUT3(10)   TYPE C,
         W_DATE_CON(8)     TYPE N.

  CLEAR : W_DATE_OUT, W_DATE_OUT2, W_DATE_OUT3.

  IF ZTCIVHD-ZFPKCUR NE 'USD'.
    IF ZTCIVHD-ZFEXRT IS INITIAL.
* NCW 막음 ( 1 LINE )
*  IF ZTCIVHD-ZFEXRT IS INITIAL.
* MKIM 2001.05.15 환율 가져오?
      CALL FUNCTION 'ZIM_GET_EXCHANGE_RATE'
           EXPORTING
                P_WAERS    = ZTCIVHD-ZFIVAMC
                P_DATE     = ZTCIVHD-BUDAT
                P_KURST    = 'M'
                P_TO_WAERS = 'USD'  " ZTCIVHD-ZFKRW -> USD 수정
           IMPORTING
                P_EXRT     = ZTCIVHD-ZFEXRT
                P_FFACT    = ZTCIVHD-FFACT
           EXCEPTIONS
                NO_INPUT   = 4
                NOT_FOUND  = 6.

      CASE SY-SUBRC.
        WHEN 4.
          MESSAGE E094.
        WHEN 6.
          MESSAGE E094.
        WHEN OTHERS.
      ENDCASE.

* DB Input Type Convert
      W_DATE_OUT     = ZTCIVHD-ZFCIDT.
      W_DATE_CON     = '99999999' - W_DATE_OUT.
      W_DATE_OUT     = W_DATE_CON.

*>> NCW 수정 2003.12.02 - Invoice date message
      SELECT SINGLE * FROM TCURR
       WHERE KURST EQ 'M'
         AND FCURR EQ  ZTCIVHD-ZFIVAMC
         AND TCURR EQ 'USD'                  " Foreign currency -> USD
         AND GDATU EQ W_DATE_OUT.

      IF SY-SUBRC NE 0.
        MESSAGE I977 WITH
        'The exchange rate of invoice date does not exist'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZTCIVHD-FFACT IS INITIAL.
*    MESSAGE W637.
    ZTCIVHD-FFACT = 1.
  ENDIF.

ENDMODULE.                 " EXCHANGE_RATE_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_REQ_DATA_SCR3514  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_REQ_DATA_SCR3514 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.
      EXIT.
  ENDCASE.

  IF ZSREQHD-ZFOPNNO IS INITIAL.         " 문서 NO를 입력하지 않을 경?
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E061.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFREQNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
* L/C NO에 count인 문서관리번호를 SELECT.
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTREQST
                        WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.
      IF W_COUNT EQ 0.
        MESSAGE E067 WITH ZSREQHD-ZFOPNNO.
      ELSEIF W_COUNT > 1.
        MESSAGE E134 WITH W_COUNT.
      ENDIF.

* L/C NO에 MAX인 문서관리번호를 SELECT.
      SELECT MAX( ZFREQNO ) INTO  W_ZFREQNO
                            FROM  ZTREQST
                            WHERE ZFOPNNO EQ ZSREQHD-ZFOPNNO.

      IF SY-SUBRC NE 0.
        MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
      ELSE.
        IF W_ZFREQNO IS INITIAL.
          IF ZSREQHD-ZFREQNO IS INITIAL.  " 관리번호가 입력되지 않?
            MESSAGE E062 WITH  ZSREQHD-ZFOPNNO.
          ENDIF.
        ELSE.
          ZSREQHD-ZFREQNO = W_ZFREQNO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* 수입의뢰 Header Table
  CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
       EXPORTING
            ZFREQNO   = ZSREQHD-ZFREQNO
       IMPORTING
            W_ZTREQHD = ZTREQHD
            W_ZTREQST = ZTREQST
       EXCEPTIONS
            NOT_FOUND = 4
            NOT_INPUT = 8.

  CASE SY-SUBRC.
    WHEN 4.
      MESSAGE E018 WITH ZSREQHD-ZFREQNO.
    WHEN 8.
      MESSAGE E019.
  ENDCASE.

*>> 삼국무역...
  IF ZTREQHD-ZFTRIPLE EQ 'X'.
    MESSAGE E535 WITH ZTREQHD-ZFREQNO.
  ENDIF.

*>> 사후관리여부...
  IF ZTREQHD-ZFCLOSE EQ 'X'.
    MESSAGE E354 WITH ZTREQHD-ZFREQNO.
  ENDIF.

*>> 환율차이 오류 메시지.
  IF ( ZTIMIMG00-ZFEXFIX EQ 'X'  ) AND
     ( ZTIMIMG00-ZFEXMTD EQ 'B'    OR  ZTIMIMG00-ZFEXMTD EQ 'A' OR
       ZTIMIMG00-ZFEXMTD EQ 'R'   ).
    IF ZTREQHD-WAERS NE 'KRW'.
      SELECT SINGLE * FROM EKKO
                      WHERE EBELN  EQ  ZTREQHD-EBELN.
      IF EKKO-KUFIX NE 'X'.
        MESSAGE W528 WITH ZTREQHD-EBELN.
      ELSE.
        IF ZTREQHD-KURSF NE EKKO-WKURS.
          MESSAGE W614 WITH ZTREQHD-EBELN EKKO-WKURS
                            ZTREQHD-ZFREQNO ZTREQHD-KURSF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZTIMIMG00-ZFEXFIX EQ 'X' AND ZTIMIMG00-ZFEXMTD EQ 'G'.

    CLEAR : ZTREQIT, W_CNT.
    SELECT * FROM  ZTREQIT WHERE ZFREQNO EQ ZTREQHD-ZFREQNO.

      SELECT  SINGLE  * FROM  ZTCIVIT
                        WHERE EBELN    EQ  ZTREQIT-EBELN
                        AND   EBELP    EQ  ZTREQIT-EBELP
                        AND   ZFPRPYN  EQ  'Y'.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE * FROM ZTCIVHD
               WHERE  ZFCIVRN  EQ ZTCIVIT-ZFCIVRN.
        MOVE : ZTCIVHD-ZFEXRT  TO  ZTREQHD-KURSF,
               ZTCIVHD-FFACT   TO  ZTREQHD-FFACT.
      ENDIF.
    ENDSELECT.
  ENDIF.

  REFRESH : IT_ZFREQNO, IT_LOCKED, IT_ZSCIVIT, IT_ZSIVIT.
  IF SY-TCODE EQ 'ZIM31'.
    PERFORM  P2000_REQ_TO_CC_MOVE.           ">수입의뢰 ===> 통관요청.
  ELSE.
    PERFORM  P2000_REQ_DATA_MOVE.            ">수입의뢰 ===> CIV DATA.
  ENDIF.

ENDMODULE.                 " READ_REQ_DATA_SCR3514  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR3514  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR3514 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR3514  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR3513  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR3513 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_3513-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR3513  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCIVIT_UPDATE_SCR3513  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSCIVIT_UPDATE_SCR3513 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK ZSCIVIT-EBELN IS INITIAL.

* Internal Table Read
  READ TABLE IT_ZSCIVIT   WITH KEY ZFCIVSQ = ZSCIVIT-ZFCIVSQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.

*>> 수입의뢰 번호가 입력되지 않았을 경우.
  IF ZSCIVIT-ZFREQNO IS INITIAL.
    MESSAGE W019.   EXIT.
  ELSE.
*>> 수입의뢰 번호가 입력되었을 경우.
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO.
    IF SY-SUBRC NE 0.
      CLEAR : ZSCIVIT.
      MESSAGE W018 WITH IT_ZSCIVIT-ZFREQNO.
      EXIT.
    ENDIF.
*>> 삼국무역...
    IF ZTREQHD-ZFTRIPLE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSCIVIT.  EXIT.
    ENDIF.
*>> 사후관리여부...
    IF ZTREQHD-ZFCLOSE EQ 'X'.
      MESSAGE W535 WITH ZTREQHD-ZFREQNO.
      CLEAR : ZSCIVIT.  EXIT.
    ENDIF.
*>> 문서 타입.
    IF ZTCIVHD-ZFREQTY EQ 'LO' OR ZTCIVHD-ZFREQTY EQ 'PU'.
      IF NOT ( ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU' ).
        MESSAGE W536 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
        CLEAR : ZSCIVIT.  EXIT.
      ENDIF.
      IF ZTCIVHD-ZFREQTY NE ZTREQHD-ZFREQTY.
        MESSAGE W568 WITH ZTCIVHD-ZFREQTY ZTREQHD-ZFREQTY.
        CLEAR : ZSCIVIT.  EXIT.
      ENDIF.
      READ TABLE IT_ZSCIVIT INDEX  1.
      IF SY-SUBRC EQ 0.
        IF IT_ZSCIVIT-ZFREQNO NE ZTREQHD-ZFREQNO.
          MESSAGE W569.
          CLEAR : ZSCIVIT.  EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE W536 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
        CLEAR : ZSCIVIT.  EXIT.
      ENDIF.
    ENDIF.

*>> 수입의뢰 ITEM번호가 입력되었을 경우.
    IF NOT IT_ZSCIVIT-ZFITMNO IS INITIAL.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSCIVIT WITH KEY ZFREQNO = ZSCIVIT-ZFREQNO
                                      ZFITMNO = ZSCIVIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE S358 WITH ZSCIVIT-ZFREQNO ZSCIVIT-ZFITMNO
                          IT_ZSCIVIT-ZFCIVSQ.
        EXIT.
      ENDIF.
*-----------------------------------------------------------------------
      MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.
*----> 수입의뢰 ITEM번호가 입력되었을 경우, 발췌하여 메세지.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF IT_ZSCIVIT
             FROM   ZTREQIT
             WHERE ZFREQNO EQ ZSCIVIT-ZFREQNO
             AND   ZFITMNO EQ ZSCIVIT-ZFITMNO.
      IF SY-SUBRC NE 0.
        CLEAR : ZSBLIT.
        MESSAGE W357 WITH ZSCIVIT-ZFREQNO ZSCIVIT-ZFITMNO.
        EXIT.
      ENDIF.
*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ
             INTO (IT_ZSCIVIT-MENGE_PO, IT_ZSCIVIT-UEBTO,
                   IT_ZSCIVIT-UEBTK,    IT_ZSCIVIT-WEPOS,
                   IT_ZSCIVIT-ELIKZ,    IT_ZSCIVIT-LOEKZ,
                   IT_ZSCIVIT-UNTTO,
                   IT_ZSCIVIT-BPUMN,    IT_ZSCIVIT-BPUMZ)
             FROM   EKPO
             WHERE  EBELN   EQ   ZTREQHD-EBELN
             AND    EBELP   EQ   ZSCIVIT-ZFITMNO.
      IF SY-SUBRC EQ 0.
        IF IT_ZSCIVIT-LOEKZ NE SPACE.
          CLEAR : ZSCIVIT.
          MESSAGE W069 WITH ZTREQHD-EBELN ZSCIVIT-ZFITMNO.
          EXIT.
        ENDIF.
*           IF IT_ZSCIVIT-ELIKZ EQ 'X'.
*              CLEAR : ZSCIVIT.
*              MESSAGE W359 WITH ZTREQHD-EBELN ZSCIVIT-ZFITMNO.
*              EXIT.
*           ENDIF.
      ELSE.
        CLEAR : ZSCIVIT.
        MESSAGE W071 WITH ZTREQHD-EBELN ZSCIVIT-ZFITMNO.
        EXIT.
      ENDIF.

*----> INVOICE 자재내역(기입력건)
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ELSE.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ENDIF.
*> 수입의뢰 SELECT
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      ENDIF.

*>> 통화단위 검증..
      IF ZTCIVHD-ZFIVAMC IS INITIAL.
        ZTCIVHD-ZFIVAMC = ZTREQHD-WAERS.
      ENDIF.
      IF ZTCIVHD-ZFIVAMC NE ZTREQHD-WAERS.
        MESSAGE E379
        WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO ZTREQHD-WAERS.
      ENDIF.
*>> Beneficiay 검증.
      IF ZTCIVHD-ZFMAVN IS INITIAL.
        ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
      ENDIF.
      IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
        MESSAGE E381
        WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
      ENDIF.
*>> 회사코드 검증.
      IF ZTCIVHD-BUKRS IS INITIAL.
        ZTCIVHD-BUKRS  =  ZTREQHD-BUKRS.
      ENDIF.
      IF ZTCIVHD-BUKRS NE ZTREQHD-BUKRS.
        MESSAGE E382
           WITH ZTCIVHD-BUKRS IT_ZSCIVIT-ZFREQNO ZTREQHD-BUKRS.
      ENDIF.


*>>> INVOICE 기본 수?
      IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT.
      IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT1.
      IT_ZSCIVIT-EBELN   = ZTREQHD-EBELN.
      IT_ZSCIVIT-EBELP   = IT_ZSCIVIT-ZFITMNO.
      IT_ZSCIVIT-ZFKRW   = 'KRW'.
      IT_ZSCIVIT-ZFIVAMC = ZTCIVHD-ZFIVAMC.

*>>> 선급비율 계산하여 수량 조정...
      IF NOT ZTCIVHD-ZFPRTE IS INITIAL.
        W_MENGE = ( IT_ZSCIVIT-MENGE * ZTCIVHD-ZFPRTE ) / 100.
        IF W_MENGE LE IT_ZSCIVIT-ZFPRQN.
          IT_ZSCIVIT-ZFPRQN = W_MENGE.
        ENDIF.
      ENDIF.

*>>> ITEM DOCUMENT LOCK...
      PERFORM P2000_SET_CIVIT_LOCK_ITEM.

*----> 기존 데이타 갱신.
      IF W_SY_SUBRC EQ 0.
        IT_ZSCIVIT-EBELN = ZTREQHD-EBELN.
        MODIFY IT_ZSCIVIT  INDEX W_TABIX.
      ELSE.
*----> 신규 데이타 추가.
        IT_ZSCIVIT-ZFBLIT = TC_3513-CURRENT_LINE * 10.
        IT_ZSCIVIT-EBELN  = ZTREQHD-EBELN.
        APPEND  IT_ZSCIVIT.
      ENDIF.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING ZSCIVIT  TO IT_ZSCIVIT.
*---> 기존 데이타가 있었을 경우.
    IF W_SY_SUBRC EQ 0.
      DELETE IT_ZSCIVIT  INDEX W_TABIX.
    ELSE.
      IT_ZSCIVIT-ZFCIVSQ = TC_3513-CURRENT_LINE * 10.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCIVIT_TMP
             FROM ZTREQIT
             WHERE ZFREQNO EQ ZSCIVIT-ZFREQNO.

    LOOP AT IT_ZSCIVIT_TMP.
      MOVE-CORRESPONDING   IT_ZSCIVIT_TMP TO  IT_ZSCIVIT.
*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
      READ TABLE  IT_ZSCIVIT WITH KEY ZFREQNO = IT_ZSCIVIT-ZFREQNO
                                      ZFITMNO = IT_ZSCIVIT-ZFITMNO.
      IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
        CLEAR : ZSCIVIT.
        MESSAGE S358 WITH IT_ZSCIVIT-ZFREQNO IT_ZSCIVIT-ZFITMNO
                          IT_ZSCIVIT-ZFCIVSQ.
        CONTINUE.
      ENDIF.
*-----------------------------------------------------------------------
*++++> P/O DATA 조회.
      SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                    BPUMN BPUMZ
             INTO (IT_ZSCIVIT-MENGE_PO, IT_ZSCIVIT-UEBTO,
                   IT_ZSCIVIT-UEBTK,    IT_ZSCIVIT-WEPOS,
                   IT_ZSCIVIT-ELIKZ,    IT_ZSCIVIT-LOEKZ,
                   IT_ZSCIVIT-UNTTO,
                   IT_ZSCIVIT-BPUMN,    IT_ZSCIVIT-BPUMZ)
             FROM   EKPO
             WHERE  EBELN   EQ   ZTREQHD-EBELN
             AND    EBELP   EQ   IT_ZSCIVIT-ZFITMNO.

      IF SY-SUBRC EQ 0.
        IF IT_ZSCIVIT-LOEKZ NE SPACE.
          CLEAR : ZSCIVIT.
          MESSAGE W069 WITH ZTREQHD-EBELN IT_ZSCIVIT-ZFITMNO.
          CONTINUE.
        ENDIF.
*           IF IT_ZSCIVIT-ELIKZ EQ 'X'.
*              CLEAR : ZSCIVIT.
*              MESSAGE W359 WITH ZTREQHD-EBELN IT_ZSCIVIT-ZFITMNO.
*              CONTINUE.
*           ENDIF.
      ELSE.
        CLEAR : ZSCIVIT.
        MESSAGE W071 WITH ZTREQHD-EBELN IT_ZSCIVIT-ZFITMNO.
        CONTINUE.
      ENDIF.
*----> INVOICE 자재내역(기입력건)
      IF W_STATUS EQ C_REQ_C.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.

      ELSE.
        SELECT SUM( ZFPRQN ) INTO IT_ZSCIVIT-CIVTOT
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO.

        SELECT SUM( CMENGE ) INTO IT_ZSCIVIT-CIVTOT1
               FROM  ZTCIVIT
*                  WHERE ZFBLNO  NE ZTBL-ZFBLNO
               WHERE ZFREQNO EQ IT_ZSCIVIT-ZFREQNO
               AND   ZFITMNO EQ IT_ZSCIVIT-ZFITMNO
               AND   ZFPRPYN NE 'Y'.        " 선급금이 아닌 것.
      ENDIF.
*> 수입의뢰 SELECT
      SELECT SINGLE * FROM  ZTREQHD
                      WHERE ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO.

      IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
        MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
      ENDIF.

*>> 통화단위 검증..
      IF ZTCIVHD-ZFIVAMC IS INITIAL.
        ZTCIVHD-ZFIVAMC = ZTREQHD-WAERS.
      ENDIF.
      IF ZTCIVHD-ZFIVAMC NE ZTREQHD-WAERS.
        MESSAGE E379
        WITH ZTCIVHD-ZFIVAMC IT_ZSCIVIT-ZFREQNO ZTREQHD-WAERS.
      ENDIF.
*>> Beneficiay 검증.
      IF ZTCIVHD-ZFMAVN IS INITIAL.
        ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
      ENDIF.
      IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
        MESSAGE E381
        WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
      ENDIF.
*>> 회사코드 검증.
      IF ZTCIVHD-BUKRS IS INITIAL.
        ZTCIVHD-BUKRS  =  ZTREQHD-BUKRS.
      ENDIF.
      IF ZTCIVHD-BUKRS NE ZTREQHD-BUKRS.
        MESSAGE E382
           WITH ZTCIVHD-BUKRS IT_ZSCIVIT-ZFREQNO ZTREQHD-BUKRS.
      ENDIF.


*>>> INVOICE 기본 수?
      IT_ZSCIVIT-ZFPRQN = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT.
      IT_ZSCIVIT-CMENGE = IT_ZSCIVIT-MENGE - IT_ZSCIVIT-CIVTOT1.
      IT_ZSCIVIT-EBELN   = ZTREQHD-EBELN.
      IT_ZSCIVIT-EBELP   = IT_ZSCIVIT-ZFITMNO.
      IT_ZSCIVIT-ZFKRW   = 'KRW'.
      IT_ZSCIVIT-ZFIVAMC = ZTCIVHD-ZFIVAMC.

*>>> 선급비율 계산하여 수량 조정...
      IF NOT ZTCIVHD-ZFPRTE IS INITIAL.
        W_MENGE = ( IT_ZSCIVIT-MENGE * ZTCIVHD-ZFPRTE ) / 100.
        IF W_MENGE LE IT_ZSCIVIT-ZFPRQN.
          IT_ZSCIVIT-ZFPRQN = W_MENGE.
        ENDIF.
      ENDIF.
*>>> ITEM DOCUMENT LOCK...
      PERFORM P2000_SET_CIVIT_LOCK_ITEM.

*----> 중간에 삽입.
      IF W_SY_SUBRC EQ 0.
        IT_ZSCIVIT-ZFCIVSQ = W_TABIX * 10.
        INSERT IT_ZSCIVIT  INDEX W_TABIX.
        ADD 1 TO W_TABIX.
      ELSE.
*----> 마지막에 추가.
        APPEND IT_ZSCIVIT.
        IT_ZSCIVIT-ZFCIVSQ = IT_ZSCIVIT-ZFCIVSQ + 10.
      ENDIF.
*     ENDSELECT.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " IT_ZSCIVIT_UPDATE_SCR3513  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCIVIT_MENGE_CHECK_SCR3513  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSCIVIT_MENGE_CHECK_SCR3513 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSCIVIT-ZFREQNO IS INITIAL. "수입의뢰NO 입력되지 않았을경우.
  CLEAR : IT_ZSCIVIT.
* READ TABLE IT_ZSCIVIT   WITH KEY ZFCIVSQ = ZSCIVIT-ZFCIVSQ.
  READ TABLE IT_ZSCIVIT   WITH KEY ZFREQNO = ZSCIVIT-ZFREQNO
                                   ZFITMNO = ZSCIVIT-ZFITMNO.
  W_OK_CODE = SY-SUBRC.
  W_TABIX = SY-TABIX.
  CHECK SY-SUBRC EQ 0.

  MOVE-CORRESPONDING ZSCIVIT TO IT_ZSCIVIT.
  IF ZTCIVHD-ZFPRPYN EQ 'Y'.           "> 선급금일 경우.
    CLEAR : ZSCIVIT-CMENGE, IT_ZSCIVIT-CMENGE.
  ENDIF.

  IF IT_ZSCIVIT-UEBTK EQ 'X'.          " 지시자: 허용된 무제한초과납품.
*>> INVOICE 자재별 금액.
    IF IT_ZSCIVIT-ZFIVAMT IS INITIAL.
      IT_ZSCIVIT-ZFIVAMT = ( IT_ZSCIVIT-CMENGE *
                        ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                        ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
    ENDIF.

    IF IT_ZSCIVIT-ZFIVAMP IS INITIAL.
      IT_ZSCIVIT-ZFIVAMP = ( IT_ZSCIVIT-ZFPRQN *
                        ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                        ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
    ENDIF.

*>> 처리 원화 금액 계산...
    PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSCIVIT-ZFIVAMP
                                            IT_ZSCIVIT-ZFIVAMC
                                            IT_ZSCIVIT-ZFIVAMK.

    IF ZTCIVHD-FFACT IS INITIAL.
      ZTCIVHD-FFACT = 1.
    ENDIF.
*     IT_ZSCIVIT-ZFIVAMK = ZTCIVHD-ZFEXRT * IT_ZSCIVIT-ZFIVAMK.
     *BAPICURR-BAPICURR  = ( ZTCIVHD-ZFEXRT / ZTCIVHD-FFACT )
                         * IT_ZSCIVIT-ZFIVAMK.

*  *BAPICURR-BAPICURR  = ZTCIVHD-ZFEXRT * IT_ZSCIVIT-ZFIVAMK.
*  IT_ZSCIVIT-ZFIVAMK = ZTCIVHD-ZFEXRT * IT_ZSCIVIT-ZFIVAMK.

*   SELECT SINGLE * FROM  TCURX
*          WHERE  CURRKEY     = 'KRW'.
*   IF SY-SUBRC NE 0.
*      TCURX-CURRDEC = 2.
*   ENDIF.

*  IF TCURX-CURRDEC NE 0.

*  SELECT SINGLE * FROM  TCURX
*         WHERE  CURRKEY     = 'KRW'.
*  IF SY-SUBRC NE 0.
*     TCURX-CURRDEC = 2.
*  ENDIF.

*  IF TCURX-CURRDEC NE 0.
    PERFORM SET_CURR_CONV_TO_INTERNAL USING
*            IT_ZSCIVIT-ZFIVAMK 'KRW'.
            *BAPICURR-BAPICURR 'KRW'.

    IF *BAPICURR-BAPICURR GT 9999999999999.
      MESSAGE W923 WITH *BAPICURR-BAPICURR.
      IT_ZSCIVIT-ZFIVAMK = 0.
    ELSE.
      IT_ZSCIVIT-ZFIVAMK = *BAPICURR-BAPICURR.
    ENDIF.

*  ENDIF.
    MODIFY  IT_ZSCIVIT  INDEX W_TABIX.
    EXIT.
  ENDIF.

  CLEAR : W_OLD_MENGE, W_MENGE1, W_MENGE2.
* SELECT SINGLE CMENGE ZFPRQN INTO (W_MENGE1, W_MENGE2)
*              FROM   ZTCIVIT
*              WHERE  ZFCIVRN   EQ   ZSCIVIT-ZFCIVRN
*              AND    ZFCIVSQ   EQ   ZSCIVIT-ZFCIVSQ.
  IF NOT ZTCIVHD-ZFCIVRN IS INITIAL.
    SELECT SINGLE CMENGE ZFPRQN INTO (W_MENGE1, W_MENGE2)
                 FROM   ZTCIVIT
                 WHERE  ZFCIVRN   EQ   ZTCIVHD-ZFCIVRN
                 AND    ZFREQNO   EQ   IT_ZSCIVIT-ZFREQNO
                 AND    ZFITMNO   EQ   IT_ZSCIVIT-ZFITMNO.
  ENDIF.

*초과납품 허용치 (%). -> 송장수량.
  IF IT_ZSCIVIT-UEBTO IS INITIAL.   " 초과  납품 허용치 (%)가 미입력시.
    W_OLD_MENGE = ZSCIVIT-MENGE - ( ZSCIVIT-CIVTOT1 - W_MENGE1 ).
  ELSE.                            " 초과납품 허용치 (%)가 입력시.
    W_MENGE3 = ( ( IT_ZSCIVIT-MENGE * ZSCIVIT-UEBTO ) / 100 ).
    W_MENGE4 = W_MENGE3 + IT_ZSCIVIT-CMENGE.   " 초과납품허용+수량..
*     W_OLD_MENGE = ZSCIVIT-MENGE - ( ZSCIVIT-CIVTOT1 - W_MENGE1 )
    W_OLD_MENGE = W_MENGE4 - ( IT_ZSCIVIT-CIVTOT1 - W_MENGE1 )
                + W_MENGE3.
  ENDIF.

  IF IT_ZSCIVIT-CMENGE GT W_OLD_MENGE.
    PERFORM P2000_NO_INPUT  USING  'ZSCIVIT' 'CMENGE'
                                   DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E360 WITH IT_ZSCIVIT-ZFCIVSQ.
  ENDIF.
*초과납품 허용치 (%). --> 물대수량.
  IF IT_ZSCIVIT-UEBTO IS INITIAL.  " 초과  납품 허용치 (%)가 미입력시.
    W_OLD_MENGE = IT_ZSCIVIT-MENGE - ( IT_ZSCIVIT-CIVTOT  - W_MENGE2 ).
  ELSE.                            " 초과납품 허용치 (%)가 입력시.
    W_MENGE1 = W_MENGE3 + IT_ZSCIVIT-ZFPRQN.   " 초과납품허용+수량..
    W_OLD_MENGE = IT_ZSCIVIT-MENGE - ( IT_ZSCIVIT-CIVTOT - W_MENGE2 )
                + W_MENGE3.
  ENDIF.
  IF IT_ZSCIVIT-ZFPRQN GT W_OLD_MENGE.
    PERFORM P2000_NO_INPUT  USING  'ZSCIVIT' 'ZFPRQN'
                                   DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E360 WITH IT_ZSCIVIT-ZFCIVSQ.
  ENDIF.

*  MOVE IT_ZSCIVIT-ZFPRQN     TO     IT_ZSCIVIT-ZFPRQN.
*>> INVOICE 자재별 금액.
  IF IT_ZSCIVIT-ZFIVAMT IS INITIAL.
    IT_ZSCIVIT-ZFIVAMT = ( IT_ZSCIVIT-CMENGE *
                         ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                         ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
  ENDIF.

  IF ZSCIVIT-ZFIVAMP IS INITIAL.
    ZSCIVIT-ZFIVAMP = ( IT_ZSCIVIT-ZFPRQN *
                      ( IT_ZSCIVIT-BPUMZ / IT_ZSCIVIT-BPUMN ) *
                      ( IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH ) ).
  ENDIF.

*  MOVE :  ZSCIVIT-CMENGE  TO     IT_ZSCIVIT-CMENGE,
*          ZSCIVIT-ZFPRQN  TO     IT_ZSCIVIT-ZFPRQN,
*          ZSCIVIT-ZFIVAMT TO     IT_ZSCIVIT-ZFIVAMT,
*          ZSCIVIT-ZFIVAMP TO     IT_ZSCIVIT-ZFIVAMP.

*>> 처리 원화 금액 계산...
  PERFORM SET_CURR_CONV_TO_EXTERNAL USING IT_ZSCIVIT-ZFIVAMP
                                          IT_ZSCIVIT-ZFIVAMC
                                          IT_ZSCIVIT-ZFIVAMK.

  IT_ZSCIVIT-ZFIVAMK = ZTCIVHD-ZFEXRT * IT_ZSCIVIT-ZFIVAMK.

*  SELECT SINGLE * FROM  TCURX
*         WHERE  CURRKEY     = 'KRW'.
*  IF SY-SUBRC NE 0.
*     TCURX-CURRDEC = 2.
*  ENDIF.

*  IF TCURX-CURRDEC NE 0.
  PERFORM SET_CURR_CONV_TO_INTERNAL USING
         IT_ZSCIVIT-ZFIVAMK 'KRW'.
*  ENDIF.

  IF W_OK_CODE EQ 0.
    MODIFY  IT_ZSCIVIT  INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " IT_ZSCIVIT_MENGE_CHECK_SCR3513  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_COMMERCIAL_IV_SCR3600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_COMMERCIAL_IV_SCR3600 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.   ANTWORT   =   'N'.

  IF ZSCIVHD-ZFCIVNO IS INITIAL.    ">Commercial Invoice No. not input.
    IF ZSCIVHD-ZFCIVRN IS INITIAL. ">관리번호가 입력하지 않을 경우.
      MESSAGE E213.
    ELSE.
      PERFORM   P1000_COMMERCIAL_DOC_READ.
    ENDIF.
  ELSE.
    CLEAR : W_COUNT.
    SELECT COUNT( * ) INTO W_COUNT
           FROM ZTCIVHD
           WHERE ZFCIVNO EQ ZSCIVHD-ZFCIVNO.

    CASE W_COUNT.
      WHEN 0.
        MESSAGE   E375 WITH ZSCIVHD-ZFCIVNO.
*          CLEAR ZSCIVHD-ZFCIVNO.
*          PERFORM   P1000_COMMERCIAL_DOC_READ.
        EXIT.
      WHEN 1.
        SELECT ZFCIVRN INTO ZSCIVHD-ZFCIVRN UP TO 1 ROWS
                       FROM ZTCIVHD
                       WHERE ZFCIVNO EQ ZSCIVHD-ZFCIVNO.
        ENDSELECT.
        PERFORM   P1000_COMMERCIAL_DOC_READ.
        EXIT.
      WHEN OTHERS.
        PERFORM P2000_GET_COMMERCIAL_NO.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
        PERFORM   P1000_COMMERCIAL_DOC_READ.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " CHECK_COMMERCIAL_IV_SCR3600  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3600  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3600 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    IF NOT ZTCIVHD-ZFMAVN IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      ZTCIVHD-ZFMAVN
                                             CHANGING  W_LIFNR_NM.
    ENDIF.

    IF NOT ZTCIVHD-ZFOPBN IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      ZTCIVHD-ZFOPBN
                                             CHANGING  W_ZFOPBN_NM.
    ENDIF.

    IF NOT ZTCIVHD-BUKRS IS INITIAL.
      SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTCIVHD-BUKRS.
    ENDIF.

    PERFORM   P2000_IT_ZSCIVIT_UPDATE  USING 'I'.

    CASE SY-DYNNR.
      WHEN '3600' OR '3601'.
        MOVE C_REQ_U TO W_STATUS.
      WHEN '3700' OR '3701'.
        MOVE C_REQ_D TO W_STATUS.
      WHEN '3800' OR '3801'.
        MOVE C_OPEN_U TO W_STATUS.
    ENDCASE.

    SET SCREEN 3510.  LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR3600  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0120 INPUT.
  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0120-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0120_MARK_TC_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0120_MARK_TC_0120 INPUT.

  READ TABLE IT_ZSCIVIT   INDEX TC_0120-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSCIVIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSCIVIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSCIVIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0120_MARK_TC_0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0120 INPUT.
  CASE OK-CODE.
    WHEN 'CIDP' OR 'IMDP'.   " COMMERCIAL INVOICE.
      PERFORM   P2000_LINE_SELECT_CIV_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_CIV_ITEM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BENEFICIARY_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_BENEFICIARY_NAME_SCR0102 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Benificiary
  IF ZTBL-ZFBENI IS INITIAL.
    ZTBL-ZFBENI = ZTBL-LIFNR.
  ENDIF.

  IF ZTBL-ZFFORD IS INITIAL.
    ZTBL-ZFFORD = ZTBL-LIFNR.
  ENDIF.

  PERFORM  P1000_GET_VENDOR   USING      ZTBL-ZFBENI
                              CHANGING   W_ZFBENI_NM.

ENDMODULE.                 " GET_BENEFICIARY_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0812 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0812-CURRENT_LINE + LINE - 1.

  REFRESH : IT_CG_SUM.

ENDMODULE.                 " IT_GET_LINE_SCR0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0812_MARK_TC_0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0812_MARK_TC_0812 INPUT.
*   READ TABLE IT_ZSBLIT  WITH KEY
*                         ZFBLIT = ZSBLIT-ZFBLIT  BINARY SEARCH.
  READ TABLE IT_ZSCGIT   INDEX TC_0812-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSCGIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSCGIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSCGIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0812_MARK_TC_0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0812 INPUT.

  W_OK_CODE = SY-UCOMM.

  IF SY-UCOMM EQ 'MKA1'.   " 전체 선택.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1'.  " 선택 해제.
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'UND1'.       " 취소.
      LOOP AT IT_ZSCGIT WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        MOVE : SPACE    TO   IT_ZSCGIT-CGLOEKZ.   "삭제지시자.
        MODIFY IT_ZSCGIT INDEX W_TABIX.
      ENDLOOP.
    WHEN 'DEL1'.       " 삭제.
*>>>> 후속작업 체크...
      LOOP AT IT_ZSCGIT WHERE ZFMARK NE SPACE.
        W_TABIX = SY-TABIX.
        IF IT_ZSCGIT-ZFCGIT IS INITIAL.
          DELETE IT_ZSCGIT INDEX W_TABIX.
          CONTINUE.
        ENDIF.

*         CLEAR : W_COUNT.
*         SELECT COUNT( * ) INTO W_COUNT
*                FROM   ZTCGIT
*                WHERE  ZFCGNO   EQ   IT_ZSCGIT-ZFCGNO
*                AND    ZFCGIT   EQ   IT_ZSCGIT-ZFCGIT.
*         IF W_COUNT EQ 0.
        MOVE : 'X'          TO   IT_ZSCGIT-CGLOEKZ.   "삭제지시자.
*         ENDIF.
        MODIFY IT_ZSCGIT  INDEX  W_TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해제.
      LOOP AT IT_ZSCGIT.
        IT_ZSCGIT-ZFMARK = W_MARK.   MODIFY IT_ZSCGIT INDEX SY-TABIX.
      ENDLOOP.
*>>> 기능 막음.
*    WHEN 'REF1'.           " 비용코드 Refresh
*      REFRESH : IT_ZSBLIT.
*      IT_ZSBLIT[] = IT_ZSBLIT_ORG[].
    WHEN 'PODP' OR 'IMDP' OR 'BLDP'.   " P/O, 수입의뢰, B/L 조회.
      PERFORM   P2000_LINE_SELECT_CG_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_CG_ITEM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0813  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0813 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0813-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0813  INPUT
*&---------------------------------------------------------------------*
*&      Module  MWSKZ_UPDATE_SCR0813  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MWSKZ_UPDATE_SCR0813 INPUT.
  READ TABLE IT_ZSCGCST  WITH KEY ZFCSQ = ZSCGCST-ZFCSQ.
  W_TABIX = SY-TABIX.

  IF SY-SUBRC EQ 0.
    MOVE : ZSCGCST-MWSKZ  TO   IT_ZSCGCST-MWSKZ.
    PERFORM   P1000_READ_KONP   USING   IT_ZSCGCST-MWSKZ
                                        IT_ZSCGCST-KBETR
                                        IT_ZSCGCST-KONWA.

    MOVE : IT_ZSCGCST-KBETR    TO     ZSCGCST-KBETR,
           IT_ZSCGCST-KONWA    TO     ZSCGCST-KONWA.

    MODIFY   IT_ZSCGCST   INDEX   W_TABIX.
  ENDIF.

ENDMODULE.                 " MWSKZ_UPDATE_SCR0813  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCGCST_UPDATE_SCR0813  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSCGCST_UPDATE_SCR0813 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

* Internal Table Read
  READ TABLE IT_ZSCGCST  WITH KEY ZFCSQ = ZSCGCST-ZFCSQ.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC EQ 0 AND ZSCGCST IS INITIAL.
    EXIT.
  ENDIF.

  IF ZSCGCST-ZFCSCD IS INITIAL.
    PERFORM P2000_NO_INPUT  USING  'ZSCGCST' 'ZFCSCD'
                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E167 WITH 'Expense code'.
  ELSE.
    IF ZSCGCST-ZFCDNM IS INITIAL.
      SELECT SINGLE ZFCDNM INTO ZSCGCST-ZFCDNM FROM ZTIMIMG08
                           WHERE ZFCDTY EQ '007'
                           AND   ZFCD   EQ ZSCGCST-ZFCSCD.
      IF ZSCGCST-ZFCDNM IS INITIAL.
        PERFORM P2000_NO_INPUT  USING  'ZSCGCST' 'ZFCSCD'
                                DFIES-SCRTEXT_M W_SUBRC.
        MESSAGE E433 WITH 'Loading/Unloading expense' ZSCGCST-ZFCSCD.
      ENDIF.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING ZSCGCST   TO IT_ZSCGCST.
  IT_ZSCGCST-ZFKRW = W_KRW.
  IT_ZSCGCST-BUKRS = ZTCGHD-BUKRS.

  IF ZSCGCST-ZFCKAMT IS INITIAL.
    IF W_SY_SUBRC EQ 0.
      MODIFY IT_ZSCGCST  INDEX W_TABIX.
    ELSE.
*        IT_ZSCGCST-ZFCSQ   = ( TC_0813-CURRENT_LINE * 10 ).
      APPEND IT_ZSCGCST.
    ENDIF.
  ENDIF.

  CHECK  NOT ZSCGCST-ZFCKAMT IS INITIAL.
*  IF ZSCGCST-ZFCKAMT IS INITIAL.
*     PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGCST' 'ZFCKAMT'.
*  ENDIF.

* 필수 입력 검증.
  IF IT_ZSCGCST-LIFNR IS INITIAL.
    READ TABLE IT_ZSCGIT INDEX 1.
    IF SY-SUBRC EQ 0.
      IF IT_ZSCGIT-ZFBNARCD IS INITIAL.
        SELECT SINGLE * FROM ZTIMIMG03
               WHERE ZFBNARCD  EQ  IT_ZSCGIT-ZFBNARCD
               AND ( LIFNR     IS  NOT NULL
               OR    LIFNR     EQ  SPACE ).
        IF SY-SUBRC EQ 0.
          IT_ZSCGCST-LIFNR = ZTIMIMG03-LIFNR.
        ELSE.
          PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGCST' 'LIFNR'.
        ENDIF.
      ELSE.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGCST' 'LIFNR'.
      ENDIF.
    ELSE.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGCST' 'LIFNR'.
    ENDIF.
  ENDIF.

  CLEAR LFA1.
  SELECT SINGLE * FROM LFA1
         WHERE LIFNR = IT_ZSCGCST-LIFNR.
  IF SY-SUBRC NE 0.
    PERFORM  P2000_NO_INPUT  USING 'ZSCGCST' 'LIFNR'
                                    DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE E025.
  ENDIF.

  IF IT_ZSCGCST-ZFPAY IS INITIAL.
    IF LFA1-LNRZA IS INITIAL.
      MOVE IT_ZSCGCST-LIFNR  TO IT_ZSCGCST-ZFPAY.   ">지불처.
    ELSE.
      MOVE LFA1-LNRZA        TO IT_ZSCGCST-ZFPAY.         ">지불처.
    ENDIF.
  ENDIF.

  IF IT_ZSCGCST-ZFPAY IS INITIAL.
    PERFORM  NO_INPUT(SAPFMMEX)  USING 'ZSCGCST' 'ZFPAY'.
  ENDIF.

  IF IT_ZSCGCST-ZTERM IS INITIAL.
    SELECT SINGLE ZTERM INTO IT_ZSCGCST-ZTERM        ">Payment Term
          FROM LFB1
          WHERE LIFNR = IT_ZSCGCST-LIFNR
          AND BUKRS   = IT_ZSCGCST-BUKRS.
    IF SY-SUBRC NE 0.
      PERFORM  NO_INPUT(SAPFMMEX)  USING 'ZSCGCST' 'ZTERM'.
    ENDIF.
  ELSE.
    SELECT * FROM T052 UP TO 1 ROWS
           WHERE  ZTERM  EQ  IT_ZSCGCST-ZTERM.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      PERFORM  NO_INPUT(SAPFMMEX)  USING 'ZSCGCST' 'ZTERM'.
    ENDIF.
  ENDIF.

  IF IT_ZSCGCST-ZTERM IS INITIAL.
    PERFORM  NO_INPUT(SAPFMMEX)  USING 'ZSCGCST' 'ZTERM'.
  ENDIF.

  IF NOT ( IT_ZSCGCST-ZFPAY IS INITIAL ).
    SELECT SINGLE * FROM LFA1
           WHERE LIFNR = IT_ZSCGCST-ZFPAY.
    IF SY-SUBRC NE 0.
      MESSAGE E341.
    ENDIF.
  ENDIF.

  IF IT_ZSCGCST-ZFOCDT IS INITIAL.   " 발생일.
    IT_ZSCGCST-ZFOCDT    =    SY-DATUM.
  ENDIF.
  IF IT_ZSCGCST-MWSKZ IS INITIAL.    " TAX CODE
    SELECT SINGLE ZFCD5 INTO IT_ZSCGCST-MWSKZ FROM ZTIMIMG08
     WHERE ZFCDTY EQ '007'
       AND   ZFCD   EQ IT_ZSCGCST-ZFCSCD.

    IF IT_ZSCGCST-MWSKZ IS INITIAL.    " TAX CODE
      PERFORM  NO_INPUT(SAPFMMEX)  USING 'ZSCGCST' 'MWSKZ'.
    ELSE.
      SELECT SINGLE * FROM T007A
             WHERE KALSM EQ 'TAXKR'
             AND   MWSKZ EQ  IT_ZSCGCST-MWSKZ.
      IF SY-SUBRC NE 0.
        PERFORM  P2000_NO_INPUT  USING 'ZSCGCST' 'MWSKZ'
                                        DFIES-SCRTEXT_M W_SUBRC.
        MESSAGE E495 WITH 'TAXKR' IT_ZSCGCST-MWSKZ.
      ENDIF.
    ENDIF.
  ENDIF.
  IF IT_ZSCGCST-WERKS IS INITIAL.       "> PLANT
    IT_ZSCGCST-WERKS  =    ZTCGHD-WERKS.
  ENDIF.
  IF IT_ZSCGCST-WERKS IS INITIAL.       "> PLANT
    PERFORM  NO_INPUT(SAPFMMEX)  USING 'ZSCGCST' 'WERKS'.
  ENDIF.
* ===> TAX RATE
  IF IT_ZSCGCST-MWSKZ IS INITIAL.
    CLEAR : IT_ZSCGCST-KBETR, IT_ZSCGCST-KONWA, IT_ZSCGCST-ZFVAT.
  ELSE.
    PERFORM   P1000_READ_KONP   USING   IT_ZSCGCST-MWSKZ
                                        IT_ZSCGCST-KBETR
                                        IT_ZSCGCST-KONWA.
  ENDIF.

  IF NOT IT_ZSCGCST-ZFCKAMT IS INITIAL.
    W_AMOUNT = IT_ZSCGCST-ZFCKAMT.
*-----------------------------------------------------------------------
* TAX CODE  ===> 부가세 원단위 절사.
    IF IT_ZSCGCST-ZFVAT IS INITIAL.
      IF NOT IT_ZSCGCST-KBETR IS INITIAL.
        W_AMOUNT = W_AMOUNT * IT_ZSCGCST-KBETR / 10.
        COMPUTE W_AMOUNT = TRUNC( W_AMOUNT ).
        W_AMOUNT = W_AMOUNT * 10.
        IT_ZSCGCST-ZFVAT = W_AMOUNT.

        PERFORM    SET_CURR_CONV_TO_INTERNAL
                                   USING IT_ZSCGCST-ZFVAT
                                         'KRW'.
      ENDIF.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------------------
*>> 부대비용 정산후 금액 수정시 CHECK!
  CLEAR W_TOT_AMT.
  IF W_SY_SUBRC EQ 0 AND IT_ZSCGCST-ZFCSTYN = 'X'.
    W_TOT_AMT = IT_ZSCGCST-ZFUPCST.
    IF W_TOT_AMT > 0.
      IF IT_ZSCGCST-ZFCKAMT < W_TOT_AMT.
        MESSAGE E471 WITH IT_ZSCGCST-ZFCSQ.
      ELSEIF IT_ZSCGCST-ZFCKAMT > W_TOT_AMT.
        MESSAGE W472 WITH IT_ZSCGCST-ZFCSQ.
      ENDIF.
    ENDIF.
  ENDIF.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSCGCST  INDEX W_TABIX.
  ELSE.
    IT_ZSCGCST-ZFCSQ   = ( TC_0813-CURRENT_LINE * 10 ).
    APPEND IT_ZSCGCST.
  ENDIF.

ENDMODULE.                 " IT_ZSCGCST_UPDATE_SCR0813  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0813_MARK_TC_0813  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0813_MARK_TC_0813 INPUT.

*   READ TABLE IT_ZSCGCST WITH KEY ZSCGCST(18)  BINARY SEARCH.
  READ TABLE IT_ZSCGCST   INDEX TC_0813-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSCGCST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSCGCST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSCGCST  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0813_MARK_TC_0813  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR0813  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0813 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSCGCST-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSCGCST-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSCGCST-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE S177(06) WITH ZSCGCST-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZSCGCST-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR0813  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0813  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0813 INPUT.

  CLEAR W_CHK_CNT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'.       " 원산지 삭?
      LOOP AT IT_ZSCGCST  WHERE ZFMARK NE SPACE.
        IF IT_ZSCGCST-ZFCSTYN = 'X'.
          MESSAGE E470 WITH IT_ZSCGCST-ZFCSQ.
        ENDIF.
        DELETE IT_ZSCGCST  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'.
      LOOP AT IT_ZSCGCST.
        IT_ZSCGCST-ZFMARK = W_MARK.   MODIFY IT_ZSCGCST.
      ENDLOOP.
    WHEN 'INS1'.           " 삽입.
      IF LINE GT 0.
        CLEAR : IT_ZSCGCST.
        MOVE : 'KRW'              TO   IT_ZSCGCST-ZFKRW,
               'KRW'              TO   IT_ZSCGCST-WAERS.
        INSERT  IT_ZSCGCST INDEX  LINE.
      ELSE.
        MESSAGE S962.
      ENDIF.
    WHEN 'REF1'.           " 원산지 Refresh
      LOOP AT IT_ZSCGCST WHERE BELNR NE SPACE.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      PERFORM    P2000_CHARGE_REFRESH_MSG.
      IF ANTWORT EQ 'Y'.
        PERFORM    P1000_READ_CARGO_CHARGE_RECORD.
      ELSE.
      ENDIF.
    WHEN 'SCHG'.
      LOOP AT IT_ZSCGCST WHERE ZFMARK = 'X'.
        W_TABIX   = SY-TABIX.
        W_CHK_CNT = W_CHK_CNT + 1.
      ENDLOOP.
      IF W_CHK_CNT > 1. MESSAGE S965. EXIT. ENDIF.
      IF W_CHK_CNT = 0. MESSAGE S766. EXIT. ENDIF.

      MOVE-CORRESPONDING IT_ZSCGCST TO ZSCGCST.
      PERFORM P2000_DOC_CHANGE.
      IF ANTWORT = 'Y'.
        MOVE ' ' TO ZSCGCST-BELNR.
        MOVE ' ' TO ZSCGCST-GJAHR.
        MOVE-CORRESPONDING ZSCGCST TO IT_ZSCGCST.
        MODIFY IT_ZSCGCST INDEX W_TABIX.
      ENDIF.
    WHEN 'VEN1' OR 'VEN2' OR 'MKPF'.
      PERFORM   P2000_LINE_SELECT_CARGO.
      PERFORM   P2000_LINE_CALL_TCODE_CARGO.
    WHEN OTHERS.
  ENDCASE.

  PERFORM   P2000_IT_ZSBLCST1_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR0813  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PORT_CHECK_SCR0810  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_PORT_CHECK_SCR0810 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

  IF ZSCGHD-ZFCGPT IS INITIAL.
    MESSAGE E167 WITH 'Loading/Unloading port'.
  ELSE.
    IF ZSCGHD-WERKS IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGHD' 'WERKS'.
    ENDIF.
    SELECT SINGLE PORTT INTO ZSIMIMG08-ZFCDNM
           FROM  ZTIEPORT
           WHERE PORT  EQ ZSCGHD-ZFCGPT
           AND   LAND1 EQ 'KR'.

*     SELECT SINGLE   ZFCDNM INTO ZSIMIMG08-ZFCDNM
*            FROM   ZTIMIMG08
*            WHERE  ZFCDTY    EQ    '002'
*            AND    ZFCD      EQ    ZSCGHD-ZFCGPT.
    IF SY-SUBRC NE 0.
      MESSAGE E387 WITH ZSCGHD-ZFCGPT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_PORT_CHECK_SCR0810  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCGIT_UPDATE_SCR0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSCGIT_UPDATE_SCR0812 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK ZSCGIT-EBELN IS INITIAL.

* Internal Table Read
*  READ TABLE IT_ZSCGIT   WITH KEY ZFCGIT = ZSCGIT-ZFCGIT.
  CLEAR : IT_ZSCGIT.
  READ TABLE IT_ZSCGIT   INDEX TC_0812-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE : ZSCGIT-ZFBLNO  TO IT_ZSCGIT-ZFBLNO,
         ZSCGIT-ZFBLIT  TO IT_ZSCGIT-ZFBLIT.

*>> B/L 번호가 입력되지 않았을 경우.
  IF ZSCGIT-ZFBLNO IS INITIAL.
    MESSAGE W039.    EXIT.
  ELSE.
*>> B/L 번호가 입력되었을 경우.
    SELECT SINGLE * FROM ZTBL
                    WHERE ZFBLNO EQ IT_ZSCGIT-ZFBLNO.
    IF SY-SUBRC NE 0.
      CLEAR : ZSCGIT.
      MESSAGE W038 WITH IT_ZSCGIT-ZFBLNO.
      EXIT.
    ENDIF.
  ENDIF.

*>> B/L ITEM번호가 입력되었을 경우.
  IF NOT IT_ZSCGIT-ZFBLIT IS INITIAL.
*     MOVE-CORRESPONDING ZSCGIT  TO IT_ZSCGIT.
*----> B/L ITEM번호가 입력되었을 경우, 발췌하여 메세지.
    SELECT SINGLE *
           FROM   ZTBLIT
           WHERE ZFBLNO EQ IT_ZSCGIT-ZFBLNO
           AND   ZFBLIT EQ IT_ZSCGIT-ZFBLIT
           AND   BLOEKZ NE 'X'.

    IF SY-SUBRC NE 0.
      CLEAR : ZSCGIT.
      MESSAGE W370 WITH IT_ZSCGIT-ZFBLNO IT_ZSCGIT-ZFBLIT.
      EXIT.
    ENDIF.

    MOVE-CORRESPONDING  ZTBLIT   TO    IT_ZSCGIT.
    MOVE : ZTBLIT-BLMENGE        TO    IT_ZSCGIT-MENGE_BL.

*++++> P/O DATA 조회.
    SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                  WERKS LGORT MATKL
        INTO (IT_ZSCGIT-MENGE_PO, IT_ZSCGIT-UEBTO,
              IT_ZSCGIT-UEBTK,    IT_ZSCGIT-WEPOS,
              IT_ZSCGIT-ELIKZ,    IT_ZSCGIT-LOEKZ,
              IT_ZSCGIT-UNTTO,    IT_ZSCGIT-WERKS,
              IT_ZSCGIT-LGORT,    IT_ZSCGIT-MATKL)
        FROM   EKPO
        WHERE  EBELN   EQ   IT_ZSCGIT-EBELN
        AND    EBELP   EQ   IT_ZSCGIT-EBELP.

    IF SY-SUBRC EQ 0.
      IF IT_ZSCGIT-LOEKZ NE SPACE.
        CLEAR : ZSCGIT.
        MESSAGE W069 WITH IT_ZSCGIT-EBELN IT_ZSCGIT-EBELP.
        EXIT.
      ENDIF.
      IF IT_ZSCGIT-ELIKZ EQ 'X'.
        CLEAR : ZSCGIT.
        MESSAGE W359 WITH IT_ZSCGIT-EBELN IT_ZSCGIT-EBELP.
        EXIT.
      ENDIF.
    ELSE.
      CLEAR : ZSCGIT.
      MESSAGE W071 WITH IT_ZSCGIT-EBELN IT_ZSCGIT-EBELP.
      EXIT.
    ENDIF.

*+++++> 수입의뢰 수량.
    SELECT SINGLE MENGE KBETR KWERT KPEIN KMEIN
           INTO (IT_ZSCGIT-MENGE, IT_ZSCGIT-KBETR,
                 IT_ZSCGIT-KWERT, IT_ZSCGIT-KPEIN,
                 IT_ZSCGIT-KMEIN)
           FROM  ZTREQIT
           WHERE ZFREQNO EQ IT_ZSCGIT-ZFREQNO
           AND   ZFITMNO EQ IT_ZSCGIT-ZFITMNO.

*>>>>> 하역 TOTAL 수량(수입의뢰별).
    SELECT SUM( CGMENGE ) INTO IT_ZSCGIT-ZFCGTOT
           FROM  ZTCGIT
           WHERE ZFBLNO    EQ IT_ZSCGIT-ZFBLNO
           AND   ZFBLIT    EQ IT_ZSCGIT-ZFBLIT
           AND   CGLOEKZ   NE 'X'.


*>>> INVOICE 기본 수?
    IT_ZSCGIT-CGMENGE = IT_ZSCGIT-MENGE_BL - IT_ZSCGIT-ZFCGTOT.
    IT_ZSCGIT-EBELN   = IT_ZSCGIT-EBELN.
    IT_ZSCGIT-EBELP   = IT_ZSCGIT-EBELP.

    IF IT_ZSCGIT-CGMENGE LT 0.
      IT_ZSCGIT-CGMENGE = 0.
    ENDIF.

*> 수입의뢰 SELECT
    SELECT SINGLE * FROM  ZTREQHD
                    WHERE ZFREQNO   EQ   IT_ZSCGIT-ZFREQNO.

    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.

*>> 통화단위 검증..
    IF ZTCGHD-WAERS IS INITIAL.
      ZTCGHD-WAERS = ZTREQHD-WAERS.
    ENDIF.
    IF ZTCGHD-WAERS NE ZTREQHD-WAERS.
      MESSAGE E379
         WITH ZTCGHD-WAERS IT_ZSCGIT-ZFREQNO ZTREQHD-WAERS.
    ENDIF.
*>> Beneficiay 검증.
*        IF ZTCGHD-ZFMAVN IS INITIAL.
*           ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
*        ENDIF.
*        IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
*            MESSAGE E381
*            WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
*        ENDIF.
*>> 회사코드 검증.
    IF ZTCGHD-BUKRS IS INITIAL.
      ZTCGHD-BUKRS  =  ZTREQHD-BUKRS.
    ENDIF.
    IF ZTCGHD-BUKRS NE ZTREQHD-BUKRS.
      MESSAGE E382
         WITH ZTCGHD-BUKRS IT_ZSCGIT-ZFREQNO ZTREQHD-BUKRS.
    ENDIF.

    MOVE : ZTREQHD-ZFOPNNO  TO    IT_ZSCGIT-ZFOPNNO.

*>>>>> 하역 TOTAL 수량(B/L 아이템별, 하역 헤더별).
    SELECT SUM( CGMENGE ) INTO IT_ZSCGIT-CGITTOT
           FROM  ZTCGIT
           WHERE ZFCGNO   EQ ZTCGHD-ZFCGNO
           AND   ZFBLNO   EQ IT_ZSCGIT-ZFBLNO
           AND   ZFBLIT   EQ IT_ZSCGIT-ZFBLIT
           AND   CGLOEKZ  NE 'X'.

    MOVE : ZTBL-ZFGMNO        TO  IT_ZSCGIT-ZFGMNO,
           ZTBL-ZFMSN         TO  IT_ZSCGIT-ZFMSN,
           ZTBL-ZFHSN         TO  IT_ZSCGIT-ZFHSN,
           ZTBL-ZFCGHNO       TO  IT_ZSCGIT-ZFCGHNO.

*>> ITEM LOCKED...(향후)
*        PERFORM P2000_SET_CIVIT_LOCK_ITEM.
*----> 신규 데이타 추가.
    CLEAR : IT_ZSCGIT-CGMENGE.
    APPEND  IT_ZSCGIT.
*     MOVE-CORRESPONDING   IT_ZSCGIT   TO  ZSCGIT.
*        ENDIF.
    EXIT.
  ENDIF.

  MOVE : ZSCGIT-ZFBLNO  TO IT_ZSCGIT-ZFBLNO,
         ZSCGIT-ZFBLIT  TO IT_ZSCGIT-ZFBLIT.
*  MOVE-CORRESPONDING ZSCGIT  TO IT_ZSCGIT.
  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZSBLIT
           FROM  ZTBLIT
           WHERE ZFBLNO EQ ZSCGIT-ZFBLNO
           AND   BLOEKZ NE 'X'.


  IF SY-SUBRC NE 0.
    CLEAR : ZSCGIT.
    MESSAGE W384 WITH IT_ZSCGIT-ZFBLNO.
    EXIT.
  ENDIF.

*-----------------------------------------------------------------------
* 기존에 해당 수입의뢰 아이템이 입력되었을 경우 체크.
*        READ TABLE  IT_ZSCIVIT WITH KEY ZFBLNO = IT_ZSCIVIT-ZFBLNO
*                                        ZFBLIT = IT_ZSCIVIT-ZFBLIT.
*        IF SY-SUBRC EQ 0 AND W_SY_SUBRC NE 0.
*           CLEAR : ZSCIVIT.
*           MESSAGE S369 WITH IT_ZSBLIT-ZFBLNO IT_ZSCIVIT-ZFBLIT
*                             IT_ZSCIVIT-ZFCIVSQ.
*           CONTINUE.
*        ENDIF.
*-----------------------------------------------------------------------
  LOOP AT IT_ZSBLIT.
    MOVE-CORRESPONDING  IT_ZSBLIT  TO  IT_ZSCGIT.
*++++> P/O DATA 조회.
    SELECT SINGLE MENGE UEBTO UEBTK WEPOS ELIKZ LOEKZ UNTTO
                     WERKS LGORT MATKL
              INTO (IT_ZSCGIT-MENGE_PO, IT_ZSCGIT-UEBTO,
                    IT_ZSCGIT-UEBTK,    IT_ZSCGIT-WEPOS,
                    IT_ZSCGIT-ELIKZ,    IT_ZSCGIT-LOEKZ,
                    IT_ZSCGIT-UNTTO,    IT_ZSCGIT-WERKS,
                    IT_ZSCGIT-LGORT,    IT_ZSCGIT-MATKL)
              FROM   EKPO
              WHERE  EBELN   EQ   IT_ZSBLIT-EBELN
              AND    EBELP   EQ   IT_ZSBLIT-EBELP.

    IF SY-SUBRC EQ 0.
      IF IT_ZSCGIT-LOEKZ NE SPACE.
        CLEAR : ZSCGIT.
        MESSAGE W069 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
        CONTINUE.
      ENDIF.

      IF IT_ZSCGIT-ELIKZ EQ 'X'.
        CLEAR : ZSCGIT.
        MESSAGE W359 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
        CONTINUE.
      ENDIF.
    ELSE.
      CLEAR : ZSCGIT.
      MESSAGE W071 WITH IT_ZSBLIT-EBELN IT_ZSBLIT-EBELP.
      CONTINUE.
    ENDIF.
*+++++> 수입의뢰 수량.
    SELECT SINGLE MENGE KBETR KWERT KPEIN KMEIN
        INTO (IT_ZSCGIT-MENGE, IT_ZSCGIT-KBETR,
              IT_ZSCGIT-KWERT, IT_ZSCGIT-KPEIN,
              IT_ZSCGIT-KMEIN)
        FROM  ZTREQIT
        WHERE ZFREQNO EQ IT_ZSBLIT-ZFREQNO
        AND   ZFITMNO EQ IT_ZSBLIT-ZFITMNO.

*>>>>> B/L TOTAL 수량(수입의뢰별).
*        SELECT SUM( BLMENGE ) INTO IT_ZSCIVIT-ZFBLTOT
*            FROM  ZTBLIT
*            WHERE ZFREQNO  EQ IT_ZSCIVIT-ZFREQNO
*            AND   ZFITMNO  EQ IT_ZSCIVIT-ZFITMNO
*            AND   BLOEKZ   NE 'X'.
*>>>> B/L 수량.
    SELECT SINGLE BLMENGE INTO IT_ZSCGIT-MENGE_BL
           FROM  ZTBLIT
           WHERE ZFBLNO   EQ    IT_ZSBLIT-ZFBLNO
           AND   ZFBLIT   EQ    IT_ZSBLIT-ZFBLIT
           AND   BLOEKZ  NE 'X'.


*> 수입의뢰 SELECT
    SELECT SINGLE * FROM  ZTREQHD
                    WHERE ZFREQNO   EQ   IT_ZSBLIT-ZFREQNO.

    IF ZTREQHD-ZFREQTY EQ 'LO' OR ZTREQHD-ZFREQTY EQ 'PU'.
      MESSAGE E317 WITH ZTREQHD-ZFREQNO ZTREQHD-ZFREQTY.
    ENDIF.

*>> 통화단위 검증..
    IF ZTCGHD-WAERS IS INITIAL.
      ZTCGHD-WAERS = ZTREQHD-WAERS.
    ENDIF.
    IF ZTCGHD-WAERS NE ZTREQHD-WAERS.
      MESSAGE E379
      WITH ZTCGHD-WAERS IT_ZSBLIT-ZFREQNO ZTREQHD-WAERS.
    ENDIF.
*>> Beneficiay 검증.
*        IF ZTCIVHD-ZFMAVN IS INITIAL.
*           ZTCIVHD-ZFMAVN  =  ZTREQHD-ZFBENI.
*        ENDIF.
*        IF ZTCIVHD-ZFMAVN NE ZTREQHD-ZFBENI.
*            MESSAGE E381
*            WITH ZTCIVHD-ZFMAVN IT_ZSCIVIT-ZFREQNO ZTREQHD-ZFBENI.
*        ENDIF.
*>> 회사코드 검증.
    IF ZTCGHD-BUKRS IS INITIAL.
      ZTCGHD-BUKRS  =  ZTREQHD-BUKRS.
    ENDIF.
    IF ZTCGHD-BUKRS NE ZTREQHD-BUKRS.
      MESSAGE E382
         WITH ZTCGHD-BUKRS IT_ZSBLIT-ZFREQNO ZTREQHD-BUKRS.
    ENDIF.
    MOVE : ZTREQHD-ZFOPNNO  TO    IT_ZSCGIT-ZFOPNNO.

*>> ITEM LOCKED.
*        PERFORM P2000_SET_CIVIT_LOCK_ITEM.
*>>>>> 하역 TOTAL 수량(수입의뢰별).
    SELECT SUM( CGMENGE ) INTO IT_ZSCGIT-ZFCGTOT
           FROM  ZTCGIT
           WHERE ZFBLNO    EQ IT_ZSBLIT-ZFBLNO
           AND   ZFBLIT    EQ IT_ZSBLIT-ZFBLIT
           AND   CGLOEKZ   NE 'X'.

*>>> INVOICE 기본 수?
*     IT_ZSCGIT-CGMENGE = IT_ZSCGIT-MENGE_BL - IT_ZSCGIT-ZFCGTOT.
*
*     IF IT_ZSCGIT-CGMENGE LT 0.
*        IT_ZSCGIT-CGMENGE = 0.
*     ENDIF.
*>>>>> 하역 TOTAL 수량(B/L 아이템별, 하역 헤더별).
    SELECT SUM( CGMENGE ) INTO IT_ZSCGIT-CGITTOT
           FROM  ZTCGIT
           WHERE ZFCGNO   EQ ZTCGHD-ZFCGNO
           AND   ZFBLNO   EQ IT_ZSCGIT-ZFBLNO
           AND   ZFBLIT   EQ IT_ZSCGIT-ZFBLIT
           AND   CGLOEKZ  NE 'X'.

    MOVE : ZTBL-ZFGMNO        TO  IT_ZSCGIT-ZFGMNO,
           ZTBL-ZFMSN         TO  IT_ZSCGIT-ZFMSN,
           ZTBL-ZFHSN         TO  IT_ZSCGIT-ZFHSN,
           ZTBL-ZFCGHNO       TO  IT_ZSCGIT-ZFCGHNO.

*----> 마지막에 추가.
    CLEAR : IT_ZSCGIT-CGMENGE.
    APPEND IT_ZSCGIT.
  ENDLOOP.
  MOVE-CORRESPONDING   IT_ZSCGIT   TO  ZSCGIT.

ENDMODULE.                 " IT_ZSCGIT_UPDATE_SCR0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_DOC_SCR0820  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_BL_DOC_SCR0820 INPUT.
  IF SY-CALLD EQ 'X'.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.        " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ENDIF.
  ELSE.
*    IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* B/L NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTBL
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
      WHEN 1.
        SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                      FROM ZTBL
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_BL_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
*    ENDIF.
  ENDIF.
* B/L READ PERFORM ?
  ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
  PERFORM   P1000_READ_BL_DOC.


ENDMODULE.                 " READ_BL_DOC_SCR0820  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PORT_CHECK_SCR0820  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_PORT_CHECK_SCR0820 INPUT.

  ANTWORT = 'N'.

  IF SY-CALLD EQ 'X'.
    W_COUNT = 1.
    GET PARAMETER ID 'ZPCGNO' FIELD ZSCGHD-ZFCGNO.
    PERFORM   P1000_READ_CARGO_WORK_DOC.
    EXIT.
  ENDIF.

  PERFORM  P2000_OK_CODE_PROCESS.
  ANTWORT = 'N'.

  IF ZSCGHD-ZFCGPT IS INITIAL.
    W_COUNT = 0.
    SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO
           FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
           ON     H~ZFCGNO   EQ   I~ZFCGNO
           WHERE  I~ZFBLNO   EQ   ZTBL-ZFBLNO
           GROUP BY H~ZFCGNO.
      ADD 1 TO W_COUNT.
    ENDSELECT.
    CASE W_COUNT.
      WHEN 0.
        MESSAGE E393 WITH ZTBL-ZFBLNO.
      WHEN 1.
        SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO UP TO 1 ROWS
               FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
               ON     H~ZFCGNO   EQ   I~ZFCGNO
               WHERE  I~ZFBLNO   EQ   ZTBL-ZFBLNO
               GROUP BY H~ZFCGNO.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_CARGO_DOC_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
    PERFORM   P1000_READ_CARGO_WORK_DOC.
    EXIT.
  ELSE.
    SELECT SINGLE PORTT INTO ZSIMIMG08-ZFCDNM
           FROM  ZTIEPORT
           WHERE PORT  EQ ZSCGHD-ZFCGPT
           AND   LAND1 EQ 'KR'.
*     SELECT SINGLE   ZFCDNM INTO ZSIMIMG08-ZFCDNM
*            FROM   ZTIMIMG08
*            WHERE  ZFCDTY    EQ    '002'
*            AND    ZFCD      EQ    ZSCGHD-ZFCGPT.
    IF SY-SUBRC NE 0.
      MESSAGE E387 WITH ZSCGHD-ZFCGPT.
    ENDIF.
  ENDIF.

  IF SY-TCODE EQ 'ZIM82' OR SY-TCODE EQ 'ZIM83'.
    W_COUNT = 0.
    SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO
           FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
           ON     H~ZFCGNO   EQ   I~ZFCGNO
           WHERE  H~ZFCGPT   EQ   ZSCGHD-ZFCGPT
           AND    I~ZFBLNO   EQ   ZTBL-ZFBLNO
           GROUP BY H~ZFCGNO.
      ADD 1 TO W_COUNT.
    ENDSELECT.

    CASE W_COUNT.
      WHEN 0.
        MESSAGE E392 WITH ZTBL-ZFBLNO ZSCGHD-ZFCGPT.
      WHEN 1.
        SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO UP TO 1 ROWS
               FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
               ON     H~ZFCGNO   EQ   I~ZFCGNO
               WHERE  H~ZFCGPT   EQ   ZSCGHD-ZFCGPT
               AND    I~ZFBLNO   EQ   ZTBL-ZFBLNO.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_CARGO_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
    PERFORM   P1000_READ_CARGO_WORK_DOC.
  ENDIF.

ENDMODULE.                 " CHECK_PORT_CHECK_SCR0820  INPUT
*&---------------------------------------------------------------------*
*&      Module  BNARCD_CHECK_SCR0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BNARCD_CHECK_SCR0812 INPUT.
  CHECK : NOT ZSCGIT-ZFBLNO  IS INITIAL,
          NOT ZSCGIT-CGMENGE IS INITIAL,
*           NOT ZSCGIT-GRMENGE IS INITIAL,
          W_STATUS           NE C_REQ_D.

  READ TABLE IT_ZSCGIT   INDEX TC_0812-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE : ZSCGIT-ZFBNARCD   TO   IT_ZSCGIT-ZFBNARCD,
           ZSCGIT-ZFINDT     TO   IT_ZSCGIT-ZFINDT.    " 반입일자.

    W_TABIX = SY-TABIX.
    IF ZSCGIT-ZFBNARCD IS INITIAL.
      CLEAR : ZSCGIT-ZFBNARM.
*         PERFORM NO_INPUT(SAPFMMEX) USING 'ZSCGIT' 'ZFBNARCD'.
      MESSAGE E167 WITH 'Bonded area code'.
    ENDIF.

    SELECT ZFBNARM INTO IT_ZSCGIT-ZFBNARM UP TO 1 ROWS
                   FROM  ZTIMIMG03
                   WHERE ZFBNARCD EQ ZSCGIT-ZFBNARCD.

    ENDSELECT.
    IF SY-SUBRC NE 0.
      PERFORM P2000_NO_INPUT USING 'ZSCGIT' 'ZFBNARCD'
                                   DFIES-SCRTEXT_M W_SUBRC.
      MESSAGE E245 WITH ZSCGIT-ZFBNARCD.
    ENDIF.
    MODIFY IT_ZSCGIT  INDEX   W_TABIX.
  ENDIF.

ENDMODULE.                 " BNARCD_CHECK_SCR0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSCGIT_MENGE_CHECK_SCR0812  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ZSCGIT_MENGE_CHECK_SCR0812 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  CHECK NOT ZSCGIT-ZFBLIT  IS INITIAL.  " B/L 가 입력되지 않았을 경우.
  IF NOT ZSCGIT-GRMENGE IS INITIAL.
    MOVE-CORRESPONDING   ZSCGIT   TO     IT_CG_SUM.
    COLLECT   IT_CG_SUM.
    EXIT.
  ENDIF.
  IF NOT ZSCGIT-CCMENGE IS INITIAL.
    MOVE-CORRESPONDING   ZSCGIT   TO     IT_CG_SUM.
    COLLECT   IT_CG_SUM.
    EXIT.
  ENDIF.

  CHECK NOT ZSCGIT-CGMENGE IS INITIAL.  " P/O 가 입력되지 않았을 경우.
*  CHECK ZSCGIT-CCMENGE IS INITIAL.  " P/O 가 입력되지 않았을 경우.

  CLEAR : IT_ZSCGIT.
  READ TABLE IT_ZSCGIT   INDEX TC_0812-CURRENT_LINE.
  W_TABIX = SY-TABIX.
  W_SUBRC = SY-SUBRC.
  CHECK : W_SUBRC EQ 0.

*>> 허용된 무제한초과납품.
  IF ZSCGIT-UEBTK EQ 'X'.          " 지시자: 허용된 무제한초과납품.
    MOVE ZSCGIT-CGMENGE  TO     IT_ZSCGIT-CGMENGE.
    MODIFY  IT_ZSCGIT  INDEX W_TABIX.
    MOVE-CORRESPONDING   IT_ZSCGIT   TO     IT_CG_SUM.
    COLLECT   IT_CG_SUM.
    EXIT.
  ENDIF.
*-----------------------------------------------------------------------
**>> 수량체크 로직 ..
*-----------------------------------------------------------------------
  CLEAR : IT_CG_SUM.
  READ TABLE IT_CG_SUM  WITH KEY ZFBLNO  =  ZSCGIT-ZFBLNO
                                 ZFBLIT  =  ZSCGIT-ZFBLIT.

  CLEAR : IT_ZSCGIT_ORG.
  READ TABLE IT_ZSCGIT_ORG WITH KEY ZFCGNO  =  ZSCGIT-ZFCGNO
                                    ZFCGIT  =  ZSCGIT-ZFCGIT.

*초과납품 허용치 (%).
  IF ZSCGIT-UEBTO IS INITIAL.      " 초과납품 허용치 (%)가 미입력시.
*-----------------------------------------------------------------------
*>> B/L ITEM 수량 - ( B/L ITEM별 SUM - B/L ITEM별 하역 HEADER별 SUM )
*>>               - 현재 문서의 입력 SUM
*-----------------------------------------------------------------------
*     IF W_STATUS EQ C_REQ_C.
    W_MENGE = ZSCGIT-MENGE_BL
          - ( IT_ZSCGIT-ZFCGTOT - IT_ZSCGIT-CGITTOT )
            - IT_CG_SUM-CGMENGE.
*     ELSE.
*        W_MENGE = ZSCGIT-MENGE_BL
*              - ( IT_ZSCGIT-ZFCGTOT - IT_ZSCGIT-CGITTOT )
*                - IT_CG_SUM-CGMENGE + ZSCGIT-CGMENGE.
*     ENDIF.

    IF ZSCGIT-CGMENGE GT W_MENGE.
      PERFORM P2000_NO_INPUT USING 'ZSCGIT' 'CGMENGE'
                                    DFIES-SCRTEXT_M W_OK_CODE.
      W_MENGE = ( IT_ZSCGIT-ZFCGTOT - IT_ZSCGIT-CGITTOT )
              + IT_CG_SUM-CGMENGE + ZSCGIT-CGMENGE.
      WRITE : W_MENGE         TO W_TEXT_18  UNIT ZSCGIT-MEINS,
              ZSCGIT-MENGE_BL TO W_TEXT1_18 UNIT ZSCGIT-MEINS.
      MESSAGE E516 WITH ZSCGIT-ZFCGIT W_TEXT_18 W_TEXT1_18.
    ENDIF.

    MOVE ZSCGIT-CGMENGE  TO     IT_ZSCGIT-CGMENGE.
    IF W_SUBRC EQ 0.
      MODIFY  IT_ZSCGIT  INDEX W_TABIX.

      MOVE-CORRESPONDING   IT_ZSCGIT   TO     IT_CG_SUM.
      COLLECT   IT_CG_SUM.
    ENDIF.
  ENDIF.
*초과납품 허용치 (%).
  IF NOT ZSCGIT-UEBTO IS INITIAL.      " 초과납품 허용치 (%)가 미입력시.
    W_MENGE1 = ( ( ZSCGIT-MENGE_PO * ZSCGIT-UEBTO ) / 100 ).
*    W_MENGE = ZSCGIT-MENGE_BL
    W_MENGE = ZSCGIT-MENGE_PO
          - ( IT_ZSCGIT-ZFCGTOT -  IT_ZSCGIT-CGITTOT )
            - IT_CG_SUM-CGMENGE.
    W_MENGE = W_MENGE + W_MENGE1.

    IF ZSCGIT-CGMENGE GT W_MENGE.
      PERFORM P2000_NO_INPUT USING 'ZSCGIT' 'CGMENGE'
                                    DFIES-SCRTEXT_M W_OK_CODE.
      W_MENGE = ( IT_ZSCGIT-ZFCGTOT - IT_ZSCGIT-CGITTOT )
              + IT_CG_SUM-CGMENGE + ZSCGIT-CGMENGE.
      W_MENGE1 = ZSCGIT-MENGE_PO + W_MENGE1.
      WRITE : W_MENGE   TO   W_TEXT_18   UNIT ZSCGIT-MEINS,
              W_MENGE1  TO   W_TEXT1_18  UNIT ZSCGIT-MEINS.

      MESSAGE E563 WITH ZSCGIT-ZFCGIT W_TEXT_18 W_TEXT1_18.
    ENDIF.

    MOVE ZSCGIT-CGMENGE  TO     IT_ZSCGIT-CGMENGE.
    IF W_SUBRC EQ 0.
      MODIFY  IT_ZSCGIT  INDEX W_TABIX.

      MOVE-CORRESPONDING   IT_ZSCGIT   TO     IT_CG_SUM.
      COLLECT   IT_CG_SUM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " IT_ZSCGIT_MENGE_CHECK_SCR0812  INPUT
*&---------------------------------------------------------------------*
*&      Module  MATER_SHIP_NO_CHECK_SCR0814  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MATER_SHIP_NO_CHECK_SCR0814 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

*> 모선관리번호.
  IF ZTCGHD-ZFMSNO IS INITIAL.
    MESSAGE E167 WITH 'MOthership management No'.
    CLEAR : ZTMSHD.
  ELSE.
    SELECT SINGLE * FROM ZTMSHD
           WHERE ZFMSNO EQ ZTCGHD-ZFMSNO.
    IF SY-SUBRC EQ 0.
      IF ZTCGHD-BUKRS IS INITIAL.
        ZTCGHD-BUKRS  =   ZTMSHD-BUKRS.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " MATER_SHIP_NO_CHECK_SCR0814  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_MATCH_CHECK_SCR3100  INPUT
*&---------------------------------------------------------------------*
MODULE FIELD_MATCH_CHECK_SCR3100 INPUT.

  PERFORM  P2000_OK_CODE_PROCESS.

*>> Customs Clearance Type
  IF ZSIV-ZFCLCD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIV' 'ZFCLCD'.
  ELSE.
    IF ZSIV-ZFCLCD EQ 'B'.
      MESSAGE E405 WITH 'Taxable clearance'.
    ENDIF.
  ENDIF.
  ZTIV-ZFCLCD = ZSIV-ZFCLCD.

*>> The Mark of lastest G/R
  IF NOT ZSIV-ZFLGRST IS INITIAL.
    MESSAGE I402.
  ENDIF.
*>> Transfer/Negotiation
  IF ZSIV-ZFTRIPLE   IS INITIAL.
    CLEAR : ZTBLINOU.
    SELECT SINGLE * FROM ZTBLINOU
           WHERE ZFBLNO   EQ   ZTBL-ZFBLNO
           AND   ZFBTSEQ  EQ
                 ( SELECT MAX( ZFBTSEQ )
                          FROM ZTBLINOU
                          WHERE ZFBLNO EQ ZTBL-ZFBLNO ).

    IF ZSIV-ZFCLCD EQ 'A'.
      IF SY-SUBRC NE 0.
        MESSAGE E445 WITH ZTBL-ZFBLNO.
      ELSE.
        IF ZTBLINOU-ZFBINYN NE 'X'.
          MESSAGE W446 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
        ELSE.
          SELECT SINGLE * FROM ZTBLINR
                 WHERE ZFBLNO  EQ   ZTBLINOU-ZFBLNO
                 AND   ZFBTSEQ EQ   ZTBLINOU-ZFBTSEQ.
          IF ZTBLINR-ZFDOCST NE 'O'.
            MESSAGE W447 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
          ENDIF.
        ENDIF.
        IF ZTBLINOU-ZFBOUYN EQ 'X'.
          MESSAGE W448 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
        ENDIF.
      ENDIF.
    ELSEIF ZSIV-ZFCLCD EQ 'C'.
      IF SY-SUBRC EQ 0.
        MESSAGE E444 WITH ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM  P2000_REF_REQ_DOC_INPUT.
  ENDIF.

ENDMODULE.                 " FIELD_MATCH_CHECK_SCR3100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_BL_DOCUMENT_SCR3118  INPUT
*&---------------------------------------------------------------------*
MODULE GET_BL_DOCUMENT_SCR3118 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC' OR 'NO'.   EXIT.
    WHEN OTHERS.
  ENDCASE.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.        " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ENDIF.
  ELSE.
*    IF ZSREQHD-ZFBLNO  IS INITIAL.     " 관리번호가 입력하지 않을 경우.
* B/L NO에 Count
    SELECT COUNT( * ) INTO  W_COUNT
                      FROM  ZTBL
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
    CASE W_COUNT.
      WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
      WHEN 1.
        SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
                      FROM ZTBL
                      WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
          EXIT.
        ENDSELECT.
      WHEN OTHERS.
        PERFORM P2000_BL_DOC_ITEM_SELECT.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        PERFORM P2000_SEARCH_FIELD_MOVE.
    ENDCASE.
*    ENDIF.
  ENDIF.

* B/L READ PERFORM ?
  ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
  PERFORM   P1000_READ_BL_DOC.

*>> B/L 납품 완료지시자.
  IF ZTBL-ZFELIKZ EQ 'X'.
    MESSAGE W404 WITH ZTBL-ZFBLNO.
  ENDIF.

*>> 양도/양수 구분자..
  IF NOT ZSIV-ZFYSDST IS INITIAL.
    IF ZSIV-ZFYSDST EQ 'L'.     "> 양도.
      IF ZTBL-ZFRENT NE 'X'.   "> B/L 양도 구분자.
        MESSAGE E503 WITH ZTBL-ZFBLNO 'Transfer'.
      ENDIF.
    ELSE.
      IF ZTBL-ZFPOTY NE 'R'.
        MESSAGE E503 WITH ZTBL-ZFBLNO 'Acquisition by transfer'.
      ENDIF.
    ENDIF.
    PERFORM  P2000_BL_TO_CC_DATA.   ">> B/L DATA ---> CC DATA.
    EXIT.
  ENDIF.
  IF NOT ( ZTBL-ZFVIA EQ 'VSL' AND ZTBL-ZFSHTY EQ 'B' ). "> Bulk
    PERFORM  P2000_BL_TO_CC_DATA.
    EXIT.
  ENDIF.

*>> 수입 IMG CONFIG.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFCGYN NE 'X'.
    PERFORM  P2000_BL_TO_CC_DATA.
    EXIT.
  ENDIF.

  ANTWORT = 'N'.
  W_COUNT = 0.
  SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO
         FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
         ON     H~ZFCGNO   EQ   I~ZFCGNO
         WHERE  I~ZFBLNO   EQ   ZTBL-ZFBLNO
         GROUP BY H~ZFCGNO.
    ADD 1 TO W_COUNT.
  ENDSELECT.
  CASE W_COUNT.
*>>>>>>>> B/L DATA ---> CC DATA.
    WHEN 0.
      IF ZTBL-ZFVIA EQ 'VSL' AND ZTBL-ZFSHTY EQ 'B'. "> Bulk
        MESSAGE  E409 WITH ZTBL-ZFBLNO.
      ELSE.
        PERFORM  P2000_BL_TO_CC_DATA.
      ENDIF.
      EXIT.
    WHEN 1.
      SELECT H~ZFCGNO INTO ZSCGHD-ZFCGNO UP TO 1 ROWS
             FROM   ZTCGHD AS H  INNER JOIN ZTCGIT AS I
             ON     H~ZFCGNO   EQ   I~ZFCGNO
             WHERE  I~ZFBLNO   EQ   ZTBL-ZFBLNO
             GROUP BY H~ZFCGNO.
      ENDSELECT.
    WHEN OTHERS.
      PERFORM P2000_CARGO_DOC_SELECT.
      IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
      PERFORM P2000_SEARCH_FIELD_MOVE.
  ENDCASE.

  PERFORM   P1000_READ_CARGO_WORK_DOC.
*>>>>>>>> CARGO WORK DATA ---> CC DATA.
  PERFORM   P2000_CG_TO_CC_DATA.

*  REFRESH : IT_ZFREQNO, IT_LOCKED.
*  PERFORM  P2000_BL_DATA_MOVE.            " B/L DATA MOVE.

ENDMODULE.                 " GET_BL_DOCUMENT_SCR3118  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR3519  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR3519 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_3519-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR3519  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3519_MARK_TC_3519  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3519_MARK_TC_3519 INPUT.

  READ TABLE IT_ZSCIVHST  WITH KEY
                      ZFCIVHST = ZSCIVHST-ZFCIVHST  BINARY SEARCH.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSCIVHST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSCIVHST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSCIVHST INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3519_MARK_TC_3519  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3519  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3519 INPUT.
  W_OK_CODE = SY-UCOMM.

  CASE OK-CODE.
    WHEN 'DIMI' OR 'DIFB' OR 'DIM2'. "P/O,수입의뢰 조회>후속문서 조회.
      PERFORM   P2000_LINE_SELECT_CIVHST_ITEM.
      PERFORM   P2000_LINE_CALL_TCODE_CIV_ITEM.
      EXIT.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR3519  INPUT
*&---------------------------------------------------------------------*
*&      Module  OPEN_BANK_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OPEN_BANK_CHECK_SCR3510 INPUT.
  CHECK W_STATUS NE C_REQ_D.

  READ TABLE IT_ZSCIVIT INDEX 1.
  SELECT SINGLE * FROM ZTREQHD
  WHERE  ZFREQNO  EQ   IT_ZSCIVIT-ZFREQNO.
  MOVE ZTREQHD-ZFREQTY  TO  ZTCIVHD-ZFREQTY.

  IF ZTCIVHD-ZFREQTY NE 'TT'.
    IF ZTCIVHD-ZFOPBN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHD' 'ZFOPBN'.
    ELSE.
      PERFORM  P1000_GET_VENDOR   USING      ZTCIVHD-ZFOPBN
                                             CHANGING  W_ZFOPBN_NM.
    ENDIF.
  ENDIF.

ENDMODULE.                 " OPEN_BANK_CHECK_SCR3510  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFPHVN_CODE_CHECK_SCR3110  INPUT
*&---------------------------------------------------------------------*
MODULE ZFPHVN_CODE_CHECK_SCR3110 INPUT.

  PERFORM  P1000_GET_VENDOR   USING      ZTIV-ZFPHVN
                                         CHANGING  W_ZFOPBN_NM.

  CHECK  ZTIV-ZFREQTY  NE 'LO'.
  CHECK  ZTIV-ZFREQTY  NE 'PU'.

  PERFORM  P1000_GET_CUT      USING      ZTIV-ZFCUT
                                        CHANGING  ZSIMIMG10-NAME1.

ENDMODULE.                 " ZFPHVN_CODE_CHECK_SCR3110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3113  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3113 INPUT.

  CASE SY-UCOMM.
    WHEN 'IVCD'.           ">비용배부 세부선택화면.
      IF ZTIV-ZFCDST NE 'Y'.
        MESSAGE S425.
        EXIT.
      ELSE.
        REFRESH : IT_ZSIVCD_TMP.
        IT_ZSIVIT_TMP[] = IT_ZSIVIT[].
      ENDIF.

      SPOP-TITEL = '비용배부 세부내역'.
      OPTION = 1.

      CALL SCREEN 3117 STARTING AT 01   1
                       ENDING   AT 110 18.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR3113  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC3114_SCR3114_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC3114_SCR3114_1 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_3114_1-CURRENT_LINE GT TC_3114_1-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.

  READ TABLE IT_ZSIVHST1   INDEX TC_3114_1-CURRENT_LINE.
  IF SY-SUBRC = 0.                                     " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHST1  TO ZSIVHST1.          " DATA MOVE
    MOVE IT_ZSIVHST1-ZFMARK         TO W_ROW_MARK1.      " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC3114_SCR3114_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3114_MARK_TC_3114  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3114_MARK_TC_3114 INPUT.

  READ TABLE IT_ZSIVHST   INDEX TC_3114-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVHST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVHST INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3114_MARK_TC_3114  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR3114_1_MARK_TC_3114_1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR3114_1_MARK_TC_3114_1 INPUT.

  READ TABLE IT_ZSIVHST1   INDEX TC_3114_1-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSIVHST1-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHST1-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVHST1 INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR3114_1_MARK_TC_3114_1  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR3114  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR3114 INPUT.
  W_OK_CODE = SY-UCOMM.

  CASE OK-CODE.
    WHEN 'CGDP' OR 'CGDP2'.   " 입고/취소 조회.
      CLEAR : ZSIVHST.
      W_COUNT = 0.
      LOOP AT IT_ZSIVHST WHERE ZFMARK = 'X'.
        W_COUNT = W_COUNT + 1.
        MOVE-CORRESPONDING  IT_ZSIVHST  TO ZSIVHST.
      ENDLOOP.

      PERFORM   P2000_LINE_CALL_TCODE_CIV_ITEM.
    WHEN 'CGDP1'.                           " 제비용처리문서.
      CLEAR : ZSIVHST1.
      W_COUNT = 0.
      LOOP AT IT_ZSIVHST1 WHERE ZFMARK = 'X'.
        W_COUNT = W_COUNT + 1.
        MOVE-CORRESPONDING  IT_ZSIVHST1  TO ZSIVHST1.
      ENDLOOP.
      PERFORM   P2000_LINE_CALL_TCODE_CIV_ITEM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR3114  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3117_UPDATE_SCR3117  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_3117_UPDATE_SCR3117 INPUT.

  CHECK SY-UCOMM NE 'ENTR'.

  READ TABLE IT_ZSIVIT_TMP INDEX TC_3117-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVIT_TMP-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVIT_TMP-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVIT_TMP INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " TC_3117_UPDATE_SCR3117  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3117_1_UPDATE_SCR3117  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_3117_1_UPDATE_SCR3117 INPUT.
  CHECK SY-UCOMM NE 'ENTR'.

  READ TABLE IT_ZSIVCD_TMP INDEX TC_3117_1-CURRENT_LINE.

  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSIVCD_TMP-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVCD_TMP-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVCD_TMP INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " TC_3117_1_UPDATE_SCR3117  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR3117  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR3117 INPUT.
* CLEAR : ZSCIVHD-ZFBLNO.

  CASE OK-CODE.
*    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'Y'.
    WHEN 'MORT'.
      ANTWORT = 'N'.       ">MOVE...
      READ TABLE IT_ZSIVIT_TMP WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC NE 0.
        MESSAGE S962.
      ELSE.
        REFRESH : IT_ZSIVCD_TMP.
        LOOP AT IT_ZSIVCD WHERE ZFIVDNO EQ IT_ZSIVIT_TMP-ZFIVDNO.
          MOVE-CORRESPONDING  IT_ZSIVCD  TO IT_ZSIVCD_TMP.
          APPEND   IT_ZSIVCD_TMP.
        ENDLOOP.
        IF SY-SUBRC NE 0.
          MESSAGE S424.
        ENDIF.
      ENDIF.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  IF ANTWORT EQ 'Y'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR3117  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ITEM_NO_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_ITEM_NO_CHECK INPUT.
  IF SY-UCOMM EQ 'ENTR'.
    CHECK NOT ZTIVCD-ZFIVDNO IS INITIAL.
    READ TABLE IT_ZSIVIT_TMP WITH KEY ZFIVDNO = ZTIVCD-ZFIVDNO.
    W_TABIX = SY-TABIX.
    IF SY-SUBRC NE 0.
      CLEAR : ZTIVCD-ZFIVDNO.
      MESSAGE E982 WITH ZTIVCD-ZFIVDNO.
    ELSE.
      CLEAR IT_ZSIVIT_TMP-ZFMARK.
      MODIFY IT_ZSIVIT_TMP TRANSPORTING ZFMARK WHERE ZFMARK = 'X'.
      READ TABLE IT_ZSIVIT_TMP INDEX W_TABIX.
      IT_ZSIVIT_TMP-ZFMARK = 'X'.
      MODIFY IT_ZSIVIT_TMP INDEX  W_TABIX.

      REFRESH : IT_ZSIVCD_TMP.
      LOOP AT IT_ZSIVCD WHERE ZFIVDNO EQ ZTIVCD-ZFIVDNO.
        MOVE-CORRESPONDING  IT_ZSIVCD  TO IT_ZSIVCD_TMP.
        APPEND   IT_ZSIVCD_TMP.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        MESSAGE S424.
      ENDIF.
      CLEAR : ZTIVCD-ZFIVDNO.
    ENDIF.
  ENDIF.

ENDMODULE.                 " IT_ITEM_NO_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0015_LIST_CHECK_SCR0015  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0015_LIST_CHECK_SCR0015 INPUT.
  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      IF SY-LANGU EQ '3'.
        WRITE : / SY-ULINE(107), / SY-VLINE NO-GAP,
                  '유형'   NO-GAP, SY-VLINE NO-GAP,
                  'Doc.관리No' NO-GAP, SY-VLINE NO-GAP,
                  '메세지 텍스트', 105 SY-VLINE NO-GAP,
                  'T'      NO-GAP, SY-VLINE,
                / SY-ULINE(107).
      ELSE.
        WRITE : / SY-ULINE(107), / SY-VLINE NO-GAP,
                  'Type'   NO-GAP, SY-VLINE NO-GAP,
                  ' Doc. No  ' NO-GAP, SY-VLINE NO-GAP,
                  'Message Text ', 105 SY-VLINE NO-GAP,
                  'T'      NO-GAP, SY-VLINE,
                / SY-ULINE(107).
      ENDIF.
      LOOP AT IT_ERR_LIST.
        W_MOD  =  SY-TABIX MOD 2.
        FORMAT RESET.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-ZFIVNO  NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
                  SY-VLINE NO-GAP.

        CASE IT_ERR_LIST-MSGTYP.
          WHEN 'E'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
          WHEN 'W'.
            FORMAT COLOR COL_KEY      INTENSIFIED OFF.
          WHEN 'I'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
          WHEN 'S'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
        ENDCASE.

        WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
        HIDE:IT_ERR_LIST.
      ENDLOOP.
      WRITE : / SY-ULINE(107).
      CLEAR : IT_ERR_LIST.

*>> Display Container List Information..
    WHEN 'CONTDISP'.

      WRITE: 60 '[Container Information]'
           COLOR COL_HEADING INTENSIFIED ON.
      FORMAT RESET.
      SKIP.
      DATA: LL_L TYPE I VALUE 140.

      WRITE: SY-ULINE(35).
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / SY-VLINE NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
           (16)'B/L No.'       NO-GAP,    SY-VLINE NO-GAP,
           (16)'P/O No.'       NO-GAP,    SY-VLINE NO-GAP.
      WRITE: / SY-ULINE(35).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      WRITE: / SY-VLINE NO-GAP,
               IT_CONTLST-ZFBLNO NO-GAP, ' '      NO-GAP,
               IT_CONTLST-ZFBLIT NO-GAP, SY-VLINE NO-GAP,
               IT_CONTLST-EBELN  NO-GAP, ' '      NO-GAP,
               IT_CONTLST-EBELP  NO-GAP, SY-VLINE NO-GAP.
      HIDE IT_CONTLST.
      WRITE: / SY-ULINE(35).

*>> Table Header Text..
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: SY-ULINE(LL_L).
      WRITE: / SY-VLINE          NO-GAP,
           (20)'Container No.'   NO-GAP,  SY-VLINE NO-GAP,
           (15)'Seal No.'        NO-GAP,  SY-VLINE NO-GAP,
           (18)'Case No.'        NO-GAP,  SY-VLINE NO-GAP,
           (10)'Inbd.Deliv'      NO-GAP,  SY-VLINE NO-GAP,
           (18)'Material Number' NO-GAP,  SY-VLINE NO-GAP,
           (30)'Description'     NO-GAP,  SY-VLINE NO-GAP,
           (21)'Quantities'      NO-GAP,  SY-VLINE NO-GAP.

      WRITE / SY-ULINE(LL_L).
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      LOOP AT IT_CONTLST.
        WRITE: / SY-VLINE NO-GAP,
                 IT_CONTLST-TRAID     NO-GAP, SY-VLINE NO-GAP,
             (15)IT_CONTLST-BORGR_GRP NO-GAP, SY-VLINE NO-GAP,
             (18)IT_CONTLST-KDMAT     NO-GAP, SY-VLINE NO-GAP,
                 IT_CONTLST-VBELN     NO-GAP, SY-VLINE NO-GAP,
             (18)IT_CONTLST-MATNR     NO-GAP, SY-VLINE NO-GAP,
             (30)IT_CONTLST-TXZ01     NO-GAP, SY-VLINE NO-GAP,
                 IT_CONTLST-LFIMG     UNIT IT_CONTLST-VRKME,
                 IT_CONTLST-VRKME     NO-GAP, SY-VLINE NO-GAP.
      ENDLOOP.
      WRITE / SY-ULINE(LL_L).
      FORMAT RESET.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " D0015_LIST_CHECK_SCR0015  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ZSBLIT_UPDATE_SCR0111  INPUT
*&---------------------------------------------------------------------*
MODULE IT_ZSBLIT_UPDATE_SCR0111 INPUT.
* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  READ TABLE IT_ZSBLIT   INDEX TC_0111-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.


*>> MATERIAL CODE CHECK.
  IF ZSBLIT-MATNR EQ SPACE AND ZSBLIT-TXZ01 EQ SPACE.
    MESSAGE S083(ME) WITH 'Material code' 'Text'.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'MATNR'.
  ENDIF.

*>> PLANT CHECK.
  IF ZSBLIT-WERKS EQ SPACE.
    MESSAGE S083(ME) WITH 'Plant' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'WERKS'.
  ELSE.
    TRTYP = 'V'.       ">Transaction Type.
    PERFORM WERKS(SAPFMMEX) USING ZSBLIT-WERKS SPACE  'X' TRTYP
                            CHANGING T001W T001K T001.
  ENDIF.
*>> VENDOR별 자재 체크.
  IF ZSBLIT-MATNR NE SPACE.
    CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_00'
         EXPORTING
              I_LFA1_LIFNR     = ZTBL-LIFNR
              I_LFM1_EKORG     = ZTBL-EKORG
*               i_lfm2_ltsnr     = SPACE
              I_LFM2_WERKS     = ZSBLIT-WERKS
              I_DATA           = 'X'
              I_PARTNER        = ' '
         IMPORTING
              A_LFM2           = LFM2
         EXCEPTIONS
              VENDOR_NOT_FOUND = 01.
    IF SY-SUBRC NE 0.
      MESSAGE E027(06) WITH ZTBL-LIFNR ZTBL-EKORG.
*        PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'MATNR'.
    ENDIF.
*      PERFORM MATERIALNUMMER_SETZEN(SAPMM06E)
*              USING  ZSBLIT-MATNR

    CALL FUNCTION 'MEX_CHECK_COMPANY_CODE_ALLOWED'
         EXPORTING
              IM_HEADER_BUKRS   = ZTBL-BUKRS
              IM_ITEM_BUKRS     = T001-BUKRS
              IM_BSTYP          = 'F'
              IM_PURCHORG_BUKRS = T024E-BUKRS.

    CALL FUNCTION 'VENDOR_MASTER_DATA_SELECT_01'
         EXPORTING
              I_LAND1     = T001W-LAND1
              I_ZONE1     = T001W-ZONE1
              I_LIFNR     = ZTBL-LIFNR
         EXCEPTIONS
              NOT_ALLOWED = 01.
    IF SY-SUBRC NE 0.
      MESSAGE W473(06) WITH ZTBL-LIFNR T001W-ZONE1 T001W-LAND1.
    ENDIF.

    CLEAR: MTCOM, MT06E, *MT06E, MT06B.
    MTCOM-KENNG = 'MT06E'.
    MTCOM-MATNR = ZSBLIT-MATNR.
    MTCOM-WERKS = ZSBLIT-WERKS.
    MTCOM-SPRAS = T001W-SPRAS.
    MTCOM-ALAND = ZTBL-ZFCARC.
    MTCOM-PSTAT = 'EBD'.
    MTCOM-KZSPR = 'X'.
    MTCOM-SPR_MEINS = 'X'.
    MTCOM-KZMPN = 'X'.
    MTCOM-XVKBW = T001K-XVKBW.
*     mtcom-lifnr = ZTBL-lifnr.
    IF TCURM IS INITIAL.               "Konsi
      SELECT SINGLE * FROM TCURM.     "Konsi
    ENDIF.
    PERFORM LESEN_MATERIAL_NEU(SAPFMMEX) USING MTCOM 'F'
                                               MT06E MTCOR.
    IF ZSBLIT-TXZ01 IS INITIAL.
      MOVE : MT06E-MAKTX      TO     ZSBLIT-TXZ01.
    ENDIF.
*     IF ZSBLIT-MEINS IS INITIAL.
    MOVE MT06E-MEINS      TO     ZSBLIT-MEINS.
*     ENDIF.
*     IF ZSBLIT-PEINH IS INITIAL.
    MOVE MT06E-PEINH      TO     ZSBLIT-PEINH.
*     ENDIF.
    IF ZSBLIT-MATKL IS INITIAL.
      MOVE MT06E-MATKL      TO     ZSBLIT-MATKL.
    ELSE.
      IF ZSBLIT-MATKL NE MT06E-MATKL.
        MESSAGE E531 WITH ZSBLIT-MATNR MT06E-MATKL ZSBLIT-MATKL.
      ENDIF.
    ENDIF.

    IF ZSBLIT-STAWN IS INITIAL.
      MOVE MT06E-STAWN      TO     ZSBLIT-STAWN.
    ENDIF.
*<>>최근 구매단가.
    IF ZSBLIT-NETPR IS INITIAL.
      CALL FUNCTION 'ZIM_MAT_PURCH_PRICE_EKPO'
           EXPORTING
                IP_WERKS = ZSBLIT-WERKS
                IP_MATNR = ZSBLIT-MATNR
                IP_EKORG = ZTBL-EKORG
                IP_LIFNR = ZTBL-LIFNR
                IP_GUBUN = 'CF'
                IP_BEDAT = SY-DATUM
           IMPORTING
                EP_PRICE = ZSBLIT-NETPR
                EP_PEINH = ZSBLIT-PEINH
                EP_BPRME = ZSBLIT-BPRME.

      IF ZSBLIT-NETPR IS INITIAL.
        MOVE : MT06E-PEINH      TO     ZSBLIT-PEINH,
               MT06E-MEINS      TO     ZSBLIT-BPRME.
        IF MT06E-VPRSV EQ 'S'.
          MOVE MT06E-STPRS     TO     ZSBLIT-NETPR.
        ELSE.
          MOVE MT06E-VERPR     TO     ZSBLIT-NETPR.
        ENDIF.
      ENDIF.
    ENDIF.
*  ELSE.
*     CLEAR : ZSBLIT-BLMENGE,
*             ZSBLIT-MEINS,
*             ZSBLIT-NETPR,
*             ZSBLIT-PEINH,
*             ZSBLIT-BPRME.
  ENDIF.
*>> 샘플일 경우 저장장소 CHECK.
  IF ZSBLIT-ZFPOTY EQ 'S' AND ZSBLIT-LGORT IS INITIAL.
    MESSAGE S083(ME) WITH 'Storage location' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'LGORT'.
  ENDIF.

*>> 저장장소 CHECK.
  IF ZSBLIT-LGORT NE SPACE.
    PERFORM LGORT(SAPFMMEX) USING ZSBLIT-LGORT ZSBLIT-WERKS
                            CHANGING T001L.
  ENDIF.

*>> 자재그룹.
  IF ZSBLIT-MATKL IS INITIAL.
    MESSAGE S083(ME) WITH 'Material group' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'MATKL'.
  ENDIF.

*>>>
*  IF NOT ZSBLIT-MATNR IS INITIAL.
*>> TEXT.
  IF ZSBLIT-TXZ01 IS INITIAL.
    MESSAGE S083(ME) WITH 'Description' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'TXZ01'.
  ENDIF.
*>> B/L 수량.
  IF ZSBLIT-BLMENGE IS INITIAL.
    MESSAGE S083(ME) WITH 'B/L quantity' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'BLMENGE'.
  ENDIF.
*>>> B/L 단위..
  IF ZSBLIT-MEINS IS INITIAL.
*        MESSAGE S083(ME) WITH 'B/L quantity' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'MENGE'.
  ENDIF.
*>> B/L 단가.
  IF ZSBLIT-NETPR IS INITIAL.
    MESSAGE S083(ME) WITH 'unit price' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'NETPR'.
  ENDIF.
*>> 가격단?
  IF ZSBLIT-PEINH IS INITIAL.
    MESSAGE S083(ME) WITH 'Price unit' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'PEINH'.
  ENDIF.
*>> 오더가격단위 (구매)
  IF ZSBLIT-BPRME IS INITIAL.
    MESSAGE S083(ME) WITH 'Order price unit' SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'BPRME'.
  ENDIF.
*  ENDIF.
*>> H/S CODE CHECK.
  IF ZSBLIT-STAWN EQ SPACE.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBLIT' 'STAWN'.
  ENDIF..

  MOVE-CORRESPONDING ZSBLIT  TO IT_ZSBLIT.
*----> 변경....
  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSBLIT  INDEX W_TABIX.
  ELSE.
*----> 마지막에 추가.
    MOVE : SY-UNAME    TO   IT_ZSBLIT-ERNAM,
           SY-DATUM    TO   IT_ZSBLIT-CDAT,
           SY-UNAME    TO   IT_ZSBLIT-UNAM,
           SY-DATUM    TO   IT_ZSBLIT-UDAT.

    APPEND IT_ZSBLIT.
  ENDIF.

ENDMODULE.                 " IT_ZSBLIT_UPDATE_SCR0111  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0111 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0111-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0111  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0111_MARK_TC_0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0111_MARK_TC_0111 INPUT.

  READ TABLE IT_ZSBLIT   INDEX TC_0111-CURRENT_LINE.
  IF SY-SUBRC = 0.
    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSBLIT-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSBLIT-ZFMARK.
    ENDIF.
    MODIFY IT_ZSBLIT INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0111_MARK_TC_0111  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTBL-BUKRS IS INITIAL.
    CLEAR : ZTIMIMG00.
    SELECT SINGLE * FROM ZTIMIMG00.
    IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  ZTBL-BUKRS.
    ENDIF.
  ENDIF.

  IF ZTBL-BUKRS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'BUKRS'.
  ELSE.
*>> 회사코드..
    SELECT SINGLE * FROM T001
           WHERE BUKRS EQ ZTBL-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E594 WITH ZTBL-BUKRS.
    ENDIF.
*>> IMG SETTING SELECT.
    SELECT SINGLE * FROM ZTIMIMG00.
*             WHERE  BUKRS EQ ZTBL-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE W963.
    ENDIF.
  ENDIF.

  IF ZTBL-INCO1 IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'INCO1'.
  ENDIF.
  IF ZTBL-ZFHBLNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFHBLNO'.
  ENDIF.
  IF ZTBL-ZFWERKS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFWERKS'.
  ENDIF.
  IF ZTBL-IMTRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'IMTRD'.
  ENDIF.

  IF SY-TCODE EQ 'ZIM21' OR SY-TCODE EQ 'ZIM22' OR
     SY-TCODE EQ 'ZIM222'.
*> 무환일 경우, 코스트 센터 및 WBS 요소 입력토록 함.
*> 무환이거나, SAMPLE Combine 된 경우.
    IF   ZTBL-ZFPOYN EQ 'N' OR
       ( ZTBL-ZFPOYN EQ 'M' AND W_SAMPLE EQ 'Y' ).
*      IF ZTBL-KOSTL IS INITIAL AND ZTBL-PS_POSID IS INITIAL AND
*         ZTBL-ZFPOYN NE 'Y'.
      IF ZTBL-ZFREBELN IS INITIAL.
        IF ZTBL-KOSTL IS INITIAL AND ZTBL-PS_POSID IS INITIAL.
          PERFORM P2000_NO_INPUT  USING  'ZTBL' 'KOSTL'
                                  DFIES-SCRTEXT_M W_SUBRC.
          MESSAGE E211(ZIM1).
        ENDIF.
      ENDIF.
    ENDIF.

    IF ZTBL-KOSTL IS INITIAL.
*         IF ZTBL-ZFPOYN EQ 'N'.
*            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'KOSTL'.
*         ENDIF.
    ELSE.
      SELECT * FROM CSKT UP TO 1 ROWS
                    WHERE KOSTL EQ ZTBL-KOSTL.
      ENDSELECT.
    ENDIF.
    IF ZTBL-ZFUPT IS INITIAL AND ZTBL-ZFPOYN EQ 'N'.
      IF ZTBL-ZFREBELN IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFUPT'.
      ENDIF.
    ENDIF.

    IF ZTBL-PS_POSID IS INITIAL.
*         IF ZTBL-ZFPOYN EQ 'N'.
*            PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'PS_POSID'.
*         ENDIF.
    ELSE.
*         SELECT PSPNR
*                INTO  W_PSPNR
      SELECT *
             FROM  PRPS "WBS (작업분할구조) 요소 마스터 데이타.
             UP TO 1 ROWS
             WHERE POSID = ZTBL-PS_POSID.
      ENDSELECT.
      IF SY-SUBRC NE 0.
        MESSAGE E212(ZIM1) WITH ZTBL-PS_POSID.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZTBL-ZFPOTY EQ 'H'.
    IF ZTBL-VBELN IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'VBELN'.
    ENDIF.
    IF ZTBL-KUNNR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'KUNNR'.
    ENDIF.

    IF ZTBL-KUNWE IS INITIAL.
      ZTBL-KUNWE = ZTBL-KUNNR.
      IF ZTBL-KUNWE IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'KUNWE'.
      ENDIF.
    ENDIF.

*      IF ZTBL-VKORG IS INITIAL.
*         PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'VKORG'.
*      ENDIF.
*      IF ZTBL-VTWEG IS INITIAL.
*         PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'VTWEG'.
*      ENDIF.
*      IF ZTBL-SPART IS INITIAL.
*         PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'SPART'.
*      ENDIF.
  ELSE.
    IF ZTBL-EKORG IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'EKORG'.
    ELSE.

    ENDIF.
    IF ZTBL-ZFPOYN NE 'N'.
      IF ZTBL-EKGRP IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'EKGRP'.
      ENDIF.
    ENDIF.
    IF ZTBL-LIFNR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'LIFNR'.
    ENDIF.
** Changed by Furong on 02/03/10
*    IF ZTBL-ZFBENI IS INITIAL.
      ZTBL-ZFBENI = ZTBL-LIFNR.
      IF ZTBL-ZFBENI IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFBENI'.
      ENDIF.
*    ENDIF.

  ENDIF.
*한수원 주석처리
** Changed by Furong on 02/02/10
*  IF   ZTBL-INCO1 EQ 'FAS' OR ZTBL-INCO1 EQ 'FCA'
*    OR ZTBL-INCO1 EQ 'FOB' OR ZTBL-INCO1 EQ 'EXW'.
*    IF ZTBL-ZFFORD IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFFORD'.
*    ENDIF.
*  ELSE.
*    MOVE ZTBL-LIFNR TO ZTBL-ZFFORD.
*  ENDIF.
   MOVE ZTBL-LIFNR TO ZTBL-ZFFORD.
** End of change
*한수원 주석처리(2003.01.18) JSY
  IF ZTBL-ZFETD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFETD'.
  ENDIF.
*한수원 주석처리(2003.01.04) JSY
  IF ZTBL-ZFETA IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFETA'.
  ENDIF.
  IF ZTBL-ZFCARC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFCARC'.
  ENDIF.
  IF ZTBL-ZFAPPC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFAPPC'.
  ENDIF.
  IF ZTBL-ZFSPRTC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFSPRTC'.
  ENDIF.
  IF ZTBL-ZFAPRTC IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFAPRTC'.
  ENDIF.

** Changed by Furong on 02/02/10
*  IF ZTBL-ZFBLDT IS INITIAL.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFBLDT'.
*  ENDIF.
  IF ZTBL-ZFBLDT IS INITIAL or
     ZTBL-ZFBLDT <> ZTBL-ZFETD.
    ZTBL-ZFBLDT = ZTBL-ZFETD.
*    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFBLDT'.
  ENDIF.
** End of change

  IF ZTBL-ZFCARNM IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFCARNM'.
  ENDIF.

*> 실입항 단계에서 포더 체크...
  IF ZTBL-ZFFORD IS INITIAL AND SY-TCODE EQ 'ZIM222'.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBL' 'ZFFORD'.
  ENDIF.

*   IF ZTBL-ZFEXRT IS INITIAL.
*     IF ZTIMIMG00-ZFEXMTD NE 'I'.
*       IF ZTBL-ZFBLAMC NE T001-WAERS.
*         PERFORM    P2000_GET_EXCHANGE_RATE     USING ZTBL-ZFBLAMC
*                                                      ZTBL-ZFBLDT
*                                                CHANGING ZTBL-ZFEXRT
*                                                      ZTBL-FFACT.
*       ENDIF.
*     ENDIF.
*   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR3516  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR3516 INPUT.

  IF NOT ( SY-UCOMM EQ 'ENTR' OR SY-UCOMM EQ 'YES' ).
    EXIT.
  ENDIF.

  IF ZTCIVHST-CBUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHST' 'CBUDAT'.
  ENDIF.

  IF ZTCIVHST-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHST' 'STGRD'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR3516  INPUT
*&---------------------------------------------------------------------*
*&      Module  PO_DUP_CHK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      BY MKIM 2001.05.09
*&---------------------------------------------------------------------*
MODULE PO_DUP_CHK_SCRCOM INPUT.
  OK-CODE = W_OK_CODE.

*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.

*-----------------------------------------------------------------------
* 기존 P/O No Dup. error Checking
*-----------------------------------------------------------------------
  IF W_EBELN IS INITIAL.
    MESSAGE W167 WITH 'P/O Number'.
  ELSE.
*     W_COUNT = 0.
*     SELECT COUNT(*) INTO W_COUNT FROM ZTPMTHD
*                  WHERE EBELN EQ W_EBELN.
**          W_COUNT = W_COUNT + 1.
**     ENDSELECT.

*     IF W_COUNT NE 0.
*        MESSAGE E572 WITH 'P/O Number'.
*     ENDIF.
  ENDIF.

ENDMODULE.                 " PO_DUP_CHK_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0114_LIST_CHECK_SCR0114  INPUT
*&---------------------------------------------------------------------*
MODULE D0114_LIST_CHECK_SCR0114 INPUT.

* list-processing
  LEAVE TO LIST-PROCESSING.
* list Write
  PERFORM P2000_DATA_BL_LISTING.

ENDMODULE.                 " D0114_LIST_CHECK_SCR0114  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ__SCREEN_9214  INPUT
*&---------------------------------------------------------------------*
MODULE READ_SCREEN_9214 INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분개.
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.
*>> 반입예정정보 미사용시 엘지버전.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFINOU EQ SPACE.
    PERFORM  P2000_NOTUSE_IMG00_ZFINOU.
  ENDIF.

  IF W_ERR_CHK EQ 'Y'. LEAVE TO SCREEN 9214. ENDIF.

ENDMODULE.                    "READ_SCREEN_9214 INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR09214  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR09214 INPUT.

*   IF W_ERR_CHK EQ 'N'.
  CASE  SY-DYNNR.
    WHEN '9214'. " LCK LG.
      PERFORM   P2000_MOVE_BL_DATA_INOU_INR.
      MOVE C_REQ_C TO W_STATUS.
    WHEN '9211'.
      MOVE C_REQ_U TO W_STATUS.
    WHEN '9212'.
      MOVE C_REQ_D TO W_STATUS.
  ENDCASE.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFINOU EQ 'X'.
    SET SCREEN 9210.  LEAVE SCREEN.
  ELSE.
    SET SCREEN 9215.  LEAVE SCREEN.
  ENDIF.
*   ENDIF.

ENDMODULE.                    "USER_COMMAND_SCR09214 INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR9215  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR9215 INPUT.

  CASE OK-CODE.
    WHEN 'ZIMG'.                     " Import IMG
      CALL TRANSACTION OK-CODE.
    WHEN 'DDLC'.           " Double click
      PERFORM  P2000_DDLC_PROCESS.
    WHEN 'BACK' OR 'EXIT' OR 'SAVE'.
      PERFORM  P2000_EXIT_PROCESS.
    WHEN 'OTDC' OR 'CRDC' OR 'CHDC' OR 'DISP'.
      PERFORM  P2000_SET_INIT_SCREEN.
    WHEN 'DELE'.
      PERFORM  P2000_SET_INDICATE.
    WHEN 'MK03'.
      PERFORM  P2000_VENDOR_MASTER USING ZTBLINR_TMP-LIFNR.
    WHEN 'ME23'.           " P/O
      PERFORM  P2000_PO_DOC_DISPLAY     USING ZTBLINR_TMP-ZFREBELN ''.
    WHEN 'ZIM03'.          " L/C
      PERFORM  P2000_LC_DOC_DISPLAY1  USING   ZTBLINR_TMP-ZFREBELN.
    WHEN 'ZIM23'.
      PERFORM   P2000_BL_DOC_DISPLAY     USING ZTBLINR_TMP-ZFBLNO.
    WHEN 'HIST'.           " HEADER CHANGE DOCUMENT
      PERFORM  P2000_HEADER_CHANGE_DOC.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR9215  INPUT
*&---------------------------------------------------------------------*
*&      Module  ARRIVE_READ_IMG03_SCR9215  INPUT
*&---------------------------------------------------------------------*
MODULE ARRIVE_READ_IMG03_SCR9215 INPUT.

*  IF  ZTBLINR_TMP-ZFREBELN IS INITIAL.
*      MESSAGE E977 WITH 'Entry Rep P/O No!'.
*  ENDIF.
  IF ZTBLINR_TMP-ZFHBLNO IS INITIAL.
    MESSAGE E977 WITH 'Entry B/L Number!'.
  ENDIF.
  IF ZTBLINR_TMP-ZFINDT IS INITIAL.
    ZTBLINR_TMP-ZFINDT = SY-DATUM.
  ENDIF.

  ZTBLINR_TMP-ZFYR =  ZTBLINR_TMP-ZFINDT+02(02).

  IF ZTBLINR_TMP-ZFBNARCD IS INITIAL.
    MESSAGE E977 WITH 'Entry Warehouse Code!'.
  ENDIF.
  PERFORM   P1000_GET_BONDED_NAME   USING   ZTBLINR_TMP-ZFBNARCD
                                  CHANGING   ZTBLINR_TMP-ZFABNAR
                                              W_DEL_NM.

  IF ZTBLINR_TMP-ZFINRNO IS INITIAL.
    MESSAGE E977 WITH 'Entry Carry-in Number!'.
  ENDIF.


*  IF ZTBLINR_TMP-ZFSEQ IS INITIAL.
*    MESSAGE E977 WITH 'Input serial No.!'.
*  ENDIF.
*>> 반입신고번호.
*  CONCATENATE ZTBLINR_TMP-ZFABNAR ZTBLINR_TMP-ZFYR ZTBLINR_TMP-ZFSEQ
*                      INTO ZTBLINR_TMP-ZFINRNO.
*  IF ZTBLINR_TMP-ZFSPOS IS INITIAL.
*     MESSAGE E977 WITH 'Input location No!'.
*  ENDIF.
  IF ZTBLINR_TMP-ZFBTRNO IS INITIAL.
    MESSAGE E977 WITH 'Entry bonded transportation license No'.
  ENDIF.
  IF ZTBLINR_TMP-ZFCARNM IS INITIAL.
    MESSAGE E977 WITH 'Entry Vessel Name!'.
  ENDIF.
  IF ZTBLINR_TMP-ZFINTY IS INITIAL.
    MESSAGE E977 WITH 'Entry Carry-in Type!'.
  ENDIF.
  IF ZTBLINR_TMP-ZFINWT IS INITIAL.
    MESSAGE E977 WITH 'Entry Carry-in Weight!'.
  ENDIF.
*  IF ZTBLINR_TMP-ZFTOWTM IS INITIAL.
*    ZTBLINR_TMP-ZFTOWTM = 'TON'.
*  ENDIF.
  IF ZTBLINR_TMP-ZFOKPK IS INITIAL.
    MESSAGE E977 WITH 'Entry Carry-in Quantity!'.
  ENDIF.

  IF ZTBLINR_TMP-ZFPKCNM IS INITIAL.
    ZTBLINR_TMP-ZFPKCNM = 'GT'.
  ENDIF.

  IF ZTBLINR_TMP-ZFTOWTM IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
         EXPORTING
              INPUT          = 'TON'
         IMPORTING
              OUTPUT         = ZTBLINR_TMP-ZFTOWTM
         EXCEPTIONS
              UNIT_NOT_FOUND = 1
              OTHERS         = 2.
    IF SY-SUBRC NE 0.
      CLEAR ZTBLINR_TMP-ZFTOWTM.
    ENDIF.
  ENDIF.
*  IF ZTBLINR_TMP-ZFSHTY EQ 'F'.
*   IF ZTBLINR_TMP-ZF20FT IS INITIAL AND
*      ZTBLINR_TMP-ZF40FT IS INITIAL.
*     MESSAGE E977 WITH 'Input container quantity!'.
*   ENDIF.
* ENDIF.

ENDMODULE.                 " ARRIVE_READ_IMG03_SCR9215  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_CARIR_NAME_SCR2603  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CARIR_NAME_SCR2603 INPUT.
*>    조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTLG-ZFCARIR IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR: W_LFA1.

  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = ZTLG-ZFCARIR
       IMPORTING
            XLFA1          = W_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    ZTLG-ZFCARIR.
  ENDCASE.
*   TRANSLATE W_LFA1 TO UPPER CASE.
*  MOVE: W_LFA1-NAME1   TO   L_TEXT.
*  TRANSLATE L_TEXT TO UPPER CASE.

  MOVE : W_LFA1-NAME1         TO         ZTLG-ZFCARR1,
         W_LFA1-NAME2         TO         ZTLG-ZFCARR2,
       W_LFA1-BAHNS         TO         ZTLG-ZFCARR3. " 수발신식별?

ENDMODULE.                 " GET_CARIR_NAME_SCR2603  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR_MARK_TC_0103 INPUT.

ENDMODULE.                 " SET_SCR_MARK_TC_0103  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZTTTSG5_GET_LINE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZTTTSG5_GET_LINE_SCR0143 INPUT.
  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0143-CURRENT_LINE + LINE - 1.
ENDMODULE.                 " ZTTTSG5_GET_LINE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSTTSG5_UPDATE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSTTSG5_UPDATE_SCR0143 INPUT.

  READ TABLE IT_ZSTTSG5  WITH KEY ZFLSG5  = ZSTTSG5-ZFLSG5
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSTTSG5 TO IT_ZSTTSG5.

  IF W_SY_SUBRC EQ 0.
    MODIFY IT_ZSTTSG5 INDEX W_TABIX.
  ELSE.
    IT_ZSTTSG5-ZFLSG5 = TC_0143-CURRENT_LINE * 10.
    APPEND IT_ZSTTSG5.
  ENDIF.

ENDMODULE.                 " ZSTTSG5_UPDATE_SCR0143  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0143_MARK_TC_0143  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0143_MARK_TC_0143 INPUT.

  READ TABLE IT_ZSTTSG5  WITH KEY ZFLSG5  = ZSTTSG5-ZFLSG5
                         BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF W_SY_SUBRC = 0.
    MOVE-CORRESPONDING   ZSTTSG5   TO IT_ZSTTSG5.
    IF NOT ( W_ROW_MARK1 IS INITIAL ).
      IT_ZSTTSG5-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSTTSG5-ZFMARK.
    ENDIF.
    MODIFY IT_ZSTTSG5 INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0143_MARK_TC_0143  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR0143  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR0143 INPUT.
  IF SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL2' OR 'UND2'.   " 상품용역명세 삭제 / 취?
      LOOP AT IT_ZSTTSG5     WHERE ZFMARK NE SPACE.
        DELETE IT_ZSTTSG5   INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'. " 전체선택 or 선택해?
      LOOP AT IT_ZSTTSG5.
        IT_ZSTTSG5-ZFMARK = W_MARK.   MODIFY IT_ZSTTSG5.
      ENDLOOP.
    WHEN 'REF2'.           " 품목명세 Refresh
      REFRESH : IT_ZSTTSG5.
      LOOP AT IT_ZSTTSG5_ORG.
        CLEAR : IT_ZSTTSG5.
        MOVE-CORRESPONDING   IT_ZSTTSG5_ORG   TO   IT_ZSTTSG5.
        APPEND IT_ZSTTSG5.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

* INDEX 수정 작?
  PERFORM   P2000_IT_ZSTTSG5_UPDATE.
ENDMODULE.                 " USER_COMMAND_SCR0143  INPUT
*&---------------------------------------------------------------------*
*&      Module  BANK_OPERATION_SCR0114  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BANK_OPERATION_SCR0114 INPUT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  IF ZTTTHD-ZFBUSFUN EQ '2AJ'.
    IF ZTTTHD-ZFCOMMTY IS INITIAL.
      MESSAGE E167 WITH 'Payment commission type'.
    ENDIF.
  ELSE.
    IF NOT ZTTTHD-ZFCOMMTY IS INITIAL.
      MESSAGE E215.
    ENDIF.
  ENDIF.


ENDMODULE.                 " BANK_OPERATION_SCR0114  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZINSDT_CONFIRM_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZINSDT_CONFIRM_BL_SCR4102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF NOT ZTINSB-ZFINSDT IS INITIAL.
    IF ZTINSB-ZFINSDT < ZTBL-CDAT.
      MESSAGE  E210(ZIM1).
    ENDIF.
  ENDIF.

ENDMODULE.                 " ZINSDT_CONFIRM_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRNAS_METHOD_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRNAS_METHOD_BL_SCR4102 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF NOT ZTINSB-ZFINSDT IS INITIAL.
    IF ZTINSB-ZFINSDT < ZTBL-CDAT.
      MESSAGE  E210(ZIM1).
    ENDIF.
  ENDIF.

ENDMODULE.                 " TRNAS_METHOD_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CALC_AMOUNT_BL_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CALC_AMOUNT_BL_SCRCOM INPUT.

  IF ZTINSB-ZFALCP EQ 0.
    ZTINSB-ZFIVAMT = ZTBL-ZFBLAMT.
  ELSE.
    ZTINSB-ZFIVAMT = ZTBL-ZFBLAMT +
                     ZTBL-ZFBLAMT * ( ZTINSB-ZFALCP / 100 ).
  ENDIF.

ENDMODULE.                 " SET_CALC_AMOUNT_BL_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INS_OPEN_DATA_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INS_OPEN_DATA_BL_SCR4102 INPUT.

* 확정일 경우만.
  CHECK  W_STATUS EQ C_OPEN_C.

  IF ZTINSB-ZFINNO IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSB' 'ZFINNO'.
  ENDIF.

  IF ZTINSBRSP-ZFISDT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSBRSP' 'ZFISDT'.
  ENDIF.

ENDMODULE.                 " CHECK_INS_OPEN_DATA_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CALC_LOC_AMOUNT_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
MODULE SET_CALC_LOC_AMOUNT_BL_SCR4102 INPUT.

*>> Incase of State in Confirm.
  CHECK  W_STATUS EQ C_OPEN_C.
*>> Insurance Rate..
  IF ZTINSB-ZFINRT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSB' 'ZFINRT'.
  ENDIF.
*>> Exchange rate
  IF ZTINSBRSP-ZFEXRT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTINSBRSP' 'ZFEXRT'.
  ENDIF.
  W_ZFINAMT = ZTINSB-ZFIVAMT * ( ZTINSB-ZFINRT / 100 )
                            * ( ZTINSB-ZFPEIV / 100 ).
*>> Foreign Currency Insurance Amt..
  IF ZTINSB-ZFINAMT IS INITIAL OR ZTINSB-ZFINAMT NE W_ZFINAMT.
    ZTINSB-ZFINAMT = W_ZFINAMT.
*    ZTINSB-ZFINAMT = ZTINSB-ZFIVAMT * ( ZTINSB-ZFINRT / 100 )
*                                    * ( ZTINSB-ZFPEIV / 100 ).
  ENDIF.
  IF ZTINSBRSP-FFACT IS INITIAL.
    MESSAGE W637.
    ZTINSBRSP-FFACT = 1.
  ENDIF.
  " 보험료 원화.
  IF ZTINSB-ZFKRWAMT IS INITIAL OR ZTINSB-ZFKRWAMT NE ZTINSB-ZFINAMT.
    PERFORM    SET_CURR_CONV_TO_EXTERNAL USING ZTINSB-ZFINAMT
                                               ZTINSB-ZFINAMTC
                                               ZTINSB-ZFKRWAMT.
    IF ZTINSBRSP-FFACT IS INITIAL.
      ZTINSBRSP-FFACT  =  1.
    ENDIF.
    ZTINSB-ZFKRWAMT = ZTINSB-ZFKRWAMT *
                    ( ZTINSBRSP-ZFEXRT / ZTINSBRSP-FFACT ).

    PERFORM    SET_CURR_CONV_TO_INTERNAL USING ZTINSB-ZFKRWAMT
                                               ZTINSB-ZFKRW.
*>>> 2001.04.07 KSB ---> 10원단위 이하 절사....
*    ZTINSB-ZFKRWAMT = ( TRUNC( ZTINSB-ZFKRWAMT * 10 ) ) / 10. "NHJ
  ENDIF.
  IF ZTINSBRSP-ZFTAMI IS INITIAL               "> Total insured amount..
  OR ZTINSBRSP-ZFTAMI NE ZTINSB-ZFINAMT.       "> Insurance Amount..
    ZTINSBRSP-ZFTAMI = ZTINSB-ZFINAMT.
  ENDIF.
  IF ZTINSBRSP-ZFCAMI IS INITIAL.
*  OR ZTINSBRSP-ZFCAMI NE ZTINSB-ZFINAMT.
    ZTINSBRSP-ZFCAMI = ZTINSB-ZFINAMT.
  ENDIF.
  IF ZTINSBRSP-ZFTPR IS INITIAL
  OR ZTINSBRSP-ZFTPR NE ZTINSB-ZFKRWAMT.
    ZTINSBRSP-ZFTPR = ZTINSB-ZFKRWAMT.
  ENDIF.
  IF ZTINSBRSP-ZFCPR IS INITIAL
  OR ZTINSBRSP-ZFCPR NE ZTINSBRSP-ZFTPR.
    ZTINSBRSP-ZFCPR = ZTINSBRSP-ZFTPR.
  ENDIF.

ENDMODULE.                 " SET_CALC_LOC_AMOUNT_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INS_EX_AMOUNT_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INS_EX_AMOUNT_BL_SCR4102 INPUT.

* Case Confirm.
  CHECK  W_STATUS EQ C_OPEN_C.
  IF ZTINSBRSP-ZFTAMI IS INITIAL OR ZTINSBRSP-ZFTAMI NE ZTINSB-ZFINAMT.
    ZTINSBRSP-ZFTAMI = ZTINSB-ZFINAMT.
  ENDIF.
  IF ZTINSBRSP-ZFCAMI IS INITIAL."OR ZTINSBRSP-ZFCAMI NE ZTINSB-ZFINAMT.
    ZTINSBRSP-ZFCAMI = ZTINSB-ZFINAMT.
  ENDIF.

  W_AMOUNT = ZTINSBRSP-ZFCAMI + ZTINSBRSP-ZFDAMI.
  IF W_AMOUNT NE ZTINSBRSP-ZFTAMI.
    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSBRSP' 'ZFTAMI'
                                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE  E521.
  ENDIF.

ENDMODULE.                 " CHECK_INS_EX_AMOUNT_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INS_LOC_AMT_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INS_LOC_AMT_BL_SCR4102 INPUT.

* 확정일 경우만.
  CHECK  W_STATUS EQ C_OPEN_C.
  IF ZTINSBRSP-ZFTPR IS INITIAL OR ZTINSBRSP-ZFTPR NE ZTINSB-ZFINAMT.
    ZTINSBRSP-ZFTPR = ZTINSB-ZFINAMT.
  ENDIF.

  IF ZTINSBRSP-ZFCPR IS INITIAL.
    ZTINSBRSP-ZFCPR = ZTINSBRSP-ZFTPR.
  ENDIF.

  W_AMOUNT = ZTINSBRSP-ZFCPR + ZTINSBRSP-ZFDPR + ZTINSBRSP-ZFVPR +
             ZTINSBRSP-ZFIPR.

  IF ZTINSBRSP-ZFTPR NE W_AMOUNT.
    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSBRSP' 'ZFTPR'
                                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE  E521.
  ENDIF.

  IF ZTINSB-ZFKRWAMT NE ZTINSBRSP-ZFTPR.       " 보험료 원화.
    PERFORM  P2000_NO_INPUT(SAPMZIM01) USING 'ZTINSB' 'ZFKRWAMT'
                                            DFIES-SCRTEXT_M W_SUBRC.
    MESSAGE  E520.
  ENDIF.

ENDMODULE.                 " CHECK_INS_LOC_AMT_BL_SCR4102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_INSU_NAME_4103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_INSU_NAME_4103 INPUT.

  CLEAR  W_ZFOPCD1.
  MOVE : ZTINSB-ZFOPCD TO W_TEXT12.

  CALL FUNCTION 'AIA6_ALPHA_OUTPUT_C12_TO_C24'
       EXPORTING
            I_INTERN = W_TEXT12
       IMPORTING
            E_EXTERN = W_TEXT24.

  W_ZFOPCD1 = W_TEXT24.

  SELECT SINGLE  ZFCD4   INTO  ZTINSB-ZFEDI
  FROM   ZTIMIMG08
  WHERE  ZFCDTY  EQ  '010'
  AND    ZFCD5   EQ  W_ZFOPCD1.

  IF SY-SUBRC NE 0.
    MESSAGE E468.
  ENDIF.

* 피보험자 상호1, 피보험자 상호2, EDI 식별자, 손해보험회사 Vendor
  SELECT  SINGLE NAME1  NAME2
  INTO    (ZTINSB-ZFINSU1, ZTINSB-ZFINSU2)
  FROM    LFA1
  WHERE   LIFNR     EQ  ZTINSB-ZFOPCD.

ENDMODULE.                 " GET_INSU_NAME_4103  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR4104 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_4104-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_HS_CODE_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_HS_CODE_BIT_SCR4104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
    IF ZTINSB-ZFRSTAW  NE  ZTINSB_OLD-ZFRSTAW.
      ZTINSB-ZFHSYN = 'X'.
    ELSE.
      CLEAR : ZTINSB-ZFHSYN.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_HS_CODE_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_DOC_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DOC_BIT_SCR4104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* 관련 서류 1
  IF ( ZTINSB-ZFREDOC1 IS INITIAL AND NOT ZTINSB-ZFREDON1 IS INITIAL ).
    MESSAGE E167 WITH 'Related Doc code1'.
  ENDIF.
  IF ( NOT ZTINSB-ZFREDOC1 IS INITIAL AND ZTINSB-ZFREDON1 IS INITIAL ).
    MESSAGE E167 WITH 'Related Doc No1'.
  ENDIF.
* 관련 서류 2
  IF ( ZTINSB-ZFREDOC2 IS INITIAL AND NOT ZTINSB-ZFREDON2 IS INITIAL ).
    MESSAGE E167 WITH 'Related Doc code2'.
  ENDIF.
  IF ( NOT ZTINSB-ZFREDOC2 IS INITIAL AND ZTINSB-ZFREDON2 IS INITIAL ).
    MESSAGE E167 WITH 'Related Doc No2'.
  ENDIF.
* 관련 서류 3
  IF ( ZTINSB-ZFREDOC3 IS INITIAL AND NOT ZTINSB-ZFREDON3 IS INITIAL ).
    MESSAGE E167 WITH 'Related Doc code3'.
  ENDIF.
  IF ( NOT ZTINSB-ZFREDOC3 IS INITIAL AND ZTINSB-ZFREDON3 IS INITIAL ).
    MESSAGE E167 WITH 'Related Doc No3'.
  ENDIF.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
    IF ( ZTINSB-ZFREDOC1 EQ ZTINSB_OLD-ZFREDOC1 ) AND
       ( ZTINSB-ZFREDOC2 EQ ZTINSB_OLD-ZFREDOC2 ) AND
       ( ZTINSB-ZFREDOC3 EQ ZTINSB_OLD-ZFREDOC3 ) AND
       ( ZTINSB-ZFREDON1 EQ ZTINSB_OLD-ZFREDON1 ) AND
       ( ZTINSB-ZFREDON2 EQ ZTINSB_OLD-ZFREDON2 ) AND
       ( ZTINSB-ZFREDON3 EQ ZTINSB_OLD-ZFREDON3 ).
      CLEAR : ZTINSB-ZFDOYN.
    ELSE.
      ZTINSB-ZFDOYN = 'X'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_DOC_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZSINSBSG2_UPDATE_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZSINSBSG2_UPDATE_SCR4104 INPUT.

  READ TABLE IT_ZSINSBSG2 WITH KEY ZFLSG2  = ZSINSBSG2-ZFLSG2
                          BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSINSBSG2  TO IT_ZSINSBSG2.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSBSG2 INDEX W_TABIX.
  ELSE.
    IT_ZSINSBSG2-ZFLSG2 = TC_4104-CURRENT_LINE * 10.
    APPEND IT_ZSINSBSG2.
  ENDIF.

ENDMODULE.                 " ZSINSBSG2_UPDATE_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_MARK_TC4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MARK_TC4104 INPUT.

  READ TABLE IT_ZSINSBSG2 WITH KEY ZFLSG2  = ZSINSSG2-ZFLSG2
                          BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ( W_ROW_MARK IS INITIAL ).
    IT_ZSINSBSG2-ZFMARK = 'X'.
  ELSE.
    CLEAR : IT_ZSINSBSG2-ZFMARK.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSBSG2 INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_MARK_TC4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TRANS_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TRANS_BIT_SCR4104 INPUT.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
    IF ZTINSBSG3-ZFCARNU EQ ZTINSBSG3_OLD-ZFCARNU AND
       ZTINSBSG3-ZFCARNM EQ ZTINSBSG3_OLD-ZFCARNM.
      CLEAR : ZTINSB-ZFTMYN.
    ELSE.
      ZTINSB-ZFTMYN = 'X'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_TRANS_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_PORT_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_PORT_BIT_SCR4104 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.
* 선적지.
  IF ( ZTINSBSG3-ZFSHCU IS INITIAL AND
       NOT ZTINSBSG3-ZFSHCUNM IS INITIAL ).
    MESSAGE E167 WITH 'Shipping area code'.
  ENDIF.
  IF ( NOT ZTINSBSG3-ZFSHCU IS INITIAL AND
           ZTINSBSG3-ZFSHCUNM IS INITIAL ).
    PERFORM GET_ORIJIN_NAME   USING    ZTINSBSG3-ZFSHCU
                              CHANGING ZTINSBSG3-ZFSHCUNM.
  ENDIF.
* 도착지.
  IF ( ZTINSBSG3-ZFARCU IS INITIAL AND
       NOT ZTINSBSG3-ZFARCUNM IS INITIAL ).
    MESSAGE E167 WITH 'Arrival area code'.
  ENDIF.
  IF ( NOT ZTINSBSG3-ZFARCU IS INITIAL AND
           ZTINSBSG3-ZFARCUNM IS INITIAL ).
    PERFORM GET_ORIJIN_NAME   USING    ZTINSBSG3-ZFARCU
                              CHANGING ZTINSBSG3-ZFARCUNM.
  ENDIF.
* 최종도착지.
  IF ( ZTINSBSG3-ZFLACU IS INITIAL AND
       NOT ZTINSBSG3-ZFLACUNM IS INITIAL ).
    MESSAGE E167 WITH 'Final arrival area code'.
  ENDIF.
  IF ( NOT ZTINSBSG3-ZFLACU IS INITIAL AND
           ZTINSBSG3-ZFLACUNM IS INITIAL ).
    PERFORM GET_ORIJIN_NAME   USING    ZTINSBSG3-ZFLACU
                              CHANGING ZTINSBSG3-ZFLACUNM.
  ENDIF.
* 환적지.
  IF ( ZTINSBSG3-ZFTRCU IS INITIAL AND
       NOT ZTINSBSG3-ZFTRCUNM IS INITIAL ).
    MESSAGE E167 WITH 'Transhipment area code'.
  ENDIF.
  IF ( NOT ZTINSBSG3-ZFTRCU IS INITIAL AND
           ZTINSBSG3-ZFTRCUNM IS INITIAL ).
    PERFORM GET_ORIJIN_NAME   USING    ZTINSBSG3-ZFTRCU
                              CHANGING ZTINSBSG3-ZFTRCUNM.
  ENDIF.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
    IF ( ZTINSBSG3-ZFSHCU   EQ ZTINSBSG3_OLD-ZFSHCU   ) AND
       ( ZTINSBSG3-ZFSHCUNM EQ ZTINSBSG3_OLD-ZFSHCUNM ) AND
       ( ZTINSBSG3-ZFARCU   EQ ZTINSBSG3_OLD-ZFARCU   ) AND
       ( ZTINSBSG3-ZFARCUNM EQ ZTINSBSG3_OLD-ZFARCUNM ) AND
       ( ZTINSBSG3-ZFLACU   EQ ZTINSBSG3_OLD-ZFLACU   ) AND
       ( ZTINSBSG3-ZFLACUNM EQ ZTINSBSG3_OLD-ZFLACUNM ) AND
       ( ZTINSBSG3-ZFTRCU   EQ ZTINSBSG3_OLD-ZFTRCU   ) AND
       ( ZTINSBSG3-ZFTRCUNM EQ ZTINSBSG3_OLD-ZFTRCUNM ).
      CLEAR : ZTINSB-ZFPRYN.
    ELSE.
      ZTINSB-ZFPRYN = 'X'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_PORT_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_START_DT_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_START_DT_BIT_SCR4104 INPUT.

  IF SY-TCODE EQ 'ZIM45' OR SY-TCODE EQ 'ZIM46' OR SY-TCODE EQ 'ZIM47'
                         OR SY-TCODE EQ 'ZIM48'.
    IF ZTINSBSG3-ZFDPDT NE ZTINSBSG3_OLD-ZFDPDT.
      ZTINSB-ZFDTYN = 'X'.
    ELSE.
      CLEAR : ZTINSB-ZFDTYN.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_START_DT_BIT_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4104 INPUT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1'.   " 전체 선?
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭?
      LOOP AT IT_ZSINSBSG2   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSBSG2  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSBSG2.
        IT_ZSINSBSG2-ZFMARK = W_MARK.   MODIFY IT_ZSINSSG2.
      ENDLOOP.
    WHEN 'REF1'.           " 품목 Refresh
      REFRESH : IT_ZSINSBSG2.
      LOOP AT IT_ZSINSBSG2_ORG.
        MOVE-CORRESPONDING   IT_ZSINSBSG2_ORG   TO   IT_ZSINSBSG2.
        APPEND IT_ZSINSBSG2.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

* 품목명세 INDEX 수정 작?
  PERFORM   P2000_IT_ZSINSSG2_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR4104  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR4105 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_4105-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR4105  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TAG7433_SCR4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TAG7433_SCR4105 INPUT.

* 조회 MODE시 MODULE EXIT.
  IF W_STATUS = C_REQ_D OR W_STATUS = C_ADD_D OR W_STATUS = C_OPEN_D.
    EXIT.
  ENDIF.

  READ TABLE IT_ZSINSBAGR INDEX  TC_4105-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE-CORRESPONDING   ZSINSBAGR  TO IT_ZSINSBAGR.
  IF NOT ZSINSBAGR-ZFINSCD IS INITIAL AND ZSINSBAGR-ZFCNCDNM IS INITIAL.
    PERFORM   GET_DD07T_SELECT USING    'ZDINSCD'  IT_ZSINSBAGR-ZFINSCD
                               CHANGING  IT_ZSINSBAGR-ZFCNCDNM.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSBAGR INDEX W_TABIX.
  ELSE.
    IT_ZSINSBAGR-ZFLAGR = TC_4105-CURRENT_LINE * 10.
    APPEND IT_ZSINSBAGR.
  ENDIF.

ENDMODULE.                 " GET_TAG7433_SCR4105  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_MARK_TC4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_MARK_TC4105 INPUT.

  READ TABLE IT_ZSINSBAGR WITH KEY ZFLAGR  = ZSINSAGR-ZFLAGR
                          BINARY SEARCH.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF NOT ( W_ROW_MARK IS INITIAL ).
    IT_ZSINSBAGR-ZFMARK = 'X'.
  ELSE.
    CLEAR : IT_ZSINSBAGR-ZFMARK.
  ENDIF.

  IF W_SY_SUBRC = 0.
    MODIFY IT_ZSINSBAGR INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " SET_MARK_TC4105  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4105 INPUT.

  IF SY-UCOMM EQ 'MKA1' OR SY-UCOMM EQ 'DEL1' OR
     SY-UCOMM EQ 'MKA2' OR SY-UCOMM EQ 'DEL2'.
    W_MARK = 'X'.
  ELSEIF SY-UCOMM EQ 'MKL1' OR SY-UCOMM EQ 'UND1' OR
         SY-UCOMM EQ 'MKL2' OR SY-UCOMM EQ 'UND2'.  " 선택 해?
    CLEAR : W_MARK.
  ENDIF.

  CASE OK-CODE.
    WHEN 'DEL1' OR 'UND1'. " 품목 삭제 / 취?
      LOOP AT IT_ZSINSBAGR   WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSBAGR  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'DEL2' OR 'UND2'.   " 상품용역명세 삭제 / 취?
      LOOP AT IT_ZSINSBSG5    WHERE ZFMARK NE SPACE.
        DELETE IT_ZSINSBSG5  INDEX SY-TABIX.
      ENDLOOP.
    WHEN 'MKA1' OR 'MKL1'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSBAGR.
        IT_ZSINSBAGR-ZFMARK = W_MARK.   MODIFY IT_ZSINSBAGR.
      ENDLOOP.
    WHEN 'MKA2' OR 'MKL2'. " 전체선택 or 선택해?
      LOOP AT IT_ZSINSBSG5.
        IT_ZSINSBSG5-ZFMARK = W_MARK.   MODIFY IT_ZSINSBSG5.
      ENDLOOP.
    WHEN 'REF1'.           " Refresh
      REFRESH : IT_ZSINSBAGR.
      LOOP AT IT_ZSINSBAGR_ORG.
        CLEAR : IT_ZSINSBAGR.
        MOVE-CORRESPONDING   IT_ZSINSBAGR_ORG   TO   IT_ZSINSBAGR.
        APPEND IT_ZSINSBAGR.
      ENDLOOP.
    WHEN 'REF2'.           " Refresh
      REFRESH : IT_ZSINSBSG5.
      LOOP AT IT_ZSINSBSG5_ORG.
        CLEAR : IT_ZSINSBSG5.
        MOVE-CORRESPONDING   IT_ZSINSBSG5_ORG   TO   IT_ZSINSBSG5.
        APPEND IT_ZSINSBSG5.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.

* INDEX 수정 작?
  PERFORM   P2000_IT_ZSINSBAGR_UPDATE.
  PERFORM   P2000_IT_ZSINSBSG5_UPDATE.

ENDMODULE.                 " USER_COMMAND_SCR4105  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR4100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCR4100 INPUT.

  IF ( ANTWORT EQ 'Y' AND  W_COUNT > 1 ) OR  W_COUNT EQ 1.
    W_STATUS = C_REQ_C.
    CLEAR : ZTINSB, ZTINSBSG3, ZTINSBRSP.
    CLEAR : W_ZFINSU1, W_ZFINSU2, W_ZFOPCD, W_ZFEDI.
* Import Config Select
    SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
    IF SY-SUBRC NE 0.   MESSAGE E961.   ENDIF.

    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE    BUKRS   EQ   ZTBL-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E949 WITH ZTBL-BUKRS.
    ENDIF.

    SELECT SINGLE ZFCD5 ZFCD4
    INTO   (W_ZFOPCD, ZTINSB-ZFEDI)
    FROM   ZTIMIMG08
    WHERE  ZFCDTY    EQ  '010'
    AND    ZFCD      EQ  ZTIMIMGTX-ZFINSC.

    PERFORM ALPHAFORMAT(SAPFF001) USING W_ZFOPCD ZTINSB-ZFOPCD.

    SELECT SINGLE *
             FROM ZTIMIMG11
            WHERE BUKRS = ZTBL-BUKRS.

* DATA MOVE
    MOVE : ZTBL-ZFBLNO        TO ZTINSB-ZFBLNO,    " 문서번호.
           ZTBL-ZFHBLNO       TO ZTINSB-ZFHBLNO,   " HOUSE BL NO.
           ZTBL-ZFINSYN       TO ZTINSB-ZFINCD,    " 보험등급.
           ZTBL-BUKRS         TO ZTINSB-BUKRS,     " 회사코드.
           ZTINSB-ZFOPCD      TO ZTINSB-ZFINCOM,   " 보험회사코드.
           ZTIMIMGTX-ZFOPNO   TO ZTINSB-ZFOPNO,    " 포괄보험증서.
           ZTIMIMGTX-ZFELTXN  TO ZTINSB-ZFELTXN,   " 사업자등록번호.
           ZTIMIMGTX-ZFELENM  TO ZTINSB-ZFELENM,   " 상호.
           ZTIMIMGTX-ZFREPRE  TO ZTINSB-ZFREPRE,   " 대표자.
           ZTIMIMGTX-ZFELEID  TO ZTINSB-ZFELEID,   " 전자서명.
           ZTIMIMG11-ZFINSCP  TO ZTINSB-ZFINCOM,   " Insurance Company..
           '3'                TO ZTINSB-ZFNUCD,    " 사본 발급요청 부수.
           '2'                TO ZTINSB-ZFNUOD,    " 원본 발급요청 부수.
           'N'                TO ZTINSB-ZFDOCST,   " 문서상태.
           'N'                TO ZTINSB-ZFEDIST,   " EDI 상태.
           'X'                TO ZTINSB-ZFEDICK,   " EDI CHECK
           ZTBL-ZFBLAMT       TO ZTINSB-ZFIVAMT,   " Invoice 금액.
           ZTBL-ZFBLAMC       TO ZTINSB-WAERS,     " Cur.
           ZTBL-ZFBLAMC       TO ZTINSBRSP-ZFTAMIC," Cur.
           ZTBL-ZFBLAMC       TO ZTINSBRSP-ZFCAMIC," Cur.
           ZTBL-ZFBLAMC       TO ZTINSBRSP-ZFDAMIC," Cur.
           ZTBL-ZFBLAMC       TO ZTINSB-ZFINAMTC,  " 보험료 통화.
           ZTBL-ZFCARC        TO ZTINSBSG3-ZFSHCU, " 선적국.
           ZTBL-ZFAPPC        TO ZTINSBSG3-ZFARCU. " 도착국.

    " LOCAL CURRENCY SET.
    SELECT SINGLE * FROM T001 WHERE BUKRS EQ ZTBL-BUKRS.
    MOVE T001-WAERS   TO  :  ZTINSBRSP-ZFTPRC,
                             ZTINSBRSP-ZFCPRC,
                             ZTINSBRSP-ZFDPRC,
                             ZTINSBRSP-ZFVPRC,
                             ZTINSBRSP-ZFIPRC,
                             ZTINSB-ZFKRW.
    IF ZTINSB-WAERS  EQ  ZTINSB-ZFKRW.
      MOVE   1      TO  : ZTINSBRSP-ZFEXRT, ZTINSBRSP-FFACT.
    ENDIF.

    " 운송방법.
    IF ZTBL-ZFVIA  EQ 'AIR'.
      MOVE  'A'   TO  ZTINSB-ZFTRANS.
    ELSEIF ZTBL-ZFVIA  EQ 'VSL'.
      MOVE  'O'   TO  ZTINSB-ZFTRANS.
    ELSE.
      MOVE  'B'   TO  ZTINSB-ZFTRANS.
    ENDIF.
    " 관련 서류코드, 서류발행번호(P/O 번호).
    IF NOT ZTBL-ZFREBELN IS INITIAL.
      MOVE:'POR'             TO ZTINSB-ZFREDOC1,
           ZTBL-ZFREBELN     TO ZTINSB-ZFREDON1.
    ENDIF.
    "희망이익율, 보험부보일,원화통화단위.
    MOVE :  '110'             TO ZTINSB-ZFPEIV,
            SY-DATUM          TO ZTINSB-ZFINSDT.
    "선적국가명.
    CLEAR : T005T.
    SELECT SINGLE * FROM T005T WHERE   SPRAS EQ 'E'
                               AND     LAND1 EQ ZTINSBSG3-ZFSHCU.
    MOVE : T005T-LANDX     TO   ZTINSBSG3-ZFSHCUNM.
    "도착국가명.
    CLEAR : T005T.
    SELECT SINGLE * FROM T005T WHERE   SPRAS EQ 'E'
                               AND     LAND1 EQ ZTINSBSG3-ZFARCU.
    MOVE : T005T-LANDX     TO   ZTINSBSG3-ZFARCUNM.

* 대표 H/S CODE / 상품명세서.
    REFRESH : IT_ZSINSBSG2.
    LOOP AT IT_ZSBLIT.
      MOVE : IT_ZSBLIT-STAWN      TO ZTINSB-ZFRSTAW.
      PERFORM ALPHAFORMAT(SAPFF001) USING W_ZFOPCD IT_ZSBLIT-MATNR.
      CONCATENATE  IT_ZSBLIT-TXZ01 '('  W_ZFOPCD  ')'
              INTO IT_ZSINSBSG2-ZFDSOG1.
      COLLECT IT_ZSINSBSG2.
      " 관련 서류코드, 서류발행번호(L/C 번호).
      IF SY-TABIX EQ 1.
        SELECT SINGLE * FROM  ZTREQHD
               WHERE  ZFREQNO EQ IT_ZSBLIT-ZFREQNO.

        MOVE:'LC'               TO ZTINSB-ZFREDOC2,
              ZTREQHD-ZFOPNNO   TO ZTINSB-ZFREDON2.
      ENDIF.
    ENDLOOP.
    LOOP AT IT_ZSINSBSG2.
      W_TABIX = SY-TABIX.
      IT_ZSINSBSG2-ZFLSG2  =  SY-TABIX * 10.
      MODIFY IT_ZSINSBSG2 INDEX W_TABIX.
    ENDLOOP.
* 부보조건 TEXT SET
    PERFORM  P2000_TRANS_METHOD_SET.

* 공동 인수율.
    REFRESH : IT_ZSINSBSG5.
    IT_ZSINSBSG5-ZFLSG5 = '00010'.           " LINE
*>>> IMG로 변경.
    IT_ZSINSBSG5-ZFINSC = ZTIMIMGTX-ZFINSC. " 공동인수사.
    IT_ZSINSBSG5-ZFINSRT = '100'.           " 인수비율.

    APPEND IT_ZSINSBSG5.
    W_ZFTRANS = ZTINSB-ZFTRANS.

    SET SCREEN 4101.  LEAVE TO SCREEN 4101.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_SCR4100  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_BL_INS_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_BL_INS_DOC_SCRCOM INPUT.
*-----------------------------------------------------------------------
* OK-CODE별 분?
*-----------------------------------------------------------------------
  PERFORM  P2000_OK_CODE_PROCESS.
  W_COUNT = 1.
* 입력값 CHECK.
  IF ZSREQHD-ZFHBLNO IS INITIAL.         " B/L NO를 입력하지 않을 경?
    IF ZSREQHD-ZFBLNO IS INITIAL.      " 관리번호가 입력하지 않을 경?
      MESSAGE E304.
    ELSE.
* B/L READ PERFORM ?
      ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
      PERFORM   P1000_READ_BL_DOC.
    ENDIF.
  ELSE.
    IF ZSREQHD-ZFBLNO  IS INITIAL.      " 관리번호가 입력하지 않을 경?
* P/O NO에 Count
      SELECT COUNT( * ) INTO  W_COUNT
                        FROM  ZTINSB
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
      CASE W_COUNT.
        WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
        WHEN 1.
          SELECT ZFBLNO INTO  ZSREQHD-ZFBLNO UP TO 1 ROWS
                        FROM  ZTINSB
                        WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
            EXIT.
          ENDSELECT.
        WHEN OTHERS.
          PERFORM P2000_BL_DOC_ITEM_SELECT.
          IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
          ZSREQHD-ZFBLNO  = W_BLNO.
          ZSREQHD-ZFHBLNO = W_HBLNO.
      ENDCASE.
    ENDIF.
* B/L READ PERFORM ?
    ZTBL-ZFBLNO = ZSREQHD-ZFBLNO.
    PERFORM   P1000_READ_BL_DOC.
  ENDIF.

ENDMODULE.                 " READ_BL_INS_DOC_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_INS_DOC_SCR4200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_INS_DOC_SCR4200 INPUT.

  IF SY-TCODE NE 'ZIMB1'.
    IF ZSREQHD-ZFINSEQ IS INITIAL.
      REFRESH : IT_ZTINS.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTINS
               FROM   ZTINSB
               WHERE  ZFBLNO   EQ ZTBL-ZFBLNO
               ORDER  BY ZFINSEQ.
      IF SY-SUBRC NE 0.
        MESSAGE E172(ZIM1) WITH ZTBL-ZFBLNO.
      ENDIF.
      W_COUNT = SY-DBCNT.
      IF SY-DBCNT EQ 1.
        LOOP AT IT_ZTINS.
          ZSREQHD-ZFINSEQ = IT_ZTINS-ZFINSEQ.
        ENDLOOP.
      ELSE.
        INCLUDE = 'ZTINS'.                 " 반입 정?
        CALL SCREEN 0014 STARTING AT  08 3
                         ENDING   AT  90 15.
        IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
        ZSREQHD-ZFINSEQ = W_INSEQ.
      ENDIF.
    ELSE.
      W_COUNT = 1.
    ENDIF.
  ENDIF.
  PERFORM   P1000_READ_INS_DOC.

ENDMODULE.                 " READ_INS_DOC_SCR4200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INSUARANCE_INCOTEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INSUARANCE_INCOTEM INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTIMIMG00-ZFINMT EQ '2'.
    PERFORM   P2000_INSURANCE_BIT_SET.
  ENDIF.

ENDMODULE.                 " CHECK_INSUARANCE_INCOTEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_COST_AREA_SCR0102  INPUT
*&---------------------------------------------------------------------*
MODULE GET_COST_AREA_SCR0102 INPUT.

  CHECK W_STATUS NE  C_REQ_D.

  IF NOT ZTBL-ZFVIA IS INITIAL AND
     NOT ZTBL-ZFCARC IS INITIAL AND
     NOT ZTBL-ZFSPRTC IS INITIAL.

    IF ZTBL-ZFVIA = 'VSL'.
      ZTBL-ZFCDTY = '014'.
    ELSEIF ZTBL-ZFVIA = 'AIR'.
      ZTBL-ZFCDTY = '015'.
    ENDIF.

    SELECT SINGLE ZFCD INTO ZTBL-ZFCD
              FROM ZTIMIMG23
             WHERE ZFGUBN = ZTBL-ZFCDTY
               AND LAND1 EQ ZTBL-ZFCARC
               AND PORT  EQ ZTBL-ZFSPRTC.

    IF SY-SUBRC NE 0.
      IF ZTBL-ZFSHTY NE 'B'.
        MESSAGE W427(ZIM1) WITH ZTBL-ZFVIA
                                ZTBL-ZFCARC
                                ZTBL-ZFSPRTC.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " GET_COST_AREA_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_AREA_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_AREA_NAME_SCR0102 INPUT.
  CLEAR: W_AREA_NM.
  IF NOT ZTBL-ZFCDTY IS INITIAL AND
     NOT ZTBL-ZFCD   IS INITIAL.
    SELECT SINGLE ZFCDNM INTO W_AREA_NM
             FROM ZTIMIMG08
            WHERE ZFCDTY = ZTBL-ZFCDTY
              AND ZFCD   = ZTBL-ZFCD.
  ENDIF.
ENDMODULE.                 " GET_AREA_NAME_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CONSENT_CHARGE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CONSENT_CHARGE_SCR0102 INPUT.

  IF NOT ZTBL-ZFFRE IS INITIAL.
    ZTBL-ZFCHARGE = 'X'.
  ELSE.
    CLEAR ZTBL-ZFCHARGE.
  ENDIF.

ENDMODULE.                 " CHECK_CONSENT_CHARGE_SCR0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR3520  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR3520 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_3520-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR3520  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_BUDAT_SCR3510  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_BUDAT_SCR3510 INPUT.

  CHECK W_STATUS NE C_REQ_D.

  IF ZTCIVHD-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHD' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_BUDAT_SCR3510  INPUT
