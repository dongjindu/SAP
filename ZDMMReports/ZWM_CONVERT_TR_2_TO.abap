*REPORT ZWM_CONVERT_TR_2_TO .
************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZWM_CONVERT_TR_2_TO
*&
*& Type   : Report/Interface                                           *
*& Author : Manjunath Venkatesh
*&
*& Title  : Program to convert Open TR to TO                           *
*&---------------------------------------------------------------------*
* Help Desk Request No  : 58WE2A3464                                   *
* System Id:    20060124 ( Issue Log No.)                              *
*                                                                      *
*   Requested by:    Quentin Hendry                                    *
*   Assigned to:     Manjunath Venkatesh                              *
*   Original Request #:                                               *
*   ABAP Analyst:    Manjunath Venkatesh                               *
*                                                                      *
* Business Users: Lorry Smith                                          *
*                                                                      *
* Business Requirement Description:                                    *
*   There are many times when a WM Transfer requirement is created     *
* ,but not turned into a transfer order automatically.Many of these are*
* either background jobs that change vehicle specs causing an UN-back  *
* Flush(262) then re-back Flush (261), CLearing COGI(261), or scrapping*
* items(551 &552).                                                     *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*  Select all the open TR's from TR Header & Detail  table.            *
*  Default Control cycle data as Source storage type & BIN for 261     *
*  movement type and as Destinaton type & destination bin for 262      *
*  movement type.                                                      *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:   Program creates TO's for the selected TR's       *
*                                                                      *
*                                                                      *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 02/03/06    Manju        UD1K919235   Initial Coding
* 06/29/06    Manju        UD1K921241   Check for message 019
************************************************************************
REPORT ZWM_CONVERT_TR_2_TO LINE-COUNT 65
                             NO STANDARD PAGE HEADING message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : LTBK,  "Transfer requirement header
         LTBP,  "Transfer requirement item
         PKHD.  "Control Cycle

*-------------------------------------------------------------*
* Constants
*-------------------------------------------------------------*
CONSTANTS :TCODE(4) value 'LT04',
           c_VLPLA(9) value 'INV_COUNT',
           c_VLTYP(3) value '999'.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
Data :WA_VLPLA like LTBK-VLPLA,
      WA_VLTYP like LTBK-VLTYP,
      CTUMODE VALUE 'N',  " No-display
      CUPDATE VALUE 'S'.  " Synchnorous Update

DATA: BEGIN OF MESS OCCURS 0,
        TBNUM LIKE LTBK-TBNUM,
        TBPOS like LTBP-TBPOS.
        INCLUDE STRUCTURE LMESS.
DATA: END OF MESS.

DATA: BEGIN OF IT_SUCC OCCURS 0,
        TBNUM LIKE LTBK-TBNUM,
        TBPOS like LTBP-TBPOS.
DATA: END OF IT_SUCC.
*-------------------------------------------------------------*
* Internal Table Declarations
*-------------------------------------------------------------*
* Internal to hold OPEN TR'S
data : begin of itab_tr occurs 0,
       LGNUM like LTBK-LGNUM,
       TBNUM like LTBK-TBNUM,
       BWART like LTBK-BWART,
       BWLVS like LTBK-BWLVS,
       TBPOS like LTBP-TBPOS,
       MATNR like LTBP-MATNR,
       MENGE like ltbp-menge,
       end of itab_tr.

* Internal Table    for Material.
data : begin of it_matnr occurs 0,
        matnr like mara-matnr,
       end of it_matnr.

* Internal table for Control Cycle
data : begin of it_PKHD occurs 0,
        matnr like pkhd-matnr,
        LGPLA like pkhd-lGPLA,
        LGTYP like pkhd-LGTYp,
        end of it_pkhd.


* BDC Table
DATA:BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA:END OF BDCDATA.

* Message table

DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-Bl1.
select-options : s_TBNUM for ltbk-TBNUM,
                 s_date  for ltbk-BDATU.
SELECTION-SCREEN END OF BLOCK BL1.
SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-Bl2.
parameters : P_261 RADIOBUTTON GROUP r1 default 'X',
             P_262 RADIOBUTTON GROUP r1,
             p_both RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK BL2.



*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
Start-of-selection.

* Select Open TR's
  perform  OPEN_TRS.


*-------------------------------------------------------------*
* End -of-selection
*--------------------------------------------------------------*
END-OF-SELECTION .

* Create TO's
  Perform create_TO.

* Write Log
  Perform write_log.


*&---------------------------------------------------------------------*
*&      Form  OPEN_TRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_TRS.
* Select OPEN TR's for 261- With Status ne 'E'.
  if p_261 eq 'X'.
    select LTBK~LGNUM
           LTBK~TBNUM
           LTBK~BWART
           LTBK~BWLVS
           LTBP~TBPOS
           LTBP~MATNR
           ltbp~menge
           from LTBK inner join LTBP
            on LTBK~LGNUM = LTBP~LGNUM
           and LTBK~TBNUM = LTBP~TBNUM
           into table itab_tr
           where LTBK~LGNUM = 'P01'
           and   ltbk~TBNUm in s_TBNUM
           and   LTBK~STATU ne 'E'
           and   LTBK~bdatu in s_date
           and   LTBK~BWLVS eq '261'
           and   LTBP~ELIKZ eq  ''.

  elseif P_262  eq 'X'.
* Select OPEN TR's for 262- With Status ne 'E'.
    select LTBK~LGNUM
           LTBK~TBNUM
           LTBK~BWART
           LTBK~BWLVS
           LTBP~TBPOS
           LTBP~MATNR
           ltbp~menge
           from LTBK inner join LTBP
            on LTBK~LGNUM = LTBP~LGNUM
              and  LTBK~TBNUM = LTBP~TBNUM
           into table itab_tr
           where LTBK~LGNUM = 'P01'
           and   ltbk~TBNUm in s_TBNUM
           and   LTBK~STATU ne 'E'
           and   LTBK~bdatu in s_date
           and   LTBK~BWLVS eq '262'
           and   LTBP~ELIKZ eq ''.
  else.
* Select OPEN TR's for both 261 / 262- With Status ne 'E'.
    select LTBK~LGNUM
            LTBK~TBNUM
            LTBK~BWART
            LTBK~BWLVS
            LTBP~TBPOS
            LTBP~MATNR
            ltbp~menge
            from LTBK inner join LTBP
             on LTBK~LGNUM = LTBP~LGNUM and
                LTBK~TBNUM = LTBP~TBNUM
            into table itab_tr
            where LTBK~LGNUM = 'P01'
            and   ltbk~TBNUm in s_TBNUM
            and   LTBK~STATU ne 'E'
            and   LTBK~bdatu in s_date
            and   LTBK~BWLVS in ('261','262')
            and   LTBP~ELIKZ eq ''.


  endif.

* Collect unique material & TR qty to check against LQUA available qty
* In case of TO creation failure. So that TR can be cleared.

  loop at itab_tr.
    it_matnr-matnr = itab_tr-matnr.
    collect it_matnr.
  endloop.


* Get  Control Cycle Data
  if not it_matnr[] is initial.
    select matnr LGPLA LGTYP into table it_PKHD
           from PKHD for all entries in it_matnr
            where matnr eq it_matnr-matnr.
  endif.

ENDFORM.                    " OPEN_TRS
*&---------------------------------------------------------------------*
*&      Form  create_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_TO.
  data : WA_LTAP like LTAP_VB occurs 0 with header line,
         WA_MSG like WMGRP_MSG occurs 0 with header line,
         wa_tanum like LTAK-TANUm.

  read table itab_tr index 1.
  if sy-subrc ne 0.
    write :/  text-014.
    stop.
    exit.
  endif.

  if  p_261 eq 'X'.
* Create TO for 261 - TR's
    Perform create_TO_for_261.
  elseif p_262 eq 'X'.
* Create TO for 261 - TR's
    Perform create_TO_for_262.
  else.
* Create TO's for 261/262 - TR's
    Perform create_TO_for_262.
    Perform create_TO_for_261.
  endif.



ENDFORM.                    " create_TO
*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_log.
  data : LMESS like LMESS occurs 0 with header line ,
         HLP_TEXT(300) type c.

 read table  it_succ index 1.
 if sy-subrc eq 0.
  WRITE :/ 'TO  Created'.
  skip 1.
  Uline (26) .
  WRITE: / TEXT-012 COLOR 1.

*  Uline (26) .
  loop at  it_succ.
    write :/  SY-VLINE,
            0(14) it_succ-TBNUM COLOR 2,
            14 SY-VLINE,
            16(9) it_SUCC-TBPOS COLOR 2,
            26 SY-VLINE.
  endloop.
  skip 1.
  ULINE (26) .
endif.
  skip 5.

  read table  MESS index 1.
  check sy-subrc eq 0.

  WRITE :/ 'TO Not Created'.
  skip 1.
  ULINE (130).
  WRITE: / TEXT-011 COLOR 1.
  ULINE (130).


  LOOP AT MESS.
    MOVE-CORRESPONDING MESS TO LMESS.
    CALL FUNCTION 'L_MESSAGE_AUFBEREITEN'
         EXPORTING
              I_MESS = LMESS
         IMPORTING
              O_TEXT = HLP_TEXT.
    SHIFT HLP_TEXT RIGHT BY 20 PLACES.
    HLP_TEXT(10)   = MESS-TBNUM.
    HLP_TEXT+14(6) = MESS-TBPOS.

    FORMAT INTENSIFIED OFF.
    WRITE: / SY-VLINE,
        0(14) HLP_TEXT+0(10) COLOR 2,
        15 SY-VLINE,
        16(8) HLP_TEXT+14(6) COLOR 2,
        30 SY-VLINE,
        32(95) HLP_TEXT+20 COLOR 2,
        130 SY-VLINE.
  endloop.
  ULIne (130).
ENDFORM.                    " write_log
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0269   text
*      -->P_0270   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING  PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0275   text
*      -->P_0276   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING   FNAM FVAL.
  IF FVAL <> ''.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.


ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  create_TO_for_261
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_TO_for_261.
  Loop at ITAB_TR where BWLVS EQ '261'.

* Get Control Cycle data for Material.
    read table it_PKHD with key matnr = itab_tr-matnr.
*    select single * from PKHD where matnr eq ITAB_TR-matnr.
    if sy-subrc eq 0.
      WA_VLPLA  = it_PKHD-LGPLA .
      WA_VLTYP  = it_PKHD-LGTYP.
    else.   " Default
      WA_VLPLA  = C_VLPLA.
      WA_VLTYP  = C_VLTYP.
    endif.

* Prepare Data for BDC
    Perform  prepare_LT04_261.

* CALL LT04
    CALL TRANSACTION TCODE USING BDCDATA
                     MODE   CTUMODE
                     UPDATE CUPDATE
                     MESSAGES INTO MESSTAB.
    if sy-subrc <> 0  and (  sy-msgno eq '037' or
                            sy-msgno eq '344'  or
                            sy-msgno eq '019' ).   "UD1K921241
* In the event that there is not enough stock in the Control Cycle bin
*for the 261 movement  We should use 999 / INV_COUNT just to clear
*the TR.
      refresh BDCdata. clear BDCdata.
* Default
      WA_VLPLA  = C_VLPLA.
      WA_VLTYP  = C_VLTYP.
      Perform  prepare_LT04_261.
      perform  prepare_log.
    elseif sy-subrc <> 0.
*BDC Failed
      MESS-MSGID = SY-MSGID.
      MESS-MSGNO = SY-MSGNO.
      MESS-MSGV1 = SY-MSGV1.
      MESS-MSGV2 = SY-MSGV2.
      MESS-MSGV3 = SY-MSGV3.
      MESS-MSGV4 = SY-MSGV4.
      MESS-TBNUM = ITAB_TR-TBNUM.
      mess-TBPOS = ITAB_TR-TBPOS.
      APPEND MESS.
    elseif sy-subrc eq 0.
*BDC Sucessful
      IT_SUCC-TBNUM = ITAB_TR-TBNUM.
      IT_SUCC-TBPOS = ITAB_TR-TBPOS.
      append it_succ.
    endif.
    refresh BDCdata. clear BDCdata.

  endloop.

ENDFORM.                    " create_TO_for_261
*&---------------------------------------------------------------------*
*&      Form  create_TO_for_262
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_TO_for_262.

* Create TO For 262 TR'S

  Loop at ITAB_TR where BWLVS EQ '262'.

* Get Control Cycle data for Material.
    read table it_PKHD with key matnr = itab_tr-matnr.
*    select single * from PKHD where matnr eq ITAB_TR-matnr.
    if sy-subrc eq 0.
      WA_VLPLA  = it_PKHD-LGPLA .
      WA_VLTYP  = it_PKHD-LGTYP.
    else.    "Default
      WA_VLPLA  = C_VLPLA.
      WA_VLTYP  = C_VLTYP.
    endif.

* Prepare LT04 for 262
    perform prepare_LT04_262.

* CALL LT04
    CALL TRANSACTION TCODE USING BDCDATA
                     MODE   CTUMODE
                     UPDATE CUPDATE
                     MESSAGES INTO MESSTAB.

    if sy-subrc <> 0  and (  sy-msgno eq '037' or
                             sy-msgno eq '344' or
                            sy-msgno eq '019' ).   "UD1K921241


* In the event that there is not enough stock in the Control Cycle bin
*for the 262 movement  We should use 999 / INV_COUNT just to clear
*the TR.
      refresh BDCdata. clear BDCdata.
      WA_VLPLA  = C_VLPLA.
      WA_VLTYP  = C_VLTYP.
      perform prepare_LT04_262.
      perform prepare_log.
    elseif sy-subrc <> 0.
* BDC Failed
      MESS-MSGID = SY-MSGID.
      MESS-MSGNO = SY-MSGNO.
      MESS-MSGV1 = SY-MSGV1.
      MESS-MSGV2 = SY-MSGV2.
      MESS-MSGV3 = SY-MSGV3.
      MESS-MSGV4 = SY-MSGV4.
      MESS-TBNUM = ITAB_TR-TBNUM.
      mess-TBPOS = ITAB_TR-TBPOS.
      APPEND MESS.
    elseif sy-subrc eq 0.
* BDC Sucessful
      IT_SUCC-TBNUM = ITAB_TR-TBNUM.
      IT_SUCC-TBPOS = ITAB_TR-TBPOS.
      append it_succ.
    endif.
    refresh BDCdata. clear BDCdata.

  endloop.

ENDFORM.                    " create_TO_for_262
*&---------------------------------------------------------------------*
*&      Form  LT04_261
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Prepare_LT04_261.
* LT04 Initial Screen
  perform BDC_DYNPRO      using 'SAPML03T' '0131'.
  perform BDC_FIELD       using 'BDC_CURSOR'  'RL03T-DUNKL'.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.
  perform bdc_field       using 'LTAK-LGNUM'  ITAB_TR-LGNUM.
  perform bdc_field       using 'LTBK-TBNUM'  ITAB_TR-TBNUM.
  perform bdc_field       using '*LTBP-TBPOS'  ITAB_TR-TBPOS.
  perform bdc_field       using 'RL03T-ALAKT' 'X'.
  perform bdc_field       using 'RL03T-DUNKL' 'H'.

* 2nd screen
  perform bdc_dynpro      using 'SAPML03T' '0132'.
  perform bdc_field       using 'BDC_OKCODE'   '=TTYP'.
  perform bdc_field       using 'BDC_SUBSCR'   'SAPML03T'  .
  perform bdc_field       using 'BDC_CURSOR'   'LTBP1-OFMEA(01)'.

* 3rd screen
  perform bdc_dynpro      using 'SAPML03T' '0105'.
  perform bdc_field       using 'BDC_CURSOR'   'T334T-LGTY0'.
  perform bdc_field       using 'BDC_OKCODE'   '=TAH2'  .
* 4th Screen

  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform BDC_FIELD       using 'BDC_CURSOR'  'LTAP-VLTYP'.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.
  perform bdc_field       using 'LTAP-VLTYP'   WA_VLTYP.
  perform bdc_field       using 'LTAP-VLPLA'   WA_VLPLA.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.

*5th screen
  perform bdc_dynpro      using 'SAPML03T' '0105'.
  perform bdc_field       using 'BDC_CURSOR'   'T334T-LGTY0'.
  perform bdc_field       using 'BDC_OKCODE'   '=BU'  .

  perform bdc_field       using 'BDC_OKCODE'   '/11'.

ENDFORM.                                                    " LT04_261
*&---------------------------------------------------------------------*
*&      Form  prepare_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_log.
  CALL TRANSACTION TCODE USING BDCDATA
                   MODE   CTUMODE
                   UPDATE CUPDATE
                   MESSAGES INTO MESSTAB.
  if sy-subrc <> 0.
*BDC Failed
    MESS-MSGID = SY-MSGID.
    MESS-MSGNO = SY-MSGNO.
    MESS-MSGV1 = SY-MSGV1.
    MESS-MSGV2 = SY-MSGV2.
    MESS-MSGV3 = SY-MSGV3.
    MESS-MSGV4 = SY-MSGV4.
    MESS-TBNUM = ITAB_TR-TBNUM.
    mess-TBPOS = ITAB_TR-TBPOS.
    APPEND MESS.
  else.
*BDC Sucessful
    IT_SUCC-TBNUM = ITAB_TR-TBNUM.
    IT_SUCC-TBPOS = ITAB_TR-TBPOS.
    append it_succ.
  endif.

ENDFORM.                    " prepare_log
*&---------------------------------------------------------------------*
*&      Form  prepare_LT04_262
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_LT04_262.
* LT04 Initial Screen
  perform BDC_DYNPRO      using 'SAPML03T'    '0131'.
  perform BDC_FIELD       using 'BDC_CURSOR'  'RL03T-DUNKL'.
  perform bdc_field       using 'BDC_OKCODE'  '/00'.
  perform bdc_field       using 'LTAK-LGNUM'  ITAB_TR-LGNUM.
  perform bdc_field       using 'LTBK-TBNUM'  ITAB_TR-TBNUM.
  perform bdc_field       using '*LTBP-TBPOS' ITAB_TR-TBPOS.
  perform bdc_field       using 'RL03T-ALAKT' 'X'.
  perform bdc_field       using 'RL03T-DUNKL' 'H'.

* 2nd screen
  perform bdc_dynpro      using 'SAPML03T' '0132'.
  perform bdc_field       using 'BDC_OKCODE'   '=TPAL'.
  perform bdc_field       using 'BDC_SUBSCR'   'SAPML03T'  .
  perform bdc_field       using 'BDC_CURSOR'   'LTBP1-OFMEA(01)'.

* 3rd screen
  perform bdc_dynpro      using 'SAPML03T' '0104'.
  perform bdc_field       using 'BDC_CURSOR'   'RL03T-ANZL1'.
  perform bdc_field       using 'BDC_OKCODE'   '=TAH1'  .

* 4th Screen
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform BDC_FIELD       using 'BDC_CURSOR'  'LTAP-NLPLA'.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.
  perform BDC_FIELD       using 'RL03T-SQUIT'  'X'.
  perform bdc_field       using 'LTAP-NLTYP'   WA_VLTYP.
  perform bdc_field       using 'LTAP-NLPLA'   WA_VLPLA.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.
  perform bdc_field       using 'BDC_OKCODE'   '/00'.

*5th screen
  perform bdc_dynpro      using 'SAPML03T' '0104'.
  perform bdc_field       using 'BDC_CURSOR'   'RL03T-ANZL1'.
  perform bdc_field       using 'BDC_OKCODE'   '=BU'  .

  perform bdc_field       using 'BDC_OKCODE'   '/11'.

ENDFORM.                    " prepare_LT04_262
