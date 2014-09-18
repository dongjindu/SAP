*----------------------------------------------------------------------*
*   INCLUDE ZXM02U05                                                   *
*----------------------------------------------------------------------*
************************************************************************
*& Modification Log
*& Date     Developer      Request ID  Description
*& 03/03/06 Manjunath      UD1K919624  Backout VAATZ user exit changes
*&                                     for released PR's
*& 06/13/06 Manju          UD1K921054  Uncomment VAATZ user exit changes
*& 12/07/06 Manju          UD1K923361  Allow changes to PR when status
*                                      <> 'S'
* 12/13/06  Manju          UD1K923414  Add new TCODE to checklist
**********************************************************************
TABLES: MARA.
*Begin of changes -  UD1K919624 - Backout Vaatz user exit
*---// PR Change is not allowed with released PR
*-- // 2006.02.15 create by sgcho.
DATA: wa_messages TYPE bapiret2.

DATA: wa_new_data TYPE mereq_item,
      wa_data_persistent type mereq_item.

DATA: lv_banfn    LIKE eban-banfn.

DATA : LS_MEREQ_ITEM TYPE MEREQ_ITEM,
       W_BSTRF LIKE MARC-BSTRF      , "Rounding Value
       W_REMAIN LIKE MEREQ_ITEM-MENGE,
       W_MSG(10)                    , "Pallet QTY
       W_MATNR LIKE MARA-MATNR.

DATA : PRGID LIKE SY-REPID VALUE 'ZEMMPM38E_MESSAGE',
       W_TMP LIKE MARC-BSTRF,
       flag.

data : begin of it_EBaN occurs 0,
        BANFN like EBKN-BANFN,
        BNFPO like ebkn-BNFPO,
        zztype1 like eban-zztype1,
       end of it_eban.

FIELD-SYMBOLS: <BANFO>.


* If account assigment category is 'U'
* Raise Error Message
if im_data_new-KNttp eq 'U'.
  CLEAR wa_messages.
  REFRESH ex_messages.
  MOVE: 'ZMM_IF' TO wa_messages-id,
        'E'      TO wa_messages-type,
        '006'    TO wa_messages-number.
  APPEND wa_messages TO ex_messages.
endif.


CHECK sy-tcode = 'ME51N'                                    "UD1K923414
   OR sy-tcode = 'ME52N'
   OR sy-tcode = 'ME52'    "Doesn't invoke userexit
   OR sy-tcode = 'ME59N'
   or sy-tcode = 'ME22N'
   or sy-tcode = 'ME22'  "Doesn't change PR
   OR sy-tcode = 'ME23N'                                   "UD1K923414
   OR sy-tcode = 'ME53N'.                                  "UD1K923414

* if Status is 'S' do not allow to add new line items or
* change any item line details other than G/L account,
* Cost center and Internal Order. But using IT_TABLE mainteance
* User id any changes can be made to PR
wa_new_data = im_data_new.
wa_data_persistent = im_data_persistent.
clear: wa_new_data-erdat,wa_data_persistent-erdat,
       wa_new_data-GSWRT,wa_data_persistent-GSWRT.

if   im_data_new-ZZTYPE1  eq 'S'  and
     ( im_data_new-BSART eq 'NB' OR
       im_data_new-BSART eq 'PM' OR
       im_data_new-BSART eq 'ZB' ) and
       wa_new_data NE wa_data_persistent and
       sy-uname ne 'IT_TBL*'.
  CLEAR wa_messages.
  REFRESH ex_messages.
  MOVE: 'ZMM_IF' TO wa_messages-id,
        'E'      TO wa_messages-type,
        '005'    TO wa_messages-number.
  APPEND wa_messages TO ex_messages.
ENDIF.



* Don't allow to add new line if PR is already interfaced
* to VAATZ - If one of the line item has status 'S'.
if   im_data_new-ZZTYPE1  eq ''  and
     ( im_data_new-BSART eq 'NB' OR
       im_data_new-BSART eq 'PM' ) and
       im_data_new NE im_data_old .
  clear flag .

* GET PR Number from MAIN Screen
  ASSIGN ('(SAPLMEGUI)MEREQ_TOPLINE-BANFN_EXT') TO <BANFO>.

  if <BANFO> is assigned.
    select  BANFN
            BNFPO
            zztype1
            into table it_eban
            from eban
            where BANFN = <BANFO> .

    loop at it_eban.
      check it_eban-ZZTYPE1 eq 'S' .
      flag = 'X' .
      exit.
    endloop.
    if flag = 'X'.
      MOVE: 'ZMM_IF' TO wa_messages-id,
            'E'      TO wa_messages-type,
            '005'    TO wa_messages-number.
      APPEND wa_messages TO ex_messages.

    endif .
  endif.
endif.

* If Released and status ne 'S' allow changes
* to PR

IF im_data_persistent-frgkz EQ '5'  and                     "UD1K923361
*   im_data_persistent-ZZTYPE1  ne 'S' . "UD1K923361
    im_data_persistent-ZZTYPE1  eq 'S' .
*  IF im_data_new NE im_data_persistent.
  if wa_new_data  ne wa_data_persistent.
    CLEAR wa_messages.
    REFRESH ex_messages.
    MOVE: 'ZMM_IF' TO wa_messages-id,
          'E'      TO wa_messages-type,
          '004'    TO wa_messages-number.
    APPEND wa_messages TO ex_messages.
*        ex_recheck_item = 'X'.
  ENDIF.
ENDIF.



*----- Lexicographical
LS_MEREQ_ITEM = IM_REQ_ITEM->GET_DATA( ).
SELECT SINGLE * FROM MARA WHERE MATNR = LS_MEREQ_ITEM-MATNR.
IF SY-SUBRC NE 0.
  CONCATENATE '0000000000' LS_MEREQ_ITEM-MATNR INTO W_MATNR.
  SELECT SINGLE * FROM MARA WHERE MATNR = W_MATNR.
  IF SY-SUBRC EQ 0.
    LS_MEREQ_ITEM-MATNR = W_MATNR.
    CALL METHOD IM_REQ_ITEM->SET_DATA
      EXPORTING
         IM_DATA = LS_MEREQ_ITEM.
  ENDIF.
ENDIF.

* clear eban values if no PReq item
IF IM_REQ_ITEM IS INITIAL.
  CLEAR : EBAN.
ELSE.
* read item data from system
  LS_MEREQ_ITEM = IM_REQ_ITEM->GET_DATA( ).

*Get Rounding value
  SELECT SINGLE BSTRF
           INTO W_BSTRF
           FROM MARC
          WHERE WERKS = LS_MEREQ_ITEM-WERKS
            AND MATNR = LS_MEREQ_ITEM-MATNR.

  CHECK W_BSTRF > 0.

*Check Rounding and menge
  W_REMAIN = LS_MEREQ_ITEM-MENGE MOD W_BSTRF.

  IF W_REMAIN > 0.
    W_TMP = W_BSTRF - W_REMAIN + LS_MEREQ_ITEM-MENGE.
    WRITE : W_TMP TO W_MSG UNIT LS_MEREQ_ITEM-MEINS CENTERED.
    SUBMIT (PRGID)  WITH P_MSG  = W_MSG
                     AND RETURN       .
  ELSE.

  ENDIF.
ENDIF.
