*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZIMM_MODPRICE_MOBIS
*& Program Name   : MOBIS Module Price Interface
*& Created by     : Victor Park
*& Created on     : 12.16.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_FMM_ MODPRICE_MOBIS
*& Stru.     :
*&----------------------------------------------------------------------

REPORT zimm_modprice_mobis MESSAGE-ID zmpp.

TABLES : ztmm_mod_pri_bk.


DATA : it_data LIKE ztmm_mod_pri_bk OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS :  s_date FOR ztmm_mod_pri_bk-run_date  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECTION-SCREEN SKIP.
PARAMETERS : p_re AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP.
PARAMETERS : p_send AS CHECKBOX..

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_send <> 'X'.
    MESSAGE s000 WITH 'Please Check Send Data'.
    STOP.
  ENDIF.

  PERFORM select_data.

  IF p_send = 'X'.
    PERFORM  pro_batch.
  ENDIF.
*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .

  IF p_re = 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztmm_mod_pri_bk
    WHERE zresult = 'E'.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztmm_mod_pri_bk
    WHERE run_date IN s_date.
  ENDIF.

  IF it_data[] IS INITIAL.
    WRITE : / 'There is No data'.
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.

*-replace blnak to '000' for KZUST field
  it_data-kzust = '000'.
  MODIFY it_data TRANSPORTING kzust  WHERE kzust = ''.

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMPM01'.   "Interface Destination.

  CLEAR : e_return.

  CALL FUNCTION 'Z_FMM_MODPRICE_MOBIS' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.

  LOOP AT it_data.

    IF p_type = 'E'.
      IF NOT  p_msg1 IS INITIAL.
        it_data-zmsg  = p_msg1.
      ELSE.
        it_data-zmsg  = p_msg2.
      ENDIF.
    ELSE.
      it_data-zmsg  = p_msg1.
    ENDIF.

    UPDATE   ztmm_mod_pri_bk
      SET zedat     =  sy-datum
          zetim     =  sy-uzeit
          zresult   =  p_type
          zmsg      =  it_data-zmsg
    WHERE matnr   =  it_data-matnr
      AND upgvc   =  it_data-upgvc
      AND comp    =  it_data-comp
      AND lifnr   =  it_data-lifnr
      AND run_date     =  it_data-run_date
      AND run_time     =  it_data-run_time.

    MODIFY it_data.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .

ENDFORM.                    " INIT
