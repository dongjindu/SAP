*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04510T
*& Program Name   : Create AFFW Backlog
*& Created by     : Victor Park
*& Created on     : 02.24.2012
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&
*&======================================================================
*& RFC func. :
*& Stru.     :
*&----------------------------------------------------------------------
REPORT zppi04510t MESSAGE-ID zmpp.

TABLES : affw, ztppaffw.

DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztppaffw.
DATA :  rsnum	TYPE rsnum,
        rspos	TYPE rspos,
       END OF it_data.

DATA : it_data_tmp LIKE it_data OCCURS 0 WITH HEADER LINE.
DATA : it_data_qty LIKE it_data OCCURS 0 WITH HEADER LINE.

*DATA : wa_save TYPE ztppaffw.
DATA : it_save TYPE STANDARD TABLE OF ztppaffw WITH HEADER LINE.


*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS : p_save(1) DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initial.

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
  CHECK p_save = 'X'.

  PERFORM select_data.

* by ig.moon 2/27/2012 {
*  PERFORM modify_data.
  PERFORM modify_data_new.
* }
  PERFORM save_data.



*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM affw AS a  INNER JOIN makt AS b
                ON a~matnr = b~matnr
  WHERE a~kzear = ''
    AND a~msgno <> ''.


  IF it_data[] IS INITIAL.
    MESSAGE s011.
    STOP.
  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  it_data_tmp[]  = it_data[].
  SORT it_data_tmp BY ersda matnr msgid msgno.
  DELETE ADJACENT DUPLICATES FROM it_data_tmp
                    COMPARING ersda matnr msgid msgno.

  LOOP AT it_data.

    it_data_qty-ersda = it_data-ersda.
    it_data_qty-matnr = it_data-matnr.
    it_data_qty-erfmg = it_data-erfmg.
    it_data_qty-msgid = it_data-msgid.
    it_data_qty-msgno = it_data-msgno.

    COLLECT it_data_qty.
  ENDLOOP.

  SORT it_data_qty BY ersda matnr msgid msgno.

  LOOP AT it_data_tmp.
    MOVE-CORRESPONDING it_data_tmp TO it_save.

    CLEAR : it_data_qty.
    READ TABLE it_data_qty WITH KEY ersda = it_data_tmp-ersda
                                  matnr = it_data_tmp-matnr
                                  msgid = it_data_tmp-msgid
                                  msgno = it_data_tmp-msgno
                                  BINARY SEARCH.
    IF sy-subrc = 0.
      it_save-erfmg  = it_data_qty-erfmg.
    ENDIF.

    APPEND it_save.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM save_data .
  DATA : l_zseq(10) TYPE n.

  SELECT zseq INTO l_zseq
    FROM ztppaffw
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit+0(2).
    it_save-log_time  =  sy-uzeit.
*    it_save-ernam = sy-uname.
    it_save-weblnr  = sy-datum.
    it_save-weblpos = l_zseq + sy-tabix.
    MODIFY it_save.
  ENDLOOP.

  INSERT ztppaffw FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.

    MESSAGE s000 WITH 'Data was saved successfuly'.
  ELSE.
    MESSAGE s000 WITH 'Error occured during  Process'.
  ENDIF.


ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  INITIAL
*&---------------------------------------------------------------------*
FORM initial .

ENDFORM.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data_new .

  CHECK NOT it_data[] IS INITIAL.

  DATA : BEGIN OF it_rsnum OCCURS 0,
          rsnum	TYPE rsnum,
          rspos	TYPE rspos,
         END OF it_rsnum.

  SELECT rsnum rspos INTO TABLE it_rsnum
    FROM resb
    FOR ALL ENTRIES IN it_data
    WHERE rsnum = it_data-rsnum
      AND rspos = it_data-rspos
      AND xloek = 'X'.

  SORT it_rsnum BY rsnum rspos.

  LOOP AT it_data.
    READ TABLE it_rsnum WITH KEY rsnum = it_data-rsnum
                                 rspos = it_data-rspos
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
    ELSE.
      MOVE '0000' TO it_data-erzet+2(4).
      CLEAR : it_data-weblnr, it_data-weblpos.

      it_save = it_data.
      COLLECT it_save.
*      APPEND it_save.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " MODIFY_DATA_NEW
