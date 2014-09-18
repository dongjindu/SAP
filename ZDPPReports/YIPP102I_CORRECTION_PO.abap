************************************************************************
* Program Name      : YIPP102I_CORRECTION_PO
* Author            : Bobby
* Creation Date     : 2003.09.04.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : PLAN ORDER DELETE
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  YIPP102I_CORRECTION_PO MESSAGE-ID zmpp LINE-SIZE 150  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: ztpp_pp_log_head,       " Table of the Interface Log(Header)
        ztpp_pp_log_deta,       " Table of the Interface Log(Detail)
        equi ,
        PLAF,
        ausp .

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_PLAF        OCCURS 0.
        INCLUDE STRUCTURE     PLAF    .
DATA:   MARK,
      END OF IT_PLAF.

DATA: BEGIN OF IT_MSG         OCCURS 0.
        INCLUDE STRUCTURE     PLAF    .
        INCLUDE STRUCTURE     BAPIRET1.
DATA: END OF IT_MSG.

DATA: wa_material             LIKE mara-matnr                 ,
      SV_CODE                 LIKE SY-UCOMM                   ,
      OK_CODE                 LIKE SY-UCOMM                   .
data: W_LINES(5) type C.
CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*
FIELD-SYMBOLS: <field1>       TYPE ANY                        .

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
*DATA: BEGIN OF wa_vehicle    OCCURS 0       .
*        INCLUDE STRUCTURE    ztpp_pmt07jb_b .
*DATA:   workorder            LIKE mara-matnr,
**       instance             LIKE mara-cuobf,
*        sorder(10)           TYPE c         ,
*        porder               LIKE plaf-plnum,
*        equnr                LIKE BAPI_ITOB_PARMS-EQUIPMENT,
*        matnr                LIKE mara-matnr,
*        b_serial(6)          TYPE n         ,
*        vin                  LIKE mara-matnr,
*        e_flag               TYPE c         ,
*      END OF wa_vehicle                     .

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: p_PLNUM          FOR PLAF-PLNUM ." OBLIGATORY .
select-options: p_PSTTR          for plaf-PSTTR .
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM record_processing.

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM record_processing.
  DATA: l_date(10)           TYPE c          ,
        l_fsc                LIKE mara-matnr ,
        l_vin                LIKE mara-matnr ,
        L_RETURN             LIKE BAPIRETURN1,
        l_mode               LIKE ztpp_common_vals-key2,
        L_LINES TYPE I.
  data: ls_resb     TYPE ppc1tp_resb,
        ls_plaf     TYPE ppc1tp_plaf,
        ls_return   TYPE bapiret2,
        ls_conf_del TYPE bapi_ppc_conf_orderid,
        lt_resb     TYPE TABLE OF ppc1tp_resb,
        lt_plaf     TYPE TABLE OF ppc1tp_plaf,
        lt_resb_del TYPE TABLE OF ppc1tp_resb,
        lt_plaf_del TYPE TABLE OF ppc1tp_plaf,
        lt_conf_del TYPE TABLE OF bapi_ppc_conf_orderid.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PLAF
    FROM PLAF
   WHERE PLNUM IN P_PLNUM
     and PSTTR in p_PSTTR
     AND KNTTP = 'M'.
*     AND AUFFX = 'X'.

  DESCRIBE TABLE IT_PLAF LINES L_LINES.
  WRITE L_LINES TO W_LINES.
  CALL SCREEN 9000  .

loop at it_plaf.
  move-corresponding it_plaf to ls_plaf.
  append ls_plaf to lt_plaf.
endloop.

LOOP AT lt_plaf INTO ls_plaf.

    SELECT *
        FROM ppc1tp_resb
        INTO TABLE lt_resb
        WHERE rsnum = ls_plaf-rsnum.

    APPEND ls_plaf TO lt_plaf_del.
    APPEND LINES OF lt_resb TO lt_resb_del.
    REFRESH lt_resb.

*   when required, set deletion flag at confirmation level
*    IF if_delconf EQ gc_true.
      MOVE ls_plaf-plnum TO ls_conf_del-orderid.
      APPEND ls_conf_del TO lt_conf_del.
*    ENDIF.



*---
* Set deletion flag for confirmations: they can be archived now
  IF NOT lt_conf_del IS INITIAL.
    CALL FUNCTION 'PPC1DC_CONF_SET_DEL_FLAG'
         TABLES
              it_conf_orderid = lt_conf_del
         EXCEPTIONS
              update_error    = 1
              OTHERS          = 2.
    IF sy-subrc <> 0.
      write:/ 'Confirm',sy-msgid, sy-msgty, sy-msgno,
            sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.    ENDIF.
  ENDIF.



* Delete entries from PLAF and RESB
  CALL FUNCTION 'PPC1TP_UPDATE_DELETE_PLAF_RESB' IN UPDATE TASK
       TABLES
            i_ppc1tp_plaf_del = lt_plaf_del
            i_ppc1tp_resb_del = lt_resb_del
       EXCEPTIONS
            OTHERS            = 1.
  IF sy-subrc <> 0.

    write:/ 'Delete:',sy-msgid, sy-msgty, sy-msgno,
            sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.
  ENDIF.


*---
* Commit the changes to database

    COMMIT WORK.  "no wait, as we delete



  refresh: lt_plaf_del, lt_resb_del, lt_conf_del, lt_resb.
  clear:   ls_plaf, ls_conf_del.



  ENDLOOP.


*  LOOP AT IT_PLAF  WHERE MARK = 'X' .
*     CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
*       EXPORTING
*         plannedorder          = IT_PLAF-PLNUM
*       IMPORTING
*         RETURN                = L_RETURN .
*     CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'  .
**      EXPORTING
**        WAIT          = 'X' .
**      IMPORTING
**        RETURN        =
*
*
*     MOVE-CORRESPONDING IT_PLAF  TO IT_MSG .
*     MOVE-CORRESPONDING L_RETURN TO IT_MSG .
*     APPEND IT_MSG.
*  ENDLOOP.
*
*  LOOP AT IT_MSG.
*    WRITE: / IT_MSG-PLNUM, IT_MSG-TYPE, IT_MSG-MESSAGE.
*  ENDLOOP.
ENDFORM.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Module  SAVE_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SAVE_SCREEN INPUT.
  MODIFY IT_PLAF INDEX TC_9000-CURRENT_LINE.
ENDMODULE.                 " SAVE_SCREEN  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SV_CODE.
    WHEN 'BACK'.
       LEAVE TO SCREEN 0.
    WHEN 'CANC'.
       LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
