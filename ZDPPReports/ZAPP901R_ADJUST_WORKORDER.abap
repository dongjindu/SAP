************************************************************************
* Program Name      : ZAPP901R_ADJUST_WORKORDER
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'ADJUST_WORKORDER
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zapp901r_adjust_workorder  NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ausp.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: it_ausp                LIKE TABLE OF ausp        WITH HEADER LINE,
      it_HEAD                LIKE TABLE OF ausp        WITH HEADER LINE,
      BEGIN OF it_aehd OCCURS 0,
        sqdt(8), "TYPE ZTPP_PMT07JB_A-SQDT,
      END OF it_aehd.

*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: wa_error                 TYPE c,
      wa_check                 TYPE c,
      wa_atinn                 LIKE ausp-atinn,
      wa_atnam                 LIKE cabn-atnam.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
  p_char                     LIKE cabn-atnam  OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1.

START-OF-SELECTION.
  PERFORM check_characteristic .
  PERFORM update_process       .

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  CHECK_CHARACTERISTIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_characteristic.
  DATA: l_matnr              LIKE mara-matnr.

  SELECT SINGLE atinn INTO wa_atinn
    FROM cabn
   WHERE atnam = p_char .

  IF sy-subrc NE 0.
    MESSAGE i001 WITH text-001.
    wa_error = 'X'            .
  ELSE.
    CONCATENATE 'E0312' '%'   INTO l_matnr.
    SELECT *  INTO TABLE  it_ausp
      FROM ausp
     WHERE objek IN ( select MATNR from MARA WHERE matnr LIKE l_matnr
                                               AND MTART = 'WOCL'     )
       AND klart = '001'
       AND atinn = wa_atinn .

    SELECT *  INTO TABLE  it_HEAD
      FROM ausp
     WHERE objek IN ( select MATNR from MARA WHERE matnr LIKE l_matnr
                                               AND MTART = 'WOHD'     )
       AND klart = '001'
       AND atinn = wa_atinn .
  ENDIF.
ENDFORM.                    " CHECK_CHARACTERISTIC

*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_process.
  DATA: L_AUSP               LIKE AUSP    ,
        L_WOSUM              LIKE ZTPP_WOSUM,
        L_MTART              LIKE MARA-MTART,
        l_vals(8)            type n         ,
        l_atinn              LIKE wa_atinn.

  CHECK wa_error = space .
  CASE p_char        .
    WHEN 'P_MOD_QTY' .
*      LOOP AT it_ausp.
*        SELECT SINGLE *    INTO L_WOSUM
*          FROM ZTPP_WOSUM
*         WHERE WO_SER = IT_AUSP-OBJEK(9)
*           AND NATION = IT_AUSP-OBJEK+9(3)
*           AND DEALER = IT_AUSP-OBJEK+12(2)
*           AND EXTC   = IT_AUSP-OBJEK+14(2)
*           AND INTC   = IT_AUSP-OBJEK+16(2) .
*
*        SELECT SINGLE * INTO L_AUSP
*          FROM ausp
*         WHERE objek = it_ausp-objek
*           AND atinn = WA_atinn
*           AND klart = '001' .
*
*        L_ausp-atflv = L_WOSUM-MODQTY .
*        MODIFY ausp  FROM L_AUSP.
*      ENDLOOP.

      LOOP AT it_HEAD.
        SELECT SUM( MODQTY ) INTO L_WOSUM-MODQTY
          FROM ZTPP_WOSUM
         WHERE WO_SER = IT_HEAD-OBJEK(9)
           AND NATION = IT_HEAD-OBJEK+9(3)
           AND DEALER = IT_HEAD-OBJEK+12(2) .

        SELECT SINGLE * INTO L_AUSP
          FROM ausp
         WHERE objek = it_head-objek
           AND atinn = WA_atinn
           AND klart = '001' .

        L_ausp-atflv = L_WOSUM-MODQTY .
        MODIFY ausp  FROM L_AUSP.
      ENDLOOP.
    WHEN 'P_INIT_QTY'.
*      LOOP AT it_ausp.
*        SELECT SINGLE *    INTO L_WOSUM
*          FROM ZTPP_WOSUM
*         WHERE WO_SER = IT_AUSP-OBJEK(9)
*           AND NATION = IT_AUSP-OBJEK+9(3)
*           AND DEALER = IT_AUSP-OBJEK+12(2)
*           AND EXTC   = IT_AUSP-OBJEK+14(2)
*           AND INTC   = IT_AUSP-OBJEK+16(2) .
*
*        SELECT SINGLE * INTO L_AUSP
*          FROM ausp
*         WHERE objek = it_ausp-objek
*           AND atinn = WA_atinn
*           AND klart = '001' .
*
*        L_ausp-atflv = L_WOSUM-INITQTY .
*        MODIFY ausp  FROM L_AUSP.
*      ENDLOOP.

      LOOP AT it_HEAD.
        SELECT SUM( INITQTY ) INTO L_WOSUM-INITQTY
          FROM ZTPP_WOSUM
         WHERE WO_SER = IT_HEAD-OBJEK(9)
           AND NATION = IT_HEAD-OBJEK+9(3)
           AND DEALER = IT_HEAD-OBJEK+12(2) .

        SELECT SINGLE * INTO L_AUSP
          FROM ausp
         WHERE objek = it_head-objek
           AND atinn = WA_atinn
           AND klart = '001' .

        L_ausp-atflv = L_WOSUM-INITQTY.
        MODIFY ausp  FROM L_AUSP.
      ENDLOOP.
    WHEN 'P_WO_MODI_DATE'  .
      append lines of it_head to it_ausp.
      LOOP AT it_ausp.
        SELECT SINGLE WOMODDATE INTO L_WOSUM-WOMODDATE
          FROM ZTPP_WOSUM
         WHERE WO_SER = IT_AUSP-OBJEK(9)
           AND NATION = IT_AUSP-OBJEK+9(3)
           AND DEALER = IT_AUSP-OBJEK+12(2) .

        SELECT SINGLE * INTO L_AUSP
          FROM ausp
         WHERE objek = it_ausp-objek
           AND atinn = WA_atinn
           AND klart = '001' .

        L_ausp-atflv = l_vals = L_WOSUM-WOMODDATE .
        MODIFY ausp  FROM L_AUSP.
      ENDLOOP.
    WHEN 'P_WO_CREATE_DATE'.
      append lines of it_head to it_ausp.
      LOOP AT it_ausp.
        SELECT SINGLE WOCREDATE INTO L_WOSUM-WOCREDATE
          FROM ZTPP_WOSUM
         WHERE WO_SER = IT_AUSP-OBJEK(9)
           AND NATION = IT_AUSP-OBJEK+9(3)
           AND DEALER = IT_AUSP-OBJEK+12(2) .

        SELECT SINGLE * INTO L_AUSP
          FROM ausp
         WHERE objek = it_ausp-objek
           AND atinn = WA_atinn
           AND klart = '001' .

        L_ausp-atflv = l_vals = L_WOSUM-WOCREDATE .
        MODIFY ausp  FROM L_AUSP.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " UPDATE_PROCESS
