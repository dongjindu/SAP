FUNCTION z_fpp_hma_fillvin.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_ZVIN_INIT) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_NATION) TYPE  ZTPP_VM-WO_NATION OPTIONAL
*"  EXPORTING
*"     REFERENCE(STATUS) LIKE  BAPIRETURN-TYPE
*"     REFERENCE(MESSAGE) LIKE  BAPIRETURN-MESSAGE
*"  TABLES
*"      ITEM STRUCTURE  ZPOSEG2 OPTIONAL
*"----------------------------------------------------------------------
***REF KMMG ZSDR03510T
  DATA : lt_data LIKE TABLE OF zposeg2 WITH HEADER LINE,
         lt_um   LIKE TABLE OF ztsd_um WITH HEADER LINE.

  DATA : lv_status LIKE status,
         lv_message LIKE message.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_um
       FROM ztsd_um
    WHERE status    = ''
     AND ( body_no   = '' OR
           body_no   = '000000' ).

  CHECK NOT lt_um[] IS INITIAL.

*# Get Available Body No From ZTPP_VM and Delete Using Body No
  PERFORM findnofilled TABLES  lt_um USING i_zvin_init i_nation.

*# Fill Body
  PERFORM fillofcar TABLES lt_um USING lv_status lv_message.

  MOVE : lv_status TO status,
         lv_message TO message.


ENDFUNCTION.
