************************************************************************
* Program Name      : ZRSD03R_INVENTORY_STATUS
* Author            : jun ho choi
* Creation Date     : 2003.11.25.
* Specifications By : jun ho choi
* Pattern           : Report 1-2
* Development Request No : UD1K904835
* Addl Documentation:
* Description       : Inventory Status in Plant
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zrsd03r_inventory_status NO STANDARD PAGE HEADING
                                MESSAGE-ID zmsd.

*
TABLES : zssd_inv_st, kna1, vbak.


*
DATA : BEGIN OF it_ausp OCCURS 0.
        INCLUDE STRUCTURE ausp.
DATA : END OF it_ausp.

RANGES r_atinn FOR ausp-atinn OCCURS 0.

DATA : BEGIN OF it_cabn OCCURS 0,
       atinn LIKE cabn-atinn,
       atnam LIKE cabn-atnam,
       END OF it_cabn.

DATA : it_inv_st TYPE STANDARD TABLE OF zssd_inv_st WITH NON-UNIQUE
                 DEFAULT KEY INITIAL SIZE 100,
       t_inv_st TYPE zssd_inv_st.

DATA : w_cnt TYPE i,
       w_sign_off_cnt TYPE i,
       w_atinn LIKE cabn-atinn,
       w_atnam LIKE cabn-atnam.

FIELD-SYMBOLS : <fs>.
DATA : field(30).

DATA : ok_code(4),
       save_ok_code(4).
DATA : alv_grid       TYPE REF TO cl_gui_alv_grid,
       container      TYPE REF TO cl_gui_custom_container.
DATA : gs_variant     TYPE disvariant.
DATA : gs_layout TYPE lvc_s_layo.


*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr,
                s_date  FOR sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.


*
START-OF-SELECTION.
  PERFORM init_cabn.
  PERFORM read_data.
  PERFORM modify_data.
  PERFORM choice_collect_data.

*
END-OF-SELECTION.
  PERFORM call_screen.





  INCLUDE zrsd03r_inventory_status_f01.
  INCLUDE zrsd03r_inventory_status_pbo.
  INCLUDE zrsd03r_inventory_status_pai.
