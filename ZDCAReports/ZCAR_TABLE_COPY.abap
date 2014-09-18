************************************************************************
* Program Name      : ZCAR_TABLE_COPY
* Creation Date     :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZCAR_TABLE_COPY NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: ztppaffw.
*CONSTANTS: C_DEST(10) VALUE 'WMPM01'.
DATA: w_dest(10).
*DATA: W_FILENAME LIKE RLGRAP-FILENAME.
data: it_itab LIKE TABLE OF ztppaffw with header line.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE i,
      w_mtart LIKE mara-mtart.

START-OF-SELECTION.


 perform get_data.

form get_data.

  DATA: l_result(1),
        l_totrec TYPE i,
        l_srec TYPE i,
        l_frec TYPE i,
        l_msgtxt(60).


*    w_dest = 'WMPM01'.
    w_dest = 'UP2CLNT300'.
    w_dest = 'UP2300'.
  CALL FUNCTION 'Z_FCA_GET_TABLE_CONTENT'
     DESTINATION w_dest
     IMPORTING
       flag          = l_result
     TABLES
       IT_DATA       = it_ITAB
     EXCEPTIONS
            communication_failure = 1 MESSAGE l_msgtxt
            system_failure        = 2 MESSAGE l_msgtxt.

  IF l_result = 'S' OR l_result = 's'.


    DELETE FROM ZTPPAFFW WHERE ZDATE <> ' '.

    INSERT ZTPPAFFW FROM it_ITAB.
    COMMIT WORK.
  ELSE.
    MESSAGE E999 WITH l_msgtxt.
   ENDIF.
ENDFORM.
