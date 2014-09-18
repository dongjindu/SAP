*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 11/10/2011
*& Specification By       : KDM
*& Pattern                : Report 1-13
*& Development Request No :
*& Addl documentation     :
*& Description  : This is developed use ALV.
*&
*& Modification Log
*&--------------------------------------------------------------------*
*& Date         Developer      Request ID      Description
*&--------------------------------------------------------------------*
REPORT zrfif13N MESSAGE-ID zmfi  NO STANDARD PAGE HEADING
                                LINE-SIZE 200
                                LINE-COUNT 65.


INCLUDE ZRFIT13N_T01.
INCLUDE ZRFIT13N_F01.

*&---------------------------------------------------------------------*
*& INITIALIZATION                                                      *
*&---------------------------------------------------------------------*
INITIALIZATION.

  perform  init_screen.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.
* archive/delete data.
  DELETE FROM fdes WHERE archk <> space
                     AND avdat < sy-datum.

* collect plan, transfer.
  PERFORM init_data.

  PERFORM get_material_master.
* Sales Plan
  IF p_cust = 'X'.
    PERFORM get_kunnr_matnr.   "customer + material
  ENDIF.
* Procurement Plan
  IF p_vend = 'X'.
*    PERFORM get_lifnr_matnr.   "vendor + material
    PERFORM get_lifnr_matnr_new.   "vendor + material
  ENDIF.

  PERFORM determine_date_locamt.

  IF p_test EQ space.
    PERFORM dele_data.
    PERFORM save_data.
  ENDIF.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
END-OF-SELECTION.
  g_repid = sy-repid.

* ALV HEADER & FIELD SETTING
* IF p_mhly EQ 'X'.
  SORT it_alv BY datum pdatu  matnr.

  PERFORM : alvprn_basic01,
            alvprn_field01_monthly USING 'IT_ALV',
            alvprn_sort_build,
            alvprn_list            TABLES it_alv.
* ELSEIF p_yrly EQ 'X'.
*   PERFORM : alvprn_basic01,
*             alvprn_field01_yearly USING 'IT_YLIST',
*             alvprn_list           TABLES it_ylist.
* ENDIF.
