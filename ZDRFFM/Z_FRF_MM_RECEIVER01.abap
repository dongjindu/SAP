FUNCTION Z_FRF_MM_RECEIVER01.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_TYPES) TYPE  ZRF_TYPES
*"     VALUE(I_UNAME) LIKE  SY-UNAME
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_TRUCK STRUCTURE  ZSRF_TRUCK OPTIONAL
*"      T_MESSAGE STRUCTURE  ZSRF_MESSAGE OPTIONAL
*"      T_RECEIVER STRUCTURE  ZSRF_RECEIVER
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------
*& Author                 : Manjunath Venkatesh
*& Creation Date          : 01/13/2006
*& Specification By       : Quentin Hendry (Issue log no: 20060124)
*& Pattern                : RFC Function Module
*& Description            : To Create TO & GR for given Deliveries
*& Addl documentation     : Copy of Z_FRF_MM_RECEIVER Function module
*&                          to speed up LP GR by doing it in background.
*&                          Also Refer Helpdesk ticket # 61PF6945A5 for
*&                          additional information.
*& Modification Log
*& Date     Developer      Request ID  Description
*& 01/25/06 Manjunath      UD1K919066  Performance fine tuning:-
*&                                     Replaced BDC with STD SAP
*&                                     function for LT03 / VL32n and
*&                                     not to update log in ztlog table
*&02/10/06 Manjunath       UD1K919323  Get External identification no
*&                                     & Vendor to use as suffix
*&                                     for background job.
*&02/28/06 Manjunath       UD1K919575  Check for deliveries before
*&                                     submitting background Job
*&04/19/06 Manjunath       UD1K920227  Check Storage type for Stock
*&                                     removal.
*&04/26/06 Manjunath       UD1K920335  Check Material status
*&06/01/06 Manjunath       UD1K920943  Storage strategy changes
*&--------------------------------------------------------------------

* Data Declarations
  DATA: L_CHECK, " ERROR CHECK
        L_TABIX TYPE SY-TABIX,
        l_cnt type i,
        l_index type i.

  DATA BEGIN OF LT_LTAK OCCURS 0.
          INCLUDE STRUCTURE LTAK_VB.
  DATA END OF LT_LTAK.

  data Begin of T_WMGRP_MSG occurs 0.
          include structure WMGRP_MSG.
  data  end of T_WMGRP_MSG.

* GR Data declaration
  data:  lt_worktab like lipov occurs 0 with header line,
         lt_success like lipov occurs 0 with header line,
         lt_error like lipov occurs 0 with header line,
         lt_tvst like tvst occurs 10 with header line,
         lf_success_count type i,
         lf_error_count type i.

  DATA: BEGIN OF LT_TRUCK OCCURS 0,
          RECEIVER TYPE ZSRF_TRUCK-RECEIVER,
          VBELN TYPE ZSRF_TRUCK-VBELN,
          WBSTK TYPE ZSRF_TRUCK-WBSTK,
          KOSTK TYPE ZSRF_TRUCK-KOSTK,
          EXECUTED_FLG TYPE ZSRF_TRUCK-EXECUTED_FLG,
        END OF LT_TRUCK.

  DATA: LT_RECE TYPE ZSRF_RECE_DATA OCCURS 0 WITH HEADER LINE.

  CHECK NOT I_TYPES IS INITIAL.        "External ID CHECK
  CHECK NOT T_RECEIVER[] IS INITIAL.   "External ID CHECK

* Button Click Date & Time
  W_BUTTON_CLICK_DATE = SY-DATUM.
  W_BUTTON_CLICK_TIME = SY-UZEIT.

* Making Inbound Deliveries list which are not posted
  REFRESH T_TRUCK. CLEAR: T_TRUCK. CLEAR L_CHECK.
  PERFORM RECEIVER_ITEM_SEARCH TABLES T_RECEIVER
                                      T_TRUCK
                               USING  I_TYPES
                                      L_CHECK.

  Refresh it_gr. CLEAR it_GR.

  IF NOT T_TRUCK[] IS INITIAL.

    SORT T_TRUCK BY VBELN.


    it_likp[]  = t_truck[].                                 "UD1K920227
*
** Select Unique Materials.
    perform select_line_items.                              "UD1K920227

* Check Material status
    Perform check_material_status.                          "UD1K920335

** Search Storage type statergy for stock placement
    perform  check_statergy.                                "UD1K920227

* Begin of changes - "UD1K920227
** Eliminate Deliveries for which storage type's are not found
    if not it_lips[] is initial.
      loop at it_lips where err_flag  eq 'X'.
        it_error-vbeln   =  it_lips-vbeln.
        it_error-matnr   =  it_lips-matnr.
        it_error-msg     =  it_lips-msg.
        append it_error.
      endloop.
      loop at it_lips where err_flag  eq 'X'.
        read table t_truck with key vbeln = it_lips-vbeln.
        if sy-subrc eq 0.
          delete t_truck  where vbeln = it_lips-vbeln.
        endif.
      endloop.
    endif.
** End of changes - "UD1K920227


    LOOP AT T_TRUCK INTO WA_TRUCK WHERE EXECUTED_FLG NE 'X'.
      L_TABIX = SY-TABIX.

*Delivery  not processed / Total goods movement status
      IF WA_TRUCK-KOSTK = 'A'.       "Deliveries Not Processed

*    Create TO for Inbound Deliveries in Foreground

        CALL FUNCTION 'L_TO_CREATE_DN'
          EXPORTING
            I_LGNUM                          = 'P01'
            I_VBELN                          =  WA_TRUCK-VBELN
*           I_UPDATE_TASK                    =  ' '
            I_COMMIT_WORK                    = 'X'
            I_BNAME                          =  I_UNAME
         IMPORTING
            E_TANUM                          =  LT_LTAK-TANUM
         TABLES
            T_LTAK                           =  LT_LTAK
*           T_LTAP_VB                        =
           T_WMGRP_MSG                       =  T_WMGRP_MSG
         EXCEPTIONS
           FOREIGN_LOCK                     = 1
           DN_COMPLETED                     = 2
           PARTIAL_DELIVERY_FORBIDDEN       = 3
           XFELD_WRONG                      = 4
           LDEST_WRONG                      = 5
           DRUKZ_WRONG                      = 6
           DN_WRONG                         = 7
           SQUIT_FORBIDDEN                  = 8
           NO_TO_CREATED                    = 9
           TEILK_WRONG                      = 10
           UPDATE_WITHOUT_COMMIT            = 11
           NO_AUTHORITY                     = 12
           NO_PICKING_ALLOWED               = 13
           DN_HU_NOT_CHOOSABLE              = 14
           OTHERS                           = 15 .
        IF SY-SUBRC <> 0.   " if not sucessful
          CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
               EXPORTING
                    langu = sy-langu
                    msgid = sy-msgid
                    msgno = sy-msgno
                    msgv1 = sy-msgv1
                    msgv2 = sy-msgv2
                    msgv3 = sy-msgv3
                    msgv4 = sy-msgv4
               IMPORTING
                    text  = wa_text.
          L_CHECK  = 'X'.  "ERROR CHECK
          WA_MESSA = WA_TEXT.
*          WA_EPOSITION = 'TO CREATION ERROR'.
          T_MESSAGE-type = 'E'.
          T_MESSAGE-ID = sy-msgid.
          T_MESSAGE-numbers = sy-msgno.
          T_MESSAGE-message = wa_text.
          APPEND T_MESSAGE.
          WA_TRUCK-EXECUTED_FLG = 'X'.
        else.                 " if TO creation is sucessful
* Post GR for above  created TO In background
* Collect All Inbound Deliveries for which TO are sucessfully
* Created and schedule Background JOB
          WA_TRUCK-EXECUTED_FLG = 'S'.
          it_gr-vbeln = WA_TRUCK-VBELN.
          it_gr-flag = 'X'.
          append it_gr.
        ENDIF.
      ELSE.                     "WA_TRUCK-KOSTK = 'A'.
* If Overall status <> A then Deliveries are either partially processed
* or completed processed - "Processed Deliveries
* For those Deliveries  Post GR directly  In Background

        it_gr-vbeln = WA_TRUCK-VBELN.
        it_gr-flag = 'X'.
        append it_gr.

      ENDIF.                   "WA_TRUCK-KOSTK = 'A'.
      MODIFY T_TRUCK FROM WA_TRUCK INDEX L_TABIX
                                   TRANSPORTING EXECUTED_FLG.
*     M/W MODIFY DATA
      MOVE-CORRESPONDING WA_TRUCK TO LT_RECE.
      CASE WA_TRUCK-EXECUTED_FLG.
        WHEN 'S'.
          LT_RECE-EXECUTED_FLG = 'X'.
          APPEND LT_RECE. CLEAR LT_RECE.
        WHEN 'X'.
          CLEAR LT_RECE.
      ENDCASE.
      CLEAR WA_TRUCK.
    ENDLOOP.
* Schedule Background Job to Post GR's for Inbound Deliveries
    case i_types.
      WHEN 'TRUCK' OR 'VENDOR'.                             "UD1K919323
        read table T_RECEIVER index 1.
        if sy-subrc eq 0.
          l_lifex =  T_RECEIVER-lifex.
          l_lifnr =  T_RECEIVER-lifnr.
        endif.
      when 'UNIT'.
        l_lifex =  'UNIT'.
        l_lifnr =  'UNIT'.
      when others.
        l_lifex =  ''.
        l_lifnr =  ''.
    endcase.
    clear l_cnt.
    describe table it_gr lines l_cnt.                       "UD1K919575
    if l_cnt > 0.                                           "UD1K919575
      perform schedule_GRPOSTING_JOB tables it_gr.
    endif.                                                  "UD1K919575

* Send email for deliveires in error.
    if not it_error[] is initial.
      perform send_email.                                   "UD1K920227
    endif.

    CASE I_TYPES.
      WHEN 'TRUCK'.

      WHEN 'UNIT'.
        LOOP AT T_TRUCK.
          MOVE-CORRESPONDING T_TRUCK TO LT_TRUCK.
          APPEND LT_TRUCK. CLEAR LT_TRUCK.
        ENDLOOP.
        REFRESH T_TRUCK. CLEAR T_TRUCK.
        SORT LT_TRUCK BY RECEIVER
                         EXECUTED_FLG DESCENDING.
        DELETE ADJACENT DUPLICATES FROM LT_TRUCK COMPARING RECEIVER.
        LOOP AT LT_TRUCK.
          MOVE-CORRESPONDING LT_TRUCK TO T_TRUCK.
          APPEND T_TRUCK. CLEAR T_TRUCK.
        ENDLOOP.
      WHEN 'VENDOR'.

    ENDCASE.

*/ Log Message Processing
    CLEAR: WA_LIPS, W_QTY_MESSAGE.

    IF L_CHECK NE 'X'.
      E_MESS  = TEXT-M22.
      ZRESULT = TEXT-M04.
    ELSE.
      ZRESULT = TEXT-M02.
      E_MESS  = WA_TEXT.
    ENDIF.
  ELSE.
    IF L_CHECK EQ 'X'.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M21.

    ELSE.
      ZRESULT = TEXT-M02.
      E_MESS  = TEXT-M01.
    ENDIF.
  ENDIF.           "NOT T_TRUCK[] IS INITIAL.
  PERFORM MIDDLEWARE_UPLODA_RECEIVER TABLES  LT_RECE.

ENDFUNCTION.
