FUNCTION z_mm_if_ib_02_007.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0053 OPTIONAL
*"----------------------------------------------------------------------

  DATA : l_mtyp    TYPE bwart,
         l_body_53 LIKE zmmt0053,
         l_return  LIKE zmms0053,
         return    LIKE TABLE OF bapiret2 WITH HEADER LINE.
  DATA : lv_zzcdate TYPE  ztimestamp,
         tmp_zzcdate LIKE zmmt0053-zzcdate,
         wa_body_53  TYPE zmmt0053.

  CONSTANTS : c_werks LIKE marc-werks VALUE 'P001',
              c_bwart LIKE mseg-bwart VALUE '311',
              c_meins LIKE mseg-meins VALUE 'EA'.

  CLEAR : gt_body_53,
          gt_body_53[].

  CONCATENATE sy-datum sy-uzeit INTO lv_zzcdate.

*  SELECT * FROM ZMMT0053
*APPENDING CORRESPONDING FIELDS OF TABLE IT_BODY
*   WHERE TYPE NE 'S'.

  IF it_body[] IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = text-m01.
    EXIT.
*  ELSE.
*    DELETE ADJACENT DUPLICATES FROM it_body. "commented on 06.20.2012
  ENDIF.

  gt_body_53[] = it_body[].

  SORT gt_body_53[] BY budat.

  PERFORM bapi_clear.

  LOOP AT gt_body_53.
    tmp_zzcdate  =  gt_body_53-zzcdate.

*MOVE HEADER INFO.
*    AT NEW budat.
      CLEAR : goodsmvt_header,
              goodsmvt_item, goodsmvt_item[].
      goodsmvt_header-pstng_date = gt_body_53-budat.
      goodsmvt_header-doc_date   = sy-datum.
*    ENDAT.

    goodsmvt_header-header_txt = gt_body_53-sgtxt. "added on 06.20.2012

*move item information
    MOVE: gt_body_53-matnr     TO goodsmvt_item-material,
          gt_body_53-lgort     TO goodsmvt_item-stge_loc,  " source stl
          gt_body_53-umlgo     TO goodsmvt_item-move_stloc," Target Stl
          gt_body_53-menge     TO goodsmvt_item-entry_qnt,
          c_bwart              TO goodsmvt_item-move_type, "
          c_meins              TO goodsmvt_item-entry_uom,
          c_werks              TO goodsmvt_item-plant,     " source Pnt
          c_werks              TO goodsmvt_item-move_plant." Target Pnt
    APPEND goodsmvt_item.

*    AT END OF budat.   "Victor commented on 06.20.2012
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = goodsmvt_header
        goodsmvt_code    = '04'
      IMPORTING
        materialdocument = gt_body_53-mblnr
      TABLES
        goodsmvt_item    = goodsmvt_item
        return           = return.

    READ TABLE return WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*         IMPORTING
*           RETURN        =
                .

      MOVE return-type    TO gt_body_53-type.
      MOVE return-message TO gt_body_53-message.
      IF tmp_zzcdate IS INITIAL.
        MOVE lv_zzcdate     TO gt_body_53-zzcdate .  "Victor 08.28.2011
      ELSE.
        MOVE tmp_zzcdate    TO gt_body_53-zzcdate .  "Victor 08.28.2011
      ENDIF.

      MODIFY gt_body_53 TRANSPORTING type
                                     message
                                     zzcdate
                              WHERE budat = gt_body_53-budat
                                AND sgtxt = gt_body_53-sgtxt.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
         wait          = 'X'
*         IMPORTING
*           RETURN        =
    .


      MOVE 'S'       TO gt_body_53-type.
      MOVE 'Success' TO gt_body_53-message.
      MOVE sy-datum  TO gt_body_53-bldat.
      IF tmp_zzcdate IS INITIAL.
        MOVE lv_zzcdate  TO gt_body_53-zzcdate .  "Victor 08.28.2011
      ELSE.
        MOVE tmp_zzcdate  TO gt_body_53-zzcdate .  "Victor 08.28.2011
      ENDIF.

      MODIFY gt_body_53 TRANSPORTING   type
                                       message
                                       mblnr
                                       bldat
                                       zzcdate
                            WHERE budat = gt_body_53-budat
                              AND sgtxt = gt_body_53-sgtxt.

    ENDIF.
*    ENDAT.
  ENDLOOP.
*  LOOP AT gt_body_37.
*    g_tabix = sy-tabix.
*    MOVE-CORRESPONDING  gt_body_37 TO l_body_37.
*
*    PERFORM update_mm02_data  USING l_body_37
*                           CHANGING l_return.
*
*    gt_body_37-type    = l_return-type.
*    gt_body_37-message = l_return-message.
*    MODIFY gt_body_37 INDEX g_tabix
*                      TRANSPORTING type
*                                   message.
*  ENDLOOP.

** Changed by Furong on 07/15/2011

*  MODIFY ZMMT0053 FROM TABLE GT_BODY_53.
*
*  IF SY-SUBRC EQ 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.
** End of change

**** LOGIC
  LOOP AT gt_body_53.
    g_tabix = sy-tabix.

    g_return =  e_return-type = gt_body_53-type.

** Changed by Furong on 07/15/2011
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF l_body_53
      FROM zmmt0053
     WHERE werks = gt_body_53-werks
       AND budat = gt_body_53-budat
       AND matnr = gt_body_53-matnr
*       AND zzcdate = gt_body_53-zzcdate   "Victor 08.28.2011
       AND mblnr = gt_body_53-mblnr     "added on 06.20.2012
       AND sgtxt = gt_body_53-sgtxt.


    IF sy-subrc = 0.
      IF l_body_53-etnam IS INITIAL.
        gt_body_53-etdat = sy-datum.
        gt_body_53-ettim = sy-uzeit.
        gt_body_53-etnam = sy-uname.
      ELSE.
        gt_body_53-atdat = sy-datum.
        gt_body_53-attim = sy-uzeit.
        gt_body_53-atnam = sy-uname.
        gt_body_53-etdat = l_body_53-etdat.
        gt_body_53-ettim = l_body_53-ettim.
        gt_body_53-etnam = l_body_53-etnam.
      ENDIF.
*--<  Victor 08.28.2011
      UPDATE zmmt0053
      SET mblnr      =  gt_body_53-mblnr
          type       =  gt_body_53-type
          message    =  gt_body_53-message
          atnam      =  gt_body_53-atnam
          atdat      =  gt_body_53-atdat
          attim      =  gt_body_53-attim
          etnam      =  gt_body_53-etnam
          etdat      =  gt_body_53-etdat
          ettim      =  gt_body_53-ettim
       WHERE  werks  =  gt_body_53-werks
         AND  budat  =  gt_body_53-budat
         AND  matnr  =  gt_body_53-matnr
         AND  mblnr  =  gt_body_53-mblnr.
*         AND  zzcdate  = gt_body_53-zzcdate.
*--->

    ELSE.
      gt_body_53-etdat = sy-datum.
      gt_body_53-ettim = sy-uzeit.
      gt_body_53-etnam = sy-uname.

      MOVE-CORRESPONDING gt_body_53 TO wa_body_53.
      MODIFY zmmt0053 FROM  wa_body_53.  "Victor 08.28.2011
    ENDIF.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
** end on 07/15/11

    e_return-message = gt_body_53-message.

    CALL METHOD zmmc_cl_if=>if_set_key(   ifkey = 'MMIF207_ECC_IB'
                              modle = 'GCS'       " 'MM', 'SD', 'FI'
                              centy = 'US'
                            dirct = 'I'  "'O' - Outbound, 'I'- Inbound
                              logox = ' '
                              ttype = 'S'
                              cparm = '6'    "How many Parameters
                           ).

    CALL METHOD zmmc_cl_if=>if_set_messg( type    = e_return-type
                              id      = ' '    "gt_retmsg-id
                              message = e_return-message
                            ).

    CALL METHOD zmmc_cl_if=>if_set_param( istat = g_return
                              ifp01 = gt_body_53-werks
                              ifp02 = gt_body_53-matnr
                              ifp03 = gt_body_53-budat
                              ifp04 = gt_body_53-zzcdate  "Victor 082811
                              ifp05 = gt_body_53-mblnr    "added on 06.20.2012
                              ifp06 = gt_body_53-sgtxt    "added on 06.20.2012
                              ).
    CALL METHOD zmmc_cl_if=>if_save_data( ).

    MODIFY gt_body_53 INDEX g_tabix.
  ENDLOOP.


** Changed by Furong on 07/15/2011
*  MODIFY ZMMT0053 FROM TABLE GT_BODY_53. "Commented by Victor

*  IF sy-subrc EQ 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.
** end

  CLEAR : it_body, it_body[].

  it_body[] = gt_body_53[].


ENDFUNCTION.
