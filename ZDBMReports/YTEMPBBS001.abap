REPORT ytempbbs001.
TABLES: marc.

DATA: BEGIN OF it_itab OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        dismm   LIKE   marc-dismm,
        beskz   LIKE   marc-beskz,
        sobsl   LIKE   marc-sobsl,
        ncost   LIKE   marc-ncost,
        msg(100),
      END   OF it_itab.

DATA: it_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

DATA: w_head  LIKE bapimathead,
      w_marc  LIKE bapi_marc,
      w_marcx LIKE bapi_marcx.

SELECT-OPTIONS: s_matnr FOR marc-matnr.

AT SELECTION-SCREEN.

  SELECT a~matnr werks dismm beskz sobsl ncost
    INTO CORRESPONDING FIELDS OF TABLE it_itab
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr = b~matnr
   WHERE a~matnr IN s_matnr
     AND a~mtart EQ 'HALB'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

START-OF-SELECTION.

  LOOP AT it_itab.
    CLEAR: w_head, w_marc, w_marcx,
           it_bapiret2, it_bapiret2[].

    MOVE: it_itab-matnr TO w_head-material,
          'X'           TO w_head-mrp_view,
          'X'           TO w_head-cost_view.

    MOVE: it_itab-werks   TO w_marc-plant,
          'ND'            TO w_marc-mrp_type,
          'X'             TO w_marc-proc_type,
          '50'            TO w_marc-spproctype,
          'X'             TO w_marc-no_costing,
          it_itab-werks   TO w_marcx-plant,
          'X'             TO w_marcx-mrp_type,
          'X'             TO w_marcx-proc_type,
          'X'             TO w_marcx-spproctype,
          'X'             TO w_marcx-no_costing.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = w_head
              plantdata      = w_marc
              plantdatax     = w_marcx
         TABLES
              returnmessages = it_bapiret2.
    LOOP AT it_bapiret2 WHERE type = 'E'
                           OR type = 'A'.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      MOVE: it_bapiret2-message TO it_itab-msg.
      MODIFY it_itab.
    ENDIF.
  ENDLOOP.


  loop at it_itab WHERE MSG NE SPACE.
    write:/ it_itab-matnr,
            it_itab-werks,
            it_itab-dismm,
            it_itab-beskz,
            it_itab-sobsl,
            it_itab-ncost,
            it_itab-msg.
  endloop.
