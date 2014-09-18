FUNCTION zmmf_po_change.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_EBELN) TYPE  EKPO-EBELN
*"     REFERENCE(I_EBELP) TYPE  EKPO-EBELP
*"  TABLES
*"      T_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
*&------------------------------------------------------------------
*& Program ID     : ZMMF_PO_CHANGE
*& Profram Name   : Purchase Order Change
*& Created by     : C.H.Jeong
*& Created on     : 10/08/2013
*& Description    : Update PO item with the price of condition 'ZOA1'
*&     - Condition type 'ZOA1'(HMMA Customs(%))
*& Modification Log
*&====================================================================
*&  program ZMMC_DELIVERY_PO_CHANGE will replace this function
*&--------------------------------------------------------------------

*"----------------------------------------------------------------------

  DATA : BEGIN OF it_ekpo OCCURS 0,
    ebeln        TYPE ekpo-ebeln,
    ebelp        TYPE ekpo-ebelp,
    matnr        TYPE ekpo-matnr,
    werks        TYPE ekpo-werks,

    stawn        TYPE marc-stawn,    "HS Code
    vakey        TYPE konh-vakey.    "HS Code
  DATA : END OF it_ekpo.

  DATA : BEGIN OF it_konp OCCURS 0,
    knumh        TYPE konp-knumh,
    kopos        TYPE konp-kopos,
    kschl        TYPE konh-kschl,
    vakey        TYPE konh-vakey,
    kbetr        TYPE konp-kbetr,
    konwa        TYPE konp-konwa.
  DATA : END OF it_konp.

* for BAPI
  DATA :
    it_return    TYPE TABLE OF bapiret2      WITH HEADER LINE,
    it_poitem    TYPE TABLE OF bapimepoitem  WITH HEADER LINE,
    it_poitemx   TYPE TABLE OF bapimepoitemx WITH HEADER LINE,
    it_pocond    TYPE TABLE OF bapimepocond  WITH HEADER LINE,
    it_pocondx   TYPE TABLE OF bapimepocondx WITH HEADER LINE,
    l_kbetr      TYPE konp-kbetr.


  SELECT a~ebeln
         b~ebelp
         b~matnr
         b~werks
         c~stawn            "HS Code
         c~stawn AS vakey   "HS Code
    FROM ekko AS a
         INNER      JOIN ekpo AS b  ON a~ebeln = b~ebeln
         LEFT OUTER JOIN marc AS c  ON b~matnr = c~matnr AND
                                       b~werks = c~werks
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
   WHERE a~ebeln = i_ebeln
     AND b~ebelp = i_ebelp.
*   AND c~stawn <> space.     "HS Code
*   (No fields from the right-hand table of a LEFT OUTER JOIN
*     may appear in the WHERE condition: "C~STAWN").

* check data exist or not
  DELETE it_ekpo WHERE stawn = space.     "HS Code

  IF it_ekpo[] IS INITIAL.
    CLEAR : t_return.
    t_return-type    = 'E'.
    t_return-message = 'No data HS Code'.
    APPEND  t_return.
    EXIT.                 "Exit this function module
  ENDIF.


  IF NOT it_ekpo[] IS INITIAL.
    SELECT b~knumh
           b~kopos
           a~kschl
           a~vakey
           b~kbetr    "(CURR 11.2)
           b~konwa
      FROM konh AS a  INNER JOIN
           konp AS b  ON a~knumh = b~knumh
      INTO CORRESPONDING FIELDS OF TABLE it_konp
       FOR ALL ENTRIES IN it_ekpo
     WHERE a~kschl = 'ZOA1'        "Condition type('ZOA1' HMMA Customs(%))
       AND a~vakey = it_ekpo-vakey.   "HS Code
  ENDIF.

* "Just one PO item
  CLEAR : it_ekpo.
  READ TABLE it_ekpo INDEX 1.
  IF sy-subrc = 0.

    CLEAR   : it_return,
              it_poitem,   it_pocond,
              it_poitemx,  it_pocondx.
    CLEAR   : it_return[],
              it_poitem[], it_pocond[],
              it_poitemx[],it_pocondx[].

    it_poitem-po_item   = it_ekpo-ebelp.
    it_poitem-material  = it_ekpo-matnr.

    it_poitemx-po_item  = it_ekpo-ebelp.
    it_poitemx-po_itemx = 'X'.
    it_poitemx-material = 'X'.

    APPEND it_poitem.
    APPEND it_poitemx.

    CLEAR it_konp.
    READ TABLE it_konp WITH KEY vakey = it_ekpo-vakey.  "HS Code
    IF sy-subrc = 0.
**     it_pocond-itm_number   = it_konp-kopos.      "NO!!
      it_pocond-itm_number   = it_ekpo-ebelp.       "!!!(ekpo-ebelp)
      it_pocond-condition_no = it_konp-knumh.
      it_pocond-cond_type    = it_konp-kschl.
*     "it_pocond-cond_value   = it_konp-kbetr.
      IF it_konp-kbetr <> 0.
        CLEAR : l_kbetr.
        l_kbetr = it_konp-kbetr / 10.
        IF l_kbetr > '2.5'.
          l_kbetr = '2.5'.
        ENDIF.

        it_pocond-cond_value = l_kbetr.
      ENDIF.
      it_pocond-change_id    = 'U'.   "Change type (U : Update)

      it_pocondx-condition_no  = it_konp-knumh.
      it_pocondx-condition_nox = 'X'.
**     t_pocondx-itm_number     = it_konp-kopos.    "NO!!
      it_pocondx-itm_number    = it_ekpo-ebelp.     "!!! (ekpo-ebelp)
      it_pocondx-itm_numberx   = 'X'.
      it_pocondx-cond_type     = 'X'.
      it_pocondx-cond_value    = 'X'.
      it_pocondx-change_id     = 'X'.

      APPEND it_pocond.
      APPEND it_pocondx.
    ENDIF.

    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = it_ekpo-ebeln
      TABLES
        return        = it_return
        poitem        = it_poitem
        poitemx       = it_poitemx
        pocond        = it_pocond
        pocondx       = it_pocondx.

    CLEAR : it_return.
    READ TABLE it_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    IF t_return[] IS INITIAL.
      t_return[] = it_return[].
    ELSE.
      APPEND LINES OF it_return TO t_return.
    ENDIF.


  ENDIF.


ENDFUNCTION.
