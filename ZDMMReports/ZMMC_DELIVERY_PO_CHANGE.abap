*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMC_DELIVERY_PO_CHANGE
*& Program Name   : Inbound Delivery PO Change(for Batch)
*& Created by     : Victor Park
*& Created on     : 01.30.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.     : This program will replace  BADI : Z_LE_DELIVERY_PROC
*&            -> IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT
*&            -> CALL FUNCTION 'ZMMF_PO_CHANGE'
*&----------------------------------------------------------------------

REPORT  zmmc_delivery_po_change MESSAGE-ID zmpp.

TABLES : likp, lips.

DATA : BEGIN OF it_lips OCCURS 0,
*        INCLUDE STRUCTURE lips.
         ebeln        TYPE ekpo-ebeln,
         ebelp        TYPE ekpo-ebelp,
         matnr        TYPE ekpo-matnr,
         werks        TYPE ekpo-werks,
         stawn        TYPE marc-stawn,    "HS Code
         vakey        TYPE konh-vakey,    "HS Code
         matwa        TYPE zmmt0105-matwa,
         matkl        TYPE zmmt0105-matkl,
         lgort        TYPE zmmt0105-lgort,
         charg        TYPE zmmt0105-charg,
         lichn        TYPE zmmt0105-lichn,
         vbeln        TYPE lips-vbeln,
         posnr        TYPE lips-posnr,
         type     TYPE zmmt0105-type,
         message  TYPE zmmt0105-message.
DATA : END OF it_lips.

DATA : BEGIN OF it_konp OCCURS 0,
  knumh        TYPE konp-knumh,
  kopos        TYPE konp-kopos,
  kschl        TYPE konh-kschl,
  vakey        TYPE konh-vakey,
  kbetr        TYPE konp-kbetr,
  konwa        TYPE konp-konwa.
DATA : END OF it_konp.

DATA : it_save LIKE zmmt0105 OCCURS 0 WITH HEADER LINE.

* for BAPI
DATA :
  it_return    TYPE TABLE OF bapiret2      WITH HEADER LINE,
  it_poitem    TYPE TABLE OF bapimepoitem  WITH HEADER LINE,
  it_poitemx   TYPE TABLE OF bapimepoitemx WITH HEADER LINE,
  it_pocond    TYPE TABLE OF bapimepocond  WITH HEADER LINE,
  it_pocondx   TYPE TABLE OF bapimepocondx WITH HEADER LINE,
  l_kbetr      TYPE konp-kbetr.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF  BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_erdat FOR likp-erdat
              DEFAULT sy-datum OPTION EQ SIGN I.
* Delivery No?
SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.
  PERFORM process_bapi.
  PERFORM save_data.


*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .

  SELECT b~vgbel  AS  ebeln b~vgpos AS ebelp
         b~matnr            b~werks
         c~stawn            "HS Code
         c~stawn AS vakey   "HS Code
         b~matwa  b~matkl b~werks b~lgort b~charg b~lichn
         b~vbeln  b~posnr
    INTO CORRESPONDING FIELDS OF TABLE it_lips
  FROM likp AS a   INNER JOIN lips AS b
                    ON a~vbeln = b~vbeln
*                   LEFT OUTER JOIN marc AS c
                   INNER JOIN marc AS c
                    ON b~matnr = c~matnr
                   AND b~werks = c~werks
  WHERE a~lifnr = 'SEF9'
    AND a~erdat IN s_erdat
    AND b~lichn = 'N'
    AND c~stawn <> ''
    AND NOT EXISTS ( SELECT *
                     FROM zmmt0105 AS x
                     WHERE x~ebeln  =  b~vgbel
                       AND x~ebelp  =  b~vgpos
                       AND x~type   = 'S' ).

  IF it_lips[] IS INITIAL.
    MESSAGE s000 WITH 'There is No data'.
    STOP.
  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  SORT it_lips BY ebeln ebelp vbeln  DESCENDING
                              posnr  DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_lips COMPARING ebeln ebelp.

  CHECK it_lips[] IS NOT INITIAL.

*-Conditions
  SELECT b~knumh
         b~kopos
         a~kschl
         a~vakey
         b~kbetr
         b~konwa
    FROM konh AS a  INNER JOIN
         konp AS b  ON a~knumh = b~knumh
    INTO CORRESPONDING FIELDS OF TABLE it_konp
     FOR ALL ENTRIES IN it_lips
   WHERE a~kschl = 'ZOA1'        "Condition type('ZOA1' HMMA Customs(%))
     AND a~vakey = it_lips-vakey.   "HS Code


ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_BAPI
*&---------------------------------------------------------------------*
FORM process_bapi .


  LOOP AT it_lips.
    AT NEW ebeln.
      PERFORM bapi_clear.
    ENDAT.

    PERFORM bapi_data.

    AT END OF ebeln.
      PERFORM bapi_exec.

    ENDAT.
  ENDLOOP.

ENDFORM.                    " PROCESS_BAPI
*&---------------------------------------------------------------------*
*&      Form  BAPI_CLEAR
*&---------------------------------------------------------------------*
FORM bapi_clear .
  CLEAR   : it_return,
            it_poitem,   it_pocond,
            it_poitemx,  it_pocondx.
  CLEAR   : it_return[],
            it_poitem[], it_pocond[],
            it_poitemx[],it_pocondx[].
ENDFORM.                    " BAPI_CLEAR
*&---------------------------------------------------------------------*
*&      Form  BAPI_EXEC
*&---------------------------------------------------------------------*
FORM bapi_exec .

  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = it_lips-ebeln
    TABLES
      return        = it_return
      poitem        = it_poitem
      poitemx       = it_poitemx
      pocond        = it_pocond
      pocondx       = it_pocondx.

  CLEAR : it_return, it_lips-type, it_lips-message.
  READ TABLE it_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    it_lips-type = 'E'.
    it_lips-message = it_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    it_lips-type = 'S'.
  ENDIF.

  MODIFY it_lips  TRANSPORTING type message
                  WHERE ebeln  = it_lips-ebeln.
ENDFORM.                    " BAPI_EXEC
*&---------------------------------------------------------------------*
*&      Form  BAPI_DATA
*&---------------------------------------------------------------------*
FORM bapi_data .

  it_poitem-po_item   = it_lips-ebelp.
  it_poitem-material  = it_lips-matnr.

  it_poitemx-po_item  = it_lips-ebelp.
  it_poitemx-po_itemx = 'X'.
  it_poitemx-material = 'X'.

  APPEND it_poitem.
  APPEND it_poitemx.

  CLEAR it_konp.
  READ TABLE it_konp WITH KEY vakey = it_lips-vakey.  "HS Code
  IF sy-subrc = 0.
**     it_pocond-itm_number   = it_konp-kopos.      "NO!!
    it_pocond-itm_number   = it_lips-ebelp.
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
    it_pocondx-itm_number    = it_lips-ebelp.
    it_pocondx-itm_numberx   = 'X'.
    it_pocondx-cond_type     = 'X'.
    it_pocondx-cond_value    = 'X'.
    it_pocondx-change_id     = 'X'.

    APPEND it_pocond.
    APPEND it_pocondx.
** Furong on 07/16/14  to clear cond_value (
    CLEAR: it_pocond, it_pocondx.
** )
  ENDIF.
ENDFORM.                    " BAPI_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM save_data .
  CLEAR : it_save[], it_save.

  WRITE :/ '==========================================',
         / '    PO No. Update Status                  ',
         / '=========================================='.

  LOOP AT it_lips.

    WRITE :/ it_lips-ebeln, it_lips-ebelp, it_lips-type, it_lips-message.

    MOVE-CORRESPONDING it_lips TO it_save.
    it_save-erdat = sy-datum.
    it_save-erzet = sy-uzeit.
    it_save-ernam = sy-uname.

    APPEND it_save.
  ENDLOOP.

  MODIFY zmmt0105 FROM TABLE it_save.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s000 WITH 'PO data successfully updated   '.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e000 WITH 'PO data Update failed into log table '.
  ENDIF.
ENDFORM.                    " SAVE_DATA
