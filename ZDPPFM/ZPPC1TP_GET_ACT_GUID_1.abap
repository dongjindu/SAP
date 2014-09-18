FUNCTION zppc1tp_get_act_guid_1.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ACTIVITY) TYPE  PPEUI_PNAME
*"     REFERENCE(RESOURCE) TYPE  PVS_RES_NAME
*"  EXPORTING
*"     REFERENCE(ACTIVITY_ID) TYPE  PVS_PNGUID
*"     REFERENCE(MODE_ID) TYPE  PVS_PAGUID
*"     REFERENCE(RESOURCE_ID) TYPE  PVS_PAGUID
*"----------------------------------------------------------------------
* Activity node
  SELECT SINGLE pnguid FROM pnodid INTO activity_id
    WHERE pname  EQ activity
    AND   clint  EQ 0
    AND   pntype EQ 'S_ACTST'
    AND   applobj_type EQ 'ACT'.

* Resource
  SELECT SINGLE pnguid FROM pnodid INTO resource_id
    WHERE pname  EQ resource
    AND   clint  EQ 0
    AND   pntype EQ 'S_RESPRD'
    AND   applobj_type EQ 'RES'.

  SELECT SINGLE paguid FROM pamodd INTO mode_id
   WHERE pnguid  EQ resource_id.

  CHECK NOT mode_id IS INITIAL.
* mode
  SELECT SINGLE * FROM paltid
    WHERE paguid EQ mode_id
    AND   patype EQ 'S_ACTSTM'
    AND   applobj_type EQ 'ACT'.

  IF sy-subrc <> 0.
    MESSAGE i001 WITH 'Not exist MODE ID'.
  ENDIF.

ENDFUNCTION.
