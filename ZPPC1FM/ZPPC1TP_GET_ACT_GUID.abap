FUNCTION ZPPC1TP_GET_ACT_GUID.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ACTIVITY) TYPE  PPEUI_PNAME OPTIONAL
*"     REFERENCE(MODE) TYPE  PVS_ALTNUM OPTIONAL
*"     REFERENCE(RESOURCE) TYPE  PVS_RES_NAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(ACTIVITY_ID) TYPE  PVS_PNGUID
*"     REFERENCE(MODE_ID) TYPE  PVS_PAGUID
*"     REFERENCE(RESOURCE_ID) TYPE  PVS_PAGUID
*"----------------------------------------------------------------------

* activity node
  select single pnguid from pnodid into activity_id
    where PNAME = activity
    and   CLINT = 0
    and   PNTYPE = 'S_ACTST'
    and   APPLOBJ_TYPE = 'ACT'.

* mode
  if sy-subrc = 0.
    select single paguid from paltid into mode_id
      where pguid = activity_id
      and   altnum = mode
      and   patype = 'S_ACTSTM'
      and   APPLOBJ_TYPE = 'ACT'.
  endif.

* resource
  select single pnguid from pnodid into resource_id
    where PNAME  = resource
    and   CLINT  = 0
    and   PNTYPE = 'S_RESPRD'
    and   APPLOBJ_TYPE = 'RES'.

ENDFUNCTION.
