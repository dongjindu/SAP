FUNCTION Z_PPC1TP_RPV_MAINTAIN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(LINE_HEADER_EXT) TYPE  PPC_LINE_HEADER_NAME
*"       EXPORTING
*"             REFERENCE(LINE_VERSION) TYPE  PPC_LINE_VERSION
*"       TABLES
*"              REPPOINTS_EXT STRUCTURE  ZREPPOINTS_EXT
*"       EXCEPTIONS
*"              DUPLICATE_REPPOINTS
*"----------------------------------------------------------------------

  data l_line_int     type PPC_LINE_HEADER_INT.
  data l_return       like BAPIRET2.
  DATA: BEGIN OF lt_guid OCCURS 0,
          reppoint_ext type ppc_reppoint_ext,
          reppoint_int TYPE ppc_reppoint_int,
        END OF lt_guid.
  data lt_rpoint_structures like table of BAPI_PPC_RPV with header line.
  data lt_reppoints   like table of BAPI_PPC_REPPOINT_VERS_EXT
                      with header line.


* line identification
  select single LINE_HEADER from PPC_RP_vers_dat into l_line_int
    where line_header_ext = line_header_ext.

  if sy-subrc <> 0.
    CALL FUNCTION 'GUID_CREATE'
         IMPORTING
              EV_GUID_16 = l_line_int.
  endif.

* get new line version
  select single max( line_version ) from ppc_rp_vers
    into line_version
    where line_header = l_line_int. "no success -> max=0
  add 1 to line_version.

* prepare line table for bapi
  lt_rpoint_structures-LINEHEADER  = l_line_int.
  lt_rpoint_structures-LHEADERNAME = line_header_ext.
  lt_rpoint_structures-LINEVERSION = line_version.
  append lt_rpoint_structures.

* no duplicate records allowed
  sort reppoints_ext.
  delete adjacent duplicates from reppoints_ext.
  if sy-subrc = 0.
    raise duplicate_reppoints.
  endif.

* get reporting point guids
  loop at reppoints_ext.
    lt_guid-reppoint_ext = reppoints_ext-reppoint_ext.
    append lt_guid.
    if reppoints_ext-reppoint_pred_ext ne space.
      lt_guid-reppoint_ext = reppoints_ext-reppoint_pred_ext.
      append lt_guid.
    endif.
  endloop.

  sort lt_guid.
  delete adjacent duplicates from lt_guid.

  loop at lt_guid.
    select single reppoint from ppc_rp into lt_guid-reppoint_int
        where REPPOINT_EXT = lt_guid-REPPOINT_EXT.
    if sy-subrc <> 0.
      CALL FUNCTION 'GUID_CREATE'
           IMPORTING
                EV_GUID_16 = lt_guid-reppoint_int.
    endif.
    modify lt_guid.
  endloop.

* create reporting point table for bapi call
  lt_reppoints-LINEHEADER  = l_line_int.
  lt_reppoints-LHEADERNAME = line_header_ext.
  lt_reppoints-LINEVERSION = line_version.

* loop at external reporting points
  loop at REPPOINTS_EXT.

    lt_reppoints-reppoint_ext  = reppoints_ext-reppoint_ext.
    read table lt_guid with key
      reppoint_ext = reppoints_ext-reppoint_ext binary search.
    lt_reppoints-reppoint =      lt_guid-reppoint_int.

    if reppoints_ext-reppoint_pred_ext eq space.
      clear lt_reppoints-reppoint_pred.
    else.
      read table lt_guid with key
        reppoint_ext = reppoints_ext-reppoint_pred_ext binary search.
      lt_reppoints-reppoint_pred = lt_guid-reppoint_int.
    endif.

    append lt_reppoints.

  endloop.

* call BAPI
  CALL FUNCTION 'BAPI_MNFCTCONFRCVR_RPV_RCV_MUL'
       IMPORTING
            RETURN            = l_return
       TABLES
            RPOINT_STRUCTURES = lt_rpoint_structures[]
            REPPOINTS         = lt_reppoints[].

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       IMPORTING
            RETURN = l_return.

ENDFUNCTION.
