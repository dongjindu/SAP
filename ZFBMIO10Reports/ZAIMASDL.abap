*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120024545 0000141721                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46A          To SAPKH46A34                                $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZAIMASDL
*& Object Header   PROG ZAIMASDL
*&-------------------------------------------------------------------*
*& REPORT ZAIMASDL
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZAIMASDL.

tables: aufk, prps, imak.

data: dummy,
      con_delete    like rimzo-updkz value 'D'.

parameters: order   like aufk-aufnr,
            wbs_el  like prps-pspnr,
            app_req like imak-posnr.


* At least one entry required.
at selection-screen.
  if order   is initial and
     wbs_el  is initial and
     app_req is initial .
     message e400(ap) with
       'missing entry' dummy dummy dummy.
  endif.

* Existence check order.
at selection-screen on order.
   if not order is initial.
      select single * from aufk
        where aufnr = order.
      if sy-subrc <> 0.
         message e400(ap) with
           'order does not exist:' order dummy dummy.
      endif.
   endif.

* Existence check WBS-element.
at selection-screen on wbs_el.
   if not wbs_el is initial.
      select single * from prps
        where pspnr = wbs_el.
      if sy-subrc <> 0.
         message e400(ap) with
           'WBS element does not exist:' wbs_el dummy dummy.
      endif.
   endif.

* Existence check appropriation request.
at selection-screen on app_req.
   if not app_req is initial.
      select single * from imak
        where posnr = app_req.
      if sy-subrc <> 0.
         message e400(ap) with
           'appr.request does not exist:' app_req dummy dummy.
      endif.
   endif.


start-of-selection.

if not order is initial.
   perform cut_assignment using aufk-objnr.
endif.

if not wbs_el is initial.
   perform cut_assignment using prps-objnr.
endif.

if not app_req is initial.
   perform cut_assignment using imak-objnr.
endif.

commit work.

message s217(ap).

end-of-selection.

form cut_assignment using value(id_objnr) like imzo-objnr.

  data: lt_upd_imzo like rimzo occurs 10 with header line.

* Enqueue measure/request.
  CALL FUNCTION 'AIPE_ENQUEUE_CO_OBJECT'
       EXPORTING
            I_OBJNR        = id_objnr
       EXCEPTIONS
            ABNORMAL_END   = 1
            FOREIGN_LOCK   = 2
            SYSTEM_FAILURE = 3
            OTHERS         = 4.
   IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

* Budget coming from budget distribution
* must be returned completely before deletion
* of assignment.
  CALL FUNCTION 'AIPA_CHECK_BUDG_FOR_DISCONNECT'
       EXPORTING
            I_OBJNR         = id_objnr
       EXCEPTIONS
            BUDGET_NOT_ZERO = 1
            OTHERS          = 0.
  IF SY-SUBRC = 1.
     MESSAGE ID SY-MSGID  TYPE 'E' NUMBER SY-MSGNO
       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Read IMZO records.
  select * from imzo
    into corresponding fields of table lt_upd_imzo
    where objnr = id_objnr.
* Delete IMZO records.
  if sy-subrc = 0.
     loop at lt_upd_imzo.
       lt_upd_imzo-updkz = con_delete.
       modify lt_upd_imzo.
     endloop.
     CALL FUNCTION 'AIPU_POSITION_PREPARE_UPDATE'
          TABLES
               T_UPD_IMZO = lt_upd_imzo.
     CALL FUNCTION 'AIPU_POSITION_UPDATE'
          TABLES
               T_UPD_IMZO = lt_upd_imzo.
  endif.

endform.
*>>>> END OF INSERTION <<<<<<
