*&---------------------------------------------------------------------*
*& Include ZAPP237M_TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp237m_rew_lts   message-id  zmpp.


tables: ausp,
        equi,
        zspp_vm_value.
*
controls tc_app237  type tableview using screen 2107.
* inquiry Key
tables: zspp_app237.
* inquiry Key Save
data: begin of  sv_key,
        model  like  zspp_app237-model,
        gubun  like  zspp_app237-gubun.
data  end of sv_key.

data: begin of IT_EQUNR_2107  occurs  0,
        objek  like  equi-equnr,
      end of IT_EQUNR_2107.

* - vehicle master charateristics
data  begin of IT_VMV_2107  occurs  0.
        include structure zspp_vin_value.
data  end of IT_VMV_2107.


data: begin  of IT_DLY_2107  occurs 0,
        bodyno(10),
        won(14),
        extc    like  ztpp_wosum-extc,
        intc    like  ztpp_wosum-intc,
        bodyin  like  sy-datum,
        paintin like  sy-datum,
        trimin  like  sy-datum,
        cfinal  like  sy-datum,
        soff    like  sy-datum,
        cgate   like  sy-datum,
        dday    type  zspp_app237-dday,
        model   like  zspp_vm_value-model,
      end  of IT_DLY_2107.
data  IT_DLS_2107  like  IT_DLY_2107 occurs 0 with header line.
*
data: P_EQUNR_2107    like  equi-equnr.
data  P_STATUS_2107   like  ausp-atinn.
data  P_ATWRT_2107    like  ausp-atwrt.
data: WA_MODEL_2107    like  zspp_vm_value-model,
      WA_SERIAL_2107   like  zspp_vm_value-bodyno.

data: WA_VMV_LINES_2107  type  i,
      WA_DLY_LINES_2107  type  i.
