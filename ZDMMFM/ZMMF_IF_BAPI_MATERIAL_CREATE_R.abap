FUNCTION zmmf_if_bapi_material_create_r.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MATERIAL) LIKE  ZSMM_IF021 STRUCTURE  ZSMM_IF021
*"     VALUE(I_CLASS) LIKE  RMCLF-CLASS OPTIONAL
*"     VALUE(I_CHECK) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      CLASSIFICATION STRUCTURE  ZSMM_IF015 OPTIONAL
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
*&------------------------------------------------------------------
*& Program ID     : ZMMF_IF_BAPI_MATERIAL_CREATE
*& Profram Name   : Material Master Create
*& Created by     : Seong Geon Cho
*& Created on     : 04.01.2006
*& Development ID : *
*& Reference Pgm. : *
*& Description    : *
*&
*& Modification Log
* Comment by sgcho 2006.01.03
*-- I_CHECK: C: CREATE FROM V_CAT.
*            R: MODIFY FROM V_CAT.
*   I_Class: Class number.
*   I_Material : Data from Vaatz System
*   CLASSIFICATION : characteristic of class ( need material master )
*   ret1 : Material Existence Check.
*          ret1-type = 'E' -> Material Not Exist
*          ret1-type = 'S' -> Material Exist
*>> Frequency : real time job( one by one )
*   E_RETURN : Material Create & Modify Success or Error Message to
*              Vaatz
*
*&====================================================================
*& Date     Developer      Request ID      Description
*& 05/24/06 Manju          UD1K920857      Refer HelpDesk 65M93A4445
*& 06/12/06 Manju          UD1K921048      Make some of the fields
*&                                         optional(HelpDesk 65M93A4445)
*&--------------------------------------------------------------------

*"----------------------------------------------------------------------
*-- I_CHECK: C: CREATE FROM V_CAT.
*            R: MODIFY FROM V_CAT.
  DATA : l_matnr LIKE mara-matnr.

  DATA : dflag TYPE c.
  DATA : ret1 LIKE bapiret1,
         ret2 LIKE bapiret2.

  CLEAR : v_material, l_matnr, v_first.
  CLEAR : dflag, ret1, ret2, v_check, v_mard, v_mbew.

  CLEAR: v_return, v_return[], v_ztmm_if015, v_ztmm_if015[].

  CLEAR: v_class, v_classification, v_classification[].

  MOVE-CORRESPONDING i_material TO v_material.
*  move: i_material to v_material,
  MOVE: i_check    TO v_check.

  MOVE: i_class          TO v_class.
  MOVE: classification[] TO v_classification[].

*  PERFORM save_if_table.

*--- get serial number
  DATA : w_serno LIKE ztmm_if020-zsen.

  CLEAR : w_serno, v_zseq.

  SELECT MAX( zseq ) INTO v_zseq
                     FROM ztmm_if001.

  IF v_zseq IS INITIAL.
    MOVE : 1 TO v_zseq.
  ELSE.
    v_zseq = v_zseq + 1.
  ENDIF.
*---

*-- matnr Existence check
  CALL FUNCTION 'BAPI_MATERIAL_EXISTENCECHECK'
       EXPORTING
            material      = v_material-matnr
       IMPORTING
            deletion_flag = dflag
            return        = ret1.

*-- ret1-type
*    S --> Material Master exist.
*    E --> Material Master not exist.
*--
  IF v_check EQ 'D'.
    CASE ret1-type.
*-- Material Master exist.
      WHEN 'S'.
        PERFORM material_delete_reuse.

        MOVE v_return[] TO e_return[].
        LOOP AT v_return.
          MOVE-CORRESPONDING v_return TO v_ztmm_if015.
          MOVE: v_zseq   TO v_ztmm_if015-zseq,
                sy-tabix TO v_ztmm_if015-zsen.
          APPEND v_ztmm_if015.
          CLEAR v_return.
        ENDLOOP.

*        INSERT ztmm_if015 FROM TABLE v_ztmm_if015.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
      WHEN 'E'.
*-- Material Master not exist.
        v_return-type = 'E'.
        CONCATENATE 'Material number ' v_material-matnr
        ' does not exist' INTO v_return-message .
        APPEND v_return.
        MOVE: v_return[] TO e_return[].
        LOOP AT v_return.
          MOVE-CORRESPONDING v_return TO v_ztmm_if015.
          MOVE: v_zseq   TO v_ztmm_if015-zseq,
                sy-tabix TO v_ztmm_if015-zsen.
          APPEND v_ztmm_if015.
          CLEAR v_return.
        ENDLOOP.
*        INSERT ztmm_if015 FROM TABLE v_ztmm_if015.
*        EXIT.
    ENDCASE.
*-- Modify from v_cat.
  ELSEIF v_check EQ 'R'.
*    if ret1-type eq 'E'.
*      v_return-type = 'E'.
*      concatenate 'Material number ' v_material-matnr
*      ' does not exist' into v_return-message .
*      append v_return.
*
*      move: v_return[] to e_return[].
*
*      loop at v_return.
*        move-corresponding v_return to v_ztmm_if015.
*        move: v_zseq   to v_ztmm_if015-zseq,
*              sy-tabix to v_ztmm_if015-zsen.
*        append v_ztmm_if015.
*        clear v_return.
*      endloop.
*      insert ztmm_if015 from table v_ztmm_if015.
*      exit.
*    else.
    CLEAR v_first.
    ret1-type = 'E'.
*     perform check_subcontratin.
    PERFORM bapi_material_savedata USING ret1-type.

    MOVE: v_return[] TO e_return[].

    LOOP AT v_return.
      MOVE-CORRESPONDING v_return TO v_ztmm_if015.
      MOVE: v_zseq   TO v_ztmm_if015-zseq,
            sy-tabix TO v_ztmm_if015-zsen.
      APPEND v_ztmm_if015.
      CLEAR v_return.
    ENDLOOP.
*    INSERT ztmm_if015 FROM TABLE v_ztmm_if015.

*   endif.

  ELSE.
    PERFORM check_subcontratin.

    CLEAR v_first.
*-- Material Master Create Start.
    PERFORM bapi_material_savedata USING ret1-type.

    MOVE: v_return[] TO e_return[].

    LOOP AT v_return.
      MOVE-CORRESPONDING v_return TO v_ztmm_if015.
      MOVE: v_zseq   TO v_ztmm_if015-zseq,
            sy-tabix TO v_ztmm_if015-zsen.
      APPEND v_ztmm_if015.
      CLEAR v_return.
    ENDLOOP.
*    INSERT ztmm_if015 FROM TABLE v_ztmm_if015.
  ENDIF.

*---
  READ TABLE v_return WITH KEY type = 'E'.

  IF sy-subrc EQ 0.     " error
    DATA : it_xmsg LIKE ztismessage OCCURS 0 WITH HEADER LINE.
    LOOP AT v_return.
      MOVE: v_return-type    TO it_xmsg-msgty,
            v_return-message TO it_xmsg-msgtx.
      APPEND it_xmsg.
    ENDLOOP.
    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
         EXPORTING
              xdocno_show = 'X'
         TABLES
              xmsg        = it_xmsg.
  ELSE.                 " success
    CLEAR : ztmm_if001.
    MOVE-CORRESPONDING v_material TO ztmm_if001.
    MOVE : v_zseq                 TO ztmm_if001-zseq,
           v_check                TO ztmm_if001-zflag,
           v_class                TO ztmm_if001-class,
           'R'                    TO ztmm_if001-type,
           v_material-zseq        TO ztmm_if001-zredoc,
           sy-datum               TO ztmm_if001-erdat,
           sy-uzeit               TO ztmm_if001-erzet,
           sy-uname               TO ztmm_if001-ernam.
*... 1. insert new row
    INSERT ztmm_if001.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
*... 2. update old row
      UPDATE ztmm_if001 SET zr2pro = 'S'
                            zredoc = v_zseq
                      WHERE zseq EQ v_material-zseq.
*... 3. insert classification
      DATA : it_ztmm_if020 LIKE ztmm_if020 OCCURS 0 WITH HEADER LINE.
      LOOP AT v_classification.
        w_serno = w_serno + 1.
        MOVE-CORRESPONDING v_classification TO it_ztmm_if020.
        MOVE : w_serno                      TO it_ztmm_if020-zsen,
               v_zseq                       TO it_ztmm_if020-zseq.
        APPEND it_ztmm_if020.
        CLEAR : v_classification, it_ztmm_if020.
      ENDLOOP.
      INSERT ztmm_if020 FROM TABLE it_ztmm_if020.
*... 4. insert log
      INSERT ztmm_if015 FROM TABLE v_ztmm_if015.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
        MESSAGE i999(zmmm) WITH 'Success!!'.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFUNCTION.
