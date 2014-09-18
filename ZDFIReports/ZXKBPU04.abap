*&---------------------------------------------------------------------*
*&  Include           ZXKBPU04
*&---------------------------------------------------------------------*

*"              I_BPBK STRUCTURE  BPBK
*"              I_BPEJ STRUCTURE  BPEJ
*"              I_BPEG STRUCTURE  BPEG
*"              I_BPEP STRUCTURE  BPEP

* if called from z, update using "SEQ" in bpbk-text  (sy-cprog)
*    - BAPI - text field has 4 digit of seq + reason + memo
* if called using std tcode, insert into ztable.
tables: imak.
DATA: ls_impr  type impr,
      ls_imfm  type ztfi_imfm,
      lf_seq   TYPE ztfi_imfm-seq.

LOOP AT i_bpej.

  READ TABLE i_bpbk WITH KEY belnr = i_bpej-belnr.

* IM budget only, plan: overwrite only
  CHECK i_bpbk-vorga = 'KBUD'
     OR i_bpbk-vorga = 'KBN0'
     OR i_bpbk-vorga = 'KBR0'.

  CLEAR: ls_impr, ls_imfm.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_impr
    FROM impr
   WHERE objnr EQ i_bpej-objnr.
  CHECK sy-subrc = 0.

*  IF sy-cprog(1) = 'Z'.  "not working...

  lf_seq = i_bpbk-sgtext(4).
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF ls_imfm
    FROM ztfi_imfm
    WHERE posid = ls_impr-posid
      AND ayear = ls_impr-gjahr
      AND seq   = lf_seq.
*        AND gubun =

  IF sy-subrc = 0.
    update ztfi_imfm set seq   = i_bpbk-sgtext(4)
                         belnr = i_bpej-belnr
                         ddate = i_bpbk-cpudt
                         vorga = i_bpej-vorga
       WHERE posid = ls_impr-posid
         AND ayear = ls_impr-gjahr
         AND seq   = lf_seq.
  ELSE.
* Only two transactions allowed.
*CHECK sy-tcode EQ 'IM27' OR
*      sy-tcode EQ 'IM27_REPEAT' OR
*      sy-tcode EQ 'IMCAOV'.

    ls_imfm-posid = ls_impr-posid.
    ls_imfm-ayear = ls_impr-gjahr.
    ls_imfm-posnr = ls_impr-posid.

    ls_imfm-gjahr = i_bpej-gjahr.

    ls_imfm-prnam  = ls_impr-prnam.
    ls_imfm-twaer  = i_bpej-twaer.
    ls_imfm-tot    = i_bpej-wtjhr.
    ls_imfm-wtp12  = i_bpej-wtjhr.

    IF i_bpej-trgkz = 'B'.  "IM c/f
      ls_imfm-gubun   = 'B'. "C/F
    ELSE.
      CASE i_bpej-vorga.
        WHEN 'KBUD'.   ls_imfm-gubun   = '1'. "orig
        WHEN 'KBN0'.   ls_imfm-gubun   = '2'. "sup
        WHEN 'KBR0'.   ls_imfm-gubun   = '3'. "return
        WHEN OTHERS.   ls_imfm-gubun   = ' '. "unknown
      ENDCASE.
    ENDIF.

    ls_imfm-status = 'X'.   "direct update
    ls_imfm-belnr = i_bpej-belnr.
    ls_imfm-vorga = i_bpej-vorga.
*      ls_imfm-TRANS =
    ls_imfm-uname = i_bpbk-usnam.
    ls_imfm-ddate = i_bpbk-cpudt.
    ls_imfm-zdate = i_bpbk-cpudt.

    SELECT MAX( seq )
      INTO lf_seq
      FROM ztfi_imfm
      WHERE posid = ls_impr-posid
        AND ayear = ls_impr-gjahr.
    ls_imfm-seq   = lf_seq + 1.

    ls_imfm-text  = sy-tcode.

*        case l_out-act_txt.
*          when 'Original'.
    ls_imfm-gubun = '1'. ls_imfm-reson  = '02'.
*          when 'Supplement'.
*            ls_imfm-gubun = '2'. ls_imfm-reson  = '1F'.
*          when 'Return'.
*            ls_imfm-gubun = '3'. ls_imfm-reson  = '2F'.
*        endcase.

** By Furong on 02/19/14 (

   If ls_imfm-text = 'IM30' or ls_imfm-text = 'IM38'.
      ls_imfm-bw_send = 'X'.
   endif.

   SELECT SINGLE * FROM imak
          WHERE posid = ls_imfm-posid.
    IF sy-subrc = 0.
       INSERT ztfi_imfm FROM ls_imfm.
    endif.
*    INSERT ztfi_imfm FROM ls_imfm.
** )


  ENDIF.



ENDLOOP.
