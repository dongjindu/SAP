FUNCTION zh_hr_if_cd_master.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_FORTABLE STRUCTURE  ZGHRSS0001 OPTIONAL
*"      T_DOMAINS STRUCTURE  ZGHRSS0008 OPTIONAL
*"      T_CONTENTS STRUCTURE  ZGHRSS0002 OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_rg_domname TYPE RANGE OF domname WITH HEADER LINE.
  DATA: lt_t005       TYPE TABLE OF t005 WITH HEADER LINE.

  LOOP AT t_domains.
    lt_rg_domname-sign = 'I'.
    lt_rg_domname-option = 'EQ'.
    lt_rg_domname-low = t_domains-domname.
    APPEND lt_rg_domname.
  ENDLOOP.

  LOOP AT t_fortable.
    CASE t_fortable-tablename.
      WHEN 'DD07T'.
        CHECK NOT lt_rg_domname[] IS INITIAL.

        SELECT * APPENDING TABLE t_contents FROM dd07t
          WHERE domname IN lt_rg_domname.

        t_fortable-rowcount = sy-dbcnt.
      WHEN 'T005'.
        SELECT land1 spras waers
          INTO CORRESPONDING FIELDS OF TABLE lt_t005
          FROM (t_fortable-tablename).
        t_fortable-rowcount = sy-dbcnt.
        LOOP AT lt_t005.
          CLEAR t_contents.
          CONCATENATE lt_t005-land1 lt_t005-spras lt_t005-waers
            INTO t_contents-content.
          APPEND t_contents.
        ENDLOOP.
      WHEN OTHERS.
        SELECT * APPENDING TABLE t_contents FROM (t_fortable-tablename).
        t_fortable-rowcount = sy-dbcnt.
    ENDCASE.

    MODIFY t_fortable.
  ENDLOOP.


ENDFUNCTION.
