FUNCTION z_fbm_abxc01dt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXC01DT STRUCTURE  ZTBM_ABXC01DT
*"----------------------------------------------------------------------
  DATA: it_abxc01dt TYPE ztbm_abxc01dt OCCURS 0 WITH HEADER LINE.
  DATA : w_int TYPE i,
         w_dsn(90).
  DATA : BEGIN OF it_c01 OCCURS 0,
         record(134),
         END OF it_c01.
  DATA : d_date LIKE sy-datum.
  CLEAR : d_date.

  d_date = sy-datum - 1.

  REFRESH it_abxc01dt.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxc01dt
     FROM ztbm_abxc01dt
      WHERE zsdat EQ d_date.

  DELETE ztbm_abxc01dt FROM TABLE it_abxc01dt.
  REFRESH it_abxc01dt.
  GET TIME.
  LOOP AT t_abxc01dt.
    MOVE-CORRESPONDING t_abxc01dt TO it_abxc01dt.
    MOVE : sy-uname TO it_abxc01dt-zuser,
           sy-datum TO it_abxc01dt-zsdat,
           sy-uzeit TO it_abxc01dt-zstim.
    APPEND it_abxc01dt.
  ENDLOOP.

  MODIFY  ztbm_abxc01dt FROM TABLE it_abxc01dt.
  IF sy-subrc EQ 0.
    t_abxc01dt-zresult = 'S'.
    MODIFY t_abxc01dt TRANSPORTING  zresult
      WHERE zc01_key1_c0000 <> space.
**Other function call : for making text file : OCN DATA
*    DATA : it_abxocndt LIKE ztbm_abxocndt  OCCURS 0 WITH HEADER LINE.
*    DATA : BEGIN OF it_ocn OCCURS 0,
*        record(310),
*        END OF it_ocn.
*    DATA : f_num(2) TYPE n,
*           s_num(2) TYPE n,
*           field(16).
*    CLEAR: f_num,s_num.
*    FIELD-SYMBOLS <fs> TYPE ANY.
*    REFRESH it_abxocndt.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_abxocndt
*          FROM ztbm_abxocndt .
*    LOOP AT  it_abxocndt.
*      it_ocn-record+0(1)    = it_abxocndt-zyear.
*      it_ocn-record+1(5)    = it_abxocndt-natn.
*      it_ocn-record+6(10)   = it_abxocndt-base.
*      it_ocn-record+16(4)   = it_abxocndt-ocno.
*      it_ocn-record+20(2)   = it_abxocndt-vers.
*      it_ocn-record+22(4)   = it_abxocndt-occn.
*      f_num = 26. s_num = 1.
*      DO 60 TIMES.
*        CONCATENATE 'IT_ABXOCNDT-OP' s_num INTO field.
*        ASSIGN (field) TO <fs>.
*        it_ocn-record+f_num(4) = <fs>.
*        f_num = f_num + 4.s_num = s_num + 1.
*      ENDDO.
*      it_ocn-record+266(12)   = it_abxocndt-zuser.
*      it_ocn-record+278(6)   = '      '.
*      it_ocn-record+282(8)   = it_abxocndt-zsdat.
*      it_ocn-record+290(6)   = it_abxocndt-zstim.
*      it_ocn-record+296(8)   = it_abxocndt-zedat.
*      it_ocn-record+304(6)   = it_abxocndt-zetim.
*
*      APPEND it_ocn.
*
*    ENDLOOP.
*    CLEAR w_int.
*    DESCRIBE TABLE it_ocn LINES w_int.
*    IF w_int <> 0.
*      DATA : d_datum LIKE sy-datum.
*      d_datum = sy-datum - 1.
*      CONCATENATE  p_mobis_imt
*                     d_datum '_ocn.txt'
*                 INTO w_dsn.
*      DELETE DATASET w_dsn.
*      CONCATENATE  p_mobis_imt
*                   sy-datum '_ocn.txt'
*               INTO w_dsn.
*
*      DELETE DATASET w_dsn.
*
*      OPEN DATASET w_dsn IN TEXT MODE FOR APPENDING.
*
*      LOOP AT it_ocn.
*        TRANSFER it_ocn TO w_dsn.
*      ENDLOOP.
*
*      CLOSE DATASET w_dsn.
*      IF sy-subrc = 0.
*        MESSAGE i000 WITH text-001 '(OCN)'.
*      ELSE.
*        MESSAGE i000 WITH text-002 '(OCN)'.
*      ENDIF.
*
*    ENDIF.

  ELSE.
    t_abxc01dt-zresult = 'E'.
    MODIFY t_abxc01dt TRANSPORTING zresult
       WHERE zc01_key1_c0000 <> space.
  ENDIF.

ENDFUNCTION.
