*&---------------------------------------------------------------------*
*& Include ZAPP221M_TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp221m_nation_def    message-id zmpp.

tables: ztpp_nation_def,
        ztppvn1,
        zsppvn1.

data: begin of it_app221  occurs 0,
        seq    type i.
include   structure  ztpp_nation_def.
data  end of it_app221.

data: begin of is_app221.
include   structure  ztpp_nation_def.
data  end of is_app221.
*
data: begin of  it_func_1203  occurs 0,   "GUI tool bar control
         fcode  like  rsmpe-func,
      end   of  it_func_1203.
*
data: it_zsppvn1 like zsppvn1 occurs 0 with header line.

data: g_company_1203(10),
      g_toggle_1203(1).
data: g_seq_1203  type  i,
      g_EDITDATA_1203,
      G_ANSWER_1203,
      C_DEST(10) value 'WMPP01'.   "Outbound Interface Destination
