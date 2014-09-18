*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU16                                                   *
*----------------------------------------------------------------------*


  loop at t_ltap_vb.
    move : t_ltap_vb-nltyp to t_ltap_vb-vltyp,     " Storage Type
           t_ltap_vb-nlpla to t_ltap_vb-vlpla.     " Storage Bin
    select single nltyp
                  nlpla
                        into (t_ltap_vb-nltyp, t_ltap_vb-nlpla)
                        from t333
                       where lgnum eq 'P01'
                         and bwlvs eq '311'.
    move : ' '   to t_ltap_vb-ndnoc.
    modify t_ltap_vb.
  endloop.
