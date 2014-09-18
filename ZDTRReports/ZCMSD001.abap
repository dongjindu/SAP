report ZCMSD001.
tables: fds1, fds2, fdsr, t036q.

select single * from t036q where inteb = '101'.   "sales orders
if sy-subrc = 0 and t036q-ebene <> space.
  delete from fdsr where ebene = t036q-ebene.
  delete from fds1 where vbeln = space or vbeln <> space.
  delete from fds2 where awtyp = space or awtyp <> space.
  commit work.

  submit rffdsd10.
endif.
