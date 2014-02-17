{application, arygon_nfc,
 [
  {description, "Arygon NFC erlang"},
  {vsn, "1.0"},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
		  gnuart
                 ]},
  {mod, { arygon_nfc_app, []}},
  {modules,[arygon_nfc, arygon__nfc_app, arygon_nfc_sup]}
 ]}.
