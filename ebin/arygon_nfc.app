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
  {mod, { arugon_nfc_app, []}},
  {modules,[arygon_nfc, arygon__nfc_app, arygon_nfc_sup]}
 ]}.
