open HolKernel Parse
open PPBackEnd;
open boolLib pairLib;
open bir_inst_liftingLib;
open bir_inst_liftingHelpersLib;
open gcc_supportLib;

val _ = Parse.current_backend := PPBackEnd.vt100_terminal;
val _ = set_trace "bir_inst_lifting.DEBUG_LEVEL" 2;

val _ = new_theory "WhatsApp_session_cipher_encrypt";

val arch_str         = "arm8";

val dafilename = "disassembly__session_cipher_encrypt.da";

val prog_range       = ((Arbnum.fromInt 0x00000000000450c), (Arbnum.fromInt 0x000000001b7ba37));

val _ = print_with_style_ [Bold, Underline] ("Lifting " ^ dafilename ^ " (" ^ arch_str ^ ")\n");

val (region_map, sections) = read_disassembly_file_regions  dafilename;
                            
val (thm, errors) = bmil_arm8.bir_lift_prog_gen prog_range sections;

val _ = save_thm ("WhatsApp_session_cipher_encrypt_thm", thm);



val _ = export_theory();



