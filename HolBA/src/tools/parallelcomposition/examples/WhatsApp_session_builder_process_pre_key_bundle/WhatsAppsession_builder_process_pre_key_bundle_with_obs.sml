open HolKernel Parse
open binariesLib;
open WhatsApp_session_builder_process_pre_key_bundleTheory;
open bir_symbexec_stateLib;
open bir_symbexec_coreLib;
open bir_symbexec_stepLib;
open bir_symbexec_sumLib;
open bir_block_collectionLib;
open bir_programSyntax;
open bir_valuesSyntax;
open bir_immSyntax;
open bir_expSyntax;
open bir_exec_typingLib;
open commonBalrobScriptLib;
open bir_cfgLib;
open bir_cfg_m0Lib;
open bir_symbexec_driverLib;
open Redblackmap;
open bir_symbexec_oracleLib;
open sbir_treeLib;
open sapicplusTheory;
open sapicplusSyntax;
open translate_to_sapicTheory;
open rich_listTheory;
open translate_to_sapicLib;
open messagesTheory;
open messagesSyntax;
open tree_to_processLib;
open sapic_to_fileLib;
open bir_symbexec_loopLib;
open bir_inst_liftingHelpersLib;
open gcc_supportLib;
open bir_obs_modelTheory;
open bir_obs_modelLib;
open bir_program_labelsSyntax;
open wordsSyntax;
     
 val ERR      = Feedback.mk_HOL_ERR "WhatsApp_session_builder_process_pre_key_bundle"
 val wrap_exn = Feedback.wrap_exn   "WhatsApp_session_builder_process_pre_key_bundle"


 val _ =  bir_symbexec_step_execstep_spec := true;

     
fun update_n_dict_ ([], n_dict) = n_dict
    | update_n_dict_ (((lbl_tm)::todo), n_dict) =
	  let
	    val n = { CFGN_lbl_tm   =  lbl_tm,
		  CFGN_hc_descr = SOME " ",
		  CFGN_targets  = [],
		  CFGN_type     = CFGNT_Halt
		} : cfg_node;
	    val n_dict' = if isSome (lookup_block_dict n_dict lbl_tm)
			  then
			      n_dict
			  else
			      Redblackmap.update (n_dict, lbl_tm, K (n));
			      
	  in
	    update_n_dict_ (todo, n_dict')
	  end;    
     
val (_, _, _, prog_tm) =
  (dest_bir_is_lifted_prog o concl)
      (DB.fetch "WhatsApp_session_builder_process_pre_key_bundle" "WhatsApp_session_builder_process_pre_key_bundle_thm");
    
val prog_range       = ((Arbnum.fromInt 0x00000000000450c), (Arbnum.fromInt 0x000000001b7ba37));

val entry = Arbnum.fromInt 0x0000000000ee5e1c;
    
fun embexp_params_cacheable x = Arbnum.+ (Arbnum.fromInt 0x0000000, x);

val stack_pointer_portion = Arbnum.fromHexString "0x0";    

val mem_bounds =
      let
        val (mem_base, mem_len) = prog_range;
	val mem_max = Arbnum.+ (mem_base, mem_len);
	val mem_end = (Arbnum.- (Arbnum.- (mem_max, stack_pointer_portion), Arbnum.fromInt 16));
	val (sp_start, sp_end) = (Arbnum.- (mem_max, stack_pointer_portion),
				  Arbnum.- (mem_max, Arbnum.fromInt 16));
      in
	if Arbnum.< (Arbnum.+ (mem_base,stack_pointer_portion), Arbnum.- (mem_max,stack_pointer_portion)) then
          pairSyntax.mk_pair
	    (pairSyntax.mk_pair
		 (wordsSyntax.mk_wordi (embexp_params_cacheable mem_base, 64),
		  wordsSyntax.mk_wordi (embexp_params_cacheable mem_end, 64)),
	     pairSyntax.mk_pair
		 (wordsSyntax.mk_wordi (embexp_params_cacheable sp_start, 64),
		  wordsSyntax.mk_wordi (embexp_params_cacheable sp_end, 64)))
	else
	  raise ERR "scamv_phase_add_obs" "the experiment memory is not properly set"
      end;
        
fun proginst_fun prog = inst [Type`:'observation_type` |-> Type`:bir_val_t`] prog;

val speculation = false;

val obs_id = if speculation = true
	     then "cache_speculation"
	     else "mem_address_pc";

val prog_w_obs = (#add_obs (get_obs_model obs_id)) mem_bounds (proginst_fun prog_tm) entry;

val bl_dict_org    = gen_block_dict prog_tm;
    
val prog_lbl_tms_org = get_block_dict_keys bl_dict_org;
val n_dict_org = bir_cfgLib.cfg_build_node_dict bl_dict_org prog_lbl_tms_org;   
    
val bl_dict_spec    = gen_block_dict prog_w_obs;
val prog_lbl_tms_spec = get_block_dict_keys bl_dict_spec;
val n_dict_spec = bir_cfgLib.cfg_build_node_dict bl_dict_spec prog_lbl_tms_spec; 
    
val prog_vars = (gen_vars_of_prog prog_w_obs) handle e => raise wrap_exn "prog_vars" e;
    
val adv_mem = “BVar "Adv_MEM" (BType_Mem Bit64 Bit8)”;

val prog_vars = adv_mem::prog_vars;

val bv_key = ``BVar "key" (BType_Imm Bit64)``;

val prog_vars = bv_key::prog_vars;

val op_mem = “BVar "Op_MEM" (BType_Mem Bit64 Bit8)”;

val prog_vars = op_mem::prog_vars;
    
val crypto = “BVar "Crypto" (BType_Imm Bit64)”;

val prog_vars = crypto::prog_vars;

    
val lbl_tm = ``BL_Address (Imm64 0xEE5E1Cw)``;

val stop_lbl_tms = [``BL_Address (Imm64 0xEE5EDCw)``,“BL_Address (Imm64 0xEE5F8Cw)”,“BL_Address (Imm64 0xEE5FA8w)”,“BL_Address (Imm64 0xEE5FB0w)”,“BL_Address (Imm64 0xEE5F80w)”,“BL_Address (Imm64 0xEE5FCCw)”,“BL_Address (Imm64 0xEE5FC8w)”];

    
val g1 = cfg_create "toy" [lbl_tm] n_dict_org bl_dict_org;

val n_dict_org = update_n_dict_ ((#CFGG_nodes g1),(#CFGG_node_dict g1));
    
    
val adr_dict = (bir_symbexec_PreprocessLib.fun_addresses_dict n_dict_org) handle e => raise wrap_exn "adr_dict" e;

    
val syst = init_state lbl_tm prog_vars;

val pred_conjs = [``bir_exp_true``];
    
val init_syst = state_add_preds "init_pred" pred_conjs syst;

val _ = print "initial state created.\n\n";

val cfb = false;

val systs = symb_exec_to_stop (abpfun cfb) n_dict_org bl_dict_spec [init_syst] stop_lbl_tms adr_dict [];
val _ = print "\n\n";
val _ = print "finished exploration of all paths.\n\n";
val _ = print ("number of stopped symbolic execution states: " ^ (Int.toString (length systs)));
val _ = print "\n\n";

val (systs_noassertfailed, systs_assertfailed) =
    List.partition (fn syst => not (identical (SYST_get_status syst) BST_AssertionViolated_tm)) systs;
val _ = print ("number of \"assert failed\" paths found: " ^ (Int.toString (length systs_assertfailed)));
val _ = print "\n";     
val _ = print ("number of \"no assert failed\" paths found: " ^ (Int.toString (length systs_noassertfailed)));
val _ = print "\n";

    
val predlists = List.map (fn syst => ((rev o SYST_get_pred) syst))
                         systs_noassertfailed;
    
val _ = print "Get predlists";
val _ = print "\n";
    
val predlists_refined = List.map (fn lst => bir_symbexec_sortLib.removeDuplicates lst) predlists;
val _ = print "Get refined predlists";    
val _ = print "\n";

    
val tree = predlist_to_tree predlists_refined;

val _ = print "Get tree";
val _ = print "\n";
    
val vals_list = bir_symbexec_treeLib.symb_execs_vals_term systs_noassertfailed [];

val _ = print "Get vals_list";
val _ = print "\n";
	
val sort_vals = bir_symbexec_sortLib.refine_symb_val_list vals_list;

val _ = print "Get sort_vals";
val _ = print "\n";    

val valtr =  tree_with_value tree sort_vals;
     
val _ = print ("built a symbolic tree with value");
val _ = print "\n";

    
val full = true;

val _ = if full = false then
	    let
		val _ = simplification := true;
		    
		val purged_tree = (purge_tree valtr);

		val _ = print ("built a purged tree\n");

		val sapic_process = sbir_tree_sapic_process sort_vals purged_tree;

		val _ = print ("built sapic_process\n");

		val refined_process = refine_process sapic_process;

		val rset = ((Redblackset.empty Term.compare): term Redblackset.set);
		    
		val process_with_live_vars = process_live_vars rset refined_process;
		    
		val _ = print ("built a refined process with live variables\n");

		val _ =  ( write_sapic_to_file o process_to_string) refined_process;	    
	    in
		print ("wrote into file\n")
	    end
	else
	    let
		val sapic_process = sbir_tree_sapic_process sort_vals valtr;

		val _ = print ("built sapic_process\n");

		val _ =  ( write_sapic_to_file o process_to_string) sapic_process;
	    in
		print ("wrote into file\n")
	    end;
		
    


