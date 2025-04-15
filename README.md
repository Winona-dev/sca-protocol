## Beyond Correctness: Automated Side-Channel Analysis of Cryptographic Protocols Implementations via Binary Verification

This repository contains the implementation of our framework which is built on top of CryptoBap framework. Our toolchain integrates four key components: 


- **Symbolic Execution:**

    - Improving CryptoBap’s crypto-aware symbolic execution to handle observations during the symbolic execution, placed in <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/symbexec/examples/libload">libload</a>.


- **Symbolic Execution Tree Translation:**

    - Refining the symbolic execution tree translation of the BIR program to the Sapic model, located in <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/parallelcomposition/prettyPrint/tree_to_processLib.sml#L147">tree_to_process</a>.

- **Simplification rules combined with live variable analysis:**

	- Reducing model complexity with simplification rules applied at levels <a href="https://github.com/Winona-dev/sca-protocol/blob/master/HolBA/src/tools/parallelcomposition/tree/sbir_treeLib.sml#L180">SBIR</a>, <a href="https://github.com/Winona-dev/sca-protocol/blob/master/HolBA/src/tools/parallelcomposition/prettyPrint/sapic_to_fileLib.sml#L313">Sapic refinement</a>, and <a href="https://github.com/Winona-dev/sca-protocol/blob/master/HolBA/src/tools/parallelcomposition/prettyPrint/sapic_to_fileLib.sml#L348">Sapic with live variable</a>.
	

- **Analysis Examples:**

	- Demonstrating the real-world applications of our methodology with analyzing the BAC protocol used in e-passports and WhatsApp, the world’s most widely used messaging application. The <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/parallelcomposition/examples">examples</a> contains essential files for extracting the Sapic model of each component, along with the results from executing the model using ProVerif, Tamarin, and DeepSec tools. 
		
## How to setup and compile


1. Establish the HolBA framework according to the guidelines provided in <a href="https://github.com/Winona-dev/sca-protocol/blob/master/HolBA/README.md">HolBA-README.md</a>. There is no need to clone HolBA separately; a version that is compatible with our framework is available in our repository.

2. **(optional step)** To generate BIR programs for the analyzed protocol binaries, run `Holmake` in the <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/symbexecbin">symbexecbin</a> directory. The BIR programs will be stored in ***\*Theory.sig*** files located in this directory. Alternatively, they will be created automatically the first time you execute an example.

3. Run the `make src/tools/parallelcomposition/examples/subdirectory/your-chosen-example.sml_run` command for your selected example while in the <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA">HolBA</a> directory. The model you extract will be saved in the ***Sapic_Translation.txt*** file located in the relevant example subdirectory. Ensure you define the cryptographic primitives’ assumptions and security properties in the extracted model before assessing it with the Sapic toolchain. For comprehensive instructions on this process and to see the outcomes we received from the Sapic toolchain backends, consult <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/parallelcomposition/examples/Results">Results</a>.


## Running example

The example is set for execution and demonstrates our core functionality using predefined inputs, files, and expected results. Now, we will clarify this example to help users create their own based on the supplied foundation. To this end, we will implement the Basic Access Control protocol as described in our paper.

1. Begin by putting the binary implementation file for the BAC protocol in the <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/symbexecbin">symbexecbin</a> directory.

2. Lift either the entire binary file to a BIR program (use `read_disassembly_file_regions` function) or transpile specific code fragments to BIR (use `read_disassembly_file_regions_filter` function) by specifying code fragments as inputs in the <a href="https://github.com/Winona-dev/sca-protocol/blob/master/HolBA/src/tools/symbexecbin/AliceScript.sml">script file</a> dedicated to BAC protocol.

3. Specify the program-under-verification’s entry and exit addresses in the <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/parallelcomposition/examples/BAC/Combination-BAC.sml">Combination-BAC</a> file, as outlined below:

    ```
    val lbl_tm = ``BL_Address (Imm64 240w)``;

    val stop_lbl_tms = [``BL_Address (Imm64 696w)``,``BL_Address (Imm64 536w)``,``BL_Address (Imm64 520w)``,``BL_Address (Imm64 528w)``,``BL_Address (Imm64 544w)``,``BL_Address (Imm64 548w)``];
    ```

4. Set the `obs_id` to the observation model you want to augment your BIR program with using the Scam-V observational models (which can be found <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/scamv/obsmodel/bir_obs_modelLib.sml#L863">here</a>), set the value `full` to true if you want to obtain the complete Sapic model, otherwise, set it to false for the simplified Sapic model in the <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/parallelcomposition/examples/BAC/Combination-BAC.sml">Combination-BAC</a> file.

5. Next, execute this command:

	- `make src/tools/parallelcomposition/examples/BAC/Combination-BAC.sml_run`

6. You can later access the extracted Sapic model in the ***Sapic_Translation.txt*** file within the <a href="https://github.com/Winona-dev/sca-protocol/tree/master/HolBA/src/tools/parallelcomposition/examples/BAC">BAC</a> directory.
