# Every target below is a name, not a file. The old list named two targets that do not
# exist (`all`, `test_sram`) and omitted eight that do.
.PHONY: gen toroidal_ws toroidal_ws_dual toroidal_ws_32 other_benchmark \
		test test_acc_sram_block clean \
		test_scalar test_dot_product test_sub_array test_sub_array_dot_product \
		test_scalar_acc test_dot_product_acc test_sub_array_acc test_sub_array_dot_product_acc \
		test_sram_scalar test_sram_dot_product test_sram_sub_array test_sram_sub_array_dot_product \
		test_grouped test_toroidal \
		test_dual test_dual_suite test_dual_no_acc test_dual_acc test_dual_stress \
		test_dual_gemm test_dual_gemm_no_acc test_dual_gemm_acc \
		test_dual_mode_switch test_dual_mode_switch_gemv

#generation config
RTL_DIR ?= output
# `?=` keeps the FIRST assignment, so the old duplicate pair left `make gen` pointing at
# toroidal_ws_test.json, which does not exist in src/main/resources/gen_config/.
TOROIDAL_WS ?= toroidal_ws.json
TOROIDAL_WS_DUAL ?= toroidal_ws_dual.json
TOROIDAL_WS_32 ?= toroidal_ws_32.json
OTHER_ARCHITECTURE ?= other_architecture.json


#test config
CONFIG_SCALAR ?= 1.conventional_systolic_array.json
CONFIG_DOT_PRODUCT ?= 2.dot_product_systolic_array.json
CONFIG_SUB_ARRAY ?= 3.sub_array_systolic_array.json
CONFIG_SUB_ARRAY_DOT_PRODUCT ?= 4.sub_array_dot_product_systolic_array.json

CONFIG_SCALAR_ACC ?= 1.conventional_systolic_array_acc.json
CONFIG_DOT_PRODUCT_ACC ?= 2.dot_product_systolic_array_acc.json
CONFIG_SUB_ARRAY_ACC ?= 3.sub_array_systolic_array_acc.json
CONFIG_SUB_ARRAY_DOT_PRODUCT_ACC ?= 4.sub_array_dot_product_systolic_array_acc.json

CONFIG_DUAL ?= 5.toroidal_dual_mode.json
GEMM_DUAL_M1_JSON ?= gemm_dual_m1.json

CONFIG_DUAL_ACC ?= 6.toroidal_dual_mode_acc.json
GEMM_DUAL_ACC_JSON ?= gemm_dual_acc.json

# Same two config files, driven through the STOCK tiled-GEMM schedule (M > 1) instead
# of the low-M GEMV one.
GEMM_DUAL_GEMM_JSON ?= gemm_dual_gemm.json
GEMM_DUAL_GEMM_ACC_JSON ?= gemm_dual_gemm_acc.json

# Settle-bound stress: the tightest weight-load settle constants, over long GEMV streams.
CONFIG_DUAL_STRESS ?= 5s.toroidal_dual_settle_stress.json
GEMM_DUAL_M1_HEAVY_JSON ?= gemm_dual_m1_heavy.json

# The mode switch's shapes live in test_suite/dual.json, not here: a switch row names a
# PAIR of files plus the case that says a switch is being run, which no Makefile variable
# can carry. gemm_dual_switch.json — the old single file with "schedule": "switch" in it —
# is gone with it.

# ── Suite manifests: WHERE THE PAIRS LIVE ────────────────────────────────────
# A target is a NAME, not a pair carrier. Every (config, GEMM) pair is declared in
# test_suite/*.json and every test target is one invocation form over it, so adding a pair
# is one manifest row rather than a new variable plus a new target — and two targets can
# no longer silently mean the same thing, because the case id is the key.
SUITE ?= all.json
SUITE_GROUPED ?= grouped.json
SUITE_TOROIDAL ?= toroidal.json
SUITE_DUAL ?= dual.json

GEMM_4_4_4_JSON ?= gemm(4,4,4).json
GEMM_4_8_4_JSON ?= gemm(4,8,4).json
GEMM_4_4_16_JSON ?= gemm(4,4,16).json
GEMM_8_8_8_JSON ?= gemm(8,8,8).json
GEMM_16_16_16_JSON ?= gemm(16,16,16).json
GEMM_16_16_64_JSON ?= gemm(16,16,64).json
GEMM_32_32_32_JSON ?= gemm(32,32,32).json
GEMM_256_256_256_JSON ?= gemm(256,256,256).json

gen: toroidal_ws other_benchmark

toroidal_ws:
	sbt "runMain torus.GenerationMain $(TOROIDAL_WS) $(RTL_DIR)"

toroidal_ws_dual:
	sbt "runMain torus.GenerationMain $(TOROIDAL_WS_DUAL) $(RTL_DIR)"

# 32-valued localVectorPeCol/multPerPe combos missing from toroidal_ws and
# toroidal_ws_dual (both Toroidal ReuseB and DualMode variants in one file).
toroidal_ws_32:
	sbt "runMain torus.GenerationMain $(TOROIDAL_WS_32) $(RTL_DIR)"

other_benchmark:
	sbt "runMain torus.GenerationMain $(OTHER_ARCHITECTURE) $(RTL_DIR)"

# The whole matrix in one dispatcher run, then the block-level SRAM test. It replaces a
# thirteen-target prerequisite list in which FOUR pairs ran twice — test_sram_scalar and
# test_scalar_acc were byte-identical recipes, as were the other three test_sram_* — so
# every accumulator pair, INCLUDING the 256x256x256 one, was simulated twice per run for
# no extra coverage. The names survive below as aliases.
#
# It also sweeps the dual rows, which the old list did not: the coverage matrix is only
# meaningful if a `make test` run drives every case, and a case printing
# `0 pass 0 fail 0 skip` is exactly what it exists to make visible.
test:
	sbt "runMain torus.TestMain --suite $(SUITE)"
	$(MAKE) test_acc_sram_block

# One rule, one form: a target names a CASE, the manifest names the pairs. Make prefers an
# explicit rule to a pattern one, so every named target below keeps its own meaning.
#   make test_grouped_noacc   make test_dual_switch_gemv_acc   ...
test_%:
	sbt "runMain torus.TestMain --suite $(SUITE) --case $(subst _,-,$*)"

# Whole fabrics.
test_grouped:
	sbt "runMain torus.TestMain --suite $(SUITE_GROUPED)"

test_toroidal:
	sbt "runMain torus.TestMain --suite $(SUITE_TOROIDAL)"

test_scalar:
	sbt "runMain torus.TestMain $(CONFIG_SCALAR) $(GEMM_4_4_4_JSON)"

# All Toroidal ReuseA/ReuseB/ReuseC broadcast configs now live in the sub-array configs —
# run via test_sub_array / test_sram_sub_array (non-acc) and test_sub_array_acc (acc).

test_dot_product:
	sbt "runMain torus.TestMain $(CONFIG_DOT_PRODUCT) $(GEMM_4_4_16_JSON)"

test_sub_array:
	sbt "runMain torus.TestMain $(CONFIG_SUB_ARRAY) $(GEMM_16_16_16_JSON)"

test_sub_array_dot_product:
	sbt "runMain torus.TestMain $(CONFIG_SUB_ARRAY_DOT_PRODUCT) $(GEMM_16_16_64_JSON)"

test_scalar_acc:
	sbt "runMain torus.TestMain $(CONFIG_SCALAR_ACC) $(GEMM_16_16_16_JSON)"

test_dot_product_acc:
	sbt "runMain torus.TestMain $(CONFIG_DOT_PRODUCT_ACC) $(GEMM_16_16_16_JSON)"

test_sub_array_acc:
	sbt "runMain torus.TestMain $(CONFIG_SUB_ARRAY_ACC) $(GEMM_32_32_32_JSON)"

test_sub_array_dot_product_acc:
	sbt "runMain torus.TestMain $(CONFIG_SUB_ARRAY_DOT_PRODUCT_ACC) $(GEMM_256_256_256_JSON)"

test_sram_scalar:
	sbt "runMain torus.TestMain $(CONFIG_SCALAR_ACC) $(GEMM_16_16_16_JSON)"

test_sram_dot_product:
	sbt "runMain torus.TestMain $(CONFIG_DOT_PRODUCT_ACC) $(GEMM_16_16_16_JSON)"

test_sram_sub_array:
	sbt "runMain torus.TestMain $(CONFIG_SUB_ARRAY_ACC) $(GEMM_32_32_32_JSON)"

test_sram_sub_array_dot_product:
	sbt "runMain torus.TestMain $(CONFIG_SUB_ARRAY_DOT_PRODUCT_ACC) $(GEMM_256_256_256_JSON)"

# Toroidal DualMode. Two schedules on the SAME array, told apart by M alone:
#   M == 1  -> low-M GEMV. A is broadcast-loaded through the stationary path and B
#              streams on the per-VectorPE second-stream lanes (inputA2 for ReuseB,
#              inputB2 for ReuseA).
#   M  > 1  -> stock tiled GEMM. Both mode selects held low, so the StationaryDemux
#              passes its bus through and every MAC takes the stock systolic stream —
#              the wrapper degenerates to the plain Toroidal array it is built around.
# Both dataflows live in each config file, so one suite run covers the whole surface.
# The four single-pair targets below stay for driving one pair at a time.
test_dual: test_dual_suite

test_dual_suite:
	sbt "runMain torus.TestMain --suite $(SUITE_DUAL)"

# Raw per-cycle partial sums, no accumulator attached.
test_dual_no_acc:
	sbt "runMain torus.TestMain $(CONFIG_DUAL) $(GEMM_DUAL_M1_JSON)"

# K-passes accumulated in the two big SRAMs. With M == 1 the accumulator's free
# output dimension is N, so freeDimensionSize is the stream steps per K-pass and
# N must be a whole number of resident panels (freeDimensionSize * output ports).
test_dual_acc:
	sbt "runMain torus.TestMain $(CONFIG_DUAL_ACC) $(GEMM_DUAL_ACC_JSON)"

# The tightest settle bounds (5s.*) over the long-stream GEMV shapes. Neither file had a
# target before the harness merge, so both were unreachable from make.
test_dual_stress:
	sbt "runMain torus.TestMain $(CONFIG_DUAL_STRESS) $(GEMM_DUAL_M1_HEAVY_JSON)"

# Same suite as test_dual — the stock-GEMM pairs are two of its four rows — plus the
# mode switch. make de-duplicates the shared prerequisite, so `make test_dual
# test_dual_gemm` still runs the suite once.
test_dual_gemm: test_dual_suite test_dual_mode_switch

# Per-tile validation, no accumulator attached.
test_dual_gemm_no_acc:
	sbt "runMain torus.TestMain $(CONFIG_DUAL) $(GEMM_DUAL_GEMM_JSON)"

# The output-indexed panel schedule through the two big SRAMs. K must be at least one
# full contraction pass (see the gemm file's comment) and within maxContractionSize.
test_dual_gemm_acc:
	sbt "runMain torus.TestMain $(CONFIG_DUAL_ACC) $(GEMM_DUAL_GEMM_ACC_JSON)"

# Both modes on ONE elaborated array with ONE reset: normal GEMM tile -> dual-mode GEMV
# burst -> the same normal GEMM tile again, so the selects are shown to be a runtime
# choice and the GEMV is shown to leave no state behind.
test_dual_mode_switch:
	sbt "runMain torus.TestMain --suite $(SUITE_DUAL) --case dual-switch-gemm,dual-switch-gemm-acc"

# The mirror order. It proves the converse of the above — that a GEMM tile leaves no
# residue in the second-stream registers the following GEMV can see — which the first
# order cannot see, because the two use different halves of the fabric.
test_dual_mode_switch_gemv:
	sbt "runMain torus.TestMain --suite $(SUITE_DUAL) --case dual-switch-gemv,dual-switch-gemv-acc"

# Block-level ping-pong SRAM test, plus the multi-port randomized sweep that used to
# run inside TestMain as its `--block` stage. It builds its configs in Scala rather than
# from a JSON file, so config-surface changes can break it while every TestMain suite
# above stays green — keep it in `test`.
test_acc_sram_block:
	sbt "runMain torus.AccSramDoubleBufferTest"

# SIM_DIR was expanded here but never defined, so the sim workspace was never cleaned
# (harmlessly — `rm -rf` with no operand is a no-op).
SIM_DIR ?= simWorkspace

clean:
	sbt clean
	rm -rf $(RTL_DIR)
	rm -rf $(SIM_DIR)
