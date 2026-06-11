# dafr (R 0.4.4) vs DataAxesFormats.jl (0.3.0) - Parity Audit & Fix Plan

Date: 2026-06-11. Reference: `~/src/DataAxesFormats.jl` @ `80aee1d` (latest main, v0.3.0).
dafr under test: source `494b47c` (v0.4.4), installed to `/tmp/dafrlib044` (no c-blosc).

Every finding below was **executed in both languages** (not static-only). Raw data:
`verdicts.json` (51 confirmed + 3 refuted), `wf_all.json` (26-subsystem gap map, 218 gaps),
`modes_compare.csv` (multi-mode benchmark).

---

## 1. Bottom line

**"As fast as Julia": essentially yes.** On the shared 79-query bake-off, single-threaded:
warm repeated queries ~parity (dafr ~4x slower on fixed per-query overhead, but sub-millisecond
either way); raw compute at parity (plain Julia `sum` = dafr kernel = 6 ms); dafr only shows
large wins (10-100x) when *repeatedly recomputing heavy sparse reductions from a cold cache*,
and that is a DAF.jl 0.3.0 query-path inefficiency on sparse data, **not** dafr beating Julia
at computation. dafr is *slower* on mmap-reopen (~2x) and matrix `Convert` (8-13x).
(The earlier "10-14x faster" headline was an artifact of the cache-cleared benchmark; corrected here.)

**"Fully featured": no - and "bug-free": no.** dafr is a faithful port for the dominant
in-memory Float64 workflow, but the audit confirmed **55 real divergences**
(26 silent-wrong-answer, 20 crash/error-direction, 6 inherent-R-type-limits,
3 other) plus whole missing backends. 3 static suspicions were **refuted** by execution.

**Fix disposition of the 55 confirmed:** 40 FIX (real bugs, align to Julia) -
3 GUARD (type-limit, make it fail/convert loudly not silently) -
9 LIMIT (inherent R type system, document only) -
3 FEATURE (whole missing backend/API, separate build).

---

## 2. Performance (corrected, multi-cache-mode)

Ratio = dafr_median / julia_median (<1 = dafr faster). Single-threaded.

| category | warm (cache hits) | compute (recompute) | cold (clear all) |
|---|---|---|---|
| light  | 4.10x | 0.64x | 0.99x |
| kernel | 3.85x | 0.01x | 0.05x |
| grouped| 4.60x | 0.04x | 0.07x |
| chain  | 2.53x | 1.43x | 1.27x |
| **overall** | **4.05x** | **0.57x** | **0.89x** |

- **warm**: dafr ~4x slower (per-query fixed overhead ~0.13ms vs ~0.034ms; both sub-ms).
- **compute/cold**: dafr much faster on big-sparse only because DAF.jl re-runs an expensive
  sparse-reduction path each query (2.7s for a sum that needs 6ms). Raw `sum` is 6ms in both.
- Decomposition (10000x10000, 5M nnz): plain Julia sum 6.3ms - DAF.jl get_matrix 0.06ms -
  DAF.jl `>| Sum` query 2686ms - dafr `>| Sum` 6ms.

---

## 3. Confirmed bugs (executed in both languages)

### 3a. Silent-wrong-answer (26) - highest priority

| probe | subsystem | divergence | R vs Julia | disp |
|---|---|---|---|---|
| `P1-match-unescaped` | query parser | Unescaped regex metachar after '~': R silently accepts and runs the pattern (returning a real match result for '^U'), while Julia rejects the same str | R: '@ cell : type ~ ^X' -> R OK, logical [F,F,F,F,F] (parses '^ ... // J: '@ cell : type ~ ^X' and '@ cell : type ~ ^U' -> Julia ERROR | **FIX** |
| `P1-uint32-overflow` | files_daf_read | dafr reads a Julia-written UInt32 dense vector as signed Int32: values >= 2^31 silently become NA / negative, no error. | R: get_vector returns integer vector c(1, NA, -1, 0, 100). The  ... // J: get_vector returns UInt32[1, 2147483648, 4294967295, 0, 100] | **GUARD** |
| `P2-match-overconsume` | query-tokens-parse | R greedily collects `^`,`A`,`:`,`label` into one regex pattern '^A:label' (IsMatch), silently returning an all-FALSE mask and swallowing the trailing  | R: get_query(d,'@ cell : type ~ ^A : label') returns a 4-elemen ... // J: parse_query('@ cell : type ~ ^A : label') is a hard PARSE ER | **FIX** |
| `P4-named-default-wrong-order` | readers-writers-readonly | A named full-length default whose names are not in axis order: Julia errors (require_axis_names); dafr silently reorders by position, returning each a | R: get_vector(d,"type","no_such", default=c(MPP=1,`MEBEMP-L`=2, ... // J: get_vector(d,"type","no_such"; default=NamedArray([4,3,2,1], | **FIX** |
| `P4-uint32-sparse-overflow` | files_daf_read sparse pa | Sparse UInt32 path is corrupted identically to dense (NA / -1 for values >= 2^31), and 0x80000000 entries are silently dropped from the nonzero set. | R: get_vector returns c(0,0,NA,0,0,0,-1,0,100,0). Nonzero entri ... // J: get_vector returns UInt32 with nonzero positions [3,7,9] =>  | **GUARD** |
| `VIEW-P1` | views | Masked-axis __axis__ vector with cardinality-dependent op (Fraction): R divides by the full-axis sum then subsets (0.333/0.5); Julia divides by the ma | R: get_vector(v,'cell','frac') => Y=0.3333333, Z=0.5000000 (no  ... // J: get_vector(v,"cell","frac") => [0.4, 0.6] for names ["Y","Z" | **FIX** |
| `VIEW-P7` | views | Masked-axis matrix with per-column Fraction: R computes column fractions over all 3 cells then subsets (columns sum to 0.833/0.733); Julia computes ov | R: get_matrix(v,'cell','gene','frac') => Y/A=0.333, Z/A=0.5, Y/ ... // J: get_matrix(v,"cell","gene","frac") => [0.4 0.4545...; 0.6 0. | **FIX** |
| `anndata-dense-X-orientation` | anndata-format | dafr writes/reads dense /X with no transpose, so its h5ad is transposed vs canonical: non-square crashes anndata and crashes dafr-reading-Julia, squar | R: dafr writes dense /X via h5$create_dataset('X', robj=X) with ... // J: daf_as_anndata writes a correct canonical (obs x var) h5ad.  | **FIX** |
| `chain-relayout-writer-mutation` | chains | On a write-chain whose matrix lives in a read-only base, the default relayout_matrix call ERRORS in Julia (and never mutates the writer); dafr instead | R: relayout_matrix(ch,'cell','gene','UMIs') SUCCEEDS silently a ... // J: relayout_matrix!(ch,'cell','gene','UMIs') ERRORS: 'existing  | **FIX** |
| `complete-intermediate-view` | complete | R silently ignores intermediate-repository base_daf_view (root data seen unviewed); Julia tries to apply it recursively but its own complete_chain!/co | R: 3-level chain root<-mid<-leaf, mid carries a data view renam ... // J: Julia collect_dafs (complete.jl:99-123) reads base_daf_view  | **FIX** |
| `complete-view-scope` | complete_daf view scope | R silently returns the BASE value (10,20) and HIDES leaf-local data ('missing vector') when a viewed chain is reopened, because R wraps the whole chai | R: complete_daf wraps the ENTIRE chain (base+leaf) in viewer()  ... // J: By design (collect_dafs complete.jl:106-122) Julia chains th | **FIX** |
| `concat-scalar-collect-missing` | concat | MERGE_COLLECT_AXIS for a scalar present in one source but missing in another: dafr silently fills NA (c(1,NA)); Julia hard-errors (cannot construct th | R: concatenate(d,'cell',list(a,b), merge=list('version'=MERGE_C ... // J: concatenate!(d,"cell",[a,b], merge=["version"=>CollectAxis]) | **FIX** |
| `files-int-sparse-matrix-eltype-loss` | files-format | dafr read+rewrite of a Julia Int32 sparse matrix silently changes the on-disk nzval eltype from Int32 to Float64 (values equal, dtype lost); Julia pre | R: Reading Julia's Int32 sparse matrix returns a dgCMatrix with ... // J: FilesDaf set_matrix! writes SparseMatrixCSC{Int32,Int32}: x. | **LIMIT** |
| `files-uint32-overflow-read` | files-format | Julia-written UInt32 value >= 2^31 reads back in dafr as a negative R integer (signed 32-bit) with no error - silent wrong answer. | R: get_vector(d2,'cell','u') => 1, -1294967296, 2 (class intege ... // J: set_vector!(d,'cell','u', UInt32[1, 3000000000%UInt32, 2]);  | **GUARD** |
| `groups-P1` | groups | group_names name strings are completely incompatible between impls: Julia = prefix+index+'.'+2-digit SHA256-derived suffix (groups.jl:46); R = prefix+ | R: group_names(d,'cell',list(c(1,2),c(3,4)),prefix='M') -> ['M4 ... // J: group_names(['c1'..'c4'],[[1,2],[3,4]];prefix='M') -> ['M1.5 | **FIX** |
| `groups-P2` | groups | Two distinct groups with identical membership: Julia disambiguates via the embedded group index (G1.56 vs G2.56); R has no index so it silently return | R: group_names(d,'cell',list(c(1,2),c(1,2)),prefix='G') -> ['G4 ... // J: group_names(['c1','c2'],[[1,2],[1,2]];prefix='G') -> ['G1.56 | **FIX** |
| `mem-setmat-relayout-default` | memory-format | Julia set_matrix! defaults relayout=true (stores both layouts); R set_matrix defaults relayout=FALSE (stores only one) - same call yields different pe | R: set_matrix(d,'cell','gene','m', matrix(1:6,2,3)) then has_ma ... // J: set_matrix!(d,'cell','gene','m', Matrix{Int64}(reshape(1:6,2 | **FIX** |
| `mem-sparse-vec-read` | memory-format | MemoryDaf sparse-vector round-trip: Julia preserves the SparseVector base array (issparse==true); R densifies to a plain numeric vector (is_sparse==FA | R: set_vector(d,'cell','sv', Matrix::sparseVector(...)) then ge ... // J: set_vector!(d,'cell','sv', SparseVector(6,[2,5],[1.5,2.5]))  | **FIX** |
| `qe-countby-bytewise-order` | query-eval | CountBy row/col dimname ordering diverges: Julia bytewise (rows Z,a; cols Y,b) vs R locale (rows a,Z; cols b,Y). dimnames order flips, so positional d | R: get_query(d,'@ cell : a_lab * b_lab') under en_US.UTF-8 retu ... // J: Same query returns 2x2 with ROWNAMES=Z,a and COLNAMES=Y,b (b | **FIX** |
| `qe-groupby-bytewise-order` | query-eval | GroupBy result-name ordering diverges: Julia bytewise (B,Z,a,c) vs R locale en_US.UTF-8 (a,B,c,Z). Values correctly track names in each, so any index/ | R: get_query(d,'@ cell : val / grp >> Sum') under LC_COLLATE=en ... // J: Same query returns NAMES=B,Z,a,c VALUES=30,10,20,40 (bytewis | **FIX** |
| `recon-empties-keyset` | reconstruction | When a migrated property has no empty-implicit entries, Julia keeps the key with value `nothing`; dafr drops the key entirely (R's `empty_values[[prop | R: reconstruct_axis(existing_axis='cell', implicit_axis='donor' ... // J: reconstruct_axis!(...) returns a dict with keys ['donor_age' | **FIX** |
| `recon-empty-implicit-rewrite` | reconstruction | Julia rewrites the implicit property's empty_implicit entries to '' on the existing axis; dafr leaves the original 'NA' sentinel strings in place, so  | R: After reconstruct_axis(existing_axis='cell', implicit_axis=' ... // J: After reconstruct_axis!(...; empty_implicit="NA"), get_vecto | **FIX** |
| `recon-int-rewrite` | reconstruction | Numeric implicit property: Julia rewrites the implicit vector back as a String with '' for empty entries (reconstruction.jl), but R never rewrites the | R: After reconstruct_axis(existing='cell', implicit='batch', em ... // J: After reconstruct_axis!(... empty_implicit=0): get_vector(m, | **FIX** |
| `reorder-float32-widen` | reorder | Reordering a Float32 dense vector silently widens its on-disk eltype to Float64 in dafr (Julia keeps Float32), because dafr reads Float32 into an R do | R: files_daf with vectors/cell/age.json descriptor 'Float32' (w ... // J: Same Julia-written Float32 fixture, reorder_axes!(d, Dict('c | **LIMIT** |
| `reorder-uint16-indtype` | reorder | Reordering a sparse matrix widens BOTH its index type (UInt16->UInt32 via .indtype_for_size, which never returns UInt16) and its value type (Int32->Fl | R: files_daf with a 3x2 sparse Int32 matrix whose colptr/rowval ... // J: Same UInt16-indexed Int32 fixture, reorder_axes!(d, Dict('ce | **FIX** |
| `zarr-int64-sparse-nzval-precision` | zarr-format | Any int64 sparse nzval read by dafr is silently CORRUPTED (raw bit-pattern reinterpreted as double, not narrowed) - garbage for both large and small v | R: WORSE than 'precision loss'. R get_vector of a Julia SparseV ... // J: Julia get_vector returns the exact dense Int64 vector: [0, 9 | **FIX** |

### 3b. Crash / error-direction (20)

| probe | subsystem | divergence | R vs Julia | disp |
|---|---|---|---|---|
| `COMP-01` | computations | R's computation() wrapper calls contractor(...) without an overwrite arg (computations.R:57-59), so re-running a single-contract computation with over | R: first run OK (q=1). Re-run with overwrite=TRUE ERRORS: 'pre- ... // J: first run OK (q=1.0). Re-run with overwrite=true SUCCEEDS, q | **FIX** |
| `CTR-P1` | contracts | A pre-existing OptionalOutput scalar with overwrite=FALSE: Julia verify_input passes silently (only CreatedOutput is forbidden as input), R verify_inp | R: verify_input(cd) ERROR: 'pre-existing OptionalOutput scalar: ... // J: verify_input(cd) returns nothing, no error (with DAF_ENFORCE | **FIX** |
| `CTR-P2` | contracts | Pre-existing OptionalOutput axis on input: R rejects, Julia DAF 0.3.0 accepts silently. | R: verify_input ERROR: 'pre-existing OptionalOutput axis: cell\ ... // J: verify_input OK (no error). cd is a real ContractDaf (enforc | **FIX** |
| `CTR-P3` | contracts | Pre-existing GuaranteedOutput scalar on input: R rejects, Julia DAF 0.3.0 accepts (GuaranteedOutput inert). | R: verify_input ERROR: 'pre-existing GuaranteedOutput scalar: v ... // J: verify_input OK (no error). enforcement ON. is_forbidden(Gua | **FIX** |
| `CTR-P4` | contracts | GuaranteedOutput scalar never created at verify_output: R rejects (missing), Julia DAF 0.3.0 passes. | R: verify_output ERROR: 'missing output scalar: version\nwith t ... // J: verify_output OK (no error). enforcement ON. is_mandatory(Gu | **FIX** |
| `P1-name-index-reserved` | readers-writers-readonly | Reserved 'name'/'index' vector properties exist as virtual reader-API vectors in Julia (has_*=true, return entry names / 1..n) but are entirely missin | R: has_vector(d,'type','name')=FALSE; has_vector(d,'type','inde ... // J: has_vector(d,'type','name')=true; has_vector(d,'type','index | **FIX** |
| `PZM-01` | packed-zip-mmap | Default-codec (blosc_zstd_bitshuffle) packed FilesDaf stores written by Julia are unreadable by a no-blosc dafr build (clean error); switching the wri | R: dafr (have_blosc=FALSE build) reading the default-codec pack ... // J: FilesDaf(tmp,"w+";packed=true) writes x.json with compressio | **LIMIT** |
| `PZM-02` | packed-zip-mmap | Julia ZipDaf single-file .daf.zip archives are unreadable by dafr: open_daf misroutes them to files_daf and emits a misleading 'not a daf directory (n | R: dafr open_daf('/tmp/pzm02.daf.zip','r') ERRORS: 'files_daf(‘ ... // J: ZipDaf('x.daf.zip','w+') writes a single-file archive (entri | **FEATURE** |
| `VIEW-P5` | views | Matrix view query with __rows_axis__ placeholder: Julia expands it to the rows-axis query and returns the matrix; R has no __rows_axis__/__columns_axi | R: get_matrix(v,'obs','var','x') => ERROR: 'missing axis: __row ... // J: get_matrix(v,"obs","var","x") => SUCCESS, returns the 3x2 ce | **FEATURE** |
| `adapter-insist-collision-scalar` | adapters | Adapter output view copying back a scalar that already exists in the base with overwrite=false: Julia errors ('existing scalar: kept'), R silently ski | R: adapter result: RETURNED:ok ; get_scalar(d,'kept') after = 1 ... // J: adapter result: ERROR:existing scalar: kept / in the daf dat | **FIX** |
| `complete-rplus-view-readonly` | complete_daf r+ writabil | For a VIEWED r+ chain, R returns a read-only ViewDaf so leaf writes error, while Julia's complete_daf crashes at reopen before returning anything -- t | R: complete_daf(nd,'r+') on a chain carrying base_daf_view retu ... // J: complete_daf(nd,'r+') on a VIEWED chain does NOT keep the le | **FIX** |
| `complete-view-json-xlang` | complete_chain | The base_daf_view JSON is structurally incompatible (Julia single-key objects + paren-tuple matrix keys vs R positional arrays); cross-language comple | R: complete_chain (complete.R:60-63 jsonlite::toJSON(list(axes= ... // J: complete_chain! (chains.jl:186-190 JSON.json(Dict(:axes=>,:d | **FIX** |
| `copy-all-both-layouts` | copies | copy_all from a source that physically stores a matrix in both layouts: Julia copies it exactly once (guard 'columns_axis >= rows_axis' + slash filter | R: copy_all(dst, src, relayout=TRUE) ERROR: 'existing matrix: U ... // J: copy_all!(...; relayout=true) SUCCESS; dst has UMIs in both  | **FIX** |
| `copy-flipped-matrix` | copies | copy_matrix requesting canonical orientation from a source that stored only the flipped layout (relayout=FALSE): Julia copies via on-the-fly transpose | R: copy_matrix(dst, src, 'cell','gene','UMIs', relayout=TRUE) E ... // J: copy_matrix!(...; rows_axis='cell', columns_axis='gene', rel | **FIX** |
| `copy-tensor-missing-slice-no-empty` | copies | copy_tensor with a main-axis entry whose per-entry source matrix is missing and empty=NULL: Julia skips the missing slice (default=empty=nothing -> ge | R: copy_tensor(dst, src, 'batch','gene','cell','counts', relayo ... // J: copy_tensor!(...; main_axis='batch', name='counts', relayout | **FIX** |
| `files-http-client-cross` | files-format | The two FilesDaf HTTP layouts are mutually incompatible: dafr serves metadata.zip (no metadata.json), Julia serves metadata.json (no metadata.zip), so | R: R http_daf fetches <url>/metadata.zip (http_format.R:62). vs ... // J: Julia HttpDaf fetches only <url>/metadata.json (http_format. | **FIX** |
| `files-sparse-string-matrix-read` | files-format | Julia auto-sparsifies a large mostly-empty string matrix to .nztxt; dafr's reader only knows .nzval and errors out reading it back. | R: get_matrix(d2,'r','c','lbl') => R ERROR: "files_daf: sparse  ... // J: Julia round-trips: writes lbl.colptr/lbl.rowval/lbl.nztxt (+ | **FEATURE** |
| `http-wire-protocol-cross-read` | http-format | R serves/expects metadata.zip, Julia serves/expects metadata.json; the two HTTP clients each 404 on the other's served tree and cannot interoperate. | R: R FilesDaf writes a metadata.zip bundle at root and NO metad ... // J: Julia FilesDaf writes a single-line metadata.json index ({"a | **FIX** |
| `reorder-zarr-crash-unrecoverable` | reorder | dafr zarr_daf reorder has no backup/lock: a mid-reorder crash leaves a silently half-permuted store and reset_reorder_axes() returns FALSE (no recover | R: dafr zarr_daf: after a simulated crash mid-reorder (new_cras ... // J: Julia ZarrDaf: reorder_axes!(z, Dict("cell"=>perm); _simulat | **FIX** |
| `zarr-all-zero-dense-missing-chunk` | zarr-format | Julia elides the chunk for all-zero dense vectors (fill_value optimization) and reads them back as zeros; dafr ignores fill_value and errors 'missing  | R: R get_vector(z,'cell','zeros') -> stop: "vector 'zeros' miss ... // J: Julia ZarrDaf set_vector! of all-zero Float64[0,0,0,0,0] OMI | **FIX** |

### 3c. Inherent R type-system limits (6) - real but not cleanly fixable in R

| probe | subsystem | divergence | R vs Julia | disp |
|---|---|---|---|---|
| `OPS-01` | operations/reductions | Julia returns Float32-rounded reduction scalars; R has no Float32 and reduces in Float64, so results differ at ~8th significant digit (rel diff ~4e-8) | R: On a Float32 'score'=[0.1,0.2,0.3,0.7,1.1] reduced via '@ ce ... // J: Julia keeps Float32: '@ cell : score >> Mean' -> type=Float3 | **LIMIT** |
| `OPS-03` | operations/Convert | Julia converts to an exact UInt64 (18000000000000000000); R routes UInt64 through signed bit64::integer64, overflows above 2^63, and silently yields N | R: '@ cell : big % Convert type UInt64' on big=Float64[1.8e19]  ... // J: Julia returns NamedVector{UInt64} value=0xf9ccd8a1c5080000 = | **LIMIT** |
| `TKR-04-convert-float32-precision` | types-keys-registry | Convert type Float32 on a scalar: Julia actually rounds to Float32 (0.10000000149011612 when widened); dafr cannot - R has no Float32 type so it retur | R: set_scalar(d,"x",0.1); get_query(d, ". x % Convert type Floa ... // J: get_query(d, ". x % Convert type Float32") returns typeof=Fl | **LIMIT** |
| `TKR-05-uint8-write-roundtrip-eltype` | types-keys-registry | UInt8 (and Float32) eltype is not preserved on a dafr write: Julia stores eltype='UInt8' (4 bytes); dafr re-writes the same values as eltype='Int32' ( | R: dafr reads the Julia UInt8 FilesDaf matrix as R integer (val ... // J: Julia set_matrix!(d,"r","c","m", UInt8.([1 2;3 4])) writes d | **LIMIT** |
| `zarr-dtype-float32-roundtrip` | zarr-format | Julia preserves on-disk float32; after any R set_matrix the on-disk data_type becomes float64 (4->8 bytes), values equal - an unavoidable consequence  | R: Reading the Julia-written float32 store: storage.mode='doubl ... // J: ZarrDaf set_matrix! of Float32[1 2;3 4] writes on-disk "data | **LIMIT** |
| `zarr-dtype-int8-roundtrip` | zarr-format | Read values all correct; narrow dtypes (uint8/int8/int16/uint16) unreproducible by the R writer (always int32) and uint32-high reads surface as R doub | R: Julia ZarrDaf wrote u8/i8/i16/u16 with on-disk data_type uin ... // J: Julia keeps narrow types end-to-end: u8 UInt8, i8 Int8, i16  | **LIMIT** |

### 3d. Other (3)

| probe | subsystem | divergence | R vs Julia | disp |
|---|---|---|---|---|
| `P5-set-matrix-relayout-default` | readers-writers-readonly | set_matrix default differs: Julia materializes the flipped (row-major) layout (relayout=true), so has_matrix(c,r;relayout=false)=true; dafr defaults r | R: set_matrix(d,"r","c","m",matrix(1:6,2,3)) then has_matrix(d, ... // J: set_matrix!(d,"r","c","m",reshape(1:6,2,3)) then has_matrix( | **FIX** |
| `http-axes-set-source` | http-format | R enumerates axes from a bundled axes/metadata.json array (returns nothing if absent); Julia enumerates from the consolidated index's axes/<name> keys | R: format_axes_set early-returns character(0) if 'axes/metadata ... // J: HttpDaf derives axes via names_under(http,'axes') scanning c | **FIX** |
| `files-meta-json-missing` | files-format | A freshly-dafr-written FilesDaf dir lacks the root metadata.json that Julia 0.3.0 writes; Julia interop still works (it self-heals on open), so impact | R: Fresh R store root entries: axes, daf.json, matrices, metada ... // J: Fresh Julia store root has metadata.json (content e.g. {"axe | **FIX** |

---

## 4. Refuted (static suspicion, executed = parity)

These were flagged by static analysis but **R and Julia actually agree** - do not chase:

- `OPS-02`: REFUTED. The claim 'Julia wraps two's-complement' is false: both Julia and R raise InexactError on 'Sum type Int32' overflow. Parity. (Aside: the *default* Sum 
- `CTR-P5`: As written (dense matrix(as.double(1:6))), BOTH R and Julia reject -> parity holds; the probe's claimed R silent-accept is wrong for the dense path.
- `adapter-insist-collision-allmatrices`: Both R and Julia return 'ok' for the ALL_MATRICES+ALL_VECTORS fixture - the output view after double axis-rename does not re-expose the pre-existing UMIs/vector

---

## 5. Whole missing features (FEATURE - separate builds, mostly known/deliberate)

- **h5df** native single-HDF5 backend (`H5df`, `.h5df`) - ~2300 lines Julia, 0 R; `open_daf` hard-errors.
- **ZipDaf** (`.daf.zip` / `.dafs.zip#/group`) - no R reader; `open_daf` misroutes to `files_daf`.
- **Empty-buffer streaming-fill writer family** (`empty_dense_*`/`empty_sparse_*`/`get_empty_*`/`filled_empty_*`, 16 fns).
- **Packed (chunked+compressed) WRITE** for every backend (read works for non-blosc).
- **Tensor (3D) support** across views/concat/copies/contracts.
- **`.nztxt` sparse-string matrix read**; **HTTP Range/striped/lazy reads**; **`@computation` 2-/3-contract forms**.

---

## 6. Fix plan (proposed)

**Bucket A - FIX (40):** genuine logic/wire bugs with a clear Julia-aligned fix.
Each fixed with a failing-first differential test (the harness in `/tmp/dh` reproduces every one),
then verified to match Julia and not regress `tests/`. Highest-severity first:
view masked-axis `__axis__`, GroupBy/CountBy C-locale ordering, groups naming, concat missing-scalar
error, regex `~` single-token, reconstruction rewrites, contracts Optional/GuaranteedOutput strictness,
computation `overwrite` plumbing, copy_matrix flipped-read, relayout default, sparse-vector preservation,
reorder index width, chain relayout, named-default order, http/files root `metadata.json` interop,
zarr all-zero chunk, anndata X orientation, zarr int64 sparse nzval.

**Bucket B - GUARD (3):** UInt32/UInt64 overflow reads currently corrupt silently
(signed reinterpret). Fix = read unsigned into R `double` (exact to 2^53) and/or error past 2^53,
so values are preserved or it fails loudly - never silent corruption.

**Bucket C - LIMIT (9):** Float32 precision, narrow/unsigned dtype width on write -
R has no Float32/unsigned types. Document as known; optionally warn on write-narrowing.

**Bucket D - FEATURE (3+):** whole backends (§5) - separate scoped efforts.

---

## 7. Reproduction

- R: `R_LIBS=/tmp/dafrlib044 Rscript ...` (dafr 0.4.4)
- Julia: `conda activate dafr-mcview && julia --project=@v1.12 ...` (DAF 0.3.0, dev-pathed)
- Query differential harness: `/tmp/dh/{run_r.R,run_jl.jl,diff.py}`
- Bench: `/tmp/bench_modes_{r.R,jl.jl}`, `/tmp/compare_modes.R`
- Fixture: `dev/adversarial-parity/fixture.daf` (rich dtypes/edge cases)
