// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <cstring>
#include <string>
#include <vector>
#include <stdexcept>
#include <cstdint>
#include <algorithm>
using namespace Rcpp;

// MAT v5 constants ------------------------------------------------------------
static const uint32_t miINT8    = 1;
static const uint32_t miUINT16  = 4;
static const uint32_t miINT32   = 5;
static const uint32_t miUINT32  = 6;
static const uint32_t miDOUBLE  = 9;
static const uint32_t miMATRIX  = 14;

static const uint32_t mxSTRUCT_CLASS = 2;
static const uint32_t mxCHAR_CLASS   = 4;
static const uint32_t mxDOUBLE_CLASS = 6;

// raw helpers -----------------------------------------------------------------
static inline void writeRaw(std::vector<uint8_t>& buf, const void* src, size_t n){
  size_t o = buf.size(); buf.resize(o+n); std::memcpy(buf.data()+o, src, n);
}
static inline void writeU32(std::vector<uint8_t>& buf, uint32_t v){ writeRaw(buf, &v, 4); }
static inline void writeI32(std::vector<uint8_t>& buf, int32_t  v){ writeRaw(buf, &v, 4); }
static inline void pad8(std::vector<uint8_t>& buf){
  size_t r = buf.size() & 7u; if (r){ buf.insert(buf.end(), 8-r, 0); }
}
static inline void writeTag(std::vector<uint8_t>& buf, uint32_t type, uint32_t nbytes){
  writeU32(buf, type); writeU32(buf, nbytes);
}
static inline std::string validField(std::string s){
  std::replace(s.begin(), s.end(), ' ', '_');
  if (s.empty()) throw std::runtime_error("Empty field name");
  return s;
}
static void writeArrayFlags(std::vector<uint8_t>& buf, uint32_t mxClass, uint32_t flags=0){
  writeTag(buf, miUINT32, 8); writeU32(buf, mxClass); writeU32(buf, flags); pad8(buf);
}
static void writeDimensions(std::vector<uint8_t>& buf, const std::vector<int32_t>& dims){
  writeTag(buf, miINT32, (uint32_t)(4*dims.size()));
  for (int32_t d: dims) writeI32(buf, d);
  pad8(buf);
}
static void writeArrayName(std::vector<uint8_t>& buf, const std::string& name){
  writeTag(buf, miINT8, (uint32_t)name.size());
  if (!name.empty()) writeRaw(buf, name.data(), name.size());
  pad8(buf);
}
static void wrapAsMiMatrix(std::vector<uint8_t>& out, const std::vector<uint8_t>& payload){
  writeTag(out, miMATRIX, (uint32_t)payload.size());
  writeRaw(out, payload.data(), payload.size());
  pad8(out);
}

// leaf: double arrays ---------------------------------------------------------
// Emits a MATLAB empty double array [] (0x0) with the given name.
static void serialize_EmptyDouble(std::vector<uint8_t>& out, const std::string& varname){
  std::vector<uint8_t> payload;
  writeArrayFlags(payload, mxDOUBLE_CLASS, 0);
  writeDimensions(payload, {0,0});
  writeArrayName(payload, varname);
  writeTag(payload, miDOUBLE, 0);
  pad8(payload);
  wrapAsMiMatrix(out, payload);
}

static void serialize_DoubleArray_payload(std::vector<uint8_t>& payload, const std::string& varname, SEXP x){
  if (!Rf_isNumeric(x) && !Rf_isLogical(x))
    throw std::runtime_error("Non-numeric leaf at '" + varname + "'.");
  SEXP rx = Rf_coerceVector(x, REALSXP);
  double* pr = REAL(rx);
  size_t len = (size_t)Rf_length(rx);

  SEXP dim = Rf_getAttrib(rx, R_DimSymbol);
  std::vector<int32_t> dims;

  if (Rf_isNull(dim)) {
    // IMPORTANT: length-0 vector -> [] (0x0) instead of 0x1
    if (len == 0) dims = {0,0};
    else          dims = { (int32_t)len, 1 };
  } else {
    IntegerVector d(dim);
    dims.assign(d.begin(), d.end());
    // allow zeros explicitly (MAT v5 supports 0-dims to indicate empty)
    for (auto& v: dims) v = std::max(0, v);
  }

  // element count from dims (not from len)
  size_t nelem = 1;
  for (int32_t v : dims) nelem *= (size_t)v;

  writeArrayFlags(payload, mxDOUBLE_CLASS, 0);
  writeDimensions(payload, dims);
  writeArrayName(payload, varname);
  writeTag(payload, miDOUBLE, (uint32_t)(8 * nelem));
  if (nelem > 0) writeRaw(payload, pr, 8 * nelem);
  pad8(payload);
}

static void serialize_DoubleArray(std::vector<uint8_t>& out, const std::string& varname, SEXP x){
  std::vector<uint8_t> payload;
  serialize_DoubleArray_payload(payload, varname, x);
  wrapAsMiMatrix(out, payload);
}

// leaf: char arrays (uint16) --------------------------------------------------
static inline void writeU16Buffer(std::vector<uint8_t>& buf, const uint16_t* src, size_t n){
  writeTag(buf, miUINT16, (uint32_t)(2*n)); writeRaw(buf, src, 2*n); pad8(buf);
}
static void serialize_CharArray(std::vector<uint8_t>& out, const std::string& varname, SEXP x){
  if (TYPEOF(x) != STRSXP) throw std::runtime_error("serialize_CharArray: non-character");
  R_xlen_t n = Rf_length(x);
  std::vector<std::string> svec; svec.reserve(n);
  size_t maxlen = 0;
  for (R_xlen_t i=0;i<n;++i){
    std::string s = (STRING_ELT(x,i)==NA_STRING) ? "" : Rcpp::as<std::string>(STRING_ELT(x,i));
    maxlen = std::max(maxlen, s.size());
    svec.push_back(std::move(s));
  }
  int32_t m = (int32_t)std::max<R_xlen_t>(1, n);
  int32_t L = (int32_t)maxlen;

  std::vector<uint8_t> payload;
  writeArrayFlags(payload, mxCHAR_CLASS, 0);
  writeDimensions(payload, {m, L});
  writeArrayName(payload, varname);

  if (L > 0){
    // Column-major uint16 char array, space-padded.
    std::vector<uint16_t> data((size_t)m*(size_t)L, (uint16_t)' ');
    for (int32_t row=0; row<m; ++row){
      const std::string& s = svec[std::min<int32_t>(row, (int32_t)svec.size()-1)];
      for (int32_t col=0; col<(int32_t)std::min<size_t>(maxlen, s.size()); ++col){
        data[(size_t)row + (size_t)m*(size_t)col] = (uint16_t)(unsigned char)s[col];
      }
    }
    writeU16Buffer(payload, data.data(), data.size());
  } else {
    writeTag(payload, miUINT16, 0);
    pad8(payload);
  }
  wrapAsMiMatrix(out, payload);
}

// Forward decl
static void serialize_Struct(std::vector<uint8_t>& out, const std::string& varname, List x);

// struct serializer (NUL+align; treats empty list() as [])
static void serialize_Struct(std::vector<uint8_t>& out, const std::string& varname, List x){
  // NEW: empty list() => [] (empty double), not an empty struct
  if (Rf_xlength(x) == 0) {
    serialize_EmptyDouble(out, varname);
    return;
  }

  // Require names for non-empty list (we don't support cells)
  SEXP nmsAttr = Rf_getAttrib(x, R_NamesSymbol);
  if (Rf_isNull(nmsAttr)) throw std::runtime_error("Unnamed list in struct '" + varname + "'");
  CharacterVector names(nmsAttr);
  const int nfields = names.size();

  // Field names & L: include NUL, then 4-byte align
  size_t L0 = 1;
  std::vector<std::string> fields(nfields);
  for (int i=0;i<nfields;++i){
    fields[i] = validField( Rcpp::as<std::string>(names[i]) );
    L0 = std::max<size_t>(L0, fields[i].size() + 1); // +1 for NUL terminator
  }
  size_t L = (L0 + 3) & ~((size_t)3); // 4-byte alignment

  // Build this struct's payload
  std::vector<uint8_t> payload;
  writeArrayFlags(payload, mxSTRUCT_CLASS, 0);
  writeDimensions(payload, {1,1}); // scalar struct
  writeArrayName(payload, varname);

  // Field name length subelement
  writeTag(payload, miINT32, 4);
  writeU32(payload, (uint32_t)L);
  pad8(payload);

  // Field names blob (nfields * L), zero-padded (zeros provide the NUL)
  {
    std::vector<uint8_t> namesBlob((size_t)nfields * L, 0);
    for (int i=0;i<nfields;++i){
      std::memcpy(namesBlob.data() + (size_t)i*L, fields[i].data(), fields[i].size());
    }
    writeTag(payload, miINT8, (uint32_t)namesBlob.size());
    writeRaw(payload, namesBlob.data(), namesBlob.size());
    pad8(payload);
  }

  // Field contents
  for (int i=0;i<nfields;++i){
    SEXP val = x[i];
    std::vector<uint8_t> child;

    if (Rf_isNewList(val) && !Rf_inherits(val, "data.frame")) {
      // nested list: empty list() becomes [], otherwise struct
      serialize_Struct(child, "", as<List>(val)); // empty array-name inside struct field
      writeRaw(payload, child.data(), child.size()); pad8(payload);
      continue;
    }

    if (Rf_isString(val)) {
      serialize_CharArray(child, "", val);
      writeRaw(payload, child.data(), child.size()); pad8(payload);
      continue;
    }

    if (Rf_inherits(val, "POSIXt") || Rf_inherits(val, "Date")) {
      SEXP num = Rf_coerceVector(val, REALSXP);
      serialize_DoubleArray(child, "", num);
      writeRaw(payload, child.data(), child.size()); pad8(payload);
      continue;
    }

    if (Rf_isNumeric(val) || Rf_isLogical(val)) {
      // length-0 numeric/logical will serialize as [] due to updated numeric path
      serialize_DoubleArray(child, "", val);
      writeRaw(payload, child.data(), child.size()); pad8(payload);
      continue;
    }

    throw std::runtime_error("Unsupported type in field '" + fields[i] + "'");
  }

  wrapAsMiMatrix(out, payload);
}

// file header -----------------------------------------------------------------
static void writeMatHeader(std::vector<uint8_t>& out){
  char text[116]; std::memset(text,' ',sizeof(text));
  const char* msg = "MATLAB 5.0 MAT-file, Created by: rTwig";
  std::memcpy(text, msg, std::min<size_t>(std::strlen(msg), sizeof(text)));
  writeRaw(out, text, sizeof(text));
  uint8_t zeros8[8] = {0}; writeRaw(out, zeros8, 8);
  uint16_t ver = 0x0100; writeRaw(out, &ver, 2);
  char endian[2] = {'I','M'}; writeRaw(out, endian, 2);  // little-endian
}

// top-level API ---------------------------------------------------------------
// [[Rcpp::export]]
void write_mat(
    std::string filename,
    Rcpp::List cylinder,
    Rcpp::List branch,
    Rcpp::List treedata,
    Rcpp::List rundata,
    Rcpp::List pmdistance,
    Rcpp::List triangulation) {
  auto ensure_named_or_empty = [](const List& L, const char* where){
    R_xlen_t n = Rf_xlength(L);
    if (n == 0) return; // allow empty list (empty struct)
    SEXP nms = Rf_getAttrib(L, R_NamesSymbol);
    if (Rf_isNull(nms)) stop("Argument '%s' must be a named list (or empty).", where);
  };
  ensure_named_or_empty(cylinder,      "cylinder");
  ensure_named_or_empty(branch,        "branch");
  ensure_named_or_empty(treedata,      "treedata");
  ensure_named_or_empty(rundata,       "rundata");
  ensure_named_or_empty(pmdistance,    "pmdistance");
  ensure_named_or_empty(triangulation, "triangulation");

  List qsm = List::create(
    _["cylinder"]      = cylinder,
    _["branch"]        = branch,
    _["treedata"]      = treedata,
    _["rundata"]       = rundata,
    _["pmdistance"]    = pmdistance,
    _["triangulation"] = triangulation
  );

  std::vector<uint8_t> filebuf; writeMatHeader(filebuf);
  std::vector<uint8_t> top; serialize_Struct(top, "qsm", qsm);
  writeRaw(filebuf, top.data(), top.size()); pad8(filebuf);

  FILE* fp = std::fopen(filename.c_str(), "wb");
  if (!fp) stop("Could not open file for writing: %s", filename.c_str());
  size_t n = std::fwrite(filebuf.data(), 1, filebuf.size(), fp);
  std::fclose(fp);
  if (n != filebuf.size()) stop("Failed to write complete MAT file.");
}
