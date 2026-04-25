#include <Rcpp.h>
#include <fstream>
#include <vector>
#include <string>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <algorithm>
#include <cctype>
#include <set>
#include <stdexcept>

using namespace Rcpp;

// [[Rcpp::plugins(cpp17)]]

enum MatType {
  miINT8       = 1,
  miUINT8      = 2,
  miINT16      = 3,
  miUINT16     = 4,
  miINT32      = 5,
  miUINT32     = 6,
  miSINGLE     = 7,
  miDOUBLE     = 9,
  miINT64      = 12,
  miUINT64     = 13,
  miMATRIX     = 14,
  miCOMPRESSED = 15,
  miUTF8       = 16,
  miUTF16      = 17,
  miUTF32      = 18
};

enum MatClass {
  mxCELL_CLASS    = 1,
  mxSTRUCT_CLASS  = 2,
  mxOBJECT_CLASS  = 3,
  mxCHAR_CLASS    = 4,
  mxSPARSE_CLASS  = 5,
  mxDOUBLE_CLASS  = 6,
  mxSINGLE_CLASS  = 7,
  mxINT8_CLASS    = 8,
  mxUINT8_CLASS   = 9,
  mxINT16_CLASS   = 10,
  mxUINT16_CLASS  = 11,
  mxINT32_CLASS   = 12,
  mxUINT32_CLASS  = 13,
  mxINT64_CLASS   = 14,
  mxUINT64_CLASS  = 15,
  mxLOGICAL_CLASS = 16,
  mxOPAQUE_CLASS  = 17
};

struct Tag {
  uint32_t type;
  uint32_t bytes;
  bool small;
  uint32_t small_data;
};

static std::string LAST_MATRIX_NAME;

static std::streampos file_size(std::ifstream& f) {
  std::streampos cur = f.tellg();
  f.seekg(0, std::ios::end);
  std::streampos end = f.tellg();
  f.seekg(cur);
  return end;
}

static uint32_t read_u32(std::ifstream& f) {
  uint32_t x = 0;
  f.read(reinterpret_cast<char*>(&x), 4);
  if (!f) Rcpp::stop("Failed to read uint32");
  return x;
}

static void skip_pad(std::ifstream& f, uint32_t n) {
  uint32_t pad = (8 - (n % 8)) % 8;
  if (pad > 0) f.seekg(pad, std::ios::cur);
}

static std::streampos tell(std::ifstream& f) {
  return f.tellg();
}

static Tag read_tag(std::ifstream& f) {
  uint32_t a = read_u32(f);

  uint16_t type16  = a & 0xffff;
  uint16_t bytes16 = a >> 16;

  if (bytes16 != 0) {
    uint32_t data = read_u32(f);
    return {
      static_cast<uint32_t>(type16),
      static_cast<uint32_t>(bytes16),
      true,
      data
    };
  }

  uint32_t bytes = read_u32(f);
  return {a, bytes, false, 0};
}

static std::vector<char> read_payload(std::ifstream& f, const Tag& t) {
  std::vector<char> out(t.bytes);

  if (t.small) {
    std::memcpy(out.data(), &t.small_data, t.bytes);
  } else {
    if (t.bytes > 0) {
      f.read(out.data(), t.bytes);
      if (!f) Rcpp::stop("Failed to read payload");
    }
    skip_pad(f, t.bytes);
  }

  return out;
}

static IntegerVector read_dims(std::ifstream& f) {
  Tag t = read_tag(f);
  std::vector<char> raw = read_payload(f, t);

  int n = static_cast<int>(raw.size() / 4);
  IntegerVector dims(n);

  int32_t* p = reinterpret_cast<int32_t*>(raw.data());

  for (int i = 0; i < n; ++i) {
    dims[i] = p[i];
  }

  return dims;
}

static std::string read_name(std::ifstream& f) {
  Tag t = read_tag(f);
  std::vector<char> raw = read_payload(f, t);

  std::string s(raw.begin(), raw.end());

  while (!s.empty() && s.back() == '\0') {
    s.pop_back();
  }

  return s;
}

static int prod_dims(IntegerVector dims) {
  int n = 1;
  for (int i = 0; i < dims.size(); ++i) n *= dims[i];
  return n;
}

static SEXP read_matrix(std::ifstream& f, std::streampos matrix_end);

static std::string trim_nulls(std::string s) {
  while (!s.empty() && s.back() == '\0') s.pop_back();
  while (!s.empty() && s.front() == '\0') s.erase(s.begin());
  return s;
}

static bool useful_string(const std::string& s) {
  if (s.empty()) return false;

  std::string z = s;
  std::transform(z.begin(), z.end(), z.begin(), [](unsigned char c) {
    return std::tolower(c);
  });

  if (z == "string") return false;
  if (z == "matlab") return false;
  if (z == "opaque") return false;
  if (z == "mcos") return false;
  if (z == "canonical") return false;
  if (z == "object") return false;
  if (z == "value") return false;
  if (z == "data") return false;

  return true;
}

static CharacterVector extract_string_from_opaque_raw(const std::vector<char>& raw) {
  std::vector<std::string> candidates;

  // ASCII / UTF-8-ish printable runs
  std::string cur;
  for (size_t i = 0; i < raw.size(); ++i) {
    unsigned char c = static_cast<unsigned char>(raw[i]);

    if (c >= 32 && c <= 126) {
      cur.push_back(static_cast<char>(c));
    } else {
      cur = trim_nulls(cur);
      if (cur.size() >= 1 && useful_string(cur)) candidates.push_back(cur);
      cur.clear();
    }
  }

  cur = trim_nulls(cur);
  if (cur.size() >= 1 && useful_string(cur)) candidates.push_back(cur);

  // UTF-16LE printable runs
  cur.clear();
  for (size_t i = 0; i + 1 < raw.size(); i += 2) {
    unsigned char lo = static_cast<unsigned char>(raw[i]);
    unsigned char hi = static_cast<unsigned char>(raw[i + 1]);

    if (hi == 0 && lo >= 32 && lo <= 126) {
      cur.push_back(static_cast<char>(lo));
    } else {
      cur = trim_nulls(cur);
      if (cur.size() >= 1 && useful_string(cur)) candidates.push_back(cur);
      cur.clear();
    }
  }

  cur = trim_nulls(cur);
  if (cur.size() >= 1 && useful_string(cur)) candidates.push_back(cur);

  if (candidates.empty()) {
    return CharacterVector::create(NA_STRING);
  }

  // Usually the actual MATLAB string value is near the end of the opaque payload.
  return CharacterVector::create(candidates.back());
}

static SEXP read_opaque_array(std::ifstream& f, std::streampos matrix_end) {
  std::streampos pos = tell(f);
  if (pos >= matrix_end) return CharacterVector::create(NA_STRING);

  std::streamoff n = matrix_end - pos;
  std::vector<char> raw(static_cast<size_t>(n));

  if (n > 0) {
    f.read(raw.data(), n);
    if (!f) Rcpp::stop("Failed to read opaque payload");
  }

  return extract_string_from_opaque_raw(raw);
}

static SEXP read_numeric_array(std::ifstream& f,
                               uint8_t mx_class,
                               bool logical_flag,
                               IntegerVector dims) {
  Tag t = read_tag(f);
  std::vector<char> raw = read_payload(f, t);

  int n = prod_dims(dims);

  if (logical_flag || mx_class == mxLOGICAL_CLASS) {
    LogicalVector out(n);

    if (t.type == miUINT8) {
      uint8_t* p = reinterpret_cast<uint8_t*>(raw.data());
      for (int i = 0; i < n; ++i) out[i] = p[i] != 0;
    } else {
      Rcpp::stop("Unsupported logical storage type");
    }

    out.attr("dim") = dims;
    return out;
  }

  NumericVector out(n);

  switch (t.type) {
  case miDOUBLE: {
    double* p = reinterpret_cast<double*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miSINGLE: {
    float* p = reinterpret_cast<float*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miUINT8: {
    uint8_t* p = reinterpret_cast<uint8_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miINT8: {
    int8_t* p = reinterpret_cast<int8_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miUINT16: {
    uint16_t* p = reinterpret_cast<uint16_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miINT16: {
    int16_t* p = reinterpret_cast<int16_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miUINT32: {
    uint32_t* p = reinterpret_cast<uint32_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miINT32: {
    int32_t* p = reinterpret_cast<int32_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = p[i];
    break;
  }

  case miUINT64: {
    uint64_t* p = reinterpret_cast<uint64_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = static_cast<double>(p[i]);
    break;
  }

  case miINT64: {
    int64_t* p = reinterpret_cast<int64_t*>(raw.data());
    for (int i = 0; i < n; ++i) out[i] = static_cast<double>(p[i]);
    break;
  }

  default:
    Rcpp::stop("Unsupported numeric MATLAB data type: " + std::to_string(t.type));
  }

  out.attr("dim") = dims;
  return out;
}

static CharacterVector read_char_array(std::ifstream& f, IntegerVector dims) {
  Tag t = read_tag(f);
  std::vector<char> raw = read_payload(f, t);

  std::string s;

  if (t.type == miUINT8 || t.type == miINT8 || t.type == miUTF8) {
    s.assign(raw.begin(), raw.end());
  } else if (t.type == miUINT16 || t.type == miUTF16) {
    uint16_t* p = reinterpret_cast<uint16_t*>(raw.data());
    int n = static_cast<int>(raw.size() / 2);
    s.reserve(n);

    for (int i = 0; i < n; ++i) {
      if (p[i] < 128) s.push_back(static_cast<char>(p[i]));
      else s.push_back('?');
    }
  } else {
    Rcpp::stop("Unsupported char storage type: " + std::to_string(t.type));
  }

  s = trim_nulls(s);
  return CharacterVector::create(s);
}

static List read_cell(std::ifstream& f, IntegerVector dims) {
  int n = prod_dims(dims);
  List out(n);

  for (int i = 0; i < n; ++i) {
    Tag t = read_tag(f);

    if (t.type != miMATRIX) {
      Rcpp::stop("Expected miMATRIX inside cell array");
    }

    std::streampos end = tell(f) + static_cast<std::streamoff>(t.bytes);
    out[i] = read_matrix(f, end);
    f.seekg(end);
    skip_pad(f, t.bytes);
  }

  out.attr("dim") = dims;
  return out;
}

static List read_struct(std::ifstream& f, IntegerVector dims) {
  int nelem = prod_dims(dims);

  Tag field_len_tag = read_tag(f);
  std::vector<char> field_len_raw = read_payload(f, field_len_tag);

  if (field_len_raw.size() < 4) {
    Rcpp::stop("Invalid struct field-name length");
  }

  int32_t field_len = *reinterpret_cast<int32_t*>(field_len_raw.data());

  Tag field_names_tag = read_tag(f);
  std::vector<char> field_names_raw = read_payload(f, field_names_tag);

  int nfields = static_cast<int>(field_names_raw.size() / field_len);

  CharacterVector field_names(nfields);

  for (int i = 0; i < nfields; ++i) {
    const char* ptr = field_names_raw.data() + i * field_len;
    std::string nm(ptr, ptr + field_len);
    nm = trim_nulls(nm);
    field_names[i] = nm;
  }

  if (nelem == 1) {
    List out(nfields);
    out.attr("names") = field_names;

    for (int i = 0; i < nfields; ++i) {
      Tag t = read_tag(f);

      if (t.type != miMATRIX) {
        Rcpp::stop("Expected miMATRIX inside struct field");
      }

      std::streampos end = tell(f) + static_cast<std::streamoff>(t.bytes);
      out[i] = read_matrix(f, end);
      f.seekg(end);
      skip_pad(f, t.bytes);
    }

    return out;
  }

  List out(nelem);

  for (int e = 0; e < nelem; ++e) {
    List one(nfields);
    one.attr("names") = field_names;

    for (int i = 0; i < nfields; ++i) {
      Tag t = read_tag(f);

      if (t.type != miMATRIX) {
        Rcpp::stop("Expected miMATRIX inside struct array field");
      }

      std::streampos end = tell(f) + static_cast<std::streamoff>(t.bytes);
      one[i] = read_matrix(f, end);
      f.seekg(end);
      skip_pad(f, t.bytes);
    }

    out[e] = one;
  }

  out.attr("mat_struct_array") = true;
  return out;
}

static SEXP read_matrix(std::ifstream& f, std::streampos matrix_end) {
  Tag flags_tag = read_tag(f);
  std::vector<char> flags_raw = read_payload(f, flags_tag);

  if (flags_raw.size() < 8) {
    Rcpp::stop("Invalid array flags");
  }

  uint32_t flags = *reinterpret_cast<uint32_t*>(flags_raw.data());
  uint8_t mx_class = flags & 0xff;
  bool logical_flag = (flags & 0x0200) != 0;

  IntegerVector dims = read_dims(f);
  std::string name = read_name(f);
  if (!name.empty()) LAST_MATRIX_NAME = name;

  SEXP out;

  switch (mx_class) {
  case mxSTRUCT_CLASS:
    out = read_struct(f, dims);
    break;

  case mxCELL_CLASS:
    out = read_cell(f, dims);
    break;

  case mxCHAR_CLASS:
    out = read_char_array(f, dims);
    break;

  case mxOPAQUE_CLASS:
    out = read_opaque_array(f, matrix_end);
    break;

  case mxDOUBLE_CLASS:
  case mxSINGLE_CLASS:
  case mxINT8_CLASS:
  case mxUINT8_CLASS:
  case mxINT16_CLASS:
  case mxUINT16_CLASS:
  case mxINT32_CLASS:
  case mxUINT32_CLASS:
  case mxINT64_CLASS:
  case mxUINT64_CLASS:
  case mxLOGICAL_CLASS:
    out = read_numeric_array(f, mx_class, logical_flag, dims);
    break;

  default:
    Rcpp::stop("Unsupported MATLAB class: " + std::to_string(mx_class));
  }

  if (tell(f) < matrix_end) {
    f.seekg(matrix_end);
  }

  return out;
}

static std::string write_temp_raw(Rcpp::RawVector x) {
  Rcpp::Function tempfile("tempfile");
  std::string path = Rcpp::as<std::string>(tempfile());

  std::ofstream out(path, std::ios::binary);

  if (!out.is_open()) {
    Rcpp::stop("Could not create temporary file");
  }

  out.write(reinterpret_cast<const char*>(RAW(x)), x.size());
  out.close();

  return path;
}

// [[Rcpp::export]]
SEXP read_mat_nested(std::string path) {
  std::ifstream f(path, std::ios::binary);

  if (!f.is_open()) {
    Rcpp::stop("Could not open file");
  }

  std::streampos f_end = file_size(f);

  f.seekg(128, std::ios::beg);

  List out;
  CharacterVector out_names;

  while (tell(f) + std::streamoff(8) <= f_end) {
    LAST_MATRIX_NAME.clear();

    Tag t = read_tag(f);

    if (t.bytes == 0) {
      break;
    }

    std::streampos payload_start = tell(f);
    std::streampos payload_end   = payload_start + static_cast<std::streamoff>(t.bytes);

    if (payload_end > f_end) {
      break;
    }

    std::vector<char> payload = read_payload(f, t);

    if (t.type == miMATRIX) {
      Rcpp::RawVector raw(payload.size());
      std::memcpy(RAW(raw), payload.data(), payload.size());

      std::string tmp = write_temp_raw(raw);
      std::ifstream g(tmp, std::ios::binary);

      if (!g.is_open()) {
        std::remove(tmp.c_str());
        Rcpp::stop("Could not open temporary matrix stream");
      }

      std::streampos end = static_cast<std::streamoff>(payload.size());
      SEXP obj = read_matrix(g, end);

      g.close();
      std::remove(tmp.c_str());

      std::string nm = LAST_MATRIX_NAME.empty()
        ? ("var" + std::to_string(out.size() + 1))
        : LAST_MATRIX_NAME;

      out.push_back(obj);
      out_names.push_back(nm);
      continue;
    }

    if (t.type == miCOMPRESSED) {
      Rcpp::RawVector comp(payload.size());
      std::memcpy(RAW(comp), payload.data(), payload.size());

      Rcpp::Function memDecompress("memDecompress");
      Rcpp::RawVector inflated;

      try {
        inflated = memDecompress(comp, Rcpp::_["type"] = "unknown");
      } catch (...) {
        inflated = memDecompress(comp, Rcpp::_["type"] = "gzip");
      }

      std::string tmp = write_temp_raw(inflated);
      std::ifstream g(tmp, std::ios::binary);

      if (!g.is_open()) {
        std::remove(tmp.c_str());
        Rcpp::stop("Could not open decompressed temporary stream");
      }

      std::streampos g_end = file_size(g);

      if (tell(g) + std::streamoff(8) > g_end) {
        g.close();
        std::remove(tmp.c_str());
        Rcpp::stop("Invalid compressed MATLAB block");
      }

      Tag inner = read_tag(g);

      if (inner.type != miMATRIX) {
        g.close();
        std::remove(tmp.c_str());
        Rcpp::stop("Compressed block did not contain miMATRIX");
      }

      std::streampos end = tell(g) + static_cast<std::streamoff>(inner.bytes);

      if (end > g_end) {
        g.close();
        std::remove(tmp.c_str());
        Rcpp::stop("Invalid inner miMATRIX size");
      }

      SEXP obj = read_matrix(g, end);

      g.close();
      std::remove(tmp.c_str());

      std::string nm = LAST_MATRIX_NAME.empty()
        ? ("var" + std::to_string(out.size() + 1))
        : LAST_MATRIX_NAME;

      out.push_back(obj);
      out_names.push_back(nm);
      continue;
    }
  }

  if (out.size() == 0) {
    Rcpp::stop("No MATLAB matrix variables found");
  }

  if (out.size() == 1) {
    return out[0];
  }

  out.attr("names") = out_names;
  return out;
}

static bool keep_treeqsm_20_var(const std::string& nm) {
  static const std::vector<std::string> keep = {
    "Added", "Axe", "BAng", "BLen", "BOrd", "BPar", "BSeg", "BVol",
    "Base", "BoC", "CExt", "CPar", "FCB", "Len", "Rad", "SoC",
    "Sta", "TreeData"
  };

  return std::find(keep.begin(), keep.end(), nm) != keep.end();
}

// [[Rcpp::export]]
Rcpp::List read_mat_flat(std::string path) {
  std::ifstream f(path, std::ios::binary);
  if (!f.is_open()) Rcpp::stop("Could not open file");

  std::streamoff f_end = static_cast<std::streamoff>(file_size(f));
  std::streamoff pos = 128;

  Rcpp::List qsm;
  Rcpp::CharacterVector qsm_names;

  auto add_matrix = [&](std::ifstream& g,
                        std::streamoff start,
                        std::streamoff end) {
    g.clear();
    g.seekg(start);

    Tag flags_tag = read_tag(g);
    std::vector<char> flags_raw = read_payload(g, flags_tag);

    if (flags_raw.size() < 8) {
      Rcpp::stop("Invalid array flags");
    }

    uint32_t flags = *reinterpret_cast<uint32_t*>(flags_raw.data());
    uint8_t mx_class = flags & 0xff;
    bool logical_flag = (flags & 0x0200) != 0;

    IntegerVector dims = read_dims(g);
    std::string nm = read_name(g);

    // Skip anything not part of TreeQSM 2.0 BEFORE reading payload data.
    if (!keep_treeqsm_20_var(nm)) {
      g.clear();
      g.seekg(end);
      return;
    }

    // Only these classes are valid for TreeQSM 2.0 workspace vars.
    bool is_numeric =
      mx_class == mxDOUBLE_CLASS ||
      mx_class == mxSINGLE_CLASS ||
      mx_class == mxINT8_CLASS ||
      mx_class == mxUINT8_CLASS ||
      mx_class == mxINT16_CLASS ||
      mx_class == mxUINT16_CLASS ||
      mx_class == mxINT32_CLASS ||
      mx_class == mxUINT32_CLASS ||
      mx_class == mxINT64_CLASS ||
      mx_class == mxUINT64_CLASS ||
      mx_class == mxLOGICAL_CLASS;

    if (!is_numeric) {
      g.clear();
      g.seekg(end);
      return;
    }

    SEXP obj = read_numeric_array(g, mx_class, logical_flag, dims);

    qsm.push_back(obj);
    qsm_names.push_back(nm);

    g.clear();
    g.seekg(end);
  };

  while (pos + 8 <= f_end) {
    f.clear();
    f.seekg(pos);

    Tag t = read_tag(f);
    if (t.bytes == 0) break;

    std::streamoff payload_start = pos + 8;
    std::streamoff payload_end =
      payload_start + static_cast<std::streamoff>(t.bytes);

    if (payload_end > f_end) break;

    std::streamoff next_pos =
      (t.type == miCOMPRESSED)
      ? payload_end
    : payload_end + static_cast<std::streamoff>((8 - (t.bytes % 8)) % 8);

    if (t.type == miMATRIX) {
      add_matrix(f, payload_start, payload_end);
      pos = next_pos;
      continue;
    }

    if (t.type == miCOMPRESSED) {
      f.clear();
      f.seekg(payload_start);

      std::vector<char> payload(t.bytes);
      f.read(payload.data(), t.bytes);
      if (!f) Rcpp::stop("Failed to read compressed payload");

      Rcpp::RawVector comp(payload.size());
      std::memcpy(RAW(comp), payload.data(), payload.size());

      Rcpp::Function memDecompress("memDecompress");
      Rcpp::RawVector inflated;

      try {
        inflated = memDecompress(comp, Rcpp::_["type"] = "unknown");
      } catch (...) {
        inflated = memDecompress(comp, Rcpp::_["type"] = "gzip");
      }

      std::string tmp = write_temp_raw(inflated);
      std::ifstream g(tmp, std::ios::binary);

      if (!g.is_open()) {
        std::remove(tmp.c_str());
        Rcpp::stop("Could not open decompressed temp stream");
      }

      std::streamoff g_end = static_cast<std::streamoff>(file_size(g));
      std::streamoff gpos = 0;

      while (gpos + 8 <= g_end) {
        g.clear();
        g.seekg(gpos);

        Tag inner = read_tag(g);
        if (inner.bytes == 0) break;

        std::streamoff inner_payload_start = gpos + 8;
        std::streamoff inner_payload_end =
          inner_payload_start + static_cast<std::streamoff>(inner.bytes);

        if (inner_payload_end > g_end) break;

        std::streamoff inner_next =
          inner_payload_end +
          static_cast<std::streamoff>((8 - (inner.bytes % 8)) % 8);

        if (inner.type == miMATRIX) {
          add_matrix(g, inner_payload_start, inner_payload_end);
        }

        gpos = inner_next;
      }

      g.close();
      std::remove(tmp.c_str());

      pos = next_pos;
      continue;
    }

    pos = next_pos;
  }

  if (qsm.size() == 0) {
    Rcpp::stop("No TreeQSM 2.0 variables read");
  }

  qsm.attr("names") = qsm_names;
  return qsm;
}

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

