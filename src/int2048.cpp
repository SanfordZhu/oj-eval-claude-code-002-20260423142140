#include "include/int2048.h"
#include <algorithm>
#include <iomanip>

namespace sjtu {

int2048::int2048() : neg(false) {}
int2048::int2048(long long v) : neg(false) {
  if (v < 0) { neg = true; v = -v; }
  while (v) { a.push_back(static_cast<int>(v % BASE)); v /= BASE; }
}
int2048::int2048(const std::string &s) { read(s); }
int2048::int2048(const int2048 &o) : a(o.a), neg(o.neg) {}

void int2048::trim() {
  while (!a.empty() && a.back() == 0) a.pop_back();
  if (a.empty()) neg = false;
}

int int2048::abs_compare(const int2048 &x, const int2048 &y) {
  if (x.a.size() != y.a.size()) return x.a.size() < y.a.size() ? -1 : 1;
  for (int i = (int)x.a.size() - 1; i >= 0; --i) {
    if (x.a[i] != y.a[i]) return x.a[i] < y.a[i] ? -1 : 1;
  }
  return 0;
}

int2048 int2048::add_abs(const int2048 &x, const int2048 &y) {
  int2048 r; r.a.resize(std::max(x.a.size(), y.a.size()) + 1, 0); r.neg = false;
  long long carry = 0;
  size_t n = r.a.size();
  for (size_t i = 0; i < n; ++i) {
    long long s = carry;
    if (i < x.a.size()) s += x.a[i];
    if (i < y.a.size()) s += y.a[i];
    r.a[i] = static_cast<int>(s % BASE);
    carry = s / BASE;
  }
  r.trim();
  return r;
}

int2048 int2048::sub_abs(const int2048 &x, const int2048 &y) { // |x| >= |y|
  int2048 r; r.a.resize(x.a.size(), 0); r.neg = false;
  long long borrow = 0;
  for (size_t i = 0; i < x.a.size(); ++i) {
    long long s = x.a[i] - borrow - (i < y.a.size() ? y.a[i] : 0);
    if (s < 0) { s += BASE; borrow = 1; } else borrow = 0;
    r.a[i] = static_cast<int>(s);
  }
  r.trim();
  return r;
}

int2048 int2048::mul_int_abs(const int2048 &x, int m) {
  if (m == 0 || x.a.empty()) return int2048();
  int2048 r; r.a.resize(x.a.size() + 1, 0); r.neg = false;
  long long carry = 0;
  for (size_t i = 0; i < x.a.size(); ++i) {
    long long t = 1LL * x.a[i] * m + carry;
    r.a[i] = static_cast<int>(t % BASE);
    carry = t / BASE;
  }
  r.a[x.a.size()] = static_cast<int>(carry);
  r.trim();
  return r;
}

int2048 int2048::shl_base(const int2048 &x, int k) {
  if (x.a.empty()) return int2048();
  int2048 r; r.neg = false; r.a.assign(k, 0); r.a.insert(r.a.end(), x.a.begin(), x.a.end());
  return r;
}

int2048 int2048::mul_abs(const int2048 &x, const int2048 &y) {
  if (x.a.empty() || y.a.empty()) return int2048();
  int2048 r; r.neg = false; r.a.assign(x.a.size() + y.a.size(), 0);
  for (size_t i = 0; i < x.a.size(); ++i) {
    long long carry = 0;
    for (size_t j = 0; j < y.a.size() || carry; ++j) {
      long long cur = r.a[i + j] + 1LL * x.a[i] * (j < y.a.size() ? y.a[j] : 0) + carry;
      r.a[i + j] = static_cast<int>(cur % BASE);
      carry = cur / BASE;
    }
  }
  r.trim();
  return r;
}

void int2048::divmod_abs(const int2048 &x, const int2048 &y, int2048 &q, int2048 &r) {
  q = int2048(); r = int2048();
  if (y.a.empty()) return; // undefined by spec, not provided in tests
  if (abs_compare(x, y) < 0) { r = x; return; }

  // Normalize by BASE to estimate digits (schoolbook division)
  r.neg = false;
  for (int i = (int)x.a.size() - 1; i >= 0; --i) {
    r.a.insert(r.a.begin(), x.a[i]); r.trim();
    // binary search quotient digit 0..BASE-1
    int low = 0, high = BASE - 1, best = 0;
    while (low <= high) {
      int mid = (low + high) >> 1;
      int2048 t = mul_int_abs(y, mid);
      if (abs_compare(t, r) <= 0) { best = mid; low = mid + 1; } else { high = mid - 1; }
    }
    q.a.insert(q.a.begin(), best);
    r = sub_abs(r, mul_int_abs(y, best));
  }
  q.trim(); r.trim();
}

void int2048::read(const std::string &s) {
  a.clear(); neg = false;
  size_t i = 0; while (i < s.size() && isspace(static_cast<unsigned char>(s[i]))) ++i;
  if (i < s.size() && (s[i] == '+' || s[i] == '-')) { neg = (s[i] == '-'); ++i; }
  while (i < s.size() && s[i] == '0') ++i;
  std::string digits;
  while (i < s.size() && isdigit(static_cast<unsigned char>(s[i]))) { digits.push_back(s[i]); ++i; }
  for (int p = (int)digits.length(); p > 0; p -= WIDTH) {
    int len = std::max(0, p - WIDTH);
    int x = std::stoi(digits.substr(len, p - len));
    a.push_back(x);
  }
  trim();
}

void int2048::print() {
  const std::vector<int> *pa = &a; bool sneg = neg;
  if (pa->empty()) { std::cout << 0; return; }
  if (sneg) std::cout << '-';
  int n = (int)pa->size();
  std::cout << pa->back();
  for (int i = n - 2; i >= 0; --i) {
    std::cout << std::setw(WIDTH) << std::setfill('0') << (*pa)[i];
  }
}

int2048 &int2048::add(int2048 b) {
  if (neg == b.neg) {
    bool sign = neg;
    *this = add_abs(*this, b);
    this->neg = sign;
  } else {
    int cmp = abs_compare(*this, b);
    if (cmp >= 0) {
      bool sign = neg;
      *this = sub_abs(*this, b);
      this->neg = sign;
    } else {
      *this = sub_abs(b, *this);
      this->neg = b.neg;
    }
  }
  trim();
  return *this;
}

int2048 add(int2048 a, const int2048 &b) { a.add(b); return a; }

int2048 &int2048::minus(int2048 b) {
  if (neg != b.neg) {
    bool sign = neg;
    *this = add_abs(*this, b);
    this->neg = sign;
  } else {
    int cmp = abs_compare(*this, b);
    if (cmp >= 0) {
      bool sign = neg;
      *this = sub_abs(*this, b);
      this->neg = sign;
    } else {
      *this = sub_abs(b, *this);
      this->neg = !b.neg;
    }
  }
  trim();
  return *this;
}

int2048 minus(int2048 a, const int2048 &b) { a.minus(b); return a; }

int2048 int2048::operator+() const { return *this; }
int2048 int2048::operator-() const { int2048 r(*this); if (!r.a.empty()) r.neg = !r.neg; return r; }

int2048 &int2048::operator=(const int2048 &o) { a = o.a; neg = o.neg; return *this; }

int2048 &int2048::operator+=(const int2048 &b) { return add(b); }
int2048 operator+(int2048 a, const int2048 &b) { return add(a, b); }

int2048 &int2048::operator-=(const int2048 &b) { return minus(b); }
int2048 operator-(int2048 a, const int2048 &b) { return minus(a, b); }

int2048 &int2048::operator*=(const int2048 &b) {
  bool sign = neg ^ b.neg;
  *this = mul_abs(*this, b); this->neg = sign; trim(); return *this;
}
int2048 operator*(int2048 a, const int2048 &b) { a *= b; return a; }

int2048 &int2048::operator/=(const int2048 &b) {
  int2048 q, r; divmod_abs(*this, b, q, r);
  bool sign = neg ^ b.neg;
  // floor division toward -inf
  if (!r.a.empty() && (neg && !b.neg)) { // negative/positive, residual -> round down
    q = add_abs(q, int2048(1));
  }
  if (!r.a.empty() && (!neg && b.neg)) { // positive/negative
    q = add_abs(q, int2048(1));
  }
  q.neg = sign; q.trim(); *this = q; return *this;
}
int2048 operator/(int2048 a, const int2048 &b) { a /= b; return a; }

int2048 &int2048::operator%=(const int2048 &b) {
  int2048 q, r; divmod_abs(*this, b, q, r);
  // r = x - floor(x/y)*y
  int2048 fq = q;
  bool sign = neg ^ b.neg;
  if (!r.a.empty() && (neg && !b.neg)) { fq = add_abs(fq, int2048(1)); }
  if (!r.a.empty() && (!neg && b.neg)) { fq = add_abs(fq, int2048(1)); }
  // compute r = x - fq*y
  int2048 prod = mul_abs(fq, b); prod.neg = sign;
  int cmp = abs_compare(*this, prod);
  if (cmp >= 0) { *this = sub_abs(*this, prod); this->neg = neg; }
  else { *this = sub_abs(prod, *this); this->neg = !neg; }
  trim(); return *this;
}
int2048 operator%(int2048 a, const int2048 &b) { a %= b; return a; }

std::istream &operator>>(std::istream &is, int2048 &x) { std::string s; is >> s; x.read(s); return is; }
std::ostream &operator<<(std::ostream &os, const int2048 &x) { if (x.a.empty()) { os << 0; return os; } if (x.neg) os << '-'; os << x.a.back(); for (int i = (int)x.a.size()-2; i >= 0; --i) os << std::setw(int2048::WIDTH) << std::setfill('0') << x.a[i]; return os; }

bool operator==(const int2048 &x, const int2048 &y) { return x.neg == y.neg && x.a == y.a; }
bool operator!=(const int2048 &x, const int2048 &y) { return !(x == y); }
bool operator<(const int2048 &x, const int2048 &y) {
  if (x.neg != y.neg) return x.neg;
  int cmp = int2048::abs_compare(x, y);
  return x.neg ? (cmp > 0) : (cmp < 0);
}
bool operator>(const int2048 &x, const int2048 &y) { return y < x; }
bool operator<=(const int2048 &x, const int2048 &y) { return !(y < x); }
bool operator>=(const int2048 &x, const int2048 &y) { return !(x < y); }

} // namespace sjtu
