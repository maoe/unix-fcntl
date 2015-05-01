#define hsc_DEFINE_PATTERN(pat, ty) \
  hsc_printf(                       \
    "pattern %s :: %s\n"            \
    "pattern %s = %s ",             \
    pat, ty, pat, ty                \
  );                                \
  hsc_const(pat);

#define hsc_DEFINE_BIDIRECTIONAL_PATTERN(pat, ty)          \
  hsc_printf(                                              \
    "pattern %s :: %s\n"                                   \
    "pattern %s <- ((\\(%s n) -> n .&. _%s > 0) -> True)\n" \
    "  where\n"                                            \
    "    %s = %s _%s\n"                                    \
    "_%s :: CInt\n"                                        \
    "_%s = ",                                              \
    pat, ty, pat, ty, pat, pat, ty, pat, pat, pat          \
  );                                                       \
  hsc_const(pat);
