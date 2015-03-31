#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void setupHaskellCtrlCHandler(void);
extern HsInt tycon_hash(HsPtr a1);
extern HsPtr tycon_richcmp(HsPtr a1, HsPtr a2, HsInt a3);
extern HsPtr tycon_str(HsPtr a1);
extern HsPtr tycon_repr(HsPtr a1);
extern HsPtr tycon_getname(HsPtr a1, HsPtr a2);
extern HsPtr tycon_getmodule(HsPtr a1, HsPtr a2);
extern HsPtr tycon_getpackage(HsPtr a1, HsPtr a2);
extern HsPtr tycon_getarity(HsPtr a1, HsPtr a2);
extern HsPtr tycon_get_is_cls(HsPtr a1, HsPtr a2);
extern HsPtr tycon_getkind(HsPtr a1, HsPtr a2);
extern HsInt hstype_hash(HsPtr a1);
extern HsPtr hstype_richcmp(HsPtr a1, HsPtr a2, HsInt a3);
extern HsPtr hstype_str(HsPtr a1);
extern HsPtr hstype_repr(HsPtr a1);
extern HsPtr hstype_gettail(HsPtr a1, HsPtr a2);
extern HsPtr hstype_getname(HsPtr a1, HsPtr a2);
extern HsPtr hstype_getkind(HsPtr a1, HsPtr a2);
extern HsPtr hstype_getfvs(HsPtr a1, HsPtr a2);
extern HsPtr hstype_subst(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsPtr hstype_gethead(HsPtr a1, HsPtr a2);
extern HsPtr hsobjraw_gethstype(HsPtr a1, HsPtr a2);
extern HsPtr hsobjraw_narrow(HsPtr a1, HsPtr a2);
extern HsPtr hsobjraw_subst(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsInt hsobjraw_new(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsPtr tycon_call(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsInt hstype_new(HsPtr a1, HsPtr a2, HsPtr a3);
extern HsInt addSimpleHsTypeObjsToModule(HsPtr a1);
extern HsPtr from_haskell_Bool(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_Char(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_String(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_Text(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_ByteString(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_Int(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_Integer(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_Float(HsPtr a1, HsPtr a2);
extern HsPtr from_haskell_Double(HsPtr a1, HsPtr a2);
extern HsPtr buildHaskellBool(HsBool a1);
extern HsPtr buildHaskellChar(HsChar a1);
extern HsPtr buildHaskellString(HsPtr a1, HsInt a2);
extern HsPtr buildHaskellText(HsPtr a1, HsInt a2);
extern HsPtr buildHaskellByteString(HsPtr a1, HsInt a2);
extern HsPtr buildHaskellInt(HsInt a1);
extern HsPtr buildHaskellInteger(HsInt a1);
extern HsPtr buildHaskellIntegerStr(HsPtr a1, HsInt a2);
extern HsPtr buildHaskellFloat(HsFloat a1);
extern HsPtr buildHaskellDouble(HsDouble a1);
extern HsInt prepare_GHC_state(HsPtr a1);
extern HsInt close_GHC_state(HsPtr a1);
extern HsPtr hyphen_import_lib_core(HsPtr a1, HsPtr a2);
extern HsPtr hyphen_import_src_core(HsPtr a1, HsPtr a2);
extern HsPtr hyphen_access_basics_core(HsPtr a1, HsPtr a2);
extern HsPtr ok_python_identif(HsPtr a1, HsPtr a2);
extern HsPtr hyphen_apply(HsPtr a1, HsPtr a2);
extern HsPtr hyphen_doio(HsPtr a1, HsPtr a2);
extern HsPtr hyphen_wrap_pyfn_impl(HsPtr a1, HsPtr a2, HsInt a3);
#ifdef __cplusplus
}
#endif

