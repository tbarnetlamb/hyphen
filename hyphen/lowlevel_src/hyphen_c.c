#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdio.h>
#include <HsFFI.h>

#include "Hyphen_stub.h"

extern void __stginit_Hyphen ( void );

#include "structmember.h"

/* --------------------------------------------------------------------- */

static PyObject *HsException;
static HsPtr ghc_interpreter_state = 0;
static HsBool ghc_srcmodules_loaded = 0;

HsPtr
pythonateInt(HsInt i)
{
  /* HsInt is 32 or 64 bit to match sizeof(void*), without regard to the
     definition of c 'long'*/
#if defined(SIZEOF_LONG_LONG) && SIZEOF_LONG != SIZEOF_VOID_P && SIZEOF_LONG_LONG == SIZEOF_VOID_P
  return PyLong_FromLongLong(i);
#else
  return PyLong_FromLong(i);
#endif
}

HsPtr
pythonateFloat(HsFloat i)
{
  return PyFloat_FromDouble(i);
}

HsPtr
pythonateDouble(HsDouble i)
{
  return PyFloat_FromDouble(i);
}

HsPtr
pythonateUTF16Ptr(const Py_UCS2* s, HsInt size)
{
  return PyUnicode_DecodeUTF16((char*) s, size*2, NULL, NULL);
}

HsPtr
pythonateBytePtr(const char* s, HsInt size)
{
  return PyBytes_FromStringAndSize(s, size);
}

HsPtr
pythonateTrue()
{
  Py_RETURN_TRUE;
}

HsPtr
pythonateFalse()
{
  Py_RETURN_FALSE;
}

HsPtr
pythonateIntegerFromStr(const Py_UCS2* s, HsInt size)
{
  PyObject *str, *num;
  str = PyUnicode_DecodeUTF16((char*) s, size*2, NULL, NULL);
  if (!str)
    {
      return 0;
    }
  
  num = PyLong_FromUnicodeObject(str, 16);
  if (!num)
    {
      Py_DECREF(str);
      return 0;
    }
  return num;
}

HsPtr
c_getHsExceptionAttr(HsPtr obj)
{
  PyObject *ret, *ty, *val, *tb;
  ret = PyObject_GetAttrString(obj, "hs_exception");
  if (PyErr_Occurred()) { /* eat any exception */
    PyErr_Fetch(&ty, &val, &tb);
    Py_XDECREF(ty);
    Py_XDECREF(val);
    Py_XDECREF(tb);
  }
  return ret;
}

HsPtr
c_pyTypeErr(const char *str)
{
  PyErr_SetString(PyExc_TypeError, str);
  return 0;
}

HsPtr
c_pyValueErr(const char *str)
{
  PyErr_SetString(PyExc_ValueError, str);
  return 0;
}

HsPtr
pyErr_NoMemory()
{
  PyErr_NoMemory();
  return 0;
}

HsInt
pyErr_CheckSignals()
{
  return PyErr_CheckSignals();
}

void
pyErr_Fetch(HsPtr ptype, HsPtr pvalue, HsPtr ptraceback)
{
  return PyErr_Fetch(ptype, pvalue, ptraceback);
}

void
pyErr_NormalizeException(HsPtr ptype, HsPtr pvalue, HsPtr ptraceback)
{
  return PyErr_NormalizeException(ptype, pvalue, ptraceback);
}

void
pyErr_Restore(HsPtr type, HsPtr value, HsPtr traceback)
{
  return PyErr_Restore(type, value, traceback);
}

void
pyErr_SetObject(HsPtr type, HsPtr value)
{
  return PyErr_SetObject(type, value);
}

HsPtr
pyTuple_New(HsInt sz)
{
  return PyTuple_New(sz);
}

HsPtr
pyTuple_GET_ITEM(HsPtr a, HsInt i)
{
  return PyTuple_GET_ITEM(a, i);
}

void
pyTuple_SET_ITEM(HsPtr a, HsInt i, HsPtr v)
{
  PyTuple_SET_ITEM(a, i, v);
}

HsInt
pyTuple_Size(HsPtr p)
{
  return PyTuple_Size(p);
}

HsBool
pyUnicode_Check(HsPtr obj)
{
  return PyUnicode_Check(obj);
}

HsBool
pyCallable_Check(HsPtr obj)
{
  return PyCallable_Check(obj);
}

HsInt
pyObject_SetAttr(HsPtr obj, HsPtr attr_name, HsPtr v)
{
  return PyObject_SetAttr(obj, attr_name, v);
}

HsPtr
pyObject_Call(HsPtr obj, HsPtr args, HsPtr kwargs)
{
  return PyObject_Call(obj, args, kwargs);
}

HsPtr
pyObject_Str(HsPtr obj)
{
  return PyObject_Str(obj);
}

HsPtr
pyDict_New()
{
  return PyDict_New();
}

HsInt
pyDict_Next(HsPtr dict, HsPtr pos, HsPtr key, HsPtr val)
{
  return PyDict_Next(dict, pos, key, val);
}


HsInt
pyDict_SetItem(HsPtr dict, HsPtr key, HsPtr val)
{
  return PyDict_SetItem(dict, key, val);
}

void
py_DECREF(HsPtr obj)
{
  Py_DECREF(obj);
}

void
py_INCREF(HsPtr obj)
{
  Py_INCREF(obj);
}

void
pyModule_AddObject(HsPtr module, const char *name, HsPtr value)
{
  PyModule_AddObject(module, name, value);
}

HsPtr
pyGILState_Ensure()
{
  PyGILState_STATE *state = malloc(sizeof(PyGILState_STATE));
  if (state) {
    *state = PyGILState_Ensure();
    return state;
  } else {
    return PyErr_NoMemory();
  }
  return state;
}

void
pyGILState_Release(HsPtr state)
{
  PyGILState_Release(*(PyGILState_STATE*) state);
  free(state);
}

HsPtr
pyEval_SaveThread()
{
  return PyEval_SaveThread();
}

void
pyEval_RestoreThread(HsPtr state)
{
  PyEval_RestoreThread(state);
}

HsPtr
exHsException()
{
  return HsException;
}

HsPtr
exKeyboardInterrupt()
{
  return PyExc_KeyboardInterrupt;
}

HsPtr
exOverflowError()
{
  return PyExc_OverflowError;
}

HsPtr
exZeroDivisionError()
{
  return PyExc_ZeroDivisionError;
}

HsPtr
exFloatingPointError()
{
  return PyExc_FloatingPointError;
}

HsPtr
exAttributeError()
{
  return PyExc_AttributeError;
}

HsPtr
exSystemExit()
{
  return PyExc_SystemExit;
}

HsPtr
exEOFError()
{
  return PyExc_EOFError;
}

HsPtr
py_NotImplemented()
{
  Py_RETURN_NOTIMPLEMENTED;
}

HsPtr
py_None()
{
  Py_RETURN_NONE;
}


/* --------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    void *stablePtr;
} TyCon;

static void
TyCon_dealloc(TyCon* self)
{
  if (ghc_interpreter_state) /* Don't free after module shutdown */
    hs_free_stable_ptr(self->stablePtr);
  Py_TYPE(self)->tp_free((PyObject*)self);
}

static Py_hash_t
TyCon_hash(PyObject *self)
{
  long foo = tycon_hash(self);
  if (foo==-1)
    foo = -2;
  return foo;
}

static PyObject *
TyCon_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyErr_SetString(PyExc_TypeError, "Cannot construct new TyCon objects from python.");
  return NULL;
}

static int
TyCon_init(TyCon *self, PyObject *args, PyObject *kwds)
{
  PyErr_SetString(PyExc_TypeError, "Cannot construct new TyCon objects from python.");
  return -1;
}

static PyObject *
TyCon_call(PyObject *self, PyObject *args, PyObject *kwargs)
{
  return tycon_call(self, args, kwargs);
}

static PyMemberDef TyCon_members[] = {
  {NULL}  /* Sentinel */
};

static PyMethodDef TyCon_methods[] = {
  {NULL}  /* Sentinel */
};

static int
unsettable(PyObject *self, PyObject *value, void *closure)
{
  PyErr_SetString(PyExc_TypeError, "Read only member variable.");
}

static PyGetSetDef TyCon_getsetters[] = {
  {"name",    (getter)tycon_getname,    unsettable,
               "The type constructor name.",                             NULL},
  {"module",  (getter)tycon_getmodule,  unsettable,
               "The type constructor module.",                           NULL},
  {"package", (getter)tycon_getpackage, unsettable,
               "The type constructor package.",                          NULL},
  {"arity",   (getter)tycon_getarity,   unsettable,
               "The type constructor arity.",                            NULL},
  {"is_cls",  (getter)tycon_get_is_cls, unsettable,
               "Is this the type constructor of a class instance dict?", NULL},
  {"visible_module",
               (getter)tycon_get_visible_module,  unsettable,
       "A preferred module in which the type constructor is visible, or None.",
                                                                         NULL},
  {"kind",    (getter)tycon_getkind,    unsettable,
               "The type constructor kind.",                             NULL},
  {NULL}  /* Sentinel */
};

static PyTypeObject TyConType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "hyphen.TyCon",                /* tp_name */
  sizeof(TyCon),             /* tp_basicsize */
  0,                         /* tp_itemsize */
  (destructor)TyCon_dealloc, /* tp_dealloc */
  0,                         /* tp_print */
  0,                         /* tp_getattr */
  0,                         /* tp_setattr */
  0,                         /* tp_reserved */
  (reprfunc)tycon_repr,      /* tp_repr */
  0,                         /* tp_as_number */
  0,                         /* tp_as_sequence */
  0,                         /* tp_as_mapping */
  TyCon_hash,                /* tp_hash */
  (ternaryfunc)tycon_call,   /* tp_call */
  (reprfunc)tycon_str,       /* tp_str */
  0,                         /* tp_getattro */
  0,                         /* tp_setattro */
  0,                         /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT |
  Py_TPFLAGS_BASETYPE,       /* tp_flags */
  "TyCon objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  (richcmpfunc)tycon_richcmp, /* tp_richcompare*/
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  TyCon_methods,             /* tp_methods */
  TyCon_members,             /* tp_members */
  TyCon_getsetters,          /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)TyCon_init,      /* tp_init */
  0,                         /* tp_alloc */
  TyCon_new,                 /* tp_new */
};

HsPtr
c_unwrapPythonTyCon(HsPtr self)
{
  return ((TyCon*) self)->stablePtr;
}

HsPtr
c_wrapPythonTyCon(HsPtr stablePtr)
{
  TyCon *self;

  self = (TyCon *)TyConType.tp_alloc(&TyConType, 0);
  if (self != NULL) {
    self->stablePtr = stablePtr;
  } else {
    hs_free_stable_ptr(stablePtr);
  }

  return (PyObject *)self;
}

HsBool
pyTyCon_Check(HsPtr obj)
{
  return PyObject_IsInstance(obj, (PyObject*) &TyConType);
}

/* --------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    void *stablePtr;
} HsType;

static void
HsType_dealloc(HsType* self)
{
  if (ghc_interpreter_state) /* Don't free after module shutdown */
    hs_free_stable_ptr(self->stablePtr);
  Py_TYPE(self)->tp_free((PyObject*)self);
}

static Py_hash_t
HsType_hash(PyObject *self)
{
  long foo = hstype_hash(self);
  if (foo==-1)
    foo = -2;
  return foo;
}

static PyObject *
HsType_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  HsType *self;
  HsPtr stablePtr = 0;
  int   retval    = hstype_new(args, kwds, &stablePtr);
  if (retval < 0)
    return 0;

  self = (HsType*) type->tp_alloc(type, 0);
  if (self != NULL) {
    self->stablePtr = stablePtr;
  } else {
    hs_free_stable_ptr(stablePtr);
  }

  return (PyObject *)self;
}

static int
HsType_init(HsType *self, PyObject *args, PyObject *kwds)
{
  return 0;
}

static PyMemberDef HsType_members[] = {
  {NULL}  /* Sentinel */
};

static PyMethodDef HsType_methods[] = {
  {"subst",(PyCFunction) hstype_subst, METH_KEYWORDS | METH_VARARGS, 
                                 "Substitute for the free variables in the type."},
  {NULL}  /* Sentinel */
};

static PyGetSetDef HsType_getsetters[] = {
  {"name", (getter) hstype_getname, unsettable, "The name of the type.",           NULL},
  {"head", (getter) hstype_gethead, unsettable, "The head of the type.",           NULL},
  {"tail", (getter) hstype_gettail, unsettable, "The tail of the type.",           NULL},
  {"kind", (getter) hstype_getkind, unsettable, "The kind of the type.",           NULL},
  {"fvs",  (getter) hstype_getfvs,  unsettable, "The free variables in the type.", NULL},
  {NULL}  /* Sentinel */
};

static PyTypeObject HsTypeType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "hyphen.HsType",               /* tp_name */
  sizeof(HsType),            /* tp_basicsize */
  0,                         /* tp_itemsize */
  (destructor)HsType_dealloc,/* tp_dealloc */
  0,                         /* tp_print */
  0,                         /* tp_getattr */
  0,                         /* tp_setattr */
  0,                         /* tp_reserved */
  (reprfunc)hstype_repr,     /* tp_repr */
  0,                         /* tp_as_number */
  0,                         /* tp_as_sequence */
  0,                         /* tp_as_mapping */
  HsType_hash,               /* tp_hash */
  0,                         /* tp_call */
  (reprfunc)hstype_str,      /* tp_str */
  0,                         /* tp_getattro */
  0,                         /* tp_setattro */
  0,                         /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT |
  Py_TPFLAGS_BASETYPE,       /* tp_flags */
  "HsType objects",          /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  (richcmpfunc)hstype_richcmp, /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  HsType_methods,            /* tp_methods */
  HsType_members,            /* tp_members */
  HsType_getsetters,         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)HsType_init,     /* tp_init */
  0,                         /* tp_alloc */
  HsType_new,                /* tp_new */
};

HsPtr
c_unwrapPythonHsType(HsPtr self)
{
  return ((TyCon*) self)->stablePtr;
}

HsPtr
c_wrapPythonHsType(HsPtr stablePtr)
{
  HsType *self;

  self = (HsType *)HsTypeType.tp_alloc(&HsTypeType, 0);
  if (self != NULL) {
    self->stablePtr = stablePtr;
  } else {
    hs_free_stable_ptr(stablePtr);
  }

  return (PyObject *)self;
}

HsPtr
parseTupleToPythonHsType(HsPtr args)
{
  PyObject *obj=NULL;
  if (!PyArg_ParseTuple((PyObject*) args, "O!", &HsTypeType, &obj))
    {
      return NULL;
    }
  return obj;
}

HsBool
pyHsType_Check(HsPtr obj)
{
  return PyObject_IsInstance(obj, (PyObject*) &HsTypeType);
}

/* --------------------------------------------------------------------- */


typedef struct {
    PyObject_HEAD
    void   *objStablePtr;
} HsObjRaw;

static void
HsObjRaw_dealloc(HsObjRaw* self)
{
  if (ghc_interpreter_state) /* Don't free after module shutdown */
    hs_free_stable_ptr(self->objStablePtr);
  Py_TYPE(self)->tp_free((PyObject*)self);
}

static PyObject *
HsObjRaw_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  HsObjRaw *self;
  HsPtr     objStablePtr = 0;
  int       retval = hsobjraw_new(args, kwds, &objStablePtr);
  if (retval < 0)
    return 0;

  self = (HsObjRaw *) type->tp_alloc(type, 0);
  if (self != NULL) {
    self->objStablePtr = objStablePtr;
  } else {
    hs_free_stable_ptr(objStablePtr);
  }

  return (PyObject *)self;
}

static int
HsObjRaw_init(HsObjRaw *self, PyObject *args, PyObject *kwds)
{
  return 0;
}

static PyMemberDef HsObjRaw_members[] = {
  {NULL}  /* Sentinel */
};

static PyMethodDef HsObjRaw_methods[] = {
  {"narrow_type", (PyCFunction)hsobjraw_narrow, METH_VARARGS,  "Attempt to narrow the type to the type supplied."},
  {"subst",       (PyCFunction)hsobjraw_subst,  METH_KEYWORDS | METH_VARARGS, "Substitute for the free variables in the type."},
  {NULL}  /* Sentinel */
};

static PyGetSetDef HsObjRaw_getsetters[] = {
  {"hstype",      (getter)hsobjraw_gethstype, unsettable,
                                                     "Object's Haskell type.", NULL},
  {NULL}  /* Sentinel */
};

static PyTypeObject HsObjRawType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "hsobjraw.HsObjRaw",          /* tp_name */
  sizeof(HsObjRaw),             /* tp_basicsize */
  0,                            /* tp_itemsize */
  (destructor)HsObjRaw_dealloc, /* tp_dealloc */
  0,                            /* tp_print */
  0,                            /* tp_getattr */
  0,                            /* tp_setattr */
  0,                            /* tp_reserved */
  0,                            /* tp_repr */
  0,                            /* tp_as_number */
  0,                            /* tp_as_sequence */
  0,                            /* tp_as_mapping */
  PyObject_HashNotImplemented,  /* tp_hash  */
  0,                            /* tp_call */
  0,                            /* tp_str */
  0,                            /* tp_getattro */
  0,                            /* tp_setattro */
  0,                            /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT |
  Py_TPFLAGS_BASETYPE,          /* tp_flags */
  "HsObjRaw objects",           /* tp_doc */
  0,                            /* tp_traverse */
  0,                            /* tp_clear */
  0,                            /* tp_richcompare */
  0,                            /* tp_weaklistoffset */
  0,                            /* tp_iter */
  0,                            /* tp_iternext */
  HsObjRaw_methods,             /* tp_methods */
  HsObjRaw_members,             /* tp_members */
  HsObjRaw_getsetters,          /* tp_getset */
  0,                            /* tp_base */
  0,                            /* tp_dict */
  0,                            /* tp_descr_get */
  0,                            /* tp_descr_set */
  0,                            /* tp_dictoffset */
  (initproc)HsObjRaw_init,      /* tp_init */
  0,                            /* tp_alloc */
  HsObjRaw_new,                 /* tp_new */
};

HsPtr
c_wrapPythonHsObjRaw(HsPtr objStablePtr)
{
  HsObjRaw *self;

  self = (HsObjRaw *)HsObjRawType.tp_alloc(&HsObjRawType, 0);
  if (self != NULL) {
    self->objStablePtr = objStablePtr;
  } else {
    hs_free_stable_ptr(objStablePtr);
  }

  return (PyObject *)self;
}

HsPtr
c_unwrapPythonHsObjRaw(HsPtr self)
{
  return ((HsObjRaw*) self)->objStablePtr;
}

HsPtr
parseTupleToPythonHsObjRaw(HsPtr args)
{
  PyObject *obj=NULL;
  if (!PyArg_ParseTuple((PyObject*) args, "O!", &HsObjRawType, &obj))
    {
      return NULL;
    }
  return obj;
}

HsBool
pyHsObjRaw_Check(HsPtr obj)
{
  return PyObject_IsInstance(obj, (PyObject*) &HsObjRawType);
}

/* --------------------------------------------------------------------- */

static PyObject *
hyphen_wrap_pyfn(PyObject *self, PyObject *args)
{
  PyObject *fn, *ty;
  int arity=-1;
  if (!PyArg_ParseTuple(args, "OO!|i:wrap_pyfn", &fn, &HsTypeType, &ty, &arity))
    return NULL;

  return hyphen_wrap_pyfn_impl(fn, ty, arity);
}

static PyObject *
to_haskell_Bool(PyObject *self, PyObject *args)
{
  int b;
  if (!PyArg_ParseTuple(args, "p:to_haskell_Bool", &b))
    return NULL;

  return buildHaskellBool(b);
}

static PyObject *
to_haskell_Char(PyObject *self, PyObject *args)
{
  int dword;
  if (!PyArg_ParseTuple(args, "C:to_haskell_Char", &dword))
    return NULL;

  return buildHaskellChar(dword);
}

static PyObject *
to_haskell_String(PyObject *self, PyObject *args)
{
  char        *buffer=0;
  Py_ssize_t   buffer_length=-1;
  if (!PyArg_ParseTuple(args, "es#:to_haskell_String", "utf-16", &buffer, &buffer_length))
    {
      return NULL;
    }

  /* Ingore first two bytes of encoded string; they are the BOM. Also
     convert length from bytes to UTF-16 words. */
  PyObject *ret = buildHaskellString((HsPtr) (buffer+2), (buffer_length-2)/2);
  PyMem_Free(buffer);
  return ret;
}

static PyObject *
to_haskell_Text(PyObject *self, PyObject *args)
{
  char       *buffer=0;
  Py_ssize_t  buffer_length=-1;
  if (!PyArg_ParseTuple(args, "es#:to_haskell_Text", "utf-16", &buffer, &buffer_length))
    {
      return NULL;
    }

  /* Ingore first two bytes of encoded string; they are the BOM. Also
     convert length from bytes to UTF-16 words. */
  PyObject *ret = buildHaskellText((HsPtr) (buffer+2), (buffer_length-2)/2);
  PyMem_Free(buffer);
  return ret;
}

HsPtr
c_makeHaskellText(HsPtr str)
{
  PyObject *argtuple = PyTuple_Pack(1, str);
  if (!argtuple) {
    return NULL;
  }
  PyObject *answer = to_haskell_Text(NULL, argtuple);
  Py_DECREF(argtuple);
  return answer;
}

static PyObject *
to_haskell_ByteString(PyObject *self, PyObject *args)
{
  char       *buffer=0;
  Py_ssize_t  buffer_length=-1;
  if (!PyArg_ParseTuple(args, "y#:to_haskell_ByteString", &buffer, &buffer_length))
    {
      return NULL;
    }

  return buildHaskellByteString(buffer, buffer_length);
}

static PyObject *
to_haskell_Int(PyObject *self, PyObject *args)
{
#if defined(SIZEOF_LONG_LONG) && SIZEOF_LONG != SIZEOF_VOID_P && SIZEOF_LONG_LONG == SIZEOF_VOID_P
  long i;
  if (!PyArg_ParseTuple(args, "l:to_haskell_Int", &i))
    return NULL;
#else
  long long i;
  if (!PyArg_ParseTuple(args, "L:to_haskell_Int", &i))
    return NULL;
#endif

  return buildHaskellInt(i);
}

static PyObject *
to_haskell_Integer(PyObject *self, PyObject *args)
{
  PyObject *int_obj;
  if (!PyArg_ParseTuple(args, "O!:to_haskell_Integer", &PyLong_Type, &int_obj))
    return NULL;

  int overflow=0;
  long as_long = PyLong_AsLongAndOverflow(int_obj, &overflow);
  if (PyErr_Occurred())
    {      
      return NULL;
    }
  else if ((!overflow) && as_long <= HS_INT_MAX && as_long >= HS_INT_MIN)
    {
      return buildHaskellInteger((HsInt) as_long);
    }
  else
    {
      PyObject *hex_rep_string = PyNumber_ToBase(int_obj, 16);
      if (!hex_rep_string)
	{
	  return NULL;
	}
      Py_ssize_t buffer_length=-1;
      char *buffer = PyUnicode_AsUTF8AndSize(hex_rep_string, &buffer_length);
      if (!buffer || (buffer_length==-1))
	{
	  Py_DECREF(hex_rep_string);
	  return NULL;
	}

      PyObject *ret = buildHaskellIntegerStr(buffer, (int) buffer_length);
      Py_DECREF(hex_rep_string);
      return ret;
    }
}

static PyObject *
to_haskell_Float(PyObject *self, PyObject *args)
{
  float b;
  if (!PyArg_ParseTuple(args, "f:to_haskell_Float", &b))
    return NULL;

  return buildHaskellFloat(b);
}

static PyObject *
to_haskell_Double(PyObject *self, PyObject *args)
{
  double b;
  if (!PyArg_ParseTuple(args, "d:to_haskell_Double", &b))
    return NULL;

  return buildHaskellDouble(b);
}

static PyObject *
hyphen_import_lib(PyObject *self, PyObject *args)
{
  return hyphen_import_lib_core(ghc_interpreter_state, args);
}

static PyObject *
hyphen_access_basics(PyObject *self, PyObject *args)
{
  return hyphen_access_basics_core(ghc_interpreter_state, args);
}

static PyObject *
hyphen_import_src(PyObject *self, PyObject *args)
{
  if (ghc_srcmodules_loaded)
    {
      PyErr_SetString(PyExc_TypeError, "Can only load Haskell source modules once.");
    }
  else
    {
      PyObject *retval = hyphen_import_src_core(ghc_interpreter_state, args);
      if (retval)
	ghc_srcmodules_loaded = 1;
      return retval;
    }
}

/* --------------------------------------------------------------------- */

static PyOS_sighandler_t haskell_siginthandler = 0;
static PyOS_sighandler_t python_siginthandler  = 0;
static volatile int      signal_count          = 0;

static void
compound_sigint_handler(int signum)
{
  ++signal_count;
  if (haskell_siginthandler)
    {
      (*haskell_siginthandler)(signum);
    }
}

HsInt
c_installHaskellCtrlCHandler()
{
#if !defined(mingw32_HOST_OS)
  if (PyOS_getsig(SIGINT) == python_siginthandler     && haskell_siginthandler)
    {
      PyOS_setsig(SIGINT, &compound_sigint_handler);
    }
#endif
  return signal_count;
}

HsInt
c_reinstallPythonCtrlCHandler()
{
#if !defined(mingw32_HOST_OS)
  if (PyOS_getsig(SIGINT) == &compound_sigint_handler && python_siginthandler)
    {
      PyOS_setsig(SIGINT, python_siginthandler);
    }
#endif
  return signal_count;
}

/* --------------------------------------------------------------------- */

/* List of functions defined in the module */

PyDoc_STRVAR(module_doc,
	     "Hyphen bridge; haskell functions from python.");

static PyMethodDef HyphenMethods[] = {
    {"apply",                   (PyCFunction)hyphen_apply,            METH_VARARGS, PyDoc_STR("Apply object to other object.")},
    {"doio",                    (PyCFunction)hyphen_doio,             METH_VARARGS, PyDoc_STR("Perform an IO action.")},
    {"wrap_pyfn",               (PyCFunction)hyphen_wrap_pyfn,        METH_VARARGS, PyDoc_STR("Wrap a python function as a Haskell object")},
    {"import_lib",              (PyCFunction)hyphen_import_lib,       METH_VARARGS, PyDoc_STR("Import a Haskell Module from libraries")},
    {"import_src",              (PyCFunction)hyphen_import_src,       METH_VARARGS, PyDoc_STR("Import a Modules from source")},
    {"ok_python_identif",       (PyCFunction)ok_python_identif,       METH_VARARGS, PyDoc_STR("Check if a string is a valid python identifier")},
    {"access_basics",           (PyCFunction)hyphen_access_basics,    METH_VARARGS, PyDoc_STR("Provide access to basic functions built in to compiler.")},
    {"from_haskell_Bool",       (PyCFunction)from_haskell_Bool,       METH_VARARGS, PyDoc_STR("Convert Haskell Bool to python.")},
    {"from_haskell_Char",       (PyCFunction)from_haskell_Char,       METH_VARARGS, PyDoc_STR("Convert Haskell Char to python.")},
    {"from_haskell_String",     (PyCFunction)from_haskell_String,     METH_VARARGS, PyDoc_STR("Convert Haskell String to python.")},
    {"from_haskell_Text",       (PyCFunction)from_haskell_Text,       METH_VARARGS, PyDoc_STR("Convert Haskell Text to python.")},
    {"from_haskell_ByteString", (PyCFunction)from_haskell_ByteString, METH_VARARGS, PyDoc_STR("Convert Haskell ByteString to python.")},
    {"from_haskell_Int",        (PyCFunction)from_haskell_Int,        METH_VARARGS, PyDoc_STR("Convert Haskell Int to python.")},
    {"from_haskell_Integer",    (PyCFunction)from_haskell_Integer,    METH_VARARGS, PyDoc_STR("Convert Haskell Integer to python.")},
    {"from_haskell_Float",      (PyCFunction)from_haskell_Float,      METH_VARARGS, PyDoc_STR("Convert Haskell Float to python.")},
    {"from_haskell_Double",     (PyCFunction)from_haskell_Double,     METH_VARARGS, PyDoc_STR("Convert Haskell Double to python.")},
    {"to_haskell_Bool",         (PyCFunction)to_haskell_Bool,         METH_VARARGS, PyDoc_STR("Convert from python to Haskell Bool.")},
    {"to_haskell_Char",         (PyCFunction)to_haskell_Char,         METH_VARARGS, PyDoc_STR("Convert from python to Haskell Char.")},
    {"to_haskell_String",       (PyCFunction)to_haskell_String,       METH_VARARGS, PyDoc_STR("Convert from python to Haskell String.")},
    {"to_haskell_Text",         (PyCFunction)to_haskell_Text,         METH_VARARGS, PyDoc_STR("Convert from python to Haskell Text.")},
    {"to_haskell_ByteString",   (PyCFunction)to_haskell_ByteString,   METH_VARARGS, PyDoc_STR("Convert from python to Haskell ByteString.")},
    {"to_haskell_Int",          (PyCFunction)to_haskell_Int,          METH_VARARGS, PyDoc_STR("Convert from python to Haskell Int.")},
    {"to_haskell_Integer",      (PyCFunction)to_haskell_Integer,      METH_VARARGS, PyDoc_STR("Convert from python to Haskell Integer.")},
    {"to_haskell_Float",        (PyCFunction)to_haskell_Float,        METH_VARARGS, PyDoc_STR("Convert from python to Haskell Float.")},
    {"to_haskell_Double",       (PyCFunction)to_haskell_Double,       METH_VARARGS, PyDoc_STR("Convert from python to Haskell Double.")},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static void pyhs_free(void *to_free)
{
  if (ghc_interpreter_state)
    {
      /*close_GHC_state(ghc_interpreter_state)*/
      hs_free_stable_ptr(ghc_interpreter_state);
    }
  ghc_interpreter_state = 0;
#if !defined(mingw32_HOST_OS)
  PyOS_sighandler_t sigint_handler = PyOS_getsig(SIGINT);
#endif
  hs_exit();
#if !defined(mingw32_HOST_OS)
  PyOS_setsig(SIGINT, sigint_handler);
#endif
  PyObject_Del(to_free);
}

static struct PyModuleDef hyphenmodule = {
  PyModuleDef_HEAD_INIT,
  "hs",       /* name of module */
  module_doc, /* module documentation, may be NULL */
  -1,         /* size of per-interpreter state of the module,
	         or -1 if the module keeps state in global variables. */
  HyphenMethods,
  0,  /* m_reload */
  0,  /* m_traverse */
  0,  /* m_clear */
  pyhs_free,  /* m_free */
};

PyMODINIT_FUNC
PyInit_hslowlevel(void)
{
  PyObject *m;

  m = PyModule_Create(&hyphenmodule);
  if (m == NULL)
    return NULL;

  HsException = PyErr_NewException("hyphen.HsException", NULL, NULL);
  if (!HsException)
    {
      Py_DECREF(m);
      return NULL;
    }
  Py_INCREF(HsException);
  if (PyModule_AddObject(m, "HsException", HsException))
    {
      Py_DECREF(m);
      Py_DECREF(HsException);
      return NULL;
    }

  if (PyType_Ready(&HsTypeType) < 0)
    {
      Py_DECREF(m);
      return NULL;
    }
  Py_INCREF(&HsTypeType);
  if (PyModule_AddObject(m, "HsType",       (PyObject *)&HsTypeType))
    {
      Py_DECREF(m);
      Py_DECREF(&HsTypeType);
      return NULL;
    }

  if (PyType_Ready(&TyConType) < 0)
    {
      Py_DECREF(m);
      return NULL;
    }
  Py_INCREF(&TyConType);
  if (PyModule_AddObject(m, "TyCon",        (PyObject *)&TyConType ))
    {
      Py_DECREF(m);
      Py_DECREF(&TyConType);
      return NULL;
    }

  if (PyType_Ready(&HsObjRawType) < 0)
    {
      Py_DECREF(m);
      return NULL;
    }
  Py_INCREF(&HsObjRawType);
  if (PyModule_AddObject(m, "HsObjRaw",     (PyObject *)&HsObjRawType ))
    {
      Py_DECREF(m);
      Py_DECREF(&HsObjRawType);
      return NULL;
    }

#if !defined(mingw32_HOST_OS)
  /* The Haskell RTS installs a SIGINT signal handler, which we don't want
     because it interferes with Python's. So store Python's first, and
     restore it after hs_init() */
  python_siginthandler  = PyOS_getsig(SIGINT);
#endif
  hs_init(0, 0);
#if !defined(mingw32_HOST_OS)
  setupHaskellCtrlCHandler();
  haskell_siginthandler = PyOS_getsig(SIGINT);
  PyOS_setsig(SIGINT, python_siginthandler);
#endif
  hs_add_root(__stginit_Hyphen);

  ghc_srcmodules_loaded = 0;
  if (prepare_GHC_state(&ghc_interpreter_state) == -1)
    {
      Py_DECREF(m);
      return NULL;
    }
  if (addSimpleHsTypeObjsToModule(m))
    {
      Py_DECREF(m);
      return NULL;
    }
  return m;
}

