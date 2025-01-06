cdef extern from "<stdatomic.h>":
    ctypedef unsigned int atomic_uint
    cdef unsigned int atomic_load_uint "atomic_load" (atomic_uint *obj) nogil
    cdef unsigned int atomic_fetch_add_uint "atomic_fetch_add" (atomic_uint *obj, unsigned int arg) nogil
    cdef unsigned int atomic_compare_exchange_weak_uint "atomic_compare_exchange_weak" (atomic_uint *obj, unsigned int *expected, unsigned int desired) nogil
