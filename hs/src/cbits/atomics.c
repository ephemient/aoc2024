#include <stdatomic.h>

int atomic_fetch_add_int(atomic_int *obj, int arg) {
    return atomic_fetch_add(obj, arg);
}
