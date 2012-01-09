#include "dll.h"

export {
typedef struct {
   int ival;
} MyStruct;
}

def_ alloc(T) (T*)malloc(sizeof(T))

// will work in this module, but the .h file will contain
// unprocessed code!
def_ This MyStruct *this

export MyStruct *create() {
    return alloc(MyStruct);
}

// Ruby-style!
def_ @ ms->

export int one(MyStruct *ms) {
    return @ival + 1
}

export int two(MyStruct *ms) {
    with(MyType *,bonzo) {
        .x = 2;
        .y = 3;
        with(SubType *,.data) {
            .name = "hello";
            .ids = my.ids;
            printf("count %d\n",.count);
        }
    }
    return 2*ms->ival;
}

grab void bonzo(This) {
    printf("hello\n");
}


