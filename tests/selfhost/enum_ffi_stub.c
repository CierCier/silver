typedef enum Linkage {
    External0 = 0,
    Internal8 = 8,
} Linkage;

static int last_linkage;

void LLVMSetLinkage(void *value, Linkage linkage) {
    (void)value;
    last_linkage = (int)linkage;
}

int silver_test_get_linkage_i32(void) {
    return last_linkage;
}

Linkage silver_test_get_fixed_linkage(void) {
    return Internal8;
}

typedef enum PositiveLinkage {
    PositiveLow = 0,
    PositiveHigh = 32769,
} PositiveLinkage;

typedef enum NegativeLinkage {
    NegativeZero = 0,
    NegativeOne = -1,
} NegativeLinkage;

static int positive_linkage;
static int negative_linkage;

void silver_test_set_positive_linkage(PositiveLinkage linkage) {
    positive_linkage = (int)linkage;
}

int silver_test_get_positive_linkage(void) {
    return positive_linkage;
}

void silver_test_set_negative_linkage(NegativeLinkage linkage) {
    negative_linkage = (int)linkage;
}

int silver_test_get_negative_linkage(void) {
    return negative_linkage;
}
