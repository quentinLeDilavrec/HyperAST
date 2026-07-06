try {
    fail();
} catch (E e) {
}

try {
    g();
    fail();
} catch (E e) {
}

try {
    g();
    h();
    fail();
} catch (E e) {
}

try {
    g();
    h();
    i();
    fail();
} catch (E e) {
}

try {
    g();
    fail();
    i();
} catch (E e) {
}
