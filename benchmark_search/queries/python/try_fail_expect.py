try:
    self.fail()
except Exception as e:
    pass


with self.assertRaises(Exception) as cm:
    do_something()

self.assertEqual(cm.exception.error_code, 3)

try:
    assert False
except Exception as e:
    pass
else:
    assert False

try:
    f()
except Exception as e:
    pass
else:
    assert False

# https://github.com/numpy/numpy/blob/411c74035e8bea9b45a9658b50c7dfd13b2bdc47/numpy/_core/tests/test_cpu_features.py#L194

# https://github.com/ceph/ceph/blob/b34429657ed4e592d044994ad5147d373925e893/src/pybind/mgr/selftest/module.py#L320-L326

# try:
#     g()
#     fail()
# except:
#     pass

# try:
#     g()
#     h()
#     fail()
# except:
#     pass

# try:
#     g()
#     h()
#     i()
#     fail()
# except:
#     pass

# try:
#     g()
#     fail()
#     i()
# except:
#     pass
