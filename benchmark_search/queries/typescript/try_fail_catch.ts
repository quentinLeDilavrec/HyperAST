// https://vitest.dev/api/expect.html#expect-unreachable
try {
  await build(dir)
  expect.unreachable('Should not pass build')
}
catch (err: any) {
  expect(err).toBeInstanceOf(Error)
  expect(err.stack).toContain('build')

  switch (dir) {
    case 'no-src-folder':
      expect(err.message).toBe(`${dir}/src does not exist`)
      break
    default:
      // to exhaust all error tests
      expect.unreachable('All error test must be handled')
      break
  }
}

// https://github.com/blackbaud/skyux/blob/f9ca0d8cc37422909e69c22d1367be1bbfd65ee7/libs/sdk/testing/vitest/src/setup-matchers.integration.spec.ts#L55


// https://github.com/2rr0r4o3/patchfuzz/blob/977703cb4030360db671eb25b1afd074ba562419/testsuite/v8/regress-6142.js#L13-L19

try {
  eval('continue;');
  assertUnreachable();
} catch (e) {
  assertTrue(e instanceof SyntaxError);
  assertEquals('Illegal continue statement: no surrounding iteration statement', e.message);
}


// https://github.com/operasoftware/presto-testo/blob/3edb42b3d6f647a8ece431ac39cadd841c946b0f/core/standards/html-parsing/dse/testharness.js#L232-L242

try {
  func.call(this);
  assert(false, make_message("assert_throws", description,
    "${func} did not throw", { func: String(func) }));
}
catch (e) {
  if (e instanceof AssertionError) {
    throw (e);
  }
}
