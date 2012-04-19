#!/usr/bin/python                                                                       

import compiler
import compiler.ast
import optparse
import sys

class AssertChecker(object):
    def __init__(self, quiet=False):
        self.quiet = quiet
        self.errors = 0
        self.current_filename = ""

    def check_files(self, files):
        for file in files:
            self.check_file(file)

    def check_file(self, filename):
        self.current_filename = filename
        try:
            ast = compiler.parseFile(filename)
        except SyntaxError:
            print >>sys.stderr, "SyntaxError on file %s" % filename
            return
        compiler.walk(ast, self)

    def visitAssert(self, node):
        if isinstance(node.test, compiler.ast.Tuple):
            if not self.quiet:
                print >>sys.stderr, "%s:%d: assert called with a tuple" % (self.current_filename, node.lineno)
                self.errors += 1
def main():
    parser = optparse.OptionParser(usage="%prog [options] file [files]", description="Checks asserts "
            "in python source files to ensure that they are not asserting "
            "tuples. Exits with 1 if there are any errors, as well as "
            "printing (unless -q is specified). Exits with 0 if there are "
            "no errors or if the source file could not be parsed.")
    parser.add_option("-q", "--quiet", dest="quiet", action="store_true", default=False,
            help="Do not print anything (exit code determines failure)")
    (opts, files) = parser.parse_args()
    if len(files) == 0:
        parser.error("No filenames provided")

    checker = AssertChecker(opts.quiet)
    checker.check_files(files)
    return 1 if checker.errors else 0

if __name__ == '__main__':
    sys.exit(main())
