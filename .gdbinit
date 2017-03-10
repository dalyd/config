python
import sys
import os
try:
    sys.path.insert(0, os.path.expanduser('~/mongo_gdb/'))
    import mongo_printer
    mongo_printer.register_mongo_printers()
except Exception:
    print "Error loading mongo_printer"
    pass
end
