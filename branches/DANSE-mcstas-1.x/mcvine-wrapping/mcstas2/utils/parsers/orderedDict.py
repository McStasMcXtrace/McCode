from UserDict import DictMixin

# This is an implementation of an ordered dictionary,
# taken from the online Python Cookbook.

class OrderedDict(DictMixin):
    
    def __init__(self, data=None, **kwdata):
        self._keys = []
        self._data = {}
        if data is not None:
            if hasattr(data, 'items'):
                items = data.items()
            else:
                items = list(data)
            for i in xrange(len(items)):
                length = len(items[i])
                if length != 2:
                    raise ValueError('dictionary update sequence element '
                        '#%d has length %d; 2 is required' % (i, length))
                self._keys.append(items[i][0])
                self._data[items[i][0]] = items[i][1]
        if kwdata:
            self._merge_keys(kwdata.iterkeys())
            self.update(kwdata)
        
        
        
    def __setitem__(self, key, value):
        if key not in self._data:
            self._keys.append(key)
        self._data[key] = value
        
        
    def __getitem__(self, key):
        return self._data[key]
    
    
    def __delitem__(self, key):
        del self._data[key]
        self._keys.remove(key)

    def __iter__(self):
        for key in self._keys:
            yield key
        
    def keys(self):
        return list(self._keys)
    
    
    def copy(self):
        copyDict = odict()
        copyDict._data = self._data.copy()
        copyDict._keys = self._keys[:]
        return copyDict


    def __repr__(self):
        result = []
        for key in self._keys:
            result.append('(%s, %s)' % (repr(key), repr(self._data[key])))
        return ''.join(['OrderedDict', '([', ', '.join(result), '])'])

    def _merge_keys(self, keys):
        self._keys.extend(keys)
        newkeys = {}
        self._keys = [newkeys.setdefault(x, x) for x in self._keys
            if x not in newkeys]


    def update(self, data):
        if data is not None: 
            if hasattr(data, 'iterkeys'):
                self._merge_keys(data.iterkeys())
            else:
                self._merge_keys(data.keys())
            self._data.update(data)
