package timeacc;

import java.util.HashMap;

public class Root {
    private HashMap<Object, IAcc> _accMap = new HashMap();

    private IAcc getAcc(Object k, IAcc defaultAcc) {
        IAcc result = _accMap.get(k);
        if (result != null) {
            return result;
        }
        _accMap.put(k, defaultAcc);
        return defaultAcc;
    }
    
    public IAcc getUnsafeAcc(Object k) {
        return getAcc(k, new UnsafeAcc());
    }

    public void reset() {
        for (IAcc acc: _accMap.values()) {
            acc.reset();
        }
    }

    public HashMap<Object, IAcc> getAccMap() {
        return _accMap;
    }
}
