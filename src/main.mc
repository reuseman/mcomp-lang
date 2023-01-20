component MyComponent provides App uses Algorithm {
    def print(a : int[], length : int) : void {
        var i : int = 0;
        put('[');
        for (; i < length; i++) {
            put(a[i]);
            if (i < length - 1) {
                put(',');
            }
        }
        print(']');
    }

    def main() : int {
        var my_arr : int[10];

        // Unsorted array
        my_arr[0] = 10;
        my_arr[1] = 9;
        my_arr[2] = 8;
        my_arr[3] = 7;
        my_arr[4] = 6;
        my_arr[5] = 5;
        my_arr[6] = 4;
        my_arr[7] = 3;
        my_arr[8] = 2;
        my_arr[9] = 1;

        print(my_arr, 10);

        // Search for 2
        var index : int = search(my_arr, 10, 2);
        print(index);

        // Search for 11
        index = search(my_arr, 10, 11);
        print(index);

        // Sort the array
        sort(my_arr, 10);

        print(my_arr, 10);

        // Search for 7
        index = binarySearch(my_arr, 10, 7);
        print(index);

        // Search for 11
        index = binarySearch(my_arr, 10, 11);
        print(index);
        
        return 0;
    }
}

connect {
  MyComponent.Algorithm <- AlgorithmLib.Algorithm;
}

interface Algorithm {
  def sort(a : int[], length: int) : void;
  def sort(a : float[], length: int) : void;
  def search(a : int[], length: int, key: int) : int;
  def search(a : float[], length: int, key: float) : int;
  def binarySearch(a : int[], length: int, key: int) : int;
  def binarySearch(a : float[], length: int, key: float) : int;
}

component AlgorithmLib provides Algorithm {
  def sort(a : int[], length: int) : void {
    var i : int = 0;
    var j : int = 0;
    var temp : int = 0;
    for (; i < length; i++) {
      for (j = i + 1; j < length; j++) {
        if (a[i] > a[j]) {
          temp = a[i];
          a[i] = a[j];
          a[j] = temp;
        }
      }
    }
  }

  def sort(a : float[], length: int) : void {
    var i : int = 0;
    var j : int = 0;
    var temp : float = 0.0;
    for (; i < length; i++) {
      for (j = i + 1; j < length; j++) {
        if (a[i] > a[j]) {
          temp = a[i];
          a[i] = a[j];
          a[j] = temp;
        }
      }
    }
  }

  def search(a : int[], length: int, key: int) : int {
    var i : int = 0;
    for (; i < length; i++) {
      if (a[i] == key) {
        return i;
      }
    }
    return -1;
  }

  def search(a : float[], length: int, key: float) : int {
    var i : int = 0;
    for (; i < length; i++) {
      if (a[i] == key) {
        return i;
      }
    }
    return -1;
  }

  def binarySearch(a : int[], length: int, key: int) : int {
    var low : int = 0;
    var high : int = length - 1;
    var mid : int = 0;
    while (low <= high) {
      mid = (low + high) / 2;
      if (a[mid] == key) {
        return mid;
      } else if (a[mid] < key) {
        low = mid + 1;
      } else {
        high = mid - 1;
      }
    }
    return -1;
  }

  def binarySearch(a : float[], length: int, key: float) : int {
    var low : int = 0;
    var high : int = length - 1;
    var mid : int = 0;
    while (low <= high) {
      mid = (low + high) / 2;
      if (a[mid] == key) {
        return mid;
      } else if (a[mid] < key) {
        low = mid + 1;
      } else {
        high = mid - 1;
      }
    }
    return -1;
  }
}
