import { useState, useEffect } from "react";

export function useLatch<T>(value: T | null): T | null {
  const [latch, setLatch] = useState(value);
  useEffect(() => {
    if (value !== null) {
      setLatch(value);
    }
  }, [value]);
  return latch;
}
