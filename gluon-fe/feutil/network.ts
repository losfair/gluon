export async function loadJson<T = any>(path: string, payload?: unknown, opts: { abort?: AbortSignal, headers?: Record<string, string> } = {}): Promise<T> {
  const rsp = await fetch(path, {
    method: payload ? "POST" : "GET",
    body: payload ? JSON.stringify(payload) : undefined,
    credentials: "include",
    signal: opts?.abort,
    headers: {
      ...(payload ? { "Content-Type": "application/json" } : {}),
      ...(opts.headers || {}),
    },
  })
  if (!rsp.ok) {
    const t = await rsp.text();
    const e = new HttpError(t);
    e.status = rsp.status;
    throw e;
  }

  return await rsp.json();
}

export class HttpError extends Error {
  private inner: any;
  status: number;

  constructor(inner: any) {
    super("http error: " + inner);
    this.inner = inner;
    this.status = 0;
  }

  get(): any {
    return this.inner;
  }
}
