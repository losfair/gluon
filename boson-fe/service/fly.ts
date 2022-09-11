export const FLY_API_HOSTNAME = process.env.FLY_API_HOSTNAME;
if (!FLY_API_HOSTNAME) {
  throw new Error("FLY_API_HOSTNAME is not defined");
}

export const FLY_API_TOKEN = process.env.FLY_API_TOKEN;
if (!FLY_API_TOKEN) {
  throw new Error("FLY_API_TOKEN is not defined");
}
