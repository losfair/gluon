import { NextApiHandler, NextApiRequest } from "next";
import { getToken, JWT } from "next-auth/jwt";
import { ValidationError } from "sequelize";

export class PropCheckError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PropCheckError";
  }
}

export class MissingTokenError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "MissingTokenError";
  }
}

export function checkProp<T>(dataName: string, data: unknown, check: (x: unknown) => x is T): T {
  if (!check(data)) {
    throw new PropCheckError("data does not have the expected property: " + dataName);
  }
  return data;
}

export function checkProp_IsString(dataName: string, data: unknown): string {
  return checkProp(dataName, data, (x): x is string => typeof x === "string");
}

export async function mustGetToken(req: NextApiRequest): Promise<JWT & { uid: string }> {
  const token = await getToken({ req });
  if (!token || typeof token.uid !== "string") {
    throw new MissingTokenError("token is missing or invalid");
  }

  return token as any;
}

export function wrapApiHandler(handler: NextApiHandler): NextApiHandler {
  return async (req, res) => {
    try {
      await handler(req, res);
    } catch (e) {
      if (e instanceof ValidationError) {
        return res.status(400).json({
          error: "validation_error",
          message: e.message,
        })
      }

      if (e instanceof PropCheckError) {
        return res.status(400).json({
          error: "validation_error",
          message: e.message,
        })
      }

      if (e instanceof MissingTokenError) {
        return res.status(401).json({
          error: "auth_error",
          message: e.message,
        })
      }

      throw e;
    }
  };
}
