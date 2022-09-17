import { GetServerSidePropsContext, NextApiHandler, NextApiRequest } from "next";
import { getToken, JWT } from "next-auth/jwt";
import { Transaction, ValidationError } from "sequelize";
import * as models from "../models";
import axios from "axios";
import { NextRequest } from "next/server";

export class PropCheckError extends Error {
  details: any;

  constructor(message: string, details: any = {}) {
    super(message);
    this.name = "PropCheckError";
    this.details = details;
  }
}

export class MissingTokenError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "MissingTokenError";
  }
}

export class ResourceNotFoundError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ResourceNotFoundError";
  }
}

export class LimitExceededError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "LimitExceededError";
  }
}

export class PermissionError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PermissionError";
  }
}

export function checkProp<T>(dataName: string, data: unknown, check: (x: unknown) => x is T): T {
  if (!check(data)) {
    throw new PropCheckError("data does not have the expected property: " + dataName, {
      errors: (check as any).errors || [],
    });
  }
  return data;
}

export function checkProp_IsString(dataName: string, data: unknown): string {
  return checkProp(dataName, data, (x): x is string => typeof x === "string");
}

export function checkProp_IsSafeInteger(dataName: string, data: unknown): number {
  return checkProp(dataName, data, (x): x is number => typeof x === "number" && Number.isSafeInteger(x));
}

export async function mustGetToken(req: GetServerSidePropsContext["req"] | NextRequest | NextApiRequest): Promise<JWT & { uid: string }> {
  const token = await getToken({ req });
  if (!token || typeof token.uid !== "string") {
    throw new MissingTokenError("token is missing or invalid");
  }

  return token as any;
}


export async function getAndVerifyProjectPermissions2(req: GetServerSidePropsContext["req"] | NextRequest | NextApiRequest, projectId: string, transaction: Transaction | null): Promise<models.ProjectMember> {
  const { uid: userId } = await mustGetToken(req);

  const membership = await models.ProjectMember.findOne({
    where: {
      projectId,
      userId,
    },
    transaction,
  });
  if (!membership) {
    throw new PermissionError("you are not a member of this project");
  }

  return membership;
}

export async function getAndVerifyProjectPermissions(req: NextApiRequest, transaction: Transaction | null): Promise<models.ProjectMember> {
  const projectId = checkProp_IsString("projectId", req.body.projectId);
  return getAndVerifyProjectPermissions2(req, projectId, transaction);
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
          error: "prop_check_error",
          message: e.message,
          details: e.details,
        })
      }

      if (e instanceof MissingTokenError) {
        return res.status(401).json({
          error: "auth_error",
          message: e.message,
        })
      }

      if (e instanceof ResourceNotFoundError) {
        return res.status(404).json({
          error: "resource_not_found",
          message: e.message,
        });
      }

      if (e instanceof LimitExceededError) {
        return res.status(400).json({
          error: "limit_exceeded_error",
          message: e.message,
        });
      }

      if (e instanceof PermissionError) {
        return res.status(403).json({
          error: "permission_error",
          message: e.message,
        });
      }

      if (axios.isAxiosError(e)) {
        console.log("axios error: ", e);
        return res.status(502).json({
          error: "fetch_error",
          message: "" + e.code,
        });
      }

      throw e;
    }
  };
}
