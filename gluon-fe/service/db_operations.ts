import { Transaction } from "sequelize";
import Project from "../models/Project";
import { sequelize } from "./db";
import { ResourceNotFoundError } from "./error_wrapper";

export async function allocateResourceId(transaction: Transaction, projectId: string) {
  const res = await sequelize.query(
    "UPDATE Projects SET lastResourceId = lastResourceId + 1 WHERE id = $1 RETURNING lastResourceId",
    {
      bind: [projectId],
      transaction, mapToModel: true, model: Project
    }
  );
  if (res.length === 0) {
    throw new ResourceNotFoundError("project not found");
  }
  return res[0].lastResourceId;
}
