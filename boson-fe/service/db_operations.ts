import { Transaction } from "sequelize";
import Project from "../models/Project";
import { sequelize } from "./db";

export async function allocateResourceId(transaction: Transaction, projectId: string) {
  const res = await sequelize.query(
    "UPDATE Projects SET lastResourceId = lastResourceId + 1 WHERE id = :projectId RETURNING lastResourceId",
    { transaction, mapToModel: true, model: Project }
  );
  if (res.length === 0) {
    throw new Error("Project not found");
  }
  return res[0].lastResourceId;
}
