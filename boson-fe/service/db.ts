import { Transaction } from "sequelize";
import { Sequelize } from "sequelize-typescript";
import * as models from "../models";
export * as models from "../models";

const BOSON_DB = process.env.BOSON_DB;
if (!BOSON_DB) {
  throw new Error("BOSON_DB is not defined");
}

export const sequelize = new Sequelize("", "", "", {
  dialect: 'sqlite',
  storage: BOSON_DB,
  models: Object.values(models),
});

export async function retryableTxn<T>(fn: (txn: Transaction) => Promise<T>): Promise<T> {
  while (true) {
    try {
      return await sequelize.transaction((t) => {
        return fn(t);
      });
    } catch (e) {
      // TODO: Check lock error
      throw e;
    }
  }
}
