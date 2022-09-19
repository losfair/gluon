import { Transaction } from "sequelize";
import { Sequelize } from "sequelize-typescript";
import * as models from "../models";
export * as models from "../models";

const GLUON_DB = process.env.GLUON_DB;
if (!GLUON_DB) {
  throw new Error("GLUON_DB is not defined");
}

export const sequelize = new Sequelize("", "", "", {
  dialect: "sqlite",
  storage: GLUON_DB,
  models: Object.values(models),
  retry: { max: 0 }, // makes no sense to retry with mvsqlite
});

export async function retryableTxn<T>(fn: (txn: Transaction) => Promise<T>): Promise<T> {
  while (true) {
    const txn = await sequelize.transaction();
    let ret: T;
    try {
      ret = await fn(txn);
    } catch (e) {
      await txn.rollback();
      throw e;
    }

    const recoverAndCommit = async () => {
      await sequelize.query("BEGIN DEFERRED TRANSACTION", { transaction: txn });
      await txn.commit();
    };

    // HACK
    try {
      await sequelize.query("COMMIT", { transaction: txn });
    } catch (e: any) {
      if (e.name === "SequelizeTimeoutError" && e.parent?.code === "SQLITE_BUSY") {
        console.warn("retryableTxn: got SQLITE_BUSY, retrying");
        await txn.rollback();
        continue;
      } else {
        await recoverAndCommit();
        throw e;
      }
    }

    await recoverAndCommit();
    return ret;
  }
}

export async function directTxn<T>(fn: (txn: Transaction) => Promise<T>): Promise<T> {
  return await sequelize.transaction(fn);
}
