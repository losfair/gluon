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
