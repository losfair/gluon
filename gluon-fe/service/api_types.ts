import { InferAttributes } from "sequelize";
import { App, Machine } from "../models";

export interface AppInfo extends InferAttributes<App> {
  machines: InferAttributes<Machine>[],
}
