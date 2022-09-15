import Ajv from "ajv";
import { AppConfig_schema, AppSpec_schema } from "./App";
import { MachineConfig_schema } from "./Machine";

const ajv = new Ajv();

export const MachineConfig_validate = ajv.compile(MachineConfig_schema);
export const AppSpec_validate = ajv.compile(AppSpec_schema);
export const AppConfig_validate = ajv.compile(AppConfig_schema);
