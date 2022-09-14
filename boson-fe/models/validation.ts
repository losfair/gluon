import Ajv from "ajv";
import { MachineConfig_schema } from "./Machine";

const ajv = new Ajv();

export const MachineConfig_validate = ajv.compile(MachineConfig_schema);
