import axios from "axios";

export interface MachineInfo {
  id: string,
  name: string,
  state: string,
  region: string,
  instance_id: string,
  private_ip: string,
  created_at: string,
  updated_at: string,
  config: any,
  image_ref: any
}

const FLY_MACHINE_API_ORIGIN = process.env.FLY_MACHINE_API_ORIGIN;
if (!FLY_MACHINE_API_ORIGIN) {
  throw new Error("FLY_MACHINE_API_ORIGIN is not defined");
}
const FLY_MACHINE_API_TOKEN = process.env.FLY_MACHINE_API_TOKEN;
if (!FLY_MACHINE_API_TOKEN) {
  throw new Error("FLY_MACHINE_API_TOKEN is not defined");
}
const FLY_MACHINE_APP_NAME = process.env.FLY_MACHINE_APP_NAME;
if (!FLY_MACHINE_APP_NAME) {
  throw new Error("FLY_MACHINE_APP_NAME is not defined");
}

export async function getMachineInfo(machineId: string): Promise<MachineInfo> {
  const { data } = await axios.get<MachineInfo>(`${FLY_MACHINE_API_ORIGIN}/v1/apps/${FLY_MACHINE_APP_NAME}/machines/${machineId}`, {
    headers: {
      "Authorization": `Bearer ${FLY_MACHINE_API_TOKEN}`,
    }
  });
  return data;
}