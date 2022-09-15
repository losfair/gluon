import { JSONSchemaType } from "ajv"
import { DataTypes } from "sequelize";
import { Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class Machine extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number;

  @Column(DataTypes.INTEGER)
  version: number;

  @Column(DataTypes.TEXT)
  name: string;

  @Column(DataTypes.JSON)
  config: MachineConfig;

  @Column(DataTypes.INTEGER)
  createdAt: number;

  @Column(DataTypes.INTEGER)
  updatedAt: number;
}

export interface MachineConfig {
  image: string;
  env?: Record<string, string> | null;
  guest?: {
    cpus: number;
    memory_mb: number;
  } | null;
}

export const MachineConfig_schema: JSONSchemaType<MachineConfig> = {
  type: "object",
  properties: {
    image: { type: "string" },
    env: {
      type: "object",
      additionalProperties: { type: "string" },
      required: [],
      nullable: true,
    },
    guest: {
      type: "object",
      properties: {
        cpus: { type: "integer" },
        memory_mb: { type: "integer" },
      },
      required: ["cpus", "memory_mb"],
      nullable: true,
    },
  },
  required: ["image"],
  additionalProperties: false,
};
