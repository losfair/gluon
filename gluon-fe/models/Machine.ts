import { JSONSchemaType } from "ajv"
import type { CreationOptional, InferAttributes, InferCreationAttributes } from "sequelize";
import { DataTypes } from "sequelize";
import { Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class Machine extends Model<InferAttributes<Machine>, InferCreationAttributes<Machine>> {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number;

  @Column(DataTypes.INTEGER)
  version: CreationOptional<number>;

  @Column(DataTypes.TEXT)
  name: string;

  @Column(DataTypes.JSON)
  config: MachineConfig;

  @Column(DataTypes.TEXT)
  flyId: CreationOptional<string | null>;

  @Column(DataTypes.INTEGER)
  createdAt: CreationOptional<number>;

  @Column(DataTypes.INTEGER)
  updatedAt: CreationOptional<number>;
}

export interface MachineConfig {
  image: string;
  env?: Record<string, string> | null;
  guest?: {
    cpus: number;
    cpu_kind: "shared" | "performance";
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
        cpu_kind: { type: "string", enum: ["shared", "performance"] },
        memory_mb: { type: "integer" },
      },
      required: ["cpus", "cpu_kind", "memory_mb"],
      nullable: true,
    },
  },
  required: ["image"],
  additionalProperties: false,
};
