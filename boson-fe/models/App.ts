import { JSONSchemaType } from "ajv";
import { DataTypes } from "sequelize";
import { AllowNull, Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class App extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number;

  @Column(DataTypes.TEXT)
  name: string;

  @Column(DataTypes.JSON)
  spec: AppSpec;

  @Column(DataTypes.JSON)
  config: AppConfig;

  @Column(DataTypes.INTEGER)
  createdAt: number;

  @Column(DataTypes.INTEGER)
  updatedAt: number;
}

export interface AppSpec {
  image: string;
  minMemoryMB?: number | null;
  env?: Record<string, {
    description?: string | null;
    default?: string | null;
    required?: boolean | null;
    type?: "text" | "switch" | "secret" | null;
  }> | null;
  s3?: Record<string, {
    description?: string | null;
    accessKeyAs: string;
    secretKeyAs: string;
  }> | null;
  port: number;
}

export const AppSpec_schema: JSONSchemaType<AppSpec> = {
  type: "object",
  properties: {
    image: { type: "string" },
    minMemoryMB: { type: "integer", nullable: true },
    env: {
      type: "object",
      additionalProperties: {
        type: "object",
        properties: {
          description: { type: "string", nullable: true },
          default: { type: "string", nullable: true },
          required: { type: "boolean", nullable: true },
          type: { type: "string", enum: ["text", "switch", "secret"], nullable: true },
        },
        additionalProperties: false,
      },
      required: [],
      nullable: true,
    },
    s3: {
      type: "object",
      additionalProperties: {
        type: "object",
        properties: {
          description: { type: "string", nullable: true },
          accessKeyAs: { type: "string" },
          secretKeyAs: { type: "string" },
        },
        required: ["accessKeyAs", "secretKeyAs"],
        additionalProperties: false,
      },
      required: [],
      nullable: true,
    },
    port: { type: "integer" },
  },
  required: ["image", "port"],
  additionalProperties: false,
};

export interface AppConfig {
  cpus: number;
  memoryMB: number;
  env?: Record<string, string> | null;
}

export const AppConfig_schema: JSONSchemaType<AppConfig> = {
  type: "object",
  properties: {
    cpus: { type: "number" },
    memoryMB: { type: "integer" },
    env: {
      type: "object",
      additionalProperties: { type: "string" },
      required: [],
      nullable: true,
    },
  },
  required: ["cpus", "memoryMB"],
  additionalProperties: false,
};
