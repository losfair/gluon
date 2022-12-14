import { DataTypes } from "sequelize";
import type { CreationOptional, InferAttributes, InferCreationAttributes } from "sequelize";
import { Column, HasOne, Model, PrimaryKey, Table } from "sequelize-typescript"
import Project from "./Project";

@Table({
  timestamps: false,
})
export default class ProjectMember extends Model<InferAttributes<ProjectMember>, InferCreationAttributes<ProjectMember>> {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.TEXT)
  userId: string;

  @Column(DataTypes.ENUM("owner", "member"))
  role: string;

  @Column(DataTypes.INTEGER)
  createdAt: CreationOptional<number>;
}
