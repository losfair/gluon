import { DataTypes } from "sequelize";
import type { CreationOptional, InferAttributes, InferCreationAttributes } from "sequelize";
import { AllowNull, BelongsToMany, Column, HasMany, Is, IsLowercase, Length, Max, Min, Model, PrimaryKey, Table, Validate } from "sequelize-typescript"
import ProjectMember from "./ProjectMember";

@Table({
  timestamps: false,
})
export default class Project extends Model<InferAttributes<Project>, InferCreationAttributes<Project>> {
  @PrimaryKey
  @Column
  id: string;

  @Is(/^[a-z0-9-]+$/)
  @Length({ min: 4, max: 50 })
  @Column
  name: string;

  @Column(DataTypes.INTEGER)
  lastResourceId: CreationOptional<number>;

  @Column(DataTypes.INTEGER)
  maxNumberOfApps: CreationOptional<number>;

  @Column(DataTypes.INTEGER)
  createdAt: CreationOptional<number>;

  @Column(DataTypes.INTEGER)
  updatedAt: CreationOptional<number>;
}
