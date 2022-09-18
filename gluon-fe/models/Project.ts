import { DataTypes } from "sequelize";
import type { CreationOptional, InferAttributes, InferCreationAttributes } from "sequelize";
import { AllowNull, BelongsToMany, Column, HasMany, IsLowercase, Length, Max, Min, Model, PrimaryKey, Table, Validate } from "sequelize-typescript"
import ProjectMember from "./ProjectMember";

@Table({
  timestamps: false,
})
export default class Project extends Model<InferAttributes<Project>, InferCreationAttributes<Project>> {
  @PrimaryKey
  @Column
  id: string;

  @IsLowercase
  @Length({ min: 4, max: 100 })
  @Column
  name: string;

  @Column(DataTypes.INTEGER)
  lastResourceId: CreationOptional<number>;

  @Column(DataTypes.INTEGER)
  createdAt: CreationOptional<number>;

  @Column(DataTypes.INTEGER)
  updatedAt: CreationOptional<number>;
}
