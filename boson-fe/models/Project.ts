import { DataTypes } from "sequelize";
import { AllowNull, Column, IsLowercase, Length, Max, Min, Model, PrimaryKey, Table, Validate } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class Project extends Model {
  @PrimaryKey
  @Column
  id: string;

  @IsLowercase
  @Length({min: 4, max: 100})
  @Column
  name: string;

  @Column(DataTypes.INTEGER)
  lastResourceId: number;

  @Column(DataTypes.INTEGER)
  createdAt: number;

  @Column(DataTypes.INTEGER)
  updatedAt: number;
}
