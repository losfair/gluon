import { DataTypes } from "sequelize";
import { AllowNull, Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class AppDatabase extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number;

  @Column(DataTypes.INTEGER)
  version: number;

  @Column(DataTypes.TEXT)
  nsKey: number;

  @Column(DataTypes.INTEGER)
  createdAt: number;

  @Column(DataTypes.INTEGER)
  updatedAt: number;
}
