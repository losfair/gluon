import { DataTypes } from "sequelize";
import { Column, HasOne, Model, PrimaryKey, Table } from "sequelize-typescript"
import Project from "./Project";

@Table({
  timestamps: false,
})
export default class MachineClaim extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  appId: number;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  machineId: number;

  @Column(DataTypes.INTEGER)
  createdAt: number;
}
