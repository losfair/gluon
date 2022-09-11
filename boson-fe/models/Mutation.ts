import { DataTypes } from "sequelize";
import { Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table
export default class Mutation extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  resourceId: number;

  @PrimaryKey
  @Column(DataTypes.ENUM(
    "create_machine", "delete_machine",
    "create_app_database", "delete_app_database",
  ))
  kind: string;

  @PrimaryKey
  @Column(DataTypes.JSON)
  info: any;
}
