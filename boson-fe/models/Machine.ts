import { DataTypes } from "sequelize";
import { Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class Machine extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string | undefined;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number | undefined;

  @Column(DataTypes.INTEGER)
  version: number | undefined;

  @Column(DataTypes.INTEGER)
  createdAt: number | undefined;

  @Column(DataTypes.INTEGER)
  updatedAt: number | undefined;
}
