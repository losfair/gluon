import { DataTypes } from "sequelize";
import { Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class Project extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  id: string | undefined;

  @Column(DataTypes.INTEGER)
  lastResourceId: number | undefined;

  @Column(DataTypes.INTEGER)
  createdAt: number | undefined;

  @Column(DataTypes.INTEGER)
  updatedAt: number | undefined;
}
