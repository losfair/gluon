import { DataTypes } from "sequelize";
import { Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class ProjectMember extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string;

  @PrimaryKey
  @Column(DataTypes.TEXT)
  userId: string;

  @Column(DataTypes.ENUM("owner", "member"))
  role: string;

  @Column(DataTypes.INTEGER)
  createdAt: number;
}
