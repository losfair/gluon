import { DataTypes } from "sequelize";
import { AllowNull, Column, Model, PrimaryKey, Table } from "sequelize-typescript"

@Table({
  timestamps: false,
})
export default class AppDatabase extends Model {
  @PrimaryKey
  @Column(DataTypes.TEXT)
  projectId: string | undefined;

  @PrimaryKey
  @Column(DataTypes.INTEGER)
  id: number | undefined;

  @Column(DataTypes.INTEGER)
  version: number | undefined;

  @Column(DataTypes.TEXT)
  nsKey: number | undefined;

  @Column(DataTypes.INTEGER)
  createdAt: number | undefined;

  @Column(DataTypes.INTEGER)
  updatedAt: number | undefined;
}

/*
  projectId TEXT NOT NULL,
  id INTEGER NOT NULL,
  version INTEGER NOT NULL DEFAULT 0,
  nsKey TEXT NOT NULL,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  updatedAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
*/