'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {

    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE Mutations (
  projectId TEXT NOT NULL,
  resourceId INTEGER NOT NULL,
  version INTEGER NOT NULL,
  resourceKind TEXT NOT NULL,
  operation TEXT NOT NULL,
  info TEXT NOT NULL,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  PRIMARY KEY (projectId, resourceId, version)
) WITHOUT ROWID;
      `, { transaction });
    });
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable("Mutations");
  }
};
