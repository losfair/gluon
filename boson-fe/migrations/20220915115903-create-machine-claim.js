'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE MachineClaims (
  projectId TEXT NOT NULL,
  appId INTEGER NOT NULL,
  machineId INTEGER NOT NULL,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  PRIMARY KEY (projectId, appId, machineId)
) WITHOUT ROWID;
      `, { transaction });
    });

  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('MachineClaims');
  }
};