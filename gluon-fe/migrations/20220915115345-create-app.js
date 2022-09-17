'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE Apps (
  projectId TEXT NOT NULL,
  id INTEGER NOT NULL,
  name TEXT NOT NULL,
  autopilotAppName TEXT,
  spec JSON NOT NULL,
  config JSON NOT NULL,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  updatedAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  PRIMARY KEY (projectId, id)
) WITHOUT ROWID;
      `, { transaction });

      await queryInterface.sequelize.query(`
CREATE TRIGGER Apps_PostUpdate AFTER UPDATE ON Apps
FOR EACH ROW BEGIN
  UPDATE Apps SET updatedAt = unixepoch('now')
    WHERE projectId = old.projectId AND id = old.id;
END;
      `, { transaction });
    });

  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('Apps');
  }
};