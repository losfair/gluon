'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE AppDatabases (
  projectId TEXT NOT NULL,
  id INTEGER NOT NULL,
  version INTEGER NOT NULL DEFAULT 0,
  nsKey TEXT NOT NULL,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  updatedAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  PRIMARY KEY (projectId, id)
) WITHOUT ROWID;
      `, { transaction });

      await queryInterface.sequelize.query(`
CREATE TRIGGER AppDatabases_PostUpdate AFTER UPDATE ON AppDatabases
FOR EACH ROW BEGIN
  UPDATE AppDatabases SET updatedAt = unixepoch('now')
    WHERE projectId = old.projectId AND id = old.id;
END;
      `, { transaction });
    });
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable("AppDatabases");
  }
};
