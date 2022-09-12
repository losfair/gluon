'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE Machines (
  projectId TEXT NOT NULL,
  id INTEGER NOT NULL,
  version INTEGER NOT NULL DEFAULT 0,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  updatedAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  PRIMARY KEY (projectId, id)
) WITHOUT ROWID;
      `, { transaction });

      await queryInterface.sequelize.query(`
CREATE TRIGGER Machines_PostUpdate AFTER UPDATE ON Machines
FOR EACH ROW BEGIN
  UPDATE Machines SET updatedAt = unixepoch('now'), version = old.version + 1
    WHERE projectId = old.projectId AND id = old.id;
END;
      `, { transaction });
    });
    
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('Machines');
  }
};