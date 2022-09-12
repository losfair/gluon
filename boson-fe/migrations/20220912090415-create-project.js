'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE Projects (
  id TEXT NOT NULL PRIMARY KEY,
  lastResourceId INTEGER NOT NULL DEFAULT 0,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  updatedAt INTEGER NOT NULL DEFAULT (unixepoch('now'))
) WITHOUT ROWID;
      `, { transaction });

      await queryInterface.sequelize.query(`
CREATE TRIGGER Projects_PostUpdate AFTER UPDATE ON Projects
FOR EACH ROW BEGIN
  UPDATE Projects SET updatedAt = unixepoch('now')
    WHERE id = old.id;
END;
      `, { transaction });
    });
    
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('Projects');
  }
};