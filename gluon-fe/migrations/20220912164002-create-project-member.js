'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE ProjectMembers (
  projectId TEXT NOT NULL,
  userId TEXT NOT NULL,
  role TEXT NOT NULL,
  createdAt INTEGER NOT NULL DEFAULT (unixepoch('now')),
  PRIMARY KEY (projectId, userId)
) WITHOUT ROWID;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE INDEX ProjectMembers_userId ON ProjectMembers (userId);
      `, { transaction });
    });
    
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable('ProjectMembers');
  }
};