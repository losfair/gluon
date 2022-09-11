'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.query(`
CREATE TABLE AppDatabases (
  projectId TEXT NOT NULL,
  id INTEGER NOT NULL,
  createdAt DATETIME NOT NULL,
  updatedAt DATETIME NOT NULL,
  PRIMARY KEY (projectId, id)
) WITHOUT ROWID;
    `);
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.dropTable("AppDatabases");
  }
};
