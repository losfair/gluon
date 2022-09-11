'use strict';

module.exports = {
  async up (queryInterface, Sequelize) {
    await queryInterface.sequelize.query(`
CREATE TABLE Mutations (
  projectId TEXT NOT NULL,
  id INTEGER NOT NULL,
  resourceId INTEGER NOT NULL,
  kind TEXT NOT NULL,
  info JSON NOT NULL,
  createdAt DATETIME NOT NULL,
  updatedAt DATETIME NOT NULL,
  PRIMARY KEY (projectId, id)
) WITHOUT ROWID;
    `);
  },

  async down (queryInterface, Sequelize) {
    await queryInterface.dropTable("Mutations");
  }
};
