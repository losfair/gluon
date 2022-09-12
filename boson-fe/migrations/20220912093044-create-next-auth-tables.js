'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    /**
     * Add altering commands here.
     *
     * Example:
     * await queryInterface.createTable('users', { id: Sequelize.INTEGER });
     */

    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TABLE users (
  id TEXT NOT NULL PRIMARY KEY,
  name TEXT,
  email TEXT,
  email_verified DATETIME,
  image TEXT
) WITHOUT ROWID;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE UNIQUE INDEX users_email ON users (email);
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TABLE accounts (
  id TEXT NOT NULL PRIMARY KEY,
  type TEXT NOT NULL,
  provider TEXT NOT NULL,
  provider_account_id TEXT NOT NULL,
  refresh_token TEXT,
  access_token TEXT,
  expires_at INTEGER,
  token_type TEXT,
  scope TEXT,
  id_token TEXT,
  session_state TEXT,
  userId TEXT
) WITHOUT ROWID;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TABLE sessions (
  id TEXT NOT NULL PRIMARY KEY,
  expires DATETIME NOT NULL,
  session_token TEXT NOT NULL,
  user_id TEXT
) WITHOUT ROWID;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE UNIQUE INDEX sessions_session_token ON sessions (session_token);
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TABLE verification_tokens (
  token TEXT NOT NULL PRIMARY KEY,
  identifier TEXT NOT NULL,
  expires DATETIME NOT NULL
) WITHOUT ROWID;
      `, { transaction });
    });
  },

  async down(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.dropTable("users", { transaction });
      await queryInterface.dropTable("accounts", { transaction });
      await queryInterface.dropTable("sessions", { transaction });
      await queryInterface.dropTable("verification_tokens", { transaction });
    });
  }
};
