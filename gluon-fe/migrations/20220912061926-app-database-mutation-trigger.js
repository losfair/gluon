'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TRIGGER AppDatabase_Insert_To_Mutation AFTER INSERT ON AppDatabases
FOR EACH ROW BEGIN
  INSERT OR IGNORE INTO Mutations (projectId, resourceId, version, resourceKind, operation, info)
    VALUES(new.projectId, new.id, new.version, 'AppDatabase', 'create', '{}');
END;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TRIGGER AppDatabase_Update_To_Mutation AFTER UPDATE ON AppDatabases
FOR EACH ROW WHEN new.version <> old.version BEGIN
  INSERT OR IGNORE INTO Mutations (projectId, resourceId, version, resourceKind, operation, info)
    VALUES(new.projectId, new.id, new.version, 'AppDatabase', 'update', '{}');
END;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TRIGGER AppDatabase_Delete_To_Mutation AFTER DELETE ON AppDatabases
FOR EACH ROW BEGIN
  INSERT OR IGNORE INTO Mutations (projectId, resourceId, version, resourceKind, operation, info)
    VALUES(old.projectId, old.id, old.version + 1, 'AppDatabase', 'delete', json_insert('{}', '$.nsKey', old.nsKey));
END;
      `, { transaction });
    });

  },

  async down(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query("DROP TRIGGER AppDatabase_Insert_To_Mutation", { transaction });
      await queryInterface.sequelize.query("DROP TRIGGER AppDatabase_Update_To_Mutation", { transaction });
      await queryInterface.sequelize.query("DROP TRIGGER AppDatabase_Delete_To_Mutation", { transaction });
    });
  }
};
