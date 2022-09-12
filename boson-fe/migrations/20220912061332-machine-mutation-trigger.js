'use strict';

module.exports = {
  async up(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query(`
CREATE TRIGGER Machine_Insert_To_Mutation AFTER INSERT ON Machines
FOR EACH ROW BEGIN
  INSERT OR IGNORE INTO Mutations (projectId, resourceId, version, resourceKind, operation, info)
    VALUES(new.projectId, new.id, new.version, 'Machine', 'create', '{}');
END;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TRIGGER Machine_Update_To_Mutation AFTER UPDATE ON Machines
FOR EACH ROW WHEN new.version <> old.version BEGIN
  INSERT OR IGNORE INTO Mutations (projectId, resourceId, version, resourceKind, operation, info)
    VALUES(new.projectId, new.id, new.version, 'Machine', 'update', '{}');
END;
      `, { transaction });
      await queryInterface.sequelize.query(`
CREATE TRIGGER Machine_Delete_To_Mutation AFTER DELETE ON Machines
FOR EACH ROW BEGIN
  INSERT OR IGNORE INTO Mutations (projectId, resourceId, version, resourceKind, operation, info)
    VALUES(old.projectId, old.id, old.version + 1, 'Machine', 'delete', '{}');
END;
      `, { transaction });
    });

  },

  async down(queryInterface, Sequelize) {
    await queryInterface.sequelize.transaction(async transaction => {
      await queryInterface.sequelize.query("DROP TRIGGER Machine_Insert_To_Mutation", { transaction });
      await queryInterface.sequelize.query("DROP TRIGGER Machine_Update_To_Mutation", { transaction });
      await queryInterface.sequelize.query("DROP TRIGGER Machine_Delete_To_Mutation", { transaction });
    });
  }
};
