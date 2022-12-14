import { Transaction } from "sequelize";
import { AppConfig, AppSpec } from "../../models/App";
import { MachineConfig } from "../../models/Machine";
import { models } from "../db";
import { allocateResourceId } from "../db_operations";
import { LimitExceededError, PropCheckError } from "../error_wrapper";

const allowedMemorySizes = [256, 512, 1024];

export class AppProvisioner {
  constructor(public projectId: string, public transaction: Transaction) {

  }

  async createApp(name: string, spec: AppSpec, config: AppConfig): Promise<models.App> {
    if (config.cpus !== 1) {
      throw new LimitExceededError("only 1 CPU is allowed");
    }
    if (!allowedMemorySizes.includes(config.memoryMB)) {
      throw new LimitExceededError("memory size must be one of " + allowedMemorySizes.join(", "));
    }

    const machineEnv: Record<string, string> = { ...config.env || {} };
    for (const [envKey, envSpec] of Object.entries(spec.env || {})) {
      const isSet = typeof machineEnv[envKey] === "string";

      if (envSpec.required && !isSet) {
        throw new PropCheckError(`missing required env var '${envKey}'`);
      }

      if (isSet && typeof envSpec.regex === "string") {
        let regex: RegExp;
        try {
          regex = new RegExp(envSpec.regex);
        } catch (e) {
          throw new PropCheckError(`env var '${envKey}' has invalid regex: ${envSpec.regex}`);
        }

        if (!regex.test(machineEnv[envKey])) {
          throw new PropCheckError(`env var '${envKey}' does not match regex: ${envSpec.regex}`);
        }
      }

      // This is the last step. Skip regex validation for the default value.
      if (!isSet && typeof envSpec.default === "string") {
        machineEnv[envKey] = envSpec.default;
      }
    }

    const app = await models.App.create({
      projectId: this.projectId,
      id: await allocateResourceId(this.transaction, this.projectId),
      name,
      spec,
      config,
    }, { transaction: this.transaction });

    const machineConfig: MachineConfig = {
      image: spec.image,
      env: machineEnv,
      guest: {
        cpus: config.cpus,
        cpu_kind: "shared",
        memory_mb: config.memoryMB,
      },
    };

    const machine = await models.Machine.create({
      projectId: this.projectId,
      id: await allocateResourceId(this.transaction, this.projectId),
      name: "app-" + app.id,
      config: machineConfig,
    }, { transaction: this.transaction });

    await models.MachineClaim.create({
      projectId: this.projectId,
      appId: app.id,
      machineId: machine.id,
    }, { transaction: this.transaction });

    const fullApp = await models.App.findOne({
      where: {
        projectId: this.projectId,
        id: app.id,
      },
      transaction: this.transaction,
    });
    return fullApp!;
  }

  async deleteApp(appId: number): Promise<boolean> {
    const deleted = await models.App.destroy({
      where: {
        projectId: this.projectId,
        id: appId,
      },
      transaction: this.transaction,
    });
    if (!deleted) {
      return false;
    }

    /* Delete machine claims and machines */

    const machineClaims = await models.MachineClaim.findAll({
      where: {
        projectId: this.projectId,
        appId,
      },
      transaction: this.transaction,
    });
    await models.MachineClaim.destroy({
      where: {
        projectId: this.projectId,
        appId,
      },
      transaction: this.transaction,
    });
    for (const claim of machineClaims) {
      await models.Machine.destroy({
        where: {
          projectId: this.projectId,
          id: claim.machineId,
        },
        transaction: this.transaction,
      });
    }
    return true;
  }
}