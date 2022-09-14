import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { checkProp, checkProp_IsString, getAndVerifyProjectPermissions, LimitExceededError, PropCheckError, wrapApiHandler } from '../../../service/error_wrapper';
import { KNOBS } from '../../../service/knobs';
import { allocateResourceId } from '../../../service/db_operations';
import { MachineConfig_validate } from '../../../models/validation';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.Machine>
) {
  const name = checkProp_IsString("name", req.body.name);
  const config = checkProp("config", req.body.config, MachineConfig_validate);
  const machine = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const numMachines = await models.Machine.count({
      where: {
        projectId,
      },
      transaction,
    });
    if (numMachines + 1 > KNOBS.maxMachinesPerProject) {
      throw new LimitExceededError("this project can have at most " + KNOBS.maxMachinesPerProject + " machines");
    }

    const machineWithThisName = await models.Machine.findOne({
      where: {
        name,
      },
      transaction,
    });
    if (machineWithThisName) {
      throw new PropCheckError("a machine with this name already exists");
    }

    const id = await allocateResourceId(transaction, projectId);
    console.log("id", id);
    const machine = await models.Machine.create({
      projectId,
      id,
      name,
      config,
    }, { transaction });
    return machine;
  });

  return res.status(200).json(machine);
}

export default wrapApiHandler(handler);
