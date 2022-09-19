import type { NextApiRequest, NextApiResponse } from 'next'
import { models, directTxn } from "../../../service/db";
import { checkProp_IsSafeInteger, getAndVerifyProjectPermissions, ResourceNotFoundError, wrapApiHandler } from '../../../service/error_wrapper';
import { getMachineInfo, MachineInfo } from '../../../service/fly';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<MachineInfo>
) {
  const id = checkProp_IsSafeInteger("id", req.body.id);

  const machine = await directTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const machine = await models.Machine.findOne({
      where: {
        projectId,
        id,
      },
      transaction,
    });
    return machine;
  });

  if (!machine?.flyId) throw new ResourceNotFoundError("machine not found or not ready");
  const info = await getMachineInfo(machine.flyId);

  return res.status(200).json(info);
}

export default wrapApiHandler(handler);
