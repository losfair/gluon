import type { NextApiRequest, NextApiResponse } from 'next'
import { retryableTxn } from "../../../service/db";
import { checkProp_IsSafeInteger, getAndVerifyProjectPermissions, wrapApiHandler } from '../../../service/error_wrapper';
import { AppProvisioner } from '../../../service/provisioner/app_provisioner';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<{ deleted: boolean }>
) {
  const id = checkProp_IsSafeInteger("id", req.body.id);
  const deleted = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const privisioner = new AppProvisioner(projectId, transaction);
    const deleted = await privisioner.deleteApp(id);
    return deleted;
  });

  return res.status(200).json({
    deleted,
  });
}

export default wrapApiHandler(handler);
