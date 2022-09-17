import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { checkProp_IsSafeInteger, getAndVerifyProjectPermissions, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<{ deleted: number }>
) {
  const id = checkProp_IsSafeInteger("id", req.body.id);
  const deleted = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const count = await models.Machine.destroy({
      where: {
        projectId,
        id,
      },
      transaction,
    });
    return count;
  });

  return res.status(200).json({
    deleted,
  });
}

export default wrapApiHandler(handler);
