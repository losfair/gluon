import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { getAndVerifyProjectPermissions, LimitExceededError, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.Machine[]>
) {
  const machines = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const machines = await models.Machine.findAll({
      where: {
        projectId,
      },
      transaction,
    });
    return machines;
  });

  return res.status(200).json(machines);
}

export default wrapApiHandler(handler);
