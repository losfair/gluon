import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { getAndVerifyProjectPermissions, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.App[]>
) {
  const apps = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const apps = await models.App.findAll({
      where: {
        projectId,
      },
      transaction,
    });
    return apps;
  });

  return res.status(200).json(apps);
}

export default wrapApiHandler(handler);
