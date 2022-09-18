import type { NextApiRequest, NextApiResponse } from 'next'
import { Machine } from '../../../models';
import { AppInfo } from '../../../service/api_types';
import { models, retryableTxn } from "../../../service/db";
import { getAndVerifyProjectPermissions, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<AppInfo[]>
) {
  const apps = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const apps = await models.App.findAll({
      where: {
        projectId,
      },
      transaction,
    });
    const appInfo: AppInfo[] = [];
    for (const app of apps) {
      const claims = await models.MachineClaim.findAll({
        where: {
          projectId,
          appId: app.id,
        },
        transaction,
      });
      const machines: Machine[] = [];
      for (const claim of claims) {
        const m = await models.Machine.findOne({
          where: {
            projectId,
            id: claim.machineId,
          },
          transaction,
        });
        if (m) {
          machines.push(m.toJSON());
        }
      }
      appInfo.push({
        ...app.toJSON(),
        machines,
      });
    }
    return appInfo;
  });

  return res.status(200).json(apps);
}

export default wrapApiHandler(handler);
