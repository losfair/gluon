import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { mustGetToken, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<{ project: models.Project, membership: models.ProjectMember }[]>
) {
  const token = await mustGetToken(req);
  const userId = token.uid;

  const result = await retryableTxn(async transaction => {
    const memberships = await models.ProjectMember.findAll({
      where: {
        userId,
        role: "owner",
      },
      order: [["projectId", "ASC"]],
      transaction,
    });
    const result: { project: models.Project, membership: models.ProjectMember }[] = [];
    for (const m of memberships) {
      const project = await models.Project.findOne({
        where: {
          id: m.projectId,
        },
        transaction,
      });
      if (project) {
        result.push({
          project,
          membership: m,
        });
      }
    }
    return result;
  })

  return res.status(200).json(result);
}

export default wrapApiHandler(handler);
