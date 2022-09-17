import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { randomUUID } from 'crypto';
import { checkProp_IsString, LimitExceededError, mustGetToken, wrapApiHandler } from '../../../service/error_wrapper';
import { KNOBS } from '../../../service/knobs';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.Project>
) {
  const name = checkProp_IsString("name", req.body.name);
  const token = await mustGetToken(req);
  const userId = token.uid;

  const project = await retryableTxn(async transaction => {
    const id = randomUUID();
    const numOwnedProjects = await models.ProjectMember.count({
      where: {
        userId,
        role: "owner",
      },
      transaction,
    });
    if (numOwnedProjects + 1 > KNOBS.maxOwnedProjectsPerUser) {
      throw new LimitExceededError("you can create at most " + KNOBS.maxOwnedProjectsPerUser + " projects");
    }
    const project = await models.Project.create({
      id,
      name,
    }, { transaction });
    await models.ProjectMember.create({
      projectId: id,
      userId,
      role: "owner",
    }, { transaction });
    return project;
  });

  return res.status(200).json(project!);
}

export default wrapApiHandler(handler);
