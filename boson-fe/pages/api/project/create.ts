import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { randomUUID } from 'crypto';
import { checkProp_IsString, mustGetToken, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.Project>
) {
  const name = checkProp_IsString("name", req.body.name);
  const token = await mustGetToken(req);
  const userId = token.uid;

  const project = await retryableTxn(async transaction => {
    const id = randomUUID();
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
