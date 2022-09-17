import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { checkProp_IsString, mustGetToken, ResourceNotFoundError, wrapApiHandler } from '../../../service/error_wrapper';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.Project>
) {
  const projectId = checkProp_IsString("id", req.body.id);
  const token = await mustGetToken(req);
  const userId = token.uid;

  const project = await retryableTxn(async transaction => {
    const project = await models.Project.findOne({
      where: {
        id: projectId,
      },
      transaction,
    });
    const membership = await models.ProjectMember.findOne({
      where: {
        projectId,
        userId,
        role: "owner",
      },
      transaction,
    });
    if (!membership || !project) throw new ResourceNotFoundError("project not found");
    await models.Project.destroy({
      where: {
        id: projectId,
      },
      transaction,
    });
    return project;
  });

  return res.status(200).json(project);
}

export default wrapApiHandler(handler);
