import type { NextApiRequest, NextApiResponse } from 'next'
import { models } from "../../service/db";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.AppDatabase[]>
) {
  const output = await models.AppDatabase.findAll({
    where: {
      projectId: req.query.projectId
    }
  });
  res.status(200).json(output);
}
