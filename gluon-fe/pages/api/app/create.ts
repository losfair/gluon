import type { NextApiRequest, NextApiResponse } from 'next'
import { models, retryableTxn } from "../../../service/db";
import { checkProp, checkProp_IsString, getAndVerifyProjectPermissions, LimitExceededError, PropCheckError, ResourceNotFoundError, wrapApiHandler } from '../../../service/error_wrapper';
import { KNOBS } from '../../../service/knobs';
import { AppSpec_validate, AppConfig_validate } from '../../../models/validation';
import { AppProvisioner } from '../../../service/provisioner/app_provisioner';

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<models.App>
) {
  const name = checkProp_IsString("name", req.body.name);
  const spec = checkProp("spec", req.body.spec, AppSpec_validate);
  const config = checkProp("config", req.body.config, AppConfig_validate);
  const app = await retryableTxn(async transaction => {
    const { projectId } = await getAndVerifyProjectPermissions(req, transaction);
    const project = await models.Project.findOne({
      where: {
        id: projectId,
      },
      transaction,
    });
    if (!project) {
      throw new ResourceNotFoundError("project not found");
    }
    const numApps = await models.App.count({
      where: {
        projectId,
      },
      transaction,
    });
    if (numApps + 1 > project.maxNumberOfApps) {
      throw new LimitExceededError("this project can have at most " + project.maxNumberOfApps + " apps");
    }

    const appWithThisName = await models.App.findOne({
      where: {
        projectId,
        name,
      },
      transaction,
    });
    if (appWithThisName) {
      throw new PropCheckError("an app with this name already exists");
    }

    const provisioner = new AppProvisioner(projectId, transaction);
    const app = await provisioner.createApp(name, spec, config);
    return app;
  });

  return res.status(200).json(app);
}

export default wrapApiHandler(handler);
