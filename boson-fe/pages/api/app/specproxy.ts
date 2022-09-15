// Next.js API route support: https://nextjs.org/docs/api-routes/introduction
import type { NextApiRequest, NextApiResponse } from 'next'
import { AppSpec } from '../../../models/App'
import { checkProp, checkProp_IsString, PermissionError, PropCheckError, wrapApiHandler } from '../../../service/error_wrapper'
import axios from "axios";
import YAML from "yaml";
import { AppSpec_validate } from '../../../models/validation';

const allowedOrigins = [
  "https://gist.githubusercontent.com",
  "https://raw.githubusercontent.com",
]

async function handler(
  req: NextApiRequest,
  res: NextApiResponse<AppSpec>
) {
  const url = new URL(checkProp_IsString("url", req.query.url));
  if (!allowedOrigins.includes(url.origin)) {
    throw new PermissionError("url origin not allowed");
  }

  const { data } = await axios.get<string>(url.href, {
    maxContentLength: 100 * 1024,
    timeout: 5000,
    responseType: "text",
  });

  let decoded: unknown;
  try {
    decoded = YAML.parse(data);
  } catch (e) {
    throw new PropCheckError("could not parse yaml: " + e);
  }

  const checked = checkProp("spec", decoded, AppSpec_validate);
  res.status(200).json(checked);
}

export default wrapApiHandler(handler);
