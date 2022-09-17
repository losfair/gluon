import { GetServerSideProps, GetServerSidePropsResult } from "next"
import { getAndVerifyProjectPermissions2, MissingTokenError, PermissionError } from "./error_wrapper";

// Ensure initialization in the renderer context
import "./db";
import { models } from "./db";

export type ProjectProps = models.ProjectMember;

export const loadProjectProps: GetServerSideProps = async (context): Promise<GetServerSidePropsResult<ProjectProps>> => {
  const projectId = context.params?.projectId;
  if (typeof projectId !== "string") {
    return {
      notFound: true,
    };
  }

  let membership: models.ProjectMember;

  try {
    membership = await getAndVerifyProjectPermissions2(context.req, projectId, null);
  } catch (e) {
    if (e instanceof MissingTokenError) {
      return {
        redirect: {
          destination: "/login",
          permanent: false,
        }
      };
    }

    if (e instanceof PermissionError) {
      return {
        notFound: true,
      };
    }

    throw e;
  }

  return {
    props: JSON.parse(JSON.stringify(membership)),
  }
}