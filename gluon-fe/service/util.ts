import { GetServerSideProps, GetServerSidePropsResult } from "next"
import { getAndVerifyProjectPermissions2, MissingTokenError, PermissionError } from "./error_wrapper";

// Ensure initialization in SSR context
import "./db";
import { directTxn, models } from "./db";
import { InferAttributes } from "sequelize";

export type ProjectProps = InferAttributes<models.ProjectMember> & {
  project: InferAttributes<models.Project>,
};

export const loadProjectProps: GetServerSideProps = async (context): Promise<GetServerSidePropsResult<ProjectProps>> => {
  const projectId = context.params?.projectId;
  if (typeof projectId !== "string") {
    return {
      notFound: true,
    };
  }

  return await directTxn(async transaction => {
    let membership: models.ProjectMember;

    try {
      membership = await getAndVerifyProjectPermissions2(context.req, projectId, transaction);
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

    const project = await models.Project.findOne({
      where: {
        id: projectId,
      },
      transaction,
    });
    if (!project) {
      return {
        notFound: true,
      };
    }

    return {
      props: {
        ...membership.toJSON(),
        project: project.toJSON(),
      },
    }
  });
}