import { selector } from "recoil";
import type { Project, ProjectMember } from "../models";
import { loadJson } from "./network";

export interface ProjectInfo {
  project: Project,
  membership: ProjectMember,
}

export const projectListQuery = selector<ProjectInfo[]>({
  key: "projectList",
  get: async () => await loadJson<ProjectInfo[]>("/api/project/list"),
});

export const firstProjectSelector = selector<ProjectInfo | null>({
  key: "firstProject",
  get: ({ get }) => {
    const projects = get(projectListQuery);
    return projects.length > 0 ? projects[0] : null;
  }
});